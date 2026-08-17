# spatial_kriging.R - interpolation engines and their shared plumbing:
# apply_OK/RK/RFK/CK/IDW/TPS via apply_kriging_pipeline/apply_interpolation,
# VIF gating (check_vif, detect_multicollinearity_engine), krige_covariates,
# optimize_idw_p, rf_infinitesimal_jackknife_var, prediction sanitizers.
# Sourced via spatial_helpers.R.

#' Which engines produce a genuine prediction variance?
#'
#' Only the kriging family does. `gstat::idw()` still returns a `var1.var`
#' column, but it is all NA (an inverse-distance weighting is a deterministic
#' exact interpolator with no variance model behind it), and apply_TPS's IDW
#' fallback inherits that column too — so a bare `"var1.var" %in% names()` test
#' is not enough to decide whether an uncertainty product exists. Single source
#' of truth for the pipeline's rasterization, the export registry and the map
#' viewer's uncertainty toggle.
METHODS_WITH_VARIANCE <- c("OK", "RK", "RFK", "CK")

method_has_variance <- function(method) {
  !is.null(method) && length(method) == 1L && !is.na(method) &&
    method %in% METHODS_WITH_VARIANCE
}

optimize_idw_p <- function(pts, target_var, nmax = 12) {
  factors <- seq(0.5, 5.0, by = 0.5)
  form <- as.formula(paste0("`", target_var, "` ~ 1"))
  
  n_folds <- if(nrow(pts) > 50) 5 else nrow(pts)

  # Seeding is CONDITIONAL here (the LOOCV branch needs no draws at all), so
  # this uses the plain sandbox and keeps its own set.seed inside.
  with_rng_sandbox({
    if (n_folds == nrow(pts)) {
      fold_assign <- 1:nrow(pts)
    } else {
      set.seed(12345)
      fold_assign <- sample(rep(1:n_folds, length.out = nrow(pts)))
    }

    rmses <- sapply(factors, function(f) {
      cv <- tryCatch({ krige.cv(form, pts, nmax = nmax, set = list(idp = f), nfold = fold_assign, debug.level = 0) }, error = function(e) NULL)
      if(!is.null(cv)) {
        sqrt(mean(cv$residual^2, na.rm = TRUE))
      } else {
        Inf
      }
    })

    best_idx <- which.min(rmses)
    if(length(best_idx) > 0 && rmses[best_idx] != Inf) factors[best_idx] else 2.0
  })
}

# "Constant" is a relative property, not an absolute one. An absolute variance
# floor (the old `var > 1e-6`) prunes legitimately small-unit covariates that
# carry real signal: a clay fraction on 0-1 with sd 5e-4 has var 1.7e-7 and was
# silently dropped before the VIF loop ever ran - and, per the Keep All rule,
# constants are dropped even under the user's explicit override, so nothing
# could rescue it. Fractions, ratios, normalized indices and anything in km or
# Mg live in that range. Compare spread against the column's own magnitude
# instead: below ~1e-8 relative the values differ only in noise digits, which
# is the case cor()/solve() actually cannot handle.
.is_degenerate_covariate <- function(col) {
  col <- col[is.finite(col)]
  if (length(col) < 2L) return(TRUE)
  if (length(unique(col)) < 2L) return(TRUE)
  scale_ref <- max(abs(col))
  if (!is.finite(scale_ref) || scale_ref == 0) return(TRUE)
  s <- stats::sd(col)
  !is.finite(s) || s <= 1e-8 * scale_ref
}

detect_multicollinearity_engine <- function(df, vars = NULL, vif_threshold = 10, pairwise_threshold = 0.95) {
  # sf's geometry column is sticky under `[ , ]`, so an sf input would carry an
  # sfc into the degenerate scan (is.finite() on an sfc errors) and, if it ever
  # simplified instead, `degen` would be longer than `kept` and misalign the
  # prune silently. Every current caller drops geometry first; this keeps the
  # publicly callable, worker-shared helper safe on its own terms.
  if (inherits(df, "sf")) df <- sf::st_drop_geometry(df)
  if (is.null(vars)) {
    df_num <- df[sapply(df, is.numeric)]
    vars <- colnames(df_num)
  } else {
    df_num <- df[, vars, drop = FALSE]
    df_num <- df_num[sapply(df_num, is.numeric)]
    vars <- colnames(df_num)
  }
  
  kept <- vars
  dropped <- c()
  # Two DIFFERENT reasons to drop a covariate share the `dropped` vector, and
  # consumers were labelling all of them "High VIF". Track them apart.
  dropped_constant <- character(0)

  # One degenerate scan shared by the pairwise report and the zero-var prune
  # below (kept is not modified in between).
  degen <- if (length(kept) >= 2) {
    sapply(df[, kept, drop = FALSE], .is_degenerate_covariate)
  } else {
    logical(0)
  }

  collinear_pairs <- data.frame(var1 = character(), var2 = character(), r = numeric(), stringsAsFactors = FALSE)
  has_collinearity <- FALSE

  if (length(kept) >= 2) {
    df_clean <- df[, kept, drop = FALSE]
    if (nrow(df_clean) >= 3) {
      valid_vars <- kept[!degen]
      
      if (length(valid_vars) >= 2) {
        # Pairwise deletion is right here: each cell is read on its own against
        # the threshold, nothing inverts this matrix, and a covariate measured
        # on a subset of the samples should still be screened on what it has.
        cormat <- cor(df_clean[, valid_vars], use = "pairwise.complete.obs")
        
        for (i in 1:(length(valid_vars) - 1)) {
          for (j in (i + 1):length(valid_vars)) {
            if (abs(cormat[i, j]) > pairwise_threshold) {
              collinear_pairs <- rbind(collinear_pairs, data.frame(
                var1 = valid_vars[i], 
                var2 = valid_vars[j], 
                r = cormat[i, j],
                stringsAsFactors = FALSE
              ))
            }
          }
        }
        has_collinearity <- nrow(collinear_pairs) > 0
      }
    }
  }
  
  if (length(kept) >= 2) {
    # A zero-variance covariate (common after subsetting to one locality)
    # makes cor() emit NA rows, which breaks solve() AND the correlation
    # fallback below — prune constants before the iterative loop. A constant
    # carries no information regardless of the user's keep/drop choice.
    zero_var <- kept[degen]
    if (length(zero_var) > 0) {
      dropped <- c(dropped, zero_var)
      dropped_constant <- zero_var
      kept <- setdiff(kept, zero_var)
    }
  }

  # An infinite threshold means the user explicitly chose to keep collinear
  # covariates — skip iterative pruning entirely so the solve() fallback
  # cannot drop anything either.
  if (length(kept) >= 2 && is.finite(vif_threshold)) {
    repeat {
      if (length(kept) == 1) {
        warning("VIF Iterative Pruning: only one covariate remains. Multicollinearity is extremely high.")
        break
      }
      if (length(kept) < 2) break
      df_clean_vif <- na.omit(df[, kept, drop = FALSE])
      if (nrow(df_clean_vif) < 3) break

      # Complete cases (na.omit above), not pairwise: this matrix gets inverted,
      # and cells estimated on different subsamples need not form a positive
      # semi-definite matrix, which can hand solve() negative VIFs.
      cor_mat <- cor(df_clean_vif)
      vif_vals <- tryCatch({ diag(solve(cor_mat)) }, error = function(e) { NULL })

      if (is.null(vif_vals)) {
        cor_mat_no_diag <- cor_mat
        diag(cor_mat_no_diag) <- 0
        max_abs <- max(abs(cor_mat_no_diag), na.rm = TRUE)
        if (!is.finite(max_abs)) break
        max_idx <- which(abs(cor_mat_no_diag) == max_abs, arr.ind = TRUE)[1,]
        pair <- kept[c(max_idx[1], max_idx[2])]
        # Drop the more GLOBALLY redundant member of the maximally-correlated
        # pair, measured as its mean |r| against every OTHER retained covariate.
        # The previous rule took kept[max_idx[1]], i.e. whichever member came
        # first in COLUMN order, so simply reordering the uploaded columns could
        # change which covariate survived and therefore the fitted model. Ties
        # break on the alphabetically later name (C collation) so the outcome is
        # a property of the data, not of the file layout.
        mean_abs_r <- vapply(pair, function(v) {
          others <- setdiff(kept, v)
          if (!length(others)) return(0)
          m <- mean(abs(cor_mat[v, others]), na.rm = TRUE)
          if (is.finite(m)) m else 0
        }, numeric(1))
        var_to_drop <- if (mean_abs_r[1] > mean_abs_r[2]) {
          pair[1]
        } else if (mean_abs_r[2] > mean_abs_r[1]) {
          pair[2]
        } else {
          sort(pair, decreasing = TRUE, method = "radix")[1]
        }
        dropped <- c(dropped, var_to_drop)
        kept <- setdiff(kept, var_to_drop)
        next
      }
      
      max_vif <- max(vif_vals)
      if (max_vif > vif_threshold) {
        var_to_drop <- names(vif_vals)[which.max(vif_vals)]
        dropped <- c(dropped, var_to_drop)
        kept <- setdiff(kept, var_to_drop)
      } else {
        break
      }
    }
  }
  
  return(list(
    has_collinearity = has_collinearity,
    pairs = if (nrow(collinear_pairs) > 0) collinear_pairs else NULL,
    kept = kept,
    # `dropped` stays the union so existing consumers are unaffected; the two
    # components let callers report the actual reason.
    dropped = dropped,
    dropped_constant = dropped_constant,
    dropped_vif = setdiff(dropped, dropped_constant)
  ))
}

check_vif <- function(df, threshold = 10) {
  res <- detect_multicollinearity_engine(df, vif_threshold = threshold)
  return(list(kept = res$kept, dropped = res$dropped,
              dropped_constant = res$dropped_constant,
              dropped_vif = res$dropped_vif))
}

#' Resolve the covariate gate for one surface. `run_regional_interpolation`
#' resolves it up front on this exact point set and hands the survivors down as
#' `method_params$aux_kept` (so dropped covariates are never kriged onto the
#' grid); a direct engine call recomputes it. Since `aux_kept` carries no
#' provenance, the constants among the dropped set are re-derived here — an
#' exact, local test costing one sd() per dropped column — so the run log names
#' the same reason on both paths.
.resolve_aux_gate <- function(data, aux_vars, method_params, vif_threshold) {
  if (is.null(method_params$aux_kept)) {
    return(check_vif(st_drop_geometry(data)[, aux_vars, drop = FALSE], threshold = vif_threshold))
  }
  drop_all <- setdiff(aux_vars, method_params$aux_kept)
  cst <- drop_all[vapply(drop_all, function(v) {
    .is_degenerate_covariate(st_drop_geometry(data)[[v]])
  }, logical(1))]
  list(kept = intersect(aux_vars, method_params$aux_kept),
       dropped = drop_all, dropped_constant = cst,
       dropped_vif = setdiff(drop_all, cst))
}

#' Run-log line for a covariate gate result. Constants and collinear covariates
#' are dropped for different reasons, so they must not both be reported as
#' "[VIF] Dropped". When the gate was pre-resolved upstream (only kept/dropped
#' are known) the provenance is unavailable, so the line stays neutral.
.vif_drop_log <- function(vif_res) {
  if (is.null(vif_res$dropped_vif) && is.null(vif_res$dropped_constant)) {
    if (!length(vif_res$dropped)) return("")
    return(paste0(" [Covariate gate] Dropped: ", paste(vif_res$dropped, collapse = ", ")))
  }
  # The bracketed prefixes are kept verbatim (they are what a user greps the run
  # log for); the reason is appended so the line reads without knowing what the
  # tag means.
  paste0(
    if (length(vif_res$dropped_vif)) paste0(" [VIF] Dropped (variance inflation above threshold; collinear with retained covariates): ", paste(vif_res$dropped_vif, collapse = ", ")) else "",
    if (length(vif_res$dropped_constant)) paste0(" [Constant] Dropped (no variance in this locality's data): ", paste(vif_res$dropped_constant, collapse = ", ")) else ""
  )
}

# Interpolate each auxiliary covariate onto the prediction grid so RK can
# evaluate the regression trend everywhere the target is predicted, not just at
# sample points. Each covariate is kriged with its own robust variogram fit and
# falls back to IDW if that fit fails.
#' `on_var(i, total)` is an optional hook invoked after each covariate surface.
#' The classification pipeline uses it to tick the progress bar and to poll its
#' cancel flag (one covariate is the coarsest interruptible unit here, since
#' gstat's krige() call is a black box). NULL = the original behaviour.
krige_covariates <- function(data, grid_p, aux_vars, lags, method_params, on_var = NULL) {
  grid_aux <- grid_p
  log_msg <- ""
  n_av <- length(aux_vars)
  for(i in seq_along(aux_vars)) {
    av <- aux_vars[i]
    kr_res <- tryCatch({
      v_emp_av <- variogram(as.formula(paste0("`", av, "` ~ 1")), data, width = lags$width, cutoff = lags$cutoff)
      fit_av <- robust_vgm_fit(v_emp_av, data[[av]])
      res_av <- krige(as.formula(paste0("`", av, "` ~ 1")), data, grid_p, model = fit_av, debug.level = 0)
      list(pred = res_av$var1.pred, warn = NULL)
    }, error = function(e) {
      warn_msg <- sprintf(" [WARN] Covariate %s kriging failed, falling back to IDW. ", av)
      idw_p <- if(!is.null(method_params$idw_p)) method_params$idw_p else 2
      idw_nmax <- if(!is.null(method_params$idw_nmax)) method_params$idw_nmax else 12
      res_av <- idw(as.formula(paste0("`", av, "` ~ 1")), data, grid_p, nmax = idw_nmax, idp = idw_p, debug.level = 0)
      list(pred = res_av$var1.pred, warn = warn_msg)
    })
    grid_aux[[av]] <- kr_res$pred
    if (!is.null(kr_res$warn)) {
      log_msg <- paste0(log_msg, kr_res$warn)
    }
    # A cancellation raised in the hook propagates out of the loop by design.
    if (is.function(on_var)) on_var(i, n_av)
  }
  return(list(grid_aux = grid_aux, log_msg = log_msg))
}


init_interpolation_res <- function() {
  # cv_obj_reps stays NULL unless the user asked for repeated CV: it holds one
  # trimmed CV frame per fold realization (see add_cv_repeats).
  list(v_emp = NULL, fit = NULL, cv_metrics = NULL, model_summary = NULL,
       rf_model = NULL, gstat_obj = NULL, res_sf = NULL, log_msg = "", cv_obj = NULL,
       cv_obj_reps = NULL, residuals = NULL)
}

safe_run_cv <- function(res, expr, label, n_data) {
  cv_obj <- tryCatch({
    expr
  }, error = function(e) {
    err <- list(error_msg = paste0(label, " CV Error: ", e$message))
    class(err) <- "cv_error"
    err
  })
  
  if (inherits(cv_obj, "cv_error")) {
    res$log_msg <- paste0(res$log_msg, cv_obj$error_msg)
    cv_obj <- NULL
  }
  
  res$cv_obj <- cv_obj
  res$cv_metrics <- perform_cv(cv_obj)
  if (!is.null(cv_obj)) {
    res$residuals <- get_cv_residuals(cv_obj, n_data)
  } else {
    # CV failed: drop any training residuals set earlier (RK/RFK set them
    # for the variogram step) so model_resid_* never mixes semantics:
    # res$residuals holds CV residuals or nothing.
    res$residuals <- NULL
  }
  return(res)
}

# ── Repeated cross-validation, engine side ──────────────────────────────────
# `cv_fun(seed)` re-runs an engine's OWN cross-validation with the fold seed it
# is handed; every engine already builds its folds through make_cv_folds, so a
# repeat differs from the reference run in the PARTITION only (model seeds,
# data, variogram policy and neighbourhood are untouched).
#
# Contract, deliberately strict: a locality either ships exactly `reps` frames
# or none. A partial set would make "mean +/- SD over R repeats" mean different
# things in different rows of the same table, and pooling would silently mix
# repeat counts across localities. On any failure the run keeps its normal
# single-realization metrics and says so in the log.
add_cv_repeats <- function(res, cv_fun, method_params, n_data, label,
                           l = "region", prefix = "act") {
  reps <- cv_repeat_count(method_params$cv_repeats, method_params$cv_strategy, n_data)
  if (reps < 2 || is.null(res$cv_obj)) return(res)

  frames <- vector("list", reps)
  frames[[1]] <- cv_repeat_frame(res$cv_obj)
  cancel_file <- method_params$cancel_file
  cancelled <- FALSE
  for (r in 2:reps) {
    # One cancel checkpoint per repeat: repeated CV is by construction the
    # longest stretch of a run, and a single CV pass is the coarsest
    # interruptible unit available (the engines are black boxes). BREAK rather
    # than stop(): both callers wrap this in a tryCatch that turns any error
    # into an engine fallback (OK for RK/RFK/CK, IDW for TPS), so raising here
    # would silently convert a cancellation into a different model. The surface
    # is already computed at this point; run_regional_interpolation's own
    # checkpoints abort the run at the next surface or locality.
    if (!is.null(cancel_file) && file.exists(cancel_file)) { cancelled <- TRUE; break }
    frames[[r]] <- tryCatch(cv_repeat_frame(cv_fun(CV_FOLD_SEED + r - 1L)),
                            error = function(e) NULL)
    # 55 -> 90: keeps the per-locality progress bar moving through the repeats
    # instead of parking it where the single-realization run used to finish.
    update_progress_file(l, prefix, 55 + round(35 * (r / reps)), 100)
  }

  if (cancelled || any(vapply(frames, is.null, logical(1)))) {
    res$log_msg <- paste0(res$log_msg, "\n[Repeated CV] ", label,
                          if (cancelled) ": cancelled; reporting the single-realization metrics only."
                          else ": a fold realization could not be evaluated; reporting the single-realization metrics only.")
    return(res)
  }
  res$cv_obj_reps <- frames
  res
}

# safe_run_cv for the reference realization (seed CV_FOLD_SEED - identical to a
# run with repeats switched off), then the optional extra realizations.
run_cv_with_repeats <- function(res, cv_fun, method_params, n_data, label,
                                l = "region", prefix = "act") {
  res <- safe_run_cv(res, cv_fun(CV_FOLD_SEED), label, n_data)
  add_cv_repeats(res, cv_fun, method_params, n_data, label, l, prefix)
}

# Scrub non-finite prediction and variance cells (NaN/Inf produced by degenerate
# fits) to NA, so downstream rasterization, colour scaling, and legends never
# choke on them.
sanitize_spatial_predictions <- function(res_sf) {
  if (!is.null(res_sf)) {
    if ("var1.pred" %in% colnames(res_sf)) {
      res_sf$var1.pred[is.nan(res_sf$var1.pred) | is.infinite(res_sf$var1.pred)] <- NA
    }
    if ("var1.var" %in% colnames(res_sf)) {
      res_sf$var1.var[is.nan(res_sf$var1.var) | is.infinite(res_sf$var1.var)] <- NA
    }
  }
  return(res_sf)
}

# Infinitesimal-jackknife variance of a random-forest ensemble-MEAN prediction
# (Wager, Hastie & Efron 2014), with the Monte-Carlo bias correction. This is
# the random-forest analogue of RK's lm `se.fit^2`: the sampling variance of the
# estimated mean surface, a better-calibrated trend-uncertainty term than the
# raw between-tree spread (which understates predictive uncertainty). It changes
# ONLY the RFK uncertainty (var1.var) surface, never the prediction (var1.pred).
#   pred_individual : n_pred x B matrix of per-tree predictions (predict.all$individual)
#   inbag           : n_train x B matrix of in-bag counts (randomForest keep.inbag = TRUE)
# Returns a length-n_pred variance vector, negatives (from the bias correction)
# truncated to 0. Chunked over prediction rows to bound the n_train x n_pred
# intermediate. Returns NA when fewer than two trees (variance undefined).
rf_infinitesimal_jackknife_var <- function(pred_individual, inbag, chunk = 2000L) {
  pred_individual <- as.matrix(pred_individual)
  inbag <- as.matrix(inbag)
  B <- ncol(pred_individual)
  n_pred <- nrow(pred_individual)
  n_train <- nrow(inbag)
  if (is.null(B) || B < 2 || ncol(inbag) != B) return(rep(NA_real_, n_pred))

  N_c <- inbag - rowMeans(inbag)                         # n_train x B, centred in-bag counts
  out <- numeric(n_pred)
  starts <- seq(1L, n_pred, by = chunk)
  for (s in starts) {
    e <- min(s + chunk - 1L, n_pred)
    Mc <- pred_individual[s:e, , drop = FALSE]
    Mc <- Mc - rowMeans(Mc)                              # m x B, centred per-tree preds
    # Cov[i, j] = (1/B) sum_b N_c[i,b] * Mc[j,b]  =>  (N_c %*% t(Mc)) / B
    cov_mat <- tcrossprod(N_c, Mc) / B                   # n_train x m
    v_ij <- colSums(cov_mat^2)                           # raw IJ per prediction
    bias <- (n_train / B^2) * rowSums(Mc^2)              # Monte-Carlo bias correction
    out[s:e] <- v_ij - bias
  }
  out[out < 0] <- 0
  out
}

# Shared "the covariate engine failed, fall back to Ordinary Kriging" tail,
# used by apply_kriging_pipeline (RK/RFK) and apply_CK. Refits the variogram of
# the MEASURED values (not residuals - there is no trend model left) and runs
# the same seeded-fold CV as every other path.
#   engine_label   : name used in the warning file ("RK", "RFK", "CK")
#   cv_label       : label safe_run_cv puts on a CV failure in the run log
#   tag_model_type : CK stamps the fallback surface; the RK/RFK path does not
# The warning wording ("... using Ordinary Kriging fallback.") is matched by the
# fallback-diagnostics UI and the tests - do not reword it here.
.ok_fallback <- function(res, data, target_var, grid_p, lags, method_params,
                         l, prefix, engine_label, cv_label, tag_model_type = FALSE) {
  write_warning_file(l, prefix, paste0(engine_label, " failed, using Ordinary Kriging fallback."))
  form_ok <- reformulate("1", response = target_var)
  res$v_emp <- variogram(form_ok, data, width = lags$width, cutoff = lags$cutoff)
  res$fit <- robust_vgm_fit(res$v_emp, data[[target_var]])
  res$res_sf <- krige(form_ok, data, grid_p, model = res$fit, debug.level = 0)
  if (tag_model_type) res$res_sf$model_type <- "Ordinary Kriging (Fallback)"
  # Consistent with every other CV path: an explicit seeded fold vector
  # (make_cv_folds) instead of a scalar nfold, so gstat never draws its own
  # unseeded folds here. Also honours the user's CV strategy on this path.
  coords_okfb <- sf::st_coordinates(data)
  cv_okfb <- function(seed) {
    krige.cv(form_ok, data, model = res$fit,
             nfold = make_cv_folds(coords_okfb, method_params$cv_strategy, nrow(data), seed),
             debug.level = 0)
  }
  run_cv_with_repeats(res, cv_okfb, method_params, nrow(data), cv_label, l, prefix)
}

apply_kriging_pipeline <- function(engine = c("OK", "RK", "RFK"), data, target_var, grid_p, lags, method_params, aux_vars = NULL, l = "region", prefix = "act", vif_threshold = 10) {
  engine <- match.arg(engine)
  res <- init_interpolation_res()
  
  if (engine == "OK") {
    update_progress_file(l, prefix, 20, 100)
    form_ok <- reformulate("1", response = target_var)
    res$v_emp <- variogram(form_ok, data, width = lags$width, cutoff = lags$cutoff)
    res$fit <- if(!is.null(method_params$pre_fit)) method_params$pre_fit else robust_vgm_fit(res$v_emp, data[[target_var]])
    
    update_progress_file(l, prefix, 50, 100)
    coords_ok <- sf::st_coordinates(data)
    cv_ok <- function(seed) {
      krige.cv(form_ok, data, model = res$fit,
               nfold = make_cv_folds(coords_ok, method_params$cv_strategy, nrow(data), seed),
               debug.level = 0)
    }
    res <- run_cv_with_repeats(res, cv_ok, method_params, nrow(data), "OK", l, prefix)
    res$res_sf <- krige(form_ok, data, grid_p, model = res$fit, debug.level = 0)
  } else {
    update_progress_file(l, prefix, 10, 100)
    krig_res <- tryCatch({
      if (engine %in% c("RK", "RFK") && length(aux_vars) > 1) {
        # run_regional_interpolation resolves this gate up front, on this exact
        # point set, so covariates the gate drops are never kriged onto the
        # prediction grid; it passes the surviving set down as
        # method_params$aux_kept. Recompute only when called directly.
        vif_res <- .resolve_aux_gate(data, aux_vars, method_params, vif_threshold)
        if (length(vif_res$dropped) > 0) {
          res$log_msg <- paste0(res$log_msg, .vif_drop_log(vif_res))
          aux_vars <- vif_res$kept
        }
        # An empty kept set builds "`v` ~ " and as.formula() dies with
        # "attempt to use zero-length variable name" — the tryCatch reports that
        # as "RK/RFK failed", naming the symptom instead of the cause. Say what
        # actually happened; the locality still routes to the named OK fallback.
        if (length(aux_vars) == 0) {
          stop("The covariate screen removed every covariate for this surface ",
               "(constant and/or collinear within this locality), so ", engine,
               " has no trend model left. Select different covariates, or answer ",
               "\"Keep All\" in the collinearity dialog.")
        }
      }

      if (!is.null(method_params$grid_aux)) {
        grid_aux <- method_params$grid_aux
      } else {
        krig_cov <- krige_covariates(data, grid_p, aux_vars, lags, method_params)
        grid_aux <- krig_cov$grid_aux
        res$log_msg <- paste0(res$log_msg, krig_cov$log_msg)
      }
      
      form_reg <- as.formula(paste(paste0("`", target_var, "`"), "~", paste(paste0("`", aux_vars, "`"), collapse = " + ")))
      
      if (engine == "RK") {
        # Same rank guard perform_kriging_loocv applies to its folds, on the
        # main fit: below (covariates + intercept) + 1 rows lm aliases
        # coefficients to NA and predict() returns a partly non-estimable trend
        # surface. Raising here routes the locality through the existing named
        # OK fallback instead of shipping a half-NA RK map.
        n_coef_rk <- length(aux_vars) + 1L
        if (nrow(data) < n_coef_rk + 1L) {
          stop(sprintf(
            paste0("RK needs at least %d points to fit %d regression coefficients; ",
                   "this locality has %d. Use fewer covariates."),
            n_coef_rk + 1L, n_coef_rk, nrow(data)))
        }
        lm_mod <- lm(form_reg, data = data)
        res$model_summary <- summary(lm_mod)
        
        data$residuals <- residuals(lm_mod)
        res$residuals <- residuals(lm_mod)
        
        res$v_emp <- variogram(residuals ~ 1, data, width = lags$width, cutoff = lags$cutoff)
        res$fit <- robust_vgm_fit(res$v_emp, data$residuals)
        res_krig <- krige(residuals ~ 1, data, grid_p, model = res$fit, debug.level = 0)
        
        pred_trend <- predict(lm_mod, newdata = grid_aux, se.fit = TRUE)
        trend_var <- (pred_trend$se.fit)^2
        
        res$res_sf <- grid_p %>% mutate(
          var1.pred = as.vector(pred_trend$fit + res_krig$var1.pred), 
          var1.var = as.vector(trend_var + res_krig$var1.var)
        )
        cv_rk <- function(seed) {
          perform_kriging_loocv(data, target_var, aux_vars, calc_scientific_lags, robust_vgm_fit,
                                model_type = "lm", l, prefix,
                                cv_strategy = method_params$cv_strategy, fold_seed = seed)
        }
        res <- run_cv_with_repeats(res, cv_rk, method_params, nrow(data), "RK", l, prefix)
      } else if (engine == "RFK") {
        rf_ntree <- if (!is.null(method_params$rf_ntree)) method_params$rf_ntree else 200
        rf_mod <- randomForest::randomForest(form_reg, data = data, ntree = rf_ntree, importance = TRUE, keep.inbag = TRUE)
        res$rf_model <- rf_mod
        
        residuals_val <- data[[target_var]] - rf_mod$predicted
        data$residuals <- residuals_val
        res$residuals <- residuals_val
        
        res$v_emp <- variogram(residuals ~ 1, data, width = lags$width, cutoff = lags$cutoff)
        res$fit <- robust_vgm_fit(res$v_emp, data$residuals)
        res_krig <- krige(residuals ~ 1, data, grid_p, model = res$fit, debug.level = 0)
        
        pred_trend_all <- predict(rf_mod, grid_aux, predict.all = TRUE)
        M <- pred_trend_all$individual
        rfk_unc <- if (!is.null(method_params$rfk_uncertainty)) method_params$rfk_uncertainty else "jackknife"
        if (identical(rfk_unc, "jackknife") && !is.null(rf_mod$inbag)) {
          # Calibrated trend variance: infinitesimal jackknife of the ensemble
          # mean (Wager et al. 2014): the RF analogue of RK's lm se.fit^2.
          trend_var <- rf_infinitesimal_jackknife_var(M, rf_mod$inbag)
          res$log_msg <- paste0(res$log_msg, " [RFK uncertainty: infinitesimal jackknife]")
        } else {
          # Ensemble spread (between-tree variance): fast stability heuristic
          # that understates predictive uncertainty (scientific_guide 7.3).
          trend_var <- rowSums((M - rowMeans(M))^2) / (ncol(M) - 1)
        }
        
        res$res_sf <- grid_p %>% mutate(
          var1.pred = as.vector(pred_trend_all$aggregate + res_krig$var1.pred), 
          var1.var = as.vector(trend_var + res_krig$var1.var)
        )
        cv_rfk <- function(seed) {
          perform_kriging_loocv(data, target_var, aux_vars, calc_scientific_lags, robust_vgm_fit,
                                model_type = "rf", l, prefix, rf_ntree = rf_ntree,
                                cv_strategy = method_params$cv_strategy, fold_seed = seed)
        }
        res <- run_cv_with_repeats(res, cv_rfk, method_params, nrow(data), "RFK", l, prefix)
      }
      res
    }, error = function(e) {
      res$log_msg <- paste0(res$log_msg, "\n", engine, " failed: ", e$message, ". Falling back to OK.")
      res$res_sf <- NULL
      res
    })
    res <- krig_res
    
    if (is.null(res$res_sf)) {
      res <- .ok_fallback(res, data, target_var, grid_p, lags, method_params,
                          l, prefix,
                          engine_label = engine,
                          cv_label = paste0(engine, " OK Fallback"))
    }
  }
  
  res$res_sf <- sanitize_spatial_predictions(res$res_sf)
  update_progress_file(l, prefix, 100, 100)
  return(res)
}

apply_OK <- function(data, target_var, grid_p, lags, method_params, l = "region", prefix = "act") {
  apply_kriging_pipeline("OK", data, target_var, grid_p, lags, method_params, NULL, l, prefix)
}

apply_RK <- function(data, target_var, grid_p, lags, method_params, aux_vars, l = "region", prefix = "act", vif_threshold = 10) {
  apply_kriging_pipeline("RK", data, target_var, grid_p, lags, method_params, aux_vars, l, prefix, vif_threshold)
}

apply_RFK <- function(data, target_var, grid_p, lags, method_params, aux_vars, l = "region", prefix = "act", vif_threshold = 10) {
  apply_kriging_pipeline("RFK", data, target_var, grid_p, lags, method_params, aux_vars, l, prefix, vif_threshold)
}

# Co-Kriging: predict the target jointly with its auxiliary variables through a
# linear model of coregionalization (LMC). Covariates are standardized first so
# the cross-variograms live on a comparable scale; the LMC is stabilized with
# correct.diagonal = 1.01 to keep the coregionalization matrices positive
# definite, and the whole fit falls back to Ordinary Kriging if it fails.
apply_CK <- function(data, target_var, grid_p, lags, method_params, aux_vars, l = "region", prefix = "act", vif_threshold = 10) {
  res <- init_interpolation_res()

  update_progress_file(l, prefix, 10, 100)
  
  # Search neighbourhood for the whole co-kriging system. This is a modelling
  # parameter (it sets how local the stationarity assumption is), not a pure
  # speed knob, so it is user-selectable; 15 is the documented default and the
  # historical hardcoded value.
  ck_nmax <- if (!is.null(method_params$ck_nmax) && is.finite(method_params$ck_nmax)) method_params$ck_nmax else 15

  ck_res <- tryCatch({
    # The same multicollinearity gate RK/RFK apply, and CK needs it at least as
    # badly: the LMC fits a direct variogram per covariate PLUS every cross
    # variogram, so collinear covariates make the coregionalization matrices
    # near-singular and fit.lmc() fails — which lands the run in the silent OK
    # fallback below. Without this, the user's "Auto-Drop and Continue" answer
    # to the collinearity modal was a no-op for CK.
    # run_regional_interpolation resolves the gate up front on this exact point
    # set and passes the survivors as method_params$aux_kept; recompute only
    # when apply_CK is called directly.
    if (length(aux_vars) > 1) {
      vif_res <- .resolve_aux_gate(data, aux_vars, method_params, vif_threshold)
      if (length(vif_res$dropped) > 0) {
        res$log_msg <- paste0(res$log_msg, .vif_drop_log(vif_res))
        aux_vars <- vif_res$kept
      }
      # CK does not die on an empty set the way RK does — gstat() with only the
      # primary variable still fits (verified: fit.lmc succeeds on a single
      # variable) and returns ordinary kriging under a 15-point neighbourhood,
      # labelled and logged as Co-Kriging. Co-kriging with no secondary variable
      # is not co-kriging, so send it to the named OK fallback instead of
      # shipping a mislabelled surface.
      if (length(aux_vars) == 0) {
        stop("The covariate screen removed every covariate for this surface ",
             "(constant and/or collinear within this locality), so Co-Kriging ",
             "has no secondary variable left. Select different covariates, or ",
             "answer \"Keep All\" in the collinearity dialog.")
      }
    }

    data_scaled <- data
    for(av in aux_vars) {
      # as.numeric: scale() returns an n x 1 matrix, and assigning that into an
      # sf column leaves a matrix-valued column that propagates through
      # variogram()/fit.lmc() and confuses any later dplyr verb on the object.
      #
      # KNOWN, ACCEPTED LEAK: these means and standard deviations come from the
      # FULL data set, and gstat.cv() below then holds points out of an already
      # centred and scaled frame — so each held-out point contributed (by 1/n)
      # to the centring of its own predictors. The leak is affine, second order
      # and O(1/n), it touches the covariates only (the target is never scaled),
      # and it cannot move a prediction's rank ordering. Removing it means
      # restandardizing inside every fold, which means abandoning gstat.cv() for
      # a hand-rolled co-kriging CV loop; that cost was reviewed and declined.
      # Documented in scientific_guide.md 4.4 / 9.1. Do not silently "fix" this
      # by scaling somewhere else — only a per-fold refit actually removes it.
      data_scaled[[av]] <- as.numeric(scale(data_scaled[[av]]))
    }

    form_ok <- reformulate("1", response = target_var)
    g <- gstat(NULL, id = target_var, formula = form_ok, data = data_scaled, nmax = ck_nmax)
    for(av in aux_vars) {
      g <- gstat(g, id = av, formula = as.formula(paste0("`", av, "` ~ 1")), data = data_scaled, nmax = ck_nmax)
    }
    
    vm <- variogram(g, width = lags$width, cutoff = lags$cutoff)
    
    v_emp_ok <- variogram(form_ok, data_scaled, width = lags$width, cutoff = lags$cutoff)
    fit_ok_init <- robust_vgm_fit(v_emp_ok, data_scaled[[target_var]])
    m_type <- suggest_lmc_model(fit_ok_init)
    
    # The single `model` argument is used by gstat as the STARTING model for
    # every direct and cross variogram (fit.lmc copies it over each id), so this
    # seeds the standardized covariate variograms — whose sills are 1.0 by
    # construction — with the target's raw variance, which can be many orders of
    # magnitude larger. That looks like a bug and was reported as one, but it is
    # inert here and must not be "fixed" by rescaling the seed: fit.lmc calls
    # fit.variogram with fit.ranges = FALSE, and with the range and model type
    # held fixed the variogram is LINEAR in its sill parameters, so the weighted
    # least-squares solve has a closed-form optimum that does not depend on the
    # starting sill. Verified 2026-07-20: starting sills spanning 1e-6 to 1e12
    # on the same empirical variogram all return a bit-identical fitted sill,
    # and per-id seeding from each variogram's own empirical plateau reproduces
    # the current fitted LMC exactly (target variances up to 3.5e8, 3
    # covariates). The starting values would matter immediately if fit.ranges
    # were ever set TRUE — the same probe then spread the fitted sill over
    # 1.0 to 3179 with no-convergence warnings — so scale the seeds per id at
    # the same time as any such change.
    g_or_err <- tryCatch({
      fit_obj <- fit.lmc(vm, g, vgm(var(data_scaled[[target_var]]), m_type, lags$cutoff / 2, 0), correct.diagonal = 1.01)
      res$log_msg <- paste0(res$log_msg, "\nLMC fitted with correct.diagonal = 1.01 (standard stabilization applied to every CK fit to keep the coregionalization matrices positive definite).")
      fit_obj
    }, error = function(e) {
      list(error_msg = paste0("LMC Fit Failed: ", e$message, ". Falling back to OK."))
    })
    
    if (is.list(g_or_err) && !is.null(g_or_err$error_msg)) {
      res$log_msg <- paste0(res$log_msg, g_or_err$error_msg)
      g <- NULL
      write_warning_file(l, prefix, "LMC model fit failed, using Ordinary Kriging fallback.")
    } else {
      g <- g_or_err
    }
    
    if(!is.null(g)) {
      res$gstat_obj <- g
      coords_ck <- sf::st_coordinates(data)
      cv_ck <- function(seed) {
        folds_ck <- make_cv_folds(coords_ck, method_params$cv_strategy, nrow(data), seed)
        # remove.all = TRUE: covariates here are co-sampled lab measurements, so
        # at real prediction locations CK has no covariate observations either -
        # each fold must remove the ENTIRE held-out row (all LMC variables), not
        # just the primary. The default (FALSE) scores CV under a collocated-
        # covariate information regime the map never enjoys (optimistic), and is
        # inconsistent with RK/RFK's perform_kriging_loocv, which holds out full
        # rows. See scientific_guide 4.4 / 9.1.
        cv_val <- gstat.cv(g, nfold = folds_ck, remove.all = TRUE, debug.level = 0)
        if (!is.null(cv_val)) {
          cnames <- names(cv_val)
          pred_col_src <- paste0(target_var, ".pred")
          obs_col_src <- paste0(target_var, ".observed")
          
          if (pred_col_src %in% cnames) {
            names(cv_val)[names(cv_val) == pred_col_src] <- "var1.pred"
          }
          if (obs_col_src %in% cnames) {
            names(cv_val)[names(cv_val) == obs_col_src] <- "var1.observed"
          }
        }
        cv_val
      }
      res <- run_cv_with_repeats(res, cv_ck, method_params, nrow(data), "CK", l, prefix)

      res_sf_or_err <- tryCatch({
        pred_obj <- predict(g, grid_p, debug.level = 0) %>% st_as_sf()
        pred_col <- paste0(target_var, ".pred")
        var_col <- paste0(target_var, ".var")
        pred_obj %>% dplyr::rename(var1.pred = !!rlang::sym(pred_col), var1.var = !!rlang::sym(var_col))
      }, error = function(e) {
        list(error_msg = paste0("CK Prediction Failed: ", e$message, ". Falling back to OK."))
      })
      
      if (is.list(res_sf_or_err) && !is.null(res_sf_or_err$error_msg)) {
        res$log_msg <- paste0(res$log_msg, res_sf_or_err$error_msg)
        res$res_sf <- NULL
      } else {
        res$res_sf <- res_sf_or_err
      }
    }
    res
  }, error = function(e) {
    res$log_msg <- paste0(res$log_msg, "\nCK failed: ", e$message, ". Falling back to OK.")
    res$res_sf <- NULL
    res
  })
  
  res <- ck_res
  if(!is.null(res$res_sf) && !("model_type" %in% names(res$res_sf))) {
    res$res_sf$model_type <- "Co-Kriging"
  }
  
  if(is.null(res$res_sf)) {
    res <- .ok_fallback(res, data, target_var, grid_p, lags, method_params,
                        l, prefix,
                        engine_label = "CK",
                        cv_label = "OK Fallback",
                        tag_model_type = TRUE)
  }
  
  res$res_sf <- sanitize_spatial_predictions(res$res_sf)
  
  update_progress_file(l, prefix, 100, 100)
  return(res)
}

apply_IDW <- function(data, target_var, grid_p, method_params, l = "region", prefix = "act") {
  res <- init_interpolation_res()
  
  update_progress_file(l, prefix, 20, 100)
  form_ok <- reformulate("1", response = target_var)
  # run_regional_interpolation always populates these, but the engine is
  # publicly callable and apply_TPS's fallback relies on that invariant holding
  # (see the comment there); default to the same values krige_covariates uses.
  idw_nmax <- method_params$idw_nmax %||% 12
  idw_p <- method_params$idw_p %||% 2
  coords_idw <- sf::st_coordinates(data)
  cv_idw <- function(seed) {
    krige.cv(form_ok, data, nmax = idw_nmax, set = list(idp = idw_p),
             nfold = make_cv_folds(coords_idw, method_params$cv_strategy, nrow(data), seed),
             debug.level = 0)
  }
  res <- run_cv_with_repeats(res, cv_idw, method_params, nrow(data), "IDW", l, prefix)

  res$res_sf <- idw(form_ok, data, grid_p, nmax = idw_nmax, idp = idw_p, debug.level = 0)
  res$res_sf <- sanitize_spatial_predictions(res$res_sf)
  
  update_progress_file(l, prefix, 100, 100)
  return(res)
}

apply_TPS <- function(data, target_var, grid_p, method_params, l = "region", prefix = "act") {
  res <- init_interpolation_res()
  
  update_progress_file(l, prefix, 10, 100)
  res$res_sf <- tryCatch({
    raw_pts <- st_coordinates(data)
    xm <- min(raw_pts[,1]); xM <- max(raw_pts[,1])
    ym <- min(raw_pts[,2]); yM <- max(raw_pts[,2])
    max_range <- max(xM - xm, yM - ym)
    if(max_range == 0) max_range <- 1
    pts_sc <- cbind((raw_pts[,1]-xm)/max_range, (raw_pts[,2]-ym)/max_range)
    tps_lam <- method_params$tps_lambda
    fit_tps <- function(x, y) {
      # is.na() first: `NA < 0` is NA, which errors the `if` and silently sent
      # the whole surface down the IDW fallback. NA is treated as "unset",
      # i.e. the documented Auto (GCV) default, same as NULL / lambda < 0.
      if (is.null(tps_lam) || is.na(tps_lam) || tps_lam < 0) fields::Tps(x, y) else fields::Tps(x, y, lambda = tps_lam)
    }
    
    gr_raw <- st_coordinates(grid_p)
    gr_sc <- cbind((gr_raw[,1]-xm)/max_range, (gr_raw[,2]-ym)/max_range)
    mod <- fit_tps(pts_sc, data[[target_var]])
    p_v <- fields::predict.Krig(mod, gr_sc)
    
    n_pts <- nrow(data)
    update_progress_file(l, prefix, 40, 100)
    # make_cv_folds seeds itself and restores the caller's RNG; LOOCV collapses
    # to one point per fold, so a single loop covers every strategy.
    tps_cv <- function(seed) {
      tps_folds <- make_cv_folds(raw_pts, method_params$cv_strategy, n_pts, seed)
      cv_vals <- rep(NA_real_, n_pts)
      for (i in sort(unique(tps_folds))) {
        test_idx <- which(tps_folds == i)
        tmp_mod <- tryCatch({
          fit_tps(pts_sc[-test_idx, , drop=FALSE], data[[target_var]][-test_idx])
        }, error = function(e) NULL)

        if (!is.null(tmp_mod)) {
          cv_vals[test_idx] <- as.numeric(fields::predict.Krig(tmp_mod, pts_sc[test_idx, , drop=FALSE]))
        } else {
          cv_vals[test_idx] <- NA_real_
        }
      }

      sf::st_as_sf(
        data.frame(observed = data[[target_var]], var1.pred = cv_vals, x = raw_pts[,1], y = raw_pts[,2]),
        coords = c("x", "y"), crs = sf::st_crs(data), remove = FALSE
      )
    }

    cv_res <- tps_cv(CV_FOLD_SEED)
    res$cv_obj <- cv_res
    res$cv_metrics <- perform_cv(cv_res)
    res$residuals <- get_cv_residuals(cv_res, nrow(data))
    res <- add_cv_repeats(res, tps_cv, method_params, n_pts, "TPS", l, prefix)

    grid_p %>% mutate(var1.pred = as.vector(p_v))
  }, error = function(e) {
    # The fallback result travels back as an attribute rather than four <<-
    # assignments into the enclosing `res`: writing the result object from two
    # scopes made this the one path where res could not be read top-to-bottom.
    # (apply_IDW is safe here - run_regional_interpolation always populates
    # idw_p/idw_nmax in mp_a/mp_p regardless of the selected method. Keep that
    # invariant if mp_* is ever slimmed.)
    write_warning_file(l, prefix, "TPS failed, using IDW fallback.")
    fb <- apply_IDW(data, target_var, grid_p, method_params, l, prefix)
    out <- fb$res_sf
    attr(out, "tps_fallback") <- list(cv_obj = fb$cv_obj, cv_metrics = fb$cv_metrics,
                                      residuals = fb$residuals, cv_obj_reps = fb$cv_obj_reps,
                                      err = e$message)
    out
  })

  fb <- attr(res$res_sf, "tps_fallback")
  if (!is.null(fb)) {
    res$cv_obj <- fb$cv_obj
    res$cv_metrics <- fb$cv_metrics
    res$residuals <- fb$residuals
    res$cv_obj_reps <- fb$cv_obj_reps
    res$log_msg <- paste0(res$log_msg, "\nTPS failed: ", fb$err, ". Falling back to IDW.")
    attr(res$res_sf, "tps_fallback") <- NULL
  }

  res$res_sf <- sanitize_spatial_predictions(res$res_sf)
  
  update_progress_file(l, prefix, 100, 100)
  return(res)
}

apply_interpolation <- function(data, target_var, method, grid_p, aux_vars, lags, method_params, l, prefix, vif_threshold = 10) {
  res <- tryCatch({
    if(method == "OK") {
      apply_OK(data, target_var, grid_p, lags, method_params, l, prefix)
    } else if(method == "RK" && length(aux_vars) > 0) {
      apply_RK(data, target_var, grid_p, lags, method_params, aux_vars, l, prefix, vif_threshold)
    } else if(method == "RFK" && length(aux_vars) > 0) {
      apply_RFK(data, target_var, grid_p, lags, method_params, aux_vars, l, prefix, vif_threshold)
    } else if(method == "CK" && length(aux_vars) > 0) {
      apply_CK(data, target_var, grid_p, lags, method_params, aux_vars, l, prefix, vif_threshold)
    } else if(method == "IDW") {
      apply_IDW(data, target_var, grid_p, method_params, l, prefix)
    } else if(method == "TPS") {
      apply_TPS(data, target_var, grid_p, method_params, l, prefix)
    } else if(method %in% c("RK", "RFK", "CK")) {
      # Reached only when a covariate-driven engine was selected with no
      # covariates: every branch above tests `length(aux_vars) > 0`, so control
      # used to fall through to the "unknown method" stop() and told the user
      # RK was an unrecognised method. The UI path guards this, but the engine
      # is publicly callable.
      stop(method, " requires at least one auxiliary covariate; none were supplied ",
           "(or all were removed by the covariate-completeness filter).")
    } else {
      stop("Unknown interpolation method: ", method)
    }
  }, error = function(e) {
    list(
      v_emp = NULL, fit = NULL, cv_metrics = NULL, model_summary = NULL, 
      rf_model = NULL, gstat_obj = NULL, res_sf = NULL, 
      log_msg = paste0("Error in apply_interpolation: ", e$message), cv_obj = NULL, residuals = NULL
    )
  })
  
  return(res)
}
