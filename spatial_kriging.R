# spatial_kriging.R - interpolation engines (apply_interpolation() dispatcher,
# apply_kriging_pipeline() for OK/RK/RFK, CK, IDW, TPS), the covariate screen,
# krige_covariates(), the IDW power selection, the RF jackknife variance and
# prediction sanitizers. Sourced via spatial_helpers.R.

#' Engines with a genuine prediction variance: the kriging family. gstat::idw()
#' (and the TPS IDW fallback) return an all-NA `var1.var`, so the column's
#' presence is not enough. Used by rasterization, the export registry and the
#' Map Viewer's SE/variance views.
METHODS_WITH_VARIANCE <- c("OK", "RK", "RFK", "CK")

# RFK grid trend prediction in blocks of this many matrix cells (predict.all
# builds n_rows x ntree): ~32 MB whatever the tree count. Exact, since a
# forest predicts rows independently.
.RFK_PREDICT_BLOCK_CELLS <- 4e6

method_has_variance <- function(method) {
  !is.null(method) && length(method) == 1L && !is.na(method) &&
    method %in% METHODS_WITH_VARIANCE
}

#' Detach a fitted model from the frame it was fitted in.
#'
#' A formula built inside a function carries that frame as its environment, so
#' an RK `summary.lm` returned from a worker would serialize the point set and
#' grids with it (15.72 MB vs 0.003 MB on 83 points at 60 m). Both holders are
#' reset: `$terms` and the `terms` of `$model`. The terms object is kept, so
#' the model still predicts on complete `newdata`. Apply where results cross
#' back to the main session, after every in-worker prediction. The RFK forest
#' (matrix interface) and CK's gstat object (0.16 MB) need no detaching.
detach_model_frame <- function(model) {
  if (is.null(model)) return(model)
  if (!is.null(model$terms)) attr(model$terms, ".Environment") <- globalenv()
  if (!is.null(attr(model$model, "terms"))) {
    attr(attr(model$model, "terms"), ".Environment") <- globalenv()
  }
  model
}

# ── IDW power selection (Auto (CV)) ─────────────────────────────────────────
# The searched powers span the whole IDW family, so no optimum lies outside
# them: p = 0 (plain mean of the Max Neighbors nearest samples), 0.25 steps to
# 6, coarser steps to IDW_MAX_FINITE_POWER, and Inf (nearest-neighbour limit).
# Below IDW_SELECT_MIN_N distinct samples p = 2 is used. From IDW_STEEP_POWER
# a sample 10% farther than the nearest keeps under a ninth of its weight
# ((1/1.1)^24 = 0.10), so the map is practically the stepped nearest-neighbour
# surface.
IDW_MAX_FINITE_POWER <- 48
IDW_POWER_GRID <- c(seq(0, 6, by = 0.25), 7, 8, 9, 10, 12, 14, 16, 20, 24, 32, IDW_MAX_FINITE_POWER, Inf)
IDW_SELECT_MIN_N <- 5L
IDW_STEEP_POWER <- 24

#' Exact IDW predictions at `test_xy` for every power at once: the
#' k = min(nmax, n_train) nearest samples weighted by d^-p, a distance-0 sample
#' taking its own value as in gstat. Returns n_test x length(powers). The
#' selection device only (it reproduces gstat's IDW CV, test-idw-selection.R);
#' maps and reported predictions come from gstat::idw() (idw_gstat()). Ties
#' between equidistant lattice neighbours may rank differently from gstat.
idw_kernel_predict <- function(train_xy, train_v, test_xy, powers, nmax) {
  nn <- FNN::get.knnx(train_xy, test_xy, k = min(nmax, nrow(train_xy)))
  idw_weighted(nn$nn.dist, matrix(train_v[nn$nn.index], nrow = nrow(test_xy)), powers)
}

# The inverse-distance average of neighbour values `nv` at distances `d` (one
# row per prediction location, nearest first), one column per power. Weights
# are taken relative to the nearest neighbour, (d1 / d)^p: the same normalised
# weights as d^-p, free of overflow and underflow at the steep end of the grid.
# p = Inf is the nearest neighbour's value.
idw_weighted <- function(d, nv, powers) {
  rel <- d[, 1] / d
  out <- vapply(powers, function(p) {
    if (is.infinite(p)) return(nv[, 1])
    w <- rel^p
    rowSums(w * nv) / rowSums(w)
  }, numeric(nrow(d)))
  out <- matrix(out, nrow = nrow(d))
  hit <- d[, 1] == 0
  if (any(hit)) out[hit, ] <- nv[hit, 1]
  out
}

#' gstat's IDW at power `p` over the `nmax` nearest samples, the engine every
#' IDW map and reported prediction comes from; the nearest-neighbour limit
#' (p = Inf) is gstat's IDW over the single nearest sample.
idw_gstat <- function(formula, data, newdata, nmax, p) {
  if (is.infinite(p)) return(gstat::idw(formula, data, newdata, nmax = 1, debug.level = 0))
  gstat::idw(formula, data, newdata, nmax = nmax, idp = p, debug.level = 0)
}

#' Which end of the IDW family a power is: "equal_weights" (p = 0, the plain
#' mean of the Max Neighbors nearest samples), "nearest_neighbour" (the
#' p -> Inf limit), NULL for every power between.
idw_power_limit <- function(p) {
  if (length(p) != 1 || is.na(p)) return(NULL)
  if (p == 0) "equal_weights" else if (is.infinite(p)) "nearest_neighbour"
}

#' A power as the run log and the panels name it: "p = 2", "equal weights
#' (p = 0)", "the nearest-neighbour limit (p → ∞)".
idw_power_text <- function(p) {
  switch(idw_power_limit(p) %||% "power",
         equal_weights = "equal weights (p = 0)",
         nearest_neighbour = "the nearest-neighbour limit (p → ∞)",
         paste0("p = ", format_power(p)))
}

#' Cross-validated IDW predictions of every row under the fold vector `folds`,
#' one column per power (n x length(powers)). Leave-one-out folds take the
#' neighbours of every row in one search (FNN::get.knn excludes the row
#' itself), which is what makes a nested leave-one-out selection affordable.
idw_cv_predictions <- function(xy, v, folds, powers, nmax) {
  n <- nrow(xy)
  if (!anyDuplicated(folds)) {
    nn <- FNN::get.knn(xy, k = min(nmax, n - 1L))
    return(idw_weighted(nn$nn.dist, matrix(v[nn$nn.index], nrow = n), powers))
  }
  pred <- matrix(NA_real_, n, length(powers))
  for (f in unique(folds)) {
    te <- which(folds == f)
    pred[te, ] <- idw_kernel_predict(xy[-te, , drop = FALSE], v[-te],
                                     xy[te, , drop = FALSE], powers, nmax)
  }
  pred
}

#' Pooled CV RMSE of every power on one fold vector, and `within_se`: TRUE when
#' a power's MSE exceeds the best's by at most one standard error of the paired
#' per-sample squared-error differences. That SE assumes independent samples
#' (too small under spatially correlated errors). Reported only; selects
#' nothing.
idw_cv_profile <- function(xy, v, folds, powers, nmax) {
  e2 <- (idw_cv_predictions(xy, v, folds, powers, nmax) - v)^2
  mse <- colMeans(e2)
  d <- e2 - e2[, which.min(mse)]
  se <- apply(d, 2, stats::sd) / sqrt(nrow(e2))
  data.frame(p = powers, rmse = sqrt(mse), within_se = colMeans(d) <= se)
}

#' The power a row set selects: the first minimum (smallest power among ties)
#' of the pooled CV RMSE over one fold vector, `folds` or make_cv_folds() under
#' `strategy` (kNNDM against `domain_xy`). `limit` names a selection at an end
#' of the family (idw_power_limit()): a finding about the data, never a
#' truncated search. Below IDW_SELECT_MIN_N rows or on a target without usable
#' variance nothing is searched: p = 2 and `skipped` names the reason.
select_idw_power <- function(xy, v, strategy, nmax, seed = CV_FOLD_SEED, powers = IDW_POWER_GRID,
                             domain_xy = NULL, folds = NULL) {
  n <- nrow(xy)
  skip <- if (n < IDW_SELECT_MIN_N) {
    sprintf("%d sample%s, fewer than %d", n, if (n == 1L) "" else "s", IDW_SELECT_MIN_N)
  } else if (.is_degenerate_covariate(v)) "the values carry no usable variance"
  if (!is.null(skip)) return(list(p = 2, profile = NULL, limit = NULL, skipped = skip))
  prof <- idw_cv_profile(xy, v, folds %||% make_cv_folds(xy, strategy, n, seed, domain_xy), powers, nmax)
  p <- powers[which.min(prof$rmse)]
  list(p = p, profile = prof, limit = idw_power_limit(p), skipped = NULL)
}

# "Constant" is relative to the column's own magnitude: an absolute variance
# floor would drop legitimately small-unit covariates (a 0-1 fraction with sd
# 5e-4 has var 2.5e-7) even under Keep All. Below ~1e-8 relative spread the
# values differ only in noise digits, which cor()/solve() cannot handle.
.is_degenerate_covariate <- function(col) {
  col <- col[is.finite(col)]
  if (length(col) < 2L) return(TRUE)
  if (length(unique(col)) < 2L) return(TRUE)
  scale_ref <- max(abs(col))
  if (!is.finite(scale_ref) || scale_ref == 0) return(TRUE)
  s <- stats::sd(col)
  !is.finite(s) || s <= 1e-8 * scale_ref
}

#' Covariate screen for RK/RFK/CK and the classification suite. Reports pairs
#' with |r| > `pairwise_threshold`, drops degenerate (constant) covariates, then
#' drops the highest-VIF covariate one at a time while any VIF exceeds
#' `vif_threshold` (`Inf` = keep all). Returns `list(has_collinearity, pairs,
#' kept, dropped, dropped_constant, dropped_vif)`. A pair without a finite
#' correlation (under two shared finite rows, or no variation on them) has
#' nothing to screen and is skipped, never read as r = 0.
detect_multicollinearity_engine <- function(df, vars = NULL, vif_threshold = 10, pairwise_threshold = 0.95) {
  # sf geometry is sticky under `[ , ]` and would break the degenerate scan.
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
  # Constant and high-VIF drops are tracked apart so callers report the reason.
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
        pair_data <- df_clean[, valid_vars, drop = FALSE]
        pair_data[] <- lapply(pair_data, function(v) { v[!is.finite(v)] <- NA_real_; v })
        cormat <- withCallingHandlers(cor(pair_data, use = "pairwise.complete.obs"),
          warning = function(w) {
            if (grepl("standard deviation is zero", conditionMessage(w), fixed = TRUE)) invokeRestart("muffleWarning")
          })
        for (i in 1:(length(valid_vars) - 1)) {
          for (j in (i + 1):length(valid_vars)) {
            r <- cormat[i, j]
            if (is.finite(r) && abs(r) > pairwise_threshold) {
              collinear_pairs <- rbind(collinear_pairs, data.frame(
                var1 = valid_vars[i], 
                var2 = valid_vars[j], 
                r = r,
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
    # A zero-variance covariate (common within one locality) gives cor() NA
    # rows and breaks solve(): prune constants first, whatever Keep/Drop says.
    zero_var <- kept[degen]
    if (length(zero_var) > 0) {
      dropped <- c(dropped, zero_var)
      dropped_constant <- zero_var
      kept <- setdiff(kept, zero_var)
    }
  }

  # An infinite threshold (Keep All) skips pruning, the solve() fallback included.
  if (length(kept) >= 2 && is.finite(vif_threshold)) {
    repeat {
      if (length(kept) == 1) {
        warning("VIF Iterative Pruning: only one covariate remains. Multicollinearity is extremely high.")
        break
      }
      if (length(kept) < 2) break
      df_clean_vif <- na.omit(df[, kept, drop = FALSE])
      if (nrow(df_clean_vif) < 3) break
      constants <- kept[vapply(df_clean_vif, .is_degenerate_covariate, logical(1))]
      if (length(constants)) {
        dropped <- c(dropped, constants)
        dropped_constant <- union(dropped_constant, constants)
        kept <- setdiff(kept, constants)
        next
      }

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
        # Drop the pair member more redundant overall (mean |r| against every
        # other kept covariate), so column order cannot decide; ties break on
        # the later name (C collation).
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
    # `dropped` is the union; the two components give the reason.
    dropped = dropped,
    dropped_constant = dropped_constant,
    dropped_vif = setdiff(dropped, dropped_constant)
  ))
}

#' detect_multicollinearity_engine() over every numeric column of `df`,
#' returning only the kept/dropped sets.
check_vif <- function(df, threshold = 10) {
  res <- detect_multicollinearity_engine(df, vif_threshold = threshold)
  return(list(kept = res$kept, dropped = res$dropped,
              dropped_constant = res$dropped_constant,
              dropped_vif = res$dropped_vif))
}

#' The constant/VIF covariate screen in ONE place: the surface's own gate and
#' every CV fold call it. Fewer than two candidates pass through (a sole
#' degenerate covariate is named by run_regional_interpolation() instead). The
#' "only one covariate remains" warning is muffled: per fold it repeats what
#' the run-log covariate line says.
screen_covariates <- function(df, candidates, vif_threshold = 10) {
  candidates <- as.character(candidates)
  if (length(candidates) < 2) {
    return(list(kept = candidates, dropped = character(0),
                dropped_constant = character(0), dropped_vif = character(0)))
  }
  if (inherits(df, "sf")) df <- sf::st_drop_geometry(df)
  withCallingHandlers(
    check_vif(df[, candidates, drop = FALSE], threshold = vif_threshold),
    warning = function(w) {
      if (grepl("VIF Iterative Pruning", conditionMessage(w), fixed = TRUE)) {
        invokeRestart("muffleWarning")
      }
    }
  )
}

#' Resolve the covariate gate for one surface: run_regional_interpolation()
#' resolves it on this point set and passes the survivors as
#' `method_params$aux_kept`; a direct engine call recomputes it. Constants
#' among the dropped set are re-derived (one sd() each), so the run log names
#' the same reason on both paths.
.resolve_aux_gate <- function(data, aux_vars, method_params, vif_threshold) {
  if (is.null(method_params$aux_kept)) {
    return(screen_covariates(data, aux_vars, vif_threshold))
  }
  drop_all <- setdiff(aux_vars, method_params$aux_kept)
  cst <- drop_all[vapply(drop_all, function(v) {
    .is_degenerate_covariate(st_drop_geometry(data)[[v]])
  }, logical(1))]
  list(kept = intersect(aux_vars, method_params$aux_kept),
       dropped = drop_all, dropped_constant = cst,
       dropped_vif = setdiff(drop_all, cst))
}

#' Run-log line for a covariate gate result, naming constants and collinear
#' covariates separately; neutral when only kept/dropped are known.
.vif_drop_log <- function(vif_res) {
  if (is.null(vif_res$dropped_vif) && is.null(vif_res$dropped_constant)) {
    if (!length(vif_res$dropped)) return("")
    return(paste0(" [Covariate gate] Dropped: ", paste(vif_res$dropped, collapse = ", ")))
  }
  # The bracketed tags are kept verbatim (users grep for them); the reason follows.
  paste0(
    if (length(vif_res$dropped_vif)) paste0(" [VIF] Dropped (variance inflation above threshold; collinear with retained covariates): ", paste(vif_res$dropped_vif, collapse = ", ")) else "",
    if (length(vif_res$dropped_constant)) paste0(" [Constant] Dropped (no variance in this locality's data): ", paste(vif_res$dropped_constant, collapse = ", ")) else ""
  )
}

#' Krige each covariate onto the prediction grid, so the RK trend is evaluated
#' wherever the target is predicted. Each uses its own robust variogram and
#' falls back to IDW when kriging fails (error, repeated location, or any
#' missing prediction). `on_var(i, total)` is an optional per-covariate hook
#' (the classification pipeline's progress and cancel check). Returns
#' `grid_aux`, the run-log text `log_msg` and `fallback`, the covariates that
#' took IDW.
krige_covariates <- function(data, grid_p, aux_vars, lags, method_params, on_var = NULL) {
  grid_aux <- grid_p
  log_msg <- ""
  fallback <- character(0)
  n_av <- length(aux_vars)
  for(i in seq_along(aux_vars)) {
    av <- aux_vars[i]
    # sp's make.names() would break non-syntactic headers such as "Fe (mg/kg)",
    # so each covariate is kriged under one syntactic alias.
    d_av <- data[av]
    names(d_av)[names(d_av) == av] <- ".mn_cov"
    kr_res <- tryCatch({
      # A repeated location makes the kriging system singular, and LAPACK then
      # fails or returns rounding noise depending on platform and row order, so
      # it is a failed fit before any solve (callers run merge_colocated()).
      if (anyDuplicated(sf::st_coordinates(d_av))) {
        stop("two samples share a location, so the kriging system is singular")
      }
      v_emp_av <- variogram(.mn_cov ~ 1, d_av, width = lags$width, cutoff = lags$cutoff)
      fit_av <- robust_vgm_fit(v_emp_av, d_av$.mn_cov)
      res_av <- krige(.mn_cov ~ 1, d_av, grid_p, model = fit_av, debug.level = 0)
      # gstat returns NA silently where its factorisation fails; a surface with
      # holes is a failed fit.
      n_na <- sum(is.na(res_av$var1.pred))
      if (n_na > 0) {
        stop(sprintf("kriging returned no prediction at %d of %d locations",
                     n_na, length(res_av$var1.pred)))
      }
      list(pred = res_av$var1.pred, warn = NULL)
    }, error = function(e) {
      warn_msg <- sprintf(" [WARN] Covariate %s kriging failed (%s), falling back to IDW. ",
                          av, gsub("\\s+", " ", trimws(conditionMessage(e))))
      idw_p <- if(!is.null(method_params$idw_p)) method_params$idw_p else 2
      idw_nmax <- if(!is.null(method_params$idw_nmax)) method_params$idw_nmax else 12
      res_av <- idw(.mn_cov ~ 1, d_av, grid_p, nmax = idw_nmax, idp = idw_p, debug.level = 0)
      list(pred = res_av$var1.pred, warn = warn_msg)
    })
    grid_aux[[av]] <- kr_res$pred
    if (!is.null(kr_res$warn)) {
      log_msg <- paste0(log_msg, kr_res$warn)
      fallback <- c(fallback, av)
    }
    # A cancellation raised in the hook propagates out of the loop by design.
    if (is.function(on_var)) on_var(i, n_av)
  }
  return(list(grid_aux = grid_aux, log_msg = log_msg, fallback = fallback))
}


#' The empty result list every engine fills and returns.
init_interpolation_res <- function() {
  # cv_obj_reps stays NULL unless the user asked for repeated CV: it holds one
  # trimmed CV frame per fold realization (see add_cv_repeats).
  list(v_emp = NULL, fit = NULL, cv_metrics = NULL, model_summary = NULL,
       rf_model = NULL, gstat_obj = NULL, res_sf = NULL, log_msg = "", cv_obj = NULL,
       cv_obj_reps = NULL)
}

#' Evaluate one CV expression into `res` (the CV object and its perform_cv()
#' metrics). A failure goes to the run log and leaves both empty instead of
#' stopping the engine.
safe_run_cv <- function(res, expr, label) {
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
  notes <- attr(cv_obj, "cv_notes")
  if (length(notes)) {
    res$log_msg <- paste0(res$log_msg, "\n[", label, " CV] ", paste(notes, collapse = "; "))
  }
  # Folds screen covariates on their own training rows, so they can keep a
  # different set than the map: say so, since it explains a CV/map mismatch.
  screen <- attr(cv_obj, "cv_screen")
  if (!is.null(screen)) {
    res$cv_screen <- screen
    if (screen$n_differ > 0) {
      res$log_msg <- paste0(
        res$log_msg, "\n[", label, " CV] the covariate screen kept a different set in ",
        screen$n_differ, " of ", screen$n_folds, " folds",
        if (length(screen$dropped)) paste0("; dropped: ", paste0(
          names(screen$dropped), " (", as.integer(screen$dropped), ")", collapse = ", ")) else "",
        ".")
    }
  }
  # A fold whose covariate kriging failed took the IDW fallback for its
  # held-out covariates (krige_covariates), as the map does for its grid.
  fold_meta <- attr(cv_obj, "cv_fold_meta")
  fb <- unlist(lapply(fold_meta, `[[`, "cov_fallback"), use.names = FALSE)
  if (length(fb)) {
    fb <- table(fb)
    res$log_msg <- paste0(
      res$log_msg, "\n[", label, " CV] covariate kriging failed, so the held-out values came from IDW: ",
      paste0(names(fb), " in ", as.integer(fb), " of ", length(fold_meta), " folds", collapse = ", "), ".")
  }

  # Per-fold variogram state, summarised as plain data so the Model Performance
  # card can say that some folds were not `ok` without re-reading the CV object.
  res$cv_vgm_status <- .cv_fold_status_table(cv_obj)

  res$cv_obj <- cv_obj
  res$cv_metrics <- perform_cv(cv_obj)
  return(res)
}

# ── Repeated cross-validation, engine side ──────────────────────────────────
# `cv_fun(seed)` re-runs the engine's own CV with that fold seed, so a repeat
# differs from the reference run in the PARTITION only. A locality ships
# exactly `reps` frames or none (a partial set would mix repeat counts in one
# table); on failure the single-realization metrics stand and the log says so.
add_cv_repeats <- function(res, cv_fun, method_params, n_data, label,
                           l = "region", prefix = "act") {
  reps <- cv_repeat_count(method_params$cv_repeats, method_params$cv_strategy, n_data)
  if (reps < 2 || is.null(res$cv_obj)) return(res)
  # kNNDM spatial folds draw no random numbers: every realization would repeat
  # the reference partition, so the locality keeps one, as under LOOCV.
  if (identical(attr(res$cv_obj, "knndm")$branch, "spatial")) {
    res$log_msg <- paste0(res$log_msg, "\n[Repeated CV] ", label, ", ", l, " (",
                          if (identical(prefix, "pre")) "Predicted" else "Actual",
                          "): kNNDM spatial folds are deterministic; one realization.")
    return(res)
  }

  frames <- vector("list", reps)
  frames[[1]] <- cv_repeat_frame(res$cv_obj)
  cancel_file <- method_params$cancel_file
  cancelled <- FALSE
  for (r in 2:reps) {
    # One cancel checkpoint per repeat. Break rather than stop(): the callers'
    # tryCatch would turn an error into an engine fallback. The surface is
    # already computed; later checkpoints abort the run.
    if (!is.null(cancel_file) && file.exists(cancel_file)) { cancelled <- TRUE; break }
    frames[[r]] <- tryCatch(cv_repeat_frame(cv_fun(CV_FOLD_SEED + r - 1L)),
                            error = function(e) NULL)
    # Progress 55 -> 90 through the repeats.
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
  res <- safe_run_cv(res, cv_fun(CV_FOLD_SEED), label)
  add_cv_repeats(res, cv_fun, method_params, n_data, label, l, prefix)
}

#' The CV population and fold plan a kriging engine scores: the run's
#' (`method_params$cv_data` / `cv_plan`) when supplied, else the engine's own
#' data folded by build_cv_plan() against the map's locations
#' (`method_params$cv_domain_xy`).
.engine_cv_plan <- function(method_params, data) {
  pop <- method_params$cv_data %||% data
  # A supplied population can be smaller than the point set (OK scored on the
  # covariate-complete rows). Below 3 samples the CV is skipped with a log
  # line; the map still runs.
  if (nrow(pop) < 3) {
    stop("the cross-validation population holds fewer than 3 samples (", nrow(pop), ")")
  }
  plan <- method_params$cv_plan %||%
    build_cv_plan(pop, method_params$cv_strategy, method_params$cv_repeats,
                  method_params$cv_domain_xy)
  if (!isTRUE(plan$n == nrow(pop))) {
    stop("CV plan population size (", plan$n, ") does not match the CV population (",
         nrow(pop), " rows).")
  }
  list(plan = plan, pop = pop)
}

#' Kriging cross-validation through the CV plan. `cv_one(pop, folds, row_id,
#' progress)` returns one realization's CV object. Realization r (seed
#' CV_FOLD_SEED + r - 1, the run_cv_with_repeats contract) takes the plan's
#' r-th fold vector; a plan error surfaces as a CV error, never an engine fallback.
#' `res$cv_design` is the plan's distance match (cv_distance_summary).
.run_kriging_cv <- function(res, cv_one, method_params, data, label, l, prefix) {
  cvp <- tryCatch(.engine_cv_plan(method_params, data), error = function(e) e)
  res$cv_design <- if (!inherits(cvp, "error")) cvp$plan$design
  cv_fun <- function(seed) {
    if (inherits(cvp, "error")) stop(conditionMessage(cvp))
    r <- seed - CV_FOLD_SEED + 1L
    if (r < 1L || r > length(cvp$plan$folds)) stop("The CV plan has no fold realization ", r, ".")
    progress <- if (r == 1L) {
      list(l = l, prefix = prefix, from = 50, to = if (length(cvp$plan$folds) > 1L) 55 else 90)
    }
    cv_one(cvp$pop, cvp$plan$folds[[r]], cvp$plan$row_id, progress)
  }
  n_data <- if (inherits(cvp, "error")) nrow(data) else cvp$plan$n
  res <- run_cv_with_repeats(res, cv_fun, method_params, n_data, label, l, prefix)
  # A CV that failed outright scored none of its population: 0 of n expected.
  if (is.null(res$cv_obj) && !inherits(cvp, "error")) {
    res$cv_metrics$n_expected <- cvp$plan$n
    res$cv_metrics$coverage <- 0
  }
  res
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
# (Wager, Hastie & Efron 2014) with the Monte-Carlo bias correction: the RF
# analogue of RK's lm se.fit^2. The between-tree spread is a different quantity
# (instability), and neither establishes interval calibration (scientific_guide
# 7.3). Affects only the RFK var1.var surface.
#   pred_individual : n_pred x B per-tree predictions (predict.all$individual)
#   inbag           : n_train x B in-bag counts (keep.inbag = TRUE)
#   inbag_centred   : optional pre-centred `inbag`, so the RFK grid loop
#                     centres once instead of once per block
# Returns a length-n_pred variance, negatives truncated to 0; NA with < 2
# trees. Chunked over prediction rows to bound the n_train x n_pred matrix.
rf_infinitesimal_jackknife_var <- function(pred_individual, inbag, chunk = 2000L,
                                           inbag_centred = NULL) {
  pred_individual <- as.matrix(pred_individual)
  inbag <- as.matrix(inbag)
  B <- ncol(pred_individual)
  n_pred <- nrow(pred_individual)
  n_train <- nrow(inbag)
  if (is.null(B) || B < 2 || ncol(inbag) != B) return(rep(NA_real_, n_pred))

  # n_train x B, centred in-bag counts
  N_c <- if (is.null(inbag_centred)) inbag - rowMeans(inbag) else inbag_centred
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

#' The fold function of OK and the OK fallback: lags, empirical variogram and
#' the full robust_vgm_fit() search are re-estimated from the fold's TRAINING
#' rows (never shrunk or warm-started), then the held-out coordinates are
#' kriged. `fixed_fit` (an applied manual model) is reused in every fold and
#' the CV is labelled conditional. `vgm_col` fits the variogram on the
#' MEASURED values of the training rows that carry one (the Predicted surface
#' kriged with the measured variogram).
.ok_fold_fun <- function(form_ok, target_var, fixed_fit = NULL, vgm_col = NULL) {
  function(train, newdata, i) {
    if (!is.null(fixed_fit)) {
      fit_i <- fixed_fit
    } else {
      vcol <- vgm_col %||% target_var
      vg <- if (is.null(vgm_col)) train else train[!is.na(train[[vgm_col]]), ]
      if (nrow(vg) < 3) {
        stop("only ", nrow(vg), " training rows carry a measured `", vcol,
             "` value, too few to fit a variogram")
      }
      lags_i <- calc_scientific_lags(vg)
      v_i <- variogram(reformulate("1", response = vcol), vg,
                       width = lags_i$width, cutoff = lags_i$cutoff)
      fit_i <- robust_vgm_fit(v_i, vg[[vcol]])
    }
    kr <- krige(form_ok, train, newdata, model = fit_i, debug.level = 0)
    list(pred = kr$var1.pred, var = kr$var1.var,
         meta = list(vgm_status = vgm_fit_status(fit_i)))
  }
}

#' How many folds set a logical flag in their `cv_fold_meta` entry, and how many
#' folds reported one at all.
.cv_fold_flag_count <- function(cv_obj, field) {
  meta <- attr(cv_obj, "cv_fold_meta")
  if (!length(meta)) return(list(n = 0L, total = 0L))
  list(n = sum(vapply(meta, function(m) isTRUE(m[[field]]), logical(1))),
       total = length(meta))
}

#' The variogram status of every fold that reported one, named by fold label.
#' A fold whose engine records no status is `ok`: it fitted nothing degraded.
.cv_fold_statuses <- function(cv_obj) {
  meta <- attr(cv_obj, "cv_fold_meta")
  meta <- Filter(function(m) !is.null(m$vgm_status), meta)
  if (!length(meta)) return(character(0))
  vapply(meta, function(m) as.character(m$vgm_status)[1], character(1))
}

#' One row per distinct fold status, worst first: `status`, `n`, and the fold
#' labels that reported it. This is the carrier the main session reads, so it
#' stays plain data (it crosses the future boundary inside `cv_info`).
.cv_fold_status_table <- function(cv_obj) {
  st <- .cv_fold_statuses(cv_obj)
  if (!length(st)) return(NULL)
  present <- intersect(VGM_FIT_STATUSES, unique(st))
  rows <- lapply(present, function(s) {
    labs <- names(st)[st == s]
    data.frame(status = s, n = length(labs),
               folds = paste(labs[order(suppressWarnings(as.numeric(labs)), labs)], collapse = ", "),
               stringsAsFactors = FALSE)
  })
  do.call(rbind, rows)
}

# Shared "covariate engine failed, fall back to Ordinary Kriging" tail of
# apply_kriging_pipeline() (RK/RFK) and apply_CK(): refits the variogram of the
# measured values and runs the same CV as every other path.
#   engine_label   : name in the warning file ("RK", "RFK", "CK")
#   cv_label       : label safe_run_cv() puts on a CV failure
#   tag_model_type : CK stamps the fallback surface; RK/RFK do not
# The warning wording is matched by the fallback diagnostics and the tests.
.ok_fallback <- function(res, data, target_var, grid_p, lags, method_params,
                         l, prefix, engine_label, cv_label, tag_model_type = FALSE) {
  write_warning_file(l, prefix, paste0(engine_label, " failed, using Ordinary Kriging fallback."))
  # The mapped surface uses no covariate; the screen's record (aux_dropped) stands.
  res$aux_used <- character(0)
  form_ok <- reformulate("1", response = target_var)
  res$v_emp <- variogram(form_ok, data, width = lags$width, cutoff = lags$cutoff)
  res$fit <- robust_vgm_fit(res$v_emp, data[[target_var]])
  res$res_sf <- krige(form_ok, data, grid_p, model = res$fit, debug.level = 0)
  if (tag_model_type) res$res_sf$model_type <- "Ordinary Kriging (Fallback)"
  # Same plan, fold runner and schema as every kriging engine; the fallback
  # never reuses a supplied model (nothing was applied to this surface).
  fold_okfb <- .ok_fold_fun(form_ok, target_var)
  cv_one <- function(pop, folds, row_id, progress) {
    run_kriging_folds(pop, target_var, row_id, folds, fold_okfb, method_params$cancel_file, progress)
  }
  res <- .run_kriging_cv(res, cv_one, method_params, data, cv_label, l, prefix)
  .log_vgm_fold_status(res, cv_label, l)
}

#' Append a run-log block naming every fold (and the locality) whose variogram
#' was not `ok`.
.log_vgm_fold_status <- function(res, label, locality = NULL) {
  st <- .cv_fold_statuses(res$cv_obj)
  bad <- st[st != "ok"]
  if (!length(bad)) return(res)
  ord <- order(suppressWarnings(as.numeric(names(bad))), names(bad))
  lines <- paste0("\n  Fold ", names(bad)[ord], ": ", vapply(bad[ord], vgm_status_label, character(1)))
  res$log_msg <- paste0(res$log_msg, "\n[", label, " CV] ",
                        if (!is.null(locality)) paste0(locality, ": "),
                        length(bad), " of ", length(st),
                        " folds used a degraded or extrapolative variogram.",
                        paste(lines, collapse = ""))
  res
}

#' Shared engine for OK, RK and RFK. OK kriges the target directly; RK (lm) and
#' RFK (randomForest) fit a trend on the screened covariates and krige its
#' residuals, adding the trend variance to the kriging variance. A failed RK/RFK
#' falls back to OK with a named warning. Returns the init_interpolation_res()
#' list with grid predictions in `res_sf` (`var1.pred`, `var1.var`).
apply_kriging_pipeline <- function(engine = c("OK", "RK", "RFK"), data, target_var, grid_p, lags, method_params, aux_vars = NULL, l = "region", prefix = "act", vif_threshold = 10) {
  engine <- match.arg(engine)
  res <- init_interpolation_res()
  
  if (engine == "OK") {
    update_progress_file(l, prefix, 20, 100)
    form_ok <- reformulate("1", response = target_var)
    res$v_emp <- variogram(form_ok, data, width = lags$width, cutoff = lags$cutoff)
    res$fit <- method_params$pre_fit %||% method_params$shared_fit %||% robust_vgm_fit(res$v_emp, data[[target_var]])
    
    update_progress_file(l, prefix, 50, 100)
    # Every fold re-estimates the variogram from its training rows, unless a
    # manual model was applied (conditional CV). A measured variogram shared
    # onto the Predicted surface is refitted from the training rows that carry
    # a measured value.
    if (!is.null(method_params$pre_fit)) res$cv_conditional <- "applied variogram"
    # `vgm_col` names the shared variogram's column and travels with
    # `shared_fit`, exactly as res$fit above resolves them.
    shared_col <- if (is.null(method_params$pre_fit) && !is.null(method_params$shared_fit)) {
      method_params$vgm_col
    }
    res$cv_vgm_col <- shared_col
    fold_ok <- .ok_fold_fun(form_ok, target_var,
                            fixed_fit = method_params$pre_fit, vgm_col = shared_col)
    cv_one <- function(pop, folds, row_id, progress) {
      run_kriging_folds(pop, target_var, row_id, folds, fold_ok, method_params$cancel_file, progress)
    }
    res <- .run_kriging_cv(res, cv_one, method_params, data, "OK", l, prefix)
    res <- .log_vgm_fold_status(res, "OK", l)
    res$res_sf <- krige(form_ok, data, grid_p, model = res$fit, debug.level = 0)

    # gstat silently returns an all-NA surface when the system is singular
    # (e.g. a zero-nugget Gaussian over samples a centimetre apart). The model
    # is not altered (run_regional_interpolation() reports the empty surface);
    # this names the likely cause.
    if (!is.null(res$res_sf) && "var1.pred" %in% names(res$res_sf) &&
        all(is.na(res$res_sf$var1.pred)) &&
        isTRUE(vgm_smooth_nugget_share(res$fit) < VGM_SMOOTH_NUGGET_WARN_SHARE)) {
      res$log_msg <- paste0(res$log_msg,
        " [OK] The Gaussian/Matern variogram has a nugget below 5% of its sill, which ",
        "makes the kriging system singular at these sample locations; add a nugget.")
    }
  } else {
    update_progress_file(l, prefix, 10, 100)
    # The full selected set: every CV fold re-screens THIS list on its own rows.
    candidates <- aux_vars
    krig_res <- tryCatch({
      if (engine %in% c("RK", "RFK") && length(aux_vars) > 1) {
        # run_regional_interpolation() resolves this gate up front (so dropped
        # covariates are never kriged) and passes method_params$aux_kept.
        vif_res <- .resolve_aux_gate(data, aux_vars, method_params, vif_threshold)
        if (length(vif_res$dropped) > 0) {
          res$log_msg <- paste0(res$log_msg, .vif_drop_log(vif_res))
          aux_vars <- vif_res$kept
        }
      }
      # For the run record: the covariates the trend uses and those the screen
      # removed (the OK fallback resets the first to none).
      res$aux_used <- aux_vars
      res$aux_dropped <- setdiff(candidates, aux_vars)
      # An empty set would fail later in as.formula() with an obscure message;
      # name the cause (the locality still takes the OK fallback).
      if (length(aux_vars) == 0) {
        stop("The covariate screen removed every covariate for this surface ",
             "(constant and/or collinear within this locality), so ", engine,
             " has no trend model left. Select different covariates, or answer ",
             "\"Keep All\" in the collinearity dialog.")
      }

      if (!is.null(method_params$grid_aux)) {
        grid_aux <- method_params$grid_aux
      } else {
        krig_cov <- krige_covariates(data, grid_p, aux_vars, lags,
                                     method_params$cov_params %||% method_params)
        grid_aux <- krig_cov$grid_aux
        res$log_msg <- paste0(res$log_msg, krig_cov$log_msg)
      }

      if (engine == "RK") {
        # Rank guard on the main fit (as in perform_kriging_loocv()'s folds): a
        # rank-deficient lm would ship a half-NA trend surface, so route the
        # locality to the named OK fallback instead.
        n_coef_rk <- length(aux_vars) + 1L
        if (nrow(data) < n_coef_rk + 1L) {
          stop(sprintf(
            paste0("RK needs at least %d points to fit %d regression coefficients; ",
                   "this locality has %d. Use fewer covariates."),
            n_coef_rk + 1L, n_coef_rk, nrow(data)))
        }
        form_reg <- as.formula(paste(paste0("`", target_var, "`"), "~", paste(paste0("`", aux_vars, "`"), collapse = " + ")))
        lm_mod <- lm(form_reg, data = data)
        res$model_summary <- summary(lm_mod)
        
        data$residuals <- residuals(lm_mod)
        
        res$v_emp <- variogram(residuals ~ 1, data, width = lags$width, cutoff = lags$cutoff)
        res$fit <- robust_vgm_fit(res$v_emp, data$residuals)
        res_krig <- krige(residuals ~ 1, data, grid_p, model = res$fit, debug.level = 0)
        
        pred_trend <- predict(lm_mod, newdata = grid_aux, se.fit = TRUE)
        trend_var <- (pred_trend$se.fit)^2
        
        res$res_sf <- grid_p %>% mutate(
          var1.pred = as.vector(pred_trend$fit + res_krig$var1.pred), 
          var1.var = as.vector(trend_var + res_krig$var1.var)
        )
        cv_rk <- function(pop, folds, row_id, progress) {
          perform_kriging_loocv(pop, target_var, aux_vars, calc_scientific_lags, robust_vgm_fit,
                                model_type = "lm", l, prefix,
                                cv_strategy = method_params$cv_strategy,
                                cov_params = method_params$cov_params %||% method_params,
                                folds = folds, row_id = row_id,
                                cancel_file = method_params$cancel_file, progress = progress,
                                candidates = candidates, vif_threshold = vif_threshold,
                                cv_domain_xy = method_params$cv_domain_xy)
        }
        res <- .run_kriging_cv(res, cv_rk, method_params, data, "RK", l, prefix)
        res <- .log_vgm_fold_status(res, "RK", l)
      } else if (engine == "RFK") {
        rf_ntree <- if (!is.null(method_params$rf_ntree)) method_params$rf_ntree else 200
        # Matrix interface: the formula method's make.names() breaks names such
        # as "Fe (mg/kg)". Seeded with CV_FOLD_SEED (fold forests use
        # CV_FOLD_SEED + label >= 1), so a locality's surface does not depend
        # on its position in the run.
        rf_mod <- with_seed(CV_FOLD_SEED, randomForest::randomForest(
          x = sf::st_drop_geometry(data)[aux_vars], y = data[[target_var]],
          ntree = rf_ntree, importance = TRUE, keep.inbag = TRUE))
        res$rf_model <- rf_mod
        
        data$residuals <- data[[target_var]] - rf_mod$predicted
        
        res$v_emp <- variogram(residuals ~ 1, data, width = lags$width, cutoff = lags$cutoff)
        res$fit <- robust_vgm_fit(res$v_emp, data$residuals)
        res_krig <- krige(residuals ~ 1, data, grid_p, model = res$fit, debug.level = 0)
        
        rfk_unc <- if (!is.null(method_params$rfk_uncertainty)) method_params$rfk_uncertainty else "jackknife"
        use_ij <- identical(rfk_unc, "jackknife") && !is.null(rf_mod$inbag)
        # predict.all allocates n_grid x ntree at once (multi-GB at the ~4M-cell
        # cap inside a worker), so predict in row blocks: forest predictions and
        # both variance estimators are row-independent, so the output is
        # identical.
        grid_aux_df <- if (inherits(grid_aux, "sf")) sf::st_drop_geometry(grid_aux) else as.data.frame(grid_aux)
        n_grid <- nrow(grid_aux_df)
        blk <- max(1L, min(n_grid, as.integer(ceiling(.RFK_PREDICT_BLOCK_CELLS / max(1L, rf_ntree)))))
        pred_mean <- numeric(n_grid)
        trend_var <- numeric(n_grid)
        # The in-bag centring belongs to the forest, so it is done once.
        inbag_c <- if (use_ij) { im <- as.matrix(rf_mod$inbag); im - rowMeans(im) } else NULL
        for (s in seq.int(1L, n_grid, by = blk)) {
          e <- min(s + blk - 1L, n_grid)
          nd <- grid_aux_df[s:e, aux_vars, drop = FALSE]
          # Name the cause: the matrix interface refuses missing covariates with
          # a bare "missing values in newdata".
          if (anyNA(nd)) {
            stop("RFK trend prediction: the covariate grid holds missing values in ",
                 sum(!stats::complete.cases(nd)), " of ", e - s + 1L, " grid cells.")
          }
          pa <- predict(rf_mod, nd, predict.all = TRUE)
          Mb <- pa$individual
          pred_mean[s:e] <- as.numeric(pa$aggregate)
          trend_var[s:e] <- if (use_ij) {
            # Trend variance: infinitesimal jackknife of the ensemble mean
            # (Wager et al. 2014), the RF analogue of RK's lm se.fit^2.
            rf_infinitesimal_jackknife_var(Mb, rf_mod$inbag, inbag_centred = inbag_c)
          } else {
            # Ensemble spread (between-tree variance): a fast model-instability
            # measure, not a predictive variance (scientific_guide 7.3).
            rowSums((Mb - rowMeans(Mb))^2) / (ncol(Mb) - 1)
          }
          rm(pa, Mb)
        }
        if (use_ij) res$log_msg <- paste0(res$log_msg, " [RFK uncertainty: infinitesimal jackknife]")
        
        res$res_sf <- grid_p %>% mutate(
          var1.pred = as.vector(pred_mean + res_krig$var1.pred), 
          var1.var = as.vector(trend_var + res_krig$var1.var)
        )
        cv_rfk <- function(pop, folds, row_id, progress) {
          perform_kriging_loocv(pop, target_var, aux_vars, calc_scientific_lags, robust_vgm_fit,
                                model_type = "rf", l, prefix, rf_ntree = rf_ntree,
                                cv_strategy = method_params$cv_strategy,
                                cov_params = method_params$cov_params %||% method_params,
                                folds = folds, row_id = row_id,
                                cancel_file = method_params$cancel_file, progress = progress,
                                candidates = candidates, vif_threshold = vif_threshold,
                                cv_domain_xy = method_params$cv_domain_xy)
        }
        res <- .run_kriging_cv(res, cv_rfk, method_params, data, "RFK", l, prefix)
        res <- .log_vgm_fold_status(res, "RFK", l)
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

#' Ordinary Kriging; see apply_kriging_pipeline().
apply_OK <- function(data, target_var, grid_p, lags, method_params, l = "region", prefix = "act") {
  apply_kriging_pipeline("OK", data, target_var, grid_p, lags, method_params, NULL, l, prefix)
}

#' Regression Kriging (linear trend + kriged residuals); see apply_kriging_pipeline().
apply_RK <- function(data, target_var, grid_p, lags, method_params, aux_vars, l = "region", prefix = "act", vif_threshold = 10) {
  apply_kriging_pipeline("RK", data, target_var, grid_p, lags, method_params, aux_vars, l, prefix, vif_threshold)
}

#' Random Forest Kriging (forest trend + kriged residuals); see apply_kriging_pipeline().
apply_RFK <- function(data, target_var, grid_p, lags, method_params, aux_vars, l = "region", prefix = "act", vif_threshold = 10) {
  apply_kriging_pipeline("RFK", data, target_var, grid_p, lags, method_params, aux_vars, l, prefix, vif_threshold)
}

#' Co-kriging needs a covariate measured together with the target at this many
#' locations before their cross-variogram can be estimated, the minimum the
#' directional variogram applies too. A covariate below it is dropped for the
#' surface (apply_CK); CV folds do not re-apply the rule.
CK_MIN_COLLOCATED <- 10L

#' The rows of one co-kriging data set that carry every covariate in
#' `candidates`, target rows first. The covariate screen runs on them: it
#' involves the covariates only, whether or not a row carries the target.
.ck_screen_rows <- function(target_rows, extra_rows, candidates) {
  rows <- sf::st_drop_geometry(target_rows)[candidates]
  if (!is.null(extra_rows) && nrow(extra_rows) > 0) {
    rows <- rbind(rows, sf::st_drop_geometry(extra_rows)[candidates])
  }
  rows[stats::complete.cases(rows), , drop = FALSE]
}

#' The constant/VIF covariate screen of a co-kriging data set
#' (screen_covariates on .ck_screen_rows). Fewer than three rows carrying every
#' candidate hold no estimable correlation, so the candidates then pass
#' unscreened, flagged `unscreened`.
.ck_screen <- function(target_rows, extra_rows, candidates, vif_threshold) {
  scr <- .ck_screen_rows(target_rows, extra_rows, candidates)
  if (length(candidates) > 1 && nrow(scr) < 3) {
    return(list(kept = candidates, dropped = character(0), dropped_constant = character(0),
                dropped_vif = character(0), unscreened = TRUE))
  }
  screen_covariates(scr, candidates, vif_threshold)
}

#' Build and fit the co-kriging LMC from the target rows (`target_rows`) and
#' the covariate-only rows (`extra_rows`, NULL or empty on isotopic data). The
#' map and every CV fold call it, so a fold refits exactly what the map fitted.
#'
#' Each variable is a gstat id with its own data: the target on the target
#' rows, each covariate (standardized) on every row that measures it; all
#' variograms use the primary's lags. A direct variogram uses its variable's
#' rows; a cross-variogram the rows carrying BOTH, as an isotopic pair. gstat's
#' pseudo cross-variogram (ids at different locations) contains half the
#' squared difference of the means (Papritz, Künsch & Webster 1993), which the
#' raw target vs standardized covariates would let dominate, so it is never
#' used (variogram(g, pseudo = 0) also crashes R). On isotopic data every piece
#' equals gstat's variogram of the whole object.
#'
#' Returns `g` (fitted gstat object, NULL on failure) with `monolith_ids` (id ->
#' column) and `monolith_vm` (the empirical variogram fitted), `error_msg`, the
#' run-log text, whether the range seed was the extent heuristic, the target's
#' id, `counts` (per covariate: rows measuring it, and how many carry the
#' target) and `design` ("isotopic" or "heterotopic").
.ck_fit_lmc <- function(target_rows, extra_rows, target_var, aux_vars, lags, ck_nmax) {
  log_msg <- ""
  # sp's make.names() breaks non-syntactic columns, so the LMC is fitted on
  # make.names() ids; `monolith_ids` maps them back for display.
  vars <- c(target_var, aux_vars)
  ids <- make.names(vars, unique = TRUE)
  rows <- target_rows[vars]
  if (!is.null(extra_rows) && nrow(extra_rows) > 0) rows <- rbind(rows, extra_rows[vars])
  # Each covariate standardized on the rows that measure it (scale() skips NA),
  # so a CV fold standardizes from its own training rows. as.numeric: scale()
  # returns a matrix.
  for (av in aux_vars) rows[[av]] <- as.numeric(scale(rows[[av]]))
  names(rows)[match(vars, names(rows))] <- ids
  has <- lapply(ids, function(id) !is.na(rows[[id]]))
  own <- function(k) rows[has[[k]], ids[k]]
  counts <- vapply(seq_along(aux_vars) + 1L, function(k) {
    c(n = sum(has[[k]]), collocated = sum(has[[k]] & has[[1]]))
  }, numeric(2))
  colnames(counts) <- aux_vars
  n_t <- nrow(target_rows)
  design <- if (all(counts["n", ] == n_t) && all(counts["collocated", ] == n_t)) "isotopic" else "heterotopic"

  # One empirical variogram (direct: one id; cross: the pair only) on the rows
  # in `mask`, via gstat's default method on plain values and coordinates:
  # what variogram(g) runs per id pair, without a gstat object's sp/CRS cost.
  xy <- sf::st_coordinates(rows)
  projected <- !isTRUE(sf::st_is_longlat(rows))
  emp <- function(ks, mask) {
    y <- stats::setNames(lapply(ks, function(k) rows[[ids[k]]][mask]), ids[ks])
    loc <- rep(list(xy[mask, , drop = FALSE]), length(ks))
    X <- rep(list(matrix(1, sum(mask), 1)), length(ks))
    variogram(y, loc, X, width = lags$width, cutoff = lags$cutoff,
              cross = if (length(ks) > 1) "ONLY" else TRUE, projected = projected)
  }

  v_emp_ok <- emp(1L, has[[1]])
  fit_ok_init <- robust_vgm_fit(v_emp_ok, own(1)[[ids[1]]])
  m_type <- suggest_lmc_model(fit_ok_init)

  # The single `model` seeds every direct and cross variogram (fit.lmc copies
  # it), so the standardized covariates start from the target's raw sill. That
  # is inert: with fit.ranges = FALSE the fit is linear in the sills and its
  # least-squares optimum does not depend on the start (starting sills from
  # 1e-6 to 1e12 gave identical fits). Scale the seeds per id if fit.ranges is
  # ever set TRUE.
  # The LMC shares ONE range across all variograms, so the seed range IS the
  # final range: the primary variable's fitted range (a geometric heuristic in
  # its place moved a short-range CK surface by up to 18% of the field's SD).
  # The extent heuristic (cutoff / 2) is used only when the primary fit is
  # itself a fallback or its range is unusable.
  lmc_range <- suppressWarnings(as.numeric(fit_ok_init$range[2])[1])
  # Seed Matern with its fitted kappa: vgm() defaults kappa to 0.5, and the
  # same `a` at nu = 0.5 is a shorter practical range (3a) than at 1.5 (4.75a).
  # Inert for Sph/Exp/Gau.
  lmc_kappa <- suppressWarnings(as.numeric(fit_ok_init$kappa[2])[1])
  if (!isTRUE(is.finite(lmc_kappa))) lmc_kappa <- 0.5
  heuristic_seed <- !isTRUE(is.finite(lmc_range)) || lmc_range <= 0 ||
    isTRUE(attr(fit_ok_init, "is_fallback"))
  if (heuristic_seed) {
    lmc_range <- lags$cutoff / 2
    lmc_kappa <- 0.5
    log_msg <- paste0(log_msg,
      "\n[CK] Primary variogram gave no usable range; LMC seeded with the extent heuristic (cutoff/2 = ",
      signif(lmc_range, 4), ").")
  } else {
    log_msg <- paste0(log_msg,
      "\n[CK] LMC range fixed at the primary variable's fitted range (",
      signif(lmc_range, 4), "); fit.lmc fits sills only.")
  }

  # A cross-variogram on the rows carrying both variables (an isotopic pair).
  cross_piece <- function(a, b) {
    both <- has[[a]] & has[[b]]
    if (sum(both) < 2) {
      stop("no two locations carry both ", vars[a], " and ", vars[b],
           ", so their cross-variogram cannot be estimated")
    }
    emp(c(a, b), both)
  }
  # The pieces in the order and form variogram(g) gives a gstat object (ids
  # in reverse; for each id, its cross-variograms with the ids before it, then
  # its own direct variogram), so fit.lmc finds every name and an isotopic data
  # set reproduces variogram(g) exactly.
  assemble_vm <- function() {
    parts <- list(); nm <- character(0); direct <- logical(0)
    for (a in rev(seq_along(ids))) for (b in seq_len(a)) {
      p <- if (a == b) {
        if (a == 1L) v_emp_ok else emp(a, has[[a]])
      } else cross_piece(b, a)
      piece <- if (a == b) ids[a] else paste(ids[b], ids[a], sep = ".")
      if (is.null(p) || nrow(p) == 0) {
        stop("the variogram ", piece, " has no point pairs within the lag cutoff")
      }
      parts[[length(parts) + 1L]] <- as.data.frame(p)[c("np", "dist", "gamma", "dir.hor", "dir.ver")]
      nm <- c(nm, rep(piece, nrow(p)))
      direct <- c(direct, a == b)
    }
    vm <- do.call(rbind, parts)
    vm$id <- factor(nm, levels = unique(nm))
    row.names(vm) <- NULL
    class(vm) <- c("gstatVariogram", "data.frame")
    attr(vm, "direct") <- data.frame(id = unique(nm), is.direct = direct)
    for (a in c("boundaries", "pseudo", "what")) attr(vm, a) <- attr(v_emp_ok, a)
    vm
  }

  fit_obj <- tryCatch({
    vm <- assemble_vm()
    g <- NULL
    for (k in seq_along(ids)) {
      g <- gstat(g, id = ids[k], formula = reformulate("1", response = ids[k]),
                 data = own(k), nmax = ck_nmax)
    }
    fit <- fit.lmc(vm, g, vgm(var(own(1)[[ids[1]]]), m_type, lmc_range, 0, kappa = lmc_kappa),
                   correct.diagonal = 1.01)
    attr(fit, "monolith_vm") <- vm
    fit
  }, error = function(e) structure(paste0("LMC Fit Failed: ", e$message, ". Falling back to OK."),
                                   class = "ck_lmc_error"))
  if (inherits(fit_obj, "ck_lmc_error")) {
    return(list(g = NULL, error_msg = as.character(fit_obj),
                log_msg = paste0(log_msg, as.character(fit_obj)),
                heuristic_seed = heuristic_seed, target_id = ids[1],
                counts = counts, design = design))
  }
  attr(fit_obj, "monolith_ids") <- stats::setNames(vars, ids)
  list(g = fit_obj, error_msg = NULL,
       log_msg = paste0(log_msg, "\nLMC fitted with correct.diagonal = 1.01 (standard stabilization applied to every CK fit to keep the coregionalization matrices positive definite)."),
       heuristic_seed = heuristic_seed, target_id = ids[1],
       counts = counts, design = design)
}

#' The run-log line naming a co-kriging surface's design: per covariate, the
#' locations measuring it and how many of them carry the target.
.ck_design_log <- function(lmc, l, surface, n_target) {
  if (identical(lmc$design, "isotopic")) {
    return(sprintf("\n[CK] %s (%s): isotopic design (n = %d).", l, surface, n_target))
  }
  per <- vapply(colnames(lmc$counts), function(av) {
    sprintf("%s: n = %d (%d collocated)", av, as.integer(lmc$counts["n", av]),
            as.integer(lmc$counts["collocated", av]))
  }, character(1))
  sprintf("\n[CK] %s (%s): heterotopic design: target n = %d; %s.", l, surface, n_target,
          paste(per, collapse = "; "))
}

#' The data column behind each id of a CK gstat object, named by id. A fitted
#' LMC uses syntactic ids (.ck_fit_lmc) and records their columns in
#' `monolith_ids`; an id without an entry is its own column.
ck_id_columns <- function(g) {
  ids <- names(g$data)
  cols <- attr(g, "monolith_ids")
  out <- if (is.null(cols)) ids else ifelse(ids %in% names(cols), cols[ids], ids)
  stats::setNames(unname(out), ids)
}

#' How often a fold's covariate screen kept a different set than the map's, and
#' which of the map's covariates the folds dropped.
.fold_screen_summary <- function(cv_obj, map_kept) {
  kept <- lapply(attr(cv_obj, "cv_fold_meta"), function(m) m$kept)
  kept <- kept[!vapply(kept, is.null, logical(1))]
  if (!length(kept)) return(NULL)
  dropped <- unlist(lapply(kept, function(k) setdiff(map_kept, k)), use.names = FALSE)
  list(n_folds = length(kept),
       n_differ = sum(vapply(kept, function(k) !setequal(k, map_kept), logical(1))),
       dropped = if (length(dropped)) table(dropped) else integer(0))
}

# Co-Kriging through a linear model of coregionalization (LMC). `data` holds
# the target rows; `method_params$ck_extra` the covariate-only rows, which
# enter the covariates' data only (heterotopic; empty = isotopic). Covariates
# are standardized on their own rows (.ck_fit_lmc()), the LMC is stabilized
# with correct.diagonal = 1.01, and a failed fit falls back to Ordinary
# Kriging.
apply_CK <- function(data, target_var, grid_p, lags, method_params, aux_vars, l = "region", prefix = "act", vif_threshold = 10) {
  res <- init_interpolation_res()

  update_progress_file(l, prefix, 10, 100)

  # Search neighbourhood of the whole system: a modelling parameter (how local
  # stationarity is assumed), so user-selectable; default 15.
  ck_nmax <- if (!is.null(method_params$ck_nmax) && is.finite(method_params$ck_nmax)) method_params$ck_nmax else 15
  extra <- method_params$ck_extra
  surface <- if (identical(prefix, "pre")) "Predicted" else "Actual"

  ck_res <- tryCatch({
    # A cross-variogram needs the covariate measured at target locations: below
    # CK_MIN_COLLOCATED it is dropped before the collinearity screen. Folds
    # screen THIS list and do not re-apply the rule.
    colloc <- vapply(aux_vars, function(av) sum(!is.na(data[[av]])), integer(1))
    short <- aux_vars[colloc < CK_MIN_COLLOCATED]
    if (length(short)) {
      res$log_msg <- paste0(res$log_msg, "\n[CK] ", l, " (", surface, "): dropped ",
        paste0(short, " (", colloc[short], " collocated)", collapse = ", "),
        "; a cross-variogram needs the covariate measured with the target at ",
        CK_MIN_COLLOCATED, " or more locations.")
    }
    candidates <- setdiff(aux_vars, short)
    if (length(candidates) == 1 &&
        .is_degenerate_covariate(c(data[[candidates]], if (!is.null(extra)) extra[[candidates]]))) {
      write_warning_file(l, prefix, paste0("Covariate '", candidates, "' is (near-)constant in this ",
                                           "locality, so Co-Kriging receives no covariate information from it."))
    }
    # The RK/RFK collinearity screen: collinear covariates make the
    # coregionalization near-singular. Runs on every location carrying all
    # candidates, covariate-only rows included (.ck_screen()).
    kept <- candidates
    if (length(candidates) > 1) {
      vif_res <- .ck_screen(data, extra, candidates, vif_threshold)
      if (isTRUE(vif_res$unscreened)) {
        res$log_msg <- paste0(res$log_msg, "\n[CK] ", l, " (", surface, "): fewer than 3 locations ",
                              "carry every covariate, so the covariates were not screened for collinearity.")
      }
      if (length(vif_res$dropped) > 0) {
        res$log_msg <- paste0(res$log_msg, .vif_drop_log(vif_res))
        kept <- vif_res$kept
      }
    }
    # For the run record, as in apply_kriging_pipeline.
    res$aux_used <- kept
    res$aux_dropped <- setdiff(aux_vars, kept)
    # With no secondary variable gstat would quietly return OK labelled
    # Co-Kriging, so the named OK fallback takes over.
    if (length(kept) == 0) {
      stop(if (length(candidates) == 0) {
        paste0("No selected covariate is measured with the target at ", CK_MIN_COLLOCATED,
               " or more locations, so Co-Kriging has no secondary variable.")
      } else {
        paste0("The covariate screen removed every covariate for this surface ",
               "(constant and/or collinear within this locality), so Co-Kriging ",
               "has no secondary variable left. Select different covariates, or ",
               "answer \"Keep All\" in the collinearity dialog.")
      })
    }

    # Same data assembly, standardization and LMC fit the folds run.
    lmc <- .ck_fit_lmc(data, extra, target_var, kept, lags, ck_nmax)
    res$ck_design <- lmc$design
    res$log_msg <- paste0(res$log_msg, .ck_design_log(lmc, l, surface, nrow(data)), lmc$log_msg)
    g <- lmc$g
    if (is.null(g)) {
      write_warning_file(l, prefix, "LMC model fit failed, using Ordinary Kriging fallback.")
    }

    if(!is.null(g)) {
      res$gstat_obj <- g
      # A smooth structure with a negligible nugget is near-singular, more so
      # with dense data (Posa 1989), and dense covariate-only rows are where
      # co-kriging gains: name the condition.
      smooth <- names(Filter(function(m) isTRUE(vgm_smooth_nugget_share(m) < VGM_SMOOTH_NUGGET_WARN_SHARE),
                             g$model[names(g$data)]))
      if (length(smooth)) {
        res$log_msg <- paste0(res$log_msg, "\n[CK] ", l, " (", surface, "): the LMC's ",
          as.character(g$model[[1]]$model[2]), " structure has a nugget below 5% of the sill for ",
          paste(ck_id_columns(g)[smooth], collapse = ", "),
          "; such a system can be ill-conditioned, the more so with dense covariate data. ",
          "Compare its cross-validation with the other engines' before relying on the map.")
      }
      # Every fold re-screens, re-standardizes and refits the LMC on its
      # training rows plus the covariate-only rows; a held-out location
      # contributes its coordinates only, as a grid cell does.
      cv_ck <- function(pop, folds, row_id, progress) {
        fold_ck <- function(train, newdata, i) {
          kept_i <- .ck_screen(train, extra, candidates, vif_threshold)$kept
          if (!length(kept_i)) stop("the covariate screen removed every covariate in this fold")
          fit_i <- .ck_fit_lmc(train, extra, target_var, kept_i, calc_scientific_lags(train), ck_nmax)
          if (is.null(fit_i$g)) stop(fit_i$error_msg)
          p <- predict(fit_i$g, newdata, debug.level = 0)
          list(pred = p[[paste0(fit_i$target_id, ".pred")]], var = p[[paste0(fit_i$target_id, ".var")]],
               meta = list(kept = kept_i, heuristic_seed = fit_i$heuristic_seed))
        }
        cv <- run_kriging_folds(pop, target_var, row_id, folds, fold_ck,
                                method_params$cancel_file, progress)
        attr(cv, "cv_screen") <- .fold_screen_summary(cv, kept)
        cv
      }
      res <- .run_kriging_cv(res, cv_ck, method_params, data, "CK", l, prefix)
      hs <- .cv_fold_flag_count(res$cv_obj, "heuristic_seed")
      if (hs$n > 0) {
        res$log_msg <- paste0(res$log_msg, "\n[CK CV] ", hs$n, " of ", hs$total,
                              " fitted folds seeded the LMC range from the extent heuristic.")
      }

      res_sf_or_err <- tryCatch({
        pred_obj <- predict(g, grid_p, debug.level = 0) %>% st_as_sf()
        pred_col <- paste0(lmc$target_id, ".pred")
        var_col <- paste0(lmc$target_id, ".var")
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

#' Inverse distance weighting over the `idw_nmax` nearest samples (default
#' 12). `idw_p` >= 0 is a fixed power (0 = equal weights); NULL, NA or negative
#' is Auto (CV): the map uses select_idw_power() on all rows and every CV fold
#' re-selects from its own training rows, so the CV includes the selection.
#' `method_params$idw_select_col` (an unseparated Predicted surface) makes the
#' selection read the measured values on the rows that carry them.
#' `res$idw_fit` records the mode, map power (Inf = nearest-neighbour limit),
#' CV-RMSE profile, `limit`, realization 1's fold powers and `select_source`
#' ("own"/"measured"). Under kNNDM every fold set, nested ones included, is
#' matched to `method_params$cv_domain_xy`. Deterministic: no usable variance.
apply_IDW <- function(data, target_var, grid_p, method_params, l = "region", prefix = "act") {
  res <- init_interpolation_res()

  update_progress_file(l, prefix, 20, 100)
  form_ok <- reformulate("1", response = target_var)
  # Default for direct calls and apply_TPS()'s fallback (the pipeline sets it).
  idw_nmax <- method_params$idw_nmax %||% 12
  idw_p <- method_params$idw_p
  auto_p <- is.null(idw_p) || is.na(idw_p) || idw_p < 0
  coords_idw <- sf::st_coordinates(data)
  domain_xy <- method_params$cv_domain_xy
  sel_col <- method_params$idw_select_col
  on_measured <- auto_p && !is.null(sel_col) && sel_col %in% names(data)
  sel_v <- if (on_measured) data[[sel_col]] else data[[target_var]]
  select_on <- function(rows, folds = NULL) {
    rows <- rows[is.finite(sel_v[rows])]
    select_idw_power(coords_idw[rows, , drop = FALSE], sel_v[rows],
                     method_params$cv_strategy, idw_nmax, domain_xy = domain_xy, folds = folds)
  }
  # The reference CV's folds. When every row carries the selection values, the
  # map's selection uses these same folds (one kNNDM search serves both).
  ref_folds <- NULL
  sel <- if (auto_p) {
    if (all(is.finite(sel_v))) {
      ref_folds <- make_cv_folds(coords_idw, method_params$cv_strategy, nrow(data), CV_FOLD_SEED, domain_xy)
    }
    select_on(seq_len(nrow(data)), ref_folds)
  }
  map_p <- if (auto_p) sel$p else idw_p

  cv_idw <- function(seed) {
    folds <- if (seed == CV_FOLD_SEED && !is.null(ref_folds)) ref_folds else
      make_cv_folds(coords_idw, method_params$cv_strategy, nrow(data), seed, domain_xy)
    if (seed == CV_FOLD_SEED) ref_folds <<- folds
    if (!auto_p) {
      cv <- krige.cv(form_ok, data, nmax = idw_nmax, set = list(idp = idw_p),
                     nfold = folds, debug.level = 0)
    } else {
      # Nested: each fold selects its power from its own training rows, so no
      # held-out value reaches the power that predicts it. Keeps krige.cv's
      # columns.
      ids <- sort(unique(folds))
      pred <- rep(NA_real_, nrow(data))
      fold_p <- stats::setNames(numeric(length(ids)), ids)
      for (j in seq_along(ids)) {
        te <- which(folds == ids[j])
        fold_p[j] <- select_on(setdiff(seq_len(nrow(data)), te))$p
        pred[te] <- idw_gstat(form_ok, data[-te, ], data[te, ], idw_nmax, fold_p[j])$var1.pred
      }
      obs <- data[[target_var]]
      cv <- sf::st_sf(var1.pred = pred, var1.var = NA_real_, observed = obs,
                      residual = obs - pred, zscore = NA_real_, fold = as.integer(folds),
                      geometry = sf::st_geometry(data))
      attr(cv, "fold_power") <- fold_p
    }
    attr(cv, "block_fallback") <- attr(folds, "block_fallback")
    attr(cv, "knndm") <- attr(folds, "knndm")
    cv
  }
  res <- run_cv_with_repeats(res, cv_idw, method_params, nrow(data), "IDW", l, prefix)
  res$cv_design <- cv_distance_summary(coords_idw, ref_folds, domain_xy, units = crs_unit_label(data))

  # `nmax` (neighbours actually averaged) and `n_samples` feed the run-log notes.
  res$idw_fit <- list(mode = if (auto_p) "cv" else "fixed", p = map_p,
                      profile = sel$profile, limit = sel$limit,
                      skipped = sel$skipped, fold_p = attr(res$cv_obj, "fold_power"),
                      nmax = min(idw_nmax, nrow(data)), n_samples = nrow(data),
                      select_source = if (auto_p) (if (on_measured) "measured" else "own"))
  if (auto_p) res <- .log_idw_selection(res, l, prefix)

  res$res_sf <- idw_gstat(form_ok, data, grid_p, idw_nmax, map_p)
  res$res_sf <- sanitize_spatial_predictions(res$res_sf)

  update_progress_file(l, prefix, 100, 100)
  return(res)
}

# Run-log lines of an Auto (CV) selection: the map power, the fold powers'
# spread, how well the data separate the powers (idw_flatness_note()) and, at
# an end of the family or a steep power, idw_limit_note(). A practically
# stepped map (idw_stepped()) is also a [WARN] on both channels.
.log_idw_selection <- function(res, l, prefix) {
  fit <- res$idw_fit
  surface <- if (identical(prefix, "pre")) "Predicted" else "Actual"
  head <- sprintf("[IDW] %s (%s): Auto (CV)", l, surface)
  if (!is.null(fit$skipped)) {
    res$log_msg <- paste0(res$log_msg, "\n", head, " not searched (", fit$skipped, "); p = 2.")
    return(res)
  }
  fp <- fit$fold_p
  folds_txt <- if (length(fp)) {
    sprintf("; folds selected p = %s [%s–%s]", format_power(stats::median(fp)),
            format_power(min(fp)), format_power(max(fp)))
  } else ""
  rows_txt <- if (identical(fit$select_source, "measured")) " on the measured values of all rows" else " on all rows"
  res$log_msg <- paste0(res$log_msg, "\n", head, " selected ", idw_power_text(fit$p),
                        rows_txt, folds_txt, ".")
  flat <- idw_flatness_note(fit$profile, fit$p)
  if (!is.null(flat)) res$log_msg <- paste0(res$log_msg, "\n", head, ": ", flat)
  note <- idw_limit_note(fit$limit, fit$nmax, fit[["n_samples"]], fit$p)
  if (idw_stepped(fit$p)) {
    msg <- sprintf("%s (%s): %s", l, surface, note)
    write_warning_file(l, prefix, msg)
    res$log_msg <- paste0(res$log_msg, "\n[WARN] ", msg)
  } else if (!is.null(note)) {
    res$log_msg <- paste0(res$log_msg, "\n", head, ": ", note)
  }
  res
}

#' Whether a power's map is practically the stepped nearest-neighbour surface:
#' the limit itself, or a power from IDW_STEEP_POWER up.
idw_stepped <- function(p) length(p) == 1 && isTRUE(p >= IDW_STEEP_POWER)

#' How well the data separate the powers of an Auto (CV) profile (`within_se`):
#' how many lie within one SE of the best and, unless p = 2 was selected,
#' whether p = 2 is among them. NULL without the flag.
idw_flatness_note <- function(profile, p) {
  w <- profile$within_se
  if (is.null(w) || !length(w) || anyNA(w)) return(NULL)
  total <- length(w)
  if (sum(w) <= 1) {
    return(sprintf("None of the other %d powers is within one standard error of the selected one.", total - 1L))
  }
  txt <- sprintf("%d of %d powers are within one standard error of the best", sum(w), total)
  two <- which(profile$p == 2)
  if (length(two) != 1 || isTRUE(p == 2)) return(paste0(txt, "."))
  if (w[two]) {
    paste0(txt, ", p = 2 among them: on these folds the data do not distinguish p = 2 from ",
           idw_power_text(p), ".")
  } else {
    paste0(txt, "; p = 2 is not among them.")
  }
}

#' What an Auto (CV) selection at an end of the IDW family, or at a steep power
#' (idw_stepped()), means and what is left to adjust; NULL for a power between.
#' `n` (samples) tells an equal-weights map over every sample from a local mean.
idw_limit_note <- function(limit, nmax, n = NULL, p = NULL) {
  if (identical(limit, "equal_weights")) {
    if (is.numeric(nmax) && is.numeric(n) && isTRUE(nmax >= n)) {
      return(sprintf(paste0(
        "Equal weights (p = 0) predicted the held-out samples best, and Max Neighbors reaches all %d samples, ",
        "so the map is one value, their mean: at this sample spacing, weighting by distance does not help. ",
        "A smaller Max Neighbors gives a local mean, and Ordinary Kriging models such short-range variation ",
        "as a nugget."), n))
    }
    return(sprintf(paste0(
      "Equal weights (p = 0) predicted the held-out samples best, so the map is the mean of the %s nearest ",
      "samples: at this sample spacing, weighting by distance does not help. Max Neighbors now sets the ",
      "smoothing, and Ordinary Kriging models such short-range variation as a nugget."), nmax))
  }
  if (identical(limit, "nearest_neighbour")) {
    return(paste0(
      "Auto (CV) selected the nearest-neighbour limit (p → ∞): every location takes the value of its ",
      "nearest sample, a stepped (Thiessen) surface. Ordinary Kriging or TPS give a continuous surface ",
      "that honours the same short-range continuity."))
  }
  if (idw_stepped(p)) {
    return(sprintf(paste0(
      "Auto (CV) selected %s: a sample 10%% farther than the nearest keeps under a ninth of the nearest's ",
      "weight, so the map is practically the stepped nearest-neighbour (Thiessen) surface. Ordinary ",
      "Kriging or TPS give a continuous surface that honours the same short-range continuity."),
      idw_power_text(p)))
  }
  NULL
}

# A power as the log, the panels and the run record print it: four significant
# digits (the app's display rule), no trailing zeros (2, 2.25, 0.25, 2.125);
# the nearest-neighbour limit prints as ∞.
format_power <- function(p) {
  vapply(p, function(x) if (is.infinite(x)) "∞" else format(signif(x, 4), trim = TRUE, drop0trailing = TRUE),
         character(1), USE.NAMES = FALSE)
}

# The GCV curve of a fields::Tps fit as the panel draws it and the export
# writes it: lambda against the GCV score, over the grid's positive lambdas
# with a finite score. NULL without a grid.
tps_gcv_table <- function(grid) {
  if (is.null(grid) || !NROW(grid)) return(NULL)
  df <- data.frame(lambda = as.numeric(grid[, 1]), gcv = as.numeric(grid[, 3]))
  df <- df[df$lambda > 0 & is.finite(df$gcv), , drop = FALSE]
  rownames(df) <- NULL
  if (nrow(df)) df else NULL
}

#' Which end of the spline family GCV's minimum sits at on fields' lambda grid:
#' "plane" (largest lambda, effective df ~3, the smoothest member) or
#' "interpolation" (smallest lambda; exact interpolation, lambda = 0, lies
#' beyond and GCV cannot score it). NULL for an interior minimum.
tps_gcv_end <- function(gcv) {
  if (is.null(gcv) || nrow(gcv) < 3) return(NULL)
  lam <- gcv$lambda[which.min(gcv$gcv)]
  if (lam == max(gcv$lambda)) "plane" else if (lam == min(gcv$lambda)) "interpolation"
}

#' The note for a GCV minimum at the least-smoothing end, with exact
#' interpolation's CV on the same folds beside this run's (`fit$exact_cv_rmse`,
#' `fit$run_cv_rmse`; NA when either missed a sample). Worker-side, so it
#' formats without ui_formatting.R.
tps_interpolation_end_note <- function(fit, n) {
  num <- function(x) format(signif(x, 4), trim = TRUE, drop0trailing = TRUE)
  gcv <- if (identical(fit$gcv_source, "measured")) "GCV on the measured values" else "GCV"
  head <- sprintf(paste0("%s chose the least smoothing it can evaluate (effective df %.1f of %d samples); ",
                         "exact interpolation (λ = 0), the other end of the spline family, lies beyond it ",
                         "and GCV cannot score it."), gcv, fit$eff_df, n)
  ex <- fit$exact_cv_rmse
  run <- fit$run_cv_rmse
  if (!isTRUE(is.finite(ex)) || !isTRUE(is.finite(run))) {
    return(paste(head, "The two could not be compared at every sample of these folds; set Smoothing (λ) to Exact (λ = 0) and run again to compare them."))
  }
  paste0(head, " On the same folds it cross-validates at RMSE ", num(ex), " against ", num(run),
         " for this run: ",
         if (ex < run) "exact interpolation predicts the held-out samples better; select Exact (λ = 0) under Smoothing (λ)."
         else "this run's smoothing predicts them better.")
}

#' Thin plate spline (fields::Tps) on coordinates scaled to the unit box.
#' `tps_lambda` NULL/NA/negative selects the smoothing by GCV, 0 interpolates
#' exactly, a positive value fixes it; CV folds refit under the same rule.
#' `method_params$tps_gcv_col` (an unseparated Predicted surface) makes GCV
#' select lambda on the measured values of the same rows, in the map fit and
#' every fold. `res$tps_fit` records the mode, lambda, effective df and, under
#' Auto (GCV), the GCV curve, `gcv_end` and, at the least-smoothing end, exact
#' interpolation's CV beside this run's. Any failure falls back to apply_IDW()
#' with a named warning; the fallback's `idw_fit` and log travel with it.
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
    # is.na() first (`NA < 0` errors the `if`); NA means unset, i.e. Auto (GCV).
    auto_lam <- is.null(tps_lam) || is.na(tps_lam) || tps_lam < 0
    gcv_col <- method_params$tps_gcv_col
    gcv_on <- if (auto_lam && !is.null(gcv_col) && gcv_col %in% names(data)) data[[gcv_col]] else NULL
    # Both axes share one scale (per-axis scaling would add anisotropy set by
    # the bounding box). On the measured values' GCV, that fit's GCV grid rides
    # along as an attribute. give.warnings = FALSE: the run reports a GCV
    # minimum at a grid end itself (tps_gcv_end()).
    fit_tps <- function(rows, lam = if (auto_lam) NULL else tps_lam) {
      x <- pts_sc[rows, , drop = FALSE]
      y <- data[[target_var]][rows]
      if (!is.null(lam)) return(fields::Tps(x, y, lambda = lam, scale.type = "unscaled", give.warnings = FALSE))
      if (is.null(gcv_on)) return(fields::Tps(x, y, scale.type = "unscaled", give.warnings = FALSE))
      m <- is.finite(gcv_on[rows])
      sel <- fields::Tps(x[m, , drop = FALSE], gcv_on[rows][m], scale.type = "unscaled", give.warnings = FALSE)
      mod <- fields::Tps(x, y, lambda = sel$lambda, scale.type = "unscaled", give.warnings = FALSE)
      attr(mod, "gcv_grid") <- sel$gcv.grid
      mod
    }

    gr_raw <- st_coordinates(grid_p)
    gr_sc <- cbind((gr_raw[,1]-xm)/max_range, (gr_raw[,2]-ym)/max_range)
    mod <- fit_tps(seq_len(nrow(pts_sc)))
    # Under Auto (GCV), the GCV grid of the fit that selected lambda (the
    # Scientific Analysis panel's curve).
    gcv_grid <- if (auto_lam) (if (is.null(gcv_on)) mod$gcv.grid else attr(mod, "gcv_grid"))
    gcv_tab <- tps_gcv_table(gcv_grid)
    res$tps_fit <- list(
      mode = if (auto_lam) "gcv" else if (tps_lam == 0) "exact" else "fixed",
      lambda = as.numeric(mod$lambda), eff_df = as.numeric(mod$eff.df),
      gcv = gcv_tab,
      gcv_source = if (auto_lam) (if (is.null(gcv_on)) "own" else "measured"),
      gcv_end = tps_gcv_end(gcv_tab))
    if (isTRUE(res$tps_fit$eff_df < 3.5)) {
      mode <- if (!auto_lam) "Fixed lambda" else if (!is.null(gcv_on)) "GCV on the measured values" else "GCV"
      msg <- if (identical(res$tps_fit$gcv_end, "plane")) {
        # The smoothest end of the family: nothing lies beyond it to search.
        sprintf(paste0("%s chose the smoothest end of the spline family, the least-squares plane ",
                       "(effective df %.2f; a plane has df 3), and nothing lies beyond it to search: at ",
                       "this sampling the data show no spatial structure beyond a linear trend, and the ",
                       "map is that trend."), mode, res$tps_fit$eff_df)
      } else {
        sprintf("%s produced a near-planar TPS surface (effective df %.2f; a plane has df 3). The map is dominated by a linear trend.",
                mode, res$tps_fit$eff_df)
      }
      write_warning_file(l, prefix, msg)
      res$log_msg <- paste0(res$log_msg, "\n", msg)
    }
    p_v <- fields::predict.Krig(mod, gr_sc)
    
    n_pts <- nrow(data)
    update_progress_file(l, prefix, 40, 100)
    # make_cv_folds() is seeded; LOOCV is one point per fold, so one loop serves
    # every strategy. `...` reaches fit_tps(): lam = 0 cross-validates exact
    # interpolation on the reference folds, which are kept, not rebuilt.
    domain_xy <- method_params$cv_domain_xy
    ref_folds <- NULL
    tps_cv <- function(seed, ...) {
      tps_folds <- if (seed == CV_FOLD_SEED && !is.null(ref_folds)) ref_folds else
        make_cv_folds(raw_pts, method_params$cv_strategy, n_pts, seed, domain_xy)
      if (seed == CV_FOLD_SEED) ref_folds <<- tps_folds
      cv_vals <- rep(NA_real_, n_pts)
      for (i in sort(unique(tps_folds))) {
        test_idx <- which(tps_folds == i)
        tmp_mod <- tryCatch({
          fit_tps(setdiff(seq_len(n_pts), test_idx), ...)
        }, error = function(e) NULL)

        if (!is.null(tmp_mod)) {
          cv_vals[test_idx] <- as.numeric(fields::predict.Krig(tmp_mod, pts_sc[test_idx, , drop=FALSE]))
        } else {
          cv_vals[test_idx] <- NA_real_
        }
      }

      cv <- sf::st_as_sf(
        data.frame(observed = data[[target_var]], var1.pred = cv_vals, x = raw_pts[,1], y = raw_pts[,2]),
        coords = c("x", "y"), crs = sf::st_crs(data), remove = FALSE
      )
      attr(cv, "block_fallback") <- attr(tps_folds, "block_fallback")
      attr(cv, "knndm") <- attr(tps_folds, "knndm")
      cv
    }

    cv_res <- tps_cv(CV_FOLD_SEED)
    res$cv_obj <- cv_res
    res$cv_metrics <- perform_cv(cv_res)
    res$cv_design <- cv_distance_summary(raw_pts, ref_folds, domain_xy, units = crs_unit_label(data))
    res <- add_cv_repeats(res, tps_cv, method_params, n_pts, "TPS", l, prefix)

    # A GCV minimum at the least-smoothing end: cross-validate exact
    # interpolation (where GCV is 0/0) on the same folds; compared only when
    # both scored every sample.
    if (identical(res$tps_fit$gcv_end, "interpolation")) {
      ex <- tryCatch(perform_cv(tps_cv(CV_FOLD_SEED, lam = 0), moran = FALSE), error = function(e) NULL)
      both <- !is.null(ex) && isTRUE(ex$coverage == 1) && isTRUE(res$cv_metrics$coverage == 1)
      res$tps_fit$exact_cv_rmse <- if (both) ex$rmse else NA_real_
      res$tps_fit$run_cv_rmse <- if (both) res$cv_metrics$rmse else NA_real_
      surface <- if (identical(prefix, "pre")) "Predicted" else "Actual"
      msg <- sprintf("%s (%s): %s", l, surface, tps_interpolation_end_note(res$tps_fit, n_pts))
      if (isTRUE(res$tps_fit$exact_cv_rmse < res$tps_fit$run_cv_rmse)) {
        write_warning_file(l, prefix, msg)
        res$log_msg <- paste0(res$log_msg, "\n[WARN] ", msg)
      } else {
        res$log_msg <- paste0(res$log_msg, "\n[TPS] ", msg)
      }
    }

    grid_p %>% mutate(var1.pred = as.vector(p_v))
  }, error = function(e) {
    # The fallback result travels back as an attribute, not <<- into `res`.
    # apply_IDW() is safe here: run_regional_interpolation() always sets
    # idw_p/idw_nmax, whatever the method.
    write_warning_file(l, prefix, "TPS failed, using IDW fallback.")
    fb <- apply_IDW(data, target_var, grid_p, method_params, l, prefix)
    out <- fb$res_sf
    attr(out, "tps_fallback") <- list(cv_obj = fb$cv_obj, cv_metrics = fb$cv_metrics,
                                      cv_obj_reps = fb$cv_obj_reps, idw_fit = fb$idw_fit,
                                      cv_design = fb$cv_design,
                                      log = fb$log_msg, err = e$message)
    out
  })

  fb <- attr(res$res_sf, "tps_fallback")
  if (!is.null(fb)) {
    res$tps_fit <- NULL
    res$idw_fit <- fb$idw_fit
    res$cv_obj <- fb$cv_obj
    res$cv_metrics <- fb$cv_metrics
    res$cv_obj_reps <- fb$cv_obj_reps
    res["cv_design"] <- list(fb$cv_design)
    # The fallback's own log (its IDW power selection, a failed CV) follows.
    res$log_msg <- paste0(res$log_msg, "\nTPS failed: ", fb$err, ". Falling back to IDW.", fb$log)
    attr(res$res_sf, "tps_fallback") <- NULL
  }

  res$res_sf <- sanitize_spatial_predictions(res$res_sf)
  
  update_progress_file(l, prefix, 100, 100)
  return(res)
}

#' Dispatch to the engine named by `method` (OK, RK, RFK, CK, IDW, TPS). An
#' engine error comes back as a result list with NULL `res_sf` and the message
#' in `log_msg`, never as a condition. Without `method_params$cv_domain_xy`
#' (run_regional_interpolation passes the boundary's lattice) the map's
#' locations are a regular thinning of `grid_p` (knndm_domain_points).
apply_interpolation <- function(data, target_var, method, grid_p, aux_vars, lags, method_params, l, prefix, vif_threshold = 10) {
  if (is.null(method_params$cv_domain_xy)) {
    method_params$cv_domain_xy <- tryCatch(knndm_domain_points(grid_xy = sf::st_coordinates(grid_p)),
                                           error = function(e) NULL)
  }
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
      # A covariate engine selected with no covariates (the UI guards this;
      # direct calls may not).
      stop(method, " requires at least one auxiliary covariate; none were supplied ",
           "(or all were removed by the covariate-completeness filter).")
    } else {
      stop("Unknown interpolation method: ", method)
    }
  }, error = function(e) {
    list(
      v_emp = NULL, fit = NULL, cv_metrics = NULL, model_summary = NULL, 
      rf_model = NULL, gstat_obj = NULL, res_sf = NULL, 
      log_msg = paste0("Error in apply_interpolation: ", e$message), cv_obj = NULL
    )
  })
  
  return(res)
}
