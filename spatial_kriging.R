# spatial_kriging.R - interpolation engines and their shared plumbing:
# apply_OK/RK/RFK/CK/IDW/TPS via apply_kriging_pipeline/apply_interpolation,
# VIF gating (check_vif, detect_multicollinearity_engine), krige_covariates,
# the IDW power selection (select_idw_power, idw_kernel_predict),
# rf_infinitesimal_jackknife_var, prediction sanitizers.
# Sourced via spatial_helpers.R.

#' Which engines produce a genuine prediction variance?
#'
#' Only the kriging family does. `gstat::idw()` still returns a `var1.var`
#' column, but it is all NA (an inverse-distance weighting is a deterministic
#' exact interpolator with no variance model behind it), and apply_TPS's IDW
#' fallback inherits that column too — so a bare `"var1.var" %in% names()` test
#' is not enough to decide whether an uncertainty product exists. Single source
#' of truth for the pipeline's rasterization, the export registry and the map
#' viewer's SE and variance views.
METHODS_WITH_VARIANCE <- c("OK", "RK", "RFK", "CK")

# Rows per block for the RFK grid trend prediction, expressed in matrix CELLS:
# predict.all materialises an n_rows x ntree double matrix, so 4e6 cells is ~32 MB
# regardless of the tree count. A forest predicts each row independently, so
# blocking is exact - this is a memory budget, never a numeric knob.
.RFK_PREDICT_BLOCK_CELLS <- 4e6

method_has_variance <- function(method) {
  !is.null(method) && length(method) == 1L && !is.na(method) &&
    method %in% METHODS_WITH_VARIANCE
}

#' Detach a fitted model from the frame it was fitted in.
#'
#' A formula built inside a function carries that function's whole frame as its
#' environment, and the `terms` object a model keeps inherits it. So an RK
#' `summary.lm` returned from a worker would serialize the locality's point
#' set, prediction grid, covariate grid and kriging output along with itself,
#' and the main session would hold that frame for the displayed run and for
#' every archived copy of it. Measured on 83 points at a 60 m cell size:
#' `summary.lm` 15.72 MB against 0.003 MB of summary, growing with the grid.
#' The RFK forest is grown through randomForest's matrix interface and has no
#' terms object, so it needs no detaching.
#'
#' Only the reporting path reads the summary afterwards (coefficients and fit
#' statistics), and it does not touch the environment. The terms object itself is kept, so a model that
#' is handed complete `newdata` still predicts. Apply it where the result
#' crosses back to the main session, after every in-worker prediction.
#'
#' A fitted model can hold the frame TWICE: `$terms`, and the `terms` attribute
#' of the model frame it kept in `$model`. Detaching only the first frees
#' nothing from a fitted `lm` (measured: 1.532 MB before and after; 0.006 MB
#' once both are detached). A `summary.lm` keeps no `$model`, but the second
#' holder is what a bare `lm` would arrive with.
#'
#' CK's gstat object is not passed through here: its formulas' frame holds the
#' rows the object already carries as data (0.16 MB on the same fixture), and
#' the cross-variogram panel draws the empirical variogram stored with the fit
#' (`monolith_vm`), not a recomputation from the object.
detach_model_frame <- function(model) {
  if (is.null(model)) return(model)
  if (!is.null(model$terms)) attr(model$terms, ".Environment") <- globalenv()
  if (!is.null(attr(model$model, "terms"))) {
    attr(attr(model$model, "terms"), ".Environment") <- globalenv()
  }
  model
}

# ── IDW power selection (Auto (CV)) ─────────────────────────────────────────
# The powers the selection searches cover the whole IDW family, so no optimum
# can lie outside them. p = 0 gives each of the Max Neighbors nearest samples
# the same weight (their plain mean, the family's lower end); steps of 0.25 run
# to 6 and coarser ones to IDW_MAX_FINITE_POWER; Inf stands for the
# nearest-neighbour limit (p -> Inf: every location takes its nearest sample's
# value), the family's upper end. Below IDW_SELECT_MIN_N distinct samples a CV
# power search is not an estimate, and the IDW default p = 2 is used instead.
# From IDW_STEEP_POWER up, a sample 10% farther than the nearest keeps under a
# ninth of the nearest's weight ((1/1.1)^24 = 0.10): the map is practically the
# stepped nearest-neighbour surface, and a selection there is read like the
# limit.
IDW_MAX_FINITE_POWER <- 48
IDW_POWER_GRID <- c(seq(0, 6, by = 0.25), 7, 8, 9, 10, 12, 14, 16, 20, 24, 32, IDW_MAX_FINITE_POWER, Inf)
IDW_SELECT_MIN_N <- 5L
IDW_STEEP_POWER <- 24

#' Exact IDW predictions at `test_xy` for every power in `powers` at once: the
#' k = min(nmax, n_train) nearest training samples (FNN::get.knnx) weighted by
#' d^-p. A test location at distance 0 from a sample takes that sample's value,
#' as gstat does. Returns an n_test x length(powers) matrix. It reproduces
#' gstat's IDW cross-validation (test-idw-selection.R) and is the power
#' selection device only: every map and every reported prediction comes from
#' gstat::idw() (idw_gstat). Equidistant neighbours on a lattice may be ranked
#' differently from gstat, which can only move a near-tied selection.
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

#' Pooled cross-validation RMSE of every power on one fold vector, and whether
#' the data separate it from the best (the first minimum): `within_se` is TRUE
#' when its mean squared error exceeds the best's by no more than one standard
#' error of the per-sample squared-error differences on the same folds (a
#' paired comparison). That standard error treats the samples as independent,
#' so under spatially correlated errors it is too small, and more powers are
#' indistinguishable than marked. The flag reports; it selects nothing.
idw_cv_profile <- function(xy, v, folds, powers, nmax) {
  e2 <- (idw_cv_predictions(xy, v, folds, powers, nmax) - v)^2
  mse <- colMeans(e2)
  d <- e2 - e2[, which.min(mse)]
  se <- apply(d, 2, stats::sd) / sqrt(nrow(e2))
  data.frame(p = powers, rmse = sqrt(mse), within_se = colMeans(d) <= se)
}

#' The power a row set selects: folds from make_cv_folds() under `strategy`
#' (the run's own fold authority, seeded inside its RNG sandbox; kNNDM matches
#' this row set's folds to the map's locations `domain_xy`), or `folds` when
#' the caller already holds that very vector, the pooled CV RMSE of every
#' power on that one fold vector (idw_cv_profile, with which powers the data
#' do not separate from the best), and the first minimum (the smallest power
#' among ties). `limit` names a selection at an end of the family
#' (idw_power_limit): the grid spans the whole family, so an end is a finding
#' about the data, never a truncated search. Below IDW_SELECT_MIN_N rows, or on
#' a target without usable variance (every power predicts it alike), nothing
#' is searched: p = 2 and `skipped` names the reason.
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

#' Covariate screen for RK/RFK/CK and the classification suite. Reports pairs
#' with |r| > `pairwise_threshold`, drops degenerate (constant) covariates, then
#' drops the highest-VIF covariate one at a time while any VIF exceeds
#' `vif_threshold` (`Inf` = keep all). Returns `list(has_collinearity, pairs,
#' kept, dropped, dropped_constant, dropped_vif)`.
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

#' detect_multicollinearity_engine() over every numeric column of `df`,
#' returning only the kept/dropped sets.
check_vif <- function(df, threshold = 10) {
  res <- detect_multicollinearity_engine(df, vif_threshold = threshold)
  return(list(kept = res$kept, dropped = res$dropped,
              dropped_constant = res$dropped_constant,
              dropped_vif = res$dropped_vif))
}

#' The constant/VIF covariate screen, in ONE place: the surface's own gate and
#' every cross-validation fold call it, so a fold can never screen by a
#' different rule than the map it scores. Fewer than two candidates pass
#' through — the gate needs a pair to compare, and a sole degenerate covariate
#' is deliberately kept and named by `run_regional_interpolation` instead.
#' `detect_multicollinearity_engine`'s "only one covariate remains" notice is
#' muffled here: a per-fold screen raises it once per fold (measured: 30 for 30
#' folds) and it says nothing the covariate-gate run-log line does not.
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

#' Resolve the covariate gate for one surface. `run_regional_interpolation`
#' resolves it up front on this exact point set and hands the survivors down as
#' `method_params$aux_kept` (so dropped covariates are never kriged onto the
#' grid); a direct engine call recomputes it. Since `aux_kept` carries no
#' provenance, the constants among the dropped set are re-derived here — an
#' exact, local test costing one sd() per dropped column — so the run log names
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
# falls back to IDW if kriging fails: an error, a repeated location or a
# prediction missing anywhere.
#' `on_var(i, total)` is an optional hook invoked after each covariate surface.
#' The classification pipeline uses it to tick the progress bar and to poll its
#' cancel flag (one covariate is the coarsest interruptible unit here, since
#' gstat's krige() call is a black box). NULL = the original behaviour.
#' Returns `grid_aux`, the run-log text `log_msg` and `fallback`, the
#' covariates whose surface is the IDW fallback.
krige_covariates <- function(data, grid_p, aux_vars, lags, method_params, on_var = NULL) {
  grid_aux <- grid_p
  log_msg <- ""
  fallback <- character(0)
  n_av <- length(aux_vars)
  for(i in seq_along(aux_vars)) {
    av <- aux_vars[i]
    # gstat builds its model frame through sp, whose data.frame conversion runs
    # make.names() over the columns: a header such as "Fe (mg/kg)" is then "not
    # found" even when backticked, and the IDW fallback fails the same way. Each
    # covariate is kriged under one syntactic alias; the grid keeps its name.
    d_av <- data[av]
    names(d_av)[names(d_av) == av] <- ".mn_cov"
    kr_res <- tryCatch({
      # Two samples at one location make the kriging system singular. LAPACK's
      # Cholesky factorisation then fails or completes on rounding noise,
      # depending on the platform and even the row order: gstat returns NA, or
      # a finite surface that is arbitrary along the singular direction. So a
      # repeated location is a failed fit before any solve. Callers merge
      # co-located samples first (merge_colocated).
      if (anyDuplicated(sf::st_coordinates(d_av))) {
        stop("two samples share a location, so the kriging system is singular")
      }
      v_emp_av <- variogram(.mn_cov ~ 1, d_av, width = lags$width, cutoff = lags$cutoff)
      fit_av <- robust_vgm_fit(v_emp_av, d_av$.mn_cov)
      res_av <- krige(.mn_cov ~ 1, d_av, grid_p, model = fit_av, debug.level = 0)
      # gstat returns NA with no error and, at debug.level 0, no warning where
      # its factorisation fails. A surface with holes is a failed fit, so it
      # takes the fallback below.
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

#' Evaluate one CV expression into `res`: the CV object and its perform_cv()
#' metrics. A failure is written to the run log and leaves both empty instead
#' of stopping the engine. CV residuals are read off `res$cv_obj` by whoever
#' needs them (get_cv_residuals), so they are never carried as a separate
#' vector whose length has to match a point set it does not name.
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
  # A fold screens its covariates on its own training rows, so it can keep a
  # different set than the surface does. Say so when it happened: it is the
  # explanation for a CV number that does not match the map's covariate list.
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
    # instead of parking it where the single-realization run finishes.
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
  # A supplied population can be smaller than the surface's own point set (OK
  # scored on the covariate-complete rows). Below three samples there is
  # nothing to fold: the CV is skipped with a named log line and the map, which
  # is fitted on the full point set, still runs.
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
# (Wager, Hastie & Efron 2014), with the Monte-Carlo bias correction. This is
# the random-forest analogue of RK's lm `se.fit^2`: the sampling variance of the
# estimated mean surface. The raw between-tree spread is a different quantity
# (model instability) with no guaranteed ordering against it, and neither
# establishes interval calibration (scientific_guide 7.3). It changes ONLY the
# RFK uncertainty (var1.var) surface, never the prediction (var1.pred).
#   pred_individual : n_pred x B matrix of per-tree predictions (predict.all$individual)
#   inbag           : n_train x B matrix of in-bag counts (randomForest keep.inbag = TRUE)
# Returns a length-n_pred variance vector, negatives (from the bias correction)
# truncated to 0. Chunked over prediction rows to bound the n_train x n_pred
# intermediate. Returns NA when fewer than two trees (variance undefined).
#   inbag_centred   : optional pre-centred `inbag`. The centring depends only on
#                     the fitted forest, but the RFK grid loop calls this once per
#                     prediction block (50 times on a 1e6-cell grid at ntree = 200),
#                     so that loop centres once and passes the result in. NULL keeps
#                     the self-contained behaviour for every other caller.
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

#' The fold function OK and the OK fallback cross-validate with: lags,
#' empirical variogram and the full `robust_vgm_fit` candidate search are
#' re-estimated from the fold's TRAINING rows, then the target is kriged at the
#' held-out coordinates. Nothing about a held-out row but its position enters.
#'
#' `fixed_fit` is the one exception: an applied manual model encodes the user's
#' judgement and cannot be refitted, so it is reused in every fold and the CV is
#' labelled conditional. `vgm_col` is D7 — the Predicted surface kriged with the
#' variogram of the MEASURED values, refitted here from the training rows that
#' carry one. (The map's shared fit comes from all Actual rows; the fold's from
#' its own training rows. Same accepted asymmetry as the RK covariate surfaces.)
#' The candidate search is deliberately run in full per fold: do not shrink it
#' or warm-start it from the full-data fit.
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
  # The mapped surface uses no covariate; the screen's record (aux_dropped) stands.
  res$aux_used <- character(0)
  form_ok <- reformulate("1", response = target_var)
  res$v_emp <- variogram(form_ok, data, width = lags$width, cutoff = lags$cutoff)
  res$fit <- robust_vgm_fit(res$v_emp, data[[target_var]])
  res$res_sf <- krige(form_ok, data, grid_p, model = res$fit, debug.level = 0)
  if (tag_model_type) res$res_sf$model_type <- "Ordinary Kriging (Fallback)"
  # Same plan, fold runner and schema as every kriging engine. The fallback
  # never reuses a supplied model: it exists because the covariate engine
  # failed, so there is nothing the user applied to this surface.
  fold_okfb <- .ok_fold_fun(form_ok, target_var)
  cv_one <- function(pop, folds, row_id, progress) {
    run_kriging_folds(pop, target_var, row_id, folds, fold_okfb, method_params$cancel_file, progress)
  }
  res <- .run_kriging_cv(res, cv_one, method_params, data, cv_label, l, prefix)
  .log_vgm_fold_status(res, cv_label, l)
}

#' Append a run-log block naming every fold whose variogram was not `ok`, with
#' the locality it belongs to - a count alone says neither which folds nor in
#' what way they were degraded, and a log line with no locality is unreadable
#' on a multi-locality run.
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
    # Every fold re-estimates the variogram from its own training rows, unless
    # the user applied a manual model (conditional CV, D6). With the measured
    # variogram shared onto the Predicted surface (D7) the fold refits THAT
    # variogram, from the training rows carrying a measured value.
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

    # gstat returns NA for every location, without a condition, when the
    # kriging system is singular - measured (gstat 2.1.5) for a zero-nugget
    # Gaussian model over samples a centimetre apart. The model is never
    # altered here: a nugget small enough to leave it unchanged also leaves the
    # system unstable, and the all-NA surface is skipped and reported by
    # run_regional_interpolation. This names the likely cause.
    if (!is.null(res$res_sf) && "var1.pred" %in% names(res$res_sf) &&
        all(is.na(res$res_sf$var1.pred)) &&
        isTRUE(vgm_smooth_nugget_share(res$fit) < VGM_SMOOTH_NUGGET_WARN_SHARE)) {
      res$log_msg <- paste0(res$log_msg,
        " [OK] The Gaussian/Matern variogram has a nugget below 5% of its sill, which ",
        "makes the kriging system singular at these sample locations; add a nugget.")
    }
  } else {
    update_progress_file(l, prefix, 10, 100)
    # The full selected set: every CV fold re-screens THIS list on its own
    # training rows, so the screen is never decided with the held-out row's
    # measured covariates in hand.
    candidates <- aux_vars
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
      }
      # For the run record: what the trend model is built on, and what the
      # screen removed (the selected list says neither). The OK fallback resets
      # the first to none.
      res$aux_used <- aux_vars
      res$aux_dropped <- setdiff(candidates, aux_vars)
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

      if (!is.null(method_params$grid_aux)) {
        grid_aux <- method_params$grid_aux
      } else {
        krig_cov <- krige_covariates(data, grid_p, aux_vars, lags, method_params)
        grid_aux <- krig_cov$grid_aux
        res$log_msg <- paste0(res$log_msg, krig_cov$log_msg)
      }

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
        # The matrix interface, not the formula: randomForest's formula method
        # rebuilds its frame with data.frame(), whose make.names() turns a
        # covariate such as "Fe (mg/kg)" into a name the formula cannot find.
        # The forest is the same (same predictor matrix, same response), and
        # it carries no formula environment.
        # Seeded like the fold forests (CV_FOLD_SEED + fold label >= 1, so this
        # seed is shared with none of them). On furrr's per-element stream the
        # map depended on the locality's POSITION in the run: deselecting another
        # locality changed this one's surface. One seed for every locality makes
        # no two forests dependent, since each is grown on its own data.
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
        # predict.all returns an n_grid x ntree matrix in ONE allocation; at a fine
        # fixed resolution (the grid is capped at ~4M cells) that is multi-GB inside
        # a PSOCK worker, and the OOM surfaces only as "Parallel Interpolation
        # Failed". A random forest predicts each row independently, and both variance
        # estimators are row-independent too, so a row block yields exactly the values
        # the full grid would: memory only, bit-identical output
        # (.RFK_PREDICT_BLOCK_CELLS).
        grid_aux_df <- if (inherits(grid_aux, "sf")) sf::st_drop_geometry(grid_aux) else as.data.frame(grid_aux)
        n_grid <- nrow(grid_aux_df)
        blk <- max(1L, min(n_grid, as.integer(ceiling(.RFK_PREDICT_BLOCK_CELLS / max(1L, rf_ntree)))))
        pred_mean <- numeric(n_grid)
        trend_var <- numeric(n_grid)
        # Centre the in-bag matrix ONCE: it is a property of the fitted forest,
        # not of the prediction block, so recomputing it per block was pure
        # repetition. Same matrix, same result.
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

#' Build and fit the co-kriging LMC from the target rows (`target_rows`: every
#' location with a measured target) and the covariate-only rows (`extra_rows`:
#' locations without the target that measure a covariate; NULL or empty on
#' isotopic data). The map and every CV fold call it, so a fold refits exactly
#' what the map fitted, from its own rows.
#'
#' Each variable is a gstat id with its own data: the target on the target
#' rows, each covariate on every row that measures it, standardized on those
#' rows. All empirical variograms use the primary's lags. A direct variogram is
#' computed on its variable's own rows; a cross-variogram on the rows carrying
#' BOTH variables, as an isotopic two-variable variogram. gstat's own
#' cross-variogram of ids with different locations is the pseudo
#' cross-variogram, which contains half the squared difference of the two
#' means (Papritz, Künsch & Webster 1993); with the target on its raw scale and
#' the covariates standardized, that offset would dominate it, so it is never
#' computed (variogram(g, pseudo = 0) also crashes R). On isotopic data every
#' piece equals the variogram gstat computes for the whole object.
#'
#' Returns `g` (the fitted gstat object, NULL on failure) carrying
#' `monolith_ids` (id -> column) and `monolith_vm` (the empirical variogram the
#' LMC was fitted to), `error_msg`, the run-log text, whether the shared range
#' was seeded from the extent heuristic, the target's id, `counts` (per
#' covariate: rows measuring it, and how many of those carry the target) and
#' `design` ("isotopic" or "heterotopic").
.ck_fit_lmc <- function(target_rows, extra_rows, target_var, aux_vars, lags, ck_nmax) {
  log_msg <- ""
  # gstat reaches its model frame through sp, whose data.frame conversion runs
  # make.names() over the columns, so a covariate such as "Fe (mg/kg)" is "not
  # found" even when backticked. The LMC is fitted on syntactic ids
  # (make.names, which leaves a syntactic name unchanged) over the variables it
  # uses only; `monolith_ids` maps each id back to its column for display.
  vars <- c(target_var, aux_vars)
  ids <- make.names(vars, unique = TRUE)
  rows <- target_rows[vars]
  if (!is.null(extra_rows) && nrow(extra_rows) > 0) rows <- rbind(rows, extra_rows[vars])
  # Each covariate centred and scaled on the rows that measure it (scale()
  # skips NA), so the cross-variograms live on a comparable scale and a CV
  # fold, handed its own training rows, standardizes from those alone.
  # as.numeric: scale() returns an n x 1 matrix.
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

  # One empirical variogram - direct (one id) or cross (two ids, the pair
  # only) - on the rows in `mask`, through gstat's default method on plain
  # values and coordinates: the computation variogram(g) runs for each id
  # pair, without the sp conversion and CRS queries a gstat object costs on
  # every call (profiled: most of a fit's time with a separate object per
  # piece).
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

  # The single `model` argument is used by gstat as the STARTING model for
  # every direct and cross variogram (fit.lmc copies it over each id), so it
  # seeds the standardized covariate variograms, whose sills are 1.0 by
  # construction, with the target's raw variance, which can be many orders of
  # magnitude larger. That is inert and must not be "fixed" by rescaling the
  # seed: fit.lmc calls fit.variogram with fit.ranges = FALSE, and with the
  # range and model type held fixed the variogram is LINEAR in its sill
  # parameters, so the weighted least-squares solve has a closed-form optimum
  # that does not depend on the starting sill. Verified 2026-07-20: starting
  # sills spanning 1e-6 to 1e12 on the same empirical variogram all return a
  # bit-identical fitted sill, and per-id seeding from each variogram's own
  # empirical plateau reproduces the fitted LMC exactly (target variances up to
  # 3.5e8, 3 covariates). The starting values would matter immediately if
  # fit.ranges were ever set TRUE (the same probe then spread the fitted sill
  # over 1.0 to 3179 with no-convergence warnings), so scale the seeds per id
  # at the same time as any such change.
  # The LMC needs ONE range shared by every direct and cross variogram, which
  # is why fit.lmc's fit.ranges = FALSE default is correct. That makes the SEED
  # range the FINAL range (verified: the fitted LMC reports the seed back on
  # every id), so it is the range weighted least squares fitted to the primary
  # variable, fit_ok_init$range[2]: a geometric heuristic in its place moved a
  # short-range CK surface by up to 18% of the field's standard deviation
  # (fitted a = 243 m against the heuristic's 685 m). The extent heuristic
  # (cutoff / 2, a quarter of the bounding-box diagonal) is used only when the
  # primary fit is itself a heuristic (is_fallback) or its range is unusable.
  lmc_range <- suppressWarnings(as.numeric(fit_ok_init$range[2])[1])
  # gstat's `a` only means a ground distance once the FAMILY and, for Matern,
  # the smoothness are fixed. vgm() defaults kappa to 0.5 while robust_vgm_fit
  # fits Matern at 1.5, so seeding "Mat" without its kappa would carry the
  # range across a smoothness change (practical range 3a at nu = 0.5 against
  # 4.75a at nu = 1.5) and quietly stretch the modelled correlation length.
  # Inert for Sph/Exp/Gau, which ignore kappa - verified identical curves.
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

  # A cross-variogram from the rows carrying both variables, as the isotopic
  # two-variable variogram of those rows; their direct variograms come from
  # each variable's own rows.
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

# Co-Kriging: predict the target jointly with its auxiliary variables through a
# linear model of coregionalization (LMC). `data` holds the target rows (every
# location with a measured target); `method_params$ck_extra` the covariate-only
# rows (locations without the target that measure a selected covariate), which
# enter the covariates' data only (heterotopic design; empty = isotopic).
# Covariates are standardized on their own rows so the cross-variograms live on
# a comparable scale (.ck_fit_lmc); the LMC is stabilized with
# correct.diagonal = 1.01, and the whole fit falls back to Ordinary Kriging if
# it fails.
apply_CK <- function(data, target_var, grid_p, lags, method_params, aux_vars, l = "region", prefix = "act", vif_threshold = 10) {
  res <- init_interpolation_res()

  update_progress_file(l, prefix, 10, 100)

  # Search neighbourhood for the whole co-kriging system. This is a modelling
  # parameter (it sets how local the stationarity assumption is), not a pure
  # speed knob, so it is user-selectable; 15 is the documented default.
  ck_nmax <- if (!is.null(method_params$ck_nmax) && is.finite(method_params$ck_nmax)) method_params$ck_nmax else 15
  extra <- method_params$ck_extra
  surface <- if (identical(prefix, "pre")) "Predicted" else "Actual"

  ck_res <- tryCatch({
    # A cross-variogram with the target needs the covariate measured at target
    # locations: below CK_MIN_COLLOCATED of them it cannot be estimated, so
    # the covariate is dropped here, before the collinearity screen decides
    # among the rest. The folds screen THIS candidate list on their own
    # training rows and do not re-apply the rule.
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
    # The same multicollinearity screen RK/RFK apply, and the LMC needs it at
    # least as badly: collinear covariates make the coregionalization matrices
    # near-singular and fit.lmc() fails into the Ordinary Kriging fallback. It
    # runs on the locations carrying every candidate, covariate-only ones
    # included (.ck_screen).
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
    # gstat() with only the primary variable still fits (fit.lmc succeeds on a
    # single variable) and would return ordinary kriging under a 15-point
    # neighbourhood labelled Co-Kriging. Co-kriging with no secondary variable
    # is not co-kriging, so the named OK fallback takes over instead.
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
      # A smooth structure (Gaussian, or Matern nu >= 1) with a negligible
      # nugget makes the kriging matrices near-singular, the more so the denser
      # the data (Posa 1989); dense covariate-only rows are exactly where
      # co-kriging gains, so the condition is named.
      smooth <- names(Filter(function(m) isTRUE(vgm_smooth_nugget_share(m) < VGM_SMOOTH_NUGGET_WARN_SHARE),
                             g$model[names(g$data)]))
      if (length(smooth)) {
        res$log_msg <- paste0(res$log_msg, "\n[CK] ", l, " (", surface, "): the LMC's ",
          as.character(g$model[[1]]$model[2]), " structure has a nugget below 5% of the sill for ",
          paste(ck_id_columns(g)[smooth], collapse = ", "),
          "; such a system can be ill-conditioned, the more so with dense covariate data. ",
          "Compare its cross-validation with the other engines' before relying on the map.")
      }
      # Every fold re-screens the covariates, re-standardizes and refits the
      # LMC on its training rows plus the covariate-only rows. A held-out
      # location loses its target AND its collocated covariate values and
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
#' is Auto (CV): the map uses the power select_idw_power() picks on all rows,
#' and every CV fold re-selects it from its own training rows (inner folds
#' under the same strategy), so the reported CV includes the selection step.
#' With `method_params$idw_select_col` set (an unseparated Predicted surface)
#' the selection reads that column, the measured values, on the rows that
#' carry it. `res$idw_fit` records the mode, the map power (Inf = the
#' nearest-neighbour limit), its CV-RMSE profile, the end of the family it
#' reached if any (`limit`), the fold powers of realization 1 and, under Auto
#' (CV), whose values the power was selected on (`select_source`: "own" or
#' "measured"). Under kNNDM every fold set, the nested ones included, is
#' matched to the map's locations `method_params$cv_domain_xy`; `res$cv_design`
#' is the distance match of the reference folds.
#' Deterministic: `res_sf` carries no usable variance.
apply_IDW <- function(data, target_var, grid_p, method_params, l = "region", prefix = "act") {
  res <- init_interpolation_res()

  update_progress_file(l, prefix, 20, 100)
  form_ok <- reformulate("1", response = target_var)
  # run_regional_interpolation always populates idw_nmax; the engine is
  # publicly callable and apply_TPS's fallback relies on it, so it resolves
  # the same default as the point-error surface.
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
  # The reference CV's folds. Where every row carries the values the power is
  # selected on, the map's selection folds the same rows under the same seed,
  # so it takes these (under kNNDM, one fold search serves both).
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
      # Nested: each fold selects its power from its own training rows before
      # gstat predicts its held-out rows, so no held-out value reaches the power
      # that predicts it. The object keeps krige.cv's columns and class.
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

  # `nmax`: the neighbours an equal-weights map averages (fewer where the
  # locality has fewer samples), and `n_samples` the samples the map is drawn
  # from, for the notes that name them.
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

# Run-log lines of an Auto (CV) power selection: the map power, the spread of
# the fold powers, how well the data separate the powers (idw_flatness_note)
# and, for a selection at an end of the family or at a steep power, what it
# means and what is left to adjust (idw_limit_note). A practically stepped map
# (idw_stepped: the nearest-neighbour limit, or a power from IDW_STEEP_POWER
# up) is also a warning on both channels (progress panel and a [WARN] run-log
# line): it should not go unnoticed.
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

#' How well the data separate the powers of an Auto (CV) profile
#' (idw_cv_profile's `within_se`): how many are within one standard error of
#' the best and, unless the selection is p = 2, whether p = 2 is among them.
#' One sentence for the run log and the IDW Power Selection panel; NULL
#' without the flag.
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

#' What an Auto (CV) selection at an end of the IDW family, or at a steep
#' power (idw_stepped), means and what is left to adjust: one sentence pair for
#' the run log and the IDW Power Selection panel. NULL for a power between.
#' `n`, the samples the map is drawn from, tells an equal-weights map over
#' every sample (one value) from a local mean.
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

#' Which end of the spline family GCV's minimum sits at on fields' lambda grid
#' (tps_gcv_table): "plane", the largest lambda, whose effective df is ~3, the
#' least-squares plane and the family's smoothest member, with nothing beyond
#' it; "interpolation", the smallest lambda, the least smoothing GCV
#' evaluates, beyond which lies exact interpolation (lambda = 0), which GCV
#' cannot score (0/0). NULL for a minimum inside the grid.
tps_gcv_end <- function(gcv) {
  if (is.null(gcv) || nrow(gcv) < 3) return(NULL)
  lam <- gcv$lambda[which.min(gcv$gcv)]
  if (lam == max(gcv$lambda)) "plane" else if (lam == min(gcv$lambda)) "interpolation"
}

#' What a GCV minimum at the least-smoothing end of fields' grid means, with
#' the run's cross-validation of exact interpolation on the same folds beside
#' this run's (`fit$exact_cv_rmse`, `fit$run_cv_rmse`, NA when either could
#' not score every sample): one note for the run log and the TPS Smoothing
#' Selection panel. Worker-side, so it formats without ui_formatting.R.
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
#' exactly, a positive value fixes it; CV folds refit under the same rule. With
#' `method_params$tps_gcv_col` set (an unseparated Predicted surface), a GCV
#' lambda is the one GCV selects for THAT column (the measured values) on the
#' same rows, in the map fit and in every fold. `res$tps_fit` records the mode,
#' the fitted lambda and effective df and, under Auto (GCV), the GCV curve of
#' the fit that set lambda, the end of the family its minimum reached if any
#' (`gcv_end`, tps_gcv_end) and, at the least-smoothing end, exact
#' interpolation's cross-validation beside this run's. Any failure falls back
#' to apply_IDW() with a named warning; the fallback's `idw_fit` and log travel
#' with it.
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
    # is.na() first: `NA < 0` is NA, which errors the `if` and silently sent
    # the whole surface down the IDW fallback. NA is treated as "unset", i.e.
    # the documented Auto (GCV) default, same as NULL / lambda < 0.
    auto_lam <- is.null(tps_lam) || is.na(tps_lam) || tps_lam < 0
    gcv_col <- method_params$tps_gcv_col
    gcv_on <- if (auto_lam && !is.null(gcv_col) && gcv_col %in% names(data)) data[[gcv_col]] else NULL
    # Fits the spline on the given rows. Both axes already share one scale;
    # per-axis scaling would introduce anisotropy determined by the sample
    # bounding box. On the measured values' GCV, the GCV grid of that selecting
    # fit rides along as an attribute (read once, for the map fit's record).
    # give.warnings = FALSE: fields prints its own note when GCV's minimum sits
    # at an end of its lambda grid; the run reports that end itself
    # (tps_gcv_end), with what it means.
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
    # The run's own record of how lambda was set: under Auto (GCV) the GCV
    # grid of the fit that selected it (the curve the Scientific Analysis panel
    # draws), with whose values it was computed on.
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
    # make_cv_folds seeds itself and restores the caller's RNG; LOOCV collapses
    # to one point per fold, so a single loop covers every strategy. `...`
    # reaches fit_tps: lam = 0 cross-validates exact interpolation on the same
    # folds, which are the reference realization's, kept rather than rebuilt
    # (a kNNDM fold search is not free).
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

    # GCV's minimum at its least-smoothing end: exact interpolation lies
    # beyond it, where GCV is 0/0, so the run cross-validates it on the same
    # folds and sets it beside this run's figure. Both are compared only when
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
      # Reached only when a covariate-driven engine was selected with no
      # covariates: every branch above tests `length(aux_vars) > 0`, and the
      # "unknown method" stop() below would tell the user RK was an
      # unrecognised method. The UI path guards this, but the engine is
      # publicly callable.
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
