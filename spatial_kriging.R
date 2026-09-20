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
#' `summary.lm` or an RFK forest returned from a worker serialized the
#' locality's point set, prediction grid, covariate grid and kriging output
#' along with itself, and the main session then held that frame for the
#' displayed run and for every archived copy of it. Measured on 83 points at a
#' 60 m cell size: `summary.lm` 15.72 MB against 0.003 MB of summary, the
#' forest 17.60 MB against 0.46 MB of forest, and both grow with the grid.
#'
#' Only the reporting path reads these afterwards - coefficients and fit
#' statistics for RK, `randomForest::importance()` for RFK - and neither
#' touches the environment. The terms object itself is kept, so a model that
#' is handed complete `newdata` still predicts. Apply it where the result
#' crosses back to the main session, after every in-worker prediction.
#'
#' A fitted model can hold the frame TWICE: `$terms`, and the `terms` attribute
#' of the model frame it kept in `$model`. Detaching only the first frees
#' nothing from a fitted `lm` (measured: 1.532 MB before and after; 0.006 MB
#' once both are detached). The objects this is applied to today carry one each
#' - a `summary.lm` keeps no `$model`, and a `randomForest` keeps no model
#' frame - but the second holder is what a bare `lm` would arrive with.
#'
#' CK's gstat object is deliberately NOT detached: `variogram(g)` is called on
#' the stored object to draw the cross-variogram, its frame is bounded by the
#' point set the object holds anyway, and it measured 0.16 MB on the same
#' fixture.
detach_model_frame <- function(model) {
  if (is.null(model)) return(model)
  if (!is.null(model$terms)) attr(model$terms, ".Environment") <- globalenv()
  if (!is.null(attr(model$model, "terms"))) {
    attr(attr(model$model, "terms"), ".Environment") <- globalenv()
  }
  model
}

# The power search uses the SAME fold authority as every reported CV
# (make_cv_folds / resolve_cv_plan), so the power that builds the surface and
# the metrics that score it share one validation design. It matters most under
# Spatial Block CV: a random split leaves each held-out point's near neighbours
# in the training set, which favours a steeper decay, so a power tuned on
# random folds is optimistic for the blocked estimate the table reports.
# make_cv_folds seeds itself under a two-sided RNG sandbox, so no local
# set.seed and no outer with_rng_sandbox are needed here; krige.cv with an
# explicit nfold vector draws nothing. One fold vector is shared across all
# candidate powers, so the comparison stays paired.
optimize_idw_p <- function(pts, target_var, nmax = 12, cv_strategy = "auto") {
  # An explicit NULL overrides the default above, and the optimizer observer
  # ships `input$idw_nmax` straight through: a slider that has not rendered
  # yet (the sidebar section collapsed, or the method just switched to IDW)
  # reached gstat as nmax = NULL and failed the whole optimization with
  # "argument is of length zero". The run path applies `%||% 12` before the
  # dispatch and apply_IDW applies it again; this is the one entry point that
  # had no such guard.
  nmax <- nmax %||% 12
  factors <- seq(0.5, 5.0, by = 0.5)
  form <- as.formula(paste0("`", target_var, "` ~ 1"))
  n <- nrow(pts)

  fold_assign <- make_cv_folds(sf::st_coordinates(pts), cv_strategy, n, CV_FOLD_SEED)

  rmses <- vapply(factors, function(f) {
    cv <- tryCatch(
      krige.cv(form, pts, nmax = nmax, set = list(idp = f),
               nfold = fold_assign, debug.level = 0),
      error = function(e) NULL)
    if (is.null(cv)) return(Inf)
    val <- sqrt(mean(cv$residual^2, na.rm = TRUE))
    if (is.finite(val)) val else Inf
  }, numeric(1))

  best_idx <- which.min(rmses)
  if (length(best_idx) > 0 && is.finite(rmses[best_idx])) factors[best_idx] else 2.0
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
#' kept, dropped, dropped_constant, dropped_vif, vif_at_drop)`; `vif_at_drop`
#' is each VIF-dropped covariate's VIF at the step it was removed (Inf where the
#' correlation matrix was singular, so no finite VIF exists).
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
  vif_at_drop <- numeric(0)

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
        vif_at_drop[[var_to_drop]] <- Inf
        kept <- setdiff(kept, var_to_drop)
        next
      }

      max_vif <- max(vif_vals)
      if (max_vif > vif_threshold) {
        var_to_drop <- names(vif_vals)[which.max(vif_vals)]
        dropped <- c(dropped, var_to_drop)
        vif_at_drop[[var_to_drop]] <- max_vif
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
    dropped_vif = setdiff(dropped, dropped_constant),
    vif_at_drop = vif_at_drop
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
  res <- safe_run_cv(res, cv_fun(CV_FOLD_SEED), label)
  add_cv_repeats(res, cv_fun, method_params, n_data, label, l, prefix)
}

#' The CV population and fold plan a kriging engine scores: the run's
#' (`method_params$cv_data` / `cv_plan`) when supplied, else the engine's own
#' data folded by build_cv_plan().
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
    build_cv_plan(pop, method_params$cv_strategy, method_params$cv_repeats)
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
.run_kriging_cv <- function(res, cv_one, method_params, data, label, l, prefix) {
  cvp <- tryCatch(.engine_cv_plan(method_params, data), error = function(e) e)
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
# estimated mean surface, a better-calibrated trend-uncertainty term than the
# raw between-tree spread (which understates predictive uncertainty). It changes
# ONLY the RFK uncertainty (var1.var) surface, never the prediction (var1.pred).
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
                                candidates = candidates, vif_threshold = vif_threshold)
        }
        res <- .run_kriging_cv(res, cv_rk, method_params, data, "RK", l, prefix)
        res <- .log_vgm_fold_status(res, "RK", l)
      } else if (engine == "RFK") {
        rf_ntree <- if (!is.null(method_params$rf_ntree)) method_params$rf_ntree else 200
        rf_mod <- randomForest::randomForest(form_reg, data = data, ntree = rf_ntree, importance = TRUE, keep.inbag = TRUE)
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
          pa <- predict(rf_mod, grid_aux_df[s:e, , drop = FALSE], predict.all = TRUE)
          Mb <- pa$individual
          # Keep the old loud failure: predict.randomForest drops rows carrying NA
          # covariates, which used to break the length check in mutate() below.
          if (length(pa$aggregate) != (e - s + 1L)) {
            stop("RFK trend prediction returned ", length(pa$aggregate), " values for ",
                 e - s + 1L, " grid cells - the covariate grid holds missing values.")
          }
          pred_mean[s:e] <- as.numeric(pa$aggregate)
          trend_var[s:e] <- if (use_ij) {
            # Calibrated trend variance: infinitesimal jackknife of the ensemble
            # mean (Wager et al. 2014): the RF analogue of RK's lm se.fit^2.
            rf_infinitesimal_jackknife_var(Mb, rf_mod$inbag, inbag_centred = inbag_c)
          } else {
            # Ensemble spread (between-tree variance): fast stability heuristic
            # that understates predictive uncertainty (scientific_guide 7.3).
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
                                candidates = candidates, vif_threshold = vif_threshold)
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

#' Centre and scale each covariate on the rows it is handed, so the LMC's
#' cross-variograms live on a comparable scale. The map standardizes from the
#' surface's own rows and every CV fold from its own training rows, so a
#' held-out row never contributes to the centring that predicts it.
#' as.numeric: scale() returns an n x 1 MATRIX, and assigning that into an sf
#' column leaves a matrix-valued column that propagates through
#' variogram()/fit.lmc() and confuses any later dplyr verb on the object.
.ck_standardize <- function(df, aux_vars) {
  for (av in aux_vars) df[[av]] <- as.numeric(scale(df[[av]]))
  df
}

#' Build and fit the co-kriging LMC on one set of already standardized rows.
#' The map and every CV fold call it, so a fold refits exactly what the map
#' fitted, from its own rows. Returns `g` (the fitted gstat object, NULL on
#' failure), `error_msg`, the run-log text and whether the shared range was
#' seeded from the extent heuristic rather than the primary fit.
.ck_fit_lmc <- function(data_scaled, target_var, aux_vars, lags, ck_nmax) {
  log_msg <- ""
  form_ok <- reformulate("1", response = target_var)
  g <- gstat(NULL, id = target_var, formula = form_ok, data = data_scaled, nmax = ck_nmax)
  for (av in aux_vars) {
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
  # The LMC needs ONE range shared by every direct and cross variogram, which
  # is why fit.lmc's fit.ranges = FALSE default is correct and stays. But that
  # is exactly what makes the SEED range the FINAL range: unlike the seed sill
  # (see the note above) it is not inert - verified, the fitted LMC reports the
  # seed value back on every id. The seed was lags$cutoff / 2, i.e. a quarter
  # of the bounding-box diagonal, a geometric heuristic unrelated to the data,
  # while the range weighted least squares fitted to the primary variable was
  # already in hand as fit_ok_init$range[2] and thrown away apart from its
  # model family. Measured on a short-range fixture (fitted a = 243 m against
  # the heuristic's 685 m): the CK surface moved by up to 18% of the field's
  # standard deviation. Fall back to the old heuristic only when the primary
  # fit is itself a heuristic (is_fallback) or its range is unusable, so a CK
  # run is never worse informed than it was.
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

  fit_obj <- tryCatch(
    fit.lmc(vm, g, vgm(var(data_scaled[[target_var]]), m_type, lmc_range, 0, kappa = lmc_kappa),
            correct.diagonal = 1.01),
    error = function(e) structure(paste0("LMC Fit Failed: ", e$message, ". Falling back to OK."),
                                  class = "ck_lmc_error"))
  if (inherits(fit_obj, "ck_lmc_error")) {
    return(list(g = NULL, error_msg = as.character(fit_obj),
                log_msg = paste0(log_msg, as.character(fit_obj)),
                heuristic_seed = heuristic_seed))
  }
  list(g = fit_obj, error_msg = NULL,
       log_msg = paste0(log_msg, "\nLMC fitted with correct.diagonal = 1.01 (standard stabilization applied to every CK fit to keep the coregionalization matrices positive definite)."),
       heuristic_seed = heuristic_seed)
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
    # The full selected set: every CV fold re-screens THIS list on its own
    # training rows, so the screen is never decided with the held-out row's
    # measured covariates in hand.
    candidates <- aux_vars
    if (length(aux_vars) > 1) {
      vif_res <- .resolve_aux_gate(data, aux_vars, method_params, vif_threshold)
      if (length(vif_res$dropped) > 0) {
        res$log_msg <- paste0(res$log_msg, .vif_drop_log(vif_res))
        aux_vars <- vif_res$kept
      }
    }
    # For the run record, as in apply_kriging_pipeline.
    res$aux_used <- aux_vars
    res$aux_dropped <- setdiff(candidates, aux_vars)
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

    # Same standardization and LMC fit the folds run, on this surface's rows.
    data_scaled <- .ck_standardize(data, aux_vars)
    lmc <- .ck_fit_lmc(data_scaled, target_var, aux_vars, lags, ck_nmax)
    res$log_msg <- paste0(res$log_msg, lmc$log_msg)
    g <- lmc$g
    if (is.null(g)) {
      write_warning_file(l, prefix, "LMC model fit failed, using Ordinary Kriging fallback.")
    }

    if(!is.null(g)) {
      res$gstat_obj <- g
      # Every fold re-screens the covariates, re-standardizes and refits the
      # LMC on its own training rows, and the held-out rows contribute their
      # coordinates only: covariates here are co-sampled lab measurements, so
      # the map has none at its prediction locations either.
      cv_ck <- function(pop, folds, row_id, progress) {
        fold_ck <- function(train, newdata, i) {
          kept <- screen_covariates(train, candidates, vif_threshold)$kept
          if (!length(kept)) stop("the covariate screen removed every covariate in this fold")
          fit_i <- .ck_fit_lmc(.ck_standardize(train, kept), target_var, kept,
                               calc_scientific_lags(train), ck_nmax)
          if (is.null(fit_i$g)) stop(fit_i$error_msg)
          p <- predict(fit_i$g, newdata, debug.level = 0)
          list(pred = p[[paste0(target_var, ".pred")]], var = p[[paste0(target_var, ".var")]],
               meta = list(kept = kept, heuristic_seed = fit_i$heuristic_seed))
        }
        cv <- run_kriging_folds(pop, target_var, row_id, folds, fold_ck,
                                method_params$cancel_file, progress)
        attr(cv, "cv_screen") <- .fold_screen_summary(cv, aux_vars)
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

#' Inverse distance weighting with power `idw_p` (default 2) over the `idw_nmax`
#' nearest samples (default 12). Deterministic: `res_sf` carries no usable
#' variance.
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
    folds <- make_cv_folds(coords_idw, method_params$cv_strategy, nrow(data), seed)
    cv <- krige.cv(form_ok, data, nmax = idw_nmax, set = list(idp = idw_p),
                   nfold = folds, debug.level = 0)
    attr(cv, "block_fallback") <- attr(folds, "block_fallback")
    cv
  }
  res <- run_cv_with_repeats(res, cv_idw, method_params, nrow(data), "IDW", l, prefix)

  res$res_sf <- idw(form_ok, data, grid_p, nmax = idw_nmax, idp = idw_p, debug.level = 0)
  res$res_sf <- sanitize_spatial_predictions(res$res_sf)
  
  update_progress_file(l, prefix, 100, 100)
  return(res)
}

#' Thin plate spline (fields::Tps) on coordinates scaled to the unit box.
#' `tps_lambda` NULL/NA/negative selects the smoothing by GCV, 0 interpolates
#' exactly, a positive value fixes it; CV folds refit under the same rule. With
#' `method_params$tps_gcv_col` set (an unseparated Predicted surface), a GCV
#' lambda is the one GCV selects for THAT column (the measured values) on the
#' same rows, in the map fit and in every fold. Any failure falls back to
#' apply_IDW() with a named warning.
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
    # bounding box.
    fit_tps <- function(rows) {
      x <- pts_sc[rows, , drop = FALSE]
      y <- data[[target_var]][rows]
      if (!auto_lam) return(fields::Tps(x, y, lambda = tps_lam, scale.type = "unscaled"))
      if (is.null(gcv_on)) return(fields::Tps(x, y, scale.type = "unscaled"))
      m <- is.finite(gcv_on[rows])
      lam <- fields::Tps(x[m, , drop = FALSE], gcv_on[rows][m], scale.type = "unscaled")$lambda
      fields::Tps(x, y, lambda = lam, scale.type = "unscaled")
    }

    gr_raw <- st_coordinates(grid_p)
    gr_sc <- cbind((gr_raw[,1]-xm)/max_range, (gr_raw[,2]-ym)/max_range)
    mod <- fit_tps(seq_len(nrow(pts_sc)))
    res$tps_fit <- list(lambda = as.numeric(mod$lambda), eff_df = as.numeric(mod$eff.df))
    if (isTRUE(res$tps_fit$eff_df < 3.5)) {
      mode <- if (!auto_lam) "Fixed lambda" else if (!is.null(gcv_on)) "GCV on the measured values" else "GCV"
      msg <- sprintf("%s produced a near-planar TPS surface (effective df %.2f; a plane has df 3). The map is dominated by a linear trend.",
                     mode, res$tps_fit$eff_df)
      write_warning_file(l, prefix, msg)
      res$log_msg <- paste0(res$log_msg, "\n", msg)
    }
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
          fit_tps(setdiff(seq_len(n_pts), test_idx))
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
      cv
    }

    cv_res <- tps_cv(CV_FOLD_SEED)
    res$cv_obj <- cv_res
    res$cv_metrics <- perform_cv(cv_res)
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
                                      cv_obj_reps = fb$cv_obj_reps, err = e$message)
    out
  })

  fb <- attr(res$res_sf, "tps_fallback")
  if (!is.null(fb)) {
    res$tps_fit <- NULL
    res$cv_obj <- fb$cv_obj
    res$cv_metrics <- fb$cv_metrics
    res$cv_obj_reps <- fb$cv_obj_reps
    res$log_msg <- paste0(res$log_msg, "\nTPS failed: ", fb$err, ". Falling back to IDW.")
    attr(res$res_sf, "tps_fallback") <- NULL
  }

  res$res_sf <- sanitize_spatial_predictions(res$res_sf)
  
  update_progress_file(l, prefix, 100, 100)
  return(res)
}

#' Dispatch to the engine named by `method` (OK, RK, RFK, CK, IDW, TPS). An
#' engine error comes back as a result list with NULL `res_sf` and the message
#' in `log_msg`, never as a condition.
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
      log_msg = paste0("Error in apply_interpolation: ", e$message), cv_obj = NULL
    )
  })
  
  return(res)
}
