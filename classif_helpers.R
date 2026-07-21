# classif_helpers.R — supervised classification engine for the Classification
# Suite module (Digital Soil Mapping style categorical prediction).
#
# Scope (Phase 1): a shared preprocessing recipe, three learners behind a
# parsnip/workflows backbone (multinomial logistic, random forest, XGBoost),
# an expandable hyperparameter-tuning registry, spatial and standard
# cross-validation, classification performance metrics + confusion matrix, and
# raster prediction (class / per-class probability / entropy) reusing the same
# krige_covariates() covariate surfaces the kriging engines build.
#
# The engine here is deliberately decoupled from Shiny and from grid
# construction: predict_classification_surface() takes a plain covariate
# data.frame (the interpolated grid), so it is unit-testable without the app.
#
# Conventions inherited from spatial_helpers.R: any set.seed() saves and
# restores the caller's .Random.seed (two-sided sandbox); projected (metric)
# coordinates only.

# renv dependency discovery: parsnip loads the engine packages from the string
# names passed to set_engine() (e.g. "ranger", "xgboost", "nnet"), which renv's
# static analysis cannot detect, so renv::snapshot() would otherwise omit them
# from the lockfile. This never-executed block makes them discoverable; the
# packages are still loaded on demand by parsnip at fit time, not attached here.
if (FALSE) {
  requireNamespace("ranger")
  requireNamespace("xgboost")
  requireNamespace("nnet")
}

# ── Seed sandbox ────────────────────────────────────────────────────────────
# Same two-sided convention as perform_kriging_loocv / make_cv_folds: seed the
# RNG, run `expr`, then restore the caller's .Random.seed (or remove it if the
# caller had none) so classification folds are reproducible without perturbing
# the global stream.
.classif_with_seed <- function(seed, expr) {
  old_seed <- if (exists(".Random.seed", envir = .GlobalEnv, inherits = FALSE)) {
    get(".Random.seed", envir = .GlobalEnv, inherits = FALSE)
  } else {
    NULL
  }
  on.exit({
    if (!is.null(old_seed)) {
      assign(".Random.seed", old_seed, envir = .GlobalEnv)
    } else if (exists(".Random.seed", envir = .GlobalEnv, inherits = FALSE)) {
      rm(".Random.seed", envir = .GlobalEnv)
    }
  }, add = TRUE)
  set.seed(seed)
  force(expr)
}

# ── Method registry ─────────────────────────────────────────────────────────
# One entry per learner. `args` are the parsnip main-model arguments with their
# Phase-1 fixed defaults; `engine_args` are engine-specific pass-throughs.
# `build(args, engine_args)` assembles the parsnip spec. Adding a learner = add
# one entry here; nothing else in the engine hard-codes the method list.
.classif_method_defs <- function() {
  list(
    multinom = list(
      label  = "Multinomial Logistic Regression",
      engine = "nnet",
      args   = list(penalty = 0.0),
      # parsnip's nnet::multinom wrapper does not expose case weights
      # (case_weights_allowed() is FALSE), so the class-imbalance weighting
      # option silently degrades to an unweighted fit for this learner and the
      # run result flags weights_applied = FALSE.
      supports_weights = FALSE,
      # MaxNWts guards against the "too many weights" error when many dummy
      # covariates x many classes inflate the weight count; trace silences the
      # nnet fitting log.
      engine_args = list(trace = FALSE, MaxNWts = 20000L),
      build = function(args, engine_args) {
        spec <- rlang::exec(parsnip::multinom_reg, mode = "classification", !!!args)
        rlang::exec(parsnip::set_engine, spec, "nnet", !!!engine_args)
      }
    ),
    rf = list(
      label  = "Random Forest",
      engine = "ranger",
      args   = list(trees = 500L, mtry = NULL, min_n = NULL),
      supports_weights = TRUE,
      # probability = TRUE => probability forest (needed for class probabilities
      # and the entropy uncertainty surface); num.threads = 1 + seed keep the
      # fit bit-reproducible under the test harness's sequential plan.
      engine_args = list(probability = TRUE, importance = "impurity",
                         num.threads = 1L, seed = 12345L),
      build = function(args, engine_args) {
        args <- args[!vapply(args, is.null, logical(1))]
        spec <- rlang::exec(parsnip::rand_forest, mode = "classification", !!!args)
        rlang::exec(parsnip::set_engine, spec, "ranger", !!!engine_args)
      }
    ),
    xgboost = list(
      label  = "Extreme Gradient Boosting",
      engine = "xgboost",
      args   = list(trees = 500L, tree_depth = 6L, learn_rate = 0.05,
                    mtry = NULL, min_n = 2L, loss_reduction = 0.0,
                    sample_size = 1.0),
      supports_weights = TRUE,
      engine_args = list(nthread = 1L),
      build = function(args, engine_args) {
        args <- args[!vapply(args, is.null, logical(1))]
        spec <- rlang::exec(parsnip::boost_tree, mode = "classification", !!!args)
        rlang::exec(parsnip::set_engine, spec, "xgboost", !!!engine_args)
      }
    )
  )
}

#' Available classification methods (id -> human label).
classif_methods <- function() {
  defs <- .classif_method_defs()
  stats::setNames(vapply(defs, `[[`, character(1), "label"), names(defs))
}

#' Whether a learner's parsnip engine accepts case weights (drives the
#' class-imbalance weighting option; unsupported learners fit unweighted and
#' the run flags it).
classif_supports_weights <- function(method) {
  def <- .classif_method_defs()[[method]]
  !is.null(def) && isTRUE(def$supports_weights)
}

# ── Class-imbalance weights ─────────────────────────────────────────────────
# Inverse-frequency ("balanced") case weights: w_c = n / (k * n_c), so every
# class contributes equally to the loss regardless of its sample count and the
# weights average to 1 over the training rows. This is the standard weighting
# heuristic (King & Zeng 2001; scikit-learn's class_weight = "balanced").
# SMOTE-style synthetic oversampling is deliberately NOT offered: synthetic
# points inherit no valid spatial position, so they break the spatial-CV
# leakage guarantees and fabricate autocorrelation structure.
.classif_class_weights <- function(y) {
  y <- droplevels(as.factor(y))
  tab <- table(y)
  w_class <- stats::setNames(length(y) / (nlevels(y) * as.numeric(tab)), names(tab))
  as.numeric(w_class[as.character(y)])
}

# ── Scope adequacy check ────────────────────────────────────────────────────
#' Human-readable diagnosis of an under-powered classification scope. Returns
#' NULL when the scoped data are adequate, otherwise one message naming every
#' shortfall — including WHICH classes fall below the per-class minimum and
#' their sample counts — plus what to do about it. The run is not blocked
#' (small-class CV is defined, just weak), so the caller shows this as a
#' warning notification.
classif_scope_adequacy <- function(tvec, n_complete, min_rows = 20L, min_class = 3L) {
  tab <- table(droplevels(as.factor(tvec)))
  small <- tab[tab < min_class]
  problems <- character(0)
  if (n_complete < min_rows) {
    problems <- c(problems, sprintf("only %d complete rows (need >= %d)", n_complete, min_rows))
  }
  if (length(small) > 0) {
    problems <- c(problems, sprintf(
      "%d class%s with fewer than %d samples: %s",
      length(small), if (length(small) == 1) "" else "es", min_class,
      paste(sprintf("'%s' (n = %d)", names(small), as.integer(small)), collapse = ", ")))
  }
  if (length(problems) == 0) return(NULL)
  paste0("Insufficient data in scope: ", paste(problems, collapse = "; "),
         ". Results for rare classes will be unreliable. Consider widening the spatial scope, ",
         "merging or excluding rare classes, or (for binned targets) reducing the number of classes.")
}

# ── Tuning-depth registry (expandable) ──────────────────────────────────────
# Maps a tuning "depth" to, per method, the set of hyperparameters that become
# tune() placeholders and the space-filling grid size. Expanding tuning later
# is purely additive: add a depth key, or extend a method's parameter vector.
# `none` (the Phase-1 default) tunes nothing and fits the fixed defaults above.
.classif_tuning_registry <- function() {
  list(
    none = list(
      grid_size = 0L,
      params = list(multinom = character(0), rf = character(0), xgboost = character(0))
    ),
    light = list(
      grid_size = 10L,
      params = list(
        multinom = c("penalty"),
        rf       = c("mtry", "min_n"),
        xgboost  = c("tree_depth", "learn_rate")
      )
    ),
    full = list(
      grid_size = 30L,
      params = list(
        multinom = c("penalty"),
        rf       = c("mtry", "min_n"),
        xgboost  = c("tree_depth", "learn_rate", "mtry", "min_n", "loss_reduction", "sample_size")
      )
    )
  )
}

#' Tuning depths available in the UI (id -> label).
classif_tuning_depths <- function() {
  c(none = "None (fixed defaults)", light = "Light", full = "Full")
}

#' Parameters that a given (method, depth) combination tunes. Unknown methods or
#' depths degrade to no tuning rather than erroring.
classif_tuning_params <- function(method, depth = "none") {
  reg <- .classif_tuning_registry()
  d <- reg[[depth]]
  if (is.null(d)) return(character(0))
  p <- d$params[[method]]
  if (is.null(p)) character(0) else p
}

.classif_grid_size <- function(depth = "none") {
  reg <- .classif_tuning_registry()
  d <- reg[[depth]]
  if (is.null(d)) 0L else d$grid_size
}

# ── Model spec ──────────────────────────────────────────────────────────────
#' Build a parsnip model spec for `method`. Any argument named in `tune_params`
#' is set to tune() (marking it for the tuning grid); `overrides` replaces fixed
#' defaults for the rest. This is the single point where tunable vs fixed is
#' decided, so the tuning registry fully drives model construction.
#'
#' `n_classes` handles the binary special case of the multinom method:
#' parsnip's multinom_reg/nnet mangles probability output for 2-class targets
#' (it emits one .pred_i column per ROW instead of per class), and for k = 2
#' the multinomial logistic model reduces EXACTLY to binomial logistic
#' regression — so a logistic_reg/glm spec is substituted. glm has no penalty
#' argument, matching the multinom default penalty = 0 (unpenalised); callers
#' must clear multinom tune_params for binary targets.
classif_build_spec <- function(method, tune_params = character(0), overrides = list(),
                               n_classes = NULL) {
  defs <- .classif_method_defs()
  def <- defs[[method]]
  if (is.null(def)) stop(sprintf("Unknown classification method: '%s'", method))

  if (identical(method, "multinom") && !is.null(n_classes) && n_classes == 2) {
    return(parsnip::set_engine(parsnip::logistic_reg(mode = "classification"), "glm"))
  }

  args <- def$args
  if (length(overrides)) args[names(overrides)] <- overrides
  for (p in tune_params) {
    if (p %in% names(args)) args[[p]] <- tune::tune()
  }
  def$build(args, def$engine_args)
}

#' Effective tuning parameters for a (method, depth, n_classes) combination:
#' the binary multinom substitution (logistic_reg/glm, see classif_build_spec)
#' has no tunable penalty, so its parameter set is empty.
.classif_effective_tune_params <- function(method, depth, n_classes) {
  if (identical(method, "multinom") && n_classes == 2) return(character(0))
  classif_tuning_params(method, depth)
}

# ── Recipe ──────────────────────────────────────────────────────────────────
#' Shared preprocessing recipe: impute missing covariates (median / mode),
#' absorb novel factor levels seen only at prediction time, one-hot-free dummy
#' encode categoricals, drop zero-variance columns, and standardise numeric
#' predictors (helps multinomial convergence; monotonic, so harmless to trees).
#' The same recipe feeds every learner, per the single-template design.
#' `weight_col` (optional) names a hardhat::importance_weights column in
#' `train_df`; recipes auto-assigns it the case_weights role (verified: it is
#' excluded from all_numeric_predictors and not required at predict time), so
#' the formula can include it without it ever becoming a predictor.
classif_build_recipe <- function(train_df, target, predictors, weight_col = NULL) {
  train_df <- as.data.frame(train_df)
  train_df[[target]] <- as.factor(train_df[[target]])
  keep <- c(target, predictors, weight_col)
  train_df <- train_df[, keep, drop = FALSE]

  form <- stats::as.formula(paste0("`", target, "` ~ ",
                                   paste(sprintf("`%s`", c(predictors, weight_col)), collapse = " + ")))

  recipes::recipe(form, data = train_df) |>
    recipes::step_novel(recipes::all_nominal_predictors()) |>
    recipes::step_impute_median(recipes::all_numeric_predictors()) |>
    recipes::step_impute_mode(recipes::all_nominal_predictors()) |>
    recipes::step_dummy(recipes::all_nominal_predictors()) |>
    recipes::step_zv(recipes::all_predictors()) |>
    recipes::step_normalize(recipes::all_numeric_predictors())
}

# ── Fold construction ───────────────────────────────────────────────────────
#' Integer fold vector for classification CV, mirroring make_cv_folds' contract
#' (one integer per row). `spatial` clusters projected coordinates
#' (spatialsample::spatial_clustering_cv, k-means, matching the interpolation
#' engines' spatial-block convention); `standard` is seeded, class-stratified
#' k-fold. Both are seed-sandboxed. v is clamped so no fold is empty and, for
#' the standard strategy, so every fold can hold each class.
classif_make_fold_id <- function(pts_sf, strategy = c("spatial", "standard"),
                                 target = NULL, v = 10L, seed = 12345L) {
  strategy <- match.arg(strategy)
  n <- nrow(pts_sf)
  v <- max(2L, min(as.integer(v), n))

  .classif_with_seed(seed, {
    if (strategy == "spatial") {
      sp <- spatialsample::spatial_clustering_cv(pts_sf, v = v)
      fold_id <- rep(NA_integer_, n)
      for (i in seq_along(sp$splits)) {
        assess_idx <- rsample::complement(sp$splits[[i]])
        fold_id[assess_idx] <- i
      }
      # Degenerate geometry can leave a point unassigned; fold it into cluster 1.
      fold_id[is.na(fold_id)] <- 1L
      fold_id
    } else {
      if (!is.null(target)) {
        y <- as.factor(sf::st_drop_geometry(pts_sf)[[target]])
        # Cap v at the smallest class so stratified folds can each hold a class.
        # A singleton class floors this at v = 2 and lands that class in exactly
        # one fold, so it is absent from that fold's training set and its recall
        # is 0 by construction, not by model failure. This is inherent to
        # stratified CV with singleton classes; classif_scope_adequacy() warns
        # up front for every class below 3 samples.
        v <- max(2L, min(v, min(table(y))))
        fold_id <- integer(n)
        for (lev in levels(y)) {
          idx <- which(y == lev)
          fold_id[idx] <- sample(rep(seq_len(v), length.out = length(idx)))
        }
        fold_id
      } else {
        sample(rep(seq_len(v), length.out = n))
      }
    }
  })
}

#' Turn an integer fold vector into an rsample rset over `train_df`, so the same
#' fold assignment can drive tune::fit_resamples / tune_grid. Each fold's
#' assessment set is the rows tagged with that fold id.
classif_folds_to_rset <- function(train_df, fold_id) {
  ids <- sort(unique(fold_id))
  splits <- lapply(ids, function(i) {
    assess <- which(fold_id == i)
    analysis <- which(fold_id != i)
    rsample::make_splits(list(analysis = analysis, assessment = assess), data = train_df)
  })
  rsample::manual_rset(splits, ids = paste0("Fold", seq_along(ids)))
}

# ── Metrics ─────────────────────────────────────────────────────────────────
#' Class-label metric set (works for binary and multiclass; multiclass
#' precision/recall/F use macro averaging so minority soil classes are not
#' masked by overall accuracy). Used on POOLED out-of-fold predictions, where
#' every class is present.
classif_class_metric_set <- function() {
  yardstick::metric_set(
    yardstick::accuracy,
    yardstick::kap,
    yardstick::bal_accuracy,
    yardstick::precision,
    yardstick::recall,
    yardstick::f_meas
  )
}

#' Human-readable labels for the yardstick metric ids emitted by
#' classif_compute_metrics(). Unknown ids fall back to the raw id in
#' classif_label_metrics() so a future metric can never blank out the table.
classif_metric_labels <- function() {
  c(accuracy     = "Overall accuracy",
    kap          = "Cohen's kappa",
    bal_accuracy = "Balanced accuracy",
    precision    = "Precision",
    recall       = "Recall",
    f_meas       = "F1 score",
    roc_auc      = "ROC AUC",
    mn_log_loss  = "Log loss",
    brier_class  = "Brier score")
}

#' Labels for yardstick estimator codes (how a metric is averaged/extended to
#' the multiclass case).
classif_estimator_labels <- function() {
  c(multiclass     = "Multiclass",
    macro          = "Macro average",
    macro_weighted = "Weighted macro average",
    micro          = "Micro average",
    hand_till      = "Multiclass (Hand-Till)",
    binary         = "Binary")
}

#' Attach display labels to a yardstick metrics data.frame
#' (.metric/.estimator/.estimate), preserving the raw ids.
classif_label_metrics <- function(m) {
  ml <- classif_metric_labels(); el <- classif_estimator_labels()
  lab <- unname(ml[m$.metric]); lab[is.na(lab)] <- m$.metric[is.na(lab)]
  est <- unname(el[m$.estimator]); est[is.na(est)] <- m$.estimator[is.na(est)]
  m$.metric_label <- lab
  m$.estimator_label <- est
  m
}

#' Lean metric set fed to tune::fit_resamples / tune_grid. Only accuracy and
#' kappa: both are defined even when a single held-out spatial cluster lacks a
#' class, so per-fold computation stays warning-free. The reported per-class and
#' macro metrics are recomputed from pooled predictions (classif_compute_metrics),
#' where all classes appear, so nothing is lost by keeping resampling lean.
classif_resample_metric_set <- function() {
  yardstick::metric_set(yardstick::accuracy, yardstick::kap)
}

#' Compute pooled classification metrics from a predictions data.frame carrying
#' the truth column, `.pred_class`, and per-class `.pred_<level>` probability
#' columns. Probability metrics (ROC AUC, log-loss, Brier) are computed
#' defensively: if they fail (e.g. a class absent from a pooled fold) the class
#' metrics are still returned.
classif_compute_metrics <- function(pred_df, target) {
  truth <- as.factor(pred_df[[target]])
  levs <- levels(truth)
  prob_cols <- paste0(".pred_", levs)
  prob_cols <- prob_cols[prob_cols %in% names(pred_df)]

  cls_metrics <- classif_class_metric_set()
  out <- cls_metrics(pred_df, truth = !!rlang::sym(target),
                     estimate = !!rlang::sym(".pred_class"))

  prob_out <- tryCatch({
    if (length(prob_cols) == length(levs) && length(levs) >= 2) {
      if (length(levs) == 2) {
        # Binary prob metrics take the first (event) level's column only.
        pm <- yardstick::metric_set(yardstick::roc_auc, yardstick::mn_log_loss,
                                    yardstick::brier_class)
        pm(pred_df, truth = !!rlang::sym(target), !!rlang::sym(prob_cols[1]),
           event_level = "first")
      } else {
        pm <- yardstick::metric_set(yardstick::roc_auc, yardstick::mn_log_loss,
                                    yardstick::brier_class)
        pm(pred_df, truth = !!rlang::sym(target), !!!rlang::syms(prob_cols))
      }
    } else {
      NULL
    }
  }, error = function(e) NULL)

  if (!is.null(prob_out)) out <- dplyr::bind_rows(out, prob_out)
  out
}

#' Per-class producer (recall) and user (precision) accuracy from a confusion
#' matrix, the standard DSM per-class report. Returns a tidy data.frame.
classif_per_class_accuracy <- function(pred_df, target) {
  cm <- yardstick::conf_mat(pred_df, truth = !!rlang::sym(target),
                            estimate = !!rlang::sym(".pred_class"))
  tab <- cm$table                       # rows = prediction, cols = truth
  classes <- colnames(tab)
  col_tot <- colSums(tab)               # actual counts per class
  row_tot <- rowSums(tab)               # predicted counts per class
  diagv <- diag(tab)
  data.frame(
    class = classes,
    n = as.integer(col_tot),
    producer_accuracy = ifelse(col_tot > 0, diagv / col_tot, NA_real_),
    user_accuracy = ifelse(row_tot > 0, diagv / row_tot, NA_real_),
    stringsAsFactors = FALSE
  )
}

# ── Cross-validation ────────────────────────────────────────────────────────
#' Cross-validate one classification method. Returns pooled out-of-fold
#' predictions, aggregate metrics, per-class accuracies, the confusion matrix,
#' and the fold vector used. If `depth` requests tuning, hyperparameters are
#' chosen by an inner grid over the same folds before the out-of-fold
#' predictions are collected (defended against leakage by tune's per-split
#' preprocessing).
#'
#' `nested = TRUE` (only meaningful when `depth` tunes something) switches to
#' nested cross-validation: instead of selecting one hyperparameter set on the
#' same folds that score it (mildly optimistic — the selection has seen every
#' fold's held-out data), each OUTER fold re-runs the grid search on
#' `inner_v` inner folds built from its analysis rows only, finalises the
#' workflow with that fold's winner, and predicts its held-out rows. The
#' pooled metrics then estimate the performance of the WHOLE procedure
#' including the tuning search (Varma & Simon 2006; Cawley & Talbot 2010).
#' Inner folds reuse the outer `strategy` (spatial folds inside spatial CV,
#' so inner selection faces the same leakage regime as the outer estimate).
#' Cost multiplies roughly by `inner_v`.
# Cooperative cancellation for the classification worker: the module writes a
# flag file, the worker checks it between expensive stages (fold boundaries,
# tuning, final fit, surface build) — the same file-based machinery the
# interpolation pipeline uses, since Shiny reactives don't exist in workers.
.classif_check_cancel <- function(cancel_file) {
  if (!is.null(cancel_file) && file.exists(cancel_file)) {
    stop("Classification run cancelled by user.", call. = FALSE)
  }
  invisible(NULL)
}

# ── Progress ladder ─────────────────────────────────────────────────────────
#' Cumulative upper bounds, as a fraction of the progress bar, for each stage of
#' run_classification_pipeline. The original denominator (outer folds + 2) gave
#' the whole post-CV half of the run two steps, so the bar reached ~92% the
#' moment cross-validation ended and then sat there for the longest part of the
#' job: interpolating every covariate onto the grid and classifying every cell.
#' These shares are wall-clock estimates, not guarantees; their only job is to
#' keep the bar moving through the stages that actually take the time.
.classif_stage_anchors <- function(make_surface = TRUE) {
  if (isTRUE(make_surface)) {
    c(cv = 0.50, fit = 0.60, importance = 0.66,
      grid = 0.70, covariates = 0.88, surface = 0.99)
  } else {
    # No surface: CV and the final fit are the whole run.
    c(cv = 0.80, fit = 0.94, importance = 0.99,
      grid = 0.99, covariates = 0.99, surface = 0.99)
  }
}

#' Build the worker's progress reporter, or NULL when progress is not being
#' reported (direct calls, tests). The returned closure takes a stage name, a
#' 0-1 fraction WITHIN that stage, and an optional human-readable label; it
#' writes the percent through the shared update_progress_file() and the label to
#' a sibling stage file the module polls, so the text tracks what is actually
#' running instead of always claiming cross-validation.
.classif_progress_reporter <- function(progress_dir, session_id = "classif",
                                       make_surface = TRUE) {
  if (is.null(progress_dir)) return(NULL)
  anchors <- .classif_stage_anchors(make_surface)
  stage_file <- file.path(progress_dir,
                          paste0("stage_", session_id, "_classification_cls.txt"))
  function(stage, frac = 1, label = NULL) {
    i <- match(stage, names(anchors))
    if (is.na(i)) return(invisible(NULL))
    lo <- if (i == 1L) 0 else unname(anchors[i - 1L])
    hi <- unname(anchors[i])
    pct <- lo + max(0, min(1, frac)) * (hi - lo)
    # step/total = pct, at 0.1% resolution (update_progress_file rounds to %).
    update_progress_file("classification", "cls", round(pct * 1000), 1000)
    if (!is.null(label)) {
      tryCatch(writeLines(label, stage_file), error = function(e) NULL)
    }
    invisible(NULL)
  }
}

run_classification_cv <- function(pts_sf, target, predictors,
                                  method = "rf",
                                  strategy = c("spatial", "standard"),
                                  v = 10L, depth = "none", seed = 12345L,
                                  group = NULL, class_weights = FALSE,
                                  nested = FALSE, inner_v = 5L,
                                  oof_importance = FALSE, importance_reps = 5L,
                                  cancel_file = NULL, progress_cb = NULL) {
  strategy <- match.arg(strategy)
  if (!is.null(group) && length(group) != nrow(pts_sf)) {
    stop("`group` must have one entry per row of `pts_sf`.")
  }

  full_df <- as.data.frame(sf::st_drop_geometry(pts_sf))
  cc <- stats::complete.cases(full_df[, c(target, predictors), drop = FALSE])
  keep_sf <- pts_sf[cc, ]
  train_df <- full_df[cc, c(target, predictors), drop = FALSE]
  train_df[[target]] <- as.factor(train_df[[target]])
  grp <- if (is.null(group)) NULL else as.character(group)[cc]

  if (nlevels(train_df[[target]]) < 2) {
    stop("Classification target must have at least two classes after removing missing rows.")
  }

  # Class-imbalance weighting: inverse-frequency case weights, applied only
  # when the engine supports them. The tuning pass uses weights computed on
  # the full training set (tune_grid cannot recompute per split); the reported
  # out-of-fold refits below recompute weights on each fold's analysis rows,
  # so no held-out class-prevalence information enters a fold's fit.
  weights_applied <- isTRUE(class_weights) && classif_supports_weights(method)
  weight_col <- if (weights_applied) ".case_wt" else NULL
  if (weights_applied) {
    train_df$.case_wt <- hardhat::importance_weights(
      .classif_class_weights(train_df[[target]]))
  }

  n_cls <- nlevels(train_df[[target]])
  rec <- classif_build_recipe(train_df, target, predictors, weight_col = weight_col)
  tune_params <- .classif_effective_tune_params(method, depth, n_cls)
  spec <- classif_build_spec(method, tune_params = tune_params, n_classes = n_cls)
  wf <- workflows::workflow() |>
    workflows::add_recipe(rec) |>
    workflows::add_model(spec)
  if (weights_applied) wf <- workflows::add_case_weights(wf, .case_wt)

  fold_id <- classif_make_fold_id(keep_sf, strategy, target = target, v = v, seed = seed)
  rset <- classif_folds_to_rset(train_df, fold_id)

  resample_metrics <- classif_resample_metric_set()

  fit_wf <- wf
  best_params <- NULL
  nested_active <- isTRUE(nested) && length(tune_params) > 0
  tune_grid_df <- NULL
  # Share of this stage's bar given to the non-nested tuning search, which runs
  # before the fold loop and can rival it in cost (it fits grid x folds models).
  # Under nesting the search happens inside the folds, so the fold loop owns the
  # whole stage. progress_cb(frac, label) reports progress WITHIN the CV stage;
  # the pipeline maps it onto the overall bar.
  tune_share <- if (length(tune_params) > 0 && !nested_active) 0.30 else 0
  report_cv <- if (is.function(progress_cb)) progress_cb else function(...) invisible(NULL)
  if (length(tune_params) > 0) {
    # One space-filling grid shared by every tuning pass. Finalising the
    # parameter set against the full predictor frame leaks nothing label-borne
    # (it only pins data-dependent hyperparameter ranges such as mtry <= p).
    tune_grid_df <- .classif_with_seed(seed, {
      pset <- hardhat::extract_parameter_set_dials(wf)
      pset <- dials::finalize(pset, x = train_df[, predictors, drop = FALSE])
      dials::grid_space_filling(pset, size = .classif_grid_size(depth))
    })
  }
  if (length(tune_params) > 0 && !nested_active) {
    # Non-nested (default): hyperparameters are selected by an inner grid over
    # the same folds; the chosen values are then cross-validated below, so
    # tuned-depth metrics are mildly optimistic. Depth "none" has no tuning
    # step and is unaffected; `nested = TRUE` removes the optimism at ~inner_v
    # times the cost.
    .classif_check_cancel(cancel_file)
    report_cv(0, "Tuning hyperparameters (grid search)...")
    tuned <- .classif_with_seed(seed, {
      tune::tune_grid(wf, resamples = rset, grid = tune_grid_df,
                      metrics = resample_metrics,
                      control = tune::control_grid(save_pred = FALSE, verbose = FALSE))
    })
    best_params <- tune::select_best(tuned, metric = "accuracy")
    fit_wf <- tune::finalize_workflow(wf, best_params)
    report_cv(tune_share)
  }

  # Manual out-of-fold loop: fit on the analysis rows, predict hard class AND
  # full class probabilities on the held-out fold, then pool. Hand-rolled (like
  # perform_kriging_loocv) so probability columns are always collected and no
  # per-fold metric warning arises when a spatial fold happens to be single-class.
  # `.row` records each prediction's row index in train_df so the spatial
  # baseline below (and McNemar pairing) can align with the model predictions.
  nested_params <- list()
  imp_parts <- list()
  folds_seq <- sort(unique(fold_id))
  n_fold <- length(folds_seq)
  preds <- .classif_with_seed(seed, {
    parts <- lapply(seq_along(folds_seq), function(k) {
      i <- folds_seq[k]
      .classif_check_cancel(cancel_file)
      # Reported at the START of the fold, so the label names the fold that is
      # actually running and the fraction counts folds already finished.
      report_cv(tune_share + (1 - tune_share) * ((k - 1) / n_fold),
                sprintf("Cross-validation: fold %d of %d", k, n_fold))
      tr <- train_df[fold_id != i, , drop = FALSE]
      te <- train_df[fold_id == i, , drop = FALSE]
      if (weights_applied) {
        # Recompute weights from the analysis rows only (no held-out
        # class-prevalence leaks into the fold's fit).
        tr$.case_wt <- hardhat::importance_weights(
          .classif_class_weights(tr[[target]]))
      }
      fold_wf <- fit_wf
      if (nested_active) {
        # Inner tuning sees ONLY this outer fold's analysis rows: fold
        # construction, grid search, and winner selection all happen inside
        # them, so the outer held-out rows never influence the chosen
        # hyperparameters.
        inner_id <- classif_make_fold_id(keep_sf[fold_id != i, , drop = FALSE],
                                         strategy, target = target,
                                         v = inner_v, seed = seed + i)
        inner_rset <- classif_folds_to_rset(tr, inner_id)
        tuned_i <- tune::tune_grid(wf, resamples = inner_rset, grid = tune_grid_df,
                                   metrics = resample_metrics,
                                   control = tune::control_grid(save_pred = FALSE, verbose = FALSE))
        bp_i <- tune::select_best(tuned_i, metric = "accuracy")
        nested_params[[length(nested_params) + 1]] <<-
          cbind(data.frame(.fold = i), as.data.frame(bp_i)[, setdiff(names(bp_i), ".config"), drop = FALSE])
        fold_wf <- tune::finalize_workflow(wf, bp_i)
      }
      fit_i <- parsnip::fit(fold_wf, data = tr)
      # Out-of-fold permutation importance: score THIS fold's model on the rows
      # it never saw, before they are used for anything else. Done here because
      # the fold's fitted workflow only exists inside this iteration — the
      # alternative (refitting afterwards) would double the CV cost, whereas
      # this reuses a fit that already exists and predicts the same number of
      # rows in total as the training-row design. Permutation shuffles a
      # predictor WITHIN the assessment rows, so very small folds decorrelate
      # it less thoroughly; that caveat is documented in the guide.
      if (isTRUE(oof_importance) && length(predictors)) {
        imp_parts[[length(imp_parts) + 1]] <<-
          .classif_perm_delta(fit_i, te, target, predictors,
                              n_rep = importance_reps, cancel_file = cancel_file)
      }
      cls <- predict(fit_i, te, type = "class")
      prob <- predict(fit_i, te, type = "prob")
      p <- cbind(data.frame(.fold = i, .row = which(fold_id == i)),
                 te[, target, drop = FALSE],
                 .pred_class = cls$.pred_class, as.data.frame(prob))
      if (!is.null(grp)) p$.scope_group <- grp[fold_id == i]
      p
    })
    do.call(rbind, parts)
  })

  # Spatial-only baseline on the SAME folds: each held-out point takes the
  # class of its nearest analysis-set point. Paired with the model predictions
  # via .row, this feeds the covariate-lift comparison.
  base_cls <- classif_spatial_baseline(sf::st_coordinates(keep_sf),
                                       train_df[[target]], fold_id)
  preds$.pred_base <- base_cls[preds$.row]

  list(
    method = method,
    strategy = strategy,
    fold_id = fold_id,
    n_folds = length(unique(fold_id)),
    predictions = preds,
    metrics = classif_compute_metrics(preds, target),
    per_class = classif_per_class_accuracy(preds, target),
    conf_mat = yardstick::conf_mat(preds, truth = !!rlang::sym(target),
                                   estimate = !!rlang::sym(".pred_class")),
    best_params = best_params,
    nested = nested_active,
    # One row per outer fold: the hyperparameters that fold's inner search
    # chose. Fold-to-fold agreement is itself a stability diagnostic.
    nested_params = if (length(nested_params)) do.call(rbind, nested_params) else NULL,
    # NULL unless oof_importance was requested; the pipeline falls back to the
    # training-row design in that case.
    importance = .classif_pool_fold_importance(imp_parts, predictors),
    weights_applied = weights_applied
  )
}

# ── Spatial-only baseline + covariate lift ──────────────────────────────────
#' Out-of-fold nearest-neighbour class assignment from coordinates alone: for
#' each fold, every held-out point receives the class of its nearest (Euclidean,
#' projected coords) analysis-set point — the categorical analogue of
#' Thiessen/nearest-neighbour interpolation, i.e. what pure spatial proximity
#' achieves with NO covariates. Returns a factor aligned with the input rows.
classif_spatial_baseline <- function(coords, y, fold_id) {
  coords <- as.matrix(coords)
  y <- as.factor(y)
  out <- rep(NA_character_, nrow(coords))
  for (i in sort(unique(fold_id))) {
    tr_idx <- which(fold_id != i)
    te_idx <- which(fold_id == i)
    if (length(tr_idx) == 0 || length(te_idx) == 0) next
    nn <- FNN::get.knnx(coords[tr_idx, , drop = FALSE],
                        coords[te_idx, , drop = FALSE], k = 1)$nn.index[, 1]
    out[te_idx] <- as.character(y[tr_idx][nn])
  }
  factor(out, levels = levels(y))
}

#' Covariate lift: how much the covariate model improves on two no-covariate
#' baselines, computed from pooled out-of-fold predictions that carry
#' `.pred_class` (model) and `.pred_base` (spatial 1-NN baseline).
#'
#' - `majority_acc` is the no-information rate (always predict the modal class).
#' - `baseline_acc` / `baseline_kap` score the spatial 1-NN baseline on the
#'   same folds, so the comparison shares the identical validation design.
#' - `lift_abs` is the accuracy gain in points over the spatial baseline;
#'   `mcnemar_p` is the exact-paired McNemar test (continuity-corrected) on the
#'   discordant correct/incorrect pairs — the standard significance test for
#'   comparing two classifiers on the same samples (Dietterich 1998).
classif_covariate_lift <- function(pred_df, target) {
  truth <- as.factor(pred_df[[target]])
  mod_ok  <- as.character(pred_df$.pred_class) == as.character(truth)
  base_ok <- as.character(pred_df$.pred_base) == as.character(truth)

  kap_of <- function(est_col) {
    m <- tryCatch(suppressWarnings(
      yardstick::kap(pred_df, truth = !!rlang::sym(target),
                     estimate = !!rlang::sym(est_col))$.estimate),
      error = function(e) NA_real_)
    if (length(m)) m[1] else NA_real_
  }

  b <- sum(!mod_ok & base_ok)   # baseline right, model wrong
  c_ <- sum(mod_ok & !base_ok)  # model right, baseline wrong
  mcnemar_p <- if ((b + c_) > 0) {
    tryCatch(stats::mcnemar.test(matrix(c(0, b, c_, 0), nrow = 2))$p.value,
             error = function(e) NA_real_)
  } else {
    NA_real_
  }

  data.frame(
    model_acc    = mean(mod_ok),
    baseline_acc = mean(base_ok),
    majority_acc = max(table(truth)) / length(truth),
    model_kap    = kap_of(".pred_class"),
    baseline_kap = kap_of(".pred_base"),
    lift_abs     = mean(mod_ok) - mean(base_ok),
    mcnemar_p    = mcnemar_p,
    n            = nrow(pred_df)
  )
}

# ── Permutation feature importance ──────────────────────────────────────────
#' Model-agnostic permutation importance (Breiman 2001; Fisher et al. 2019):
#' permute one covariate at a time, re-predict, and record the increase in
#' multiclass log-loss relative to the unpermuted baseline. Log-loss is used
#' (rather than accuracy) because it consumes the full probability output, so
#' it detects importance even when permutation rarely flips the argmax class.
#' Comparable across all learners, unlike engine-native measures (ranger
#' impurity vs xgboost gain vs multinom coefficients).
#'
#' Two evaluation designs are supported (see `classif_permutation_importance`
#' for the training-row one and `run_classification_cv(oof_importance = TRUE)`
#' for the out-of-fold one); both share the core below so the two variants are
#' guaranteed to compute the same quantity on different rows.
#'
#' Core: given ONE fitted workflow and ONE evaluation frame, return the
#' per-predictor increase in multiclass log-loss under permutation, plus the
#' unpermuted baseline and the row count (the caller needs `n` to pool folds).
#' Callers are responsible for the seed sandbox — the shuffles happen here.
.classif_perm_delta <- function(wf, df, target, predictors, n_rep = 5L,
                                cancel_file = NULL, progress = NULL) {
  truth <- as.factor(df[[target]])

  log_loss_of <- function(newdata) {
    prob <- as.matrix(predict(wf, newdata, type = "prob"))
    colnames(prob) <- sub("^\\.pred_", "", colnames(prob))
    p_true <- prob[cbind(seq_len(nrow(prob)), match(as.character(truth), colnames(prob)))]
    -mean(log(pmax(p_true, 1e-15)))
  }

  base_ll <- log_loss_of(df)
  n_pred <- length(predictors)
  # The cancel check and the progress tick consume no RNG, so the shuffle
  # sequence (and therefore every importance value) is unchanged.
  delta <- vapply(seq_len(n_pred), function(j) {
    p <- predictors[j]
    .classif_check_cancel(cancel_file)
    lls <- vapply(seq_len(n_rep), function(r) {
      d2 <- df
      d2[[p]] <- d2[[p]][sample.int(nrow(d2))]
      log_loss_of(d2)
    }, numeric(1))
    if (is.function(progress)) progress(j / n_pred)
    mean(lls) - base_ll
  }, numeric(1))

  list(delta = delta, baseline = base_ll, n = nrow(df))
}

#' Shared presentation frame for both importance designs. `share_pct`
#' renormalises the positive importances to sum to 100 for the "elevation
#' contributed X%" reading; negative raw values (a predictor whose permutation
#' IMPROVES the loss, i.e. pure noise) clamp to a 0 share. `evaluated_on`
#' records the design so the plot, the table and the metrics CSV can never
#' present an optimistic and an honest number as though they were the same.
.classif_importance_frame <- function(predictors, delta, baseline, evaluated_on) {
  pos <- pmax(delta, 0)
  share <- if (sum(pos) > 0) 100 * pos / sum(pos) else rep(NA_real_, length(pos))
  out <- data.frame(
    predictor = predictors,
    importance = as.numeric(delta),    # increase in multiclass log-loss
    share_pct = as.numeric(share),
    baseline_logloss = baseline,
    evaluated_on = evaluated_on,
    stringsAsFactors = FALSE
  )
  out[order(-out$importance), , drop = FALSE]
}

#' Training-row design: the final fitted model scored on the rows it was fitted
#' on — the standard default (vip::vi_permute). Rankings are informative but the
#' absolute values lean optimistic for flexible learners, because a model that
#' has partly memorised its training rows loses more when a predictor it
#' memorised through is destroyed. Seed-sandboxed; `n_rep` permutation repeats.
classif_permutation_importance <- function(model, train_df, target, predictors,
                                           n_rep = 5L, seed = 12345L,
                                           cancel_file = NULL, progress = NULL) {
  wf <- model$workflow
  df <- as.data.frame(train_df)
  df <- df[stats::complete.cases(df[, c(target, predictors), drop = FALSE]), , drop = FALSE]
  core <- .classif_with_seed(seed, {
    .classif_perm_delta(wf, df, target, predictors, n_rep = n_rep,
                        cancel_file = cancel_file, progress = progress)
  })
  .classif_importance_frame(predictors, core$delta, core$baseline, "training")
}

#' Pool per-fold out-of-fold importances into one frame. Each fold contributed
#' a mean-over-its-own-rows log-loss delta, so weighting by fold size and
#' dividing by the total reproduces EXACTLY the delta that pooling every
#' out-of-fold row into a single evaluation set would give.
.classif_pool_fold_importance <- function(parts, predictors) {
  if (!length(parts)) return(NULL)
  w <- vapply(parts, function(z) as.numeric(z$n), numeric(1))
  if (sum(w) <= 0) return(NULL)
  dl <- do.call(cbind, lapply(parts, function(z) z$delta))   # n_pred x n_fold
  delta <- as.numeric(dl %*% w) / sum(w)
  base <- sum(vapply(parts, function(z) z$baseline, numeric(1)) * w) / sum(w)
  .classif_importance_frame(predictors, delta, base, "out-of-fold")
}

# ── Final fit ───────────────────────────────────────────────────────────────
#' Fit the final classification workflow on all training points, tuning first
#' if `depth` requests it. The tuning CV uses the SAME `strategy` as the run's
#' evaluation CV: tuning under random stratified folds while reporting spatial-CV
#' metrics picks hyperparameters against exactly the optimism spatial CV exists
#' to remove, and those hyperparameters are what the map, the entropy surface,
#' the permutation importance and the exported .rds bundle are all built from.
#' Returns the fitted workflow plus the target levels for downstream raster
#' layer naming.
fit_classification_model <- function(pts_sf, target, predictors,
                                     method = "rf", depth = "none",
                                     strategy = c("spatial", "standard"),
                                     v = 10L, seed = 12345L,
                                     class_weights = FALSE) {
  strategy <- match.arg(strategy)
  full_df <- as.data.frame(sf::st_drop_geometry(pts_sf))
  cc <- stats::complete.cases(full_df[, c(target, predictors), drop = FALSE])
  train_df <- full_df[cc, c(target, predictors), drop = FALSE]
  train_df[[target]] <- as.factor(train_df[[target]])

  weights_applied <- isTRUE(class_weights) && classif_supports_weights(method)
  weight_col <- if (weights_applied) ".case_wt" else NULL
  if (weights_applied) {
    train_df$.case_wt <- hardhat::importance_weights(
      .classif_class_weights(train_df[[target]]))
  }

  n_cls <- nlevels(train_df[[target]])
  rec <- classif_build_recipe(train_df, target, predictors, weight_col = weight_col)
  tune_params <- .classif_effective_tune_params(method, depth, n_cls)
  spec <- classif_build_spec(method, tune_params = tune_params, n_classes = n_cls)
  wf <- workflows::workflow() |>
    workflows::add_recipe(rec) |>
    workflows::add_model(spec)
  if (weights_applied) wf <- workflows::add_case_weights(wf, .case_wt)

  final_params <- NULL
  if (length(tune_params) > 0) {
    keep_sf <- pts_sf[cc, ]
    fold_id <- classif_make_fold_id(keep_sf, strategy, target = target, v = v, seed = seed)
    rset <- classif_folds_to_rset(train_df, fold_id)
    tuned <- .classif_with_seed(seed, {
      pset <- hardhat::extract_parameter_set_dials(wf)
      pset <- dials::finalize(pset, x = train_df[, predictors, drop = FALSE])
      grid <- dials::grid_space_filling(pset, size = .classif_grid_size(depth))
      tune::tune_grid(wf, resamples = rset,
                      grid = grid, metrics = classif_resample_metric_set(),
                      control = tune::control_grid(save_pred = FALSE, verbose = FALSE))
    })
    final_params <- tune::select_best(tuned, metric = "accuracy")
    wf <- tune::finalize_workflow(wf, final_params)
  }

  fitted <- .classif_with_seed(seed, parsnip::fit(wf, data = train_df))
  list(workflow = fitted, levels = levels(train_df[[target]]),
       target = target, predictors = predictors, method = method,
       # The hyperparameters this exported/deployed model was actually built
       # with (its own full-data tuning pass) — not the CV loop's selection.
       best_params = final_params,
       weights_applied = weights_applied)
}

# ── Prediction surface ──────────────────────────────────────────────────────
#' Shannon entropy of a per-row probability matrix, normalised to [0, 1] by
#' log(n_class): 0 = a single class certain, 1 = uniform over classes. The
#' natural spatial-uncertainty surface for a classifier, analogous to the
#' kriging variance map for continuous predictions.
classif_shannon_entropy <- function(prob_mat) {
  prob_mat <- as.matrix(prob_mat)
  k <- ncol(prob_mat)
  if (k < 2) return(rep(0, nrow(prob_mat)))
  # Vectorised -sum(p log p). Zeroing the non-finite terms is equivalent to the
  # old per-row p[p > 0] filter (adding an exact 0 to a running sum is a no-op
  # and the surviving terms keep their column order), but avoids apply()'s
  # per-row list allocation, which dominated on million-cell prediction grids.
  pl <- prob_mat * log(prob_mat)
  pl[!is.finite(pl)] <- 0
  as.numeric(-rowSums(pl) / log(k))
}

#' Predict class, per-class probability, and normalised entropy for a covariate
#' data.frame (typically the interpolated prediction grid from
#' krige_covariates()). Returns `newdata` with `.pred_class`, `.pred_<level>`
#' columns, and `.entropy` appended.
#' Prediction runs in row blocks. Every recipe step in classif_build_recipe is
#' TRAINED (novel levels, impute medians/modes, dummy levels, zv removals,
#' normalise centres/scales), so baking is row-independent and a blocked
#' prediction is identical to a single whole-grid call. Blocking buys two
#' things on the million-cell grids this stage produces: a cancel checkpoint
#' and a progress tick between blocks (the surface stage used to run for
#' minutes with the bar frozen and the cancel flag unread), plus a lower peak
#' memory footprint.
predict_classification_surface <- function(model, newdata, chunk_size = NULL,
                                           cancel_file = NULL, progress = NULL) {
  wf <- model$workflow
  nd <- as.data.frame(newdata)
  n <- nrow(nd)
  if (n == 0) stop("No prediction locations to classify.")
  # ~40 blocks so the bar moves visibly, but never smaller than 5000 rows
  # (per-call predict overhead would start to matter below that).
  if (is.null(chunk_size)) chunk_size <- max(5000L, as.integer(ceiling(n / 40)))
  chunk_size <- max(1L, as.integer(chunk_size))

  starts <- seq.int(1L, n, by = chunk_size)
  cls_parts <- vector("list", length(starts))
  prob_parts <- vector("list", length(starts))
  for (j in seq_along(starts)) {
    .classif_check_cancel(cancel_file)
    idx <- seq.int(starts[j], min(starts[j] + chunk_size - 1L, n))
    block <- nd[idx, , drop = FALSE]
    cls_parts[[j]] <- predict(wf, block, type = "class")$.pred_class
    prob_parts[[j]] <- as.data.frame(predict(wf, block, type = "prob"))
    if (is.function(progress)) progress(j / length(starts))
  }
  # Rebuild the factor against the trained level set rather than relying on
  # c()/unlist() level handling, which differs across R versions.
  lev <- levels(cls_parts[[1]])
  cls <- factor(unlist(lapply(cls_parts, as.character), use.names = FALSE),
                levels = lev)
  prob <- if (length(prob_parts) == 1L) prob_parts[[1]] else do.call(rbind, prob_parts)
  # rbind carries each block's row names through; drop them so the assembled
  # frame is identical whatever the block size.
  rownames(prob) <- NULL

  out <- cbind(nd, .pred_class = cls, prob)
  out$.entropy <- classif_shannon_entropy(prob)
  out
}

# ── Rasterisation ───────────────────────────────────────────────────────────
#' Rasterise a prediction surface. `grid_sf` must carry x/y (projected) plus the
#' `.pred_class`, `.pred_<level>`, and `.entropy` columns from
#' predict_classification_surface(). Returns a list with a categorical class
#' SpatRaster, a multi-layer probability SpatRaster, an entropy SpatRaster, and
#' a per-class area table in hectares (exact cell counts x cell area, matching
#' the app's continuous classified-map area reporting).
#' `conf_threshold` implements the abstention ("reject option", Chow 1970)
#' rule: cells whose maximum class probability falls BELOW the threshold are
#' assigned an explicit "Unclassified" category (rendered grey) instead of a
#' weak argmax guess, flagging where field verification is needed. 0 disables
#' abstention. Thresholds at or below 1/n_classes cannot fire (the max
#' probability of a k-class prediction is always >= 1/k).
classif_surface_to_rasters <- function(grid_sf, res, crs_wkt, levels_order = NULL,
                                       conf_threshold = 0) {
  df <- as.data.frame(grid_sf)
  if (is.null(df$x) || is.null(df$y)) {
    coords <- sf::st_coordinates(grid_sf)
    df$x <- coords[, 1]; df$y <- coords[, 2]
  }
  levs <- if (is.null(levels_order)) levels(as.factor(df$.pred_class)) else levels_order
  cls_chr <- as.character(df$.pred_class)

  prob_cols <- paste0(".pred_", levs)
  prob_cols <- prob_cols[prob_cols %in% names(df)]

  # Abstention mask from the per-cell winning probability. The "Unclassified"
  # level is appended only when a threshold is active so a plain run's class
  # raster, legend, and area table are byte-identical to the pre-feature ones.
  use_abstain <- is.numeric(conf_threshold) && length(conf_threshold) == 1 &&
    !is.na(conf_threshold) && conf_threshold > 0 && length(prob_cols) == length(levs)
  if (use_abstain) {
    max_p <- do.call(pmax, c(df[prob_cols], na.rm = TRUE))
    cls_chr[!is.na(max_p) & max_p < conf_threshold] <- "Unclassified"
  }
  levs_map <- if (use_abstain) c(levs, "Unclassified") else levs
  cls_int <- match(cls_chr, levs_map)

  ext <- terra::ext(min(df$x) - res / 2, max(df$x) + res / 2,
                    min(df$y) - res / 2, max(df$y) + res / 2)
  templ <- terra::rast(ext, resolution = res, crs = crs_wkt)

  class_r <- terra::rasterize(as.matrix(df[, c("x", "y")]), templ, values = cls_int)
  levels(class_r) <- data.frame(ID = seq_along(levs_map), class = levs_map)
  # Embed the same viridis palette the in-app map uses so the exported class
  # GeoTIFF opens coloured in GIS/image viewers (a bare integer band renders
  # greyscale). viridisLite is a hard dependency of ggplot2, always installed.
  # Abstained cells get a neutral grey, matching the on-screen palette.
  pal <- viridisLite::viridis(length(levs))
  if (use_abstain) pal <- c(pal, "#9E9E9E")
  terra::coltab(class_r) <- data.frame(value = seq_along(levs_map), col = pal)
  names(class_r) <- "class"

  prob_r <- terra::rast(lapply(prob_cols, function(cc) {
    terra::rasterize(as.matrix(df[, c("x", "y")]), templ, values = df[[cc]])
  }))
  names(prob_r) <- sub("^\\.pred_", "P_", prob_cols)

  ent_r <- terra::rasterize(as.matrix(df[, c("x", "y")]), templ, values = df$.entropy)
  names(ent_r) <- "entropy"

  cell_ha <- (res * res) / 10000
  counts <- table(factor(cls_chr, levels = levs_map))
  area_tbl <- data.frame(
    class = levs_map,
    n_cells = as.integer(counts),
    area_ha = as.numeric(counts) * cell_ha,
    stringsAsFactors = FALSE
  )

  list(class = class_r, prob = prob_r, entropy = ent_r, area = area_tbl,
       conf_threshold = if (use_abstain) conf_threshold else 0)
}

# ── Prediction grid + covariate surface ─────────────────────────────────────
#' Build a projected prediction grid inside the sample boundary, mirroring the
#' interpolation pipeline's approach (concave/convex hull, terra raster, clip).
#' A pre-resolved scope boundary (sf/sfc in the points' CRS, e.g. from
#' classif_resolve_scope) can be supplied via `boundary_sf` and then replaces
#' the hull construction entirely.
#' Resolution defaults to ~50k cells inside the domain, clamped to [5, 1000] m.
classif_build_grid <- function(pts_proj, res = NULL,
                               boundary = c("concave", "convex", "bbox", "wrapped", "strict"),
                               boundary_sf = NULL,
                               buffer_mode = "fixed", buffer_dist = 250) {
  boundary <- match.arg(boundary)
  bnd <- if (!is.null(boundary_sf)) {
    g <- sf::st_geometry(boundary_sf)
    if (is.na(sf::st_crs(g))) sf::st_crs(g) <- sf::st_crs(pts_proj)
    sf::st_union(g)
  } else {
    # Single-hull fallback for direct calls without a pre-resolved scope
    # boundary; shares the style/buffer semantics with .classif_scope_hulls.
    .classif_scope_hulls(pts_proj, group = NULL, style = boundary,
                         buffer_mode = buffer_mode, buffer_dist = buffer_dist)
  }
  bnd <- sf::st_as_sf(sf::st_sfc(sf::st_geometry(bnd), crs = sf::st_crs(pts_proj)))

  bbox <- sf::st_bbox(bnd)
  if (is.null(res)) {
    area_m2 <- sum(as.numeric(sf::st_area(bnd)))
    res <- max(5, min(1000, sqrt(area_m2 / 50000)))
    # A multi-part boundary (distant localities) can cover a bounding box far
    # larger than its own area; the raster template spans the full bbox before
    # clipping, so additionally cap the auto resolution to keep the candidate
    # grid below ~4M cells. User-supplied resolutions are respected as-is.
    dx <- as.numeric(bbox["xmax"] - bbox["xmin"])
    dy <- as.numeric(bbox["ymax"] - bbox["ymin"])
    res <- max(res, sqrt(dx * dy / 4e6))
  }
  grid_r <- terra::rast(terra::ext(bbox), resolution = res, crs = sf::st_crs(pts_proj)$wkt)
  grid_p <- terra::as.points(grid_r, values = FALSE) |> sf::st_as_sf()
  inside <- sf::st_within(grid_p, bnd, sparse = FALSE)[, 1]
  # Falling back to the full bbox here would silently predict over the entire
  # bounding box — the opposite of what scoping promises. Fail loudly instead;
  # the module's promise-error handler surfaces this message.
  if (!any(inside)) {
    stop("No grid cells fall inside the scope boundary at this resolution; reduce the grid resolution or widen the scope.")
  }
  grid_p <- grid_p[inside, ]
  coords <- sf::st_coordinates(grid_p)
  grid_p$x <- coords[, 1]; grid_p$y <- coords[, 2]
  list(grid_p = grid_p, res = res, crs_wkt = sf::st_crs(pts_proj)$wkt)
}

#' Populate a prediction grid with covariate values. Numeric covariates are
#' interpolated with the existing krige_covariates() surface builder (same path
#' as RK/RFK); categorical covariates, which cannot be kriged, inherit the class
#' of their nearest training point (FNN). Both approaches are documented as
#' approximations that propagate onto the classifier's inputs.
build_classification_grid_aux <- function(pts_proj, grid_p, predictors,
                                          cancel_file = NULL, progress = NULL) {
  df <- sf::st_drop_geometry(pts_proj)
  # A predictor absent from the frame yields df[[p]] = NULL, and is.numeric(NULL)
  # is FALSE - it would be routed to the categorical branch, become an all-NA
  # column, get mode-imputed to a constant and then dropped by step_zv, leaving
  # the model quietly trained on fewer predictors than the user selected.
  missing_preds <- setdiff(predictors, names(df))
  if (length(missing_preds) > 0) {
    stop("Predictor column(s) not found in the training data: ",
         paste(missing_preds, collapse = ", "))
  }
  is_num <- vapply(predictors, function(p) is.numeric(df[[p]]), logical(1))
  num_preds <- predictors[is_num]
  cat_preds <- predictors[!is_num]

  grid_aux <- grid_p
  if (length(num_preds) > 0) {
    lags <- calc_scientific_lags(pts_proj)
    mp <- list(idw_p = 2, idw_nmax = 12)
    # One covariate kriged onto the full grid is the coarsest interruptible
    # unit here (gstat's krige() call is a black box), so cancel latency in
    # this stage is one covariate.
    kc <- krige_covariates(pts_proj, grid_p, num_preds, lags, mp,
                           on_var = function(i, total) {
                             .classif_check_cancel(cancel_file)
                             if (is.function(progress)) progress(i / total)
                           })
    grid_aux <- kc$grid_aux
  }
  if (length(cat_preds) > 0) {
    nn <- FNN::get.knnx(sf::st_coordinates(pts_proj),
                        sf::st_coordinates(grid_p), k = 1)$nn.index[, 1]
    for (cp in cat_preds) {
      grid_aux[[cp]] <- factor(as.character(df[[cp]])[nn],
                               levels = levels(as.factor(df[[cp]])))
    }
  }
  grid_aux
}

# ── Spatial scope (localities / polygons) ───────────────────────────────────
#' Collect the polygons available for scoping a classification run: shapes the
#' user drew on the Leaflet map (EPSG:4326) and/or the uploaded shapefile (any
#' CRS). Returns one sf with a `label` column, or NULL when no usable polygon
#' exists. Non-polygon shapefile geometry degrades to its convex hull, matching
#' the interpolation pipeline's treatment of point/line uploads.
classif_scope_polygons <- function(drawn_sf = NULL, shp_sf = NULL, target_crs = NULL) {
  is_poly <- function(g) as.character(sf::st_geometry_type(g)) %in% c("POLYGON", "MULTIPOLYGON")

  drawn <- NULL
  if (!is.null(drawn_sf) && nrow(drawn_sf) > 0) {
    keep <- is_poly(sf::st_geometry(drawn_sf))
    if (any(keep)) {
      drawn <- sf::st_sf(label = paste("Drawn", seq_len(sum(keep))),
                         geometry = sf::st_geometry(drawn_sf)[keep])
    }
  }

  shp <- NULL
  if (!is.null(shp_sf) && inherits(shp_sf, "sf") && nrow(shp_sf) > 0 &&
      !is.na(sf::st_crs(shp_sf))) {
    keep <- is_poly(sf::st_geometry(shp_sf))
    if (any(keep)) {
      s <- shp_sf[keep, , drop = FALSE]
      # Feature labels: first attribute column whose values uniquely name every
      # feature; otherwise a generic sequence.
      attrs <- sf::st_drop_geometry(s)
      lab <- NULL
      for (cn in names(attrs)) {
        v <- as.character(attrs[[cn]])
        if (!anyNA(v) && !any(v == "") && length(unique(v)) == nrow(s)) { lab <- v; break }
      }
      if (is.null(lab)) lab <- paste("Shape", seq_len(nrow(s)))
      shp <- sf::st_sf(label = lab, geometry = sf::st_geometry(s))
    } else {
      hull <- tryCatch(sf::st_convex_hull(sf::st_union(sf::st_geometry(shp_sf))),
                       error = function(e) NULL)
      if (!is.null(hull) && all(is_poly(hull))) {
        shp <- sf::st_sf(label = "Uploaded boundary", geometry = hull)
      }
    }
  }

  pieces <- Filter(Negate(is.null), list(drawn, shp))
  if (length(pieces) == 0) return(NULL)
  if (!is.null(target_crs)) {
    pieces <- lapply(pieces, function(p) sf::st_transform(p, target_crs))
  } else if (length(pieces) == 2) {
    pieces[[2]] <- sf::st_transform(pieces[[2]], sf::st_crs(pieces[[1]]))
  }
  out <- do.call(rbind, pieces)
  out$label <- make.unique(out$label)
  rownames(out) <- NULL
  out
}

# Union of per-group hulls over projected points. Grouped hulls (one per
# locality) keep the prediction domain from bridging unsampled terrain between
# localities; a NULL group reproduces the single-hull behaviour. Degenerate
# hulls (a group with < 3 unique coordinates collapses to a point/line) get a
# small pad so the boundary still encloses grid cells.
#
# Styles mirror the interpolation sidebar's Boundary Type exactly:
#   concave/convex/bbox — tight hulls (millimetre snap buffer, see below);
#   wrapped — concave hull padded by a buffer: "fixed" uses `buffer_dist`
#     metres, "dynamic" derives the distance per group from its sampling
#     density (mean 1-NN spacing x 0.5, x the generic 2.0 multiplier, clamped
#     to [5, 2000] m), the same construction run_regional_interpolation uses
#     (the method-specific multiplier is an interpolation concept, so the
#     classifier uses get_buffer_multiplier's default of 2.0);
#   strict — union of per-point buffers of `buffer_dist` metres (always fixed,
#     matching the sidebar, which offers dynamic logic for wrapped only).
.classif_scope_hulls <- function(pts_proj, group = NULL, style = "concave",
                                 buffer_mode = "fixed", buffer_dist = 250) {
  bb <- sf::st_bbox(pts_proj)
  diag_len <- sqrt((bb["xmax"] - bb["xmin"])^2 + (bb["ymax"] - bb["ymin"])^2)
  pad <- max(1, 0.02 * diag_len)
  # concaveman's C++ backend can emit hull vertices a few 1e-5 map units off
  # the input coordinates, leaving boundary samples marginally outside their
  # own hull; a millimetre-scale snap buffer (projected CRSs are metric)
  # guarantees the domain contains every training point.
  snap <- max(0.001, 1e-6 * diag_len)
  b_dist_safe <- if (is.null(buffer_dist) || !is.finite(buffer_dist)) 250 else max(0, buffer_dist)
  group_buffer <- function(p) {
    if (style == "wrapped" && identical(buffer_mode, "dynamic")) {
      co <- sf::st_coordinates(p)
      if (nrow(co) > 1) {
        local_res <- mean(FNN::get.knn(co, k = 1)$nn.dist) * 0.5
        return(max(5, min(2000, 2.0 * local_res)))
      }
    }
    b_dist_safe
  }
  concave_of <- function(p) {
    sf::st_union(tryCatch(sf::st_geometry(concaveman::concaveman(p)),
                          error = function(e) sf::st_convex_hull(sf::st_union(p))))
  }
  one_hull <- function(p) {
    if (style == "strict") {
      return(sf::st_union(sf::st_buffer(sf::st_geometry(p), dist = max(snap, b_dist_safe))))
    }
    if (style == "wrapped") {
      return(sf::st_buffer(concave_of(p), dist = max(snap, group_buffer(p))))
    }
    g <- switch(style,
      concave = concave_of(p),
      convex  = sf::st_convex_hull(sf::st_union(p)),
      bbox    = sf::st_as_sfc(sf::st_bbox(p)))
    g <- sf::st_union(g)
    is_poly <- all(as.character(sf::st_geometry_type(g)) %in% c("POLYGON", "MULTIPOLYGON"))
    sf::st_buffer(g, dist = if (is_poly) snap else pad)
  }
  if (is.null(group)) return(one_hull(pts_proj))
  parts <- lapply(split(seq_len(nrow(pts_proj)), as.character(group)),
                  function(idx) one_hull(pts_proj[idx, , drop = FALSE]))
  sf::st_union(do.call(c, parts))
}

#' Resolve the spatial scope of a classification run: filter `df` to the
#' selected localities and/or polygons, assign each retained point an
#' evaluation group (locality, or polygon label in polygons-only mode), and
#' build the projected prediction boundary as WKT.
#'
#' `poly_mode`: "ignore" scopes by localities alone; "intersect" keeps points
#' inside the selected localities AND the polygons; "only" keeps points inside
#' the polygons regardless of locality. `localities = NULL`, an empty vector,
#' or a vector containing "ALL" means every locality.
#'
#' The boundary is the polygon union ("only"), the union of per-locality hulls
#' of the scoped points ("ignore"), or the intersection of the two
#' ("intersect"), so prediction never extends into unsampled terrain between
#' localities or outside the user's polygons.
classif_resolve_scope <- function(df, x_col, y_col, src_crs, proj_crs,
                                  loc_col = NULL, localities = NULL,
                                  poly_sf = NULL,
                                  poly_mode = c("ignore", "intersect", "only"),
                                  boundary_style = "concave",
                                  buffer_mode = "fixed", buffer_dist = 250) {
  poly_mode <- match.arg(poly_mode)
  if (is.null(poly_sf) || !inherits(poly_sf, "sf") || nrow(poly_sf) == 0) {
    poly_mode <- "ignore"
  }

  d <- df[!is.na(df[[x_col]]) & !is.na(df[[y_col]]), , drop = FALSE]
  n_input <- nrow(d)
  pts <- sf::st_as_sf(d, coords = c(x_col, y_col), crs = src_crs)
  pts <- sf::st_transform(pts, proj_crs)

  use_loc <- !is.null(loc_col) && loc_col %in% names(d)
  keep <- rep(TRUE, nrow(d))
  if (poly_mode != "only" && use_loc && length(localities) > 0 && !("ALL" %in% localities)) {
    keep <- keep & (as.character(d[[loc_col]]) %in% as.character(localities))
  }

  poly_proj <- NULL
  poly_hit <- NULL
  if (poly_mode %in% c("intersect", "only")) {
    poly_proj <- sf::st_transform(poly_sf, proj_crs)
    if (!"label" %in% names(poly_proj)) {
      poly_proj$label <- paste("Polygon", seq_len(nrow(poly_proj)))
    }
    hits <- sf::st_intersects(pts, poly_proj)
    keep <- keep & (lengths(hits) > 0)
    poly_hit <- vapply(hits, function(h) if (length(h)) h[1] else NA_integer_, integer(1))
  }

  d <- d[keep, , drop = FALSE]
  pts <- pts[keep, , drop = FALSE]
  if (nrow(d) == 0) {
    return(list(df = d, group = character(0), boundary_wkt = NULL,
                n_input = n_input, n_scoped = 0L))
  }

  group <- if (poly_mode == "only") {
    as.character(poly_proj$label)[poly_hit[keep]]
  } else if (use_loc) {
    as.character(d[[loc_col]])
  } else {
    rep("All data", nrow(d))
  }

  boundary <- if (poly_mode == "only") {
    sf::st_union(sf::st_geometry(poly_proj))
  } else {
    hulls <- .classif_scope_hulls(pts, group = if (use_loc) group else NULL,
                                  style = boundary_style,
                                  buffer_mode = buffer_mode, buffer_dist = buffer_dist)
    if (poly_mode == "intersect") {
      poly_u <- sf::st_union(sf::st_geometry(poly_proj))
      bi <- tryCatch({
        x <- suppressWarnings(sf::st_intersection(poly_u, hulls))
        if (any(sf::st_geometry_type(x) == "GEOMETRYCOLLECTION")) {
          x <- tryCatch(sf::st_collection_extract(x, "POLYGON"), error = function(e) x)
        }
        sf::st_union(x)
      }, error = function(e) NULL)
      if (is.null(bi) || length(bi) == 0 || all(sf::st_is_empty(bi))) poly_u else bi
    } else {
      hulls
    }
  }

  # Full-precision WKT: the default writer truncates to 7 significant digits,
  # which at UTM northing magnitudes shifts the boundary by up to ~0.5 m and
  # can push boundary samples outside the reconstructed domain.
  list(df = d, group = group,
       boundary_wkt = sf::st_as_text(sf::st_union(boundary), digits = 15),
       n_input = n_input, n_scoped = nrow(d))
}

#' Per-area performance from pooled out-of-fold predictions: one row per scope
#' group plus a Total row (which reproduces the pooled headline metrics). Class
#' metrics only — probability metrics are unstable on small per-area subsets.
#' Metrics undefined for an area (e.g. a class never observed there) come back
#' NA rather than erroring.
classif_group_metrics <- function(pred_df, target, group_col = ".scope_group") {
  ms <- classif_class_metric_set()
  core <- function(sub) {
    m <- tryCatch(suppressWarnings(
      ms(sub, truth = !!rlang::sym(target), estimate = !!rlang::sym(".pred_class"))),
      error = function(e) NULL)
    grab <- function(id) {
      v <- if (is.null(m)) numeric(0) else m$.estimate[m$.metric == id]
      if (length(v)) v[1] else NA_real_
    }
    data.frame(n = nrow(sub), accuracy = grab("accuracy"), kap = grab("kap"),
               bal_accuracy = grab("bal_accuracy"), f_meas = grab("f_meas"))
  }
  rows <- list()
  if (group_col %in% names(pred_df)) {
    grp <- as.character(pred_df[[group_col]])
    for (g in sort(unique(grp))) {
      rows[[length(rows) + 1]] <- cbind(
        data.frame(scope = g, stringsAsFactors = FALSE),
        core(pred_df[grp == g, , drop = FALSE]))
    }
  }
  rows[[length(rows) + 1]] <- cbind(
    data.frame(scope = "Total", stringsAsFactors = FALSE), core(pred_df))
  out <- do.call(rbind, rows)
  rownames(out) <- NULL
  out
}

# ── Orchestrator (worker-safe) ──────────────────────────────────────────────
#' End-to-end classification run intended to execute inside a future worker.
#' Takes a plain data.frame plus coordinate/CRS metadata (not an sf, which is
#' cheaper to serialise), builds projected points, cross-validates, and — when
#' make_surface = TRUE — fits the final model and predicts over the grid.
#'
#' Returns ONLY serialisable objects (data.frames, tables, vectors): no terra
#' SpatRaster (external pointers cannot cross a future boundary) and no
#' in-memory fitted workflow. The caller rasterises `surface_df` with
#' classif_surface_to_rasters() in the main session. The fitted workflow is
#' instead persisted to `model_rds_path` (when given) as an .rds bundle with
#' its training metadata, so the trained model survives the session and can be
#' reused on new data (verified: nnet/ranger/xgboost fits all round-trip
#' through saveRDS across R processes on the pinned package versions).
run_classification_pipeline <- function(df, target, predictors,
                                        x_col, y_col, src_crs, proj_crs,
                                        method = "rf", strategy = "spatial",
                                        depth = "none", v = 10L,
                                        grid_res = NULL, boundary = "concave",
                                        buffer_mode = "fixed", buffer_dist = 250,
                                        make_surface = TRUE, seed = 12345L,
                                        group_col = NULL, boundary_wkt = NULL,
                                        class_weights = FALSE,
                                        model_rds_path = NULL,
                                        importance_reps = 5L,
                                        importance_mode = c("oof", "training"),
                                        nested = FALSE,
                                        progress_dir = NULL,
                                        session_id = "classif",
                                        cancel_file = NULL) {
  # File-based progress + cooperative cancel, mirroring the interpolation
  # pipeline (update_progress_file reads these options inside the worker).
  report_progress <- !is.null(progress_dir)
  if (report_progress) {
    options(monolith_progress_dir = progress_dir)
    options(monolith_session_id = session_id)
  }
  # Stage-aware reporter; a no-op for direct calls and tests (no progress_dir).
  report <- .classif_progress_reporter(progress_dir, session_id, make_surface)
  if (is.null(report)) report <- function(...) invisible(NULL)
  importance_mode <- match.arg(importance_mode)
  .classif_check_cancel(cancel_file)

  keep_cols <- unique(c(target, predictors, x_col, y_col, group_col))
  d <- df[, intersect(keep_cols, names(df)), drop = FALSE]
  d <- d[!is.na(d[[x_col]]) & !is.na(d[[y_col]]), , drop = FALSE]

  grp <- if (!is.null(group_col) && group_col %in% names(d)) as.character(d[[group_col]]) else NULL

  pts <- sf::st_as_sf(d, coords = c(x_col, y_col), crs = src_crs)
  pts <- sf::st_transform(pts, proj_crs)
  co <- sf::st_coordinates(pts); pts$x <- co[, 1]; pts$y <- co[, 2]

  cv <- run_classification_cv(pts, target, predictors, method = method,
                              strategy = strategy, v = v, depth = depth, seed = seed,
                              group = grp, class_weights = class_weights,
                              nested = nested,
                              oof_importance = identical(importance_mode, "oof"),
                              importance_reps = importance_reps,
                              cancel_file = cancel_file,
                              progress_cb = function(frac, label = NULL) report("cv", frac, label))

  levs <- levels(as.factor(sf::st_drop_geometry(pts)[[target]]))
  out <- list(
    cv_metrics = as.data.frame(cv$metrics),
    per_class  = cv$per_class,
    conf_mat   = cv$conf_mat$table,
    best_params = if (is.null(cv$best_params)) NULL else as.data.frame(cv$best_params),
    nested = isTRUE(cv$nested),
    nested_params = cv$nested_params,
    fold_id = cv$fold_id, n_folds = cv$n_folds,
    method = method, strategy = strategy, depth = depth,
    n = nrow(pts), levels = levs, predictors = predictors,
    target_col = target,
    # Pooled out-of-fold predictions (small: n rows) power the main session's
    # confidence-threshold coverage/selective-accuracy readout.
    cv_predictions = cv$predictions,
    lift = classif_covariate_lift(cv$predictions, target),
    weights_requested = isTRUE(class_weights),
    weights_applied = isTRUE(cv$weights_applied),
    group_metrics = if (is.null(grp)) NULL else classif_group_metrics(cv$predictions, target)
  )

  # The final model is always fitted (not only for surfaces): it drives the
  # permutation feature importance and the exportable model bundle.
  .classif_check_cancel(cancel_file)
  report("fit", 0, "Fitting the final model on all points...")
  model <- fit_classification_model(pts, target, predictors, method = method,
                                    depth = depth, strategy = strategy,
                                    v = v, seed = seed,
                                    class_weights = class_weights)
  report("fit", 1)

  .classif_check_cancel(cancel_file)
  # Out-of-fold importance was already scored inside the CV loop (each fold's
  # model on its own held-out rows), so there is nothing left to do here; only
  # the training-row design needs the final model. Falling back when the pooled
  # frame is NULL covers the degenerate cases (no predictors, every fold empty).
  cv_imp <- if (identical(importance_mode, "oof")) cv$importance else NULL
  if (!is.null(cv_imp)) {
    out$importance <- cv_imp
    report("importance", 1)
  } else {
    report("importance", 0, "Scoring permutation feature importance...")
    train_cc <- sf::st_drop_geometry(pts)
    out$importance <- tryCatch(
      classif_permutation_importance(model, train_cc, target, predictors,
                                     n_rep = importance_reps, seed = seed,
                                     cancel_file = cancel_file,
                                     progress = function(f) report("importance", f)),
      error = function(e) NULL)
  }
  # An importance failure is non-fatal and is swallowed above; a cancellation
  # raised inside that loop must NOT be. Re-read the flag to tell them apart.
  .classif_check_cancel(cancel_file)

  if (!is.null(model_rds_path)) {
    bundle <- list(
      workflow   = model$workflow,
      method     = method,
      method_label = unname(classif_methods()[method]),
      target     = target,
      levels     = levs,
      predictors = predictors,
      class_weights_applied = isTRUE(model$weights_applied),
      tuning_depth = depth,
      # The exported workflow's OWN tuned hyperparameters (full-data tuning in
      # fit_classification_model) — the CV loop's selection can differ and,
      # under nested CV, doesn't even exist as a single set.
      best_params  = if (is.null(model$best_params)) NULL else as.data.frame(model$best_params),
      proj_crs   = proj_crs,
      n_train    = nrow(pts),
      trained_at = Sys.time(),
      versions   = list(
        r = R.version.string,
        parsnip = as.character(utils::packageVersion("parsnip")),
        workflows = as.character(utils::packageVersion("workflows")),
        recipes = as.character(utils::packageVersion("recipes")),
        engine = tryCatch(as.character(utils::packageVersion(
          .classif_method_defs()[[method]]$engine)), error = function(e) NA_character_)
      ),
      usage = paste("b <- readRDS('classification_model.rds');",
                    "predict(b$workflow, new_data, type = 'prob')  # or type = 'class'.",
                    "new_data needs the predictor columns listed in b$predictors.")
    )
    out$model_path <- tryCatch({
      saveRDS(bundle, model_rds_path)
      model_rds_path
    }, error = function(e) NULL)
  }

  if (make_surface) {
    .classif_check_cancel(cancel_file)
    report("grid", 0, "Building the prediction grid...")
    bnd_sf <- if (is.null(boundary_wkt)) NULL else sf::st_as_sfc(boundary_wkt, crs = proj_crs)
    gr <- classif_build_grid(pts, res = grid_res, boundary = boundary, boundary_sf = bnd_sf,
                             buffer_mode = buffer_mode, buffer_dist = buffer_dist)
    report("grid", 1)
    n_cell_lab <- format(nrow(gr$grid_p), big.mark = ",")

    .classif_check_cancel(cancel_file)
    report("covariates", 0,
           sprintf("Interpolating covariates onto %s grid cells...", n_cell_lab))
    grid_aux <- build_classification_grid_aux(
      pts, gr$grid_p, predictors,
      cancel_file = cancel_file,
      progress = function(f) report("covariates", f))

    report("surface", 0, sprintf("Classifying %s grid cells...", n_cell_lab))
    surf <- predict_classification_surface(
      model, sf::st_drop_geometry(grid_aux),
      cancel_file = cancel_file,
      progress = function(f) report("surface", f))

    cell_ha <- gr$res^2 / 10000
    counts <- table(factor(as.character(surf$.pred_class), levels = levs))
    out$surface_df <- surf
    out$res <- gr$res
    out$crs_wkt <- gr$crs_wkt
    out$area <- data.frame(class = levs, n_cells = as.integer(counts),
                           area_ha = as.numeric(counts) * cell_ha,
                           stringsAsFactors = FALSE)
  }
  report("surface", 1, "Finishing...")
  # A cancel requested during the last unguarded moments still counts: the module
  # has already told the user the run is being cancelled, so a completed result
  # must never arrive behind their back.
  .classif_check_cancel(cancel_file)
  out
}
