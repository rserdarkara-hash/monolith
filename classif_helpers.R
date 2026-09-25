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

# How many BOUNDING-BOX cells a classification prediction grid may consider
# before its resolution is coarsened. The raster template spans the full bbox
# and a multi-part scope (distant localities) covers far more of it than the
# hulls do, so this bounds the candidate grid, not the surviving one. Both the
# Auto resolution and a manual one from the slider are floored by it. Named so
# the suite can shrink it and exercise the cap without allocating the budget.
.CLASSIF_MAX_CANDIDATE_CELLS <- 4e6

# The IDW that stands in for a numeric covariate surface kriging could not
# provide (krige_covariates), on the grid and in every fold; the fallback
# notes (classif_covariate_notes) state these values.
.CLASSIF_COV_IDW <- list(idw_p = 2, idw_nmax = 12)

# ── Seed sandbox ────────────────────────────────────────────────────────────
# Thin alias for the app-wide sandbox `with_seed()` (spatial_vgm.R): seed the
# RNG, run `expr`, then restore the caller's .Random.seed (or remove it if the
# caller had none) so classification folds are reproducible without perturbing
# the global stream. The name and the ~40 call sites stay as they are; only the
# implementation is shared now. Safe by load order: global.R and the
# classification worker both source spatial_helpers.R BEFORE this file.
.classif_with_seed <- function(seed, expr) {
  with_seed(seed, expr)
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
#' warning notification. A sample is a sampled location (classif_resolve_scope
#' merges co-located rows), so `n_complete` counts complete locations.
classif_scope_adequacy <- function(tvec, n_complete, min_rows = 20L, min_class = 3L) {
  tab <- table(droplevels(as.factor(tvec)))
  small <- tab[tab < min_class]
  problems <- character(0)
  if (n_complete < min_rows) {
    problems <- c(problems, sprintf("only %d complete locations (need >= %d)", n_complete, min_rows))
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

# ── Collinearity screen step ────────────────────────────────────────────────
#' Recipe step that reruns the iterative VIF screen (detect_multicollinearity_engine,
#' the engine the module's Auto-Drop modal uses) whenever the recipe is prepped.
#' tidymodels preps a recipe on every analysis set, so the screen is repeated
#' on the training rows of each outer fold, tuning resample and nested inner
#' fold, and on all rows for the final model: held-out covariates never help
#' choose the predictors that are scored on them. Trained and row-independent
#' at bake (it only removes columns), as classif_build_recipe requires.
step_vif_screen <- function(recipe, ..., role = NA, trained = FALSE, threshold = 10,
                            removals = NULL, skip = FALSE,
                            id = recipes::rand_id("vif_screen")) {
  recipes::add_step(recipe, .step_vif_screen_new(
    terms = rlang::enquos(...), role = role, trained = trained, threshold = threshold,
    removals = removals, skip = skip, id = id))
}

.step_vif_screen_new <- function(terms, role, trained, threshold, removals, skip, id) {
  recipes::step(subclass = "vif_screen", terms = terms, role = role, trained = trained,
                threshold = threshold, removals = removals, skip = skip, id = id)
}

prep.step_vif_screen <- function(x, training, info = NULL, ...) {
  cols <- recipes::recipes_eval_select(x$terms, training, info)
  cols <- cols[vapply(cols, function(p) is.numeric(training[[p]]), logical(1))]
  removals <- character(0)
  if (length(cols) >= 2) {
    chk <- suppressWarnings(detect_multicollinearity_engine(
      as.data.frame(training[, cols, drop = FALSE]), vars = cols,
      vif_threshold = x$threshold))
    removals <- intersect(chk$dropped, cols)
  }
  .step_vif_screen_new(terms = x$terms, role = x$role, trained = TRUE,
                       threshold = x$threshold, removals = removals,
                       skip = x$skip, id = x$id)
}

bake.step_vif_screen <- function(object, new_data, ...) {
  drop <- intersect(object$removals, names(new_data))
  if (length(drop)) new_data <- new_data[, setdiff(names(new_data), drop), drop = FALSE]
  new_data
}

print.step_vif_screen <- function(x, width = max(20, options()$width - 30), ...) {
  cat("VIF screen (threshold ", x$threshold, ")",
      if (isTRUE(x$trained)) paste0(": removed ", if (length(x$removals)) paste(x$removals, collapse = ", ") else "none"),
      "\n", sep = "")
  invisible(x)
}

tidy.step_vif_screen <- function(x, ...) {
  tibble::tibble(terms = if (isTRUE(x$trained)) as.character(x$removals) else character(0),
                 id = rep(x$id, if (isTRUE(x$trained)) length(x$removals) else 0L))
}

# Registered explicitly: the generics are called from inside the recipes and
# generics namespaces, and a method that only lives in the global environment
# of a sourced session or PSOCK worker must not depend on search-path lookup.
registerS3method("prep", "step_vif_screen", prep.step_vif_screen, envir = asNamespace("recipes"))
registerS3method("bake", "step_vif_screen", bake.step_vif_screen, envir = asNamespace("recipes"))
registerS3method("tidy", "step_vif_screen", tidy.step_vif_screen, envir = asNamespace("generics"))
registerS3method("print", "step_vif_screen", print.step_vif_screen)

#' Numeric covariates the VIF screen removes on all rows of `train_df`;
#' character(0) when no finite threshold is set (Keep All).
.classif_screen_all_rows <- function(train_df, predictors, vif_threshold) {
  if (!(is.numeric(vif_threshold) && length(vif_threshold) == 1 && is.finite(vif_threshold))) {
    return(character(0))
  }
  num <- predictors[vapply(predictors, function(p) is.numeric(train_df[[p]]), logical(1))]
  if (length(num) < 2) return(character(0))
  intersect(suppressWarnings(detect_multicollinearity_engine(
    as.data.frame(train_df[, num, drop = FALSE]), vars = num,
    vif_threshold = vif_threshold))$dropped, num)
}

#' Covariates the fitted workflow's VIF screen removed (character(0) when the
#' recipe has no screen step).
classif_screened_out <- function(fitted_wf) {
  tryCatch({
    rec <- workflows::extract_recipe(fitted_wf)
    st <- Filter(function(s) inherits(s, "step_vif_screen"), rec$steps)
    if (length(st)) as.character(st[[1]]$removals) else character(0)
  }, error = function(e) character(0))
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
classif_build_recipe <- function(train_df, target, predictors, weight_col = NULL,
                                 vif_threshold = NULL) {
  train_df <- as.data.frame(train_df)
  train_df[[target]] <- as.factor(train_df[[target]])
  keep <- c(target, predictors, weight_col)
  train_df <- train_df[, keep, drop = FALSE]

  form <- stats::as.formula(paste0("`", target, "` ~ ",
                                   paste(sprintf("`%s`", c(predictors, weight_col)), collapse = " + ")))

  # EVERY step here must stay TRAINED and ROW-INDEPENDENT. Two things depend on
  # it: `predict_classification_surface` predicts the grid in blocks and relies
  # on block == whole-grid identity, and `.classif_perm_fast_path` permutes baked
  # columns instead of raw ones. A step that mixes rows (step_pca, an
  # interaction, a spatial lag) breaks the first outright; the second detects it
  # at runtime and falls back, but the surface path does not.
  rec <- recipes::recipe(form, data = train_df) |>
    recipes::step_novel(recipes::all_nominal_predictors()) |>
    recipes::step_impute_median(recipes::all_numeric_predictors()) |>
    recipes::step_impute_mode(recipes::all_nominal_predictors())
  # Auto-Drop: the collinearity screen is part of the recipe, so it reruns on
  # every set the recipe is prepped on (see step_vif_screen). NULL = Keep All
  # or nothing flagged: no screen.
  if (is.numeric(vif_threshold) && length(vif_threshold) == 1 && is.finite(vif_threshold)) {
    rec <- step_vif_screen(rec, recipes::all_numeric_predictors(), threshold = vif_threshold)
  }
  rec |>
    recipes::step_dummy(recipes::all_nominal_predictors()) |>
    recipes::step_zv(recipes::all_predictors()) |>
    recipes::step_normalize(recipes::all_numeric_predictors())
}

# ── Fold construction ───────────────────────────────────────────────────────
#' Class-stratified random k-fold, the Standard strategy's folds: the samples
#' of each class in random order, the classes one after another, and fold ids
#' dealt in turn along that sequence from a random order of the v folds. Each
#' class spreads over as many folds as it has samples (a class of m <= v
#' samples lands in m different folds, so no training set loses more than one
#' of its samples), and the folds differ in size by at most one. The fold count
#' does not follow the smallest class: only a singleton leaves a fold's
#' training set without its class (a reported class gap), and it would at any
#' fold count. Seed-sandboxed. The same partition is kNNDM's random partition
#' in this suite and the random reference of its CV Distance Match panel.
classif_stratified_folds <- function(y, v, seed = 12345L) {
  y <- droplevels(as.factor(y))
  n <- length(y)
  .classif_with_seed(seed, {
    ord <- unlist(lapply(levels(y), function(lev) {
      idx <- which(y == lev)
      idx[sample.int(length(idx))]
    }), use.names = FALSE)
    folds <- integer(n)
    folds[ord] <- rep_len(sample.int(v), length(ord))
    folds
  })
}

#' Integer fold vector for classification CV, mirroring make_cv_folds' contract
#' (one integer per row). `spatial` clusters projected coordinates
#' (spatialsample::spatial_clustering_cv, k-means, matching the interpolation
#' engines' spatial-block convention); `standard` is the class-stratified
#' random k-fold (classif_stratified_folds; unstratified random k-fold without
#' a target). Both are seed-sandboxed. v is clamped to the point count, so no
#' fold is empty.
#' `knndm` matches the folds to the prediction grid's locations `domain_xy`
#' (knndm_folds, with k = v), with the `standard` folds for the same v and seed
#' as its random partition: where kNNDM chooses spatial folds they are
#' returned, and everywhere else - random folds already match the map, no
#' spatial partition is valid, there is no domain, or fewer than
#' CV_KNNDM_MIN_N points - the `standard` folds. attr(, "knndm") records the
#' choice (knndm_folds' record; branch "small" below CV_KNNDM_MIN_N).
classif_make_fold_id <- function(pts_sf, strategy = c("spatial", "knndm", "standard"),
                                 target = NULL, v = 10L, seed = 12345L, domain_xy = NULL) {
  strategy <- match.arg(strategy)
  n <- nrow(pts_sf)
  v <- max(2L, min(as.integer(v), n))
  if (strategy == "standard") {
    if (is.null(target)) return(cv_random_folds(n, v, seed))
    return(classif_stratified_folds(sf::st_drop_geometry(pts_sf)[[target]], v, seed))
  }
  if (strategy == "knndm") {
    rnd <- classif_make_fold_id(pts_sf, "standard", target = target, v = v, seed = seed)
    if (n < CV_KNNDM_MIN_N) {
      attr(rnd, "knndm") <- list(branch = "small", q = NA_integer_, k = v, W = NA_real_,
                                 W_random = NA_real_, ks_p = NA_real_,
                                 n_domain = NROW(domain_xy), units = n, exact = TRUE)
      return(rnd)
    }
    return(knndm_folds(sf::st_coordinates(pts_sf), domain_xy, k = v, maxp = KNNDM_MAXP, seed = seed,
                       random_folds = rnd))
  }

  .classif_with_seed(seed, {
    sp <- spatialsample::spatial_clustering_cv(pts_sf, v = v)
    fold_id <- rep(NA_integer_, n)
    for (i in seq_along(sp$splits)) {
      assess_idx <- rsample::complement(sp$splits[[i]])
      fold_id[assess_idx] <- i
    }
    # Degenerate geometry can leave a point unassigned; fold it into cluster 1.
    fold_id[is.na(fold_id)] <- 1L
    fold_id
  })
}

#' Turn an integer fold vector into an rsample rset over `train_df`, so the same
#' fold assignment can drive tune::fit_resamples / tune_grid. Each fold's
#' assessment set is the rows tagged with that fold id.
#' With `target` given and a `.case_wt` column present, every split gets its
#' own copy of its analysis rows carrying class weights recomputed from THOSE
#' rows, so no assessment label enters the class frequencies a tuning fit is
#' weighted by. Importance weights never enter the metrics, so the assessment
#' rows' weights are irrelevant.
classif_folds_to_rset <- function(train_df, fold_id, assess_df = NULL, target = NULL) {
  ids <- sort(unique(fold_id))
  if (!is.null(target) && ".case_wt" %in% names(train_df)) {
    parts <- list(if (is.null(assess_df)) train_df else assess_df)
    offset <- nrow(parts[[1]])
    idx <- vector("list", length(ids))
    for (k in seq_along(ids)) {
      tr <- train_df[fold_id != ids[k], , drop = FALSE]
      tr$.case_wt <- hardhat::importance_weights(.classif_class_weights(tr[[target]]))
      parts[[k + 1]] <- tr
      idx[[k]] <- list(analysis = offset + seq_len(nrow(tr)),
                       assessment = which(fold_id == ids[k]))
      offset <- offset + nrow(tr)
    }
    data <- dplyr::bind_rows(parts)
    splits <- lapply(idx, rsample::make_splits, data = data)
    return(rsample::manual_rset(splits, ids = paste0("Fold", seq_along(ids))))
  }
  data <- if (is.null(assess_df)) train_df else dplyr::bind_rows(train_df, assess_df)
  offset <- if (is.null(assess_df)) 0L else nrow(train_df)
  splits <- lapply(ids, function(i) {
    assess <- offset + which(fold_id == i)
    analysis <- which(fold_id != i)
    rsample::make_splits(list(analysis = analysis, assessment = assess), data = data)
  })
  rsample::manual_rset(splits, ids = paste0("Fold", seq_along(ids)))
}

#' Integer covariates stored as double. Held-out and grid covariates are kriged
#' (double), and a workflow trained on an integer column refuses them at
#' prediction ("loss of precision"). Values are unchanged.
.classif_numeric_as_double <- function(df, predictors) {
  for (p in predictors) {
    if (is.integer(df[[p]])) df[[p]] <- as.double(df[[p]])
  }
  df
}

#' Build the covariates each fold would have at its held-out locations using
#' only that fold's analysis rows. Target labels are retained for scoring, but
#' never passed to the covariate-surface builder.
#' attr(, "cov_fallback"): one row (fold, covariate) per numeric covariate
#' whose held-out values came from the IDW fallback (krige_covariates); NULL
#' when every fold's covariates were kriged.
.classif_fold_assessment <- function(keep_sf, train_df, predictors, fold_id,
                                     cancel_file = NULL, progress = NULL) {
  assess_df <- train_df
  ids <- sort(unique(fold_id))
  fallback <- list()
  for (k in seq_along(ids)) {
    .classif_check_cancel(cancel_file)
    idx <- which(fold_id == ids[k])
    grid <- sf::st_sf(geometry = sf::st_geometry(keep_sf[idx, ]))
    aux <- build_classification_grid_aux(
      keep_sf[fold_id != ids[k], ], grid, predictors,
      cancel_file = cancel_file,
      progress = function(f) {
        if (is.function(progress)) progress((k - 1 + f) / length(ids))
      })
    fb <- attr(aux, "cov_fallback")
    if (length(fb)) {
      fallback[[length(fallback) + 1]] <- data.frame(fold = ids[k], covariate = fb,
                                                     stringsAsFactors = FALSE)
    }
    cov <- sf::st_drop_geometry(aux)
    for (p in predictors) {
      values <- cov[[p]]
      if (is.factor(train_df[[p]])) {
        values <- factor(as.character(values), levels = levels(train_df[[p]]),
                         ordered = is.ordered(train_df[[p]]))
      } else if (is.character(train_df[[p]])) {
        values <- as.character(values)
      } else if (is.logical(train_df[[p]])) {
        values <- as.logical(as.character(values))
      }
      assess_df[[p]][idx] <- values
    }
    if (is.function(progress)) progress(k / length(ids))
  }
  attr(assess_df, "cov_fallback") <- if (length(fallback)) do.call(rbind, fallback)
  assess_df
}

#' One sentence per numeric covariate whose surface came from the IDW
#' fallback of krige_covariates() (a kriging error, or a solve that returned
#' no prediction): where it happened (the prediction grid, and in how many
#' cross-validation folds). `fb` is the pipeline's `covariate_fallback`
#' record; `label_of` maps a column name to its display label. NULL when
#' every covariate surface was kriged.
classif_covariate_notes <- function(fb, label_of = identity) {
  if (is.null(fb)) return(NULL)
  folds <- fb$folds
  covs <- unique(c(if (!is.null(folds)) as.character(folds$covariate), fb$grid))
  if (!length(covs)) return(NULL)
  vapply(covs, function(cv) {
    n_f <- if (is.null(folds)) 0L else length(unique(folds$fold[folds$covariate == cv]))
    where <- c(if (cv %in% fb$grid) "on the prediction grid",
               if (n_f > 0) sprintf("in %d of %d cross-validation folds", n_f, fb$n_folds))
    sprintf("%s: kriging failed %s; inverse distance weighting (p = %s, %d nearest samples) was used instead.",
            label_of(cv), paste(where, collapse = " and "),
            format(.CLASSIF_COV_IDW$idw_p), as.integer(.CLASSIF_COV_IDW$idw_nmax))
  }, character(1), USE.NAMES = FALSE)
}

# ── Metrics ─────────────────────────────────────────────────────────────────
#' Class-label metric set for the statistics yardstick owns here: overall
#' accuracy, Cohen's kappa and balanced accuracy. Precision, recall and F1 are
#' NOT in it - they come from classif_macro_metrics() below, which scores an
#' unpredicted class rather than dropping it. Used on POOLED out-of-fold
#' predictions.
classif_class_metric_set <- function() {
  yardstick::metric_set(
    yardstick::accuracy,
    yardstick::kap,
    yardstick::bal_accuracy
  )
}

#' Precision, recall and F1 over a FIXED class universe.
#'
#' Every class in `universe` contributes exactly once to each of the three
#' averages. A class the model never predicts has an undefined 0/0 precision;
#' yardstick removes it from the multiclass average (with a warning raised
#' inside the worker, where nothing sees it), which puts the three figures on
#' different denominators - a macro F1 could then exceed both its own
#' components, and a model that ignores a class outscored one that found a
#' little of it. Scikit-learn's zero_division = 0 convention is used instead:
#' the class scores 0 and stays in the average, which is the only choice that
#' keeps one denominator and the only one consistent with reporting macro
#' metrics so minority classes are not masked.
#'
#' `n_true == 0` is a different situation and stays different: that class has
#' no reference sample in this evaluation population, so its recall is not
#' evaluable and comes back NA (which makes the macro recall NA - a loud
#' failure, never a silent zero). Unused target levels are dropped before CV,
#' so it cannot arise for a pooled run; it does arise per area, where
#' `universe` is restricted to the classes that area actually holds.
#'
#' Two classes are reported with the BINARY estimator (the first level is the
#' event), which is what the guides state and what every two-class run has
#' always reported - with the same zero-division rule, so only the degenerate
#' "no predicted events" case moves.
classif_macro_metrics <- function(truth, pred, classes = levels(as.factor(truth)),
                                  universe = classes) {
  truth <- factor(as.character(truth), levels = classes)
  pred <- factor(as.character(pred), levels = classes)
  # Counts over the FULL class set: a prediction of a class outside `universe`
  # is still a miss for the true class it displaced.
  tab <- table(pred, truth)
  tp <- diag(tab)
  n_pred <- rowSums(tab)
  n_true <- colSums(tab)

  precision <- ifelse(n_pred == 0, 0, tp / n_pred)
  recall <- ifelse(n_true == 0, NA_real_, tp / n_true)
  f1 <- ifelse(is.na(recall), NA_real_,
               ifelse(precision + recall == 0, 0,
                      2 * precision * recall / (precision + recall)))

  binary <- length(classes) == 2
  keep <- if (binary) classes[1] else intersect(classes, universe)
  est <- if (binary) "binary" else "macro"
  out <- data.frame(
    .metric = c("precision", "recall", "f_meas"),
    .estimator = est,
    .estimate = c(mean(precision[keep]), mean(recall[keep]), mean(f1[keep])),
    # The denominator the three averages share, so a reader can see it; a
    # binary run names its event class instead.
    .n_classes = if (binary) NA_integer_ else length(keep),
    .event = if (binary) classes[1] else NA_character_,
    stringsAsFactors = FALSE
  )
  attr(out, "per_class") <- data.frame(
    class = classes, n_true = as.integer(n_true), n_pred = as.integer(n_pred),
    precision = as.numeric(precision), recall = as.numeric(recall),
    f1 = as.numeric(f1), in_universe = classes %in% keep,
    stringsAsFactors = FALSE)
  out
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
#' (.metric/.estimator/.estimate), preserving the raw ids. A macro average
#' carries the number of classes it was taken over and a binary one its event
#' class: both are part of what the estimator IS, and without them two rows
#' averaged over different class sets read as comparable numbers.
classif_label_metrics <- function(m) {
  ml <- classif_metric_labels(); el <- classif_estimator_labels()
  lab <- unname(ml[m$.metric]); lab[is.na(lab)] <- m$.metric[is.na(lab)]
  est <- unname(el[m$.estimator]); est[is.na(est)] <- m$.estimator[is.na(est)]
  k <- if (".n_classes" %in% names(m)) m$.n_classes else rep(NA_integer_, nrow(m))
  ev <- if (".event" %in% names(m)) m$.event else rep(NA_character_, nrow(m))
  est <- ifelse(!is.na(k), sprintf("%s (K = %d)", est, as.integer(k)),
                ifelse(!is.na(ev), sprintf("%s (event: %s)", est, ev), est))
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
  # Precision / recall / F1 over the fixed class universe, not yardstick's
  # drop-the-undefined-class default (see classif_macro_metrics).
  out <- dplyr::bind_rows(out, classif_macro_metrics(truth, pred_df$.pred_class, levs))

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
#' matrix, the standard DSM per-class report. `n` counts the class's reference
#' samples and `n_pred` the predictions of it: a class with `n_pred == 0` is
#' one the model never assigned, which is why its user accuracy is undefined
#' (the displays say "no predictions" rather than leaving the cell blank).
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
    n_pred = as.integer(row_tot),
    producer_accuracy = ifelse(col_tot > 0, diagv / col_tot, NA_real_),
    user_accuracy = ifelse(row_tot > 0, diagv / row_tot, NA_real_),
    stringsAsFactors = FALSE
  )
}

# ── Cross-validation ────────────────────────────────────────────────────────
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
    # step/total carries the percent scaled by 1000; update_progress_file itself
    # rounds step/total to a whole percent, so the reported bar resolution is 1%.
    update_progress_file("classification", "cls", round(pct * 1000), 1000)
    if (!is.null(label)) {
      tryCatch(writeLines(label, stage_file), error = function(e) NULL)
    }
    invisible(NULL)
  }
}

#' Align one fold's probability frame to the trained level set.
#'
#' A fold's analysis rows need not contain every class: under spatial folds a
#' spatially clustered class can fall entirely inside one held-out block, and
#' even class-stratified folds strand a singleton class in exactly one fold.
#' ranger and nnet::multinom then DROP the unused outcome level ("Dropped unused
#' factor level(s) in dependent variable"), so `predict(type = "prob")` comes
#' back with fewer `.pred_<level>` columns than the target has levels and the
#' pooled `rbind` fails with "numbers of columns of arguments do not match"
#' (xgboost happens to keep all columns, so the crash was engine-dependent).
#'
#' Absent classes are padded with probability 0 and the columns pinned to the
#' trained level order. Zero is the fold model's genuine posterior, not an
#' imputation: a model fitted without a class cannot assign it any mass. Rows
#' whose truth IS that class therefore score as a total miss, which is the
#' honest reading — the fold had no chance at them. `missing` reports the
#' affected classes so the caller can surface the CV-design caveat.
.classif_align_prob_cols <- function(prob, lvl) {
  prob <- as.data.frame(prob)
  want <- paste0(".pred_", lvl)
  absent <- setdiff(want, names(prob))
  for (cc in absent) prob[[cc]] <- 0
  list(prob = prob[, want, drop = FALSE],
       missing = sub("^\\.pred_", "", absent))
}

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
#' so inner selection faces the same leakage regime as the outer estimate;
#' under kNNDM, folds matched on the outer fold's analysis rows against the
#' same prediction grid locations `domain_xy`).
#' Cost multiplies roughly by `inner_v`.
run_classification_cv <- function(pts_sf, target, predictors,
                                  method = "rf",
                                  strategy = c("spatial", "knndm", "standard"),
                                  v = 10L, depth = "none", seed = 12345L,
                                  group = NULL, class_weights = FALSE,
                                  nested = FALSE, inner_v = 5L,
                                  oof_importance = FALSE, importance_reps = 5L,
                                  vif_threshold = NULL,
                                  cancel_file = NULL, progress_cb = NULL,
                                  domain_xy = NULL) {
  strategy <- match.arg(strategy)
  if (!is.null(group) && length(group) != nrow(pts_sf)) {
    stop("`group` must have one entry per row of `pts_sf`.")
  }

  full_df <- as.data.frame(sf::st_drop_geometry(pts_sf))
  cc <- stats::complete.cases(full_df[, c(target, predictors), drop = FALSE])
  keep_sf <- pts_sf[cc, ]
  train_df <- .classif_numeric_as_double(full_df[cc, c(target, predictors), drop = FALSE], predictors)
  # droplevels, not as.factor alone: classif_build_target() carries EVERY bin
  # label as a level (an equal-interval or Jenks break can enclose no samples),
  # and dropping rows with missing covariates can empty a level that the scoped
  # data did populate. A level with no rows is not a class the model can learn:
  # it makes yardstick's macro recall undefined (bal_accuracy comes back NA),
  # adds an all-zero row and column to the confusion matrix, and later trips
  # predict_classification_surface()'s missing-column stop() because the fitted
  # engine never emits a probability column for it.
  train_df[[target]] <- droplevels(as.factor(train_df[[target]]))
  lvl <- levels(train_df[[target]])
  grp <- if (is.null(group)) NULL else as.character(group)[cc]

  if (nlevels(train_df[[target]]) < 2) {
    stop("Classification target must have at least two classes after removing missing rows.")
  }

  # Class-imbalance weighting: inverse-frequency case weights, applied only
  # when the engine supports them. Every fit recomputes them from its own
  # analysis rows: the out-of-fold refits below, and each tuning resample
  # (classif_folds_to_rset with `target`), so no held-out class-prevalence
  # information enters a fit.
  weights_applied <- isTRUE(class_weights) && classif_supports_weights(method)
  weight_col <- if (weights_applied) ".case_wt" else NULL
  if (weights_applied) {
    train_df$.case_wt <- hardhat::importance_weights(
      .classif_class_weights(train_df[[target]]))
  }

  n_cls <- nlevels(train_df[[target]])
  rec <- classif_build_recipe(train_df, target, predictors, weight_col = weight_col,
                              vif_threshold = vif_threshold)
  tune_params <- .classif_effective_tune_params(method, depth, n_cls)
  spec <- classif_build_spec(method, tune_params = tune_params, n_classes = n_cls)
  wf <- workflows::workflow() |>
    workflows::add_recipe(rec) |>
    workflows::add_model(spec)
  if (weights_applied) wf <- workflows::add_case_weights(wf, .case_wt)

  fold_id <- classif_make_fold_id(keep_sf, strategy, target = target, v = v, seed = seed,
                                  domain_xy = domain_xy)
  nested_active <- isTRUE(nested) && length(tune_params) > 0
  # progress_cb reports progress within the CV stage. Covariate reconstruction
  # has its own share because it can dominate the fold-fitting work.
  cov_share <- 0.25
  tune_share <- if (length(tune_params) > 0 && !nested_active) 0.25 else 0
  fold_share <- 1 - cov_share - tune_share
  report_cv <- if (is.function(progress_cb)) progress_cb else function(...) invisible(NULL)
  report_cv(0, "Interpolating held-out covariates...")
  assess_df <- .classif_fold_assessment(
    keep_sf, train_df, predictors, fold_id, cancel_file = cancel_file,
    progress = function(f) report_cv(cov_share * f,
                                      "Interpolating held-out covariates..."))
  rset <- classif_folds_to_rset(train_df, fold_id, assess_df, target = target)

  resample_metrics <- classif_resample_metric_set()
  fit_wf <- wf
  best_params <- NULL
  tune_grid_df <- NULL
  if (length(tune_params) > 0) {
    # One space-filling grid shared by every tuning pass. Finalising the
    # parameter set against the full predictor frame leaks nothing label-borne
    # (it only pins data-dependent hyperparameter ranges such as mtry <= p).
    tune_grid_df <- .classif_with_seed(seed, {
      pset <- hardhat::extract_parameter_set_dials(wf)
      # Ranges such as mtry <= p count the covariates an Auto-Drop screen keeps
      # on all rows (a range bound, carrying no label information).
      pset <- dials::finalize(pset, x = train_df[, setdiff(predictors,
        .classif_screen_all_rows(train_df, predictors, vif_threshold)), drop = FALSE])
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
    report_cv(cov_share, "Tuning hyperparameters (grid search)...")
    tuned <- .classif_with_seed(seed, {
      tune::tune_grid(wf, resamples = rset, grid = tune_grid_df,
                      metrics = resample_metrics,
                      control = tune::control_grid(save_pred = FALSE, verbose = FALSE))
    })
    best_params <- tune::select_best(tuned, metric = "accuracy")
    fit_wf <- tune::finalize_workflow(wf, best_params)
    report_cv(cov_share + tune_share)
  }

  # Manual out-of-fold loop: fit on the analysis rows, predict hard class AND
  # full class probabilities on the held-out fold, then pool. Hand-rolled (like
  # perform_kriging_loocv) so probability columns are always collected and no
  # per-fold metric warning arises when a spatial fold happens to be single-class.
  # `.row` records each prediction's row index in train_df so the spatial
  # baseline below (and McNemar pairing) can align with the model predictions.
  nested_params <- list()
  imp_parts <- list()
  # (fold, covariate) pairs the fold's own VIF screen removed.
  fold_screen <- list()
  # (fold, class) pairs whose class was absent from that fold's analysis rows.
  # Collected rather than warned about in-worker: the module surfaces them once,
  # naming the classes, because it is a property of the CV design (and of the
  # class balance), not a model failure.
  class_gaps <- list()
  folds_seq <- sort(unique(fold_id))
  n_fold <- length(folds_seq)
  preds <- .classif_with_seed(seed, {
    parts <- lapply(seq_along(folds_seq), function(k) {
      i <- folds_seq[k]
      .classif_check_cancel(cancel_file)
      # Reported at the START of the fold, so the label names the fold that is
      # actually running and the fraction counts folds already finished.
      report_cv(cov_share + tune_share + fold_share * ((k - 1) / n_fold),
                sprintf("Cross-validation: fold %d of %d", k, n_fold))
      tr <- train_df[fold_id != i, , drop = FALSE]
      te <- assess_df[fold_id == i, , drop = FALSE]
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
                                         v = inner_v, seed = seed + i, domain_xy = domain_xy)
        inner_assess <- .classif_fold_assessment(
          keep_sf[fold_id != i, , drop = FALSE], tr, predictors, inner_id,
          cancel_file = cancel_file,
          progress = function(f) report_cv(
            cov_share + tune_share + fold_share * ((k - 1 + 0.25 * f) / n_fold),
            sprintf("Interpolating inner-fold covariates: fold %d of %d", k, n_fold)))
        inner_rset <- classif_folds_to_rset(tr, inner_id, inner_assess, target = target)
        tuned_i <- tune::tune_grid(wf, resamples = inner_rset, grid = tune_grid_df,
                                   metrics = resample_metrics,
                                   control = tune::control_grid(save_pred = FALSE, verbose = FALSE))
        bp_i <- tune::select_best(tuned_i, metric = "accuracy")
        nested_params[[length(nested_params) + 1]] <<-
          cbind(data.frame(.fold = i), as.data.frame(bp_i)[, setdiff(names(bp_i), ".config"), drop = FALSE])
        fold_wf <- tune::finalize_workflow(wf, bp_i)
      }
      fit_i <- parsnip::fit(fold_wf, data = tr)
      screened_i <- classif_screened_out(fit_i)
      if (length(screened_i)) {
        fold_screen[[length(fold_screen) + 1]] <<-
          data.frame(fold = i, covariate = screened_i, stringsAsFactors = FALSE)
      }
      # Out-of-fold permutation importance: score THIS fold's model on the rows
      # it never saw, before they are used for anything else. Done here because
      # the fold's fitted workflow only exists inside this iteration — the
      # alternative (refitting afterwards) would double the CV cost, whereas
      # this reuses a fit that already exists and predicts the same number of
      # rows in total as the training-row design. Permutation shuffles a
      # predictor WITHIN the assessment rows, so very small folds decorrelate
      # it less thoroughly; that caveat is documented in the guide.
      #
      # Its OWN seed sandbox, deliberately: .classif_perm_delta burns
      # n_rep x n_predictors sample.int() draws, and on the shared fold-loop
      # stream those draws shifted the random state every LATER fold's fit
      # started from — so a purely diagnostic toggle could move the reported
      # accuracy/kappa/confusion matrix. It bites whenever the learner consumes
      # RNG: with the current registry that is xgboost at depth "full" (which
      # tunes sample_size and mtry, i.e. row subsampling and colsample), while
      # multinom (nnet's rang = 0 starts weights at zero), ranger (engine
      # seed = 12345L) and xgboost at none/light (sample_size = 1) fit
      # deterministically. Nesting the two-sided sandbox restores the fold
      # stream on exit, so the CV loop is byte-identical whether or not
      # importance is requested — for every learner, including any stochastic
      # one added later — and each fold's importance is independently
      # reproducible from `seed` and `k` alone.
      # A covariate this fold's own Auto-Drop screen removed is not in its
      # model, so permuting it would score a zero for a covariate the fold
      # never used and drag the pooled importance of a genuinely used one down.
      # It is left out of this fold's contribution, and the pool averages each
      # covariate over the folds that kept it.
      imp_preds <- setdiff(predictors, screened_i)
      if (isTRUE(oof_importance) && length(imp_preds)) {
        imp_parts[[length(imp_parts) + 1]] <<-
          c(.classif_with_seed(seed + 977L + k,
              .classif_perm_delta(fit_i, te, target, imp_preds,
                                  n_rep = importance_reps, cancel_file = cancel_file)),
            list(predictors = imp_preds))
      }
      cls <- predict(fit_i, te, type = "class")
      # Pad any class this fold's model never saw, so every fold contributes the
      # same columns in the same order and the pooled rbind below is well posed.
      al <- .classif_align_prob_cols(predict(fit_i, te, type = "prob"), lvl)
      if (length(al$missing)) {
        class_gaps[[length(class_gaps) + 1]] <<-
          data.frame(fold = i, class = al$missing, stringsAsFactors = FALSE)
      }
      p <- cbind(data.frame(.fold = i, .row = which(fold_id == i)),
                 te[, target, drop = FALSE],
                 # Rebuilt against the trained level set rather than trusted:
                 # rbind() on factors with differing levels coerces silently.
                 .pred_class = factor(as.character(cls$.pred_class), levels = lvl),
                 al$prob)
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
    # The point set the baseline was scored on, so a 1-NN map drawn from it is
    # the surface that baseline accuracy describes.
    base_xy = unname(sf::st_coordinates(keep_sf)[, 1:2, drop = FALSE]),
    base_y = train_df[[target]],
    method = method,
    strategy = strategy,
    fold_id = fold_id,
    assessment_df = assess_df,
    n_folds = length(unique(fold_id)),
    # The levels actually modelled (complete cases, empty levels dropped) — the
    # single source of truth for the surface columns and the exported bundle.
    levels = lvl,
    # NULL when every fold saw every class; otherwise one row per (fold, class)
    # the fold's training set lacked. See .classif_align_prob_cols().
    class_gaps = if (length(class_gaps)) do.call(rbind, class_gaps) else NULL,
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
    fold_screen = if (length(fold_screen)) do.call(rbind, fold_screen) else NULL,
    # (fold, covariate) pairs whose held-out values came from the IDW fallback.
    cov_fallback = attr(assess_df, "cov_fallback"),
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

#' The spatial 1-NN baseline as a MAP: every grid cell takes the class of its
#' nearest sample (Euclidean, projected coords) - the categorical analogue of
#' Thiessen polygons. Built from the same complete-case point set the CV
#' baseline scores, so the reported baseline accuracy describes this surface.
#' Hard classes only: 1-NN has no class probabilities, so no entropy either.
classif_nn_surface <- function(train_xy, train_y, grid_xy, levels = NULL) {
  train_xy <- as.matrix(train_xy)
  grid_xy <- as.matrix(grid_xy)
  y <- as.character(train_y)
  lev <- if (is.null(levels)) sort(unique(y)) else levels
  nn <- FNN::get.knnx(train_xy, grid_xy, k = 1)$nn.index[, 1]
  data.frame(x = grid_xy[, 1], y = grid_xy[, 2],
             .pred_class = factor(y[nn], levels = lev))
}

#' Covariate-free cross-validation: the spatial 1-NN classifier IS the model.
#' Uses the same fold construction as run_classification_cv (same seed, same
#' strategy, points with a target value), so on a dataset without missing
#' covariates its folds and out-of-fold predictions equal the `.pred_base`
#' column of a covariate run. No probabilities exist, so only the class
#' metrics are computed. `.pred_base` mirrors `.pred_class` so every consumer
#' of the pooled predictions keeps working.
run_classification_nn_cv <- function(pts_sf, target, strategy = c("spatial", "knndm", "standard"),
                                     v = 10L, seed = 12345L, group = NULL, domain_xy = NULL) {
  strategy <- match.arg(strategy)
  if (!is.null(group) && length(group) != nrow(pts_sf)) {
    stop("`group` must have one entry per row of `pts_sf`.")
  }
  full_df <- as.data.frame(sf::st_drop_geometry(pts_sf))
  cc <- !is.na(full_df[[target]])
  keep_sf <- pts_sf[cc, ]
  y <- droplevels(as.factor(full_df[[target]][cc]))
  if (nlevels(y) < 2) {
    stop("Classification target must have at least two classes after removing missing rows.")
  }
  lvl <- levels(y)
  grp <- if (is.null(group)) NULL else as.character(group)[cc]

  fold_id <- classif_make_fold_id(keep_sf, strategy, target = target, v = v, seed = seed,
                                  domain_xy = domain_xy)
  xy <- unname(sf::st_coordinates(keep_sf)[, 1:2, drop = FALSE])
  base <- classif_spatial_baseline(xy, y, fold_id)

  # A fold whose training rows hold no sample of a class cannot assign it:
  # the same CV-design caveat the covariate path reports.
  gaps <- lapply(sort(unique(fold_id)), function(i) {
    miss <- setdiff(lvl, as.character(unique(y[fold_id != i])))
    if (length(miss)) data.frame(fold = i, class = miss, stringsAsFactors = FALSE)
  })
  gaps <- Filter(Negate(is.null), gaps)

  preds <- data.frame(.fold = fold_id, .row = seq_along(y))
  preds[[target]] <- y
  preds$.pred_class <- base
  preds$.pred_base <- base
  if (!is.null(grp)) preds$.scope_group <- grp

  list(
    base_xy = xy, base_y = y,
    method = "nn", strategy = strategy,
    fold_id = fold_id, n_folds = length(unique(fold_id)),
    levels = lvl,
    class_gaps = if (length(gaps)) do.call(rbind, gaps) else NULL,
    predictions = preds,
    metrics = classif_compute_metrics(preds, target),
    per_class = classif_per_class_accuracy(preds, target),
    conf_mat = yardstick::conf_mat(preds, truth = !!rlang::sym(target),
                                   estimate = !!rlang::sym(".pred_class")),
    majority_acc = max(table(y)) / length(y),
    best_params = NULL, nested = FALSE, importance = NULL, weights_applied = FALSE
  )
}

#' The fold design a classification run was scored under, as its badge and
#' its Metrics CSV name it: the strategy, and for kNNDM the design it chose
#' (`knndm`, classif_make_fold_id()'s record) - random or spatial folds, or the
#' random folds it used without a search and why.
classif_cv_label <- function(strategy, knndm = NULL) {
  switch(strategy %||% "",
    spatial = "Spatial blocked CV",
    knndm = switch(knndm$branch %||% "",
      random   = "kNNDM CV [random folds]",
      spatial  = "kNNDM CV [spatial folds]",
      small    = sprintf("Random k-fold CV [kNNDM needs n ≥ %d]", CV_KNNDM_MIN_N),
      none     = "Random k-fold CV [kNNDM: no prediction domain]",
      fallback = "Random k-fold CV [kNNDM: no valid spatial partition]",
      "kNNDM CV"),
    "Random k-fold CV")
}

#' Plain-language reading of the covariate-lift result. McNemar's test is
#' two-sided, so the direction comes from lift_abs and significance from p.
#' `target_mode` ("cat" / "bin") decides where a significantly negative lift
#' points the user; `strategy` adds the random-fold caveat.
classif_lift_interpretation <- function(lift, target_mode = "cat", strategy = "spatial") {
  p <- lift$mcnemar_p
  d <- lift$lift_abs
  msg <- if (is.na(p)) {
    "McNemar test: not defined - the covariate model and the spatial baseline are right and wrong on exactly the same points."
  } else {
    p_txt <- if (p < 0.001) "p < 0.001" else sprintf("p = %.3f", p)
    if (p >= 0.05) {
      sprintf("McNemar %s: no significant difference from the spatial baseline - the covariates add no demonstrable information beyond spatial position.", p_txt)
    } else if (d > 0) {
      sprintf("McNemar %s: the covariate model is significantly MORE accurate than the spatial baseline.", p_txt)
    } else if (identical(target_mode, "bin")) {
      sprintf("McNemar %s: the covariate model is significantly LESS accurate than the spatial baseline. Consider interpolating the continuous variable in the Spatial Engine and classifying that surface instead.", p_txt)
    } else {
      sprintf("McNemar %s: the covariate model is significantly LESS accurate than the spatial baseline. Consider the Spatial 1-NN map (map selector above).", p_txt)
    }
  }
  if (identical(strategy, "standard")) {
    msg <- paste(msg, "Random folds favour the 1-NN baseline, because each held-out point keeps its near neighbours in the training set; re-check under Spatial blocked CV.")
  }
  msg
}

#' Covariate lift: how much the covariate model improves on two no-covariate
#' baselines, computed from pooled out-of-fold predictions that carry
#' `.pred_class` (model) and `.pred_base` (spatial 1-NN baseline).
#'
#' - `majority_acc` is the no-information rate (always predict the modal class).
#' - `baseline_acc` / `baseline_kap` score the spatial 1-NN baseline on the
#'   same folds, so the comparison shares the identical validation design.
#' - `lift_abs` is the accuracy gain in points over the spatial baseline;
#'   `mcnemar_p` is McNemar's paired test on the discordant correct/incorrect
#'   pairs (exact binomial below 25 discordant pairs, continuity-corrected
#'   chi-square above) — the standard significance test for comparing two
#'   classifiers on the same samples (Dietterich 1998).
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
  # Exact binomial below 25 discordant pairs, continuity-corrected chi-square
  # above it (Edwards 1948; the usual switch point). mcnemar.test() is the
  # chi-square APPROXIMATION to the binomial, and it is unreliable exactly
  # where this statistic usually lands: b + c_ counts only the samples where
  # the covariate model and the spatial 1-NN baseline disagree in correctness,
  # which on a few hundred well-separated points is routinely under 25. Under
  # the null both discordant cells are equally likely, so the exact test is
  # binom.test(c_, b + c_, p = 0.5).
  mcnemar_p <- if ((b + c_) > 0) {
    tryCatch({
      if ((b + c_) < 25L) {
        stats::binom.test(c_, b + c_, p = 0.5)$p.value
      } else {
        stats::mcnemar.test(matrix(c(0, b, c_, 0), nrow = 2))$p.value
      }
    }, error = function(e) NA_real_)
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
#'
#' Predicting through the workflow re-bakes the whole recipe on every shuffle
#' (n_rep x n_predictors bakes per evaluation frame, inside the CV loop under
#' the out-of-fold design). The fast path below bakes ONCE and permutes the
#' baked block instead — see `.classif_perm_fast_path` for why that is exact and
#' how it is verified. It is a pure efficiency change: same draws, same order,
#' same numbers, and any frame where the equivalence cannot be demonstrated
#' falls back to the original path per predictor.
.classif_perm_delta <- function(wf, df, target, predictors, n_rep = 5L,
                                cancel_file = NULL, progress = NULL) {
  truth <- as.factor(df[[target]])

  # Out-of-fold design: this fold's model may have been fitted without one of
  # the classes and so emits no column for it (see .classif_align_prob_cols).
  # Without the pad, match() returns NA, p_true is NA, and the WHOLE importance
  # frame collapses to NA. Padding at 0 keeps the baseline and the permuted
  # scores on the same footing, so their difference — which is all the
  # importance measure uses — stays meaningful.
  score <- function(prob) {
    prob <- as.matrix(prob)
    colnames(prob) <- sub("^\\.pred_", "", colnames(prob))
    absent <- setdiff(levels(truth), colnames(prob))
    if (length(absent)) {
      prob <- cbind(prob, matrix(0, nrow(prob), length(absent),
                                 dimnames = list(NULL, absent)))
    }
    p_true <- prob[cbind(seq_len(nrow(prob)), match(as.character(truth), colnames(prob)))]
    -mean(log(pmax(p_true, 1e-15)))
  }
  log_loss_of <- function(newdata) score(predict(wf, newdata, type = "prob"))

  base_ll <- log_loss_of(df)

  # SETUP RUNS IN AN RNG SANDBOX, and that is load-bearing, not defensive:
  # `predict()` itself consumes random numbers for some engines (measured:
  # ranger's does), so the verification predict below would otherwise shift the
  # stream every later sample.int() draws from and move the importances. Inside
  # the sandbox the ambient state is restored, so the shuffle sequence is exactly
  # the one the re-bake-per-shuffle implementation produced. What keeps the loop
  # itself aligned is that both branches make the SAME number of predict calls
  # (one per repeat) — keep it that way.
  fast <- with_rng_sandbox({
    fp <- .classif_perm_fast_path(wf, df, predictors)
    # Second half of the runtime proof: the baked frame must reproduce the
    # workflow's OWN baseline prediction. If it does not (an engine whose
    # predict method expects something forge() supplies), the fast path is
    # discarded wholesale rather than trusted.
    if (!is.null(fp)) {
      base_fast <- tryCatch(score(predict(fp$model, fp$baked, type = "prob")),
                            error = function(e) NA_real_)
      if (!isTRUE(all.equal(base_fast, base_ll))) fp <- NULL
    }
    fp
  })
  ll_baked <- if (is.null(fast)) NULL else {
    function(b) score(predict(fast$model, b, type = "prob"))
  }

  n_pred <- length(predictors)
  # The cancel check, the progress tick and the fast/slow branch consume no RNG:
  # exactly one sample.int(n) per (predictor, repeat), in the same nesting order
  # as before, so the shuffle sequence and every importance value are unchanged.
  delta <- vapply(seq_len(n_pred), function(j) {
    p <- predictors[j]
    .classif_check_cancel(cancel_file)
    use_fast <- !is.null(fast) && isTRUE(fast$ok[[p]])
    cols <- if (use_fast) fast$blocks[[p]] else NULL
    lls <- vapply(seq_len(n_rep), function(r) {
      idx <- sample.int(nrow(df))
      if (use_fast) {
        b2 <- fast$baked
        if (length(cols)) b2[cols] <- fast$baked[idx, cols, drop = FALSE]
        ll_baked(b2)
      } else {
        d2 <- df
        d2[[p]] <- d2[[p]][idx]
        log_loss_of(d2)
      }
    }, numeric(1))
    if (is.function(progress)) progress(j / n_pred)
    mean(lls) - base_ll
  }, numeric(1))

  list(delta = delta, baseline = base_ll, n = nrow(df))
}

#' Fast-path setup for `.classif_perm_delta`: bake the evaluation frame once and
#' map each raw predictor to the baked column block it owns.
#'
#' Why permuting the baked block is the same thing as permuting the raw column:
#' every step in `classif_build_recipe` is TRAINED at prep time and applied
#' ROW-WISE at bake time (novel-level absorption, median/mode imputation, dummy
#' expansion, zero-variance column removal, normalisation), and each baked column
#' is a function of exactly one raw predictor. A row permutation therefore
#' commutes with baking — including the NA case, where permute-then-impute and
#' impute-then-permute both put the trained median wherever the NA landed.
#'
#' That is an argument about the recipe as it stands today, so it is checked
#' rather than assumed: each predictor is probed with one fixed non-identity
#' permutation and the fast result must reproduce a real bake exactly. A step
#' that is not row-independent (a future step_pca, an interaction term, a spatial
#' lag) or a block-mapping collision (a nominal `ph` with level `field` sitting
#' beside a numeric `ph_field`) fails the probe and that predictor silently
#' reverts to the slow path instead of returning a wrong importance.
#'
#' @return list(model, baked, blocks, ok) or NULL if the workflow cannot be split.
.classif_perm_fast_path <- function(wf, df, predictors) {
  tryCatch({
    rec <- workflows::extract_recipe(wf)
    mdl <- workflows::extract_fit_parsnip(wf)
    baked <- recipes::bake(rec, new_data = df, recipes::all_predictors())
    nms <- names(baked)

    # A numeric predictor keeps its own name through the recipe; a nominal one is
    # replaced by its step_dummy block. Both are candidates only — the probe is
    # what makes them safe.
    blocks <- lapply(predictors, function(p) {
      if (p %in% nms) p else nms[startsWith(nms, paste0(p, "_"))]
    })
    names(blocks) <- predictors

    idx <- rev(seq_len(nrow(df)))
    ok <- vapply(predictors, function(p) {
      d2 <- df
      d2[[p]] <- d2[[p]][idx]
      probe <- recipes::bake(rec, new_data = d2, recipes::all_predictors())
      fast <- baked
      cols <- blocks[[p]]
      if (length(cols)) fast[cols] <- baked[idx, cols, drop = FALSE]
      isTRUE(all.equal(as.data.frame(probe), as.data.frame(fast),
                       check.attributes = FALSE))
    }, logical(1))
    names(ok) <- predictors

    list(model = mdl, baked = baked, blocks = blocks, ok = ok)
  }, error = function(e) NULL)
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
  # n_pred x n_fold, NA where a fold's own Auto-Drop screen removed the
  # covariate, so each covariate is averaged over the folds that used it and a
  # covariate no fold kept is left out of the table entirely. Without a screen
  # every part carries every predictor and this is the plain weighted mean.
  dl <- matrix(NA_real_, nrow = length(predictors), ncol = length(parts),
               dimnames = list(predictors, NULL))
  for (j in seq_along(parts)) {
    dl[parts[[j]]$predictors %||% predictors, j] <- parts[[j]]$delta
  }
  used <- !is.na(dl)
  denom <- vapply(seq_len(nrow(dl)), function(r) sum(w[used[r, ]]), numeric(1))
  dl[!used] <- 0
  keep <- denom > 0
  delta <- as.numeric(dl %*% w)[keep] / denom[keep]
  base <- sum(vapply(parts, function(z) z$baseline, numeric(1)) * w) / sum(w)
  .classif_importance_frame(predictors[keep], delta, base, "out-of-fold")
}

# ── Final fit ───────────────────────────────────────────────────────────────
#' Fit the final classification workflow on all training points, tuning first
#' if `depth` requests it. The tuning CV uses the SAME `strategy` as the run's
#' evaluation CV: tuning under random stratified folds while reporting spatial-CV
#' metrics picks hyperparameters against exactly the optimism spatial CV exists
#' to remove, and those hyperparameters are what the map, the entropy surface,
#' the permutation importance and the exported .rds bundle are all built from.
#' Under kNNDM the same `domain_xy` gives the same folds as the CV's, so its
#' held-out covariates (`cv_assessment_df`) are reused.
#' Returns the fitted workflow plus the target levels for downstream raster
#' layer naming.
fit_classification_model <- function(pts_sf, target, predictors,
                                     method = "rf", depth = "none",
                                     strategy = c("spatial", "knndm", "standard"),
                                     v = 10L, seed = 12345L,
                                     class_weights = FALSE,
                                     cv_assessment_df = NULL, cv_fold_id = NULL,
                                     vif_threshold = NULL, domain_xy = NULL) {
  strategy <- match.arg(strategy)
  full_df <- as.data.frame(sf::st_drop_geometry(pts_sf))
  cc <- stats::complete.cases(full_df[, c(target, predictors), drop = FALSE])
  train_df <- .classif_numeric_as_double(full_df[cc, c(target, predictors), drop = FALSE], predictors)
  # Same droplevels contract as run_classification_cv, so `model$levels` (which
  # names the surface probability columns and the exported bundle's classes) can
  # never claim a class the fitted engine has no column for.
  train_df[[target]] <- droplevels(as.factor(train_df[[target]]))

  weights_applied <- isTRUE(class_weights) && classif_supports_weights(method)
  weight_col <- if (weights_applied) ".case_wt" else NULL
  if (weights_applied) {
    train_df$.case_wt <- hardhat::importance_weights(
      .classif_class_weights(train_df[[target]]))
  }

  n_cls <- nlevels(train_df[[target]])
  screened_out <- .classif_screen_all_rows(train_df, predictors, vif_threshold)
  rec <- classif_build_recipe(train_df, target, predictors, weight_col = weight_col,
                              vif_threshold = vif_threshold)
  tune_params <- .classif_effective_tune_params(method, depth, n_cls)
  spec <- classif_build_spec(method, tune_params = tune_params, n_classes = n_cls)
  wf <- workflows::workflow() |>
    workflows::add_recipe(rec) |>
    workflows::add_model(spec)
  if (weights_applied) wf <- workflows::add_case_weights(wf, .case_wt)

  final_params <- NULL
  if (length(tune_params) > 0) {
    keep_sf <- pts_sf[cc, ]
    fold_id <- classif_make_fold_id(keep_sf, strategy, target = target, v = v, seed = seed,
                                    domain_xy = domain_xy)
    assess_df <- if (!is.null(cv_assessment_df) && identical(fold_id, cv_fold_id) &&
                     nrow(cv_assessment_df) == nrow(train_df)) {
      cv_assessment_df
    } else {
      .classif_fold_assessment(keep_sf, train_df, predictors, fold_id)
    }
    rset <- classif_folds_to_rset(train_df, fold_id, assess_df, target = target)
    tuned <- .classif_with_seed(seed, {
      pset <- hardhat::extract_parameter_set_dials(wf)
      # Ranges such as mtry <= p count the covariates an Auto-Drop screen keeps.
      pset <- dials::finalize(pset, x = train_df[, setdiff(predictors, screened_out), drop = FALSE])
      grid <- dials::grid_space_filling(pset, size = .classif_grid_size(depth))
      tune::tune_grid(wf, resamples = rset,
                      grid = grid, metrics = classif_resample_metric_set(),
                      control = tune::control_grid(save_pred = FALSE, verbose = FALSE))
    })
    final_params <- tune::select_best(tuned, metric = "accuracy")
    wf <- tune::finalize_workflow(wf, final_params)
  }

  # Auto-Drop: the tuning resamples above screened inside each split; the
  # model itself is fitted on the covariates the same screen keeps on all
  # rows, WITHOUT the custom step. The exported bundle is loaded in sessions
  # that do not define step_vif_screen's methods, and it should not ask for
  # covariates the model never uses.
  if (is.numeric(vif_threshold) && length(vif_threshold) == 1 && is.finite(vif_threshold)) {
    predictors <- setdiff(predictors, screened_out)
    wf <- workflows::workflow() |>
      workflows::add_recipe(classif_build_recipe(train_df, target, predictors, weight_col = weight_col)) |>
      workflows::add_model(spec)
    if (weights_applied) wf <- workflows::add_case_weights(wf, .case_wt)
    if (!is.null(final_params)) wf <- tune::finalize_workflow(wf, final_params)
  }

  fitted <- .classif_with_seed(seed, parsnip::fit(wf, data = train_df))
  list(workflow = fitted, levels = levels(train_df[[target]]),
       # The covariates the fitted model uses (after an Auto-Drop screen).
       target = target, predictors = predictors, method = method,
       screened_out = screened_out,
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
#' and a progress tick between blocks (a whole-grid call runs for minutes with
#' the bar frozen and the cancel flag unread), plus a lower peak memory
#' footprint.
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
  prob_parts <- vector("list", length(starts))
  for (j in seq_along(starts)) {
    .classif_check_cancel(cancel_file)
    idx <- seq.int(starts[j], min(starts[j] + chunk_size - 1L, n))
    block <- nd[idx, , drop = FALSE]
    prob_parts[[j]] <- as.data.frame(predict(wf, block, type = "prob"))
    if (is.function(progress)) progress(j / length(starts))
  }
  prob <- if (length(prob_parts) == 1L) prob_parts[[1]] else do.call(rbind, prob_parts)
  # rbind carries each block's row names through; drop them so the assembled
  # frame is identical whatever the block size.
  rownames(prob) <- NULL

  # Every learner in .classif_method_defs() is a probability model, so the hard
  # class IS the argmax of the probabilities parsnip just returned — asking for
  # type = "class" as well would re-bake the recipe and re-run the model over
  # the whole grid a second time, on the longest stage of the run. The only
  # behavioural difference is at exact probability ties, where ties.method =
  # "first" fixes the winner on the trained level order instead of leaving it to
  # the engine. The factor is rebuilt against the trained level set rather than
  # relying on c()/unlist() level handling, which differs across R versions.
  lev <- model$levels
  if (is.null(lev)) lev <- sub("^\\.pred_", "", names(prob))
  prob_cols <- paste0(".pred_", lev)
  missing_cols <- setdiff(prob_cols, names(prob))
  if (length(missing_cols) > 0) {
    stop("Predicted probabilities are missing columns for class level(s): ",
         paste(sub("^\\.pred_", "", missing_cols), collapse = ", "))
  }
  prob_mat <- as.matrix(prob[, prob_cols, drop = FALSE])
  cls <- factor(lev[max.col(prob_mat, ties.method = "first")], levels = lev)
  # A row with any non-finite probability has no defined argmax; predict(type =
  # "class") returns NA there too.
  cls[!stats::complete.cases(prob_mat)] <- NA

  out <- cbind(nd, .pred_class = cls, prob)
  # Entropy comes from prob_mat, the frame validated against the trained level
  # set three lines above — not from the raw predict() output. Identical today
  # (every learner returns exactly the level columns), but an engine that ever
  # returned an extra column would silently rescale the whole uncertainty
  # surface instead of tripping the missing-column guard.
  out$.entropy <- classif_shannon_entropy(prob_mat)
  out
}

# ── Rasterisation ───────────────────────────────────────────────────────────
#' Rasterise a prediction surface. `grid_sf` must carry x/y (projected) plus the
#' `.pred_class`, `.pred_<level>`, and `.entropy` columns from
#' predict_classification_surface(). Returns a list with a categorical class
#' SpatRaster, a multi-layer probability SpatRaster, an entropy SpatRaster, and
#' a per-class area table in ellipsoidal (true ground) hectares from
#' terra::expanse, the same call the app's continuous classified-map area
#' reporting and class-zone GIS export use.
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

  # One coordinate matrix for every layer below (class + k probability layers +
  # entropy), built once: inside the probability lapply it would materialise an
  # n x 2 matrix once per class on a million-cell grid.
  xy <- as.matrix(df[, c("x", "y")])

  class_r <- terra::rasterize(xy, templ, values = cls_int)
  levels(class_r) <- data.frame(ID = seq_along(levs_map), class = levs_map)
  # Embed the same viridis palette the in-app map uses so the exported class
  # GeoTIFF opens coloured in GIS/image viewers (a bare integer band renders
  # greyscale). viridisLite is a hard dependency of ggplot2, always installed.
  # Abstained cells get a neutral grey, matching the on-screen palette.
  pal <- viridisLite::viridis(length(levs))
  if (use_abstain) pal <- c(pal, "#9E9E9E")
  terra::coltab(class_r) <- data.frame(value = seq_along(levs_map), col = pal)
  names(class_r) <- "class"

  # A hard-class surface (the spatial 1-NN map) carries no probabilities and no
  # entropy: those layers are NULL rather than fabricated.
  prob_r <- NULL
  if (length(prob_cols)) {
    prob_r <- terra::rast(lapply(prob_cols, function(cc) {
      terra::rasterize(xy, templ, values = df[[cc]])
    }))
    names(prob_r) <- sub("^\\.pred_", "P_", prob_cols)
  }

  ent_r <- NULL
  if (!is.null(df$.entropy)) {
    ent_r <- terra::rasterize(xy, templ, values = df$.entropy)
    names(ent_r) <- "entropy"
  }

  counts <- table(factor(cls_chr, levels = levs_map))
  # Ellipsoidal area, from the SAME terra::expanse call the interpolation Area
  # Coverage table (server_sci_analysis.R) and the class-zone GIS export
  # (spatial_pipeline.R) make, so the three cannot disagree. The planimetric
  # alternative (n_cells x res^2) is the area IN THE PROJECTION PLANE and
  # differs from ground area by the CRS's area scale factor k^2: measured
  # -0.08% on a UTM central meridian, +0.19% at the zone edge. Small, but
  # systematic, and in a column labelled hectares on the ground.
  # class_r is CATEGORICAL, so expanse(byValue = TRUE) reports `value` as the
  # class LABEL, not the integer ID - match on the label. Falls back to the
  # planimetric figure only if expanse cannot be evaluated.
  cell_ha <- (res * res) / 10000
  area_ha <- as.numeric(counts) * cell_ha
  area_exp <- tryCatch({
    e <- as.data.frame(terra::expanse(class_r, unit = "ha", byValue = TRUE))
    if (all(c("value", "area") %in% names(e)))
      e$area[match(levs_map, as.character(e$value))] else NULL
  }, error = function(e) NULL)
  if (!is.null(area_exp)) area_ha <- ifelse(is.na(area_exp), 0, area_exp)

  area_tbl <- data.frame(
    class = levs_map,
    n_cells = as.integer(counts),
    area_ha = area_ha,
    stringsAsFactors = FALSE
  )

  list(class = class_r, prob = prob_r, entropy = ent_r, area = area_tbl,
       conf_threshold = if (use_abstain) conf_threshold else 0)
}

# ── Prediction grid + covariate surface ─────────────────────────────────────
#' Auto grid resolution for a classification scope: ~50k cells inside the
#' domain, clamped to [5, 1000] m. A multi-part boundary (distant localities)
#' can cover a bounding box far larger than its own area, and the raster
#' template spans the full bbox before clipping, so the resolution is
#' additionally floored to keep the candidate grid below ~4M cells.
#' Shared by classif_build_grid and the module's pre-run resolution advisory so
#' the number the user is shown is the number the run will use.
classif_auto_res <- function(area_m2, bbox) {
  max(.classif_auto_res_rule(area_m2), .classif_res_floor(bbox))
}

#' The Auto rule before the candidate-cell floor: sqrt(area / 50,000) m, about
#' 50,000 cells inside the domain, clamped to [5, 1000] m.
.classif_auto_res_rule <- function(area_m2) {
  max(5, min(1000, sqrt(area_m2 / 50000)))
}

#' The cell size a classification run will grid at, as classif_build_grid
#' derives it (Auto: classif_auto_res; Fixed: classif_cap_res), with the scope
#' note's sentence stating it and, where the candidate-cell budget coarsens it,
#' why. NULL without a scope boundary.
classif_grid_res_note <- function(mode, res, area_m2, bbox) {
  if (is.null(bbox) || (!identical(mode, "fixed") && is.null(area_m2))) return(NULL)
  m <- function(x) paste(trimws(formatC(x, digits = 3, format = "fg")), "m")
  budget <- format(.CLASSIF_MAX_CANDIDATE_CELLS, big.mark = ",", scientific = FALSE)
  if (identical(mode, "fixed")) {
    eff <- classif_cap_res(res, bbox)
    txt <- if (eff > res) {
      sprintf("Grid: Fixed %s would need more than %s cells over the scope's bounding box; the run uses %s cells.",
              m(res), budget, m(eff))
    } else sprintf("Grid: Fixed, %s cells.", m(res))
  } else {
    rule <- .classif_auto_res_rule(area_m2)
    eff <- classif_auto_res(area_m2, bbox)
    txt <- if (eff > rule) {
      sprintf("Grid: Auto, %s cells (the area rule's %s would need more than %s cells over the scope's bounding box).",
              m(eff), m(rule), budget)
    } else sprintf("Grid: Auto, %s cells.", m(eff))
  }
  list(res = eff, text = txt)
}

#' The candidate-cell budget, as a resolution floor in metres. Named so the
#' suite can shrink it and exercise the cap without allocating the budget.
.classif_res_floor <- function(bbox) {
  dx <- as.numeric(bbox["xmax"] - bbox["xmin"])
  dy <- as.numeric(bbox["ymax"] - bbox["ymin"])
  sqrt(dx * dy / .CLASSIF_MAX_CANDIDATE_CELLS)
}

#' Apply the same ~4M candidate-cell floor to a MANUAL resolution. The Auto
#' path has carried it since 2026-07-10; the Manual Resolution slider had no
#' cap at all, and it is the path that can ask for the finest grid. The raster
#' template spans the whole bounding box before the clip, so the slider's 5 m
#' minimum over a multi-locality scope of 20 x 20 km is 1.6e7 candidate nodes -
#' several GB inside the classification worker, where an allocation failure
#' surfaces to the user only as a generic run failure. Shared by
#' classif_build_grid and the module's live resolution advisory so the number
#' the user is shown before the run is the number the run uses.
classif_cap_res <- function(res, bbox) {
  if (is.null(res) || !is.finite(res)) return(res)
  cap <- .classif_res_floor(bbox)
  if (is.finite(cap) && res < cap) cap else res
}

#' The scope boundary a classification run maps, as sf in the points' CRS: a
#' pre-resolved boundary (sf/sfc, e.g. from classif_resolve_scope) supplied as
#' `boundary_sf`, unioned into one geometry, else - for direct calls - the
#' single hull of the points by the Boundary Type, with the style/buffer
#' semantics of .classif_scope_hulls. The prediction grid is clipped to it and
#' kNNDM reads the map's locations from it.
classif_domain_boundary <- function(pts_proj, boundary = "concave", boundary_sf = NULL,
                                    buffer_mode = "fixed", buffer_dist = 250) {
  bnd <- if (!is.null(boundary_sf)) {
    g <- sf::st_geometry(boundary_sf)
    if (is.na(sf::st_crs(g))) sf::st_crs(g) <- sf::st_crs(pts_proj)
    sf::st_union(g)
  } else {
    .classif_scope_hulls(pts_proj, group = NULL, style = boundary,
                         buffer_mode = buffer_mode, buffer_dist = buffer_dist)
  }
  sf::st_as_sf(sf::st_sfc(sf::st_geometry(bnd), crs = sf::st_crs(pts_proj)))
}

#' Build a projected prediction grid inside the scope boundary
#' (classif_domain_boundary), mirroring the interpolation pipeline's approach
#' (hull, terra raster, clip). Resolution defaults to ~50k cells inside the
#' domain, clamped to [5, 1000] m.
#' `strict_scope` says whether the domain in force really is a union of
#' per-point buffers, which is what the buffer/cell-size advisory below is
#' about. It defaults to the boundary style, but a caller that supplies its own
#' `boundary_sf` can override it: polygons-only scoping, for instance, hands
#' over the user's polygons and the Boundary Type control is then inert.
#' `scope` is the boundary classif_domain_boundary() already resolved from the
#' same arguments (run_classification_pipeline resolves it before the
#' cross-validation), so the hull is not built twice.
classif_build_grid <- function(pts_proj, res = NULL,
                               boundary = c("concave", "convex", "bbox", "wrapped", "strict"),
                               boundary_sf = NULL,
                               buffer_mode = "fixed", buffer_dist = 250,
                               strict_scope = NULL, scope = NULL) {
  boundary <- match.arg(boundary)
  if (is.null(strict_scope)) strict_scope <- identical(boundary, "strict")
  bnd <- scope %||% classif_domain_boundary(pts_proj, boundary, boundary_sf, buffer_mode, buffer_dist)

  bbox <- sf::st_bbox(bnd)
  res_note <- NULL
  if (is.null(res)) {
    res <- classif_auto_res(sum(as.numeric(sf::st_area(bnd))), bbox)
  } else {
    res_capped <- classif_cap_res(res, bbox)
    if (!isTRUE(all.equal(res_capped, res))) {
      res_note <- sprintf(
        "Grid resolution %.1f m over this scope would need more than %s candidate cells; coarsened to %.1f m to keep the run inside memory.",
        res, format(.CLASSIF_MAX_CANDIDATE_CELLS, big.mark = ",", scientific = FALSE),
        res_capped)
      res <- res_capped
    }
  }
  # A Point buffer boundary narrower than half the cell diagonal discards
  # the cells of isolated samples (see strict_buffer_gap, spatial_pipeline.R).
  # Reported here because this is the only place the effective resolution is
  # known in Auto mode; advisory only, the grid is still built as asked.
  strict_warning <- if (isTRUE(strict_scope)) {
    strict_buffer_message(buffer_dist, res)
  } else NULL
  # Square cells of exactly `res` (grid_template, spatial_pipeline.R).
  grid_r <- grid_template(bbox, res, sf::st_crs(pts_proj)$wkt)
  # Cell centres as a plain MATRIX, tested against the boundary a block at a
  # time, with the sf built ONCE from the survivors - the same shape
  # run_regional_interpolation uses, and for the same reason: an sfc_POINT
  # stores every node as its own classed numeric(2), ~430 bytes per cell
  # against 16 for a matrix row, and terra::as.points materialised every
  # bounding-box node before anything was discarded. Same st_within predicate,
  # same surviving rows, same order, same columns as the single-pass form.
  # st_within (not st_intersects) is deliberate: a node exactly on the boundary
  # is outside the scope.
  grid_xy <- terra::crds(grid_r, na.rm = FALSE)
  n_bbox <- nrow(grid_xy)
  blk <- max(1L, as.integer(.GRID_CLIP_BLOCK_CELLS))
  inside <- logical(n_bbox)
  for (s in seq.int(1L, n_bbox, by = blk)) {
    e <- min(s + blk - 1L, n_bbox)
    chunk <- sf::st_as_sf(data.frame(x = grid_xy[s:e, 1], y = grid_xy[s:e, 2]),
                          coords = c("x", "y"), crs = sf::st_crs(pts_proj))
    inside[s:e] <- sf::st_within(chunk, bnd, sparse = FALSE)[, 1]
    rm(chunk)
  }
  # Falling back to the full bbox here would silently predict over the entire
  # bounding box — the opposite of what scoping promises. Fail loudly instead;
  # the module's promise-error handler surfaces this message. When the cap has
  # already coarsened the cells, "reduce the resolution" is advice the user
  # cannot take, so the message has to name what actually happened.
  if (!any(inside)) {
    stop("No grid cells fall inside the scope boundary at this resolution; reduce the grid resolution or widen the scope.",
         if (is.null(res_note)) "" else paste0(" ", res_note))
  }
  keep_xy <- grid_xy[inside, , drop = FALSE]
  rm(grid_xy)
  grid_p <- sf::st_as_sf(data.frame(x = keep_xy[, 1], y = keep_xy[, 2]),
                         coords = c("x", "y"), crs = sf::st_crs(pts_proj))
  grid_p$x <- keep_xy[, 1]; grid_p$y <- keep_xy[, 2]
  rm(keep_xy)
  # One advisory channel: the module raises res$grid_warning as a single
  # notification, so a coarsened resolution and a strict-buffer gap must arrive
  # together rather than one silencing the other.
  notes <- c(res_note, strict_warning)
  notes <- notes[!is.na(notes) & nzchar(notes)]
  list(grid_p = grid_p, res = res, crs_wkt = sf::st_crs(pts_proj)$wkt,
       strict_warning = if (length(notes)) paste(notes, collapse = " ") else NULL)
}

#' Populate a prediction grid with covariate values. Numeric covariates are
#' interpolated with the existing krige_covariates() surface builder (same path
#' as RK/RFK); categorical covariates, which cannot be kriged, inherit the class
#' of their nearest training point (FNN). Both approaches are documented as
#' approximations that propagate onto the classifier's inputs.
#' attr(, "cov_fallback") names the numeric covariates whose surface is the
#' IDW fallback (absent when every one was kriged).
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
  fallback <- character(0)
  if (length(num_preds) > 0) {
    lags <- calc_scientific_lags(pts_proj)
    # One covariate kriged onto the full grid is the coarsest interruptible
    # unit here (gstat's krige() call is a black box), so cancel latency in
    # this stage is one covariate.
    kc <- krige_covariates(pts_proj, grid_p, num_preds, lags, .CLASSIF_COV_IDW,
                           on_var = function(i, total) {
                             .classif_check_cancel(cancel_file)
                             if (is.function(progress)) progress(i / length(predictors))
                           })
    grid_aux <- kc$grid_aux
    fallback <- kc$fallback
  }
  if (length(cat_preds) > 0) {
    .classif_check_cancel(cancel_file)
    nn <- FNN::get.knnx(sf::st_coordinates(pts_proj),
                        sf::st_coordinates(grid_p), k = 1)$nn.index[, 1]
    for (j in seq_along(cat_preds)) {
      .classif_check_cancel(cancel_file)
      cp <- cat_preds[j]
      grid_aux[[cp]] <- factor(as.character(df[[cp]])[nn],
                               levels = levels(as.factor(df[[cp]])))
      if (is.function(progress)) progress((length(num_preds) + j) / length(predictors))
    }
  }
  if (length(fallback)) attr(grid_aux, "cov_fallback") <- fallback
  grid_aux
}

# ── Spatial scope (localities / polygons) ───────────────────────────────────
#' Put a point set on a metric CRS before any spatial step of a classification
#' run touches it. The Target Mapping CRS may legitimately be geographic
#' (EPSG:4326 is the first entry of that selector), but grid resolution and
#' hull buffers are metres, the covariate surfaces are kriged on metric lags,
#' spatial-CV blocks are sized in metres and cell area is reported in hectares.
#' Left in degrees, the auto resolution (metres, derived from an s2 area in
#' m^2) is applied to an extent measured in degrees and the prediction grid
#' collapses to a single continent-sized cell: one flat rectangle instead of a
#' map. The fallback is the data's UTM zone, the same one the interpolation
#' pipeline applies (validate_and_project_sf), restated as its EPSG code -
#' the proj4 string that function builds carries none, which would leave the
#' maps and the exported GeoTIFFs describing their projection as "unknown".
classif_project_metric <- function(pts) {
  if (is.null(pts) || nrow(pts) == 0 || !sf::st_is_longlat(pts)) return(pts)
  pts <- validate_and_project_sf(pts)
  tryCatch({
    p4 <- sf::st_crs(pts)$proj4string
    z <- as.integer(sub(".*\\+zone=([0-9]+).*", "\\1", p4))
    stopifnot(grepl("+proj=utm", p4, fixed = TRUE), !is.na(z), z >= 1, z <= 60)
    sf::st_transform(pts, (if (grepl("+south", p4, fixed = TRUE)) 32700 else 32600) + z)
  }, error = function(e) pts)
}

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
#'
#' One sample is one sampled location. The scoped rows whose working-CRS
#' coordinates agree to the centimetre are merged (merge_colocated) BEFORE the
#' target is built: a numeric column (the source-CRS coordinates, a covariate,
#' a variable to bin) becomes the mean of the rows that measured it, a class
#' column (character or factor) the location's majority, NA where the most
#' frequent classes tie. The locality, and the polygon in polygon modes, come
#' from the location's first row, so co-located rows filed under different
#' localities are one location. Every fold design, baseline and map downstream
#' then works on locations.
#'
#' Returns `df` (one row per location), `group`, the boundary, the working
#' CRS, `n_input` (georeferenced rows), `n_rows` (scoped rows), `n_scoped`
#' (scoped locations), `n_merged = c(locations, rows)` (the locations that held
#' two or more rows and how many rows they held) and `majority_ties` (per class
#' column, the locations a tie left without a class).
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
  # remove = FALSE: the source-CRS coordinate columns travel with the points,
  # so a merged location carries their means.
  pts <- sf::st_as_sf(d, coords = c(x_col, y_col), crs = src_crs, remove = FALSE)
  pts <- sf::st_transform(pts, proj_crs)
  # Everything below this line is metric (see classif_project_metric); the CRS
  # actually used travels back so the caller stays on it.
  crs_requested <- sf::st_crs(pts)
  pts <- classif_project_metric(pts)
  work_crs <- sf::st_crs(pts)
  crs_fallback <- !identical(work_crs, crs_requested)

  use_loc <- !is.null(loc_col) && loc_col %in% names(d)
  keep <- rep(TRUE, nrow(d))
  if (poly_mode != "only" && use_loc && length(localities) > 0 && !("ALL" %in% localities)) {
    keep <- keep & (as.character(d[[loc_col]]) %in% as.character(localities))
  }

  poly_proj <- NULL
  poly_hit <- NULL
  if (poly_mode %in% c("intersect", "only")) {
    poly_proj <- sf::st_transform(poly_sf, work_crs)
    if (!"label" %in% names(poly_proj)) {
      poly_proj$label <- paste("Polygon", seq_len(nrow(poly_proj)))
    }
    hits <- sf::st_intersects(pts, poly_proj)
    keep <- keep & (lengths(hits) > 0)
    poly_hit <- vapply(hits, function(h) if (length(h)) h[1] else NA_integer_, integer(1))
  }

  d <- d[keep, , drop = FALSE]
  pts <- pts[keep, , drop = FALSE]
  if (!is.null(poly_hit)) poly_hit <- poly_hit[keep]
  n_rows <- nrow(d)
  cls_cols <- setdiff(names(d)[vapply(d, function(v) is.character(v) || is.factor(v), logical(1))],
                      if (use_loc) loc_col)
  if (n_rows == 0) {
    return(list(df = d, group = character(0), boundary_wkt = NULL,
                boundary_area_m2 = NULL, boundary_bbox = NULL,
                working_crs = work_crs$wkt, crs_fallback = crs_fallback,
                n_input = n_input, n_rows = 0L, n_scoped = 0L,
                n_merged = c(locations = 0L, rows = 0L),
                majority_ties = stats::setNames(integer(length(cls_cols)), cls_cols)))
  }

  # One row per sampled location. The row index travels as TEXT, so the
  # merge keeps each location's first row index instead of averaging it; the
  # locality (a numeric code included) and the polygon hit are then read off
  # that row, never averaged or voted.
  pts$.mn_scope_row <- as.character(seq_len(n_rows))
  pts <- merge_colocated(pts, majority = cls_cols)
  first <- as.integer(pts$.mn_scope_row)
  pts$.mn_scope_row <- NULL
  merged <- attr(pts, "merged")
  ties <- attr(pts, "majority_ties") %||% stats::setNames(integer(length(cls_cols)), cls_cols)
  d_loc <- d
  # A new table, one row per location, with automatic row names whether or not
  # anything merged.
  d <- sf::st_drop_geometry(pts)
  rownames(d) <- NULL
  if (use_loc) d[[loc_col]] <- d_loc[[loc_col]][first]
  if (!is.null(poly_hit)) poly_hit <- poly_hit[first]

  group <- if (poly_mode == "only") {
    as.character(poly_proj$label)[poly_hit]
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
  bnd_u <- sf::st_union(boundary)
  # Area + bbox travel with the scope so the module can reproduce the run's
  # Auto resolution (classif_auto_res) without re-parsing the WKT.
  list(df = d, group = group,
       boundary_wkt = sf::st_as_text(bnd_u, digits = 15),
       boundary_area_m2 = sum(as.numeric(sf::st_area(bnd_u))),
       boundary_bbox = sf::st_bbox(bnd_u),
       # The CRS the boundary, the grid, and every distance are expressed in:
       # the requested target CRS when it is metric, its UTM fallback when it
       # is not. Callers must reuse it rather than the requested CRS.
       working_crs = work_crs$wkt, crs_fallback = crs_fallback,
       n_input = n_input, n_rows = n_rows, n_scoped = nrow(d),
       n_merged = c(locations = as.integer(merged[["groups"]] %||% 0L),
                    rows = as.integer(merged[["rows"]] %||% 0L)),
       majority_ties = ties)
}

#' The Spatial Scope note's count, from classif_resolve_scope(): the scoped
#' locations out of the georeferenced rows, and how many locations held more
#' than one sample. Without co-located rows a location is a point and the
#' sentence says so.
classif_scope_count_text <- function(sc) {
  merged <- sc$n_merged[["locations"]] %||% 0L
  if (!isTRUE(merged > 0)) {
    return(sprintf("In scope: %d of %d georeferenced points.", sc$n_scoped, sc$n_input))
  }
  sprintf("In scope: %d locations from %d of %d georeferenced points; %d location%s held more than one sample and %s merged.",
          sc$n_scoped, sc$n_rows, sc$n_input, merged,
          if (merged == 1) "" else "s", if (merged == 1) "was" else "were")
}

#' The warning for merged locations whose class columns tied (the majority
#' rule of classif_resolve_scope): a categorical target's tied locations have
#' no class and are left out of the run; a categorical covariate's tied
#' locations leave training like any location missing a covariate. NULL when
#' neither holds a tie.
classif_tie_note <- function(sc, target_col = NULL, predictors = character(0),
                             label_of = identity) {
  ties <- sc$majority_ties
  if (!length(ties)) return(NULL)
  n_of <- function(col) if (col %in% names(ties)) ties[[col]] else 0L
  parts <- character(0)
  if (!is.null(target_col) && n_of(target_col) > 0) {
    k <- n_of(target_col)
    parts <- sprintf("%d of %d locations %s no majority class of %s and %s left out.",
                     k, sc$n_scoped, if (k == 1) "has" else "have", label_of(target_col),
                     if (k == 1) "is" else "are")
  }
  for (p in setdiff(predictors, target_col)) {
    k <- n_of(p)
    if (k > 0) {
      parts <- c(parts, sprintf(
        "%d location%s %s no majority value of %s and %s out of training, like a location missing a covariate.",
        k, if (k == 1) "" else "s", if (k == 1) "has" else "have", label_of(p),
        if (k == 1) "is" else "are"))
    }
  }
  if (!length(parts)) return(NULL)
  paste(c(parts, paste("If the rows at a location are depth intervals or repeat surveys,",
                       "classify one interval or survey at a time (a file holding only its rows).")),
        collapse = " ")
}

#' Per-area performance from pooled out-of-fold predictions: one row per scope
#' group plus a Total row (which reproduces the pooled headline metrics). Class
#' metrics only — probability metrics are unstable on small per-area subsets.
#' Metrics undefined for an area (e.g. a class never observed there) come back
#' NA rather than erroring.
#'
#' The macro F1 of an AREA is taken over the classes with reference support in
#' that area: a class the pooled target has but this area does not is not
#' evaluable here, while a class the area holds and the model never predicts
#' there still scores 0 (the F13 rule). `K` reports that denominator per row,
#' because two areas' macro figures need not be over the same class set.
classif_group_metrics <- function(pred_df, target, group_col = ".scope_group") {
  ms <- classif_class_metric_set()
  classes <- levels(as.factor(pred_df[[target]]))
  core <- function(sub) {
    m <- tryCatch(suppressWarnings(
      ms(sub, truth = !!rlang::sym(target), estimate = !!rlang::sym(".pred_class"))),
      error = function(e) NULL)
    grab <- function(id) {
      v <- if (is.null(m)) numeric(0) else m$.estimate[m$.metric == id]
      if (length(v)) v[1] else NA_real_
    }
    supported <- classes[table(factor(as.character(sub[[target]]), levels = classes)) > 0]
    mm <- tryCatch(classif_macro_metrics(sub[[target]], sub$.pred_class, classes,
                                         universe = supported),
                   error = function(e) NULL)
    data.frame(n = nrow(sub), accuracy = grab("accuracy"), kap = grab("kap"),
               bal_accuracy = grab("bal_accuracy"),
               f_meas = if (is.null(mm)) NA_real_ else mm$.estimate[mm$.metric == "f_meas"],
               # NA for a two-class run, which reports the event class's own F1
               # rather than an average over a class set.
               K = if (is.null(mm)) NA_integer_ else as.integer(mm$.n_classes[1]))
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

#' Write the predicted-class download into `dir`: the class GeoTIFF (INT1U, so
#' the colour table is embedded), the `.tif.aux.xml` sidecar in which GDAL
#' stores the category names, and a legend CSV (`ID,class`) that names the codes
#' without depending on the sidecar surviving the reader's unzip tool. Returns
#' the paths written; when GDAL wrote no sidecar the bundle is the tif and the
#' CSV, with a warning. `tags` (named character) are the run's MONOLITH_* tags,
#' set through terra's metadata: GDAL writes them inside the TIFF, and the colour
#' table and categories are written exactly as without them (a gdal_translate
#' pass would have to carry those over).
classif_class_download_files <- function(class_r, dir, base = "predicted_class", tags = NULL) {
  tif <- file.path(dir, paste0(base, ".tif"))
  tags <- tags[!is.na(tags) & nzchar(tags)]
  if (length(tags)) terra::metags(class_r) <- tags
  terra::writeRaster(class_r, tif, overwrite = TRUE, datatype = "INT1U")
  cats <- terra::cats(class_r)[[1]]
  legend <- file.path(dir, paste0(base, "_legend.csv"))
  utils::write.csv(data.frame(ID = cats[[1]], class = as.character(cats[[2]])),
                   legend, row.names = FALSE)
  aux <- paste0(tif, ".aux.xml")
  if (!file.exists(aux)) {
    warning("The class names could not be written beside the GeoTIFF; ",
            "the legend CSV in the download names the class codes.", call. = FALSE)
    return(c(tif, legend))
  }
  c(tif, aux, legend)
}

#' The classification metrics CSV: every result table of the run in one tidy
#' frame of five columns (scope, metric, yardstick_id, estimator, value).
#'
#' Pure, so the exported file is testable; the download handler only writes it.
#' The per-class block and the confusion matrix are in it because they are what
#' lets a reader recompute the macro averages - for an imbalanced problem they
#' are the rows that matter most, and a file that reports only the averages
#' cannot be checked at all. `area` is the rasteriser's area table (it honours
#' the live confidence threshold, so it cannot be recomputed from `res`).
classif_metrics_csv_df <- function(res, area = NULL, area_note = NULL) {
  if (is.null(res)) return(NULL)
  blk <- function(scope, metric, id, estimator, value) {
    if (!length(metric)) return(NULL)
    data.frame(scope = scope, metric = metric, yardstick_id = id,
               estimator = estimator, value = as.numeric(value),
               stringsAsFactors = FALSE)
  }
  m <- classif_label_metrics(res$cv_metrics)
  out <- list(blk(if (isTRUE(res$nn_only)) "Total (spatial 1-NN, no covariates)" else "Total",
                  m$.metric_label, m$.metric, m$.estimator_label, m$.estimate))

  # Per-area rows (class metrics only), matching the Performance by Area table;
  # the Total row above already carries the full pooled metric set.
  gm <- res$group_metrics
  if (!is.null(gm)) {
    gm <- gm[gm$scope != "Total", , drop = FALSE]
    if (nrow(gm) > 0) {
      ml <- classif_metric_labels()
      ids <- c("accuracy", "kap", "bal_accuracy", "f_meas")
      for (i in seq_len(nrow(gm))) {
        est <- c("Multiclass", "Multiclass", "Macro average",
                 if (is.na(gm$K[i])) "Binary" else sprintf("Macro average (K = %d)", gm$K[i]))
        out[[length(out) + 1]] <- blk(gm$scope[i], unname(ml[ids]), ids, est,
                                      as.numeric(gm[i, ids]))
      }
    }
  }

  # Covariate-free run: the rows above ARE the spatial 1-NN model's.
  if (isTRUE(res$nn_only)) {
    out[[length(out) + 1]] <- blk(
      "Baseline comparison", "Majority-class accuracy (no-information rate)",
      "majority_acc", "Pooled out-of-fold", res$majority_acc)
  }
  # Baseline comparison rows (same CV folds as the model metrics above).
  if (!is.null(res$lift)) {
    lf <- res$lift
    out[[length(out) + 1]] <- blk(
      "Baseline comparison",
      c("Spatial 1-NN baseline accuracy", "Spatial 1-NN baseline kappa",
        "Majority-class accuracy (no-information rate)",
        "Covariate lift (accuracy points vs spatial baseline)",
        "McNemar p (model vs spatial baseline)"),
      c("baseline_acc", "baseline_kap", "majority_acc", "lift_abs", "mcnemar_p"),
      "Paired out-of-fold",
      c(lf$baseline_acc, lf$baseline_kap, lf$majority_acc, lf$lift_abs, lf$mcnemar_p))
  }
  # The fold design and how closely its held-out distances match the
  # prediction grid's (the CV Distance Match panel): W for this run's folds and
  # for the random reference partition, in the working CRS's units.
  d <- res$cv_design
  if (!is.null(d)) {
    u <- if (is.na(d$units %||% NA_character_)) "map units" else d$units
    out[[length(out) + 1]] <- blk(
      "CV design",
      c(sprintf("W, this CV (%s)", u), sprintf("W, %s (%s)", d$reference, u)),
      c("W_cv", "W_random"),
      classif_cv_label(res$strategy, res$knndm),
      c(d$W_cv, d$W_random))
  }
  # Permutation feature importance. The scope names the evaluation design (each
  # fold's model on its held-out rows, or the final model on its own training
  # rows) so a reader of the CSV alone cannot mistake one for the other - they
  # are not comparable numbers.
  if (!is.null(res$importance)) {
    imp <- res$importance
    out[[length(out) + 1]] <- blk(
      sprintf("Feature importance (%s)", imp$evaluated_on[1]),
      imp$predictor, "perm_delta_logloss",
      sprintf("share %.1f%%", imp$share_pct), imp$importance)
  }

  # Per class: the reference and predicted counts beside producer and user
  # accuracy. A class with no predictions carries that in its estimator, so the
  # NA is not read as a failed computation.
  pc <- res$per_class
  if (!is.null(pc) && nrow(pc) > 0) {
    none <- !is.na(pc$n_pred) & pc$n_pred == 0
    out[[length(out) + 1]] <- blk(
      "Per class",
      c(paste0(pc$class, ": producer accuracy (recall)"),
        paste0(pc$class, ": user accuracy (precision)"),
        paste0(pc$class, ": n (reference)"),
        paste0(pc$class, ": n (predicted)")),
      rep(c("producer_accuracy", "user_accuracy", "n_reference", "n_predicted"),
          each = nrow(pc)),
      c(ifelse(none, "no predictions", "Pooled out-of-fold"),
        ifelse(none, "no predictions", "Pooled out-of-fold"),
        rep("Pooled out-of-fold", 2 * nrow(pc))),
      c(pc$producer_accuracy, pc$user_accuracy, pc$n, pc$n_pred))
  }

  # Confusion matrix, one row per cell. The orientation is IN the metric string:
  # a reader of the file cannot see which margin is which otherwise.
  cm <- res$conf_mat
  tab <- if (is.null(cm)) NULL else tryCatch(as.table(cm$table), error = function(e) NULL)
  if (!is.null(tab)) {
    idx <- expand.grid(pred = rownames(tab), truth = colnames(tab),
                       stringsAsFactors = FALSE)
    out[[length(out) + 1]] <- blk(
      "Confusion matrix",
      sprintf("truth %s / predicted %s", idx$truth, idx$pred),
      "n", "Pooled out-of-fold",
      as.numeric(tab[cbind(idx$pred, idx$truth)]))
  }

  # Class areas of the mapped surface, keyed by class, as the Area panel shows
  # them (the confidence threshold applies, which is why they are passed in).
  if (!is.null(area) && nrow(area) > 0) {
    est <- area_note %||% "Mapped surface"
    out[[length(out) + 1]] <- blk(
      "Class area",
      c(paste0(area$class, ": area (ha)"), paste0(area$class, ": cells")),
      rep(c("area_ha", "n_cells"), each = nrow(area)),
      est, c(area$area_ha, area$n_cells))
  }

  out <- Filter(Negate(is.null), out)
  if (!length(out)) return(NULL)
  do.call(rbind, out)
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
                                        strict_scope = NULL,
                                        make_surface = TRUE, nn_surface = FALSE,
                                        seed = 12345L,
                                        group_col = NULL, boundary_wkt = NULL,
                                        class_weights = FALSE,
                                        model_rds_path = NULL,
                                        importance_reps = 5L,
                                        importance_mode = c("oof", "training"),
                                        nested = FALSE,
                                        vif_threshold = NULL,
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
  # Metric working CRS (see classif_project_metric). The module hands over the
  # CRS classif_resolve_scope already settled on, so this is normally a no-op;
  # it protects direct calls and keeps `boundary_wkt` interpretable (it is read
  # in `proj_crs`, then moved to the working CRS below).
  pts <- classif_project_metric(pts)
  work_crs <- sf::st_crs(pts)
  co <- sf::st_coordinates(pts); pts$x <- co[, 1]; pts$y <- co[, 2]
  # One row is one sampled location. Rows at exactly the same coordinates make
  # the covariate kriging system singular and put one location on both sides
  # of a CV split. The module merges co-located rows before it builds the
  # target (classif_resolve_scope).
  if (anyDuplicated(co[, 1:2, drop = FALSE])) {
    stop("The classification input holds more than one row at the same coordinates; ",
         "merge co-located rows first (classif_resolve_scope does).")
  }

  # The scope boundary the maps are clipped to, resolved before the
  # cross-validation: kNNDM matches its folds to the map's locations inside it
  # (knndm_domain_points), and the CV Distance Match compares any strategy's
  # folds with them. A boundary that cannot be resolved here leaves the grid to
  # resolve its own, as it always did, and kNNDM without a domain.
  bnd_sf <- if (is.null(boundary_wkt)) NULL else {
    sf::st_transform(sf::st_as_sfc(boundary_wkt, crs = proj_crs), work_crs)
  }
  scope_bnd <- tryCatch(classif_domain_boundary(pts, boundary, bnd_sf, buffer_mode, buffer_dist),
                        error = function(e) NULL)
  domain_xy <- if (!is.null(scope_bnd)) tryCatch(knndm_domain_points(scope_bnd), error = function(e) NULL)
  # The panel's random reference is this suite's random partition, the
  # Standard folds (class-stratified random k-fold at the run's fold count):
  # the partition kNNDM compares against and returns where random folds win.
  cv_design_of <- function(cv) {
    v_ref <- max(2L, min(as.integer(v), length(cv$base_y)))
    cv_distance_summary(cv$base_xy, cv$fold_id, domain_xy, units = crs_unit_label(pts),
                        random_folds = classif_stratified_folds(cv$base_y, v_ref, seed),
                        reference = sprintf("class-stratified random %d-fold", v_ref))
  }

  # Covariate-free run: the spatial 1-NN classifier is the model. No fitting,
  # tuning, importance, covariate kriging or model bundle - only its
  # out-of-fold metrics and, on request, its map on the usual grid.
  if (length(predictors) == 0) {
    .classif_check_cancel(cancel_file)
    report("cv", 0, "Cross-validating the spatial 1-NN classifier...")
    cv <- run_classification_nn_cv(pts, target, strategy = strategy, v = v,
                                   seed = seed, group = grp, domain_xy = domain_xy)
    report("cv", 1)
    out <- list(
      nn_only = TRUE,
      cv_metrics = as.data.frame(cv$metrics),
      per_class = cv$per_class,
      conf_mat = cv$conf_mat$table,
      best_params = NULL, nested = FALSE, nested_params = NULL,
      fold_id = cv$fold_id, n_folds = cv$n_folds,
      knndm = attr(cv$fold_id, "knndm"), cv_design = cv_design_of(cv),
      class_gaps = cv$class_gaps,
      method = "nn", strategy = strategy, depth = "none",
      n = length(cv$fold_id), levels = cv$levels, predictors = character(0),
      target_col = target,
      cv_predictions = cv$predictions,
      majority_acc = cv$majority_acc,
      weights_requested = FALSE, weights_applied = FALSE,
      group_metrics = if (is.null(grp)) NULL else classif_group_metrics(cv$predictions, target)
    )
    if (make_surface) {
      .classif_check_cancel(cancel_file)
      report("grid", 0, "Building the prediction grid...")
      gr <- classif_build_grid(pts, res = grid_res, boundary = boundary, boundary_sf = bnd_sf,
                               buffer_mode = buffer_mode, buffer_dist = buffer_dist,
                               strict_scope = strict_scope, scope = scope_bnd)
      report("surface", 0, sprintf("Assigning nearest-sample classes to %s grid cells...",
                                   format(nrow(gr$grid_p), big.mark = ",")))
      out$surface_df <- classif_nn_surface(cv$base_xy, cv$base_y,
                                           cbind(gr$grid_p$x, gr$grid_p$y), cv$levels)
      out$res <- gr$res
      out$crs_wkt <- gr$crs_wkt
      out$grid_warning <- gr$strict_warning
    }
    report("surface", 1, "Finishing...")
    .classif_check_cancel(cancel_file)
    return(out)
  }

  cv <- run_classification_cv(pts, target, predictors, method = method,
                              strategy = strategy, v = v, depth = depth, seed = seed,
                              group = grp, class_weights = class_weights,
                              nested = nested,
                              oof_importance = identical(importance_mode, "oof"),
                              importance_reps = importance_reps,
                              vif_threshold = vif_threshold,
                              cancel_file = cancel_file,
                              progress_cb = function(frac, label = NULL) report("cv", frac, label),
                              domain_xy = domain_xy)

  # The modelled level set, taken from the CV (complete cases, empty levels
  # dropped) rather than re-derived from every scoped point: the two disagree
  # whenever a class has no complete covariate row, and this vector names the
  # surface probability columns, the raster legend and the exported bundle.
  levs <- cv$levels
  out <- list(
    cv_metrics = as.data.frame(cv$metrics),
    per_class  = cv$per_class,
    conf_mat   = cv$conf_mat$table,
    best_params = if (is.null(cv$best_params)) NULL else as.data.frame(cv$best_params),
    nested = isTRUE(cv$nested),
    nested_params = cv$nested_params,
    fold_id = cv$fold_id, n_folds = cv$n_folds,
    # What a kNNDM request chose, and how closely the folds match the map.
    knndm = attr(cv$fold_id, "knndm"), cv_design = cv_design_of(cv),
    # Classes some fold could not learn because its analysis rows held none of
    # them; NULL in the ordinary case. A CV-design caveat the module reports.
    class_gaps = cv$class_gaps,
    method = method, strategy = strategy, depth = depth,
    n = length(cv$fold_id), levels = levs, predictors = predictors,
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
                                    class_weights = class_weights,
                                    cv_assessment_df = cv$assessment_df,
                                    cv_fold_id = cv$fold_id,
                                    vif_threshold = vif_threshold,
                                    domain_xy = domain_xy)
  report("fit", 1)
  # Auto-Drop: what the final model's screen removed on all rows, and what
  # each CV fold's own screen removed on its training rows.
  out$screened_out <- model$screened_out
  out$cv_fold_screen <- cv$fold_screen
  out$vif_threshold <- vif_threshold

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
    # model$predictors: the covariates the final model uses. One its Auto-Drop
    # screen removed is not in the model, so permuting it would report it as
    # unimportant rather than as absent.
    out$importance <- tryCatch(
      classif_permutation_importance(model, train_cc, target, model$predictors,
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
      predictors = model$predictors,
      # Provenance of the covariate list above: which covariates the Auto-Drop
      # screen removed and at which VIF threshold, so a bundle whose predictor
      # list is shorter than the run's selection explains itself. NULL when no
      # screen ran (Keep All).
      screened_out = model$screened_out,
      vif_threshold = out$vif_threshold,
      class_weights_applied = isTRUE(model$weights_applied),
      tuning_depth = depth,
      # The exported workflow's OWN tuned hyperparameters (full-data tuning in
      # fit_classification_model) — the CV loop's selection can differ and,
      # under nested CV, doesn't even exist as a single set.
      best_params  = if (is.null(model$best_params)) NULL else as.data.frame(model$best_params),
      # The CRS the run was actually computed in (the UTM fallback when the
      # requested target CRS was geographic), not the one that was asked for.
      proj_crs   = if (is.null(work_crs$wkt) || is.na(work_crs$wkt)) proj_crs else work_crs$wkt,
      n_train    = out$n,
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

  # The numeric covariates whose map surface is the IDW fallback.
  grid_fallback <- character(0)
  if (make_surface) {
    .classif_check_cancel(cancel_file)
    report("grid", 0, "Building the prediction grid...")
    gr <- classif_build_grid(pts, res = grid_res, boundary = boundary, boundary_sf = bnd_sf,
                             buffer_mode = buffer_mode, buffer_dist = buffer_dist,
                             strict_scope = strict_scope, scope = scope_bnd)
    report("grid", 1)
    n_cell_lab <- format(nrow(gr$grid_p), big.mark = ",")

    .classif_check_cancel(cancel_file)
    report("covariates", 0,
           sprintf("Interpolating covariates onto %s grid cells...", n_cell_lab))
    # model$predictors: a covariate the final model's screen removed is never
    # read by the model, so it is not kriged. The surfaces use the rows that
    # carry every covariate the model reads (a missing target is fine here, as
    # for RK): gstat refuses a missing value in the variable it kriges.
    cov_ok <- stats::complete.cases(
      sf::st_drop_geometry(pts)[, model$predictors, drop = FALSE])
    grid_aux <- build_classification_grid_aux(
      pts[cov_ok, ], gr$grid_p, model$predictors,
      cancel_file = cancel_file,
      progress = function(f) report("covariates", f))
    grid_fallback <- attr(grid_aux, "cov_fallback") %||% character(0)

    report("surface", 0, sprintf("Classifying %s grid cells...", n_cell_lab))
    surf <- predict_classification_surface(
      model, sf::st_drop_geometry(grid_aux),
      cancel_file = cancel_file,
      progress = function(f) report("surface", f))

    out$surface_df <- surf
    if (isTRUE(nn_surface)) {
      out$surface_nn <- classif_nn_surface(cv$base_xy, cv$base_y,
                                           cbind(gr$grid_p$x, gr$grid_p$y), levs)
    }
    out$res <- gr$res
    out$crs_wkt <- gr$crs_wkt
    # Advisory carried back to the main session (the module raises it as a
    # notification): a strict boundary too narrow for the cell size leaves
    # isolated samples with no mapped cell.
    out$grid_warning <- gr$strict_warning
    # No area table here: the MAIN session rasterises surface_df through
    # classif_surface_to_rasters(), whose table is the one the UI and the
    # exports read (it alone knows the live confidence threshold and its
    # "Unclassified" row). A second, always-tau-0 table shipped across the
    # future boundary was dead payload and an invitation to report the wrong
    # numbers.
  }
  # The covariate surfaces that came from the IDW fallback instead of kriging,
  # in the CV folds and on the prediction grid: the module reports them
  # (classif_covariate_notes). NULL when every surface was kriged.
  if (!is.null(cv$cov_fallback) || length(grid_fallback)) {
    out$covariate_fallback <- list(folds = cv$cov_fallback, grid = grid_fallback,
                                   n_folds = cv$n_folds)
  }
  report("surface", 1, "Finishing...")
  # A cancel requested during the last unguarded moments still counts: the module
  # has already told the user the run is being cancelled, so a completed result
  # must never arrive behind their back.
  .classif_check_cancel(cancel_file)
  out
}
