# Tests for the Classification Suite engine (classif_helpers.R).

test_that("method and tuning-depth registries expose the expected ids", {
  expect_setequal(names(classif_methods()), c("multinom", "rf", "xgboost"))
  expect_setequal(names(classif_tuning_depths()), c("none", "light", "full"))
})

test_that("recipe imputes, dummy-encodes categoricals and drops the geometry", {
  pts <- make_classif_points(n = 40)
  df <- sf::st_drop_geometry(pts)
  rec <- classif_build_recipe(df, "soil", c("elev", "slope", "parent"))
  baked <- recipes::bake(recipes::prep(rec), new_data = NULL)

  # parent (2 levels) -> one dummy column; numerics retained; target kept.
  expect_true("parent_Shale" %in% names(baked))
  expect_false("parent" %in% names(baked))
  expect_true(all(c("elev", "slope", "soil") %in% names(baked)))
  expect_false(any(is.na(baked)))
})

test_that("build_spec marks tuned args as tune() and leaves the rest fixed", {
  fixed <- classif_build_spec("rf", tune_params = character(0))
  tuned <- classif_build_spec("rf", tune_params = classif_tuning_params("rf", "light"))

  # A depth with tuning must expose tunable parameters; 'none' must not.
  expect_length(hardhat::extract_parameter_set_dials(fixed)$id, 0)
  expect_setequal(hardhat::extract_parameter_set_dials(tuned)$id, c("mtry", "min_n"))
  expect_length(classif_tuning_params("rf", "none"), 0)
})

test_that("spatial and standard fold ids are reproducible and cover every row", {
  pts <- make_classif_points(n = 50)
  f1 <- classif_make_fold_id(pts, "spatial", target = "soil", v = 5, seed = 12345)
  f2 <- classif_make_fold_id(pts, "spatial", target = "soil", v = 5, seed = 12345)
  expect_identical(f1, f2)
  expect_false(anyNA(f1))
  expect_equal(length(f1), nrow(pts))

  s1 <- classif_make_fold_id(pts, "standard", target = "soil", v = 5, seed = 12345)
  s2 <- classif_make_fold_id(pts, "standard", target = "soil", v = 5, seed = 12345)
  expect_identical(s1, s2)
  expect_true(all(s1 >= 1))
})

test_that("fold construction preserves the caller's RNG stream (seed sandbox)", {
  pts <- make_classif_points(n = 40)
  set.seed(999)
  before <- .Random.seed
  invisible(classif_make_fold_id(pts, "standard", target = "soil", v = 5, seed = 12345))
  expect_identical(before, .Random.seed)
})

test_that("normalised Shannon entropy hits its analytic bounds", {
  # Uniform over K classes -> 1; a certain class -> 0.
  expect_equal(classif_shannon_entropy(matrix(c(0.5, 0.5), nrow = 1)), 1)
  expect_equal(classif_shannon_entropy(matrix(rep(1/3, 3), nrow = 1)), 1)
  expect_equal(classif_shannon_entropy(matrix(c(1, 0, 0), nrow = 1)), 0)
  # Monotonic: a peaked distribution is less uncertain than a flatter one.
  peaked <- classif_shannon_entropy(matrix(c(0.8, 0.1, 0.1), nrow = 1))
  flat   <- classif_shannon_entropy(matrix(c(0.4, 0.35, 0.25), nrow = 1))
  expect_lt(peaked, flat)
})

test_that("vectorised entropy is bit-identical to the per-row reference", {
  # Guards the apply() -> rowSums() rewrite, including the p == 0 terms the old
  # code dropped via p[p > 0] and the new code zeroes.
  set.seed(11)
  m <- matrix(runif(300), ncol = 3)
  m <- m / rowSums(m)
  m[1, ] <- c(1, 0, 0)
  m[2, ] <- c(0.5, 0.5, 0)
  reference <- apply(m, 1, function(p) {
    p <- p[p > 0]
    -sum(p * log(p))
  }) / log(ncol(m))
  expect_identical(classif_shannon_entropy(m), as.numeric(reference))
})

test_that("run_classification_cv produces valid pooled metrics and confusion matrix", {
  pts <- make_classif_points(n = 60)
  cv <- run_classification_cv(pts, "soil", c("elev", "slope", "parent"),
                              method = "rf", strategy = "spatial", v = 5, depth = "none")

  acc <- cv$metrics$.estimate[cv$metrics$.metric == "accuracy"]
  expect_true(acc >= 0 && acc <= 1)
  # A signal-bearing target should beat the ~1/3 no-information rate.
  expect_gt(acc, 0.4)

  expect_setequal(cv$per_class$class, c("Low", "Med", "High"))
  expect_equal(sum(cv$conf_mat$table), nrow(pts))

  # Every pooled row's class probabilities sum to 1.
  levs <- levels(sf::st_drop_geometry(pts)$soil)
  prob_cols <- paste0(".pred_", levs)
  psum <- rowSums(cv$predictions[, prob_cols])
  expect_true(all(abs(psum - 1) < 1e-6))
})

test_that("a fold that never sees a class still pools its predictions", {
  pts <- make_classif_points(n = 60)
  # A singleton class is capped to v = 2 by classif_make_fold_id and lands in
  # exactly ONE fold, so that fold's analysis rows hold none of it. ranger (and
  # nnet::multinom) then drop the unused outcome level and return one fewer
  # .pred_ column, which used to abort the pooled rbind with "numbers of columns
  # of arguments do not match".
  soil <- as.character(sf::st_drop_geometry(pts)$soil)
  soil[1] <- "Rare"
  pts$soil <- factor(soil, levels = c("Low", "Med", "High", "Rare"))

  cv <- suppressWarnings(run_classification_cv(
    pts, "soil", c("elev", "slope"), method = "rf", strategy = "standard",
    v = 5, depth = "none", oof_importance = TRUE, importance_reps = 2L))

  prob_cols <- paste0(".pred_", c("Low", "Med", "High", "Rare"))
  expect_true(all(prob_cols %in% names(cv$predictions)))
  expect_equal(nrow(cv$predictions), nrow(pts))
  expect_true(all(abs(rowSums(cv$predictions[, prob_cols]) - 1) < 1e-6))

  # The gap is reported by fold and class rather than silently absorbed.
  expect_false(is.null(cv$class_gaps))
  expect_true("Rare" %in% cv$class_gaps$class)
  # A fold with no Rare training rows assigns Rare exactly zero mass — the
  # model's genuine posterior, not an imputed share.
  gap_fold <- cv$class_gaps$fold[cv$class_gaps$class == "Rare"][1]
  expect_true(all(cv$predictions$.pred_Rare[cv$predictions$.fold == gap_fold] == 0))

  # Out-of-fold importance survives the gap: matching truth against the missing
  # probability column used to turn every importance value NA.
  expect_false(is.null(cv$importance))
  expect_true(all(is.finite(cv$importance$importance)))
})

test_that("target levels with no samples are dropped before modelling", {
  pts <- make_classif_points(n = 60)
  # classif_build_target() carries every bin label as a level, so an equal-
  # interval or Jenks break enclosing no samples reaches the engine as an empty
  # class — as does a class whose every row lacks a covariate value.
  pts$soil <- factor(as.character(sf::st_drop_geometry(pts)$soil),
                     levels = c("Low", "Med", "High", "Absent"))

  cv <- run_classification_cv(pts, "soil", c("elev", "slope"),
                              method = "rf", strategy = "standard",
                              v = 4, depth = "none")

  expect_equal(cv$levels, c("Low", "Med", "High"))
  expect_false(".pred_Absent" %in% names(cv$predictions))
  expect_false("Absent" %in% cv$per_class$class)
  expect_equal(sum(cv$conf_mat$table), nrow(pts))
  # An empty level leaves yardstick's macro recall undefined, which propagated
  # into a silently NA balanced accuracy.
  ba <- cv$metrics$.estimate[cv$metrics$.metric == "bal_accuracy"]
  expect_length(ba, 1)
  expect_false(is.na(ba))
  # The empty level must not collapse the stratified fold cap (min class = 0).
  expect_gt(cv$n_folds, 2)
})

test_that("all three learners fit and cross-validate", {
  pts <- make_classif_points(n = 60)
  for (m in c("multinom", "rf", "xgboost")) {
    cv <- run_classification_cv(pts, "soil", c("elev", "slope", "parent"),
                                method = m, strategy = "standard", v = 4, depth = "none")
    expect_true("accuracy" %in% cv$metrics$.metric)
    expect_equal(sum(cv$conf_mat$table), nrow(pts))
  }
})

test_that("light tuning selects hyperparameters and still cross-validates", {
  pts <- make_classif_points(n = 60)
  cv <- run_classification_cv(pts, "soil", c("elev", "slope"),
                              method = "rf", strategy = "standard", v = 4, depth = "light")
  expect_false(is.null(cv$best_params))
  expect_true(all(c("mtry", "min_n") %in% names(cv$best_params)))
})

test_that("final fit predicts a valid class/probability/entropy surface", {
  pts <- make_classif_points(n = 60)
  mod <- fit_classification_model(pts, "soil", c("elev", "slope", "parent"),
                                  method = "rf", depth = "none")
  nd <- data.frame(elev = seq(0, 30, length.out = 20), slope = 7,
                   parent = factor("Granite", levels = c("Granite", "Shale")))
  surf <- predict_classification_surface(mod, nd)

  expect_true(all(as.character(surf$.pred_class) %in% mod$levels))
  psum <- rowSums(surf[, paste0(".pred_", mod$levels)])
  expect_true(all(abs(psum - 1) < 1e-6))
  expect_true(all(surf$.entropy >= 0 & surf$.entropy <= 1))
})

test_that("blocked surface prediction is identical to a single-block call", {
  # The surface stage predicts in row blocks so it can tick progress and poll the
  # cancel flag. Every recipe step is trained, so baking is row-independent and
  # the blocked result must match the whole-grid result exactly.
  pts <- make_classif_points(n = 60)
  mod <- fit_classification_model(pts, "soil", c("elev", "slope", "parent"),
                                  method = "rf", depth = "none")
  nd <- data.frame(elev = seq(0, 30, length.out = 50), slope = 7,
                   parent = factor("Granite", levels = c("Granite", "Shale")))

  expect_identical(predict_classification_surface(mod, nd, chunk_size = nrow(nd)),
                   predict_classification_surface(mod, nd, chunk_size = 7))

  seen <- numeric(0)
  invisible(predict_classification_surface(
    mod, nd, chunk_size = 10, progress = function(f) seen <<- c(seen, f)))
  expect_equal(seen, seq_len(5) / 5)
})

test_that("the argmax class agrees with a direct type = 'class' prediction", {
  # The surface stage derives the hard class from the probabilities it already
  # has instead of running a second full predict() pass over the grid. Every
  # shipped learner is a probability model, so the two must agree (bar exact
  # ties, which the argmax resolves to the first trained level).
  pts <- make_classif_points(n = 60)
  nd <- data.frame(elev = seq(0, 30, length.out = 60), slope = c(4, 7, 11),
                   parent = factor(c("Granite", "Shale"),
                                   levels = c("Granite", "Shale")))
  for (m in c("rf", "multinom", "xgboost")) {
    mod <- suppressWarnings(fit_classification_model(
      pts, "soil", c("elev", "slope", "parent"), method = m, depth = "none"))
    surf <- predict_classification_surface(mod, nd, chunk_size = 13)
    direct <- predict(mod$workflow, nd, type = "class")$.pred_class
    expect_identical(surf$.pred_class, direct, info = m)
  }
})

test_that("a cancel flag stops the surface stage instead of running to completion", {
  # The regression this pins: cancellation used to be checked only BEFORE the
  # surface stage, so a cancel pressed while the grid was being classified (the
  # longest part of a run) was never seen and the run completed anyway.
  pts <- make_classif_points(n = 40)
  mod <- fit_classification_model(pts, "soil", c("elev", "slope"), method = "rf")
  nd <- data.frame(elev = seq(0, 30, length.out = 40), slope = 7)

  flag <- tempfile(fileext = ".txt")
  file.create(flag)
  on.exit(unlink(flag), add = TRUE)
  expect_error(
    predict_classification_surface(mod, nd, chunk_size = 5, cancel_file = flag),
    "cancelled by user")
  # No flag: unchanged behaviour.
  expect_s3_class(predict_classification_surface(mod, nd, chunk_size = 5),
                  "data.frame")
})

test_that("the progress ladder is monotone and gives the map stages real room", {
  dir <- tempfile(); dir.create(dir)
  old_dir <- getOption("monolith_progress_dir")
  old_sid <- getOption("monolith_session_id")
  options(monolith_progress_dir = dir, monolith_session_id = "cls_ladder")
  on.exit({
    options(monolith_progress_dir = old_dir, monolith_session_id = old_sid)
    unlink(dir, recursive = TRUE)
  }, add = TRUE)

  report <- .classif_progress_reporter(dir, "cls_ladder", make_surface = TRUE)
  pct_file <- file.path(dir, "progress_cls_ladder_classification_cls.txt")
  stage_file <- file.path(dir, "stage_cls_ladder_classification_cls.txt")

  steps <- list(
    function() report("cv", 0, "Cross-validation: fold 1 of 10"),
    function() report("cv", 1),
    function() report("fit", 1),
    function() report("importance", 1),
    function() report("grid", 1),
    function() report("covariates", 1),
    function() report("surface", 1, "Finishing...")
  )
  pcts <- vapply(steps, function(f) {
    f()
    as.numeric(readLines(pct_file, warn = FALSE))[1]
  }, numeric(1))

  expect_equal(pcts, c(0, 50, 60, 66, 70, 88, 99))
  expect_false(is.unsorted(pcts))
  # The caption travels with the bar, so the UI stops claiming
  # "cross-validation" while the grid is being classified.
  expect_equal(readLines(stage_file, warn = FALSE)[1], "Finishing...")
  # Regression: under the old folds+2 denominator everything from the end of CV
  # onward collapsed into ~92%-100%. The map-building stages now own ~40 points.
  expect_gt(pcts[7] - pcts[3], 35)
})

test_that("krige_covariates reports each covariate and propagates a cancel", {
  pts <- make_test_points(n = 25)
  grid <- make_test_grid_safe(pts, res = 250)
  lags <- calc_scientific_lags(pts)
  mp <- list(idw_p = 2, idw_nmax = 12)

  seen <- list()
  kc <- krige_covariates(pts, grid, c("aux1", "aux2"), lags, mp,
                         on_var = function(i, total) {
                           seen[[length(seen) + 1]] <<- c(i, total)
                         })
  expect_equal(seen, list(c(1, 2), c(2, 2)))
  expect_true(all(c("aux1", "aux2") %in% names(kc$grid_aux)))

  # A hook that raises (the classification cancel path) aborts the loop.
  expect_error(
    krige_covariates(pts, grid, c("aux1", "aux2"), lags, mp,
                     on_var = function(i, total) {
                       stop("Classification run cancelled by user.")
                     }),
    "cancelled by user")
})

test_that("surface rasterisation yields aligned class/prob/entropy layers and hectare areas", {
  pts <- make_classif_points(n = 50)
  mod <- fit_classification_model(pts, "soil", c("elev", "slope"), method = "rf")
  grid <- expand.grid(x = seq(450000, 452000, by = 200),
                      y = seq(5800000, 5802000, by = 200))
  grid$elev <- (grid$x - 450000) / 100
  grid$slope <- 7
  surf <- predict_classification_surface(mod, grid)
  surf$x <- grid$x; surf$y <- grid$y

  rl <- classif_surface_to_rasters(surf, res = 200, crs_wkt = sf::st_crs(32633)$wkt,
                                   levels_order = mod$levels)
  expect_equal(names(rl$class), "class")
  expect_setequal(names(rl$prob), paste0("P_", mod$levels))
  expect_equal(names(rl$entropy), "entropy")
  expect_true(terra::is.factor(rl$class))
  # Exact area accounting: sum of class areas == cells x cell area (ha).
  expect_equal(sum(rl$area$area_ha), sum(rl$area$n_cells) * (200 * 200 / 10000))
})

test_that("every emitted metric id and estimator has a display label", {
  set.seed(42)
  n <- 90
  levs <- c("A", "B", "C")
  p <- matrix(runif(n * 3), ncol = 3); p <- p / rowSums(p)
  pred_df <- data.frame(
    truth = factor(sample(levs, n, replace = TRUE), levels = levs),
    .pred_class = factor(sample(levs, n, replace = TRUE), levels = levs),
    .pred_A = p[, 1], .pred_B = p[, 2], .pred_C = p[, 3]
  )
  m <- classif_compute_metrics(pred_df, "truth")
  # Probability metrics must be present so their labels are exercised too.
  expect_true(all(c("roc_auc", "mn_log_loss", "brier_class") %in% m$.metric))

  lab <- classif_label_metrics(m)
  expect_true(all(lab$.metric_label %in% unname(classif_metric_labels())))
  expect_true(all(lab$.estimator_label %in% unname(classif_estimator_labels())))

  # Unknown ids degrade to the raw id instead of NA.
  fake <- data.frame(.metric = "new_metric", .estimator = "new_est", .estimate = 1)
  expect_equal(classif_label_metrics(fake)$.metric_label, "new_metric")
  expect_equal(classif_label_metrics(fake)$.estimator_label, "new_est")
})

test_that("class raster embeds a colour table that survives GeoTIFF round-trip", {
  grid <- expand.grid(x = seq(450000, 450800, by = 200),
                      y = seq(5800000, 5800800, by = 200))
  levs <- c("Low", "Med", "High")
  grid$.pred_class <- factor(rep(levs, length.out = nrow(grid)), levels = levs)
  for (l in levs) grid[[paste0(".pred_", l)]] <- 1 / 3
  grid$.entropy <- 1

  rl <- classif_surface_to_rasters(grid, res = 200, crs_wkt = sf::st_crs(32633)$wkt,
                                   levels_order = levs)
  expect_true(terra::has.colors(rl$class))
  ct <- terra::coltab(rl$class)[[1]]
  expect_equal(sum(ct$value %in% seq_along(levs)), length(levs))

  tf <- tempfile(fileext = ".tif")
  terra::writeRaster(rl$class, tf, overwrite = TRUE, datatype = "INT1U")
  r2 <- terra::rast(tf)
  expect_true(terra::has.colors(r2))
  expect_equal(as.integer(terra::values(r2, mat = FALSE)),
               as.integer(terra::values(rl$class, mat = FALSE)))
  unlink(c(tf, paste0(tf, ".aux.xml")))
})

test_that("run_classification_pipeline returns only serialisable pieces and honours make_surface", {
  set.seed(11)
  n <- 55
  df <- data.frame(
    lon = runif(n, 32.0, 32.05), lat = runif(n, 39.0, 39.05)
  )
  score <- (df$lon - 32) * 40 + (df$lat - 39) * 40 + rnorm(n, 0, 0.3)
  df$soil <- cut(score, breaks = stats::quantile(score, c(0, .34, .67, 1)),
                 labels = c("A", "B", "C"), include.lowest = TRUE)
  df$elev <- score * 10 + rnorm(n, 0, 2)
  df$slope <- runif(n, 0, 15)

  res <- run_classification_pipeline(
    df, target = "soil", predictors = c("elev", "slope"),
    x_col = "lon", y_col = "lat", src_crs = 4326, proj_crs = "EPSG:32636",
    method = "rf", strategy = "spatial", depth = "none",
    v = 5, grid_res = 250, boundary = "concave", make_surface = TRUE
  )

  expect_false(is.null(res$surface_df))
  expect_true(is.data.frame(res$surface_df))
  # No terra pointers may leak into the (serialisable) result.
  expect_false(any(vapply(res, function(x) inherits(x, "SpatRaster"), logical(1))))
  expect_setequal(res$levels, c("A", "B", "C"))
  # Grid metadata the main session needs to rasterise surface_df. The worker
  # deliberately ships NO area table: area accounting belongs to
  # classif_surface_to_rasters(), which alone knows the confidence threshold.
  expect_true(is.numeric(res$res) && res$res > 0)
  expect_true(is.character(res$crs_wkt) && nzchar(res$crs_wkt))
  expect_null(res$area)

  res_eval <- run_classification_pipeline(
    df, target = "soil", predictors = c("elev", "slope"),
    x_col = "lon", y_col = "lat", src_crs = 4326, proj_crs = "EPSG:32636",
    method = "rf", strategy = "standard", depth = "none", v = 5, make_surface = FALSE
  )
  expect_null(res_eval$surface_df)
})

test_that("numeric covariates are never auto-classified as categorical", {
  # Coarse-raster covariates (e.g. precipitation metrics) can carry very few
  # distinct values; they must still be treated as continuous predictors.
  expect_false(.classif_is_categorical(rep(c(10, 11, 12), 50)))
  expect_false(.classif_is_categorical(c(38:43, NA)))
  expect_true(.classif_is_categorical(c("clay", "loam")))
  expect_true(.classif_is_categorical(factor(c("a", "b"))))
})

test_that("classif_resolve_scope filters by locality and bounds per-locality hulls", {
  d <- make_classif_scope_df()

  sc <- classif_resolve_scope(d, "x", "y", 32633, "EPSG:32633",
                              loc_col = "loc", localities = "A")
  expect_equal(sc$n_scoped, sum(d$loc == "A"))
  expect_equal(sc$n_input, nrow(d))
  expect_true(all(sc$group == "A"))

  bnd <- sf::st_as_sfc(sc$boundary_wkt, crs = 32633)
  pts_a <- sf::st_as_sf(d[d$loc == "A", ], coords = c("x", "y"), crs = 32633)
  pts_b <- sf::st_as_sf(d[d$loc == "B", ], coords = c("x", "y"), crs = 32633)
  expect_true(all(lengths(sf::st_intersects(pts_a, bnd)) > 0))
  expect_false(any(lengths(sf::st_intersects(pts_b, bnd)) > 0))

  # ALL keeps everything, but the boundary is the union of per-locality hulls:
  # the unsampled gap between the two localities stays outside the domain.
  sc_all <- classif_resolve_scope(d, "x", "y", 32633, "EPSG:32633",
                                  loc_col = "loc", localities = "ALL")
  expect_equal(sc_all$n_scoped, nrow(d))
  expect_setequal(unique(sc_all$group), c("A", "B"))
  bnd_all <- sf::st_as_sfc(sc_all$boundary_wkt, crs = 32633)
  gap_pt <- sf::st_sfc(sf::st_point(c(454500, 5800500)), crs = 32633)
  expect_false(any(lengths(sf::st_intersects(gap_pt, bnd_all)) > 0))
})

test_that("polygon scope modes restrict points, groups, and boundary", {
  d <- make_classif_scope_df()
  # Square fully covering locality A, far from locality B.
  poly <- sf::st_sf(
    label = "Zone 1",
    geometry = sf::st_sfc(sf::st_polygon(list(rbind(
      c(449900, 5799900), c(451100, 5799900), c(451100, 5801100),
      c(449900, 5801100), c(449900, 5799900)))), crs = 32633))

  # Polygons-only ignores the locality filter entirely.
  sc_only <- classif_resolve_scope(d, "x", "y", 32633, "EPSG:32633",
                                   loc_col = "loc", localities = "B",
                                   poly_sf = poly, poly_mode = "only")
  expect_equal(sc_only$n_scoped, sum(d$loc == "A"))
  expect_true(all(sc_only$group == "Zone 1"))
  bnd <- sf::st_as_sfc(sc_only$boundary_wkt, crs = 32633)
  expect_equal(as.numeric(sf::st_area(bnd)),
               as.numeric(sf::st_area(poly)), tolerance = 1e-6)

  # Intersect: locality B has no points inside the polygon -> empty scope.
  sc_none <- classif_resolve_scope(d, "x", "y", 32633, "EPSG:32633",
                                   loc_col = "loc", localities = "B",
                                   poly_sf = poly, poly_mode = "intersect")
  expect_equal(sc_none$n_scoped, 0L)
  expect_null(sc_none$boundary_wkt)

  # Intersect with both localities selected keeps only the points in A, and
  # the boundary stays inside the polygon.
  sc_int <- classif_resolve_scope(d, "x", "y", 32633, "EPSG:32633",
                                  loc_col = "loc", localities = c("A", "B"),
                                  poly_sf = poly, poly_mode = "intersect")
  expect_equal(sc_int$n_scoped, sum(d$loc == "A"))
  expect_true(all(sc_int$group == "A"))
  bnd_int <- sf::st_as_sfc(sc_int$boundary_wkt, crs = 32633)
  expect_true(as.numeric(sf::st_area(bnd_int)) <= as.numeric(sf::st_area(poly)) + 1e-6)
})

test_that("classif_scope_polygons merges drawn and uploaded polygons with labels", {
  drawn <- sf::st_sf(geometry = sf::st_sfc(sf::st_polygon(list(rbind(
    c(15, 52), c(15.01, 52), c(15.01, 52.01), c(15, 52.01), c(15, 52)))),
    crs = 4326))
  shp <- sf::st_sf(
    zone_name = c("North", "South"),
    geometry = sf::st_sfc(
      sf::st_polygon(list(rbind(c(450000, 5800000), c(451000, 5800000),
                                c(451000, 5801000), c(450000, 5801000),
                                c(450000, 5800000)))),
      sf::st_polygon(list(rbind(c(452000, 5800000), c(453000, 5800000),
                                c(453000, 5801000), c(452000, 5801000),
                                c(452000, 5800000)))),
      crs = 32633))

  out <- classif_scope_polygons(drawn, shp, target_crs = "EPSG:32633")
  expect_s3_class(out, "sf")
  expect_equal(nrow(out), 3)
  # Drawn shapes get sequence labels; shapefile features keep their attribute.
  expect_setequal(out$label, c("Drawn 1", "North", "South"))
  expect_equal(sf::st_crs(out), sf::st_crs("EPSG:32633"))

  # Point-geometry uploads degrade to their convex hull, as in interpolation.
  pts <- sf::st_sf(geometry = sf::st_sfc(
    sf::st_point(c(450000, 5800000)), sf::st_point(c(451000, 5800000)),
    sf::st_point(c(450500, 5801000)), crs = 32633))
  hull <- classif_scope_polygons(NULL, pts, target_crs = "EPSG:32633")
  expect_equal(hull$label, "Uploaded boundary")
  expect_true(all(as.character(sf::st_geometry_type(hull)) %in%
                    c("POLYGON", "MULTIPOLYGON")))

  expect_null(classif_scope_polygons(NULL, NULL))
})

test_that("classif_group_metrics reports per-area rows plus a consistent Total", {
  levs <- c("A", "B")
  pred_df <- data.frame(
    truth = factor(c("A", "A", "A", "A", "A", "A", "B", "B"), levels = levs),
    .pred_class = factor(c("A", "A", "A", "B", "B", "B", "B", "B"), levels = levs),
    .scope_group = c(rep("g1", 4), rep("g2", 4))
  )
  gm <- classif_group_metrics(pred_df, "truth")
  expect_setequal(gm$scope, c("g1", "g2", "Total"))
  expect_equal(gm$n[gm$scope == "g1"], 4)
  # Hand-computed: g1 = 3/4 correct, g2 = 2/4 correct, Total = 5/8.
  expect_equal(gm$accuracy[gm$scope == "g1"], 0.75)
  expect_equal(gm$accuracy[gm$scope == "g2"], 0.5)
  expect_equal(gm$accuracy[gm$scope == "Total"], 5 / 8)

  # Without a group column only the Total row is produced.
  gm_tot <- classif_group_metrics(pred_df[, c("truth", ".pred_class")], "truth")
  expect_equal(gm_tot$scope, "Total")
  expect_equal(gm_tot$accuracy, 5 / 8)
})

test_that("pipeline threads scope groups and confines the surface to the boundary", {
  d <- make_classif_scope_df(nA = 35, nB = 30)
  sc <- classif_resolve_scope(d, "x", "y", 32633, "EPSG:32633", loc_col = "loc")
  adf <- sc$df
  adf$.scope_group <- sc$group

  res <- run_classification_pipeline(
    adf, target = "soil", predictors = c("elev", "slope"),
    x_col = "x", y_col = "y", src_crs = 32633, proj_crs = "EPSG:32633",
    method = "rf", strategy = "standard", depth = "none", v = 4,
    grid_res = 100, make_surface = TRUE,
    group_col = ".scope_group", boundary_wkt = sc$boundary_wkt
  )

  expect_setequal(res$group_metrics$scope, c("A", "B", "Total"))
  expect_equal(res$group_metrics$n[res$group_metrics$scope == "Total"], nrow(adf))
  # The Total row must reproduce the pooled headline accuracy.
  acc_pooled <- res$cv_metrics$.estimate[res$cv_metrics$.metric == "accuracy"]
  expect_equal(res$group_metrics$accuracy[res$group_metrics$scope == "Total"],
               acc_pooled)

  # Every predicted grid cell sits inside the per-locality hull union: the
  # unsampled corridor between the localities is never predicted.
  bnd <- sf::st_as_sfc(sc$boundary_wkt, crs = "EPSG:32633")
  spts <- sf::st_as_sf(res$surface_df, coords = c("x", "y"), crs = "EPSG:32633")
  expect_true(all(lengths(sf::st_intersects(spts, bnd)) > 0))
  gap <- sf::st_sfc(sf::st_point(c(454500, 5800500)), crs = "EPSG:32633")
  expect_false(any(lengths(sf::st_intersects(gap, bnd)) > 0))
})

# ── Shared boundary styles (wrapped / strict buffers) ────────────────────────

test_that("wrapped and strict boundary styles buffer the domain like the sidebar", {
  pts <- make_classif_points(n = 50)

  h_conc <- .classif_scope_hulls(pts, style = "concave")
  h_wrap <- .classif_scope_hulls(pts, style = "wrapped",
                                 buffer_mode = "fixed", buffer_dist = 100)
  h_dyn  <- .classif_scope_hulls(pts, style = "wrapped",
                                 buffer_mode = "dynamic", buffer_dist = 100)
  h_str  <- .classif_scope_hulls(pts, style = "strict",
                                 buffer_mode = "fixed", buffer_dist = 50)

  a_conc <- as.numeric(sf::st_area(h_conc))
  expect_gt(as.numeric(sf::st_area(h_wrap)), a_conc)
  expect_gt(as.numeric(sf::st_area(h_dyn)), a_conc)
  # Strict = union of 50 m point buffers: bounded by n x pi x r^2 and far
  # smaller than the wrapped hull; must still contain every sample.
  a_str <- as.numeric(sf::st_area(h_str))
  expect_lte(a_str, nrow(pts) * pi * 50^2 + 1)
  for (h in list(h_wrap, h_dyn, h_str)) {
    expect_true(all(lengths(sf::st_intersects(pts, h)) > 0))
  }

  # The fixed wrapped buffer pushes the boundary out by ~100 m: a point 50 m
  # outside the concave hull's edge is inside the wrapped domain.
  edge <- sf::st_cast(sf::st_boundary(h_conc), "POINT")[1]
  ctr <- sf::st_centroid(h_conc)
  dir_v <- (sf::st_coordinates(edge) - sf::st_coordinates(ctr))
  dir_v <- dir_v / sqrt(sum(dir_v^2))
  outside <- sf::st_sfc(sf::st_point(as.numeric(sf::st_coordinates(edge) + 50 * dir_v)),
                        crs = sf::st_crs(pts))
  expect_false(any(lengths(sf::st_intersects(outside, h_conc)) > 0))
  expect_true(any(lengths(sf::st_intersects(outside, h_wrap)) > 0))
})

test_that("classif_build_grid honours the wrapped boundary and resolve_scope threads buffers", {
  pts <- make_classif_points(n = 50)
  g_conc <- classif_build_grid(pts, res = 100, boundary = "concave")
  g_wrap <- classif_build_grid(pts, res = 100, boundary = "wrapped",
                               buffer_mode = "fixed", buffer_dist = 200)
  expect_gt(nrow(g_wrap$grid_p), nrow(g_conc$grid_p))

  d <- make_classif_scope_df()
  sc_conc <- classif_resolve_scope(d, "x", "y", 32633, "EPSG:32633",
                                   loc_col = "loc", localities = "A")
  sc_wrap <- classif_resolve_scope(d, "x", "y", 32633, "EPSG:32633",
                                   loc_col = "loc", localities = "A",
                                   boundary_style = "wrapped",
                                   buffer_mode = "fixed", buffer_dist = 200)
  a1 <- as.numeric(sf::st_area(sf::st_as_sfc(sc_conc$boundary_wkt, crs = 32633)))
  a2 <- as.numeric(sf::st_area(sf::st_as_sfc(sc_wrap$boundary_wkt, crs = 32633)))
  expect_gt(a2, a1)
})

# ── Class-imbalance weights ──────────────────────────────────────────────────

test_that("inverse-frequency class weights follow the balanced heuristic", {
  y <- factor(c(rep("a", 8), rep("b", 2)))
  w <- .classif_class_weights(y)
  # w_c = n / (k * n_c): a -> 10/(2*8) = 0.625, b -> 10/(2*2) = 2.5; mean 1.
  expect_equal(unique(w[y == "a"]), 0.625)
  expect_equal(unique(w[y == "b"]), 2.5)
  expect_equal(mean(w), 1)
})

test_that("class weights apply for supported engines and change the fit", {
  expect_true(classif_supports_weights("rf"))
  expect_true(classif_supports_weights("xgboost"))
  expect_false(classif_supports_weights("multinom"))

  # Imbalanced two-class fixture (~80/20).
  set.seed(3)
  n <- 70
  x <- runif(n, 450000, 452000); y <- runif(n, 5800000, 5802000)
  score <- (x - 450000) / 2000 + rnorm(n, 0, 0.2)
  df <- data.frame(
    x = x, y = y,
    soil = factor(ifelse(score > stats::quantile(score, 0.8), "Rare", "Common")),
    elev = score * 10 + rnorm(n, 0, 1),
    slope = runif(n, 0, 15)
  )
  pts <- sf::st_as_sf(df, coords = c("x", "y"), crs = 32633)

  cv_uw <- run_classification_cv(pts, "soil", c("elev", "slope"), method = "rf",
                                 strategy = "standard", v = 4, class_weights = FALSE)
  cv_w  <- run_classification_cv(pts, "soil", c("elev", "slope"), method = "rf",
                                 strategy = "standard", v = 4, class_weights = TRUE)
  expect_false(isTRUE(cv_uw$weights_applied))
  expect_true(isTRUE(cv_w$weights_applied))
  # Same seed, same folds: any difference must come from the weighting.
  expect_identical(cv_uw$fold_id, cv_w$fold_id)
  expect_false(isTRUE(all.equal(cv_uw$predictions$.pred_Rare,
                                cv_w$predictions$.pred_Rare)))

  # Unsupported engine degrades to an unweighted fit and says so.
  cv_mn <- run_classification_cv(pts, "soil", c("elev", "slope"), method = "multinom",
                                 strategy = "standard", v = 4, class_weights = TRUE)
  expect_false(isTRUE(cv_mn$weights_applied))
})

test_that("binary multinom substitutes logistic regression with valid probabilities", {
  # parsnip's multinom_reg/nnet emits one .pred_i column per ROW for 2-class
  # targets; the engine must fall back to the exactly-equivalent binomial
  # logistic regression and return one probability column per class.
  d <- make_classif_scope_df(nA = 40, nB = 30)
  pts <- sf::st_as_sf(d, coords = c("x", "y"), crs = 32633)
  levs <- levels(d$soil)
  expect_length(levs, 2)

  cv <- run_classification_cv(pts, "soil", c("elev", "slope"),
                              method = "multinom", strategy = "standard", v = 4)
  prob_cols <- paste0(".pred_", levs)
  expect_true(all(prob_cols %in% names(cv$predictions)))
  psum <- rowSums(cv$predictions[, prob_cols])
  expect_true(all(abs(psum - 1) < 1e-6))

  # Tuning depths degrade cleanly for the substitution (glm has no penalty).
  expect_length(.classif_effective_tune_params("multinom", "full", 2), 0)
  expect_length(.classif_effective_tune_params("multinom", "full", 3), 1)
})

# ── Spatial baseline + covariate lift ────────────────────────────────────────

test_that("spatial 1-NN baseline assigns the nearest analysis point's class", {
  # Two folds; each held-out point's nearest other-fold neighbour is known.
  coords <- rbind(c(0, 0), c(10, 0), c(1, 0), c(9, 0))
  y <- factor(c("A", "B", "B", "A"))
  fold <- c(1L, 1L, 2L, 2L)
  b <- classif_spatial_baseline(coords, y, fold)
  # Fold 1 held out: (0,0) -> nearest of rows 3/4 is (1,0) = "B";
  #                  (10,0) -> nearest is (9,0) = "A".
  # Fold 2 held out: (1,0) -> nearest of rows 1/2 is (0,0) = "A";
  #                  (9,0) -> (10,0) = "B".
  expect_equal(as.character(b), c("B", "A", "A", "B"))
  expect_identical(levels(b), levels(y))
})

test_that("covariate lift reproduces hand-computed accuracies and McNemar pairing", {
  levs <- c("A", "B")
  pred_df <- data.frame(
    truth = factor(c("A", "A", "A", "A", "B", "B", "B", "B"), levels = levs),
    .pred_class = factor(c("A", "A", "A", "B", "B", "B", "B", "A"), levels = levs),
    .pred_base  = factor(c("A", "B", "A", "B", "B", "A", "B", "B"), levels = levs)
  )
  # Hand-tallied: model correct rows {1,2,3,5,6,7} -> 6/8; baseline correct
  # rows {1,3,5,7,8} -> 5/8; discordant pairs: baseline-only right = {8} (b=1),
  # model-only right = {2,6} (c=2).
  lf <- classif_covariate_lift(pred_df, "truth")
  expect_equal(lf$model_acc, 0.75)
  expect_equal(lf$baseline_acc, 0.625)
  expect_equal(lf$majority_acc, 0.5)
  expect_equal(lf$lift_abs, 0.125)
  # 3 discordant pairs is far below the 25-pair switch point, so the exact
  # binomial form applies, not the chi-square approximation.
  expect_equal(lf$mcnemar_p, stats::binom.test(2, 3, p = 0.5)$p.value)

  # No discordant pairs -> p undefined, not an error.
  same <- pred_df; same$.pred_base <- same$.pred_class
  expect_true(is.na(classif_covariate_lift(same, "truth")$mcnemar_p))
})

test_that("covariate lift switches from exact binomial to chi-square at 25 discordant pairs", {
  # 2026-08-23 audit, Tier 2: mcnemar.test is the continuity-corrected
  # chi-square APPROXIMATION and is unreliable on few discordant pairs, which
  # is the common case for this panel. Build a frame with an exact discordant
  # tally: n_conc rows where model and baseline are both right, b rows where
  # only the baseline is right, c_ rows where only the model is right.
  levs <- c("A", "B")
  mk <- function(b, c_, n_conc = 40) {
    truth <- rep("A", b + c_ + n_conc)
    model <- c(rep("B", b), rep("A", c_), rep("A", n_conc))
    base  <- c(rep("A", b), rep("B", c_), rep("A", n_conc))
    data.frame(truth = factor(truth, levels = levs),
               .pred_class = factor(model, levels = levs),
               .pred_base  = factor(base, levels = levs))
  }

  # 24 discordant pairs: exact binomial.
  lf_lo <- classif_covariate_lift(mk(10, 14), "truth")
  expect_equal(lf_lo$mcnemar_p, stats::binom.test(14, 24, p = 0.5)$p.value)

  # 25 discordant pairs: continuity-corrected chi-square.
  lf_hi <- classif_covariate_lift(mk(10, 15), "truth")
  expect_equal(lf_hi$mcnemar_p,
               stats::mcnemar.test(matrix(c(0, 10, 15, 0), nrow = 2))$p.value)

  # The two forms genuinely disagree in this regime, which is why the switch
  # exists: the corrected chi-square is the conservative one.
  expect_false(isTRUE(all.equal(
    stats::binom.test(14, 24, p = 0.5)$p.value,
    stats::mcnemar.test(matrix(c(0, 10, 14, 0), nrow = 2))$p.value)))
})

test_that("run_classification_cv threads the baseline through .row alignment", {
  pts <- make_classif_points(n = 60)
  cv <- run_classification_cv(pts, "soil", c("elev", "slope"),
                              method = "rf", strategy = "spatial", v = 5)
  expect_true(all(c(".row", ".pred_base") %in% names(cv$predictions)))
  expect_setequal(cv$predictions$.row, seq_len(nrow(pts)))
  expect_false(anyNA(cv$predictions$.pred_base))
})

# ── Permutation feature importance ───────────────────────────────────────────

test_that("permutation importance ranks the informative covariate above noise", {
  pts <- make_classif_points(n = 60)
  mod <- fit_classification_model(pts, "soil", c("elev", "slope"), method = "rf")
  imp <- classif_permutation_importance(mod, sf::st_drop_geometry(pts),
                                        "soil", c("elev", "slope"))
  expect_setequal(imp$predictor, c("elev", "slope"))
  # elev drives the class gradient in the fixture; slope is pure noise.
  expect_gt(imp$importance[imp$predictor == "elev"],
            imp$importance[imp$predictor == "slope"])
  expect_gt(imp$importance[imp$predictor == "elev"], 0)
  # Positive shares renormalise to 100 %.
  expect_equal(sum(imp$share_pct, na.rm = TRUE), 100, tolerance = 1e-8)
  # Deterministic under the seed sandbox.
  imp2 <- classif_permutation_importance(mod, sf::st_drop_geometry(pts),
                                         "soil", c("elev", "slope"))
  expect_equal(imp$importance, imp2$importance)
})

# ── Confidence threshold (abstention) ────────────────────────────────────────

test_that("confidence threshold abstains low-certainty cells into Unclassified", {
  levs <- c("A", "B", "C")
  grid <- data.frame(
    x = c(450000, 450200, 450400, 450600),
    y = rep(5800000, 4),
    .pred_class = factor(c("A", "A", "C", "B"), levels = levs),
    .pred_A = c(0.90, 0.80, 0.34, 0.10),
    .pred_B = c(0.05, 0.10, 0.33, 0.80),
    .pred_C = c(0.05, 0.10, 0.33, 0.10),
    .entropy = 0.5
  )

  # Threshold 0: byte-identical behaviour to the pre-feature rasteriser.
  rl0 <- classif_surface_to_rasters(grid, res = 200, crs_wkt = sf::st_crs(32633)$wkt,
                                    levels_order = levs, conf_threshold = 0)
  expect_false("Unclassified" %in% rl0$area$class)
  expect_false("Unclassified" %in% terra::levels(rl0$class)[[1]]$class)

  # Threshold 0.5: only the conflicted 34/33/33 cell abstains.
  rl <- classif_surface_to_rasters(grid, res = 200, crs_wkt = sf::st_crs(32633)$wkt,
                                   levels_order = levs, conf_threshold = 0.5)
  expect_true("Unclassified" %in% terra::levels(rl$class)[[1]]$class)
  expect_equal(rl$area$n_cells[rl$area$class == "Unclassified"], 1L)
  expect_equal(rl$area$n_cells[rl$area$class == "C"], 0L)
  # Area accounting stays exact, including the abstained cells.
  expect_equal(sum(rl$area$area_ha), sum(rl$area$n_cells) * (200 * 200 / 10000))
  # Probability and entropy layers are untouched by the threshold.
  expect_equal(terra::values(rl$prob, mat = FALSE),
               terra::values(rl0$prob, mat = FALSE))

  # A threshold at or below 1/k can never fire.
  rl_lo <- classif_surface_to_rasters(grid, res = 200, crs_wkt = sf::st_crs(32633)$wkt,
                                      levels_order = levs, conf_threshold = 1 / 3)
  expect_equal(rl_lo$area$n_cells[rl_lo$area$class == "Unclassified"], 0L)
})

# ── Model export bundle ──────────────────────────────────────────────────────

test_that("pipeline persists a reusable model bundle and reports lift/importance", {
  d <- make_classif_scope_df(nA = 35, nB = 30)
  rds <- tempfile(fileext = ".rds")
  on.exit(unlink(rds), add = TRUE)

  res <- run_classification_pipeline(
    d, target = "soil", predictors = c("elev", "slope"),
    x_col = "x", y_col = "y", src_crs = 32633, proj_crs = "EPSG:32633",
    method = "rf", strategy = "standard", depth = "none", v = 4,
    make_surface = FALSE, model_rds_path = rds
  )

  # New result fields: baseline lift, importance, weight flags, CV predictions.
  expect_s3_class(res$lift, "data.frame")
  expect_true(all(c("model_acc", "baseline_acc", "majority_acc", "lift_abs",
                    "mcnemar_p") %in% names(res$lift)))
  expect_s3_class(res$importance, "data.frame")
  expect_setequal(res$importance$predictor, c("elev", "slope"))
  expect_false(isTRUE(res$weights_applied))
  expect_true(is.data.frame(res$cv_predictions))
  expect_equal(res$target_col, "soil")

  # Bundle round-trip: metadata present and the workflow predicts new data.
  expect_equal(res$model_path, rds)
  expect_true(file.exists(rds))
  b <- readRDS(rds)
  expect_setequal(b$predictors, c("elev", "slope"))
  expect_equal(b$method, "rf")
  expect_equal(b$n_train, nrow(d))
  expect_true(all(c("r", "parsnip", "engine") %in% names(b$versions)))
  nd <- data.frame(elev = c(0, 5, 10), slope = 7)
  p <- predict(b$workflow, nd, type = "prob")
  expect_equal(rowSums(as.matrix(p)), rep(1, 3), tolerance = 1e-6)
})

test_that("classif_scope_adequacy names the offending classes and shortfalls", {
  # Adequate scope: NULL (no warning).
  ok <- factor(rep(c("A", "B", "C"), each = 10))
  expect_null(classif_scope_adequacy(ok, n_complete = 30))

  # Two 1-sample classes must be named with their counts.
  y <- factor(c(rep("A", 20), rep("B", 5), "Rare1", "Rare2"))
  msg <- classif_scope_adequacy(y, n_complete = 27)
  expect_match(msg, "'Rare1' (n = 1)", fixed = TRUE)
  expect_match(msg, "'Rare2' (n = 1)", fixed = TRUE)
  expect_match(msg, "2 classes with fewer than 3", fixed = TRUE)
  expect_false(grepl("'A'", msg, fixed = TRUE))

  # Row shortfall is reported with both counts; combined shortfalls stack.
  msg2 <- classif_scope_adequacy(factor(rep(c("A", "B"), each = 7)), n_complete = 14)
  expect_match(msg2, "only 14 complete rows (need >= 20)", fixed = TRUE)
  msg3 <- classif_scope_adequacy(factor(c(rep("A", 9), "B")), n_complete = 10)
  expect_match(msg3, "only 10 complete rows", fixed = TRUE)
  expect_match(msg3, "'B' (n = 1)", fixed = TRUE)
})

test_that("nested CV tunes per outer fold and reports per-fold winners", {
  pts <- make_classif_points(n = 60)
  cv <- run_classification_cv(pts, "soil", c("elev", "slope"),
                              method = "multinom", strategy = "standard",
                              v = 3, depth = "light", nested = TRUE, inner_v = 3)

  expect_true(cv$nested)
  # No single hyperparameter set exists under nesting; the per-fold winners
  # (one row per outer fold, the tuned penalty) are reported instead.
  expect_null(cv$best_params)
  expect_s3_class(cv$nested_params, "data.frame")
  expect_equal(nrow(cv$nested_params), cv$n_folds)
  expect_true(all(c(".fold", "penalty") %in% names(cv$nested_params)))

  # Out-of-fold predictions still cover every row with valid probabilities.
  expect_equal(nrow(cv$predictions), nrow(pts))
  prob_cols <- paste0(".pred_", levels(sf::st_drop_geometry(pts)$soil))
  expect_equal(unname(rowSums(cv$predictions[, prob_cols])), rep(1, nrow(pts)),
               tolerance = 1e-6)
  acc <- cv$metrics$.estimate[cv$metrics$.metric == "accuracy"]
  expect_true(is.finite(acc) && acc >= 0 && acc <= 1)

  # nested is inert without tuning: depth "none" never activates it.
  cv0 <- run_classification_cv(pts, "soil", c("elev", "slope"),
                               method = "multinom", strategy = "standard",
                               v = 3, depth = "none", nested = TRUE)
  expect_false(cv0$nested)
  expect_null(cv0$nested_params)
})

test_that("stricter VIF threshold flags moderate collinearity the default keeps", {
  # Two predictors with r ~ 0.93 -> VIF ~ 7: below the default 10, above the
  # stricter Random Forest screen at 5.
  set.seed(42)
  x1 <- rnorm(200)
  x2 <- 0.93 * x1 + sqrt(1 - 0.93^2) * rnorm(200)
  x3 <- rnorm(200)
  d <- data.frame(x1 = x1, x2 = x2, x3 = x3)

  loose <- detect_multicollinearity_engine(d, vars = names(d), vif_threshold = 10)
  strict <- detect_multicollinearity_engine(d, vars = names(d), vif_threshold = 5)
  expect_length(loose$dropped, 0)
  expect_gte(length(strict$dropped), 1)
  expect_true(all(strict$dropped %in% c("x1", "x2")))
})

test_that("classif_build_target widens label precision when rounded breaks collide", {
  # Values so tightly clustered that every quantile break rounds to the same
  # 2-dp label; factor() errors on duplicated levels, so the labels must be
  # widened until unique.
  df <- data.frame(v = seq(1.0001, 1.0009, length.out = 40))
  tv <- classif_build_target(df, mode = "bin", cat_col = NULL, num_col = "v",
                             n_classes = 4, style = "quantile")
  expect_s3_class(tv, "factor")
  expect_equal(anyDuplicated(levels(tv)), 0L)
  expect_true(all(!is.na(tv)))
  expect_gte(nlevels(tv), 2)
})

test_that("the final classification model tunes under the run's CV strategy", {
  # fit_classification_model hardcoded "standard" folds for its own tuning
  # search while run_classification_cv honoured the user's strategy. The class
  # map, entropy surface, permutation importance and exported .rds bundle are
  # all built from the FINAL model, so under Spatial CV every one of them
  # shipped hyperparameters chosen under random stratified folds -- exactly the
  # optimism spatial CV exists to remove -- while the metrics table advertised
  # a leakage-free estimate.
  pts <- make_classif_points(60)

  seen <- character(0)
  orig <- classif_make_fold_id
  assign("classif_make_fold_id",
         function(pts_sf, strategy = c("spatial", "standard"), ...) {
           strategy <- match.arg(strategy)
           seen <<- c(seen, strategy)
           orig(pts_sf, strategy, ...)
         }, envir = globalenv())
  on.exit(assign("classif_make_fold_id", orig, envir = globalenv()), add = TRUE)

  suppressWarnings(fit_classification_model(
    pts, "soil", c("elev", "slope"), method = "rf", depth = "light",
    strategy = "spatial", v = 3L))
  expect_identical(unique(seen), "spatial")

  seen <- character(0)
  suppressWarnings(fit_classification_model(
    pts, "soil", c("elev", "slope"), method = "rf", depth = "light",
    strategy = "standard", v = 3L))
  expect_identical(unique(seen), "standard")
})

test_that("untuned classification fits are unaffected by the CV strategy", {
  # depth = "none" tunes nothing, so no folds are built for the final fit and
  # both strategies must give the same model.
  pts <- make_classif_points(60)
  a <- suppressWarnings(fit_classification_model(
    pts, "soil", c("elev", "slope"), method = "rf", depth = "none",
    strategy = "spatial"))
  b <- suppressWarnings(fit_classification_model(
    pts, "soil", c("elev", "slope"), method = "rf", depth = "none",
    strategy = "standard"))

  nd <- data.frame(elev = seq(0, 30, length.out = 25), slope = 7)
  expect_identical(predict_classification_surface(a, nd),
                   predict_classification_surface(b, nd))
  expect_null(a$best_params)
})

test_that("fit_classification_model rejects an unknown CV strategy", {
  pts <- make_classif_points(40)
  expect_error(
    fit_classification_model(pts, "soil", c("elev", "slope"),
                             method = "rf", strategy = "nonsense"),
    "arg")
})

# ── Out-of-fold permutation importance ──────────────────────────────────────

test_that("out-of-fold importance is scored on held-out rows and labelled", {
  pts <- make_classif_points(80)
  cv <- suppressWarnings(run_classification_cv(
    pts, "soil", c("elev", "slope"), method = "rf", strategy = "standard",
    v = 4L, depth = "none", oof_importance = TRUE, importance_reps = 2L))

  imp <- cv$importance
  expect_s3_class(imp, "data.frame")
  expect_setequal(imp$predictor, c("elev", "slope"))
  expect_true(all(imp$evaluated_on == "out-of-fold"))
  expect_true(all(is.finite(imp$importance)))
  # share_pct renormalises the positive importances to 100
  pos <- imp$share_pct[!is.na(imp$share_pct)]
  if (length(pos)) expect_equal(sum(pos), 100, tolerance = 1e-6)
})

test_that("the out-of-fold importance sandbox leaves the fold RNG stream untouched", {
  # .classif_perm_delta burns n_rep x n_predictors sample.int() draws. Before the
  # sandbox those draws landed on the fold loop's own stream, so every LATER
  # fold's fit started from a different random state and a purely diagnostic
  # toggle could move the reported CV metrics. It bites whenever the learner
  # consumes RNG: with the shipped registry that is xgboost at depth "full",
  # which tunes sample_size / mtry (subsampling and colsample are random) —
  # multinom (nnet rang = 0), ranger (seed = 12345L) and xgboost at
  # none/light (sample_size = 1) all fit deterministically. This pins the
  # property that makes the fold loop invariant regardless of learner, which
  # matters as the tuning registry and learner list are meant to grow.
  pts <- make_classif_points(60)
  df <- as.data.frame(sf::st_drop_geometry(pts))
  mod <- suppressWarnings(fit_classification_model(
    pts, "soil", c("elev", "slope"), method = "rf", depth = "none"))

  set.seed(99); ref <- runif(3)
  set.seed(99)
  imp <- .classif_with_seed(1L + 977L,
    .classif_perm_delta(mod$workflow, df, "soil", c("elev", "slope"), n_rep = 2L))
  expect_equal(runif(3), ref)
  # ... and the importance itself is still computed, reproducibly from the seed
  expect_length(imp$delta, 2L)
  imp2 <- .classif_with_seed(1L + 977L,
    .classif_perm_delta(mod$workflow, df, "soil", c("elev", "slope"), n_rep = 2L))
  expect_equal(imp2$delta, imp$delta)
})

test_that("out-of-fold importance leaves the CV predictions unchanged", {
  pts <- make_classif_points(70)
  args <- list(pts, "soil", c("elev", "slope"), method = "multinom",
               strategy = "standard", v = 4L, depth = "none")
  without  <- suppressWarnings(do.call(run_classification_cv, args))
  with_imp <- suppressWarnings(do.call(run_classification_cv,
    c(args, list(oof_importance = TRUE, importance_reps = 2L))))

  expect_equal(with_imp$predictions, without$predictions)
  expect_equal(with_imp$metrics, without$metrics)
  expect_s3_class(with_imp$importance, "data.frame")
  expect_null(without$importance)
})

test_that("out-of-fold importance is not computed unless asked for", {
  pts <- make_classif_points(60)
  cv <- suppressWarnings(run_classification_cv(
    pts, "soil", c("elev", "slope"), method = "rf", strategy = "standard",
    v = 3L, depth = "none"))
  expect_null(cv$importance)
})

test_that("pooling fold importances equals a size-weighted mean", {
  # The pooled delta must be what one evaluation over every out-of-fold row
  # would give, so folds contribute in proportion to the rows they scored.
  parts <- list(
    list(delta = c(1, 0), baseline = 0.5, n = 10),
    list(delta = c(3, 4), baseline = 1.5, n = 30)
  )
  out <- .classif_pool_fold_importance(parts, c("a", "b"))
  # a: (1*10 + 3*30)/40 = 2.5 ; b: (0*10 + 4*30)/40 = 3.0
  expect_equal(out$importance[out$predictor == "a"], 2.5)
  expect_equal(out$importance[out$predictor == "b"], 3.0)
  expect_equal(out$baseline_logloss[1], (0.5 * 10 + 1.5 * 30) / 40)
  expect_true(all(out$evaluated_on == "out-of-fold"))
  expect_null(.classif_pool_fold_importance(list(), c("a", "b")))
})

test_that("the baked-block fast path reproduces the re-bake-per-shuffle deltas", {
  # Identity pin for the permutation-importance fast path. The reference below
  # IS the pre-2026-08-14 implementation (permute the raw column, predict through
  # the workflow, which re-bakes the recipe on every shuffle). The fast path
  # bakes once and permutes the baked block; it must return the same numbers from
  # the same seed, or the equivalence argument in .classif_perm_fast_path is
  # wrong for this recipe. The fixture carries a nominal predictor (dummy block)
  # and NAs (imputation) so both non-trivial steps are exercised.
  perm_delta_reference <- function(wf, df, target, predictors, n_rep) {
    truth <- as.factor(df[[target]])
    log_loss_of <- function(newdata) {
      prob <- as.matrix(predict(wf, newdata, type = "prob"))
      colnames(prob) <- sub("^\\.pred_", "", colnames(prob))
      absent <- setdiff(levels(truth), colnames(prob))
      if (length(absent)) {
        prob <- cbind(prob, matrix(0, nrow(prob), length(absent),
                                   dimnames = list(NULL, absent)))
      }
      p_true <- prob[cbind(seq_len(nrow(prob)), match(as.character(truth), colnames(prob)))]
      -mean(log(pmax(p_true, 1e-15)))
    }
    base_ll <- log_loss_of(df)
    delta <- vapply(seq_along(predictors), function(j) {
      p <- predictors[j]
      lls <- vapply(seq_len(n_rep), function(r) {
        d2 <- df
        d2[[p]] <- d2[[p]][sample.int(nrow(d2))]
        log_loss_of(d2)
      }, numeric(1))
      mean(lls) - base_ll
    }, numeric(1))
    list(delta = delta, baseline = base_ll, n = nrow(df))
  }

  pts <- make_classif_points(70)
  df <- as.data.frame(sf::st_drop_geometry(pts))
  df$slope[c(3, 11, 25)] <- NA_real_          # median imputation
  df$parent[c(5, 19)] <- NA                   # mode imputation + dummy block
  preds <- c("elev", "slope", "parent")
  mod <- suppressWarnings(fit_classification_model(
    pts, "soil", preds, method = "rf", depth = "none"))

  set.seed(4242)
  ref <- perm_delta_reference(mod$workflow, df, "soil", preds, n_rep = 3L)
  set.seed(4242)
  got <- .classif_perm_delta(mod$workflow, df, "soil", preds, n_rep = 3L)

  expect_equal(got$delta, ref$delta, tolerance = 1e-12)
  expect_equal(got$baseline, ref$baseline, tolerance = 1e-12)
  expect_equal(got$n, ref$n)

  # ... and the fast path was actually taken for every predictor (otherwise the
  # equality above would only prove the fallback still works).
  fp <- .classif_perm_fast_path(mod$workflow, df, preds)
  expect_false(is.null(fp))
  expect_true(all(fp$ok))
  expect_equal(fp$blocks$elev, "elev")
  expect_true(all(startsWith(fp$blocks$parent, "parent_")))
})

test_that("a predictor whose baked block cannot be identified falls back, not wrong", {
  # The block mapping is a name guess (`<var>` or `<var>_<level>`), so a nominal
  # predictor whose dummy names collide with another predictor's name would
  # permute two variables at once. The probe must catch it and mark that
  # predictor for the slow path.
  set.seed(11)
  n <- 60
  df <- data.frame(
    soil = factor(sample(c("Low", "High"), n, replace = TRUE)),
    ph = factor(sample(c("field", "lab"), n, replace = TRUE)),
    ph_field = runif(n, 4, 8)
  )
  pts <- sf::st_as_sf(
    cbind(df, x = runif(n, 450000, 451000), y = runif(n, 5800000, 5801000)),
    coords = c("x", "y"), crs = 32633)
  preds <- c("ph", "ph_field")
  mod <- suppressWarnings(fit_classification_model(
    pts, "soil", preds, method = "rf", depth = "none"))

  fp <- .classif_perm_fast_path(mod$workflow, df, preds)
  # `ph`'s candidate block wrongly swallows the `ph_field` column, so its probe
  # must fail; `ph_field` maps to itself and is fine.
  expect_false(fp$ok[["ph"]])
  expect_true(fp$ok[["ph_field"]])

  # and the delta is still the slow-path answer for the colliding predictor
  perm_ref <- function(p, seed) {
    set.seed(seed)
    truth <- as.factor(df$soil)
    ll <- function(nd) {
      prob <- as.matrix(predict(mod$workflow, nd, type = "prob"))
      colnames(prob) <- sub("^\\.pred_", "", colnames(prob))
      pt <- prob[cbind(seq_len(nrow(prob)), match(as.character(truth), colnames(prob)))]
      -mean(log(pmax(pt, 1e-15)))
    }
    base <- ll(df)
    d2 <- df; d2[[p]] <- d2[[p]][sample.int(nrow(df))]
    ll(d2) - base
  }
  set.seed(7)
  got <- .classif_perm_delta(mod$workflow, df, "soil", "ph", n_rep = 1L)
  expect_equal(got$delta[[1]], perm_ref("ph", 7), tolerance = 1e-12)
})

test_that("training-row importance keeps its own label", {
  pts <- make_classif_points(60)
  mod <- suppressWarnings(fit_classification_model(
    pts, "soil", c("elev", "slope"), method = "rf", depth = "none"))
  imp <- classif_permutation_importance(mod, sf::st_drop_geometry(pts),
                                        "soil", c("elev", "slope"), n_rep = 2L)
  expect_true(all(imp$evaluated_on == "training"))
  expect_setequal(imp$predictor, c("elev", "slope"))
})

test_that("classif_build_grid flags a strict buffer below half the cell diagonal", {
  pts <- make_classif_points(n = 50)

  g_bad <- classif_build_grid(pts, res = 350, boundary = "strict",
                              buffer_dist = 175)
  expect_match(g_bad$strict_warning, "Strict Measured buffer")
  expect_match(g_bad$strict_warning, "248 m or more")

  # 300 m clears the 247.5 m half-diagonal of a 350 m cell.
  g_ok <- classif_build_grid(pts, res = 350, boundary = "strict",
                             buffer_dist = 300)
  expect_null(g_ok$strict_warning)

  # Hull styles are never flagged: a hull surrounds every interior sample, so
  # only its perimeter is exposed, unlike strict where every isolated sample is.
  expect_null(classif_build_grid(pts, res = 350, boundary = "concave")$strict_warning)
  expect_null(classif_build_grid(pts, res = 350, boundary = "wrapped",
                                 buffer_dist = 175)$strict_warning)

  # Polygons-only scoping replaces the hull with the user's polygons, so the
  # Boundary Type control is inert and the advisory must stay silent even
  # though the style still reads "strict".
  expect_null(classif_build_grid(pts, res = 350, boundary = "strict",
                                 buffer_dist = 175,
                                 strict_scope = FALSE)$strict_warning)
})

test_that("classif_auto_res reproduces the grid builder's Auto resolution", {
  # The module's pre-run advisory computes the effective cell size from the
  # scope's area and bbox; it must be the number the run will actually use.
  pts <- make_classif_points(n = 50)
  bnd <- sf::st_as_sf(sf::st_sfc(.classif_scope_hulls(pts, style = "concave"),
                                 crs = sf::st_crs(pts)))
  gr <- classif_build_grid(pts, res = NULL, boundary_sf = bnd)
  expect_equal(gr$res,
               classif_auto_res(sum(as.numeric(sf::st_area(bnd))), sf::st_bbox(bnd)))
})

test_that("classif_resolve_scope reports the boundary area and bbox", {
  d <- make_classif_scope_df()
  sc <- classif_resolve_scope(d, "x", "y", 32633, "EPSG:32633",
                              loc_col = "loc", localities = "ALL")
  bnd <- sf::st_as_sfc(sc$boundary_wkt, crs = 32633)
  expect_equal(sc$boundary_area_m2, sum(as.numeric(sf::st_area(bnd))))
  expect_equal(as.numeric(sc$boundary_bbox), as.numeric(sf::st_bbox(bnd)))

  # An empty scope carries no boundary geometry at all.
  sc_none <- classif_resolve_scope(d, "x", "y", 32633, "EPSG:32633",
                                   loc_col = "loc", localities = "nonexistent")
  expect_null(sc_none$boundary_area_m2)
  expect_null(sc_none$boundary_bbox)
})
