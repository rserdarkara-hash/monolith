# test-interpolation-pipeline.R — tests for init_interpolation_res,
# sanitize_spatial_predictions, safe_run_cv, suggest_lmc_model,
# calc_scientific_lags, merge_wrapped_rasters, get_joint_scale_values,
# validate_and_project_sf, the apply_* interpolation engines
# (IDW/TPS/OK/RK/RFK/CK) and the apply_interpolation dispatcher.
#
# Engine calls write progress files via update_progress_file(); the
# monolith_progress_dir option is unset in tests, so they go to tempdir()
# and leave no trace in the repo.

# ── init_interpolation_res ─────────────────────────────────────────────────

test_that("init_interpolation_res returns list with all expected names", {
  res <- init_interpolation_res()
  expected_names <- c("v_emp", "fit", "cv_metrics", "model_summary",
                      "rf_model", "gstat_obj", "res_sf", "log_msg",
                      "cv_obj", "cv_obj_reps", "residuals")
  expect_setequal(names(res), expected_names)
  expect_equal(res$log_msg, "")
  expect_null(res$v_emp)
  expect_null(res$res_sf)
  expect_null(res$cv_obj_reps)  # repeated CV is opt-in
})

# ── sanitize_spatial_predictions ───────────────────────────────────────────

test_that("sanitize_spatial_predictions replaces NaN and Inf with NA", {
  pts <- make_test_points(10)
  grid <- make_test_grid_safe(pts, res = 100)
  n <- nrow(grid)
  vals <- rep(1, n)
  vals[1:5] <- c(NaN, Inf, -Inf, NA, 5)
  grid$var1.pred <- vals
  grid$var1.var  <- rep(0.5, n)

  cleaned <- sanitize_spatial_predictions(grid)
  # After sanitization, no NaN or Inf should remain
  remaining <- cleaned$var1.pred[!is.na(cleaned$var1.pred)]
  expect_false(any(is.nan(remaining) | is.infinite(remaining)))
  # Original NAs should remain
  expect_true(any(is.na(cleaned$var1.pred)))
})

test_that("sanitize_spatial_predictions handles NULL input", {
  expect_null(sanitize_spatial_predictions(NULL))
})

test_that("sanitize_spatial_predictions handles sf without var1.pred column", {
  pts <- make_test_points(5)
  cleaned <- sanitize_spatial_predictions(pts)
  expect_s3_class(cleaned, "sf")
  expect_equal(nrow(cleaned), 5)
})

# ── safe_run_cv ────────────────────────────────────────────────────────────

test_that("safe_run_cv catches errors and stores error message", {
  res <- init_interpolation_res()
  res <- safe_run_cv(res, stop("forced error"), "TEST", 10)
  expect_match(res$log_msg, "TEST CV Error: forced error")
  expect_null(res$cv_obj)
})

test_that("safe_run_cv drops pre-set training residuals when CV fails", {
  res <- init_interpolation_res()
  res$residuals <- 1:5  # training residuals, set by RK/RFK for the variogram step
  res <- safe_run_cv(res, stop("boom"), "RK", 5)
  expect_null(res$cv_obj)
  expect_null(res$residuals)
  expect_match(res$log_msg, "RK CV Error", fixed = TRUE)
})

test_that("safe_run_cv stores cv_obj on success", {
  pts <- make_test_points(10)
  cv_df <- data.frame(
    var1.pred     = pts$v + rnorm(10, 0, 0.5),
    var1.observed = pts$v,
    x = sf::st_coordinates(pts)[, 1],
    y = sf::st_coordinates(pts)[, 2]
  )
  res <- init_interpolation_res()
  res <- safe_run_cv(res, cv_df, "TEST_OK", 10)
  expect_false(is.null(res$cv_obj))
  expect_false(is.null(res$cv_metrics))
  expect_false(is.na(res$cv_metrics$rmse))
})

test_that("safe_run_cv computes residuals when cv_obj is available", {
  pts <- make_test_points(8)
  cv_df <- data.frame(
    var1.pred     = pts$v + rnorm(8, 0, 0.3),
    var1.observed = pts$v,
    x = sf::st_coordinates(pts)[, 1],
    y = sf::st_coordinates(pts)[, 2]
  )
  res <- init_interpolation_res()
  res <- safe_run_cv(res, cv_df, "TEST_RESID", 8)
  expect_equal(length(res$residuals), 8)
  expect_true(is.numeric(res$residuals))
})

# ── suggest_lmc_model ─────────────────────────────────────────────────────

test_that("suggest_lmc_model returns 'Sph' when primary_vgm is NULL", {
  expect_equal(suggest_lmc_model(NULL), "Sph")
})

test_that("suggest_lmc_model extracts model type from vgm object", {
  vgm_sph <- make_mock_vgm("Sph")
  expect_equal(suggest_lmc_model(vgm_sph), "Sph")

  vgm_exp <- make_mock_vgm("Exp")
  expect_equal(suggest_lmc_model(vgm_exp), "Exp")

  vgm_gau <- make_mock_vgm("Gau")
  expect_equal(suggest_lmc_model(vgm_gau), "Gau")
})

test_that("suggest_lmc_model returns 'Sph' for Nug-only model", {
  vgm_nug <- gstat::vgm(0.5, "Nug", 0, 0.1)
  # Nug model has no non-Nug component
  expect_equal(suggest_lmc_model(vgm_nug), "Sph")
})

# ── calc_scientific_lags ──────────────────────────────────────────────────

test_that("calc_scientific_lags returns width = cutoff/15", {
  pts <- make_test_points(15)
  lags <- calc_scientific_lags(pts)
  expect_true(is.list(lags))
  expect_true("width" %in% names(lags))
  expect_true("cutoff" %in% names(lags))
  expect_equal(lags$width, lags$cutoff / 15)
})

test_that("calc_scientific_lags cutoff is half the bounding-box diagonal", {
  pts <- make_test_points(10)
  bbox <- sf::st_bbox(pts)
  max_dist <- sqrt((bbox[["xmax"]] - bbox[["xmin"]])^2 +
                   (bbox[["ymax"]] - bbox[["ymin"]])^2)
  lags <- calc_scientific_lags(pts)
  expect_equal(as.numeric(lags$cutoff), as.numeric(max_dist / 2))
})

# ── merge_wrapped_rasters ─────────────────────────────────────────────────

test_that("merge_wrapped_rasters returns NULL for empty or NULL input", {
  expect_null(merge_wrapped_rasters(NULL))
  expect_null(merge_wrapped_rasters(list()))
  expect_null(merge_wrapped_rasters(list(NULL, NULL)))
})

test_that("merge_wrapped_rasters returns single unwrapped raster", {
  pts <- make_test_points(10)
  bbox <- sf::st_bbox(pts)
  r <- terra::rast(terra::ext(bbox), resolution = 50,
                   crs = sf::st_crs(pts)$wkt)
  values(r) <- 1:terra::ncell(r)
  wrapped <- terra::wrap(r)
  result <- merge_wrapped_rasters(list(wrapped))
  expect_s4_class(result, "SpatRaster")
})

test_that("merge_wrapped_rasters merges multiple rasters", {
  pts <- make_test_points(10)
  bbox <- sf::st_bbox(pts)
  r1 <- terra::rast(terra::ext(bbox), resolution = 80,
                    crs = sf::st_crs(pts)$wkt)
  values(r1) <- 1:terra::ncell(r1)

  # Second raster shifted slightly
  bbox2 <- c(xmin = as.numeric(bbox[["xmin"]]) + 500,
             xmax = as.numeric(bbox[["xmax"]]) + 500,
             ymin = as.numeric(bbox[["ymin"]]),
             ymax = as.numeric(bbox[["ymax"]]))
  r2 <- terra::rast(terra::ext(bbox2), resolution = 80,
                    crs = sf::st_crs(pts)$wkt)
  values(r2) <- 100:(99 + terra::ncell(r2))

  result <- merge_wrapped_rasters(list(terra::wrap(r1), terra::wrap(r2)))
  expect_s4_class(result, "SpatRaster")
})

# ── get_joint_scale_values ────────────────────────────────────────────────

test_that("get_joint_scale_values returns NULL when match_scales is FALSE", {
  expect_null(get_joint_scale_values(NULL, NULL, match_scales = FALSE, is_uncertainty = FALSE))
})

test_that("get_joint_scale_values returns NULL for is_uncertainty = TRUE", {
  expect_null(get_joint_scale_values(NULL, NULL, match_scales = TRUE, is_uncertainty = TRUE))
})

test_that("get_joint_scale_values returns combined values from two rasters", {
  pts <- make_test_points(10)
  bbox <- sf::st_bbox(pts)
  r1 <- terra::rast(terra::ext(bbox), resolution = 80,
                    crs = sf::st_crs(pts)$wkt)
  values(r1) <- 10
  r1$var1.pred <- r1

  r2 <- terra::rast(terra::ext(bbox), resolution = 80,
                    crs = sf::st_crs(pts)$wkt)
  values(r2) <- 20
  r2$var1.pred <- r2

  vals <- get_joint_scale_values(terra::wrap(r1), terra::wrap(r2),
                                 match_scales = TRUE, is_uncertainty = FALSE)
  expect_true(is.numeric(vals))
  expect_true(length(vals) > 0)
})

# ── validate_and_project_sf ───────────────────────────────────────────────

test_that("validate_and_project_sf returns NULL for NULL input", {
  expect_null(validate_and_project_sf(NULL))
})

test_that("validate_and_project_sf returns NULL for empty sf (0 rows)", {
  pts <- make_test_points(3)
  pts_empty <- pts[0, ]
  expect_null(validate_and_project_sf(pts_empty))
})

test_that("validate_and_project_sf auto-projects lat/lon to UTM", {
  # Create points in WGS84
  set.seed(42)
  coords <- data.frame(
    x = runif(10, 32.5, 33.0),
    y = runif(10, 39.5, 40.0),
    v = rnorm(10)
  )
  pts_ll <- sf::st_as_sf(coords, coords = c("x", "y"), crs = 4326)
  pts_proj <- validate_and_project_sf(pts_ll)
  expect_s3_class(pts_proj, "sf")
  # Should now be projected (not longlat)
  expect_false(sf::st_is_longlat(pts_proj))
})

test_that("validate_and_project_sf leaves projected data unchanged", {
  pts <- make_test_points(10)  # already UTM zone 33
  pts_proj <- validate_and_project_sf(pts)
  expect_equal(sf::st_crs(pts_proj)$epsg, sf::st_crs(pts)$epsg)
})

# ── dedup_valid_points ─────────────────────────────────────────────────────

test_that("dedup_valid_points drops target NAs and refreshes x/y", {
  pts <- make_test_points(6)
  pts$v[c(2, 4)] <- NA
  out <- dedup_valid_points(pts, "v")
  expect_equal(nrow(out), 4)
  expect_false(any(is.na(out$v)))
  # x/y columns must match the geometry after filtering
  coords <- sf::st_coordinates(out)
  expect_equal(out$x, unname(coords[, 1]))
  expect_equal(out$y, unname(coords[, 2]))
})

test_that("dedup_valid_points removes points sharing a rounded coordinate", {
  pts <- make_test_points(5)
  # Force rows 1 and 2 onto the same location (within 2 dp)
  geom <- sf::st_geometry(pts)
  geom[[2]] <- geom[[1]]
  sf::st_geometry(pts) <- geom
  out <- dedup_valid_points(pts, "v")
  expect_equal(nrow(out), 4)
})

test_that("dedup_valid_points keeps the valid neighbour when the co-located point has an NA target", {
  # Regression: deduping BEFORE NA-filtering would keep row 1 (NA target) and
  # then drop it, silently discarding row 2's valid measurement at that spot.
  pts <- make_test_points(4)
  geom <- sf::st_geometry(pts)
  geom[[2]] <- geom[[1]]          # rows 1 & 2 co-located
  sf::st_geometry(pts) <- geom
  pts$v[1] <- NA                  # first (surviving) copy has no target
  keep_val <- pts$v[2]
  out <- dedup_valid_points(pts, "v")
  # The co-located location must survive, carrying row 2's value
  same_loc <- out[round(out$x, 2) == round(sf::st_coordinates(pts)[1, 1], 2) &
                  round(out$y, 2) == round(sf::st_coordinates(pts)[1, 2], 2), ]
  expect_equal(nrow(same_loc), 1)
  expect_equal(same_loc$v, keep_val)
})

test_that("dedup_valid_points returns 0-row sf when all targets are NA", {
  pts <- make_test_points(4)
  pts$v <- NA_real_
  out <- dedup_valid_points(pts, "v")
  expect_s3_class(out, "sf")
  expect_equal(nrow(out), 0)
})

# ── apply_TPS CV object ───────────────────────────────────────────────────

test_that("apply_TPS returns cv_obj as sf carrying the input CRS", {
  pts <- make_test_points(15)
  grid <- make_test_grid_safe(pts, res = 200)
  # fields::Tps always runs its GCV grid search for diagnostics, which warns
  # about endpoint minima on small noisy data — irrelevant to what we test here
  res <- suppressWarnings(apply_TPS(pts, "v", grid, list(tps_lambda = 0.01)))
  expect_s3_class(res$cv_obj, "sf")
  expect_equal(sf::st_crs(res$cv_obj), sf::st_crs(pts))
  expect_true(all(c("observed", "var1.pred") %in% colnames(res$cv_obj)))
  # CV metrics must still compute from the sf object
  expect_false(is.na(res$cv_metrics$rmse))
})

# ── apply_IDW ─────────────────────────────────────────────────────────────

test_that("apply_IDW returns predictions, CV metrics and residuals", {
  pts <- make_test_points(15)
  grid <- make_test_grid_safe(pts, res = 200)
  res <- apply_IDW(pts, "v", grid, list(idw_p = 2, idw_nmax = 12))

  expect_s3_class(res$res_sf, "sf")
  expect_true("var1.pred" %in% colnames(res$res_sf))
  preds <- res$res_sf$var1.pred
  expect_false(any(is.nan(preds) | is.infinite(preds)))
  # IDW is a convex combination of observations, so predictions are bounded
  # by the observed data range
  ok_preds <- preds[!is.na(preds)]
  expect_true(all(ok_preds >= min(pts$v) - 1e-9 & ok_preds <= max(pts$v) + 1e-9))

  expect_false(is.na(res$cv_metrics$rmse))
  expect_length(res$residuals, 15)
})

test_that("apply_IDW reproduces observed values at data locations", {
  pts <- make_test_points(12)
  # Predict at the sample locations themselves: IDW must be exact there
  res <- apply_IDW(pts, "v", pts, list(idw_p = 2, idw_nmax = 12))
  expect_equal(res$res_sf$var1.pred, pts$v, tolerance = 1e-8)
})

# ── Repeated CV through the engines ───────────────────────────────────────

test_that("repeated CV adds realizations without moving the reference run", {
  # n > 50 so the Auto plan is a random 10-fold (a repeatable partition)
  pts <- make_test_points(60)
  grid <- make_test_grid_safe(pts, res = 200)
  mp <- list(idw_p = 2, idw_nmax = 12, cv_strategy = "auto")

  off <- apply_IDW(pts, "v", grid, mp)
  on  <- apply_IDW(pts, "v", grid, c(mp, list(cv_repeats = 3)))

  # Switching repeats on must not change ANY reported number: realization 1
  # keeps CV_FOLD_SEED, and the surface never sees the CV at all.
  expect_null(off$cv_obj_reps)
  expect_equal(on$cv_metrics, off$cv_metrics)
  expect_equal(on$res_sf$var1.pred, off$res_sf$var1.pred)
  expect_equal(on$residuals, off$residuals)

  expect_length(on$cv_obj_reps, 3)
  # Realization 1 IS the reference run
  expect_equal(perform_cv(on$cv_obj_reps[[1]], moran = FALSE)$rmse, off$cv_metrics$rmse)
  # ... and the others are genuinely different partitions
  rmses <- vapply(on$cv_obj_reps, function(x) perform_cv(x, moran = FALSE)$rmse, numeric(1))
  expect_equal(length(unique(rmses)), 3)

  summ <- summarise_cv_repeats(on$cv_obj_reps)
  expect_equal(summ$n_repeats, 3L)
  expect_true(summ$sd[["rmse"]] > 0)
})

test_that("repeated CV is skipped for deterministic LOOCV plans", {
  # Small n under Auto degrades to LOOCV; explicit LOOCV does the same at any n
  pts <- make_test_points(20)
  grid <- make_test_grid_safe(pts, res = 200)
  auto <- apply_IDW(pts, "v", grid, list(idw_p = 2, idw_nmax = 12,
                                         cv_strategy = "auto", cv_repeats = 5))
  loo <- apply_IDW(pts, "v", grid, list(idw_p = 2, idw_nmax = 12,
                                        cv_strategy = "loocv", cv_repeats = 5))
  expect_null(auto$cv_obj_reps)
  expect_null(loo$cv_obj_reps)
  expect_false(is.na(auto$cv_metrics$rmse))
})

test_that("repeated CV reaches the kriging engines too", {
  pts <- make_test_points(60)
  grid <- make_test_grid_safe(pts, res = 200)
  lags <- calc_scientific_lags(pts)

  ok_off <- suppressWarnings(apply_OK(pts, "v", grid, lags, list(cv_strategy = "auto")))
  ok_on <- suppressWarnings(apply_OK(pts, "v", grid, lags,
                                     list(cv_strategy = "auto", cv_repeats = 2)))
  expect_null(ok_off$cv_obj_reps)
  expect_length(ok_on$cv_obj_reps, 2)
  expect_equal(ok_on$cv_metrics, ok_off$cv_metrics)
  expect_equal(ok_on$res_sf$var1.pred, ok_off$res_sf$var1.pred)

  rk_on <- suppressWarnings(apply_RK(pts, "v", grid, lags,
                                     list(cv_strategy = "auto", cv_repeats = 2),
                                     c("aux1", "aux2")))
  expect_length(rk_on$cv_obj_reps, 2)
  expect_equal(perform_cv(rk_on$cv_obj_reps[[1]], moran = FALSE)$rmse, rk_on$cv_metrics$rmse)
})

# ── apply_TPS exactness ───────────────────────────────────────────────────

test_that("apply_TPS with lambda = 0 interpolates data points exactly", {
  pts <- make_test_points(15)
  # fields::Tps warns about its GCV diagnostics grid on small noisy data;
  # irrelevant here since lambda is fixed at 0
  res <- suppressWarnings(apply_TPS(pts, "v", pts, list(tps_lambda = 0)))
  expect_equal(res$res_sf$var1.pred, pts$v, tolerance = 1e-4)
})

test_that("apply_TPS logs and writes a warning file when it falls back to IDW", {
  pts <- make_test_points(12)
  grid <- make_test_grid_safe(pts, res = 200)

  tmp <- tempfile("tps_fallback_")
  dir.create(tmp)
  old_progress_dir <- getOption("monolith_progress_dir")
  old_session_id  <- getOption("monolith_session_id")
  options(monolith_progress_dir = tmp, monolith_session_id = "tps_fb")
  on.exit({
    options(monolith_progress_dir = old_progress_dir,
            monolith_session_id  = old_session_id)
    unlink(tmp, recursive = TRUE)
  }, add = TRUE)

  testthat::with_mocked_bindings(
    {
      # the app always passes the IDW params alongside tps_lambda (see mp_a
      # in run_regional_interpolation), so the fallback can use them
      res <- apply_TPS(pts, "v", grid, list(tps_lambda = 0.01, idw_p = 2, idw_nmax = 12),
                       l = "loc_A", prefix = "act")
    },
    Tps = function(...) stop("forced TPS failure"),
    .package = "fields"
  )

  # Fallback still yields IDW predictions and CV results
  expect_s3_class(res$res_sf, "sf")
  expect_false(is.na(res$cv_metrics$rmse))
  # The fallback must be visible in the run log, like the CK/RFK fallbacks
  expect_match(res$log_msg, "TPS failed", fixed = TRUE)
  expect_match(res$log_msg, "Falling back to IDW", fixed = TRUE)
  # ... and in the per-locality warning file
  warn_file <- file.path(tmp, "warn_tps_fb_loc_A_act.txt")
  expect_true(file.exists(warn_file))
  expect_match(readLines(warn_file), "IDW fallback", fixed = TRUE)
})

# ── apply_OK ──────────────────────────────────────────────────────────────

test_that("apply_OK returns variogram, fit, predictions and CV results", {
  pts <- make_test_points(20)
  grid <- make_test_grid_safe(pts, res = 200)
  lags <- calc_scientific_lags(pts)
  res <- suppressWarnings(apply_OK(pts, "v", grid, lags, list()))

  expect_s3_class(res$v_emp, "gstatVariogram")
  expect_s3_class(res$fit, "variogramModel")
  expect_true(all(c("var1.pred", "var1.var") %in% colnames(res$res_sf)))
  # Kriging variance must be non-negative (up to numerical noise)
  vv <- res$res_sf$var1.var
  expect_true(all(vv[!is.na(vv)] >= -1e-6))
  expect_false(is.na(res$cv_metrics$rmse))
  expect_length(res$residuals, 20)
})

test_that("apply_OK honours the Spatial Block CV strategy end-to-end", {
  pts <- make_test_points(40) # >= CV_BLOCK_MIN_N so blocks are real
  grid <- make_test_grid_safe(pts, res = 200)
  lags <- calc_scientific_lags(pts)
  res <- suppressWarnings(apply_OK(pts, "v", grid, lags, list(cv_strategy = "block")))

  # Block folds (a length-n integer vector) must flow through krige.cv and
  # still yield finite CV metrics over all 40 held-out points.
  expect_false(is.na(res$cv_metrics$rmse))
  expect_length(res$residuals, 40)
  expect_s3_class(res$res_sf, "sf")
})

test_that("apply_OK uses a supplied pre_fit variogram instead of refitting", {
  pts <- make_test_points(15)
  grid <- make_test_grid_safe(pts, res = 200)
  lags <- calc_scientific_lags(pts)
  manual_fit <- gstat::vgm(psill = var(pts$v), model = "Sph",
                           range = lags$cutoff / 2, nugget = 0.1)
  res <- suppressWarnings(
    apply_OK(pts, "v", grid, lags, list(pre_fit = manual_fit))
  )
  expect_identical(res$fit, manual_fit)
})

# ── apply_RK ──────────────────────────────────────────────────────────────

test_that("apply_RK fits an lm trend and kriges its residuals", {
  pts <- make_test_points(15)
  pts$v <- pts$v + 0.5 * pts$aux1   # real trend so the regression is meaningful
  grid <- make_test_grid_safe(pts, res = 200)
  lags <- calc_scientific_lags(pts)
  res <- suppressWarnings(apply_RK(pts, "v", grid, lags, list(), c("aux1")))

  expect_s3_class(res$model_summary, "summary.lm")
  expect_false(grepl("Falling back to OK", res$log_msg, fixed = TRUE))
  expect_s3_class(res$res_sf, "sf")
  expect_true(all(c("var1.pred", "var1.var") %in% colnames(res$res_sf)))
  expect_length(res$residuals, 15)
})

test_that("apply_RK falls back to OK when trend prediction fails", {
  pts <- make_test_points(12)
  grid <- make_test_grid_safe(pts, res = 200)
  lags <- calc_scientific_lags(pts)
  # grid_aux lacking the covariate column makes predict.lm fail
  # deterministically, which must trigger the OK fallback
  bad_grid_aux <- sf::st_drop_geometry(grid)[, c("x", "y")]
  res <- suppressWarnings(
    apply_RK(pts, "v", grid, lags, list(grid_aux = bad_grid_aux), c("aux1"))
  )
  expect_match(res$log_msg, "RK failed")
  expect_s3_class(res$res_sf, "sf")
  expect_true("var1.pred" %in% colnames(res$res_sf))
  expect_false(is.na(res$cv_metrics$rmse))
})

# ── apply_RFK ─────────────────────────────────────────────────────────────

test_that("apply_RFK returns rf model with requested ntree and predictions", {
  pts <- make_test_points(12)
  pts$v <- pts$v + 0.5 * pts$aux1
  grid <- make_test_grid_safe(pts, res = 200)
  lags <- calc_scientific_lags(pts)
  set.seed(99)
  res <- suppressWarnings(
    apply_RFK(pts, "v", grid, lags, list(rf_ntree = 50), c("aux1"))
  )

  expect_s3_class(res$rf_model, "randomForest")
  expect_equal(res$rf_model$ntree, 50)
  expect_false(grepl("Falling back to OK", res$log_msg, fixed = TRUE))
  expect_s3_class(res$res_sf, "sf")
  expect_true(all(c("var1.pred", "var1.var") %in% colnames(res$res_sf)))
})

test_that("rf_infinitesimal_jackknife_var matches the brute-force Wager formula", {
  set.seed(1)
  n_train <- 20L; B <- 40L; n_pred <- 7L
  inbag <- matrix(rpois(n_train * B, lambda = 1), nrow = n_train)
  M <- matrix(rnorm(n_pred * B), nrow = n_pred)

  # Naive reference: raw IJ minus Monte-Carlo bias, negatives floored at 0.
  ij_ref <- function(M, N) {
    Bt <- ncol(M); nt <- nrow(N)
    Nc <- N - rowMeans(N)
    Mc <- M - rowMeans(M)
    v <- vapply(seq_len(nrow(M)), function(j) {
      cov_i <- as.numeric(Nc %*% Mc[j, ]) / Bt
      sum(cov_i^2) - (nt / Bt^2) * sum(Mc[j, ]^2)
    }, numeric(1))
    v[v < 0] <- 0
    v
  }

  ref <- ij_ref(M, inbag)
  # chunk < n_pred exercises the chunked path
  got <- rf_infinitesimal_jackknife_var(M, inbag, chunk = 3L)
  expect_equal(got, ref, tolerance = 1e-10)
  expect_length(got, n_pred)
  expect_true(all(got >= 0))
})

test_that("rf_infinitesimal_jackknife_var is 0 when all trees agree, NA with <2 trees", {
  const_M <- matrix(rep(2.5, 5 * 10), nrow = 5)   # every tree identical => no spread
  inbag <- matrix(1L, nrow = 8, ncol = 10)
  expect_equal(rf_infinitesimal_jackknife_var(const_M, inbag), rep(0, 5))
  expect_true(all(is.na(rf_infinitesimal_jackknife_var(matrix(1, 3, 1), matrix(1, 4, 1)))))
})

test_that("RFK jackknife changes only the uncertainty surface, not predictions", {
  pts <- make_test_points(15)
  pts$v <- pts$v + 0.5 * pts$aux1
  grid <- make_test_grid_safe(pts, res = 200)
  lags <- calc_scientific_lags(pts)

  set.seed(123)
  res_spread <- suppressWarnings(
    apply_RFK(pts, "v", grid, lags, list(rf_ntree = 80, rfk_uncertainty = "spread"), c("aux1"))
  )
  set.seed(123)
  res_jack <- suppressWarnings(
    apply_RFK(pts, "v", grid, lags, list(rf_ntree = 80, rfk_uncertainty = "jackknife"), c("aux1"))
  )

  # Same seed => identical forest => identical prediction surface.
  expect_equal(res_spread$res_sf$var1.pred, res_jack$res_sf$var1.pred, tolerance = 1e-8)
  # Uncertainty surface differs, stays finite and non-negative.
  expect_false(isTRUE(all.equal(res_spread$res_sf$var1.var, res_jack$res_sf$var1.var)))
  expect_true(all(res_jack$res_sf$var1.var >= 0, na.rm = TRUE))
  expect_match(res_jack$log_msg, "infinitesimal jackknife")
})

# ── apply_CK ──────────────────────────────────────────────────────────────

test_that("apply_CK returns predictions labelled Co-Kriging or OK fallback", {
  pts <- make_test_points(15)
  pts$v <- pts$v + 0.5 * pts$aux1   # correlated secondary for the LMC
  grid <- make_test_grid_safe(pts, res = 200)
  lags <- calc_scientific_lags(pts)
  res <- suppressWarnings(apply_CK(pts, "v", grid, lags, list(), c("aux1")))

  expect_s3_class(res$res_sf, "sf")
  expect_true(all(c("var1.pred", "model_type") %in% colnames(res$res_sf)))
  expect_true(all(res$res_sf$model_type %in%
                    c("Co-Kriging", "Ordinary Kriging (Fallback)")))
  # The prediction column must be renamed to the engine-agnostic var1.pred,
  # and an observed column perform_cv recognizes must be present (gstat.cv
  # returns plain "observed"), so CV metrics compute
  if (!is.null(res$cv_obj)) {
    expect_true("var1.pred" %in% names(res$cv_obj))
    expect_true(any(grepl("(^|\\.)observed$", names(res$cv_obj))))
    expect_false(is.na(res$cv_metrics$rmse))
  }
})

# ── apply_interpolation dispatcher ────────────────────────────────────────

test_that("apply_interpolation dispatches IDW identically to apply_IDW", {
  pts <- make_test_points(15)
  grid <- make_test_grid_safe(pts, res = 200)
  lags <- calc_scientific_lags(pts)
  mp <- list(idw_p = 2, idw_nmax = 12)
  res_d <- apply_interpolation(pts, "v", "IDW", grid, character(0), lags,
                               mp, "region", "act")
  res_i <- apply_IDW(pts, "v", grid, mp)
  expect_equal(res_d$res_sf$var1.pred, res_i$res_sf$var1.pred)
  expect_equal(res_d$cv_metrics, res_i$cv_metrics)
})

test_that("apply_interpolation returns error result for unknown method", {
  pts <- make_test_points(10)
  grid <- make_test_grid_safe(pts, res = 200)
  lags <- calc_scientific_lags(pts)
  res <- apply_interpolation(pts, "v", "NOPE", grid, character(0), lags,
                             list(), "region", "act")
  expect_null(res$res_sf)
  expect_null(res$cv_metrics)
  expect_match(res$log_msg, "Unknown interpolation method: NOPE")
})

test_that("pre-resolved VIF set (aux_kept) reproduces the engine's own gate exactly", {
  # run_regional_interpolation resolves the multicollinearity gate BEFORE the
  # covariates are kriged onto the grid (so dropped ones are never kriged) and
  # passes the surviving set down as method_params$aux_kept. That is a compute-
  # path change only: the gate sees the same frame either way, so the fitted
  # model and its predictions must be identical.
  pts <- make_test_points(30)
  pts$aux3 <- pts$aux1 * 2 + rnorm(30, 0, 1e-3)   # collinear with aux1
  grid <- make_test_grid_safe(pts, res = 200)
  lags <- calc_scientific_lags(pts)
  aux <- c("aux1", "aux2", "aux3")

  gate <- check_vif(sf::st_drop_geometry(pts)[, aux, drop = FALSE], threshold = 10)
  expect_gt(length(gate$dropped), 0)   # the fixture must actually exercise the gate

  # Engine recomputes the gate itself (aux_kept absent)
  res_self <- suppressWarnings(apply_interpolation(
    pts, "v", "RK", grid, aux, lags, list(cv_strategy = "loocv"), "region", "act", 10))
  # Gate pre-resolved upstream and handed down
  res_pre <- suppressWarnings(apply_interpolation(
    pts, "v", "RK", grid, aux, lags,
    list(cv_strategy = "loocv", aux_kept = gate$kept), "region", "act", 10))

  expect_false(is.null(res_self$res_sf))
  expect_equal(res_pre$res_sf$var1.pred, res_self$res_sf$var1.pred)
  expect_equal(res_pre$res_sf$var1.var, res_self$res_sf$var1.var)
  expect_equal(res_pre$cv_metrics$rmse, res_self$cv_metrics$rmse)
  # both paths report the same dropped covariates
  expect_match(res_pre$log_msg, "\\[VIF\\] Dropped")
  expect_match(res_self$log_msg, "\\[VIF\\] Dropped")
})

test_that("covariate engines without aux vars report the real cause, not 'unknown method'", {
  # Every covariate branch tests length(aux_vars) > 0, so RK/RFK/CK with no
  # covariates used to fall through to the unknown-method stop() and tell the
  # user RK was an unrecognised method. server_execution.R guards the UI path,
  # but the engine is publicly callable.
  pts <- make_test_points(10)
  grid <- make_test_grid_safe(pts, res = 200)
  lags <- calc_scientific_lags(pts)

  for (m in c("RK", "RFK", "CK")) {
    res <- apply_interpolation(pts, "v", m, grid, character(0), lags,
                               list(), "region", "act")
    expect_null(res$res_sf)
    expect_match(res$log_msg, "requires at least one auxiliary covariate")
    expect_false(grepl("Unknown interpolation method", res$log_msg))
  }
})

# ── raster_value_layer (PackedSpatRaster safety) ──────────────────────────

test_that("raster_value_layer reads packed and live rasters identically", {
  r <- terra::rast(nrows = 20, ncols = 20, vals = seq_len(400))
  names(r) <- "var1.pred"
  live <- raster_value_layer(r)
  packed <- raster_value_layer(terra::wrap(r))
  expect_equal(live, packed)
  expect_length(live, 400)
  # first-layer fallback when var1.pred is absent
  names(r) <- "something_else"
  expect_equal(raster_value_layer(r), live)
  expect_null(raster_value_layer(NULL))
  expect_null(raster_value_layer("not a raster"))
})

# ── RNG sandbox (shared by every seeded helper) ────────────────────────────

test_that("with_rng_sandbox is two-sided and with_seed is reproducible", {
  had <- exists(".Random.seed", envir = globalenv(), inherits = FALSE)
  keep <- if (had) get(".Random.seed", envir = globalenv(), inherits = FALSE) else NULL
  on.exit({
    if (!is.null(keep)) assign(".Random.seed", keep, envir = globalenv())
    else if (exists(".Random.seed", envir = globalenv(), inherits = FALSE)) rm(".Random.seed", envir = globalenv())
  }, add = TRUE)

  # Restore branch: a seeded draw inside leaves the caller's stream untouched.
  set.seed(4242); before <- .Random.seed
  x1 <- with_seed(99, runif(3))
  expect_identical(.Random.seed, before)
  expect_equal(with_seed(99, runif(3)), x1)

  # Remove branch: no .Random.seed before the call, none left behind after it
  # (the half-sided version of this sandbox used to leak a seeded stream into
  # whatever ran next in a fresh session or worker).
  if (exists(".Random.seed", envir = globalenv(), inherits = FALSE)) rm(".Random.seed", envir = globalenv())
  x2 <- with_seed(99, runif(3))
  expect_false(exists(".Random.seed", envir = globalenv(), inherits = FALSE))
  expect_equal(x2, x1)

  # The block is a promise evaluated in the CALLER's frame: assignments land
  # here, and the block's value is the wrapper's value.
  val <- with_rng_sandbox({ marker <- 7; set.seed(5); marker + length(runif(2)) })
  expect_equal(val, 9)
  expect_equal(marker, 7)
})

# ── calc_class_breaks (seeded, sampled classification breaks) ─────────────

test_that("calc_class_breaks is deterministic and seed-sandboxed", {
  set.seed(999)
  vv <- rnorm(20000, 50, 10)

  set.seed(1); b1 <- calc_class_breaks(vv, 5, "kmeans")
  set.seed(2); b2 <- calc_class_breaks(vv, 5, "kmeans")
  expect_equal(b1, b2)          # caller RNG state must not leak in
  expect_length(b1, 4)

  # jenks subsampling path (n > max_n) is deterministic too
  j1 <- calc_class_breaks(vv, 4, "jenks", max_n = 1000L)
  j2 <- calc_class_breaks(vv, 4, "jenks", max_n = 1000L)
  expect_equal(j1, j2)
  expect_length(j1, 3)
  expect_true(all(j1 > min(vv) & j1 < max(vv)))

  # caller's RNG state restored (two-sided sandbox)
  set.seed(77); before <- .Random.seed
  invisible(calc_class_breaks(vv, 5, "kmeans"))
  expect_identical(.Random.seed, before)

  expect_null(calc_class_breaks(c(1, 2), 5, "kmeans"))
})

test_that("calc_class_breaks emits no conditions (Jenks message regression)", {
  set.seed(999)
  vv <- rnorm(20000, 50, 10)
  # classInt's jenks signals a message() ("Use fisher instead...") that
  # suppressWarnings alone lets escape; a stray condition unwinding through
  # the classification_params reactive poisoned it and made Jenks styling
  # silently fall back to the continuous palette.
  expect_no_condition(calc_class_breaks(vv, 4, "jenks"))
  expect_no_condition(calc_class_breaks(vv, 4, "kmeans"))
  expect_length(calc_class_breaks(vv, 4, "jenks"), 3)
})

# ── top-level furrr worker items ──────────────────────────────────────────

test_that("autofit_vgm_item fits actual and predicted variograms per item", {
  pts <- make_test_points(30)
  coords <- sf::st_coordinates(pts)
  df_a <- data.frame(x = coords[, 1], y = coords[, 2], v = pts$v)
  df_p <- data.frame(x = coords[, 1], y = coords[, 2], v = pts$pv)

  res <- autofit_vgm_item(list(l = "LocA", act = df_a, pre = df_p), current_crs = 32633)
  expect_identical(res$l, "LocA")
  expect_s3_class(res$act$fit, "variogramModel")
  expect_s3_class(res$pre$fit, "variogramModel")
  expect_true(res$act$mod %in% c("Sph", "Exp", "Gau", "Mat"))

  # matches the same fit computed inline (the observer's former lambda body)
  sub_a <- validate_and_project_sf(sf::st_as_sf(df_a, coords = c("x", "y"), crs = 32633))
  sub_a <- sub_a[!duplicated(round(sf::st_coordinates(sub_a), 2)), ]
  lags <- calc_scientific_lags(sub_a)
  v_emp <- gstat::variogram(v ~ 1, sub_a, width = lags$width, cutoff = lags$cutoff)
  expect_equal(res$act$emp$gamma, v_emp$gamma)
  expect_equal(as.character(res$act$fit$model[2]),
               as.character(robust_vgm_fit(v_emp, sub_a$v)$model[2]))

  # no predicted data -> FAIL placeholder result, actual side unaffected
  res2 <- autofit_vgm_item(list(l = "LocB", act = df_a, pre = NULL), current_crs = 32633)
  expect_null(res2$pre$fit)
  expect_identical(res2$pre$mod, "FAIL")
})

test_that("tps_gcv_item returns a GCV curve and idw_opt_item an optimized power", {
  pts <- make_test_points(25)
  coords <- sf::st_coordinates(pts)
  df <- data.frame(x = coords[, 1], y = coords[, 2], v = pts$v)

  tps_res <- tps_gcv_item(list(l = "LocA", df = df), current_crs = 32633)
  expect_identical(tps_res$l, "LocA")
  expect_null(tps_res$err)
  expect_true(is.numeric(tps_res$best_lam))
  expect_true(is.data.frame(tps_res$gcv_data) && all(c("lambda", "gcv") %in% names(tps_res$gcv_data)))
  expect_true(all(tps_res$gcv_data$lambda > 0))

  idw_res <- idw_opt_item(list(l = "LocA", df = df), current_crs = 32633, idw_nmax_val = 12)
  expect_true(idw_res$best_f >= 0.5 && idw_res$best_f <= 5)

  # small-n guards
  small <- df[1:3, ]
  expect_identical(tps_gcv_item(list(l = "S", df = small), 32633)$best_lam, 0)
  expect_identical(idw_opt_item(list(l = "S", df = small), 32633, 12)$best_f, 2.0)
})

test_that("idw_opt_item and tps_gcv_item project geographic input to match the run CRS", {
  # Finding 1 (third external review): both optimizers searched on the raw
  # upload CRS. For a geographic upload that means degree distances (and, for
  # TPS, a degree-scaled unit box), while the actual run interpolates on
  # projected metres -- so the stored power / lambda disagreed with the run they
  # feed. They now project via validate_and_project_sf first, exactly like the
  # sibling autofit_vgm_item and the run pipeline, so the answer is invariant to
  # whether the same points arrive in a geographic or a projected CRS.
  set.seed(101)
  n   <- 40
  lon <- 13 + runif(n, -0.05, 0.05)                   # UTM zone 33N territory
  lat <- 52 + runif(n, -0.05, 0.05)
  v   <- 10 + 5 * lon + 3 * lat + rnorm(n, 0, 0.2)    # smooth spatial signal
  df_geo <- data.frame(x = lon, y = lat, v = v)

  # The projected twin: exactly what validate_and_project_sf yields for df_geo,
  # so both inputs reduce to the identical metric coordinate set.
  pts_proj <- validate_and_project_sf(
    sf::st_as_sf(df_geo, coords = c("x", "y"), crs = 4326))
  cc       <- sf::st_coordinates(pts_proj)
  df_proj  <- data.frame(x = cc[, 1], y = cc[, 2], v = v)
  utm_crs  <- sf::st_crs(pts_proj)

  # IDW power: identical whether supplied as geographic or projected.
  idw_geo  <- idw_opt_item(list(l = "L", df = df_geo),  current_crs = 4326,    idw_nmax_val = 12)$best_f
  idw_proj <- idw_opt_item(list(l = "L", df = df_proj), current_crs = utm_crs, idw_nmax_val = 12)$best_f
  expect_identical(idw_geo, idw_proj)

  # TPS lambda: same invariance (the unit-box normalization now runs on metres).
  # suppressWarnings muffles fields' benign "GCV minimum at endpoint" note that
  # a near-linear signal provokes; it is orthogonal to the CRS invariance tested.
  tps_geo  <- suppressWarnings(tps_gcv_item(list(l = "L", df = df_geo),  current_crs = 4326))$best_lam
  tps_proj <- suppressWarnings(tps_gcv_item(list(l = "L", df = df_proj), current_crs = utm_crs))$best_lam
  expect_equal(tps_geo, tps_proj)
})

test_that("idw_opt_item and tps_gcv_item dedup co-located points like the run pipeline", {
  # Review 2026-07-22: the run fits on dedup_valid_points() output, but the
  # optimizer workers searched on the raw na.omit()ed frame. A co-located twin
  # predicts its held-out partner at distance zero (an exact hit for every IDW
  # power) and triggers fields::Tps's replicate handling (shifted GCV curve),
  # so the stored parameter was optimized on a different point set than the
  # run that consumes it. Appending exact twins must now be a no-op.
  pts <- make_test_points(25)
  coords <- sf::st_coordinates(pts)
  df <- data.frame(x = coords[, 1], y = coords[, 2], v = pts$v)
  df_dup <- rbind(df, df[1:5, ])

  idw_clean <- idw_opt_item(list(l = "L", df = df),     current_crs = 32633, idw_nmax_val = 12)$best_f
  idw_dup   <- idw_opt_item(list(l = "L", df = df_dup), current_crs = 32633, idw_nmax_val = 12)$best_f
  expect_identical(idw_dup, idw_clean)

  tps_clean <- suppressWarnings(tps_gcv_item(list(l = "L", df = df),     current_crs = 32633))$best_lam
  tps_dup   <- suppressWarnings(tps_gcv_item(list(l = "L", df = df_dup), current_crs = 32633))$best_lam
  expect_equal(tps_dup, tps_clean)
})

test_that("interp_run_item forwards a run_params list into a full regional run", {
  pts <- make_test_points(15)
  coords <- sf::st_coordinates(pts)
  pts_data <- data.frame(x = coords[, 1], y = coords[, 2],
                         v = pts$v, pv = NA, Locality = "LocA")
  item <- list(l = "LocA", pts_data = pts_data,
               m_params = list(idw_p_act = 2, idw_p_pre = 2, idw_nmax = 12,
                               tps_lambda_act = -1, tps_lambda_pre = -1,
                               pre_fit_act = NULL, pre_fit_pre = NULL,
                               cv_strategy = "auto", rfk_uncertainty = "jackknife"))
  proj_root <- normalizePath(file.path(testthat::test_path(), "..", ".."), winslash = "/")
  run_params <- list(
    main_wd = proj_root, current_method = "IDW", current_crs = 32633,
    aux_vars = character(0), shp_bound = NULL, b_type = "wrapped",
    buff_mode = "dynamic", b_dist = 250, res_mode = "fixed", grid_res = 200,
    crs_sel = "EPSG:4326", comp_mode = FALSE, val_type = "actual",
    progress_dir_val = tempdir(), session_id_val = "test",
    cancel_file_val = NULL, vif_threshold = 10
  )
  res <- interp_run_item(item, run_params)
  expect_identical(res$l, "LocA")
  expect_false(grepl("Error", res$log_msg))
  expect_false(is.null(res$r_a))
})

test_that("CK applies the multicollinearity gate to its co-kriging system", {
  # apply_CK had NO gate: the Auto-Drop / Keep All threshold was threaded to
  # RK/RFK only, so collinear covariates still reached fit.lmc() -- which is
  # precisely what makes the LMC fit fail and drop the run into the silent
  # Ordinary Kriging fallback.
  set.seed(11)
  pts <- make_test_points(30)
  pts$aux3 <- pts$aux1 * 2 + rnorm(30, 0, 1e-3)   # collinear with aux1
  grid <- make_test_grid_safe(pts, res = 200)
  lags <- calc_scientific_lags(pts)
  aux <- c("aux1", "aux2", "aux3")

  gate <- check_vif(sf::st_drop_geometry(pts)[, aux, drop = FALSE], threshold = 10)
  expect_gt(length(gate$dropped), 0)   # the fixture must actually trip the gate

  res <- suppressWarnings(apply_CK(
    pts, "v", grid, lags,
    list(cv_strategy = "loocv", aux_kept = gate$kept), aux, "region", "act"))
  expect_match(res$log_msg, "\\[VIF\\] Dropped")
  # The dropped covariate must not appear in the fitted co-kriging system.
  if (!is.null(res$gstat_obj)) {
    expect_false(any(gate$dropped %in% names(res$gstat_obj$data)))
  }
})

test_that("apply_interpolation threads vif_threshold through to CK", {
  set.seed(12)
  pts <- make_test_points(30)
  pts$aux3 <- pts$aux1 * 2 + rnorm(30, 0, 1e-3)
  grid <- make_test_grid_safe(pts, res = 200)
  lags <- calc_scientific_lags(pts)
  aux <- c("aux1", "aux2", "aux3")

  auto_drop <- suppressWarnings(apply_interpolation(
    pts, "v", "CK", grid, aux, lags, list(cv_strategy = "loocv"),
    "region", "act", vif_threshold = 10))
  # Inf == the user's "Keep All" answer: no iterative pruning at all.
  keep_all <- suppressWarnings(apply_interpolation(
    pts, "v", "CK", grid, aux, lags, list(cv_strategy = "loocv"),
    "region", "act", vif_threshold = Inf))

  expect_match(auto_drop$log_msg, "\\[VIF\\] Dropped")
  expect_false(grepl("\\[VIF\\] Dropped", keep_all$log_msg))
})

test_that("engine parameter guards: absent IDW params and an NA TPS lambda", {
  pts <- make_test_points(20)
  grid <- make_test_grid_safe(pts, res = 200)

  # R2: apply_IDW is publicly callable, and apply_TPS's fallback depends on
  # these defaults existing rather than on a run_regional_interpolation
  # invariant holding.
  idw_res <- suppressWarnings(apply_IDW(pts, "v", grid, list(cv_strategy = "loocv")))
  expect_false(is.null(idw_res$res_sf))
  expect_true(any(is.finite(idw_res$res_sf$var1.pred)))

  # R1: `NA < 0` is NA, which errors the if() and silently sent the entire
  # surface down the IDW fallback. NA now means unset = Auto (GCV).
  tps_res <- suppressWarnings(apply_TPS(
    pts, "v", grid, list(cv_strategy = "loocv", tps_lambda = NA_real_)))
  expect_false(is.null(tps_res$res_sf))
  expect_false(grepl("TPS failed", tps_res$log_msg))
})

test_that("residual (Delta) raster carries only the prediction layer", {
  # r_a - r_p differenced the whole stack, so when both surfaces carried a
  # kriging-variance layer the exported residual GeoTIFF gained a
  # "difference of two kriging variances" band -- not a quantity that exists.
  # The viewer was safe (raster_value_layer picks var1.pred); the export was not.
  pts <- make_test_points(20)
  coords <- sf::st_coordinates(pts)
  pts_data <- data.frame(x = coords[, 1], y = coords[, 2],
                         v = pts$v, pv = pts$pv, Locality = "LocA")
  item <- list(l = "LocA", pts_data = pts_data,
               m_params = list(idw_p_act = 2, idw_p_pre = 2, idw_nmax = 12,
                               tps_lambda_act = -1, tps_lambda_pre = -1,
                               pre_fit_act = NULL, pre_fit_pre = NULL,
                               cv_strategy = "auto", rfk_uncertainty = "jackknife"))
  res <- suppressWarnings(run_regional_interpolation(
    item, "OK", 32633, character(0), NULL, "wrapped", "dynamic", 250,
    "fixed", 200, "EPSG:4326", TRUE, "actual"))

  expect_false(is.null(res$r_res))
  rr <- terra::unwrap(res$r_res)
  expect_equal(terra::nlyr(rr), 1)
  expect_identical(names(rr), "var1.pred")
  # ...and the OK surfaces it was built from DO carry a variance layer.
  expect_true("var1.var" %in% names(terra::unwrap(res$r_a)))
})

test_that("IDW and TPS surfaces carry no phantom variance band", {
  # gstat::idw() returns an all-NA `var1.var` next to var1.pred (verified under
  # the project's installed gstat), so rasterizing every returned field doubled
  # the size of every IDW surface and wrote a blank second band into the
  # exported GeoTIFF; the main session then registered two blank "Uncertainty
  # Map" export items off the same layer. Gate on the METHOD: apply_TPS's IDW
  # fallback returns a gstat idw object too.
  pts <- make_test_points(20)
  coords <- sf::st_coordinates(pts)
  pts_data <- data.frame(x = coords[, 1], y = coords[, 2],
                         v = pts$v, pv = pts$pv, Locality = "LocA")
  item <- list(l = "LocA", pts_data = pts_data,
               m_params = list(idw_p_act = 2, idw_p_pre = 2, idw_nmax = 12,
                               tps_lambda_act = -1, tps_lambda_pre = -1,
                               pre_fit_act = NULL, pre_fit_pre = NULL,
                               cv_strategy = "auto", rfk_uncertainty = "jackknife"))

  for (mth in c("IDW", "TPS")) {
    res <- suppressWarnings(run_regional_interpolation(
      item, mth, 32633, character(0), NULL, "wrapped", "dynamic", 250,
      "fixed", 200, "EPSG:4326", FALSE, "actual"))
    ra <- terra::unwrap(res$r_a)
    expect_equal(terra::nlyr(ra), 1)
    expect_identical(names(ra), "var1.pred")
  }

  # The single source of truth the pipeline, the export registry and the map
  # viewer all key on.
  expect_true(all(vapply(c("OK", "RK", "RFK", "CK"), method_has_variance, logical(1))))
  expect_false(method_has_variance("IDW"))
  expect_false(method_has_variance("TPS"))
  expect_false(method_has_variance(NULL))
  expect_false(method_has_variance(NA_character_))
  expect_false(method_has_variance(""))
})

test_that("a constant target names itself in the run warnings", {
  # A locality whose target has no variance produces a flat surface, an
  # is_fallback variogram and all-NA R2/NSE/CCC/RPD/RPIQ (those metrics are
  # ratios against the observed variance, so they are UNDEFINED here, not zero).
  # Without this the only signal was the amber "fallback model" banner, which
  # names the symptom. Message only: the run must still complete.
  pts <- make_test_points(20)
  coords <- sf::st_coordinates(pts)
  pts_data <- data.frame(x = coords[, 1], y = coords[, 2],
                         v = 7.5, pv = pts$pv, Locality = "LocA")
  item <- list(l = "LocA", pts_data = pts_data,
               m_params = list(idw_p_act = 2, idw_p_pre = 2, idw_nmax = 12,
                               tps_lambda_act = -1, tps_lambda_pre = -1,
                               pre_fit_act = NULL, pre_fit_pre = NULL,
                               cv_strategy = "auto", rfk_uncertainty = "jackknife"))

  tmp <- tempfile("const_tgt_")
  dir.create(tmp)
  old_progress_dir <- getOption("monolith_progress_dir")
  old_session_id  <- getOption("monolith_session_id")
  on.exit({
    options(monolith_progress_dir = old_progress_dir,
            monolith_session_id  = old_session_id)
    unlink(tmp, recursive = TRUE)
  }, add = TRUE)

  res <- suppressWarnings(run_regional_interpolation(
    item, "OK", 32633, character(0), NULL, "wrapped", "dynamic", 250,
    "fixed", 200, "EPSG:4326", FALSE, "actual",
    progress_dir_val = tmp, session_id_val = "const_tgt"))

  expect_false(is.null(res$r_a))
  wf <- file.path(tmp, "warn_const_tgt_LocA_act.txt")
  expect_true(file.exists(wf))
  warn_txt <- paste(readLines(wf), collapse = " ")
  expect_match(warn_txt, "no usable variance")
  expect_match(warn_txt, "undefined")
})

test_that("an emptied covariate screen names the cause instead of failing as RK", {
  # When the gate drops every covariate, aux_vars becomes character(0) and the
  # trend formula built from it is "`v` ~ ", which as.formula() rejects with
  # "attempt to use zero-length variable name" -- reported by the tryCatch as a
  # bare "RK failed". The locality still routes to the named OK fallback; only
  # the message changes.
  pts <- make_test_points(20)
  pts$covA <- 1.0   # both constant, so the gate's constant prune empties the set
  pts$covB <- 3.0
  grid <- make_test_grid_safe(pts, res = 200)
  lags <- calc_scientific_lags(pts)

  res <- suppressWarnings(apply_RK(pts, "v", grid, lags,
                                   list(cv_strategy = "loocv"),
                                   c("covA", "covB")))

  expect_false(is.null(res$res_sf))          # OK fallback produced a surface
  expect_match(res$log_msg, "covariate screen")
  expect_false(grepl("zero-length variable name", res$log_msg))

  # Co-Kriging does not die on an empty set (gstat fits a single-variable LMC
  # happily) -- it silently returns ordinary kriging labelled as Co-Kriging, so
  # it needs the same guard for a different reason.
  res_ck <- suppressWarnings(apply_CK(pts, "v", grid, lags,
                                      list(cv_strategy = "loocv"),
                                      c("covA", "covB")))
  expect_false(is.null(res_ck$res_sf))
  expect_match(res_ck$log_msg, "covariate screen")
})

test_that("a sole degenerate covariate is named in the run warnings", {
  # The multicollinearity gate needs >= 2 covariates, so a single constant
  # covariate reaches the engines ungated: RK aliases its coefficient and fits
  # an intercept-only trend rather than failing. The run must still complete,
  # the covariate must be passed through (dropping it would hard-error the
  # dispatch), and the warning file must name it so the degradation is not
  # silent.
  pts <- make_test_points(20)
  coords <- sf::st_coordinates(pts)
  pts_data <- data.frame(x = coords[, 1], y = coords[, 2],
                         v = pts$v, pv = pts$pv, covA = 1.0, Locality = "LocA")
  item <- list(l = "LocA", pts_data = pts_data,
               m_params = list(idw_p_act = 2, idw_p_pre = 2, idw_nmax = 12,
                               tps_lambda_act = -1, tps_lambda_pre = -1,
                               pre_fit_act = NULL, pre_fit_pre = NULL,
                               cv_strategy = "auto", rfk_uncertainty = "jackknife"))

  tmp <- tempfile("degen_cov_")
  dir.create(tmp)
  old_progress_dir <- getOption("monolith_progress_dir")
  old_session_id  <- getOption("monolith_session_id")
  on.exit({
    options(monolith_progress_dir = old_progress_dir,
            monolith_session_id  = old_session_id)
    unlink(tmp, recursive = TRUE)
  }, add = TRUE)

  res <- suppressWarnings(run_regional_interpolation(
    item, "RK", 32633, "covA", NULL, "wrapped", "dynamic", 250,
    "fixed", 200, "EPSG:4326", FALSE, "actual",
    progress_dir_val = tmp, session_id_val = "degen_cov"))

  # The run completed (RK degraded to an intercept-only trend, not a crash).
  expect_false(is.null(res$r_a))
  wf <- file.path(tmp, "warn_degen_cov_LocA_act.txt")
  expect_true(file.exists(wf))
  expect_match(paste(readLines(wf), collapse = " "), "covA")
  expect_match(paste(readLines(wf), collapse = " "), "constant")
})

# ── build_class_zone_sf ─────────────────────────────────────────────────────
# Class-zone polygons are the GIS form of what the map shows, so they must be
# the SAME classification and the SAME hectares the Area Coverage table reports.

make_zone_test_raster <- function() {
  r <- terra::rast(nrows = 10, ncols = 10,
                   xmin = 450000, xmax = 451000,
                   ymin = 5800000, ymax = 5801000,
                   crs = "EPSG:32633")
  # 30 cells below 40, 40 between 40 and 60, 30 above 60
  terra::values(r) <- c(rep(20, 30), rep(50, 40), rep(80, 30))
  r
}

make_zone_test_params <- function() {
  brks <- c(-Inf, 40, 60, Inf)
  list(
    brks = brks,
    rcl_mat = matrix(c(brks[1:3], brks[2:4], 1:3), ncol = 3),
    colors = c("#d73027", "#fee08b", "#1a9850"),
    labels = c("Low", "Med", "High"),
    leg_labels = c("< 40", "40 - 60", "> 60"),
    n_c = 3
  )
}

test_that("build_class_zone_sf returns one dissolved polygon per class present", {
  z <- build_class_zone_sf(make_zone_test_raster(), make_zone_test_params())
  expect_s3_class(z, "sf")
  expect_equal(nrow(z), 3)
  expect_equal(z$class, c("Low", "Med", "High"))
  expect_true(all(sf::st_geometry_type(z) %in% c("POLYGON", "MULTIPOLYGON")))
})

test_that("class zone areas equal the areas the Area Coverage table reports", {
  r <- make_zone_test_raster()
  z <- build_class_zone_sf(r, make_zone_test_params())

  # Same call calc_area_df makes: classify, then expanse by value.
  r_class <- terra::classify(r, make_zone_test_params()$rcl_mat, right = FALSE)
  ref <- as.data.frame(terra::expanse(r_class, unit = "ha", byValue = TRUE))
  ref <- ref[order(as.numeric(as.character(ref$value))), ]

  expect_equal(z$area_ha, round(ref$area, 2))
  # 100 x 100 m cells, so the three classes split the grid 30/40/30. terra
  # measures on the ellipsoid rather than in grid units (expanse transforms a
  # planar CRS for accuracy), which is why these are ~30.02 rather than 30.00 -
  # the app's own area table carries the same correction.
  expect_equal(z$area_ha, c(30, 40, 30), tolerance = 0.01)
  expect_equal(sum(z$area_ha), 100, tolerance = 0.01)
})

test_that("build_class_zone_sf keeps the analysis CRS and carries provenance", {
  z <- build_class_zone_sf(make_zone_test_raster(), make_zone_test_params(),
                           labels = make_zone_test_params()$leg_labels,
                           surface = "Predicted", variable = "pH", method = "OK")
  expect_true(sf::st_crs(z) == sf::st_crs(32633))
  expect_equal(z$class, c("< 40", "40 - 60", "> 60"))
  expect_true(all(z$surface == "Predicted"))
  expect_true(all(z$variable == "pH"))
  expect_true(all(z$method == "OK"))
})

test_that("open outer breaks are written as NA, never as an infinity", {
  # A GIS attribute field cannot hold -Inf/Inf, and the outer breaks always are.
  z <- build_class_zone_sf(make_zone_test_raster(), make_zone_test_params())
  expect_true(is.na(z$class_min[1]))
  expect_true(is.na(z$class_max[3]))
  expect_equal(z$class_max[1], 40)
  expect_equal(z$class_min[3], 60)
  expect_false(any(is.infinite(c(z$class_min, z$class_max)), na.rm = TRUE))
})

test_that("build_class_zone_sf accepts a packed raster and rejects junk", {
  z <- build_class_zone_sf(terra::wrap(make_zone_test_raster()), make_zone_test_params())
  expect_equal(nrow(z), 3)

  expect_null(build_class_zone_sf(NULL, make_zone_test_params()))
  expect_null(build_class_zone_sf(make_zone_test_raster(), NULL))
  expect_null(build_class_zone_sf(make_zone_test_raster(), list(brks = c(0, 1))))
})

test_that("a surface holding only one class yields one zone, not an error", {
  r <- make_zone_test_raster()
  terra::values(r) <- rep(50, 100)
  z <- build_class_zone_sf(r, make_zone_test_params())
  expect_equal(nrow(z), 1)
  expect_equal(z$class, "Med")
  expect_equal(z$area_ha, 100, tolerance = 0.01)
})

test_that("an all-NA surface yields NULL rather than an empty layer", {
  r <- make_zone_test_raster()
  terra::values(r) <- NA_real_
  expect_null(build_class_zone_sf(r, make_zone_test_params()))
})

test_that("a boundary that encloses no grid node skips the locality with a named warning", {
  # A coarse fixed resolution over a tight strict boundary can leave NO
  # candidate node inside it; the engines then kriged the full bbox and the
  # mask discarded every cell -- a blank locality with no message, after
  # paying for the whole interpolation. The run now names the cause and skips
  # (classif_build_grid already stops loudly in the same situation).
  pts_data <- data.frame(x = c(0, 100, 200), y = c(0, 100, 0),
                         v = c(1.2, 3.4, 2.1), pv = NA, Locality = "LocA")
  item <- list(l = "LocA", pts_data = pts_data,
               m_params = list(idw_p_act = 2, idw_p_pre = 2, idw_nmax = 12,
                               tps_lambda_act = -1, tps_lambda_pre = -1,
                               pre_fit_act = NULL, pre_fit_pre = NULL,
                               cv_strategy = "auto", rfk_uncertainty = "jackknife"))

  # 1 m point buffers, 500 m fixed grid: every node centre sits tens of metres
  # from the nearest sample, so nothing intersects the boundary.
  res <- suppressWarnings(run_regional_interpolation(
    item, "IDW", 32633, character(0), NULL, "strict", "fixed", 1,
    "fixed", 500, "EPSG:4326", FALSE, "actual"))
  expect_null(res$r_a)
  expect_true(grepl("no grid cells fall inside the boundary", res$log_msg))
  expect_false(grepl("Error in", res$log_msg))

  # Positive control: a 100 m buffer guarantees a node within reach of
  # (100, 100) at 50 m spacing under ANY grid alignment (nearest node centre
  # is at most ~36 m away), so the guard must not trip on ordinary runs.
  res_ok <- suppressWarnings(run_regional_interpolation(
    item, "IDW", 32633, character(0), NULL, "strict", "fixed", 100,
    "fixed", 50, "EPSG:4326", FALSE, "actual"))
  expect_false(is.null(res_ok$r_a))
  expect_false(grepl("no grid cells fall inside the boundary", res_ok$log_msg))
})

# ── Strict boundary vs grid resolution coherence ─────────────────────────────

test_that("strict_buffer_gap quantifies a buffer below half the cell diagonal", {
  # A sample sits anywhere in its cell, so it is up to res/sqrt(2) from that
  # cell's CENTRE -- the only point the boundary clip tests. A 175 m buffer on
  # a 350 m grid therefore needs 247.5 m to guarantee coverage, and the share
  # of in-cell positions that lose their cell is the cell area outside the
  # inscribed circle: 1 - pi/4.
  g <- strict_buffer_gap(175, 350)
  expect_true(g$short)
  expect_equal(g$req_buffer, 350 / sqrt(2))
  expect_equal(g$req_res, 175 * sqrt(2))
  expect_equal(g$fraction, 1 - pi / 4)

  # Above res/2 the buffer disc spills over the cell edges, so the inscribed
  # -circle area overstates coverage: 1 - pi*b^2/res^2 would report NO loss
  # from b = res/sqrt(pi) = 197.5 m upwards, inside the flagged range. The
  # references below match Monte-Carlo sampling of a uniformly placed sample
  # (4e6 draws) to within its standard error.
  expect_equal(strict_buffer_gap(190, 350)$fraction, 0.1229089, tolerance = 1e-5)
  expect_equal(strict_buffer_gap(200, 350)$fraction, 0.0809532, tolerance = 1e-5)
  expect_equal(strict_buffer_gap(240, 350)$fraction, 0.0018500, tolerance = 1e-4)
  # Continuous and strictly decreasing, reaching zero exactly at the threshold:
  # a flagged pair therefore always carries a non-zero loss.
  fr <- vapply(seq(1, 247, by = 2), function(b) strict_buffer_gap(b, 350)$fraction,
               numeric(1))
  expect_true(all(diff(fr) < 0))
  expect_true(all(fr > 0))
  expect_equal(strict_buffer_gap(350 / sqrt(2), 350)$fraction, 0, tolerance = 1e-6)

  # Exactly half the diagonal is coherent; anything wider loses nothing.
  expect_false(strict_buffer_gap(350 / sqrt(2), 350)$short)
  expect_false(strict_buffer_gap(400, 350)$short)
  expect_equal(strict_buffer_gap(400, 350)$fraction, 0)

  # A zero buffer loses every cell; unusable inputs stay silent.
  expect_equal(strict_buffer_gap(0, 350)$fraction, 1)
  expect_null(strict_buffer_gap(175, 0))
  expect_null(strict_buffer_gap(NA, 350))
  expect_null(strict_buffer_gap(175, NULL))
})

test_that("strict_buffer_message speaks only for an incoherent pair", {
  msg <- strict_buffer_message(175, 350, label = "Yorga")
  expect_match(msg, "^Yorga: Strict Measured buffer")
  expect_match(msg, "248 m or more")   # ceiling(350 / sqrt(2))
  expect_match(msg, "247 m or less")   # floor(175 * sqrt(2))
  expect_match(msg, "21%")             # 100 * (1 - pi/4)
  expect_false(grepl(": ", strict_buffer_message(175, 350), fixed = TRUE))

  # Just inside the threshold the loss is a genuine fraction of a percent;
  # rounding it to "0%" would contradict the warning carrying it.
  expect_match(strict_buffer_message(240, 350), "under 1% of isolated samples",
               fixed = TRUE)

  # Both manual-resolution sliders stop at 5 m, so a corrective cell size the
  # user could not set is left out and only the buffer arm remains.
  expect_match(strict_buffer_message(3, 5), "Raise the buffer to 4 m or more\\.$")
  expect_false(grepl("lower the resolution", strict_buffer_message(3, 5),
                     fixed = TRUE))

  expect_null(strict_buffer_message(250, 350))
  expect_null(strict_buffer_message(175, NA))
})

test_that("run_regional_interpolation names an incoherent strict buffer", {
  pts_data <- data.frame(x = c(0, 100, 200, 300, 150), y = c(0, 100, 0, 150, 250),
                         v = c(1.2, 3.4, 2.1, 2.8, 1.9), pv = NA, Locality = "LocA")
  item <- list(l = "LocA", pts_data = pts_data,
               m_params = list(idw_p_act = 2, idw_p_pre = 2, idw_nmax = 12,
                               tps_lambda_act = -1, tps_lambda_pre = -1,
                               pre_fit_act = NULL, pre_fit_pre = NULL,
                               cv_strategy = "auto", rfk_uncertainty = "jackknife"))

  # 100 m point buffers on a 350 m grid: below the 247.5 m half-diagonal, so
  # isolated samples lose their own cell. Advisory only -- the run proceeds.
  res <- suppressWarnings(run_regional_interpolation(
    item, "IDW", 32633, character(0), NULL, "strict", "fixed", 100,
    "fixed", 350, "EPSG:4326", FALSE, "actual"))
  expect_match(res$log_msg, "Strict Measured buffer")
  expect_false(grepl("Error in", res$log_msg))

  # A coherent pair (100 m buffer, 50 m grid) must stay silent.
  res_ok <- suppressWarnings(run_regional_interpolation(
    item, "IDW", 32633, character(0), NULL, "strict", "fixed", 100,
    "fixed", 50, "EPSG:4326", FALSE, "actual"))
  expect_false(grepl("Strict Measured buffer", res_ok$log_msg))

  # Non-strict boundaries are never flagged: a hull covers the neighbourhood of
  # every interior sample, so only its perimeter is exposed - and a dynamic
  # wrapped buffer is 1-3x the cell size, always past the half-diagonal.
  res_wrap <- suppressWarnings(run_regional_interpolation(
    item, "IDW", 32633, character(0), NULL, "wrapped", "fixed", 100,
    "fixed", 350, "EPSG:4326", FALSE, "actual"))
  expect_false(grepl("Strict Measured buffer", res_wrap$log_msg))
})
