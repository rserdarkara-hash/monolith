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
                      "cv_obj", "residuals")
  expect_setequal(names(res), expected_names)
  expect_equal(res$log_msg, "")
  expect_null(res$v_emp)
  expect_null(res$res_sf)
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

test_that("apply_interpolation treats RK without aux vars as an error", {
  # Current contract: covariate methods require aux_vars; with none supplied
  # the dispatcher falls through to the unknown-method error
  pts <- make_test_points(10)
  grid <- make_test_grid_safe(pts, res = 200)
  lags <- calc_scientific_lags(pts)
  res <- apply_interpolation(pts, "v", "RK", grid, character(0), lags,
                             list(), "region", "act")
  expect_null(res$res_sf)
  expect_match(res$log_msg, "Unknown interpolation method: RK")
})
