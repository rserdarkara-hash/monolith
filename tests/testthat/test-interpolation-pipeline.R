# test-interpolation-pipeline.R — tests for init_interpolation_res,
# sanitize_spatial_predictions, safe_run_cv, suggest_lmc_model,
# calc_scientific_lags, merge_wrapped_rasters, get_joint_scale_values,
# and validate_and_project_sf.

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
