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
                      "cv_obj", "cv_obj_reps")
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
  res <- safe_run_cv(res, stop("forced error"), "TEST")
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
  res <- safe_run_cv(res, cv_df, "TEST_OK")
  expect_false(is.null(res$cv_obj))
  expect_false(is.null(res$cv_metrics))
  expect_false(is.na(res$cv_metrics$rmse))
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

# ── class areas: per-locality sum vs the merged grid ──────────────────────
# The Area Coverage total is summed over the per-locality surfaces rather than
# computed on merge_wrapped_rasters()'s output, because terra::expanse() costs
# O(total cells) - NA padding included - and two localities far apart share a
# merged grid that is almost entirely padding. This pins the equality that
# makes the cheap route the same answer.

test_that("class areas of disjoint localities sum to the merged surface's", {
  mk <- function(x0, y0) {
    r <- terra::rast(xmin = x0, xmax = x0 + 400, ymin = y0, ymax = y0 + 400,
                     resolution = 20, crs = "EPSG:32636")
    terra::values(r) <- seq(0, 10, length.out = terra::ncell(r))
    names(r) <- "var1.pred"
    r
  }
  # 5 km apart: the merged grid is mostly NA, which is the case at issue
  a <- mk(500000, 4300000)
  b <- mk(505000, 4304000)

  rcl <- matrix(c(-Inf, 3, 1,
                  3,    7, 2,
                  7,  Inf, 3), ncol = 3, byrow = TRUE)

  class_ha <- function(r) {
    e <- as.data.frame(terra::expanse(terra::classify(r[[1]], rcl, right = FALSE),
                                      unit = "ha", byValue = TRUE))
    ha <- rep(0, 3)
    ha[as.numeric(as.character(e$value))] <- e$area
    ha
  }

  # One raster comes back unwrapped and untouched - there is nothing to merge,
  # so neither the geometry nor a single value may move.
  one <- merge_wrapped_rasters(list(terra::wrap(a)))
  expect_s4_class(one, "SpatRaster")
  expect_equal(terra::ext(one), terra::ext(a))
  expect_identical(names(one), names(a))
  expect_equal(terra::values(one), terra::values(a))

  merged <- merge_wrapped_rasters(list(terra::wrap(a), terra::wrap(b)))
  expect_s4_class(merged, "SpatRaster")
  expect_gt(terra::ncell(merged), 4 * (terra::ncell(a) + terra::ncell(b)))

  # 1e-6 relative, not exact: terra computes each cell's geodesic area from its
  # own transformed corners, and a cell sits in a different row of the merged
  # grid than of its locality's. Measured at 1.1e-7 relative, which is ~3e-5 ha
  # on the areas here - four orders below the table's 2-decimal display.
  per_locality <- class_ha(a) + class_ha(b)
  expect_equal(per_locality, class_ha(merged), tolerance = 1e-6)
  expect_equal(sum(per_locality), sum(class_ha(merged)), tolerance = 1e-6)
})

# ── merge_wrapped_rasters ─────────────────────────────────────────────────

test_that("merge_wrapped_rasters returns NULL for empty or NULL input", {
  expect_null(merge_wrapped_rasters(NULL))
  expect_null(merge_wrapped_rasters(list()))
  expect_null(merge_wrapped_rasters(list(NULL, NULL)))
})

# ── get_joint_scale_values ────────────────────────────────────────────────

test_that("get_joint_scale_values returns NULL when match_scales is FALSE", {
  expect_null(get_joint_scale_values(NULL, NULL, match_scales = FALSE))
  expect_null(get_joint_scale_values(NULL, NULL, match_scales = FALSE, layer = "se"))
})

test_that("get_joint_scale_values pools the band the view shows", {
  mk <- function(pred, var) {
    r <- terra::rast(nrows = 2, ncols = 2, xmin = 0, xmax = 2, ymin = 0, ymax = 2,
                     crs = "EPSG:32633", nlyrs = 2)
    names(r) <- c("var1.pred", "var1.var")
    terra::values(r) <- cbind(rep(pred, 4), rep(var, 4))
    terra::wrap(r)
  }
  a <- mk(10, 4); p <- mk(20, 9)
  expect_setequal(get_joint_scale_values(a, p, TRUE), c(10, 20))
  expect_setequal(get_joint_scale_values(a, p, TRUE, "var"), c(4, 9))
  expect_setequal(get_joint_scale_values(a, p, TRUE, "se"), c(2, 3))

  # A surface without a variance band contributes nothing to an uncertainty
  # scale, never its predictions.
  pred_only <- terra::rast(nrows = 2, ncols = 2, xmin = 0, xmax = 2, ymin = 0, ymax = 2,
                           crs = "EPSG:32633", vals = 50)
  names(pred_only) <- "var1.pred"
  expect_setequal(get_joint_scale_values(a, terra::wrap(pred_only), TRUE, "se"), 2)
  expect_null(raster_value_layer(terra::wrap(pred_only), "var1.var"))
  expect_equal(unique(raster_value_layer(terra::wrap(pred_only))), 50)
})

# ── Map Viewer views ──────────────────────────────────────────────────────

test_that("parse_map_view splits a view id into surfaces and layer", {
  expect_equal(parse_map_view("view_pred_se"), list(base = "view_pred", layer = "se"))
  expect_equal(parse_map_view("view_comp_var"), list(base = "view_comp", layer = "var"))
  expect_equal(parse_map_view("view_act"), list(base = "view_act", layer = "value"))
  # The residual view has no variance band; unknown or missing ids fall back.
  expect_equal(parse_map_view("view_resid_se"), list(base = "view_resid", layer = "value"))
  expect_equal(parse_map_view(NULL), list(base = "view_act", layer = "value"))
  expect_equal(parse_map_view("nonsense"), list(base = "view_act", layer = "value"))
})

test_that("map_view_choices offers SE and variance views for variance methods only", {
  plain <- map_view_choices(has_pred = TRUE, has_resid = TRUE, has_variance = FALSE)
  expect_false(any(grepl("_se$|_var$", unlist(plain))))
  expect_setequal(unname(plain), c("view_act", "view_pred", "view_comp", "view_resid"))

  krig <- map_view_choices(has_pred = TRUE, has_resid = TRUE, has_variance = TRUE)
  ids <- unlist(krig, use.names = FALSE)
  expect_setequal(ids, c("view_act", "view_pred", "view_comp", "view_resid",
                         "view_act_se", "view_pred_se", "view_comp_se",
                         "view_act_var", "view_pred_var", "view_comp_var"))
  # Every id the menu offers parses back to itself.
  for (v in ids) {
    pv <- parse_map_view(v)
    expect_equal(if (pv$layer == "value") pv$base else paste0(pv$base, "_", pv$layer), v)
  }
  actual_only <- unlist(map_view_choices(FALSE, FALSE, TRUE), use.names = FALSE)
  expect_setequal(actual_only, c("view_act", "view_act_se", "view_act_var"))
})

test_that("uncertainty maps never take a diverging palette", {
  expect_equal(uncertainty_palette("RdYlBu"), "viridis")
  expect_equal(uncertainty_palette("Spectral"), "viridis")
  expect_equal(uncertainty_palette("BrBG"), "viridis")
  expect_equal(uncertainty_palette("YlOrRd"), "YlOrRd")
  expect_equal(uncertainty_palette("viridis"), "viridis")
})

# ── Grid template and Auto (Global) resolution ────────────────────────────

test_that("grid_template gives square cells of exactly the requested size", {
  bb <- c(xmin = 500000, ymin = 4200000, xmax = 501000, ymax = 4200800)
  # terra::rast(ext, resolution = 45) would stretch these to 45.45 x 44.44 m.
  g <- grid_template(bb, 45, "EPSG:32635")
  expect_equal(terra::res(g), c(45, 45))
  e <- unname(as.vector(terra::ext(g)))
  expect_equal(e[c(1, 3)], c(500000, 4200000))
  expect_true(e[2] >= 501000 && e[2] - 501000 < 45)
  expect_true(e[4] >= 4200800 && e[4] - 4200800 < 45)

  # Snapped grids of two boxes share one lattice.
  g2 <- grid_template(c(xmin = 503017, ymin = 4201333, xmax = 504100, ymax = 4202000),
                      45, "EPSG:32635", snap = TRUE)
  g1 <- grid_template(bb, 45, "EPSG:32635", snap = TRUE)
  on_lattice <- function(v) isTRUE(all.equal(v / 45, round(v / 45)))
  expect_true(all(vapply(as.vector(terra::ext(g1)), on_lattice, logical(1))))
  expect_true(all(vapply(as.vector(terra::ext(g2)), on_lattice, logical(1))))
  expect_equal(terra::res(g2), c(45, 45))
})

test_that("Auto (Global) grids every locality at the Auto size of the largest boundary", {
  proj_root <- normalizePath(file.path(testthat::test_path(), "..", ".."), winslash = "/")
  mk_item <- function(l, n, x0, span, seed) {
    set.seed(seed)
    pts_data <- data.frame(x = x0 + runif(n, 0, span), y = 4400000 + runif(n, 0, span),
                           v = rnorm(n, 10, 2), pv = NA, Locality = l)
    list(l = l, pts_data = pts_data,
         m_params = list(idw_p_act = 2, idw_p_pre = 2, idw_nmax = 12,
                         tps_lambda_act = -1, tps_lambda_pre = -1,
                         pre_fit_act = NULL, pre_fit_pre = NULL,
                         cv_strategy = "auto", rfk_uncertainty = "jackknife"))
  }
  items <- list(mk_item("Small", 20, 500000, 300, 1), mk_item("Large", 30, 520000, 3000, 2))
  rp <- list(main_wd = proj_root, current_method = "IDW", current_crs = 32635,
             aux_vars = character(0), shp_bound = NULL, b_type = "convex",
             buff_mode = "dynamic", b_dist = 250, res_mode = "global", grid_res = 50,
             crs_sel = "EPSG:32635", comp_mode = FALSE, val_type = "actual",
             progress_dir_val = tempdir(), session_id_val = "global_res",
             cancel_file_val = NULL, vif_threshold = 10)

  # Reference from the definition: the Auto size of the larger convex hull.
  areas <- vapply(items, function(it) {
    p <- sf::st_as_sf(it$pts_data, coords = c("x", "y"), crs = 32635)
    as.numeric(sf::st_area(sf::st_convex_hull(sf::st_union(p))))
  }, numeric(1))
  expected <- max(5, min(1000, sqrt(max(areas) / 1e5)))
  shared <- shared_auto_resolution(items, rp)
  expect_equal(shared, expected)

  rp$shared_res <- shared
  runs <- lapply(items, interp_run_item, run_params = rp)
  for (r in runs) {
    expect_false(grepl("Error", r$log_msg))
    expect_equal(r$actual_res, expected)
    ras <- terra::unwrap(r$r_a)
    expect_equal(terra::res(ras), c(expected, expected), tolerance = 1e-6)
    ex <- as.vector(terra::ext(ras))
    expect_equal(ex / expected, round(ex / expected), tolerance = 1e-6)
  }

  # Per Locality keeps each boundary's own Auto size.
  rp_local <- rp; rp_local$res_mode <- "local"; rp_local$shared_res <- NULL
  small_local <- interp_run_item(items[[1]], rp_local)
  expect_equal(small_local$actual_res, max(5, min(1000, sqrt(areas[1] / 1e5))))
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
  # The CV object holds one row per sample; residuals are read off it.
  expect_equal(nrow(res$cv_obj), 15)
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
  expect_equal(sf::st_drop_geometry(on$cv_obj), sf::st_drop_geometry(off$cv_obj))

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

test_that("TPS and its optimizer preserve geometry under rotation", {
  d <- with_seed(11, {
    x <- runif(80, 0, 4000); y <- runif(80, 0, 1000)
    data.frame(x = x, y = y, v = sin(x / 700) + cos(y / 300) + rnorm(80, 0, 0.05))
  })
  rotated <- d
  rotated$x <- (d$x - d$y) / sqrt(2)
  rotated$y <- (d$x + d$y) / sqrt(2)
  mk <- function(z) sf::st_as_sf(z, coords = c("x", "y"), crs = 32633)
  fits <- lapply(list(d, rotated), function(z) {
    p <- mk(z)
    suppressWarnings(apply_TPS(p, "v", p, list(tps_lambda = -1)))
  })
  expect_equal(fits[[1]]$res_sf$var1.pred, fits[[2]]$res_sf$var1.pred, tolerance = 1e-6)
  for (i in 1:2) {
    z <- list(d, rotated)[[i]]
    opt <- suppressWarnings(tps_gcv_item(list(l = "R", df = z), 32633))
    p <- mk(z)
    fixed <- suppressWarnings(apply_TPS(p, "v", p, list(tps_lambda = opt$best_lam)))
    expect_equal(fixed$res_sf$var1.pred, fits[[1]]$res_sf$var1.pred, tolerance = 1e-6)
    expect_equal(fits[[i]]$tps_fit$lambda, opt$best_lam)
  }
})

test_that("TPS reports fitted smoothing and a qualified near-plane warning", {
  pts <- golden_sf("full", localities = "Tavas")
  dir <- tempfile("tps_report_"); dir.create(dir)
  withr::defer(unlink(dir, recursive = TRUE))
  withr::local_options(monolith_progress_dir = dir, monolith_session_id = "report")
  res <- suppressWarnings(apply_TPS(pts, "ph", pts, list(tps_lambda = -1), "Tavas"))
  expect_type(res$tps_fit$lambda, "double")
  expect_lt(res$tps_fit$eff_df, 3.5)
  expect_match(res$log_msg, "near-planar")
  warnings <- list.files(dir, pattern = "^warn_", full.names = TRUE)
  expect_gt(length(warnings), 0)
  expect_match(paste(unlist(lapply(warnings, readLines)), collapse = " "), "GCV")
  fixed <- suppressWarnings(apply_TPS(pts, "ph", pts, list(tps_lambda = 1e8), "Tavas"))
  expect_match(fixed$log_msg, "Fixed lambda")
  expect_false(grepl("GCV", fixed$log_msg))
})

test_that("locality overlap is excess coverage in square metres", {
  sq <- sf::st_as_sfc(sf::st_bbox(c(xmin = 500000, xmax = 500100,
                                    ymin = 4500000, ymax = 4500100), crs = sf::st_crs(32633)))
  far <- sf::st_set_crs(sq + c(1000, 0), 32633)
  expect_equal(locality_boundary_overlap(list(sq, sq)), 10000)
  expect_equal(locality_boundary_overlap(list(sq, sq, sq)), 20000)
  expect_equal(locality_boundary_overlap(list(sq, far)), 0)
  expect_equal(locality_boundary_overlap(list(sq)), 0)
  expect_true(is.na(locality_boundary_overlap(list(sq, NULL))))
  expect_true(is.na(locality_boundary_overlap(list(NULL, NULL))))
  expect_equal(locality_boundary_overlap(list(sq, sf::st_transform(far, 4326))), 0)
  feet <- sf::st_transform(sq, 2263)
  expect_equal(locality_boundary_overlap(list(feet, feet)),
               as.numeric(units::set_units(sf::st_area(feet), "m^2")))
})

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
  expect_equal(nrow(res$cv_obj), 20)
  expect_identical(names(res$cv_obj), KRIGING_CV_SCHEMA)
})

test_that("apply_OK honours the Spatial Block CV strategy end-to-end", {
  pts <- make_test_points(40) # >= CV_BLOCK_MIN_N so blocks are real
  grid <- make_test_grid_safe(pts, res = 200)
  lags <- calc_scientific_lags(pts)
  res <- suppressWarnings(apply_OK(pts, "v", grid, lags, list(cv_strategy = "block")))

  # Block folds (a length-n integer vector) must flow through the fold runner
  # and still yield finite CV metrics over all 40 held-out points.
  expect_false(is.na(res$cv_metrics$rmse))
  expect_equal(nrow(res$cv_obj), 40)
  expect_identical(names(res$cv_obj), KRIGING_CV_SCHEMA)
  expect_s3_class(res$res_sf, "sf")
})

test_that("variogram stamps preserve manual provenance and require the current key", {
  fit <- make_mock_vgm("Sph")
  manual <- stamp_vgm(fit, "ph [subset Test]", "manual")
  expect_identical(attr(manual, "monolith_key"), "ph [subset Test]")
  expect_identical(attr(manual, "monolith_source"), "manual")
  expect_identical(attr(stamp_vgm(manual, "ph [subset Test]", "run"), "monolith_source"), "manual")
  expect_identical(attr(stamp_vgm(fit, "ph", "autofit"), "monolith_source"), "autofit")
})

test_that("stored variograms are supplied only when applied manually for the run key", {
  fit <- make_mock_vgm("Sph")
  attr(fit, "monolith_key") <- "ph"
  attr(fit, "monolith_source") <- "manual"
  attr(fit, "formula") <- ph ~ 1
  attr(fit, "call") <- quote(manual_vgm())
  expect_identical(resolve_stored_vgm(fit, "manual", "ph"), clean_gstat_env(fit))
  expect_null(resolve_stored_vgm(fit, "auto", "ph"))
  expect_null(resolve_stored_vgm(fit, "manual", "som"))
  expect_null(resolve_stored_vgm(fit, "manual", NA_character_))
  attr(fit, "monolith_key") <- NULL
  expect_null(resolve_stored_vgm(fit, "manual", "ph"))
  attr(fit, "monolith_key") <- "ph"
  for (source in c("autofit", "run")) {
    attr(fit, "monolith_source") <- source
    expect_null(resolve_stored_vgm(fit, "manual", "ph"))
  }
  expect_null(resolve_stored_vgm(NULL, "manual", "ph"))
})

test_that("variogram key matching rejects absent and unequal keys", {
  fit <- make_mock_vgm("Sph")
  expect_false(vgm_key_matches(fit, "ph"))
  attr(fit, "monolith_key") <- "ph"
  expect_true(vgm_key_matches(fit, "ph"))
  expect_false(vgm_key_matches(fit, "ph [subset Test]"))
  expect_false(vgm_key_matches(fit, NA_character_))
  expect_false(vgm_key_matches(NULL, "ph"))
})

test_that("OK worker shares the measured variogram only when separate fitting is off", {
  pts <- make_test_points(15)
  xy <- sf::st_coordinates(pts)
  df <- data.frame(x = xy[, 1], y = xy[, 2], v = pts$v,
                   pv = 100 + 20 * pts$v, Locality = "LocA")
  run <- function(separate, fit = NULL) suppressWarnings(run_regional_interpolation(
    list(l = "LocA", pts_data = df, m_params = list(sep_fit = separate, pre_fit_act = fit)),
    "OK", 32633, character(0), NULL, "convex", "dynamic", 250,
    "fixed", 200, "EPSG:32633", FALSE, "pred"))
  shared <- run(FALSE)
  expect_false(is.null(shared$v_fit_act))
  expect_identical(shared$v_fit_pre, shared$v_fit_act)
  separate <- run(TRUE)
  expect_false(isTRUE(all.equal(separate$v_fit_pre, separate$v_fit_act)))
  applied <- make_mock_vgm("Sph")
  fixed <- run(FALSE, applied)
  expect_identical(fixed$v_fit_act, applied)
  expect_identical(fixed$v_fit_pre, applied)
})

test_that("TPS shares the measured values' smoothing only when separate fitting is off", {
  # The TPS counterpart of the variogram sharing above: unticked, a lambda on
  # Auto is the one GCV selects for the MEASURED values of the same rows, so
  # the Predicted surface reuses the model fitted to the measured values.
  set.seed(21)
  pts <- make_test_points(20)
  xy <- sf::st_coordinates(pts)
  df <- data.frame(x = xy[, 1], y = xy[, 2], v = pts$v,
                   pv = 100 + 20 * pts$v + rnorm(20, 0, 40), Locality = "LocA")
  run <- function(separate) suppressWarnings(run_regional_interpolation(
    list(l = "LocA", pts_data = df,
         m_params = list(sep_fit = separate, tps_lambda_act = -1, tps_lambda_pre = -1,
                         idw_p_act = 2, idw_p_pre = 2, idw_nmax = 12, cv_strategy = "loocv")),
    "TPS", 32633, character(0), NULL, "convex", "dynamic", 250,
    "fixed", 200, "EPSG:32633", FALSE, "pred"))
  shared <- run(FALSE)
  expect_equal(shared$tps_fit_pre$lambda, shared$tps_fit_act$lambda)

  # Engine level: the shared lambda depends on the measured values only, so
  # changing only the predicted values leaves it where they put it.
  grid <- make_test_grid_safe(pts, res = 200)
  mp <- list(cv_strategy = "loocv", tps_lambda = -1, tps_gcv_col = "v")
  a <- suppressWarnings(apply_TPS(pts, "pv", grid, mp))
  pts2 <- pts; pts2$pv <- pts2$pv * 3 + rnorm(nrow(pts2))
  b <- suppressWarnings(apply_TPS(pts2, "pv", grid, mp))
  own_v <- suppressWarnings(apply_TPS(pts, "v", grid, list(cv_strategy = "loocv", tps_lambda = -1)))
  expect_equal(a$tps_fit$lambda, own_v$tps_fit$lambda)
  expect_equal(b$tps_fit$lambda, a$tps_fit$lambda)
  # A fixed lambda is shared at dispatch; the column does not touch it.
  fixed <- suppressWarnings(apply_TPS(pts, "pv", grid, list(cv_strategy = "loocv", tps_lambda = 0.01,
                                                            tps_gcv_col = "v")))
  expect_equal(fixed$tps_fit$lambda, 0.01)
})

test_that("the Comparable CV population scores exactly the rows RK scores", {
  # OK maps every sample with a measured target, but a comparison with RK is
  # only a comparison when both are scored on the same information. The
  # Comparable switch folds OK over the covariate-complete rows RK uses -
  # including the deduplication, which is where the two populations can pick
  # DIFFERENT members of a co-located pair: the pair's first row has no aux1,
  # so OK Native keeps it and the covariate-complete population keeps the
  # second.
  set.seed(404)
  n <- 30
  crd <- cbind(500000 + runif(n, 0, 3000), 4000000 + runif(n, 0, 3000))
  crd[2, ] <- crd[1, ]                     # co-located pair: rows 1 and 2
  aux <- 10 + runif(n, 0, 5)
  aux[c(1, 7, 12, 18, 25)] <- NA_real_     # 5 rows without the covariate
  df <- data.frame(.mn_row_id = seq_len(n), x = crd[, 1], y = crd[, 2],
                   v = 20 + 0.8 * (crd[, 1] - 500000) / 300 + rnorm(n, 0, 0.5),
                   pv = NA_real_, aux1 = aux, Locality = "LocA")

  run <- function(method, population) suppressWarnings(run_regional_interpolation(
    list(l = "LocA", pts_data = df,
         m_params = list(cv_strategy = "auto", cv_population = population,
                         idw_p_act = 2, idw_nmax = 12)),
    method, 32633, "aux1", NULL, "convex", "fixed", 300,
    "fixed", 300, "EPSG:32633", FALSE, "actual"))

  ok_nat <- run("OK", "native")
  ok_cmp <- run("OK", "comparable")
  rk     <- run("RK", "native")            # RK always uses the common rows
  # IDW scores its own point set with its own folds, so no kriging population
  # is folded for it and it carries no population to name or hash.
  idw    <- run("IDW", "native")
  expect_true(is.na(idw$cv_info_act$population))
  expect_null(idw$cv_info_act$row_id)
  # The worker carries each surface's covariate record back to the run
  # configuration; an engine that models no covariate carries none.
  expect_equal(rk$aux_used_act, "aux1")
  expect_equal(rk$aux_dropped_act, character(0))
  expect_null(ok_cmp$aux_used_act)

  # Same rows, same partition, therefore the same experiment id.
  expect_identical(ok_cmp$cv_info_act$row_id, rk$cv_info_act$row_id)
  expect_identical(ok_cmp$cv_info_act$folds1, rk$cv_info_act$folds1)
  expect_identical(
    cv_population_id("v", ok_cmp$cv_info_act$row_id, ok_cmp$cv_info_act$folds1),
    cv_population_id("v", rk$cv_info_act$row_id, rk$cv_info_act$folds1))

  # 30 rows, one co-located duplicate: OK Native scores 29. The covariate-
  # complete population loses the five aux1-less rows but recovers the pair's
  # second row, which the Native dedup had discarded.
  expect_equal(ok_nat$cv_info_act$n_expected, 29)
  expect_equal(ok_cmp$cv_info_act$n_expected, 25)
  expect_gt(ok_nat$cv_info_act$n_expected, ok_cmp$cv_info_act$n_expected)
  expect_true(2L %in% ok_cmp$cv_info_act$row_id)
  expect_false(1L %in% ok_cmp$cv_info_act$row_id)
  expect_true(1L %in% ok_nat$cv_info_act$row_id)
  expect_identical(ok_cmp$cv_info_act$population, "common rows")
  expect_identical(ok_nat$cv_info_act$population, "native rows")

  # The residual write-back follows the CV object's own coordinates, so a
  # Comparable run marks the samples it actually scored - not the first 25
  # rows of a longer point set.
  ckey <- function(sfo) {
    cc <- sf::st_coordinates(sfo)
    paste(round(cc[, 1], 2), round(cc[, 2], 2))
  }
  scored <- ckey(ok_cmp$cv_obj_act)[!is.na(ok_cmp$cv_obj_act$residual)]
  got <- ckey(ok_cmp$pts)[!is.na(ok_cmp$pts$model_resid_act)]
  expect_setequal(got, scored)
  expect_equal(length(got), length(scored))
  idx <- match(ckey(ok_cmp$cv_obj_act), ckey(ok_cmp$pts))
  expect_equal(ok_cmp$pts$model_resid_act[idx], ok_cmp$cv_obj_act$residual)

  # The row-identity column is internal and must never reach the popups or the
  # colour-by choices, which read rv$sf (= res$pts) column by column.
  expect_false(".mn_row_id" %in% names(ok_cmp$pts))
  expect_false(".mn_row_id" %in% names(rk$pts))
})

test_that("a CV population too small to fold skips the CV and still draws the map", {
  # A Comparable population can be far smaller than the surface's point set.
  # Below three samples there is nothing to fold: the cross-validation is
  # skipped with a named log line rather than quietly scored on OK's own,
  # larger, Native population - and the map, fitted on every sample, still runs.
  set.seed(606)
  n <- 20
  crd <- cbind(500000 + runif(n, 0, 2000), 4000000 + runif(n, 0, 2000))
  aux <- rep(NA_real_, n)
  aux[1:2] <- c(3, 9)
  df <- data.frame(.mn_row_id = seq_len(n), x = crd[, 1], y = crd[, 2],
                   v = 20 + rnorm(n), pv = NA_real_, aux1 = aux, Locality = "LocA")

  res <- suppressWarnings(run_regional_interpolation(
    list(l = "LocA", pts_data = df,
         m_params = list(cv_strategy = "auto", cv_population = "comparable",
                         idw_p_act = 2, idw_nmax = 12)),
    "OK", 32633, "aux1", NULL, "convex", "fixed", 300,
    "fixed", 300, "EPSG:32633", FALSE, "actual"))

  expect_false(is.null(res$r_a))
  expect_null(res$cv_obj_act)
  expect_true(is.na(res$cv_act$rmse))
  expect_match(res$log_msg, "cross-validation population (common rows) holds 2 samples",
               fixed = TRUE)
  expect_true(all(is.na(res$pts$model_resid_act)))
})

test_that("a locality is projected once, before the covariate filter", {
  # D4f: the working CRS is a property of the locality, not of the covariate
  # selection. This fixture straddles the 12 deg E UTM zone boundary so that
  # ALL rows average into zone 32 while the covariate-complete rows average
  # into zone 33 - projecting after the filter would put RK's samples on a
  # different grid from OK's, which is exactly what the Comparable switch is
  # meant to rule out.
  set.seed(505)
  lon <- c(runif(20, 10.5, 11.0), runif(32, 12.1, 12.5))
  lat <- c(runif(20, 45.0, 45.4), runif(32, 45.0, 45.4))
  aux <- c(rep(NA_real_, 20), 5 + runif(32, 0, 3))
  n <- length(lon)
  df <- data.frame(.mn_row_id = seq_len(n), x = lon, y = lat,
                   v = 30 + 2 * (lon - 10.5) + rnorm(n, 0, 0.3),
                   pv = NA_real_, aux1 = aux, Locality = "LocA")

  all_sf <- sf::st_as_sf(df, coords = c("x", "y"), crs = 4326)
  proj_all <- validate_and_project_sf(all_sf)
  proj_filtered_first <- validate_and_project_sf(all_sf[!is.na(all_sf$aux1), ])
  # The fixture only tests anything while the two orders disagree.
  expect_false(sf::st_crs(proj_all) == sf::st_crs(proj_filtered_first))

  run <- function(method, population) suppressWarnings(run_regional_interpolation(
    list(l = "LocA", pts_data = df,
         m_params = list(cv_strategy = "block", cv_population = population,
                         idw_p_act = 2, idw_nmax = 12)),
    method, 4326, "aux1", NULL, "convex", "fixed", 2000,
    "fixed", 5000, "EPSG:4326", FALSE, "actual"))

  ok_cmp <- run("OK", "comparable")
  rk     <- run("RK", "native")

  expected <- sf::st_coordinates(proj_all[!is.na(proj_all$aux1), ])
  expect_equal(sf::st_coordinates(rk$cv_obj_act), expected)
  expect_equal(sf::st_coordinates(ok_cmp$cv_obj_act), expected)
  expect_identical(ok_cmp$cv_info_act$folds1, rk$cv_info_act$folds1)
  expect_identical(ok_cmp$cv_info_act$row_id, rk$cv_info_act$row_id)
})

test_that("stored regional values belong only to their tuning key", {
  expect_equal(resolve_regional_param(list(value = 3.5, key = "ph"), "ph", 2), 3.5)
  expect_equal(resolve_regional_param(list(value = 0.25, key = "ph [subset Test]"), "ph [subset Test]", -1), 0.25)
  for (entry in list(NULL, 4, list(value = 4), list(value = 4, key = NA_character_),
                     list(value = 4, key = "som"), list(value = 4, key = "ph [subset Test]"))) {
    expect_equal(resolve_regional_param(entry, "ph", 2), 2)
  }
  expect_equal(resolve_regional_param(list(value = 4, key = NA_character_), NA_character_, -1), -1)
})

test_that("the run record separates the two CRS and selected from used covariates", {
  # Two runs that fitted different models (a covariate dropped in one
  # locality) used to export identical records, and the record named only the
  # Input Data CRS.
  res_all <- list(
    list(l = "Kale", aux_used_act = c("elev", "ndvi"), aux_dropped_act = "clay",
         aux_used_pre = c("elev", "clay", "ndvi"), aux_dropped_pre = character(0)),
    list(l = "Tavas", aux_used_act = character(0), aux_dropped_act = c("elev", "clay")),
    list(l = "Yorga"))                       # a locality with no modelled surface
  rec <- covariate_screen_record(res_all)
  expect_equal(rec$retained$Kale, list(actual = "elev, ndvi", predicted = "elev, clay, ndvi"))
  expect_equal(rec$dropped$Kale$actual, "clay")
  expect_equal(rec$retained$Tavas, list(actual = ""))
  expect_null(rec$retained$Yorga)

  cfg <- list(run_id = 7L, method = "RK", app_version = "9.9.9",
              input_crs = "EPSG:4326", target_crs = "EPSG:32633",
              covariates_selected = "elev, clay, ndvi",
              covariates_retained = rec$retained, covariates_dropped = rec$dropped)
  txt <- covariate_record_text(cfg)
  expect_match(txt, "Covariates selected: elev, clay, ndvi", fixed = TRUE)
  expect_match(txt, "Kale (actual): elev, ndvi (removed: clay)", fixed = TRUE)
  expect_match(txt, "Tavas: none (Ordinary Kriging fallback) (removed: elev, clay)", fixed = TRUE)
  same <- covariate_screen_record(list(list(l = "A", aux_used_act = "elev", aux_dropped_act = "clay"),
                                       list(l = "B", aux_used_act = "elev", aux_dropped_act = "clay")))
  cfg_same <- cfg
  cfg_same$covariates_retained <- same$retained
  cfg_same$covariates_dropped <- same$dropped
  expect_match(covariate_record_text(cfg_same), "used: elev | removed by the screen: clay", fixed = TRUE)

  # Written and read back, the record keeps both CRS fields and the per-
  # locality covariate lists; the kriging engines consume no IDW power or TPS
  # lambda, so none is written for them.
  params <- list(Kale = list(idw_p_act = 2, idw_p_pre = 2.5, tps_lambda_act = -1, tps_lambda_pre = -1))
  back <- jsonlite::fromJSON(jsonlite::toJSON(run_record_payload(cfg, params, "9.9.9", list(sf = "1")),
                                              auto_unbox = TRUE, null = "null", force = TRUE),
                             simplifyVector = FALSE)
  expect_equal(back$config$input_crs, "EPSG:4326")
  expect_equal(back$config$target_crs, "EPSG:32633")
  expect_null(back$config$crs)
  expect_equal(back$config$covariates_dropped$Kale$actual, "clay")
  expect_null(back$regional_params)
  idw <- run_record_payload(modifyList(cfg, list(method = "IDW")), params, "9.9.9", list())
  expect_equal(names(idw$regional_params$Kale), c("idw_p_act", "idw_p_pre"))
  tps <- run_record_payload(modifyList(cfg, list(method = "TPS")), params, "9.9.9", list())
  expect_equal(names(tps$regional_params$Kale), c("tps_lambda_act", "tps_lambda_pre"))

  # Load Config reads session configurations. A run record - recent, or an
  # older one with a single `crs` field - is refused by name, never "loaded"
  # as nothing.
  expect_null(session_config_refusal(list(map_x = "x", map_crs = "EPSG:32633")))
  expect_match(session_config_refusal(back), "run record .*Monolith 9\\.9\\.9")
  legacy <- list(config = list(crs = "EPSG:32633", app_version = "1.1.0"), provenance = list())
  expect_match(session_config_refusal(legacy), "Monolith 1.1.0", fixed = TRUE)
})

test_that("manual parameter targets follow the switch in every prediction view", {
  for (view in c("pred", "pred_ss", "resid")) {
    expect_identical(manual_param_target(FALSE, view, "pre"), "pre")
    expect_identical(manual_param_target(FALSE, view, "act"), "act")
  }
  expect_identical(manual_param_target(TRUE, "actual", "pre"), "pre")
  expect_identical(manual_param_target(FALSE, "actual", "pre"), "act")
  expect_identical(manual_param_target(FALSE, "pred", NULL), "act")
  # "Fit Actual/Predicted separately" unticked: the Predicted surface reuses
  # the Actual parameter, so tuning writes the Actual slot only.
  expect_identical(manual_param_target(TRUE, "pred", "pre", sep_fit = FALSE), "act")
})

test_that("supplied OK CV retains gstat predictions and variances in the common schema", {
  pts <- golden_sf("tiny")
  lags <- calc_scientific_lags(pts)
  fit <- golden_pin_vgm()
  for (strategy in c("loocv", "block")) {
    res <- suppressWarnings(apply_OK(pts, "ph", pts[1:5, ], lags,
      list(pre_fit = fit, cv_strategy = strategy)))
    expected <- gstat::krige.cv(ph ~ 1, pts, model = fit,
      nfold = make_cv_folds(sf::st_coordinates(pts), strategy, nrow(pts)), debug.level = 0)
    expect_identical(names(res$cv_obj), c("row_id", "fold", "observed", "var1.pred", "var1.var", "residual", "geometry"))
    expect_equal(res$cv_obj$var1.pred, expected$var1.pred, tolerance = 1e-10)
    expect_equal(res$cv_obj$var1.var, expected$var1.var, tolerance = 1e-10)
    expect_equal(res$cv_obj$observed, pts$ph)
  }
})

test_that("kriging engines and the OK fallback share a poolable CV schema", {
  pts <- golden_sf("tiny"); lags <- calc_scientific_lags(pts); grid <- pts[1:5, ]
  results <- suppressWarnings(list(
    apply_OK(pts, "ph", grid, lags, list()),
    apply_OK(pts, "ph", grid, lags, list(pre_fit = golden_pin_vgm())),
    apply_RK(pts, "ph", grid, lags, list(grid_aux = data.frame(x = 1:5)), "v82"),
    apply_RK(pts, "ph", grid, lags, list(), "v82"),
    apply_RFK(pts, "ph", grid, lags, list(rf_ntree = 200), "v82"),
    apply_CK(pts, "ph", grid, lags, list(), "v82")
  ))
  expect_match(results[[3]]$log_msg, "RK failed")
  expect_false(grepl("Falling back to OK", results[[6]]$log_msg, fixed = TRUE))
  for (res in results) {
    expect_identical(names(res$cv_obj), c("row_id", "fold", "observed", "var1.pred", "var1.var", "residual", "geometry"))
    expect_equal(nrow(res$cv_obj), nrow(pts))
  }
  frames <- lapply(results, function(res) res$cv_obj)
  tps <- cv_repeat_frame(frames[[1]])
  tps$x <- sf::st_coordinates(tps)[, 1]; tps$y <- sf::st_coordinates(tps)[, 2]
  expect_equal(nrow(pool_cv_sf(c(frames, list(tps)))), 7 * nrow(pts))
})

test_that("kriging CV uses a supplied population and validates its plan", {
  pts <- make_test_points(12)
  pop <- pts[1:8, ]; pop$.mn_row_id <- 101:108
  plan <- build_cv_plan(pop, "loocv")
  expect_identical(.engine_cv_plan(list(cv_data = pop, cv_plan = plan), pts)$plan, plan)
  bad <- plan; bad$n <- bad$n + 1L
  expect_error(.engine_cv_plan(list(cv_data = pop, cv_plan = bad), pts), "CV plan population size")
  res <- suppressWarnings(apply_OK(pts, "v", pts[1:3, ], calc_scientific_lags(pts),
    list(cv_data = pop, cv_plan = plan, pre_fit = make_mock_vgm("Sph"))))
  expect_identical(res$cv_obj$row_id, 101:108)
  expect_equal(res$cv_metrics$n, nrow(pop))
})

test_that("a kriging CV that fails outright reports none of its expected samples", {
  # Coverage is predicted over expected. A cross-validation that never produced
  # an object scored none of its population, which is 0 of n, not 0 of 0.
  pts <- make_test_points(12)
  res <- .run_kriging_cv(init_interpolation_res(),
    function(pop, folds, row_id, progress) stop("forced CV failure"),
    list(cv_strategy = "loocv"), pts, "OK", "test", "act")
  expect_null(res$cv_obj)
  expect_match(res$log_msg, "OK CV Error: forced CV failure", fixed = TRUE)
  expect_equal(res$cv_metrics$n, 0)
  expect_equal(res$cv_metrics$n_expected, nrow(pts))
  expect_equal(res$cv_metrics$coverage, 0)
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
  # An applied manual model encodes the user's judgement and cannot be
  # refitted, so it is reused in every fold and the CV is labelled conditional.
  expect_identical(res$cv_conditional, "applied variogram")
})

test_that("an OK fold refits its variogram from its own training rows", {
  # The held-out row's own measured value must not help fit the model that
  # predicts it. Raising only the last row's target must therefore leave its
  # out-of-fold prediction where it was.
  pts <- golden_sf("tiny")
  n <- nrow(pts)
  grid <- pts[1:5, ]
  lags <- calc_scientific_lags(pts)
  raised <- pts
  raised$ph[n] <- raised$ph[n] + 10 * sd(pts$ph)
  run <- function(p) suppressWarnings(apply_OK(p, "ph", grid, lags, list(cv_strategy = "loocv")))
  base <- run(pts); moved <- run(raised)
  expect_true(is.finite(base$cv_obj$var1.pred[n]))
  expect_equal(moved$cv_obj$var1.pred[n], base$cv_obj$var1.pred[n], tolerance = 1e-8)
})

test_that("a shared measured-value variogram is refitted inside every fold", {
  # "Fit Actual/Predicted Separately" unticked: the Predicted surface is kriged
  # with the variogram of the MEASURED values. Its CV must refit that variogram
  # from each fold's training rows, so a measured value at a held-out location
  # cannot reach that location's own prediction.
  pts <- golden_sf("tiny")
  n <- nrow(pts)
  set.seed(5)
  pts$v <- pts$ph
  pts$pv <- pts$ph + rnorm(n, 0, 0.05)
  grid <- pts[1:5, ]
  lags <- calc_scientific_lags(pts)
  shared_of <- function(p) suppressWarnings(robust_vgm_fit(
    gstat::variogram(v ~ 1, p, width = lags$width, cutoff = lags$cutoff), p$v))
  raised <- pts
  raised$v[n] <- raised$v[n] + 10 * sd(pts$v)
  expect_false(isTRUE(all.equal(shared_of(pts), shared_of(raised))))

  run <- function(p) suppressWarnings(apply_OK(p, "pv", grid, lags,
    list(shared_fit = shared_of(p), vgm_col = "v", cv_strategy = "loocv")))
  base <- run(pts); moved <- run(raised)
  expect_identical(base$cv_vgm_col, "v")
  expect_true(is.finite(base$cv_obj$var1.pred[n]))
  expect_equal(moved$cv_obj$var1.pred[n], base$cv_obj$var1.pred[n], tolerance = 1e-8)
})

test_that("the OK fallback refits its variogram inside every fold", {
  pts <- make_test_points(12)
  n <- nrow(pts)
  grid <- make_test_grid_safe(pts, res = 200)
  lags <- calc_scientific_lags(pts)
  # A grid_aux without the covariate column makes predict.lm fail, which is the
  # documented route into the named OK fallback.
  bad_grid_aux <- sf::st_drop_geometry(grid)[, c("x", "y")]
  run <- function(p) suppressWarnings(apply_RK(p, "v", grid, lags,
    list(grid_aux = bad_grid_aux, cv_strategy = "loocv"), c("aux1")))
  raised <- pts
  raised$v[n] <- raised$v[n] + 10 * sd(pts$v)
  base <- run(pts); moved <- run(raised)
  expect_match(base$log_msg, "RK failed")
  expect_true(is.finite(base$cv_obj$var1.pred[n]))
  expect_equal(moved$cv_obj$var1.pred[n], base$cv_obj$var1.pred[n], tolerance = 1e-8)
})

test_that("a CK fold refits its LMC and standardization from its own rows", {
  # Nothing about the held-out row — not its target, not its covariate, not
  # their contribution to the standardization means — may reach its own
  # prediction.
  pts <- golden_sf("tiny")
  n <- nrow(pts)
  grid <- pts[1:5, ]
  lags <- calc_scientific_lags(pts)
  raised <- pts
  raised$ph[n] <- raised$ph[n] + 10 * sd(pts$ph)
  raised$v82[n] <- raised$v82[n] + 10 * sd(pts$v82)
  run <- function(p) suppressWarnings(apply_CK(p, "ph", grid, lags,
    list(cv_strategy = "loocv"), "v82"))
  base <- run(pts); moved <- run(raised)
  expect_false(grepl("Falling back to OK", base$log_msg, fixed = TRUE))
  expect_true(is.finite(base$cv_obj$var1.pred[n]))
  expect_equal(moved$cv_obj$var1.pred[n], base$cv_obj$var1.pred[n], tolerance = 1e-8)
})

test_that("a CK fold screens its covariates on its own training rows", {
  # Same A/B construction as the RK screen test: the full-data screens differ,
  # the last row's fold-training rows do not.
  set.seed(7)
  n <- 20L
  a1 <- rnorm(n)
  base_pts <- sf::st_as_sf(data.frame(
    x = runif(n, 450000, 451000), y = runif(n, 5800000, 5801000),
    aux1 = a1, aux2 = a1 + rnorm(n, 0, 0.25),
    v = 3 * a1 + rnorm(n, 0, 0.5)
  ), coords = c("x", "y"), crs = 32633)
  a <- base_pts
  b <- base_pts; b$aux2[n] <- b$aux2[n] + 5
  expect_length(screen_covariates(a, c("aux1", "aux2"))$kept, 1)
  expect_length(screen_covariates(b, c("aux1", "aux2"))$kept, 2)

  grid <- make_test_grid_safe(base_pts, res = 400)
  lags <- calc_scientific_lags(base_pts)
  run <- function(p) suppressWarnings(apply_CK(p, "v", grid, lags,
    list(cv_strategy = "loocv"), c("aux1", "aux2")))
  res_a <- run(a); res_b <- run(b)
  expect_true(is.finite(res_a$cv_obj$var1.pred[n]))
  expect_equal(res_b$cv_obj$var1.pred[n], res_a$cv_obj$var1.pred[n], tolerance = 1e-8)
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
  expect_equal(nrow(res$cv_obj), 15)
  expect_identical(names(res$cv_obj), KRIGING_CV_SCHEMA)
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

test_that("apply_RFK grows one forest per LOOCV fold, all at the requested ntree", {
  # The rf_ntree the user set has to reach every forest the run fits, not just
  # the main one: a fold loop that quietly dropped the argument, or stopped
  # calling randomForest at all and fell back to OK, would leave the map and
  # its cross-validation describing different models.
  captured <- list()
  orig_rf <- randomForest::randomForest
  mock_rf <- function(...) {
    args <- list(...)
    captured <<- c(captured, list(args))
    do.call(orig_rf, args)
  }

  pts <- make_test_points(n = 12)          # 3 <= n <= 50, so auto CV is LOOCV
  grid <- make_test_grid_safe(pts, res = 200)
  lags <- calc_scientific_lags(pts)
  target_ntree <- 37                       # distinct from every default

  testthat::with_mocked_bindings(
    suppressWarnings(apply_RFK(
      data = pts, target_var = "v", grid_p = grid, lags = lags,
      method_params = list(rf_ntree = target_ntree),
      aux_vars = c("aux1", "aux2"))),
    randomForest = mock_rf,
    .package = "randomForest"
  )

  # One main model plus one per held-out row. Asserted exactly: a fold loop
  # that stopped fitting forests leaves two calls, which `> 1` accepts.
  expect_equal(length(captured), nrow(pts) + 1L)
  for (i in seq_along(captured)) {
    expect_true("ntree" %in% names(captured[[i]]), info = paste("call", i))
    expect_equal(captured[[i]]$ntree, target_ntree, info = paste("call", i))
  }
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

test_that("RFK grid trend prediction is invariant to the prediction block size", {
  # The grid trend is predicted in row blocks so predict.all cannot allocate an
  # n_grid x ntree matrix in one shot. A forest predicts each row independently
  # and both variance estimators are row-independent, so shrinking the block must
  # not move a single value - if it does, the blocking is not exact.
  pts <- make_test_points(15)
  pts$v <- pts$v + 0.5 * pts$aux1
  grid <- make_test_grid_safe(pts, res = 200)
  lags <- calc_scientific_lags(pts)
  expect_gt(nrow(grid), 3)   # otherwise the block override below is a no-op

  run_rfk <- function(unc) {
    set.seed(123)
    suppressWarnings(
      apply_RFK(pts, "v", grid, lags, list(rf_ntree = 40, rfk_uncertainty = unc), c("aux1"))
    )
  }

  for (unc in c("jackknife", "spread")) {
    one_block <- run_rfk(unc)

    orig <- .RFK_PREDICT_BLOCK_CELLS
    withr::defer(assign(".RFK_PREDICT_BLOCK_CELLS", orig, envir = globalenv()))
    # 2 rows per block => several blocks over this grid.
    assign(".RFK_PREDICT_BLOCK_CELLS", 2 * 40, envir = globalenv())
    many_blocks <- run_rfk(unc)
    assign(".RFK_PREDICT_BLOCK_CELLS", orig, envir = globalenv())

    expect_identical(many_blocks$res_sf$var1.pred, one_block$res_sf$var1.pred)
    expect_identical(many_blocks$res_sf$var1.var, one_block$res_sf$var1.var)
  }
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
  # CK cross-validation returns the common kriging CV schema, so CV metrics
  # compute and pooling works whichever path the locality took.
  if (!is.null(res$cv_obj)) {
    expect_identical(names(res$cv_obj), c("row_id", "fold", "observed", "var1.pred", "var1.var", "residual", "geometry"))
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

test_that("OK keeps a supplied model that gives an empty surface and names the cause", {
  # A zero-nugget Gaussian over near-coincident samples makes the kriging
  # covariance matrix singular, and gstat does NOT raise there: it returns NA
  # for every location. A nugget small enough to leave the model unchanged
  # does not make the system stable (on the reference data a nugget of 1e-6 of
  # the sill still put predictions up to 33 spans outside the observed range),
  # so the model is kept, the empty surface reaches run_regional_interpolation's
  # skip path, and the log names the cause.
  set.seed(19)
  n <- 40
  # A 1 km extent under a 900 m Gaussian range: the near-degenerate regime a
  # smooth, low-noise variable produces routinely (reproduces across seeds).
  x <- runif(n, 450000, 451000); y <- runif(n, 5800000, 5801000)
  for (i in 1:8) { x[2 * i] <- x[2 * i - 1] + 0.011; y[2 * i] <- y[2 * i - 1] }
  pts <- sf::st_as_sf(data.frame(x = x, y = y, v = rnorm(n, 50, 10)),
                      coords = c("x", "y"), crs = 32633)
  # The 2 dp dedup keeps these: 11 mm apart is distinct at centimetre rounding.
  expect_equal(nrow(pts[!duplicated(round(sf::st_coordinates(pts), 2)), ]), n)

  grid <- make_test_grid_safe(pts, res = 400)
  lags <- calc_scientific_lags(pts)
  zero_nug <- gstat::vgm(psill = 1, model = "Gau", range = 900, nugget = 0)

  # Confirm the premise on this fixture before asserting the repair.
  bare <- gstat::krige(v ~ 1, pts, grid, model = zero_nug, debug.level = 0)
  expect_true(all(is.na(bare$var1.pred)))

  res <- suppressWarnings(apply_interpolation(
    pts, "v", "OK", grid, character(0), lags,
    list(pre_fit = zero_nug, cv_strategy = "loocv"), "region", "act"))

  expect_true(all(is.na(res$res_sf$var1.pred)))
  expect_identical(res$fit$psill, zero_nug$psill)
  expect_match(res$log_msg, "nugget below 5% of its sill", fixed = TRUE)
})

test_that("a healthy OK run keeps its supplied model and reports no instability", {
  pts <- make_test_points(25)
  grid <- make_test_grid_safe(pts, res = 200)
  lags <- calc_scientific_lags(pts)
  fit <- gstat::vgm(psill = 90, model = "Sph", range = 400, nugget = 0)
  res <- suppressWarnings(apply_interpolation(
    pts, "v", "OK", grid, character(0), lags,
    list(pre_fit = fit, cv_strategy = "loocv"), "region", "act"))
  ref <- gstat::krige(v ~ 1, pts, grid, model = fit, debug.level = 0)

  expect_equal(res$res_sf$var1.pred, ref$var1.pred)
  expect_identical(res$fit$psill[1], 0)
  expect_false(grepl("nugget below 5%", res$log_msg, fixed = TRUE))
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
  # ... and record them for the run configuration: what the trend model used
  # and what the screen removed, never the selected list.
  for (r in list(res_self, res_pre)) {
    expect_equal(r$aux_used, gate$kept)
    expect_equal(r$aux_dropped, gate$dropped)
  }
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
  keep_kind <- RNGkind()
  on.exit({
    # kind first: RNGkind(<value>) writes .Random.seed, so the seed handling
    # has to come after it or a failed expectation leaves one behind.
    do.call(RNGkind, as.list(keep_kind))
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

  # The GENERATOR is restored on both branches. with_seed() names the kind, so
  # a caller is left on Mersenne-Twister unless the sandbox puts its own back.
  # The restore branch gets it free (.Random.seed's first element carries the
  # kind); the remove branch has to set it explicitly.
  RNGkind("L'Ecuyer-CMRG")
  invisible(with_seed(99, runif(3)))
  expect_identical(RNGkind()[1], "L'Ecuyer-CMRG")

  if (exists(".Random.seed", envir = globalenv(), inherits = FALSE)) rm(".Random.seed", envir = globalenv())
  expect_identical(RNGkind()[1], "L'Ecuyer-CMRG")
  x3 <- with_seed(99, runif(3))
  expect_identical(RNGkind()[1], "L'Ecuyer-CMRG")
  expect_false(exists(".Random.seed", envir = globalenv(), inherits = FALSE))
  # and the block's own numbers are the named generator's, not the caller's
  expect_equal(x3, x1)

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

  # A small-valued variable has a nonzero weighted SSE below six decimal
  # places; the diagnostics must not print it as a perfect fit.
  small <- df_a
  small$v <- small$v * 1e-4
  small_fit <- autofit_vgm_item(list(l = "LocA", act = small, pre = NULL), 32633)$act
  fit_sse <- attr(small_fit$fit, "SSErr")
  expect_true(is.finite(fit_sse) && fit_sse > 0)
  expect_equal(small_fit$sse, signif(fit_sse, 4))
  expect_gt(small_fit$sse, 0)

  fit_original <- robust_vgm_fit
  assign("robust_vgm_fit", function(...) {
    fit <- fit_original(...)
    attr(fit, "SSErr") <- NULL
    fit
  }, envir = globalenv())
  on.exit(assign("robust_vgm_fit", fit_original, envir = globalenv()), add = TRUE)
  expect_identical(autofit_vgm_item(
    list(l = "LocA", act = df_a, pre = NULL), 32633)$act$sse, "N/A")
  assign("robust_vgm_fit", fit_original, envir = globalenv())

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

test_that("idw_opt_item forwards cv_strategy to the power search", {
  # 2026-08-23 audit, Tier 3: the worker must hand the run's CV strategy to
  # optimize_idw_p, or the power is tuned on a partition the metrics never use.
  pts <- make_test_points(60, seed = 21)
  coords <- sf::st_coordinates(pts)
  df <- data.frame(x = coords[, 1], y = coords[, 2], v = pts$v)
  # idw_opt_item dedups at centimetre precision before searching; mirror that
  # so the expectation is scored on the identical point set.
  kept <- pts[!duplicated(round(sf::st_coordinates(pts), 2)), ]

  for (strategy in c("loocv", "block")) {
    expect_identical(
      idw_opt_item(list(l = "L", df = df), current_crs = 32633,
                   idw_nmax_val = 12, cv_strategy = strategy)$best_f,
      optimize_idw_p(kept, "v", nmax = 12, cv_strategy = strategy),
      info = strategy)
  }
  # Default keeps the auto plan when the caller supplies no strategy.
  expect_identical(
    idw_opt_item(list(l = "L", df = df), 32633, 12)$best_f,
    optimize_idw_p(kept, "v", nmax = 12, cv_strategy = "auto"))
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

test_that("an uploaded boundary works as points, without a .prj, and is refused when it misses the data", {
  pts <- make_test_points(30)
  coords <- sf::st_coordinates(pts)
  pts_data <- data.frame(x = coords[, 1], y = coords[, 2],
                         v = pts$v, pv = NA, Locality = "LocA")
  item <- list(l = "LocA", pts_data = pts_data,
               m_params = list(idw_p_act = 2, idw_p_pre = 2, idw_nmax = 12,
                               tps_lambda_act = -1, tps_lambda_pre = -1,
                               pre_fit_act = NULL, pre_fit_pre = NULL,
                               cv_strategy = "auto", rfk_uncertainty = "jackknife"))
  proj_root <- normalizePath(file.path(testthat::test_path(), "..", ".."), winslash = "/")
  run_with <- function(shp) {
    rp <- list(main_wd = proj_root, current_method = "IDW", current_crs = 32633,
               aux_vars = character(0), shp_bound = shp, b_type = "wrapped",
               buff_mode = "dynamic", b_dist = 250, res_mode = "fixed", grid_res = 50,
               crs_sel = "EPSG:32633", comp_mode = FALSE, val_type = "actual",
               progress_dir_val = tempdir(), session_id_val = "shp_test",
               cancel_file_val = NULL, vif_threshold = 10)
    interp_run_item(item, rp)
  }
  # A point layer (the samples themselves, in another UTM zone, with an
  # all-NA attribute column): its convex hull is the boundary, so the grid is
  # the hull's area and the run completes. Both the name-matched path
  # (Locality column) and the overlap path (no name column) must work.
  shp_pts <- sf::st_transform(sf::st_as_sf(pts_data, coords = c("x", "y"), crs = 32633), 32635)
  for (shp in list(shp_pts, shp_pts[, "v"])) {
    res <- run_with(shp)
    expect_false(grepl("Error", res$log_msg))
    expect_false(is.null(res$r_a))
  }
  hull_ha <- as.numeric(sf::st_area(sf::st_convex_hull(sf::st_union(pts)))) / 1e4
  r <- terra::unwrap(run_with(shp_pts)$r_a)
  expect_equal(sum(!is.na(terra::values(r[["var1.pred"]]))) * 50^2 / 1e4, hull_ha,
               tolerance = 0.15)

  # No .prj: the layer is taken to be in the Input Data CRS.
  no_prj <- sf::st_set_crs(sf::st_as_sf(pts_data, coords = c("x", "y"), crs = 32633), NA)
  res <- run_with(no_prj)
  expect_false(grepl("Error", res$log_msg))
  expect_false(is.null(res$r_a))

  # A feature NAMED for the locality that encloses none of its samples is
  # refused: the run falls back to the point-derived boundary instead of
  # gridding 50 km away.
  far <- pts_data; far$x <- far$x + 50000
  res <- run_with(sf::st_as_sf(far, coords = c("x", "y"), crs = 32633))
  expect_false(is.null(res$r_a))
  ext <- terra::ext(terra::unwrap(res$r_a))
  expect_true(ext$xmin < max(coords[, 1]) && ext$xmax > min(coords[, 1]))
})

test_that("shared unnamed boundaries fall back per locality and explicit names win", {
  pts <- golden_sf("full", localities = c("Kale", "Yorga"))
  items <- lapply(split(pts, pts$locality, drop = TRUE), function(p) {
    xy <- sf::st_coordinates(p)
    list(l = as.character(p$locality[1]),
         pts_data = data.frame(x = xy[, 1], y = xy[, 2], v = p$ph, pv = NA_real_),
         m_params = list(idw_p_act = 2, idw_nmax = 12, cv_strategy = "auto"))
  })
  polygon <- sf::st_as_sf(sf::st_buffer(sf::st_convex_hull(sf::st_union(pts)), 500))
  shared <- shared_boundary_features(polygon, items, sf::st_crs(pts))
  expect_identical(shared, 1L)
  expect_identical(shared_boundary_features(pts, items, sf::st_crs(pts)), 1L)
  expect_length(shared_boundary_features(polygon, items[1], sf::st_crs(pts)), 0)
  dir <- tempfile("shared_boundary_"); dir.create(dir)
  withr::defer(unlink(dir, recursive = TRUE))
  withr::local_options(monolith_progress_dir = dir, monolith_session_id = "shared")
  run <- function(it, shp, ids) suppressWarnings(run_regional_interpolation(
    it, "IDW", sf::st_crs(pts), character(0), shp, "convex", "fixed", 250,
    "fixed", 250, sf::st_crs(pts)$wkt, FALSE, "actual",
    progress_dir_val = dir, session_id_val = "shared", shp_shared = ids))
  res <- lapply(items, run, shp = polygon, ids = shared)
  for (i in seq_along(items)) {
    ref <- run(items[[i]], NULL, integer(0))
    expect_false(is.null(res[[i]]$r_a))
    expect_equal(sf::st_bbox(res[[i]]$bound), sf::st_bbox(ref$bound))
    expect_match(res[[i]]$log_msg, "shared by several")
  }
  expect_equal(locality_boundary_overlap(lapply(res, `[[`, "bound")), 0)
  polygon$Locality <- items[[1]]$l
  named <- run(items[[1]], polygon, shared)
  expect_equal(as.numeric(sf::st_bbox(named$bound)), as.numeric(sf::st_bbox(polygon)))
  expect_true(sf::st_crs(named$bound) == sf::st_crs(polygon))
})

test_that("boundary helpers: hull, assumed CRS and overlap", {
  pts <- make_test_points(30)
  cc <- sf::st_coordinates(pts)
  # Polygons pass through; points become one hull row; degenerate input is NULL.
  # 1 m wider than the samples' bbox: extreme samples would otherwise sit
  # exactly on an edge, where a transform's rounding decides the side.
  poly <- sf::st_sf(geometry = sf::st_as_sfc(sf::st_bbox(sf::st_buffer(pts, 1))))
  expect_identical(shp_boundary_polygons(poly), poly)
  h <- shp_boundary_polygons(pts)
  expect_equal(nrow(h), 1L)
  expect_true(all(as.character(sf::st_geometry_type(h)) == "POLYGON"))
  expect_null(shp_boundary_polygons(pts[1:2, ]))

  no_crs <- sf::st_set_crs(pts, NA)
  expect_true(sf::st_crs(shp_assume_crs(no_crs, "EPSG:32633")) == sf::st_crs(32633))
  expect_true(is.na(sf::st_crs(shp_assume_crs(no_crs, "not a crs"))))
  expect_identical(shp_assume_crs(pts, "EPSG:4326"), pts)

  ov <- shp_boundary_overlap(poly, cc[, 1], cc[, 2], "EPSG:32633")
  expect_equal(c(ov$n, ov$inside), c(30L, 30L))
  expect_false(ov$hull)
  # 50 km east: nothing inside, and the gap is reported in km.
  far <- sf::st_set_geometry(poly, sf::st_geometry(poly) + c(50000, 0))
  far <- sf::st_set_crs(far, 32633)
  ov <- shp_boundary_overlap(far, cc[, 1], cc[, 2], "EPSG:32633")
  expect_equal(ov$inside, 0L)
  gap_km <- (50000 - diff(range(cc[, 1])) - 2) / 1000
  expect_equal(ov$dist_km, gap_km, tolerance = 0.01)
  # The same boundary without a .prj is read in the Input Data CRS.
  expect_equal(shp_boundary_overlap(sf::st_set_crs(poly, NA), cc[, 1], cc[, 2], "EPSG:32633")$inside, 30L)
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

test_that("the prediction grid is invariant to the boundary-clip block size", {
  # The bounding-box cells are converted to sf points a block at a time, tested
  # against the boundary as they go, and only the keep MASK survives the loop -
  # the sf is built once from the masked rows, so peak memory is one block plus
  # the survivors rather than the whole box or the survivors twice (an sfc_POINT
  # costs ~430 bytes per cell against 16 for a matrix row). The predicate and
  # the surviving rows must be exactly what a single pass produced - if the
  # block size can move a value, the blocking is not exact.
  pts <- make_test_points(20)
  coords <- sf::st_coordinates(pts)
  pts_data <- data.frame(x = coords[, 1], y = coords[, 2],
                         v = pts$v, pv = pts$pv, Locality = "LocA")
  item <- list(l = "LocA", pts_data = pts_data,
               m_params = list(idw_p_act = 2, idw_p_pre = 2, idw_nmax = 12,
                               tps_lambda_act = -1, tps_lambda_pre = -1,
                               pre_fit_act = NULL, pre_fit_pre = NULL,
                               cv_strategy = "auto", rfk_uncertainty = "jackknife"))
  run_grid <- function(b_type) {
    suppressWarnings(run_regional_interpolation(
      item, "IDW", 32633, character(0), NULL, b_type, "dynamic", 250,
      "fixed", 200, "EPSG:4326", FALSE, "actual"))
  }

  orig <- .GRID_CLIP_BLOCK_CELLS
  withr::defer(assign(".GRID_CLIP_BLOCK_CELLS", orig, envir = globalenv()))

  # "concave" leaves a large out-of-boundary surplus in the bounding box, which
  # is the case the block clip exists for; "convex" retains most of its box,
  # which is the case where holding the surviving blocks AND their rbind cost
  # the most. Both must come out of the block loop identical.
  for (b_type in c("concave", "convex")) {
    assign(".GRID_CLIP_BLOCK_CELLS", orig, envir = globalenv())
    one_block <- run_grid(b_type)
    assign(".GRID_CLIP_BLOCK_CELLS", 7, envir = globalenv())   # several blocks
    many_blocks <- run_grid(b_type)
    assign(".GRID_CLIP_BLOCK_CELLS", orig, envir = globalenv())

    r1 <- terra::unwrap(one_block$r_a)
    r2 <- terra::unwrap(many_blocks$r_a)
    expect_gt(prod(dim(r1)[1:2]), 7)   # otherwise the block override is a no-op
    expect_identical(dim(r1), dim(r2), info = b_type)
    expect_identical(terra::values(r1), terra::values(r2), info = b_type)
  }
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

  # And in the RETURNED LOG, not only in the file. The warning file holds one
  # message per locality and surface and the completion handler deletes it, so
  # a message that lives only there is on screen for a few seconds and then
  # gone - it never reaches the exported run log.
  expect_match(res$log_msg, "no usable variance")
  expect_match(res$log_msg, "[WARN] LocA (Actual)", fixed = TRUE)
})

test_that("a degenerate target is flagged on its fitted variogram", {
  # The variogram of a constant target fits a sill 60 orders of magnitude
  # below the data and reports "Structural Dependence 100%". The fit carries
  # the cause so the panels can suppress the parameters instead of printing a
  # strong claim manufactured from numerical noise.
  pts <- make_test_points(20)
  lags <- calc_scientific_lags(pts)
  flat <- pts
  flat$v <- 7.5
  v_emp <- gstat::variogram(v ~ 1, flat, width = lags$width, cutoff = lags$cutoff)
  fit <- suppressWarnings(robust_vgm_fit(v_emp, flat$v))
  expect_true(vgm_target_degenerate(fit))
  expect_identical(names(vgm_params_table_df(list(LocA_act = fit), "LocA")), "Status")
})

test_that("an Actual-only run carries no ML-prediction products", {
  # pred_col is resolved from the variable's _cve/_ss column whatever the view
  # is, so an Actual-only run used to fill pv and then register a point-error
  # surface and a residual map for a run that predicted nothing. The pipeline
  # half of that contract: with pv absent, no prediction product is built.
  pts <- make_test_points(20)
  coords <- sf::st_coordinates(pts)
  pts_data <- data.frame(x = coords[, 1], y = coords[, 2],
                         v = pts$v, pv = NA_real_, Locality = "LocA")
  item <- list(l = "LocA", pts_data = pts_data,
               m_params = list(idw_p_act = 2, idw_p_pre = 2, idw_nmax = 12,
                               tps_lambda_act = -1, tps_lambda_pre = -1,
                               pre_fit_act = NULL, pre_fit_pre = NULL,
                               cv_strategy = "auto", rfk_uncertainty = "jackknife"))

  res <- suppressWarnings(run_regional_interpolation(
    item, "IDW", 32633, character(0), NULL, "wrapped", "dynamic", 250,
    "fixed", 200, "EPSG:4326", FALSE, "actual"))

  expect_false(is.null(res$r_a))
  expect_null(res$r_p)             # no predicted surface
  expect_null(res$r_res)           # no residual (delta) raster
  expect_null(res$r_point_err)     # no interpolated point-error surface
  expect_true(all(is.na(res$pts$pv)))
  expect_true(all(is.na(res$pts$resid)))
  expect_true(all(is.na(res$pts$model_resid_pre)))
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
  # The run record says what happened: the screen removed both, and the
  # surface that was mapped (the OK fallback) used no covariate.
  expect_equal(res$aux_used, character(0))
  expect_setequal(res$aux_dropped, c("covA", "covB"))

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

  # A corrective cell size the calling suite cannot set is left out, so only
  # the buffer arm remains. floor(3 * sqrt(2)) = 4 m: below the Classification
  # Suite's 5 m slider floor (the default), reachable on the interpolation
  # suite's own slider.
  expect_match(strict_buffer_message(3, 5), "Raise the buffer to 4 m or more\\.$")
  expect_false(grepl("lower the resolution", strict_buffer_message(3, 5),
                     fixed = TRUE))
  expect_match(strict_buffer_message(3, 5, res_floor = 1),
               "or lower the resolution to 4 m or less\\.$")
  # Still dropped below the stated floor: floor(0.6 * sqrt(2)) = 0 m.
  expect_match(strict_buffer_message(0.6, 5, res_floor = 1),
               "Raise the buffer to 4 m or more\\.$")

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

test_that("a coarsened grid reaches both the progress panel and the run log", {
  # The candidate-cell cap changes the cell size the surface is computed at, so
  # it has to survive the run: the progress panel holds one warning per
  # locality and closes when the maps are revealed.
  pts_data <- data.frame(x = c(0, 100, 200, 300, 150), y = c(0, 100, 0, 150, 250),
                         v = c(1.2, 3.4, 2.1, 2.8, 1.9), pv = NA, Locality = "LocA")
  item <- list(l = "LocA", pts_data = pts_data,
               m_params = list(idw_p_act = 2, idw_p_pre = 2, idw_nmax = 12,
                               tps_lambda_act = -1, tps_lambda_pre = -1,
                               pre_fit_act = NULL, pre_fit_pre = NULL,
                               cv_strategy = "auto", rfk_uncertainty = "jackknife"))

  tmp <- withr::local_tempdir("cap_res_")
  # run_regional_interpolation sets these options itself and never restores
  # them; local_options puts the session's own values back at test exit.
  withr::local_options(monolith_progress_dir = tmp, monolith_session_id = "cap_res")
  # Shrink the budget rather than widen the extent: firing the real 4e6 cap
  # allocates 4e6 candidate cells by construction, which is what it exists to
  # prevent.
  orig <- .INTERP_MAX_CANDIDATE_CELLS
  withr::defer(assign(".INTERP_MAX_CANDIDATE_CELLS", orig, envir = globalenv()))
  assign(".INTERP_MAX_CANDIDATE_CELLS", 100, envir = globalenv())
  res <- suppressWarnings(run_regional_interpolation(
    item, "IDW", 32633, character(0), NULL, "wrapped", "dynamic", 250,
    "fixed", 5, "EPSG:32633", FALSE, "actual",
    progress_dir_val = tmp, session_id_val = "cap_res"))
  assign(".INTERP_MAX_CANDIDATE_CELLS", orig, envir = globalenv())

  expect_false(grepl("Error in", res$log_msg))
  # sqrt(bbox area / budget): the continuous-area target. Whole-cell
  # extension means the raster's actual candidate count is approximate.
  bb <- sf::st_bbox(res$bound)
  expect_equal(res$actual_res,
               sqrt(as.numeric(bb["xmax"] - bb["xmin"]) *
                    as.numeric(bb["ymax"] - bb["ymin"]) / 100))
  expect_gt(res$actual_res, 5)

  # Durable channel: [WARN] also raises it as a notification (server_sci_analysis.R).
  expect_match(res$log_msg, "[WARN] LocA: Fixed grid resolution", fixed = TRUE)
  expect_match(res$log_msg, "coarsened to", fixed = TRUE)
  # Progress-panel channel.
  wf <- file.path(tmp, "warn_cap_res_LocA_act.txt")
  expect_true(file.exists(wf))
  expect_match(paste(readLines(wf), collapse = " "), "coarsened to")
})

# ── Numeric contracts: the engines' arithmetic ─────────────────────────────
#
# Everything above tests behaviour: branches, guards, fallbacks, invariance.
# This block tests the numbers themselves, against the definitions rather than
# against our own output. Where a golden constant is used it is derived in the
# comment; where a theorem is available it is preferred to a constant, because
# a theorem survives a gstat upgrade and a refactor.

test_that("IDW reproduces the hand-computed Shepard weighted mean", {
  pts <- golden_sf("tiny")
  # A single prediction location, with nmax = n so no neighbourhood truncation
  # stands between the engine and the plain Shepard sum.
  tgt <- sf::st_as_sf(data.frame(x = mean(pts$x), y = mean(pts$y)),
                      coords = c("x", "y"), crs = 32635)
  d <- as.numeric(sf::st_distance(tgt, pts))

  for (p in c(1, 2, 3.5)) {
    res <- apply_IDW(pts, "ph", tgt, list(idw_p = p, idw_nmax = nrow(pts)))
    # Shepard (1968): z(s0) = sum(w_i z_i) / sum(w_i), w_i = d_i^-p
    w <- d^(-p)
    expect_equal(res$res_sf$var1.pred, sum(w * pts$ph) / sum(w),
                 tolerance = 1e-10)
  }
})

test_that("IDW with nmax = 1 returns the nearest observation", {
  pts <- golden_sf("tiny")
  tgt <- sf::st_as_sf(data.frame(x = mean(pts$x), y = mean(pts$y)),
                      coords = c("x", "y"), crs = 32635)
  d <- as.numeric(sf::st_distance(tgt, pts))

  res <- apply_IDW(pts, "ph", tgt, list(idw_p = 2, idw_nmax = 1))
  # One neighbour makes the weighted mean that neighbour, whatever the power.
  expect_equal(res$res_sf$var1.pred, pts$ph[which.min(d)])
})

test_that("raising the IDW power pulls the prediction toward the nearest point", {
  pts <- golden_sf("tiny")
  tgt <- sf::st_as_sf(data.frame(x = mean(pts$x), y = mean(pts$y)),
                      coords = c("x", "y"), crs = 32635)
  nearest <- pts$ph[which.min(as.numeric(sf::st_distance(tgt, pts)))]

  gap <- vapply(c(0.5, 1, 2, 4, 8), function(p) {
    r <- apply_IDW(pts, "ph", tgt, list(idw_p = p, idw_nmax = nrow(pts)))
    abs(r$res_sf$var1.pred - nearest)
  }, numeric(1))
  # The weight of the nearest point dominates as p grows, so the prediction
  # approaches it monotonically.
  expect_true(all(diff(gap) < 0))
})

test_that("OK reproduces the observation and reports zero variance at a data location", {
  pts <- golden_sf("tiny")
  lags <- calc_scientific_lags(pts)
  # Predict AT the sample locations: kriging is an exact interpolator, so the
  # prediction must be the observation and the kriging variance must vanish.
  # This holds for a fitted model with a nugget too - gstat does not filter the
  # nugget - so it is a property of the method, not of this variogram.
  res <- suppressWarnings(apply_OK(pts, "ph", pts[1:6, ], lags, list()))

  expect_equal(res$res_sf$var1.pred, pts$ph[1:6], tolerance = 1e-8)
  expect_true(all(abs(res$res_sf$var1.var) < 1e-8))
})

test_that("OK under a pure nugget returns the global mean and nugget(1 + 1/n)", {
  pts <- golden_sf("tiny")
  lags <- calc_scientific_lags(pts)
  nug <- 0.05
  tgt <- sf::st_as_sf(data.frame(x = mean(pts$x), y = mean(pts$y)),
                      coords = c("x", "y"), crs = 32635)

  res <- suppressWarnings(
    apply_OK(pts, "ph", tgt, lags, list(pre_fit = gstat::vgm(nug, "Nug", 0))))

  # With no spatial correlation every weight is 1/n, so ordinary kriging
  # degenerates to the sample mean, and its variance is the nugget plus the
  # variance of the estimated mean: C0 + C0/n.
  n <- nrow(pts)
  expect_equal(res$res_sf$var1.pred, mean(pts$ph), tolerance = 1e-10)
  expect_equal(res$res_sf$var1.var, nug * (1 + 1 / n), tolerance = 1e-10)
})

test_that("RK prediction is the lm trend plus the kriged residual", {
  pts  <- golden_sf("tiny")
  grid <- make_test_grid_safe(pts, res = 800)
  lags <- calc_scientific_lags(pts)
  # Supply grid_aux so the covariate surface is fixed and the test isolates the
  # recomposition rather than re-kriging the covariate.
  grid_aux <- grid
  grid_aux$v82 <- seq(min(pts$v82), max(pts$v82), length.out = nrow(grid))

  res <- suppressWarnings(
    apply_RK(pts, "ph", grid, lags, list(grid_aux = grid_aux), c("v82")))
  skip_if(grepl("Falling back to OK", res$log_msg, fixed = TRUE),
          "RK fell back to OK on this fixture")

  # Rebuild the decomposition by hand, reusing the engine's OWN fitted
  # variogram so what is under test is the recomposition, not the fit.
  lm_mod <- lm(ph ~ v82, data = pts)
  dat <- pts
  dat$residuals <- residuals(lm_mod)
  kr <- gstat::krige(residuals ~ 1, dat, grid, model = res$fit, debug.level = 0)
  tr <- predict(lm_mod, newdata = sf::st_drop_geometry(grid_aux), se.fit = TRUE)

  expect_equal(res$res_sf$var1.pred, as.vector(tr$fit + kr$var1.pred),
               tolerance = 1e-8)
  expect_equal(res$res_sf$var1.var, as.vector(tr$se.fit^2 + kr$var1.var),
               tolerance = 1e-8)
})

test_that("RFK prediction is the forest trend plus the kriged residual", {
  pts  <- golden_sf("tiny")
  grid <- make_test_grid_safe(pts, res = 1500)
  lags <- calc_scientific_lags(pts)
  grid_aux <- grid
  grid_aux$v82 <- seq(min(pts$v82), max(pts$v82), length.out = nrow(grid))

  res <- suppressWarnings(
    apply_RFK(pts, "ph", grid, lags,
              list(grid_aux = grid_aux, rf_ntree = 60), c("v82")))
  skip_if(grepl("Falling back to OK", res$log_msg, fixed = TRUE),
          "RFK fell back to OK on this fixture")

  # The forest is unseeded here, so the reference is built from the engine's
  # own fitted forest and its own OOB residuals: the claim under test is the
  # decomposition, not the forest.
  #
  # The trend residual is recomputed from the returned forest: the engine
  # kriges the OOB residual, which is a training quantity the result list no
  # longer carries (CV residuals live in res$cv_obj).
  dat <- pts
  dat$residuals <- pts$ph - res$rf_model$predicted
  kr <- gstat::krige(residuals ~ 1, dat, grid, model = res$fit, debug.level = 0)
  ga <- sf::st_drop_geometry(grid_aux)
  pa <- predict(res$rf_model, ga, predict.all = TRUE)

  expect_equal(res$res_sf$var1.pred, as.vector(pa$aggregate + kr$var1.pred),
               tolerance = 1e-8)
  # The uncertainty band is the infinitesimal-jackknife trend variance plus the
  # residual kriging variance.
  ij <- rf_infinitesimal_jackknife_var(pa$individual, res$rf_model$inbag)
  expect_equal(res$res_sf$var1.var, as.vector(ij + kr$var1.var),
               tolerance = 1e-8)
})

test_that("TPS at a large lambda collapses onto the least-squares plane", {
  pts  <- golden_sf("tiny")
  grid <- make_test_grid_safe(pts, res = 800)

  res <- apply_TPS(pts, "ph", grid, list(tps_lambda = 1e8))
  expect_false(grepl("Falling back to IDW", res$log_msg, fixed = TRUE))

  # The thin-plate penalty annihilates only the linear null space, so as the
  # smoothing parameter grows the fit converges to the OLS plane in x and y.
  # The engine fits on rescaled coordinates, but rescaling is affine and maps
  # planes to planes, so the raw-coordinate OLS plane is the right reference.
  gc_ <- sf::st_coordinates(grid)
  plane <- lm(ph ~ x + y, data = sf::st_drop_geometry(pts))
  ref <- as.vector(predict(plane, newdata = data.frame(x = gc_[, 1],
                                                       y = gc_[, 2])))
  expect_equal(res$res_sf$var1.pred, ref, tolerance = 1e-3)
})

test_that("CK is invariant to a linear rescaling of a covariate", {
  pts  <- golden_sf("tiny")
  grid <- make_test_grid_safe(pts, res = 800)
  lags <- calc_scientific_lags(pts)
  ga <- grid
  ga$v82 <- seq(min(pts$v82), max(pts$v82), length.out = nrow(grid))

  base <- suppressWarnings(
    apply_CK(pts, "ph", grid, lags, list(grid_aux = ga), "v82"))

  # Co-kriging standardizes the covariates before building the LMC, so a change
  # of units (metres to millimetres, plus a datum offset) cannot move the
  # predictions. Without that step the cross-variograms would live on a
  # different scale and the surface would change with the unit the user
  # happened to upload.
  p2 <- pts;  p2$v82 <- p2$v82 * 1000 + 5e5
  g2 <- ga;   g2$v82 <- g2$v82 * 1000 + 5e5
  rescaled <- suppressWarnings(
    apply_CK(p2, "ph", grid, lags, list(grid_aux = g2), "v82"))

  expect_equal(rescaled$res_sf$var1.pred, base$res_sf$var1.pred,
               tolerance = 1e-8)
  expect_equal(rescaled$res_sf$var1.var, base$res_sf$var1.var,
               tolerance = 1e-8)
  # Every cross-validation fold standardizes from its own training rows, so
  # the out-of-fold predictions carry the same invariance.
  expect_equal(rescaled$cv_obj$var1.pred, base$cv_obj$var1.pred, tolerance = 1e-8)
  expect_equal(rescaled$cv_obj$var1.var, base$cv_obj$var1.var, tolerance = 1e-8)
})

test_that("CK is an exact interpolator at the sample locations", {
  pts  <- golden_sf("tiny")
  lags <- calc_scientific_lags(pts)
  # Predicting at the samples themselves: co-kriging, like ordinary kriging,
  # must return the observation and no uncertainty there.
  res <- suppressWarnings(
    apply_CK(pts, "ph", pts, lags, list(grid_aux = pts), "v82"))
  skip_if(grepl("OK fallback", res$log_msg, fixed = TRUE),
          "CK fell back to OK on this fixture")

  expect_equal(res$res_sf$var1.pred, pts$ph, tolerance = 1e-8)
  expect_true(all(abs(res$res_sf$var1.var) < 1e-8))
})

test_that("TPS roughness decreases monotonically with lambda", {
  pts  <- golden_sf("tiny")
  xy <- sf::st_coordinates(pts)
  xy <- sweep(xy, 2, apply(xy, 2, min)) / max(apply(xy, 2, function(z) diff(range(z))))
  d <- as.matrix(dist(xy))
  K <- d^2 * log(d); K[d == 0] <- 0
  T <- cbind(1, xy)
  system <- rbind(cbind(K, T), cbind(t(T), matrix(0, 3, 3)))

  # Independent TPS definition: reconstruct the radial coefficients from the
  # fitted values with T'c = 0. Bending energy is proportional to c'Kc for
  # K(r) = r^2 log(r). Raster-order first differences are not this penalty and
  # have no monotonicity guarantee (they also count linear slope as roughness).
  rough <- vapply(c(1e-6, 1e-4, 1e-2, 1, 1e3), function(lam) {
    r <- apply_TPS(pts, "ph", pts, list(tps_lambda = lam))
    coef <- solve(system, c(r$res_sf$var1.pred, 0, 0, 0))[seq_len(nrow(pts))]
    as.numeric(crossprod(coef, K %*% coef))
  }, numeric(1))

  expect_true(all(diff(rough) < 0))
  expect_gt(rough[1] / rough[length(rough)], 5)
})

test_that("the GCV optimum agrees with the curve the panel plots", {
  pts <- golden_sf("tiny")
  item <- list(l = "Kale", df = data.frame(x = pts$x, y = pts$y, v = pts$ph))
  tg <- tps_gcv_item(item, "EPSG:32635")

  expect_false(is.null(tg$gcv_data))
  expect_gt(tg$best_lam, 0)
  # fields optimises lambda continuously while the plotted curve is its coarse
  # GCV grid, so the two agree to the grid's resolution rather than exactly.
  # A reported optimum sitting off the visible minimum would be a real defect.
  argmin <- tg$gcv_data$lambda[which.min(tg$gcv_data$gcv)]
  expect_lt(abs(log10(tg$best_lam) - log10(argmin)), 0.1)
})

test_that("a one-square-kilometre surface reports its ground area, not its projected area", {
  # A 1 km x 1 km block in UTM 35N, placed at a realistic easting for this
  # survey. terra::expanse reprojects each cell to compute geodesic area, so
  # what the Area Coverage table reports is ground hectares.
  r <- terra::rast(xmin = 650000, xmax = 651000,
                   ymin = 4150000, ymax = 4151000,
                   resolution = 100, crs = "EPSG:32635")
  terra::values(r) <- 5
  names(r) <- "var1.pred"
  params <- list(brks = c(0, 10),
                 rcl_mat = matrix(c(-Inf, 10, 1), ncol = 3, byrow = TRUE),
                 labels = "One", n_c = 1)

  z <- build_class_zone_sf(r, params)
  expect_equal(nrow(z), 1L)

  poly <- sf::st_sfc(sf::st_polygon(list(rbind(
    c(650000, 4150000), c(651000, 4150000), c(651000, 4151000),
    c(650000, 4151000), c(650000, 4150000)))), crs = 32635)
  expect_equal(as.numeric(sf::st_area(poly)) / 1e4, 100)      # planar, by construction
  # Ground area, computed independently from the polygon rather than the raster.
  expect_equal(z$area_ha, terra::expanse(terra::vect(poly), unit = "ha"),
               tolerance = 1e-3)
  # Within a tenth of a hectare of the planar figure at this easting, but not
  # identical to it: the projection is not area-true.
  expect_lt(abs(z$area_ha - 100), 0.1)
})

test_that("calc_class_breaks reproduces the quantile and equal-interval definitions", {
  x <- golden_soil("full")$ph

  # Interior breaks only: n_c classes need n_c - 1 cuts, and the outer edges
  # are the data's own extremes.
  q <- calc_class_breaks(x, 4, "quantile")
  expect_length(q, 3L)
  expect_equal(q, unname(quantile(x, probs = c(0.25, 0.50, 0.75))),
               tolerance = 1e-10)

  e <- calc_class_breaks(x, 4, "equal")
  expect_length(e, 3L)
  expect_equal(e, min(x) + (1:3) * diff(range(x)) / 4, tolerance = 1e-10)

  # Fewer values than classes has no answer.
  expect_null(calc_class_breaks(c(1, 2), 5, "quantile"))
})

test_that("the Jenks break path is pinned", {
  # No closed form to check Jenks against, so this is a regression lock, valid
  # only because the quantile and equal paths above establish that the slicing
  # and the seed sandbox are right. The expected values are recorded for this
  # golden set by make_baselines.R; if they move, classInt changed or the
  # fixture did.
  x <- golden_soil("full")[[golden_meta()$columns$target]]
  recorded <- golden_baseline("jenks_target_5")
  skip_if(is.null(recorded), "no baselines recorded for this golden set")

  expect_equal(calc_class_breaks(x, 5, "jenks"), recorded, tolerance = 1e-9,
               info = golden_baseline_info())
  # Seed-sandboxed: repeated calls agree and the caller's stream is untouched.
  set.seed(99); before <- .Random.seed
  expect_equal(calc_class_breaks(x, 5, "jenks"), recorded, tolerance = 1e-9)
  expect_identical(.Random.seed, before)
})

test_that("the whole regional driver still produces the surface it produced before", {
  # The end-to-end alarm. Every test above proves a component computes what its
  # method defines; this one asks whether the driver still assembles them into
  # the same map - the question an edit anywhere in the pipeline actually raises.
  #
  # It is a regression lock, and it is only legitimate because the component
  # tests establish correctness first: on its own it would say "unchanged", not
  # "right". Ordinary Kriging on purpose (fully seed-sandboxed; RFK's forest is
  # unseeded and would not reproduce), and sequential, so it does NOT cover the
  # future/PSOCK dispatch layer.
  #
  # The variogram is PINNED by golden_pin_vgm() rather than fitted. Fitted, this
  # lock is not portable: on this scope all sixteen of robust_vgm_fit()'s
  # candidates fail to converge and the single survivor of the sanity window
  # follows the platform's floating-point path, which moved the range 12%
  # between Windows and Linux and every value downstream with it. What remains
  # is a linear solve on a matrix with condition number 5.8, so the 1e-6
  # relative tolerance is comfortable rather than tight, and the vgm_* entries
  # are constants confirming the pin reached the engine. robust_vgm_fit() keeps
  # its own coverage in test-robust-vgm-fit.R.
  #
  # The BOUNDARY is pinned the same way, by golden_pin_boundary(). The
  # point-derived hull put two cell centres 1.2 m and 2.0 m from its edge on a
  # 300 m grid, so a sub-metre GEOS or concaveman difference decided whether
  # they were in: moving the buffer 0.5 m locally reproduced the CI failure's
  # exact signature, the same five surface entries moving while cells, vgm_* and
  # cv_* stayed bit-identical. The pinned boundary is a union of whole lattice
  # cells, so every candidate centre sits exactly 150 m from an edge and the
  # mask survives +/- 20 m of vertex noise unchanged. It still clips - 294
  # candidate cells, 127 kept - so the mask remains under test.
  # If this fails where nothing in the repository changed, read the `info` line
  # on the failure: it carries the R and package versions the baseline was
  # recorded under and names any that differ here.
  recorded <- golden_baseline("ok_surface_digest")
  skip_if(is.null(recorded), "no baselines recorded for this golden set")

  digest <- run_surface_digest(golden_sf("tiny"))
  expect_named(digest, names(recorded))
  expect_equal(digest, recorded, tolerance = 1e-6,
               info = golden_baseline_info())
})

test_that("calc_metric_spacing is the mean nearest-neighbour distance in metres", {
  pts <- golden_sf("tiny")
  co <- sf::st_coordinates(pts)
  D <- as.matrix(dist(co))
  diag(D) <- Inf

  sp <- calc_metric_spacing(pts)
  expect_equal(sp$mean_nn, mean(apply(D, 1, min)), tolerance = 1e-8)
  expect_equal(sp$max_dim,
               max(diff(range(co[, 1])), diff(range(co[, 2]))),
               tolerance = 1e-8)

  # The same points in degrees must still be measured in metres: the geographic
  # branch projects to Web Mercator and divides out its 1/cos(latitude)
  # inflation, so it has to land on the projected answer, not 1.24x it (the
  # factor at this latitude).
  sp_ll <- calc_metric_spacing(golden_sf("tiny", crs = 4326))
  expect_equal(sp_ll$mean_nn / sp$mean_nn, 1, tolerance = 0.05)
  expect_equal(sp_ll$max_dim / sp$max_dim, 1, tolerance = 0.05)
})

# -- the parallel dispatch contract -----------------------------------------

test_that("every pinned dispatch ships the globals its body reads", {
  # The promise bodies pin `globals =` rather than paying future's recursive
  # discovery walk, which cost 6 s of frozen main session on every run and, at
  # the governing-factors site, shipped masked base generics whose package
  # references made the worker attach spam, terra, fields and sf. The price of
  # pinning is that a name added to a body and forgotten in its list fails
  # only inside the worker, as "object not found", where no sequential test
  # can see it. So: every free name in a pinned body must be pinned, defined
  # at the top level of a spatial_helpers.R fragment (the worker sources those
  # itself), or come from a package.
  root <- normalizePath(file.path(testthat::test_path(), "..", ".."), mustWork = TRUE)

  # The fragment list is READ from the master, never restated here: a fragment
  # this test still trusted after the master stopped sourcing it would account
  # for names no worker defines, which is the failure the test exists to catch.
  master <- readLines(file.path(root, "spatial_helpers.R"), warn = FALSE)
  frags <- sub('.*source\\(file\\.path\\(src_dir, "([^"]+)"\\)\\).*', "\\1",
               grep('source\\(file\\.path\\(src_dir, "', master, value = TRUE))
  expect_gte(length(frags), 4L)
  top_level_names <- function(files) {
    src <- unlist(lapply(files,
                         function(f) readLines(file.path(root, f), warn = FALSE)))
    sub(" <- .*$", "", grep("^[^ #]+ <- ", src, value = TRUE))
  }
  # Each worker is trusted with exactly what its own body source()s.
  from_spatial <- top_level_names(frags)
  from_classif <- top_level_names(c(frags, "classif_helpers.R"))

  check_dispatch <- function(file, tail_line, expected_pinned,
                             from_helpers = from_spatial) {
    src <- readLines(file.path(root, file), warn = FALSE)
    i0 <- grep("promises::future_promise({", src, fixed = TRUE)
    i1 <- grep(tail_line, src, fixed = TRUE)
    expect_length(i0, 1L)
    expect_length(i1, 1L)

    txt <- src[i0:i1]
    txt[length(txt)] <- sub(" %...>%.*$", "", txt[length(txt)])
    dispatch <- parse(text = paste(txt, collapse = "\n"))[[1]]

    pinned <- names(as.list(dispatch$globals))[-1]
    expect_setequal(pinned, expected_pinned)

    body_fn <- eval(call("function", NULL, dispatch[[2]]))
    free <- globals::findGlobals(body_fn)
    called <- codetools::findGlobals(body_fn, merge = FALSE)$functions

    # A name read as DATA has to be shipped even when it happens to collide
    # with a package function: `df` resolves to stats::df, so a forgotten
    # `df` would hand the worker the F density instead of the user's table.
    read_as_data <- setdiff(free, called)
    expect_identical(setdiff(read_as_data, c(pinned, from_helpers)), character(0),
                     info = paste(file, "- read as data in the worker but never shipped:",
                                  paste(setdiff(read_as_data, c(pinned, from_helpers)),
                                        collapse = ", ")))

    # A name CALLED as a function may also come from a package.
    from_package <- vapply(called, function(nm) {
      fn <- tryCatch(get(nm), error = function(e) NULL)
      !is.null(fn) && !identical(environment(fn), globalenv())
    }, logical(1))
    unaccounted <- setdiff(called[!from_package], c(pinned, from_helpers))
    expect_identical(unaccounted, character(0),
                     info = paste(file, "- called in the worker but never shipped to it:",
                                  paste(unaccounted, collapse = ", ")))
    dispatch
  }

  interp <- check_dispatch("server_execution.R",
                           "seed = 12345) %...>% (function(res_all) {",
                           c("main_wd", "run_params", "df_list", "nested_workers"))
  # Pinning also switches off the package detection that walk used to do, so
  # the unqualified calls in the helper graph need the same declaration the
  # nested furrr_options makes.
  expect_identical(eval(interp$packages), c("sf", "gstat", "dplyr"))

  check_dispatch("gov_module.R",
                 "seed = TRUE) %...>% (function(res) {",
                 c("proj_root_ship", "df", "target_col", "preds", "n_perms",
                   "ntree_val", "shap_size_val", "cores_hint_val",
                   "cancel_file_ship"))

  # The classification worker sources classif_helpers.R as well, and ships one
  # object: everything the pipeline reads travels inside run_args.
  check_dispatch("classif_module.R",
                 "seed = TRUE) %...>% (function(res) {",
                 "run_args", from_helpers = from_classif)
})

test_that("every pinned dispatch establishes its own parallel plan", {
  # A pool worker is reused across features. If a task's teardown does not
  # complete, it leaves that worker on a plan pointing at a cluster nobody owns
  # any more, and nbrOfWorkers() then reports the dead cluster's size - which is
  # what every escalation guard reads (the run and optimizer bodies directly,
  # compute_governing_factors() for SHAP, tune:::get_future_workers() for the
  # classification grid). Each body therefore resets the plan after its
  # source(), rather than trusting what it inherited.
  root <- normalizePath(file.path(testthat::test_path(), "..", ".."), mustWork = TRUE)
  for (f in c("server_execution.R", "server_model_tuning.R", "gov_module.R",
              "classif_module.R")) {
    src <- readLines(file.path(root, f), warn = FALSE)
    i_src <- grep("source(\"spatial_helpers.R\"", src, fixed = TRUE)
    # A bare statement, not the tryCatch()-wrapped reset in the teardown: the
    # teardown tidies up after this task, the reset protects this task from the
    # last one.
    i_plan <- grep("^\\s*future::plan\\(future::sequential\\)\\s*$", src)
    expect_gte(length(i_src), 1L)
    expect_true(any(i_plan > i_src[1]), info = paste(f, "- no plan reset after the source()"))
  }
})

# ── Models handed back to the main session ─────────────────────────────────
# A formula built inside a function carries that function's whole frame as its
# environment, and `terms` inherits it. A fitted model returned from a worker
# therefore dragged the locality's point set, prediction grid, covariate grid
# and kriging output back with it, and the main session held that frame for
# the displayed run and for every archived copy of it.

test_that("detach_model_frame drops the fitting frame and keeps the model", {
  fit_in_a_frame <- function() {
    stand_in_for_the_grid <- runif(2e5)
    d <- data.frame(y = rnorm(50), x = rnorm(50))
    # The environment of a formula built here IS this frame.
    lm(as.formula("y ~ x"), data = d)
  }
  m <- fit_in_a_frame()
  # A fitted lm holds the frame TWICE: $terms, and the terms attribute of the
  # model frame it kept in $model. Detaching only the first frees nothing.
  expect_true("stand_in_for_the_grid" %in% ls(attr(m$terms, ".Environment")))
  expect_true("stand_in_for_the_grid" %in% ls(attr(attr(m$model, "terms"), ".Environment")))

  d <- detach_model_frame(m)
  expect_identical(environmentName(attr(d$terms, ".Environment")), "R_GlobalEnv")
  expect_identical(environmentName(attr(attr(d$model, "terms"), ".Environment")), "R_GlobalEnv")
  # The model itself is untouched; only the environment reference is gone.
  expect_equal(coef(d), coef(m))
  expect_equal(summary(d)$r.squared, summary(m)$r.squared)
  # 2e5 doubles is 1.6 MB, so the frame is unmistakable in the payload.
  expect_gt(length(serialize(m, NULL)), 1e6)
  expect_lt(length(serialize(d, NULL)), length(serialize(m, NULL)) / 10)
})

test_that("the pipeline's RK summary and RFK forest carry no pipeline frame", {
  set.seed(4)
  n <- 40
  pts_data <- data.frame(x = 500000 + runif(n, 0, 1200), y = 4400000 + runif(n, 0, 1200),
                         Locality = "L", aux1 = rnorm(n))
  pts_data$v <- 10 + 1.5 * pts_data$aux1 + rnorm(n, 0, 0.5)
  pts_data$pv <- NA_real_
  item <- list(l = "L", pts_data = pts_data,
               m_params = list(idw_p_act = 2, idw_p_pre = 2, idw_nmax = 12,
                               tps_lambda_act = -1, tps_lambda_pre = -1,
                               pre_fit_act = NULL, pre_fit_pre = NULL,
                               cv_strategy = "auto", rfk_uncertainty = "jackknife",
                               rf_ntree = 50, vif_threshold = 10))

  rk <- suppressWarnings(run_regional_interpolation(
    item, "RK", 32635, "aux1", NULL, "convex", "fixed", 200,
    "fixed", 60, "EPSG:32635", FALSE, "actual"))
  expect_s3_class(rk$summ_act, "summary.lm")
  expect_identical(environmentName(attr(rk$summ_act$terms, ".Environment")), "R_GlobalEnv")
  # The reporting path reads these and nothing else.
  expect_equal(nrow(rk$summ_act$coefficients), 2L)
  expect_true(is.finite(rk$summ_act$r.squared))
  # The summary's own content is a few kB; the frame it used to carry is not.
  expect_lt(length(serialize(rk$summ_act, NULL)), 1e5)

  rfk <- suppressWarnings(run_regional_interpolation(
    item, "RFK", 32635, "aux1", NULL, "convex", "fixed", 200,
    "fixed", 60, "EPSG:32635", FALSE, "actual"))
  expect_s3_class(rfk$rf_act, "randomForest")
  expect_identical(environmentName(attr(rfk$rf_act$terms, ".Environment")), "R_GlobalEnv")
  imp <- randomForest::importance(rfk$rf_act)
  expect_equal(rownames(imp), "aux1")
  # A detached model still predicts when it is handed complete newdata.
  expect_true(all(is.finite(predict(rfk$rf_act, data.frame(aux1 = c(-1, 0, 1))))))
})

test_that("the point-error surface resolves an absent IDW neighbour count", {
  # gstat::idw(nmax = NULL) ends the call with "argument is of length zero".
  # The run path resolves idw_nmax before building m_params, so this only bites
  # a caller that builds m_params itself (tests, a restored configuration).
  set.seed(5)
  n <- 30
  pts_data <- data.frame(x = 500000 + runif(n, 0, 1000), y = 4400000 + runif(n, 0, 1000),
                         Locality = "L", v = rnorm(n, 10, 2))
  pts_data$pv <- pts_data$v + rnorm(n, 0, 0.3)
  item <- list(l = "L", pts_data = pts_data,
               m_params = list(idw_p_act = 2, idw_p_pre = 2,
                               tps_lambda_act = -1, tps_lambda_pre = -1,
                               pre_fit_act = NULL, pre_fit_pre = NULL,
                               cv_strategy = "auto", rfk_uncertainty = "jackknife"))
  res <- suppressWarnings(run_regional_interpolation(
    item, "OK", 32635, character(0), NULL, "convex", "fixed", 200,
    "fixed", 60, "EPSG:32635", TRUE, "actual"))
  expect_false(is.null(res$r_point_err))
  expect_s4_class(terra::unwrap(res$r_point_err), "SpatRaster")
})
