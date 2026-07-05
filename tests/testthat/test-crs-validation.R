# test-crs-validation.R — tests for validate_crs from monolith.R.
# NOTE: validate_crs returns an sf crs object (S3 class 'crs', which is a list),
# not a character string. It uses showNotification on error which requires a
# running Shiny session.

test_that("validate_crs accepts EPSG code string and returns crs object", {
  result <- validate_crs("EPSG:4326")
  expect_s3_class(result, "crs")
})

test_that("validate_crs accepts WKT string and returns crs object", {
  wkt <- 'GEOGCS["WGS 84",DATUM["WGS_1984",SPHEROID["WGS 84",6378137,298.257223563]],PRIMEM["Greenwich",0],UNIT["degree",0.0174532925199433]]'
  result <- validate_crs(wkt)
  expect_s3_class(result, "crs")
})

test_that("validate_crs accepts proj4 string and returns crs object", {
  result <- validate_crs("+proj=longlat +datum=WGS84 +no_defs")
  expect_s3_class(result, "crs")
})

test_that("validate_crs errors on invalid CRS string", {
  expect_error(
    validate_crs("not_a_valid_crs_string_xyz"),
    NULL
  )
})

test_that("validate_crs errors on empty string", {
  expect_error(
    validate_crs(""),
    NULL
  )
})

test_that("validate_crs EPSG:32633 returns a projected CRS", {
  result <- validate_crs("EPSG:32633")
  expect_s3_class(result, "crs")
  # UTM zone 33N is a projected CRS
  expect_false(sf::st_is_longlat(result))
})

# ── calc_metric_spacing ────────────────────────────────────────────────────
# Backs the grid-resolution suggestion: must return metres for BOTH projected
# and geographic analysis CRSs (the grid_res slider is metric and
# interpolation always runs in a projected CRS).

test_that("calc_metric_spacing returns NA for NULL or single-point input", {
  expect_true(is.na(calc_metric_spacing(NULL)$mean_nn))
  pts1 <- sf::st_as_sf(data.frame(x = 500000, y = 5000000),
                       coords = c("x", "y"), crs = 32635)
  expect_true(is.na(calc_metric_spacing(pts1)$mean_nn))
})

test_that("calc_metric_spacing measures projected coordinates directly", {
  # 3 points 100 m apart on a line in UTM 35N
  pts <- sf::st_as_sf(data.frame(x = c(500000, 500100, 500200), y = 5500000),
                      coords = c("x", "y"), crs = 32635)
  sp <- calc_metric_spacing(pts)
  expect_equal(sp$mean_nn, 100, tolerance = 1e-9)
  expect_equal(sp$max_dim, 200, tolerance = 1e-9)
})

test_that("calc_metric_spacing converts geographic coordinates to metres", {
  # Points 0.001 deg longitude apart at ~50N:
  # true spacing ~ 111320 * cos(50 deg) * 0.001 ~ 71.6 m
  pts <- sf::st_as_sf(
    data.frame(x = seq(14, by = 0.001, length.out = 5), y = 50),
    coords = c("x", "y"), crs = 4326
  )
  sp <- calc_metric_spacing(pts)
  expected <- 111320 * cos(50 * pi / 180) * 0.001
  expect_equal(sp$mean_nn, expected, tolerance = 0.02)
  # crucially metres, not degrees
  expect_gt(sp$mean_nn, 1)
  expect_gt(sp$max_dim, 4 * sp$mean_nn * 0.98)
})
