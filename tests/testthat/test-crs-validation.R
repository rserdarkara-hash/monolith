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
