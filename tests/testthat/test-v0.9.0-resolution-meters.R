library(testthat)

test_that("Resolution is always reported in meters in UI logic", {
  app_file <- "../../monolith_ver_0.9.0.R"
  if(!file.exists(app_file)) skip("monolith_ver_0.9.0.R not found")
  
  content <- readLines(app_file)
  
  # Check that we don't have updateSliderInput setting label to "Resolution (Degrees)" anymore
  has_degrees_label <- any(grepl("Resolution \\(Degrees\\)", content))
  expect_false(has_degrees_label, info = "The app should no longer report resolution in degrees in the slider")
  
  # Check if sf::st_transform is used to force metric distance for local res
  # We look for something like st_transform(., 3857) or similar metric CRS for distance calculation
  has_metric_transform <- any(grepl("st_transform.*3857", content))
  expect_true(has_metric_transform, info = "The app should use a metric projection (e.g. EPSG:3857) to compute resolution in meters")
})
