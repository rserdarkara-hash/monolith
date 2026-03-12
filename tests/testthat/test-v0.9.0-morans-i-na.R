library(testthat)

test_that("calc_moran gracefully handles identical coordinates or pure nugget", {
  source("../../improvements/spatial_helpers_0.9.0.R")
  
  # Mock coordinates that could cause issues (all same or very few)
  coords <- data.frame(x = c(1, 1, 1), y = c(1, 1, 1))
  residuals <- c(0.1, -0.2, 0.1)
  
  # This should return NA gracefully without crashing
  res <- calc_moran(residuals, coords)
  expect_true(is.na(res), info = "calc_moran should return NA for invalid geometries")
})

test_that("UI handles Moran's I NA by showing 'No Spatial Structure'", {
  app_file <- "../../monolith_ver_0.9.0.R"
  if(!file.exists(app_file)) skip("monolith_ver_0.9.0.R not found")
  
  content <- readLines(app_file)
  
  # We expect to see some logic that converts NA to "No Spatial Structure Detected"
  # This is the failing test; it should fail until we implement the fix
  has_na_handling <- any(grepl("No Spatial Structure Detected|NA.*Spatial Structure", content))
  
  expect_true(has_na_handling, info = "The app should handle Moran's I NA by rendering 'No Spatial Structure Detected'")
})
