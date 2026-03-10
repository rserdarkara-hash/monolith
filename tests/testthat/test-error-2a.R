library(testthat)

# This test checks for Error 2A in app_v1.9.R

test_that("Error 2A: TPS should use isotropic scaling", {
  app_file <- "../../app_v1.9.R"
  
  if(!file.exists(app_file)) {
    expect_true(FALSE, info = "app_v1.9.R does not exist.")
    return()
  }
  
  app_content <- readLines(app_file)
  
  # Search for isotropic scaling logic using fixed = TRUE
  iso_range_string <- "max_range <- max(xM - xm, yM - ym)"
  iso_range_line <- grep(iso_range_string, app_content, fixed = TRUE)
  
  iso_scaling_string <- "/max_range"
  iso_scaling_line <- grep(iso_scaling_string, app_content, fixed = TRUE)
  
  # Check if it exists in the file
  is_fixed <- length(iso_range_line) > 0 && length(iso_scaling_line) > 0
  
  expect_true(is_fixed, info = "The TPS implementation is still using independent (anisotropic) scaling for X and Y axes.")
})
