library(testthat)
library(sf)
library(dplyr)
library(gstat)

# This test checks for Error 1A in spatial_helpers_v1.9.R

test_that("Error 1A: perform_rk_cv should not interpolate covariates", {
  # We will implement the fix in improvements/spatial_helpers_v1.9.R
  # For now, let's make it point to the file we're about to create/modify
  helpers_file <- "../../improvements/spatial_helpers_v1.9.R"
  
  if(!file.exists(helpers_file)) {
    # If it doesn't exist yet, it's effectively "failed" to be fixed
    expect_true(FALSE, info = "The v1.9 helpers file has not been created yet.")
    return()
  }
  
  helpers_content <- readLines(helpers_file)
  rk_cv_lines <- grep("perform_rk_cv <- function", helpers_content)
  interpolation_line <- grep("res_av <- idw\\(as\\.formula", helpers_content)
  
  # Error 1A is present if idw is called within the loop
  is_bugged <- any(interpolation_line > rk_cv_lines[1] & interpolation_line < (rk_cv_lines[1] + 30))
  
  expect_false(is_bugged, info = "The RK CV loop still contains the unnecessary IDW interpolation of covariates.")
})
