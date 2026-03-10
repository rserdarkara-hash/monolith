library(testthat)
library(sf)
library(dplyr)
library(randomForest)

# This test checks for Error 1B in spatial_helpers_v1.9.R

test_that("Error 1B: perform_rfk_cv should use OOB residuals", {
  helpers_file <- "../../improvements/spatial_helpers_v1.9.R"
  
  if(!file.exists(helpers_file)) {
    expect_true(FALSE, info = "The v1.9 helpers file has not been created yet.")
    return()
  }
  
  helpers_content <- readLines(helpers_file)
  rfk_cv_lines <- grep("perform_rfk_cv <- function", helpers_content)
  
  # Search for the OOB version using fixed = TRUE to avoid regex escaping headaches
  oob_string <- "train$residuals <- train[[target_var]] - rf_mod$predicted"
  oob_line <- grep(oob_string, helpers_content, fixed = TRUE)
  
  # It must be within the perform_rfk_cv function
  is_fixed <- any(oob_line > rfk_cv_lines[1] & oob_line < (rfk_cv_lines[1] + 30))
  
  expect_true(is_fixed, info = "The RFK CV loop is still using in-sample predictions for residuals instead of OOB.")
})
