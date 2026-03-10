library(testthat)
library(sf)
library(dplyr)
library(gstat)
library(randomForest)
library(fields)

# Source the helpers
source("../../improvements/spatial_helpers_v1.9.R")

test_that("V1.9: perform_rk_cv should NOT have covariate interpolation block (Error 1A Fixed)", {
  helpers_content <- readLines("../../improvements/spatial_helpers_v1.9.R")
  rk_cv_lines <- grep("perform_rk_cv <- function", helpers_content)
  interpolation_block <- grep("res_av <- idw", helpers_content)
  
  # Error 1A is FIXED if idw is NOT called within the first 30 lines of the function
  is_bugged <- any(interpolation_block > rk_cv_lines[1] & interpolation_block < (rk_cv_lines[1] + 30))
  expect_false(is_bugged)
})

test_that("V1.9: perform_rfk_cv should use OOB residuals (Error 1B Fixed)", {
  helpers_content <- readLines("../../improvements/spatial_helpers_v1.9.R")
  rfk_cv_lines <- grep("perform_rfk_cv <- function", helpers_content)
  oob_string <- "rf_mod$predicted"
  oob_line <- grep(oob_string, helpers_content, fixed = TRUE)
  
  # Error 1B is FIXED if OOB residual calculation is present
  is_fixed <- any(oob_line > rfk_cv_lines[1] & oob_line < (rfk_cv_lines[1] + 30))
  expect_true(is_fixed)
})

test_that("V1.9: opt_tps should use isotropic scaling and internal optimization (Errors 2A, 2B Fixed)", {
  app_content <- readLines("../../app_v1.9.R")
  opt_tps_start <- grep("observeEvent\\(input\\$opt_tps", app_content)
  
  # Check for Error 2A Fix (Isotropic scaling)
  iso_scaling <- grep("max_range <- max(xM - xm, yM - ym)", app_content, fixed = TRUE)
  expect_true(any(iso_scaling > opt_tps_start[1] & iso_scaling < (opt_tps_start[1] + 150)))
  
  # Check for Error 2B Fix (Internal optimization)
  internal_opt <- grep("mod <- fields::Tps(pts, vals)", app_content, fixed = TRUE)
  expect_true(any(internal_opt > opt_tps_start[1] & internal_opt < (opt_tps_start[1] + 150)))
  
  # Verify manual loop is GONE
  manual_loop <- grep("for(j in seq_along(lambdas))", app_content, fixed = TRUE)
  is_bugged_2b <- any(manual_loop > opt_tps_start[1] & manual_loop < (opt_tps_start[1] + 150))
  expect_false(is_bugged_2b)
})

test_that("Baseline: calc_moran still uses dense matrix (To be fixed in Phase 4)", {
  helpers_content <- readLines("../../improvements/spatial_helpers_v1.9.R")
  moran_lines <- grep("calc_moran <- function", helpers_content)
  dense_matrix <- grep("dist(coords)", helpers_content, fixed = TRUE)
  
  expect_true(any(dense_matrix > moran_lines[1] & dense_matrix < (moran_lines[1] + 40)))
})
