library(testthat)

test_that("V2.0: apply_interpolation should use OOB residuals for RFK (Issue A)", {
  app_content <- readLines("../../app_v2.0.R")
  apply_interp_start <- grep("apply_interpolation <- function", app_content)
  
  # Find RFK block
  rfk_start <- grep("else if\\(method == \"RFK\"", app_content)
  rfk_start <- rfk_start[rfk_start > apply_interp_start[1]][1]
  
  # Check for OOB residuals instead of in-bag predict
  oob_line <- grep("data\\$residuals <- data\\$target - rf_mod\\$predicted", app_content)
  inbag_line <- grep("data\\$residuals <- data\\$target - predict\\(rf_mod, data\\)", app_content)
  
  is_fixed <- any(oob_line > rfk_start & oob_line < (rfk_start + 40))
  is_bugged <- any(inbag_line > rfk_start & inbag_line < (rfk_start + 40))
  
  expect_true(is_fixed, info = "RFK must use OOB residuals (rf_mod$predicted)")
  expect_false(is_bugged, info = "RFK must not use in-bag predictions (predict(rf_mod, data))")
})

test_that("V2.0: apply_interpolation should NOT use VIF filtering for RFK (Issue C)", {
  app_content <- readLines("../../app_v2.0.R")
  apply_interp_start <- grep("apply_interpolation <- function", app_content)
  
  # Find RFK block
  rfk_start <- grep("else if\\(method == \"RFK\"", app_content)
  rfk_start <- rfk_start[rfk_start > apply_interp_start[1]][1]
  
  # Find next else if (CK or IDW)
  ck_start <- grep("else if\\(method == \"CK\"", app_content)
  ck_start <- ck_start[ck_start > rfk_start][1]
  
  rfk_block <- app_content[rfk_start:ck_start]
  
  has_vif <- any(grepl("check_vif", rfk_block))
  
  expect_false(has_vif, info = "VIF filtering should be strictly isolated to RK, not used in RFK")
})