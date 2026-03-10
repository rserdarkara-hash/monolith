library(testthat)

test_that("V2.0: Covariate interpolation should use OK instead of IDW (Issue E)", {
  app_content <- readLines("../../app_v2.0.R")
  apply_interp_start <- grep("apply_interpolation <- function", app_content)
  
  rk_start <- grep("else if\(method == "RK"", app_content)
  rk_start <- rk_start[rk_start > apply_interp_start[1]][1]
  
  rfk_start <- grep("else if\(method == "RFK"", app_content)
  rfk_start <- rfk_start[rfk_start > apply_interp_start[1]][1]
  
  ck_start <- grep("else if\(method == "CK"", app_content)
  ck_start <- ck_start[ck_start > apply_interp_start[1]][1]
  
  rk_block <- app_content[rk_start:rfk_start]
  rfk_block <- app_content[rfk_start:ck_start]
  
  has_idw_rk <- any(grepl("res_av <- idw\(", rk_block))
  has_idw_rfk <- any(grepl("res_av <- idw\(", rfk_block))
  
  has_ok_rk <- any(grepl("res_av <- krige\(", rk_block))
  has_ok_rfk <- any(grepl("res_av <- krige\(", rfk_block))
  
  expect_false(has_idw_rk, info = "RK should not use IDW for covariate smoothing")
  expect_false(has_idw_rfk, info = "RFK should not use IDW for covariate smoothing")
  
  expect_true(has_ok_rk, info = "RK should use OK (krige) for covariate smoothing")
  expect_true(has_ok_rfk, info = "RFK should use OK (krige) for covariate smoothing")
})