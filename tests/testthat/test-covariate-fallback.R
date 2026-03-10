library(testthat)

test_that("V2.1: apply_interpolation should wrap covariate kriging in tryCatch with IDW fallback", {
  app_content <- readLines("../../app_v2.1.R")
  
  # Look for the fallback implementation
  trycatch_line <- grep("tryCatch\\(\\{ showNotification\\(sprintf\\(\"Covariate %s kriging failed", app_content)
  idw_fallback_line <- grep("res_av <- idw\\(as\\.formula\\(paste\\(av, \"~ 1\"\\)\\), data, grid_p, nmax = idw_nmax", app_content)
  
  expect_true(length(trycatch_line) >= 2, info = "tryCatch with notification should be present in both RK and RFK blocks")
  expect_true(length(idw_fallback_line) >= 2, info = "IDW fallback should be implemented in both RK and RFK blocks")
})
