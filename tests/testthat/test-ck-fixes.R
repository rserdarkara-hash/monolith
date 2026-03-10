library(testthat)

test_that("V2.0: CK should standardize covariates (Issue B)", {
  app_content <- readLines("../../app_v2.0.R")
  apply_interp_start <- grep("apply_interpolation <- function", app_content)
  
  ck_start <- grep('else if\\(method == "CK"', app_content)
  ck_start <- ck_start[ck_start > apply_interp_start[1]][1]
  
  idw_start <- grep('else if\\(method == "IDW"', app_content)
  idw_start <- idw_start[idw_start > ck_start][1]
  
  ck_block <- app_content[ck_start:idw_start]
  
  has_scale <- any(grepl("scale\\(", ck_block))
  
  expect_true(has_scale, info = "CK covariates must be standardized (e.g., using scale()) prior to gstat.")
})

test_that("V2.0: CK fallback should trigger a UI warning (Issue F)", {
  app_content <- readLines("../../app_v2.0.R")
  apply_interp_start <- grep("apply_interpolation <- function", app_content)
  
  ck_start <- grep('else if\\(method == "CK"', app_content)
  ck_start <- ck_start[ck_start > apply_interp_start[1]][1]
  
  idw_start <- grep('else if\\(method == "IDW"', app_content)
  idw_start <- idw_start[idw_start > ck_start][1]
  
  ck_block <- app_content[ck_start:idw_start]
  
  # The warning should be in the tryCatch block for CK fallback
  has_warning <- any(grepl('showNotification\\(.*type = "warning"', ck_block) | grepl("showNotification\\(.*type = 'warning'", ck_block))
  
  expect_true(has_warning, info = "CK convergence fallback must trigger a UI warning (showNotification).")
})