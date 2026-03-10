library(testthat)

test_that("V2.3 Scaffold: app_v2.3.R sources v2.3 helpers", {
  app_content <- readLines("../../app_v2.3.R")
  
  # Check for v2.3 helper source lines
  ui_helper_line <- grep("source\\(\"improvements/ui_helpers_v2.3.R\"\\)", app_content)
  spatial_helper_line <- grep("source\\(\"improvements/spatial_helpers_v2.3.R\"\\)", app_content)
  
  expect_true(length(ui_helper_line) > 0, info = "app_v2.3.R must source ui_helpers_v2.3.R")
  expect_true(length(spatial_helper_line) > 0, info = "app_v2.3.R must source spatial_helpers_v2.3.R")
  
  # Ensure it does NOT source v2.2 helpers
  ui_helper_old_line <- grep("source\\(\"improvements/ui_helpers_v2.2.R\"\\)", app_content)
  spatial_helper_old_line <- grep("source\\(\"improvements/spatial_helpers_v2.2.R\"\\)", app_content)
  
  expect_true(length(ui_helper_old_line) == 0, info = "app_v2.3.R must not source ui_helpers_v2.2.R")
  expect_true(length(spatial_helper_old_line) == 0, info = "app_v2.3.R must not source spatial_helpers_v2.2.R")
})
