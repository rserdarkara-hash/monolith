library(testthat)

test_that("V2.3 Export UI: Assets registry and Styler buttons exist", {
  app_content <- readLines("../../app_v2.3.R")
  
  # Check for Asset List UI
  asset_ui <- grep("uiOutput\\(\"export_registry_ui\"\\)", app_content)
  expect_true(length(asset_ui) > 0, info = "Export Panel must contain export_registry_ui")
  
  # Check for Styler Button trigger
  styler_btn <- grep("actionButton\\(\"open_styler\"", app_content)
  expect_true(length(styler_btn) > 0, info = "Export Panel must contain open_styler button")
})
