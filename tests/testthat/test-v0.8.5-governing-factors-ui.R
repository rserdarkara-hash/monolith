library(testthat)
library(shiny)

test_that("Governing Factors tab is present in the UI", {
  # Mock interactive mode to prevent shinyApp() from starting when sourced
  Sys.setenv(SHINY_PORT = "")
  
  source("../../monolith_0.8.7.R", local = TRUE, chdir = TRUE)
  
  # Check if "Governing Factors" tab exists anywhere in the stringified UI
  ui_str <- as.character(ui)
  expect_true(grepl("Governing Factors", ui_str), info = "'Governing Factors' tab should be present in the UI structure.")
  
  # Check for specific UI elements for Governing Factors
  expect_true(grepl("target_param", ui_str) || grepl("gov_target", ui_str), info = "Target parameter input should be present.")
  expect_true(grepl("gov_factors", ui_str) || grepl("gov_predictors", ui_str), info = "Governing factors (predictors) input should be present.")
  expect_true(grepl("gov_run", ui_str) || grepl("gov_update", ui_str), info = "Run Analysis button should be present.")
})