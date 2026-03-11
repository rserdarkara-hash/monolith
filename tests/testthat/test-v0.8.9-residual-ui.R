library(testthat)
library(shiny)

test_that("Residual UI elements do not contain the resid_type dropdown and display descriptive text", {
  # Mock interactive mode
  Sys.setenv(SHINY_PORT = "")
  source("../../monolith_ver_0.8.9.R", local = TRUE, chdir = TRUE)
  
  # The UI should not contain a select input for 'resid_type'
  # We can check by grepping the UI string representation
  ui_str <- as.character(ui)
  
  expect_false(grepl("resid_type", ui_str), info = "resid_type dropdown should be removed")
  expect_true(grepl("Interpolated Delta", ui_str), info = "Text for Interpolated Delta should be in UI")
  expect_true(grepl("Interpolated Point Errors", ui_str), info = "Text for Interpolated Point Errors should be in UI")
})