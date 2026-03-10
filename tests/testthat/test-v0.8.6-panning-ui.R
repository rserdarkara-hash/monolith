library(testthat)
library(shiny)

test_that("Locality Panning dropdown is present in the UI", {
  # Mock interactive mode to prevent shinyApp() from starting when sourced
  Sys.setenv(SHINY_PORT = "")
  
  # Source the new version
  source("../../monolith_0.8.6.R", local = TRUE, chdir = TRUE)
  
  # Check if the UI contains the placeholder for the locality panning dropdown
  ui_str <- as.character(ui)
  
  # We expect an uiOutput named "locality_pan_ui"
  expect_true(grepl("locality_pan_ui", ui_str), info = "'locality_pan_ui' output should be present in the UI structure.")
})