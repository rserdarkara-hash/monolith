library(testthat)
library(shiny)

test_that("5. Descriptive and Exploratory Suite is present in the UI", {
  # Mock interactive mode to prevent shinyApp() from starting when sourced
  Sys.setenv(SHINY_PORT = "")
  
  source("../../monolith_0.8.0.R", local = TRUE, chdir = TRUE)
  
  # Check if "5. Descriptive and Exploratory Suite" exists anywhere in the stringified UI
  ui_str <- as.character(ui)
  expect_true(grepl("5. Descriptive and Exploratory Suite", ui_str), info = "5. Descriptive and Exploratory Suite should be present in the UI structure.")
})
