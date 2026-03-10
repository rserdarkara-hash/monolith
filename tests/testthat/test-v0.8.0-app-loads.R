library(testthat)
library(shiny)

test_that("App UI and Server initialize without errors", {
  # Mock interactive mode to prevent shinyApp() from starting
  Sys.setenv(SHINY_PORT = "")
  
  # Source the app
  source("../../monolith_0.8.0.R", local = TRUE, chdir = TRUE)
  
  # Check that UI and Server are valid objects
  expect_true(exists("ui"), info = "UI object should be defined.")
  expect_true(exists("server"), info = "Server object should be defined.")
  expect_s3_class(ui, "shiny.tag.list")
  expect_type(server, "closure")
})