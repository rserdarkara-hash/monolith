library(testthat)
library(shiny)

test_that("App UI and Server initialize without errors", {
  Sys.setenv(SHINY_PORT = "")
  
  source("../../monolith_0.8.2.R", local = TRUE, chdir = TRUE)
  
  expect_true(exists("ui"), info = "UI object should be defined.")
  expect_true(exists("server"), info = "Server object should be defined.")
  expect_s3_class(ui, "shiny.tag.list")
  expect_type(server, "closure")
})