library(testthat)

test_that("V2.3 Export Registry: register_export_item function is defined", {
  app_content <- readLines("../../app_v2.3.R")
  
  # Check for function definition
  func_def <- grep("register_export_item <- function", app_content)
  
  expect_true(length(func_def) > 0, info = "app_v2.3.R must define register_export_item function")
})

test_that("V2.3 Export Registry: export_registry reactive value is initialized", {
  app_content <- readLines("../../app_v2.3.R")
  
  # Check for initialization in reactiveValues
  registry_init <- grep("export_registry = list\\(\\)", app_content)
  
  expect_true(length(registry_init) > 0, info = "export_registry must be initialized in reactiveValues")
})
