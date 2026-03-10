library(testthat)

test_that("v2.4 scaffold files exist", {
  expect_true(file.exists("app_v2.4.R"))
  expect_true(file.exists("improvements/ui_helpers_v2.4.R"))
  expect_true(file.exists("improvements/spatial_helpers_v2.4.R"))
  expect_true(file.exists("improvements/theme_helpers_v2.4.R"))
})

test_that("app_v2.4.R sources correct helpers", {
  content <- readLines("app_v2.4.R")
  expect_true(any(grepl("source\("improvements/ui_helpers_v2.4.R"\)", content)))
  expect_true(any(grepl("source\("improvements/spatial_helpers_v2.4.R"\)", content)))
  expect_true(any(grepl("source\("improvements/theme_helpers_v2.4.R"\)", content)))
})
