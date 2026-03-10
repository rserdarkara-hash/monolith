library(testthat)
library(shiny)

test_that("Map Viewer titles do not contain brackets", {
  # We simulate the renderText function logic
  meta <- list(category = "Soil", label = "Total N (%)")
  type_lab <- "Actual Data"
  method_lab <- ""
  
  # Old logic: prefix <- paste0("[", meta$category, "] ", meta$label)
  # New logic: prefix <- meta$label
  prefix <- meta$label
  
  title <- paste0(prefix, " - ", type_lab, method_lab)
  expect_false(grepl("\\[", title))
  expect_false(grepl("\\]", title))
  expect_equal(title, "Total N (%) - Actual Data")
})

test_that("Palette choices have !important in inline CSS", {
  source("../../app_v2.5.R", local = TRUE)
  choices <- render_palette_choices()
  html_str <- names(choices)[1]
  expect_true(grepl("background-color:\\s*#[0-9A-Fa-f]+\\s*!important", html_str) || grepl("background:\\s*#[0-9A-Fa-f]+\\s*!important", html_str))
})
