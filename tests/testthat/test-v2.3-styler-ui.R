library(testthat)

test_that("V2.3 Styler UI: Inputs for Typography, Spacing, and Quality exist", {
  app_content <- readLines("../../app_v2.3.R")
  
  # Check for Typography inputs
  expect_true(any(grepl("selectInput\\(\"styler_font_family\"", app_content)), info = "Styler must contain styler_font_family")
  expect_true(any(grepl("sliderInput\\(\"styler_base_size\"", app_content)), info = "Styler must contain styler_base_size")
  
  # Check for Layout inputs
  expect_true(any(grepl("selectInput\\(\"styler_legend_pos\"", app_content)), info = "Styler must contain styler_legend_pos")
  
  # Check for Quality inputs
  expect_true(any(grepl("numericInput\\(\"styler_dpi\"", app_content)), info = "Styler must contain styler_dpi")
})
