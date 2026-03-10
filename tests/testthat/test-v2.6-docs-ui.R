library(testthat)
library(shiny)

test_that("Documentation drawer UI component exists", {
  source("../../app_v2.6.R", local = TRUE)
  
  # Check if a function for the docs drawer exists in the sourced helpers
  expect_true(exists("render_docs_drawer"))
  
  # Test the output of the function
  drawer_html <- as.character(render_docs_drawer())
  expect_true(grepl("docs_drawer", drawer_html) || grepl("docs-drawer", drawer_html))
  expect_true(grepl("tabsetPanel", drawer_html) || grepl("nav-tabs", drawer_html) || grepl("Guide", drawer_html))
})

test_that("Contextual popover icons are injected next to inputs", {
  source("../../improvements/ui_helpers_v2.6.R", local = TRUE)
  
  # Test if we have a helper for info icons
  expect_true(exists("info_tooltip"))
  
  icon_html <- as.character(info_tooltip("test_id", "Test Help"))
  expect_true(grepl("info-circle", icon_html))
  # Should have attributes for a popover or tooltip
  expect_true(grepl("data-toggle", icon_html) || grepl("bsPopover", icon_html) || grepl("tooltip", icon_html) || grepl("onclick", icon_html))
})