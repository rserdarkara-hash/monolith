test_that("theme switcher UI and server functions exist and work", {
  source("../../theme_helpers.R")
  
  expect_true(exists("theme_switcher_ui"))
  expect_true(exists("theme_switcher_server"))
  
  # Check if UI returns a shiny.tag.list
  ui_output <- theme_switcher_ui("theme_mod")
  expect_s3_class(ui_output, "shiny.tag.list")
})
