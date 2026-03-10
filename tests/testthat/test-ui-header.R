library(testthat)
library(shiny)

test_that("Banner image is present and text title is removed", {
  # Mock interactive mode to prevent shinyApp() from starting when sourced
  Sys.setenv(SHINY_PORT = "")
  
  source("../../monolith_0.8.7.R", local = TRUE, chdir = TRUE)
  
  # Check if "Monolith" text title is gone from the main header
  # Note: We search for it in the stringified UI
  ui_str <- as.character(ui)
  
  # The original title was likely in a titlePanel or h2
  # We expect it NOT to be there as a plain text "Monolith" in the header
  # This might be tricky if "Monolith" is used elsewhere, so we look for specific patterns
  # For now, let's look for the banner image
  expect_true(grepl("src=\"assets/banner.png\"", ui_str) || grepl("src='assets/banner.png'", ui_str), 
              info = "Banner image with src='assets/banner.png' should be present in the UI.")
  
  # And ensure the old title is removed (this depends on how it was implemented)
  # If it was titlePanel("Monolith"), it would produce <title>Monolith</title> and a header.
  # Let's assume we want to remove the visible header text.
  expect_false(grepl("<h2>Monolith</h2>", ui_str), info = "Old text title 'Monolith' should be removed.")
})
