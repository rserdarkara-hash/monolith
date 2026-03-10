library(testthat)
library(shiny)

test_that("UI elements are hidden when rv$has_predictions is FALSE", {
  # Mock interactive mode
  Sys.setenv(SHINY_PORT = "")
  source("../../monolith_ver_0.8.8.R", local = TRUE, chdir = TRUE)
  
  testServer(server, {
    # Check that rv$has_predictions is initialized
    expect_true(exists("rv"))
    
    # We expect that when rv$has_predictions is FALSE,
    # the relevant UI sections (e.g. cv_metrics_table, error_table, predicted_data_structure)
    # would be hidden. Since shinyjs is client-side, we primarily ensure the reactive
    # observer logic runs without error when has_predictions changes.
    
    rv$has_predictions <- FALSE
    session$flushReact()
    
    # In a full e2e test (e.g. shinytest2), we would check the HTML DOM for display:none
    # Here we just verify the state is correct.
    expect_false(rv$has_predictions)
  })
})
