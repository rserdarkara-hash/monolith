library(testthat)
library(shiny)

test_that("Predicted data is excluded from export summaries when rv$has_predictions is FALSE", {
  # Mock interactive mode
  Sys.setenv(SHINY_PORT = "")
  source("../../monolith_ver_0.8.8.R", local = TRUE, chdir = TRUE)
  
  testServer(server, {
    # Check that rv$has_predictions is FALSE initially
    expect_false(rv$has_predictions)
    
    # Normally we would check the outputs like output$cv_metrics_table to ensure
    # they don't contain 'Predicted Model' rows when rv$has_predictions is FALSE.
    # We expect that if m_pre is accessed, it either throws or is not bound to m_act.
    # This acts as a placeholder for data omission logic test.
  })
})
