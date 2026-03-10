library(testthat)
library(shiny)

test_that("rv$has_predictions is correctly initialized and tracks interpolation state", {
  # Mock interactive mode to prevent shinyApp() from starting
  Sys.setenv(SHINY_PORT = "")
  
  # Source the app to get the server function
  source("../../monolith_ver_0.8.8.R", local = TRUE, chdir = TRUE)
  
  testServer(server, {
    # Check that rv$has_predictions is initialized to FALSE
    expect_true(exists("rv"), info = "rv should be initialized.")
    expect_true(!is.null(rv$has_predictions), info = "rv$has_predictions should be defined.")
    expect_false(rv$has_predictions, info = "rv$has_predictions should be FALSE initially.")
    
    # Simulate an interpolation run by setting some prediction data
    # (assuming setting prediction list updates state)
    rv$rast_list_pre <- list("Var1" = "mock_raster")
    
    # Note: testing if the actual observeEvent updates this requires triggering it,
    # but we will just ensure the state variable itself exists for now as a failing test.
  })
})
