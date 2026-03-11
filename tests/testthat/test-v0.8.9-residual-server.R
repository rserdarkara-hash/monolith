library(testthat)

test_that("Server logic renders both residual maps simultaneously", {
  server_code <- paste(readLines("../../monolith_ver_0.8.9.R"), collapse = "\n")
  
  # Check that comp_map_left and comp_map_right are present (they already are, but we'll check logic later)
  expect_true(grepl("output\\$comp_map_left\\s*<-", server_code))
  expect_true(grepl("output\\$comp_map_right\\s*<-", server_code))
  
  # This should fail initially since the server code still contains 'input$resid_type'
  expect_false(grepl("input\\$resid_type", server_code), info = "resid_type should no longer be referenced in the server code")
})