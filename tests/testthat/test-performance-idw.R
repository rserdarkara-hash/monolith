library(testthat)

# This test checks for the IDW optimization efficiency fix in spatial_helpers_v1.9.R

test_that("IDW Optimization Efficiency: Grid search should use 0.5 increment", {
  helpers_file <- "../../improvements/spatial_helpers_v1.9.R"
  
  if(!file.exists(helpers_file)) {
    expect_true(FALSE, info = "The v1.9 helpers file does not exist.")
    return()
  }
  
  helpers_content <- readLines(helpers_file)
  opt_idw_lines <- grep("optimize_idw_p <- function", helpers_content)
  
  # Search for the optimized increment
  # Correct: seq(0.5, 5.0, by = 0.5)
  # Bugged:  seq(0.5, 5.0, by = 0.1)
  
  optimized_string <- "seq(0.5, 5.0, by = 0.5)"
  optimized_line <- grep(optimized_string, helpers_content, fixed = TRUE)
  
  # Check if it exists within the function
  is_fixed <- any(optimized_line > opt_idw_lines[1] & optimized_line < (opt_idw_lines[1] + 20))
  
  expect_true(is_fixed, info = "The IDW optimization still uses a slow 0.1 increment.")
})
