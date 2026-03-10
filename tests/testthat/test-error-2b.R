library(testthat)

# This test checks for Error 2B in app_v1.9.R

test_that("Error 2B: TPS should not use manual grid search for lambda", {
  app_file <- "../../app_v1.9.R"
  
  if(!file.exists(app_file)) {
    expect_true(FALSE, info = "app_v1.9.R does not exist.")
    return()
  }
  
  app_content <- readLines(app_file)
  
  # Search for the manual loop which should be REMOVED
  manual_loop_string <- "for(j in seq_along(lambdas))"
  manual_loop_line <- grep(manual_loop_string, app_content, fixed = TRUE)
  
  # Search for the correct internal optimization call
  # Correct: mod <- fields::Tps(pts, vals) # without lambda parameter, it optimizes by GCV
  internal_opt_string <- "mod <- fields::Tps(pts, vals)"
  internal_opt_line <- grep(internal_opt_string, app_content, fixed = TRUE)
  
  # Check if fixed
  is_fixed <- length(manual_loop_line) == 0 && length(internal_opt_line) > 0
  
  expect_true(is_fixed, info = "The TPS optimization still contains a manual lambda grid search loop.")
})
