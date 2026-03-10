library(testthat)

source("../../app_v2.7.R", local = TRUE)

test_that("apply_interpolation does not use global assignment for log_msg", {
  # We test the scoping by running apply_interpolation on an empty dataset or something that forces an error,
  # and verifying that res$log_msg contains the error without creating a global 'res' variable.
  
  # Remove any global 'res' just in case
  if(exists("res", envir = .GlobalEnv)) rm("res", envir = .GlobalEnv)
  
  pts <- data.frame(x = 1, y = 1, target = 1) # Not enough points for kriging, will fail
  coordinates(pts) <- ~x+y
  
  grid_p <- data.frame(x = 2, y = 2)
  coordinates(grid_p) <- ~x+y
  
  result <- apply_interpolation(
    data = pts,
    target_var = "target",
    method = "OK",
    grid_p = grid_p,
    aux_vars = c(),
    lags = list(width = 1, cutoff = 10),
    method_params = list(pre_fit = NULL),
    l = "test",
    prefix = "act"
  )
  
  # It should capture an error message
  expect_true(nchar(result$log_msg) > 0)
  
  # res should NOT exist in the global environment
  expect_false(exists("res", envir = .GlobalEnv))
})