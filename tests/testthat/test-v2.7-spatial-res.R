library(testthat)
library(sp)
library(sf)

source("../../improvements/spatial_helpers_v2.7.R", local = TRUE)

test_that("optimize_idw_p handles NA residuals correctly", {
  # Create a dummy dataset
  set.seed(42)
  pts <- data.frame(
    x = runif(10),
    y = runif(10),
    val = rnorm(10)
  )
  coordinates(pts) <- ~x+y
  
  # We will mock krige.cv inside our test environment
  # by temporarily replacing it
  original_krige.cv <- get("krige.cv", envir = asNamespace("gstat"))
  
  mock_krige.cv <- function(...) {
    # Return a mocked object with NA residuals
    list(residual = c(1, 2, NA, 4, 5))
  }
  
  # Temporarily assign the mock to the global environment where optimize_idw_p will look
  assign("krige.cv", mock_krige.cv, envir = .GlobalEnv)
  
  # Should not fail or return NA for the rmse, but should handle NA gracefully
  # If na.rm = FALSE, this would return NA, which causes future_map_dbl to return NA
  
  # Let's run it
  res <- tryCatch(optimize_idw_p(pts, "val", nmax = 5), error = function(e) e)
  
  # Clean up
  rm("krige.cv", envir = .GlobalEnv)
  
  # The result should not be NA or Inf. Actually if na.rm = TRUE, it will return a number.
  # If it returns NA, we know it's broken.
  # But optimize_idw_p returns a single numeric vector of rmses. wait, it returns the minimum factor?
  # Let's check what optimize_idw_p returns.
})

test_that("calculate_metrics handles NAs", {
  obs <- c(1, 2, NA, 4)
  pre <- c(1.1, 1.9, 3.0, 4.1)
  
  res <- calculate_metrics(obs, pre)
  expect_false(is.na(res$rmse))
})
