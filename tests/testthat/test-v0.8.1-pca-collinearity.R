library(testthat)

tryCatch(source("../../improvements/ui_helpers_0.8.1.R", local = TRUE, chdir = TRUE), error=function(e) NULL)

test_that("check_collinearity identifies highly correlated variables", {
  set.seed(42)
  df <- data.frame(
    v1 = rnorm(100),
    v2 = rnorm(100),
    v3 = rnorm(100)
  )
  
  # Inject high collinearity
  df$v1 <- df$v2 * 0.99 + rnorm(100) * 0.01
  
  res <- check_collinearity(df, vars = c("v1", "v2", "v3"), threshold = 0.95)
  
  expect_true(res$has_collinearity, info = "Should detect collinearity > 0.95")
  expect_true(any(grepl("v1", res$pairs$var1) | grepl("v1", res$pairs$var2)))
  
  # No collinearity case
  df2 <- data.frame(
    v1 = rnorm(100),
    v2 = rnorm(100),
    v3 = rnorm(100)
  )
  res2 <- check_collinearity(df2, vars = c("v1", "v2", "v3"), threshold = 0.95)
  expect_false(res2$has_collinearity, info = "Should not detect collinearity in random data")
})