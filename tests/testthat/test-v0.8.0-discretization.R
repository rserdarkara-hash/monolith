library(testthat)

# We will source the helpers where this function will live
tryCatch(source("../../improvements/ui_helpers_0.8.0.R", local = TRUE, chdir = TRUE), error=function(e) NULL)

test_that("discretize_numeric_var correctly handles various methods", {
  x <- 1:100
  
  # Median
  res_med <- discretize_numeric_var(x, method = "median")
  expect_equal(levels(res_med), c("Below Median", "Above Median"))
  expect_equal(sum(res_med == "Below Median"), 50)
  
  # Mean
  res_mean <- discretize_numeric_var(x, method = "mean")
  expect_equal(levels(res_mean), c("Below Mean", "Above Mean"))
  expect_equal(sum(res_mean == "Below Mean"), 50)
  
  # Tertiles
  res_tert <- discretize_numeric_var(x, method = "tertiles")
  expect_equal(length(levels(res_tert)), 3)
  
  # Quintiles
  res_quint <- discretize_numeric_var(x, method = "quintiles")
  expect_equal(length(levels(res_quint)), 5)
  
  # Custom
  res_cust <- discretize_numeric_var(x, method = "custom", custom_breaks = c(30, 70))
  expect_equal(length(levels(res_cust)), 3)
  expect_equal(sum(res_cust == "<= 30"), 30)
})