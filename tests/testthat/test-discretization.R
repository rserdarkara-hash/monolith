# test-discretization.R — tests for discretize_numeric_var.

test_that("median split produces two factor levels", {
  x <- c(1, 2, 3, 10, 20, 30)
  result <- discretize_numeric_var(x, method = "median")
  expect_s3_class(result, "factor")
  expect_length(levels(result), 2)
  expect_match(levels(result)[1], "<= Median|<= Mean")
})

test_that("mean split produces two factor levels", {
  x <- c(1, 2, 3, 4, 5)
  result <- discretize_numeric_var(x, method = "mean")
  expect_s3_class(result, "factor")
  expect_length(levels(result), 2)
})

test_that("tertiles produce 3 levels", {
  x <- 1:30
  result <- discretize_numeric_var(x, method = "tertiles")
  expect_s3_class(result, "factor")
  expect_length(levels(result), 3)
})

test_that("quintiles produce 5 levels", {
  x <- 1:50
  result <- discretize_numeric_var(x, method = "quintiles")
  expect_s3_class(result, "factor")
  expect_length(levels(result), 5)
})

test_that("quintiles with low-variation data returns single level", {
  x <- rep(5, 20)
  result <- discretize_numeric_var(x, method = "quintiles")
  expect_s3_class(result, "factor")
  expect_match(levels(result)[1], "Low Variation")
})

test_that("custom breaks produce expected bins", {
  x <- c(1, 3, 5, 7, 9, 11, 13, 15)
  result <- discretize_numeric_var(x, method = "custom", custom_breaks = c(5, 10))
  expect_s3_class(result, "factor")
  expect_length(levels(result), 3)  # <=5, (5-10], >10
})

test_that("handles all-NA input", {
  x <- rep(NA_real_, 10)
  result <- discretize_numeric_var(x, method = "median")
  expect_s3_class(result, "factor")
  expect_true(all(is.na(result)))
})

test_that("includes variable name prefix when provided", {
  x <- 1:20
  result <- discretize_numeric_var(x, method = "median", var_name = "pH")
  expect_match(levels(result)[1], "pH:")
})

test_that("median split assigns correctly", {
  x <- c(1, 2, 3, 4, 100)  # median = 3
  result <- discretize_numeric_var(x, method = "median")
  expect_equal(as.character(result[1]), levels(result)[1])  # 1 <= 3
  expect_equal(as.character(result[5]), levels(result)[2])  # 100 > 3
})

# ── Edge cases ────────────────────────────────────────────────────────────

test_that("discretize_numeric_var handles two unique values", {
  x <- c(1, 2)
  result <- discretize_numeric_var(x, method = "median")
  expect_s3_class(result, "factor")
})

test_that("discretize_numeric_var handles single value with mean method", {
  x <- rep(10, 10)
  result <- discretize_numeric_var(x, method = "mean")
  expect_s3_class(result, "factor")
})

test_that("discretize_numeric_var handles negative values", {
  x <- c(-10, -5, 0, 5, 10)
  result <- discretize_numeric_var(x, method = "median")
  expect_s3_class(result, "factor")
  expect_length(levels(result), 2)
})

test_that("discretize_numeric_var handles Inf values", {
  x <- c(1, 2, 3, Inf, 5)
  result <- discretize_numeric_var(x, method = "median")
  expect_s3_class(result, "factor")
})
