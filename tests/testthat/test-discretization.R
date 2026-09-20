# test-discretization.R — tests for discretize_numeric_var.

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

# ── Edge cases ────────────────────────────────────────────────────────────

test_that("degenerate inputs still produce a usable factor", {
  # Two points: the median falls between them, so each lands in its own class.
  two <- discretize_numeric_var(c(1, 2), method = "median")
  expect_length(levels(two), 2L)
  expect_equal(as.integer(two), c(1L, 2L))

  # A constant column has a mean but no spread: both labels still exist and
  # every row is <= the mean, so the panel shows one populated group.
  flat <- discretize_numeric_var(rep(10, 10), method = "mean")
  expect_length(levels(flat), 2L)
  expect_true(all(as.integer(flat) == 1L))

  # An infinite value does not break the median split (it would the mean one).
  inf <- discretize_numeric_var(c(1, 2, 3, Inf, 5), method = "median")
  expect_length(levels(inf), 2L)
  expect_equal(as.integer(inf), c(1L, 1L, 1L, 2L, 2L))
})

test_that("discretize_numeric_var handles negative values", {
  x <- c(-10, -5, 0, 5, 10)
  result <- discretize_numeric_var(x, method = "median")
  expect_s3_class(result, "factor")
  expect_length(levels(result), 2)
})

# ── Numeric contract: where the split actually falls ───────────────────────

test_that("tertile and quintile splits cut at the sample quantiles", {
  x <- golden_soil("full")$ph

  f3 <- discretize_numeric_var(x, "tertiles")
  q3 <- unname(quantile(x, probs = c(0, 1 / 3, 2 / 3, 1), na.rm = TRUE))
  expect_equal(levels(f3), c("Low", "Medium", "High"))
  # The boundary is read off the data rather than off cut(): the largest value
  # in a class must not exceed its upper quantile, and the smallest value in
  # the next class must exceed it.
  expect_lte(max(x[f3 == "Low"]), q3[2])
  expect_gt(min(x[f3 == "Medium"]), q3[2])
  expect_lte(max(x[f3 == "Medium"]), q3[3])
  expect_gt(min(x[f3 == "High"]), q3[3])

  f5 <- discretize_numeric_var(x, "quintiles")
  q5 <- unname(quantile(x, probs = seq(0, 1, by = 0.2), na.rm = TRUE))
  expect_equal(levels(f5), paste0("Q", 1:5))
  for (j in 1:4) {
    expect_lte(max(x[f5 == paste0("Q", j)]), q5[j + 1])
    expect_gt(min(x[f5 == paste0("Q", j + 1)]), q5[j + 1])
  }
})

test_that("the median and mean splits cut at median() and mean()", {
  x <- golden_soil("full")$som

  fm <- discretize_numeric_var(x, "median")
  med <- median(x)
  expect_equal(levels(fm), c("<= Median", "> Median"))
  expect_lte(max(x[as.integer(fm) == 1L]), med)
  expect_gt(min(x[as.integer(fm) == 2L]), med)

  fa <- discretize_numeric_var(x, "mean")
  mu <- mean(x)
  expect_equal(levels(fa), c("<= Mean", "> Mean"))
  expect_lte(max(x[as.integer(fa) == 1L]), mu)
  expect_gt(min(x[as.integer(fa) == 2L]), mu)

  # A skewed variable puts the two splits in different places; if these agreed
  # the test could not tell median from mean.
  expect_false(identical(as.integer(fm), as.integer(fa)))
})
