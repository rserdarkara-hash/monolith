# test-correlation-plots.R — tests for melt_cormat, generate_correlation_heatmap,
# generate_correlation_network, generate_partial_correlation, generate_correlogram,
# generate_lagged_correlation, and check_collinearity.

# ── melt_cormat ────────────────────────────────────────────────────────────

test_that("melt_cormat produces correct melted format", {
  mat <- matrix(c(1.0, 0.5, 0.5, 1.0), nrow = 2,
                dimnames = list(c("A", "B"), c("A", "B")))
  df <- melt_cormat(mat, "Corr")
  expect_s3_class(df, "data.frame")
  expect_equal(nrow(df), 4)
  expect_setequal(colnames(df), c("Var1", "Var2", "Corr"))
})

# ── generate_correlation_heatmap ───────────────────────────────────────────

test_that("generate_correlation_heatmap returns ggplot for valid data", {
  df <- make_test_df(20)
  p <- generate_correlation_heatmap(df, c("a", "b", "c", "d"))
  expect_s3_class(p, "ggplot")
})

test_that("generate_correlation_heatmap returns ggplot for single var", {
  df <- make_test_df(10)
  p <- generate_correlation_heatmap(df, "a")
  expect_s3_class(p, "ggplot")
})

# ── generate_correlation_network ───────────────────────────────────────────

test_that("generate_correlation_network returns ggplot for valid data", {
  df <- make_test_df(20)
  p <- generate_correlation_network(df, c("a", "b", "c", "d"), threshold = 0.1)
  expect_s3_class(p, "ggplot")
})

test_that("generate_correlation_network respects threshold", {
  df <- make_test_df(20)
  p_high <- generate_correlation_network(df, c("a", "b", "c"), threshold = 0.99)
  p_low  <- generate_correlation_network(df, c("a", "b", "c"), threshold = 0.0)
  expect_s3_class(p_high, "ggplot")
  expect_s3_class(p_low, "ggplot")
})

# ── generate_partial_correlation ───────────────────────────────────────────

test_that("generate_partial_correlation returns ggplot for valid data", {
  df <- make_test_df(20)
  p <- generate_partial_correlation(df, c("a", "b", "c"), control_vars = c("d", "e"))
  expect_s3_class(p, "ggplot")
})

test_that("generate_partial_correlation works without control vars", {
  df <- make_test_df(20)
  p <- generate_partial_correlation(df, c("a", "b", "c"))
  expect_s3_class(p, "ggplot")
})

test_that("generate_partial_correlation returns ggplot for single var", {
  df <- make_test_df(10)
  p <- generate_partial_correlation(df, "a")
  expect_s3_class(p, "ggplot")
})

# ── generate_correlogram ──────────────────────────────────────────────────

test_that("generate_correlogram returns ggplot for valid data", {
  df <- make_test_df(20)
  p <- generate_correlogram(df, c("a", "b", "c", "d"))
  expect_s3_class(p, "ggplot")
})

# ── generate_lagged_correlation ────────────────────────────────────────────

test_that("generate_lagged_correlation returns ggplot for valid data", {
  df <- make_test_df(50)
  p <- generate_lagged_correlation(df, "a", "b", max_lag = 5)
  expect_s3_class(p, "ggplot")
})

test_that("generate_lagged_correlation returns ggplot for nonexistent vars", {
  df <- make_test_df(20)
  p <- generate_lagged_correlation(df, "nonexistent", "b")
  expect_s3_class(p, "ggplot")
})

test_that("generate_lagged_correlation handles insufficient data", {
  df <- make_test_df(5)
  p <- generate_lagged_correlation(df, "a", "b", max_lag = 10)
  expect_s3_class(p, "ggplot")
})

# ── check_collinearity ────────────────────────────────────────────────────

test_that("check_collinearity detects high correlations", {
  df <- make_collinear_df(20)
  result <- check_collinearity(df, c("v1", "v2", "v3", "v4"))
  expect_type(result, "list")
  expect_true("has_collinearity" %in% names(result))
  expect_true("pairs" %in% names(result))
  # v1 and v2 are highly collinear
  expect_true(result$has_collinearity)
})

test_that("check_collinearity returns no collinearity for independent vars", {
  df <- make_test_df(20)
  result <- check_collinearity(df, c("a", "b", "c"))
  expect_false(result$has_collinearity)
})
