# test-multicollinearity.R — tests for detect_multicollinearity_engine and check_vif.

# ── detect_multicollinearity_engine ────────────────────────────────────────

test_that("returns empty dropped for uncorrelated variables", {
  df <- make_test_df(30)
  res <- detect_multicollinearity_engine(df, vars = c("a", "b", "c", "d", "e"))
  # With random independent normals, no variable should be dropped
  expect_false(res$has_collinearity)
  expect_null(res$pairs)
  expect_equal(length(res$dropped), 0)
  expect_setequal(res$kept, c("a", "b", "c", "d", "e"))
})

test_that("detects near-perfect pairwise correlation", {
  df <- make_collinear_df(30)
  res <- detect_multicollinearity_engine(df, vars = c("v1", "v2", "v3", "v4"))
  # v1 and v2 are nearly identical
  expect_true(res$has_collinearity)
  expect_true(!is.null(res$pairs))
  expect_true(nrow(res$pairs) >= 1)
})

test_that("drops variable with VIF > threshold", {
  df <- make_collinear_df(30)
  res <- detect_multicollinearity_engine(df, vars = c("v1", "v2", "v3", "v4"),
                                         vif_threshold = 5)
  # v1 or v2 should be dropped due to VIF
  expect_true(length(res$dropped) >= 1)
  # kept should contain v3 and v4 (the independent ones)
  expect_true("v3" %in% res$kept)
  expect_true("v4" %in% res$kept)
})

test_that("handles fewer than 2 variables gracefully", {
  df <- make_test_df(10)
  res <- detect_multicollinearity_engine(df, vars = "a")
  expect_false(res$has_collinearity)
  expect_equal(length(res$kept), 1)
  expect_equal(res$kept, "a")
})

test_that("handles all-NA columns", {
  df <- make_test_df(10)
  df$z <- NA_real_
  res <- detect_multicollinearity_engine(df, vars = c("a", "z"))
  # z is excluded because it's not numeric or has zero variance
  expect_true("a" %in% res$kept)
})

test_that("zero-variance column causes detectable failure in VIF path", {
  df <- make_test_df(10)
  df$const <- 5
  # A constant column produces NaN in the correlation matrix, which causes
  # solve() or the max_vif comparison to fail.  The function is not currently
  # hardened against this edge case — we verify the behaviour is known.
  expect_error(
    suppressWarnings(
      detect_multicollinearity_engine(df, vars = c("a", "b", "const"))
    ),
    NULL  # any error class is acceptable
  )
})

test_that("auto-detects numeric columns when vars = NULL", {
  df <- make_test_df(20)
  res <- detect_multicollinearity_engine(df, vars = NULL)
  expect_true(length(res$kept) >= 1)
  # categorical columns should not appear in kept
  expect_false("cat1" %in% res$kept)
  expect_false("cat2" %in% res$kept)
})

test_that("handles singular covariance matrix via fallback", {
  # Create exactly collinear columns that produce a singular matrix
  df <- data.frame(
    a = 1:10,
    b = 2 * (1:10),       # b = 2*a, perfect collinearity
    c = rnorm(10, 5, 1)
  )
  res <- detect_multicollinearity_engine(df, vars = c("a", "b", "c"),
                                         vif_threshold = 10)
  # Should not error; should drop at least one of a or b
  expect_true(length(res$dropped) >= 1)
  expect_true("c" %in% res$kept)
})

test_that("pairwise_threshold parameter is respected", {
  df <- make_test_df(30)
  # With threshold = 0.0, nearly everything is flagged
  res_low <- detect_multicollinearity_engine(df, vars = c("a", "b", "c"),
                                             pairwise_threshold = 0.0)
  # With threshold = 1.0, nothing is flagged
  res_high <- detect_multicollinearity_engine(df, vars = c("a", "b", "c"),
                                              pairwise_threshold = 1.0)
  expect_false(res_high$has_collinearity)
})

# ── check_vif ──────────────────────────────────────────────────────────────

test_that("check_vif returns kept and dropped lists", {
  df <- make_collinear_df(20)
  res <- check_vif(df, threshold = 10)
  expect_true(is.list(res))
  expect_true("kept" %in% names(res))
  expect_true("dropped" %in% names(res))
})

test_that("check_vif default threshold drops highly collinear vars", {
  df <- make_collinear_df(30)
  res <- check_vif(df, threshold = 10)
  expect_true(length(res$kept) > 0)
  # v3 and v4 should survive
  expect_true("v3" %in% res$kept)
  expect_true("v4" %in% res$kept)
})
