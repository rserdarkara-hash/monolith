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

test_that("zero-variance column is pruned instead of crashing the VIF path", {
  df <- make_test_df(10)
  df$const <- 5
  # A constant column used to produce NA rows in the correlation matrix and
  # crash the solve() fallback ("subscript out of bounds"); it is now dropped
  # before the iterative loop.
  res <- suppressWarnings(
    detect_multicollinearity_engine(df, vars = c("a", "b", "const"))
  )
  expect_true("const" %in% res$dropped)
  expect_true(all(c("a", "b") %in% res$kept))
})

test_that("small-unit covariates survive the constant check (scale-free)", {
  # The old absolute floor (var > 1e-6) treated any covariate whose natural
  # units put its variance below 1e-6 as a constant and pruned it before the
  # VIF loop even ran - and constants are dropped even under Keep All, so the
  # user could not rescue it. Fractions, ratios, normalized indices and
  # anything in km or Mg live in this range.
  set.seed(42)
  df <- data.frame(
    ph = rnorm(30, 6.5, 0.4),
    om = rnorm(30, 2.0, 0.5),
    clay_frac = rnorm(30, 0.25, 5e-4)   # 0-1 fraction: var ~ 1.7e-7
  )
  expect_lt(var(df$clay_frac), 1e-6)    # the case the old floor pruned

  res <- suppressWarnings(
    detect_multicollinearity_engine(df, vars = c("ph", "om", "clay_frac"))
  )
  expect_false("clay_frac" %in% res$dropped)
  expect_true("clay_frac" %in% res$kept)
})

test_that("numerically constant columns are still pruned regardless of magnitude", {
  set.seed(7)
  df <- data.frame(
    a = rnorm(20, 5, 1),
    b = rnorm(20, 3, 1),
    # varies only in the ~13th significant digit of its own magnitude: this is
    # the case cor()/solve() genuinely cannot handle
    noise_only = 7.5 + rnorm(20, 0, 1e-13),
    all_zero = 0
  )
  res <- suppressWarnings(
    detect_multicollinearity_engine(df, vars = c("a", "b", "noise_only", "all_zero"))
  )
  expect_true(all(c("noise_only", "all_zero") %in% res$dropped))
  expect_true(all(c("a", "b") %in% res$kept))
})

test_that("infinite vif_threshold (user's Keep All choice) never drops collinear vars", {
  df <- data.frame(
    a = 1:20,
    b = 2 * (1:20) + rnorm(20, 0, 1e-4),  # near-perfectly collinear with a
    c = rnorm(20, 5, 1)
  )
  res <- suppressWarnings(
    detect_multicollinearity_engine(df, vars = c("a", "b", "c"),
                                    vif_threshold = Inf)
  )
  expect_length(res$dropped, 0)
  expect_setequal(res$kept, c("a", "b", "c"))
  # collinearity is still REPORTED (pairs), just not acted upon
  expect_true(res$has_collinearity)
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

test_that("the singular-matrix fallback drops the globally redundant member, whatever the column order", {
  # `ab` is an exact linear combination of `a` and `b`, so the correlation
  # matrix is singular and solve() throws -> the fallback picks the drop. `b`
  # carries four times the variance of `a`, so the maximally correlated pair is
  # (b, ab); of those two, `ab` is the more globally redundant one (it
  # correlates with BOTH other covariates, `b` only with `ab`). The old rule
  # dropped whichever member came first in column order, so the surviving
  # covariate - and hence the fitted model - depended on the upload's layout.
  set.seed(20260811)
  n <- 40
  a <- rnorm(n, 0, 1)
  b <- rnorm(n, 0, 4)
  df <- data.frame(a = a, b = b, ab = a + b)

  res <- detect_multicollinearity_engine(df, vars = c("a", "b", "ab"),
                                         vif_threshold = 10)
  expect_true("ab" %in% res$dropped)
  expect_setequal(res$kept, c("a", "b"))

  # Same data, reversed column order: identical outcome.
  res_rev <- detect_multicollinearity_engine(df[, c("ab", "b", "a")],
                                             vars = c("ab", "b", "a"),
                                             vif_threshold = 10)
  expect_setequal(res_rev$kept, res$kept)
  expect_setequal(res_rev$dropped, res$dropped)
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
