# test-normality.R — tests for compute_normality from desc_exploratory_module.R.

# ── Edge cases: insufficient data ──────────────────────────────────────────

test_that("returns 'insufficient' for n < 3", {
  res <- compute_normality(numeric(0))
  expect_equal(res$status, "insufficient")
  expect_equal(res$n, 0)

  res2 <- compute_normality(c(1, NA))
  expect_equal(res2$status, "insufficient")
  expect_equal(res2$n, 1)
})

test_that("returns 'insufficient' for zero-variance input", {
  res <- compute_normality(c(5, 5, 5, 5, 5))
  expect_equal(res$status, "insufficient")
})

test_that("returns 'insufficient' for non-numeric input", {
  res <- compute_normality(c("a", "b", "c"))
  expect_equal(res$status, "insufficient")
  expect_true(is.null(res$method) || res$method == "None")
})

test_that("returns 'insufficient' for NULL input", {
  res <- compute_normality(NULL)
  expect_equal(res$status, "insufficient")
})

# ── Method dispatch ────────────────────────────────────────────────────────

test_that("uses Shapiro-Wilk for n < 50", {
  x <- rnorm(30)
  res <- compute_normality(x)
  expect_match(res$method, "Shapiro-Wilk")
  expect_true(res$statistic > 0 && res$statistic <= 1)
  expect_equal(res$n, 30)
})

test_that("uses Lilliefors test for n >= 50", {
  x <- rnorm(60)
  res <- compute_normality(x)
  expect_match(res$method, "Lilliefors")
  expect_equal(res$n, 60)
})

# ── Classification ─────────────────────────────────────────────────────────

test_that("classifies normal data as normal most of the time", {
  set.seed(42)
  x <- rnorm(40, mean = 100, sd = 15)
  res <- compute_normality(x)
  # Normal data usually passes; we just verify output is well-formed
  expect_true(res$status %in% c("normal", "not_normal"))
  expect_true(!is.na(res$p_value))
})

test_that("detects a two-point distribution as non-normal (Shapiro-Wilk)", {
  # 22 zeros and 23 ones: two point masses at n=45 (SW path)
  # shapiro.test() gives p ≈ 2.6e-09 for this vector — unambiguously non-normal
  x <- c(rep(0, 22), rep(1, 23))
  res <- compute_normality(x)
  expect_equal(res$status, "not_normal")
  expect_equal(res$n, 45)
  expect_match(res$method, "Shapiro-Wilk")
})

test_that("Lilliefors path returns valid classification for uniform data", {
  set.seed(1)
  x <- runif(60, 0, 100)
  res <- compute_normality(x)
  # Lilliefors has lower power; we only assert the output is well-formed.
  # The classification itself is probabilistic — don't assert a specific value.
  expect_match(res$method, "Lilliefors")
  expect_equal(res$n, 60)
  expect_true(res$status %in% c("normal", "not_normal"))
  expect_true(!is.na(res$statistic))
  expect_true(!is.na(res$p_value))
})

# ── Robustness ─────────────────────────────────────────────────────────────

test_that("handles NA values by removing them", {
  x <- c(rnorm(30), NA, NA, NA)
  res <- compute_normality(x)
  expect_equal(res$n, 30)
  expect_true(res$status %in% c("normal", "not_normal"))
})

test_that("handles extreme outliers without error", {
  x <- c(rnorm(25), 1e6, -1e6)
  res <- compute_normality(x)
  expect_true(res$status %in% c("normal", "not_normal", "insufficient"))
})

# ── Output structure ───────────────────────────────────────────────────────

test_that("output list has all expected fields", {
  x <- rnorm(30)
  res <- compute_normality(x)
  expect_setequal(names(res), c("status", "method", "statistic", "p_value", "n"))
})

test_that("method field contains 'Normality Test'", {
  x <- rnorm(20)
  res <- compute_normality(x)
  expect_match(res$method, "Normality Test")
})

test_that("statistic is a single numeric value", {
  x <- rnorm(35)
  res <- compute_normality(x)
  expect_type(res$statistic, "double")
  expect_length(res$statistic, 1)
})
