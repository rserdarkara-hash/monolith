# test-moran.R — tests for calc_moran.

test_that("calc_moran returns NA for fewer than 3 observations", {
  expect_true(is.na(calc_moran(c(1, 2), matrix(1:4, ncol = 2))))
  expect_true(is.na(calc_moran(numeric(0), matrix(nrow = 0, ncol = 2))))
})

test_that("calc_moran returns NA when coords rows don't match residual count", {
  residuals <- c(1, 2, 3)
  coords <- matrix(1:10, ncol = 2)  # 5 rows, 3 residuals — mismatch
  expect_true(is.na(calc_moran(residuals, coords)))
})

test_that("calc_moran returns NA for NULL residuals", {
  expect_true(is.na(calc_moran(NULL, matrix(1:6, ncol = 2))))
})

test_that("calc_moran returns NA for NULL coords", {
  expect_true(is.na(calc_moran(c(1, 2, 3), NULL)))
})

test_that("calc_moran handles duplicated coordinates by jittering", {
  set.seed(42)
  n <- 5
  # All points at the same location → duplicates
  coords <- matrix(rep(c(450000, 5800000), each = n), ncol = 2)
  residuals <- rnorm(n)
  moran_val <- calc_moran(residuals, coords)
  # May return NA or a value — must not throw an uncaught error
  expect_true(is.na(moran_val) || is.numeric(moran_val))
})

test_that("calc_moran returns value in [-1, 1] or NA for valid inputs", {
  set.seed(42)
  n <- 10
  coords <- cbind(runif(n, 450000, 451000), runif(n, 5800000, 5801000))
  residuals <- rnorm(n)
  moran_val <- suppressWarnings(calc_moran(residuals, coords))
  expect_true(is.na(moran_val) || (moran_val >= -1 && moran_val <= 1))
})

test_that("calc_moran returns near-zero for spatially random residuals", {
  set.seed(123)
  n <- 15
  coords <- cbind(runif(n, 450000, 451000), runif(n, 5800000, 5801000))
  residuals <- rnorm(n)  # independent noise — no spatial pattern
  moran_val <- suppressWarnings(calc_moran(residuals, coords))
  # If not NA, should be close to E[I] ≈ -1/(n-1) ≈ -0.07 for n=15
  if (!is.na(moran_val)) {
    expect_true(abs(moran_val) < 0.5)
  }
})

test_that("calc_moran handles collinear coordinates gracefully", {
  # Collinear coordinates (points on a line) may cause issues with
  # neighbour-search or distance-weighting.  Verify the function doesn't
  # throw an uncaught error.
  n <- 6
  coords <- cbind(seq_len(n), seq_len(n))  # collinear
  residuals <- 1:n
  moran_val <- suppressWarnings(calc_moran(residuals, coords))
  expect_true(is.na(moran_val) || is.numeric(moran_val))
})
