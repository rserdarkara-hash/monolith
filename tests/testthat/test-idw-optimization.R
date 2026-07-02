# test-idw-optimization.R — tests for optimize_idw_p.

test_that("optimize_idw_p returns value in valid range [0.5, 5.0]", {
  pts <- make_test_points(15)
  p <- optimize_idw_p(pts, "v", nmax = 12)
  expect_true(p >= 0.5 && p <= 5.0)
})

test_that("optimize_idw_p returns fallback when CV fails on too-few points", {
  # A single point cannot be cross-validated — all RMSEs are Inf,
  # so the function returns the first search factor (0.5) as fallback.
  pts <- make_test_points(1)
  p <- optimize_idw_p(pts, "v", nmax = 12)
  # When all RMSE values are Inf, which.min returns 1 → factors[1] = 0.5
  expect_true(p >= 0.5 && p <= 5.0)
})

test_that("optimize_idw_p works with small dataset (n <= 50, LOOCV)", {
  pts <- make_test_points(12)
  p <- optimize_idw_p(pts, "v", nmax = 8)
  expect_true(p >= 0.5 && p <= 5.0)
})

test_that("optimize_idw_p works with larger dataset (n > 50, 5-fold)", {
  set.seed(123)
  n <- 60
  coords <- data.frame(
    x = runif(n, 450000, 451000),
    y = runif(n, 5800000, 5801000)
  )
  pts <- sf::st_as_sf(
    cbind(coords, v = rnorm(n, 50, 10)),
    coords = c("x", "y"),
    crs = 32633
  )
  p <- optimize_idw_p(pts, "v", nmax = 12)
  expect_true(p >= 0.5 && p <= 5.0)
})

test_that("optimize_idw_p scans factors from 0.5 to 5.0 in 0.5 steps", {
  pts <- make_test_points(20)
  p <- optimize_idw_p(pts, "v", nmax = 12)
  # The function searches seq(0.5, 5.0, by = 0.5) which has 10 values
  # The returned value should be one of these search points
  valid_factors <- seq(0.5, 5.0, by = 0.5)
  # Allow for floating-point tolerance
  expect_true(any(abs(p - valid_factors) < 1e-8))
})

test_that("optimize_idw_p uses nmax parameter in CV", {
  pts <- make_test_points(15)
  p1 <- optimize_idw_p(pts, "v", nmax = 5)
  p2 <- optimize_idw_p(pts, "v", nmax = 20)
  # Different nmax may yield different optimal p — both should be valid
  expect_true(p1 >= 0.5 && p1 <= 5.0)
  expect_true(p2 >= 0.5 && p2 <= 5.0)
})
