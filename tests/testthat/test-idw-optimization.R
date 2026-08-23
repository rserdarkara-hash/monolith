# test-idw-optimization.R — tests for optimize_idw_p.

test_that("optimize_idw_p returns value in valid range [0.5, 5.0]", {
  pts <- make_test_points(15)
  p <- optimize_idw_p(pts, "v", nmax = 12)
  expect_true(p >= 0.5 && p <= 5.0)
})

test_that("optimize_idw_p returns fallback when CV fails on too-few points", {
  # A single point cannot be cross-validated: every candidate scores a
  # non-finite RMSE, so the search declines to pick a winner and returns the
  # documented IDW default of 2.0 (the same value idw_opt_item's small-n guard
  # and get_regional_param fall back to).
  pts <- make_test_points(1)
  p <- suppressWarnings(optimize_idw_p(pts, "v", nmax = 12))
  expect_identical(p, 2.0)
})

test_that("optimize_idw_p works with small dataset (n <= 50, LOOCV)", {
  pts <- make_test_points(12)
  p <- optimize_idw_p(pts, "v", nmax = 8)
  expect_true(p >= 0.5 && p <= 5.0)
})

test_that("optimize_idw_p works with larger dataset (n > 50, random 10-fold)", {
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

# ── cv_strategy plumbing (2026-08-23 audit, Tier 3) ────────────────────────
# The power search shares the run's fold authority. These pin that it really is
# make_cv_folds/resolve_cv_plan choosing the folds, not a private scheme.

test_that("optimize_idw_p reproduces a hand-built search on make_cv_folds folds", {
  pts <- make_test_points(60, seed = 7)
  for (strategy in c("auto", "loocv", "block")) {
    folds <- make_cv_folds(sf::st_coordinates(pts), strategy, nrow(pts), CV_FOLD_SEED)
    factors <- seq(0.5, 5.0, by = 0.5)
    rmses <- vapply(factors, function(f) {
      cv <- gstat::krige.cv(v ~ 1, pts, nmax = 12, set = list(idp = f),
                            nfold = folds, debug.level = 0)
      sqrt(mean(cv$residual^2, na.rm = TRUE))
    }, numeric(1))
    expect_identical(optimize_idw_p(pts, "v", nmax = 12, cv_strategy = strategy),
                     factors[which.min(rmses)],
                     info = strategy)
  }
})

test_that("optimize_idw_p folds follow the resolved plan, not a fixed 5-fold", {
  # n = 60: auto resolves to random 10-fold and loocv to 60 folds, so the two
  # score the same candidate powers on genuinely different partitions.
  pts <- make_test_points(60, seed = 11)
  coords <- sf::st_coordinates(pts)
  expect_length(unique(make_cv_folds(coords, "auto", 60, CV_FOLD_SEED)), 10)
  expect_length(unique(make_cv_folds(coords, "loocv", 60, CV_FOLD_SEED)), 60)
  expect_length(unique(make_cv_folds(coords, "block", 60, CV_FOLD_SEED)), 10)
  for (strategy in c("auto", "loocv", "block")) {
    p <- optimize_idw_p(pts, "v", nmax = 12, cv_strategy = strategy)
    expect_true(p >= 0.5 && p <= 5.0)
  }
})

test_that("optimize_idw_p leaves the caller's RNG stream untouched", {
  pts <- make_test_points(40, seed = 3)
  set.seed(999)
  before <- .Random.seed
  invisible(optimize_idw_p(pts, "v", nmax = 12, cv_strategy = "block"))
  expect_identical(.Random.seed, before)
})

test_that("optimize_idw_p defaults to the auto plan when no strategy is given", {
  pts <- make_test_points(60, seed = 5)
  expect_identical(optimize_idw_p(pts, "v", nmax = 12),
                   optimize_idw_p(pts, "v", nmax = 12, cv_strategy = "auto"))
})
