# test-idw-optimization.R — tests for optimize_idw_p.

test_that("optimize_idw_p returns fallback when CV fails on too-few points", {
  # A single point cannot be cross-validated: every candidate scores a
  # non-finite RMSE, so the search declines to pick a winner and returns the
  # documented IDW default of 2.0 (the same value idw_opt_item's small-n guard
  # and get_regional_param fall back to).
  pts <- make_test_points(1)
  p <- suppressWarnings(optimize_idw_p(pts, "v", nmax = 12))
  expect_identical(p, 2.0)
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

test_that("the three strategies resolve to genuinely different partitions", {
  # The premise the hand-built search above rests on: at n = 60, auto resolves
  # to random 10-fold, loocv to 60 folds and block to 10 spatial clusters. If
  # these collapsed onto one partition, the strategy loop would still pass
  # while proving nothing about the fold authority.
  pts <- make_test_points(60, seed = 11)
  coords <- sf::st_coordinates(pts)
  expect_length(unique(make_cv_folds(coords, "auto", 60, CV_FOLD_SEED)), 10)
  expect_length(unique(make_cv_folds(coords, "loocv", 60, CV_FOLD_SEED)), 60)
  expect_length(unique(make_cv_folds(coords, "block", 60, CV_FOLD_SEED)), 10)
  expect_false(identical(make_cv_folds(coords, "auto", 60, CV_FOLD_SEED),
                         make_cv_folds(coords, "block", 60, CV_FOLD_SEED)))
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

test_that("optimize_idw_p survives a NULL nmax from an unrendered slider", {
  # The optimizer observer passes input$idw_nmax through untouched, so before
  # the guard a collapsed or not-yet-rendered IDW Max Neighbors slider reached
  # gstat as NULL and the whole optimization failed with "argument is of
  # length zero". NULL has to mean the documented default of 12, which is what
  # the run path and apply_IDW already resolve it to.
  pts <- make_test_points(60)
  expect_identical(optimize_idw_p(pts, "v", nmax = NULL),
                   optimize_idw_p(pts, "v", nmax = 12))
})
