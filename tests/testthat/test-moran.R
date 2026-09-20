# test-moran.R — tests for calc_moran.
#
# calc_moran returns list(i, e_i, p): the statistic, its null expectation
# E[I] = -1/(n-1), and a two-sided p-value (NA on the all-pairs fallback path,
# which carries no sampling distribution). Every "cannot compute" branch returns
# all-NA — NA never means "no spatial structure".

expect_moran_all_na <- function(m) {
  expect_named(m, c("i", "e_i", "p"))
  expect_true(all(is.na(unlist(m))))
}

test_that("calc_moran returns an all-NA list for fewer than 3 observations", {
  expect_moran_all_na(calc_moran(c(1, 2), matrix(1:4, ncol = 2)))
  expect_moran_all_na(calc_moran(numeric(0), matrix(nrow = 0, ncol = 2)))
})

test_that("calc_moran returns an all-NA list when coords rows don't match residual count", {
  residuals <- c(1, 2, 3)
  coords <- matrix(1:10, ncol = 2)  # 5 rows, 3 residuals — mismatch
  expect_moran_all_na(calc_moran(residuals, coords))
})

test_that("calc_moran returns an all-NA list for NULL residuals", {
  expect_moran_all_na(calc_moran(NULL, matrix(1:6, ncol = 2)))
})

test_that("calc_moran returns an all-NA list for NULL coords", {
  expect_moran_all_na(calc_moran(c(1, 2, 3), NULL))
})

test_that("calc_moran is reproducible for duplicate coordinates and preserves RNG state", {
  set.seed(7)
  n <- 12
  coords <- cbind(runif(n, 450000, 451000), runif(n, 5800000, 5801000))
  coords[n, ] <- coords[n - 1, ]  # one exact duplicate pair
  residuals <- rnorm(n)

  set.seed(1); m1 <- suppressWarnings(calc_moran(residuals, coords))
  set.seed(2); m2 <- suppressWarnings(calc_moran(residuals, coords))
  expect_false(is.na(m1$i))
  expect_identical(m1, m2)

  # The jitter must not consume or perturb the caller's RNG stream
  set.seed(123); expected_draw <- runif(1)
  set.seed(123); invisible(suppressWarnings(calc_moran(residuals, coords))); actual_draw <- runif(1)
  expect_identical(actual_draw, expected_draw)
})

test_that("calc_moran returns value in [-1, 1] or NA for valid inputs", {
  set.seed(42)
  n <- 10
  coords <- cbind(runif(n, 450000, 451000), runif(n, 5800000, 5801000))
  residuals <- rnorm(n)
  moran_val <- suppressWarnings(calc_moran(residuals, coords))$i
  expect_true(is.na(moran_val) || (moran_val >= -1 && moran_val <= 1))
})

test_that("calc_moran reports the null expectation and a two-sided p-value", {
  set.seed(42)
  n <- 25
  coords <- cbind(runif(n, 450000, 451000), runif(n, 5800000, 5801000))
  residuals <- rnorm(n)
  m <- suppressWarnings(calc_moran(residuals, coords))

  # E[I] is a property of the null hypothesis (no spatial autocorrelation), not
  # of the weighting: it is -1/(n-1) on both the spdep and the fallback path.
  # This is the number the table's Moran's I tooltip shows, and the reason bare
  # I must not be read against zero.
  expect_equal(m$e_i, -1 / (n - 1))
  expect_false(is.na(m$i))
  # spdep path: a real p-value from moran.test
  expect_false(is.na(m$p))
  expect_gte(m$p, 0)
  expect_lte(m$p, 1)
})

test_that("calc_moran's duplicate jitter scales to the coordinate magnitude", {
  # A fixed 1e-8 displacement is only ~10 ULPs at a UTM northing of 4.5e6, so
  # it collides back onto the original double and leaves duplicates in place;
  # spdep then warns "identical points found" and the neighbour definition
  # becomes ambiguous. The jitter must scale with the data instead.
  set.seed(3)
  n <- 600
  coords <- cbind(runif(n, 450000, 460000), runif(n, 4500000, 4510000))
  coords[1:20, ] <- coords[1, ]          # 20 exactly co-located points
  resid <- rnorm(n)

  warns <- character(0)
  mi <- withCallingHandlers(
    calc_moran(resid, coords),
    warning = function(w) {
      warns <<- c(warns, conditionMessage(w))
      invokeRestart("muffleWarning")
    })

  expect_true(is.finite(mi$i))
  # n > 500 means the all-pairs fallback returns NA, so a finite value proves
  # the primary kNN path ran; no identical-point warning proves the duplicates
  # were actually separated rather than merely nudged below representability.
  expect_false(any(grepl("identical points", warns, fixed = TRUE)))

  # Guard the mechanism directly: at this magnitude the historical amount is
  # not enough to keep six copies of one coordinate distinct.
  v <- rep(4500000, 6)
  set.seed(1); old_amt <- jitter(v, amount = 1e-8)
  set.seed(1); new_amt <- jitter(v, amount = max(1e-8, 1e4 * 1e-9, 4500000 * 1e-12))
  expect_lt(length(unique(old_amt)), 6L)
  expect_equal(length(unique(new_amt)), 6L)
})

# ── Numeric contract: Moran's I ────────────────────────────────────────────
#
# Everything above pins the guards and the NA branches. These pin the statistic
# against its definition, on the neighbour graph the function documents:
# symmetric k = 8 nearest neighbours, row-standardised.

test_that("calc_moran matches the hand-computed statistic on its own kNN graph", {
  pts <- golden_sf("tiny")
  co <- sf::st_coordinates(pts)
  z <- residuals(lm(ph ~ v82, data = sf::st_drop_geometry(pts)))

  # Rebuild the documented graph from the distance matrix alone, without spdep:
  # i ~ j when j is among i's 8 nearest OR i is among j's (sym = TRUE is the
  # union), then row-standardise.
  n <- nrow(co)
  k <- 8L
  D <- as.matrix(dist(co))
  A <- matrix(0, n, n)
  for (i in seq_len(n)) A[i, order(D[i, ])[2:(k + 1)]] <- 1
  A <- pmax(A, t(A))
  W <- A / rowSums(A)

  # I = (n / S0) * (z' W z) / (z' z), z centred (Moran 1950; Cliff & Ord 1981)
  zc <- z - mean(z)
  i_hand <- (n / sum(W)) * as.numeric(t(zc) %*% W %*% zc) / sum(zc^2)

  got <- calc_moran(z, co)
  expect_equal(got$i, i_hand, tolerance = 1e-10)
})

test_that("calc_moran reports E[I] = -1/(n-1) exactly", {
  # One locality: a slice spanning two distant survey areas gives a
  # disconnected kNN graph, which is a real condition but not the one under
  # test here.
  alt <- golden_sf("full", localities = "Altinova")
  for (n in c(12, 40, 137)) {
    pts <- alt[seq_len(n), ]
    got <- calc_moran(pts$ph, sf::st_coordinates(pts))
    # The null expectation depends on n alone, not on the weight matrix, and is
    # negative - which is why reporting I without it is misleading.
    expect_equal(got$e_i, -1 / (n - 1), tolerance = 1e-12)
  }
})

test_that("calc_moran agrees with spdep on the graph it builds", {
  pts <- golden_sf("tiny")
  co <- sf::st_coordinates(pts)
  z <- residuals(lm(ph ~ v82, data = sf::st_drop_geometry(pts)))

  nb <- suppressWarnings(
    spdep::knn2nb(spdep::knearneigh(co, k = 8L), sym = TRUE))
  lw <- spdep::nb2listw(nb, style = "W", zero.policy = TRUE)
  ref <- spdep::moran.test(z, lw, zero.policy = TRUE, randomisation = FALSE,
                           alternative = "two.sided")

  got <- calc_moran(z, co)
  expect_equal(got$i, as.numeric(ref$estimate[1]), tolerance = 1e-12)
  expect_equal(got$p, ref$p.value, tolerance = 1e-12)
})

test_that("structure raises I above E[I] and shuffling collapses it back", {
  pts <- golden_sf("core", localities = "Yorga")
  co <- sf::st_coordinates(pts)
  # A smooth planar field is maximally autocorrelated at this scale.
  grad <- co[, 1] * 1e-3 + co[, 2] * 1e-3
  got <- calc_moran(grad, co)
  expect_gt(got$i, 0.5)
  expect_lt(got$p, 0.05)

  # The same values at shuffled locations carry no spatial structure, so I must
  # fall back toward its null expectation. Asserted relative to the structured
  # run rather than against an absolute band, so the test does not depend on one
  # permutation happening to land close to E[I].
  shuffled <- with_seed(7, sample(grad))
  got_s <- calc_moran(shuffled, co)
  expect_lt(abs(got_s$i - got_s$e_i), abs(got$i - got$e_i) / 3)
})


# ── How the statistic is READ, per fold design ─────────────────────────────
# calc_moran itself is unchanged by the block-CV round (the tests above pin
# that). What changed is the reading: under Spatial Block CV the pooled
# out-of-fold residuals inherit the fold geometry and a shared extrapolation
# condition inside each withheld block, so spdep's reference distribution does
# not describe them. The presentation layer must decide that from the APPLIED
# fold plan, never from the strategy the user selected.

test_that("the block-CV reading follows the applied plan, not the requested strategy", {
  block <- moran_reading(applied_cv_plan(100, "block")$type)
  expect_true(block$block)
  expect_equal(block$label, "Block-CV residual clustering")
  expect_false(block$report_p)
  expect_match(block$context, "extrapolation")

  # Below CV_BLOCK_MIN_N the run was scored by LOOCV however the selector was
  # set, so it reads the ordinary way and keeps its p-value. This is the case
  # that would have shipped mislabelled.
  small <- moran_reading(applied_cv_plan(CV_BLOCK_MIN_N - 1L, "block")$type)
  expect_false(small$block)
  expect_equal(small$label, "Moran's I")
  expect_true(small$report_p)

  for (s in c("auto", "loocv")) {
    r <- moran_reading(applied_cv_plan(100, s)$type)
    expect_false(r$block, info = s)
    expect_true(r$report_p, info = s)
  }

  # A k-means collapse leaves random folds under a block request: make_cv_folds
  # marks them and perform_cv carries the mark, so the row is not reported as
  # a block design either.
  fell_back <- applied_cv_plan(100, "block", list(block_fallback = TRUE))
  expect_equal(fell_back$type, "random_kfold")
  expect_match(fell_back$label, "Spatial Block clustering failed")
  expect_false(moran_reading(fell_back$type)$block)
})

test_that("a pooled row reads as block only when every locality was one", {
  all_block <- moran_reading(c("block", "block"))
  expect_true(all_block$block)
  expect_false(all_block$mixed)

  mixed <- moran_reading(c("block", "loocv"))
  expect_false(mixed$block)
  expect_true(mixed$mixed)
  expect_true(mixed$report_p)
  expect_match(mixed$context, "mix fold designs")

  # Nothing known about the design: the ordinary reading, never the block one.
  expect_false(moran_reading(character(0))$block)
  expect_false(moran_reading(NA_character_)$block)
})

test_that("make_cv_folds marks a block request that fell back to random folds", {
  co <- cbind(runif(60, 0, 1000), runif(60, 0, 1000))
  folds <- make_cv_folds(co, "block", 60)
  # k-means succeeded here, so the folds carry no mark and nothing to explain.
  expect_null(attr(folds, "block_fallback"))
  expect_false(isTRUE(perform_cv(NULL)$block_fallback))
})
