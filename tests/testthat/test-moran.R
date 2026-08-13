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

test_that("calc_moran handles duplicated coordinates by jittering", {
  set.seed(42)
  n <- 5
  # All points at the same location → duplicates
  coords <- matrix(rep(c(450000, 5800000), each = n), ncol = 2)
  residuals <- rnorm(n)
  moran_val <- calc_moran(residuals, coords)$i
  # May return NA or a value — must not throw an uncaught error
  expect_true(is.na(moran_val) || is.numeric(moran_val))
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

test_that("calc_moran handles collinear coordinates gracefully", {
  # Collinear coordinates (points on a line) may cause issues with
  # neighbour-search or distance-weighting.  Verify the function doesn't
  # throw an uncaught error.
  n <- 6
  coords <- cbind(seq_len(n), seq_len(n))  # collinear
  residuals <- 1:n
  m <- suppressWarnings(calc_moran(residuals, coords))
  expect_true(is.na(m$i) || is.numeric(m$i))
  # Whichever path served it, E[I] is available analytically.
  expect_true(is.na(m$i) || isTRUE(all.equal(m$e_i, -1 / (n - 1))))
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

test_that("calc_moran stays reproducible for duplicate coordinates", {
  set.seed(11)
  coords <- cbind(runif(40, 450000, 451000), runif(40, 4500000, 4501000))
  coords[1:5, ] <- coords[1, ]
  resid <- rnorm(40)
  expect_equal(suppressWarnings(calc_moran(resid, coords)),
               suppressWarnings(calc_moran(resid, coords)))
})
