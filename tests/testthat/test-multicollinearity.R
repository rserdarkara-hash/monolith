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
  # Both ends, or the parameter is only half asserted: at 0.0 every pair
  # clears the bar and is reported, at 1.0 none does.
  res_low <- detect_multicollinearity_engine(df, vars = c("a", "b", "c"),
                                             pairwise_threshold = 0.0)
  expect_true(res_low$has_collinearity)
  expect_gt(nrow(res_low$pairs), 0)
  expect_equal(nrow(res_low$pairs), choose(3, 2))

  res_high <- detect_multicollinearity_engine(df, vars = c("a", "b", "c"),
                                              pairwise_threshold = 1.0)
  expect_false(res_high$has_collinearity)
  expect_null(res_high$pairs)
})

# ── check_vif ──────────────────────────────────────────────────────────────

test_that("check_vif forwards the engine's four fields unchanged", {
  # check_vif is a four-line forwarder over detect_multicollinearity_engine.
  # Its whole contract is that the four fields arrive intact, so assert
  # equality with the engine rather than re-testing the gate through it.
  for (thr in c(5, 10, Inf)) {
    df <- make_collinear_df(30)
    got <- check_vif(df, threshold = thr)
    ref <- detect_multicollinearity_engine(df, vif_threshold = thr)
    expect_named(got, c("kept", "dropped", "dropped_constant", "dropped_vif"))
    for (f in names(got)) expect_identical(got[[f]], ref[[f]], info = paste(thr, f))
  }

  # And the fields carry something at the default threshold, so an accidental
  # all-empty forward cannot pass the equality above by agreeing on nothing.
  res <- check_vif(make_collinear_df(30), threshold = 10)
  expect_gte(length(res$dropped_vif), 1L)
  expect_true(all(c("v3", "v4") %in% res$kept))
})

# ── Numeric contract: the VIF itself ───────────────────────────────────────
#
# Everything above pins which column the gate drops. These pin the arithmetic
# behind that decision: the engine computes VIF as diag(solve(cor(X))), and the
# reference recomputes it the long way, as 1/(1 - R2_j) from an actual
# regression of each covariate on the others.

test_that("the VIF gate drops exactly what an lm-based VIF says it should", {
  cov <- golden_meta()$columns$covariates
  X <- golden_soil("full")[cov]

  # Independent reference: the same iterative rule (drop the worst, refit,
  # repeat) with every VIF obtained from a regression rather than from a
  # matrix inverse.
  ref_kept <- cov
  ref_dropped <- character(0)
  repeat {
    if (length(ref_kept) < 2) break
    vif <- vapply(ref_kept, function(v) {
      r2 <- summary(lm(reformulate(setdiff(ref_kept, v), v), data = X))$r.squared
      1 / (1 - r2)
    }, numeric(1))
    if (max(vif) <= 10) break
    ref_dropped <- c(ref_dropped, ref_kept[which.max(vif)])
    ref_kept <- setdiff(ref_kept, ref_dropped[length(ref_dropped)])
  }

  res <- detect_multicollinearity_engine(X, vif_threshold = 10)
  # Same covariates, dropped in the same order: the engine's VIF ranking is the
  # regression VIF ranking at every step, not merely at the first one.
  expect_equal(res$dropped_vif, ref_dropped)
  expect_setequal(res$kept, ref_kept)
  # Recorded for this golden set, so a change of gate rule is visible even if
  # someone weakens the reference above. On the shipped survey the two
  # temperature variables and one of the slope/ruggedness pair go.
  recorded <- golden_baseline("vif_drop_order")
  if (!is.null(recorded)) {
    expect_equal(ref_dropped, recorded, info = golden_baseline_info())
  }
})

test_that("an orthogonal design has VIF exactly 1 and survives the gate", {
  # A full 2^3 factorial in +-1 has exactly zero sample correlation, so its
  # correlation matrix is the identity and every VIF is exactly 1 - not 1.0008,
  # which is what a random "uncorrelated" design actually gives.
  O <- data.frame(
    a = c(-1, -1, -1, -1, 1, 1, 1, 1),
    b = c(-1, -1,  1,  1, -1, -1, 1, 1),
    c = c(-1,  1, -1,  1, -1,  1, -1, 1)
  )
  expect_equal(unname(diag(solve(cor(O)))), c(1, 1, 1), tolerance = 1e-12)

  res <- detect_multicollinearity_engine(O, vif_threshold = 10)
  expect_length(res$dropped, 0)
  expect_false(res$has_collinearity)
  expect_setequal(res$kept, c("a", "b", "c"))
})

test_that("the VIF gate is invariant to rescaling and to column order", {
  cov <- golden_meta()$columns$covariates
  X <- golden_soil("full")[cov]
  base <- detect_multicollinearity_engine(X, vif_threshold = 10)

  # VIF is a correlation-matrix quantity, so an affine change of units cannot
  # move it.
  Xs <- X
  Xs$v82 <- Xs$v82 * 1000 + 5e5     # metres -> millimetres, plus an offset
  Xs$v12 <- Xs$v12 / 25.4
  expect_setequal(detect_multicollinearity_engine(Xs, vif_threshold = 10)$kept,
                  base$kept)

  # And the outcome must be a property of the data, not of the file layout.
  expect_setequal(detect_multicollinearity_engine(X[rev(cov)],
                                                  vif_threshold = 10)$kept,
                  base$kept)
})

test_that("an infinite threshold keeps every covariate however collinear", {
  # The two most collinear pairs this golden set carries.
  all_cov <- golden_meta()$columns$covariates
  cm <- cor(golden_soil("full")[all_cov]); diag(cm) <- 0
  hits <- which(abs(cm) > 0.99, arr.ind = TRUE)
  cov <- unique(all_cov[as.vector(hits)])
  X <- golden_soil("full")[cov]
  expect_gte(length(cov), 2L)
  res <- detect_multicollinearity_engine(X, vif_threshold = Inf)
  expect_length(res$dropped, 0)
  expect_setequal(res$kept, cov)
  # The pairwise report still fires: "keep all" is a modelling choice, not a
  # reason to stop telling the user the covariates are collinear.
  expect_true(res$has_collinearity)
})
