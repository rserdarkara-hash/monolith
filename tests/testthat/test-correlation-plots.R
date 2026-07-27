# test-correlation-plots.R — tests for melt_cormat, generate_correlation_heatmap,
# generate_correlation_network, generate_partial_correlation, generate_correlogram,
# generate_lagged_correlation, and check_collinearity.

# ── melt_cormat ────────────────────────────────────────────────────────────

test_that("melt_cormat produces correct melted format", {
  mat <- matrix(c(1.0, 0.5, 0.5, 1.0), nrow = 2,
                dimnames = list(c("A", "B"), c("A", "B")))
  df <- melt_cormat(mat, "Corr")
  expect_s3_class(df, "data.frame")
  expect_equal(nrow(df), 4)
  expect_setequal(colnames(df), c("Var1", "Var2", "Corr"))
})

# ── generate_correlation_heatmap ───────────────────────────────────────────

test_that("generate_correlation_heatmap returns ggplot for valid data", {
  df <- make_test_df(20)
  p <- generate_correlation_heatmap(df, c("a", "b", "c", "d"))
  expect_s3_class(p, "ggplot")
})

test_that("generate_correlation_heatmap returns ggplot for single var", {
  df <- make_test_df(10)
  p <- generate_correlation_heatmap(df, "a")
  expect_s3_class(p, "ggplot")
})

# ── generate_correlation_network ───────────────────────────────────────────

test_that("generate_correlation_network returns ggplot for valid data", {
  df <- make_test_df(20)
  p <- generate_correlation_network(df, c("a", "b", "c", "d"), threshold = 0.1)
  expect_s3_class(p, "ggplot")
})

test_that("generate_correlation_network respects threshold", {
  df <- make_test_df(20)
  p_high <- generate_correlation_network(df, c("a", "b", "c"), threshold = 0.99)
  p_low  <- generate_correlation_network(df, c("a", "b", "c"), threshold = 0.0)
  expect_s3_class(p_high, "ggplot")
  expect_s3_class(p_low, "ggplot")
})

# ── generate_partial_correlation ───────────────────────────────────────────

test_that("generate_partial_correlation returns ggplot for valid data", {
  df <- make_test_df(20)
  p <- generate_partial_correlation(df, c("a", "b", "c"), control_vars = c("d", "e"))
  expect_s3_class(p, "ggplot")
})

test_that("generate_partial_correlation works without control vars", {
  df <- make_test_df(20)
  p <- generate_partial_correlation(df, c("a", "b", "c"))
  expect_s3_class(p, "ggplot")
})

test_that("generate_partial_correlation returns ggplot for single var", {
  df <- make_test_df(10)
  p <- generate_partial_correlation(df, "a")
  expect_s3_class(p, "ggplot")
})

# ── compute_partial_correlation ───────────────────────────────────────────

test_that("pearson partial correlation matches the lm-residual reference", {
  df <- make_test_df(40)
  pc <- compute_partial_correlation(df, c("a", "b", "c"), c("d", "e"), method = "pearson")

  ref_resid <- sapply(c("a", "b", "c"), function(v) {
    residuals(lm(as.formula(paste(v, "~ d + e")), data = df))
  })
  expect_equal(pc$cormat, cor(ref_resid), tolerance = 1e-10)
  expect_equal(pc$n, nrow(df))
  expect_equal(pc$k, 2L)
})

test_that("spearman partial correlation residualizes RANKS (ppcor convention)", {
  df <- make_test_df(40)
  pc <- compute_partial_correlation(df, c("a", "b"), "d", method = "spearman")

  # ppcor::pcor(method = "spearman") inverts the Spearman matrix, which is the
  # Pearson matrix of the ranks — algebraically the same as residualizing ranks.
  ranked <- as.data.frame(lapply(df[, c("a", "b", "d")], rank))
  ref <- sapply(c("a", "b"), function(v) residuals(lm(as.formula(paste(v, "~ d")), data = ranked)))
  expect_equal(unname(pc$cormat[1, 2]), unname(cor(ref)[1, 2]), tolerance = 1e-10)

  # ...and it is NOT the old behaviour (spearman correlation of raw residuals).
  raw_resid <- sapply(c("a", "b"), function(v) residuals(lm(as.formula(paste(v, "~ d")), data = df)))
  expect_false(isTRUE(all.equal(unname(pc$cormat[1, 2]),
                                unname(cor(raw_resid, method = "spearman")[1, 2]),
                                tolerance = 1e-6)))
})

test_that("kendall partial correlation matches the first-order partial tau", {
  df <- make_test_df(40)
  pc <- compute_partial_correlation(df, c("a", "b"), "d", method = "kendall")

  tau <- cor(df[, c("a", "b", "d")], method = "kendall")
  ref <- (tau["a", "b"] - tau["a", "d"] * tau["b", "d"]) /
    sqrt((1 - tau["a", "d"]^2) * (1 - tau["b", "d"]^2))
  expect_equal(unname(pc$cormat[1, 2]), unname(ref), tolerance = 1e-10)
})

test_that("compute_partial_correlation excludes self-controls and survives odd names", {
  df <- make_test_df(30)
  # "c" appears in both sets: controlling for itself would give a NaN row.
  pc <- compute_partial_correlation(df, c("a", "b", "c"), c("c", "d"), method = "pearson")
  expect_equal(pc$k, 1L)
  expect_equal(dim(pc$cormat), c(3L, 3L))
  expect_true(all(is.finite(pc$cormat)))

  names(df)[1:4] <- c("Organic Matter (%)", "pH (1:2.5)", "Clay content", "Slope [deg]")
  pc2 <- compute_partial_correlation(df, c("Organic Matter (%)", "pH (1:2.5)"),
                                     "Slope [deg]", method = "pearson")
  expect_true(all(is.finite(pc2$cormat)))
  expect_equal(colnames(pc2$cormat), c("Organic Matter (%)", "pH (1:2.5)"))
})

test_that("compute_partial_correlation reports missing columns instead of guessing", {
  df <- make_test_df(20)
  pc <- compute_partial_correlation(df, c("a", "nope"), "d")
  expect_null(pc$cormat)
  expect_equal(pc$failed, "nope")
})

test_that("generate_partial_correlation renders with labelled (spaced) names", {
  df <- make_test_df(30)
  names(df)[1:3] <- c("Organic Matter (%)", "pH (1:2.5)", "Clay content")
  p <- generate_partial_correlation(df, c("Organic Matter (%)", "pH (1:2.5)"),
                                    control_vars = "Clay content", method = "spearman")
  expect_s3_class(p, "ggplot")
})

# ── generate_correlogram ──────────────────────────────────────────────────

test_that("generate_correlogram returns ggplot for valid data", {
  df <- make_test_df(20)
  p <- generate_correlogram(df, c("a", "b", "c", "d"))
  expect_s3_class(p, "ggplot")
})

# ── compute/generate_spatial_cross_correlogram ─────────────────────────────

test_that("the spatial cross-correlogram bins by DISTANCE, not by row order", {
  # This is the regression that retired stats::ccf() from this panel: its lag k
  # meant "k rows down the uploaded table", so simply re-sorting the upload
  # changed the published curve. Distance binning cannot depend on row order.
  df <- make_xcorr_df(200)
  res <- compute_spatial_cross_correlogram(df, "a", "b", "x", "y", src_crs = 32633)
  expect_null(res$message)
  expect_true(all(c("dist", "np", "gamma", "rho") %in% names(res$bins)))

  set.seed(99)
  shuffled <- df[sample(nrow(df)), ]
  res2 <- compute_spatial_cross_correlogram(shuffled, "a", "b", "x", "y", src_crs = 32633)
  expect_equal(res2$bins, res$bins, tolerance = 1e-12)
  expect_equal(res2$r0, res$r0, tolerance = 1e-12)
})

test_that("cross-correlation is the standardised cross-covariance r - gamma12(h)", {
  df <- make_xcorr_df(200)
  res <- compute_spatial_cross_correlogram(df, "a", "b", "x", "y", src_crs = 32633)
  expect_equal(res$bins$rho, res$r0 - res$bins$gamma, tolerance = 1e-12)
  # r0 is the ordinary non-spatial correlation of the two variables.
  expect_equal(res$r0, cor(df$a, df$b), tolerance = 1e-8)
  # Co-structured field: near-neighbour pairs are more alike than distant ones.
  expect_gt(res$bins$rho[1], res$bins$rho[nrow(res$bins)])
})

test_that("an unstructured variable gives a flat cross-correlogram near zero", {
  df <- make_xcorr_df(200)
  res <- compute_spatial_cross_correlogram(df, "a", "c", "x", "y", src_crs = 32633)
  expect_null(res$message)
  expect_lt(max(abs(res$bins$rho)), 0.35)
})

test_that("the rank-based cross-correlogram runs on ranks and is flagged", {
  df <- make_xcorr_df(150)
  res <- compute_spatial_cross_correlogram(df, "a", "b", "x", "y", src_crs = 32633,
                                           method = "spearman")
  expect_true(res$ranked)
  expect_equal(res$r0, cor(df$a, df$b, method = "spearman"), tolerance = 1e-8)
})

test_that("the cross-correlogram explains itself instead of blanking", {
  df <- make_xcorr_df(60)
  expect_match(compute_spatial_cross_correlogram(df, "a", "b", NULL, NULL, NULL)$message,
               "Coordinates are not mapped")
  expect_match(compute_spatial_cross_correlogram(df, "a", "a", "x", "y", 32633)$message,
               "two different variables")
  expect_match(compute_spatial_cross_correlogram(df[1:5, ], "a", "b", "x", "y", 32633)$message,
               "Insufficient data")
  df$a <- 1
  expect_match(compute_spatial_cross_correlogram(df, "a", "b", "x", "y", 32633)$message,
               "constant")
})

test_that("generate_spatial_cross_correlogram returns a ggplot in both states", {
  df <- make_xcorr_df(120)
  expect_s3_class(generate_spatial_cross_correlogram(df, "a", "b", "x", "y", 32633), "ggplot")
  expect_s3_class(generate_spatial_cross_correlogram(df, "a", "b", NULL, NULL, NULL), "ggplot")
})

# ── check_collinearity ────────────────────────────────────────────────────

test_that("check_collinearity detects high correlations", {
  df <- make_collinear_df(20)
  result <- check_collinearity(df, c("v1", "v2", "v3", "v4"))
  expect_type(result, "list")
  expect_true("has_collinearity" %in% names(result))
  expect_true("pairs" %in% names(result))
  # v1 and v2 are highly collinear
  expect_true(result$has_collinearity)
})

test_that("check_collinearity returns no collinearity for independent vars", {
  df <- make_test_df(20)
  result <- check_collinearity(df, c("a", "b", "c"))
  expect_false(result$has_collinearity)
})
