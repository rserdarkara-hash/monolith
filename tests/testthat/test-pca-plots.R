# test-pca-plots.R — tests for PCA plotting functions.
# These tests verify that each function returns a ggplot (or plotly) object
# given a valid prcomp result.

# ── Shared fixture ─────────────────────────────────────────────────────────

make_pca <- function(n = 30, seed = 42) {
  df <- make_test_df(n, seed)
  prcomp(df[, c("a", "b", "c", "d", "e")], scale. = TRUE, center = TRUE)
}

# ── generate_pca_scree ────────────────────────────────────────────────────

test_that("generate_pca_scree returns ggplot with correct bar count", {
  pca <- make_pca()
  p <- generate_pca_scree(pca)
  expect_s3_class(p, "ggplot")
})

# ── generate_pca_biplot ───────────────────────────────────────────────────

test_that("generate_pca_biplot returns ggplot", {
  pca <- make_pca()
  df <- make_test_df(30)
  p <- generate_pca_biplot(pca, df, pc_x = 1, pc_y = 2)
  expect_s3_class(p, "ggplot")
})

test_that("generate_pca_biplot with group_col adds colors", {
  pca <- make_pca()
  df <- make_test_df(30)
  p <- generate_pca_biplot(pca, df, pc_x = 1, pc_y = 2, group_col = "cat1")
  expect_s3_class(p, "ggplot")
})

# ── generate_pca_loadings ─────────────────────────────────────────────────

test_that("generate_pca_loadings returns ggplot", {
  pca <- make_pca()
  p <- generate_pca_loadings(pca, pc = 1)
  expect_s3_class(p, "ggplot")
})

# ── generate_pca_contribution ─────────────────────────────────────────────

test_that("generate_pca_contribution returns ggplot with reference line", {
  pca <- make_pca()
  p <- generate_pca_contribution(pca, pc = 1)
  expect_s3_class(p, "ggplot")
})

# ── generate_pca_cos2 ─────────────────────────────────────────────────────

test_that("generate_pca_cos2 returns ggplot", {
  pca <- make_pca()
  p <- generate_pca_cos2(pca, axes = 1:2)
  expect_s3_class(p, "ggplot")
})

# ── generate_pca_cumvar ───────────────────────────────────────────────────

test_that("generate_pca_cumvar returns ggplot approaching 1.0", {
  pca <- make_pca()
  p <- generate_pca_cumvar(pca)
  expect_s3_class(p, "ggplot")
})

# ── generate_pca_mahalanobis ──────────────────────────────────────────────

test_that("generate_pca_mahalanobis returns ggplot", {
  pca <- make_pca()
  p <- generate_pca_mahalanobis(pca)
  expect_s3_class(p, "ggplot")
})

# ── generate_pca_biplot_3d ────────────────────────────────────────────────

test_that("generate_pca_biplot_3d returns plotly object", {
  pca <- make_pca()
  df <- make_test_df(30)
  p <- generate_pca_biplot_3d(pca, df, pc_x = 1, pc_y = 2, pc_z = 3)
  expect_s3_class(p, "plotly")
})

test_that("generate_pca_biplot_3d with group_col adds coloring", {
  pca <- make_pca()
  df <- make_test_df(30)
  p <- generate_pca_biplot_3d(pca, df, pc_x = 1, pc_y = 2, pc_z = 3,
                              group_col = "cat1")
  expect_s3_class(p, "plotly")
})
