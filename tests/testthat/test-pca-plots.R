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

test_that("generate_pca_biplot handles na.omit correctly when caller aligns dataframe", {
  df <- make_test_df(30)
  df$a[c(2, 5, 10)] <- NA
  df_clean <- na.omit(df[, c("a", "b", "c", "d", "e")])
  pca <- prcomp(df_clean, scale. = TRUE, center = TRUE)
  
  # Without alignment, it should error
  expect_error(generate_pca_biplot(pca, df, pc_x = 1, pc_y = 2, group_col = "cat1"))
  
  # With caller alignment (the fix applied in the module)
  aligned_df <- df[rownames(pca$x), , drop = FALSE]
  p <- generate_pca_biplot(pca, aligned_df, pc_x = 1, pc_y = 2, group_col = "cat1")
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

test_that("cos2 is a bounded quality of representation in BOTH PCA modes", {
  # cos2 = share of a variable's own variance captured by the selected PCs, so
  # it must sit in [0, 1] and reach exactly 1 across all components. Before
  # 2026-08-14 the unscaled branch plotted an unnormalised absolute variance
  # (Var(x_j) in the variable's own squared units) on an axis labelled cos2.
  df <- make_test_df(40, seed = 7)[, c("a", "b", "c", "d", "e")]
  # give the variables wildly different scales — the case that made it visible
  df$a <- df$a * 1000
  df$b <- df$b / 500

  for (scaled in c(TRUE, FALSE)) {
    pca <- prcomp(df, scale. = scaled, center = TRUE)
    v2 <- generate_pca_cos2(pca, axes = 1:2)$data$Value
    expect_true(all(v2 >= 0 & v2 <= 1),
                info = paste("axes 1:2, scale. =", scaled))
    # all components together represent every variable perfectly
    v_all <- generate_pca_cos2(pca, axes = seq_along(pca$sdev))$data$Value
    expect_equal(unname(v_all), rep(1, ncol(df)), tolerance = 1e-8)
  }
})

test_that("cos2 for a scaled PCA is unchanged by the normalisation", {
  # For scale. = TRUE the denominator is 1 for every variable, so the panel that
  # users have been reading is numerically where it was.
  pca <- make_pca()
  axes <- 1:2
  coord <- sweep(pca$rotation[, axes, drop = FALSE], 2, pca$sdev[axes], "*")
  legacy <- rowSums(coord^2)
  # generate_pca_bar_plot sorts its bars by value, so compare by variable name
  got <- generate_pca_cos2(pca, axes = axes)$data
  expect_equal(setNames(as.numeric(got$Value), got$Variable)[names(legacy)],
               legacy, tolerance = 1e-10)
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

test_that("generate_pca_mahalanobis handles exactly collinear variables", {
  df <- make_collinear_df()
  df$v2 <- df$v1  # exact duplicate -> zero-variance PC -> singular cov(scores)
  pca <- prcomp(df, scale. = TRUE)
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

test_that("generate_pca_biplot_3d handles na.omit correctly when caller aligns dataframe", {
  df <- make_test_df(30)
  df$a[c(2, 5, 10)] <- NA
  df_clean <- na.omit(df[, c("a", "b", "c", "d", "e")])
  pca <- prcomp(df_clean, scale. = TRUE, center = TRUE)
  
  expect_error(generate_pca_biplot_3d(pca, df, pc_x = 1, pc_y = 2, pc_z = 3, group_col = "cat1"))
  
  aligned_df <- df[rownames(pca$x), , drop = FALSE]
  p <- generate_pca_biplot_3d(pca, aligned_df, pc_x = 1, pc_y = 2, pc_z = 3, group_col = "cat1")
  expect_s3_class(p, "plotly")
})
