# test-pca-plots.R — tests for the PCA plotting functions.

# ── Shared fixture ─────────────────────────────────────────────────────────

make_pca <- function(n = 30, seed = 42) {
  df <- make_test_df(n, seed)
  prcomp(df[, c("a", "b", "c", "d", "e")], scale. = TRUE, center = TRUE)
}

# ── The builder sweep ──────────────────────────────────────────────────────
#
# expect_s3_class(p, "ggplot") does not build the plot, so an aesthetic naming
# a column that is not there survives it. Each builder is swept through
# ggplot_build() here, with the one quantity its panel exists to show.

test_that("every PCA builder renders, and shows the quantity it is named for", {
  pca <- make_pca()
  df <- make_test_df(30)
  k <- length(pca$sdev)
  var_exp <- unname(pca$sdev^2 / sum(pca$sdev^2))

  for (p in list(generate_pca_scree(pca),
                 generate_pca_biplot(pca, df, pc_x = 1, pc_y = 2),
                 generate_pca_biplot(pca, df, pc_x = 1, pc_y = 2, group_col = "cat1"),
                 generate_pca_loadings(pca, pc = 1),
                 generate_pca_contribution(pca, pc = 1),
                 generate_pca_cos2(pca, axes = 1:2),
                 generate_pca_cumvar(pca),
                 generate_pca_mahalanobis(pca))) {
    expect_s3_class(p, "ggplot")
    expect_no_error(ggplot2::ggplot_build(p))
  }

  # One bar per component, carrying that component's variance share.
  scree <- generate_pca_scree(pca)$data
  expect_equal(nrow(scree), k)
  expect_equal(scree$Variance, var_exp)

  # Contribution is loading^2 as a percentage, with the dashed reference line
  # at the contribution every variable would have if all contributed equally.
  contrib <- generate_pca_contribution(pca, pc = 1)
  vars <- rownames(pca$rotation)
  expect_equal(setNames(contrib$data$Value, contrib$data$Variable)[vars],
               setNames(pca$rotation[, 1]^2 * 100, vars))
  href <- Filter(function(l) inherits(l$geom, "GeomHline"), contrib$layers)
  expect_length(href, 1L)
  expect_equal(href[[1]]$data$yintercept, 100 / k)

  # Cumulative variance ends at exactly 1, with the 80 % threshold drawn.
  cum <- generate_pca_cumvar(pca)
  expect_equal(cum$data$CumVar, cumsum(var_exp))
  expect_equal(cum$data$CumVar[k], 1)
  expect_equal(Filter(function(l) inherits(l$geom, "GeomHline"),
                      cum$layers)[[1]]$data$yintercept, 0.8)

  # Loadings are the rotation column itself, ordered by absolute weight.
  load <- generate_pca_loadings(pca, pc = 1)$data
  expect_equal(setNames(load$Value, load$Variable)[vars],
               setNames(pca$rotation[, 1], vars))

  # The 3D biplot is plotly, not ggplot, with and without a grouping column.
  expect_s3_class(generate_pca_biplot_3d(pca, df, pc_x = 1, pc_y = 2, pc_z = 3), "plotly")
  expect_s3_class(generate_pca_biplot_3d(pca, df, pc_x = 1, pc_y = 2, pc_z = 3,
                                         group_col = "cat1"), "plotly")
})

# ── generate_pca_biplot ───────────────────────────────────────────────────

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

# ── generate_pca_cos2 ─────────────────────────────────────────────────────

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

# ── generate_pca_mahalanobis ──────────────────────────────────────────────

test_that("generate_pca_mahalanobis handles exactly collinear variables", {
  df <- make_collinear_df()
  df$v2 <- df$v1  # exact duplicate -> zero-variance PC -> singular cov(scores)
  pca <- prcomp(df, scale. = TRUE)
  p <- generate_pca_mahalanobis(pca)
  expect_s3_class(p, "ggplot")
})

# ── generate_pca_biplot_3d ────────────────────────────────────────────────

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
test_that("PCA views reject unavailable and stale component selections", {
  d <- data.frame(a = 1:20, b = sin(1:20), constant = 1)
  fit <- desc_pca_fit(d, names(d))
  p <- generate_pca_biplot_3d(fit$res, d)
  expect_s3_class(p, "ggplot")
  expect_match(p$layers[[1]]$aes_params$label, "three")
  for (axis in c(NA_real_, 0, 3, 1.5)) {
    p <- generate_pca_biplot(fit$res, d, pc_x = axis)
    expect_match(p$layers[[1]]$aes_params$label, "component")
    p <- generate_pca_loadings(fit$res, pc = axis)
    expect_match(p$layers[[1]]$aes_params$label, "component")
  }
  full <- prcomp(data.frame(a = 1:20, b = sin(1:20), c = cos(1:20)), scale. = TRUE)
  p <- plotly::plotly_build(generate_pca_biplot_3d(full, d))
  title <- p$x$layout$title
  expect_match(if (is.list(title)) title$text else title, "3D PCA Scores")
})

test_that("a new PCA with fewer components keeps supported views available", {
  d <- data.frame(a = 1:20, b = sin(1:20), c = cos(1:20), constant = 1)
  shiny::testServer(desc_exploratory_server, args = list(
    data_reactive = shiny::reactive(d), vars_metadata_reactive = shiny::reactive(NULL)
  ), {
    session$setInputs(pca_vars = c("a", "b", "c"), pca_scale = TRUE, run_pca_btn = 1,
                      pca_plot_type = "3d_biplot", pca_pc_x = 1, pca_pc_y = 2, pca_pc_z = 3)
    expect_s3_class(pca_plot_obj(), "plotly")
    session$setInputs(pca_vars = c("a", "b", "constant"), run_pca_btn = 2)
    expect_equal(ncol(pca_rv$res$x), 2L)
    expect_match(pca_plot_obj()$layers[[1]]$aes_params$label, "three")
    session$setInputs(pca_expand_plot_btn = 1)
    expect_match(output$pca_expanded_ui$html, 'pca_main_plot_expanded"', fixed = TRUE)
    session$setInputs(pca_plot_type = "loadings", pca_pc_single = 3)
    expect_match(pca_plot_obj()$layers[[1]]$aes_params$label, "component")
    session$setInputs(pca_pc_single = 2)
    expect_equal(nrow(pca_plot_obj()$data), 2L)
  })
})
