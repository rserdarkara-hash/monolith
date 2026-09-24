# test-correlation-plots.R — tests for melt_cormat, generate_correlation_heatmap,
# generate_correlation_network, generate_partial_correlation, generate_correlogram,
# generate_lagged_correlation, and check_collinearity.

test_that("auxiliary ranks use the mapped target and SS partition", {
  df <- data.frame(
    loc = rep(c("A", "B"), each = 6),
    SubSet = rep(c("Train", "Train", "Train", "Test", "Test", "Test"), 2),
    x = 1:12,
    y = 12:1,
    actual = 1:12,
    custom_cve = c(3, 1, 2, 6, 4, 5, 9, 7, 8, 12, 10, 11),
    custom_ss = c(3, 2, 1, 4, 5, 6, 9, 8, 7, 10, 11, 12),
    aux = 1:12
  )
  mapping <- list(
    loc = "loc",
    x = "x",
    y = "y",
    vars = list(list(
      actual = "actual",
      pred = "custom_cve",
      pred_ss = "custom_ss"
    ))
  )
  actual <- rank_auxiliary_correlations(
    df,
    mapping,
    "actual",
    "actual",
    "predictions",
    "A",
    "Test"
  )
  cve <- rank_auxiliary_correlations(
    df,
    mapping,
    "actual",
    "pred",
    "predictions",
    "A",
    "Test"
  )
  ss <- rank_auxiliary_correlations(
    df,
    mapping,
    "actual",
    "pred_ss",
    "predictions",
    "A",
    "Train"
  )
  ss_actual <- rank_auxiliary_correlations(
    df,
    mapping,
    "actual",
    "pred_ss",
    "actual",
    "A",
    "Train"
  )
  aux_corr <- function(res) res$results$Corr[res$results$Variable == "aux"]
  expect_equal(aux_corr(actual), 1)
  expect_identical(actual$target, "actual")
  expect_identical(cve$target, "custom_cve")
  expect_equal(cve$n, 6L) # CVE ignores the hidden SS subset control.
  expect_equal(aux_corr(cve), 23 / 35) # Centred product sum 11.5 / square sum 17.5.
  expect_equal(
    cve$results$Pval[cve$results$Variable == "aux"],
    2 * pt(-abs((23 / 35) * sqrt(4 / (1 - (23 / 35)^2))), 4)
  )
  expect_identical(ss$target, "custom_ss")
  expect_equal(ss$n, 3L)
  expect_equal(aux_corr(ss), -1)
  expect_equal(aux_corr(ss_actual), 1)
  expect_identical(ss_actual$subset, "Train")
  expect_setequal(cve$results$Variable, c("custom_ss", "aux"))
  expect_setequal(actual$results$Variable, c("custom_cve", "custom_ss", "aux"))
  expect_equal(
    rank_auxiliary_correlations(
      df,
      mapping,
      "actual",
      "pred_ss",
      "predictions",
      "ALL"
    )$n,
    12L
  )
  mapping$vars[[1]]$pred_ss <- NA_character_
  expect_error(
    rank_auxiliary_correlations(df, mapping, "actual", "pred_ss"),
    "prediction column"
  )
})

test_that("auxiliary ranks count finite pairs and explain unrankable candidates", {
  df <- data.frame(
    actual = c(1:5, NA, Inf),
    good = c(1, 3, 2, 4, NA, 6, 7),
    constant = 1,
    sparse = c(1, 2, rep(NA, 5))
  )
  mapping <- list(vars = list(list(actual = "actual")))
  res <- rank_auxiliary_correlations(df, mapping, "actual")
  expect_identical(res$results$Variable, "good")
  expect_equal(res$results$N, 4L)
  expect_equal(res$results$Corr, 0.8) # Centred product sum 4 / square sum 5.
  expect_setequal(res$skipped, c("constant", "sparse"))
  empty <- rank_auxiliary_correlations(df[1:2, ], mapping, "actual")
  expect_equal(nrow(empty$results), 0L)
})

test_that("golden auxiliary ranks agree with centred Pearson and Student t definitions", {
  df <- golden_soil("core")
  cols <- golden_meta()$columns
  cve <- grep("_cve$", cols$pred, value = TRUE)[1]
  target <- sub("_cve$", "", cve)
  ss <- sub("_cve$", "_ss", cve)
  loc <- unique(df$locality)[1]
  partition <- unique(df$subset)[1]
  mapping <- list(
    loc = "locality",
    x = "x",
    y = "y",
    vars = list(list(
      actual = target,
      pred = cve,
      pred_ss = ss
    ))
  )
  for (mode in c("actual", "pred", "pred_ss")) {
    res <- rank_auxiliary_correlations(
      df,
      mapping,
      target,
      mode,
      "predictions",
      loc,
      partition
    )
    ref <- df[df$locality == loc, , drop = FALSE]
    if (mode == "pred_ss") {
      ref <- ref[ref$subset == partition, , drop = FALSE]
    }
    response <- switch(mode, actual = target, pred = cve, pred_ss = ss)
    x <- ref[[response]]
    y <- ref[[cols$covariate_main]]
    ok <- is.finite(x) & is.finite(y)
    x <- x[ok] - mean(x[ok])
    y <- y[ok] - mean(y[ok])
    r <- sum(x * y) / sqrt(sum(x^2) * sum(y^2))
    n <- sum(ok)
    row <- res$results[res$results$Variable == cols$covariate_main, ]
    expect_equal(row$Corr, r, tolerance = 1e-12)
    expect_equal(
      row$Pval,
      2 * pt(-abs(r * sqrt((n - 2) / (1 - r^2))), n - 2),
      tolerance = 1e-12
    )
    expect_equal(row$N, n)
  }
})

# ── melt_cormat ────────────────────────────────────────────────────────────

test_that("melt_cormat produces correct melted format", {
  mat <- matrix(
    c(1.0, 0.5, 0.5, 1.0),
    nrow = 2,
    dimnames = list(c("A", "B"), c("A", "B"))
  )
  df <- melt_cormat(mat, "Corr")
  expect_s3_class(df, "data.frame")
  expect_equal(nrow(df), 4)
  expect_setequal(colnames(df), c("Var1", "Var2", "Corr"))
})

# ── generate_correlation_heatmap ───────────────────────────────────────────

# ── The four correlation panels ────────────────────────────────────────────
#
# expect_s3_class(p, "ggplot") does not build the plot, and every one of these
# builders returns a ggplot for any input including the degenerate ones - the
# "Need >=2 variables" notice is a ggplot too. Build them, and assert the
# quantity each panel exists to show.

test_that("every correlation panel renders and carries its own correlations", {
  df <- make_test_df(20)
  vars <- c("a", "b", "c", "d")
  cm <- cor(df[, vars])

  for (p in list(generate_correlation_heatmap(df, vars),
                 generate_correlation_network(df, vars, threshold = 0.1),
                 generate_partial_correlation(df, c("a", "b", "c"),
                                              control_vars = c("d", "e")),
                 generate_correlogram(df, vars))) {
    expect_s3_class(p, "ggplot")
    expect_no_error(ggplot2::ggplot_build(p))
  }

  # Heatmap and correlogram plot cor() itself, one cell per ordered pair.
  for (panel in list(generate_correlation_heatmap(df, vars),
                     generate_correlogram(df, vars))) {
    cells <- panel$data
    expect_equal(nrow(cells), length(vars)^2)
    key <- paste(cells$Var1, cells$Var2)
    expect_equal(unname(cells$Corr[match(paste("a", "b"), key)]), cm["a", "b"])
    expect_equal(sort(unique(as.character(cells$Var1))), sort(vars))
  }

  # Labelled names with spaces and parentheses must survive the partial
  # correlation route, which builds model formulas from the column names.
  named <- df
  names(named)[1:3] <- c("Organic Matter (%)", "pH (1:2.5)", "Clay content")
  expect_no_error(ggplot2::ggplot_build(generate_partial_correlation(
    named, c("Organic Matter (%)", "pH (1:2.5)"),
    control_vars = "Clay content", method = "spearman")))
})

test_that("the network threshold is what decides the edges", {
  # The edge count is the quantity the threshold controls; building a plot at
  # 0.99 and another at 0.0 and checking both are ggplots says nothing at all.
  df <- make_test_df(20)
  vars <- c("a", "b", "c")
  cm <- cor(df[, vars])
  off_diag <- abs(cm[upper.tri(cm)])

  n_edges <- function(thr) {
    segs <- Filter(function(l) inherits(l$geom, "GeomSegment"),
                   generate_correlation_network(df, vars, threshold = thr)$layers)
    if (length(segs) == 0) 0L else nrow(segs[[1]]$data)
  }

  # Above every pairwise correlation: no edges at all, and the node layer is
  # still drawn so the panel is not blank.
  expect_equal(n_edges(0.99), 0L)
  expect_gt(length(generate_correlation_network(df, vars, threshold = 0.99)$layers), 0L)
  # At zero, every pair is an edge.
  expect_equal(n_edges(0), length(off_diag))
  # And in between, exactly the pairs at or above the cut.
  mid <- sort(off_diag)[2]
  expect_equal(n_edges(mid), sum(off_diag >= mid))
})

test_that("a selection too small for a correlation says so", {
  df <- make_test_df(10)
  label_of <- function(p) ggplot2::ggplot_build(p)$data[[1]]$label
  for (p in list(generate_correlation_heatmap(df, "a"),
                 generate_correlation_network(df, "a"),
                 generate_correlogram(df, "a"))) {
    expect_equal(label_of(p), "Need >=2 variables")
  }
  # The partial panel says the same thing in its own words.
  expect_equal(label_of(generate_partial_correlation(df, "a")),
               "Need >=2 variables to correlate")
  # Fewer than three complete rows cannot support a correlation either.
  expect_equal(label_of(generate_correlation_heatmap(df[1:2, ], c("a", "b"))),
               "Insufficient data")
})

test_that("a supplied correlation matrix is aligned to the plotted variables", {
  # Both panels index the matrix POSITIONALLY (cormat[i, j] against vars[i], and
  # vars[hc$order]), which was only correct because the caller happened to build
  # it over the same variables in the same order. A superset or a re-ordered
  # matrix would have mislabelled every cell.
  df <- make_test_df(20)
  vars <- c("c", "a")
  full <- cor(df[, c("a", "b", "c", "d")])
  aligned <- align_cormat(full, vars)
  expect_identical(rownames(aligned), vars)
  expect_identical(colnames(aligned), vars)
  expect_equal(aligned["c", "a"], full["c", "a"])
  # A matrix without usable dimnames is left alone (positional is all there is).
  bare <- unname(full)
  expect_identical(align_cormat(bare, vars), bare)

  expect_s3_class(
    generate_correlation_heatmap(df, vars, cormat = full),
    "ggplot"
  )
  expect_s3_class(
    generate_correlation_network(df, vars, threshold = 0.1, cormat = full),
    "ggplot"
  )
})

test_that("a constant variable is named instead of blanking the panel", {
  # cor() returns an NA row/column for a zero-variance column. as.dist() then
  # feeds those NAs to hclust ("NA/NaN/Inf in foreign function call") and the
  # network's `if (abs(w) >= threshold)` gets a missing value — both abort the
  # render, so the panel vanishes with no message.
  df <- make_test_df(20)
  df$flat <- 1
  vars <- c("a", "b", "flat")

  # cor() itself warns ("the standard deviation is zero") before returning the
  # NA row; that is stats' business, the point here is what the panel does with it.
  p_heat <- suppressWarnings(generate_correlation_heatmap(df, vars))
  p_net <- suppressWarnings(generate_correlation_network(
    df,
    vars,
    threshold = 0.1
  ))
  expect_s3_class(p_heat, "ggplot")
  expect_s3_class(p_net, "ggplot")
  expect_match(
    as.character(p_heat$layers[[1]]$aes_params$label %||% ""),
    "constant"
  )
  expect_match(
    as.character(p_net$layers[[1]]$aes_params$label %||% ""),
    "constant"
  )
})

# ── generate_partial_correlation ───────────────────────────────────────────

test_that("generate_partial_correlation works without control vars", {
  df <- make_test_df(20)
  p <- generate_partial_correlation(df, c("a", "b", "c"))
  expect_s3_class(p, "ggplot")
  # With nothing partialled out the figure is an ordinary correlation heatmap,
  # and its legend must say so as its title does.
  fill_name <- function(g) g$scales$get_scales("fill")$name
  expect_equal(p$labels$title, "Standard Correlation Heatmap")
  expect_equal(fill_name(p), "Correlation")
  p_ctrl <- generate_partial_correlation(df, c("a", "b", "c"), control_vars = "d")
  expect_equal(fill_name(p_ctrl), "Partial\nCorrelation")
})

# ── compute_partial_correlation ───────────────────────────────────────────

test_that("pearson partial correlation matches the lm-residual reference", {
  df <- make_test_df(40)
  pc <- compute_partial_correlation(
    df,
    c("a", "b", "c"),
    c("d", "e"),
    method = "pearson"
  )

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
  ref <- sapply(c("a", "b"), function(v) {
    residuals(lm(as.formula(paste(v, "~ d")), data = ranked))
  })
  expect_equal(
    unname(pc$cormat[1, 2]),
    unname(cor(ref)[1, 2]),
    tolerance = 1e-10
  )

  # ...and it is NOT the old behaviour (spearman correlation of raw residuals).
  raw_resid <- sapply(c("a", "b"), function(v) {
    residuals(lm(as.formula(paste(v, "~ d")), data = df))
  })
  expect_false(isTRUE(all.equal(
    unname(pc$cormat[1, 2]),
    unname(cor(raw_resid, method = "spearman")[1, 2]),
    tolerance = 1e-6
  )))
})

test_that("kendall partial correlation matches the first-order partial tau", {
  df <- make_test_df(40)
  pc <- compute_partial_correlation(df, c("a", "b"), "d", method = "kendall")

  tau <- cor(df[, c("a", "b", "d")], method = "kendall")
  ref <- (tau["a", "b"] - tau["a", "d"] * tau["b", "d"]) /
    sqrt((1 - tau["a", "d"]^2) * (1 - tau["b", "d"]^2))
  expect_equal(unname(pc$cormat[1, 2]), unname(ref), tolerance = 1e-10)

  # Adding a third target must not condition a-b on it: every pair is
  # partialled on the explicit controls only, like pearson/spearman. c is
  # built to depend on a and b, so conditioning on it would move a-b.
  df$c <- df$a + 0.5 * df$b + df$e
  pc3 <- compute_partial_correlation(df, c("a", "b", "c"), "d", method = "kendall")
  inv_all <- solve(cor(df[, c("a", "b", "c", "d")], method = "kendall"))
  whole_matrix <- -inv_all["a", "b"] / sqrt(inv_all["a", "a"] * inv_all["b", "b"])
  expect_gt(abs(whole_matrix - ref), 0.05)
  expect_equal(pc3$k, 1L)
  expect_equal(unname(pc3$cormat["a", "b"]), unname(ref), tolerance = 1e-10)
  tau_c <- cor(df[, c("a", "c", "d")], method = "kendall")
  ref_ac <- (tau_c["a", "c"] - tau_c["a", "d"] * tau_c["c", "d"]) /
    sqrt((1 - tau_c["a", "d"]^2) * (1 - tau_c["c", "d"]^2))
  expect_equal(unname(pc3$cormat["a", "c"]), unname(ref_ac), tolerance = 1e-10)
  expect_equal(pc3$cormat, t(pc3$cormat))
})

test_that("compute_partial_correlation excludes self-controls and survives odd names", {
  df <- make_test_df(30)
  # "c" appears in both sets: controlling for itself would give a NaN row.
  pc <- compute_partial_correlation(
    df,
    c("a", "b", "c"),
    c("c", "d"),
    method = "pearson"
  )
  expect_equal(pc$k, 1L)
  expect_equal(dim(pc$cormat), c(3L, 3L))
  expect_true(all(is.finite(pc$cormat)))

  names(df)[1:4] <- c(
    "Organic Matter (%)",
    "pH (1:2.5)",
    "Clay content",
    "Slope [deg]"
  )
  pc2 <- compute_partial_correlation(
    df,
    c("Organic Matter (%)", "pH (1:2.5)"),
    "Slope [deg]",
    method = "pearson"
  )
  expect_true(all(is.finite(pc2$cormat)))
  expect_equal(colnames(pc2$cormat), c("Organic Matter (%)", "pH (1:2.5)"))
})

test_that("compute_partial_correlation reports missing columns instead of guessing", {
  df <- make_test_df(20)
  pc <- compute_partial_correlation(df, c("a", "nope"), "d")
  expect_null(pc$cormat)
  expect_equal(pc$failed, "nope")
})

# ── compute/generate_spatial_cross_correlogram ─────────────────────────────

test_that("the spatial cross-correlogram bins by DISTANCE, not by row order", {
  # This is the regression that retired stats::ccf() from this panel: its lag k
  # meant "k rows down the uploaded table", so simply re-sorting the upload
  # changed the published curve. Distance binning cannot depend on row order.
  df <- make_xcorr_df(200)
  res <- compute_spatial_cross_correlogram(
    df,
    "a",
    "b",
    "x",
    "y",
    src_crs = 32633
  )
  expect_null(res$message)
  expect_true(all(c("dist", "np", "gamma", "rho") %in% names(res$bins)))

  set.seed(99)
  shuffled <- df[sample(nrow(df)), ]
  res2 <- compute_spatial_cross_correlogram(
    shuffled,
    "a",
    "b",
    "x",
    "y",
    src_crs = 32633
  )
  expect_equal(res2$bins, res$bins, tolerance = 1e-12)
  expect_equal(res2$r0, res$r0, tolerance = 1e-12)
})

test_that("cross-correlation is the standardised cross-covariance r - gamma12(h)", {
  df <- make_xcorr_df(200)
  res <- compute_spatial_cross_correlogram(
    df,
    "a",
    "b",
    "x",
    "y",
    src_crs = 32633
  )
  expect_equal(res$bins$rho, res$r0 - res$bins$gamma, tolerance = 1e-12)
  # r0 is the ordinary non-spatial correlation of the two variables.
  expect_equal(res$r0, cor(df$a, df$b), tolerance = 1e-8)
  # Co-structured field: near-neighbour pairs are more alike than distant ones.
  expect_gt(res$bins$rho[1], res$bins$rho[nrow(res$bins)])
})

test_that("an unstructured variable gives a flat cross-correlogram near zero", {
  df <- make_xcorr_df(200)
  res <- compute_spatial_cross_correlogram(
    df,
    "a",
    "c",
    "x",
    "y",
    src_crs = 32633
  )
  expect_null(res$message)
  expect_lt(max(abs(res$bins$rho)), 0.35)
})

test_that("the rank-based cross-correlogram runs on ranks and is flagged", {
  df <- make_xcorr_df(150)
  res <- compute_spatial_cross_correlogram(
    df,
    "a",
    "b",
    "x",
    "y",
    src_crs = 32633,
    method = "spearman"
  )
  expect_true(res$ranked)
  expect_equal(res$r0, cor(df$a, df$b, method = "spearman"), tolerance = 1e-8)
})

test_that("the cross-correlogram explains itself instead of blanking", {
  df <- make_xcorr_df(60)
  expect_match(
    compute_spatial_cross_correlogram(df, "a", "b", NULL, NULL, NULL)$message,
    "Coordinates are not mapped"
  )
  expect_match(
    compute_spatial_cross_correlogram(df, "a", "a", "x", "y", 32633)$message,
    "two different variables"
  )
  expect_match(
    compute_spatial_cross_correlogram(
      df[1:5, ],
      "a",
      "b",
      "x",
      "y",
      32633
    )$message,
    "Insufficient data"
  )
  df$a <- 1
  expect_match(
    compute_spatial_cross_correlogram(df, "a", "b", "x", "y", 32633)$message,
    "constant"
  )
})

test_that("generate_spatial_cross_correlogram returns a ggplot in both states", {
  df <- make_xcorr_df(120)
  expect_s3_class(
    generate_spatial_cross_correlogram(df, "a", "b", "x", "y", 32633),
    "ggplot"
  )
  expect_s3_class(
    generate_spatial_cross_correlogram(df, "a", "b", NULL, NULL, NULL),
    "ggplot"
  )
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

test_that("check_collinearity reports near-duplicate pairs and constants apart", {
  # One near-duplicate pair (a, b), one variable that is a combination of two
  # others without matching either (z = x + y), and a constant.
  set.seed(8)
  n <- 60
  x <- rnorm(n); y <- rnorm(n); a <- rnorm(n)
  df <- data.frame(a = a, b = a + rnorm(n, sd = 0.05), x = x, y = y,
                   z = x + y + rnorm(n, sd = 0.05), k = 3)
  res <- check_collinearity(df, names(df))

  expect_setequal(names(res), c("has_collinearity", "pairs", "constant"))
  expect_equal(nrow(res$pairs), 1)
  expect_setequal(c(res$pairs$var1, res$pairs$var2), c("a", "b"))
  expect_equal(res$constant, "k")
  # A constant is not a collinearity finding: it appears in no other group.
  expect_false("k" %in% c(res$pairs$var1, res$pairs$var2))
  expect_true(res$has_collinearity)

  # A constant alone is not an advisory finding (it is excluded, not debated).
  only_k <- check_collinearity(data.frame(x = x, y = y, k = 3), c("x", "y", "k"))
  expect_false(only_k$has_collinearity)
  expect_equal(only_k$constant, "k")
})

test_that("a high VIF without a near-duplicate pair does not stop a PCA", {
  # z = x + y is predicted almost exactly by the other two (VIF far above 10),
  # yet no pair reaches |r| = 0.95: VIF describes regression coefficients and
  # is no objection to a PCA, which exists to summarise correlated variables.
  set.seed(8)
  n <- 60
  x <- rnorm(n); y <- rnorm(n)
  df <- data.frame(x = x, y = y, z = x + y + rnorm(n, sd = 0.05))
  vif <- diag(solve(stats::cor(df)))
  expect_gt(max(vif), 10)
  expect_lt(max(abs(stats::cor(df)[upper.tri(diag(3))])), 0.95)
  res <- check_collinearity(df, names(df))
  expect_false(res$has_collinearity)
  expect_equal(nrow(res$pairs), 0)
})

test_that("desc_empty_dt returns a renderable placeholder, never NULL", {
  # The desc module's DT outputs used to return NULL on their empty states,
  # against the app-wide "never hand a widget output NULL" convention (the
  # reason sci_dt(NULL) renders a placeholder), and a silent blank table also
  # hid WHY there was nothing to show.
  w <- desc_empty_dt("nothing here")
  expect_s3_class(w, "datatables")
  expect_true(any(grepl("nothing here", unlist(w$x$data))))
})

# ── complete-case reporting ────────────────────────────────────────────────

test_that("complete_case_note states the sample and the rows it dropped", {
  expect_equal(complete_case_note(50, 50), "Complete cases: n = 50 of 50 rows.")
  expect_equal(
    complete_case_note(37, 50),
    "Complete cases: n = 37 of 50 rows (13 dropped for missing values)."
  )
})
