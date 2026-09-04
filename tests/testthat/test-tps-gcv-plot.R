# test-tps-gcv-plot.R — TPS diagnostics panel builders:
# build_tps_gcv_plot placeholder/curve states (GCV curves exist only after
# the "OPTIMIZE TPS LAMBDA" button runs; every empty state must yield an
# explicit placeholder, never a blank panel) and build_regional_params_df
# (reports the run-committed per-locality params, never the live store).

make_gcv_df <- function() {
  data.frame(lambda = 10^seq(-6, 0, length.out = 8), gcv = (1:8) / 8)
}

# Collect all text-layer labels from a built plot ("" when none).
gcv_plot_label <- function(p) {
  b <- ggplot2::ggplot_build(p)
  labs <- unlist(lapply(b$data, function(d) {
    if ("label" %in% names(d)) as.character(d$label) else character(0)
  }))
  paste(labs, collapse = " ")
}

test_that("no curves at all -> optimizer placeholder in Total and locality view", {
  for (loc in c("Total (Combined)", "LocA")) {
    p <- build_tps_gcv_plot(list(), loc, "act")
    expect_s3_class(p, "ggplot")
    expect_match(gcv_plot_label(p), "OPTIMIZE TPS LAMBDA")
  }
})

test_that("curves for the other target only -> optimizer placeholder", {
  gl <- list("LocA_act" = make_gcv_df())
  p <- build_tps_gcv_plot(gl, "LocA", "pre")
  expect_match(gcv_plot_label(p), "OPTIMIZE TPS LAMBDA")
  expect_match(gcv_plot_label(p), "Predicted")
})

test_that("curves exist + Total view -> per-locality prompt", {
  gl <- list("LocA_act" = make_gcv_df())
  p <- build_tps_gcv_plot(gl, "Total (Combined)", "act")
  expect_match(gcv_plot_label(p), "select a specific locality")
})

test_that("curves exist but not for selected locality -> explicit message", {
  gl <- list("LocA_act" = make_gcv_df())
  p <- build_tps_gcv_plot(gl, "LocB", "act")
  expect_match(gcv_plot_label(p), "No GCV curve is available for 'LocB'")
})

test_that("curve present -> real GCV plot with no placeholder text", {
  gl <- list("LocA_act" = make_gcv_df(), "LocA_pre" = make_gcv_df())
  for (tgt in c("act", "pre")) {
    p <- build_tps_gcv_plot(gl, "LocA", tgt)
    expect_s3_class(p, "ggplot")
    expect_identical(gcv_plot_label(p), "")
    b <- ggplot2::ggplot_build(p)
    expect_equal(nrow(b$data[[1]]), 8)
  }
})

# ── build_regional_params_df ───────────────────────────────────────────────

make_rp <- function() list(
  LocA = list(idw_p_act = 2.5, idw_p_pre = 3, tps_lambda_act = 0,    tps_lambda_pre = -1),
  LocB = list(idw_p_act = 2,   idw_p_pre = 2, tps_lambda_act = 0.01, tps_lambda_pre = 0)
)

test_that("committed lambda 0 shows as 0, not Auto (GCV)", {
  df <- build_regional_params_df("TPS", "LocA", make_rp(), has_pre = TRUE)
  expect_equal(df$Actual, "0")
  expect_equal(df$Predicted, "Auto (GCV)")
  expect_equal(df$Param, "Lambda")
})

test_that("Total (Combined) lists every run locality with committed values", {
  df <- build_regional_params_df("TPS", "Total (Combined)", make_rp(), has_pre = TRUE)
  expect_equal(df$Locality, c("LocA", "LocB"))
  expect_equal(df$Actual, c("0", "0.01"))
  expect_equal(df$Predicted, c("Auto (GCV)", "0"))
})

test_that("Predicted column is dropped when the run had no predicted surface", {
  df <- build_regional_params_df("TPS", "LocB", make_rp(), has_pre = FALSE)
  expect_false("Predicted" %in% names(df))
  expect_equal(df$Actual, "0.01")

  df_tot <- build_regional_params_df("IDW", "Total (Combined)", make_rp(), has_pre = FALSE)
  expect_equal(names(df_tot), c("Locality", "Param", "Actual"))
  expect_equal(df_tot$Actual, c("2.5", "2"))
})

test_that("IDW uses its own keys and label", {
  df <- build_regional_params_df("IDW", "LocA", make_rp(), has_pre = TRUE)
  expect_equal(df$Param, "Power (p)")
  expect_equal(df$Actual, "2.5")
  expect_equal(df$Predicted, "3")
})

test_that("missing snapshot or unknown locality -> NULL", {
  expect_null(build_regional_params_df("TPS", "LocA", NULL, has_pre = TRUE))
  expect_null(build_regional_params_df("TPS", "LocA", list(), has_pre = TRUE))
  expect_null(build_regional_params_df("TPS", "LocX", make_rp(), has_pre = TRUE))
})
