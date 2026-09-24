# test-tps-gcv-plot.R — the parameter-selection panels and the Regional
# Parameters table. build_tps_gcv_plot / build_idw_power_plot draw from the
# run's own fit record (res$tps_fit / res$idw_fit, committed to
# rv$disp$regional_params); every state without a curve must say why in the
# panel, never leave it blank. build_regional_params_df reports what each
# surface ran with, never the live tuning store.

make_gcv_df <- function() {
  data.frame(lambda = 10^seq(-6, 0, length.out = 8), gcv = (1:8) / 8)
}

# Collect all text-layer labels from a built plot ("" when none).
plot_label <- function(p) {
  b <- ggplot2::ggplot_build(p)
  labs <- unlist(lapply(b$data, function(d) {
    if ("label" %in% names(d)) as.character(d$label) else character(0)
  }))
  paste(labs, collapse = " ")
}

gcv_fit <- function(source = "own") {
  list(mode = "gcv", lambda = 1e-3, eff_df = 6.2, gcv = make_gcv_df(), gcv_source = source)
}

test_that("an Auto (GCV) fit draws its curve and marks the fitted lambda", {
  for (tgt in c("act", "pre")) {
    p <- build_tps_gcv_plot(gcv_fit(), "LocA", tgt)
    expect_s3_class(p, "ggplot")
    expect_identical(plot_label(p), "")
    b <- ggplot2::ggplot_build(p)
    expect_equal(nrow(b$data[[1]]), 8)
    # The vertical line sits at the fitted lambda (log10 x axis).
    expect_equal(b$data[[3]]$xintercept, log10(1e-3))
  }
  expect_no_match(build_tps_gcv_plot(gcv_fit(), "LocA", "act")$labels$subtitle, "measured")
  expect_match(build_tps_gcv_plot(gcv_fit("measured"), "LocA", "pre")$labels$subtitle,
               "GCV on the measured values", fixed = TRUE)
})

test_that("every TPS state without a curve says why", {
  expect_match(plot_label(build_tps_gcv_plot(gcv_fit(), "Total (Combined)", "act")), "per locality")
  expect_match(plot_label(build_tps_gcv_plot(NULL, "LocA", "act")), "fell back to IDW")
  expect_match(plot_label(build_tps_gcv_plot(list(lambda = NA_real_, eff_df = NA_real_), "LocA", "pre")),
               "fell back to IDW")
  fixed <- plot_label(build_tps_gcv_plot(list(mode = "fixed", lambda = 0.01, eff_df = 4), "LocA", "act"))
  expect_match(fixed, "no GCV search")
  expect_match(fixed, "0.01", fixed = TRUE)
  exact <- plot_label(build_tps_gcv_plot(list(mode = "exact", lambda = 0, eff_df = 40), "LocA", "act"))
  expect_match(exact, "exact interpolation")
  expect_match(exact, "no GCV search")
})

test_that("the GCV panel names an end of the spline family", {
  one_line <- function(fit) gsub("\n", " ", build_tps_gcv_plot(fit, "LocA", "act")$labels$subtitle)
  expect_no_match(one_line(gcv_fit()), "end", fixed = TRUE)
  plane <- modifyList(gcv_fit(), list(gcv_end = "plane"))
  expect_match(one_line(plane), "the least-squares plane: nothing lies beyond it.", fixed = TRUE)
  rough <- modifyList(gcv_fit(), list(gcv_end = "interpolation", exact_cv_rmse = 3.81, run_cv_rmse = 3.76))
  expect_match(one_line(rough), "cross-validates at RMSE 3.81 against 3.76 for this run.", fixed = TRUE)
  expect_match(one_line(modifyList(rough, list(exact_cv_rmse = 3.5))), "for this run: select Exact (λ = 0).",
               fixed = TRUE)
  expect_match(one_line(modifyList(rough, list(exact_cv_rmse = NA_real_))),
               "set Smoothing (λ) to Exact (λ = 0) and run again to compare.", fixed = TRUE)
})

test_that("an Auto (CV) IDW fit draws its power profile over the whole family", {
  g <- IDW_POWER_GRID
  fit <- list(mode = "cv", p = 2.5, limit = NULL, skipped = NULL, nmax = 12,
              profile = data.frame(p = g, rmse = ifelse(is.finite(g), (g - 2.5)^2 + 1, 50)),
              fold_p = c(`1` = 2, `2` = 2.5, `3` = 3.25))
  p <- build_idw_power_plot(fit, "LocA", "act")
  expect_identical(plot_label(p), "")
  b <- ggplot2::ggplot_build(p)
  # The finite powers draw the curve on a log(1 + p) axis; the selected power
  # carries the dashed line; the nearest-neighbour limit is a point of its own
  # at the right end, labelled ∞.
  expect_equal(nrow(b$data[[1]]), sum(is.finite(g)))
  expect_equal(b$data[[3]]$xintercept, log1p(2.5))
  expect_equal(nrow(b$data[[4]]), 1)
  expect_identical(utils::tail(p$scales$get_scales("x")$labels, 1), "∞")
  expect_match(p$labels$subtitle, "Selected p = 2.5 on all rows; CV folds selected 2.5 [2–3.25]",
               fixed = TRUE)
  # The profile fixes each power across the folds; the reported CV does not.
  expect_match(gsub("\n", " ", p$labels$caption), "re-selected in each fold", fixed = TRUE)

  one_line <- function(f) gsub("\n", " ", build_idw_power_plot(f, "LocA", "act")$labels$subtitle)
  # Filled points are the powers within one standard error of the best; the
  # subtitle says how many and whether p = 2 is among them.
  band <- abs(g - 2.5) <= 1
  banded <- fit
  banded$profile$within_se <- band
  pb <- build_idw_power_plot(banded, "LocA", "act")
  bb <- ggplot2::ggplot_build(pb)
  expect_identical(bb$data[[2]]$shape, ifelse(band[is.finite(g)], 16, 1))
  expect_identical(bb$data[[4]]$shape, 1)
  expect_match(one_line(banded), sprintf(paste0("%d of 37 powers are within one standard error of the best, ",
                                                "p = 2 among them: on these folds the data do not distinguish ",
                                                "p = 2 from p = 2.5."), sum(band)), fixed = TRUE)
  expect_match(gsub("\n", " ", pb$labels$caption), "Filled: within one standard error of the best", fixed = TRUE)
  # A steep finite power is read like the nearest-neighbour limit.
  steep <- one_line(modifyList(fit, list(p = 48)))
  expect_match(steep, "Auto (CV) selected p = 48: a sample 10% farther than the nearest keeps under a ninth",
               fixed = TRUE)
  expect_match(steep, "practically the stepped nearest-neighbour (Thiessen) surface", fixed = TRUE)
  eq <- one_line(modifyList(fit, list(p = 0, limit = "equal_weights")))
  expect_match(eq, "Selected equal weights (p = 0) on all rows", fixed = TRUE)
  expect_match(eq, "the map is the mean of the 12 nearest samples", fixed = TRUE)
  expect_match(eq, "Max Neighbors now sets the smoothing", fixed = TRUE)
  flat <- one_line(modifyList(fit, list(p = 0, limit = "equal_weights", n_samples = 12)))
  expect_match(flat, "Max Neighbors reaches all 12 samples, so the map is one value, their mean", fixed = TRUE)
  nn <- one_line(modifyList(fit, list(p = Inf, limit = "nearest_neighbour")))
  expect_match(nn, "Selected the nearest-neighbour limit (p → ∞) on all rows", fixed = TRUE)
  expect_match(nn, "a stepped (Thiessen) surface", fixed = TRUE)
})

test_that("every IDW state without a profile says why", {
  expect_match(plot_label(build_idw_power_plot(NULL, "Total (Combined)", "act")), "per locality")
  expect_match(plot_label(build_idw_power_plot(NULL, "LocA", "act")), "No IDW fit")
  expect_match(plot_label(build_idw_power_plot(list(mode = "fixed", p = 2), "LocA", "act")),
               "Fixed p = 2 for 'LocA' (Actual): no selection.", fixed = TRUE)
  skipped <- list(mode = "cv", p = 2, skipped = "4 samples, fewer than 5", profile = NULL)
  expect_match(plot_label(build_idw_power_plot(skipped, "LocA", "pre")), "fewer than 5")
})

# ── build_regional_params_df ───────────────────────────────────────────────

make_rp <- function() list(
  LocA = list(idw_p_act = 2.5, idw_p_pre = 3, tps_lambda_act = 0,    tps_lambda_pre = -1),
  LocB = list(idw_p_act = 2,   idw_p_pre = 2, tps_lambda_act = 0.01, tps_lambda_pre = 0)
)

test_that("regional parameter exports keep fitted values numeric", {
  rp <- make_rp()
  rp$LocA$tps_lambda_act <- -1
  rp$LocA$tps_fit_act <- list(lambda = 1.234e-9, eff_df = 5.6)
  rp$LocA$tps_fit_pre <- list(lambda = 2e-5, eff_df = 8)
  df <- build_regional_params_df("TPS", "LocA", rp, TRUE, export = TRUE)
  expect_equal(df$Mode, c("Auto (GCV)", "Auto (GCV)"))
  expect_equal(df$Surface, c("Actual", "Predicted"))
  expect_equal(df$Lambda, c(1.234e-9, 2e-5))
  expect_equal(df[["Effective df"]], c(5.6, 8))
  # On screen, lambda and df follow the app's display rule (format_sig), as
  # the GCV panel's subtitle does.
  shown <- build_regional_params_df("TPS", "LocA", rp, FALSE)
  expect_identical(shown$Actual, "Auto (GCV): 1.234e-09 (df 5.6)")
  expect_equal(build_regional_params_df("TPS", "LocB", make_rp(), TRUE, export = TRUE)$Mode,
               c("Fixed", "Exact"))
})

test_that("IDW parameters report the mode, the map power and the fold powers", {
  rp <- make_rp()
  rp$LocA$idw_p_act <- -1
  rp$LocA$idw_fit_act <- list(mode = "cv", p = 2.25, fold_p = c(`1` = 2, `2` = 2.25, `3` = 3))
  rp$LocA$idw_fit_pre <- list(mode = "fixed", p = 3)
  rp$LocB$idw_fit_act <- list(mode = "fixed", p = 2)
  rp$LocB$idw_fit_pre <- list(mode = "fixed", p = 2)

  ex <- build_regional_params_df("IDW", "Total (Combined)", rp, TRUE, export = TRUE)
  expect_equal(ex$Mode, c("Auto (CV)", "Fixed", "Fixed", "Fixed"))
  expect_equal(ex$Selected, c("p = 2.25", "p = 3", "p = 2", "p = 2"))
  expect_equal(ex[["Power (map)"]], c(2.25, 3, 2, 2))
  expect_equal(ex[["Fold power (median)"]], c(2.25, NA, NA, NA))
  expect_equal(ex[["Fold power (min)"]], c(2, NA, NA, NA))
  expect_equal(ex[["Fold power (max)"]], c(3, NA, NA, NA))
  expect_equal(ex[["Folds at the nearest-neighbour limit"]], c(0L, NA, NA, NA))

  shown <- build_regional_params_df("IDW", "LocA", rp, TRUE)
  expect_equal(shown$Param, "Power (p)")
  expect_equal(shown$Actual, "Auto (CV): 2.25 (folds 2.25 [2–3])")
  expect_equal(shown$Predicted, "Fixed: 3")
  # A surface without a fit record (not produced) states the setting it ran
  # with in the same wording.
  no_fit <- make_rp()
  no_fit$LocA$idw_p_act <- -1
  no_fit$LocA$idw_p_pre <- 0
  shown_nf <- build_regional_params_df("IDW", "LocA", no_fit, TRUE)
  expect_identical(c(shown_nf$Actual, shown_nf$Predicted), c("Auto (CV)", "Fixed: 0 (equal weights)"))
  # A typed power keeps four significant digits wherever it is named.
  expect_identical(param_setting_text("IDW", 2.125), "Fixed p = 2.125")
  expect_identical(build_regional_params_df("IDW", "LocB", modifyList(no_fit, list(LocB = list(idw_p_act = 12.25))),
                                            FALSE)$Actual, "Fixed: 12.25")

  # The nearest-neighbour limit (p = Inf): a spreadsheet cell cannot hold Inf,
  # so the limit is named and counted and its numeric cells stay empty.
  rp$LocA$idw_fit_act <- list(mode = "cv", p = Inf, fold_p = c(`1` = 6, `2` = Inf, `3` = Inf))
  ex_nn <- build_regional_params_df("IDW", "LocA", rp, TRUE, export = TRUE)[1, ]
  expect_identical(ex_nn$Selected, "the nearest-neighbour limit (p → ∞)")
  expect_true(is.na(ex_nn[["Power (map)"]]))
  expect_true(is.na(ex_nn[["Fold power (median)"]]))
  expect_equal(ex_nn[["Fold power (min)"]], 6)
  expect_true(is.na(ex_nn[["Fold power (max)"]]))
  expect_identical(ex_nn[["Folds at the nearest-neighbour limit"]], 2L)
  expect_equal(build_regional_params_df("IDW", "LocA", rp, TRUE)$Actual,
               "Auto (CV): ∞ (nearest neighbour) (folds ∞ [6–∞])")
  prof <- idw_profile_export_df(data.frame(p = c(0, 2, Inf), rmse = c(1.2, 1, 1.4)))
  expect_identical(prof$Candidate, c("equal weights (p = 0)", "p = 2", "the nearest-neighbour limit (p → ∞)"))
  expect_equal(prof[["Power (p)"]], c(0, 2, NA))
  expect_equal(prof[["Pooled CV RMSE"]], c(1.2, 1, 1.4))
  # The panel's filled points travel with the profile, and the parameter
  # export counts them and says whether p = 2 is among them.
  banded <- data.frame(p = c(0, 2, Inf), rmse = c(1.2, 1, 1.4), within_se = c(TRUE, TRUE, FALSE))
  expect_identical(idw_profile_export_df(banded)[["Within one SE of the best"]], c(TRUE, TRUE, FALSE))
  rb <- rp
  rb$LocA$idw_fit_act <- list(mode = "cv", p = 2, profile = banded, fold_p = c(`1` = 2))
  exb <- build_regional_params_df("IDW", "LocA", rb, TRUE, export = TRUE)
  expect_identical(exb[["Powers within one SE of the best"]], c(2L, NA))
  expect_identical(exb[["p = 2 within one SE"]], c(TRUE, NA))

  # A Fixed p = 0 is equal weights, and every view says so.
  rp$LocA$idw_p_pre <- 0
  rp$LocA$idw_fit_pre <- list(mode = "fixed", p = 0)
  expect_equal(build_regional_params_df("IDW", "LocA", rp, TRUE)$Predicted, "Fixed: 0 (equal weights)")
  expect_identical(build_regional_params_df("IDW", "LocA", rp, TRUE, export = TRUE)$Selected[2],
                   "equal weights (p = 0)")
  expect_identical(param_setting_text("IDW", 0), "Fixed p = 0 (equal weights)")
  expect_match(plot_label(build_idw_power_plot(rp$LocA$idw_fit_pre, "LocA", "pre")),
               "Fixed equal weights (p = 0) for 'LocA' (Predicted): no selection.", fixed = TRUE)
})

test_that("a selection states whose values it was made on, and a skipped search says why", {
  # An unseparated Predicted surface takes the measured values' selection: the
  # export says so, and so do the panel's subtitle and axis.
  rp <- make_rp()
  rp$LocA$idw_p_act <- -1
  rp$LocA$idw_p_pre <- -1
  prof <- data.frame(p = c(0, 2, Inf), rmse = c(1.2, 1, 1.4))
  rp$LocA$idw_fit_act <- list(mode = "cv", p = 2, profile = prof, select_source = "own", nmax = 12)
  rp$LocA$idw_fit_pre <- list(mode = "cv", p = 2, profile = prof, select_source = "measured", nmax = 12)
  ex <- build_regional_params_df("IDW", "LocA", rp, TRUE, export = TRUE)
  expect_identical(ex[["Selected on"]], c("measured values", "measured values"))
  expect_true(all(is.na(ex[["Not searched (reason)"]])))
  own_pre <- modifyList(rp, list(LocA = list(idw_fit_pre = list(select_source = "own"))))
  expect_identical(build_regional_params_df("IDW", "LocA", own_pre, TRUE, export = TRUE)[["Selected on"]][2],
                   "predicted values")
  p_pre <- build_idw_power_plot(rp$LocA$idw_fit_pre, "LocA", "pre")
  expect_match(p_pre$labels$subtitle, "Selected p = 2 on the measured values of all rows", fixed = TRUE)
  expect_identical(p_pre$labels$y, "Pooled CV RMSE of the measured values")
  p_act <- build_idw_power_plot(rp$LocA$idw_fit_act, "LocA", "act")
  expect_match(p_act$labels$subtitle, "Selected p = 2 on all rows", fixed = TRUE)
  expect_identical(p_act$labels$y, "Pooled CV RMSE")

  # A search that did not run names its reason in the export, as the table's
  # "(not searched)" does on screen.
  rp$LocB$idw_p_act <- -1
  rp$LocB$idw_fit_act <- list(mode = "cv", p = 2, skipped = "4 samples, fewer than 5", select_source = "own")
  exb <- build_regional_params_df("IDW", "LocB", rp, FALSE, export = TRUE)
  expect_identical(exb[["Not searched (reason)"]], "4 samples, fewer than 5")
  expect_identical(build_regional_params_df("IDW", "LocB", rp, FALSE)$Actual, "Auto (CV): 2 (not searched)")
  # Fixed powers select nothing.
  expect_true(is.na(build_regional_params_df("IDW", "LocB", make_rp(), FALSE, export = TRUE)[["Selected on"]]))

  # TPS: the lambda of an unseparated Predicted surface is GCV's on the measured values.
  rt <- make_rp()
  rt$LocA$tps_lambda_act <- -1
  rt$LocA$tps_fit_act <- list(mode = "gcv", lambda = 1e-4, eff_df = 6, gcv_source = "own")
  rt$LocA$tps_fit_pre <- list(mode = "gcv", lambda = 1e-4, eff_df = 6, gcv_source = "measured")
  expect_identical(build_regional_params_df("TPS", "LocA", rt, TRUE, export = TRUE)[["Selected on"]],
                   c("measured values", "measured values"))
  expect_true(all(is.na(build_regional_params_df("TPS", "LocB", make_rp(), TRUE, export = TRUE)[["Selected on"]])))
})

test_that("TPS parameter exports say where GCV's minimum sat and what exact interpolation scored", {
  rp <- make_rp()
  rp$LocA$tps_lambda_act <- -1
  rp$LocA$tps_fit_act <- list(mode = "gcv", lambda = 1e-6, eff_df = 78.8, gcv_end = "interpolation",
                              exact_cv_rmse = 3.81, run_cv_rmse = 3.76)
  rp$LocA$tps_fit_pre <- list(mode = "gcv", lambda = 20, eff_df = 3.001, gcv_end = "plane")
  df <- build_regional_params_df("TPS", "LocA", rp, TRUE, export = TRUE)
  expect_identical(df[["GCV minimum"]], c("least-smoothing end", "smoothest end (least-squares plane)"))
  expect_equal(df[["Exact (λ = 0) CV RMSE"]], c(3.81, NA))
  expect_equal(df[["Run CV RMSE"]], c(3.76, NA))
  # Fixed and Exact lambdas run no GCV search.
  expect_true(all(is.na(build_regional_params_df("TPS", "LocB", make_rp(), TRUE, export = TRUE)[["GCV minimum"]])))
})

test_that("a committed lambda of 0 reads Exact, not Auto (GCV)", {
  df <- build_regional_params_df("TPS", "LocA", make_rp(), has_pre = TRUE)
  expect_equal(df$Actual, "Exact: 0")
  expect_equal(df$Predicted, "Auto (GCV)")
  expect_equal(df$Param, "Lambda")
})

test_that("Total (Combined) lists every run locality with committed values", {
  df <- build_regional_params_df("TPS", "Total (Combined)", make_rp(), has_pre = TRUE)
  expect_equal(df$Locality, c("LocA", "LocB"))
  expect_equal(df$Actual, c("Exact: 0", "Fixed: 0.01"))
  expect_equal(df$Predicted, c("Auto (GCV)", "Exact: 0"))
})

test_that("Predicted column is dropped when the run had no predicted surface", {
  df <- build_regional_params_df("TPS", "LocB", make_rp(), has_pre = FALSE)
  expect_false("Predicted" %in% names(df))
  expect_equal(df$Actual, "Fixed: 0.01")

  df_tot <- build_regional_params_df("IDW", "Total (Combined)", make_rp(), has_pre = FALSE)
  expect_equal(names(df_tot), c("Locality", "Param", "Actual"))
  expect_equal(df_tot$Actual, c("Fixed: 2.5", "Fixed: 2"))
})

test_that("missing snapshot or unknown locality -> NULL", {
  expect_null(build_regional_params_df("TPS", "LocA", NULL, has_pre = TRUE))
  expect_null(build_regional_params_df("TPS", "LocA", list(), has_pre = TRUE))
  expect_null(build_regional_params_df("TPS", "LocX", make_rp(), has_pre = TRUE))
})

test_that("the per-locality panels encode and validate what they store", {
  expect_identical(idw_param_value("cv", 3), -1)
  expect_identical(idw_param_value("fixed", 3), 3)
  expect_identical(tps_param_value("gcv", 0.5), -1)
  expect_identical(tps_param_value("exact", 0.5), 0)
  expect_identical(tps_param_value("fixed", 2.4e-06), 2.4e-06)
  expect_identical(param_value_mode("IDW", -1), "cv")
  expect_identical(param_value_mode("IDW", 2), "fixed")
  expect_identical(param_value_mode("TPS", -1), "gcv")
  expect_identical(param_value_mode("TPS", NA_real_), "gcv")
  expect_identical(param_value_mode("TPS", 0), "exact")
  expect_identical(param_value_mode("TPS", 20.2), "fixed")
  expect_true(tps_fixed_ok(2.4e-06))
  expect_true(tps_fixed_ok(20.2))
  for (bad in list(0, -1, NA_real_, Inf, NULL, c(1, 2))) expect_false(tps_fixed_ok(bad))
  # A Fixed p spans the range Auto (CV) searches: equal weights (0) to the
  # largest finite power.
  expect_true(idw_fixed_ok(0))
  expect_true(idw_fixed_ok(2.5))
  expect_true(idw_fixed_ok(IDW_MAX_FINITE_POWER))
  for (bad in list(-0.1, IDW_MAX_FINITE_POWER + 0.5, Inf, NA_real_, NULL)) expect_false(idw_fixed_ok(bad))
})
