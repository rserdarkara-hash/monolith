# Tests for the RK linear-trend presentation helpers (ui_helpers.R):
# rk_fit_stats / rk_coef_table / format_p_value / signif_stars /
# build_rk_trend_ui. Display-layer only — values must reproduce the
# corresponding summary.lm quantities exactly.

make_rk_fixture <- function() {
  set.seed(42)
  n <- 40
  df <- data.frame(
    elev = runif(n, 100, 300),
    ndvi = runif(n, 0.2, 0.8)
  )
  df$soc <- 1.5 + 0.01 * df$elev + 2 * df$ndvi + rnorm(n, sd = 0.3)
  lm(soc ~ elev + ndvi, data = df)
}

test_that("format_p_value follows the display convention", {
  expect_equal(format_p_value(0.0004), "< 0.001")
  expect_equal(format_p_value(0.0234), "0.023")
  expect_equal(format_p_value(0.5), "0.500")
  expect_equal(format_p_value(NA), "NA")
  expect_equal(format_p_value(NULL), "NA")
})

test_that("signif_stars matches the conventional thresholds", {
  expect_equal(signif_stars(0.0005), "***")
  expect_equal(signif_stars(0.005), "**")
  expect_equal(signif_stars(0.03), "*")
  expect_equal(signif_stars(0.07), ".")
  expect_equal(signif_stars(0.5), "")
  expect_equal(signif_stars(NA), "")
})

test_that("rk_fit_stats reproduces summary.lm quantities", {
  fit <- make_rk_fixture()
  s <- summary(fit)
  stats <- rk_fit_stats(s)

  expect_equal(stats$r2, s$r.squared)
  expect_equal(stats$adj_r2, s$adj.r.squared)
  expect_equal(stats$sigma, s$sigma)
  expect_equal(stats$df_res, s$df[2])
  expect_equal(stats$f_value, unname(s$fstatistic[1]))
  expect_equal(stats$n, nobs(fit))
  expect_equal(
    stats$f_p,
    unname(pf(s$fstatistic[1], s$fstatistic[2], s$fstatistic[3],
              lower.tail = FALSE))
  )
})

test_that("rk_fit_stats and rk_coef_table reject non-summary objects", {
  expect_null(rk_fit_stats(NULL))
  expect_null(rk_fit_stats(list(foo = 1)))
  expect_null(rk_coef_table(NULL))
  expect_null(rk_coef_table(list(coefficients = matrix(1))))
})

test_that("rk_coef_table reproduces coefficients and t-based CIs", {
  fit <- make_rk_fixture()
  s <- summary(fit)
  tab <- rk_coef_table(s)

  expect_equal(colnames(tab),
               c("Term", "Estimate", "Std. Error", "95% CI",
                 "t value", "p value", "Sig."))
  expect_equal(tab$Term, c("(Intercept)", "elev", "ndvi"))
  expect_equal(tab$Estimate, signif(unname(coef(fit)), 4))

  # CI column must match stats::confint (same t quantile on residual df)
  ci <- confint(fit)
  expect_equal(tab[["95% CI"]],
               sprintf("[%.4g, %.4g]", ci[, 1], ci[, 2]))
})

test_that("rk_coef_table maps terms to metadata labels", {
  fit <- make_rk_fixture()
  meta <- list(
    list(actual = "elev", label = "Elevation (m)", category = "Terrain"),
    list(actual = "ndvi", label = "NDVI", category = "Remote Sensing")
  )
  tab <- rk_coef_table(summary(fit), vars_metadata = meta)
  expect_equal(tab$Term, c("(Intercept)", "Elevation (m)", "NDVI"))
})

test_that("build_rk_trend_ui returns tags for summary.lm, NULL otherwise", {
  fit <- make_rk_fixture()
  ui <- build_rk_trend_ui(summary(fit), "dt_id", "raw_id")
  expect_s3_class(ui, "shiny.tag.list")
  html <- as.character(ui)
  expect_match(html, "dt_id")
  expect_match(html, "raw_id")
  expect_match(html, "Signif. codes", fixed = TRUE)

  expect_null(build_rk_trend_ui(list(not = "a summary"), "a", "b"))
})
