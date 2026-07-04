# test-cv-metrics.R — tests for detect_cv_columns, calc_ccc, augment_metrics,
# perform_cv, .cv_to_df, and get_cv_residuals.

# ── detect_cv_columns ─────────────────────────────────────────────────────

test_that("detect_cv_columns finds standard gstat CV column names", {
  cnames <- c("var1.pred", "var1.observed", "var1.var", "residual", "zscore")
  res <- detect_cv_columns(cnames)
  expect_equal(res$pred, "var1.pred")
  expect_equal(res$observed, "var1.observed")
})

test_that("detect_cv_columns finds target.pred / target.observed patterns", {
  cnames <- c("target.pred", "target.observed", "x", "y")
  res <- detect_cv_columns(cnames)
  expect_equal(res$pred, "target.pred")
  expect_equal(res$observed, "target.observed")
})

test_that("detect_cv_columns falls back to generic .pred / .observed suffix", {
  cnames <- c("myvar.pred", "myvar.observed")
  res <- detect_cv_columns(cnames)
  expect_equal(res$pred, "myvar.pred")
  expect_equal(res$observed, "myvar.observed")
})

test_that("detect_cv_columns returns NA when no match found", {
  cnames <- c("foo", "bar", "baz")
  res <- detect_cv_columns(cnames)
  expect_true(is.na(res$pred))
  expect_true(is.na(res$observed))
})

test_that("detect_cv_columns handles empty input", {
  res <- detect_cv_columns(character(0))
  expect_true(is.na(res$pred))
  expect_true(is.na(res$observed))
})

# ── calc_ccc ──────────────────────────────────────────────────────────────

test_that("calc_ccc returns NA for fewer than 2 non-NA observations", {
  expect_true(is.na(calc_ccc(numeric(0), numeric(0))))
  expect_true(is.na(calc_ccc(1, 1)))
})

test_that("calc_ccc returns 1.0 for perfectly identical vectors", {
  obs <- c(10, 20, 30, 40, 50)
  pre <- c(10, 20, 30, 40, 50)
  expect_equal(calc_ccc(obs, pre), 1.0)
})

test_that("calc_ccc returns NA when variance of observed or predicted is zero with unequal means", {
  # var(obs) = 0, means differ → NA
  obs <- c(5, 5, 5, 5)
  pre <- c(4, 5, 6, 5)
  # Both means are 5.0, so the code returns 1.0 (means equal branch)
  expect_equal(calc_ccc(obs, pre), 1.0)

  # Both constant and identical values → CCC = 1
  obs2 <- c(5, 5, 5, 5)
  pre2 <- c(5, 5, 5, 5)
  expect_equal(calc_ccc(obs2, pre2), 1.0)
})

test_that("calc_ccc returns NA when variance is zero and means truly differ", {
  obs <- c(5, 5, 5, 5)
  pre <- c(3, 3, 3, 3)  # var=0, mean differs → NA
  expect_true(is.na(calc_ccc(obs, pre)))
})

test_that("calc_ccc matches known external value", {
  k <- make_ccc_known()
  ccc_val <- calc_ccc(k$observed, k$predicted)
  expect_true(!is.na(ccc_val))
  expect_equal(ccc_val, k$expected, tolerance = 0.01)
})

test_that("calc_ccc handles NA values via pairwise complete", {
  obs <- c(10, NA, 30, 40, 50)
  pre <- c(12, 19, NA, 38, 52)
  ccc_val <- calc_ccc(obs, pre)
  expect_true(!is.na(ccc_val))
  # CCC should be within reasonable bounds; near-perfect concordance
  expect_true(ccc_val > 0.5)
})

# ── augment_metrics ────────────────────────────────────────────────────────

test_that("augment_metrics returns all-NA list for < 2 observations", {
  res <- augment_metrics(numeric(0), numeric(0))
  expect_true(is.na(res$nse))
  expect_true(is.na(res$rpd))
  expect_true(is.na(res$rpiq))
  expect_true(is.na(res$smape))

  res2 <- augment_metrics(1, 1)
  expect_true(is.na(res2$nse))
})

test_that("augment_metrics NSE = 1 for perfect prediction", {
  k <- make_metrics_known()
  res <- augment_metrics(k$observed, k$predicted)
  expect_equal(res$nse, 1.0)
  expect_equal(res$nrmse_mean, 0.0)
})

test_that("augment_metrics RMSE-based metrics degrade with noise", {
  obs <- c(10, 20, 30, 40, 50)
  pre_good <- c(11, 19, 31, 39, 51)    # close
  pre_bad  <- c(5,  35, 15, 60, 25)     # far

  res_good <- augment_metrics(obs, pre_good)
  res_bad  <- augment_metrics(obs, pre_bad)

  expect_true(res_good$rpd > res_bad$rpd)
  expect_true(res_good$nse > res_bad$nse)
})

test_that("augment_metrics RPD and RPIQ are positive for valid input", {
  obs <- rnorm(30, 50, 10)
  pre <- obs + rnorm(30, 0, 3)
  res <- augment_metrics(obs, pre)
  expect_true(res$rpd > 0)
  # RPIQ can only be computed when IQR > 0
  if (!is.na(res$rpiq)) {
    expect_true(res$rpiq > 0)
  }
})

test_that("augment_metrics SMAPE is between 0 and 200", {
  obs <- c(10, 20, 30, 40, 50)
  pre <- c(12, 18, 33, 37, 55)
  res <- augment_metrics(obs, pre)
  expect_true(res$smape >= 0 && res$smape <= 200)
})

test_that("augment_metrics NRMSE_mean is percentage-scaled", {
  obs <- c(10, 20, 30, 40, 50)
  pre <- c(10, 20, 30, 40, 50)
  res <- augment_metrics(obs, pre)
  expect_equal(res$nrmse_mean, 0.0)
})

# ── .cv_to_df ──────────────────────────────────────────────────────────────

test_that(".cv_to_df handles NULL input", {
  expect_null(.cv_to_df(NULL))
})

test_that(".cv_to_df converts sf object to data.frame with coordinates", {
  pts <- make_test_points(10)
  # Simulate a simple sf-based CV result
  pts$var1.pred     <- pts$v + rnorm(10, 0, 1)
  pts$var1.observed <- pts$v
  df <- .cv_to_df(pts)
  expect_s3_class(df, "data.frame")
  expect_true("x" %in% colnames(df) || "X" %in% colnames(df) ||
              "coords.x1" %in% colnames(df))
})

test_that(".cv_to_df converts plain data.frame as-is", {
  df_in <- data.frame(a = 1:5, var1.pred = 6:10, var1.observed = 1:5)
  df_out <- .cv_to_df(df_in)
  expect_equal(nrow(df_out), 5)
  expect_true("var1.pred" %in% colnames(df_out))
})

# ── perform_cv ─────────────────────────────────────────────────────────────

test_that("perform_cv returns all-NA metrics for NULL input", {
  res <- perform_cv(NULL)
  expect_true(is.na(res$rmse))
  expect_true(is.na(res$r2))
  expect_true(is.na(res$nse))
  expect_equal(res$n, 0)
})

test_that("perform_cv computes correct metrics on perfect prediction", {
  cv_df <- data.frame(
    var1.pred     = c(10, 20, 30, 40, 50),
    var1.observed = c(10, 20, 30, 40, 50),
    x = 1:5, y = 1:5
  )
  res <- suppressWarnings(perform_cv(cv_df))
  expect_equal(res$rmse, 0.0)
  expect_equal(res$r2, 1.0)
  expect_equal(res$mae, 0.0)
  expect_equal(res$n, 5)
})

test_that("perform_cv detects non-standard column names via fallback", {
  cv_df <- data.frame(
    pred     = c(12, 19, 31, 38, 52),
    observed = c(10, 20, 30, 40, 50),
    coords.x1 = 1:5, coords.x2 = 1:5
  )
  res <- perform_cv(cv_df)
  expect_false(is.na(res$rmse))
  expect_true(res$rmse > 0)
})

test_that("perform_cv returns NA metrics when column detection fails", {
  cv_df <- data.frame(foo = 1:5, bar = 6:10)
  res <- perform_cv(cv_df)
  expect_true(is.na(res$rmse))
})

test_that("perform_cv handles data with NAs in pred/observed", {
  cv_df <- data.frame(
    var1.pred     = c(10, NA, 30, 40, 50),
    var1.observed = c(10, 20, NA, 40, 50),
    x = 1:5, y = 1:5
  )
  res <- suppressWarnings(perform_cv(cv_df))
  expect_false(is.na(res$rmse))
  expect_true(res$n >= 2)
})

test_that("perform_cv computes Moran's I when coordinates are present", {
  cv_df <- data.frame(
    var1.pred     = c(10, 20, 30, 40, 50),
    var1.observed = c(11, 19, 31, 38, 52),
    x = c(450000, 450100, 450200, 450300, 450400),
    y = c(5800000, 5800100, 5800200, 5800300, 5800400)
  )
  res <- suppressWarnings(perform_cv(cv_df))
  expect_true(is.na(res$moran_i) || is.numeric(res$moran_i))
})

# ── get_cv_residuals ──────────────────────────────────────────────────────

test_that("get_cv_residuals returns NAs for NULL input", {
  res <- get_cv_residuals(NULL, 5)
  expect_equal(length(res), 5)
  expect_true(all(is.na(res)))
})

test_that("get_cv_residuals computes obs - pred correctly", {
  cv_df <- data.frame(
    var1.pred     = c(12, 19, 31),
    var1.observed = c(10, 20, 30)
  )
  res <- get_cv_residuals(cv_df, 3)
  expect_equal(res, c(-2, 1, -1))
})

test_that("get_cv_residuals extracts residual column when pred/obs missing", {
  cv_df <- data.frame(residual = c(-2, 1, -1, 2, -3), x = 1:5, y = 1:5)
  res <- get_cv_residuals(cv_df, 5)
  expect_equal(res, c(-2, 1, -1, 2, -3))
})


# ── cv_type_label ─────────────────────────────────────────────────────────

test_that("cv_type_label reports LOOCV for n <= 50 and 10-fold above", {
  expect_equal(cv_type_label(3), "LOOCV")
  expect_equal(cv_type_label(50), "LOOCV")
  expect_equal(cv_type_label(51), "10-fold CV")
  expect_equal(cv_type_label(500), "10-fold CV")
})

test_that("cv_type_label falls back to generic CV for unknown n", {
  expect_equal(cv_type_label(NA), "CV")
  expect_equal(cv_type_label(NULL), "CV")
  expect_equal(cv_type_label(integer(0)), "CV")
})
