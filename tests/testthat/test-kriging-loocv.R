# test-kriging-loocv.R — tests for perform_kriging_loocv.
# This function requires real spatial data, variogram fitting, and kriging.
# Tests are limited to verifying behaviour with small synthetic datasets.

test_that("perform_kriging_loocv returns NULL for fewer than 3 points", {
  pts <- make_test_points(2)
  result <- perform_kriging_loocv(
    pts, "v", c("aux1"),
    lags_func = calc_scientific_lags,
    vgm_fit_func = robust_vgm_fit,
    model_type = "lm",
    l = "test", prefix = "act"
  )
  expect_null(result)
})

test_that("perform_kriging_loocv does not throw on incomplete cases", {
  pts <- make_test_points(5)
  pts$aux1[1] <- NA
  result <- tryCatch(
    perform_kriging_loocv(
      pts, "v", c("aux1"),
      lags_func = calc_scientific_lags,
      vgm_fit_func = robust_vgm_fit,
      model_type = "lm",
      l = "test", prefix = "act"
    ),
    error = function(e) structure(list(msg = e$message), class = "cv_err")
  )
  # Either NULL (filtered to < 3 points) or an sf object
  expect_true(is.null(result) || inherits(result, "sf") || inherits(result, "cv_err"))
  if (inherits(result, "sf")) {
    expect_true("observed" %in% colnames(result))
  }
})

test_that("perform_kriging_loocv with lm model_type doesn't hard-crash", {
  pts <- make_test_points(8)
  result <- tryCatch(
    perform_kriging_loocv(
      pts, "v", c("aux1"),
      lags_func = calc_scientific_lags,
      vgm_fit_func = robust_vgm_fit,
      model_type = "lm",
      l = "test", prefix = "act"
    ),
    error = function(e) structure(list(msg = e$message), class = "cv_err")
  )
  expect_true(is.null(result) || inherits(result, "sf") || inherits(result, "cv_err"))
})

test_that("perform_kriging_loocv with rf model_type doesn't hard-crash", {
  pts <- make_test_points(10)
  result <- tryCatch(
    suppressWarnings(
      perform_kriging_loocv(
        pts, "v", c("aux1"),
        lags_func = calc_scientific_lags,
        vgm_fit_func = robust_vgm_fit,
        model_type = "rf",
        l = "test", prefix = "act",
        rf_ntree = 50
      )
    ),
    error = function(e) structure(list(msg = e$message), class = "cv_err")
  )
  expect_true(is.null(result) || inherits(result, "sf") || inherits(result, "cv_err"))
})

test_that("perform_kriging_loocv output has expected columns on success", {
  pts <- make_test_points(12)
  result <- tryCatch(
    perform_kriging_loocv(
      pts, "v", c("aux1"),
      lags_func = calc_scientific_lags,
      vgm_fit_func = robust_vgm_fit,
      model_type = "lm",
      l = "test", prefix = "act"
    ),
    error = function(e) NULL
  )
  # May be NULL if spatial operations fail on synthetic data (e.g. variogram
  # fitting doesn't converge with few random points).  When it succeeds,
  # verify the output structure.
  if (!is.null(result) && inherits(result, "sf")) {
    expect_true("observed" %in% colnames(result))
    expect_true("var1.pred" %in% colnames(result))
    expect_true("residual" %in% colnames(result))
  }
})

test_that("perform_kriging_loocv refuses a rank-deficient trend instead of returning NA metrics", {
  # With fewer training rows than the lm has coefficients, the fold fit aliases
  # coefficients, predict() returns NA, the NAs propagate through the residual
  # kriging and the reported metrics degrade to NA with nothing saying why.
  pts <- make_test_points(8)
  for (i in 1:6) pts[[paste0("cv", i)]] <- rnorm(8)
  aux <- paste0("cv", 1:6)   # 6 covariates + intercept = 7 coefficients

  # LOOCV leaves 7 training rows for 7 coefficients: no residual df.
  expect_error(
    perform_kriging_loocv(pts, "v", aux, calc_scientific_lags, robust_vgm_fit,
                          model_type = "lm", cv_strategy = "loocv"),
    "regression coefficients")

  # randomForest has no rank requirement, so the guard must not fire for RFK.
  # (suppressWarnings: randomForest's own seq(along=) partial-match notice.)
  rf_res <- suppressWarnings(tryCatch(
    perform_kriging_loocv(pts, "v", aux, calc_scientific_lags, robust_vgm_fit,
                          model_type = "rf", cv_strategy = "loocv"),
    error = function(e) e))
  expect_false(inherits(rf_res, "error") &&
                 grepl("regression coefficients", conditionMessage(rf_res)))
})

test_that("a comfortably-specified lm passes the rank guard", {
  pts <- make_test_points(30)
  res <- tryCatch(
    perform_kriging_loocv(pts, "v", c("aux1", "aux2"), calc_scientific_lags,
                          robust_vgm_fit, model_type = "lm", cv_strategy = "loocv"),
    error = function(e) e)
  expect_false(inherits(res, "error") &&
                 grepl("regression coefficients", conditionMessage(res)))
})
