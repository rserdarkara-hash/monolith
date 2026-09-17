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

test_that("lm and rf kriging CV return a complete common schema", {
  pts <- make_test_points(12)
  for (engine in c("lm", "rf")) {
    cv <- suppressWarnings(perform_kriging_loocv(pts, "v", "aux1", calc_scientific_lags,
      robust_vgm_fit, model_type = engine, rf_ntree = 50))
    expect_s3_class(cv, "sf")
    expect_identical(names(cv), c("row_id", "fold", "observed", "var1.pred", "var1.var", "residual", "geometry"))
    expect_equal(nrow(cv), nrow(pts))
    expect_equal(cv$observed, pts$v)
    expect_true(all(is.finite(cv$var1.pred)))
    expect_true(all(is.na(cv$var1.var)))
  }
})

test_that("perform_kriging_loocv refuses a fold vector that does not match its rows", {
  pts <- make_test_points(12)
  expect_error(perform_kriging_loocv(pts, "v", "aux1", calc_scientific_lags, robust_vgm_fit,
                                     model_type = "lm", folds = seq_len(11)),
               "fold vector has 11 entries for 12 complete-case samples", fixed = TRUE)
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

test_that("RK and RFK holdout predictions do not use the held-out covariate", {
  set.seed(42)
  n <- 16L
  aux <- rnorm(n)
  pts <- sf::st_as_sf(data.frame(
    x = runif(n, 450000, 451000), y = runif(n, 5800000, 5801000),
    aux1 = aux, v = 5 * aux + rnorm(n, 0, 0.3)
  ), coords = c("x", "y"), crs = 32633)
  changed <- pts
  changed$aux1[1] <- changed$aux1[1] + 100

  for (type in c("lm", "rf")) {
    cv <- suppressWarnings(perform_kriging_loocv(
      pts, "v", "aux1", calc_scientific_lags, robust_vgm_fit,
      model_type = type, cv_strategy = "loocv", rf_ntree = 50))
    cv_changed <- suppressWarnings(perform_kriging_loocv(
      changed, "v", "aux1", calc_scientific_lags, robust_vgm_fit,
      model_type = type, cv_strategy = "loocv", rf_ntree = 50))
    expect_true(is.finite(cv$var1.pred[1]))
    expect_true(is.finite(cv_changed$var1.pred[1]))
    expect_equal(cv_changed$var1.pred[1], cv$var1.pred[1], tolerance = 1e-8)
  }
})

# ── The covariate screen inside each fold ─────────────────────────────────

test_that("the per-fold covariate screen raises no pruning warning", {
  set.seed(11)
  df <- data.frame(a = rnorm(30))
  df$b <- df$a + rnorm(30, 0, 1e-6)
  # One screen per fold would otherwise put one "VIF Iterative Pruning" warning
  # per fold in the run log (measured: 30 for 30 folds) saying nothing the
  # covariate-gate log line does not already say.
  expect_silent(res <- screen_covariates(df, c("a", "b")))
  expect_length(res$kept, 1)
  expect_identical(res$dropped_vif, setdiff(c("a", "b"), res$kept))
  # Fewer than two candidates have nothing to compare, so they pass through.
  expect_identical(screen_covariates(df, "a")$kept, "a")
  expect_identical(screen_covariates(df, character(0))$kept, character(0))
})

test_that("the RK fold screen reads only its own training rows", {
  # aux2 follows aux1 closely in every row but the last. Version A keeps that
  # pattern at the last row too, so the FULL-data screen sees VIF 38 and drops
  # one; version B puts an outlier there, so the full-data screen sees VIF 2.4
  # and keeps both. The last row's own fold trains on the SAME rows in both
  # versions (VIF 38 either way), so a screen run inside the fold must hand it
  # the same prediction — a screen run once on all the data cannot.
  set.seed(7)
  n <- 20L
  a1 <- rnorm(n)
  base <- sf::st_as_sf(data.frame(
    x = runif(n, 450000, 451000), y = runif(n, 5800000, 5801000),
    aux1 = a1, aux2 = a1 + rnorm(n, 0, 0.25),
    v = 3 * a1 + rnorm(n, 0, 0.5)
  ), coords = c("x", "y"), crs = 32633)
  a <- base
  b <- base; b$aux2[n] <- b$aux2[n] + 5

  expect_length(screen_covariates(a, c("aux1", "aux2"))$kept, 1)
  expect_length(screen_covariates(b, c("aux1", "aux2"))$kept, 2)

  grid <- make_test_grid_safe(base, res = 400)
  lags <- calc_scientific_lags(base)
  run <- function(p) suppressWarnings(apply_RK(p, "v", grid, lags,
    list(cv_strategy = "loocv"), c("aux1", "aux2")))
  res_a <- run(a); res_b <- run(b)
  expect_false(grepl("Falling back to OK", res_a$log_msg, fixed = TRUE))
  expect_false(grepl("Falling back to OK", res_b$log_msg, fixed = TRUE))
  expect_true(is.finite(res_a$cv_obj$var1.pred[n]))
  expect_equal(res_b$cv_obj$var1.pred[n], res_a$cv_obj$var1.pred[n], tolerance = 1e-8)
})

test_that("a fold whose screen keeps no covariate reports NA, not another model", {
  # Both covariates are zero in every row but the last, so the last row's own
  # fold trains on two constants: its screen keeps nothing and the fold has no
  # trend model. Its prediction must be NA and counted as missing coverage,
  # never filled in from another engine.
  set.seed(3)
  n <- 12L
  pts <- sf::st_as_sf(data.frame(
    x = runif(n, 450000, 451000), y = runif(n, 5800000, 5801000),
    aux1 = c(rep(0, n - 1L), 5), aux2 = c(rep(0, n - 1L), 3),
    v = rnorm(n, 10, 2)
  ), coords = c("x", "y"), crs = 32633)
  grid <- make_test_grid_safe(pts, res = 400)
  res <- suppressWarnings(apply_RK(pts, "v", grid, calc_scientific_lags(pts),
    list(cv_strategy = "loocv"), c("aux1", "aux2")))

  expect_false(grepl("Falling back to OK", res$log_msg, fixed = TRUE))
  expect_s3_class(res$res_sf, "sf")          # the map still runs
  expect_true(is.na(res$cv_obj$var1.pred[n]))
  expect_equal(sum(!is.na(res$cv_obj$var1.pred)), n - 1L)
  m <- perform_cv(res$cv_obj)
  expect_equal(m$n, n - 1L)
  expect_equal(m$n_expected, n)
  expect_match(res$log_msg, paste0("fold ", n), fixed = TRUE)
})

test_that("each RFK fold draws its forest from its own seed", {
  # A forest's number of RNG draws depends on the data it is grown on, so a
  # single stream over the whole fold loop lets EARLIER folds — which train on
  # the last row — decide where the last row's own forest starts. Raising only
  # the last row's target must not move its own out-of-fold prediction.
  set.seed(42)
  n <- 30L
  a1 <- rnorm(n); a2 <- rnorm(n)
  pts <- sf::st_as_sf(data.frame(
    x = runif(n, 450000, 452000), y = runif(n, 5800000, 5802000),
    aux1 = a1, aux2 = a2, v = 2 * a1 - a2 + rnorm(n, 0, 0.5)
  ), coords = c("x", "y"), crs = 32633)
  raised <- pts
  raised$v[n] <- raised$v[n] + 10 * sd(pts$v)

  cv_of <- function(p) suppressWarnings(perform_kriging_loocv(
    p, "v", c("aux1", "aux2"), calc_scientific_lags, robust_vgm_fit,
    model_type = "rf", cv_strategy = "loocv", rf_ntree = 100))
  base <- cv_of(pts); moved <- cv_of(raised)
  expect_true(is.finite(base$var1.pred[n]))
  expect_equal(moved$var1.pred[n], base$var1.pred[n], tolerance = 1e-8)
})
