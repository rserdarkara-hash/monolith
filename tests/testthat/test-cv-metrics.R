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

test_that("calc_ccc returns NA whenever either vector is constant (CCC undefined)", {
  # var(obs) = 0: the correlation term is 0/0 regardless of the means, so the
  # 2026-07-19 convention reports NA instead of asserting agreement.
  obs <- c(5, 5, 5, 5)
  pre <- c(4, 5, 6, 5)
  expect_true(is.na(calc_ccc(obs, pre)))

  # Two equal constant vectors: formula degenerates to 0/0 → NA.
  obs2 <- c(5, 5, 5, 5)
  pre2 <- c(5, 5, 5, 5)
  expect_true(is.na(calc_ccc(obs2, pre2)))
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
  # 1e-5 separates the population-moment definition (Lin 1989, DescTools) from
  # the sample-moment one - they differ by 8e-5 on this fixture, which the
  # previous 0.01 tolerance could not resolve. Re-derive the fixture externally
  # if a formula ever changes; never relax this to make the test pass.
  expect_equal(ccc_val, k$expected, tolerance = 1e-5)
})

test_that("calc_ccc filters to jointly complete pairs before computing moments", {
  obs <- c(10, NA, 30, 40, 50)
  pre <- c(12, 19, NA, 38, 52)
  ccc_val <- calc_ccc(obs, pre)
  expect_true(!is.na(ccc_val))
  # Misaligned NAs: means, variances and the covariance must all come from the
  # SAME jointly complete subset, so the value equals CCC of the pre-filtered
  # pairs (the old per-vector na.rm mixed subsets here).
  ok <- !is.na(obs) & !is.na(pre)
  expect_equal(ccc_val, calc_ccc(obs[ok], pre[ok]))
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
  k <- make_metrics_known()$perfect
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

# Every metric here is a ratio and each has a zero-denominator configuration.
# NA (undefined), never +/-Inf, which would flow into the Model Performance
# table, the metrics CSV and the pooled Total (Combined) diagnostics.
test_that("augment_metrics NSE is NA when observations are constant", {
  obs <- rep(7.5, 8)
  pre <- c(7.4, 7.6, 7.5, 7.7, 7.3, 7.5, 7.6, 7.4)
  res <- augment_metrics(obs, pre)
  expect_true(is.na(res$nse))
  expect_false(is.infinite(res$nse))
})

test_that("augment_metrics NRMSE_mean is NA for a zero-mean variable", {
  obs <- c(-2, -1, 0, 1, 2)          # centred/anomaly variable: mean(obs) == 0
  pre <- c(-1.8, -1.1, 0.2, 0.9, 2.1)
  res <- augment_metrics(obs, pre)
  expect_true(is.na(res$nrmse_mean))
  expect_false(is.infinite(res$nrmse_mean))
})

test_that("augment_metrics RPD and RPIQ are NA at zero RMSE", {
  obs <- c(10, 20, 30, 40, 50)
  res <- augment_metrics(obs, obs)   # perfect prediction: RMSE == 0
  expect_true(is.na(res$rpd))
  expect_true(is.na(res$rpiq))
  expect_false(is.infinite(res$rpd))
  expect_false(is.infinite(res$rpiq))

  # ...and through perform_cv, which is what the uploaded-prediction tables
  # call. A "predicted" column uploaded as a copy of the measured column is a
  # routine sanity check, and it is the reachable path to this branch.
  pcv <- perform_cv(data.frame(var1.observed = obs, var1.pred = obs), moran = FALSE)
  expect_true(is.na(pcv$rpd))
  expect_true(is.na(pcv$rpiq))
  expect_false(is.infinite(pcv$rpd))
  expect_false(is.infinite(pcv$rpiq))
  # The reason those tables must not call yardstick directly: it answers Inf.
  expect_true(is.infinite(yardstick::rpd_vec(obs, obs)))
})

test_that("augment_metrics sMAPE defines the 0/0 term as 0, keeping n consistent", {
  # Rows where obs == pre == 0 would give NaN and be dropped by na.rm,
  # averaging sMAPE over a different n than every other metric.
  obs <- c(0, 10, 20, 0)
  pre <- c(0, 10, 20, 0)
  expect_equal(augment_metrics(obs, pre)$smape, 0)

  # One genuinely wrong row out of four: mean over n = 4, not n = 2.
  obs2 <- c(0, 0, 10, 10)
  pre2 <- c(0, 0, 10, 30)
  # terms: 0, 0, 0, 2*20/40 = 1  ->  mean = 0.25  ->  25%
  expect_equal(augment_metrics(obs2, pre2)$smape, 25)

  # Same convention through perform_cv, the authority the uploaded-prediction
  # tables read. yardstick voids the whole column on a single zero pair.
  expect_equal(perform_cv(data.frame(var1.observed = obs2, var1.pred = pre2),
                          moran = FALSE)$smape, 25)
  expect_true(is.nan(yardstick::smape_vec(obs2, pre2)))
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

test_that("the metric dictionary reproduces its external known answers", {
  # A perfect-prediction fixture cannot distinguish a correct RMSE from one with
  # the wrong divisor, nor NSE from SSE/SST, so every metric perform_cv reports
  # is pinned here on a NON-degenerate pair whose answers were derived from the
  # definitions (see make_metrics_known). round_values = FALSE because the
  # display lattice (1e-4 / 0.01) is coarser than the tolerance below.
  k <- make_metrics_known()
  m <- perform_cv(data.frame(var1.observed = k$observed, var1.pred = k$predicted),
                  moran = FALSE, round_values = FALSE)
  expect_equal(m$rmse,       k$rmse,  tolerance = 1e-9)
  expect_equal(m$mae,        k$mae,   tolerance = 1e-9)
  expect_equal(m$me,         k$me,    tolerance = 1e-9)
  expect_equal(m$r2,         k$r2,    tolerance = 1e-9)
  expect_equal(m$nse,        k$nse,   tolerance = 1e-9)
  expect_equal(m$nrmse_mean, k$nrmse, tolerance = 1e-9)
  expect_equal(m$rpd,        k$rpd,   tolerance = 1e-9)
  expect_equal(m$rpiq,       k$rpiq,  tolerance = 1e-9)
  expect_equal(m$smape,      k$smape, tolerance = 1e-9)
  expect_equal(m$n, length(k$observed))

  # The uploaded-prediction tables (server_sci_analysis.R, server_execution.R,
  # server_setup.R) read these same fields instead of calling yardstick, whose
  # ccc_vec defaults to the sample-moment variant this app removed in 1.0.8.
  # Both halves are pinned: what the app's CCC IS, and what it is NOT.
  expect_equal(calc_ccc(k$observed, k$predicted),
               yardstick::ccc_vec(k$observed, k$predicted, bias = TRUE),
               tolerance = 1e-10)
  expect_false(isTRUE(all.equal(calc_ccc(k$observed, k$predicted),
                                yardstick::ccc_vec(k$observed, k$predicted))))
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

test_that("perform_cv returns NA R2 on a constant vector without warning", {
  # cor() already returns NA for a zero-variance vector, but it emits "the
  # standard deviation is zero" on the way, and inside a PSOCK worker that
  # warning lands in the user's run log with nothing explaining it.
  # augment_metrics() and calc_ccc() guard the same degenerate case explicitly.
  cv_df <- data.frame(
    var1.pred     = c(10, 20, 30, 40, 50),
    var1.observed = rep(30, 5),
    x = 1:5, y = 1:5
  )
  res <- expect_no_warning(perform_cv(cv_df, moran = FALSE))
  expect_true(is.na(res$r2))
  # The metrics that remain defined are still reported.
  expect_false(is.na(res$rmse))
  expect_equal(res$n, 5)

  # ...and the same when the PREDICTIONS are constant (a flat surface).
  cv_flat <- data.frame(
    var1.pred     = rep(30, 5),
    var1.observed = c(10, 20, 30, 40, 50),
    x = 1:5, y = 1:5
  )
  res_flat <- expect_no_warning(perform_cv(cv_flat, moran = FALSE))
  expect_true(is.na(res_flat$r2))
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
  # I travels with its null expectation and significance; the table cannot be
  # read honestly without them (E[I] = -1/(n-1) is negative, not 0).
  expect_true(all(c("moran_i", "moran_e", "moran_p") %in% names(res)))
  if (!is.na(res$moran_i)) {
    expect_equal(res$moran_e, round(-1 / (res$n - 1), 4))
    expect_true(is.na(res$moran_p) || (res$moran_p >= 0 && res$moran_p <= 1))
  }
})

test_that("perform_cv leaves every Moran field NA when coordinates are absent", {
  cv_df <- data.frame(
    var1.pred     = c(10, 20, 30, 40, 50),
    var1.observed = c(11, 19, 31, 38, 52)
  )
  res <- suppressWarnings(perform_cv(cv_df))
  expect_true(is.na(res$moran_i))
  expect_true(is.na(res$moran_e))
  expect_true(is.na(res$moran_p))
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


# ── cv_type_label / resolve_cv_plan / make_cv_folds ────────────────────────

test_that("cv_type_label (Auto) reports LOOCV for n <= 50 and random 10-fold above", {
  expect_equal(cv_type_label(3), "LOOCV")
  expect_equal(cv_type_label(50), "LOOCV")
  expect_equal(cv_type_label(51), "Random 10-fold CV")
  expect_equal(cv_type_label(500), "Random 10-fold CV")
})

test_that("cv_type_label falls back to generic CV for unknown n", {
  expect_equal(cv_type_label(NA), "CV")
  expect_equal(cv_type_label(NULL), "CV")
  expect_equal(cv_type_label(integer(0)), "CV")
})

test_that("cv_type_label reflects the chosen strategy", {
  expect_equal(cv_type_label(500, "loocv"), "Full LOOCV")
  expect_equal(cv_type_label(20, "loocv"), "Full LOOCV")
  expect_match(cv_type_label(500, "block"), "^Spatial Block CV")
  # Spatial Block degrades to LOOCV below the minimum block size
  expect_match(cv_type_label(20, "block"), "^LOOCV")
  # NULL / empty strategy is treated as Auto
  expect_equal(cv_type_label(51, NULL), "Random 10-fold CV")
})

test_that("resolve_cv_plan encodes fold type and count per strategy", {
  expect_equal(resolve_cv_plan("loocv", 200)$type, "loocv")
  expect_equal(resolve_cv_plan("auto", 30)$type, "loocv")
  expect_equal(resolve_cv_plan("auto", 100)$type, "random_kfold")
  expect_equal(resolve_cv_plan("auto", 100)$k, 10L)
  expect_equal(resolve_cv_plan("block", 100)$type, "block")
  expect_equal(resolve_cv_plan("block", 100)$k, 10L)
  expect_equal(resolve_cv_plan("block", 25)$type, "loocv") # small-n degrade
})

test_that("resolve_cv_plan degrades to auto on an unrecognised strategy", {
  # A stale or hand-edited run-config upload must not raise match.arg's error
  # inside a PSOCK worker (it surfaces as the generic "Parallel Interpolation
  # Failed" modal instead of anything actionable).
  expect_silent(plan <- resolve_cv_plan("spatial", 100))
  expect_equal(plan$type, resolve_cv_plan("auto", 100)$type)
  expect_equal(resolve_cv_plan("nonsense", 30)$type, "loocv")
})

test_that("make_cv_folds returns valid, reproducible, strategy-appropriate folds", {
  set.seed(1)
  coords <- cbind(runif(100, 0, 1000), runif(100, 0, 1000))

  # LOOCV: one point per fold
  loo <- make_cv_folds(coords, "loocv", 100)
  expect_equal(loo, seq_len(100))

  # Auto n > 50: balanced random 10-fold, reproducible
  a1 <- make_cv_folds(coords, "auto", 100)
  a2 <- make_cv_folds(coords, "auto", 100)
  expect_identical(a1, a2)
  expect_equal(length(a1), 100)
  expect_equal(sort(unique(a1)), 1:10)
  expect_true(all(table(a1) == 10)) # balanced

  # Auto n <= 50: LOOCV
  expect_equal(make_cv_folds(coords[1:40, ], "auto", 40), seq_len(40))

  # Spatial Block: 10 contiguous k-means folds, reproducible
  b1 <- make_cv_folds(coords, "block", 100)
  b2 <- make_cv_folds(coords, "block", 100)
  expect_identical(b1, b2)
  expect_equal(length(b1), 100)
  expect_equal(length(unique(b1)), 10)
})


# ── Repeated cross-validation (opt-in) ────────────────────────────────────

test_that("make_cv_folds' default seed is the reference realization", {
  set.seed(2)
  coords <- cbind(runif(100, 0, 1000), runif(100, 0, 1000))

  # The default MUST stay CV_FOLD_SEED: every reported single-realization
  # metric in the app comes from this partition, so a repeat run can never
  # move a number that was already displayed.
  expect_identical(make_cv_folds(coords, "auto", 100),
                   make_cv_folds(coords, "auto", 100, CV_FOLD_SEED))

  # A different seed is a different partition of the same plan (same k, same
  # balance) - the fold assignment is the ONLY thing a repeat varies.
  r2 <- make_cv_folds(coords, "auto", 100, CV_FOLD_SEED + 1L)
  expect_false(identical(make_cv_folds(coords, "auto", 100), r2))
  expect_equal(sort(unique(r2)), 1:10)
  expect_true(all(table(r2) == 10))
  expect_identical(r2, make_cv_folds(coords, "auto", 100, CV_FOLD_SEED + 1L))

  # LOOCV ignores the seed entirely: it is deterministic by construction.
  expect_identical(make_cv_folds(coords, "loocv", 100, CV_FOLD_SEED + 7L),
                   seq_len(100))
})

test_that("cv_repeat_count collapses to one realization for deterministic plans", {
  # Off / nonsense input
  expect_equal(cv_repeat_count(NULL, "auto", 100), 1L)
  expect_equal(cv_repeat_count(1, "auto", 100), 1L)
  expect_equal(cv_repeat_count(0, "auto", 100), 1L)
  expect_equal(cv_repeat_count(NA, "auto", 100), 1L)
  expect_equal(cv_repeat_count("5", "auto", 100), 5L)
  # Sanity cap
  expect_equal(cv_repeat_count(1000, "auto", 100), 25L)
  # Every LOOCV plan repeats the identical partition, so repeats are pointless
  expect_equal(cv_repeat_count(5, "loocv", 100), 1L)  # explicit LOOCV
  expect_equal(cv_repeat_count(5, "auto", 40), 1L)    # Auto degrades below n=50
  expect_equal(cv_repeat_count(5, "block", 20), 1L)   # Block degrades below n=30
  # Live plans keep the request
  expect_equal(cv_repeat_count(5, "auto", 100), 5L)
  expect_equal(cv_repeat_count(3, "block", 100), 3L)
})

test_that("perform_cv(moran = FALSE) skips only the Moran block", {
  cv_df <- data.frame(
    var1.pred     = c(10, 20, 30, 40, 50),
    var1.observed = c(11, 19, 31, 38, 52),
    x = c(450000, 450100, 450200, 450300, 450400),
    y = c(5800000, 5800100, 5800200, 5800300, 5800400)
  )
  with_moran <- suppressWarnings(perform_cv(cv_df))
  no_moran <- suppressWarnings(perform_cv(cv_df, moran = FALSE))

  expect_true(is.na(no_moran$moran_i))
  expect_true(is.na(no_moran$moran_e))
  expect_true(is.na(no_moran$moran_p))
  # Everything else must be bit-identical - repeated CV reports these columns.
  for (k in c("rmse", "mae", "r2", "nse", "me", "ccc", "nrmse_mean", "rpd", "rpiq", "smape", "n")) {
    expect_equal(no_moran[[k]], with_moran[[k]], info = k)
  }
})

test_that("cv_repeat_frame normalises engine CV objects to one shape", {
  pts <- make_test_points(12)
  # krige.cv-style names
  a <- sf::st_as_sf(data.frame(observed = pts$v, var1.pred = pts$v + 1,
                               x = sf::st_coordinates(pts)[, 1],
                               y = sf::st_coordinates(pts)[, 2]),
                    coords = c("x", "y"), crs = sf::st_crs(pts))
  # gstat.cv-style names, plus columns nothing downstream needs
  b <- sf::st_as_sf(data.frame(var1.observed = pts$v, var1.pred = pts$v - 1,
                               zscore = 0, fold = 1,
                               x = sf::st_coordinates(pts)[, 1],
                               y = sf::st_coordinates(pts)[, 2]),
                    coords = c("x", "y"), crs = sf::st_crs(pts))

  fa <- cv_repeat_frame(a)
  fb <- cv_repeat_frame(b)
  expect_s3_class(fa, "sf")
  expect_equal(setdiff(names(fa), attr(fa, "sf_column")), c("observed", "var1.pred"))
  expect_identical(names(fa), names(fb))
  # Identical column sets are what makes pool_cv_sf's rbind safe across
  # localities whose engines produced different CV objects.
  expect_s3_class(rbind(fa, fb), "sf")
  # Metrics are unchanged by the trim
  expect_equal(perform_cv(fa, moran = FALSE)$rmse, perform_cv(a, moran = FALSE)$rmse)

  expect_null(cv_repeat_frame(NULL))
  expect_null(cv_repeat_frame(sf::st_as_sf(data.frame(foo = 1, x = 1, y = 1),
                                           coords = c("x", "y"), crs = 4326)))
})

test_that("summarise_cv_repeats reports mean and SD across realizations", {
  pts <- make_test_points(15)
  mk <- function(offset) {
    sf::st_as_sf(data.frame(observed = pts$v, var1.pred = pts$v + offset,
                            x = sf::st_coordinates(pts)[, 1],
                            y = sf::st_coordinates(pts)[, 2]),
                 coords = c("x", "y"), crs = sf::st_crs(pts))
  }
  reps <- list(mk(1), mk(2), mk(3))
  summ <- summarise_cv_repeats(reps)

  expect_equal(summ$n_repeats, 3L)
  expect_equal(summ$n, 15L)
  # ME is mean(obs - pred) = -offset, so the three realizations are -1, -2, -3
  expect_equal(unname(summ$mean[["me"]]), -2)
  expect_equal(unname(summ$sd[["me"]]), sd(c(-1, -2, -3)))
  # RMSE for a constant offset is the offset itself
  expect_equal(unname(summ$mean[["rmse"]]), 2)
  expect_equal(names(summ$mean), names(CV_REPEAT_METRICS))

  # Fewer than two realizations is not a spread; a NULL member poisons the set
  expect_null(summarise_cv_repeats(list(mk(1))))
  expect_null(summarise_cv_repeats(list(mk(1), NULL, mk(2))))
})

# ── Display rounding must not reach the repeated-CV aggregation ────────────
# 2026-08-23 audit, Tier 2: mean/SD across fold realizations were taken over
# values perform_cv had already rounded for display, so the SD reported the
# rounding lattice (RPD 0.01, RMSE 1e-4) instead of fold-assignment variance.

test_that("perform_cv(round_values = FALSE) returns full precision, TRUE is the default", {
  cv_df <- data.frame(
    var1.pred     = c(10.00013, 20.00027, 30.00041, 40.00019, 50.00033),
    var1.observed = c(10, 20, 30, 40, 50)
  )
  rounded <- perform_cv(cv_df, moran = FALSE)
  raw <- perform_cv(cv_df, moran = FALSE, round_values = FALSE)

  # Default is unchanged: the reference Model Performance table still rounds.
  expect_identical(rounded, perform_cv(cv_df, moran = FALSE, round_values = TRUE))
  for (k in c("rmse", "me", "mae", "r2", "ccc")) {
    expect_equal(rounded[[k]], round(raw[[k]], 4), info = k)
  }
  for (k in c("nrmse_mean", "rpd", "rpiq", "smape")) {
    expect_equal(rounded[[k]], round(raw[[k]], 2), info = k)
  }
  # The raw RMSE here is well below the 1e-4 display lattice, so rounding it
  # destroys the value entirely - the exact failure mode the flag exists for.
  expect_gt(raw$rmse, 0)
  expect_equal(rounded$rmse, 3e-4)
  expect_false(isTRUE(all.equal(raw$rmse, rounded$rmse)))
})

test_that("augment_metrics(round_values = FALSE) leaves its ratios unrounded", {
  obs <- c(1, 2, 3, 4, 5, 6, 7, 8)
  pre <- obs + c(0.0031, -0.0027, 0.0044, -0.0019, 0.0038, -0.0022, 0.0029, -0.0035)
  raw <- augment_metrics(obs, pre, round_values = FALSE)
  rounded <- augment_metrics(obs, pre)

  expect_identical(rounded, augment_metrics(obs, pre, round_values = TRUE))
  expect_equal(rounded$rpd, round(raw$rpd, 2))
  expect_equal(rounded$nse, round(raw$nse, 4))
  # RPD on a near-perfect fit is large, so the 0.01 display lattice discards
  # real variation between realizations.
  expect_false(isTRUE(all.equal(raw$rpd, rounded$rpd)))
})

test_that("summarise_cv_repeats aggregates raw metrics, not the display lattice", {
  pts <- make_test_points(15, seed = 4)
  # Offsets separated by far less than the RPD/RMSE display rounding: on the
  # rounded values the three realizations collapse onto one lattice point and
  # the SD reads 0, which would claim perfect stability across folds.
  mk <- function(offset) {
    sf::st_as_sf(data.frame(observed = pts$v, var1.pred = pts$v + offset,
                            x = sf::st_coordinates(pts)[, 1],
                            y = sf::st_coordinates(pts)[, 2]),
                 coords = c("x", "y"), crs = sf::st_crs(pts))
  }
  offsets <- c(1.000011, 1.000022, 1.000033)
  reps <- lapply(offsets, mk)
  summ <- summarise_cv_repeats(reps)

  # RMSE for a constant offset is the offset, so the SD is known exactly.
  expect_equal(unname(summ$sd[["rmse"]]), sd(offsets))
  expect_gt(unname(summ$sd[["rmse"]]), 0)
  expect_gt(unname(summ$sd[["rpd"]]), 0)
  # The old behaviour: aggregating the rounded values reports zero spread.
  rounded_rmse <- vapply(reps, function(r) perform_cv(r, moran = FALSE)$rmse, numeric(1))
  expect_equal(sd(rounded_rmse), 0)
})

test_that("build_cv_repeat_summary pools localities and recycles deterministic ones", {
  pts <- make_test_points(15, seed = 11)
  mk <- function(offset) {
    sf::st_as_sf(data.frame(observed = pts$v, var1.pred = pts$v + offset,
                            x = sf::st_coordinates(pts)[, 1],
                            y = sf::st_coordinates(pts)[, 2]),
                 coords = c("x", "y"), crs = sf::st_crs(pts))
  }
  # Locality A repeated three times; locality B under a deterministic LOOCV
  # plan, so it ships a single frame that must be reused in every pooled repeat.
  out <- build_cv_repeat_summary(list(A = list(mk(1), mk(2), mk(3)), B = list(mk(2))))

  expect_equal(out$n_repeats, 3L)
  expect_equal(names(out$per_loc), "A")            # B has no spread of its own
  expect_equal(unname(out$per_loc$A$mean[["me"]]), -2)
  expect_false(is.null(out$total))
  expect_equal(out$total$n, 30L)                   # both localities in every repeat
  # Pooled ME per repeat: mean of (-1,-2), (-2,-2), (-3,-2) = -1.5, -2, -2.5
  expect_equal(unname(out$total$mean[["me"]]), -2)
  expect_equal(unname(out$total$sd[["me"]]), sd(c(-1.5, -2, -2.5)))

  # Nothing to report when no locality produced more than one realization
  expect_null(build_cv_repeat_summary(list(A = list(mk(1)))))
  expect_null(build_cv_repeat_summary(list()))
  expect_null(build_cv_repeat_summary(NULL))
})

test_that("make_cv_folds preserves the caller's RNG stream", {
  coords <- cbind(runif(60, 0, 1000), runif(60, 0, 1000))
  set.seed(123); expected <- runif(1)
  set.seed(123); invisible(make_cv_folds(coords, "block", 60)); actual <- runif(1)
  expect_equal(actual, expected)
})

# ── pool_cv_sf: pooled "Total (Combined)" diagnostics CRS ───────────────────

test_that("pool_cv_sf pools per-locality sf CV objects in a metric auto-UTM CRS", {
  # Two localities projected in different local UTM zones (35N and 36N).
  make_loc_sf <- function(lon0, epsg) {
    df <- data.frame(lon = lon0 + runif(10, 0, 0.01), lat = 41 + runif(10, 0, 0.01),
                     observed = rnorm(10), var1.pred = rnorm(10))
    sf::st_transform(sf::st_as_sf(df, coords = c("lon", "lat"), crs = 4326), epsg)
  }
  set.seed(7)
  cv_list <- list(A = make_loc_sf(27.0, 32635), B = make_loc_sf(33.5, 32636))

  pooled <- pool_cv_sf(cv_list)
  expect_s3_class(pooled, "sf")
  expect_equal(nrow(pooled), 20)

  crs <- sf::st_crs(pooled)
  expect_false(isTRUE(sf::st_is_longlat(pooled)))
  expect_false(identical(crs, sf::st_crs(3857)))
  # combined centroid lon ~30.25 deg -> UTM zone 36 ((30.25+180)/6 + 1)
  expect_true(grepl("zone=36", crs$proj4string) || grepl("zone 36", crs$wkt))
  # metric units: pooled spread must be on the order of the true separation
  # (~540 km between zones), impossible if degrees survived
  bb <- sf::st_bbox(pooled)
  expect_gt(bb["xmax"] - bb["xmin"], 1e5)
})

test_that("pool_cv_sf skips CRS-less entries and returns NULL when nothing is poolable", {
  set.seed(8)
  good <- sf::st_as_sf(data.frame(lon = 27 + runif(5, 0, 0.01), lat = 41 + runif(5, 0, 0.01),
                                  observed = rnorm(5), var1.pred = rnorm(5)),
                       coords = c("lon", "lat"), crs = 4326)
  bad <- data.frame(x = 1:5, y = 1:5, observed = rnorm(5), var1.pred = rnorm(5))

  pooled <- pool_cv_sf(list(A = good, B = bad))
  expect_s3_class(pooled, "sf")
  expect_equal(nrow(pooled), 5)

  expect_null(pool_cv_sf(list(A = bad)))
  expect_null(pool_cv_sf(list()))
  expect_null(pool_cv_sf(NULL))
})

test_that("NRMSE normalises by the absolute mean, so negative-mean variables report a positive value", {
  # rmse / mean(obs) with a signed mean gave centred/anomaly variables (or
  # sub-zero temperatures) a NEGATIVE NRMSE%, a nonsensical sign for a
  # normalised error. Positive-mean data are byte-identical under abs().
  obs <- c(-10, -12, -8, -11, -9)
  pre <- c(-9.5, -12.5, -8.2, -10.4, -9.3)
  neg <- augment_metrics(obs, pre)
  rmse <- sqrt(mean((obs - pre)^2))
  expect_gt(neg$nrmse_mean, 0)
  expect_equal(neg$nrmse_mean, round(rmse / abs(mean(obs)) * 100, 2))
  # Sign-flip symmetry: negating both vectors changes neither the error nor
  # the scale, so the sign-flipped twin must report the identical NRMSE.
  pos <- augment_metrics(-obs, -pre)
  expect_equal(neg$nrmse_mean, pos$nrmse_mean)
})

# ── Numeric contract: the CV numbers and the folds under them ──────────────

test_that("perform_cv agrees with yardstick on the metrics they share", {
  pts <- golden_sf("tiny")
  lags <- calc_scientific_lags(pts)
  fit <- suppressWarnings(
    robust_vgm_fit(gstat::variogram(ph ~ 1, pts, width = lags$width,
                                    cutoff = lags$cutoff), pts$ph))
  cv <- gstat::krige.cv(ph ~ 1, pts, model = fit, nfold = nrow(pts),
                        debug.level = 0)

  m <- perform_cv(cv, round_values = FALSE)
  o <- cv$observed
  p <- cv$var1.pred
  # An independent implementation of the same three conventions: RMSE is the
  # root of the mean squared residual, MAE the mean absolute residual, and R2
  # the squared Pearson correlation (not 1 - SSE/SST, which is NSE and is
  # reported separately).
  expect_equal(m$rmse, yardstick::rmse_vec(o, p), tolerance = 1e-10)
  expect_equal(m$mae, yardstick::mae_vec(o, p), tolerance = 1e-10)
  expect_equal(m$r2, yardstick::rsq_vec(o, p), tolerance = 1e-10)
  expect_equal(m$n, length(o))
})

test_that("krige.cv LOOCV equals a hand-written leave-one-out loop", {
  pts <- golden_sf("tiny")
  lags <- calc_scientific_lags(pts)
  fit <- suppressWarnings(
    robust_vgm_fit(gstat::variogram(ph ~ 1, pts, width = lags$width,
                                    cutoff = lags$cutoff), pts$ph))

  cv <- gstat::krige.cv(ph ~ 1, pts, model = fit, nfold = nrow(pts),
                        debug.level = 0)
  # Refit the whole system n times, each without one point. LOOCV means exactly
  # this and nothing else, so any short cut in the engine's fold handling shows
  # up here.
  hand <- vapply(seq_len(nrow(pts)), function(i) {
    gstat::krige(ph ~ 1, pts[-i, ], pts[i, ], model = fit,
                 debug.level = 0)$var1.pred
  }, numeric(1))

  expect_equal(cv$var1.pred, hand, tolerance = 1e-10)
  expect_equal(cv$observed, pts$ph)
})

test_that("spatial block folds are spatially compact and random folds are not", {
  pts <- golden_sf("core")
  co <- sf::st_coordinates(pts)
  D <- as.matrix(dist(co))
  ut <- upper.tri(D)

  ratio <- function(f) {
    same <- outer(f, f, "==") & ut
    mean(D[same]) / mean(D[!outer(f, f, "==") & ut])
  }

  block <- make_cv_folds(co, "block", nrow(co))
  rand <- make_cv_folds(co, "auto", nrow(co))

  expect_length(unique(block), 10L)
  expect_length(unique(rand), 10L)
  # A spatial block holds points that are near each other, so the mean distance
  # within a fold is a fraction of the mean distance between folds. A random
  # fold is a random subset of the same cloud, so the two are the same - which
  # is precisely why random k-fold over-reports skill on autocorrelated data.
  expect_lt(ratio(block), 0.4)
  expect_equal(ratio(rand), 1, tolerance = 0.05)
})

# ── export builders: the screen's table and the exported sheet ─────────────
# These exist because each export site used to re-derive its own subset of the
# metrics under its own labels. The assertion is parity with perform_cv (the
# metric authority) and with the column set the card renders — never a
# hardcoded number.

test_that("cv_metrics_export_df reports every perform_cv metric, numerically", {
  set.seed(11)
  cv <- data.frame(var1.observed = rnorm(60, 10, 2))
  cv$var1.pred <- cv$var1.observed + rnorm(60, 0, 0.4)
  res <- perform_cv(cv, moran = FALSE)

  out <- cv_metrics_export_df(res, "Actual Model", "Standard LOOCV")

  expect_equal(nrow(out), 1)
  expect_equal(names(out), c("Source", "CV Design", "n", unname(CV_METRIC_LABELS)))
  expect_equal(out$Source, "Actual Model")
  expect_equal(out$`CV Design`, "Standard LOOCV")
  expect_equal(out$n, res$n)
  # every value comes straight off perform_cv, and stays a number
  for (k in names(CV_METRIC_LABELS)) {
    col <- out[[unname(CV_METRIC_LABELS[[k]])]]
    expect_true(is.numeric(col), info = k)
    expect_equal(col, as.numeric(res[[k]] %||% NA_real_), info = k)
  }
  expect_null(cv_metrics_export_df(NULL, "x"))
})

test_that("the Model Performance column set and its export share one dictionary", {
  # The card drops Moran's expectation (it renders as a tooltip); nothing else
  # may differ, in membership or in order.
  screen <- unname(CV_METRIC_LABELS[setdiff(names(CV_METRIC_LABELS), "moran_e")])
  exported <- unname(CV_METRIC_LABELS)

  expect_equal(setdiff(exported, screen), "Moran E[I]")
  expect_equal(screen, exported[exported != "Moran E[I]"])
  # and the labels are the ones perform_cv can actually fill
  expect_true(all(names(CV_METRIC_LABELS) %in% names(perform_cv(NULL))))
})

test_that("cv_repeats_export_df splits mean and SD into numeric columns", {
  set.seed(12)
  reps <- lapply(1:4, function(i) {
    d <- data.frame(var1.observed = rnorm(40, 5, 1))
    d$var1.pred <- d$var1.observed + rnorm(40, 0, 0.3)
    d
  })
  summ <- summarise_cv_repeats(reps)
  skip_if(is.null(summ), "repeat summary unavailable")

  out <- cv_repeats_export_df(summ, "Actual Model")

  expect_equal(nrow(out), length(CV_REPEAT_METRICS))
  expect_equal(names(out), c("Source", "Fold realizations", "n", "Metric", "Mean", "SD"))
  expect_equal(out$Metric, unname(CV_REPEAT_METRICS))
  expect_true(is.numeric(out$Mean) && is.numeric(out$SD))
  expect_equal(out$Mean, unname(vapply(names(CV_REPEAT_METRICS),
                                       function(k) as.numeric(summ$mean[[k]]), numeric(1))))
  expect_equal(out$`Fold realizations`, rep(4L, nrow(out)))
  expect_null(cv_repeats_export_df(NULL, "x"))
})

test_that("pred_perf_df reports the uploaded-prediction dictionary off perform_cv", {
  set.seed(13)
  obs <- rnorm(50, 20, 3)
  pre <- obs + rnorm(50, 0.5, 1)
  cv_df <- data.frame(var1.observed = obs, var1.pred = pre)
  m <- perform_cv(cv_df, moran = FALSE, round_values = FALSE)

  # the card: perform_cv's own display rounding
  shown <- pred_perf_df(obs, pre, round_values = TRUE)
  m_shown <- perform_cv(cv_df, moran = FALSE)
  expect_equal(shown$Value[shown$Metric == "RMSE"], m_shown$rmse)
  expect_equal(shown$Value[shown$Metric == "NMAE (%)"],
               round(mean(abs(obs - pre)) / abs(mean(obs)) * 100, 2))

  # the export: full precision
  out <- pred_perf_df(obs, pre)

  expect_equal(nrow(out), 12)
  expect_true(is.numeric(out$Value))
  val <- function(nm) out$Value[out$Metric == nm]
  expect_equal(val("RMSE"), m$rmse)
  expect_equal(val("R² (NSE/Traditional)"), m$nse)
  expect_equal(val("SMAPE (%)"), m$smape)
  expect_equal(val("n"), as.numeric(m$n))
  # MBE is predicted-minus-observed, the documented sign flip against Bias (ME)
  expect_equal(val("MBE (ML pred - observed)"), -m$me)
  # NMAE off the raw residuals, not the display-rounded MAE
  expect_equal(val("NMAE (%)"), mean(abs(obs - pre)) / abs(mean(obs)) * 100)
  expect_null(pred_perf_df(obs[1:2], pre[1:2]))
})

test_that("pred_perf_df drops incomplete pairs before scoring", {
  obs <- c(1, 2, 3, 4, NA, 6)
  pre <- c(1.1, 2.2, NA, 4.1, 5, 5.8)
  keep <- !is.na(obs) & !is.na(pre)
  out <- pred_perf_df(obs, pre)
  expect_equal(out$Value[out$Metric == "n"], sum(keep))
  expect_equal(out$Value[out$Metric == "RMSE"],
               sqrt(mean((obs[keep] - pre[keep])^2)))
})

test_that("a strongly autocorrelated residual field exports a Moran p that is small, not zero", {
  # A 10 x 10 grid whose residual is a smooth east-west gradient. At 4 dp the
  # two-sided p (far below 5e-5 here) was rounded to exactly 0, an impossible
  # value, and landed in an exported numeric column.
  g <- expand.grid(x = seq(0, 900, by = 100), y = seq(0, 900, by = 100))
  g$var1.observed <- 10 + g$x / 100
  g$var1.pred <- 10 + 0.5 * g$x / 100
  res <- perform_cv(g)
  expect_gt(res$moran_i, 0.5)

  p <- cv_metrics_export_df(res, "Actual Model")[["Moran p"]]
  expect_true(is.numeric(p))
  expect_gt(p, 0)
  expect_lt(p, 1e-4)
})

test_that("the Model Performance labels extend the fold-realization labels", {
  # one dictionary: relabelling a metric in CV_REPEAT_METRICS relabels it in
  # Model Performance and both exports at once
  k <- names(CV_REPEAT_METRICS)
  expect_identical(names(CV_METRIC_LABELS)[seq_along(k)], k)
  expect_identical(CV_METRIC_LABELS[k], CV_REPEAT_METRICS)
  expect_equal(setdiff(names(CV_METRIC_LABELS), k), c("moran_i", "moran_e", "moran_p"))
})

test_that("rf_importance_df writes every importance measure the forest recorded", {
  set.seed(5)
  d <- data.frame(a = runif(80), b = runif(80), c = runif(80))
  d$y <- 3 * d$a + d$b + rnorm(80, 0, 0.1)

  # randomForest's own seq(along =) partial-match warning, not ours
  grow <- function(...) suppressWarnings(randomForest::randomForest(y ~ ., data = d, ntree = 60, ...))
  with_imp <- grow(importance = TRUE)
  out <- rf_importance_df(with_imp)
  expect_equal(names(out), c("Variable", "%IncMSE", "IncNodePurity"))
  expect_equal(out$Variable[1], "a")
  imp <- randomForest::importance(with_imp)
  expect_equal(out$IncNodePurity, unname(imp[out$Variable, "IncNodePurity"]))
  expect_false(is.unsorted(rev(out[["%IncMSE"]])))   # ordered by the first measure

  without <- grow()
  out2 <- rf_importance_df(without)
  expect_equal(names(out2), c("Variable", "IncNodePurity"))
  expect_equal(out2$Variable[1], "a")
})
