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
  expect_equal(ccc_val, k$expected, tolerance = 0.01)
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
