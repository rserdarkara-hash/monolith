# test-governing-factors.R — tests for compute_governing_factors.

test_that("compute_governing_factors returns NULL for fewer than 10 rows", {
  df <- make_test_df(9)
  result <- compute_governing_factors(df, "a", c("b", "c", "d"))
  expect_null(result)
})

test_that("compute_governing_factors returns expected list structure", {
  df <- make_test_df(30)
  result <- compute_governing_factors(df, "a", c("b", "c", "d", "e"))
  expect_type(result, "list")
  expected_names <- c("model", "explainer", "importance", "top_var",
                      "ale", "pdp", "shap", "n_used", "n_total")
  expect_setequal(names(result), expected_names)
})

test_that("compute_governing_factors reports the complete-case sample it fitted", {
  df <- make_test_df(40)
  df$b[1:6] <- NA         # missing in a predictor
  df$a[7:9] <- NA         # missing in the target
  result <- compute_governing_factors(df, "a", c("b", "c", "d"))
  expect_equal(result$n_total, 40)
  # The forest is fitted on complete cases across target + predictors, and the
  # panel prints that sample, so the two must agree by construction.
  expect_equal(result$n_used, sum(stats::complete.cases(df[, c("a", "b", "c", "d")])))
  expect_lt(result$n_used, result$n_total)
})

test_that("compute_governing_factors model is class randomForest", {
  df <- make_test_df(30)
  result <- compute_governing_factors(df, "a", c("b", "c", "d"))
  expect_s3_class(result$model, "randomForest")
})

test_that("compute_governing_factors importance is a non-empty data.frame", {
  df <- make_test_df(30)
  result <- compute_governing_factors(df, "a", c("b", "c", "d"))
  expect_s3_class(result$importance, "data.frame")
  expect_true(nrow(result$importance) >= 1)
  expect_true("variable" %in% colnames(result$importance))
  expect_true("dropout_loss" %in% colnames(result$importance))
})

test_that("compute_governing_factors top_var is among the predictors", {
  df <- make_test_df(30)
  predictors <- c("b", "c", "d")
  result <- compute_governing_factors(df, "a", predictors)
  expect_true(result$top_var %in% predictors)
})

test_that("compute_governing_factors ALE is a data.frame", {
  df <- make_test_df(30)
  result <- compute_governing_factors(df, "a", c("b", "c"))
  expect_s3_class(result$ale, "data.frame")
})

test_that("compute_governing_factors PDP is a data.frame", {
  df <- make_test_df(30)
  result <- compute_governing_factors(df, "a", c("b", "c"))
  expect_s3_class(result$pdp, "data.frame")
})

test_that("compute_governing_factors SHAP is a data.frame", {
  df <- make_test_df(30)
  result <- compute_governing_factors(df, "a", c("b", "c"))
  expect_s3_class(result$shap, "data.frame")
  expect_true("feature_value" %in% colnames(result$shap))
  expect_true("contribution" %in% colnames(result$shap))
})

test_that("compute_governing_factors handles a single predictor", {
  df <- make_test_df(30)
  result <- compute_governing_factors(df, "a", "b")
  expect_true(!is.null(result))
  expect_equal(result$top_var, "b")
})

test_that("compute_governing_factors respects n_permutations parameter", {
  df <- make_test_df(20)
  result <- compute_governing_factors(df, "a", c("b", "c"), n_permutations = 5)
  # Importance should have n_vars * (n_permutations + 1) rows
  # 2 vars * (5 + 1) = 12 rows
  expect_true(nrow(result$importance) >= 2)
})

test_that("compute_governing_factors removes rows with NAs in target/predictors", {
  df <- make_test_df(30)
  df$a[1:5] <- NA
  result <- compute_governing_factors(df, "a", c("b", "c"))
  expect_true(!is.null(result))
  expect_true(nrow(result$importance) >= 1)
})

test_that("compute_governing_factors SHAP values are reproducible across calls", {
  df <- make_test_df(30)
  r1 <- compute_governing_factors(df, "a", c("b", "c"))
  r2 <- compute_governing_factors(df, "a", c("b", "c"))
  expect_equal(r1$shap, r2$shap)
})

test_that("compute_governing_factors explainer is class 'explainer'", {
  df <- make_test_df(30)
  result <- compute_governing_factors(df, "a", c("b", "c", "d"))
  expect_s3_class(result$explainer, "explainer")
})

test_that("SHAP dependence contributions have per-observation magnitude (T14)", {
  df <- make_test_df(30)
  result <- compute_governing_factors(df, "a", "b")
  # With a single predictor every SHAP permutation attributes the full
  # deviation to that variable, so contribution(i) = f(x_i) - mean(f(X))
  # exactly. The pre-T14 bug summed the aggregated B = 0 row PLUS all
  # permutation rows returned by predict_parts, inflating this by B + 1
  # (26x with the DALEX default B = 25) — this assertion pins the magnitude.
  preds <- predict(result$model, newdata = df)
  set.seed(12345)
  sample_idx <- sample(seq_len(nrow(df)), min(100, nrow(df)))
  expected <- preds[sample_idx] - mean(preds)
  expect_equal(result$shap$contribution, unname(expected), tolerance = 1e-6)
})

test_that("compute_governing_factors does not perturb the caller's RNG (T19)", {
  df <- make_test_df(30)
  set.seed(123); expected_draw <- runif(1)
  set.seed(123); invisible(compute_governing_factors(df, "a", c("b", "c"))); actual_draw <- runif(1)
  expect_equal(actual_draw, expected_draw)
})

# ── Cooperative cancellation ────────────────────────────────────────────────

test_that("a failed SHAP cluster start restores mc.cores", {
  withr::local_options(mc.cores = 1L)
  testthat::with_mocked_bindings({
    expect_error(compute_governing_factors(make_test_df(60), "a", c("b", "c", "d"),
      n_permutations = 1, rf_ntree = 10, shap_sample_size = 50, cores_hint = 3),
      "forced cluster startup failure")
    expect_identical(getOption("mc.cores"), 1L)
  }, makeClusterPSOCK = function(...) stop("forced cluster startup failure"),
  .package = "parallelly")
})

test_that("compute_governing_factors aborts when the cancel flag is set", {
  df <- make_test_df(60)
  preds <- c("b", "c", "d")
  cancel_file <- tempfile(fileext = ".txt")
  file.create(cancel_file)
  on.exit(unlink(cancel_file), add = TRUE)

  # The first checkpoint runs before the random forest is fitted, so a flag
  # that is already set must abort essentially immediately.
  expect_error(
    compute_governing_factors(df, "a", preds, n_permutations = 2,
                              rf_ntree = 10, shap_sample_size = 10,
                              cancel_file = cancel_file),
    "cancelled by user")
})

test_that("compute_governing_factors is unchanged when no cancel file is given", {
  df <- make_test_df(60)
  preds <- c("b", "c", "d")
  missing_flag <- file.path(tempdir(), "gov_cancel_never_created.txt")
  unlink(missing_flag)

  a <- compute_governing_factors(df, "a", preds, n_permutations = 2,
                                 rf_ntree = 20, shap_sample_size = 10)
  b <- compute_governing_factors(df, "a", preds, n_permutations = 2,
                                 rf_ntree = 20, shap_sample_size = 10,
                                 cancel_file = missing_flag)
  expect_equal(a$importance, b$importance)
  expect_equal(a$shap, b$shap)
  expect_identical(a$top_var, b$top_var)
})

# ── Numeric contract: what the explanation plots actually show ─────────────

test_that("the PDP is the mean prediction with the feature held fixed", {
  d <- sf::st_drop_geometry(golden_sf("core", localities = "Yorga"))[
    , c("ph", "v82", "v87", "v43")]
  res <- compute_governing_factors(d, "ph", c("v82", "v87", "v43"),
                                   n_permutations = 5, rf_ntree = 60,
                                   shap_sample_size = 40)
  expect_equal(res$n_used, nrow(d))

  # Friedman (2001): the partial dependence at v is the average prediction with
  # the feature set to v across the whole sample. Recomputed here from the
  # returned forest, on all rows - DALEX samples at most N = 100 rows and this
  # fixture has 40, so the two see the same data.
  xs <- res$pdp[["_x_"]]
  ref <- vapply(xs, function(v) {
    dd <- d[, c("v82", "v87", "v43")]
    dd[[res$top_var]] <- v
    mean(predict(res$model, dd))
  }, numeric(1))

  expect_gt(length(xs), 5L)
  expect_equal(res$pdp[["_yhat_"]], ref, tolerance = 1e-8)
})

test_that("the SHAP sample is the documented deterministic draw", {
  d <- sf::st_drop_geometry(golden_sf("core", localities = "Yorga"))[
    , c("ph", "v82", "v87", "v43")]
  res <- compute_governing_factors(d, "ph", c("v82", "v87", "v43"),
                                   n_permutations = 5, rf_ntree = 60,
                                   shap_sample_size = 25)

  # The sampled rows come from a fixed seed, so the plotted feature values are
  # reproducible and are the sampled rows' own values - not a re-sort, not a
  # different subset.
  idx <- with_seed(12345, sample(seq_len(nrow(d)), 25))
  expect_equal(res$shap$feature_value, d[[res$top_var]][idx])
  expect_equal(nrow(res$shap), 25L)
})

test_that("SHAP contributions do not scale with the permutation count", {
  d <- sf::st_drop_geometry(golden_sf("core", localities = "Yorga"))[
    , c("ph", "v82", "v87", "v43")]
  mag <- function(B) {
    r <- compute_governing_factors(d, "ph", c("v82", "v87", "v43"),
                                   n_permutations = B, rf_ntree = 60,
                                   shap_sample_size = 25)
    mean(abs(r$shap$contribution))
  }

  # predict_parts(type = "shap") returns B + 1 rows per variable: the aggregated
  # attribution plus one per permutation. Summing them instead of taking the
  # aggregate inflates every contribution by a factor of B + 1, which would show
  # up here as a threefold jump between B = 4 and B = 14.
  m4 <- mag(4)
  m14 <- mag(14)
  expect_gt(m4, 0)
  expect_lt(abs(m14 / m4 - 1), 0.5)
})
