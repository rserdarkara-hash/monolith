# test-governing-factors.R — tests for compute_governing_factors.

test_that("compute_governing_factors returns NULL for fewer than 10 rows", {
  df <- make_test_df(9)
  result <- compute_governing_factors(df, "a", c("b", "c", "d"))
  expect_null(result)
})

test_that("the returned list is the shape the Governing Factors panel reads", {
  # One fit, read seven ways. Each component used to have a block of its own
  # making the same call, which paid for eight forests plus eight DALEX passes
  # to assert eight classes.
  df <- make_test_df(30)
  predictors <- c("b", "c", "d")
  result <- compute_governing_factors(df, "a", predictors)

  expect_type(result, "list")
  expect_setequal(names(result),
                  c("model", "explainer", "importance", "top_var",
                    "ale", "pdp", "shap", "n_used", "n_total"))
  expect_s3_class(result$model, "randomForest")
  expect_s3_class(result$explainer, "explainer")

  expect_s3_class(result$importance, "data.frame")
  expect_gte(nrow(result$importance), 1L)
  expect_true(all(c("variable", "mse_increase") %in% colnames(result$importance)))
  expect_setequal(as.character(result$importance$variable), predictors)

  expect_true(result$top_var %in% predictors)
  expect_s3_class(result$ale, "data.frame")
  expect_s3_class(result$pdp, "data.frame")
  expect_s3_class(result$shap, "data.frame")
  expect_true(all(c("feature_value", "contribution") %in% colnames(result$shap)))
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

test_that("governing-factors importance is the forest's out-of-bag permutation importance", {
  df <- make_test_df(30)
  result <- compute_governing_factors(df, "a", c("b", "c"), rf_ntree = 30)

  # Breiman (2001): per tree, the increase in out-of-bag MSE when a predictor
  # is permuted, averaged over the trees and NOT divided by its standard error.
  # The reference forest is grown here, independently, with the same seed.
  ref_fit <- with_seed(12345, randomForest::randomForest(
    x = df[, c("b", "c")], y = df$a, ntree = 30, importance = TRUE))
  ref <- randomForest::importance(ref_fit, type = 1, scale = FALSE)
  expect_equal(result$importance$variable, rownames(ref))
  expect_equal(result$importance$mse_increase, unname(ref[, 1]))
  # It is the unscaled column, not randomForest's default scaled one, which is
  # reported beside it: the same increase divided by its standard error.
  scaled <- unname(randomForest::importance(ref_fit, type = 1)[, 1])
  expect_false(isTRUE(all.equal(result$importance$mse_increase, scaled)))
  expect_equal(result$importance$mse_increase_scaled, scaled)
  expect_equal(result$importance$mse_increase_scaled,
               unname(ref[, 1] / ref_fit$importanceSD[rownames(ref)]))
})

test_that("out-of-bag importance gives a pure-noise predictor no credit", {
  # Permuting on the rows the forest was grown on credits a noise predictor
  # with what the forest memorised; the out-of-bag rows do not.
  set.seed(7)
  n <- 200
  d <- data.frame(x1 = runif(n), x2 = runif(n))
  d$y <- 3 * d$x1 + rnorm(n, 0, 0.3)
  res <- compute_governing_factors(d, "y", c("x1", "x2"), rf_ntree = 200, shap_sample_size = 20)
  imp <- stats::setNames(res$importance$mse_increase, res$importance$variable)
  expect_identical(res$top_var, "x1")
  expect_lt(imp[["x2"]], 0.02 * imp[["x1"]])
})

test_that("compute_governing_factors handles a single predictor", {
  df <- make_test_df(30)
  result <- compute_governing_factors(df, "a", "b")
  expect_equal(result$top_var, "b")
  expect_setequal(as.character(result$importance$variable), "b")
})

test_that("compute_governing_factors removes rows with NAs in target/predictors", {
  df <- make_test_df(30)
  df$a[1:5] <- NA
  result <- compute_governing_factors(df, "a", c("b", "c"))
  # The five NA-target rows are dropped from the fit and counted out of n_used,
  # while n_total still reports the sample the user supplied.
  expect_equal(result$n_total, 30)
  expect_equal(result$n_used, 25)
  # And the forest really was grown on those 25 rows, not on 30 with NAs.
  expect_length(result$model$predicted, 25)
})

test_that("compute_governing_factors SHAP values are reproducible across calls", {
  df <- make_test_df(30)
  r1 <- compute_governing_factors(df, "a", c("b", "c"))
  r2 <- compute_governing_factors(df, "a", c("b", "c"))
  expect_equal(r1$shap, r2$shap)
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
  set.seed(123)
  invisible(compute_governing_factors(df, "a", c("b", "c")))
  actual_draw <- runif(1)
  expect_equal(actual_draw, expected_draw)
})

# ── Cooperative cancellation ────────────────────────────────────────────────

test_that("a failed SHAP cluster start restores mc.cores", {
  withr::local_options(mc.cores = 1L)
  testthat::with_mocked_bindings({
    expect_error(
      compute_governing_factors(make_test_df(60), "a", c("b", "c", "d"),
                                rf_ntree = 10,
                                shap_sample_size = 50, cores_hint = 3),
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
    compute_governing_factors(df, "a", preds,
                              rf_ntree = 10, shap_sample_size = 10,
                              cancel_file = cancel_file),
    "cancelled by user")
})

test_that("compute_governing_factors is unchanged when no cancel file is given", {
  df <- make_test_df(60)
  preds <- c("b", "c", "d")
  missing_flag <- file.path(tempdir(), "gov_cancel_never_created.txt")
  unlink(missing_flag)

  a <- compute_governing_factors(df, "a", preds,
                                 rf_ntree = 20, shap_sample_size = 10)
  b <- compute_governing_factors(df, "a", preds,
                                 rf_ntree = 20, shap_sample_size = 10,
                                 cancel_file = missing_flag)
  expect_equal(a$importance, b$importance)
  expect_equal(a$shap, b$shap)
  expect_identical(a$top_var, b$top_var)
})

# ── Numeric contract: what the explanation plots actually show ─────────────

test_that("the PDP is the mean prediction with the feature held fixed", {
  d <- sf::st_drop_geometry(golden_sf("core", localities = golden_locality("core", "smallest", min_n = 30L)))[
    , c("ph", "v82", "v87", "v43")]
  res <- compute_governing_factors(d, "ph", c("v82", "v87", "v43"),
                                   rf_ntree = 60,
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
  d <- sf::st_drop_geometry(golden_sf("core", localities = golden_locality("core", "smallest", min_n = 30L)))[
    , c("ph", "v82", "v87", "v43")]
  res <- compute_governing_factors(d, "ph", c("v82", "v87", "v43"),
                                   rf_ntree = 60,
                                   shap_sample_size = 25)

  # The sampled rows come from a fixed seed, so the plotted feature values are
  # reproducible and are the sampled rows' own values - not a re-sort, not a
  # different subset.
  idx <- with_seed(12345, sample(seq_len(nrow(d)), 25))
  expect_equal(res$shap$feature_value, d[[res$top_var]][idx])
  expect_equal(nrow(res$shap), 25L)
})

test_that("governing factors accept column names that are not syntactic", {
  # randomForest's formula method re-reads names through make.names(); a
  # header with units or a space ("Total N (%)", "soil moisture") must give
  # the results of its syntactic twin, under its own name.
  d <- make_test_df(40)
  plain <- compute_governing_factors(d, "a", c("b", "c"), rf_ntree = 30, shap_sample_size = 15)
  names(d)[names(d) == "a"] <- "Total N (%)"
  names(d)[names(d) == "b"] <- "soil moisture"
  odd <- compute_governing_factors(d, "Total N (%)", c("soil moisture", "c"),
                                   rf_ntree = 30, shap_sample_size = 15)
  expect_equal(odd$importance$mse_increase, plain$importance$mse_increase)
  expect_equal(odd$importance$variable, c("soil moisture", "c"))
  expect_equal(odd$shap, plain$shap)
  expect_equal(odd$pdp[["_yhat_"]], plain$pdp[["_yhat_"]])
  expect_identical(odd$top_var, c(b = "soil moisture", c = "c")[[plain$top_var]])
})


# ── The Tabular Data Metrics table ─────────────────────────────────────────

test_that("the governing-factors table keeps full precision and names its units", {
  res <- list(
    importance = data.frame(variable = c("som", "clay", "sand"),
                            mse_increase = c(2.001495588850122, 0.5, 1.25),
                            mse_increase_scaled = c(14.2, 3.1, 8.7)),
    model = list(rsq = c(0.10, 0.4213)))
  df <- gov_summary_df(res, NULL)

  expect_equal(names(df), c("Governing Factor / Metric", "Value", "Unit", GOV_SCALED_COL))
  # The scaled column travels with its factor; the model-quality row has none.
  expect_equal(df[[GOV_SCALED_COL]], c(NA, 14.2, 8.7, 3.1))
  # The model-quality row comes first, and each row states its own unit.
  expect_match(df[[1]][1], "OOB variance explained", fixed = TRUE)
  expect_equal(df$Value[1], 42.13)
  expect_equal(df$Unit[1], "% of variance (out-of-bag)")
  expect_true(all(df$Unit[-1] == GOV_IMPORTANCE_UNIT))
  expect_equal(length(unique(df$Unit)), 2)

  # Importances are the values the model reported, in decreasing order, at full
  # precision - the display formats them (they used to print as raw doubles).
  expect_equal(df$Value[-1], sort(res$importance$mse_increase, decreasing = TRUE))
  expect_equal(df[[1]][-1], c("som", "sand", "clay"))
  expect_equal(format_sig(df$Value[2]), "2.001")

  # Without a usable OOB figure the table is the importance rows alone.
  bare <- gov_summary_df(list(importance = res$importance, model = list(rsq = NA_real_)), NULL)
  expect_equal(nrow(bare), 3)
  expect_null(gov_summary_df(NULL))

  # The module keeps only the OOB figure and drops the forest and the DALEX
  # explainer once a run lands, so the table has to read either shape.
  kept <- gov_summary_df(list(importance = res$importance, oob_rsq = 0.4213), NULL)
  expect_equal(kept$Value[1], 42.13)
  expect_equal(kept[[1]], df[[1]])
})
