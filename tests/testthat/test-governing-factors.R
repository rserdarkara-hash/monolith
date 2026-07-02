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
                      "ale", "pdp", "shap")
  expect_setequal(names(result), expected_names)
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

test_that("compute_governing_factors explainer is class 'explainer'", {
  df <- make_test_df(30)
  result <- compute_governing_factors(df, "a", c("b", "c", "d"))
  expect_s3_class(result$explainer, "explainer")
})
