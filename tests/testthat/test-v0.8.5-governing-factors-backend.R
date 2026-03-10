library(testthat)
library(randomForest)
source("../../improvements/spatial_helpers_0.8.7.R", local = TRUE)

test_that("Governing Factors backend calculations work correctly", {
  # 1. Create dummy data
  set.seed(42)
  df <- data.frame(
    target = rnorm(100),
    pred1 = rnorm(100),
    pred2 = runif(100),
    pred3 = rnorm(100, mean = 5)
  )
  df$target <- df$pred1 * 2 + df$pred2 * 0.5 + rnorm(100, sd = 0.1) # pred1 is most important

  expect_true(exists("compute_governing_factors"), info = "compute_governing_factors function should exist")
  
  res <- compute_governing_factors(df, target_col = "target", predictors = c("pred1", "pred2", "pred3"), n_permutations = 5)
  
  # Check if result is a list with expected components
  expect_type(res, "list")
  expect_true(all(c("importance", "shap", "ale", "model", "top_var") %in% names(res)))
  
  # Check importance structure
  expect_s3_class(res$importance, "data.frame")
  expect_true(all(c("variable", "dropout_loss") %in% colnames(res$importance)))
  
  # Check that pred1 is the most important
  top_var <- res$importance$variable[which.max(res$importance$dropout_loss)]
  expect_equal(as.character(top_var), "pred1")
  expect_equal(as.character(res$top_var), "pred1")
  
  # Check SHAP structure
  expect_s3_class(res$shap, "data.frame")
  expect_true(all(c("feature_value", "contribution") %in% colnames(res$shap)))
  
  # Check ALE structure for the top variable
  expect_s3_class(res$ale, "data.frame")
})