library(testthat)

test_that("Correlation p-values are calculated", {
  app_file <- "../../app_v2.2.R"
  if(!file.exists(app_file)) skip("app_v2.2.R not found")
  
  content <- readLines(app_file)
  has_cor_test <- any(grepl("cor\.test", content))
  
  expect_true(has_cor_test, info = "cor.test should be used to compute p-values for correlations")
})
