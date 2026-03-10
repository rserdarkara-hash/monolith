library(testthat)
library(ggplot2)

tryCatch(source("../../improvements/ui_helpers_0.8.1.R", local = TRUE, chdir = TRUE), error=function(e) NULL)

test_that("Advanced PCA plots return valid objects", {
  set.seed(42)
  df <- data.frame(
    v1 = rnorm(100),
    v2 = rnorm(100),
    v3 = rnorm(100),
    v4 = rnorm(100)
  )
  
  pca_res <- prcomp(df, scale. = TRUE)
  
  # Contribution
  p_contrib <- generate_pca_contribution(pca_res, pc = 1)
  expect_s3_class(p_contrib, "ggplot")
  
  # Variable Importance (cos2)
  p_cos2 <- generate_pca_cos2(pca_res, axes = 1:2)
  expect_s3_class(p_cos2, "ggplot")
  
  # Cumulative Variance
  p_cumvar <- generate_pca_cumvar(pca_res)
  expect_s3_class(p_cumvar, "ggplot")
  
  # Mahalanobis
  p_mahal <- generate_pca_mahalanobis(pca_res)
  expect_s3_class(p_mahal, "ggplot")
  
  # 3D Biplot (returns plotly object or list to be handled by plotly)
  p_3d <- generate_pca_biplot_3d(pca_res, df)
  expect_true(inherits(p_3d, "plotly") || inherits(p_3d, "list"))
})