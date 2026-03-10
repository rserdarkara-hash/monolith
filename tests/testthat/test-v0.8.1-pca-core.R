library(testthat)
library(ggplot2)

tryCatch(source("../../improvements/ui_helpers_0.8.1.R", local = TRUE, chdir = TRUE), error=function(e) NULL)

test_that("PCA core plots return ggplot objects", {
  set.seed(42)
  df <- data.frame(
    v1 = rnorm(100),
    v2 = rnorm(100),
    v3 = rnorm(100)
  )
  
  pca_res <- prcomp(df, scale. = TRUE)
  
  # Scree plot
  p_scree <- generate_pca_scree(pca_res)
  expect_s3_class(p_scree, "ggplot")
  
  # Biplot
  p_biplot <- generate_pca_biplot(pca_res, df, pc_x = 1, pc_y = 2)
  expect_s3_class(p_biplot, "ggplot")
  
  # Loadings
  p_loadings <- generate_pca_loadings(pca_res, pc = 1)
  expect_s3_class(p_loadings, "ggplot")
})