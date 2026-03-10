library(testthat)
library(ggplot2)

tryCatch(source("../../improvements/ui_helpers_0.8.0.R", local = TRUE, chdir = TRUE), error=function(e) NULL)

test_that("Advanced plots generate ggplot objects", {
  df <- data.frame(
    v1 = rnorm(100),
    v2 = rnorm(100),
    v3 = rnorm(100),
    group_id = factor(sample(c("A", "B"), 100, replace = TRUE))
  )
  
  p_qq <- generate_advanced_plot(df, vars = c("v1"), group_col = "group_id", plot_type = "qq")
  expect_s3_class(p_qq, "ggplot")
  
  p_sina <- generate_advanced_plot(df, vars = c("v1"), group_col = "group_id", plot_type = "sinaplot")
  expect_s3_class(p_sina, "ggplot")
  
  p_ridge <- generate_advanced_plot(df, vars = c("v1"), group_col = "group_id", plot_type = "ridge")
  expect_s3_class(p_ridge, "ggplot")
  
  p_heat <- generate_advanced_plot(df, vars = c("v1", "v2"), group_col = "group_id", plot_type = "density_heatmap")
  expect_s3_class(p_heat, "ggplot")
  
  p_par <- generate_advanced_plot(df, vars = c("v1", "v2", "v3"), group_col = "group_id", plot_type = "parallel")
  expect_s3_class(p_par, "ggplot")
  
  p_rad <- generate_advanced_plot(df, vars = c("v1", "v2", "v3"), group_col = "group_id", plot_type = "radar")
  expect_s3_class(p_rad, "ggplot")
  
  # XYZ Fits
  for(fit in c("linear", "loess", "polynomial", "gam", "tps")) {
    p_xyz <- generate_advanced_plot(df, vars = c("v1", "v2", "v3"), group_col = "group_id", plot_type = "xyz_surface", xyz_fit = fit)
    expect_s3_class(p_xyz, "ggplot")
  }
})