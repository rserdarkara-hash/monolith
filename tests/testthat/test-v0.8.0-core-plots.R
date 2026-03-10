library(testthat)
library(ggplot2)

# We will source the helpers where these plotting functions will live
tryCatch(source("../../improvements/ui_helpers_0.8.0.R", local = TRUE, chdir = TRUE), error=function(e) NULL)

test_that("Core distribution plots return ggplot objects", {
  df <- data.frame(
    val = rnorm(100),
    group_id = factor(sample(c("A", "B"), 100, replace = TRUE)),
    val_y = rnorm(100) # for scatterplot
  )
  
  # Histogram
  p_hist <- generate_core_plot(df, var_name = "val", group_col = "group_id", plot_type = "histogram")
  expect_s3_class(p_hist, "ggplot")
  
  # Density
  p_dens <- generate_core_plot(df, var_name = "val", group_col = "group_id", plot_type = "density")
  expect_s3_class(p_dens, "ggplot")
  
  # Boxplot
  p_box <- generate_core_plot(df, var_name = "val", group_col = "group_id", plot_type = "boxplot")
  expect_s3_class(p_box, "ggplot")
  
  # Violin
  p_violin <- generate_core_plot(df, var_name = "val", group_col = "group_id", plot_type = "violin")
  expect_s3_class(p_violin, "ggplot")
  
  # Scatterplot
  p_scatter <- generate_core_plot(df, var_name = "val", y_var = "val_y", group_col = "group_id", plot_type = "scatter")
  expect_s3_class(p_scatter, "ggplot")
  
  # ECDF
  p_ecdf <- generate_core_plot(df, var_name = "val", group_col = "group_id", plot_type = "ecdf")
  expect_s3_class(p_ecdf, "ggplot")
})