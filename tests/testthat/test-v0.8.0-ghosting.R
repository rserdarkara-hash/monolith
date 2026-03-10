library(testthat)
library(ggplot2)

tryCatch(source("../../improvements/ui_helpers_0.8.0.R", local = TRUE, chdir = TRUE), error=function(e) NULL)

test_that("Ghosting logic adds a background layer for global data", {
  df_global <- data.frame(
    val = rnorm(200),
    group_id = factor(sample(c("A", "B"), 200, replace = TRUE)),
    val_y = rnorm(200)
  )
  
  # Simulate a selected locality subset
  df_local <- df_global[1:50, ]
  
  # Histogram
  p_hist <- generate_ghosted_plot(df_global, df_local, var_name = "val", group_col = "group_id", plot_type = "histogram")
  expect_s3_class(p_hist, "ggplot")
  expect_true(length(p_hist$layers) >= 2, info = "Ghosted histogram should have background and foreground layers")
  
  # Density
  p_dens <- generate_ghosted_plot(df_global, df_local, var_name = "val", group_col = "group_id", plot_type = "density")
  expect_true(length(p_dens$layers) >= 2, info = "Ghosted density plot should have background and foreground layers")
  
  # Boxplot
  p_box <- generate_ghosted_plot(df_global, df_local, var_name = "val", group_col = "group_id", plot_type = "boxplot")
  expect_true(length(p_box$layers) >= 2, info = "Ghosted boxplot should have background and foreground layers")
  
  # Scatterplot
  p_scatter <- generate_ghosted_plot(df_global, df_local, var_name = "val", y_var = "val_y", group_col = "group_id", plot_type = "scatter")
  expect_true(length(p_scatter$layers) >= 2, info = "Ghosted scatterplot should have background and foreground layers")
})