library(testthat)

tryCatch(source("../../improvements/ui_helpers_0.8.0.R", local = TRUE, chdir = TRUE), error=function(e) NULL)

test_that("Correlation analysis generates valid plots", {
  set.seed(42)
  df <- data.frame(
    v1 = rnorm(100),
    v2 = rnorm(100),
    v3 = rnorm(100)
  )
  df$v1 <- df$v2 * 0.8 + rnorm(100)*0.2 # create high correlation between v1 and v2
  
  # Heatmap
  p_heat <- generate_correlation_heatmap(df, vars = c("v1", "v2", "v3"), method = "pearson")
  # We check if it returns a plot object (ggplot or otherwise)
  expect_true(!is.null(p_heat), info = "Heatmap function should return a plot object")
  
  # Network
  p_net <- generate_correlation_network(df, vars = c("v1", "v2", "v3"), threshold = 0.1)
  expect_true(!is.null(p_net), info = "Network function should return a plot object")
  
  # Partial Correlation
  p_part <- generate_partial_correlation(df, vars = c("v1", "v2", "v3"))
  expect_true(inherits(p_part, "ggplot") || inherits(p_part, "plotly") || inherits(p_part, "list"))
  
  # Correlogram
  p_corgram <- generate_correlogram(df, vars = c("v1", "v2", "v3"))
  expect_true(inherits(p_corgram, "ggplot") || inherits(p_corgram, "plotly") || inherits(p_corgram, "list"))
  
  # Lagged Correlation (requires temporal or ordered data, we simulate it with index)
  p_lagged <- generate_lagged_correlation(df, var1 = "v1", var2 = "v2", max_lag = 5)
  expect_true(inherits(p_lagged, "ggplot") || inherits(p_lagged, "plotly") || inherits(p_lagged, "list"))
})