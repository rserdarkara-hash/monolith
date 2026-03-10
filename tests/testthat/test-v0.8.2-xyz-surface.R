library(testthat)
library(ggplot2)

source("../../improvements/ui_helpers_0.8.2.R", local = TRUE, chdir = TRUE)

test_that("XYZ Surface plots generate properly for all fit types without falling back to error text", {
  set.seed(123)
  df <- data.frame(
    v1 = rnorm(50),
    v2 = rnorm(50),
    v3 = rnorm(50)
  )
  
  fits <- c("linear", "loess", "polynomial", "gam", "tps")
  
  for (f in fits) {
    p <- generate_advanced_plot(df, vars = c("v1", "v2", "v3"), plot_type = "xyz_surface", xyz_fit = f)
    
    # We expect a ggplot object
    expect_s3_class(p, "ggplot")
    
    # We expect the plot to actually contain data layers (geom_contour_filled), not just an annotate text 
    # indicating "Model fitting failed"
    has_text_fallback <- any(sapply(p$layers, function(l) inherits(l$geom, "GeomText")))
    
    expect_false(has_text_fallback, info = paste("Fit type", f, "failed and returned the text fallback instead of a surface."))
  }
})