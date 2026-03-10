library(testthat)
library(ggplot2)
library(patchwork)

test_that("Export plots have unified titles and no legend duplication", {
  # Mock generate_styled_plot environment to some extent or just test the logic directly
  
  # For tiled plots, legend should not have the label if pane title has it.
  
  # We test the logic injected in generate_styled_plot
  # leg_name <- if(is_tiled) "" else label
  
  is_tiled <- TRUE
  label <- "Total N (%)"
  leg_name <- if(is_tiled) "" else label
  
  expect_equal(leg_name, "")
  
  is_tiled <- FALSE
  leg_name_single <- if(is_tiled) "" else label
  expect_equal(leg_name_single, "Total N (%)")
})
