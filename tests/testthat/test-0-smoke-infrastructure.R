# Smoke test — verifies the testthat infrastructure is wired up correctly.
# If this file doesn't run, nothing else will.

test_that("testthat infrastructure loads application functions", {
  # helper.R should have sourced global.R + all helpers + monolith.R
  expect_true(exists("calc_ccc"),              label = "calc_ccc from spatial_helpers.R")
  expect_true(exists("perform_cv"),            label = "perform_cv from spatial_helpers.R")
  expect_true(exists("detect_multicollinearity_engine"), label = "multicollinearity engine")
  expect_true(exists("fuzzy_match_column"),    label = "fuzzy_match_column from ui_helpers.R")
  expect_true(exists("generate_core_plot"),    label = "generate_core_plot from ui_helpers.R")
  expect_true(exists("discretize_numeric_var"),label = "discretize_numeric_var from ui_helpers.R")
  expect_true(exists("compute_normality"),     label = "compute_normality from desc_exploratory_module.R")
  expect_true(exists("monolith_theme_css"),    label = "monolith_theme_css from theme_helpers.R")
  expect_true(exists("compute_governing_factors"), label = "compute_governing_factors from spatial_helpers.R")
  expect_true(exists("get_nut_key"),           label = "get_nut_key from ui_helpers.R")
  expect_true(exists("validate_crs"),          label = "validate_crs from monolith.R")
  expect_true(exists("estimate_run_duration"), label = "estimate_run_duration from monolith.R")
})

test_that("fixture factories produce valid objects", {
  pts <- make_test_points(20)
  expect_s3_class(pts, "sf")
  expect_true(nrow(pts) == 20)
  expect_true(all(c("v", "pv", "aux1", "aux2") %in% colnames(pts)))

  df <- make_test_df(50)
  expect_s3_class(df, "data.frame")
  expect_true(nrow(df) == 50)

  grid <- make_test_grid_safe(pts, res = 50)
  expect_s3_class(grid, "sf")
  expect_true(nrow(grid) > 0)
  expect_true(all(c("x", "y") %in% colnames(grid)))
})

test_that("DESCRIPTION file exists and is parseable", {
  desc_path <- file.path(testthat::test_path(), "..", "..", "DESCRIPTION")
  expect_true(file.exists(desc_path))
  desc <- read.dcf(desc_path)
  expect_true("monolith" %in% desc[, "Package"])
})
