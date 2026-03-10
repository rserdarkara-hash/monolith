library(testthat)

test_that("V2.1: Correlation Rank Module displays variable labels with raw column name fallback", {
  app_content <- readLines("../../app_v2.1.R")
  
  # Check if var_to_label helper is implemented
  has_var_to_label <- any(grepl("var_to_label <- sapply\\(colnames\\(cor_matrix\\), function", app_content))
  expect_true(has_var_to_label, info = "A mapping function from raw column names to labels should exist.")
  
  # Check for fallback logic
  has_fallback <- any(grepl("v # Fallback to column name", app_content))
  expect_true(has_fallback, info = "The mapping function must include fallback to the raw column name.")
  
  # Check if var_to_label is actually used when generating the list items
  used_var_to_label_all <- any(grepl("tags\\$li\\(sprintf\\(\"%s: %\\.3f\", var_to_label\\[\\[v_name\\]\\], res_all\\$Corr\\[i\\]\\)\\)", app_content))
  used_var_to_label_cat <- any(grepl("tags\\$li\\(sprintf\\(\"%s: %\\.3f\", var_to_label\\[\\[v_name\\]\\], res_cat\\$Corr\\[i\\]\\)\\)", app_content))
  
  expect_true(used_var_to_label_all, info = "var_to_label should be used in the 'All' tab of the correlation module.")
  expect_true(used_var_to_label_cat, info = "var_to_label should be used in the category-specific tabs of the correlation module.")
})
