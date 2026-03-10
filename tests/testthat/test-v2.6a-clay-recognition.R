library(testthat)

source("../../improvements/ui_helpers_v2.6a.R")

test_that("Clay metadata is mapped gracefully despite naming discrepancies", {
  # Mock user columns from data
  user_cols <- c("x", "y", "ph", "clay", "sand", "clay_cve", "clay_ss")
  
  # Mock metadata dataframe
  m_df <- data.frame(
    variable = c("pH", "Clay (%)", "Sand"),
    group = c("Chemical", "Physical", "Physical")
  )
  
  mapped <- match_metadata_columns(m_df, user_cols)
  
  # Check if clay is mapped
  clay_mapped <- Filter(function(v) v$actual == "clay", mapped)
  expect_length(clay_mapped, 1)
  expect_equal(clay_mapped[[1]]$label, "Clay (%)")
  expect_equal(clay_mapped[[1]]$pred, "clay_cve")
})