library(testthat)

tryCatch(source("../../improvements/ui_helpers_0.8.2.R", local = TRUE, chdir = TRUE), error=function(e) NULL)

test_that("match_metadata_columns handles large datasets without hanging", {
  # We know the logic hangs not purely in R but because of how many variables it tries to create UI for.
  # Let's ensure the function itself is fast and doesn't return massive combinations
  
  # Create a large user columns list (140 columns)
  user_cols <- paste0("var", 1:140)
  
  # Create a metadata dataframe with ~115 rows (like samp_var_list_2.xlsx)
  m_df <- data.frame(
    actual = paste0("var", 1:115),
    label = paste0("Label ", 1:115),
    category = rep("Cat", 115)
  )
  
  start_time <- Sys.time()
  res <- match_metadata_columns(m_df, user_cols)
  end_time <- Sys.time()
  
  # Execution should be practically instantaneous
  expect_true(as.numeric(end_time - start_time) < 1, info = "Matching 140 columns should take less than 1 second.")
  
  # Also verify it only returns the matched columns (115) and not 140
  expect_equal(length(res), 115)
})