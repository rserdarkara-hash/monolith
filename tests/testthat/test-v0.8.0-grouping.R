library(testthat)

# We will source the helpers where this function will live
# For now it will fail because the function is not defined
tryCatch(source("../../improvements/ui_helpers_0.8.0.R", local = TRUE, chdir = TRUE), error=function(e) NULL)

test_that("process_grouping_vars handles categorical and numeric factors", {
  df <- data.frame(
    Locality = c("A", "A", "B", "B", "C", "C"),
    Yield = c(10, 20, 30, 40, 50, 60),
    Target = c(1, 2, 3, 4, 5, 6)
  )
  
  # Group by categorical
  res_cat <- process_grouping_vars(df, vars = c("Locality"), types = c("categorical"))
  expect_equal(res_cat$group_id, as.factor(c("A", "A", "B", "B", "C", "C")))
  
  # Group by numeric (should auto-discretize by median as a default if not specified)
  res_num <- process_grouping_vars(df, vars = c("Yield"), types = c("numeric"))
  expect_true(is.factor(res_num$group_id))
  expect_equal(length(levels(res_num$group_id)), 2) # Below/Above median
  
  # Multi-factor
  res_multi <- process_grouping_vars(df, vars = c("Locality", "Yield"), types = c("categorical", "numeric"))
  expect_equal(length(levels(res_multi$group_id)), 4) # combination of levels with drop=TRUE
})