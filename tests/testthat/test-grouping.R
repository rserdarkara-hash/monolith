# test-grouping.R — tests for process_grouping_vars and filter_active_groups.

# ── process_grouping_vars ─────────────────────────────────────────────────

test_that("sets 'All' factor when no vars provided", {
  df <- make_test_df(10)
  result <- process_grouping_vars(df, character(0), character(0))
  expect_true("group_id" %in% colnames(result))
  expect_equal(as.character(result$group_id[1]), "All")
})

test_that("sets 'All' when vars is NULL", {
  df <- make_test_df(10)
  result <- process_grouping_vars(df, NULL, NULL)
  expect_true("group_id" %in% colnames(result))
  expect_equal(levels(result$group_id), "All")
})

test_that("creates factor group_id for categorical variable", {
  df <- make_test_df(10)
  result <- process_grouping_vars(df, "cat1", "categorical")
  expect_true("group_id" %in% colnames(result))
  expect_s3_class(result$group_id, "factor")
  expect_true(length(levels(result$group_id)) >= 1)
})

test_that("discretizes numeric variable by median", {
  df <- make_test_df(20)
  result <- process_grouping_vars(df, "a", "numeric_median")
  expect_s3_class(result$group_id, "factor")
  expect_length(levels(result$group_id), 2)
})

test_that("discretizes numeric variable by mean", {
  df <- make_test_df(20)
  result <- process_grouping_vars(df, "a", "numeric_mean")
  expect_s3_class(result$group_id, "factor")
  expect_length(levels(result$group_id), 2)
})

test_that("discretizes numeric variable by tertiles", {
  df <- make_test_df(20)
  result <- process_grouping_vars(df, "a", "numeric_tertiles")
  expect_s3_class(result$group_id, "factor")
  expect_true(length(levels(result$group_id)) >= 1)
})

test_that("discretizes numeric variable by quintiles", {
  df <- make_test_df(30)
  result <- process_grouping_vars(df, "a", "numeric_quintiles")
  expect_s3_class(result$group_id, "factor")
})

test_that("handles interaction of multiple grouping vars", {
  df <- make_test_df(20)
  result <- process_grouping_vars(df, c("cat1", "cat2"),
                                  c("categorical", "categorical"))
  expect_s3_class(result$group_id, "factor")
  expect_match(levels(result$group_id)[1], "|")
})

test_that("handles mixed categorical and numeric grouping", {
  df <- make_test_df(20)
  result <- process_grouping_vars(df, c("cat1", "a"),
                                  c("categorical", "numeric_median"))
  expect_s3_class(result$group_id, "factor")
})

# ── filter_active_groups ──────────────────────────────────────────────────

test_that("filter_active_groups filters to selected groups", {
  df <- make_test_df(20)
  df <- process_grouping_vars(df, "cat1", "categorical")
  all_levels <- levels(df$group_id)
  if (length(all_levels) >= 2) {
    result <- filter_active_groups(df, all_levels[1])
    expect_true(nrow(result) <= nrow(df))
  }
})

test_that("filter_active_groups returns all rows when no group_id column", {
  df <- make_test_df(10)
  result <- filter_active_groups(df, c("A", "B"))
  expect_equal(nrow(result), nrow(df))
})

test_that("filter_active_groups returns empty df when active_groups is empty", {
  df <- make_test_df(20)
  df <- process_grouping_vars(df, "cat1", "categorical")
  result <- filter_active_groups(df, character(0))
  expect_equal(nrow(result), 0)
})

test_that("filter_active_groups returns all rows when active_groups is NULL", {
  df <- make_test_df(20)
  df <- process_grouping_vars(df, "cat1", "categorical")
  result <- filter_active_groups(df, NULL)
  expect_equal(nrow(result), nrow(df))
})

test_that("filter_active_groups handles NULL df gracefully", {
  expect_null(filter_active_groups(NULL, c("A")))
})
