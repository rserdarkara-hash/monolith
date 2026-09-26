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

test_that("each numeric_* key routes to its own discretiser method", {
  # The discretiser's own numeric contract lives in test-discretization.R.
  # What process_grouping_vars adds is the key -> method mapping, so assert
  # that and nothing else: the routed column must BE the direct call.
  df <- make_test_df(30)
  keys <- c(numeric_median = "median", numeric_mean = "mean",
            numeric_tertiles = "tertiles", numeric_quintiles = "quintiles")
  for (k in names(keys)) {
    got <- process_grouping_vars(df, "a", k)$group_id
    expect_identical(got, discretize_numeric_var(df$a, method = keys[[k]], var_name = "a"),
                     info = k)
  }

  # The four keys are genuinely four different cuts, so a mapping collapsed
  # onto one method cannot pass the loop above unnoticed.
  lvl <- lapply(names(keys), function(k) levels(process_grouping_vars(df, "a", k)$group_id))
  expect_equal(lengths(lvl), c(2L, 2L, 3L, 5L))
  expect_length(unique(lvl), 4L)
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
test_that("collapsed quantile groups preserve missing assignments", {
  x <- c(1, 1, 1, 2, NA_real_)
  for (method in c("tertiles", "quintiles")) {
    out <- discretize_numeric_var(x, method)
    expect_identical(is.na(out), is.na(x))
    expect_equal(length(unique(na.omit(out))), 1L)
    d <- process_grouping_vars(data.frame(x = x, y = 1:5), "x", paste0("numeric_", method))
    selected <- filter_active_groups(d, levels(d$group_id))
    expect_equal(selected$y, 1:4)
    expect_equal(tail(desc_summary_table(selected$y, selected$group_id)$Count, 1), 4)
  }
})
