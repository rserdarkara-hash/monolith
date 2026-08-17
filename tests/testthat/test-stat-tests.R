# test-stat-tests.R — tests for get_stat_letters (ANOVA, Tukey HSD, Duncan).

test_that("the None choice draws nothing, including for two groups", {
  # The test control became radioButtons with a "None" = "" choice (2026-08-14).
  # get_stat_letters' if-chain ends in `test_type == "anova" || n_groups == 2`,
  # so without an explicit guard "None" still annotated every two-group plot.
  set.seed(42)
  df <- data.frame(
    value = c(rnorm(10, 10, 2), rnorm(10, 15, 2)),
    group = factor(rep(c("A", "B"), each = 10))
  )
  expect_null(get_stat_letters(df, "value", "group", ""))
  expect_null(get_stat_letters(df, "value", "group", NULL))

  df3 <- data.frame(
    value = c(rnorm(10, 10, 2), rnorm(10, 15, 2), rnorm(10, 20, 2)),
    group = factor(rep(c("A", "B", "C"), each = 10))
  )
  expect_null(get_stat_letters(df3, "value", "group", ""))

  # add_stat_layer must likewise return the plot untouched
  p <- ggplot2::ggplot(df, ggplot2::aes(x = group, y = value)) + ggplot2::geom_boxplot()
  expect_identical(add_stat_layer(p, df, "value", "group", "", "above"), p)
  expect_identical(add_stat_layer(p, df, "value", "group", character(0), "above"), p)
})

test_that("get_stat_letters returns NULL for fewer than 2 groups", {
  df <- data.frame(
    value = rnorm(9),
    group = factor(rep("A", 9))
  )
  result <- get_stat_letters(df, "value", "group", "tukey")
  expect_null(result)
})

test_that("get_stat_letters returns NULL when n < 3", {
  df <- data.frame(
    value = c(1, 2),
    group = factor(c("A", "B"))
  )
  result <- get_stat_letters(df, "value", "group", "anova")
  expect_null(result)
})

test_that("get_stat_letters returns ANOVA results for 2 groups", {
  set.seed(42)
  df <- data.frame(
    value = c(rnorm(10, 10, 2), rnorm(10, 15, 2)),
    group = factor(rep(c("A", "B"), each = 10))
  )
  result <- get_stat_letters(df, "value", "group", "anova")
  expect_true(!is.null(result))
  expect_s3_class(result, "data.frame")
  expect_true("group" %in% colnames(result))
  expect_true("letter" %in% colnames(result))
  if (!is.null(result)) {
    expect_match(result$letter[1], "ANOVA: F")
    expect_match(result$letter[1], "p")
  }
})

test_that("get_stat_letters returns Tukey HSD results for 3+ groups", {
  set.seed(42)
  df <- data.frame(
    value = c(rnorm(10, 10, 2), rnorm(10, 15, 2), rnorm(10, 20, 2)),
    group = factor(rep(c("A", "B", "C"), each = 10))
  )
  result <- get_stat_letters(df, "value", "group", "tukey")
  expect_true(!is.null(result))
  if (!is.null(result)) {
    expect_s3_class(result, "data.frame")
    expect_true("group" %in% colnames(result))
    expect_true("letter" %in% colnames(result))
  }
})

test_that("get_stat_letters returns Duncan results for 3+ groups", {
  set.seed(42)
  df <- data.frame(
    value = c(rnorm(10, 10, 2), rnorm(10, 15, 2), rnorm(10, 20, 2)),
    group = factor(rep(c("A", "B", "C"), each = 10))
  )
  result <- get_stat_letters(df, "value", "group", "duncan")
  expect_true(!is.null(result))
  if (!is.null(result)) {
    expect_s3_class(result, "data.frame")
    expect_true("group" %in% colnames(result))
    expect_true("letter" %in% colnames(result))
  }
})

test_that("get_stat_letters uses ANOVA for 2 groups even when tukey requested", {
  set.seed(42)
  df <- data.frame(
    value = c(rnorm(10, 10, 2), rnorm(10, 12, 2)),
    group = factor(rep(c("A", "B"), each = 10))
  )
  # With only 2 groups, tukey falls back to ANOVA-like label
  result <- get_stat_letters(df, "value", "group", "tukey")
  expect_true(!is.null(result))
})

test_that("get_stat_letters handles errors gracefully", {
  df <- data.frame(
    value = c(1, 2, 3),
    group = factor(c("A", "B", "C"))
  )
  result <- get_stat_letters(df, "value", "group", "tukey")
  # With n < required for tests, it may return NULL or error
  # The function should not throw uncaught errors
  expect_true(is.null(result) || is.data.frame(result))
})

test_that("get_stat_letters handles NA values in variable", {
  df <- data.frame(
    value = c(rnorm(8, 10, 2), NA, NA, rnorm(8, 15, 2)),
    group = factor(rep(c("A", "B"), each = 9))
  )
  result <- get_stat_letters(df, "value", "group", "anova")
  expect_true(!is.null(result))
})

test_that("get_stat_letters handles NA values in group column", {
  df <- data.frame(
    value = rnorm(20),
    group = factor(c(rep("A", 8), NA, rep("B", 10), NA))
  )
  result <- get_stat_letters(df, "value", "group", "anova")
  expect_true(!is.null(result))
})

test_that("get_stat_letters returns NULL when N <= k (zero error df)", {
  # 3 observations across 3 groups means k=3, N=3
  df <- data.frame(
    value = c(10, 15, 20),
    group = factor(c("A", "B", "C"))
  )
  result <- get_stat_letters(df, "value", "group", "tukey")
  expect_null(result)
})
