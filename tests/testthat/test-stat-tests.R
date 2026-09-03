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

test_that("post-hoc letters separate genuinely different groups and join equal ones", {
  # Structural assertions alone cannot tell a correct compact-letter display
  # from a broken one: agricolae's `groups` column, its descending-mean row
  # order and its test choice are all things a version bump can move, and these
  # letters are drawn onto figures the user exports into papers.
  skip_if_not_installed("agricolae")
  set.seed(42)
  sep <- data.frame(
    value = c(rnorm(10, 10, 1), rnorm(10, 15, 1), rnorm(10, 20, 1)),
    group = factor(rep(c("A", "B", "C"), each = 10)))
  for (tt in c("tukey", "duncan", "kruskal")) {
    l <- get_stat_letters(sep, "value", "group", tt)
    # Five sd apart at n = 10: three groups, three distinct letters.
    expect_equal(length(unique(l$letter)), 3L, info = tt)
    # agricolae returns rows in descending-mean order, so the highest-mean
    # group carries "a". The app reads those rownames positionally.
    expect_identical(l$group[l$letter == "a"], "C", info = tt)
  }

  set.seed(7)
  same <- data.frame(value = rnorm(30, 10, 1),
                     group = factor(rep(c("A", "B", "C"), each = 10)))
  for (tt in c("tukey", "duncan", "kruskal")) {
    l <- get_stat_letters(same, "value", "group", tt)
    # One population: every group shares a letter.
    expect_equal(length(unique(l$letter)), 1L, info = tt)
  }
})

test_that("each post-hoc choice routes to its own test, not a neighbour's", {
  # A Duncan result silently produced by HSD.test, or a rank test that quietly
  # ran the parametric one, passes every structural assertion in this file.
  skip_if_not_installed("agricolae")
  letters_of <- function(df, tt) {
    l <- get_stat_letters(df, "value", "group", tt)
    paste(l$group, l$letter, sep = "=", collapse = " ")
  }

  # Duncan controls the comparison-wise error rate only, so it separates means
  # Tukey's family-wise bound cannot. Five evenly spaced groups one sd apart
  # sit squarely in the window where the two disagree.
  set.seed(42)
  liberal <- data.frame(
    value = unlist(lapply(10:14, function(m) rnorm(10, m, 1))),
    group = factor(rep(LETTERS[1:5], each = 10)))
  expect_false(identical(letters_of(liberal, "tukey"),
                         letters_of(liberal, "duncan")))

  # Three cleanly ordered, tight groups, each carrying one extreme outlier.
  # The outliers inflate the pooled MSE enough that neither parametric post-hoc
  # separates anything, while the ranks are still perfectly ordered - so this
  # separates the rank branch from the two parametric ones by construction,
  # not by seed luck (no RNG here at all).
  ranked <- data.frame(
    value = c(1:9, 500, 21:29, 520, 41:49, 540),
    group = factor(rep(c("A", "B", "C"), each = 10)))
  expect_equal(length(unique(get_stat_letters(ranked, "value", "group", "tukey")$letter)), 1L)
  expect_equal(length(unique(get_stat_letters(ranked, "value", "group", "duncan")$letter)), 1L)
  kw <- get_stat_letters(ranked, "value", "group", "kruskal")
  expect_equal(length(unique(kw$letter)), 3L)
  expect_identical(kw$group[kw$letter == "a"], "C")
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
