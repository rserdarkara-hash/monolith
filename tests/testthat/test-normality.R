# test-normality.R — tests for compute_normality from desc_exploratory_module.R.

# ── Edge cases: insufficient data ──────────────────────────────────────────

test_that("returns 'insufficient' for n < 3", {
  res <- compute_normality(numeric(0))
  expect_equal(res$status, "insufficient")
  expect_equal(res$n, 0)

  res2 <- compute_normality(c(1, NA))
  expect_equal(res2$status, "insufficient")
  expect_equal(res2$n, 1)
})

test_that("returns 'insufficient' for zero-variance input", {
  res <- compute_normality(c(5, 5, 5, 5, 5))
  expect_equal(res$status, "insufficient")
})

test_that("returns 'insufficient' for non-numeric input", {
  res <- compute_normality(c("a", "b", "c"))
  expect_equal(res$status, "insufficient")
  expect_true(is.null(res$method) || res$method == "None")
})

test_that("returns 'insufficient' for NULL input", {
  res <- compute_normality(NULL)
  expect_equal(res$status, "insufficient")
})

# ── Method dispatch ────────────────────────────────────────────────────────

test_that("uses Lilliefors test for n >= 5000", {
  # The n >= 5000 branch, reported as what nortest actually returned.
  x <- with_seed(11, runif(5000, 0, 100))
  res <- compute_normality(x)
  ref <- nortest::lillie.test(x)
  expect_match(res$method, "Lilliefors")
  expect_equal(res$n, 5000)
  expect_equal(res$statistic, unname(ref$statistic), tolerance = 1e-12)
  expect_equal(res$p_value, ref$p.value, tolerance = 1e-12)
  expect_null(names(res$statistic))
  expect_equal(res$status, if (ref$p.value >= 0.05) "normal" else "not_normal")
})

# ── Classification ─────────────────────────────────────────────────────────

test_that("detects a two-point distribution as non-normal (Shapiro-Wilk)", {
  # 22 zeros and 23 ones: two point masses at n=45 (SW path)
  # shapiro.test() gives p ≈ 2.6e-09 for this vector — unambiguously non-normal
  x <- c(rep(0, 22), rep(1, 23))
  res <- compute_normality(x)
  expect_equal(res$status, "not_normal")
  expect_equal(res$n, 45)
  expect_match(res$method, "Shapiro-Wilk")
})

# ── Robustness ─────────────────────────────────────────────────────────────

test_that("handles NA values by removing them", {
  x <- c(qnorm(ppoints(30)), NA, NA, NA)
  res <- compute_normality(x)
  # n is the count AFTER removal, and the test ran on the 30 finite values.
  expect_equal(res$n, 30)
  expect_equal(res$statistic, unname(shapiro.test(qnorm(ppoints(30)))$statistic),
               tolerance = 1e-12)
})

# ── Output structure ───────────────────────────────────────────────────────

test_that("output list has all expected fields", {
  x <- rnorm(30)
  res <- compute_normality(x)
  expect_setequal(names(res), c("status", "method", "statistic", "p_value", "n"))
})

# ── Normality tooltip on residuals vs raw values ───────────────────────────

test_that("normality tooltip shows (on residuals) only when groups are present", {
  df_single <- data.frame(
    x_val = rnorm(30),
    group_id = factor(rep("All", 30))
  )
  df_multi <- data.frame(
    x_val = rnorm(30),
    group_id = factor(rep(c("A", "B"), each = 15))
  )
  
  # Test with single group (should contain "(on raw values)", should NOT contain "(on residuals)")
  shiny::testServer(desc_exploratory_server, args = list(
    data_reactive = reactive(df_single),
    vars_metadata_reactive = reactive(NULL)
  ), {
    session$setInputs(
      desc_plot_type = "boxplot",
      desc_var_x = "x_val",
      analytics_group_vars = NULL
    )
    
    indicator_html <- output$desc_normality_indicator
    html_str <- indicator_html$html
    expect_false(grepl("(on residuals)", html_str, fixed = TRUE))
    expect_true(grepl("(on raw values)", html_str, fixed = TRUE))
  })
  
  # Test with multi group (should contain "(on residuals)", should NOT contain "(on raw values)")
  shiny::testServer(desc_exploratory_server, args = list(
    data_reactive = reactive(df_multi),
    vars_metadata_reactive = reactive(NULL)
  ), {
    session$setInputs(
      desc_plot_type = "boxplot",
      desc_var_x = "x_val",
      analytics_group_vars = "group_id",
      grp_type_1 = "categorical",
      analytics_active_group = c("A", "B")
    )
    
    indicator_html <- output$desc_normality_indicator
    html_str <- indicator_html$html
    expect_true(grepl("(on residuals)", html_str, fixed = TRUE))
    expect_false(grepl("(on raw values)", html_str, fixed = TRUE))
  })
})

test_that("compute_normality reports the test's own statistic and p-value", {
  x <- golden_soil("core")$ph
  res <- compute_normality(x)
  ref <- shapiro.test(x)

  # No rescaling, no rounding, no partial extraction: what the panel shows is
  # what the test returned, unnamed.
  expect_equal(res$statistic, unname(ref$statistic), tolerance = 1e-12)
  expect_equal(res$p_value, ref$p.value, tolerance = 1e-12)
  expect_null(names(res$statistic))
  expect_equal(res$n, length(x))
  expect_match(res$method, "Shapiro-Wilk")

  # The 0.05 threshold is applied in the documented direction, checked from
  # both sides so a flipped comparison cannot pass.
  expect_equal(res$status, if (ref$p.value >= 0.05) "normal" else "not_normal")
  expect_equal(compute_normality(qnorm(ppoints(60)))$status, "normal")
  expect_equal(compute_normality(exp(qnorm(ppoints(60)) * 2))$status, "not_normal")
})


# ── The verdict a reader sees and copies ───────────────────────────────────
# The verdict used to exist only inside an icon's `title` attribute, so it
# could not be copied into a report or read without hovering. It is text on
# the page now, and it has to be a complete, self-describing sentence.

test_that("the normality verdict is a complete sentence naming test, statistic, p and n", {
  x <- qnorm(ppoints(60))
  res <- compute_normality(x)
  txt <- normality_verdict_text(res, on_residuals = TRUE)

  expect_match(txt, "Shapiro-Wilk", fixed = TRUE)
  expect_match(txt, "within-group residuals", fixed = TRUE)
  expect_match(txt, "W = ", fixed = TRUE)          # the symbol this test reports
  expect_match(txt, "n = 60", fixed = TRUE)
  expect_match(txt, "No significant departure from normality", fixed = TRUE)
  # p comes through the app's own formatter, so it reads like every other p
  expect_match(txt, format_p_value(res$p_value), fixed = TRUE)
  expect_true(endsWith(txt, "."))

  # A clear departure states the opposite conclusion and carries the stars.
  bad <- compute_normality(exp(qnorm(ppoints(60)) * 2))
  txt_bad <- normality_verdict_text(bad, on_residuals = FALSE)
  expect_match(txt_bad, "Significant departure from normality", fixed = TRUE)
  expect_match(txt_bad, "raw values", fixed = TRUE)
  expect_match(txt_bad, signif_stars(bad$p_value), fixed = TRUE)
})

test_that("an untested column says WHY, not 'insufficient data'", {
  # A constant column is not a small sample: blaming n told the reader to
  # collect more of a variable that does not vary.
  const <- compute_normality(rep(3, 40))
  expect_equal(const$reason, "the values are constant")
  expect_match(normality_verdict_text(const), "the values are constant", fixed = TRUE)
  expect_match(normality_verdict_text(const), "n = 40", fixed = TRUE)

  small <- compute_normality(c(1, 2))
  expect_match(normality_verdict_text(small), "fewer than 3", fixed = TRUE)
})
