# test-multicollinearity.R: tests for detect_multicollinearity_engine and
# check_vif.

# ── detect_multicollinearity_engine ────────────────────────────────────────

test_that("returns empty dropped for uncorrelated variables", {
  df <- make_test_df(30)
  res <- detect_multicollinearity_engine(df, vars = c("a", "b", "c", "d", "e"))
  # With random independent normals, no variable should be dropped
  expect_false(res$has_collinearity)
  expect_null(res$pairs)
  expect_equal(length(res$dropped), 0)
  expect_setequal(res$kept, c("a", "b", "c", "d", "e"))
})

test_that("detects near-perfect pairwise correlation", {
  df <- make_collinear_df(30)
  res <- detect_multicollinearity_engine(df, vars = c("v1", "v2", "v3", "v4"))
  # v1 and v2 are nearly identical
  expect_true(res$has_collinearity)
  expect_true(!is.null(res$pairs))
  expect_true(nrow(res$pairs) >= 1)
})

test_that("drops variable with VIF > threshold", {
  df <- make_collinear_df(30)
  res <- detect_multicollinearity_engine(df, vars = c("v1", "v2", "v3", "v4"),
                                         vif_threshold = 5)
  # v1 or v2 should be dropped due to VIF
  expect_true(length(res$dropped) >= 1)
  # kept should contain v3 and v4 (the independent ones)
  expect_true("v3" %in% res$kept)
  expect_true("v4" %in% res$kept)
})

test_that("handles fewer than 2 variables gracefully", {
  df <- make_test_df(10)
  res <- detect_multicollinearity_engine(df, vars = "a")
  expect_false(res$has_collinearity)
  expect_equal(length(res$kept), 1)
  expect_equal(res$kept, "a")
})

test_that("handles all-NA columns", {
  df <- make_test_df(10)
  df$z <- NA_real_
  res <- detect_multicollinearity_engine(df, vars = c("a", "z"))
  # An all-NA column has no variance to screen.
  expect_true("a" %in% res$kept)
})

test_that("zero-variance column is pruned instead of crashing the VIF path", {
  df <- make_test_df(10)
  df$const <- 5
  # A constant column would put NA rows in the correlation matrix and break the
  # solve() fallback; it is dropped before the iterative loop.
  res <- suppressWarnings(
    detect_multicollinearity_engine(df, vars = c("a", "b", "const"))
  )
  expect_true("const" %in% res$dropped)
  expect_true(all(c("a", "b") %in% res$kept))
})

test_that("small-unit covariates survive the constant check (scale-free)", {
  # Constancy is judged relative to the column's own magnitude, not by an
  # absolute variance floor: fractions, ratios, normalized indices and anything
  # in km or Mg have variances below 1e-6, and constants are dropped even under
  # Keep All, so the user could not rescue them.
  set.seed(42)
  df <- data.frame(
    ph = rnorm(30, 6.5, 0.4),
    om = rnorm(30, 2.0, 0.5),
    clay_frac = rnorm(30, 0.25, 5e-4)   # 0-1 fraction: var ~ 1.5e-7
  )
  expect_lt(var(df$clay_frac), 1e-6)    # below any absolute 1e-6 floor

  res <- suppressWarnings(
    detect_multicollinearity_engine(df, vars = c("ph", "om", "clay_frac"))
  )
  expect_false("clay_frac" %in% res$dropped)
  expect_true("clay_frac" %in% res$kept)
})

test_that("numerically constant columns are still pruned regardless of magnitude", {
  set.seed(7)
  df <- data.frame(
    a = rnorm(20, 5, 1),
    b = rnorm(20, 3, 1),
    # varies only in the ~13th significant digit of its own magnitude: this is
    # the case cor()/solve() genuinely cannot handle
    noise_only = 7.5 + rnorm(20, 0, 1e-13),
    all_zero = 0
  )
  res <- suppressWarnings(
    detect_multicollinearity_engine(df, vars = c("a", "b", "noise_only", "all_zero"))
  )
  expect_true(all(c("noise_only", "all_zero") %in% res$dropped))
  expect_true(all(c("a", "b") %in% res$kept))
})

test_that("relative degeneracy remains meaningful at extreme finite unit scales", {
  for (unit in c(1e-200, 1e200)) {
    expect_false(.is_degenerate_covariate((1:3) * unit))
    expect_false(.is_degenerate_covariate(c(-1, 1) * unit, scale = unit))
    # The relative sample sd is sqrt(2) / 1e10, below the unchanged 1e-8
    # floor when these are residuals judged at the target's larger scale.
    expect_true(.is_degenerate_covariate(c(-1, 1) * unit, scale = unit * 1e10))
    expect_true(.is_degenerate_covariate((1 + c(-1, 0, 1) * 1e-10) * unit))
  }
})

test_that("extreme and mixed units preserve pair correlations and VIF arithmetic", {
  plain <- data.frame(a = c(-3, -2, -1, 0, 1, 2, 3, 4),
                      b = c(-2, -1, 1, 0, 2, 1, 4, 3))
  expected_r <- stats::cor(plain$a, plain$b)
  expected_vif <- 1 / (1 - summary(stats::lm(a ~ b, data = plain))$r.squared)
  # At 1e-160, cor() can return a finite but inaccurate result because its
  # covariance is subnormal; checking only for an NA/Inf sd cannot detect it.
  scales <- list(c(1e-200, 1e-200), c(1e200, 1e200),
                 c(1e200, 1e-200), c(1e-200, 1e200),
                 c(1e-160, 1e-160), c(1e300, 1e-300))
  for (units in scales) {
    d <- data.frame(a = plain$a * units[1], b = plain$b * units[2])
    out <- detect_multicollinearity_engine(d, diagnostics = TRUE)
    step <- if (length(out$diagnostics$steps)) out$diagnostics$steps[[1]] else NULL
    expect_length(out$dropped, 0L)
    expect_setequal(out$kept, names(plain))
    expect_identical(out$diagnostics$status, "checked")
    expect_equal(out$diagnostics$pairs$r, expected_r, tolerance = 1e-12)
    expect_equal(out$diagnostics$pairs$n, nrow(plain))
    expect_equal(unname(step$vif), rep(expected_vif, 2), tolerance = 1e-10)
  }
})

test_that("extreme-unit exact pairs remain singular and are not labelled constant", {
  for (units in list(c(1e200, 1e-200), c(1e-200, 1e200))) {
    for (sign in c(-1, 1)) {
      x <- c(-1, 1, -1, 1)
      d <- data.frame(a = x * units[1], b = sign * x * units[2])
      out <- suppressWarnings(detect_multicollinearity_engine(d, diagnostics = TRUE))
      step <- if (length(out$diagnostics$steps)) out$diagnostics$steps[[1]] else NULL
      expect_length(out$dropped_constant, 0L)
      expect_length(out$dropped_vif, 1L)
      expect_equal(out$diagnostics$pairs$r, sign, tolerance = 1e-12)
      expect_true(out$diagnostics$pairs$near_duplicate)
      expect_identical(step$status, "singular")
      expect_null(step$vif)
    }
  }
})

test_that("extreme-unit checks use the finite shared population for each calculation", {
  plain <- data.frame(a = 1:4, b = c(4, 1, 3, 2))
  expected_r <- stats::cor(plain$a, plain$b)
  expected_vif <- 1 / (1 - summary(stats::lm(a ~ b, data = plain))$r.squared)
  # The outlier is absent from b's population. Scaling a by its global
  # maximum would erase all shared finite a values through underflow.
  cases <- list(
    data.frame(a = c(1e300, plain$a * 1e-200), b = c(NA_real_, plain$b * 1e-200)),
    # Both global standard deviations are ordinary here; only the shared
    # finite population has a covariance vulnerable to underflow.
    data.frame(a = c(1, plain$a * 1e-200, NA_real_), b = c(NA_real_, plain$b * 1e-200, 1)))
  for (d in cases) {
    out <- detect_multicollinearity_engine(d, diagnostics = TRUE)
    step <- if (length(out$diagnostics$steps)) out$diagnostics$steps[[1]] else NULL
    expect_length(out$dropped, 0L)
    expect_equal(out$diagnostics$n_complete, nrow(plain))
    expect_equal(out$diagnostics$pairs$n, nrow(plain))
    expect_equal(out$diagnostics$pairs$r, expected_r, tolerance = 1e-12)
    expect_equal(unname(step$vif), rep(expected_vif, 2), tolerance = 1e-10)
  }

  d <- cases[[1]]
  d$a[-1] <- 1e-200
  overlap_constant <- suppressWarnings(detect_multicollinearity_engine(d, diagnostics = TRUE))
  expect_identical(overlap_constant$dropped_constant, "a")
  expect_identical(overlap_constant$diagnostics$pairs$status, "constant")
  expect_true(is.na(overlap_constant$diagnostics$pairs$r))

  sparse <- data.frame(a = c(1e200, 2e200, NA_real_, NA_real_),
                       b = c(NA_real_, 1e-200, 2e-200, 3e-200))
  incomplete <- detect_multicollinearity_engine(sparse, diagnostics = TRUE)
  expect_identical(incomplete$diagnostics$status, "insufficient_rows")
  expect_identical(incomplete$diagnostics$pairs$status, "insufficient_rows")
  expect_equal(incomplete$diagnostics$pairs$n, 1L)
  expect_true(is.na(incomplete$diagnostics$pairs$r))
})

test_that("infinite vif_threshold (user's Keep All choice) never drops collinear vars", {
  df <- data.frame(
    a = 1:20,
    b = 2 * (1:20) + rnorm(20, 0, 1e-4),  # near-perfectly collinear with a
    c = rnorm(20, 5, 1)
  )
  res <- suppressWarnings(
    detect_multicollinearity_engine(df, vars = c("a", "b", "c"),
                                    vif_threshold = Inf)
  )
  expect_length(res$dropped, 0)
  expect_setequal(res$kept, c("a", "b", "c"))
  # collinearity is still REPORTED (pairs), just not acted upon
  expect_true(res$has_collinearity)
})

test_that("auto-detects numeric columns when vars = NULL", {
  df <- make_test_df(20)
  res <- detect_multicollinearity_engine(df, vars = NULL)
  expect_true(length(res$kept) >= 1)
  # categorical columns should not appear in kept
  expect_false("cat1" %in% res$kept)
  expect_false("cat2" %in% res$kept)
})

test_that("the singular-matrix fallback drops the globally redundant member, whatever the column order", {
  # `ab` is an exact linear combination of `a` and `b`, so the correlation
  # matrix is singular and solve() throws -> the fallback picks the drop. `b`
  # carries four times the variance of `a`, so the maximally correlated pair is
  # (b, ab); of those two, `ab` is the more globally redundant one (it
  # correlates with BOTH other covariates, `b` only with `ab`). The drop must
  # not depend on column order, or the fitted model would depend on the
  # upload's layout.
  set.seed(20260811)
  n <- 40
  a <- rnorm(n, 0, 1)
  b <- rnorm(n, 0, 4)
  df <- data.frame(a = a, b = b, ab = a + b)

  res <- detect_multicollinearity_engine(df, vars = c("a", "b", "ab"),
                                         vif_threshold = 10)
  expect_true("ab" %in% res$dropped)
  expect_setequal(res$kept, c("a", "b"))

  # Same data, reversed column order: identical outcome.
  res_rev <- detect_multicollinearity_engine(df[, c("ab", "b", "a")],
                                             vars = c("ab", "b", "a"),
                                             vif_threshold = 10)
  expect_setequal(res_rev$kept, res$kept)
  expect_setequal(res_rev$dropped, res$dropped)
})

test_that("pairwise_threshold parameter is respected", {
  df <- make_test_df(30)
  # Both ends, or the parameter is only half asserted: at 0.0 every pair
  # clears the bar and is reported, at 1.0 none does.
  res_low <- detect_multicollinearity_engine(df, vars = c("a", "b", "c"),
                                             pairwise_threshold = 0.0)
  expect_true(res_low$has_collinearity)
  expect_gt(nrow(res_low$pairs), 0)
  expect_equal(nrow(res_low$pairs), choose(3, 2))

  res_high <- detect_multicollinearity_engine(df, vars = c("a", "b", "c"),
                                              pairwise_threshold = 1.0)
  expect_false(res_high$has_collinearity)
  expect_null(res_high$pairs)
})

# ── check_vif ──────────────────────────────────────────────────────────────

test_that("check_vif forwards the engine's four fields unchanged", {
  # The compact check_vif result forwards the engine's four screen fields.
  # Assert equality with the engine rather than re-testing the gate through it.
  for (thr in c(5, 10, Inf)) {
    df <- make_collinear_df(30)
    got <- check_vif(df, threshold = thr)
    ref <- detect_multicollinearity_engine(df, vif_threshold = thr)
    expect_named(got, c("kept", "dropped", "dropped_constant", "dropped_vif"))
    for (f in names(got)) expect_identical(got[[f]], ref[[f]], info = paste(thr, f))
  }

  # And the fields carry something at the default threshold, so an accidental
  # all-empty forward cannot pass the equality above by agreeing on nothing.
  res <- check_vif(make_collinear_df(30), threshold = 10)
  expect_gte(length(res$dropped_vif), 1L)
  expect_true(all(c("v3", "v4") %in% res$kept))
})

# ── Numeric contract: the VIF itself ───────────────────────────────────────
#
# Everything above pins which column the gate drops. These pin the arithmetic
# behind that decision: the engine computes VIF as diag(solve(cor(X))), and the
# reference recomputes it the long way, as 1/(1 - R2_j) from an actual
# regression of each covariate on the others.

test_that("the VIF gate drops exactly what an lm-based VIF says it should", {
  cov <- golden_meta()$columns$covariates
  X <- golden_soil("full")[cov]

  # Independent reference: the same iterative rule (drop the worst, refit,
  # repeat) with every VIF obtained from a regression rather than from a
  # matrix inverse.
  ref_kept <- cov
  ref_dropped <- character(0)
  repeat {
    if (length(ref_kept) < 2) break
    vif <- vapply(ref_kept, function(v) {
      r2 <- summary(lm(reformulate(setdiff(ref_kept, v), v), data = X))$r.squared
      1 / (1 - r2)
    }, numeric(1))
    if (max(vif) <= 10) break
    ref_dropped <- c(ref_dropped, ref_kept[which.max(vif)])
    ref_kept <- setdiff(ref_kept, ref_dropped[length(ref_dropped)])
  }

  res <- detect_multicollinearity_engine(X, vif_threshold = 10)
  # Same covariates, dropped in the same order: the engine's VIF ranking is the
  # regression VIF ranking at every step, not merely at the first one.
  expect_equal(res$dropped_vif, ref_dropped)
  expect_setequal(res$kept, ref_kept)
  # Recorded for this golden set, so a change of gate rule is visible even if
  # the reference above is weakened. On the shipped survey: annual mean
  # temperature, slope, then warmest-quarter temperature.
  recorded <- golden_baseline("vif_drop_order")
  if (!is.null(recorded)) {
    expect_equal(ref_dropped, recorded, info = golden_baseline_info())
  }
})

test_that("an orthogonal design has VIF exactly 1 and survives the gate", {
  # A full 2^3 factorial in +-1 has exactly zero sample correlation, so its
  # correlation matrix is the identity and every VIF is exactly 1 - not 1.0008,
  # which is what a random "uncorrelated" design actually gives.
  O <- data.frame(
    a = c(-1, -1, -1, -1, 1, 1, 1, 1),
    b = c(-1, -1,  1,  1, -1, -1, 1, 1),
    c = c(-1,  1, -1,  1, -1,  1, -1, 1)
  )
  expect_equal(unname(diag(solve(cor(O)))), c(1, 1, 1), tolerance = 1e-12)

  res <- detect_multicollinearity_engine(O, vif_threshold = 10)
  expect_length(res$dropped, 0)
  expect_false(res$has_collinearity)
  expect_setequal(res$kept, c("a", "b", "c"))
})

test_that("the VIF gate is invariant to rescaling and to column order", {
  cov <- golden_meta()$columns$covariates
  X <- golden_soil("full")[cov]
  base <- detect_multicollinearity_engine(X, vif_threshold = 10)

  # VIF is a correlation-matrix quantity, so an affine change of units cannot
  # move it.
  Xs <- X
  Xs$v82 <- Xs$v82 * 1000 + 5e5     # metres -> millimetres, plus an offset
  Xs$v12 <- Xs$v12 / 25.4
  expect_setequal(detect_multicollinearity_engine(Xs, vif_threshold = 10)$kept,
                  base$kept)

  # And the outcome must be a property of the data, not of the file layout.
  expect_setequal(detect_multicollinearity_engine(X[rev(cov)],
                                                  vif_threshold = 10)$kept,
                  base$kept)
})

test_that("an infinite threshold keeps every covariate however collinear", {
  # Every covariate in a pair this golden set correlates above 0.99.
  all_cov <- golden_meta()$columns$covariates
  cm <- cor(golden_soil("full")[all_cov]); diag(cm) <- 0
  hits <- which(abs(cm) > 0.99, arr.ind = TRUE)
  cov <- unique(all_cov[as.vector(hits)])
  X <- golden_soil("full")[cov]
  expect_gte(length(cov), 2L)
  res <- detect_multicollinearity_engine(X, vif_threshold = Inf)
  expect_length(res$dropped, 0)
  expect_setequal(res$kept, cov)
  # The pairwise report still fires: "keep all" is a modelling choice, not a
  # reason to stop telling the user the covariates are collinear.
  expect_true(res$has_collinearity)
})
test_that("pairs without a finite correlation are skipped, not read as uncorrelated or fatal", {
  # a and b share 0 or 1 rows, so no correlation exists for them; each tracks
  # c exactly on its own rows, and those two pairs must still be screened.
  for (overlap in 0:1) {
    d <- data.frame(a = c(1:10, rep(NA, 10)), b = c(rep(NA, 10 - overlap), seq_len(10 + overlap)), c = 1:20)
    out <- detect_multicollinearity_engine(d, vif_threshold = Inf)
    expect_false(any(out$pairs$var1 == "a" & out$pairs$var2 == "b"))
    expect_setequal(paste(out$pairs$var1, out$pairs$var2), c("a c", "b c"))
    expect_setequal(out$kept, c("a", "b", "c"))
  }
  # a varies overall but is constant on the rows complete across all three,
  # which the VIF stage inverts: it is dropped as constant there.
  d <- data.frame(a = c(1, 2, 3, 3, 3), b = c(NA, NA, 1, 2, 3), c = 1:5)
  expect_warning(out <- detect_multicollinearity_engine(d), "only one covariate remains")
  expect_true("a" %in% out$dropped_constant)
})

# ── The collinearity dialog: each locality screened as the run screens it ──
test_that("optional diagnostics distinguish pair evidence from multivariate VIF", {
  o <- expand.grid(a = c(-1, 1), b = c(-1, 1), e = c(-1, 1))
  d <- transform(o, c = a + b + 0.1 * e)[c("a", "b", "c")]
  out <- detect_multicollinearity_engine(d, diagnostics = TRUE)
  expect_false(out$has_collinearity)
  expect_identical(out$dropped_vif, "c")
  step <- out$diagnostics$steps[[1]]
  expect_equal(unname(step$vif[c("a", "b", "c")]), c(101, 101, 201), tolerance = 1e-9)
  expect_identical(step$candidates, c("a", "b", "c"))
  expect_identical(step$drop, "c")
  expect_identical(out$diagnostics$status, "checked")
  expect_equal(out$diagnostics$n_complete, nrow(d))

  pair <- data.frame(a = o$a, b = 0.93 * o$a + sqrt(1 - 0.93^2) * o$b)
  rf <- suppressWarnings(detect_multicollinearity_engine(pair, vif_threshold = 5, diagnostics = TRUE))
  expect_false(rf$has_collinearity)
  expect_length(rf$dropped_vif, 1L)
  expect_equal(rf$diagnostics$pairs$r, 0.93, tolerance = 1e-12)
  expect_equal(rf$diagnostics$pairs$n, nrow(pair))
  expect_false(rf$diagnostics$pairs$near_duplicate)
  expect_equal(unname(rf$diagnostics$steps[[1]]$vif), rep(1 / (1 - 0.93^2), 2), tolerance = 1e-10)
})

test_that("VIF explanations follow the candidates at each removal step", {
  # Three orthogonal, centered vectors with equal variance give an exact
  # r(a, b) = .93. The noisy sum is removed first, leaving a genuine pair
  # whose VIF is independently 1 / (1 - .93^2), above the RF threshold.
  a <- rep(c(-1, 1), 8)
  orthogonal <- rep(c(-1, -1, 1, 1), 4)
  noise <- rep(c(-1, -1, -1, -1, 1, 1, 1, 1), 2)
  b <- .93 * a + sqrt(1 - .93^2) * orthogonal
  d <- data.frame(a = a, b = b, c = a + b + .1 * noise)
  s <- suppressWarnings(screen_covariates(d, names(d), vif_threshold = 5, diagnostics = TRUE))
  steps <- s$diagnostics$steps
  expect_identical(steps[[1]]$drop, "c")
  expect_setequal(steps[[2]]$candidates, c("a", "b"))
  expect_equal(unname(steps[[2]]$vif), rep(1 / (1 - .93^2), 2), tolerance = 1e-10)

  # c = 1.93 a + sqrt(1 - .93^2) o + .1 n on orthogonal unit-variance vectors.
  # {a, b} spans {a, o}, so the other two leave c a residual variance of .01
  # out of 3.87: VIF 387. Removing c leaves the pair, then one covariate.
  prof <- collinearity_profile(s)
  expect_identical(vapply(prof$removals, `[[`, character(1), "var"), s$dropped_vif)
  expect_identical(prof$removals[[1]]$var, "c")
  expect_equal(prof$removals[[1]]$at, 387, tolerance = 1e-9)
  expect_equal(prof$removals[[2]]$at, 1 / (1 - .93^2), tolerance = 1e-10)
  pair_drop <- steps[[2]]$drop
  partner <- setdiff(steps[[2]]$candidates, pair_drop)
  left <- prof$rows[prof$rows$var == partner, ]
  expect_true(left$sole)
  expect_equal(left$final, 1)
  expect_identical(prof$rows$var[1], "c")
  html <- as.character(collinearity_card("k", "Scope", "meta", s, NULL, "pick", names(d)))
  expect_match(html, "VIF 387: the other 2 together explain 99.7% of its variation", fixed = TRUE)
  expect_match(html, paste0("VIF 7.40 comes from its correlation with ", partner, " alone"), fixed = TRUE)
  expect_match(html, "only one left", fixed = TRUE)

  # A constant is another initial candidate, but it is absent from the VIF
  # design and must not turn this pair's explanation into a joint finding.
  with_constant <- data.frame(a = a, b = b, flat = 1)
  pair_screen <- suppressWarnings(screen_covariates(with_constant, names(with_constant),
    vif_threshold = 5, diagnostics = TRUE))
  pair_html <- as.character(collinearity_card("k", "Scope", "meta", pair_screen, NULL, "pick",
                                              setdiff(names(with_constant), pair_screen$dropped_constant)))
  expect_match(pair_html, "comes from its correlation with", fixed = TRUE)
  expect_false(grepl("together explain", pair_html, fixed = TRUE))
  # The constant is a chip, not a row; offered for removal (a constant on one
  # surface only) it is a row that states why it has no VIF.
  expect_match(pair_html, "Constant here, removed whatever you answer:", fixed = TRUE)
  expect_false(grepl('value="flat"', pair_html, fixed = TRUE))
  offered <- as.character(collinearity_card("k", "Scope", "meta", pair_screen, NULL, "pick", names(with_constant)))
  expect_match(offered, 'value="flat"/><span><span class="mn-cl-row" title="flat: not in the VIF check: constant here.">',
               fixed = TRUE)
})

# ── The covariate check card: every number read from the screen ────────────
test_that("covariate check values print as the check reads them", {
  expect_identical(format_vif(c(416.37, 161.28, 11.91, 7.25, 1234.6, NA, Inf)),
                   c("416", "161", "11.91", "7.25", "1,235", "", ""))
  expect_identical(format_vif(416.37, exact = TRUE), "416.37")
  # A removed value never prints as the threshold it exceeded.
  expect_identical(format_vif(c(10.004, 10.00001, 11.91), above = 10), c("10.004", "10.00001", "11.91"))
  expect_identical(vif_share_text(c(387, 1 / (1 - .93^2), 10, 1, 1000, 5000)),
                   c("99.7%", "86.5%", "90.0%", "0.0%", "99.9%", "more than 99.9%"))
  expect_identical(share_text(.93^2), "86.5%")
  expect_equal(.vif_axis_pos(c(10, 5, 1, 1000, 5000, .5)),
               c(100 / 3, 100 * log10(5) / 3, 0, 100, 100, 0))
  # The checkbox rows are Shiny's own checkbox group, written in one pass.
  rows <- c('<span class="mn-cl-row">A &amp; B</span>', '<span>B</span>')
  ref <- gsub(">\\s+<", "><", as.character(shiny::checkboxGroupInput("vif_remove_2", NULL,
    choiceValues = c("a&b", "b"), choiceNames = lapply(rows, shiny::HTML))))
  expect_identical(.cl_checkbox_group("vif_remove_2", c("a&b", "b"), rows), ref)
})

test_that("each removal is explained at its own step, with the same-rows bound", {
  a <- rep(c(-1, 1), 8)
  o <- rep(c(-1, -1, 1, 1), 4)
  noise <- rep(c(-1, -1, -1, -1, 1, 1, 1, 1), 2)
  b <- .93 * a + sqrt(1 - .93^2) * o
  d <- data.frame(a = a, b = b, c = a + b + .1 * noise)
  s <- suppressWarnings(screen_covariates(d, names(d), vif_threshold = 5, diagnostics = TRUE))
  prof <- collinearity_profile(s)
  # On complete rows the strongest partner's r^2 bounds R^2 = 1 - 1/VIF from
  # below; for a pair it is R^2 itself.
  expect_equal(prof$removals[[1]]$bound, stats::cor(d$c, d$a)^2, tolerance = 1e-12)
  expect_equal(prof$removals[[1]]$bound, 1.93^2 / 3.87, tolerance = 1e-12)
  for (rm in prof$removals) expect_lte(rm$bound, 1 - 1 / rm$at + 1e-12)
  expect_equal(prof$removals[[2]]$bound, .93^2, tolerance = 1e-12)
  # The next highest VIF at each step, flagged when it is above the threshold too.
  step1 <- s$diagnostics$steps[[1]]
  expect_equal(unname(prof$removals[[1]]$runner), max(step1$vif[c("a", "b")]))
  html <- as.character(collinearity_card("k", "Scope", "meta", s, NULL, "pick", names(d)))
  expect_match(html, "its strongest partner alone, [ab], explains 96.3%")
  expect_match(html, "(also above 5; rechecked after this removal)", fixed = TRUE)

  # A partner measured on more rows than the step's complete rows bounds
  # nothing: no clause, and its bar names its own n.
  d$m <- c(NA, rep(c(1, 4, 2, 3), 4)[-1])
  miss <- suppressWarnings(screen_covariates(d, names(d), vif_threshold = 5, diagnostics = TRUE))
  mp <- collinearity_profile(miss)
  expect_identical(mp$removals[[1]]$var, "c")
  expect_identical(mp$removals[[1]]$n_complete, 15L)
  expect_null(mp$removals[[1]]$bound)
  mh <- as.character(collinearity_card("k", "Scope", "meta", miss, NULL, "pick", names(d)))
  expect_false(grepl("strongest partner alone", strsplit(mh, "mn-cl-step\">2<")[[1]][1], fixed = TRUE))
  expect_match(mh, "<small>n=16</small>", fixed = TRUE)
  # Missing and non-finite values are counted per covariate when rows are lost.
  d$m[2] <- Inf
  inf <- suppressWarnings(screen_covariates(d, names(d), vif_threshold = 5, diagnostics = TRUE))
  ip <- collinearity_profile(inf)
  expect_identical(unname(ip$missing[c("a", "m")]), c(0L, 2L))
  ih <- as.character(collinearity_card("k", "Scope", "meta", inf, NULL, "pick", names(d)))
  expect_identical(lengths(regmatches(ih, gregexpr("&#183; 2 missing</span>", ih, fixed = TRUE))), 1L)
  expect_false(grepl("&#183; 0 missing", ih, fixed = TRUE))
  # A runner-up at or below the threshold carries no flag.
  quiet <- list(dropped_vif = "x", kept = c("y", "z"), dropped_constant = character(0), diagnostics = list(
    status = "checked", n_rows = 10L, n_complete = 10L, vif_threshold = 10, steps = list(
      list(candidates = c("x", "y", "z"), n_complete = 10L, status = "estimated", vif = c(x = 12, y = 4, z = 3), drop = "x"),
      list(candidates = c("y", "z"), n_complete = 10L, status = "estimated", vif = c(y = 1.2, z = 1.2), drop = character(0)))))
  qh <- as.character(collinearity_card("k", "Scope", "meta", quiet, NULL, "pick", c("x", "y", "z")))
  expect_match(qh, "Next highest at this step: y, 4.00.", fixed = TRUE)
  expect_false(grepl("also above", qh, fixed = TRUE))
})

test_that("a singular step has no VIF, and the chart says so", {
  d <- data.frame(a = 1:8, b = -(1:8), c = c(2, 1, 4, 3, 6, 5, 8, 7))
  s <- suppressWarnings(screen_covariates(d, names(d), diagnostics = TRUE))
  prof <- collinearity_profile(s)
  expect_true(prof$first_singular)
  expect_true(all(is.na(prof$rows$first)))
  expect_identical(prof$removals[[1]]$status, "singular")
  expect_true(is.infinite(prof$removals[[1]]$at))
  expect_identical(prof$rows$var[1], prof$removals[[1]]$var)
  left <- setdiff(names(d), prof$removals[[1]]$var)
  expect_equal(prof$rows$final[match(left, prof$rows$var)],
               rep(1 / (1 - stats::cor(d[[left[1]]], d[[left[2]]])^2), 2), tolerance = 1e-10)
  html <- as.character(collinearity_card("k", "Scope", "meta", s, NULL, "pick", names(d)))
  expect_match(html, '<span class="mn-cl-step">1</span><b>singular</b>', fixed = TRUE)
  expect_false(grepl("mn-cl-dot first", html, fixed = TRUE))
  expect_match(html, paste0("The 3 covariates left at this step are linearly dependent, so no VIF is defined; ",
                            "Auto-Drop removes the more redundant member of the most correlated pair."), fixed = TRUE)
  expect_match(html, "Its strongest correlations with the other 2", fixed = TRUE)
  expect_match(html, "the first step has no VIFs to plot", fixed = TRUE)
})

test_that("a clean verdict says nothing is removed only when nothing is", {
  # Three orthogonal centred columns: every VIF is exactly 1.
  o <- data.frame(a = rep(c(-1, 1), 4), b = rep(c(-1, -1, 1, 1), 2), c = rep(c(-1, 1), each = 4))
  clean <- suppressWarnings(screen_covariates(o[c("a", "b")], c("a", "b"), diagnostics = TRUE))
  ch <- as.character(collinearity_card("k", "S", "m", clean, NULL, "pick", c("a", "b")))
  expect_match(ch, "Every VIF is 10 or less. Nothing to remove.", fixed = TRUE)
  expect_match(ch, "VIF 1.00 with both covariates", fixed = TRUE)
  # A constant goes whatever the answer (interpolation) or under Auto-Drop
  # (classification): its chip says so, and the verdict does not contradict it.
  flat <- suppressWarnings(screen_covariates(transform(o, flat = 3), c("a", "b", "c", "flat"), diagnostics = TRUE))
  expect_identical(flat$dropped_constant, "flat")
  fh <- as.character(collinearity_card("k", "S", "m", flat, NULL, "pick", c("a", "b", "c")))
  expect_match(fh, "Every VIF is 10 or less.</span>", fixed = TRUE)
  expect_false(grepl("Nothing to remove", fh, fixed = TRUE))
  # Pairs use the locations both covariates share, the VIF the complete ones:
  # a pair can be a near-duplicate where both are measured while every VIF on
  # the complete rows is 1. The verdict counts it and the staircase shows it.
  t <- seq(-10, 10, length.out = 192)
  d <- data.frame(a = c(t, o$a), b = c(t + .01 * sin(seq_along(t)), o$b), c = c(rep(NA, 192), o$c))
  chk <- classif_collinearity_diagnostic(d, names(d), 5)
  expect_length(chk$dropped_vif, 0L)
  expect_equal(unname(chk$diagnostics$steps[[1]]$vif), rep(1, 3), tolerance = 1e-12)
  dup <- chk$diagnostics$pairs[chk$diagnostics$pairs$near_duplicate, ]
  expect_identical(c(dup$var1, dup$var2, as.character(dup$n)), c("a", "b", "200"))
  expect_equal(dup$r, stats::cor(d$a, d$b), tolerance = 1e-12)
  expect_identical(chk$diagnostics$n_complete, 8L)
  for (collapsed in c(FALSE, TRUE)) {
    html <- as.character(collinearity_card("k", "S", "m", chk, NULL, "pick", names(d), collapsed = collapsed))
    expect_match(html, "1 pair has |r| above 0.95 on the locations where both are measured", fixed = TRUE)
    expect_match(html, "All pairwise correlations (3 pairs)", fixed = TRUE)
    expect_identical(lengths(regmatches(html, gregexpr('class="mn-cl-hc dup"', html, fixed = TRUE))), 1L)
    expect_false(grepl("numbered names", html, fixed = TRUE))
  }
})

test_that("an incomplete check still lists every covariate it could not use", {
  few <- suppressWarnings(screen_covariates(data.frame(a = c(1, 2), b = c(3, 1)), c("a", "b"), diagnostics = TRUE))
  html <- as.character(collinearity_card("k", "Scope", collinearity_meta(few, 2), few, NULL, "pick", c("a", "b")))
  expect_match(html, "<b>Check incomplete:</b> fewer than three locations carry every covariate", fixed = TRUE)
  expect_match(html, 'value="a"', fixed = TRUE)
  expect_match(html, 'value="b"', fixed = TRUE)
  expect_identical(lengths(regmatches(html, gregexpr(">not checked<", html, fixed = TRUE))), 2L)
  expect_identical(collinearity_meta(few, 2), "2 locations \u00b7 2 covariates")
  failed <- list(diagnostics = list(status = "failed", reason = "Warning in L: boom.", n_rows = NA_integer_))
  expect_match(as.character(collinearity_card("k", "L", collinearity_meta(failed, 3), failed, NULL, "p", "a")),
               "the check failed (boom)", fixed = TRUE)
  expect_identical(collinearity_meta(failed, 3), "3 covariates")
})

test_that("card markup escapes labels, keeps IDs as values and marks steps and near-duplicates", {
  d <- data.frame(a = 1:8, b = -(1:8))
  s <- suppressWarnings(screen_covariates(d, names(d), diagnostics = TRUE))
  labels <- desc_var_labels(names(d), list(list(actual = "a", label = "<Height & slope>"),
                                         list(actual = "b", label = "<Height & slope>")))
  html <- as.character(collinearity_card("vif_remove_1_card", "<Locality>", "m", s, labels, "vif_remove_1", names(d)))
  expect_match(html, "&lt;Height &amp; slope&gt; [a]", fixed = TRUE)
  expect_match(html, "&lt;Locality&gt;", fixed = TRUE)
  expect_false(grepl("<Height", html, fixed = TRUE))
  expect_match(html, 'name="vif_remove_1" value="a"', fixed = TRUE)
  expect_match(html, 'name="vif_remove_1" value="b"', fixed = TRUE)
  expect_match(html, 'aria-hidden="true"><span class="mn-cl-step">1</span><b>singular</b>', fixed = TRUE)
  expect_match(html, "&#8722;1.00</span><span class=\"mn-cl-tag\">near-duplicate</span>", fixed = TRUE)
})

test_that("the staircase holds every pair once, tinted by |r|", {
  a <- rep(c(-1, 1), 8)
  o <- rep(c(-1, -1, 1, 1), 4)
  noise <- rep(c(-1, -1, -1, -1, 1, 1, 1, 1), 2)
  b <- .93 * a + sqrt(1 - .93^2) * o
  d <- data.frame(a = a, b = b, c = a + b + .1 * noise, e = rep(c(1, -1, -1, 1), 4) * rep(c(1, 1, -1, -1), each = 4))
  s <- suppressWarnings(screen_covariates(d, names(d), vif_threshold = 5, diagnostics = TRUE))
  prof <- collinearity_profile(s)
  stair <- .cl_staircase(prof, s$diagnostics$pairs, function(v) v)
  k <- nrow(prof$rows)
  expect_equal(lengths(regmatches(stair, gregexpr('class="mn-cl-hc', stair, fixed = TRUE))), k * (k - 1) / 2)
  pairs <- s$diagnostics$pairs
  expect_equal(lengths(regmatches(stair, gregexpr('class="mn-cl-hc dup"', stair, fixed = TRUE))),
               sum(pairs$near_duplicate))
  tint <- as.integer(sub(".*--cl-a:([0-9]+)%.*", "\\1", regmatches(stair, gregexpr("--cl-a:[0-9]+%", stair))[[1]]))
  expect_identical(sort(tint), sort(as.integer(round(45 * abs(pairs$r)))))
  # An unestimable pair is a dash, not a number.
  pairs$r[1] <- NA_real_
  expect_match(.cl_staircase(prof, pairs, function(v) v), 'class="mn-cl-hc na" title="Not estimable">&#8211;<', fixed = TRUE)
})

test_that("co-kriging names covariates with too few co-located samples", {
  i <- seq_len(40)
  d <- data.frame(x = 500000 + (i %% 8) * 90 + (i %/% 8) * 7, y = 4400000 + (i %/% 8) * 85,
                  v = c(sin(i[1:30] / 3), rep(NA, 10)), pv = NA_real_,
                  e1 = cos(i / 4), e2 = (i %% 7) / 7, e3 = NA_real_)
  d$e3[c(1:6, 31:40)] <- i[c(1:6, 31:40)] / 10
  scr <- locality_screen_preview("A", d, 32633, "CK", c("e1", "e2", "e3"), 10, "v")
  expect_identical(scr$v$dropped_collocation, "e3")
  expect_identical(unname(scr$v$collocated), c(30L, 30L, 6L))
  expect_false("e3" %in% scr$v$kept)
  # Like a constant, the exclusion needs no answer.
  expect_identical(collinearity_screen_flags(list(A = scr)), list(any_vif = FALSE, incomplete = FALSE))
  html <- as.character(collinearity_card("k", "A", "m", scr$v, NULL, "pick", c("e1", "e2", "e3")))
  expect_match(html, "Too few co-located samples for co-kriging (fewer than 10), removed whatever you answer:",
               fixed = TRUE)
  expect_match(html, ">co-located: 6<", fixed = TRUE)
  expect_identical(locality_screen_preview("A", d, 32633, "RK", c("e1", "e2"), 10, "v")$v$dropped_collocation, NULL)
  # Removed on every surface the run maps, it is listed and never offered;
  # removed on one surface only, the other still uses it, so it stays a row.
  expect_identical(collinearity_forced_out(scr), "e3")
  expect_identical(collinearity_forced_out(list(v = scr$v, pv = list(dropped_constant = "e3"))), "e3")
  expect_identical(collinearity_forced_out(list(v = scr$v, pv = list())), character(0))
})

test_that("an RK check counts what each covariate lacks on the target's locations", {
  i <- seq_len(30)
  d <- data.frame(x = 500000 + (i %% 6) * 90, y = 4400000 + (i %/% 6) * 85, v = sin(i / 3), pv = NA_real_,
                  e1 = cos(i / 4), e2 = (i %% 7) / 7, e3 = NA_real_)
  # RK fits only locations carrying every covariate. e3 is measured nowhere,
  # so none is left, and the refused check names the covariate that emptied it.
  none <- locality_screen_preview("A", d, 32635, "RK", c("e1", "e2", "e3"), 10, "v")$v
  expect_identical(none$diagnostics$status, "insufficient_rows")
  expect_identical(c(none$diagnostics$n_rows, none$diagnostics$n_complete), c(30L, 0L))
  expect_identical(none$diagnostics$missing_by_var, c(e1 = 0L, e2 = 0L, e3 = 30L))
  expect_identical(collinearity_meta(none, 3), "30 locations, 0 complete · 3 covariates")
  html <- as.character(collinearity_card("k", "A", "m", none, NULL, "pick", c("e1", "e2", "e3")))
  expect_identical(lengths(regmatches(html, gregexpr("&#183; 30 missing</span>", html, fixed = TRUE))), 1L)
  expect_match(html, "removing the sparsest covariate usually lets the check run", fixed = TRUE)
  # Measured at ten locations: the check runs on those ten (its steps), and
  # the counts cover all thirty.
  d$e3[1:10] <- c(3, 1, 4, 1, 5, 9, 2, 6, 5, 3)
  part <- locality_screen_preview("A", d, 32635, "RK", c("e1", "e2", "e3"), 10, "v")$v
  expect_identical(c(part$diagnostics$n_rows, part$diagnostics$n_complete), c(30L, 10L))
  expect_identical(part$diagnostics$missing_by_var[["e3"]], 20L)
  expect_identical(part$diagnostics$steps[[1]]$n_complete, 10L)
  # Complete covariates keep the engine's own counts.
  d$e3 <- i %% 5
  full <- locality_screen_preview("A", d, 32635, "RK", c("e1", "e2", "e3"), 10, "v")$v
  expect_identical(c(full$diagnostics$n_rows, full$diagnostics$n_complete), c(30L, 30L))
  # Complete is what RK fits: samples carrying every covariate, merged. Two
  # samples per location that each lack what the other measured leave no
  # covariate missing after merging, yet no complete sample.
  split <- rbind(transform(d, e3 = NA_real_), transform(d, e1 = NA_real_, e2 = NA_real_))
  apart <- locality_screen_preview("A", split, 32635, "RK", c("e1", "e2", "e3"), 10, "v")$v
  expect_identical(apart$diagnostics$status, "insufficient_rows")
  expect_identical(c(apart$diagnostics$n_rows, apart$diagnostics$n_complete), c(30L, 0L))
  expect_identical(unname(apart$diagnostics$missing_by_var), c(0L, 0L, 0L))
})

test_that("dialog summaries count removals by locality and by scope", {
  strip <- as.character(collinearity_summary_strip(list(
    list(name = "Kale", target = "vif_remove_1_card", n_remove = 2, incomplete = FALSE),
    list(name = "A<B", target = "vif_remove_2_card", n_remove = 0, incomplete = TRUE),
    list(name = "Yorga", target = "vif_remove_3_card", n_remove = 0, incomplete = FALSE))))
  expect_match(strip, "3 localities checked separately &#183; 1 with Auto-Drop removals &#183; 1 incomplete", fixed = TRUE)
  expect_match(strip, "getElementById('vif_remove_1_card')", fixed = TRUE)
  expect_match(strip, "<b>A&lt;B</b><span class=\"cnt\">check incomplete</span>", fixed = TRUE)
  expect_match(strip, "&#10003; clean", fixed = TRUE)
  a <- rep(c(-1, 1), 8)
  b <- .93 * a + sqrt(1 - .93^2) * rep(c(-1, -1, 1, 1), 4)
  d <- data.frame(a = a, b = b, c = a + b + .1 * rep(c(-1, -1, -1, -1, 1, 1, 1, 1), 2))
  chk <- classif_collinearity_diagnostic(d, names(d), 5)
  line <- as.character(collinearity_inline_summary(chk, NULL, caption = "All localities", model = "Random Forest"))
  expect_match(line, "Auto-Drop would remove 2 of 3 (VIF above 5, Random Forest)", fixed = TRUE)
  expect_match(line, '<span class="mn-cl-step">2</span>', fixed = TRUE)
  wait <- list(diagnostics = list(status = "checking", n_rows = 16L))
  expect_match(as.character(collinearity_inline_summary(wait)), "checking the selected scope", fixed = TRUE)
})

test_that("diagnostics keep signed pairs and identify singular rather than measured infinite VIF", {
  d <- data.frame(a = 1:8, b = -(1:8))
  out <- suppressWarnings(detect_multicollinearity_engine(d, diagnostics = TRUE))
  expect_equal(out$diagnostics$pairs$r, -1)
  expect_true(out$diagnostics$pairs$near_duplicate)
  expect_identical(out$diagnostics$steps[[1]]$status, "singular")
  expect_null(out$diagnostics$steps[[1]]$vif)
  expect_true(out$diagnostics$steps[[1]]$drop %in% c("a", "b"))
  # The detailed trace is opt-in; ordinary model/fold records stay compact.
  expect_false("diagnostics" %in% names(suppressWarnings(detect_multicollinearity_engine(d))))
  o <- expand.grid(a = c(-1, 1), b = c(-1, 1))
  joint <- transform(o, c = a + b)
  combined <- detect_multicollinearity_engine(joint, diagnostics = TRUE)
  expect_false(any(combined$diagnostics$pairs$near_duplicate))
  expect_identical(combined$diagnostics$steps[[1]]$status, "singular")
  expect_null(combined$diagnostics$steps[[1]]$vif)
  expect_identical(combined$diagnostics$steps[[1]]$drop, "c")
})

test_that("nonfinite observations share the missing-value population and incomplete checks are explicit", {
  o <- expand.grid(a = c(-1, 1), b = c(-1, 1), e = c(-1, 1))
  d <- transform(o, c = a + b + 0.1 * e)[c("a", "b", "c")]
  d$a[1:3] <- c(Inf, -Inf, NaN)
  ref <- d
  ref$a[1:3] <- NA_real_
  out <- suppressWarnings(detect_multicollinearity_engine(d, diagnostics = TRUE))
  expected <- suppressWarnings(detect_multicollinearity_engine(ref, diagnostics = TRUE))
  expect_identical(out$kept, expected$kept)
  expect_identical(out$dropped, expected$dropped)
  expect_equal(out$diagnostics$steps, expected$diagnostics$steps)
  expect_equal(out$diagnostics$nonfinite_by_var[["a"]], 3L)
  expect_equal(out$diagnostics$n_complete, 5L)

  disjoint <- data.frame(a = c(1:4, rep(NA, 4)), b = c(rep(NA, 4), 1:4))
  insufficient <- detect_multicollinearity_engine(disjoint, diagnostics = TRUE)
  expect_identical(insufficient$diagnostics$status, "insufficient_rows")
  expect_identical(insufficient$diagnostics$pairs$status, "insufficient_rows")
  expect_true(is.na(insufficient$diagnostics$pairs$r))
  expect_equal(insufficient$diagnostics$pairs$n, 0L)
  expect_identical(detect_multicollinearity_engine(disjoint[FALSE, ], diagnostics = TRUE)$diagnostics$status,
                   "insufficient_rows")
  expect_identical(screen_covariates(disjoint, "a", diagnostics = TRUE)$diagnostics$status, "one_candidate")
  expect_identical(screen_covariates(disjoint, character(0), diagnostics = TRUE)$diagnostics$status, "no_candidates")
  expect_identical(detect_multicollinearity_engine(data.frame(text = letters[1:8]), diagnostics = TRUE)$diagnostics$status,
                   "no_candidates")
  # A diagnostic request never changes Keep All's handling of a covariate
  # that varies overall but is constant on the complete numeric rows.
  overlap <- data.frame(a = c(1, 2, 3, 3, 3), b = c(NA, NA, 1, 2, 3), c = 1:5)
  keep <- detect_multicollinearity_engine(overlap, vif_threshold = Inf)
  detailed <- detect_multicollinearity_engine(overlap, vif_threshold = Inf, diagnostics = TRUE)
  expect_identical(detailed$kept, keep$kept)
  expect_identical(detailed$dropped, keep$dropped)
  expect_length(detailed$diagnostics$steps[[1]]$drop, 0L)
})

test_that("a pair needs three shared observations to count as correlation evidence", {
  # Any two distinct points lie on a line, so r = +-1 there by definition. a
  # and b share two rows; a and c, b and c share four.
  d <- data.frame(a = c(1, 2, NA, NA, 5, 6), b = c(3, 1, 4, 2, NA, NA),
                  c = c(1, 5, 2, 8, 3, 3.5))
  out <- detect_multicollinearity_engine(d, diagnostics = TRUE)
  ab <- out$diagnostics$pairs[out$diagnostics$pairs$var1 == "a" & out$diagnostics$pairs$var2 == "b", ]
  expect_identical(ab$status, "insufficient_rows")
  expect_equal(ab$n, 2L)
  expect_true(is.na(ab$r))
  expect_false(ab$near_duplicate)
  expect_null(out$pairs)
  expect_false(out$has_collinearity)
  ac <- out$diagnostics$pairs[out$diagnostics$pairs$var2 == "c" & out$diagnostics$pairs$var1 == "a", ]
  both <- !is.na(d$a)
  expect_equal(ac$r, stats::cor(d$a[both], d$c[both]))
  expect_equal(ac$n, 4L)
  # Two complete rows: no pair is estimable and nothing is reported.
  two <- detect_multicollinearity_engine(data.frame(a = c(1, 2), b = c(5, 3)), diagnostics = TRUE)
  expect_identical(two$diagnostics$pairs$status, "insufficient_rows")
  expect_false(two$diagnostics$pairs$near_duplicate)
  expect_identical(two$diagnostics$status, "insufficient_rows")
})

test_that("the co-kriging screen rows accept a single location", {
  one <- make_test_points(1)
  expect_equal(nrow(.ck_screen_rows(one, NULL, c("aux1", "aux2"))), 1L)
  one$aux1 <- Inf
  expect_equal(nrow(.ck_screen_rows(one, NULL, c("aux1", "aux2"))), 0L)
})

test_that("diagnostic wrappers preserve the engine trace", {
  d <- make_collinear_df(20)
  ref <- suppressWarnings(detect_multicollinearity_engine(d, diagnostics = TRUE))
  expect_identical(suppressWarnings(check_vif(d, diagnostics = TRUE))$diagnostics, ref$diagnostics)
  expect_identical(suppressWarnings(screen_covariates(d, names(d), diagnostics = TRUE))$diagnostics, ref$diagnostics)
  pts <- make_test_points(8)
  pts$aux1[1] <- Inf
  expect_true(all(is.finite(.ck_screen_rows(pts, NULL, c("aux1", "aux2"))$aux1)))
  pts$aux2[3:8] <- NA_real_
  ck <- .ck_screen(pts, NULL, c("aux1", "aux2"), 10, diagnostics = TRUE)
  expect_true(ck$unscreened)
  expect_identical(ck$diagnostics$status, "insufficient_rows")
  expect_equal(ck$diagnostics$n_rows, 8L)
  expect_equal(ck$diagnostics$n_complete, 1L)
  expect_equal(ck$diagnostics$nonfinite_by_var[["aux1"]], 1L)
})

test_that("covariate interpolation uses only finite support observations", {
  pts <- make_test_points(20)
  grid <- make_test_grid_safe(pts, res = 250)
  pts$aux1[1:4] <- c(NA, NaN, Inf, -Inf)
  finite <- pts[is.finite(pts$aux1), ]
  lags <- calc_scientific_lags(finite)
  got <- krige_covariates(pts, grid, "aux1", lags, list(idw_p = 2, idw_nmax = 12))
  reference <- krige_covariates(finite, grid, "aux1", lags, list(idw_p = 2, idw_nmax = 12))
  expect_equal(got$grid_aux$aux1, reference$grid_aux$aux1)
  expect_equal(got$fallback, reference$fallback)
  pts$aux1[] <- Inf
  expect_error(krige_covariates(pts, grid, "aux1", lags, list()), "has no finite observations")
})

# Two localities of 40 samples on one table. In A, `twin` repeats `elev` up to
# noise, so A's screen drops one of the pair; in B the two are independent, and
# `flat` is constant there. Pooled over both localities no covariate exceeds
# VIF 10 (`pooled_vif` below), so a screen of the pooled rows finds nothing.
collinear_localities <- function() {
  with_seed(11, {
    n <- 40
    elev <- c(rnorm(n), rnorm(n))
    d <- data.frame(
      x = c(500000 + runif(n, 0, 1500), 520000 + runif(n, 0, 1500)),
      y = 4400000 + runif(2 * n, 0, 1500),
      Locality = rep(c("A", "B"), each = n),
      elev = elev,
      twin = c(elev[1:n] + rnorm(n, 0, 0.02), rnorm(n)),
      flat = c(rnorm(n), rep(3, n)))
    d$ph <- 6 + 0.4 * d$elev + rnorm(2 * n, 0, 0.2)
    d$ph_cve <- d$ph + rnorm(2 * n, 0, 0.1)
    d
  })
}

test_that("each locality is screened on its own rows, as its run screens them", {
  d <- collinear_localities()
  aux <- c("elev", "twin", "flat")
  mapping <- list(x = "x", y = "y", loc = "Locality")
  # The premise: the pooled rows trip nothing, so only a per-locality screen
  # can find what each locality's model drops.
  pooled <- check_vif(d[aux], threshold = 10)
  expect_length(pooled$dropped, 0L)

  frame_of <- function(l) locality_run_frame(d, mapping, l, "all", "ph", "ph_cve")
  expect_identical(frame_of("A")$v, d$ph[d$Locality == "A"])
  expect_identical(frame_of("A")$pv, d$ph_cve[d$Locality == "A"])
  expect_true(all(is.na(locality_run_frame(d, mapping, "A", "all", "ph")$pv)))
  expect_false("v" %in% names(locality_run_frame(d, mapping, "A")))

  for (method in c("RK", "RFK")) {
    scr <- lapply(c(A = "A", B = "B"), function(l) {
      locality_screen_preview(l, frame_of(l), 32635, method, aux, 10, c("v", "pv"))
    })
    # A: one of the near-identical pair, as collinear. B: the constant one.
    expect_length(scr$A$v$dropped_vif, 1L)
    expect_true(scr$A$v$dropped_vif %in% c("elev", "twin"))
    expect_length(scr$A$v$dropped_constant, 0L)
    expect_identical(scr$B$v$dropped_constant, "flat")
    expect_length(scr$B$v$dropped_vif, 0L)

    # The run drops exactly what the preview lists, per locality and surface.
    for (l in c("A", "B")) {
      run <- suppressWarnings(run_regional_interpolation(
        list(l = l, pts_data = frame_of(l),
             m_params = list(cv_strategy = "auto", idw_p_act = 2, idw_p_pre = 2, idw_nmax = 12, rf_ntree = 50)),
        method, 32635, aux, NULL, "convex", "fixed", 200, "fixed", 150, "EPSG:32635", TRUE, "actual"))
      for (col in c("v", "pv")) {
        expect_setequal(run[[paste0("aux_dropped_", if (col == "v") "act" else "pre")]],
                        as.character(unlist(scr[[l]][[col]][c("dropped_constant", "dropped_vif")])))
        expect_setequal(run[[paste0("aux_dropped_constant_", if (col == "v") "act" else "pre")]],
                        scr[[l]][[col]]$dropped_constant)
        expect_setequal(run[[paste0("aux_dropped_vif_", if (col == "v") "act" else "pre")]],
                        scr[[l]][[col]]$dropped_vif)
      }
    }

    expect_identical(collinearity_screen_flags(scr), list(any_vif = TRUE, incomplete = FALSE))
  }

  # Keep All screens no collinear covariate; the constant one still goes.
  keep <- locality_screen_preview("A", frame_of("A"), 32635, "RK", aux, Inf, "v")
  expect_length(keep$v$dropped_vif, 0L)
  expect_identical(locality_screen_preview("B", frame_of("B"), 32635, "RK", aux, Inf, "v")$v$dropped_constant, "flat")
})

test_that("a locality that only loses constants raises no question", {
  d <- collinear_localities()
  scr <- list(B = locality_screen_preview("B", locality_run_frame(d, list(x = "x", y = "y", loc = "Locality"), "B", "all", "ph"),
                                          32635, "RK", c("elev", "twin", "flat"), 10, "v"))
  expect_identical(collinearity_screen_flags(scr), list(any_vif = FALSE, incomplete = FALSE))
  # A single informative covariate also needs no multicollinearity decision.
  none <- list(L = locality_screen_preview("B", locality_run_frame(d, list(x = "x", y = "y", loc = "Locality"), "B", "all", "ph"),
                                           32635, "RK", "elev", 10, "v"))
  expect_identical(collinearity_screen_flags(none), list(any_vif = FALSE, incomplete = FALSE))
})

test_that("gate flags retain findings on any surface and distinguish incomplete checks", {
  s <- function(vif = character(0), cst = character(0), status = "checked")
    list(dropped_constant = cst, dropped_vif = vif, diagnostics = list(status = status))
  screens <- list(A = list(v = s(cst = "flat"), pv = s("a")), B = list(v = s(), pv = s()))
  expect_identical(collinearity_screen_flags(screens), list(any_vif = TRUE, incomplete = FALSE))
  screens$A$pv <- s(status = "insufficient_rows")
  expect_identical(collinearity_screen_flags(screens), list(any_vif = FALSE, incomplete = TRUE))
  screens$B$v <- s("b", status = "failed")
  expect_identical(collinearity_screen_flags(screens), list(any_vif = TRUE, incomplete = TRUE))
  expect_identical(collinearity_screen_flags(list(A = list(v = list(dropped_vif = character(0))))),
                   list(any_vif = FALSE, incomplete = TRUE))
  expect_identical(collinearity_screen_flags(list()), list(any_vif = FALSE, incomplete = FALSE))
})

test_that("different surface locations retain distinct diagnostic population identities", {
  d <- data.frame(x = 500000 + c(0, 100, 0, 100, 300, 400, 300, 400),
                  y = 4400000 + rep(c(0, 0, 100, 100), 2),
                  v = c(1:4, rep(NA_real_, 4)), pv = c(rep(NA_real_, 4), 1:4),
                  a = rep(1:4, 2), b = rep(2 * (1:4), 2))
  screens <- locality_screen_preview("A", d, 32635, "RK", c("a", "b"), 10, c("v", "pv"))
  # Equal counts and covariate distributions do not imply equal locations.
  expect_equal(screens$v$diagnostics$n_rows, screens$pv$diagnostics$n_rows)
  expect_equal(screens$v$diagnostics$pairs, screens$pv$diagnostics$pairs)
  expect_equal(screens$v$diagnostics$steps, screens$pv$diagnostics$steps)
  expect_type(screens$v$diagnostics$population_id, "character")
  expect_type(screens$pv$diagnostics$population_id, "character")
  expect_false(identical(screens$v$diagnostics$population_id, screens$pv$diagnostics$population_id))
})

test_that("a surface left without a covariate says why, by cause", {
  # Constants: named as constant, and no advice the user could not follow
  # (no answer of the collinearity dialog keeps a constant covariate).
  msg <- .screen_emptied_msg(list(dropped_constant = c("a", "b"), dropped_vif = character(0)),
                             "RK has no trend model left")
  expect_match(msg, "a, b are constant in this locality's samples", fixed = TRUE)
  expect_match(msg, "so RK has no trend model left. Select covariates that vary within this locality", fixed = TRUE)
  expect_false(grepl("Keep remaining", msg, fixed = TRUE))
  expect_false(grepl("\\.$", msg))
  one <- .screen_emptied_msg(list(dropped_constant = "a", dropped_vif = "b"), "Co-Kriging has no secondary variable left")
  expect_match(one, "a is constant in this locality's samples; b was removed as collinear", fixed = TRUE)
  expect_match(one, "Keep remaining", fixed = TRUE)

  # Through the engine: every covariate constant in the locality.
  d <- collinear_localities()
  d$twin[d$Locality == "B"] <- 1
  d$elev[d$Locality == "B"] <- 2
  run <- suppressWarnings(run_regional_interpolation(
    list(l = "B", pts_data = locality_run_frame(d, list(x = "x", y = "y", loc = "Locality"), "B", "all", "ph"),
         m_params = list(cv_strategy = "auto", idw_p_act = 2, idw_nmax = 12)),
    "RK", 32635, c("elev", "twin", "flat"), NULL, "convex", "fixed", 200, "fixed", 150, "EPSG:32635", FALSE, "actual"))
  expect_match(run$log_msg, "are constant in this locality's samples", fixed = TRUE)
  expect_match(run$log_msg, "Falling back to OK", fixed = TRUE)
  expect_false(grepl("Keep remaining", run$log_msg, fixed = TRUE))
  expect_false(is.null(run$r_a))
})

test_that("run-frame mappings read the original columns before assigning internal names", {
  d <- data.frame(Locality = c("A", "A", "B"), x = c(10, 20, 30), y = c(100, 200, 300),
                  v = c(7, 8, 9), pv = c(1, 2, 3))
  got <- locality_run_frame(d, list(x = "y", y = "x", loc = "Locality"), "A",
                            actual_col = "pv", pred_col = "v")
  expect_equal(got$x, d$y[1:2])
  expect_equal(got$y, d$x[1:2])
  expect_equal(got$v, d$pv[1:2])
  expect_equal(got$pv, d$v[1:2])
  # Targets may themselves use the names reserved for coordinates.
  got <- locality_run_frame(d, list(x = "v", y = "pv", loc = "Locality"), "A",
                            actual_col = "x", pred_col = "y")
  expect_equal(got$v, d$x[1:2])
  expect_equal(got$pv, d$y[1:2])
})

test_that("a sole numeric candidate with too few finite values is explicitly unestimable", {
  for (a in list(c(NA_real_, Inf, NaN), c(1, 2, NA_real_))) {
    d <- data.frame(a = a)
    ordinary <- screen_covariates(d, "a")
    detailed <- screen_covariates(d, "a", diagnostics = TRUE)
    expect_identical(detailed$diagnostics$status, "insufficient_rows")
    expect_equal(detailed$diagnostics$n_complete, sum(is.finite(a)))
    expect_identical(detailed[names(ordinary)], ordinary)
  }
  expect_identical(screen_covariates(data.frame(a = 1:3), "a", diagnostics = TRUE)$diagnostics$status,
                   "one_candidate")
})
