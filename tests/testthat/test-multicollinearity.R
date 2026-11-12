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
  # check_vif is a four-line forwarder over detect_multicollinearity_engine.
  # Its whole contract is that the four fields arrive intact, so assert
  # equality with the engine rather than re-testing the gate through it.
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
      }
    }

    lines <- collinearity_dialog_lines(scr, c(v = "Actual", pv = "Predicted"))
    expect_true(lines$any_vif)
    expect_identical(lines$lines, c(paste0("A: collinear: ", scr$A$v$dropped_vif), "B: constant: flat"))
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
  lines <- collinearity_dialog_lines(scr)
  expect_false(lines$any_vif)
  expect_identical(lines$lines, "B: constant: flat")
  # Nothing to drop: no line at all, and fewer than two covariates screen nothing.
  none <- list(L = locality_screen_preview("B", locality_run_frame(d, list(x = "x", y = "y", loc = "Locality"), "B", "all", "ph"),
                                           32635, "RK", "elev", 10, "v"))
  expect_identical(collinearity_dialog_lines(none), list(lines = character(0), any_vif = FALSE))
})

test_that("the dialog names a surface only where a locality's two surfaces differ", {
  s <- function(vif = character(0), cst = character(0)) list(dropped_constant = cst, dropped_vif = vif)
  screens <- list(Kale = list(v = s("a"), pv = s("a")),
                  Yorga = list(v = s("a", "c"), pv = s()),
                  Tavas = list(v = s(), pv = s()),
                  Karacasu = list(v = s("a"), pv = s("b")))
  out <- collinearity_dialog_lines(screens, c(v = "Actual", pv = "Predicted"))
  expect_identical(out$lines, c("Kale: collinear: a",
                                "Yorga (Actual): collinear: a; constant: c",
                                "Karacasu (Actual): collinear: a", "Karacasu (Predicted): collinear: b"))
  expect_true(out$any_vif)
})

test_that("a surface left without a covariate says why, by cause", {
  # Constants: named as constant, and no advice the user could not follow
  # (no answer of the collinearity dialog keeps a constant covariate).
  msg <- .screen_emptied_msg(list(dropped_constant = c("a", "b"), dropped_vif = character(0)),
                             "RK has no trend model left")
  expect_match(msg, "a, b are constant in this locality's samples", fixed = TRUE)
  expect_match(msg, "so RK has no trend model left. Select covariates that vary within this locality", fixed = TRUE)
  expect_false(grepl("Keep All", msg, fixed = TRUE))
  expect_false(grepl("\\.$", msg))
  one <- .screen_emptied_msg(list(dropped_constant = "a", dropped_vif = "b"), "Co-Kriging has no secondary variable left")
  expect_match(one, "a is constant in this locality's samples; b was removed as collinear", fixed = TRUE)
  expect_match(one, "Keep All", fixed = TRUE)

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
  expect_false(grepl("Keep All", run$log_msg, fixed = TRUE))
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
