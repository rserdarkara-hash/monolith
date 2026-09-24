# test-robust-vgm-fit.R — candidate screening, diagnostics, and selection
# policy for robust_vgm_fit.
# Spec: docs/superpowers/specs/2026-07-04-robust-vgm-fit-convergence-design.md
# Pinned values captured 2026-07-04 via app-context discovery (see plan).
# NOTE: pinned psill/range values are gstat-version-anchored fit.variogram
# outputs; after a gstat upgrade a mismatch is a fixture refresh, not a regression.

test_that("manual Matern uses nu 1.5 and the Auto-Fit weighted criterion", {
  mat <- manual_vgm(1, "Mat", 100, 0)
  expect_equal(mat$kappa[2], 1.5)
  expect_equal(gstat::variogramLine(mat, dist_vector = 100)$gamma, 1 - 2 / exp(1))
  expect_identical(manual_vgm(1, "Exp", 100, 0), gstat::vgm(1, "Exp", 100, 0))
  pts <- make_test_points(60)
  lags <- calc_scientific_lags(pts)
  emp <- gstat::variogram(v ~ 1, pts, width = lags$width, cutoff = lags$cutoff)
  fit <- suppressWarnings(gstat::fit.variogram(emp, gstat::vgm(1, "Sph", 300, 0.1)))
  expect_equal(vgm_weighted_sse(emp, fit), attr(fit, "SSErr"), tolerance = 1e-10)
})

test_that("a manual model with no total sill is refused; a pure nugget is not", {
  expect_null(validate_manual_vgm(psill = 1, nugget = 0.2, range = 300))
  expect_null(validate_manual_vgm(psill = 0, nugget = 0.5, range = 300))
  expect_match(validate_manual_vgm(psill = 0, nugget = 0, range = 300), "no variance")
  expect_match(validate_manual_vgm(psill = 1, nugget = 0, range = 0), "range")
  expect_match(validate_manual_vgm(psill = -1, nugget = 2, range = 300), "negative")
  expect_match(validate_manual_vgm(psill = NA, nugget = 0.2, range = 300), "finite")
})

test_that("manual sliders are scaled to the data, not to fixed decimals", {
  # Kale total nitrogen: variance 7.2e-4, auto-fit nugget 2.49e-4. Fixed
  # rounding gave a sill axis of max 0 and a step of 0.01.
  fit <- gstat::vgm(psill = 3.7e-4, model = "Sph", range = 3371, nugget = 2.49e-4)
  s <- manual_vgm_slider_spec(7.2e-4, 5000, fit)
  expect_gt(s$nugget$max, fit$psill[1])
  expect_gt(s$psill$max, fit$psill[2])
  expect_lte(s$nugget$step, 7.2e-4 / 50)
  expect_equal(s$nugget$value, fit$psill[1])
  expect_equal(s$range$value, 3371)
  expect_identical(s$model, "Sph")
  expect_true(s$step_ok)

  # Large-variance variable: same proportions.
  k <- manual_vgm_slider_spec(2.5e4, 5000)
  expect_equal(k$psill$value, 2.5e4)
  expect_equal(k$nugget$max, 5e4)
  expect_equal(k$range$max, 7500)
  expect_equal(k$range$value, 1250)

  # Bounds follow the data: a short-range or small-sill model does not narrow
  # them, and a model beyond them widens them.
  short <- manual_vgm_slider_spec(1, 5000, gstat::vgm(0.1, "Exp", 10, 0.01))
  expect_equal(short$range$max, 7500)
  expect_equal(short$nugget$max, 2)
  wide <- manual_vgm_slider_spec(1, 5000, gstat::vgm(3, "Exp", 9000, 1))
  expect_gte(wide$psill$max, 8)
  expect_gte(wide$range$max, 27000)

  # A single-row pure-nugget model has no structure to read a range from.
  nug <- manual_vgm_slider_spec(1, 5000, gstat::vgm(0.5, "Nug", 0))
  expect_equal(nug$nugget$value, 0.5)
  expect_equal(nug$psill$value, 0)
  expect_equal(nug$range$value, 1250)
  expect_null(nug$model)

  # ion.rangeSlider cannot represent a step below 1e-6.
  expect_false(manual_vgm_slider_spec(1e-5, 5000)$step_ok)
  expect_null(manual_vgm_slider_spec(0, 5000))
})

test_that("candidate screening emits no warnings on hostile data", {
  h <- make_hostile_vgm_input(seed = 1) # emits 17 warnings before this change
  expect_no_warning(robust_vgm_fit(h$v_emp, h$v_data))
})

test_that("returned fit carries the vgm_diagnostics contract", {
  h <- make_hostile_vgm_input(seed = 1)
  fit <- suppressWarnings(robust_vgm_fit(h$v_emp, h$v_data))
  d <- attr(fit, "vgm_diagnostics")
  expect_type(d, "list")
  expect_named(d, c("n_tried", "n_flawed", "flawed_winner", "status",
                    "practical_range", "max_lag", "sill_resolved", "range_side",
                    "trend_suspected", "target_degenerate"), ignore.order = TRUE)
  expect_identical(d$n_tried, 16L)
  expect_identical(d$n_flawed, 16L)
  expect_true(is.logical(d$flawed_winner))
  expect_true(d$status %in% VGM_FIT_STATUSES)
  # The categorical and the two legacy booleans must agree: they are read by
  # the CK seed guard and the map banner respectively.
  expect_identical(vgm_fit_status(fit), d$status)
  expect_equal(isTRUE(attr(fit, "flawed_winner")), identical(d$status, "singular_selected"))
  expect_equal(isTRUE(attr(fit, "is_fallback")),
               d$status %in% c("fit_failed", "heuristic_fallback"))
})

test_that("a converged fit outside the lag window beats a flawed one and a guess", {
  # The range window is a DIAGNOSTIC, not an eligibility rule. On a
  # trend-bearing field no candidate converges inside the window, so the old
  # rule found nothing eligible and returned the heuristic Spherical guess.
  # Among converged eligible candidates the lowest SSErr wins, whatever its range.
  h <- make_trend_vgm_input()
  fit <- suppressWarnings(robust_vgm_fit(h$v_emp, h$v_data))
  d <- attr(fit, "vgm_diagnostics")

  # Re-derive the selection here rather than reading it off the output.
  cands <- screen_vgm_candidates(h)
  converged <- Filter(function(x) x$valid && !x$flawed, cands)
  expect_gt(length(converged), 0)
  expect_false(any(vapply(converged, function(x) x$resolved, logical(1))))  # the premise
  expected <- converged[[which.min(vapply(converged, function(x) x$sse, numeric(1)))]]$fit

  expect_identical(as.character(fit$model[2]), as.character(expected$model[2]))
  expect_equal(fit$range[2], expected$range[2], tolerance = 1e-8)
  expect_false(isTRUE(attr(fit, "is_fallback")))
  expect_false(isTRUE(attr(fit, "flawed_winner")))
  expect_identical(d$status, "range_unresolved")
  # Used, but not presented as identified: the range sits beyond the cutoff
  # and the empirical variogram is still climbing there.
  expect_false(d$sill_resolved)
  expect_identical(d$range_side, "beyond")
  expect_true(d$trend_suspected)
  expect_gt(d$practical_range, d$max_lag)
})

test_that("a better-fitting unresolved fit is not beaten by a resolved one", {
  # The case a hard resolved-first tier gets wrong: both a converged candidate
  # whose range the lags resolve and a converged one whose range they do not,
  # and the unresolved one fits the empirical variogram better. Range
  # resolution is a diagnostic; the fitting criterion decides.
  h <- make_mixed_vgm_input()
  cands <- screen_vgm_candidates(h)
  converged <- Filter(function(x) x$valid && !x$flawed, cands)
  sse <- vapply(converged, function(x) x$sse, numeric(1))
  res <- vapply(converged, function(x) x$resolved, logical(1))
  expect_true(any(res) && any(!res))                 # the premise
  best_res <- min(sse[res]); best_unres <- min(sse[!res])
  expect_gt(best_res / best_unres, 1.5)               # a real gap, not a tie

  fit <- suppressWarnings(robust_vgm_fit(h$v_emp, h$v_data))
  winner <- converged[[which.min(sse)]]$fit
  expect_identical(as.character(fit$model[2]), as.character(winner$model[2]))
  expect_equal(attr(fit, "SSErr"), best_unres, tolerance = 1e-10)
  expect_identical(attr(fit, "vgm_diagnostics")$status, "range_unresolved")
  expect_false(isTRUE(attr(fit, "flawed_winner")))

  # The resolved candidate would have been the resolved-first answer.
  resolved_first <- converged[res][[which.min(sse[res])]]$fit
  expect_false(isTRUE(all.equal(attr(fit, "SSErr"), attr(resolved_first, "SSErr"))))
})

test_that("range resolution breaks a numerical tie and nothing more", {
  # Two candidates tied on the criterion: the resolved one is taken. A
  # resolved candidate that fits measurably worse is not.
  f_unres <- gstat::vgm(2, "Mat", 3000, 0.5, kappa = 1.5)
  f_res <- gstat::vgm(2, "Gau", 300, 0.5)
  tied <- list(list(fit = f_unres, sse = 1e-4, range_resolved = FALSE),
               list(fit = f_res, sse = 1e-4 * (1 + 1e-10), range_resolved = TRUE))
  expect_identical(as.character(.vgm_pick_best(tied)$fit$model[2]), "Gau")
  apart <- list(list(fit = f_unres, sse = 1e-4, range_resolved = FALSE),
                list(fit = f_res, sse = 1.01e-4, range_resolved = TRUE))
  expect_identical(as.character(.vgm_pick_best(apart)$fit$model[2]), "Mat")
  # The tie band is numerical, not a preference margin.
  expect_lte(VGM_SSE_TIE_REL, 1e-6)
})

test_that("the five fit statuses map to the attributes the UI reads", {
  # fit_failed is reachable end to end: fewer than 5 bins.
  h <- make_hostile_vgm_input(seed = 1)
  expect_identical(vgm_fit_status(robust_vgm_fit(h$v_emp[1:3, ], h$v_data)), "fit_failed")
  expect_identical(vgm_fit_status(robust_vgm_fit(NULL, rnorm(10))), "fit_failed")

  # heuristic_fallback needs every candidate to be ineligible, which
  # no real empirical variogram in this suite produces; pin the mapping.
  fb <- make_mock_vgm()
  attr(fb, "is_fallback") <- TRUE
  attr(fb, "vgm_diagnostics") <- list(status = "heuristic_fallback", n_tried = 16L)
  expect_identical(vgm_fit_status(fb), "heuristic_fallback")

  fw <- make_mock_vgm(); attr(fw, "flawed_winner") <- TRUE
  expect_identical(vgm_fit_status(fw), "singular_selected")
  # A supplied (manual) model carries no diagnostics and is not degraded.
  expect_identical(vgm_fit_status(make_mock_vgm()), "ok")
  expect_identical(vgm_fit_status(NULL), "fit_failed")

  expect_match(vgm_status_label("range_unresolved"), "range unresolved by lag support")
  expect_match(vgm_status_label("heuristic_fallback"), "heuristic")
})

test_that("the rising-at-the-cutoff detector needs a rise, not a wobble", {
  h <- make_trend_vgm_input()
  rising <- h$v_emp; rising$gamma <- seq(1, 10, length.out = nrow(rising))
  flat <- h$v_emp
  flat$gamma <- c(seq(1, 9, length.out = 5), rep(9, nrow(flat) - 5))
  expect_true(.vgm_still_rising(rising))
  expect_false(.vgm_still_rising(flat))
  # A single noisy last bin must not trip it: the detector compares thirds.
  noisy <- flat; noisy$gamma[nrow(noisy)] <- 9.4
  expect_false(.vgm_still_rising(noisy))
  expect_true(is.na(.vgm_still_rising(h$v_emp[1:3, ])))
  expect_true(is.na(.vgm_still_rising(NULL)))
})

test_that("a degenerate target is reported on the fit, not hidden in its sill", {
  h <- make_hostile_vgm_input(seed = 1)
  const <- suppressWarnings(robust_vgm_fit(h$v_emp, rep(7, 48)))
  expect_true(attr(const, "vgm_diagnostics")$target_degenerate)
  expect_true(vgm_target_degenerate(const))
  # near_constant: sd/max ~ 1.4e-8 sits ABOVE .is_degenerate_covariate's 1e-8
  # relative floor, so it is a real (tiny) variance and is NOT suppressed.
  set.seed(9)
  near <- suppressWarnings(robust_vgm_fit(h$v_emp, 7 + rnorm(48, 0, 1e-7)))
  expect_false(vgm_target_degenerate(near))
  expect_false(vgm_target_degenerate(make_mock_vgm()))
})

test_that("the candidate screen refuses a negative nugget", {
  # gamma(h) < 0 near the origin is not a valid variogram: the model is not
  # conditionally negative definite and gstat::krige() answers with 100% NA
  # predictions and NO condition raised, which reaches the user as a blank
  # locality behind a clean-looking variogram panel. Eligibility must exclude
  # such a candidate so it cannot win the lowest-SSErr contest.
  #
  # gstat 2.1.5 clamps negative sills itself, but ONLY for an empirical
  # variogram carrying attr(, "direct") - so drive the screen directly rather
  # than trying to coax a negative nugget out of fit.variogram.
  fit_neg <- gstat::vgm(psill = 2, model = "Sph", range = 300, nugget = -0.05)
  fit_ok  <- gstat::vgm(psill = 2, model = "Sph", range = 300, nugget = 0.05)
  max_dist <- 700
  screen <- function(f) {
    prange <- f$range[2] * .vgm_practical_range_factor(f$model[2], f$kappa[2])
    prange > (max_dist / 100) && prange < max_dist * 2 && f$psill[2] > 0 &&
      is.finite(f$psill[1]) && f$psill[1] >= 0
  }
  expect_false(screen(fit_neg))
  expect_true(screen(fit_ok))

  # And the shipped function never returns one on a real empirical variogram.
  pts <- make_test_points(40)
  lags <- calc_scientific_lags(pts)
  v_emp <- gstat::variogram(v ~ 1, pts, width = lags$width, cutoff = lags$cutoff)
  fit <- suppressWarnings(robust_vgm_fit(v_emp, pts$v))
  expect_true(is.finite(fit$psill[1]) && fit$psill[1] >= 0)
})

test_that("auto-fit eligibility rejects non-finite criteria and structural sills", {
  fit <- gstat::vgm(psill = 2, model = "Sph", range = 300, nugget = 0.05)
  expect_true(.vgm_autofit_eligible(fit, sse = 1, practical_range = 300))
  expect_false(.vgm_autofit_eligible(fit, sse = Inf, practical_range = 300))
  expect_false(.vgm_autofit_eligible(fit, sse = NULL, practical_range = 300))

  infinite_sill <- fit
  infinite_sill$psill[2] <- Inf
  expect_false(.vgm_autofit_eligible(infinite_sill, sse = 1, practical_range = 300))
})

test_that("diagnostics present on the tiny-variogram early return", {
  fit <- robust_vgm_fit(NULL, rnorm(10))
  d <- attr(fit, "vgm_diagnostics")
  expect_identical(d$n_tried, 0L)
  expect_identical(d$n_flawed, 0L)
})

test_that("robust_vgm_fit tolerates an NA empirical-variogram bin", {
  # gstat::variogram drops empty bins, so gamma is finite in practice; this
  # pins the initial_nugget seed against min(NA) -> the "missing value where
  # TRUE/FALSE needed" crash on the `== 0` test (na.rm now matches the
  # var()/max() seeds in the same function). Runs inside PSOCK workers, where
  # the crash would surface only as a generic "Parallel ... Failed" modal.
  pts <- make_test_points(30)
  lags <- calc_scientific_lags(pts)
  v_emp <- gstat::variogram(v ~ 1, pts, width = lags$width, cutoff = lags$cutoff)
  v_emp$gamma[1] <- NA_real_
  fit <- suppressWarnings(robust_vgm_fit(v_emp, pts$v))
  expect_s3_class(fit, "variogramModel")
})

test_that("diagnostics survive clean_gstat_env (worker serialization path)", {
  h <- make_hostile_vgm_input(seed = 1)
  fit <- suppressWarnings(robust_vgm_fit(h$v_emp, h$v_data))
  cleaned <- clean_gstat_env(fit)
  expect_false(is.null(attr(cleaned, "vgm_diagnostics")))
})

test_that("muffling does not change selection on a clean-winner fixture", {
  # Invariance guard (green before AND after this target): seed 12's
  # lowest-SSErr candidate is already clean, so neither Task 1 (neutral)
  # nor Task 2 (clean preference) may alter this selection.
  h <- make_hostile_vgm_input(seed = 12)
  fit <- suppressWarnings(robust_vgm_fit(h$v_emp, h$v_data))
  expect_identical(as.character(fit$model[2]), "Gau")
  expect_equal(fit$psill[1], 21.239390, tolerance = 1e-3)
  expect_equal(fit$psill[2], 84.136984, tolerance = 1e-3)
  expect_equal(fit$range[2], 379.252147, tolerance = 1e-3)
  expect_type(attr(fit, "vgm_diagnostics"), "list") # contract holds on the clean-winner path too
})

test_that("a clean candidate is preferred over a lower-SSErr flawed one", {
  # Candidate table for this fixture (2026-09-16, gstat 2.1.6): clean Mat at
  # nugget 0 (SSErr 11.4508), clean Exp at nugget 0 (12.1713), flawed Sph
  # (lowest 11.9040) and flawed Gau at nugget 0 (10.9345). The zero-nugget
  # Mat and Gau are ineligible, so the converged Exp must beat the
  # lower-SSErr flawed Sph.
  pts <- make_test_points(30)
  lags <- calc_scientific_lags(pts)
  v_emp <- gstat::variogram(v ~ 1, pts, width = lags$width, cutoff = lags$cutoff)
  fit <- robust_vgm_fit(v_emp, pts$v)
  expect_identical(as.character(fit$model[2]), "Exp")
  expect_equal(fit$psill[1], 0, tolerance = 1e-6)
  expect_equal(fit$psill[2], 124.889, tolerance = 1e-3)
  expect_equal(fit$range[2], 93.046, tolerance = 1e-3)
  expect_false(isTRUE(attr(fit, "flawed_winner")))
  expect_gt(attr(fit, "vgm_diagnostics")$n_flawed, 0)
})

test_that("the smooth-origin nugget share covers Gaussian and Matern only", {
  expect_equal(vgm_smooth_nugget_share(gstat::vgm(0.8, "Gau", 300, 0.2)), 0.2)
  expect_equal(vgm_smooth_nugget_share(manual_vgm(1, "Mat", 300, 0)), 0)
  # Matern nu = 0.5 is the exponential model: linear at the origin.
  expect_true(is.na(vgm_smooth_nugget_share(gstat::vgm(1, "Mat", 300, 0, kappa = 0.5))))
  expect_true(is.na(vgm_smooth_nugget_share(gstat::vgm(1, "Exp", 300, 0))))
  expect_true(is.na(vgm_smooth_nugget_share(gstat::vgm(1, "Sph", 300, 0))))
  expect_true(is.na(vgm_smooth_nugget_share(gstat::vgm(0.5, "Nug", 0))))
  expect_true(is.na(vgm_smooth_nugget_share(NULL)))
})

test_that("auto-fit never returns a Gaussian or Matern structure at a zero nugget", {
  # make_test_points(30): the lowest-SSErr clean candidate is a Matern at
  # nugget 0 (see the candidate table above).
  pts <- make_test_points(30)
  lags <- calc_scientific_lags(pts)
  v_emp <- gstat::variogram(v ~ 1, pts, width = lags$width, cutoff = lags$cutoff)
  expect_false(isTRUE(vgm_smooth_nugget_share(robust_vgm_fit(v_emp, pts$v)) <= 1e-8))

  # Golden Acipayam, available P (83 points): the lowest-SSErr clean candidate
  # is a Gaussian at nugget 0, whose ordinary-kriging surface left the observed
  # range by 0.32 of its span and went negative for a non-negative variable.
  g <- golden_sf("full", "Acipayam")
  g$v <- g$p
  g <- dedup_valid_points(g, "v")
  lags <- calc_scientific_lags(g)
  v_emp <- gstat::variogram(v ~ 1, g, width = lags$width, cutoff = lags$cutoff)
  fit <- suppressWarnings(robust_vgm_fit(v_emp, g$v))
  expect_false(isTRUE(vgm_smooth_nugget_share(fit) <= 1e-8))

  hull <- sf::st_convex_hull(sf::st_union(g))
  grid <- sf::st_as_sf(sf::st_make_grid(hull, n = c(40, 40), what = "centers"))
  sf::st_geometry(grid) <- "geometry"
  grid <- grid[lengths(sf::st_intersects(grid, hull)) > 0, ]
  pred <- gstat::krige(v ~ 1, g, grid, model = fit, debug.level = 0)$var1.pred
  span <- diff(range(g$v))
  expect_gte(min(pred), min(g$v) - 0.1 * span)
  expect_lte(max(pred), max(g$v) + 0.1 * span)
})

test_that("flawed winner is tagged when no clean candidate exists", {
  h <- make_hostile_vgm_input(seed = 1) # 16/16 candidates flawed, 5 in-window
  fit <- robust_vgm_fit(h$v_emp, h$v_data)
  expect_true(isTRUE(attr(fit, "flawed_winner")))
  expect_true(attr(fit, "vgm_diagnostics")$flawed_winner)
  expect_identical(as.character(fit$model[2]), "Sph")
  expect_equal(fit$psill[2], 123.891310, tolerance = 1e-3)
  expect_false(isTRUE(attr(fit, "is_fallback")))
})

test_that("tiny empirical variogram fallback is tagged is_fallback", {
  fit_null <- robust_vgm_fit(NULL, rnorm(10))
  expect_true(isTRUE(attr(fit_null, "is_fallback")))
  h <- make_hostile_vgm_input(seed = 1)
  fit_tiny <- robust_vgm_fit(h$v_emp[1:3, ], h$v_data)
  expect_true(isTRUE(attr(fit_tiny, "is_fallback")))
})

test_that("the sanity window is applied to the PRACTICAL range, per family", {
  # gstat's `a` means a different ground distance per family, so screening on it
  # made the window ~3x tighter (at the low end) for Exp than for Sph and let an
  # Exp structure three times longer than the data extent through at the top.
  expect_equal(.vgm_practical_range_factor("Sph"), 1)
  expect_equal(.vgm_practical_range_factor("Exp"), 3)
  expect_equal(.vgm_practical_range_factor("Gau"), sqrt(3))
  expect_equal(.vgm_practical_range_factor("Mat", 1.5), 4.75)
  # Matern with nu = 0.5 IS the exponential model and must screen like one.
  expect_equal(.vgm_practical_range_factor("Mat", 0.5), 3)
  # Unknown/nugget-only families fall back to "a is the practical range".
  expect_equal(.vgm_practical_range_factor("Nug"), 1)

  pts <- make_test_points(30)
  lags <- calc_scientific_lags(pts)
  v_emp <- gstat::variogram(v ~ 1, pts, width = lags$width, cutoff = lags$cutoff)
  fit <- robust_vgm_fit(v_emp, pts$v)
  max_dist <- max(v_emp$dist, na.rm = TRUE)
  prange <- fit$range[2] * .vgm_practical_range_factor(fit$model[2], fit$kappa[2])
  expect_gt(prange, max_dist / 100)
  expect_lt(prange, max_dist * 2)
})

test_that("build_vgm_warning_html returns NULL when nothing is flagged", {
  expect_null(build_vgm_warning_html(list(A_act = make_mock_vgm())))
  expect_null(build_vgm_warning_html(list()))
})

test_that("build_vgm_warning_html renders red fallback and amber flawed sections", {
  f_fb <- make_mock_vgm(); attr(f_fb, "is_fallback") <- TRUE
  f_fw <- make_mock_vgm(); attr(f_fw, "flawed_winner") <- TRUE
  html <- build_vgm_warning_html(list(LocA_act = f_fb, LocB_pre = f_fw, LocC_act = make_mock_vgm()))
  expect_match(html, "LocA (actual)", fixed = TRUE)
  expect_match(html, "LocB (predicted)", fixed = TRUE)
  expect_match(html, "default spherical variogram model", fixed = TRUE)
  expect_match(html, "non-converged or singular", fixed = TRUE)
  expect_no_match(html, "LocC", fixed = TRUE)
})

test_that("build_vgm_warning_html filters by target and strips the suffix", {
  f_fb <- make_mock_vgm(); attr(f_fb, "is_fallback") <- TRUE
  f_fw <- make_mock_vgm(); attr(f_fw, "flawed_winner") <- TRUE
  fits <- list(LocA_act = f_fb, LocB_pre = f_fw)

  html_act <- build_vgm_warning_html(fits, target = "act")
  expect_match(html_act, "LocA", fixed = TRUE)
  expect_no_match(html_act, "LocA_act", fixed = TRUE)
  expect_no_match(html_act, "LocB", fixed = TRUE)

  html_pre <- build_vgm_warning_html(fits, target = "pre")
  expect_match(html_pre, "LocB", fixed = TRUE)
  expect_no_match(html_pre, "LocA", fixed = TRUE)

  # NULL when the only flagged fits belong to the other target
  expect_null(build_vgm_warning_html(list(LocA_act = f_fb), target = "pre"))
})

test_that("build_vgm_warning_html flags a Gaussian or Matern model with a small nugget", {
  html <- build_vgm_warning_html(list(LocA_act = manual_vgm(1, "Gau", 300, 0.02),
                                      LocB_act = manual_vgm(1, "Gau", 300, 0.2),
                                      LocC_act = manual_vgm(1, "Exp", 300, 0)))
  expect_match(html, "nugget below 5% of the sill for: LocA (actual)", fixed = TRUE)
  expect_no_match(html, "LocB", fixed = TRUE)
  expect_no_match(html, "LocC", fixed = TRUE)
  expect_no_match(html, "non-converged or singular", fixed = TRUE)
  expect_null(build_vgm_warning_html(list(LocB_act = manual_vgm(1, "Mat", 300, 0.2))))
})

test_that("a converged out-of-window fit gets its own band, not the failure band", {
  f <- gstat::vgm(psill = 2, model = "Exp", range = 300, nugget = 0.5) # practical 900
  attr(f, "vgm_diagnostics") <- list(status = "range_unresolved", practical_range = 900,
                                     max_lag = 700, sill_resolved = FALSE,
                                     range_side = "beyond", trend_suspected = FALSE,
                                     target_degenerate = FALSE)
  html <- build_vgm_warning_html(list(LocA_act = f))
  expect_match(html, "Variogram range not resolved", fixed = TRUE)
  expect_match(html, "Practical range extends beyond sampled lag support for: LocA (actual)", fixed = TRUE)
  expect_no_match(html, "Variogram fit failed", fixed = TRUE)
  expect_no_match(html, "non-converged or singular", fixed = TRUE)

  # The window fails at both ends, and the two mean opposite things.
  below <- f
  attr(below, "vgm_diagnostics")$range_side <- "below"
  html_b <- build_vgm_warning_html(list(LocB_act = below))
  expect_match(html_b, "Practical range is below sampled-distance resolution", fixed = TRUE)
  expect_match(html_b, "effective short-range resolution threshold", fixed = TRUE)
  expect_match(html_b, "near-pure nugget", fixed = TRUE)
  expect_no_match(html_b, "extends beyond sampled lag support", fixed = TRUE)
})

test_that("a variogram still rising at the cutoff gets a hedged trend advisory", {
  f <- gstat::vgm(psill = 2, model = "Exp", range = 300, nugget = 0.5)
  attr(f, "vgm_diagnostics") <- list(status = "range_unresolved", practical_range = 900,
                                     max_lag = 700, sill_resolved = FALSE,
                                     range_side = "beyond", trend_suspected = TRUE,
                                     target_degenerate = FALSE)
  html <- build_vgm_warning_html(list(LocA_act = f))
  expect_match(html, "Sill not observed; possible large-scale trend", fixed = TRUE)
  # The trend case is named by its remedy, never by an engine to switch to:
  # the variogram alone cannot say which case applies.
  expect_match(html, "kriging its residuals addresses the trend case", fixed = TRUE)
  expect_no_match(html, "Regression Kriging", fixed = TRUE)
  # A rising variogram is a diagnostic for trend, never proof of it.
  expect_no_match(html, "non-stationarity detected", fixed = TRUE)

  # An unknown engine keeps the value-scale wording: the banner must never
  # depend on the caller having resolved a method.
  expect_match(build_vgm_warning_html(list(LocA_act = f), engine = "OK"),
               "Sill not observed; possible large-scale trend", fixed = TRUE)
})

test_that("the rising-variogram advisory never tells a detrending engine to detrend", {
  f <- gstat::vgm(psill = 2, model = "Exp", range = 300, nugget = 0.5)
  attr(f, "vgm_diagnostics") <- list(status = "range_unresolved", practical_range = 900,
                                     max_lag = 700, sill_resolved = FALSE,
                                     range_side = "beyond", trend_suspected = TRUE,
                                     target_degenerate = FALSE)

  # RK and RFK krige the residuals of a fitted trend, so "model the trend with
  # covariates and fit the variogram to the residuals" is what they already did.
  for (eng in c("RK", "RFK")) {
    html <- build_vgm_warning_html(list(LocA_act = f), engine = eng)
    expect_match(html, "Residual variogram still rising at the lag cutoff", fixed = TRUE)
    expect_match(html, "Scientific Analysis", fixed = TRUE)
    # The circularity guard: no engine is named, in either direction.
    expect_no_match(html, "Regression Kriging", fixed = TRUE)
    expect_no_match(html, "Sill not observed", fixed = TRUE)
    # The extent cause has no in-app remedy and must stay on the list.
    expect_match(html, "beyond this locality's extent", fixed = TRUE)
  }

  # CK does not detrend (its variogram is seeded from the measured values), so
  # it keeps the value-scale wording if it ever reaches this banner.
  html_ck <- build_vgm_warning_html(list(LocA_act = f), engine = "CK")
  expect_match(html_ck, "Sill not observed; possible large-scale trend", fixed = TRUE)
  expect_no_match(html_ck, "Residual variogram still rising", fixed = TRUE)
})

test_that("banner close button targets its own container, not a fixed id", {
  f_fw <- make_mock_vgm(); attr(f_fw, "flawed_winner") <- TRUE
  html <- build_vgm_warning_html(list(L1_act = f_fw))
  expect_match(html, "this.parentElement.style.display", fixed = TRUE)
  expect_no_match(html, "getElementById", fixed = TRUE)
})

test_that("amber-only banner omits the red section", {
  f_fw <- make_mock_vgm(); attr(f_fw, "flawed_winner") <- TRUE
  html <- build_vgm_warning_html(list(L1_act = f_fw))
  expect_match(html, "non-converged or singular", fixed = TRUE)
  expect_no_match(html, "Variogram fit failed", fixed = TRUE)
})

test_that("red-only banner omits the amber section", {
  f_fb <- make_mock_vgm(); attr(f_fb, "is_fallback") <- TRUE
  html <- build_vgm_warning_html(list(L1_act = f_fb))
  expect_match(html, "default spherical variogram model", fixed = TRUE)
  expect_no_match(html, "non-converged or singular", fixed = TRUE)
})

# ── Sill-derived diagnostics and their qualifier ────────────────────────────

test_that("structural dependency is qualified when the sill is not resolved", {
  # Practical range 3a = 900 m against a 700 m cutoff: the sill the percentage
  # is derived from was never reached by the data.
  f <- gstat::vgm(psill = 2, model = "Exp", range = 300, nugget = 0.5)
  attr(f, "vgm_diagnostics") <- list(status = "range_unresolved", max_lag = 700,
                                     sill_resolved = FALSE, target_degenerate = FALSE)
  p <- vgm_params_row(f)
  expect_equal(p$practical_range, 900)
  expect_equal(p$max_lag, 700)
  expect_false(p$sill_resolved)
  expect_equal(p$sdep, 80)   # (2.5 - 0.5) / 2.5

  chr <- .vgm_params_chr(f)
  expect_match(chr[6], "Not reliably identified", fixed = TRUE)
  expect_match(chr[6], "Model-extrapolated: 80%", fixed = TRUE)   # in the tooltip
  expect_match(chr[5], "max lag 700", fixed = TRUE)

  # Resolved: the plain percentage, no qualifier anywhere.
  g <- gstat::vgm(psill = 2, model = "Exp", range = 100, nugget = 0.5) # practical 300
  attr(g, "vgm_diagnostics") <- list(status = "ok", max_lag = 700,
                                     sill_resolved = TRUE, target_degenerate = FALSE)
  chr_ok <- .vgm_params_chr(g)
  expect_identical(chr_ok[6], "80%")
  expect_no_match(chr_ok[5], "max lag", fixed = TRUE)

  # A supplied model records no empirical support, so the qualifier must not
  # fire for it: the user chose those parameters.
  m <- manual_vgm(2, "Exp", 300, 0.5)
  expect_true(is.na(vgm_params_row(m)$sill_resolved))
  expect_identical(.vgm_params_chr(m)[6], "80%")
})

test_that("a variogram subtitle reads the fitted parameters the Variogram Parameters card reports", {
  # Resolved: family, nugget, partial sill, a and the practical range (3a for Exp).
  g <- gstat::vgm(psill = 2, model = "Exp", range = 100, nugget = 0.5)
  attr(g, "vgm_diagnostics") <- list(status = "ok", max_lag = 700,
                                     sill_resolved = TRUE, target_degenerate = FALSE)
  expect_identical(vgm_fit_subtitle(g),
                   "Fitted: Exp (Nugget: 0.5, Partial Sill: 2, Range (a): 100, Practical Range: 300)")
  # Not resolved: the practical range carries its qualifier, as on the card.
  f <- gstat::vgm(psill = 2, model = "Exp", range = 300, nugget = 0.5)
  attr(f, "vgm_diagnostics") <- list(status = "range_unresolved", max_lag = 700,
                                     sill_resolved = FALSE, target_degenerate = FALSE)
  expect_match(vgm_fit_subtitle(f),
               "Practical Range: 900, model-extrapolated beyond the longest lag 700)", fixed = TRUE)
  # A Matern names its smoothness; small values keep their digits.
  m <- gstat::vgm(psill = 3.873e-05, model = "Mat", range = 6510, nugget = 0.000172, kappa = 1.5)
  expect_match(vgm_fit_subtitle(m),
               "Fitted: Mat, kappa = 1.5 (Nugget: 0.000172, Partial Sill: 3.873e-05, Range (a): 6510,",
               fixed = TRUE)
  expect_identical(vgm_fit_subtitle(gstat::vgm(0.4, "Nug", 0)), "Fitted: pure nugget (Nugget: 0.4)")
  expect_null(vgm_fit_subtitle(NULL))
  # Never "Fitted" for a model nothing was fitted to: the heuristic stand-in
  # of a failed fit, or a model the user applied.
  fb <- robust_vgm_fit(NULL, c(1, 2, 3))
  expect_match(vgm_fit_subtitle(fb), "^Heuristic fallback, not fitted: Sph ")
  applied <- stamp_vgm(manual_vgm(2, "Exp", 300, 0.5), "ph", "manual")
  expect_match(vgm_fit_subtitle(applied), "^Applied manual model: Exp ")
})

test_that("the variogram export carries the lag support beside the percentage", {
  f <- gstat::vgm(psill = 2, model = "Exp", range = 300, nugget = 0.5)
  attr(f, "vgm_diagnostics") <- list(status = "range_unresolved", max_lag = 700,
                                     sill_resolved = FALSE, target_degenerate = FALSE)
  df <- vgm_params_export_df(list(LocA_act = f, LocA_pre = manual_vgm(2, "Exp", 300, 0.5)))
  expect_true(all(c("Max Lag", "Sill Resolved") %in% names(df)))
  expect_equal(df$`Structural Dep. (%)`[1], 80)   # a file cannot carry a tooltip
  expect_equal(df$`Max Lag`[1], 700)
  expect_false(df$`Sill Resolved`[1])
  expect_true(is.na(df$`Sill Resolved`[2]))       # the manual model
})

test_that("a degenerate target reports its cause instead of a noise sill", {
  f <- gstat::vgm(psill = 1e-60, model = "Exp", range = 70, nugget = 0)
  attr(f, "vgm_diagnostics") <- list(status = "ok", max_lag = 700,
                                     sill_resolved = TRUE, target_degenerate = TRUE)
  df <- vgm_params_table_df(list(LocA_act = f), "LocA")
  expect_identical(names(df), "Status")
  expect_match(df$Status[1], "no usable variance", fixed = TRUE)

  # With a usable Predicted surface beside it, the table still renders and the
  # degenerate column says so rather than printing 100% structural dependency.
  g <- gstat::vgm(psill = 2, model = "Exp", range = 100, nugget = 0.5)
  attr(g, "vgm_diagnostics") <- list(status = "ok", max_lag = 700,
                                     sill_resolved = TRUE, target_degenerate = FALSE)
  df2 <- vgm_params_table_df(list(LocA_act = f, LocA_pre = g), "LocA")
  expect_true(all(c("Param", "Actual", "Predicted") %in% names(df2)))
  expect_true(all(df2$Actual == "Not estimated"))
  expect_identical(df2$Predicted[6], "80%")
})

test_that("the per-fold variogram status is summarised and logged by fold", {
  cv <- structure(list(), class = "list")
  attr(cv, "cv_fold_meta") <- list(
    "1" = list(vgm_status = "ok"),
    "2" = list(vgm_status = "range_unresolved"),
    "3" = list(vgm_status = "heuristic_fallback"),
    "7" = list(vgm_status = "singular_selected"),
    "10" = list(vgm_status = "ok"))
  tb <- .cv_fold_status_table(cv)
  expect_identical(tb$status, c("heuristic_fallback", "singular_selected",
                                "range_unresolved", "ok"))   # worst first
  expect_identical(tb$n, c(1L, 1L, 1L, 2L))
  expect_identical(tb$folds[tb$status == "ok"], "1, 10")
  expect_identical(tb$folds[tb$status == "range_unresolved"], "2")

  res <- .log_vgm_fold_status(list(cv_obj = cv, log_msg = ""), "OK", "West_Field")
  expect_match(res$log_msg, "West_Field: 3 of 5 folds", fixed = TRUE)
  expect_match(res$log_msg, "Fold 2: converged fit, range unresolved by lag support", fixed = TRUE)
  expect_match(res$log_msg, "Fold 3: heuristic fallback", fixed = TRUE)
  expect_match(res$log_msg, "Fold 7: singular/non-converged fit selected", fixed = TRUE)

  # Every fold clean: nothing to say.
  clean <- structure(list(), class = "list")
  attr(clean, "cv_fold_meta") <- list("1" = list(vgm_status = "ok"))
  expect_identical(.log_vgm_fold_status(list(cv_obj = clean, log_msg = ""), "OK", "L")$log_msg, "")
  expect_null(.cv_fold_status_table(structure(list(), class = "list")))
})

# ── calc_directional_variogram (anisotropy diagnostic) ──────────────────────

test_that("directional variogram cones partition the point pairs exactly", {
  # alpha = 0/45/90/135 with tol.hor = 22.5 covers the half circle exactly
  # once, so every pair counted by the omnidirectional variogram is counted in
  # exactly one direction. If this drifts, the four curves are no longer
  # disjoint subsets of the omnidirectional one and comparing them is invalid.
  set.seed(4)
  m <- 220
  px <- runif(m, 0, 5000); py <- runif(m, 0, 5000)
  pv <- 10 * sin(px / 900) + 4 * cos(py / 2500) + rnorm(m, 0, 1)
  pts <- sf::st_as_sf(data.frame(x = px, y = py, v = pv),
                      coords = c("x", "y"), crs = 32633)

  lg <- calc_scientific_lags(pts)
  omni <- gstat::variogram(v ~ 1, pts, width = lg$width, cutoff = lg$cutoff)
  dir <- calc_directional_variogram(pts, "v")

  expect_s3_class(dir, "data.frame")
  expect_true(all(c("np", "dist", "gamma", "dir.hor") %in% names(dir)))
  expect_equal(sort(unique(dir$dir.hor)), c(0, 45, 90, 135))
  expect_equal(sum(dir$np), sum(omni$np))
})

test_that("calc_directional_variogram projects geographic input to metres", {
  # Bearings and lag distances are meaningless in degrees; the helper must
  # apply the app's auto-UTM rule rather than trusting the incoming CRS.
  set.seed(4)
  m <- 200
  px <- runif(m, 0, 5000); py <- runif(m, 0, 5000)
  pv <- 10 * sin(px / 900) + rnorm(m, 0, 1)
  pts <- sf::st_as_sf(data.frame(x = px, y = py, v = pv),
                      coords = c("x", "y"), crs = 32633)
  d_proj <- calc_directional_variogram(pts, "v")
  d_ll <- calc_directional_variogram(sf::st_transform(pts, 4326), "v")

  expect_false(is.null(d_ll))
  # Same field, so the lag range must stay metric and comparable, not collapse
  # to the ~0.05 degree extent of the same points in EPSG:4326.
  expect_gt(max(d_ll$dist), 100)
  expect_equal(max(d_ll$dist), max(d_proj$dist), tolerance = 0.05)
})

test_that("calc_directional_variogram degrades to NULL instead of erroring", {
  set.seed(4)
  pts <- sf::st_as_sf(data.frame(x = runif(30, 0, 1000), y = runif(30, 0, 1000),
                                 v = rnorm(30)),
                      coords = c("x", "y"), crs = 32633)
  expect_null(calc_directional_variogram(pts[1:5, ], "v"))   # below min_n
  expect_null(calc_directional_variogram(pts, "not_a_column"))
  expect_null(calc_directional_variogram(NULL, "v"))
  pts$all_na <- NA_real_
  expect_null(calc_directional_variogram(pts, "all_na"))
})

test_that("build_directional_variogram_ggplot labels bearings with compass names", {
  set.seed(4)
  m <- 200
  pts <- sf::st_as_sf(data.frame(x = runif(m, 0, 5000), y = runif(m, 0, 5000),
                                 v = rnorm(m)),
                      coords = c("x", "y"), crs = 32633)
  vd <- calc_directional_variogram(pts, "v")
  p <- build_directional_variogram_ggplot(vd, title = "T", subtitle = "S")

  expect_s3_class(p, "ggplot")
  built <- ggplot2::ggplot_build(p)
  labs_seen <- levels(built$plot$data$dir)
  expect_true(any(grepl("N-S", labs_seen, fixed = TRUE)))
  expect_true(any(grepl("E-W", labs_seen, fixed = TRUE)))
  expect_null(build_directional_variogram_ggplot(NULL))
  expect_null(build_directional_variogram_ggplot(data.frame(dist = 1, gamma = 1)))
})

# ── Pooled within-locality variograms ("Total (Combined)") ────────────────

pooled_locality <- function(n, x0, mu = 0, seed = 1) {
  with_seed(seed, sf::st_as_sf(data.frame(x = x0 + runif(n, 0, 1500), y = 4e6 + runif(n, 0, 1200),
                                          v = mu + rnorm(n)),
                               coords = c("x", "y"), crs = 32635))
}

# The classical estimator by hand over the pairs of each locality, on classes
# (b_k, b_k+1].
brute_within_variogram <- function(pts_list, b) {
  pairs <- do.call(rbind, lapply(pts_list, function(p) {
    d <- as.matrix(stats::dist(sf::st_coordinates(p)))
    sv <- outer(p$v, p$v, "-")^2 / 2
    lt <- lower.tri(d)
    data.frame(bin = findInterval(d[lt], b, left.open = TRUE), d = d[lt], g = sv[lt])
  }))
  pairs <- pairs[pairs$bin >= 1 & pairs$bin < length(b), ]
  data.frame(np = as.numeric(tapply(pairs$d, pairs$bin, length)),
             dist = as.numeric(tapply(pairs$d, pairs$bin, mean)),
             gamma = as.numeric(tapply(pairs$g, pairs$bin, mean)))
}

test_that("the pooled within-locality variogram is the classical estimator over within-locality pairs", {
  # Localities 50 km apart, then two sharing one area with means 6 apart: in
  # the second layout a pair across localities falls inside the lag range and
  # would add (6^2)/2 to its class.
  for (layout in list(c(500000, 550000), c(500000, 500000))) {
    pl <- list(A = pooled_locality(40, layout[1], seed = 1),
               B = pooled_locality(55, layout[2], mu = 6, seed = 2))
    pv <- pooled_within_variogram(pl, "v")
    half_diag <- vapply(pl, function(p) {
      bb <- sf::st_bbox(p)
      sqrt((bb$xmax - bb$xmin)^2 + (bb$ymax - bb$ymin)^2) / 2
    }, numeric(1))
    b <- seq(0, min(half_diag), length.out = 16)
    expect_equal(attr(pv, "boundaries"), b)
    ref <- brute_within_variogram(pl, b)
    expect_equal(pv$np, ref$np)
    expect_equal(pv$dist, ref$dist, tolerance = 1e-12)
    expect_equal(pv$gamma, ref$gamma, tolerance = 1e-12)
    expect_identical(attr(pv, "localities"), c("A", "B"))
    expect_length(attr(pv, "excluded"), 0)
  }
  # In the shared area every class of all points mixes in cross pairs, whose
  # expected semivariance is (6^2 + 2)/2 = 19 against 1 within a locality.
  every <- gstat::variogram(v ~ 1, rbind(pl$A, pl$B), boundaries = b)
  expect_lt(sum(pv$np), sum(every$np))
  expect_lt(mean(pv$gamma), 2)
  expect_gt(min(every$gamma), 3)
})

test_that("a pooled variogram of one locality is gstat's variogram on the same classes", {
  a <- pooled_locality(40, 500000)
  one <- pooled_within_variogram(list(A = a), "v")
  ref <- gstat::variogram(v ~ 1, a, boundaries = attr(one, "boundaries"))
  expect_identical(one$np, ref$np)
  expect_equal(one$dist, ref$dist, tolerance = 1e-12)
  expect_equal(one$gamma, ref$gamma, tolerance = 1e-12)
  expect_equal(attr(one, "within"), a$v - mean(a$v))
  fit <- robust_vgm_fit(one, attr(one, "within"))
  expect_s3_class(fit, "variogramModel")
})

test_that("a locality below the minimum is left out of the pooled variogram and named", {
  pl <- list(A = pooled_locality(40, 500000, seed = 1), B = pooled_locality(55, 550000, seed = 2),
             C = pooled_locality(POOLED_VGM_MIN_N - 1L, 600000, seed = 3), D = NULL)
  pv <- pooled_within_variogram(pl, "v")
  expect_identical(attr(pv, "localities"), c("A", "B"))
  expect_identical(attr(pv, "excluded"), c("C", "D"))
  kept <- pooled_within_variogram(pl[c("A", "B")], "v")
  expect_identical(pv$np, kept$np)
  expect_identical(pv$gamma, kept$gamma)
  cap <- pooled_within_caption(pv)
  expect_identical(cap$count, "2 localities")
  expect_identical(cap$note, sprintf("Left out (fewer than %d located values): C, D", POOLED_VGM_MIN_N))
  expect_null(pooled_within_caption(pooled_within_variogram(pl["A"], "v"))$note)
  expect_null(pooled_within_variogram(pl[c("C", "D")], "v"))
})

test_that("the pooled directional cones partition the pooled omnidirectional pairs", {
  pl <- list(A = pooled_locality(90, 500000, seed = 1), B = pooled_locality(110, 550000, seed = 2))
  omni <- pooled_within_variogram(pl, "v")
  dv <- pooled_within_directional(pl, "v")
  expect_equal(sort(unique(dv$dir.hor)), c(0, 45, 90, 135))
  b <- attr(omni, "boundaries")
  per_class <- tapply(dv$np, findInterval(dv$dist, b, left.open = TRUE), sum)
  expect_equal(as.numeric(per_class), omni$np)
  expect_identical(attr(dv, "localities"), c("A", "B"))
  expect_s3_class(build_directional_variogram_ggplot(dv, title = "T"), "ggplot")
})

test_that("the practical-range factor is the 95 % correlation-decay solution", {
  # The practical range is where the correlation has decayed to 0.05. Each
  # factor below is that solution for its family, re-derived here rather than
  # copied from the implementation.
  decay_root <- function(rho) {
    stats::uniroot(function(t) rho(t) - 0.05, c(1e-6, 30))$root
  }

  # Spherical reaches the sill exactly at the range.
  expect_equal(.vgm_practical_range_factor("Sph"), 1)

  # Exponential: exp(-t) = 0.05
  expect_equal(.vgm_practical_range_factor("Exp"), 3)
  expect_equal(decay_root(function(t) exp(-t)), 3, tolerance = 2e-3)

  # Gaussian: exp(-t^2) = 0.05
  expect_equal(.vgm_practical_range_factor("Gau"), sqrt(3))
  expect_equal(decay_root(function(t) exp(-t^2)), sqrt(3), tolerance = 2e-3)

  # Matern nu = 0.5 IS the exponential, so it must report the same factor.
  expect_equal(.vgm_practical_range_factor("Mat", 0.5), 3)
  # nu = 1.5 (the screen's fixed smoothness): (1 + t) exp(-t) = 0.05
  # 4.75 is the rounded solution (4.7439), so the tolerance has to admit the
  # rounding the implementation deliberately applies.
  expect_equal(.vgm_practical_range_factor("Mat", 1.5),
               decay_root(function(t) (1 + t) * exp(-t)), tolerance = 2e-3)
  # nu = 2.5: (1 + t + t^2/3) exp(-t) = 0.05
  expect_equal(.vgm_practical_range_factor("Mat", 2.5),
               decay_root(function(t) (1 + t + t^2 / 3) * exp(-t)),
               tolerance = 1e-3)
  # An unrecognised family falls back to the range itself rather than erroring.
  expect_equal(.vgm_practical_range_factor("Lin"), 1)
})

# ── Numeric contract: the variogram itself ─────────────────────────────────

test_that("the empirical variogram is the binned mean of half squared differences", {
  pts <- golden_sf("tiny")
  lags <- calc_scientific_lags(pts)
  v <- gstat::variogram(ph ~ 1, pts, width = lags$width, cutoff = lags$cutoff)

  # Matheron's estimator, computed from every pair without gstat:
  # gamma(h) = (1 / 2N(h)) * sum (z_i - z_j)^2 over the pairs in the bin.
  co <- sf::st_coordinates(pts)
  D <- as.matrix(dist(co))
  S <- outer(pts$ph, pts$ph, "-")^2
  ut <- upper.tri(D)
  d_v <- D[ut]
  s_v <- S[ut]
  bin <- ceiling(d_v / lags$width)
  keep <- d_v <= lags$cutoff & bin >= 1
  f <- factor(bin[keep], levels = seq_len(nrow(v)))

  expect_equal(as.integer(v$np), as.integer(table(f)))
  expect_equal(v$gamma,
               as.numeric(tapply(s_v[keep], f, function(x) 0.5 * mean(x))),
               tolerance = 1e-10)
})

test_that("robust_vgm_fit recovers the model a field was simulated from", {
  # Unconditional simulation from a KNOWN model, then refit. Bands are wide on
  # purpose: one realization of a random field does not reproduce its own
  # generating parameters exactly, and a band tight enough to be violated by
  # sampling noise would be a flaky test rather than a strict one.
  truth <- gstat::vgm(psill = 1, model = "Sph", range = 1200, nugget = 0.2)
  sim <- with_seed(11, {
    g <- expand.grid(x = seq(0, 4000, by = 200), y = seq(0, 4000, by = 200))
    s <- gstat::krige(z ~ 1, locations = NULL,
                      newdata = sf::st_as_sf(g, coords = c("x", "y"), crs = 32635),
                      dummy = TRUE, beta = 0, model = truth, nmax = 25,
                      nsim = 1, debug.level = 0)
    names(s)[1] <- "z"
    s
  })

  lags <- calc_scientific_lags(sim)
  v <- gstat::variogram(z ~ 1, sim, width = lags$width, cutoff = lags$cutoff)
  fit <- suppressWarnings(robust_vgm_fit(v, sim$z))

  nugget <- fit$psill[1]
  total_sill <- sum(fit$psill)
  practical <- fit$range[nrow(fit)] *
    .vgm_practical_range_factor(fit$model[nrow(fit)])

  expect_gt(nugget, 0.05)          # truth 0.2
  expect_lt(nugget, 0.45)
  expect_gt(total_sill, 0.6)       # truth 1.2
  expect_lt(total_sill, 2.4)
  expect_gt(practical, 600)        # truth 1200
  expect_lt(practical, 2400)
})

test_that("the fitted total sill tracks the sample variance", {
  # For a second-order stationary field sampled well past its range, the sill
  # estimates the process variance, so the fitted total sill and the sample
  # variance must be of the same size. A fit that reports a sill orders away
  # from var(z) is not describing this data.
  truth <- gstat::vgm(psill = 1, model = "Exp", range = 400, nugget = 0.1)
  sim <- with_seed(23, {
    g <- expand.grid(x = seq(0, 4000, by = 200), y = seq(0, 4000, by = 200))
    s <- gstat::krige(z ~ 1, locations = NULL,
                      newdata = sf::st_as_sf(g, coords = c("x", "y"), crs = 32635),
                      dummy = TRUE, beta = 0, model = truth, nmax = 25,
                      nsim = 1, debug.level = 0)
    names(s)[1] <- "z"
    s
  })

  lags <- calc_scientific_lags(sim)
  fit <- suppressWarnings(
    robust_vgm_fit(gstat::variogram(z ~ 1, sim, width = lags$width,
                                    cutoff = lags$cutoff), sim$z))
  ratio <- sum(fit$psill) / var(sim$z)
  expect_gt(ratio, 0.6)
  expect_lt(ratio, 1.7)
})

test_that("the four directional variograms pool back to the omnidirectional one", {
  pts <- golden_sf("core", localities = "Yorga")
  lags <- calc_scientific_lags(pts)
  omni <- as.data.frame(gstat::variogram(ph ~ 1, pts, width = lags$width,
                                         cutoff = lags$cutoff))
  dv <- calc_directional_variogram(pts, "ph", lags)
  expect_false(is.null(dv))
  expect_setequal(unique(dv$dir.hor), c(0, 45, 90, 135))

  # The four 45-degree cones partition every pair, and each direction's gamma is
  # the Matheron estimator over its own share of them. So the pair counts add up
  # and the count-weighted mean of the directional gammas has to return the
  # omnidirectional value in every lag bin. A cone that double-counted, dropped
  # or misbinned pairs could not satisfy both at once.
  edges <- c(0, seq_len(nrow(omni)) * lags$width)
  pooled <- do.call(rbind, lapply(split(dv, cut(dv$dist, breaks = edges)),
    function(g) {
      if (nrow(g) == 0) return(NULL)
      data.frame(np = sum(g$np), gamma = sum(g$np * g$gamma) / sum(g$np))
    }))

  expect_equal(as.integer(pooled$np), as.integer(omni$np))
  expect_equal(pooled$gamma, omni$gamma, tolerance = 1e-10)
})


# ── fitted-parameter presentation (ui_formatting.R) ────────────────────────
# The Variogram Parameters card and its export read one builder. Sill is the
# TOTAL sill C0 + C; structural dependency is the partial-sill share of it,
# C / (C0 + C) x 100 — the complement of Cambardella's nugget-to-sill ratio,
# so 100% is a pure spatial structure and 0% a pure nugget.

test_that("vgm_params_row reports the total sill and the partial-sill share", {
  m <- gstat::vgm(psill = 0.75, model = "Sph", range = 400, nugget = 0.25)
  p <- vgm_params_row(m)

  expect_equal(p$model, "Sph")
  expect_equal(p$nugget, 0.25)
  expect_equal(p$sill, 1.00)                     # C0 + C, not the partial sill
  expect_equal(p$range, 400)
  expect_equal(p$practical_range, 400)           # Sph reaches its sill at a
  expect_true(is.na(p$kappa))                    # smoothness is Matern-only
  expect_equal(p$sdep, 75)                       # 0.75 / 1.00 x 100
  expect_true(is.numeric(p$sdep))
})

test_that("vgm_params_row keeps small-unit parameters at full precision", {
  # Total N: a fitted nugget of 2.151e-4 on a total sill of 3.722e-4. At a
  # fixed 4 dp these exported as 2e-04 / 4e-04, and the SD% a reader
  # recomputed from that pair read 50% where the fit says 42.2%.
  m <- gstat::vgm(psill = 3.722e-4 - 2.151e-4, model = "Exp", range = 250,
                  nugget = 2.151e-4)
  p <- vgm_params_row(m)
  expect_equal(p$nugget, 2.151e-4)
  expect_equal(p$sill, 3.722e-4)
  expect_equal(p$sdep, (3.722e-4 - 2.151e-4) / 3.722e-4 * 100)
  expect_equal((p$sill - p$nugget) / p$sill * 100, p$sdep)

  out <- vgm_params_export_df(list(A_act = m))
  expect_equal(out$Nugget, 2.151e-4)
  expect_equal(out$Sill, 3.722e-4)

  # The card shows these very values at four significant digits, so a small
  # nugget reaches the reader with all four digits rather than as 0 (it sits
  # above the 1e-4 switch, so in fixed notation).
  expect_equal(format_sig(p$nugget), "0.0002151")
  expect_equal(format_sig(p$sdep), "42.21")
})

test_that("the practical range makes families comparable; kappa travels with Matern", {
  # 95%-of-sill distance: a (Sph), 3a (Exp), sqrt(3)a (Gau), ~4.75a (Mat nu 1.5)
  mk <- function(model, ...) gstat::vgm(psill = 1, model = model, range = 100, nugget = 0.1, ...)
  expect_equal(vgm_params_row(mk("Exp"))$practical_range, 300)
  expect_equal(vgm_params_row(mk("Gau"))$practical_range, 100 * sqrt(3))
  mat <- vgm_params_row(mk("Mat", kappa = 1.5))
  expect_equal(mat$practical_range, 475)
  expect_equal(mat$kappa, 1.5)
  expect_equal(mat$range, 100)                   # the parameter a itself is kept
  # the definition behind 3a: an exponential structure is at 1 - e^-3 = 95.0%
  # of its sill there
  e <- gstat::variogramLine(gstat::vgm(1, "Exp", 100), dist_vector = 300)
  expect_equal(e$gamma, 1 - exp(-3), tolerance = 1e-12)
})

test_that("vgm_params_row reports a pure-nugget and a nested model", {
  pure <- gstat::vgm(psill = 0.8, model = "Nug", range = 0)
  p <- vgm_params_row(pure)
  expect_equal(p$model, "Nug")
  expect_equal(p$nugget, 0.8)
  expect_equal(p$sill, 0.8)
  expect_true(is.na(p$range))
  expect_equal(p$sdep, 0)

  nested <- gstat::vgm(psill = 0.5, model = "Sph", range = 900,
                       add.to = gstat::vgm(psill = 0.3, model = "Exp", range = 100,
                                           nugget = 0.2))
  q <- vgm_params_row(nested)
  expect_equal(q$nugget, 0.2)
  expect_equal(q$sill, 1.0)
  expect_equal(q$sdep, 80)
  expect_match(q$model, "Sph", fixed = TRUE)
  expect_match(q$model, "Exp", fixed = TRUE)
  # the structure that reaches its sill last: Sph at 900 m, not Exp at 3 x 100 m
  expect_equal(q$range, 900)
  expect_equal(q$practical_range, 900)
})

test_that("vgm_params_row gives a pure nugget 0% and a nugget-free fit 100%", {
  pure_nug <- gstat::vgm(psill = 0, model = "Sph", range = 100, nugget = 1)
  expect_equal(vgm_params_row(pure_nug)$sdep, 0)

  no_nug <- gstat::vgm(psill = 2, model = "Exp", range = 100, nugget = 0)
  expect_equal(vgm_params_row(no_nug)$sdep, 100)

  absent <- vgm_params_row(NULL)
  expect_true(is.na(absent$model))
  expect_true(is.na(absent$sdep))
})

test_that("vgm_params_export_df is one tidy numeric row per fitted target", {
  fits <- list(
    A_act = gstat::vgm(psill = 0.6, model = "Sph", range = 300, nugget = 0.4),
    A_pre = gstat::vgm(psill = 0.9, model = "Exp", range = 500, nugget = 0.1),
    B_act = gstat::vgm(psill = 1.0, model = "Gau", range = 200, nugget = 0.0)
  )
  out <- vgm_params_export_df(fits)

  expect_equal(names(out), c("Locality", "Target", "Model", "Kappa", "Nugget", "Sill",
                             "Range (a)", "Practical Range", "Structural Dep. (%)",
                             "Max Lag", "Sill Resolved"))
  expect_equal(nrow(out), 3)
  expect_equal(out$Locality, c("A", "A", "B"))
  expect_equal(out$Target, c("Actual", "Predicted", "Actual"))
  expect_true(all(vapply(out[4:10], is.numeric, logical(1))))
  # These fits carry no diagnostics (hand-built, not from the screen), so the
  # lag support is unknown and nothing is claimed about the sill.
  expect_true(all(is.na(out$`Max Lag`)))
  expect_true(all(is.na(out$`Sill Resolved`)))
  expect_equal(out$Sill, c(1.0, 1.0, 1.0))
  expect_equal(out$`Range (a)`, c(300, 500, 200))
  # Sph 300 x 1, Exp 500 x 3, Gau 200 x sqrt(3): comparable across the rows
  expect_equal(out$`Practical Range`, c(300, 1500, 200 * sqrt(3)))
  expect_equal(out$`Structural Dep. (%)`, c(60, 90, 100))

  # one locality only
  expect_equal(nrow(vgm_params_export_df(fits, locs = "B")), 1)
  expect_null(vgm_params_export_df(list()))
})

test_that("vgm_params_table_df transposes a named locality and pools the total", {
  fits <- list(
    A_act = gstat::vgm(psill = 0.6, model = "Sph", range = 300, nugget = 0.4),
    A_pre = gstat::vgm(psill = 0.9, model = "Exp", range = 500, nugget = 0.1)
  )

  one <- vgm_params_table_df(fits, "A")
  expect_equal(names(one), c("Param", "Actual", "Predicted"))
  expect_equal(one$Param, c("Model", "Nugget", "Sill", "Range (a)",
                            "Practical Range", "Structural Dep."))
  expect_equal(one$Actual, c("Sph", "0.4", "1", "300", "300", "60%"))
  expect_equal(one$Predicted[5], "1500")         # Exp: 3 x 500

  # a fit with no predicted counterpart drops the column rather than filling NA
  act_only <- vgm_params_table_df(fits["A_act"], "A")
  expect_equal(names(act_only), c("Param", "Actual"))

  # the combined card IS the export frame: one builder, one set of numbers
  total <- vgm_params_table_df(fits, "Total (Combined)")
  expect_equal(total, vgm_params_export_df(fits))

  mat <- vgm_params_table_df(list(M_act = gstat::vgm(1, "Mat", 100, 0.2, kappa = 1.5)), "M")
  expect_equal(mat$Actual[1], "Mat (kappa = 1.5)")

  expect_null(vgm_params_table_df(fits, "Missing"))
})

test_that("a residual-kriging listing says what each variogram describes", {
  fits <- list(
    A_act = gstat::vgm(psill = 0.6, model = "Sph", range = 300, nugget = 0.4),
    B_act = gstat::vgm(psill = 1.0, model = "Gau", range = 200, nugget = 0.0),
    B_pre = gstat::vgm(psill = 0.9, model = "Exp", range = 500, nugget = 0.1)
  )
  of <- c(A_act = VGM_OF_RESIDUALS, B_act = VGM_OF_FALLBACK, B_pre = VGM_OF_RESIDUALS)
  out <- vgm_params_export_df(fits, of = of)
  expect_identical(names(out)[1:4], c("Locality", "Target", "Variogram Of", "Model"))
  expect_identical(out$`Variogram Of`, unname(of))
  # The same rows as the plain listing, which has no such column.
  expect_identical(out[setdiff(names(out), "Variogram Of")], vgm_params_export_df(fits))
  expect_identical(vgm_params_export_df(fits, locs = "B", of = of)$`Variogram Of`,
                   c(VGM_OF_FALLBACK, VGM_OF_RESIDUALS))
  expect_identical(vgm_params_table_df(fits, "Total (Combined)", of = of), out)

  expect_identical(vgm_params_title("RK"), "Residual Variogram Parameters")
  expect_identical(vgm_params_title("RFK"), "Residual Variogram Parameters")
  expect_identical(vgm_params_title("OK"), "Variogram Parameters")
  expect_identical(vgm_params_title(NULL), "Variogram Parameters")
})
