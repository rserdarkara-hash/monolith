# test-robust-vgm-fit.R — candidate screening, diagnostics, and selection
# policy for robust_vgm_fit.
# Spec: docs/superpowers/specs/2026-07-04-robust-vgm-fit-convergence-design.md
# Pinned values captured 2026-07-04 via app-context discovery (see plan).
# NOTE: pinned psill/range values are gstat-version-anchored fit.variogram
# outputs; after a gstat upgrade a mismatch is a fixture refresh, not a regression.

test_that("candidate screening emits no warnings on hostile data", {
  h <- make_hostile_vgm_input(seed = 1) # emits 17 warnings before this change
  expect_no_warning(robust_vgm_fit(h$v_emp, h$v_data))
})

test_that("returned fit carries the vgm_diagnostics contract", {
  h <- make_hostile_vgm_input(seed = 1)
  fit <- suppressWarnings(robust_vgm_fit(h$v_emp, h$v_data))
  d <- attr(fit, "vgm_diagnostics")
  expect_type(d, "list")
  expect_named(d, c("n_tried", "n_flawed", "flawed_winner"), ignore.order = TRUE)
  expect_identical(d$n_tried, 16L)
  expect_identical(d$n_flawed, 16L)
  expect_true(is.logical(d$flawed_winner))
})

test_that("diagnostics present on the tiny-variogram early return", {
  fit <- robust_vgm_fit(NULL, rnorm(10))
  d <- attr(fit, "vgm_diagnostics")
  expect_identical(d$n_tried, 0L)
  expect_identical(d$n_flawed, 0L)
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
  # Discovery (2026-07-04): pure-SSErr selection picks a non-converged Gau
  # for this fixture; clean preference must select the converged Mat.
  pts <- make_test_points(30)
  lags <- calc_scientific_lags(pts)
  v_emp <- gstat::variogram(v ~ 1, pts, width = lags$width, cutoff = lags$cutoff)
  fit <- robust_vgm_fit(v_emp, pts$v)
  expect_identical(as.character(fit$model[2]), "Mat")
  expect_equal(fit$psill[1], 0, tolerance = 1e-6)
  expect_equal(fit$psill[2], 114.613972, tolerance = 1e-3)
  expect_equal(fit$range[2], 32.241131, tolerance = 1e-3)
  expect_false(isTRUE(attr(fit, "flawed_winner")))
  expect_gt(attr(fit, "vgm_diagnostics")$n_flawed, 0)
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
