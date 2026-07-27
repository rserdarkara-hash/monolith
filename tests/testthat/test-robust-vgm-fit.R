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
