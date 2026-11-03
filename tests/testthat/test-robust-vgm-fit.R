# robust_vgm_fit(): the profile weighted-least-squares auto-fit, its
# diagnostics and its selection policy. Expected values come from references
# independent of the fit under test: gstat's shapes and criterion, the
# brute-force profile brute_vgm_profile() (helper.R), hand computations, and
# gstat::fit.variogram where it converges cleanly.

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
  # A small-variance variable (Kale total nitrogen: variance 7.2e-4, auto-fit
  # nugget 2.49e-4): fixed decimals would give a sill axis of max 0, step 0.01.
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

# ── The estimator: gstat's shapes and criterion, solved exactly ────────────

test_that("the unit-sill shapes are gstat's, the Matern at any smoothness", {
  x <- exp(seq(log(1e-3), log(50), length.out = 400))
  line <- function(m, kappa = 0.5) {
    gstat::variogramLine(gstat::vgm(1, m, 1, kappa = kappa), dist_vector = x)$gamma
  }
  for (m in c("Sph", "Exp", "Gau")) expect_equal(.vgm_shape(m, x), line(m), tolerance = 1e-12)
  for (k in c(0.5, 1.5, 2.5)) expect_equal(.vgm_shape("Mat", x, k), line("Mat", k), tolerance = 1e-12)
})

test_that("the auto-fit minimises gstat's own criterion", {
  # gstat's fit of each automatic family reports SSErr = sum N/h^2 (gamma -
  # model)^2 written with the shapes above, and the auto-fit reports that same
  # criterion for the model it returns.
  pts <- make_test_points(60)
  lags <- calc_scientific_lags(pts)
  emp <- gstat::variogram(v ~ 1, pts, width = lags$width, cutoff = lags$cutoff)
  w <- emp$np / emp$dist^2
  for (m in VGM_AUTOFIT_FAMILIES) {
    fit <- suppressWarnings(gstat::fit.variogram(emp, manual_vgm(80, m, 300, 20)))
    model_gamma <- fit$psill[1] + fit$psill[2] * .vgm_shape(m, emp$dist / fit$range[2])
    expect_equal(sum(w * (emp$gamma - model_gamma)^2), attr(fit, "SSErr"), tolerance = 1e-12)
  }
  auto <- robust_vgm_fit(emp, pts$v)
  expect_equal(attr(auto, "SSErr"), vgm_weighted_sse(emp, auto), tolerance = 1e-12)
})

test_that("nugget and partial sill solve the non-negative least squares exactly", {
  # At every range of the domain: the KKT conditions of min sum w (g - c0 -
  # c1 r)^2 over c0, c1 >= 0 hold at the returned pair (gradient zero where a
  # parameter is free, non-negative where it sits at 0), and no point of a
  # dense (c0, c1) grid fits better. A constant column is a pure nugget.
  v <- make_mixed_vgm_input()$v_emp
  g <- v$gamma
  w <- v$np / v$dist^2
  a <- exp(seq(log(max(v$dist) / 100), log(10 * max(v$dist)), length.out = 40))
  R <- cbind(do.call(cbind, lapply(VGM_AUTOFIT_FAMILIES, function(m) .vgm_shape(m, outer(v$dist, 1 / a)))), 1)
  nn <- .vgm_nnls_cols(g, R, w)
  tol <- 1e-9 * 2 * sum(w * g)
  active <- character(0)
  for (j in seq_len(ncol(R))) {
    r <- R[, j]
    res <- g - nn$c0[j] - nn$c1[j] * r
    expect_equal(nn$sse[j], sum(w * res^2), tolerance = 1e-12)
    expect_gte(nn$c0[j], 0)
    expect_gte(nn$c1[j], 0)
    d0 <- -2 * sum(w * res)
    d1 <- -2 * sum(w * res * r)
    if (nn$c0[j] > 0) expect_lt(abs(d0), tol) else expect_gt(d0, -tol)
    if (nn$c1[j] > 0) expect_lt(abs(d1), tol) else expect_gt(d1, -tol)
    active <- c(active, if (nn$c0[j] > 0 && nn$c1[j] > 0) "free" else if (nn$c1[j] > 0) "c0 = 0" else "c1 = 0")
  }
  expect_true(all(c("free", "c0 = 0", "c1 = 0") %in% active))   # every branch exercised
  expect_equal(nn$c0[ncol(R)], sum(w * g) / sum(w))
  expect_identical(nn$c1[ncol(R)], 0)

  cg <- seq(0, 2 * max(g), length.out = 301)
  for (j in round(seq(1, ncol(R) - 1, length.out = 9))) {
    grid_min <- min(vapply(cg, function(c1) min(colSums(w * outer(g - c1 * R[, j], cg, "-")^2)),
                           numeric(1)))
    expect_lte(nn$sse[j], grid_min * (1 + 1e-12))
  }
})

test_that("each family's fit is the global minimum over its range domain", {
  # Brute force over 5,000 log-spaced ranges of the same domain
  # (brute_vgm_profile()). The fit returned is the representative of the ranges
  # fitting within VGM_SSE_TIE_REL of the minimum; at a domain bound, where the
  # criterion is still falling, that set's midpoint lies half the band above the
  # minimum, so the band is the tolerance.
  inputs <- c(lapply(golden_vgm_pairs("core"), `[[`, "v_emp"),
              list(trend = make_trend_vgm_input()$v_emp, mixed = make_mixed_vgm_input()$v_emp),
              lapply(stats::setNames(1:12, paste0("hostile", 1:12)),
                     function(s) make_hostile_vgm_input(seed = s)$v_emp))
  for (k in names(inputs)) {
    v <- inputs[[k]]
    ref <- brute_vgm_profile(v)
    for (m in VGM_AUTOFIT_FAMILIES) {
      f <- .vgm_profile_family(v$dist, v$gamma, v$np / v$dist^2, m, max(v$dist) / 100,
                               VGM_RANGE_CEILING_MULT * max(v$dist))
      expect_lte(f$sse, ref[[m]]$sse * (1 + VGM_SSE_TIE_REL), label = paste(k, m))
      expect_gte(f$n_basins, 1L)
    }
  }
})

test_that("each family's fit is gstat's where gstat converges cleanly", {
  # The same estimator: on fields simulated from known models, wherever gstat's
  # fit of a family converges without a warning, the profile optimum is no
  # worse and lies within gstat's own convergence tolerance of it.
  for (tr in list(list(model = gstat::vgm(1, "Sph", 1200, 0.2), seed = 11),
                  list(model = gstat::vgm(1, "Exp", 400, 0.1), seed = 23))) {
    sim <- simulate_vgm_field(tr$model, tr$seed)
    lags <- calc_scientific_lags(sim)
    v <- gstat::variogram(z ~ 1, sim, width = lags$width, cutoff = lags$cutoff)
    md <- max(v$dist)
    for (m in VGM_AUTOFIT_FAMILIES) {
      prof <- .vgm_profile_family(v$dist, v$gamma, v$np / v$dist^2, m, md / 100,
                                  VGM_RANGE_CEILING_MULT * md)
      clean <- Filter(Negate(is.null), lapply(md / c(10, 5, 4, 2), function(r) {
        warned <- FALSE
        f <- withCallingHandlers(
          tryCatch(gstat::fit.variogram(v, manual_vgm(0.8 * var(sim$z), m, r, 0.2 * var(sim$z))),
                   error = function(e) NULL),
          warning = function(w) {
            warned <<- TRUE
            invokeRestart("muffleWarning")
          })
        if (is.null(f) || warned || isTRUE(attr(f, "singular"))) NULL else f
      }))
      expect_gt(length(clean), 0)
      best <- clean[[which.min(vapply(clean, function(f) attr(f, "SSErr"), numeric(1)))]]
      sse <- attr(best, "SSErr")
      tot <- sum(best$psill)
      expect_lte(prof$sse, sse * (1 + 1e-9))
      expect_gte(prof$sse, sse * (1 - 1e-6))
      expect_lt(abs(prof$c0 - best$psill[1]) / tot, 1e-3)
      expect_lt(abs(prof$c1 - best$psill[2]) / tot, 1e-3)
      expect_lt(abs(prof$a / best$range[2] - 1), 1e-3)
    }
  }
})

# ── Reproducibility: the fit is a function of the data ──────────────────────

test_that("the fit does not move with the last bits or the order of the empirical variogram", {
  # Every golden locality and variable. A nudge of gamma by 1e-15 or 1e-13
  # (relative), or permuted bins, leaves the family, the nugget and partial
  # sill (to 1e-6 of the sill) and the range (to 1e-6 relative) unchanged.
  nudge <- function(v, seed, eps) {
    v$gamma <- v$gamma * (1 + eps * with_seed(seed, stats::runif(nrow(v), -1, 1)))
    v
  }
  same <- function(a, b) {
    if (!identical(as.character(a$model), as.character(b$model))) return(FALSE)
    tot <- sum(a$psill)
    all(abs(a$psill - b$psill) <= 1e-6 * tot) &&
      (NROW(a) < 2 || abs(b$range[2] / a$range[2] - 1) <= 1e-6)
  }
  pairs <- golden_vgm_pairs("full")
  expect_gt(length(pairs), 0)
  for (k in names(pairs)) {
    P <- pairs[[k]]
    f0 <- robust_vgm_fit(P$v_emp, P$pts$v)
    for (eps in c(1e-15, 1e-13)) {
      expect_true(same(f0, robust_vgm_fit(nudge(P$v_emp, 1, eps), P$pts$v)), label = paste(k, eps))
    }
    o <- with_seed(2, sample(nrow(P$v_emp)))
    expect_true(same(f0, robust_vgm_fit(P$v_emp[o, ], P$pts$v)), label = paste(k, "permuted"))
  }
})

test_that("the fit is unit-free: rescaling distances and values rescales it and nothing else", {
  # Distances x 1000 and values x 1000 (semivariances x 1e6): the same family
  # and state, sills x 1e6 and the range x 1000, to 1e-9.
  synthetic <- list(trend = make_trend_vgm_input(), mixed = make_mixed_vgm_input(),
                    noise = make_hostile_vgm_input(seed = 1))
  inputs <- c(lapply(golden_vgm_pairs("core"), function(P) list(v = P$v_emp, z = P$pts$v)),
              lapply(synthetic, function(h) list(v = h$v_emp, z = h$v_data)))
  for (k in names(inputs)) {
    I <- inputs[[k]]
    f0 <- robust_vgm_fit(I$v, I$z)
    v2 <- I$v
    v2$dist <- v2$dist * 1e3
    v2$gamma <- v2$gamma * 1e6
    f2 <- robust_vgm_fit(v2, I$z * 1e3)
    expect_identical(as.character(f2$model), as.character(f0$model), label = k)
    expect_identical(vgm_fit_status(f2), vgm_fit_status(f0), label = k)
    expect_equal(f2$psill / 1e6, f0$psill, tolerance = 1e-9, label = k)
    if (NROW(f0) > 1) expect_equal(f2$range[2] / 1e3, f0$range[2], tolerance = 1e-9, label = k)
  }
})

test_that("the auto-fit raises no condition, even on hostile inputs", {
  for (s in 1:12) {
    h <- make_hostile_vgm_input(seed = s)
    expect_no_condition(robust_vgm_fit(h$v_emp, h$v_data))
  }
  h <- make_trend_vgm_input()
  expect_no_condition(robust_vgm_fit(h$v_emp, h$v_data))
})

test_that("a PSOCK worker that sources only the model code fits the same variogram", {
  # The worker contract: every worker sources spatial_helpers.R and nothing
  # else from the app, so the auto-fit must be complete inside its fragments.
  # parallelly, as the app: its workers get this session's library paths.
  root <- normalizePath(file.path(testthat::test_path(), "..", ".."), mustWork = TRUE)
  h <- make_mixed_vgm_input()
  cl <- parallelly::makeClusterPSOCK(1L)
  on.exit(parallel::stopCluster(cl), add = TRUE)
  remote <- parallel::clusterCall(cl, function(root, v_emp, v_data) {
    suppressPackageStartupMessages({
      library(sf)
      library(gstat)
      library(dplyr)
    })
    source(file.path(root, "spatial_helpers.R"), local = FALSE)
    robust_vgm_fit(v_emp, v_data)
  }, root, h$v_emp, h$v_data)[[1]]
  expect_identical(remote, robust_vgm_fit(h$v_emp, h$v_data))
})

# ── Selection ───────────────────────────────────────────────────────────────

test_that("the lowest criterion wins whatever its range: range resolution never ranks", {
  # On every golden pair the auto-fit returns the eligible family with the
  # lowest brute-force criterion, and a pure nugget exactly when no family is
  # eligible.
  for (P in golden_vgm_pairs("full")) {
    ref <- Filter(function(x) x$admissible && !x$smooth_zero_nugget, brute_vgm_profile(P$v_emp))
    fit <- robust_vgm_fit(P$v_emp, P$pts$v)
    expect_equal(identical(vgm_fit_status(fit), "pure_nugget"), !length(ref))
    if (!length(ref)) next
    sse <- vapply(ref, `[[`, numeric(1), "sse")
    expect_lte(attr(fit, "SSErr"), min(sse) * (1 + VGM_SSE_TIE_REL))
    if (sum(sse <= min(sse) * (1 + VGM_SSE_TIE_REL)) == 1L) {
      expect_identical(as.character(fit$model[2]), names(ref)[which.min(sse)],
                       label = paste(P$locality, P$var))
    }
  }

  # Built so a resolved-first rule would take a measurably poorer fit: an
  # exponential structure with a practical range of 3 x the longest lag and a
  # 1% alternating wiggle. Its own family fits best, beyond the lag support;
  # Spherical and Matern optima have resolved ranges and fit far worse.
  h <- seq(100, 1500, by = 100)
  gam <- 0.1 + (1 - exp(-h / 1500)) * (1 + 0.01 * rep(c(1, -1), length.out = 15))
  v <- data.frame(np = rep(50, 15), dist = h, gamma = gam, dir.hor = 0, dir.ver = 0, id = factor("var1"))
  class(v) <- c("gstatVariogram", "data.frame")
  ref <- brute_vgm_profile(v)
  sse <- vapply(ref, `[[`, numeric(1), "sse")
  expect_identical(names(which.min(sse)), "Exp")                                  # the premises
  expect_false(ref$Exp$resolved)
  expect_true(ref$Sph$resolved && ref$Mat$resolved && !ref$Mat$smooth_zero_nugget)
  expect_gt(min(sse[c("Sph", "Mat")]) / sse[["Exp"]], 2)
  fit <- robust_vgm_fit(v, gam)
  expect_identical(as.character(fit$model[2]), "Exp")
  expect_identical(vgm_fit_status(fit), "range_unresolved")
  expect_identical(attr(fit, "vgm_diagnostics")$range_side, "beyond")
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

test_that("inside the tie band the family order decides, never rounding", {
  # Two resolved candidates within the band: the first in family order wins,
  # whichever reports the lower criterion, so a last-bit change cannot swap them.
  sph <- list(fit = manual_vgm(2, "Sph", 300, 0.5), sse = 1e-4 * (1 + 1e-10), range_resolved = TRUE)
  exp_ <- list(fit = manual_vgm(2, "Exp", 100, 0.5), sse = 1e-4, range_resolved = TRUE)
  expect_identical(as.character(.vgm_pick_best(list(sph, exp_))$fit$model[2]), "Sph")
  sph$sse <- 1e-4 * (1 - 1e-10)
  expect_identical(as.character(.vgm_pick_best(list(sph, exp_))$fit$model[2]), "Sph")
  # Outside the band the criterion decides.
  sph$sse <- 1.01e-4
  expect_identical(as.character(.vgm_pick_best(list(sph, exp_))$fit$model[2]), "Exp")
})

test_that("a trend field's fit is unresolved beyond the lags, at the search limit, with the trend hint", {
  h <- make_trend_vgm_input()
  ref <- brute_vgm_profile(h$v_emp)
  # The premise: every family's best range is the ceiling, and the best of all
  # is a Matern at a zero nugget, which the rule refuses.
  expect_true(all(vapply(ref, function(x) x$p, numeric(1)) >= 0.999 * VGM_RANGE_CEILING_MULT * max(h$v_emp$dist)))
  expect_true(ref$Mat$smooth_zero_nugget)
  el <- Filter(function(x) x$admissible && !x$smooth_zero_nugget, ref)
  sse <- vapply(el, `[[`, numeric(1), "sse")

  fit <- robust_vgm_fit(h$v_emp, h$v_data)
  d <- attr(fit, "vgm_diagnostics")
  expect_identical(as.character(fit$model[2]), names(el)[which.min(sse)])
  expect_identical(d$status, "range_unresolved")
  expect_identical(d$range_side, "beyond")
  expect_true(d$range_at_ceiling)
  expect_equal(d$practical_range, VGM_RANGE_CEILING_MULT * d$max_lag, tolerance = 1e-6)
  # Used, but not presented as identified: the sill lies beyond the cutoff and
  # the empirical variogram is still climbing there.
  expect_false(d$sill_resolved)
  expect_true(d$trend_suspected)
})

test_that("the better-fitting unresolved Matern wins on the mixed field", {
  h <- make_mixed_vgm_input()
  ref <- Filter(function(x) x$admissible && !x$smooth_zero_nugget, brute_vgm_profile(h$v_emp))
  sse <- vapply(ref, `[[`, numeric(1), "sse")
  expect_identical(names(ref)[which.min(sse)], "Mat")                  # the premise
  expect_gt(min(sse[names(sse) != "Mat"]) / sse[["Mat"]], 1.5)        # a real gap, not a tie

  fit <- robust_vgm_fit(h$v_emp, h$v_data)
  d <- attr(fit, "vgm_diagnostics")
  expect_identical(as.character(fit$model[2]), "Mat")
  expect_equal(attr(fit, "SSErr"), sse[["Mat"]], tolerance = VGM_SSE_TIE_REL)
  expect_identical(d$status, "range_unresolved")
  expect_identical(d$range_side, "beyond")
  expect_false(d$range_at_ceiling)
})

# ── The zero-nugget rule (Matern) and the Gaussian ─────────────────────────

test_that("the auto-fit never returns a Matern at a zero nugget, and the rule changes picks", {
  # make_test_points(30): the best fit of all is a Matern at nugget 0; the
  # auto-fit refuses it and returns the best eligible family.
  pts <- make_test_points(30)
  lags <- calc_scientific_lags(pts)
  v_emp <- gstat::variogram(v ~ 1, pts, width = lags$width, cutoff = lags$cutoff)
  ref <- brute_vgm_profile(v_emp)
  sse <- vapply(ref, `[[`, numeric(1), "sse")
  expect_identical(names(which.min(sse)), "Mat")
  expect_true(ref$Mat$smooth_zero_nugget)
  fit <- robust_vgm_fit(v_emp, pts$v)
  el <- setdiff(names(ref), "Mat")
  expect_identical(as.character(fit$model[2]), el[which.min(sse[el])])
  expect_false(isTRUE(vgm_smooth_nugget_share(fit) <= 1e-8))

  # The Gaussian is not auto-selected, and stays a valid manual model.
  expect_false("Gau" %in% VGM_AUTOFIT_FAMILIES)
  expect_null(validate_manual_vgm(psill = 1, nugget = 0.1, range = 300))
  expect_identical(as.character(manual_vgm(1, "Gau", 300, 0.1)$model[2]), "Gau")
})

test_that("on real data the zero-nugget rule changes picks and keeps surfaces in range", {
  # On every golden pair the auto-fit returns no Matern whose equally-fitting
  # set reaches a zero nugget. Pairs whose best fit is a Matern at nugget 0 get
  # another family, and one of them has at least 30 samples, so the rule acts
  # on real data, not only on small sets. (A Matern optimum with a small
  # positive nugget is eligible: smooth, nearly noise-free fields have one, and
  # the 5% advisory flags it.)
  hits <- 0L
  for (P in golden_vgm_pairs("full")) {
    v <- P$v_emp
    fit <- robust_vgm_fit(v, P$pts$v)
    if (identical(as.character(fit$model[2]), "Mat")) {
      expect_gt(attr(fit, "vgm_diagnostics")$nugget_share_interval[1], 1e-8)
    }
    ref <- brute_vgm_profile(v)
    sse <- vapply(ref, function(x) if (x$admissible) x$sse else Inf, numeric(1))
    if (ref[[which.min(sse)]]$smooth_zero_nugget) {
      expect_false(isTRUE(vgm_smooth_nugget_share(fit) <= 1e-8))
      if (nrow(P$pts) >= 30) hits <- hits + 1L
    }
  }
  expect_gt(hits, 0L)

  # Golden case vgm_zero_nugget: the Matern optimum sits at nugget 0 and krigs
  # this variable outside its observed range by more than 10% of the span
  # (below zero, for a variable that cannot be negative); the auto-fit's model
  # stays inside, on the convex hull of the samples.
  case <- golden_case("vgm_zero_nugget")
  g <- golden_sf("full", localities = case$locality)
  g$v <- g[[case$target]]
  g <- dedup_valid_points(g, "v")
  lags <- calc_scientific_lags(g)
  v_emp <- gstat::variogram(v ~ 1, g, width = lags$width, cutoff = lags$cutoff)
  ref <- brute_vgm_profile(v_emp)
  expect_true(ref$Mat$smooth_zero_nugget)
  refused <- manual_vgm(ref$Mat$c1, "Mat", ref$Mat$p / 4.75, ref$Mat$c0)

  hull <- sf::st_convex_hull(sf::st_union(g))
  grid <- sf::st_as_sf(sf::st_make_grid(hull, n = c(40, 40), what = "centers"))
  sf::st_geometry(grid) <- "geometry"
  grid <- grid[lengths(sf::st_intersects(grid, hull)) > 0, ]
  bounds <- range(g$v) + c(-0.1, 0.1) * diff(range(g$v))
  surface <- function(model) range(gstat::krige(v ~ 1, g, grid, model = model, debug.level = 0)$var1.pred)
  left <- surface(refused)
  expect_true(left[1] < bounds[1] || left[2] > bounds[2])
  fit <- robust_vgm_fit(v_emp, g$v)
  expect_false(isTRUE(vgm_smooth_nugget_share(fit) <= 1e-8))
  kept <- surface(fit)
  expect_gte(kept[1], bounds[1])
  expect_lte(kept[2], bounds[2])
  if (min(g$v) >= 0) {
    expect_lt(left[1], 0)
    expect_gte(kept[1], 0)
  }
})

# ── States: pure nugget, failed fits, the stand-in ─────────────────────────

test_that("white noise gets the flat fit: a pure nugget at the weighted mean semivariance", {
  h <- make_hostile_vgm_input(seed = 1)
  ref <- brute_vgm_profile(h$v_emp)
  expect_true(all(vapply(ref, function(x) x$c1 == 0, logical(1))))   # every family is flat
  fit <- robust_vgm_fit(h$v_emp, h$v_data)
  w <- h$v_emp$np / h$v_emp$dist^2
  expect_identical(as.character(fit$model), "Nug")
  expect_equal(fit$psill, sum(w * h$v_emp$gamma) / sum(w), tolerance = 1e-12)
  expect_equal(attr(fit, "SSErr"), sum(w * (h$v_emp$gamma - fit$psill)^2), tolerance = 1e-12)
  d <- attr(fit, "vgm_diagnostics")
  expect_identical(d$status, "pure_nugget")
  expect_identical(d$pure_nugget_cause, "no_structure")
  expect_equal(d$pairs_per_lag, range(h$v_emp$np))
  # No range, so nothing is claimed about one.
  expect_true(is.na(d$practical_range))
  expect_true(is.na(d$sill_resolved))
  expect_true(is.na(d$range_at_ceiling))
  expect_false(isTRUE(attr(fit, "is_fallback")))
  # Kriged, it predicts the sample mean away from the samples.
  grid <- make_test_grid_safe(h$pts, res = 200)
  kr <- gstat::krige(v ~ 1, h$pts, grid, model = fit, debug.level = 0)
  expect_equal(kr$var1.pred, rep(mean(h$v_data), nrow(grid)), tolerance = 1e-10)
})

test_that("a zero empirical variogram is fit_failed with a stand-in that krigs the constant", {
  # A constant target (or a CV fold whose training values are all equal) has no
  # variance to estimate a variogram from; its flat fit would be a zero-sill
  # model gstat cannot krige.
  pts <- make_test_points(20)
  pts$v <- 7.5
  lags <- calc_scientific_lags(pts)
  v_emp <- gstat::variogram(v ~ 1, pts, width = lags$width, cutoff = lags$cutoff)
  expect_true(all(v_emp$gamma == 0))   # the premise
  fit <- robust_vgm_fit(v_emp, pts$v)
  d <- attr(fit, "vgm_diagnostics")
  expect_identical(d$status, "fit_failed")
  expect_true(isTRUE(attr(fit, "is_fallback")))
  expect_gt(sum(fit$psill), 0)
  expect_true(d$target_degenerate)
  grid <- make_test_grid_safe(pts, res = 200)
  kr <- gstat::krige(v ~ 1, pts, grid, model = fit, debug.level = 0)
  expect_equal(kr$var1.pred, rep(7.5, nrow(grid)))
})

test_that("every fit carries the vgm_diagnostics contract", {
  fields <- c("status", "practical_range", "max_lag", "sill_resolved", "range_side",
              "range_at_ceiling", "range_interval", "nugget_share_interval", "nugget_identified",
              "trend_suspected", "pure_nugget_cause", "pairs_per_lag", "target_degenerate")
  h <- make_hostile_vgm_input(seed = 1)
  fits <- list(robust_vgm_fit(NULL, rnorm(10)), robust_vgm_fit(h$v_emp, h$v_data),
               robust_vgm_fit(make_mixed_vgm_input()$v_emp, make_mixed_vgm_input()$v_data),
               robust_vgm_fit(make_hostile_vgm_input(seed = 2)$v_emp, make_hostile_vgm_input(seed = 2)$v_data))
  expect_setequal(vapply(fits, vgm_fit_status, character(1)), VGM_FIT_STATUSES)
  for (f in fits) {
    d <- attr(f, "vgm_diagnostics")
    expect_named(d, fields, ignore.order = TRUE)
    expect_identical(vgm_fit_status(f), d$status)
    # is_fallback, which the CK seed guard reads, marks the stand-in and nothing
    # else; only the stand-in has no criterion.
    expect_equal(isTRUE(attr(f, "is_fallback")), identical(d$status, "fit_failed"))
    expect_equal(is.null(attr(f, "SSErr")), identical(d$status, "fit_failed"))
  }
})

test_that("the four fit states map to the attributes the UI reads", {
  expect_identical(VGM_FIT_STATUSES, c("fit_failed", "pure_nugget", "range_unresolved", "ok"))
  # fit_failed is reachable end to end: fewer than 5 bins.
  h <- make_hostile_vgm_input(seed = 1)
  expect_identical(vgm_fit_status(robust_vgm_fit(h$v_emp[1:3, ], h$v_data)), "fit_failed")
  expect_identical(vgm_fit_status(robust_vgm_fit(NULL, rnorm(10))), "fit_failed")
  fb <- make_mock_vgm()
  attr(fb, "is_fallback") <- TRUE
  expect_identical(vgm_fit_status(fb), "fit_failed")
  # A supplied (manual) model carries no diagnostics and is not degraded.
  expect_identical(vgm_fit_status(make_mock_vgm()), "ok")
  expect_identical(vgm_fit_status(NULL), "fit_failed")

  expect_match(vgm_status_label("range_unresolved"), "range unresolved by lag support")
  expect_match(vgm_status_label("pure_nugget"), "pure nugget")
  expect_no_match(vgm_status_label("ok"), "converged")
})

test_that("only the stand-in and a fit to rounding withhold their kriging variance", {
  # Every fitted state, a pure nugget included, estimates its covariance from
  # the data; an applied manual model is the user's. The stand-in is a seed,
  # and a fit to values constant up to rounding describes rounding.
  h <- make_hostile_vgm_input(seed = 1)
  m <- make_mixed_vgm_input()
  fits <- list(robust_vgm_fit(NULL, rnorm(10)), robust_vgm_fit(h$v_emp, h$v_data),
               robust_vgm_fit(m$v_emp, m$v_data),
               robust_vgm_fit(make_hostile_vgm_input(seed = 2)$v_emp, make_hostile_vgm_input(seed = 2)$v_data))
  for (f in fits) {
    expect_identical(vgm_variance_supported(f), !identical(vgm_fit_status(f), "fit_failed"))
  }
  expect_setequal(vapply(fits, vgm_fit_status, character(1)), VGM_FIT_STATUSES)
  expect_identical(vgm_variance_withheld(fits[[1]]), "stand_in")
  expect_null(vgm_variance_withheld(fits[[2]]))
  # The same variogram fitted, but to values without usable variance.
  noise <- robust_vgm_fit(h$v_emp, h$v_data, target_degenerate = TRUE)
  expect_identical(vgm_fit_status(noise), vgm_fit_status(fits[[2]]))
  expect_identical(vgm_variance_withheld(noise), "noise_floor")
  expect_false(vgm_variance_supported(noise))
  expect_true(vgm_variance_supported(manual_vgm(1, "Sph", 300, 0.2)))
  expect_false(vgm_variance_supported(NULL))
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

test_that("auto-fit eligibility refuses a negative nugget", {
  # gamma(h) < 0 near the origin is not a valid variogram: the model is not
  # conditionally negative definite and gstat::krige() answers with 100% NA
  # predictions and NO condition raised, which reaches the user as a blank
  # locality behind a clean-looking variogram panel. Eligibility must exclude
  # such a candidate, and the non-negative least squares never returns one.
  fit_neg <- gstat::vgm(psill = 2, model = "Sph", range = 300, nugget = -0.05)
  fit_ok  <- gstat::vgm(psill = 2, model = "Sph", range = 300, nugget = 0.05)
  expect_false(.vgm_autofit_eligible(fit_neg, sse = 1, practical_range = 300))
  expect_true(.vgm_autofit_eligible(fit_ok, sse = 1, practical_range = 300))

  pts <- make_test_points(40)
  lags <- calc_scientific_lags(pts)
  v_emp <- gstat::variogram(v ~ 1, pts, width = lags$width, cutoff = lags$cutoff)
  fit <- robust_vgm_fit(v_emp, pts$v)
  expect_true(is.finite(fit$psill[1]) && fit$psill[1] >= 0)
})

test_that("auto-fit eligibility rejects non-finite criteria, structural sills and zero-nugget sets", {
  fit <- gstat::vgm(psill = 2, model = "Sph", range = 300, nugget = 0.05)
  expect_true(.vgm_autofit_eligible(fit, sse = 1, practical_range = 300))
  expect_false(.vgm_autofit_eligible(fit, sse = Inf, practical_range = 300))
  expect_false(.vgm_autofit_eligible(fit, sse = NULL, practical_range = 300))

  infinite_sill <- fit
  infinite_sill$psill[2] <- Inf
  expect_false(.vgm_autofit_eligible(infinite_sill, sse = 1, practical_range = 300))

  # The zero-nugget rule judged over the equally-fitting set: a Matern that fits
  # as well at nugget 0 is refused, whatever nugget its representative carries.
  mat <- manual_vgm(1, "Mat", 300, 0.1)
  expect_true(.vgm_autofit_eligible(mat, sse = 1, practical_range = 1425))
  expect_false(.vgm_autofit_eligible(mat, sse = 1, practical_range = 1425, share_min = 0))
  expect_true(.vgm_autofit_eligible(mat, sse = 1, practical_range = 1425, share_min = 0.05))
  # Spherical and Exponential rise linearly from the origin: the rule does not apply.
  expect_true(.vgm_autofit_eligible(manual_vgm(1, "Sph", 300, 0), sse = 1, practical_range = 300,
                                    share_min = 0))
})

test_that("the fit_failed stand-in claims nothing about range or sill", {
  fit <- robust_vgm_fit(NULL, rnorm(10))
  d <- attr(fit, "vgm_diagnostics")
  expect_identical(d$status, "fit_failed")
  expect_true(is.na(d$practical_range))
  expect_true(is.na(d$sill_resolved))
  expect_true(is.na(d$range_at_ceiling))
  expect_true(is.na(d$max_lag))
  expect_false(d$trend_suspected)
  ex <- vgm_params_export_df(list(L_act = fit))
  expect_true(is.na(ex$`Range At Search Limit`))
})

test_that("an NA, zero-distance or empty empirical bin is ignored", {
  pts <- make_test_points(30)
  lags <- calc_scientific_lags(pts)
  v_emp <- gstat::variogram(v ~ 1, pts, width = lags$width, cutoff = lags$cutoff)
  ref <- robust_vgm_fit(v_emp[-1, ], pts$v)
  bad <- list(function(v) { v$gamma[1] <- NA_real_; v },
              function(v) { v$np[1] <- 0; v },
              function(v) { v$dist[1] <- 0; v })
  for (b in bad) expect_equal(robust_vgm_fit(b(v_emp), pts$v), ref)
})

test_that("diagnostics survive clean_gstat_env (worker serialization path)", {
  h <- make_hostile_vgm_input(seed = 1)
  fit <- suppressWarnings(robust_vgm_fit(h$v_emp, h$v_data))
  cleaned <- clean_gstat_env(fit)
  expect_false(is.null(attr(cleaned, "vgm_diagnostics")))
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

test_that("an exact ridge is represented by the middle of its nugget interval", {
  # Only the first lag lies inside the Spherical range, so the model has two
  # free fitted values: that lag, and a plateau at the weighted mean m2 of the
  # rest. Every range from the zero-nugget point a0, where rho(h1/a0) = m1/m2,
  # up to h2 fits identically, the nugget sliding from 0 to
  # m2 - (m2 - m1)/(1 - rho(h1/h2)). Built so that this two-level step is the
  # best any non-decreasing model can do, hence every family's optimum. The
  # 1e-8 tie band extends the ridge by about 1e-5 at each end.
  h <- seq(100, 1500, by = 100)
  gam <- c(0.80, 1.03, rep(c(0.99, 1.01), length.out = 13))
  np <- rep(50, 15)
  w <- np / h^2
  m1 <- gam[1]
  m2 <- sum(w[-1] * gam[-1]) / sum(w[-1])
  rho <- function(x) ifelse(x < 1, 1.5 * x - 0.5 * x^3, 1)
  # The premises: the first lag lies below the plateau, above the Spherical's
  # value at h1/h2, every leading run of the later lags averages at least m2,
  # and the plateau does not fit the later lags exactly.
  expect_lt(m1, m2)
  expect_gt(m1 / m2, rho(h[1] / h[2]))
  expect_true(all(cumsum(w[-1] * gam[-1]) / cumsum(w[-1]) >= m2 * (1 - 1e-12)))
  expect_gt(sum(w[-1] * (gam[-1] - m2)^2), 0)

  c0_at <- function(a) m2 - (m2 - m1) / (1 - rho(h[1] / a))
  a0 <- stats::uniroot(c0_at, c(h[1] * (1 + 1e-9), h[2]), tol = 1e-14)$root
  c0_hi <- c0_at(h[2])
  a_mid <- stats::uniroot(function(a) c0_at(a) - c0_hi / 2, c(a0, h[2]), tol = 1e-14)$root

  v <- data.frame(np = np, dist = h, gamma = gam, dir.hor = 0, dir.ver = 0, id = factor("var1"))
  class(v) <- c("gstatVariogram", "data.frame")
  fit <- robust_vgm_fit(v, gam)
  d <- attr(fit, "vgm_diagnostics")
  expect_identical(as.character(fit$model[2]), "Sph")
  expect_identical(d$status, "ok")
  expect_false(d$nugget_identified)
  expect_equal(d$range_interval, c(a0, h[2]), tolerance = 1e-4)
  expect_equal(d$nugget_share_interval[2], c0_hi / m2, tolerance = 1e-4)
  expect_lt(d$nugget_share_interval[1], 1e-8)
  expect_equal(fit$psill[1], c0_hi / 2, tolerance = 1e-4)
  expect_equal(fit$range[2], a_mid, tolerance = 1e-4)
})

test_that("a ridge holding one profile point is represented by its nugget midpoint, whatever the last bits", {
  # White noise, 100 points, seed 7: a Spherical exact ridge (nugget 0 to 4.7%
  # of the sill) that holds exactly one point of the range profile. Whether
  # that grid point or Brent's point beside it seeds the search is decided by
  # the last bits; the representative must not depend on it. Along the ridge
  # the total sill is the plateau, so the middle of the nugget interval is the
  # middle of the nugget-share interval (D2).
  h <- make_hostile_vgm_input(n = 100, seed = 7)
  fit <- robust_vgm_fit(h$v_emp, h$v_data)
  d <- attr(fit, "vgm_diagnostics")
  expect_identical(as.character(fit$model[2]), "Sph")                             # the premises
  expect_false(d$nugget_identified)
  grid_p <- exp(seq(log(d$max_lag / 100), log(VGM_RANGE_CEILING_MULT * d$max_lag),
                    length.out = VGM_PROFILE_GRID_N))
  expect_identical(sum(grid_p > d$range_interval[1] & grid_p < d$range_interval[2]), 1L)
  expect_lt(abs(fit$psill[1] / sum(fit$psill) - mean(d$nugget_share_interval)), 1e-5)
  for (s in 1:4) for (eps in c(1e-15, 1e-13)) {
    v <- h$v_emp
    v$gamma <- v$gamma * (1 + eps * with_seed(s, stats::runif(nrow(v), -1, 1)))
    f <- robust_vgm_fit(v, h$v_data)
    expect_lt(max(abs(f$psill - fit$psill)) / sum(fit$psill), 1e-6)
    expect_lt(abs(f$range[2] / fit$range[2] - 1), 1e-6)
  }
})

test_that("a dip just past a flat ridge is found whatever the last bits", {
  # White noise, 100 points, seed 29: the Spherical profile is flat up to the
  # second lag distance h2 (a ridge, nugget 0 to 47%) and dips about 3e-6
  # lower just past it, where a second lag enters the range. Brent's method
  # over the mostly flat bracket found the dip or not by the last bits; the
  # global minimum is the dip.
  h <- make_hostile_vgm_input(n = 100, seed = 29)
  h2 <- sort(h$v_emp$dist)[2]
  ref <- brute_vgm_profile(h$v_emp)
  expect_gt(ref$Sph$p, h2)                                                        # the premise
  expect_lt(ref$Sph$p, 1.05 * h2)
  fit <- robust_vgm_fit(h$v_emp, h$v_data)
  expect_identical(as.character(fit$model[2]), "Sph")
  expect_lte(attr(fit, "SSErr"), ref$Sph$sse * (1 + VGM_SSE_TIE_REL))
  for (s in 1:4) for (eps in c(1e-15, 1e-13)) {
    v <- h$v_emp
    v$gamma <- v$gamma * (1 + eps * with_seed(s, stats::runif(nrow(v), -1, 1)))
    f <- robust_vgm_fit(v, h$v_data)
    expect_lt(max(abs(f$psill - fit$psill)) / sum(fit$psill), 1e-6)
    expect_lt(abs(f$range[2] / fit$range[2] - 1), 1e-6)
  }
})

test_that("tiny empirical variogram fallback is tagged is_fallback", {
  fit_null <- robust_vgm_fit(NULL, rnorm(10))
  expect_true(isTRUE(attr(fit_null, "is_fallback")))
  h <- make_hostile_vgm_input(seed = 1)
  fit_tiny <- robust_vgm_fit(h$v_emp[1:3, ], h$v_data)
  expect_true(isTRUE(attr(fit_tiny, "is_fallback")))
})

test_that("the resolution window is judged on the PRACTICAL range, per family", {
  # gstat's `a` means a different ground distance per family, so the window is
  # applied to the practical range: on `a` it would be ~3x tighter at the low
  # end for Exp than for Sph, and admit an Exp structure three times longer
  # than the data extent at the top.
  expect_equal(.vgm_practical_range_factor("Sph"), 1)
  expect_equal(.vgm_practical_range_factor("Exp"), 3)
  expect_equal(.vgm_practical_range_factor("Gau"), sqrt(3))
  expect_equal(.vgm_practical_range_factor("Mat", 1.5), 4.75)
  # Matern with nu = 0.5 IS the exponential model and must be judged like one.
  expect_equal(.vgm_practical_range_factor("Mat", 0.5), 3)
  # Unknown/nugget-only families fall back to "a is the practical range".
  expect_equal(.vgm_practical_range_factor("Nug"), 1)

  # A structured fit is `ok` exactly when its practical range lies inside
  # (max lag / 100, 2 x max lag), and `range_unresolved` otherwise.
  for (s in 2:12) {
    h <- make_hostile_vgm_input(seed = s)
    fit <- robust_vgm_fit(h$v_emp, h$v_data)
    if (NROW(fit) < 2) next
    d <- attr(fit, "vgm_diagnostics")
    prange <- fit$range[2] * .vgm_practical_range_factor(fit$model[2], fit$kappa[2])
    expect_equal(d$practical_range, prange, tolerance = 1e-12)
    inside <- prange > d$max_lag / 100 && prange < 2 * d$max_lag
    expect_identical(d$status, if (inside) "ok" else "range_unresolved")
  }
})

# ── The map banner, worded for the engine that fitted the variogram ─────────

# A hand-built pure-nugget fit with the diagnostics the auto-fit records.
mock_pure_nugget <- function(cause = "no_structure", trend = FALSE) {
  f <- gstat::vgm(0.4, "Nug", 0)
  attr(f, "vgm_diagnostics") <- list(status = "pure_nugget", practical_range = NA_real_, max_lag = 700,
                                     sill_resolved = NA, range_side = NA_character_,
                                     range_at_ceiling = FALSE, nugget_share_interval = c(1, 1),
                                     nugget_identified = TRUE, trend_suspected = trend,
                                     pure_nugget_cause = cause, pairs_per_lag = c(3, 8),
                                     target_degenerate = FALSE)
  f
}

test_that("build_vgm_warning_html returns NULL when nothing is flagged", {
  expect_null(build_vgm_warning_html(list(A_act = make_mock_vgm())))
  expect_null(build_vgm_warning_html(list()))
})

test_that("build_vgm_warning_html renders the red failure band and the amber pure-nugget band", {
  f_fb <- make_mock_vgm(); attr(f_fb, "is_fallback") <- TRUE
  html <- build_vgm_warning_html(list(LocA_act = f_fb, LocB_pre = mock_pure_nugget(),
                                      LocC_act = make_mock_vgm()))
  expect_match(html, "LocA (actual)", fixed = TRUE)
  expect_match(html, "LocB (predicted)", fixed = TRUE)
  expect_match(html, "default spherical variogram model", fixed = TRUE)
  # The stand-in's variance is withheld, and the band says so.
  expect_match(html, "no standard error or variance is reported", fixed = TRUE)
  expect_match(html, "No spatial structure detected in the variogram", fixed = TRUE)
  expect_no_match(html, "LocC", fixed = TRUE)
})

test_that("build_vgm_warning_html filters by target and strips the suffix", {
  f_fb <- make_mock_vgm(); attr(f_fb, "is_fallback") <- TRUE
  fits <- list(LocA_act = f_fb, LocB_pre = mock_pure_nugget())

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
  expect_no_match(html, "No spatial structure", fixed = TRUE)
  expect_null(build_vgm_warning_html(list(LocB_act = manual_vgm(1, "Mat", 300, 0.2))))
})

test_that("an out-of-window fit gets its own band, not the failure band", {
  f <- gstat::vgm(psill = 2, model = "Exp", range = 300, nugget = 0.5) # practical 900
  attr(f, "vgm_diagnostics") <- list(status = "range_unresolved", practical_range = 900,
                                     max_lag = 700, sill_resolved = FALSE,
                                     range_side = "beyond", trend_suspected = FALSE,
                                     target_degenerate = FALSE)
  html <- build_vgm_warning_html(list(LocA_act = f))
  expect_match(html, "Variogram range not resolved", fixed = TRUE)
  expect_match(html, "Practical range extends beyond sampled lag support for: LocA (actual)", fixed = TRUE)
  expect_no_match(html, "Variogram fit failed", fixed = TRUE)
  expect_no_match(html, "search limit", fixed = TRUE)
  # Convergence is not a notion of this search, so no band speaks of it.
  expect_no_match(html, "converged", fixed = TRUE)

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

test_that("a pure-nugget band follows the engine and names no engine on the residual scale", {
  pn <- mock_pure_nugget()
  for (eng in list(NULL, "OK", "CK")) {
    html <- build_vgm_warning_html(list(LocA_act = pn), engine = eng)
    expect_match(html, "No spatial structure detected in the variogram for: LocA (actual) (3 to 8 pairs per lag)",
                 fixed = TRUE)
    # Not detected is not absent, and the flat fit is a model selection, not a
    # calibrated test.
    expect_match(html, "does not show that no structure exists", fixed = TRUE)
    expect_match(html, "not a significance test", fixed = TRUE)
    # Naming IDW and TPS is not circular under a value-scale engine.
    expect_match(html, "IDW and TPS", fixed = TRUE)
    expect_match(html, "Manual Tuning", fixed = TRUE)
  }
  for (eng in c("RK", "RFK")) {
    html <- build_vgm_warning_html(list(LocA_act = pn), engine = eng)
    # A flat residual fit does not establish that no residual structure exists.
    expect_match(html, "No residual spatial structure detected by the variogram fit for: LocA (actual)",
                 fixed = TRUE)
    expect_match(html, "does not show that the residuals carry no structure", fixed = TRUE)
    expect_no_match(html, "remains", fixed = TRUE)
    expect_match(html, "the trend plus the mean residual", fixed = TRUE)
    expect_no_match(html, "IDW", fixed = TRUE)
    expect_no_match(html, "Regression Kriging", fixed = TRUE)
    expect_no_match(html, "Manual Tuning", fixed = TRUE)
  }

  # A pure nugget left by the zero-nugget rule never claims that no structure
  # was detected: a structured Matern was refused.
  for (eng in c("OK", "RK")) {
    html <- build_vgm_warning_html(list(LocA_act = mock_pure_nugget("zero_nugget_rule")), engine = eng)
    expect_match(html, "Pure nugget used for", fixed = TRUE)
    expect_match(html, "a Mat&eacute;rn at a zero nugget", fixed = TRUE)
    expect_no_match(html, "detected", fixed = TRUE)
  }

  # A pure nugget whose variogram still rises carries the engine-aware trend band.
  rising <- mock_pure_nugget(trend = TRUE)
  expect_match(build_vgm_warning_html(list(LocA_act = rising), engine = "OK"),
               "Sill not observed; possible large-scale trend", fixed = TRUE)
  expect_match(build_vgm_warning_html(list(LocA_act = rising), engine = "RK"),
               "Residual variogram still rising", fixed = TRUE)

  # Under RK or RFK, a locality whose trend fell back to OK holds the measured
  # values' variogram (`of`, as the Variogram Parameters card reads it), so its
  # bands take the value-scale wording beside the residual ones.
  of <- c(A_act = VGM_OF_RESIDUALS, B_act = VGM_OF_FALLBACK)
  html <- build_vgm_warning_html(list(A_act = rising, B_act = rising), engine = "RFK", of = of)
  expect_match(html, "No residual spatial structure detected by the variogram fit for: A (actual).", fixed = TRUE)
  expect_match(html, "No spatial structure detected in the variogram for: B (actual) (3 to 8 pairs per lag)",
               fixed = TRUE)
  expect_match(html, "Residual variogram still rising at the lag cutoff for: A (actual).", fixed = TRUE)
  expect_match(html, "Sill not observed; possible large-scale trend for: B (actual).", fixed = TRUE)
  refused <- mock_pure_nugget("zero_nugget_rule")
  html <- build_vgm_warning_html(list(A_act = refused, B_act = refused), engine = "RK", of = of)
  expect_match(html, "Pure nugget used for the trend residuals of: A (actual).", fixed = TRUE)
  expect_match(html, "Pure nugget used for: B (actual).", fixed = TRUE)
})

test_that("a range at the search limit is a bound under every engine, naming none", {
  f <- manual_vgm(2, "Sph", 7000, 0.5)
  attr(f, "vgm_diagnostics") <- list(status = "range_unresolved", practical_range = 7000,
                                     max_lag = 700, sill_resolved = FALSE, range_side = "beyond",
                                     range_at_ceiling = TRUE, trend_suspected = FALSE,
                                     target_degenerate = FALSE)
  for (eng in list(NULL, "OK", "CK", "RK", "RFK")) {
    html <- build_vgm_warning_html(list(LocA_act = f), engine = eng)
    expect_match(html, "Range at the search limit for: LocA (actual)", fixed = TRUE)
    expect_match(html, paste0("up to ", VGM_RANGE_CEILING_MULT, " &times; the longest lag"), fixed = TRUE)
    expect_match(html, "a bound, not an estimate", fixed = TRUE)
    expect_no_match(html, "Kriging", fixed = TRUE)
    expect_no_match(html, "IDW", fixed = TRUE)
  }
})

test_that("a nugget the data do not identify is reported as an interval, naming the locality", {
  f <- manual_vgm(41.9, "Sph", 295, 7.35)
  attr(f, "vgm_diagnostics") <- list(status = "ok", practical_range = 295, max_lag = 3216,
                                     sill_resolved = TRUE, range_side = NA_character_,
                                     range_at_ceiling = FALSE, range_interval = c(271, 335),
                                     nugget_share_interval = c(0, 0.298), nugget_identified = FALSE,
                                     trend_suspected = FALSE, target_degenerate = FALSE)
  for (eng in list(NULL, "OK", "RK")) {
    html <- build_vgm_warning_html(list(Kale_act = f), engine = eng)
    expect_match(html, "Nugget not identified for: Kale (actual) (nugget 0 to 29.8% of the sill)",
                 fixed = TRUE)
    # Equal criteria on the chosen bins, not a statement of statistical precision.
    expect_match(html, "binned empirical variogram equally well (numerically equivalent fits, not a confidence interval)",
                 fixed = TRUE)
  }
  chr <- .vgm_params_chr(f)
  expect_match(chr[6], "Not reliably identified (70.2 to 100%)", fixed = TRUE)
  expect_match(chr[6], "numerically equivalent fits, not a confidence interval", fixed = TRUE)
  expect_match(chr[5], "Practical ranges from 271 to 335 fit equally well (numerically equivalent fits, not a confidence interval).",
               fixed = TRUE)
  expect_match(vgm_fit_subtitle(f), "nugget not identified: 0 to 29.8% of the sill fit equally well",
               fixed = TRUE)
  ex <- vgm_params_export_df(list(Kale_act = f))
  expect_false(ex$`Nugget Identified`)
  expect_equal(c(ex$`Structural Dep. Low (%)`, ex$`Structural Dep. High (%)`), c(70.2, 100))
  expect_equal(c(ex$`Practical Range Low`, ex$`Practical Range High`), c(271, 335))
  expect_false(ex$`Range At Search Limit`)
})

test_that("banner close button targets its own container, not a fixed id", {
  html <- build_vgm_warning_html(list(L1_act = mock_pure_nugget()))
  expect_match(html, "this.parentElement.style.display", fixed = TRUE)
  expect_no_match(html, "getElementById", fixed = TRUE)
})

test_that("a locality name from the data is escaped in the banner", {
  html <- build_vgm_warning_html(list(`Plot <2> & B_act` = mock_pure_nugget()))
  expect_match(html, "Plot &lt;2&gt; &amp; B (actual)", fixed = TRUE)
  expect_no_match(html, "<2>", fixed = TRUE)
})

test_that("amber-only banner omits the red section", {
  html <- build_vgm_warning_html(list(L1_act = mock_pure_nugget()))
  expect_match(html, "No spatial structure detected", fixed = TRUE)
  expect_no_match(html, "Variogram fit failed", fixed = TRUE)
})

test_that("red-only banner omits the amber section", {
  f_fb <- make_mock_vgm(); attr(f_fb, "is_fallback") <- TRUE
  html <- build_vgm_warning_html(list(L1_act = f_fb))
  expect_match(html, "default spherical variogram model", fixed = TRUE)
  expect_no_match(html, "No spatial structure", fixed = TRUE)
  expect_no_match(html, "var(--mn-warn)", fixed = TRUE)
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
  # At the search limit the range is a bound, and the subtitle says so.
  attr(f, "vgm_diagnostics")$range_at_ceiling <- TRUE
  expect_match(vgm_fit_subtitle(f), "Practical Range: 900 at the search limit, model-extrapolated",
               fixed = TRUE)
  # A Matern names its smoothness; small values keep their digits.
  m <- gstat::vgm(psill = 3.873e-05, model = "Mat", range = 6510, nugget = 0.000172, kappa = 1.5)
  expect_match(vgm_fit_subtitle(m),
               "Fitted: Mat, kappa = 1.5 (Nugget: 0.000172, Partial Sill: 3.873e-05, Range (a): 6510,",
               fixed = TRUE)
  expect_identical(vgm_fit_subtitle(gstat::vgm(0.4, "Nug", 0)), "Fitted: pure nugget (Nugget: 0.4)")
  expect_null(vgm_fit_subtitle(NULL))
  # Never "Fitted" for a model nothing was fitted to: the stand-in of a failed
  # fit, or a model the user applied.
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
    "3" = list(vgm_status = "fit_failed"),
    "7" = list(vgm_status = "pure_nugget"),
    "10" = list(vgm_status = "ok"))
  tb <- .cv_fold_status_table(cv)
  expect_identical(tb$status, c("fit_failed", "pure_nugget", "range_unresolved", "ok"))   # worst first
  expect_identical(tb$n, c(1L, 1L, 1L, 2L))
  expect_identical(tb$folds[tb$status == "ok"], "1, 10")
  expect_identical(tb$folds[tb$status == "range_unresolved"], "2")

  res <- .log_vgm_fold_status(list(cv_obj = cv, log_msg = ""), "OK", "West_Field")
  expect_match(res$log_msg, "West_Field: in 3 of 5 folds the variogram fitted to the training rows was not a resolved structured fit.",
               fixed = TRUE)
  expect_match(res$log_msg, "Fold 2: range unresolved by lag support", fixed = TRUE)
  expect_match(res$log_msg, "Fold 3: variogram fit failed", fixed = TRUE)
  expect_match(res$log_msg, "Fold 7: pure nugget, no structured fit selected", fixed = TRUE)
  # A pure-nugget fold is a finding about its rows, not a degraded fit.
  expect_no_match(res$log_msg, "degraded", fixed = TRUE)

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
  # nu = 1.5 (the auto-fit's fixed smoothness): (1 + t) exp(-t) = 0.05
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
  sim <- simulate_vgm_field(truth, 11)

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
  sim <- simulate_vgm_field(truth, 23)

  lags <- calc_scientific_lags(sim)
  fit <- suppressWarnings(
    robust_vgm_fit(gstat::variogram(z ~ 1, sim, width = lags$width,
                                    cutoff = lags$cutoff), sim$z))
  ratio <- sum(fit$psill) / var(sim$z)
  expect_gt(ratio, 0.6)
  expect_lt(ratio, 1.7)
})

test_that("the four directional variograms pool back to the omnidirectional one", {
  pts <- golden_sf("core", localities = golden_locality("core", "smallest", min_n = 30L))
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
# C / (C0 + C) x 100, the complement of Cambardella's nugget-to-sill ratio,
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
  # Total N: a fitted nugget of 2.151e-4 on a total sill of 3.722e-4. Rounded
  # to 4 dp they would read 2e-04 / 4e-04, and an SD% recomputed from that pair
  # 50% where the fit says 42.2%.
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
                             "Max Lag", "Sill Resolved", "Nugget Identified",
                             "Structural Dep. Low (%)", "Structural Dep. High (%)",
                             "Practical Range Low", "Practical Range High",
                             "Range At Search Limit", "Fit Status"))
  expect_equal(nrow(out), 3)
  expect_identical(out$`Fit Status`, rep("fitted", 3))
  expect_equal(out$Locality, c("A", "A", "B"))
  expect_equal(out$Target, c("Actual", "Predicted", "Actual"))
  expect_true(all(vapply(out[c(4:10, 13:16)], is.numeric, logical(1))))
  # These fits carry no diagnostics (hand-built, not from the auto-fit), so
  # the lag support and the equally-fitting set are unknown and nothing is
  # claimed about the sill or the nugget.
  expect_true(all(is.na(out$`Max Lag`)))
  expect_true(all(is.na(out$`Sill Resolved`)))
  expect_true(all(is.na(out$`Nugget Identified`)))
  expect_true(all(is.na(out$`Practical Range Low`)))
  expect_equal(out$Sill, c(1.0, 1.0, 1.0))
  expect_equal(out$`Range (a)`, c(300, 500, 200))
  # Sph 300 x 1, Exp 500 x 3, Gau 200 x sqrt(3): comparable across the rows
  expect_equal(out$`Practical Range`, c(300, 1500, 200 * sqrt(3)))
  expect_equal(out$`Structural Dep. (%)`, c(60, 90, 100))

  # one locality only
  expect_equal(nrow(vgm_params_export_df(fits, locs = "B")), 1)
  expect_null(vgm_params_export_df(list()))
})

test_that("an exported variogram row says what it is", {
  # Without the status a stand-in's seeds (80% structural dependency) read as
  # a fit; a fit to rounding reports no parameters, as on the card.
  stand_in <- robust_vgm_fit(NULL, rnorm(10))
  noise <- gstat::vgm(psill = 1e-20, model = "Exp", range = 70, nugget = 0)
  attr(noise, "vgm_diagnostics") <- list(status = "ok", max_lag = 700,
                                         sill_resolved = TRUE, target_degenerate = TRUE)
  manual <- stamp_vgm(manual_vgm(2, "Exp", 300, 0.5), "v", "manual")
  h <- make_hostile_vgm_input(seed = 1)
  fitted <- robust_vgm_fit(h$v_emp, h$v_data)
  out <- vgm_params_export_df(list(A_act = stand_in, B_act = noise, C_act = manual, D_act = fitted))
  expect_identical(out$`Fit Status`,
                   c("variogram fit failed", "no usable variance, not estimated",
                     "applied manual model", vgm_status_label(vgm_fit_status(fitted))))
  expect_identical(out$Model[c(1, 3)], c("Sph", "Exp"))
  expect_equal(out$`Structural Dep. (%)`[1], 80)
  expect_true(all(is.na(unlist(out[2, c("Model", "Kappa", "Nugget", "Sill", "Range (a)",
                                        "Practical Range", "Structural Dep. (%)",
                                        "Sill Resolved", "Range At Search Limit")]))))
  expect_equal(out$`Max Lag`[2], 700)
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
