# spatial_vgm.R - variogram machinery: calc_scientific_lags, robust_vgm_fit
# (candidate screening policy - see spatial-model-conventions), clean_gstat_env
# and suggest_lmc_model. Also hosts the shared RNG sandbox (below). Sourced via
# spatial_helpers.R.


# ── RNG sandbox ─────────────────────────────────────────────────────────────
# ONE implementation of the app's seeding convention, shared by every helper
# that draws random numbers (fold building, kriging LOOCV, the IDW power
# search, Moran's duplicate jitter, class breaks, the governing-factors forest)
# and by classif_helpers.R's .classif_with_seed. It lives at the top of the
# FIRST fragment spatial_helpers.R sources, so every later fragment - and every
# PSOCK worker that sources the master - sees it.
#
# Two-sided by contract: the caller's .Random.seed is restored on exit, or
# REMOVED when the caller had none, so a helper never leaves a seeded stream
# behind for the next computation to inherit.
#
# `expr` is a promise, so it is evaluated in the CALLER's frame: assignments,
# on.exit() and return() inside the block behave exactly as they would without
# the wrapper, and the block's value is the wrapper's value.
with_rng_sandbox <- function(expr) {
  old_seed <- if (exists(".Random.seed", envir = .GlobalEnv, inherits = FALSE)) {
    get(".Random.seed", envir = .GlobalEnv, inherits = FALSE)
  } else {
    NULL
  }
  on.exit({
    if (!is.null(old_seed)) assign(".Random.seed", old_seed, envir = .GlobalEnv)
    else if (exists(".Random.seed", envir = .GlobalEnv, inherits = FALSE)) rm(".Random.seed", envir = .GlobalEnv)
  }, add = TRUE)
  force(expr)
}

# Sandboxed AND seeded - the common case (reproducible draws that leave the
# caller's stream untouched). Sites that seed CONDITIONALLY use
# with_rng_sandbox directly and keep their own set.seed inside the block.
with_seed <- function(seed, expr) {
  with_rng_sandbox({
    set.seed(seed)
    expr
  })
}


suggest_lmc_model <- function(primary_vgm) {
  if (is.null(primary_vgm)) return("Sph")
  m_type <- as.character(primary_vgm$model[primary_vgm$model != "Nug"])
  if (length(m_type) == 0) return("Sph")
  return(m_type[1])
}

# gstat's `range` parameter (`a`) is NOT the practical range. The distance at
# which a family reaches ~95% of its sill is a for Spherical (which reaches the
# sill exactly at a), but 3a for Exponential, sqrt(3)a for Gaussian and ~4.75a
# for Matern with nu = 1.5. Screening candidates on the raw `a` therefore
# applied a window whose meaning changed with the family - an Exponential fit
# whose structure extended three times further than a Spherical one was judged
# by the same number - so the clean-candidate pool was composed on a
# family-dependent criterion rather than on fit quality. Multiplying by the
# factor below puts every candidate on the ground-distance scale before the
# sanity window is applied.
#
# Matern: fit.kappa is deliberately left off (see scientific_guide), so kappa
# stays at the 1.5 the screen starts it on; the other half-integer values are
# listed for the day that changes.
.vgm_practical_range_factor <- function(model, kappa = NA_real_) {
  m <- as.character(model)[1]
  if (identical(m, "Mat")) {
    k <- suppressWarnings(as.numeric(kappa)[1])
    if (isTRUE(abs(k - 0.5) < 1e-8)) return(3)      # Matern nu = 0.5 IS exponential
    if (isTRUE(abs(k - 2.5) < 1e-8)) return(5.92)
    return(4.75)                                    # nu = 1.5 (the screen's value)
  }
  switch(m, "Sph" = 1, "Exp" = 3, "Gau" = sqrt(3), 1)
}

calc_scientific_lags <- function(sf_pts) {
  bbox <- sf::st_bbox(sf_pts)
  max_dist <- as.numeric(sqrt((bbox$xmax - bbox$xmin)^2 + (bbox$ymax - bbox$ymin)^2))
  cutoff <- max_dist / 2
  list(width = cutoff / 15, cutoff = cutoff)
}

# Directional (anisotropy) diagnostic. Same empirical semivariogram estimator
# the fitting path uses, but computed within four angular cones instead of
# pooling every point pair regardless of orientation: if the four curves reach
# their sill at clearly different distances, the spatial structure is
# anisotropic (directional), and an omnidirectional variogram averages that
# away.
#
# gstat measures `alpha` in degrees CLOCKWISE FROM NORTH, so 0 = N-S,
# 45 = NE-SW, 90 = E-W, 135 = NW-SE. tol.hor = 22.5 makes those four cones
# exactly partition the half circle, so each point pair contributes to exactly
# one direction and the four curves are disjoint subsets of the omnidirectional
# one (their pair counts sum to it).
#
# STRICTLY A DIAGNOSTIC: nothing in the prediction path consumes this. Every
# interpolation engine in the app remains omnidirectional / geometrically
# isotropic, so reading anisotropy here does not silently change any surface.
# Coordinates are projected first (via the app's usual auto-UTM rule) because a
# bearing measured in degrees of longitude is not a bearing on the ground.
calc_directional_variogram <- function(pts_sf, value_col, lags = NULL,
                                       angles = c(0, 45, 90, 135),
                                       tol_hor = 22.5, min_n = 10L) {
  if (is.null(pts_sf) || !inherits(pts_sf, "sf") || nrow(pts_sf) < min_n) return(NULL)
  if (!value_col %in% names(pts_sf)) return(NULL)

  d <- pts_sf[!is.na(pts_sf[[value_col]]), ]
  if (nrow(d) < min_n) return(NULL)
  d <- tryCatch(validate_and_project_sf(d), error = function(e) NULL)
  if (is.null(d)) return(NULL)

  if (is.null(lags)) lags <- calc_scientific_lags(d)
  if (!is.finite(lags$cutoff) || lags$cutoff <= 0) return(NULL)

  form <- stats::as.formula(paste0("`", value_col, "` ~ 1"))
  out <- tryCatch(
    gstat::variogram(form, d, width = lags$width, cutoff = lags$cutoff,
                     alpha = angles, tol.hor = tol_hor),
    error = function(e) NULL)
  if (is.null(out) || nrow(out) == 0) return(NULL)
  as.data.frame(out)
}

# Strips the environments gstat attaches to fitted variogram objects (formula
# environment, call attribute) so fits can cross future/worker boundaries
# without dragging their creation environment along.
clean_gstat_env <- function(vgm_obj) {
  if (is.null(vgm_obj)) return(NULL)
  if (is.list(vgm_obj)) {
    if (!is.null(attr(vgm_obj, "formula"))) {
      environment(attr(vgm_obj, "formula")) <- emptyenv()
    }
    if (!is.null(attr(vgm_obj, "call"))) {
      attr(vgm_obj, "call") <- NULL
    }
  }
  return(vgm_obj)
}

robust_vgm_fit <- function(v_emp, v_data) {
  initial_sill <- var(v_data, na.rm=TRUE)
  if (is.na(initial_sill) || initial_sill == 0) initial_sill <- 1

  max_dist <- if (!is.null(v_emp) && nrow(v_emp) > 0) max(v_emp$dist, na.rm = TRUE) else 1.0
  if (is.na(max_dist) || is.infinite(max_dist) || max_dist <= 0) {
    max_dist <- 1.0 # Safe default positive distance fallback
  }

  vgm_diag <- function(n_tried, n_flawed, flawed_winner) {
    list(n_tried = n_tried, n_flawed = n_flawed, flawed_winner = flawed_winner)
  }

  if (is.null(v_emp) || nrow(v_emp) < 5) {
    # Skip fitting to prevent gstat::fit.variogram from crashing R on very small empirical variograms
    fallback <- gstat::vgm(psill = initial_sill * 0.8, "Sph", range = max_dist/2, nugget = initial_sill * 0.2)
    attr(fallback, "is_fallback") <- TRUE
    attr(fallback, "vgm_diagnostics") <- vgm_diag(0L, 0L, FALSE)
    return(fallback)
  }

  # na.rm matches the var()/max() seeds above: gstat::variogram drops empty
  # bins so gamma is finite in practice, but an NA here would make min() NA and
  # crash the `== 0` / `> sill` tests with "missing value where TRUE/FALSE needed".
  initial_nugget <- min(v_emp$gamma, na.rm = TRUE)
  if (initial_nugget == 0) initial_nugget <- max(initial_sill * 1e-6, 1e-6)

  if (initial_nugget > initial_sill) initial_nugget <- initial_sill * 0.9
  initial_psill <- max(initial_sill - initial_nugget, initial_sill * 0.1)

  ranges <- c(max_dist / 10, max_dist / 5, max_dist / 4, max_dist / 2)
  models <- c("Sph", "Exp", "Gau", "Mat") # Added Matern

  # gstat reports singular fits via attr(, "singular") but non-convergence
  # only as a C-level warning, so the warning itself is the detection signal.
  # These are expected while screening candidates and are muffled; anything
  # unrecognized still propagates.
  screening_warning <- "No convergence after|singular model|singular covariance"

  candidates <- list()
  for (m in models) {
    for (r in ranges) {
      start_kappa <- if (m == "Mat") 1.5 else 0.5
      flawed <- FALSE
      f <- tryCatch({
        withCallingHandlers(
          gstat::fit.variogram(v_emp, gstat::vgm(psill = initial_psill, model = m, range = r, nugget = initial_nugget, kappa = start_kappa)),
          warning = function(w) {
            if (grepl(screening_warning, conditionMessage(w))) {
              flawed <<- TRUE
              invokeRestart("muffleWarning")
            }
          }
        )
      }, error = function(e) NULL)
      if (is.null(f)) next
      flawed <- flawed || isTRUE(attr(f, "singular"))
      sse <- attr(f, "SSErr")
      # Sanity window on the PRACTICAL range (ground distance), not on gstat's
      # `a` - see .vgm_practical_range_factor().
      prange <- f$range[2] * .vgm_practical_range_factor(f$model[2], f$kappa[2])
      # The nugget must be non-negative for gamma(h) to be a valid variogram: a
      # negative psill[1] makes gamma(h) < 0 near the origin, the model is not
      # conditionally negative definite, and gstat::krige() answers a system it
      # cannot solve with 100% NA predictions and NO condition raised - which
      # would reach the user as a blank locality behind a variogram panel
      # reporting a clean converged fit. This is ELIGIBILITY, not preference: an
      # invalid model must not be comparable on SSErr at all.
      # Belt-and-braces as of gstat 2.1.5: fit.variogram itself clamps negative
      # sills to zero and refits, but ONLY when the empirical variogram carries
      # attr(, "direct") - which gstat::variogram() sets and every call site
      # here supplies. Keep this test so eligibility does not depend on that
      # attribute surviving, or on the clamp staying in a future gstat.
      in_window <- !is.null(sse) && !is.na(sse) &&
                   prange > (max_dist/100) && prange < max_dist * 2 &&
                   f$psill[2] > 0 &&
                   is.finite(f$psill[1]) && f$psill[1] >= 0
      candidates[[length(candidates) + 1]] <- list(fit = f, sse = sse, flawed = flawed, in_window = in_window)
    }
  }

  n_tried <- length(candidates)
  n_flawed <- sum(vapply(candidates, function(x) x$flawed, logical(1)))
  eligible <- Filter(function(x) x$in_window, candidates)
  clean_pool <- Filter(function(x) !x$flawed, eligible)
  flawed_pool <- Filter(function(x) x$flawed, eligible)

  pick_best <- function(pool) pool[[which.min(vapply(pool, function(x) x$sse, numeric(1)))]]$fit

  best_fit <- NULL
  flawed_winner <- FALSE
  if (length(clean_pool) > 0) {
    best_fit <- pick_best(clean_pool)
  } else if (length(flawed_pool) > 0) {
    # No clean candidate anywhere: still better than the heuristic fallback, but flagged.
    best_fit <- pick_best(flawed_pool)
    flawed_winner <- TRUE
    attr(best_fit, "flawed_winner") <- TRUE
  }

  if (is.null(best_fit)) {
    if (initial_nugget > initial_sill * 0.8) {
      best_fit <- gstat::vgm(psill = initial_sill * 0.05, "Sph", range = max_dist/10, nugget = initial_sill * 0.95)
    } else {
      best_fit <- gstat::vgm(psill = initial_sill * 0.8, "Sph", range = max_dist/2, nugget = initial_sill * 0.2)
    }
    attr(best_fit, "is_fallback") <- TRUE
  }
  attr(best_fit, "vgm_diagnostics") <- vgm_diag(n_tried, n_flawed, flawed_winner)
  return(best_fit)
}
