# spatial_vgm.R - variogram machinery: calc_scientific_lags, robust_vgm_fit
# (candidate screening policy - see spatial-model-conventions), clean_gstat_env
# and suggest_lmc_model. Also hosts the shared RNG sandbox (below). Sourced via
# spatial_helpers.R.


# Manual and automatic Mat use the same family: Matern nu = 1.5.
manual_vgm <- function(psill, model, range, nugget) {
  gstat::vgm(psill = psill, model = model, range = range, nugget = nugget,
             kappa = if (identical(model, "Mat")) 1.5 else 0.5)
}

# NULL when a manual model can be kriged, else the reason it cannot. A zero
# total sill leaves no covariance to solve; a pure nugget (psill 0, nugget > 0)
# is a valid, spatially unstructured model.
validate_manual_vgm <- function(psill, nugget, range) {
  vals <- suppressWarnings(as.numeric(c(psill, nugget, range)))
  if (length(vals) != 3 || any(!is.finite(vals))) {
    return("Nugget, partial sill and range must all be finite numbers.")
  }
  if (vals[1] < 0 || vals[2] < 0) return("Nugget and partial sill cannot be negative.")
  if (vals[1] + vals[2] <= 0) {
    return("Nugget + partial sill is 0: the model has no variance, so there is nothing to krige.")
  }
  if (vals[3] <= 0) return("The range must be greater than 0.")
  NULL
}

# gstat fit.method = 7: the criterion shown by Auto-Fit.
vgm_weighted_sse <- function(v_emp, model) {
  line <- gstat::variogramLine(model, dist_vector = v_emp$dist)
  sum(v_emp$np / v_emp$dist^2 * (v_emp$gamma - line$gamma)^2)
}

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


#' Model family for the co-kriging LMC: the first non-nugget structure of the
#' primary variable's fitted variogram, or "Sph" when there is none.
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

# Nugget share of the total sill when the model's structure has a parabolic
# origin (Gaussian, or Matern with nu >= 1), NA for any other model. With little
# or no nugget such a structure makes the kriging covariance matrix
# near-singular (Posa 1989; Ababou et al. 1994) and produces large negative
# kriging weights that place predictions outside the observed range (Deutsch
# 1996). Spherical and Exponential structures rise linearly from the origin and
# are not affected.
vgm_smooth_nugget_share <- function(model) {
  if (is.null(model) || NROW(model) == 0) return(NA_real_)
  mdl <- as.character(model$model)
  st <- which(mdl != "Nug")
  total <- sum(model$psill)
  if (!length(st) || !isTRUE(total > 0)) return(NA_real_)
  smooth <- identical(mdl[st[1]], "Gau") ||
    (identical(mdl[st[1]], "Mat") && isTRUE(model$kappa[st[1]] >= 1))
  if (!smooth) return(NA_real_)
  sum(model$psill[mdl == "Nug"]) / total
}

# A supplied (manual) Gaussian or Matern model with a nugget below this share of
# the total sill is flagged, not changed. Advisory only: on the reference data,
# 5% bounded the overshoot beyond the observed range to 0.02 of its span in
# every locality, while 1% allowed up to 0.5.
VGM_SMOOTH_NUGGET_WARN_SHARE <- 0.05

#' Default empirical-variogram lags: cutoff = half the bounding-box diagonal of
#' the points, split into 15 bins. Returns `list(width, cutoff)` in CRS units.
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

#' Automated variogram fit. Screens 4 families (Sph, Exp, Gau, Mat with
#' nu = 1.5) x 4 starting ranges with gstat::fit.variogram. A candidate is
#' eligible when its practical range lies between max lag / 100 and 2 x max
#' lag, its partial sill is positive, its nugget non-negative, and, for a
#' Gaussian or Matern structure, its nugget positive; the lowest
#' SSErr wins, converged candidates before flawed ones. Returns a vgm carrying
#' attr "vgm_diagnostics", plus "flawed_winner", or "is_fallback" for the
#' heuristic Spherical model used when nothing is eligible or the empirical
#' variogram has fewer than 5 bins.
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
      # A Gaussian or Matern fit whose nugget sits at its lower bound of zero
      # is ineligible: the least-squares fit wanted a negative nugget, i.e. the
      # parabolic origin of the family cannot follow the short-lag rise of the
      # empirical variogram, and without a nugget that origin gives a
      # near-singular kriging system and predictions far outside the data
      # range (vgm_smooth_nugget_share). On the reference data such winners
      # overshot the observed range by up to 1990 times its span.
      smooth_share <- vgm_smooth_nugget_share(f)
      in_window <- !is.null(sse) && !is.na(sse) &&
                   prange > (max_dist/100) && prange < max_dist * 2 &&
                   f$psill[2] > 0 &&
                   is.finite(f$psill[1]) && f$psill[1] >= 0 &&
                   !isTRUE(smooth_share <= 1e-8)
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
