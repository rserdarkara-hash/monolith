# spatial_vgm.R - variogram machinery: calc_scientific_lags, robust_vgm_fit
# (candidate screening policy - see spatial-model-conventions), clean_gstat_env
# and suggest_lmc_model. Sourced via spatial_helpers.R.



suggest_lmc_model <- function(primary_vgm) {
  if (is.null(primary_vgm)) return("Sph")
  m_type <- as.character(primary_vgm$model[primary_vgm$model != "Nug"])
  if (length(m_type) == 0) return("Sph")
  return(m_type[1])
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
      in_window <- !is.null(sse) && !is.na(sse) && f$range[2] > (max_dist/100) && f$range[2] < max_dist * 2 && f$psill[2] > 0
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
