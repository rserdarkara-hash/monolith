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
  # Reported without touching .Random.seed (a bare RNGkind() has no side
  # effect), so capturing it cannot create the state the next line checks for.
  old_kind <- RNGkind()
  old_seed <- if (exists(".Random.seed", envir = .GlobalEnv, inherits = FALSE)) {
    get(".Random.seed", envir = .GlobalEnv, inherits = FALSE)
  } else {
    NULL
  }
  on.exit({
    if (!is.null(old_seed)) {
      # .Random.seed's first element carries the generator, so this restores
      # the kind along with the stream.
      assign(".Random.seed", old_seed, envir = .GlobalEnv)
    } else {
      # Nothing to restore the kind FROM, and with_seed() names it, so without
      # this the caller is left on Mersenne-Twister. RNGkind(<value>) writes
      # .Random.seed, so set the kind first and remove the variable after, in
      # that order. Numerically inert: this branch runs only where the caller
      # had no random state at all.
      do.call(RNGkind, as.list(old_kind))
      if (exists(".Random.seed", envir = .GlobalEnv, inherits = FALSE)) {
        rm(".Random.seed", envir = .GlobalEnv)
      }
    }
  }, add = TRUE)
  force(expr)
}

# Sandboxed AND seeded - the common case (reproducible draws that leave the
# caller's stream untouched). Sites that seed CONDITIONALLY use
# with_rng_sandbox directly and keep their own set.seed inside the block.
#
# The generator is NAMED here, not inherited. set.seed() keeps whatever RNG
# kind is in force, and a future/furrr worker runs under the L'Ecuyer-CMRG
# stream that `seed = TRUE` installs, so a bare set.seed(12345) drew one stream
# inside a worker and a different one in-process: the app's RFK forests,
# spatial CV blocks, k-means class breaks, Moran jitter, classification tuning
# and governing-factors draws did not reproduce what the same seed produces in a
# script or in the test suite. Naming R's defaults makes one seed mean one
# stream everywhere. The parallel streams are untouched: .Random.seed carries
# the kind in its first element, so restoring it on exit returns the worker to
# its own L'Ecuyer stream, and furrr's per-element streams keep their
# independence. Changing any of these three values moves every seeded result
# in the application.
with_seed <- function(seed, expr) {
  with_rng_sandbox({
    set.seed(seed, kind = "Mersenne-Twister", normal.kind = "Inversion",
             sample.kind = "Rejection")
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

# Two candidates whose weighted least-squares criteria differ by less than
# this relative amount are numerically tied; only then does range resolution
# choose between them. It is a tie rule, not a penalty: a candidate that fits
# the empirical variogram measurably better always wins.
VGM_SSE_TIE_REL <- 1e-8

# ── Fit state ────────────────────────────────────────────────────────────────
# One categorical state per fitted variogram, worst first. It replaces the two
# booleans (`is_fallback`, `flawed_winner`) as the primary signal; both are kept
# as attributes because the CK seed guard and the map banner read them.
#
#   fit_failed         no candidate could be fitted at all, or the empirical
#                      variogram had fewer than 5 bins
#   heuristic_fallback candidates were tried, none was eligible for auto-fit
#   singular_selected  the winner is a singular or non-converged fit
#   range_unresolved   the winner converged, but its practical range lies
#                      outside the span the empirical variogram supports
#   ok                 converged, and its range is inside that span
VGM_FIT_STATUSES <- c("fit_failed", "heuristic_fallback", "singular_selected",
                      "range_unresolved", "ok")

#' The status of a fitted variogram. A model carrying no diagnostics did not
#' come from the candidate screen - it is a model the user applied - and is
#' reported as `ok`: nothing about it is degraded, and its CV is labelled
#' conditional elsewhere.
vgm_fit_status <- function(fit) {
  if (is.null(fit) || NROW(fit) == 0) return("fit_failed")
  s <- attr(fit, "vgm_diagnostics")$status
  if (!is.null(s) && s %in% VGM_FIT_STATUSES) return(s)
  if (isTRUE(attr(fit, "is_fallback"))) return("heuristic_fallback")
  if (isTRUE(attr(fit, "flawed_winner"))) return("singular_selected")
  "ok"
}

#' Plain-language state, for a run-log line or a table cell.
vgm_status_label <- function(status) {
  switch(as.character(status)[1],
         fit_failed = "variogram fit failed",
         heuristic_fallback = "heuristic fallback",
         singular_selected = "singular/non-converged fit selected",
         range_unresolved = "converged fit, range unresolved by lag support",
         "converged fit")
}

#' TRUE when the target this variogram was fitted to carries no usable variance,
#' so its sill and structural dependency are numerical noise rather than
#' estimates. NA (unknown) reads as FALSE, which is the safe default for a
#' supplied model.
vgm_target_degenerate <- function(fit) {
  isTRUE(attr(fit, "vgm_diagnostics")$target_degenerate)
}

# Is the empirical variogram still climbing at the cutoff? Compares the mean of
# the last third of the bins against the third before it, so one noisy bin
# cannot decide it, and calls a rise of more than 5% of the observed gamma span
# a rise. NA when there are too few bins to split, in which case the
# non-stationarity advisory does not fire.
.vgm_still_rising <- function(v_emp) {
  if (is.null(v_emp) || !"gamma" %in% names(v_emp)) return(NA)
  g <- as.numeric(v_emp$gamma)
  n <- length(g)
  k <- max(2L, floor(n / 3))
  if (n < 2L * k) return(NA)
  tail_mean <- mean(utils::tail(g, k), na.rm = TRUE)
  prev_mean <- mean(g[seq(n - 2L * k + 1L, n - k)], na.rm = TRUE)
  span <- suppressWarnings(diff(range(g, na.rm = TRUE)))
  if (!isTRUE(is.finite(span)) || span <= 0 ||
      !is.finite(tail_mean) || !is.finite(prev_mean)) return(NA)
  (tail_mean - prev_mean) > 0.05 * span
}

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

#' The winner of one candidate pool (`list(fit, sse, range_resolved, ...)`
#' entries). The fitting criterion decides; range resolution only breaks a tie:
#' when the lowest-SSErr candidate is unresolved and a resolved candidate's
#' SSErr is numerically equal to it (relative difference within
#' VGM_SSE_TIE_REL), the resolved one is taken. Never a penalty on a better fit.
.vgm_pick_best <- function(pool) {
  sse <- vapply(pool, function(x) x$sse, numeric(1))
  i <- which.min(sse)
  if (!isTRUE(pool[[i]]$range_resolved)) {
    resolved <- vapply(pool, function(x) isTRUE(x$range_resolved), logical(1))
    tie <- which(resolved & sse <= sse[i] * (1 + VGM_SSE_TIE_REL))
    if (length(tie)) i <- tie[which.min(sse[tie])]
  }
  pool[[i]]
}

#' Whether a fitted candidate is eligible for automated selection. The first
#' four rules require a finite comparison criterion and a mathematically valid,
#' finite two-component variogram. The final rule is Monolith's numerical-
#' safety policy for smooth-origin families; a zero-nugget Gaussian or Matern
#' model is mathematically valid, but is not auto-selected in this workflow.
.vgm_autofit_eligible <- function(fit, sse, practical_range) {
  smooth_share <- vgm_smooth_nugget_share(fit)
  length(sse) == 1L && is.finite(sse) &&
    is.finite(fit$psill[2]) && fit$psill[2] > 0 &&
    is.finite(practical_range) && practical_range > 0 &&
    is.finite(fit$psill[1]) && fit$psill[1] >= 0 &&
    !isTRUE(smooth_share <= 1e-8)
}

#' Automated variogram fit. Screens 4 families (Sph, Exp, Gau, Mat with
#' nu = 1.5) x 4 starting ranges with gstat::fit.variogram.
#'
#' Two separate questions are asked of every candidate, and they must not be
#' merged again:
#'
#'  * AUTO-FIT ELIGIBILITY (five rules) requires a finite fitting criterion, a
#'    finite positive partial sill, a finite positive practical range, a finite
#'    non-negative nugget, and a nugget above zero for a parabolic-origin
#'    family. The parameter rules enforce mathematical validity; the last rule
#'    is Monolith's numerical-safety policy. An ineligible candidate is not
#'    comparable on SSErr in the automated search.
#'  * IDENTIFIABILITY (`range_resolved`) is a DIAGNOSTIC: whether the practical
#'    range falls between max lag / 100 and 2 x max lag, i.e. inside the span
#'    the empirical variogram can speak to. It never ranks candidates: a
#'    converged fit that describes the empirical variogram better is not
#'    beaten by a poorer one merely because the poorer one's range lies inside
#'    that span. What an unresolved winner CLAIMS is qualified downstream
#'    (status, sill_resolved) instead.
#'
#' Selection, in order:
#'   1. converged eligible candidates: the lowest SSErr wins; range resolution
#'      only breaks a tie (a resolved candidate whose SSErr is numerically
#'      equal to the winner's is preferred);
#'   2. only if no eligible candidate converged: singular / non-converged
#'      eligible
#'      candidates, lowest SSErr, same tie rule;
#'   3. only if no eligible candidate exists at all: the heuristic.
#'
#' Returns a vgm carrying attr "vgm_diagnostics" (see vgm_diag below), plus
#' "flawed_winner" for a singular/non-converged winner, or "is_fallback" for
#' the heuristic Spherical model used when nothing is eligible or the empirical
#' variogram has fewer than 5 bins.
robust_vgm_fit <- function(v_emp, v_data) {
  initial_sill <- var(v_data, na.rm=TRUE)
  if (is.na(initial_sill) || initial_sill == 0) initial_sill <- 1

  max_dist <- if (!is.null(v_emp) && nrow(v_emp) > 0) max(v_emp$dist, na.rm = TRUE) else 1.0
  if (is.na(max_dist) || is.infinite(max_dist) || max_dist <= 0) {
    max_dist <- 1.0 # Safe default positive distance fallback
  }
  # The span the empirical variogram actually covers. NA when there is no
  # empirical variogram to speak to, so nothing downstream claims support the
  # data never provided.
  max_lag <- if (!is.null(v_emp) && nrow(v_emp) > 0) suppressWarnings(max(v_emp$dist, na.rm = TRUE)) else NA_real_
  if (!isTRUE(is.finite(max_lag))) max_lag <- NA_real_
  still_rising <- .vgm_still_rising(v_emp)
  target_degenerate <- .is_degenerate_covariate(v_data)

  # Everything the UI needs to say what this fit is and what may be claimed
  # about it, computed here because this is where the empirical variogram is in
  # hand. `sill_resolved` is a STRICTER test than `range_resolved` (which
  # admits up to 2 x max lag) and a different question: the status says how
  # well the winner's range is supported, `sill_resolved` what may be CLAIMED
  # about its sill.
  vgm_diag <- function(n_tried, n_flawed, flawed_winner, status, fit = NULL) {
    prange <- if (is.null(fit) || NROW(fit) < 2) NA_real_ else
      fit$range[2] * .vgm_practical_range_factor(fit$model[2], fit$kappa[2])
    sill_resolved <- if (is.na(prange) || is.na(max_lag)) NA else isTRUE(prange <= max_lag)
    list(n_tried = n_tried, n_flawed = n_flawed, flawed_winner = flawed_winner,
         status = status,
         practical_range = prange,
         max_lag = max_lag,
         sill_resolved = sill_resolved,
         # Which end of the window the range fell outside, for the banner.
         range_side = if (!identical(status, "range_unresolved") || is.na(prange)) NA_character_
                      else if (prange <= max_dist / 100) "below" else "beyond",
         # A sill the lags never reached AND an empirical variogram still
         # climbing at the cutoff. Both, because either alone is weak.
         trend_suspected = identical(sill_resolved, FALSE) && isTRUE(still_rising),
         target_degenerate = target_degenerate)
  }

  if (is.null(v_emp) || nrow(v_emp) < 5) {
    # Skip fitting to prevent gstat::fit.variogram from crashing R on very small empirical variograms
    fallback <- gstat::vgm(psill = initial_sill * 0.8, "Sph", range = max_dist/2, nugget = initial_sill * 0.2)
    attr(fallback, "is_fallback") <- TRUE
    # No fit to interpret: practical_range and sill_resolved stay NA rather
    # than describing the heuristic's own invented range as data-supported.
    attr(fallback, "vgm_diagnostics") <- vgm_diag(0L, 0L, FALSE, "fit_failed")
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
      # A zero-nugget Gaussian or Matern is mathematically valid, but Monolith
      # does not auto-select it: in this workflow such smooth-origin fits have
      # produced near-singular systems and predictions far outside the data
      # range. On the reference data the worst overshoot was 1990 times the
      # observed span. Manual models remain available and are warned, not
      # rejected.
      # A non-positive or non-finite range is not a variogram either: gamma(h)
      # is undefined or decreasing. The old window excluded these implicitly
      # (its lower bound is positive), so moving the window out of eligibility
      # would have let a fit with a negative range win the selection - gstat
      # returns them on non-converged fits.
      valid <- .vgm_autofit_eligible(f, sse, prange)
      # Identifiability, a diagnostic, never a ranking - see the header. Fails
      # at BOTH ends:
      # above 2 x max lag the sill is extrapolated beyond the observed support;
      # below max lag / 100 the range is under the application's effective
      # short-range resolution threshold and behaves as near-pure nugget.
      range_resolved <- prange > (max_dist/100) && prange < max_dist * 2
      candidates[[length(candidates) + 1]] <- list(fit = f, sse = sse, flawed = flawed,
                                                   valid = valid, range_resolved = range_resolved)
    }
  }

  n_tried <- length(candidates)
  n_flawed <- sum(vapply(candidates, function(x) x$flawed, logical(1)))
  valid_pool <- Filter(function(x) x$valid, candidates)
  converged <- Filter(function(x) !x$flawed, valid_pool)
  not_converged <- Filter(function(x) x$flawed, valid_pool)

  best_fit <- NULL
  status <- NULL
  if (length(converged) > 0) {
    win <- .vgm_pick_best(converged)
    best_fit <- win$fit
    # Converged either way; an unresolved range is used, and what it claims
    # about the sill is qualified downstream (sill_resolved), not discarded.
    status <- if (win$range_resolved) "ok" else "range_unresolved"
  } else if (length(not_converged) > 0) {
    # No eligible candidate converged: still better than a guess, but flagged.
    best_fit <- .vgm_pick_best(not_converged)$fit
    status <- "singular_selected"
    attr(best_fit, "flawed_winner") <- TRUE
  }

  if (is.null(best_fit)) {
    status <- if (n_tried == 0L) "fit_failed" else "heuristic_fallback"
    if (initial_nugget > initial_sill * 0.8) {
      best_fit <- gstat::vgm(psill = initial_sill * 0.05, "Sph", range = max_dist/10, nugget = initial_sill * 0.95)
    } else {
      best_fit <- gstat::vgm(psill = initial_sill * 0.8, "Sph", range = max_dist/2, nugget = initial_sill * 0.2)
    }
    attr(best_fit, "is_fallback") <- TRUE
    attr(best_fit, "vgm_diagnostics") <- vgm_diag(n_tried, n_flawed, FALSE, status)
    return(best_fit)
  }
  attr(best_fit, "vgm_diagnostics") <- vgm_diag(n_tried, n_flawed,
                                                identical(status, "singular_selected"),
                                                status, best_fit)
  return(best_fit)
}
