# spatial_vgm.R - variogram machinery (lags, robust_vgm_fit, directional and
# pooled within-locality variograms, LMC model choice) and the shared RNG
# sandbox. Sourced via spatial_helpers.R.

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
# The app's one seeding convention, used by every helper that draws random
# numbers and by classif_helpers.R's .classif_with_seed; defined in the first
# fragment so every later fragment and worker sees it. The caller's
# .Random.seed is restored on exit, or removed when it had none. `expr` is a
# promise evaluated in the caller's frame, so assignments and return() behave
# as without the wrapper.
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
      # Restore the kind (with_seed() names it), then remove the seed that
      # RNGkind() writes - in that order.
      do.call(RNGkind, as.list(old_kind))
      if (exists(".Random.seed", envir = .GlobalEnv, inherits = FALSE)) {
        rm(".Random.seed", envir = .GlobalEnv)
      }
    }
  }, add = TRUE)
  force(expr)
}

# Sandboxed AND seeded: reproducible draws that leave the caller's stream
# untouched (conditional seeding uses with_rng_sandbox() directly).
#
# The generator is NAMED, not inherited: future/furrr workers run L'Ecuyer-CMRG,
# so a bare set.seed() would draw a different stream in a worker than in a
# script or the tests. Restoring .Random.seed on exit returns a worker to its
# own stream. Changing these three kinds moves every seeded result in the app.
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

# gstat's `range` (a) is not the practical range, where ~95% of the sill is
# reached: that is a for Spherical (the full sill, exactly), 3a for
# Exponential, sqrt(3)a for Gaussian and ~4.75a for Matern nu = 1.5. The factor
# puts every candidate on the ground-distance scale before the sanity window.
# kappa is not fitted (see scientific_guide), so Matern stays at 1.5.
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
# One categorical state per fitted variogram, worst first. The `is_fallback`
# and `flawed_winner` attributes are kept for the CK seed guard and the banner.
#
#   fit_failed         no candidate fitted, or fewer than 5 empirical bins
#   heuristic_fallback candidates tried, none eligible for auto-fit
#   singular_selected  the winner is singular or non-converged
#   range_unresolved   converged, practical range outside the lag support
#   ok                 converged, range inside that span
VGM_FIT_STATUSES <- c("fit_failed", "heuristic_fallback", "singular_selected",
                      "range_unresolved", "ok")

#' The status of a fitted variogram. A model without diagnostics was applied by
#' the user and reports `ok` (its CV is labelled conditional elsewhere).
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

#' TRUE when the fitted target has no usable variance (sill and structure are
#' numerical noise). Unknown reads FALSE, the safe default for a supplied model.
vgm_target_degenerate <- function(fit) {
  isTRUE(attr(fit, "vgm_diagnostics")$target_degenerate)
}

# Is the empirical variogram still climbing at the cutoff? The last third of
# the bins against the third before it; a rise above 5% of the gamma span. NA
# with too few bins (the non-stationarity advisory then stays silent).
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

# Directional (anisotropy) diagnostic: the fitting path's estimator within four
# angular cones. gstat's `alpha` is clockwise from north (0 = N-S, 45 = NE-SW,
# 90 = E-W, 135 = NW-SE); tol.hor = 22.5 partitions the half circle, so each
# pair lands in exactly one cone. Diagnostic only: every engine stays
# isotropic. Coordinates are projected first (validate_and_project_sf()).
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

#' A locality enters a pooled within-locality variogram with at least this many
#' distinct located values (the directional-variogram minimum).
POOLED_VGM_MIN_N <- 10L

#' The per-locality point sets of a pooled within-locality variogram and their
#' common lag classes. Each locality keeps its finite `value_col` (renamed
#' `.mn_val`), is projected on its own and has co-located samples merged; it is
#' eligible with at least `min_n` locations. The classes are the lags of the
#' eligible locality with the smallest cutoff, so every locality spans them.
#' NULL when no locality is eligible.
.pooled_within_prep <- function(pts_list, value_col, min_n) {
  prepared <- lapply(pts_list, function(p) {
    if (!inherits(p, "sf") || !value_col %in% names(p)) return(NULL)
    p <- p[is.finite(p[[value_col]]), value_col]
    if (nrow(p) == 0) return(NULL)
    p <- tryCatch(merge_colocated(validate_and_project_sf(p)), error = function(e) NULL)
    if (is.null(p)) return(NULL)
    names(p)[names(p) == value_col] <- ".mn_val"
    p
  })
  n <- vapply(prepared, function(d) if (is.null(d)) 0L else nrow(d), integer(1))
  ok <- names(prepared)[n >= min_n]
  if (!length(ok)) return(NULL)
  lags <- lapply(prepared[ok], calc_scientific_lags)
  lg <- lags[[which.min(vapply(lags, `[[`, numeric(1), "cutoff"))]]
  n_bins <- max(1L, as.integer(round(lg$cutoff / lg$width)))
  list(pts = prepared[ok], lags = lg,
       boundaries = seq(0, lg$cutoff, length.out = n_bins + 1L),
       localities = ok, excluded = setdiff(names(pts_list), ok))
}

#' Pool per-locality empirical variograms computed on the same lag classes:
#' per class (and direction), the pair-weighted mean semivariance and distance
#' over the localities, i.e. the classical estimator over the union of their
#' within-locality pairs. A row's class is read off its mean distance, which
#' lies inside the class it was binned in (gstat's classes are (b_k, b_k+1]).
.pool_vgm_classes <- function(per, boundaries) {
  rows <- do.call(rbind, lapply(per, function(v) {
    if (is.null(v) || nrow(v) == 0) return(NULL)
    v <- as.data.frame(v)
    bin <- findInterval(v$dist, boundaries, left.open = TRUE)
    data.frame(bin = pmin(pmax(bin, 1L), length(boundaries) - 1L), dir = v$dir.hor,
               np = v$np, dist = v$dist, gamma = v$gamma)
  }))
  if (is.null(rows) || nrow(rows) == 0) return(NULL)
  key <- paste(rows$dir, rows$bin)
  w <- rows$np / stats::ave(rows$np, key, FUN = sum)
  agg <- stats::aggregate(cbind(np = rows$np, dist = w * rows$dist, gamma = w * rows$gamma),
                          by = list(dir = rows$dir, bin = rows$bin), FUN = sum)
  agg <- agg[order(agg$dir, agg$bin), , drop = FALSE]
  data.frame(np = agg$np, dist = agg$dist, gamma = agg$gamma, dir.hor = agg$dir, dir.ver = 0,
             id = factor(rep("var1", nrow(agg))))
}

#' The pooled within-locality variogram of `value_col` over the named list
#' `pts_list`: the classical (Matheron) estimator over the union of
#' WITHIN-locality pairs on common lag classes,
#' gamma(h_k) = sum_l N_l(h_k) gamma_l(h_k) / sum_l N_l(h_k), at the
#' pair-weighted mean distance (McBratney & Pringle 1999). No pair joins two
#' localities. Attributes: `localities`, `excluded`, and `within` (values
#' centred on their locality mean, whose variance the pooled sill estimates).
#' NULL when no locality has `min_n` located values.
pooled_within_variogram <- function(pts_list, value_col, min_n = POOLED_VGM_MIN_N) {
  prep <- .pooled_within_prep(pts_list, value_col, min_n)
  if (is.null(prep)) return(NULL)
  per <- lapply(prep$pts, function(d) gstat::variogram(.mn_val ~ 1, d, boundaries = prep$boundaries))
  out <- .pool_vgm_classes(per, prep$boundaries)
  if (is.null(out)) return(NULL)
  class(out) <- c("gstatVariogram", "data.frame")
  attr(out, "direct") <- data.frame(id = "var1", is.direct = TRUE)
  attr(out, "boundaries") <- prep$boundaries
  attr(out, "pseudo") <- 0
  attr(out, "what") <- "semivariance"
  attr(out, "localities") <- prep$localities
  attr(out, "excluded") <- prep$excluded
  attr(out, "within") <- unlist(lapply(prep$pts, function(d) d$.mn_val - mean(d$.mn_val)),
                                use.names = FALSE)
  out
}

#' The directional twin of pooled_within_variogram(): each locality's four-cone
#' variogram (calc_directional_variogram) on the common lag classes, pooled per
#' cone. Same shape as calc_directional_variogram()'s result, with the same
#' `localities` / `excluded` attributes.
pooled_within_directional <- function(pts_list, value_col, min_n = POOLED_VGM_MIN_N) {
  prep <- .pooled_within_prep(pts_list, value_col, min_n)
  if (is.null(prep)) return(NULL)
  per <- lapply(prep$pts, function(d) calc_directional_variogram(d, ".mn_val", lags = prep$lags, min_n = min_n))
  out <- .pool_vgm_classes(per, prep$boundaries)
  if (is.null(out)) return(NULL)
  attr(out, "localities") <- prep$localities
  attr(out, "excluded") <- prep$excluded
  out
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

#' Whether a fitted candidate may be auto-selected: a finite criterion, a valid
#' finite two-component variogram, and (Monolith's numerical-safety policy) no
#' zero-nugget Gaussian or Matern.
.vgm_autofit_eligible <- function(fit, sse, practical_range) {
  smooth_share <- vgm_smooth_nugget_share(fit)
  length(sse) == 1L && is.finite(sse) &&
    is.finite(fit$psill[2]) && fit$psill[2] > 0 &&
    is.finite(practical_range) && practical_range > 0 &&
    is.finite(fit$psill[1]) && fit$psill[1] >= 0 &&
    !isTRUE(smooth_share <= 1e-8)
}

#' Automated variogram fit: 4 families (Sph, Exp, Gau, Mat nu = 1.5) x 4
#' starting ranges with gstat::fit.variogram.
#'
#' Two separate questions, never to be merged:
#'  * ELIGIBILITY (.vgm_autofit_eligible()): an ineligible candidate is not
#'    compared on SSErr at all.
#'  * IDENTIFIABILITY (`range_resolved`: practical range within max lag / 100
#'    to 2 x max lag) is a DIAGNOSTIC that never ranks candidates; what an
#'    unresolved winner claims is qualified downstream (status, sill_resolved).
#'
#' Selection: (1) converged eligible candidates, lowest SSErr, range resolution
#' breaking only numerical ties; (2) failing that, singular/non-converged
#' eligible ones, same rule; (3) failing that, the heuristic Spherical model.
#'
#' Returns a vgm with attr "vgm_diagnostics", plus "flawed_winner" (case 2) or
#' "is_fallback" (case 3, or fewer than 5 empirical bins).
robust_vgm_fit <- function(v_emp, v_data) {
  initial_sill <- var(v_data, na.rm=TRUE)
  if (is.na(initial_sill) || initial_sill == 0) initial_sill <- 1

  max_dist <- if (!is.null(v_emp) && nrow(v_emp) > 0) max(v_emp$dist, na.rm = TRUE) else 1.0
  if (is.na(max_dist) || is.infinite(max_dist) || max_dist <= 0) {
    max_dist <- 1.0 # Safe default positive distance fallback
  }
  # The span the empirical variogram covers; NA when there is none, so nothing
  # claims support the data never provided.
  max_lag <- if (!is.null(v_emp) && nrow(v_emp) > 0) suppressWarnings(max(v_emp$dist, na.rm = TRUE)) else NA_real_
  if (!isTRUE(is.finite(max_lag))) max_lag <- NA_real_
  still_rising <- .vgm_still_rising(v_emp)
  target_degenerate <- .is_degenerate_covariate(v_data)

  # What may be claimed about the fit, computed where the empirical variogram
  # is in hand. `sill_resolved` (range <= max lag) is stricter than
  # `range_resolved` (up to 2 x max lag) and says what the sill may claim.
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
    # gstat::fit.variogram can crash R on very small empirical variograms.
    fallback <- gstat::vgm(psill = initial_sill * 0.8, "Sph", range = max_dist/2, nugget = initial_sill * 0.2)
    attr(fallback, "is_fallback") <- TRUE
    # No fit to interpret: practical_range and sill_resolved stay NA rather
    # than describing the heuristic's own invented range as data-supported.
    attr(fallback, "vgm_diagnostics") <- vgm_diag(0L, 0L, FALSE, "fit_failed")
    return(fallback)
  }

  # na.rm as in the seeds above: one NA bin would break the comparisons below.
  initial_nugget <- min(v_emp$gamma, na.rm = TRUE)
  if (initial_nugget == 0) initial_nugget <- max(initial_sill * 1e-6, 1e-6)

  if (initial_nugget > initial_sill) initial_nugget <- initial_sill * 0.9
  initial_psill <- max(initial_sill - initial_nugget, initial_sill * 0.1)

  ranges <- c(max_dist / 10, max_dist / 5, max_dist / 4, max_dist / 2)
  models <- c("Sph", "Exp", "Gau", "Mat")

  # gstat reports non-convergence only as a warning, so the warning is the
  # signal; the expected screening warnings are muffled, others propagate.
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
      # ELIGIBILITY, not preference (.vgm_autofit_eligible()):
      # - a negative nugget makes gamma(h) < 0 near the origin, and
      #   gstat::krige() then returns all-NA predictions without a condition
      #   (gstat 2.1.5 clamps it only when attr(, "direct") survives);
      # - a zero-nugget Gaussian/Matern is valid but not auto-selected: such
      #   fits overshot the observed span up to 1990x on the reference data
      #   (manual models are warned, not rejected);
      # - a non-positive or non-finite range is not a variogram (gstat returns
      #   them on non-converged fits).
      valid <- .vgm_autofit_eligible(f, sse, prange)
      # Identifiability, a diagnostic: above 2 x max lag the sill is
      # extrapolated; below max lag / 100 the structure acts as near-pure nugget.
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
