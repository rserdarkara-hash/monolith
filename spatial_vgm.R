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
# numbers, the Classification Suite's included; defined in the first
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

# gstat's `range` (a) is not the practical range, the distance at which the
# model completes ~95% of its rise above the nugget (all of it, at a, for
# Spherical): 3a for Exponential, sqrt(3)a for Gaussian and ~4.75a for Matern
# nu = 1.5. The factor puts every family on the ground-distance scale for the
# range search and the resolution window. kappa is not fitted (see
# scientific_guide), so Matern stays at 1.5.
.vgm_practical_range_factor <- function(model, kappa = NA_real_) {
  m <- as.character(model)[1]
  if (identical(m, "Mat")) {
    k <- suppressWarnings(as.numeric(kappa)[1])
    if (isTRUE(abs(k - 0.5) < 1e-8)) return(3)      # Matern nu = 0.5 IS exponential
    if (isTRUE(abs(k - 2.5) < 1e-8)) return(5.92)
    return(4.75)                                    # nu = 1.5 (the auto-fit's value)
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

# A Gaussian or Matern model (applied, or an auto-fitted Matern) with a nugget
# below this share of the total sill is flagged, not changed. Advisory only: on
# the reference data, 5% bounded the overshoot beyond the observed range to
# 0.02 of its span in every locality, while 1% allowed up to 0.5.
VGM_SMOOTH_NUGGET_WARN_SHARE <- 0.05

# Two candidates whose weighted least-squares criteria differ by less than
# this relative amount are numerically tied; only then does range resolution
# choose between them. It is a tie rule, not a penalty: a candidate that fits
# the empirical variogram measurably better always wins. The same band makes
# grid values one plateau and bounds a fit's equally-fitting set.
VGM_SSE_TIE_REL <- 1e-8

# The auto-fit's families, in the order that settles a remaining tie. The
# Gaussian is left to manual models: it implies an analytic, infinitely smooth
# field (Stein 1999), and on the reference data the best other family
# cross-validated better in 36 of the 50 pairs the Gaussian won on the fitting
# criterion (each model held fixed across the folds: exploratory evidence, not
# a general verdict on the Gaussian). Matern is nu = 1.5, as in manual_vgm().
VGM_AUTOFIT_FAMILIES <- c("Sph", "Exp", "Mat")

# The range profile spans practical ranges from max lag / 100 (the lower edge
# of the resolution window) to VGM_RANGE_CEILING_MULT x max lag, on
# VGM_PROFILE_GRID_N log-spaced points about 3.5% apart; 400 or 800 points
# changed no pick on the reference data. The ceiling is a numerical-safety
# convention, not an estimate: it binds only where the variogram keeps rising,
# where no ceiling is right (2 x max lag worsened fits by up to 55%, 100 x gave
# near-linear models with very large sills), and a fit there is reported as a
# bound.
VGM_PROFILE_GRID_N <- 200L
VGM_RANGE_CEILING_MULT <- 10

# Every grid basin within this relative distance of the best grid value is
# refined, so a minimum the grid samples coarsely is not lost to a neighbouring
# basin; 1e-2 changed no pick on the reference data.
VGM_CONTENDER_REL <- 1e-3

# A fit whose equally-fitting nugget shares span more than one percentage point
# has a nugget the binned empirical variogram does not fix under the criterion.
# The set holds numerically equivalent fits, not a confidence interval: a
# narrow one does not show that the data estimate the nugget precisely.
VGM_NUGGET_ID_WIDTH <- 0.01

# ── Fit state ────────────────────────────────────────────────────────────────
# One categorical state per fitted variogram, worst first:
#
#   fit_failed        fewer than 5 usable empirical bins, or every semivariance
#                     zero: a stand-in that is not estimated from the data
#   pure_nugget       no eligible structured model: every family's optimum is
#                     flat, or a structured Matern was refused (pure_nugget_cause)
#   range_unresolved  practical range outside the lag support
#   ok                range inside that span
VGM_FIT_STATUSES <- c("fit_failed", "pure_nugget", "range_unresolved", "ok")

#' The status of a fitted variogram. A model without diagnostics was applied by
#' the user and reports `ok` (its CV is labelled conditional elsewhere).
vgm_fit_status <- function(fit) {
  if (is.null(fit) || NROW(fit) == 0) return("fit_failed")
  s <- attr(fit, "vgm_diagnostics")$status
  if (!is.null(s) && s %in% VGM_FIT_STATUSES) s else "ok"
}

#' Plain-language state, for a run-log line or a table cell.
vgm_status_label <- function(status) {
  switch(as.character(status)[1],
         fit_failed = "variogram fit failed",
         pure_nugget = "pure nugget, no structured fit selected",
         range_unresolved = "range unresolved by lag support",
         "fitted")
}

#' TRUE when the values the fit describes have no usable variance (the target,
#' or RK/RFK residuals judged at the target's scale), so its sill and structure
#' are numerical noise. Unknown reads FALSE, the safe default for a supplied
#' model.
vgm_target_degenerate <- function(fit) {
  isTRUE(attr(fit, "vgm_diagnostics")$target_degenerate)
}

#' Why no kriging variance may be reported from `fit`, NULL when it may:
#' "stand_in" for the `fit_failed` stand-in, whose nugget share, sill and range
#' are seeds, not estimates; "noise_floor" for a fit to values without usable
#' variance (`target_degenerate`), which describes rounding, as its unreported
#' parameters do. Kriging weights do not change when a covariance is rescaled,
#' so the predictions stand; the variance follows the seeded or noise-level
#' sill (x100 sill, x100 variance) and is withheld as NA, never zero. Fitted
#' models and applied manual ones (conditional, but the user's) keep it.
vgm_variance_withheld <- function(fit) {
  if (identical(vgm_fit_status(fit), "fit_failed")) "stand_in"
  else if (vgm_target_degenerate(fit)) "noise_floor"
}

# Is the empirical variogram still climbing at the cutoff? `g` holds the
# semivariances of its usable bins in lag order: the last third of them against
# the third before it, a rise above 5% of the gamma span. NA with too few bins
# or a flat variogram (the non-stationarity advisory then stays silent).
.vgm_still_rising <- function(g) {
  n <- length(g)
  k <- max(2L, floor(n / 3))
  if (n < 2L * k) return(NA)
  span <- diff(range(g))
  if (!isTRUE(span > 0)) return(NA)
  (mean(utils::tail(g, k)) - mean(g[seq(n - 2L * k + 1L, n - k)])) > 0.05 * span
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
#' entries, in VGM_AUTOFIT_FAMILIES order). The fitting criterion decides; range
#' resolution only breaks a tie: the tie set is every candidate within
#' VGM_SSE_TIE_REL of the lowest SSErr, narrowed to its resolved members when
#' there are any, and its first member wins. So rounding never chooses between
#' two candidates inside the band, and a better fit is never penalised.
.vgm_pick_best <- function(pool) {
  sse <- vapply(pool, function(x) x$sse, numeric(1))
  tie <- which(sse <= min(sse) * (1 + VGM_SSE_TIE_REL))
  resolved <- vapply(pool[tie], function(x) isTRUE(x$range_resolved), logical(1))
  if (any(resolved)) tie <- tie[resolved]
  pool[[tie[1]]]
}

#' Whether a fitted candidate may be auto-selected: a finite criterion, a valid
#' finite two-component variogram, and (Monolith's numerical-safety policy) no
#' Gaussian or Matern at a zero nugget. `share_min`, the smallest nugget share
#' over the fit's equally-fitting set, extends that rule to the whole set: a
#' smooth family that fits as well with a zero nugget is refused.
.vgm_autofit_eligible <- function(fit, sse, practical_range, share_min = NULL) {
  smooth_share <- vgm_smooth_nugget_share(fit)
  if (!is.na(smooth_share) && length(share_min) == 1L) smooth_share <- min(smooth_share, share_min)
  length(sse) == 1L && is.finite(sse) &&
    is.finite(fit$psill[2]) && fit$psill[2] > 0 &&
    is.finite(practical_range) && practical_range > 0 &&
    is.finite(fit$psill[1]) && fit$psill[1] >= 0 &&
    !isTRUE(smooth_share <= 1e-8)
}

# ── Profile weighted-least-squares search ───────────────────────────────────
# The auto-fit minimises gstat's weighted least-squares criterion
# (fit.method = 7), sum_j N_j / h_j^2 (gamma_j - gamma(h_j))^2 (vgm_weighted_sse),
# without starting values, iteration caps or convergence flags: for a fixed
# range the model is linear in nugget and partial sill, which come in closed
# form, so only the range is searched (separable least squares; Golub &
# Pereyra 2003), over a fixed data-based grid refined by Brent's method.

# Unit-sill shape rho(x), x = h / a, of each VGM_AUTOFIT_FAMILIES member as
# gstat defines it (variogramLine; Matern at nu = 1.5), so the criterion here
# is gstat's SSErr.
.vgm_shape <- function(family, x) {
  switch(family,
         Sph = ifelse(x < 1, 1.5 * x - 0.5 * x^3, 1),
         Exp = 1 - exp(-x),
         Mat = 1 - (1 + x) * exp(-x),
         stop("unknown variogram family ", family))
}

# Column-wise min sum(w (g - c0 - c1 r)^2) over c0, c1 >= 0, for every column r
# of R: a two-variable non-negative least squares (Lawson & Hanson 1974),
# solved exactly. The weighted unconstrained solution is optimal when feasible
# (the problem is convex); otherwise the optimum lies on the c0 = 0 or the
# c1 = 0 ray, and the lower of the two is taken. A column whose weighted
# variance is at most 1e-12 cannot be told from a nugget and is returned as
# the pure nugget, never as an arbitrary split. An active bound gives exactly
# 0, never a value just above it.
.vgm_nnls_cols <- function(g, R, w) {
  n <- length(g)
  sw <- sum(w)
  gb <- sum(w * g) / sw
  dg <- g - gb
  c0n <- max(gb, 0)
  sse_n <- sum(w * (g - c0n)^2)
  rb <- colSums(w * R) / sw
  D <- R - rep(rb, each = n)
  srr <- colSums(w * D^2)
  ok <- srr > 1e-12 * sw
  c1 <- ifelse(ok, colSums(w * D * dg) / srr, 0)
  c0 <- gb - c1 * rb
  sse_int <- colSums(w * (g - rep(c0, each = n) - R * rep(c1, each = n))^2)
  interior <- ok & c1 > 0 & c0 >= 0
  c1z <- pmax(0, colSums(w * R * g) / colSums(w * R^2))
  sse_z <- colSums(w * (g - R * rep(c1z, each = n))^2)
  use_z <- !interior & ok & sse_z < sse_n
  list(c0 = ifelse(interior, c0, ifelse(use_z, 0, c0n)),
       c1 = ifelse(interior, c1, ifelse(use_z, c1z, 0)),
       sse = ifelse(interior, sse_int, ifelse(use_z, sse_z, sse_n)))
}

# One family's best fit over practical ranges [p_lo, p_hi] for bins at mean
# distance h with semivariance g and weights w.
#  * The criterion is profiled on VGM_PROFILE_GRID_N log-spaced ranges, plus,
#    for the Spherical, every lag distance inside them: its profile has a kink
#    where a lag enters the range, so each cell then holds one smooth piece.
#    Values equal within VGM_SSE_TIE_REL form one plateau; a run lower than
#    both neighbours (one neighbour at a domain end) is a basin, and every
#    basin within VGM_CONTENDER_REL of the best grid value is refined with
#    Brent's method on log a, over its bracket and, for a plateau, over each
#    end cell as well: the profile can dip just past the end of a flat run,
#    where Brent over the mostly flat bracket finds the dip or not by the last
#    bits. The lowest wins; tied basins go to the shorter range.
#  * Brent sets the minimum S_min only. It places the minimiser to about 1e-7
#    in log a, depending on the distance unit, so no parameter is read off it:
#    the ranges fitting within VGM_SSE_TIE_REL of S_min form the
#    equally-fitting set, whose edges are bisected (45 steps), and the fit
#    returned is the set's representative. A narrow set, holding no grid point
#    between its edges, is represented by the midpoint of its edges; a wider
#    set (an exact ridge, where one lag lies inside a Spherical range) by the
#    member in the middle of its nugget interval, or the middle of its
#    log-range span when the nugget is not monotone across it.
# Returns the representative's c0, c1, a, practical range p and sse, the
# set's practical-range and nugget-share intervals, and whether it reaches the
# lower or upper domain bound.
.vgm_profile_family <- function(h, g, w, family, p_lo, p_hi) {
  fac <- .vgm_practical_range_factor(family, 1.5)
  la <- seq(log(p_lo / fac), log(p_hi / fac), length.out = VGM_PROFILE_GRID_N)
  if (identical(family, "Sph")) {
    la <- sort(unique(c(la, log(h[h > p_lo / fac & h < p_hi / fac]))))
  }
  prof <- function(x) {
    .vgm_nnls_cols(g, outer(h, exp(x), function(hh, aa) .vgm_shape(family, hh / aa)), w)
  }
  S1 <- function(x) prof(x)$sse
  S <- prof(la)$sse
  tau <- VGM_SSE_TIE_REL
  n <- length(S)
  run <- cumsum(c(TRUE, abs(diff(S)) > tau * pmax(S[-1], S[-n])))
  idx <- split(seq_len(n), run)
  v <- vapply(idx, function(ix) S[ix[1]], numeric(1))
  nr <- length(v)
  is_basin <- c(TRUE, v[-nr] > v[-1]) & c(v[-1] > v[-nr], TRUE)
  cont <- which(is_basin & v <= min(v) * (1 + VGM_CONTENDER_REL))
  basins <- lapply(cont, function(r) {
    ix <- idx[[r]]
    i_lo <- max(1, ix[1] - 1)
    i_hi <- min(n, ix[length(ix)] + 1)
    brackets <- list(c(i_lo, i_hi))
    if (length(ix) > 1) brackets <- c(brackets, list(c(i_lo, ix[1]), c(ix[length(ix)], i_hi)))
    best <- list(x = la[ix[(length(ix) + 1) %/% 2]], s = v[r])
    for (b in brackets) {
      if (b[1] == b[2]) next
      o <- stats::optimize(S1, la[b], tol = 1e-10)
      if (o$objective < best$s) best <- list(x = o$minimum, s = o$objective)
    }
    best
  })
  s_b <- vapply(basins, `[[`, numeric(1), "s")
  thr <- min(s_b) * (1 + tau)
  x_star <- min(vapply(basins[s_b <= thr], `[[`, numeric(1), "x"))
  # Walk from x_star to the first grid point outside the set and bisect the
  # crossing; a domain bound reached inside the set is the edge.
  edge <- function(dir) {
    j <- if (dir < 0) rev(which(la < x_star)) else which(la > x_star)
    x_in <- x_star
    for (i in j) {
      if (S[i] <= thr) {
        x_in <- la[i]
        next
      }
      x_out <- la[i]
      for (k in seq_len(45L)) {
        m <- (x_in + x_out) / 2
        if (S1(m) <= thr) x_in <- m else x_out <- m
      }
      return(x_in)
    }
    if (dir < 0) la[1] else la[n]
  }
  x_lo <- edge(-1)
  x_hi <- edge(1)
  x_rep <- (x_lo + x_hi) / 2
  # Narrow or wide is judged on the set itself, never on where x_star landed:
  # on a flat ridge, whether Brent's point or a grid point seeds the walk is
  # decided by the last bits.
  if (any(la > x_lo & la < x_hi)) {
    c0_at <- function(x) prof(x)$c0
    c0_lo <- c0_at(x_lo)
    c0_hi <- c0_at(x_hi)
    if (abs(c0_hi - c0_lo) > 1e-9 * max(abs(c0_lo), abs(c0_hi), 1e-300)) {
      cs <- vapply(seq(x_lo, x_hi, length.out = 9), c0_at, numeric(1))
      if (all(diff(cs) >= 0) || all(diff(cs) <= 0)) {
        target <- (c0_lo + c0_hi) / 2
        a <- x_lo
        b <- x_hi
        for (k in seq_len(45L)) {
          m <- (a + b) / 2
          if ((c0_at(m) - target) * (c0_lo - target) > 0) a <- m else b <- m
        }
        x_rep <- (a + b) / 2
      }
    }
  }
  # A set that is not one interval between its edges keeps the member Brent
  # found, the one point known to lie in it.
  if (S1(x_rep) > thr) x_rep <- x_star
  at <- function(x) {
    p <- prof(x)
    list(c0 = p$c0, c1 = p$c1, sse = p$sse, p = exp(x) * fac)
  }
  rep_ <- at(x_rep)
  lo_ <- at(x_lo)
  hi_ <- at(x_hi)
  share <- function(z) if (z$c0 + z$c1 > 0) z$c0 / (z$c0 + z$c1) else NA_real_
  shares <- c(share(lo_), share(rep_), share(hi_))
  list(c0 = rep_$c0, c1 = rep_$c1, a = exp(x_rep), p = rep_$p, sse = rep_$sse,
       p_interval = c(lo_$p, hi_$p),
       share_interval = if (all(is.na(shares))) c(NA_real_, NA_real_) else range(shares, na.rm = TRUE),
       at_low = x_lo <= la[1] + 1e-9, at_high = x_hi >= la[n] - 1e-9)
}

#' Automated variogram fit: the profile search above for each family in
#' VGM_AUTOFIT_FAMILIES, over practical ranges from max lag / 100 to
#' VGM_RANGE_CEILING_MULT x max lag. The fit is a function of the empirical
#' variogram and the stated rules only; starting values, iteration caps,
#' convergence flags and evaluation order do not enter it.
#'
#' Two separate questions, never to be merged:
#'  * ELIGIBILITY (.vgm_autofit_eligible()): an ineligible family is not
#'    compared on SSErr at all.
#'  * IDENTIFIABILITY (`range_resolved`: practical range strictly between max
#'    lag / 100 and 2 x max lag, the equally-fitting set clear of the search
#'    floor) is a DIAGNOSTIC: in selection it only breaks a numerical tie and
#'    never outranks a measurably better fit. What an unresolved winner claims
#'    is qualified downstream (status, sill_resolved).
#'
#' Selection: the eligible family with the lowest SSErr, range resolution and
#' then the family order settling numerical ties only (.vgm_pick_best()). With
#' no eligible family the flat fit is returned, a pure nugget at the weighted
#' mean semivariance. Fewer than 5 usable bins, or a zero empirical variogram
#' (a constant target), leave nothing to estimate: a Spherical stand-in seeded
#' from the sample variance is returned in state fit_failed; no kriging
#' variance is reported from it (vgm_variance_withheld()). `target_degenerate`
#' marks values without usable variance, whose fit describes rounding; RK and
#' RFK judge their residuals at the target's scale (.residuals_degenerate()).
#'
#' Returns a vgm with attr "vgm_diagnostics", and "SSErr" unless it is the
#' stand-in.
robust_vgm_fit <- function(v_emp, v_data, target_degenerate = .is_degenerate_covariate(v_data)) {
  # Usable bins only: an NA, non-finite, zero-distance or empty bin carries no
  # information. Their largest distance is the one max lag that sets the search
  # domain, the resolution window, sill_resolved and max_lag alike.
  keep <- if (is.null(v_emp)) logical(0) else
    is.finite(v_emp$gamma) & is.finite(v_emp$dist) & is.finite(v_emp$np) &
      v_emp$dist > 0 & v_emp$np > 0
  h <- v_emp$dist[keep]
  g <- v_emp$gamma[keep]
  np <- v_emp$np[keep]
  max_lag <- if (length(h)) max(h) else NA_real_
  still_rising <- .vgm_still_rising(g)

  # What may be claimed about the fit, computed where the empirical variogram
  # is in hand. `sill_resolved` (range <= max lag) is stricter than
  # `range_resolved` (up to 2 x max lag) and says what the sill may claim.
  # `fam` is the winning family's profile result.
  vgm_diag <- function(status, fam = NULL, cause = NA_character_) {
    prange <- if (is.null(fam)) NA_real_ else fam$p
    sill_resolved <- if (is.na(prange)) NA else prange <= max_lag
    shares <- if (!is.null(fam)) fam$share_interval else
      if (identical(status, "pure_nugget")) c(1, 1) else c(NA_real_, NA_real_)
    list(status = status,
         practical_range = prange,
         max_lag = max_lag,
         sill_resolved = sill_resolved,
         # Which end of the window the range fell outside, for the banner. A
         # set reaching the search floor lies below it whatever its middle.
         range_side = if (!identical(status, "range_unresolved")) NA_character_
                      else if (fam$at_low || prange <= max_lag / 100) "below" else "beyond",
         # The range reached the search ceiling, so it is a bound; NA without
         # a fitted range (a pure nugget or the stand-in).
         range_at_ceiling = if (is.null(fam)) NA else isTRUE(fam$at_high),
         # The equally-fitting set: practical ranges and nugget shares fitting
         # within VGM_SSE_TIE_REL of the fit.
         range_interval = if (is.null(fam)) c(NA_real_, NA_real_) else fam$p_interval,
         nugget_share_interval = shares,
         nugget_identified = if (anyNA(shares)) NA else diff(shares) <= VGM_NUGGET_ID_WIDTH,
         # A sill the lags never reached (or no structured sill for a pure nugget)
         # AND an empirical variogram still climbing at the cutoff. Both,
         # because either alone is weak.
         trend_suspected = (identical(sill_resolved, FALSE) || identical(status, "pure_nugget")) &&
           isTRUE(still_rising),
         # For a pure nugget: every family's optimum flat ("no_structure"), or
         # a structured smooth optimum refused at a zero nugget.
         pure_nugget_cause = cause,
         pairs_per_lag = if (length(np)) range(np) else c(NA_real_, NA_real_),
         target_degenerate = target_degenerate)
  }

  if (length(g) < 5 || all(g == 0)) {
    # Nothing to estimate. The stand-in krigs a constant exactly; the
    # diagnostics claim no range or sill for it, and its kriging variance is
    # withheld (vgm_variance_withheld()).
    sill <- stats::var(v_data, na.rm = TRUE)
    if (!isTRUE(sill > 0)) sill <- 1
    fallback <- gstat::vgm(psill = sill * 0.8, "Sph", range = (if (is.na(max_lag)) 1 else max_lag) / 2,
                           nugget = sill * 0.2)
    attr(fallback, "vgm_diagnostics") <- vgm_diag("fit_failed")
    return(fallback)
  }

  w <- np / h^2
  cands <- lapply(VGM_AUTOFIT_FAMILIES, function(m) {
    fam <- .vgm_profile_family(h, g, w, m, max_lag / 100, VGM_RANGE_CEILING_MULT * max_lag)
    fit <- manual_vgm(fam$c1, m, fam$a, fam$c0)
    attr(fit, "SSErr") <- fam$sse
    list(fit = fit, sse = fam$sse, fam = fam,
         eligible = .vgm_autofit_eligible(fit, fam$sse, fam$p, fam$share_interval[1]),
         # Identifiability, a diagnostic: above 2 x max lag the sill is
         # extrapolated; below max lag / 100 the structure acts as near-pure nugget.
         range_resolved = fam$p > max_lag / 100 && fam$p < 2 * max_lag && !fam$at_low)
  })
  pool <- Filter(function(x) x$eligible, cands)
  if (!length(pool)) {
    # No eligible structured model: the flat fit (c1 = 0, c0 the weighted mean
    # semivariance). Its cause says whether every optimum was flat or a
    # structured optimum was refused by the zero-nugget rule.
    c0 <- sum(w * g) / sum(w)
    fit <- gstat::vgm(psill = c0, model = "Nug", range = 0)
    attr(fit, "SSErr") <- sum(w * (g - c0)^2)
    refused <- any(vapply(cands, function(x) x$fam$c1 > 0, logical(1)))
    attr(fit, "vgm_diagnostics") <- vgm_diag("pure_nugget",
                                             cause = if (refused) "zero_nugget_rule" else "no_structure")
    return(fit)
  }
  win <- .vgm_pick_best(pool)
  fit <- win$fit
  attr(fit, "vgm_diagnostics") <- vgm_diag(if (win$range_resolved) "ok" else "range_unresolved", win$fam)
  fit
}
