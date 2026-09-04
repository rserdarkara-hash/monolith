# spatial_metrics.R - CV plans/folds (resolve_cv_plan, make_cv_folds), CV
# execution (perform_cv, perform_kriging_loocv), error metrics (calc_ccc,
# augment_metrics, calc_moran) and CV pooling. Sourced via spatial_helpers.R -
# see the worker contract note there before moving anything.


detect_cv_columns <- function(cnames) {
  pre_col <- grep("^var1\\.pred$|^target\\.pred$|^pred$", cnames, value = TRUE)[1]
  if (is.na(pre_col)) pre_col <- grep("\\.pred$", cnames, value = TRUE)[1]
  
  obs_col <- grep("^var1\\.observed$|^observed$|^target\\.observed$", cnames, value = TRUE)[1]
  if (is.na(obs_col)) obs_col <- grep("\\.observed$", cnames, value = TRUE)[1]
  
  list(pred = pre_col, observed = obs_col)
}

calc_ccc <- function(observed, predicted) {
  # Filter to jointly complete pairs up front so means, variances and the
  # covariance all come from the SAME subset (mixing per-vector na.rm with
  # pairwise.complete.obs would use different subsets under misaligned NAs).
  ok <- !is.na(observed) & !is.na(predicted)
  observed <- observed[ok]
  predicted <- predicted[ok]
  if (length(observed) < 2) return(NA)

  n <- length(observed)
  mean_obs <- mean(observed)
  mean_pred <- mean(predicted)

  # Lin (1989) defines CCC on POPULATION second moments. Using the sample (n-1)
  # variances and covariance while leaving the squared bias (mean_obs -
  # mean_pred)^2 unscaled is a different statistic: the two agree only when the
  # means are equal, and elsewhere the (n-1) form is optimistic - it discounts
  # exactly the systematic offset CCC exists to penalise. Measured against
  # DescTools::CCC, the reference the known-answer fixture cites: +0.17% at
  # n = 30 and +0.66% at n = 12 on a biased fixture, and the (n-1)/n rescaling
  # below reproduces DescTools to 1e-10. Excess shrinks as 1/n, so it is under
  # 0.1% at the reference dataset's n = 632.
  scale_pop <- (n - 1) / n
  var_obs <- var(observed) * scale_pop
  var_pred <- var(predicted) * scale_pop

  if (is.na(var_obs) || is.na(var_pred) || var_obs == 0 || var_pred == 0) {
    # CCC is undefined when either vector is constant: the correlation term
    # does not exist (0/0). Report NA rather than asserting agreement.
    return(NA)
  }

  cov_op <- cov(observed, predicted) * scale_pop

  numerator <- 2 * cov_op
  denominator <- var_obs + var_pred + (mean_obs - mean_pred)^2
  
  if (is.na(denominator) || denominator == 0) return(NA)
  
  ccc <- numerator / denominator
  return(ccc)
}

# Degenerate cases return NA rather than +/-Inf: every metric here is a ratio,
# and each has an input configuration that zeroes its denominator (constant
# observations, a zero-mean variable, a perfect fit). Inf/-Inf would flow
# straight into the Model Performance table, the metrics CSV and the pooled
# Total (Combined) diagnostics; NA states "undefined here", which is what these
# quantities actually are. Same convention as calc_ccc's constant-vector branch.
# `round_values = FALSE` returns the metrics at full precision. Repeated CV
# uses it: a mean/SD taken across fold realizations must be computed on raw
# values, or the SD is quantized by the display rounding and reports rounding
# noise instead of fold-assignment variance. The display layer formats.
augment_metrics <- function(obs, pre, round_values = TRUE) {
  rnd <- if (isTRUE(round_values)) function(x, d) round(x, d) else function(x, d) x
  res <- list(nse = NA, nrmse_mean = NA, rpd = NA, rpiq = NA, smape = NA)
  if (length(obs) < 2) return(res)

  residuals <- obs - pre
  rmse <- sqrt(mean(residuals^2, na.rm = TRUE))
  mean_obs <- mean(obs, na.rm = TRUE)
  sd_obs <- sd(obs, na.rm = TRUE)
  iqr_obs <- IQR(obs, na.rm = TRUE)

  sst <- sum((obs - mean_obs)^2, na.rm = TRUE)
  sse <- sum(residuals^2, na.rm = TRUE)
  # NSE is undefined when the observations carry no variance (0/0).
  res$nse <- if (is.finite(sst) && sst > 0) rnd(1 - (sse / sst), 4) else NA

  # Relative RMSE is undefined for a zero-mean variable (centred/anomaly data).
  # Normalised by |mean|, not the signed mean: RMSE is non-negative, so a
  # negative-mean variable (anomalies, sub-zero temperatures, redox potential)
  # would otherwise report a negative NRMSE%, a nonsensical sign for a
  # normalised error. Identical for every positive-mean variable.
  res$nrmse_mean <- if (is.finite(mean_obs) && abs(mean_obs) > 0) rnd((rmse / abs(mean_obs)) * 100, 2) else NA

  # RPD / RPIQ are spread-to-error ratios (Chang et al. 2001): undefined at
  # zero error, so both guard on rmse > 0, not just RPIQ's spread term.
  res$rpd <- if (is.finite(rmse) && rmse > 0) rnd(sd_obs / rmse, 2) else NA
  res$rpiq <- if (is.finite(rmse) && rmse > 0 && iqr_obs > 0) rnd(iqr_obs / rmse, 2) else NA

  # sMAPE's summand is 0/0 where obs == pre == 0. Dropping those rows via
  # na.rm would average sMAPE over a different n than every other metric;
  # define the term as 0 instead (the usual convention) so n stays consistent.
  denom <- abs(obs) + abs(pre)
  term <- ifelse(denom == 0, 0, 2 * abs(residuals) / denom)
  res$smape <- rnd(mean(term, na.rm = TRUE) * 100, 2)

  return(res)
}

#' Residual Moran's I with its null expectation and significance.
#'
#' Returns `list(i, e_i, p)`: the statistic, its expectation under the null of
#' no spatial autocorrelation (E[I] = -1/(n-1), identical under the normality
#' and randomization assumptions), and a two-sided p-value. Reporting I alone
#' is misleading, because E[I] is negative rather than zero: an I of, say,
#' +0.01 at n = 30 sits barely above E[I] = -0.034 and is no evidence of
#' clustering at all. Every "cannot compute" branch returns all-NA - NA here
#' means the statistic was not computable for this point set, never "no
#' spatial structure was found".
calc_moran <- function(residuals, coords) {
  na_res <- list(i = NA_real_, e_i = NA_real_, p = NA_real_)
  if (is.null(residuals) || is.null(coords)) return(na_res)
  n <- length(residuals)
  if (n < 3 || nrow(coords) != n) return(na_res)

  tryCatch({
    coords_matrix <- as.matrix(coords)
    if (any(duplicated(coords_matrix))) {
      # Separate duplicates under a sandboxed RNG so Moran's I is
      # reproducible and the caller's RNG stream is not perturbed.
      # The displacement has to clear two bars: large enough to actually
      # separate two identical doubles at THIS data's coordinate magnitude, and
      # small enough to leave the neighbour graph untouched. A fixed 1e-8 fails
      # the first one — at a UTM northing of ~4.5e6 one ULP is ~9.3e-10, so
      # 1e-8 buys about ten representable steps and is one coordinate-magnitude
      # change away from rounding to a silent no-op. A no-op would send the
      # duplicates into knearneigh(), which errors on them, dropping the whole
      # calculation into the all-pairs fallback with a different neighbour
      # definition. Scale to the field instead: 1e-9 of the coordinate span,
      # floored at 1e-12 of the coordinate magnitude (~4500 ULPs, which keeps
      # small-extent fields at large projected offsets separable), never below
      # the historical 1e-8. All three are orders of magnitude below any real
      # sample spacing, so the kNN contiguity is unchanged.
      coords <- with_seed(12345, {
        span <- suppressWarnings(max(diff(range(coords_matrix[, 1], na.rm = TRUE)),
                                     diff(range(coords_matrix[, 2], na.rm = TRUE))))
        mag <- suppressWarnings(max(abs(coords_matrix), na.rm = TRUE))
        if (!is.finite(span)) span <- 0
        if (!is.finite(mag)) mag <- 0
        jit_amt <- max(1e-8, span * 1e-9, mag * 1e-12)
        coords_matrix[, 1] <- jitter(coords_matrix[, 1], amount = jit_amt)
        coords_matrix[, 2] <- jitter(coords_matrix[, 2], amount = jit_amt)
        coords_matrix
      })
    }
    
    # Residual Moran's I uses a symmetric k-nearest-neighbour contiguity
    # (k = 8, a common default), capped at n - 1 for small samples. kNN is
    # scale-stable and avoids an arbitrary distance-band multiplier (such as
    # mean-NN x 5), which, being a wide band, dilutes local autocorrelation
    # toward zero. The reported I is contingent
    # on this neighbour definition (documented in scientific_guide.md).
    k_nn <- min(8L, nrow(coords) - 1L)
    # For small n, k=8 exceeds n/3 and spdep emits an expected informational
    # warning ("k greater than one-third of the number of data points"); muffle
    # only that known message, let anything unrecognized propagate.
    nb <- withCallingHandlers(
      spdep::knn2nb(spdep::knearneigh(as.matrix(coords), k = k_nn), sym = TRUE),
      warning = function(w) {
        if (grepl("greater than one-third", conditionMessage(w))) invokeRestart("muffleWarning")
      }
    )

    lw <- spdep::nb2listw(nb, style = "W", zero.policy = TRUE)
    
    # alternative = "two.sided" departs from spdep's default ("greater"): this is
    # a diagnostic, so a strongly NEGATIVE residual autocorrelation (checkerboard
    # error pattern) is just as much a model-misspecification signal as a
    # positive one, and we do not presuppose the direction.
    m_res <- spdep::moran.test(residuals, lw, zero.policy = TRUE,
                               randomisation = FALSE, alternative = "two.sided")
    return(list(i = as.numeric(m_res$estimate[1]),
                e_i = as.numeric(m_res$estimate[2]),
                p = as.numeric(m_res$p.value)))
  }, error = function(e) {
    if (n > 500) return(na_res)
    dists <- as.matrix(dist(coords))
    diag(dists) <- 0
    weights <- 1 / dists
    weights[is.infinite(weights)] <- 0
    diag(weights) <- 0
    # Row-standardize (decided 2026-07-05, user sign-off) so this fallback
    # matches the primary spdep path's nb2listw(style = "W") convention
    row_sums <- rowSums(weights, na.rm = TRUE)
    row_sums[row_sums == 0] <- 1
    weights <- weights / row_sums
    mean_res <- mean(residuals, na.rm = TRUE)
    diffs <- residuals - mean_res
    numerator <- n * sum(weights * outer(diffs, diffs), na.rm = TRUE)
    denominator <- sum(weights, na.rm = TRUE) * sum(diffs^2, na.rm = TRUE)
    # E[I] is a property of the null hypothesis, not of the neighbour scheme, so
    # it is available analytically here. The p-value is NOT: this branch has no
    # sampling distribution attached to it, and fabricating one (e.g. reusing the
    # normality-assumption variance derived for a different weight matrix) would
    # be worse than reporting nothing.
    return(list(i = numerator / denominator, e_i = -1 / (n - 1), p = NA_real_))
  })
}

.cv_to_df <- function(cv_obj) {
  if (is.null(cv_obj)) return(NULL)
  if (inherits(cv_obj, "Spatial")) {
    as.data.frame(cv_obj)
  } else if (inherits(cv_obj, "sf")) {
    coords <- st_coordinates(cv_obj)
    df <- st_drop_geometry(cv_obj)
    df$x <- coords[, 1]
    df$y <- coords[, 2]
    df
  } else {
    as.data.frame(cv_obj)
  }
}

# ── Coordinate-column names (single source of truth) ────────────────────────
# One list per axis, shared by perform_cv's coordinate detection below and by
# is_coord_col(). This lives in a spatial fragment, NOT in ui_formatting.R or
# global_utils.R, because PSOCK workers source only spatial_helpers.R.
.coord_names_x <- c("x", "lon", "long", "lng", "longitude", "easting")
.coord_names_y <- c("y", "lat", "latitude", "northing")

# A column counts as a coordinate column only when its whole (trimmed) name is
# a recognised coordinate token. Substring matching ("lon"/"lat" anywhere in
# the name) silently excluded legitimate variables like Precipitation_cumulative
# ("lat" in cumulative), correlation_index, or along_slope from variable
# mapping, grouping, and plotting choices. Vectorised over `x`.
is_coord_col <- function(x) {
  tolower(trimws(x)) %in% c(.coord_names_x, .coord_names_y)
}

#' Error metrics for one cross-validation object.
#'
#' `moran = FALSE` skips the residual Moran's I block (leaving moran_i/e/p at
#' NA). Repeated CV calls this once per fold realization and reports the spread
#' of the deterministic error metrics only: Moran's I is a spatial diagnostic of
#' ONE residual field, is the most expensive term here (an spdep neighbour
#' search), and is reported for the reference realization in the main table.
#' `round_values = FALSE` returns every metric at full precision (see
#' augment_metrics): repeated CV aggregates across fold realizations and must
#' not take an SD over values the display rounding has already quantized.
perform_cv <- function(cv_obj, moran = TRUE, round_values = TRUE) {
  rnd <- if (isTRUE(round_values)) function(x, d) round(x, d) else function(x, d) x
  res <- list(rmse = NA, r2 = NA, nse = NA, me = NA, mae = NA, ccc = NA,
              nrmse_mean = NA, rpd = NA, rpiq = NA, smape = NA,
              moran_i = NA, moran_e = NA, moran_p = NA, n = 0)
  
  if (is.null(cv_obj)) return(res)
  
  df <- .cv_to_df(cv_obj)
        
  if (nrow(df) == 0) return(res)
  cnames <- colnames(df)
  
  cols <- detect_cv_columns(cnames)
  pre_col <- cols$pred
  obs_col <- cols$observed
  
  if (is.na(pre_col) || is.na(obs_col)) return(res)
  
  observed <- df[[obs_col]]
  predicted <- df[[pre_col]]
  
  valid <- !is.na(observed) & !is.na(predicted)
  obs <- observed[valid]
  pre <- predicted[valid]
  
  if (length(obs) < 2) return(res)
  
  residuals <- obs - pre
  
  res$rmse <- rnd(sqrt(mean(residuals^2, na.rm = TRUE)), 4)
  res$me <- rnd(mean(residuals, na.rm = TRUE), 4)
  res$mae <- rnd(mean(abs(residuals), na.rm = TRUE), 4)
  # cor() on a constant vector already returns NA, but it emits "the standard
  # deviation is zero" on the way — and inside a PSOCK worker that warning
  # surfaces in the run log as an unexplained condition. Guard explicitly, the
  # way augment_metrics() and calc_ccc() do for the same degenerate case.
  r2_val <- if (isTRUE(stats::sd(obs) > 0) && isTRUE(stats::sd(pre) > 0)) {
    tryCatch(cor(obs, pre)^2, error = function(e) NA_real_)
  } else NA_real_
  res$r2 <- rnd(r2_val, 4)
  res$n <- length(obs)
  
  res$ccc <- rnd(calc_ccc(obs, pre), 4)
  aug <- augment_metrics(obs, pre, round_values = round_values)
  res$nse <- aug$nse
  res$nrmse_mean <- aug$nrmse_mean
  res$rpd <- aug$rpd
  res$rpiq <- aug$rpiq
  res$smape <- aug$smape
  
  # Exact-name matching first, on the SAME token lists is_coord_col() uses:
  # prefix matching let a covariate named e.g. "Longitude_deg" win over the
  # guaranteed x/y columns, because [1] takes the first match in COLUMN order,
  # not pattern order. .cv_to_df() always supplies x/y for sf and Spatial
  # inputs, so the exact names normally settle it.
  pick_coord <- function(cn, exact, fallback) {
    hit <- cn[tolower(cn) %in% exact]
    if (length(hit)) return(hit[1])
    grep(fallback, cn, ignore.case = TRUE, value = TRUE)[1]
  }
  x_col <- pick_coord(cnames, .coord_names_x, "^easting")
  y_col <- pick_coord(cnames, .coord_names_y, "^northing")
  if (isTRUE(moran) && !is.na(x_col) && !is.na(y_col)) {
      coords <- df[valid, c(x_col, y_col)]
      # I on its own cannot be read without its null expectation; carry E[I] and
      # the two-sided p alongside it (p is NA on the all-pairs fallback path).
      mor <- calc_moran(residuals, coords)
      # rnd(), not round(): `moran` and `round_values` are independent arguments,
      # so a caller asking for full precision must get it on these three fields
      # too, the way every other metric in `res` already does.
      res$moran_i <- rnd(mor$i, 4)
      res$moran_e <- rnd(mor$e_i, 4)
      res$moran_p <- rnd(mor$p, 4)
  }
  
  return(res)
}

# ── Cross-validation fold planning ──────────────────────────────────────────
# Single source of truth for the CV strategy so the fold builder
# (make_cv_folds) and the UI label (cv_type_label) can never drift.
#   "auto"  : LOOCV for n <= 50, seeded random 10-fold above (historical default)
#   "loocv" : full leave-one-out regardless of n
#   "block" : 10 spatially-clustered (k-means) folds; degrades to LOOCV when
#             n is too small for the blocks to be meaningful.
CV_BLOCK_MIN_N <- 30L

# The one fold seed every engine uses for its reported (reference) CV run.
# Repeated CV walks CV_FOLD_SEED + 1, + 2, ... so repeat 1 IS the reference
# realization: turning repeats on can never move the numbers already displayed.
CV_FOLD_SEED <- 12345L

# Central authority for CV plan selection: maps (strategy, n) to a fold scheme
# (type, fold count k, human-readable label) so every caller builds folds and
# labels them identically, including the small-n degradations to LOOCV.
resolve_cv_plan <- function(strategy = "auto", n) {
  if (is.null(strategy) || length(strategy) != 1 || !nzchar(strategy)) strategy <- "auto"
  # Degrade rather than throw on an unrecognised strategy: a stale or
  # hand-edited run-config upload carrying an old key would otherwise raise
  # match.arg's error inside a PSOCK worker and surface as the generic
  # "Parallel Interpolation Failed" modal.
  if (!strategy %in% c("auto", "loocv", "block")) strategy <- "auto"
  if (is.null(n) || length(n) == 0 || is.na(n)) return(list(type = "loocv", k = NA_integer_, label = "CV"))
  if (strategy == "loocv") return(list(type = "loocv", k = n, label = "Full LOOCV"))
  if (strategy == "block") {
    if (n < CV_BLOCK_MIN_N) return(list(type = "loocv", k = n, label = paste0("LOOCV [Spatial Block needs n ≥ ", CV_BLOCK_MIN_N, "]")))
    return(list(type = "block", k = 10L, label = "Spatial Block CV"))
  }
  # auto
  if (n > 50) return(list(type = "random_kfold", k = 10L, label = "Random 10-fold CV"))
  list(type = "loocv", k = n, label = "LOOCV")
}

# Returns an integer fold-id vector of length n. `coords` is an n x 2 matrix of
# projected (metric) coordinates, used only by Spatial Block (k-means). Seeded
# (CV_FOLD_SEED) under a two-sided RNG sandbox so folds are reproducible and the
# caller's .Random.seed is preserved (same convention as calc_moran). `seed` is
# varied ONLY by repeated CV, which needs alternative fold realizations of the
# same plan; every reported single-realization run keeps the default.
make_cv_folds <- function(coords, strategy = "auto", n = NULL, seed = CV_FOLD_SEED) {
  if (is.null(n)) n <- nrow(coords)
  plan <- resolve_cv_plan(strategy, n)

  if (plan$type == "loocv") return(seq_len(n))

  with_seed(seed, {
    if (plan$type == "block") {
      folds <- tryCatch({
        cm <- as.matrix(coords)[, 1:2, drop = FALSE]
        km <- stats::kmeans(cm, centers = plan$k, nstart = 5, iter.max = 50)
        as.integer(km$cluster)
      }, error = function(e) NULL)
      # Degenerate geometry (e.g. many duplicate coordinates) can make k-means
      # fail or collapse; fall back to a seeded random k-fold rather than
      # losing CV entirely.
      if (is.null(folds) || length(unique(folds)) < 2) {
        folds <- sample(rep(seq_len(plan$k), length.out = n))
      }
      folds
    } else {
      # random_kfold: balanced, seeded
      sample(rep(seq_len(plan$k), length.out = n))
    }
  })
}

# ── Repeated cross-validation (opt-in) ──────────────────────────────────────
# A k-fold estimate is ONE realization of a random partition: at moderate n the
# spread across alternative splits can rival the difference between two methods.
# Repeating the CV under alternative fold assignments and reporting mean +/- SD
# separates model skill from split luck. It is opt-in because it costs one full
# CV pass per extra repeat, and it never touches the reported reference run
# (repeat 1 keeps CV_FOLD_SEED) nor the prediction surface.

# How many fold realizations to run for this point set. LOOCV plans are
# deterministic - every "repeat" would return the identical partition - so they
# always collapse to 1 regardless of the user's setting. Guards a nonsense
# request (0, NA, huge) into a sane range.
cv_repeat_count <- function(n_repeats, strategy = "auto", n = NULL) {
  if (is.null(n_repeats) || length(n_repeats) != 1) return(1L)
  n_repeats <- suppressWarnings(as.integer(n_repeats))
  if (is.na(n_repeats) || n_repeats < 2L) return(1L)
  n_repeats <- min(n_repeats, 25L)
  if (identical(resolve_cv_plan(strategy, n)$type, "loocv")) return(1L)
  n_repeats
}

# Reduce a CV object to the minimum every downstream consumer needs (observed,
# prediction, geometry) under FIXED column names. Repeats are pooled across
# localities with pool_cv_sf(), whose rbind fails on a column mismatch, and
# engines emit different column sets (krige.cv vs gstat.cv vs the TPS frame) -
# normalising here makes the pooled repeat set structurally safe by
# construction. Metrics are unaffected: perform_cv reads only these columns.
cv_repeat_frame <- function(cv_obj) {
  if (is.null(cv_obj)) return(NULL)
  if (inherits(cv_obj, "Spatial")) cv_obj <- tryCatch(sf::st_as_sf(cv_obj), error = function(e) NULL)
  if (!inherits(cv_obj, "sf")) return(NULL)
  cols <- detect_cv_columns(colnames(cv_obj))
  if (is.na(cols$pred) || is.na(cols$observed)) return(NULL)
  out <- tryCatch(cv_obj[, c(cols$observed, cols$pred)], error = function(e) NULL)
  if (is.null(out)) return(NULL)
  names(out)[1:2] <- c("observed", "var1.pred")
  out
}

# Metrics that carry a meaningful spread across fold realizations. Moran's I is
# deliberately absent (see perform_cv's `moran` argument).
CV_REPEAT_METRICS <- c(rmse = "RMSE", nrmse_mean = "NRMSE (%)", mae = "MAE",
                       r2 = "R² (Corr)", nse = "R² (NSE/Trad)", me = "Bias (ME)",
                       ccc = "Lin's CCC (Agree)", rpd = "RPD (Prec)",
                       rpiq = "RPIQ", smape = "SMAPE (%)")

# mean / SD across fold realizations. A metric that is undefined in ANY repeat
# (NA by the augment_metrics convention) is reported as NA rather than averaged
# over the subset where it happened to exist - a mean over a varying number of
# repeats is not the quantity the column claims to be.
summarise_cv_repeats <- function(reps) {
  if (is.null(reps) || length(reps) < 2) return(NULL)
  if (any(vapply(reps, is.null, logical(1)))) return(NULL)
  # Raw precision: a mean/SD across realizations must not be taken over values
  # the display rounding has already quantized (RPD to 0.01, RMSE to 1e-4), or
  # the SD reports the rounding lattice rather than fold-assignment variance.
  mets <- lapply(reps, function(x) perform_cv(x, moran = FALSE, round_values = FALSE))
  keys <- names(CV_REPEAT_METRICS)
  agg <- lapply(keys, function(k) {
    v <- vapply(mets, function(m) {
      val <- m[[k]]
      if (is.null(val) || length(val) != 1) NA_real_ else as.numeric(val)
    }, numeric(1))
    if (any(!is.finite(v))) return(c(mean = NA_real_, sd = NA_real_))
    c(mean = mean(v), sd = stats::sd(v))
  })
  names(agg) <- keys
  list(n_repeats = length(reps),
       n = mets[[1]]$n,
       mean = vapply(agg, function(a) unname(a["mean"]), numeric(1)),
       sd = vapply(agg, function(a) unname(a["sd"]), numeric(1)))
}

# Main-session assembly of a run's repeated-CV report.
#   reps_by_loc : locality -> list of CV frames (length R, or length 1 for a
#                 locality whose plan degraded to deterministic LOOCV)
# Localities carrying a single frame are RECYCLED into every pooled repeat:
# under LOOCV their out-of-fold predictions are identical in every realization,
# so this is exact, and it keeps the pooled repeat rows built from the same
# locality set as the pooled row of the main metrics table.
build_cv_repeat_summary <- function(reps_by_loc) {
  reps_by_loc <- Filter(function(x) length(x) > 0, reps_by_loc %||% list())
  if (!length(reps_by_loc)) return(NULL)
  n_rep <- max(vapply(reps_by_loc, length, integer(1)))
  if (n_rep < 2) return(NULL)

  per_loc <- lapply(reps_by_loc, function(reps) {
    if (length(reps) < 2) NULL else summarise_cv_repeats(reps)
  })
  per_loc <- Filter(Negate(is.null), per_loc)

  pooled <- lapply(seq_len(n_rep), function(r) {
    parts <- lapply(reps_by_loc, function(reps) reps[[min(r, length(reps))]])
    pool_cv_sf(parts)
  })
  total <- if (any(vapply(pooled, is.null, logical(1)))) NULL else summarise_cv_repeats(pooled)

  if (!length(per_loc) && is.null(total)) return(NULL)
  list(n_repeats = n_rep, per_loc = per_loc, total = total)
}

perform_kriging_loocv <- function(pts, target_var, aux_vars, lags_func, vgm_fit_func, model_type = c("lm", "rf"), l = "region", prefix = "act", rf_ntree = 200, cv_strategy = "auto", fold_seed = CV_FOLD_SEED) {
  model_type <- match.arg(model_type)
  pts <- pts[complete.cases(sf::st_drop_geometry(pts)[, c(target_var, aux_vars), drop=FALSE]), ]
  n <- nrow(pts)
  if (n < 3) return(NULL)
  form_reg <- as.formula(paste0("`", target_var, "` ~ ", paste(paste0("`", aux_vars, "`"), collapse = " + ")))

  pts$orig_idx <- seq_len(n)

  # Fold assignment (make_cv_folds seeds itself); computed here on the
  # complete-case rows so the fold vector length always matches n. `fold_seed`
  # moves only under repeated CV; the per-fold model draws below stay on
  # CV_FOLD_SEED so a repeat varies the PARTITION and nothing else.
  folds <- make_cv_folds(sf::st_coordinates(pts), cv_strategy, n, fold_seed)

  # Rank guard. Every fold refits the trend on n - |fold| rows; below
  # (covariates + intercept) + 1 rows that fit is rank-deficient, predict()
  # hands back NA for the held-out points, kriging carries the NAs through and
  # the reported CV metrics degrade to NA with nothing saying why. Fail loudly
  # instead — safe_run_cv turns this into a visible "<engine> CV Error" log
  # line. Only the lm trend is checked: randomForest has no rank requirement.
  # (Factor covariates expand to more coefficients than this count, so the
  # guard is conservative, not exhaustive.)
  if (model_type == "lm") {
    n_coef <- length(aux_vars) + 1L
    min_train <- n - max(table(folds))
    if (min_train < n_coef + 1L) {
      stop(sprintf(
        paste0("Cross-validation needs at least %d training points per fold to fit %d ",
               "regression coefficients, but the largest fold leaves only %d of %d points. ",
               "Use fewer covariates, or a CV strategy with smaller folds."),
        n_coef + 1L, n_coef, min_train, n))
    }
  }

  # Sandbox the seed so the per-fold randomForest draws are reproducible
  # without perturbing the caller's RNG stream. Fold assignment is already
  # fixed upstream; this covers RFK's in-fold randomForest, so its LOOCV is
  # seeded consistently across strategies and n.
  with_seed(CV_FOLD_SEED, {
    fold_fn <- function(i) {
      test_idx <- which(folds == i)
      train <- pts[-test_idx, ]; test <- pts[test_idx, ]

      if (model_type == "lm") {
        lm_mod <- lm(form_reg, data = train)
        train$residuals <- residuals(lm_mod)
        pred_trend <- predict(lm_mod, newdata = test)
      } else {
        rf_mod <- randomForest::randomForest(form_reg, data = train, ntree = rf_ntree)
        train$residuals <- train[[target_var]] - rf_mod$predicted
        pred_trend <- predict(rf_mod, test)
      }

      lags <- lags_func(train)
      v_emp <- variogram(residuals ~ 1, train, width = lags$width, cutoff = lags$cutoff)
      v_fit <- vgm_fit_func(v_emp, train$residuals)
      tryCatch({
        res_krig <- krige(residuals ~ 1, train, test, model = v_fit, debug.level = 0)
        fold_sf <- test[, c("orig_idx", target_var), drop = FALSE]
        names(fold_sf)[names(fold_sf) == target_var] <- "observed"
        fold_sf$var1.pred <- as.numeric(pred_trend) + res_krig$var1.pred
        fold_sf$residual <- fold_sf$observed - fold_sf$var1.pred
        fold_sf
      }, error = function(e) {
        warning(paste("Kriging CV fold failed:", e$message))
        fold_sf <- test[, c("orig_idx", target_var), drop = FALSE]
        names(fold_sf)[names(fold_sf) == target_var] <- "observed"
        fold_sf$var1.pred <- NA
        fold_sf$residual <- NA
        fold_sf
      })
    }

    results_list <- lapply(sort(unique(folds)), fold_fn)

    res_combined <- do.call(rbind, results_list)

    res_combined <- res_combined[order(res_combined$orig_idx), ]
    res_combined$orig_idx <- NULL

    res_combined
  })
}

get_cv_residuals <- function(cv_obj, n_rows) {
  if (is.null(cv_obj)) return(rep(NA_real_, n_rows))
  df <- .cv_to_df(cv_obj)
  cnames <- colnames(df)
  cols <- detect_cv_columns(cnames)
  pre_col <- cols$pred
  obs_col <- cols$observed
  
  if (is.na(pre_col) || is.na(obs_col)) {
    res_col <- grep("^residual$", cnames, ignore.case = TRUE, value = TRUE)[1]
    if (!is.na(res_col)) return(df[[res_col]])
    return(rep(NA_real_, n_rows))
  }
  return(df[[obs_col]] - df[[pre_col]])
}

# Pool per-locality CV objects into ONE sf object in a common METRIC CRS for
# the "Total (Combined)" diagnostics. Each locality's CV object travels in its
# own local UTM zone, so pooling reprojects everything to the auto-UTM zone of
# the combined centroid (same zone rule as validate_and_project_sf) — never
# Web Mercator: EPSG:3857 distances are inflated by 1/cos(latitude) (~40% at
# 45°N), which systematically stretched the pooled residual-variogram lag axis
# and the pooled Moran's I neighbour distances. Entries that are neither sf
# nor Spatial are skipped: every current engine returns its CV object as sf in
# the locality CRS, and a bare data.frame's x/y columns carry no knowable CRS.
pool_cv_sf <- function(df_list) {
  if (is.null(df_list) || length(df_list) == 0) return(NULL)
  sf_list <- lapply(df_list, function(x) {
    if (inherits(x, "Spatial")) x <- tryCatch(sf::st_as_sf(x), error = function(e) NULL)
    if (inherits(x, "sf") && !is.na(sf::st_crs(x))) x else NULL
  })
  sf_list <- Filter(Negate(is.null), sf_list)
  if (length(sf_list) == 0) return(NULL)

  ll_list <- lapply(sf_list, function(x) tryCatch(sf::st_transform(x, 4326), error = function(e) NULL))
  ll_list <- Filter(Negate(is.null), ll_list)
  if (length(ll_list) == 0) return(NULL)

  coords <- do.call(rbind, lapply(ll_list, sf::st_coordinates))
  lon_c <- mean(coords[, 1], na.rm = TRUE)
  lat_c <- mean(coords[, 2], na.rm = TRUE)
  if (is.na(lon_c) || is.na(lat_c)) return(NULL)
  utm_zone <- floor((lon_c + 180) / 6) + 1
  utm_crs <- paste0("+proj=utm +zone=", utm_zone, " +datum=WGS84 +units=m +no_defs")
  if (lat_c < 0) utm_crs <- paste0(utm_crs, " +south")

  proj_list <- lapply(ll_list, function(x) sf::st_transform(x, utm_crs))
  # If the localities' CV objects cannot be row-bound (column mismatch),
  # return NULL so the UI shows its empty state — silently returning only the
  # first locality as "Total (Combined)" would be scientifically wrong.
  tryCatch(do.call(rbind, proj_list), error = function(e) NULL)
}
