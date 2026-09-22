# spatial_metrics.R - CV plans/folds (resolve_cv_plan, make_cv_folds), CV
# execution (perform_cv, perform_kriging_loocv), error metrics (calc_ccc,
# augment_metrics, calc_moran) and CV pooling. Sourced via spatial_helpers.R -
# see the worker contract note there before moving anything.


#' Prediction and observation column names in a CV object (gstat's `var1.*`
#' names first, then any `*.pred` / `*.observed`). Returns `list(pred,
#' observed)`, NA where a column is absent.
detect_cv_columns <- function(cnames) {
  pre_col <- grep("^var1\\.pred$|^target\\.pred$|^pred$", cnames, value = TRUE)[1]
  if (is.na(pre_col)) pre_col <- grep("\\.pred$", cnames, value = TRUE)[1]
  
  obs_col <- grep("^var1\\.observed$|^observed$|^target\\.observed$", cnames, value = TRUE)[1]
  if (is.na(obs_col)) obs_col <- grep("\\.observed$", cnames, value = TRUE)[1]
  
  list(pred = pre_col, observed = obs_col)
}

#' Lin's concordance correlation coefficient on jointly complete pairs, using
#' population moments (Lin 1989). NA below 2 pairs or when either vector is
#' constant.
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
# Values are returned at full precision; the display layer formats them
# (format_sig() / mnFormatSig), so a small value never becomes 0 before it
# reaches a table or an exported file.
augment_metrics <- function(obs, pre) {
  res <- list(nse = NA, nrmse_mean = NA, nrmse_sd = NA, rpd = NA, rpiq = NA,
              smape = NA, signed_target = NA)
  if (length(obs) < 2) return(res)

  residuals <- obs - pre
  rmse <- sqrt(mean(residuals^2, na.rm = TRUE))
  mean_obs <- mean(obs, na.rm = TRUE)
  sd_obs <- sd(obs, na.rm = TRUE)
  iqr_obs <- IQR(obs, na.rm = TRUE)
  rng_obs <- suppressWarnings(range(obs, na.rm = TRUE))

  sst <- sum((obs - mean_obs)^2, na.rm = TRUE)
  sse <- sum(residuals^2, na.rm = TRUE)
  # NSE is undefined when the observations carry no variance (0/0).
  res$nse <- if (is.finite(sst) && sst > 0) 1 - (sse / sst) else NA

  # Mean-normalised and percentage errors need a ratio scale. A target whose
  # observed values span zero has an arbitrary origin: the same fit reported
  # against a differently centred version of the variable gives a different
  # NRMSE, and sMAPE's |obs| + |pred| denominator collapses at the crossing,
  # so a single sign disagreement contributes its maximum term however small
  # both values are. Report NA rather than a number with no interpretation.
  # The observed sign crossing is the rule because the variable list carries
  # no scale metadata. |mean| < sd is NOT used: a positive ratio-scale variable
  # can have sd > mean and still normalise correctly.
  res$signed_target <- all(is.finite(rng_obs)) && rng_obs[1] < 0 && rng_obs[2] > 0

  # NRMSE (mean) = CV(RMSE). Normalised by |mean|, not the signed mean: RMSE is
  # non-negative, so a negative-mean variable would otherwise report a
  # negative percentage. The zero-mean guard is a numerical-stability branch,
  # separate from the sign-crossing rule above.
  res$nrmse_mean <- if (!res$signed_target && is.finite(mean_obs) && abs(mean_obs) > 0) {
    (rmse / abs(mean_obs)) * 100
  } else NA

  # NRMSE (SD): RMSE in units of the observed spread. Invariant to recentring
  # (SD(y + c) = SD(y)), so it stays defined for anomaly and interval scales.
  # With rmse on divisor n and sd on n - 1 it equals sqrt((1 - NSE)(n - 1)/n)
  # and 1/RPD exactly: it adds no evidence beyond NSE, it restates the error on
  # a scale-free axis.
  res$nrmse_sd <- if (is.finite(sd_obs) && sd_obs > 0) rmse / sd_obs else NA

  # RPD / RPIQ are spread-to-error ratios (Chang et al. 2001): undefined at
  # zero error, so both guard on rmse > 0, not just RPIQ's spread term.
  res$rpd <- if (is.finite(rmse) && rmse > 0) sd_obs / rmse else NA
  res$rpiq <- if (is.finite(rmse) && rmse > 0 && iqr_obs > 0) iqr_obs / rmse else NA

  # sMAPE's summand is 0/0 where obs == pre == 0. Dropping those rows via
  # na.rm would average sMAPE over a different n than every other metric;
  # define the term as 0 instead (the usual convention) so n stays consistent.
  if (!res$signed_target) {
    denom <- abs(obs) + abs(pre)
    term <- ifelse(denom == 0, 0, 2 * abs(residuals) / denom)
    res$smape <- mean(term, na.rm = TRUE) * 100
  }

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

#' A CV object (sf or Spatial) as a plain data.frame; sf input gains `x`/`y`
#' coordinate columns.
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
#' Every value is returned at full precision (see augment_metrics).
#' `signed_target` is TRUE when the scored observations span zero (NRMSE (mean)
#' and SMAPE are then NA by definition, not by failure); `block_fallback` is
#' TRUE when a Spatial Block request fell back to random folds (make_cv_folds).
perform_cv <- function(cv_obj, moran = TRUE) {
  # n = predicted pairs, n_expected = rows with an observed value; metrics use
  # the predicted pairs only, and coverage says how many rows that is.
  res <- list(rmse = NA, r2 = NA, nse = NA, me = NA, mae = NA, ccc = NA,
              nrmse_mean = NA, nrmse_sd = NA, rpd = NA, rpiq = NA, smape = NA,
              moran_i = NA, moran_e = NA, moran_p = NA, n = 0,
              n_expected = 0, coverage = NA_real_, signed_target = NA,
              block_fallback = isTRUE(attr(cv_obj, "block_fallback")))

  if (is.null(cv_obj)) return(res)

  df <- .cv_to_df(cv_obj)

  if (nrow(df) == 0) return(res)
  cnames <- colnames(df)

  cols <- detect_cv_columns(cnames)
  pre_col <- cols$pred
  obs_col <- cols$observed

  if (!is.na(obs_col)) res$n_expected <- sum(!is.na(df[[obs_col]]))
  if (res$n_expected > 0) res$coverage <- 0
  if (is.na(pre_col) || is.na(obs_col)) return(res)

  observed <- df[[obs_col]]
  predicted <- df[[pre_col]]

  valid <- !is.na(observed) & !is.na(predicted)
  obs <- observed[valid]
  pre <- predicted[valid]
  res$n <- length(obs)
  if (res$n_expected > 0) res$coverage <- res$n / res$n_expected

  if (length(obs) < 2) return(res)

  residuals <- obs - pre

  res$rmse <- sqrt(mean(residuals^2, na.rm = TRUE))
  res$me <- mean(residuals, na.rm = TRUE)
  res$mae <- mean(abs(residuals), na.rm = TRUE)
  # cor() on a constant vector already returns NA, but it emits "the standard
  # deviation is zero" on the way — and inside a PSOCK worker that warning
  # surfaces in the run log as an unexplained condition. Guard explicitly, the
  # way augment_metrics() and calc_ccc() do for the same degenerate case.
  res$r2 <- if (isTRUE(stats::sd(obs) > 0) && isTRUE(stats::sd(pre) > 0)) {
    tryCatch(cor(obs, pre)^2, error = function(e) NA_real_)
  } else NA_real_

  res$ccc <- calc_ccc(obs, pre)
  aug <- augment_metrics(obs, pre)
  res$nse <- aug$nse
  res$nrmse_mean <- aug$nrmse_mean
  res$nrmse_sd <- aug$nrmse_sd
  res$rpd <- aug$rpd
  res$rpiq <- aug$rpiq
  res$smape <- aug$smape
  res$signed_target <- aug$signed_target
  
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
      res$moran_i <- mor$i
      res$moran_e <- mor$e_i
      # Every display formats the p-value through format_p_value().
      res$moran_p <- mor$p
  }
  
  return(res)
}

# ── Class-agreement metrics (the "Agreement (Kappa)" table) ──────────────────
# Bins a continuous observed/predicted pair into ordered classes and reports the
# confusion-matrix agreement statistics. Two binning conventions, deliberately
# DIFFERENT in interval closure:
#   "agro"     - the applied agronomical/binned class limits, right = FALSE, so
#                classes are [low, high) exactly as terra::classify(rcl_mat,
#                right = FALSE) paints them on the map. A value sitting on a
#                break must get the same class in this table as on the map.
#   "quartile" - the observed quartiles, cut()'s default right = TRUE. These
#                breaks are data order statistics with no map counterpart, so
#                there is nothing to align with and the conventional
#                right-closed reading applies.
# `params` is the classification_params() snapshot (rcl_mat + labels); it is
# only read for method = "agro". The UI-level question of whether agro styling
# is selected and applied belongs to the caller, not here.
#
# Returns a list whose `status` is a non-NULL message when the metrics are not
# computable (too few points, no variance for quartiles, nothing left after
# binning) and NULL otherwise; the binned factors travel back alongside the
# metrics so a caller can inspect the classification it was scored on.
compute_agreement_metrics <- function(actual, predicted,
                                      method = c("quartile", "agro"),
                                      params = NULL) {
  method <- match.arg(method)

  keep <- !is.na(actual) & !is.na(predicted)
  actual <- actual[keep]
  predicted <- predicted[keep]
  if (length(actual) < 3) return(list(status = "Not enough data points for Kappa."))

  if (method == "agro") {
    if (is.null(params$rcl_mat) || is.null(params$labels)) {
      return(list(status = "No applied classification limits for Kappa."))
    }
    brks <- c(-Inf, params$rcl_mat[-1, 1], Inf)
    lvl <- params$labels
    act_bin  <- cut(actual,    breaks = brks, labels = lvl, include.lowest = TRUE, right = FALSE)
    pred_bin <- cut(predicted, breaks = brks, labels = lvl, include.lowest = TRUE, right = FALSE)
  } else {
    brks <- unique(stats::quantile(actual, probs = seq(0, 1, 0.25), na.rm = TRUE))
    if (length(brks) < 2) return(list(status = "Not enough variance for quartiles."))
    brks_ext <- brks
    brks_ext[1] <- -Inf
    brks_ext[length(brks_ext)] <- Inf
    lvl <- paste0("Q", seq_len(length(brks) - 1))
    act_bin  <- cut(actual,    breaks = brks_ext, include.lowest = TRUE, labels = lvl)
    pred_bin <- cut(predicted, breaks = brks_ext, include.lowest = TRUE, labels = lvl)
  }

  ok <- !is.na(act_bin) & !is.na(pred_bin)
  act_bin  <- factor(act_bin[ok],  levels = lvl)
  pred_bin <- factor(pred_bin[ok], levels = lvl)
  if (length(act_bin) < 3) return(list(status = "Not enough data after binning."))

  safe <- function(expr) tryCatch(expr, error = function(e) NA_real_)
  list(
    status        = NULL,
    n             = length(act_bin),
    levels        = lvl,
    actual_bin    = act_bin,
    predicted_bin = pred_bin,
    accuracy      = safe(yardstick::accuracy_vec(act_bin, pred_bin)),
    bal_accuracy  = safe(yardstick::bal_accuracy_vec(act_bin, pred_bin)),
    # Off-by-one accuracy: the classes are ORDERED, so a prediction landing in
    # an adjacent class is a different kind of error from one landing two
    # classes away. Counts |rank(actual) - rank(predicted)| <= 1 as agreement.
    off_by_one    = safe(sum(abs(as.integer(act_bin) - as.integer(pred_bin)) <= 1) / length(act_bin)),
    mcc           = safe(yardstick::mcc_vec(act_bin, pred_bin)),
    kappa         = safe(yardstick::kap_vec(act_bin, pred_bin)),
    kappa_linear  = safe(yardstick::kap_vec(act_bin, pred_bin, weighting = "linear"))
  )
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
      # losing CV entirely. The folds say so: they are not spatial blocks, and
      # no metric scored on them may be reported as Spatial Block CV.
      if (is.null(folds) || length(unique(folds)) < 2) {
        folds <- sample(rep(seq_len(plan$k), length.out = n))
        attr(folds, "block_fallback") <- TRUE
      }
      folds
    } else {
      # random_kfold: balanced, seeded
      sample(rep(seq_len(plan$k), length.out = n))
    }
  })
}

# Row identity of a CV population: the row number in the uploaded table, added
# at dispatch. Direct engine calls have none and number their rows 1..n.
CV_ROW_ID_COL <- ".mn_row_id"

#' The cross-validation plan of one population: its row ids and one fold vector
#' per realization, from the same make_cv_folds() call (and seeds) the engines
#' always used, so a population's folds do not depend on which engine scores it.
build_cv_plan <- function(pts, strategy = "auto", repeats = 1L) {
  n <- nrow(pts)
  row_id <- if (CV_ROW_ID_COL %in% names(pts)) as.integer(pts[[CV_ROW_ID_COL]]) else seq_len(n)
  coords <- sf::st_coordinates(pts)
  folds <- lapply(seq_len(cv_repeat_count(repeats, strategy, n)), function(r) {
    make_cv_folds(coords, strategy, n, CV_FOLD_SEED + r - 1L)
  })
  list(row_id = row_id, n = n, strategy = strategy,
       label = resolve_cv_plan(strategy, n)$label, folds = folds)
}

#' Run one kriging cross-validation realization fold by fold.
#'
#' `fold_fun(train, newdata, i)` fits from the fold's training rows and returns
#' `list(pred, var = NULL, meta = NULL)` for `newdata`, which carries the
#' held-out rows' coordinates only. A fold that errors, or returns the wrong
#' number of predictions, gives NA rows and a note; it is never filled from
#' another engine. Folds run sequentially (the nested worker already holds the
#' cores). Returns the common kriging CV schema, in population row order, with
#' `attr(, "cv_notes")` and `attr(, "cv_fold_meta")` (per fold label).
run_kriging_folds <- function(pop, target_var, row_id, folds, fold_fun,
                              cancel_file = NULL, progress = NULL) {
  n <- nrow(pop)
  if (length(folds) != n || length(row_id) != n) {
    stop("The CV plan (", length(folds), " fold ids, ", length(row_id),
         " row ids) does not match the CV population (", n, " rows).")
  }
  pred <- rep(NA_real_, n)
  pvar <- rep(NA_real_, n)
  notes <- character(0)
  fold_meta <- list()
  geom <- sf::st_geometry(pop)
  labels <- sort(unique(folds))
  # At most ~20 progress writes, whatever the fold count.
  write_every <- max(1L, ceiling(length(labels) / 20))
  for (k in seq_along(labels)) {
    i <- labels[k]
    if (!is.null(cancel_file) && file.exists(cancel_file)) stop("Model generation cancelled by user.")
    test_idx <- which(folds == i)
    out <- tryCatch(fold_fun(pop[-test_idx, ], sf::st_sf(geometry = geom[test_idx]), i),
                    error = function(e) e)
    if (inherits(out, "error")) {
      notes <- c(notes, paste0("fold ", i, ": ", conditionMessage(out)))
    } else {
      p <- suppressWarnings(as.numeric(out$pred))
      if (length(p) != length(test_idx)) {
        notes <- c(notes, sprintf("fold %s: %d predictions returned for %d held-out samples",
                                  i, length(p), length(test_idx)))
      } else {
        undefined <- !is.finite(p)
        if (any(undefined)) {
          notes <- c(notes, sprintf("fold %s: %d of %d predictions undefined", i, sum(undefined), length(p)))
          p[undefined] <- NA_real_
        }
        pred[test_idx] <- p
        v <- suppressWarnings(as.numeric(out$var))
        if (length(v) == length(test_idx)) pvar[test_idx] <- ifelse(is.finite(v), v, NA_real_)
      }
      if (!is.null(out$meta)) fold_meta[[as.character(i)]] <- out$meta
    }
    if (!is.null(progress) && (k %% write_every == 0L || k == length(labels))) {
      update_progress_file(progress$l, progress$prefix,
                           progress$from + (progress$to - progress$from) * k / length(labels), 100)
    }
  }
  observed <- pop[[target_var]]
  cv <- sf::st_sf(row_id = as.integer(row_id), fold = folds, observed = observed,
                  var1.pred = pred, var1.var = pvar, residual = observed - pred,
                  geometry = geom)
  attr(cv, "cv_notes") <- notes
  attr(cv, "cv_fold_meta") <- fold_meta
  attr(cv, "block_fallback") <- attr(folds, "block_fallback")
  cv
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
# localities with pool_cv_sf(), whose rbind fails on a column mismatch, and the
# engines still emit different column sets (the kriging schema vs the TPS/IDW
# frame) - normalising here makes the pooled repeat set structurally safe by
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
# deliberately absent (see perform_cv's `moran` argument). NRMSE (mean) is the
# mean-normalised RMSE, CV(RMSE); NRMSE (SD) sits beside it because it is the
# form that stays defined for a target whose values span zero.
CV_REPEAT_METRICS <- c(rmse = "RMSE", nrmse_mean = "NRMSE (mean, %)",
                       nrmse_sd = "NRMSE (SD)", mae = "MAE",
                       r2 = "R² (Corr)", nse = "R² (NSE/Trad)", me = "Bias (ME)",
                       ccc = "Lin's CCC (Agree)", rpd = "RPD (Prec)",
                       rpiq = "RPIQ", smape = "SMAPE (%)")

# Column order and labels of the Model Performance table and its export: the
# repeat dictionary plus the three Moran fields. Built FROM CV_REPEAT_METRICS so
# the Fold-Realization Stability table cannot relabel a metric the Model
# Performance table still shows under its old name. It lives here rather than in
# ui_formatting.R because global.R sources ui_helpers.R BEFORE spatial_helpers.R,
# so a top-level reference from there would not resolve.
CV_METRIC_LABELS <- c(CV_REPEAT_METRICS,
                      moran_i = "Moran's I", moran_e = "Moran E[I]",
                      moran_p = "Moran p")

# mean / SD across fold realizations. A metric that is undefined in ANY repeat
# (NA by the augment_metrics convention) is reported as NA rather than averaged
# over the subset where it happened to exist - a mean over a varying number of
# repeats is not the quantity the column claims to be.
summarise_cv_repeats <- function(reps) {
  if (is.null(reps) || length(reps) < 2) return(NULL)
  if (any(vapply(reps, is.null, logical(1)))) return(NULL)
  mets <- lapply(reps, function(x) perform_cv(x, moran = FALSE))
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
  n_pred <- vapply(mets, function(m) as.numeric(m$n), numeric(1))
  list(n_repeats = length(reps),
       n = mets[[1]]$n,
       n_expected = mets[[1]]$n_expected,
       n_min = min(n_pred),
       n_max = max(n_pred),
       # NRMSE (mean) and SMAPE are NA across the repeats when any realization
       # scored observations that span zero; the display says why.
       signed_target = any(vapply(mets, function(m) isTRUE(m$signed_target), logical(1))),
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

#' Cross-validation for RK (`model_type = "lm"`) and RFK (`"rf"`). Every fold
#' re-screens the covariates, refits the covariate surfaces, the trend and the
#' residual variogram on its training rows, and predicts the held-out rows as
#' trend + kriged residual, through run_kriging_folds(). `candidates` is the
#' full selected covariate list the screen chooses from (defaults to
#' `aux_vars`, the surface's own kept set). `folds` / `row_id` come from the
#' run's CV plan and must align with the complete-case rows; with `folds =
#' NULL` they are built from `cv_strategy` and `fold_seed`. Returns the common
#' kriging CV schema (`var1.var` NA), or NULL below 3 complete-case rows.
perform_kriging_loocv <- function(pts, target_var, aux_vars, lags_func, vgm_fit_func, model_type = c("lm", "rf"), l = "region", prefix = "act", rf_ntree = 200, cv_strategy = "auto", fold_seed = CV_FOLD_SEED, cov_params = list(),
                                  folds = NULL, row_id = NULL, cancel_file = NULL, progress = NULL,
                                  candidates = NULL, vif_threshold = 10) {
  model_type <- match.arg(model_type)
  candidates <- candidates %||% aux_vars
  pts <- pts[complete.cases(sf::st_drop_geometry(pts)[, c(target_var, aux_vars), drop=FALSE]), ]
  n <- nrow(pts)
  if (n < 3) return(NULL)

  # `fold_seed` moves only under repeated CV; each fold's model draws below are
  # seeded from its own fold LABEL, so a repeat varies the PARTITION and
  # nothing else.
  if (is.null(folds)) {
    folds <- make_cv_folds(sf::st_coordinates(pts), cv_strategy, n, fold_seed)
  } else if (length(folds) != n) {
    stop("The CV fold vector has ", length(folds), " entries for ", n, " complete-case samples.")
  }
  if (is.null(row_id)) {
    row_id <- if (CV_ROW_ID_COL %in% names(pts)) as.integer(pts[[CV_ROW_ID_COL]]) else seq_len(n)
  }

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

  # `newdata` holds the held-out coordinates only: their covariates are kriged
  # from the training rows, exactly as the map kriges its grid.
  fold_fn <- function(train, newdata, i) {
    # The covariate screen is a data-driven step like any other, so it runs on
    # the fold's training rows: otherwise the held-out rows' own measured
    # covariates help decide which covariates their prediction uses.
    kept <- screen_covariates(train, candidates, vif_threshold)$kept
    if (!length(kept)) stop("the covariate screen removed every covariate in this fold")
    if (model_type == "lm" && nrow(train) < length(kept) + 2L) {
      stop(sprintf("%d training rows cannot fit %d regression coefficients",
                   nrow(train), length(kept) + 1L))
    }
    lags <- lags_func(train)
    test_cov <- sf::st_drop_geometry(krige_covariates(
      train, newdata, kept, lags, cov_params)$grid_aux)

    if (model_type == "lm") {
      form_i <- as.formula(paste0("`", target_var, "` ~ ",
                                  paste(paste0("`", kept, "`"), collapse = " + ")))
      lm_mod <- lm(form_i, data = train)
      train$residuals <- residuals(lm_mod)
      pred_trend <- predict(lm_mod, newdata = test_cov)
    } else {
      # Each fold's forest is drawn from its OWN seed. A forest's number of RNG
      # draws depends on the data it is grown on, so one stream over the whole
      # loop let earlier folds — which train on fold i's rows — decide where
      # fold i's forest started, and a held-out row moved its own prediction.
      # The seed follows the fold LABEL, not the realization, so a repeated-CV
      # repeat still varies the partition and nothing else. Matrix interface,
      # as for the map's forest (apply_kriging_pipeline): the formula method
      # cannot find a covariate whose name is not syntactic.
      rf_mod <- with_seed(CV_FOLD_SEED + as.integer(i),
                          randomForest::randomForest(x = sf::st_drop_geometry(train)[kept],
                                                     y = train[[target_var]], ntree = rf_ntree))
      train$residuals <- train[[target_var]] - rf_mod$predicted
      pred_trend <- predict(rf_mod, test_cov[kept])
    }

    v_emp <- variogram(residuals ~ 1, train, width = lags$width, cutoff = lags$cutoff)
    v_fit <- vgm_fit_func(v_emp, train$residuals)
    res_krig <- krige(residuals ~ 1, train, newdata, model = v_fit, debug.level = 0)
    # The residual variogram is refitted per fold like OK's, so it reports the
    # same fit state and the run log can name a fold that took a degraded one.
    list(pred = as.numeric(pred_trend) + res_krig$var1.pred,
         meta = list(kept = kept, vgm_status = vgm_fit_status(v_fit)))
  }

  cv <- run_kriging_folds(pts, target_var, row_id, folds, fold_fn, cancel_file, progress)
  attr(cv, "cv_screen") <- .fold_screen_summary(cv, aux_vars)
  cv
}

#' Observed minus predicted from a CV object, or its `residual` column when the
#' pair is not found; an all-NA vector of length `n_rows` when neither exists.
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
# Every spatial entry is reduced to cv_repeat_frame()'s fixed columns first, so
# localities whose engines (or fallbacks) produced different CV schemas still
# pool; a spatial entry that cannot be reduced or reprojected makes the pool
# NULL, so a subset is never reported as the pool.
pool_cv_sf <- function(df_list) {
  if (is.null(df_list) || length(df_list) == 0) return(NULL)
  sf_list <- list()
  for (x in df_list) {
    if (inherits(x, "Spatial")) x <- tryCatch(sf::st_as_sf(x), error = function(e) NULL)
    if (!inherits(x, "sf") || is.na(sf::st_crs(x))) next
    frame <- cv_repeat_frame(x)
    if (is.null(frame)) return(NULL)
    sf_list[[length(sf_list) + 1L]] <- frame
  }
  if (length(sf_list) == 0) return(NULL)

  ll_list <- lapply(sf_list, function(x) tryCatch(sf::st_transform(x, 4326), error = function(e) NULL))
  # A locality whose CRS cannot reach the common frame fails the pool too.
  if (any(vapply(ll_list, is.null, logical(1)))) return(NULL)

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

#' Metrics of the pooled "Total (Combined)" row: perform_cv() on the pool, with
#' the expected count summed over every locality in `metrics_list`. A locality
#' whose cross-validation failed outright adds no rows to the pool, so without
#' that sum the pooled row would report full coverage. NULL when nothing pools.
perform_pooled_cv <- function(data_list, metrics_list = NULL) {
  pooled <- pool_cv_sf(data_list)
  if (is.null(pooled) || nrow(pooled) == 0) return(NULL)
  res <- perform_cv(pooled)
  expected <- sum(vapply(metrics_list %||% list(),
                         function(m) as.numeric(m$n_expected %||% 0), numeric(1)))
  if (expected > res$n_expected) {
    res$n_expected <- expected
    res$coverage <- res$n / expected
  }
  res
}
