# spatial_metrics.R - CV plans and folds, CV execution, error metrics and CV
# pooling. Sourced via spatial_helpers.R (see its worker contract).

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

  # Lin (1989) uses POPULATION moments; the sample (n - 1) form with an
  # unscaled bias term is optimistic about exactly the offset CCC penalises.
  # Matches DescTools::CCC to 1e-10.
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

# Degenerate cases (constant observations, zero mean, perfect fit) return NA,
# not +/-Inf, which would flow into the tables, exports and pooled row. Values
# are returned at full precision; the display layer formats them.
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

  # Mean-normalised and percentage errors need a ratio scale: when the observed
  # values span zero the origin is arbitrary (NRMSE changes with recentring and
  # sMAPE's denominator collapses at the crossing), so they are NA. The sign
  # crossing is the rule because the variable list carries no scale metadata;
  # |mean| < sd is not (a ratio-scale variable can have sd > mean).
  res$signed_target <- all(is.finite(rng_obs)) && rng_obs[1] < 0 && rng_obs[2] > 0

  # NRMSE (mean) = CV(RMSE), normalised by |mean| so a negative-mean variable
  # does not report a negative percentage.
  res$nrmse_mean <- if (!res$signed_target && is.finite(mean_obs) && abs(mean_obs) > 0) {
    (rmse / abs(mean_obs)) * 100
  } else NA

  # NRMSE (SD): RMSE in units of the observed spread, invariant to recentring.
  # With RMSE on n and SD on n - 1 it equals sqrt((1 - NSE)(n - 1)/n) and
  # 1/RPD: a scale-free restatement, no evidence beyond NSE.
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

# Permutations behind every residual Moran p-value, and the seed they are drawn
# under (inside with_seed, so the caller's RNG stream is untouched).
MORAN_NSIM <- 999L
MORAN_PERM_SEED <- 12345L

#' Two-sided permutation p-value by doubling the smaller tail, with the
#' (b + 1) / (m + 1) correction (Davison & Hinkley 1997; Phipson & Smyth 2010),
#' so p is never 0. NA when the observed statistic is not finite.
#'
#' A permuted statistic within `tol` of the observed one is a tie and counts in
#' both tails: equal I values summed in another order differ in the last bits
#' (e.g. the complete kNN graph of nine or fewer samples, where every
#' arrangement has I = -1/(n-1) and p must be 1).
moran_perm_p <- function(obs, sim, tol = sqrt(.Machine$double.eps) * max(1, abs(obs))) {
  if (length(obs) != 1L || !is.finite(obs)) return(NA_real_)
  m <- length(sim)
  upper <- (1 + sum(sim >= obs - tol)) / (m + 1)
  lower <- (1 + sum(sim <= obs + tol)) / (m + 1)
  min(1, 2 * min(upper, lower))
}

#' Moran cross-products z' W z of MORAN_NSIM permutations of the centred
#' residuals `zc`, for weights as an edge list (w[e] between zc[ei[e]] and
#' zc[ej[e]]). Drawn under with_seed(MORAN_PERM_SEED) as spdep::moran.mc draws
#' them; the caller scales by n / (S0 * sum(zc^2)). Scored in blocks of at
#' most `block_cells` values to bound memory.
.moran_perm_crossprods <- function(zc, ei, ej, w, block_cells = 2e6) {
  nsim <- MORAN_NSIM
  b <- max(1L, as.integer(block_cells %/% max(1L, length(ei))))
  out <- numeric(nsim)
  with_seed(MORAN_PERM_SEED, {
    s <- 1L
    while (s <= nsim) {
      e <- min(s + b - 1L, nsim)
      z <- vapply(s:e, function(k) sample(zc), numeric(length(zc)))
      out[s:e] <- colSums(w * z[ei, , drop = FALSE] * z[ej, , drop = FALSE])
      s <- e + 1L
    }
  })
  out
}

#' Residual Moran's I with its null expectation and significance.
#'
#' Returns `list(i, e_i, p)`: the statistic, E[I] = -1/(n-1) (negative, so I
#' alone misleads at small n) and a two-sided permutation p-value (MORAN_NSIM
#' seeded permutations; free of the normality the analytical tests assume,
#' which CV residuals often violate). spdep::moran.mc is not used for p: it
#' returns 0 when the observed I is the most extreme draw. All-NA means "not
#' computable", never "no spatial structure".
calc_moran <- function(residuals, coords) {
  na_res <- list(i = NA_real_, e_i = NA_real_, p = NA_real_)
  if (is.null(residuals) || is.null(coords)) return(na_res)
  n <- length(residuals)
  if (n < 3 || nrow(coords) != n) return(na_res)

  tryCatch({
    coords_matrix <- as.matrix(coords)
    if (any(duplicated(coords_matrix))) {
      # Separate duplicates (knearneigh() errors on them) under a seeded
      # sandbox. The jitter must exceed the coordinates' ULP (~9.3e-10 at a UTM
      # northing of 4.5e6) yet leave the neighbour graph unchanged: 1e-9 of the
      # span, floored at 1e-12 of the magnitude and at 1e-8.
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
    
    # Symmetric kNN contiguity (k = 8, capped at n - 1): scale-stable, unlike a
    # wide distance band that dilutes local autocorrelation. I is contingent on
    # this definition (scientific_guide.md).
    k_nn <- min(8L, nrow(coords) - 1L)
    # Muffle only spdep's expected "k greater than one-third" note at small n.
    nb <- withCallingHandlers(
      spdep::knn2nb(spdep::knearneigh(as.matrix(coords), k = k_nn), sym = TRUE),
      warning = function(w) {
        if (grepl("greater than one-third", conditionMessage(w))) invokeRestart("muffleWarning")
      }
    )

    lw <- spdep::nb2listw(nb, style = "W", zero.policy = TRUE)

    # I is spdep::moran(); the permutation draws reproduce moran.mc's without
    # its refusal of nsim > n! (which would push n <= 6 to the fallback).
    # Two-sided: strong negative autocorrelation is a misspecification signal too.
    cards <- spdep::card(lw$neighbours)
    n_eff <- n - sum(cards == 0L)
    s0 <- spdep::Szero(lw)
    i_obs <- spdep::moran(residuals, lw, n_eff, s0, zero.policy = TRUE)$I
    zc <- residuals - mean(residuals)
    sims <- (n_eff / s0) * .moran_perm_crossprods(
      zc, rep.int(seq_len(n), cards), unlist(lw$neighbours), unlist(lw$weights)) / sum(zc^2)
    return(list(i = i_obs, e_i = -1 / (n_eff - 1), p = moran_perm_p(i_obs, sims)))
  }, error = function(e) {
    if (n > 500) return(na_res)
    dists <- as.matrix(dist(coords))
    diag(dists) <- 0
    weights <- 1 / dists
    weights[is.infinite(weights)] <- 0
    diag(weights) <- 0
    # Row-standardised, as the primary path's nb2listw(style = "W").
    row_sums <- rowSums(weights, na.rm = TRUE)
    row_sums[row_sums == 0] <- 1
    weights <- weights / row_sums
    s0 <- sum(weights)
    diffs <- residuals - mean(residuals)
    ss <- sum(diffs^2)
    i_obs <- n * sum(weights * outer(diffs, diffs)) / (s0 * ss)
    # E[I] is a property of the null hypothesis, not of the weights. The
    # p-value is the kNN path's permutation test, run over THIS weight matrix.
    edge <- which(weights != 0, arr.ind = TRUE)
    sims <- n * .moran_perm_crossprods(diffs, edge[, 1], edge[, 2], weights[edge]) / (s0 * ss)
    return(list(i = i_obs, e_i = -1 / (n - 1), p = moran_perm_p(i_obs, sims)))
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
# Shared by perform_cv() and is_coord_col(); kept in a spatial fragment because
# PSOCK workers source only spatial_helpers.R.
.coord_names_x <- c("x", "lon", "long", "lng", "longitude", "easting")
.coord_names_y <- c("y", "lat", "latitude", "northing")

# A coordinate column is one whose whole (trimmed) name is a coordinate token;
# substring matching would catch variables like "Precipitation_cumulative".
is_coord_col <- function(x) {
  tolower(trimws(x)) %in% c(.coord_names_x, .coord_names_y)
}

#' Error metrics for one cross-validation object, at full precision.
#'
#' `moran = FALSE` skips residual Moran's I (repeated CV reports it for the
#' reference realization only: it diagnoses one residual field and is the
#' costliest term). `signed_target` is TRUE when the observations span zero
#' (NRMSE (mean) and SMAPE are then NA by definition); `block_fallback` when a
#' Spatial Block request fell back to random folds; `knndm_branch` is the fold
#' design a kNNDM request chose ("random", "spatial", "fallback", "none").
perform_cv <- function(cv_obj, moran = TRUE) {
  # n = predicted pairs, n_expected = rows with an observed value; metrics use
  # the predicted pairs only, and coverage says how many rows that is.
  res <- list(rmse = NA, r2 = NA, nse = NA, me = NA, mae = NA, ccc = NA,
              nrmse_mean = NA, nrmse_sd = NA, rpd = NA, rpiq = NA, smape = NA,
              moran_i = NA, moran_e = NA, moran_p = NA, n = 0,
              n_expected = 0, coverage = NA_real_, signed_target = NA,
              block_fallback = isTRUE(attr(cv_obj, "block_fallback")),
              knndm_branch = attr(cv_obj, "knndm")$branch %||% NA_character_)

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
  # Guard constant vectors explicitly: cor() would warn, and in a worker the
  # warning reaches the run log unexplained.
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
  
  # Exact names first, on is_coord_col()'s token lists: prefix matching could
  # let a covariate like "Longitude_deg" beat the x/y that .cv_to_df() supplies.
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
      # the two-sided permutation p alongside it.
      mor <- calc_moran(residuals, coords)
      res$moran_i <- mor$i
      res$moran_e <- mor$e_i
      # Every display formats the p-value through format_p_value().
      res$moran_p <- mor$p
  }
  
  return(res)
}

# ── Class-agreement metrics (the "Agreement (Kappa)" table) ──────────────────
# Bins observed/predicted into ordered classes and reports confusion-matrix
# agreement. Interval closure differs on purpose:
#   "agro"     - the applied class limits, [low, high) (right = FALSE) exactly
#                as terra::classify paints them, so table and map agree;
#   "quartile" - observed quartiles, cut()'s default right-closed reading (no
#                map counterpart).
# `params` (classification_params(): rcl_mat + labels) is read only for "agro".
# `status` is a message when not computable, else NULL; the binned factors are
# returned too.
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
    # Off-by-one accuracy: classes are ordered, so |rank difference| <= 1 counts.
    off_by_one    = safe(sum(abs(as.integer(act_bin) - as.integer(pred_bin)) <= 1) / length(act_bin)),
    mcc           = safe(yardstick::mcc_vec(act_bin, pred_bin)),
    kappa         = safe(yardstick::kap_vec(act_bin, pred_bin)),
    kappa_linear  = safe(yardstick::kap_vec(act_bin, pred_bin, weighting = "linear"))
  )
}

# ── Cross-validation fold planning ──────────────────────────────────────────
# One authority for the CV strategy, shared by make_cv_folds() and the UI label
# (applied_cv_plan(), ui_formatting.R):
#   "auto"  : LOOCV for n <= 50, seeded random 10-fold above
#   "loocv" : leave-one-out regardless of n
#   "knndm" : 10 folds matched to the map's prediction distances (knndm_folds);
#             LOOCV below CV_KNNDM_MIN_N
#   "block" : 10 k-means spatial folds; LOOCV below CV_BLOCK_MIN_N
CV_BLOCK_MIN_N <- 30L

# kNNDM (Linnenbrink, Mila, Ludwig & Meyer 2024). CV_KNNDM_MIN_N is the Spatial
# Block floor (>= 3 samples per fold at k = 10). maxp, the domain sample, the
# candidate count and the gate level are CAST::knndm's defaults (maxp 0.5,
# 1000 regular points, nk_len 100, KS p 0.05). Up to KNNDM_EXACT_MAX samples
# the Ward tree is built on the samples (16 MB distance matrix at 2000); above
# it on at most KNNDM_MAX_CELLS square cells, with KNNDM_NQ_CELLS candidate
# cuts (the same W as 100 cuts at 20k and 100k samples, three times faster).
CV_KNNDM_MIN_N <- 30L
KNNDM_MAXP <- 0.5
KNNDM_DOMAIN_N <- 1000L
KNNDM_EXACT_MAX <- 2000L
KNNDM_MAX_CELLS <- 2000L
KNNDM_NQ_EXACT <- 100L
KNNDM_NQ_CELLS <- 30L
KNNDM_KS_ALPHA <- 0.05

# The one fold seed every engine uses for its reported (reference) CV run.
# Repeated CV walks CV_FOLD_SEED + 1, + 2, ... so repeat 1 IS the reference
# realization: turning repeats on can never move the numbers already displayed.
CV_FOLD_SEED <- 12345L

# The fold count of every k-fold plan (random, kNNDM, Spatial Block), and of
# the random reference the CV Distance Match panel compares against.
CV_FOLD_K <- 10L

# Central authority for CV plan selection: maps (strategy, n) to a fold scheme
# (type, fold count k, human-readable label) so every caller builds folds and
# labels them identically, including the small-n degradations to LOOCV.
resolve_cv_plan <- function(strategy = "auto", n) {
  if (is.null(strategy) || length(strategy) != 1 || !nzchar(strategy)) strategy <- "auto"
  # An unknown strategy (stale or hand-edited config) degrades to "auto"
  # instead of failing inside a worker.
  if (!strategy %in% c("auto", "loocv", "knndm", "block")) strategy <- "auto"
  if (is.null(n) || length(n) == 0 || is.na(n)) return(list(type = "loocv", k = NA_integer_, label = "CV"))
  if (strategy == "loocv") return(list(type = "loocv", k = n, label = "Full LOOCV"))
  if (strategy == "knndm") {
    if (n < CV_KNNDM_MIN_N) return(list(type = "loocv", k = n, label = paste0("LOOCV [kNNDM needs n ≥ ", CV_KNNDM_MIN_N, "]")))
    return(list(type = "knndm", k = CV_FOLD_K, label = "kNNDM CV"))
  }
  if (strategy == "block") {
    if (n < CV_BLOCK_MIN_N) return(list(type = "loocv", k = n, label = paste0("LOOCV [Spatial Block needs n ≥ ", CV_BLOCK_MIN_N, "]")))
    return(list(type = "block", k = CV_FOLD_K, label = "Spatial Block CV"))
  }
  # auto
  if (n > 50) return(list(type = "random_kfold", k = CV_FOLD_K, label = "Random 10-fold CV"))
  list(type = "loocv", k = n, label = "LOOCV")
}

# The seeded, balanced random k-fold: Auto's folds above 50 samples, kNNDM's
# folds where they already match the map, and the reference of the CV
# Distance Match panel. One expression, so the three cannot drift apart.
cv_random_folds <- function(n, k = CV_FOLD_K, seed = CV_FOLD_SEED) {
  with_seed(seed, sample(rep(seq_len(k), length.out = n)))
}

# Integer fold ids of length n. `coords` is an n x 2 projected matrix (Spatial
# Block and kNNDM). Seeded under the RNG sandbox; only repeated CV varies
# `seed`. `domain_xy` (knndm_domain_points()) is read by kNNDM only, which
# returns the random folds without it.
make_cv_folds <- function(coords, strategy = "auto", n = NULL, seed = CV_FOLD_SEED,
                          domain_xy = NULL) {
  if (is.null(n)) n <- nrow(coords)
  plan <- resolve_cv_plan(strategy, n)

  switch(plan$type,
    loocv = seq_len(n),
    random_kfold = cv_random_folds(n, plan$k, seed),
    knndm = knndm_folds(coords, domain_xy, plan$k, KNNDM_MAXP, seed),
    block = with_seed(seed, {
      folds <- tryCatch({
        cm <- as.matrix(coords)[, 1:2, drop = FALSE]
        km <- stats::kmeans(cm, centers = plan$k, nstart = 5, iter.max = 50)
        as.integer(km$cluster)
      }, error = function(e) NULL)
      # Degenerate geometry can make k-means fail or collapse: fall back to a
      # seeded random k-fold, flagged so it is never reported as Spatial Block.
      # The draw continues the k-means stream, so it is not cv_random_folds().
      if (is.null(folds) || length(unique(folds)) < 2) {
        folds <- sample(rep(seq_len(plan$k), length.out = n))
        attr(folds, "block_fallback") <- TRUE
      }
      folds
    }))
}

# ── kNNDM (k-fold nearest-neighbour distance matching) ──────────────────────
# CV estimates the map's error when held-out samples are predicted from the
# distances at which the map predicts its cells (Mila et al. 2022). kNNDM
# (Linnenbrink et al. 2024, Geosci. Model Dev. 17:5897; CAST::knndm, Ward
# ward.D2 clustering) picks the k folds whose distributions match best:
#   Gj   each sample's distance to its nearest other sample,
#   Gij  each map location's distance to its nearest sample,
#   Gj*  each sample's distance to the nearest sample outside its fold.
# App conventions (Scientific Guide 5.1): the first principal axis's sign is
# fixed (same folds on every platform); above KNNDM_EXACT_MAX samples the tree
# is built on square cells; the random partition is always a candidate, so
# the folds never match the map worse than it.

#' Wasserstein-1 distance between the empirical distributions of `a` and `b`:
#' the integral of |F_a(t) - F_b(t)| dt over the pooled support, i.e. the area
#' between the two cumulative curves, in the units of the data. It equals
#' twosamples::wass_stat, which the reference implementation uses.
cv_wasserstein1 <- function(a, b) {
  a <- sort(a); b <- sort(b)
  x <- sort(c(a, b)); xm <- x[-length(x)]
  sum(abs(findInterval(xm, a) / length(a) - findInterval(xm, b) / length(b)) * diff(x))
}

#' Distance from every sample to the nearest sample outside its own fold (Gj*).
#' With one sample per fold (LOOCV) this is the sample-to-sample
#' nearest-neighbour distance. Co-located samples in different folds give 0.
cv_heldout_nnd <- function(xy, folds) {
  xy <- unname(as.matrix(xy)[, 1:2, drop = FALSE])
  if (!anyDuplicated(folds)) return(FNN::get.knn(xy, k = 1)$nn.dist[, 1])
  d <- numeric(nrow(xy))
  for (f in unique(folds)) {
    te <- folds == f
    d[te] <- FNN::get.knnx(xy[!te, , drop = FALSE], xy[te, , drop = FALSE], k = 1)$nn.dist[, 1]
  }
  d
}

#' The map's locations as kNNDM reads them: a regular square lattice of about
#' `n_target` points inside the clipping boundary (spacing
#' sqrt(area / n_target), divided by sqrt(2) while a thin boundary catches
#' fewer than half, five attempts at most). It depends on the boundary only,
#' never on the grid resolution. Without a boundary, a regular thinning of
#' `grid_xy`; NULL without either.
knndm_domain_points <- function(bound = NULL, grid_xy = NULL, n_target = KNNDM_DOMAIN_N) {
  if (!is.null(bound)) {
    pts <- tryCatch(.knndm_lattice(bound, n_target), error = function(e) NULL)
    if (NROW(pts)) return(pts)
  }
  if (!is.null(grid_xy) && NROW(grid_xy) > 0) {
    grid_xy <- as.matrix(grid_xy)
    keep <- unique(round(seq(1, nrow(grid_xy), length.out = min(n_target, nrow(grid_xy)))))
    return(unname(grid_xy[keep, 1:2, drop = FALSE]))
  }
  NULL
}

# Lattice cells rasterised per band of rows, so a boundary covering a small
# share of its bounding box (distant point buffers) never allocates the box.
.KNNDM_LATTICE_BLOCK <- 4e6

.knndm_lattice <- function(bound, n_target) {
  geom <- sf::st_union(sf::st_geometry(bound))
  area <- sum(as.numeric(sf::st_area(geom)))
  if (!is.finite(area) || area <= 0) return(NULL)
  bb <- sf::st_bbox(geom)
  v <- terra::vect(geom)
  s <- sqrt(area / n_target)
  # A side shorter than the spacing carries one lattice line, at its midpoint.
  axis_pts <- function(lo, hi) if (hi - lo <= s) (lo + hi) / 2 else seq(lo + s / 2, hi, by = s)
  inside <- matrix(numeric(0), 0, 2)
  for (attempt in 1:5) {
    xs <- axis_pts(bb[["xmin"]], bb[["xmax"]])
    ys <- axis_pts(bb[["ymin"]], bb[["ymax"]])
    rows_per_band <- max(1L, floor(.KNNDM_LATTICE_BLOCK / length(xs)))
    bands <- unname(split(seq_along(ys), ceiling(seq_along(ys) / rows_per_band)))
    inside <- do.call(rbind, lapply(bands, function(i) {
      r <- terra::rast(xmin = xs[1] - s / 2, xmax = xs[length(xs)] + s / 2,
                       ymin = ys[min(i)] - s / 2, ymax = ys[max(i)] + s / 2,
                       ncols = length(xs), nrows = length(i), crs = terra::crs(v))
      terra::crds(terra::rasterize(v, r), na.rm = TRUE)
    }))
    if (nrow(inside) >= n_target / 2) break
    s <- s / sqrt(2)
  }
  if (nrow(inside)) unname(inside[, 1:2, drop = FALSE]) else NULL
}

#' Square cells holding the samples, for the tree above KNNDM_EXACT_MAX: the
#' finest cell size with at most `max_cells` occupied cells (bisection on the
#' log cell size, 40 steps). Returns each sample's cell id (1..U, in order of
#' first appearance).
.knndm_cell_units <- function(xy, max_cells) {
  ext <- apply(xy, 2, range)
  span <- max(ext[2, ] - ext[1, ])
  key_at <- function(s) floor((xy[, 1] - ext[1, 1]) / s) * 1e7 + floor((xy[, 2] - ext[1, 2]) / s)
  lo <- log(span / 1e6); hi <- log(span + 1)
  for (i in 1:40) {
    mid <- (lo + hi) / 2
    if (length(unique(key_at(exp(mid)))) > max_cells) lo <- mid else hi <- mid
  }
  key <- key_at(exp(hi))
  match(key, unique(key))
}

#' Ward's tree above the cell level, from cell centroids `cen` and sizes `cnt`.
#' With dissimilarities sqrt(2 nA nB / (nA + nB)) |cA - cB| and the sizes as
#' `members`, hclust's ward.D2 reproduces every Ward merge above the cell level
#' (Lance & Williams 1967). Weighted one column at a time on the unclassed
#' vector (sub-assigning a classed "dist" copies it), so no U x U matrix exists.
.knndm_cell_tree <- function(cen, cnt) {
  U <- nrow(cen)
  cnt <- as.numeric(cnt)
  d <- unclass(stats::dist(cen))
  off <- 0L
  for (j in seq_len(U - 1L)) {
    i <- (j + 1L):U
    at <- off + seq_along(i)
    d[at] <- d[at] * sqrt(2 * cnt[j] * cnt[i] / (cnt[j] + cnt[i]))
    off <- off + length(i)
  }
  class(d) <- "dist"
  stats::hclust(d, method = "ward.D2", members = cnt)
}

#' The first principal axis of the sample coordinates (centred, unscaled), its
#' sign fixed so that its largest-magnitude loading is positive: an
#' eigenvector's sign is arbitrary, and it decides the order in which clusters
#' are dealt to folds.
.knndm_pc_axis <- function(xy) {
  pc <- stats::prcomp(xy, center = TRUE, scale. = FALSE, rank. = 1)
  axis <- pc$rotation[, 1]
  if (axis[which.max(abs(axis))] < 0) axis <- -axis
  list(center = pc$center, axis = unname(axis))
}

#' The reference merge of q clusters (`cl`, ids 1..q) into k folds: clusters
#' are ordered along the first principal axis by their centroids' scores; a
#' cluster of at least n/k samples keeps a fold of its own (ids 1, 2, ... in
#' that order); the others are dealt in turn, in that order, over the remaining
#' fold ids, so a fold gathers clusters from across the locality. NULL when the
#' large clusters leave no fold for the small ones.
.knndm_merge <- function(xy, cl, q, k, pc_center, pc_axis) {
  n <- nrow(xy)
  sizes <- tabulate(cl, nbins = q)
  cen <- rowsum(xy, cl) / sizes
  ord <- order(drop(sweep(cen, 2, pc_center) %*% pc_axis))
  big <- ord[sizes[ord] >= n / k]
  small <- ord[sizes[ord] < n / k]
  if (length(big) > k || (length(big) == k && length(small))) return(NULL)
  fk <- rep(NA_integer_, q)
  fk[big] <- seq_along(big)
  rest <- setdiff(seq_len(k), seq_along(big))
  if (length(small)) fk[small] <- rep(rest, ceiling(length(small) / length(rest)))[seq_along(small)]
  fk[cl]
}

#' kNNDM fold assignment for the samples `xy` (n x 2, projected) against the
#' map's locations `domain_xy`. Returns integer folds with attr(, "knndm") =
#' list(branch, q, k, W, W_random, ks_p, n_domain, units, exact). `branch`:
#'   "random"   the one-sided KS test does not find the map's distances larger
#'              than the sample spacing (p >= alpha), or no cluster partition
#'              beat the random partition;
#'   "spatial"  a cluster partition won (q clusters merged into k folds);
#'   "fallback" no candidate partition was valid; the random partition;
#'   "none"     no prediction domain; the random partition.
#' The random partition is `random_folds` (the Classification Suite's
#' class-stratified k-fold) or cv_random_folds(n, k, seed); it must hold k
#' folds, since W depends on the fold count. Candidates are CAST's plus the
#' random partition, and the smallest W wins. W (W_random) is the Wasserstein-1
#' distance of the chosen (random) folds' held-out distances from the map's.
knndm_folds <- function(xy, domain_xy, k = CV_FOLD_K, maxp = KNNDM_MAXP, seed = CV_FOLD_SEED,
                        exact_max = KNNDM_EXACT_MAX, max_cells = KNNDM_MAX_CELLS,
                        nq_exact = KNNDM_NQ_EXACT, nq_cells = KNNDM_NQ_CELLS,
                        alpha = KNNDM_KS_ALPHA, random_folds = NULL) {
  xy <- unname(as.matrix(xy)[, 1:2, drop = FALSE]); n <- nrow(xy)
  rnd <- if (is.null(random_folds)) cv_random_folds(n, k, seed) else as.integer(random_folds)
  if (length(rnd) != n) stop("The random partition has ", length(rnd), " fold ids for ", n, " samples.")
  tag <- function(folds, branch, W, W_random, q = NA_integer_, ks_p = NA_real_,
                  units = n, exact = TRUE) {
    folds <- as.integer(folds)
    attr(folds, "knndm") <- list(branch = branch, q = q, k = k, W = W, W_random = W_random,
                                 ks_p = ks_p, n_domain = if (is.null(domain_xy)) 0L else nrow(domain_xy),
                                 units = units, exact = exact)
    folds
  }
  if (is.null(domain_xy) || !nrow(domain_xy)) return(tag(rnd, "none", NA_real_, NA_real_))
  domain_xy <- as.matrix(domain_xy)[, 1:2, drop = FALSE]
  Gj <- FNN::get.knn(xy, k = 1)$nn.dist[, 1]
  Gij <- FNN::get.knnx(xy, domain_xy, k = 1)$nn.dist[, 1]
  W_rnd <- cv_wasserstein1(cv_heldout_nnd(xy, rnd), Gij)
  # H1: the samples' spacing is stochastically smaller than the map's distance
  # to the samples, i.e. the map predicts farther out than random folds would.
  ks_p <- suppressWarnings(stats::ks.test(Gj, Gij, alternative = "greater")$p.value)
  if (isTRUE(ks_p >= alpha)) return(tag(rnd, "random", W_rnd, W_rnd, ks_p = ks_p))

  exact <- n <= exact_max
  unit <- if (exact) seq_len(n) else .knndm_cell_units(xy, max_cells)
  U <- max(unit)
  # Candidates: the published cuts plus the random partition, which a cluster
  # partition must beat strictly (a gate that only just rejects, or the cell
  # path's missing fine end, can leave every cut worse). Balanced folds break
  # the cap only when maxp < 1/k.
  rnd_ok <- !any(tabulate(rnd, nbins = k) / n > maxp)
  best <- if (rnd_ok) list(folds = rnd, W = W_rnd, q = NA_integer_) else list(folds = NULL, W = Inf, q = NA_integer_)
  # The tree is built only when it has cuts to score: below k + 2 units (every
  # sample in one cell, say) the random partition is the only candidate.
  if (U - 2 >= k) {
    hc <- if (exact) stats::hclust(stats::dist(xy), method = "ward.D2") else {
      cnt <- tabulate(unit)
      .knndm_cell_tree(rowsum(xy, unit) / cnt, cnt)
    }
    pc <- .knndm_pc_axis(xy)
    qs <- unique(as.integer(round(exp(seq(log(k), log(U - 2),
                                          length.out = if (exact) nq_exact else nq_cells)))))
    for (q in qs) {
      folds <- .knndm_merge(xy, stats::cutree(hc, k = q)[unit], q, k, pc$center, pc$axis)
      if (is.null(folds) || any(tabulate(folds, nbins = k) / n > maxp)) next
      W <- cv_wasserstein1(cv_heldout_nnd(xy, folds), Gij)
      # Strictly smaller: a tie keeps the smaller q, the first minimum.
      if (W < best$W) best <- list(folds = folds, W = W, q = q)
    }
  }
  if (is.null(best$folds)) return(tag(rnd, "fallback", W_rnd, W_rnd, ks_p = ks_p, units = U, exact = exact))
  tag(best$folds, if (is.na(best$q)) "random" else "spatial", best$W, W_rnd, best$q, ks_p, U, exact)
}

#' What the CV Distance Match panel draws for one CV population: quantiles
#' (`probs`) of the `sample` (Gj), `map` (Gij) and `cv` (Gj* under `folds`)
#' distances, with W for these folds (`W_cv`) and for the random reference
#' (`W_random`: `random_folds` or cv_random_folds(), named by `reference`, `k`
#' folds), in the coordinates' `units`. Plain data for the future boundary.
#' NULL without a domain, below 3 samples, on mismatched folds or on error: a
#' diagnostic never stops the CV it describes.
cv_distance_summary <- function(xy, folds, domain_xy, k = CV_FOLD_K, seed = CV_FOLD_SEED,
                                probs = seq(0, 1, by = 0.01), units = NA_character_,
                                random_folds = NULL, reference = NULL) {
  if (is.null(domain_xy) || !NROW(domain_xy) || NROW(xy) < 3 || length(folds) != NROW(xy)) return(NULL)
  tryCatch({
    xy <- unname(as.matrix(xy)[, 1:2, drop = FALSE])
    domain_xy <- as.matrix(domain_xy)[, 1:2, drop = FALSE]
    rnd <- random_folds %||% cv_random_folds(nrow(xy), k, seed)
    k_ref <- length(unique(rnd))
    Gj <- FNN::get.knn(xy, k = 1)$nn.dist[, 1]
    Gij <- FNN::get.knnx(xy, domain_xy, k = 1)$nn.dist[, 1]
    Gcv <- cv_heldout_nnd(xy, folds)
    Grnd <- cv_heldout_nnd(xy, rnd)
    q <- function(v) unname(stats::quantile(v, probs, type = 7))
    list(probs = probs, sample = q(Gj), map = q(Gij), cv = q(Gcv),
         W_cv = cv_wasserstein1(Gcv, Gij), W_random = cv_wasserstein1(Grnd, Gij),
         n = nrow(xy), n_domain = nrow(domain_xy), k = k_ref,
         reference = reference %||% sprintf("random %d-fold", k_ref),
         units = as.character(units %||% NA_character_)[1])
  }, error = function(e) NULL)
}

#' The unit symbol of an sf object's coordinates ("m" for every working CRS
#' the interpolation pipeline projects to), NA when the CRS declares none.
crs_unit_label <- function(x) {
  u <- tryCatch(sf::st_crs(x)$units, error = function(e) NULL)
  if (is.character(u) && length(u) == 1 && nzchar(u)) u else NA_character_
}

#' The run-log line of a kNNDM request for one surface of locality `loc`, from
#' knndm_folds()'s record `info`, or the LOOCV note when `n` < CV_KNNDM_MIN_N.
#' Formatted without ui_formatting.R (workers do not load it). NULL when there
#' is nothing to report.
knndm_log_line <- function(info, loc, surface, n, units = NA_character_) {
  head <- sprintf("[CV] %s (%s): ", loc, surface)
  if (is.null(info)) {
    if (length(n) == 1 && isTRUE(n < CV_KNNDM_MIN_N)) {
      return(sprintf("%skNNDM needs %d samples; LOOCV used (%d samples).", head, CV_KNNDM_MIN_N, as.integer(n)))
    }
    return(NULL)
  }
  u <- if (is.na(units)) " map units" else paste0(" ", units)
  d <- function(x) paste0(format(signif(x, 4), trim = TRUE, drop0trailing = TRUE), u)
  cells <- if (isFALSE(info$exact)) sprintf(" Tree built on %d cells of samples (n = %d).", info$units, as.integer(n)) else ""
  switch(info$branch %||% "",
    random = if (isTRUE(info$ks_p >= KNNDM_KS_ALPHA)) {
      sprintf("%skNNDM chose random folds; the map's distances do not exceed the sample spacing (KS p = %.3f); W = %s.",
              head, info$ks_p, d(info$W))
    } else {
      sprintf("%skNNDM chose random folds; they matched the map's distances better than any spatial partition; W = %s.%s",
              head, d(info$W), cells)
    },
    spatial = sprintf("%skNNDM chose spatial folds (%d clusters merged into %d folds); W = %s against %s for random folds.%s",
                      head, as.integer(info$q), as.integer(info$k), d(info$W), d(info$W_random), cells),
    fallback = sprintf("%skNNDM found no valid spatial partition; seeded random %d-fold used.", head, as.integer(info$k)),
    none = sprintf("%skNNDM had no prediction domain; seeded random %d-fold used.", head, as.integer(info$k)),
    NULL)
}

# Row identity of a CV population: the row number in the uploaded table, added
# at dispatch. Direct engine calls have none and number their rows 1..n.
CV_ROW_ID_COL <- ".mn_row_id"

#' The CV plan of one population: its row ids and one fold vector per
#' realization from make_cv_folds(), so the folds do not depend on the engine.
#' `domain_xy` (knndm_domain_points()) drives kNNDM, and `design`
#' (cv_distance_summary(), NULL without it) records how realization 1 matches
#' the map. A kNNDM plan with spatial folds is deterministic: one realization.
build_cv_plan <- function(pts, strategy = "auto", repeats = 1L, domain_xy = NULL) {
  n <- nrow(pts)
  row_id <- if (CV_ROW_ID_COL %in% names(pts)) as.integer(pts[[CV_ROW_ID_COL]]) else seq_len(n)
  coords <- sf::st_coordinates(pts)[, 1:2, drop = FALSE]
  first <- make_cv_folds(coords, strategy, n, CV_FOLD_SEED, domain_xy)
  reps <- if (identical(attr(first, "knndm")$branch, "spatial")) 1L else cv_repeat_count(repeats, strategy, n)
  folds <- c(list(first), lapply(seq_len(reps)[-1], function(r) {
    make_cv_folds(coords, strategy, n, CV_FOLD_SEED + r - 1L, domain_xy)
  }))
  list(row_id = row_id, n = n, strategy = strategy,
       label = resolve_cv_plan(strategy, n)$label, folds = folds,
       design = cv_distance_summary(coords, first, domain_xy, CV_FOLD_K, CV_FOLD_SEED,
                                    units = crs_unit_label(pts)))
}

#' Run one kriging CV realization fold by fold.
#'
#' `fold_fun(train, newdata, i)` fits on the fold's training rows and returns
#' `list(pred, var = NULL, meta = NULL)` for `newdata` (held-out coordinates
#' only). A failing fold gives NA rows and a note, never another engine's
#' values. Folds run sequentially (the nested worker already holds the cores).
#' Returns the common kriging CV schema in population order, with
#' `attr(, "cv_notes")` and `attr(, "cv_fold_meta")`.
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
  attr(cv, "knndm") <- attr(folds, "knndm")
  cv
}

# ── Repeated cross-validation (opt-in) ──────────────────────────────────────
# A k-fold estimate is ONE realization of a random partition; repeating it
# under alternative folds and reporting mean +/- SD separates model skill from
# split luck. Repeat 1 keeps CV_FOLD_SEED, so the reference run and the
# surface never change.

# How many fold realizations to run, clamped to 1..25. LOOCV is deterministic
# and always gets 1; a kNNDM plan with spatial folds is cut to 1 by
# build_cv_plan() / add_cv_repeats(), which see the folds.
cv_repeat_count <- function(n_repeats, strategy = "auto", n = NULL) {
  if (is.null(n_repeats) || length(n_repeats) != 1) return(1L)
  n_repeats <- suppressWarnings(as.integer(n_repeats))
  if (is.na(n_repeats) || n_repeats < 2L) return(1L)
  n_repeats <- min(n_repeats, 25L)
  if (identical(resolve_cv_plan(strategy, n)$type, "loocv")) return(1L)
  n_repeats
}

# A CV object reduced to observed, prediction and geometry under fixed names,
# so the CV frames of different engines rbind safely in pool_cv_sf().
# perform_cv() reads only these columns.
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

# Metrics with a meaningful spread across fold realizations (Moran's I is
# excluded, see perform_cv()). NRMSE (SD) stays defined for zero-spanning
# targets.
CV_REPEAT_METRICS <- c(rmse = "RMSE", nrmse_mean = "NRMSE (mean, %)",
                       nrmse_sd = "NRMSE (SD)", mae = "MAE",
                       r2 = "R² (Corr)", nse = "R² (NSE/Trad)", me = "Bias (ME)",
                       ccc = "Lin's CCC (Agree)", rpd = "RPD (Prec)",
                       rpiq = "RPIQ", smape = "SMAPE (%)")

# Labels of the Model Performance table and its export, built from
# CV_REPEAT_METRICS so the two tables cannot disagree. Kept here because
# global.R sources ui_helpers.R before spatial_helpers.R.
CV_METRIC_LABELS <- c(CV_REPEAT_METRICS,
                      moran_i = "Moran's I", moran_e = "Moran E[I]",
                      moran_p = "Moran p")

# Mean / SD across fold realizations; a metric undefined in ANY repeat is NA
# rather than averaged over a varying subset.
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

# Main-session assembly of a run's repeated-CV report from `reps_by_loc`
# (locality -> list of CV frames). A locality with one frame has a
# deterministic plan (LOOCV, kNNDM spatial) and is recycled into every pooled
# repeat, which is exact and keeps the pooled locality set unchanged.
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
#' re-screens the covariates (from `candidates`, default `aux_vars`) and refits
#' the covariate surfaces, the trend and the residual variogram on its training
#' rows, predicting trend + kriged residual (run_kriging_folds()). `folds` /
#' `row_id` come from the run's CV plan; with `folds = NULL` they are built
#' from `cv_strategy`, `fold_seed` and `cv_domain_xy`. Returns the common
#' kriging CV schema (`var1.var` NA), or NULL below 3 complete-case rows.
perform_kriging_loocv <- function(pts, target_var, aux_vars, lags_func, vgm_fit_func, model_type = c("lm", "rf"), l = "region", prefix = "act", rf_ntree = 200, cv_strategy = "auto", fold_seed = CV_FOLD_SEED, cov_params = list(),
                                  folds = NULL, row_id = NULL, cancel_file = NULL, progress = NULL,
                                  candidates = NULL, vif_threshold = 10, cv_domain_xy = NULL) {
  model_type <- match.arg(model_type)
  candidates <- candidates %||% aux_vars
  pts <- pts[complete.cases(sf::st_drop_geometry(pts)[, c(target_var, aux_vars), drop=FALSE]), ]
  n <- nrow(pts)
  if (n < 3) return(NULL)

  # `fold_seed` moves only under repeated CV; per-fold model draws are seeded
  # from the fold label, so a repeat varies the partition only.
  if (is.null(folds)) {
    folds <- make_cv_folds(sf::st_coordinates(pts), cv_strategy, n, fold_seed, cv_domain_xy)
  } else if (length(folds) != n) {
    stop("The CV fold vector has ", length(folds), " entries for ", n, " complete-case samples.")
  }
  if (is.null(row_id)) {
    row_id <- if (CV_ROW_ID_COL %in% names(pts)) as.integer(pts[[CV_ROW_ID_COL]]) else seq_len(n)
  }

  # Rank guard: a fold's lm trend needs more training rows than coefficients
  # (covariates + 1); otherwise it is rank-deficient or saturated and the CV
  # metrics degrade silently, so fail loudly (safe_run_cv logs it). Factor
  # covariates add coefficients, so the check is necessary, not sufficient.
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
    # The covariate screen is data-driven, so it runs on the training rows only.
    kept <- screen_covariates(train, candidates, vif_threshold)$kept
    if (!length(kept)) stop("the covariate screen removed every covariate in this fold")
    if (model_type == "lm" && nrow(train) < length(kept) + 2L) {
      stop(sprintf("%d training rows cannot fit %d regression coefficients",
                   nrow(train), length(kept) + 1L))
    }
    lags <- lags_func(train)
    kc <- krige_covariates(train, newdata, kept, lags, cov_params)
    test_cov <- sf::st_drop_geometry(kc$grid_aux)

    if (model_type == "lm") {
      form_i <- as.formula(paste0("`", target_var, "` ~ ",
                                  paste(paste0("`", kept, "`"), collapse = " + ")))
      lm_mod <- lm(form_i, data = train)
      train$residuals <- residuals(lm_mod)
      pred_trend <- predict(lm_mod, newdata = test_cov)
    } else {
      # Each fold's forest has its own seed (from the fold label): a shared
      # stream would let earlier folds' draw counts move fold i's forest. Matrix
      # interface, as for the map's forest: the formula method cannot handle
      # non-syntactic covariate names.
      rf_mod <- with_seed(CV_FOLD_SEED + as.integer(i),
                          randomForest::randomForest(x = sf::st_drop_geometry(train)[kept],
                                                     y = train[[target_var]], ntree = rf_ntree))
      train$residuals <- train[[target_var]] - rf_mod$predicted
      pred_trend <- predict(rf_mod, test_cov[kept])
    }

    v_emp <- variogram(residuals ~ 1, train, width = lags$width, cutoff = lags$cutoff)
    v_fit <- vgm_fit_func(v_emp, train$residuals)
    res_krig <- krige(residuals ~ 1, train, newdata, model = v_fit, debug.level = 0)
    # Per-fold fit state and covariate fallbacks, for the run log.
    list(pred = as.numeric(pred_trend) + res_krig$var1.pred,
         meta = list(kept = kept, vgm_status = vgm_fit_status(v_fit),
                     cov_fallback = kc$fallback))
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

# Pool per-locality CV objects into ONE sf in a common METRIC CRS for the
# "Total (Combined)" diagnostics: the UTM zone of the combined centroid (as in
# validate_and_project_sf()), never Web Mercator, whose distances are inflated
# by 1/cos(latitude). Non-spatial entries are skipped (no knowable CRS); every
# spatial entry is reduced to cv_repeat_frame() columns, and one that cannot be
# reduced or reprojected makes the pool NULL, so a subset is never reported.
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
  # NULL rather than a partial pool reported as "Total (Combined)".
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
