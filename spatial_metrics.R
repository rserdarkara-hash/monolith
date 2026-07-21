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
  if (length(observed) < 2) return(NA)
  
  mean_obs <- mean(observed, na.rm = TRUE)
  mean_pred <- mean(predicted, na.rm = TRUE)
  
  var_obs <- var(observed, na.rm = TRUE)
  var_pred <- var(predicted, na.rm = TRUE)
  
  if (is.na(var_obs) || is.na(var_pred) || var_obs == 0 || var_pred == 0) {
    # CCC is undefined when either vector is constant: the correlation term
    # does not exist (0/0). Report NA rather than asserting agreement.
    return(NA)
  }
  
  sd_obs <- sqrt(var_obs)
  sd_pred <- sqrt(var_pred)
  
  cov_op <- cov(observed, predicted, use = "pairwise.complete.obs")
  rho <- cov_op / (sd_obs * sd_pred)
  
  numerator <- 2 * rho * sd_obs * sd_pred
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
augment_metrics <- function(obs, pre) {
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
  res$nse <- if (is.finite(sst) && sst > 0) round(1 - (sse / sst), 4) else NA

  # Relative RMSE is undefined for a zero-mean variable (centred/anomaly data).
  res$nrmse_mean <- if (is.finite(mean_obs) && abs(mean_obs) > 0) round((rmse / mean_obs) * 100, 2) else NA

  # RPD / RPIQ are spread-to-error ratios (Chang et al. 2001): undefined at
  # zero error, so both guard on rmse > 0, not just RPIQ's spread term.
  res$rpd <- if (is.finite(rmse) && rmse > 0) round(sd_obs / rmse, 2) else NA
  res$rpiq <- if (is.finite(rmse) && rmse > 0 && iqr_obs > 0) round(iqr_obs / rmse, 2) else NA

  # sMAPE's summand is 0/0 where obs == pre == 0. Dropping those rows via
  # na.rm would average sMAPE over a different n than every other metric;
  # define the term as 0 instead (the usual convention) so n stays consistent.
  denom <- abs(obs) + abs(pre)
  term <- ifelse(denom == 0, 0, 2 * abs(residuals) / denom)
  res$smape <- round(mean(term, na.rm = TRUE) * 100, 2)

  return(res)
}

calc_moran <- function(residuals, coords) {
  if (is.null(residuals) || is.null(coords)) return(NA)
  n <- length(residuals)
  if (n < 3 || nrow(coords) != n) return(NA)
  
  tryCatch({
    coords_matrix <- as.matrix(coords)
    if (any(duplicated(coords_matrix))) {
      # Separate duplicates under a sandboxed RNG so Moran's I is
      # reproducible and the caller's RNG stream is not perturbed
      if (exists(".Random.seed", envir = .GlobalEnv)) {
        old_seed <- get(".Random.seed", envir = .GlobalEnv)
        on.exit(assign(".Random.seed", old_seed, envir = .GlobalEnv))
      }
      set.seed(12345)
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
      span <- suppressWarnings(max(diff(range(coords_matrix[, 1], na.rm = TRUE)),
                                   diff(range(coords_matrix[, 2], na.rm = TRUE))))
      mag <- suppressWarnings(max(abs(coords_matrix), na.rm = TRUE))
      if (!is.finite(span)) span <- 0
      if (!is.finite(mag)) mag <- 0
      jit_amt <- max(1e-8, span * 1e-9, mag * 1e-12)
      coords_matrix[,1] <- jitter(coords_matrix[,1], amount = jit_amt)
      coords_matrix[,2] <- jitter(coords_matrix[,2], amount = jit_amt)
      coords <- coords_matrix
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
    
    m_res <- spdep::moran.test(residuals, lw, zero.policy = TRUE, randomisation = FALSE)
    return(as.numeric(m_res$estimate[1]))
  }, error = function(e) {
    if (n > 500) return(NA)
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
    return(numerator / denominator)
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

perform_cv <- function(cv_obj) {
  res <- list(rmse = NA, r2 = NA, nse = NA, me = NA, mae = NA, ccc = NA, 
              nrmse_mean = NA, rpd = NA, rpiq = NA, smape = NA, moran_i = NA, n = 0)
  
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
  
  res$rmse <- round(sqrt(mean(residuals^2, na.rm = TRUE)), 4)
  res$me <- round(mean(residuals, na.rm = TRUE), 4)
  res$mae <- round(mean(abs(residuals), na.rm = TRUE), 4)
  r2_val <- tryCatch(cor(obs, pre)^2, error = function(e) NA)
  res$r2 <- round(r2_val, 4)
  res$n <- length(obs)
  
  res$ccc <- round(calc_ccc(obs, pre), 4)
  aug <- augment_metrics(obs, pre)
  res$nse <- aug$nse
  res$nrmse_mean <- aug$nrmse_mean
  res$rpd <- aug$rpd
  res$rpiq <- aug$rpiq
  res$smape <- aug$smape
  
  # Exact-name matching first, mirroring is_coord_col() (ui_formatting.R, which
  # workers do not source): prefix matching let a covariate named e.g.
  # "Longitude_deg" win over the guaranteed x/y columns, because [1] takes the
  # first match in COLUMN order, not pattern order. .cv_to_df() always supplies
  # x/y for sf and Spatial inputs, so the exact names normally settle it.
  pick_coord <- function(cn, exact, fallback) {
    hit <- cn[tolower(cn) %in% exact]
    if (length(hit)) return(hit[1])
    grep(fallback, cn, ignore.case = TRUE, value = TRUE)[1]
  }
  x_col <- pick_coord(cnames, c("x", "lon", "long", "lng", "longitude", "easting"), "^easting")
  y_col <- pick_coord(cnames, c("y", "lat", "latitude", "northing"), "^northing")
  if (!is.na(x_col) && !is.na(y_col)) {
      coords <- df[valid, c(x_col, y_col)]
      res$moran_i <- round(calc_moran(residuals, coords), 4)
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
# (12345) under a two-sided RNG sandbox so folds are reproducible and the
# caller's .Random.seed is preserved (same convention as calc_moran).
make_cv_folds <- function(coords, strategy = "auto", n = NULL) {
  if (is.null(n)) n <- nrow(coords)
  plan <- resolve_cv_plan(strategy, n)

  if (plan$type == "loocv") return(seq_len(n))

  old_seed <- if (exists(".Random.seed", envir = .GlobalEnv, inherits = FALSE)) get(".Random.seed", envir = .GlobalEnv, inherits = FALSE) else NULL
  on.exit({
    if (!is.null(old_seed)) assign(".Random.seed", old_seed, envir = .GlobalEnv)
    else if (exists(".Random.seed", envir = .GlobalEnv, inherits = FALSE)) rm(".Random.seed", envir = .GlobalEnv)
  }, add = TRUE)
  set.seed(12345)

  if (plan$type == "block") {
    folds <- tryCatch({
      cm <- as.matrix(coords)[, 1:2, drop = FALSE]
      km <- stats::kmeans(cm, centers = plan$k, nstart = 5, iter.max = 50)
      as.integer(km$cluster)
    }, error = function(e) NULL)
    # Degenerate geometry (e.g. many duplicate coordinates) can make k-means
    # fail or collapse; fall back to a seeded random k-fold rather than losing
    # CV entirely.
    if (is.null(folds) || length(unique(folds)) < 2) {
      folds <- sample(rep(seq_len(plan$k), length.out = n))
    }
    return(folds)
  }

  # random_kfold: balanced, seeded
  sample(rep(seq_len(plan$k), length.out = n))
}

perform_kriging_loocv <- function(pts, target_var, aux_vars, lags_func, vgm_fit_func, model_type = c("lm", "rf"), l = "region", prefix = "act", rf_ntree = 200, cv_strategy = "auto") {
  model_type <- match.arg(model_type)
  pts <- pts[complete.cases(sf::st_drop_geometry(pts)[, c(target_var, aux_vars), drop=FALSE]), ]
  n <- nrow(pts)
  if (n < 3) return(NULL)
  form_reg <- as.formula(paste0("`", target_var, "` ~ ", paste(paste0("`", aux_vars, "`"), collapse = " + ")))

  pts$orig_idx <- seq_len(n)

  # Fold assignment (make_cv_folds seeds itself); computed here on the
  # complete-case rows so the fold vector length always matches n.
  folds <- make_cv_folds(sf::st_coordinates(pts), cv_strategy, n)

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
  old_seed <- if (exists(".Random.seed", envir = .GlobalEnv, inherits = FALSE)) get(".Random.seed", envir = .GlobalEnv, inherits = FALSE) else NULL
  on.exit({
    if (!is.null(old_seed)) assign(".Random.seed", old_seed, envir = .GlobalEnv)
    else if (exists(".Random.seed", envir = .GlobalEnv, inherits = FALSE)) rm(".Random.seed", envir = .GlobalEnv)
  }, add = TRUE)
  set.seed(12345)

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
  
  return(res_combined)
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
