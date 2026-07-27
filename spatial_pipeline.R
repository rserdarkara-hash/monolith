# spatial_pipeline.R - regional orchestration + parallel worker entry points
# (run_regional_interpolation, interp_run_item, autofit_vgm_item, tps_gcv_item,
# idw_opt_item), progress/warning files, CRS projection, dedup, raster and
# class-break utilities. Sourced via spatial_helpers.R.


update_progress_file <- function(l, prefix, step, total) {
  clean_l <- gsub("[^a-zA-Z0-9_]", "_", as.character(l))
  
  progress_dir <- getOption("monolith_progress_dir", tempdir())
  session_id <- getOption("monolith_session_id", "default")
  
  if (!dir.exists(progress_dir)) {
    tryCatch(dir.create(progress_dir, recursive = TRUE, showWarnings = FALSE), error = function(e) NULL)
  }
  
  file_name <- file.path(progress_dir, paste0("progress_", session_id, "_", clean_l, "_", prefix, ".txt"))
  pct <- round((step / total) * 100)
  
  tryCatch({
    writeLines(as.character(pct), file_name)
  }, error = function(e) {
  })
}

write_warning_file <- function(l, prefix, message) {
  clean_l <- gsub("[^a-zA-Z0-9_]", "_", as.character(l))
  progress_dir <- getOption("monolith_progress_dir", tempdir())
  session_id <- getOption("monolith_session_id", "default")
  
  if (!dir.exists(progress_dir)) {
    tryCatch(dir.create(progress_dir, recursive = TRUE, showWarnings = FALSE), error = function(e) NULL)
  }
  
  file_name <- file.path(progress_dir, paste0("warn_", session_id, "_", clean_l, "_", prefix, ".txt"))
  tryCatch({
    writeLines(as.character(message), file_name)
  }, error = function(e) NULL)
}

get_joint_scale_values <- function(r1_packed, r2_packed, match_scales, is_uncertainty) {
  if(match_scales && !is_uncertainty) {
    res <- c(raster_value_layer(r1_packed), raster_value_layer(r2_packed))
    if(length(res) == 0) return(NULL)
    return(res)
  }
  return(NULL)
}


compute_governing_factors <- function(df, target_col, predictors, n_permutations = 10, rf_ntree = 100, shap_sample_size = 100, cores_hint = NULL, cancel_file = NULL) {
  req_cols <- c(target_col, predictors)
  df_clean <- df[, req_cols, drop = FALSE]
  df_clean <- df_clean[complete.cases(df_clean), , drop = FALSE]

  if (nrow(df_clean) < 10) return(NULL) # Not enough data

  # Cooperative cancellation, same file-flag contract as the classification
  # pipeline: the module touches `cancel_file` from the main session and the
  # worker aborts at its next checkpoint. Kept as a LOCAL closure rather than a
  # top-level helper because this function crosses the future boundary through
  # future's automatic global detection (the gov worker source()s nothing), so
  # a new global would be one more thing that has to be discovered correctly.
  # The message is matched by the module's error handler; keep them in sync.
  check_cancel <- function() {
    if (!is.null(cancel_file) && file.exists(cancel_file)) {
      stop("Analysis cancelled by user.", call. = FALSE)
    }
  }
  check_cancel()

  # Two-sided seed sandbox (same convention as perform_kriging_loocv):
  # restore the caller's RNG state, or remove the state set.seed() created
  old_seed <- if (exists(".Random.seed", envir = .GlobalEnv, inherits = FALSE)) get(".Random.seed", envir = .GlobalEnv, inherits = FALSE) else NULL
  on.exit({
    if (!is.null(old_seed)) assign(".Random.seed", old_seed, envir = .GlobalEnv)
    else if (exists(".Random.seed", envir = .GlobalEnv, inherits = FALSE)) rm(".Random.seed", envir = .GlobalEnv)
  }, add = TRUE)

  formula_str <- paste(target_col, "~ .")
  set.seed(12345)
  rf_model <- randomForest::randomForest(as.formula(formula_str), data = df_clean, ntree = rf_ntree, importance = TRUE)
  
  explainer_rf <- DALEX::explain(
    model = rf_model, 
    data = df_clean[, predictors, drop = FALSE], 
    y = df_clean[[target_col]], 
    label = "Random Forest",
    verbose = FALSE
  )
  
  # model_parts runs `n_permutations` full permutation passes internally and
  # cannot be interrupted mid-call, so this checkpoint bounds cancel latency at
  # one importance run (as the tuning grid search does in the classifier).
  check_cancel()
  vip <- DALEX::model_parts(explainer_rf, B = n_permutations)

  vip_df <- as.data.frame(vip)
  vip_df <- vip_df[vip_df$variable != "_baseline_" & vip_df$variable != "_full_model_", ]
  vip_agg <- aggregate(dropout_loss ~ variable, data = vip_df, FUN = mean)
  top_var <- as.character(vip_agg$variable[which.max(vip_agg$dropout_loss)])
  
  check_cancel()
  ale_prof <- DALEX::model_profile(explainer_rf, variables = top_var, type = "accumulated")
  ale_df <- as.data.frame(ale_prof$agr_profiles)

  pdp_prof <- DALEX::model_profile(explainer_rf, variables = top_var, type = "partial")
  pdp_df <- as.data.frame(pdp_prof$agr_profiles)

  check_cancel()
  set.seed(12345)
  sample_idx <- sample(seq_len(nrow(df_clean)), min(shap_sample_size, nrow(df_clean)))

  # Per-observation SHAP is embarrassingly parallel. This runs inside the
  # gov-module future_promise worker, where the nested plan is sequential, so
  # escalate only for workloads big enough to amortize worker startup.
  # seed = TRUE gives each observation its own L'Ecuyer RNG stream, making
  # results identical under any plan (sequential or parallel).
  # A PSOCK worker reports availableCores() = 1 (the same constraint the run
  # pipeline hits), so the caller passes the core count in from the main
  # session (cores_hint); the in-process default keeps direct calls (tests,
  # scripts) working. The cluster is owned explicitly and torn down on exit,
  # with mc.cores restored for the reused promise worker.
  if (is.null(cores_hint)) {
    cores_hint <- tryCatch(as.integer(future::availableCores()), error = function(e) 1L)
  }
  n_shap_workers <- min(max(0L, cores_hint - 1L), 8L)
  if (length(sample_idx) >= 50 && n_shap_workers >= 2L && future::nbrOfWorkers() == 1L) {
    old_mc_cores <- getOption("mc.cores")
    options(mc.cores = n_shap_workers)
    shap_cl <- parallelly::makeClusterPSOCK(n_shap_workers)
    old_plan <- future::plan(future::cluster, workers = shap_cl)
    on.exit({
      future::plan(old_plan)
      parallel::stopCluster(shap_cl)
      options(mc.cores = old_mc_cores)
    }, add = TRUE)
  }
  # Per-observation cancel check. Deliberately an inline file.exists() on a
  # PLAIN character path, not a call to check_cancel(): shipping that closure
  # would drag its enclosing frame (df, df_clean, explainer_rf, ...) into every
  # worker as an extra global, which is exactly the serialization blow-up the
  # interpolation pipeline was bitten by. file.exists consumes no RNG, so the
  # per-observation L'Ecuyer streams — and every SHAP value — are unchanged.
  cancel_path <- cancel_file
  shap_list <- furrr::future_map(sample_idx, function(i) {
    if (!is.null(cancel_path) && file.exists(cancel_path)) {
      stop("Analysis cancelled by user.", call. = FALSE)
    }
    sp <- DALEX::predict_parts(explainer_rf, new_observation = df_clean[i, predictors, drop = FALSE], type = "shap")
    sp <- as.data.frame(sp)
    sp$obs_id <- i
    sp
  }, .options = furrr::furrr_options(seed = TRUE, packages = c("DALEX", "randomForest")))
  shap_df <- do.call(rbind, shap_list)
  check_cancel()

  # predict_parts(type = "shap") returns B+1 rows per variable: the aggregated
  # attribution (B == 0) plus one row per permutation. Summing them inflates the
  # value by a factor of B+1, so only the aggregated B == 0 row is used.
  # ONE pass: the previous per-observation subset rescanned all
  # shap_sample_size x (B+1) x n_predictors rows for each sampled observation
  # (three full-length comparisons each). Filtering once and grouping by obs_id
  # gives identical values (the per-observation mean of the same rows).
  top_rows <- shap_df[shap_df$variable_name == top_var & shap_df$B == 0, , drop = FALSE]
  contrib_by_obs <- if (nrow(top_rows) > 0) {
    tapply(top_rows$contribution, as.character(top_rows$obs_id), mean)
  } else {
    numeric(0)
  }
  # as.numeric(): tapply returns a 1-d array, and the column must be plain
  # numeric like the sapply() it replaces.
  contribution <- as.numeric(contrib_by_obs[as.character(sample_idx)])
  contribution[is.na(contribution)] <- 0

  shap_val_df <- data.frame(
    feature_value = df_clean[[top_var]][sample_idx],
    contribution = contribution
  )

  list(
    model = rf_model,
    explainer = explainer_rf,
    importance = vip_agg,
    top_var = top_var,
    ale = ale_df,
    pdp = pdp_df,
    shap = shap_val_df
  )
}

merge_wrapped_rasters <- function(raster_list) {
  if (is.null(raster_list) || length(raster_list) == 0) return(NULL)
  valid_list <- Filter(Negate(is.null), raster_list)
  if (length(valid_list) == 0) return(NULL)
  
  unwrap_if_needed <- function(r) {
    if (inherits(r, "PackedSpatRaster")) terra::unwrap(r) else r
  }
  
  merged <- if (length(valid_list) > 1) {
    do.call(terra::merge, lapply(unname(valid_list), unwrap_if_needed))
  } else {
    unwrap_if_needed(valid_list[[1]])
  }
  merged
}

validate_and_project_sf <- function(pts_sf) {
  if (is.null(pts_sf) || nrow(pts_sf) == 0) return(NULL)
  
  if (sf::st_is_longlat(pts_sf)) {
    coords_4326 <- sf::st_coordinates(sf::st_transform(pts_sf, 4326))
    lon_c <- mean(coords_4326[, 1], na.rm = TRUE)
    lat_c <- mean(coords_4326[, 2], na.rm = TRUE)
    if (is.na(lon_c) || is.na(lat_c)) {
      stop("Calculated geographic center contains NA.")
    }
    utm_zone <- floor((lon_c + 180) / 6) + 1
    utm_crs <- paste0("+proj=utm +zone=", utm_zone, " +datum=WGS84 +units=m +no_defs")
    if (lat_c < 0) utm_crs <- paste0(utm_crs, " +south")
    
    pts_sf <- sf::st_transform(pts_sf, utm_crs)
  }
  
  return(pts_sf)
}

# Build the point set for one interpolation surface: drop rows whose `target`
# value is NA FIRST, then remove points sharing the same coordinate (rounded to
# 2 dp). Order matters scientifically: deduping before NA-filtering (as the
# shared pass upstream does) lets a co-located point with a missing target evict
# a valid neighbour, silently discarding a real observation and making the run
# fit a different point set than the auto-fit variogram preview showed (which
# na.omit()s before deduping). Filtering first keeps the valid measurement and
# keeps preview and run consistent. Returns an sf with refreshed x/y columns.
dedup_valid_points <- function(pts_sf, target) {
  pts_sf <- pts_sf[!is.na(pts_sf[[target]]), ]
  if (nrow(pts_sf) == 0) return(pts_sf)
  pts_sf <- pts_sf[!duplicated(round(sf::st_coordinates(pts_sf), 2)), ]
  cc <- sf::st_coordinates(pts_sf)
  dplyr::mutate(pts_sf, x = cc[, 1], y = cc[, 2])
}

run_regional_interpolation <- function(item, current_method, current_crs, aux_vars, shp_bound, b_type, buff_mode, b_dist, res_mode, grid_res, crs_sel, comp_mode, val_type, progress_dir_val = tempdir(), session_id_val = "default", cancel_file_val = NULL, vif_threshold = 10) {
  options(monolith_progress_dir = progress_dir_val)
  options(monolith_session_id = session_id_val)
  
  if (!is.null(cancel_file_val) && file.exists(cancel_file_val)) {
    stop("Model generation cancelled by user.")
  }
  
  l <- item$l
  pts_data <- item$pts_data
  m_params <- item$m_params        
  
  res_out <- list(l = l, r_a = NULL, r_p = NULL, r_res = NULL, bound = NULL, pts = NULL, 
                  v_emp_act = NULL, v_fit_act = NULL, cv_act = NULL, cv_obj_act = NULL, summ_act = NULL, rf_act = NULL, gstat_act = NULL,
                  v_emp_pre = NULL, v_fit_pre = NULL, cv_pre = NULL, cv_obj_pre = NULL, summ_pre = NULL, rf_pre = NULL, gstat_pre = NULL, log_msg = "", actual_res = NULL)
  
  res_out <- tryCatch({
    if (!is.numeric(pts_data$x)) pts_data$x <- as.numeric(as.character(pts_data$x))
    if (!is.numeric(pts_data$y)) pts_data$y <- as.numeric(as.character(pts_data$y))
    
    pts_raw <- pts_data %>% dplyr::filter(!is.na(x), !is.na(y))
    if (nrow(pts_raw) < 3) {
      res_out$log_msg <- paste0("Warning in ", l, ": Insufficient data points after cleaning (needed >= 3, got ", nrow(pts_raw), ").")
      return(res_out)
    }
    
    pts_raw <- pts_raw %>% sf::st_as_sf(coords=c("x","y"), crs=current_crs)
    if (current_method %in% c("RK", "RFK", "CK") && length(aux_vars) > 0) {
       pts_raw <- pts_raw %>% dplyr::filter(dplyr::if_all(dplyr::all_of(aux_vars), ~!is.na(.)))
    }
    
    if (nrow(pts_raw) < 3) {
      res_out$log_msg <- paste0("Warning in ", l, ": Insufficient data points after covariate filtering (needed >= 3, got ", nrow(pts_raw), ").")
      return(res_out)
    }
    
    pts_projected <- validate_and_project_sf(pts_raw)
    utm_crs <- sf::st_crs(pts_projected)$wkt
    pts <- pts_projected
    
    if(nrow(pts) < 3) {
      res_out$log_msg <- paste0("Warning in ", l, ": Insufficient data points after UTM conversion (needed >= 3, got ", nrow(pts), ").")
      return(res_out)
    }
    
    coords <- sf::st_coordinates(pts)
    c_round <- data.frame(
      x = round(coords[, "X"], 2),
      y = round(coords[, "Y"], 2)
    )
    pts <- pts[!duplicated(c_round), ]
    if(nrow(pts) < 3) {
      res_out$log_msg <- paste0("Warning in ", l, ": Insufficient unique points after duplicate coordinate removal (needed >= 3, got ", nrow(pts), ").")
      return(res_out)
    }
    
    b_mode_safe <- if (!is.null(buff_mode) && length(buff_mode) > 0) buff_mode else "dynamic"
    b_dist_safe <- if (!is.null(b_dist) && length(b_dist) > 0) b_dist else 250
    grid_res_safe <- if (!is.null(grid_res) && length(grid_res) > 0) grid_res else 50
    current_method_safe <- if (!is.null(current_method) && length(current_method) > 0) current_method else "OK"

    coords_local <- sf::st_coordinates(pts)
    if (!is.null(res_mode) && res_mode == "fixed") {
      local_res <- grid_res_safe
    } else if (nrow(coords_local) > 1) {
      knn_res <- FNN::get.knn(coords_local, k = 1)
      local_res <- mean(knn_res$nn.dist) * 0.5
    } else {
      local_res <- grid_res_safe
    }
    
    b_dist_local <- if (b_mode_safe == "dynamic" && b_type == "wrapped") {
      val <- get_buffer_multiplier(current_method_safe) * local_res
      max(5, min(2000, val))
    } else {
      b_dist_safe
    }
    
    local_shp <- NULL
    if (!is.null(shp_bound)) {
      match_col <- NULL
      for(col_name in colnames(shp_bound)) {
        if (any(as.character(shp_bound[[col_name]]) == l)) {
          match_col <- col_name
          break
        }
      }
      
      if (!is.null(match_col)) {
        local_shp <- shp_bound %>% dplyr::filter(!!sym(match_col) == l)
        local_shp <- sf::st_transform(local_shp, sf::st_crs(pts)) %>% sf::st_union()
      } else {
        local_shp <- tryCatch({
          shp_trans <- tryCatch(sf::st_transform(shp_bound, sf::st_crs(pts)), error = function(e) NULL)
          if (!is.null(shp_trans)) {
            intersects <- sf::st_intersects(shp_trans, sf::st_union(pts), sparse = FALSE)
            if (any(intersects)) {
              shp_trans[which(intersects)[1], ] %>% sf::st_union()
            } else {
              NULL
            }
          } else NULL
        }, error = function(e) NULL)
        if (is.null(local_shp)) {
          # The user supplied a boundary shapefile but it cannot be applied to
          # this locality (projection failure or no spatial overlap); say so
          # instead of silently swapping in the point-derived boundary.
          write_warning_file(l, "act", "Uploaded shapefile boundary could not be applied (projection or overlap issue); using point-derived boundary.")
        }
      }
    }
    
    bound <- NULL
    if (!is.null(local_shp)) {
      bound <- local_shp
    } else {
      bound <- tryCatch({
        b <- switch(b_type,
               "convex"  = sf::st_convex_hull(sf::st_union(pts)),
               "concave" = concaveman::concaveman(pts),
               "wrapped" = sf::st_buffer(concaveman::concaveman(pts), dist = b_dist_local),
               "strict"  = sf::st_union(sf::st_buffer(pts, dist = b_dist_local)))
        sf::st_as_sf(sf::st_sfc(sf::st_geometry(b), crs = sf::st_crs(pts)))
      }, error = function(e) {
        sf::st_as_sf(sf::st_sfc(sf::st_convex_hull(sf::st_union(pts)), crs = sf::st_crs(pts)))
      })
    }
    
    bbox <- sf::st_bbox(bound)
    area_m2 <- as.numeric(sf::st_area(bound))
    cell_area_target <- area_m2 / 100000 
    
    if (!is.null(res_mode) && res_mode == "fixed") {
      actual_res <- grid_res_safe
      # terra::as.points below materializes EVERY bbox cell before the boundary
      # clip, so a fine fixed resolution over a large extent can allocate
      # hundreds of millions of nodes and exhaust memory. Cap the candidate grid
      # at ~4M cells, matching classif_build_grid's auto-resolution guard. Fires
      # only in that pathological case; ordinary fixed resolutions are untouched.
      dx <- as.numeric(bbox["xmax"] - bbox["xmin"])
      dy <- as.numeric(bbox["ymax"] - bbox["ymin"])
      min_res_cap <- sqrt(dx * dy / 4e6)
      if (is.finite(min_res_cap) && actual_res < min_res_cap) {
        write_warning_file(l, "act", sprintf(
          "Fixed grid resolution %.1f m over this extent exceeds ~4M cells; coarsened to %.1f m to avoid exhausting memory.",
          actual_res, min_res_cap))
        actual_res <- min_res_cap
      }
      # Absolute sanity floor only. The slider itself cannot go below 5 m, but a
      # restored run-config could carry any value, and a sub-decimetre grid over
      # any real extent is a memory accident rather than an intent.
      actual_res <- max(actual_res, 0.1)
    } else {
      # Auto is SELF-CONTAINED: the resolution follows this locality's own
      # boundary area (~100k cells), clamped to [5, 1000] m. It must NOT be
      # floored against grid_res — that slider is hidden outside Fixed mode
      # (ui_sidebar.R) and in Auto modes it holds the GLOBAL recommendation
      # (max(mean_1NN * 0.5, max_dim / 300), server_data_setup.R), so a widely
      # spread dataset pushed the floor to ~50 m and silently coarsened every
      # compact locality's density-derived grid via an input the user could
      # neither see nor set.
      actual_res <- sqrt(cell_area_target)
      actual_res <- max(5, min(1000, actual_res))
    }

    grid_r <- terra::rast(terra::ext(bbox), resolution = actual_res, crs = sf::st_crs(pts)$wkt)
    grid_p <- terra::as.points(grid_r, values=FALSE) %>% sf::st_as_sf()
    # One st_coordinates pass, not two: this grid reaches ~1e6 nodes and each
    # call materializes the full coordinate matrix.
    grid_cc <- sf::st_coordinates(grid_p)
    grid_p <- dplyr::mutate(grid_p, x = grid_cc[, 1], y = grid_cc[, 2])
    rm(grid_cc)

    # All engines predict pointwise, so cells outside the boundary — which the
    # post-interpolation mask discards anyway — can be dropped up front. This
    # cuts kriging cost substantially for concave/multi-part boundaries with
    # zero change to within-boundary values.
    inside <- tryCatch(sf::st_intersects(grid_p, bound, sparse = FALSE)[, 1], error = function(e) NULL)
    if (!is.null(inside) && any(inside)) grid_p <- grid_p[inside, ]

    r_a <- NULL; r_p <- NULL

    # The per-surface point sets are built BEFORE the covariate surfaces so the
    # VIF gate can be resolved first: a covariate the gate will drop otherwise
    # costs a full global kriging over the prediction grid (plus its own
    # robust_vgm_fit candidate search, 4 models x 4 start ranges) before
    # predict.lm/predict.randomForest silently ignores the surplus column.
    # The gate must see the frames the engines actually fit - pts_a/pts_p are
    # NA-filtered THEN deduped, while `pts` is only coordinate-deduped, so
    # running it on `pts` could pick a different kept set and change results.
    pts_a <- dedup_valid_points(pts_projected, "v")
    run_pre <- comp_mode || val_type != "actual"
    pts_p <- if (run_pre) dedup_valid_points(pts_projected, "pv") else NULL

    # NULL = unresolved (gate failed); the engine then recomputes it itself.
    resolve_aux_kept <- function(p, prefix = "act") {
      if (is.null(p) || nrow(p) < 3) return(character(0))
      if (length(aux_vars) < 1) return(aux_vars)
      if (length(aux_vars) == 1) {
        # The multicollinearity gate needs >= 2 covariates, so a sole
        # degenerate covariate reaches the engines ungated. Every downstream
        # path degrades visibly rather than wrongly (RK aliases the
        # coefficient to an intercept-only trend; CK's scale() yields NaN and
        # lands in the named OK fallback) — but nothing names the cause, so
        # say it here. It is deliberately passed through, not dropped:
        # emptying aux_vars would turn a degraded-but-working run into a hard
        # dispatch error. Message only, no numeric change.
        if (.is_degenerate_covariate(sf::st_drop_geometry(p)[[aux_vars]])) {
          write_warning_file(l, prefix, paste0(
            "Covariate '", aux_vars, "' is (near-)constant in this locality; ",
            "the ", current_method, " trend model will carry no covariate information."))
        }
        return(aux_vars)
      }
      tryCatch(
        check_vif(sf::st_drop_geometry(p)[, aux_vars, drop = FALSE], threshold = vif_threshold)$kept,
        error = function(e) NULL
      )
    }

    grid_aux <- grid_p
    cov_log_msg <- ""
    aux_kept_a <- NULL; aux_kept_p <- NULL
    # CK is gated too (its LMC is the most collinearity-sensitive fit of the
    # three), but it predicts the covariates jointly instead of reading a
    # kriged covariate grid, so it takes the gate result WITHOUT the
    # krige_covariates pass below.
    if (current_method %in% c("RK", "RFK", "CK") && length(aux_vars) > 0) {
        aux_kept_a <- resolve_aux_kept(pts_a, "act")
        aux_kept_p <- resolve_aux_kept(pts_p, "pre")
    }
    if (current_method %in% c("RK", "RFK") && length(aux_vars) > 0) {
        # Krige the union of the surfaces' kept sets (they can differ: the two
        # surfaces have different NA patterns). If either gate failed, fall
        # back to kriging everything so the engine's own gate still has data.
        cov_vars <- if (is.null(aux_kept_a) || is.null(aux_kept_p)) aux_vars else union(aux_kept_a, aux_kept_p)
        dropped_cov <- setdiff(aux_vars, cov_vars)
        if (length(dropped_cov) > 0) {
          cov_log_msg <- paste0(" [VIF] Covariate surfaces built for the retained set only; not kriged: ",
                                paste(dropped_cov, collapse = ", "), ".")
        }
        if (length(cov_vars) > 0) {
          lags_cov <- calc_scientific_lags(pts)
          mp_cov <- list(idw_p = m_params$idw_p_act, idw_nmax = m_params$idw_nmax)
          # Cancel checkpoint per covariate. This loop kriges every covariate
          # over the full prediction grid and used to be the longest
          # uninterruptible stretch of the run: the surrounding checks only fire
          # before/after it, so pressing Cancel here did nothing until the whole
          # covariate block finished. gstat's krige() is a black box, so one
          # covariate is the coarsest interruptible unit available.
          cov_cancel <- if (!is.null(cancel_file_val)) {
            function(i, total) {
              if (file.exists(cancel_file_val)) stop("Model generation cancelled by user.")
            }
          } else NULL
          krig_cov <- krige_covariates(pts, grid_p, cov_vars, lags_cov, mp_cov, on_var = cov_cancel)
          grid_aux <- krig_cov$grid_aux
          cov_log_msg <- paste0(cov_log_msg, krig_cov$log_msg)
        }
    }

    if(nrow(pts_a) >= 3) {
        lags_a <- calc_scientific_lags(pts_a)
        mp_a <- list(idw_p = m_params$idw_p_act, idw_nmax = m_params$idw_nmax, tps_lambda = m_params$tps_lambda_act, pre_fit = m_params$pre_fit_act, grid_aux = grid_aux, cv_strategy = m_params$cv_strategy, rfk_uncertainty = m_params$rfk_uncertainty, ck_nmax = m_params$ck_nmax, aux_kept = aux_kept_a)
        if (!is.null(cancel_file_val) && file.exists(cancel_file_val)) stop("Model generation cancelled by user.")
        res_a_list <- apply_interpolation(pts_a, "v", current_method, grid_p, aux_vars, lags_a, mp_a, l, "act", vif_threshold)
        res_out$v_emp_act <- res_a_list$v_emp; res_out$v_fit_act <- res_a_list$fit; res_out$cv_act <- res_a_list$cv_metrics; res_out$cv_obj_act <- res_a_list$cv_obj
        res_out$summ_act <- res_a_list$model_summary; res_out$rf_act <- res_a_list$rf_model; res_out$gstat_act <- res_a_list$gstat_obj
        res_out$log_msg <- paste0(res_out$log_msg, "\n", res_a_list$log_msg)
        if (cov_log_msg != "") res_out$log_msg <- paste0(res_out$log_msg, "\n", cov_log_msg)
        
        if(!is.null(res_a_list$res_sf)) {
            fields_a <- if("var1.var" %in% colnames(res_a_list$res_sf)) c("var1.pred", "var1.var") else "var1.pred"
            r_a <- terra::rasterize(res_a_list$res_sf, grid_r, field=fields_a) %>% terra::mask(terra::vect(bound)) %>% terra::project(crs_sel)
            res_out$r_a <- terra::wrap(r_a)
        }
    }
    
    if(run_pre) {
        if(nrow(pts_p) >= 3) {
            lags_p <- calc_scientific_lags(pts_p)
            mp_p <- list(idw_p = m_params$idw_p_pre, idw_nmax = m_params$idw_nmax, tps_lambda = m_params$tps_lambda_pre, pre_fit = m_params$pre_fit_pre, grid_aux = grid_aux, cv_strategy = m_params$cv_strategy, rfk_uncertainty = m_params$rfk_uncertainty, ck_nmax = m_params$ck_nmax, aux_kept = aux_kept_p)
            if (!is.null(cancel_file_val) && file.exists(cancel_file_val)) stop("Model generation cancelled by user.")
            res_p_list <- apply_interpolation(pts_p, "pv", current_method, grid_p, aux_vars, lags_p, mp_p, l, "pre", vif_threshold)
            res_out$v_emp_pre <- res_p_list$v_emp; res_out$v_fit_pre <- res_p_list$fit; res_out$cv_pre <- res_p_list$cv_metrics; res_out$cv_obj_pre <- res_p_list$cv_obj
            res_out$summ_pre <- res_p_list$model_summary; res_out$rf_pre <- res_p_list$rf_model; res_out$gstat_pre <- res_p_list$gstat_obj
            res_out$log_msg <- paste0(res_out$log_msg, "\n", res_p_list$log_msg)
            
            if(!is.null(res_p_list$res_sf)) {
                fields_p <- if("var1.var" %in% colnames(res_p_list$res_sf)) c("var1.pred", "var1.var") else "var1.pred"
                r_p <- terra::rasterize(res_p_list$res_sf, grid_r, field=fields_p) %>% terra::mask(terra::vect(bound)) %>% terra::project(crs_sel)
                res_out$r_p <- terra::wrap(r_p)
            }
        }
    }
    
    if(!is.null(r_a) && !is.null(r_p)) {
      # Difference the PREDICTION layers only. Both rasters can carry a
      # var1.var layer, and subtracting the full stacks writes a
      # "difference of two kriging variances" band into the residual raster —
      # a quantity with no meaning. The in-app viewer was safe because
      # raster_value_layer() picks var1.pred, but the exported residual GeoTIFF
      # shipped the bogus second band.
      pred_layer <- function(r) if ("var1.pred" %in% names(r)) r[["var1.pred"]] else r[[1]]
      r_res <- pred_layer(r_a) - pred_layer(r_p)
      names(r_res) <- "var1.pred"
      res_out$r_res <- terra::wrap(r_res)
    }
    
    # x/y must be filtered too: pts_data is the raw upload, and st_as_sf
    # errors on NA coordinates, which would abort the rest of this locality
    pts_err_raw <- pts_data %>% dplyr::filter(!is.na(x), !is.na(y), !is.na(v), !is.na(pv))
    if(nrow(pts_err_raw) >= 3) {
        # pts_data carries the RAW uploaded coordinates in current_crs; they
        # must be projected, not just stamped with utm_crs (stamping lon/lat
        # degrees as UTM metres put every sample ~10^6 m from the grid and
        # made the point-error surface near-constant for geographic uploads)
        pts_err <- sf::st_as_sf(pts_err_raw, coords=c("x","y"), crs=current_crs) %>%
                   sf::st_transform(utm_crs) %>%
                   dplyr::mutate(err = v - pv)
        err_mod <- gstat::idw(err ~ 1, pts_err, grid_p, nmax = m_params$idw_nmax, idp = 2, debug.level = 0)
        r_err <- terra::rasterize(err_mod, grid_r, field="var1.pred") %>% terra::mask(terra::vect(bound)) %>% terra::project(crs_sel)
        res_out$r_point_err <- terra::wrap(r_err)
    }
    
    # Residuals belong to the dedup_valid_points() sets (pts_a / pts_p: NA-
    # filtered THEN deduped), while pts is only coordinate-deduped. When co-
    # located points differ in NA pattern the two sets keep different rows, so
    # positional assignment via !is.na(pts$v) can silently recycle/shift
    # residuals onto the wrong points. Join on the rounded-coordinate key (the
    # same 2-dp key both dedup passes use) instead.
    coord_key <- function(s) {
      cc <- sf::st_coordinates(s)
      paste(round(cc[, 1], 2), round(cc[, 2], 2))
    }
    pts$model_resid_act <- NA_real_
    # inherits = FALSE: without it the lookup walks up to globalenv(), which the
    # workers source() this file into, so a leftover object of that name from a
    # previous item could satisfy the guard.
    if (nrow(pts_a) >= 3 && exists("res_a_list", inherits = FALSE) && !is.null(res_a_list$residuals) &&
        length(res_a_list$residuals) == nrow(pts_a)) {
      idx_a <- match(coord_key(pts_a), coord_key(pts))
      ok_a <- !is.na(idx_a)
      pts$model_resid_act[idx_a[ok_a]] <- res_a_list$residuals[ok_a]
    }

    pts$model_resid_pre <- NA_real_
    if (run_pre && nrow(pts_p) >= 3 && exists("res_p_list", inherits = FALSE) && !is.null(res_p_list$residuals) &&
        length(res_p_list$residuals) == nrow(pts_p)) {
      idx_p <- match(coord_key(pts_p), coord_key(pts))
      ok_p <- !is.na(idx_p)
      pts$model_resid_pre[idx_p[ok_p]] <- res_p_list$residuals[ok_p]
    }

    res_out$bound <- sf::st_transform(bound, crs_sel)
    res_out$pts <- sf::st_transform(pts, crs_sel) %>% dplyr::mutate(loc = l, resid = v - pv)
    res_out$actual_res <- actual_res
    
    res_out
  }, error = function(e) {
    res_out$log_msg <- paste0(res_out$log_msg, "\nError in ", l, ": ", e$message)
    res_out
  })
  
  return(res_out)
}

# Mean 1-NN distance and bounding-box max dimension in METRES for a point set
# in any CRS. Geographic coordinates are measured via EPSG:3857 with a
# cos(latitude) correction (Web Mercator inflates distances by 1/cos(lat));
# if that transform fails, degree distances are scaled by 111319*cos(lat).
calc_metric_spacing <- function(pts) {
  if (is.null(pts) || nrow(pts) < 2) return(list(mean_nn = NA_real_, max_dim = NA_real_))

  crs_units <- sf::st_crs(pts)$units_gdal
  if (!is.null(crs_units) && !is.na(crs_units) && grepl("degree", crs_units, ignore.case = TRUE)) {
    pts_m <- tryCatch(sf::st_transform(pts, 3857), error = function(e) pts)
    lat_c <- mean(sf::st_coordinates(sf::st_transform(pts, 4326))[, 2])
    dist_scale <- if (identical(pts_m, pts)) {
      111319 * cos(lat_c * pi / 180)
    } else {
      cos(lat_c * pi / 180)
    }
    coords <- sf::st_coordinates(pts_m)
  } else {
    dist_scale <- 1
    coords <- sf::st_coordinates(pts)
  }

  knn_res <- FNN::get.knn(coords, k = 1)
  list(
    mean_nn = mean(knn_res$nn.dist) * dist_scale,
    max_dim = max(diff(range(coords[, 1])), diff(range(coords[, 2]))) * dist_scale
  )
}

buffer_multipliers <- c(
  "TPS" = 1.0,
  "IDW" = 2.0,
  "OK"  = 3.0,
  "CK"  = 3.0,
  "RK"  = 3.0,
  "RFK" = 3.0
)

get_buffer_multiplier <- function(method) {
  if (is.null(method) || length(method) == 0 || is.na(method) || method == "") return(2.0)
  if (method %in% names(buffer_multipliers)) {
    return(buffer_multipliers[[method]])
  }
  return(2.0)
}

# Value vector of a raster's display layer (var1.pred when present, else the
# first layer), accepting both live and Packed SpatRasters. PackedSpatRaster
# is NOT subsettable ([[/values error on it), so every consumer that can see
# a raster that crossed a future boundary must go through this.
raster_value_layer <- function(r) {
  if (is.null(r)) return(NULL)
  if (inherits(r, "PackedSpatRaster")) r <- terra::unwrap(r)
  if (!inherits(r, "SpatRaster")) return(NULL)
  layer <- if ("var1.pred" %in% names(r)) r[["var1.pred"]] else r[[1]]
  as.vector(terra::values(layer, na.rm = TRUE))
}

# Class-break computation for the Agronomical styling algorithms. classInt's
# "jenks" is O(n^2)-slow and silently switches to an UNSEEDED sample above
# n = 3000; "kmeans" also draws unseeded random starts. Both are made
# deterministic here under the app's standard two-sided seed sandbox
# (seed 12345, caller's .Random.seed restored), and jenks is computed on a
# seeded subsample capped at max_n (default 5000) - the standard practice for
# raster classification (GIS packages classify on samples too). Returns the
# n_c - 1 inner break values, or NULL when vv is too short.
calc_class_breaks <- function(vv, n_c, style, max_n = 5000L) {
  vv <- vv[is.finite(vv)]
  if (length(vv) < n_c) return(NULL)

  old_seed <- if (exists(".Random.seed", envir = .GlobalEnv, inherits = FALSE)) get(".Random.seed", envir = .GlobalEnv, inherits = FALSE) else NULL
  on.exit({
    if (!is.null(old_seed)) assign(".Random.seed", old_seed, envir = .GlobalEnv)
    else if (exists(".Random.seed", envir = .GlobalEnv, inherits = FALSE)) rm(".Random.seed", envir = .GlobalEnv)
  }, add = TRUE)
  set.seed(12345)

  if (identical(style, "jenks") && length(vv) > max_n) {
    vv <- sample(vv, max_n)
  }

  tryCatch({
    # suppressMessages matters: classInt's jenks emits a message() condition
    # ("Use fisher instead...") that suppressWarnings does not muffle. A stray
    # message escaping a reactive gets caught by any consumer's tryCatch and
    # aborts the reactive mid-evaluation, poisoning its cached state.
    suppressMessages(suppressWarnings(
      classInt::classIntervals(vv, n = n_c, style = style)$brks[2:n_c]))
  }, error = function(e) {
    seq(min(vv, na.rm = TRUE), max(vv, na.rm = TRUE), length.out = n_c + 1)[2:n_c]
  })
}

# ── Top-level furrr worker entry points ─────────────────────────────────────
# These MUST stay top-level named functions: an inline lambda defined inside a
# Shiny observer closes over the observer -> server environment chain, and
# future serializes that entire chain (reactiveValues, raster caches, the
# whole session) to every worker. That is exactly the "globals exceed
# future.globals.maxSize" failure seen in production once a session had
# accumulated a few runs. Top-level functions are enclosed by the global
# environment, which future never serializes.

# Per-locality worker for the run pipeline's nested furrr map. Sources
# spatial_helpers.R itself (nested workers are fresh processes) and forwards
# the run parameters shipped as one plain list.
interp_run_item <- function(item, run_params) {
  source(file.path(run_params$main_wd, "spatial_helpers.R"), local = FALSE)
  run_regional_interpolation(
    item = item,
    current_method = run_params$current_method,
    current_crs = run_params$current_crs,
    aux_vars = run_params$aux_vars,
    shp_bound = run_params$shp_bound,
    b_type = run_params$b_type,
    buff_mode = run_params$buff_mode,
    b_dist = run_params$b_dist,
    res_mode = run_params$res_mode,
    grid_res = run_params$grid_res,
    crs_sel = run_params$crs_sel,
    comp_mode = run_params$comp_mode,
    val_type = run_params$val_type,
    progress_dir_val = run_params$progress_dir_val,
    session_id_val = run_params$session_id_val,
    cancel_file_val = run_params$cancel_file_val,
    vif_threshold = run_params$vif_threshold
  )
}

# Per-locality worker for the sidebar "OPTIMIZE ALL VARIOGRAMS" button.
# item = list(l, act = data.frame(x, y, v), pre = data.frame(x, y, v) | NULL).
autofit_vgm_item <- function(item, current_crs) {
  res_a <- list(emp = NULL, fit = NULL, mod = "FAIL", sse = "N/A")
  sub_a_raw <- sf::st_as_sf(item$act, coords = c("x", "y"), crs = current_crs)
  sub_a <- validate_and_project_sf(sub_a_raw)
  sub_a <- sub_a[!duplicated(round(sf::st_coordinates(sub_a), 2)), ]

  if (nrow(sub_a) >= 3) {
    lags_a <- calc_scientific_lags(sub_a)
    v_emp_a <- gstat::variogram(v ~ 1, sub_a, width = lags_a$width, cutoff = lags_a$cutoff)
    best_f_a <- robust_vgm_fit(v_emp_a, sub_a$v)
    res_a$emp <- v_emp_a
    res_a$fit <- best_f_a
    res_a$mod <- if (!is.null(best_f_a)) as.character(best_f_a$model[2]) else "FAIL"
    res_a$sse <- if (!is.null(best_f_a)) round(attr(best_f_a, "SSErr") %||% 0, 6) else "N/A"
  }

  res_p <- list(emp = NULL, fit = NULL, mod = "FAIL", sse = "N/A")
  if (!is.null(item$pre)) {
    sub_p_raw <- sf::st_as_sf(item$pre, coords = c("x", "y"), crs = current_crs)
    sub_p <- validate_and_project_sf(sub_p_raw)
    sub_p <- sub_p[!duplicated(round(sf::st_coordinates(sub_p), 2)), ]

    if (nrow(sub_p) >= 3) {
      lags_p <- calc_scientific_lags(sub_p)
      v_emp_p <- gstat::variogram(v ~ 1, sub_p, width = lags_p$width, cutoff = lags_p$cutoff)
      best_f_p <- robust_vgm_fit(v_emp_p, sub_p$v)
      res_p$emp <- v_emp_p
      res_p$fit <- best_f_p
      res_p$mod <- if (!is.null(best_f_p)) as.character(best_f_p$model[2]) else "FAIL"
      res_p$sse <- if (!is.null(best_f_p)) round(attr(best_f_p, "SSErr") %||% 0, 6) else "N/A"
    }
  }

  list(l = item$l, act = res_a, pre = res_p)
}

# Per-locality worker for the "OPTIMIZE TPS LAMBDA" button (GCV curve search).
# item = list(l, df = data.frame(x, y, v)).
tps_gcv_item <- function(item, current_crs) {
  if (nrow(item$df) < 5) return(list(l = item$l, best_lam = 0, gcv_data = NULL, err = NULL))

  # Project before normalizing to the unit box, exactly as apply_TPS does on the
  # run path: on geographic coordinates 1 deg lon != 1 deg lat on the ground, so
  # the point-cloud aspect ratio (and the GCV-optimal lambda) would otherwise
  # differ from the run that consumes this value. No-op for projected uploads.
  pts_sf <- validate_and_project_sf(
    sf::st_as_sf(item$df, coords = c("x", "y"), crs = current_crs))
  # Dedup co-located points exactly as dedup_valid_points does on the run path
  # (the server observer already na.omit()ed, so NA-filter-then-dedup order
  # holds): fields::Tps treats replicates via its pure-error handling, which
  # shifts the GCV curve away from the deduped point set the run actually fits.
  pts_sf <- pts_sf[!duplicated(round(sf::st_coordinates(pts_sf), 2)), ]
  if (nrow(pts_sf) < 5) return(list(l = item$l, best_lam = 0, gcv_data = NULL, err = NULL))
  raw_coords <- sf::st_coordinates(pts_sf)
  vals <- pts_sf$v

  xm <- min(raw_coords[, 1]); xM <- max(raw_coords[, 1])
  ym <- min(raw_coords[, 2]); yM <- max(raw_coords[, 2])
  max_range <- max(xM - xm, yM - ym)
  if (max_range == 0) max_range <- 1
  pts <- cbind((raw_coords[, 1] - xm) / max_range,
               (raw_coords[, 2] - ym) / max_range)

  tryCatch({
    mod <- fields::Tps(pts, vals)
    best_lam <- mod$lambda

    gcv_res <- data.frame(
      lambda = mod$gcv.grid[, 1],
      gcv = mod$gcv.grid[, 3]
    )
    gcv_res <- gcv_res[gcv_res$lambda > 0, , drop = FALSE]

    list(l = item$l, best_lam = best_lam, gcv_data = gcv_res, err = NULL)
  }, error = function(e) {
    list(l = item$l, best_lam = NULL, gcv_data = NULL, err = e$message)
  })
}

# Per-locality worker for the "OPTIMIZE IDW FACTORS" button.
# item = list(l, df = data.frame(x, y, v)).
idw_opt_item <- function(item, current_crs, idw_nmax_val) {
  if (nrow(item$df) < 5) return(list(l = item$l, best_f = 2.0))
  # Project first so optimize_idw_p's nmax neighbour selection and distance-decay
  # weighting run on the same metric coordinates the run pipeline uses. IDW is
  # scale-invariant, but degree axes are anisotropic (1 deg lon != 1 deg lat), so
  # a geographic CRS distorts both. No-op for already-projected uploads.
  pts <- validate_and_project_sf(
    sf::st_as_sf(item$df, coords = c("x", "y"), crs = current_crs))
  # Dedup co-located points exactly as dedup_valid_points does on the run path
  # (NA rows already dropped by the server observer's na.omit()): a co-located
  # twin predicts its held-out partner at distance zero, so every candidate
  # power scores an exact hit there and the search is inflated.
  pts <- pts[!duplicated(round(sf::st_coordinates(pts), 2)), ]
  if (nrow(pts) < 5) return(list(l = item$l, best_f = 2.0))
  best_f <- optimize_idw_p(pts, "v", nmax = idw_nmax_val)
  list(l = item$l, best_f = best_f)
}
