# spatial_pipeline.R - regional orchestration + parallel worker entry points
# (run_regional_interpolation, interp_run_item, autofit_vgm_item, tps_gcv_item,
# idw_opt_item), progress/warning files, CRS projection, dedup, raster and
# class-break utilities. Sourced via spatial_helpers.R.

# How many bounding-box cells the prediction grid converts to sf points at a
# time before testing them against the boundary. Peak memory is one block plus
# the cells that survive the clip, instead of the whole bounding box. Named so
# the suite can shrink it and assert the clip is block-invariant.
.GRID_CLIP_BLOCK_CELLS <- 2e5


# Workers cannot touch Shiny reactives, so run state travels to the main
# session as small files under the session's progress directory. Both writers
# share this body; `kind` is the file-name stem the pollers watch
# ("progress_<sid>_<locality>_<prefix>.txt" / "warn_...").
# Never let a status write break a run: the directory creation and the write
# are both best-effort.
.write_status_file <- function(l, prefix, kind, content) {
  clean_l <- gsub("[^a-zA-Z0-9_]", "_", as.character(l))
  progress_dir <- getOption("monolith_progress_dir", tempdir())
  session_id <- getOption("monolith_session_id", "default")

  if (!dir.exists(progress_dir)) {
    tryCatch(dir.create(progress_dir, recursive = TRUE, showWarnings = FALSE), error = function(e) NULL)
  }

  file_name <- file.path(progress_dir, paste0(kind, "_", session_id, "_", clean_l, "_", prefix, ".txt"))
  tryCatch({
    writeLines(as.character(content), file_name)
  }, error = function(e) NULL)
}

# Percent is rounded to whole numbers here; callers that need finer resolution
# (the classification progress ladder) scale step/total themselves.
update_progress_file <- function(l, prefix, step, total) {
  .write_status_file(l, prefix, "progress", round((step / total) * 100))
}

write_warning_file <- function(l, prefix, message) {
  .write_status_file(l, prefix, "warn", message)
}

# ── Strict-boundary vs grid-resolution coherence ────────────────────────────
# Every surface here is a raster of `res` metre cells, and a cell survives the
# boundary clip only when its CENTRE falls inside the boundary (st_intersects
# in run_regional_interpolation, st_within in classif_build_grid, and
# terra::mask's default centre rule on the rasterised output). Under a Strict
# Measured boundary the domain IS the union of `buffer` metre circles around
# the samples, so an isolated sample paints a cell only when its own cell
# centre lies within `buffer` of it.
#
# A sample sits anywhere in its cell, and on a square grid the centre of the
# cell containing it is also the NEAREST cell centre (cells are the Voronoi
# regions of their centres), so an isolated sample paints no cell at all
# exactly when its own cell's centre escapes the buffer. That distance runs
# from 0 to res/sqrt(2), hence coverage is guaranteed only when
# buffer >= res / sqrt(2) (half the cell diagonal).
#
# Below that, the share of in-cell positions that lose their cell is the cell
# area outside a disc of radius `buffer` centred on the cell centre. The disc
# is inscribed only while buffer <= res/2; beyond that it spills over the cell
# edges and four circular segments must come off, or the loss is understated
# (and 1 - pi*buffer^2/res^2 even turns negative from buffer = res/sqrt(pi)
# upwards, which would report a 0% gap inside the range this very function
# flags). With the segments subtracted the covered area reaches the full cell
# exactly at buffer = res/sqrt(2), so `fraction > 0` and `short` agree by
# construction. Verified against Monte-Carlo sampling of the unit cell.
#
# The result is the expected fraction of ISOLATED samples left with no
# coloured cell; samples in dense clusters are rescued by their neighbours'
# buffers, so it is an upper bound on the visible gaps, not a prediction.
#
# Returns NULL when the inputs are unusable, otherwise a list carrying the
# required buffer, the required resolution, the uncovered fraction, and
# whether the pair is incoherent (`short`).
strict_buffer_gap <- function(buffer, res) {
  buffer <- suppressWarnings(as.numeric(buffer)[1])
  res <- suppressWarnings(as.numeric(res)[1])
  if (!isTRUE(is.finite(buffer)) || !isTRUE(is.finite(res)) ||
      res <= 0 || buffer < 0) return(NULL)
  # Disc-in-square overlap, as a fraction of the cell, at t = buffer / res.
  t <- buffer / res
  covered <- if (t <= 0.5) {
    pi * t^2
  } else if (t < 1 / sqrt(2)) {
    pi * t^2 - 4 * (t^2 * acos(0.5 / t) - 0.5 * sqrt(t^2 - 0.25))
  } else {
    1
  }
  list(
    buffer     = buffer,
    res        = res,
    req_buffer = res / sqrt(2),
    req_res    = buffer * sqrt(2),
    fraction   = min(1, max(0, 1 - covered)),
    short      = buffer < res / sqrt(2)
  )
}

# Plain-text advisory for an incoherent strict buffer/resolution pair; NULL
# when the pair is fine (or unusable). `label` prefixes the locality/scope
# name when there is one.
strict_buffer_message <- function(buffer, res, label = NULL) {
  g <- strict_buffer_gap(buffer, res)
  if (is.null(g) || !g$short) return(NULL)
  # Both suites' manual-resolution sliders stop at 5 m and the Auto rules clamp
  # there as well, so a corrective cell size below that is not something the
  # user could actually set; widening the buffer is then the only real fix.
  res_arm <- if (floor(g$req_res) < 5) "" else sprintf(
    ", or lower the resolution to %s m or less", format(floor(g$req_res), trim = TRUE))
  sprintf(paste0(
    "%sStrict Measured buffer (%s m) is smaller than half the diagonal of a ",
    "%s m grid cell (%s m). Cells are kept only when their centre falls inside ",
    "the buffer, so %s of isolated samples will have no mapped cell beneath ",
    "them. Raise the buffer to %s m or more%s."),
    if (is.null(label) || !nzchar(label)) "" else paste0(label, ": "),
    format(round(g$buffer, 1), trim = TRUE),
    format(round(g$res, 1), trim = TRUE),
    format(round(g$req_buffer, 1), trim = TRUE),
    # Near the threshold the true loss is genuinely a fraction of a percent;
    # rounding it to "0%" would contradict the warning it sits inside.
    if (g$fraction < 0.01) "under 1%" else sprintf("up to %.0f%%", 100 * g$fraction),
    format(ceiling(g$req_buffer), trim = TRUE),
    res_arm)
}

get_joint_scale_values <- function(r1_packed, r2_packed, match_scales, is_uncertainty) {
  if(match_scales && !is_uncertainty) {
    res <- c(raster_value_layer(r1_packed), raster_value_layer(r2_packed))
    if(length(res) == 0) return(NULL)
    return(res)
  }
  return(NULL)
}


# Per-observation worker for the governing-factors SHAP loop. TOP-LEVEL for the
# same reason as interp_run_item / autofit_vgm_item / tps_gcv_item /
# idw_opt_item: an inline lambda inside compute_governing_factors would close
# over that function's frame, so future would serialize df, df_clean, rf_model,
# explainer_rf, vip, vip_df, ale_prof, pdp_prof, old_plan AND shap_cl (the live
# PSOCK cluster handle) to every SHAP worker, on top of the model copy already
# inside the explainer. cancel_path is a PLAIN character path, never a closure,
# and file.exists() consumes no RNG - so the per-observation L'Ecuyer streams,
# and every SHAP value, are unchanged.
gov_shap_item <- function(i, explainer, newdata, cancel_path = NULL) {
  if (!is.null(cancel_path) && file.exists(cancel_path)) {
    stop("Analysis cancelled by user.", call. = FALSE)
  }
  sp <- DALEX::predict_parts(explainer, new_observation = newdata[i, , drop = FALSE],
                             type = "shap")
  sp <- as.data.frame(sp)
  sp$obs_id <- i
  sp
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

  formula_str <- paste(target_col, "~ .")

  # Everything that draws runs under the shared two-sided sandbox (with_seed,
  # spatial_vgm.R). NOTE: the cluster-teardown on.exit() below is registered
  # inside this block but belongs to THIS function's frame (the block is a
  # promise evaluated in the caller's frame), so the cluster is still torn down
  # at function exit, after the RNG state is restored - the same order as the
  # hand-rolled sandbox this replaced.
  with_seed(12345, {
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
    # gov_shap_item is TOP-LEVEL (see its note above): plain-data arguments only,
    # so nothing from this frame is serialized to the SHAP workers. Subsetting
    # the predictor columns once here is identical to the old per-observation
    # df_clean[i, predictors, ] (column-then-row and row-then-column subsetting
    # of a data.frame agree, row names included) and hands each worker a frame
    # without the target column.
    shap_nd <- df_clean[, predictors, drop = FALSE]
    shap_list <- furrr::future_map(sample_idx, gov_shap_item,
      explainer = explainer_rf, newdata = shap_nd, cancel_path = cancel_file,
      .options = furrr::furrr_options(seed = TRUE, packages = c("DALEX", "randomForest")))
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
      shap = shap_val_df,
      # The forest is fitted on the rows complete across the target and every
      # predictor; report that sample so a silent drop cannot pass unnoticed.
      n_used = nrow(df_clean),
      n_total = nrow(df)
    )
  })
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
  
  # cv_reps_* carry the extra fold realizations of an opt-in repeated-CV run
  # (NULL otherwise); the main session turns them into the mean +/- SD report.
  res_out <- list(l = l, r_a = NULL, r_p = NULL, r_res = NULL, bound = NULL, pts = NULL,
                  v_emp_act = NULL, v_fit_act = NULL, cv_act = NULL, cv_obj_act = NULL, cv_reps_act = NULL, summ_act = NULL, rf_act = NULL, gstat_act = NULL,
                  v_emp_pre = NULL, v_fit_pre = NULL, cv_pre = NULL, cv_obj_pre = NULL, cv_reps_pre = NULL, summ_pre = NULL, rf_pre = NULL, gstat_pre = NULL, log_msg = "", actual_res = NULL)
  
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
      # The raster template below spans the whole bounding box before the
      # boundary clip, so a fine fixed resolution over a large extent puts
      # hundreds of millions of candidate cells through the clip. Cap the
      # candidate grid at ~4M cells, matching the floor classif_build_grid
      # applies to both of its resolution modes. Fires only in that
      # pathological case; ordinary fixed resolutions are untouched.
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

    # Authoritative strict-boundary coherence check: only here is the effective
    # resolution known for every mode (Auto derives it from this locality's own
    # boundary area, so no pre-run UI advisory can be exact). A buffer below
    # half the cell diagonal silently drops the cells of isolated samples, which
    # reads as sampled points sitting on blank map. Advisory only - the run is
    # scientifically valid, the support is just under-resolved.
    if (identical(b_type, "strict") && is.null(local_shp)) {
      sb_msg <- strict_buffer_message(b_dist_local, actual_res)
      if (!is.null(sb_msg)) {
        # Two channels, because neither alone reaches the user reliably. The
        # progress panel holds ONE warning per locality/prefix (.write_status_file
        # truncates), so a later engine fallback or VIF note overwrites this one
        # mid-run. The [WARN] tag makes the durable log line double as a
        # notification through the rv$log observer in server_sci_analysis.R,
        # which is what gives the Auto resolution modes - the ones the sidebar
        # advisory cannot cover - the same visibility the Classification Suite
        # gets from its own showNotification.
        write_warning_file(l, "act", sb_msg)
        res_out$log_msg <- paste0(res_out$log_msg, "\n[WARN] ", l, ": ", sb_msg)
      }
    }

    grid_r <- terra::rast(terra::ext(bbox), resolution = actual_res, crs = sf::st_crs(pts)$wkt)

    # Cell centres as a plain MATRIX first. An sfc_POINT stores every node as its
    # own classed numeric(2), ~430 bytes per cell against 16 for a matrix row:
    # measured, a 1e6-cell bounding box costs 427 MB as an sf object (848 MB peak,
    # 10.8 s to build), and the Fixed-mode ~4M-cell cap above extrapolates to
    # ~1.7 GB - per locality, inside a nested PSOCK worker, with up to
    # availableCores()-1 of them live, and roughly 3x that again for RK/RFK once
    # grid_aux and res_sf exist. terra::crds returns exactly the centres
    # terra::as.points does, in the same cell order, at matrix cost.
    grid_xy <- terra::crds(grid_r, na.rm = FALSE)

    # `i` is either a start index paired with `e` (a contiguous block) or a
    # ready-made row index vector (the survivors of the clip).
    grid_block <- function(i, e = NULL) {
      if (!is.null(e)) i <- i:e
      sf::st_as_sf(data.frame(x = grid_xy[i, 1], y = grid_xy[i, 2]),
                   coords = c("x", "y"), crs = sf::st_crs(pts))
    }

    # All engines predict pointwise, so cells outside the boundary - which the
    # post-interpolation mask discards anyway - can be dropped up front. This
    # cuts kriging cost substantially for concave/multi-part boundaries with
    # zero change to within-boundary values. Testing in BLOCKS is what keeps the
    # peak at one block plus the survivors instead of the whole bounding box:
    # for a concave, wrapped or multi-part boundary the bbox routinely carries
    # 1.5-3x the cells that survive, and the clip used to run only after all of
    # them existed. Same st_intersects predicate, same surviving rows, same
    # order, same columns as the single-pass form. Only the keep MASK survives
    # the loop (1 byte per bounding-box cell, ~4 MB at the ~4M-cell cap), never
    # the surviving blocks: holding the blocks and then rbind()-ing them kept
    # the survivors alive TWICE at the bind - measured 618 MB where 309 MB
    # suffices, on a 1e6-cell box retaining 750,000 cells.
    keep_mask <- tryCatch({
      blk_g <- max(1L, as.integer(.GRID_CLIP_BLOCK_CELLS))
      n_bbox <- nrow(grid_xy)
      km <- logical(n_bbox)
      for (s in seq.int(1L, n_bbox, by = blk_g)) {
        e <- min(s + blk_g - 1L, n_bbox)
        chunk <- grid_block(s, e)
        km[s:e] <- sf::st_intersects(chunk, bound, sparse = FALSE)[, 1]
        rm(chunk)
      }
      km
    }, error = function(e) NULL)

    if (is.null(keep_mask)) {
      # Predicate failed (degenerate boundary geometry): fall back to the
      # unclipped grid, exactly as the `inside <- NULL` branch did.
      grid_p <- grid_block(1L, nrow(grid_xy))
    } else if (!any(keep_mask)) {
      # A coarse fixed resolution over a small boundary can leave NO grid node
      # inside it. The engines would then krige the full bbox and the mask
      # would discard every cell - a blank locality with no message, after
      # paying for the whole interpolation. Name the cause and skip instead
      # (the surface was all-NA in this state before; nothing displayable is
      # lost). classif_build_grid stops loudly in the same situation.
      write_warning_file(l, "act", sprintf(
        "No grid node falls inside this locality's boundary at %.1f m resolution; the surface would be empty. Reduce the fixed grid resolution or widen the boundary/buffer.",
        actual_res))
      res_out$log_msg <- paste0(res_out$log_msg, "\nWarning in ", l,
        ": no grid cells fall inside the boundary at this resolution; locality skipped.")
      return(res_out)
    } else {
      grid_p <- grid_block(which(keep_mask))
    }
    rm(grid_xy, keep_mask)

    # x/y attached ONCE, after the clip: the column order is (geometry, x, y)
    # whatever the block count, and it is one st_coordinates pass over the
    # survivors rather than over the whole bounding box.
    grid_cc <- sf::st_coordinates(grid_p)
    grid_p <- dplyr::mutate(grid_p, x = grid_cc[, 1], y = grid_cc[, 2])
    rm(grid_cc)

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
        # A target with no variance in this locality produces a flat surface, an
        # is_fallback variogram and all-NA R2/NSE/CCC/RPD/RPIQ (those metrics are
        # ratios against the observed variance, i.e. UNDEFINED here, not zero).
        # Without this the user only sees the amber "fallback model" banner,
        # which names the symptom rather than the cause. Message only.
        if (.is_degenerate_covariate(pts_a$v)) {
          write_warning_file(l, "act", paste0(
            "Target has no usable variance in this locality (all values equal or ",
            "differing only in noise digits); the surface will be constant and ",
            "R² / NSE / CCC / RPD / RPIQ are undefined."))
        }
        lags_a <- calc_scientific_lags(pts_a)
        mp_a <- list(idw_p = m_params$idw_p_act, idw_nmax = m_params$idw_nmax, tps_lambda = m_params$tps_lambda_act, pre_fit = m_params$pre_fit_act, grid_aux = grid_aux, cv_strategy = m_params$cv_strategy, cv_repeats = m_params$cv_repeats, cancel_file = cancel_file_val, rfk_uncertainty = m_params$rfk_uncertainty, rf_ntree = m_params$rf_ntree, ck_nmax = m_params$ck_nmax, aux_kept = aux_kept_a)
        if (!is.null(cancel_file_val) && file.exists(cancel_file_val)) stop("Model generation cancelled by user.")
        res_a_list <- apply_interpolation(pts_a, "v", current_method, grid_p, aux_vars, lags_a, mp_a, l, "act", vif_threshold)
        res_out$v_emp_act <- res_a_list$v_emp; res_out$v_fit_act <- res_a_list$fit; res_out$cv_act <- res_a_list$cv_metrics; res_out$cv_obj_act <- res_a_list$cv_obj
        res_out$cv_reps_act <- res_a_list$cv_obj_reps
        res_out$summ_act <- res_a_list$model_summary; res_out$rf_act <- res_a_list$rf_model; res_out$gstat_act <- res_a_list$gstat_obj
        res_out$log_msg <- paste0(res_out$log_msg, "\n", res_a_list$log_msg)
        if (cov_log_msg != "") res_out$log_msg <- paste0(res_out$log_msg, "\n", cov_log_msg)
        
        # A non-NULL res_sf whose predictions are ALL NA is not a surface. It
        # reaches here whenever the kriging system could not be solved - gstat
        # returns NA per location rather than raising (measured: a zero-nugget
        # model over near-coincident samples returns 100% NA silently) - and the
        # rasterize/mask/project chain accepts it happily, so the locality was
        # stored and displayed as a completed run with a blank map and an all-NA
        # metrics row. Name it instead. The column test comes first because
        # all(is.na(NULL)) is TRUE, which would skip a locality whose engine
        # named its prediction column something else.
        if (!is.null(res_a_list$res_sf) &&
            "var1.pred" %in% names(res_a_list$res_sf) &&
            all(is.na(res_a_list$res_sf$var1.pred))) {
            write_warning_file(l, "act", paste0(
              "The ", current_method, " system produced no predictions for this locality ",
              "(every grid cell is undefined). This usually means the fitted variogram is not ",
              "a valid covariance model or the kriging matrix is singular; check the variogram ",
              "panel for this locality."))
            res_out$log_msg <- paste0(res_out$log_msg, "\n[WARN] ", l,
              ": the ", current_method, " surface is entirely undefined; locality skipped.")
        } else if(!is.null(res_a_list$res_sf)) {
            # gstat::idw() returns an all-NA `var1.var` alongside var1.pred (IDW
            # is an exact deterministic weighting; it has no prediction variance
            # to report). Rasterizing it doubled the size of every IDW surface
            # and shipped a blank second band into the GeoTIFF export. Gate on
            # the METHOD, not just on the column: apply_TPS's IDW fallback also
            # returns a gstat idw object, and only the kriging engines produce a
            # meaningful variance. Mirrors the map viewer's own guard.
            fields_a <- if(method_has_variance(current_method) && "var1.var" %in% colnames(res_a_list$res_sf)) c("var1.pred", "var1.var") else "var1.pred"
            r_a <- terra::rasterize(res_a_list$res_sf, grid_r, field=fields_a) %>% terra::mask(terra::vect(bound)) %>% terra::project(crs_sel)
            # terra names a SINGLE-field rasterization "last", which then travels
            # into the exported GeoTIFF as the band description (TPS surfaces
            # always shipped that way). Name it after the field it holds; every
            # consumer already prefers "var1.pred" when it is there. The
            # multi-field case is left alone -- terra names those from the
            # fields, and renaming blind could mislabel the variance band.
            if (length(fields_a) == 1L) names(r_a) <- fields_a
            res_out$r_a <- terra::wrap(r_a)
        }
    }
    
    if(run_pre) {
        if(nrow(pts_p) >= 3) {
            # Same constant-target check for the predicted surface.
            if (.is_degenerate_covariate(pts_p$pv)) {
              write_warning_file(l, "pre", paste0(
                "Target has no usable variance in this locality (all values equal or ",
                "differing only in noise digits); the surface will be constant and ",
                "R² / NSE / CCC / RPD / RPIQ are undefined."))
            }
            lags_p <- calc_scientific_lags(pts_p)
            mp_p <- list(idw_p = m_params$idw_p_pre, idw_nmax = m_params$idw_nmax, tps_lambda = m_params$tps_lambda_pre, pre_fit = m_params$pre_fit_pre, grid_aux = grid_aux, cv_strategy = m_params$cv_strategy, cv_repeats = m_params$cv_repeats, cancel_file = cancel_file_val, rfk_uncertainty = m_params$rfk_uncertainty, rf_ntree = m_params$rf_ntree, ck_nmax = m_params$ck_nmax, aux_kept = aux_kept_p)
            if (!is.null(cancel_file_val) && file.exists(cancel_file_val)) stop("Model generation cancelled by user.")
            res_p_list <- apply_interpolation(pts_p, "pv", current_method, grid_p, aux_vars, lags_p, mp_p, l, "pre", vif_threshold)
            res_out$v_emp_pre <- res_p_list$v_emp; res_out$v_fit_pre <- res_p_list$fit; res_out$cv_pre <- res_p_list$cv_metrics; res_out$cv_obj_pre <- res_p_list$cv_obj
            res_out$cv_reps_pre <- res_p_list$cv_obj_reps
            res_out$summ_pre <- res_p_list$model_summary; res_out$rf_pre <- res_p_list$rf_model; res_out$gstat_pre <- res_p_list$gstat_obj
            res_out$log_msg <- paste0(res_out$log_msg, "\n", res_p_list$log_msg)
            
            # Same all-NA guard as the actual surface above.
            if (!is.null(res_p_list$res_sf) &&
                "var1.pred" %in% names(res_p_list$res_sf) &&
                all(is.na(res_p_list$res_sf$var1.pred))) {
                write_warning_file(l, "pre", paste0(
                  "The ", current_method, " system produced no predictions for this locality ",
                  "(every grid cell is undefined). This usually means the fitted variogram is not ",
                  "a valid covariance model or the kriging matrix is singular; check the variogram ",
                  "panel for this locality."))
                res_out$log_msg <- paste0(res_out$log_msg, "\n[WARN] ", l,
                  ": the ", current_method, " predicted surface is entirely undefined; locality skipped.")
            } else if(!is.null(res_p_list$res_sf)) {
                # Same method gate as the actual surface above.
                fields_p <- if(method_has_variance(current_method) && "var1.var" %in% colnames(res_p_list$res_sf)) c("var1.pred", "var1.var") else "var1.pred"
                r_p <- terra::rasterize(res_p_list$res_sf, grid_r, field=fields_p) %>% terra::mask(terra::vect(bound)) %>% terra::project(crs_sel)
                if (length(fields_p) == 1L) names(r_p) <- fields_p
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
        # Deliberate asymmetries with the model point sets: this diagnostic
        # surface keeps co-located twins (gstat's idw tolerates them; each row
        # is a real ML error) and uses a FIXED idp = 2 rather than the run's
        # optimized power, so error surfaces stay comparable across methods.
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
    # Flag the failure where it happens rather than inferring it afterwards from
    # deep object equality: identical(pts_m, pts) tied a numeric scaling decision
    # to sf's internal representation and compared two whole point sets to answer
    # a yes/no question.
    projected_ok <- TRUE
    pts_m <- tryCatch(sf::st_transform(pts, 3857),
                      error = function(e) { projected_ok <<- FALSE; pts })
    lat_c <- mean(sf::st_coordinates(sf::st_transform(pts, 4326))[, 2])
    dist_scale <- if (projected_ok) {
      # Web Mercator inflates distances by 1/cos(latitude); undo it.
      cos(lat_c * pi / 180)
    } else {
      # Still in degrees: approximate metres per degree at this latitude.
      111319 * cos(lat_c * pi / 180)
    }
    coords <- sf::st_coordinates(pts_m)
  } else {
    dist_scale <- 1
    coords <- sf::st_coordinates(pts)
  }

  # A CRS that cannot hold the numbers it was declared over - WGS 84 applied to
  # projected eastings is the routine case, and it is the first entry in the
  # Input Data CRS list - makes sf return a non-finite coordinate for every
  # point WITHOUT raising, and leaves the degree branch above rescaling by a
  # latitude that is not one. FNN::get.knn() answers non-finite input with an
  # error, and this function is called from an observeEvent where nothing
  # caught it, so the whole Shiny session ended. Report it as the same NA the
  # too-few-points case returns: every caller already tests for that, and the
  # sidebar's own plausibility guard is what tells the user their CRS is wrong.
  if (!is.finite(dist_scale)) return(list(mean_nn = NA_real_, max_dim = NA_real_))
  coords <- coords[is.finite(coords[, 1]) & is.finite(coords[, 2]), , drop = FALSE]
  if (nrow(coords) < 2) return(list(mean_nn = NA_real_, max_dim = NA_real_))

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

  with_seed(12345, {
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
  })
}

# Dissolve a classified interpolation surface into class-zone polygons for GIS
# export. The classification is the SAME call the map and the Area Coverage
# table make (terra::classify on the params' rcl_mat with right = FALSE), and
# the areas are taken from terra::expanse(byValue = TRUE) rather than from the
# polygon geometry - both for the same reason: an exported zone layer that
# disagreed with the on-screen map or with the reported hectares would be worse
# than no export at all. Polygon boundaries are therefore cell boundaries and
# inherit the grid resolution; they are not smoothed.
# `params` is the classification_params() list (brks, rcl_mat, labels, n_c).
# Returns one row per class PRESENT in the surface, or NULL when the surface
# holds no classified cell.
build_class_zone_sf <- function(r, params, labels = NULL,
                                surface = NA_character_,
                                variable = NA_character_,
                                method = NA_character_) {
  if (is.null(r) || is.null(params) || is.null(params$rcl_mat)) return(NULL)
  if (inherits(r, "PackedSpatRaster")) r <- terra::unwrap(r)
  if (!inherits(r, "SpatRaster")) return(NULL)

  labs <- if (!is.null(labels)) labels else params$labels
  if (length(labs) == 0) return(NULL)

  zones <- tryCatch({
    r_class <- terra::classify(r[[1]], params$rcl_mat, right = FALSE)
    names(r_class) <- "class_id"

    polys <- terra::as.polygons(r_class, dissolve = TRUE, na.rm = TRUE)
    if (is.null(polys) || nrow(polys) == 0) return(NULL)

    z <- sf::st_as_sf(polys)
    ids <- suppressWarnings(as.integer(z$class_id))
    keep <- !is.na(ids) & ids >= 1 & ids <= length(labs)
    z <- z[keep, , drop = FALSE]
    ids <- ids[keep]
    if (nrow(z) == 0) return(NULL)

    area_df <- as.data.frame(terra::expanse(r_class, unit = "ha", byValue = TRUE))
    area_ha <- rep(NA_real_, length(ids))
    if (all(c("value", "area") %in% names(area_df))) {
      area_ha <- round(area_df$area[match(ids, as.numeric(as.character(area_df$value)))], 2)
    }

    # The outer breaks are -Inf / Inf by construction (every value falls in a
    # class); a GIS field cannot hold an infinity, so open ends are written NA.
    brks <- params$brks
    fin <- function(x) ifelse(is.finite(x), x, NA_real_)

    sf::st_sf(
      class     = as.character(labs[ids]),
      class_min = fin(brks[ids]),
      class_max = fin(brks[ids + 1L]),
      area_ha   = area_ha,
      surface   = as.character(surface),
      variable  = as.character(variable),
      method    = as.character(method),
      geometry  = sf::st_geometry(z)
    )
  }, error = function(e) NULL)

  zones
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
idw_opt_item <- function(item, current_crs, idw_nmax_val, cv_strategy = "auto") {
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
  best_f <- optimize_idw_p(pts, "v", nmax = idw_nmax_val, cv_strategy = cv_strategy)
  list(l = item$l, best_f = best_f)
}
