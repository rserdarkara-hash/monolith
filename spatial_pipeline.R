# spatial_pipeline.R - regional orchestration + parallel worker entry points
# (run_regional_interpolation, interp_run_item, autofit_vgm_item),
# progress/warning files, CRS projection, co-located sample merging, the Auto
# grid rule, raster and class-break utilities. Sourced via spatial_helpers.R.

# Bounding-box cells converted to sf points per block before the boundary clip,
# so peak memory is one block plus the survivors (tests shrink it).
.GRID_CLIP_BLOCK_CELLS <- 2e5

# Bounding-box cells a prediction grid may span before its resolution is
# coarsened: bounds the candidate (pre-clip) grid in every resolution mode.
# Named so the suite can shrink it.
.INTERP_MAX_CANDIDATE_CELLS <- 4e6


# The first 8 hex digits of the MD5 of a string's UTF-8 bytes. Here, not in
# global_utils.R, because workers load this file: safe_key() runs in them.
.hash8 <- function(s) {
  substr(unname(tools::md5sum(bytes = charToRaw(enc2utf8(s)))), 1, 8)
}

#' A key for a name made of [A-Za-z0-9_] only (file names, export registry):
#' the name itself when already so, else its stem (other characters -> "_")
#' plus "_" and .hash8() of the name, so names differing only in punctuation
#' or non-Latin letters never share a key. Vectorised.
safe_key <- function(x) {
  x <- enc2utf8(as.character(x))
  # PCRE: the ranges are code points, so A-Z is ASCII under every locale.
  stem <- gsub("[^A-Za-z0-9_]", "_", x, perl = TRUE)
  alter <- !is.na(x) & stem != x
  stem[alter] <- paste0(stem[alter], "_", vapply(x[alter], .hash8, character(1), USE.NAMES = FALSE))
  stem
}

# Workers cannot touch Shiny reactives, so run state reaches the main session
# as small files ("<kind>_<sid>_<safe_key(l)>_<prefix>.txt") in the progress
# directory. Best-effort: a status write never breaks a run.
.write_status_file <- function(l, prefix, kind, content) {
  clean_l <- safe_key(l)
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

#' Latest run warning for a locality/surface, read by the main-session poller.
write_warning_file <- function(l, prefix, message) {
  .write_status_file(l, prefix, "warn", message)
}

# One wording for the constant-target condition, raised on both channels
# (warning file and run log) by both surfaces.
.degenerate_target_msg <- paste0(
  "Target has no usable variance in this locality (all values equal or ",
  "differing only in noise digits); the surface will be constant and ",
  "R² / NSE / CCC / RPD / RPIQ are undefined.")

# ── Point-buffer boundary vs grid-resolution coherence ─────────────────────
# A cell survives the boundary clip only when its CENTRE is inside the
# boundary. Under a Point buffer boundary an isolated sample's nearest cell
# centre is that of its own cell, 0 to res/sqrt(2) away, so the sample is
# guaranteed a cell only when buffer >= res / sqrt(2) (half the diagonal).
# Below that, the expected share of isolated samples left without a cell is
# the share of the cell outside a disc of radius `buffer` about its centre
# (inscribed up to buffer = res/2, then minus the four circular segments that
# spill over the edges; checked by Monte Carlo). Clustered samples are rescued
# by neighbours, so it is an upper bound.
# Returns NULL for unusable inputs, else the required buffer and resolution,
# the uncovered fraction and whether the pair is incoherent (`short`).
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

# Plain-text advisory for an incoherent buffer/resolution pair, NULL when fine.
# `label` prefixes the locality/scope. A corrective size below `res_floor` (the
# smallest cell the calling suite allows: 1 for interpolation, 5 for
# classification) is left out, since the user cannot act on it.
strict_buffer_message <- function(buffer, res, label = NULL, res_floor = 5) {
  g <- strict_buffer_gap(buffer, res)
  if (is.null(g) || !g$short) return(NULL)
  res_arm <- if (floor(g$req_res) < res_floor) "" else sprintf(
    ", or lower the resolution to %s m or less", format(floor(g$req_res), trim = TRUE))
  sprintf(paste0(
    "%sPoint buffer (%s m) is smaller than half the diagonal of a ",
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

#' Pooled values of two rasters, so a comparison view can share one colour
#' scale. `layer` "value" pools the predictions, "var" the prediction
#' variances and "se" their square roots (a raster without a variance band
#' contributes nothing). NULL when scales are not matched.
get_joint_scale_values <- function(r1_packed, r2_packed, match_scales, layer = "value") {
  if (!isTRUE(match_scales)) return(NULL)
  band <- if (layer %in% c("se", "var")) "var1.var" else "var1.pred"
  res <- c(raster_value_layer(r1_packed, band), raster_value_layer(r2_packed, band))
  if (length(res) == 0) return(NULL)
  if (identical(layer, "se")) res <- sqrt(res)
  res
}


# Per-observation worker of the Governing Factors SHAP loop. TOP-LEVEL (like
# interp_run_item()): a lambda inside compute_governing_factors() would make
# future serialize that whole frame, the live PSOCK cluster included, to every
# SHAP worker. `cancel_path` is a plain path; file.exists() draws no RNG.
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

#' Governing-factors analysis: a random forest of the target on the predictors
#' (complete rows only), the forest's out-of-bag permutation importance, ALE
#' and PDP profiles of the most important predictor, and its SHAP contributions
#' on a sample of up to `shap_sample_size` rows. Seeded (12345). Returns
#' `list(model, explainer, importance, top_var, ale, pdp, shap, n_used,
#' n_total)`, or NULL below 10 complete rows.
compute_governing_factors <- function(df, target_col, predictors, rf_ntree = 100, shap_sample_size = 100, cores_hint = NULL, cancel_file = NULL) {
  req_cols <- c(target_col, predictors)
  df_clean <- df[, req_cols, drop = FALSE]
  df_clean <- df_clean[complete.cases(df_clean), , drop = FALSE]

  if (nrow(df_clean) < 10) return(NULL) # Not enough data

  # Cooperative cancellation via a file flag the module touches; the message
  # is matched by the module's error handler.
  check_cancel <- function() {
    if (!is.null(cancel_file) && file.exists(cancel_file)) {
      stop("Analysis cancelled by user.", call. = FALSE)
    }
  }
  check_cancel()

  # Everything that draws runs under with_seed(). The cluster-teardown
  # on.exit() below belongs to THIS function's frame (the block is a promise
  # evaluated here), so the cluster is torn down at exit, after the RNG state
  # is restored.
  with_seed(12345, {
    # Matrix interface: the formula method's make.names() fails on names such
    # as "Total N (%)".
    rf_model <- randomForest::randomForest(x = df_clean[, predictors, drop = FALSE],
                                           y = df_clean[[target_col]],
                                           ntree = rf_ntree, importance = TRUE)

    explainer_rf <- DALEX::explain(
      model = rf_model,
      data = df_clean[, predictors, drop = FALSE],
      y = df_clean[[target_col]],
      label = "Random Forest",
      verbose = FALSE
    )

    # Breiman's (2001) out-of-bag permutation importance: per tree, the rise in
    # OOB MSE when one predictor is permuted, averaged (the training rows would
    # also score what the forest memorised). The unscaled rise ranks; the scaled
    # %IncMSE is kept beside it but grows with ntree and is not a z-score
    # (Strobl & Zeileis 2008).
    imp <- randomForest::importance(rf_model, type = 1, scale = FALSE)
    imp_scaled <- randomForest::importance(rf_model, type = 1, scale = TRUE)
    vip_agg <- data.frame(variable = rownames(imp), mse_increase = unname(imp[, 1]),
                          mse_increase_scaled = unname(imp_scaled[rownames(imp), 1]),
                          stringsAsFactors = FALSE)
    top_var <- vip_agg$variable[which.max(vip_agg$mse_increase)]

    check_cancel()
    ale_prof <- DALEX::model_profile(explainer_rf, variables = top_var, type = "accumulated")
    ale_df <- as.data.frame(ale_prof$agr_profiles)

    pdp_prof <- DALEX::model_profile(explainer_rf, variables = top_var, type = "partial")
    pdp_df <- as.data.frame(pdp_prof$agr_profiles)

    check_cancel()
    set.seed(12345)
    sample_idx <- sample(seq_len(nrow(df_clean)), min(shap_sample_size, nrow(df_clean)))

    # SHAP per observation is embarrassingly parallel. Inside the module's
    # worker the nested plan is sequential, so escalate only for large enough
    # workloads; seed = TRUE gives each observation its own L'Ecuyer stream, so
    # results are identical under any plan. A PSOCK worker sees one core, so
    # the main session passes `cores_hint`. The cluster is torn down on exit and
    # mc.cores restored for the reused worker.
    if (is.null(cores_hint)) {
      cores_hint <- tryCatch(as.integer(future::availableCores()), error = function(e) 1L)
    }
    n_shap_workers <- min(max(0L, cores_hint - 1L), 8L)
    if (length(sample_idx) >= 50 && n_shap_workers >= 2L && future::nbrOfWorkers() == 1L) {
      old_mc_cores <- getOption("mc.cores")
      old_plan <- future::plan()
      shap_cl <- NULL
      on.exit({
        options(mc.cores = old_mc_cores)
        if (!is.null(shap_cl)) {
          tryCatch(future::plan(old_plan), finally = parallel::stopCluster(shap_cl))
        }
      }, add = TRUE)
      options(mc.cores = n_shap_workers)
      shap_cl <- parallelly::makeClusterPSOCK(n_shap_workers)
      future::plan(future::cluster, workers = shap_cl)
    }
    # gov_shap_item() takes plain data only; the predictor columns are subset
    # once, without the target.
    shap_nd <- df_clean[, predictors, drop = FALSE]
    shap_list <- furrr::future_map(sample_idx, gov_shap_item,
      explainer = explainer_rf, newdata = shap_nd, cancel_path = cancel_file,
      .options = furrr::furrr_options(seed = TRUE, packages = c("DALEX", "randomForest")))
    shap_df <- do.call(rbind, shap_list)
    check_cancel()

    # predict_parts(type = "shap") returns B + 1 rows per variable; only the
    # aggregated B == 0 row is the attribution (summing would inflate it).
    top_rows <- shap_df[shap_df$variable_name == top_var & shap_df$B == 0, , drop = FALSE]
    contrib_by_obs <- if (nrow(top_rows) > 0) {
      tapply(top_rows$contribution, as.character(top_rows$obs_id), mean)
    } else {
      numeric(0)
    }
    # as.numeric(): tapply returns a 1-d array.
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
      # Rows complete across target and predictors, so a silent drop shows.
      n_used = nrow(df_clean),
      n_total = nrow(df)
    )
  })
}

#' Merge per-locality rasters (live or Packed) into one SpatRaster; NULL when
#' the list holds none.
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

# Metres per projected axis unit; shared by worker projections and UI gates.
crs_metre_factor <- function(crs) {
  co <- tryCatch(sf::st_crs(crs), error = function(e) NULL)
  if (is.null(co) || is.na(co) || isTRUE(sf::st_is_longlat(co))) return(NA_real_)
  f <- tryCatch(as.numeric(units::set_units(co$ud_unit, "m")), error = function(e) NA_real_)
  if (length(f) == 1 && is.finite(f) && f > 0) f else NA_real_
}

#' Reproject a finished surface (working CRS) into the Target Mapping CRS. The
#' same system (terra::same.crs) is relabelled, not resampled, which would
#' rewrite every cell at float precision. Another projected CRS gets cells of
#' the run's resolution `res_m` (metres) on one lattice with origin 0, so
#' localities merge without resampling and cell sizes stay exact; a
#' geographic target keeps terra's choice of degrees.
project_to_target <- function(r, crs_sel, res_m) {
  if (terra::same.crs(r, crs_sel)) {
    terra::crs(r) <- crs_sel
    return(r)
  }
  mf <- crs_metre_factor(crs_sel)
  if (!is.finite(mf) || !isTRUE(is.finite(res_m) && res_m > 0)) {
    return(terra::project(r, crs_sel))
  }
  terra::project(r, crs_sel, res = res_m / mf, origin = c(0, 0))
}

#' Geographic and non-metre projected input use the WGS 84 UTM zone of the
#' points' mean position. Metre-based projected input is unchanged.
validate_and_project_sf <- function(pts_sf) {
  if (is.null(pts_sf) || nrow(pts_sf) == 0) return(NULL)
  
  unit_f <- crs_metre_factor(sf::st_crs(pts_sf))
  if (sf::st_is_longlat(pts_sf) || (!is.na(unit_f) && abs(unit_f - 1) > 1e-9)) {
    coords_4326 <- sf::st_coordinates(sf::st_transform(pts_sf, 4326))
    lon_c <- mean(coords_4326[, 1], na.rm = TRUE)
    lat_c <- mean(coords_4326[, 2], na.rm = TRUE)
    if (is.na(lon_c) || is.na(lat_c)) {
      stop("Calculated geographic center contains NA.")
    }
    utm_zone <- min(60, max(1, floor((lon_c + 180) / 6) + 1))
    utm_crs <- paste0("+proj=utm +zone=", utm_zone, " +datum=WGS84 +units=m +no_defs")
    if (lat_c < 0) utm_crs <- paste0(utm_crs, " +south")
    
    pts_sf <- sf::st_transform(pts_sf, utm_crs)
  }
  
  return(pts_sf)
}

#' Merge samples whose projected coordinates agree to `dp` decimals (1 cm):
#' numeric columns become the mean of the non-missing values (NA when none),
#' the geometry the mean coordinate, CV_ROW_ID_COL the smallest id, any other
#' column the smallest-id row's value. Columns in `majority` (class labels)
#' take the most frequent non-missing value instead (a tie, like no value,
#' gives NA; factors keep their levels). `count = TRUE` adds `.mn_n_rep`.
#' Output follows each group's first row; data without repeats come back
#' unchanged. attr(, "merged") = c(groups, rows) when something merged, and
#' attr(, "majority_ties") counts the ties per `majority` column.
merge_colocated <- function(pts_sf, dp = 2, count = FALSE, majority = character(0)) {
  if (is.null(pts_sf) || nrow(pts_sf) == 0) return(pts_sf)
  cc <- sf::st_coordinates(pts_sf)
  key <- paste(round(cc[, 1], dp), round(cc[, 2], dp))
  g <- match(key, unique(key))
  if (!anyDuplicated(g)) {
    if (count) pts_sf$.mn_n_rep <- 1L
    return(pts_sf)
  }

  n_rep <- tabulate(g)
  ids <- if (CV_ROW_ID_COL %in% names(pts_sf)) pts_sf[[CV_ROW_ID_COL]] else seq_len(nrow(pts_sf))
  # Groups are numbered by first appearance; within each, the smallest id
  # sorts first and is the row whose non-numeric values and id are kept.
  ord <- order(g, ids)
  out <- pts_sf[ord[!duplicated(g[ord])], ]

  grp_mean <- function(x) {
    ok <- !is.na(x)
    s <- rowsum(ifelse(ok, x, 0), g, reorder = TRUE)[, 1]
    k <- rowsum(as.numeric(ok), g, reorder = TRUE)[, 1]
    ifelse(k > 0, s / k, NA_real_)
  }
  df <- sf::st_drop_geometry(pts_sf)
  num_cols <- setdiff(names(df)[vapply(df, is.numeric, logical(1))], CV_ROW_ID_COL)
  for (col in num_cols) out[[col]] <- grp_mean(df[[col]])
  # A numeric column keeps its mean: the vote is for class labels only.
  majority <- intersect(majority, setdiff(names(df), num_cols))
  if (length(majority)) {
    ties <- stats::setNames(integer(length(majority)), majority)
    # Only a location holding several rows can differ from its first row; its
    # class starts missing and is set where one class is the most frequent.
    multi_row <- n_rep[g] > 1L
    for (col in majority) {
      x <- df[[col]]
      v <- as.character(x)
      val <- as.character(out[[col]])
      val[n_rep > 1L] <- NA_character_
      sel <- which(multi_row & !is.na(v))
      if (length(sel)) {
        # Every (location, class) pair counted once, in one pass: a location's
        # pairs sorted by count put its most frequent class first, and a
        # runner-up with the same count is a tie.
        lv <- unique(v[sel])
        pair <- (g[sel] - 1) * length(lv) + match(v[sel], lv)
        up <- unique(pair)
        cnt <- tabulate(match(pair, up))
        pg <- (up - 1) %/% length(lv) + 1
        o <- order(pg, -cnt)
        pg <- pg[o]; cnt <- cnt[o]; cls <- lv[(up[o] - 1) %% length(lv) + 1]
        m <- length(pg)
        lead <- !duplicated(pg)
        tied <- lead & c(pg[-1] == pg[-m] & cnt[-1] == cnt[-m], FALSE)
        val[pg[lead & !tied]] <- cls[lead & !tied]
        ties[[col]] <- sum(tied)
      }
      out[[col]] <- if (is.factor(x)) factor(val, levels = levels(x), ordered = is.ordered(x)) else val
    }
  }
  sf::st_geometry(out) <- sf::st_geometry(sf::st_as_sf(
    data.frame(.x = grp_mean(cc[, 1]), .y = grp_mean(cc[, 2])),
    coords = c(".x", ".y"), crs = sf::st_crs(pts_sf)))
  if (count) out$.mn_n_rep <- n_rep
  attr(out, "merged") <- c(groups = sum(n_rep > 1L), rows = sum(n_rep[n_rep > 1L]))
  if (length(majority)) attr(out, "majority_ties") <- ties
  out
}

#' Run-log line for a display set whose co-located samples were merged, from
#' its `.mn_n_rep` counts; NULL when no location carried more than one sample.
replicate_merge_note <- function(l, n_rep) {
  multi <- n_rep[!is.na(n_rep) & n_rep > 1L]
  if (!length(multi)) return(NULL)
  span <- if (min(multi) == max(multi)) max(multi) else paste0(min(multi), "–", max(multi))
  sprintf("[Replicates] %s: %d location%s carried %s co-located samples (%d rows); their values were averaged.",
          l, length(multi), if (length(multi) == 1L) "" else "s", span, sum(multi))
}

# The point set of one interpolation surface: rows with a missing `target`
# dropped FIRST, then co-located samples merged, so every value of a fitted
# location comes from the rows that measured the target (as in the variogram
# preview and the manual-tuning bounds). Returns an sf with refreshed x/y.
dedup_valid_points <- function(pts_sf, target) {
  pts_sf <- pts_sf[!is.na(pts_sf[[target]]), ]
  if (nrow(pts_sf) == 0) return(pts_sf)
  pts_sf <- merge_colocated(pts_sf)
  cc <- sf::st_coordinates(pts_sf)
  dplyr::mutate(pts_sf, x = cc[, 1], y = cc[, 2])
}

#' The two row sets of a co-kriging surface. `t`: every location with a
#' measured `target`, built as dedup_valid_points() builds it (OK's Native
#' population). `x`: locations without the target that measure at least one
#' of `aux_vars` (empty on isotopic data). A covariate's value at a location
#' is the mean of EVERY sample there that measured it; the target keeps the
#' mean of the samples that measured it.
ck_design_rows <- function(pts_all, target, aux_vars) {
  cc <- sf::st_coordinates(pts_all)
  key <- paste(round(cc[, 1], 2), round(cc[, 2], 2))
  # A location index, merged exactly (the mean of equal integers) by both
  # merges, joins the two sets without re-deriving a coordinate key.
  pts_all$.mn_ck_loc <- match(key, unique(key))
  all_loc <- merge_colocated(pts_all)
  t_rows <- dedup_valid_points(pts_all, target)
  if (nrow(t_rows) > 0 && length(aux_vars)) {
    idx <- match(t_rows$.mn_ck_loc, all_loc$.mn_ck_loc)
    for (av in aux_vars) t_rows[[av]] <- all_loc[[av]][idx]
  }
  has_cov <- if (length(aux_vars)) {
    rowSums(!is.na(sf::st_drop_geometry(all_loc)[aux_vars])) > 0
  } else rep(FALSE, nrow(all_loc))
  x_rows <- all_loc[is.na(all_loc[[target]]) & has_cov, ]
  t_rows$.mn_ck_loc <- NULL
  x_rows$.mn_ck_loc <- NULL
  list(t = t_rows, x = x_rows)
}

#' A boundary without a .prj is taken to be in the Input Data CRS, as the
#' upload notice says (one rule for the pipeline, Data Setup and the
#' Classification Suite). A layer with a CRS, or an unparseable `crs`, is
#' returned unchanged.
shp_assume_crs <- function(shp, crs) {
  if (is.null(shp) || !is.na(sf::st_crs(shp))) return(shp)
  co <- suppressWarnings(tryCatch(sf::st_crs(crs), error = function(e) NULL))
  if (is.null(co) || is.na(co)) return(shp)
  sf::st_set_crs(shp, co)
}

#' An uploaded boundary as polygons: polygon layers pass through; point or line
#' layers become the convex hull of all features (as in
#' classif_scope_polygons()). NULL when the hull encloses no area.
shp_boundary_polygons <- function(shp) {
  is_poly <- function(g) all(as.character(sf::st_geometry_type(g)) %in% c("POLYGON", "MULTIPOLYGON"))
  if (is.null(shp) || length(sf::st_geometry(shp)) == 0) return(NULL)
  if (is_poly(shp)) return(shp)
  hull <- sf::st_convex_hull(sf::st_union(sf::st_geometry(shp)))
  if (!is_poly(hull)) return(NULL)
  sf::st_sf(geometry = hull)
}

#' Excess coverage from overlapping locality domains, in square metres.
#' With three identical polygons the excess is twice their area. CRS units
#' are converted explicitly; final mapping boundaries may be geographic.
locality_boundary_overlap <- function(bounds) {
  if (length(bounds) < 2) return(0)
  if (any(vapply(bounds, is.null, logical(1)))) return(NA_real_)
  g <- lapply(bounds, function(b) sf::st_union(sf::st_geometry(b)))
  g <- lapply(g, sf::st_transform, crs = sf::st_crs(g[[1]]))
  area <- function(x) sum(as.numeric(units::set_units(sf::st_area(x), "m^2")))
  max(0, sum(vapply(g, area, numeric(1))) - area(sf::st_union(do.call(c, unname(g)))))
}

#' Uploaded feature indices intersecting samples from multiple run localities.
#' A point/line layer becomes one hull, matching the worker's unnamed route.
shared_boundary_features <- function(shp, items, current_crs) {
  if (is.null(shp) || length(items) < 2) return(integer(0))
  xy <- do.call(rbind, lapply(items, function(it) data.frame(
    x = suppressWarnings(as.numeric(as.character(it$pts_data$x))),
    y = suppressWarnings(as.numeric(as.character(it$pts_data$y))), l = it$l)))
  xy <- xy[is.finite(xy$x) & is.finite(xy$y), , drop = FALSE]
  if (!nrow(xy)) return(integer(0))
  pts <- validate_and_project_sf(sf::st_as_sf(xy, coords = c("x", "y"), crs = current_crs))
  shp <- shp_boundary_polygons(sf::st_transform(shp_assume_crs(shp, current_crs), sf::st_crs(pts)))
  if (is.null(shp)) return(integer(0))
  hit <- sf::st_intersects(shp, pts)
  which(vapply(hit, function(h) length(unique(pts$l[h])) > 1, logical(1)))
}

# Auto grid resolution constants (Hengl 2006, point samples). The pixel sizes
# a point sample supports are fractions of sqrt(A / N): 0.025 is the finest
# legible, 0.25 the coarsest, and 0.0791 their geometric mean, the recommended
# compromise (about 160 cells per sample). The bounds are metres.
HENGL_POINT_FACTOR <- 0.0791
AUTO_RES_MIN <- 1
AUTO_RES_MAX <- 1000

#' Auto grid resolution from sampling density (Hengl 2006):
#' p = min(0.0791 * sqrt(A / N), h / 2), bounded to [AUTO_RES_MIN,
#' AUTO_RES_MAX] m; A = boundary area (m^2), N = distinct sample locations,
#' h = their mean nearest-neighbour distance (m). p <= h / 2 binds only for
#' clustered designs such as transects; without a finite spacing only the
#' first term applies.
auto_grid_resolution <- function(area_m2, n, mean_nn) {
  p <- HENGL_POINT_FACTOR * sqrt(area_m2 / n)
  if (isTRUE(n >= 2) && isTRUE(is.finite(mean_nn))) p <- min(p, mean_nn / 2)
  max(AUTO_RES_MIN, min(AUTO_RES_MAX, p))
}

#' A locality's Auto cell size: auto_grid_resolution() of its boundary and of
#' its display set `pts` (distinct sample locations after merge_colocated, in
#' the metric working CRS).
locality_auto_resolution <- function(bound, pts) {
  auto_grid_resolution(sum(as.numeric(sf::st_area(bound))), nrow(pts),
                       calc_metric_spacing(pts)$mean_nn)
}

#' Raster template with SQUARE cells of exactly `res` covering `bbox`: the
#' extent grows to whole cells from the lower-left corner (terra::rast(ext,
#' resolution) would stretch the cells instead). `snap = TRUE` anchors on
#' multiples of `res`, so localities share one lattice.
grid_template <- function(bbox, res, crs_wkt, snap = FALSE) {
  xmin <- bbox[["xmin"]]
  ymin <- bbox[["ymin"]]
  if (isTRUE(snap)) {
    xmin <- floor(xmin / res) * res
    ymin <- floor(ymin / res) * res
  }
  nx <- max(1, ceiling((bbox[["xmax"]] - xmin) / res - 1e-9))
  ny <- max(1, ceiling((bbox[["ymax"]] - ymin) / res - 1e-9))
  terra::rast(nrows = ny, ncols = nx, xmin = xmin, xmax = xmin + nx * res,
              ymin = ymin, ymax = ymin + ny * res, crs = crs_wkt)
}

#' A locality's projected point sets, as run_regional_interpolation builds
#' them. `ok = FALSE` carries the log message that ends the locality.
.locality_points <- function(l, pts_data, current_crs, current_method, aux_vars, m_params) {
  if (!is.numeric(pts_data$x)) pts_data$x <- as.numeric(as.character(pts_data$x))
  if (!is.numeric(pts_data$y)) pts_data$y <- as.numeric(as.character(pts_data$y))

  # Namespaced calls throughout: shared_auto_resolution() also runs this in the
  # promise worker, where dplyr is not necessarily attached.
  pts_raw <- dplyr::filter(pts_data, !is.na(x), !is.na(y))
  if (nrow(pts_raw) < 3) {
    return(list(ok = FALSE, msg = paste0("Warning in ", l, ": Insufficient data points after cleaning (needed >= 3, got ", nrow(pts_raw), ").")))
  }

  pts_raw <- sf::st_as_sf(pts_raw, coords=c("x","y"), crs=current_crs)

  # Projected ONCE, from every row with coordinates, BEFORE any covariate
  # filter: the working CRS is a property of the locality, so OK and
  # RK/RFK/CK share zone, lags and folds.
  pts_all <- validate_and_project_sf(pts_raw)

  if(nrow(pts_all) < 3) {
    return(list(ok = FALSE, msg = paste0("Warning in ", l, ": Insufficient data points after UTM conversion (needed >= 3, got ", nrow(pts_all), ").")))
  }

  # Rows complete for every selected covariate: RK/RFK map with them, and OK
  # scores them under the Comparable switch. Only computed where used (IDW/TPS
  # may carry a stale selection). CK maps with every row (heterotopic,
  # ck_design_rows()).
  uses_covariates <- current_method %in% c("RK", "RFK") ||
    (identical(current_method, "OK") && identical(m_params$cv_population, "comparable"))
  covariate_complete <- if (uses_covariates && length(aux_vars) > 0) {
    dplyr::filter(pts_all, dplyr::if_all(dplyr::all_of(aux_vars), ~!is.na(.)))
  } else pts_all

  pts_projected <- if (current_method %in% c("RK", "RFK")) covariate_complete else pts_all

  if (nrow(pts_projected) < 3) {
    return(list(ok = FALSE, msg = paste0("Warning in ", l, ": Insufficient data points after covariate filtering (needed >= 3, got ", nrow(pts_projected), ").")))
  }

  # The DISPLAY set: co-located samples merged, NOT filtered on the target. A
  # location's value is the mean over the rows that measured it; one with none
  # keeps NA and is styled apart on the map. `.mn_n_rep` feeds the pop-up.
  pts <- merge_colocated(pts_projected, count = TRUE)
  if(nrow(pts) < 3) {
    return(list(ok = FALSE, msg = paste0("Warning in ", l, ": Insufficient distinct sample locations after merging co-located samples (needed >= 3, got ", nrow(pts), ").")))
  }

  list(ok = TRUE, pts_all = pts_all, utm_crs = sf::st_crs(pts_all)$wkt,
       covariate_complete = covariate_complete, pts_projected = pts_projected, pts = pts)
}

#' A locality's boundary, as run_regional_interpolation builds it: the matching
#' uploaded feature when one applies, else the selected hull or buffer of the
#' deduplicated points. `warn` holds the progress-panel warnings and `log` the
#' run-log lines, in the order the run writes them.
.locality_boundary <- function(l, pts, current_crs, current_method, shp_bound, b_type,
                               buff_mode, b_dist, res_mode, grid_res, shp_shared = integer(0)) {
  warn <- character(0)
  log <- character(0)
  b_mode_safe <- if (!is.null(buff_mode) && length(buff_mode) > 0) buff_mode else "dynamic"
  b_dist_safe <- if (!is.null(b_dist) && length(b_dist) > 0) b_dist else 250
  grid_res_safe <- if (!is.null(grid_res) && length(grid_res) > 0) grid_res else 50
  current_method_safe <- if (!is.null(current_method) && length(current_method) > 0) current_method else "OK"

  local_res <- locality_buffer_res(pts, res_mode, grid_res_safe)

  b_dist_local <- if (b_mode_safe == "dynamic" && b_type == "wrapped") {
    dynamic_buffer_dist(current_method_safe, local_res)
  } else {
    b_dist_safe
  }

  local_shp <- NULL
  if (!is.null(shp_bound)) {
    shp_bound <- shp_assume_crs(shp_bound, current_crs)
    match_col <- NULL
    for(col_name in setdiff(colnames(shp_bound), attr(shp_bound, "sf_column"))) {
      # na.rm: an attribute column that is entirely NA made any() return NA
      # and the if() abort the locality.
      if (any(as.character(shp_bound[[col_name]]) == l, na.rm = TRUE)) {
        match_col <- col_name
        break
      }
    }

    if (!is.null(match_col)) {
      local_shp <- dplyr::filter(shp_bound, !!rlang::sym(match_col) == l)
      local_shp <- shp_boundary_polygons(sf::st_transform(local_shp, sf::st_crs(pts)))
      if (is.null(local_shp)) {
        warn <- c(warn, "Uploaded shapefile features for this locality do not enclose an area (fewer than 3 non-collinear points); using point-derived boundary.")
      } else {
        local_shp <- sf::st_union(local_shp)
        # A name match is not a location match: a feature labelled with this
        # locality that encloses none of its samples would move the grid
        # away from the data, so it is refused like an unnamed one.
        if (!any(sf::st_intersects(local_shp, sf::st_union(pts), sparse = FALSE))) {
          local_shp <- NULL
          warn <- c(warn, "Uploaded shapefile feature named for this locality does not overlap its samples; using point-derived boundary.")
        }
      }
    } else {
      local_shp <- tryCatch({
        shp_trans <- tryCatch(shp_boundary_polygons(sf::st_transform(shp_bound, sf::st_crs(pts))),
                              error = function(e) NULL)
        if (!is.null(shp_trans)) {
          intersects <- sf::st_intersects(shp_trans, sf::st_union(pts), sparse = FALSE)
          eligible <- setdiff(which(intersects), shp_shared)
          if (length(eligible)) {
            sf::st_union(shp_trans[eligible[1], ])
          } else {
            NULL
          }
        } else NULL
      }, error = function(e) NULL)
      if (is.null(local_shp)) {
        # Say so rather than silently using the point-derived boundary.
        msg <- if (length(shp_shared)) {
          "Uploaded boundary has features shared by several run localities, and no unshared feature could be applied here; using the selected sidebar boundary. Add an attribute column containing locality names to assign features explicitly."
        } else "Uploaded shapefile boundary could not be applied (projection or overlap issue); using point-derived boundary."
        warn <- c(warn, msg)
        log <- c(log, paste0(l, ": ", msg))
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

  list(bound = bound, local_shp = local_shp,
       b_dist_local = b_dist_local, grid_res_safe = grid_res_safe, warn = warn, log = log)
}

#' The one cell size of an Auto (Global) run: the FINEST of the localities'
#' own Auto sizes (locality_auto_resolution). A shared cell coarser than a
#' dense locality's h / 2 would merge its samples into shared cells; a finer
#' one costs time only. Points and boundaries are built exactly as the run
#' builds them. NA when no locality yields one.
shared_auto_resolution <- function(items, run_params) {
  sizes <- vapply(items, function(item) tryCatch({
    pp <- .locality_points(item$l, item$pts_data, run_params$current_crs,
                           run_params$current_method, run_params$aux_vars, item$m_params)
    if (!isTRUE(pp$ok)) return(NA_real_)
    lb <- .locality_boundary(item$l, pp$pts, run_params$current_crs, run_params$current_method,
                             run_params$shp_bound, run_params$b_type, run_params$buff_mode,
                             run_params$b_dist, run_params$res_mode, run_params$grid_res,
                             run_params$shp_shared %||% integer(0))
    locality_auto_resolution(lb$bound, pp$pts)
  }, error = function(e) NA_real_), numeric(1))
  if (!any(is.finite(sizes))) return(NA_real_)
  min(sizes, na.rm = TRUE)
}

#' One locality's whole run: clean, project and deduplicate the points, build
#' the boundary and the prediction grid, screen and krige covariates, run the
#' selected engine on the actual (and, in comparison mode, the predicted)
#' surface and rasterize the results. Runs inside a PSOCK worker. Returns a list
#' of Packed rasters, CV objects and metrics, fitted models, boundary and points
#' in `crs_sel`, and `log_msg`. Errors are reported in `log_msg`; only a
#' cancellation raises.
run_regional_interpolation <- function(item, current_method, current_crs, aux_vars, shp_bound, b_type, buff_mode, b_dist, res_mode, grid_res, crs_sel, comp_mode, val_type, progress_dir_val = tempdir(), session_id_val = "default", cancel_file_val = NULL, vif_threshold = 10, shp_shared = integer(0), shared_res = NA_real_) {
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
  # cv_design_* are the CV Distance Match records (cv_distance_summary).
  res_out <- list(l = l, r_a = NULL, r_p = NULL, r_res = NULL, bound = NULL, pts = NULL,
                  v_emp_act = NULL, v_fit_act = NULL, cv_act = NULL, cv_obj_act = NULL, cv_reps_act = NULL, summ_act = NULL, rf_act = NULL, gstat_act = NULL,
                  v_emp_pre = NULL, v_fit_pre = NULL, cv_pre = NULL, cv_obj_pre = NULL, cv_reps_pre = NULL, summ_pre = NULL, rf_pre = NULL, gstat_pre = NULL, log_msg = "", actual_res = NULL,
                  cv_info_act = NULL, cv_info_pre = NULL, cv_design_act = NULL, cv_design_pre = NULL)
  
  res_out <- tryCatch({
    pp <- .locality_points(l, pts_data, current_crs, current_method, aux_vars, m_params)
    if (!isTRUE(pp$ok)) {
      res_out$log_msg <- pp$msg
      return(res_out)
    }
    pts_all <- pp$pts_all
    utm_crs <- pp$utm_crs
    covariate_complete <- pp$covariate_complete
    pts_projected <- pp$pts_projected
    pts <- pp$pts
    rep_note <- replicate_merge_note(l, pts$.mn_n_rep)
    if (!is.null(rep_note)) res_out$log_msg <- paste0(res_out$log_msg, "\n", rep_note)

    lb <- .locality_boundary(l, pts, current_crs, current_method, shp_bound, b_type,
                             buff_mode, b_dist, res_mode, grid_res, shp_shared)
    for (w in lb$warn) write_warning_file(l, "act", w)
    for (g in lb$log) res_out$log_msg <- paste0(res_out$log_msg, "\n", g)
    bound <- lb$bound
    local_shp <- lb$local_shp
    b_dist_local <- lb$b_dist_local
    grid_res_safe <- lb$grid_res_safe

    bbox <- sf::st_bbox(bound)

    # The template spans the whole bounding box before the clip, so cap the
    # candidate grid at ~4M cells (as classif_build_grid() does).
    cap_res <- function(res, what) {
      dx <- as.numeric(bbox["xmax"] - bbox["xmin"])
      dy <- as.numeric(bbox["ymax"] - bbox["ymin"])
      min_res_cap <- sqrt(dx * dy / .INTERP_MAX_CANDIDATE_CELLS)
      if (is.finite(min_res_cap) && res < min_res_cap) {
        msg <- sprintf(
          "%s %.1f m over this extent would need more than %s candidate cells; coarsened to %.1f m to keep the run inside memory.",
          what, res,
          format(.INTERP_MAX_CANDIDATE_CELLS, big.mark = ",", scientific = FALSE),
          min_res_cap)
        # Both channels: the progress panel keeps one warning per locality and
        # closes with the run; the [WARN] log line persists.
        write_warning_file(l, "act", msg)
        res_out$log_msg <<- paste0(res_out$log_msg, "\n[WARN] ", l, ": ", msg)
        res <- min_res_cap
      }
      res
    }
    shared_grid <- identical(res_mode, "global") && is.numeric(shared_res) &&
      length(shared_res) == 1 && is.finite(shared_res)

    if (!is.null(res_mode) && res_mode == "fixed") {
      actual_res <- cap_res(grid_res_safe, "Fixed grid resolution")
      # Sanity floor: the slider stops at 1 m, but a restored config may not.
      actual_res <- max(actual_res, 0.1)
    } else if (shared_grid) {
      # Auto (Global): one cell size for every locality (the finest of their
      # own Auto sizes, shared_auto_resolution()) on one snapped lattice;
      # capped as Fixed, since a strict buffer can span a far larger box.
      actual_res <- cap_res(shared_res, "Shared Auto (Global) resolution")
    } else {
      # Auto (Per Locality): this locality's own sampling density (Hengl 2006).
      # grid_res is hidden outside Fixed and holds a dataset-wide suggestion.
      if (identical(res_mode, "global")) {
        write_warning_file(l, "act", "The shared Auto (Global) resolution could not be computed; this locality uses its own Auto resolution.")
      }
      # Capped too: a strict point-buffer boundary fills a small share of its box.
      actual_res <- cap_res(locality_auto_resolution(bound, pts), "Auto grid resolution")
    }

    # Authoritative strict-buffer check: only here is the effective resolution
    # known in every mode. Advisory only (the run is valid, the support
    # under-resolved).
    if (identical(b_type, "strict") && is.null(local_shp)) {
      # res_floor 1: the corrective size is set on the Fixed slider (1 m minimum).
      sb_msg <- strict_buffer_message(b_dist_local, actual_res, res_floor = 1)
      if (!is.null(sb_msg)) {
        # Both channels: the progress panel keeps one warning per surface (a
        # later note overwrites it), and the [WARN] log line also raises a
        # notification (server_sci_analysis.R), covering the Auto modes the
        # sidebar advisory cannot.
        write_warning_file(l, "act", sb_msg)
        res_out$log_msg <- paste0(res_out$log_msg, "\n[WARN] ", l, ": ", sb_msg)
      }
    }

    # Fixed and Auto (Global) grids snap to one lattice of multiples of the
    # cell size, so terra::merge need not resample localities onto the first
    # one's origin. Auto (Per Locality) grids start at their boundary's corner.
    grid_r <- grid_template(bbox, actual_res, sf::st_crs(pts)$wkt,
                            snap = shared_grid || identical(res_mode, "fixed"))

    # Cell centres as a plain MATRIX first: an sfc_POINT costs ~430 bytes per
    # cell against 16 (a 1e6-cell box: 427 MB as sf), per locality inside a
    # nested worker. terra::crds gives the same centres in the same order.
    grid_xy <- terra::crds(grid_r, na.rm = FALSE)

    # `i` is either a start index paired with `e` (a contiguous block) or a
    # ready-made row index vector (the survivors of the clip).
    grid_block <- function(i, e = NULL) {
      if (!is.null(e)) i <- i:e
      sf::st_as_sf(data.frame(x = grid_xy[i, 1], y = grid_xy[i, 2]),
                   coords = c("x", "y"), crs = sf::st_crs(pts))
    }

    # Every engine predicts pointwise, so cells outside the boundary are dropped
    # up front (same values inside). The clip runs in BLOCKS and keeps only a
    # logical mask, so the peak is one block plus the survivors, not the whole
    # bounding box.
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
      # Predicate failed (degenerate boundary geometry): use the unclipped grid.
      grid_p <- grid_block(1L, nrow(grid_xy))
    } else if (!any(keep_mask)) {
      # A coarse cell over a small boundary (Fixed, or a shared Auto (Global)
      # size) can leave NO node inside: name the cause and skip instead of
      # kriging the whole box into a blank map (classif_build_grid() stops too).
      write_warning_file(l, "act", sprintf(
        "No grid node falls inside this locality's boundary at %.1f m resolution; the surface would be empty. %s",
        actual_res,
        if (identical(res_mode, "fixed")) "Reduce the fixed grid resolution or widen the boundary/buffer."
        else if (shared_grid) "The shared Auto (Global) cell size, after any coarsening by the candidate-cell cap, is too coarse for this boundary: switch to Auto (Per Locality), or set a Fixed resolution, or widen the boundary/buffer."
        else "Widen the boundary/buffer, or set a Fixed resolution."))
      res_out$log_msg <- paste0(res_out$log_msg, "\nWarning in ", l,
        ": no grid cells fall inside the boundary at this resolution; locality skipped.")
      return(res_out)
    } else {
      grid_p <- grid_block(which(keep_mask))
    }
    rm(grid_xy, keep_mask)

    # x/y attached once after the clip, over the survivors only.
    grid_cc <- sf::st_coordinates(grid_p)
    grid_p <- dplyr::mutate(grid_p, x = grid_cc[, 1], y = grid_cc[, 2])
    rm(grid_cc)

    # The map's locations for the CV (kNNDM folds, CV Distance Match panel): a
    # lattice inside the boundary, independent of the cell size; the grid only
    # where the boundary yields no lattice.
    cv_domain_xy <- tryCatch(knndm_domain_points(bound, grid_xy = cbind(grid_p$x, grid_p$y)),
                             error = function(e) NULL)

    r_a <- NULL; r_p <- NULL

    # Per-surface point sets come BEFORE the covariate surfaces so the VIF gate
    # resolves first (a covariate it drops would otherwise be kriged over the
    # whole grid for nothing). The gate must see the frames the engines fit
    # (NA-filtered, then deduplicated), not `pts`. CK splits target and
    # covariate-only locations (ck_design_rows()) and gates itself in apply_CK().
    run_pre <- comp_mode || val_type != "actual"
    ck_a <- NULL; ck_p <- NULL
    if (identical(current_method, "CK")) {
      ck_a <- ck_design_rows(pts_all, "v", aux_vars)
      pts_a <- ck_a$t
      if (run_pre) { ck_p <- ck_design_rows(pts_all, "pv", aux_vars); pts_p <- ck_p$t } else pts_p <- NULL
    } else {
      pts_a <- dedup_valid_points(pts_projected, "v")
      pts_p <- if (run_pre) dedup_valid_points(pts_projected, "pv") else NULL
    }

    # The CV population of each surface. RK/RFK: the covariate-complete rows.
    # OK: every sample with a target (Native) or, under Comparable, trained AND
    # scored on the covariate-complete rows folded as RK/RFK fold them, so the
    # engines compare on the same samples and partition (the OK map still uses
    # every sample). CK: the Native population.
    ok_comparable <- identical(current_method, "OK") &&
      identical(m_params$cv_population, "comparable") && length(aux_vars) > 0
    cv_pop_label <- if (current_method %in% c("RK", "RFK")) "common rows"
      else if (!current_method %in% c("OK", "CK")) NA_character_
      else if (ok_comparable) "common rows" else "native rows"
    cv_population_of <- function(surface_pts, target) {
      if (!ok_comparable) return(surface_pts)
      dedup_valid_points(covariate_complete, target)
    }
    # Below 3 samples no plan is built and the engine skips its CV with a named
    # message (the map still runs). IDW and TPS fold their own point set.
    attach_cv_plan <- function(mp, pop, prefix) {
      if (is.na(cv_pop_label)) return(mp)
      mp$cv_data <- pop
      if (nrow(pop) >= 3) {
        mp$cv_plan <- build_cv_plan(pop, m_params$cv_strategy, m_params$cv_repeats,
                                    domain_xy = cv_domain_xy)
      } else {
        res_out$log_msg <<- paste0(res_out$log_msg, "\n[CV] ", l, " (", prefix,
          "): the cross-validation population (", cv_pop_label, ") holds ", nrow(pop),
          " samples; cross-validation skipped. The map is unaffected.")
      }
      mp
    }
    # Everything the main session needs to label, hash and explain this
    # surface's cross-validation. Plain data only - it crosses the future.
    cv_info_of <- function(mp, res_list) {
      list(population = cv_pop_label,
           row_id = mp$cv_plan$row_id, folds1 = mp$cv_plan$folds[[1]],
           n_expected = mp$cv_plan$n,
           conditional = res_list$cv_conditional,
           vgm_col = res_list$cv_vgm_col,
           screen = res_list$cv_screen,
           vgm_status = res_list$cv_vgm_status)
    }
    # The kNNDM run-log line: the scored folds' record, else the plan's (a
    # kriging CV that failed outright); nothing where no CV ran.
    knndm_note <- function(mp, res_list, surface_pts, surface) {
      if (!identical(m_params$cv_strategy, "knndm")) return(NULL)
      info <- attr(res_list$cv_obj, "knndm") %||% attr(mp$cv_plan$folds[[1]], "knndm")
      if (is.null(info) && is.null(res_list$cv_obj)) return(NULL)
      knndm_log_line(info, l, surface, mp$cv_plan$n %||% nrow(surface_pts), crs_unit_label(surface_pts))
    }

    # NULL = unresolved (gate failed); the engine then recomputes it itself.
    resolve_aux_kept <- function(p, prefix = "act") {
      if (is.null(p) || nrow(p) < 3) return(character(0))
      if (length(aux_vars) < 1) return(aux_vars)
      if (length(aux_vars) == 1) {
        # The gate needs >= 2 covariates, so a sole degenerate covariate passes
        # ungated (RK then fits an intercept-only trend, RFK a bagged mean).
        # Kept rather than dropped, which would turn a degraded run into a
        # dispatch error; the warning names the cause (apply_CK() does the same).
        if (.is_degenerate_covariate(sf::st_drop_geometry(p)[[aux_vars]])) {
          write_warning_file(l, prefix, paste0(
            "Covariate '", aux_vars, "' is (near-)constant in this locality; ",
            "the ", current_method, " trend model will carry no covariate information."))
        }
        return(aux_vars)
      }
      # The same screen every CV fold runs on its own training rows, so the map
      # and the folds can never screen by different rules.
      tryCatch(screen_covariates(p, aux_vars, vif_threshold)$kept, error = function(e) NULL)
    }

    # The covariate surfaces' IDW fallback, in the map and every RK/RFK fold:
    # p = 2 whatever the target's IDW setting (Auto, -1, is not a power).
    mp_cov_fallback <- list(idw_p = 2, idw_nmax = m_params$idw_nmax)
    grid_aux <- grid_p
    cov_log_msg <- ""
    aux_kept_a <- NULL; aux_kept_p <- NULL
    # CK gates itself in apply_CK() on target and covariate-only locations and
    # predicts the covariates jointly.
    if (current_method %in% c("RK", "RFK") && length(aux_vars) > 0) {
        aux_kept_a <- resolve_aux_kept(pts_a, "act")
        aux_kept_p <- resolve_aux_kept(pts_p, "pre")
        # Krige the union of the two surfaces' kept sets (NA patterns differ);
        # if either gate failed, krige everything so the engine's gate has data.
        cov_vars <- if (is.null(aux_kept_a) || is.null(aux_kept_p)) aux_vars else union(aux_kept_a, aux_kept_p)
        dropped_cov <- setdiff(aux_vars, cov_vars)
        if (length(dropped_cov) > 0) {
          cov_log_msg <- paste0(" [VIF] Covariate surfaces built for the retained set only; not kriged: ",
                                paste(dropped_cov, collapse = ", "), ".")
        }
        if (length(cov_vars) > 0) {
          lags_cov <- calc_scientific_lags(pts)
          # Cancel checkpoint per covariate: the longest stretch between checks,
          # and krige() cannot be interrupted.
          cov_cancel <- if (!is.null(cancel_file_val)) {
            function(i, total) {
              if (file.exists(cancel_file_val)) stop("Model generation cancelled by user.")
            }
          } else NULL
          krig_cov <- krige_covariates(pts, grid_p, cov_vars, lags_cov, mp_cov_fallback, on_var = cov_cancel)
          grid_aux <- krig_cov$grid_aux
          cov_log_msg <- paste0(cov_log_msg, krig_cov$log_msg)
        }
    }

    if(nrow(pts_a) >= 3) {
        # A constant target gives a flat surface, a fallback variogram and
        # undefined (NA) ratio metrics; name the cause on both channels (the
        # progress panel closes when the maps are revealed).
        if (.is_degenerate_covariate(pts_a$v)) {
          write_warning_file(l, "act", .degenerate_target_msg)
          res_out$log_msg <- paste0(res_out$log_msg, "
[WARN] ", l, " (Actual): ",
                                    .degenerate_target_msg)
        }
        lags_a <- calc_scientific_lags(pts_a)
        mp_a <- list(idw_p = m_params$idw_p_act, idw_nmax = m_params$idw_nmax, cov_params = mp_cov_fallback, tps_lambda = m_params$tps_lambda_act, pre_fit = m_params$pre_fit_act, grid_aux = grid_aux, cv_strategy = m_params$cv_strategy, cv_repeats = m_params$cv_repeats, cv_domain_xy = cv_domain_xy, cancel_file = cancel_file_val, rfk_uncertainty = m_params$rfk_uncertainty, rf_ntree = m_params$rf_ntree, ck_nmax = m_params$ck_nmax, ck_extra = ck_a$x, aux_kept = aux_kept_a)
        mp_a <- attach_cv_plan(mp_a, cv_population_of(pts_a, "v"), "act")
        if (!is.null(cancel_file_val) && file.exists(cancel_file_val)) stop("Model generation cancelled by user.")
        res_a_list <- apply_interpolation(pts_a, "v", current_method, grid_p, aux_vars, lags_a, mp_a, l, "act", vif_threshold)
        res_out$cv_info_act <- cv_info_of(mp_a, res_a_list)
        res_out$cv_design_act <- res_a_list$cv_design
        res_out$v_emp_act <- res_a_list$v_emp; res_out$v_fit_act <- res_a_list$fit; res_out$cv_act <- res_a_list$cv_metrics; res_out$cv_obj_act <- res_a_list$cv_obj
        res_out$cv_reps_act <- res_a_list$cv_obj_reps
        res_out$tps_fit_act <- res_a_list$tps_fit
        res_out$idw_fit_act <- res_a_list$idw_fit
        # detach_model_frame(): otherwise the model's terms carry this
        # locality's whole frame back to the main session and every archive.
        res_out$summ_act <- detach_model_frame(res_a_list$model_summary); res_out$rf_act <- res_a_list$rf_model; res_out$gstat_act <- res_a_list$gstat_obj
        # Covariates the mapped model used and the ones the screen removed
        # (RK/RFK/CK only), and CK's data design, for the run configuration.
        res_out$aux_used_act <- res_a_list$aux_used; res_out$aux_dropped_act <- res_a_list$aux_dropped
        res_out$ck_design_act <- res_a_list$ck_design
        res_out$log_msg <- paste0(res_out$log_msg, "\n", res_a_list$log_msg)
        kn_a <- knndm_note(mp_a, res_a_list, pts_a, "Actual")
        if (!is.null(kn_a)) res_out$log_msg <- paste0(res_out$log_msg, "\n", kn_a)
        if (cov_log_msg != "") res_out$log_msg <- paste0(res_out$log_msg, "\n", cov_log_msg)
        
        # An all-NA res_sf (an unsolvable kriging system; gstat returns NA
        # silently) is not a surface: name it rather than store a blank map.
        # The column test comes first because all(is.na(NULL)) is TRUE.
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
            # Only the kriging engines have a variance band: gstat::idw() (and
            # the TPS IDW fallback) return an all-NA var1.var.
            fields_a <- if(method_has_variance(current_method) && "var1.var" %in% colnames(res_a_list$res_sf)) c("var1.pred", "var1.var") else "var1.pred"
            r_a <- terra::rasterize(res_a_list$res_sf, grid_r, field=fields_a) %>% terra::mask(terra::vect(bound)) %>% project_to_target(crs_sel, actual_res)
            # terra names a single-field rasterization "last", which would
            # become the GeoTIFF band description.
            if (length(fields_a) == 1L) names(r_a) <- fields_a
            res_out$r_a <- terra::wrap(r_a)
        }
    }
    
    if(run_pre) {
        if(nrow(pts_p) >= 3) {
            # Same constant-target check for the predicted surface.
            if (.is_degenerate_covariate(pts_p$pv)) {
              write_warning_file(l, "pre", .degenerate_target_msg)
              res_out$log_msg <- paste0(res_out$log_msg, "
[WARN] ", l, " (Predicted): ",
                                        .degenerate_target_msg)
            }
            lags_p <- calc_scientific_lags(pts_p)
            mp_p <- list(idw_p = m_params$idw_p_pre, idw_nmax = m_params$idw_nmax, cov_params = mp_cov_fallback, tps_lambda = m_params$tps_lambda_pre, pre_fit = m_params$pre_fit_pre, grid_aux = grid_aux, cv_strategy = m_params$cv_strategy, cv_repeats = m_params$cv_repeats, cv_domain_xy = cv_domain_xy, cancel_file = cancel_file_val, rfk_uncertainty = m_params$rfk_uncertainty, rf_ntree = m_params$rf_ntree, ck_nmax = m_params$ck_nmax, ck_extra = ck_p$x, aux_kept = aux_kept_p)
            mp_p <- attach_cv_plan(mp_p, cv_population_of(pts_p, "pv"), "pre")
            if (!is.null(cancel_file_val) && file.exists(cancel_file_val)) stop("Model generation cancelled by user.")
            if (current_method == "OK" && isFALSE(m_params$sep_fit) && is.null(mp_p$pre_fit)) {
              if (!is.null(res_out$v_fit_act)) {
                mp_p$shared_fit <- res_out$v_fit_act
                mp_p$vgm_col <- "v"
              } else {
                res_out$log_msg <- paste0(res_out$log_msg, "\n[Variogram] No Actual fit is available for ", l,
                                           "; the Predicted surface fits its own variogram.")
              }
            }
            # TPS unseparated: Auto (GCV) selects lambda on the MEASURED values
            # of the same rows, again in every fold (a fixed lambda is already
            # shared at dispatch).
            if (current_method == "TPS" && isFALSE(m_params$sep_fit)) mp_p$tps_gcv_col <- "v"
            # IDW unseparated: Auto (CV) selects the power on the measured
            # values, on all rows and in every fold (TPS carries it for its IDW
            # fallback).
            if (current_method %in% c("IDW", "TPS") && isFALSE(m_params$sep_fit)) mp_p$idw_select_col <- "v"
            res_p_list <- apply_interpolation(pts_p, "pv", current_method, grid_p, aux_vars, lags_p, mp_p, l, "pre", vif_threshold)
            res_out$cv_info_pre <- cv_info_of(mp_p, res_p_list)
            res_out$cv_design_pre <- res_p_list$cv_design
            res_out$v_emp_pre <- res_p_list$v_emp; res_out$v_fit_pre <- res_p_list$fit; res_out$cv_pre <- res_p_list$cv_metrics; res_out$cv_obj_pre <- res_p_list$cv_obj
            res_out$cv_reps_pre <- res_p_list$cv_obj_reps
            res_out$tps_fit_pre <- res_p_list$tps_fit
            res_out$idw_fit_pre <- res_p_list$idw_fit
            res_out$summ_pre <- detach_model_frame(res_p_list$model_summary); res_out$rf_pre <- res_p_list$rf_model; res_out$gstat_pre <- res_p_list$gstat_obj
            res_out$aux_used_pre <- res_p_list$aux_used; res_out$aux_dropped_pre <- res_p_list$aux_dropped
            res_out$ck_design_pre <- res_p_list$ck_design
            res_out$log_msg <- paste0(res_out$log_msg, "\n", res_p_list$log_msg)
            kn_p <- knndm_note(mp_p, res_p_list, pts_p, "Predicted")
            if (!is.null(kn_p)) res_out$log_msg <- paste0(res_out$log_msg, "\n", kn_p)
            
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
                r_p <- terra::rasterize(res_p_list$res_sf, grid_r, field=fields_p) %>% terra::mask(terra::vect(bound)) %>% project_to_target(crs_sel, actual_res)
                if (length(fields_p) == 1L) names(r_p) <- fields_p
                res_out$r_p <- terra::wrap(r_p)
            }
        }
    }
    
    if(!is.null(r_a) && !is.null(r_p)) {
      # Difference the PREDICTION layers only: a difference of two kriging
      # variances has no meaning.
      pred_layer <- function(r) if ("var1.pred" %in% names(r)) r[["var1.pred"]] else r[[1]]
      r_res <- pred_layer(r_a) - pred_layer(r_p)
      names(r_res) <- "var1.pred"
      res_out$r_res <- terra::wrap(r_res)
    }
    
    # x/y must be filtered too: pts_data is the raw upload, and st_as_sf
    # errors on NA coordinates, which would abort the rest of this locality
    pts_err_raw <- pts_data %>% dplyr::filter(!is.na(x), !is.na(y), !is.na(v), !is.na(pv))
    if(nrow(pts_err_raw) >= 3) {
        # The raw coordinates are in current_crs and must be projected, not
        # stamped with utm_crs.
        pts_err <- sf::st_as_sf(pts_err_raw, coords=c("x","y"), crs=current_crs) %>%
                   sf::st_transform(utm_crs) %>%
                   dplyr::mutate(err = v - pv)
        # Deliberately unlike the model point sets: co-located rows are kept
        # (each is a real ML error) and idp is fixed at 2, so error surfaces
        # stay comparable across methods.
        err_mod <- gstat::idw(err ~ 1, pts_err, grid_p, nmax = m_params$idw_nmax %||% 12, idp = 2, debug.level = 0)
        r_err <- terra::rasterize(err_mod, grid_r, field="var1.pred") %>% terra::mask(terra::vect(bound)) %>% project_to_target(crs_sel, actual_res)
        res_out$r_point_err <- terra::wrap(r_err)
    }
    
    # CV residuals are joined by sample position (the dedup passes' 2-dp
    # coordinate key), not by row: a CV population need not be the point set
    # (OK Comparable). `pts` is deduplicated, so the key is unique.
    coord_key <- function(s) {
      cc <- sf::st_coordinates(s)
      paste(round(cc[, 1], 2), round(cc[, 2], 2))
    }
    join_cv_residuals <- function(cv_obj) {
      out <- rep(NA_real_, nrow(pts))
      if (inherits(cv_obj, "Spatial")) cv_obj <- tryCatch(sf::st_as_sf(cv_obj), error = function(e) NULL)
      if (!inherits(cv_obj, "sf") || nrow(cv_obj) == 0) return(out)
      idx <- match(coord_key(cv_obj), coord_key(pts))
      ok <- !is.na(idx)
      out[idx[ok]] <- get_cv_residuals(cv_obj, nrow(cv_obj))[ok]
      out
    }
    pts$model_resid_act <- join_cv_residuals(res_out$cv_obj_act)
    pts$model_resid_pre <- if (run_pre) join_cv_residuals(res_out$cv_obj_pre) else NA_real_

    res_out$bound <- sf::st_transform(bound, crs_sel)
    res_out$pts <- sf::st_transform(pts, crs_sel) %>% dplyr::mutate(loc = l, resid = v - pv)
    # The CV row id is internal: rv$sf is res_out$pts, whose columns the popup,
    # colour-by and label pickers offer by name.
    res_out$pts[[CV_ROW_ID_COL]] <- NULL
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
  unit_f <- crs_metre_factor(sf::st_crs(pts))
  if (!is.na(unit_f) && abs(unit_f - 1) > 1e-9) {
    pts <- validate_and_project_sf(pts)
    crs_units <- sf::st_crs(pts)$units_gdal
  }
  if (!is.null(crs_units) && !is.na(crs_units) && grepl("degree", crs_units, ignore.case = TRUE)) {
    # Flag a failed transform where it happens.
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

  # A CRS that cannot hold its numbers (WGS 84 over projected eastings) gives
  # non-finite coordinates without an error, and FNN::get.knn() would then
  # error inside an observer and end the session. Return the too-few-points
  # NA instead; the sidebar's plausibility guard names the wrong CRS.
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

#' Method factor for the dynamic wrapped buffer: buffer = factor x the local
#' resolution (half the mean nearest-neighbour distance, or the fixed grid
#' resolution), clamped to 5-2000 m. 2.0 for an unknown method.
get_buffer_multiplier <- function(method) {
  if (is.null(method) || length(method) == 0 || is.na(method) || method == "") return(2.0)
  if (method %in% names(buffer_multipliers)) {
    return(buffer_multipliers[[method]])
  }
  return(2.0)
}

#' The resolution a Buffered boundary's dynamic buffer scales with: the Fixed
#' cell size `grid_res`, else half the mean nearest-neighbour distance of the
#' locality's distinct locations `pts` (as .locality_points() builds them).
#' Shared by the run and the sidebar preview.
locality_buffer_res <- function(pts, res_mode, grid_res) {
  if (identical(res_mode, "fixed") || nrow(pts) < 2) return(grid_res)
  mean(FNN::get.knn(sf::st_coordinates(pts), k = 1)$nn.dist) * 0.5
}

#' The dynamic buffer distance (m) for `method` at the buffer resolution `res`.
dynamic_buffer_dist <- function(method, res) {
  max(5, min(2000, get_buffer_multiplier(method) * res))
}

# Values of a raster's display layer (var1.pred, else the first), live or
# Packed (PackedSpatRaster is not subsettable, so every consumer of a raster
# that crossed a future boundary goes through this). `band = "var1.var"`
# reads the variance band, NULL when there is none.
raster_value_layer <- function(r, band = "var1.pred") {
  if (is.null(r)) return(NULL)
  if (inherits(r, "PackedSpatRaster")) r <- terra::unwrap(r)
  if (!inherits(r, "SpatRaster")) return(NULL)
  layer <- if (band %in% names(r)) r[[band]]
           else if (identical(band, "var1.pred")) r[[1]]
           else return(NULL)
  as.vector(terra::values(layer, na.rm = TRUE))
}

#' Jenks natural breaks, computed exactly: the partition of `x` into `k`
#' classes of consecutive values minimising the within-class sum of squares
#' (Jenks 1977; Fisher 1958), by dynamic programming over EVERY value
#' (Ckmeans.1d.dp, Wang & Song 2011). Each break lies midway between adjacent
#' classes, so the partition holds under either interval closure (classInt
#' puts it ON a class maximum, which [low, high) would move up a class).
#' Returns the inner breaks; fewer than k - 1 for fewer distinct values.
natural_breaks <- function(x, k) {
  x <- x[is.finite(x)]
  if (length(x) < 2 || k < 2) return(numeric(0))
  # The package warns (and uses every distinct value as a class) when x holds
  # fewer distinct values than k; the shorter result says the same thing.
  fit <- suppressWarnings(Ckmeans.1d.dp::Ckmeans.1d.dp(x, k = k))
  kk <- length(fit$size)
  if (kk < 2) return(numeric(0))
  # Clusters are numbered in increasing order of value, so on sorted values
  # each one ends where the cumulative size says.
  xs <- sort(x)
  ends <- cumsum(fit$size)[-kk]
  (xs[ends] + xs[ends + 1L]) / 2
}

# Class breaks for the Agronomical styling algorithms: "jenks" is
# natural_breaks(); the classInt styles run under with_seed(12345) ("kmeans"
# draws random starts). Returns the n_c - 1 inner breaks (fewer for fewer
# distinct values), or NULL when vv is too short.
calc_class_breaks <- function(vv, n_c, style) {
  vv <- vv[is.finite(vv)]
  if (length(vv) < n_c) return(NULL)
  if (identical(style, "jenks")) return(natural_breaks(vv, n_c))

  with_seed(12345, {
    tryCatch({
      # suppressMessages: a message() escaping a reactive aborts it mid-evaluation.
      suppressMessages(suppressWarnings(
        classInt::classIntervals(vv, n = n_c, style = style)$brks[2:n_c]))
    }, error = function(e) {
      seq(min(vv, na.rm = TRUE), max(vv, na.rm = TRUE), length.out = n_c + 1)[2:n_c]
    })
  })
}

# Dissolve a classified surface into class-zone polygons for GIS export. The
# classification is the map's and the Area Coverage table's own call
# (terra::classify, rcl_mat, right = FALSE) and the areas come from
# terra::expanse(byValue = TRUE), so the export agrees with both. Boundaries
# are cell edges (not smoothed). `params` is classification_params() (brks,
# rcl_mat, labels, n_c). One row per class present; NULL when none.
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
      area_ha <- area_df$area[match(ids, as.numeric(as.character(area_df$value)))]
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
# These MUST stay top-level: a lambda defined inside a Shiny observer closes
# over the session's environment chain, which future would serialize to every
# worker (the "globals exceed future.globals.maxSize" failure). Top-level
# functions are enclosed by the global environment, never serialized.

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
    vif_threshold = run_params$vif_threshold,
    shp_shared = run_params$shp_shared %||% integer(0),
    shared_res = run_params$shared_res %||% NA_real_
  )
}

# Per-locality worker for the sidebar "OPTIMIZE ALL VARIOGRAMS" button.
# item = list(l, act = data.frame(x, y, v), pre = data.frame(x, y, v) | NULL).
autofit_vgm_item <- function(item, current_crs) {
  display_sse <- function(fit) {
    sse <- attr(fit, "SSErr")
    if (length(sse) == 1L && is.finite(sse)) signif(sse, 4) else "N/A"
  }
  res_a <- list(emp = NULL, fit = NULL, mod = "FAIL", sse = "N/A")
  sub_a_raw <- sf::st_as_sf(item$act, coords = c("x", "y"), crs = current_crs)
  sub_a <- validate_and_project_sf(sub_a_raw)
  sub_a <- merge_colocated(sub_a)

  if (nrow(sub_a) >= 3) {
    lags_a <- calc_scientific_lags(sub_a)
    v_emp_a <- gstat::variogram(v ~ 1, sub_a, width = lags_a$width, cutoff = lags_a$cutoff)
    best_f_a <- robust_vgm_fit(v_emp_a, sub_a$v)
    res_a$emp <- v_emp_a
    res_a$fit <- best_f_a
    res_a$mod <- if (!is.null(best_f_a)) as.character(best_f_a$model[2]) else "FAIL"
    res_a$sse <- display_sse(best_f_a)
  }

  res_p <- list(emp = NULL, fit = NULL, mod = "FAIL", sse = "N/A")
  if (!is.null(item$pre)) {
    sub_p_raw <- sf::st_as_sf(item$pre, coords = c("x", "y"), crs = current_crs)
    sub_p <- validate_and_project_sf(sub_p_raw)
    sub_p <- merge_colocated(sub_p)

    if (nrow(sub_p) >= 3) {
      lags_p <- calc_scientific_lags(sub_p)
      v_emp_p <- gstat::variogram(v ~ 1, sub_p, width = lags_p$width, cutoff = lags_p$cutoff)
      best_f_p <- robust_vgm_fit(v_emp_p, sub_p$v)
      res_p$emp <- v_emp_p
      res_p$fit <- best_f_p
      res_p$mod <- if (!is.null(best_f_p)) as.character(best_f_p$model[2]) else "FAIL"
      res_p$sse <- display_sse(best_f_p)
    }
  }

  list(l = item$l, act = res_a, pre = res_p)
}
