# spatial_pipeline.R - regional orchestration + parallel worker entry points
# (run_regional_interpolation, interp_run_item, autofit_vgm_item, tps_gcv_item,
# idw_opt_item), progress/warning files, CRS projection, dedup, raster and
# class-break utilities. Sourced via spatial_helpers.R.

# How many bounding-box cells the prediction grid converts to sf points at a
# time before testing them against the boundary. Peak memory is one block plus
# the cells that survive the clip, instead of the whole bounding box. Named so
# the suite can shrink it and assert the clip is block-invariant.
.GRID_CLIP_BLOCK_CELLS <- 2e5

# How many BOUNDING-BOX cells a prediction grid may consider before its
# resolution is coarsened. The raster template spans the full bbox before the
# boundary clip, so this bounds the candidate grid, not the surviving one, and
# all three resolution modes are floored by it. Named, like the Classification
# Suite's .CLASSIF_MAX_CANDIDATE_CELLS, so the suite can shrink it and exercise
# the cap without allocating the budget.
.INTERP_MAX_CANDIDATE_CELLS <- 4e6


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
#
# `res_floor` is the smallest cell size the CALLING suite lets a user set, and
# a corrective size below it is dropped from the message: advice the user
# cannot act on is worse than the buffer arm alone. The interpolation suite
# passes 1 (its Fixed slider), the Classification Suite takes the default 5
# (its own slider, and the floor every Auto rule clamps to).
strict_buffer_message <- function(buffer, res, label = NULL, res_floor = 5) {
  g <- strict_buffer_gap(buffer, res)
  if (is.null(g) || !g$short) return(NULL)
  res_arm <- if (floor(g$req_res) < res_floor) "" else sprintf(
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


# Per-observation worker for the governing-factors SHAP loop. TOP-LEVEL for the
# same reason as interp_run_item / autofit_vgm_item / tps_gcv_item /
# idw_opt_item: an inline lambda inside compute_governing_factors would close
# over that function's frame, so future would serialize df, df_clean, rf_model,
# explainer_rf, imp, vip_agg, ale_prof, pdp_prof, old_plan AND shap_cl (the live
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

  # Cooperative cancellation, same file-flag contract as the classification
  # pipeline: the module touches `cancel_file` from the main session and the
  # worker aborts at its next checkpoint. The message is matched by the
  # module's error handler; keep them in sync.
  check_cancel <- function() {
    if (!is.null(cancel_file) && file.exists(cancel_file)) {
      stop("Analysis cancelled by user.", call. = FALSE)
    }
  }
  check_cancel()

  # Everything that draws runs under the shared two-sided sandbox (with_seed,
  # spatial_vgm.R). NOTE: the cluster-teardown on.exit() below is registered
  # inside this block but belongs to THIS function's frame (the block is a
  # promise evaluated in the caller's frame), so the cluster is still torn down
  # at function exit, after the RNG state is restored - the same order as the
  # hand-rolled sandbox this replaced.
  with_seed(12345, {
    # Matrix interface, not a formula: the formula method re-reads column names
    # through make.names() and fails on a target such as "Total N (%)" or a
    # predictor such as "soil moisture". The forest is the same.
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

    # Importance is Breiman's (2001) permutation importance on the OUT-OF-BAG
    # rows: for every tree, the increase in the MSE of its out-of-bag
    # predictions when one predictor is permuted, averaged over the trees.
    # A forest reproduces its own training rows closely, so permuting on the
    # rows it was grown on scores what the forest memorised as well as what it
    # learned; the out-of-bag rows are the ones each tree never saw. The
    # unscaled increase ranks the factors; the scaled form randomForest prints
    # as %IncMSE (divided by its standard error across trees) is kept beside
    # it, but it grows with ntree and is not a z-score (Strobl & Zeileis 2008).
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

#' Reproject a finished surface (in the working CRS) into the Target Mapping
#' CRS. When the two are the same system (terra::same.crs, which equates a
#' proj4 UTM and its EPSG code but not two datums) the surface is relabelled,
#' not resampled: terra::project() onto the same system rewrote every cell at
#' float precision (up to 3e-8 relative on the golden digest) to change only
#' the CRS's spelling, which the file now carries as the target's EPSG code.
#' Another projected CRS gets cells of the run's resolution `res_m` (metres)
#' on one lattice (origin 0): terra's own choice gives a 30 m grid 30.10 m
#' cells from UTM 35N to 36N, and per-locality origins that the merged surface
#' would have to resample. A geographic target keeps terra's choice of degrees.
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

#' A boundary uploaded without a .prj carries no CRS; it is taken to be in the
#' Input Data CRS (the CRS of the sample coordinates), which is what the upload
#' notice promises. One rule for the pipeline, the Data Setup note and the
#' Classification Suite's polygon scope. A layer that has a CRS, or an
#' unparseable `crs`, is returned unchanged.
shp_assume_crs <- function(shp, crs) {
  if (is.null(shp) || !is.na(sf::st_crs(shp))) return(shp)
  co <- suppressWarnings(tryCatch(sf::st_crs(crs), error = function(e) NULL))
  if (is.null(co) || is.na(co)) return(shp)
  sf::st_set_crs(shp, co)
}

#' An uploaded boundary as polygons. Polygon layers pass through unchanged;
#' point or line layers become the convex hull of all their features (one row),
#' the same treatment classif_scope_polygons() gives them. NULL when the hull
#' does not enclose an area (fewer than 3 non-collinear vertices). Without this,
#' a point layer reached the grid clip as a MULTIPOINT boundary of zero area and
#' the locality failed.
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

#' Auto grid resolution for one boundary area: ~100,000 cells, clamped to
#' [5, 1000] m. Shared by Auto (Per Locality) and Auto (Global).
auto_grid_resolution <- function(area_m2) {
  max(5, min(1000, sqrt(area_m2 / 100000)))
}

#' Raster template with SQUARE cells of exactly `res`, covering `bbox`.
#' terra::rast(ext, resolution = res) keeps the extent and stretches the cells
#' to fit it (a 1000 x 800 m box at 45 m gives 45.45 x 44.44 m cells), so the
#' extent grows to whole cells instead, anchored at the box's lower-left corner.
#' `snap = TRUE` anchors at the lattice of multiples of `res` in the CRS, so
#' every locality gridded at the same `res` in the same CRS shares one lattice.
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

  # The locality is projected ONCE, from every row carrying coordinates,
  # BEFORE any covariate filter. The working CRS - for a geographic upload,
  # the UTM zone of the locality's centre - is a property of the locality,
  # not of which covariates happen to be selected. Filtering first let OK and
  # RK/RFK/CK land in different zones, so their coordinates, variogram lags
  # and spatial folds described different grids. A no-op for metre-projected
  # input, where validate_and_project_sf returns its argument.
  pts_all <- validate_and_project_sf(pts_raw)

  if(nrow(pts_all) < 3) {
    return(list(ok = FALSE, msg = paste0("Warning in ", l, ": Insufficient data points after UTM conversion (needed >= 3, got ", nrow(pts_all), ").")))
  }

  # The rows complete for every selected covariate: what RK/RFK/CK map with,
  # and the CV population OK scores under the Comparable switch. Computed only
  # where it is used - IDW and TPS can carry a stale covariate selection that
  # the dispatch never validated, and all_of() would stop on a name the
  # uploaded table no longer has.
  uses_covariates <- current_method %in% c("RK", "RFK", "CK") ||
    (identical(current_method, "OK") && identical(m_params$cv_population, "comparable"))
  covariate_complete <- if (uses_covariates && length(aux_vars) > 0) {
    dplyr::filter(pts_all, dplyr::if_all(dplyr::all_of(aux_vars), ~!is.na(.)))
  } else pts_all

  pts_projected <- if (current_method %in% c("RK", "RFK", "CK")) covariate_complete else pts_all

  if (nrow(pts_projected) < 3) {
    return(list(ok = FALSE, msg = paste0("Warning in ", l, ": Insufficient data points after covariate filtering (needed >= 3, got ", nrow(pts_projected), ").")))
  }

  pts <- pts_projected

  # The DISPLAY set: deduplicated by coordinate, NOT filtered on the target.
  # That order is the reverse of the fitted set's (dedup_valid_points drops
  # target-NA rows FIRST, then deduplicates), so the two are not nested - where
  # a co-located pair carries the measurement on one member only, this keeps
  # the first member and the fit keeps the measured one. Deliberate: rv$sf is
  # what the popup system, the point colour-by and the locality tables read,
  # and changing the order here would move which member is displayed without
  # changing the fit. Points with no measured value are styled apart on the map
  # (add_styled_points, value_col) so the display says so.
  coords <- sf::st_coordinates(pts)
  c_round <- data.frame(
    x = round(coords[, "X"], 2),
    y = round(coords[, "Y"], 2)
  )
  pts <- pts[!duplicated(c_round), ]
  if(nrow(pts) < 3) {
    return(list(ok = FALSE, msg = paste0("Warning in ", l, ": Insufficient unique points after duplicate coordinate removal (needed >= 3, got ", nrow(pts), ").")))
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
        # The user supplied a boundary shapefile but it cannot be applied to
        # this locality (projection failure or no spatial overlap); say so
        # instead of silently swapping in the point-derived boundary.
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

  list(bound = bound, local_shp = local_shp, local_res = local_res,
       b_dist_local = b_dist_local, grid_res_safe = grid_res_safe, warn = warn, log = log)
}

#' The one cell size of an Auto (Global) run: the Auto resolution of the
#' LARGEST locality boundary, so every locality keeps at most ~100,000 cells
#' and all share that size. Boundaries are built exactly as the run builds
#' them. NA when no locality yields a boundary.
shared_auto_resolution <- function(items, run_params) {
  areas <- vapply(items, function(item) tryCatch({
    pp <- .locality_points(item$l, item$pts_data, run_params$current_crs,
                           run_params$current_method, run_params$aux_vars, item$m_params)
    if (!isTRUE(pp$ok)) return(NA_real_)
    lb <- .locality_boundary(item$l, pp$pts, run_params$current_crs, run_params$current_method,
                             run_params$shp_bound, run_params$b_type, run_params$buff_mode,
                             run_params$b_dist, run_params$res_mode, run_params$grid_res,
                             run_params$shp_shared %||% integer(0))
    as.numeric(sf::st_area(lb$bound))
  }, error = function(e) NA_real_), numeric(1))
  if (!any(is.finite(areas))) return(NA_real_)
  auto_grid_resolution(max(areas, na.rm = TRUE))
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
  res_out <- list(l = l, r_a = NULL, r_p = NULL, r_res = NULL, bound = NULL, pts = NULL,
                  v_emp_act = NULL, v_fit_act = NULL, cv_act = NULL, cv_obj_act = NULL, cv_reps_act = NULL, summ_act = NULL, rf_act = NULL, gstat_act = NULL,
                  v_emp_pre = NULL, v_fit_pre = NULL, cv_pre = NULL, cv_obj_pre = NULL, cv_reps_pre = NULL, summ_pre = NULL, rf_pre = NULL, gstat_pre = NULL, log_msg = "", actual_res = NULL,
                  cv_info_act = NULL, cv_info_pre = NULL)
  
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

    lb <- .locality_boundary(l, pts, current_crs, current_method, shp_bound, b_type,
                             buff_mode, b_dist, res_mode, grid_res, shp_shared)
    for (w in lb$warn) write_warning_file(l, "act", w)
    for (g in lb$log) res_out$log_msg <- paste0(res_out$log_msg, "\n", g)
    bound <- lb$bound
    local_shp <- lb$local_shp
    b_dist_local <- lb$b_dist_local
    grid_res_safe <- lb$grid_res_safe

    bbox <- sf::st_bbox(bound)
    area_m2 <- as.numeric(sf::st_area(bound))

    # The raster template spans the whole bounding box before the boundary
    # clip, so a fine resolution over a large extent puts hundreds of millions
    # of candidate cells through the clip. Cap the candidate grid at ~4M cells,
    # matching the floor classif_build_grid applies to both of its resolution
    # modes. Fires only in that pathological case.
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
        # Two channels, for the reason spelled out at the strict-buffer check
        # below: the progress panel keeps one warning per locality and closes
        # with the run, so the [WARN] log line is what still tells the user the
        # surface was not computed at the size they asked for.
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
      # Absolute sanity floor only. The slider itself stops at 1 m, but a
      # restored run-config could carry any value, and a sub-decimetre grid over
      # any real extent is a memory accident rather than an intent.
      actual_res <- max(actual_res, 0.1)
    } else if (shared_grid) {
      # Auto (Global): one cell size for every locality of the run, the Auto
      # resolution of the largest boundary (shared_auto_resolution), on one
      # snapped lattice. A strict-buffer boundary can span a far larger box
      # than its area suggests, hence the same cap as Fixed.
      actual_res <- cap_res(shared_res, "Shared Auto (Global) resolution")
    } else {
      # Auto is SELF-CONTAINED: the resolution follows this locality's own
      # boundary area (~100k cells), clamped to [5, 1000] m. It must NOT be
      # floored against grid_res — that slider is hidden outside Fixed mode
      # (ui_sidebar.R) and in Auto modes it holds the GLOBAL recommendation
      # (max(mean_1NN * 0.5, max_dim / 300), server_data_setup.R), so a widely
      # spread dataset pushed the floor to ~50 m and silently coarsened every
      # compact locality's density-derived grid via an input the user could
      # neither see nor set.
      if (identical(res_mode, "global")) {
        write_warning_file(l, "act", "The shared Auto (Global) resolution could not be computed; this locality uses its own Auto resolution.")
      }
      # Capped like the other two modes: the target is ~100,000 cells inside
      # the BOUNDARY, and a strict point-buffer boundary occupies a small
      # fraction of its own bounding box, which is what the template spans.
      actual_res <- cap_res(auto_grid_resolution(area_m2), "Auto grid resolution")
    }

    # Authoritative strict-boundary coherence check: only here is the effective
    # resolution known for every mode (Auto derives it from this locality's own
    # boundary area, so no pre-run UI advisory can be exact). A buffer below
    # half the cell diagonal silently drops the cells of isolated samples, which
    # reads as sampled points sitting on blank map. Advisory only - the run is
    # scientifically valid, the support is just under-resolved.
    if (identical(b_type, "strict") && is.null(local_shp)) {
      # res_floor 1: the corrective cell size is set on the Fixed slider, which
      # reaches 1 m. In an Auto mode that means switching to Fixed, the same
      # move a 5 m suggestion would need.
      sb_msg <- strict_buffer_message(b_dist_local, actual_res, res_floor = 1)
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

    # Fixed and Auto (Global) grids sit on one lattice of multiples of the cell
    # size, so localities that share a working CRS and a cell size share their
    # cells: terra::merge resamples every raster whose origin differs from the
    # first one's, which interpolated the merged surface of every locality but
    # one. Auto (Per Locality) gives each locality its own cell size, so its
    # grids start at their own boundary's corner.
    grid_r <- grid_template(bbox, actual_res, sf::st_crs(pts)$wkt,
                            snap = shared_grid || identical(res_mode, "fixed"))

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
      # A coarse cell size over a small boundary can leave NO grid node inside
      # it - a Fixed value, or the shared Auto (Global) size taken from a much
      # larger boundary elsewhere in the run. The engines would then krige the full bbox and the mask
      # would discard every cell - a blank locality with no message, after
      # paying for the whole interpolation. Name the cause and skip instead
      # (the surface was all-NA in this state before; nothing displayable is
      # lost). classif_build_grid stops loudly in the same situation.
      write_warning_file(l, "act", sprintf(
        "No grid node falls inside this locality's boundary at %.1f m resolution; the surface would be empty. %s",
        actual_res,
        if (identical(res_mode, "fixed")) "Reduce the fixed grid resolution or widen the boundary/buffer."
        else if (shared_grid) "The Auto (Global) cell size comes from the largest boundary of the run and is too coarse for this one: switch to Auto (Per Locality), or set a Fixed resolution, or widen the boundary/buffer."
        else "Widen the boundary/buffer, or set a Fixed resolution."))
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

    # Which samples each surface is cross-validated on. RK/RFK/CK can only use
    # the covariate-complete rows. OK maps every sample with a measured target
    # and scores those by default (Native); under the Comparable switch it is
    # TRAINED AND SCORED on the covariate-complete rows instead, deduplicated
    # and folded exactly as the covariate engines fold them, so a comparison
    # between engines rests on the same samples and the same partition. The OK
    # map still uses every sample either way.
    ok_comparable <- identical(current_method, "OK") &&
      identical(m_params$cv_population, "comparable") && length(aux_vars) > 0
    cv_pop_label <- if (current_method %in% c("RK", "RFK", "CK")) "common rows"
      else if (!identical(current_method, "OK")) NA_character_
      else if (ok_comparable) "common rows" else "native rows"
    cv_population_of <- function(surface_pts, target) {
      if (!ok_comparable) return(surface_pts)
      dedup_valid_points(covariate_complete, target)
    }
    # A population too small to fold leaves the engine's own CV plan unset; the
    # engine then stops its CV with a named message and still draws the map.
    # Only the kriging engines read a plan: IDW and TPS fold their own point
    # set, so nothing is built for them.
    attach_cv_plan <- function(mp, pop, prefix) {
      if (is.na(cv_pop_label)) return(mp)
      mp$cv_data <- pop
      if (nrow(pop) >= 3) {
        mp$cv_plan <- build_cv_plan(pop, m_params$cv_strategy, m_params$cv_repeats)
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
      # The same screen every CV fold runs on its own training rows, so the map
      # and the folds can never screen by different rules.
      tryCatch(screen_covariates(p, aux_vars, vif_threshold)$kept, error = function(e) NULL)
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
        # BOTH channels, like the grid-coarsening warning: the progress panel
        # holds one warning per locality and surface and is gone the moment the
        # maps are revealed, so the run log is what makes this survive.
        if (.is_degenerate_covariate(pts_a$v)) {
          write_warning_file(l, "act", .degenerate_target_msg)
          res_out$log_msg <- paste0(res_out$log_msg, "
[WARN] ", l, " (Actual): ",
                                    .degenerate_target_msg)
        }
        lags_a <- calc_scientific_lags(pts_a)
        mp_a <- list(idw_p = m_params$idw_p_act, idw_nmax = m_params$idw_nmax, cov_params = list(idw_p = m_params$idw_p_act, idw_nmax = m_params$idw_nmax), tps_lambda = m_params$tps_lambda_act, pre_fit = m_params$pre_fit_act, grid_aux = grid_aux, cv_strategy = m_params$cv_strategy, cv_repeats = m_params$cv_repeats, cancel_file = cancel_file_val, rfk_uncertainty = m_params$rfk_uncertainty, rf_ntree = m_params$rf_ntree, ck_nmax = m_params$ck_nmax, aux_kept = aux_kept_a)
        mp_a <- attach_cv_plan(mp_a, cv_population_of(pts_a, "v"), "act")
        if (!is.null(cancel_file_val) && file.exists(cancel_file_val)) stop("Model generation cancelled by user.")
        res_a_list <- apply_interpolation(pts_a, "v", current_method, grid_p, aux_vars, lags_a, mp_a, l, "act", vif_threshold)
        res_out$cv_info_act <- cv_info_of(mp_a, res_a_list)
        res_out$v_emp_act <- res_a_list$v_emp; res_out$v_fit_act <- res_a_list$fit; res_out$cv_act <- res_a_list$cv_metrics; res_out$cv_obj_act <- res_a_list$cv_obj
        res_out$cv_reps_act <- res_a_list$cv_obj_reps
        res_out$tps_fit_act <- res_a_list$tps_fit
        # detach_model_frame: the fitted model's terms otherwise carry this
        # locality's whole pipeline frame (point set, grids, kriging output)
        # back to the main session and into every archived copy of the run.
        res_out$summ_act <- detach_model_frame(res_a_list$model_summary); res_out$rf_act <- res_a_list$rf_model; res_out$gstat_act <- res_a_list$gstat_obj
        # Covariates the mapped model used and the ones the screen removed
        # (RK/RFK/CK only), for the run configuration.
        res_out$aux_used_act <- res_a_list$aux_used; res_out$aux_dropped_act <- res_a_list$aux_dropped
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
            r_a <- terra::rasterize(res_a_list$res_sf, grid_r, field=fields_a) %>% terra::mask(terra::vect(bound)) %>% project_to_target(crs_sel, actual_res)
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
              write_warning_file(l, "pre", .degenerate_target_msg)
              res_out$log_msg <- paste0(res_out$log_msg, "
[WARN] ", l, " (Predicted): ",
                                        .degenerate_target_msg)
            }
            lags_p <- calc_scientific_lags(pts_p)
            mp_p <- list(idw_p = m_params$idw_p_pre, idw_nmax = m_params$idw_nmax, cov_params = list(idw_p = m_params$idw_p_act, idw_nmax = m_params$idw_nmax), tps_lambda = m_params$tps_lambda_pre, pre_fit = m_params$pre_fit_pre, grid_aux = grid_aux, cv_strategy = m_params$cv_strategy, cv_repeats = m_params$cv_repeats, cancel_file = cancel_file_val, rfk_uncertainty = m_params$rfk_uncertainty, rf_ntree = m_params$rf_ntree, ck_nmax = m_params$ck_nmax, aux_kept = aux_kept_p)
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
            # The TPS counterpart: unseparated, a lambda on Auto (GCV) is the one
            # GCV selects for the MEASURED values of the same rows, reselected in
            # every CV fold from that fold's training rows. A fixed lambda is
            # shared at dispatch already.
            if (current_method == "TPS" && isFALSE(m_params$sep_fit)) mp_p$tps_gcv_col <- "v"
            res_p_list <- apply_interpolation(pts_p, "pv", current_method, grid_p, aux_vars, lags_p, mp_p, l, "pre", vif_threshold)
            res_out$cv_info_pre <- cv_info_of(mp_p, res_p_list)
            res_out$v_emp_pre <- res_p_list$v_emp; res_out$v_fit_pre <- res_p_list$fit; res_out$cv_pre <- res_p_list$cv_metrics; res_out$cv_obj_pre <- res_p_list$cv_obj
            res_out$cv_reps_pre <- res_p_list$cv_obj_reps
            res_out$tps_fit_pre <- res_p_list$tps_fit
            res_out$summ_pre <- detach_model_frame(res_p_list$model_summary); res_out$rf_pre <- res_p_list$rf_model; res_out$gstat_pre <- res_p_list$gstat_obj
            res_out$aux_used_pre <- res_p_list$aux_used; res_out$aux_dropped_pre <- res_p_list$aux_dropped
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
                r_p <- terra::rasterize(res_p_list$res_sf, grid_r, field=fields_p) %>% terra::mask(terra::vect(bound)) %>% project_to_target(crs_sel, actual_res)
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
        err_mod <- gstat::idw(err ~ 1, pts_err, grid_p, nmax = m_params$idw_nmax %||% 12, idp = 2, debug.level = 0)
        r_err <- terra::rasterize(err_mod, grid_r, field="var1.pred") %>% terra::mask(terra::vect(bound)) %>% project_to_target(crs_sel, actual_res)
        res_out$r_point_err <- terra::wrap(r_err)
    }
    
    # The CV object is the only carrier of cross-validation residuals, and it
    # names the samples it scored through its own geometry: a surface's CV
    # population need not be its point set (OK Comparable scores the covariate-
    # complete rows), so a positional vector would have to match a point set it
    # does not name. Join on the rounded-coordinate key, the same 2-dp key both
    # dedup passes use; `pts` is coordinate-deduped, so the key is unique there.
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
    # Row identity is internal plumbing for the CV plan. rv$sf IS res_out$pts,
    # and the popup system, the point colour-by and the label field all offer
    # its columns by name, so it must not survive the trip back.
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

# Value vector of a raster's display layer (var1.pred when present, else the
# first layer), accepting both live and Packed SpatRasters. PackedSpatRaster
# is NOT subsettable ([[/values error on it), so every consumer that can see
# a raster that crossed a future boundary must go through this.
# `band = "var1.var"` reads the prediction-variance band instead, and returns
# NULL when the raster has none (never the prediction band in its place).
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
#' classes of consecutive values that minimises the within-class sum of squared
#' deviations (Jenks 1977; Fisher 1958), found by dynamic programming over
#' EVERY value (Ckmeans.1d.dp, Wang & Song 2011; deterministic, O(n log n)).
#' Each break lies midway between the largest value of one class and the
#' smallest of the next, so the partition of the data is the optimum under
#' either interval closure. A break placed ON a class's largest value (as
#' classInt's "jenks" returns it) moves that value into the next class under
#' the app's [low, high) convention. Returns the inner breaks: fewer than
#' k - 1 when `x` holds fewer than k distinct values.
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

# Class-break computation for the Agronomical styling algorithms. "jenks" is
# natural_breaks() over every value. classInt's "kmeans" draws random starts,
# so it runs under the app's two-sided seed sandbox (seed 12345, caller's
# .Random.seed restored). Returns the n_c - 1 inner break values (fewer for
# data with fewer distinct values), or NULL when vv is too short.
calc_class_breaks <- function(vv, n_c, style) {
  vv <- vv[is.finite(vv)]
  if (length(vv) < n_c) return(NULL)
  if (identical(style, "jenks")) return(natural_breaks(vv, n_c))

  with_seed(12345, {
    tryCatch({
      # suppressMessages: a message() escaping a reactive is caught by any
      # consumer's tryCatch and aborts the reactive mid-evaluation, poisoning
      # its cached state.
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
  sub_a <- sub_a[!duplicated(round(sf::st_coordinates(sub_a), 2)), ]

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
    sub_p <- sub_p[!duplicated(round(sf::st_coordinates(sub_p), 2)), ]

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

# Below this many distinct samples the optimizers search nothing and store
# nothing, so the locality keeps the sidebar setting: a GCV curve or a CV power
# search over four points is not an estimate, and storing a stand-in value
# would override the setting the user chose (Auto (GCV) became exact
# interpolation).
OPTIMIZER_MIN_POINTS <- 5L
.optimizer_skip_note <- function(n) {
  sprintf("%d distinct sample%s, fewer than %d; nothing stored, so the sidebar setting applies",
          n, if (n == 1L) "" else "s", OPTIMIZER_MIN_POINTS)
}

# Per-locality worker for the "OPTIMIZE TPS LAMBDA" button (GCV curve search).
# item = list(l, df = data.frame(x, y, v)).
tps_gcv_item <- function(item, current_crs) {
  if (nrow(item$df) < OPTIMIZER_MIN_POINTS) {
    return(list(l = item$l, skipped = .optimizer_skip_note(nrow(item$df))))
  }

  # Project before normalizing to the unit box, exactly as apply_TPS does on the
  # run path: on geographic coordinates 1 deg lon != 1 deg lat on the ground, so
  # the point-cloud aspect ratio (and the GCV-optimal lambda) would otherwise
  # differ from the run that consumes this value. No-op for metre-based projections.
  pts_sf <- validate_and_project_sf(
    sf::st_as_sf(item$df, coords = c("x", "y"), crs = current_crs))
  # Dedup co-located points exactly as dedup_valid_points does on the run path
  # (the server observer already na.omit()ed, so NA-filter-then-dedup order
  # holds): fields::Tps treats replicates via its pure-error handling, which
  # shifts the GCV curve away from the deduped point set the run actually fits.
  pts_sf <- pts_sf[!duplicated(round(sf::st_coordinates(pts_sf), 2)), ]
  if (nrow(pts_sf) < OPTIMIZER_MIN_POINTS) {
    return(list(l = item$l, skipped = .optimizer_skip_note(nrow(pts_sf))))
  }
  raw_coords <- sf::st_coordinates(pts_sf)
  vals <- pts_sf$v

  xm <- min(raw_coords[, 1]); xM <- max(raw_coords[, 1])
  ym <- min(raw_coords[, 2]); yM <- max(raw_coords[, 2])
  max_range <- max(xM - xm, yM - ym)
  if (max_range == 0) max_range <- 1
  pts <- cbind((raw_coords[, 1] - xm) / max_range,
               (raw_coords[, 2] - ym) / max_range)

  tryCatch({
    mod <- fields::Tps(pts, vals, scale.type = "unscaled")
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
  if (nrow(item$df) < OPTIMIZER_MIN_POINTS) {
    return(list(l = item$l, skipped = .optimizer_skip_note(nrow(item$df))))
  }
  # Project first so optimize_idw_p's nmax neighbour selection and distance-decay
  # weighting run on the same metric coordinates the run pipeline uses. IDW is
  # scale-invariant, but degree axes are anisotropic (1 deg lon != 1 deg lat), so
  # a geographic CRS distorts both. No-op for metre-based projections.
  pts <- validate_and_project_sf(
    sf::st_as_sf(item$df, coords = c("x", "y"), crs = current_crs))
  # Dedup co-located points exactly as dedup_valid_points does on the run path
  # (NA rows already dropped by the server observer's na.omit()): a co-located
  # twin predicts its held-out partner at distance zero, so every candidate
  # power scores an exact hit there and the search is inflated.
  pts <- pts[!duplicated(round(sf::st_coordinates(pts), 2)), ]
  if (nrow(pts) < OPTIMIZER_MIN_POINTS) {
    return(list(l = item$l, skipped = .optimizer_skip_note(nrow(pts))))
  }
  best_f <- optimize_idw_p(pts, "v", nmax = idw_nmax_val, cv_strategy = cv_strategy)
  list(l = item$l, best_f = best_f)
}
