# global_utils.R - static configuration and pure (non-reactive) functions
# extracted from the top of monolith.R. Must stay free of reactive code:
# validate_crs() in particular is called outside reactive blocks so a bad CRS
# is caught before the st_transform pipeline.
#' Location of the run-duration history log. It used to be built RELATIVE to the
#' process working directory, so the file landed wherever the app happened to be
#' started from (silently unwritable on a read-only deployment, and shared
#' between concurrent sessions). It now lives in the user's per-application data
#' directory, overridable through `monolith_history_dir` - the same option
#' pattern `update_progress_file` uses for `monolith_progress_dir`, so tests can
#' redirect it without touching the real one.
#' Concurrent multi-session appends are unsynchronised (no file lock): the
#' primary deployment is a single-user desktop app, and a torn append only costs
#' one ETA record, so the risk is tolerated rather than engineered away.
monolith_history_file <- function() {
  file.path(
    getOption("monolith_history_dir", tools::R_user_dir("monolith", which = "data")),
    "run_history.csv"
  )
}

estimate_run_duration <- function(loc_sample_counts, method, comp_mode, cores) {
  # History aware run duration estimator
  history_file <- monolith_history_file()

  # Base multipliers for different methods. Note: RFK, RK, and CK are calibrated 
  # from real measurements; OK/IDW/TPS multipliers are still unverified guesses.
  method_mult <- switch(method,
    "RFK" = 1.0,
    "RK"  = 1.0,
    "CK"  = 1.3,
    "OK"  = 0.5,
    "IDW" = 0.5,
    "TPS" = 0.3,
    1.0
  )
  
  # Ensure valid sample counts
  loc_sample_counts[is.na(loc_sample_counts) | loc_sample_counts == 0] <- 50
  
  n_locs <- length(loc_sample_counts)
  n_models <- n_locs * (if(comp_mode) 2 else 1)
  
  loc_times_sec <- numeric(n_locs)
  history_data <- NULL
  
  if (file.exists(history_file)) {
    # The whole block is the tryCatch VALUE. The error handler used to run
    # `history_data <- NULL`, which assigns into the handler's own frame and
    # leaves the outer binding untouched: a throw partway through the filters
    # (e.g. an old run_history.csv with no cores_used column) left the
    # partially-filtered frame in place and the ETA lm was fitted on it.
    history_data <- tryCatch({
      hd <- read.csv(history_file)
      hd <- hd[hd$method == method, ]

      # Try filtering by comp_mode if enough data
      comp_history <- hd[hd$comp_mode == comp_mode, ]
      if (nrow(comp_history) >= 5) {
        hd <- comp_history
      }

      # Try filtering by cores if enough data
      cores_history <- hd[hd$cores_used == cores, ]
      if (nrow(cores_history) >= 5) {
        hd <- cores_history
      }
      hd
    }, error = function(e) NULL)
  }
  
  is_history_based <- !is.null(history_data) && nrow(history_data) >= 5
  
  if (is_history_based) {
    fit <- tryCatch(lm(per_locality_share_sec ~ n_samples, data = history_data), error = function(e) NULL)
    if (!is.null(fit)) {
      preds <- predict(fit, newdata = data.frame(n_samples = loc_sample_counts))
      loc_times_sec <- pmax(5, preds)
    } else {
      is_history_based <- FALSE
    }
  }
  
  if (!is_history_based) {
    # Cold-start formula based on real data points (79->150s, 355->510s for 2 models)
    # Scaled down to 70% per user request
    base_sec <- pmax(5.25, 16.45 + 0.455 * loc_sample_counts)
    model_time <- base_sec * method_mult
    loc_times_sec <- model_time * (if (comp_mode) 2 else 1)
  }
  
  max_single_loc_time <- max(loc_times_sec)
  
  # Distributed efficiency fix (0.75 effective cores)
  eff_factor <- 0.75
  distributed_time <- sum(loc_times_sec) / max(1, (cores * eff_factor))
  
  # Apply fudge factor (more uncertainty for cold start)
  fudge_mult <- if (is_history_based) 1.25 else 1.4
  est_time_sec <- max(max_single_loc_time, distributed_time) * fudge_mult
  
  est_time_str <- if (est_time_sec < 60) {
    paste(round(est_time_sec), "seconds")
  } else {
    paste(round(est_time_sec / 60, 1), "minutes")
  }
  # Without at least 5 matching history records the number comes from the
  # cold-start formula (two hardware measurements, and unverified multipliers
  # for OK/IDW/TPS), so it is labelled as the guess it is.
  if (!is_history_based) est_time_str <- paste0(est_time_str, " (rough estimate)")

  estimate_text <- paste0("~", n_models, " locality model(s), ~", est_time_str, " estimated.\n\n* Note: This time estimate is calibrated based on a below-average hardware benchmark. Actual execution time on modern or cloud hardware will likely be significantly faster.")
  is_long_run <- est_time_sec >= 120 || method %in% c("RK", "RFK", "CK")
  
  return(list(
    est_time_sec = est_time_sec,
    est_time_str = est_time_str,
    estimate_text = estimate_text,
    is_long_run = is_long_run,
    n_models = n_models
  ))
}


validate_crs <- function(crs_selection, error_prefix = "Invalid CRS provided", duration = NULL) {
  tryCatch({
    c_obj <- sf::st_crs(crs_selection)
    if (is.na(c_obj)) stop("Invalid CRS format.")
    
    t_obj <- terra::crs(crs_selection)
    if (t_obj == "") stop("Invalid CRS for terra.")
    
    c_obj
  }, error = function(e) {
    showNotification(paste(error_prefix, e$message), type = "error", duration = duration)
    NULL
  })
}

common_crs <- c(
  "WGS 84 (EPSG:4326)" = "EPSG:4326",
  "UTM 35N (EPSG:32635)" = "EPSG:32635",
  "UTM 33N (EPSG:32633)" = "EPSG:32633",
  "UTM 34N (EPSG:32634)" = "EPSG:32634",
  "S-JTSK / Krovak East North (EPSG:5514)" = "EPSG:5514",
  "Pseudo-Mercator (EPSG:3857)" = "EPSG:3857"
)

dashboard_palettes <- c("viridis", "Greens", "Blues", "Oranges", "YlOrRd", "RdYlBu", "BrBG", "YlOrBr", "Greys", "Spectral")

palette_choices_precomputed <- (function() {
  pals <- dashboard_palettes
  labels <- sapply(pals, function(p) {
    cols <- if (p == "viridis") {
      viridis::viridis(5)
    } else {
      info <- RColorBrewer::brewer.pal.info
      max_cols <- if (p %in% rownames(info)) info[p, "maxcolors"] else 5
      n_cols <- max(3, min(5, max_cols))
      RColorBrewer::brewer.pal(n_cols, p)
    }
    swatches <- paste0(sapply(cols, function(c) {
      sprintf('<div style="width: 15px; height: 15px; background-color: %s !important; border: 0.5px solid #ccc; display: inline-block; margin-left: 2px;"></div>', c)
    }), collapse = "")
    display_name <- if (p == "viridis") "Viridis" else p
    sprintf('<div style="display: flex; justify-content: space-between; align-items: center; width: 100%%;"><span>%s</span><div style="display: flex;">%s</div></div>', display_name, swatches)
  })
  setNames(pals, labels)
})()

styler_fields <- list(
  title_size = list(fn = updateSliderInput, name = "styler_title_size"),
  base_size = list(fn = updateSliderInput, name = "styler_base_size"),
  x_size = list(fn = updateSliderInput, name = "styler_x_size"),
  y_size = list(fn = updateSliderInput, name = "styler_y_size"),
  label_size = list(fn = updateSliderInput, name = "styler_label_size"),
  legend_size = list(fn = updateSliderInput, name = "styler_legend_size"),
  legend_key_size = list(fn = updateSliderInput, name = "styler_legend_key_size"),
  font_family = list(fn = updateSelectInput, name = "styler_font_family", val_param = "selected"),
  label_orient = list(fn = updateSelectInput, name = "styler_label_orient", val_param = "selected"),
  legend_pos = list(fn = updateSelectInput, name = "styler_legend_pos", val_param = "selected"),
  legend_dir = list(fn = updateSelectInput, name = "styler_legend_dir", val_param = "selected"),
  legend_text_angle = list(fn = updateSelectInput, name = "styler_legend_text_angle", val_param = "selected"),
  margin_t = list(fn = updateNumericInput, name = "styler_margin_t"),
  margin_r = list(fn = updateNumericInput, name = "styler_margin_r"),
  margin_b = list(fn = updateNumericInput, name = "styler_margin_b"),
  margin_l = list(fn = updateNumericInput, name = "styler_margin_l"),
  show_grid = list(fn = updateCheckboxInput, name = "styler_show_grid"),
  high_contrast = list(fn = updateCheckboxInput, name = "styler_high_contrast"),
  resid_palette = list(fn = updateSelectInput, name = "styler_resid_palette", val_param = "selected"),
  aspect_ratio = list(fn = updateNumericInput, name = "styler_aspect_ratio"),
  width = list(fn = updateNumericInput, name = "styler_width"),
  height = list(fn = updateNumericInput, name = "styler_height"),
  dpi = list(fn = updateNumericInput, name = "styler_dpi"),
  format = list(fn = updateSelectInput, name = "styler_format", val_param = "selected")
)

sync_styler_config <- function(cfg, session) {
  for (key in names(styler_fields)) {
    val <- cfg[[key]]
    if (is.null(val)) val <- cfg[[paste0("styler_", key)]]
    
    if (!is.null(val)) {
      field <- styler_fields[[key]]
      args <- list(session = session, inputId = field$name)
      val_param <- if (!is.null(field$val_param)) field$val_param else "value"
      args[[val_param]] <- val
      do.call(field$fn, args)
    }
  }
}

#' The raster behind an export-registry item, or NULL when it has none.
#'
#' GeoTIFF export is offered only for registry items whose payload IS one
#' raster surface. Two items registered under type "map" carry something else
#' and have no single-raster form: the Actual vs Predicted comparison holds a
#' list of two packed rasters (type "map_combined"), and the Point Error Map
#' holds sf points plus a boundary. Both stay image-only exports; the vector
#' route for point and polygon geometry is the Map Viewer's GIS export.
export_raster_payload <- function(item) {
  if (is.null(item) || !is.list(item)) return(NULL)
  if (!identical(item$type, "map")) return(NULL)
  obj <- item$obj
  if (inherits(obj, "PackedSpatRaster")) return(terra::unwrap(obj))
  if (inherits(obj, "SpatRaster")) return(obj)
  NULL
}

#' File extension for a styler format token ("gtiff" writes a .tif).
styler_format_ext <- function(fmt) {
  switch(fmt %||% "png",
         gtiff = "tif", tiff = "tiff", pdf = "pdf", jpg = "jpg", png = "png",
         "png")
}

#' Write a SpatRaster as a compressed GeoTIFF at `file`.
#'
#' terra picks its driver from the file extension, so a path without one would
#' silently produce a non-TIFF. Shiny does hand download handlers a path that
#' keeps the extension declared by `filename()`, so this is belt and braces
#' rather than a live failure mode; it stays because the guarantee is Shiny's,
#' not ours, and a wrong-format file is a silent corruption.
#' Values, CRS and extent are written exactly as computed: a GeoTIFF is the
#' data, so no styling, palette or DPI applies to it. Multi-layer surfaces
#' (kriging returns prediction and variance) are written as multi-band files
#' with the layer names kept as band descriptions.
write_geotiff <- function(r, file) {
  if (is.null(r) || !inherits(r, "SpatRaster")) stop("No raster surface to export.")
  target <- if (tolower(tools::file_ext(file)) %in% c("tif", "tiff")) file else paste0(file, ".tif")
  terra::writeRaster(r, target, overwrite = TRUE, gdal = c("COMPRESS=LZW"))
  if (!identical(target, file)) {
    on.exit(unlink(target), add = TRUE)
    if (!file.copy(target, file, overwrite = TRUE)) {
      stop("Could not move the written GeoTIFF into place.")
    }
  }
  invisible(file)
}

#' Fold an sf layer's attribute table into the two fields KML can carry.
#'
#' GDAL's KML driver writes only <name> and <description> per placemark: every
#' other field is dropped on write, so a class-zone layer would arrive in Google
#' Earth as unlabelled polygons with no break limits and no hectares. The
#' LIBKML driver does support full attributes through ExtendedData, but it is
#' absent from the GDAL that ships with sf on Windows, so it cannot be relied
#' on. Instead, the label goes to <name> (what a GIS shows next to the polygon)
#' and the whole record is serialised into <description>, which is where a KML
#' reader looks when the placemark is clicked. Nothing is silently lost.
#' NA prints as an empty value: the open outer breaks are genuinely unbounded,
#' and "class_max:" reads as such where "class_max: NA" would look like a
#' failed computation.
kml_attribute_fields <- function(sf_obj, name_field = NULL) {
  geom_col <- attr(sf_obj, "sf_column")
  attr_cols <- setdiff(names(sf_obj), geom_col)
  df <- sf::st_drop_geometry(sf_obj)

  if (length(attr_cols) == 0) {
    out <- sf_obj[, geom_col, drop = FALSE]
    out$Name <- as.character(seq_len(nrow(sf_obj)))
    out$Description <- ""
    return(out[, c("Name", "Description", geom_col)])
  }

  fmt_val <- function(x) {
    # NA must be tested on the ORIGINAL vector: format() renders NA as the
    # literal string "NA", which is not itself missing.
    na <- is.na(x)
    out <- if (is.numeric(x)) format(x, trim = TRUE, digits = 15, scientific = FALSE) else as.character(x)
    ifelse(na, "", trimws(out))
  }
  parts <- lapply(attr_cols, function(cn) paste0(cn, ": ", fmt_val(df[[cn]])))
  desc <- do.call(paste, c(parts, list(sep = "; ")))

  nm <- if (!is.null(name_field) && name_field %in% attr_cols) {
    fmt_val(df[[name_field]])
  } else {
    fmt_val(df[[attr_cols[1]]])
  }

  out <- sf_obj[, geom_col, drop = FALSE]
  out$Name <- nm
  out$Description <- desc
  out[, c("Name", "Description", geom_col)]
}

#' Write an sf layer in one of the four offered GIS formats.
#'
#' Shared by both vector exports (drawn polygons and map class zones) so the
#' format list cannot drift between them. KML and GeoJSON are WGS84 formats by
#' specification and the layer is reprojected for them; Shapefile and
#' GeoPackage keep the analysis CRS, which is the projected metric one for
#' class zones, so a GIS measures them in the same projection the app reported.
write_vector_export <- function(sf_obj, file, fmt, layer_name = "layer") {
  if (is.null(sf_obj) || nrow(sf_obj) == 0) stop("Nothing to export.")

  if (fmt %in% c("kml", "geojson")) {
    crs_in <- sf::st_crs(sf_obj)
    if (!is.na(crs_in) && crs_in != sf::st_crs(4326)) {
      sf_obj <- sf::st_transform(sf_obj, 4326)
    }
  }

  if (fmt == "shp") {
    # A shapefile is a set of sibling files, so it travels as a zip.
    temp_dir <- file.path(tempdir(), paste0("vec_export_", as.integer(Sys.time())))
    dir.create(temp_dir, showWarnings = FALSE)
    on.exit(unlink(temp_dir, recursive = TRUE), add = TRUE)
    sf::st_write(sf_obj, file.path(temp_dir, paste0(layer_name, ".shp")),
                 driver = "ESRI Shapefile", quiet = TRUE, delete_layer = TRUE)
    zip::zip(zipfile = file, files = list.files(temp_dir), root = temp_dir)
  } else if (fmt == "geojson") {
    sf::st_write(sf_obj, file, driver = "GeoJSON", quiet = TRUE, delete_dsn = TRUE)
  } else if (fmt == "kml") {
    # "class" is the class-zone label; drawn polygons have no such column and
    # fall back to the first attribute.
    kml_sf <- kml_attribute_fields(sf_obj, name_field = "class")
    sf::st_write(kml_sf, file, driver = "KML", quiet = TRUE, delete_dsn = TRUE,
                 dataset_options = c("NameField=Name", "DescriptionField=Description"))
  } else if (fmt == "gpkg") {
    sf::st_write(sf_obj, file, driver = "GPKG", layer = layer_name, quiet = TRUE, delete_dsn = TRUE)
  } else {
    stop("Unsupported vector export format: ", fmt)
  }
  invisible(file)
}

# robust_vgm_fit and clean_gstat_env live in spatial_helpers.R (they are model
# code and must be resolvable by workers that source only that file).
