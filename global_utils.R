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

#' Evaluate a plot write with showtext's assumed resolution matched to the device.
#'
#' `showtext_auto()` (global.R) routes every glyph through showtext, which sizes
#' text against its OWN dpi option (96) instead of the resolution of the device
#' being drawn on. On a 300-dpi export that renders every point size at 96/300 of
#' the value the theme asked for, and raising the DPI shrinks the text further.
#' Setting the option to the device's resolution for the duration of a write
#' makes a point mean a point on the page, at any DPI.
#'
#' @param dpi Resolution of the device `expr` draws on. Vector devices (pdf, svg)
#'   are defined in points, so they take 72.
#' @param expr Write to perform; evaluated once, with the option in force.
with_showtext_dpi <- function(dpi, expr) {
  if (!requireNamespace("showtext", quietly = TRUE)) return(force(expr))
  old <- showtext::showtext_opts(dpi = dpi)
  on.exit(showtext::showtext_opts(old), add = TRUE)
  force(expr)
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


#' Metres per linear axis unit of a PROJECTED CRS.
#'
#' Monolith states every length in metres: the resolution slider, the buffer
#' distance, the nearest-neighbour spacing rule and the ruler's projected column
#' all label their numbers "m", while the engines operate on the CRS's own axis
#' units. That identity holds only while the Target Mapping CRS is metric, and
#' nothing in a CRS string forces it to be - a State Plane zone in US survey
#' feet would turn a "50 m" grid into 15 m, a "250 m" buffer into 76 m and a
#' variogram range into a number 3.28x its stated size, all without an error.
#'
#' Returns NA when the question does not apply or cannot be answered: a
#' geographic CRS (no linear axis unit), an unparseable CRS, or a projected CRS
#' whose unit udunits cannot resolve. Callers treat NA as "do not block", so an
#' exotic but valid CRS is never refused merely for being unrecognised.
crs_metre_factor <- function(crs) {
  co <- tryCatch(sf::st_crs(crs), error = function(e) NULL)
  if (is.null(co) || is.na(co) || isTRUE(sf::st_is_longlat(co))) return(NA_real_)
  f <- tryCatch(as.numeric(units::set_units(co$ud_unit, "m")), error = function(e) NA_real_)
  if (length(f) == 1 && is.finite(f) && f > 0) f else NA_real_
}

#' @param require_metric Enforce the metric-axis rule above. Reserved for the
#'   Target Mapping CRS: the Input Data CRS may use any unit, because the
#'   pipeline projects out of it before measuring anything.
validate_crs <- function(crs_selection, error_prefix = "Invalid CRS provided", duration = NULL,
                         require_metric = FALSE) {
  tryCatch({
    c_obj <- sf::st_crs(crs_selection)
    if (is.na(c_obj)) stop("Invalid CRS format.")

    t_obj <- terra::crs(crs_selection)
    if (t_obj == "") stop("Invalid CRS for terra.")

    # Only a POSITIVELY non-metric unit blocks. A geographic CRS passes (the
    # pipeline projects it to a metric UTM zone itself) and so does one whose
    # unit could not be resolved.
    if (isTRUE(require_metric)) {
      f <- crs_metre_factor(c_obj)
      if (!is.na(f) && abs(f - 1) > 1e-9) {
        stop(sprintf("axis unit is '%s', not metres. Resolution, buffer distance, variogram ranges and on-map measurements are all expressed in metres, so this CRS would report every distance %.4gx its true size. Choose a metric projected CRS for the area (e.g. its UTM zone).",
                     as.character(c_obj$units), 1 / f))
      }
    }

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

#' Measure a path drawn with the Map Viewer's ruler.
#'
#' The Leaflet measure control computes on a SPHERE (its own calc module, radius
#' 6371000 m), which is up to ~0.5% off the ellipsoid in length, and it knows
#' nothing about the coordinate system the models run in. Both numbers this app
#' owes the user are therefore recomputed here from the clicked WGS84 vertices:
#'
#'   - the GEODESIC length on the WGS84 ellipsoid, via terra (GeographicLib):
#'     the ground distance, independent of any projection;
#'   - the PLANAR length in the Target Mapping CRS: the metric every engine
#'     actually works in (variogram lags, IDW separation distances, TPS
#'     coordinates, grid resolution, buffer radii), so it is the number to
#'     compare against a variogram range or a cell size.
#'
#' The two differ by the projection's distance distortion, small at field scale
#' (~0.003% over 2.5 km in UTM) and worth showing rather than hiding: a wide gap
#' means the selected CRS is a poor fit for the area being measured. A
#' GEOGRAPHIC Target Mapping CRS yields no planar figure at all - a length in
#' degrees is meaningless, the same rule `validate_and_project_sf()` enforces
#' before interpolation.
#'
#' Area is reported on the same two bases once the path has three vertices,
#' which is where the measure control itself switches to area. `terra::expanse`
#' is the ellipsoidal area, the same call the class-zone export uses. From that
#' third vertex on the length CLOSES with the shape and becomes a perimeter, so
#' the two figures describe one and the same ring: an open path reported beside
#' the area of a closed one differs from it by the closing leg and invites the
#' reader to add a boundary that was never measured.
#'
#' A ring that crosses itself has no area worth printing: GEOS and terra both
#' integrate around the ring in traversal order (the planar shoelace sum and its
#' geodesic counterpart), so a figure-eight's oppositely-traversed lobes return
#' their DIFFERENCE, not the area drawn on the screen - a symmetric bowtie
#' evaluates to zero. The perimeter is unaffected by the crossing and stays
#' exact, so it is still reported; the area is withheld rather than invented.
#'
#' @param lonlat Two-column matrix of clicked vertices, longitude then latitude,
#'   in WGS84.
#' @param proj_crs Target Mapping CRS (anything `sf::st_crs` accepts). NULL or a
#'   geographic CRS leaves the projected fields NA.
#' @return List with `n_points`, `closed` (TRUE once the shape is a ring, which
#'   makes the lengths perimeters), `self_intersecting`, `length_geodesic`,
#'   `length_projected` (metres), `area_geodesic`, `area_projected` (square
#'   metres) and `crs_label`. Un-measurable quantities are NA, never 0.
measure_path_metrics <- function(lonlat, proj_crs = NULL) {
  out <- list(n_points = 0L, closed = FALSE, self_intersecting = FALSE,
              length_geodesic = NA_real_, length_projected = NA_real_,
              area_geodesic = NA_real_, area_projected = NA_real_,
              crs_label = NA_character_)
  if (is.null(lonlat)) return(out)
  lonlat <- suppressWarnings(matrix(as.numeric(as.matrix(lonlat)), ncol = 2))
  lonlat <- lonlat[stats::complete.cases(lonlat), , drop = FALSE]
  # A vertex placed on top of its predecessor contributes nothing to the path
  # and would hand terra a zero-length segment; dropping it keeps the vertex
  # count honest as well.
  if (nrow(lonlat) > 1) {
    lonlat <- lonlat[c(TRUE, rowSums(abs(diff(lonlat))) > 0), , drop = FALSE]
  }
  # An already-closed ring is counted by its corners: the repeated first vertex
  # is the closure this function applies itself below, not a place the user
  # clicked, and leaving it in would overstate the count by one.
  if (nrow(lonlat) > 2 && all(lonlat[1, ] == lonlat[nrow(lonlat), ])) {
    lonlat <- lonlat[-nrow(lonlat), , drop = FALSE]
  }
  out$n_points <- nrow(lonlat)
  if (out$n_points < 2) return(out)

  out$closed <- out$n_points >= 3
  out$self_intersecting <- out$closed && ring_self_intersects(lonlat)
  path <- if (out$closed) rbind(lonlat, lonlat[1, ]) else lonlat
  report_area <- out$closed && !out$self_intersecting

  out$length_geodesic <- tryCatch(
    sum(terra::perim(terra::vect(path, type = "lines", crs = "EPSG:4326"))),
    error = function(e) NA_real_
  )
  if (report_area) {
    out$area_geodesic <- tryCatch(
      sum(terra::expanse(terra::vect(path, type = "polygons", crs = "EPSG:4326"),
                         unit = "m")),
      error = function(e) NA_real_
    )
  }

  crs_obj <- if (is.null(proj_crs) || identical(proj_crs, "")) NULL else {
    tryCatch(sf::st_crs(proj_crs), error = function(e) NULL)
  }
  if (is.null(crs_obj) || is.na(crs_obj) || isTRUE(sf::st_is_longlat(crs_obj))) return(out)

  lab <- crs_obj$input
  if (is.null(lab) || is.na(lab) || nchar(lab) > 30) lab <- crs_obj$Name
  out$crs_label <- if (is.null(lab) || is.na(lab)) "projected CRS" else lab

  out$length_projected <- tryCatch(
    as.numeric(sf::st_length(sf::st_transform(
      sf::st_sfc(sf::st_linestring(path), crs = 4326), crs_obj))),
    error = function(e) NA_real_
  )
  if (report_area) {
    out$area_projected <- tryCatch(
      as.numeric(sf::st_area(sf::st_transform(
        sf::st_sfc(sf::st_polygon(list(path)), crs = 4326), crs_obj))),
      error = function(e) NA_real_
    )
  }
  out
}

#' Does a closed ring cross itself?
#'
#' Tested combinatorially on the clicked longitude/latitude pairs rather than
#' through `st_is_valid()`: over a measurement-sized shape the planar and the
#' projected topologies are identical, and this stays a plain predicate whether
#' or not a Target Mapping CRS is set and whichever way `sf_use_s2()` happens to
#' be switched. Only PROPER crossings count (edges passing through each other,
#' strict sign change on both orientation tests); a ring whose edges merely
#' touch at a vertex still has the area its shoelace sum reports.
#'
#' @param xy Two-column matrix of the ring's corners, WITHOUT the repeated
#'   closing vertex; edges wrap from the last corner back to the first.
ring_self_intersects <- function(xy) {
  n <- nrow(xy)
  if (is.null(n) || n < 4) return(FALSE)  # a triangle cannot cross itself
  nxt <- function(i) if (i == n) 1L else i + 1L
  side <- function(o, a, b) (a[1] - o[1]) * (b[2] - o[2]) - (a[2] - o[2]) * (b[1] - o[1])
  for (i in seq_len(n - 1L)) {
    for (j in seq(i + 1L, n)) {
      # Consecutive edges share a vertex by construction, as do the first and
      # the last once the ring wraps; neither is a self-intersection.
      if (j == i + 1L || (i == 1L && j == n)) next
      p1 <- xy[i, ]; p2 <- xy[nxt(i), ]
      q1 <- xy[j, ]; q2 <- xy[nxt(j), ]
      d1 <- side(p1, p2, q1); d2 <- side(p1, p2, q2)
      d3 <- side(q1, q2, p1); d4 <- side(q1, q2, p2)
      if (d1 * d2 < 0 && d3 * d4 < 0) return(TRUE)
    }
  }
  FALSE
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
