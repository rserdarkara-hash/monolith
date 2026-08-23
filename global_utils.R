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

# ── Input-CRS identification ───────────────────────────────────────────────
# Bare projected coordinates do NOT carry their own CRS: UTM zones are
# mathematically congruent, so one easting/northing pair is equally valid in
# all 60 zones and reproduces the same latitude with a longitude shifted 6
# degrees per zone. Monolith therefore identifies the input CRS only when the
# file carries the evidence to prove it, and never assumes a zone.

#' Normalise a free-typed CRS entry.
#'
#' Both CRS selectors accept typed input, but `sf::st_crs("32633")` errors - a
#' bare EPSG number is rejected while `EPSG:32633` parses. Promote a bare 4-6
#' digit code to `EPSG:<n>`; leave everything else (PROJ strings, WKT,
#' already-prefixed codes, unparseable text) untouched for the caller's own
#' validation to judge.
normalize_crs_input <- function(x) {
  if (is.null(x) || length(x) != 1) return(x)
  if (is.numeric(x) && is.finite(x)) return(paste0("EPSG:", format(x, scientific = FALSE)))
  if (!is.character(x) || is.na(x)) return(x)
  s <- trimws(x)
  if (grepl("^[0-9]{4,6}$", s)) paste0("EPSG:", s) else x
}

#' Format a WGS84 position for a human: "12.958°E, 52.466°N".
format_lonlat <- function(lon, lat, digits = 3) {
  sprintf("%.*f°%s, %.*f°%s",
          digits, abs(lon), if (lon >= 0) "E" else "W",
          digits, abs(lat), if (lat >= 0) "N" else "S")
}

# Column headings that positively identify a geographic coordinate pair. Only
# whole-name matches count, for the same reason `is_coord_col()` uses whole
# names: "Longevity_index" is a variable, not a longitude.
.crs_lon_names <- c("lon", "long", "lng", "longitude")
.crs_lat_names <- c("lat", "latitude")

#' Locate a companion lon/lat pair in the uploaded table.
#'
#' @return list(lon_col, lat_col, lon, lat), or NULL when no plausible pair
#'   exists (no such heading, non-numeric, or magnitudes outside +/-180 / +/-90).
find_geographic_pair <- function(df, exclude = character(0)) {
  cn <- colnames(df)
  nm <- tolower(trimws(cn))
  lon_i <- which(nm %in% .crs_lon_names & !(cn %in% exclude))
  lat_i <- which(nm %in% .crs_lat_names & !(cn %in% exclude))
  if (length(lon_i) == 0 || length(lat_i) == 0) return(NULL)
  lon <- suppressWarnings(as.numeric(as.character(df[[lon_i[1]]])))
  lat <- suppressWarnings(as.numeric(as.character(df[[lat_i[1]]])))
  ok <- is.finite(lon) & is.finite(lat)
  if (sum(ok) < 1) return(NULL)
  if (any(abs(lon[ok]) > 180) || any(abs(lat[ok]) > 90)) return(NULL)
  list(lon_col = cn[lon_i[1]], lat_col = cn[lat_i[1]], lon = lon, lat = lat)
}

#' Candidate EPSG codes for a longitude: the WGS84 UTM zone (both
#' hemispheres), its ETRS89 twin and Web Mercator. Four codes, not 180 - a
#' longitude fixes the zone exactly, so everything left is a datum question.
crs_zone_candidates <- function(lon) {
  zone <- min(max(floor((lon + 180) / 6) + 1, 1), 60)
  c(32600 + zone, 32700 + zone, 25800 + zone, 3857)
}

#' Collapse candidates that put the data in the same place.
#'
#' A WGS84 UTM zone and its ETRS89 twin are the same grid to under a metre, so
#' they score identically and would defeat every discrimination test by tying
#' with each other. Group candidates by the position they produce, keep the
#' first of each group (candidate order encodes preference) and report the rest
#' as equivalent.
#'
#' @param tol_m Two candidates belong to one group when their positions are
#'   within this distance. Tier 2A keeps the default 5 m: it asks whether a
#'   candidate is PROVEN by evidence in the file, so only genuinely identical
#'   grids may tie there. The Tier-3 shortlist passes a much wider tolerance -
#'   see crs_candidate_shortlist().
#' @param pos Optional data.frame(lon, lat) holding each candidate's already
#'   computed landing position, so a caller that has transformed the data once
#'   need not pay for a second pass. It must be what crs_landing_position()
#'   produces (the mean point read in each candidate CRS), or the grouping is
#'   not the same grouping.
#' @return list(cands, equivalent, spread_m); `spread_m` is how far the
#'   furthest member of a group sits from its representative, so a caller can
#'   state the size of the difference it is folding away.
crs_collapse_candidates <- function(x, y, cands, tol_m = 5, pos = NULL) {
  as_pt <- function(lon, lat) {
    if (!isTRUE(is.finite(lon)) || !isTRUE(is.finite(lat))) return(NULL)
    sf::st_as_sf(data.frame(.x = lon, .y = lat), coords = c(".x", ".y"), crs = 4326)
  }
  cen <- if (!is.null(pos)) {
    lapply(seq_along(cands), function(i) as_pt(pos$lon[i], pos$lat[i]))
  } else {
    lapply(cands, function(e) suppressWarnings(tryCatch({
      src <- sf::st_crs(e)
      if (is.na(src)) return(NULL)
      sf::st_transform(sf::st_as_sf(data.frame(.x = mean(x), .y = mean(y)),
                                    coords = c(".x", ".y"), crs = src), 4326)
    }, error = function(e) NULL)))
  }
  keep <- integer(0)
  same <- vector("list", length(cands))
  spread <- numeric(length(cands))
  for (i in seq_along(cands)) {
    if (is.null(cen[[i]])) next
    hit <- NA_integer_; hit_d <- NA_real_
    for (k in keep) {
      d <- suppressWarnings(tryCatch(as.numeric(sf::st_distance(cen[[i]], cen[[k]])),
                                     error = function(e) NA_real_))
      if (isTRUE(d <= tol_m)) { hit <- k; hit_d <- d; break }
    }
    if (is.na(hit)) {
      keep <- c(keep, i)
    } else {
      same[[hit]] <- c(same[[hit]], cands[i])
      spread[hit] <- max(spread[hit], hit_d, na.rm = TRUE)
    }
  }
  list(cands = cands[keep], equivalent = same[keep], spread_m = spread[keep])
}

#' Largest ground distance, in metres, between (x, y) read as `epsg` and the
#' reference lon/lat. NA when the candidate cannot be parsed or transformed.
crs_candidate_residual <- function(x, y, lon, lat, epsg) {
  suppressWarnings(tryCatch({
    src <- sf::st_crs(epsg)
    if (is.na(src)) return(NA_real_)
    p <- sf::st_transform(
      sf::st_as_sf(data.frame(.x = x, .y = y), coords = c(".x", ".y"), crs = src), 4326)
    ref <- sf::st_as_sf(data.frame(.x = lon, .y = lat), coords = c(".x", ".y"), crs = 4326)
    d <- as.numeric(sf::st_distance(p, ref, by_element = TRUE))
    if (all(is.finite(d))) max(d) else NA_real_
  }, error = function(e) NA_real_))
}

#' Tier 2A - identify the input CRS from a companion lon/lat pair.
#'
#' Accepts only when one candidate reproduces the geographic pair to within
#' `tol_m` AND every candidate that does not is at least `ratio` times worse.
#' Datum siblings that both agree to within `tol_m` (WGS84 vs ETRS89 UTM: the
#' same grid) are not a contradiction - the first in candidate order wins and
#' the rest are reported as equivalent.
#'
#' @return list(crs, epsg, residual, runner_up, equivalent, evidence) or NULL.
identify_crs_from_lonlat <- function(x, y, lon, lat, tol_m = 5, ratio = 100) {
  x <- suppressWarnings(as.numeric(x)); y <- suppressWarnings(as.numeric(y))
  ok <- is.finite(x) & is.finite(y) & is.finite(lon) & is.finite(lat)
  if (sum(ok) < 1) return(NULL)
  x <- x[ok]; y <- y[ok]; lon <- lon[ok]; lat <- lat[ok]

  grp <- crs_collapse_candidates(x, y, crs_zone_candidates(mean(lon)), tol_m)
  cands <- grp$cands
  res <- vapply(cands, function(e) crs_candidate_residual(x, y, lon, lat, e), numeric(1))
  keep <- is.finite(res)
  if (!any(keep)) return(NULL)
  cands <- cands[keep]; res <- res[keep]; equiv <- grp$equivalent[keep]

  o <- order(res)
  best <- res[o[1]]
  runner <- if (length(o) > 1) res[o[2]] else Inf
  # A lone survivor proves nothing: require a rival that is decisively worse.
  if (!(best <= tol_m && is.finite(runner) && best < runner / ratio)) return(NULL)

  list(crs = paste0("EPSG:", cands[o[1]]), epsg = cands[o[1]],
       residual = best, runner_up = runner,
       equivalent = equiv[[o[1]]], evidence = "lonlat")
}

#' Tier 2B - identify the input CRS from an uploaded boundary shapefile.
#'
#' Read under the right CRS the points drop inside (or beside) the boundary; a
#' wrong zone throws them hundreds of kilometres away, so the fraction
#' contained is decisive. The boundary's own CRS is tested first, which also
#' covers national grids the zone family does not contain.
#'
#' @return list(crs, epsg, fraction, evidence) or NULL.
identify_crs_from_boundary <- function(x, y, boundary, min_frac = 0.5) {
  if (is.null(boundary)) return(NULL)
  geom <- suppressWarnings(tryCatch({
    g <- sf::st_union(sf::st_geometry(boundary))
    if (is.na(sf::st_crs(g))) return(NULL)
    sf::st_transform(g, 4326)
  }, error = function(e) NULL))
  if (is.null(geom) || length(geom) == 0) return(NULL)

  # Points legitimately sit slightly outside a study boundary, so allow a
  # margin of 5% of the boundary's own diagonal before counting a miss.
  bb <- sf::st_bbox(geom)
  cen_lon <- mean(c(bb[["xmin"]], bb[["xmax"]]))
  pad <- 0.05 * sqrt((bb[["xmax"]] - bb[["xmin"]])^2 + (bb[["ymax"]] - bb[["ymin"]])^2)
  target <- suppressWarnings(tryCatch(sf::st_buffer(geom, pad), error = function(e) geom))

  x <- suppressWarnings(as.numeric(x)); y <- suppressWarnings(as.numeric(y))
  ok <- is.finite(x) & is.finite(y)
  if (sum(ok) < 1) return(NULL)
  x <- x[ok]; y <- y[ok]

  b_epsg <- sf::st_crs(boundary)$epsg
  grp <- crs_collapse_candidates(x, y, unique(c(
    if (!is.null(b_epsg) && !is.na(b_epsg)) b_epsg, crs_zone_candidates(cen_lon))))
  cands <- grp$cands
  frac <- vapply(cands, function(e) suppressWarnings(tryCatch({
    src <- sf::st_crs(e)
    if (is.na(src)) return(NA_real_)
    p <- sf::st_transform(
      sf::st_as_sf(data.frame(.x = x, .y = y), coords = c(".x", ".y"), crs = src), 4326)
    mean(lengths(sf::st_intersects(p, target)) > 0)
  }, error = function(e) NA_real_)), numeric(1))

  keep <- is.finite(frac)
  if (!any(keep)) return(NULL)
  cands <- cands[keep]; frac <- frac[keep]; equiv <- grp$equivalent[keep]
  o <- order(frac, decreasing = TRUE)
  if (frac[o[1]] < min_frac) return(NULL)
  if (length(o) > 1 && frac[o[2]] > frac[o[1]] / 2) return(NULL)
  list(crs = paste0("EPSG:", cands[o[1]]), epsg = cands[o[1]],
       fraction = frac[o[1]], equivalent = equiv[[o[1]]], evidence = "boundary")
}

#' Identify the input CRS of a mapped X/Y pair, with a message naming the
#' evidence. Tier 1 (degrees) -> EPSG:4326; Tier 2A (companion lon/lat) and
#' Tier 2B (boundary .prj) -> the proven code; otherwise NULL, i.e. no guess.
identify_input_crs <- function(df, x_col, y_col, boundary = NULL) {
  if (is.null(df) || !all(c(x_col, y_col) %in% colnames(df))) return(NULL)
  x <- suppressWarnings(as.numeric(as.character(df[[x_col]])))
  y <- suppressWarnings(as.numeric(as.character(df[[y_col]])))
  ok <- is.finite(x) & is.finite(y)
  if (sum(ok) < 1) return(NULL)

  if (all(abs(x[ok]) <= 180) && all(abs(y[ok]) <= 90)) {
    return(list(crs = "EPSG:4326", epsg = 4326, evidence = "degrees",
                message = "Input CRS set to EPSG:4326 (WGS 84): the mapped coordinates are degrees."))
  }

  pair <- find_geographic_pair(df, exclude = c(x_col, y_col))
  if (!is.null(pair)) {
    hit <- identify_crs_from_lonlat(x, y, pair$lon, pair$lat)
    if (!is.null(hit)) {
      msg <- sprintf(
        "Input CRS identified as %s from the '%s'/'%s' columns - agreement %.3g m, next candidate %.3g m off.",
        hit$crs, pair$lon_col, pair$lat_col, hit$residual, hit$runner_up)
      if (length(hit$equivalent)) {
        msg <- paste0(msg, " EPSG:", paste(hit$equivalent, collapse = "/"),
                      " describes the same grid to within that tolerance.")
      }
      hit$message <- msg
      return(hit)
    }
  }

  hit <- identify_crs_from_boundary(x, y, boundary)
  if (!is.null(hit)) {
    hit$message <- sprintf(
      "Input CRS identified as %s from the uploaded boundary shapefile - %.0f%% of the points fall inside it under this CRS, and no other candidate comes close.",
      hit$crs, 100 * hit$fraction)
    return(hit)
  }
  NULL
}

#' Where the mapped coordinates currently land, in degrees, under `crs`.
#'
#' The single readout that makes a wrong zone obvious: a soil scientist working
#' at Potsdam recognises 13°E instantly, and 24.958°E as nonsense. Returns NULL
#' when the position cannot be computed.
crs_landing_position <- function(df, x_col, y_col, crs) {
  if (is.null(crs) || !nzchar(as.character(crs)[1])) return(NULL)
  if (is.null(df) || is.null(x_col) || is.null(y_col)) return(NULL)
  if (!all(c(x_col, y_col) %in% colnames(df))) return(NULL)
  x <- suppressWarnings(as.numeric(as.character(df[[x_col]])))
  y <- suppressWarnings(as.numeric(as.character(df[[y_col]])))
  ok <- is.finite(x) & is.finite(y)
  if (sum(ok) < 1) return(NULL)
  suppressWarnings(tryCatch({
    co <- sf::st_crs(crs)
    if (is.na(co)) return(NULL)
    p <- sf::st_transform(
      sf::st_as_sf(data.frame(.x = mean(x[ok]), .y = mean(y[ok])),
                   coords = c(".x", ".y"), crs = co), 4326)
    xy <- sf::st_coordinates(p)
    if (!all(is.finite(xy))) return(NULL)
    lon <- unname(xy[1, 1]); lat <- unname(xy[1, 2])
    list(lon = lon, lat = lat, text = format_lonlat(lon, lat))
  }, error = function(e) NULL))
}

# ── EPSG registry: candidates by declared area of use ──────────────────────
# Tier 3. With no evidence in the file the zone cannot be recovered, but the
# question can be turned around: instead of asking the user for an EPSG code,
# ask WHERE the data is and report which projections put it there.
#
# The catalogue is already on disk. sf ships PROJ's own SQLite database
# (proj.db), whose projected_crs / usage / extent tables carry every EPSG
# projected CRS together with the lat/lon box it declares as its area of use -
# 5,330 non-deprecated codes, worldwide, read in about 0.3 s and cached for
# the session. No curated national-grid list, no region assumptions.
#
# Order matters for speed. Transforming all 5,330 candidates takes over two
# minutes, so the containment test is applied FIRST, as plain arithmetic on
# the query result (0 ms, and anywhere on earth it leaves 15-50 candidates);
# only the survivors are transformed.
#
# Every entry point returns NULL instead of erroring. A build without proj.db
# or without RSQLite falls back to crs_zone_candidates(), so Tier 3 degrades
# to the four-code zone family rather than breaking the upload.

.crs_registry_cache <- new.env(parent = emptyenv())

.crs_registry_sql <- "
SELECT pc.code AS code, pc.name AS name, e.name AS area, e.description AS descr,
       e.west_lon AS west, e.east_lon AS east,
       e.south_lat AS south, e.north_lat AS north
  FROM projected_crs pc
  JOIN usage u ON u.object_table_name = 'projected_crs'
              AND u.object_auth_name  = pc.auth_name
              AND u.object_code       = pc.code
  JOIN extent e ON e.auth_name = u.extent_auth_name
               AND e.code      = u.extent_code
 WHERE pc.auth_name = 'EPSG'
   AND pc.deprecated = 0
   AND e.deprecated = 0
   AND e.west_lon IS NOT NULL AND e.east_lon IS NOT NULL
   AND e.south_lat IS NOT NULL AND e.north_lat IS NOT NULL"

#' Locate PROJ's proj.db. NULL when the build does not ship one.
crs_registry_path <- function() {
  paths <- tryCatch(sf::sf_proj_search_paths(), error = function(e) character(0))
  pl <- Sys.getenv("PROJ_LIB")
  if (nzchar(pl)) paths <- c(paths, pl)
  db <- file.path(paths, "proj.db")
  db <- db[file.exists(db)]
  if (length(db)) db[1] else NULL
}

#' The EPSG projected-CRS catalogue with its declared areas of use.
#'
#' @return data.frame(code, name, area, descr, west, east, south, north), one
#'   row per declared extent (a few codes declare more than one). `area` is the
#'   short label of the area of use and `descr` its full text, which lists the
#'   countries the CRS covers - that is what the free-text filter searches.
#'   NULL when the
#'   catalogue cannot be read. Cached for the session; a failure is cached too,
#'   so a missing proj.db costs one attempt rather than one per upload.
crs_registry <- function(refresh = FALSE) {
  if (!refresh) {
    if (!is.null(.crs_registry_cache$reg)) return(.crs_registry_cache$reg)
    if (isTRUE(.crs_registry_cache$failed)) return(NULL)
  }
  reg <- tryCatch({
    if (!requireNamespace("DBI", quietly = TRUE) ||
        !requireNamespace("RSQLite", quietly = TRUE)) stop("DBI/RSQLite unavailable")
    db <- crs_registry_path()
    if (is.null(db)) stop("proj.db not found on the PROJ search path")
    con <- DBI::dbConnect(RSQLite::SQLite(), db, flags = RSQLite::SQLITE_RO)
    on.exit(DBI::dbDisconnect(con), add = TRUE)
    d <- DBI::dbGetQuery(con, .crs_registry_sql)
    if (!is.data.frame(d) || nrow(d) == 0) stop("empty registry")
    d$code <- suppressWarnings(as.integer(d$code))
    d <- d[is.finite(d$code), , drop = FALSE]
    for (nm in c("west", "east", "south", "north")) d[[nm]] <- as.numeric(d[[nm]])
    d[stats::complete.cases(d[, c("west", "east", "south", "north")]), , drop = FALSE]
  }, error = function(e) NULL)
  if (is.null(reg)) .crs_registry_cache$failed <- TRUE else .crs_registry_cache$reg <- reg
  reg
}

#' Does a declared area of use contain a point?
#'
#' The one containment test in the app. Vectorised over both sides, so it
#' serves the whole-catalogue filter (many extents, one point), the
#' self-consistency check (one extent per row, one point per row) and the
#' suitability gate (one extent, several sample points) without a second copy.
#'
#' `west > east` marks an extent that CROSSES THE ANTIMERIDIAN - a wrapped
#' range, not an empty one - so longitude is tested as a union there and as an
#' interval everywhere else. Returns NA for an extent with missing bounds;
#' callers use which() and therefore treat NA as "no answer", never as a hit.
crs_extent_contains <- function(west, east, south, north, lon, lat, pad = 0) {
  w <- west - pad; e <- east + pad
  # NOT ifelse(): it returns a result the length of its TEST, so a single
  # extent tested against several points would collapse to the first point.
  # Written as a boolean combination, every shape recycles correctly and NA
  # bounds still answer NA.
  wrapped <- west > east
  in_lon <- (!wrapped & lon >= w & lon <= e) | (wrapped & (lon >= w | lon <= e))
  in_lon & lat >= south - pad & lat <= north + pad
}

#' Primary Tier-3 filter: every CRS whose declared area of use contains the
#' point the user indicated. Pure arithmetic - no transform, no sf call - so it
#' runs over the whole catalogue instantly. `west > east` marks an extent that
#' crosses the antimeridian.
crs_registry_filter_extent <- function(reg, lon, lat, pad = 0) {
  if (is.null(reg) || !nrow(reg)) return(NULL)
  if (!isTRUE(is.finite(lon)) || !isTRUE(is.finite(lat))) return(NULL)
  hit <- reg[which(crs_extent_contains(reg$west, reg$east, reg$south, reg$north,
                                      lon, lat, pad)), , drop = FALSE]
  hit[!duplicated(hit$code), , drop = FALSE]
}

#' Secondary Tier-3 filter: free text against the CRS name and the full text of
#' its area of use, which lists the countries covered ("Germany", "Kansas",
#' "zone 33N").
#'
#' Text does NOT bound the search the way a point does - "United States" alone
#' matches well over a thousand codes, and transforming that many costs tens of
#' seconds - so the result is capped at `limit` and flagged with a `truncated`
#' attribute. Narrow the text, or intersect it with an extent, for a usable
#' shortlist.
crs_registry_filter_area <- function(reg, text, limit = 120) {
  if (is.null(reg) || !nrow(reg)) return(NULL)
  if (is.null(text) || !nzchar(trimws(text))) return(NULL)
  pat <- tolower(trimws(text))
  hit <- reg[grepl(pat, tolower(reg$descr), fixed = TRUE) |
               grepl(pat, tolower(reg$name), fixed = TRUE), , drop = FALSE]
  hit <- hit[!duplicated(hit$code), , drop = FALSE]
  if (!nrow(hit)) return(hit)
  hit <- hit[order(hit$code), , drop = FALSE]
  trunc <- nrow(hit) > limit
  if (trunc) hit <- hit[seq_len(limit), , drop = FALSE]
  attr(hit, "truncated") <- trunc
  hit
}

#' Preference order within a group of CRS that put the data in the same place.
#'
#' crs_collapse_candidates() keeps the FIRST member of each group as the row's
#' representative, so this ordering decides what the user is shown. WGS 84
#' first, then ETRS89, then the rest: the datum siblings agree to well under a
#' metre here and are all listed on the row, but "WGS 84 / UTM zone 33N" is the
#' name a user recognises. The sort is stable, so within a rank the caller's
#' own order stands - which is how the crs_zone_candidates() fallback keeps its
#' deliberate ordering when no registry names are available.
.crs_preference_order <- function(names) {
  names[is.na(names)] <- ""
  order(ifelse(grepl("^WGS 84 /", names), 1L,
               ifelse(grepl("^ETRS89 /", names), 2L, 3L)))
}

#' Tier 3 - the candidate shortlist.
#'
#' @param x,y The mapped coordinate columns, in their own (unknown) CRS.
#' @param lon,lat Where the user says the data is (a click on the world
#'   mini-map). Primary path.
#' @param area_text Country or region text. Secondary path, used when no click
#'   is supplied.
#' @param limit Rows returned after collapsing same-place candidates.
#' @param max_km Reject a candidate landing further than this from the
#'   indicated point. The click is rough by nature, so the tolerance is wide;
#'   the neighbouring UTM zone misses by 400 km and Web Mercator by far more.
#' @param collapse_m How far apart two candidates may land and still be offered
#'   as ONE place. Wide on purpose: a grid's datum siblings (WGS 84, WGS 72,
#'   WGS 72BE and ED50 UTM zone 33N all read Potsdam coordinates within 210 m
#'   of each other) are separated by far less than a map click can resolve, so
#'   listing them as competing rows ranked by distance to that click lets noise
#'   pick the winner - and an obsolete datum can lead. Folded onto one row they
#'   are ranked by preference instead, with the size of the fold reported. Two
#'   genuinely different answers - adjacent UTM zones - sit ~400 km apart and
#'   are never folded.
#' @return data.frame(epsg, name, area, lon, lat, distance_km, text, spread_m)
#'   with an `equivalent` list column naming the codes folded onto the row,
#'   nearest first; NULL when nothing can be computed, and a zero-row frame
#'   when the catalogue holds no candidate that fits. Two attributes travel
#'   with the frame: `truncated` (the country text matched more codes than
#'   could be searched) and `text_no_match` (the text matched nothing inside
#'   the area the click had already bounded).
crs_candidate_shortlist <- function(x, y, lon = NULL, lat = NULL, area_text = NULL,
                                    limit = 8, max_km = 500, collapse_m = 250) {
  trunc <- FALSE; no_match <- FALSE
  flag <- function(df) {
    attr(df, "truncated") <- trunc
    attr(df, "text_no_match") <- no_match
    df
  }
  x <- suppressWarnings(as.numeric(x)); y <- suppressWarnings(as.numeric(y))
  ok <- is.finite(x) & is.finite(y)
  if (sum(ok) < 1) return(NULL)
  x <- x[ok]; y <- y[ok]

  have_click <- isTRUE(is.finite(lon)) && isTRUE(is.finite(lat))
  reg <- crs_registry()
  pool <- if (have_click) crs_registry_filter_extent(reg, lon, lat) else
    crs_registry_filter_area(reg, area_text)
  # Text alongside a click narrows the pruned pool instead of replacing it,
  # which costs nothing: the point has already bounded the search.
  narrowed <- FALSE
  if (have_click && !is.null(pool) && nrow(pool) &&
      !is.null(area_text) && nzchar(trimws(area_text))) {
    pool <- crs_registry_filter_area(pool, area_text, limit = nrow(pool))
    narrowed <- TRUE
  }
  trunc <- isTRUE(attr(pool, "truncated"))

  if (is.null(pool) || !nrow(pool)) {
    # A text filter that matched nothing must narrow to EMPTY. Falling through
    # to the zone family below would widen the search back out and answer a
    # question the user did not ask.
    if (narrowed) { no_match <- TRUE; return(flag(.crs_shortlist_empty())) }
    # Degraded path: no catalogue at all. A longitude still fixes the UTM zone
    # exactly, which is what Phase A's zone family encodes, so Tier 3 narrows
    # to four codes instead of failing.
    if (!have_click) return(NULL)
    pool <- data.frame(code = crs_zone_candidates(lon), name = NA_character_,
                       area = NA_character_, descr = NA_character_,
                       west = -180, east = 180, south = -90, north = 90)
  }

  fx <- data.frame(.x = x, .y = y)
  land <- lapply(pool$code, function(e) crs_landing_position(fx, ".x", ".y", paste0("EPSG:", e)))
  keep <- !vapply(land, is.null, logical(1))
  pool <- pool[keep, , drop = FALSE]; land <- land[keep]
  if (!nrow(pool)) return(flag(.crs_shortlist_empty()))
  pool$lon <- vapply(land, `[[`, numeric(1), "lon")
  pool$lat <- vapply(land, `[[`, numeric(1), "lat")

  # Self-consistency: a candidate is an answer only if reading the data under
  # it lands inside the area that same candidate declares it covers. This is
  # what prunes the national grids - Potsdam coordinates read as Swiss LV95
  # come out nowhere near Switzerland.
  self <- crs_extent_contains(pool$west, pool$east, pool$south, pool$north,
                              pool$lon, pool$lat)
  pool <- pool[which(self), , drop = FALSE]
  if (!nrow(pool)) return(flag(.crs_shortlist_empty()))

  # Rank by how close the result is to the place the user indicated; with only
  # text to go on, by how centrally it falls in its own declared area.
  ref_lon <- if (have_click) rep(lon, nrow(pool)) else (pool$west + pool$east) / 2
  ref_lat <- if (have_click) rep(lat, nrow(pool)) else (pool$south + pool$north) / 2
  dist_km <- suppressWarnings(tryCatch({
    as.numeric(sf::st_distance(
      sf::st_as_sf(data.frame(a = pool$lon, b = pool$lat), coords = c("a", "b"), crs = 4326),
      sf::st_as_sf(data.frame(a = ref_lon, b = ref_lat), coords = c("a", "b"), crs = 4326),
      by_element = TRUE)) / 1000
  }, error = function(e) rep(NA_real_, nrow(pool))))
  pool$distance_km <- dist_km
  if (have_click && is.finite(max_km)) {
    pool <- pool[which(is.finite(pool$distance_km) & pool$distance_km <= max_km), , drop = FALSE]
    if (!nrow(pool)) return(flag(.crs_shortlist_empty()))
  }
  # Preference decides the representative of each collapsed group, distance
  # decides the order the groups are offered in - and in that order, not the
  # reverse. Members of one group sit metres apart, so sorting the pool by
  # distance first would let sub-metre datum differences pick the representative.
  pool <- pool[.crs_preference_order(pool$name), , drop = FALSE]

  # The landing positions are already computed, so the collapse is handed them
  # rather than transforming the same mean point a second time.
  grp <- crs_collapse_candidates(x, y, pool$code, tol_m = collapse_m,
                                 pos = data.frame(lon = pool$lon, lat = pool$lat))
  idx <- match(grp$cands, pool$code)
  out <- data.frame(
    epsg = pool$code[idx], name = pool$name[idx], area = pool$area[idx],
    lon = pool$lon[idx], lat = pool$lat[idx], distance_km = pool$distance_km[idx],
    spread_m = grp$spread_m,
    stringsAsFactors = FALSE
  )
  out$text <- vapply(seq_len(nrow(out)),
                     function(i) format_lonlat(out$lon[i], out$lat[i]), character(1))
  out$equivalent <- grp$equivalent
  out <- out[order(out$distance_km, na.last = TRUE), , drop = FALSE]
  if (nrow(out) > limit) out <- out[seq_len(limit), , drop = FALSE]
  rownames(out) <- NULL
  flag(out)
}

#' The shortlist's empty answer, with the columns a caller indexes.
.crs_shortlist_empty <- function() {
  out <- data.frame(epsg = integer(0), name = character(0), area = character(0),
                    lon = numeric(0), lat = numeric(0), distance_km = numeric(0),
                    spread_m = numeric(0), text = character(0),
                    stringsAsFactors = FALSE)
  out$equivalent <- list()
  out
}

# -- Target Mapping CRS suitability -----------------------------------------
# The run gate used to ask one question of the Target Mapping CRS: is its axis
# unit the metre? That is necessary and nowhere near sufficient. Web Mercator's
# axis unit IS the metre, and at 52 deg N it reports every distance 64% too
# long; a UTM zone two zones away is metric too, and 1.07% too long. Grid
# resolution, buffer radius, variogram ranges, exported cell size and the
# ruler's projected column are all stated in metres and all read straight off
# this CRS, so a CRS that does not suit the region corrupts every one of them
# while raising no error anywhere.
#
# Two checks, both empirical, both dependency-free:
#   1. the data must fall inside the area of use the CRS itself declares;
#   2. the point scale factor k at the data must be near 1.
# Neither ever blocks on "cannot answer" - an unparseable CRS, one with no
# declared extent, one PROJ refuses to transform into - the same posture
# crs_metre_factor() takes when it returns NA.

#' A CRS's own declared area of use, as c(west, east, south, north) degrees.
#'
#' The inverse of crs_registry_filter_extent(): that maps a point to candidate
#' CRS, this maps a CRS to its extent. Resolved in three steps - PROJ's
#' catalogue by EPSG code (authoritative, and already cached for the session),
#' then the BBOX node of the WKT2 sf returns anyway (which covers geographic
#' and non-EPSG CRS the projected-only registry does not carry), then NULL.
#'
#' WKT2 writes the node as BBOX[south, west, north, east] - latitude first -
#' so EPSG:32633 reads BBOX[0,12,84,18] and means 12 deg E to 18 deg E.
#'
#' EVERY declared extent is returned, not the first. 17 EPSG codes in this
#' catalogue declare more than one - EPSG:2393 (KKJ / Finland Uniform
#' Coordinate System) declares both "Finland - 25.5E to 28.5E onshore" and
#' "Finland - onshore" - and a code's area of use is their UNION, which is the
#' rule crs_registry_filter_extent() already applies when it offers that code
#' as a candidate. Reading one row instead told a Finnish user at 22 deg E that
#' their data landed outside a CRS that covers it.
#'
#' @return data.frame(west, east, south, north), one row per declared extent,
#'   or NULL when the CRS declares none.
crs_area_of_use <- function(crs) {
  if (is.null(crs) || length(crs) != 1) return(NULL)
  co <- suppressWarnings(tryCatch(sf::st_crs(crs), error = function(e) NULL))
  if (is.null(co) || is.na(co)) return(NULL)

  code <- suppressWarnings(as.integer(co$epsg))
  reg <- crs_registry()
  if (!is.null(reg) && length(code) == 1 && !is.na(code)) {
    i <- which(reg$code == code)
    if (length(i)) {
      v <- data.frame(west = reg$west[i], east = reg$east[i],
                      south = reg$south[i], north = reg$north[i])
      v <- v[stats::complete.cases(v), , drop = FALSE]
      if (nrow(v)) { rownames(v) <- NULL; return(v) }
    }
  }

  wkt <- tryCatch(co$wkt, error = function(e) NULL)
  if (is.null(wkt) || !nzchar(wkt)) return(NULL)
  m <- regmatches(wkt, regexpr("BBOX\\[[^]]*\\]", wkt))
  if (!length(m)) return(NULL)
  v <- suppressWarnings(as.numeric(strsplit(gsub("^BBOX\\[|\\]$", "", m[1]), ",")[[1]]))
  if (length(v) != 4 || !all(is.finite(v))) return(NULL)
  data.frame(west = v[2], east = v[4], south = v[1], north = v[3])
}

#' Widest of a set of declared extents, in square degrees of lon x lat, so a
#' message about a CRS carrying several of them names its outer bound.
#' `west > east` is a wrapped range, so its span runs the long way round.
.crs_extent_widest <- function(ext) {
  lon_span <- ifelse(ext$west > ext$east, 360 - ext$west + ext$east, ext$east - ext$west)
  ext[which.max(lon_span * (ext$north - ext$south)), , drop = FALSE]
}

#' Empirical point scale factor of a projected CRS at given positions.
#'
#' Projects two short geodesic arcs centred on each position and compares each
#' projected length against its true length on the WGS84 ellipsoid:
#'   parallel arc  ground length = N(phi) * cos(phi) * dlambda,
#'                 N = a / sqrt(1 - e^2 sin^2 phi)
#'   meridian arc  ground length = M(phi) * dphi,
#'                 M = a (1 - e^2) / (1 - e^2 sin^2 phi)^(3/2)
#' k = projected / ground. A CRS that preserves distance at the data reports
#' k = 1 there; k = 1.64 means every distance it reports is 64% too long.
#'
#' BOTH arcs are measured, because one is only sufficient for a CONFORMAL
#' projection (TM, Mercator, LCC), where k is identical in every direction. An
#' equal-area projection distorts the two by construction and in opposite
#' senses - EPSG:5070 at Potsdam is +3.17% along the parallel and -3.07% along
#' the meridian - and equal-area is exactly what a user might wrongly pick for
#' distance work. The gate judges the larger |k - 1|.
#'
#' @param lon,lat Positions in degrees; the worst case over them is returned.
#' @param arc_deg Half-arc in degrees. 0.001 deg is about 111 m, short enough
#'   that k is the POINT scale factor rather than a finite-distance average.
#' @return list(k, dev, parallel, meridian, lon, lat) at the worst position, or
#'   NULL for a geographic CRS (no linear scale to speak of) and whenever the
#'   projection cannot be evaluated. Callers treat NULL as "do not block".
crs_scale_factor <- function(crs, lon, lat, arc_deg = 0.001) {
  if (is.null(crs) || length(crs) != 1) return(NULL)
  co <- suppressWarnings(tryCatch(sf::st_crs(crs), error = function(e) NULL))
  if (is.null(co) || is.na(co) || isTRUE(sf::st_is_longlat(co))) return(NULL)

  lon <- suppressWarnings(as.numeric(lon)); lat <- suppressWarnings(as.numeric(lat))
  ok <- is.finite(lon) & is.finite(lat) & abs(lat) < 89.9
  if (!any(ok)) return(NULL)
  lon <- lon[ok]; lat <- lat[ok]

  a <- 6378137; f <- 1 / 298.257223563; e2 <- f * (2 - f)
  phi <- lat * pi / 180
  rad <- arc_deg * pi / 180
  N <- a / sqrt(1 - e2 * sin(phi)^2)
  M <- a * (1 - e2) / (1 - e2 * sin(phi)^2)^1.5
  g_par <- N * cos(phi) * rad
  g_mer <- M * rad

  h <- arc_deg / 2
  pts <- data.frame(
    .x = c(lon - h, lon + h, lon, lon),
    .y = c(lat, lat, lat - h, lat + h))
  xy <- suppressWarnings(tryCatch(
    sf::st_coordinates(sf::st_transform(
      sf::st_as_sf(pts, coords = c(".x", ".y"), crs = 4326), co)),
    error = function(e) NULL))
  if (is.null(xy) || nrow(xy) != nrow(pts)) return(NULL)

  n <- length(lon)
  i <- seq_len(n)
  seg <- function(a_i, b_i) sqrt((xy[b_i, 1] - xy[a_i, 1])^2 + (xy[b_i, 2] - xy[a_i, 2])^2)
  k_par <- seg(i, n + i) / g_par
  k_mer <- seg(2 * n + i, 3 * n + i) / g_mer

  dev <- pmax(abs(k_par - 1), abs(k_mer - 1))
  dev[!is.finite(dev)] <- NA_real_
  if (all(is.na(dev))) return(NULL)
  w <- which.max(dev)
  k <- if (abs(k_par[w] - 1) >= abs(k_mer[w] - 1)) k_par[w] else k_mer[w]
  list(k = unname(k), dev = unname(dev[w]), parallel = unname(k_par[w]),
       meridian = unname(k_mer[w]), lon = lon[w], lat = lat[w])
}

#' Where the data lands, in degrees: centroid plus bounding-box corners.
#'
#' The gate samples five positions rather than one because a large study area
#' can be well inside tolerance at its centre and outside it at an edge - the
#' UTM zone-boundary case exactly. Each is resolved through Phase A's
#' crs_landing_position(), so the gate and the Data Setup readout can never
#' disagree about where the data is.
#'
#' @param crs The INPUT Data CRS: this converts the file's own coordinates to
#'   degrees, and is unrelated to the target CRS being judged.
crs_sample_positions <- function(df, x_col, y_col, crs) {
  if (is.null(df) || is.null(x_col) || is.null(y_col)) return(NULL)
  if (!all(c(x_col, y_col) %in% colnames(df))) return(NULL)
  x <- suppressWarnings(as.numeric(as.character(df[[x_col]])))
  y <- suppressWarnings(as.numeric(as.character(df[[y_col]])))
  ok <- is.finite(x) & is.finite(y)
  if (!any(ok)) return(NULL)
  x <- x[ok]; y <- y[ok]

  cand <- unique(rbind(
    c(mean(x), mean(y)),
    as.matrix(expand.grid(c(min(x), max(x)), c(min(y), max(y)),
                          KEEP.OUT.ATTRS = FALSE))))
  pos <- lapply(seq_len(nrow(cand)), function(i)
    crs_landing_position(data.frame(.x = cand[i, 1], .y = cand[i, 2]), ".x", ".y", crs))
  pos <- pos[!vapply(pos, is.null, logical(1))]
  if (!length(pos)) return(NULL)
  data.frame(lon = vapply(pos, function(z) z$lon, numeric(1)),
             lat = vapply(pos, function(z) z$lat, numeric(1)))
}

#' Is this Target Mapping CRS fit to measure with, where the data actually is?
#'
#' The shared judgement behind both the selection-time advisory and the run
#' gate, so the two can never state different rules.
#'
#' A GEOGRAPHIC target CRS returns "ok" untouched: EPSG:4326 is not a
#' projection, has no meaningful k, and the pipeline projects it to a metric
#' UTM zone itself - the same exemption validate_crs(require_metric) makes.
#'
#' Thresholds. 0.1% is the practical floor for spatial analysis: a UTM zone
#' holds |k - 1| below 0.04% across its own 6 deg of longitude, so anything
#' worse means the CRS does not belong to the region. 1% is where the error
#' exceeds any plausible tolerance - a 250 m buffer becomes 252.5 m, a
#' variogram range is misreported by the same factor - and is refused, subject
#' to an explicit override. Falling outside the declared area of use WARNS but
#' never refuses: the declared box is an authority's advisory bound, and a
#' study area straddling a zone boundary sits outside one box while still
#' measuring correctly. k is what measures the harm.
#'
#' `msg` states the FACTS about this CRS at this data and nothing else; the
#' standing consequence clause is `detail`, so a caller can lead with the
#' finding and put the explanation behind a tooltip without either copy of the
#' wording drifting from the other.
#'
#' @return list(level = "ok"/"warn"/"block", title, msg, detail, k, dev, outside).
crs_target_suitability <- function(crs, lon, lat, warn_dev = 0.001, block_dev = 0.01) {
  out <- list(level = "ok", title = NULL, msg = NULL, detail = NULL, k = NA_real_,
              dev = NA_real_, outside = NA)
  if (is.null(crs) || length(crs) != 1 || !nzchar(as.character(crs)[1])) return(out)
  co <- suppressWarnings(tryCatch(sf::st_crs(crs), error = function(e) NULL))
  if (is.null(co) || is.na(co) || isTRUE(sf::st_is_longlat(co))) return(out)

  lon <- suppressWarnings(as.numeric(lon)); lat <- suppressWarnings(as.numeric(lat))
  ok <- is.finite(lon) & is.finite(lat)
  if (!any(ok)) return(out)
  lon <- lon[ok]; lat <- lat[ok]
  label <- as.character(crs)[1]
  msgs <- character(0)

  ext <- crs_area_of_use(crs)
  if (!is.null(ext) && nrow(ext)) {
    # A sample position is inside the area of use when ANY declared extent
    # holds it: several extents are a union, not a sequence of separate claims.
    inside <- vapply(seq_along(lon), function(j)
      any(crs_extent_contains(ext$west, ext$east, ext$south, ext$north,
                              lon[j], lat[j]) %in% TRUE), logical(1))
    out$outside <- !all(inside)
    if (isTRUE(out$outside)) {
      out$level <- "warn"
      b <- .crs_extent_widest(ext)
      msgs <- c(msgs, sprintf(
        "'%s' declares an area of use of %.3f to %.3f degrees longitude and %.3f to %.3f degrees latitude%s; your data lands at %s, outside it.",
        label, b$west, b$east, b$south, b$north,
        if (nrow(ext) > 1) sprintf(" (the widest of its %d declared extents)", nrow(ext)) else "",
        format_lonlat(mean(lon), mean(lat))))
    }
  }

  k_res <- crs_scale_factor(crs, lon, lat)
  if (!is.null(k_res)) {
    out$k <- k_res$k; out$dev <- k_res$dev
    if (k_res$dev > warn_dev) {
      out$level <- if (k_res$dev > block_dev) "block" else "warn"
      msgs <- c(msgs, sprintf(
        "At your data, '%s' has a point scale factor of k = %.6f: every distance it reports is %+.3f%% off.",
        label, k_res$k, 100 * (k_res$k - 1)))
    }
  }

  if (!length(msgs)) return(out)
  out$title <- if (identical(out$level, "block"))
    "Target Mapping CRS unsuitable for this area" else
    "Target Mapping CRS may not suit this area"
  out$msg <- paste(msgs, collapse = " ")
  out$detail <- crs_measure_detail
  out
}

#' Why the Target Mapping CRS matters, in one place.
#'
#' Every surface that reports a CRS problem needs this sentence and none of
#' them needs it in the headline, so it travels as `crs_target_suitability()`'s
#' `detail` and is shown behind a tooltip (Data Setup) or as the second
#' paragraph (run gate). The last clause answers the question users actually
#' ask when the advisory fires: no, this is not why the points are in the wrong
#' place.
crs_measure_detail <- paste(
  "Grid resolution, buffer radius, variogram ranges, exported cell size and the Map Viewer's projected ruler are all stated in metres and read straight from the Target Mapping CRS, so its scale error at your data is carried by every one of them, with no error raised anywhere.",
  "The Target Mapping CRS never moves your points on the map (that is the Input Data CRS); it decides what a metre means.")

#' The Target Mapping CRS this data should be measured in.
#'
#' crs_target_suitability() says a CRS is wrong; this says which one is right.
#' Diagnosing without prescribing was the whole defect it repairs: the app can
#' derive the answer from the data's own longitude, and used to hand the user a
#' bounding box in decimal degrees to compare against instead.
#'
#' A longitude fixes the UTM zone exactly - the same arithmetic
#' crs_zone_candidates() encodes - so the answer needs no catalogue and no
#' transform; the catalogue is consulted only for the CRS's proper name.
#'
#' WGS 84 / UTM is recommended everywhere rather than the local national grid:
#' it exists for every longitude, |k - 1| never exceeds 0.04% inside its own
#' zone, and it needs no judgement about which of several national systems an
#' area "belongs" to. A user who prefers their national grid types it in, and
#' the gate then judges that choice on the same two rules as any other.
#'
#' The zone comes from the MEAN position while `dev` is measured at EVERY
#' sample position, so a study area spanning zones reports the true worst case
#' of the CRS being offered - which lets a caller decline to offer a
#' recommendation that is no better than what the user already has.
#'
#' @param lon,lat Sample positions in degrees, as crs_sample_positions() returns.
#' @return list(crs, code, label, k, dev), or NULL when no position is usable.
crs_recommend_target <- function(lon, lat) {
  lon <- suppressWarnings(as.numeric(lon)); lat <- suppressWarnings(as.numeric(lat))
  ok <- is.finite(lon) & is.finite(lat) & abs(lat) < 89.9 & abs(lon) <= 180
  if (!any(ok)) return(NULL)
  lon <- lon[ok]; lat <- lat[ok]
  mid_lon <- mean(lon); mid_lat <- mean(lat)

  zone <- min(max(floor((mid_lon + 180) / 6) + 1, 1), 60)
  code <- if (mid_lat < 0) 32700 + zone else 32600 + zone
  crs <- paste0("EPSG:", code)

  label <- NA_character_
  reg <- crs_registry()
  if (!is.null(reg)) {
    i <- which(reg$code == code)
    if (length(i)) label <- reg$name[i[1]]
  }
  if (is.na(label) || !nzchar(label))
    label <- sprintf("WGS 84 / UTM zone %d%s", zone, if (mid_lat < 0) "S" else "N")

  k_res <- crs_scale_factor(crs, lon, lat)
  list(crs = crs, code = code, label = label,
       k = if (is.null(k_res)) NA_real_ else k_res$k,
       dev = if (is.null(k_res)) NA_real_ else k_res$dev)
}

# Two selectors, two lists. Web Mercator is a legitimate INPUT CRS - data
# really does arrive in it - but never an analysis one: at 52 deg N it inflates
# every distance by 64%, so it is not offered as a Target, and the suitability
# gate refuses it if it is typed in anyway.
common_crs_input <- c(
  "WGS 84 (EPSG:4326)" = "EPSG:4326",
  "UTM 35N (EPSG:32635)" = "EPSG:32635",
  "UTM 33N (EPSG:32633)" = "EPSG:32633",
  "UTM 34N (EPSG:32634)" = "EPSG:32634",
  "S-JTSK / Krovak East North (EPSG:5514)" = "EPSG:5514",
  "Pseudo-Mercator (EPSG:3857)" = "EPSG:3857"
)

common_crs_target <- common_crs_input[common_crs_input != "EPSG:3857"]

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

#' Retire the raster image layers a shorter restyle left behind.
#'
#' The Map Viewer gives its raster images positional Leaflet ids
#' (`rast_img_1`, `rast_img_2`, ...) and re-adds them on every restyle.
#' Leaflet REPLACES a layer whose id matches, but it never drops one the new
#' pass does not re-add, so a run that paints fewer images than the one
#' already on the map would leave the surplus on screen. `n_prev` is how many
#' images the previous pass added to this map, `n_now` how many this one did.
#'
#' @param m A leaflet widget or leafletProxy.
#' @return `m`, with the surplus ids removed.
remove_surplus_raster_images <- function(m, n_now, n_prev) {
  if (is.null(n_prev) || n_prev <= n_now) return(m)
  for (k in seq.int(n_now + 1L, n_prev)) {
    m <- leaflet::removeImage(m, raster_img_layer_id(k))
  }
  m
}

#' Leaflet layer id of the i-th Map Viewer raster image.
raster_img_layer_id <- function(i) paste0("rast_img_", i)

# robust_vgm_fit and clean_gstat_env live in spatial_helpers.R (they are model
# code and must be resolvable by workers that source only that file).
