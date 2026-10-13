# global_utils.R - static configuration and pure (non-reactive) functions.

# Stored tuning values carry the column/subset key they were tuned for; a
# "manual" source survives a later "run" stamp.
stamp_vgm <- function(obj, key, source) {
  if (is.null(obj)) return(NULL)
  if (identical(source, "run") && identical(attr(obj, "monolith_source"), "manual")) source <- "manual"
  attr(obj, "monolith_key") <- key
  attr(obj, "monolith_source") <- source
  obj
}

vgm_key_matches <- function(obj, key) {
  stored <- attr(obj, "monolith_key")
  is_valid_col_ref(key) && is_valid_col_ref(stored) && identical(as.character(stored), as.character(key))
}

resolve_stored_vgm <- function(fit, vgm_mode, key) {
  if (!identical(vgm_mode, "manual") || !vgm_key_matches(fit, key) ||
      !identical(attr(fit, "monolith_source"), "manual")) return(NULL)
  clean_gstat_env(fit)
}

# A stored IDW power or TPS lambda is list(value, key); it applies only to the
# key it was tuned for, otherwise the caller's default is used.
resolve_regional_param <- function(entry, key, default) {
  if (is.list(entry) && is_valid_col_ref(key) && is_valid_col_ref(entry$key) &&
      identical(as.character(entry$key), as.character(key))) entry$value else default
}

# The IDW/TPS manual slot to read or write: "pre" only when the view computes a
# Predicted surface, that surface gets its own parameter (sep_fit, "Fit
# Actual/Predicted separately") and the target switch selects it.
manual_param_target <- function(comp_mode, value_type, switch_value, sep_fit = TRUE) {
  has_pred <- isTRUE(comp_mode) || isTRUE(value_type %in% c("pred", "pred_ss", "resid"))
  if (has_pred && isTRUE(sep_fit) && identical(switch_value, "pre")) "pre" else "act"
}

# The next value of a reactiveValues trigger (rv$proceed_run, rv$proceed_vif):
# an integer counter, so firing a trigger never draws from the session's RNG.
next_trigger <- function(x) {
  if (is.numeric(x) && length(x) == 1 && is.finite(x)) as.integer(x) + 1L else 1L
}

# Identity of one CV experiment: target key, scored rows and their folds.
# Equal ids mean the same samples in the same folds, so metrics are comparable.
# Ids are written as integers (as.character(1e5) is "1e+05").
cv_population_id <- function(key, row_id, folds) {
  if (is.null(row_id) || !length(row_id)) return(NA_character_)
  ints <- function(x) paste(as.character(as.integer(x)), collapse = ",")
  .hash8(paste(as.character(key %||% ""), ints(row_id), ints(folds), sep = "|"))
}

# The pooled ("Total (Combined)") id: the same hash over the localities' sorted
# ids, so their finishing order cannot change it.
cv_pooled_population_id <- function(ids) {
  ids <- ids[!vapply(ids, function(x) is.null(x) || length(x) != 1 || is.na(x), logical(1))]
  if (!length(ids)) return(NA_character_)
  .hash8(paste(sort(paste0(names(ids), ":", unlist(ids, use.names = FALSE))), collapse = "|"))
}

# Main-session record of a surface's CV population: the worker's report plus
# the experiment id and what the folds re-estimated. IDW and TPS carry no
# population name, so no id.
stamp_cv_population <- function(cv_info, key) {
  if (is.null(cv_info)) return(NULL)
  named <- !is.na(cv_info$population %||% NA_character_)
  cv_info$pop_id <- if (named) cv_population_id(key, cv_info$row_id, cv_info$folds1) else NA_character_
  cv_info$refit <- if (!named) NA_character_
    else if (!is.null(cv_info$conditional)) paste0("conditional on ", cv_info$conditional)
    else "per fold"
  # The row and fold vectors exist only to build the id; not kept in rv.
  cv_info$row_id <- NULL
  cv_info$folds1 <- NULL
  cv_info
}

# The pooled ("Total (Combined)") row's population record. A field the
# localities disagree on is reported as "mixed" rather than as one of them.
pooled_cv_population <- function(infos) {
  infos <- Filter(Negate(is.null), infos %||% list())
  if (!length(infos)) return(NULL)
  agreed <- function(field) {
    v <- unique(vapply(infos, function(x) as.character(x[[field]] %||% NA_character_), character(1)))
    if (length(v) == 1L) v else "mixed"
  }
  ids <- lapply(infos, function(x) x$pop_id)
  names(ids) <- names(infos)
  list(population = agreed("population"),
       refit = agreed("refit"),
       pop_id = cv_pooled_population_id(ids),
       n_localities = length(infos),
       n_conditional = sum(vapply(infos, function(x) !is.null(x$conditional), logical(1))))
}

#' Which locality and surface a progress/warning status file belongs to.
#'
#' Files are `<kind>_<session>_<key>_<act|pre>.txt` (`.write_status_file()`);
#' safe_key() keys may contain underscores, so the trailing `_act`/`_pre`
#' separates the fields. The one parser for the live poller and the run log.
#'
#' Returns `list(key, locality, target, suffix, label)`; `locality` falls back
#' to the key, and `target` is NA for a file without a surface suffix.
status_file_parts <- function(path, session_id, kind = "warn", localities = NULL) {
  base <- basename(as.character(path)[1])
  stem <- sub(paste0("^", kind, "_", session_id, "_"), "", sub("\\.txt$", "", base))
  target <- if (grepl("_act$", stem)) "act" else if (grepl("_pre$", stem)) "pre" else NA_character_
  key <- if (is.na(target)) stem else sub("_(act|pre)$", "", stem)
  hit <- match(key, safe_key(localities))
  locality <- if (is.na(hit)) key else as.character(localities[hit])
  suffix <- if (identical(target, "act")) " (Actual)"
            else if (identical(target, "pre")) " (Predicted)" else ""
  list(key = key, locality = locality, target = target, suffix = suffix,
       label = paste0(locality, suffix))
}

#' Run-duration history log in the user's data directory; tests redirect it via
#' option `monolith_history_dir`. Appends are not locked: a torn append costs
#' one ETA record in a single-user desktop app.
monolith_history_file <- function() {
  file.path(
    getOption("monolith_history_dir", tools::R_user_dir("monolith", which = "data")),
    "run_history.csv"
  )
}

#' Evaluate a plot write with showtext's dpi matched to the device.
#'
#' showtext sizes text against its own dpi option (96), so a 300-dpi export
#' would otherwise render text at 96/300 of the requested size.
#'
#' @param dpi Device resolution; vector devices (pdf, svg) take 72.
#' @param expr Write to perform, evaluated once with the option in force.
with_showtext_dpi <- function(dpi, expr) {
  if (!requireNamespace("showtext", quietly = TRUE)) return(force(expr))
  old <- showtext::showtext_opts(dpi = dpi)
  on.exit(showtext::showtext_opts(old), add = TRUE)
  force(expr)
}

estimate_run_duration <- function(loc_sample_counts, method, comp_mode, cores) {
  history_file <- monolith_history_file()

  # Method multipliers: RFK/RK/CK are measured, OK/IDW/TPS unverified guesses.
  method_mult <- switch(method,
    "RFK" = 1.0,
    "RK"  = 1.0,
    "CK"  = 1.3,
    "OK"  = 0.5,
    "IDW" = 0.5,
    "TPS" = 0.3,
    1.0
  )
  
  loc_sample_counts[is.na(loc_sample_counts) | loc_sample_counts == 0] <- 50
  
  n_locs <- length(loc_sample_counts)
  n_models <- n_locs * (if(comp_mode) 2 else 1)
  
  loc_times_sec <- numeric(n_locs)
  history_data <- NULL
  
  if (file.exists(history_file)) {
    # The whole block is the tryCatch value: a handler assigning NULL would only
    # touch its own frame and leave a partially filtered table behind.
    history_data <- tryCatch({
      hd <- read.csv(history_file)
      hd <- hd[hd$method == method, ]

      # Narrow to the same comp_mode, then core count, while >= 5 records remain.
      comp_history <- hd[hd$comp_mode == comp_mode, ]
      if (nrow(comp_history) >= 5) {
        hd <- comp_history
      }

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
    # Cold-start formula from two measurements (79 -> 150 s, 355 -> 510 s for
    # two models), scaled to 70%.
    base_sec <- pmax(5.25, 16.45 + 0.455 * loc_sample_counts)
    model_time <- base_sec * method_mult
    loc_times_sec <- model_time * (if (comp_mode) 2 else 1)
  }
  
  max_single_loc_time <- max(loc_times_sec)
  
  # 0.75 effective cores per core.
  eff_factor <- 0.75
  distributed_time <- sum(loc_times_sec) / max(1, (cores * eff_factor))
  
  # Wider margin for the cold-start formula.
  fudge_mult <- if (is_history_based) 1.25 else 1.4
  est_time_sec <- max(max_single_loc_time, distributed_time) * fudge_mult
  
  est_time_str <- if (est_time_sec < 60) {
    paste(round(est_time_sec), "seconds")
  } else {
    paste(round(est_time_sec / 60, 1), "minutes")
  }
  # Fewer than 5 matching history records: the cold-start formula is a guess.
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


#' Validate a CRS for sf and terra; NULL (with a notification) when invalid.
#'
#' @param require_metric Refuse a CRS whose axis unit is positively not the
#'   metre (crs_metre_factor(), spatial_pipeline.R). For the Target Mapping CRS
#'   only: the pipeline projects out of the Input Data CRS before measuring.
validate_crs <- function(crs_selection, error_prefix = "Invalid CRS provided", duration = NULL,
                         require_metric = FALSE) {
  tryCatch({
    c_obj <- sf::st_crs(crs_selection)
    if (is.na(c_obj)) stop("Invalid CRS format.")

    t_obj <- terra::crs(crs_selection)
    if (t_obj == "") stop("Invalid CRS for terra.")

    # Only a positively non-metric unit blocks: a geographic CRS is projected to
    # UTM by the pipeline, and an unresolved unit passes.
    if (isTRUE(require_metric)) {
      f <- crs_metre_factor(c_obj)
      if (!is.na(f) && abs(f - 1) > 1e-9) {
        stop(sprintf("axis unit is '%s', not metres. Exported rasters, the resolution suggestion and on-map measurements are stated in metres, so this CRS would report every distance %.4gx its true size. Choose a metric projected CRS for the area (e.g. its UTM zone).",
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
# Bare projected coordinates do not carry their CRS (one easting/northing is
# valid in all 60 UTM zones), so the input CRS is identified only from evidence
# in the file, never assumed.

#' Normalise a typed CRS entry: a bare 4-6 digit code becomes `EPSG:<n>`
#' (sf::st_crs("32633") errors); anything else is returned untouched.
normalize_crs_input <- function(x) {
  if (is.null(x) || length(x) != 1) return(x)
  if (is.numeric(x) && is.finite(x)) return(paste0("EPSG:", format(x, scientific = FALSE)))
  if (!is.character(x) || is.na(x)) return(x)
  s <- trimws(x)
  if (grepl("^[0-9]{4,6}$", s)) paste0("EPSG:", s) else x
}

#' Whether a CRS selector's value was put there by the user: it is set, it is
#' not a value still waiting for its clear to reach the browser (`stale`), and
#' it is not one the app wrote itself (`auto`, every value the app has set in
#' that selector).
crs_chosen_by_user <- function(value, auto = character(0), stale = NULL) {
  v <- as.character(value %||% "")
  if (length(v) != 1 || is.na(v) || !nzchar(v)) return(FALSE)
  if (identical(v, stale %||% "")) return(FALSE)
  !(v %in% auto)
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
#' A WGS84 UTM zone and its ETRS89 twin agree to under a metre and would tie in
#' every test, so candidates are grouped by landing position; the first of each
#' group (candidate order = preference) is kept, the rest reported equivalent.
#'
#' @param tol_m Grouping distance. Tier 2A keeps 5 m (only identical grids tie);
#'   the Tier-3 shortlist passes a wider one (crs_candidate_shortlist()).
#' @param pos Optional data.frame(lon, lat) of positions already computed by
#'   crs_landing_position(), sparing a second transform.
#' @return list(cands, equivalent, spread_m); `spread_m` is the furthest
#'   member's distance from its representative.
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
#' Accepted only when one candidate reproduces the pair within `tol_m` and every
#' other is at least `ratio` times worse; same-grid datum siblings (WGS84 vs
#' ETRS89 UTM) are folded as equivalent first.
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
#' Under the right CRS the points fall inside (or beside) the boundary; a wrong
#' zone throws them hundreds of kilometres away. The boundary's own CRS is
#' tried first, which covers national grids outside the zone family.
#'
#' @return list(crs, epsg, fraction, evidence) or NULL.
identify_crs_from_boundary <- function(x, y, boundary, min_frac = 0.5) {
  tg <- .crs_boundary_target(boundary)
  if (is.null(tg)) return(NULL)
  target <- tg$target
  cen_lon <- tg$cen_lon

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

#' A boundary's union in EPSG:4326, buffered by 5% of its own diagonal
#' (points legitimately sit slightly outside a study boundary), plus its centre
#' longitude. NULL when the boundary is absent or carries no CRS.
.crs_boundary_target <- function(boundary) {
  if (is.null(boundary)) return(NULL)
  geom <- suppressWarnings(tryCatch({
    g <- sf::st_union(sf::st_geometry(boundary))
    if (is.na(sf::st_crs(g))) return(NULL)
    sf::st_transform(g, 4326)
  }, error = function(e) NULL))
  if (is.null(geom) || length(geom) == 0) return(NULL)
  bb <- sf::st_bbox(geom)
  pad <- 0.05 * sqrt((bb[["xmax"]] - bb[["xmin"]])^2 + (bb[["ymax"]] - bb[["ymin"]])^2)
  # With s2 on, st_buffer() on lon/lat takes metres: pad by the geodesic diagonal.
  if (isTRUE(sf::sf_use_s2())) {
    corners <- sf::st_sfc(sf::st_point(c(bb[["xmin"]], bb[["ymin"]])),
                          sf::st_point(c(bb[["xmax"]], bb[["ymax"]])), crs = 4326)
    diag_m <- suppressWarnings(tryCatch(as.numeric(sf::st_distance(corners[1], corners[2])),
                                        error = function(e) NA_real_))
    pad <- if (is.finite(diag_m)) 0.05 * diag_m else 0
  }
  list(target = suppressWarnings(tryCatch(sf::st_buffer(geom, pad), error = function(e) geom)),
       cen_lon = mean(c(bb[["xmin"]], bb[["xmax"]])))
}

#' Identify the input CRS of a mapped X/Y pair, with a message naming the
#' evidence: Tier 1 (degree ranges) -> EPSG:4326, Tier 2A (companion lon/lat)
#' and Tier 2B (boundary) -> the proven code, otherwise NULL (no guess).
#'
#' Degree ranges alone prove nothing (a local metre grid fits them too), so
#' Tier 1 also needs lon/lat column names or a boundary containing at least
#' half the points read as degrees; otherwise it returns `crs = NULL` with
#' `suggest = "EPSG:4326"` and the caller asks.
identify_input_crs <- function(df, x_col, y_col, boundary = NULL) {
  if (is.null(df) || !all(c(x_col, y_col) %in% colnames(df))) return(NULL)
  x <- suppressWarnings(as.numeric(as.character(df[[x_col]])))
  y <- suppressWarnings(as.numeric(as.character(df[[y_col]])))
  ok <- is.finite(x) & is.finite(y)
  if (sum(ok) < 1) return(NULL)

  if (all(abs(x[ok]) <= 180) && all(abs(y[ok]) <= 90)) {
    datum_note <- " If they were recorded on another geographic datum (for example ED50), set that CRS instead."
    if (tolower(trimws(x_col)) %in% .crs_lon_names && tolower(trimws(y_col)) %in% .crs_lat_names) {
      return(list(crs = "EPSG:4326", epsg = 4326, evidence = "degrees",
                  message = paste0(sprintf(
                    "Input CRS set to EPSG:4326 (WGS 84): the '%s'/'%s' columns are named as longitude/latitude and their values fit degree ranges.",
                    x_col, y_col), datum_note)))
    }
    tg <- .crs_boundary_target(boundary)
    if (!is.null(tg)) {
      frac <- suppressWarnings(tryCatch({
        p <- sf::st_as_sf(data.frame(.x = x[ok], .y = y[ok]), coords = c(".x", ".y"), crs = 4326)
        mean(lengths(sf::st_intersects(p, tg$target)) > 0)
      }, error = function(e) NA_real_))
      if (is.finite(frac) && frac >= 0.5) {
        return(list(crs = "EPSG:4326", epsg = 4326, evidence = "degrees", fraction = frac,
                    message = paste0(sprintf(
                      "Input CRS set to EPSG:4326 (WGS 84): read as degrees, %.0f%% of the points fall inside the uploaded boundary shapefile.",
                      100 * frac), datum_note)))
      }
    }
    return(list(crs = NULL, epsg = NULL, suggest = "EPSG:4326", evidence = "degree_range",
                message = sprintf(
                  "The '%s'/'%s' values fit longitude/latitude ranges, but nothing in the file confirms they are degrees: the columns are not named as longitude/latitude and no boundary shapefile confirms their position. A local metre grid fits these ranges too. Confirm below, or set the CRS the coordinates were recorded in.",
                  x_col, y_col)))
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

#' Where the mapped coordinates land, in degrees, under `crs` (NULL when not
#' computable): the readout that makes a wrong zone obvious.
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

# ── EPSG registry: candidates by declared area of use (Tier 3) ─────────────
# With no evidence in the file, the user says WHERE the data is and the app
# lists the projections that put it there. The catalogue is PROJ's proj.db
# (shipped with sf): ~5,330 non-deprecated EPSG projected CRSs with their
# area-of-use boxes, read once (~0.3 s) and cached. Containment runs first as
# plain arithmetic, so only the 15-50 survivors are transformed. Entry points
# return NULL instead of erroring; without proj.db or RSQLite the shortlist
# falls back to crs_zone_candidates().

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
#'   row per declared extent; `descr` lists the countries covered (what the
#'   free-text filter searches). NULL when unreadable. Cached for the session,
#'   a failure included.
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
#' The app's one containment test, vectorised over both sides (catalogue
#' filter, shortlist self-check, suitability gate). `west > east` is an extent
#' crossing the antimeridian, tested as a union. NA bounds answer NA, so
#' callers use which().
crs_extent_contains <- function(west, east, south, north, lon, lat, pad = 0) {
  w <- west - pad; e <- east + pad
  # Not ifelse(): its result has the TEST's length, which would collapse one
  # extent tested against several points to the first point.
  wrapped <- west > east
  in_lon <- (!wrapped & lon >= w & lon <= e) | (wrapped & (lon >= w | lon <= e))
  in_lon & lat >= south - pad & lat <= north + pad
}

#' Primary Tier-3 filter: every CRS whose area of use contains the indicated
#' point. Pure arithmetic, so it runs over the whole catalogue instantly.
crs_registry_filter_extent <- function(reg, lon, lat, pad = 0) {
  if (is.null(reg) || !nrow(reg)) return(NULL)
  if (!isTRUE(is.finite(lon)) || !isTRUE(is.finite(lat))) return(NULL)
  hit <- reg[which(crs_extent_contains(reg$west, reg$east, reg$south, reg$north,
                                      lon, lat, pad)), , drop = FALSE]
  hit[!duplicated(hit$code), , drop = FALSE]
}

#' Secondary Tier-3 filter: free text against the CRS name and its area-of-use
#' text ("Germany", "Kansas", "zone 33N"). Text does not bound the search like
#' a point ("United States" matches over a thousand codes), so the result is
#' capped at `limit` and flagged with a `truncated` attribute.
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

#' Preference order within a group of CRS that land the data in the same place:
#' WGS 84, then ETRS89, then the rest. crs_collapse_candidates() keeps the first
#' member, so this decides the name shown; the sort is stable, so the
#' crs_zone_candidates() fallback keeps its own order.
.crs_preference_order <- function(names) {
  names[is.na(names)] <- ""
  order(ifelse(grepl("^WGS 84 /", names), 1L,
               ifelse(grepl("^ETRS89 /", names), 2L, 3L)))
}

#' Tier 3 - the candidate shortlist.
#'
#' @param x,y The mapped coordinate columns, in their own (unknown) CRS.
#' @param lon,lat Where the user says the data is (a click on the world
#'   mini-map); the primary path.
#' @param area_text Country or region text; used alone when there is no click.
#' @param limit Rows returned after collapsing same-place candidates.
#' @param max_km Reject a candidate landing further than this from the click;
#'   wide because a click is rough (the next UTM zone misses by 400 km).
#' @param collapse_m Candidates landing within this distance form one row.
#'   Datum siblings (WGS 84/72/72BE and ED50 UTM 33N land within 210 m at
#'   Potsdam) are closer than a click resolves, so they are ranked by
#'   preference rather than click noise; adjacent UTM zones are never folded.
#' @return data.frame(epsg, name, area, lon, lat, distance_km, text, spread_m)
#'   plus an `equivalent` list column of the codes folded onto each row,
#'   nearest first; NULL when nothing can be computed, zero rows when nothing
#'   fits. Attributes: `truncated` (the text matched too many codes to search)
#'   and `text_no_match` (the text matched nothing inside the clicked area).
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
  # Text alongside a click narrows the already bounded pool.
  narrowed <- FALSE
  if (have_click && !is.null(pool) && nrow(pool) &&
      !is.null(area_text) && nzchar(trimws(area_text))) {
    pool <- crs_registry_filter_area(pool, area_text, limit = nrow(pool))
    narrowed <- TRUE
  }
  trunc <- isTRUE(attr(pool, "truncated"))

  if (is.null(pool) || !nrow(pool)) {
    # A text filter that matched nothing narrows to EMPTY, never back out.
    if (narrowed) { no_match <- TRUE; return(flag(.crs_shortlist_empty())) }
    # No catalogue: a longitude still fixes the UTM zone, so fall back to the
    # four-code zone family.
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

  # Self-consistency: the data read under a candidate must land inside that
  # candidate's own area of use (Potsdam read as Swiss LV95 does not).
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
  # Preference picks each group's representative, then distance orders the
  # groups; the reverse would let sub-metre datum differences pick it.
  pool <- pool[.crs_preference_order(pool$name), , drop = FALSE]

  # Reuse the landing positions instead of transforming again.
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
# A metre axis unit is necessary but not sufficient: Web Mercator is metric yet
# 64% long at 52 deg N, and a UTM zone two zones away is 1.07% long. Grid
# resolution, buffers, variogram ranges, exported cell size and the ruler all
# read metres off this CRS. Two checks: the data inside the CRS's declared
# area of use, and point scale factor k near 1. "Cannot answer" (unparseable,
# no extent, untransformable) never blocks.

#' A CRS's own declared area of use, in degrees.
#'
#' Read from PROJ's catalogue by EPSG code, else from the WKT2 BBOX node (which
#' covers geographic and non-EPSG CRS; written BBOX[south, west, north, east]),
#' else NULL. Every declared extent is returned (17 codes declare several, e.g.
#' EPSG:2393): a code's area of use is their union.
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
#' Projects two short arcs centred on each position and divides each projected
#' length by its WGS84 ellipsoid length:
#'   parallel  N(phi) cos(phi) dlambda,  N = a / sqrt(1 - e^2 sin^2 phi)
#'   meridian  M(phi) dphi,              M = a (1 - e^2) / (1 - e^2 sin^2 phi)^(3/2)
#' Both arcs, because only a conformal projection scales equally in every
#' direction (EPSG:5070 at Potsdam: +3.17% along the parallel, -3.07% along the
#' meridian). The larger |k - 1| is judged.
#'
#' @param lon,lat Positions in degrees; the worst case over them is returned.
#' @param arc_deg Arc length (0.001 deg ~ 111 m), short enough for a point scale.
#' @return list(k, dev, parallel, meridian, lon, lat) at the worst position;
#'   NULL for a geographic CRS or an unevaluable projection ("do not block").
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
#' Five positions, because a large study area can be within tolerance at its
#' centre and outside it at an edge (the UTM zone-boundary case). Each goes
#' through crs_landing_position(), as the Data Setup readout does.
#'
#' @param crs The INPUT Data CRS (converts the file's coordinates to degrees).
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

#' Is this Target Mapping CRS fit to measure with where the data is?
#'
#' One judgement for the selection-time advisory and the run gate. A geographic
#' target returns "ok": the pipeline projects it to UTM itself.
#'
#' |k - 1| above 0.1% warns (a UTM zone stays below it across its own 6 deg);
#' above 1% blocks, subject to an explicit override. Falling outside the
#' declared area of use only warns: the box is advisory and a study area
#' straddling a zone boundary still measures correctly; k measures the harm.
#'
#' `msg` states the findings; the standing explanation is `detail`
#' (crs_measure_detail), so callers can put it behind a tooltip.
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
    # Several declared extents are a union: inside any one counts.
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

#' Why the Target Mapping CRS matters: the `detail` of crs_target_suitability(),
#' shown behind a tooltip (Data Setup) or as the run gate's second paragraph.
crs_measure_detail <- paste(
  "The finished surface is resampled into the Target Mapping CRS, so exported rasters and their cell size, the resolution suggestion and the Map Viewer's projected ruler are all stated in its metres and carry its scale error at your data, with no error raised anywhere. The models are fitted in the Input Data CRS (or, for geographic or non-metre input, its UTM zone); for projected data choose the same UTM zone for both.",
  "The Target Mapping CRS never moves your points on the map (that is the Input Data CRS); it decides what a metre means.")

#' The Target Mapping CRS this data should be measured in: the WGS 84 / UTM
#' zone of the mean position (defined everywhere, |k - 1| under 0.1% inside its
#' zone, no judgement between national grids; a user may still type one in).
#' The catalogue is consulted only for the name. `dev` is measured at every
#' sample position, so a caller can decline a recommendation no better than
#' the current CRS.
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

#' How an uploaded boundary relates to the samples: how many fall inside the
#' polygons the pipeline will use (a point/line layer's convex hull, via
#' shp_boundary_polygons()) and, when none do, how far the boundary is from
#' them.
#' @return list(n, inside, dist_km, hull, usable) or NULL when it cannot be
#'   answered (no CRS on either side, no usable coordinates).
shp_boundary_overlap <- function(shp, x, y, crs) {
  shp <- shp_assume_crs(shp, crs)
  if (is.null(shp) || is.na(sf::st_crs(shp))) return(NULL)
  src <- suppressWarnings(tryCatch(sf::st_crs(crs), error = function(e) NULL))
  if (is.null(src) || is.na(src)) return(NULL)
  x <- suppressWarnings(as.numeric(as.character(x)))
  y <- suppressWarnings(as.numeric(as.character(y)))
  ok <- is.finite(x) & is.finite(y)
  if (!any(ok)) return(NULL)
  tryCatch({
    hull <- !all(as.character(sf::st_geometry_type(shp)) %in% c("POLYGON", "MULTIPOLYGON"))
    poly <- shp_boundary_polygons(shp)
    if (is.null(poly)) return(list(n = sum(ok), inside = 0L, dist_km = NA_real_, hull = hull, usable = FALSE))
    # Tested in the data's own projected CRS, as the pipeline clips (in 4326 sf
    # draws polygon edges as geodesics); geographic data is tested in 4326.
    frame <- if (isTRUE(sf::st_is_longlat(src))) sf::st_crs(4326) else src
    poly <- sf::st_union(sf::st_transform(sf::st_make_valid(poly), frame))
    pts <- sf::st_transform(sf::st_as_sf(data.frame(.x = x[ok], .y = y[ok]),
                                         coords = c(".x", ".y"), crs = src), frame)
    inside <- sum(lengths(sf::st_intersects(pts, poly)) > 0)
    dist_km <- if (inside == 0)
      as.numeric(units::set_units(sf::st_distance(sf::st_union(pts), poly), "km")) else 0
    list(n = sum(ok), inside = inside, dist_km = dist_km, hull = hull, usable = TRUE)
  }, error = function(e) NULL)
}

# Every WGS 84 / UTM zone in both hemispheres is listed, so the dropdown covers
# any study area; crs_choice_groups() lifts the zones around the data to the top.
utm_crs_choices <- local({
  z <- 1:60
  n <- setNames(paste0("EPSG:", 32600 + z), sprintf("UTM %dN (EPSG:%d)", z, 32600 + z))
  s <- setNames(paste0("EPSG:", 32700 + z), sprintf("UTM %dS (EPSG:%d)", z, 32700 + z))
  c(n, s)
})

# Web Mercator is a legitimate INPUT CRS but never a Target: it inflates
# distances 64% at 52 deg N, and the suitability gate refuses it if typed in.
common_crs_input <- c(
  "WGS 84 (EPSG:4326)" = "EPSG:4326",
  "S-JTSK / Krovak East North (EPSG:5514)" = "EPSG:5514",
  "Pseudo-Mercator (EPSG:3857)" = "EPSG:3857",
  utm_crs_choices
)

common_crs_target <- common_crs_input[common_crs_input != "EPSG:3857"]

#' The WGS 84 / UTM zone of a position and its two neighbours, same hemisphere,
#' as EPSG codes with the position's own zone first. Neighbours are included
#' because a study area near a zone edge is often mapped in the adjacent zone.
crs_near_zone_codes <- function(lon, lat) {
  lon <- suppressWarnings(as.numeric(lon)); lat <- suppressWarnings(as.numeric(lat))
  if (length(lon) != 1 || length(lat) != 1 || !is.finite(lon) || !is.finite(lat)) return(integer(0))
  zone <- min(max(floor((lon + 180) / 6) + 1, 1), 60)
  zones <- ((c(zone, zone - 1, zone + 1) - 1) %% 60) + 1
  as.integer((if (lat < 0) 32700 else 32600) + zones)
}

#' Grouped selectize choices for a CRS selector: zones near the data first, then
#' non-UTM entries, the remaining UTM zones by hemisphere, and any value outside
#' `base` (selectize refuses a value that is not an option). Groups are lists so
#' a one-item group still renders as a group.
crs_choice_groups <- function(base, near = integer(0), extra = NULL) {
  near_v <- intersect(paste0("EPSG:", near), base)
  rest <- base[!(base %in% near_v)]
  north <- rest[rest %in% utm_crs_choices & grepl("^EPSG:326", rest)]
  south <- rest[rest %in% utm_crs_choices & grepl("^EPSG:327", rest)]
  extra <- extra[nzchar(extra %||% "") & !(extra %in% base)]
  groups <- list(
    "Near your data" = base[match(near_v, base)],
    "Common" = rest[!(rest %in% utm_crs_choices)],
    "WGS 84 / UTM north" = north,
    "WGS 84 / UTM south" = south,
    "Entered" = setNames(extra, extra)
  )
  lapply(groups[lengths(groups) > 0], as.list)
}

# Every palette get_default_palette() can return must be listed here:
# resolve_var_palette() returns only these, and the picker offers only these.
# The nutrient defaults come first, in nutrient order.
dashboard_palettes <- c("viridis", "Greens", "Blues", "Oranges", "YlOrRd",
                        "PuBuGn", "Purples", "GnBu", "YlGn", "YlOrBr",
                        "RdYlBu", "BrBG", "Greys", "Spectral")

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

#' The palette variable `var` is drawn in: the one the user picked for it this
#' session (`picks`, column -> palette), else the palette its entry in `vars`
#' carries (get_default_palette at mapping time), else `fallback`. Only palettes
#' the picker offers are returned.
resolve_var_palette <- function(var, vars, picks = list(), fallback = "YlOrRd") {
  offered <- function(p) {
    p <- as.character(p %||% NA_character_)[1]
    if (!is.na(p) && p %in% dashboard_palettes) p
  }
  var <- as.character(var %||% NA_character_)[1]
  if (is.na(var) || !nzchar(var)) return(offered(fallback) %||% "YlOrRd")
  entry <- Find(function(v) identical(as.character(v$actual), var), vars %||% list())
  offered(picks[[var]]) %||% offered(entry$palette) %||% offered(fallback) %||% "YlOrRd"
}

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

#' The payload an export-registry item draws, materialised on demand.
#'
#' Uncertainty items store a `derived` spec, not a raster: the variance band
#' already sits in the surface item and the SE is its square root, so a copy
#' would cost ~30 MB per layer at the grid cap, per archived run.
#' `derived$src` is the surface's own SpatRaster; `layer` names the band, `fun`
#' an optional transform and `name` the GeoTIFF band description.
export_item_obj <- function(item) {
  if (is.null(item) || !is.list(item)) return(NULL)
  if (!is.null(item$obj)) return(item$obj)
  d <- item$derived
  if (is.null(d) || is.null(d$src)) return(NULL)
  r <- tryCatch(terra::unwrap(d$src), error = function(e) NULL)
  if (!inherits(r, "SpatRaster")) return(NULL)
  if (!is.null(d$layer)) {
    if (!d$layer %in% names(r)) return(NULL)
    r <- r[[d$layer]]
  }
  if (identical(d$fun, "sqrt")) r <- sqrt(r)
  if (!is.null(d$name)) names(r) <- d$name
  r
}

#' The raster behind an export-registry item, or NULL when it has none. The
#' Actual vs Predicted pair ("map_combined") and the Point Error Map (sf points)
#' have no single-raster form and stay image-only.
export_raster_payload <- function(item) {
  if (is.null(item) || !is.list(item)) return(NULL)
  if (!identical(item$type, "map")) return(NULL)
  obj <- export_item_obj(item)
  if (inherits(obj, "PackedSpatRaster")) return(terra::unwrap(obj))
  if (inherits(obj, "SpatRaster")) return(obj)
  NULL
}

#' Excel sheet name for an export-registry table: at most 31 characters of
#' letters, digits and spaces, unique within `used` ignoring case (as Excel
#' compares). The whole "<variable label> - " prefix is dropped (one variable
#' per workbook; the label itself may contain " - "). A clash gets "_Pre" for
#' a predicted-surface item, else a counter.
export_sheet_name <- function(label, id = "", used = character(0), var_label = NULL) {
  label <- label %||% ""
  id <- id %||% ""
  taken <- function(x) tolower(x) %in% tolower(used)
  if (is.character(var_label) && length(var_label) == 1 && !is.na(var_label) &&
      nzchar(var_label) && startsWith(label, paste0(var_label, " - "))) {
    label <- substring(label, nchar(var_label) + 4L)
  }
  clean <- function(s) trimws(gsub("[^a-zA-Z0-9 ]", "_", s))
  name <- clean(label)
  if (!nzchar(name)) name <- clean(id)
  if (!nzchar(name)) name <- paste0("Table_", length(used) + 1L)
  fit <- function(base, suffix) paste0(trimws(substr(base, 1, 31 - nchar(suffix))), suffix)
  name <- fit(name, "")
  if (taken(name)) {
    suffix <- if (grepl("_pre_|_pre$", id)) "_Pre" else "_2"
    counter <- 2
    candidate <- fit(name, suffix)
    while (taken(candidate)) {
      counter <- counter + 1
      candidate <- fit(name, paste0("_", counter))
    }
    name <- candidate
  }
  name
}

#' File name of an exported registry item: prefix, item id (which names the
#' locality for a per-locality item), the run's method and a timestamp, so a
#' file that leaves the app still says which run it came from.
export_file_name <- function(prefix, id, method, timestamp, ext) {
  parts <- c(prefix, id, if (length(method) == 1 && !is.na(method) && nzchar(method)) method, timestamp)
  paste0(paste(parts, collapse = "_"), ".", ext)
}

#' File extension for a styler format token ("gtiff" writes a .tif).
styler_format_ext <- function(fmt) {
  switch(fmt %||% "png",
         gtiff = "tif", tiff = "tiff", pdf = "pdf", jpg = "jpg", png = "png",
         "png")
}

#' The Download Run Configuration record: run configuration, the per-locality
#' tuning the method consumed (IDW powers and fits, or TPS lambdas, effective
#' df and GCV curve; none for kriging) and provenance. A kNNDM run also records
#' its fold settings (`cv_knndm`).
run_record_payload <- function(cfg, regional_params, app_version, pkg_versions) {
  keep <- switch(cfg$method %||% "", IDW = "^idw_", TPS = "^tps_", NULL)
  if (identical(cfg$cv_strategy, "knndm")) {
    settings <- list(cv_knndm = list(
      k = CV_FOLD_K, maxp = KNNDM_MAXP, min_n = CV_KNNDM_MIN_N,
      domain_points = KNNDM_DOMAIN_N, exact_max = KNNDM_EXACT_MAX,
      max_cells = KNNDM_MAX_CELLS, candidates_exact = KNNDM_NQ_EXACT,
      candidates_cells = KNNDM_NQ_CELLS, ks_alpha = KNNDM_KS_ALPHA))
    at <- match("cv_repeats", names(cfg))
    cfg <- append(cfg, settings, after = if (is.na(at)) length(cfg) else at)
  }
  out <- list(config = cfg)
  if (!is.null(keep)) {
    out$regional_params <- lapply(regional_params, function(p) {
      p <- p[grepl(keep, names(p))]
      for (nm in grep("^idw_fit_", names(p), value = TRUE)) p[[nm]] <- idw_fit_record(p[[nm]])
      p
    })
  }
  out$provenance <- list(
    app_version = cfg$app_version %||% app_version,
    r_version = R.version.string,
    platform = R.version$platform,
    exported_at = format(Sys.time(), "%Y-%m-%d %H:%M:%S %Z"),
    packages = pkg_versions
  )
  out
}

# An IDW fit as the JSON record writes it: each power as a number where finite
# (JSON has no Inf) beside its name (idw_power_text()).
idw_fit_record <- function(fit) {
  if (!is.list(fit)) return(fit)
  num <- function(p) ifelse(is.finite(p), p, NA_real_)
  name <- function(p) vapply(p, idw_power_text, character(1), USE.NAMES = FALSE)
  # [[ ]], not $: `$p` partially matches `profile` on a record without p.
  p <- fit[["p"]]
  if (length(p) == 1) {
    fit$selected <- name(p)
    fit["p"] <- list(if (is.finite(p)) p else NULL)
  }
  if (NROW(fit$profile)) {
    prof <- data.frame(candidate = name(fit$profile$p), p = num(fit$profile$p), cv_rmse = fit$profile$rmse)
    # Added only when recorded: a named NULL argument makes data.frame() fail.
    if (!is.null(fit$profile$within_se)) prof$within_se <- fit$profile$within_se
    fit$profile <- prof
  }
  if (length(fit$fold_p)) {
    fit$fold_p <- data.frame(fold = names(fit$fold_p) %||% as.character(seq_along(fit$fold_p)),
                             p = num(unname(fit$fold_p)), selected = name(fit$fold_p))
  }
  fit
}

#' The run record as JSON at full precision (jsonlite's default 4 decimals
#' would truncate small GCV scores and RMSEs).
run_record_json <- function(payload) {
  jsonlite::toJSON(payload, pretty = TRUE, auto_unbox = TRUE, digits = NA,
                   null = "null", force = TRUE, POSIXt = "ISO8601")
}

#' Why a JSON file handed to Load Config cannot be loaded (a run record:
#' config + provenance, no session fields), or NULL for a session configuration.
session_config_refusal <- function(cfg) {
  if (!is.list(cfg) || is.null(cfg$config) || is.null(cfg$provenance)) return(NULL)
  ver <- cfg$provenance$app_version %||% cfg$config$app_version %||% "unknown"
  sprintf(paste0("This file is a run record (Download Run Configuration, Monolith %s), not a ",
                 "session configuration: it documents a finished run and cannot be loaded. ",
                 "Load a file saved with DOWNLOAD CONFIGURATION FILE instead."), ver)
}

#' Format of the session configuration Save config writes: every run-defining
#' input, the variable list, the palettes picked per variable and the tuning
#' stores. A file without `config_version` is an older configuration and
#' restores the fields it has.
SESSION_CONFIG_VERSION <- 2L

#' One scalar from a parsed configuration field (jsonlite, simplifyVector =
#' FALSE), NULL when the field is absent, empty or not a single value.
config_scalar <- function(x) {
  x <- unlist(x)
  if (length(x) == 1 && !is.na(x) && nzchar(as.character(x))) x else NULL
}

#' The tuning stores as rows a JSON file can carry: the manual variogram models
#' (Apply manual model; OPTIMIZE ALL VARIOGRAMS previews are refitted, not
#' stored) and the per-locality IDW power and TPS lambda entries, each with the
#' tuning key it was set for.
config_stores_out <- function(v_fit_list, idw_factors, tps_lambdas) {
  vgm_rows <- lapply(names(v_fit_list %||% list()), function(slot) {
    m <- v_fit_list[[slot]]
    if (!identical(attr(m, "monolith_source"), "manual")) return(NULL)
    d <- as.data.frame(m)
    s <- d[d$model != "Nug", , drop = FALSE]
    if (!nrow(s)) return(NULL)
    list(loc = sub("_(act|pre)$", "", slot), target = sub("^.*_", "", slot),
         key = attr(m, "monolith_key"), model = as.character(s$model[1]),
         psill = s$psill[1], range = s$range[1], nugget = sum(d$psill[d$model == "Nug"]))
  })
  param_rows <- function(store) {
    unlist(lapply(names(store %||% list()), function(loc) {
      lapply(names(store[[loc]]), function(tg) {
        list(loc = loc, target = tg, value = store[[loc]][[tg]]$value, key = store[[loc]][[tg]]$key)
      })
    }), recursive = FALSE)
  }
  list(variograms = Filter(Negate(is.null), vgm_rows),
       idw = param_rows(idw_factors), tps = param_rows(tps_lambdas))
}

#' The tuning stores back from config_stores_out() rows as parsed from JSON.
#' Rows of a locality outside `locs`, and rows that are not a valid model or
#' value, are dropped; the localities outside `locs` are listed in `skipped`.
config_stores_in <- function(stores, locs) {
  key_of <- function(r) {
    if (!is.list(r)) return(NULL)
    loc <- config_scalar(r$loc); target <- config_scalar(r$target); key <- config_scalar(r$key)
    if (is.null(loc) || !isTRUE(target %in% c("act", "pre")) || is.null(key)) return(NULL)
    list(loc = as.character(loc), target = target, key = as.character(key))
  }
  all_rows <- c(stores$variograms, stores$idw, stores$tps)
  row_locs <- unlist(lapply(all_rows, function(r) key_of(r)$loc))
  params <- function(rows, ok) {
    out <- list()
    for (r in rows) {
      k <- key_of(r)
      v <- suppressWarnings(as.numeric(config_scalar(r$value)))
      if (is.null(k) || !k$loc %in% locs || length(v) != 1 || !ok(v)) next
      out[[k$loc]][[k$target]] <- list(value = v, key = k$key)
    }
    out
  }
  vgms <- list()
  for (r in stores$variograms) {
    k <- key_of(r)
    model <- config_scalar(r$model)
    p <- suppressWarnings(as.numeric(c(config_scalar(r$psill), config_scalar(r$range),
                                       config_scalar(r$nugget))))
    if (is.null(k) || !k$loc %in% locs || !isTRUE(model %in% c("Sph", "Exp", "Gau", "Mat")) ||
        length(p) != 3 || !is.null(validate_manual_vgm(p[1], p[3], p[2]))) next
    vgms[[paste0(k$loc, "_", k$target)]] <- stamp_vgm(manual_vgm(p[1], model, p[2], p[3]), k$key, "manual")
  }
  list(v_fit_list = vgms,
       idw_factors = params(stores$idw, function(v) isTRUE(v == -1) || idw_fixed_ok(v)),
       tps_lambdas = params(stores$tps, function(v) v %in% c(-1, 0) || tps_fixed_ok(v)),
       skipped = setdiff(unique(row_locs), locs))
}

#' A saved variable list, kept to the variables whose measured column is in
#' `cols`; a prediction column missing from `cols` is dropped from its entry.
#' `skipped` lists the measured columns that are missing.
config_vars_in <- function(vars, cols) {
  str1 <- function(x, default) {
    x <- config_scalar(x)
    if (is.null(x)) default else as.character(x)
  }
  col_or_null <- function(x) {
    x <- str1(x, NA_character_)
    if (!is.na(x) && x %in% cols) x else NULL
  }
  kept <- list(); skipped <- character(0)
  for (v in vars %||% list()) {
    act <- str1(v$actual, NA_character_)
    if (is.na(act)) next
    if (!act %in% cols) { skipped <- c(skipped, act); next }
    cat_v <- str1(v$category, "Uploaded Data")
    lab <- str1(v$label, act)
    pal <- str1(v$palette, NA_character_)
    if (!pal %in% dashboard_palettes) pal <- get_default_palette(act, cat_v, lab)
    kept[[length(kept) + 1]] <- list(
      actual = act, pred = col_or_null(v$pred), pred_ss = col_or_null(v$pred_ss),
      label = lab, category = cat_v, unit = str1(v$unit, ""), palette = pal)
  }
  list(vars = kept, skipped = skipped)
}

#' The palettes the user picked (column -> palette) as a session configuration
#' carries them: the entries of variables in `vars`.
config_palettes_out <- function(picks, vars) {
  cols <- vapply(vars %||% list(), function(v) as.character(v$actual), character(1))
  picks <- picks %||% list()
  picks[intersect(names(picks), cols)]
}

#' Picked palettes back from a configuration: entries of variables in `vars`
#' naming an offered palette; the others are listed in `skipped`. An older
#' file's single `palette_select` applies to `legacy_var`.
config_palettes_in <- function(palettes, vars, legacy_palette = NULL, legacy_var = NULL) {
  if (is.null(palettes) && !is.null(legacy_palette) && !is.null(legacy_var)) {
    palettes <- stats::setNames(list(legacy_palette), legacy_var)
  }
  cols <- vapply(vars %||% list(), function(v) as.character(v$actual), character(1))
  picks <- list(); skipped <- character(0)
  for (var in intersect(names(palettes %||% list()), cols)) {
    p <- config_scalar(palettes[[var]])
    if (isTRUE(as.character(p) %in% dashboard_palettes)) {
      picks[[var]] <- as.character(p)
    } else {
      skipped <- c(skipped, sprintf("palette %s of %s (not offered)", p %||% "(empty)", var))
    }
  }
  list(picks = picks, skipped = skipped)
}

#' `-mo` options for gdal_translate from a named character vector of tags. A
#' newline would end the tag, so line breaks become spaces; a tag with no value
#' is left out rather than written as `NAME=`.
geotiff_tag_options <- function(tags) {
  if (!length(tags)) return(character(0))
  vals <- trimws(gsub("[\r\n]+", " ", as.character(tags)))
  keep <- !is.na(vals) & nzchar(vals)
  as.vector(rbind("-mo", paste0(names(tags)[keep], "=", enc2utf8(vals[keep]))))
}

#' Write a SpatRaster as a compressed GeoTIFF at `file`.
#'
#' Values, CRS and extent are written exactly as computed (no styling);
#' multi-layer surfaces become multi-band files with layer names as band
#' descriptions. A `.tif` extension is enforced because terra picks the driver
#' from it. terra's -9999 STATISTICS placeholders would stretch a GIS display,
#' so the file goes through gdal_translate -stats, which also embeds `tags`
#' (no .aux.xml). If that pass fails, the plain file is kept with a warning.
write_geotiff <- function(r, file, tags = NULL) {
  if (is.null(r) || !inherits(r, "SpatRaster")) stop("No raster surface to export.")
  target <- if (tolower(tools::file_ext(file)) %in% c("tif", "tiff")) file else paste0(file, ".tif")
  tmp <- tempfile(fileext = ".tif")
  on.exit(unlink(c(tmp, paste0(tmp, ".aux.xml"))), add = TRUE)
  terra::writeRaster(r, tmp, overwrite = TRUE, gdal = c("COMPRESS=LZW"))
  if (file.exists(target)) unlink(target)
  tagged <- tryCatch({
    sf::gdal_utils("translate", tmp, target, quiet = TRUE,
                   options = c("-of", "GTiff", "-stats", "-co", "COMPRESS=LZW",
                               geotiff_tag_options(tags)))
    file.exists(target)
  }, error = function(e) FALSE)
  if (!tagged) {
    if (!file.copy(tmp, target, overwrite = TRUE)) stop("Could not write the GeoTIFF.")
    warning("GeoTIFF statistics and tags could not be written; the file holds the raster values only.",
            call. = FALSE)
  }
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
#' The Leaflet measure control works on a sphere (up to ~0.5% off) and knows no
#' CRS, so both figures are recomputed from the WGS84 vertices:
#'   - GEODESIC length on the WGS84 ellipsoid (terra/GeographicLib);
#'   - PLANAR length in the Target Mapping CRS, the CRS of exported rasters
#'     (the variogram lags' metric only when it is also the working CRS).
#' A wide gap between the two flags a CRS that suits the area poorly; a
#' geographic Target CRS gives no planar figure.
#'
#' From three vertices the path closes into a ring: lengths become perimeters
#' and areas are reported on both bases. A self-crossing ring gets no area (its
#' lobes cancel; a symmetric bowtie gives zero), while its perimeter stays exact.
#'
#' @param lonlat Two-column matrix of clicked WGS84 vertices (lon, lat).
#' @param proj_crs Target Mapping CRS; NULL or geographic leaves the projected
#'   fields NA.
#' @return List with `n_points`, `closed`, `self_intersecting`,
#'   `length_geodesic`, `length_projected` (m), `area_geodesic`,
#'   `area_projected` (m^2) and `crs_label`. Unmeasurable quantities are NA.
measure_path_metrics <- function(lonlat, proj_crs = NULL) {
  out <- list(n_points = 0L, closed = FALSE, self_intersecting = FALSE,
              length_geodesic = NA_real_, length_projected = NA_real_,
              area_geodesic = NA_real_, area_projected = NA_real_,
              crs_label = NA_character_)
  if (is.null(lonlat)) return(out)
  lonlat <- suppressWarnings(matrix(as.numeric(as.matrix(lonlat)), ncol = 2))
  lonlat <- lonlat[stats::complete.cases(lonlat), , drop = FALSE]
  # Drop a vertex placed on its predecessor (a zero-length segment).
  if (nrow(lonlat) > 1) {
    lonlat <- lonlat[c(TRUE, rowSums(abs(diff(lonlat))) > 0), , drop = FALSE]
  }
  # An already-closed ring: the repeated first vertex is not a click.
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
#' Tested combinatorially on the lon/lat corners, independent of the Target CRS
#' and sf_use_s2(). Only proper crossings count; edges touching at a vertex do
#' not.
#'
#' @param xy Two-column matrix of corners WITHOUT the repeated closing vertex.
ring_self_intersects <- function(xy) {
  n <- nrow(xy)
  if (is.null(n) || n < 4) return(FALSE)  # a triangle cannot cross itself
  nxt <- function(i) if (i == n) 1L else i + 1L
  side <- function(o, a, b) (a[1] - o[1]) * (b[2] - o[2]) - (a[2] - o[2]) * (b[1] - o[1])
  for (i in seq_len(n - 1L)) {
    for (j in seq(i + 1L, n)) {
      # Adjacent edges (last and first included) share a vertex by construction.
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
#' GDAL's KML driver keeps only <name> and <description> (LIBKML, which keeps
#' every field, is absent from sf's Windows GDAL), so the label goes to <name>
#' and the whole record to <description>. NA prints empty: the open outer
#' breaks are unbounded, not failed.
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

#' Write an sf layer in one of the four offered GIS formats (drawn polygons and
#' class zones). KML and GeoJSON are WGS84 by specification; Shapefile and
#' GeoPackage keep the analysis CRS.
write_vector_export <- function(sf_obj, file, fmt, layer_name = "layer") {
  if (is.null(sf_obj) || nrow(sf_obj) == 0) stop("Nothing to export.")

  if (fmt %in% c("kml", "geojson")) {
    crs_in <- sf::st_crs(sf_obj)
    if (!is.na(crs_in) && crs_in != sf::st_crs(4326)) {
      sf_obj <- sf::st_transform(sf_obj, 4326)
    }
  }

  if (fmt == "shp") {
    # A shapefile travels as a zip; a per-call directory stops two exports with
    # one name from deleting each other's files.
    temp_dir <- tempfile("vec_export_")
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
#' Map Viewer images have positional ids (`rast_img_1`, ...); Leaflet replaces
#' a re-added id but never drops one the new pass omits. `n_prev` and `n_now`
#' are the image counts of the previous and current pass.
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
