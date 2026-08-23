# test-crs-validation.R — tests for validate_crs from monolith.R.
# NOTE: validate_crs returns an sf crs object (S3 class 'crs', which is a list),
# not a character string. It uses showNotification on error which requires a
# running Shiny session.

test_that("validate_crs accepts EPSG code string and returns crs object", {
  result <- validate_crs("EPSG:4326")
  expect_s3_class(result, "crs")
})

test_that("validate_crs accepts WKT string and returns crs object", {
  wkt <- 'GEOGCS["WGS 84",DATUM["WGS_1984",SPHEROID["WGS 84",6378137,298.257223563]],PRIMEM["Greenwich",0],UNIT["degree",0.0174532925199433]]'
  result <- validate_crs(wkt)
  expect_s3_class(result, "crs")
})

test_that("validate_crs accepts proj4 string and returns crs object", {
  result <- validate_crs("+proj=longlat +datum=WGS84 +no_defs")
  expect_s3_class(result, "crs")
})

test_that("validate_crs errors on invalid CRS string", {
  expect_error(
    validate_crs("not_a_valid_crs_string_xyz"),
    NULL
  )
})

test_that("validate_crs errors on empty string", {
  expect_error(
    validate_crs(""),
    NULL
  )
})

test_that("validate_crs EPSG:32633 returns a projected CRS", {
  result <- validate_crs("EPSG:32633")
  expect_s3_class(result, "crs")
  # UTM zone 33N is a projected CRS
  expect_false(sf::st_is_longlat(result))
})

# ── Metric axis units (Target Mapping CRS) ─────────────────────────────────
# Every distance the app accepts or prints is stated in metres while the
# engines work on the CRS's own axis units. That identity has to be enforced,
# not assumed: a CRS in feet would silently rescale the grid, the buffers and
# every variogram range by 3.28.

test_that("crs_metre_factor resolves the linear unit of a projected CRS", {
  expect_equal(crs_metre_factor("EPSG:32635"), 1)
  expect_equal(crs_metre_factor("EPSG:27700"), 1)   # OSGB National Grid
  expect_equal(crs_metre_factor("EPSG:3857"), 1)
  # NAD83 / New York Long Island, in US survey feet
  expect_equal(crs_metre_factor("EPSG:2263"), 1200 / 3937, tolerance = 1e-9)
  expect_equal(crs_metre_factor("+proj=tmerc +lat_0=0 +lon_0=0 +k=1 +x_0=0 +y_0=0 +datum=WGS84 +units=ft +no_defs"),
               0.3048, tolerance = 1e-9)
})

test_that("crs_metre_factor returns NA where the question does not apply", {
  # A geographic CRS has no linear axis unit; an unparseable one has no answer.
  expect_true(is.na(crs_metre_factor("EPSG:4326")))
  expect_true(is.na(crs_metre_factor("+proj=longlat +datum=WGS84 +no_defs")))
  expect_true(is.na(crs_metre_factor("not_a_crs_xyz")))
  expect_true(is.na(crs_metre_factor(NULL)))
})

test_that("require_metric refuses a projected CRS on another linear unit", {
  # showNotification outside a session turns the refusal into an error, which
  # is how the other validate_crs failure paths are asserted in this file.
  expect_error(validate_crs("EPSG:2263", require_metric = TRUE))
  expect_error(validate_crs("+proj=tmerc +lat_0=0 +lon_0=0 +k=1 +x_0=0 +y_0=0 +datum=WGS84 +units=ft +no_defs",
                            require_metric = TRUE))
  # The same CRSs are perfectly valid as an INPUT CRS: the pipeline projects
  # out of that one before measuring anything.
  expect_s3_class(validate_crs("EPSG:2263"), "crs")
})

test_that("require_metric passes metric and geographic CRSs untouched", {
  expect_s3_class(validate_crs("EPSG:32635", require_metric = TRUE), "crs")
  expect_s3_class(validate_crs("EPSG:27700", require_metric = TRUE), "crs")
  # Geographic is allowed: validate_and_project_sf() projects it to a metric
  # UTM zone itself, so the metre contract still holds downstream.
  expect_s3_class(validate_crs("EPSG:4326", require_metric = TRUE), "crs")
})

test_that("the run gate commits its CRS and demands a metric one", {
  root <- normalizePath(file.path(testthat::test_path(), "..", ".."), mustWork = TRUE)
  exec <- paste(readLines(file.path(root, "server_execution.R"), warn = FALSE), collapse = "\n")
  expect_match(exec, "require_metric = TRUE", fixed = TRUE)
  # The displayed run must carry the CRS it was computed in, or the ruler has
  # nothing to name but the live sidebar.
  expect_match(exec, "crs_sel = input$crs_selection", fixed = TRUE)
})

# ── calc_metric_spacing ────────────────────────────────────────────────────
# Backs the grid-resolution suggestion: must return metres for BOTH projected
# and geographic analysis CRSs (the grid_res slider is metric and
# interpolation always runs in a projected CRS).

test_that("calc_metric_spacing returns NA for NULL or single-point input", {
  expect_true(is.na(calc_metric_spacing(NULL)$mean_nn))
  pts1 <- sf::st_as_sf(data.frame(x = 500000, y = 5000000),
                       coords = c("x", "y"), crs = 32635)
  expect_true(is.na(calc_metric_spacing(pts1)$mean_nn))
})

test_that("calc_metric_spacing measures projected coordinates directly", {
  # 3 points 100 m apart on a line in UTM 35N
  pts <- sf::st_as_sf(data.frame(x = c(500000, 500100, 500200), y = 5500000),
                      coords = c("x", "y"), crs = 32635)
  sp <- calc_metric_spacing(pts)
  expect_equal(sp$mean_nn, 100, tolerance = 1e-9)
  expect_equal(sp$max_dim, 200, tolerance = 1e-9)
})

test_that("calc_metric_spacing converts geographic coordinates to metres", {
  # Points 0.001 deg longitude apart at ~50N:
  # true spacing ~ 111320 * cos(50 deg) * 0.001 ~ 71.6 m
  pts <- sf::st_as_sf(
    data.frame(x = seq(14, by = 0.001, length.out = 5), y = 50),
    coords = c("x", "y"), crs = 4326
  )
  sp <- calc_metric_spacing(pts)
  expected <- 111320 * cos(50 * pi / 180) * 0.001
  expect_equal(sp$mean_nn, expected, tolerance = 0.02)
  # crucially metres, not degrees
  expect_gt(sp$mean_nn, 1)
  expect_gt(sp$max_dim, 4 * sp$mean_nn * 0.98)
})

# ── measure_path_metrics (Map Viewer ruler) ────────────────────────────────
# Two bases, deliberately: the geodesic length is the ground distance and the
# projected length is the metric the engines work in. Neither may be quietly
# substituted for the other, and a geographic analysis CRS must produce no
# planar figure at all (a length in degrees is meaningless).

test_that("measure_path_metrics reproduces the WGS84 geodesic length", {
  # One degree of latitude along the prime meridian: 110574.389 m on WGS84
  # (GeographicLib). A spherical approximation gives 111195 m, i.e. 0.56% high,
  # so this tolerance separates the two.
  res <- measure_path_metrics(matrix(c(0, 0, 0, 1), ncol = 2, byrow = TRUE))
  expect_equal(res$length_geodesic, 110574.389, tolerance = 1e-5)
  expect_equal(res$n_points, 2L)
})

test_that("measure_path_metrics reports the projected length in the analysis CRS", {
  ll <- matrix(c(35.000, 39.000,
                 35.010, 39.008,
                 35.020, 39.000), ncol = 2, byrow = TRUE)
  res <- measure_path_metrics(ll, "EPSG:32636")
  expect_equal(res$crs_label, "EPSG:32636")
  # Both bases measure the same path: they agree to well under 0.1% in UTM,
  # and the difference IS the projection's distance distortion.
  expect_equal(res$length_projected, res$length_geodesic, tolerance = 1e-3)
  expect_false(isTRUE(all.equal(res$length_projected, res$length_geodesic,
                                tolerance = 1e-9)))
  # Both must be the whole shape, not one segment
  seg <- measure_path_metrics(ll[1:2, ])$length_geodesic
  expect_gt(res$length_geodesic, 2 * seg)
})

test_that("measure_path_metrics closes the ring from the third vertex", {
  ll <- matrix(c(35.000, 39.000,
                 35.010, 39.008,
                 35.020, 39.000), ncol = 2, byrow = TRUE)
  two <- measure_path_metrics(ll[1:2, ], "EPSG:32636")
  expect_false(two$closed)

  three <- measure_path_metrics(ll, "EPSG:32636")
  expect_true(three$closed)
  # A perimeter, not an open path: the length must include the closing leg, so
  # that it describes the same ring the reported area does.
  legs <- vapply(list(ll[1:2, ], ll[2:3, ], ll[c(3, 1), ]),
                 function(p) measure_path_metrics(p)$length_geodesic, numeric(1))
  expect_equal(three$length_geodesic, sum(legs), tolerance = 1e-6)
  expect_gt(three$length_geodesic, sum(legs[1:2]))

  # Same closure in the analysis CRS
  xy <- sf::st_coordinates(sf::st_transform(
    sf::st_sfc(sf::st_multipoint(ll), crs = 4326), "EPSG:32636"))[, 1:2]
  ring <- rbind(xy, xy[1, ])
  expect_equal(three$length_projected,
               sum(sqrt(rowSums(diff(ring)^2))), tolerance = 1e-6)
})

test_that("measure_path_metrics leaves the planar fields NA for a geographic CRS", {
  ll <- matrix(c(35.000, 39.000, 35.010, 39.008, 35.020, 39.000),
               ncol = 2, byrow = TRUE)
  for (crs in list(NULL, "", "EPSG:4326")) {
    res <- measure_path_metrics(ll, crs)
    expect_true(is.na(res$length_projected))
    expect_true(is.na(res$area_projected))
    expect_true(is.na(res$crs_label))
    expect_true(is.finite(res$length_geodesic))  # the ground figure always stands
  }
})

test_that("measure_path_metrics reports area only from three vertices", {
  ll <- matrix(c(35.000, 39.000, 35.010, 39.008, 35.020, 39.000),
               ncol = 2, byrow = TRUE)
  two <- measure_path_metrics(ll[1:2, ], "EPSG:32636")
  expect_true(is.na(two$area_geodesic))
  expect_true(is.na(two$area_projected))

  three <- measure_path_metrics(ll, "EPSG:32636")
  expect_true(is.finite(three$area_geodesic))
  # Shoelace area of the closed triangle in UTM, computed independently
  xy <- sf::st_coordinates(sf::st_transform(
    sf::st_sfc(sf::st_multipoint(ll), crs = 4326), "EPSG:32636"))[, 1:2]
  shoelace <- abs(sum(xy[, 1] * xy[c(2, 3, 1), 2] - xy[c(2, 3, 1), 1] * xy[, 2])) / 2
  expect_equal(three$area_projected, shoelace, tolerance = 1e-6)
  expect_equal(three$area_geodesic, three$area_projected, tolerance = 1e-3)
})

test_that("measure_path_metrics degrades instead of erroring on unusable input", {
  expect_equal(measure_path_metrics(NULL)$n_points, 0L)
  expect_true(is.na(measure_path_metrics(NULL)$length_geodesic))
  one <- measure_path_metrics(matrix(c(35, 39), ncol = 2))
  expect_equal(one$n_points, 1L)
  expect_true(is.na(one$length_geodesic))
  # Incomplete vertices are dropped, not measured as zero
  with_na <- measure_path_metrics(matrix(c(35, 39, NA, NA, 35.01, 39.008),
                                         ncol = 2, byrow = TRUE))
  expect_equal(with_na$n_points, 2L)
  expect_true(is.finite(with_na$length_geodesic))
})

test_that("measure_path_metrics counts a closed ring by its corners", {
  # Whoever sends the vertices may already have closed the ring; the repeated
  # first vertex is closure, not a click, and must not inflate the count or
  # leave a zero-length segment in the perimeter.
  ll <- matrix(c(35.000, 39.000, 35.010, 39.008, 35.020, 39.000),
               ncol = 2, byrow = TRUE)
  open <- measure_path_metrics(ll, "EPSG:32636")
  ring <- measure_path_metrics(rbind(ll, ll[1, ]), "EPSG:32636")
  expect_equal(ring$n_points, 3L)
  expect_equal(ring$length_geodesic, open$length_geodesic)
  expect_equal(ring$area_geodesic, open$area_geodesic)
})

test_that("measure_path_metrics drops a vertex placed on its predecessor", {
  a <- c(35.000, 39.000); b <- c(35.010, 39.008)
  plain <- measure_path_metrics(matrix(c(a, b), ncol = 2, byrow = TRUE))
  doubled <- measure_path_metrics(matrix(c(a, a, b), ncol = 2, byrow = TRUE))
  expect_equal(doubled$n_points, 2L)
  expect_equal(doubled$length_geodesic, plain$length_geodesic)
})

# ── Self-intersecting rings ────────────────────────────────────────────────
# The shoelace sum both engines use returns the DIFFERENCE of a figure-eight's
# lobes, not the area drawn, so that number must not be reported. The
# perimeter is unaffected by the crossing and is kept.

test_that("ring_self_intersects flags a crossing and nothing else", {
  square <- matrix(c(0, 0, 1, 0, 1, 1, 0, 1), ncol = 2, byrow = TRUE)
  expect_false(ring_self_intersects(square))
  # Same four corners, two of them swapped: a bowtie
  bowtie <- matrix(c(0, 0, 1, 0, 0, 1, 1, 1), ncol = 2, byrow = TRUE)
  expect_true(ring_self_intersects(bowtie))
  # A triangle has no pair of non-adjacent edges to cross
  expect_false(ring_self_intersects(square[1:3, ]))
  expect_false(ring_self_intersects(square[1:2, ]))
  # Concave but simple: a chevron must not be flagged
  chevron <- matrix(c(0, 0, 2, 0, 2, 2, 1, 0.5, 0, 2), ncol = 2, byrow = TRUE)
  expect_false(ring_self_intersects(chevron))
})

test_that("measure_path_metrics withholds the area of a crossed path", {
  # A symmetric bowtie: the two lobes cancel exactly, so the shoelace area
  # would be reported as ~0 rather than as the shape drawn on the map.
  bow <- matrix(c(35.000, 39.000,
                  35.020, 39.000,
                  35.000, 39.015,
                  35.020, 39.015), ncol = 2, byrow = TRUE)
  res <- measure_path_metrics(bow, "EPSG:32636")
  expect_true(res$closed)
  expect_true(res$self_intersecting)
  expect_equal(res$n_points, 4L)
  expect_true(is.na(res$area_geodesic))
  expect_true(is.na(res$area_projected))
  # The perimeter is still the sum of the four edges of the ring drawn
  legs <- vapply(list(bow[1:2, ], bow[2:3, ], bow[3:4, ], bow[c(4, 1), ]),
                 function(p) measure_path_metrics(p)$length_geodesic, numeric(1))
  expect_equal(res$length_geodesic, sum(legs), tolerance = 1e-6)
  expect_true(is.finite(res$length_projected))

  # The same four corners in simple order keep their area
  simple <- measure_path_metrics(bow[c(1, 2, 4, 3), ], "EPSG:32636")
  expect_false(simple$self_intersecting)
  expect_true(is.finite(simple$area_geodesic))
  expect_true(is.finite(simple$area_projected))
})

# ── Ruler presentation ─────────────────────────────────────────────────────

test_that("ruler formatters keep metres and hectares as the leading unit", {
  expect_equal(format_measure_length(248.4), "248.4 m")
  expect_match(format_measure_length(2481.204), "^2,481\\.2 m \\(2\\.481 km\\)$")
  expect_match(format_measure_area(769309.1), "^76\\.93 ha")
  expect_match(format_measure_area(4321.5), "^4321\\.5 m")
  expect_equal(format_measure_length(NA_real_), "n/a")
  expect_equal(format_measure_area(NA_real_), "n/a")
  expect_equal(format_measure_length(Inf), "n/a")
})

test_that("map_ruler_popup_html names the basis of every figure it prints", {
  ll <- matrix(c(35.000, 39.000, 35.010, 39.008, 35.020, 39.000),
               ncol = 2, byrow = TRUE)
  html <- map_ruler_popup_html(measure_path_metrics(ll, "EPSG:32636"))
  # A ring reports a perimeter, and says so: "length" beside an area would read
  # as the open path the area does not describe.
  expect_match(html, "Perimeter, ground \\(WGS84\\)")
  expect_match(html, "Perimeter, projected \\(EPSG:32636\\)")
  expect_false(grepl("Length", html, fixed = TRUE))
  expect_match(html, "Area, ground \\(WGS84\\)")
  expect_match(html, "Area, projected \\(EPSG:32636\\)")
  expect_match(html, "3 points")

  # Two vertices are an open line: a length, and no area at all
  line <- map_ruler_popup_html(measure_path_metrics(ll[1:2, ], "EPSG:32636"))
  expect_match(line, "Length, ground \\(WGS84\\)")
  expect_false(grepl("Perimeter", line, fixed = TRUE))
  expect_false(grepl("Area", line, fixed = TRUE))

  # The task links add_map_ruler() wires must be in the markup it wires them in
  expect_match(html, "mono-ruler-zoom")
  expect_match(html, "mono-ruler-delete")

  # No analysis CRS: the ground figure alone, never an unlabelled number
  geo_only <- map_ruler_popup_html(measure_path_metrics(ll))
  expect_match(geo_only, "Perimeter, ground \\(WGS84\\)")
  expect_false(grepl("projected", geo_only))

  # Nothing to report below two vertices
  expect_null(map_ruler_popup_html(measure_path_metrics(ll[1, , drop = FALSE])))
  expect_null(map_ruler_popup_html(NULL))
})

test_that("map_ruler_popup_html explains a withheld area instead of dropping it", {
  bow <- matrix(c(35.000, 39.000, 35.020, 39.000,
                  35.000, 39.015, 35.020, 39.015), ncol = 2, byrow = TRUE)
  html <- map_ruler_popup_html(measure_path_metrics(bow, "EPSG:32636"))
  # Silence would read as a failure of the tool rather than a property of the
  # shape, and the heading must not promise a figure that is not there.
  expect_match(html, "crosses itself")
  expect_false(grepl("Area,", html, fixed = TRUE))
  expect_false(grepl("Area measurement", html, fixed = TRUE))
  expect_match(html, "Closed path")
  # The perimeter is exact and stays
  expect_match(html, "Perimeter, ground \\(WGS84\\)")
  expect_match(html, "Perimeter, projected \\(EPSG:32636\\)")
})

test_that("the ruler stylesheet is scoped, outranks the plugin, and keeps the icons", {
  css <- map_ruler_css()
  blocks <- trimws(strsplit(css, "}", fixed = TRUE)[[1]])
  blocks <- blocks[nzchar(blocks)]
  sels <- trimws(unlist(strsplit(sub("\\{.*$", "", blocks), ",", fixed = TRUE)))
  sels <- sels[nzchar(sels)]

  # The plugin styles its control and its result popup through shared
  # selectors, so an unscoped rule here would strip the popup too.
  expect_true(all(grepl(".leaflet-control-measure", sels, fixed = TRUE)))
  expect_false(any(grepl("leaflet-measure-resultpopup", sels, fixed = TRUE)))

  # The plugin's stylesheet arrives as a widget dependency and therefore loads
  # AFTER this block, so the toggle rules can only win on specificity: the
  # element qualifier is what makes them stick, in touch mode as well.
  toggle <- sels[grepl("measure-toggle", sels, fixed = TRUE)]
  expect_gt(length(toggle), 0)
  expect_true(all(grepl("a.leaflet-control-measure-toggle", toggle, fixed = TRUE)))
  expect_true(any(grepl("leaflet-touch", toggle, fixed = TRUE)))

  # Cancel / Finish lose their label text but keep the plugin's icon, which is
  # drawn as a background image inside the left padding: display:none or a
  # zeroed padding would take the icon with the text.
  task <- blocks[grepl("js-measuretasks a.cancel", blocks, fixed = TRUE)]
  expect_length(task, 1)
  expect_match(task, "padding-left: 18px")
  expect_match(task, "overflow: hidden")
  expect_false(grepl("display: none", task, fixed = TRUE))
})

test_that("ui_main.R injects the ruler stylesheet", {
  # A stylesheet nothing includes is worse than none: the control would look
  # correct in the unit tests above and oversized in the app.
  root <- normalizePath(file.path(testthat::test_path(), "..", ".."), mustWork = TRUE)
  ui_main <- paste(readLines(file.path(root, "ui_main.R"), warn = FALSE), collapse = "\n")
  expect_match(ui_main, "map_ruler_css()", fixed = TRUE)
})

test_that("the ruler observer answers on the channel the widget listens on", {
  # The readout is a round trip between two files. A handler name that drifted
  # on either side would leave every measurement stuck on its placeholder, and
  # no unit test of either half alone can see that.
  root <- normalizePath(file.path(testthat::test_path(), "..", ".."), mustWork = TRUE)
  viewer <- paste(readLines(file.path(root, "server_map_viewer.R"), warn = FALSE),
                  collapse = "\n")
  expect_match(viewer, "sendCustomMessage(\"monolith_ruler_result\"", fixed = TRUE)
  expect_match(viewer, "map_ruler_popup_html", fixed = TRUE)

  # The projected figure must name the CRS of the run ON SCREEN. Reading the
  # live sidebar would relabel an old measurement the moment the sidebar is
  # retargeted, against the very surface it was taken on.
  expect_match(viewer, "rv$disp$crs_sel %||% input$crs_selection", fixed = TRUE)
  expect_match(viewer, "measure_path_metrics(cbind(lon, lat), ruler_crs)", fixed = TRUE)
})

test_that("add_map_ruler puts the measure control on the widget", {
  m <- add_map_ruler(leaflet::leaflet())
  expect_s3_class(m, "leaflet")
  methods <- vapply(m$x$calls, function(c) c$method, character(1))
  expect_true("addMeasure" %in% methods)
  opts <- m$x$calls[[which(methods == "addMeasure")[1]]]$args[[1]]
  # Metric units: the plugin defaults to feet and acres
  expect_equal(opts$primaryLengthUnit, "meters")
  expect_equal(opts$primaryAreaUnit, "hectares")
  # Clear of the top-left drawing stack
  expect_equal(opts$position, "bottomleft")
  # The JS hook that feeds the R-side readout must travel with it
  hook <- paste(unlist(m$jsHooks), collapse = "")
  expect_true(grepl("measurefinish", hook, fixed = TRUE))
  # Cancel fires measurefinish too, so the result shape - the layeradd that
  # follows a real finish - is what triggers the round trip
  expect_true(grepl("layeradd", hook, fixed = TRUE))
  expect_true(grepl("token", hook, fixed = TRUE))
  # The reply lands in the shape's own popup, through the handler name the
  # ruler observer in server_map_viewer.R sends to
  expect_true(grepl("monolith_ruler_result", hook, fixed = TRUE))
  expect_true(grepl("setPopupContent", hook, fixed = TRUE))
  expect_true(grepl("mono-ruler-delete", hook, fixed = TRUE))
  # The control is lifted clear of the tile attribution, which wraps to two or
  # three lines on a wordy provider and would otherwise cover it
  expect_true(grepl("leaflet-control-attribution", hook, fixed = TRUE))
  expect_true(grepl("marginBottom", hook, fixed = TRUE))
  # Hidden label text is not an accessible name, so the icon-only Cancel /
  # Finish links get a real one (map_ruler_css strips their text)
  expect_true(grepl("aria-label", hook, fixed = TRUE))
  expect_true(grepl("Finish measurement", hook, fixed = TRUE))
})

# ── Input-CRS identification (Phase A of the 2026-08-22 placement fix) ──────
# A UTM easting/northing pair is equally valid in all 60 zones, so the app must
# never assume one. It identifies the CRS only from evidence carried by the
# upload, and otherwise asks.

#' UTM 33N fixture over Potsdam: projected x/y plus the lon/lat they came from.
make_utm33_fixture <- function(n = 15, seed = 1) {
  set.seed(seed)
  lon <- 12.9546 + stats::runif(n, 0, 0.013)
  lat <- 52.4635 + stats::runif(n, 0, 0.005)
  p <- sf::st_transform(
    sf::st_as_sf(data.frame(lon = lon, lat = lat), coords = c("lon", "lat"), crs = 4326), 32633)
  xy <- sf::st_coordinates(p)
  data.frame(x = xy[, 1], y = xy[, 2], lon = lon, lat = lat, v = seq_len(n))
}

test_that("normalize_crs_input promotes a bare EPSG number and leaves the rest alone", {
  expect_equal(normalize_crs_input("32633"), "EPSG:32633")
  expect_equal(normalize_crs_input(" 25832 "), "EPSG:25832")
  expect_equal(normalize_crs_input(32633), "EPSG:32633")
  # Already parseable, or not an EPSG code at all: untouched.
  expect_equal(normalize_crs_input("EPSG:32633"), "EPSG:32633")
  expect_equal(normalize_crs_input("+proj=utm +zone=33 +datum=WGS84"),
               "+proj=utm +zone=33 +datum=WGS84")
  expect_equal(normalize_crs_input("not_a_crs"), "not_a_crs")
  expect_null(normalize_crs_input(NULL))

  # The promotion is what makes the free-text route work: sf rejects the bare
  # number outright.
  expect_true(is.na(suppressWarnings(tryCatch(sf::st_crs("32633"), error = function(e) NA))))
  expect_false(is.na(sf::st_crs(normalize_crs_input("32633"))))
})

test_that("find_geographic_pair locates a companion lon/lat pair, or reports none", {
  fx <- make_utm33_fixture()
  pair <- find_geographic_pair(fx, exclude = c("x", "y"))
  expect_equal(pair$lon_col, "lon")
  expect_equal(pair$lat_col, "lat")

  # No such heading
  expect_null(find_geographic_pair(fx[, c("x", "y", "v")], exclude = c("x", "y")))
  # Whole-name matching only: a variable merely containing "lat" is not one
  df <- data.frame(x = 1, y = 2, Lateral_flow = 3, Longevity_index = 4)
  expect_null(find_geographic_pair(df, exclude = c("x", "y")))
  # Present but not degree magnitudes
  bad <- fx; bad$lon <- bad$lon * 1000
  expect_null(find_geographic_pair(bad, exclude = c("x", "y")))
})

test_that("crs_zone_candidates fixes the zone from a longitude and stays short", {
  expect_equal(crs_zone_candidates(12.96), c(32633, 32733, 25833, 3857))
  expect_equal(crs_zone_candidates(-75), c(32618, 32718, 25818, 3857))
  expect_equal(crs_zone_candidates(179.9)[1], 32660)
  expect_equal(crs_zone_candidates(-179.9)[1], 32601)
})

test_that("crs_collapse_candidates folds datum twins into one answer", {
  fx <- make_utm33_fixture()
  # WGS84 UTM 33N and its ETRS89 twin describe the same grid to under a metre;
  # left separate they tie on every score and defeat the discrimination tests.
  grp <- crs_collapse_candidates(fx$x, fx$y, c(32633, 32733, 25833, 3857))
  expect_equal(grp$cands, c(32633, 32733, 3857))
  expect_equal(grp$equivalent[[1]], 25833)
  # How far the folded members sit from their representative, so a caller can
  # state the size of the difference instead of implying there is none.
  expect_length(grp$spread_m, 3L)
  expect_lt(grp$spread_m[1], 5)
  expect_equal(grp$spread_m[2], 0)

  # Supplying the already computed landing positions must give the SAME
  # grouping - it only skips a second transform pass.
  pos <- do.call(rbind, lapply(c(32633, 32733, 25833, 3857), function(e) {
    lp <- crs_landing_position(data.frame(.x = fx$x, .y = fx$y), ".x", ".y", paste0("EPSG:", e))
    data.frame(lon = lp$lon, lat = lp$lat)
  }))
  fast <- crs_collapse_candidates(fx$x, fx$y, c(32633, 32733, 25833, 3857), pos = pos)
  expect_equal(fast$cands, grp$cands)
  expect_equal(fast$equivalent, grp$equivalent)
  expect_equal(fast$spread_m, grp$spread_m, tolerance = 1e-6)
})

test_that("Tier 2 identifies EPSG:32633 from a companion lon/lat pair", {
  fx <- make_utm33_fixture()
  hit <- identify_crs_from_lonlat(fx$x, fx$y, fx$lon, fx$lat)
  expect_equal(hit$crs, "EPSG:32633")
  expect_lt(hit$residual, 0.01)
  # The wrong-zone alternative is hundreds of kilometres away: decisive.
  expect_gt(hit$runner_up, 1e5)
  expect_equal(hit$equivalent, 25833)

  full <- identify_input_crs(fx, "x", "y")
  expect_equal(full$crs, "EPSG:32633")
  expect_equal(full$evidence, "lonlat")
  expect_match(full$message, "EPSG:32633", fixed = TRUE)
})

test_that("Tier 2 returns NULL - no guess - when the file carries no evidence", {
  fx <- make_utm33_fixture()
  expect_null(identify_input_crs(fx[, c("x", "y", "v")], "x", "y"))
})

test_that("Tier 2 refuses to decide when the best candidate is not decisively best", {
  fx <- make_utm33_fixture()
  # Shift the reference pair ~1 km away from every candidate.
  off <- fx; off$lat <- off$lat + 0.01

  # Nothing is within the default 5 m tolerance.
  expect_null(identify_crs_from_lonlat(off$x, off$y, off$lon, off$lat))

  # Loosen the tolerance past the 1 km error and the winner is accepted, so
  # the refusal above is the tolerance and not an accident of the fixture.
  ok <- identify_crs_from_lonlat(off$x, off$y, off$lon, off$lat, tol_m = 2000)
  expect_equal(ok$crs, "EPSG:32633")

  # Same data, but now demanding the runner-up be 10000x worse: it is only
  # ~1000x worse, so the identifier declines rather than guessing.
  expect_null(identify_crs_from_lonlat(off$x, off$y, off$lon, off$lat,
                                       tol_m = 2000, ratio = 1e4))
})

test_that("Tier 1 recognises degrees without any other evidence", {
  fx <- make_utm33_fixture()
  deg <- data.frame(x = fx$lon, y = fx$lat)
  hit <- identify_input_crs(deg, "x", "y")
  expect_equal(hit$crs, "EPSG:4326")
  expect_equal(hit$evidence, "degrees")
})

test_that("Tier 2B identifies the CRS from an uploaded boundary", {
  fx <- make_utm33_fixture()
  pts <- sf::st_as_sf(fx, coords = c("x", "y"), crs = 32633)
  bnd <- sf::st_sf(geometry = sf::st_as_sfc(sf::st_bbox(sf::st_buffer(sf::st_as_sfc(sf::st_bbox(pts)), 500))))
  sf::st_crs(bnd) <- 32633

  hit <- identify_crs_from_boundary(fx$x, fx$y, bnd)
  expect_equal(hit$crs, "EPSG:32633")
  expect_equal(hit$fraction, 1)

  # A boundary with no CRS proves nothing.
  bnd_nocrs <- bnd; sf::st_crs(bnd_nocrs) <- NA
  expect_null(identify_crs_from_boundary(fx$x, fx$y, bnd_nocrs))
  expect_null(identify_crs_from_boundary(fx$x, fx$y, NULL))

  # It is the evidence of last resort: reached only when no lon/lat pair exists.
  full <- identify_input_crs(fx[, c("x", "y", "v")], "x", "y", boundary = bnd)
  expect_equal(full$crs, "EPSG:32633")
  expect_equal(full$evidence, "boundary")
})

test_that("crs_landing_position states where the data lands, in degrees", {
  fx <- make_utm33_fixture()
  right <- crs_landing_position(fx, "x", "y", "EPSG:32633")
  wrong <- crs_landing_position(fx, "x", "y", "EPSG:32635")
  expect_equal(right$lon, 12.96, tolerance = 1e-2)
  expect_equal(wrong$lon, 24.96, tolerance = 1e-2)
  # Two zones east is exactly 12 degrees: the 812 km misplacement of the
  # 2026-08-22 incident, and the number this readout puts on screen.
  expect_equal(wrong$lon - right$lon, 12, tolerance = 1e-6)
  expect_match(right$text, "12.9", fixed = TRUE)
  expect_match(right$text, "N", fixed = TRUE)

  # Unset CRS: nothing to report, and no error.
  expect_null(crs_landing_position(fx, "x", "y", ""))
  expect_null(crs_landing_position(fx, "x", "y", NULL))
  expect_null(crs_landing_position(fx, "x", "y", "not_a_crs"))
})

test_that("format_lonlat names the hemisphere", {
  expect_match(format_lonlat(12.9584, 52.4662), "^12\\.958.*E, 52\\.466.*N$")
  expect_match(format_lonlat(-70.1, -33.4), "^70\\.100.*W, 33\\.400.*S$")
})

test_that("neither CRS selector carries a default zone", {
  root <- normalizePath(file.path(testthat::test_path(), "..", ".."), mustWork = TRUE)
  tabs <- paste(readLines(file.path(root, "ui_main_tabs.R"), warn = FALSE), collapse = "\n")
  setup <- paste(readLines(file.path(root, "server_setup.R"), warn = FALSE), collapse = "\n")

  # The root cause of the incident: a hardcoded zone silently georeferenced
  # every user's data into the Turkey/Ukraine/Belarus longitude band.
  expect_false(grepl('selected = "EPSG:', tabs, fixed = TRUE))
  expect_match(tabs, 'selectizeInput("map_crs", "Input Data CRS", choices = common_crs_input, selected = ""',
               fixed = TRUE)
  expect_match(tabs, 'selectizeInput("crs_selection", "Target Mapping CRS", choices = common_crs_target, selected = ""',
               fixed = TRUE)
  expect_match(tabs, 'uiOutput("crs_landing_note")', fixed = TRUE)
  # rv$mapping$crs must start unset too, or the default returns by the back door
  expect_match(setup, "x = NULL, y = NULL, loc = NULL, crs = NULL", fixed = TRUE)
})

test_that("the setup guards report the landing position and catch swapped axes", {
  root <- normalizePath(file.path(testthat::test_path(), "..", ".."), mustWork = TRUE)
  ds <- paste(readLines(file.path(root, "server_data_setup.R"), warn = FALSE), collapse = "\n")

  expect_match(ds, "identify_input_crs(rv$user_data, input$map_x, input$map_y, rv$shp_bound)",
               fixed = TRUE)
  expect_match(ds, "normalize_crs_input(input$map_crs)", fixed = TRUE)
  expect_match(ds, "normalize_crs_input(input$crs_selection)", fixed = TRUE)
  # Axis swap: X inside +/-90 while Y runs past it but stays inside +/-180
  expect_match(ds, "all(abs(df$x) <= 90) && any(abs(df$y) > 90) && all(abs(df$y) <= 180)",
               fixed = TRUE)
  expect_match(ds, "output$crs_landing_note", fixed = TRUE)
  expect_match(ds, "crs_landing_position(rv$user_data, rv$mapping$x, rv$mapping$y, rv$mapping$crs)",
               fixed = TRUE)
})

test_that("a run is refused VISIBLY until both CRS selectors are set", {
  root <- normalizePath(file.path(testthat::test_path(), "..", ".."), mustWork = TRUE)
  exec <- paste(readLines(file.path(root, "server_execution.R"), warn = FALSE), collapse = "\n")
  ds <- paste(readLines(file.path(root, "server_data_setup.R"), warn = FALSE), collapse = "\n")

  # ONE refusal, called from both gates. Written twice, the copy in the
  # rv$proceed_run observer sat downstream of a req() that had already aborted
  # the run in silence, so the modal could never execute; test-app-smoke.R
  # covers the behaviour end to end.
  expect_match(exec, "crs_selection_gate <- function()", fixed = TRUE)
  expect_equal(length(gregexpr("if (!crs_selection_gate()) return()", exec,
                               fixed = TRUE)[[1]]), 2L)
  expect_match(exec, 'paste(missing, "Not Set")', fixed = TRUE)
  # The observer that raises it must not req() the CRS out from under itself.
  expect_match(exec, "req(rv$user_data, input$locality, rv$mapping$x, rv$mapping$y)",
               fixed = TRUE)
  expect_false(grepl("req(rv$user_data, input$locality, rv$mapping$x, rv$mapping$y, rv$mapping$crs)",
                     exec, fixed = TRUE))
  # Nor may the column mapping be gated behind the CRS: rv$mapping$x would be
  # NULL for as long as the CRS is unset, and every guard that depends on it -
  # the landing-position caption included - would stay silent.
  expect_match(ds, "req(input$map_x, input$map_y, input$map_loc)\n", fixed = TRUE)
})

test_that("every CRS refusal happens before the run commits any state", {
  # A refusal after the commit point leaves the session with the PREVIOUS run's
  # results destroyed: empty Export Registry, empty Map Viewer, and a run-config
  # panel describing a run that never happened.
  root <- normalizePath(file.path(testthat::test_path(), "..", ".."), mustWork = TRUE)
  exec <- readLines(file.path(root, "server_execution.R"), warn = FALSE)
  line_of <- function(pat) {
    hit <- grep(pat, exec, fixed = TRUE)
    expect_length(hit, 1L)
    hit[1]
  }
  commit <- line_of("rv$run_counter <- rv$run_counter + 1L")
  expect_lt(line_of('safe_crs <- validate_crs(input$crs_selection, "CRS Validation Error:"'), commit)
  expect_lt(line_of('safe_src_crs <- validate_crs(rv$mapping$crs, "Input Data CRS Validation Error:"'), commit)
  expect_lt(line_of("crs_target_suitability(input$crs_selection, crs_suit_pos$lon, crs_suit_pos$lat)"), commit)
})

# ── Tier 3: the candidate shortlist (Phase B of the 2026-08-22 fix) ────────
# With no evidence in the file the zone is not recoverable, so the question is
# turned around: the user says where the data is and the app reports which
# projections put it there. The catalogue is PROJ's own proj.db, read through
# named helpers so the suitability gate can inherit the same area-of-use data.

skip_without_registry <- function() {
  testthat::skip_if(is.null(crs_registry()),
                    "PROJ's proj.db is not readable in this build")
}

test_that("the EPSG registry loads from PROJ's own catalogue", {
  skip_without_registry()
  reg <- crs_registry()
  expect_true(all(c("code", "name", "area", "descr",
                    "west", "east", "south", "north") %in% names(reg)))
  expect_type(reg$code, "integer")
  # The whole non-deprecated projected set with a declared area of use, not a
  # curated national-grid list.
  expect_gt(length(unique(reg$code)), 4000)
  expect_true(all(is.finite(reg$west) & is.finite(reg$north)))
  expect_true(all(c(32633, 32635, 27700, 2056, 26914) %in% reg$code))
  # Cached for the session: one read, not one per upload.
  expect_identical(crs_registry(), reg)
})

test_that("the extent filter prunes the catalogue arithmetically, anywhere on earth", {
  skip_without_registry()
  reg <- crs_registry()

  pots <- crs_registry_filter_extent(reg, 12.958, 52.466)
  expect_true(32633 %in% pots$code)
  # Zone 35 declares 24E-30E: it cannot claim Potsdam, which is the whole
  # point - the app default that caused the incident is pruned on sight.
  expect_false(32635 %in% pots$code)
  expect_false(any(duplicated(pots$code)))

  # A point anywhere leaves a shortlist, never the whole catalogue and never
  # nothing. These are the transforms the next stage has to pay for.
  for (p in list(c(12.958, 52.466), c(32.86, 39.93), c(-97.34, 37.69),
                 c(-46.63, -23.55), c(36.82, -1.29), c(115.86, -31.95),
                 c(116.4, 39.9), c(-21.94, 64.15), c(178.4, -18.1))) {
    n <- nrow(crs_registry_filter_extent(reg, p[1], p[2]))
    expect_gt(n, 5)
    expect_lt(n, 100)
  }

  # An extent that crosses the antimeridian (west > east) is a wrapped range,
  # not an empty one.
  wrap <- reg[reg$west > reg$east, , drop = FALSE]
  skip_if(nrow(wrap) == 0, "this PROJ build declares no antimeridian extents")
  w <- wrap[1, ]
  mid_lat <- (w$south + w$north) / 2
  expect_true(w$code %in% crs_registry_filter_extent(reg, w$east - 0.5, mid_lat)$code)
  expect_false(w$code %in% crs_registry_filter_extent(reg, (w$west + w$east) / 2, mid_lat)$code)

  expect_null(crs_registry_filter_extent(reg, NA_real_, 52))
  expect_null(crs_registry_filter_extent(NULL, 12.958, 52.466))
})

test_that("the area-of-use text names the countries a CRS covers, and is capped", {
  skip_without_registry()
  reg <- crs_registry()
  expect_match(reg$descr[reg$code == 32633][1], "Germany", fixed = TRUE)
  expect_false(grepl("Germany", reg$descr[reg$code == 32635][1], fixed = TRUE))

  # Text does NOT bound the search the way a point does, so it has to be
  # capped: "United States" alone matches over a thousand codes, and
  # transforming that many costs tens of seconds.
  expect_gt(nrow(crs_registry_filter_area(reg, "United States", limit = 1e6)), 1000)
  big <- crs_registry_filter_area(reg, "United States", limit = 10)
  expect_equal(nrow(big), 10)
  expect_true(isTRUE(attr(big, "truncated")))
  expect_false(isTRUE(attr(crs_registry_filter_area(reg, "Iceland"), "truncated")))
  expect_null(crs_registry_filter_area(reg, "   "))
})

test_that("preference order picks the representative a user recognises", {
  # crs_collapse_candidates() keeps the first member of a group, so this order
  # decides which of a set of datum siblings is offered by name.
  expect_equal(.crs_preference_order(c("ETRS89 / UTM zone 33N",
                                       "WGS 84 / UTM zone 33N",
                                       "ED50 / UTM zone 33N")),
               c(2L, 1L, 3L))
  # Stable, so with no names to rank by the caller's own order stands - which
  # is how the crs_zone_candidates() fallback keeps its deliberate ordering.
  expect_equal(.crs_preference_order(rep(NA_character_, 4)), 1:4)
})

test_that("Tier 3 shortlists one row per place, carrying its equivalent codes", {
  skip_without_registry()
  fx <- make_utm33_fixture()
  sl <- crs_candidate_shortlist(fx$x, fx$y, lon = 12.958, lat = 52.466)

  expect_s3_class(sl, "data.frame")
  expect_gt(nrow(sl), 0)
  expect_lte(nrow(sl), 8)

  # The right answer leads, and its datum siblings ride on the same row rather
  # than competing for rows of their own. WGS 72, WGS 72BE and ED50 UTM 33N
  # land 10-210 m away - far below what a map click resolves - so ranking them
  # as separate rows by distance to that click let an obsolete datum lead.
  expect_equal(sl$epsg[1], 32633)
  expect_true(all(c(3045, 10733, 25833, 23033, 32233, 32433) %in% sl$equivalent[[1]]))
  expect_false(any(c(23033, 32233, 32433) %in% sl$epsg))
  # The fold is reported, not hidden: ED50 is the furthest member.
  expect_gt(sl$spread_m[1], 100)
  expect_lt(sl$spread_m[1], 250)
  expect_lt(sl$distance_km[1], 1)
  expect_match(sl$text[1], "^12\\.96[0-9]*°E, 52\\.46[0-9]*°N$")

  # Every row is a place the data could plausibly be, none of them the wrong
  # zone and none of them a world-scale display projection.
  expect_true(all(sl$distance_km <= 500))
  expect_false(any(c(32635, 32632, 3857) %in% sl$epsg))
  expect_true(all(nzchar(sl$text)))
  # Rows are distinct places: no position repeats.
  expect_equal(anyDuplicated(round(cbind(sl$lon, sl$lat), 6)), 0)
})

test_that("Tier 3 narrows by country text when there is no click", {
  skip_without_registry()
  fx <- make_utm33_fixture()
  sl <- crs_candidate_shortlist(fx$x, fx$y, area_text = "Germany", limit = 12)
  expect_gt(nrow(sl), 0)
  codes <- unlist(c(sl$epsg, sl$equivalent))
  # The three zones a German study area can plausibly be on.
  expect_true(all(c(32631, 32632, 32633) %in% codes))
  expect_true(all(nzchar(sl$text)))
})

test_that("a text filter narrows the shortlist and never widens it", {
  skip_without_registry()
  fx <- make_utm33_fixture()
  # Text that matches nothing INSIDE the area the click already bounded must
  # narrow to empty. It used to empty the pool and fall through to the
  # four-code zone family, i.e. reopen the very search the text restricted.
  z <- crs_candidate_shortlist(fx$x, fx$y, lon = 12.958, lat = 52.466,
                               area_text = "Zimbabwe")
  expect_equal(nrow(z), 0L)
  expect_true(isTRUE(attr(z, "text_no_match")))

  # Text that does match narrows the same pool rather than replacing it.
  hit <- crs_candidate_shortlist(fx$x, fx$y, lon = 12.958, lat = 52.466,
                                 area_text = "Germany")
  expect_gt(nrow(hit), 0L)
  expect_equal(hit$epsg[1], 32633)
  expect_false(isTRUE(attr(hit, "text_no_match")))
  expect_false(isTRUE(attr(hit, "truncated")))

  # A capped text-only search reports that it was capped, so the panel cannot
  # read as an exhaustive answer.
  big <- crs_candidate_shortlist(fx$x, fx$y, area_text = "United States")
  expect_true(isTRUE(attr(big, "truncated")))
})

test_that("Tier 3 reports no answer rather than a wrong one", {
  skip_without_registry()
  fx <- make_utm33_fixture()
  # Same coordinates, but the user indicates Peru. Read as a southern-
  # hemisphere grid these northings do resolve to a position, ~3000 km from
  # the stated one - so the shortlist is empty, not confidently wrong.
  expect_equal(nrow(crs_candidate_shortlist(fx$x, fx$y, lon = -75, lat = -12)), 0)
  # Nothing to work from at all.
  expect_null(crs_candidate_shortlist(c(NA, NA), c(NA, NA), lon = 12.958, lat = 52.466))
})

test_that("Tier 3 degrades to the zone family when the catalogue is unavailable", {
  fx <- make_utm33_fixture()
  # Exactly the state a build without proj.db or without RSQLite lands in.
  saved_reg <- .crs_registry_cache$reg
  saved_failed <- .crs_registry_cache$failed
  on.exit({
    .crs_registry_cache$reg <- saved_reg
    .crs_registry_cache$failed <- saved_failed
  }, add = TRUE)
  .crs_registry_cache$reg <- NULL
  .crs_registry_cache$failed <- TRUE

  expect_null(crs_registry())
  sl <- crs_candidate_shortlist(fx$x, fx$y, lon = 12.958, lat = 52.466)
  expect_equal(sl$epsg[1], 32633)
  # A longitude still fixes the zone exactly; only the datum question is left.
  expect_true(25833 %in% c(sl$epsg[1], sl$equivalent[[1]]))
  # Text alone has nothing to search: no guess, no error.
  expect_null(crs_candidate_shortlist(fx$x, fx$y, area_text = "Germany"))
})

test_that("the CRS catalogue's dependencies are declared", {
  root <- normalizePath(file.path(testthat::test_path(), "..", ".."), mustWork = TRUE)
  g <- paste(readLines(file.path(root, "global.R"), warn = FALSE), collapse = "\n")
  expect_match(g, '"DBI", "RSQLite"', fixed = TRUE)
  lock <- jsonlite::fromJSON(file.path(root, "renv.lock"), simplifyVector = FALSE)
  expect_true(all(c("DBI", "RSQLite") %in% names(lock$Packages)))
})

test_that("the Tier 3 picker is wired in and runs off the main thread", {
  root <- normalizePath(file.path(testthat::test_path(), "..", ".."), mustWork = TRUE)
  tabs <- paste(readLines(file.path(root, "ui_main_tabs.R"), warn = FALSE), collapse = "\n")
  ds <- paste(readLines(file.path(root, "server_data_setup.R"), warn = FALSE), collapse = "\n")

  expect_match(tabs, 'uiOutput("crs_picker_ui")', fixed = TRUE)
  # Offered only when Tier 1/2 found no evidence - never in place of an
  # identification the file actually supports.
  expect_match(ds, "crs_pick$no_evidence <- TRUE", fixed = TRUE)
  expect_match(ds, "crs_pick$no_evidence <- FALSE", fixed = TRUE)
  expect_match(ds, "crs_candidate_shortlist(xs, ys, lon = c_lon, lat = c_lat, area_text = a_txt)",
               fixed = TRUE)
  # Confirming a row goes through set_input_crs: selectize ignores a value
  # that is not one of its options.
  expect_match(ds, "observeEvent(input$crs_pick_epsg", fixed = TRUE)
  expect_match(ds, "set_input_crs(val)", fixed = TRUE)

  # Even a one-second search must not block the session, and the promise body
  # must never reach into a reactive - every value it needs is read before the
  # dispatch, the same contract server_execution.R's run_params keeps.
  expect_match(ds, "promises::future_promise({", fixed = TRUE)
  body <- sub(".*promises::future_promise\\(\\{(.*?)\\}, packages.*", "\\1", ds)
  expect_false(grepl("rv$", body, fixed = TRUE))
  expect_false(grepl("input$", body, fixed = TRUE))
  # A rejection must not leave the button disabled or the panel spinning.
  expect_match(ds, "promises::finally(p, function() {", fixed = TRUE)
  expect_match(ds, 'shinyjs::enable("crs_find_candidates")', fixed = TRUE)
})

test_that("the Map Viewer flags results computed under a different mapping", {
  root <- normalizePath(file.path(testthat::test_path(), "..", ".."), mustWork = TRUE)
  tabs <- paste(readLines(file.path(root, "ui_main_tabs.R"), warn = FALSE), collapse = "\n")
  mv <- paste(readLines(file.path(root, "server_map_viewer.R"), warn = FALSE), collapse = "\n")
  exec <- paste(readLines(file.path(root, "server_execution.R"), warn = FALSE), collapse = "\n")

  expect_match(tabs, 'uiOutput("map_crs_stale_note")', fixed = TRUE)
  expect_match(mv, "output$map_crs_stale_note", fixed = TRUE)
  # The run records the mapping it was computed from; the banner compares.
  expect_match(exec, "map_crs = rv$mapping$crs", fixed = TRUE)
  expect_match(exec, "map_x = rv$mapping$x", fixed = TRUE)
  expect_match(exec, "map_y = rv$mapping$y", fixed = TRUE)
  expect_match(mv, "!identical(d$map_crs, rv$mapping$crs)", fixed = TRUE)
  expect_match(mv, "!identical(d$map_x, rv$mapping$x)", fixed = TRUE)
  expect_match(mv, "!identical(d$map_y, rv$mapping$y)", fixed = TRUE)
  # Warn, never clear: the archive and the Export Registry hold references to
  # exactly these results.
  expect_false(grepl("rv$disp <- NULL", mv, fixed = TRUE))
})


# -- Target Mapping CRS suitability (Phase C of the 2026-08-22 fix) ----------
# The run gate used to ask only whether the target CRS's axis unit was the
# metre. Web Mercator passes that test and inflates every distance by 64% at
# Potsdam. The suitability gate asks the two questions that matter: does the
# CRS declare an area of use containing the data, and what is its point scale
# factor there. Reference values, measured at 12.958E 52.466N:
#   EPSG:32633 0.999836 | EPSG:25833 0.999836 | EPSG:5514 1.000356
#   EPSG:32634 1.003260 | EPSG:32635 1.010729 | EPSG:3857  1.637952 (parallel)

test_that("one wrap-aware containment test serves the whole app", {
  # Plain interval.
  expect_true(crs_extent_contains(12, 18, 0, 84, 12.958, 52.466))
  expect_false(crs_extent_contains(24, 30, 0, 84, 12.958, 52.466))
  # west > east is a WRAPPED range (crosses the antimeridian), not an empty one.
  expect_true(crs_extent_contains(174, -172, -52, -8, 179.5, -18))
  expect_true(crs_extent_contains(174, -172, -52, -8, -175, -18))
  expect_false(crs_extent_contains(174, -172, -52, -8, 100, -18))
  # Vectorised on both sides, and pad widens the box. Many extents against one
  # point is the registry filter; ONE extent against many points is the
  # suitability gate sampling the data corners, and that shape is the one an
  # ifelse() would silently collapse to its first element.
  expect_equal(crs_extent_contains(c(12, 24), c(18, 30), c(0, 0), c(84, 84),
                                   12.958, 52.466), c(TRUE, FALSE))
  expect_equal(crs_extent_contains(6, 12, 0, 84, c(11.5, 12.5), c(52.4, 52.5)),
               c(TRUE, FALSE))
  expect_equal(crs_extent_contains(174, -172, -52, -8, c(179.5, 100), c(-18, -18)),
               c(TRUE, FALSE))
  expect_false(crs_extent_contains(12, 18, 0, 84, 18.5, 52.466))
  expect_true(crs_extent_contains(12, 18, 0, 84, 18.5, 52.466, pad = 1))
  # Missing bounds answer NA - "cannot say" - and callers use which().
  expect_true(is.na(crs_extent_contains(NA_real_, 18, 0, 84, 12.958, 52.466)))

  # And it is the ONLY copy: the registry filter and the shortlist's
  # self-consistency check both go through it.
  root <- normalizePath(file.path(testthat::test_path(), "..", ".."), mustWork = TRUE)
  gu <- paste(readLines(file.path(root, "global_utils.R"), warn = FALSE), collapse = "\n")
  expect_equal(length(gregexpr("crs_extent_contains(", gu, fixed = TRUE)[[1]]), 3L)
})

test_that("crs_area_of_use answers from the catalogue, then from the WKT, then not at all", {
  skip_without_registry()
  expect_equal(unname(unlist(crs_area_of_use("EPSG:32633"))), c(12, 18, 0, 84))
  expect_equal(unname(unlist(crs_area_of_use("EPSG:32635"))), c(24, 30, 0, 84))
  expect_named(crs_area_of_use("EPSG:32633"), c("west", "east", "south", "north"))
  expect_equal(nrow(crs_area_of_use("EPSG:32633")), 1L)
  # A CRS that declares nothing usable, and one that does not exist.
  expect_null(crs_area_of_use("EPSG:99999"))
  expect_null(crs_area_of_use(NULL))
})

test_that("a CRS declaring several extents is judged against their union", {
  skip_without_registry()
  # 17 codes in this catalogue declare more than one extent. EPSG:2393 (KKJ /
  # Finland Uniform Coordinate System) declares a 3-degree zone strip AND the
  # whole of onshore Finland; reading only the first row told a Finnish user at
  # 22 deg E that their data landed outside a CRS that covers it.
  ext <- crs_area_of_use("EPSG:2393")
  expect_gt(nrow(ext), 1L)
  expect_true(any(ext$west < 20))
  # Inside the wider extent but outside the narrow one: not "outside".
  wide <- crs_target_suitability("EPSG:2393", 22, 62)
  expect_false(wide$outside)
  # Genuinely outside every declared extent still warns.
  far <- crs_target_suitability("EPSG:2393", 12.958, 52.466)
  expect_true(far$outside)
  expect_match(far$msg, "declared extents", fixed = TRUE)
})

test_that("crs_area_of_use falls back to the WKT2 BBOX node without the catalogue", {
  saved_reg <- .crs_registry_cache$reg
  saved_failed <- .crs_registry_cache$failed
  on.exit({
    .crs_registry_cache$reg <- saved_reg
    .crs_registry_cache$failed <- saved_failed
  }, add = TRUE)
  .crs_registry_cache$reg <- NULL
  .crs_registry_cache$failed <- TRUE

  expect_null(crs_registry())
  # WKT2 writes BBOX[south, west, north, east]: 32633 reads BBOX[0,12,84,18].
  expect_equal(unname(unlist(crs_area_of_use("EPSG:32633"))), c(12, 18, 0, 84))
  # The projected-only registry never carried this one; the WKT does.
  expect_equal(unname(unlist(crs_area_of_use("EPSG:4326"))), c(-180, 180, -90, 90))
})

test_that("the point scale factor reproduces the measured values at Potsdam", {
  ref <- c("EPSG:32633" = 0.999836, "EPSG:25833" = 0.999836, "EPSG:5514" = 1.000356,
           "EPSG:32634" = 1.003260, "EPSG:32635" = 1.010729)
  for (cc in names(ref)) {
    r <- crs_scale_factor(cc, 12.958, 52.466)
    expect_equal(r$k, unname(ref[[cc]]), tolerance = 1e-4, info = cc)
    # These are conformal: k is the same in every direction, by construction.
    expect_equal(r$parallel, r$meridian, tolerance = 1e-6, info = cc)
    expect_equal(r$dev, abs(r$k - 1), tolerance = 1e-12, info = cc)
  }
  # Web Mercator: the case the unit-only gate waved through.
  r <- crs_scale_factor("EPSG:3857", 12.958, 52.466)
  expect_gt(r$k, 1.6)
  expect_equal(r$parallel, 1.637952, tolerance = 1e-4)
})

test_that("both arcs are measured, because equal-area projections need both", {
  # EPSG:5070 (Albers, equal-area) at Potsdam stretches the parallel and
  # squeezes the meridian by the same construction: a single arc understates it.
  r <- crs_scale_factor("EPSG:5070", 12.958, 52.466)
  expect_gt(r$parallel, 1.03)
  expect_lt(r$meridian, 0.97)
  expect_equal(r$dev, max(abs(r$parallel - 1), abs(r$meridian - 1)), tolerance = 1e-12)
  # EPSG:3035 (LAEA Europe) is equal-area too and is centred here: the two arcs
  # differ, both are tiny, and it passes. The check does not refuse a sound CRS.
  r2 <- crs_scale_factor("EPSG:3035", 12.958, 52.466)
  expect_false(isTRUE(all.equal(r2$parallel, r2$meridian)))
  expect_lt(r2$dev, 0.001)
})

test_that("the scale factor reports the worst of the positions it is given", {
  # A study area can be inside tolerance at its centre and outside it at an
  # edge, which is why the gate samples the bbox corners as well as the
  # centroid: 20 deg E is 5 deg off zone 33s central meridian.
  centre <- crs_scale_factor("EPSG:32633", 12.958, 52.466)
  edge <- crs_scale_factor("EPSG:32633", 20, 52.466)
  many <- crs_scale_factor("EPSG:32633", c(12.958, 20), c(52.466, 52.466))
  expect_gt(edge$dev, centre$dev)
  expect_equal(many$k, edge$k, tolerance = 1e-12)
  expect_equal(many$dev, edge$dev, tolerance = 1e-12)
})

test_that("the scale factor declines to answer rather than guessing", {
  # Geographic: not a projection, no linear scale, and the pipeline projects it
  # to a metric UTM zone itself.
  expect_null(crs_scale_factor("EPSG:4326", 12.958, 52.466))
  expect_null(crs_scale_factor("not a crs at all", 12.958, 52.466))
  expect_null(crs_scale_factor("EPSG:32633", NA_real_, NA_real_))
  expect_null(crs_scale_factor(NULL, 12.958, 52.466))
})

test_that("crs_sample_positions samples the centroid and the bbox corners", {
  fx <- make_utm33_fixture()
  pos <- crs_sample_positions(fx, "x", "y", "EPSG:32633")
  expect_equal(nrow(pos), 5L)
  expect_equal(pos$lon[1], mean(fx$lon), tolerance = 1e-3)
  expect_true(all(pos$lon > 12.9 & pos$lon < 13.0))
  # A single point is one position, not five copies of it.
  expect_equal(nrow(crs_sample_positions(fx[1, ], "x", "y", "EPSG:32633")), 1L)
  # Nothing to measure, no answer.
  expect_null(crs_sample_positions(fx, "nope", "y", "EPSG:32633"))
  expect_null(crs_sample_positions(fx, "x", "y", "not a crs at all"))
})

test_that("the suitability verdict separates fit, doubtful and unusable", {
  skip_without_registry()
  lon <- 12.958; lat <- 52.466

  ok <- crs_target_suitability("EPSG:32633", lon, lat)
  expect_equal(ok$level, "ok")
  expect_null(ok$msg)
  expect_false(ok$outside)

  # Two zones east: the incident's CRS. Refused, and the number is stated.
  bad <- crs_target_suitability("EPSG:32635", lon, lat)
  expect_equal(bad$level, "block")
  expect_equal(bad$k, 1.010729, tolerance = 1e-4)
  expect_match(bad$msg, "1.010729")
  expect_true(bad$outside)

  # Web Mercator: metric axis, and 64% wrong. The whole point of the gate.
  wm <- crs_target_suitability("EPSG:3857", lon, lat)
  expect_equal(wm$level, "block")
  expect_gt(wm$k, 1.6)

  # One zone east: wrong, but not catastrophically. Warn, do not refuse.
  near <- crs_target_suitability("EPSG:32634", lon, lat)
  expect_equal(near$level, "warn")

  # Outside its declared area yet measuring correctly (k = +0.036%): a warning
  # only. A study area straddling a zone boundary must not be refused.
  kv <- crs_target_suitability("EPSG:5514", lon, lat)
  expect_equal(kv$level, "warn")
  expect_true(kv$outside)
  expect_lt(kv$dev, 0.001)
  expect_match(kv$msg, "outside it")
})

test_that("a study area is outside the box when any of its corners is", {
  skip_without_registry()
  # An area straddling the 12 deg E zone boundary, projected in UTM 32N
  # (declared 6-12 deg E): sound to measure with (+0.03%) and outside the
  # declared box at its eastern edge. Warned, never refused - the corner the
  # centroid alone would have missed.
  s <- crs_target_suitability("EPSG:32632", c(11.5, 12.5), c(52.4, 52.5))
  expect_true(s$outside)
  expect_equal(s$level, "warn")
  expect_lt(s$dev, 0.001)
  # Wholly inside: no warning at all.
  expect_false(crs_target_suitability("EPSG:32632", c(10.5, 11.5), c(52.4, 52.5))$outside)
})

test_that("the suitability gate never blocks on a question it cannot answer", {
  # A geographic target CRS skips the gate entirely: refusing EPSG:4326 would
  # be a regression, and validate_crs(require_metric) exempts it for the same
  # reason.
  geo <- crs_target_suitability("EPSG:4326", 12.958, 52.466)
  expect_equal(geo$level, "ok")
  expect_true(is.na(geo$k))

  expect_equal(crs_target_suitability("not a crs at all", 12.958, 52.466)$level, "ok")
  expect_equal(crs_target_suitability("", 12.958, 52.466)$level, "ok")
  expect_equal(crs_target_suitability(NULL, 12.958, 52.466)$level, "ok")
  expect_equal(crs_target_suitability("EPSG:3857", NA_real_, NA_real_)$level, "ok")
})

test_that("the advisory names the CRS that is right, not only the one that is wrong", {
  # The Potsdam incident: data at 12.96 E, target left on UTM 35N. The gate
  # said the CRS was wrong; this says the answer is EPSG:32633.
  rec <- crs_recommend_target(12.958, 52.466)
  expect_equal(rec$crs, "EPSG:32633")
  expect_equal(rec$code, 32633L)
  # Inside its own zone a UTM grid holds well under the 0.1% warn threshold.
  expect_lt(rec$dev, 0.001)
  # The label is a name a user recognises, not a bare code.
  expect_match(rec$label, "UTM zone 33N")

  # Denizli, the user's usual sampling area, really is UTM 35N: the sticky
  # default was the right answer there and the wrong one 800 km away, which is
  # exactly why the recommendation has to be computed per dataset.
  expect_equal(crs_recommend_target(29.09, 37.78)$crs, "EPSG:32635")

  # Southern hemisphere takes the 327xx family.
  expect_equal(crs_recommend_target(-58.38, -34.60)$crs, "EPSG:32721")

  # Zone from the MEAN position, deviation from EVERY position, so an area
  # spanning zones reports the worst case of what is being offered rather than
  # its best.
  wide <- crs_recommend_target(c(11.0, 17.0), c(52.4, 52.4))
  expect_equal(wide$crs, "EPSG:32633")
  expect_gt(wide$dev, crs_recommend_target(12.958, 52.466)$dev)

  # Never blocks on a question it cannot answer.
  expect_null(crs_recommend_target(NA_real_, NA_real_))
  expect_null(crs_recommend_target(numeric(0), numeric(0)))
})

test_that("the suitability verdict separates the finding from its consequence", {
  skip_without_registry()
  bad <- crs_target_suitability("EPSG:32635", 12.958, 52.466)
  # msg carries the facts and only the facts, so a caller can lead with it.
  expect_match(bad$msg, "1.010729", fixed = TRUE)
  expect_false(grepl("Grid resolution", bad$msg, fixed = TRUE))
  # detail is the standing explanation, one copy, shared with the run gate.
  expect_identical(bad$detail, crs_measure_detail)
  expect_match(crs_measure_detail, "never moves your points", fixed = TRUE)
  # An "ok" verdict carries neither.
  expect_null(crs_target_suitability("EPSG:32633", 12.958, 52.466)$detail)
})

test_that("the Target CRS advisory is one readout with a one-click remedy", {
  root <- normalizePath(file.path(testthat::test_path(), "..", ".."), mustWork = TRUE)
  ds <- paste(readLines(file.path(root, "server_data_setup.R"), warn = FALSE), collapse = "\n")
  tabs <- paste(readLines(file.path(root, "ui_main_tabs.R"), warn = FALSE), collapse = "\n")
  exec <- paste(readLines(file.path(root, "server_execution.R"), warn = FALSE), collapse = "\n")

  expect_match(tabs, 'uiOutput("crs_target_note")', fixed = TRUE)
  expect_match(ds, "rec <- crs_recommend_target(pos$lon, pos$lat)", fixed = TRUE)
  # The remedy is applied through the same shared-id route the candidate picker
  # uses, and it writes the Target selector.
  expect_match(ds, "Shiny.setInputValue('crs_target_apply'", fixed = TRUE)
  expect_match(ds, "observeEvent(input$crs_target_apply", fixed = TRUE)
  expect_match(ds, 'set_target_crs(paste0("EPSG:", code))', fixed = TRUE)
  # A geographic target is answered, not met with silence: users otherwise
  # learn that EPSG:4326 makes the warning go away rather than why it is safe.
  expect_match(ds, "is geographic: maps and exports come out", fixed = TRUE)
  # The run gate prescribes as well as diagnoses.
  expect_match(exec, "rec <- crs_recommend_target(pos$lon, pos$lat)", fixed = TRUE)
  expect_match(exec, "tags$p(suit$detail)", fixed = TRUE)
})

test_that("Web Mercator is offered as an input CRS and never as a target", {
  expect_true("EPSG:3857" %in% common_crs_input)
  expect_false("EPSG:3857" %in% common_crs_target)
  # Nothing else changes: the target list is the input list minus 3857.
  expect_equal(unname(common_crs_target),
               unname(common_crs_input[common_crs_input != "EPSG:3857"]))
  # crs_zone_candidates keeps offering it - as an INPUT candidate, where data
  # really can arrive in Web Mercator.
  expect_true(3857 %in% crs_zone_candidates(12.958))

  root <- normalizePath(file.path(testthat::test_path(), "..", ".."), mustWork = TRUE)
  ds <- paste(readLines(file.path(root, "server_data_setup.R"), warn = FALSE), collapse = "\n")
  # A CRS outside the list still travels with its own choice entry, so an
  # uploaded config or a restored run made under any CRS keeps loading.
  expect_match(ds, "ch <- if (value %in% base) base else c(base, setNames(value, value))",
               fixed = TRUE)
  expect_match(ds, 'set_crs_choice("map_crs", value, "Select the CRS your coordinates were recorded in", common_crs_input',
               fixed = TRUE)
  expect_match(ds, 'set_crs_choice("crs_selection", value, "Select the CRS for output maps and exports", common_crs_target',
               fixed = TRUE)
  # Promoting a typed `32633` to `EPSG:32633` writes the selector but must NOT
  # be recorded as the app's own choice: it is the user's CRS in a parseable
  # spelling, and recording it would let the identification observer overwrite
  # it on the next upload or boundary shapefile.
  expect_match(ds, "set_input_crs(norm, record = FALSE)", fixed = TRUE)
  expect_match(ds, "set_target_crs(norm, record = FALSE)", fixed = TRUE)
})

test_that("the suitability gate is advisory at selection time and enforced at run time", {
  root <- normalizePath(file.path(testthat::test_path(), "..", ".."), mustWork = TRUE)
  ds <- paste(readLines(file.path(root, "server_data_setup.R"), warn = FALSE), collapse = "\n")
  exec <- paste(readLines(file.path(root, "server_execution.R"), warn = FALSE), collapse = "\n")

  # Advisory: a persistent readout under the selectors, so the rule is met
  # before a whole run is configured and the verdict is still on screen when
  # the user acts on it.
  expect_match(ds, "crs_target_suitability(sel, pos$lon, pos$lat)", fixed = TRUE)
  expect_match(ds, "output$crs_target_note <- renderUI", fixed = TRUE)
  # The toasts it replaced are gone: three channels for one verdict could queue
  # together and could contradict each other.
  expect_false(grepl("crs_suit_guard", ds, fixed = TRUE))
  expect_false(grepl("crs_unit_guard", ds, fixed = TRUE))

  # Enforcement sits with the metric-axis rule, on the run gate itself.
  expect_match(exec, "validate_crs(input$crs_selection, \"CRS Validation Error:\", duration = 15,", fixed = TRUE)
  expect_match(exec, "crs_target_suitability(input$crs_selection, crs_suit_pos$lon, crs_suit_pos$lat)",
               fixed = TRUE)
  # A block stops the run only when it was not explicitly overridden.
  expect_match(exec, 'if (!is.null(crs_suit) && identical(crs_suit$level, "block")) {', fixed = TRUE)
  expect_match(exec, "identical(rv$crs_gate_ack, crs_gate_key(input$crs_selection, crs_suit$dev))", fixed = TRUE)
  # The refusal needs no unwind, because it happens before the processing
  # overlay goes up and before any run state is committed - see
  # "every CRS refusal happens before the run commits any state" above. It
  # must therefore NOT carry its own copy of the failure handler's teardown.
  gate <- substr(exec, regexpr("crs_target_suitability(input$crs_selection", exec, fixed = TRUE),
                 regexpr("locs <- resolve_selected_localities", exec, fixed = TRUE))
  expect_false(grepl("rv$model_running <- FALSE", gate, fixed = TRUE))
  expect_false(grepl('shinyjs::hide("map_processing_overlay")', gate, fixed = TRUE))

  # The override mirrors the collinearity modal, and is recorded in the run
  # config the same way "Keep All" is.
  expect_match(exec, 'actionButton("crs_gate_override_btn", "Use Anyway (Not Recommended)"', fixed = TRUE)
  expect_match(exec, "observeEvent(input$crs_gate_override_btn, {", fixed = TRUE)
  expect_match(exec, "rv$run_config_summary$crs_gate_override <- crs_override", fixed = TRUE)
  expect_match(exec, "rv$run_config_summary$crs_scale_factor <- ", fixed = TRUE)

  # The collinearity screen is asked once, from one place, after the CRS gate.
  expect_equal(length(gregexpr("run_collinearity_gate()", exec, fixed = TRUE)[[1]]), 3L)
  expect_equal(length(gregexpr("check_vif(df_aux, threshold = 10)", exec, fixed = TRUE)[[1]]), 1L)

  # Restoring an archived run must not be re-gated: it replays stored results
  # and never re-enters the pipeline.
  ex <- paste(readLines(file.path(root, "server_export.R"), warn = FALSE), collapse = "\n")
  expect_false(grepl("crs_target_suitability", ex, fixed = TRUE))
  expect_match(ex, "cfg$crs_scale_factor", fixed = TRUE)
})
