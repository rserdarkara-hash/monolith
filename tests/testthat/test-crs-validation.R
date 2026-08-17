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
