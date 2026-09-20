# test-styled-points.R — tests for the leaflet layer helpers in ui_helpers.R:
# add_styled_points and add_base_tiles.

# ── Fixtures ───────────────────────────────────────────────────────────────

make_leaflet_map <- function() {
  leaflet::leaflet() |> leaflet::addTiles()
}

make_sf_points <- function(n = 10, seed = 42) {
  set.seed(seed)
  coords <- data.frame(
    lon = runif(n, 32.5, 33.0),
    lat = runif(n, 39.5, 40.0),
    value  = rnorm(n, 50, 10),
    group  = factor(sample(c("A", "B", "C"), n, replace = TRUE)),
    label  = paste0("Pt", seq_len(n))
  )
  sf::st_as_sf(coords, coords = c("lon", "lat"), crs = 4326)
}

# ── Reading what the layer actually emitted ────────────────────────────────
#
# add_styled_points returns `map |> addCircleMarkers(...)`, so a class check on
# the result holds for any implementation that does not raise. These read the
# emitted call instead. leaflet packs the path options into one argument; find
# that by content rather than by position, which its argument order does not
# promise. Positions 1-3 (lat, lng, radius) come from leaflet's own
# invokeMethod signature; the lat/lng assertions below compare against the
# transformed coordinates, so a swapped pair fails rather than passing quietly.

marker_calls <- function(m) Filter(function(c) c$method == "addCircleMarkers", m$x$calls)
marker_args <- function(m) marker_calls(m)[[1]]$args
marker_opts <- function(m) {
  hit <- Filter(function(a) is.list(a) && "fillOpacity" %in% names(a), marker_args(m))
  hit[[1]]
}
# addLabelOnlyMarkers is issued as an addMarkers call; the label vector is the
# character argument with one entry per point (the only other character
# argument is the layer group name, which is scalar).
label_values <- function(m, n) {
  hit <- Filter(function(c) c$method == "addMarkers", m$x$calls)
  if (length(hit) == 0) return(NULL)
  Filter(function(a) is.character(a) && length(a) == n, hit[[1]]$args)[[1]]
}
legend_calls <- function(m) Filter(function(c) c$method == "addLegend", m$x$calls)
legend_args <- function(m, i = 1L) {
  args <- legend_calls(m)[[i]]$args
  Filter(function(a) is.list(a) && "labels" %in% names(a), args)[[1]]
}

# ── What add_styled_points draws ───────────────────────────────────────────

test_that("add_styled_points emits the colours each mode promises", {
  pts <- make_sf_points(9)
  pts$group <- factor(rep(c("A", "B", "C"), each = 3))
  grp <- as.character(pts$group)
  custom <- c(A = "#E69F00", B = "#009E73", C = "#56B4E9")

  # No colouring: one cyan for every marker, half-opaque, and no legend.
  m <- add_styled_points(make_leaflet_map(), pts)
  expect_identical(marker_opts(m)$fillColor, "cyan")
  expect_identical(marker_opts(m)$color, "cyan")
  expect_equal(marker_opts(m)$fillOpacity, 0.5)
  expect_length(legend_calls(m), 0L)

  # color_by alone is not enough - without a palette there is nothing to map
  # the groups onto, so it stays cyan and legend-free.
  m <- add_styled_points(make_leaflet_map(), pts, color_by = "group")
  expect_identical(marker_opts(m)$fillColor, "cyan")
  expect_length(legend_calls(m), 0L)

  # A complete palette: each marker carries ITS group's colour, the border
  # turns white, and the legend lists the groups in sorted order.
  m <- add_styled_points(make_leaflet_map(), pts, color_by = "group",
                         custom_colors = custom)
  expect_identical(marker_opts(m)$fillColor, unname(custom[grp]))
  expect_identical(marker_opts(m)$color, "white")
  expect_equal(marker_opts(m)$fillOpacity, 0.85)
  expect_identical(as.character(legend_args(m)$labels), c("A", "B", "C"))
  expect_identical(as.character(legend_args(m)$colors), unname(custom))

  # A partial palette: the named groups keep their colours and the unnamed one
  # is filled in with a distinct colour rather than dropped or recycled.
  m <- add_styled_points(make_leaflet_map(), pts, color_by = "group",
                         custom_colors = custom[c("A", "B")])
  fills <- marker_opts(m)$fillColor
  expect_identical(unique(fills[grp == "A"]), unname(custom[["A"]]))
  expect_identical(unique(fills[grp == "B"]), unname(custom[["B"]]))
  expect_length(unique(fills[grp == "C"]), 1L)
  expect_false(unique(fills[grp == "C"]) %in% custom[c("A", "B")])
  expect_length(legend_args(m)$labels, 3L)

  # A colour-by column that is not in the frame falls back to the default
  # instead of erroring or colouring by something else.
  m <- add_styled_points(make_leaflet_map(), pts, color_by = "nonexistent",
                         custom_colors = custom)
  expect_identical(marker_opts(m)$fillColor, "cyan")
  expect_length(legend_calls(m), 0L)
})

test_that("add_styled_points carries geometry, size, popups and labels through", {
  # Nothing to draw: no marker call is issued at all.
  expect_length(marker_calls(add_styled_points(make_leaflet_map(), make_sf_points(6)[0, ])), 0L)

  # A projected input is transformed, not passed through - UTM eastings and
  # northings would be orders of magnitude outside either coordinate range.
  utm <- make_test_points(8)                       # EPSG:32633
  ll <- sf::st_coordinates(sf::st_transform(utm, 4326))
  args <- marker_args(add_styled_points(make_leaflet_map(), utm, marker_size = 7))
  expect_equal(args[[1]], unname(ll[, "Y"]), tolerance = 1e-9)   # lat
  expect_equal(args[[2]], unname(ll[, "X"]), tolerance = 1e-9)   # lng
  expect_equal(args[[3]], 7)                                     # radius

  # popup_fn is applied row by row and its output reaches the popup slot.
  pts <- make_sf_points(4)
  m <- add_styled_points(make_leaflet_map(), pts,
                         popup_fn = function(row) paste("Value:", row$value))
  expect_identical(Filter(function(a) identical(a, paste("Value:", pts$value)),
                          marker_args(m))[[1]],
                   paste("Value:", pts$value))
  expect_null(label_values(add_styled_points(make_leaflet_map(), pts), nrow(pts)))

  # Labels are their own layer, and the field's type decides its formatting.
  # A character column is shown verbatim...
  labelled <- function(field) {
    label_values(add_styled_points(make_leaflet_map(), pts, show_labels = TRUE,
                                   label_field = field), nrow(pts))
  }
  expect_identical(labelled("label"), pts$label)
  # ...a factor by its level labels, not by its integer codes...
  expect_identical(labelled("group"), as.character(pts$group))
  # ...and a numeric to two decimals, which is the only place that rounding
  # happens.
  expect_identical(labelled("value"), sprintf("%.2f", pts$value))

  # A point with no measured value carries no label text. sprintf("%.2f", NA)
  # returns the STRING "NA", which is not is.na() and would be painted on the
  # map beside the real readings, so the numeric branch has to guard it.
  gapped <- pts
  gapped$value[c(2, 4)] <- NA_real_
  lab <- label_values(add_styled_points(make_leaflet_map(), gapped,
                                        show_labels = TRUE, label_field = "value"),
                      nrow(gapped))
  expect_true(all(is.na(lab[c(2, 4)])))
  expect_identical(lab[-c(2, 4)], sprintf("%.2f", gapped$value[-c(2, 4)]))
  # A field that is not there draws no label layer rather than an empty one,
  # and neither does the "none" the control sends when labels are off.
  expect_null(labelled("nope"))
  expect_null(labelled("none"))
})

# ── add_base_tiles ─────────────────────────────────────────────────────────
#
# Every map in the app adds its basemap through this one call, so what matters
# is that all five providers come out on identical zoom terms: a switch that
# left the new layer shallower than the map's current zoom used to blank it.

tile_call <- function(m) {
  hit <- Filter(function(c) identical(c$method, "addProviderTiles"), m$x$calls)
  expect_length(hit, 1)
  hit[[1]]$args
}

test_that("add_base_tiles gives every provider the same maximum zoom", {
  for (p in names(BASE_TILE_NATIVE_ZOOM)) {
    args <- tile_call(add_base_tiles(leaflet::leaflet(), p))
    expect_equal(args[[1]], p)
    expect_equal(args[[2]], "base_tiles")          # layerId
    expect_equal(args[[4]]$maxZoom, BASE_TILE_MAX_ZOOM)
  }
})

test_that("add_base_tiles declares each provider's own native depth", {
  # Below its native limit a provider would be hidden rather than upscaled,
  # so the two must never be collapsed into one number.
  expect_equal(tile_call(add_base_tiles(leaflet::leaflet(), "OpenTopoMap"))[[4]]$maxNativeZoom, 17)
  expect_equal(tile_call(add_base_tiles(leaflet::leaflet(), "CartoDB.Positron"))[[4]]$maxNativeZoom, 20)
})

test_that("add_base_tiles pins the basemap one level below the rasters", {
  # addRasterImage paints its surface as a canvas GridLayer in the same tile
  # pane, on the GridLayer default z-index of 1. The basemap must sit strictly
  # below that, and not below zero, where some browsers stop painting it.
  for (p in names(BASE_TILE_NATIVE_ZOOM)) {
    z <- tile_call(add_base_tiles(leaflet::leaflet(), p))[[4]]$zIndex
    expect_equal(z, 0)
  }
})

test_that("add_base_tiles falls back to the satellite layer for an empty provider", {
  # The fallback must be a provider that needs no API key, or a map with no
  # explicit choice comes up watermarked.
  expect_equal(tile_call(add_base_tiles(leaflet::leaflet(), NULL))[[1]], "Esri.WorldImagery")
  expect_equal(tile_call(add_base_tiles(leaflet::leaflet(), ""))[[1]], "Esri.WorldImagery")
})

# ── CARTO API key ──────────────────────────────────────────────────────────
#
# CARTO's raster basemaps answer an unkeyed request with an "API key required"
# watermark, and leaflet-providers' CartoDB entry has nowhere to put a key, so
# a keyed layer is issued as a plain tile layer built from CARTO's own URL
# template. The two paths must be interchangeable to every other part of the
# map: same layerId, same z-index, same zoom terms.

keyed_call <- function(m) {
  hit <- Filter(function(c) identical(c$method, "addTiles"), m$x$calls)
  expect_length(hit, 1)
  hit[[1]]$args
}

test_that("a CARTO key produces a keyed tile URL for each CARTO variant", {
  expect_equal(
    keyed_call(add_base_tiles(leaflet::leaflet(), "CartoDB.Positron", "KEY123"))[[1]],
    "https://{s}.basemaps.cartocdn.com/light_all/{z}/{x}/{y}{r}.png?key=KEY123")
  expect_equal(
    keyed_call(add_base_tiles(leaflet::leaflet(), "CartoDB.DarkMatter", "KEY123"))[[1]],
    "https://{s}.basemaps.cartocdn.com/dark_all/{z}/{x}/{y}{r}.png?key=KEY123")
})

test_that("a keyed CARTO layer keeps the same identity and zoom terms", {
  for (p in names(BASE_TILE_CARTO_VARIANT)) {
    args <- keyed_call(add_base_tiles(leaflet::leaflet(), p, "KEY123"))
    expect_equal(args[[2]], "base_tiles")                                  # layerId
    expect_equal(args[[4]]$zIndex, 0)
    expect_equal(args[[4]]$maxZoom, BASE_TILE_MAX_ZOOM)
    expect_equal(args[[4]]$maxNativeZoom, unname(BASE_TILE_NATIVE_ZOOM[p]))
    expect_equal(args[[4]]$subdomains, "abcd")
    expect_true(grepl("CARTO", args[[4]]$attribution, fixed = TRUE))
  }
})

test_that("a key that is absent, empty or blank leaves the provider path alone", {
  # Unkeyed tiles are watermarked, not missing, so the map still draws.
  for (key in list(NULL, "", "   ", NA_character_)) {
    expect_equal(tile_call(add_base_tiles(leaflet::leaflet(), "CartoDB.Positron", key))[[1]],
                 "CartoDB.Positron")
  }
})

test_that("a CARTO key is ignored by every non-CARTO provider", {
  for (p in setdiff(names(BASE_TILE_NATIVE_ZOOM), names(BASE_TILE_CARTO_VARIANT))) {
    expect_equal(tile_call(add_base_tiles(leaflet::leaflet(), p, "KEY123"))[[1]], p)
  }
})

test_that("a CARTO key is percent-encoded into the query string", {
  # An unescaped & or = in the key would truncate or corrupt the query.
  expect_equal(
    keyed_call(add_base_tiles(leaflet::leaflet(), "CartoDB.Positron", "a b&c=d"))[[1]],
    "https://{s}.basemaps.cartocdn.com/light_all/{z}/{x}/{y}{r}.png?key=a%20b%26c%3Dd")
})

test_that("add_base_tiles accepts a provider it has no depth entry for", {
  args <- tile_call(add_base_tiles(leaflet::leaflet(), "Stadia.OSMBright"))
  expect_equal(args[[4]]$maxNativeZoom, BASE_TILE_MAX_ZOOM)
})

# ── Map Viewer raster image ids ─────────────────────────────────────

# style_map_rasters() gives its images positional Leaflet ids and re-adds them
# on every restyle. Leaflet REPLACES a layer whose id matches but never drops
# one the new pass does not re-add, so a run with fewer images than the one on
# screen (fewer localities, or the residual branch iterating a shorter layer
# list) would leave the surplus painted over the new surface.

removed_image_ids <- function(m) {
  calls <- Filter(function(cl) identical(cl$method, "removeImage"), m$x$calls)
  vapply(calls, function(cl) as.character(cl$args[[1]]), character(1))
}

test_that("restyling with fewer images leaves no stale raster id behind", {
  prev_ids <- vapply(seq_len(3), raster_img_layer_id, character(1))
  now_ids  <- vapply(seq_len(2), raster_img_layer_id, character(1))

  m <- remove_surplus_raster_images(make_leaflet_map(), n_now = 2, n_prev = 3)
  removed <- removed_image_ids(m)

  # Exactly the ids the shorter pass did not re-add, and nothing it did.
  expect_identical(removed, setdiff(prev_ids, now_ids))
  expect_identical(intersect(removed, now_ids), character(0))
})

test_that("remove_surplus_raster_images retires every surplus id, in order", {
  m <- remove_surplus_raster_images(make_leaflet_map(), n_now = 2, n_prev = 5)
  expect_identical(removed_image_ids(m),
                   c("rast_img_3", "rast_img_4", "rast_img_5"))
})

test_that("remove_surplus_raster_images removes nothing when the count holds or grows", {
  # A first render has no recorded count at all, and a longer run replaces
  # every id it re-adds, so neither may issue a removal.
  expect_length(removed_image_ids(remove_surplus_raster_images(make_leaflet_map(), 2, 2)), 0)
  expect_length(removed_image_ids(remove_surplus_raster_images(make_leaflet_map(), 3, 2)), 0)
  expect_length(removed_image_ids(remove_surplus_raster_images(make_leaflet_map(), 2, 0)), 0)
})

test_that("raster_img_layer_id is the id the restyler adds and retires", {
  expect_identical(raster_img_layer_id(1), "rast_img_1")
  expect_identical(raster_img_layer_id(12), "rast_img_12")
})

# ── Points with no measured value ──────────────────────────────────────────
# rv$sf is the DISPLAY set: deduplicated by coordinate but never NA-target
# filtered, so it holds points the surface was not fitted from. Under the
# default colouring every marker rendered identically, so those points read as
# samples supporting the map.

test_that("a point with no measured value renders hollow and names itself", {
  pts <- make_sf_points(6)
  pts$value[c(2, 5)] <- NA_real_
  m <- add_styled_points(make_leaflet_map(), pts, value_col = "value")

  opts <- marker_opts(m)
  expect_equal(opts$fillOpacity[c(2, 5)], c(0, 0))
  expect_true(all(opts$fillOpacity[-c(2, 5)] > 0))
  expect_equal(opts$color[c(2, 5)], rep(MISSING_VALUE_POINT_COLOR, 2))
  expect_false(any(opts$color[-c(2, 5)] == MISSING_VALUE_POINT_COLOR))

  lg <- legend_calls(m)
  expect_length(lg, 1L)
  expect_identical(as.character(legend_args(m)$labels), MISSING_VALUE_POINT_LABEL)
  expect_identical(as.character(legend_args(m)$colors), MISSING_VALUE_POINT_COLOR)
})

test_that("a point set with no missing values gets neither treatment", {
  pts <- make_sf_points(6)
  m <- add_styled_points(make_leaflet_map(), pts, value_col = "value")
  expect_length(legend_calls(m), 0L)
  expect_length(marker_opts(m)$fillOpacity, 1L)   # still the scalar
  # And without value_col the helper cannot know, so nothing changes.
  pts$value[2] <- NA_real_
  expect_length(legend_calls(add_styled_points(make_leaflet_map(), pts)), 0L)
})

test_that("the distinction survives every colour-by mode", {
  pts <- make_sf_points(8)
  pts$value[3] <- NA_real_
  m <- add_styled_points(make_leaflet_map(), pts, color_by = "group",
                         custom_colors = c(A = "#111111", B = "#222222", C = "#333333"),
                         value_col = "value")
  opts <- marker_opts(m)
  expect_equal(opts$fillOpacity[3], 0)
  expect_identical(opts$color[3], MISSING_VALUE_POINT_COLOR)
  # One legend carrying the groups AND the missing-value entry, so switching
  # the colouring cannot hide it.
  lg <- legend_calls(m)
  expect_length(lg, 1L)
  labs <- as.character(legend_args(m)$labels)
  expect_true(all(c("A", "B", "C") %in% labs))
  expect_identical(labs[length(labs)], MISSING_VALUE_POINT_LABEL)
})
