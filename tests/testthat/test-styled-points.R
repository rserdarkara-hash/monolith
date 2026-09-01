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

# ── Basic functionality ────────────────────────────────────────────────────

test_that("add_styled_points returns a leaflet object", {
  map  <- make_leaflet_map()
  pts  <- make_sf_points(10)
  result <- add_styled_points(map, pts)
  expect_s3_class(result, "leaflet")
})

test_that("add_styled_points with color_by adds legend and group coloring", {
  map  <- make_leaflet_map()
  pts  <- make_sf_points(10)
  result <- add_styled_points(map, pts, color_by = "group")
  expect_s3_class(result, "leaflet")
})

test_that("add_styled_points with custom_colors returns leaflet", {
  map  <- make_leaflet_map()
  pts  <- make_sf_points(10)
  custom <- c(A = "#E69F00", B = "#009E73", C = "#56B4E9")
  result <- add_styled_points(map, pts, color_by = "group",
                               custom_colors = custom)
  expect_s3_class(result, "leaflet")
})

test_that("add_styled_points with show_labels returns leaflet", {
  map  <- make_leaflet_map()
  pts  <- make_sf_points(10)
  result <- add_styled_points(map, pts, show_labels = TRUE,
                               label_field = "group")
  expect_s3_class(result, "leaflet")
})

test_that("add_styled_points with numeric labels returns leaflet", {
  map  <- make_leaflet_map()
  pts  <- make_sf_points(10)
  result <- add_styled_points(map, pts, show_labels = TRUE,
                               label_field = "value")
  expect_s3_class(result, "leaflet")
})

test_that("add_styled_points with popup_fn returns leaflet", {
  map  <- make_leaflet_map()
  pts  <- make_sf_points(8)
  popup_fn <- function(row) paste("Value:", row$value)
  result <- add_styled_points(map, pts, popup_fn = popup_fn)
  expect_s3_class(result, "leaflet")
})

# ── Edge cases ─────────────────────────────────────────────────────────────

test_that("add_styled_points handles 0-row sf object", {
  map  <- make_leaflet_map()
  pts  <- make_sf_points(10)
  pts_empty <- pts[0, ]
  result <- add_styled_points(map, pts_empty)
  expect_s3_class(result, "leaflet")
})

test_that("add_styled_points auto-transforms non-WGS84 CRS", {
  pts <- make_test_points(8)  # UTM zone 33N (EPSG:32633)
  map <- make_leaflet_map()
  result <- add_styled_points(map, pts, marker_size = 5)
  expect_s3_class(result, "leaflet")
})

test_that("add_styled_points handles missing custom_colors entries", {
  map  <- make_leaflet_map()
  pts  <- make_sf_points(10)
  # Custom colors only for A and B — C should get an auto-generated color
  custom <- c(A = "#E69F00", B = "#009E73")
  result <- add_styled_points(map, pts, color_by = "group",
                               custom_colors = custom)
  expect_s3_class(result, "leaflet")
})

test_that("add_styled_points respects marker_size parameter", {
  map  <- make_leaflet_map()
  pts  <- make_sf_points(5)
  result <- add_styled_points(map, pts, marker_size = 8)
  expect_s3_class(result, "leaflet")
})

test_that("add_styled_points handles nonexistent color_by column gracefully", {
  map  <- make_leaflet_map()
  pts  <- make_sf_points(5)
  # "nonexistent" is not a column → falls back to cyan default
  result <- add_styled_points(map, pts, color_by = "nonexistent")
  expect_s3_class(result, "leaflet")
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
