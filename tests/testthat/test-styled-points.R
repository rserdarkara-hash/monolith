# Leaflet layer helpers (ui_plotting.R): the sampling-point layer
# (point_style_spec(), add_styled_points(), add_point_legend(),
# point_label_text()), add_base_tiles(), the raster image ids the restyler
# retires, and the point pop-ups (ui_formatting.R).

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
# promise. Positions 1-5 (lat, lng, radius, layerId, group) and 9 (popup) come
# from leaflet's own invokeMethod signature; the lat/lng assertions below
# compare against the transformed coordinates, so a swapped pair fails rather
# than passing quietly.

marker_calls <- function(m) Filter(function(c) c$method == "addCircleMarkers", m$x$calls)
marker_args <- function(m) marker_calls(m)[[1]]$args
marker_opts <- function(m) {
  hit <- Filter(function(a) is.list(a) && "fillOpacity" %in% names(a), marker_args(m))
  hit[[1]]
}
legend_calls <- function(m) Filter(function(c) c$method == "addLegend", m$x$calls)
legend_args <- function(m, i = 1L) {
  args <- legend_calls(m)[[i]]$args
  Filter(function(a) is.list(a) && "labels" %in% names(a), args)[[1]]
}
# The points drawn with the style spec of the same rows, as the app draws them.
styled_map <- function(pts, ...) add_styled_points(make_leaflet_map(), pts, point_style_spec(pts, ...))

# ── What add_styled_points draws ───────────────────────────────────────────

test_that("add_styled_points emits the colours each mode promises", {
  pts <- make_sf_points(9)
  pts$group <- factor(rep(c("A", "B", "C"), each = 3))
  grp <- as.character(pts$group)
  custom <- c(A = "#E69F00", B = "#009E73", C = "#56B4E9")

  # No colouring: one cyan for every marker, half-opaque, and no legend.
  m <- styled_map(pts)
  expect_identical(marker_opts(m)$fillColor, "cyan")
  expect_identical(marker_opts(m)$color, "cyan")
  expect_equal(marker_opts(m)$fillOpacity, 0.5)
  expect_length(legend_calls(m), 0L)

  # color_by alone is not enough - without a palette there is nothing to map
  # the groups onto, so it stays cyan and legend-free.
  m <- styled_map(pts, color_by = "group")
  expect_identical(marker_opts(m)$fillColor, "cyan")
  expect_length(legend_calls(m), 0L)

  # A complete palette: each marker carries ITS group's colour, the border
  # turns white, and the legend lists the groups in sorted order.
  m <- styled_map(pts, color_by = "group", custom_colors = custom)
  expect_identical(marker_opts(m)$fillColor, unname(custom[grp]))
  expect_identical(marker_opts(m)$color, "white")
  expect_equal(marker_opts(m)$fillOpacity, 0.85)
  expect_identical(as.character(legend_args(m)$labels), c("A", "B", "C"))
  expect_identical(as.character(legend_args(m)$colors), unname(custom))

  # A partial palette: the named groups keep their colours and the unnamed one
  # is filled in with a distinct colour rather than dropped or recycled.
  m <- styled_map(pts, color_by = "group", custom_colors = custom[c("A", "B")])
  fills <- marker_opts(m)$fillColor
  expect_identical(unique(fills[grp == "A"]), unname(custom[["A"]]))
  expect_identical(unique(fills[grp == "B"]), unname(custom[["B"]]))
  expect_length(unique(fills[grp == "C"]), 1L)
  expect_false(unique(fills[grp == "C"]) %in% custom[c("A", "B")])
  expect_length(legend_args(m)$labels, 3L)

  # A colour-by column that is not in the frame falls back to the default
  # instead of erroring or colouring by something else.
  m <- styled_map(pts, color_by = "nonexistent", custom_colors = custom)
  expect_identical(marker_opts(m)$fillColor, "cyan")
  expect_length(legend_calls(m), 0L)
})

test_that("add_styled_points carries geometry, size and layer ids through", {
  # Nothing to draw: no marker call is issued at all.
  expect_length(marker_calls(styled_map(make_sf_points(6)[0, ])), 0L)

  # A projected input is transformed, not passed through - UTM eastings and
  # northings would be orders of magnitude outside either coordinate range.
  utm <- make_test_points(8)                       # EPSG:32633
  ll <- sf::st_coordinates(sf::st_transform(utm, 4326))
  args <- marker_args(add_styled_points(make_leaflet_map(), utm, point_style_spec(utm), marker_size = 7))
  expect_equal(args[[1]], unname(ll[, "Y"]), tolerance = 1e-9)   # lat
  expect_equal(args[[2]], unname(ll[, "X"]), tolerance = 1e-9)   # lng
  expect_equal(args[[3]], 7)                                     # radius

  # Each marker carries the id the restyle, label and pop-up paths find it by,
  # in the group the overlay clears, and no pop-up travels with it.
  pts <- make_sf_points(4)
  args <- marker_args(add_styled_points(make_leaflet_map(), pts, point_style_spec(pts),
                                        layer_ids = paste0("pt_", 1:4)))
  expect_identical(args[[4]], paste0("pt_", 1:4))                # layerId
  expect_identical(args[[5]], "styled_points")                   # group
  expect_null(args[[9]])                                         # popup
})

test_that("point_style_spec gives an NA group leaflet's grey and every point one look when nothing differs", {
  pts <- make_sf_points(6)
  pts$group <- c("A", "B", NA, "A", NA, "B")
  spec <- point_style_spec(pts, color_by = "group", custom_colors = c(A = "#111111", B = "#222222"))
  # colorFactor()'s na.color; the legend lists the named groups only.
  expect_identical(spec$fill[is.na(pts$group)], rep("#808080", 2))
  expect_identical(spec$fill[!is.na(pts$group)], c("#111111", "#222222", "#111111", "#222222"))
  expect_identical(spec$legend$labels, c("A", "B"))
  # A blank cell (a CSV's empty text field reads as "") is no group either.
  pts$group <- c("A", "", " ", "A", NA, "B")
  spec <- point_style_spec(pts, color_by = "group", custom_colors = c(A = "#111111", B = "#222222"))
  expect_identical(spec$fill[2:3], rep("#808080", 2))
  expect_identical(spec$legend$labels, c("A", "B"))
  expect_identical(point_group_values(c("A", "", "  ", NA)), c("A", NA, NA, NA))

  # No colouring and no missing value: one look, scalars, and nothing to list.
  spec <- point_style_spec(make_sf_points(6), value_col = "value")
  expect_identical(spec[c("fill", "stroke", "fill_opacity")],
                   list(fill = "cyan", stroke = "cyan", fill_opacity = 0.5))
  expect_null(spec$legend)
})

test_that("add_point_legend draws one bottom-left legend or none", {
  expect_length(legend_calls(add_point_legend(make_leaflet_map(), NULL, "lg")), 0L)
  m <- add_point_legend(make_leaflet_map(),
                        list(colors = c("#111111", "#222222"), labels = c("A", "B"), title = "group"),
                        "styled_points_legend")
  expect_length(legend_calls(m), 1L)
  lg <- legend_args(m)
  expect_identical(lg$position, "bottomleft")
  expect_equal(lg$opacity, 0.9)
  expect_identical(lg$layerId, "styled_points_legend")
  expect_identical(as.character(lg$labels), c("A", "B"))
  expect_identical(lg$title, "group")
})

test_that("point_label_text formats each field type", {
  pts <- make_sf_points(4)
  # A character column verbatim, a factor by its labels (not its codes), and a
  # number to two decimals, the only place that rounding happens.
  expect_identical(point_label_text(pts, "label"), pts$label)
  expect_identical(point_label_text(pts, "group"), as.character(pts$group))
  expect_identical(point_label_text(pts, "value"), sprintf("%.2f", pts$value))
  # A missing value has no label: sprintf("%.2f", NA) is the STRING "NA", which
  # would be painted beside the real readings.
  pts$value[c(2, 4)] <- NA_real_
  lab <- point_label_text(pts, "value")
  expect_true(all(is.na(lab[c(2, 4)])))
  expect_identical(lab[-c(2, 4)], sprintf("%.2f", pts$value[-c(2, 4)]))
  # No field, or one the point set lacks: no labels at all.
  expect_null(point_label_text(pts, "nope"))
  expect_null(point_label_text(pts, "none"))
  expect_null(point_label_text(pts, NULL))
})

# ── add_base_tiles ─────────────────────────────────────────────────────────
#
# Every map in the app adds its basemap through this one call, so all five
# providers must come out on identical zoom terms: a layer shallower than the
# map's current zoom would draw blank after a switch.

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
# filtered, so it holds points the surface was not fitted from. Drawn like the
# rest, they would read as samples supporting the map.

test_that("a point with no measured value renders hollow and names itself", {
  pts <- make_sf_points(6)
  pts$value[c(2, 5)] <- NA_real_
  m <- styled_map(pts, value_col = "value")

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
  m <- styled_map(pts, value_col = "value")
  expect_length(legend_calls(m), 0L)
  expect_length(marker_opts(m)$fillOpacity, 1L)   # still the scalar
  # And without value_col the helper cannot know, so nothing changes.
  pts$value[2] <- NA_real_
  expect_length(legend_calls(styled_map(pts)), 0L)
})

test_that("the distinction survives every colour-by mode", {
  pts <- make_sf_points(8)
  pts$value[3] <- NA_real_
  m <- styled_map(pts, color_by = "group",
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

# ── Point pop-ups ──────────────────────────────────────────────────────────

# Five variables in two categories, interleaved, with the first-listed category
# sorting last, so list order and alphabetical order disagree.
popup_test_vars <- function() {
  lapply(list(c("a", "Soil"), c("d", "Climate"), c("b", "Soil"), c("e", "Climate"), c("c", "Soil")),
         function(x) list(actual = x[1], label = toupper(x[1]), category = x[2]))
}

test_that("the pop-up lists what Pop-up Content picks, the mapped variable's category by default", {
  vars <- popup_test_vars()
  expect_identical(var_actuals(vars), c("a", "d", "b", "e", "c"))
  groups <- group_vars_by_category(vars)
  expect_identical(names(groups), c("Soil", "Climate"))
  expect_identical(lapply(groups, var_actuals), list(Soil = c("a", "b", "c"), Climate = c("d", "e")))
  listed <- function(...) var_actuals(popup_variables(vars, ...))

  # "auto" follows the mapped variable: its whole category, in list order; a
  # focus outside the list, or none, lists nothing.
  expect_identical(listed("auto", focus_var = "b"), c("a", "b", "c"))
  expect_identical(listed("auto", focus_var = "e"), c("d", "e"))
  expect_length(popup_variables(vars, "auto", focus_var = "zzz"), 0L)
  expect_length(popup_variables(vars, "auto"), 0L)
  # A category is that category whatever is mapped; one no longer in the list,
  # nothing.
  expect_identical(listed("cat:Climate", focus_var = "b"), c("d", "e"))
  expect_identical(listed("cat:Soil", focus_var = "e"), c("a", "b", "c"))
  expect_length(popup_variables(vars, "cat:Nope"), 0L)
  expect_length(popup_variables(vars, "cat:"), 0L)
  # Every variable, in list order.
  expect_identical(listed("all"), c("a", "d", "b", "e", "c"))
  # A choice keeps the listed variables it names, in list order; none chosen is
  # a choice of none, not a return to the default.
  expect_identical(listed("manual", chosen = c("e", "a", "zzz"), focus_var = "b"), c("a", "e"))
  expect_length(popup_variables(vars, "manual", chosen = character(0), focus_var = "b"), 0L)
  expect_length(popup_variables(vars, "manual", focus_var = "b"), 0L)
})

test_that("Pop-up Content offers each category of the list, between the default and the choice", {
  ch <- popup_content_choices(popup_test_vars())
  expect_identical(unname(unlist(ch)), c("auto", "cat:Soil", "cat:Climate", "all", "manual"))
  expect_identical(names(ch), c("Mapped variable's category", "Categories", "All variables", "Variables I choose"))
  expect_identical(names(ch$Categories), c("Soil", "Climate"))
  # One category is still a group of its own; no variable list, no group.
  expect_identical(popup_content_choices(popup_test_vars()[c(1, 3)])$Categories, c(Soil = "cat:Soil"))
  expect_identical(unname(unlist(popup_content_choices(list()))), c("auto", "all", "manual"))
})

test_that("point_popup_html carries the notes and one row per variable", {
  groups <- group_vars_by_category(popup_test_vars())
  html <- point_popup_html(data.frame(v = NA_real_, .mn_n_rep = 3L, a = 1.23456, check.names = FALSE), groups)
  count <- function(pattern) lengths(regmatches(html, gregexpr(pattern, html, fixed = TRUE)))
  expect_match(html, "Point Details", fixed = TRUE)
  expect_match(html, "No measured value", fixed = TRUE)
  expect_match(html, "Co-located samples: 3", fixed = TRUE)
  expect_match(html, ">1.235<", fixed = TRUE)
  expect_identical(count(">N/A<"), 4L)                            # b, c, d, e: no column
  # One header per category, in list order, and one row per variable.
  expect_identical(count("<td colspan='2'>"), 2L)
  expect_lt(regexpr("Soil", html, fixed = TRUE), regexpr("Climate", html, fixed = TRUE))
  expect_identical(count("<td style='padding: 3px;'>"), 5L)

  # A measured, single sample carries neither note; no variable leaves the notes only.
  html <- point_popup_html(data.frame(v = 5, a = 1), groups)
  expect_false(grepl("No measured value|Co-located", html))
  html <- point_popup_html(data.frame(v = NA_real_), list())
  expect_match(html, "No measured value", fixed = TRUE)
  expect_false(grepl("<tr", html, fixed = TRUE))
})

test_that("point pop-up rows escape markup in labels and values", {
  # Labels come from the variable list and values from the uploaded table:
  # either can carry text a browser would read as markup.
  row <- popup_value_row("<b>K</b> (mg/kg)", "<b>high</b>")
  expect_false(grepl("<b>", row, fixed = TRUE))
  expect_match(row, "&lt;b&gt;K&lt;/b&gt; (mg/kg)", fixed = TRUE)
  expect_match(row, "&lt;b&gt;high&lt;/b&gt;", fixed = TRUE)
  # Numbers keep the four-significant-digit display every result uses.
  expect_match(popup_value_row("pH", 7.123456), ">7.123<", fixed = TRUE)
  expect_match(popup_value_row("pH", NULL), ">N/A<", fixed = TRUE)
  grp <- popup_group_row("Soil & <Water>")
  expect_match(grp, "Soil &amp; &lt;Water&gt;", fixed = TRUE)
})
