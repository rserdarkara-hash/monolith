# test-styled-points.R — tests for add_styled_points from ui_helpers.R.

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
