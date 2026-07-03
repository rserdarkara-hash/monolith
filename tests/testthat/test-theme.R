# test-theme.R — tests for create_app_theme and export_plot_to_file.

# ── create_app_theme ──────────────────────────────────────────────────────

test_that("create_app_theme returns list with theme, manual_style, map_tiles", {
  result <- create_app_theme(
    light_blue = "#2d5a27", dark_bg = "#22252a",
    content_bg = "#e9ecef", font_family = "Inter",
    map_tiles = "Esri.WorldImagery",
    sidebar_text_color = "#f0f0f0", body_text_color = "#333333",
    header_text_color = "#ffffff"
  )
  expect_type(result, "list")
  expect_true("theme" %in% names(result))
  expect_true("manual_style" %in% names(result))
  expect_true("map_tiles" %in% names(result))
  expect_equal(result$map_tiles, "Esri.WorldImagery")
})

test_that("create_app_theme CSS contains configured font family", {
  result <- create_app_theme(
    light_blue = "#2d5a27", dark_bg = "#22252a",
    content_bg = "#e9ecef", font_family = "Inter",
    map_tiles = "Esri.WorldImagery",
    sidebar_text_color = "#f0f0f0", body_text_color = "#333333",
    header_text_color = "#ffffff"
  )
  expect_match(result$manual_style, "Inter", fixed = TRUE)
})

test_that("create_app_theme with banner_style accent includes border-left", {
  result <- create_app_theme(
    light_blue = "#2d5a27", dark_bg = "#22252a",
    content_bg = "#e9ecef", font_family = "Inter",
    map_tiles = "Esri.WorldImagery",
    banner_style = "accent",
    sidebar_text_color = "#f0f0f0", body_text_color = "#333333",
    header_text_color = "#ffffff"
  )
  expect_match(result$manual_style, "border-left", fixed = TRUE)
})

test_that("create_app_theme with banner_style standard includes max-height", {
  result <- create_app_theme(
    light_blue = "#2d5a27", dark_bg = "#22252a",
    content_bg = "#e9ecef", font_family = "Inter",
    map_tiles = "Esri.WorldImagery",
    banner_style = "standard",
    sidebar_text_color = "#f0f0f0", body_text_color = "#333333",
    header_text_color = "#ffffff"
  )
  expect_match(result$manual_style, "max-height", fixed = TRUE)
})

# ── Themes integrity ──────────────────────────────────────────────────────

test_that("all pre-defined themes produce valid theme objects", {
  expect_true("Deep Forest" %in% names(app_themes))
  expect_true("Obsidian Night" %in% names(app_themes))
  expect_true("Terra Cotta" %in% names(app_themes))
  expect_true("Arctic Mineral" %in% names(app_themes))
  expect_true("Midnight Neon" %in% names(app_themes))
  expect_true("Muted Sage (modified)" %in% names(app_themes))
  expect_true("Slate & Gold" %in% names(app_themes))
  expect_true("Oceanic Deep" %in% names(app_themes))
  expect_true("Sandstone" %in% names(app_themes))
  expect_true("Cyberpunk" %in% names(app_themes))

  for (name in names(app_themes)) {
    th <- app_themes[[name]]
    expect_type(th, "list")
    expect_true("theme" %in% names(th), info = paste(name, "missing theme"))
    expect_true("manual_style" %in% names(th), info = paste(name, "missing manual_style"))
    expect_true("map_tiles" %in% names(th), info = paste(name, "missing map_tiles"))
  }
})
