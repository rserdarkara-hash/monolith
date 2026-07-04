# test-palettes.R — tests for get_agro_colors, get_nut_key, get_default_palette,
# get_method_label, get_buffer_multiplier, and generate_group_palette.

# ── get_agro_colors ────────────────────────────────────────────────────────

test_that("get_agro_colors returns correct number of colors", {
  expect_length(get_agro_colors(2), 2)
  expect_length(get_agro_colors(3), 3)
  expect_length(get_agro_colors(4), 4)
  expect_length(get_agro_colors(5), 5)
  expect_length(get_agro_colors(10), 10)
})

test_that("get_agro_colors returns known colors for n = 2", {
  cols <- get_agro_colors(2)
  expect_equal(cols, c("#E69F00", "#009E73"))
})

test_that("get_agro_colors returns known colors for n = 3", {
  cols <- get_agro_colors(3)
  expect_equal(cols, c("#E69F00", "#F0E442", "#009E73"))
})

test_that("get_agro_colors fallback for large n uses colorRampPalette", {
  cols <- get_agro_colors(8)
  expect_length(cols, 8)
  expect_true(all(grepl("^#[0-9A-Fa-f]{6}$", cols)))
})

# ── get_nut_key ────────────────────────────────────────────────────────────

test_that("get_nut_key matches short nutrient codes", {
  expect_equal(get_nut_key("TN"), "TN")
  expect_equal(get_nut_key("P"), "P")
  expect_equal(get_nut_key("K"), "K")
  expect_equal(get_nut_key("Ca"), "Ca")
  expect_equal(get_nut_key("Mg"), "Mg")
  expect_equal(get_nut_key("Fe"), "Fe")
  expect_equal(get_nut_key("Mn"), "Mn")
  expect_equal(get_nut_key("Cu"), "Cu")
  expect_equal(get_nut_key("Zn"), "Zn")
})

test_that("get_nut_key matches full nutrient names", {
  expect_equal(get_nut_key("NITROGEN"), "TN")
  expect_equal(get_nut_key("PHOSPHORUS"), "P")
  expect_equal(get_nut_key("POTASSIUM"), "K")
  expect_equal(get_nut_key("CALCIUM"), "Ca")
  expect_equal(get_nut_key("MAGNESIUM"), "Mg")
  expect_equal(get_nut_key("IRON"), "Fe")
  expect_equal(get_nut_key("MANGANESE"), "Mn")
  expect_equal(get_nut_key("COPPER"), "Cu")
  expect_equal(get_nut_key("ZINC"), "Zn")
})

test_that("get_nut_key is case-insensitive", {
  expect_equal(get_nut_key("tn"), "TN")
  expect_equal(get_nut_key("nitrogen"), "TN")
  expect_equal(get_nut_key("Phosphorus"), "P")
})

test_that("get_nut_key handles OLSEN as P", {
  expect_equal(get_nut_key("OLSEN"), "P")
})

test_that("get_nut_key returns NULL for unrecognized input", {
  expect_null(get_nut_key("pH"))
  expect_null(get_nut_key("Sand"))
  expect_null(get_nut_key("Clay"))
  expect_null(get_nut_key(""))
  expect_null(get_nut_key(NA_character_))
})

# ── get_default_palette ───────────────────────────────────────────────────

test_that("get_default_palette returns nutrient palette when matched", {
  expect_equal(get_default_palette("TN"), "Greens")
  expect_equal(get_default_palette("P"), "Blues")
  expect_equal(get_default_palette("K", label = "Potassium"), "Oranges")
})

test_that("get_default_palette returns category-based palettes", {
  expect_equal(get_default_palette("NDVI", "Environmental Data"), "RdYlBu")
  expect_equal(get_default_palette("B4", "Landsat Data"), "viridis")
  expect_equal(get_default_palette("VV", "Sentinel Data"), "viridis")
  expect_equal(get_default_palette("merged_var", "Merged Data"), "viridis")
  expect_equal(get_default_palette("Slope", "Terrain Data"), "BrBG")
})

test_that("get_default_palette falls back to YlOrRd for unrecognized", {
  expect_equal(get_default_palette("UnknownVar", "Unknown"), "YlOrRd")
  expect_equal(get_default_palette("SomeVar"), "YlOrRd")
})

test_that("get_default_palette handles NULL category", {
  expect_equal(get_default_palette("XYZ", NULL), "YlOrRd")
})

test_that("get_default_palette uses label when var_name doesn't match", {
  expect_equal(get_default_palette("V1", label = "Nitrogen"), "Greens")
  expect_equal(get_default_palette("Col2", label = "MN"), "GnBu")
})

# ── get_method_label ──────────────────────────────────────────────────────

test_that("get_method_label returns full names for all methods", {
  expect_equal(get_method_label("OK"), "Ordinary Kriging")
  expect_equal(get_method_label("UK"), "Universal Kriging")
  expect_equal(get_method_label("RK"), "Regression Kriging")
  expect_equal(get_method_label("RFK"), "Random Forest Kriging")
  expect_equal(get_method_label("CK"), "Co-Kriging")
  expect_equal(get_method_label("IDW"), "IDW")
  expect_equal(get_method_label("TPS"), "Thin Plate Spline")
})

test_that("get_method_label returns input for unknown methods", {
  expect_equal(get_method_label("ABC"), "ABC")
})

test_that("get_method_label returns empty string for empty/NA input", {
  expect_equal(get_method_label(""), "")
  expect_equal(get_method_label(NULL), "")
})

# ── get_buffer_multiplier ─────────────────────────────────────────────────

test_that("get_buffer_multiplier returns correct values per method", {
  expect_equal(get_buffer_multiplier("TPS"), 1.0)
  expect_equal(get_buffer_multiplier("IDW"), 2.0)
  expect_equal(get_buffer_multiplier("OK"), 3.0)
  expect_equal(get_buffer_multiplier("CK"), 3.0)
  expect_equal(get_buffer_multiplier("RK"), 3.0)
  expect_equal(get_buffer_multiplier("RFK"), 3.0)
})

test_that("get_buffer_multiplier returns 2.0 for unknown methods", {
  expect_equal(get_buffer_multiplier("XYZ"), 2.0)
  expect_equal(get_buffer_multiplier(NULL), 2.0)
  expect_equal(get_buffer_multiplier(""), 2.0)
})

# ── generate_group_palette ────────────────────────────────────────────────

test_that("generate_group_palette returns named vector of correct length", {
  groups <- c("Low", "Medium", "High")
  pal <- generate_group_palette(groups, "Set1")
  expect_length(pal, 3)
  expect_named(pal, groups)
  expect_true(all(grepl("^#[0-9A-Fa-f]{6}$", pal)))
})

test_that("generate_group_palette handles single group", {
  pal <- generate_group_palette("Only", "Set2")
  expect_length(pal, 1)
  expect_named(pal, "Only")
})

test_that("generate_group_palette handles empty input", {
  pal <- generate_group_palette(character(0), "Set1")
  expect_length(pal, 0)
})

test_that("generate_group_palette supports Tableau10 palette", {
  groups <- c("A", "B", "C", "D", "E")
  pal <- generate_group_palette(groups, "Tableau10")
  expect_length(pal, 5)
  expect_named(pal, groups)
})

test_that("generate_group_palette colorRamps when n exceeds palette max", {
  groups <- paste0("G", 1:15)
  pal <- generate_group_palette(groups, "Set3")  # Set3 has max 12 colors
  expect_length(pal, 15)
})

# ── Edge cases ────────────────────────────────────────────────────────────

test_that("get_agro_colors returns valid hex for all small n", {
  for (n in 1:7) {
    cols <- get_agro_colors(n)
    expect_length(cols, n)
    expect_true(all(grepl("^#[0-9A-Fa-f]{6}$", cols)))
  }
})

test_that("get_nut_key handles whitespace-padded input", {
  expect_equal(get_nut_key(" TN "), "TN")
  expect_equal(get_nut_key("  P"), "P")
})

test_that("get_nut_key handles numeric-looking string", {
  # "B4" could be a Landsat band — not a nutrient
  expect_null(get_nut_key("B4"))
})

test_that("get_default_palette handles label with extra whitespace", {
  result <- get_default_palette("V1", label = "  Nitrogen  ")
  expect_equal(result, "Greens")
})

test_that("generate_group_palette handles duplicate group names", {
  groups <- c("A", "A", "B")
  pal <- generate_group_palette(groups, "Set1")
  # Should deduplicate or assign same color
  expect_true(length(pal) >= 2)
})
