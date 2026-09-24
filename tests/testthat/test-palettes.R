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
  expect_equal(get_method_label("RK"), "Regression Kriging")
  expect_equal(get_method_label("RFK"), "Random Forest Kriging")
  expect_equal(get_method_label("CK"), "Co-Kriging")
  expect_equal(get_method_label("IDW"), "IDW")
  expect_equal(get_method_label("TPS"), "Thin Plate Spline")
  # The labels are exactly the six engines the Interpolation control offers.
  expect_setequal(names(method_labels), c("OK", "RK", "RFK", "CK", "IDW", "TPS"))
})

test_that("get_method_label returns input for unknown methods", {
  expect_equal(get_method_label("ABC"), "ABC")
})

test_that("get_method_label returns empty string for empty/NA input", {
  expect_equal(get_method_label(""), "")
  expect_equal(get_method_label(NULL), "")
})

test_that("Match Scales counts only where the sidebar shows it", {
  # The sidebar set up for a comparison of predictions ...
  expect_true(match_scales_shown(TRUE, "pred", FALSE, "view_act"))
  expect_true(match_scales_shown(TRUE, "pred_ss", FALSE, "view_act"))
  # ... or the comparison view of a run that has a predicted surface.
  expect_true(match_scales_shown(FALSE, "actual", TRUE, "view_comp"))
  # Hidden: a predictions-only view, an Actual run, a residual view.
  expect_false(match_scales_shown(FALSE, "pred", TRUE, "view_pred"))
  expect_false(match_scales_shown(TRUE, "actual", FALSE, "view_act"))
  expect_false(match_scales_shown(TRUE, "resid", TRUE, "view_resid"))
  expect_false(match_scales_shown(FALSE, "pred", FALSE, "view_comp"))
  expect_false(match_scales_shown(NULL, NULL, NULL, NULL))
  # The checkbox's own conditionalPanel states the same condition.
  root <- normalizePath(file.path(testthat::test_path(), "..", ".."), winslash = "/")
  sidebar <- paste(readLines(file.path(root, "ui_sidebar.R"), warn = FALSE), collapse = "\n")
  expect_match(sidebar, "(input.comp_mode && ['pred', 'pred_ss'].includes(input.value_type)) || (output.disp_has_pred == 'yes' && /^view_comp/.test(input.map_view || ''))",
               fixed = TRUE)
})

test_that("res_mode_label names the resolution logic the sidebar offers", {
  # The run record stores the raw id, so the reader has to print the wording
  # the user chose from: pin the ids to the selector itself, or a renamed
  # choice would be printed back unlabelled.
  root <- normalizePath(file.path(testthat::test_path(), "..", ".."), mustWork = TRUE)
  sidebar <- paste(readLines(file.path(root, "ui_sidebar.R"), warn = FALSE), collapse = "\n")
  expect_match(sidebar,
    'choices = c("Auto (Per Locality)" = "local", "Auto (Global)" = "global", "Fixed" = "fixed")',
    fixed = TRUE)
  expect_equal(res_mode_label("fixed"), "Fixed")
  expect_equal(res_mode_label("global"), "Auto (Global)")
  expect_equal(res_mode_label("local"), "Auto (Per Locality)")
  # An archived run from an older session carries no value at all.
  expect_equal(res_mode_label(NULL), "not recorded")
  expect_equal(res_mode_label(NA), "not recorded")
  # Anything unrecognised is passed through rather than mislabelled.
  expect_equal(res_mode_label("other"), "other")
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

# ── desc_palette_colors (Descriptive Suite palettes) ──────────────────────

test_that("desc_palette_colors returns exactly n colors for every option", {
  pals <- unname(unlist(desc_palette_choices))
  pals <- pals[pals != "default"]
  for (pal in pals) {
    expect_length(desc_palette_colors(pal, 3), 3)
    expect_length(desc_palette_colors(pal, 15), 15)
  }
})

test_that("desc_palette_colors ramps Brewer palettes past their native max", {
  cols <- desc_palette_colors("Set2", 12)  # Set2 native max = 8
  expect_length(cols, 12)
  expect_false(any(is.na(cols)))
  expect_length(unique(cols), 12)
})

test_that("desc_palette_colors uses the Okabe-Ito palette", {
  expect_equal(desc_palette_colors("okabe", 4),
               unname(grDevices::palette.colors(4, palette = "Okabe-Ito")))
})

# ── apply_desc_palette ────────────────────────────────────────────────────

test_that("apply_desc_palette leaves the plot untouched for default/NULL", {
  p <- ggplot2::ggplot(make_test_df(), ggplot2::aes(cat1, a, fill = cat1)) +
    ggplot2::geom_boxplot()
  expect_identical(apply_desc_palette(p, "default"), p)
  expect_identical(apply_desc_palette(p, NULL), p)
})

test_that("apply_desc_palette colours discrete groups beyond a Brewer max", {
  set.seed(1)
  df <- data.frame(g = factor(rep(paste0("G", 1:12), each = 5)), y = rnorm(60))
  p <- ggplot2::ggplot(df, ggplot2::aes(g, y, fill = g)) + ggplot2::geom_boxplot()
  built <- ggplot2::ggplot_build(apply_desc_palette(p, "Set1"))  # Set1 max = 9
  fills <- unique(built$data[[1]]$fill)
  expect_false(any(is.na(fills)))
  expect_length(fills, 12)
})

test_that("apply_desc_palette works on the 2D density heatmap's discrete fill", {
  df <- make_test_df(n = 200)
  p <- generate_advanced_plot(df, vars = c("a", "b"), plot_type = "density_heatmap")
  # regression: the old code added scale_fill_viridis_c/_distiller here, which
  # errors on geom_density_2d_filled's ordered-factor fill
  p2 <- apply_desc_palette(p, "viridis")
  expect_no_error(ggplot2::ggplot_build(p2))
  p3 <- apply_desc_palette(p, "Set1")
  expect_no_error(ggplot2::ggplot_build(p3))
})

test_that("apply_desc_palette applies a continuous gradient for XYZ surface", {
  df <- make_test_df(n = 60)
  p <- generate_advanced_plot(df, vars = c("a", "b", "c"), plot_type = "xyz_surface")
  p2 <- apply_desc_palette(p, "plasma", continuous = TRUE)
  expect_no_error(ggplot2::ggplot_build(p2))
})

# ── Reference class limits (Supervised styling) ───────────────────────────

test_that("the reference class limits are the published ones, with method and unit", {
  # Table S2 of the accompanying manuscript, written out by hand.
  s2 <- data.frame(
    key    = c("TN", "P", "K", "Ca", "Mg", "Fe", "Mn", "Cu", "Zn"),
    method = c("Total N", "Olsen P", "NH₄OAc K", "NH₄OAc Ca", "NH₄OAc Mg",
               "DTPA Fe", "DTPA Mn", "DTPA Cu", "DTPA Zn"),
    unit   = c("%", rep("mg kg⁻¹", 8)),
    low    = c(0.05, 8, 200, 1428, 80, 4, 1.2, 0.3, 1),
    high   = c(0.10, 25, 300, 2857, 160, 6, 3.5, 0.8, 3),
    stringsAsFactors = FALSE)
  expect_identical(names(NUTRIENT_REFERENCE), s2$key)
  for (i in seq_len(nrow(s2))) {
    r <- NUTRIENT_REFERENCE[[s2$key[i]]]
    expect_identical(r$method, s2$method[i], info = s2$key[i])
    expect_identical(r$unit, s2$unit[i], info = s2$key[i])
    expect_identical(r$limits, c(s2$low[i], s2$high[i]), info = s2$key[i])
  }
  # The DTPA classes are local limits, cited with the test they are based on.
  for (k in c("Fe", "Mn", "Cu", "Zn")) {
    expect_match(NUTRIENT_REFERENCE[[k]]$source, "Çokuysal & Erbaş (2004)", fixed = TRUE)
    expect_match(NUTRIENT_REFERENCE[[k]]$source, "Lindsay & Norvell (1978)", fixed = TRUE)
  }
})

test_that("a variable's unit decides whether the reference limits apply", {
  for (u in c("mg/kg", "mg kg-1", "mg kg⁻¹", "ppm", "µg/g", "MG/KG", " mg·kg⁻¹ "))
    expect_identical(reference_unit_status(u, "mg kg⁻¹"), "equivalent", info = u)
  expect_identical(reference_unit_status("%", "%"), "equivalent")
  expect_identical(reference_unit_status("cmol/kg", "mg kg⁻¹"), "different")
  expect_identical(reference_unit_status("g/kg", "%"), "different")
  expect_identical(reference_unit_status("", "mg kg⁻¹"), "empty")
  expect_identical(reference_unit_status(NA, "mg kg⁻¹"), "empty")

  vals <- c(120, 180, 240, 310, 420)
  d <- class_limit_defaults("K", "ppm", 3, vals)
  expect_identical(d$source, "reference")
  expect_identical(d$limits, c(200, 300))
  d_empty <- class_limit_defaults("K", "", 3, vals)
  expect_identical(d_empty$source, "reference")
  expect_match(paste(class_limit_note(d_empty, ""), collapse = " "), "No unit is recorded")
  d_cmol <- class_limit_defaults("K", "cmol/kg", 3, vals)
  expect_identical(d_cmol$source, "quantile")
  expect_match(paste(class_limit_note(d_cmol, "cmol/kg"), collapse = " "),
               "this variable is recorded in cmol/kg", fixed = TRUE)
})

test_that("reference limits prefill only a three-class split; otherwise data quantiles", {
  vals <- c(3.1, 5.8, 9.4, 12.2, 17.5, 21.0, 26.3, 30.9)
  for (k in c(2, 4, 5)) {
    d <- class_limit_defaults("P", "mg/kg", k, vals)
    expect_identical(d$source, "quantile", info = k)
    expect_identical(d$limits, stats::quantile(vals, probs = seq_len(k - 1) / k,
                                               type = 7, names = FALSE), info = k)
    expect_match(class_limit_note(d)[2], "define three classes", fixed = TRUE)
  }
  # A variable with no reference gets quantiles at any class count.
  d_ph <- class_limit_defaults("ph", "", 3, vals)
  expect_identical(d_ph$source, "quantile")
  expect_identical(d_ph$limits, unname(stats::quantile(vals, c(1, 2) / 3, type = 7)))
  expect_identical(class_limit_note(d_ph), "Data quantiles (not agronomic limits).")
  expect_match(class_limit_note(class_limit_defaults("P", "mg/kg", 3, vals))[1],
               "Reference limits: Olsen P, mg kg⁻¹, Yüksel & Ekinci (2019). Low < 8 ≤ Moderate < 25 ≤ High.",
               fixed = TRUE)
})

test_that("Supervised limits stay in their boxes, and classify only their own variable", {
  local_mocked_bindings(shinyApp = .real_shinyApp, .package = "shiny")
  withr::local_dir(proj_root)
  box_value <- function(html, i) {
    m <- regmatches(html, regexpr(sprintf('id="agro_limit_%d"[^>]*value="[^"]*"', i), html))
    as.numeric(sub('.*value="([^"]*)"$', "\\1", m))
  }
  k_vals <- c(150, 180, 220, 260, 310, 400)
  shiny::testServer(function(input, output, session) {
    rv <- shiny::reactiveValues(
      user_data = data.frame(k = k_vals, k_cve = k_vals + c(5, -5, 10, -10, -10, -10),
                             ph = c(6.1, 6.4, 6.9, 7.2, 7.6, 8.0)),
      mapping = list(vars = list(
        list(actual = "k", pred = "k_cve", pred_ss = NULL, label = "K", category = "Soil", unit = "mg/kg"),
        list(actual = "ph", pred = NULL, pred_ss = NULL, label = "pH", category = "Soil", unit = ""))),
      disp = NULL, rast = NULL, rast_pred = NULL)
    source(file.path(proj_root, "server_run_config.R"), local = TRUE)
  }, {
    session$setInputs(var_id = "k", value_type = "actual", color_style = "agro",
                      agro_method = "limits", agro_n_classes = 3)
    # K in mg/kg at three classes opens on the published limits.
    html <- output$agro_options$html
    expect_identical(c(box_value(html, 1), box_value(html, 2)), c(200, 300))

    # The boxes are rebuilt whenever their defaults' inputs move (here a view
    # change before any run); for the same variable and class count they keep
    # what is on screen.
    session$setInputs(agro_limit_1 = 210, agro_limit_2 = 320)
    session$setInputs(value_type = "pred")
    html <- output$agro_options$html
    expect_identical(c(box_value(html, 1), box_value(html, 2)), c(210, 320))
    # Another class count opens on its own defaults, the data quantiles.
    session$setInputs(agro_n_classes = 4)
    q <- stats::quantile(k_vals, c(1, 2, 3) / 4, type = 7, names = FALSE)
    html <- output$agro_options$html
    expect_equal(vapply(1:3, function(i) box_value(html, i), numeric(1)), signif(q, 4))

    # Applied limits classify the variable they were applied for, and no other:
    # they are numbers in that variable's units.
    session$setInputs(agro_n_classes = 3)
    session$setInputs(agro_limit_1 = 210, agro_limit_2 = 320)
    session$setInputs(agro_apply = 1)
    expect_identical(class_breaks_act(), c(210, 320))
    expect_null(output$agro_pending_note$html)
    session$setInputs(var_id = "ph", value_type = "actual")
    expect_null(class_breaks_act())
    expect_match(output$agro_pending_note$html, "Class settings are staged", fixed = TRUE)
    session$setInputs(var_id = "k")
    expect_identical(class_breaks_act(), c(210, 320))
  })
})

test_that("a value equal to a class limit falls in the upper class, on the map and in the agreement table", {
  cb <- class_breaks_matrix(c(8, 25))
  vals <- c(7.99, 8, 24.99, 25, 30)
  r <- terra::rast(nrows = 1, ncols = 5, xmin = 0, xmax = 5, ymin = 0, ymax = 1, vals = vals)
  # The map's own call (build_classification_params' matrix, right = FALSE).
  expect_equal(as.vector(terra::values(terra::classify(r, cb$rcl_mat, right = FALSE))),
               c(1, 2, 2, 3, 3))
  ag <- compute_agreement_metrics(vals, vals, method = "agro",
                                  params = list(rcl_mat = cb$rcl_mat, labels = c("Low", "Med", "High")))
  expect_identical(as.character(ag$actual_bin), c("Low", "Med", "Med", "High", "High"))
})
