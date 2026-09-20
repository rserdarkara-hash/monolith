# test-base-plot-styler.R — tests for generate_base_plot and apply_styler_theme
# from ui_helpers.R.  These functions depend on Shiny's `input` reactive list;
# we mock it as a plain named list since both functions only read from it with
# `$` / `%||%` (never rely on reactivity).

# ── Mock input ─────────────────────────────────────────────────────────────

mock_input_full <- list(
  # An empty title box, spelled out: `$` on a plain list partial-matches, so
  # without this entry input$styler_title returns styler_title_size and every
  # styled plot is titled "16" instead of its item label. Shiny's real `input`
  # does not partial-match, so the mock has to say what it means.
  styler_title         = "",
  palette_select       = "YlOrRd",
  color_style          = "continuous",
  styler_high_contrast = FALSE,
  styler_title_size    = 16,
  styler_base_size     = 12,
  styler_x_size        = 12,
  styler_y_size        = 12,
  styler_label_size    = 10,
  styler_legend_size   = 10,
  styler_font_family   = "sans",
  styler_legend_pos    = "right",
  styler_legend_dir    = "auto",
  styler_legend_key_size = 1.0,
  styler_legend_text_angle = 0,
  styler_margin_t      = 10,
  styler_margin_r      = 10,
  styler_margin_b      = 10,
  styler_margin_l      = 15,
  styler_x_title       = "X Axis",
  styler_y_title       = "Y Axis"
)

mock_input_minimal <- list()

# ── generate_base_plot ─────────────────────────────────────────────────────

test_that("generate_base_plot returns item$obj for non-map type", {
  p <- ggplot2::ggplot(mtcars, ggplot2::aes(wt, mpg)) +
    ggplot2::geom_point()
  item <- list(type = "plot", obj = p, label = "Test Plot")
  result <- generate_base_plot(item, mock_input_full)
  expect_s3_class(result, "ggplot")
})

test_that("generate_base_plot returns item$obj for histogram type", {
  p <- ggplot2::ggplot(mtcars, ggplot2::aes(mpg)) +
    ggplot2::geom_histogram()
  item <- list(type = "histogram", obj = p, label = "Hist")
  result <- generate_base_plot(item, mock_input_full)
  expect_s3_class(result, "ggplot")
})

test_that("generate_base_plot handles NULL agro_params for non-map", {
  p <- ggplot2::ggplot(mtcars, ggplot2::aes(wt, mpg)) +
    ggplot2::geom_point()
  item <- list(type = "plot", obj = p, label = "Test")
  result <- generate_base_plot(item, mock_input_full, agro_params = NULL)
  expect_s3_class(result, "ggplot")
})

# ── generate_base_plot: map kinds vs agro/bin classification ───────────────
# Agronomic/binned class limits are defined on the variable's concentration
# units, so only "value" maps may be classified; "residual" maps must keep a
# diverging continuous scale and "uncertainty" maps a sequential continuous
# scale, regardless of the active styling.

make_test_wrapped_raster <- function(seed = 7) {
  set.seed(seed)
  r <- terra::rast(nrows = 10, ncols = 10,
                   xmin = 450000, xmax = 451000,
                   ymin = 5800000, ymax = 5801000,
                   crs = "EPSG:32633")
  terra::values(r) <- rnorm(100, mean = 50, sd = 15)
  terra::wrap(r)
}

make_test_agro_params <- function() {
  brks <- c(-Inf, 40, 60, Inf)
  list(
    brks = brks,
    rcl_mat = matrix(c(brks[1:3], brks[2:4], 1:3), ncol = 3),
    colors = c("#d73027", "#fee08b", "#1a9850"),
    labels = c("Low", "Med", "High"),
    leg_labels = c("< 40", "40 - 60", "> 60"),
    n_c = 3
  )
}

fill_scale_of <- function(p) {
  scales <- p$scales$scales
  fills <- Filter(function(s) "fill" %in% s$aesthetics, scales)
  expect_true(length(fills) >= 1)
  fills[[1]]
}

test_that("an exported map's legend names the variable, its unit and its layer", {
  # The export used to leave the legend untitled while the Map Viewer names it.
  expect_equal(map_legend_title("Soil pH", ""), "Soil pH")
  expect_equal(map_legend_title("Total N", "%"), "Total N %")
  expect_equal(map_legend_title("Total N", "%", "se"), "SE: Total N %")
  expect_equal(map_legend_title("Total N", "%", "var"), "Variance: Total N (%)^2")
  expect_equal(map_legend_title("Total N", "", "var"), "Variance: Total N (squared units)")
  expect_equal(map_legend_title("Total N", "%", "resid"), "Resid: Total N")

  for (style in c("cont", "agro")) {
    inp <- mock_input_full; inp$color_style <- style
    item <- list(type = "map", obj = make_test_wrapped_raster(), kind = "value",
                 label = "Total N - Actual Map - Ordinary Kriging", legend = "Total N %")
    p <- generate_base_plot(item, inp, agro_params = make_test_agro_params())
    expect_equal(fill_scale_of(p)$name, "Total N %")
  }
  # The figure's title is the registry label, which names the method. The
  # mock's empty styler_title (see the fixture) is the empty title box.
  styled <- generate_styled_plot(list(type = "map", obj = make_test_wrapped_raster(), kind = "value",
                                      label = "Total N - Actual Map - Ordinary Kriging",
                                      legend = "Total N %"), mock_input_full)
  expect_equal(styled$labels$title, "Total N - Actual Map - Ordinary Kriging")
})

test_that("an exported file's name carries the item, the method and the time", {
  expect_equal(export_file_name("Export", "map_actual", "OK", "20260919_120000", "tif"),
               "Export_map_actual_OK_20260919_120000.tif")
  expect_equal(export_file_name("Batch_Statistics", NULL, "RK", "t", "xlsx"),
               "Batch_Statistics_RK_t.xlsx")
  expect_equal(export_file_name("Export", "x", NULL, "t", "png"), "Export_x_t.png")
})

test_that("value maps ARE classified under agro styling", {
  input_agro <- mock_input_full
  input_agro$color_style <- "agro"
  item <- list(type = "map", obj = make_test_wrapped_raster(),
               label = "pH - Actual Map", kind = "value")
  p <- generate_base_plot(item, input_agro, agro_params = make_test_agro_params())
  expect_s3_class(fill_scale_of(p), "ScaleDiscrete")
})

test_that("point error maps are NOT classified under agro styling", {
  input_agro <- mock_input_full
  input_agro$color_style <- "agro"
  item <- list(type = "map", obj = make_test_wrapped_raster(),
               label = "pH - ML Predictions Point Error Map", kind = "residual")
  p <- generate_base_plot(item, input_agro, agro_params = make_test_agro_params())
  expect_s3_class(fill_scale_of(p), "ScaleContinuous")
})

test_that("residual maps keep a symmetric diverging domain under agro styling", {
  input_agro <- mock_input_full
  input_agro$color_style <- "agro"
  item <- list(type = "map", obj = make_test_wrapped_raster(),
               label = "pH - ML Predictions Residual Map (Delta)", kind = "residual")
  p <- generate_base_plot(item, input_agro, agro_params = make_test_agro_params())
  sc <- fill_scale_of(p)
  expect_s3_class(sc, "ScaleContinuous")
  lims <- sc$limits
  expect_equal(lims[1], -lims[2])
})

test_that("uncertainty maps are NOT classified under agro or bin styling", {
  for (style in c("agro", "bin")) {
    input_s <- mock_input_full
    input_s$color_style <- style
    item <- list(type = "map", obj = make_test_wrapped_raster(),
                 label = "pH - Uncertainty Map (SE - Actual)", kind = "uncertainty")
    p <- generate_base_plot(item, input_s, agro_params = if (style == "agro") make_test_agro_params() else NULL)
    sc <- fill_scale_of(p)
    expect_s3_class(sc, "ScaleContinuous")
    # scale_fill_fermenter/viridis_b (binned) would be a ScaleBinned
    expect_false(inherits(sc, "ScaleBinned"))
  }
})

test_that("legacy archived items without kind fall back to label matching", {
  input_agro <- mock_input_full
  input_agro$color_style <- "agro"
  # Simulates a registry item archived before the kind field existed
  item <- list(type = "map", obj = make_test_wrapped_raster(),
               label = "pH - ML Predictions Point Error Map")
  p <- generate_base_plot(item, input_agro, agro_params = make_test_agro_params())
  expect_s3_class(fill_scale_of(p), "ScaleContinuous")
})

# ── point error maps (sf points, mirroring the viewer's Point Residuals) ────

make_test_resid_points <- function(n = 12, seed = 11) {
  pts <- make_test_points(n = n, seed = seed)
  pts$resid <- pts$v - pts$pv
  pts$loc <- "LocA"
  pts[, c("resid", "loc")]
}

test_that("point error map items render as sf point layers, not rasters", {
  item <- list(type = "map",
               obj = list(pts = make_test_resid_points(), bound = NULL),
               label = "pH - ML Predictions Point Error Map", kind = "residual")
  p <- generate_base_plot(item, mock_input_full)
  expect_s3_class(p, "ggplot")
  expect_true(any(vapply(p$layers, function(l) inherits(l$geom, "GeomSf"), logical(1))))
  sc <- fill_scale_of(p)
  expect_s3_class(sc, "ScaleContinuous")
  # diverging scale must stay centered on zero
  lims <- sc$limits
  expect_equal(lims[1], -lims[2])
})

test_that("point error map adds the boundary outline when supplied", {
  pts <- make_test_resid_points()
  bound <- sf::st_as_sfc(sf::st_bbox(pts))
  item <- list(type = "map", obj = list(pts = pts, bound = bound),
               label = "pH - ML Predictions Point Error Map", kind = "residual")
  p <- generate_base_plot(item, mock_input_full)
  n_sf_layers <- sum(vapply(p$layers, function(l) inherits(l$geom, "GeomSf"), logical(1)))
  expect_equal(n_sf_layers, 2)
})

test_that("point error maps ignore agro classification", {
  input_agro <- mock_input_full
  input_agro$color_style <- "agro"
  item <- list(type = "map", obj = list(pts = make_test_resid_points(), bound = NULL),
               label = "pH - ML Predictions Point Error Map", kind = "residual")
  p <- generate_base_plot(item, input_agro, agro_params = make_test_agro_params())
  expect_s3_class(fill_scale_of(p), "ScaleContinuous")
})

# ── resolve_resid_palette ───────────────────────────────────────────────────

test_that("resolve_resid_palette defaults to RdBu and honors an explicit choice", {
  expect_equal(resolve_resid_palette(list()), "RdBu")
  expect_equal(resolve_resid_palette(list(styler_resid_palette = "BrBG")), "BrBG")
})

test_that("resolve_resid_palette forces colorblind safety only when needed", {
  expect_equal(resolve_resid_palette(
    list(styler_resid_palette = "Spectral", styler_high_contrast = TRUE)), "PuOr")
  expect_equal(resolve_resid_palette(
    list(styler_resid_palette = "RdYlGn", styler_high_contrast = TRUE)), "PuOr")
  # already colorblind-safe choices are respected under high contrast
  expect_equal(resolve_resid_palette(
    list(styler_resid_palette = "RdBu", styler_high_contrast = TRUE)), "RdBu")
  expect_equal(resolve_resid_palette(
    list(styler_resid_palette = "PiYG", styler_high_contrast = TRUE)), "PiYG")
})

test_that("point error map honors the selected diverging palette", {
  input_pal <- mock_input_full
  input_pal$styler_resid_palette <- "PuOr"
  item <- list(type = "map",
               obj = list(pts = make_test_resid_points(), bound = NULL),
               label = "pH - ML Predictions Point Error Map", kind = "residual")
  p <- generate_base_plot(item, input_pal)
  built <- ggplot2::ggplot_build(p)
  fills <- built$data[[1]]$fill
  # PuOr endpoints are orange/purple: no pure RdBu red should appear
  expect_false(any(grepl("^#67001F$", toupper(fills))))
  expect_true(all(!is.na(fills)))
})

# ── apply_styler_theme ─────────────────────────────────────────────────────
#
# apply_styler_theme's whole job is to put the styler's controls onto the
# plot's theme. It returns `p + theme_minimal() + theme(...)`, so a class check
# holds for any implementation that does not raise - including one that ignores
# every control. These read the theme back instead.
#
# The high-contrast switch is NOT part of this function: styler_high_contrast
# is read by generate_base_plot and resolve_resid_palette, which have their own
# tests above.

styler_plot <- function() {
  ggplot2::ggplot(mtcars, ggplot2::aes(wt, mpg, color = factor(cyl))) +
    ggplot2::geom_point() + ggplot2::labs(title = "Test Plot")
}

test_that("apply_styler_theme uses the slider point sizes verbatim", {
  # No export-time rescaling: a point in the styler is a point on the page, so
  # the theme must carry the slider values themselves (the former `calibration`
  # multiplier existed only to offset showtext's fixed-dpi text rendering).
  res <- apply_styler_theme(styler_plot(), mock_input_full,
                            item_label = "Test Label", item_type = "plot")
  th <- res$theme
  expect_equal(th$plot.title$size, mock_input_full$styler_title_size)
  expect_equal(th$plot.subtitle$size, mock_input_full$styler_title_size * 0.8)
  expect_equal(th$axis.title.x$size, mock_input_full$styler_x_size)
  expect_equal(th$axis.title.y$size, mock_input_full$styler_y_size)
  expect_equal(th$axis.text$size, mock_input_full$styler_label_size)
  expect_equal(th$legend.text$size, mock_input_full$styler_legend_size)
  expect_equal(th$legend.title$size, mock_input_full$styler_legend_size)
  expect_equal(th$text$size, mock_input_full$styler_base_size)
  expect_equal(th$text$family, mock_input_full$styler_font_family)
  expect_equal(as.numeric(th$legend.key.size), mock_input_full$styler_legend_key_size)
  expect_equal(as.numeric(th$plot.margin), c(10, 10, 10, 15))

  # The item label becomes the title; the axis-title overrides are verbatim.
  expect_equal(res$labels$title, "Test Label")
  expect_equal(res$labels$x, "X Axis")
  expect_equal(res$labels$y, "Y Axis")
})

test_that("an empty styler input falls back to the documented defaults", {
  # The styler panel can be collapsed or not yet rendered, in which case every
  # input reads NULL. mock_input_full has fonts and sizes, so it cannot
  # exercise this path at all.
  res <- apply_styler_theme(styler_plot(), mock_input_minimal,
                            item_label = "", item_type = "plot")
  th <- res$theme
  expect_equal(th$plot.title$size, 16)
  expect_equal(th$axis.title.x$size, 12)
  expect_equal(th$axis.title.y$size, 12)
  expect_equal(th$axis.text$size, 10)
  expect_equal(th$legend.text$size, 10)
  expect_equal(th$text$size, 12)
  expect_equal(th$text$family, "sans")
  expect_equal(th$legend.position, "right")
  expect_equal(th$legend.direction, "vertical")
  expect_equal(th$legend.text$angle, 0)
  expect_equal(as.numeric(th$legend.key.size), 1)
  # No axis-title override means the plot keeps its own mapping-derived labels.
  expect_null(res$labels$x)
  expect_null(res$labels$y)
})

test_that("legend placement, direction and text angle reach the theme", {
  leg <- function(...) {
    apply_styler_theme(styler_plot(), modifyList(mock_input_full, list(...)),
                       item_label = "", item_type = "plot")$theme
  }

  # "auto" is resolved here, not left for ggplot: horizontal under a
  # bottom/top legend, vertical beside one.
  for (pos in c("bottom", "top")) {
    th <- leg(styler_legend_pos = pos, styler_legend_dir = "auto")
    expect_equal(th$legend.position, pos, info = pos)
    expect_equal(th$legend.direction, "horizontal", info = pos)
  }
  for (pos in c("right", "left")) {
    th <- leg(styler_legend_pos = pos, styler_legend_dir = "auto")
    expect_equal(th$legend.position, pos, info = pos)
    expect_equal(th$legend.direction, "vertical", info = pos)
  }

  # An explicit direction overrides the position's default.
  expect_equal(leg(styler_legend_pos = "right",
                   styler_legend_dir = "horizontal")$legend.direction, "horizontal")

  # A rotated legend label needs both justifications centred, or it drifts off
  # its key; at angle 0 they must stay unset so ggplot's own defaults apply.
  th <- leg(styler_legend_text_angle = 90)
  expect_equal(th$legend.text$angle, 90)
  expect_equal(th$legend.text$hjust, 0.5)
  expect_equal(th$legend.text$vjust, 0.5)
  th0 <- leg(styler_legend_text_angle = 0)
  expect_equal(th0$legend.text$angle, 0)
  expect_null(th0$legend.text$hjust)
  expect_null(th0$legend.text$vjust)
})

test_that("map_combined assembles two named panes on its own legend defaults", {
  p_obj <- list(p1 = styler_plot(),
                p2 = ggplot2::ggplot(mtcars, ggplot2::aes(hp, qsec)) + ggplot2::geom_point())

  # Two maps side by side default to a shared horizontal legend underneath
  # with rotated labels, which is not what a single plot defaults to.
  res <- apply_styler_theme(p_obj, mock_input_minimal,
                            item_label = "Combined", item_type = "map_combined")
  expect_s3_class(res, "patchwork")
  expect_equal(res$theme$legend.position, "bottom")
  expect_equal(res$theme$legend.direction, "horizontal")
  expect_equal(res$theme$legend.text$angle, 90)
  expect_equal(res[[1]]$labels$title, "Actual")
  expect_equal(res[[2]]$labels$title, "Predicted")

  # An explicit choice still wins, and the panes shrink their keys and margins.
  res_f <- apply_styler_theme(p_obj, mock_input_full,
                              item_label = "Combined", item_type = "map_combined")
  expect_equal(res_f$theme$legend.position, "right")
  expect_equal(res_f$theme$legend.direction, "vertical")
  expect_equal(res_f$theme$legend.text$angle, 0)
  expect_equal(res_f[[1]]$theme$plot.title$size,
               mock_input_full$styler_title_size * 0.85)
  expect_equal(as.numeric(res_f[[1]]$theme$legend.key.size),
               mock_input_full$styler_legend_key_size * 0.6)
  expect_equal(as.numeric(res_f[[1]]$theme$plot.margin),
               c(10, 10, 10, 15) * 0.3)
})

# ── GeoTIFF / GIS export routing ────────────────────────────────────────────
# The styler offers GeoTIFF only for registry items that really hold one
# raster: writing a paired comparison or a point layer under a .tif name would
# hand the user a corrupt file.

test_that("export_raster_payload unwraps a single-raster map item", {
  item <- list(type = "map", obj = make_test_wrapped_raster(), label = "pH - Actual Map")
  r <- export_raster_payload(item)
  expect_s4_class(r, "SpatRaster")
  expect_equal(terra::ncell(r), 100)
})

test_that("export_raster_payload rejects the two non-raster map payloads", {
  comparison <- list(type = "map_combined",
                     obj = list(act = make_test_wrapped_raster(),
                                pre = make_test_wrapped_raster(seed = 8)),
                     label = "pH - Actual vs Predicted Comparison")
  point_err <- list(type = "map",
                    obj = list(pts = make_test_resid_points(), bound = NULL),
                    label = "pH - ML Predictions Point Error Map")
  expect_null(export_raster_payload(comparison))
  expect_null(export_raster_payload(point_err))
  expect_null(export_raster_payload(list(type = "table", obj = mtcars, label = "t")))
  expect_null(export_raster_payload(NULL))
})

# ── Derived (uncertainty) registry items ────────────────────────────────────
# A variance/SE item stores a derivation of the surface registered beside it,
# not a second copy of its values; export_item_obj() rebuilds it on demand.

make_test_kriging_raster <- function() {
  set.seed(11)
  r <- terra::rast(nrows = 10, ncols = 10,
                   xmin = 450000, xmax = 451000,
                   ymin = 5800000, ymax = 5801000,
                   crs = "EPSG:32633")
  terra::values(r) <- rnorm(100, mean = 50, sd = 15)
  v <- r
  terra::values(v) <- runif(100, min = 1, max = 9)
  out <- c(r, v)
  names(out) <- c("var1.pred", "var1.var")
  out
}

test_that("export_item_obj returns a stored payload unchanged", {
  item <- list(type = "map", obj = make_test_wrapped_raster(), label = "pH - Actual Map")
  expect_identical(export_item_obj(item), item$obj)
  expect_null(export_item_obj(NULL))
})

test_that("export_item_obj rebuilds the variance and SE bands from the surface", {
  src <- make_test_kriging_raster()
  var_item <- list(type = "map", obj = NULL, kind = "uncertainty",
                   label = "pH - Uncertainty Map (Variance - Actual)",
                   derived = list(src = src, layer = "var1.var", name = "var1.var"))
  se_item <- list(type = "map", obj = NULL, kind = "uncertainty",
                  label = "pH - Uncertainty Map (SE - Actual)",
                  derived = list(src = src, layer = "var1.var", fun = "sqrt",
                                 name = "var1.se"))

  v <- export_item_obj(var_item)
  expect_s4_class(v, "SpatRaster")
  expect_equal(terra::nlyr(v), 1L)
  expect_equal(names(v), "var1.var")
  expect_equal(terra::values(v)[, 1], terra::values(src[["var1.var"]])[, 1])

  se <- export_item_obj(se_item)
  # The layer name travels into the GeoTIFF as the band description, so the
  # square root must not describe itself as a variance.
  expect_equal(names(se), "var1.se")
  expect_equal(terra::values(se)[, 1], sqrt(terra::values(src[["var1.var"]])[, 1]))

  # The source must not be touched by either derivation.
  expect_equal(names(src), c("var1.pred", "var1.var"))
})

test_that("a derived item routes through the raster and plot export paths", {
  src <- make_test_kriging_raster()
  item <- list(type = "map", obj = NULL, kind = "uncertainty",
               label = "pH - Uncertainty Map (SE - Actual)",
               derived = list(src = src, layer = "var1.var", fun = "sqrt",
                              name = "var1.se"))
  # GeoTIFF export is offered for it, and the styler preview builds.
  r <- export_raster_payload(item)
  expect_s4_class(r, "SpatRaster")
  expect_equal(names(r), "var1.se")
  expect_s3_class(generate_base_plot(item, mock_input_full), "ggplot")
})

test_that("a derived item whose source lacks the band yields no payload", {
  src <- make_test_kriging_raster()[["var1.pred"]]
  item <- list(type = "map", obj = NULL, kind = "uncertainty", label = "x",
               derived = list(src = src, layer = "var1.var", name = "var1.var"))
  expect_null(export_item_obj(item))
  expect_null(export_raster_payload(item))
})

test_that("export_sheet_name drops exactly the variable-label prefix", {
  # a label that itself contains " - ": splitting at the first " - " kept the
  # rest of the variable label and cut the locality and table name instead
  lab <- "Na - exchangeable (mg/kg)"
  full <- paste(lab, "- Kale - Model CV Metrics (Actual)")
  nm <- export_sheet_name(full, "table_cv_loc_Kale", character(0), lab)
  expect_equal(nm, "Kale _ Model CV Metrics _Actual")
  expect_lte(nchar(nm), 31)
  # without the variable label nothing is guessed away
  expect_match(export_sheet_name(full, "table_cv_loc_Kale"), "^Na _ exchangeable")
})

test_that("export_sheet_name keeps names unique, ignoring case as Excel does", {
  lab <- "pH"
  a <- export_sheet_name("pH - Kale - Model CV Metrics (Actual)", "table_cv_loc_Kale",
                         character(0), lab)
  b <- export_sheet_name("pH - Kale - Model CV Metrics (Actual)", "table_cv_pre_loc_Kale",
                         a, lab)
  expect_true(endsWith(b, "_Pre"))
  expect_lte(nchar(b), 31)
  c3 <- export_sheet_name("pH - Kale - Model CV Metrics (Actual)", "x", c(a, b), lab)
  expect_false(tolower(c3) %in% tolower(c(a, b)))

  # "Kale" and "KALE" are the same sheet name to Excel
  expect_equal(export_sheet_name("t - Kale", "x", "KALE", "t"), "Kale_2")
  # an empty label falls back to the id, then to a positional name
  expect_equal(export_sheet_name("", "table_x", character(0)), "table_x")
  expect_equal(export_sheet_name("", "", c("a", "b")), "Table_3")
})

test_that("styler_format_ext maps the GeoTIFF token to .tif", {
  expect_equal(styler_format_ext("gtiff"), "tif")
  expect_equal(styler_format_ext("tiff"), "tiff")
  expect_equal(styler_format_ext("jpg"), "jpg")
  expect_equal(styler_format_ext(NULL), "png")
})

test_that("write_geotiff round-trips values, CRS and extent", {
  r <- terra::unwrap(make_test_wrapped_raster())
  f <- tempfile(fileext = ".tif")
  on.exit(unlink(f), add = TRUE)

  write_geotiff(r, f)
  back <- terra::rast(f)

  expect_equal(terra::crs(back, describe = TRUE)$code, "32633")
  expect_equal(as.vector(terra::ext(back)), as.vector(terra::ext(r)))
  expect_equal(terra::values(back)[, 1], terra::values(r)[, 1], tolerance = 1e-6)
})

test_that("write_geotiff writes a real GeoTIFF even when the path has no extension", {
  # Shiny hands download handlers a temporary path; terra picks its driver from
  # the extension, so a bare path must not silently produce a non-TIFF.
  r <- terra::unwrap(make_test_wrapped_raster())
  f <- tempfile()
  on.exit(unlink(f), add = TRUE)

  write_geotiff(r, f)
  expect_true(file.exists(f))
  back <- terra::rast(f)
  expect_equal(terra::ncell(back), terra::ncell(r))
})

test_that("write_geotiff keeps every layer of a kriging surface", {
  r <- terra::unwrap(make_test_wrapped_raster())
  names(r) <- "var1.pred"
  v <- r; names(v) <- "var1.var"; terra::values(v) <- abs(terra::values(r)) / 10
  stacked <- c(r, v)
  f <- tempfile(fileext = ".tif")
  on.exit(unlink(f), add = TRUE)

  write_geotiff(stacked, f)
  back <- terra::rast(f)
  expect_equal(terra::nlyr(back), 2)
  expect_equal(names(back), c("var1.pred", "var1.var"))
})

test_that("write_geotiff refuses a payload that is not a raster", {
  expect_error(write_geotiff(NULL, tempfile(fileext = ".tif")), "raster")
})

# gdalinfo's JSON report: per-band metadata and the dataset's own tags.
geotiff_info <- function(f) {
  jsonlite::fromJSON(sf::gdal_utils("info", f, options = "-json", quiet = TRUE),
                     simplifyVector = FALSE)
}

test_that("write_geotiff writes real band statistics and the supplied tags", {
  r <- terra::unwrap(make_test_wrapped_raster())
  names(r) <- "var1.pred"
  v <- r; names(v) <- "var1.var"; terra::values(v) <- abs(terra::values(r)) / 10
  stacked <- c(r, v)
  stacked[1:3] <- NA                     # a masked edge, as a boundary clip leaves
  f <- tempfile(fileext = ".tif"); plain <- tempfile(fileext = ".tif")
  on.exit(unlink(c(f, plain, paste0(c(f, plain), ".aux.xml"))), add = TRUE)

  write_geotiff(stacked, f, tags = c(MONOLITH_VARIABLE = "Soil pH",
                                     MONOLITH_UNIT = "",
                                     MONOLITH_METHOD = "Ordinary Kriging",
                                     MONOLITH_PRODUCT = "Actual Map\n(line two)"))
  expect_false(file.exists(paste0(f, ".aux.xml")))

  # The tagging pass moves no value: the cells equal what a plain writeRaster
  # stores, bit for bit, in the same datatype and with the same band names.
  terra::writeRaster(stacked, plain, gdal = "COMPRESS=LZW")
  ref <- terra::values(terra::rast(plain))
  back <- terra::rast(f)
  expect_identical(terra::values(back), ref)
  expect_equal(names(back), c("var1.pred", "var1.var"))
  expect_equal(terra::datatype(back), terra::datatype(terra::rast(plain)))
  expect_true(terra::crs(back) == terra::crs(stacked))

  info <- geotiff_info(f)
  for (b in 1:2) {
    md <- info$bands[[b]]$metadata[[1]]
    x <- ref[, b]; x <- x[!is.na(x)]
    # Real statistics, not terra's -9999 placeholders. GDAL reports the
    # population standard deviation (divisor n).
    expect_equal(as.numeric(md$STATISTICS_MEAN), mean(x), tolerance = 1e-9)
    expect_equal(as.numeric(md$STATISTICS_STDDEV), sqrt(mean((x - mean(x))^2)), tolerance = 1e-9)
    expect_equal(as.numeric(md$STATISTICS_MINIMUM), min(x), tolerance = 1e-9)
    expect_equal(as.numeric(md$STATISTICS_MAXIMUM), max(x), tolerance = 1e-9)
  }
  tags <- info$metadata[[1]]
  expect_equal(tags$MONOLITH_VARIABLE, "Soil pH")
  expect_equal(tags$MONOLITH_METHOD, "Ordinary Kriging")
  # A newline would end the tag; an empty value is left out, not written blank.
  expect_equal(tags$MONOLITH_PRODUCT, "Actual Map (line two)")
  expect_null(tags$MONOLITH_UNIT)
})

test_that("write_geotiff still writes the raster when the tagging pass fails", {
  # A GDAL build without the translate utility must cost the tags, never the file.
  r <- terra::unwrap(make_test_wrapped_raster())
  f <- tempfile(fileext = ".tif"); plain <- tempfile(fileext = ".tif")
  on.exit(unlink(c(f, plain)), add = TRUE)
  local_mocked_bindings(gdal_utils = function(...) stop("translate unavailable"), .package = "sf")

  expect_warning(write_geotiff(r, f, tags = c(MONOLITH_VARIABLE = "x")), "statistics and tags")
  terra::writeRaster(r, plain)
  expect_identical(terra::values(terra::rast(f)), terra::values(terra::rast(plain)))
})

test_that("geotiff_tag_options cleans values and drops empty tags", {
  expect_equal(geotiff_tag_options(NULL), character(0))
  expect_equal(geotiff_tag_options(c(A = "x\r\ny", B = "", C = NA, D = "  z ")),
               c("-mo", "A=x y", "-mo", "D=z"))
})

test_that("write_vector_export writes GeoPackage and GeoJSON layers", {
  pts <- make_test_resid_points()

  gpkg <- tempfile(fileext = ".gpkg")
  on.exit(unlink(gpkg), add = TRUE)
  write_vector_export(pts, gpkg, "gpkg", "class_zones")
  back <- sf::st_read(gpkg, quiet = TRUE)
  expect_equal(nrow(back), nrow(pts))

  gj <- tempfile(fileext = ".geojson")
  on.exit(unlink(gj), add = TRUE)
  write_vector_export(pts, gj, "geojson", "class_zones")
  expect_equal(nrow(sf::st_read(gj, quiet = TRUE)), nrow(pts))
})

test_that("write_vector_export reprojects to WGS84 for the WGS84-only formats", {
  # KML and GeoJSON are WGS84 by specification; Shapefile/GPKG keep the
  # analysis CRS so a GIS re-measures the same areas the app reports.
  pts <- make_test_resid_points()   # EPSG:32633
  expect_true(sf::st_crs(pts) == sf::st_crs(32633))
  gj <- tempfile(fileext = ".geojson")
  on.exit(unlink(gj), add = TRUE)

  write_vector_export(pts, gj, "geojson", "zones")
  expect_true(sf::st_crs(sf::st_read(gj, quiet = TRUE)) == sf::st_crs(4326))
})

test_that("write_vector_export rejects an empty layer and an unknown format", {
  pts <- make_test_resid_points()
  expect_error(write_vector_export(NULL, tempfile(), "gpkg"), "Nothing to export")
  expect_error(write_vector_export(pts, tempfile(), "dxf"), "Unsupported")
})

# A class-zone layer is what both remaining formats are really asked to carry:
# the attributes ARE the payload (label, break limits, hectares, provenance),
# so a format that silently drops them is a broken export, not a lossy one.
make_test_zone_layer <- function() {
  sq <- function(x0, y0) sf::st_polygon(list(cbind(
    c(x0, x0 + 100, x0 + 100, x0, x0),
    c(y0, y0, y0 + 100, y0 + 100, y0))))
  sf::st_sf(
    class     = c("Med", "High"),
    class_min = c(0.05, 0.10),
    class_max = c(0.10, NA_real_),   # open outer break
    area_ha   = c(20481.34, 748.82),
    surface   = c("Actual", "Actual"),
    variable  = c("Total N (%)", "Total N (%)"),
    method    = c("OK", "OK"),
    geometry  = sf::st_sfc(sq(450000, 5800000), sq(450200, 5800000), crs = 32633)
  )
}

test_that("write_vector_export round-trips a shapefile through its zip", {
  z <- make_test_zone_layer()
  f <- tempfile(fileext = ".zip")
  on.exit(unlink(f), add = TRUE)

  write_vector_export(z, f, "shp", "class_zones")
  expect_true(file.exists(f))

  d <- tempfile(); dir.create(d)
  on.exit(unlink(d, recursive = TRUE), add = TRUE)
  zip::unzip(f, exdir = d)
  # .prj must be in the zip or the layer arrives unreferenced
  expect_true(any(grepl("\\.prj$", list.files(d))))

  back <- sf::st_read(list.files(d, "\\.shp$", full.names = TRUE), quiet = TRUE)
  expect_equal(nrow(back), 2)
  expect_true(sf::st_crs(back) == sf::st_crs(32633))
  expect_equal(back$class, c("Med", "High"))
  expect_equal(back$area_ha, c(20481.34, 748.82))
  expect_true(is.na(back$class_max[2]))
})

test_that("KML keeps every attribute, in the two fields the driver can carry", {
  # GDAL's KML driver writes only <name>/<description>, and LIBKML (which
  # supports ExtendedData) is absent from sf's Windows GDAL. Without the
  # fold-in, this layer arrives as unlabelled polygons.
  z <- make_test_zone_layer()
  f <- tempfile(fileext = ".kml")
  on.exit(unlink(f), add = TRUE)

  suppressWarnings(write_vector_export(z, f, "kml", "class_zones"))
  back <- sf::st_read(f, quiet = TRUE)

  expect_equal(nrow(back), 2)
  expect_true(sf::st_crs(back) == sf::st_crs(4326))   # KML is WGS84 by spec
  expect_equal(back$Name, c("Med", "High"))           # class label, not a row number

  # The payload is asserted on the file itself, not on the read-back frame:
  # which driver GDAL picks to READ a .kml varies by build (plain KML here,
  # LIBKML on the Linux CI image) and the two name the field differently
  # ("Description" vs "description"), so a column lookup either breaks or
  # passes vacuously on a zero-length vector. <description> is KML per spec.
  xml <- paste(readLines(f, warn = FALSE), collapse = "\n")
  desc <- regmatches(xml, gregexpr("(?s)<description>.*?</description>", xml, perl = TRUE))[[1]]
  desc <- gsub("</?description>", "", desc)
  expect_length(desc, 2)

  for (field in c("class_min", "class_max", "area_ha", "surface", "variable", "method")) {
    expect_true(all(grepl(field, desc, fixed = TRUE)))
  }
  expect_true(grepl("area_ha: 20481.34", desc[1], fixed = TRUE))
  # the open outer break stays empty rather than printing a bogus limit
  expect_true(grepl("class_max: ;|class_max: $", desc[2]))
  expect_false(any(grepl("NA", desc, fixed = TRUE)))

  # ...and a reader really does surface it, under whichever name its driver uses.
  desc_col <- grep("^description$", names(back), ignore.case = TRUE, value = TRUE)
  expect_gt(length(desc_col), 0)
  expect_true(all(nzchar(as.character(back[[desc_col[1]]]))))
})

test_that("kml_attribute_fields falls back sanely without a class column", {
  # Drawn polygons carry whatever leaflet attached, and sometimes nothing.
  pts <- make_test_resid_points()
  out <- kml_attribute_fields(pts, name_field = "class")
  expect_equal(names(out), c("Name", "Description", attr(pts, "sf_column")))
  expect_equal(out$Name, trimws(format(pts$resid, trim = TRUE, digits = 15, scientific = FALSE)))

  bare <- sf::st_sf(geometry = sf::st_geometry(pts))
  out2 <- kml_attribute_fields(bare)
  expect_equal(out2$Name, as.character(seq_len(nrow(bare))))
  expect_true(all(out2$Description == ""))
})
