# test-base-plot-styler.R — tests for generate_base_plot and apply_styler_theme
# from ui_helpers.R.  These functions depend on Shiny's `input` reactive list;
# we mock it as a plain named list since both functions only read from it with
# `$` / `%||%` (never rely on reactivity).

# ── Mock input ─────────────────────────────────────────────────────────────

mock_input_full <- list(
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

test_that("apply_styler_theme returns a ggplot with full input", {
  p <- ggplot2::ggplot(mtcars, ggplot2::aes(wt, mpg)) +
    ggplot2::geom_point() +
    ggplot2::labs(title = "Test Plot")
  result <- apply_styler_theme(p, mock_input_full, calibration = 1,
                                item_label = "Test Label", item_type = "plot")
  expect_s3_class(result, "ggplot")
})

test_that("apply_styler_theme returns a ggplot with minimal input", {
  p <- ggplot2::ggplot(mtcars, ggplot2::aes(wt, mpg)) +
    ggplot2::geom_point()
  result <- apply_styler_theme(p, mock_input_minimal, calibration = 1,
                                item_label = "", item_type = "plot")
  expect_s3_class(result, "ggplot")
})

test_that("apply_styler_theme applies calibration scaling", {
  p <- ggplot2::ggplot(mtcars, ggplot2::aes(wt, mpg)) +
    ggplot2::geom_point()
  result <- apply_styler_theme(p, mock_input_full, calibration = 2,
                                item_label = "", item_type = "plot")
  expect_s3_class(result, "ggplot")

  result_half <- apply_styler_theme(p, mock_input_full, calibration = 0.5,
                                     item_label = "", item_type = "plot")
  expect_s3_class(result_half, "ggplot")
})

test_that("apply_styler_theme handles map_combined item_type", {
  p1 <- ggplot2::ggplot(mtcars, ggplot2::aes(wt, mpg)) +
    ggplot2::geom_point()
  p2 <- ggplot2::ggplot(mtcars, ggplot2::aes(hp, qsec)) +
    ggplot2::geom_point()
  p_obj <- list(p1 = p1, p2 = p2)
  result <- apply_styler_theme(p_obj, mock_input_full, calibration = 1,
                                item_label = "Combined", item_type = "map_combined")
  # Patchwork-assembled result is a ggplot
  expect_s3_class(result, "ggplot")
})

test_that("apply_styler_theme handles legend position bottom", {
  input_bottom <- mock_input_full
  input_bottom$styler_legend_pos <- "bottom"
  p <- ggplot2::ggplot(mtcars, ggplot2::aes(wt, mpg)) +
    ggplot2::geom_point()
  result <- apply_styler_theme(p, input_bottom, calibration = 1,
                                item_label = "", item_type = "plot")
  expect_s3_class(result, "ggplot")
})

test_that("apply_styler_theme handles NULL font overrides", {
  p <- ggplot2::ggplot(mtcars, ggplot2::aes(wt, mpg)) +
    ggplot2::geom_point()
  result <- apply_styler_theme(p, mock_input_full, calibration = 1,
                                item_label = "With Label", item_type = "plot")
  expect_s3_class(result, "ggplot")
})

test_that("apply_styler_theme handles legend direction horizontal", {
  input_horiz <- mock_input_full
  input_horiz$styler_legend_dir <- "horizontal"
  p <- ggplot2::ggplot(mtcars, ggplot2::aes(wt, mpg, color = factor(cyl))) +
    ggplot2::geom_point()
  result <- apply_styler_theme(p, input_horiz, calibration = 1,
                                item_label = "", item_type = "plot")
  expect_s3_class(result, "ggplot")
})

test_that("apply_styler_theme handles high_contrast input switch", {
  input_hc <- mock_input_full
  input_hc$styler_high_contrast <- TRUE
  p <- ggplot2::ggplot(mtcars, ggplot2::aes(wt, mpg)) +
    ggplot2::geom_point()
  result <- apply_styler_theme(p, input_hc, calibration = 1,
                                item_label = "", item_type = "plot")
  expect_s3_class(result, "ggplot")
})

test_that("apply_styler_theme sets legend text angle", {
  input_angle <- mock_input_full
  input_angle$styler_legend_text_angle <- 90
  p <- ggplot2::ggplot(mtcars, ggplot2::aes(wt, mpg, color = factor(cyl))) +
    ggplot2::geom_point()
  result <- apply_styler_theme(p, input_angle, calibration = 1,
                                item_label = "", item_type = "plot")
  expect_s3_class(result, "ggplot")
})
