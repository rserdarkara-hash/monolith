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
