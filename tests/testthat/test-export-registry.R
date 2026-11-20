# test-export-registry.R - the export registry's keys, driven through the real
# server_export.R chunk (testServer, with stubs for the names it reads from the
# chunks sourced before it).

test_that("registry keys never merge two items, and Quick Export selects its own", {
  local_mocked_bindings(shinyApp = .real_shinyApp, .package = "shiny")
  withr::local_dir(proj_root)
  r <- terra::rast(nrows = 2, ncols = 2, xmin = 0, xmax = 2, ymin = 0, ymax = 2,
                   vals = 1:4, crs = "EPSG:32635")
  names(r) <- "var1.pred"
  shiny::testServer(function(input, output, session) {
    rv <- shiny::reactiveValues(
      export_registry = list(), run_config_summary = NULL, run_history = NULL,
      disp = list(method = "OK", localities = "L"), model_running = FALSE, log = "",
      rast_list_act = list(L = terra::wrap(r)), rast_list_pre = list(), rast_list_res = list())
    # Server-setup and map-viewer names this chunk reads.
    get_display_meta <- function() {
      list(actual = "Total N (%)", label = "Total N", unit = "%", category = "Soil",
           method = "OK", variance_surfaces = character(0))
    }
    map_view_base <- function() "view_act"
    map_view_layer <- function() "value"
    source(file.path(proj_root, "server_export.R"), local = TRUE)
  }, {
    # Three ids that sanitise alike keep three entries.
    ids <- paste0("table_stats_loc_", c("Field-1", "Field 1", "Field_1"))
    keys <- vapply(ids, function(id) {
      register_export_item(id, id, "table", data.frame(a = 1))
    }, character(1), USE.NAMES = FALSE)
    expect_identical(keys, safe_key(ids))
    expect_length(unique(keys), 3L)
    expect_setequal(names(rv$export_registry), keys)
    expect_identical(unname(vapply(rv$export_registry[keys], `[[`, character(1), "label")), ids)

    # Quick Export of a column whose name is not made of [A-Za-z0-9_]: the item
    # it selects is the one it registered, so the preview, the GeoTIFF option
    # and the download all find it.
    session$setInputs(quick_export_map = 1)
    sel <- active_styler_item()
    expect_identical(sel, safe_key("quick_view_act_Total N (%)"))
    expect_true(sel %in% names(rv$export_registry))
    item <- rv$export_registry[[sel]]
    expect_false(is.null(item))
    expect_identical(export_raster_count(item), 1L)
    expect_identical(export_ext_for(item, "gtiff"), "tif")
  })
})

test_that("a GeoTIFF export is one file on one lattice and one file per locality otherwise", {
  local_mocked_bindings(shinyApp = .real_shinyApp, .package = "shiny")
  withr::local_dir(proj_root)
  grid <- function(xmin, n, res, base) {
    r <- terra::rast(nrows = n, ncols = n, xmin = xmin, xmax = xmin + n * res, ymin = 4400000,
                     ymax = 4400000 + n * res, crs = "EPSG:32635", nlyrs = 2)
    names(r) <- c("var1.pred", "var1.var")
    terra::values(r) <- cbind(base + seq_len(n * n) / 100, 0.5 + seq_len(n * n) / 1000)
    r
  }
  # Auto (Per Locality): 90 m and 15 m cells. A third locality on Kale's lattice.
  sparse <- grid(500040, 4, 90, 5); dense <- grid(510000, 12, 15, 7); twin <- grid(520005, 6, 15, 9)
  tif_info <- function(f) {
    jsonlite::fromJSON(sf::gdal_utils("info", f, options = "-json", quiet = TRUE), simplifyVector = FALSE)
  }
  shiny::testServer(function(input, output, session) {
    rv <- shiny::reactiveValues(
      export_registry = list(), run_config_summary = list(run_id = 4L), run_history = NULL,
      disp = list(method = "OK", label = "Soil pH", unit = "", crs_sel = "EPSG:32635",
                  localities = c("Acipayam", "Kale"), mergeable = FALSE),
      model_running = FALSE, log = "",
      rast_list_act = list(Acipayam = terra::wrap(sparse), Kale = terra::wrap(dense)),
      rast_list_pre = list(), rast_list_res = list())
    get_display_meta <- function() {
      list(actual = "ph", label = "Soil pH", unit = "", category = "Soil", method = "OK",
           variance_surfaces = "act")
    }
    view <- new.env()
    view$layer <- "value"
    map_view_base <- function() "view_act"
    map_view_layer <- function() view$layer
    source(file.path(proj_root, "server_export.R"), local = TRUE)
  }, {
    key <- register_export_item("map_actual", "Soil pH - Actual Map - Ordinary Kriging", "map",
                                rv$rast_list_act, surface = "act")
    item <- rv$export_registry[[key]]
    expect_false(gtiff_single(item))
    expect_identical(export_ext_for(item, "gtiff"), "zip")
    expect_identical(export_ext_for(item, "png"), "png")

    dir <- withr::local_tempdir()
    files <- write_item_geotiffs(item, dir, "Export_map_actual_OK")
    expect_identical(files, c("Export_map_actual_OK_Acipayam.tif", "Export_map_actual_OK_Kale.tif"))
    own <- list(Acipayam = sparse, Kale = dense)
    for (l in names(own)) {
      f <- file.path(dir, files[match(l, names(own))])
      back <- terra::rast(f)
      # Each locality at its own cell size, extent and values: nothing resampled.
      expect_equal(terra::res(back), terra::res(own[[l]]))
      expect_equal(as.vector(terra::ext(back)), as.vector(terra::ext(own[[l]])))
      expect_equal(terra::values(back), terra::values(own[[l]]), tolerance = 1e-6, ignore_attr = TRUE)
      expect_identical(names(back), c("var1.pred", "var1.var"))
      tags <- tif_info(f)$metadata[[1]]
      expect_identical(tags$MONOLITH_LOCALITY, l)
      expect_identical(tags$MONOLITH_PRODUCT, "Actual Map - Ordinary Kriging")
      expect_identical(tags$MONOLITH_RUN_ID, "4")
    }

    # The SE view, quick-exported: single-band files of the same grids.
    view$layer <- "se"
    session$setInputs(quick_export_map = 1)
    se_item <- rv$export_registry[[active_styler_item()]]
    expect_identical(se_item$kind, "uncertainty")
    expect_identical(export_ext_for(se_item, "gtiff"), "zip")
    se_files <- write_item_geotiffs(se_item, dir, "se")
    se_back <- terra::rast(file.path(dir, se_files[2]))
    expect_identical(names(se_back), "var1.se")
    expect_equal(terra::values(se_back)[, 1], sqrt(terra::values(dense[["var1.var"]])[, 1]), tolerance = 1e-6)
    view$layer <- "value"

    # One locality of that run on its own is one file.
    solo <- item
    solo$obj <- rv$rast_list_act["Kale"]
    expect_true(gtiff_single(solo))
    expect_identical(export_ext_for(solo, "gtiff"), "tif")
    expect_identical(write_item_geotiffs(solo, dir, "solo"), "solo.tif")
    expect_identical(tif_info(file.path(dir, "solo.tif"))$metadata[[1]]$MONOLITH_LOCALITY, "Kale")

    # A run on one lattice: one file holding every locality, cell for cell.
    rv$rast_list_act <- list(Kale = terra::wrap(dense), Tavas = terra::wrap(twin))
    rv$disp$mergeable <- TRUE
    shared <- item
    shared$obj <- rv$rast_list_act
    expect_identical(export_ext_for(shared, "gtiff"), "tif")
    one <- write_item_geotiffs(shared, dir, "shared")
    expect_identical(one, "shared.tif")
    m <- terra::rast(file.path(dir, one))
    expect_equal(terra::res(m), c(15, 15))
    for (r in list(dense, twin)) {
      expect_equal(terra::extract(m, terra::crds(r))[["var1.pred"]], terra::values(r)[, 1], tolerance = 1e-6)
    }
    expect_identical(tif_info(file.path(dir, one))$metadata[[1]]$MONOLITH_LOCALITY, "Kale, Tavas")

    # Packed one-row grids far apart need the resolution recorded by the run.
    res <- 7.2632570371654
    tile <- function(iy, pred, variance) {
      r <- terra::rast(nrows = 1, ncols = 2, nlyrs = 2,
                       xmin = 62000 * res, xmax = 62002 * res,
                       ymin = iy * res, ymax = (iy + 1) * res, crs = "EPSG:32633")
      names(r) <- c("var1.pred", "var1.var")
      terra::values(r) <- cbind(pred, variance)
      r
    }
    pair <- list(Kale = tile(790000, c(1.2345, 2), c(NA_real_, NA_real_)),
                 Tavas = tile(950000, c(3, 4.5678), c(0.124, 0.4)))
    rv$disp$grid_res_native <- c(Kale = res, Tavas = res)
    fractional <- shared
    fractional$obj <- lapply(pair, terra::wrap)
    expect_identical(export_ext_for(fractional, "gtiff"), "tif")
    packed_files <- write_item_geotiffs(fractional, dir, "fractional")
    expect_identical(packed_files, "fractional.tif")
    back <- terra::rast(file.path(dir, packed_files))
    expect_true(terra::same.crs(back, pair[[1]]))
    expect_equal(dim(back), c(160001, 2, 2))
    expect_identical(names(back), c("var1.pred", "var1.var"))
    for (r in pair) {
      centers <- terra::xyFromCell(r, seq_len(terra::ncell(r)))
      cells <- terra::cellFromXY(back, centers)
      expect_lt(max(abs(terra::xyFromCell(back, cells) - centers)), 1e-6 * res)
      expect_equal(as.matrix(terra::extract(back, centers)), terra::values(r),
                   tolerance = 1e-6, ignore_attr = TRUE)
    }
    expect_true(all(is.na(terra::values(back, row = 2, nrows = 1))))

    # The missing-cell mask can differ by product, even on the same run grid.
    # Each tile matches the longer anchor, but the two shorter tiles' raw
    # edges differ by more than the alignment tolerance.
    tile_at <- function(x, columns, value) {
      terra::rast(nrows = 1, ncols = columns, xmin = x, xmax = x + columns,
                  ymin = 0, ymax = 1, crs = "EPSG:32633", vals = value)
    }
    rv$disp$grid_res_native <- c(Anchor = 1, West = 1, East = 1)
    alternate <- fractional
    alternate$obj <- list(Anchor = tile_at(0, 4, 1),
                          West = tile_at(10 - 0.75e-6, 2, 2),
                          East = tile_at(11 + 0.75e-6, 2, 3))
    expect_true(rasters_share_lattice(alternate$obj, rv$disp$grid_res_native))
    expect_true(rasters_have_shared_cells(alternate$obj, rv$disp$grid_res_native))
    expect_identical(export_ext_for(alternate, "gtiff"), "zip")
    expect_identical(write_item_geotiffs(alternate, dir, "alternate"),
                     c("alternate_Anchor.tif", "alternate_West.tif", "alternate_East.tif"))

    alternate$obj$East <- tile_at(11.5, 2, 3)
    expect_identical(export_ext_for(alternate, "gtiff"), "zip")

    # Distinct names must survive a case-insensitive filesystem, including a
    # literal name that is another locality's sanitised key.
    rv$disp$mergeable <- FALSE
    collision <- item
    locs <- c("Field", "field", "Field_2", "A/B", safe_key("A/B"))
    collision$obj <- stats::setNames(lapply(seq_along(locs), function(i) {
      r <- sparse[[1]]; terra::values(r) <- i; terra::wrap(r)
    }), locs)
    paths <- write_item_geotiffs(collision, dir, "collision")
    expect_equal(length(unique(tolower(paths))), length(locs))
    for (i in seq_along(locs)) {
      f <- file.path(dir, paths[i])
      expect_equal(unique(as.vector(terra::values(terra::rast(f)))), i)
      expect_identical(tif_info(f)$metadata[[1]]$MONOLITH_LOCALITY, locs[i])
    }

    # A table has no raster form whatever the format asked for.
    tab_key <- register_export_item("t", "t", "table", data.frame(a = 1))
    expect_identical(export_ext_for(rv$export_registry[[tab_key]], "gtiff"), "xlsx")
  })
})
