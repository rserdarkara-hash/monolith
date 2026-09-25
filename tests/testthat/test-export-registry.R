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
      rast = terra::wrap(r), rast_pred = NULL, rast_res = NULL)
    # Server-setup and map-viewer names this chunk reads.
    get_display_meta <- function() {
      list(actual = "Total N (%)", label = "Total N", unit = "%", category = "Soil",
           method = "OK", has_variance = FALSE)
    }
    map_view_base <- function() "view_act"
    map_view_layer <- function() "value"
    source(file.path(proj_root, "server_export.R"), local = TRUE)
  }, {
    # Three ids that sanitise alike keep three entries.
    ids <- paste0("table_stats_loc_", c("Field-1", "Field 1", "Field_1"))
    keys <- vapply(ids, function(id) {
      register_export_item(id, id, "table", data.frame(a = 1), var_label = "Total N")
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
    expect_false(is.null(export_raster_payload(item)))
    expect_identical(export_ext_for(item, "gtiff"), "tif")
  })
})
