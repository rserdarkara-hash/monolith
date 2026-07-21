
source("global.R")
source("global_utils.R")   # static config + pure functions (no reactivity)

source("ui_main.R")        # assembles `ui` from ui_sidebar.R + ui_main_tabs.R

# The server body is sliced into sequentially sourced chunks that share ONE
# evaluation environment via source(local = TRUE): `rv`, session_state and all
# helper closures remain visible across every chunk, exactly as in the former
# inline body. The files are sourced in the ORIGINAL physical order of the
# monolithic server function - do not reorder them, and do not wrap any chunk
# in moduleServer(): later chunks rely on names defined in earlier ones.
server <- function(input, output, session) {

  # A. Core setup: session dirs, raster caches, diagnostics closures,
  #    decoupled module wiring (desc/classif), session_state, map_overlay_rev
  #    and the central `rv` reactiveValues object.
  source("server_setup.R", local = TRUE)

  # B. Export manager: export registry, run-config/run-history panels,
  #    WYSIWYG styler, config download/upload, confirm/batch export handlers.
  source("server_export.R", local = TRUE)

  # C. Map interactions: draw handlers, locality assignment, regional params,
  #    popup system, point styling and header status chips.
  source("server_map_interactions.R", local = TRUE)

  # D. Data setup: file/shp/metadata upload, CRS parsing + plausibility
  #    guards, variable mapping and the Setup-tab minimap.
  source("server_data_setup.R", local = TRUE)

  # E. Run configuration + display context: get_current_meta/get_display_meta,
  #    docs drawer, classification params, config persistence, palette and
  #    locality/covariate selectors.
  source("server_run_config.R", local = TRUE)

  # F. Geostatistical tuning: TPS lambda / IDW power optimization, variogram
  #    manual tuning and the expert auto-fit loop.
  source("server_model_tuning.R", local = TRUE)

  # G. Execution engine: run estimates, archive/VIF gates and the
  #    future_promise interpolation pipeline (run_params decoupling and the
  #    nested makeClusterPSOCK topology live here - keep them intact).
  source("server_execution.R", local = TRUE)

  # H. Map viewer: draw_map, proxy-managed overlays, view switcher and the
  #    main/comparison renderLeaflet blocks.
  source("server_map_viewer.R", local = TRUE)

  # I. Scientific analysis: model diagnostics, variogram/importance/obs-pred
  #    plots, stats/area/metrics/kappa tables, log + polygon export.
  source("server_sci_analysis.R", local = TRUE)
}

shinyApp(ui = ui, server = server)
