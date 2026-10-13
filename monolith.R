
source("global.R")
source("global_utils.R")   # static config + pure functions (no reactivity)

source("ui_main.R")        # assembles `ui` from ui_sidebar.R + ui_main_tabs.R

# The server is split into chunks sourced with local = TRUE into ONE shared
# environment, in this fixed order: later chunks read names defined by earlier
# ones, so never reorder them or wrap one in moduleServer().
server <- function(input, output, session) {

  # A. Session dirs, caches, diagnostics closures, module wiring, session_state, rv.
  source("server_setup.R", local = TRUE)

  # B. Export registry, run-config/history panels, styler, config + batch export.
  source("server_export.R", local = TRUE)

  # C. Draw handlers, locality assignment, regional params, popups, status chips.
  source("server_map_interactions.R", local = TRUE)

  # D. Data/shapefile/metadata upload, CRS guards, variable mapping, minimap.
  source("server_data_setup.R", local = TRUE)

  # E. get_current_meta()/get_display_meta(), docs drawer, session config, selectors.
  source("server_run_config.R", local = TRUE)

  # F. Per-locality IDW/TPS values, manual variogram tuning, variogram auto-fit.
  source("server_model_tuning.R", local = TRUE)

  # G. Run estimates, archive/VIF gates and the future_promise run (keep the
  #    run_params decoupling and the nested PSOCK topology intact).
  source("server_execution.R", local = TRUE)

  # H. draw_map, proxy overlays, view switcher, main/comparison maps.
  source("server_map_viewer.R", local = TRUE)

  # I. Diagnostics plots, stats/metrics/kappa tables, log + polygon export.
  source("server_sci_analysis.R", local = TRUE)
}

shinyApp(ui = ui, server = server)
