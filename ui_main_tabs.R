# ui_main_tabs.R - main tab panel as a plain variable assignment (no function
# wrapper), consumed by ui_main.R inside sidebarLayout().
ui_main_tabs <- mainPanel(width = 9,
      tabsetPanel(id = "main_tabs",
        tabPanel("Data Setup", value = "tab_data",
                 div(style = "padding: 8px 2px;",
                     div(class = "setup-card",
                         div(class = "setup-card-header",
                             span(class = "setup-step-badge", "1"),
                             span(class = "setup-card-title", "Upload Your Dataset")
                         ),
                         p(class = "setup-card-sub", "Load the georeferenced sampling data to analyze."),
                         div(class = "setup-primary-upload",
                             div(class = "setup-upload-field",
                                 fileInput("user_file", "Choose CSV or Excel File", accept = c(".csv", ".xlsx", ".xls"))
                             ),
                             # The variable list sits beside the dataset it describes rather
                             # than three cards below it: both files are chosen in one pass,
                             # and the pairs it produces are still edited in step 4.
                             div(class = "setup-upload-field",
                                 fileInput("meta_file",
                                           HTML(paste0("Variable List (Optional)", info_tooltip("meta_file_info", "A second table giving your columns readable labels, units, categories and their Actual/Predicted pairs. Headers containing 'label' or 'name' supply the labels shown on every map and report; headers containing 'cat' or 'group' file the variables into folders. Without one the pairs are auto-detected from the column names. Either way they are listed for review and editing under Variable Mapping & Verification, at the foot of this tab."))),
                                           accept = c(".xlsx", ".xls", ".csv"))
                             )
                         ),
                         div(class = "setup-optional",
                             div(class = "setup-optional-text",
                                 div(class = "setup-optional-title", "Boundary shapefile"),
                                 p(class = "setup-optional-sub",
                                   "Optional — the built-in hulls (convex, concave, strict, wrapped) cover most cases and are configured in the sidebar once your dataset is loaded. To use your own, select the .shp, .shx, .dbf and .prj files together.")
                             ),
                             fileInput("user_shp", "Boundary shapefile", multiple = TRUE,
                                       accept = c(".shp", ".shx", ".dbf", ".prj"),
                                       buttonLabel = "Upload .shp set")
                         )
                     ),
                     conditionalPanel(condition = "output.file_uploaded",
                        div(class = "setup-card",
                            div(class = "setup-card-header",
                                span(class = "setup-step-badge", "2"),
                                span(class = "setup-card-title", "Spatial Mapping")
                            ),
                            p(class = "setup-card-sub", "Select the columns holding the coordinates and set how they are projected."),
                            div(class = "setup-grid",
                                selectInput("map_x", "X Coordinate (Longitude/Easting)", choices = NULL),
                                selectInput("map_y", "Y Coordinate (Latitude/Northing)", choices = NULL),
                                selectInput("map_loc", "Locality/Grouping Column", choices = NULL)
                            ),
                            div(class = "setup-grid",
                                selectizeInput("map_crs", "Input Data CRS", choices = common_crs_input, selected = "",
                                               options = list(create = TRUE,
                                                              placeholder = "Select the CRS your coordinates were recorded in",
                                                              onInitialize = I('function() { this.setValue(""); }'))),
                                selectizeInput("crs_selection", "Target Mapping CRS", choices = common_crs_target, selected = "",
                                               options = list(create = TRUE,
                                                              placeholder = "Select the CRS for output maps and exports",
                                                              onInitialize = I('function() { this.setValue(""); }'))),
                                p(class = "setup-hint", style = "align-self: center;",
                                  tags$b("Instructions:"), "Please wait for the sampling coordinates to render and verify their accuracy on the mini-map below.")
                            ),
                            uiOutput("crs_target_note"),
                            uiOutput("crs_picker_ui")
                        ),
                        div(class = "setup-card",
                            div(class = "setup-card-header",
                                span(class = "setup-step-badge", "3"),
                                span(class = "setup-card-title", "Mini-Map Validation")
                            ),
                            p(class = "setup-card-sub", "Verify that the sampling points land where you expect them before fitting any model."),
                            div(style = "border-radius: 8px; overflow: hidden;", leafletOutput("setup_minimap", height = "400px")),
                            uiOutput("crs_landing_note")
                        ),
                        div(class = "setup-card",
                            div(class = "setup-card-header",
                                span(class = "setup-step-badge", "4"),
                                span(class = "setup-card-title", "Variable Mapping & Verification")
                            ),
                            p(class = "setup-card-sub", "Pair your Target (Actual) variables with their Predictions."),
                            p(class = "setup-hint", "Filled in from the variable list uploaded in step 1 when one was supplied, and auto-detected otherwise. If you modify the pairs, please click 'Confirm Variable Mapping' at the bottom."),
                            # withSpinner() bakes the colour into generated CSS and takes a
                            # literal, so this is the one place the accent cannot be a token.
                            shinycssloaders::withSpinner(uiOutput("var_mapping_ui"), type = 6, color = "#0F6E8C")
                        )
                     )
                 )
        ),
                tabPanel("Map Viewer", value = "tab_map",
                         div(style="position: relative;",
                             div(id="map_processing_overlay", class="map-processing-overlay",
                                 h3(id="map_processing_title", "Awaiting Spatial Interpolation"),
                                 p(id="map_progress_text", HTML("Please configure parameters in the left panel and click <b>'Run Interpolation'</b> to generate geostatistical maps and review diagnostic results.")),
                                 shinyjs::hidden(
                                   div(id="map_progress_bar_container", class="premium-progress-bar-container",
                                       div(id="map_progress_bar_inner", class="premium-progress-bar-inner")
                                   )
                                 ),
                                 # The phase strip mirrors the checkpoints the
                                 # engines write into their progress files. The
                                 # second label is method-dependent and is set
                                 # by the run observer.
                                 shinyjs::hidden(
                                   div(id="map_run_steps", class="mn-run-steps",
                                       span(id="map_step_1", class="mn-run-step", "Validate"),
                                       span(id="map_step_2", class="mn-run-step", "Fit variogram"),
                                       span(id="map_step_3", class="mn-run-step", "Predict grid"),
                                       span(id="map_step_4", class="mn-run-step", "Cross-validate")
                                   )
                                 ),
                                 shinyjs::hidden(
                                     actionButton("reveal_maps_btn", "Reveal Maps & Enable Analysis", class="btn-primary")
                                 ),
                                 shinyjs::hidden(
                                     actionButton("cancel_model_btn", "Cancel", class="btn-light btn-sm")
                                 )
                             ),
                         div(class = "mn-maptoolbar",
                             mn_popover("Overlays", icon_tag = icon("layer-group"), width = "270px",
                                 checkboxInput("show_points_viewer", HTML(paste0("Show Points", info_tooltip("show_points_info", "Rendering the sampling points on the map can take a while, up to ~30 seconds depending on the number of samples and the size of the dataset. The map stays responsive while the points are being drawn."))), FALSE, width = "auto"),
                                 checkboxInput("show_res_overlay", "Show Res", FALSE, width = "auto"),
                                 checkboxInput("show_north", "North Arrow", FALSE, width = "auto"),
                                 checkboxInput("show_borders", "Borders", FALSE, width = "auto"),
                                 checkboxInput("show_scale", "Map Scale", FALSE, width = "auto"),
                                 # Leaflet's own controls. Both are on by default - this is
                                 # the way out for anyone who wants the bare surface, e.g.
                                 # before a Quick export.
                                 checkboxInput("show_draw_tools", "Drawing Tools", TRUE, width = "auto"),
                                 checkboxInput("show_ruler", "Ruler", TRUE, width = "auto"),
                                 # Off by default: the same label is already printed as the
                                 # map heading, and a long one stretched the legend box out
                                 # across the surface.
                                 checkboxInput("show_legend_title", HTML(paste0("Variable Label in Legend", info_tooltip("legend_title_info", "Repeats the variable label (and its unit) inside the map legend. It is already shown as the map heading above, so it is off by default - turn it on for a legend that has to stand alone in an export."))), FALSE, width = "auto")
                             ),
                             div(class = "mn-tb-group",
                                 # The two CARTO layers sit last because they are the only
                                 # ones that need a key; the three above them work as-is.
                                 selectInput("base_map_layer", NULL,
                                             choices = c("Satellite (Esri)" = "Esri.WorldImagery",
                                                         "Topographic" = "OpenTopoMap",
                                                         "Standard Street" = "OpenStreetMap",
                                                         "Dark Matter (key)" = "CartoDB.DarkMatter",
                                                         "Light / Positron (key)" = "CartoDB.Positron"),
                                             selected = "Esri.WorldImagery", width = "185px", selectize = FALSE),
                                 conditionalPanel(
                                   condition = "input.base_map_layer == 'CartoDB.Positron' || input.base_map_layer == 'CartoDB.DarkMatter'",
                                   div(style = "display:flex; align-items:center; gap:6px;",
                                       textInput("carto_api_key", NULL, value = "", width = "175px",
                                                 placeholder = "CARTO API key"),
                                       info_tooltip("carto_key_info", paste0(
                                         "CARTO now requires a free API key for its Positron and Dark Matter basemaps. ",
                                         "Without one the tiles arrive stamped with an 'API key required' watermark. ",
                                         "Request a key at carto.com/basemaps/apikey and paste it here. ",
                                         "The key is held for this session only - it is sent to CARTO to fetch tiles and ",
                                         "is never written into a run configuration, an export, or the run history.")))
                                 )
                             ),
                                 uiOutput("locality_pan_ui"),
                                 div(title = "Switch between the surfaces computed by the last interpolation run. Rerun to change variable or method.",
                                     uiOutput("map_view_ui")),
                             div(class = "mn-tb-spacer"),
                             actionButton("refresh_map_area", "Refresh", icon = icon("sync"), class = "btn-default btn-sm"),
                             actionButton("show_popup_settings", "Pop-up", icon = icon("cog"), class = "btn-default btn-sm"),
                             actionButton("quick_export_map", "Quick export", icon = icon("camera"), class = "btn-default btn-sm", title = "Immediately send the currently viewed map to the Export Registry."),
                             actionButton("toggle_pt_style", "Point styling", icon = icon("palette"), class = "btn-default btn-sm",
                                          title = "Colour, label and size the sampling points drawn on the map"),
                             # Named apart from "Quick export" on purpose: that
                             # button sends the map to the Export Registry, this
                             # one writes GIS/data files straight to disk.
                             mn_popover("Downloads", icon_tag = icon("download"), align = "right", width = "300px",
                                 selectInput("polygon_export_format", "Vector format", choices = c("Shapefile (ZIP)" = "shp", "GeoJSON" = "geojson", "KML" = "kml", "GPKG" = "gpkg"), selected = "shp", width = "100%", selectize = FALSE),
                                 # Wrapper spans exist to carry the tooltip while the button inside is
                                 # disabled: a disabled .btn anchor has pointer-events: none, so its own
                                 # title never fires and the wrapper is what the pointer actually sees.
                                 tags$span(id = "polygon_dl_wrap", style = "display: inline-flex;",
                                   downloadButton("polygon_download_btn", "Drawn polygons", class = "btn-default btn-sm btn-block",
                                                  title = "Downloads all polygons drawn on the map, in the format selected above.")),
                                 tags$span(id = "class_zone_dl_wrap", style = "display: inline-flex;",
                                   downloadButton("class_zone_download_btn", "Class zones", class = "btn-default btn-sm btn-block",
                                                  title = "Downloads the class zones of the surface currently displayed as a GIS vector layer, in the format selected above. One dissolved polygon per class, carrying its label, its break limits and its area in hectares.")),
                                 downloadButton("export_updated_data", "Updated dataset", class = "btn-default btn-sm btn-block",
                                                title = "Use after modifying your dataset in the app - e.g. after drawing a polygon on the map and saving it as a new group ('Assign Locality / Analysis Group'). Downloads the current dataset as .xlsx, including the 'Assigned_Locality' column.")
                             )
                         ),
                         shinyjs::hidden(
                           div(id = "pt_style_toolbar",
                             style = "margin-bottom:10px; padding: 12px 15px; background: var(--mn-surface-2); border-radius: var(--mn-radius-lg); border: 1px solid var(--mn-line); color: var(--mn-text); display: flex; flex-wrap: wrap; align-items: flex-start; gap: 18px;",
                             div(style = "min-width: 160px;",
                               tags$label("Color By", style = "font-size: 11px; color: var(--mn-text-3); margin-bottom: 2px; display: block; text-transform: uppercase; letter-spacing: 0.5px;"),
                               selectInput("pt_color_by", NULL, choices = c("None (Cyan)" = "none"), selected = "none", width = "160px", selectize = FALSE),
                               selectInput("pt_palette", "Palette", choices = c("Set1", "Dark2", "Paired", "Set2", "Set3", "Accent", "Pastel1", "Tableau10"), selected = "Set1", width = "160px", selectize = FALSE),
                               actionButton("pt_custom_colors", "Custom Colors...", icon = icon("paint-brush"), class = "btn-xs btn-default",
                                            style = "margin-top: 4px; background-color: var(--mn-surface); color: var(--mn-text); border-color: var(--mn-line-2);")
                             ),
                             div(style = "min-width: 160px;",
                               tags$label("Labels", style = "font-size: 11px; color: var(--mn-text-3); margin-bottom: 2px; display: block; text-transform: uppercase; letter-spacing: 0.5px;"),
                               checkboxInput("pt_show_labels", "Show Labels", FALSE, width = "auto"),
                               selectInput("pt_label_field", "Label Field", choices = c("(none)" = "none"), selected = "none", width = "160px", selectize = FALSE),
                               sliderInput("pt_label_size", "Label Size", min = 8, max = 18, value = 11, step = 1, width = "150px", ticks = FALSE)
                             ),
                             div(style = "min-width: 130px;",
                               tags$label("Point Options", style = "font-size: 11px; color: var(--mn-text-3); margin-bottom: 2px; display: block; text-transform: uppercase; letter-spacing: 0.5px;"),
                               sliderInput("pt_marker_size", "Point Size", min = 1, max = 12, value = 3, step = 1, width = "130px", ticks = FALSE),
                               checkboxInput("pt_apply_minimap", "Apply Colour Set to Mini Map", FALSE, width = "auto")
                             )
                           )
                         ),
                         uiOutput("map_crs_stale_note"),
                         uiOutput("run_config_display_map"),
                         conditionalPanel(condition = "!input.map_view || ['view_act', 'view_pred'].includes(input.map_view)",
                                          h4(textOutput("main_map_title")),
                                          leafletOutput("main_map", height = "700px")),
                         conditionalPanel(condition = "['view_comp', 'view_resid'].includes(input.map_view)",
                                          fluidRow(column(6, h4(textOutput("comp_left_title")), leafletOutput("comp_map_left", height = "600px")),
                                                   column(6, h4(textOutput("comp_right_title")), leafletOutput("comp_map_right", height = "600px")))),
                         # Scale controls are moved here directly by the
                         # show_scale overlay observer when it creates them -
                         # no DOM polling.
                         div(id = "distance_scale_container", style = "margin-top: 15px; display: flex; justify-content: center; min-height: 30px;")
                 )),
        tabPanel("Scientific Analysis", value = "tab_analysis",
                 conditionalPanel(
                   condition = "output.model_ready == 'no'",
                   div(style = "text-align: center; padding: 120px 50px; color: var(--mn-text-3);",
                       icon("microscope", class = "fa-4x", style = "margin-bottom: 20px; color: var(--mn-line-2);"),
                       h3("Awaiting Scientific Analysis", style = "font-weight: 300; margin-bottom: 10px;"),
                       p("Fit spatial interpolation models on the left pane and click 'Run Interpolation' to discover spatial structures and diagnostics.")
                   )
                 ),
                 conditionalPanel(
                   condition = "output.model_ready == 'yes'",
                   # Flex column, not plain block flow: while the sidebar is in
                   # variogram tuning the variogram cards have to sit directly
                   # under the header (the sliders are useless if their curve is
                   # three cards down the page) and the displayed run's own
                   # cards have to step aside when they came from an engine with
                   # no variogram. Both are one class on this container
                   # (server_map_interactions.R) because every output id below
                   # exists exactly once and cannot be rendered twice.
                   div(id = "sci_stack", class = "sci-stack",
                   div(class = "sci-stack-head",
                     fluidRow(
                       column(6, uiOutput("locality_selector_ui")),
                       column(6, shinyWidgets::radioGroupButtons("sci_name_mode", "Variable naming:",
                                              choices = c("Variable labels" = "label", "Column names" = "colname"),
                                              selected = "label", size = "sm"))
                     )
                   ),
                   conditionalPanel(
                     condition = "output.sci_stale_run == 'yes'",
                     class = "sci-stack-head sci-keep",
                     tags$p(class = "mn-note-warn",
                            tags$b("Variogram tuning. "),
                            "The panels below describe the variograms you are fitting. The last interpolation run used an engine with no variogram of its own, so its result cards are held back until you run the analysis again; the maps from that run are untouched.")
                   ),
                            sci_card("Spatial Interpolation Statistics",
                              "Model-specific diagnostics and performance metrics (RMSE, R²).",
                              conditionalPanel(condition = "output.disp_method == 'OK'",
                                h5("Variogram Parameters (per locality)"), div(class="table-container", DT::dataTableOutput("vgm_params_table")),
                                hr()
                              ),
                              conditionalPanel(condition = "['IDW', 'TPS'].includes(output.disp_method)",
                                h5("Regional Parameters (per locality)"), div(class="table-container", DT::dataTableOutput("regional_params_table")),
                                hr()
                              ),
                              h5("Model Performance"), uiOutput("cv_strategy_badge"), div(class="table-container", DT::dataTableOutput("metrics_table")),
                              # Only present when the run was launched with
                              # repeated CV switched on (Spatial Engine panel).
                              conditionalPanel(condition = "output.has_cv_repeats === true",
                                hr(),
                                h5(HTML(paste0("Fold-Realization Stability", info_tooltip("cv_repeats_info", "Repeated cross-validation: the same model re-scored under alternative fold assignments (the partition is the only thing that changes). Cells are mean ± SD across realizations. Treat the SD as the resolution of the comparison: two methods whose metrics differ by less than this are separated by fold luck, not skill. Leave-one-out folds are deterministic and never repeat. Moran's I is reported for realization 1 only, in the table above.")))),
                                div(class="table-container", DT::dataTableOutput("cv_repeats_table"))
                              )
                            ),
                            div(id = "prediction_performance_ui",
                              sci_card("Variable Prediction Statistics",
                                "Prediction accuracy and classification agreement metrics for uploaded data.",
                                h5("Prediction Performance (Uploaded Data)"),
                                div(class="table-container", DT::dataTableOutput("uploaded_metrics_table")),
                                hr(),
                                h5("Classification Performance (Uploaded Predictions)"),
                                selectInput("kappa_bin_method", "Binning Method:", choices = c("Agronomical Classes" = "agro", "Quartiles" = "quartile")),
                                div(class="table-container", DT::dataTableOutput("kappa_table"))
                              )
                            ),
                            sci_card("Data Summary Statistics",
                              "Aggregated descriptive statistics and area coverage for the data.",
                              conditionalPanel(condition = "!['agro', 'bin'].includes(input.color_style)",
                                tags$p(style="font-size: 0.85em; color: var(--mn-text-2); font-style: italic;",
                                       "Area coverage by class appears here when the map Styling is set to Agronomical or Binned.")
                              ),
                              # Keyed to the DISPLAYED run (disp_method is '' before the first
                              # run, so no bare titles pre-run): the Total tables describe the
                              # run's combined coverage - a single-locality run included - and
                              # the Locality rows appear when the analysis filter picks one.
                              conditionalPanel(condition = "['agro', 'bin'].includes(input.color_style) && output.disp_method && output.disp_method != ''",
                                h5("Area Coverage"),
                                fluidRow(
                                  column(6, h6("Total - Actual"), div(class="table-container", DT::dataTableOutput("area_table_total_act"))),
                                  column(6, div(id = "area_total_pred_col", h6("Total - Predicted"), div(class="table-container", DT::dataTableOutput("area_table_total_pre"))))
                                ),
                                conditionalPanel(condition = "input.sel_loc_stats && input.sel_loc_stats != 'Total (Combined)'",
                                  fluidRow(
                                    column(6, h6("Locality - Actual"), div(class="table-container", DT::dataTableOutput("area_table_loc_act"))),
                                    column(6, div(id = "loc_pred_col", h6("Locality - Predicted"), div(class="table-container", DT::dataTableOutput("area_table_loc_pre"))))
                                  )
                                ),
                                hr()
                              ),
                              conditionalPanel(condition = "output.disp_method && output.disp_method != ''",
                                h5("Descriptive Statistics"),
                                div(class="table-container", DT::dataTableOutput("stats_table_total")),
                                div(class="table-container", DT::dataTableOutput("stats_table_loc"))
                              )
                            ),
                            conditionalPanel(condition = "output.sci_diag_method == 'OK' || output.sci_vgm_tuning == 'yes'",
                              class = "sci-vgm-block",
                              sci_plot_card("vgm_plot_main", "Actual Data Structure"),
                              div(id = "predicted_data_structure_ui",
                                sci_plot_card("vgm_plot_pred", "Predicted Data Structure")
                              )
                            ),
                            conditionalPanel(condition = "output.sci_diag_method == 'RK'",
                               h4("Linear Trend Performance (Actual)"), uiOutput("model_summary_ui_act"),
                               div(id = "rk_pred_ui", h4("Linear Trend Performance (Predicted)"), uiOutput("model_summary_ui_pre")),
                               hr(),
                               sci_plot_card("rk_internal_vgm_act", "Internal Residual Variogram (Actual)"),
                               div(id = "rk_internal_vgm_pre_ui", sci_plot_card("rk_internal_vgm_pre", "Internal Residual Variogram (Predicted)"))
                             ),
                            conditionalPanel(condition = "output.sci_diag_method == 'RFK'",
                               sci_plot_card("rf_importance_plot_act", "RF Variable Importance (Actual)"),
                               div(id = "rfk_pred_ui", sci_plot_card("rf_importance_plot_pre", "RF Variable Importance (Predicted)")),
                               hr(),
                               sci_plot_card("rfk_internal_vgm_act", "Internal Residual Variogram (Actual)"),
                               div(id = "rfk_internal_vgm_pre_ui", sci_plot_card("rfk_internal_vgm_pre", "Internal Residual Variogram (Predicted)"))
                             ),
                            conditionalPanel(condition = "output.sci_diag_method == 'CK'",
                               sci_plot_card("ck_variogram_plot_act", "Cross-Variogram (Actual)"),
                               div(id = "ck_pred_ui", sci_plot_card("ck_variogram_plot_pred", "Cross-Variogram (Predicted)"))
                             ),
                            conditionalPanel(condition = "output.sci_diag_method == 'TPS'",
                               sci_plot_card("tps_gcv_plot_act", "TPS GCV Diagnostics (Actual)"),
                               div(id = "tps_pred_ui", sci_plot_card("tps_gcv_plot_pre", "TPS GCV Diagnostics (Predicted)"))
                             ),
                            conditionalPanel(condition = "!['OK', 'RK', 'RFK', 'CK', 'TPS'].includes(output.sci_diag_method) && output.sci_vgm_tuning != 'yes'",
                              div(style="padding: 20px; text-align: center; color: var(--mn-text-2);",
                                  h4("Diagnostic Mode Active"),
                                  p("Detailed spatial diagnostics are currently optimized for Kriging and TPS."))
                            ),
                            # Anisotropy is a property of the sampled field, not
                            # of the engine, so this card is shown for every
                            # method (including IDW/TPS, which have no variogram
                            # of their own).
                            div(id = "directional_vgm_ui", class = "sci-keep",
                               hr(),
                               radioButtons("dir_vgm_source", "Directional variogram computed on:",
                                            choices = c("Measured values" = "v",
                                                        "Model residuals (CV)" = "resid"),
                                            selected = "v", inline = TRUE),
                               sci_plot_card("directional_vgm_plot",
                                 tags$span("Directional Variogram (Anisotropy Check)",
                                   info_tooltip("dir_vgm", "Semivariance computed separately within four angular cones (bearings measured clockwise from north). If the four curves reach their sill at clearly different distances, the spatial structure is directional (anisotropic) and a single omnidirectional range under-describes it. Diagnostic only: every interpolation engine in this app is omnidirectional, so nothing on the map changes because of what you read here.")),
                                 height = "330px")
                            ),
                            div(id = "validation_diagnostics_act_ui",
                               hr(),
                               h4("Validation Diagnostics (Actual)"),
                               fluidRow(
                                 column(6, sci_plot_card("obs_pred_plot_act", "Observed vs Predicted", height = "300px")),
                                 column(6, sci_plot_card("resid_vgm_plot_act", "Residual Variogram", height = "300px"))
                               )
                            ),
                            conditionalPanel(condition = "output.disp_has_pred == 'yes'",
                              div(id = "validation_diagnostics_pre_ui",
                                hr(),
                                h4("Validation Diagnostics (Predicted)"),
                                fluidRow(
                                  column(6, sci_plot_card("obs_pred_plot_pre", "Observed vs Predicted", height = "300px")),
                                  column(6, sci_plot_card("resid_vgm_plot_pre", "Residual Variogram", height = "300px"))
                                )
                              )
                            )
                   )
                 ),
                 hr(),
                 uiOutput("run_config_display"),
                 verbatimTextOutput("log_output")),
        tabPanel("Export", value = "tab_export",
                 div(style = "padding: 20px;",
                     h2("Unified Session Export Registry"),
                     p("Manage all maps and tables generated during this session. Select an item to customize and export."),
                     hr(),
                     fluidRow(
                       column(12,
                              div(class = "mn-panel",
                                  h4("Session Assets"),
                                  div(style = "margin-bottom: 10px;",
                                      actionButton("select_all_assets", "Select All", class = "btn-xs"),
                                      actionButton("deselect_all_assets", "Deselect All", class = "btn-xs")
                                  ),
                                  uiOutput("export_registry_ui"),
                                  div(style = "display: flex; gap: 10px; margin-top: 15px; flex-wrap: wrap;",
                                      actionButton("open_styler", "Open Export Styler", class = "btn-primary", icon = icon("palette")),
                                      downloadButton("batch_export", "Batch Export Selected", class = "btn-success", title = "Download all checked items as a ZIP archive."),
                                      downloadButton("download_run_config", "Download Run Configuration (.json)", class = "btn-info", title = "Machine-readable record of the current run: every model setting, the per-locality tuning it used, and the app / R / package versions it ran under. Reproducibility and methods reporting."),
                                      actionButton("clear_registry", "Clear Session Registry", class = "btn-danger", icon = icon("trash"))
                                  )
                                  )
                                  )
                                  ),
                     hr(),
                     div(class = "mn-panel",
                         h4(icon("archive"), "Run History Archive"),
                         tags$p(style="font-size: 0.85em; opacity: 0.8; font-style: italic;", "Previous model runs are archived here. You can restore or permanently remove them."),
                         uiOutput("run_history_ui"),
                         uiOutput("reset_archive_choice_ui")
                     )
                                  )),
        # Every tab carries an explicit value=: without one the tab TITLE is the
        # input$main_tabs value, so every renaming (including the numbering)
        # would silently break the sidebar's conditionalPanels and the
        # server-side reveal handlers that key on the tab id.
        tabPanel("Exploratory", value = "tab_desc",
                 div(class = "mn-suite", desc_exploratory_ui("exploratory"))
        ),
        tabPanel("Classification", value = "tab_classif",
                 div(class = "mn-suite", classif_ui("classification"))
        )      )
    )
