# ui_main_tabs.R - main tab panel as a plain variable assignment (no function
# wrapper), consumed by ui_main.R inside sidebarLayout().
ui_main_tabs <- mainPanel(width = 9,
      tabsetPanel(id = "main_tabs",
        tabPanel("1. Data Setup", value = "tab_data",
                 div(style = "padding: 8px 2px;",
                     div(class = "setup-card",
                         div(class = "setup-card-header",
                             span(class = "setup-step-badge", "1"),
                             span(class = "setup-card-title", "Upload Your Dataset")
                         ),
                         p(class = "setup-card-sub", "Load the georeferenced sampling data to analyze. An optional shapefile can define a custom interpolation boundary."),
                         div(class = "setup-grid",
                             fileInput("user_file", "Choose CSV or Excel File", accept = c(".csv", ".xlsx", ".xls")),
                             div(
                               fileInput("user_shp", "Shapefile - Optional (.shp, .shx, .dbf, .prj)", multiple = TRUE, accept = c(".shp", ".shx", ".dbf", ".prj")),
                               p(class = "setup-hint",
                                 tags$b("Tip:"), "You do not need to upload custom shapefiles! Standard boundary types (Convex, Concave, Strict, or Wrapped Hulls) can be selected and configured dynamically in the Sidebar panel once your dataset is loaded.")
                             )
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
                                selectizeInput("map_crs", "Input Data CRS", choices = common_crs, selected = "EPSG:32635", options = list(create = TRUE)),
                                selectizeInput("crs_selection", "Target Mapping CRS", choices = common_crs, selected = "EPSG:32635", options = list(create = TRUE)),
                                p(class = "setup-hint", style = "align-self: center;",
                                  tags$b("Instructions:"), "Please wait for the sampling coordinates to render and verify their accuracy on the mini-map below. Optionally, upload a variable list to enable automated data categorization.")
                            )
                        ),
                        div(class = "setup-card",
                            div(class = "setup-card-header",
                                span(class = "setup-step-badge", "3"),
                                span(class = "setup-card-title", "Mini-Map Validation")
                            ),
                            p(class = "setup-card-sub", "Verify that the sampling points land where you expect them before fitting any model."),
                            div(style = "border-radius: 8px; overflow: hidden;", leafletOutput("setup_minimap", height = "400px"))
                        ),
                        div(class = "setup-card",
                            div(class = "setup-card-header",
                                span(class = "setup-step-badge", "4"),
                                span(class = "setup-card-title", "Variable Mapping & Verification")
                            ),
                            p(class = "setup-card-sub", "Pair your Target (Actual) variables with their Predictions. You can map them manually below."),
                            fileInput("meta_file", "Upload Variable List (Optional)", accept = c(".xlsx", ".xls", ".csv")),
                            p(class = "setup-hint", "If you modify the auto-detected pairs, please click 'Confirm Variable Mapping' at the bottom."),
                            shinycssloaders::withSpinner(uiOutput("var_mapping_ui"), type = 6, color = "#2ecc71")
                        )
                     )
                 )
        ),
                tabPanel("2. Map Viewer", value = "tab_map",
                         div(style="position: relative;",
                             div(id="map_processing_overlay", class="map-processing-overlay",
                                 shinyjs::hidden(div(id="map_spinner", class="premium-spinner")),
                                 h3(id="map_processing_title", "Awaiting Spatial Interpolation", style="margin-bottom:10px; font-weight:bold;"),
                                 p(id="map_progress_text", HTML("Please configure parameters in the left panel and click <b>'Run Interpolation'</b> to generate geostatistical maps and review diagnostic results."), style="font-size:15px; margin-bottom:20px; color:#555; text-align: center; max-width: 500px; line-height: 1.45;"),
                                 shinyjs::hidden(
                                   div(id="map_progress_bar_container", class="premium-progress-bar-container",
                                       div(id="map_progress_bar_inner", class="premium-progress-bar-inner")
                                   )
                                 ),
                                 shinyjs::hidden(
                                     actionButton("reveal_maps_btn", "Reveal Maps & Enable Analysis", class="btn-success btn-lg", style="box-shadow: 0 4px 15px rgba(46, 204, 113, 0.4); border: none; font-weight: bold; padding: 12px 30px; border-radius: 30px; transition: all 0.3s;")
                                 ),
                                 shinyjs::hidden(
                                     actionButton("cancel_model_btn", "Cancel Generation", class="btn-danger btn-sm", style="margin-top: 15px; border-radius: 20px; font-weight: bold;")
                                 )
                             ),
                             div(style="margin-bottom:10px; display: flex; align-items: center; gap: 10px; flex-wrap: wrap; background-color: #f8f9fa; padding: 10px; border-radius: 5px; border: 1px solid #ddd;",
                             div(style="display: flex; align-items: center; gap: 10px; margin-right: 15px;",
                                 checkboxInput("show_points_viewer", HTML(paste0("Show Points", info_tooltip("show_points_info", "Rendering the sampling points on the map can take a while, up to ~30 seconds depending on the number of samples and the size of the dataset. The map stays responsive while the points are being drawn."))), FALSE, width = "auto"),
                                 checkboxInput("show_res_overlay", "Show Res", FALSE, width = "auto"),
                                 checkboxInput("show_north", "North Arrow", FALSE, width = "auto"),
                                 checkboxInput("show_borders", "Borders", FALSE, width = "auto"),
                                 checkboxInput("show_scale", "Map Scale", FALSE, width = "auto"),
                                 selectInput("base_map_layer", NULL,
                                             choices = c("Satellite (Esri)" = "Esri.WorldImagery",
                                                         "Topographic" = "OpenTopoMap",
                                                         "Standard Street" = "OpenStreetMap",
                                                         "Dark Matter" = "CartoDB.DarkMatter",
                                                         "Light (Positron)" = "CartoDB.Positron"),
                                             selected = "Esri.WorldImagery", width = "160px", selectize = FALSE),
                                 uiOutput("locality_pan_ui"),
                                 div(title = "Switch between the surfaces computed by the last interpolation run. Rerun to change variable or method.",
                                     uiOutput("map_view_ui"))
                                 ),
                             actionButton("refresh_map_area", "Refresh Map Area", icon = icon("sync"), class = "btn-info btn-sm", style = "margin-left: auto;"),
                             actionButton("show_popup_settings", "Pop-up Settings", icon = icon("cog"), class = "btn-info btn-sm"),
                             actionButton("quick_export_map", "Quick Export", icon = icon("camera"), class = "btn-info btn-sm", title = "Immediately send the currently viewed map to the Export Registry."),
                             actionButton("toggle_pt_style", NULL, icon = icon("palette"), class = "btn-sm",
                                          style = "background-color: #6c5ce7; color: white; border: none;",
                                          title = "Point Styling Options"),
                             div(class="map-toolbar-export-container", style="display: flex; align-items: center; gap: 5px; border-left: 1px solid #ccc; padding-left: 10px;",
                                 selectInput("polygon_export_format", NULL, choices = c("Shapefile (ZIP)" = "shp", "GeoJSON" = "geojson", "KML" = "kml", "GPKG" = "gpkg"), selected = "shp", width = "120px", selectize = FALSE),
                                 downloadButton("polygon_download_btn", "Export Manually Drawn Polygon", class = "btn-success btn-sm", style = "padding: 4px 10px; font-size: 12px; line-height: 1.5; border-radius: 3px;",
                                                title = "Available once you have drawn at least one polygon on the map using the drawing toolbar (left edge of the map). Downloads all drawn polygons in the format selected on the left."),
                                 downloadButton("export_updated_data", "Export Updated Dataset", class = "btn-success btn-sm", style = "padding: 4px 10px; font-size: 12px; line-height: 1.5; border-radius: 3px;",
                                                title = "Use after modifying your dataset in the app - e.g. after drawing a polygon on the map and saving it as a new group ('Assign Locality / Analysis Group'). Downloads the current dataset as .xlsx, including the 'Assigned_Locality' column.")
                             )
                         ),
                         shinyjs::hidden(
                           div(id = "pt_style_toolbar",
                             style = "margin-bottom:10px; padding: 12px 15px; background: linear-gradient(135deg, #2d3436 0%, #1e272e 100%); border-radius: 6px; border: 1px solid #636e72; color: #dfe6e9; display: flex; flex-wrap: wrap; align-items: flex-start; gap: 18px;",
                             div(style = "min-width: 160px;",
                               tags$label("Color By", style = "font-size: 11px; color: #a0aec0; margin-bottom: 2px; display: block; text-transform: uppercase; letter-spacing: 0.5px;"),
                               selectInput("pt_color_by", NULL, choices = c("None (Cyan)" = "none"), selected = "none", width = "160px", selectize = FALSE),
                               selectInput("pt_palette", "Palette", choices = c("Set1", "Dark2", "Paired", "Set2", "Set3", "Accent", "Pastel1", "Tableau10"), selected = "Set1", width = "160px", selectize = FALSE),
                               actionButton("pt_custom_colors", "Custom Colors...", icon = icon("paint-brush"), class = "btn-xs btn-default",
                                            style = "margin-top: 4px; background-color: #4a5568; color: #e2e8f0; border-color: #2d3748;")
                             ),
                             div(style = "min-width: 160px;",
                               tags$label("Labels", style = "font-size: 11px; color: #a0aec0; margin-bottom: 2px; display: block; text-transform: uppercase; letter-spacing: 0.5px;"),
                               checkboxInput("pt_show_labels", "Show Labels", FALSE, width = "auto"),
                               selectInput("pt_label_field", "Label Field", choices = c("(none)" = "none"), selected = "none", width = "160px", selectize = FALSE),
                               sliderInput("pt_label_size", "Label Size", min = 8, max = 18, value = 11, step = 1, width = "150px", ticks = FALSE)
                             ),
                             div(style = "min-width: 130px;",
                               tags$label("Point Options", style = "font-size: 11px; color: #a0aec0; margin-bottom: 2px; display: block; text-transform: uppercase; letter-spacing: 0.5px;"),
                               sliderInput("pt_marker_size", "Point Size", min = 1, max = 12, value = 3, step = 1, width = "130px", ticks = FALSE),
                               checkboxInput("pt_apply_minimap", "Apply Colour Set to Mini Map", FALSE, width = "auto")
                             )
                           )
                         ),
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
        tabPanel("3. Scientific Analysis & Summary",
                 conditionalPanel(
                   condition = "output.model_ready == 'no'",
                   div(style = "text-align: center; padding: 120px 50px; color: #888;",
                       icon("microscope", class = "fa-4x", style = "margin-bottom: 20px; color: #ccc;"),
                       h3("Awaiting Scientific Analysis", style = "font-weight: 300; margin-bottom: 10px;"),
                       p("Fit spatial interpolation models on the left pane and click 'Run Interpolation' to discover spatial structures and diagnostics.")
                   )
                 ),
                 conditionalPanel(
                   condition = "output.model_ready == 'yes'",
                   fluidRow(
                     column(6, uiOutput("locality_selector_ui")),
                     column(6, shinyWidgets::radioGroupButtons("sci_name_mode", "Variable naming:",
                                            choices = c("Variable labels" = "label", "Column names" = "colname"),
                                            selected = "label", size = "sm"))
                   ),
                   fluidRow(
                     column(8,
                            conditionalPanel(condition = "output.sci_diag_method == 'OK'",
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
                            conditionalPanel(condition = "!['OK', 'RK', 'RFK', 'CK', 'TPS'].includes(output.sci_diag_method)",
                              div(style="padding: 20px; text-align: center; color: #666;",
                                  h4("Diagnostic Mode Active"),
                                  p("Detailed spatial diagnostics are currently optimized for Kriging and TPS."))
                            ),
                            # Anisotropy is a property of the sampled field, not
                            # of the engine, so this card is shown for every
                            # method (including IDW/TPS, which have no variogram
                            # of their own).
                            div(id = "directional_vgm_ui",
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
                     ),
                     column(4,
                            sci_card("Spatial Interpolation Statistics",
                              "Model-specific diagnostics and performance metrics (RMSE, R2).",
                              accent = "#fab005",
                              conditionalPanel(condition = "output.disp_method == 'OK'",
                                h5("Variogram Parameters (per locality)"), div(class="table-container", DT::dataTableOutput("vgm_params_table")),
                                hr(style="opacity: 0.3;")
                              ),
                              conditionalPanel(condition = "['IDW', 'TPS'].includes(output.disp_method)",
                                h5("Regional Parameters (per locality)"), div(class="table-container", DT::dataTableOutput("regional_params_table")),
                                hr(style="opacity: 0.3;")
                              ),
                              h5("Model Performance"), uiOutput("cv_strategy_badge"), div(class="table-container", DT::dataTableOutput("metrics_table"))
                            ),
                            div(id = "prediction_performance_ui",
                              sci_card("Variable Prediction Statistics",
                                "Prediction accuracy and classification agreement metrics for uploaded data.",
                                accent = "#9b59b6",
                                h5("Prediction Performance (Uploaded Data)"),
                                div(class="table-container", DT::dataTableOutput("uploaded_metrics_table")),
                                hr(style="opacity: 0.3;"),
                                h5("Classification Performance (Uploaded Predictions)"),
                                selectInput("kappa_bin_method", "Binning Method:", choices = c("Agronomical Classes" = "agro", "Quartiles" = "quartile")),
                                div(class="table-container", DT::dataTableOutput("kappa_table"))
                              )
                            ),
                            sci_card("Data Summary Statistics",
                              "Aggregated descriptive statistics and area coverage for the data.",
                              accent = "#339af0",
                              conditionalPanel(condition = "!['agro', 'bin'].includes(input.color_style)",
                                tags$p(style="font-size: 0.85em; color: #666; font-style: italic;",
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
                                hr(style="border-top: 1px solid #339af0;")
                              ),
                              conditionalPanel(condition = "output.disp_method && output.disp_method != ''",
                                h5("Descriptive Statistics"),
                                div(class="table-container", DT::dataTableOutput("stats_table_total")),
                                div(class="table-container", DT::dataTableOutput("stats_table_loc"))
                              )
                            )
                     )
                   )
                 ),
                 hr(),
                 uiOutput("run_config_display"),
                 verbatimTextOutput("log_output")),
        tabPanel("4. Export Panel",
                 div(style = "padding: 20px;",
                     h2("Unified Session Export Registry"),
                     p("Manage all maps and tables generated during this session. Select an item to customize and export."),
                     hr(),
                     fluidRow(
                       column(12,
                              div(style = "background-color: white; padding: 20px; border: 1px solid #ddd; border-radius: 8px;",
                                  h4("Session Assets"),
                                  div(style = "margin-bottom: 10px;",
                                      actionButton("select_all_assets", "Select All", class = "btn-xs"),
                                      actionButton("deselect_all_assets", "Deselect All", class = "btn-xs")
                                  ),
                                  uiOutput("export_registry_ui"),
                                  div(style = "display: flex; gap: 10px; margin-top: 15px;",
                                      actionButton("open_styler", "Open Export Styler", class = "btn-primary", icon = icon("palette")),
                                      downloadButton("batch_export", "Batch Export Selected", class = "btn-success", title = "Download all checked items as a ZIP archive."),
                                      actionButton("clear_registry", "Clear Session Registry", class = "btn-danger", icon = icon("trash"))
                                  )
                                  )
                                  )
                                  ),
                     hr(),
                     div(style = "background-color: #fff3cd; padding: 20px; border: 1px solid #ffc107; border-radius: 8px;",
                         h4(icon("archive"), "Run History Archive"),
                         tags$p(style="font-size: 0.85em; opacity: 0.8; font-style: italic;", "Previous model runs are archived here. You can restore or permanently remove them."),
                         uiOutput("run_history_ui"),
                         uiOutput("reset_archive_choice_ui")
                     )
                                  )),
        tabPanel("Descriptive and Exploratory Suite",
                 desc_exploratory_ui("exploratory")
        ),
        tabPanel("Classification Suite",
                 classif_ui("classification")
        )      )
    )
