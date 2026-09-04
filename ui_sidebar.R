# ui_sidebar.R - sidebar panel as a plain variable assignment (no function
# wrapper), consumed by ui_main.R inside sidebarLayout().
ui_sidebar_panel <- sidebarPanel(width = 3,
      # The suite tabs are matched on their value= ids (tab_desc / tab_classif),
      # never on their titles - the titles carry the "5."/"6." numbering and are
      # free to be reworded without touching these conditions.
      # Neither suite reads the Context selections - the Exploratory module is
      # handed the raw dataset and picks its own variables, and Classification
      # configures target/predictors in its own panel - so the section is only
      # shown for the tabs it actually drives.
      conditionalPanel(
        condition = "input.main_tabs !== 'tab_desc' && input.main_tabs !== 'tab_classif'",
      div(class = "mn-section",
        tags$details(class = "sidebar-section", `data-key` = "context", open = NA,
          tags$summary(h4("Context")),
          div(
          selectInput("locality", "Locality", choices = NULL, multiple = TRUE),
          selectInput("var_category", "Variable Category", choices = NULL),
          shinyWidgets::pickerInput("var_id", "Variable", choices = NULL,
                                    options = shinyWidgets::pickerOptions(liveSearch = TRUE, size = 10)),
          div(class = "mn-seg-grid", shinyWidgets::radioGroupButtons("value_type", HTML(paste0("Primary View", info_tooltip("primary_view_info", "<b>Actual Values (observed):</b> Maps the raw observed/measured ground-truth data points directly without any machine learning predictions.<br><br><span style='border-top: 1px solid #ddd; display: block; margin: 8px 0;'></span><b>Machine Learning Predictions:</b> Use these options if you want to map predicted parameters from your machine learning models:<br><br>• <b>Best ML Predictions (_cve):</b> Maps predicted values from the cross-validation ensemble (CVE), which represent the best overall ML predictions.<br><br>• <b>Single Split ML Predictions (_ss):</b> Maps predicted values from a single train/test split partition.<br><br>• <b>Residuals (v - pv) of ML Predictions:</b> Maps ML model residuals (observed Actual value minus the ML Predicted value uploaded in your dataset) to study local spatial error patterns. These are NOT errors of the interpolation itself."))), choices = c("Actual values" = "actual", "ML predictions" = "pred", "Single split" = "pred_ss", "Residuals" = "resid"),
                       size = "sm")),
                     conditionalPanel(
                       condition = "input.value_type == 'pred_ss'",
                       selectInput("subset", HTML(paste0("Data Subset", info_tooltip("data_subset_info", "Restricts the Single Split (_ss) view to one data partition (e.g. Train/Test/Validation), read from a 'subset' column in the uploaded data. Available choices are detected when a dataset containing such a column is loaded."))), choices = c("All" = "all"), selected = "all")
                     ),
                     # The three boxes below are not sub-options of Primary View:
                     # they configure one thing between them - whether the actual
                     # and predicted surfaces are compared, how each is fitted and
                     # how the two are coloured. Same hr() + h5() grouping the Map
                     # Styling section uses for Uncertainty Mapping.
                     conditionalPanel(
                       condition = "['pred', 'pred_ss', 'resid'].includes(input.value_type)",
                       hr(),
                       h5("Actual vs Predicted"),
                       checkboxInput("comp_mode", HTML(paste0("Comparison Mode", info_tooltip("comp_mode", "Splits the viewer to compare the Actual (observed) map against the map of your uploaded ML predictions. Useful for visual validation."))), FALSE)
                     ),          conditionalPanel(condition = "input.comp_mode && ['pred', 'pred_ss'].includes(input.value_type)", 
                           checkboxInput("sep_fit", HTML(paste0("Fit Actual/Predicted Separately", info_tooltip("sep_fit_info", "If checked, optimizes variograms separately for actual and predicted data. If unchecked, applies actual variogram to predictions."))), TRUE),
                           checkboxInput("match_scales", HTML(paste0("Match Scales", info_tooltip("match_info", "Forces the map legends for Actual and Predicted data to use the same color range."))), FALSE))
        ))
      )
      ),
      conditionalPanel(
        condition = "input.main_tabs !== 'tab_desc' && input.main_tabs !== 'tab_classif'",
        div(class = "mn-section",
          tags$details(class = "sidebar-section", `data-key` = "engine", open = NA,
            tags$summary(h4("Spatial Engine")),
            div(
            div(class = "mn-seg-grid", shinyWidgets::radioGroupButtons("method", HTML(paste0("Interpolation", info_tooltip("method_info", "Cross-validation strategy is selectable below. It governs the reported Model Performance metrics only, never the prediction surface. Folds use a fixed seed (12345) for reproducibility. See Scientific Guide Section 5 for details."))),
                        choices = c("Ordinary kriging" = "OK",
                                    "Regression kriging" = "RK",
                                    "Random forest kriging" = "RFK",
                                    "Co-kriging" = "CK",
                                    "IDW" = "IDW",
                                    "Thin plate spline" = "TPS"),
                        size = "sm")),
            conditionalPanel(condition = "input.method == 'CK'",
              sliderInput("ck_nmax", HTML(paste0("CK Max Neighbors", info_tooltip("ck_nmax_info", "Search neighbourhood for every variable in the co-kriging system: each prediction uses only the closest N samples. Smaller values assume local stationarity and are faster; larger values approach a global neighbourhood. Default 15. See Scientific Guide Section 1.4."))), min = 5, max = 60, value = 15, step = 1),
              helpText(HTML("<em style='color: var(--mn-text-3); font-size: 0.9em; font-style: normal;'>The neighbourhood is a modelling choice, not just a speed setting: it controls how local the stationarity assumption is.</em>"))
            ),
            shinyWidgets::radioGroupButtons("cv_strategy",
              HTML(paste0("Cross-Validation Strategy", info_tooltip("cv_strategy_info", "How held-out folds are formed for the reported performance metrics. It does not change the interpolated map, except that the IDW power optimizer tunes under this same strategy, so re-running it after a change can store a different power. Auto (Default): LOOCV for n ≤ 50, seeded random 10-fold above. Standard LOOCV: full leave-one-out, the most rigorous, but noticeably slow beyond ~2000 samples (especially RK/RFK, which refit the variogram every fold). Spatial Block CV: 10 spatially-clustered (k-means) folds that hold out contiguous regions to curb the optimistic bias random folds suffer under spatial autocorrelation; recommended for DSM-style validation. Below n=30 it degrades to LOOCV."))),
              choices = c("Auto (Default)" = "auto", "Standard LOOCV" = "loocv", "Spatial Block CV" = "block"),
              selected = "auto", size = "sm", direction = "vertical", justified = TRUE),

            # Repeated CV. Hidden under Standard LOOCV, whose folds are
            # deterministic (every "repeat" is the same partition); under Auto
            # it still collapses to a single realization for any locality with
            # n <= 50, which the run log reports.
            conditionalPanel(condition = "input.cv_strategy != 'loocv'",
              checkboxInput("cv_repeat_on",
                HTML(paste0("Repeated CV (fold-realization stability)", info_tooltip("cv_repeat_info", "OFF (default): metrics come from ONE fold assignment (fixed seed 12345), which is reproducible and keeps method comparisons paired. ON: the cross-validation is re-run under additional fold assignments (seeds 12346, 12347, ...) and an extra table reports each metric as mean ± SD across realizations, so you can see whether a difference between two methods is larger than the split-to-split noise. The reported single-realization numbers and the interpolated map are IDENTICAL either way - realization 1 is the reference run. Cost: one extra full cross-validation per repeat (RK/RFK refit the variogram in every fold, so 5 repeats is roughly 5x the CV time). Leave-one-out plans are deterministic and are never repeated."))),
                value = FALSE),
              conditionalPanel(condition = "input.cv_repeat_on == true",
                selectInput("cv_repeat_n", "Fold realizations:",
                            choices = c("3 (fast)" = 3, "5 (recommended)" = 5, "10 (thorough)" = 10),
                            selected = 5),
                helpText(HTML("<em style='color: var(--mn-text-3); font-size: 0.9em; font-style: normal;'>Adds one full cross-validation pass per realization. The map and the reported metrics do not change; the extra table quantifies how much of a metric gap is fold luck.</em>"))
              )
            ),

            conditionalPanel(condition = "input.method == 'RFK'",
              radioButtons("rfk_uncertainty",
                HTML(paste0("RFK Uncertainty Method", info_tooltip("rfk_unc_info", "Controls ONLY the RFK uncertainty (variance) map, never the prediction surface, and never the reported metrics. Infinitesimal Jackknife (default, calibrated; Wager et al. 2014): the random-forest analogue of the regression standard error, a better-calibrated variance of the ensemble mean, slightly slower to compute. Ensemble spread (fast): the between-tree variance of the forest; a stability heuristic that understates true predictive uncertainty. See Scientific Guide Section 7.3."))),
                choices = c("Infinitesimal Jackknife (calibrated)" = "jackknife", "Ensemble spread (fast)" = "spread"),
                selected = "jackknife")
            ),

                       conditionalPanel(condition = "['RK', 'RFK', 'CK'].includes(input.method)",
                         div(class = "mn-subsection",
                           h5(HTML(paste0("Auxiliary Variables", info_tooltip("aux_info", "Select secondary variables to assist interpolation (e.g. Elevation). Ensure they are strongly correlated with the target. If VIF > 10, they are dropped to avoid multicollinearity.")))),
                           uiOutput("covariate_selector_ui"),
                           fluidRow(
                             column(6, selectInput("corr_pval_thresh", "Max P-Value:", choices = c("All" = 1, "0.05" = 0.05, "0.01" = 0.01, "0.001" = 0.001), selected = 1)),
                             column(6, actionButton("calc_corr", "RANK BY CORR.", class = "btn-default btn-block", style="margin-top:25px;"))
                           ),
                           uiOutput("corr_results_ui")
                         )
                       ),
             
                       conditionalPanel(condition = "input.value_type == 'resid'",
                         div(class = "mn-subsection",
                           div(style = "display: flex; align-items: center;",
                             h5("Residual Diagnostics", style = "margin-top: 0; margin-bottom: 0;"),
                             actionLink("resid_info_btn", label = NULL, icon = icon("info-circle"), style = "color: var(--mn-accent); margin-left: 5px;")
                           ),
                           tags$p(style="font-size: 0.85em; margin: 5px 0;", tags$em("Residuals = observed values minus the ML-predicted values uploaded in your dataset. They diagnose your external ML model, not the interpolation itself.")),
                           tags$p(style="font-size: 0.85em; margin-bottom: 5px;", tags$b("Interpolated Delta:"), " Difference between two full surfaces (actual - prediction). Reveals regional zones of consistent over/under-prediction."),
                           tags$p(style="font-size: 0.85em; margin-bottom: 5px;", tags$b("Point Errors:"), " Local prediction errors [Observed - Predicted] shown at the exact sample locations, highlighting individual points of model failure."),
                           tags$p(style="font-size: 0.85em; margin-bottom: 0;", tags$b("Interpolated Point Errors:"), " IDW surface of those local errors (Export Panel only). Acts as an 'Uncertainty Map' of the spatial structure of model failure.")
                         )
                       ),          
            conditionalPanel(condition = "['OK', 'RK', 'RFK', 'CK'].includes(input.method)",
              shinyWidgets::radioGroupButtons("vgm_mode", HTML(paste0("Fitting Mode", info_tooltip("vgm_mode_info", "Optional convenience. Click OPTIMIZE ALL VARIOGRAMS to pre-compute and inspect the auto-fitted variogram curves, then (if you wish) switch to Manual to hand-tune the already-fitted Nugget / Partial Sill / Range. If you don't need manual tuning you can skip the button entirely: Run Analysis performs the identical auto-fit internally, so pressing it first does not change the map or metrics; it only lets you preview the fit and avoids a redundant wait."))), choices = c("Auto-Fit" = "auto", "Manual" = "manual"), size = "sm", justified = TRUE),
              conditionalPanel(condition = "input.vgm_mode == 'auto'",
                actionButton("auto_fit", "Optimize all variograms", class = "btn-default btn-block", style="margin-bottom:10px;")
              ),
              conditionalPanel(condition = "input.vgm_mode == 'manual'",
                div(class = "mn-subsection",
                    div(h5(HTML(paste0("Manual Tuning", info_tooltip("m_tune", "Switch to the Scientific Analysis tab to view the Variogram plot interactively updating as you slide the Nugget, Partial Sill, and Range sliders.")))), style="margin-bottom:5px;"),
                    selectInput("k_mod", "Variogram Model", choices = c("Sph", "Exp", "Gau", "Mat")),
                    selectInput("m_loc", "Locality to Tune", choices = NULL),
                    conditionalPanel(condition = "input.comp_mode == true || ['pred', 'pred_ss', 'resid'].includes(input.value_type)",
                      shinyWidgets::radioGroupButtons("m_target", "Target", choices = c("Actual" = "act", "Predicted" = "pre"), size = "sm", justified = TRUE)
                    ),
                    sliderInput("m_nugget", "Nugget", min = 0, max = 1, value = 0, step = 0.01),
                    sliderInput("m_psill", "Partial Sill", min = 0, max = 1, value = 1, step = 0.01),
                    sliderInput("m_range", "Range", min = 1, max = 1000, value = 100),
                    actionButton("apply_manual", "Apply manual model", class = "btn-default btn-block"),
                    # A hand-tuned fit describes the VALUE-scale variogram, so
                    # it is only meaningful for OK. RK/RFK model the residual
                    # variogram after the trend is removed and CK fits an LMC;
                    # imposing a value-scale model on either would be wrong, so
                    # they refit - which used to happen silently.
                    conditionalPanel(condition = "['RK', 'RFK', 'CK'].includes(input.method)",
                      tags$p(class = "mn-note-warn",
                             tags$b("Not used by the selected method. "),
                             "Manual variogram fits are consumed by Ordinary Kriging only. RK/RFK fit the residual variogram automatically after the trend is removed; CK fits a linear model of coregionalization.")
                    )
                )
              )
            ),
            
            conditionalPanel(condition = "input.method == 'IDW'",
                tuning_ui(
                    id = "idw", label = "IDW FACTORS",
                    global_slider_id = "idw_p", manual_slider_id = "idw_m_p",
                    global_slider_args = list(label = "Global IDW Power (p)", min = 0.5, max = 5, value = 2, step = 0.1),
                    manual_slider_args = list(label = "Power (p)", min = 0.5, max = 5, value = 2, step = 0.1),
                    optimize_btn_label = "OPTIMIZE IDW FACTORS",
                    manual_btn_label = "Apply Manual Power",
                    top_extra_ui = sliderInput("idw_nmax", HTML(paste0("Max Neighbors", info_tooltip("idw_nmax_info", "Limits the IDW calculation to the closest N points. This prevents distant, unrelated data from distorting local predictions. Select this BEFORE optimizing."))), min = 4, max = 50, value = 12),
                    extra_ui = div(style="background-color: var(--mn-surface-2); border: 1px solid var(--mn-line); border-radius: 4px; padding: 10px; color: var(--mn-text);", tableOutput("idw_metrics_table"))
                )
            ),
            
            conditionalPanel(condition = "input.method == 'TPS'",
                tuning_ui(
                    id = "tps", label = "TPS LAMBDA",
                    global_slider_id = "tps_lambda", manual_slider_id = "tps_m_lambda",
                    global_slider_args = list(label = "Global Smoothing (Lambda)", min = -1, max = 1, value = -1, step = 0.001),
                    manual_slider_args = list(label = "Lambda", min = -1, max = 1, value = -1, step = 0.001),
                    optimize_btn_label = "OPTIMIZE TPS LAMBDA",
                    manual_btn_label = "Apply Manual Lambda",
                    extra_ui = tagList(
                        conditionalPanel(condition = "input.tps_mode == 'auto'",
                            div(style = "display: flex; gap: 6px; margin-bottom: 6px;",
                                actionButton("tps_preset_auto", "Set Auto (GCV)", class = "btn-default btn-xs", style = "flex: 1;"),
                                actionButton("tps_preset_exact", "Set Exact (0)", class = "btn-default btn-xs", style = "flex: 1;")
                            )
                        ),
                        conditionalPanel(condition = "input.tps_mode == 'manual'",
                            div(style = "display: flex; gap: 6px; margin-bottom: 6px;",
                                actionButton("tps_m_preset_auto", "Set Auto (GCV)", class = "btn-default btn-xs", style = "flex: 1;"),
                                actionButton("tps_m_preset_exact", "Set Exact (0)", class = "btn-default btn-xs", style = "flex: 1;")
                            )
                        ),
                        p(style="font-size: 0.8em; opacity: 0.8;", "Lambda < 0: Auto (GCV Optimization); Lambda = 0: Exact interpolation; Lambda > 0: Manual Smoothing.")
                    )
                )
            ),
            
          ))
        ),

        div(class = "mn-section",
          tags$details(class = "sidebar-section", `data-key` = "domain", open = NA,
            tags$summary(h4("Domain & Grid")),
            div(
            div(class = "mn-seg-grid", shinyWidgets::radioGroupButtons("boundary_type", HTML(paste0("Boundary Type", info_tooltip("bound", "Defines how the interpolation surface is cropped. Convex hull wraps points tightly; Buffered adds padding."))), 
                        choices = c("Concave hull" = "concave",
                                    "Convex hull" = "convex",
                                    "Buffered" = "wrapped",
                                    "Point buffer" = "strict"),
                        size = "sm")),
            conditionalPanel(condition = "['wrapped', 'strict'].includes(input.boundary_type)",
              conditionalPanel(condition = "input.boundary_type == 'wrapped'",
                shinyWidgets::radioGroupButtons("buff_mode", HTML(paste0("Buffer Logic", info_tooltip("buff_logic_info", "Dynamic mode calculates buffer distance per locality based on point density and selected method. Fixed allows manual setting."))),
                             choices = c("Auto (Dynamic)" = "dynamic", "Fixed (Manual)" = "fixed"), selected = "dynamic", size = "sm", justified = TRUE)
              ),
              conditionalPanel(condition = "input.boundary_type == 'strict' || (input.boundary_type == 'wrapped' && input.buff_mode == 'fixed')",
                numericInput("buff_dist", HTML(paste0("Buffer Distance (m)", info_tooltip("buff_dist_info", "Sets the spatial buffer distance. For Strict Point mode, this acts as the fixed radius around each point."))), value = 250, min = 0)
              )
            ),
            
            shinyWidgets::radioGroupButtons("res_mode", HTML(paste0("Resolution Logic", info_tooltip("res", "Dynamic modes calculate cell size based on spatial extent. Manual forces a specific cell size (e.g. 10m)."))),
                         choices = c("Auto (Per Locality)" = "local", "Auto (Global)" = "global", "Fixed" = "fixed"),
                         size = "sm", direction = "vertical", justified = TRUE),
            conditionalPanel(condition = "input.res_mode == 'fixed'",
              sliderInput("grid_res", "Manual Resolution", min = 5, max = 500, value = 50)
            ),
            
            div(class = "mn-subsection", 
                tableOutput("loc_res_table"),
                uiOutput("strict_buffer_note"),
                conditionalPanel(condition = "input.res_mode == 'fixed' && input.boundary_type == 'wrapped' && input.buff_mode == 'dynamic'",
                  p(style="font-size: 0.78em; margin-top: 8px; border-left: 2px solid var(--mn-accent); padding-left: 8px; color: var(--mn-text-3); font-style: italic; line-height: 1.35;", 
                    "Note: Dynamic buffers scale with the physical sample density (spacing) to prevent spatial clipping, completely independent of your manual grid pixel size.")
                )
            ),
            
          ))
        ),

        div(class = "mn-section",
          tags$details(class = "sidebar-section", `data-key` = "styling", open = NA,
            tags$summary(h4("Map Styling")),
            div(
            shinyWidgets::radioGroupButtons("color_style", "Styling",
              choices = c("Continuous" = "cont", "Binned (5)" = "bin", "Agronomical" = "agro"),
              size = "sm", justified = TRUE),
            uiOutput("palette_ui"),
            conditionalPanel(condition = "input.color_style == 'agro'",
                selectInput("agro_method", "Algorithm", choices = c("Supervised" = "limits", "Jenks" = "jenks", "K-means" = "kmeans")),
                sliderInput("agro_n_classes", "Classes", min = 2, max = 5, value = 3),
                uiOutput("agro_options"),
                uiOutput("agro_pending_note"),
                actionButton("agro_apply", "Apply to maps and statistics", class = "btn-primary btn-block", style = "margin-bottom: 6px;")),
            hr(),
            h5("Uncertainty Mapping"),
            # Keyed to the method of the DISPLAYED run (disp_method): this
            # toggles a view of the map on screen, so picking a non-kriging
            # method for the next run must not remove it (and vice versa).
            conditionalPanel(condition = "['OK', 'RK', 'RFK', 'CK'].includes(output.disp_method)",
              checkboxInput("show_uncertainty", "Map Uncertainty Instead of Interpolation", FALSE),
              conditionalPanel(condition = "input.show_uncertainty",
                shinyWidgets::radioGroupButtons("uncertainty_type", "Metric", choices = c("Variance" = "var", "Standard Error" = "se"), selected = "se", size = "sm", justified = TRUE),
                p(style="font-size: 0.8em; opacity: 0.8; margin-bottom: 0;", "Variance is in squared units of the variable; SE shares the variable's unit. Uncertainty layers always use a continuous palette; Agronomic/Binned class breaks apply to concentration maps only.")
              )
            ),
            conditionalPanel(condition = "!['OK', 'RK', 'RFK', 'CK'].includes(output.disp_method)",
              p(style="font-size: 0.8em; opacity: 0.8;", "Uncertainty mapping becomes available once a Kriging-based map has been generated.")
            )
          ))
        ),

        div(class = "mn-section",
          tags$details(class = "sidebar-section", `data-key` = "management", open = NA,
            tags$summary(h4("Session")),
            div(style = "display: flex; gap: 8px;",
                actionButton("save_config", "Save config", class = "btn-default", style = "flex:1;"),
                shinyFilesButton("load_config", "Load config", "Select Config", multiple = FALSE, class = "btn-default", style = "flex:1;")
            )
          )
        ),
        div(class = "sidebar-run-sticky",
          uiOutput("run_estimate_line"),
          actionButton("run", "Run Interpolation", class = "btn-primary btn-lg", style = "width:100%;")
        )
      ),
      
      conditionalPanel(
        condition = "input.main_tabs === 'tab_desc'",
        div(class = "mn-sidebar-tail", style = "background-color: var(--mn-surface-2); padding: 12px; border: 1px solid var(--mn-line); border-radius: 6px; margin-top: 10px;",
            h4("Exploratory Suite Active", style="margin-top: 0; color: var(--mn-text); font-weight: 600;"),
            p(style="font-size:0.85em; color:var(--mn-text-3); line-height:1.45; margin-bottom: 0;",
              "Plot and analyze descriptive statistics, perform correlation analysis, and execute Principal Component Analysis (PCA) directly on your raw data. These tools operate independently of the spatial interpolation model configuration.")
        )
      ),
      conditionalPanel(
        condition = "input.main_tabs === 'tab_classif'",
        div(class = "mn-sidebar-tail", style = "background-color: var(--mn-surface-2); padding: 12px; border: 1px solid var(--mn-line); border-radius: 6px; margin-top: 10px;",
            h4("Classification Suite Active", style="margin-top: 0; color: var(--mn-text); font-weight: 600;"),
            p(style="font-size:0.85em; color:var(--mn-text-3); line-height:1.45; margin-bottom: 0;",
              "Train and evaluate predictive class models from co-sampled covariates. Everything the suite needs - target, predictors, localities, boundary, buffer, and grid resolution - is configured in its own panel on the left of the tab; the interpolation sidebar settings do not affect classification runs.")
        )
      )
    )
