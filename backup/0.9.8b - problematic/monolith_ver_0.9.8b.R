# Monolith: Advanced Spatial Analysis Dashboard v0.9.8b
# Copyright (c) 2026 Recep Serdar Kara. All rights reserved.
#
# This program is free software: you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation, either version 3 of the License, or
# (at your option) any later version.
#
# This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
# GNU General Public License for more details.
#
# You should have received a copy of the GNU General Public License
# along with this program. If not, see <https://www.gnu.org/licenses/>.

source("global_0.9.8b.R")

# --- Helpers ---

validate_crs <- function(crs_selection, error_prefix = "Invalid CRS provided", duration = NULL) {
  tryCatch({
    c_obj <- sf::st_crs(crs_selection)
    if (is.na(c_obj)) stop("Invalid CRS format.")
    
    # Test terra conversion
    t_obj <- terra::crs(crs_selection)
    if (t_obj == "") stop("Invalid CRS for terra.")
    
    c_obj
  }, error = function(e) {
    showNotification(paste(error_prefix, e$message), type = "error", duration = duration)
    NULL
  })
}

common_crs <- c(
  "WGS 84 (EPSG:4326)" = "EPSG:4326",
  "UTM 35N (EPSG:32635)" = "EPSG:32635",
  "UTM 33N (EPSG:32633)" = "EPSG:32633",
  "UTM 34N (EPSG:32634)" = "EPSG:32634",
  "S-JTSK / Krovak East North (EPSG:5514)" = "EPSG:5514",
  "Pseudo-Mercator (EPSG:3857)" = "EPSG:3857"
)

# --- Palette Helpers ---
# Mandatory palettes for (Simplified + Earthy colors)
dashboard_palettes <- c("viridis", "Greens", "Blues", "Oranges", "YlOrRd", "RdYlBu", "BrBG", "YlOrBr", "Greys", "Spectral")

# Precompute palette choices once to avoid expensive HTML rendering inside UI loops
palette_choices_precomputed <- (function() {
  pals <- dashboard_palettes
  labels <- sapply(pals, function(p) {
    # Generate a few colors for the visual preview
    cols <- if (p == "viridis") {
      viridis::viridis(5)
    } else {
      info <- RColorBrewer::brewer.pal.info
      max_cols <- if (p %in% rownames(info)) info[p, "maxcolors"] else 5
      n_cols <- max(3, min(5, max_cols))
      RColorBrewer::brewer.pal(n_cols, p)
    }
    # Create the HTML swatch row
    swatches <- paste0(sapply(cols, function(c) {
      sprintf('<div style="width: 15px; height: 15px; background-color: %s !important; border: 0.5px solid #ccc; display: inline-block; margin-left: 2px;"></div>', c)
    }), collapse = "")
    display_name <- if (p == "viridis") "Viridis" else p
    sprintf('<div style="display: flex; justify-content: space-between; align-items: center; width: 100%%;"><span>%s</span><div style="display: flex;">%s</div></div>', display_name, swatches)
  })
  setNames(pals, labels)
})()

styler_fields <- list(
  title_size = list(fn = updateSliderInput, name = "styler_title_size"),
  base_size = list(fn = updateSliderInput, name = "styler_base_size"),
  x_size = list(fn = updateSliderInput, name = "styler_x_size"),
  y_size = list(fn = updateSliderInput, name = "styler_y_size"),
  label_size = list(fn = updateSliderInput, name = "styler_label_size"),
  legend_size = list(fn = updateSliderInput, name = "styler_legend_size"),
  legend_key_size = list(fn = updateSliderInput, name = "styler_legend_key_size"),
  font_family = list(fn = updateSelectInput, name = "styler_font_family", val_param = "selected"),
  label_orient = list(fn = updateSelectInput, name = "styler_label_orient", val_param = "selected"),
  legend_pos = list(fn = updateSelectInput, name = "styler_legend_pos", val_param = "selected"),
  legend_dir = list(fn = updateSelectInput, name = "styler_legend_dir", val_param = "selected"),
  legend_text_angle = list(fn = updateSelectInput, name = "styler_legend_text_angle", val_param = "selected"),
  margin_t = list(fn = updateNumericInput, name = "styler_margin_t"),
  margin_r = list(fn = updateNumericInput, name = "styler_margin_r"),
  margin_b = list(fn = updateNumericInput, name = "styler_margin_b"),
  margin_l = list(fn = updateNumericInput, name = "styler_margin_l"),
  show_grid = list(fn = updateCheckboxInput, name = "styler_show_grid"),
  high_contrast = list(fn = updateCheckboxInput, name = "styler_high_contrast"),
  aspect_ratio = list(fn = updateNumericInput, name = "styler_aspect_ratio"),
  width = list(fn = updateNumericInput, name = "styler_width"),
  height = list(fn = updateNumericInput, name = "styler_height"),
  dpi = list(fn = updateNumericInput, name = "styler_dpi"),
  format = list(fn = updateSelectInput, name = "styler_format", val_param = "selected")
)

sync_styler_config <- function(cfg, session) {
  for (key in names(styler_fields)) {
    val <- cfg[[key]]
    if (is.null(val)) val <- cfg[[paste0("styler_", key)]]
    
    if (!is.null(val)) {
      field <- styler_fields[[key]]
      args <- list(session = session, inputId = field$name)
      val_param <- if (!is.null(field$val_param)) field$val_param else "value"
      args[[val_param]] <- val
      do.call(field$fn, args)
    }
  }
}

# --- Safe Concaveman Boundary Helper ---
safe_concaveman <- function(pts) {
  if (nrow(pts) < 3) {
    return(sf::st_convex_hull(sf::st_union(pts)))
  }
  b <- tryCatch({
    concaveman::concaveman(pts)
  }, error = function(e) {
    NULL
  })
  
  if (is.null(b) || sf::st_is_empty(b) || sf::st_geometry_type(b) == "GEOMETRYCOLLECTION") {
    return(sf::st_convex_hull(sf::st_union(pts)))
  }
  return(b)
}

# --- Clean gstat Environment Helper ---
clean_gstat_env <- function(vgm_obj) {
  if (is.null(vgm_obj)) return(NULL)
  if (is.list(vgm_obj)) {
    if (!is.null(attr(vgm_obj, "formula"))) {
      environment(attr(vgm_obj, "formula")) <- emptyenv()
    }
    if (!is.null(attr(vgm_obj, "call"))) {
      attr(vgm_obj, "call") <- NULL
    }
  }
  return(vgm_obj)
}

# --- Scientific Variogram Parameters ---

# --- Robust Variogram Fitting ---
robust_vgm_fit <- function(v_emp, v_data) {
  initial_sill <- var(v_data, na.rm=TRUE)
  if (is.na(initial_sill) || initial_sill == 0) initial_sill <- 1
  
  initial_nugget <- min(v_emp$gamma)
  # Stability fix: Ensure a tiny nugget to prevent singular matrices in krige()
  if (initial_nugget == 0) initial_nugget <- max(initial_sill * 1e-6, 1e-6)
  
  if (initial_nugget > initial_sill) initial_nugget <- initial_sill * 0.9
  initial_psill <- max(initial_sill - initial_nugget, initial_sill * 0.1)
  
  max_dist <- max(v_emp$dist, na.rm = TRUE)
  if (is.na(max_dist) || is.infinite(max_dist) || max_dist <= 0) {
    max_dist <- 1.0 # Safe default positive distance fallback
  }
  
  initial_range <- max_dist / 4
  models <- c("Sph", "Exp", "Gau", "Mat") # Added Matern
  
  fits <- lapply(models, function(m) {
    tryCatch({
      # Try fitting with initial guesses
      start_kappa <- if(m == "Mat") 1.5 else 0.5
      f <- gstat::fit.variogram(v_emp, gstat::vgm(psill = initial_psill, model = m, range = initial_range, nugget = initial_nugget, kappa = start_kappa))
      sse <- attr(f, "SSErr")
      if (!is.null(sse) && f$range[2] > (max_dist/100) && f$range[2] < max_dist * 2 && f$psill[2] > 0) {
        return(list(fit = f, sse = sse))
      }
      return(NULL)
    }, error = function(e) NULL)
  })
  
  valid_fits <- Filter(Negate(is.null), fits)
  best_fit <- NULL
  if (length(valid_fits) > 0) {
    best_idx <- which.min(sapply(valid_fits, function(x) x$sse))
    best_fit <- valid_fits[[best_idx]]$fit
  }
  
  if (is.null(best_fit)) {
    if (initial_nugget > initial_sill * 0.8) {
      best_fit <- gstat::vgm(psill = initial_sill * 0.05, "Sph", range = max_dist/10, nugget = initial_sill * 0.95)
    } else {
      best_fit <- gstat::vgm(psill = initial_sill * 0.8, "Sph", range = max_dist/2, nugget = initial_sill * 0.2)
    }
    if (!is.null(shiny::getDefaultReactiveDomain())) {
      tryCatch({ showNotification("Variogram auto-fit failed. Using fallback.", type = "warning", duration = 5) }, error=function(e) NULL)
    }
  }
  return(best_fit)
}

# 

# --- UI ---
ui <- fluidPage(
  useShinyjs(),
  render_docs_drawer(),
  
  # Static default theme to prevent FOUC (flash of unstyled content) on startup
  fresh::use_theme(app_themes[["Muted Sage"]]$theme),
  tags$head(
    tags$style(HTML(app_themes[["Muted Sage"]]$manual_style))
  ),
  
  uiOutput("dynamic_theme"),
  tags$head(
    tags$style(HTML("
      .bootstrap-select .dropdown-menu li a span.text { display: flex !important; width: 100% !important; align-items: center; justify-content: space-between; }
      #shiny-notification-container { position: fixed; top: 20px; right: 20px; width: 380px; z-index: 99999; }
      .shiny-notification { width: 100% !important; }
      .well { padding: 15px; }
      .header-panel { background-color: #2c3e50; color: white; padding: 10px 20px; margin-bottom: 20px; border-radius: 0 0 10px 10px; display: flex; justify-content: space-between; align-items: center; }
      .header-title { margin: 0; font-weight: bold; font-size: 24px; }
      .header-controls { display: flex; align-items: center; gap: 20px; }
      .info-btn { background: none; border: 1px solid #ecf0f1; color: #ecf0f1; border-radius: 50%; width: 25px; height: 25px; display: flex; align-items: center; justify-content: center; cursor: pointer; }
      .table-container { width: 100%; overflow-x: auto; font-size: 0.95em; margin-bottom: 10px; }
      .table-container table { width: 100% !important; margin-bottom: 0; background-color: #ffffff !important; color: #000000 !important; }
      .table-container th { background-color: #f8f9fa !important; color: #000000 !important; }
      .popover { color: #333 !important; background-color: #fff !important; max-width: 400px; }
      .popover-header { color: #333 !important; background-color: #f8f9fa !important; border-bottom: 1px solid #ebebeb; }
      .popover-body { color: #333 !important; }
      .expand-icon-btn { position: absolute; top: 10px; right: 10px; z-index: 100; opacity: 0.8; width: 32px; height: 32px; padding: 0 !important; display: inline-flex !important; align-items: center !important; justify-content: center !important; }
      .expand-icon-btn > * { margin: 0 !important; padding: 0 !important; }
      #pt_style_toolbar .form-group { margin-bottom: 4px !important; }
      #pt_style_toolbar select, #pt_style_toolbar .form-control { background-color: #4a5568; color: #e2e8f0; border-color: #2d3748; font-size: 12px; height: 28px; padding: 2px 6px; }
      #pt_style_toolbar .irs--shiny .irs-bar { background: #6c5ce7; }
      #pt_style_toolbar .irs--shiny .irs-handle { border-color: #6c5ce7; }
      #pt_style_toolbar .irs--shiny .irs-single { background: #6c5ce7; }
      #pt_style_toolbar .checkbox label { color: #e2e8f0; font-size: 12px; }
      #pt_style_toolbar label { font-weight: normal; color: #a0aec0; }
      #pt_style_toolbar .btn-xs { font-size: 11px; padding: 2px 8px; }
      .map-toolbar-export-container .form-group { margin-bottom: 0 !important; }
    ")),
    uiOutput("dynamic_manual_style"),
    tags$script(HTML("$(function () { $('[data-toggle=\"popover\"]').popover({html: true}); });"))
  ),
  
  div(class = "header-panel", style = "display: flex; justify-content: space-between; align-items: center; padding: 5px 20px;",
      img(src = "assets/banner.png", class = "header-banner", style = "max-height: 50px; width: auto; object-fit: contain; float: left;"),
      div(style = "flex-grow: 1;"),
      div(class = "header-controls", style = "display: flex; align-items: center; gap: 10px; margin-left: auto;",
          tags$style(HTML("
            .header-controls .shiny-input-container { width: auto !important; margin: 0 !important; }
            .header-controls .form-group { margin-bottom: 0 !important; margin-right: 0 !important; }
            .header-controls .checkbox { margin: 0 !important; padding: 0 !important; }
            .header-controls .checkbox label { margin: 0 !important; padding-left: 20px !important; color: white !important; font-size: 11px !important; }
            
            .header-controls .btn-header-circle,
            .header-controls .dropdown-toggle {
              background: #ffffff !important;
              color: #2c3e50 !important;
              border: none !important;
              width: 32px !important;
              height: 32px !important;
              border-radius: 50% !important;
              padding: 0 !important;
              display: inline-flex !important;
              align-items: center !important;
              justify-content: center !important;
              font-size: 0 !important;
              cursor: pointer !important;
              box-shadow: 0 2px 4px rgba(0,0,0,0.1) !important;
              transition: all 0.2s ease !important;
              margin: 0 !important;
            }
            .header-controls .btn-header-circle:hover,
            .header-controls .dropdown-toggle:hover {
              background: #f1f3f5 !important;
              transform: scale(1.08) !important;
            }
            .header-controls .dropdown {
              margin: 0 !important;
              padding: 0 !important;
              display: inline-flex !important;
              align-items: center !important;
              justify-content: center !important;
              width: 32px !important;
              height: 32px !important;
            }
            .header-controls .dropdown-toggle::after,
            .header-controls .dropdown-toggle .caret {
              display: none !important;
            }
            .header-controls .btn-header-circle i,
            .header-controls .dropdown-toggle i {
              font-size: 15px !important;
              line-height: 1 !important;
              width: 1em !important;
              text-align: center !important;
              margin: 0 !important;
              padding: 0 !important;
              display: inline-block !important;
            }
          ")),
          div(style = "display: flex; flex-direction: column; align-items: flex-start; font-size: 0.8em; line-height: 1; margin-right: 5px;",
              checkboxInput("show_north", "North Arrow", FALSE),
              checkboxInput("show_borders", "Borders", FALSE),
              checkboxInput("show_scale", "Map Scale", FALSE)
          ),
          theme_switcher_ui("theme_mod"),
          actionButton("info_btn", "", icon = icon("info"), class = "btn-header-circle"),
          actionButton("about_btn", "", icon = icon("question"), class = "btn-header-circle")
      )
  ),
  
  sidebarLayout(
    sidebarPanel(width = 3,
      div(style="background-color: #f8f9fa; padding: 10px; border: 1px solid #ddd;",
          h4("1. Context"),
          selectInput("locality", "Locality", choices = NULL, multiple = TRUE),
          selectInput("subset", HTML(paste0("Data Subset", info_tooltip("data_subset_info", "Use this filter when mapping predicted parameters from single-split or similar models. Selecting 'Train', 'Test', or 'Validation' restricts the analysis to that specific data partition."))), choices = c("All" = "all", "Test" = "Test", "Train" = "Train", "Validation" = "Validation"), selected = "all"),
          selectInput("var_category", "Variable Category", choices = NULL),
          selectInput("var_id", "Variable", choices = NULL),
          selectInput("value_type", HTML(paste0("Primary View", info_tooltip("primary_view_info", "<b>Actual Values (observed):</b> Maps the raw observed/measured ground-truth data points directly without any machine learning predictions.<br><br><span style='border-top: 1px solid #ddd; display: block; margin: 8px 0;'></span><b>Machine Learning Predictions:</b> Use these options if you want to map predicted parameters from your machine learning models:<br><br>• <b>Best Predictions (_cve):</b> Maps predicted values from the cross-validation ensemble (CVE), which represent the best overall ML predictions.<br><br>• <b>Single Split Predictions (_ss):</b> Maps predicted values from a single train/test split partition.<br><br>• <b>Residuals (v - pv):</b> Maps model residuals (difference between observed Actual and ML Predicted values) to study local spatial error patterns."))), choices = c("Actual Values" = "actual", "Best Predictions (_cve)" = "pred", "Single Split Predictions (_ss)" = "pred_ss", "Residuals (v - pv)" = "resid")),
                     conditionalPanel(
                       condition = "['pred', 'pred_ss', 'resid'].includes(input.value_type)",
                       checkboxInput("comp_mode", HTML(paste0("Comparison Mode", info_tooltip("comp_mode", "Splits the viewer to compare Actual vs. Predicted maps. Useful for visual validation."))), FALSE)
                     ),          conditionalPanel(condition = "input.comp_mode && ['pred', 'pred_ss'].includes(input.value_type)", 
                           checkboxInput("sep_fit", HTML(paste0("Fit Actual/Predicted Separately", info_tooltip("sep_fit_info", "If checked, optimizes variograms separately for actual and predicted data. If unchecked, applies actual variogram to predictions."))), TRUE),
                           checkboxInput("match_scales", HTML(paste0("Match Scales", info_tooltip("match_info", "Forces the map legends for Actual and Predicted data to use the same color range."))), FALSE))
      ),
      # Only show spatial parameters when NOT on the Descriptive Suite tab
      conditionalPanel(
        condition = "input.main_tabs !== '5. Descriptive and Exploratory Suite'",
        br(),
        div(style="background-color: #e7f5ff; padding: 10px; border: 1px solid #a5d8ff;",
            h4("2. Spatial Engine"),
            selectInput("method", "Interpolation", 
                        choices = c("Ordinary Kriging" = "OK", 
                                    "Regression Kriging" = "RK",
                                    "Random Forest Kriging" = "RFK",
                                    "Co-Kriging" = "CK",
                                    "IDW" = "IDW", 
                                    "Thin Plate Spline (TPS)" = "TPS")),
            
                       # Advanced Kriging Controls (RK, RFK, CK)
                       conditionalPanel(condition = "['RK', 'RFK', 'CK'].includes(input.method)",
                         div(style = "background-color: #f3f0ff; padding: 10px; border: 1px solid #d0bfff; border-radius: 4px; margin-bottom: 10px;",
                           h5(HTML(paste0("Auxiliary Variables", info_tooltip("aux_info", "Select secondary variables to assist interpolation (e.g. Elevation). Ensure they are strongly correlated with the target. If VIF > 10, they are dropped to avoid multicollinearity.")))),
                           uiOutput("covariate_selector_ui"),
                           fluidRow(
                             column(6, selectInput("corr_pval_thresh", "Max P-Value:", choices = c("All" = 1, "0.05" = 0.05, "0.01" = 0.01, "0.001" = 0.001), selected = 1)),
                             column(6, actionButton("calc_corr", "RANK BY CORR.", class = "btn-secondary btn-block", style="margin-top:25px;"))
                           ),
                           uiOutput("corr_results_ui")
                         )
                       ),
             
                       # Residual Information Section
                       conditionalPanel(condition = "input.value_type == 'resid'",
                         div(style = "background-color: #fff5f5; padding: 10px; border: 1px solid #ffc9c9; border-radius: 4px; margin-bottom: 10px;",
                           div(style = "display: flex; justify-content: space-between; align-items: center;",
                             h5("Residual Diagnostics"),
                             actionButton("resid_info_btn", "i", class = "info-btn")
                           ),
                           tags$p(style="font-size: 0.85em; margin-bottom: 5px;", tags$b("Interpolated Delta:"), " Difference between two full surfaces (actual - prediction). Reveals regional zones of consistent over/under-prediction."),
                           tags$p(style="font-size: 0.85em; margin-bottom: 0;", tags$b("Interpolated Point Errors:"), " Kriged map of local prediction errors. Acts as an 'Uncertainty Map' highlighting exact points of model failure.")
                         )
                       ),          
            # Static Kriging Controls
            conditionalPanel(condition = "['OK', 'RK', 'RFK', 'CK'].includes(input.method)",
              radioButtons("vgm_mode", "Fitting Mode", choices = c("Auto-Fit" = "auto", "Manual" = "manual"), inline = TRUE),
              conditionalPanel(condition = "input.vgm_mode == 'auto'",
                actionButton("auto_fit", "OPTIMIZE ALL VARIOGRAMS", class = "btn-info btn-block", style="margin-bottom:10px;")
              ),
              conditionalPanel(condition = "input.vgm_mode == 'manual'",
                div(style = "background-color: #fff9db; padding: 10px; border: 1px solid #fab005; border-radius: 4px; margin-bottom: 10px;",
                    div(h5(HTML(paste0("Manual Tuning", info_tooltip("m_tune", "Switch to the Scientific Analysis tab to view the Variogram plot interactively updating as you slide the Nugget, Partial Sill, and Range sliders.")))), style="margin-bottom:5px;"),
                    selectInput("k_mod", "Variogram Model", choices = c("Sph", "Exp", "Gau", "Mat")),
                    selectInput("m_loc", "Locality to Tune", choices = NULL),
                    conditionalPanel(condition = "input.comp_mode == true || ['pred', 'pred_ss', 'resid'].includes(input.value_type)",
                      radioButtons("m_target", "Target", choices = c("Actual" = "act", "Predicted" = "pre"), inline = TRUE)
                    ),
                    sliderInput("m_nugget", "Nugget", min = 0, max = 1, value = 0, step = 0.01),
                    sliderInput("m_psill", "Partial Sill", min = 0, max = 1, value = 1, step = 0.01),
                    sliderInput("m_range", "Range", min = 1, max = 1000, value = 100),
                    actionButton("apply_manual", "Apply Manual Model", class = "btn-warning btn-block")
                )
              )
            ),
            
            # Static IDW Controls
            conditionalPanel(condition = "input.method == 'IDW'",
                tuning_ui(
                    id = "idw", label = "IDW FACTORS",
                    global_slider_id = "idw_p", manual_slider_id = "idw_m_p",
                    global_slider_args = list(label = "Global IDW Power (p)", min = 0.5, max = 5, value = 2, step = 0.1),
                    manual_slider_args = list(label = "Power (p)", min = 0.5, max = 5, value = 2, step = 0.1),
                    optimize_btn_label = "OPTIMIZE IDW FACTORS",
                    manual_btn_label = "Apply Manual Power",
                    outer_style = "background-color: #e3fafc; padding: 10px; border: 1px solid #3bc9db; border-radius: 4px; margin-bottom: 10px;",
                    top_extra_ui = sliderInput("idw_nmax", HTML(paste0("Max Neighbors", info_tooltip("idw_nmax_info", "Limits the IDW calculation to the closest N points. This prevents distant, unrelated data from distorting local predictions. Select this BEFORE optimizing."))), min = 4, max = 50, value = 12),
                    extra_ui = div(style="background-color: #f8f9fa; border: 1px solid #e9ecef; border-radius: 4px; padding: 10px; color: #495057;", tableOutput("idw_metrics_table"))
                )
            ),
            
            # Static TPS Controls
            conditionalPanel(condition = "input.method == 'TPS'",
                tuning_ui(
                    id = "tps", label = "TPS LAMBDA",
                    global_slider_id = "tps_lambda", manual_slider_id = "tps_m_lambda",
                    global_slider_args = list(label = "Global Smoothing (Lambda)", min = 0, max = 1, value = 0, step = 0.001),
                    manual_slider_args = list(label = "Lambda", min = 0, max = 1, value = 0, step = 0.001),
                    optimize_btn_label = "OPTIMIZE TPS LAMBDA",
                    manual_btn_label = "Apply Manual Lambda",
                    outer_style = "background-color: #fff4e6; padding: 10px; border: 1px solid #ffd8a8; border-radius: 4px; margin-bottom: 10px;",
                    extra_ui = p(style="font-size: 0.8em; opacity: 0.8;", "Lambda = 0: Exact interpolation; Lambda > 0: Smoothing.")
                )
            ),
            
            selectInput("boundary_type", HTML(paste0("Boundary Type", info_tooltip("bound", "Defines how the interpolation surface is cropped. Convex hull wraps points tightly; Buffered adds padding."))), 
                        choices = c("Concave Hull" = "concave", 
                                    "Convex Hull" = "convex", 
                                    "Wrapped (Buffered)" = "wrapped",
                                    "Strict Measured (Point Buffer)" = "strict")),
            conditionalPanel(condition = "['wrapped', 'strict'].includes(input.boundary_type)",
              conditionalPanel(condition = "input.boundary_type == 'wrapped'",
                radioButtons("buff_mode", HTML(paste0("Buffer Logic", info_tooltip("buff_logic_info", "Dynamic mode calculates buffer distance per locality based on point density and selected method. Fixed allows manual setting."))),
                             choices = c("Auto (Dynamic)" = "dynamic", "Fixed (Manual)" = "fixed"), selected = "dynamic")
              ),
              conditionalPanel(condition = "input.boundary_type == 'strict' || (input.boundary_type == 'wrapped' && input.buff_mode == 'fixed')",
                numericInput("buff_dist", HTML(paste0("Buffer Distance (m)", info_tooltip("buff_dist_info", "Sets the spatial buffer distance. For Strict Point mode, this acts as the fixed radius around each point."))), value = 250, min = 0)
              )
            ),
            
            radioButtons("res_mode", HTML(paste0("Resolution Logic", info_tooltip("res", "Dynamic modes calculate cell size based on spatial extent. Manual forces a specific cell size (e.g. 10m)."))), 
                         choices = c("Auto (Per Locality)" = "local", "Auto (Global)" = "global", "Fixed" = "fixed")),
            conditionalPanel(condition = "input.res_mode == 'fixed'",
              sliderInput("grid_res", "Manual Resolution", min = 5, max = 500, value = 50)
            ),
            
            div(style="margin-top: 10px; background-color: rgba(255, 255, 255, 0.05); border: 1px solid rgba(255, 255, 255, 0.1); border-radius: 4px; padding: 10px; color: #f1f5f9;", 
                tableOutput("loc_res_table"),
                conditionalPanel(condition = "input.res_mode == 'fixed' && input.boundary_type == 'wrapped' && input.buff_mode == 'dynamic'",
                  p(style="font-size: 0.78em; margin-top: 8px; border-left: 3px solid #2196F3; padding-left: 8px; color: #cbd5e1; font-style: italic; line-height: 1.35;", 
                    "Note: Dynamic buffers scale with the physical sample density (spacing) to prevent spatial clipping, completely independent of your manual grid pixel size.")
                )
            ),
            
            hr(),
            h5("Uncertainty Mapping"),
            conditionalPanel(condition = "['OK', 'RK', 'RFK', 'CK'].includes(input.method)",
              checkboxInput("show_uncertainty", "Map Uncertainty Instead of Interpolation", FALSE),
              conditionalPanel(condition = "input.show_uncertainty",
                radioButtons("uncertainty_type", "Metric", choices = c("Variance" = "var", "Standard Error" = "se"), selected = "se", inline = TRUE)
              )
            ),
            conditionalPanel(condition = "!['OK', 'RK', 'RFK', 'CK'].includes(input.method)",
              p(style="font-size: 0.8em; opacity: 0.8;", "Uncertainty mapping requires a Kriging-based method.")
            )
        ),
        br(),
        selectInput("color_style", "Styling", choices = c("Continuous" = "cont", "Binned (5)" = "bin", "Agronomical" = "agro")),
        uiOutput("palette_ui"),
        conditionalPanel(condition = "input.color_style == 'agro'",
            selectInput("agro_method", "Algorithm", choices = c("Supervised" = "limits", "Jenks" = "jenks", "K-means" = "kmeans")),
            sliderInput("agro_n_classes", "Classes", min = 2, max = 5, value = 3),
            uiOutput("agro_options")),
        hr(),
        h4("3. Management - Save for Future Sessions"),
        div(style="display: flex; gap: 5px;",
            actionButton("save_config", "Save", class = "btn-warning", style="flex:1;"),
            shinyFilesButton("load_config", "Load", "Select Config", multiple = FALSE, class = "btn-info", style="flex:1;")
        ),
        br(),
        actionButton("run", "Run Interpolation", class = "btn-success btn-lg", style="width:100%;")
      ),
      
      # Friendly descriptive placeholder in sidebar when Analytics Suite is active
      conditionalPanel(
        condition = "input.main_tabs === '5. Descriptive and Exploratory Suite'",
        div(style="background-color: rgba(255, 255, 255, 0.08); padding: 12px; border: 1px solid rgba(255, 255, 255, 0.15); border-radius: 6px; margin-top: 10px;",
            h4("Exploratory Suite Active", style="margin-top: 0; color: #ffffff; font-weight: bold;"),
            p(style="font-size:0.85em; color:#cbd5e1; line-height:1.45; margin-bottom: 0;", 
              "Plot and analyze descriptive statistics, perform correlation analysis, and execute Principal Component Analysis (PCA) directly on your raw data. These tools operate independently of the spatial interpolation model configuration.")
        )
      )
    ),
    
    mainPanel(width = 9,
      tabsetPanel(id = "main_tabs",
        tabPanel("1. Data Setup", value = "tab_data",
                 div(style = "padding: 20px; background-color: #f1f3f5; border-radius: 8px; border: 1px solid #dee2e6;",
                     h3("Step 1: Upload Your Dataset"),
                     fluidRow(
                       column(6, fileInput("user_file", "Choose CSV or Excel File", accept = c(".csv", ".xlsx", ".xls"))),
                        column(6, 
                               fileInput("user_shp", "Shapefile - Optional (.shp, .shx, .dbf, .prj)", multiple = TRUE, accept = c(".shp", ".shx", ".dbf", ".prj")),
                               tags$p(style="margin-top: -22px; font-size: 10px; color: #777; font-style: italic; line-height: 1.3;", 
                                      tags$b("Tip:"), "You do not need to upload custom shapefiles! Standard boundary types (Convex, Concave, Strict, or Wrapped Hulls) can be selected and configured dynamically in the Sidebar panel once your dataset is loaded.")
                        )
                     ),
                     conditionalPanel(condition = "output.file_uploaded",
                       downloadButton("export_updated_data", "Export Updated Dataset", class = "btn-success", style = "margin-bottom: 15px;")
                     ),
                     hr(),
                     conditionalPanel(condition = "output.file_uploaded",
                        h3("Step 2: Spatial Mapping"),
                        fluidRow(
                          column(4, selectInput("map_x", "X Coordinate (Longitude/Easting)", choices = NULL)),
                          column(4, selectInput("map_y", "Y Coordinate (Latitude/Northing)", choices = NULL)),
                          column(4, selectInput("map_loc", "Locality/Grouping Column", choices = NULL))
                        ),
                        fluidRow(
                          column(4, selectizeInput("map_crs", "Input Data CRS", choices = common_crs, selected = "EPSG:32635", options = list(create = TRUE))),
                          column(4, selectizeInput("crs_selection", "Target Mapping CRS", choices = common_crs, selected = "EPSG:32635", options = list(create = TRUE))),
                          column(4, style="margin-top: 25px;", tags$p(tags$b("Instructions:"), "Please wait for the sampling coordinates to render and verify their accuracy on the mini-map below. Optionally, upload a variable list to enable automated data categorization."))
                        ),
                        hr(),
                        h3("Step 3: Mini-Map Validation"),
                        leafletOutput("setup_minimap", height = "400px"),
                        hr(),
                        h3("Step 4: Variable Mapping & Verification"),
                        tags$p(style="margin-top: -8px; font-size: 13px; color: #777; font-style: italic; margin-bottom: 12px;", 
                               "(Confirm mapped variables below after uploading)"),
                        tags$p("Pair your Target (Actual) variables with their Predictions. You can map them manually or upload a metadata file."),
                        fileInput("meta_file", "Upload Variable List (Optional)", accept = c(".xlsx", ".xls", ".csv")),
                        shinycssloaders::withSpinner(uiOutput("var_mapping_ui"), type = 6, color = "#2ecc71")
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
                                 checkboxInput("show_points_viewer", "Show Points", FALSE, width = "auto"),
                                 checkboxInput("show_res_overlay", "Show Res", FALSE, width = "auto"),
                                 selectInput("base_map_layer", NULL,
                                             choices = c("Satellite (Esri)" = "Esri.WorldImagery",
                                                         "Topographic" = "OpenTopoMap",
                                                         "Standard Street" = "OpenStreetMap",
                                                         "Dark Matter" = "CartoDB.DarkMatter",
                                                         "Light (Positron)" = "CartoDB.Positron"),
                                             selected = "Esri.WorldImagery", width = "160px", selectize = FALSE),
                                 uiOutput("locality_pan_ui")
                                 ),
                             actionButton("refresh_map_area", "Refresh Map Area", icon = icon("sync"), class = "btn-info btn-sm", style = "margin-left: auto;"),
                             actionButton("show_popup_settings", "Pop-up Settings", icon = icon("cog"), class = "btn-info btn-sm"),
                             actionButton("quick_export_map", "Quick Export", icon = icon("camera"), class = "btn-info btn-sm", title = "Immediately send the currently viewed map to the Export Registry."),
                             actionButton("toggle_pt_style", NULL, icon = icon("palette"), class = "btn-sm",
                                          style = "background-color: #6c5ce7; color: white; border: none;",
                                          title = "Point Styling Options"),
                             div(class="map-toolbar-export-container", style="display: flex; align-items: center; gap: 5px; border-left: 1px solid #ccc; padding-left: 10px;",
                                 selectInput("polygon_export_format", NULL, choices = c("Shapefile (ZIP)" = "shp", "GeoJSON" = "geojson", "KML" = "kml", "GPKG" = "gpkg"), selected = "shp", width = "120px", selectize = FALSE),
                                 downloadButton("polygon_download_btn", "Export Polygon", class = "btn-success btn-sm", style = "padding: 4px 10px; font-size: 12px; line-height: 1.5; border-radius: 3px;")
                             )
                         ),
                         # F2: Collapsible Point Styling Toolbar
                         shinyjs::hidden(
                           div(id = "pt_style_toolbar",
                             style = "margin-bottom:10px; padding: 12px 15px; background: linear-gradient(135deg, #2d3436 0%, #1e272e 100%); border-radius: 6px; border: 1px solid #636e72; color: #dfe6e9; display: flex; flex-wrap: wrap; align-items: flex-start; gap: 18px;",
                             # Column 1: Color-by controls
                             div(style = "min-width: 160px;",
                               tags$label("Color By", style = "font-size: 11px; color: #a0aec0; margin-bottom: 2px; display: block; text-transform: uppercase; letter-spacing: 0.5px;"),
                               selectInput("pt_color_by", NULL, choices = c("None (Cyan)" = "none"), selected = "none", width = "160px", selectize = FALSE),
                               selectInput("pt_palette", "Palette", choices = c("Set1", "Dark2", "Paired", "Set2", "Set3", "Accent", "Pastel1", "Tableau10"), selected = "Set1", width = "160px", selectize = FALSE),
                               actionButton("pt_custom_colors", "Custom Colors...", icon = icon("paint-brush"), class = "btn-xs btn-default",
                                            style = "margin-top: 4px; background-color: #4a5568; color: #e2e8f0; border-color: #2d3748;")
                             ),
                             # Column 2: Label controls
                             div(style = "min-width: 160px;",
                               tags$label("Labels", style = "font-size: 11px; color: #a0aec0; margin-bottom: 2px; display: block; text-transform: uppercase; letter-spacing: 0.5px;"),
                               checkboxInput("pt_show_labels", "Show Labels", FALSE, width = "auto"),
                               selectInput("pt_label_field", "Label Field", choices = c("(none)" = "none"), selected = "none", width = "160px", selectize = FALSE),
                               sliderInput("pt_label_size", "Label Size", min = 8, max = 18, value = 11, step = 1, width = "150px", ticks = FALSE)
                             ),
                             # Column 3: Point size & options
                             div(style = "min-width: 130px;",
                               tags$label("Point Options", style = "font-size: 11px; color: #a0aec0; margin-bottom: 2px; display: block; text-transform: uppercase; letter-spacing: 0.5px;"),
                               sliderInput("pt_marker_size", "Point Size", min = 1, max = 12, value = 3, step = 1, width = "130px", ticks = FALSE),
                               checkboxInput("pt_apply_minimap", "Apply Colour Set to Mini Map", FALSE, width = "auto")
                             )
                           )
                         ),
                         uiOutput("run_config_display_map"),
                         conditionalPanel(condition = "(!input.comp_mode && input.value_type != 'resid') || input.value_type == 'actual'",
                                          h4(textOutput("main_map_title")),
                                          leafletOutput("main_map", height = "700px")),
                         conditionalPanel(condition = "(input.comp_mode && input.value_type != 'actual') || input.value_type == 'resid'",
                                          fluidRow(column(6, h4(textOutput("comp_left_title")), leafletOutput("comp_map_left", height = "600px")),
                                                   column(6, h4(textOutput("comp_right_title")), leafletOutput("comp_map_right", height = "600px")))),
                         div(id = "distance_scale_container", style = "margin-top: 15px; display: flex; justify-content: center; min-height: 30px;"),
                         tags$script(HTML("
                           setInterval(function() {
                             var scales = $('.leaflet-control-scale');
                             if(scales.length > 0) {
                               scales.appendTo('#distance_scale_container');
                               scales.css({'background': 'white', 'padding': '5px', 'border': '1px solid #ccc', 'border-radius': '4px', 'margin': '0 auto'});
                             }
                           }, 500);
                         "))
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
                   uiOutput("locality_selector_ui"),
                   fluidRow(
                     column(8,
                            conditionalPanel(condition = "input.method == 'OK'",
                              h4("Actual Data Structure"), plotOutput("vgm_plot_main", height = "350px"),
                              div(id = "predicted_data_structure_ui",
                                h4("Predicted Data Structure"), plotOutput("vgm_plot_pred", height = "350px")
                              )
                            ),
                            conditionalPanel(condition = "input.method == 'RK'",
                               h4("Linear Trend Performance (Actual)"), uiOutput("model_summary_ui_act"),
                               div(id = "rk_pred_ui", h4("Linear Trend Performance (Predicted)"), uiOutput("model_summary_ui_pre")),
                               hr(),
                               h4("Internal Residual Variogram (Actual)"), plotOutput("rk_internal_vgm_act", height = "350px"),
                               div(id = "rk_internal_vgm_pre_ui", h4("Internal Residual Variogram (Predicted)"), plotOutput("rk_internal_vgm_pre", height = "350px"))
                             ),
                            conditionalPanel(condition = "input.method == 'RFK'",
                               h4("RF Variable Importance (Actual)"), plotOutput("rf_importance_plot_act", height = "350px"),
                               div(id = "rfk_pred_ui", h4("RF Variable Importance (Predicted)"), plotOutput("rf_importance_plot_pre", height = "350px")),
                               hr(),
                               h4("Internal Residual Variogram (Actual)"), plotOutput("rfk_internal_vgm_act", height = "350px"),
                               div(id = "rfk_internal_vgm_pre_ui", h4("Internal Residual Variogram (Predicted)"), plotOutput("rfk_internal_vgm_pre", height = "350px"))
                             ),
                            conditionalPanel(condition = "input.method == 'CK'",
                               h4("Cross-Variogram (Actual)"), plotOutput("ck_variogram_plot_act", height = "350px"),
                               div(id = "ck_pred_ui", h4("Cross-Variogram (Predicted)"), plotOutput("ck_variogram_plot_pred", height = "350px"))
                             ),
                            conditionalPanel(condition = "input.method == 'TPS'",
                               h4("TPS GCV Diagnostics (Actual)"), plotOutput("tps_gcv_plot_act", height = "350px"),
                               div(id = "tps_pred_ui", h4("TPS GCV Diagnostics (Predicted)"), plotOutput("tps_gcv_plot_pre", height = "350px"))
                             ),
                            conditionalPanel(condition = "!['OK', 'RK', 'RFK', 'CK', 'TPS'].includes(input.method)",
                              div(style="padding: 20px; text-align: center; color: #666;",
                                  h4("Diagnostic Mode Active"),
                                  p("Detailed spatial diagnostics are currently optimized for Kriging and TPS."))
                            ),
                            div(id = "validation_diagnostics_act_ui",
                               hr(),
                               h4("Validation Diagnostics (Actual)"),
                               fluidRow(
                                 column(6, plotOutput("obs_pred_plot_act", height = "300px")),
                                 column(6, plotOutput("resid_vgm_plot_act", height = "300px"))
                               )
                            ),
                            conditionalPanel(condition = "input.comp_mode || input.value_type != 'actual'",
                              div(id = "validation_diagnostics_pre_ui",
                                hr(),
                                h4("Validation Diagnostics (Predicted)"),
                                fluidRow(
                                  column(6, plotOutput("obs_pred_plot_pre", height = "300px")),
                                  column(6, plotOutput("resid_vgm_plot_pre", height = "300px"))
                                )
                              )
                            )
                     ),
                     column(4,
                            div(style = "background-color: #fff9db; padding: 15px; border: 2px solid #fab005; border-radius: 8px; margin-bottom: 20px;",
                              h4("Spatial Interpolation Statistics"),
                              tags$p(style="font-size: 0.85em; opacity: 0.8; font-style: italic;", "Model-specific diagnostics and performance metrics (RMSE, R )."),
                              conditionalPanel(condition = "input.method == 'OK'",
                                h5("Variogram Parameters"), div(class="table-container", tableOutput("vgm_params_table")),
                                hr(style="opacity: 0.3;")
                              ),
                              conditionalPanel(condition = "['IDW', 'TPS'].includes(input.method)",
                                h5("Regional Parameters"), div(class="table-container", tableOutput("regional_params_table")),
                                hr(style="opacity: 0.3;")
                              ),
                              h5("Model Performance"), div(class="table-container", tableOutput("metrics_table"))
                            ),
                            div(id = "prediction_performance_ui",
                              style = "background-color: #f3e8ff; padding: 15px; border: 2px solid #9b59b6; border-radius: 8px; margin-bottom: 20px;",
                              h4("Variable Prediction Statistics"),
                              tags$p(style="font-size: 0.85em; opacity: 0.8; font-style: italic;", "Prediction accuracy and classification agreement metrics for uploaded data."),
                              h5("Prediction Performance (Uploaded Data)"),
                              div(class="table-container", tableOutput("uploaded_metrics_table")),
                              hr(style="opacity: 0.3;"),
                              h5("Classification Performance (Uploaded Predictions)"),
                              selectInput("kappa_bin_method", "Binning Method:", choices = c("Agronomical Classes" = "agro", "Quartiles" = "quartile")),
                              div(class="table-container", tableOutput("kappa_table"))
                            ),
                            div(style = "background-color: #e7f5ff; padding: 15px; border: 2px solid #339af0; border-radius: 8px;",
                              h4("Data Summary Statistics"),
                              tags$p(style="font-size: 0.85em; opacity: 0.8; font-style: italic;", "Aggregated descriptive statistics and area coverage for the data."),
                              h5("Area Coverage"),
                              conditionalPanel(condition = "input.locality && (typeof input.locality === 'string' ? input.locality === 'ALL' : (input.locality.length > 1 || input.locality.indexOf('ALL') > -1))",
                                fluidRow(
                                  column(6, h6("Total - Actual"), tableOutput("area_table_total_act")),
                                  column(6, div(id = "area_total_pred_col", h6("Total - Predicted"), tableOutput("area_table_total_pre")))
                                )
                              ),
                              fluidRow(
                                column(6, h6("Locality - Actual"), tableOutput("area_table_loc_act")),
                                column(6, div(id = "loc_pred_col", h6("Locality - Predicted"), tableOutput("area_table_loc_pre")))
                              ),
                              hr(style="border-top: 1px solid #339af0;"),
                              h5("Descriptive Statistics"),
                              conditionalPanel(condition = "input.locality && (typeof input.locality === 'string' ? input.locality === 'ALL' : (input.locality.length > 1 || input.locality.indexOf('ALL') > -1))",
                                tableOutput("stats_table_total")
                              ),
                              tableOutput("stats_table_loc")
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
        tabPanel("5. Descriptive and Exploratory Suite",
                 desc_exploratory_ui("exploratory")
        )      )
    )
  )
)

# --- Server ---
server <- function(input, output, session) {

  # Generate a unique session ID and a session isolated progress folder in tempdir
  session_id <- paste0("session_", shiny:::createUniqueId(8))
  session_progress_dir <- file.path(tempdir(), "monolith_progress", session_id)
  
  # Ensure isolated session folder is clean
  dir.create(session_progress_dir, recursive = TRUE, showWarnings = FALSE)

  # Cache environments for optimized rendering and calculation
  leaflet_proj_cache <- new.env(parent = emptyenv())
  area_calc_cache <- new.env(parent = emptyenv())

  # --- Dynamic Residual Plot Factory ---
  render_resid_plot <- function(cv_data_reactive, title_suffix = "") {
    renderPlot({
      req(input$sel_loc_stats, cv_data_reactive())
      loc <- input$sel_loc_stats
      df_list <- cv_data_reactive()
      
      if(loc == "Total (Combined)") {
         sf_list <- lapply(df_list, function(x) {
           if(inherits(x, "sf")) return(x)
           if(is.data.frame(x) && "x" %in% colnames(x) && "y" %in% colnames(x)) return(st_as_sf(x, coords = c("x", "y"), crs = rv$mapping$crs))
           return(NULL)
         })
         sf_list <- sf_list[!sapply(sf_list, is.null)]
         req(length(sf_list) > 0)
         cv_obj <- tryCatch(do.call(rbind, sf_list), error = function(e) sf_list[[1]])
      } else {
         cv_obj <- df_list[[loc]]
      }
      
      req(cv_obj)
      
      if(!inherits(cv_obj, "sf") && !inherits(cv_obj, "Spatial")) {
         if("x" %in% colnames(cv_obj) && "y" %in% colnames(cv_obj)) {
            cv_obj <- st_as_sf(cv_obj, coords = c("x", "y"), crs = rv$mapping$crs)
         } else {
            return(NULL) 
         }
      }
      
      if(!("residual" %in% names(cv_obj))) {
         cols <- detect_cv_columns(names(cv_obj))
         obs_col <- cols$observed
         pre_col <- cols$pred
         
         req(obs_col, pre_col)
         cv_obj$residual <- cv_obj[[obs_col]] - cv_obj[[pre_col]]
      }
      
      tryCatch({
         lags <- calc_scientific_lags(cv_obj)
         v_res <- variogram(residual ~ 1, cv_obj, width = lags$width, cutoff = lags$cutoff)
         v_fit <- robust_vgm_fit(v_res, cv_obj$residual)
         v_sub <- if (!is.null(v_fit)) {
           model_name <- as.character(v_fit$model[2])
           nugget <- round(v_fit$psill[1], 4)
           psill <- if(nrow(v_fit) > 1) round(v_fit$psill[2], 4) else 0
           v_range <- if(nrow(v_fit) > 1) round(v_fit$range[2], 2) else 0
           paste0("Fitted: ", model_name, " (Nugget: ", nugget, ", Partial Sill: ", psill, ", Range: ", v_range, ")")
         } else {
           "Target: Pure Nugget (No structure)"
         }
         p_res <- plot(v_res, model = v_fit, main = paste("Residual Variogram:", loc, title_suffix), sub = v_sub)
         print(p_res)
      }, error = function(e) {
         plot(1, 1, type="n", main=paste("Error:", e$message), axes=F)
      })
    })
  }

  # --- Per-Locality Asset Export Registration Helper ---
  register_locality_assets <- function(l, meta, comp_mode, val_type) {
     if(!is.null(rv$sf)) {
       # 1. Descriptive Stats (Actual)
       df_l_act <- rv$sf %>% st_drop_geometry() %>% filter(loc == !!l, !is.na(v))
       if(nrow(df_l_act) > 0) {
         s_l <- summary(df_l_act$v)
         stats_l <- data.frame(Metric = names(s_l), Value = as.character(round(as.numeric(s_l), 3)))
         register_export_item(paste0("table_stats_loc_", l), paste(meta$label, "-", l, "- Descriptive Statistics (Actual)"), "table", stats_l, meta$category)
       }
       
       # 1.5 Descriptive Stats (Predicted)
       if(comp_mode || val_type != "actual") {
         df_l_pre <- rv$sf %>% st_drop_geometry() %>% filter(loc == !!l, !is.na(pv))
         if(nrow(df_l_pre) > 0) {
           s_l_pre <- summary(df_l_pre$pv)
           stats_l_pre <- data.frame(Metric = names(s_l_pre), Value = as.character(round(as.numeric(s_l_pre), 3)))
           register_export_item(paste0("table_stats_pre_loc_", l), paste(meta$label, "-", l, "- Descriptive Statistics (Predicted)"), "table", stats_l_pre, meta$category)
         }
       }

       # 2. Prediction Performance
       if(comp_mode || val_type != "actual") {
         df_l_perf <- rv$sf %>% st_drop_geometry() %>% filter(loc == !!l, !is.na(v), !is.na(pv))
         if(nrow(df_l_perf) >= 3) {
           perf_l <- data.frame(
             Metric = c("R2 (Trad)", "R2 (Corr)", "RMSE", "MBE (Bias)", "CCC", "RPD"),
             Value = c(
               round(yardstick::rsq_trad_vec(df_l_perf$v, df_l_perf$pv), 4),
               round(yardstick::rsq_vec(df_l_perf$v, df_l_perf$pv), 4),
               round(yardstick::rmse_vec(df_l_perf$v, df_l_perf$pv), 4),
               round(mean(df_l_perf$pv - df_l_perf$v, na.rm=TRUE), 4),
               round(yardstick::ccc_vec(df_l_perf$v, df_l_perf$pv), 4),
               round(yardstick::rpd_vec(df_l_perf$v, df_l_perf$pv), 4)
             )
           )
           register_export_item(paste0("table_perf_loc_", l), paste(meta$label, "-", l, "- Prediction Performance"), "table", perf_l, meta$category)
         }
       }
     }
     
     # 3. Interpolation CV Metrics (Actual)
     if(!is.null(rv$cv_metrics_act[[l]])) {
       cv_l <- rv$cv_metrics_act[[l]]
       cv_table <- data.frame(Metric = names(cv_l), Value = as.character(round(as.numeric(cv_l), 4)))
       register_export_item(paste0("table_cv_loc_", l), paste(meta$label, "-", l, "- Model CV Metrics (Actual)"), "table", cv_table, meta$category)
     }
     
     # 3.5 Interpolation CV Metrics (Predicted)
     if((comp_mode || val_type != "actual") && !is.null(rv$cv_metrics_pre[[l]])) {
       cv_l_p <- rv$cv_metrics_pre[[l]]
       cv_table_p <- data.frame(Metric = names(cv_l_p), Value = as.character(round(as.numeric(cv_l_p), 4)))
       register_export_item(paste0("table_cv_pre_loc_", l), paste(meta$label, "-", l, "- Model CV Metrics (Predicted)"), "table", cv_table_p, meta$category)
     }
     
     # 4. Area Coverage
     if(isTruthy(input$color_style == "agro") && !is.null(rv$rast_list_act[[l]])) {
       area_l <- calc_area_df(rv$rast_list_act[[l]], paste0("export_act_", l))
       if(is.data.frame(area_l)) register_export_item(paste0("table_area_loc_", l), paste(meta$label, "-", l, "- Area Coverage"), "table", area_l, meta$category)
     }
     
     # B. Plots per locality
     # 1. Variograms
     if(!is.null(rv$v_emp_list[[paste0(l, "_act")]])) {
       v_emp <- rv$v_emp_list[[paste0(l, "_act")]]
       v_fit <- rv$v_fit_list[[paste0(l, "_act")]]
       p_vgm <- plot(v_emp, v_fit, main = paste("Variogram (Actual):", l))
       register_export_item(paste0("plot_vgm_act_", l), paste(meta$label, "-", l, "- Variogram (Actual)"), "plot", p_vgm, meta$category)
       df_vgm <- as.data.frame(v_emp) %>% select(np, dist, gamma, dir.hor, dir.ver)
       register_export_item(paste0("table_vgm_act_", l), paste(meta$label, "-", l, "- Variogram Data (Actual)"), "table", df_vgm, meta$category)
     }
     if((comp_mode || val_type != "actual") && !is.null(rv$v_emp_list[[paste0(l, "_pre")]])) {
       v_emp_p <- rv$v_emp_list[[paste0(l, "_pre")]]
       v_fit_p <- rv$v_fit_list[[paste0(l, "_pre")]]
       p_vgm_p <- plot(v_emp_p, v_fit_p, main = paste("Variogram (Predicted):", l))
       register_export_item(paste0("plot_vgm_pre_", l), paste(meta$label, "-", l, "- Variogram (Predicted)"), "plot", p_vgm_p, meta$category)
       df_vgm_p <- as.data.frame(v_emp_p) %>% select(np, dist, gamma, dir.hor, dir.ver)
       register_export_item(paste0("table_vgm_pre_", l), paste(meta$label, "-", l, "- Variogram Data (Predicted)"), "table", df_vgm_p, meta$category)
     }
     
     # 2. Obs vs Pred
     if(!is.null(rv$cv_data_act[[l]])) {
       df_cv <- as.data.frame(rv$cv_data_act[[l]])
       p_op <- tryCatch({
         build_obs_pred_plot(df_cv, title = paste("Obs vs Pred (Actual):", l), x_lab = "Observed", y_lab = "Predicted")
       }, error = function(e) NULL)
       if(!is.null(p_op)) {
         register_export_item(paste0("plot_obs_pred_", l), paste(meta$label, "-", l, "- Obs vs Pred Scatter (Actual)"), "plot", p_op, meta$category)
       }
     }
     if((comp_mode || val_type != "actual") && !is.null(rv$cv_data_pre[[l]])) {
       df_cv_p <- as.data.frame(rv$cv_data_pre[[l]])
       p_op_p <- tryCatch({
         build_obs_pred_plot(df_cv_p, title = paste("Obs vs Pred (Predicted Map):", l), x_lab = "Observed", y_lab = "Predicted")
       }, error = function(e) NULL)
       if(!is.null(p_op_p)) {
         register_export_item(paste0("plot_obs_pred_pre_", l), paste(meta$label, "-", l, "- Obs vs Pred Scatter (Predicted Map)"), "plot", p_op_p, meta$category)
       }
     }
     
     # 3. TPS GCV Curves
     if(input$method == "TPS" && !is.null(rv$tps_gcv_data[[paste0(l, "_act")]])) {
       df_gcv <- rv$tps_gcv_data[[paste0(l, "_act")]]
       p_gcv <- ggplot(df_gcv, aes(x = lambda, y = gcv)) + 
         geom_line(color = "steelblue") + geom_point() + scale_x_log10() +
         labs(title = paste("TPS GCV Diagnostics (Actual):", l)) + theme_minimal()
       register_export_item(paste0("plot_tps_gcv_", l), paste(meta$label, "-", l, "- TPS GCV Curve (Actual)"), "plot", p_gcv, meta$category)
     }
     if(input$method == "TPS" && (comp_mode || val_type != "actual") && !is.null(rv$tps_gcv_data[[paste0(l, "_pre")]])) {
       df_gcv_p <- rv$tps_gcv_data[[paste0(l, "_pre")]]
       p_gcv_p <- ggplot(df_gcv_p, aes(x = lambda, y = gcv)) + 
         geom_line(color = "firebrick") + geom_point() + scale_x_log10() +
         labs(title = paste("TPS GCV Diagnostics (Predicted):", l)) + theme_minimal()
       register_export_item(paste0("plot_tps_gcv_pre_", l), paste(meta$label, "-", l, "- TPS GCV Curve (Predicted)"), "plot", p_gcv_p, meta$category)
     }
     
     # 4. RF Importance Plots & Tables
     if(input$method == "RFK" && !is.null(rv$rf_models[[paste0(l, "_act")]])) {
       rf_mod <- rv$rf_models[[paste0(l, "_act")]]
       imp_mat <- randomForest::importance(rf_mod)
       imp_col <- colnames(imp_mat)[1]
       df_imp <- data.frame(Variable = rownames(imp_mat), Importance = imp_mat[, imp_col])
       df_imp <- df_imp[order(df_imp$Importance, decreasing = TRUE), ]
       p_imp <- ggplot(df_imp, aes(x = reorder(Variable, Importance), y = Importance)) +
         geom_bar(stat = "identity", fill = "steelblue") + coord_flip() +
         labs(title = paste("Variable Importance (Actual):", l), x = "Variables", y = imp_col) + theme_minimal()
       register_export_item(paste0("plot_rf_imp_act_", l), paste(meta$label, "-", l, "- RF Variable Importance (Actual)"), "plot", p_imp, meta$category)
       register_export_item(paste0("table_rf_imp_act_", l), paste(meta$label, "-", l, "- RF Variable Importance Data (Actual)"), "table", df_imp, meta$category)
     }
     if(input$method == "RFK" && (comp_mode || val_type != "actual") && !is.null(rv$rf_models[[paste0(l, "_pre")]])) {
       rf_mod_p <- rv$rf_models[[paste0(l, "_pre")]]
       imp_mat_p <- randomForest::importance(rf_mod_p)
       imp_col_p <- colnames(imp_mat_p)[1]
       df_imp_p <- data.frame(Variable = rownames(imp_mat_p), Importance = imp_mat_p[, imp_col_p])
       df_imp_p <- df_imp_p[order(df_imp_p$Importance, decreasing = TRUE), ]
       p_imp_p <- ggplot(df_imp_p, aes(x = reorder(Variable, Importance), y = Importance)) +
         geom_bar(stat = "identity", fill = "firebrick") + coord_flip() +
         labs(title = paste("Variable Importance (Predicted):", l), x = "Variables", y = imp_col_p) + theme_minimal()
       register_export_item(paste0("plot_rf_imp_pre_", l), paste(meta$label, "-", l, "- RF Variable Importance (Predicted)"), "plot", p_imp_p, meta$category)
       register_export_item(paste0("table_rf_imp_pre_", l), paste(meta$label, "-", l, "- RF Variable Importance Data (Predicted)"), "table", df_imp_p, meta$category)
     }

     # 5. RK Coefficients Tables
     if(input$method == "RK" && !is.null(rv$model_summaries[[paste0(l, "_act")]])) {
       lm_sum <- rv$model_summaries[[paste0(l, "_act")]]
       coef_df <- as.data.frame(lm_sum$coefficients)
       coef_df$Variable <- rownames(coef_df)
       coef_df <- coef_df[, c("Variable", "Estimate", "Std. Error", "t value", "Pr(>|t|)" )]
       register_export_item(paste0("table_rk_coef_act_", l), paste(meta$label, "-", l, "- RK Regression Coefficients (Actual)"), "table", coef_df, meta$category)
     }
     if(input$method == "RK" && (comp_mode || val_type != "actual") && !is.null(rv$model_summaries[[paste0(l, "_pre")]])) {
       lm_sum_p <- rv$model_summaries[[paste0(l, "_pre")]]
       coef_df_p <- as.data.frame(lm_sum_p$coefficients)
       coef_df_p$Variable <- rownames(coef_df_p)
       coef_df_p <- coef_df_p[, c("Variable", "Estimate", "Std. Error", "t value", "Pr(>|t|)" )]
       register_export_item(paste0("table_rk_coef_pre_", l), paste(meta$label, "-", l, "- RK Regression Coefficients (Predicted)"), "table", coef_df_p, meta$category)
     }

     # 6. CK Cross-Variogram Plots
     if(input$method == "CK" && !is.null(rv$gstat_objs[[paste0(l, "_act")]])) {
       g <- rv$gstat_objs[[paste0(l, "_act")]]
       vm <- variogram(g)
       p_ck <- plot(vm, model = g$model, main = paste("Cross-Variogram (Actual):", l))
       register_export_item(paste0("plot_ck_vgm_act_", l), paste(meta$label, "-", l, "- CK Cross-Variogram (Actual)"), "plot", p_ck, meta$category)
     }
     if(input$method == "CK" && (comp_mode || val_type != "actual") && !is.null(rv$gstat_objs[[paste0(l, "_pre")]])) {
       g_p <- rv$gstat_objs[[paste0(l, "_pre")]]
       vm_p <- variogram(g_p)
       p_ck_p <- plot(vm_p, model = g_p$model, main = paste("Cross-Variogram (Predicted):", l))
       register_export_item(paste0("plot_ck_vgm_pred_", l), paste(meta$label, "-", l, "- CK Cross-Variogram (Predicted)"), "plot", p_ck_p, meta$category)
     }
     
     # 7. Model Parameters (IDW/TPS)
     if(input$method %in% c("IDW", "TPS")) {
       param_df <- data.frame(
         Param = if(input$method == "IDW") "Power (p)" else "Lambda",
         Actual = as.character(round(get_regional_param(input$method, l, "act"), 6)),
         Predicted = if(comp_mode || val_type != "actual") as.character(round(get_regional_param(input$method, l, "pre"), 6)) else "NA"
       )
       register_export_item(paste0("table_params_loc_", l), paste(meta$label, "-", l, "- Model Parameters"), "table", param_df, meta$category)
     }
  }

  # --- Force Comparison Mode for Residuals ---
  observeEvent(input$value_type, {
    if (isTruthy(input$value_type) && input$value_type == "resid") {
      updateCheckboxInput(session, "comp_mode", value = TRUE)
      shinyjs::disable("comp_mode")
    } else {
      shinyjs::enable("comp_mode")
    }
  })

  # --- UI Visibility based on Prediction State ---
  observe({
    # 1. Elements requiring Interpolation
    prediction_active <- isTRUE(input$comp_mode) || (isTruthy(input$value_type) && input$value_type != "actual")
    has_interp <- prediction_active || rv$has_predictions
    shinyjs::toggle(id = "predicted_data_structure_ui", condition = has_interp)
    shinyjs::toggle(id = "rk_pred_ui", condition = has_interp)
    shinyjs::toggle(id = "rk_internal_vgm_pre_ui", condition = has_interp)
    shinyjs::toggle(id = "rfk_pred_ui", condition = has_interp)
    shinyjs::toggle(id = "rfk_internal_vgm_pre_ui", condition = has_interp)
    shinyjs::toggle(id = "ck_pred_ui", condition = has_interp)
    shinyjs::toggle(id = "tps_pred_ui", condition = has_interp)
    shinyjs::toggle(id = "validation_diagnostics_act_ui", condition = length(rv$cv_data_act) > 0)
    shinyjs::toggle(id = "validation_diagnostics_pre_ui", condition = has_interp)
    shinyjs::toggle(id = "loc_pred_col", condition = has_interp)
    shinyjs::toggle(id = "area_total_pred_col", condition = has_interp)
    
    # 2. Elements requiring Uploaded Predictions
    has_upl_pred <- FALSE
    if(!is.null(rv$mapping$vars) && !is.null(input$var_id)) {
       meta <- Filter(function(x) x$actual == input$var_id, rv$mapping$vars)
       if(length(meta) > 0) {
         if(!is.null(meta[[1]]$pred) || !is.null(meta[[1]]$pred_ss)) has_upl_pred <- TRUE
       }
    }
    shinyjs::toggle(id = "prediction_performance_ui", condition = has_upl_pred)
  })

  # --- Descriptive & Exploratory Analytics Module ---
  desc_exploratory_server(
    id = "exploratory",
    data_reactive = reactive(rv$user_data),
    vars_metadata_reactive = reactive(rv$mapping$vars)
  )

     active_theme_name <- theme_switcher_server("theme_mod")  
  output$dynamic_theme <- renderUI({
    req(active_theme_name())
    theme_obj <- app_themes[[active_theme_name()]]$theme
    fresh::use_theme(theme_obj)
  })
  
  output$dynamic_manual_style <- renderUI({
    req(active_theme_name())
    style_content <- app_themes[[active_theme_name()]]$manual_style
    tags$style(HTML(style_content))
  })
  
  observeEvent(active_theme_name(), {
    req(active_theme_name())
    theme_data <- app_themes[[active_theme_name()]]
    new_tiles <- theme_data$map_tiles
    
    # Sync UI dropdown selection with active theme's map tiles
    if (!is.null(new_tiles) && new_tiles != "") {
      updateSelectInput(session, "base_map_layer", selected = new_tiles)
    }
  }, ignoreInit = FALSE)

  # Local state variables for map rendering tracking (non-reactive to avoid feedback cycles)
  session_state <- new.env(parent = emptyenv())
  session_state$main_map_rendered <- FALSE
  session_state$comp_maps_rendered <- FALSE
  session_state$minimap_rendered <- FALSE

  rv <- reactiveValues(
    user_data = NULL, # Uploaded data
    has_predictions = FALSE, # Tracks interpolation state
    export_registry = list(), # Registry of plots and tables for export
    drawn_polygons = list(), # Stores drawn polygons from Leaflet
    shp_bound = NULL, # Custom shapefile boundary
    mapping = list(
      x = NULL, y = NULL, loc = NULL, crs = "EPSG:32635",
      vars = list() # List of actual/pred pairs
    ),
    rast = NULL, rast_pred = NULL, rast_res = NULL, rast_point_res = NULL, sf = NULL, bound = NULL, 
    v_fit_list = list(), v_emp_list = list(), 
    rast_list_act = list(), rast_list_pre = list(), rast_list_res = list(), rast_list_point_res = list(),
    desc_vars_state = list(x = "", y = "", z = "", multi = character(0)),
    cv_metrics_act = list(), cv_metrics_pre = list(),
    cv_data_act = list(), cv_data_pre = list(),
    loc_resolutions = list(), # Track spatial resolutions per locality
    idw_factors = list(), tps_lambdas = list(), # Regional Parameters
    tps_gcv_data = list(), # GCV Diagnostic Data
    full_cor_matrix = NULL, # Correlation Matrix for all numeric variables
    show_corr_panel = FALSE, # Toggle for sidebar correlation panel
    pop_up_vars = NULL, # Selected variables for pop-ups
    run_method = list(), # Tracking method used per variable
    model_summaries = list(), # summaries for UK/RK
    rf_models = list(), # trained random forests
    gstat_objs = list(), # gstat objects for CK
    loc_names = NULL, metrics = NULL, log = "Ready.",
    drawn_feature = NULL, # Temporarily store drawn shape for grouping
    run_config_summary = NULL, # B3: Plain text summary of latest run configuration
    run_counter = 0L, # B4: Incremental run counter
    run_history = list(), # B4: Archive of previous run results and configs
    proceed_run = NULL, # B4: Trigger for model generation after archive decision
    pt_style_colors = NULL, # F2: Named vector group_value -> hex_color
    pt_style_palette = "Set1", # F2: Current qualitative palette name
    auto_archive_choice = "none", # "none", "archive", or "discard"
    model_running = FALSE # True when parallel model calculations are active
  )
  
  # --- Export Registry Core ---
  register_export_item <- function(id, label, type, obj, category = "General") {
    req(obj)
    # Ensure ID is unique and clean
    clean_id <- gsub("[^a-zA-Z0-9_]", "_", id)
    
    # Store item in registry
    # We use a reactive list to trigger UI updates
    new_item <- list(
      id = clean_id,
      label = label,
      type = type, # "plot", "table", "map"
      obj = obj,
      category = category,
      timestamp = Sys.time()
    )
    
    # Append or update
    current_reg <- isolate(rv$export_registry)
    current_reg[[clean_id]] <- new_item
    rv$export_registry <- current_reg
    
    rv$log <- paste0(rv$log, "\n[Registry] Registered ", type, ": ", label)
  }
  
  # --- Export Panel Server Logic ---
  output$export_registry_ui <- renderUI({
    req(rv$export_registry)
    reg <- rv$export_registry
    if (length(reg) == 0) return(tags$p("Registry is empty. Run models to populate."))
    
    # Create choices for checkboxGroupInput
    choices <- setNames(names(reg), sapply(reg, function(x) {
      sprintf("%s [%s] - %s", x$label, x$type, format(x$timestamp, "%H:%M:%S"))
    }))
    
    checkboxGroupInput("selected_assets", NULL, choices = choices, width = "100%")
  })

  # --- B3: Run Configuration Summary Display ---
  output$run_config_display <- renderUI({
    cfg <- rv$run_config_summary
    if (is.null(cfg)) return(NULL)
    div(style = "background-color: #e8f4fd; padding: 12px 15px; border-left: 4px solid #2196F3; border-radius: 4px; margin-bottom: 10px; font-family: monospace; font-size: 0.85em;",
      tags$strong(icon("info-circle"), paste0(" Run #", cfg$run_id, " Configuration (", format(cfg$timestamp, "%Y-%m-%d %H:%M:%S"), ")")),
      tags$br(),
      tags$span(paste0("Variable: ", cfg$variable, " | Method: ", cfg$method, " | Localities: ", cfg$localities)),
      tags$br(),
      tags$span(paste0("Subset: ", cfg$subset, " | View: ", cfg$value_type, " | CRS: ", cfg$crs)),
      tags$br(),
      tags$span(paste0("Boundary: ", cfg$boundary_type, " | Buffer: ", if (is.null(cfg$buffer_mode) || cfg$buffer_mode == "fixed") paste0(cfg$buffer_dist, "m") else "Dynamic", " | Resolution: ", cfg$resolution, " (", cfg$res_mode, ")")),
      if (!is.null(cfg$method_params) && nzchar(cfg$method_params)) tagList(tags$br(), tags$span(cfg$method_params))
    )
  })

  # B3: Compact config display for Map Viewer tab
  output$run_config_display_map <- renderUI({
    cfg <- rv$run_config_summary
    if (is.null(cfg)) return(NULL)
    div(style = "background-color: #e8f4fd; padding: 8px 12px; border-left: 4px solid #2196F3; border-radius: 4px; margin-bottom: 8px; font-size: 0.82em;",
      tags$strong(paste0("Run #", cfg$run_id, ": ")),
      tags$span(paste0(cfg$variable, " | ", cfg$method, " | ", cfg$localities, " | ", format(cfg$timestamp, "%H:%M:%S")))
    )
  })

  # --- B4: Run History Archive UI ---
  output$run_history_ui <- renderUI({
    hist <- rv$run_history
    if (length(hist) == 0) return(tags$p(style="color: #888;", "No archived runs yet. Previous runs will appear here when a new model generation begins."))

    run_panels <- lapply(seq_along(hist), function(i) {
      run <- hist[[i]]
      cfg <- run$config
      n_items <- length(run$registry)
      div(style = "background-color: #fff; padding: 12px; border: 1px solid #ddd; border-radius: 6px; margin-bottom: 8px;",
        fluidRow(
          column(8,
            tags$strong(paste0("Run #", cfg$run_id, " - ", cfg$variable, " (", cfg$method, ")")),
            tags$br(),
            tags$small(style="color: #666;", paste0(
              format(cfg$timestamp, "%Y-%m-%d %H:%M:%S"),
              " | ", cfg$localities,
              " | ", n_items, " registry items"
            ))
          ),
          column(4, style = "text-align: right;",
            actionButton(paste0("restore_run_", cfg$run_id), "Restore", class = "btn-sm btn-info", icon = icon("undo")),
            actionButton(paste0("delete_run_", cfg$run_id), "Remove", class = "btn-sm btn-danger", icon = icon("trash"))
          )
        )
      )
    })
    tagList(run_panels)
  })

  # B4: Dynamic observers for archive restore/delete buttons
  env_hist <- new.env(parent = emptyenv())
  env_hist$history_obs <- list()
  observe({
    hist <- rv$run_history
    lapply(env_hist$history_obs, function(o) {
      if (!is.null(o)) o$destroy()
    })
    env_hist$history_obs <- list()
    
    lapply(seq_along(hist), function(i) {
      run <- hist[[i]]
      run_id <- run$config$run_id
      
      # Restore observer
      obs_restore <- observeEvent(input[[paste0("restore_run_", run_id)]], {
        history_list <- isolate(rv$run_history)
        idx <- which(sapply(history_list, function(x) x$config$run_id) == run_id)[1]
        if (is.na(idx)) return()
        
        run_to_restore <- history_list[[idx]]
        
        # Archive current results first
        current_cfg <- isolate(rv$run_config_summary)
        current_reg <- isolate(rv$export_registry)
        if (!is.null(current_cfg) && length(current_reg) > 0) {
          rv$run_history <- c(
            list(list(config = current_cfg, registry = current_reg)),
            history_list[-idx]
          )
        } else {
          rv$run_history <- history_list[-idx]
        }
        
        # Restore the selected run
        rv$export_registry <- run_to_restore$registry
        rv$run_config_summary <- run_to_restore$config
        
        showNotification(paste0("Restored Run #", run_to_restore$config$run_id, " to active session."), type = "message")
      }, ignoreInit = TRUE)

      # Delete observer
      obs_delete <- observeEvent(input[[paste0("delete_run_", run_id)]], {
        history_list <- isolate(rv$run_history)
        idx <- which(sapply(history_list, function(x) x$config$run_id) == run_id)[1]
        if (!is.na(idx)) {
          rv$run_history <- history_list[-idx]
          showNotification("Archived run removed.", type = "warning")
        }
      }, ignoreInit = TRUE)
      
      env_hist$history_obs[[paste0("restore_", run_id)]] <- obs_restore
      env_hist$history_obs[[paste0("delete_", run_id)]] <- obs_delete
    })
  })

  # Track the most recently selected item for the Styler
  active_styler_item <- reactiveVal(NULL)
  
  observeEvent(input$selected_assets, {
    req(input$selected_assets)
    if (length(input$selected_assets) > 0) {
      active_styler_item(input$selected_assets[length(input$selected_assets)])
    }
  }, ignoreNULL = FALSE)
  
  observeEvent(active_styler_item(), {
    req(active_styler_item(), rv$export_registry)
    item <- rv$export_registry[[active_styler_item()]]
    req(item)
    if (item$type == "map_combined") {
      updateSelectInput(session, "styler_legend_pos", selected = "bottom")
      updateSelectInput(session, "styler_legend_dir", selected = "horizontal")
      updateSelectInput(session, "styler_legend_text_angle", selected = 90)
    } else {
      updateSelectInput(session, "styler_legend_pos", selected = "right")
      updateSelectInput(session, "styler_legend_dir", selected = "auto")
      updateSelectInput(session, "styler_legend_text_angle", selected = 0)
    }
  }, ignoreInit = TRUE)
  
  # Reactive for the styled plot preview
  # Cached base plot to avoid expensive geom_spatraster reconstruction on slider changes
  base_preview_plot <- reactive({
    req(active_styler_item(), rv$export_registry)
    item <- rv$export_registry[[active_styler_item()]]
    req(item)
    
    generate_base_plot(
      item = item,
      input = input,
      agro_params = tryCatch(agro_params(), condition = function(c) NULL)
    )
  })
  
  # Styled plot preview applying theme layers on top of cached base plot
  styled_preview_obj <- reactive({
    req(active_styler_item(), rv$export_registry)
    item <- rv$export_registry[[active_styler_item()]]
    req(item)
    
    base_p <- base_preview_plot()
    req(base_p)
    
    apply_styler_theme(
      p = base_p,
      input = input,
      calibration = 1,
      item_label = item$label,
      item_type = item$type
    )
  })
  
  # Debounce the preview to avoid flickering during slider movement
  styled_preview_obj_d <- styled_preview_obj %>% debounce(500)
  
  # Dynamic UI for Styler Preview to show aspect ratio changes
  output$styler_preview_dynamic_ui <- renderUI({
    req(input$styler_width, input$styler_height)
    w_px <- (if(isTruthy(input$styler_width)) input$styler_width else 10) * 96
    h_px <- (if(isTruthy(input$styler_height)) input$styler_height else 8) * 96
    
    # Scale to fit the 800x600 viewing area while maintaining aspect ratio
    scale <- min(1, 800 / w_px, 600 / h_px)
    w_disp <- w_px * scale
    h_disp <- h_px * scale
    
    div(style = sprintf("width: %fpx; height: %fpx; background-color: white; box-shadow: 0 4px 8px rgba(0,0,0,0.2);", w_disp, h_disp),
        plotOutput("styler_preview_plot", height = "100%", width = "100%")
    )
  })

  output$styler_preview_plot <- renderPlot({
    req(styled_preview_obj_d())
    styled_preview_obj_d()
  }, res = 96)
  
  observeEvent(input$select_all_assets, {
    req(rv$export_registry)
    updateCheckboxGroupInput(session, "selected_assets", selected = names(rv$export_registry))
  })
  
  observeEvent(input$deselect_all_assets, {
    updateCheckboxGroupInput(session, "selected_assets", selected = character(0))
  })
  
  observeEvent(input$clear_registry, {
    rv$export_registry <- list()
    showNotification("Export registry cleared.", type = "message")
  })
  
  # For now, just a notification for the other buttons until Phase 3/4
  observeEvent(input$quick_export_map, {
    meta <- get_current_meta(); req(meta)
    target <- if(input$value_type == "actual") rv$rast 
              else if(input$value_type == "resid") rv$rast_res
              else rv$rast_pred
    req(target)
    
    id <- paste0("quick_", input$value_type, "_", input$var_id)
    label <- paste("Quick Export:", meta$label, "(", input$value_type, ")")
    
    register_export_item(id, label, "map", target, meta$category)
    active_styler_item(id)
    
    # Trigger the Open Styler logic (we can't just call it, but we can trigger the modal)
    shinyjs::click("open_styler")
  })
  
  # Styler Configuration Persistence (Local Storage)
  observe({
    req(input$styler_title_size)
    cfg <- lapply(styler_fields, function(field) {
      input[[field$name]]
    })
    shinyjs::runjs(sprintf("localStorage.setItem('monolith_styler_config', JSON.stringify(%s));", jsonlite::toJSON(cfg, auto_unbox = TRUE)))
  })

  observeEvent(input$open_styler, {
    # Request config from localStorage
    shinyjs::runjs("
      var cfg = localStorage.getItem('monolith_styler_config');
      if(cfg) {
        Shiny.setInputValue('styler_local_config', JSON.parse(cfg));
      }
    ")
    
    showModal(modalDialog(
      title = "Monolith Export Styler",
      size = "l",
      easyClose = FALSE,
      fluidRow(
        column(4,
               tabsetPanel(
                 tabPanel("Basic",
                          wellPanel(
                            h4("1. Typography Overrides"),
                            textInput("styler_title", "Main Title", placeholder = "Auto-generated"),
                            fluidRow(
                              column(6, textInput("styler_x_title", "X-Axis Label", placeholder = "Default")),
                              column(6, textInput("styler_y_title", "Y-Axis Label", placeholder = "Default"))
                            ),
                            hr(),
                            h4("2. Output Quality"),
                            numericInput("styler_dpi", "Export DPI", value = 300, min = 72, max = 600),
                            selectInput("styler_format", "File Format", 
                                        choices = c("PNG" = "png", "TIFF" = "tiff", "PDF" = "pdf", "JPEG" = "jpg"))
                          )
                 ),
                 tabPanel("Advanced",
                          wellPanel(
                            h4("Font Sizes (pt)"),
                            fluidRow(
                              column(6, sliderInput("styler_title_size", "Main Title", min = 6, max = 40, value = 15)),
                              column(6, sliderInput("styler_base_size", "Base Text", min = 4, max = 30, value = 13))
                            ),
                            fluidRow(
                              column(6, sliderInput("styler_x_size", "X-Axis Label Size", min = 4, max = 30, value = 13)),
                              column(6, sliderInput("styler_y_size", "Y-Axis Label Size", min = 4, max = 30, value = 13))
                            ),
                            fluidRow(
                              column(6, sliderInput("styler_label_size", "Axis Text", min = 4, max = 30, value = 15)),
                              column(6, sliderInput("styler_legend_size", "Legend Text", min = 4, max = 30, value = 15))
                            ),
                            fluidRow(
                              column(6, sliderInput("styler_legend_key_size", "Legend Element Size", min = 0.5, max = 5.0, value = 0.5, step = 0.1)),
                              column(6, selectInput("styler_font_family", "Font Family", 
                                          choices = c("sans", "serif", "mono", "Roboto", "Open Sans", "Lato", "Montserrat")))
                            ),
                            selectInput("styler_label_orient", "X-Label Orientation", 
                                        choices = c("Vertical" = 90, "Horizontal" = 0, "Angled (45)" = 45)),
                            hr(),
                            h4("Layout & Spacing"),
                            selectInput("styler_legend_pos", "Legend Position", 
                                        choices = c("Right" = "right", "Bottom" = "bottom", "Left" = "left", "Top" = "top", "None" = "none")),
                            selectInput("styler_legend_dir", "Legend Orientation", 
                                        choices = c("Automatic" = "auto", "Horizontal" = "horizontal", "Vertical" = "vertical")),
                            selectInput("styler_legend_text_angle", "Legend Text Orientation", 
                                        choices = c("Horizontal" = 0, "Vertical" = 90, "Angled (45)" = 45)),
                            fluidRow(
                              column(3, numericInput("styler_margin_t", "Top", value = 10)),
                              column(3, numericInput("styler_margin_r", "Right", value = 10)),
                              column(3, numericInput("styler_margin_b", "Bottom", value = 10)),
                              column(3, numericInput("styler_margin_l", "Left", value = 10))
                            ),
                            checkboxInput("styler_show_grid", "Show Coordinate Grid", TRUE),
                            hr(),
                            h4("Publication Modifiers"),
                            checkboxInput("styler_high_contrast", "High Contrast Palette (Colorblind Safe)", FALSE),
                            fluidRow(
                              column(6, numericInput("styler_width", "Export Width (in)", value = 10, min = 1, max = 50, step = 0.5)),
                              column(6, numericInput("styler_height", "Export Height (in)", value = 8, min = 1, max = 50, step = 0.5))
                            ),
                            numericInput("styler_aspect_ratio", "Custom Aspect Ratio (Width/Height)", value = 1.25, step = 0.1)
                          )
                 )
               )
        ),
        column(8,
               div(style = "background-color: #f0f0f0; border: 1px solid #ccc; height: 600px; display: flex; justify-content: center; align-items: center; overflow: auto;",
                   uiOutput("styler_preview_dynamic_ui")
               ),
               tags$p(style="font-size: 0.85em; color: #666; margin-top: 5px;",
                      "Preview aspect ratio and dimensions are now live. Final export uses 2.5x typographical density enhancement.")
        )
      ),
      footer = tagList(
        div(style = "float: left; display: flex; gap: 10px;",
            downloadButton("styler_download_config", "Save Config", class = "btn-secondary btn-sm"),
            fileInput("styler_upload_config", NULL, buttonLabel = "Load Config", accept = c(".json"), multiple = FALSE, placeholder = "No file")
        ),
        modalButton("Cancel"),
        downloadButton("confirm_export", "Finalize & Download", class = "btn-success", icon = icon("check"))
      )
    ))
  })
  
  observeEvent(input$styler_local_config, {
    cfg <- input$styler_local_config
    sync_styler_config(cfg, session)
  })

  # Sync styler width, height and aspect ratio
  observeEvent(input$styler_width, {
    req(input$styler_width, input$styler_height)
    new_ratio <- round(input$styler_width / input$styler_height, 2)
    if (abs(new_ratio - (input$styler_aspect_ratio %||% 0)) > 0.01) {
      updateNumericInput(session, "styler_aspect_ratio", value = new_ratio)
    }
  }, ignoreInit = TRUE)

  observeEvent(input$styler_height, {
    req(input$styler_width, input$styler_height)
    new_ratio <- round(input$styler_width / input$styler_height, 2)
    if (abs(new_ratio - (input$styler_aspect_ratio %||% 0)) > 0.01) {
      updateNumericInput(session, "styler_aspect_ratio", value = new_ratio)
    }
  }, ignoreInit = TRUE)

  observeEvent(input$styler_aspect_ratio, {
    req(input$styler_aspect_ratio, input$styler_width)
    # Update height based on aspect ratio
    new_height <- round(input$styler_width / input$styler_aspect_ratio, 2)
    if (abs(new_height - (input$styler_height %||% 0)) > 0.01) {
      updateNumericInput(session, "styler_height", value = new_height)
    }
  }, ignoreInit = TRUE)
  
  output$styler_download_config <- downloadHandler(
    filename = function() { paste0("styler_config_", format(Sys.time(), "%Y%m%d"), ".json") },
    content = function(file) {
      cfg <- lapply(styler_fields, function(field) {
        input[[field$name]]
      })
      names(cfg) <- sapply(styler_fields, function(f) f$name)
      write(jsonlite::toJSON(cfg, auto_unbox = TRUE), file)
    }
  )

  observeEvent(input$styler_upload_config, {
    req(input$styler_upload_config)
    tryCatch({
      cfg <- jsonlite::fromJSON(input$styler_upload_config$datapath)
      
      sync_styler_config(cfg, session)
      
      showNotification("Styler configuration loaded successfully.", type = "message")
    }, error = function(e) {
      showNotification("Failed to load configuration. Invalid JSON.", type = "error")
    })
  })
  
  output$confirm_export <- downloadHandler(
    filename = function() {
      req(active_styler_item())
      item <- rv$export_registry[[active_styler_item()]]
      timestamp <- format(Sys.time(), "%Y%m%d_%H%M%S")
      ext <- if(item$type %in% c("plot", "map", "map_combined")) (input$styler_format %||% "png") else "xlsx"
      sprintf("Export_%s_%s.%s", item$id, timestamp, ext)
    },
    content = function(file) {
      req(active_styler_item())
      item <- rv$export_registry[[active_styler_item()]]
      ext <- if(item$type %in% c("plot", "map", "map_combined")) (input$styler_format %||% "png") else "xlsx"
      
      withProgress(message = paste("Exporting", item$type, "..."), {
        tryCatch({
          if (item$type %in% c("plot", "map", "map_combined")) {
            p_obj <- generate_styled_plot(
              item, input, 
              calibration = 2.5, 
              agro_params = tryCatch(agro_params(), condition = function(c) NULL)
            )

            export_plot_to_file(p_obj, file, ext, input)
          } else if (item$type == "table") {
            if (ext == "xlsx") {
              wb <- createWorkbook()
              addWorksheet(wb, "Data")
              writeData(wb, "Data", item$obj)
              saveWorkbook(wb, file, overwrite = TRUE)
            } else {
              write.csv(item$obj, file, row.names = FALSE)
            }
          }
          
          removeModal()
        }, error = function(e) {
          showNotification(paste("Export Failed:", e$message), type = "error")
        })
      })
    }
  )
  
  output$batch_export <- downloadHandler(
    filename = function() { paste0("Batch_Export_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".zip") },
    contentType = "application/zip",
    content = function(file) {
      req(input$selected_assets, length(input$selected_assets) > 0)
      
      # Create a temporary directory for zipping
      temp_dir <- file.path(tempdir(), paste0("export_", as.integer(Sys.time())))
      dir.create(temp_dir, showWarnings = FALSE)
      
      withProgress(message = "Batch Exporting...", value = 0, {
        selected_ids <- input$selected_assets
        items <- rv$export_registry[selected_ids]
        
        # Group items by type
        table_items <- Filter(function(x) x$type == "table", items)
        plot_items <- Filter(function(x) x$type %in% c("plot", "map", "map_combined"), items)
        
        n_plots <- length(plot_items)
        has_tables <- length(table_items) > 0
        total_steps <- n_plots + (if(has_tables) 1 else 0)
        current_step <- 0
        
        files_to_zip <- c()
        
        # 1. Handle Unified Excel Export for all selected tables
        if(has_tables) {
          current_step <- current_step + 1
          incProgress(1/total_steps, detail = "Compiling statistical tables into Excel...")
          
          timestamp <- format(Sys.time(), "%Y%m%d_%H%M%S")
          excel_name <- sprintf("Batch_Statistics_%s.xlsx", timestamp)
          excel_path <- file.path(temp_dir, excel_name)
          
          wb <- createWorkbook()
          used_sheet_names <- c()
          for(item in table_items) {
            # Build a clean sheet name from the label, max 31 chars for Excel
            clean_label <- gsub("[^a-zA-Z0-9 ]", "_", item$label)
            sheet_name <- substr(clean_label, 1, 31)
            
            # Deduplicate: when actual & predicted labels truncate identically
            if(sheet_name %in% used_sheet_names) {
              # Determine a meaningful suffix from the registry ID
              suffix <- if(grepl("_pre_|_pre$", item$id)) "_Pre" else "_2"
              counter <- 2
              candidate <- paste0(substr(sheet_name, 1, 31 - nchar(suffix)), suffix)
              while(candidate %in% used_sheet_names) {
                counter <- counter + 1
                suffix <- paste0("_", counter)
                candidate <- paste0(substr(sheet_name, 1, 31 - nchar(suffix)), suffix)
              }
              sheet_name <- candidate
            }
            used_sheet_names <- c(used_sheet_names, sheet_name)
            addWorksheet(wb, sheet_name)
            writeData(wb, sheet_name, item$obj)
          }
          saveWorkbook(wb, excel_path, overwrite = TRUE)
          files_to_zip <- c(files_to_zip, excel_name)
        }
        
        # 2. Handle Individual Plot Exports
        for (i in seq_along(plot_items)) {
          current_step <- current_step + i
          item <- plot_items[[i]]
          incProgress(1/total_steps, detail = paste("Exporting Plot:", item$label))
          
          timestamp <- format(Sys.time(), "%Y%m%d_%H%M%S")
          # FIX: Recognition of map_combined and plot types
          ext <- if(item$type %in% c("plot", "map", "map_combined")) (input$styler_format %||% "png") else "png"
          if(ext == "csv") ext <- "png" # Extra safety
          
          filename <- sprintf("Batch_%s_%s.%s", item$id, timestamp, ext)
          filepath <- file.path(temp_dir, filename)
          
          tryCatch({
            # Use the unified styling engine with batch calibration (2.5x)
            p <- generate_styled_plot(
              item, input, 
              calibration = 2.5, 
              agro_params = tryCatch(agro_params(), condition = function(c) NULL)
            )
            
            export_plot_to_file(p, filepath, ext, input)
            files_to_zip <- c(files_to_zip, filename)
          }, error = function(e) {
            rv$log <- paste0(rv$log, "\n[Batch] Failed to export ", item$label, ": ", e$message)
          })
        }
      })
      
      # 3. Create ZIP archive
      zip_path <- file.path(temp_dir, "export.zip")
      zip::zip(zipfile = zip_path, files = files_to_zip, root = temp_dir)
      
      # 4. Push to user via shiny download
      file.copy(zip_path, file)
      
      # Cleanup
      unlink(temp_dir, recursive = TRUE)
    }
  )
  
  # --- Map Selection & Grouping ---
  # Remove the debug observer, add handlers for all maps
  
  handle_new_feature <- function(feature) {
    rv$drawn_feature <- feature
    showModal(modalDialog(
      title = "Assign Locality / Analysis Group",
      textInput("new_group_name", "Group Name:", placeholder = "Enter name (e.g. Zone A)"),
      footer = tagList(
        modalButton("Cancel"),
        actionButton("save_group", "Save Group", class = "btn-primary")
      ),
      easyClose = FALSE
    ))
  }

  observeEvent(input$main_map_draw_new_feature, { handle_new_feature(input$main_map_draw_new_feature) })
  observeEvent(input$comp_map_left_draw_new_feature, { handle_new_feature(input$comp_map_left_draw_new_feature) })
  observeEvent(input$comp_map_right_draw_new_feature, { handle_new_feature(input$comp_map_right_draw_new_feature) })

  observeEvent(input$save_group, {
    req(rv$drawn_feature, input$new_group_name, rv$user_data, rv$mapping$x, rv$mapping$y)
    group_name <- trimws(input$new_group_name)
    if (group_name == "") {
      showNotification("Group name cannot be empty.", type = "error")
      return()
    }

    feature <- rv$drawn_feature
    feat_json <- jsonlite::toJSON(feature, auto_unbox = TRUE)
    poly_sf <- sf::st_read(feat_json, quiet = TRUE)
    sf::st_crs(poly_sf) <- 4326

    # Create sf object from all user_data
    df_map <- rv$user_data %>% 
      filter(!is.na(!!sym(rv$mapping$x)) & !is.na(!!sym(rv$mapping$y)))
    
    pts_sf <- sf::st_as_sf(df_map, coords = c(rv$mapping$x, rv$mapping$y), crs = rv$mapping$crs)
    poly_sf_trans <- sf::st_transform(poly_sf, sf::st_crs(pts_sf))
    
    intersect_idx <- sf::st_intersects(pts_sf, poly_sf_trans, sparse = FALSE)
    
    # st_intersects returns a matrix (points x polygons). We want rowSums > 0
    in_poly <- rowSums(intersect_idx) > 0
    
    if (any(in_poly)) {
      if (!"Assigned_Locality" %in% names(rv$user_data)) {
        rv$user_data$Assigned_Locality <- NA
      }
      
      valid_rows <- which(!is.na(rv$user_data[[rv$mapping$x]]) & !is.na(rv$user_data[[rv$mapping$y]]))
      intersecting_user_rows <- valid_rows[in_poly]
      
      rv$user_data$Assigned_Locality[intersecting_user_rows] <- group_name
      
      showNotification(paste("Assigned", length(intersecting_user_rows), "points to group:", group_name), type = "message")
      
      # Update map_loc choices to include the new column
      current_loc <- input$map_loc
      updateSelectInput(session, "map_loc", choices = colnames(rv$user_data), selected = current_loc)
      
      # Force trigger update on dropdowns if Assigned_Locality is the current loc mapping
      rv$user_data <- rv$user_data 
    } else {
      showNotification("No points found within the selected area.", type = "warning")
    }
    
    removeModal()
    rv$drawn_feature <- NULL
  })
  
  # --- Regional Parameter Helpers ---
  get_regional_param <- function(type, loc, target, default = 2.0) {
    field <- if(type == "IDW") "idw_factors" else "tps_lambdas"
    val <- rv[[field]][[loc]][[target]]
    if(is.null(val)) default else val
  }
  
  set_regional_param <- function(type, loc, target, value) {
    field <- if(type == "IDW") "idw_factors" else "tps_lambdas"
    if(is.null(rv[[field]][[loc]])) {
      params <- list()
      params[[target]] <- value
      rv[[field]][[loc]] <- params
    } else {
      rv[[field]][[loc]][[target]] <- value
    }
  }

  # --- Pop-up Engine ---
  popup_metadata_cache <- reactive({
    req(rv$mapping$vars)
    meta_list <- rv$mapping$vars
    
    # 1. Determine vars_to_show
    vars_to_show <- rv$pop_up_vars
    if(is.null(vars_to_show) || length(vars_to_show) == 0) {
      soil_vars <- Filter(function(x) grepl("Soil|Physicochem", x$category, ignore.case = TRUE), meta_list)
      if(length(soil_vars) > 0) {
        vars_to_show <- sapply(soil_vars, function(x) x$actual)
      } else {
        vars_to_show <- sapply(meta_list, function(x) x$actual)
      }
    }
    
    # 2. Group vars by category
    all_cats <- unique(sapply(meta_list, function(x) x$category))
    priority_cats <- all_cats[grepl("Soil|Physicochem", all_cats, ignore.case = TRUE)]
    other_cats <- setdiff(all_cats, priority_cats)
    cats <- c(priority_cats, other_cats)
    
    # Pre-filter and pre-group variables by category
    grouped_vars <- list()
    for(cat in cats) {
      cat_vars <- Filter(function(x) x$category == cat && x$actual %in% vars_to_show, meta_list)
      if(length(cat_vars) > 0) {
        grouped_vars[[cat]] <- cat_vars
      }
    }
    
    meta_actuals <- sapply(meta_list, function(x) x$actual)
    
    list(
      vars_to_show = vars_to_show,
      grouped_vars = grouped_vars,
      meta_actuals = meta_actuals
    )
  })

  generate_popup <- function(data_row) {
    data_row <- as.list(data_row)
    names_in_row <- names(data_row)
    
    find_val <- function(key) {
      if (key %in% names_in_row) return(data_row[[key]])
      idx <- grep(paste0("^", key, "$"), names_in_row, ignore.case = TRUE)
      if (length(idx) > 0) return(data_row[[idx[1]]])
      idx <- grep(paste0(key, "$"), names_in_row, ignore.case = TRUE)
      if (length(idx) > 0) return(data_row[[idx[1]]])
      idx <- grep(as.character(key), names_in_row, ignore.case = TRUE)
      if (length(idx) > 0) return(data_row[[idx[1]]])
      return(NULL)
    }

    cache <- popup_metadata_cache()
    vars_to_show <- cache$vars_to_show
    grouped_vars <- cache$grouped_vars
    meta_actuals <- cache$meta_actuals
    
    html_content <- "<div style='max-height: 300px; overflow-y: auto; font-family: sans-serif; min-width: 200px;'>"
    html_content <- paste0(html_content, "<h4>Point Details</h4><table style='width: 100%; border-collapse: collapse;'>")
    
    for(cat in names(grouped_vars)) {
      cat_vars <- grouped_vars[[cat]]
      html_content <- paste0(html_content, "<tr style='background-color: #f2f2f2;'><td colspan='2'><b>", cat, "</b></td></tr>")
      for(v in cat_vars) {
        val <- find_val(as.character(v$actual))
        val_str <- if(!is.null(val) && (is.numeric(val) || !is.na(suppressWarnings(as.numeric(val))))) round(as.numeric(val), 3) else as.character(val %||% "N/A")
        html_content <- paste0(html_content, "<tr><td style='padding: 3px;'>", v$label, "</td><td style='padding: 3px; text-align: right;'>", val_str, "</td></tr>")
      }
    }
    
    other_vars <- setdiff(vars_to_show, meta_actuals)
    if(length(other_vars) > 0) {
      html_content <- paste0(html_content, "<tr style='background-color: #f2f2f2;'><td colspan='2'><b>Other Variables</b></td></tr>")
      for(ov in other_vars) {
        val <- find_val(as.character(ov))
        val_str <- if(!is.null(val) && (is.numeric(val) || !is.na(suppressWarnings(as.numeric(val))))) round(as.numeric(val), 3) else as.character(val %||% "N/A")
        html_content <- paste0(html_content, "<tr><td style='padding: 3px;'>", ov, "</td><td style='padding: 3px; text-align: right;'>", val_str, "</td></tr>")
      }
    }
    
    html_content <- paste0(html_content, "</table></div>")
    return(html_content)
  }

  # --- Pop-up Settings ---
  observeEvent(input$show_popup_settings, {
    req(rv$mapping$vars)
    vars_list <- rv$mapping$vars
    cats <- unique(sapply(vars_list, function(x) x$category))
    choices <- list()
    for(cat in cats) {
      cat_vars <- Filter(function(x) x$category == cat, vars_list)
      choices[[cat]] <- setNames(sapply(cat_vars, function(x) x$actual), sapply(cat_vars, function(x) x$label))
    }
    
    default_selected <- rv$pop_up_vars
    if (is.null(default_selected)) {
      soil_vars <- Filter(function(x) grepl("Soil|Physicochem", x$category, ignore.case = TRUE), vars_list)
      if (length(soil_vars) > 0) {
        default_selected <- sapply(soil_vars, function(x) x$actual)
      } else {
        default_selected <- sapply(vars_list, function(x) x$actual)
      }
    }
    
    showModal(modalDialog(
      title = "Sampling Point Pop-up Settings",
      pickerInput("popup_var_select", "Select Variables to Display in Pop-ups:", 
                  choices = choices, 
                  selected = default_selected, 
                  multiple = TRUE, 
                  options = list(`actions-box` = TRUE, `live-search` = TRUE)),
      footer = tagList(
        modalButton("Cancel"),
        actionButton("confirm_popups", "Apply Settings", class = "btn-primary")
      )
    ))
  })

  observeEvent(input$confirm_popups, {
    rv$pop_up_vars <- input$popup_var_select
    removeModal()
    showNotification("Pop-up settings updated.", type = "message")
  })

  # --- F2: Point Styling Toolbar Logic ---
  observeEvent(input$toggle_pt_style, {
    shinyjs::toggle("pt_style_toolbar", anim = TRUE, animType = "slide", time = 0.3)
  })

  # Populate color-by and label-field dropdowns when data loads or mapping changes
  observe({
    req(rv$user_data)
    df <- rv$user_data
    cols <- colnames(df)

    # Establish reactive dependency on variable mappings
    vars_meta <- rv$mapping$vars

    cat_cols <- cols[sapply(df, function(x) {
      is.character(x) || is.factor(x) || (is.integer(x) && length(unique(x)) <= 20)
    })]

    color_choices <- c("None (Cyan)" = "none")
    if (!is.null(rv$mapping$loc) && rv$mapping$loc %in% cols) {
      color_choices <- c(color_choices, stats::setNames(rv$mapping$loc, paste0("Locality (", rv$mapping$loc, ")")))
    }
    other_cats <- setdiff(cat_cols, c(rv$mapping$loc, rv$mapping$x, rv$mapping$y))
    if (length(other_cats) > 0) {
      color_choices <- c(color_choices, stats::setNames(other_cats, other_cats))
    }
    
    # Preserve current selections if valid
    curr_color_by <- isolate(input$pt_color_by)
    selected_color_by <- if (!is.null(curr_color_by) && curr_color_by %in% color_choices) curr_color_by else "none"
    updateSelectInput(session, "pt_color_by", choices = color_choices, selected = selected_color_by)

    # Translate column names to human-readable labels, falling back to raw names
    col_labels <- sapply(cols, function(c) {
      get_var_label(c, vars_meta)
    })
    label_choices <- c("(none)" = "none", stats::setNames(cols, col_labels))
    
    curr_label_field <- isolate(input$pt_label_field)
    selected_label_field <- if (!is.null(curr_label_field) && curr_label_field %in% label_choices) curr_label_field else "none"
    updateSelectInput(session, "pt_label_field", choices = label_choices, selected = selected_label_field)
  })

  # Generate default palette when color-by or palette changes
  observeEvent(list(input$pt_color_by, input$pt_palette), {
    req(input$pt_color_by)
    if (input$pt_color_by == "none") {
      rv$pt_style_colors <- NULL
      return()
    }
    req(rv$user_data, input$pt_color_by %in% colnames(rv$user_data))
    groups <- sort(unique(as.character(rv$user_data[[input$pt_color_by]])))
    pal_name <- input$pt_palette %||% "Set1"
    rv$pt_style_colors <- generate_group_palette(groups, pal_name)
    rv$pt_style_palette <- pal_name
  }, ignoreInit = TRUE)

  # Custom color assignment modal
  observeEvent(input$pt_custom_colors, {
    req(rv$pt_style_colors)
    groups <- names(rv$pt_style_colors)

    color_inputs <- lapply(seq_along(groups), function(i) {
      g <- groups[i]
      col_hex <- rv$pt_style_colors[g]
      div(style = "display: flex; align-items: center; gap: 10px; margin-bottom: 8px;",
        div(style = paste0("width: 16px; height: 16px; border-radius: 3px; background-color: ", col_hex, "; border: 1px solid #ccc; flex-shrink: 0;")),
        tags$span(g, style = "width: 120px; font-size: 13px; font-weight: 500; white-space: nowrap; overflow: hidden; text-overflow: ellipsis; flex-shrink: 0;"),
        div(class = "custom-color-row-input", style = "width: 90px; flex-shrink: 0;",
          textInput(paste0("pt_grp_col_", i), NULL, value = col_hex, width = "90px")
        )
      )
    })

    showModal(modalDialog(
      title = tags$span(icon("palette"), " Custom Group Colors"),
      div(style = "max-height: 400px; overflow-y: auto; padding: 5px;",
        tags$style(HTML("
          .custom-color-row-input .form-group { margin-bottom: 0 !important; margin-top: 0 !important; }
          .custom-color-row-input .shiny-input-container { margin-bottom: 0 !important; margin-top: 0 !important; }
        ")),
        p("Enter hex color codes (e.g. #FF5733) for each group:", style = "font-size: 12px; color: #888; margin-bottom: 12px;"),
        color_inputs
      ),
      footer = tagList(
        modalButton("Cancel"),
        actionButton("pt_apply_custom_colors", "Apply Colors", class = "btn-primary", icon = icon("check"))
      ),
      size = "s"
    ))
  })

  observeEvent(input$pt_apply_custom_colors, {
    req(rv$pt_style_colors)
    groups <- names(rv$pt_style_colors)
    for (i in seq_along(groups)) {
      col_val <- input[[paste0("pt_grp_col_", i)]]
      if (!is.null(col_val) && grepl("^#[0-9A-Fa-f]{6}$", col_val)) {
        rv$pt_style_colors[groups[i]] <- col_val
      }
    }
    removeModal()
    showNotification("Custom colors applied.", type = "message", duration = 3)
  })

  # --- Data Setup Logic ---
  output$file_uploaded <- reactive({ !is.null(input$user_file) })
  outputOptions(output, "file_uploaded", suspendWhenHidden = FALSE)
  
  output$model_ready <- reactive({ if(!is.null(rv$rast) || length(rv$v_emp_list) > 0) "yes" else "no" })
  outputOptions(output, "model_ready", suspendWhenHidden = FALSE)
  
  output$export_updated_data <- downloadHandler(
    filename = function() {
      paste0("updated_spatial_dataset_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".xlsx")
    },
    content = function(file) {
      req(rv$user_data)
      openxlsx::write.xlsx(rv$user_data, file = file)
    }
  )
  
  observeEvent(input$user_file, {
    req(input$user_file)
    ext <- tools::file_ext(input$user_file$name)
    
    if (!(tolower(ext) %in% c("csv", "xls", "xlsx"))) {
      showNotification("Invalid file type. Only CSV, XLS, and XLSX are supported.", type = "error")
      return()
    }
    
    fsize <- file.info(input$user_file$datapath)$size
    if (!is.null(fsize) && fsize > 30 * 1024 * 1024) {
      showNotification("File size exceeds 30MB limit.", type = "error")
      return()
    }
    
    df <- tryCatch({
      if (tolower(ext) == "csv") read.csv(input$user_file$datapath)
      else if (tolower(ext) %in% c("xls", "xlsx")) readxl::read_excel(input$user_file$datapath)
      else NULL
    }, error = function(e) { 
      showNotification(paste("Error reading file:", e$message), type = "error")
      NULL
    })
    
    req(df); rv$user_data <- df
    
    # Update selectors with stricter regex
    cols <- colnames(df)
    updateSelectInput(session, "map_x", choices = cols, selected = grep("\\bx\\b|^lon|^longitude", cols, ignore.case=TRUE, value=TRUE)[1])
    updateSelectInput(session, "map_y", choices = cols, selected = grep("\\by\\b|^lat|^latitude", cols, ignore.case=TRUE, value=TRUE)[1])
    updateSelectInput(session, "map_loc", choices = cols, selected = grep("loc|site|farm|id|group", cols, ignore.case=TRUE, value=TRUE)[1])
    
    # Populate Mapping Vars immediately with defaults
    new_vars <- list()
    num_cols <- cols[sapply(df, is.numeric)]
    for (col in num_cols) {
      if (!grepl("\\bx\\b|\\by\\b|lon|lat|latitude|longitude", col, ignore.case=TRUE)) {
        p_cve <- detect_pred_column(col, num_cols, "cve")
        p_ss  <- detect_pred_column(col, num_cols, "ss")
        new_vars[[length(new_vars) + 1]] <- list(
          actual = col, pred = p_cve, pred_ss = p_ss, label = col, category = "Uploaded Data",
          palette = get_default_palette(col, "Uploaded Data", col)
        )
      }
    }
    rv$mapping$vars <- new_vars
    
    # Set default pop-up variables to all numeric columns (excluding coordinates)
    rv$pop_up_vars <- num_cols[!grepl("\\bx\\b|\\by\\b|lon|lat|latitude|longitude", num_cols, ignore.case=TRUE)]
    
    # Populate Sidebar Locality
    curr_locs <- isolate(input$locality)
    new_choices <- c("ALL", unique(df[[input$map_loc %||% cols[1]]]))
    selected_locs <- intersect(curr_locs, new_choices)
    updateSelectInput(session, "locality", choices = new_choices, selected = selected_locs)
    
    # Smooth scroll to variable mapping UI (Issue 5.d)
    shinyjs::runjs("setTimeout(function() { $('html, body').animate({ scrollTop: $('#map_x').offset().top - 20 }, 1000); }, 500);")
  })

  # --- Shapefile Integration ---
  observeEvent(input$user_shp, {
    req(input$user_shp)
    temp_dir <- file.path(tempdir(), paste0("shp_upload_", as.integer(Sys.time())))
    dir.create(temp_dir, showWarnings = FALSE, recursive = TRUE)
    session$onSessionEnded(function() { unlink(temp_dir, recursive = TRUE) })
    
    for(i in 1:nrow(input$user_shp)) {
      file.copy(input$user_shp$datapath[i], file.path(temp_dir, input$user_shp$name[i]), overwrite = TRUE)
    }
    shp_file <- input$user_shp$name[grep("\\.shp$", input$user_shp$name, ignore.case = TRUE)]
    if(length(shp_file) == 0) { showNotification("No .shp file found.", type = "error"); return() }
    s <- tryCatch({ st_read(file.path(temp_dir, shp_file[1]), quiet = TRUE) }, error = function(e) { 
      showNotification(paste("Error reading shapefile:", e$message), type = "error"); NULL 
    })
    req(s)
    if (nrow(s) == 0) {
      showNotification("Uploaded shapefile contains zero features.", type = "error")
      return()
    }
    rv$shp_bound <- s
    
    # Check if shapefile contains polygon geometries
    geom_types <- unique(sf::st_geometry_type(s))
    if (!any(geom_types %in% c("POLYGON", "MULTIPOLYGON"))) {
      showNotification("Uploaded shapefile contains point/line geometry. Monolith will automatically generate boundary polygons (convex hulls) around these points/lines for interpolation.", type = "warning", duration = 12)
    } else {
      showNotification("Custom shapefile loaded successfully!", type = "message")
    }
    
    crs_obj <- sf::st_crs(s)
    crs_val <- NULL
    if (!is.null(crs_obj$epsg) && !is.na(crs_obj$epsg)) {
      crs_val <- paste0("EPSG:", crs_obj$epsg)
    } else if (!is.null(crs_obj$proj4string) && !is.na(crs_obj$proj4string) && crs_obj$proj4string != "") {
      crs_val <- crs_obj$proj4string
    } else if (!is.null(crs_obj$wkt) && !is.na(crs_obj$wkt) && crs_obj$wkt != "") {
      crs_val <- crs_obj$wkt
    }
    if(!is.null(crs_val)) {
      updateSelectizeInput(session, "crs_selection", selected = crs_val)
    }
  })

  # --- Intelligent CRS Suggestion ---
  observeEvent(list(rv$user_data, input$map_x, input$map_y), {
    req(rv$user_data, input$map_x, input$map_y)
    if (!(input$map_x %in% colnames(rv$user_data) && input$map_y %in% colnames(rv$user_data))) return()
    
    df <- rv$user_data %>% select(x = !!sym(input$map_x), y = !!sym(input$map_y)) %>% na.omit()
    if (nrow(df) == 0) return()
    
    x_range <- range(df$x)
    y_range <- range(df$y)
    
    suggested_crs <- "EPSG:4326" # Default WGS84
    
    # Heuristic for detection
    if (all(x_range >= -180 & x_range <= 180) && all(y_range >= -90 & y_range <= 90)) {
       suggested_crs <- "EPSG:4326"
    } else if (all(x_range > 100000 | x_range < -100000)) {
       # Likely projected (UTM or S-JTSK)
       if (any(x_range < 0)) suggested_crs <- "EPSG:5514" 
       else suggested_crs <- "EPSG:32635" 
    }
    
    updateSelectizeInput(session, "map_crs", selected = suggested_crs)
    updateSelectizeInput(session, "crs_selection", selected = suggested_crs)
  })

  # --- Dynamic Resolution Sync ---
  observeEvent(input$crs_selection, {
    req(input$crs_selection)

    # We now enforce metric resolution reporting regardless of target CRS
    updateSliderInput(session, "grid_res", label = "Resolution (m)",
                      min = 1, max = 500, value = 50, step = 1)
  })
  # --- Smart Resolution Recommendation ---
  observeEvent(list(rv$user_data, input$map_x, input$map_y, input$crs_selection, input$locality, input$res_mode), {
    req(rv$user_data, input$map_x, input$map_y, input$crs_selection, input$locality, input$res_mode)
    if (!(input$map_x %in% colnames(rv$user_data) && input$map_y %in% colnames(rv$user_data))) return()
    
    if (input$res_mode == "fixed") {
       output$res_reasoning <- renderText({ "Manual resolution override active." })
       return()
    }

    # Adaptive: Filter data based on selected locality for specific density analysis
    df_raw <- rv$user_data %>% select(x = !!sym(input$map_x), y = !!sym(input$map_y), loc = !!sym(input$map_loc)) %>% na.omit()
    
    if (input$res_mode == "global" || "ALL" %in% input$locality || length(input$locality) == 0) {
      df <- df_raw
      loc_context <- "dataset-wide (Global)"
    } else {
      df <- df_raw %>% filter(loc %in% input$locality)
      loc_context <- if(length(input$locality) == 1) paste("locality:", input$locality) else "selected localities (Per-Locality)"
    }
    
    if (nrow(df) < 2) return()
    
    # Check current CRS units
    crs_obj <- validate_crs(input$crs_selection, "Invalid CRS provided:")
    req(crs_obj)
    is_degree <- grepl("degree", crs_obj$units_gdal %||% "meters", ignore.case = TRUE)
    
    # Project data to target CRS for accurate distance measurement
    pts <- tryCatch({
      st_as_sf(df, coords = c("x", "y"), crs = input$map_crs) %>% st_transform(input$crs_selection)
    }, error = function(e) NULL)
    req(pts)
    coords <- st_coordinates(pts)
    
    # Average Nearest Neighbor Distance
    # Optimized with FNN
    knn_res <- FNN::get.knn(coords, k = 1)
    avg_nn_dist <- mean(knn_res$nn.dist)
    
    # Hybrid Recommendation: 50% of avg NN dist
    rec_res <- avg_nn_dist * 0.5 
    
    # Performance limit: Max 300 cells on longest axis (increased for better detail)
    bbox <- st_bbox(pts)
    width <- bbox["xmax"] - bbox["xmin"]
    height <- bbox["ymax"] - bbox["ymin"]
    max_dim <- max(width, height)
    min_res_by_dim <- max_dim / 300 
    
    final_rec <- max(rec_res, min_res_by_dim)
    
    if (is_degree) {
       final_rec <- max(0.00001, min(0.01, round(final_rec, 6)))
       reasoning <- sprintf("Suggested: %.6f deg. Optimized for %s.", final_rec, loc_context)
    } else {
       final_rec <- max(0.1, min(500, round(final_rec, 1)))
       reasoning <- sprintf("Suggested: %.1f m. Optimized for %s.", final_rec, loc_context)
    }
    
    if (input$res_mode != "fixed") updateSliderInput(session, "grid_res", value = final_rec)
    output$res_reasoning <- renderText({ reasoning })
    
    # Pre-calculate per-locality resolutions for the UI if in 'local' mode
    if (input$res_mode == "local") {
        locs_to_calc <- if("ALL" %in% input$locality || length(input$locality) == 0) unique(df_raw$loc) else input$locality
        temp_res <- list()
        for (l in locs_to_calc) {
            sub_df <- df_raw %>% filter(loc == l)
            if (nrow(sub_df) < 2) next
            
            sub_pts <- tryCatch(st_as_sf(sub_df, coords=c("x","y"), crs=input$map_crs) %>% st_transform(input$crs_selection), error=function(e) { showNotification(paste("Projection failed for subset:", e$message), type = "error"); NULL })
            if(is.null(sub_pts)) next
            
            if (nrow(sub_pts) > 1) {
                 # Strictly enforce metric distances for resolution estimation
                 sub_pts_m <- tryCatch(sf::st_transform(sub_pts, 3857), error = function(e) sub_pts)
                 sub_coords_m <- sf::st_coordinates(sub_pts_m)
                 sub_knn <- FNN::get.knn(sub_coords_m, k = 1)
                 l_res <- mean(sub_knn$nn.dist) * 0.5

                 # If transformation failed and we are in degrees, apply heuristic to convert to meters
                 if (is_degree && identical(sub_pts, sub_pts_m)) {
                    lat_c <- mean(sf::st_coordinates(sf::st_transform(sub_pts, 4326))[,2])
                    m_per_deg <- 111319 * cos(lat_c * pi / 180)
                    l_res <- l_res * m_per_deg
                 }
            } else l_res <- final_rec

            l_res <- max(1, min(5000, l_res))

            temp_res[[l]] <- l_res        }
        rv$loc_resolutions <- temp_res
    } else {
        # If not local mode, apply global/fixed resolution to all selected localities
        locs_to_calc <- if("ALL" %in% input$locality || length(input$locality) == 0) unique(df_raw$loc) else input$locality
        temp_res <- list()
        for (l in locs_to_calc) temp_res[[l]] <- final_rec
        rv$loc_resolutions <- temp_res
    }
  })

  observeEvent(input$meta_file, {
    req(input$meta_file, rv$user_data)
    ext <- tools::file_ext(input$meta_file$name)
    
    if (!(tolower(ext) %in% c("csv", "xls", "xlsx"))) {
      showNotification("Invalid metadata file type. Only CSV, XLS, and XLSX are supported.", type = "error")
      return()
    }
    
    fsize <- file.info(input$meta_file$datapath)$size
    if (!is.null(fsize) && fsize > 30 * 1024 * 1024) {
      showNotification("Metadata file size exceeds 30MB limit.", type = "error")
      return()
    }
    
    m_df <- tryCatch({
      if (tolower(ext) == "csv") read.csv(input$meta_file$datapath)
      else readxl::read_excel(input$meta_file$datapath)
    }, error = function(e) NULL)
    
    req(m_df)
    user_cols <- colnames(rv$user_data)
    new_vars <- match_metadata_columns(m_df, user_cols)
    
    if (length(new_vars) > 0) {
      rv$mapping$vars <- new_vars
      showNotification(paste("Auto-mapped", length(new_vars), "variables with dual predictions."), type = "message")
    }
  })

  output$var_mapping_ui <- renderUI({
    req(rv$user_data)
    cols <- colnames(rv$user_data)
    num_cols <- cols[sapply(rv$user_data, is.numeric)]
    
    # Use existing mapping if available to populate targets, otherwise fallback to first 20 numeric columns
    if (!is.null(rv$mapping$vars) && length(rv$mapping$vars) > 0) {
      targets <- sapply(rv$mapping$vars, function(x) x$actual)
    } else {
      targets <- num_cols[!grepl("\\bx\\b|\\by\\b|lon|lat|latitude|longitude", num_cols, ignore.case=TRUE)]
      # Limit to 30 to prevent UI freezing if no metadata is provided
      if (length(targets) > 30) {
        targets <- head(targets, 30)
        showNotification("Too many columns. Showing first 30 for mapping. Please use an Excel metadata file for bulk mapping.", type = "warning")
      }
    }
    
    get_map_val <- function(target, field) {
      match <- Filter(function(x) x$actual == target, rv$mapping$vars)
      if (length(match) > 0) {
         val <- match[[1]][[field]]
         if(is.null(val) || length(val) == 0) return(NULL)
         if(is.na(val)) return(NULL) else return(val)
      } else {
         return(NULL)
      }
    }

    tryCatch({
      tagList(
        lapply(seq_along(targets), function(i) {
          t <- targets[i]
          def_p_cve <- get_map_val(t, "pred")    %||% detect_pred_column(t, num_cols, "cve") %||% "None"
          def_p_ss  <- get_map_val(t, "pred_ss") %||% detect_pred_column(t, num_cols, "ss")  %||% "None"
          def_l     <- get_map_val(t, "label")    %||% t
          def_c     <- get_map_val(t, "category") %||% "Uploaded Data"
          
          if(is.na(def_p_cve)) def_p_cve <- "None"
          if(is.na(def_p_ss)) def_p_ss <- "None"
          if(is.na(def_l)) def_l <- t
          if(is.na(def_c)) def_c <- "Uploaded Data"
          
          div(style="border-bottom: 1px solid #eee; padding: 10px 0; margin-bottom: 10px;",
            fluidRow(
              column(2, tags$b(t)),
              column(3, selectInput(paste0("pair_pred_cve_", i), "Best Pred (_cve)", choices = c("None", num_cols), selected = def_p_cve)),
              column(3, selectInput(paste0("pair_pred_ss_", i),  "Split Pred (_ss)", choices = c("None", num_cols), selected = def_p_ss)),
              column(2, textInput(paste0("pair_label_", i), "Label", value = def_l)),
              column(2, textInput(paste0("pair_cat_", i), "Category", value = def_c))
            )
          )
        }),
        actionButton("confirm_mapping", "CONFIRM VARIABLE MAPPING", class = "btn-primary btn-block")
      )
    }, error = function(e) {
      print(paste("Error in var_mapping_ui:", e$message))
      h4(paste("Error rendering UI:", e$message), style="color:red;")
    })
  })

  observeEvent(input$confirm_mapping, {
    req(rv$user_data)
    cols <- colnames(rv$user_data)
    num_cols <- cols[sapply(rv$user_data, is.numeric)]
    
    if (!is.null(rv$mapping$vars) && length(rv$mapping$vars) > 0) {
      targets <- sapply(rv$mapping$vars, function(x) x$actual)
    } else {
      targets <- num_cols[!grepl("\\bx\\b|\\by\\b|lon|lat|latitude|longitude", num_cols, ignore.case=TRUE)]
      if (length(targets) > 30) targets <- head(targets, 30)
    }
    
    new_vars <- list()
    for (i in seq_along(targets)) {
      p_cve <- input[[paste0("pair_pred_cve_", i)]]
      p_ss  <- input[[paste0("pair_pred_ss_", i)]]
      
      raw_cat <- input[[paste0("pair_cat_", i)]]
      cat_val <- if (is.null(raw_cat) || is.na(raw_cat) || raw_cat == "") "Uploaded Data" else raw_cat
      
      raw_lab <- input[[paste0("pair_label_", i)]]
      lab_val <- if (is.null(raw_lab) || is.na(raw_lab) || raw_lab == "") targets[i] else raw_lab
      
      new_vars[[length(new_vars) + 1]] <- list(
        actual = targets[i],
        pred = if (is.null(p_cve) || is.na(p_cve) || p_cve == "None") NULL else p_cve,
        pred_ss = if (is.null(p_ss) || is.na(p_ss) || p_ss == "None") NULL else p_ss,
        label = lab_val,
        category = cat_val,
        palette = get_default_palette(targets[i], cat_val, lab_val)
      )
    }
    rv$mapping$vars <- new_vars
    showNotification("Variable mapping saved!", type = "message")
  })

  observe({
    req(input$map_x, input$map_y, input$map_loc, input$map_crs)
    rv$mapping$x <- input$map_x
    rv$mapping$y <- input$map_y
    rv$mapping$loc <- input$map_loc
    rv$mapping$crs <- input$map_crs
    
    # Update sidebar locality choices whenever the mapping column changes
    if (!is.null(rv$user_data) && input$map_loc %in% colnames(rv$user_data)) {
      loc_choices <- unique(rv$user_data[[input$map_loc]])
      curr_locs <- isolate(input$locality)
      new_choices <- c("ALL", loc_choices)
      selected_locs <- intersect(curr_locs, new_choices)
      if (length(selected_locs) == 0) selected_locs <- loc_choices[1]
      updateSelectInput(session, "locality", choices = new_choices, selected = selected_locs)
    }
  })

  output$setup_minimap <- renderLeaflet({
    req(rv$user_data, rv$mapping$x, rv$mapping$y, rv$mapping$crs)

    # F2: Determine which extra columns are needed for styling
    color_by <- input$pt_color_by %||% "none"
    apply_mini <- isTRUE(input$pt_apply_minimap)

    needed <- c(rv$mapping$x, rv$mapping$y)
    if (apply_mini && color_by != "none" && color_by %in% colnames(rv$user_data)) needed <- c(needed, color_by)
    needed <- unique(needed)

    df_map <- rv$user_data %>% dplyr::select(dplyr::all_of(needed)) %>% na.omit()
    if (nrow(df_map) == 0) return(NULL)

    
    pts <- tryCatch({
      st_as_sf(df_map, coords = c(rv$mapping$x, rv$mapping$y), crs = rv$mapping$crs) %>% st_transform(4326)
    }, error = function(e) NULL)
    req(pts)

    current_tiles <- input$base_map_layer %||% "Esri.WorldImagery"

    m <- leaflet(pts, options = leafletOptions(zoomControl = FALSE)) %>% addProviderTiles(current_tiles, layerId = "base_tiles")

    if (apply_mini && color_by != "none") {
      m <- add_styled_points(m, pts,
        color_by = color_by,
        custom_colors = rv$pt_style_colors,
        show_labels = FALSE,
        label_field = "none",
        label_size = 11,
        marker_size = input$pt_marker_size %||% 3
      )
    } else {
      m <- m %>% addCircleMarkers(radius = input$pt_marker_size %||% 3, color = "cyan", opacity = 1)
    }
    session_state$minimap_rendered <- TRUE
    m
  })

  # --- Metadata Helper ---
  get_current_meta <- function() {
    var <- input$var_id
    if (is.null(var) || var == "" || is.null(rv$mapping$vars)) return(NULL)
    
    # Find variable in dynamic mapping
    idx <- which(sapply(rv$mapping$vars, function(x) x$actual == var))
    if (length(idx) == 0) return(NULL)
    m <- rv$mapping$vars[[idx]]
    
    # Use selected palette from pickerInput if available, otherwise default
    pal <- "YlOrRd"
    if (!is.null(input$palette_select) && input$palette_select != "") {
      pal <- input$palette_select
    } else if (!is.null(m$palette) && m$palette != "") {
      pal <- m$palette
    }
    
    # Determine the column being viewed
    view_col <- switch(input$value_type,
      "actual" = as.character(m$actual),
      "pred"   = if(!is.null(m$pred)) as.character(m$pred) else NULL,
      "pred_ss"= if(!is.null(m$pred_ss)) as.character(m$pred_ss) else NULL,
      "resid"  = as.character(m$actual)
    )
    
    list(
      actual = as.character(m$actual),
      pred = if(!is.null(m$pred)) as.character(m$pred) else NULL,
      pred_ss = if(!is.null(m$pred_ss)) as.character(m$pred_ss) else NULL,
      view_col = view_col,
      label = as.character(m$label %||% m$actual),
      palette = as.character(pal),
      unit = as.character(m$unit %||% "")
    )
  }
  
  # --- Dynamic Sidebar Sync ---
  # Centralized observer to keep sidebar in sync with mapping state
  observe({
    req(rv$mapping$vars)
    vars <- rv$mapping$vars
    cats <- unique(sapply(vars, function(x) x$category))
    
    # Update Category choices
    current_cat <- input$var_category
    sel_cat <- if(!is.null(current_cat) && current_cat %in% cats) current_cat else cats[1]
    updateSelectInput(session, "var_category", choices = cats, selected = sel_cat)
    
    # Update Variable choices based on selected/predicted category
    filtered <- Filter(function(x) x$category == sel_cat, vars)
    choices <- setNames(sapply(filtered, function(x) x$actual), sapply(filtered, function(x) x$label))
    updateSelectInput(session, "var_id", choices = choices)
  })

  observeEvent(input$info_btn, {
    shinyjs::runjs("$('#docs_drawer').css('right', '0');")
  })
  
  observeEvent(input$close_docs_btn, {
    shinyjs::runjs("$('#docs_drawer').css('right', '-600px');")
  })

  observeEvent(input$about_btn, {
    showModal(modalDialog(
      title = "About Monolith",
      size = "m",
      easyClose = TRUE,
      footer = modalButton("Close"),
      div(style = "text-align: center; padding: 20px;",
          img(src = "assets/banner.png", style = "max-width: 100%; height: auto; margin-bottom: 20px;"),
          h4("Workbench for statistics and optimized mapping in life sciences."),
          p("Version: 0.9.8a"),
          p("Integrated geostatistical modeling, classification and statistical interpretation."),
          hr(),
          p("Designed for high-performance parallel processing and spatial diagnostics, multi-scale interpolation via kriging, inverse distance weighting, and thin plate splines with practical multi-criteria optimization."),
          p("Supported with the Descriptive and Exploratory Suite with dynamic visualizations and statistics."),
          hr(),
          p(strong("A vibe-coded product of `that` couple of months following the loose of institutional e-mail address.")),
          p(style = "color: #666; font-size: 0.9em;", "  by Recep Serdar Kara in cooperation with Gemini CLI - 2026")
      )
    ))
  })
  
  output$render_ui_ux_guide <- renderUI({
    withMathJax(HTML(commonmark::markdown_html(paste(readLines("docs/ui_ux_guide.md", warn = FALSE), collapse = "\n"))))
  })
  
  output$render_scientific_guide <- renderUI({
    withMathJax(HTML(commonmark::markdown_html(paste(readLines("docs/scientific_guide.md", warn = FALSE), collapse = "\n"))))
  })
  
  # --- Global Scale Synchronization ---
  joint_vv <- reactive({
    is_uncertainty <- input$value_type %in% c("pred_ss", "uncert")
    get_joint_scale_values(rv$rast, rv$rast_pred, input$match_scales, is_uncertainty)
  })

  # --- Unified Agro Engine ---
  agro_params <- reactive({
    req(input$color_style == "agro")
    req(input$var_id)
    meta <- get_current_meta()
    req(meta)
    n_c <- input$agro_n_classes
    
    if(input$agro_method == "limits") {
      brks_inner <- sapply(1:(n_c-1), function(i) {
        val <- input[[paste0("agro_limit_", i)]]
        if(is.null(val)) i * 10 else val
      })
    } else {
      vv_joint <- joint_vv()
      if(!is.null(vv_joint)) {
        vv <- vv_joint
      } else {
        # Use the current raster's values for data-driven breaks
        target <- if(input$value_type == "actual") rv$rast else rv$rast_pred
        if(is.null(target)) {
          # Fallback to quantiles of raw data if raster isn't ready
          df <- rv$user_data
          v_data <- df[[meta$actual]]
          if(is.null(v_data) || length(v_data) < n_c) return(NULL)
          vv <- v_data
        } else {
          vv <- as.vector(values(target, na.rm=TRUE))
        }
      }
      
      if(length(vv) < n_c) return(NULL)
      brks_inner <- classIntervals(vv, n=n_c, style=input$agro_method)$brks[2:n_c]
    }
    brks <- sort(unique(c(-Inf, brks_inner, Inf)))
    
    # Create classification matrix for terra::classify
    rcl_mat <- matrix(NA, nrow = n_c, ncol = 3)
    for(i in 1:n_c) {
      rcl_mat[i, ] <- c(brks[i], brks[i+1], i)
    }
    
    colors <- get_agro_colors(n_c)
    labels <- if(n_c==3) c("Low", "Med", "High") else paste("Class", 1:n_c)
    
    leg_labels <- character(n_c)
    for(i in 1:n_c) {
      if(i==1) leg_labels[i] <- paste("<", round(brks[2], 3))
      else if(i==n_c) leg_labels[i] <- paste(">", round(brks[n_c], 3))
      else leg_labels[i] <- paste(round(brks[i],3), "-", round(brks[i+1],3))
    }
    if(n_c == 3) leg_labels <- paste(labels, ":", leg_labels)
    
    list(brks = brks, rcl_mat = rcl_mat, colors = colors, labels = labels, leg_labels = leg_labels, n_c = n_c)
  })

  # Pickers
  volumes <- c(Home = fs::path_home(), Project = getwd())
  shinyFileChoose(input, "load_config", roots = volumes, session = session, filetypes = c("json"))

  # --- Sidebar Configuration Management: Save & Load Backend ---
  
  observeEvent(input$save_config, {
    showModal(modalDialog(
      title = "Save Session Configuration",
      size = "m",
      easyClose = TRUE,
      footer = modalButton("Cancel"),
      div(style = "padding: 10px;",
          h4("Export active parameters to a local JSON file:"),
          p("This configuration file saves active coordinate column pairings, variable lists, category associations, custom color palettes, and active spatial interpolation engines. You can load this file back in a future session."),
          hr(),
          div(style = "text-align: center; margin-top: 20px;",
              downloadButton("download_config_json", "DOWNLOAD CONFIGURATION FILE", class = "btn-success btn-lg")
          )
      )
    ))
  })

  output$download_config_json <- downloadHandler(
    filename = function() {
      paste0("monolith_config_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".json")
    },
    content = function(file) {
      config_list <- list(
        # Coordinate selection
        map_x = input$map_x,
        map_y = input$map_y,
        map_loc = input$map_loc,
        map_crs = input$map_crs,
        crs_selection = input$crs_selection,
        # Variable category / Variable selector
        var_category = input$var_category,
        var_id = input$var_id,
        value_type = input$value_type,
        # Interpolation configurations
        method = input$method,
        boundary_type = input$boundary_type,
        buff_mode = input$buff_mode,
        buff_dist = input$buff_dist,
        res_mode = input$res_mode,
        grid_res = input$grid_res,
        color_style = input$color_style,
        # Active variable mappings
        vars_mapping = rv$mapping$vars
      )
      writeLines(jsonlite::toJSON(config_list, auto_unbox = TRUE, pretty = TRUE), file)
    }
  )

  observeEvent(input$load_config, {
    req(input$load_config)
    file_info <- shinyFiles::parseFilePaths(volumes, input$load_config)
    req(nrow(file_info) > 0)
    config_path <- file_info$datapath[1]
    
    tryCatch({
      cfg <- jsonlite::fromJSON(config_path, simplifyVector = FALSE)
      
      # Restore reactive values
      if (!is.null(cfg$vars_mapping)) {
        # Ensure mapping$vars gets formatted as list of elements matching our internal structure
        rv$mapping$vars <- cfg$vars_mapping
      }
      
      # Restore UI inputs
      if (!is.null(cfg$map_x)) updateSelectInput(session, "map_x", selected = cfg$map_x)
      if (!is.null(cfg$map_y)) updateSelectInput(session, "map_y", selected = cfg$map_y)
      if (!is.null(cfg$map_loc)) updateSelectInput(session, "map_loc", selected = cfg$map_loc)
      if (!is.null(cfg$map_crs)) updateSelectizeInput(session, "map_crs", selected = cfg$map_crs)
      if (!is.null(cfg$crs_selection)) updateSelectizeInput(session, "crs_selection", selected = cfg$crs_selection)
      
      if (!is.null(cfg$var_category)) updateSelectInput(session, "var_category", selected = cfg$var_category)
      if (!is.null(cfg$var_id)) updateSelectInput(session, "var_id", selected = cfg$var_id)
      if (!is.null(cfg$value_type)) updateSelectInput(session, "value_type", selected = cfg$value_type)
      
      if (!is.null(cfg$method)) updateSelectInput(session, "method", selected = cfg$method)
      if (!is.null(cfg$boundary_type)) updateSelectInput(session, "boundary_type", selected = cfg$boundary_type)
      if (!is.null(cfg$buff_mode)) updateRadioButtons(session, "buff_mode", selected = cfg$buff_mode)
      if (!is.null(cfg$buff_dist)) updateNumericInput(session, "buff_dist", value = cfg$buff_dist)
      if (!is.null(cfg$res_mode)) updateRadioButtons(session, "res_mode", selected = cfg$res_mode)
      if (!is.null(cfg$grid_res)) updateSliderInput(session, "grid_res", value = cfg$grid_res)
      if (!is.null(cfg$color_style)) updateSelectInput(session, "color_style", selected = cfg$color_style)
      
      showNotification("Configuration loaded successfully!", type = "message", duration = 5)
    }, error = function(e) {
      showNotification(paste("Failed to load configuration:", e$message), type = "error", duration = 7)
    })
  })

  output$palette_ui <- renderUI({
    req(input$var_id, rv$mapping$vars)
    idx <- which(sapply(rv$mapping$vars, function(x) x$actual == input$var_id))
    if (length(idx) == 0) return(NULL)
    m <- rv$mapping$vars[[idx]]
    choices <- palette_choices_precomputed
    pickerInput("palette_select", "Color Palette", 
                choices = choices, 
                selected = m$palette %||% "YlOrRd",
                options = list(`live-search` = TRUE),
                choicesOpt = list(content = names(choices)))
  })

  # Automatic Export Title Sync
  observeEvent(input$var_id, {
    req(input$var_id); meta <- get_current_meta(); req(meta)
    updateTextInput(session, "exp_title", value = paste(meta$label, "- Soil Mapping"))
  })

  # Dynamic Manual Variogram Sliders Auto-Scaling
  observe({
    req(rv$user_data, input$var_id, rv$mapping$vars)
    meta <- get_current_meta()
    req(meta)
    
    col_name <- meta$actual
    v_data <- rv$user_data[[col_name]]
    if (!is.null(v_data) && is.numeric(v_data) && length(na.omit(v_data)) >= 3) {
      variance <- var(v_data, na.rm = TRUE)
      if (!is.na(variance) && variance > 0) {
        max_sill <- round(variance * 2, 2)
        step_val <- round(variance / 100, 4)
        if(step_val == 0) step_val <- 0.01
        
        x_col <- rv$mapping$x
        y_col <- rv$mapping$y
        if (!is.null(x_col) && !is.null(y_col) && x_col %in% colnames(rv$user_data) && y_col %in% colnames(rv$user_data)) {
          xs <- rv$user_data[[x_col]]
          ys <- rv$user_data[[y_col]]
          max_dist <- sqrt((max(xs, na.rm=TRUE) - min(xs, na.rm=TRUE))^2 + (max(ys, na.rm=TRUE) - min(ys, na.rm=TRUE))^2)
          if (!is.na(max_dist) && max_dist > 0) {
            max_range <- round(max_dist * 1.5, 0)
            step_range <- round(max_range / 100, 0)
            if(step_range == 0) step_range <- 1
            
            fit <- rv$v_fit_list[[paste0(input$m_loc %||% "global", "_act")]]
            if (is.null(fit)) {
              updateSliderInput(session, "m_nugget", min = 0, max = max_sill, value = 0, step = step_val)
              updateSliderInput(session, "m_psill", min = 0, max = max_sill, value = round(variance, 2), step = step_val)
              updateSliderInput(session, "m_range", min = 1, max = max_range, value = round(max_dist / 4, 0), step = step_range)
            }
          }
        }
      }
    }
  })

  output$agro_options <- renderUI({
    req(input$color_style == "agro", input$agro_method == "limits", input$var_id)
    nut <- get_nut_key(input$var_id)
    def_limits <- if(!is.null(nut) && nut %in% names(nutrient_limits)) nutrient_limits[[nut]] else NULL
    
    lapply(1:(input$agro_n_classes - 1), function(i) {
      val <- if(!is.null(def_limits) && i <= length(def_limits)) def_limits[i] else i * 10
      numericInput(paste0("agro_limit_", i), paste("Limit", i), value = val)
    })
  })

  output$locality_selector_ui <- renderUI({
      req(rv$loc_names); selectInput("sel_loc_stats", "Filter Analysis View:", choices = c("Total (Combined)", rv$loc_names))
    })
  
  # --- Advanced Kriging & Covariates ---
  output$covariate_selector_ui <- renderUI({
    req(rv$user_data, input$var_id)
    cols <- colnames(rv$user_data)
    num_cols <- cols[sapply(rv$user_data, is.numeric)]
    exclude <- c(input$map_x, input$map_y, input$var_id)
    raw_choices <- num_cols[!(num_cols %in% exclude)]
    
    vars_metadata <- rv$mapping$vars
    choices_named <- setNames(raw_choices, sapply(raw_choices, function(v) {
      match <- Filter(function(x) x$actual == v, vars_metadata)
      if(length(match) > 0 && !is.null(match[[1]]$label) && match[[1]]$label != "") {
        match[[1]]$label
      } else {
        v
      }
    }))
    
    pickerInput("aux_vars", "Select Predictors:", 
                choices = choices_named, multiple = TRUE, 
                options = list(`live-search` = TRUE, `actions-box` = TRUE))
  })

      observeEvent(input$calc_corr, {
        req(rv$user_data, input$var_id)
        df <- rv$user_data[sapply(rv$user_data, is.numeric)]
        df <- df[, !(colnames(df) %in% c(rv$mapping$x, rv$mapping$y))]
        
        target <- input$var_id
        if(!(target %in% colnames(df))) {
          showNotification("Target variable not in numeric data.", type = "error")
          return()
        }
        
        res_list <- lapply(setdiff(colnames(df), target), function(v) {
          test <- tryCatch(cor.test(df[[target]], df[[v]], use = "pairwise.complete.obs"), error = function(e) NULL)
          if(!is.null(test)) {
             data.frame(Variable = v, Corr = test$estimate, Pval = test$p.value, stringsAsFactors = FALSE)
          } else {
             NULL
          }
        })
        res_df <- do.call(rbind, Filter(Negate(is.null), res_list))
        
        if(is.null(res_df) || nrow(res_df) == 0) {
          showNotification("Could not calculate correlations. Ensure numeric data is available.", type = "error")
          return()
        }
        
        rv$full_cor_matrix <- res_df # Re-using variable name but storing dataframe instead of matrix
        rv$show_corr_panel <- TRUE
      })
    
      output$corr_results_ui <- renderUI({
        req(rv$show_corr_panel, rv$full_cor_matrix, input$var_id)
        res_df <- rv$full_cor_matrix
        target <- input$var_id
        
        # Apply p-value filter
        thresh <- as.numeric(input$corr_pval_thresh %||% 1)
        if(thresh < 1) {
           res_df <- res_df[!is.na(res_df$Pval) & res_df$Pval <= thresh, ]
        }
        
        if(nrow(res_df) == 0) return(tags$p("No variables meet the significance threshold."))
        
        # Map variables to categories and labels from metadata
        vars_metadata <- rv$mapping$vars
        var_to_cat <- sapply(res_df$Variable, function(v) {
          match <- Filter(function(x) x$actual == v, vars_metadata)
          if(length(match) > 0) match[[1]]$category else "Uploaded Data"
        })

        var_to_label <- sapply(res_df$Variable, function(v) {
          match <- Filter(function(x) x$actual == v, vars_metadata)
          if(length(match) > 0 && !is.null(match[[1]]$label) && match[[1]]$label != "") {
            match[[1]]$label
          } else {
            v # Fallback to column name
          }
        })
        
        res_df$Category <- var_to_cat
        res_df$Label <- var_to_label
        res_df$AbsCorr <- abs(res_df$Corr)

        # Filter for valid categories only
        cats <- unique(res_df$Category)

        # Build list of tabs
        tabs <- list()

        # 1. "All" Tab
        results_all <- res_df[order(res_df$AbsCorr, decreasing = TRUE), ]
        res_all <- head(results_all, 8)

        tabs[[1]] <- tabPanel("All",
          tags$ul(style="font-size: 0.85em; padding-left: 15px; margin-top: 5px; list-style-type: none;",
            lapply(1:nrow(res_all), function(i) {
              tags$li(sprintf("%s: %.3f (p=%.3f)", res_all$Label[i], res_all$Corr[i], res_all$Pval[i]))
            })
          )
        )

        # 2. Category Tabs using lapply
        cat_dfs <- split(res_df, res_df$Category)
        cat_tabs <- lapply(names(cat_dfs), function(cat) {
          results_cat <- cat_dfs[[cat]]
          results_cat <- results_cat[order(results_cat$AbsCorr, decreasing = TRUE), ]
          res_cat <- head(results_cat, 8)
          
          tabPanel(cat,
            tags$ul(style="font-size: 0.85em; padding-left: 15px; margin-top: 5px; list-style-type: none;",
              lapply(1:nrow(res_cat), function(i) {
                tags$li(sprintf("%s: %.3f (p=%.3f)", res_cat$Label[i], res_cat$Corr[i], res_cat$Pval[i]))
              })
            )
          )
        })
        
        tabs <- c(list(tabs[[1]]), cat_tabs)        
        tagList(
          hr(),
          tags$h6("Predictor Ranks (Correlation):"),
          tags$div(style = "overflow-x: auto; white-space: nowrap; border-bottom: 1px solid #ddd; margin-bottom: 5px;",
            do.call(tabsetPanel, c(list(id = "cor_tabs", type = "pills"), tabs))
          )
        )
      })
    
      # Deprecated UI but keeping it as NULL or redirecting
      output$cor_ranks_modal_ui <- renderUI({ NULL })  # --- TPS Optimization ---
  tps_opt_vals <- reactiveVal(NULL)
  observeEvent(input$opt_tps, {
    req(rv$user_data, input$var_id, input$method == "TPS")
    locs <- if("ALL" %in% input$locality) unique(rv$user_data[[rv$mapping$loc]]) else input$locality
    meta <- get_current_meta(); req(meta)
    
    # Expanded Lambda Search Space for better resolution
    lambdas <- c(0, 10^seq(-8, 1, length.out = 30))
    rv$tps_gcv_data <- list()
    
    withProgress(message = "Optimizing TPS Lambda per region...", {
      targets <- "act"
      if(input$comp_mode || input$value_type != "actual") targets <- c("act", "pre")
      
      for(target in targets) {
        val_col <- if(target == "act") meta$actual else (if(input$value_type == "pred_ss") meta$pred_ss else meta$pred)
        if(is.null(val_col) || !(val_col %in% colnames(rv$user_data))) next
        
        current_crs <- rv$mapping$crs
        
        df_list <- lapply(locs, function(l) {
          sub_df <- rv$user_data %>% filter(!!sym(rv$mapping$loc) == l) %>% 
            select(x = !!sym(rv$mapping$x), y = !!sym(rv$mapping$y), v = !!sym(val_col)) %>% na.omit()
          list(l = l, df = sub_df)
        })

        res_list <- furrr::future_map(df_list, function(item) {
          if(nrow(item$df) < 5) return(list(l = item$l, best_lam = 0, gcv_data = NULL, err = NULL))
          
          raw_coords <- sf::st_coordinates(sf::st_as_sf(item$df, coords=c("x","y"), crs=current_crs))
          vals <- item$df$v
          
          xm <- min(raw_coords[,1]); xM <- max(raw_coords[,1])
          ym <- min(raw_coords[,2]); yM <- max(raw_coords[,2])
          max_range <- max(xM - xm, yM - ym)
          if(max_range == 0) max_range <- 1
          pts <- cbind((raw_coords[,1]-xm)/max_range, 
                       (raw_coords[,2]-ym)/max_range)
          
          tryCatch({
            mod <- fields::Tps(pts, vals)
            best_lam <- mod$lambda

            gcv_res <- data.frame(
              lambda = mod$gcv.grid[,1],
              gcv = mod$gcv.grid[,3]
            )
            gcv_res <- gcv_res[gcv_res$lambda > 0, , drop=FALSE]

            list(l = item$l, best_lam = best_lam, gcv_data = gcv_res, err = NULL)          }, error = function(e) {
            list(l = item$l, best_lam = 0, gcv_data = NULL, err = e$message)
          })
        }, .options = furrr::furrr_options(seed = 12345, packages = c("sf", "fields")))
        
        for(res in res_list) {
          l <- res$l
          if(!is.null(res$err)) {
            rv$log <- paste0(rv$log, "\nTPS Opt Error (", l, "): ", res$err)
          } else {
            set_regional_param("TPS", l, target, res$best_lam)
            if(!is.null(res$gcv_data)) {
              rv$tps_gcv_data[[paste0(l, "_", target)]] <- res$gcv_data
            }
          }
        }
      }
    })
    
    all_best <- sapply(locs, function(l) get_regional_param("TPS", l, "act"))
    updateSliderInput(session, "tps_lambda", value = mean(all_best))
    
    tps_opt_vals(list(locs = locs, targets = targets))
    showNotification("TPS Optimization Complete. Per-region Lambdas stored.", type = "message")
  })
  
  output$tps_opt_panel <- renderUI({
    res <- tps_opt_vals(); if(is.null(res)) return(NULL)
    
    rows <- lapply(res$locs, function(l) {
      act_val <- get_regional_param("TPS", l, "act")
      pre_val <- if("pre" %in% res$targets) get_regional_param("TPS", l, "pre") else NA
      tags$tr(
        tags$td(l),
        tags$td(sprintf("%.6f", act_val)),
        tags$td(if(is.na(pre_val)) "N/A" else sprintf("%.6f", pre_val))
      )
    })
    
    div(style = "margin-top: 10px; padding: 10px; background-color: #f8f9fa; color: #495057; border: 1px solid #dee2e6; border-radius: 4px; font-size: 0.8em;",
        h5("Optimization Summary (Best Lambdas):"),
        tags$table(class = "table table-condensed table-bordered", style = "background-color: #ffffff; color: #000000;",
          tags$thead(tags$tr(tags$th("Locality"), tags$th("Actual"), tags$th("Predicted"))),
          tags$tbody(rows)
        )
    )
  })

  # --- IDW Optimization ---
  idw_opt_vals <- reactiveVal(NULL)
  observeEvent(input$opt_idw, {
    req(rv$user_data, input$var_id, input$method == "IDW", input$locality)
    
    # Respect user selection: If specific localities are selected, only optimize those.
    # We shouldn't blindly optimize all unless "ALL" is explicitly in the selection.
    locs <- if("ALL" %in% input$locality || length(input$locality) == 0) {
      unique(rv$user_data[[rv$mapping$loc]])
    } else {
      input$locality
    }
    
    meta <- get_current_meta(); req(meta)
    factors <- seq(0.5, 5.0, by = 0.5)
    
    withProgress(message = "Calculating optimal IDW factors per region...", {
      # Iterate over targets: Actual and Predicted (if in comparison/prediction mode)
      targets <- "act"
      if(input$comp_mode || input$value_type != "actual") targets <- c("act", "pre")
      
      for(target in targets) {
        val_col <- if(target == "act") meta$actual else (if(input$value_type == "pred_ss") meta$pred_ss else meta$pred)
        if(is.null(val_col) || !(val_col %in% colnames(rv$user_data))) next

        current_crs <- rv$mapping$crs
        idw_nmax_val <- input$idw_nmax

        df_list <- lapply(locs, function(l) {
          sub_df <- rv$user_data %>% filter(!!sym(rv$mapping$loc) == l) %>% 
            select(x = !!sym(rv$mapping$x), y = !!sym(rv$mapping$y), v = !!sym(val_col)) %>% na.omit()
          list(l = l, df = sub_df)
        })

        res_list <- furrr::future_map(df_list, function(item) {
          # Force future to export custom geostatistical functions
          force_globals <- list(optimize_idw_p)
          
          if(nrow(item$df) < 5) return(list(l = item$l, best_f = 2.0))
          pts <- sf::st_as_sf(item$df, coords=c("x","y"), crs=current_crs)
          best_f <- optimize_idw_p(pts, "v", nmax = idw_nmax_val)
          return(list(l = item$l, best_f = best_f))
        }, .options = furrr::furrr_options(seed = 12345, packages = c("sf", "gstat")))

        for(res in res_list) {
          set_regional_param("IDW", res$l, target, res$best_f)
        }
      }    })
    
    # Update main slider with average of best factors for current selection to provide feedback
    all_best <- sapply(locs, function(l) get_regional_param("IDW", l, "act"))
    updateSliderInput(session, "idw_p", value = mean(all_best))
    
    idw_opt_vals(list(locs = locs, targets = targets))
    showNotification(paste("IDW Optimization Complete for:", paste(locs, collapse=", ")), type = "message", duration = 5)
  })
  
  output$idw_opt_panel <- renderUI({
    res <- idw_opt_vals(); if(is.null(res)) return(NULL)
    
    rows <- lapply(res$locs, function(l) {
      act_val <- get_regional_param("IDW", l, "act")
      pre_val <- if("pre" %in% res$targets) get_regional_param("IDW", l, "pre") else NA
      tags$tr(
        tags$td(l),
        tags$td(sprintf("%.1f", act_val)),
        tags$td(if(is.na(pre_val)) "N/A" else sprintf("%.1f", pre_val))
      )
    })
    
    div(style = "margin-top: 10px; padding: 10px; background-color: #f8f9fa; color: #495057; border: 1px solid #dee2e6; border-radius: 4px; font-size: 0.8em;",
        h5("Optimization Summary (Best Factors):"),
        tags$table(class = "table table-condensed table-bordered", style = "background-color: #ffffff; color: #000000;",
          tags$thead(tags$tr(tags$th("Locality"), tags$th("Actual"), tags$th("Predicted"))),
          tags$tbody(rows)
        )
    )
  })

    output$idw_metrics_table <- renderTable({
      req(input$method == "IDW", rv$cv_metrics_act)
      m_act <- rv$cv_metrics_act
      if(length(m_act) == 0) return(NULL)
      
      # Correct weighted pooling
      ns <- sapply(m_act, function(x) x$n %||% 0)
      rmses <- sapply(m_act, function(x) x$rmse %||% NA)
      mes <- sapply(m_act, function(x) x$me %||% NA)
      
      total_n <- sum(ns, na.rm=TRUE)
      if(total_n == 0) return(NULL)
      
      avg_rmse <- sqrt(sum(ns * rmses^2, na.rm=TRUE) / total_n)
      avg_me <- sum(ns * mes, na.rm=TRUE) / total_n
      
      data.frame(Metric = c("Mean CV RMSE (Pooled)", "Mean Bias (ME)"), Value = c(round(avg_rmse, 4), round(avg_me, 4)))
    })
  # --- Slider & UI Sync ---
  observeEvent(list(input$locality, rv$user_data, rv$mapping$loc), {
    req(input$locality, rv$user_data, rv$mapping$loc)
    loc_col <- rv$mapping$loc
    locs <- if("ALL" %in% input$locality) unique(rv$user_data[[loc_col]]) else input$locality
    
    # Update selectors for all methods
    update_selector <- function(id, current_locs) {
      current_sel <- isolate(input[[id]])
      if (!identical(sort(as.character(current_locs)), sort(as.character(current_sel)))) {
        updateSelectInput(session, id, choices = current_locs, selected = if(length(current_locs) > 0) current_locs[1] else NULL)
      }
    }
    
    update_selector("m_loc", locs)
    update_selector("idw_m_loc", locs)
    update_selector("tps_m_loc", locs)
  })

  observeEvent(input$vgm_mode, {
    if(input$vgm_mode == "manual") shinyjs::disable("auto_fit") else shinyjs::enable("auto_fit")
  })

  observeEvent(list(input$vgm_mode, input$m_loc, input$comp_mode, input$m_target, rv$v_fit_list), {
    req(input$vgm_mode == "manual", input$m_loc)
    loc <- input$m_loc
    
    target <- if(input$comp_mode && !is.null(input$m_target)) input$m_target else "act"
    fit <- rv$v_fit_list[[paste0(loc, "_", target)]]
    
    if(!is.null(fit)) {
      nugget_val <- fit$psill[1]
      psill_val  <- fit$psill[2]
      range_val  <- fit$range[2]
      
      updateSliderInput(session, "m_nugget", value = nugget_val, max = round(max(nugget_val + psill_val, 0.1), 2))
      updateSliderInput(session, "m_psill", value = psill_val, max = round(max((nugget_val + psill_val) * 1.5, 0.1), 2))
      updateSliderInput(session, "m_range", value = range_val, max = round(max(range_val * 3, 100), 0))
    }
  })

  observeEvent(input$apply_manual, {
    req(input$vgm_mode == "manual", input$m_loc)
    loc <- input$m_loc
    target <- if(input$comp_mode && !is.null(input$m_target)) input$m_target else "act"
    
    rv$v_fit_list[[paste0(loc, "_", target)]] <- vgm(psill = input$m_psill, model = input$k_mod, range = input$m_range, nugget = input$m_nugget)
    showNotification(paste("Manual model applied to", loc, "(", target, ")"), type = "message")
  })

  # --- IDW Manual Sync & Apply ---
  observeEvent(list(input$idw_mode, input$idw_m_loc, input$comp_mode, input$idw_m_target), {
    req(input$idw_mode == "manual", input$idw_m_loc)
    loc <- input$idw_m_loc
    target <- if(input$comp_mode && !is.null(input$idw_m_target)) input$idw_m_target else "act"
    val <- get_regional_param("IDW", loc, target, default = input$idw_p)
    updateSliderInput(session, "idw_m_p", value = val)
  })

  observeEvent(input$apply_idw_manual, {
    req(input$idw_mode == "manual", input$idw_m_loc)
    loc <- input$idw_m_loc
    target <- if(input$comp_mode && !is.null(input$idw_m_target)) input$idw_m_target else "act"
    set_regional_param("IDW", loc, target, input$idw_m_p)
    showNotification(paste("Manual IDW Power applied to", loc, "(", target, ")"), type = "message")
  })

  # --- TPS Manual Sync & Apply ---
  observeEvent(list(input$tps_mode, input$tps_m_loc, input$comp_mode, input$tps_m_target), {
    req(input$tps_mode == "manual", input$tps_m_loc)
    loc <- input$tps_m_loc
    target <- if(input$comp_mode && !is.null(input$tps_m_target)) input$tps_m_target else "act"
    val <- get_regional_param("TPS", loc, target, default = input$tps_lambda)
    updateSliderInput(session, "tps_m_lambda", value = val)
  })

  observeEvent(input$apply_tps_manual, {
    req(input$tps_mode == "manual", input$tps_m_loc)
    loc <- input$tps_m_loc
    target <- if(input$comp_mode && !is.null(input$tps_m_target)) input$tps_m_target else "act"
    set_regional_param("TPS", loc, target, input$tps_m_lambda)
    showNotification(paste("Manual TPS Lambda applied to", loc, "(", target, ")"), type = "message")
  })

  # --- Auto-Fit ---
  observeEvent(input$auto_fit, {
    req(rv$user_data, input$locality, rv$mapping$x, rv$mapping$y)
    locs <- if("ALL" %in% input$locality) unique(rv$user_data[[rv$mapping$loc]]) else input$locality
    meta <- get_current_meta()
    req(meta)
    results <- list()
    rv$loc_names <- locs # Ensure selectors update
    
    withProgress(message = "Optimizing Variograms", {
      current_crs <- rv$mapping$crs
      df_list <- lapply(locs, function(l) {
        sub_a_raw <- rv$user_data %>% filter(!!sym(rv$mapping$loc) == l) %>% 
          select(x = !!sym(rv$mapping$x), y = !!sym(rv$mapping$y), v = !!sym(meta$actual)) %>% 
          na.omit()
        
        sub_p_raw <- NULL
        if(input$comp_mode || input$value_type != "actual") {
          pred_col <- if(input$value_type == "pred_ss") meta$pred_ss else meta$pred
          if (!is.null(pred_col) && pred_col %in% colnames(rv$user_data)) {
            sub_p_raw <- rv$user_data %>% filter(!!sym(rv$mapping$loc) == l) %>% 
              select(x = !!sym(rv$mapping$x), y = !!sym(rv$mapping$y), v = !!sym(pred_col)) %>% 
              na.omit()
          }
        }
        list(l = l, act = sub_a_raw, pre = sub_p_raw)
      })
      
      res_list <- furrr::future_map(df_list, function(item) {
        # Force future to export custom geostatistical functions to parallel sessions
        force_globals <- list(calc_scientific_lags, robust_vgm_fit)
        
        res_a <- list(emp = NULL, fit = NULL, mod = "FAIL", sse = "N/A")
        sub_a_raw <- sf::st_as_sf(item$act, coords=c("x","y"), crs=current_crs)
        sub_a <- validate_and_project_sf(sub_a_raw, current_crs)
        sub_a <- sub_a[!duplicated(round(sf::st_coordinates(sub_a), 2)),]
        
        if(nrow(sub_a) >= 3) {
          lags_a <- calc_scientific_lags(sub_a)
          v_emp_a <- gstat::variogram(v ~ 1, sub_a, width = lags_a$width, cutoff = lags_a$cutoff)
          best_f_a <- robust_vgm_fit(v_emp_a, sub_a$v)
          res_a$emp <- v_emp_a
          res_a$fit <- best_f_a
          res_a$mod <- if(!is.null(best_f_a)) as.character(best_f_a$model[2]) else "FAIL"
          res_a$sse <- if(!is.null(best_f_a)) round(attr(best_f_a, "SSErr") %||% 0, 6) else "N/A"
        }
        
        res_p <- list(emp = NULL, fit = NULL, mod = "FAIL", sse = "N/A")
        if(!is.null(item$pre)) {
          sub_p_raw <- sf::st_as_sf(item$pre, coords=c("x","y"), crs=current_crs)
          sub_p <- validate_and_project_sf(sub_p_raw, current_crs)
          sub_p <- sub_p[!duplicated(round(sf::st_coordinates(sub_p), 2)),]
          
          if(nrow(sub_p) >= 3) {
            lags_p <- calc_scientific_lags(sub_p)
            v_emp_p <- gstat::variogram(v ~ 1, sub_p, width = lags_p$width, cutoff = lags_p$cutoff)
            best_f_p <- robust_vgm_fit(v_emp_p, sub_p$v)
            res_p$emp <- v_emp_p
            res_p$fit <- best_f_p
            res_p$mod <- if(!is.null(best_f_p)) as.character(best_f_p$model[2]) else "FAIL"
            res_p$sse <- if(!is.null(best_f_p)) round(attr(best_f_p, "SSErr") %||% 0, 6) else "N/A"
          }
        }
        
        list(l = item$l, act = res_a, pre = res_p)
      }, .options = furrr::furrr_options(seed = 12345, packages = c("sf", "gstat")))
      
      for(res in res_list) {
        l <- res$l
        if(!is.null(res$act$fit)) {
          rv$v_emp_list[[paste0(l, "_act")]] <- res$act$emp
          rv$v_fit_list[[paste0(l, "_act")]] <- res$act$fit
        }
        if(!is.null(res$pre$fit)) {
          rv$v_emp_list[[paste0(l, "_pre")]] <- res$pre$emp
          rv$v_fit_list[[paste0(l, "_pre")]] <- res$pre$fit
        }
        results[[l]] <- list(
          act_mod = res$act$mod,
          act_sse = res$act$sse,
          pre_mod = res$pre$mod,
          pre_sse = res$pre$sse
        )
      }
    })
    res_tags <- lapply(names(results), function(l) {
      r <- results[[l]]
      txt <- paste0("<b>", l, "</b>: Actual: ", r$act_mod, " (SSE: ", r$act_sse, ")")
      if(input$comp_mode || input$value_type != "actual") {
        txt <- paste0(txt, " | Predicted: ", r$pre_mod, " (SSE: ", r$pre_sse, ")")
      }
      tags$li(HTML(txt))
    })
    showModal(modalDialog(title = "Expert Auto-Fit: Variogram Diagnostics", tags$ul(res_tags), easyClose = TRUE))
  })

  # --- Engine ---

  # B4: Modal dialog for archive choice when previous results exist
  # Unified Archive and Proceed Helper (Issue 3.a and 5.c)
  archive_and_proceed <- function(action, meta, n_locs, estimate_text, is_long_run) {
    if (action == "archive") {
      current_cfg <- rv$run_config_summary
      current_reg <- rv$export_registry
      if (!is.null(current_cfg) && length(current_reg) > 0) {
        rv$run_history <- c(list(list(config = current_cfg, registry = current_reg)), rv$run_history)
      }
    } else if (action == "discard") {
      rv$v_fit_list <- list()
    }
    
    if (is_long_run) {
      showModal(modalDialog(
        title = "Ready to Run Interpolation",
        tags$p("You are about to start the spatial interpolation pipeline with the following parameters:"),
        div(style = "background-color: #f8f9fa; padding: 12px; border-radius: 5px; margin: 10px 0;",
          tags$strong("Method: "), tags$span(input$method), tags$br(),
          tags$strong("Localities: "), tags$span(n_locs), tags$br(),
          tags$strong("Variables: "), tags$span(meta$label)
        ),
        div(style = "background-color: #e8f4fd; color: #1d6fa5; padding: 10px; border-radius: 5px; margin: 10px 0;",
          icon("hourglass-half"), tags$strong(" Run Estimate: "), tags$span(estimate_text)
        ),
        footer = tagList(
          actionButton("confirm_start_run", "Start Interpolation", class = "btn-primary", icon = icon("play")),
          modalButton("Cancel")
        ),
        size = "m", easyClose = FALSE
      ))
    } else {
      rv$proceed_run <- runif(1)
    }
  }

  # B4: Modal dialog for archive choice when previous results exist
  observeEvent(input$run, {
    req(rv$user_data, input$locality, rv$mapping$x, rv$mapping$y)
    
    # UI Guard: check if auxiliary variables are missing for RK/RFK/CK
    if (input$method %in% c("RK", "RFK", "CK") && (is.null(input$aux_vars) || length(input$aux_vars) == 0)) {
      showNotification("Please select at least one auxiliary variable for RK/RFK/CK model generation.", type = "error")
      return()
    }
    
    meta <- get_current_meta()
    req(meta)
    
    # Cost Estimate Calculation (Issue 5.c)
    loc_col <- rv$mapping$loc
    selected_locs <- if ("ALL" %in% input$locality || length(input$locality) == 0) {
      if (!is.null(rv$user_data) && !is.null(loc_col) && loc_col %in% colnames(rv$user_data)) {
        unique(na.omit(rv$user_data[[loc_col]]))
      } else NULL
    } else {
      input$locality
    }
    n_locs <- length(selected_locs)
    if (n_locs == 0) n_locs <- 1
    
    comp_mode <- isTruthy(input$comp_mode) || isTruthy(input$value_type != "actual")
    n_models <- n_locs * (if(comp_mode) 2 else 1)
    
    # Adjust multiplier based on selected interpolation method
    method_mult <- switch(input$method,
      "RFK" = 1.0,
      "RK"  = 1.0,
      "CK"  = 1.5,
      "OK"  = 0.5,
      "IDW" = 0.5,
      "TPS" = 0.3,
      1.0 # default fallback
    )
    
    # Calculate individual locality calculation times to handle the parallel bottleneck correctly
    loc_times_sec <- numeric(n_locs)
    if (!is.null(rv$user_data) && !is.null(loc_col) && loc_col %in% colnames(rv$user_data) && length(selected_locs) > 0) {
      for (idx in seq_along(selected_locs)) {
        l <- selected_locs[idx]
        n_samples <- nrow(rv$user_data[rv$user_data[[loc_col]] == l, ])
        if (is.null(n_samples) || is.na(n_samples) || n_samples == 0) n_samples <- 50
        
        # Base model time: Adaptive 10-Fold CV (N > 50) runs exactly 10 folds, yielding a massive speedup!
        # Small datasets (N <= 50) use traditional LOOCV (2 min for 50 samples).
        if (n_samples > 50) {
          base_sec <- max(10, 15 + (n_samples / 100) * 2.0)
        } else {
          base_sec <- max(10, 120 + (n_samples - 50) * 0.84)
        }
        model_time <- base_sec * method_mult
        
        # In run_regional_interpolation, both Actual and Predicted are run sequentially within the worker
        loc_times_sec[idx] <- model_time * (if (comp_mode) 2 else 1)
      }
    } else {
      # Safe fallback
      loc_times_sec <- rep(120 * method_mult * (if (comp_mode) 2 else 1), n_locs)
    }
    
    # Query parallel workers count
    cores <- tryCatch(future::nbrOfWorkers(), error = function(e) 1)
    if (is.null(cores) || cores < 1) cores <- 1
    
    # Parallel bottleneck estimation (Amdahl's bottleneck):
    # Total calculation is bound by the slowest single locality worker, or the distributed sum divided by cores.
    max_single_loc_time <- max(loc_times_sec)
    distributed_time <- sum(loc_times_sec) / cores
    
    # Select the bottleneck and add a 10% parallel overhead
    est_time_sec <- max(max_single_loc_time, distributed_time) * 1.1
    
    est_time_str <- if (est_time_sec < 60) {
      paste(round(est_time_sec), "seconds")
    } else {
      paste(round(est_time_sec / 60, 1), "minutes")
    }
    estimate_text <- paste0("~", n_models, " locality model(s), ~", est_time_str, " estimated")
    is_long_run <- est_time_sec >= 120 || input$method %in% c("RK", "RFK", "CK")
    
    # If there are previous results, check remember settings or ask user
    if (!is.null(rv$run_config_summary) && length(rv$export_registry) > 0) {
      if (rv$auto_archive_choice == "archive") {
        archive_and_proceed("archive", meta, n_locs, estimate_text, is_long_run)
      } else if (rv$auto_archive_choice == "discard") {
        archive_and_proceed("discard", meta, n_locs, estimate_text, is_long_run)
      } else {
        showModal(modalDialog(
          title = "Previous Results Detected",
          tags$p("A previous model run exists. What would you like to do with those results?"),
          div(style = "background-color: #f8f9fa; padding: 10px; border-radius: 5px; margin: 10px 0;",
            tags$strong(paste0("Run #", rv$run_config_summary$run_id, ": ",
              rv$run_config_summary$variable, " (", rv$run_config_summary$method, ")")),
            tags$br(),
            tags$small(paste0(rv$run_config_summary$localities, " | ",
              format(rv$run_config_summary$timestamp, "%H:%M:%S")))
          ),
          div(style = "background-color: #e8f4fd; color: #1d6fa5; padding: 10px; border-radius: 5px; margin: 10px 0;",
            icon("hourglass-half"), tags$strong(" Run Estimate: "), tags$span(estimate_text)
          ),
          checkboxInput("auto_archive_remember", "Remember my choice (apply automatically for future runs)", FALSE),
          footer = tagList(
            actionButton("archive_prev_run", "Archive & Continue", class = "btn-warning", icon = icon("archive")),
            actionButton("discard_prev_run", "Discard & Continue", class = "btn-danger", icon = icon("trash")),
            modalButton("Cancel")
          ),
          size = "m", easyClose = FALSE
        ))
      }
    } else {
      # No previous results, proceed directly
      archive_and_proceed("none", meta, n_locs, estimate_text, is_long_run)
    }
  })

  # B4: Archive previous results then proceed
  observeEvent(input$archive_prev_run, {
    removeModal()
    if (isTRUE(input$auto_archive_remember)) {
      rv$auto_archive_choice <- "archive"
    }
    
    meta <- get_current_meta()
    loc_col <- rv$mapping$loc
    n_locs <- if ("ALL" %in% input$locality || length(input$locality) == 0) {
      if (!is.null(rv$user_data) && !is.null(loc_col) && loc_col %in% colnames(rv$user_data)) {
        length(unique(na.omit(rv$user_data[[loc_col]])))
      } else 1
    } else {
      length(input$locality)
    }
    comp_mode <- isTruthy(input$comp_mode) || isTruthy(input$value_type != "actual")
    n_models <- n_locs * (if(comp_mode) 2 else 1)
    sec_per_model <- 1.5
    est_time_sec <- n_models * sec_per_model
    est_time_str <- if (est_time_sec < 60) paste(round(est_time_sec), "seconds") else paste(round(est_time_sec / 60, 1), "minutes")
    estimate_text <- paste0("~", n_models, " locality model(s), ~", est_time_str, " estimated")
    is_long_run <- n_models >= 3 || input$method %in% c("RK", "RFK", "CK")
    
    archive_and_proceed("archive", meta, n_locs, estimate_text, is_long_run)
  })

  # B4: Discard previous results then proceed
  observeEvent(input$discard_prev_run, {
    removeModal()
    if (isTRUE(input$auto_archive_remember)) {
      rv$auto_archive_choice <- "discard"
    }
    
    meta <- get_current_meta()
    loc_col <- rv$mapping$loc
    n_locs <- if ("ALL" %in% input$locality || length(input$locality) == 0) {
      if (!is.null(rv$user_data) && !is.null(loc_col) && loc_col %in% colnames(rv$user_data)) {
        length(unique(na.omit(rv$user_data[[loc_col]])))
      } else 1
    } else {
      length(input$locality)
    }
    comp_mode <- isTruthy(input$comp_mode) || isTruthy(input$value_type != "actual")
    n_models <- n_locs * (if(comp_mode) 2 else 1)
    sec_per_model <- 1.5
    est_time_sec <- n_models * sec_per_model
    est_time_str <- if (est_time_sec < 60) paste(round(est_time_sec), "seconds") else paste(round(est_time_sec / 60, 1), "minutes")
    estimate_text <- paste0("~", n_models, " locality model(s), ~", est_time_str, " estimated")
    is_long_run <- n_models >= 3 || input$method %in% c("RK", "RFK", "CK")
    
    archive_and_proceed("discard", meta, n_locs, estimate_text, is_long_run)
  })

  # B4: Confirm start calculation observer
  observeEvent(input$confirm_start_run, {
    removeModal()
    rv$proceed_run <- runif(1)
  })

  output$reset_archive_choice_ui <- renderUI({
    if (rv$auto_archive_choice != "none") {
      actionButton("reset_archive_choice", "Reset Auto-Archive Decision", class = "btn-secondary btn-sm", style = "width: 100%; margin-top: 10px;", icon = icon("sync-alt"))
    } else {
      NULL
    }
  })

  observeEvent(input$reset_archive_choice, {
    rv$auto_archive_choice <- "none"
    showNotification("Auto-archive/discard setting has been reset. You will be prompted for future runs.", type = "message")
  })

  # Main model generation logic (triggered after archive decision)
  observeEvent(rv$proceed_run, {
    req(rv$user_data, input$locality, rv$mapping$x, rv$mapping$y);
    meta <- get_current_meta()
    req(meta)

    # Validate coordinate mapping and numeric type correctness
    x_col_name <- rv$mapping$x
    y_col_name <- rv$mapping$y
    
    if (is.null(x_col_name) || is.null(y_col_name) || !(x_col_name %in% colnames(rv$user_data)) || !(y_col_name %in% colnames(rv$user_data))) {
      showModal(modalDialog(
        title = tags$div(style = "color: #d9534f; font-weight: bold;", icon("exclamation-triangle"), "Coordinate Mapping Error"),
        tags$p("The selected coordinate columns (X, Y) do not exist in the dataset. Please verify your variable mapping in the setup tab."),
        easyClose = TRUE,
        footer = modalButton("Dismiss")
      ))
      return()
    }
    
    x_vals <- rv$user_data[[x_col_name]]
    y_vals <- rv$user_data[[y_col_name]]
    x_num <- suppressWarnings(as.numeric(as.character(x_vals)))
    y_num <- suppressWarnings(as.numeric(as.character(y_vals)))
    
    valid_xy_count <- sum(!is.na(x_num) & !is.na(y_num))
    
    if (valid_xy_count < 3) {
      showModal(modalDialog(
        title = tags$div(style = "color: #d9534f; font-weight: bold;", icon("exclamation-triangle"), "Invalid Coordinate Data"),
        tags$p("The selected coordinate columns (X, Y) do not contain sufficient valid numeric values."),
        tags$p(paste0("Total rows with valid numeric coordinates: ", valid_xy_count, " (minimum 3 required).")),
        tags$p("Please verify that your selected coordinate columns are strictly numeric and contain no missing values (NAs) or text."),
        easyClose = TRUE,
        footer = modalButton("Dismiss")
      ))
      return()
    }
    
    # Validate covariates if using Regression Kriging (RK), RFK, or CK
    current_method <- input$method
    aux_vars <- input$aux_vars
    if (current_method %in% c("RK", "RFK", "CK") && length(aux_vars) > 0) {
      missing_vars <- setdiff(aux_vars, colnames(rv$user_data))
      if (length(missing_vars) > 0) {
        showModal(modalDialog(
          title = tags$div(style = "color: #d9534f; font-weight: bold;", icon("exclamation-triangle"), "Missing Covariates"),
          tags$p("The following selected covariates do not exist in the dataset:"),
          tags$pre(style = "background-color: #f8d7da; color: #721c24; border: 1px solid #f5c6cb; padding: 10px; border-radius: 4px;", paste(missing_vars, collapse = ", ")),
          easyClose = TRUE,
          footer = modalButton("Dismiss")
        ))
        return()
      }
      
      non_numeric_vars <- c()
      for (v in aux_vars) {
        v_vals <- suppressWarnings(as.numeric(as.character(rv$user_data[[v]])))
        if (sum(!is.na(v_vals)) < 3) {
          non_numeric_vars <- c(non_numeric_vars, v)
        }
      }
      
      if (length(non_numeric_vars) > 0) {
        showModal(modalDialog(
          title = tags$div(style = "color: #d9534f; font-weight: bold;", icon("exclamation-triangle"), "Non-Numeric Covariates"),
          tags$p("The following selected covariates do not contain sufficient valid numeric values (minimum 3 required):"),
          tags$pre(style = "background-color: #f8d7da; color: #721c24; border: 1px solid #f5c6cb; padding: 10px; border-radius: 4px;", paste(non_numeric_vars, collapse = ", ")),
          easyClose = TRUE,
          footer = modalButton("Dismiss")
        ))
        return()
      }
    }

    locs <- if("ALL" %in% input$locality) unique(rv$user_data[[rv$mapping$loc]]) else input$locality

    # If the method has changed since the last run, clear the variogram fits
    if (!is.null(rv$run_config_summary) && rv$run_config_summary$method != input$method) {
      rv$v_fit_list <- list()
    }

    # Automatically switch to Map Viewer tab to show progress
    updateTabsetPanel(session, "main_tabs", selected = "tab_map")

    # Disable execution button and show spin on it
    shinyjs::disable("run")
    updateActionButton(session, "run", label = "Interpolating...", icon = icon("spinner", class = "fa-spin"))

    # Show progress overlay, hide reveal button, and show progress widgets
    shinyjs::show("map_processing_overlay")
    shinyjs::show("map_spinner")
    shinyjs::show("map_progress_bar_container")
    shinyjs::show("cancel_model_btn")
    shinyjs::hide("reveal_maps_btn")
    shinyjs::html("map_processing_title", "Processing...")
    update_premium_progress(5, "Initializing Spatial Analysis Engine...")

    # Wipe any old progress files and start the timer
    cancel_file <- file.path(session_progress_dir, "cancel_flag.txt")
    if (file.exists(cancel_file)) tryCatch(file.remove(cancel_file), error = function(e) NULL)
    old_files <- list.files(path = session_progress_dir, pattern = paste0("^progress_", session_id, "_.*_.*\\.txt$"), full.names = TRUE)
    if (length(old_files) > 0) tryCatch(file.remove(old_files), error = function(e) NULL)
    rv$model_running <- TRUE

    # B3: Capture run configuration before clearing
    rv$run_counter <- rv$run_counter + 1L
    method_params_list <- list(
      "IDW" = paste0("IDW Power: ", input$idw_p, " | Nmax: ", input$idw_nmax),
      "TPS" = paste0("TPS Lambda: ", input$tps_lambda),
      "OK"  = "Ordinary Kriging (auto variogram)",
      "RK"  = paste0("Regression Kriging | Aux: ", paste(input$aux_vars, collapse=", ")),
      "RFK" = paste0("Random Forest Kriging | Aux: ", paste(input$aux_vars, collapse=", ")),
      "CK"  = paste0("Co-Kriging | Aux: ", paste(input$aux_vars, collapse=", "))
    )
    method_params_str <- method_params_list[[input$method]] %||% ""
    rv$run_config_summary <- list(
      run_id = rv$run_counter,
      timestamp = Sys.time(),
      variable = paste0(meta$label, " [", meta$actual, "]"),
      method = input$method,
      localities = paste(locs, collapse = ", "),
      subset = input$subset,
      value_type = input$value_type,
      crs = rv$mapping$crs,
      boundary_type = input$boundary_type,
      buffer_mode = input$buff_mode,
      buffer_dist = input$buff_dist,
      resolution = input$grid_res,
      res_mode = input$res_mode,
      comp_mode = input$comp_mode,
      sep_fit = input$sep_fit,
      method_params = method_params_str
    )

    tryCatch({
      # Clear current results for fresh run
      rv$export_registry <- list()
      rv$rast_list_act <- list(); rv$rast_list_pre <- list(); sf_list <- list(); b_list <- list()
      rv$rast <- NULL; rv$rast_pred <- NULL; rv$rast_res <- NULL; rv$has_predictions <- FALSE
    rv$v_emp_list <- list(); rv$log <- paste0("[Run #", rv$run_counter, "] Starting spatial interpolation using method: ", input$method, "...")
    rv$run_method[[input$var_id]] <- input$method
    rv$model_summaries <- list(); rv$rf_models <- list(); rv$gstat_objs <- list()
    rv$cv_metrics_act <- list(); rv$cv_metrics_pre <- list() # Reset CV metrics
    rv$cv_data_act <- list(); rv$cv_data_pre <- list()
    
    update_premium_progress(15, "Validating and Cleaning Spatial Input Data...")
    
    pred_col <- if(input$value_type == "pred_ss") meta$pred_ss else meta$pred
    aux_vars <- input$aux_vars
    
    update_premium_progress(25, "Preparing Neighborhood Search Grids...")
    
    # Pre-extract reactive states
    current_method <- input$method
    current_crs <- rv$mapping$crs
    current_loc_col <- rv$mapping$loc
    current_x_col <- rv$mapping$x
    current_y_col <- rv$mapping$y
    val_type <- input$value_type
    subset_val <- input$subset
    actual_col <- meta$actual
    b_type <- input$boundary_type
    buff_mode <- input$buff_mode
    b_dist <- input$buff_dist
    shp_bound <- rv$shp_bound
    res_mode <- input$res_mode
    grid_res <- input$grid_res
    crs_sel <- input$crs_selection
    
    # Strict CRS Sanitization and Validation
    safe_crs <- validate_crs(crs_sel, "CRS Validation Error:", duration = 15)
    req(safe_crs)
    
    comp_mode <- input$comp_mode
    sep_fit <- input$sep_fit
    idw_p_val <- input$idw_p
    idw_nmax_val <- input$idw_nmax
    tps_lambda_val <- input$tps_lambda
    
    update_premium_progress(35, "Organizing Localized Data Chunks...")
    
    df_list <- lapply(locs, function(l) {
      sub_df <- rv$user_data %>% filter(!!sym(current_loc_col) == l)
      if (val_type == "pred_ss" && "subset" %in% colnames(sub_df) && subset_val != "all") {
        sub_df <- sub_df %>% filter(subset == subset_val)
      }
      
      pts_data <- sub_df
      pts_data$x <- sub_df[[current_x_col]]
      pts_data$y <- sub_df[[current_y_col]]
      pts_data$v <- sub_df[[actual_col]]
      pts_data$pv <- if (!is.null(pred_col) && pred_col %in% colnames(sub_df)) sub_df[[pred_col]] else NA
      
      m_params <- list(
        idw_p_act = get_regional_param("IDW", l, "act", default = idw_p_val %||% 2),
        idw_p_pre = get_regional_param("IDW", l, "pre", default = idw_p_val %||% 2),
        idw_nmax = idw_nmax_val %||% 12,
        tps_lambda_act = get_regional_param("TPS", l, "act", default = tps_lambda_val),
        tps_lambda_pre = get_regional_param("TPS", l, "pre", default = tps_lambda_val),
        pre_fit_act = clean_gstat_env(rv$v_fit_list[[paste0(l, "_act")]]),
        pre_fit_pre = clean_gstat_env(if(sep_fit) rv$v_fit_list[[paste0(l, "_pre")]] else rv$v_fit_list[[paste0(l, "_act")]])
      )
      
      list(l = l, pts_data = pts_data, m_params = m_params)
    })

    update_premium_progress(50, "Executing Parallel Interpolation Algorithms...")

    # C1: Clear rast lists before starting new run to prevent spatial interference
    rv$rast_list_act <- list(); rv$rast_list_pre <- list(); rv$rast_list_res <- list(); rv$rast_list_point_res <- list()

    main_wd <- getwd()
    progress_dir_val <- session_progress_dir
    session_id_val <- session_id
    cancel_file_val <- file.path(session_progress_dir, "cancel_flag.txt")

    promises::future_promise({
      # Dynamically source helpers to guarantee 100% function availability on the worker
      source("spatial_helpers_0.9.8b.R", local = FALSE)
      
      # Force future to export custom geostatistical functions to parallel sessions
      force_globals <- list(
        run_regional_interpolation, calc_scientific_lags, robust_vgm_fit, 
        apply_interpolation, apply_OK, apply_RK, apply_RFK, apply_CK, 
        apply_IDW, apply_TPS, perform_kriging_loocv, safe_run_cv, 
        optimize_idw_p, get_regional_param, clean_gstat_env,
        apply_kriging_pipeline, check_vif, krige_covariates, get_buffer_multiplier,
        sanitize_spatial_predictions, validate_and_project_sf, suggest_lmc_model,
        .cv_to_df, detect_cv_columns
      )
      
      res_all <- furrr::future_map(df_list, function(item) {
        # Also reference inside the nested map to ensure worker propagation
        force_globals_nested <- list(
          run_regional_interpolation, calc_scientific_lags, robust_vgm_fit, 
          apply_interpolation, apply_OK, apply_RK, apply_RFK, apply_CK, 
          apply_IDW, apply_TPS, perform_kriging_loocv, safe_run_cv, 
          optimize_idw_p, get_regional_param, clean_gstat_env,
          apply_kriging_pipeline, check_vif, krige_covariates, get_buffer_multiplier,
          sanitize_spatial_predictions, validate_and_project_sf, suggest_lmc_model,
          .cv_to_df, detect_cv_columns
        )
        
        run_regional_interpolation(
          item = item,
          current_method = current_method,
          current_crs = current_crs,
          aux_vars = aux_vars,
          shp_bound = shp_bound,
          b_type = b_type,
          buff_mode = buff_mode,
          b_dist = b_dist,
          res_mode = res_mode,
          grid_res = grid_res,
          crs_sel = crs_sel,
          comp_mode = comp_mode,
          val_type = val_type,
          progress_dir_val = progress_dir_val,
          session_id_val = session_id_val,
          cancel_file_val = cancel_file_val
        )
      }, .options = furrr::furrr_options(
        seed = 12345,
        globals = c(
          "run_regional_interpolation", "calc_scientific_lags", "robust_vgm_fit",
          "apply_interpolation", "apply_OK", "apply_RK", "apply_RFK", "apply_CK",
          "apply_IDW", "apply_TPS", "perform_kriging_loocv", "safe_run_cv",
          "optimize_idw_p", "get_regional_param", "clean_gstat_env",
          "apply_kriging_pipeline", "check_vif", "krige_covariates", "get_buffer_multiplier",
          "sanitize_spatial_predictions", "validate_and_project_sf", "suggest_lmc_model",
          ".cv_to_df", "detect_cv_columns"
        )
      ))
      return(res_all)
    }, seed = 12345) %...>% (function(res_all) {
      
      # Aggregate results back to rv
      for(res in res_all) {
          l <- res$l
          if(res$log_msg != "") {
              rv$log <- paste0(rv$log, res$log_msg)
              if(grepl("Error", res$log_msg)) {
                showNotification(paste("Error in region:", l, "-", res$log_msg), type = "error", duration = 15)
                showModal(modalDialog(
                  title = tags$div(style = "color: #d9534f; font-weight: bold;", icon("exclamation-circle"), paste("Region Error:", l)),
                  tags$p("An error occurred during modeling of locality: ", tags$b(l)),
                  tags$pre(style = "background-color: #f8d7da; color: #721c24; border: 1px solid #f5c6cb; padding: 15px; border-radius: 4px; overflow-x: auto; white-space: pre-wrap; font-family: monospace; font-size: 0.9em;", res$log_msg),
                  easyClose = TRUE,
                  footer = modalButton("Dismiss")
                ))
              }
          }
          if(!is.null(res$r_a)) rv$rast_list_act[[l]] <- res$r_a
          if(!is.null(res$r_p)) rv$rast_list_pre[[l]] <- res$r_p
          if(!is.null(res$r_res)) rv$rast_list_res[[l]] <- res$r_res
          if(!is.null(res$r_point_err)) rv$rast_list_point_res[[l]] <- res$r_point_err
          if(!is.null(res$bound)) b_list[[length(b_list)+1]] <- res$bound
          if(!is.null(res$pts)) sf_list[[length(sf_list)+1]] <- res$pts
          
          if(!is.null(res$v_emp_act)) rv$v_emp_list[[paste0(l, "_act")]] <- res$v_emp_act
          if(!is.null(res$v_fit_act)) rv$v_fit_list[[paste0(l, "_act")]] <- res$v_fit_act
          if(!is.null(res$cv_act)) rv$cv_metrics_act[[l]] <- res$cv_act
          if(!is.null(res$cv_obj_act)) rv$cv_data_act[[l]] <- res$cv_obj_act
          if(!is.null(res$summ_act)) rv$model_summaries[[paste0(l, "_act")]] <- res$summ_act
          if(!is.null(res$rf_act)) rv$rf_models[[paste0(l, "_act")]] <- res$rf_act
          if(!is.null(res$gstat_act)) rv$gstat_objs[[paste0(l, "_act")]] <- res$gstat_act
          
          if(!is.null(res$v_emp_pre)) rv$v_emp_list[[paste0(l, "_pre")]] <- res$v_emp_pre
          if(!is.null(res$v_fit_pre)) rv$v_fit_list[[paste0(l, "_pre")]] <- res$v_fit_pre
          if(!is.null(res$cv_pre)) rv$cv_metrics_pre[[l]] <- res$cv_pre
          if(!is.null(res$cv_obj_pre)) rv$cv_data_pre[[l]] <- res$cv_obj_pre
          if(!is.null(res$summ_pre)) rv$model_summaries[[paste0(l, "_pre")]] <- res$summ_pre
          if(!is.null(res$rf_pre)) rv$rf_models[[paste0(l, "_pre")]] <- res$rf_pre
          if(!is.null(res$gstat_pre)) rv$gstat_objs[[paste0(l, "_pre")]] <- res$gstat_pre
      }
    
    valid_a <- Filter(Negate(is.null), rv$rast_list_act)
    valid_p <- Filter(Negate(is.null), rv$rast_list_pre)
    valid_r <- Filter(Negate(is.null), rv$rast_list_res)
    valid_pr <- Filter(Negate(is.null), rv$rast_list_point_res)
    
    if(length(valid_a) > 0) {
      rv$rast <- merge_wrapped_rasters(valid_a)
      register_export_item("map_actual", paste(meta$label, "- Actual Map"), "map", rv$rast, meta$category)
    }
    if(length(valid_p) > 0) {
      rv$rast_pred <- merge_wrapped_rasters(valid_p)
      rv$has_predictions <- TRUE
      register_export_item("map_predicted", paste(meta$label, "- Predicted Map"), "map", rv$rast_pred, meta$category)
    }
    if(length(valid_r) > 0) {
      rv$rast_res <- merge_wrapped_rasters(valid_r)
      register_export_item("map_residuals", paste(meta$label, "- Residual Map (Delta)"), "map", rv$rast_res, meta$category)
    }
    if(length(valid_pr) > 0) {
      rv$rast_point_res <- merge_wrapped_rasters(valid_pr)
      register_export_item("map_point_residuals", paste(meta$label, "- Point Error Map"), "map", rv$rast_point_res, meta$category)
    }
    
    # NEW: Register Combined Comparison Map
    if(!is.null(rv$rast) && !is.null(rv$rast_pred)) {
       register_export_item("map_comparison", paste(meta$label, "- Actual vs Predicted Comparison"), "map_combined", list(act = rv$rast, pre = rv$rast_pred), meta$category)
    }
    
    if(length(sf_list) > 0) {
      target_crs <- sf::st_crs(sf_list[[1]])
      sf_list_aligned <- lapply(sf_list, function(x) {
        if (sf::st_crs(x) != target_crs) {
          sf::st_transform(x, target_crs)
        } else {
          x
        }
      })
      rv$sf <- do.call(rbind, sf_list_aligned)
    }
    # Filter and align CRS for boundaries in b_list
    valid_bounds <- Filter(function(x) !is.null(x) && inherits(x, "sf"), b_list)
    if(length(valid_bounds) > 0) {
      target_crs_b <- sf::st_crs(valid_bounds[[1]])
      b_list_aligned <- lapply(valid_bounds, function(x) {
        if (sf::st_crs(x) != target_crs_b) {
          sf::st_transform(x, target_crs_b)
        } else {
          x
        }
      })
      rv$bound <- do.call(rbind, unname(b_list_aligned)) %>% sf::st_union()
    }
    rv$loc_names <- names(valid_a)
    
    # Automatically zoom/pan maps to the active boundary bounding box when the run completes
    if (!is.null(rv$bound)) {
      tryCatch({
        bbox <- sf::st_bbox(sf::st_transform(sf::st_as_sf(rv$bound), 4326))
        leafletProxy("main_map") %>% fitBounds(as.numeric(bbox$xmin), as.numeric(bbox$ymin), as.numeric(bbox$xmax), as.numeric(bbox$ymax))
        if (isTRUE(input$comp_mode)) {
          leafletProxy("comp_map_left") %>% fitBounds(as.numeric(bbox$xmin), as.numeric(bbox$ymin), as.numeric(bbox$xmax), as.numeric(bbox$ymax))
          leafletProxy("comp_map_right") %>% fitBounds(as.numeric(bbox$xmin), as.numeric(bbox$ymin), as.numeric(bbox$xmax), as.numeric(bbox$ymax))
        }
      }, error = function(e) NULL)
    }
    
    # Global Metrics (Standard RMSE/R2)
    if(!is.null(rv$sf)) {
      df_met <- rv$sf %>% st_drop_geometry() %>% filter(!is.na(v), !is.na(pv))
      if(nrow(df_met) > 0) {
        resids <- df_met$v - df_met$pv
        rmse_val <- sqrt(mean(resids^2))
        r2_val <- cor(df_met$v, df_met$pv)^2
        mbe_val <- mean(df_met$pv - df_met$v)
        nse_val <- 1 - sum(resids^2) / sum((df_met$v - mean(df_met$v))^2)
        
        rv$metrics <- data.frame(
          Metric = c("RMSE (Avg Error)", "R2 (Correlation)", "R2 (Traditional)", "MBE (Bias)"), 
          Value = c(round(rmse_val, 4), round(r2_val, 4), round(nse_val, 4), round(mbe_val, 4))
        )
        register_export_item("table_global_metrics", paste(meta$label, "- Global Performance Metrics"), "table", rv$metrics, meta$category)
      } else {
        rv$metrics <- data.frame(Metric = c("RMSE (Avg Error)", "R2 (Correlation)", "R2 (Traditional)", "MBE (Bias)"), Value = c(NA, NA, NA, NA))
      }
    }
    # --- REGISTRY: Register Total Statistics ---
    meta <- get_current_meta()
    
    # 1. Total Performance Metrics (Uploaded Data)
    if(!is.null(rv$sf)) {
      df_perf <- rv$sf %>% st_drop_geometry() %>% filter(!is.na(v), !is.na(pv))
      if(nrow(df_perf) >= 3) {
        perf_total <- data.frame(
          Metric = c("R2 (Trad)", "R2 (Corr)", "RMSE", "MBE (Bias)", "CCC", "RPD"),
          Value = c(
            round(yardstick::rsq_trad_vec(df_perf$v, df_perf$pv), 4),
            round(yardstick::rsq_vec(df_perf$v, df_perf$pv), 4),
            round(yardstick::rmse_vec(df_perf$v, df_perf$pv), 4),
            round(mean(df_perf$pv - df_perf$v, na.rm=TRUE), 4),
            round(yardstick::ccc_vec(df_perf$v, df_perf$pv), 4),
            round(yardstick::rpd_vec(df_perf$v, df_perf$pv), 4)
          )
        )
        register_export_item("table_perf_uploaded_total", paste(meta$label, "- Total Prediction Performance"), "table", perf_total, meta$category)
      }
      
      # 2. Total Descriptive Statistics (Actual)
      v_all <- rv$sf$v[!is.na(rv$sf$v)]
      if(length(v_all) > 0) {
        s_a <- summary(v_all)
        stats_total <- data.frame(Metric = names(s_a), Value = as.character(round(as.numeric(s_a), 3)))
        register_export_item("table_stats_total", paste(meta$label, "- Total Descriptive Statistics (Actual)"), "table", stats_total, meta$category)
      }
      
      # 2.5 Total Descriptive Statistics (Predicted)
      if(comp_mode || val_type != "actual") {
        pv_all <- rv$sf$pv[!is.na(rv$sf$pv)]
        if(length(pv_all) > 0) {
          s_p <- summary(pv_all)
          stats_total_p <- data.frame(Metric = names(s_p), Value = as.character(round(as.numeric(s_p), 3)))
          register_export_item("table_stats_pre_total", paste(meta$label, "- Total Descriptive Statistics (Predicted)"), "table", stats_total_p, meta$category)
        }
      }
      
      # 3. Total Classification Performance (Kappa)
      # Using default 'agro' binning for registration
      params_k <- tryCatch(agro_params(), condition = function(c) NULL)
      if(!is.null(params_k) && (comp_mode || val_type != "actual")) {
        df_k <- df_perf
        brks_k <- c(-Inf, params_k$rcl_mat[-1, 1], Inf)
        df_k$act_bin <- cut(df_k$v, breaks = brks_k, labels = params_k$labels, include.lowest = TRUE)
        df_k$pred_bin <- cut(df_k$pv, breaks = brks_k, labels = params_k$labels, include.lowest = TRUE)
        df_k <- df_k[!is.na(df_k$act_bin) & !is.na(df_k$pred_bin), ]
        if(nrow(df_k) >= 3) {
          kappa_total <- data.frame(
            Metric = c("Accuracy", "Kappa (Unweighted)", "Weighted Kappa (Linear)", "MCC"),
            Value = c(
              round(yardstick::accuracy_vec(df_k$act_bin, df_k$pred_bin), 4),
              round(yardstick::kap_vec(df_k$act_bin, df_k$pred_bin), 4),
              round(yardstick::kap_vec(df_k$act_bin, df_k$pred_bin, weighting = "linear"), 4),
              round(yardstick::mcc_vec(df_k$act_bin, df_k$pred_bin), 4)
            )
          )
          register_export_item("table_kappa_total", paste(meta$label, "- Total Classification Performance"), "table", kappa_total, meta$category)
        }
      }
    }

    # 4. Total Area Coverage (if Agro)
    if(isTruthy(input$color_style == "agro") && !is.null(rv$rast)) {
       area_total <- area_df_total_act()
       if(is.data.frame(area_total)) register_export_item("table_area_total", paste(meta$label, "- Total Area Coverage"), "table", area_total, meta$category)
    }

    # --- REGISTRY: Per-Locality Assets ---
    # Per-Locality Assets Registration
    for(l in locs) {
       register_locality_assets(l, meta, comp_mode, val_type)
    }
    
    rv$log <- paste0(rv$log, "\n\n--- Run #", rv$run_counter, " Complete ---",
      "\nConfig: ", rv$run_config_summary$method, " | ", rv$run_config_summary$variable,
      " | ", rv$run_config_summary$localities,
      "\n", rv$run_config_summary$method_params)

    shinyjs::hide("map_spinner")
    shinyjs::html("map_processing_title", "Map Generation Complete")
    update_premium_progress(100, "Click below to reveal the updated geostatistical surfaces.")
    shinyjs::show("reveal_maps_btn")
    
    # Enable execution button and change state to Interpolated
    shinyjs::enable("run")
    updateActionButton(session, "run", label = "Interpolated", icon = icon("check"))
    
    # Wipe the polling state and remove temporary files
    rv$model_running <- FALSE
    old_files <- list.files(path = session_progress_dir, pattern = paste0("^(progress|warn)_", session_id, "_.*_.*\\.txt$"), full.names = TRUE)
    if(length(old_files) > 0) tryCatch(file.remove(old_files), error = function(e) NULL)
    }) %...!% (function(err) {
      shinyjs::hide("map_spinner")
      shinyjs::hide("map_progress_bar_container")
      shinyjs::hide("cancel_model_btn")
      shinyjs::hide("reveal_maps_btn")
      
      # Enable execution button and reset state
      shinyjs::enable("run")
      updateActionButton(session, "run", label = "Run Interpolation", icon = NULL)
      shinyjs::runjs("$('#run i').remove();")
      
      if (grepl("cancelled", tolower(err$message))) {
        shinyjs::html("map_processing_title", "Interpolation Cancelled")
        shinyjs::html("map_progress_text", HTML("Please configure parameters in the left panel and click <b>'Run Interpolation'</b> to generate geostatistical maps and review diagnostic results."))
      } else {
        shinyjs::html("map_processing_title", "Interpolation Failed")
        shinyjs::html("map_progress_text", "An error occurred during parallel modeling. Please check the error message and click 'Run Interpolation' to try again.")
        showModal(modalDialog(
          title = tags$div(style = "color: #d9534f; font-weight: bold;", icon("exclamation-triangle"), "Parallel Interpolation Failed"),
          tags$p("An error occurred while executing the parallel interpolation algorithms:"),
          tags$pre(style = "background-color: #f8d7da; color: #721c24; border: 1px solid #f5c6cb; padding: 15px; border-radius: 4px; overflow-x: auto; white-space: pre-wrap; font-family: monospace; font-size: 0.9em;", err$message),
          tags$p(style = "margin-top: 15px; font-weight: bold;", "Recommended Troubleshooting Steps:"),
          tags$ul(
            tags$li("Verify that your selected coordinate columns (X, Y) are strictly numeric and contain no missing values (NAs)."),
            tags$li("Check for highly collinear covariates if using Regression Kriging (RK) or RFK. Try removing redundant variables."),
            tags$li("Ensure you have at least 3-5 unique data points per locality region to allow variogram fitting.")
          ),
          easyClose = TRUE,
          footer = modalButton("Dismiss")
        ))
      }
      
      # Wipe the polling state and remove temporary files on failure
      rv$model_running <- FALSE
      old_files <- list.files(path = session_progress_dir, pattern = paste0("^(progress|warn)_", session_id, "_.*_.*\\.txt$"), full.names = TRUE)
      if(length(old_files) > 0) tryCatch(file.remove(old_files), error = function(e) NULL)
    })
    
    }, error = function(e) {
      shinyjs::hide("map_spinner")
      shinyjs::hide("map_progress_bar_container")
      shinyjs::hide("cancel_model_btn")
      shinyjs::html("map_processing_title", "Interpolation Failed")
      shinyjs::html("map_progress_text", paste("Model preparation failed:", e$message))
      showNotification(paste("Model preparation failed:", e$message), type = "error")
      rv$model_running <- FALSE
      
      # Enable execution button and reset state
      shinyjs::enable("run")
      updateActionButton(session, "run", label = "Run Interpolation", icon = NULL)
      shinyjs::runjs("$('#run i').remove();")
    })
    
    # CRITICAL: Return NULL so Shiny does NOT lock the session waiting for the promise.
    # If the promise is the last expression, Shiny treats it as "wait for this before allowing interaction".
    NULL
  })

  # Step 2: Implement the dynamic reactiveTimer polling observer
  observe({
    req(rv$model_running)
    n_locs <- 1
    if (!is.null(rv$sf) && "loc" %in% colnames(rv$sf)) {
      n_locs <- length(unique(rv$sf$loc))
    } else if (!is.null(input$locality) && !("ALL" %in% input$locality) && length(input$locality) > 0) {
      n_locs <- length(input$locality)
    }
    poll_interval <- if (n_locs < 5) 250 else if (n_locs <= 20) 1000 else 2000
    invalidateLater(poll_interval)
    
    # Calculate the total expected number of models to prevent progress bar jumps
    loc_col <- rv$mapping$loc
    selected_locs <- if ("ALL" %in% input$locality || length(input$locality) == 0) {
      if (!is.null(rv$user_data) && !is.null(loc_col) && loc_col %in% colnames(rv$user_data)) {
        unique(na.omit(rv$user_data[[loc_col]]))
      } else NULL
    } else {
      input$locality
    }
    n_locs_calc <- length(selected_locs)
    if (n_locs_calc == 0) n_locs_calc <- 1
    comp_mode <- isTruthy(input$comp_mode) || isTruthy(input$value_type != "actual")
    expected_models <- n_locs_calc * (if(comp_mode) 2 else 1)
    
    files <- list.files(path = session_progress_dir, pattern = paste0("^progress_", session_id, "_.*_.*\\.txt$"), full.names = TRUE)
    if(length(files) > 0) {
      vals <- vapply(files, function(f) {
        val <- tryCatch(as.numeric(readLines(f, warn = FALSE)), error = function(e) NA_real_)
        if(length(val) == 0 || is.na(val)) 0 else val
      }, numeric(1))
      
      # Sum up the percentages of all files and divide by total expected models
      # Unstarted models implicitly contribute 0% progress, preventing backward jumps
      avg_pct <- sum(vals, na.rm = TRUE) / expected_models
      
      # Scale progress between 50% (start of math loops) and 100% (done)
      bar_width <- 50 + (avg_pct * 0.5)
      bar_width <- max(50, min(99, bar_width)) # Cap at 99% until complete handler resolves
      
      update_premium_progress(bar_width)
      
      # Build premium individual region status
      progress_msgs <- c()
      for (f in files) {
        f_base <- basename(f)
        if (grepl("_act\\.txt$", f_base)) {
          loc_name <- gsub(paste0("^progress_", session_id, "_(.*)_act\\.txt$"), "\\1", f_base)
          type_suffix <- " (Actual)"
        } else if (grepl("_pre\\.txt$", f_base)) {
          loc_name <- gsub(paste0("^progress_", session_id, "_(.*)_pre\\.txt$"), "\\1", f_base)
          type_suffix <- " (Predicted)"
        } else {
          loc_name <- gsub(paste0("^progress_", session_id, "_(.*)_(act|pre)\\.txt$"), "\\1", f_base)
          type_suffix <- ""
        }
        
        loc_display <- gsub("_", " ", loc_name)
        val <- tryCatch(as.numeric(readLines(f, warn = FALSE)), error = function(e) NA_real_)
        if(length(val) > 0 && !is.na(val)) {
          progress_msgs <- c(progress_msgs, paste0("<b>", loc_display, type_suffix, "</b>: ", val, "%"))
        }
      }
      
      # Retrieve any parallel worker geostatistical warnings dynamically
      warn_files <- list.files(path = session_progress_dir, pattern = paste0("^warn_", session_id, "_.*_.*\\.txt$"), full.names = TRUE)
      warn_msgs <- c()
      if(length(warn_files) > 0) {
        for (wf in warn_files) {
          wf_base <- basename(wf)
          if (grepl("_act\\.txt$", wf_base)) {
            loc_name <- gsub(paste0("^warn_", session_id, "_(.*)_act\\.txt$"), "\\1", wf_base)
            type_suffix <- " (Actual)"
          } else if (grepl("_pre\\.txt$", wf_base)) {
            loc_name <- gsub(paste0("^warn_", session_id, "_(.*)_pre\\.txt$"), "\\1", wf_base)
            type_suffix <- " (Predicted)"
          } else {
            loc_name <- gsub(paste0("^warn_", session_id, "_(.*)_(act|pre)\\.txt$"), "\\1", wf_base)
            type_suffix <- ""
          }
          loc_display <- gsub("_", " ", loc_name)
          msg <- tryCatch(readLines(wf, warn = FALSE), error = function(e) "")
          if (length(msg) > 0 && msg != "") {
            warn_msgs <- c(warn_msgs, paste0("⚠️ <b>", loc_display, type_suffix, "</b>: ", msg))
          }
        }
      }
      
      warn_block <- ""
      if (length(warn_msgs) > 0) {
        warn_block <- paste0("<br/><span style='font-size: 0.85em; color: #e74c3c; margin-top: 5px; display: inline-block;'>", paste(warn_msgs, collapse = "<br/>"), "</span>")
      }
      
      if (length(progress_msgs) > 0) {
        shinyjs::html("map_progress_text", paste0("Executing Parallel Interpolation Algorithms...<br/><span style='font-size: 0.85em; opacity: 0.8;'>", paste(progress_msgs, collapse = " &nbsp;|&nbsp; "), "</span>", warn_block))
      }
    }
  })

  # Event to cancel model generation
  observeEvent(input$cancel_model_btn, {
    cancel_file <- file.path(session_progress_dir, "cancel_flag.txt")
    file.create(cancel_file)
    rv$model_running <- FALSE
    
    # Hide generation elements, keep overlay visible with help text
    shinyjs::hide("map_spinner")
    shinyjs::hide("map_progress_bar_container")
    shinyjs::hide("cancel_model_btn")
    shinyjs::hide("reveal_maps_btn")
    
    shinyjs::html("map_processing_title", "Interpolation Cancelled")
    shinyjs::html("map_progress_text", HTML("Please configure parameters in the left panel and click <b>'Run Interpolation'</b> to generate geostatistical maps and review diagnostic results."))
    
    showNotification("Model generation cancelled by user.", type = "warning")
    
    # Wipe the temporary progress files
    old_files <- list.files(path = session_progress_dir, pattern = paste0("^(progress|warn)_", session_id, "_.*_.*\\.txt$"), full.names = TRUE)
    if(length(old_files) > 0) tryCatch(file.remove(old_files), error = function(e) NULL)
    
    # Reset Run Interpolation button state
    shinyjs::enable("run")
    updateActionButton(session, "run", label = "Run Interpolation", icon = NULL)
    shinyjs::runjs("$('#run i').remove();")
  })

  # Event to reveal maps and unlock analysis
  observeEvent(input$reveal_maps_btn, {
    shinyjs::hide("map_processing_overlay")
    showNotification("Maps and scientific analysis metrics are now available.", type = "message")
    
    # Reset Run Interpolation button state
    updateActionButton(session, "run", label = "Run Interpolation", icon = NULL)
    shinyjs::runjs("$('#run i').remove();")
  })

  observeEvent(input$resid_info_btn, {
    showModal(modalDialog(
      title = "Residual Mapping & Diagnostics",
      size = "l",
      easyClose = TRUE,
      tags$div(
        h4("Mathematical Formula"),
        p(HTML("<b>Residual = Observed (Actual Data Uploaded) - Predicted Value</b>")),
        p("A residual is the deviation of the model from the actual measured value at a given location. All residuals in this dashboard are derived from your primary target variable (e.g. Actual Nitrogen - Predicted Nitrogen)."),
        hr(),
        h4("Available Residual Types"),
        tags$ul(
          tags$li(tags$b("Interpolated Delta (Surface Diff):"), " Calculated by subtracting the entire Predicted surface from the Actual surface [interpolate(Actual) - interpolate(Predicted)]. This shows the net difference between the two mapped geostatistical surfaces."),
          tags$li(tags$b("Interpolated Point Errors (Model Error):"), " Calculated by first finding the error at each individual sample point location [Observed - Predicted] and THEN interpolating those local errors into a continuous surface. This specifically maps the spatial structure of the model's inability to capture local variation.")
        ),
        hr(),
        h4("Interpretation Guide"),
        tags$ul(
          tags$li(tags$b("Positive Residual (Blue):"), " Under-prediction. The actual measured value is HIGHER than the predicted model value."),
          tags$li(tags$b("Negative Residual (Red):"), " Over-prediction. The actual measured value is LOWER than the predicted model value."),
          tags$li(tags$b("Zero (White):"), " Perfect prediction at that location.")
        ),
        hr(),
        h5("References"),
        tags$ul(
          tags$li("Hengl, T. (2009). A Practical Guide to Geostatistical Mapping."),
          tags$li("Isaaks, E. H., & Srivastava, R. M. (1989). Applied Geostatistics.")
        )
      )
    ))
  })

  output$loc_res_table <- renderTable({
    req(rv$loc_resolutions)
    res_list <- rv$loc_resolutions
    if(length(res_list) == 0) return(NULL)
    
    show_buffer <- input$boundary_type %in% c("wrapped", "strict")
    res_mode_val <- input$res_mode %||% "local"
    manual_res_val <- input$grid_res %||% 50
    
    df <- data.frame(
      Locality = names(res_list),
      Resolution = sapply(res_list, function(x) {
        if (res_mode_val == "fixed") {
          paste0(round(manual_res_val, 1), " m")
        } else {
          if (is.numeric(x)) paste0(round(x, 1), " m") else x
        }
      })
    )
    
    if (show_buffer) {
      buff_mode_val <- input$buff_mode %||% "dynamic"
      method_val <- input$method %||% "OK"
      fixed_dist <- input$buff_dist %||% 250
      
      df$`Buffer (m)` <- sapply(res_list, function(x) {
        # Determine the base resolution for calculations
        if (res_mode_val == "fixed") {
          base_res <- manual_res_val
        } else {
          if (!is.numeric(x)) return("-")
          base_res <- x
        }
        
        if (buff_mode_val == "dynamic" && input$boundary_type == "wrapped") {
          val <- switch(method_val,
            "TPS" = 1.0 * base_res,
            "IDW" = 2.0 * base_res,
            "OK"  = 3.0 * base_res,
            "CK"  = 3.0 * base_res,
            "RK"  = 3.0 * base_res,
            "RFK" = 3.0 * base_res,
            2.0 * base_res
          )
          val <- max(5, min(2000, val))
          paste0(round(val, 1), " m")
        } else {
          paste0(fixed_dist, " m")
        }
      })
    }
    df
  }, striped = TRUE, hover = TRUE, bordered = TRUE, width = "100%")

  # --- Map Helper ---
  draw_map <- function(r_obj, lab) {
    current_tiles <- input$base_map_layer %||% "Esri.WorldImagery"
    
    # Check if we are drawing a raster or just points (resid_points mode)
    if((is.null(r_obj) || (is.list(r_obj) && length(r_obj) == 0)) && lab != "resid_points") return(leaflet(options = leafletOptions(zoomControl = FALSE)) %>% addProviderTiles(current_tiles, layerId="base_tiles"))
    
    m <- leaflet(options = leafletOptions(zoomControl = FALSE)) %>% addProviderTiles(current_tiles, layerId="base_tiles") %>%
      leaflet.extras::addDrawToolbar(
        targetGroup = "drawn_features",
        polylineOptions = FALSE,
        polygonOptions = drawPolygonOptions(),
        circleOptions = FALSE,
        rectangleOptions = drawRectangleOptions(),
        markerOptions = drawMarkerOptions(),
        circleMarkerOptions = FALSE,
        editOptions = editToolbarOptions(selectedPathOptions = selectedPathOptions())
      )
    meta <- get_current_meta()
    req(meta)
    
    # 1. Draw Raster if provided
    if(!is.null(r_obj) && !(is.list(r_obj) && length(r_obj) == 0)) {
      # Normalize to a list of rasters (handles single rasters and lists of regional sub-grids)
      r_list <- if(inherits(r_obj, "SpatRaster") || inherits(r_obj, "PackedSpatRaster")) list(r_obj) else r_obj
      r_list <- Filter(Negate(is.null), r_list)
      
      if (length(r_list) > 0) {
        r_names <- names(r_list)
        # Combine cell values across all active sub-grids to determine global legend bounds
        vv_list <- lapply(seq_along(r_list), function(i) {
          r <- r_list[[i]]
          r_name <- if (!is.null(r_names) && length(r_names) >= i && !is.na(r_names[i]) && r_names[i] != "") r_names[i] else as.character(i)
          cache_key <- paste0(rv$run_counter, "_", lab, "_", r_name)
          
          if (exists(cache_key, envir = leaflet_proj_cache)) {
            r_proj <- get(cache_key, envir = leaflet_proj_cache)
          } else {
            if (inherits(r, "PackedSpatRaster")) r <- terra::unwrap(r)
            r_proj <- tryCatch(terra::project(r, "EPSG:4326"), error = function(e) NULL)
            assign(cache_key, r_proj, envir = leaflet_proj_cache)
          }
          if (is.null(r_proj)) return(NULL)
          
          is_uncertainty <- isTruthy(input$show_uncertainty) && input$method %in% c("OK", "RK", "RFK", "CK") && "var1.var" %in% names(r_proj)
          active_layer <- if (is_uncertainty) {
            al <- r_proj[["var1.var"]]
            if (input$uncertainty_type == "se") sqrt(al) else al
          } else {
            if("var1.pred" %in% names(r_proj)) r_proj[["var1.pred"]] else r_proj[[1]]
          }
          as.vector(values(active_layer, na.rm=TRUE))
        })
        vv <- unlist(vv_list)
        vv_scale <- joint_vv() %||% vv
        
        is_viridis <- meta$palette == "viridis"
        if(input$value_type == "resid" || lab == "resid_raster") {
          abs_max <- max(abs(vv), na.rm = TRUE)
          if(is.infinite(abs_max) || is.na(abs_max)) abs_max <- 1
          pal <- colorNumeric("RdBu", domain = c(-abs_max, abs_max), na.color = "transparent")
          
          # Render each regional sub-grid at native crisp resolution
          for (i in seq_along(r_list)) {
            r <- r_list[[i]]
            r_name <- if (!is.null(r_names) && length(r_names) >= i && !is.na(r_names[i]) && r_names[i] != "") r_names[i] else as.character(i)
            cache_key <- paste0(rv$run_counter, "_", lab, "_", r_name)
            
            if (exists(cache_key, envir = leaflet_proj_cache)) {
              r_w <- get(cache_key, envir = leaflet_proj_cache)
            } else {
              if (inherits(r, "PackedSpatRaster")) r <- terra::unwrap(r)
              r_w <- tryCatch(terra::project(r, "EPSG:4326"), error = function(e) NULL)
              assign(cache_key, r_w, envir = leaflet_proj_cache)
            }
            if (is.null(r_w)) next
            active_layer <- if("var1.pred" %in% names(r_w)) r_w[["var1.pred"]] else r_w[[1]]
            m <- m %>% addRasterImage(active_layer, colors = pal, opacity = 0.8)
          }
          m <- m %>% leaflet::addLegend(pal = pal, values = c(-abs_max, abs_max), title = paste("Resid:", meta$label))
        } else if(input$color_style == "agro") {
          params <- agro_params()
          if(!is.null(params)) {
            pal <- colorBin(params$colors, bins = params$brks, na.color = "transparent", right = FALSE)
            
            # Render each regional sub-grid at native crisp resolution
            for (i in seq_along(r_list)) {
              r <- r_list[[i]]
              r_name <- if (!is.null(r_names) && length(r_names) >= i && !is.na(r_names[i]) && r_names[i] != "") r_names[i] else as.character(i)
              cache_key <- paste0(rv$run_counter, "_", lab, "_", r_name)
              
              if (exists(cache_key, envir = leaflet_proj_cache)) {
                r_w <- get(cache_key, envir = leaflet_proj_cache)
              } else {
                if (inherits(r, "PackedSpatRaster")) r <- terra::unwrap(r)
                r_w <- tryCatch(terra::project(r, "EPSG:4326"), error = function(e) NULL)
                assign(cache_key, r_w, envir = leaflet_proj_cache)
              }
              if (is.null(r_w)) next
              
              is_uncertainty <- isTruthy(input$show_uncertainty) && input$method %in% c("OK", "RK", "RFK", "CK") && "var1.var" %in% names(r_w)
              active_layer <- if (is_uncertainty) {
                al <- r_w[["var1.var"]]
                if (input$uncertainty_type == "se") sqrt(al) else al
              } else {
                if("var1.pred" %in% names(r_w)) r_w[["var1.pred"]] else r_w[[1]]
              }
              m <- m %>% addRasterImage(active_layer, colors = pal, opacity = 0.8)
            }
            m <- m %>% leaflet::addLegend(colors = params$colors, labels = params$leg_labels, opacity = 0.8, title = paste(meta$label, meta$unit))
          }
        } else if(input$color_style == "bin") {
          pal <- if(is_viridis) colorBin(viridis::viridis(256, option = meta$palette), vv_scale, bins = 5, na.color = "transparent") 
                 else colorBin(meta$palette, vv_scale, bins = 5, na.color = "transparent")
                 
          # Render each regional sub-grid at native crisp resolution
          for (i in seq_along(r_list)) {
            r <- r_list[[i]]
            r_name <- if (!is.null(r_names) && length(r_names) >= i && !is.na(r_names[i]) && r_names[i] != "") r_names[i] else as.character(i)
            cache_key <- paste0(rv$run_counter, "_", lab, "_", r_name)
            
            if (exists(cache_key, envir = leaflet_proj_cache)) {
              r_w <- get(cache_key, envir = leaflet_proj_cache)
            } else {
              if (inherits(r, "PackedSpatRaster")) r <- terra::unwrap(r)
              r_w <- tryCatch(terra::project(r, "EPSG:4326"), error = function(e) NULL)
              assign(cache_key, r_w, envir = leaflet_proj_cache)
            }
            if (is.null(r_w)) next
            
            is_uncertainty <- isTruthy(input$show_uncertainty) && input$method %in% c("OK", "RK", "RFK", "CK") && "var1.var" %in% names(r_w)
            active_layer <- if (is_uncertainty) {
              al <- r_w[["var1.var"]]
              if (input$uncertainty_type == "se") sqrt(al) else al
            } else {
              if("var1.pred" %in% names(r_w)) r_w[["var1.pred"]] else r_w[[1]]
            }
            m <- m %>% addRasterImage(active_layer, colors = pal, opacity = 0.8)
          }
          m <- m %>% leaflet::addLegend(pal = pal, values = vv_scale, opacity = 0.8, title = paste(meta$label, meta$unit))
        } else {
          pal <- if(is_viridis) colorNumeric(viridis::viridis(256, option = meta$palette), vv_scale, na.color = "transparent") 
                 else colorNumeric(meta$palette, vv_scale, na.color = "transparent")
                 
          # Render each regional sub-grid at native crisp resolution
          for (i in seq_along(r_list)) {
            r <- r_list[[i]]
            r_name <- if (!is.null(r_names) && length(r_names) >= i && !is.na(r_names[i]) && r_names[i] != "") r_names[i] else as.character(i)
            cache_key <- paste0(rv$run_counter, "_", lab, "_", r_name)
            
            if (exists(cache_key, envir = leaflet_proj_cache)) {
              r_w <- get(cache_key, envir = leaflet_proj_cache)
            } else {
              if (inherits(r, "PackedSpatRaster")) r <- terra::unwrap(r)
              r_w <- tryCatch(terra::project(r, "EPSG:4326"), error = function(e) NULL)
              assign(cache_key, r_w, envir = leaflet_proj_cache)
            }
            if (is.null(r_w)) next
            
            is_uncertainty <- isTruthy(input$show_uncertainty) && input$method %in% c("OK", "RK", "RFK", "CK") && "var1.var" %in% names(r_w)
            active_layer <- if (is_uncertainty) {
              al <- r_w[["var1.var"]]
              if (input$uncertainty_type == "se") sqrt(al) else al
            } else {
              if("var1.pred" %in% names(r_w)) r_w[["var1.pred"]] else r_w[[1]]
            }
            m <- m %>% addRasterImage(active_layer, colors = pal, opacity = 0.8)
          }
          
          # Dynamic precision based on range
          v_range <- diff(range(vv_scale, na.rm=TRUE))
          d_format <- if(is.na(v_range)) 2 else if(v_range < 0.01) 6 else if(v_range < 0.1) 4 else 2
          m <- m %>% leaflet::addLegend(pal = pal, values = vv_scale, title = paste(meta$label, meta$unit), labFormat = labelFormat(digits = d_format))
        }
      }
    }

    # 2. Draw Points
    # Separation: Regular Points (cyan, toggled) vs Colored Residual Points (RdBu, context-dependent)
    
    # RIGHT MAP in comparison mode: ALWAYS show colored residual points
    if(lab == "resid_points") {
       pts_view <- st_transform(rv$sf, 4326)
       abs_max_p <- max(abs(pts_view$resid), na.rm=T)
       if(is.infinite(abs_max_p) || is.na(abs_max_p)) abs_max_p <- 1
       pal_pts <- colorNumeric("RdBu", domain = c(-abs_max_p, abs_max_p), na.color = "black")
       df_clean <- st_drop_geometry(pts_view)
       popups <- vapply(1:nrow(df_clean), function(i) generate_popup(df_clean[i, ]), character(1))
       
       m <- m %>% addCircleMarkers(data = pts_view, radius = 5, color = "black", weight = 1,
                                  fillColor = ~pal_pts(resid), fillOpacity = 0.9,
                                  popup = popups)
       m <- m %>% leaflet::addLegend(pal = pal_pts, values = c(-abs_max_p, abs_max_p), title = paste("Point Resid:", meta$label))
    }
    
    # OTHER MAPS (Single view or Comparison Left): Use the Toggle for REGULAR points
    # (unless it's the left map in resid comp mode, where we want only interpolated)
    if(input$show_points_viewer && lab != "resid_raster" && lab != "resid_points") {
      pts_view <- st_transform(rv$sf, 4326)
      if(nrow(pts_view) > 0) {
        # F2: Use styled points with group coloring, labels, and custom sizes
        color_by <- input$pt_color_by %||% "none"
        label_field <- input$pt_label_field %||% "none"

        m <- add_styled_points(m, pts_view,
          color_by = color_by,
          custom_colors = rv$pt_style_colors,
          show_labels = isTRUE(input$pt_show_labels),
          label_field = label_field,
          label_size = input$pt_label_size %||% 11,
          marker_size = input$pt_marker_size %||% 3,
          popup_fn = generate_popup
        )
      }
    }
    
    if(input$show_borders && !is.null(rv$bound)) {
      m <- m %>% addPolygons(data = st_transform(st_as_sf(rv$bound), 4326), fill = FALSE, color = "white", weight = 2)
    }
    
    if(input$show_north) {
      m <- m %>% addControl(html="<div style='text-align: center; color: white; font-family: Arial, sans-serif; pointer-events: none;'><div style='font-size: 16px; font-weight: bold; line-height: 1; margin-bottom: 4px; text-shadow: 1px 1px 2px black;'>N</div><svg width='30' height='30' viewBox='0 0 24 24' style='filter: drop-shadow(1px 1px 2px black);'><polygon points='12,2 7,22 12,17 17,22' fill='#e74c3c' stroke='white' stroke-width='1.5'/><polygon points='12,2 7,22 12,17' fill='#c0392b' stroke='white' stroke-width='1.5'/></svg></div>", position="topleft")
    }
    if(input$show_scale) {
      m <- m %>% htmlwidgets::onRender("
        function(el, x) {
          var map = this;
          var scale = L.control.scale({position: 'bottomleft', metric: true, imperial: false}).addTo(map);
          var c = document.getElementById('distance_scale_container');
          if(c) {
            c.innerHTML = '';
            var s = scale.getContainer();
            s.style.margin = '0 auto';
            s.style.border = '1px solid #ccc';
            s.style.borderRadius = '4px';
            c.appendChild(s);
          }
        }
      ")
    }
    
    if(input$show_res_overlay && length(rv$loc_resolutions) > 0) {
      res_html <- paste0("<div style='background:white; padding:5px; border-radius:4px; border: 1px solid #ccc; font-size:12px; font-family:sans-serif;'><b>Resolutions:</b><br>", paste(names(rv$loc_resolutions), sapply(rv$loc_resolutions, function(x) round(x,2)), sep=": ", collapse="<br>"), "</div>")
      m <- m %>% addControl(html=res_html, position="bottomright")
    }
    
    m
  }

  output$main_map_title <- renderText({
    req(input$value_type); meta <- get_current_meta(); req(meta)
    # Reactive dependency on results to ensure it updates when model finishes
    model_done <- !is.null(rv$rast_list_act) && length(rv$rast_list_act) > 0
    
    type_lab <- switch(input$value_type,
           "actual" = "Actual Data View",
           "pred" = "Best Predictions View (_cve)",
           "pred_ss" = "Single Split Predictions View (_ss)",
           "resid" = "Residuals View (Actual - Predicted)")
    
    current_method <- rv$run_method[[input$var_id]]
    method_lab <- if(!is.null(current_method)) {
      m_name <- get_method_label(current_method)
      paste0(" (", m_name, ")")
    } else ""
    
    prefix <- meta$label
    paste0(prefix, " - ", type_lab, method_lab)
  })

  output$comp_left_title <- renderText({
    req(input$var_id, rv$mapping$vars); meta <- get_current_meta(); req(meta)
    
    current_method <- rv$run_method[[input$var_id]]
    method_lab <- if(!is.null(current_method)) {
      m_name <- get_method_label(current_method)
      paste0(" (", m_name, ")")
    } else ""
    
    prefix <- meta$label
    if(input$value_type == "resid") return(paste0(prefix, " - Interpolated Residuals", method_lab))
    paste0(prefix, " - Actual Data", method_lab)
  })
  
  output$comp_right_title <- renderText({
    req(input$var_id, rv$mapping$vars); meta <- get_current_meta(); req(meta)
    
    current_method <- rv$run_method[[input$var_id]]
    method_lab <- if(!is.null(current_method)) {
      m_name <- get_method_label(current_method)
      paste0(" (", m_name, ")")
    } else ""
    
    prefix <- meta$label
    if(input$value_type == "resid") return(paste0(prefix, " - Point Residuals", method_lab))
    
    type_lab <- switch(input$value_type,
           "pred" = "Best Predictions (_cve)",
           "pred_ss" = "Split Predictions (_ss)",
           "resid" = "Residuals (v - pv)")
    paste0(prefix, " - ", type_lab, method_lab)
  })

  observeEvent(input$base_map_layer, {
    leafletProxy("main_map") %>%
      clearTiles() %>%
      addProviderTiles(input$base_map_layer, layerId="base_tiles", options = providerTileOptions(zIndex = -10))
  })

  observeEvent(input$refresh_map_area, {
    req(input$base_map_layer)
    # Re-apply the base tile layer to trigger a redraw/refresh of the map canvas
    leafletProxy("main_map") %>%
      clearTiles() %>%
      addProviderTiles(input$base_map_layer, layerId="base_tiles", options = providerTileOptions(zIndex = -10))
    
    leafletProxy("comp_map_left") %>%
      clearTiles() %>%
      addProviderTiles(input$base_map_layer, layerId="base_tiles", options = providerTileOptions(zIndex = -10))
      
    leafletProxy("comp_map_right") %>%
      clearTiles() %>%
      addProviderTiles(input$base_map_layer, layerId="base_tiles", options = providerTileOptions(zIndex = -10))
      
    # Reset to the initial reveal position (zoom/pan to bounding box)
    if (!is.null(rv$bound)) {
      tryCatch({
        bbox <- sf::st_bbox(sf::st_transform(sf::st_as_sf(rv$bound), 4326))
        leafletProxy("main_map") %>% fitBounds(as.numeric(bbox$xmin), as.numeric(bbox$ymin), as.numeric(bbox$xmax), as.numeric(bbox$ymax))
        if (isTRUE(input$comp_mode)) {
          leafletProxy("comp_map_left") %>% fitBounds(as.numeric(bbox$xmin), as.numeric(bbox$ymin), as.numeric(bbox$xmax), as.numeric(bbox$ymax))
          leafletProxy("comp_map_right") %>% fitBounds(as.numeric(bbox$xmin), as.numeric(bbox$ymin), as.numeric(bbox$xmax), as.numeric(bbox$ymax))
        }
      }, error = function(e) NULL)
    } else if (!is.null(rv$user_data) && !is.null(rv$mapping$x) && !is.null(rv$mapping$y)) {
      tryCatch({
        df_map <- rv$user_data %>% 
          dplyr::select(x = !!sym(rv$mapping$x), y = !!sym(rv$mapping$y)) %>% 
          na.omit()
        pts <- st_as_sf(df_map, coords = c("x", "y"), crs = rv$mapping$crs) %>% st_transform(4326)
        bbox <- st_bbox(pts)
        leafletProxy("main_map") %>% fitBounds(as.numeric(bbox$xmin), as.numeric(bbox$ymin), as.numeric(bbox$xmax), as.numeric(bbox$ymax))
        if (isTRUE(input$comp_mode)) {
          leafletProxy("comp_map_left") %>% fitBounds(as.numeric(bbox$xmin), as.numeric(bbox$ymin), as.numeric(bbox$xmax), as.numeric(bbox$ymax))
          leafletProxy("comp_map_right") %>% fitBounds(as.numeric(bbox$xmin), as.numeric(bbox$ymin), as.numeric(bbox$xmax), as.numeric(bbox$ymax))
        }
      }, error = function(e) NULL)
    }
      
    # Trigger window resize to force Leaflet invalidateSize which fixes gray tiles
    shinyjs::runjs("setTimeout(function() { window.dispatchEvent(new Event('resize')); }, 100);")
  })

  output$main_map <- renderLeaflet({
    req(input$value_type); req(rv$run_method[[input$var_id]])
    target <- if(input$value_type == "actual") rv$rast_list_act
              else if(input$value_type == "resid") rv$rast_list_res
              else rv$rast_list_pre
    m <- draw_map(target, input$value_type)
    session_state$main_map_rendered <- TRUE
    m
  })

  output$comp_map_left <- renderLeaflet({
    req(rv$run_method[[input$var_id]])
    m <- if(input$value_type == "resid") {
      draw_map(rv$rast_list_res, "resid_raster")
    } else {
      draw_map(rv$rast_list_act, "Actual")
    }
    session_state$comp_maps_rendered <- TRUE
    m
  })

  output$comp_map_right <- renderLeaflet({
    req(rv$run_method[[input$var_id]])
    m <- if(input$value_type == "resid") {
      draw_map(NULL, "resid_points")
    } else {
      draw_map(rv$rast_list_pre, "Predicted")
    }
    session_state$comp_maps_rendered <- TRUE
    m
  })
      # --- Map Panning Logic ---
      output$locality_pan_ui <- renderUI({
      req(rv$loc_names)
      render_locality_pan_input(rv$loc_names)
      })

      observeEvent(input$locality_pan, {
      req(input$locality_pan, rv$user_data, rv$mapping$x, rv$mapping$y, rv$mapping$crs)

      # 1. Get bounding box
      bbox <- if (input$locality_pan == "global") {
        # Use only localities that were actually processed (rv$loc_names)
        df_map <- rv$user_data %>% 
          dplyr::filter(!!sym(rv$mapping$loc) %in% rv$loc_names) %>%
          dplyr::select(x = !!sym(rv$mapping$x), y = !!sym(rv$mapping$y)) %>% 
          na.omit()
        pts <- st_as_sf(df_map, coords = c("x", "y"), crs = rv$mapping$crs) %>% st_transform(4326)
        st_bbox(pts)
      } else {        # Filter by selected locality
        df_map <- rv$user_data %>% 
          dplyr::filter(!!sym(rv$mapping$loc) == input$locality_pan) %>%
          dplyr::select(x = !!sym(rv$mapping$x), y = !!sym(rv$mapping$y)) %>% 
          na.omit()
        pts <- st_as_sf(df_map, coords = c("x", "y"), crs = rv$mapping$crs) %>% st_transform(4326)
        st_bbox(pts)
      }

      # 2. Pan maps
      leafletProxy("main_map") %>% fitBounds(as.numeric(bbox$xmin), as.numeric(bbox$ymin), as.numeric(bbox$xmax), as.numeric(bbox$ymax))

      if (isTRUE(input$comp_mode)) {
        leafletProxy("comp_map_left") %>% fitBounds(as.numeric(bbox$xmin), as.numeric(bbox$ymin), as.numeric(bbox$xmax), as.numeric(bbox$ymax))
        leafletProxy("comp_map_right") %>% fitBounds(as.numeric(bbox$xmin), as.numeric(bbox$ymin), as.numeric(bbox$xmax), as.numeric(bbox$ymax))
      }
   })

   # --- Scientific Outputs ---
   output$vgm_plot_main <- renderPlot({
     loc <- input$sel_loc_stats; meta <- get_current_meta()
     req(loc, meta)
     if(loc == "Total (Combined)") {
       # Use rv$sf if available (populated after interpolation completes)
       # Otherwise, build a temporary sf object directly from rv$user_data for pre-interpolation tuning
       pts_sf <- if(!is.null(rv$sf)) {
         rv$sf
       } else {
         req(rv$user_data, rv$mapping$x, rv$mapping$y, rv$mapping$crs)
         act_col <- meta$actual
         req(act_col %in% colnames(rv$user_data))
         df_clean <- rv$user_data %>% 
           dplyr::select(x = !!sym(rv$mapping$x), y = !!sym(rv$mapping$y), v = !!sym(act_col)) %>% 
           na.omit()
         req(nrow(df_clean) >= 3)
         
         sf_obj <- sf::st_as_sf(df_clean, coords = c("x", "y"), crs = rv$mapping$crs)
         sf_obj <- validate_and_project_sf(sf_obj, rv$mapping$crs)
         sf_obj
       }
       req(pts_sf)
       plot(gstat::variogram(v ~ 1, pts_sf), main = paste("Global Variogram (Actual):", meta$label))
     }
     else { 
       req(rv$v_emp_list[[paste0(loc, "_act")]]); 
       v_emp <- rv$v_emp_list[[paste0(loc, "_act")]]
       v_fit <- rv$v_fit_list[[paste0(loc, "_act")]]
       p <- plot(v_emp, v_fit, main = paste("Fitted (Actual):", loc))
       
       # Manual Overlay logic
       if(input$vgm_mode == "manual" && loc == input$m_loc && (is.null(input$m_target) || input$m_target == "act")) {
         v_mod_m <- vgm(psill = input$m_psill, model = input$k_mod, range = input$m_range, nugget = input$m_nugget)
         v_line_m <- variogramLine(v_mod_m, maxdist = max(v_emp$dist))
         v_line_at_emp <- variogramLine(v_mod_m, dist_vector = v_emp$dist)
         sse_m <- sum((v_emp$gamma - v_line_at_emp$gamma)^2)
         
         p <- p + latticeExtra::layer({
           panel.lines(v_line_m$dist, v_line_m$gamma, col = "red", lwd = 2, lty = 2)
           panel.text(max_dist * 0.8, max_gamma * 0.9, paste("Manual SSE:", round(sse_m, 4)), col="red", font=2)
         }, data = list(v_line_m = v_line_m, sse_m = sse_m, max_dist = max(v_emp$dist), max_gamma = max(v_emp$gamma)))
       }
       p
     }
   })
   output$vgm_plot_pred <- renderPlot({
     loc <- input$sel_loc_stats; meta <- get_current_meta()
     req(loc, meta)
     if(loc == "Total (Combined)") {
       # Use rv$sf if available and has the 'pv' column
       # Otherwise, build a temporary sf object if dual predictions exist in rv$user_data
       pred_col <- if(input$value_type == "pred_ss") meta$pred_ss else meta$pred
       
       pts_sf <- if(!is.null(rv$sf) && "pv" %in% colnames(rv$sf)) {
         rv$sf
       } else if(!is.null(pred_col) && pred_col %in% colnames(rv$user_data)) {
         req(rv$user_data, rv$mapping$x, rv$mapping$y, rv$mapping$crs)
         df_clean <- rv$user_data %>% 
           dplyr::select(x = !!sym(rv$mapping$x), y = !!sym(rv$mapping$y), pv = !!sym(pred_col)) %>% 
           na.omit()
         if(nrow(df_clean) < 3) NULL else {
           sf_obj <- sf::st_as_sf(df_clean, coords = c("x", "y"), crs = rv$mapping$crs)
           sf_obj <- validate_and_project_sf(sf_obj, rv$mapping$crs)
           sf_obj
         }
       } else {
         NULL
       }
       
       if (is.null(pts_sf) || !("pv" %in% colnames(pts_sf))) {
         return(ggplot() + annotate("text", x = 4, y = 4, label = "Predicted data structure is not available.\nPlease run spatial interpolation first.", size = 5, color = "grey40") + theme_void())
       }
       
       plot(gstat::variogram(pv ~ 1, pts_sf %>% filter(!is.na(pv))), main = paste("Global Variogram (Predicted):", meta$label))
     }
     else { 
       req(rv$v_emp_list[[paste0(loc, "_pre")]]); 
       v_emp <- rv$v_emp_list[[paste0(loc, "_pre")]]
       v_fit <- rv$v_fit_list[[paste0(loc, "_pre")]]
       p <- plot(v_emp, v_fit, main = paste("Fitted (Predicted):", loc))
       
       # Manual Overlay logic
       if(input$vgm_mode == "manual" && loc == input$m_loc && !is.null(input$m_target) && input$m_target == "pre") {
         v_mod_m <- vgm(psill = input$m_psill, model = input$k_mod, range = input$m_range, nugget = input$m_nugget)
         v_line_m <- variogramLine(v_mod_m, maxdist = max(v_emp$dist))
         v_line_at_emp <- variogramLine(v_mod_m, dist_vector = v_emp$dist)
         sse_m <- sum((v_emp$gamma - v_line_at_emp$gamma)^2)
         
         p <- p + latticeExtra::layer({
           panel.lines(v_line_m$dist, v_line_m$gamma, col = "red", lwd = 2, lty = 2)
           panel.text(max_dist * 0.8, max_gamma * 0.9, paste("Manual SSE:", round(sse_m, 4)), col="red", font=2)
         }, data = list(v_line_m = v_line_m, sse_m = sse_m, max_dist = max(v_emp$dist), max_gamma = max(v_emp$gamma)))
       }
       p
     }
   })

    # --- Method-Specific Diagnostics ---
    # RK Summaries
    output$model_summary_ui_act <- renderUI({
    loc <- input$sel_loc_stats; req(loc)
    if (loc == "Total (Combined)") {
      return(div(style="padding: 12px; background-color: #f8f9fa; border: 1px dashed #ced4da; border-radius: 6px; color: #6c757d; font-style: italic; text-align: center;", 
                 "Linear trend summaries are computed per locality. Please select a specific locality from the analysis filter list above to view details."))
    }
    req(rv$model_summaries[[paste0(loc, "_act")]])
    tagList(verbatimTextOutput("summ_act_static"))
  })
  output$model_summary_ui_pre <- renderUI({
    loc <- input$sel_loc_stats; req(loc)
    if (loc == "Total (Combined)") {
      return(div(style="padding: 12px; background-color: #f8f9fa; border: 1px dashed #ced4da; border-radius: 6px; color: #6c757d; font-style: italic; text-align: center;", 
                 "Linear trend summaries are computed per locality. Please select a specific locality from the analysis filter list above to view details."))
    }
    req(rv$model_summaries[[paste0(loc, "_pre")]])
    tagList(verbatimTextOutput("summ_pre_static"))
  })
  
  output$summ_act_static <- renderPrint({
    loc <- input$sel_loc_stats
    req(loc, loc != "Total (Combined)")
    summary_obj <- rv$model_summaries[[paste0(loc, "_act")]]
    req(summary_obj)
    summary_obj
  })
  
  output$summ_pre_static <- renderPrint({
    loc <- input$sel_loc_stats
    req(loc, loc != "Total (Combined)")
    summary_obj <- rv$model_summaries[[paste0(loc, "_pre")]]
    req(summary_obj)
    summary_obj
  })

  # RFK Importance
  output$rf_importance_plot_act <- renderPlot({
    loc <- input$sel_loc_stats; req(loc)
    if (loc == "Total (Combined)") {
      return(ggplot() + annotate("text", x = 4, y = 4, label = "RF Variable Importance is generated per locality.\nPlease select a specific locality from the dropdown.", size = 5, color = "grey40") + theme_void())
    }
    req(rv$rf_models[[paste0(loc, "_act")]])
    randomForest::varImpPlot(rv$rf_models[[paste0(loc, "_act")]], main = paste("Variable Importance (Actual):", loc))
  })
  output$rf_importance_plot_pre <- renderPlot({
    loc <- input$sel_loc_stats; req(loc)
    if (loc == "Total (Combined)") {
      return(ggplot() + annotate("text", x = 4, y = 4, label = "RF Variable Importance is generated per locality.\nPlease select a specific locality from the dropdown.", size = 5, color = "grey40") + theme_void())
    }
    req(rv$rf_models[[paste0(loc, "_pre")]])
    randomForest::varImpPlot(rv$rf_models[[paste0(loc, "_pre")]], main = paste("Variable Importance (Predicted):", loc))
  })
  
  # --- Residual & Cross-Variogram Rendering Engine ---
  render_internal_vgm_plot <- function(type) {
    renderPlot({
      loc <- input$sel_loc_stats; req(loc)
      title_suffix <- if (type == "act") "(Actual)" else "(Predicted)"
      col_resid <- if (type == "act") "model_resid_act" else "model_resid_pre"
      
      if (loc == "Total (Combined)") {
        req(rv$sf, col_resid %in% colnames(rv$sf))
        formula_obj <- as.formula(paste(col_resid, "~ 1"))
        df_filtered <- rv$sf[!is.na(rv$sf[[col_resid]]), ]
        p_res <- plot(variogram(formula_obj, df_filtered), main = paste("Global Internal Residual Variogram", title_suffix))
        print(p_res)
      } else {
        req(rv$v_emp_list[[paste0(loc, "_", type)]], rv$v_fit_list[[paste0(loc, "_", type)]])
        p_res <- plot(rv$v_emp_list[[paste0(loc, "_", type)]], rv$v_fit_list[[paste0(loc, "_", type)]], 
             main = paste("Internal Residual Variogram", paste0(title_suffix, ":"), loc))
        print(p_res)
      }
    })
  }

  output$rk_internal_vgm_act  <- render_internal_vgm_plot("act")
  output$rk_internal_vgm_pre  <- render_internal_vgm_plot("pre")
  output$rfk_internal_vgm_act <- render_internal_vgm_plot("act")
  output$rfk_internal_vgm_pre <- render_internal_vgm_plot("pre")

  render_ck_variogram_plot <- function(type) {
    renderPlot({
      loc <- input$sel_loc_stats; req(loc)
      if (loc == "Total (Combined)") {
        return(ggplot() + annotate("text", x = 4, y = 4, label = "Cross-variograms are generated per locality.\nPlease select a specific locality from the dropdown.", size = 5, color = "grey40") + theme_void())
      }
      key <- paste0(loc, "_", type)
      g <- rv$gstat_objs[[key]]
      if (is.null(g)) {
        return(ggplot() + annotate("text", x = 4, y = 4, label = "Cross-variogram is not available\n(LMC model fit failed, using Ordinary Kriging fallback.)", size = 5, color = "grey40") + theme_void())
      }
      vm <- variogram(g)
      title_suffix <- if (type == "act") "(Actual)" else "(Predicted)"
      p_ck <- plot(vm, model = g$model, main = paste("Cross-Variogram", paste0(title_suffix, ":"), loc))
      print(p_ck)
    })
  }

  output$ck_variogram_plot_act <- render_ck_variogram_plot("act")
  output$ck_variogram_plot_pred <- render_ck_variogram_plot("pre")

  output$vgm_params_table <- renderTable({
    loc <- input$sel_loc_stats; req(loc); if(loc == "Total (Combined)") return(NULL)
    f_a <- rv$v_fit_list[[paste0(loc, "_act")]]; f_p <- rv$v_fit_list[[paste0(loc, "_pre")]]
    if(is.null(f_a) && is.null(f_p)) return(NULL)

    get_vgm_params <- function(f) {
      if(is.null(f)) return(rep("NA", 5))
      mod <- as.character(f$model[2])
      nug <- f$psill[1]
      sill <- sum(f$psill)
      rng <- f$range[2]
      str_dep <- if(sill > 0) ((sill - nug) / sill) * 100 else 0
      c(mod, round(nug, 4), round(sill, 4), round(rng, 1), paste0(round(str_dep, 1), "%"))
    }

    data.frame(Param = c("Model", "Nugget", "Sill", "Range", "Structural Dep."), 
               Actual = get_vgm_params(f_a),
               Predicted = get_vgm_params(f_p))
  })
  output$tps_gcv_plot_act <- renderPlot({
    loc <- input$sel_loc_stats; req(loc, input$method == "TPS")
    if(loc == "Total (Combined)") {
      return(ggplot() + annotate("text", x = 4, y = 4, label = "TPS GCV diagnostics are generated per locality.\nPlease select a specific locality from the dropdown.", size = 5, color = "grey40") + theme_void())
    }
    df <- rv$tps_gcv_data[[paste0(loc, "_act")]]
    req(df, nrow(df) > 0)
    tryCatch({
      ggplot(df, aes(x = lambda, y = gcv)) + 
        geom_line(color = "steelblue", size = 1) + 
        geom_point(color = "darkblue") +
        scale_x_log10() + theme_minimal() + 
        labs(title = paste("GCV Curve (Actual):", loc), x = "Lambda (Log Scale)", y = "GCV Score")
    }, error = function(e) {
      plot(1, 1, type="n", main=paste("GCV Plot Error:", e$message), axes=F, xlab="", ylab="")
    })
  })

  build_obs_pred_plot <- function(df, title, x_lab = "Observed", y_lab = "Predicted") {
    req(df, nrow(df) > 0)
    
    # Robust conversion to data frame
    if (inherits(df, "Spatial")) {
      df <- as.data.frame(df)
    } else if (inherits(df, "sf")) {
      df <- sf::st_drop_geometry(df)
    }
    df <- as.data.frame(df)
    
    cnames <- names(df)
    cols <- detect_cv_columns(cnames)
    obs_col <- cols$observed
    pre_col <- cols$pred
    
    req(obs_col, pre_col)
    
    obs <- df[[obs_col]]; pre <- df[[pre_col]]
    
    ggplot(data.frame(Observed = obs, Predicted = pre), aes(x = Observed, y = Predicted)) +
      geom_point(alpha = 0.6) +
      geom_abline(intercept = 0, slope = 1, color = "red", linetype = "dashed") +
      geom_smooth(method = "lm", color = "blue", se = FALSE) +
      labs(title = title, subtitle = "Red: 1:1 Line, Blue: Regression", x = x_lab, y = y_lab) +
      theme_minimal()
  }

  output$obs_pred_plot_act <- renderPlot({
    req(input$sel_loc_stats, rv$cv_data_act)
    loc <- input$sel_loc_stats
    if(loc == "Total (Combined)") {
       df_list <- rv$cv_data_act
       df <- do.call(rbind, lapply(df_list, function(x) if(inherits(x, "sf")) st_drop_geometry(x) else as.data.frame(x)))
    } else {
       df <- rv$cv_data_act[[loc]]
       if(inherits(df, "sf")) df <- st_drop_geometry(df)
       if(inherits(df, "Spatial")) df <- as.data.frame(df)
    }
    build_obs_pred_plot(df, title = paste("Observed vs Predicted:", loc))
  })

  output$resid_vgm_plot_act <- render_resid_plot(reactive(rv$cv_data_act), "")

  output$obs_pred_plot_pre <- renderPlot({
    req(input$sel_loc_stats, rv$cv_data_pre)
    loc <- input$sel_loc_stats
    if(loc == "Total (Combined)") {
       df_list <- rv$cv_data_pre
       df <- do.call(rbind, lapply(df_list, function(x) if(inherits(x, "sf")) st_drop_geometry(x) else as.data.frame(x)))
    } else {
       df <- rv$cv_data_pre[[loc]]
       if(inherits(df, "sf")) df <- st_drop_geometry(df)
       if(inherits(df, "Spatial")) df <- as.data.frame(df)
    }
    build_obs_pred_plot(df, title = paste("Observed vs Predicted (Predicted Map):", loc))
  })

  output$resid_vgm_plot_pre <- render_resid_plot(reactive(rv$cv_data_pre), "(Predicted Map)")

  output$tps_gcv_plot_pre <- renderPlot({
    loc <- input$sel_loc_stats; req(loc, input$method == "TPS")
    if(loc == "Total (Combined)") {
      return(ggplot() + annotate("text", x = 4, y = 4, label = "TPS GCV diagnostics are generated per locality.\nPlease select a specific locality from the dropdown.", size = 5, color = "grey40") + theme_void())
    }
    df <- rv$tps_gcv_data[[paste0(loc, "_pre")]]
    req(df, nrow(df) > 0)
    tryCatch({
      ggplot(df, aes(x = lambda, y = gcv)) + 
        geom_line(color = "firebrick", size = 1) + 
        geom_point(color = "darkred") +
        scale_x_log10() + theme_minimal() + 
        labs(title = paste("GCV Curve (Predicted):", loc), x = "Lambda (Log Scale)", y = "GCV Score")
    }, error = function(e) {
      plot(1, 1, type="n", main=paste("GCV Plot Error:", e$message), axes=F, xlab="", ylab="")
    })
  })

  output$regional_params_table <- renderTable({
    loc <- input$sel_loc_stats; req(loc, input$method %in% c("IDW", "TPS"))
    if(loc == "Total (Combined)") return(NULL)
    type <- input$method
    data.frame(
      Param = if(type == "IDW") "Power (p)" else "Lambda",
      Actual = as.character(round(get_regional_param(type, loc, "act"), 6)),
      Predicted = as.character(round(get_regional_param(type, loc, "pre"), 6))
    )
  })

  output$stats_table_total <- renderTable({
    req(rv$user_data, input$var_id)
    meta <- get_current_meta()
    req(meta)
    
    # Get actual and predicted data from raw frame
    df <- rv$user_data
    v_act <- if(!is.null(meta$actual) && !is.na(meta$actual) && meta$actual %in% colnames(df)) df[[meta$actual]] else NULL
    if (is.null(v_act)) return(NULL)
    
    v_pre <- if(!is.null(meta$pred) && !is.na(meta$pred) && meta$pred %in% colnames(df)) df[[meta$pred]] else if(!is.null(meta$pred_ss) && !is.na(meta$pred_ss) && meta$pred_ss %in% colnames(df)) df[[meta$pred_ss]] else NULL
    
    s_a <- summary(v_act)
    res <- data.frame(Metric = names(s_a), Total_Actual = as.character(round(as.numeric(s_a), 3)))
    
    if(!is.null(v_pre)) {
      s_p <- summary(v_pre)
      res$Total_Predicted <- as.character(round(as.numeric(s_p), 3))
    }
    res
  })
  
  output$stats_table_loc <- renderTable({
    req(rv$user_data, input$var_id, input$sel_loc_stats)
    if(input$sel_loc_stats == "Total (Combined)") return(NULL)
    meta <- get_current_meta()
    req(meta)
    
    # Filter raw data by mapped locality column
    df <- rv$user_data %>% filter(!!sym(rv$mapping$loc) == input$sel_loc_stats)
    v_act <- if(!is.null(meta$actual) && !is.na(meta$actual) && meta$actual %in% colnames(df)) df[[meta$actual]] else NULL
    if (is.null(v_act)) return(NULL)
    
    v_pre <- if(!is.null(meta$pred) && !is.na(meta$pred) && meta$pred %in% colnames(df)) df[[meta$pred]] else if(!is.null(meta$pred_ss) && !is.na(meta$pred_ss) && meta$pred_ss %in% colnames(df)) df[[meta$pred_ss]] else NULL
    
    s_a <- summary(v_act)
    res <- data.frame(Metric = names(s_a), Selected_Actual = as.character(round(as.numeric(s_a), 3)))
    
    if(!is.null(v_pre)) {
      s_p <- summary(v_pre)
      res$Selected_Predicted <- as.character(round(as.numeric(s_p), 3))
    }
    res
  })

  calc_area_df <- function(r_obj, r_id = NULL) {
    if(is.null(r_obj)) return(NULL)
    if(inherits(r_obj, "PackedSpatRaster")) r_obj <- terra::unwrap(r_obj)
    params <- tryCatch(agro_params(), condition = function(c) NULL)
    if(is.null(params)) return(data.frame(Status = "Awaiting Agro Params"))
    
    # Construct cache key if r_id is provided
    if (!is.null(r_id)) {
      brk_str <- if (!is.null(params) && !is.null(params$brks)) paste(params$brks, collapse = "_") else "nobrks"
      cache_key <- paste0(rv$run_counter, "_", r_id, "_", brk_str)
      if (exists(cache_key, envir = area_calc_cache)) {
        return(get(cache_key, envir = area_calc_cache))
      }
    }
    
    tryCatch({
      # Use matrix classification for 100% predictability
      r_class <- classify(r_obj[[1]], params$rcl_mat, right = FALSE)
      
      # Use high-performance, geodetically accurate terra::expanse() to calculate hectares
      # This handles both decimal degree grids and planar metric grids correctly and seamlessly!
      area_df <- as.data.frame(expanse(r_class, unit = "ha", byValue = TRUE))
      
      full_res <- data.frame(value = as.numeric(1:params$n_c), Class = params$labels)
      
      if(!"value" %in% names(area_df)) {
        res_df <- data.frame(Class = params$labels, Ha = 0)
        if (!is.null(r_id)) {
          assign(cache_key, res_df, envir = area_calc_cache)
        }
        return(res_df)
      }
      
      # Clean area_df: handle factors/character vs numeric values
      is_label <- any(as.character(area_df$value) %in% params$labels)
      if (is_label) {
         area_df$value <- match(as.character(area_df$value), params$labels)
      } else {
         area_df$value <- as.numeric(as.character(area_df$value))
      }
      
      area_df <- area_df[!is.na(area_df$value), ]
      
      # Rename and aggregate the computed area column (which is 'area') to 'Ha'
      area_df <- area_df %>%
        group_by(value) %>%
        summarise(Ha = round(sum(area, na.rm = TRUE), 2), .groups = "drop")

      # Use left_join to strictly keep only defined classes
      res_df <- full_res %>%
        left_join(area_df, by = "value") %>%
        mutate(Ha = ifelse(is.na(Ha), 0, Ha)) %>%
        select(Class, Ha)
      
      if (!is.null(r_id)) {
        assign(cache_key, res_df, envir = area_calc_cache)
      }
      return(res_df)
    }, error = function(e) {
      return(data.frame(Error = as.character(e$message)))
    })
  }
  area_df_total_act <- reactive({
    req(rv$rast)
    calc_area_df(rv$rast, "total_act")
  })
  
  area_df_total_pre <- reactive({
    req(rv$rast_pred)
    calc_area_df(rv$rast_pred, "total_pre")
  })

  output$area_table_total_act <- renderTable({ req(input$color_style == "agro"); area_df_total_act() })
  output$area_table_total_pre <- renderTable({ req(input$color_style == "agro"); area_df_total_pre() })
  
  output$area_table_loc_act <- renderTable({
    req(rv$rast_list_act, input$color_style == "agro"); loc <- input$sel_loc_stats
    if(loc == "Total (Combined)") return(NULL) else calc_area_df(rv$rast_list_act[[loc]], paste0("loc_act_", loc))
  })
  output$area_table_loc_pre <- renderTable({
    req(rv$rast_list_pre, input$color_style == "agro"); loc <- input$sel_loc_stats
    if(loc == "Total (Combined)") return(NULL) else calc_area_df(rv$rast_list_pre[[loc]], paste0("loc_pre_", loc))
  })

  output$metrics_table <- renderTable({
    req(input$sel_loc_stats)
    loc <- input$sel_loc_stats
    
    # helper to build df row
    get_metrics_df <- function(cv_list, data_list, label) {
      if(loc == "Total (Combined)") {
        # Mathematically correct pooling, preserving coordinates if available
        all_cv <- do.call(rbind, lapply(data_list, function(x) {
          if(inherits(x, "sf")) {
            coords <- sf::st_coordinates(x)
            df <- sf::st_drop_geometry(x)
            if(!"x" %in% colnames(df)) df$x <- coords[,1]
            if(!"y" %in% colnames(df)) df$y <- coords[,2]
            return(df)
          } else if(inherits(x, "Spatial")) {
            return(as.data.frame(x))
          } else {
            return(as.data.frame(x))
          }
        }))
        if(is.null(all_cv) || nrow(all_cv) == 0) {
          empty_df <- data.frame(Source=label, RMSE=NA, R2_Corr=NA, R2_NSE=NA, Bias_ME=NA, RPD_Prec=NA, SMAPE_Pct=NA, Moran_I=NA)
          names(empty_df) <- c("Source", "RMSE", "R  (Corr)", "R  (NSE/Trad)", "Bias (ME)", "RPD (Prec)", "SMAPE (%)", "Moran's I")
          return(empty_df)
        }
        
        # Use centralized perform_cv for pooled metrics
        res <- perform_cv(all_cv)
        rmse <- res$rmse
        r2 <- res$r2
        nse <- res$nse
        me <- res$me
        rpd <- res$rpd
        smape <- res$smape
        moran_i <- res$moran_i
      } else {
        res <- cv_list[[loc]]
        rmse <- if(!is.null(res)) res$rmse else NA
        r2   <- if(!is.null(res)) res$r2 else NA
        nse  <- if(!is.null(res)) res$nse else NA
        me   <- if(!is.null(res)) res$me else NA
        rpd  <- if(!is.null(res)) res$rpd else NA
        smape <- if(!is.null(res)) res$smape else NA
        moran_i <- if(!is.null(res)) res$moran_i else NA
      }
                  res_df <- data.frame(
                    Source = label,
                    RMSE = round(rmse, 4),
                    R2_Corr = round(r2, 4),
                    R2_NSE = round(nse, 4),
                    Bias_ME = round(me, 4),
                    RPD_Prec = round(rpd, 4),
                    SMAPE_Pct = round(smape, 4),
                    Moran_I = if(is.na(moran_i)) '<span title="No Spatial Structure Detected">NA*</span>' else as.character(round(moran_i, 4))
                    )
                    names(res_df) <- c("Source", "RMSE", "R  (Corr)", "R  (NSE/Trad)", "Bias (ME)", "RPD (Prec)", "SMAPE (%)", "Moran's I")
                    res_df
                    }

                    m_act <- get_metrics_df(rv$cv_metrics_act, rv$cv_data_act, "Actual Model (CV)")
                    if(rv$has_predictions) {
                    m_pre <- get_metrics_df(rv$cv_metrics_pre, rv$cv_data_pre, "Predicted Model (CV)")
                    return(rbind(m_act, m_pre))
                    } else {
                    return(m_act)
                    }        }, sanitize.text.function = function(x) x)      
        output$uploaded_metrics_table <- renderTable({
          req(rv$sf, input$sel_loc_stats)
          loc <- input$sel_loc_stats
          
          df <- rv$sf %>% st_drop_geometry() %>% filter(!is.na(v), !is.na(pv))
          if(loc != "Total (Combined)") {
            df <- df %>% filter(loc == !!loc)
          }
          
          if(nrow(df) < 3) return(data.frame(Status = "Not enough data points for numeric metrics."))
          
          rmse_val <- tryCatch(yardstick::rmse_vec(df$v, df$pv), error = function(e) NA)
          rsq_val <- tryCatch(yardstick::rsq_vec(df$v, df$pv), error = function(e) NA)
          rsq_trad <- tryCatch(yardstick::rsq_trad_vec(df$v, df$pv), error = function(e) NA)
          mae_val <- tryCatch(yardstick::mae_vec(df$v, df$pv), error = function(e) NA)
          mbe_val <- mean(df$pv - df$v, na.rm = TRUE)
          ccc_val <- tryCatch(yardstick::ccc_vec(df$v, df$pv), error = function(e) NA)
          rpd_val <- tryCatch(yardstick::rpd_vec(df$v, df$pv), error = function(e) NA)
          rpiq_val <- tryCatch(yardstick::rpiq_vec(df$v, df$pv), error = function(e) NA)
          smape_val <- tryCatch(yardstick::smape_vec(df$v, df$pv), error = function(e) NA)
          
          mean_v <- mean(df$v, na.rm=TRUE)
          nrmse_val <- if(!is.na(rmse_val) && mean_v != 0) (rmse_val / mean_v) * 100 else NA
          nmae_val <- if(!is.na(mae_val) && mean_v != 0) (mae_val / mean_v) * 100 else NA
          
              data.frame(
                Metric = c("R  (NSE/Traditional)", "R  (Correlation)", "RMSE", "NRMSE (%)", "MAE", "NMAE (%)", "MBE (Bias)", "Lin's CCC (Agree)", "RPD (Precision)", "RPIQ", "SMAPE (%)"),
                Value = c(round(rsq_trad, 4), round(rsq_val, 4), round(rmse_val, 4), round(nrmse_val, 4), round(mae_val, 4), round(nmae_val, 4), round(mbe_val, 4), round(ccc_val, 4), round(rpd_val, 4), round(rpiq_val, 4), round(smape_val, 4))
              )        })
  output$kappa_table <- renderTable({
    req(rv$sf, input$sel_loc_stats, input$kappa_bin_method)
    
    loc <- input$sel_loc_stats
    
    # Filter points
    df <- rv$sf %>% st_drop_geometry() %>% filter(!is.na(v), !is.na(pv))
    if(loc != "Total (Combined)") {
      df <- df %>% filter(loc == !!loc)
    }
    
    if(nrow(df) < 3) return(data.frame(Status = "Not enough data points for Kappa."))
    
    # Binning Logic
    if (input$kappa_bin_method == "agro") {
      params <- tryCatch(agro_params(), condition = function(c) NULL)
      if(is.null(params) || input$color_style != "agro") return(data.frame(Status = "Please select Agronomical Classes style for this method."))
      
      # Use cut() with extended range to avoid NA for out-of-bounds predictions
      breaks <- c(-Inf, params$rcl_mat[-1, 1], Inf)
      labels <- params$labels
      
      df$act_bin <- cut(df$v, breaks = breaks, labels = labels, include.lowest = TRUE)
      df$pred_bin <- cut(df$pv, breaks = breaks, labels = labels, include.lowest = TRUE)
      
      df <- df[!is.na(df$act_bin) & !is.na(df$pred_bin), ]
      df$act_bin <- factor(df$act_bin, levels = labels)
      df$pred_bin <- factor(df$pred_bin, levels = labels)
      
    } else {
      # Quartile Binning
      # Always base quartiles on the actual target values distribution
      brks <- unique(quantile(df$v, probs = seq(0, 1, 0.25), na.rm = TRUE))
      if(length(brks) < 2) return(data.frame(Status = "Not enough variance for quartiles."))
      
      # Extend boundaries to handle out-of-range predictions
      brks_ext <- brks
      brks_ext[1] <- -Inf
      brks_ext[length(brks_ext)] <- Inf
      
      lvl <- paste0("Q", 1:(length(brks)-1))
      df$act_bin <- cut(df$v, breaks = brks_ext, include.lowest = TRUE, labels = lvl)
      df$pred_bin <- cut(df$pv, breaks = brks_ext, include.lowest = TRUE, labels = lvl)
      
      df <- df[!is.na(df$act_bin) & !is.na(df$pred_bin), ]
      df$act_bin <- factor(df$act_bin, levels = lvl)
      df$pred_bin <- factor(df$pred_bin, levels = lvl)
    }
    
    if(nrow(df) < 3) return(data.frame(Status = "Not enough data after binning."))
    
    k_unw <- tryCatch(yardstick::kap_vec(df$act_bin, df$pred_bin), error = function(e) NA)
    k_lin <- tryCatch(yardstick::kap_vec(df$act_bin, df$pred_bin, weighting = "linear"), error = function(e) NA)
    acc   <- tryCatch(yardstick::accuracy_vec(df$act_bin, df$pred_bin), error = function(e) NA)
    b_acc <- tryCatch(yardstick::bal_accuracy_vec(df$act_bin, df$pred_bin), error = function(e) NA)
    mcc   <- tryCatch(yardstick::mcc_vec(df$act_bin, df$pred_bin), error = function(e) NA)
    
    data.frame(
      Metric = c("Overall Accuracy", "Balanced Accuracy", "Matthews Corr. Coef. (MCC)", "Kappa (Unweighted)", "Weighted Kappa (Linear)"),
      Value = c(round(acc, 4), round(b_acc, 4), round(mcc, 4), round(k_unw, 4), round(k_lin, 4))
    )
  })

  output$log_output <- renderText({ rv$log })

  # Scan rv$log for new warnings and display UI notifications
  last_notified_warnings <- reactiveVal(character(0))
  observe({
    req(rv$log)
    log_lines <- unlist(strsplit(rv$log, "\n", fixed = TRUE))
    warn_lines <- grep("\\[WARN\\]", log_lines, value = TRUE)
    new_warns <- setdiff(warn_lines, last_notified_warnings())
    if (length(new_warns) > 0) {
      for (w in new_warns) {
        showNotification(gsub("\\[WARN\\]", "", w), type = "warning", duration = 15)
      }
      last_notified_warnings(union(last_notified_warnings(), new_warns))
    }
  })

  # --- Export ---
  build_export_plot <- function(target, title, vv_scale = NULL, subtitle = NULL) {
    if(is.null(target)) return(ggplot() + annotate("text", x=0.5, y=0.5, label="Awaiting model results...") + theme_void())
    meta <- get_current_meta()
    if(is.null(meta)) return(ggplot() + annotate("text", x=0.5, y=0.5, label="Awaiting metadata...") + theme_void())
    
    current_method <- rv$run_method[[input$var_id]]
    is_uncertainty <- isTruthy(input$show_uncertainty) && current_method %in% c("OK", "RK", "RFK", "CK")
    if (is_uncertainty && input$value_type != "resid" && input$exp_layer != "resid") {
        if (input$uncertainty_type == "se") meta$label <- paste(meta$label, "(Std Error)")
        else meta$label <- paste(meta$label, "(Variance)")
        meta$palette <- "inferno"
    }
    
    is_viridis <- meta$palette == "viridis" || meta$palette == "inferno"
    
    p <- tryCatch({
      if(input$color_style == "agro" && !is_uncertainty && input$value_type != "resid" && input$exp_layer != "resid") {
        params <- agro_params()
        if(is.null(params)) return(ggplot() + annotate("text", x=0.5, y=0.5, label="Awaiting agronomical parameters...") + theme_void())
        # 100% Robust approach: convert to dataframe for geom_tile
        # Fix Area Coverage Bug: Subset target to first layer
        target_c <- classify(target[[1]], params$rcl_mat, right = FALSE)
        df_plot <- as.data.frame(target_c, xy = TRUE)
        if(nrow(df_plot) == 0) return(ggplot() + annotate("text", x=0.5, y=0.5, label="No data in classified raster.") + theme_void())
        colnames(df_plot) <- c("x", "y", "val")
        
        # Calculate Ha coverage for legend zero indicators
        cell_area_ha <- prod(res(target)) / 10000
        ha_counts <- as.numeric(table(factor(df_plot$val, levels = 1:params$n_c))) * cell_area_ha
        new_labels <- sapply(1:params$n_c, function(i) {
          if (ha_counts[i] == 0) paste0(params$leg_labels[i], " (0 ha)")
          else paste0(params$leg_labels[i], "       ")
        })
        
        # Add dummy rows for any missing level to force it into the legend
        missing_levels <- which(ha_counts == 0)
        if(length(missing_levels) > 0) {
          dummy_df <- data.frame(x = NA, y = NA, val = missing_levels)
          df_plot <- rbind(df_plot, dummy_df)
        }
        
        df_plot$val <- factor(df_plot$val, levels = 1:params$n_c, labels = new_labels)
        
        ggplot(df_plot, aes(x = x, y = y, fill = val)) + 
          geom_tile() + 
          scale_fill_manual(values = setNames(params$colors, new_labels), limits = new_labels, na.value = "transparent", name = meta$label, drop = FALSE) +
          coord_equal()
      } else if(input$value_type == "resid" || (input$exp_layer == "resid")) {
        # Handle Residuals in Export
        vv <- as.vector(values(target, na.rm=TRUE))
        abs_max <- max(abs(vv), na.rm = TRUE)
        if(is.infinite(abs_max) || is.na(abs_max)) abs_max <- 1
        ggplot() + geom_spatraster(data = target) + 
          scale_fill_distiller(palette = "RdBu", direction = 1, limits = c(-abs_max, abs_max), 
                               na.value = "transparent", name = "Residual")
      } else if(input$color_style == "bin") {
        bp <- ggplot() + geom_spatraster(data = target)
        if(is_viridis) bp + scale_fill_viridis_b(option = meta$palette, name = meta$unit, na.value = "transparent", n.breaks = 5, limits = if(!is.null(vv_scale)) range(vv_scale, na.rm=T) else NULL)
        else bp + scale_fill_fermenter(palette = meta$palette, direction = 1, name = meta$unit, na.value = "transparent", n.breaks = 5, limits = if(!is.null(vv_scale)) range(vv_scale, na.rm=T) else NULL)
      } else {
        bp <- ggplot() + geom_spatraster(data = target)
        if(is_viridis) bp + scale_fill_viridis_c(option = meta$palette, name = meta$unit, na.value = "transparent", limits = if(!is.null(vv_scale)) range(vv_scale, na.rm=T) else NULL)
        else bp + scale_fill_distiller(palette=meta$palette, direction=1, name=meta$unit, na.value="transparent", limits = if(!is.null(vv_scale)) range(vv_scale, na.rm=T) else NULL)
      }
    }, error = function(e) {
      ggplot() + annotate("text", x=0.5, y=0.5, label=paste("Plot Error:", e$message)) + theme_void()
    })
    
    font_base <- if (is.null(input$exp_font_base)) 11 else input$exp_font_base
    title_size <- if (is.null(input$exp_title_size)) 14 else input$exp_title_size
    exp_scale <- isTruthy(input$exp_scale)

    p <- p + theme_minimal(base_size = font_base) +
        theme(plot.title = element_text(size = title_size, face = "bold"),
              plot.subtitle = element_text(size = font_base, face = "italic"),
              axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1),
              legend.text = element_text(angle = 90, hjust = 0.5),
              legend.title = element_text(angle = 90, hjust = 0.5),
              plot.margin = ggplot2::margin(40, 10, 10, 10)) +
        labs(title = title, subtitle = subtitle)

    if(exp_scale) p <- p + coord_sf(clip = "off") + annotation_scale(location = "bl", width_hint = 0.4, pad_y = unit(-2.5, "cm")) + theme(plot.margin = ggplot2::margin(10, 10, 70, 10))
    p  }

  get_export_plot <- function() {
    meta <- get_current_meta()
    if(is.null(meta)) return(ggplot() + annotate("text", x=0.5, y=0.5, label="Please select a variable.") + theme_void())
    
        current_method <- rv$run_method[[input$var_id]]
        method_lab <- if(!is.null(current_method)) {
          m_name <- get_method_label(current_method)
          paste0("Method: ", m_name)
        } else NULL
    
        get_active_layer <- function(r_obj) {
          if(is.null(r_obj)) return(NULL)
          r_w <- r_obj
          if(inherits(r_w, "PackedSpatRaster")) r_w <- terra::unwrap(r_w)
          is_uncertainty <- isTruthy(input$show_uncertainty) && current_method %in% c("OK", "RK", "RFK", "CK") && "var1.var" %in% names(r_w)
          if (is_uncertainty) {
            layer <- r_w[["var1.var"]]
            if (input$uncertainty_type == "se") layer <- sqrt(layer)
            return(layer)
          } else {
            if("var1.pred" %in% names(r_w)) return(r_w[["var1.pred"]]) else return(r_w[[1]])
          }
        }
    
        if(input$exp_layer == "comp") {
          r_act <- get_active_layer(rv$rast)
          r_pre <- get_active_layer(rv$rast_pred)
          v_scale <- if(input$match_scales && !is.null(r_act) && !is.null(r_pre)) {
            c(as.vector(values(r_act, na.rm=T)), as.vector(values(r_pre, na.rm=T)))
          } else NULL
              p1 <- build_export_plot(r_act, paste0("[", meta$category, "] ", meta$label, " - Actual"), v_scale, subtitle = method_lab)
              p2 <- build_export_plot(r_pre, paste0("[", meta$category, "] ", meta$label, " - Predicted"), v_scale, subtitle = method_lab)
              return(p1 + p2 + plot_layout(ncol = 2, guides = "collect") & theme(legend.position = "bottom"))
            }
            
            target_raw <- switch(input$exp_layer,
              "act" = rv$rast,
              "pre" = rv$rast_pred,
              "resid" = rv$rast_res,
              "view" = if(input$value_type == "actual") rv$rast else if(input$value_type == "resid") rv$rast_res else rv$rast_pred,
              rv$rast # fallback
            )
            target <- get_active_layer(target_raw)
            
            # Derive title for single view
            single_title <- if(input$exp_title == "Soil Mapping") {
               paste0("[", meta$category, "] ", meta$label, " - ", input$exp_layer)
            } else input$exp_title
            
            build_export_plot(target, single_title, subtitle = method_lab)  }
  # --- Polygon Drawing & Export Logic ---
  observeEvent(input$main_map_draw_new_feature, {
    feat <- input$main_map_draw_new_feature
    if(feat$geometry$type %in% c("Polygon", "MultiPolygon")) {
      rv$drawn_polygons[[as.character(feat$properties$`_leaflet_id`)]] <- feat
    }
  })
  
  observeEvent(input$main_map_draw_edited_features, {
    feats <- input$main_map_draw_edited_features$features
    for (feat in feats) {
      if(feat$geometry$type %in% c("Polygon", "MultiPolygon")) {
        rv$drawn_polygons[[as.character(feat$properties$`_leaflet_id`)]] <- feat
      }
    }
  })
  
  observeEvent(input$main_map_draw_deleted_features, {
    feats <- input$main_map_draw_deleted_features$features
    for (feat in feats) {
      rv$drawn_polygons[[as.character(feat$properties$`_leaflet_id`)]] <- NULL
    }
  })
  
  get_drawn_sf <- reactive({
    polys <- rv$drawn_polygons
    if(length(polys) == 0) return(NULL)
    
    # Convert list of GeoJSON features to an sf object
    sf_list <- lapply(polys, function(p) {
      json_str <- jsonlite::toJSON(p, auto_unbox = TRUE)
      sf::st_read(json_str, quiet = TRUE)
    })
    
    sf_combined <- do.call(rbind, sf_list)
    sf::st_crs(sf_combined) <- 4326 # Leaflet uses WGS84
    return(sf_combined)
  })
  
  output$polygon_download_btn <- downloadHandler(
    filename = function() {
      fmt <- input$polygon_export_format
      ext <- switch(fmt, "shp" = "zip", "geojson" = "geojson", "kml" = "kml", "gpkg" = "gpkg", "zip")
      paste0("Drawn_Polygons_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".", ext)
    },
    content = function(file) {
      sf_obj <- get_drawn_sf()
      if(is.null(sf_obj)) {
        showNotification("No polygons to export. Please draw a polygon first.", type = "warning")
        return(NULL)
      }
      
      fmt <- input$polygon_export_format
      
      tryCatch({
        if (fmt == "shp") {
          temp_dir <- file.path(tempdir(), paste0("shp_export_", as.integer(Sys.time())))
          dir.create(temp_dir, showWarnings = FALSE)
          shp_path <- file.path(temp_dir, "drawn_polygons.shp")
          
          sf::st_write(sf_obj, shp_path, driver = "ESRI Shapefile", quiet = TRUE, delete_layer = TRUE)
          
          # Zip the files
          files_to_zip <- list.files(temp_dir, full.names = FALSE)
          zip::zip(zipfile = file, files = files_to_zip, root = temp_dir)
          
          unlink(temp_dir, recursive = TRUE)
        } else if (fmt == "geojson") {
          sf::st_write(sf_obj, file, driver = "GeoJSON", quiet = TRUE, delete_dsn = TRUE)
        } else if (fmt == "kml") {
          sf::st_write(sf_obj, file, driver = "KML", quiet = TRUE, delete_dsn = TRUE)
        } else if (fmt == "gpkg") {
          sf::st_write(sf_obj, file, driver = "GPKG", layer = "drawn_polygons", quiet = TRUE, delete_dsn = TRUE)
        }
      }, error = function(e) {
        showNotification(paste("Export failed:", e$message), type = "error")
      })
    }
  )

  output$export_preview <- renderPlot({ get_export_plot() })
  output$dl_map <- downloadHandler(
    filename = function() { paste0("Map_", format(Sys.time(), "%H%M%S"), ".", tolower(input$exp_format %||% "png")) },
    content = function(file) {
      p <- get_export_plot()
      
      ext <- tolower(input$exp_format %||% "png")
      if (inherits(p, "trellis")) {
        if (ext == "png") png(file, width = (if(isTruthy(input$styler_width)) input$styler_width else 10), height = (if(isTruthy(input$styler_height)) input$styler_height else 8), units = "in", res = 300)
        else if (ext == "tiff") tiff(file, width = (if(isTruthy(input$styler_width)) input$styler_width else 10), height = (if(isTruthy(input$styler_height)) input$styler_height else 8), units = "in", res = 300)
        else if (ext == "pdf") pdf(file, width = (if(isTruthy(input$styler_width)) input$styler_width else 10), height = (if(isTruthy(input$styler_height)) input$styler_height else 8))
        else jpeg(file, width = (if(isTruthy(input$styler_width)) input$styler_width else 10), height = (if(isTruthy(input$styler_height)) input$styler_height else 8), units = "in", res = 300)
        print(p)
        dev.off()
      } else {
      ggsave(file, plot = p, device = if(ext == "pdf") "pdf" else (if(ext == "tiff") "tiff" else NULL), width = input$styler_width %||% 10, height = (if(isTruthy(input$styler_height)) input$styler_height else 8), dpi = 300)
      }
      }
      )
      }
      if (Sys.getenv("SHINY_PORT") != "" || interactive()) {  shinyApp(ui, server)
}
shinyApp(ui = ui, server = server)
