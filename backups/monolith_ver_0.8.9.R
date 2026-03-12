library(shiny)
library(leaflet)
library(leaflet.extras)
library(sf)
library(terra)
library(dplyr)
library(ggplot2)
library(readxl)
library(gstat)
library(yardstick)
library(RColorBrewer)
library(concaveman)
library(tidyterra)
library(fields)
library(viridis)
library(shinyFiles)
library(latticeExtra)
library(classInt)
library(jsonlite)
library(shinyWidgets)
library(shinyjs)
library(patchwork)
library(automap)
library(randomForest)
library(ggspatial)
library(future)
library(progressr)
library(promises)
library(shinycssloaders)
library(furrr)
library(showtext)
library(openxlsx)
library(officer)
library(zip)
showtext_auto()
addResourcePath("assets", getwd())
plan(list(multisession, multisession)) # Enable nested async processing
# --- Improvements---
source("improvements/ui_helpers_0.8.9.R")
source("improvements/spatial_helpers_0.8.9.R")
source("improvements/theme_helpers_0.8.9.R")

# --- Helpers ---
`%||%` <- function(a, b) if (!is.null(a)) a else b

# Universal Agronomical Colors
agro_colors <- c("#E69F00", "#F0E442", "#009E73") # Orange, Yellow, Green

# Classic Nutrient Palettes
nutrient_palettes <- list(
  TN = "Greens", P = "Blues", K = "Oranges", Ca = "YlOrRd", 
  Mg = "PuBuGn", Fe = "Purples", Mn = "GnBu", Cu = "YlGn", Zn = "YlOrBr"
)

# Classic Agronomical Limits
nutrient_limits <- list(
  TN = c(0.05, 0.10), P = c(8, 25), K = c(150, 300), Ca = c(1428, 2857),
  Mg = c(80, 160), Fe = c(4, 6), Mn = c(1.2, 3.5), Cu = c(0.3, 0.8), Zn = c(1, 3)
)

common_crs <- c(
  "WGS 84 (EPSG:4326)" = "EPSG:4326",
  "UTM 35N (EPSG:32635)" = "EPSG:32635",
  "UTM 33N (EPSG:32633)" = "EPSG:32633",
  "UTM 34N (EPSG:32634)" = "EPSG:32634",
  "S-JTSK / Krovak East North (EPSG:5514)" = "EPSG:5514",
  "Pseudo-Mercator (EPSG:3857)" = "EPSG:3857"
)

get_nut_key <- function(v) {
  v_up <- toupper(as.character(v))
  if (grepl("\\bTN\\b|NITROGEN", v_up)) return("TN")
  if (grepl("\\bP\\b|PHOSPHORUS|OLSEN", v_up)) return("P")
  if (grepl("\\bK\\b|POTASSIUM", v_up)) return("K")
  if (grepl("\\bCA\\b|CALCIUM", v_up)) return("Ca")
  if (grepl("\\bMG\\b|MAGNESIUM", v_up)) return("Mg")
  if (grepl("\\bFE\\b|IRON", v_up)) return("Fe")
  if (grepl("\\bMN\\b|MANGANESE", v_up)) return("Mn")
  if (grepl("\\bCU\\b|COPPER", v_up)) return("Cu")
  if (grepl("\\bZN\\b|ZINC", v_up)) return("Zn")
  return(NULL)
}

get_default_palette <- function(var_name, category = "Soil", label = NULL) {
  # Match nutrient shorthand robustly (check name then label)
  nut <- get_nut_key(var_name)
  if (is.null(nut) && !is.null(label)) nut <- get_nut_key(label)
  
  if (!is.null(nut)) return(nutrient_palettes[[nut]])
  
  # Category-based defaults
  case_when(
    category == "Environmental Data" ~ "RdYlBu",
    category == "Landsat Data" ~ "viridis",
    category == "Sentinel Data" ~ "plasma",
    category == "Merged Data" ~ "inferno",
    category == "Terrain Data" ~ "BrBG",
    TRUE ~ "YlOrRd"
  )
}

# --- Palette Helpers ---
# Mandatory palettes for (Simplified + Earthy colors)
dashboard_palettes <- c("viridis", "Greens", "Blues", "Oranges", "YlOrRd", "RdYlBu", "BrBG", "YlOrBr", "Greys", "Spectral")

render_palette_choices <- function() {
  pals <- dashboard_palettes
  # Map internal names to HTML labels for pickerInput
  labels <- sapply(pals, function(p) {
    # Generate a few colors for the visual preview
    cols <- if (p == "viridis") {
      viridis::viridis(5)
    } else {
      RColorBrewer::brewer.pal(5, p)
    }
    # Create the HTML swatch row
    swatches <- paste0(sapply(cols, function(c) {
      sprintf('<div style="width: 15px; height: 15px; background-color: %s !important; border: 0.5px solid #ccc; display: inline-block; margin-left: 2px;"></div>', c)
    }), collapse = "")
    sprintf('<div style="display: flex; justify-content: space-between; align-items: center; width: 100%%;"><span>%s</span><div style="display: flex;">%s</div></div>', p, swatches)
  })
  setNames(pals, labels)
}

# --- Scientific Variogram Parameters ---
calc_scientific_lags <- function(sf_pts) {
  # Reputable heuristic: cutoff = max distance / 2, width = cutoff / 15
  bbox <- st_bbox(sf_pts)
  max_dist <- as.numeric(sqrt((bbox$xmax - bbox$xmin)^2 + (bbox$ymax - bbox$ymin)^2))
  cutoff <- max_dist / 2
  list(width = cutoff / 15, cutoff = cutoff)
}

# --- Robust Variogram Fitting ---
robust_vgm_fit <- function(v_emp, v_data) {
  initial_sill <- var(v_data, na.rm=TRUE)
  if (is.na(initial_sill) || initial_sill == 0) initial_sill <- 1
  
  initial_nugget <- min(v_emp$gamma)
  # Stability fix: Ensure a tiny nugget to prevent singular matrices in krige()
  if (initial_nugget == 0) initial_nugget <- initial_sill * 1e-6
  
  if (initial_nugget > initial_sill) initial_nugget <- initial_sill * 0.9
  initial_psill <- max(initial_sill - initial_nugget, initial_sill * 0.1)
  
  initial_range <- max(v_emp$dist) / 4
  models <- c("Sph", "Exp", "Gau", "Mat") # Added Matern
  
  fits <- furrr::future_map(models, function(m) {
    tryCatch({
      # Try fitting with initial guesses
      start_kappa <- if(m == "Mat") 1.5 else 0.5
      f <- gstat::fit.variogram(v_emp, gstat::vgm(psill = initial_psill, model = m, range = initial_range, nugget = initial_nugget, kappa = start_kappa))
      sse <- attr(f, "SSErr")
      if (!is.null(sse) && f$range[2] > (max(v_emp$dist)/100) && f$range[2] < max(v_emp$dist) * 2 && f$psill[2] > 0) {
        return(list(fit = f, sse = sse))
      }
      return(NULL)
    }, error = function(e) NULL)
  }, .options = furrr::furrr_options(seed = TRUE, packages = c("gstat")))
  
  valid_fits <- Filter(Negate(is.null), fits)
  best_fit <- NULL
  if (length(valid_fits) > 0) {
    best_idx <- which.min(sapply(valid_fits, function(x) x$sse))
    best_fit <- valid_fits[[best_idx]]$fit
  }
  
  if (is.null(best_fit)) {
    if (initial_nugget > initial_sill * 0.8) {
      best_fit <- gstat::vgm(psill = initial_sill * 0.05, "Sph", range = max(v_emp$dist)/10, nugget = initial_sill * 0.95)
    } else {
      best_fit <- gstat::vgm(psill = initial_sill * 0.8, "Sph", range = max(v_emp$dist)/2, nugget = initial_sill * 0.2)
    }
    tryCatch({ showNotification("Variogram auto-fit failed. Using fallback.", type = "warning", duration = 5) }, error=function(e) NULL)
  }
  return(best_fit)
}

# --- UI ---
ui <- fluidPage(
  useShinyjs(),
  render_docs_drawer(),
  uiOutput("dynamic_theme"),
  tags$head(
    tags$style(HTML("
      .shiny-notification { position:fixed; top: 50%; left: 50%; transform: translate(-50%, -50%); width: 350px; z-index: 9999; }
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
    ")),
    uiOutput("dynamic_manual_style"),
    tags$script(HTML("$(function () { $('[data-toggle=\"popover\"]').popover({html: true}); });"))
  ),
  
  div(class = "header-panel", style = "display: flex; justify-content: space-between; align-items: center; padding: 5px 20px;",
      img(src = "assets/banner.png", class = "header-banner", style = "max-height: 50px; width: auto; object-fit: contain; float: left;"),
      div(style = "flex-grow: 1;"),
      div(class = "header-controls", style = "display: flex; align-items: center; gap: 15px; margin-left: auto;",
          tags$style(HTML(".header-controls .form-group { margin-bottom: 0 !important; } .header-controls .checkbox { margin-top: 2px !important; margin-bottom: 2px !important; }")),
          theme_switcher_ui("theme_mod"),
          div(style = "display: flex; flex-direction: column; align-items: flex-start; font-size: 0.8em; line-height: 1;",
              checkboxInput("show_north", "North Arrow", FALSE),
              checkboxInput("show_borders", "Borders", FALSE),
              checkboxInput("show_scale", "Map Scale", FALSE)
          ),
          actionButton("info_btn", "", icon = icon("info-circle"), style = "background: none; border: none; color: white; font-size: 32px; padding: 0; cursor: pointer; margin-left: 10px;"),
          actionButton("about_btn", "", icon = icon("question-circle"), style = "background: none; border: none; color: white; font-size: 32px; padding: 0; cursor: pointer; margin-left: 10px;")
      )
  ),
  
  sidebarLayout(
    sidebarPanel(width = 3,
      div(style="background-color: #f8f9fa; padding: 10px; border: 1px solid #ddd;",
          h4("1. Context"),
          selectInput("locality", "Locality", choices = "Upload data first...", multiple = TRUE),
          selectInput("subset", "Data Subset", choices = c("All" = "all", "Test" = "Test", "Train" = "Train", "Validation" = "Validation"), selected = "all"),
          selectInput("var_category", "Variable Category", choices = NULL),
          selectInput("var_id", "Variable", choices = NULL),
                     selectInput("value_type", "Primary View", choices = c("Actual Values" = "actual", "Best Predictions (_cve)" = "pred", "Single Split Predictions (_ss)" = "pred_ss", "Residuals (v - pv)" = "resid")),
                     conditionalPanel(
                       condition = "['pred', 'pred_ss', 'resid'].includes(input.value_type)",
                       checkboxInput("comp_mode", HTML(paste0("Comparison Mode", info_tooltip("comp_mode", "Splits the viewer to compare Actual vs. Predicted maps. Useful for visual validation."))), FALSE)
                     ),          conditionalPanel(condition = "input.comp_mode && ['pred', 'pred_ss'].includes(input.value_type)", 
                           checkboxInput("sep_fit", HTML(paste0("Fit Actual/Predicted Separately", info_tooltip("sep_fit_info", "If checked, optimizes variograms separately for actual and predicted data. If unchecked, applies actual variogram to predictions."))), TRUE),
                           checkboxInput("match_scales", HTML(paste0("Match Scales", info_tooltip("match_info", "Forces the map legends for Actual and Predicted data to use the same color range."))), FALSE))
      ),
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
                  conditionalPanel(condition = "input.comp_mode == true",
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
          numericInput("buff_dist", HTML(paste0("Buffer Distance (m)", info_tooltip("buff_dist_info", "Sets the spatial padding applied around the outer limits of your data points. Higher values extrapolate further into unmeasured territory."))), value = 250, min = 0),
          
          radioButtons("res_mode", HTML(paste0("Resolution Logic", info_tooltip("res", "Dynamic modes calculate cell size based on spatial extent. Manual forces a specific cell size (e.g. 10m)."))), 
                       choices = c("Auto (Per Locality)" = "local", "Auto (Global)" = "global", "Fixed" = "fixed")),
          conditionalPanel(condition = "input.res_mode == 'fixed'",
            sliderInput("grid_res", "Manual Resolution", min = 5, max = 500, value = 50)
          ),
          
          div(style="margin-top: 10px; background-color: #f8f9fa; border: 1px solid #e9ecef; border-radius: 4px; padding: 10px; color: #495057;", tableOutput("loc_res_table")),
          
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
      actionButton("run", "GENERATE MODELS", class = "btn-success btn-lg", style="width:100%;")
    ),
    
    mainPanel(width = 9,
      tabsetPanel(id = "main_tabs",
        tabPanel("1. Data Setup", value = "tab_data",
                 div(style = "padding: 20px; background-color: #f1f3f5; border-radius: 8px; border: 1px solid #dee2e6;",
                     h3("Step 1: Upload Your Dataset"),
                     fluidRow(
                       column(6, fileInput("user_file", "Choose CSV or Excel File", accept = c(".csv", ".xlsx", ".xls"))),
                       column(6, fileInput("user_shp", "Shapefile - Optional (.shp, .shx, .dbf, .prj)", multiple = TRUE, accept = c(".shp", ".shx", ".dbf", ".prj")))
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
                          column(4, style="margin-top: 25px;", tags$p(tags$b("Direction:"), "Wait for the sampling points to appear, and verify your coordinates on the map below. Then, upload your variable list if you would like to use one for automated data categorization."))
                        ),
                        hr(),
                        h3("Step 3: Mini-Map Validation"),
                        leafletOutput("setup_minimap", height = "400px"),
                        hr(),
                        h3("Step 4: Variable Mapping - confirm your variables at the bottom after the upload"),
                        tags$p("Pair your Target (Actual) variables with their Predictions. You can map them manually or upload a metadata file."),
                        fileInput("meta_file", "Upload Variable List (Optional)", accept = c(".xlsx", ".xls", ".csv")),
                        uiOutput("var_mapping_ui")
                     )
                 )
        ),
                tabPanel("2. Map Viewer", value = "tab_map",
                         div(style="position: relative;",
                             div(id="map_reveal_overlay", style="position: absolute; top: 0; left: 0; width: 100%; height: 100%; background-color: rgba(255,255,255,0.9); z-index: 1000; display: none; align-items: center; justify-content: center; flex-direction: column;",
                                 h3("Map Generation Complete", style="margin-bottom: 20px; color: #333;"),
                                 actionButton("reveal_maps_btn", "Click here to view maps and enable scientific analysis", class="btn-primary btn-lg", style="box-shadow: 0 4px 8px rgba(0,0,0,0.2); transition: all 0.3s;")
                             ),
                             div(id="map_progress_overlay", style="position: absolute; top: 0; left: 0; width: 100%; height: 100%; background-color: rgba(255,255,255,0.8); z-index: 1001; display: none; align-items: center; justify-content: center; flex-direction: column;",
                                 h3("Generating Maps...", style="margin-bottom: 15px; color: #555;"),
                                 div(style="width: 50%; max-width: 400px; background-color: #f3f3f3; border-radius: 5px; overflow: hidden;",
                                     div(id="map_progress_bar_fill", style="height: 20px; width: 0%; background-color: #4CAF50; transition: width 0.3s ease;")
                                 ),
                                 p(id="map_progress_text", "Initializing...", style="margin-top: 10px; font-weight: bold; color: #666;")
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
                             actionButton("quick_export_map", "Quick Export", icon = icon("camera"), class = "btn-info btn-sm", title = "Immediately send the currently viewed map to the Export Registry.")
                         ),                         conditionalPanel(condition = "(!input.comp_mode && input.value_type != 'resid') || input.value_type == 'actual'",
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
                  uiOutput("locality_selector_ui"),
                  fluidRow(
                    column(8,
                           conditionalPanel(condition = "input.method == 'OK'",
                             h4("Actual Data Structure"), plotOutput("vgm_plot_main", height = "350px"),
                             div(id = "predicted_data_structure_ui",
                               h4("Predicted Data Structure"), plotOutput("vgm_plot_pred", height = "350px")
                             )
                           ),                           conditionalPanel(condition = "input.method == 'RK'",
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
                              div(id = "ck_pred_ui", h4("Cross-Variogram (Predicted)"), plotOutput("ck_variogram_plot_pre", height = "350px"))
                            ),
                            conditionalPanel(condition = "input.method == 'TPS'",
                              h4("TPS GCV Diagnostics (Actual)"), plotOutput("tps_gcv_plot_act", height = "350px"),
                              div(id = "tps_pred_ui", h4("TPS GCV Diagnostics (Predicted)"), plotOutput("tps_gcv_plot_pre", height = "350px"))
                            ),                           conditionalPanel(condition = "!['OK', 'RK', 'RFK', 'CK', 'TPS'].includes(input.method)",
                             div(style="padding: 20px; text-align: center; color: #666;",
                                 h4("Diagnostic Mode Active"),
                                 p("Detailed spatial diagnostics are currently optimized for Kriging and TPS."))
                           ),
                           hr(),
                           h4("Validation Diagnostics (Actual)"),
                           fluidRow(
                             column(6, plotOutput("obs_pred_plot_act", height = "300px")),
                             column(6, plotOutput("resid_vgm_plot_act", height = "300px"))
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
                           )                    ),
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
                             h5("Model Performance"), div(class="table-container", tableOutput("metrics_table")),
                             div(id = "prediction_performance_ui",
                               hr(style="opacity: 0.3;"),
                               h5("Prediction Performance (Uploaded Data)"),
                               div(class="table-container", tableOutput("uploaded_metrics_table")),
                               hr(style="opacity: 0.3;"),
                               h5("Classification Performance (Uploaded Predictions)"),
                               selectInput("kappa_bin_method", "Binning Method:", choices = c("Agronomical Classes" = "agro", "Quartiles" = "quartile")),
                               div(class="table-container", tableOutput("kappa_table"))
                             )                           ),
                           div(style = "background-color: #e7f5ff; padding: 15px; border: 2px solid #339af0; border-radius: 8px;",
                             h4("Data Summary Statistics"),
                             tags$p(style="font-size: 0.85em; opacity: 0.8; font-style: italic;", "Aggregated descriptive statistics and area coverage for the data."),
                             h5("Area Coverage"),
                             conditionalPanel(condition = "input.locality.length > 1 || (input.locality.length == 1 && input.locality[0] == 'ALL')",
                               fluidRow(
                                 column(6, h6("Total - Actual"), tableOutput("area_table_total_act")),
                                 column(6, div(id = "area_total_pred_col", h6("Total - Predicted"), tableOutput("area_table_total_pre")))
                               )
                             ),                             fluidRow(
                               column(6, h6("Locality - Actual"), tableOutput("area_table_loc_act")),
                               column(6, div(id = "loc_pred_col", h6("Locality - Predicted"), tableOutput("area_table_loc_pre")))
                             ),                             hr(style="border-top: 1px solid #339af0;"),
                             h5("Descriptive Statistics"),
                             conditionalPanel(condition = "input.locality.length > 1 || (input.locality.length == 1 && input.locality[0] == 'ALL')",
                               tableOutput("stats_table_total")
                             ),
                             tableOutput("stats_table_loc")
                           )
                    )
                  ),
                  hr(), verbatimTextOutput("log_output")),
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
                                  )
                                  )),
        tabPanel("5. Descriptive and Exploratory Suite",
                 div(style = "padding: 20px;",
                     h2("Analytics Engine"),
                     p("Explore your data with descriptive statistics, correlation mapping, and principal component analysis. Investigate governing factors on a specific parameter."),
                     hr(),
                     fluidRow(
                       column(12,
                         wellPanel(
                           h4("Data Grouping & Discretization"),
                           fluidRow(
                             column(6, selectInput("analytics_group_vars", "Grouping Variables (Max 5)", choices = NULL, multiple = TRUE)),
                             column(6, uiOutput("analytics_group_types_ui"))
                           ),
                           uiOutput("analytics_group_filter_ui")
                         )
                       )
                     ),
                     hr(),
                     tabsetPanel(id = "scientific_analytics_tabs",
                                 tabPanel("Descriptive Suite",
                                          div(style = "padding: 10px;",
                                              fluidRow(
                                                column(3,
                                                  selectInput("desc_plot_type", "Plot Type", 
                                                    choices = c("Histogram" = "histogram", 
                                                                "Density" = "density", 
                                                                "Boxplot" = "boxplot", 
                                                                "Violin" = "violin", 
                                                                "Scatterplot" = "scatter", 
                                                                "ECDF" = "ecdf",
                                                                "QQ Plot" = "qq",
                                                                "Sina-style Plot" = "sinaplot",
                                                                "Ridge/Joyplot" = "ridge",
                                                                "2D Density Heatmap" = "density_heatmap",
                                                                "Parallel Coordinates" = "parallel",
                                                                "Radar Chart" = "radar",
                                                                "XYZ Surface" = "xyz_surface")),
                                                  checkboxInput("desc_ghosting", "Enable Ghosting (Selected vs. Total)", value = FALSE),
                                                  selectInput("desc_palette", "Color Palette", 
                                                              choices = c("Default" = "default", "Viridis (Colorblind)" = "viridis", "Set1" = "Set1", "Set2" = "Set2", "Dark2" = "Dark2", "Pastel1" = "Pastel1")),
                                                  uiOutput("desc_plot_vars_ui")
                                                ),
                                                column(9,
                                                  div(style = "position: relative;",
                                                      actionButton("desc_expand_plot_btn", "Expand / Interactive", icon = icon("expand"), style = "position: absolute; top: 10px; right: 10px; z-index: 100; opacity: 0.8;"),
                                                      plotOutput("desc_main_plot", height = "500px")
                                                  ),
                                                  hr(),
                                                  h4("Group Statistics"),
                                                  DT::dataTableOutput("desc_summary_table")
                                                )
                                              )
                                          )
                                 ),
                                 tabPanel("Correlation Analysis",
                                          div(style = "padding: 10px;",
                                              fluidRow(
                                                column(3,
                                                  selectInput("corr_plot_type", "Correlation Plot Type", 
                                                    choices = c("Hierarchical Heatmap" = "heatmap",
                                                                "Correlation Network" = "network",
                                                                "Partial Correlation" = "partial",
                                                                "Correlogram" = "correlogram",
                                                                "Lagged CCF" = "lagged")),
                                                  selectInput("corr_method", "Method", choices = c("pearson", "spearman", "kendall")),
                                                  uiOutput("corr_vars_ui")
                                                ),
                                                column(9,
                                                  div(style = "position: relative;",
                                                      actionButton("corr_expand_plot_btn", "Expand / Interactive", icon = icon("expand"), style = "position: absolute; top: 10px; right: 10px; z-index: 100; opacity: 0.8;"),
                                                      plotOutput("corr_main_plot", height = "500px")
                                                  ),
                                                  hr(),
                                                  h4("Correlation Matrix"),
                                                  DT::dataTableOutput("corr_summary_table")
                                                )
                                              )
                                          )
                                 ),
                                 tabPanel("PCA",
                                          div(style = "padding: 10px;",
                                              fluidRow(
                                                column(3,
                                                  h4("PCA Setup"),
                                                  uiOutput("pca_vars_ui"),
                                                  actionButton("run_pca_btn", "Run PCA", class="btn-primary btn-block"),
                                                  hr(),
                                                  conditionalPanel("output.pca_ready == 'yes'",
                                                    selectInput("pca_plot_type", "Plot Type",
                                                                choices = c("Scree Plot" = "scree",
                                                                            "Biplot (2D)" = "biplot",
                                                                            "Loadings" = "loadings",
                                                                            "Contribution" = "contrib",
                                                                            "Cumulative Variance" = "cumvar",
                                                                            "Mahalanobis Distance" = "mahalanobis")),
                                                    uiOutput("pca_plot_controls")
                                                  )
                                                ),
                                                column(9,
                                                  uiOutput("pca_collinearity_warning_ui"),
                                                  div(style = "position: relative;",
                                                      actionButton("pca_expand_plot_btn", "Expand / Interactive", icon = icon("expand"), style = "position: absolute; top: 10px; right: 10px; z-index: 100; opacity: 0.8;"),
                                                      plotOutput("pca_main_plot", height = "500px")
                                                  ),
                                                  hr(),
                                                  conditionalPanel("output.pca_ready == 'yes'",
                                                    h4("PCA Results"),
                                                    DT::dataTableOutput("pca_summary_table")
                                                  )
                                                )
                                              )
                                          )
                                 ),
                                 tabPanel("Governing Factors",
                                          div(style = "padding: 10px;",
                                              fluidRow(
                                                column(3,
                                                  h4("Analysis Configuration"),
                                                  uiOutput("gov_target_ui"),
                                                  uiOutput("gov_predictors_ui"),
                                                  sliderInput("gov_permutations", "Permutations (for RF importance)", min = 10, max = 100, value = 50, step = 10),
                                                  actionButton("gov_run_btn", "Run Analysis", class="btn-primary btn-block"),
                                                  hr(),
                                                  h4("Plot Settings"),
                                                  radioButtons("gov_effect_type", "Functional Effect Plot:", choices = c("ALE" = "ale", "SHAP" = "shap"), inline = TRUE)
                                                ),
                                                column(9,
                                                  conditionalPanel("output.gov_ready == 'yes'",
                                                    fluidRow(
                                                      column(6, 
                                                        h4("Global Importance"),
                                                        div(style = "position: relative;",
                                                            actionButton("gov_expand_imp_btn", "Expand / Interactive", icon = icon("expand"), style = "position: absolute; top: 10px; right: 10px; z-index: 100; opacity: 0.8;"),
                                                            plotOutput("gov_plot_importance", height = "300px")
                                                        )
                                                      ),
                                                      column(6, 
                                                        h4("Causality / Interaction (A)"),
                                                        div(style = "position: relative;",
                                                            actionButton("gov_expand_inta_btn", "Expand / Interactive", icon = icon("expand"), style = "position: absolute; top: 10px; right: 10px; z-index: 100; opacity: 0.8;"),
                                                            plotOutput("gov_plot_interaction_a", height = "300px")
                                                        )
                                                      )
                                                    ),
                                                    hr(),
                                                    fluidRow(
                                                      column(6, 
                                                        h4("Functional Effect"),
                                                        div(style = "position: relative;",
                                                            actionButton("gov_expand_eff_btn", "Expand / Interactive", icon = icon("expand"), style = "position: absolute; top: 10px; right: 10px; z-index: 100; opacity: 0.8;"),
                                                            plotOutput("gov_plot_effect", height = "300px")
                                                        )
                                                      ),
                                                      column(6, 
                                                        h4("Causality / Interaction (B)"),
                                                        div(style = "position: relative;",
                                                            actionButton("gov_expand_intb_btn", "Expand / Interactive", icon = icon("expand"), style = "position: absolute; top: 10px; right: 10px; z-index: 100; opacity: 0.8;"),
                                                            plotOutput("gov_plot_interaction_b", height = "300px")
                                                        )
                                                      )
                                                    ),
                                                    hr(),
                                                    h4("Tabular Data Metrics"),
                                                    DT::dataTableOutput("gov_summary_table")
                                                  )
                                                )
                                              )
                                          )
                                 )
                     )
                 )
        )      )
    )
  )
)

# --- Server ---
server <- function(input, output, session) {

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
    has_interp <- rv$has_predictions
    shinyjs::toggle(id = "predicted_data_structure_ui", condition = has_interp)
    shinyjs::toggle(id = "rk_pred_ui", condition = has_interp)
    shinyjs::toggle(id = "rk_internal_vgm_pre_ui", condition = has_interp)
    shinyjs::toggle(id = "rfk_pred_ui", condition = has_interp)
    shinyjs::toggle(id = "rfk_internal_vgm_pre_ui", condition = has_interp)
    shinyjs::toggle(id = "ck_pred_ui", condition = has_interp)
    shinyjs::toggle(id = "tps_pred_ui", condition = has_interp)
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

  # --- Phase 2: Analytics Engine (Grouping Logic) ---
  observe({
    req(rv$user_data)
    cols <- colnames(rv$user_data)
    # Exclude x, y, and target var if needed, or allow all
    valid_cols <- cols[!grepl("\\bx\\b|\\by\\b|lon|lat|latitude|longitude", cols, ignore.case=TRUE)]
    
    # Use labels if available
    vars_metadata <- rv$mapping$vars
    if (!is.null(vars_metadata)) {
      choices_named <- setNames(valid_cols, sapply(valid_cols, function(v) {
        match <- Filter(function(x) x$actual == v, vars_metadata)
        if(length(match) > 0 && !is.null(match[[1]]$label) && match[[1]]$label != "") {
          match[[1]]$label
        } else {
          v
        }
      }))
    } else {
      choices_named <- valid_cols
    }
    
    updateSelectInput(session, "analytics_group_vars", choices = choices_named)
  })
  
  output$analytics_group_types_ui <- renderUI({
    req(input$analytics_group_vars)
    vars <- input$analytics_group_vars
    lapply(seq_along(vars), function(i) {
      v <- vars[i]
      # Auto-detect default type based on data
      is_num <- is.numeric(rv$user_data[[v]])
      def_type <- if(is_num) "numeric" else "categorical"
      
      div(style="margin-bottom: 5px;",
          selectInput(paste0("grp_type_", i), paste("Type/Binning for:", v),
                       choices = c("Categorical" = "categorical", 
                                   "Numeric: Median" = "numeric_median",
                                   "Numeric: Mean" = "numeric_mean",
                                   "Numeric: Tertiles" = "numeric_tertiles",
                                   "Numeric: Quintiles" = "numeric_quintiles"),
                       selected = if(is_num) "numeric_median" else "categorical")
      )
    })
  })
  
  # Reactive dataset with grouping applied
  rv_analytics_data <- reactive({
    req(rv$user_data)
    df <- rv$user_data
    vars <- input$analytics_group_vars
    
    if (is.null(vars) || length(vars) == 0) {
       df$group_id <- as.factor("All")
       return(df)
    }
    
    types <- sapply(seq_along(vars), function(i) {
      v <- vars[i]
      def <- if(is.numeric(df[[v]])) "numeric_median" else "categorical"
      input[[paste0("grp_type_", i)]] %||% def
    })
    
    # Process using the extracted function in ui_helpers_0.8.0.R
    process_grouping_vars(df, vars, types)
  })
  
  output$analytics_group_filter_ui <- renderUI({
    req(rv_analytics_data())
    df <- rv_analytics_data()
    if ("group_id" %in% colnames(df)) {
      levels_present <- levels(df$group_id)
      selectInput("analytics_active_group", "Select Active Groups to Compare", 
                  choices = levels_present, multiple = TRUE, selected = levels_present)
    }
  })
  
  # --- Phase 3: Plot Rendering ---
  output$desc_plot_vars_ui <- renderUI({
    req(rv$user_data)
    cols <- colnames(rv$user_data)
    num_cols <- cols[sapply(rv$user_data, is.numeric)]
    valid_cols <- cols[!grepl("\\bx\\b|\\by\\b|lon|lat|latitude|longitude", cols, ignore.case=TRUE)]
    
    vars_metadata <- rv$mapping$vars
    if (!is.null(vars_metadata)) {
      valid_named <- setNames(valid_cols, sapply(valid_cols, function(v) {
        match <- Filter(function(x) x$actual == v, vars_metadata)
        if(length(match) > 0 && !is.null(match[[1]]$label) && match[[1]]$label != "") match[[1]]$label else v
      }))
      num_named <- setNames(num_cols, sapply(num_cols, function(v) {
        match <- Filter(function(x) x$actual == v, vars_metadata)
        if(length(match) > 0 && !is.null(match[[1]]$label) && match[[1]]$label != "") match[[1]]$label else v
      }))
    } else {
      valid_named <- valid_cols
      num_named <- num_cols
    }
    
    p_type <- input$desc_plot_type %||% "histogram"

    # Persistent selections logic
    sel_x <- if(isTruthy(rv$desc_vars_state$x)) rv$desc_vars_state$x else valid_named[1]

    tagList(
      if (!(p_type %in% c("parallel", "radar"))) {
        div(style = "display: flex; align-items: center; gap: 5px;",
            selectInput("desc_var_x", "Primary Variable (X)", choices = valid_named, selected = sel_x, width = "calc(100% - 40px)"),
            actionButton("clear_desc_vars", "", icon = icon("times"), class = "btn-danger btn-sm", style = "margin-top: 10px;", title = "Clear selections")
        )
      },
      if (p_type %in% c("boxplot", "violin", "sinaplot", "scatter", "density_heatmap", "xyz_surface")) {        choices_y <- if(p_type %in% c("boxplot", "violin", "sinaplot")) c("None" = "", valid_named) else valid_named
        sel_y <- if(isTruthy(rv$desc_vars_state$y)) rv$desc_vars_state$y else { if(p_type %in% c("boxplot", "violin", "sinaplot")) "" else valid_cols[2] }
        selectInput("desc_var_y", "Secondary Variable (Y)", choices = choices_y, selected = sel_y)
      },
      if (p_type %in% c("boxplot", "violin", "sinaplot")) {
        div(style="background-color: #f0f8ff; padding: 10px; border-radius: 5px; border: 1px solid #b8daff; margin-bottom: 10px;",
            h5("Statistical Significance Tests", style="margin-top:0; color: #0056b3;"),
            checkboxGroupInput("desc_stat_tests", "Select Test (Choose One):", 
                               choices = c("ANOVA" = "anova", "Duncan's" = "duncan", "Tukey's HSD" = "tukey"), inline = TRUE),
            radioButtons("desc_stat_letter_pos", "Letter Placement:", choices = c("Above Data" = "above", "Top of Plot" = "top"), inline = TRUE)
        )
      },
      if (p_type %in% c("scatter")) {
        selectInput("desc_scatter_fit", "Add Trend Line", choices = c("None" = "none", "Linear (lm)" = "linear", "Loess" = "loess", "Polynomial (degree 2)" = "polynomial", "GAM" = "gam"))
      },
      if (p_type %in% c("xyz_surface")) {
        sel_z <- if(isTruthy(rv$desc_vars_state$z)) rv$desc_vars_state$z else num_cols[3]
        selectInput("desc_var_z", "Tertiary Variable (Z)", choices = num_named, selected = sel_z)
      },
      if (p_type %in% c("parallel", "radar")) {
        label_text <- ifelse(p_type == "radar", "Select Variables (Min 3)", "Select Variables (Min 2)")
        sel_m <- if(length(rv$desc_vars_state$multi) > 0) rv$desc_vars_state$multi else head(num_cols, 3)
        selectInput("desc_vars_multi", label_text, choices = num_named, multiple = TRUE, selected = sel_m)
      },
      if (p_type == "xyz_surface") {
        selectInput("desc_xyz_fit", "Surface Fit Model", 
                    choices = c("Linear" = "linear", "Loess" = "loess", "Polynomial" = "polynomial", "GAM" = "gam", "Thin Plate Splines" = "tps"))
      }
    )
  })
  
  observeEvent(input$desc_var_x, { rv$desc_vars_state$x <- input$desc_var_x })
  observeEvent(input$desc_var_y, { rv$desc_vars_state$y <- input$desc_var_y })
  observeEvent(input$desc_var_z, { rv$desc_vars_state$z <- input$desc_var_z })
  observeEvent(input$desc_vars_multi, { rv$desc_vars_state$multi <- input$desc_vars_multi })

  desc_plot_obj <- reactive({
    req(rv_analytics_data())
    df_global <- rv_analytics_data()
    p_type <- input$desc_plot_type
    if (!(p_type %in% c("parallel", "radar"))) {
      req(input$desc_var_x)
    }
    
    df_local <- df_global
    
    active_groups <- input$analytics_active_group
    if (!is.null(active_groups) && length(active_groups) > 0 && "group_id" %in% colnames(df_local)) {
      df_local <- df_local[df_local$group_id %in% active_groups, ]
    } else if (!is.null(active_groups) && length(active_groups) == 0 && "group_id" %in% colnames(df_local)) {
      df_local <- df_local[0, ]
    }
    
    if (nrow(df_local) == 0) {
      p <- ggplot() + annotate("text", x=0, y=0, label="No data selected") + theme_void()
      return(p)
    }
    
    # Overwrite axes with friendly labels by renaming df columns
    get_var_label <- function(v) {
      if (is.null(v) || v == "") return(NULL)
      vars_metadata <- rv$mapping$vars
      if (!is.null(vars_metadata)) {
        match <- Filter(function(x) x$actual == v, vars_metadata)
        if(length(match) > 0 && !is.null(match[[1]]$label) && match[[1]]$label != "") return(match[[1]]$label)
      }
      return(v)
    }
    
    var_x_label <- get_var_label(input$desc_var_x)
    var_y_label <- get_var_label(input$desc_var_y)
    
    if(!is.null(input$desc_var_x) && input$desc_var_x != "") {
        colnames(df_global)[colnames(df_global) == input$desc_var_x] <- var_x_label
        colnames(df_local)[colnames(df_local) == input$desc_var_x] <- var_x_label
    }
    if(!is.null(input$desc_var_y) && input$desc_var_y != "") {
        colnames(df_global)[colnames(df_global) == input$desc_var_y] <- var_y_label
        colnames(df_local)[colnames(df_local) == input$desc_var_y] <- var_y_label
    }
    
    # Core Plots + Ghosting
    core_types <- c("histogram", "density", "boxplot", "violin", "scatter", "ecdf")
    
    if (p_type %in% core_types) {
      if (isTruthy(input$desc_ghosting) && nrow(df_local) < nrow(df_global)) {
        p <- generate_ghosted_plot(df_global, df_local, 
                                   var_name = var_x_label, 
                                   y_var = var_y_label, 
                                   group_col = "group_id", 
                                   plot_type = p_type)
      } else {
        p <- generate_core_plot(df_local,
                                var_name = var_x_label,
                                y_var = var_y_label,
                                group_col = "group_id",
                                plot_type = p_type,
                                scatter_fit = input$desc_scatter_fit,
                                stat_test = input$desc_stat_tests,
                                stat_letter_pos = input$desc_stat_letter_pos)
      }    } else {
      # Advanced Plots
      var_z_label <- get_var_label(input$desc_var_z)
      if(!is.null(input$desc_var_z) && input$desc_var_z != "") {
          colnames(df_global)[colnames(df_global) == input$desc_var_z] <- var_z_label
          colnames(df_local)[colnames(df_local) == input$desc_var_z] <- var_z_label
      }
      
      multi_labels <- sapply(input$desc_vars_multi, get_var_label)
      if(!is.null(input$desc_vars_multi)) {
          for (i in seq_along(input$desc_vars_multi)) {
              orig <- input$desc_vars_multi[i]
              newl <- multi_labels[i]
              colnames(df_global)[colnames(df_global) == orig] <- newl
              colnames(df_local)[colnames(df_local) == orig] <- newl
          }
      }
      
      vars <- switch(p_type,
                     "qq" = var_x_label,
                     "sinaplot" = if(isTruthy(input$desc_var_y)) c(var_x_label, get_var_label(input$desc_var_y)) else var_x_label,
                     "ridge" = var_x_label,
                     "density_heatmap" = c(var_x_label, var_y_label),
                     "xyz_surface" = c(var_x_label, var_y_label, var_z_label),
                     "parallel" = unname(multi_labels),
                     "radar" = unname(multi_labels),
                     var_x_label)
      
      p <- generate_advanced_plot(df_local, vars = vars, group_col = "group_id", plot_type = p_type, xyz_fit = input$desc_xyz_fit, stat_test = input$desc_stat_tests, stat_letter_pos = input$desc_stat_letter_pos)
    }
    
    # Apply Palette
    pal <- input$desc_palette %||% "default"
    if (pal != "default") {
       if (p_type %in% c("density_heatmap", "xyz_surface")) {
          if (pal == "viridis") {
             p <- p + scale_fill_viridis_c() + scale_color_viridis_c()
          } else {
             p <- p + scale_fill_distiller(palette = pal) + scale_color_distiller(palette = pal)
          }
       } else {
          if (pal == "viridis") {
             p <- p + scale_fill_viridis_d() + scale_color_viridis_d()
          } else {
             p <- p + scale_fill_brewer(palette = pal) + scale_color_brewer(palette = pal)
          }
       }
    }
    
    return(p)
  })

  # Clear variables logic
  observeEvent(input$clear_desc_vars, {
    updateSelectInput(session, "desc_var_x", selected = "")
    updateSelectInput(session, "desc_var_y", selected = "")
    updateSelectInput(session, "desc_var_z", selected = "")
    updateSelectInput(session, "desc_vars_multi", selected = character(0))
  })

  output$desc_main_plot <- renderPlot({
    desc_plot_obj()
  })
  
  output$desc_summary_table <- DT::renderDataTable({
    req(rv_analytics_data())
    p_type <- input$desc_plot_type
    if (!(p_type %in% c("parallel", "radar"))) {
      req(input$desc_var_x)
    }
    df <- rv_analytics_data()

    active_groups <- input$analytics_active_group
    if (!is.null(active_groups) && length(active_groups) > 0 && "group_id" %in% colnames(df)) {
      df <- df[df$group_id %in% active_groups, ]
    } else if (!is.null(active_groups) && length(active_groups) == 0 && "group_id" %in% colnames(df)) {
      df <- df[0, ]
    }

    if (nrow(df) == 0) return(NULL)

    if (p_type %in% c("parallel", "radar")) {
        return(data.frame(Message="Summary statistics table is not available for multi-variable plots."))
    }

    var <- input$desc_var_x
    if(!is.numeric(df[[var]])) return(data.frame(Message="Selected primary variable is not numeric."))

    # Calculate summary statistics per group
    agg_mean <- aggregate(df[[var]] ~ df$group_id, FUN=function(x) mean(x, na.rm=TRUE))
    agg_sd <- aggregate(df[[var]] ~ df$group_id, FUN=function(x) sd(x, na.rm=TRUE))
    agg_n <- aggregate(df[[var]] ~ df$group_id, FUN=length)
    agg_min <- aggregate(df[[var]] ~ df$group_id, FUN=function(x) min(x, na.rm=TRUE))
    agg_max <- aggregate(df[[var]] ~ df$group_id, FUN=function(x) max(x, na.rm=TRUE))
    
    res <- data.frame(
      Group = agg_mean[,1], 
      Count = agg_n[,2], 
      Mean = round(agg_mean[,2], 3), 
      SD = round(agg_sd[,2], 3),
      Min = round(agg_min[,2], 3),
      Max = round(agg_max[,2], 3)
    )
    
    # Add Total row
    tot_mean <- mean(df[[var]], na.rm=TRUE)
    tot_sd <- sd(df[[var]], na.rm=TRUE)
    tot_n <- nrow(df[!is.na(df[[var]]), ])
    tot_min <- min(df[[var]], na.rm=TRUE)
    tot_max <- max(df[[var]], na.rm=TRUE)
    
    res <- rbind(res, data.frame(Group="TOTAL", Count=tot_n, Mean=round(tot_mean,3), SD=round(tot_sd,3), Min=round(tot_min,3), Max=round(tot_max,3)))
    
    if (input$desc_plot_type == "scatter" && !is.null(input$desc_scatter_fit) && input$desc_scatter_fit != "none") {
      # Extract Trendline R-squared if applicable
      y_var <- if(!is.null(input$desc_var_y) && input$desc_var_y != "") input$desc_var_y else NULL
      if (!is.null(y_var)) {
         r2_vals <- sapply(as.character(res$Group), function(g) {
            if (g == "TOTAL") sub_df <- df else sub_df <- df[as.character(df$group_id) == g,]
            if (nrow(sub_df) < 5) return(NA)
            f <- input$desc_scatter_fit
            tryCatch({
               if (f == "linear") summary(lm(sub_df[[y_var]] ~ sub_df[[var]]))$r.squared
               else if (f == "polynomial") { if(length(unique(sub_df[[var]])) > 2) summary(lm(sub_df[[y_var]] ~ poly(sub_df[[var]], 2)))$r.squared else NA }
               else if (f == "loess") { mod <- loess(sub_df[[y_var]] ~ sub_df[[var]], span=0.7); cor(sub_df[[y_var]], fitted(mod))^2 }
               else if (f == "gam") { if(requireNamespace("mgcv", quietly=TRUE)) summary(mgcv::gam(sub_df[[y_var]] ~ s(sub_df[[var]], bs = "cs")))$r.sq else NA }
               else NA
            }, error = function(e) NA)
         })
         
         p_vals <- sapply(as.character(res$Group), function(g) {
            if (g == "TOTAL") sub_df <- df else sub_df <- df[as.character(df$group_id) == g,]
            if (nrow(sub_df) < 5) return(NA)
            f <- input$desc_scatter_fit
            tryCatch({
               if (f == "linear") {
                   mod <- summary(lm(sub_df[[y_var]] ~ sub_df[[var]]))
                   if(!is.null(mod$fstatistic)) pf(mod$fstatistic[1], mod$fstatistic[2], mod$fstatistic[3], lower.tail=FALSE) else NA
               } else if (f == "polynomial") { 
                   if(length(unique(sub_df[[var]])) > 2) {
                       mod <- summary(lm(sub_df[[y_var]] ~ poly(sub_df[[var]], 2)))
                       if(!is.null(mod$fstatistic)) pf(mod$fstatistic[1], mod$fstatistic[2], mod$fstatistic[3], lower.tail=FALSE) else NA
                   } else NA 
               }
               else if (f == "gam") { if(requireNamespace("mgcv", quietly=TRUE)) summary(mgcv::gam(sub_df[[y_var]] ~ s(sub_df[[var]], bs = "cs")))$s.table[1, "p-value"] else NA }
               else NA
            }, error = function(e) NA)
         })
         
         res$Trend_R2 <- round(as.numeric(r2_vals), 3)
         res$Trend_PVal <- format.pval(as.numeric(p_vals), digits = 3, eps = 0.001)
      }
    }
    
    DT::datatable(res, options = list(pageLength = 10, dom = 'tip', scrollX = TRUE))
  })
  
  # --- Phase 4: Correlation Rendering ---
  output$corr_vars_ui <- renderUI({
    req(rv$user_data)
    cols <- colnames(rv$user_data)
    num_cols <- cols[sapply(rv$user_data, is.numeric)]
    
    vars_metadata <- rv$mapping$vars
    num_named <- if (!is.null(vars_metadata)) {
      setNames(num_cols, sapply(num_cols, function(v) {
        match <- Filter(function(x) x$actual == v, vars_metadata)
        if(length(match) > 0 && !is.null(match[[1]]$label) && match[[1]]$label != "") match[[1]]$label else v
      }))
    } else { num_cols }
    
    p_type <- input$corr_plot_type %||% "heatmap"
    
    # Isolate selections to prevent resetting when plot type changes
    curr_multi <- isolate(input$corr_vars_multi)
    if (is.null(curr_multi) || length(curr_multi) == 0) curr_multi <- head(num_cols, 5)
    
    curr_var1 <- isolate(input$corr_var_1) %||% num_cols[1]
    curr_var2 <- isolate(input$corr_var_2) %||% (if(length(num_cols) > 1) num_cols[2] else num_cols[1])
    
    if (p_type == "lagged") {
      tagList(
        selectInput("corr_var_1", "Primary Variable", choices = num_named, selected = curr_var1),
        selectInput("corr_var_2", "Secondary Variable", choices = num_named, selected = curr_var2),
        numericInput("corr_max_lag", "Max Lag", value = 10, min = 1, max = 100)
      )
    } else {
      tagList(
        selectInput("corr_vars_multi", "Select Variables (Min 2)", choices = num_named, multiple = TRUE, selected = curr_multi),
        if (p_type == "partial") {
          curr_control <- isolate(input$corr_vars_control)
          selectInput("corr_vars_control", "Control Variables (Partial Out)", choices = num_named, multiple = TRUE, selected = curr_control)
        },
        if (p_type == "network") {
          curr_thresh <- isolate(input$corr_net_thresh) %||% 0.3
          numericInput("corr_net_thresh", "Correlation Threshold", value = curr_thresh, min = 0, max = 1, step = 0.05)
        }
      )
    }
  })
  
  corr_plot_obj <- reactive({
    req(rv_analytics_data())
    df <- rv_analytics_data()
    
    active_groups <- input$analytics_active_group
    if (!is.null(active_groups) && length(active_groups) > 0 && "group_id" %in% colnames(df)) {
      df <- df[df$group_id %in% active_groups, ]
    } else if (!is.null(active_groups) && length(active_groups) == 0 && "group_id" %in% colnames(df)) {
      df <- df[0, ]
    }
    
    if (nrow(df) == 0) {
      p <- ggplot() + annotate("text", x=0, y=0, label="No data selected") + theme_void()
      return(p)
    }
    
    p_type <- input$corr_plot_type
    method <- input$corr_method %||% "pearson"
    
    get_var_label <- function(v) {
      if (is.null(v) || v == "") return(NULL)
      vars_metadata <- rv$mapping$vars
      if (!is.null(vars_metadata)) {
        match <- Filter(function(x) x$actual == v, vars_metadata)
        if(length(match) > 0 && !is.null(match[[1]]$label) && match[[1]]$label != "") return(match[[1]]$label)
      }
      return(v)
    }
    
    if (p_type == "lagged") {
      req(input$corr_var_1, input$corr_var_2)
      v1_lab <- get_var_label(input$corr_var_1)
      v2_lab <- get_var_label(input$corr_var_2)
      colnames(df)[colnames(df) == input$corr_var_1] <- v1_lab
      colnames(df)[colnames(df) == input$corr_var_2] <- v2_lab
      p <- generate_lagged_correlation(df, v1_lab, v2_lab, max_lag = input$corr_max_lag %||% 10)
    } else {
      req(input$corr_vars_multi)
      vars <- input$corr_vars_multi
      if (length(vars) < 2) return(ggplot() + annotate("text", x=0, y=0, label="Need >=2 variables"))
      
      vars_lab <- unname(sapply(vars, get_var_label))
      for(i in seq_along(vars)) {
         colnames(df)[colnames(df) == vars[i]] <- vars_lab[i]
      }
      
      if (p_type == "heatmap") {
        p <- generate_correlation_heatmap(df, vars_lab, method = method)
      } else if (p_type == "network") {
        p <- generate_correlation_network(df, vars_lab, threshold = input$corr_net_thresh %||% 0.3, method = method)
      } else if (p_type == "partial") {
        c_vars <- input$corr_vars_control
        if(!is.null(c_vars) && length(c_vars) > 0) {
           c_vars_lab <- unname(sapply(c_vars, get_var_label))
           for(i in seq_along(c_vars)) {
              if(c_vars[i] %in% colnames(df)) colnames(df)[colnames(df) == c_vars[i]] <- c_vars_lab[i]
           }
        } else {
           c_vars_lab <- NULL
        }
        p <- generate_partial_correlation(df, vars_lab, control_vars = c_vars_lab, method = method)
      } else if (p_type == "correlogram") {
        p <- generate_correlogram(df, vars_lab, method = method)
      }
    }
    return(p)
  })

  output$corr_main_plot <- renderPlot({
    corr_plot_obj()
  })
  
  output$corr_summary_table <- DT::renderDataTable({
    req(rv_analytics_data())
    df <- rv_analytics_data()
    
    active_groups <- input$analytics_active_group
    if (!is.null(active_groups) && length(active_groups) > 0 && "group_id" %in% colnames(df)) {
      df <- df[df$group_id %in% active_groups, ]
    } else if (!is.null(active_groups) && length(active_groups) == 0 && "group_id" %in% colnames(df)) {
      df <- df[0, ]
    }
    
    if (nrow(df) < 3) return(NULL)
    
    p_type <- input$corr_plot_type
    method <- input$corr_method %||% "pearson"
    
    get_var_label <- function(v) {
      if (is.null(v) || v == "") return(NULL)
      vars_metadata <- rv$mapping$vars
      if (!is.null(vars_metadata)) {
        match <- Filter(function(x) x$actual == v, vars_metadata)
        if(length(match) > 0 && !is.null(match[[1]]$label) && match[[1]]$label != "") return(match[[1]]$label)
      }
      return(v)
    }
    
    if (p_type == "lagged") {
      req(input$corr_var_1, input$corr_var_2)
      v1 <- input$corr_var_1
      v2 <- input$corr_var_2
      df_clean <- na.omit(df[, c(v1, v2)])
      if(nrow(df_clean) < 3) return(NULL)
      max_lag <- input$corr_max_lag %||% 10
      ccf_res <- ccf(df_clean[[v1]], df_clean[[v2]], lag.max = max_lag, plot = FALSE)
      res_df <- data.frame(Lag = ccf_res$lag[,1,1], CrossCorrelation = round(ccf_res$acf[,1,1], 3))
      return(DT::datatable(res_df, options = list(pageLength = 10, dom = 't', scrollX = TRUE)))
    } else {
      req(input$corr_vars_multi)
      vars <- input$corr_vars_multi
      if (length(vars) < 2) return(NULL)
      
      vars_lab <- unname(sapply(vars, get_var_label))
      for(i in seq_along(vars)) {
         colnames(df)[colnames(df) == vars[i]] <- vars_lab[i]
      }
      
      if (p_type == "partial") {
        c_vars <- input$corr_vars_control
        if(!is.null(c_vars) && length(c_vars) > 0) {
           c_vars_lab <- unname(sapply(c_vars, get_var_label))
           for(i in seq_along(c_vars)) {
              if(c_vars[i] %in% colnames(df)) colnames(df)[colnames(df) == c_vars[i]] <- c_vars_lab[i]
           }
           
           all_vars <- unique(c(vars_lab, c_vars_lab))
           df_clean <- na.omit(df[, all_vars, drop=FALSE])
           if(nrow(df_clean) < 5) return(NULL)
           
           res_list <- list()
           formula_rhs <- paste(c_vars_lab, collapse=" + ")
           for (v in vars_lab) {
             mod <- try(lm(as.formula(paste0("`", v, "` ~ ", paste(paste0("`", c_vars_lab, "`"), collapse=" + "))), data=df_clean), silent=TRUE)
             if(!inherits(mod, "try-error")) res_list[[v]] <- residuals(mod)
           }
           if(length(res_list) == length(vars_lab)) {
               df_clean <- as.data.frame(res_list)
           }
        } else {
           df_clean <- na.omit(df[, vars_lab, drop=FALSE])
        }
      } else {
        df_clean <- na.omit(df[, vars_lab, drop=FALSE])
      }
      
      if(nrow(df_clean) < 3) return(NULL)
      
      if (p_type %in% c("heatmap", "network", "correlogram", "partial")) {
         n_v <- ncol(df_clean)
         res_list <- list()
         for(i in 1:(n_v-1)) {
            for(j in (i+1):n_v) {
               ct <- tryCatch(cor.test(df_clean[[i]], df_clean[[j]], method = method), error=function(e) NULL)
               if(!is.null(ct)) {
                  res_list[[length(res_list)+1]] <- data.frame(
                      Variable_1 = colnames(df_clean)[i],
                      Variable_2 = colnames(df_clean)[j],
                      Correlation = round(ct$estimate, 3),
                      P_Value = format.pval(ct$p.value, digits=3, eps=0.001)
                  )
               }
            }
         }
         if(length(res_list) > 0) {
            res_df <- do.call(rbind, res_list)
            return(DT::datatable(res_df, options = list(pageLength = 10, dom = 'tip', scrollX = TRUE)))
         }
      }
      
      cormat <- round(cor(df_clean, method = method), 3)
      cormat_df <- as.data.frame(cormat)
      
      return(DT::datatable(cormat_df, options = list(pageLength = 10, dom = 't', scrollX = TRUE)))
    }
  })
  
  # --- Phase 6: Expandable Plot Logic ---
  observeEvent(input$desc_expand_plot_btn, {
    showModal(modalDialog(
      title = "Expanded View: Descriptive Suite",
      size = "l",
      easyClose = TRUE,
      radioButtons("desc_expand_mode", "View Mode:", choices=c("Static (High-Res)"="static", "Interactive (Hover/Zoom)"="interactive"), inline=TRUE),
      uiOutput("desc_expanded_ui"),
      footer = modalButton("Close")
    ))
  })
  
  output$desc_expanded_ui <- renderUI({
     if (input$desc_expand_mode == "interactive") {
        plotly::plotlyOutput("desc_main_plot_expanded_plotly", height = "700px")
     } else {
        plotOutput("desc_main_plot_expanded", height = "700px")
     }
  })
  
  output$desc_main_plot_expanded <- renderPlot({
     desc_plot_obj()
  })
  
  output$desc_main_plot_expanded_plotly <- plotly::renderPlotly({
     p <- desc_plot_obj()
     if (input$desc_plot_type == "radar" && inherits(p, "ggplot") && nrow(p$data) > 0 && "variable" %in% colnames(p$data)) {
        d <- p$data
        fig <- plotly::plot_ly(type = 'scatterpolar', mode = 'lines+markers')
        for(g in unique(d$group)) {
            dg <- d[d$group == g, ]
            dg <- rbind(dg, dg[1, ])
            fig <- plotly::add_trace(fig, r = dg$value, theta = dg$variable, name = g, fill = 'toself')
        }
        fig <- plotly::layout(fig, 
                              polar = list(radialaxis = list(visible = TRUE, range = c(0, max(d$value, na.rm=TRUE)))), 
                              showlegend = TRUE, 
                              title = list(text = "Radar Chart (Normalized Means)<br><sup>Note: Native plotly style used for interactive mode</sup>", x = 0.5))
        return(fig)
     }
     if(inherits(p, "ggplot")) plotly::ggplotly(p) else p
  })

  observeEvent(input$corr_expand_plot_btn, {
    showModal(modalDialog(
      title = "Expanded View: Correlation Analysis",
      size = "l",
      easyClose = TRUE,
      radioButtons("corr_expand_mode", "View Mode:", choices=c("Static (High-Res)"="static", "Interactive (Hover/Zoom)"="interactive"), inline=TRUE),
      uiOutput("corr_expanded_ui"),
      footer = modalButton("Close")
    ))
  })
  
  output$corr_expanded_ui <- renderUI({
     if (input$corr_expand_mode == "interactive") {
        plotly::plotlyOutput("corr_main_plot_expanded_plotly", height = "700px")
     } else {
        plotOutput("corr_main_plot_expanded", height = "700px")
     }
  })
  
  output$corr_main_plot_expanded <- renderPlot({
     corr_plot_obj()
  })
  
  output$corr_main_plot_expanded_plotly <- plotly::renderPlotly({
     p <- corr_plot_obj()
     if(inherits(p, "ggplot")) plotly::ggplotly(p) else p
  })
  
  # --- Phase 5: PCA Logic ---
  output$pca_vars_ui <- renderUI({
    req(rv$user_data)
    cols <- colnames(rv$user_data)
    num_cols <- cols[sapply(rv$user_data, is.numeric)]
    
    vars_metadata <- rv$mapping$vars
    num_named <- if (!is.null(vars_metadata)) {
      setNames(num_cols, sapply(num_cols, function(v) {
        match <- Filter(function(x) x$actual == v, vars_metadata)
        if(length(match) > 0 && !is.null(match[[1]]$label) && match[[1]]$label != "") match[[1]]$label else v
      }))
    } else { num_cols }
    
    tagList(
      selectInput("pca_vars", "Variables for PCA (Min 3)", choices = num_named, multiple = TRUE, selected = head(num_cols, 5)),
      checkboxInput("pca_scale", "Scale & Center Data (Recommended)", value = TRUE)
    )
  })
  
  # Reactive values to hold PCA results
  pca_rv <- reactiveValues(res = NULL, data = NULL, cols = NULL, collinearity_warn = FALSE, collinear_pairs = NULL)
  
  observeEvent(input$run_pca_btn, {
    req(rv_analytics_data(), input$pca_vars)
    df <- rv_analytics_data()
    
    active_groups <- input$analytics_active_group
    if (!is.null(active_groups) && length(active_groups) > 0 && "group_id" %in% colnames(df)) {
      df <- df[df$group_id %in% active_groups, ]
    } else if (!is.null(active_groups) && length(active_groups) == 0 && "group_id" %in% colnames(df)) {
      df <- df[0, ]
    }
    
    if(nrow(df) < 5 || length(input$pca_vars) < 3) {
      showNotification("Insufficient data or variables for PCA.", type="error")
      return()
    }
    
    # Check Collinearity using original names
    col_check <- check_collinearity(df, input$pca_vars, threshold = 0.95)
    
    if (col_check$has_collinearity) {
      pca_rv$collinearity_warn <- TRUE
      pca_rv$collinear_pairs <- col_check$pairs
      pca_rv$res <- NULL
    } else {
      pca_rv$collinearity_warn <- FALSE
      pca_rv$collinear_pairs <- NULL
      
      # Translate to labels
      vars_metadata <- rv$mapping$vars
      get_var_label <- function(v) {
        if (!is.null(vars_metadata)) {
          match <- Filter(function(x) x$actual == v, vars_metadata)
          if(length(match) > 0 && !is.null(match[[1]]$label) && match[[1]]$label != "") return(match[[1]]$label)
        }
        return(v)
      }
      vars_lab <- sapply(input$pca_vars, get_var_label)
      
      df_clean <- na.omit(df[, input$pca_vars, drop=FALSE])
      colnames(df_clean) <- vars_lab
      
      tryCatch({
        pca_rv$res <- prcomp(df_clean, scale. = input$pca_scale, center = input$pca_scale)
        pca_rv$data <- df_clean
        pca_rv$cols <- vars_lab
      }, error = function(e) {
        showNotification(paste("PCA Failed:", e$message), type="error")
      })
    }
  })
  
  output$pca_collinearity_warning_ui <- renderUI({
    if (!pca_rv$collinearity_warn) return(NULL)
    
    div(class = "alert alert-warning",
        h4(icon("exclamation-triangle"), "High Collinearity Detected!"),
        p("The following variable pairs have a correlation > 0.95. This can severely distort PCA results (multicollinearity)."),
        tags$ul(
          lapply(1:nrow(pca_rv$collinear_pairs), function(i) {
            tags$li(paste0(pca_rv$collinear_pairs$var1[i], " & ", pca_rv$collinear_pairs$var2[i], " (r = ", round(pca_rv$collinear_pairs$r[i], 3), ")"))
          })
        ),
        p("You should either remove one of the correlated variables from your selection, or force execution if you know what you're doing."),
        actionButton("pca_force_btn", "Ignore Warning & Force PCA", class="btn-danger")
    )
  })
  
  observeEvent(input$pca_force_btn, {
    req(rv_analytics_data(), input$pca_vars)
    df <- rv_analytics_data()
    active_groups <- input$analytics_active_group
    if (!is.null(active_groups) && length(active_groups) > 0 && "group_id" %in% colnames(df)) {
      df <- df[df$group_id %in% active_groups, ]
    }
    
    vars_metadata <- rv$mapping$vars
    get_var_label <- function(v) {
      if (!is.null(vars_metadata)) {
        match <- Filter(function(x) x$actual == v, vars_metadata)
        if(length(match) > 0 && !is.null(match[[1]]$label) && match[[1]]$label != "") return(match[[1]]$label)
      }
      return(v)
    }
    vars_lab <- sapply(input$pca_vars, get_var_label)
    
    df_clean <- na.omit(df[, input$pca_vars, drop=FALSE])
    colnames(df_clean) <- vars_lab
    
    tryCatch({
      pca_rv$res <- prcomp(df_clean, scale. = input$pca_scale, center = input$pca_scale)
      pca_rv$data <- df_clean
      pca_rv$cols <- vars_lab
      pca_rv$collinearity_warn <- FALSE
    }, error = function(e) {
      showNotification(paste("PCA Failed:", e$message), type="error")
    })
  })
  
  output$pca_ready <- reactive({ if(!is.null(pca_rv$res)) "yes" else "no" })
  outputOptions(output, "pca_ready", suspendWhenHidden = FALSE)

  output$pca_plot_controls <- renderUI({
     req(pca_rv$res)
     n_pcs <- ncol(pca_rv$res$x)
     p_type <- input$pca_plot_type %||% "scree"

     if (p_type == "biplot") {
        tagList(
           numericInput("pca_pc_x", "X-Axis (PC)", value = 1, min = 1, max = n_pcs),
           numericInput("pca_pc_y", "Y-Axis (PC)", value = 2, min = 1, max = n_pcs)
        )
     } else if (p_type == "3d_biplot") {
        tagList(
           numericInput("pca_pc_x", "X-Axis (PC)", value = 1, min = 1, max = n_pcs),
           numericInput("pca_pc_y", "Y-Axis (PC)", value = 2, min = 1, max = n_pcs),
           numericInput("pca_pc_z", "Z-Axis (PC)", value = 3, min = 1, max = n_pcs)
        )
     } else if (p_type %in% c("loadings", "contrib")) {
        numericInput("pca_pc_single", "Select PC", value = 1, min = 1, max = n_pcs)
     } else if (p_type == "cos2") {
        selectInput("pca_cos2_axes", "Select PCs to evaluate", choices = 1:n_pcs, multiple = TRUE, selected = 1:min(2, n_pcs))
     } else {
        NULL
     }
  })

  pca_plot_obj <- reactive({
     req(pca_rv$res)
     p_type <- input$pca_plot_type %||% "scree"

     if (p_type == "scree") {
        p <- generate_pca_scree(pca_rv$res)
     } else if (p_type == "biplot") {
        req(input$pca_pc_x, input$pca_pc_y)
        p <- generate_pca_biplot(pca_rv$res, rv_analytics_data(), pc_x = input$pca_pc_x, pc_y = input$pca_pc_y, group_col = "group_id")
     } else if (p_type == "loadings") {
        req(input$pca_pc_single)
        p <- generate_pca_loadings(pca_rv$res, pc = input$pca_pc_single)
     } else if (p_type == "contrib") {
        req(input$pca_pc_single)
        p <- generate_pca_contribution(pca_rv$res, pc = input$pca_pc_single)
     } else if (p_type == "cos2") {
        req(input$pca_cos2_axes)
        p <- generate_pca_cos2(pca_rv$res, axes = as.numeric(input$pca_cos2_axes))
     } else if (p_type == "cumvar") {
        p <- generate_pca_cumvar(pca_rv$res)
     } else if (p_type == "mahalanobis") {
        p <- generate_pca_mahalanobis(pca_rv$res)
     } else if (p_type == "3d_biplot") {
        # This one is plotly, so handle it separately
        p <- generate_pca_biplot_3d(pca_rv$res, rv_analytics_data(), pc_x = input$pca_pc_x, pc_y = input$pca_pc_y, pc_z = input$pca_pc_z, group_col="group_id")
     }

     return(p)
  })

  output$pca_main_plot <- renderPlot({
     p <- pca_plot_obj()
     if(!inherits(p, "plotly")) return(p)
     # If it's plotly, renderPlot will fail, but we'll handle this in the UI
     # Actually, PCA 3D biplot is plotly. We shouldn't output it in renderPlot.
     # I'll suppress it if it's plotly for now.
     plot(1,1,type="n", axes=F, xlab="", ylab="", main="3D Plotly available in interactive mode")
  })

  observeEvent(input$pca_expand_plot_btn, {
    showModal(modalDialog(
      title = "Expanded View: PCA",
      size = "l",
      easyClose = TRUE,
      if (input$pca_plot_type != "3d_biplot") {
         radioButtons("pca_expand_mode", "View Mode:", choices=c("Static (High-Res)"="static", "Interactive (Hover/Zoom)"="interactive"), inline=TRUE)
      },
      uiOutput("pca_expanded_ui"),
      footer = modalButton("Close")
    ))
  })

  output$pca_expanded_ui <- renderUI({
     if (input$pca_plot_type == "3d_biplot") {
        plotly::plotlyOutput("pca_main_plot_expanded_plotly_3d", height = "700px")
     } else {
        if (!is.null(input$pca_expand_mode) && input$pca_expand_mode == "interactive") {
           plotly::plotlyOutput("pca_main_plot_expanded_plotly", height = "700px")
        } else {
           plotOutput("pca_main_plot_expanded", height = "700px")
        }
     }
  })

  output$pca_main_plot_expanded <- renderPlot({
     p <- pca_plot_obj()
     if(inherits(p, "ggplot")) return(p)
  })

  output$pca_main_plot_expanded_plotly <- plotly::renderPlotly({
     p <- pca_plot_obj()
     if(inherits(p, "ggplot")) plotly::ggplotly(p) else p
  })

  output$pca_main_plot_expanded_plotly_3d <- plotly::renderPlotly({
     p <- pca_plot_obj()
     if(inherits(p, "plotly")) return(p)
  })
  
  output$pca_summary_table <- DT::renderDataTable({
     req(pca_rv$res)
     var_explained <- pca_rv$res$sdev^2 / sum(pca_rv$res$sdev^2)
     cum_var <- cumsum(var_explained)

     df_res <- data.frame(
        PC = paste0("PC", 1:length(var_explained)),
        Eigenvalue = round(pca_rv$res$sdev^2, 3),
        Variance_Explained_Pct = round(var_explained * 100, 2),
        Cumulative_Variance_Pct = round(cum_var * 100, 2)
     )

     DT::datatable(df_res, options = list(pageLength = 10, dom = 't', scrollX = TRUE), rownames = FALSE)
     })

     # --- Phase 7: Governing Factors Logic ---
     output$gov_target_ui <- renderUI({
     req(rv$user_data)
     cols <- colnames(rv$user_data)
     num_cols <- cols[sapply(rv$user_data, is.numeric)]

     vars_metadata <- rv$mapping$vars
     num_named <- if (!is.null(vars_metadata)) {
      setNames(num_cols, sapply(num_cols, function(v) {
        match <- Filter(function(x) x$actual == v, vars_metadata)
        if(length(match) > 0 && !is.null(match[[1]]$label) && match[[1]]$label != "") match[[1]]$label else v
      }))
     } else { num_cols }

     selectInput("gov_target", "Target Parameter", choices = num_named)
     })

     output$gov_predictors_ui <- renderUI({
     req(rv$user_data)
     cols <- colnames(rv$user_data)
     num_cols <- cols[sapply(rv$user_data, is.numeric)]

     vars_metadata <- rv$mapping$vars
     num_named <- if (!is.null(vars_metadata)) {
      setNames(num_cols, sapply(num_cols, function(v) {
        match <- Filter(function(x) x$actual == v, vars_metadata)
        if(length(match) > 0 && !is.null(match[[1]]$label) && match[[1]]$label != "") match[[1]]$label else v
      }))
     } else { num_cols }

     shinyWidgets::pickerInput("gov_predictors", "Governing Factors", choices = num_named, multiple = TRUE, options = list(`actions-box` = TRUE))
     })

     gov_rv <- reactiveValues(res = NULL, ready = FALSE)

     observeEvent(input$gov_run_btn, {
     req(rv_analytics_data(), input$gov_target, input$gov_predictors)
     df <- rv_analytics_data()

     # Respect the existing grouping/discretization engine
     active_groups <- input$analytics_active_group
     if (!is.null(active_groups) && length(active_groups) > 0 && "group_id" %in% colnames(df)) {
       df <- df[df$group_id %in% active_groups, ]
     } else if (!is.null(active_groups) && length(active_groups) == 0 && "group_id" %in% colnames(df)) {
       df <- df[0, ]
     }

     # Exclude target from predictors if mistakenly selected
     preds <- setdiff(input$gov_predictors, input$gov_target)

     if (length(preds) < 1 || nrow(df) < 10) {
       showNotification("Insufficient data or predictors for analysis.", type = "error")
       return()
     }

     withProgress(message = 'Calculating Governing Factors...', value = 0, {
       incProgress(0.2, detail = "Fitting Random Forest...")
       res <- compute_governing_factors(df, target_col = input$gov_target, predictors = preds, n_permutations = input$gov_permutations)
       incProgress(0.8, detail = "Extracting ML Explanations...")

       if (!is.null(res)) {
         gov_rv$res <- res
         gov_rv$ready <- TRUE
       } else {
         gov_rv$ready <- FALSE
         showNotification("Failed to calculate governing factors. Check data quality.", type = "error")
       }
     })
     })

     output$gov_ready <- reactive({
     if (isTRUE(gov_rv$ready)) "yes" else "no"
     })
     outputOptions(output, "gov_ready", suspendWhenHidden = FALSE)

     output$gov_plot_importance <- renderPlot({
     req(gov_rv$res)
     vip_df <- gov_rv$res$importance
     vip_df <- vip_df[order(vip_df$dropout_loss, decreasing = FALSE), ]
     
     # Apply labels
     vip_df$variable_label <- sapply(as.character(vip_df$variable), function(v) {
        match <- Filter(function(x) x$actual == v, rv$mapping$vars)
        if(length(match) > 0 && !is.null(match[[1]]$label) && match[[1]]$label != "") match[[1]]$label else v
     })
     vip_df$variable_label <- factor(vip_df$variable_label, levels = vip_df$variable_label)

     ggplot(vip_df, aes(x = variable_label, y = dropout_loss)) +
       geom_bar(stat = "identity", fill = "steelblue") +
       coord_flip() +
       labs(title = "Global Variable Importance", x = "Variable", y = "Dropout Loss (RMSE increase)") +
       theme_minimal()
     })

     output$gov_plot_interaction_a <- renderPlot({
     req(gov_rv$res)
     shap_df <- gov_rv$res$shap
     
     # Apply labels
     top_var_label <- (function(v) {
        match <- Filter(function(x) x$actual == v, rv$mapping$vars)
        if(length(match) > 0 && !is.null(match[[1]]$label) && match[[1]]$label != "") match[[1]]$label else v
     })(gov_rv$res$top_var)

     ggplot(shap_df, aes(x = feature_value, y = contribution)) +
       geom_point(color = "darkred", alpha = 0.6) +
       geom_smooth(method = "loess", color = "blue", se = FALSE) +
       labs(title = paste("SHAP Dependence:", top_var_label), x = paste(top_var_label, "Value"), y = "SHAP Contribution") +
       theme_minimal()
     })

     output$gov_plot_effect <- renderPlot({
     req(gov_rv$res)
     top_var_label <- (function(v) {
        match <- Filter(function(x) x$actual == v, rv$mapping$vars)
        if(length(match) > 0 && !is.null(match[[1]]$label) && match[[1]]$label != "") match[[1]]$label else v
     })(gov_rv$res$top_var)

     if (!is.null(input$gov_effect_type) && input$gov_effect_type == "ale") {
       ale_df <- gov_rv$res$ale
       ggplot(ale_df, aes(x = `_x_`, y = `_yhat_`)) +
         geom_line(color = "purple", linewidth = 1) +
         labs(title = paste("ALE Profile:", top_var_label), x = top_var_label, y = "ALE Effect") +
         theme_minimal()
     } else {
       shap_df <- gov_rv$res$shap
       ggplot(shap_df, aes(x = feature_value, y = contribution)) +
         geom_point(color = "forestgreen", alpha = 0.6) +
         labs(title = paste("SHAP Profile:", top_var_label), x = top_var_label, y = "SHAP Value") +
         theme_minimal()
     }
     })

     output$gov_plot_interaction_b <- renderPlot({
     req(gov_rv$res, input$gov_target)
     df <- rv_analytics_data()
     
     top_var_label <- (function(v) {
        match <- Filter(function(x) x$actual == v, rv$mapping$vars)
        if(length(match) > 0 && !is.null(match[[1]]$label) && match[[1]]$label != "") match[[1]]$label else v
     })(gov_rv$res$top_var)
     target_label <- (function(v) {
        match <- Filter(function(x) x$actual == v, rv$mapping$vars)
        if(length(match) > 0 && !is.null(match[[1]]$label) && match[[1]]$label != "") match[[1]]$label else v
     })(input$gov_target)

     # Ensure data has the selected variables
     if (gov_rv$res$top_var %in% colnames(df) && input$gov_target %in% colnames(df)) {
        ggplot(df, aes_string(x = paste0("`", gov_rv$res$top_var, "`"), y = paste0("`", input$gov_target, "`"))) +
          geom_point(alpha = 0.5) +
          geom_smooth(method = "lm", color = "red") +
          labs(title = paste("Target vs Top Factor:", top_var_label), x = top_var_label, y = target_label) +
          theme_minimal()
     } else {
        ggplot() + annotate("text", x=0, y=0, label="Data not available") + theme_void()
     }
     })

     output$gov_summary_table <- DT::renderDataTable({
     req(gov_rv$res)
     vip_df <- gov_rv$res$importance
     vip_df <- vip_df[order(vip_df$dropout_loss, decreasing = TRUE), ]
     
     vip_df$variable <- sapply(as.character(vip_df$variable), function(v) {
        match <- Filter(function(x) x$actual == v, rv$mapping$vars)
        if(length(match) > 0 && !is.null(match[[1]]$label) && match[[1]]$label != "") match[[1]]$label else v
     })

     colnames(vip_df) <- c("Governing Factor", "Importance (Dropout Loss)")
     DT::datatable(vip_df, options = list(pageLength = 5, dom = 't', scrollX = TRUE), rownames = FALSE)
     })

     # --- Expanded View Handlers for Governing Factors ---
     # Importance
     observeEvent(input$gov_expand_imp_btn, {
       showModal(modalDialog(
         title = "Expanded View: Global Importance", size = "l", easyClose = TRUE,
         plotOutput("gov_plot_imp_exp", height = "700px"), footer = modalButton("Close")
       ))
     })
     output$gov_plot_imp_exp <- renderPlot({
       req(gov_rv$res)
       vip_df <- gov_rv$res$importance
       vip_df <- vip_df[order(vip_df$dropout_loss, decreasing = FALSE), ]
       vip_df$variable_label <- sapply(as.character(vip_df$variable), function(v) {
          match <- Filter(function(x) x$actual == v, rv$mapping$vars)
          if(length(match) > 0 && !is.null(match[[1]]$label) && match[[1]]$label != "") match[[1]]$label else v
       })
       vip_df$variable_label <- factor(vip_df$variable_label, levels = vip_df$variable_label)
       ggplot(vip_df, aes(x = variable_label, y = dropout_loss)) + geom_bar(stat = "identity", fill = "steelblue") +
         coord_flip() + labs(title = "Global Variable Importance", x = "Variable", y = "Dropout Loss (RMSE increase)") + theme_minimal(base_size=16)
     })

     # Interaction A
     observeEvent(input$gov_expand_inta_btn, {
       showModal(modalDialog(
         title = "Expanded View: Interaction (A)", size = "l", easyClose = TRUE,
         plotOutput("gov_plot_inta_exp", height = "700px"), footer = modalButton("Close")
       ))
     })
     output$gov_plot_inta_exp <- renderPlot({
       req(gov_rv$res)
       shap_df <- gov_rv$res$shap
       top_var_label <- (function(v) {
          match <- Filter(function(x) x$actual == v, rv$mapping$vars)
          if(length(match) > 0 && !is.null(match[[1]]$label) && match[[1]]$label != "") match[[1]]$label else v
       })(gov_rv$res$top_var)
       ggplot(shap_df, aes(x = feature_value, y = contribution)) + geom_point(color = "darkred", alpha = 0.6, size=3) +
         geom_smooth(method = "loess", color = "blue", se = FALSE, linewidth=1.5) +
         labs(title = paste("SHAP Dependence:", top_var_label), x = paste(top_var_label, "Value"), y = "SHAP Contribution") + theme_minimal(base_size=16)
     })

     # Effect
     observeEvent(input$gov_expand_eff_btn, {
       showModal(modalDialog(
         title = "Expanded View: Functional Effect", size = "l", easyClose = TRUE,
         plotOutput("gov_plot_eff_exp", height = "700px"), footer = modalButton("Close")
       ))
     })
     output$gov_plot_eff_exp <- renderPlot({
       req(gov_rv$res)
       top_var_label <- (function(v) {
          match <- Filter(function(x) x$actual == v, rv$mapping$vars)
          if(length(match) > 0 && !is.null(match[[1]]$label) && match[[1]]$label != "") match[[1]]$label else v
       })(gov_rv$res$top_var)
       if (!is.null(input$gov_effect_type) && input$gov_effect_type == "ale") {
         ale_df <- gov_rv$res$ale
         ggplot(ale_df, aes(x = `_x_`, y = `_yhat_`)) + geom_line(color = "purple", linewidth = 2) +
           labs(title = paste("ALE Profile:", top_var_label), x = top_var_label, y = "ALE Effect") + theme_minimal(base_size=16)
       } else {
         shap_df <- gov_rv$res$shap
         ggplot(shap_df, aes(x = feature_value, y = contribution)) + geom_point(color = "forestgreen", alpha = 0.6, size=3) +
           labs(title = paste("SHAP Profile:", top_var_label), x = top_var_label, y = "SHAP Value") + theme_minimal(base_size=16)
       }
     })

     # Interaction B
     observeEvent(input$gov_expand_intb_btn, {
       showModal(modalDialog(
         title = "Expanded View: Interaction (B)", size = "l", easyClose = TRUE,
         plotOutput("gov_plot_intb_exp", height = "700px"), footer = modalButton("Close")
       ))
     })
     output$gov_plot_intb_exp <- renderPlot({
       req(gov_rv$res, input$gov_target)
       df <- rv_analytics_data()
       top_var_label <- (function(v) {
          match <- Filter(function(x) x$actual == v, rv$mapping$vars)
          if(length(match) > 0 && !is.null(match[[1]]$label) && match[[1]]$label != "") match[[1]]$label else v
       })(gov_rv$res$top_var)
       target_label <- (function(v) {
          match <- Filter(function(x) x$actual == v, rv$mapping$vars)
          if(length(match) > 0 && !is.null(match[[1]]$label) && match[[1]]$label != "") match[[1]]$label else v
       })(input$gov_target)
       if (gov_rv$res$top_var %in% colnames(df) && input$gov_target %in% colnames(df)) {
          ggplot(df, aes_string(x = paste0("`", gov_rv$res$top_var, "`"), y = paste0("`", input$gov_target, "`"))) +
            geom_point(alpha = 0.5, size=3) + geom_smooth(method = "lm", color = "red", linewidth=1.5) +
            labs(title = paste("Target vs Top Factor:", top_var_label), x = top_var_label, y = target_label) + theme_minimal(base_size=16)
       } else {
          ggplot() + annotate("text", x=0, y=0, label="Data not available") + theme_void()
       }
     })

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
    
    # Update all maps without a full re-render
    leafletProxy("setup_minimap") %>% clearTiles() %>% addProviderTiles(new_tiles)
    leafletProxy("main_map") %>% clearTiles() %>% addProviderTiles(new_tiles)
    leafletProxy("comp_map_left") %>% clearTiles() %>% addProviderTiles(new_tiles)
    leafletProxy("comp_map_right") %>% clearTiles() %>% addProviderTiles(new_tiles)
  }, ignoreInit = TRUE)

  rv <- reactiveValues(
    user_data = NULL, # Uploaded data
    has_predictions = FALSE, # Tracks interpolation state
    export_registry = list(), # Registry of plots and tables for export
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
    drawn_feature = NULL # Temporarily store drawn shape for grouping
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
  
  # Track the most recently selected item for the Styler
  active_styler_item <- reactiveVal(NULL)
  
  observeEvent(input$selected_assets, {
    req(input$selected_assets)
    if (length(input$selected_assets) > 0) {
      active_styler_item(input$selected_assets[length(input$selected_assets)])
    }
  }, ignoreNULL = FALSE)
  
  # Reactive for the styled plot preview
  styled_preview_obj <- reactive({
    req(active_styler_item(), rv$export_registry)
    item <- rv$export_registry[[active_styler_item()]]
    req(item)
    
    # Use the unified styling engine
    generate_styled_plot(
      item, input, 
      calibration = 1, 
      agro_params = tryCatch(agro_params(), error = function(e) NULL)
    )
  })
  
  # Debounce the preview to avoid flickering during slider movement
  styled_preview_obj_d <- styled_preview_obj %>% debounce(500)
  
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
    click("open_styler")
  })
  
  # Styler Configuration Persistence (Local Storage)
  observe({
    req(input$styler_title_size) # trigger on any styler input
    cfg <- list(
      title_size = input$styler_title_size,
      base_size = input$styler_base_size,
      x_size = input$styler_x_size,
      y_size = input$styler_y_size,
      label_size = input$styler_label_size,
      legend_size = input$styler_legend_size,
      legend_key_size = input$styler_legend_key_size,
      font_family = input$styler_font_family,
      label_orient = input$styler_label_orient,
      legend_pos = input$styler_legend_pos,
      margin_t = input$styler_margin_t,
      margin_r = input$styler_margin_r,
      margin_b = input$styler_margin_b,
      margin_l = input$styler_margin_l,
      show_grid = input$styler_show_grid,
      high_contrast = input$styler_high_contrast,
      aspect_ratio = input$styler_aspect_ratio,
      dpi = input$styler_dpi,
      format = input$styler_format
    )
    # Debounce slightly by just writing to localStorage
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
                              column(6, sliderInput("styler_title_size", "Main Title", min = 6, max = 40, value = 16)),
                              column(6, sliderInput("styler_base_size", "Base Text", min = 4, max = 30, value = 12))
                            ),
                            fluidRow(
                              column(6, sliderInput("styler_x_size", "X-Axis Text Size", min = 4, max = 30, value = 12)),
                              column(6, sliderInput("styler_y_size", "Y-Axis Text Size", min = 4, max = 30, value = 12))
                            ),
                            fluidRow(
                              column(6, sliderInput("styler_label_size", "Axis Labels", min = 4, max = 30, value = 10)),
                              column(6, sliderInput("styler_legend_size", "Legend Text", min = 4, max = 30, value = 10))
                            ),
                            fluidRow(
                              column(6, sliderInput("styler_legend_key_size", "Legend Element Size", min = 0.5, max = 5.0, value = 1.0, step = 0.1)),
                              column(6, selectInput("styler_font_family", "Font Family", 
                                          choices = c("sans", "serif", "mono", "Roboto", "Open Sans", "Lato", "Montserrat")))
                            ),
                            selectInput("styler_label_orient", "X-Label Orientation", 
                                        choices = c("Horizontal" = 0, "Vertical" = 90, "Angled (45)" = 45)),
                            hr(),
                            h4("Layout & Spacing"),
                            selectInput("styler_legend_pos", "Legend Position", 
                                        choices = c("Right" = "right", "Bottom" = "bottom", "Left" = "left", "Top" = "top", "None" = "none")),
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
                            numericInput("styler_aspect_ratio", "Custom Aspect Ratio (Width/Height)", value = 1.25, step = 0.1)
                          )
                 )
               )
        ),
        column(8,
               div(style = "background-color: #f0f0f0; border: 1px solid #ccc; height: 600px; display: flex; justify-content: center; align-items: center; overflow: auto;",
                   div(style = "width: 100%; max-width: 800px; height: 600px; background-color: white; box-shadow: 0 4px 8px rgba(0,0,0,0.2);",
                       plotOutput("styler_preview_plot", height = "600px", width = "100%")
                   )
               ),
               tags$p(style="font-size: 0.85em; color: #666; margin-top: 5px;", 
                      "Preview is calibrated to 10x8 aspect ratio. Final export uses 2.5x typographical density enhancement.")
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
    if(!is.null(cfg$title_size)) updateSliderInput(session, "styler_title_size", value = cfg$title_size)
    if(!is.null(cfg$base_size)) updateSliderInput(session, "styler_base_size", value = cfg$base_size)
    if(!is.null(cfg$x_size)) updateSliderInput(session, "styler_x_size", value = cfg$x_size)
    if(!is.null(cfg$y_size)) updateSliderInput(session, "styler_y_size", value = cfg$y_size)
    if(!is.null(cfg$label_size)) updateSliderInput(session, "styler_label_size", value = cfg$label_size)
    if(!is.null(cfg$legend_size)) updateSliderInput(session, "styler_legend_size", value = cfg$legend_size)
    if(!is.null(cfg$legend_key_size)) updateSliderInput(session, "styler_legend_key_size", value = cfg$legend_key_size)
    if(!is.null(cfg$font_family)) updateSelectInput(session, "styler_font_family", selected = cfg$font_family)
    if(!is.null(cfg$label_orient)) updateSelectInput(session, "styler_label_orient", selected = cfg$label_orient)
    if(!is.null(cfg$legend_pos)) updateSelectInput(session, "styler_legend_pos", selected = cfg$legend_pos)
    if(!is.null(cfg$margin_t)) updateNumericInput(session, "styler_margin_t", value = cfg$margin_t)
    if(!is.null(cfg$margin_r)) updateNumericInput(session, "styler_margin_r", value = cfg$margin_r)
    if(!is.null(cfg$margin_b)) updateNumericInput(session, "styler_margin_b", value = cfg$margin_b)
    if(!is.null(cfg$margin_l)) updateNumericInput(session, "styler_margin_l", value = cfg$margin_l)
    if(!is.null(cfg$show_grid)) updateCheckboxInput(session, "styler_show_grid", value = cfg$show_grid)
    if(!is.null(cfg$high_contrast)) updateCheckboxInput(session, "styler_high_contrast", value = cfg$high_contrast)
    if(!is.null(cfg$aspect_ratio)) updateNumericInput(session, "styler_aspect_ratio", value = cfg$aspect_ratio)
    if(!is.null(cfg$dpi)) updateNumericInput(session, "styler_dpi", value = cfg$dpi)
    if(!is.null(cfg$format)) updateSelectInput(session, "styler_format", selected = cfg$format)
  })
  
  output$styler_download_config <- downloadHandler(
    filename = function() { paste0("styler_config_", format(Sys.time(), "%Y%m%d"), ".json") },
    content = function(file) {
      cfg <- list(
        styler_title_size = input$styler_title_size,
        styler_base_size = input$styler_base_size,
        styler_x_size = input$styler_x_size,
        styler_y_size = input$styler_y_size,
        styler_label_size = input$styler_label_size,
        styler_legend_size = input$styler_legend_size,
        styler_legend_key_size = input$styler_legend_key_size,
        styler_font_family = input$styler_font_family,
        styler_label_orient = input$styler_label_orient,
        styler_legend_pos = input$styler_legend_pos,
        styler_margin_t = input$styler_margin_t,
        styler_margin_r = input$styler_margin_r,
        styler_margin_b = input$styler_margin_b,
        styler_margin_l = input$styler_margin_l,
        styler_show_grid = input$styler_show_grid,
        styler_high_contrast = input$styler_high_contrast,
        styler_aspect_ratio = input$styler_aspect_ratio,
        styler_dpi = input$styler_dpi,
        styler_format = input$styler_format
      )
      write(jsonlite::toJSON(cfg, auto_unbox = TRUE), file)
    }
  )

  observeEvent(input$styler_upload_config, {
    req(input$styler_upload_config)
    tryCatch({
      cfg <- jsonlite::fromJSON(input$styler_upload_config$datapath)
      
      if(!is.null(cfg$styler_title_size)) updateSliderInput(session, "styler_title_size", value = cfg$styler_title_size)
      if(!is.null(cfg$styler_base_size)) updateSliderInput(session, "styler_base_size", value = cfg$styler_base_size)
      if(!is.null(cfg$styler_x_size)) updateSliderInput(session, "styler_x_size", value = cfg$styler_x_size)
      if(!is.null(cfg$styler_y_size)) updateSliderInput(session, "styler_y_size", value = cfg$styler_y_size)
      if(!is.null(cfg$styler_label_size)) updateSliderInput(session, "styler_label_size", value = cfg$styler_label_size)
      if(!is.null(cfg$styler_legend_size)) updateSliderInput(session, "styler_legend_size", value = cfg$styler_legend_size)
      if(!is.null(cfg$styler_legend_key_size)) updateSliderInput(session, "styler_legend_key_size", value = cfg$styler_legend_key_size)
      if(!is.null(cfg$styler_font_family)) updateSelectInput(session, "styler_font_family", selected = cfg$styler_font_family)
      if(!is.null(cfg$styler_label_orient)) updateSelectInput(session, "styler_label_orient", selected = cfg$styler_label_orient)
      if(!is.null(cfg$styler_legend_pos)) updateSelectInput(session, "styler_legend_pos", selected = cfg$styler_legend_pos)
      if(!is.null(cfg$styler_margin_t)) updateNumericInput(session, "styler_margin_t", value = cfg$styler_margin_t)
      if(!is.null(cfg$styler_margin_r)) updateNumericInput(session, "styler_margin_r", value = cfg$styler_margin_r)
      if(!is.null(cfg$styler_margin_b)) updateNumericInput(session, "styler_margin_b", value = cfg$styler_margin_b)
      if(!is.null(cfg$styler_margin_l)) updateNumericInput(session, "styler_margin_l", value = cfg$styler_margin_l)
      if(!is.null(cfg$styler_show_grid)) updateCheckboxInput(session, "styler_show_grid", value = cfg$styler_show_grid)
      if(!is.null(cfg$styler_high_contrast)) updateCheckboxInput(session, "styler_high_contrast", value = cfg$styler_high_contrast)
      if(!is.null(cfg$styler_aspect_ratio)) updateNumericInput(session, "styler_aspect_ratio", value = cfg$styler_aspect_ratio)
      if(!is.null(cfg$styler_dpi)) updateNumericInput(session, "styler_dpi", value = cfg$styler_dpi)
      if(!is.null(cfg$styler_format)) updateSelectInput(session, "styler_format", selected = cfg$styler_format)
      
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
              agro_params = tryCatch(agro_params(), error = function(e) NULL)
            )

            if (inherits(p_obj, "trellis")) {
              if (ext == "png") png(file, width = 10, height = 8, units = "in", res = input$styler_dpi %||% 300)
              else if (ext == "tiff") tiff(file, width = 10, height = 8, units = "in", res = input$styler_dpi %||% 300)
              else if (ext == "pdf") pdf(file, width = 10, height = 8)
              else jpeg(file, width = 10, height = 8, units = "in", res = input$styler_dpi %||% 300)

              print(p_obj)
              dev.off()
            } else {
              ggsave(file, plot = p_obj, 
                     device = if(ext == "pdf") "pdf" else (if(ext == "tiff") "tiff" else NULL),
                     dpi = input$styler_dpi %||% 300,
                     width = 10, height = 8, units = "in")
            }
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
          for(item in table_items) {
            # Sheet names must be <= 31 chars and unique
            sheet_name <- substr(gsub("[^a-zA-Z0-9 ]", "_", item$label), 1, 31)
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
              agro_params = tryCatch(agro_params(), error = function(e) NULL)
            )
            
            if (inherits(p, "trellis")) {
              if (ext == "png") png(filepath, width = 10, height = 8, units = "in", res = input$styler_dpi %||% 300)
              else if (ext == "tiff") tiff(filepath, width = 10, height = 8, units = "in", res = input$styler_dpi %||% 300)
              else if (ext == "pdf") pdf(filepath, width = 10, height = 8)
              else jpeg(filepath, width = 10, height = 8, units = "in", res = input$styler_dpi %||% 300)
              print(p); dev.off()
            } else {
              ggsave(filepath, plot = p, dpi = input$styler_dpi %||% 300, width = 10, height = 8, units = "in")
            }
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
  generate_popup <- function(data_row) {
    # Ensure it's a list for reliable access
    data_row <- as.list(data_row)
    names_in_row <- names(data_row)
    
    # Helper to find value with robust matching
    find_val <- function(key) {
      if (key %in% names_in_row) return(data_row[[key]])
      # Case-insensitive match
      idx <- grep(paste0("^", key, "$"), names_in_row, ignore.case = TRUE)
      if (length(idx) > 0) return(data_row[[idx[1]]])
      # Suffix match (e.g., if it's "Locality_ID" vs "ID")
      idx <- grep(paste0(key, "$"), names_in_row, ignore.case = TRUE)
      if (length(idx) > 0) return(data_row[[idx[1]]])
      # Suffix match without boundary
      idx <- grep(as.character(key), names_in_row, ignore.case = TRUE)
      if (length(idx) > 0) return(data_row[[idx[1]]])
      return(NULL)
    }

    # If no specific variables selected, use intelligent defaults
    vars_to_show <- rv$pop_up_vars
    if(is.null(vars_to_show) || length(vars_to_show) == 0) {
      # Prioritize "Soil" or "Physicochemistry" categories as requested
      soil_vars <- Filter(function(x) grepl("Soil|Physicochem", x$category, ignore.case = TRUE), rv$mapping$vars)
      if(length(soil_vars) > 0) {
        vars_to_show <- sapply(soil_vars, function(x) x$actual)
      } else {
        vars_to_show <- rv$mapping$vars %>% sapply(function(x) x$actual)
      }
      # If still empty, use all numeric from row
      if (is.null(vars_to_show) || length(vars_to_show) == 0) {
        vars_to_show <- names_in_row[sapply(data_row, is.numeric)]
      }
    }
    
    # Get metadata for categories
    meta_list <- rv$mapping$vars
    
    # Organize by Category
    html_content <- "<div style='max-height: 300px; overflow-y: auto; font-family: sans-serif; min-width: 200px;'>"
    html_content <- paste0(html_content, "<h4>Point Details</h4><table style='width: 100%; border-collapse: collapse;'>")
    
    # Group variables by category
    all_cats <- unique(sapply(meta_list, function(x) x$category))
    # Prioritize Soil/Physicochemistry categories
    priority_cats <- all_cats[grepl("Soil|Physicochem", all_cats, ignore.case = TRUE)]
    other_cats <- setdiff(all_cats, priority_cats)
    cats <- c(priority_cats, other_cats)
    
    for(cat in cats) {
      cat_vars <- Filter(function(x) x$category == cat && x$actual %in% vars_to_show, meta_list)
      if(length(cat_vars) > 0) {
        html_content <- paste0(html_content, "<tr style='background-color: #f2f2f2;'><td colspan='2'><b>", cat, "</b></td></tr>")
        for(v in cat_vars) {
          # Use helper for robust lookup
          val <- find_val(as.character(v$actual))
          val_str <- if(!is.null(val) && (is.numeric(val) || !is.na(suppressWarnings(as.numeric(val))))) round(as.numeric(val), 3) else as.character(val %||% "N/A")
          html_content <- paste0(html_content, "<tr><td style='padding: 3px;'>", v$label, "</td><td style='padding: 3px; text-align: right;'>", val_str, "</td></tr>")
        }
      }
    }
    
    # Show any selected variables not in metadata
    other_vars <- setdiff(vars_to_show, sapply(meta_list, function(x) x$actual))
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
    
    showModal(modalDialog(
      title = "Sampling Point Pop-up Settings",
      pickerInput("popup_var_select", "Select Variables to Display in Pop-ups:", 
                  choices = choices, 
                  selected = rv$pop_up_vars %||% sapply(vars_list, function(x) x$actual), 
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

  # --- Data Setup Logic ---
  output$file_uploaded <- reactive({ !is.null(input$user_file) })
  outputOptions(output, "file_uploaded", suspendWhenHidden = FALSE)
  
  output$export_updated_data <- downloadHandler(
    filename = function() {
      paste0("updated_spatial_dataset_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".xlsx")
    },
    content = function(file) {
      req(rv$user_data)
      writexl::write_xlsx(rv$user_data, path = file)
    }
  )
  
  observeEvent(input$user_file, {
    req(input$user_file)
    ext <- tools::file_ext(input$user_file$datapath)
    df <- tryCatch({
      if (ext == "csv") read.csv(input$user_file$datapath)
      else if (ext %in% c("xls", "xlsx")) readxl::read_excel(input$user_file$datapath)
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
        p_cve <- grep(paste0("^", col, "_cve$"), num_cols, ignore.case=TRUE, value=TRUE)[1]
        p_ss  <- grep(paste0("^", col, "_ss$"),  num_cols, ignore.case=TRUE, value=TRUE)[1]
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
    updateSelectInput(session, "locality", choices = c("ALL", unique(df[[input$map_loc %||% cols[1]]])))
  })

  # --- Shapefile Integration ---
  observeEvent(input$user_shp, {
    req(input$user_shp)
    temp_dir <- file.path(tempdir(), "shp_upload"); if(!dir.exists(temp_dir)) dir.create(temp_dir)
    for(i in 1:nrow(input$user_shp)) {
      file.copy(input$user_shp$datapath[i], file.path(temp_dir, input$user_shp$name[i]), overwrite = TRUE)
    }
    shp_file <- input$user_shp$name[grep("\\.shp$", input$user_shp$name, ignore.case = TRUE)]
    if(length(shp_file) == 0) { showNotification("No .shp file found.", type = "error"); return() }
    s <- tryCatch({ st_read(file.path(temp_dir, shp_file[1]), quiet = TRUE) }, error = function(e) { 
      showNotification(paste("Error reading shapefile:", e$message), type = "error"); NULL 
    })
    req(s); rv$shp_bound <- s
    showNotification("Custom shapefile loaded successfully!", type = "message")
    crs_val <- st_crs(s)$input
    if(!is.na(crs_val)) updateSelectizeInput(session, "crs_selection", selected = crs_val)
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
    crs_obj <- tryCatch(st_crs(input$crs_selection), error = function(e) NULL)
    req(crs_obj)
    
    units <- crs_obj$units_gdal
    if (is.null(units)) units <- "meters" # fallback
    
    if (grepl("degree", units, ignore.case = TRUE)) {
      updateSliderInput(session, "grid_res", label = "Resolution (Degrees)", 
                        min = 0.0001, max = 0.01, value = 0.0005, step = 0.0001)
    } else {
      updateSliderInput(session, "grid_res", label = "Resolution (m)", 
                        min = 1, max = 500, value = 50, step = 1)
    }
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
    crs_obj <- tryCatch(st_crs(input$crs_selection), error = function(e) NULL)
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
            
            sub_pts <- tryCatch(st_as_sf(sub_df, coords=c("x","y"), crs=input$map_crs) %>% st_transform(input$crs_selection), error=function(e) NULL)
            if(is.null(sub_pts)) next
            
            sub_coords <- st_coordinates(sub_pts)
            if (nrow(sub_coords) > 1) {
                 sub_knn <- FNN::get.knn(sub_coords, k = 1)
                 l_res <- mean(sub_knn$nn.dist) * 0.5
            } else l_res <- final_rec
            
            sub_bbox <- st_bbox(sub_pts)
            sub_max_dim <- max(sub_bbox["xmax"] - sub_bbox["xmin"], sub_bbox["ymax"] - sub_bbox["ymin"])
            sub_min_res <- sub_max_dim / 300
            l_res <- max(l_res, sub_min_res)
            
            if (is_degree) l_res <- max(0.00001, min(0.01, l_res))
            else l_res <- max(0.1, min(500, l_res))
            
            temp_res[[l]] <- l_res
        }
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
    ext <- tools::file_ext(input$meta_file$datapath)
    m_df <- tryCatch({
      if (ext == "csv") read.csv(input$meta_file$datapath)
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
          def_p_cve <- get_map_val(t, "pred")    %||% grep(paste0("^", t, "_cve$"), num_cols, ignore.case=TRUE, value=TRUE)[1] %||% "None"
          def_p_ss  <- get_map_val(t, "pred_ss") %||% grep(paste0("^", t, "_ss$"),  num_cols, ignore.case=TRUE, value=TRUE)[1] %||% "None"
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
      updateSelectInput(session, "locality", choices = c("ALL", loc_choices), selected = loc_choices[1])
    }
  })

  output$setup_minimap <- renderLeaflet({
    req(rv$user_data, rv$mapping$x, rv$mapping$y, rv$mapping$crs)
    
    # Try to create SF object for mapping
    df_map <- rv$user_data %>% select(x = !!sym(rv$mapping$x), y = !!sym(rv$mapping$y)) %>% na.omit()
    if (nrow(df_map) == 0) return(NULL)
    
    pts <- tryCatch({
      st_as_sf(df_map, coords = c("x", "y"), crs = rv$mapping$crs) %>% st_transform(4326)
    }, error = function(e) NULL)
    
    req(pts)
    current_tiles <- input$base_map_layer %||% if(isTruthy(input$styler_local_config)) jsonlite::fromJSON(input$styler_local_config)$map_tiles else "Esri.WorldImagery"
    if (is.null(current_tiles) || current_tiles == "") current_tiles <- "Esri.WorldImagery"
    
    leaflet(pts) %>% addProviderTiles(current_tiles, layerId="base_tiles") %>%
      addCircleMarkers(radius = 3, color = "cyan", opacity = 1)
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
          p("Version: 0.8.8"),
          p("Integrated geostatistical modeling, classification and statistical interpretation."),
          hr(),
          p("Designed for high-performance parallel processing and spatial diagnostics, multi-scale interpolation via kriging, inverse distance weighting, and thin plate splines with practical multi-criteria optimization."),
          p("Supported with the Descriptive and Explarotive Suite with dynamic visualizations and statistics."),
          hr(),
          p(strong("A product of `that` couple of months following the loose of institutional e-mail address.")),
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
        target <- if(input$value_type == "actual") terra::unwrap(rv$rast) else terra::unwrap(rv$rast_pred)
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
    
    colors <- colorRampPalette(agro_colors)(n_c)
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

  output$palette_ui <- renderUI({
    req(input$var_id, rv$mapping$vars)
    idx <- which(sapply(rv$mapping$vars, function(x) x$actual == input$var_id))
    if (length(idx) == 0) return(NULL)
    m <- rv$mapping$vars[[idx]]
    choices <- render_palette_choices()
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

        # 2. Category Tabs
        for(cat in cats) {
          results_cat <- res_df[res_df$Category == cat, ]
          if(nrow(results_cat) > 0) {
            results_cat <- results_cat[order(results_cat$AbsCorr, decreasing = TRUE), ]
            res_cat <- head(results_cat, 8)

            tabs[[length(tabs)+1]] <- tabPanel(cat,
              tags$ul(style="font-size: 0.85em; padding-left: 15px; margin-top: 5px; list-style-type: none;",
                lapply(1:nrow(res_cat), function(i) {
                  tags$li(sprintf("%s: %.3f (p=%.3f)", res_cat$Label[i], res_cat$Corr[i], res_cat$Pval[i]))
                })
              )
            )
          }
        }        
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

            list(l = item$l, best_lam = best_lam, gcv_data = gcv_res, err = NULL)          }, error = function(e) {
            list(l = item$l, best_lam = 0, gcv_data = NULL, err = e$message)
          })
        }, .options = furrr::furrr_options(seed = TRUE, packages = c("sf", "fields")))
        
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
          if(nrow(item$df) < 5) return(list(l = item$l, best_f = 2.0))
          pts <- sf::st_as_sf(item$df, coords=c("x","y"), crs=current_crs)
          best_f <- optimize_idw_p(pts, "v", nmax = idw_nmax_val)
          return(list(l = item$l, best_f = best_f))
        }, .options = furrr::furrr_options(seed = TRUE, packages = c("sf", "gstat")))

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
  observe({
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

  observe({
    if(input$vgm_mode == "manual") shinyjs::disable("auto_fit") else shinyjs::enable("auto_fit")
  })

  observe({
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
  observe({
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
  observe({
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
        res_a <- list(emp = NULL, fit = NULL, mod = "FAIL", sse = "N/A")
        sub_a_raw <- sf::st_as_sf(item$act, coords=c("x","y"), crs=current_crs)
        
        if(sf::st_is_longlat(sub_a_raw)) {
            c_geo <- sf::st_coordinates(sf::st_transform(sub_a_raw, 4326))
            utm_crs <- paste0("+proj=utm +zone=", floor((mean(c_geo[,1]) + 180) / 6) + 1, " +datum=WGS84 +units=m +no_defs", if(mean(c_geo[,2]) < 0) " +south" else "")
            sub_a <- sf::st_transform(sub_a_raw, utm_crs)
        } else { sub_a <- sub_a_raw }
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
          if(sf::st_is_longlat(sub_p_raw)) {
              c_geo <- sf::st_coordinates(sf::st_transform(sub_p_raw, 4326))
              utm_crs <- paste0("+proj=utm +zone=", floor((mean(c_geo[,1]) + 180) / 6) + 1, " +datum=WGS84 +units=m +no_defs", if(mean(c_geo[,2]) < 0) " +south" else "")
              sub_p <- sf::st_transform(sub_p_raw, utm_crs)
          } else { sub_p <- sub_p_raw }
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
      }, .options = furrr::furrr_options(seed = TRUE, packages = c("sf", "gstat")))
      
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
  
# --- Extracted Pure Function ---
apply_interpolation <- function(data, target_var, method, grid_p, aux_vars, lags, method_params, l, prefix) {
  res <- list(v_emp = NULL, fit = NULL, cv_metrics = NULL, model_summary = NULL, 
              rf_model = NULL, gstat_obj = NULL, res_sf = NULL, log_msg = "")
  
  # Capture the current local environment for safe mutation inside handlers
  env <- environment()
  
  data$target <- data[[target_var]]
  
  tryCatch({
    if(method == "OK") {
      res$v_emp <- variogram(target ~ 1, data, width = lags$width, cutoff = lags$cutoff)
      res$fit <- if(!is.null(method_params$pre_fit)) method_params$pre_fit else robust_vgm_fit(res$v_emp, data$target)
      cv_obj <- tryCatch(krige.cv(target ~ 1, data, model = res$fit, nfold = nrow(data), debug.level = 0), error = function(e) { env$res$log_msg <- paste0("OK CV Error: ", e$message); NULL })
      res$cv_obj <- cv_obj
      res$cv_metrics <- perform_cv(cv_obj)
      res$res_sf <- krige(target ~ 1, data, grid_p, model = res$fit, debug.level = 0)
      
      # NaN Protection for unstable Kriging results (especially Iron/low-variance)
      if (!is.null(res$res_sf)) {
        res$res_sf$var1.pred[is.nan(res$res_sf$var1.pred) | is.infinite(res$res_sf$var1.pred)] <- NA
        if ("var1.var" %in% colnames(res$res_sf)) {
          res$res_sf$var1.var[is.nan(res$res_sf$var1.var) | is.infinite(res$res_sf$var1.var)] <- NA
        }
      }
    } else if(method == "RK" && length(aux_vars) > 0) {
      if(length(aux_vars) > 1) {
        vif_res <- check_vif(st_drop_geometry(data)[, aux_vars, drop = FALSE])
        if(length(vif_res$dropped) > 0) {
          res$log_msg <- paste0(res$log_msg, " [VIF] Dropped: ", paste(vif_res$dropped, collapse=", "))
          aux_vars <- vif_res$kept
        }
      }
      grid_aux <- grid_p
      for(av in aux_vars) {
        tryCatch({
          v_emp_av <- variogram(as.formula(paste0("`", av, "` ~ 1")), data, width = lags$width, cutoff = lags$cutoff)
          fit_av <- robust_vgm_fit(v_emp_av, data[[av]])
          res_av <- krige(as.formula(paste0("`", av, "` ~ 1")), data, grid_p, model = fit_av, debug.level = 0)
          grid_aux[[av]] <- res_av$var1.pred
        }, error = function(e) {
          tryCatch({ showNotification(sprintf("Covariate %s kriging failed. Falling back to IDW.", av), type = "warning", duration = 8) }, error=function(err) NULL)
          idw_p <- if(!is.null(method_params$idw_p)) method_params$idw_p else 2
          idw_nmax <- if(!is.null(method_params$idw_nmax)) method_params$idw_nmax else 12
          res_av <- idw(as.formula(paste(av, "~ 1")), data, grid_p, nmax = idw_nmax, idp = idw_p, debug.level = 0)
          grid_aux[[av]] <- res_av$var1.pred
        })
      }
      form_reg <- as.formula(paste("target ~", paste(aux_vars, collapse = " + ")))
      lm_mod <- lm(form_reg, data = data)
      res$model_summary <- summary(lm_mod)
      data$residuals <- residuals(lm_mod)
      v_emp_res <- variogram(residuals ~ 1, data, width = lags$width, cutoff = lags$cutoff)
      fit_res <- robust_vgm_fit(v_emp_res, data$residuals)
      res_krig <- krige(residuals ~ 1, data, grid_p, model = fit_res, debug.level = 0)
      pred_trend <- predict(lm_mod, newdata = grid_aux, se.fit = TRUE)
      trend_var <- (pred_trend$se.fit)^2
      res$res_sf <- grid_p %>% mutate(var1.pred = as.vector(pred_trend$fit + res_krig$var1.pred), var1.var = as.vector(trend_var + res_krig$var1.var))
      
      # Correct Full-Pipeline CV
      cv_obj <- tryCatch({
        perform_rk_cv(data, "target", aux_vars, calc_scientific_lags, robust_vgm_fit)
      }, error = function(e) { env$res$log_msg <- paste0("RK CV Error: ", e$message); NULL })
      
      res$cv_obj <- cv_obj
      res$cv_metrics <- perform_cv(cv_obj)
    } else if(method == "RFK" && length(aux_vars) > 0) {
      grid_aux <- grid_p
      for(av in aux_vars) {
        tryCatch({
          v_emp_av <- variogram(as.formula(paste0("`", av, "` ~ 1")), data, width = lags$width, cutoff = lags$cutoff)
          fit_av <- robust_vgm_fit(v_emp_av, data[[av]])
          res_av <- krige(as.formula(paste0("`", av, "` ~ 1")), data, grid_p, model = fit_av, debug.level = 0)
          grid_aux[[av]] <- res_av$var1.pred
        }, error = function(e) {
          tryCatch({ showNotification(sprintf("Covariate %s kriging failed. Falling back to IDW.", av), type = "warning", duration = 8) }, error=function(err) NULL)
          idw_p <- if(!is.null(method_params$idw_p)) method_params$idw_p else 2
          idw_nmax <- if(!is.null(method_params$idw_nmax)) method_params$idw_nmax else 12
          res_av <- idw(as.formula(paste(av, "~ 1")), data, grid_p, nmax = idw_nmax, idp = idw_p, debug.level = 0)
          grid_aux[[av]] <- res_av$var1.pred
        })
      }
      form_reg <- as.formula(paste("target ~", paste(aux_vars, collapse = " + ")))
      rf_mod <- randomForest::randomForest(form_reg, data = data, ntree = 200, importance = TRUE)
      res$rf_model <- rf_mod
      data$residuals <- data$target - rf_mod$predicted
      v_emp_res <- variogram(residuals ~ 1, data, width = lags$width, cutoff = lags$cutoff)
      fit_res <- robust_vgm_fit(v_emp_res, data$residuals)
      res_krig <- krige(residuals ~ 1, data, grid_p, model = fit_res, debug.level = 0)
      pred_trend_all <- predict(rf_mod, grid_aux, predict.all = TRUE)
      trend_var <- apply(pred_trend_all$individual, 1, var)
      res$res_sf <- grid_p %>% mutate(var1.pred = as.vector(pred_trend_all$aggregate + res_krig$var1.pred), var1.var = as.vector(trend_var + res_krig$var1.var))
      
      # Correct Full-Pipeline CV
      cv_obj <- tryCatch({
        perform_rfk_cv(data, "target", aux_vars, calc_scientific_lags, robust_vgm_fit)
      }, error = function(e) { env$res$log_msg <- paste0("RFK CV Error: ", e$message); NULL })
      
      res$cv_obj <- cv_obj
      res$cv_metrics <- perform_cv(cv_obj)
    } else if(method == "CK" && length(aux_vars) > 0) {
      # Standardize covariates to mean 0, var 1
      for(av in aux_vars) {
        data[[av]] <- scale(data[[av]])
      }
      g <- gstat(NULL, id = "target", formula = target ~ 1, data = data)
      for(av in aux_vars) g <- gstat(g, id = av, formula = as.formula(paste(av, "~ 1")), data = data)
      vm <- variogram(g, width = lags$width, cutoff = lags$cutoff)
      
      # Adaptive LMC fitting
      v_emp_ok <- variogram(target ~ 1, data, width = lags$width, cutoff = lags$cutoff)
      fit_ok_init <- robust_vgm_fit(v_emp_ok, data$target)
      m_type <- suggest_lmc_model(fit_ok_init)
      
      g <- tryCatch({
        fit.lmc(vm, g, vgm(var(data$target), m_type, lags$cutoff / 2, 0), correct.diagonal = 1.01)
      }, error = function(e) {
        env$res$log_msg <- paste0("LMC Fit Failed: ", e$message, ". Falling back to OK.")
        tryCatch({ showNotification(paste("Co-Kriging convergence failed. Falling back to Ordinary Kriging."), type = "warning", duration = 8) }, error=function(err) NULL)
        NULL
      })
      
      if(!is.null(g)) {
        res$gstat_obj <- g
        cv_obj <- tryCatch(gstat.cv(g, nfold = nrow(data), debug.level = 0), error = function(e) { env$res$log_msg <- paste0("CK CV Error: ", e$message); NULL })
        res$cv_obj <- cv_obj
      res$cv_metrics <- perform_cv(cv_obj)
        res$res_sf <- tryCatch({
          predict(g, grid_p, debug.level = 0) %>% st_as_sf() %>% rename(var1.pred = target.pred, var1.var = target.var)
        }, error = function(e) {
          env$res$log_msg <- paste0("CK Prediction Failed: ", e$message, ". Falling back to OK.")
          NULL
        })
      }
      
      # Fallback to OK if CK failed
      if(is.null(res$res_sf)) {
         v_emp_ok <- variogram(target ~ 1, data, width = lags$width, cutoff = lags$cutoff)
         fit_ok <- robust_vgm_fit(v_emp_ok, data$target)
         res$res_sf <- krige(target ~ 1, data, grid_p, model = fit_ok, debug.level = 0)
         # Re-calc CV for OK fallback if needed? 
         # The UI might show CK selected but OK results. ideally we log this.
      }
    } else if(method == "IDW") {
      cv_obj <- tryCatch(krige.cv(target ~ 1, data, nmax = method_params$idw_nmax, set = list(idp = method_params$idw_p), nfold = nrow(data), debug.level = 0), error = function(e) { env$res$log_msg <- paste0("IDW CV Error: ", e$message); NULL })
      res$cv_obj <- cv_obj
      res$cv_metrics <- perform_cv(cv_obj)
      res$res_sf <- idw(target ~ 1, data, grid_p, nmax = method_params$idw_nmax, idp = method_params$idw_p, debug.level = 0)
    } else {
      res$res_sf <- tryCatch({
        raw_pts <- st_coordinates(data)
        xm <- min(raw_pts[,1]); xM <- max(raw_pts[,1])
        ym <- min(raw_pts[,2]); yM <- max(raw_pts[,2])
        max_range <- max(xM - xm, yM - ym)
        if(max_range == 0) max_range <- 1
        pts_sc <- cbind((raw_pts[,1]-xm)/max_range, (raw_pts[,2]-ym)/max_range)
        gr_raw <- st_coordinates(grid_p)
        gr_sc <- cbind((gr_raw[,1]-xm)/max_range, (gr_raw[,2]-ym)/max_range)
        mod <- fields::Tps(pts_sc, data$target, lambda = method_params$tps_lambda)
        p_v <- fields::predict.Krig(mod, gr_sc)
        
        # Calculate realistic CV predictions using LOOCV
        n_pts <- nrow(data)
        cv_vals <- vapply(1:n_pts, function(i) {
          tryCatch({
            tmp_mod <- fields::Tps(pts_sc[-i, , drop=FALSE], data$target[-i], lambda = method_params$tps_lambda)
            as.numeric(fields::predict.Krig(tmp_mod, pts_sc[i, , drop=FALSE]))
          }, error = function(e) NA_real_)
        }, numeric(1))
        
        # Fallback for failed folds
        if(any(is.na(cv_vals))) {
          cv_vals[is.na(cv_vals)] <- mod$fitted.values[is.na(cv_vals)]
        }
        
        cv_res <- data.frame(observed = data$target, var1.pred = cv_vals, x = raw_pts[,1], y = raw_pts[,2])
        res$cv_obj <- cv_res
        res$cv_metrics <- perform_cv(cv_res)
        grid_p %>% mutate(var1.pred = as.vector(p_v))
      }, error = function(e) idw(target ~ 1, data, grid_p, nmax = 12, idp = 2))
    }
  }, error = function(e) { env$res$log_msg <- paste0("Error in apply_interpolation: ", e$message) })
  
  return(res)
}

  observeEvent(input$run, {
    req(rv$user_data, input$locality, rv$mapping$x, rv$mapping$y); 
    locs <- if("ALL" %in% input$locality) unique(rv$user_data[[rv$mapping$loc]]) else input$locality
    meta <- get_current_meta()
    req(meta)
    
    # Automatically switch to Map Viewer tab to show progress
    updateTabsetPanel(session, "main_tabs", selected = "tab_map")
    shinyjs::runjs("$('#main_tabs li a[data-value=\"tab_map\"]').click();")
    
    # Show progress overlay, hide reveal button
    shinyjs::show("map_progress_overlay")
    shinyjs::hide("map_reveal_overlay")
    shinyjs::runjs("document.getElementById('map_progress_bar_fill').style.width = '5%'; document.getElementById('map_progress_text').innerText = 'Initializing Spatial Analysis Engine...';")

    rv$rast_list_act <- list(); rv$rast_list_pre <- list(); sf_list <- list(); b_list <- list()
    rv$rast <- NULL; rv$rast_pred <- NULL; rv$rast_res <- NULL; rv$has_predictions <- FALSE
    rv$v_emp_list <- list(); rv$log <- paste0("Starting spatial interpolation using method: ", input$method, "...")
    rv$run_method[[input$var_id]] <- input$method
    rv$model_summaries <- list(); rv$rf_models <- list(); rv$gstat_objs <- list()
    rv$cv_metrics_act <- list(); rv$cv_metrics_pre <- list() # Reset CV metrics
    rv$cv_data_act <- list(); rv$cv_data_pre <- list()
    
    shinyjs::runjs("document.getElementById('map_progress_bar_fill').style.width = '15%'; document.getElementById('map_progress_text').innerText = 'Validating and Cleaning Spatial Input Data...';")
    
    pred_col <- if(input$value_type == "pred_ss") meta$pred_ss else meta$pred
    aux_vars <- input$aux_vars
    
    shinyjs::runjs("document.getElementById('map_progress_bar_fill').style.width = '25%'; document.getElementById('map_progress_text').innerText = 'Preparing Neighborhood Search Grids...';")
    
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
    b_dist <- input$buff_dist
    shp_bound <- rv$shp_bound
    res_mode <- input$res_mode
    grid_res <- input$grid_res
    crs_sel <- input$crs_selection
    comp_mode <- input$comp_mode
    sep_fit <- input$sep_fit
    idw_p_val <- input$idw_p
    idw_nmax_val <- input$idw_nmax
    tps_lambda_val <- input$tps_lambda
    
    shinyjs::runjs("document.getElementById('map_progress_bar_fill').style.width = '35%'; document.getElementById('map_progress_text').innerText = 'Organizing Localized Data Chunks...';")
    
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
        pre_fit_act = rv$v_fit_list[[paste0(l, "_act")]],
        pre_fit_pre = if(sep_fit) rv$v_fit_list[[paste0(l, "_pre")]] else rv$v_fit_list[[paste0(l, "_act")]]
      )
      
      list(l = l, pts_data = pts_data, m_params = m_params)
    })

    shinyjs::runjs("document.getElementById('map_progress_bar_fill').style.width = '50%'; document.getElementById('map_progress_text').innerText = 'Executing Parallel Interpolation Algorithms...';")

    res_all <- furrr::future_map(df_list, function(item) {
      l <- item$l
      pts_data <- item$pts_data
      m_params <- item$m_params        
        res_out <- list(l = l, r_a = NULL, r_p = NULL, r_res = NULL, bound = NULL, pts = NULL, 
                        v_emp_act = NULL, v_fit_act = NULL, cv_act = NULL, cv_obj_act = NULL, summ_act = NULL, rf_act = NULL, gstat_act = NULL,
                        v_emp_pre = NULL, v_fit_pre = NULL, cv_pre = NULL, cv_obj_pre = NULL, summ_pre = NULL, rf_pre = NULL, gstat_pre = NULL, log_msg = "", actual_res = NULL)
        
        pts_raw <- pts_data %>% filter(!is.na(x), !is.na(y)) %>% sf::st_as_sf(coords=c("x","y"), crs=current_crs)
        if (current_method %in% c("RK", "RFK", "CK") && length(aux_vars) > 0) {
           pts_raw <- pts_raw %>% filter(dplyr::if_all(dplyr::all_of(aux_vars), ~!is.na(.)))
        }
        
        coords_4326_geo <- sf::st_coordinates(sf::st_transform(pts_raw, 4326))
        lon_c <- mean(coords_4326_geo[,1])
        lat_c <- mean(coords_4326_geo[,2])
        utm_zone <- floor((lon_c + 180) / 6) + 1
        utm_crs <- paste0("+proj=utm +zone=", utm_zone, " +datum=WGS84 +units=m +no_defs")
        if (lat_c < 0) utm_crs <- paste0(utm_crs, " +south")
        
        pts <- sf::st_transform(pts_raw, utm_crs)
        if(nrow(pts) < 3) return(res_out)
        
        c_round <- round(sf::st_coordinates(pts), 2)
        pts <- pts[!duplicated(cbind(c_round[,1], c_round[,2])),]
        if(nrow(pts) < 3) return(res_out)
        
        tryCatch({
            bound <- if(!is.null(shp_bound)) {
              sf::st_transform(shp_bound, sf::st_crs(pts)) %>% sf::st_union() %>% sf::st_as_sf()
            } else {
              tryCatch({
                b <- switch(b_type,
                       "convex"  = sf::st_convex_hull(sf::st_union(pts)),
                       "concave" = concaveman::concaveman(pts),
                       "wrapped" = sf::st_buffer(concaveman::concaveman(pts), dist = b_dist),
                       "strict"  = sf::st_union(sf::st_buffer(pts, dist = b_dist)))
                sf::st_as_sf(sf::st_sfc(sf::st_geometry(b), crs = sf::st_crs(pts)))
              }, error = function(e) sf::st_as_sf(sf::st_sfc(sf::st_convex_hull(sf::st_union(pts)), crs = sf::st_crs(pts))))
            }
            
            bbox <- sf::st_bbox(bound)
            width <- as.numeric(bbox["xmax"] - bbox["xmin"])
            height <- as.numeric(bbox["ymax"] - bbox["ymin"])
            
            if (res_mode == "local") {
               coords_local <- sf::st_coordinates(pts)
               if (nrow(coords_local) > 1) {
                 knn_res <- FNN::get.knn(coords_local, k = 1)
                 actual_res <- mean(knn_res$nn.dist) * 0.5
               } else {
                 actual_res <- grid_res
               }
            } else {
               actual_res <- grid_res
               crs_obj_target <- tryCatch(sf::st_crs(crs_sel), error = function(e) NULL)
               if (!is.null(crs_obj_target) && grepl("degree", crs_obj_target$units_gdal %||% "meters", ignore.case = TRUE)) {
                  coords_4326 <- sf::st_coordinates(sf::st_transform(pts_raw, 4326))
                  lat_c <- mean(coords_4326[,2])
                  m_per_deg <- 111319 * cos(lat_c * pi / 180) 
                  actual_res <- grid_res * m_per_deg
               }
            }
            
            max_dim <- max(width, height)
            min_res_safe <- max_dim / 300
            if (actual_res > min_res_safe) actual_res <- max(0.1, min_res_safe)

            grid_r <- terra::rast(terra::ext(bbox), res=actual_res, crs=sf::st_crs(pts)$wkt)
            grid_p <- terra::as.points(grid_r, values=FALSE) %>% sf::st_as_sf() %>%
              dplyr::mutate(x = sf::st_coordinates(.)[,1], y = sf::st_coordinates(.)[,2])
            
            r_a <- NULL; r_p <- NULL
            
            pts_a <- pts %>% dplyr::filter(!is.na(v)) %>% dplyr::mutate(x = sf::st_coordinates(.)[,1], y = sf::st_coordinates(.)[,2])
            if(nrow(pts_a) >= 3) {
                lags_a <- calc_scientific_lags(pts_a)
                mp_a <- list(idw_p = m_params$idw_p_act, idw_nmax = m_params$idw_nmax, tps_lambda = m_params$tps_lambda_act, pre_fit = m_params$pre_fit_act)
                res_a_list <- apply_interpolation(pts_a, "v", current_method, grid_p, aux_vars, lags_a, mp_a, l, "act")
                res_out$v_emp_act <- res_a_list$v_emp; res_out$v_fit_act <- res_a_list$fit; res_out$cv_act <- res_a_list$cv_metrics; res_out$cv_obj_act <- res_a_list$cv_obj
                res_out$summ_act <- res_a_list$model_summary; res_out$rf_act <- res_a_list$rf_model; res_out$gstat_act <- res_a_list$gstat_obj
                res_out$log_msg <- paste0(res_out$log_msg, "
", res_a_list$log_msg)
                
                if(!is.null(res_a_list$res_sf)) {
                    fields_a <- if("var1.var" %in% colnames(res_a_list$res_sf)) c("var1.pred", "var1.var") else "var1.pred"
                    r_a <- terra::rasterize(res_a_list$res_sf, grid_r, field=fields_a) %>% terra::mask(terra::vect(bound)) %>% terra::project(crs_sel)
                    res_out$r_a <- terra::wrap(r_a)
                }
            }
            
            if(comp_mode || val_type != "actual") {
                pts_p <- pts %>% dplyr::filter(!is.na(pv)) %>% dplyr::mutate(x = sf::st_coordinates(.)[,1], y = sf::st_coordinates(.)[,2])
                if(nrow(pts_p) >= 3) {
                    lags_p <- calc_scientific_lags(pts_p)
                    mp_p <- list(idw_p = m_params$idw_p_pre, idw_nmax = m_params$idw_nmax, tps_lambda = m_params$tps_lambda_pre, pre_fit = m_params$pre_fit_pre)
                    res_p_list <- apply_interpolation(pts_p, "pv", current_method, grid_p, aux_vars, lags_p, mp_p, l, "pre")
                    res_out$v_emp_pre <- res_p_list$v_emp; res_out$v_fit_pre <- res_p_list$fit; res_out$cv_pre <- res_p_list$cv_metrics; res_out$cv_obj_pre <- res_p_list$cv_obj
                    res_out$summ_pre <- res_p_list$model_summary; res_out$rf_pre <- res_p_list$rf_model; res_out$gstat_pre <- res_p_list$gstat_obj
                    res_out$log_msg <- paste0(res_out$log_msg, "
", res_p_list$log_msg)
                    
                    if(!is.null(res_p_list$res_sf)) {
                        fields_p <- if("var1.var" %in% colnames(res_p_list$res_sf)) c("var1.pred", "var1.var") else "var1.pred"
                        r_p <- terra::rasterize(res_p_list$res_sf, grid_r, field=fields_p) %>% terra::mask(terra::vect(bound)) %>% terra::project(crs_sel)
                        res_out$r_p <- terra::wrap(r_p)
                    }
                }
            }
            
            if(!is.null(r_a) && !is.null(r_p)) res_out$r_res <- terra::wrap(r_a - r_p)
            
            # --- Extended Residuals: Kriged Point Errors ---
            pts_err_raw <- pts_data %>% filter(!is.na(v), !is.na(pv))
            if(nrow(pts_err_raw) >= 3) {
                pts_err <- sf::st_as_sf(pts_err_raw, coords=c("x","y"), crs=utm_crs) %>%
                           dplyr::mutate(err = v - pv)
                # Use IDW for a robust, fast error surface (doesn't need variogram)
                err_mod <- gstat::idw(err ~ 1, pts_err, grid_p, nmax = 12, idp = 2, debug.level = 0)
                r_err <- terra::rasterize(err_mod, grid_r, field="var1.pred") %>% terra::mask(terra::vect(bound)) %>% terra::project(crs_sel)
                res_out$r_point_err <- terra::wrap(r_err)
            }
            
            res_out$bound <- sf::st_transform(bound, crs_sel)
            res_out$pts <- sf::st_transform(pts, crs_sel) %>% dplyr::mutate(loc = l, resid = v - pv)
            res_out$actual_res <- actual_res
            
        }, error = function(e) {
            res_out$log_msg <- paste0(res_out$log_msg, "
Error in ", l, ": ", e$message)
        })
        
        return(res_out)
      }, .options = furrr::furrr_options(seed = TRUE, packages = c("sf", "terra", "dplyr", "gstat", "randomForest", "fields", "concaveman", "FNN")))
      
      # Aggregate results back to rv
      for(res in res_all) {
          l <- res$l
          if(res$log_msg != "") {
              rv$log <- paste0(rv$log, res$log_msg)
              if(grepl("Error", res$log_msg)) showNotification(res$log_msg, type = "error", duration = 8)
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
      rv$rast <- terra::wrap(if(length(valid_a) > 1) do.call(merge, lapply(unname(valid_a), terra::unwrap)) else terra::unwrap(valid_a[[1]]))
      register_export_item("map_actual", paste(meta$label, "- Actual Map"), "map", rv$rast, meta$category)
    }
    if(length(valid_p) > 0) {
      rv$rast_pred <- terra::wrap(if(length(valid_p) > 1) do.call(merge, lapply(unname(valid_p), terra::unwrap)) else terra::unwrap(valid_p[[1]]))
      rv$has_predictions <- TRUE
      register_export_item("map_predicted", paste(meta$label, "- Predicted Map"), "map", rv$rast_pred, meta$category)
    }
    if(length(valid_r) > 0) {
      rv$rast_res <- terra::wrap(if(length(valid_r) > 1) do.call(merge, lapply(unname(valid_r), terra::unwrap)) else terra::unwrap(valid_r[[1]]))
      register_export_item("map_residuals", paste(meta$label, "- Residual Map (Delta)"), "map", rv$rast_res, meta$category)
    }
    if(length(valid_pr) > 0) {
      rv$rast_point_res <- terra::wrap(if(length(valid_pr) > 1) do.call(merge, lapply(unname(valid_pr), terra::unwrap)) else terra::unwrap(valid_pr[[1]]))
      register_export_item("map_point_residuals", paste(meta$label, "- Point Error Map"), "map", rv$rast_point_res, meta$category)
    }
    
    # NEW: Register Combined Comparison Map
    if(!is.null(rv$rast) && !is.null(rv$rast_pred)) {
       register_export_item("map_comparison", paste(meta$label, "- Actual vs Predicted Comparison"), "map_combined", list(act = rv$rast, pre = rv$rast_pred), meta$category)
    }
    
    if(length(sf_list) > 0) rv$sf <- do.call(rbind, unname(sf_list))
    if(length(b_list) > 0) rv$bound <- do.call(rbind, unname(b_list)) %>% st_union()
    rv$loc_names <- names(valid_a)
    
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
      
      # 2. Total Descriptive Statistics
      s_a <- summary(df_perf$v)
      stats_total <- data.frame(Metric = names(s_a), Value = as.character(round(as.numeric(s_a), 3)))
      register_export_item("table_stats_total", paste(meta$label, "- Total Descriptive Statistics"), "table", stats_total, meta$category)
      
      # 3. Total Classification Performance (Kappa)
      # Using default 'agro' binning for registration
      params_k <- tryCatch(agro_params(), error = function(e) NULL)
      if(!is.null(params_k)) {
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
       area_total <- calc_area_df(terra::unwrap(rv$rast))
       if(is.data.frame(area_total)) register_export_item("table_area_total", paste(meta$label, "- Total Area Coverage"), "table", area_total, meta$category)
    }

    # --- REGISTRY: Per-Locality Assets ---
    for(l in locs) {
       # A. Tables per locality
       if(!is.null(rv$sf)) {
         df_l <- rv$sf %>% st_drop_geometry() %>% filter(loc == !!l, !is.na(v), !is.na(pv))
         if(nrow(df_l) >= 3) {
           # 1. Prediction Performance
           perf_l <- data.frame(
             Metric = c("R2 (Trad)", "R2 (Corr)", "RMSE", "MBE (Bias)", "CCC", "RPD"),
             Value = c(
               round(yardstick::rsq_trad_vec(df_l$v, df_l$pv), 4),
               round(yardstick::rsq_vec(df_l$v, df_l$pv), 4),
               round(yardstick::rmse_vec(df_l$v, df_l$pv), 4),
               round(mean(df_l$pv - df_l$v, na.rm=TRUE), 4),
               round(yardstick::ccc_vec(df_l$v, df_l$pv), 4),
               round(yardstick::rpd_vec(df_l$v, df_l$pv), 4)
             )
           )
           register_export_item(paste0("table_perf_loc_", l), paste(meta$label, "-", l, "- Prediction Performance"), "table", perf_l, meta$category)
           
           # 2. Descriptive Stats
           s_l <- summary(df_l$v)
           stats_l <- data.frame(Metric = names(s_l), Value = as.character(round(as.numeric(s_l), 3)))
           register_export_item(paste0("table_stats_loc_", l), paste(meta$label, "-", l, "- Descriptive Statistics"), "table", stats_l, meta$category)
         }
       }
       
       # 3. Interpolation CV Metrics
       if(!is.null(rv$cv_metrics_act[[l]])) {
         cv_l <- rv$cv_metrics_act[[l]]
         cv_table <- data.frame(Metric = names(cv_l), Value = as.character(round(as.numeric(cv_l), 4)))
         register_export_item(paste0("table_cv_loc_", l), paste(meta$label, "-", l, "- Model CV Metrics"), "table", cv_table, meta$category)
       }
       
       # 4. Area Coverage
       if(isTruthy(input$color_style == "agro") && !is.null(rv$rast_list_act[[l]])) {
         area_l <- calc_area_df(terra::unwrap(rv$rast_list_act[[l]]))
         if(is.data.frame(area_l)) register_export_item(paste0("table_area_loc_", l), paste(meta$label, "-", l, "- Area Coverage"), "table", area_l, meta$category)
       }

       # B. Plots per locality (Captured as objects)
       # 1. Variogram (Actual)
       if(!is.null(rv$v_emp_list[[paste0(l, "_act")]])) {
         v_emp <- rv$v_emp_list[[paste0(l, "_act")]]
         v_fit <- rv$v_fit_list[[paste0(l, "_act")]]
         # Store the plot object
         p_vgm <- plot(v_emp, v_fit, main = paste("Variogram (Actual):", l))
         register_export_item(paste0("plot_vgm_act_", l), paste(meta$label, "-", l, "- Variogram (Actual)"), "plot", p_vgm, meta$category)
         
         # NEW: Register Tabular Variogram Data
         df_vgm <- as.data.frame(v_emp) %>% select(np, dist, gamma, dir.hor, dir.ver)
         register_export_item(paste0("table_vgm_act_", l), paste(meta$label, "-", l, "- Variogram Data (Actual)"), "table", df_vgm, meta$category)
       }
       
       # 2. Obs vs Pred (Actual)
       if(!is.null(rv$cv_data_act[[l]])) {
         df_cv <- as.data.frame(rv$cv_data_act[[l]])
         # Robust column finding
         o_col <- grep("\\.observed$|^observed$|^target$", colnames(df_cv), value = TRUE)[1]
         p_col <- grep("\\.pred$|^var1\\.pred$", colnames(df_cv), value = TRUE)[1]
         
         if(!is.na(o_col) && !is.na(p_col)) {
           # Use a local copy of data to ensure it's captured in the ggplot object
           df_p <- data.frame(Observed = df_cv[[o_col]], Predicted = df_cv[[p_col]])
           p_op <- ggplot(df_p, aes(x = Observed, y = Predicted)) +
             geom_point(alpha = 0.6) + geom_abline(intercept = 0, slope = 1, color = "red", linetype = "dashed") +
             geom_smooth(method = "lm", color = "blue", se = FALSE) +
             labs(title = paste("Obs vs Pred:", l), x = "Observed", y = "Predicted") + theme_minimal()
           
           register_export_item(paste0("plot_obs_pred_", l), paste(meta$label, "-", l, "- Obs vs Pred Scatter"), "plot", p_op, meta$category)
         }
       }
       
       # 3. TPS GCV (if applicable)
       if(input$method == "TPS" && !is.null(rv$tps_gcv_data[[paste0(l, "_act")]])) {
         df_gcv <- rv$tps_gcv_data[[paste0(l, "_act")]]
         p_gcv <- ggplot(df_gcv, aes(x = lambda, y = gcv)) + 
           geom_line(color = "steelblue") + geom_point() + scale_x_log10() +
           labs(title = paste("TPS GCV Diagnostics:", l)) + theme_minimal()
         register_export_item(paste0("plot_tps_gcv_", l), paste(meta$label, "-", l, "- TPS GCV Curve"), "plot", p_gcv, meta$category)
       }
       
       # 4. RF Importance (if applicable)
       if(input$method == "RFK" && !is.null(rv$rf_models[[paste0(l, "_act")]])) {
         # varImpPlot doesn't return a ggplot easily, we'll try to convert or just store
         # For now, let's keep it simple
         rv$log <- paste0(rv$log, "\n[Registry] RF Importance plot skipped (non-ggplot)")
       }
    }
    
    rv$log <- paste0(rv$log, "\nDone.")

    shinyjs::runjs("document.getElementById('map_progress_bar_fill').style.width = '100%'; document.getElementById('map_progress_text').innerText = 'Complete!';")
    # Small delay before showing reveal overlay
    shinyjs::delay(500, {
      shinyjs::hide("map_progress_overlay")
      shinyjs::show("map_reveal_overlay")
    })
  })

  # Event to reveal maps and unlock analysis
  observeEvent(input$reveal_maps_btn, {
    shinyjs::hide("map_reveal_overlay")
    # Note: 'unlocking' analysis is conceptual here as the UI is generally reactive to rv$rast being set.
    showNotification("Maps and scientific analysis metrics are now available.", type="message")
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
    
    df <- data.frame(
      Locality = names(res_list),
      Resolution = sapply(res_list, function(x) if(is.numeric(x)) round(x, 2) else x)
    )
    df
  }, striped = TRUE, hover = TRUE, bordered = TRUE, width = "100%")

  # --- Map Helper ---
  draw_map <- function(r_obj, lab) {
    # Determine base tiles from input, fallback to theme, fallback to Esri
    current_tiles <- input$base_map_layer %||% if(isTruthy(input$styler_local_config)) jsonlite::fromJSON(input$styler_local_config)$map_tiles else "Esri.WorldImagery"
    if (is.null(current_tiles) || current_tiles == "") current_tiles <- "Esri.WorldImagery"
    
    # Check if we are drawing a raster or just points (resid_points mode)
    if(is.null(r_obj) && lab != "resid_points") return(leaflet() %>% addProviderTiles(current_tiles, layerId="base_tiles"))
    
    m <- leaflet() %>% addProviderTiles(current_tiles, layerId="base_tiles") %>%
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
    if(!is.null(r_obj)) {
      if(inherits(r_obj, "PackedSpatRaster")) r_obj <- terra::unwrap(r_obj)
      r_w <- terra::project(r_obj, "EPSG:4326")
      
      # Select the active layer based on uncertainty toggle
      is_uncertainty <- isTruthy(input$show_uncertainty) && input$method %in% c("OK", "RK", "RFK", "CK") && "var1.var" %in% names(r_w)
      if (is_uncertainty) {
        active_layer <- r_w[["var1.var"]]
        if (input$uncertainty_type == "se") {
          active_layer <- sqrt(active_layer)
          meta$label <- paste(meta$label, "(Std Error)")
        } else {
          meta$label <- paste(meta$label, "(Variance)")
        }
        meta$palette <- "inferno"
      } else {
        active_layer <- if("var1.pred" %in% names(r_w)) r_w[["var1.pred"]] else r_w[[1]]
      }
      
      vv <- as.vector(values(active_layer, na.rm=TRUE))
      vv_scale <- joint_vv() %||% vv

      is_viridis <- meta$palette == "viridis"
      if(input$value_type == "resid" || lab == "resid_raster") {
        # Use divergent palette for residuals: Red (Neg) -> White (Zero) -> Blue (Pos)
        abs_max <- max(abs(vv), na.rm = TRUE)
        if(is.infinite(abs_max) || is.na(abs_max)) abs_max <- 1
        # Removing reverse=TRUE makes Red negative and Blue positive in brewer RdBu
        pal <- colorNumeric("RdBu", domain = c(-abs_max, abs_max), na.color = "transparent")
        m <- m %>% addRasterImage(active_layer, colors = pal, opacity = 0.8)
        m <- m %>% leaflet::addLegend(pal = pal, values = c(-abs_max, abs_max), title = paste("Resid:", meta$label))
      } else if(input$color_style == "agro") {
        params <- agro_params()
        if(!is.null(params)) {
          pal <- colorBin(params$colors, bins = params$brks, na.color = "transparent", right = FALSE)
          m <- m %>% addRasterImage(active_layer, colors = pal, opacity = 0.8)
          m <- m %>% leaflet::addLegend(colors = params$colors, labels = params$leg_labels, opacity = 0.8, title = paste(meta$label, meta$unit))
        }
      } else if(input$color_style == "bin") {
        pal <- if(is_viridis) colorBin(viridis::viridis(256, option = meta$palette), vv_scale, bins = 5, na.color = "transparent") 
               else colorBin(meta$palette, vv_scale, bins = 5, na.color = "transparent")
        m <- m %>% addRasterImage(active_layer, colors = pal, opacity = 0.8)
        m <- m %>% leaflet::addLegend(pal = pal, values = vv_scale, opacity = 0.8, title = paste(meta$label, meta$unit))
      } else {
        pal <- if(is_viridis) colorNumeric(viridis::viridis(256, option = meta$palette), vv_scale, na.color = "transparent") 
               else colorNumeric(meta$palette, vv_scale, na.color = "transparent")
        m <- m %>% addRasterImage(active_layer, colors = pal, opacity = 0.8)
        
        # Dynamic precision based on range
        v_range <- diff(range(vv_scale, na.rm=TRUE))
        d_format <- if(is.na(v_range)) 2 else if(v_range < 0.01) 6 else if(v_range < 0.1) 4 else 2
        m <- m %>% leaflet::addLegend(pal = pal, values = vv_scale, title = paste(meta$label, meta$unit), labFormat = labelFormat(digits = d_format))
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
       popups <- lapply(1:nrow(df_clean), function(i) generate_popup(df_clean[i, ])) %>% unlist()
       
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
        df_clean <- st_drop_geometry(pts_view)
        popups <- lapply(1:nrow(df_clean), function(i) generate_popup(df_clean[i, ])) %>% unlist()
        
        # Even in single resid view, user says "regular sample points"
        m <- m %>% addCircleMarkers(data = pts_view, radius = 3, color = "cyan", opacity = 1, weight = 1,
                                   fillOpacity = 0.5, popup = popups)
      }
    }
    
    if(input$show_borders && !is.null(rv$bound)) {
      m <- m %>% addPolygons(data = st_transform(st_as_sf(rv$bound), 4326), fill = FALSE, color = "white", weight = 2)
    }
    
    if(input$show_north) {
      m <- m %>% addControl(html="<div style='color:white; font-size:20px; font-weight:bold; text-shadow: 1px 1px 2px black;'>  N</div>", position="topleft")
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
      m_name <- switch(current_method,
        "OK"  = "Ordinary Kriging",
        "RK"  = "Regression Kriging",
        "RFK" = "Random Forest Kriging",
        "CK"  = "Co-Kriging",
        "IDW" = "IDW",
        "TPS" = "Thin Plate Spline",
        current_method)
      paste0(" (", m_name, ")")
    } else ""
    
    prefix <- meta$label
    paste0(prefix, " - ", type_lab, method_lab)
  })

  output$comp_left_title <- renderText({
    req(input$var_id, rv$mapping$vars); meta <- get_current_meta(); req(meta)
    
    current_method <- rv$run_method[[input$var_id]]
    method_lab <- if(!is.null(current_method)) {
      m_name <- switch(current_method, "OK"="Ordinary Kriging","UK"="Universal Kriging","RK"="Regression Kriging","RFK"="Random Forest Kriging","CK"="Co-Kriging","IDW"="IDW","TPS"="Thin Plate Spline", current_method)
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
      m_name <- switch(current_method, "OK"="Ordinary Kriging","UK"="Universal Kriging","RK"="Regression Kriging","RFK"="Random Forest Kriging","CK"="Co-Kriging","IDW"="IDW","TPS"="Thin Plate Spline", current_method)
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

  observe({
    req(input$base_map_layer)
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
    # Trigger window resize to force Leaflet invalidateSize which fixes gray tiles
    shinyjs::runjs("setTimeout(function() { window.dispatchEvent(new Event('resize')); }, 100);")
  })

  output$main_map <- renderLeaflet({
    req(input$value_type); req(rv$run_method[[input$var_id]])
    target <- if(input$value_type == "actual") rv$rast
              else if(input$value_type == "resid") rv$rast_res
              else rv$rast_pred
    draw_map(target, input$value_type)
  })

  output$comp_map_left <- renderLeaflet({
    req(rv$run_method[[input$var_id]])
    if(input$value_type == "resid") {
      draw_map(rv$rast_res, "resid_raster")
    } else {
      draw_map(rv$rast, "Actual")
    }
  })

  output$comp_map_right <- renderLeaflet({
    req(rv$run_method[[input$var_id]])
    if(input$value_type == "resid") {
      draw_map(NULL, "resid_points")
    } else {
      draw_map(terra::unwrap(rv$rast_pred), "Predicted")
    }
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
      req(rv$sf)
      plot(variogram(v ~ 1, rv$sf), main = paste("Global Variogram (Actual):", meta$label))
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
      req(rv$sf)
      plot(variogram(pv ~ 1, rv$sf %>% filter(!is.na(pv))), main = paste("Global Variogram (Predicted):", meta$label))
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
    loc <- input$sel_loc_stats; req(rv$model_summaries[[paste0(loc, "_act")]])
    s <- rv$model_summaries[[paste0(loc, "_act")]]
    tagList(verbatimTextOutput(paste0("summ_act_", loc)))
  })
  output$model_summary_ui_pre <- renderUI({
    loc <- input$sel_loc_stats; req(rv$model_summaries[[paste0(loc, "_pre")]])
    s <- rv$model_summaries[[paste0(loc, "_pre")]]
    tagList(verbatimTextOutput(paste0("summ_pre_", loc)))
  })
  observe({
    loc <- input$sel_loc_stats
    if(!is.null(rv$model_summaries[[paste0(loc, "_act")]])) {
      output[[paste0("summ_act_", loc)]] <- renderPrint({ rv$model_summaries[[paste0(loc, "_act")]] })
    }
    if(!is.null(rv$model_summaries[[paste0(loc, "_pre")]])) {
      output[[paste0("summ_pre_", loc)]] <- renderPrint({ rv$model_summaries[[paste0(loc, "_pre")]] })
    }
  })

  # RFK Importance
  output$rf_importance_plot_act <- renderPlot({
    loc <- input$sel_loc_stats; req(rv$rf_models[[paste0(loc, "_act")]])
    randomForest::varImpPlot(rv$rf_models[[paste0(loc, "_act")]], main = paste("Variable Importance (Actual):", loc))
  })
  output$rf_importance_plot_pre <- renderPlot({
    loc <- input$sel_loc_stats; req(rv$rf_models[[paste0(loc, "_pre")]])
    randomForest::varImpPlot(rv$rf_models[[paste0(loc, "_pre")]], main = paste("Variable Importance (Predicted):", loc))
  })
  
  # RK/RFK Internal Variograms
  output$rk_internal_vgm_act <- renderPlot({
    loc <- input$sel_loc_stats; req(rv$v_emp_list[[paste0(loc, "_act")]], rv$v_fit_list[[paste0(loc, "_act")]])
    plot(rv$v_emp_list[[paste0(loc, "_act")]], rv$v_fit_list[[paste0(loc, "_act")]], main = paste("Internal Residual Variogram (Actual):", loc))
  })
  output$rk_internal_vgm_pre <- renderPlot({
    loc <- input$sel_loc_stats; req(rv$v_emp_list[[paste0(loc, "_pre")]], rv$v_fit_list[[paste0(loc, "_pre")]])
    plot(rv$v_emp_list[[paste0(loc, "_pre")]], rv$v_fit_list[[paste0(loc, "_pre")]], main = paste("Internal Residual Variogram (Predicted):", loc))
  })
  
  output$rfk_internal_vgm_act <- renderPlot({
    loc <- input$sel_loc_stats; req(rv$v_emp_list[[paste0(loc, "_act")]], rv$v_fit_list[[paste0(loc, "_act")]])
    plot(rv$v_emp_list[[paste0(loc, "_act")]], rv$v_fit_list[[paste0(loc, "_act")]], main = paste("Internal Residual Variogram (Actual):", loc))
  })
  output$rfk_internal_vgm_pre <- renderPlot({
    loc <- input$sel_loc_stats; req(rv$v_emp_list[[paste0(loc, "_pre")]], rv$v_fit_list[[paste0(loc, "_pre")]])
    plot(rv$v_emp_list[[paste0(loc, "_pre")]], rv$v_fit_list[[paste0(loc, "_pre")]], main = paste("Internal Residual Variogram (Predicted):", loc))
  })

  # CK Variograms
  output$ck_variogram_plot_act <- renderPlot({
    loc <- input$sel_loc_stats; req(rv$gstat_objs[[paste0(loc, "_act")]])
    g <- rv$gstat_objs[[paste0(loc, "_act")]]
    vm <- variogram(g)
    plot(vm, model = g$model, main = paste("Cross-Variogram (Actual):", loc))
  })
  output$ck_variogram_plot_pre <- renderPlot({
    loc <- input$sel_loc_stats; req(rv$gstat_objs[[paste0(loc, "_pre")]])
    g <- rv$gstat_objs[[paste0(loc, "_pre")]]
    vm <- variogram(g)
    plot(vm, model = g$model, main = paste("Cross-Variogram (Predicted):", loc))
  })

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
    if(loc == "Total (Combined)") return(NULL)
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

  output$obs_pred_plot_act <- renderPlot({
    req(input$sel_loc_stats, rv$cv_data_act)
    loc <- input$sel_loc_stats
    if(loc == "Total (Combined)") {
       df_list <- rv$cv_data_act
       df <- do.call(rbind, lapply(df_list, function(x) if(inherits(x, "sf")) st_drop_geometry(x) else as.data.frame(x)))
    } else {
       df <- rv$cv_data_act[[loc]]
       if(inherits(df, "sf")) df <- st_drop_geometry(df)
    }
    req(df, nrow(df) > 0)
    
    obs_col <- grep("\\.observed$|^observed$", colnames(df), value = TRUE)[1]
    pre_col <- grep("\\.pred$|^var1\\.pred$", colnames(df), value = TRUE)[1]
    req(obs_col, pre_col)
    
    obs <- df[[obs_col]]; pre <- df[[pre_col]]
    
    ggplot(data.frame(Observed = obs, Predicted = pre), aes(x = Observed, y = Predicted)) +
      geom_point(alpha = 0.6) +
      geom_abline(intercept = 0, slope = 1, color = "red", linetype = "dashed") +
      geom_smooth(method = "lm", color = "blue", se = FALSE) +
      labs(title = paste("Observed vs Predicted:", loc), subtitle = "Red: 1:1 Line, Blue: Regression") +
      theme_minimal()
  })

  output$resid_vgm_plot_act <- renderPlot({
    req(input$sel_loc_stats, rv$cv_data_act)
    loc <- input$sel_loc_stats
    
    if(loc == "Total (Combined)") {
       df_list <- rv$cv_data_act
       # Safely combine sf objects
       sf_list <- lapply(df_list, function(x) {
         if(inherits(x, "sf")) return(x)
         if(is.data.frame(x) && "x" %in% colnames(x) && "y" %in% colnames(x)) return(st_as_sf(x, coords = c("x", "y"), crs = rv$mapping$crs))
         return(NULL)
       })
       sf_list <- sf_list[!sapply(sf_list, is.null)]
       req(length(sf_list) > 0)
       
       # Use base R rbind if do.call fails on sf, though dplyr::bind_rows is safer if available
       cv_obj <- tryCatch(do.call(rbind, sf_list), error = function(e) sf_list[[1]])
    } else {
       cv_obj <- rv$cv_data_act[[loc]]
    }
    
    req(cv_obj)
    
    if(!inherits(cv_obj, "sf") && !inherits(cv_obj, "Spatial")) {
       if("x" %in% colnames(cv_obj) && "y" %in% colnames(cv_obj)) {
          cv_obj <- st_as_sf(cv_obj, coords = c("x", "y"), crs = rv$mapping$crs)
       } else {
          return(NULL) 
       }
    }
    
    if(!("residual" %in% colnames(cv_obj))) {
       obs_col <- grep("\\.observed$|^observed$", colnames(cv_obj), value = TRUE)[1]
       pre_col <- grep("\\.pred$|^var1\\.pred$", colnames(cv_obj), value = TRUE)[1]
       req(obs_col, pre_col)
       cv_obj$residual <- cv_obj[[obs_col]] - cv_obj[[pre_col]]
    }
    
    tryCatch({
       lags <- calc_scientific_lags(cv_obj)
       v_res <- variogram(residual ~ 1, cv_obj, width = lags$width, cutoff = lags$cutoff)
       plot(v_res, main = paste("Residual Variogram:", loc), sub = "Target: Pure Nugget (No structure)")
    }, error = function(e) {
       plot(1, 1, type="n", main=paste("Error:", e$message), axes=F)
    })
  })

  output$obs_pred_plot_pre <- renderPlot({
    req(input$sel_loc_stats, rv$cv_data_pre)
    loc <- input$sel_loc_stats
    if(loc == "Total (Combined)") {
       df_list <- rv$cv_data_pre
       df <- do.call(rbind, lapply(df_list, function(x) if(inherits(x, "sf")) st_drop_geometry(x) else as.data.frame(x)))
    } else {
       df <- rv$cv_data_pre[[loc]]
       if(inherits(df, "sf")) df <- st_drop_geometry(df)
    }
    req(df, nrow(df) > 0)
    
    obs_col <- grep("\\.observed$|^observed$", colnames(df), value = TRUE)[1]
    pre_col <- grep("\\.pred$|^var1\\.pred$", colnames(df), value = TRUE)[1]
    req(obs_col, pre_col)
    
    obs <- df[[obs_col]]; pre <- df[[pre_col]]
    
    ggplot(data.frame(Observed = obs, Predicted = pre), aes(x = Observed, y = Predicted)) +
      geom_point(alpha = 0.6) +
      geom_abline(intercept = 0, slope = 1, color = "red", linetype = "dashed") +
      geom_smooth(method = "lm", color = "blue", se = FALSE) +
      labs(title = paste("Observed vs Predicted (Predicted Map):", loc), subtitle = "Red: 1:1 Line, Blue: Regression") +
      theme_minimal()
  })

  output$resid_vgm_plot_pre <- renderPlot({
    req(input$sel_loc_stats, rv$cv_data_pre)
    loc <- input$sel_loc_stats
    
    if(loc == "Total (Combined)") {
       df_list <- rv$cv_data_pre
       sf_list <- lapply(df_list, function(x) {
         if(inherits(x, "sf")) return(x)
         if(is.data.frame(x) && "x" %in% colnames(x) && "y" %in% colnames(x)) return(st_as_sf(x, coords = c("x", "y"), crs = rv$mapping$crs))
         return(NULL)
       })
       sf_list <- sf_list[!sapply(sf_list, is.null)]
       req(length(sf_list) > 0)
       cv_obj <- tryCatch(do.call(rbind, sf_list), error = function(e) sf_list[[1]])
    } else {
       cv_obj <- rv$cv_data_pre[[loc]]
    }
    
    req(cv_obj)
    
    if(!inherits(cv_obj, "sf") && !inherits(cv_obj, "Spatial")) {
       if("x" %in% colnames(cv_obj) && "y" %in% colnames(cv_obj)) {
          cv_obj <- st_as_sf(cv_obj, coords = c("x", "y"), crs = rv$mapping$crs)
       } else {
          return(NULL) 
       }
    }
    
    if(!("residual" %in% colnames(cv_obj))) {
       obs_col <- grep("\\.observed$|^observed$", colnames(cv_obj), value = TRUE)[1]
       pre_col <- grep("\\.pred$|^var1\\.pred$", colnames(cv_obj), value = TRUE)[1]
       req(obs_col, pre_col)
       cv_obj$residual <- cv_obj[[obs_col]] - cv_obj[[pre_col]]
    }
    
    tryCatch({
       lags <- calc_scientific_lags(cv_obj)
       v_res <- variogram(residual ~ 1, cv_obj, width = lags$width, cutoff = lags$cutoff)
       plot(v_res, main = paste("Residual Variogram (Predicted Map):", loc), sub = "Target: Pure Nugget (No structure)")
    }, error = function(e) {
       plot(1, 1, type="n", main=paste("Error:", e$message), axes=F)
    })
  })

  output$tps_gcv_plot_pre <- renderPlot({
    loc <- input$sel_loc_stats; req(loc, input$method == "TPS")
    if(loc == "Total (Combined)") return(NULL)
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
    v_act <- df[[meta$actual]]
    v_pre <- if(!is.null(meta$pred)) df[[meta$pred]] else if(!is.null(meta$pred_ss)) df[[meta$pred_ss]] else NULL
    
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
    v_act <- df[[meta$actual]]
    v_pre <- if(!is.null(meta$pred)) df[[meta$pred]] else if(!is.null(meta$pred_ss)) df[[meta$pred_ss]] else NULL
    
    s_a <- summary(v_act)
    res <- data.frame(Metric = names(s_a), Selected_Actual = as.character(round(as.numeric(s_a), 3)))
    
    if(!is.null(v_pre)) {
      s_p <- summary(v_pre)
      res$Selected_Predicted <- as.character(round(as.numeric(s_p), 3))
    }
    res
  })

  calc_area_df <- function(r_obj) {
    if(is.null(r_obj)) return(NULL)
    params <- tryCatch(agro_params(), error = function(e) NULL)
    if(is.null(params)) return(data.frame(Status = "Awaiting Agro Params"))
    
    tryCatch({
      # Use matrix classification for 100% predictability
      # Fix Area Coverage Bug: Subset r_obj to the first layer (prediction) to prevent double-counting multi-layer rasters (like Kriging variance)
      r_class <- classify(r_obj[[1]], params$rcl_mat, right = FALSE)
      freq_df <- as.data.frame(freq(r_class))
      
      full_res <- data.frame(value = as.numeric(1:params$n_c), Class = params$labels)
      
      if(!"value" %in% names(freq_df)) {
        return(data.frame(Class = params$labels, Ha = 0))
      }
      
      # Clean freq_df: ensure numeric value and remove NAs (background)
      freq_df$value <- as.numeric(as.character(freq_df$value))
      freq_df <- freq_df[!is.na(freq_df$value), ]
      
      # Aggregate count by value to avoid duplicates if freq() returns multiple layers
      freq_df <- freq_df %>%
        group_by(value) %>%
        summarise(count = sum(count, na.rm = TRUE), .groups = "drop")

      # Use left_join to strictly keep only defined classes
      res_df <- full_res %>%
        left_join(freq_df, by = "value") %>%
        mutate(count = ifelse(is.na(count), 0, count)) %>%
        mutate(Ha = round((count * prod(res(r_obj))) / 10000, 2)) %>%
        select(Class, Ha)
      
      return(res_df)
    }, error = function(e) {
      return(data.frame(Error = as.character(e$message)))
    })
  }
  output$area_table_total_act <- renderTable({ req(rv$rast, input$color_style == "agro"); calc_area_df(terra::unwrap(rv$rast)) })
  output$area_table_total_pre <- renderTable({ req(rv$rast_pred, input$color_style == "agro"); calc_area_df(terra::unwrap(rv$rast_pred)) })
  
  output$area_table_loc_act <- renderTable({
    req(rv$rast_list_act, input$color_style == "agro"); loc <- input$sel_loc_stats
    if(loc == "Total (Combined)") return(NULL) else calc_area_df(terra::unwrap(rv$rast_list_act[[loc]]))
  })
  output$area_table_loc_pre <- renderTable({
    req(rv$rast_list_pre, input$color_style == "agro"); loc <- input$sel_loc_stats
    if(loc == "Total (Combined)") return(NULL) else calc_area_df(terra::unwrap(rv$rast_list_pre[[loc]]))
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
                    Moran_I = round(moran_i, 4)
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
          }        })
      
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
      params <- tryCatch(agro_params(), error = function(e) NULL)
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
    
        p <- p + theme_minimal(base_size = input$exp_font_base) +
            theme(plot.title = element_text(size = input$exp_title_size, face = "bold"),
                  plot.subtitle = element_text(size = input$exp_font_base, face = "italic"),
                  axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1),
                  legend.text = element_text(angle = 90, hjust = 0.5),
                  legend.title = element_text(angle = 90, hjust = 0.5),
                  plot.margin = ggplot2::margin(40, 10, 10, 10)) +
            labs(title = title, subtitle = subtitle)
    
        if(input$exp_scale) p <- p + coord_sf(clip = "off") + annotation_scale(location = "bl", width_hint = 0.4, pad_y = unit(-2.5, "cm")) + theme(plot.margin = ggplot2::margin(10, 10, 70, 10))
        p  }

  get_export_plot <- function() {
    meta <- get_current_meta()
    if(is.null(meta)) return(ggplot() + annotate("text", x=0.5, y=0.5, label="Please select a variable.") + theme_void())
    
        current_method <- rv$run_method[[input$var_id]]
        method_lab <- if(!is.null(current_method)) {
          m_name <- switch(current_method,
            "OK"  = "Ordinary Kriging",
            "RK"  = "Regression Kriging",
            "RFK" = "Random Forest Kriging",
            "CK"  = "Co-Kriging",
            "IDW" = "IDW",
            "TPS" = "Thin Plate Spline",
            current_method)
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
  output$export_preview <- renderPlot({ get_export_plot() })
  output$dl_map <- downloadHandler(
    filename = function() { paste0("Map_", format(Sys.time(), "%H%M%S"), ".", tolower(input$exp_format %||% "png")) },
    content = function(file) {
      p <- get_export_plot()
      
      ext <- tolower(input$exp_format %||% "png")
      if (inherits(p, "trellis")) {
        if (ext == "png") png(file, width = 10, height = 8, units = "in", res = 300)
        else if (ext == "tiff") tiff(file, width = 10, height = 8, units = "in", res = 300)
        else if (ext == "pdf") pdf(file, width = 10, height = 8)
        else jpeg(file, width = 10, height = 8, units = "in", res = 300)
        print(p)
        dev.off()
      } else {
        ggsave(file, plot = p, device = if(ext == "pdf") "pdf" else (if(ext == "tiff") "tiff" else NULL), width = 10, height = 8, dpi = 300)
      }
    }
  )
}
if (Sys.getenv("SHINY_PORT") != "" || interactive()) {
  shinyApp(ui, server)
}
