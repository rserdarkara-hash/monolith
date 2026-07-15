
source("global.R")
source("spatial_helpers.R")

estimate_run_duration <- function(loc_sample_counts, method, comp_mode, cores) {
  # History aware run duration estimator
  history_dir <- "run_history"
  history_file <- file.path(history_dir, "run_history.csv")
  
  # Base multipliers for different methods. Note: RFK, RK, and CK are calibrated 
  # from real measurements; OK/IDW/TPS multipliers are still unverified guesses.
  method_mult <- switch(method,
    "RFK" = 1.0,
    "RK"  = 1.0,
    "CK"  = 1.3,
    "OK"  = 0.5,
    "IDW" = 0.5,
    "TPS" = 0.3,
    1.0
  )
  
  # Ensure valid sample counts
  loc_sample_counts[is.na(loc_sample_counts) | loc_sample_counts == 0] <- 50
  
  n_locs <- length(loc_sample_counts)
  n_models <- n_locs * (if(comp_mode) 2 else 1)
  
  loc_times_sec <- numeric(n_locs)
  history_data <- NULL
  
  if (file.exists(history_file)) {
    tryCatch({
      history_data <- read.csv(history_file)
      history_data <- history_data[history_data$method == method, ]
      
      # Try filtering by comp_mode if enough data
      comp_history <- history_data[history_data$comp_mode == comp_mode, ]
      if (nrow(comp_history) >= 5) {
        history_data <- comp_history
      }
      
      # Try filtering by cores if enough data
      cores_history <- history_data[history_data$cores_used == cores, ]
      if (nrow(cores_history) >= 5) {
        history_data <- cores_history
      }
    }, error = function(e) { history_data <- NULL })
  }
  
  is_history_based <- !is.null(history_data) && nrow(history_data) >= 5
  
  if (is_history_based) {
    fit <- tryCatch(lm(per_locality_share_sec ~ n_samples, data = history_data), error = function(e) NULL)
    if (!is.null(fit)) {
      preds <- predict(fit, newdata = data.frame(n_samples = loc_sample_counts))
      loc_times_sec <- pmax(5, preds)
    } else {
      is_history_based <- FALSE
    }
  }
  
  if (!is_history_based) {
    # Cold-start formula based on real data points (79->150s, 355->510s for 2 models)
    # Scaled down to 70% per user request
    base_sec <- pmax(5.25, 16.45 + 0.455 * loc_sample_counts)
    model_time <- base_sec * method_mult
    loc_times_sec <- model_time * (if (comp_mode) 2 else 1)
  }
  
  max_single_loc_time <- max(loc_times_sec)
  
  # Distributed efficiency fix (0.75 effective cores)
  eff_factor <- 0.75
  distributed_time <- sum(loc_times_sec) / max(1, (cores * eff_factor))
  
  # Apply fudge factor (more uncertainty for cold start)
  fudge_mult <- if (is_history_based) 1.25 else 1.4
  est_time_sec <- max(max_single_loc_time, distributed_time) * fudge_mult
  
  est_time_str <- if (est_time_sec < 60) {
    paste(round(est_time_sec), "seconds")
  } else {
    paste(round(est_time_sec / 60, 1), "minutes")
  }
  
  estimate_text <- paste0("~", n_models, " locality model(s), ~", est_time_str, " estimated.\n\n* Note: This time estimate is calibrated based on a below-average hardware benchmark. Actual execution time on modern or cloud hardware will likely be significantly faster.")
  is_long_run <- est_time_sec >= 120 || method %in% c("RK", "RFK", "CK")
  
  return(list(
    est_time_sec = est_time_sec,
    est_time_str = est_time_str,
    estimate_text = estimate_text,
    is_long_run = is_long_run,
    n_models = n_models
  ))
}


validate_crs <- function(crs_selection, error_prefix = "Invalid CRS provided", duration = NULL) {
  tryCatch({
    c_obj <- sf::st_crs(crs_selection)
    if (is.na(c_obj)) stop("Invalid CRS format.")
    
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

dashboard_palettes <- c("viridis", "Greens", "Blues", "Oranges", "YlOrRd", "RdYlBu", "BrBG", "YlOrBr", "Greys", "Spectral")

palette_choices_precomputed <- (function() {
  pals <- dashboard_palettes
  labels <- sapply(pals, function(p) {
    cols <- if (p == "viridis") {
      viridis::viridis(5)
    } else {
      info <- RColorBrewer::brewer.pal.info
      max_cols <- if (p %in% rownames(info)) info[p, "maxcolors"] else 5
      n_cols <- max(3, min(5, max_cols))
      RColorBrewer::brewer.pal(n_cols, p)
    }
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
  resid_palette = list(fn = updateSelectInput, name = "styler_resid_palette", val_param = "selected"),
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


robust_vgm_fit <- function(v_emp, v_data) {
  initial_sill <- var(v_data, na.rm=TRUE)
  if (is.na(initial_sill) || initial_sill == 0) initial_sill <- 1

  max_dist <- if (!is.null(v_emp) && nrow(v_emp) > 0) max(v_emp$dist, na.rm = TRUE) else 1.0
  if (is.na(max_dist) || is.infinite(max_dist) || max_dist <= 0) {
    max_dist <- 1.0 # Safe default positive distance fallback
  }

  vgm_diag <- function(n_tried, n_flawed, flawed_winner) {
    list(n_tried = n_tried, n_flawed = n_flawed, flawed_winner = flawed_winner)
  }

  if (is.null(v_emp) || nrow(v_emp) < 5) {
    # Skip fitting to prevent gstat::fit.variogram from crashing R on very small empirical variograms
    fallback <- gstat::vgm(psill = initial_sill * 0.8, "Sph", range = max_dist/2, nugget = initial_sill * 0.2)
    attr(fallback, "is_fallback") <- TRUE
    attr(fallback, "vgm_diagnostics") <- vgm_diag(0L, 0L, FALSE)
    return(fallback)
  }

  initial_nugget <- min(v_emp$gamma)
  if (initial_nugget == 0) initial_nugget <- max(initial_sill * 1e-6, 1e-6)

  if (initial_nugget > initial_sill) initial_nugget <- initial_sill * 0.9
  initial_psill <- max(initial_sill - initial_nugget, initial_sill * 0.1)

  ranges <- c(max_dist / 10, max_dist / 5, max_dist / 4, max_dist / 2)
  models <- c("Sph", "Exp", "Gau", "Mat") # Added Matern

  # gstat reports singular fits via attr(, "singular") but non-convergence
  # only as a C-level warning, so the warning itself is the detection signal.
  # These are expected while screening candidates and are muffled; anything
  # unrecognized still propagates.
  screening_warning <- "No convergence after|singular model|singular covariance"

  candidates <- list()
  for (m in models) {
    for (r in ranges) {
      start_kappa <- if (m == "Mat") 1.5 else 0.5
      flawed <- FALSE
      f <- tryCatch({
        withCallingHandlers(
          gstat::fit.variogram(v_emp, gstat::vgm(psill = initial_psill, model = m, range = r, nugget = initial_nugget, kappa = start_kappa)),
          warning = function(w) {
            if (grepl(screening_warning, conditionMessage(w))) {
              flawed <<- TRUE
              invokeRestart("muffleWarning")
            }
          }
        )
      }, error = function(e) NULL)
      if (is.null(f)) next
      flawed <- flawed || isTRUE(attr(f, "singular"))
      sse <- attr(f, "SSErr")
      in_window <- !is.null(sse) && !is.na(sse) && f$range[2] > (max_dist/100) && f$range[2] < max_dist * 2 && f$psill[2] > 0
      candidates[[length(candidates) + 1]] <- list(fit = f, sse = sse, flawed = flawed, in_window = in_window)
    }
  }

  n_tried <- length(candidates)
  n_flawed <- sum(vapply(candidates, function(x) x$flawed, logical(1)))
  eligible <- Filter(function(x) x$in_window, candidates)
  clean_pool <- Filter(function(x) !x$flawed, eligible)
  flawed_pool <- Filter(function(x) x$flawed, eligible)

  pick_best <- function(pool) pool[[which.min(vapply(pool, function(x) x$sse, numeric(1)))]]$fit

  best_fit <- NULL
  flawed_winner <- FALSE
  if (length(clean_pool) > 0) {
    best_fit <- pick_best(clean_pool)
  } else if (length(flawed_pool) > 0) {
    # No clean candidate anywhere: still better than the heuristic fallback, but flagged.
    best_fit <- pick_best(flawed_pool)
    flawed_winner <- TRUE
    attr(best_fit, "flawed_winner") <- TRUE
  }

  if (is.null(best_fit)) {
    if (initial_nugget > initial_sill * 0.8) {
      best_fit <- gstat::vgm(psill = initial_sill * 0.05, "Sph", range = max_dist/10, nugget = initial_sill * 0.95)
    } else {
      best_fit <- gstat::vgm(psill = initial_sill * 0.8, "Sph", range = max_dist/2, nugget = initial_sill * 0.2)
    }
    attr(best_fit, "is_fallback") <- TRUE
  }
  attr(best_fit, "vgm_diagnostics") <- vgm_diag(n_tried, n_flawed, flawed_winner)
  return(best_fit)
}


ui <- fluidPage(
  useShinyjs(),
  render_docs_drawer(),
  
  fresh::use_theme(app_themes[["Muted Sage (modified)"]]$theme),
  tags$head(
    tags$style(HTML(app_themes[["Muted Sage (modified)"]]$manual_style))
  ),
  
  uiOutput("dynamic_theme"),
  tags$head(
    tags$style(HTML("
      .bootstrap-select .dropdown-menu li a span.text { display: flex !important; width: 100% !important; align-items: center; justify-content: space-between; }
      .shiny-notification { width: 100% !important; }
      .well { padding: 15px; }
      .header-panel { background-color: #2c3e50; color: white; padding: 10px 20px; margin-bottom: 20px; border-radius: 0 0 10px 10px; display: flex; justify-content: space-between; align-items: center; }
      .header-title { margin: 0; font-weight: bold; font-size: 24px; }
      .header-controls { display: flex; align-items: center; gap: 20px; }
      .table-container { width: 100%; overflow-x: auto; font-size: 0.95em; margin-bottom: 10px; }
      .table-container table { width: 100% !important; margin-bottom: 0; background-color: #ffffff !important; color: #000000 !important; }
      .table-container th { background-color: #f8f9fa !important; color: #000000 !important; }
      .table-container .dataTables_wrapper { background-color: #ffffff !important; border-radius: 4px; padding: 2px; }
      .table-container table.dataTable td, .table-container table.dataTable th { color: #000000 !important; }
      .popover { color: #333 !important; background-color: #fff !important; max-width: 400px; }
      .popover-header { color: #333 !important; background-color: #f8f9fa !important; border-bottom: 1px solid #ebebeb; }
      .popover-body { color: #333 !important; }
      .expand-icon-btn { position: absolute; top: 10px; right: 10px; z-index: 100; opacity: 0.8; width: 32px; height: 32px; padding: 0 !important; display: inline-flex !important; align-items: center !important; justify-content: center !important; }
      .expand-icon-btn > * { margin: 0 !important; padding: 0 !important; }
      .map-toolbar-export-container .form-group { margin-bottom: 0 !important; }
    ")),
    uiOutput("dynamic_manual_style"),
    tags$script(HTML("$(function () { $('[data-toggle=\"popover\"]').popover({html: true}); });")),
    # DT tables in this app pre-render while their tab is hidden
    # (suspendWhenHidden = FALSE); with scrollX the cloned header is then
    # sized against a zero-width container, so realign columns on tab reveal.
    tags$script(HTML("$(document).on('shown.bs.tab', 'a[data-toggle=\"tab\"]', function () { setTimeout(function () { if ($.fn.dataTable) { $.fn.dataTable.tables({ visible: true, api: true }).columns.adjust(); } }, 60); });"))
  ),
  
  div(class = "header-panel", style = "display: flex; justify-content: space-between; align-items: center; padding: 5px 20px;",
      img(src = "assets/banner.png", class = "header-banner", style = "max-height: 50px; width: auto; object-fit: contain; float: left;"),
      div(style = "flex-grow: 1;"),
      div(class = "header-controls", style = "display: flex; align-items: center; gap: 10px; margin-left: auto;",
          tags$style(HTML("
            .header-controls .shiny-input-container { width: auto !important; margin: 0 !important; }
            .header-controls .form-group { margin-bottom: 0 !important; margin-right: 0 !important; }
            .header-controls .checkbox { margin: 0 !important; padding: 0 !important; display: flex !important; align-items: center !important; }
            .header-controls .checkbox label { margin: 0 !important; padding-left: 0 !important; color: white !important; font-size: 11px !important; display: flex !important; align-items: center !important; gap: 5px !important; line-height: 1 !important; }
            .header-controls .checkbox input[type=\"checkbox\"] { position: static !important; margin: 0 !important; }
            
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
          selectInput("var_category", "Variable Category", choices = NULL),
          selectInput("var_id", "Variable", choices = NULL),
          selectInput("value_type", HTML(paste0("Primary View", info_tooltip("primary_view_info", "<b>Actual Values (observed):</b> Maps the raw observed/measured ground-truth data points directly without any machine learning predictions.<br><br><span style='border-top: 1px solid #ddd; display: block; margin: 8px 0;'></span><b>Machine Learning Predictions:</b> Use these options if you want to map predicted parameters from your machine learning models:<br><br>• <b>Best ML Predictions (_cve):</b> Maps predicted values from the cross-validation ensemble (CVE), which represent the best overall ML predictions.<br><br>• <b>Single Split ML Predictions (_ss):</b> Maps predicted values from a single train/test split partition.<br><br>• <b>Residuals (v - pv) of ML Predictions:</b> Maps ML model residuals (observed Actual value minus the ML Predicted value uploaded in your dataset) to study local spatial error patterns. These are NOT errors of the interpolation itself."))), choices = c("Actual Values" = "actual", "Best ML Predictions (_cve)" = "pred", "Single Split ML Predictions (_ss)" = "pred_ss", "Residuals (v - pv) of ML Predictions" = "resid")),
                     conditionalPanel(
                       condition = "input.value_type == 'pred_ss'",
                       selectInput("subset", HTML(paste0("Data Subset", info_tooltip("data_subset_info", "Restricts the Single Split (_ss) view to one data partition (e.g. Train/Test/Validation), read from a 'subset' column in the uploaded data. Available choices are detected when a dataset containing such a column is loaded."))), choices = c("All" = "all"), selected = "all")
                     ),
                     conditionalPanel(
                       condition = "['pred', 'pred_ss', 'resid'].includes(input.value_type)",
                       checkboxInput("comp_mode", HTML(paste0("Comparison Mode", info_tooltip("comp_mode", "Splits the viewer to compare the Actual (observed) map against the map of your uploaded ML predictions. Useful for visual validation."))), FALSE)
                     ),          conditionalPanel(condition = "input.comp_mode && ['pred', 'pred_ss'].includes(input.value_type)", 
                           checkboxInput("sep_fit", HTML(paste0("Fit Actual/Predicted Separately", info_tooltip("sep_fit_info", "If checked, optimizes variograms separately for actual and predicted data. If unchecked, applies actual variogram to predictions."))), TRUE),
                           checkboxInput("match_scales", HTML(paste0("Match Scales", info_tooltip("match_info", "Forces the map legends for Actual and Predicted data to use the same color range."))), FALSE))
      ),
      conditionalPanel(
        condition = "input.main_tabs !== 'Descriptive and Exploratory Suite'",
        br(),
        div(style="background-color: #e7f5ff; padding: 10px; border: 1px solid #a5d8ff;",
            h4("2. Spatial Engine"),
            selectInput("method", HTML(paste0("Interpolation", info_tooltip("method_info", "Cross-validation strategy is selectable below. It governs the reported Model Performance metrics only, never the prediction surface. Folds use a fixed seed (12345) for reproducibility. See Scientific Guide Section 9 for details."))),
                        choices = c("Ordinary Kriging" = "OK",
                                    "Regression Kriging" = "RK",
                                    "Random Forest Kriging" = "RFK",
                                    "Co-Kriging" = "CK",
                                    "IDW" = "IDW",
                                    "Thin Plate Spline (TPS)" = "TPS")),
            conditionalPanel(condition = "input.method == 'CK'",
              helpText(HTML("<em style='color: #ffffff; font-size: 0.9em;'>Note: CK uses nmax=15 by default to ensure optimal speed. See Scientific Guide to change it.</em>"))
            ),
            radioButtons("cv_strategy",
              HTML(paste0("Cross-Validation Strategy", info_tooltip("cv_strategy_info", "How held-out folds are formed for the reported performance metrics; it does NOT change the interpolated map. Auto (Default): LOOCV for n ≤ 50, seeded random 10-fold above. Standard LOOCV: full leave-one-out, the most rigorous, but noticeably slow beyond ~2000 samples (especially RK/RFK, which refit the variogram every fold). Spatial Block CV: 10 spatially-clustered (k-means) folds that hold out contiguous regions to curb the optimistic bias random folds suffer under spatial autocorrelation; recommended for DSM-style validation. Below n=30 it degrades to LOOCV."))),
              choices = c("Auto (Default)" = "auto", "Standard LOOCV" = "loocv", "Spatial Block CV" = "block"),
              selected = "auto"),

            conditionalPanel(condition = "input.method == 'RFK'",
              radioButtons("rfk_uncertainty",
                HTML(paste0("RFK Uncertainty Method", info_tooltip("rfk_unc_info", "Controls ONLY the RFK uncertainty (variance) map, never the prediction surface, and never the reported metrics. Ensemble spread (default, fast): the between-tree variance of the forest; a stability heuristic that understates true predictive uncertainty. Infinitesimal Jackknife (calibrated, Wager et al. 2014): the random-forest analogue of the regression standard error, a better-calibrated variance of the ensemble mean, slightly slower to compute. See Scientific Guide Section 7.3."))),
                choices = c("Infinitesimal Jackknife (calibrated)" = "jackknife", "Ensemble spread (fast)" = "spread"),
                selected = "jackknife")
            ),

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
             
                       conditionalPanel(condition = "input.value_type == 'resid'",
                         div(style = "background-color: #fff5f5; padding: 10px; border: 1px solid #ffc9c9; border-radius: 4px; margin-bottom: 10px;",
                           div(style = "display: flex; align-items: center;",
                             h5("Residual Diagnostics", style = "margin-top: 0; margin-bottom: 0;"),
                             actionLink("resid_info_btn", label = NULL, icon = icon("info-circle"), style = "color: #17a2b8; margin-left: 5px;")
                           ),
                           tags$p(style="font-size: 0.85em; margin: 5px 0;", tags$em("Residuals = observed values minus the ML-predicted values uploaded in your dataset. They diagnose your external ML model, not the interpolation itself.")),
                           tags$p(style="font-size: 0.85em; margin-bottom: 5px;", tags$b("Interpolated Delta:"), " Difference between two full surfaces (actual - prediction). Reveals regional zones of consistent over/under-prediction."),
                           tags$p(style="font-size: 0.85em; margin-bottom: 5px;", tags$b("Point Errors:"), " Local prediction errors [Observed - Predicted] shown at the exact sample locations, highlighting individual points of model failure."),
                           tags$p(style="font-size: 0.85em; margin-bottom: 0;", tags$b("Interpolated Point Errors:"), " IDW surface of those local errors (Export Panel only). Acts as an 'Uncertainty Map' of the spatial structure of model failure.")
                         )
                       ),          
            conditionalPanel(condition = "['OK', 'RK', 'RFK', 'CK'].includes(input.method)",
              radioButtons("vgm_mode", HTML(paste0("Fitting Mode", info_tooltip("vgm_mode_info", "Optional convenience. Click OPTIMIZE ALL VARIOGRAMS to pre-compute and inspect the auto-fitted variogram curves, then (if you wish) switch to Manual to hand-tune the already-fitted Nugget / Partial Sill / Range. If you don't need manual tuning you can skip the button entirely: Run Analysis performs the identical auto-fit internally, so pressing it first does not change the map or metrics; it only lets you preview the fit and avoids a redundant wait."))), choices = c("Auto-Fit" = "auto", "Manual" = "manual"), inline = TRUE),
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
            
            conditionalPanel(condition = "input.method == 'TPS'",
                tuning_ui(
                    id = "tps", label = "TPS LAMBDA",
                    global_slider_id = "tps_lambda", manual_slider_id = "tps_m_lambda",
                    global_slider_args = list(label = "Global Smoothing (Lambda)", min = -1, max = 1, value = -1, step = 0.001),
                    manual_slider_args = list(label = "Lambda", min = -1, max = 1, value = -1, step = 0.001),
                    optimize_btn_label = "OPTIMIZE TPS LAMBDA",
                    manual_btn_label = "Apply Manual Lambda",
                    outer_style = "background-color: #fff4e6; padding: 10px; border: 1px solid #ffd8a8; border-radius: 4px; margin-bottom: 10px;",
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
            # Keyed to the method of the DISPLAYED run (disp_method): this
            # toggles a view of the map on screen, so picking a non-kriging
            # method for the next run must not remove it (and vice versa).
            conditionalPanel(condition = "['OK', 'RK', 'RFK', 'CK'].includes(output.disp_method)",
              checkboxInput("show_uncertainty", "Map Uncertainty Instead of Interpolation", FALSE),
              conditionalPanel(condition = "input.show_uncertainty",
                radioButtons("uncertainty_type", "Metric", choices = c("Variance" = "var", "Standard Error" = "se"), selected = "se", inline = TRUE),
                p(style="font-size: 0.8em; opacity: 0.8; margin-bottom: 0;", "Variance is in squared units of the variable; SE shares the variable's unit. Uncertainty layers always use a continuous palette; Agronomic/Binned class breaks apply to concentration maps only.")
              )
            ),
            conditionalPanel(condition = "!['OK', 'RK', 'RFK', 'CK'].includes(output.disp_method)",
              p(style="font-size: 0.8em; opacity: 0.8;", "Uncertainty mapping becomes available once a Kriging-based map has been generated.")
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
      
      conditionalPanel(
        condition = "input.main_tabs === 'Descriptive and Exploratory Suite'",
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
                   uiOutput("locality_selector_ui"),
                   fluidRow(
                     column(8,
                            conditionalPanel(condition = "output.disp_method == 'OK'",
                              h4("Actual Data Structure"), plotOutput("vgm_plot_main", height = "350px"),
                              div(id = "predicted_data_structure_ui",
                                h4("Predicted Data Structure"), plotOutput("vgm_plot_pred", height = "350px")
                              )
                            ),
                            conditionalPanel(condition = "output.disp_method == 'RK'",
                               h4("Linear Trend Performance (Actual)"), uiOutput("model_summary_ui_act"),
                               div(id = "rk_pred_ui", h4("Linear Trend Performance (Predicted)"), uiOutput("model_summary_ui_pre")),
                               hr(),
                               h4("Internal Residual Variogram (Actual)"), plotOutput("rk_internal_vgm_act", height = "350px"),
                               div(id = "rk_internal_vgm_pre_ui", h4("Internal Residual Variogram (Predicted)"), plotOutput("rk_internal_vgm_pre", height = "350px"))
                             ),
                            conditionalPanel(condition = "output.disp_method == 'RFK'",
                               h4("RF Variable Importance (Actual)"), plotOutput("rf_importance_plot_act", height = "350px"),
                               div(id = "rfk_pred_ui", h4("RF Variable Importance (Predicted)"), plotOutput("rf_importance_plot_pre", height = "350px")),
                               hr(),
                               h4("Internal Residual Variogram (Actual)"), plotOutput("rfk_internal_vgm_act", height = "350px"),
                               div(id = "rfk_internal_vgm_pre_ui", h4("Internal Residual Variogram (Predicted)"), plotOutput("rfk_internal_vgm_pre", height = "350px"))
                             ),
                            conditionalPanel(condition = "output.disp_method == 'CK'",
                               h4("Cross-Variogram (Actual)"), plotOutput("ck_variogram_plot_act", height = "350px"),
                               div(id = "ck_pred_ui", h4("Cross-Variogram (Predicted)"), plotOutput("ck_variogram_plot_pred", height = "350px"))
                             ),
                            conditionalPanel(condition = "output.disp_method == 'TPS'",
                               h4("TPS GCV Diagnostics (Actual)"), plotOutput("tps_gcv_plot_act", height = "350px"),
                               div(id = "tps_pred_ui", h4("TPS GCV Diagnostics (Predicted)"), plotOutput("tps_gcv_plot_pre", height = "350px"))
                             ),
                            conditionalPanel(condition = "!['OK', 'RK', 'RFK', 'CK', 'TPS'].includes(output.disp_method)",
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
                            conditionalPanel(condition = "output.disp_has_pred == 'yes'",
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
                              tags$p(style="font-size: 0.85em; opacity: 0.8; font-style: italic;", "Model-specific diagnostics and performance metrics (RMSE, R2)."),
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
                              style = "background-color: #f3e8ff; padding: 15px; border: 2px solid #9b59b6; border-radius: 8px; margin-bottom: 20px;",
                              h4("Variable Prediction Statistics"),
                              tags$p(style="font-size: 0.85em; opacity: 0.8; font-style: italic;", "Prediction accuracy and classification agreement metrics for uploaded data."),
                              h5("Prediction Performance (Uploaded Data)"),
                              div(class="table-container", DT::dataTableOutput("uploaded_metrics_table")),
                              hr(style="opacity: 0.3;"),
                              h5("Classification Performance (Uploaded Predictions)"),
                              selectInput("kappa_bin_method", "Binning Method:", choices = c("Agronomical Classes" = "agro", "Quartiles" = "quartile")),
                              div(class="table-container", DT::dataTableOutput("kappa_table"))
                            ),
                            div(style = "background-color: #e7f5ff; padding: 15px; border: 2px solid #339af0; border-radius: 8px;",
                              h4("Data Summary Statistics"),
                              tags$p(style="font-size: 0.85em; opacity: 0.8; font-style: italic;", "Aggregated descriptive statistics and area coverage for the data."),
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
  )
)

server <- function(input, output, session) {

  session_id <- paste0("session_", substr(session$token, 1, 16))
  session_progress_dir <- file.path(tempdir(), "monolith_progress", session_id)
  
  dir.create(session_progress_dir, recursive = TRUE, showWarnings = FALSE)

  leaflet_proj_cache <- new.env(parent = emptyenv())
  area_calc_cache <- new.env(parent = emptyenv())

  # Cache keys embed rv$run_counter, so entries from previous runs are
  # unreachable; cleared at each run start to stop unbounded memory growth.
  clear_raster_caches <- function() {
    rm(list = ls(envir = leaflet_proj_cache), envir = leaflet_proj_cache)
    rm(list = ls(envir = area_calc_cache), envir = area_calc_cache)
  }

  get_projected_raster <- function(r, cache_key) {
    if (exists(cache_key, envir = leaflet_proj_cache)) return(get(cache_key, envir = leaflet_proj_cache))
    if (inherits(r, "PackedSpatRaster")) r <- terra::unwrap(r)
    # The NULL result is cached too, so this notifies once per layer instead
    # of silently leaving the map empty.
    r_proj <- tryCatch(terra::project(r, "EPSG:4326"), error = function(e) {
      showNotification(paste("Map layer could not be projected for display:", conditionMessage(e)), type = "error")
      NULL
    })
    assign(cache_key, r_proj, envir = leaflet_proj_cache)
    r_proj
  }

  render_resid_plot <- function(cv_data_reactive, title_suffix = "") {
    renderCachedPlot({
      req(input$sel_loc_stats, cv_data_reactive())
      loc <- input$sel_loc_stats
      df_list <- cv_data_reactive()
      
      if(loc == "Total (Combined)") {
         sf_list <- lapply(df_list, function(x) {
           if(inherits(x, "sf")) return(sf::st_transform(x, 3857))
           if(is.data.frame(x) && "x" %in% colnames(x) && "y" %in% colnames(x)) {
             tryCatch({
               return(st_as_sf(x, coords = c("x", "y"), crs = rv$mapping$crs) %>% sf::st_transform(3857))
             }, error = function(e) return(NULL))
           }
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
         p_res <- plot(v_res, model = v_fit, 
                       main = list(label = paste("Residual Variogram:", loc, title_suffix), cex = 0.85), 
                       sub = list(label = v_sub, cex = 0.75), 
                       scales = list(cex = 0.75))
         print(p_res)
      }, error = function(e) {
         plot(1, 1, type="n", main=paste("Error:", e$message), axes=F)
      })
    }, cacheKeyExpr = {
      # cv data only changes at run completion (rv$results_rev); the names()
      # component invalidates the cache when the lists are reset at dispatch
      # so a running model shows a blank panel, not the previous run's plot.
      list("resid_vgm", title_suffix, input$sel_loc_stats, rv$results_rev,
           names(cv_data_reactive()))
    }, cache = "session")
  }

  # method is passed in from the run that produced the assets: reading
  # input$method here would mis-register diagnostics if the user changed the
  # sidebar while the run was still executing.
  register_locality_assets <- function(l, meta, comp_mode, val_type, method) {
     if(!is.null(rv$sf)) {
       df_l_act <- rv$sf %>% st_drop_geometry() %>% filter(loc == !!l, !is.na(v))
       if(nrow(df_l_act) > 0) {
         s_l <- summary(df_l_act$v)
         stats_l <- data.frame(Metric = names(s_l), Value = as.character(round(as.numeric(s_l), 3)))
         register_export_item(paste0("table_stats_loc_", l), paste(meta$label, "-", l, "- Descriptive Statistics (Actual)"), "table", stats_l, meta$category)
       }
       
       if(comp_mode || val_type != "actual") {
         df_l_pre <- rv$sf %>% st_drop_geometry() %>% filter(loc == !!l, !is.na(pv))
         if(nrow(df_l_pre) > 0) {
           s_l_pre <- summary(df_l_pre$pv)
           stats_l_pre <- data.frame(Metric = names(s_l_pre), Value = as.character(round(as.numeric(s_l_pre), 3)))
           register_export_item(paste0("table_stats_pre_loc_", l), paste(meta$label, "-", l, "- Descriptive Statistics (Predicted)"), "table", stats_l_pre, meta$category)
         }
       }

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
     
     if(!is.null(rv$cv_metrics_act[[l]])) {
       cv_l <- rv$cv_metrics_act[[l]]
       n_obs_l <- if(!is.null(rv$cv_data_act[[l]])) nrow(rv$cv_data_act[[l]]) else NA
       cv_table <- data.frame(Metric = names(cv_l), Value = as.character(round(as.numeric(cv_l), 4)))
       cv_table <- rbind(data.frame(Metric = "CV Type", Value = cv_type_label(n_obs_l, rv$cv_strategy_sel)), cv_table)
       register_export_item(paste0("table_cv_loc_", l), paste(meta$label, "-", l, "- Model CV Metrics (Actual)"), "table", cv_table, meta$category)
     }

     if((comp_mode || val_type != "actual") && !is.null(rv$cv_metrics_pre[[l]])) {
       cv_l_p <- rv$cv_metrics_pre[[l]]
       n_obs_l_p <- if(!is.null(rv$cv_data_pre[[l]])) nrow(rv$cv_data_pre[[l]]) else NA
       cv_table_p <- data.frame(Metric = names(cv_l_p), Value = as.character(round(as.numeric(cv_l_p), 4)))
       cv_table_p <- rbind(data.frame(Metric = "CV Type", Value = cv_type_label(n_obs_l_p, rv$cv_strategy_sel)), cv_table_p)
       register_export_item(paste0("table_cv_pre_loc_", l), paste(meta$label, "-", l, "- Model CV Metrics (Predicted)"), "table", cv_table_p, meta$category)
     }
     
     if(isTruthy(input$color_style %in% c("agro", "bin")) && !is.null(rv$rast_list_act[[l]])) {
       area_l <- calc_area_df(rv$rast_list_act[[l]], paste0("export_act_", l))
       if(is.data.frame(area_l)) register_export_item(paste0("table_area_loc_", l), paste(meta$label, "-", l, "- Area Coverage"), "table", area_l, meta$category)
     }
     
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
     
     if(!is.null(rv$cv_data_act[[l]])) {
       df_cv <- as.data.frame(rv$cv_data_act[[l]])
       p_op <- tryCatch({
         build_obs_pred_plot(df_cv, title = paste("Obs vs Pred (Actual):", l), x_lab = "Observed", y_lab = "Predicted")
       }, error = function(e) {
         rv$log <- paste0(rv$log, "\n[WARN] Obs vs Pred export plot (Actual) skipped for ", l, ": ", conditionMessage(e))
         NULL
       })
       if(!is.null(p_op)) {
         register_export_item(paste0("plot_obs_pred_", l), paste(meta$label, "-", l, "- Obs vs Pred Scatter (Actual)"), "plot", p_op, meta$category)
       }
     }
     if((comp_mode || val_type != "actual") && !is.null(rv$cv_data_pre[[l]])) {
       df_cv_p <- as.data.frame(rv$cv_data_pre[[l]])
       p_op_p <- tryCatch({
         build_obs_pred_plot(df_cv_p, title = paste("Obs vs Pred (Predicted Map):", l), x_lab = "Observed", y_lab = "Predicted")
       }, error = function(e) {
         rv$log <- paste0(rv$log, "\n[WARN] Obs vs Pred export plot (Predicted Map) skipped for ", l, ": ", conditionMessage(e))
         NULL
       })
       if(!is.null(p_op_p)) {
         register_export_item(paste0("plot_obs_pred_pre_", l), paste(meta$label, "-", l, "- Obs vs Pred Scatter (Predicted Map)"), "plot", p_op_p, meta$category)
       }
     }
     
     if(method == "TPS" && !is.null(rv$tps_gcv_data[[paste0(l, "_act")]])) {
       df_gcv <- rv$tps_gcv_data[[paste0(l, "_act")]]
       p_gcv <- ggplot(df_gcv, aes(x = lambda, y = gcv)) + 
         geom_line(color = "steelblue") + geom_point() + scale_x_log10() +
         labs(title = paste("TPS GCV Diagnostics (Actual):", l)) + theme_minimal()
       register_export_item(paste0("plot_tps_gcv_", l), paste(meta$label, "-", l, "- TPS GCV Curve (Actual)"), "plot", p_gcv, meta$category)
     }
     if(method == "TPS" && (comp_mode || val_type != "actual") && !is.null(rv$tps_gcv_data[[paste0(l, "_pre")]])) {
       df_gcv_p <- rv$tps_gcv_data[[paste0(l, "_pre")]]
       p_gcv_p <- ggplot(df_gcv_p, aes(x = lambda, y = gcv)) + 
         geom_line(color = "firebrick") + geom_point() + scale_x_log10() +
         labs(title = paste("TPS GCV Diagnostics (Predicted):", l)) + theme_minimal()
       register_export_item(paste0("plot_tps_gcv_pre_", l), paste(meta$label, "-", l, "- TPS GCV Curve (Predicted)"), "plot", p_gcv_p, meta$category)
     }
     
     if(method == "RFK" && !is.null(rv$rf_models[[paste0(l, "_act")]])) {
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
     if(method == "RFK" && (comp_mode || val_type != "actual") && !is.null(rv$rf_models[[paste0(l, "_pre")]])) {
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

     if(method == "RK" && !is.null(rv$model_summaries[[paste0(l, "_act")]])) {
       lm_sum <- rv$model_summaries[[paste0(l, "_act")]]
       coef_df <- as.data.frame(lm_sum$coefficients)
       coef_df$Variable <- rownames(coef_df)
       coef_df <- coef_df[, c("Variable", "Estimate", "Std. Error", "t value", "Pr(>|t|)" )]
       register_export_item(paste0("table_rk_coef_act_", l), paste(meta$label, "-", l, "- RK Regression Coefficients (Actual)"), "table", coef_df, meta$category)
     }
     if(method == "RK" && (comp_mode || val_type != "actual") && !is.null(rv$model_summaries[[paste0(l, "_pre")]])) {
       lm_sum_p <- rv$model_summaries[[paste0(l, "_pre")]]
       coef_df_p <- as.data.frame(lm_sum_p$coefficients)
       coef_df_p$Variable <- rownames(coef_df_p)
       coef_df_p <- coef_df_p[, c("Variable", "Estimate", "Std. Error", "t value", "Pr(>|t|)" )]
       register_export_item(paste0("table_rk_coef_pre_", l), paste(meta$label, "-", l, "- RK Regression Coefficients (Predicted)"), "table", coef_df_p, meta$category)
     }

     if(method == "CK" && !is.null(rv$gstat_objs[[paste0(l, "_act")]])) {
       g <- rv$gstat_objs[[paste0(l, "_act")]]
       vm <- variogram(g)
       p_ck <- plot(vm, model = g$model, main = paste("Cross-Variogram (Actual):", l))
       register_export_item(paste0("plot_ck_vgm_act_", l), paste(meta$label, "-", l, "- CK Cross-Variogram (Actual)"), "plot", p_ck, meta$category)
     }
     if(method == "CK" && (comp_mode || val_type != "actual") && !is.null(rv$gstat_objs[[paste0(l, "_pre")]])) {
       g_p <- rv$gstat_objs[[paste0(l, "_pre")]]
       vm_p <- variogram(g_p)
       p_ck_p <- plot(vm_p, model = g_p$model, main = paste("Cross-Variogram (Predicted):", l))
       register_export_item(paste0("plot_ck_vgm_pred_", l), paste(meta$label, "-", l, "- CK Cross-Variogram (Predicted)"), "plot", p_ck_p, meta$category)
     }
     
     if(method %in% c("IDW", "TPS")) {
       param_df <- build_regional_params_df(method, l, rv$disp$regional_params,
                                            has_pre = comp_mode || val_type != "actual")
       if(!is.null(param_df)) {
         register_export_item(paste0("table_params_loc_", l), paste(meta$label, "-", l, "- Model Parameters"), "table", param_df, meta$category)
       }
     }
  }

  observeEvent(input$value_type, {
    if (isTruthy(input$value_type) && input$value_type == "resid") {
      updateCheckboxInput(session, "comp_mode", value = TRUE)
      shinyjs::disable("comp_mode")
    } else {
      shinyjs::enable("comp_mode")
    }
  })

  observeEvent(list(rv$disp, rv$has_predictions, rv$cv_data_act), {
    # Committed run context (not the live sidebar): the predicted-side panels
    # describe the run on screen and must survive sidebar reconfiguration.
    d <- rv$disp
    prediction_active <- !is.null(d) && (isTRUE(d$comp_mode) || !identical(d$value_type, "actual"))
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
    
    # Only show the uploaded-prediction statistics when the DISPLAYED run's
    # variable actually has an uploaded prediction column (detect_pred_column
    # stores NA - not NULL - when none exists, hence is_valid_col_ref).
    has_upl_pred <- !is.null(d) && (is_valid_col_ref(d$pred) || is_valid_col_ref(d$pred_ss))
    shinyjs::toggle(id = "prediction_performance_ui", condition = has_upl_pred)
  }, ignoreNULL = FALSE)

  desc_exploratory_server(
    id = "exploratory",
    data_reactive = reactive(rv$user_data),
    vars_metadata_reactive = reactive(rv$mapping$vars)
  )

  classif_server(
    id = "classification",
    data_reactive = reactive(rv$user_data),
    vars_metadata_reactive = reactive(rv$mapping$vars),
    spatial_reactive = reactive(list(
      x = rv$mapping$x, y = rv$mapping$y,
      src_crs = rv$mapping$crs, proj_crs = input$crs_selection,
      loc = rv$mapping$loc
    )),
    # Context-panel locality selection feeds the module's scope default;
    # polygons (map-drawn + uploaded shapefile) enable its polygon scope.
    # get_drawn_sf is defined later in this server function - reactives only
    # look the binding up at evaluation time, after server setup completes.
    context_localities_reactive = reactive(input$locality),
    polygons_reactive = reactive(list(drawn = get_drawn_sf(), shp = rv$shp_bound)),
    # Sidebar Spatial Engine settings (Boundary Type / Buffer Logic /
    # Resolution Logic) shared with the interpolation runs, so the
    # classification maps use the same boundary, buffer, and grid resolution
    # the user configured once - no duplicate controls inside the module.
    boundary_settings_reactive = reactive(list(
      type = input$boundary_type, buff_mode = input$buff_mode,
      buff_dist = input$buff_dist, res_mode = input$res_mode,
      res = input$grid_res
    ))
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
    
    if (!is.null(new_tiles) && new_tiles != "") {
      updateSelectInput(session, "base_map_layer", selected = new_tiles)
    }
  }, ignoreInit = FALSE)

  session_state <- new.env(parent = emptyenv())
  session_state$main_map_rendered <- FALSE
  session_state$comp_maps_rendered <- FALSE
  session_state$minimap_rendered <- FALSE

  # Bumped by every map renderLeaflet. Overlay observers depend on it so
  # proxy-managed layers (points, borders, controls) are re-applied after each
  # full re-render; leafletProxy defers until after the flush, so the calls
  # land on the freshly rendered widget.
  map_overlay_rev <- reactiveVal(0L)
  overlay_map_ids <- c("main_map", "comp_map_left", "comp_map_right")

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
    cv_strategy_sel = "auto", # CV strategy applied in the last run (for labels)
    loc_resolutions = list(), # Track spatial resolutions per locality
    idw_factors = list(), tps_lambdas = list(), # Regional Parameters
    tps_gcv_data = list(), # GCV Diagnostic Data
    full_cor_matrix = NULL, # Correlation Matrix for all numeric variables
    show_corr_panel = FALSE, # Toggle for sidebar correlation panel
    pop_up_vars = NULL, # Selected variables for pop-ups
    model_summaries = list(), # summaries for UK/RK
    rf_models = list(), # trained random forests
    gstat_objs = list(), # gstat objects for CK
    loc_names = NULL, log = "Ready.",
    results_rev = 0L, # Bumped when a run's results land (keys cached plots)
    drawn_feature = NULL, # Temporarily store drawn shape for grouping
    run_config_summary = NULL, # Plain text summary of latest run configuration
    disp = NULL, # Display context committed at run dispatch (see get_display_meta)
    run_counter = 0L, # Incremental run counter
    run_history = list(), # Archive of previous run results and configs
    proceed_run = NULL, # Trigger for model generation after archive decision
    pt_style_colors = NULL, # F2: Named vector group_value -> hex_color
    pt_style_palette = "Set1", # F2: Current qualitative palette name
    auto_archive_choice = "none", # "none", "archive", or "discard"
    model_running = FALSE, # True when parallel model calculations are active
    run_token = 0L # Incremental run token for async cancellation
  )
  
  register_export_item <- function(id, label, type, obj, category = "General", kind = "value") {
    req(obj)
    clean_id <- gsub("[^a-zA-Z0-9_]", "_", id)

    new_item <- list(
      id = clean_id,
      label = label,
      type = type, # "plot", "table", "map"
      obj = obj,
      category = category,
      # For maps: "value" (concentration surface, may be agro/bin classified),
      # "residual" (diverging scale, never classified), "uncertainty"
      # (sequential continuous, never classified). Agronomic class limits are
      # defined on the variable's units, so classifying errors or variances
      # with them is scientifically meaningless.
      kind = kind,
      timestamp = Sys.time()
    )
    
    current_reg <- isolate(rv$export_registry)
    current_reg[[clean_id]] <- new_item
    rv$export_registry <- current_reg
    
    rv$log <- paste0(rv$log, "\n[Registry] Registered ", type, ": ", label)
  }
  
  output$export_registry_ui <- renderUI({
    req(rv$export_registry)
    reg <- rv$export_registry
    if (length(reg) == 0) return(tags$p("Registry is empty. Run models to populate."))
    
    choices <- setNames(names(reg), sapply(reg, function(x) {
      sprintf("%s [%s] - %s", x$label, x$type, format(x$timestamp, "%H:%M:%S"))
    }))
    
    checkboxGroupInput("selected_assets", NULL, choices = choices, width = "100%")
  })

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

  output$run_config_display_map <- renderUI({
    cfg <- rv$run_config_summary
    if (is.null(cfg)) return(NULL)
    div(style = "background-color: #e8f4fd; padding: 8px 12px; border-left: 4px solid #2196F3; border-radius: 4px; margin-bottom: 8px; font-size: 0.82em;",
      tags$strong(paste0("Run #", cfg$run_id, ": ")),
      tags$span(paste0(cfg$variable, " | ", cfg$method, " | ", cfg$localities, " | ", format(cfg$timestamp, "%H:%M:%S")))
    )
  })

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

  env_hist <- new.env(parent = emptyenv())
  env_hist$history_obs <- list()
  observeEvent(rv$run_history, {
    hist <- rv$run_history
    lapply(env_hist$history_obs, function(o) {
      if (!is.null(o)) o$destroy()
    })
    env_hist$history_obs <- list()
    
    lapply(seq_along(hist), function(i) {
      run <- hist[[i]]
      run_id <- run$config$run_id
      
      obs_restore <- observeEvent(input[[paste0("restore_run_", run_id)]], {
        history_list <- isolate(rv$run_history)
        idx <- which(sapply(history_list, function(x) x$config$run_id) == run_id)[1]
        if (is.na(idx)) return()
        
        run_to_restore <- history_list[[idx]]
        
        current_cfg <- isolate(rv$run_config_summary)
        current_reg <- isolate(rv$export_registry)
        if (!is.null(current_cfg) && length(current_reg) > 0) {
          push_run_history(list(config = current_cfg, registry = current_reg),
                           base = history_list[-idx])
        } else {
          rv$run_history <- history_list[-idx]
        }
        
        rv$export_registry <- run_to_restore$registry
        rv$run_config_summary <- run_to_restore$config
        
        showNotification(paste0("Restored Run #", run_to_restore$config$run_id, " to active session."), type = "message")
      }, ignoreInit = TRUE)

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
  }, ignoreNULL = FALSE)

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
  
  styled_preview_obj_d <- styled_preview_obj %>% debounce(500)
  
  output$styler_preview_dynamic_ui <- renderUI({
    req(input$styler_width, input$styler_height)
    w_px <- (if(isTruthy(input$styler_width)) input$styler_width else 10) * 96
    h_px <- (if(isTruthy(input$styler_height)) input$styler_height else 8) * 96
    
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
  
  observeEvent(input$quick_export_map, {
    # Export exactly what the viewer shows: the committed run's surface for
    # the currently selected view.
    meta <- get_display_meta(); req(meta)
    view <- input$map_view %||% "view_act"
    target <- switch(view,
      "view_pred"  = rv$rast_pred,
      "view_resid" = rv$rast_res,
      "view_comp"  = if (!is.null(rv$rast) && !is.null(rv$rast_pred)) list(act = rv$rast, pre = rv$rast_pred) else NULL,
      rv$rast)
    req(target)

    type <- if (view == "view_comp") "map_combined" else "map"
    kind <- if (view == "view_resid") "residual" else "value"
    view_lab <- switch(view,
      "view_pred" = "Predicted", "view_resid" = "Residuals",
      "view_comp" = "Actual vs Predicted", "Actual")

    id <- paste0("quick_", view, "_", meta$actual)
    label <- paste("Quick Export:", meta$label, "(", view_lab, ")")

    register_export_item(id, label, type, target, meta$category, kind = kind)
    active_styler_item(id)

    shinyjs::click("open_styler")
  })
  
  # Persist the styler config once per settled state (debounced), not on
  # every keystroke/slider tick in every styler field.
  styler_persist_cfg <- debounce(reactive({
    req(input$styler_title_size)
    lapply(styler_fields, function(field) {
      input[[field$name]]
    })
  }), 750)
  observeEvent(styler_persist_cfg(), {
    shinyjs::runjs(sprintf("localStorage.setItem('monolith_styler_config', JSON.stringify(%s));", jsonlite::toJSON(styler_persist_cfg(), auto_unbox = TRUE)))
  })

  observeEvent(input$open_styler, {
    # priority: 'event' forces the sync to re-fire on every open: Shiny
    # dedupes identical input payloads, and the modal re-creates its inputs
    # with default values each time, so a deduped (skipped) restore would let
    # the debounced persist observer overwrite the saved config with defaults.
    shinyjs::runjs("
      var cfg = localStorage.getItem('monolith_styler_config');
      if(cfg) {
        Shiny.setInputValue('styler_local_config', JSON.parse(cfg), {priority: 'event'});
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
                                        choices = c("PNG" = "png", "TIFF" = "tiff", "PDF" = "pdf", "JPEG" = "jpg")),
                            hr(),
                            h4("3. Residual / Error Maps"),
                            selectInput("styler_resid_palette", "Diverging Palette",
                                        choices = c("Red-Blue" = "RdBu", "Red-Yellow-Blue" = "RdYlBu",
                                                    "Purple-Orange" = "PuOr", "Brown-Teal" = "BrBG",
                                                    "Pink-Green" = "PiYG", "Purple-Green" = "PRGn",
                                                    "Spectral" = "Spectral", "Red-Yellow-Green" = "RdYlGn",
                                                    "Red-Grey" = "RdGy"),
                                        selected = "RdBu"),
                            tags$p(style = "font-size: 0.8em; color: #666; margin-top: -8px;",
                                   "Applies to residual and point error maps only. Zero is always centered.")
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
      
      temp_dir <- file.path(tempdir(), paste0("export_", as.integer(Sys.time())))
      dir.create(temp_dir, showWarnings = FALSE)
      
      withProgress(message = "Batch Exporting...", value = 0, {
        selected_ids <- input$selected_assets
        items <- rv$export_registry[selected_ids]
        
        table_items <- Filter(function(x) x$type == "table", items)
        plot_items <- Filter(function(x) x$type %in% c("plot", "map", "map_combined"), items)
        
        n_plots <- length(plot_items)
        has_tables <- length(table_items) > 0
        total_steps <- n_plots + (if(has_tables) 1 else 0)

        files_to_zip <- c()

        if(has_tables) {
          incProgress(1/total_steps, detail = "Compiling statistical tables into Excel...")
          
          timestamp <- format(Sys.time(), "%Y%m%d_%H%M%S")
          excel_name <- sprintf("Batch_Statistics_%s.xlsx", timestamp)
          excel_path <- file.path(temp_dir, excel_name)
          
          wb <- createWorkbook()
          used_sheet_names <- c()
          for(item in table_items) {
            clean_label <- gsub("[^a-zA-Z0-9 ]", "_", item$label)
            sheet_name <- substr(clean_label, 1, 31)
            
            if(sheet_name %in% used_sheet_names) {
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
        
        for (i in seq_along(plot_items)) {
          item <- plot_items[[i]]
          incProgress(1/total_steps, detail = paste("Exporting Plot:", item$label))
          
          timestamp <- format(Sys.time(), "%Y%m%d_%H%M%S")
          ext <- if(item$type %in% c("plot", "map", "map_combined")) (input$styler_format %||% "png") else "png"
          if(ext == "csv") ext <- "png" # Extra safety
          
          filename <- sprintf("Batch_%s_%s.%s", item$id, timestamp, ext)
          filepath <- file.path(temp_dir, filename)
          
          tryCatch({
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
      
      if (length(files_to_zip) == 0) {
        unlink(temp_dir, recursive = TRUE)
        showNotification("Batch export failed: none of the selected assets could be exported. Check the Log tab for details.", type = "error", duration = 10)
        stop("No assets could be exported.")
      }

      zip_path <- file.path(temp_dir, "export.zip")
      zip::zip(zipfile = zip_path, files = files_to_zip, root = temp_dir)

      file.copy(zip_path, file)

      unlink(temp_dir, recursive = TRUE)
    }
  )
  
  
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

  # One handler set per map: a newly drawn shape opens the assign-locality
  # modal, and polygons are also kept in rv$drawn_polygons for map exports.
  # Keys are prefixed with the map id; leaflet ids are only unique per map.
  for (.map_id in c("main_map", "comp_map_left", "comp_map_right")) local({
    map_id <- .map_id
    poly_key <- function(feat) paste0(map_id, "_", feat$properties$`_leaflet_id`)
    observeEvent(input[[paste0(map_id, "_draw_new_feature")]], {
      feat <- input[[paste0(map_id, "_draw_new_feature")]]
      if (feat$geometry$type %in% c("Polygon", "MultiPolygon")) rv$drawn_polygons[[poly_key(feat)]] <- feat
      handle_new_feature(feat)
    })
    observeEvent(input[[paste0(map_id, "_draw_edited_features")]], {
      for (feat in input[[paste0(map_id, "_draw_edited_features")]]$features) {
        if (feat$geometry$type %in% c("Polygon", "MultiPolygon")) rv$drawn_polygons[[poly_key(feat)]] <- feat
      }
    })
    observeEvent(input[[paste0(map_id, "_draw_deleted_features")]], {
      for (feat in input[[paste0(map_id, "_draw_deleted_features")]]$features) {
        rv$drawn_polygons[[poly_key(feat)]] <- NULL
      }
    })
  })

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

    df_map <- rv$user_data %>% 
      filter(!is.na(!!sym(rv$mapping$x)) & !is.na(!!sym(rv$mapping$y)))
    
    pts_sf <- sf::st_as_sf(df_map, coords = c(rv$mapping$x, rv$mapping$y), crs = rv$mapping$crs)
    poly_sf_trans <- sf::st_transform(poly_sf, sf::st_crs(pts_sf))
    
    intersect_idx <- sf::st_intersects(pts_sf, poly_sf_trans, sparse = FALSE)
    
    in_poly <- rowSums(intersect_idx) > 0
    
    if (any(in_poly)) {
      if (!"Assigned_Locality" %in% names(rv$user_data)) {
        rv$user_data$Assigned_Locality <- NA
      }
      
      valid_rows <- which(!is.na(rv$user_data[[rv$mapping$x]]) & !is.na(rv$user_data[[rv$mapping$y]]))
      intersecting_user_rows <- valid_rows[in_poly]
      
      rv$user_data$Assigned_Locality[intersecting_user_rows] <- group_name
      
      showNotification(paste("Assigned", length(intersecting_user_rows), "points to group:", group_name), type = "message")
      
      current_loc <- input$map_loc
      updateSelectInput(session, "map_loc", choices = colnames(rv$user_data), selected = current_loc)
      
      rv$user_data <- rv$user_data 
    } else {
      showNotification("No points found within the selected area.", type = "warning")
    }
    
    removeModal()
    rv$drawn_feature <- NULL
  })
  
  get_regional_param <- function(type, loc, target, default = NULL) {
    field <- if(type == "IDW") "idw_factors" else "tps_lambdas"
    val <- rv[[field]][[loc]][[target]]
    if(is.null(val)) {
      if(!is.null(default)) return(default)
      if(type == "IDW") return(2.0)
      if(type == "TPS") return(-1.0)
    }
    return(val)
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

  popup_metadata_cache <- reactive({
    req(rv$mapping$vars)
    meta_list <- rv$mapping$vars
    
    vars_to_show <- rv$pop_up_vars
    if(is.null(vars_to_show) || length(vars_to_show) == 0) {
      soil_vars <- Filter(function(x) grepl("Soil|Physicochem", x$category, ignore.case = TRUE), meta_list)
      if(length(soil_vars) > 0) {
        vars_to_show <- sapply(soil_vars, function(x) x$actual)
      } else {
        vars_to_show <- sapply(meta_list, function(x) x$actual)
      }
    }
    
    all_cats <- unique(sapply(meta_list, function(x) x$category))
    priority_cats <- all_cats[grepl("Soil|Physicochem", all_cats, ignore.case = TRUE)]
    other_cats <- setdiff(all_cats, priority_cats)
    cats <- c(priority_cats, other_cats)
    
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

  # Resolve each popup variable to its data column once per point set, so
  # generate_popup does a plain lookup per point instead of up to four regex
  # searches per variable per point. Returns a popup_fn(data_row) closure.
  make_popup_fn <- function(cnames) {
    resolve_col <- function(key) {
      key <- as.character(key)
      if (key %in% cnames) return(key)
      idx <- grep(paste0("^", key, "$"), cnames, ignore.case = TRUE)
      if (length(idx) > 0) return(cnames[idx[1]])
      idx <- grep(paste0(key, "$"), cnames, ignore.case = TRUE)
      if (length(idx) > 0) return(cnames[idx[1]])
      idx <- grep(key, cnames, ignore.case = TRUE)
      if (length(idx) > 0) return(cnames[idx[1]])
      NA_character_
    }
    cache <- popup_metadata_cache()
    keys <- unique(c(
      unlist(lapply(cache$grouped_vars, function(cvs) vapply(cvs, function(v) as.character(v$actual), character(1)))),
      as.character(setdiff(cache$vars_to_show, cache$meta_actuals))
    ))
    col_map <- vapply(keys, resolve_col, character(1))
    function(data_row) generate_popup(data_row, col_map = col_map)
  }

  generate_popup <- function(data_row, col_map = NULL) {
    data_row <- as.list(data_row)
    names_in_row <- names(data_row)

    find_val <- function(key) {
      key <- as.character(key)
      if (!is.null(col_map) && key %in% names(col_map)) {
        col <- col_map[[key]]
        if (is.na(col)) return(NULL)
        return(data_row[[col]])
      }
      if (key %in% names_in_row) return(data_row[[key]])
      idx <- grep(paste0("^", key, "$"), names_in_row, ignore.case = TRUE)
      if (length(idx) > 0) return(data_row[[idx[1]]])
      idx <- grep(paste0(key, "$"), names_in_row, ignore.case = TRUE)
      if (length(idx) > 0) return(data_row[[idx[1]]])
      idx <- grep(key, names_in_row, ignore.case = TRUE)
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

  observeEvent(input$toggle_pt_style, {
    shinyjs::toggle("pt_style_toolbar", anim = TRUE, animType = "slide", time = 0.3)
  })

  observeEvent(list(rv$user_data, rv$mapping$vars, rv$mapping$loc, rv$mapping$x, rv$mapping$y), {
    req(rv$user_data)
    df <- rv$user_data
    cols <- colnames(df)

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
    
    curr_color_by <- isolate(input$pt_color_by)
    selected_color_by <- if (!is.null(curr_color_by) && curr_color_by %in% color_choices) curr_color_by else "none"
    updateSelectInput(session, "pt_color_by", choices = color_choices, selected = selected_color_by)

    col_labels <- sapply(cols, function(c) {
      get_var_label(c, vars_meta)
    })
    label_choices <- c("(none)" = "none", stats::setNames(cols, col_labels))
    
    curr_label_field <- isolate(input$pt_label_field)
    selected_label_field <- if (!is.null(curr_label_field) && curr_label_field %in% label_choices) curr_label_field else "none"
    updateSelectInput(session, "pt_label_field", choices = label_choices, selected = selected_label_field)
  })

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
  }, ignoreInit = TRUE)

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

  output$file_uploaded <- reactive({ !is.null(input$user_file) })
  outputOptions(output, "file_uploaded", suspendWhenHidden = FALSE)
  
  output$model_ready <- reactive({ if(!is.null(rv$rast) || length(rv$v_emp_list) > 0) "yes" else "no" })
  outputOptions(output, "model_ready", suspendWhenHidden = FALSE)

  # Committed run context for JS conditionalPanels (same pattern as
  # model_ready): the Scientific Analysis sections must describe the run on
  # screen, not the live sidebar method/value-type selections.
  output$disp_method <- reactive({ rv$disp$method %||% "" })
  outputOptions(output, "disp_method", suspendWhenHidden = FALSE)
  output$disp_has_pred <- reactive({
    d <- rv$disp
    active <- !is.null(d) && (isTRUE(d$comp_mode) || !identical(d$value_type, "actual"))
    if (active || isTRUE(rv$has_predictions)) "yes" else "no"
  })
  outputOptions(output, "disp_has_pred", suspendWhenHidden = FALSE)
  
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
      if (tolower(ext) == "csv") as.data.frame(data.table::fread(input$user_file$datapath))
      else if (tolower(ext) %in% c("xls", "xlsx")) readxl::read_excel(input$user_file$datapath)
      else NULL
    }, error = function(e) { 
      showNotification(paste("Error reading file:", e$message), type = "error")
      NULL
    })
    
    req(df); rv$user_data <- df
    
    cols <- colnames(df)
    updateSelectInput(session, "map_x", choices = cols, selected = grep("\\bx\\b|^lon|^longitude", cols, ignore.case=TRUE, value=TRUE)[1])
    updateSelectInput(session, "map_y", choices = cols, selected = grep("\\by\\b|^lat|^latitude", cols, ignore.case=TRUE, value=TRUE)[1])
    loc_guess <- grep("loc|site|farm|id|group", cols, ignore.case=TRUE, value=TRUE)[1]
    if (is.na(loc_guess)) loc_guess <- cols[1]
    updateSelectInput(session, "map_loc", choices = cols, selected = loc_guess)
    
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
    
    rv$pop_up_vars <- num_cols[!grepl("\\bx\\b|\\by\\b|lon|lat|latitude|longitude", num_cols, ignore.case=TRUE)]
    
    curr_locs <- isolate(input$locality)
    # input$map_loc is still the pre-updateSelectInput value here, so use
    # the freshly guessed column instead of the stale input.
    new_choices <- c("ALL", unique(df[[loc_guess]]))
    selected_locs <- intersect(curr_locs, new_choices)
    updateSelectInput(session, "locality", choices = new_choices, selected = selected_locs)

    subset_col <- find_subset_column(cols)
    subset_choices <- if (!is.na(subset_col)) {
      vals <- sort(unique(na.omit(as.character(df[[subset_col]]))))
      c("All" = "all", setNames(vals, vals))
    } else c("All" = "all")
    updateSelectInput(session, "subset", choices = subset_choices, selected = "all")

    shinyjs::runjs("setTimeout(function() { $('html, body').animate({ scrollTop: $('#map_x').offset().top - 20 }, 1000); }, 500);")
  })

  observeEvent(input$user_shp, {
    req(input$user_shp)
    temp_dir <- file.path(tempdir(), paste0("shp_upload_", as.integer(Sys.time())))
    dir.create(temp_dir, showWarnings = FALSE, recursive = TRUE)
    session$onSessionEnded(function() { unlink(temp_dir, recursive = TRUE) })
    
    for(i in seq_len(nrow(input$user_shp))) {
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

  observeEvent(list(rv$user_data, input$map_x, input$map_y), {
    req(rv$user_data, input$map_x, input$map_y)
    if (!(input$map_x %in% colnames(rv$user_data) && input$map_y %in% colnames(rv$user_data))) return()
    
    df <- rv$user_data %>% select(x = !!sym(input$map_x), y = !!sym(input$map_y)) %>% na.omit()
    if (nrow(df) == 0) return()
    
    x_range <- range(df$x)
    y_range <- range(df$y)
    
    suggested_crs <- "EPSG:4326" # Default WGS84
    
    if (all(x_range >= -180 & x_range <= 180) && all(y_range >= -90 & y_range <= 90)) {
       suggested_crs <- "EPSG:4326"
    } else if (all(x_range > 100000 | x_range < -100000)) {
       if (any(x_range < 0)) suggested_crs <- "EPSG:5514" 
       else suggested_crs <- "EPSG:32635" 
    }
    
    updateSelectizeInput(session, "map_crs", selected = suggested_crs)
    updateSelectizeInput(session, "crs_selection", selected = suggested_crs)
  })

  observeEvent(input$crs_selection, {
    req(input$crs_selection)

    # In fixed mode the user chose the value deliberately, so only refresh the
    # slider frame; in auto modes the suggestion observer overwrites the value
    # right after anyway.
    if (isTRUE(input$res_mode == "fixed")) {
      updateSliderInput(session, "grid_res", label = "Resolution (m)",
                        min = 1, max = 500, step = 1)
    } else {
      updateSliderInput(session, "grid_res", label = "Resolution (m)",
                        min = 1, max = 500, value = 50, step = 1)
    }
  })
  observeEvent(list(rv$user_data, input$map_x, input$map_y, input$crs_selection, input$locality, input$res_mode), {
    req(rv$user_data, input$map_x, input$map_y, input$crs_selection, input$locality, input$res_mode)
    if (!(input$map_x %in% colnames(rv$user_data) && input$map_y %in% colnames(rv$user_data))) return()
    
    if (input$res_mode == "fixed") {
       return()
    }

    df_raw <- rv$user_data %>% select(x = !!sym(input$map_x), y = !!sym(input$map_y), loc = !!sym(input$map_loc)) %>% na.omit()

    locs_scope <- resolve_selected_localities(input$locality, df_raw, "loc")
    df <- if (input$res_mode == "global") df_raw else df_raw %>% filter(loc %in% locs_scope)
    
    if (nrow(df) < 2) return()
    
    crs_obj <- validate_crs(input$crs_selection, "Invalid CRS provided:")
    req(crs_obj)

    pts <- tryCatch({
      st_as_sf(df, coords = c("x", "y"), crs = input$map_crs) %>% st_transform(input$crs_selection)
    }, error = function(e) NULL)
    req(pts)

    # The grid_res slider is metric and interpolation always runs in a
    # projected CRS, so the suggestion is measured in metres even when the
    # analysis CRS is geographic (calc_metric_spacing handles the conversion).
    spacing <- calc_metric_spacing(pts)
    req(is.finite(spacing$mean_nn))

    rec_res <- spacing$mean_nn * 0.5
    min_res_by_dim <- spacing$max_dim / 300

    final_rec <- max(rec_res, min_res_by_dim)
    final_rec <- max(0.1, min(500, round(final_rec, 1)))

    if (input$res_mode != "fixed") updateSliderInput(session, "grid_res", value = final_rec)
    
    if (input$res_mode == "local") {
        locs_to_calc <- locs_scope
        temp_res <- list()
        for (l in locs_to_calc) {
            sub_df <- df_raw %>% filter(loc == l)
            if (nrow(sub_df) < 2) next
            
            sub_pts <- tryCatch(st_as_sf(sub_df, coords=c("x","y"), crs=input$map_crs) %>% st_transform(input$crs_selection), error=function(e) { showNotification(paste("Projection failed for subset:", e$message), type = "error"); NULL })
            if(is.null(sub_pts)) next
            
            if (nrow(sub_pts) > 1) {
                 l_res <- calc_metric_spacing(sub_pts)$mean_nn * 0.5
            } else l_res <- final_rec

            l_res <- max(1, min(5000, l_res))

            temp_res[[l]] <- l_res        }
        rv$loc_resolutions <- temp_res
    } else {
        temp_res <- list()
        for (l in locs_scope) temp_res[[l]] <- final_rec
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
    }, error = function(e) {
      showNotification(paste("Could not read the metadata file:", conditionMessage(e)), type = "error")
      NULL
    })

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
    
    if (!is.null(rv$mapping$vars) && length(rv$mapping$vars) > 0) {
      targets <- sapply(rv$mapping$vars, function(x) x$actual)
    } else {
      targets <- num_cols[!grepl("\\bx\\b|\\by\\b|lon|lat|latitude|longitude", num_cols, ignore.case=TRUE)]
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
          
          div(style="border-bottom: 1px solid rgba(0,0,0,0.08); padding: 10px 0; margin-bottom: 10px;",
            fluidRow(
              column(2, tags$b(t)),
              column(3, selectInput(paste0("pair_pred_cve_", i), "Best Pred (_cve)", choices = c("None", num_cols), selected = def_p_cve)),
              column(3, selectInput(paste0("pair_pred_ss_", i),  "Split Pred (_ss)", choices = c("None", num_cols), selected = def_p_ss)),
              column(2, textInput(paste0("pair_label_", i), "Label", value = def_l)),
              column(2, textInput(paste0("pair_cat_", i), "Category", value = def_c))
            )
          )
        }),
        actionButton("confirm_mapping", "Confirm Variable Mapping", icon = icon("check-circle"), class = "btn-primary btn-block btn-pill")
      )
    }, error = function(e) {
      warning(paste("Error in var_mapping_ui:", e$message))
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

  # Rebuild the Context-panel locality selector, preserving the current
  # selection. Re-issued ONLY when the choice set actually changed: data
  # mutations that keep the same localities (drawn groups, discretized
  # columns) must not churn the selector mid-analysis.
  refresh_locality_choices <- function() {
    loc_col <- rv$mapping$loc
    if (is.null(rv$user_data) || is.null(loc_col) || !(loc_col %in% colnames(rv$user_data))) return(invisible(NULL))
    loc_choices <- unique(rv$user_data[[loc_col]])
    new_choices <- c("ALL", loc_choices)
    if (identical(session_state$locality_choices, new_choices)) return(invisible(NULL))
    session_state$locality_choices <- new_choices
    curr_locs <- isolate(input$locality)
    selected_locs <- intersect(curr_locs, new_choices)
    if (length(selected_locs) == 0) selected_locs <- loc_choices[1]
    updateSelectInput(session, "locality", choices = new_choices, selected = selected_locs)
  }

  observeEvent(list(input$map_x, input$map_y, input$map_loc, input$map_crs), {
    req(input$map_x, input$map_y, input$map_loc, input$map_crs)
    rv$mapping$x <- input$map_x
    rv$mapping$y <- input$map_y
    rv$mapping$loc <- input$map_loc
    rv$mapping$crs <- input$map_crs
    refresh_locality_choices()
  })

  # Data-driven refresh (new upload, added locality values); no-ops unless
  # the locality choice set changed.
  observeEvent(rv$user_data, {
    refresh_locality_choices()
  }, ignoreInit = TRUE)

  output$setup_minimap <- renderLeaflet({
    req(rv$user_data, rv$mapping$x, rv$mapping$y, rv$mapping$crs)

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

  get_current_meta <- function() {
    var <- input$var_id
    if (is.null(var) || var == "" || is.null(rv$mapping$vars)) return(NULL)
    
    idx <- which(sapply(rv$mapping$vars, function(x) x$actual == var))
    if (length(idx) == 0) return(NULL)
    m <- rv$mapping$vars[[idx]]
    
    pal <- "YlOrRd"
    if (!is.null(input$palette_select) && input$palette_select != "") {
      pal <- input$palette_select
    } else if (!is.null(m$palette) && m$palette != "") {
      pal <- m$palette
    }
    
    pred_col <- if(is_valid_col_ref(m$pred)) as.character(m$pred) else NULL
    pred_ss_col <- if(is_valid_col_ref(m$pred_ss)) as.character(m$pred_ss) else NULL

    view_col <- switch(input$value_type,
      "actual" = as.character(m$actual),
      "pred"   = pred_col,
      "pred_ss"= pred_ss_col,
      "resid"  = as.character(m$actual)
    )

    list(
      actual = as.character(m$actual),
      pred = pred_col,
      pred_ss = pred_ss_col,
      view_col = view_col,
      label = as.character(m$label %||% m$actual),
      palette = as.character(pal),
      unit = as.character(m$unit %||% "")
    )
  }

  # Display-side twin of get_current_meta(): returns the context committed at
  # run dispatch (rv$disp) instead of reading the live sidebar inputs, so the
  # Map Viewer and Scientific Analysis tabs keep describing the run that is
  # actually on screen while the sidebar is reconfigured for the next run.
  # Only the colour palette stays live - styling may be changed on the
  # displayed map at any time. Returns NULL before the first run.
  get_display_meta <- function() {
    d <- rv$disp
    if (is.null(d)) return(NULL)
    if (isTruthy(input$palette_select)) d$palette <- as.character(input$palette_select)
    d
  }

  observeEvent(list(rv$mapping$vars, input$var_category), {
    req(rv$mapping$vars)
    vars <- rv$mapping$vars
    cats <- unique(sapply(vars, function(x) x$category))

    current_cat <- input$var_category
    sel_cat <- if(!is.null(current_cat) && current_cat %in% cats) current_cat else cats[1]
    updateSelectInput(session, "var_category", choices = cats, selected = sel_cat)
    
    filtered <- Filter(function(x) x$category == sel_cat, vars)
    choices <- setNames(sapply(filtered, function(x) x$actual), sapply(filtered, function(x) x$label))
    updateSelectInput(session, "var_id", choices = choices)
  })

  # Guard: only offer ML prediction/residual views when the selected variable
  # actually has the corresponding prediction column in the uploaded data.
  observeEvent(list(input$var_id, rv$mapping$vars), {
    var <- input$var_id
    if (is.null(var) || var == "" || is.null(rv$mapping$vars)) return(NULL)
    idx <- which(sapply(rv$mapping$vars, function(x) x$actual == var))
    if (length(idx) == 0) return(NULL)
    m <- rv$mapping$vars[[idx[1]]]

    has_col <- is_valid_col_ref
    choices <- c("Actual Values" = "actual")
    if (has_col(m$pred)) choices <- c(choices, "Best ML Predictions (_cve)" = "pred")
    if (has_col(m$pred_ss)) choices <- c(choices, "Single Split ML Predictions (_ss)" = "pred_ss")
    if (has_col(m$pred)) choices <- c(choices, "Residuals (v - pv) of ML Predictions" = "resid")

    sel <- if (isTruthy(input$value_type) && input$value_type %in% choices) input$value_type else "actual"
    updateSelectInput(session, "value_type", choices = choices, selected = sel)

    # No prediction columns at all: Comparison Mode has nothing to compare,
    # so clear it even though its (hidden) checkbox keeps its last state.
    if (!has_col(m$pred) && !has_col(m$pred_ss) && isTRUE(input$comp_mode)) {
      updateCheckboxInput(session, "comp_mode", value = FALSE)
    }
  })

  # Open/close via the .open class (not inline right) so the drawer's CSS -
  # including the floating nav buttons and the outside-click closer - keys on
  # a single source of truth.
  observeEvent(input$info_btn, {
    shinyjs::runjs("document.getElementById('docs_drawer').classList.add('open');")
  })

  observeEvent(input$close_docs_btn, {
    shinyjs::runjs("document.getElementById('docs_drawer').classList.remove('open');")
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
          p("Integrated geostatistical modeling, classification and statistical interpretation."),
          hr(),
          p("Designed for high-performance parallel processing and spatial diagnostics, multi-scale interpolation via kriging, inverse distance weighting, and thin plate splines with practical multi-criteria optimization."),
          p("Supported with the Descriptive and Exploratory Suite with dynamic visualizations and statistics."),
          hr(),
          p(strong("A product of `that` couple of months following the loss of institutional e-mail address.")),
          p(style = "color: #666; font-size: 0.9em;", "  by Recep Serdar Kara in cooperation with Antigravity CLI and Claude Code - 2026 (v1.0.2)"),
          hr(),
          tags$details(
            tags$summary(style = "cursor: pointer; color: #007bff;", "Session Info (reproducibility)"),
            tags$pre(style = "text-align: left; font-size: 0.72em; max-height: 250px; overflow-y: auto; margin-top: 8px;",
                     paste(utils::capture.output(utils::sessionInfo()), collapse = "\n"))
          ),
          downloadButton("download_session_info", "Download session info (.txt)", class = "btn-sm btn-default", style = "margin-top: 8px;")
      )
    ))
  })

  output$download_session_info <- downloadHandler(
    filename = function() { paste0("monolith_session_info_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".txt") },
    content = function(file) {
      writeLines(c(
        paste0("Monolith session info: generated ", format(Sys.time(), "%Y-%m-%d %H:%M:%S")),
        "",
        utils::capture.output(utils::sessionInfo())
      ), file)
    }
  )
  
  output$render_user_guide <- renderUI({
    withMathJax(HTML(commonmark::markdown_html(paste(readLines("docs/user_guide.md", warn = FALSE), collapse = "\n"))))
  })
  
  output$render_desc_exploratory_guide <- renderUI({
    withMathJax(HTML(commonmark::markdown_html(paste(readLines("docs/desc_exploratory_guide.md", warn = FALSE), collapse = "\n"))))
  })
  
  output$render_scientific_guide <- renderUI({
    withMathJax(HTML(commonmark::markdown_html(paste(readLines("docs/scientific_guide.md", warn = FALSE), collapse = "\n"))))
  })
  
  joint_vv <- reactive({
    is_uncertainty <- isTruthy(input$show_uncertainty) && isTruthy((rv$disp$method %||% "") %in% c("OK", "RK", "RFK", "CK"))
    get_joint_scale_values(rv$rast, rv$rast_pred, input$match_scales, is_uncertainty)
  })

  classification_params <- reactive({
    req(input$color_style %in% c("agro", "bin"))
    # Displayed run's variable when one exists (class breaks must describe the
    # map on screen); live selection as pre-run fallback so the styling
    # controls stay usable before the first interpolation.
    meta <- get_display_meta()
    if (is.null(meta)) meta <- get_current_meta()
    req(meta)
    
    if (input$color_style == "agro") {
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
          target <- if(identical(input$map_view, "view_pred") || identical(input$map_view, "view_comp")) rv$rast_pred else rv$rast
          if(is.null(target)) {
            df <- rv$user_data
            v_data <- df[[meta$actual]]
            if(is.null(v_data) || length(v_data) < n_c) return(NULL)
            vv <- v_data
          } else {
            target_layer <- if("var1.pred" %in% names(target)) target[["var1.pred"]] else target[[1]]
            vv <- as.vector(values(target_layer, na.rm=TRUE))
          }
        }
        
        if(length(vv) < n_c) return(NULL)
        brks_inner <- tryCatch({
          classIntervals(vv, n=n_c, style=input$agro_method)$brks[2:n_c]
        }, error = function(e) {
          seq(min(vv, na.rm=TRUE), max(vv, na.rm=TRUE), length.out = n_c + 1)[2:n_c]
        })
      }
      
      brks <- sort(unique(c(-Inf, brks_inner, Inf)))
      n_c_actual <- length(brks) - 1
      
      rcl_mat <- matrix(NA, nrow = n_c_actual, ncol = 3)
      for(i in 1:n_c_actual) {
        rcl_mat[i, ] <- c(brks[i], brks[i+1], i)
      }
      
      colors <- get_agro_colors(n_c_actual)
      labels <- if(n_c_actual==3) c("Low", "Med", "High") else paste("Class", 1:n_c_actual)
      
      leg_labels <- character(n_c_actual)
      for(i in 1:n_c_actual) {
        if(i==1) leg_labels[i] <- paste("<", round(brks[2], 3))
        else if(i==n_c_actual) leg_labels[i] <- paste(">", round(brks[n_c_actual], 3))
        else leg_labels[i] <- paste(round(brks[i],3), "-", round(brks[i+1],3))
      }
      if(n_c_actual == 3) leg_labels <- paste(labels, ":", leg_labels)
      
      list(brks = brks, rcl_mat = rcl_mat, colors = colors, labels = labels, leg_labels = leg_labels, n_c = n_c_actual)
      
    } else {
      n_c <- 5
      vv_joint <- joint_vv()
      if(!is.null(vv_joint)) {
        vv <- vv_joint
      } else {
        target <- if(identical(input$map_view, "view_pred") || identical(input$map_view, "view_comp")) rv$rast_pred else rv$rast
        if(is.null(target)) {
          df <- rv$user_data
          v_data <- df[[meta$actual]]
          if(is.null(v_data) || length(v_data) < n_c) return(NULL)
          vv <- v_data
        } else {
          target_layer <- if("var1.pred" %in% names(target)) target[["var1.pred"]] else target[[1]]
          vv <- as.vector(values(target_layer, na.rm=TRUE))
        }
      }
      
      if(length(vv) < n_c) return(NULL)
      
      rng <- range(vv, na.rm = TRUE)
      if(is.infinite(rng[1]) || is.infinite(rng[2]) || rng[1] == rng[2]) {
        brks_inner <- seq(rng[1], rng[1] + 1, length.out = n_c + 1)[2:n_c]
      } else {
        brks_inner <- seq(rng[1], rng[2], length.out = n_c + 1)[2:n_c]
      }
      
      brks <- sort(unique(c(-Inf, brks_inner, Inf)))
      n_c_actual <- length(brks) - 1
      
      rcl_mat <- matrix(NA, nrow = n_c_actual, ncol = 3)
      for(i in 1:n_c_actual) {
        rcl_mat[i, ] <- c(brks[i], brks[i+1], i)
      }
      
      is_viridis <- meta$palette == "viridis"
      colors <- if(is_viridis) {
        viridis::viridis(n_c_actual, option = meta$palette)
      } else {
        colorRampPalette(RColorBrewer::brewer.pal(min(8, max(3, n_c_actual)), meta$palette))(n_c_actual)
      }
      
      labels <- paste("Bin", 1:n_c_actual)
      
      leg_labels <- character(n_c_actual)
      for(i in 1:n_c_actual) {
        if(i==1) leg_labels[i] <- paste("<", round(brks[2], 3))
        else if(i==n_c_actual) leg_labels[i] <- paste(">", round(brks[n_c_actual], 3))
        else leg_labels[i] <- paste(round(brks[i],3), "-", round(brks[i+1],3))
      }
      
      list(brks = brks, rcl_mat = rcl_mat, colors = colors, labels = labels, leg_labels = leg_labels, n_c = n_c_actual)
    }
  })

  agro_params <- reactive({
    req(input$color_style == "agro")
    classification_params()
  })

  volumes <- c(Home = fs::path_home(), Project = getwd())
  shinyFileChoose(input, "load_config", roots = volumes, session = session, filetypes = c("json"))

  
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
        map_x = input$map_x,
        map_y = input$map_y,
        map_loc = input$map_loc,
        map_crs = input$map_crs,
        crs_selection = input$crs_selection,
        var_category = input$var_category,
        var_id = input$var_id,
        value_type = input$value_type,
        method = input$method,
        boundary_type = input$boundary_type,
        buff_mode = input$buff_mode,
        buff_dist = input$buff_dist,
        res_mode = input$res_mode,
        grid_res = input$grid_res,
        color_style = input$color_style,
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
      
      if (!is.null(cfg$vars_mapping)) {
        rv$mapping$vars <- cfg$vars_mapping
      }
      
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
    # Follow the displayed run's variable once one exists so a context change
    # cannot silently reset the palette of the map on screen (input$var_id is
    # only a reactive dependency before the first run).
    vid <- if (!is.null(rv$disp)) rv$disp$var_id else input$var_id
    req(vid, rv$mapping$vars)
    # Agronomical styling supplies its own class palette, so the manual
    # colour-palette picker is irrelevant there — hide it.
    if (isTruthy(input$color_style) && input$color_style == "agro") return(NULL)
    idx <- which(sapply(rv$mapping$vars, function(x) x$actual == vid))
    if (length(idx) == 0) return(NULL)
    m <- rv$mapping$vars[[idx]]
    choices <- palette_choices_precomputed
    pickerInput("palette_select", "Color Palette", 
                choices = choices, 
                selected = m$palette %||% "YlOrRd",
                options = list(`live-search` = TRUE),
                choicesOpt = list(content = names(choices)))
  })


  # The range slider parameterizes a variogram fitted in the analysis CRS
  # (metric), so its scale must be measured there; the raw x/y columns may be
  # in degrees. The projection is cached against (data, mapping, CRS) so the
  # slider-bounds observer below does not re-transform every coordinate each
  # time a run finishes or the diagnostics locality changes.
  projected_max_dist <- reactive({
    req(rv$user_data)
    x_col <- rv$mapping$x
    y_col <- rv$mapping$y
    req(x_col, y_col, x_col %in% colnames(rv$user_data), y_col %in% colnames(rv$user_data))
    coords_df <- rv$user_data[, c(x_col, y_col)]
    coords_df <- coords_df[complete.cases(coords_df), , drop = FALSE]
    tryCatch({
      pts_proj <- st_transform(st_as_sf(coords_df, coords = c(x_col, y_col), crs = rv$mapping$crs), input$crs_selection)
      bb <- st_bbox(pts_proj)
      as.numeric(sqrt((bb["xmax"] - bb["xmin"])^2 + (bb["ymax"] - bb["ymin"])^2))
    }, error = function(e) NA_real_)
  })

  observeEvent(list(input$var_id, input$m_loc, input$crs_selection, rv$user_data, rv$v_fit_list), {
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

        max_dist <- tryCatch(projected_max_dist(), error = function(e) NA_real_)
        if (!is.null(max_dist) && !is.na(max_dist) && max_dist > 0) {
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
  })

  output$agro_options <- renderUI({
    req(input$color_style == "agro", input$agro_method == "limits")
    # input$var_id is only a reactive dependency before the first run;
    # afterwards the limits follow the displayed variable and are not reset
    # by sidebar context changes.
    vid <- if (!is.null(rv$disp)) rv$disp$var_id else input$var_id
    req(vid)
    nut <- get_nut_key(vid)
    def_limits <- if(!is.null(nut) && nut %in% names(nutrient_limits)) nutrient_limits[[nut]] else NULL
    
    lapply(1:(input$agro_n_classes - 1), function(i) {
      val <- if(!is.null(def_limits) && i <= length(def_limits)) def_limits[i] else i * 10
      numericInput(paste0("agro_limit_", i), paste("Limit", i), value = val)
    })
  })

  output$locality_selector_ui <- renderUI({
      req(rv$loc_names); selectInput("sel_loc_stats", "Filter Analysis View:", choices = c("Total (Combined)", rv$loc_names))
    })
  
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
        # Scope the correlations to the Context panel's locality selection:
        # ranks computed across ALL samples can be driven by between-locality
        # contrasts and mislead covariate choice for a single-locality run.
        df_base <- rv$user_data
        scope_lbl <- "all localities"
        if (!is.null(rv$mapping$loc) && rv$mapping$loc %in% colnames(df_base) &&
            length(input$locality) > 0 && !("ALL" %in% input$locality)) {
          df_base <- df_base[as.character(df_base[[rv$mapping$loc]]) %in% input$locality, , drop = FALSE]
          scope_lbl <- paste(input$locality, collapse = ", ")
        }
        if (nrow(df_base) < 3) {
          showNotification("Fewer than 3 samples in the selected localities - cannot rank predictors.", type = "error")
          return()
        }
        df <- df_base[sapply(df_base, is.numeric)]
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
        rv$cor_scope_label <- sprintf("%s, n = %d", scope_lbl, nrow(df_base))
        rv$show_corr_panel <- TRUE
      })
    
      output$corr_results_ui <- renderUI({
        req(rv$show_corr_panel, rv$full_cor_matrix, input$var_id)
        res_df <- rv$full_cor_matrix
        target <- input$var_id
        
        thresh <- as.numeric(input$corr_pval_thresh %||% 1)
        if(thresh < 1) {
           res_df <- res_df[!is.na(res_df$Pval) & res_df$Pval <= thresh, ]
        }
        
        if(nrow(res_df) == 0) return(tags$p("No variables meet the significance threshold."))
        
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

        cats <- unique(res_df$Category)

        tabs <- list()

        results_all <- res_df[order(res_df$AbsCorr, decreasing = TRUE), ]
        res_all <- head(results_all, 8)

        tabs[[1]] <- tabPanel("All",
          tags$ul(style="font-size: 0.85em; padding-left: 15px; margin-top: 5px; list-style-type: none;",
            lapply(seq_len(nrow(res_all)), function(i) {
              tags$li(sprintf("%s: %.3f (p=%.3f)", res_all$Label[i], res_all$Corr[i], res_all$Pval[i]))
            })
          )
        )

        cat_dfs <- split(res_df, res_df$Category)
        cat_tabs <- lapply(names(cat_dfs), function(cat) {
          results_cat <- cat_dfs[[cat]]
          results_cat <- results_cat[order(results_cat$AbsCorr, decreasing = TRUE), ]
          res_cat <- head(results_cat, 8)
          
          tabPanel(cat,
            tags$ul(style="font-size: 0.85em; padding-left: 15px; margin-top: 5px; list-style-type: none;",
              lapply(seq_len(nrow(res_cat)), function(i) {
                tags$li(sprintf("%s: %.3f (p=%.3f)", res_cat$Label[i], res_cat$Corr[i], res_cat$Pval[i]))
              })
            )
          )
        })
        
        tabs <- c(list(tabs[[1]]), cat_tabs)
        tagList(
          hr(),
          tags$h6("Predictor Ranks (Correlation):"),
          # Scope stamp: ranks are frozen at Calculate time, so make the data
          # subset they refer to explicit even if the locality selection has
          # changed since (press Calculate again to refresh).
          if (!is.null(rv$cor_scope_label))
            tags$p(style = "font-size: 0.78em; color: #888; margin: 0 0 4px 0;",
                   paste0("Scope: ", rv$cor_scope_label)),
          tags$div(style = "overflow-x: auto; white-space: nowrap; border-bottom: 1px solid #ddd; margin-bottom: 5px;",
            do.call(tabsetPanel, c(list(id = "cor_tabs", type = "pills"), tabs))
          )
        )
      })
    
  # --- TPS Optimization ---
  # Lambda presets: the 0.001-step slider makes the special values -1 (Auto)
  # and 0 (exact interpolation) hard to hit by dragging.
  observeEvent(input$tps_preset_auto,    updateSliderInput(session, "tps_lambda",   value = -1))
  observeEvent(input$tps_preset_exact,   updateSliderInput(session, "tps_lambda",   value = 0))
  observeEvent(input$tps_m_preset_auto,  updateSliderInput(session, "tps_m_lambda", value = -1))
  observeEvent(input$tps_m_preset_exact, updateSliderInput(session, "tps_m_lambda", value = 0))

  tps_opt_vals <- reactiveVal(NULL)
  observeEvent(input$opt_tps, {
    req(rv$user_data, input$var_id, input$method == "TPS")
    locs <- resolve_selected_localities(input$locality, rv$user_data, rv$mapping$loc)
    meta <- get_current_meta(); req(meta)
    
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

            list(l = item$l, best_lam = best_lam, gcv_data = gcv_res, err = NULL)
          }, error = function(e) {
            list(l = item$l, best_lam = NULL, gcv_data = NULL, err = e$message)
          })
        }, .options = furrr::furrr_options(seed = 12345, packages = c("sf", "fields")))
        
        for(res in res_list) {
          l <- res$l
          if(!is.null(res$err)) {
            rv$log <- paste0(rv$log, "\nTPS Opt Error (", l, "): ", res$err)
            tryCatch({ showNotification(paste("TPS Optimization failed for", l, "- using fallback lambda."), type = "warning") }, error=function(e) NULL)
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
  
  # Shared builder for the per-locality optimization summary panels (TPS
  # lambdas / IDW power factors) - same table, different engine and format.
  render_opt_summary_panel <- function(engine, vals_reactive, fmt, heading) {
    renderUI({
      res <- vals_reactive(); if(is.null(res)) return(NULL)

      rows <- lapply(res$locs, function(l) {
        act_val <- get_regional_param(engine, l, "act")
        pre_val <- if("pre" %in% res$targets) get_regional_param(engine, l, "pre") else NA
        tags$tr(
          tags$td(l),
          tags$td(sprintf(fmt, act_val)),
          tags$td(if(is.na(pre_val)) "N/A" else sprintf(fmt, pre_val))
        )
      })

      div(style = "margin-top: 10px; padding: 10px; background-color: #f8f9fa; color: #495057; border: 1px solid #dee2e6; border-radius: 4px; font-size: 0.8em;",
          h5(heading),
          tags$table(class = "table table-condensed table-bordered", style = "background-color: #ffffff; color: #000000;",
            tags$thead(tags$tr(tags$th("Locality"), tags$th("Actual"), tags$th("Predicted"))),
            tags$tbody(rows)
          )
      )
    })
  }

  output$tps_opt_panel <- render_opt_summary_panel("TPS", tps_opt_vals, "%.6f", "Optimization Summary (Best Lambdas):")

  idw_opt_vals <- reactiveVal(NULL)
  observeEvent(input$opt_idw, {
    req(rv$user_data, input$var_id, input$method == "IDW", input$locality)
    
    locs <- resolve_selected_localities(input$locality, rv$user_data, rv$mapping$loc)

    meta <- get_current_meta(); req(meta)
    factors <- seq(0.5, 5.0, by = 0.5)
    
    withProgress(message = "Calculating optimal IDW factors per region...", {
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
    
    all_best <- sapply(locs, function(l) get_regional_param("IDW", l, "act"))
    updateSliderInput(session, "idw_p", value = mean(all_best))
    
    idw_opt_vals(list(locs = locs, targets = targets))
    showNotification(paste("IDW Optimization Complete for:", paste(locs, collapse=", ")), type = "message", duration = 5)
  })
  
  output$idw_opt_panel <- render_opt_summary_panel("IDW", idw_opt_vals, "%.1f", "Optimization Summary (Best Factors):")

    output$idw_metrics_table <- renderTable({
      req(input$method == "IDW", rv$cv_metrics_act)
      m_act <- rv$cv_metrics_act
      if(length(m_act) == 0) return(NULL)
      
      ns <- sapply(m_act, function(x) x$n %||% 0)
      rmses <- sapply(m_act, function(x) x$rmse %||% NA)
      mes <- sapply(m_act, function(x) x$me %||% NA)
      
      total_n <- sum(ns, na.rm=TRUE)
      if(total_n == 0) return(NULL)
      
      avg_rmse <- sqrt(sum(ns * rmses^2, na.rm=TRUE) / total_n)
      avg_me <- sum(ns * mes, na.rm=TRUE) / total_n
      
      data.frame(Metric = c("Mean CV RMSE (Pooled)", "Mean Bias (ME)"), Value = c(round(avg_rmse, 4), round(avg_me, 4)))
    })
  observeEvent(list(input$locality, rv$user_data, rv$mapping$loc), {
    req(input$locality, rv$user_data, rv$mapping$loc)
    locs <- resolve_selected_localities(input$locality, rv$user_data, rv$mapping$loc)

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

  observeEvent(input$auto_fit, {
    req(rv$user_data, input$locality, rv$mapping$x, rv$mapping$y)
    locs <- resolve_selected_localities(input$locality, rv$user_data, rv$mapping$loc)
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


  calculate_run_estimates <- function() {
    meta <- get_current_meta()
    req(meta)
    
    loc_col <- rv$mapping$loc
    selected_locs <- resolve_selected_localities(input$locality, rv$user_data, loc_col)
    n_locs <- length(selected_locs)
    if (n_locs == 0) n_locs <- 1
    
    comp_mode <- isTruthy(input$comp_mode) || isTruthy(input$value_type != "actual")
    # mirror the nested-worker topology the run pipeline actually uses
    cores <- tryCatch(as.integer(future::availableCores()), error = function(e) 1L)
    if (is.null(cores) || is.na(cores) || cores < 1) cores <- 1L
    cores <- if (n_locs > 1) max(1L, min(cores - 1L, n_locs)) else 1L
    
    loc_sample_counts <- numeric(n_locs)
    if (length(selected_locs) > 0 && !is.null(rv$user_data) && !is.null(loc_col) && loc_col %in% colnames(rv$user_data)) {
      for (idx in seq_along(selected_locs)) {
        l <- selected_locs[idx]
        n_samples <- nrow(rv$user_data[rv$user_data[[loc_col]] == l, ])
        loc_sample_counts[idx] <- if (is.null(n_samples) || is.na(n_samples) || n_samples == 0) 50 else n_samples
      }
    } else {
      loc_sample_counts <- rep(50, n_locs)
    }
    
    est_res <- estimate_run_duration(loc_sample_counts, input$method, comp_mode, cores)
    
    return(list(
      meta = meta,
      n_locs = n_locs,
      estimate_text = est_res$estimate_text,
      is_long_run = est_res$is_long_run
    ))
  }

  # Archived registries hold wrapped rasters, so an unbounded archive
  # grows RAM run after run; keep only the most recent few.
  MAX_RUN_HISTORY <- 5L
  push_run_history <- function(entry, base = NULL) {
    hist <- c(list(entry), if (is.null(base)) rv$run_history else base)
    if (length(hist) > MAX_RUN_HISTORY) {
      n_drop <- length(hist) - MAX_RUN_HISTORY
      hist <- hist[seq_len(MAX_RUN_HISTORY)]
      showNotification(paste0("Run archive limit (", MAX_RUN_HISTORY, ") reached: ", n_drop,
                              " oldest archived run(s) discarded to free memory."),
                       type = "warning", duration = 8)
    }
    rv$run_history <- hist
  }

  archive_and_proceed <- function(action, meta, n_locs, estimate_text, is_long_run) {
    if (action == "archive") {
      current_cfg <- rv$run_config_summary
      current_reg <- rv$export_registry
      if (!is.null(current_cfg) && length(current_reg) > 0) {
        push_run_history(list(config = current_cfg, registry = current_reg))
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

  observeEvent(input$vif_drop_btn, {
    removeModal()
    rv$vif_choice_made <- 10
    rv$proceed_vif <- runif(1)
  })

  observeEvent(input$vif_keep_btn, {
    removeModal()
    rv$vif_choice_made <- Inf
    rv$proceed_vif <- runif(1)
  })

  # Locality is part of the reset list because the VIF screen below runs on
  # the SELECTED localities' data: a drop/keep decision made for one spatial
  # context must not silently carry over to another.
  observeEvent(list(input$method, input$aux_vars, input$locality), {
    rv$vif_choice_made <- NULL
  })

  observeEvent(input$run, {
    if (isTRUE(rv$model_running)) {
      showNotification("A model run is already in progress.", type = "warning")
      return()
    }
    req(rv$user_data, input$locality, rv$mapping$x, rv$mapping$y)
    
    if (input$method %in% c("RK", "RFK", "CK") && (is.null(input$aux_vars) || length(input$aux_vars) == 0)) {
      showNotification("Please select at least one auxiliary variable for RK/RFK/CK model generation.", type = "error")
      return()
    }

    if (input$method %in% c("RK", "RFK", "CK") && length(input$aux_vars) > 1 && is.null(rv$vif_choice_made)) {
       # Screen multicollinearity on the data the run will actually fit (the
       # selected localities), not the full table: covariates can be collinear
       # within one locality but not across all of them, and vice versa.
       df_vif <- sf::st_drop_geometry(rv$user_data)
       if (!is.null(rv$mapping$loc) && rv$mapping$loc %in% colnames(df_vif) &&
           length(input$locality) > 0 && !("ALL" %in% input$locality)) {
         df_vif <- df_vif[as.character(df_vif[[rv$mapping$loc]]) %in% input$locality, , drop = FALSE]
       }
       df_aux <- df_vif[, input$aux_vars, drop = FALSE]
       vif_res <- check_vif(df_aux, threshold = 10)

       if (length(vif_res$dropped) > 0) {
          showModal(modalDialog(
            title = tags$div(style = "color: #d9534f; font-weight: bold;", icon("exclamation-triangle"), "High Multicollinearity Detected"),
            tags$p("High correlation / multicollinearity detected among the selected variables within the selected localities. This may destabilize the spatial estimation model."),
            tags$p(tags$b("Variables recommended to be dropped:"), paste(vif_res$dropped, collapse=", ")),
            tags$p("What would you like to do?"),
            footer = tagList(
              actionButton("vif_drop_btn", "Auto-Drop and Continue", class = "btn-success"),
              actionButton("vif_keep_btn", "Keep All (Not Recommended)", class = "btn-warning"),
              modalButton("Cancel")
            ),
            easyClose = FALSE
          ))
          return()
       }
    }
    
    rv$proceed_vif <- runif(1)
  })

  observeEvent(rv$proceed_vif, {
    vif_thresh <- if (!is.null(rv$vif_choice_made)) rv$vif_choice_made else 10
    rv$vif_choice_made <- NULL
    rv$active_vif_thresh <- vif_thresh
    
    est <- calculate_run_estimates()
    meta <- est$meta
    n_locs <- est$n_locs
    estimate_text <- est$estimate_text
    is_long_run <- est$is_long_run
    
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
      archive_and_proceed("none", meta, n_locs, estimate_text, is_long_run)
    }
  })

  observeEvent(input$archive_prev_run, {
    removeModal()
    if (isTRUE(input$auto_archive_remember)) {
      rv$auto_archive_choice <- "archive"
    }
    
    est <- calculate_run_estimates()
    meta <- est$meta
    n_locs <- est$n_locs
    estimate_text <- est$estimate_text
    is_long_run <- est$is_long_run
    
    archive_and_proceed("archive", meta, n_locs, estimate_text, is_long_run)
  })

  observeEvent(input$discard_prev_run, {
    removeModal()
    if (isTRUE(input$auto_archive_remember)) {
      rv$auto_archive_choice <- "discard"
    }
    
    est <- calculate_run_estimates()
    meta <- est$meta
    n_locs <- est$n_locs
    estimate_text <- est$estimate_text
    is_long_run <- est$is_long_run
    
    archive_and_proceed("discard", meta, n_locs, estimate_text, is_long_run)
  })

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

  # Where a model run actually executes. input$run first passes through the
  # archive-confirmation gate and then flips rv$proceed_run; this observer picks
  # it up, validates the coordinate mapping, builds the per-locality point sets,
  # and dispatches the interpolation to the future_promise pipeline so the UI
  # stays responsive while localities are processed.
  observeEvent(rv$proceed_run, {
    if (isTRUE(rv$model_running)) {
      showNotification("A model run is already in progress.", type = "warning")
      return()
    }
    req(rv$user_data, input$locality, rv$mapping$x, rv$mapping$y);
    meta <- get_current_meta()
    req(meta)

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

    locs <- resolve_selected_localities(input$locality, rv$user_data, rv$mapping$loc)

    if (!is.null(rv$run_config_summary) && rv$run_config_summary$method != input$method) {
      rv$v_fit_list <- list()
    }

    # Switch tabs client-side: shinyjs messages reach the browser immediately,
    # whereas updateTabsetPanel queues an input message that is only flushed
    # after this whole observer (validation + data prep + future dispatch)
    # finishes - the pan would lag several seconds and yank the user back if
    # they had already navigated elsewhere in the meantime.
    shinyjs::runjs("$('#main_tabs a[data-value=\"tab_map\"]').tab('show');")

    shinyjs::disable("run")
    # Label swap must go through updateActionButton: Shiny 1.13 renders the
    # label into a .action-label child span, and raw shinyjs::html() here
    # destroys that span, so later updateActionButton restores (cancel/finish)
    # would APPEND their label next to the stale text instead of replacing it.
    updateActionButton(session, "run", label = "Interpolating...", icon = icon("spinner", class = "fa-spin"))

    shinyjs::show("map_processing_overlay")
    shinyjs::show("map_spinner")
    shinyjs::show("map_progress_bar_container")
    shinyjs::show("cancel_model_btn")
    shinyjs::hide("reveal_maps_btn")
    shinyjs::html("map_processing_title", "Processing...")
    update_premium_progress(5, "Initializing Spatial Analysis Engine...")

    cancel_file <- file.path(session_progress_dir, "cancel_flag.txt")
    if (file.exists(cancel_file)) tryCatch(file.remove(cancel_file), error = function(e) NULL)
    old_files <- list.files(path = session_progress_dir, pattern = paste0("^progress_", session_id, "_.*_.*\\.txt$"), full.names = TRUE)
    if (length(old_files) > 0) tryCatch(file.remove(old_files), error = function(e) NULL)
    rv$model_running <- TRUE

    rv$run_counter <- rv$run_counter + 1L
    clear_raster_caches()
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

    # Committed display context: everything the Map Viewer and Scientific
    # Analysis tabs render is keyed to this snapshot (via get_display_meta or
    # rv$disp directly), never to the live sidebar inputs, so reconfiguring
    # the sidebar for the next run cannot alter the displayed results.
    # Superset of get_current_meta()'s fields so it is a drop-in replacement.
    rv$disp <- c(meta, list(
      var_id = meta$actual,
      method = input$method,
      value_type = input$value_type,
      comp_mode = isTRUE(input$comp_mode),
      localities = locs
    ))

    tryCatch({
      rv$export_registry <- list()
      rv$rast_list_act <- list(); rv$rast_list_pre <- list(); sf_list <- list(); b_list <- list()
      rv$rast <- NULL; rv$rast_pred <- NULL; rv$rast_res <- NULL; rv$has_predictions <- FALSE
    rv$v_emp_list <- list(); rv$log <- paste0("[Run #", rv$run_counter, "] Starting spatial interpolation using method: ", input$method, "...")
    rv$model_summaries <- list(); rv$rf_models <- list(); rv$gstat_objs <- list()
    rv$cv_metrics_act <- list(); rv$cv_metrics_pre <- list() # Reset CV metrics
    rv$cv_data_act <- list(); rv$cv_data_pre <- list()
    rv$cv_strategy_sel <- input$cv_strategy %||% "auto"
    
    update_premium_progress(15, "Validating and Cleaning Spatial Input Data...")
    
    pred_col <- if(input$value_type == "pred_ss") meta$pred_ss else meta$pred
    aux_vars <- input$aux_vars
    
    update_premium_progress(25, "Preparing Neighborhood Search Grids...")
    
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
      subset_col <- find_subset_column(colnames(sub_df))
      if (val_type == "pred_ss" && !is.na(subset_col) && subset_val != "all") {
        sub_df <- sub_df[!is.na(sub_df[[subset_col]]) & sub_df[[subset_col]] == subset_val, , drop = FALSE]
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
        pre_fit_pre = clean_gstat_env(if(sep_fit) rv$v_fit_list[[paste0(l, "_pre")]] else rv$v_fit_list[[paste0(l, "_act")]]),
        cv_strategy = input$cv_strategy %||% "auto",
        rfk_uncertainty = input$rfk_uncertainty %||% "jackknife"
      )

      list(l = l, pts_data = pts_data, m_params = m_params)
    })

    # Snapshot the per-locality method params this run actually consumes so
    # display/export tables report them; the live tuning store holds no entry
    # for localities that fell back to the global slider value.
    rv$disp$regional_params <- setNames(
      lapply(df_list, function(item) item$m_params[c("idw_p_act", "idw_p_pre", "tps_lambda_act", "tps_lambda_pre")]),
      vapply(df_list, function(item) item$l, character(1))
    )

    update_premium_progress(50, "Executing Parallel Interpolation Algorithms...")

    rv$rast_list_act <- list(); rv$rast_list_pre <- list(); rv$rast_list_res <- list(); rv$rast_list_point_res <- list()

    main_wd <- getwd()
    progress_dir_val <- session_progress_dir
    session_id_val <- session_id
    cancel_file_val <- file.path(session_progress_dir, "cancel_flag.txt")

    rv$run_token <- rv$run_token + 1L
    this_token <- rv$run_token

    log_start_time <- Sys.time()
    log_method <- current_method
    log_comp_mode <- comp_mode
    log_n_locs <- length(df_list)
    log_sample_counts <- sapply(df_list, function(x) nrow(x$pts_data))
    
    vif_thresh_local <- rv$active_vif_thresh

    # Single source of truth for every helper the interpolation worker
    # needs. future_promise ships them into the worker's global env via
    # `globals` (robust_vgm_fit / clean_gstat_env live in monolith.R and
    # get_buffer_multiplier in ui_helpers.R, so the worker's
    # source("spatial_helpers.R") alone does NOT define them); the inner
    # furrr::future_map re-declares the same names for any nested workers.
    interp_globals <- c(
      "run_regional_interpolation", "calc_scientific_lags", "robust_vgm_fit",
      "apply_interpolation", "apply_OK", "apply_RK", "apply_RFK", "apply_CK",
      "apply_IDW", "apply_TPS", "perform_kriging_loocv", "safe_run_cv",
      "optimize_idw_p", "clean_gstat_env", "make_cv_folds", "resolve_cv_plan",
      "apply_kriging_pipeline", "check_vif", "krige_covariates", "get_buffer_multiplier",
      "sanitize_spatial_predictions", "validate_and_project_sf", "dedup_valid_points",
      "suggest_lmc_model", ".cv_to_df", "detect_cv_columns"
    )
    # Materialize the helpers ONCE here in the main session. A plain named
    # list referenced by symbol is shipped reliably by automatic globals
    # detection; future_promise does NOT honor future's structure(TRUE, add=)
    # globals idiom: the add= names silently never reach the worker. mget()
    # also fails fast if a helper is missing.
    interp_helper_values <- mget(interp_globals, inherits = TRUE)

    # Nested futures default to a sequential plan, so without an explicit
    # escalation all localities run one after another inside the single
    # future_promise worker. The nested worker count is decided HERE in the
    # main session (availableCores() introspection inside a PSOCK worker is
    # unreliable) and shipped into the worker as plain data. Numerics are
    # plan-independent: furrr's fixed seed assigns one L'Ecuyer stream per
    # locality regardless of topology.
    cores_hint <- tryCatch(as.integer(future::availableCores()), error = function(e) 1L)
    nested_workers <- if (length(df_list) > 1L) max(1L, min(cores_hint - 1L, length(df_list))) else 1L
    # record the ACTUAL parallelism in run_history.csv so the duration
    # estimator calibrates against what really happened
    log_cores <- nested_workers

    promises::future_promise({
      setwd(main_wd)
      # Define the helpers in the worker's GLOBAL env: functions sourced from
      # spatial_helpers.R resolve robust_vgm_fit & co. through their
      # enclosure (the worker's globalenv), not through the future's eval env
      list2env(interp_helper_values, envir = globalenv())
      source("spatial_helpers.R", local = FALSE)

      nested_cl <- NULL
      old_mc_cores <- getOption("mc.cores")
      if (nested_workers >= 2L && future::nbrOfWorkers() == 1L) {
        # PSOCK workers report mc.cores = 1; the main session allocated
        # nested_workers cores to this batch, so tell parallelly before
        # spawning or its worker-count guard misfires. Owning the cluster
        # explicitly (instead of plan(multisession)) guarantees a clean
        # teardown in the finally block below.
        options(mc.cores = nested_workers)
        nested_cl <- parallelly::makeClusterPSOCK(nested_workers)
        future::plan(future::cluster, workers = nested_cl)
      }

      # Exact export set for the nested workers, as a named list: neither
      # furrr nor future_promise honors future's structure(TRUE, add=)
      # globals idiom, and the bare character-vector form omits the run
      # parameters.
      furrr_globals <- c(
        interp_helper_values,
        list(main_wd = main_wd,
             current_method = current_method, current_crs = current_crs, aux_vars = aux_vars,
             shp_bound = shp_bound, b_type = b_type, buff_mode = buff_mode, b_dist = b_dist,
             res_mode = res_mode, grid_res = grid_res, crs_sel = crs_sel,
             comp_mode = comp_mode, val_type = val_type,
             progress_dir_val = progress_dir_val, session_id_val = session_id_val,
             cancel_file_val = cancel_file_val, vif_thresh_local = vif_thresh_local)
      )

      tryCatch({
        furrr::future_map(df_list, function(item) {
          # Nested workers are fresh processes: define the FULL helper set the
          # same way the promise worker does. The globals list above only
          # ships the entry points plus the helpers living outside
          # spatial_helpers.R (robust_vgm_fit, clean_gstat_env,
          # get_buffer_multiplier), not every internal spatial_helpers
          # function they call.
          source(file.path(main_wd, "spatial_helpers.R"), local = FALSE)
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
            cancel_file_val = cancel_file_val,
            vif_threshold = vif_thresh_local
          )
        }, .options = furrr::furrr_options(
          seed = 12345,
          globals = furrr_globals,
          # packages used UNQUALIFIED in the helper call graph; namespaced
          # calls (terra::, FNN::, randomForest::, ...) need no attaching
          packages = c("sf", "gstat", "dplyr")
        ))
      }, finally = {
        # tear the nested cluster down and restore mc.cores so the (reused)
        # promise worker returns to the plain single-threaded state other
        # future_promise tasks expect
        if (!is.null(nested_cl)) {
          future::plan(future::sequential)
          parallel::stopCluster(nested_cl)
          options(mc.cores = old_mc_cores)
        }
      })
    }, seed = 12345) %...>% (function(res_all) {
      if (this_token != rv$run_token) return()
      
      tryCatch({
        batch_elapsed_sec <- as.numeric(difftime(Sys.time(), log_start_time, units = "secs"))
        history_dir <- "run_history"
        if (!dir.exists(history_dir)) dir.create(history_dir, recursive = TRUE, showWarnings = FALSE)
        history_file <- file.path(history_dir, "run_history.csv")
        
        total_samples <- sum(log_sample_counts)
        per_locality_share <- if (total_samples > 0) {
          batch_elapsed_sec * (log_sample_counts / total_samples)
        } else {
          rep(batch_elapsed_sec / log_n_locs, log_n_locs)
        }
        
        new_rows <- data.frame(
          timestamp = format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
          method = log_method,
          comp_mode = log_comp_mode,
          n_locs_in_batch = log_n_locs,
          n_samples = log_sample_counts,
          cores_used = log_cores,
          batch_elapsed_sec = batch_elapsed_sec,
          per_locality_share_sec = per_locality_share,
          stringsAsFactors = FALSE
        )
        
        if (file.exists(history_file)) {
          write.table(new_rows, history_file, append = TRUE, sep = ",", row.names = FALSE, col.names = FALSE)
        } else {
          write.table(new_rows, history_file, append = FALSE, sep = ",", row.names = FALSE, col.names = TRUE)
        }
      }, error = function(e) {})
      
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
      
      temp_rast_a <- terra::unwrap(rv$rast)
      if ("var1.var" %in% names(temp_rast_a)) {
        uncert_var_a <- temp_rast_a[["var1.var"]]
        register_export_item("map_uncert_var_act", paste(meta$label, "- Uncertainty Map (Variance - Actual)"), "map", terra::wrap(uncert_var_a), meta$category, kind = "uncertainty")
        register_export_item("map_uncert_se_act", paste(meta$label, "- Uncertainty Map (SE - Actual)"), "map", terra::wrap(sqrt(uncert_var_a)), meta$category, kind = "uncertainty")
      }
    }
    if(length(valid_p) > 0) {
      rv$rast_pred <- merge_wrapped_rasters(valid_p)
      rv$has_predictions <- TRUE
      register_export_item("map_predicted", paste(meta$label, "- Predicted Map"), "map", rv$rast_pred, meta$category)
      
      temp_rast_p <- terra::unwrap(rv$rast_pred)
      if ("var1.var" %in% names(temp_rast_p)) {
        uncert_var_p <- temp_rast_p[["var1.var"]]
        register_export_item("map_uncert_var_pre", paste(meta$label, "- Uncertainty Map (Variance - Predicted)"), "map", terra::wrap(uncert_var_p), meta$category, kind = "uncertainty")
        register_export_item("map_uncert_se_pre", paste(meta$label, "- Uncertainty Map (SE - Predicted)"), "map", terra::wrap(sqrt(uncert_var_p)), meta$category, kind = "uncertainty")
      }
    }
    if(length(valid_r) > 0) {
      rv$rast_res <- merge_wrapped_rasters(valid_r)
      register_export_item("map_residuals", paste(meta$label, "- ML Predictions Residual Map (Delta)"), "map", rv$rast_res, meta$category, kind = "residual")
    }
    if(length(valid_pr) > 0) {
      rv$rast_point_res <- merge_wrapped_rasters(valid_pr)
      register_export_item("map_interp_point_errors", paste(meta$label, "- ML Predictions Interpolated Point Errors Map"), "map", rv$rast_point_res, meta$category, kind = "residual")
    }
    
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
    # Signals "this run's results are now in rv$..." to the cached Scientific
    # Analysis plots (their cache keys embed it).
    rv$results_rev <- rv$results_rev + 1L

    # Point error map: the discrete sample-location errors the Map Viewer's
    # Point Residuals panel shows, exported as points (not the IDW surface,
    # which is registered separately above as Interpolated Point Errors).
    if (!is.null(rv$sf) && "resid" %in% colnames(rv$sf) && any(!is.na(rv$sf$resid))) {
      pts_err <- rv$sf[!is.na(rv$sf$resid), c("resid", "loc")]
      register_export_item("map_point_residuals", paste(meta$label, "- ML Predictions Point Error Map"),
                           "map", list(pts = pts_err, bound = rv$bound), meta$category, kind = "residual")
    }
    
    if (!is.null(rv$bound)) {
      tryCatch({
        bbox <- sf::st_bbox(sf::st_transform(sf::st_as_sf(rv$bound), 4326))
        leafletProxy("main_map") %>% fitBounds(as.numeric(bbox$xmin), as.numeric(bbox$ymin), as.numeric(bbox$xmax), as.numeric(bbox$ymax))
        if (comp_mode || val_type != "actual") {
          leafletProxy("comp_map_left") %>% fitBounds(as.numeric(bbox$xmin), as.numeric(bbox$ymin), as.numeric(bbox$xmax), as.numeric(bbox$ymax))
          leafletProxy("comp_map_right") %>% fitBounds(as.numeric(bbox$xmin), as.numeric(bbox$ymin), as.numeric(bbox$xmax), as.numeric(bbox$ymax))
        }
      }, error = function(e) NULL)
    }

    if(!is.null(rv$sf)) {
      df_met <- rv$sf %>% st_drop_geometry() %>% filter(!is.na(v), !is.na(pv))
      if(nrow(df_met) > 0) {
        resids <- df_met$v - df_met$pv
        rmse_val <- sqrt(mean(resids^2))
        r2_val <- cor(df_met$v, df_met$pv)^2
        mbe_val <- mean(df_met$pv - df_met$v)
        nse_val <- 1 - sum(resids^2) / sum((df_met$v - mean(df_met$v))^2)
        
        global_metrics <- data.frame(
          Metric = c("RMSE (Avg Error)", "R2 (Correlation)", "R2 (Traditional)", "MBE (Bias)"),
          Value = c(round(rmse_val, 4), round(r2_val, 4), round(nse_val, 4), round(mbe_val, 4))
        )
        register_export_item("table_global_metrics", paste(meta$label, "- Global Performance Metrics"), "table", global_metrics, meta$category)
      }
    }
    # NOTE: intentionally no get_current_meta() re-read here - the export
    # labels below must use the meta captured at dispatch, not whatever the
    # sidebar points at when the run finishes.

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
      
      v_all <- rv$sf$v[!is.na(rv$sf$v)]
      if(length(v_all) > 0) {
        s_a <- summary(v_all)
        stats_total <- data.frame(Metric = names(s_a), Value = as.character(round(as.numeric(s_a), 3)))
        register_export_item("table_stats_total", paste(meta$label, "- Total Descriptive Statistics (Actual)"), "table", stats_total, meta$category)
      }
      
      if(comp_mode || val_type != "actual") {
        pv_all <- rv$sf$pv[!is.na(rv$sf$pv)]
        if(length(pv_all) > 0) {
          s_p <- summary(pv_all)
          stats_total_p <- data.frame(Metric = names(s_p), Value = as.character(round(as.numeric(s_p), 3)))
          register_export_item("table_stats_pre_total", paste(meta$label, "- Total Descriptive Statistics (Predicted)"), "table", stats_total_p, meta$category)
        }
      }
      
      params_k <- tryCatch(agro_params(), condition = function(c) NULL)
      if(!is.null(params_k) && (comp_mode || val_type != "actual")) {
        df_k <- df_perf
        brks_k <- c(-Inf, params_k$rcl_mat[-1, 1], Inf)
        # right = FALSE matches the map classification (terra::classify with
        # right = FALSE): classes are [low, high)
        df_k$act_bin <- cut(df_k$v, breaks = brks_k, labels = params_k$labels, include.lowest = TRUE, right = FALSE)
        df_k$pred_bin <- cut(df_k$pv, breaks = brks_k, labels = params_k$labels, include.lowest = TRUE, right = FALSE)
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
          register_export_item("table_kappa_total", paste(meta$label, "- Total Classification Performance - Map in Agro or Binned styling to see the stats"), "table", kappa_total, meta$category)
        }
      }
    }

    if(isTruthy(input$color_style %in% c("agro", "bin")) && !is.null(rv$rast)) {
       area_total <- area_df_total_act()
       if(is.data.frame(area_total)) register_export_item("table_area_total", paste(meta$label, "- Total Area Coverage"), "table", area_total, meta$category)
    }

    for(l in locs) {
       register_locality_assets(l, meta, comp_mode, val_type, current_method)
    }
    
    rv$log <- paste0(rv$log, "\n\n--- Run #", rv$run_counter, " Complete ---",
      "\nConfig: ", rv$run_config_summary$method, " | ", rv$run_config_summary$variable,
      " | ", rv$run_config_summary$localities,
      "\n", rv$run_config_summary$method_params)

    shinyjs::hide("map_spinner")
    shinyjs::html("map_processing_title", "Map Generation Complete")
    update_premium_progress(100, "Click below to reveal the updated geostatistical surfaces.")
    shinyjs::show("reveal_maps_btn")
    
    shinyjs::enable("run")
    updateActionButton(session, "run", label = "Interpolated", icon = icon("check"))
    
    rv$model_running <- FALSE
    old_files <- list.files(path = session_progress_dir, pattern = paste0("^(progress|warn)_", session_id, "_.*_.*\\.txt$"), full.names = TRUE)
    if(length(old_files) > 0) tryCatch(file.remove(old_files), error = function(e) NULL)
    }) %...!% (function(err) {
      if (this_token != rv$run_token) return()
      shinyjs::hide("map_spinner")
      shinyjs::hide("map_progress_bar_container")
      shinyjs::hide("cancel_model_btn")
      shinyjs::hide("reveal_maps_btn")
      
      shinyjs::enable("run")
      updateActionButton(session, "run", label = "Run Interpolation", icon = character(0))
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
      
      shinyjs::enable("run")
      updateActionButton(session, "run", label = "Run Interpolation", icon = character(0))
      shinyjs::runjs("$('#run i').remove();")
    })
    
    NULL
  })

  observe({
    req(rv$model_running)
    # Everything about the run in progress comes from the committed context
    # (rv$disp), never the live sidebar: changing the locality selection while
    # a run executes must not move the expected-model count under the bar.
    d <- rv$disp
    selected_locs <- if (!is.null(d)) d$localities else NULL
    n_locs_calc <- max(1L, length(selected_locs))
    poll_interval <- if (n_locs_calc < 5) 250 else if (n_locs_calc <= 20) 1000 else 2000
    invalidateLater(poll_interval)

    comp_mode <- !is.null(d) && (isTRUE(d$comp_mode) || !identical(d$value_type, "actual"))
    expected_models <- n_locs_calc * (if(comp_mode) 2 else 1)

    files <- list.files(path = session_progress_dir, pattern = paste0("^progress_", session_id, "_.*_.*\\.txt$"), full.names = TRUE)
    if(length(files) > 0) {
      vals <- vapply(files, function(f) {
        val <- tryCatch(as.numeric(readLines(f, warn = FALSE)), error = function(e) NA_real_)
        if(length(val) == 0 || is.na(val)) 0 else val
      }, numeric(1))

      # Defensive denominator: if an engine fails before its progress file
      # exists, averaging over the full expectation would park the bar short
      # of its cap until the completion handler resolves.
      avg_pct <- sum(vals, na.rm = TRUE) / max(1L, min(expected_models, length(files)))
      
      bar_width <- 50 + (avg_pct * 0.5)
      bar_width <- max(50, min(99, bar_width)) # Cap at 99% until complete handler resolves
      
      update_premium_progress(bar_width)
      
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

  observeEvent(input$cancel_model_btn, {
    cancel_file <- file.path(session_progress_dir, "cancel_flag.txt")
    file.create(cancel_file)
    rv$model_running <- FALSE
    rv$run_token <- rv$run_token + 1L
    
    shinyjs::hide("map_spinner")
    shinyjs::hide("map_progress_bar_container")
    shinyjs::hide("cancel_model_btn")
    shinyjs::hide("reveal_maps_btn")
    
    shinyjs::html("map_processing_title", "Interpolation Cancelled")
    shinyjs::html("map_progress_text", HTML("Please configure parameters in the left panel and click <b>'Run Interpolation'</b> to generate geostatistical maps and review diagnostic results."))
    
    showNotification("Model generation cancelled by user.", type = "warning")
    
    old_files <- list.files(path = session_progress_dir, pattern = paste0("^(progress|warn)_", session_id, "_.*_.*\\.txt$"), full.names = TRUE)
    if(length(old_files) > 0) tryCatch(file.remove(old_files), error = function(e) NULL)
    
    shinyjs::enable("run")
    updateActionButton(session, "run", label = "Run Interpolation", icon = character(0))
    shinyjs::runjs("$('#run i').remove();")
  })

  observeEvent(input$reveal_maps_btn, {
    shinyjs::hide("map_processing_overlay")
    showNotification("Maps and scientific analysis metrics are now available.", type = "message")
    
    updateActionButton(session, "run", label = "Run Interpolation", icon = character(0))
    shinyjs::runjs("$('#run i').remove();")
  })

  observeEvent(input$resid_info_btn, {
    showModal(modalDialog(
      title = "Residual Mapping & Diagnostics",
      size = "l",
      easyClose = TRUE,
      tags$div(
        h4("Mathematical Formula"),
        p(HTML("<b>Residual = Observed value (v) - ML Predicted value (pv)</b>")),
        p("A residual is the deviation of your machine learning model from the actual measured value at a given location. Both columns come from your uploaded dataset: the observed measurements and the predictions of the external ML model you supplied (e.g. Actual Nitrogen - ML Predicted Nitrogen). Residuals therefore diagnose that ML model's error, not the error of the interpolation performed in this dashboard."),
        hr(),
        h4("Available Residual Types"),
        tags$ul(
          tags$li(tags$b("Interpolated Delta (Surface Diff):"), " Calculated by subtracting the entire Predicted surface from the Actual surface [interpolate(Actual) - interpolate(Predicted)]. This shows the net difference between the two mapped geostatistical surfaces."),
          tags$li(tags$b("Point Errors:"), " The discrete error at each individual sample point location [Observed - Predicted], displayed as coloured markers at the exact sampling positions (right map of the Residuals view, and the 'Point Error Map' in the Export Panel)."),
          tags$li(tags$b("Interpolated Point Errors (Model Error):"), " The same local errors interpolated (IDW) into a continuous surface, available in the Export Panel as the 'Interpolated Point Errors Map'. This specifically maps the spatial structure of the model's inability to capture local variation.")
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
        if (res_mode_val == "fixed") {
          base_res <- manual_res_val
        } else {
          if (!is.numeric(x)) return("-")
          base_res <- x
        }
        
        if (buff_mode_val == "dynamic" && input$boundary_type == "wrapped") {
          val <- get_buffer_multiplier(method_val) * base_res
          val <- max(5, min(2000, val))
          paste0(round(val, 1), " m")
        } else {
          paste0(fixed_dist, " m")
        }
      })
    }
    df
  }, striped = TRUE, hover = TRUE, bordered = TRUE, width = "100%")

  # draw_map only builds what requires re-encoding raster layers (run
  # results, value type, classification/palette/uncertainty). Cheap overlays
  # (styled points, borders, north arrow, scale, resolution box, base tiles)
  # are applied by leafletProxy observers keyed on map_overlay_rev, so
  # toggling them does not re-render the whole widget.
  draw_map <- function(r_obj, lab) {
    current_tiles <- isolate(input$base_map_layer) %||% "Esri.WorldImagery"
    
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
    meta <- get_display_meta()
    req(meta)

    if(!is.null(r_obj) && !(is.list(r_obj) && length(r_obj) == 0)) {
      r_list <- if(inherits(r_obj, "SpatRaster") || inherits(r_obj, "PackedSpatRaster")) list(r_obj) else r_obj
      r_list <- Filter(Negate(is.null), r_list)
      
      if (length(r_list) > 0) {
        vgm_target <- if (lab %in% c("actual", "Actual")) "act"
                      else if (lab %in% c("pred", "pred_ss", "Predicted")) "pre"
                      else NULL  # residual maps derive from both fits
        vgm_warn_html <- build_vgm_warning_html(rv$v_fit_list, target = vgm_target)
        if (!is.null(vgm_warn_html)) {
          m <- m %>% addControl(html = vgm_warn_html, position = "bottomleft")
        }
        r_names <- names(r_list)
        layer_key <- function(i) {
          r_name <- if (!is.null(r_names) && length(r_names) >= i && !is.na(r_names[i]) && r_names[i] != "") r_names[i] else as.character(i)
          paste0(rv$run_counter, "_", lab, "_", r_name)
        }
        select_active_layer <- function(r_w) {
          is_uncertainty <- isTruthy(input$show_uncertainty) && meta$method %in% c("OK", "RK", "RFK", "CK") && "var1.var" %in% names(r_w)
          if (is_uncertainty) {
            al <- r_w[["var1.var"]]
            if (input$uncertainty_type == "se") sqrt(al) else al
          } else {
            if("var1.pred" %in% names(r_w)) r_w[["var1.pred"]] else r_w[[1]]
          }
        }
        vv_list <- lapply(seq_along(r_list), function(i) {
          r_proj <- get_projected_raster(r_list[[i]], layer_key(i))
          if (is.null(r_proj)) return(NULL)
          as.vector(values(select_active_layer(r_proj), na.rm=TRUE))
        })
        vv <- unlist(vv_list)
        vv_scale <- joint_vv() %||% vv
        
        is_viridis <- meta$palette == "viridis"
        is_uncert_view <- isTruthy(input$show_uncertainty) && meta$method %in% c("OK", "RK", "RFK", "CK")
        legend_title <- if (is_uncert_view) {
          if (input$uncertainty_type == "se") {
            paste0("SE: ", meta$label, if (nzchar(meta$unit)) paste0(" ", meta$unit) else "")
          } else {
            paste0("Variance: ", meta$label, if (nzchar(meta$unit)) paste0(" (", meta$unit, ")^2") else " (squared units)")
          }
        } else paste(meta$label, meta$unit)
        if(lab == "resid_raster") {
          # The residual view always displays the var1.pred difference, so the
          # palette domain must come from that layer too (vv would hold the
          # var1.var difference when show_uncertainty is on)
          resid_layers <- list()
          for (i in seq_along(r_list)) {
            r_w <- get_projected_raster(r_list[[i]], layer_key(i))
            if (is.null(r_w)) next
            resid_layers[[length(resid_layers) + 1]] <- if("var1.pred" %in% names(r_w)) r_w[["var1.pred"]] else r_w[[1]]
          }
          vv_resid <- unlist(lapply(resid_layers, function(al) as.vector(values(al, na.rm = TRUE))))
          abs_max <- max(abs(vv_resid), na.rm = TRUE)
          if(is.infinite(abs_max) || is.na(abs_max)) abs_max <- 1
          pal <- colorNumeric("RdBu", domain = c(-abs_max, abs_max), na.color = "transparent")

          for (al in resid_layers) {
            m <- m %>% addRasterImage(al, colors = pal, opacity = 0.8)
          }
          m <- m %>% leaflet::addLegend(pal = pal, values = c(-abs_max, abs_max), title = paste("Resid:", meta$label))
        } else if(input$color_style == "agro" && !is_uncert_view) {
          params <- agro_params()
          if(!is.null(params)) {
            pal <- colorBin(params$colors, bins = params$brks, na.color = "transparent", right = FALSE)
            
            for (i in seq_along(r_list)) {
              r_w <- get_projected_raster(r_list[[i]], layer_key(i))
              if (is.null(r_w)) next
              m <- m %>% addRasterImage(select_active_layer(r_w), colors = pal, opacity = 0.8)
            }
            m <- m %>% leaflet::addLegend(colors = params$colors, labels = params$leg_labels, opacity = 0.8, title = paste(meta$label, meta$unit))
          }
        } else if(input$color_style == "bin" && !is_uncert_view) {
          params <- classification_params()
          if(!is.null(params)) {
            pal <- colorBin(params$colors, bins = params$brks, na.color = "transparent", right = FALSE)
            
            for (i in seq_along(r_list)) {
              r_w <- get_projected_raster(r_list[[i]], layer_key(i))
              if (is.null(r_w)) next
              m <- m %>% addRasterImage(select_active_layer(r_w), colors = pal, opacity = 0.8)
            }
            m <- m %>% leaflet::addLegend(colors = params$colors, labels = params$leg_labels, opacity = 0.8, title = paste(meta$label, meta$unit))
          }
        } else {
          pal <- if(is_viridis) colorNumeric(viridis::viridis(256, option = meta$palette), vv_scale, na.color = "transparent") 
                 else colorNumeric(meta$palette, vv_scale, na.color = "transparent")
                 
          for (i in seq_along(r_list)) {
            r_w <- get_projected_raster(r_list[[i]], layer_key(i))
            if (is.null(r_w)) next
            m <- m %>% addRasterImage(select_active_layer(r_w), colors = pal, opacity = 0.8)
          }

          v_range <- diff(range(vv_scale, na.rm=TRUE))
          d_format <- if(is.na(v_range)) 2 else if(v_range < 0.01) 6 else if(v_range < 0.1) 4 else 2
          m <- m %>% leaflet::addLegend(pal = pal, values = vv_scale, title = legend_title, labFormat = labelFormat(digits = d_format))
        }
      }
    }

    
    if(lab == "resid_points") {
       req(rv$sf, "resid" %in% colnames(rv$sf))
       pts_view <- st_transform(rv$sf, 4326)
       abs_max_p <- max(abs(pts_view$resid), na.rm=T)
       if(is.infinite(abs_max_p) || is.na(abs_max_p)) abs_max_p <- 1
       pal_pts <- colorNumeric("RdBu", domain = c(-abs_max_p, abs_max_p), na.color = "black")
       df_clean <- st_drop_geometry(pts_view)
       popup_builder <- make_popup_fn(colnames(df_clean))
       popups <- vapply(seq_len(nrow(df_clean)), function(i) popup_builder(df_clean[i, ]), character(1))
       
       m <- m %>% addCircleMarkers(data = pts_view, radius = 5, color = "black", weight = 1,
                                  fillColor = ~pal_pts(resid), fillOpacity = 0.9,
                                  popup = popups)
       m <- m %>% leaflet::addLegend(pal = pal_pts, values = c(-abs_max_p, abs_max_p), title = paste("Point Resid:", meta$label))
    }
    
    m
  }

  # --- proxy-managed overlays (no raster re-encode on toggle) ---

  # Run points reprojected for Leaflet, cached on rv$sf only: styling ticks
  # (marker size slider, label toggles, palette) reuse the projected object
  # instead of re-running st_transform on every invalidation.
  pts_view_4326 <- reactive({
    pts <- rv$sf
    if (is.null(pts) || nrow(pts) == 0) return(NULL)
    tryCatch(st_transform(pts, 4326), error = function(e) NULL)
  })

  # Styled sampling points + labels + their legend
  observe({
    map_overlay_rev()
    show <- isTRUE(input$show_points_viewer)
    is_resid <- identical(input$map_view, "view_resid")

    pts_view <- NULL
    popup_fn <- NULL
    if (show) {
      pts_view <- pts_view_4326()
      if (!is.null(pts_view)) popup_fn <- make_popup_fn(colnames(st_drop_geometry(pts_view)))
    }

    for (map_id in overlay_map_ids) {
      proxy <- leafletProxy(map_id) %>%
        clearGroup("styled_points") %>%
        clearGroup("styled_labels") %>%
        removeControl("styled_points_legend")
      # Same rule as the old draw_map: no styled points on the residual
      # comparison maps (resid_raster / resid_points views)
      eligible <- map_id == "main_map" || !is_resid
      if (eligible && !is.null(pts_view)) {
        add_styled_points(proxy, pts_view,
          color_by = input$pt_color_by %||% "none",
          custom_colors = rv$pt_style_colors,
          show_labels = isTRUE(input$pt_show_labels),
          label_field = input$pt_label_field %||% "none",
          label_size = input$pt_label_size %||% 11,
          marker_size = input$pt_marker_size %||% 3,
          popup_fn = popup_fn,
          legend_layer_id = "styled_points_legend"
        )
      }
    }
  })

  # Boundary outlines
  observe({
    map_overlay_rev()
    bound_4326 <- if (isTRUE(input$show_borders) && !is.null(rv$bound)) {
      tryCatch(st_transform(st_as_sf(rv$bound), 4326), error = function(e) NULL)
    } else NULL
    for (map_id in overlay_map_ids) {
      proxy <- leafletProxy(map_id) %>% clearGroup("bound_borders")
      if (!is.null(bound_4326)) {
        proxy %>% addPolygons(data = bound_4326, fill = FALSE, color = "white", weight = 2, group = "bound_borders")
      }
    }
  })

  # North arrow
  north_arrow_html <- "<div style='text-align: center; color: white; font-family: Arial, sans-serif; pointer-events: none;'><div style='font-size: 16px; font-weight: bold; line-height: 1; margin-bottom: 4px; text-shadow: 1px 1px 2px black;'>N</div><svg width='30' height='30' viewBox='0 0 24 24' style='filter: drop-shadow(1px 1px 2px black);'><polygon points='12,2 7,22 12,17 17,22' fill='#e74c3c' stroke='white' stroke-width='1.5'/><polygon points='12,2 7,22 12,17' fill='#c0392b' stroke='white' stroke-width='1.5'/></svg></div>"
  observe({
    map_overlay_rev()
    show <- isTRUE(input$show_north)
    for (map_id in overlay_map_ids) {
      proxy <- leafletProxy(map_id) %>% removeControl("north_ctrl")
      if (show) proxy %>% addControl(html = north_arrow_html, position = "topleft", layerId = "north_ctrl")
    }
  })

  # Per-locality resolution box
  observe({
    map_overlay_rev()
    show <- isTRUE(input$show_res_overlay) && length(rv$loc_resolutions) > 0
    res_html <- if (show) {
      paste0("<div style='background:white; padding:5px; border-radius:4px; border: 1px solid #ccc; font-size:12px; font-family:sans-serif;'><b>Resolutions:</b><br>", paste(names(rv$loc_resolutions), sapply(rv$loc_resolutions, function(x) round(x,2)), sep=": ", collapse="<br>"), "</div>")
    } else NULL
    for (map_id in overlay_map_ids) {
      proxy <- leafletProxy(map_id) %>% removeControl("res_overlay_ctrl")
      if (!is.null(res_html)) proxy %>% addControl(html = res_html, position = "bottomright", layerId = "res_overlay_ctrl")
    }
  })

  # Distance scale. The control lives in the external #distance_scale_container:
  # it is moved (and styled) there right after creation, so no DOM polling is
  # needed, and it is toggled with plain JS on the live map instances instead
  # of a re-render.
  observe({
    map_overlay_rev()
    show <- isTRUE(input$show_scale)
    shinyjs::runjs(sprintf("
      setTimeout(function() {
        var show = %s;
        var c = document.getElementById('distance_scale_container');
        if (c) c.innerHTML = '';
        ['main_map','comp_map_left','comp_map_right'].forEach(function(id) {
          var el = document.getElementById(id);
          if (!el || el.offsetParent === null) return;
          var w = HTMLWidgets.find('#' + id);
          if (!w || !w.getMap) return;
          var map = w.getMap();
          if (map._monolithScale) { try { map.removeControl(map._monolithScale); } catch(e) {} map._monolithScale = null; }
          if (show) {
            map._monolithScale = L.control.scale({position: 'bottomleft', metric: true, imperial: false}).addTo(map);
            var sc = map._monolithScale.getContainer();
            if (c && sc) {
              c.appendChild(sc);
              sc.style.background = 'white';
              sc.style.padding = '5px';
              sc.style.border = '1px solid #ccc';
              sc.style.borderRadius = '4px';
              sc.style.margin = '0 auto';
            }
          }
        });
      }, 400);
    ", if (show) "true" else "false"))
  })

  # View switcher for the Map Viewer: offers only the surfaces the committed
  # run actually computed, so switching views is instant (no recompute) and a
  # context change in the sidebar can never blank the displayed map.
  # Re-renders only when a run is dispatched/completed, defaulting to the view
  # implied by the committed run configuration.
  output$map_view_ui <- renderUI({
    req(rv$disp)
    choices <- c("View: Actual" = "view_act")
    if (length(rv$rast_list_pre) > 0) {
      choices <- c(choices,
                   "View: ML Predicted" = "view_pred",
                   "View: Actual vs Predicted" = "view_comp")
    }
    if (length(rv$rast_list_res) > 0) choices <- c(choices, "View: ML Residuals" = "view_resid")

    d <- isolate(rv$disp)
    default_view <- if (identical(d$value_type, "resid")) "view_resid"
      else if (isTRUE(d$comp_mode) && d$value_type %in% c("pred", "pred_ss")) "view_comp"
      else if (d$value_type %in% c("pred", "pred_ss")) "view_pred"
      else "view_act"
    if (!default_view %in% choices) default_view <- "view_act"

    selectInput("map_view", NULL, choices = choices, selected = default_view, width = "210px", selectize = FALSE)
  })
  # keep the view choices in sync even while the Map Viewer tab is hidden -
  # the layout conditionalPanels depend on input$map_view being current
  outputOptions(output, "map_view_ui", suspendWhenHidden = FALSE)

  disp_method_label <- function(d) {
    if (is.null(d$method)) "" else paste0(" (", get_method_label(d$method), ")")
  }
  disp_pred_label <- function(d, long = FALSE) {
    if (identical(d$value_type, "pred_ss")) {
      if (long) "Single Split ML Predictions View (_ss)" else "Single Split ML Predictions (_ss)"
    } else {
      if (long) "Best ML Predictions View (_cve)" else "Best ML Predictions (_cve)"
    }
  }

  output$main_map_title <- renderText({
    d <- rv$disp; req(d)
    type_lab <- if (identical(input$map_view, "view_pred")) disp_pred_label(d, long = TRUE) else "Actual Data View"
    paste0(d$label, " - ", type_lab, disp_method_label(d))
  })

  output$comp_left_title <- renderText({
    d <- rv$disp; req(d)
    if (identical(input$map_view, "view_resid")) return(paste0(d$label, " - Interpolated Residuals", disp_method_label(d)))
    paste0(d$label, " - Actual Data", disp_method_label(d))
  })

  output$comp_right_title <- renderText({
    d <- rv$disp; req(d)
    if (identical(input$map_view, "view_resid")) return(paste0(d$label, " - Point Residuals", disp_method_label(d)))
    paste0(d$label, " - ", disp_pred_label(d), disp_method_label(d))
  })

  observeEvent(input$base_map_layer, {
    # draw_map reads the tile choice under isolate(), so this proxy swap is
    # the only path that updates tiles on an already-rendered map
    for (map_id in overlay_map_ids) {
      leafletProxy(map_id) %>%
        clearTiles() %>%
        addProviderTiles(input$base_map_layer, layerId="base_tiles", options = providerTileOptions(zIndex = -10))
    }
  })

  observeEvent(input$refresh_map_area, {
    req(input$base_map_layer)
    leafletProxy("main_map") %>%
      clearTiles() %>%
      addProviderTiles(input$base_map_layer, layerId="base_tiles", options = providerTileOptions(zIndex = -10))
    
    leafletProxy("comp_map_left") %>%
      clearTiles() %>%
      addProviderTiles(input$base_map_layer, layerId="base_tiles", options = providerTileOptions(zIndex = -10))
      
    leafletProxy("comp_map_right") %>%
      clearTiles() %>%
      addProviderTiles(input$base_map_layer, layerId="base_tiles", options = providerTileOptions(zIndex = -10))
      
    if (!is.null(rv$bound)) {
      tryCatch({
        bbox <- sf::st_bbox(sf::st_transform(sf::st_as_sf(rv$bound), 4326))
        leafletProxy("main_map") %>% fitBounds(as.numeric(bbox$xmin), as.numeric(bbox$ymin), as.numeric(bbox$xmax), as.numeric(bbox$ymax))
        if (isTRUE(input$map_view %in% c("view_comp", "view_resid"))) {
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
        if (isTRUE(input$map_view %in% c("view_comp", "view_resid"))) {
          leafletProxy("comp_map_left") %>% fitBounds(as.numeric(bbox$xmin), as.numeric(bbox$ymin), as.numeric(bbox$xmax), as.numeric(bbox$ymax))
          leafletProxy("comp_map_right") %>% fitBounds(as.numeric(bbox$xmin), as.numeric(bbox$ymin), as.numeric(bbox$xmax), as.numeric(bbox$ymax))
        }
      }, error = function(e) NULL)
    }
      
    shinyjs::runjs("setTimeout(function() { window.dispatchEvent(new Event('resize')); }, 100);")
  })

  output$main_map <- renderLeaflet({
    d <- rv$disp; req(d)
    view <- input$map_view %||% "view_act"
    req(view %in% c("view_act", "view_pred"))
    target <- if (view == "view_pred") rv$rast_list_pre else rv$rast_list_act
    view_lab <- if (view == "view_pred") {
      if (identical(d$value_type, "pred_ss")) "pred_ss" else "pred"
    } else "actual"
    m <- draw_map(target, view_lab)
    session_state$main_map_rendered <- TRUE
    map_overlay_rev(isolate(map_overlay_rev()) + 1L)
    m
  })

  output$comp_map_left <- renderLeaflet({
    req(rv$disp, input$map_view %in% c("view_comp", "view_resid"))
    m <- if(input$map_view == "view_resid") {
      draw_map(rv$rast_list_res, "resid_raster")
    } else {
      draw_map(rv$rast_list_act, "Actual")
    }
    session_state$comp_maps_rendered <- TRUE
    map_overlay_rev(isolate(map_overlay_rev()) + 1L)
    m
  })

  output$comp_map_right <- renderLeaflet({
    req(rv$disp, input$map_view %in% c("view_comp", "view_resid"))
    m <- if(input$map_view == "view_resid") {
      draw_map(NULL, "resid_points")
    } else {
      draw_map(rv$rast_list_pre, "Predicted")
    }
    session_state$comp_maps_rendered <- TRUE
    map_overlay_rev(isolate(map_overlay_rev()) + 1L)
    m
  })
      output$locality_pan_ui <- renderUI({
      req(rv$loc_names)
      render_locality_pan_input(rv$loc_names)
      })

      observeEvent(input$locality_pan, {
      req(input$locality_pan, rv$user_data, rv$mapping$x, rv$mapping$y, rv$mapping$crs)

      bbox <- if (input$locality_pan == "global") {
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

      leafletProxy("main_map") %>% fitBounds(as.numeric(bbox$xmin), as.numeric(bbox$ymin), as.numeric(bbox$xmax), as.numeric(bbox$ymax))

      if (isTRUE(input$map_view %in% c("view_comp", "view_resid"))) {
        leafletProxy("comp_map_left") %>% fitBounds(as.numeric(bbox$xmin), as.numeric(bbox$ymin), as.numeric(bbox$xmax), as.numeric(bbox$ymax))
        leafletProxy("comp_map_right") %>% fitBounds(as.numeric(bbox$xmin), as.numeric(bbox$ymin), as.numeric(bbox$xmax), as.numeric(bbox$ymax))
      }
   })

   # Manual-mode slider values feed a live overlay line on the fitted
   # variogram plots; they belong in the cache key only while they actually
   # affect the plot (the short-circuit reads mirror the plot's own logic).
   vgm_manual_overlay_key <- function(target) {
     if (identical(input$vgm_mode, "manual") && identical(input$sel_loc_stats, input$m_loc)) {
       applies <- if (target == "act") {
         is.null(input$m_target) || input$m_target == "act"
       } else {
         identical(input$m_target, "pre")
       }
       if (applies) return(list(input$m_psill, input$k_mod, input$m_range, input$m_nugget))
     }
     NULL
   }

   output$vgm_plot_main <- renderCachedPlot({
     loc <- input$sel_loc_stats; meta <- get_display_meta()
     req(loc, meta)
     if(loc == "Total (Combined)") {
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
   }, cacheKeyExpr = {
     loc <- input$sel_loc_stats
     list("vgm_main", loc, rv$results_rev, rv$disp$actual,
          rv$v_emp_list[[paste0(loc, "_act")]], rv$v_fit_list[[paste0(loc, "_act")]],
          vgm_manual_overlay_key("act"))
   }, cache = "session")
   output$vgm_plot_pred <- renderCachedPlot({
     loc <- input$sel_loc_stats; meta <- get_display_meta()
     req(loc, meta)
     if(loc == "Total (Combined)") {
       pred_col <- if(identical(meta$value_type, "pred_ss")) meta$pred_ss else meta$pred
       
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
   }, cacheKeyExpr = {
     loc <- input$sel_loc_stats
     list("vgm_pred", loc, rv$results_rev, rv$disp$actual, rv$disp$value_type,
          rv$v_emp_list[[paste0(loc, "_pre")]], rv$v_fit_list[[paste0(loc, "_pre")]],
          vgm_manual_overlay_key("pre"))
   }, cache = "session")

  # RK linear-trend panels: fit-statistic chips + coefficient table (raw
  # summary.lm print kept behind a collapsible details element). Falls back to
  # the verbatim print if the stored object is not a summary.lm.
  output$model_summary_ui_act <- renderUI({
    loc <- input$sel_loc_stats; req(loc)
    if (loc == "Total (Combined)") {
      return(div(style="padding: 12px; background-color: #f8f9fa; border: 1px dashed #ced4da; border-radius: 6px; color: #6c757d; font-style: italic; text-align: center;",
                 "Linear trend summaries are computed per locality. Please select a specific locality from the analysis filter list above to view details."))
    }
    summary_obj <- rv$model_summaries[[paste0(loc, "_act")]]
    req(summary_obj)
    build_rk_trend_ui(summary_obj, "rk_coef_dt_act", "summ_act_static") %||%
      tagList(verbatimTextOutput("summ_act_static"))
  })
  output$model_summary_ui_pre <- renderUI({
    loc <- input$sel_loc_stats; req(loc)
    if (loc == "Total (Combined)") {
      return(div(style="padding: 12px; background-color: #f8f9fa; border: 1px dashed #ced4da; border-radius: 6px; color: #6c757d; font-style: italic; text-align: center;",
                 "Linear trend summaries are computed per locality. Please select a specific locality from the analysis filter list above to view details."))
    }
    summary_obj <- rv$model_summaries[[paste0(loc, "_pre")]]
    req(summary_obj)
    build_rk_trend_ui(summary_obj, "rk_coef_dt_pre", "summ_pre_static") %||%
      tagList(verbatimTextOutput("summ_pre_static"))
  })

  output$rk_coef_dt_act <- DT::renderDataTable({
    loc <- input$sel_loc_stats
    req(loc, loc != "Total (Combined)")
    summary_obj <- rv$model_summaries[[paste0(loc, "_act")]]
    req(summary_obj)
    df <- rk_coef_table(summary_obj, rv$mapping$vars)
    req(df)
    sci_dt(df)
  })

  output$rk_coef_dt_pre <- DT::renderDataTable({
    loc <- input$sel_loc_stats
    req(loc, loc != "Total (Combined)")
    summary_obj <- rv$model_summaries[[paste0(loc, "_pre")]]
    req(summary_obj)
    df <- rk_coef_table(summary_obj, rv$mapping$vars)
    req(df)
    sci_dt(df)
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

  output$rf_importance_plot_act <- renderCachedPlot({
    loc <- input$sel_loc_stats; req(loc)
    if (loc == "Total (Combined)") {
      return(ggplot() + annotate("text", x = 4, y = 4, label = "RF Variable Importance is generated per locality.\nPlease select a specific locality from the dropdown.", size = 5, color = "grey40") + theme_void())
    }
    req(rv$rf_models[[paste0(loc, "_act")]])
    randomForest::varImpPlot(rv$rf_models[[paste0(loc, "_act")]], main = paste("Variable Importance (Actual):", loc))
  }, cacheKeyExpr = {
    # is.null() stands in for the model object itself (too heavy to hash);
    # rv$results_rev separates runs, the null flag catches the dispatch reset.
    loc <- input$sel_loc_stats
    list("rf_imp_act", loc, rv$results_rev, is.null(rv$rf_models[[paste0(loc, "_act")]]))
  }, cache = "session")
  output$rf_importance_plot_pre <- renderCachedPlot({
    loc <- input$sel_loc_stats; req(loc)
    if (loc == "Total (Combined)") {
      return(ggplot() + annotate("text", x = 4, y = 4, label = "RF Variable Importance is generated per locality.\nPlease select a specific locality from the dropdown.", size = 5, color = "grey40") + theme_void())
    }
    req(rv$rf_models[[paste0(loc, "_pre")]])
    randomForest::varImpPlot(rv$rf_models[[paste0(loc, "_pre")]], main = paste("Variable Importance (Predicted):", loc))
  }, cacheKeyExpr = {
    loc <- input$sel_loc_stats
    list("rf_imp_pre", loc, rv$results_rev, is.null(rv$rf_models[[paste0(loc, "_pre")]]))
  }, cache = "session")
  
  render_internal_vgm_plot <- function(type) {
    renderCachedPlot({
      loc <- input$sel_loc_stats; req(loc)
      title_suffix <- if (type == "act") "(Actual)" else "(Predicted)"
      col_resid <- if (type == "act") "model_resid_act" else "model_resid_pre"
      
      if (loc == "Total (Combined)") {
        req(rv$sf, col_resid %in% colnames(rv$sf))
        formula_obj <- as.formula(paste(col_resid, "~ 1"))
        df_filtered <- rv$sf[!is.na(rv$sf[[col_resid]]), ]
        # Unlike the per-locality plots (internal trend-residual variogram of
        # the fitted model), the combined view pools CV residuals across
        # localities, so label it as such.
        p_res <- plot(variogram(formula_obj, df_filtered), main = list(label = paste("Pooled CV Residual Variogram", title_suffix), cex = 0.85), scales = list(cex = 0.75))
        print(p_res)
      } else {
        req(rv$v_emp_list[[paste0(loc, "_", type)]], rv$v_fit_list[[paste0(loc, "_", type)]])
        p_res <- plot(rv$v_emp_list[[paste0(loc, "_", type)]], rv$v_fit_list[[paste0(loc, "_", type)]],
             main = list(label = paste("Internal Residual Variogram", paste0(title_suffix, ":"), loc), cex = 0.85), scales = list(cex = 0.75))
        print(p_res)
      }
    }, cacheKeyExpr = {
      loc <- input$sel_loc_stats
      list("internal_vgm", type, loc, rv$results_rev,
           rv$v_emp_list[[paste0(loc, "_", type)]], rv$v_fit_list[[paste0(loc, "_", type)]])
    }, cache = "session")
  }

  output$rk_internal_vgm_act  <- render_internal_vgm_plot("act")
  output$rk_internal_vgm_pre  <- render_internal_vgm_plot("pre")
  output$rfk_internal_vgm_act <- render_internal_vgm_plot("act")
  output$rfk_internal_vgm_pre <- render_internal_vgm_plot("pre")

  render_ck_variogram_plot <- function(type) {
    renderCachedPlot({
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
    }, cacheKeyExpr = {
      loc <- input$sel_loc_stats
      list("ck_vgm", type, loc, rv$results_rev, is.null(rv$gstat_objs[[paste0(loc, "_", type)]]))
    }, cache = "session")
  }

  output$ck_variogram_plot_act <- render_ck_variogram_plot("act")
  output$ck_variogram_plot_pred <- render_ck_variogram_plot("pre")

  output$vgm_params_table <- DT::renderDataTable({
    loc <- input$sel_loc_stats; req(loc)

    get_vgm_params <- function(f) {
      if(is.null(f)) return(rep("NA", 5))
      mod <- as.character(f$model[2])
      nug <- f$psill[1]
      sill <- sum(f$psill)
      rng <- f$range[2]
      str_dep <- if(sill > 0) ((sill - nug) / sill) * 100 else 0
      c(mod, round(nug, 4), round(sill, 4), round(rng, 1), paste0(round(str_dep, 1), "%"))
    }

    if(loc == "Total (Combined)") {
      # Variograms are fitted per locality; the combined view lists every
      # fitted locality (one row per fitted target) instead of showing nothing
      fits <- rv$v_fit_list
      locs <- unique(sub("_(act|pre)$", "", names(fits)))
      rows <- list()
      for (l in locs) {
        for (tgt in c("act", "pre")) {
          f <- fits[[paste0(l, "_", tgt)]]
          if(is.null(f)) next
          pr <- get_vgm_params(f)
          rows[[length(rows) + 1]] <- data.frame(
            Locality = l, Target = if(tgt == "act") "Actual" else "Predicted",
            Model = pr[1], Nugget = pr[2], Sill = pr[3], Range = pr[4],
            Structural.Dep. = pr[5], check.names = FALSE)
        }
      }
      if(length(rows) == 0) return(NULL)
      res <- do.call(rbind, rows)
      names(res)[7] <- "Structural Dep."
      return(sci_dt(res))
    }

    f_a <- rv$v_fit_list[[paste0(loc, "_act")]]; f_p <- rv$v_fit_list[[paste0(loc, "_pre")]]
    if(is.null(f_a) && is.null(f_p)) return(NULL)

    sci_dt(data.frame(Param = c("Model", "Nugget", "Sill", "Range", "Structural Dep."),
                      Actual = get_vgm_params(f_a),
                      Predicted = get_vgm_params(f_p)))
  })
  output$tps_gcv_plot_act <- renderCachedPlot({
    loc <- input$sel_loc_stats; req(loc, identical(rv$disp$method, "TPS"))
    tryCatch({
      build_tps_gcv_plot(rv$tps_gcv_data, loc, "act")
    }, error = function(e) {
      plot(1, 1, type="n", main=paste("GCV Plot Error:", e$message), axes=F, xlab="", ylab="")
    })
  }, cacheKeyExpr = {
    # tps_gcv_data also changes outside runs (the Optimize button), so the
    # whole (small) list is part of the key.
    list("tps_gcv_act", input$sel_loc_stats, rv$results_rev, rv$disp$method, rv$tps_gcv_data)
  }, cache = "session")

  build_obs_pred_plot <- function(df, title, x_lab = "Observed", y_lab = "Predicted") {
    req(df, nrow(df) > 0)
    
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

  output$obs_pred_plot_act <- renderCachedPlot({
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
  }, cacheKeyExpr = {
    list("obs_pred_act", input$sel_loc_stats, rv$results_rev, names(rv$cv_data_act))
  }, cache = "session")

  output$resid_vgm_plot_act <- render_resid_plot(reactive(rv$cv_data_act), "")

  output$obs_pred_plot_pre <- renderCachedPlot({
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
  }, cacheKeyExpr = {
    list("obs_pred_pre", input$sel_loc_stats, rv$results_rev, names(rv$cv_data_pre))
  }, cache = "session")

  output$resid_vgm_plot_pre <- render_resid_plot(reactive(rv$cv_data_pre), "(Predicted Map)")

  output$tps_gcv_plot_pre <- renderCachedPlot({
    loc <- input$sel_loc_stats; req(loc, identical(rv$disp$method, "TPS"))
    tryCatch({
      build_tps_gcv_plot(rv$tps_gcv_data, loc, "pre")
    }, error = function(e) {
      plot(1, 1, type="n", main=paste("GCV Plot Error:", e$message), axes=F, xlab="", ylab="")
    })
  }, cacheKeyExpr = {
    list("tps_gcv_pre", input$sel_loc_stats, rv$results_rev, rv$disp$method, rv$tps_gcv_data)
  }, cache = "session")

  output$regional_params_table <- DT::renderDataTable({
    loc <- input$sel_loc_stats; req(loc, (rv$disp$method %||% "") %in% c("IDW", "TPS"))
    has_pre <- isTRUE(rv$disp$comp_mode) || !identical(rv$disp$value_type, "actual")
    sci_dt(build_regional_params_df(rv$disp$method, loc, rv$disp$regional_params, has_pre))
  })

  output$stats_table_total <- DT::renderDataTable({
    req(rv$user_data)
    meta <- get_display_meta()
    req(meta)
    
    df <- rv$user_data
    # "Total (Combined)" means the localities covered by the DISPLAYED run,
    # so a partial-locality run summarises only the data it interpolated
    # (matching the run-scoped area and CV tables in this panel)
    loc_col <- rv$mapping$loc
    if (!is.null(meta$localities) && !is.null(loc_col) && loc_col %in% colnames(df)) {
      df <- df %>% filter(!!sym(loc_col) %in% meta$localities)
    }
    v_act <- if(!is.null(meta$actual) && !is.na(meta$actual) && meta$actual %in% colnames(df)) df[[meta$actual]] else NULL
    if (is.null(v_act)) return(NULL)

    v_pre <- if(!is.null(meta$pred) && !is.na(meta$pred) && meta$pred %in% colnames(df)) df[[meta$pred]] else if(!is.null(meta$pred_ss) && !is.na(meta$pred_ss) && meta$pred_ss %in% colnames(df)) df[[meta$pred_ss]] else NULL

    s_a <- summary(v_act)
    res <- data.frame(Metric = names(s_a), Total_Actual = as.character(round(as.numeric(s_a), 3)))
    
    if(!is.null(v_pre)) {
      s_p <- summary(v_pre)
      res$Total_Predicted <- as.character(round(as.numeric(s_p), 3))
    }
    sci_dt(res)
  })

  output$stats_table_loc <- DT::renderDataTable({
    req(rv$user_data, input$sel_loc_stats)
    if(input$sel_loc_stats == "Total (Combined)") return(NULL)
    meta <- get_display_meta()
    req(meta)
    
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
    sci_dt(res)
  })

  calc_area_df <- function(r_obj, r_id = NULL) {
    if(is.null(r_obj)) return(NULL)
    if(inherits(r_obj, "PackedSpatRaster")) r_obj <- terra::unwrap(r_obj)
    params <- tryCatch(classification_params(), condition = function(c) NULL)
    if(is.null(params)) return(data.frame(Status = "Awaiting Classification Params"))
    
    if (!is.null(r_id)) {
      brk_str <- if (!is.null(params) && !is.null(params$brks)) paste(params$brks, collapse = "_") else "nobrks"
      cache_key <- paste0(rv$run_counter, "_", r_id, "_", brk_str)
      if (exists(cache_key, envir = area_calc_cache)) {
        return(get(cache_key, envir = area_calc_cache))
      }
    }
    
    tryCatch({
      r_class <- classify(r_obj[[1]], params$rcl_mat, right = FALSE)
      
      area_df <- as.data.frame(expanse(r_class, unit = "ha", byValue = TRUE))
      
      class_names <- if(isTruthy(input$color_style == "bin")) params$leg_labels else params$labels
      full_res <- data.frame(value = as.numeric(1:params$n_c), Class = class_names)
      
      if(!"value" %in% names(area_df)) {
        res_df <- data.frame(Class = class_names, Ha = 0)
        if (!is.null(r_id)) {
          assign(cache_key, res_df, envir = area_calc_cache)
        }
        return(res_df)
      }
      
      is_label <- any(as.character(area_df$value) %in% class_names)
      if (is_label) {
         area_df$value <- match(as.character(area_df$value), class_names)
      } else {
         area_df$value <- as.numeric(as.character(area_df$value))
      }
      
      area_df <- area_df[!is.na(area_df$value), ]
      
      area_df <- area_df %>%
        group_by(value) %>%
        summarise(Ha = round(sum(area, na.rm = TRUE), 2), .groups = "drop")

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

  output$area_table_total_act <- DT::renderDataTable({ req(input$color_style %in% c("agro", "bin")); sci_dt(area_df_total_act()) })
  output$area_table_total_pre <- DT::renderDataTable({ req(input$color_style %in% c("agro", "bin")); sci_dt(area_df_total_pre()) })

  output$area_table_loc_act <- DT::renderDataTable({
    req(rv$rast_list_act, input$color_style %in% c("agro", "bin")); loc <- input$sel_loc_stats
    if(loc == "Total (Combined)") return(NULL) else sci_dt(calc_area_df(rv$rast_list_act[[loc]], paste0("loc_act_", loc)))
  })
  output$area_table_loc_pre <- DT::renderDataTable({
    req(rv$rast_list_pre, input$color_style %in% c("agro", "bin")); loc <- input$sel_loc_stats
    if(loc == "Total (Combined)") return(NULL) else sci_dt(calc_area_df(rv$rast_list_pre[[loc]], paste0("loc_pre_", loc)))
  })

  output$cv_strategy_badge <- renderUI({
    req(length(rv$cv_metrics_act) > 0)
    strat <- rv$cv_strategy_sel %||% "auto"
    label <- switch(strat,
      "loocv" = "Standard LOOCV (full leave-one-out)",
      "block" = "Spatial Block CV (10 k-means folds; LOOCV below n=30)",
      "Auto (LOOCV for n ≤ 50, random 10-fold above)")
    tags$div(
      style = "font-size: 0.82em; color: #495057; margin: -4px 0 8px 0;",
      tags$span(style = "font-weight: 600;", "Cross-validation: "),
      tags$span(label),
      tags$span(style = "color: #868e96;", " (applies to these metrics only, not the map).")
    )
  })

  output$metrics_table <- DT::renderDataTable({
    req(input$sel_loc_stats)
    loc <- input$sel_loc_stats
    
    get_metrics_df <- function(cv_list, data_list, label) {
      if(loc == "Total (Combined)") {
        all_cv <- do.call(rbind, lapply(data_list, function(x) {
          if(inherits(x, "sf")) {
            x_proj <- sf::st_transform(x, 3857)
            coords <- sf::st_coordinates(x_proj)
            df <- sf::st_drop_geometry(x_proj)
            if(!"x" %in% colnames(df)) df$x <- coords[,1]
            if(!"y" %in% colnames(df)) df$y <- coords[,2]
            return(df)
          } else if(inherits(x, "Spatial")) {
            x_sf <- sf::st_as_sf(x) %>% sf::st_transform(3857)
            coords <- sf::st_coordinates(x_sf)
            df <- sf::st_drop_geometry(x_sf)
            if(!"x" %in% colnames(df)) df$x <- coords[,1]
            if(!"y" %in% colnames(df)) df$y <- coords[,2]
            return(df)
          } else {
            return(as.data.frame(x))
          }
        }))
        if(is.null(all_cv) || nrow(all_cv) == 0) {
          empty_df <- data.frame(Source=paste0(label, " (pooled CV)"), RMSE=NA, R2_Corr=NA, R2_NSE=NA, Bias_ME=NA, RPD_Prec=NA, SMAPE_Pct=NA, Moran_I=NA)
          names(empty_df) <- c("Source", "RMSE", "R2 (Corr)", "R2 (NSE/Trad)", "Bias (ME)", "RPD (Prec)", "SMAPE (%)", "Moran's I")
          return(empty_df)
        }

        res <- perform_cv(all_cv)
        src_label <- paste0(label, " (pooled per-locality CV, n=", res$n, ")")
        rmse <- res$rmse
        r2 <- res$r2
        nse <- res$nse
        me <- res$me
        rpd <- res$rpd
        smape <- res$smape
        moran_i <- res$moran_i
      } else {
        res <- cv_list[[loc]]
        n_obs <- if(!is.null(data_list[[loc]])) nrow(data_list[[loc]]) else NA
        src_label <- if(!is.null(res)) {
          paste0(label, " (", cv_type_label(n_obs, rv$cv_strategy_sel), ", n=", res$n, ")")
        } else {
          paste0(label, " (CV)")
        }
        rmse <- if(!is.null(res)) res$rmse else NA
        r2   <- if(!is.null(res)) res$r2 else NA
        nse  <- if(!is.null(res)) res$nse else NA
        me   <- if(!is.null(res)) res$me else NA
        rpd  <- if(!is.null(res)) res$rpd else NA
        smape <- if(!is.null(res)) res$smape else NA
        moran_i <- if(!is.null(res)) res$moran_i else NA
      }
                  res_df <- data.frame(
                    Source = src_label,
                    RMSE = round(rmse, 4),
                    R2_Corr = round(r2, 4),
                    R2_NSE = round(nse, 4),
                    Bias_ME = round(me, 4),
                    RPD_Prec = round(rpd, 4),
                    SMAPE_Pct = round(smape, 4),
                    Moran_I = if(is.na(moran_i)) '<span title="No Spatial Structure Detected">NA*</span>' else as.character(round(moran_i, 4))
                    )
                    names(res_df) <- c("Source", "RMSE", "R2 (Corr)", "R2 (NSE/Trad)", "Bias (ME)", "RPD (Prec)", "SMAPE (%)", "Moran's I")
                    res_df
                    }

    m_act <- get_metrics_df(rv$cv_metrics_act, rv$cv_data_act, "Actual Model")
    if(rv$has_predictions) {
      m_pre <- get_metrics_df(rv$cv_metrics_pre, rv$cv_data_pre, "Predicted Model")
      # escape = FALSE keeps the tooltip-bearing NA* span in the Moran's I column
      sci_dt(rbind(m_act, m_pre), escape = FALSE)
    } else {
      sci_dt(m_act, escape = FALSE)
    }
  })

  output$uploaded_metrics_table <- DT::renderDataTable({
          req(rv$sf, input$sel_loc_stats)
          loc <- input$sel_loc_stats
          
          df <- rv$sf %>% st_drop_geometry() %>% filter(!is.na(v), !is.na(pv))
          if(loc != "Total (Combined)") {
            df <- df %>% filter(loc == !!loc)
          }
          
          if(nrow(df) < 3) return(sci_dt(data.frame(Status = "Not enough data points for numeric metrics.")))
          
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
          
              sci_dt(data.frame(
                Metric = c("R2 (NSE/Traditional)", "R2 (Correlation)", "RMSE", "NRMSE (%)", "MAE", "NMAE (%)", "MBE (Bias)", "Lin's CCC (Agree)", "RPD (Precision)", "RPIQ", "SMAPE (%)"),
                Value = c(round(rsq_trad, 4), round(rsq_val, 4), round(rmse_val, 4), round(nrmse_val, 4), round(mae_val, 4), round(nmae_val, 4), round(mbe_val, 4), round(ccc_val, 4), round(rpd_val, 4), round(rpiq_val, 4), round(smape_val, 4))
              ))        })
  output$kappa_table <- DT::renderDataTable({
    req(rv$sf, input$sel_loc_stats, input$kappa_bin_method)
    
    loc <- input$sel_loc_stats
    
    df <- rv$sf %>% st_drop_geometry() %>% filter(!is.na(v), !is.na(pv))
    if(loc != "Total (Combined)") {
      df <- df %>% filter(loc == !!loc)
    }
    
    if(nrow(df) < 3) return(sci_dt(data.frame(Status = "Not enough data points for Kappa.")))

    if (input$kappa_bin_method == "agro") {
      params <- tryCatch(agro_params(), condition = function(c) NULL)
      if(is.null(params) || input$color_style != "agro") return(sci_dt(data.frame(Status = "Please select Agronomical Classes style for this method.")))
      
      breaks <- c(-Inf, params$rcl_mat[-1, 1], Inf)
      labels <- params$labels
      
      # right = FALSE matches the map classification (terra::classify with
      # right = FALSE): classes are [low, high)
      df$act_bin <- cut(df$v, breaks = breaks, labels = labels, include.lowest = TRUE, right = FALSE)
      df$pred_bin <- cut(df$pv, breaks = breaks, labels = labels, include.lowest = TRUE, right = FALSE)
      
      df <- df[!is.na(df$act_bin) & !is.na(df$pred_bin), ]
      df$act_bin <- factor(df$act_bin, levels = labels)
      df$pred_bin <- factor(df$pred_bin, levels = labels)
      
    } else {
      brks <- unique(quantile(df$v, probs = seq(0, 1, 0.25), na.rm = TRUE))
      if(length(brks) < 2) return(sci_dt(data.frame(Status = "Not enough variance for quartiles.")))
      
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
    
    if(nrow(df) < 3) return(sci_dt(data.frame(Status = "Not enough data after binning.")))
    
    k_unw <- tryCatch(yardstick::kap_vec(df$act_bin, df$pred_bin), error = function(e) NA)
    k_lin <- tryCatch(yardstick::kap_vec(df$act_bin, df$pred_bin, weighting = "linear"), error = function(e) NA)
    acc   <- tryCatch(yardstick::accuracy_vec(df$act_bin, df$pred_bin), error = function(e) NA)
    b_acc <- tryCatch(yardstick::bal_accuracy_vec(df$act_bin, df$pred_bin), error = function(e) NA)
    mcc   <- tryCatch(yardstick::mcc_vec(df$act_bin, df$pred_bin), error = function(e) NA)
    
    off_by_one_acc <- tryCatch(sum(abs(as.integer(df$act_bin) - as.integer(df$pred_bin)) <= 1, na.rm = TRUE) / sum(!is.na(df$act_bin) & !is.na(df$pred_bin)), error = function(e) NA)
    
    sci_dt(data.frame(
      Metric = c("Overall Accuracy", "Balanced Accuracy", "Off-by-one Accuracy", "Matthews Corr. Coef. (MCC)", "Kappa (Unweighted)", "Weighted Kappa (Linear)"),
      Value = c(round(acc, 4), round(b_acc, 4), round(off_by_one_acc, 4), round(mcc, 4), round(k_unw, 4), round(k_lin, 4))
    ))
  })

  output$log_output <- renderText({ rv$log })

  # Keep the Scientific Analysis tables computing while the tab is hidden:
  # a run auto-pans the user to the Map Viewer, and with the default
  # suspend-when-hidden these outputs would only start rendering when the
  # tab is opened - the user then stares at the PREVIOUS run's tables behind
  # Shiny's pale-grey recalculating overlay until the whole burst (pooled CV,
  # area expanse, kappa, ...) finishes. Rendering them in the run-completion
  # flush makes the tab current the moment it is opened. Plots stay
  # suspended: hidden plots re-render on reveal anyway (client sizing).
  for (out_id in c("vgm_params_table", "regional_params_table", "metrics_table",
                   "cv_strategy_badge", "stats_table_total", "stats_table_loc",
                   "area_table_total_act", "area_table_total_pre",
                   "area_table_loc_act", "area_table_loc_pre",
                   "uploaded_metrics_table", "kappa_table",
                   "run_config_display", "log_output",
                   # raw RK summaries live inside a collapsed <details>:
                   # opening it fires no Shiny visibility event, so they must
                   # render eagerly or they would stay blank until reveal
                   "summ_act_static", "summ_pre_static")) {
    outputOptions(output, out_id, suspendWhenHidden = FALSE)
  }

  last_notified_warnings <- reactiveVal(character(0))
  observeEvent(rv$log, {
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


  get_drawn_sf <- reactive({
    polys <- rv$drawn_polygons
    if(length(polys) == 0) return(NULL)
    
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


      }
shinyApp(ui = ui, server = server)
