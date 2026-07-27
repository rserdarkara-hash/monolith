# server_setup.R (sourced with local = TRUE inside server) - session infra,
# raster caches, diagnostics closures, decoupled module wiring, session_state,
# map_overlay_rev/overlay_map_ids and the central `rv` reactiveValues.
# This file MUST be sourced first: every later chunk reads names defined here.

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

  # Display cap for the Leaflet viewer: every styling change re-encodes the
  # raster layers (leaflet reprojects + PNG-encodes on the main thread), so
  # very fine interpolation grids made each styling tick take seconds and
  # blocked the whole UI. DISPLAY-ONLY: exports, area statistics and GeoTIFFs
  # always use the full-resolution rasters.
  LEAFLET_DISPLAY_MAX_CELLS <- 5e5

  get_projected_raster <- function(r, cache_key) {
    if (exists(cache_key, envir = leaflet_proj_cache)) return(get(cache_key, envir = leaflet_proj_cache))
    if (inherits(r, "PackedSpatRaster")) r <- terra::unwrap(r)
    if (inherits(r, "SpatRaster") && terra::ncell(r) > LEAFLET_DISPLAY_MAX_CELLS) {
      fact <- ceiling(sqrt(terra::ncell(r) / LEAFLET_DISPLAY_MAX_CELLS))
      r <- tryCatch(terra::aggregate(r, fact = fact, fun = "mean", na.rm = TRUE), error = function(e) r)
    }
    # Project straight to the Leaflet display CRS (EPSG:3857) and cache that:
    # addRasterImage is then called with project = FALSE, so each styling tick
    # only re-colours + PNG-encodes instead of re-resampling every locality
    # layer (that internal projectRasterForLeaflet call was uncached and ran
    # per layer per tick). Single resample, native -> 3857; the previous path
    # double-resampled via an EPSG:4326 intermediate.
    # The NULL result is cached too, so this notifies once per layer instead
    # of silently leaving the map empty.
    r_proj <- tryCatch(leaflet::projectRasterForLeaflet(r, method = "bilinear"), error = function(e) {
      showNotification(paste("Map layer could not be projected for display:", conditionMessage(e)), type = "error")
      NULL
    })
    assign(cache_key, r_proj, envir = leaflet_proj_cache)
    r_proj
  }

  # Build closure factory for the CV residual variograms; render_resid_plot
  # wraps the same closure in the cached in-page renderer, register_sci_plot
  # reuses it for the expand modal and PNG download.
  build_resid_vgm_diag <- function(cv_data_reactive, title_suffix = "") {
    function() {
      req(input$sel_loc_stats, cv_data_reactive())
      loc <- input$sel_loc_stats
      df_list <- cv_data_reactive()

      if(loc == "Total (Combined)") {
         # Pool in the auto-UTM zone of the combined centroid (metric lags);
         # EPSG:3857 stretched the pooled lag axis by 1/cos(latitude).
         cv_obj <- pool_cv_sf(df_list)
      } else {
         cv_obj <- df_list[[loc]]
      }

      req(cv_obj)

      # Every engine returns CV objects as sf in the locality's projected CRS;
      # anything else has no knowable metric CRS for a variogram.
      req(inherits(cv_obj, "sf") || inherits(cv_obj, "Spatial"))

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
         build_variogram_ggplot(v_res, v_fit,
                                title = paste("Residual Variogram:", loc, title_suffix),
                                subtitle = v_sub)
      }, error = function(e) {
         sci_placeholder(paste("Residual variogram error:\n", e$message), size = 4)
      })
    }
  }

  render_resid_plot <- function(cv_data_reactive, title_suffix, build_fn) {
    renderCachedPlot({
      p <- build_fn(); req(p); p
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
             Metric = c("R2 (Trad)", "R2 (Corr)", "RMSE", "MBE (ML pred - observed)", "CCC", "RPD"),
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
     
     # Variogram exports register the same ggplot builders the Scientific
     # Analysis tab renders (former lattice look retired; numbers unchanged).
     if(!is.null(rv$v_emp_list[[paste0(l, "_act")]])) {
       v_emp <- rv$v_emp_list[[paste0(l, "_act")]]
       v_fit <- rv$v_fit_list[[paste0(l, "_act")]]
       p_vgm <- build_variogram_ggplot(v_emp, v_fit, title = paste("Variogram (Actual):", l))
       register_export_item(paste0("plot_vgm_act_", l), paste(meta$label, "-", l, "- Variogram (Actual)"), "plot", p_vgm, meta$category)
       df_vgm <- as.data.frame(v_emp) %>% select(np, dist, gamma, dir.hor, dir.ver)
       register_export_item(paste0("table_vgm_act_", l), paste(meta$label, "-", l, "- Variogram Data (Actual)"), "table", df_vgm, meta$category)
     }
     if((comp_mode || val_type != "actual") && !is.null(rv$v_emp_list[[paste0(l, "_pre")]])) {
       v_emp_p <- rv$v_emp_list[[paste0(l, "_pre")]]
       v_fit_p <- rv$v_fit_list[[paste0(l, "_pre")]]
       p_vgm_p <- build_variogram_ggplot(v_emp_p, v_fit_p, title = paste("Variogram (Predicted):", l))
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
     
     # RF importance exports reuse the labeled SA-tab builder (metadata labels
     # instead of raw column names; every importance measure gets a panel).
     # The data-table export keeps raw column names: it is the numeric record.
     if(method == "RFK" && !is.null(rv$rf_models[[paste0(l, "_act")]])) {
       rf_mod <- rv$rf_models[[paste0(l, "_act")]]
       imp_mat <- randomForest::importance(rf_mod)
       imp_col <- colnames(imp_mat)[1]
       df_imp <- data.frame(Variable = rownames(imp_mat), Importance = imp_mat[, imp_col])
       df_imp <- df_imp[order(df_imp$Importance, decreasing = TRUE), ]
       p_imp <- build_rf_importance_plot(rf_mod, paste("Variable Importance (Actual):", l), rv$mapping$vars)
       register_export_item(paste0("plot_rf_imp_act_", l), paste(meta$label, "-", l, "- RF Variable Importance (Actual)"), "plot", p_imp, meta$category)
       register_export_item(paste0("table_rf_imp_act_", l), paste(meta$label, "-", l, "- RF Variable Importance Data (Actual)"), "table", df_imp, meta$category)
     }
     if(method == "RFK" && (comp_mode || val_type != "actual") && !is.null(rv$rf_models[[paste0(l, "_pre")]])) {
       rf_mod_p <- rv$rf_models[[paste0(l, "_pre")]]
       imp_mat_p <- randomForest::importance(rf_mod_p)
       imp_col_p <- colnames(imp_mat_p)[1]
       df_imp_p <- data.frame(Variable = rownames(imp_mat_p), Importance = imp_mat_p[, imp_col_p])
       df_imp_p <- df_imp_p[order(df_imp_p$Importance, decreasing = TRUE), ]
       p_imp_p <- build_rf_importance_plot(rf_mod_p, paste("Variable Importance (Predicted):", l), rv$mapping$vars)
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

     # CK exports use the same faceted ggplot + metadata labels as the SA tab.
     ck_export_plot <- function(g, title) {
       vm <- variogram(g)
       ids <- names(g$data)
       id_labels <- vapply(ids, function(id) {
         if (id %in% c("v", "pv")) meta$label else get_var_label(id, rv$mapping$vars)
       }, character(1))
       rel <- relabel_ck_variogram(vm, g$model, id_labels)
       build_ck_variogram_ggplot(rel$vm, rel$model, title)
     }
     if(method == "CK" && !is.null(rv$gstat_objs[[paste0(l, "_act")]])) {
       p_ck <- ck_export_plot(rv$gstat_objs[[paste0(l, "_act")]], paste("Cross-Variogram (Actual):", l))
       register_export_item(paste0("plot_ck_vgm_act_", l), paste(meta$label, "-", l, "- CK Cross-Variogram (Actual)"), "plot", p_ck, meta$category)
     }
     if(method == "CK" && (comp_mode || val_type != "actual") && !is.null(rv$gstat_objs[[paste0(l, "_pre")]])) {
       p_ck_p <- ck_export_plot(rv$gstat_objs[[paste0(l, "_pre")]], paste("Cross-Variogram (Predicted):", l))
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

  observeEvent(list(rv$disp, rv$has_predictions, rv$cv_data_act, rv$v_emp_list), {
    # Committed run context (not the live sidebar): the predicted-side panels
    # describe the run on screen and must survive sidebar reconfiguration.
    d <- rv$disp
    prediction_active <- !is.null(d) && (isTRUE(d$comp_mode) || !identical(d$value_type, "actual"))
    has_interp <- prediction_active || rv$has_predictions
    # Pre-run only: auto-fit already fitted predicted-side variograms, so the
    # Predicted Data Structure card must be visible for manual tuning of the
    # "pre" target before the first interpolation.
    prerun_pre_vgm <- is.null(d) && any(grepl("_pre$", names(rv$v_emp_list)))
    shinyjs::toggle(id = "predicted_data_structure_ui", condition = has_interp || prerun_pre_vgm)
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
    vars_metadata_reactive = reactive(rv$mapping$vars),
    # Coordinate mapping for the Spatial Cross-Correlogram (same contract as
    # classif_server): it bins point pairs by projected ground distance.
    spatial_reactive = reactive(list(
      x = rv$mapping$x, y = rv$mapping$y,
      src_crs = rv$mapping$crs, proj_crs = input$crs_selection
    ))
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
    # Polygons (map-drawn + uploaded shapefile) enable the module's polygon
    # scope. get_drawn_sf is defined later in this server function - reactives
    # only look the binding up at evaluation time, after server setup
    # completes. Localities, boundary, buffer, and grid resolution are
    # module-local controls: the interpolation sidebar is hidden on the
    # Classification Suite tab and never affects classification runs.
    polygons_reactive = reactive(list(drawn = get_drawn_sf(), shp = rv$shp_bound))
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
  
