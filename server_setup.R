# server_setup.R (sourced with local = TRUE inside server) - session infra,
# raster caches, diagnostics closures, decoupled module wiring, session_state,
# map_overlay_rev/overlay_map_ids and the central `rv` reactiveValues.
# This file MUST be sourced first: every later chunk reads names defined here.

  session_id <- paste0("session_", substr(session$token, 1, 16))
  session_progress_dir <- file.path(tempdir(), "monolith_progress", session_id)
  
  dir.create(session_progress_dir, recursive = TRUE, showWarnings = FALSE)
  # One directory per session, and nothing removed it: on a multi-user
  # deployment they accumulate for the lifetime of the R process. Same idiom the
  # shapefile upload already uses (server_data_setup.R).
  session$onSessionEnded(function() {
    unlink(session_progress_dir, recursive = TRUE)
  })

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
      # gstat::variogram() stops on an NA response, which a failed CV fold
      # leaves behind; use the scored samples only.
      if (inherits(cv_obj, "Spatial")) cv_obj <- sf::st_as_sf(cv_obj)
      cv_obj <- cv_obj[is.finite(cv_obj$residual), ]
      req(nrow(cv_obj) >= 3)

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
     # The per-locality Descriptive Statistics card's own frame and builder
     # (uploaded rows of this locality, not the deduplicated rv$sf).
     sv_l <- stats_table_vectors(rv$user_data, rv$disp, rv$mapping$loc, l)
     stats_l <- if(!is.null(sv_l)) summary_stats_df(sv_l$act, sv_l$pre, labels = c("Selected_Actual", "Selected_Predicted"))
     if(!is.null(stats_l)) {
       register_export_item(paste0("table_stats_loc_", l), paste(meta$label, "-", l, "- Descriptive Statistics"), "table", stats_l, meta$category)
     }

     if(!is.null(rv$sf)) {
       if(comp_mode || val_type != "actual") {
         df_l_perf <- rv$sf %>% st_drop_geometry() %>% filter(loc == !!l, !is.na(v), !is.na(pv))
         # Same builder as the on-screen Prediction Performance card, so the
         # export carries all eleven statistics rather than a six-metric
         # subset under its own labels. Per-locality n is small (8-30), which
         # is exactly where the two CCC estimators diverge most.
         perf_l <- pred_perf_df(df_l_perf$v, df_l_perf$pv)
         if(!is.null(perf_l)) {
           register_export_item(paste0("table_perf_loc_", l), paste(meta$label, "-", l, "- Prediction Performance"), "table", perf_l, meta$category)
         }

       }
     }

     # Model CV metrics export: the same wide row the Model Performance card
     # shows, with the fold plan and Moran's null expectation as columns of
     # their own. It used to be a two-column dump of perform_cv()'s internal
     # field names, with every value coerced to text.
     if(!is.null(rv$cv_metrics_act[[l]])) {
       n_obs_l <- if(!is.null(rv$cv_data_act[[l]])) nrow(rv$cv_data_act[[l]]) else NA
       cv_table <- cv_metrics_export_df(rv$cv_metrics_act[[l]], "Actual Model",
                                        cv_type_label(n_obs_l, rv$cv_strategy_sel),
                                        rv$cv_info_act[[l]])
       register_export_item(paste0("table_cv_loc_", l), paste(meta$label, "-", l, "- Model CV Metrics (Actual)"), "table", cv_table, meta$category)
     }

     if((comp_mode || val_type != "actual") && !is.null(rv$cv_metrics_pre[[l]])) {
       n_obs_l_p <- if(!is.null(rv$cv_data_pre[[l]])) nrow(rv$cv_data_pre[[l]]) else NA
       cv_table_p <- cv_metrics_export_df(rv$cv_metrics_pre[[l]], "Predicted Model",
                                          cv_type_label(n_obs_l_p, rv$cv_strategy_sel),
                                          rv$cv_info_pre[[l]])
       register_export_item(paste0("table_cv_pre_loc_", l), paste(meta$label, "-", l, "- Model CV Metrics (Predicted)"), "table", cv_table_p, meta$category)
     }

     # Fold-realization stability (opt-in repeated CV): reported on screen,
     # never exportable before.
     rep_l_a <- rv$cv_repeats_act$per_loc[[l]]
     if(!is.null(rep_l_a)) {
       register_export_item(paste0("table_cv_repeats_loc_", l), paste(meta$label, "-", l, "- Fold-Realization Stability (Actual)"), "table", cv_repeats_export_df(rep_l_a, "Actual Model"), meta$category)
     }
     if(comp_mode || val_type != "actual") {
       rep_l_p <- rv$cv_repeats_pre$per_loc[[l]]
       if(!is.null(rep_l_p)) {
         register_export_item(paste0("table_cv_repeats_pre_loc_", l), paste(meta$label, "-", l, "- Fold-Realization Stability (Predicted)"), "table", cv_repeats_export_df(rep_l_p, "Predicted Model"), meta$category)
       }
     }

     # Per-locality class-area coverage is registered by the classification
     # observer in server_sci_analysis.R, not here: the classification is
     # usually applied after the run, and a table registered at run completion
     # would miss it. That observer covers both surfaces and re-registers on
     # every re-classification.


     # Variogram exports register the same ggplot builders the Scientific
     # Analysis tab renders (former lattice look retired; numbers unchanged).
     if(!is.null(rv$disp$v_emps[[paste0(l, "_act")]])) {
       v_emp <- rv$disp$v_emps[[paste0(l, "_act")]]
       v_fit <- rv$disp$v_fits[[paste0(l, "_act")]]
       p_vgm <- build_variogram_ggplot(v_emp, v_fit, title = paste("Variogram (Actual):", l))
       register_export_item(paste0("plot_vgm_act_", l), paste(meta$label, "-", l, "- Variogram (Actual)"), "plot", p_vgm, meta$category)
       df_vgm <- as.data.frame(v_emp) %>% select(np, dist, gamma, dir.hor, dir.ver)
       register_export_item(paste0("table_vgm_act_", l), paste(meta$label, "-", l, "- Variogram Data (Actual)"), "table", df_vgm, meta$category)
     }
     if((comp_mode || val_type != "actual") && !is.null(rv$disp$v_emps[[paste0(l, "_pre")]])) {
       v_emp_p <- rv$disp$v_emps[[paste0(l, "_pre")]]
       v_fit_p <- rv$disp$v_fits[[paste0(l, "_pre")]]
       p_vgm_p <- build_variogram_ggplot(v_emp_p, v_fit_p, title = paste("Variogram (Predicted):", l))
       register_export_item(paste0("plot_vgm_pre_", l), paste(meta$label, "-", l, "- Variogram (Predicted)"), "plot", p_vgm_p, meta$category)
       df_vgm_p <- as.data.frame(v_emp_p) %>% select(np, dist, gamma, dir.hor, dir.ver)
       register_export_item(paste0("table_vgm_pre_", l), paste(meta$label, "-", l, "- Variogram Data (Predicted)"), "table", df_vgm_p, meta$category)
     }

     # The FITTED model (model family, nugget, sill, range, structural
     # dependency). Only the empirical points were exportable before, so the
     # parameters the kriging system actually solved with left no record.
     vgm_par_l <- vgm_params_export_df(rv$disp$v_fits, locs = l)
     if(!is.null(vgm_par_l)) {
       register_export_item(paste0("table_vgm_params_", l), paste(meta$label, "-", l, "- Variogram Parameters"), "table", vgm_par_l, meta$category)
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
     
     # Like the variogram above, the GCV curve exports as BOTH the figure and
     # the lambda/GCV grid it was drawn from - the numeric record of how the
     # smoothing parameter was chosen.
     if(method == "TPS" && !is.null(rv$disp$tps_gcv_data[[paste0(l, "_act")]])) {
       df_gcv <- rv$disp$tps_gcv_data[[paste0(l, "_act")]]
       p_gcv <- ggplot(df_gcv, aes(x = lambda, y = gcv)) +
         geom_line(color = "steelblue") + geom_point() + scale_x_log10() +
         labs(title = paste("TPS GCV Diagnostics (Actual):", l)) + theme_minimal()
       register_export_item(paste0("plot_tps_gcv_", l), paste(meta$label, "-", l, "- TPS GCV Curve (Actual)"), "plot", p_gcv, meta$category)
       register_export_item(paste0("table_tps_gcv_", l), paste(meta$label, "-", l, "- TPS GCV Data (Actual)"), "table", as.data.frame(df_gcv), meta$category)
     }
     if(method == "TPS" && (comp_mode || val_type != "actual") && !is.null(rv$disp$tps_gcv_data[[paste0(l, "_pre")]])) {
       df_gcv_p <- rv$disp$tps_gcv_data[[paste0(l, "_pre")]]
       p_gcv_p <- ggplot(df_gcv_p, aes(x = lambda, y = gcv)) +
         geom_line(color = "firebrick") + geom_point() + scale_x_log10() +
         labs(title = paste("TPS GCV Diagnostics (Predicted):", l)) + theme_minimal()
       register_export_item(paste0("plot_tps_gcv_pre_", l), paste(meta$label, "-", l, "- TPS GCV Curve (Predicted)"), "plot", p_gcv_p, meta$category)
       register_export_item(paste0("table_tps_gcv_pre_", l), paste(meta$label, "-", l, "- TPS GCV Data (Predicted)"), "table", as.data.frame(df_gcv_p), meta$category)
     }
     
     # RF importance exports reuse the labeled SA-tab builder (metadata labels
     # instead of raw column names; every importance measure gets a panel).
     # The data-table export (rf_importance_df, ui_formatting.R) keeps raw
     # column names and carries EVERY importance measure the forest recorded.
     if(method == "RFK" && !is.null(rv$rf_models[[paste0(l, "_act")]])) {
       rf_mod <- rv$rf_models[[paste0(l, "_act")]]
       p_imp <- build_rf_importance_plot(rf_mod, paste("Variable Importance (Actual):", l), rv$mapping$vars)
       register_export_item(paste0("plot_rf_imp_act_", l), paste(meta$label, "-", l, "- RF Variable Importance (Actual)"), "plot", p_imp, meta$category)
       register_export_item(paste0("table_rf_imp_act_", l), paste(meta$label, "-", l, "- RF Variable Importance Data (Actual)"), "table", rf_importance_df(rf_mod), meta$category)
     }
     if(method == "RFK" && (comp_mode || val_type != "actual") && !is.null(rv$rf_models[[paste0(l, "_pre")]])) {
       rf_mod_p <- rv$rf_models[[paste0(l, "_pre")]]
       p_imp_p <- build_rf_importance_plot(rf_mod_p, paste("Variable Importance (Predicted):", l), rv$mapping$vars)
       register_export_item(paste0("plot_rf_imp_pre_", l), paste(meta$label, "-", l, "- RF Variable Importance (Predicted)"), "plot", p_imp_p, meta$category)
       register_export_item(paste0("table_rf_imp_pre_", l), paste(meta$label, "-", l, "- RF Variable Importance Data (Predicted)"), "table", rf_importance_df(rf_mod_p), meta$category)
     }

     # The coefficient table the RK trend panel shows (labelled terms, CI,
     # significance codes), in its numeric export flavour, plus the fit
     # statistics that panel puts in chips.
     register_rk_trend <- function(lm_sum, tgt, tgt_label) {
       coef_df <- rk_coef_export_df(lm_sum, rv$mapping$vars)
       if(!is.null(coef_df)) {
         register_export_item(paste0("table_rk_coef_", tgt, "_", l), paste(meta$label, "-", l, paste0("- RK Regression Coefficients (", tgt_label, ")")), "table", coef_df, meta$category)
       }
       fit_df <- rk_fit_stats_df(lm_sum)
       if(!is.null(fit_df)) {
         register_export_item(paste0("table_rk_fit_", tgt, "_", l), paste(meta$label, "-", l, paste0("- RK Trend Fit Statistics (", tgt_label, ")")), "table", fit_df, meta$category)
       }
     }
     if(method == "RK" && !is.null(rv$model_summaries[[paste0(l, "_act")]])) {
       register_rk_trend(rv$model_summaries[[paste0(l, "_act")]], "act", "Actual")
     }
     if(method == "RK" && (comp_mode || val_type != "actual") && !is.null(rv$model_summaries[[paste0(l, "_pre")]])) {
       register_rk_trend(rv$model_summaries[[paste0(l, "_pre")]], "pre", "Predicted")
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
                                            has_pre = comp_mode || val_type != "actual", export = TRUE)
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
      # "" (neither selector has a default) has to reach the modules as NULL:
      # that is the value their guards test for.
      src_crs = rv$mapping$crs, proj_crs = if (isTruthy(input$crs_selection)) input$crs_selection else NULL
    ))
  )

  classif_server(
    id = "classification",
    data_reactive = reactive(rv$user_data),
    vars_metadata_reactive = reactive(rv$mapping$vars),
    spatial_reactive = reactive(list(
      x = rv$mapping$x, y = rv$mapping$y,
      src_crs = rv$mapping$crs,
      proj_crs = if (isTruthy(input$crs_selection)) input$crs_selection else NULL,
      loc = rv$mapping$loc
    )),
    # Polygons (map-drawn + uploaded shapefile) enable the module's polygon
    # scope. get_drawn_sf is defined later in this server function - reactives
    # only look the binding up at evaluation time, after server setup
    # completes. Localities, boundary, buffer, and grid resolution are
    # module-local controls: the interpolation sidebar is hidden on the
    # Classification Suite tab and never affects classification runs.
    polygons_reactive = reactive(list(drawn = get_drawn_sf(),
                                      shp = shp_assume_crs(rv$shp_bound, rv$mapping$crs)))
  )

  # No theme wiring here any more. There is one theme; its light and dark
  # variants are two token blocks in the single stylesheet emitted from
  # ui_main.R, and the toggle flips a data-theme attribute client-side. That
  # removes the two renderUI round-trips a theme change used to cost, and the
  # observer that re-selected the basemap per theme: the Map Viewer's default
  # basemap is the selectInput's own `selected` value in ui_main_tabs.R.

  # CARTO's Positron and Dark Matter raster basemaps require a free API key
  # (ui_plotting.R, add_base_tiles); the field only appears in the Map Viewer
  # toolbar while one of them is selected. Defined here, in the first chunk, so
  # the Data Setup validation minimap (chunk D) and the Map Viewer (chunk H)
  # read one value. Debounced because a textInput reports every keystroke, and
  # each report would otherwise send a half-typed key to CARTO as a fresh tile
  # request. Session-scoped: never persisted to a run config or an export.
  carto_api_key <- shiny::debounce(
    reactive(trimws(input$carto_api_key %||% "")), 800)

  session_state <- new.env(parent = emptyenv())
  session_state$main_map_rendered <- FALSE
  session_state$comp_maps_rendered <- FALSE
  session_state$minimap_rendered <- FALSE
  # Every value the app itself has written into either CRS selector, so an
  # automatic fill can tell "the user chose this" from "we chose this"
  # (crs_user_chose / crs_has_value, server_data_setup.R).
  session_state$crs_auto <- list(map_crs = character(0), crs_selection = character(0))
  # The value a selector held when it was cleared, until the clear round-trips.
  # An upload resets both selectors and then reads them back in the same flush,
  # where input$<id> still reports the old file's CRS (crs_effective).
  session_state$crs_stale <- list()
  # The value each selector last held as far as the server knows (written by
  # the app or reported by the browser); used to re-send the selection when
  # the zone ordering rebuilds the dropdowns.
  session_state$crs_last <- list()

  # Bumped by every map renderLeaflet. Overlay observers depend on it so
  # proxy-managed layers (points, borders, controls) are re-applied after each
  # full re-render; leafletProxy defers until after the flush, so the calls
  # land on the freshly rendered widget.
  map_overlay_rev <- reactiveVal(0L)
  overlay_map_ids <- c("main_map", "comp_map_left", "comp_map_right")

  # The Map Viewer's view menu, split into the surfaces shown and the layer
  # drawn from them (parse_map_view). reactiveVals invalidate only on a real
  # change, so moving between a surface and its SE/variance view restyles the
  # live widgets through the proxy (zoom and pan kept) instead of rebuilding
  # them. Written by the observer in server_map_viewer.R.
  map_view_base <- reactiveVal("view_act")
  map_view_layer <- reactiveVal("value")

  rv <- reactiveValues(
    user_data = NULL, # Uploaded data
    has_predictions = FALSE, # Tracks interpolation state
    export_registry = list(), # Registry of plots and tables for export
    drawn_polygons = list(), # Stores drawn polygons from Leaflet
    shp_bound = NULL, # Custom shapefile boundary
    mapping = list(
      # crs stays NULL until the user confirms one (or it is identified from
      # evidence in the file): a default zone would silently georeference
      # everyone else's data into the wrong country.
      x = NULL, y = NULL, loc = NULL, crs = NULL,
      vars = list() # List of actual/pred pairs
    ),
    rast = NULL, rast_pred = NULL, rast_res = NULL, rast_point_res = NULL, sf = NULL, bound = NULL,
    bound_overlap_m2 = c(act = 0, pre = 0),
    v_fit_list = list(), v_emp_list = list(), 
    rast_list_act = list(), rast_list_pre = list(), rast_list_res = list(), rast_list_point_res = list(),
    desc_vars_state = list(x = "", y = "", z = "", multi = character(0)),
    cv_metrics_act = list(), cv_metrics_pre = list(),
    cv_data_act = list(), cv_data_pre = list(),
    cv_strategy_sel = "auto", # CV strategy applied in the last run (for labels)
    cv_repeats_sel = 1L, # Fold realizations requested by the last run (1 = off)
    cv_repeats_act = NULL, cv_repeats_pre = NULL, # Repeated-CV mean/SD report
    # Per-locality CV population of the last run: what was scored, how many
    # samples were expected, and the id two archived runs are matched on.
    cv_info_act = list(), cv_info_pre = list(),

    loc_resolutions = list(), # Track spatial resolutions per locality
    idw_factors = list(), tps_lambdas = list(), # Regional Parameters
    tps_gcv_data = list(), # GCV Diagnostic Data
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
    # TRUE between a completed OPTIMIZE ALL VARIOGRAMS and the next run: the
    # fitted curves on the Scientific Analysis tab belong to the tuning
    # session, not to whatever run is on screen (see sci_vgm_tuning, chunk C).
    vgm_preview = FALSE,
    model_running = FALSE, # True when parallel model calculations are active
    opt_running = FALSE, # True while a sidebar optimizer promise is in flight
    run_token = 0L # Incremental run token for async cancellation
  )
  
