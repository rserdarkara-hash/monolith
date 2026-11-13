# server_setup.R (chunk A, sourced first with local = TRUE inside server):
# session dirs, raster caches, diagnostics closures, module wiring,
# session_state, map_overlay_rev/overlay_map_ids and the central `rv`.

  session_id <- paste0("session_", substr(session$token, 1, 16))
  session_progress_dir <- file.path(tempdir(), "monolith_progress", session_id)
  
  dir.create(session_progress_dir, recursive = TRUE, showWarnings = FALSE)
  # One directory per session, removed when it ends.
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

  # Display cap for the Leaflet viewer (every styling change re-encodes the
  # rasters on the main thread). DISPLAY-ONLY: exports, area statistics and
  # GeoTIFFs use the full-resolution rasters.
  LEAFLET_DISPLAY_MAX_CELLS <- 5e5

  get_projected_raster <- function(r, cache_key) {
    if (exists(cache_key, envir = leaflet_proj_cache)) return(get(cache_key, envir = leaflet_proj_cache))
    if (inherits(r, "PackedSpatRaster")) r <- terra::unwrap(r)
    if (inherits(r, "SpatRaster") && terra::ncell(r) > LEAFLET_DISPLAY_MAX_CELLS) {
      fact <- ceiling(sqrt(terra::ncell(r) / LEAFLET_DISPLAY_MAX_CELLS))
      r <- tryCatch(terra::aggregate(r, fact = fact, fun = "mean", na.rm = TRUE), error = function(e) r)
    }
    # Project once to EPSG:3857 and cache it, so addRasterImage(project =
    # FALSE) only re-colours on each styling tick. A failure is cached too
    # (NULL), so it notifies once per layer.
    r_proj <- tryCatch(leaflet::projectRasterForLeaflet(r, method = "bilinear"), error = function(e) {
      showNotification(paste("Map layer could not be projected for display:", conditionMessage(e)), type = "error")
      NULL
    })
    assign(cache_key, r_proj, envir = leaflet_proj_cache)
    r_proj
  }

  # Builder of the CV residual variogram plots, shared by the cached in-page
  # renderer (render_resid_plot()) and register_sci_plot() (expand, PNG).
  build_resid_vgm_diag <- function(cv_data_reactive, title_suffix = "") {
    # A locality's scored CV residuals as sf (NULL otherwise): engines return
    # CV objects as sf in the projected CRS, and the NA residuals of a failed
    # fold are dropped because gstat::variogram() stops on them.
    cv_residuals <- function(cv_obj) {
      if (!inherits(cv_obj, "sf") && !inherits(cv_obj, "Spatial")) return(NULL)
      if (inherits(cv_obj, "Spatial")) cv_obj <- sf::st_as_sf(cv_obj)
      if (!"residual" %in% names(cv_obj)) {
        cols <- detect_cv_columns(names(cv_obj))
        if (is.na(cols$observed) || is.na(cols$pred)) return(NULL)
        cv_obj$residual <- cv_obj[[cols$observed]] - cv_obj[[cols$pred]]
      }
      cv_obj[is.finite(cv_obj$residual), "residual"]
    }
    function() {
      req(input$sel_loc_stats, cv_data_reactive())
      loc <- input$sel_loc_stats
      df_list <- cv_data_reactive()
      total <- loc == "Total (Combined)"
      if (!total) {
        cv_obj <- cv_residuals(df_list[[loc]])
        req(cv_obj, nrow(cv_obj) >= 3)
      }

      tryCatch({
        if (total) {
          # Pooled within localities (pooled_within_variogram): no pair joins
          # two localities, and the fit starts from the within-locality spread.
          v_res <- pooled_within_variogram(lapply(df_list, cv_residuals), "residual")
          if (is.null(v_res)) {
            return(sci_placeholder(sprintf("No locality has %d cross-validation residuals.",
                                           POOLED_VGM_MIN_N), size = 4))
          }
          cap <- pooled_within_caption(v_res)
          return(build_fitted_variogram_plot(
            v_res, robust_vgm_fit(v_res, attr(v_res, "within")),
            title = paste0(trimws(paste("Pooled within-locality residual variogram", title_suffix)),
                           ": ", cap$count),
            extra_sub = cap$note))
        }
        lags <- calc_scientific_lags(cv_obj)
        v_res <- variogram(residual ~ 1, cv_obj, width = lags$width, cutoff = lags$cutoff)
        v_fit <- robust_vgm_fit(v_res, cv_obj$residual)
        # A residual set without usable variance is drawn as points plus the
        # reason (build_fitted_variogram_plot()).
        build_fitted_variogram_plot(v_res, v_fit,
                                    title = paste("Residual Variogram:", loc, title_suffix))
      }, error = function(e) {
        sci_placeholder(paste("Residual variogram error:\n", e$message), size = 4)
      })
    }
  }

  render_resid_plot <- function(cv_data_reactive, title_suffix, build_fn) {
    renderCachedPlot({
      p <- build_fn(); req(p); p
    }, cacheKeyExpr = {
      # CV data change at run completion (rv$results_rev); names() also
      # invalidates when the lists are reset at dispatch, so a running model
      # shows a blank panel.
      list("resid_vgm", title_suffix, input$sel_loc_stats, rv$results_rev,
           names(cv_data_reactive()))
    }, cache = "session")
  }

  # RK/RFK krige trend residuals; where a locality's trend step failed, the OK
  # fallback stored the measured values' variogram and no trend object exists.
  # `keys` are v_fits names ("<locality>_act" / "_pre").
  disp_trend_fallback <- function(keys) {
    m <- rv$disp$method %||% ""
    trend <- if (m == "RFK") rv$rf_models else rv$model_summaries
    stats::setNames(vapply(keys, function(k) m %in% c("RK", "RFK") && is.null(trend[[k]]),
                           logical(1)), keys)
  }
  # What each fit of the displayed run describes (vgm_params_export_df's
  # `of`); NULL for an engine without a trend step.
  disp_vgm_of <- function(fits = rv$disp$v_fits) {
    if (!(rv$disp$method %||% "") %in% c("RK", "RFK") || !length(fits)) return(NULL)
    ifelse(disp_trend_fallback(names(fits)), VGM_OF_FALLBACK, VGM_OF_RESIDUALS)
  }

  # `method` comes from the run that produced the assets, never input$method,
  # which may have changed during the run.
  register_locality_assets <- function(l, meta, comp_mode, val_type, method) {
     has_pre <- comp_mode || val_type != "actual"
     # The surfaces the run mapped. An item of the Predicted surface carries
     # "pre" in its registry id where the Actual one carries "act" or nothing.
     targets <- if (has_pre) c("act", "pre") else "act"
     s_lab <- c(act = "Actual", pre = "Predicted")
     sfx <- c(act = "", pre = "pre_")
     of <- function(tgt, act, pre) if (tgt == "act") act else pre
     reg <- function(id, what, type, obj) {
       register_export_item(paste0(id, l), paste(meta$label, "-", l, "-", what), type, obj)
     }

     # The per-locality Descriptive Statistics card's own frame and builder
     # (uploaded rows of this locality, not the deduplicated rv$sf).
     sv_l <- stats_table_vectors(rv$user_data, rv$disp, rv$mapping$loc, l)
     stats_l <- if(!is.null(sv_l)) summary_stats_df(sv_l$act, sv_l$pre, labels = c("Selected_Actual", "Selected_Predicted"))
     if(!is.null(stats_l)) reg("table_stats_loc_", "Descriptive Statistics", "table", stats_l)

     if(!is.null(rv$sf) && has_pre) {
       df_l_perf <- rv$sf %>% st_drop_geometry() %>% filter(loc == !!l, !is.na(v), !is.na(pv))
       # Same builder as the on-screen Prediction Performance card.
       perf_l <- pred_perf_df(df_l_perf$v, df_l_perf$pv)
       if(!is.null(perf_l)) reg("table_perf_loc_", "Prediction Performance", "table", perf_l)
     }

     # Model CV metrics export: the Model Performance row, with the fold plan
     # and the Moran reading as columns.
     for (tgt in targets) {
       cv_m <- of(tgt, rv$cv_metrics_act, rv$cv_metrics_pre)[[l]]
       if (is.null(cv_m)) next
       cv_d <- of(tgt, rv$cv_data_act, rv$cv_data_pre)[[l]]
       # The APPLIED fold plan (LOOCV below CV_BLOCK_MIN_N is not exported as
       # Spatial Block), which also decides whether Moran's p is reported.
       plan_l <- applied_cv_plan(if (!is.null(cv_d)) nrow(cv_d) else NA, rv$cv_strategy_sel, cv_m)
       reg(paste0("table_cv_", sfx[[tgt]], "loc_"), paste0("Model CV Metrics (", s_lab[[tgt]], ")"), "table",
           cv_metrics_export_df(cv_m, paste(s_lab[[tgt]], "Model"), plan_l$label,
                                of(tgt, rv$cv_info_act, rv$cv_info_pre)[[l]], moran_reading(plan_l$type)))
     }

     # Fold-realization stability (opt-in repeated CV).
     for (tgt in targets) {
       rep_l <- of(tgt, rv$cv_repeats_act, rv$cv_repeats_pre)$per_loc[[l]]
       if (is.null(rep_l)) next
       reg(paste0("table_cv_repeats_", sfx[[tgt]], "loc_"), paste0("Fold-Realization Stability (", s_lab[[tgt]], ")"), "table",
           cv_repeats_export_df(rep_l, paste(s_lab[[tgt]], "Model")))
     }

     # Class-area coverage is registered by the classification observer in
     # server_sci_analysis.R (classification is usually applied after the run).

     # Variogram exports reuse the Scientific Analysis builders.
     for (tgt in targets) {
       v_emp <- rv$disp$v_emps[[paste0(l, "_", tgt)]]
       if (is.null(v_emp)) next
       p_vgm <- build_fitted_variogram_plot(v_emp, rv$disp$v_fits[[paste0(l, "_", tgt)]],
                                            title = paste0("Variogram (", s_lab[[tgt]], "): ", l))
       reg(paste0("plot_vgm_", tgt, "_"), paste0("Variogram (", s_lab[[tgt]], ")"), "plot", p_vgm)
       reg(paste0("table_vgm_", tgt, "_"), paste0("Variogram Data (", s_lab[[tgt]], ")"), "table",
           as.data.frame(v_emp) %>% select(np, dist, gamma, dir.hor, dir.ver))
     }

     # The fitted model the kriging system solved with (family, nugget, sill,
     # range, structural dependency), beside its empirical points above.
     vgm_par_l <- vgm_params_export_df(rv$disp$v_fits, locs = l, of = disp_vgm_of())
     if(!is.null(vgm_par_l)) reg("table_vgm_params_", vgm_params_title(method), "table", vgm_par_l)

     for (tgt in targets) {
       cv_d <- of(tgt, rv$cv_data_act, rv$cv_data_pre)[[l]]
       if (is.null(cv_d)) next
       what <- of(tgt, "Actual", "Predicted Map")
       # NULL when no fold of this locality was scored.
       p_op <- build_obs_pred_plot(cv_d, title = paste0("Obs vs Pred (", what, "): ", l))
       if (!is.null(p_op)) reg(paste0("plot_obs_pred_", sfx[[tgt]]), paste0("Obs vs Pred Scatter (", what, ")"), "plot", p_op)
     }

     # Selection records export as figure plus data (selecting runs only:
     # Auto (GCV) / Auto (CV)); the CV Distance Match likewise, for every run.
     for (tgt in targets) {
       rp_l <- rv$disp$regional_params[[l]]
       tps_fit <- rp_l[[paste0("tps_fit_", tgt)]]
       if (method == "TPS" && !is.null(tps_fit$gcv)) {
         reg(paste0("plot_tps_gcv_", sfx[[tgt]]), paste0("TPS GCV Curve (", s_lab[[tgt]], ")"), "plot",
             build_tps_gcv_plot(tps_fit, l, tgt))
         reg(paste0("table_tps_gcv_", sfx[[tgt]]), paste0("TPS GCV Data (", s_lab[[tgt]], ")"), "table", tps_fit$gcv)
       }
       idw_fit <- rp_l[[paste0("idw_fit_", tgt)]]
       if (method == "IDW" && !is.null(idw_fit$profile)) {
         reg(paste0("plot_idw_power_", sfx[[tgt]]), paste0("IDW Power Selection (", s_lab[[tgt]], ")"), "plot",
             build_idw_power_plot(idw_fit, l, tgt))
         reg(paste0("table_idw_power_", sfx[[tgt]]), paste0("IDW Power CV Profile (", s_lab[[tgt]], ")"), "table",
             idw_profile_export_df(idw_fit$profile))
       }
       # The CV Distance Match of every strategy and engine, with the distance
       # percentiles it is drawn from.
       design <- of(tgt, rv$cv_design_act, rv$cv_design_pre)[[l]]
       if (!is.null(design)) {
         reg(paste0("plot_cv_distance_", sfx[[tgt]]), paste0("CV Distance Match (", s_lab[[tgt]], ")"), "plot",
             build_cv_distance_plot(stats::setNames(list(design), s_lab[[tgt]]),
                                    title = paste("CV Distance Match:", l)))
         reg(paste0("table_cv_distance_", sfx[[tgt]]), paste0("CV Distance Match Data (", s_lab[[tgt]], ")"), "table",
             cv_distance_export_df(design))
       }
     }

     # RF importance: the labelled plot and the raw-name data table with every
     # recorded measure (rf_importance_df()).
     for (tgt in if (method == "RFK") targets) {
       rf_mod <- rv$rf_models[[paste0(l, "_", tgt)]]
       if (is.null(rf_mod)) next
       reg(paste0("plot_rf_imp_", tgt, "_"), paste0("RF Variable Importance (", s_lab[[tgt]], ")"), "plot",
           build_rf_importance_plot(rf_mod, paste0("Variable Importance (", s_lab[[tgt]], "): ", l), rv$mapping$vars))
       reg(paste0("table_rf_imp_", tgt, "_"), paste0("RF Variable Importance Data (", s_lab[[tgt]], ")"), "table",
           rf_importance_df(rf_mod))
     }

     # The RK trend panel's coefficient table (numeric flavour) and fit statistics.
     for (tgt in if (method == "RK") targets) {
       lm_sum <- rv$model_summaries[[paste0(l, "_", tgt)]]
       if (is.null(lm_sum)) next
       coef_df <- rk_coef_export_df(lm_sum, rv$mapping$vars)
       if (!is.null(coef_df)) reg(paste0("table_rk_coef_", tgt, "_"), paste0("RK Regression Coefficients (", s_lab[[tgt]], ")"), "table", coef_df)
       fit_df <- rk_fit_stats_df(lm_sum)
       if (!is.null(fit_df)) reg(paste0("table_rk_fit_", tgt, "_"), paste0("RK Trend Fit Statistics (", s_lab[[tgt]], ")"), "table", fit_df)
     }

     # CK exports: the SA tab's faceted plot on the empirical variogram stored
     # with the LMC fit.
     for (tgt in if (method == "CK") targets) {
       g <- rv$gstat_objs[[paste0(l, "_", tgt)]]
       vm <- attr(g, "monolith_vm")
       if (is.null(g) || is.null(vm)) next
       cols <- ck_id_columns(g)
       id_labels <- vapply(names(cols), function(id) {
         if (cols[[id]] %in% c("v", "pv")) meta$label else get_var_label(cols[[id]], rv$mapping$vars)
       }, character(1))
       rel <- relabel_ck_variogram(vm, g$model, id_labels)
       reg(paste0("plot_ck_vgm_", tgt, "_"), paste0("CK Cross-Variogram (", s_lab[[tgt]], ")"), "plot",
           build_ck_variogram_ggplot(rel$vm, rel$model, paste0("Cross-Variogram (", s_lab[[tgt]], "): ", l)))
     }

     if(method %in% c("IDW", "TPS")) {
       param_df <- build_regional_params_df(method, l, rv$disp$regional_params, has_pre = has_pre, export = TRUE)
       if(!is.null(param_df)) reg("table_params_loc_", "Model Parameters", "table", param_df)
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
    # Before the first run, auto-fit may already hold predicted-side
    # variograms to tune, so the Predicted Data Structure card shows.
    prerun_pre_vgm <- is.null(d) && any(grepl("_pre$", names(rv$v_emp_list)))
    shinyjs::toggle(id = "predicted_data_structure_ui", condition = has_interp || prerun_pre_vgm)
    shinyjs::toggle(id = "rk_pred_ui", condition = has_interp)
    shinyjs::toggle(id = "rk_internal_vgm_pre_ui", condition = has_interp)
    shinyjs::toggle(id = "rfk_pred_ui", condition = has_interp)
    shinyjs::toggle(id = "rfk_internal_vgm_pre_ui", condition = has_interp)
    shinyjs::toggle(id = "ck_pred_ui", condition = has_interp)
    shinyjs::toggle(id = "tps_pred_ui", condition = has_interp)
    shinyjs::toggle(id = "idw_pred_ui", condition = has_interp)
    shinyjs::toggle(id = "validation_diagnostics_act_ui", condition = length(rv$cv_data_act) > 0)
    shinyjs::toggle(id = "validation_diagnostics_pre_ui", condition = has_interp)
    shinyjs::toggle(id = "loc_pred_col", condition = has_interp)
    shinyjs::toggle(id = "area_total_pred_col", condition = has_interp)
    
    # Uploaded-prediction statistics only when the DISPLAYED run mapped a
    # prediction side and its variable has a prediction column (NA, not NULL,
    # when none: is_valid_col_ref()).
    has_upl_pred <- !is.null(d) && has_interp &&
      (is_valid_col_ref(d$pred) || is_valid_col_ref(d$pred_ss))
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
    # Polygons (drawn + uploaded) enable the polygon scope. get_drawn_sf() is
    # defined in a later chunk; reactives resolve it at evaluation time. The
    # interpolation sidebar never affects classification runs.
    polygons_reactive = reactive(list(drawn = get_drawn_sf(),
                                      shp = shp_assume_crs(rv$shp_bound, rv$mapping$crs)))
  )

  # No theme wiring: the light/dark toggle is client-side (ui_main.R). The
  # default basemap is the selectInput's `selected` value (ui_main_tabs.R).

  # CARTO basemap API key (add_base_tiles()), shown only while a CARTO layer is
  # selected. Defined in chunk A so the Data Setup minimap and the Map Viewer
  # read one value; debounced so half-typed keys are not sent as tile
  # requests. Session-scoped: never saved to a config or export.
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
  # TRUE from a data upload until the Pop-up Content choices are rebuilt: the
  # browser still reports the previous table's choice in that flush.
  session_state$popup_fresh <- FALSE

  # Bumped by every map renderLeaflet, so proxy-managed overlays (points,
  # borders, controls) are re-applied after each full re-render.
  map_overlay_rev <- reactiveVal(0L)
  overlay_map_ids <- c("main_map", "comp_map_left", "comp_map_right")

  # The view menu split into surfaces and layer (parse_map_view()), written by
  # server_map_viewer.R. reactiveVals invalidate only on change, so switching
  # between a surface and its SE/variance view restyles through the proxy.
  map_view_base <- reactiveVal("view_act")
  map_view_layer <- reactiveVal("value")

  rv <- reactiveValues(
    user_data = NULL, # Uploaded data
    has_predictions = FALSE, # Tracks interpolation state
    export_registry = list(), # Registry of plots and tables for export
    drawn_polygons = list(), # Stores drawn polygons from Leaflet
    shp_bound = NULL, # Custom shapefile boundary
    mapping = list(
      # crs stays NULL until confirmed or identified from evidence: a default
      # zone would georeference other users' data into the wrong country.
      x = NULL, y = NULL, loc = NULL, crs = NULL,
      vars = list() # List of actual/pred pairs
    ),
    sf = NULL, bound = NULL,
    bound_overlap_m2 = c(act = 0, pre = 0),
    v_fit_list = list(), v_emp_list = list(), 
    # A run's surfaces, each a named list of its localities' Packed rasters on
    # their own grids: the one copy every map, table and export reads.
    rast_list_act = list(), rast_list_pre = list(), rast_list_res = list(), rast_list_point_res = list(),
    cv_metrics_act = list(), cv_metrics_pre = list(),
    cv_data_act = list(), cv_data_pre = list(),
    cv_strategy_sel = "auto", # CV strategy applied in the last run (for labels)
    cv_repeats_sel = 1L, # Fold realizations requested by the last run (1 = off)
    cv_repeats_act = NULL, cv_repeats_pre = NULL, # Repeated-CV mean/SD report
    # Per-locality CV population of the last run: what was scored, how many
    # samples were expected, and the id two archived runs are matched on.
    cv_info_act = list(), cv_info_pre = list(),
    # Per-locality CV Distance Match records (cv_distance_summary) of the last run.
    cv_design_act = list(), cv_design_pre = list(),

    loc_buffer_res = list(), # Per locality, the Auto dynamic-buffer resolution (sidebar preview)
    idw_factors = list(), tps_lambdas = list(), # Per-locality IDW powers / TPS lambdas
    # Colour palette picked per variable (column -> palette) this session;
    # resolve_var_palette() falls back to the variable's default. Not run state:
    # an archive restore leaves it alone.
    palette_picks = list(),
    model_summaries = list(), # RK trend summaries (summary.lm)
    rf_models = list(), # trained random forests
    gstat_objs = list(), # gstat objects for CK
    loc_names = NULL, log = "Ready.",
    # Worker warnings of the last run, persisted out of the progress overlay
    # (server_execution.R persist_run_warnings) so they outlive "Reveal maps".
    run_warnings = character(0),
    results_rev = 0L, # Bumped when a run's results land (keys cached plots)
    drawn_feature = NULL, # Temporarily store drawn shape for grouping
    run_config_summary = NULL, # Plain text summary of latest run configuration
    disp = NULL, # Display context committed at run dispatch (see get_display_meta)
    run_counter = 0L, # Incremental run counter
    run_history = list(), # Archive of previous run results and configs
    proceed_run = NULL, # Trigger for model generation after archive decision
    pt_color_overrides = list(), # Custom point colours: column -> palette -> group -> hex
    auto_archive_choice = "none", # "none", "archive", or "discard"
    # TRUE between a completed OPTIMIZE ALL VARIOGRAMS and the next run: the
    # fitted curves on the Scientific Analysis tab belong to the tuning
    # session, not to whatever run is on screen (see sci_vgm_tuning, chunk C).
    vgm_preview = FALSE,
    model_running = FALSE, # True when parallel model calculations are active
    opt_running = FALSE, # True while a sidebar optimizer promise is in flight
    run_token = 0L # Incremental run token for async cancellation
  )

  # The displayed run's pooled "Total (Combined)" CV metrics per surface,
  # computed once per run or restore (the pooled Moran permutations are
  # costly) and read by the Model Performance card and the export.
  pooled_cv_metrics <- list(
    act = reactive(perform_pooled_cv(rv$cv_data_act, rv$cv_metrics_act)),
    pre = reactive(perform_pooled_cv(rv$cv_data_pre, rv$cv_metrics_pre))
  )

  # ── The display state of one run ──────────────────────────────────────────
  # Everything the Map Viewer and Scientific Analysis render for a run besides
  # its configuration and registry; archive and restore move all of it
  # together. It must cover every field the dispatch reset clears
  # (server_execution.R), or a restore would keep the newer run's value.
  # Rasters are shared with the export registry, not copied.
  DISPLAY_STATE_FIELDS <- c(
    "disp", "has_predictions",
    "rast_list_act", "rast_list_pre", "rast_list_res", "rast_list_point_res",
    "sf", "bound", "bound_overlap_m2", "loc_names",
    "log", "run_warnings", "model_summaries", "rf_models", "gstat_objs",
    "cv_metrics_act", "cv_metrics_pre", "cv_data_act", "cv_data_pre",
    "cv_repeats_act", "cv_repeats_pre", "cv_info_act", "cv_info_pre",
    "cv_design_act", "cv_design_pre",
    "cv_strategy_sel", "cv_repeats_sel"
  )

  # TRUE while the run on screen has results a new run or a restore would
  # replace: a finished (not cancelled) run with at least one surface. Asked of
  # the run itself, not of the export registry, which the user can clear.
  displayed_run_has_results <- function() {
    cfg <- rv$run_config_summary
    !is.null(cfg) && !identical(cfg$status, "cancelled") &&
      length(rv$rast_list_act) + length(rv$rast_list_pre) > 0
  }

  # One archive entry: the configuration and registry under their historical
  # names (the history panel reads them), the rest under `state`.
  snapshot_display_state <- function() {
    list(config = rv$run_config_summary,
         registry = rv$export_registry,
         state = lapply(stats::setNames(nm = DISPLAY_STATE_FIELDS), function(f) rv[[f]]))
  }

  # Put an archived run back on screen. The raster caches are keyed on
  # rv$run_counter, which a restore keeps, so they are cleared; rv$results_rev
  # keys the cached plots. rv$run_token (a running run's abort token) is left.
  restore_display_state <- function(entry) {
    clear_raster_caches()
    for (f in names(entry$state)) rv[[f]] <- entry$state[[f]]
    rv$run_config_summary <- entry$config
    rv$export_registry <- entry$registry
    # As when a run lands: the variogram curves on screen belong to a run again.
    rv$vgm_preview <- FALSE
    rv$results_rev <- rv$results_rev + 1L
  }

