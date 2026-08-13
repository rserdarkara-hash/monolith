# server_sci_analysis.R (sourced with local = TRUE inside server) - model
# diagnostics, variogram/importance/obs-pred plots, stats/area/metrics/kappa
# tables, notifications, log and polygon export.
  # Shared note box for the per-locality diagnostic panels (same look as the
  # "select a locality" hints these panels already used).
  sci_ui_note <- function(msg) {
    div(style="padding: 12px; background-color: #f8f9fa; border: 1px dashed #ced4da; border-radius: 6px; color: #6c757d; font-style: italic; text-align: center;",
        msg)
  }

  # A missing per-locality trend object is NOT "nothing worth saying": for
  # RK/RFK it means the trend step did not run for that locality, so
  # apply_kriging_pipeline took its Ordinary Kriging fallback (it writes
  # "<engine> failed, using Ordinary Kriging fallback" via write_warning_file
  # and logs the cause), or the locality failed outright. A bare req() blanked
  # the panel and left the reason sitting in the Run Log; these panels name it
  # in place instead.
  trend_missing_msg <- function(loc, what) {
    m <- rv$disp$method %||% ""
    if (m %in% c("RK", "RFK")) {
      sprintf(paste0("No %s is stored for \"%s\".\nThis locality's %s trend model was not fitted, so the run fell back to ",
                     "Ordinary Kriging for it (or the locality failed).\nSee the Run Log on this tab for the reported cause."),
              what, loc, m)
    } else {
      sprintf("No %s is stored for \"%s\".\nThe displayed run used %s, which fits no covariate trend model.",
              what, loc, if (nzchar(m)) m else "an engine without a trend step")
    }
  }

  # RK linear-trend panels: fit-statistic chips + coefficient table (raw
  # summary.lm print kept behind a collapsible details element). Falls back to
  # the verbatim print if the stored object is not a summary.lm.
  output$model_summary_ui_act <- renderUI({
    loc <- input$sel_loc_stats; req(loc)
    if (loc == "Total (Combined)") {
      return(sci_ui_note("Linear trend summaries are computed per locality. Please select a specific locality from the analysis filter list above to view details."))
    }
    summary_obj <- rv$model_summaries[[paste0(loc, "_act")]]
    if (is.null(summary_obj)) return(sci_ui_note(trend_missing_msg(loc, "linear trend summary")))
    build_rk_trend_ui(summary_obj, "rk_coef_dt_act", "summ_act_static") %||%
      tagList(verbatimTextOutput("summ_act_static"))
  })
  output$model_summary_ui_pre <- renderUI({
    loc <- input$sel_loc_stats; req(loc)
    if (loc == "Total (Combined)") {
      return(sci_ui_note("Linear trend summaries are computed per locality. Please select a specific locality from the analysis filter list above to view details."))
    }
    summary_obj <- rv$model_summaries[[paste0(loc, "_pre")]]
    if (is.null(summary_obj)) return(sci_ui_note(trend_missing_msg(loc, "linear trend summary")))
    build_rk_trend_ui(summary_obj, "rk_coef_dt_pre", "summ_pre_static") %||%
      tagList(verbatimTextOutput("summ_pre_static"))
  })

  output$rk_coef_dt_act <- DT::renderDataTable({
    loc <- input$sel_loc_stats
    req(loc, loc != "Total (Combined)")
    summary_obj <- rv$model_summaries[[paste0(loc, "_act")]]
    # Always qualify: global.R attaches jsonlite after shiny, and
    # jsonlite::validate() masks shiny::validate() - a bare call dies with
    # "is.character(txt) is not TRUE". need() is qualified for the same reason.
    shiny::validate(shiny::need(summary_obj, trend_missing_msg(loc, "coefficient table")))
    df <- rk_coef_table(summary_obj, sci_vars_meta())
    shiny::validate(shiny::need(df, "The stored trend model carries no estimable coefficients."))
    sci_dt(df)
  })

  output$rk_coef_dt_pre <- DT::renderDataTable({
    loc <- input$sel_loc_stats
    req(loc, loc != "Total (Combined)")
    summary_obj <- rv$model_summaries[[paste0(loc, "_pre")]]
    shiny::validate(shiny::need(summary_obj, trend_missing_msg(loc, "coefficient table")))
    df <- rk_coef_table(summary_obj, sci_vars_meta())
    shiny::validate(shiny::need(df, "The stored trend model carries no estimable coefficients."))
    sci_dt(df)
  })


  output$summ_act_static <- renderPrint({
    loc <- input$sel_loc_stats
    req(loc, loc != "Total (Combined)")
    summary_obj <- rv$model_summaries[[paste0(loc, "_act")]]
    shiny::validate(shiny::need(summary_obj, trend_missing_msg(loc, "linear trend summary")))
    summary_obj
  })

  output$summ_pre_static <- renderPrint({
    loc <- input$sel_loc_stats
    req(loc, loc != "Total (Combined)")
    summary_obj <- rv$model_summaries[[paste0(loc, "_pre")]]
    shiny::validate(shiny::need(summary_obj, trend_missing_msg(loc, "linear trend summary")))
    summary_obj
  })

  build_rf_imp_diag <- function(target) {
    loc <- input$sel_loc_stats; req(loc)
    if (loc == "Total (Combined)") {
      return(sci_placeholder("RF Variable Importance is generated per locality.\nPlease select a specific locality from the dropdown."))
    }
    if (is.null(rv$rf_models[[paste0(loc, "_", target)]])) {
      return(sci_placeholder(trend_missing_msg(loc, "random-forest trend model"), size = 4))
    }
    build_rf_importance_plot(rv$rf_models[[paste0(loc, "_", target)]],
                             paste0("Variable Importance (", if (target == "act") "Actual" else "Predicted", "): ", loc),
                             sci_vars_meta())
  }

  output$rf_importance_plot_act <- renderCachedPlot({
    p <- build_rf_imp_diag("act"); req(p); p
  }, cacheKeyExpr = {
    # is.null() stands in for the model object itself (too heavy to hash);
    # rv$results_rev separates runs, the null flag catches the dispatch reset.
    loc <- input$sel_loc_stats
    list("rf_imp_act", loc, rv$results_rev, input$sci_name_mode, is.null(rv$rf_models[[paste0(loc, "_act")]]))
  }, cache = "session")
  output$rf_importance_plot_pre <- renderCachedPlot({
    p <- build_rf_imp_diag("pre"); req(p); p
  }, cacheKeyExpr = {
    loc <- input$sel_loc_stats
    list("rf_imp_pre", loc, rv$results_rev, input$sci_name_mode, is.null(rv$rf_models[[paste0(loc, "_pre")]]))
  }, cache = "session")

  build_internal_vgm_diag <- function(type) {
    loc <- input$sel_loc_stats; req(loc)
    title_suffix <- if (type == "act") "(Actual)" else "(Predicted)"
    col_resid <- if (type == "act") "model_resid_act" else "model_resid_pre"

    if (loc == "Total (Combined)") {
      if (is.null(rv$sf) || !col_resid %in% colnames(rv$sf) || !any(!is.na(rv$sf[[col_resid]]))) {
        return(sci_placeholder(paste0(
          "No cross-validation residuals are stored for this run ", tolower(title_suffix), ".\n",
          "Pooled residual variograms need per-locality CV to have succeeded;\n",
          "see the Run Log on this tab for the reported cause."), size = 4))
      }
      formula_obj <- as.formula(paste(col_resid, "~ 1"))
      df_filtered <- rv$sf[!is.na(rv$sf[[col_resid]]), ]
      # Unlike the per-locality plots (internal trend-residual variogram of
      # the fitted model), the combined view pools CV residuals across
      # localities, so label it as such.
      build_variogram_ggplot(variogram(formula_obj, df_filtered),
                             title = paste("Pooled CV Residual Variogram", title_suffix))
    } else {
      v_emp <- rv$v_emp_list[[paste0(loc, "_", type)]]
      v_fit <- rv$v_fit_list[[paste0(loc, "_", type)]]
      if (is.null(v_emp) || is.null(v_fit)) {
        return(sci_placeholder(sprintf(paste0(
          "No fitted variogram is stored for \"%s\" %s.\nThe locality failed before the variogram step; ",
          "see the Run Log on this tab."), loc, tolower(title_suffix)), size = 4))
      }
      # RK/RFK store a RESIDUAL variogram only when their trend step ran. When
      # it did not, apply_kriging_pipeline's OK fallback overwrites v_emp/v_fit
      # with the variogram of the MEASURED values, so the panel must stop
      # calling that a residual variogram. The trend object is the marker: it
      # exists for exactly the localities whose trend step succeeded.
      trend_obj <- if (identical(rv$disp$method, "RFK")) {
        rv$rf_models[[paste0(loc, "_", type)]]
      } else {
        rv$model_summaries[[paste0(loc, "_", type)]]
      }
      ttl <- if ((rv$disp$method %||% "") %in% c("RK", "RFK") && is.null(trend_obj)) {
        paste("Variogram of Measured Values - Ordinary Kriging Fallback", paste0(title_suffix, ":"), loc)
      } else {
        paste("Internal Residual Variogram", paste0(title_suffix, ":"), loc)
      }
      build_variogram_ggplot(v_emp, v_fit, title = ttl)
    }
  }

  render_internal_vgm_plot <- function(type) {
    renderCachedPlot({
      p <- build_internal_vgm_diag(type); req(p); p
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

  build_ck_diag <- function(type) {
    loc <- input$sel_loc_stats; req(loc)
    if (loc == "Total (Combined)") {
      return(sci_placeholder("Cross-variograms are generated per locality.\nPlease select a specific locality from the dropdown."))
    }
    key <- paste0(loc, "_", type)
    g <- rv$gstat_objs[[key]]
    if (is.null(g)) {
      return(sci_placeholder("Cross-variogram is not available\n(LMC model fit failed, using Ordinary Kriging fallback.)"))
    }
    vm <- variogram(g)
    # Panel strips carry the gstat ids (internal target id + raw covariate
    # columns); map them to the run variable's display name and the
    # covariate labels/column names per the tab's naming radio.
    ids <- names(g$data)
    target_name <- sci_disp_label() %||% ids[1]
    id_labels <- vapply(ids, function(id) {
      if (id %in% c("v", "pv")) target_name else get_var_label(id, sci_vars_meta())
    }, character(1))
    rel <- relabel_ck_variogram(vm, g$model, id_labels)
    title_suffix <- if (type == "act") "(Actual)" else "(Predicted)"
    build_ck_variogram_ggplot(rel$vm, rel$model, paste("Cross-Variogram", paste0(title_suffix, ":"), loc))
  }

  render_ck_variogram_plot <- function(type) {
    renderCachedPlot({
      p <- build_ck_diag(type); req(p); p
    }, cacheKeyExpr = {
      loc <- input$sel_loc_stats
      list("ck_vgm", type, loc, rv$results_rev, input$sci_name_mode, is.null(rv$gstat_objs[[paste0(loc, "_", type)]]))
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
      # sci_dt(NULL) is the empty state, not a NULL payload: a DT output must
      # never be handed NULL (see sci_dt() in ui_components.R).
      if(length(rows) == 0) return(sci_dt(NULL))
      res <- do.call(rbind, rows)
      names(res)[7] <- "Structural Dep."
      return(sci_dt(res))
    }

    f_a <- rv$v_fit_list[[paste0(loc, "_act")]]; f_p <- rv$v_fit_list[[paste0(loc, "_pre")]]
    if(is.null(f_a) && is.null(f_p)) return(sci_dt(NULL))

    res <- data.frame(Param = c("Model", "Nugget", "Sill", "Range", "Structural Dep."),
                      Actual = get_vgm_params(f_a))
    # Predicted column only when a predicted-surface fit exists: an all-"NA"
    # column for a run that never mapped predictions is just noise.
    if (!is.null(f_p)) res$Predicted <- get_vgm_params(f_p)
    sci_dt(res)
  })
  build_tps_gcv_diag <- function(target) {
    loc <- input$sel_loc_stats; req(loc, identical(rv$disp$method, "TPS"))
    tryCatch({
      build_tps_gcv_plot(rv$tps_gcv_data, loc, target)
    }, error = function(e) {
      sci_placeholder(paste("GCV Plot Error:\n", e$message), size = 4)
    })
  }

  output$tps_gcv_plot_act <- renderCachedPlot({
    p <- build_tps_gcv_diag("act"); req(p); p
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

    df_plot <- data.frame(Observed = obs, Predicted = pre)
    # text aes on the point layer only: a per-point discrete aesthetic on the
    # smooth layer would fragment its grouping into one group per point.
    ggplot(df_plot, aes(x = Observed, y = Predicted)) +
      suppressWarnings(geom_point(aes(text = paste0("Observed: ", signif(Observed, 5),
                                                    "\nPredicted: ", signif(Predicted, 5),
                                                    "\nResidual: ", signif(Observed - Predicted, 5))),
                                  alpha = 0.6)) +
      geom_abline(intercept = 0, slope = 1, color = "red", linetype = "dashed") +
      geom_smooth(method = "lm", color = "blue", se = FALSE) +
      labs(title = title, subtitle = "Red: 1:1 Line, Blue: Regression", x = x_lab, y = y_lab) +
      theme_minimal()
  }

  build_obs_pred_diag <- function(target) {
    data_list <- if (target == "act") rv$cv_data_act else rv$cv_data_pre
    req(input$sel_loc_stats, data_list)
    loc <- input$sel_loc_stats
    if(loc == "Total (Combined)") {
       df <- do.call(rbind, lapply(data_list, function(x) if(inherits(x, "sf")) st_drop_geometry(x) else as.data.frame(x)))
    } else {
       df <- data_list[[loc]]
       if(inherits(df, "sf")) df <- st_drop_geometry(df)
       if(inherits(df, "Spatial")) df <- as.data.frame(df)
    }
    title <- if (target == "act") paste("Observed vs Predicted:", loc) else paste("Observed vs Predicted (Predicted Map):", loc)
    build_obs_pred_plot(df, title = title)
  }

  output$obs_pred_plot_act <- renderCachedPlot({
    p <- build_obs_pred_diag("act"); req(p); p
  }, cacheKeyExpr = {
    list("obs_pred_act", input$sel_loc_stats, rv$results_rev, names(rv$cv_data_act))
  }, cache = "session")

  build_resid_vgm_act <- build_resid_vgm_diag(reactive(rv$cv_data_act), "")
  build_resid_vgm_pre <- build_resid_vgm_diag(reactive(rv$cv_data_pre), "(Predicted Map)")
  output$resid_vgm_plot_act <- render_resid_plot(reactive(rv$cv_data_act), "", build_resid_vgm_act)

  output$obs_pred_plot_pre <- renderCachedPlot({
    p <- build_obs_pred_diag("pre"); req(p); p
  }, cacheKeyExpr = {
    list("obs_pred_pre", input$sel_loc_stats, rv$results_rev, names(rv$cv_data_pre))
  }, cache = "session")

  output$resid_vgm_plot_pre <- render_resid_plot(reactive(rv$cv_data_pre), "(Predicted Map)", build_resid_vgm_pre)

  output$tps_gcv_plot_pre <- renderCachedPlot({
    p <- build_tps_gcv_diag("pre"); req(p); p
  }, cacheKeyExpr = {
    list("tps_gcv_pre", input$sel_loc_stats, rv$results_rev, rv$disp$method, rv$tps_gcv_data)
  }, cache = "session")

  # ── Directional variogram (anisotropy diagnostic) ────────────────────────
  # Computed lazily in the main session from the displayed run's points, so a
  # user who never opens this card pays nothing and every engine (including
  # IDW/TPS, which fit no variogram) gets the same check. calc_directional_
  # variogram re-projects to a metric CRS itself — rv$sf carries the user's
  # chosen DISPLAY crs, which may well be geographic, and a bearing in degrees
  # of longitude is not a bearing on the ground.
  build_directional_vgm_diag <- function() {
    loc <- input$sel_loc_stats; req(loc)
    req(rv$sf)
    src <- input$dir_vgm_source %||% "v"

    pts <- rv$sf
    if (loc != "Total (Combined)" && "loc" %in% colnames(pts)) {
      pts <- pts[!is.na(pts$loc) & pts$loc == loc, ]
    }
    if (nrow(pts) == 0) return(sci_placeholder("No points available for this locality."))

    if (identical(src, "resid")) {
      # Prefer the displayed surface's CV residual column; both are written by
      # the run, and only one exists for an actual-only run.
      col <- if ("model_resid_act" %in% colnames(pts) &&
                 any(!is.na(pts$model_resid_act))) {
        "model_resid_act"
      } else if ("model_resid_pre" %in% colnames(pts) &&
                 any(!is.na(pts$model_resid_pre))) {
        "model_resid_pre"
      } else {
        NULL
      }
      if (is.null(col)) {
        return(sci_placeholder(paste0("No cross-validation residuals are stored for this run.\n",
                                      "Switch to \"Measured values\", or re-run with a method that reports CV.")))
      }
      value_col <- col
      what <- "CV residuals"
    } else {
      if (!"v" %in% colnames(pts) || !any(!is.na(pts$v))) {
        return(sci_placeholder("No measured values are available for this run."))
      }
      value_col <- "v"
      what <- "measured values"
    }

    vd <- calc_directional_variogram(pts, value_col)
    if (is.null(vd)) {
      return(sci_placeholder(paste0("Not enough point pairs for a directional variogram.\n",
                                    "Four directions need appreciably more points than one omnidirectional curve.")))
    }
    build_directional_variogram_ggplot(
      vd,
      title = paste0("Directional Variogram (", what, "): ", loc),
      subtitle = paste0("Bearings clockwise from north, 22.5° half-angle cones. ",
                        "Curves separating by range indicate anisotropy; ",
                        "the engines remain omnidirectional."))
  }

  output$directional_vgm_plot <- renderCachedPlot({
    p <- build_directional_vgm_diag(); req(p); p
  }, cacheKeyExpr = {
    list("dir_vgm", input$sel_loc_stats, input$dir_vgm_source, rv$results_rev,
         is.null(rv$sf), if (is.null(rv$sf)) 0L else nrow(rv$sf))
  }, cache = "session")

  # ── expand modal + PNG download wiring for every SA plot card ────────────
  register_sci_plot("directional_vgm_plot", "Directional Variogram (Anisotropy Check)", build_directional_vgm_diag)
  register_sci_plot("vgm_plot_main", "Actual Data Structure", function() build_vgm_structure_plot("act"))
  register_sci_plot("vgm_plot_pred", "Predicted Data Structure", function() build_vgm_structure_plot("pre"))
  register_sci_plot("rk_internal_vgm_act", "Internal Residual Variogram (Actual)", function() build_internal_vgm_diag("act"))
  register_sci_plot("rk_internal_vgm_pre", "Internal Residual Variogram (Predicted)", function() build_internal_vgm_diag("pre"))
  register_sci_plot("rfk_internal_vgm_act", "Internal Residual Variogram (Actual)", function() build_internal_vgm_diag("act"))
  register_sci_plot("rfk_internal_vgm_pre", "Internal Residual Variogram (Predicted)", function() build_internal_vgm_diag("pre"))
  register_sci_plot("rf_importance_plot_act", "RF Variable Importance (Actual)", function() build_rf_imp_diag("act"))
  register_sci_plot("rf_importance_plot_pre", "RF Variable Importance (Predicted)", function() build_rf_imp_diag("pre"))
  register_sci_plot("ck_variogram_plot_act", "Cross-Variogram (Actual)", function() build_ck_diag("act"))
  register_sci_plot("ck_variogram_plot_pred", "Cross-Variogram (Predicted)", function() build_ck_diag("pre"))
  register_sci_plot("tps_gcv_plot_act", "TPS GCV Diagnostics (Actual)", function() build_tps_gcv_diag("act"))
  register_sci_plot("tps_gcv_plot_pre", "TPS GCV Diagnostics (Predicted)", function() build_tps_gcv_diag("pre"))
  register_sci_plot("obs_pred_plot_act", "Observed vs Predicted (Actual)", function() build_obs_pred_diag("act"))
  register_sci_plot("obs_pred_plot_pre", "Observed vs Predicted (Predicted Map)", function() build_obs_pred_diag("pre"))
  register_sci_plot("resid_vgm_plot_act", "Residual Variogram (Actual)", build_resid_vgm_act)
  register_sci_plot("resid_vgm_plot_pre", "Residual Variogram (Predicted Map)", build_resid_vgm_pre)

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
    if (is.null(v_act)) return(sci_dt(NULL))

    # Predicted summary only when the displayed run actually mapped
    # predictions (user's choice), not merely because a prediction column
    # exists in the uploaded data.
    disp_has_pred <- isTRUE(meta$comp_mode) || (!is.null(meta$value_type) && !identical(meta$value_type, "actual"))
    # Summarise the SAME prediction column the displayed run mapped: a
    # Single-Split run must describe _ss, not fall back to the _cve column
    # the map / CV / performance tables are not showing.
    pv_col <- if (identical(meta$value_type, "pred_ss")) meta$pred_ss else meta$pred
    v_pre <- if(disp_has_pred && is_valid_col_ref(pv_col) && pv_col %in% colnames(df)) df[[pv_col]] else NULL

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
    if(input$sel_loc_stats == "Total (Combined)") return(sci_dt(NULL))
    meta <- get_display_meta()
    req(meta)
    
    df <- rv$user_data %>% filter(!!sym(rv$mapping$loc) == input$sel_loc_stats)
    v_act <- if(!is.null(meta$actual) && !is.na(meta$actual) && meta$actual %in% colnames(df)) df[[meta$actual]] else NULL
    if (is.null(v_act)) return(sci_dt(NULL))

    # Same gate as stats_table_total: Predicted column only when the run
    # mapped predictions.
    disp_has_pred <- isTRUE(meta$comp_mode) || (!is.null(meta$value_type) && !identical(meta$value_type, "actual"))
    # Same column rule as stats_table_total: honour the displayed value_type.
    pv_col <- if (identical(meta$value_type, "pred_ss")) meta$pred_ss else meta$pred
    v_pre <- if(disp_has_pred && is_valid_col_ref(pv_col) && pv_col %in% colnames(df)) df[[pv_col]] else NULL

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
    # error-only: catching `condition` here also unwound message() conditions
    # escaping classification_params(), aborting the reactive mid-evaluation
    # and poisoning it for the map renderers (Jenks fell back to continuous)
    params <- tryCatch(classification_params(), error = function(e) NULL)
    if(is.null(params)) return(data.frame(Status = "Awaiting classification - press APPLY TO MAPS & STATS under Map Styling in the sidebar"))

    if (!is.null(r_id)) {
      brk_str <- if (!is.null(params) && !is.null(params$brks)) paste(params$brks, collapse = "_") else "nobrks"
      cache_key <- paste0(rv$run_counter, "_", r_id, "_", brk_str)
      if (exists(cache_key, envir = area_calc_cache)) {
        return(get(cache_key, envir = area_calc_cache))
      }
    }
    # unwrap only on a cache miss (deserializing a packed raster is expensive)
    if(inherits(r_obj, "PackedSpatRaster")) r_obj <- terra::unwrap(r_obj)

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
    if(loc == "Total (Combined)") sci_dt(NULL) else sci_dt(calc_area_df(rv$rast_list_act[[loc]], paste0("loc_act_", loc)))
  })
  output$area_table_loc_pre <- DT::renderDataTable({
    req(rv$rast_list_pre, input$color_style %in% c("agro", "bin")); loc <- input$sel_loc_stats
    if(loc == "Total (Combined)") sci_dt(NULL) else sci_dt(calc_area_df(rv$rast_list_pre[[loc]], paste0("loc_pre_", loc)))
  })

  output$cv_strategy_badge <- renderUI({
    req(length(rv$cv_metrics_act) > 0)
    strat <- rv$cv_strategy_sel %||% "auto"
    label <- switch(strat,
      "loocv" = "Standard LOOCV (full leave-one-out)",
      "block" = "Spatial Block CV (10 k-means folds; LOOCV below n=30)",
      "Auto (LOOCV for n ≤ 50, random 10-fold above)")
    # The pooled row's variance-explained scores are measured against the POOLED
    # mean, so between-locality differences in level count as variance the model
    # gets credit for explaining. That is an aggregation artifact, not skill, and
    # it makes the pooled row incomparable with the per-locality rows above it.
    # Message only - the computation is deliberately left as it is.
    pooled_note <- if (identical(input$sel_loc_stats, "Total (Combined)")) {
      tags$div(
        style = "font-size: 0.82em; color: #495057; margin: -4px 0 8px 0;",
        tags$span(style = "color: #868e96;",
                  "Pooled R²/NSE are computed against the pooled mean; when localities differ in their means, between-locality variance inflates these scores. Judge model skill on the per-locality rows.")
      )
    }
    # Repeated CV never moves the numbers above (realization 1 keeps the fixed
    # seed); say where the extra realizations are reported instead.
    n_rep <- rv$cv_repeats_sel %||% 1L
    repeat_note <- if (is.numeric(n_rep) && n_rep > 1) {
      tags$span(style = "color: #868e96;",
                sprintf(" Repeated CV is on (%d fold realizations); the values here are realization 1, the spread is in the table below.", n_rep))
    }
    tagList(
      tags$div(
        style = "font-size: 0.82em; color: #495057; margin: -4px 0 8px 0;",
        tags$span(style = "font-weight: 600;", "Cross-validation: "),
        tags$span(label),
        tags$span(style = "color: #868e96;", " (applies to these metrics only, not the map)."),
        repeat_note
      ),
      pooled_note
    )
  })

  output$metrics_table <- DT::renderDataTable({
    req(input$sel_loc_stats)
    loc <- input$sel_loc_stats
    
    # One definition of the Model Performance column set, shared by the empty
    # stub and by both populated branches so they cannot drift apart. The
    # labels and their order mirror the uploaded-prediction metrics table so the
    # two can be read side by side: perform_cv already computed MAE, NRMSE, CCC
    # and RPIQ, they were simply never displayed. Moran's I / p have no
    # counterpart there (uploaded predictions carry no CV residual field).
    metric_cols <- c("Source", "RMSE", "NRMSE (%)", "MAE", "R2 (Corr)",
                     "R2 (NSE/Trad)", "Bias (ME)", "Lin's CCC (Agree)",
                     "RPD (Prec)", "RPIQ", "SMAPE (%)", "Moran's I", "Moran p")
    # NA in the Moran columns means the statistic could not be computed for this
    # point set (fewer than 3 points, no coordinate columns, or the neighbour
    # search failed) - it never means "no spatial structure was detected".
    na_marker <- '<span title="Not computable (see Run Log)">NA*</span>'

    get_metrics_df <- function(cv_list, data_list, label) {
      if(loc == "Total (Combined)") {
        # Pool in the auto-UTM zone of the combined centroid: pooled Moran's I
        # uses these coordinates, and EPSG:3857 distances are inflated by
        # 1/cos(latitude). perform_cv/.cv_to_df extract x/y from the geometry.
        all_cv <- pool_cv_sf(data_list)
        if(is.null(all_cv) || nrow(all_cv) == 0) {
          empty_df <- data.frame(Source=paste0(label, " (pooled CV)"), RMSE=NA, NRMSE_Pct=NA, MAE=NA, R2_Corr=NA, R2_NSE=NA, Bias_ME=NA, CCC=NA, RPD_Prec=NA, RPIQ=NA, SMAPE_Pct=NA, Moran_I=NA, Moran_P=NA)
          names(empty_df) <- metric_cols
          return(empty_df)
        }

        res <- perform_cv(all_cv)
        src_label <- paste0(label, " (pooled per-locality CV, n=", res$n, ")")
        rmse <- res$rmse
        nrmse <- res$nrmse_mean
        mae <- res$mae
        r2 <- res$r2
        nse <- res$nse
        me <- res$me
        ccc <- res$ccc
        rpd <- res$rpd
        rpiq <- res$rpiq
        smape <- res$smape
        moran_i <- res$moran_i
        moran_e <- res$moran_e
        moran_p <- res$moran_p
      } else {
        res <- cv_list[[loc]]
        n_obs <- if(!is.null(data_list[[loc]])) nrow(data_list[[loc]]) else NA
        src_label <- if(!is.null(res)) {
          paste0(label, " (", cv_type_label(n_obs, rv$cv_strategy_sel), ", n=", res$n, ")")
        } else {
          # An all-NA row used to be labelled plain "(CV)", indistinguishable
          # from a computed one; say that CV did not produce metrics here.
          paste0(label, " (CV unavailable - see Run Log)")
        }
        rmse <- if(!is.null(res)) res$rmse else NA
        nrmse <- if(!is.null(res)) res$nrmse_mean else NA
        mae  <- if(!is.null(res)) res$mae else NA
        r2   <- if(!is.null(res)) res$r2 else NA
        nse  <- if(!is.null(res)) res$nse else NA
        me   <- if(!is.null(res)) res$me else NA
        ccc  <- if(!is.null(res)) res$ccc else NA
        rpd  <- if(!is.null(res)) res$rpd else NA
        rpiq <- if(!is.null(res)) res$rpiq else NA
        smape <- if(!is.null(res)) res$smape else NA
        moran_i <- if(!is.null(res)) res$moran_i else NA
        moran_e <- if(!is.null(res)) res$moran_e else NA
        moran_p <- if(!is.null(res)) res$moran_p else NA
      }
                  res_df <- data.frame(
                    Source = src_label,
                    RMSE = round(rmse, 4),
                    NRMSE_Pct = round(nrmse, 4),
                    MAE = round(mae, 4),
                    R2_Corr = round(r2, 4),
                    R2_NSE = round(nse, 4),
                    Bias_ME = round(me, 4),
                    CCC = round(ccc, 4),
                    RPD_Prec = round(rpd, 4),
                    RPIQ = round(rpiq, 4),
                    SMAPE_Pct = round(smape, 4),
                    # The null expectation rides along as a per-row tooltip: I is
                    # centred on E[I] = -1/(n-1), not on 0, so an I marginally
                    # above zero is not evidence of clustering at small n.
                    Moran_I = if(is.na(moran_i)) na_marker else sprintf(
                      '<span title="Expected I under no spatial autocorrelation: E[I] = -1/(n-1) = %s">%s</span>',
                      if(is.na(moran_e)) "NA" else as.character(round(moran_e, 4)),
                      as.character(round(moran_i, 4))),
                    # Rendered like every other p in the app ("< 0.001" rather
                    # than a rounded 0), HTML-escaped because this table renders
                    # with escape = FALSE. NA on the all-pairs fallback path,
                    # which has no sampling distribution.
                    Moran_P = if(is.na(moran_p)) na_marker else htmltools::htmlEscape(format_p_value(moran_p))
                    )
                    names(res_df) <- metric_cols
                    res_df
                    }

    m_act <- get_metrics_df(rv$cv_metrics_act, rv$cv_data_act, "Actual Model")
    if(rv$has_predictions) {
      m_pre <- get_metrics_df(rv$cv_metrics_pre, rv$cv_data_pre, "Predicted Model")
      # escape = FALSE keeps the tooltip-bearing spans in the two Moran columns
      sci_dt(rbind(m_act, m_pre), escape = FALSE, header_tooltips = sci_metric_tooltips())
    } else {
      sci_dt(m_act, escape = FALSE, header_tooltips = sci_metric_tooltips())
    }
  })

  # ── Repeated cross-validation (opt-in) ────────────────────────────────────
  # The table above reports ONE fold realization (seed CV_FOLD_SEED). When the
  # user asked for repeated CV, this second table reports the mean and the
  # standard deviation of the same metrics across the alternative realizations,
  # which is the honest scale for comparing two methods: an RMSE gap smaller
  # than this SD is fold luck, not skill.
  cv_repeat_row <- function(summ, label) {
    if (is.null(summ)) return(NULL)
    fmt <- function(m, s) {
      if (!is.finite(m)) return("NA")
      digits <- if (abs(m) >= 100) 2 else 4
      # formatC, not round(): a small SD next to a larger mean would otherwise
      # print in scientific notation ("0.0287 ± 5e-04"), which reads as a
      # different quantity at a glance. drop0trailing keeps short values short.
      num <- function(x) formatC(round(x, digits), format = "f", digits = digits, drop0trailing = TRUE)
      paste0(num(m), " ± ", if (is.finite(s)) num(s) else "NA")
    }
    row <- data.frame(
      Source = paste0(label, " (", summ$n_repeats, " fold realizations, n=", summ$n, ")"),
      stringsAsFactors = FALSE
    )
    for (k in names(CV_REPEAT_METRICS)) {
      row[[k]] <- fmt(summ$mean[[k]], summ$sd[[k]])
    }
    names(row) <- c("Source", unname(CV_REPEAT_METRICS))
    row
  }

  cv_repeat_rows <- reactive({
    loc <- input$sel_loc_stats
    pick <- function(rep_summary) {
      if (is.null(rep_summary)) return(NULL)
      if (identical(loc, "Total (Combined)")) rep_summary$total else rep_summary$per_loc[[loc]]
    }
    rows <- list(cv_repeat_row(pick(rv$cv_repeats_act), "Actual Model"))
    if (isTRUE(rv$has_predictions)) {
      rows <- c(rows, list(cv_repeat_row(pick(rv$cv_repeats_pre), "Predicted Model")))
    }
    rows <- Filter(Negate(is.null), rows)
    if (!length(rows)) return(NULL)
    do.call(rbind, rows)
  })

  output$has_cv_repeats <- reactive({ !is.null(cv_repeat_rows()) })
  outputOptions(output, "has_cv_repeats", suspendWhenHidden = FALSE)

  output$cv_repeats_table <- DT::renderDataTable({
    df <- cv_repeat_rows()
    req(df)
    sci_dt(df, header_tooltips = sci_metric_tooltips())
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
                Metric = c("R2 (NSE/Traditional)", "R2 (Correlation)", "RMSE", "NRMSE (%)", "MAE", "NMAE (%)", "MBE (ML pred - observed)", "Lin's CCC (Agree)", "RPD (Precision)", "RPIQ", "SMAPE (%)"),
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
      params <- tryCatch(agro_params(), error = function(e) NULL)
      if(is.null(params) || input$color_style != "agro") return(sci_dt(data.frame(Status = "Select Agronomical styling and press APPLY TO MAPS & STATS (sidebar) for this method.")))
      
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
                   "cv_strategy_badge", "cv_repeats_table",
                   "stats_table_total", "stats_table_loc",
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
  
  vector_export_ext <- function(fmt) {
    switch(fmt %||% "shp", "shp" = "zip", "geojson" = "geojson", "kml" = "kml", "gpkg" = "gpkg", "zip")
  }

  # A downloadHandler cannot decline: whatever its content function does, the
  # browser has already opened the download URL, and a content function that
  # returns without writing its file leaves the user on a dead page with the
  # app behind them. So the two vector exports state their requirements BEFORE
  # the click - each reason below both greys its button out and becomes the
  # wrapper's hover tooltip. The handlers keep the same checks as a backstop
  # for the click-during-state-change race, raising them as errors so the
  # sentence is at least readable wherever the browser lands.
  polygon_block_reason <- reactive({
    if (length(rv$drawn_polygons) == 0)
      return("No polygons to export. Draw one first, using the drawing toolbar on the left edge of the map.")
    NULL
  })

  class_zone_block_reason <- reactive({
    if (is.null(rv$rast) && is.null(rv$rast_pred))
      return("No interpolated surface yet. Run an interpolation first.")
    if (!isTRUE(input$color_style %in% c("agro", "bin")))
      return("Class zones exist only under Agronomical or Binned map styling. Switch Map Styling in the sidebar (Agronomical also needs APPLY TO MAPS & STATS).")
    if (identical(input$map_view, "view_resid"))
      return("The residual view is not classified. Switch the Map Viewer to Actual, Predicted or Comparison to export its class zones.")
    NULL
  })

  set_export_block_state <- function(btn_id, wrap_id, reason) {
    shinyjs::toggleState(btn_id, condition = is.null(reason))
    shinyjs::runjs(sprintf("$('#%s').attr('title', %s);", wrap_id,
                           jsonlite::toJSON(reason %||% "", auto_unbox = TRUE)))
  }

  observe({
    set_export_block_state("polygon_download_btn", "polygon_dl_wrap",
                           polygon_block_reason())
  })

  observe({
    set_export_block_state("class_zone_download_btn", "class_zone_dl_wrap",
                           class_zone_block_reason())
  })

  output$polygon_download_btn <- downloadHandler(
    filename = function() {
      paste0("Drawn_Polygons_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".",
             vector_export_ext(input$polygon_export_format))
    },
    content = function(file) {
      reason <- polygon_block_reason()
      if (!is.null(reason)) stop(safeError(reason))

      sf_obj <- get_drawn_sf()
      if (is.null(sf_obj)) stop(safeError("No polygons to export. Draw one first."))

      tryCatch({
        write_vector_export(sf_obj, file, input$polygon_export_format, "drawn_polygons")
      }, error = function(e) {
        stop(safeError(paste("Export failed:", conditionMessage(e))))
      })
    }
  )

  # Class zones of the surface on screen, as a GIS vector layer. It follows the
  # Map Viewer's view switcher rather than the sidebar, for the same reason the
  # Quick Export button does: what is exported must be what is being looked at.
  class_zone_sf <- reactive({
    params <- tryCatch(classification_params(), error = function(e) NULL)
    if (is.null(params)) return(NULL)
    meta <- get_display_meta()
    if (is.null(meta)) return(NULL)

    labs <- if (isTruthy(input$color_style == "bin")) params$leg_labels else params$labels
    view <- input$map_view %||% "view_act"
    sources <- switch(view,
      "view_pred"  = list(list(r = rv$rast_pred, tag = "Predicted")),
      "view_comp"  = list(list(r = rv$rast, tag = "Actual"),
                          list(r = rv$rast_pred, tag = "Predicted")),
      list(list(r = rv$rast, tag = "Actual")))

    parts <- lapply(sources, function(s) {
      build_class_zone_sf(s$r, params, labs, s$tag, meta$label, meta$method)
    })
    parts <- Filter(Negate(is.null), parts)
    if (length(parts) == 0) return(NULL)
    do.call(rbind, parts)
  })

  output$class_zone_download_btn <- downloadHandler(
    filename = function() {
      meta <- tryCatch(get_display_meta(), error = function(e) NULL)
      var_tag <- gsub("[^A-Za-z0-9]+", "_", meta$actual %||% "surface")
      paste0("Class_Zones_", var_tag, "_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".",
             vector_export_ext(input$polygon_export_format))
    },
    content = function(file) {
      reason <- class_zone_block_reason()
      if (!is.null(reason)) stop(safeError(reason))

      zones <- tryCatch(class_zone_sf(), error = function(e) NULL)
      if (is.null(zones))
        stop(safeError("Could not build class zones for the displayed surface (no classified cells)."))

      withProgress(message = "Building class zone polygons...", {
        tryCatch({
          write_vector_export(zones, file, input$polygon_export_format, "class_zones")
        }, error = function(e) {
          stop(safeError(paste("Export failed:", conditionMessage(e))))
        })
      })
    }
  )


