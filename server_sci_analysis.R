# server_sci_analysis.R (sourced with local = TRUE inside server) - model
# diagnostics, variogram/importance/obs-pred plots, stats/area/metrics/kappa
# tables, notifications, log and polygon export.
  # Shared note box for the per-locality diagnostic panels (same look as the
  # "select a locality" hints these panels already used).
  sci_ui_note <- function(msg) {
    div(style="padding: 12px; background-color: var(--mn-surface-2); border: 1px dashed var(--mn-line-2); border-radius: 6px; color: var(--mn-text-3); font-style: italic; text-align: center;",
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
      v_emp <- rv$disp$v_emps[[paste0(loc, "_", type)]]
      v_fit <- rv$disp$v_fits[[paste0(loc, "_", type)]]
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
           rv$disp$v_emps[[paste0(loc, "_", type)]], rv$disp$v_fits[[paste0(loc, "_", type)]])
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

  # Table shape and the parameter arithmetic both live in vgm_params_table_df()
  # (ui_formatting.R), shared with the export registry so a downloaded sheet
  # cannot report a different sill or structural dependency than this card.
  # sci_dt(NULL) is the empty state, not a NULL payload: a DT output must never
  # be handed NULL (see sci_dt() in ui_components.R).
  output$vgm_params_table <- DT::renderDataTable({
    loc <- input$sel_loc_stats; req(loc)
    fits <- if (isTRUE(sci_vgm_tuning())) tuning_vgm_entries(rv$v_fit_list) else rv$disp$v_fits
    sci_dt(vgm_params_table_df(fits, loc))
  })
  build_tps_gcv_diag <- function(target) {
    loc <- input$sel_loc_stats; req(loc, identical(rv$disp$method, "TPS"))
    tryCatch({
      build_tps_gcv_plot(rv$disp$tps_gcv_data, loc, target)
    }, error = function(e) {
      sci_placeholder(paste("GCV Plot Error:\n", e$message), size = 4)
    })
  }

  output$tps_gcv_plot_act <- renderCachedPlot({
    p <- build_tps_gcv_diag("act"); req(p); p
  }, cacheKeyExpr = {
    # The run's own snapshot of the curves tuned for its keys.
    list("tps_gcv_act", input$sel_loc_stats, rv$results_rev, rv$disp$method, rv$disp$tps_gcv_data)
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
    
    # A failed CV fold leaves NA predictions; plot the scored pairs only.
    ok <- is.finite(df[[obs_col]]) & is.finite(df[[pre_col]])
    req(any(ok))
    df_plot <- data.frame(Observed = df[[obs_col]][ok], Predicted = df[[pre_col]][ok])
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
       # pool_cv_sf normalizes each locality's CV schema (an engine fallback
       # produces a different one), so the pooled scatter never goes blank.
       df <- pool_cv_sf(data_list)
       req(df)
       df <- st_drop_geometry(df)
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
    list("tps_gcv_pre", input$sel_loc_stats, rv$results_rev, rv$disp$method, rv$disp$tps_gcv_data)
  }, cache = "session")

  # ── Directional variogram (anisotropy diagnostic) ────────────────────────
  # Computed lazily in the main session from the displayed run's points, so a
  # user who never opens this card pays nothing and every engine (including
  # IDW/TPS, which fit no variogram) gets the same check. calc_directional_
  # variogram re-projects to a metric CRS itself — rv$sf carries the user's
  # chosen DISPLAY crs, which may well be geographic, and a bearing in degrees
  # of longitude is not a bearing on the ground.
  # The two switches over the card pick one of four columns of rv$sf: the DATA
  # (measured values `v`, or the uploaded ML prediction column `pv`) times the
  # SOURCE (those values, or that surface's CV residuals). An uploaded
  # prediction column is a field in its own right — a model's output carries its
  # own spatial structure, usually smoother than the measurements it
  # approximates — so its anisotropy is read directly rather than inferred from
  # the measured side.
  dir_vgm_value_col <- function(tgt, src) {
    if (identical(src, "resid")) {
      if (identical(tgt, "pre")) "model_resid_pre" else "model_resid_act"
    } else {
      if (identical(tgt, "pre")) "pv" else "v"
    }
  }

  build_directional_vgm_diag <- function() {
    loc <- input$sel_loc_stats; req(loc)
    req(rv$sf)
    # Guarded rather than trusted: the switch loses its second choice when a run
    # without a prediction side is displayed, and the stale input value can
    # still reach a download handler before that update lands.
    tgt <- if (identical(input$dir_vgm_target, "pre") && isTRUE(disp_has_pred())) "pre" else "act"
    src <- if (identical(input$dir_vgm_source, "resid")) "resid" else "v"
    surf <- if (identical(tgt, "pre")) "uploaded-prediction" else "measured-value"

    pts <- rv$sf
    if (loc != "Total (Combined)" && "loc" %in% colnames(pts)) {
      pts <- pts[!is.na(pts$loc) & pts$loc == loc, ]
    }
    if (nrow(pts) == 0) return(sci_placeholder("No points available for this locality."))

    value_col <- dir_vgm_value_col(tgt, src)
    if (!value_col %in% colnames(pts) || !any(!is.na(pts[[value_col]]))) {
      return(sci_placeholder(
        if (identical(src, "resid")) {
          # The two sources do not have the same precondition, and a reader who
          # has just seen the values plot plausibly reads an empty residual
          # panel as a fault: values are the uploaded column itself, residuals
          # exist only where this run's interpolation AND its cross-validation
          # completed for that surface. State that, not just the absence.
          paste0("No cross-validation residuals are stored for the ", surf, " surface of this run.\n",
                 "Values are read from the uploaded table, so they plot whenever that column is\n",
                 "filled; residuals exist only where this run's interpolation and its\n",
                 "cross-validation both completed for this surface. The Run Log on this tab\n",
                 "reports the cause.")
        } else if (identical(tgt, "pre")) {
          paste0("This run carries no uploaded prediction values.\n",
                 "The prediction column of the mapped variable is empty for these points.")
        } else {
          "No measured values are available for this run."
        }))
    }

    what <- if (identical(src, "resid")) {
      paste0("CV residuals, ", surf, " surface")
    } else if (identical(tgt, "pre")) {
      "uploaded prediction values"
    } else {
      "measured values"
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
    list("dir_vgm", input$sel_loc_stats, input$dir_vgm_target, input$dir_vgm_source,
         rv$results_rev, is.null(rv$sf), if (is.null(rv$sf)) 0L else nrow(rv$sf))
  }, cache = "session")

  # "Uploaded predictions" is offered by the same rule that gates the Validation
  # Diagnostics (Predicted) block, so the two cannot disagree about whether the
  # displayed run has a prediction side.
  observeEvent(list(rv$disp, rv$has_predictions), {
    choices <- c("Actual data" = "act")
    if (isTRUE(disp_has_pred())) choices <- c(choices, "Uploaded predictions" = "pre")
    sel <- if (isTruthy(input$dir_vgm_target) && input$dir_vgm_target %in% choices) {
      input$dir_vgm_target
    } else {
      "act"
    }
    shinyWidgets::updateRadioGroupButtons(session, "dir_vgm_target", choices = choices,
                                          selected = sel, size = "sm")
  }, ignoreNULL = FALSE)

  # The values choice names the column it reads, and that column differs by
  # target; only the label moves, so the selection carries over unchanged.
  observeEvent(input$dir_vgm_target, {
    lbl <- if (identical(input$dir_vgm_target, "pre")) "Uploaded prediction values" else "Measured values"
    shinyWidgets::updateRadioGroupButtons(
      session, "dir_vgm_source",
      choices = stats::setNames(c("v", "resid"), c(lbl, "Model residuals (CV)")),
      selected = if (identical(input$dir_vgm_source, "resid")) "resid" else "v",
      size = "sm")
  })

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

  # Both descriptive cards read stats_table_vectors() (ui_formatting.R), the
  # frame their exports read too: the uploaded rows of the DISPLAYED run's
  # localities ("Total (Combined)" = the localities that run covered), and
  # the prediction column that run mapped, only when it mapped predictions.
  output$stats_table_total <- DT::renderDataTable({
    req(rv$user_data, length(rv$loc_names) > 1)
    meta <- get_display_meta()
    req(meta)
    sv <- stats_table_vectors(rv$user_data, meta, rv$mapping$loc, meta$localities)
    if (is.null(sv)) return(sci_dt(NULL))
    sci_dt(summary_stats_df(sv$act, sv$pre, labels = c("Total_Actual", "Total_Predicted"),
                            round_values = TRUE))
  })

  output$stats_table_loc <- DT::renderDataTable({
    req(rv$user_data, input$sel_loc_stats)
    if(input$sel_loc_stats == "Total (Combined)") return(sci_dt(NULL))
    meta <- get_display_meta()
    req(meta)
    sv <- stats_table_vectors(rv$user_data, meta, rv$mapping$loc, input$sel_loc_stats)
    if (is.null(sv)) return(sci_dt(NULL))
    sci_dt(summary_stats_df(sv$act, sv$pre, labels = c("Selected_Actual", "Selected_Predicted"),
                            round_values = TRUE))
  })

  # Hectares per class for ONE surface, UNROUNDED and in class order (0 for a
  # class that wins no cell). NULL when the surface cannot be classified.
  #
  # terra::expanse() defaults to transform = TRUE, which reprojects every cell
  # to lon/lat to get its geodesic area. That cost is linear in the raster's
  # TOTAL cell count, NA padding included, so it must never be handed a sparse
  # grid - see class_area_ha_sum() below.
  class_area_ha <- function(r_obj, params, r_id = NULL) {
    if (is.null(r_obj) || is.null(params)) return(NULL)

    cache_key <- NULL
    if (!is.null(r_id)) {
      brk_str <- if (!is.null(params$brks)) paste(params$brks, collapse = "_") else "nobrks"
      # "ha_" namespaces the unrounded vectors apart from anything else the
      # cache has held under a (run, id, breaks) key.
      cache_key <- paste0("ha_", rv$run_counter, "_", r_id, "_", brk_str)
      if (exists(cache_key, envir = area_calc_cache)) {
        return(get(cache_key, envir = area_calc_cache))
      }
    }
    # unwrap only on a cache miss (deserializing a packed raster is expensive)
    if (inherits(r_obj, "PackedSpatRaster")) r_obj <- terra::unwrap(r_obj)

    out <- tryCatch({
      r_class <- classify(r_obj[[1]], params$rcl_mat, right = FALSE)
      area_df <- as.data.frame(expanse(r_class, unit = "ha", byValue = TRUE))

      ha <- rep(0, params$n_c)
      if ("value" %in% names(area_df)) {
        class_names <- if (isTruthy(input$color_style == "bin")) params$leg_labels else params$labels
        # A categorical layer reports `value` as the label rather than the code
        is_label <- any(as.character(area_df$value) %in% class_names)
        idx <- if (is_label) {
          match(as.character(area_df$value), class_names)
        } else {
          suppressWarnings(as.numeric(as.character(area_df$value)))
        }
        keep <- !is.na(idx) & idx >= 1 & idx <= params$n_c
        if (any(keep)) {
          agg <- tapply(area_df$area[keep], idx[keep], sum, na.rm = TRUE)
          ha[as.integer(names(agg))] <- as.numeric(agg)
        }
      }
      ha
    }, error = function(e) structure(character(0), area_error = conditionMessage(e)))

    if (!is.null(cache_key)) assign(cache_key, out, envir = area_calc_cache)
    out
  }

  # Total across the localities of a run, summed on the UNROUNDED per-locality
  # hectares so the total is the sum of the rows above it to the last decimal.
  #
  # Deliberately NOT computed on the merged rv$rast: terra::merge() spans the
  # union of the locality extents, so two fields 25 km apart share a grid that
  # is ~97% NA. expanse() charges for those cells - measured at 109 s and
  # 1.1 GB of working set on a 13 M-cell merged grid holding 320 k real cells,
  # against 1.4 s per locality - and four of these run in the flush that
  # applies a classification. The total is allowed only after the domain
  # overlap check below confirms that the locality areas can be added.
  class_area_ha_sum <- function(r_list, id_prefix, params) {
    r_list <- Filter(Negate(is.null), r_list)
    if (length(r_list) == 0) return(NULL)
    nms <- names(r_list)
    total <- rep(0, params$n_c)
    for (i in seq_along(r_list)) {
      tag <- if (!is.null(nms) && !is.na(nms[i]) && nzchar(nms[i])) nms[i] else as.character(i)
      ha <- class_area_ha(r_list[[i]], params, paste0(id_prefix, "_", tag))
      if (is.null(ha) || length(ha) != params$n_c) return(ha)  # NULL, or the error marker
      total <- total + ha
    }
    total
  }

  # Shapes a hectare vector (or an error marker) into the displayed table.
  area_ha_to_df <- function(ha, params) {
    if (is.null(params)) return(data.frame(Status = "Awaiting classification - press Apply to maps and statistics under Map Styling in the sidebar"))
    if (is.null(ha)) return(NULL)
    err <- attr(ha, "area_error")
    if (!is.null(err)) return(data.frame(Error = as.character(err)))
    class_names <- if (isTruthy(input$color_style == "bin")) params$leg_labels else params$labels
    data.frame(Class = class_names, Ha = round(ha, 2))
  }

  calc_area_df <- function(r_obj, r_id = NULL) {
    if (is.null(r_obj)) return(NULL)
    # error-only: catching `condition` here also unwound message() conditions
    # escaping classification_params(), aborting the reactive mid-evaluation
    # and poisoning it for the map renderers (Jenks fell back to continuous)
    params <- tryCatch(classification_params(), error = function(e) NULL)
    if (is.null(params)) return(area_ha_to_df(NULL, NULL))
    area_ha_to_df(class_area_ha(r_obj, params, r_id), params)
  }

  area_df_total_act <- reactive({
    req(rv$rast)
    ov <- (rv$bound_overlap_m2 %||% c(act = 0))[["act"]]
    if (!is.finite(ov)) return(data.frame(Status = "Locality overlap could not be measured; see the per-locality area tables."))
    if (ov > 0.5) return(data.frame(Status = sprintf(
      "Overlapping locality boundaries would count %.3g ha more than once; no combined total is reported. See the per-locality tables.", ov / 1e4)))
    params <- tryCatch(classification_params(), error = function(e) NULL)
    if (is.null(params)) return(area_ha_to_df(NULL, NULL))
    ha <- class_area_ha_sum(rv$rast_list_act, "loc_act", params)
    # Only a run that stored no per-locality surface falls back to the merged
    # grid; every path that fills rv$rast fills rv$rast_list_act with the
    # rasters it was merged from.
    if (is.null(ha)) ha <- class_area_ha(rv$rast, params, "total_act_merged")
    area_ha_to_df(ha, params)
  })

  area_df_total_pre <- reactive({
    req(rv$rast_pred)
    ov <- (rv$bound_overlap_m2 %||% c(pre = 0))[["pre"]]
    if (!is.finite(ov)) return(data.frame(Status = "Locality overlap could not be measured; see the per-locality area tables."))
    if (ov > 0.5) return(data.frame(Status = sprintf(
      "Overlapping locality boundaries would count %.3g ha more than once; no combined total is reported. See the per-locality tables.", ov / 1e4)))
    params <- tryCatch(classification_params(), error = function(e) NULL)
    if (is.null(params)) return(area_ha_to_df(NULL, NULL))
    ha <- class_area_ha_sum(rv$rast_list_pre, "loc_pre", params)
    if (is.null(ha)) ha <- class_area_ha(rv$rast_pred, params, "total_pre_merged")
    area_ha_to_df(ha, params)
  })

  output$area_table_total_act <- DT::renderDataTable({ req(length(rv$loc_names) > 1, input$color_style %in% c("agro", "bin")); sci_dt(area_df_total_act()) })
  output$area_table_total_pre <- DT::renderDataTable({ req(length(rv$loc_names) > 1, input$color_style %in% c("agro", "bin")); sci_dt(area_df_total_pre()) })

  output$area_table_loc_act <- DT::renderDataTable({
    req(rv$rast_list_act, input$color_style %in% c("agro", "bin")); loc <- input$sel_loc_stats
    if(loc == "Total (Combined)") sci_dt(NULL) else sci_dt(calc_area_df(rv$rast_list_act[[loc]], paste0("loc_act_", loc)))
  })
  output$area_table_loc_pre <- DT::renderDataTable({
    req(rv$rast_list_pre, input$color_style %in% c("agro", "bin")); loc <- input$sel_loc_stats
    if(loc == "Total (Combined)") sci_dt(NULL) else sci_dt(calc_area_df(rv$rast_list_pre[[loc]], paste0("loc_pre_", loc)))
  })

  # Class-area and class-agreement exports follow the CLASSIFICATION, not the
  # run. Both tables come into existence only once the surface is classified,
  # and Apply is normally pressed after a run - the sidebar hint says so - so
  # registering them only at run completion left the Export panel without the
  # two tables the Scientific Analysis tab had just gained. They are now
  # (re-)registered whenever the committed classification changes, and
  # calc_area_df's per-run cache means this costs nothing the on-screen tables
  # have not already paid. Every previous area/agreement sheet is dropped
  # first: a class system that changed (Agronomical -> Binned), a surface that
  # went Continuous, or an agreement that became non-computable must not leave
  # sheets from the old classification beside the new ones.
  # classification_params() is read under tryCatch here as at every other call
  # site: an error in an event expression is an unhandled observer error and
  # ends the session.
  observeEvent(list(tryCatch(classification_params(), error = function(e) NULL),
                    rv$results_rev), {
    req(rv$disp)
    reg <- isolate(rv$export_registry)
    stale <- grepl("^table_(area|kappa)_", names(reg))
    if (any(stale)) rv$export_registry <- reg[!stale]

    params_now <- tryCatch(classification_params(), error = function(e) NULL)
    if (is.null(params_now) || !isTruthy(input$color_style %in% c("agro", "bin"))) return()
    meta <- get_display_meta()
    req(meta, !is.null(rv$rast))
    has_pre <- isTRUE(rv$disp$comp_mode) || !identical(rv$disp$value_type, "actual")

    reg_if_df <- function(id, label, df) {
      if (is.data.frame(df) && nrow(df) > 0 && !identical(names(df), "Status")) {
        register_export_item(id, paste(meta$label, "-", label), "table", df, meta$category)
      }
    }

    reg_if_df("table_area_total", "Total Area Coverage",
              tryCatch(area_df_total_act(), error = function(e) NULL))
    if (has_pre && !is.null(rv$rast_pred)) {
      reg_if_df("table_area_pre_total", "Total Area Coverage (Predicted)",
                tryCatch(area_df_total_pre(), error = function(e) NULL))
    }
    for (l in names(rv$rast_list_act)) {
      if (is.null(rv$rast_list_act[[l]])) next
      reg_if_df(paste0("table_area_loc_", l), paste(l, "- Area Coverage"),
                tryCatch(calc_area_df(rv$rast_list_act[[l]], paste0("loc_act_", l)),
                         error = function(e) NULL))
    }
    if (has_pre) {
      for (l in names(rv$rast_list_pre)) {
        if (is.null(rv$rast_list_pre[[l]])) next
        reg_if_df(paste0("table_area_pre_loc_", l), paste(l, "- Area Coverage (Predicted)"),
                  tryCatch(calc_area_df(rv$rast_list_pre[[l]], paste0("loc_pre_", l)),
                           error = function(e) NULL))
      }
    }

    # Agreement needs an uploaded prediction column AND agronomical classes;
    # quartile binning is a screen-side choice with no map counterpart, so the
    # export follows the map's own class limits.
    params_k <- tryCatch(agro_params(), error = function(e) NULL)
    if (has_pre && !is.null(params_k) && !is.null(rv$sf)) {
      df_k <- rv$sf %>% st_drop_geometry() %>% filter(!is.na(v), !is.na(pv))
      reg_if_df("table_kappa_total", "Total Classification Performance (Agronomical)",
                agreement_metrics_df(compute_agreement_metrics(df_k$v, df_k$pv,
                                                               method = "agro", params = params_k)))
      for (l in unique(df_k$loc)) {
        d_l <- df_k[df_k$loc == l, , drop = FALSE]
        reg_if_df(paste0("table_kappa_loc_", l), paste(l, "- Classification Performance (Agronomical)"),
                  agreement_metrics_df(compute_agreement_metrics(d_l$v, d_l$pv,
                                                                 method = "agro", params = params_k)))
      }
    }
  }, ignoreInit = TRUE)

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
        style = "font-size: 0.82em; color: var(--mn-text-2); margin: -4px 0 8px 0;",
        tags$span(style = "color: var(--mn-text-3);",
                  "Pooled R²/NSE are computed against the pooled mean; when localities differ in their means, between-locality variance inflates these scores. Judge model skill on the per-locality rows.")
      )
    }
    # Repeated CV never moves the numbers above (realization 1 keeps the fixed
    # seed); say where the extra realizations are reported instead.
    n_rep <- rv$cv_repeats_sel %||% 1L
    repeat_note <- if (is.numeric(n_rep) && n_rep > 1) {
      tags$span(style = "color: var(--mn-text-3);",
                sprintf(" Repeated CV is on (%d fold realizations); the values here are realization 1, the spread is in the table below.", n_rep))
    }
    # What each fold re-estimates. Every kriging engine refits from its own
    # training samples, so the held-out sample contributes its coordinates and
    # nothing else; the remaining reuse is IDW's distance power and a FIXED TPS
    # lambda, both selected once on the full point set. rv$disp records the
    # method, not whether the power/lambda was tuned or typed, so the IDW/TPS
    # wording stays conditional - the reader knows which button they pressed.
    method_now <- rv$disp$method %||% ""
    reuse_txt <- switch(
      method_now,
      "OK" = " refits its variogram from each fold's training samples; the held-out sample contributes its coordinates only.",
      "CK" = " re-screens its covariates, re-standardizes them and refits the linear model of coregionalization in each fold.",
      "RK" = , "RFK" = " re-screens its covariates, re-interpolates them at the held-out samples, and refits the trend and the residual variogram in each fold.",
      "IDW" = " re-solves each fold with one distance power. If that power came from OPTIMIZE IDW FACTORS it was selected on the full point set, so the held-out point contributed to it, unlike the kriging engines, which refit inside every fold; a power you typed yourself carries no such reuse. See Scientific Guide §5.",
      "TPS" = " re-fits its spline inside every fold, but a fixed lambda - typed into the slider or taken from OPTIMIZE TPS LAMBDA - is reused unchanged in every fold and was selected on the full point set, unlike the kriging engines, which refit inside every fold. Auto (GCV) re-selects lambda inside each fold and carries no such reuse. See Scientific Guide §5.",
      NULL
    )
    infos <- c(rv$cv_info_act, if (isTRUE(rv$has_predictions)) rv$cv_info_pre)
    infos <- Filter(Negate(is.null), infos)
    # An applied manual model cannot be refitted without scoring a different
    # model than the one that drew the map, so those localities are reused and
    # labelled conditional. Name them: it is the one exception to the sentence
    # above.
    cond_locs <- unique(names(Filter(function(x) !is.null(x$conditional), infos)))
    cond_txt <- if (length(cond_locs)) {
      paste0(" Conditional on an applied variogram (reused in every fold): ",
             paste(cond_locs, collapse = ", "), ".")
    }
    # Only where the Predicted surface actually borrowed the measured-value
    # variogram under Auto-Fit (the worker records its column). A borrowed
    # APPLIED model is conditional instead, and cond_txt names it.
    shared_txt <- if (isTRUE(rv$has_predictions) &&
                      any(vapply(rv$cv_info_pre, function(x) identical(x$vgm_col, "v"), logical(1)))) {
      " The Predicted surface is kriged with the measured-value variogram, refitted in each fold."
    }
    screen_txt <- {
      diff_locs <- Filter(function(x) isTRUE((x$screen$n_differ %||% 0) > 0), infos)
      if (length(diff_locs)) {
        k <- sum(vapply(diff_locs, function(x) as.integer(x$screen$n_differ), integer(1)))
        n <- sum(vapply(diff_locs, function(x) as.integer(x$screen$n_folds), integer(1)))
        paste0(" In ", k, " of ", n, " folds (", paste(unique(names(diff_locs)), collapse = ", "),
               ") the fold's covariate screen kept a different set than the map; see the Run Log.")
      }
    }
    cov_txt <- {
      short <- Filter(function(m) isTRUE((m$coverage %||% 1) < 1),
                      c(rv$cv_metrics_act, if (isTRUE(rv$has_predictions)) rv$cv_metrics_pre))
      if (length(short)) {
        " Some rows are INCOMPLETE: part or all of their samples received no cross-validation prediction (see the Run Log), and their metrics describe the predicted samples only."
      }
    }
    refit_note <- if (!is.null(reuse_txt) || !is.null(cov_txt)) {
      tags$div(
        style = "font-size: 0.82em; color: var(--mn-text-2); margin: -4px 0 8px 0;",
        tags$span(style = "color: var(--mn-text-3);",
                  paste0(get_method_label(rv$disp$method), reuse_txt %||% "",
                         cond_txt %||% "", shared_txt %||% "", screen_txt %||% "", cov_txt %||% ""))
      )
    }
    tagList(
      tags$div(
        style = "font-size: 0.82em; color: var(--mn-text-2); margin: -4px 0 8px 0;",
        tags$span(style = "font-weight: 600;", "Cross-validation: "),
        tags$span(label),
        tags$span(style = "color: var(--mn-text-3);", " (applies to these metrics only, not the map)."),
        repeat_note
      ),
      refit_note,
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
    # CV_METRIC_LABELS (ui_formatting.R) is that one definition; the export
    # flavour of this table reads the same vector. Moran's null expectation is
    # dropped here because it rides along as a per-row tooltip below - a file
    # cannot carry a tooltip, so the export keeps it as a column.
    metric_cols <- c("Source", unname(CV_METRIC_LABELS[setdiff(names(CV_METRIC_LABELS), "moran_e")]))
    # NA in the Moran columns means the statistic could not be computed for this
    # point set (fewer than 3 points, no coordinate columns, or the neighbour
    # search failed) - it never means "no spatial structure was detected".
    na_marker <- '<span title="Not computable (see Run Log)">NA*</span>'

    # The Source cell states WHAT was scored, not only how: the fold plan, the
    # cross-validation population, how many of the expected samples got a
    # prediction, and whether the folds were conditional on a model the user
    # applied. The experiment id and the refit statement ride along as a hover,
    # which a table cell can carry and a file cannot - the export has both as
    # columns of their own.
    source_cell <- function(label, design, res, info) {
      n_pred <- res$n %||% NA_integer_
      n_exp <- res$n_expected %||% NA_integer_
      incomplete <- !is.na(n_exp) && n_exp > 0 && !isTRUE(n_pred == n_exp)
      named_pop <- !is.null(info) && !is.na(info$population %||% NA_character_)
      parts <- c(design,
                 if (named_pop) info$population,
                 if (incomplete) paste0("n=", n_pred, " of ", n_exp) else paste0("n=", n_pred),
                 if (incomplete) "INCOMPLETE",
                 if (!is.null(info$conditional)) paste0("conditional on ", info$conditional),
                 if (isTRUE((info$n_conditional %||% 0) > 0))
                   paste0(info$n_conditional, " of ", info$n_localities, " localities conditional"))
      txt <- paste0(label, " (", paste(parts, collapse = ", "), ")")
      tip <- c(if (named_pop && !is.na(info$pop_id %||% NA_character_))
                 paste0("CV population ID: ", info$pop_id),
               if (!is.na(info$refit %||% NA_character_))
                 paste0("Model refit: ", info$refit))
      if (!length(tip)) return(htmltools::htmlEscape(txt))
      sprintf('<span title="%s">%s</span>',
              htmltools::htmlEscape(paste(tip, collapse = ". "), attribute = TRUE),
              htmltools::htmlEscape(txt))
    }

    get_metrics_df <- function(cv_list, data_list, label, info_list) {
      if(loc == "Total (Combined)") {
        # Pool in the auto-UTM zone of the combined centroid: pooled Moran's I
        # uses these coordinates, and EPSG:3857 distances are inflated by
        # 1/cos(latitude). perform_cv/.cv_to_df extract x/y from the geometry.
        res <- perform_pooled_cv(data_list, cv_list)
        if(is.null(res)) {
          empty_df <- data.frame(Source=paste0(label, " (pooled CV)"), RMSE=NA, NRMSE_Pct=NA, MAE=NA, R2_Corr=NA, R2_NSE=NA, Bias_ME=NA, CCC=NA, RPD_Prec=NA, RPIQ=NA, SMAPE_Pct=NA, Moran_I=NA, Moran_P=NA)
          names(empty_df) <- metric_cols
          return(empty_df)
        }

        src_label <- source_cell(label, "pooled per-locality CV", res,
                                 pooled_cv_population(info_list[names(data_list)]))
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
          source_cell(label, cv_type_label(n_obs, rv$cv_strategy_sel), res, info_list[[loc]])
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

    m_act <- get_metrics_df(rv$cv_metrics_act, rv$cv_data_act, "Actual Model", rv$cv_info_act)
    if(rv$has_predictions) {
      m_pre <- get_metrics_df(rv$cv_metrics_pre, rv$cv_data_pre, "Predicted Model", rv$cv_info_pre)
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
    # Realizations can differ in how many samples they managed to predict, so
    # the count is a range whenever they do, always against what was expected.
    n_txt <- if (!isTRUE(summ$n_min == summ$n_max)) {
      paste0("n=", summ$n_min, "-", summ$n_max)
    } else paste0("n=", summ$n)
    if (!is.null(summ$n_expected) && !is.na(summ$n_expected) &&
        !isTRUE(summ$n_min == summ$n_expected && summ$n_max == summ$n_expected)) {
      n_txt <- paste0(n_txt, " of ", summ$n_expected)
    }
    row <- data.frame(
      Source = paste0(label, " (", summ$n_repeats, " fold realizations, ", n_txt, ")"),
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

          # ONE metric dictionary: pred_perf_df() (ui_formatting.R) wraps
          # perform_cv(), the app's metric authority, so this table, the Total
          # and per-locality exports, and Model Performance cannot drift apart.
          # It carries calc_ccc's population moments (yardstick's ccc_vec
          # defaults to the sample-moment variant removed in 1.0.8), NRMSE
          # against |mean| (a signed denominator reports a negative error
          # percentage for an anomaly variable), and NA - never Inf or NaN -
          # for every degenerate ratio.
          sci_dt(pred_perf_df(df$v, df$pv, round_values = TRUE))
        })
  output$kappa_table <- DT::renderDataTable({
    req(rv$sf, input$sel_loc_stats, input$kappa_bin_method)
    
    loc <- input$sel_loc_stats
    
    df <- rv$sf %>% st_drop_geometry() %>% filter(!is.na(v), !is.na(pv))
    if(loc != "Total (Combined)") {
      df <- df %>% filter(loc == !!loc)
    }

    params <- NULL
    if (input$kappa_bin_method == "agro") {
      params <- tryCatch(agro_params(), error = function(e) NULL)
      if(is.null(params) || input$color_style != "agro") return(sci_dt(data.frame(Status = "Select Agronomical styling and press Apply to maps and statistics (sidebar) for this method.")))
    }

    # All binning and confusion-matrix arithmetic lives in
    # compute_agreement_metrics() (spatial_metrics.R); this block only chooses
    # the data and formats the answer.
    ag <- compute_agreement_metrics(df$v, df$pv, method = input$kappa_bin_method, params = params)
    if(!is.null(ag$status)) return(sci_dt(data.frame(Status = ag$status)))

    sci_dt(agreement_metrics_df(ag, round_values = TRUE))
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
  # The tally exists to stop one warning being re-notified as the log grows
  # WITHIN a run; a new run rebuilds rv$log from scratch, so carrying it over
  # would silence a warning that is still true the second time round (e.g. an
  # unchanged strict buffer/resolution pair).
  observeEvent(rv$run_counter, last_notified_warnings(character(0)))
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
      return("Class zones exist only under Agronomical or Binned map styling. Switch Map Styling in the sidebar (Agronomical also needs Apply to maps and statistics).")
    if (identical(map_view_base(), "view_resid"))
      return("The residual view is not classified. Switch the Map Viewer to Actual, Predicted or Comparison to export its class zones.")
    if (map_view_layer() %in% c("se", "var") && isTRUE(rv$disp$has_variance))
      return("Standard-error and variance maps are not classified. Switch the Map Viewer to Actual, Predicted or Comparison to export their class zones.")
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
    view <- map_view_base()
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
