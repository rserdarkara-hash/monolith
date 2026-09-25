# server_sci_analysis.R (sourced with local = TRUE inside server) - model
# diagnostics, variogram/importance/obs-pred plots, stats/area/metrics/kappa
# tables, notifications, log and polygon export.
  # Shared note box for the per-locality diagnostic panels (same look as the
  # "select a locality" hints these panels already used).
  sci_ui_note <- function(msg) {
    div(style="padding: 12px; background-color: var(--mn-surface-2); border: 1px dashed var(--mn-line-2); border-radius: 6px; color: var(--mn-text-3); font-style: italic; text-align: center;",
        msg)
  }

  # Run warnings, in the panel rather than only in the run log. Each entry is
  # "<locality> (<surface>): <message>" as persist_run_warnings() recorded it;
  # the card is absent when the run raised none.
  output$run_warnings_card <- renderUI({
    w <- rv$run_warnings
    if (is.null(w) || length(w) == 0) return(NULL)
    sci_card("Run Warnings",
             "Conditions the interpolation reported for individual localities.",
             tags$ul(style = "margin: 0; padding-left: 20px; color: var(--mn-text-2);",
                     lapply(w, function(x) tags$li(x))))
  })

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
      # Unlike the per-locality plots (internal trend-residual variogram of
      # the fitted model), the combined view pools CV residuals within
      # localities (pooled_within_variogram), so label it as such.
      v <- pooled_within_variogram(split(rv$sf[col_resid], rv$sf$loc), col_resid)
      if (is.null(v)) {
        return(sci_placeholder(sprintf("No locality has %d cross-validation residuals %s.",
                                       POOLED_VGM_MIN_N, tolower(title_suffix)), size = 4))
      }
      cap <- pooled_within_caption(v)
      build_variogram_ggplot(v, title = paste0("Pooled within-locality CV residual variogram ", title_suffix,
                                               ": ", cap$count),
                             subtitle = cap$note)
    } else {
      v_emp <- rv$disp$v_emps[[paste0(loc, "_", type)]]
      v_fit <- rv$disp$v_fits[[paste0(loc, "_", type)]]
      if (is.null(v_emp) || is.null(v_fit)) {
        return(sci_placeholder(sprintf(paste0(
          "No fitted variogram is stored for \"%s\" %s.\nThe locality failed before the variogram step; ",
          "see the Run Log on this tab."), loc, tolower(title_suffix)), size = 4))
      }
      # A locality whose trend step failed stored the OK fallback's variogram
      # of the measured values (disp_trend_fallback), not a residual variogram.
      ttl <- if (disp_trend_fallback(paste0(loc, "_", type))) {
        paste("Variogram of Measured Values - Ordinary Kriging Fallback", paste0(title_suffix, ":"), loc)
      } else {
        paste("Internal Residual Variogram", paste0(title_suffix, ":"), loc)
      }
      # The model the run kriged with, as the other variogram panels state it.
      build_fitted_variogram_plot(v_emp, v_fit, title = ttl)
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
    # The empirical variogram the LMC was fitted to, stored with the fit
    # (.ck_fit_lmc): the run's own lag classes, and cross-variograms from the
    # locations carrying both variables. Recomputing it from the object would
    # re-bin with gstat's defaults and, for a heterotopic design, return the
    # pseudo cross-variogram.
    vm <- attr(g, "monolith_vm")
    req(vm)
    # Panel strips carry the gstat ids (syntactic aliases of the target and
    # covariate columns, ck_id_columns); map them to the run variable's display
    # name and the covariate labels/column names per the tab's naming radio.
    cols <- ck_id_columns(g)
    target_name <- sci_disp_label() %||% names(cols)[1]
    id_labels <- vapply(names(cols), function(id) {
      if (cols[[id]] %in% c("v", "pv")) target_name else get_var_label(cols[[id]], sci_vars_meta())
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
  # The tuning store holds variograms of the measured values; the displayed
  # run's fits are residual variograms for RK and RFK (vgm_params_title).
  output$vgm_params_title <- renderText({
    paste(vgm_params_title(if (!isTRUE(sci_vgm_tuning())) rv$disp$method), "(per locality)")
  })
  output$vgm_params_table <- DT::renderDataTable({
    loc <- input$sel_loc_stats; req(loc)
    tuning <- isTRUE(sci_vgm_tuning())
    fits <- if (tuning) tuning_vgm_entries(rv$v_fit_list) else rv$disp$v_fits
    df <- vgm_params_table_df(fits, loc, of = if (!tuning) disp_vgm_of(fits))
    # A named locality transposes to three narrow character columns and needs no
    # scrollX; the pooled listing is wide and numeric, so it keeps both.
    if (identical(loc, "Total (Combined)")) {
      # "Sill Resolved" is logical: the significant-digit renderer would turn
      # TRUE into 1.
      sci_dt(df, signif_cols = if (!is.null(df))
        setdiff(names(df), c("Locality", "Target", "Variogram Of", "Model", "Sill Resolved")))
    } else {
      # The cells carry the sill qualifier as a tooltip span (.vgm_params_chr).
      sci_dt(df, scroll_x = FALSE, escape = FALSE)
    }
  })
  # A named locality's transposed table has no "Variogram Of" column: the note
  # names a column that holds the OK fallback's variogram of measured values.
  output$vgm_params_note <- renderUI({
    loc <- input$sel_loc_stats
    req(loc, !identical(loc, "Total (Combined)"), !isTRUE(sci_vgm_tuning()))
    keys <- intersect(paste0(loc, c("_act", "_pre")), names(rv$disp$v_fits))
    fb <- keys[disp_trend_fallback(keys)]
    if (!length(fb)) return(NULL)
    cols <- ifelse(grepl("_act$", fb), "Actual", "Predicted")
    tags$div(class = "mn-table-note", sprintf(paste0(
      "%s: the %s trend model of this locality was not fitted, so the Ordinary Kriging fallback ",
      "kriged the measured values and this is their variogram, not a residual variogram. ",
      "The Run Log names the cause."), paste(cols, collapse = " and "), rv$disp$method))
  })
  # The smoothing / power selection panels read the displayed run's own fit
  # record for the locality (rv$disp$regional_params), never the tuning store.
  run_fit_record <- function(field, target) {
    loc <- input$sel_loc_stats
    if (identical(loc, "Total (Combined)")) return(NULL)
    rv$disp$regional_params[[loc]][[paste0(field, target)]]
  }
  build_tps_gcv_diag <- function(target) {
    loc <- input$sel_loc_stats; req(loc, identical(rv$disp$method, "TPS"))
    tryCatch(build_tps_gcv_plot(run_fit_record("tps_fit_", target), loc, target),
             error = function(e) sci_placeholder(paste("GCV Plot Error:\n", e$message), size = 4))
  }
  build_idw_power_diag <- function(target) {
    loc <- input$sel_loc_stats; req(loc, identical(rv$disp$method, "IDW"))
    tryCatch(build_idw_power_plot(run_fit_record("idw_fit_", target), loc, target),
             error = function(e) sci_placeholder(paste("Power Plot Error:\n", e$message), size = 4))
  }
  selection_plot <- function(id, build) {
    renderCachedPlot({
      p <- build(); req(p); p
    }, cacheKeyExpr = {
      list(id, input$sel_loc_stats, rv$results_rev, rv$disp$method, rv$disp$regional_params)
    }, cache = "session")
  }
  output$tps_gcv_plot_act <- selection_plot("tps_gcv_act", function() build_tps_gcv_diag("act"))
  output$tps_gcv_plot_pre <- selection_plot("tps_gcv_pre", function() build_tps_gcv_diag("pre"))
  output$idw_power_plot_act <- selection_plot("idw_power_act", function() build_idw_power_diag("act"))
  output$idw_power_plot_pre <- selection_plot("idw_power_pre", function() build_idw_power_diag("pre"))

  # CV Distance Match: the displayed run's own distance record for the
  # selected locality (every strategy, every engine), with a Predicted facet
  # when the run mapped one. Each locality's folds are matched to its own map,
  # so the pooled selection has nothing to draw.
  build_cv_distance_diag <- function() {
    loc <- input$sel_loc_stats; req(loc, rv$disp)
    if (identical(loc, "Total (Combined)")) {
      return(sci_placeholder("Select a locality: each locality's folds are matched to its own map."))
    }
    designs <- Filter(Negate(is.null), list(
      Actual = rv$cv_design_act[[loc]],
      Predicted = if (isTRUE(rv$has_predictions)) rv$cv_design_pre[[loc]]))
    build_cv_distance_plot(if (length(designs)) designs, title = paste("CV Distance Match:", loc))
  }
  output$cv_distance_plot <- renderCachedPlot({
    p <- build_cv_distance_diag(); req(p); p
  }, cacheKeyExpr = {
    list("cv_distance", input$sel_loc_stats, rv$results_rev, isTRUE(rv$has_predictions))
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

    # The Total pools each locality's cones (pooled_within_directional): no
    # pair joins two localities.
    total <- identical(loc, "Total (Combined)")
    vd <- if (total) {
      pooled_within_directional(split(pts[value_col], pts$loc), value_col)
    } else {
      calc_directional_variogram(pts, value_col)
    }
    if (is.null(vd)) {
      return(sci_placeholder(paste0("Not enough point pairs for a directional variogram.\n",
                                    "Four directions need appreciably more points than one omnidirectional curve.")))
    }
    cap <- if (total) pooled_within_caption(vd)
    build_directional_variogram_ggplot(
      vd,
      title = if (total) {
        paste0("Pooled within-locality directional variogram (", what, "): ", cap$count)
      } else {
        paste0("Directional Variogram (", what, "): ", loc)
      },
      subtitle = paste(c(paste0("Bearings clockwise from north, 22.5° half-angle cones. ",
                                "Curves separating by range indicate anisotropy; ",
                                "the engines remain omnidirectional."), cap$note), collapse = "\n"))
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
  register_sci_plot("tps_gcv_plot_act", "TPS Smoothing Selection (GCV, Actual)", function() build_tps_gcv_diag("act"))
  register_sci_plot("tps_gcv_plot_pre", "TPS Smoothing Selection (GCV, Predicted)", function() build_tps_gcv_diag("pre"))
  register_sci_plot("idw_power_plot_act", "IDW Power Selection (Actual)", function() build_idw_power_diag("act"))
  register_sci_plot("idw_power_plot_pre", "IDW Power Selection (Predicted)", function() build_idw_power_diag("pre"))
  register_sci_plot("cv_distance_plot", "CV Distance Match", build_cv_distance_diag)
  register_sci_plot("obs_pred_plot_act", "Observed vs Predicted (Actual)", function() build_obs_pred_diag("act"))
  register_sci_plot("obs_pred_plot_pre", "Observed vs Predicted (Predicted Map)", function() build_obs_pred_diag("pre"))
  register_sci_plot("resid_vgm_plot_act", "Residual Variogram (Actual)", build_resid_vgm_act)
  register_sci_plot("resid_vgm_plot_pre", "Residual Variogram (Predicted Map)", build_resid_vgm_pre)

  output$regional_params_table <- DT::renderDataTable({
    loc <- input$sel_loc_stats; req(loc, (rv$disp$method %||% "") %in% c("IDW", "TPS"))
    has_pre <- isTRUE(rv$disp$comp_mode) || !identical(rv$disp$value_type, "actual")
    # Two or three narrow columns: no scrollX, so header and body stay one table.
    sci_dt(build_regional_params_df(rv$disp$method, loc, rv$disp$regional_params, has_pre),
           scroll_x = FALSE)
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
    st <- summary_stats_df(sv$act, sv$pre, labels = c("Total_Actual", "Total_Predicted"))
    # scroll_x = FALSE: two or three narrow columns fit the smallest supported
    # viewport, so the table stays ONE table and its header cannot drift.
    sci_dt(st, scroll_x = FALSE, signif_cols = setdiff(names(st), "Metric"))
  })

  output$stats_table_loc <- DT::renderDataTable({
    req(rv$user_data, input$sel_loc_stats)
    if(input$sel_loc_stats == "Total (Combined)") return(sci_dt(NULL))
    meta <- get_display_meta()
    req(meta)
    sv <- stats_table_vectors(rv$user_data, meta, rv$mapping$loc, input$sel_loc_stats)
    if (is.null(sv)) return(sci_dt(NULL))
    st <- summary_stats_df(sv$act, sv$pre, labels = c("Selected_Actual", "Selected_Predicted"))
    sci_dt(st, scroll_x = FALSE, signif_cols = setdiff(names(st), "Metric"))
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

  # Shapes a hectare vector (or an error marker) into the table: each class
  # with its range, because every surface has its own class breaks, and the
  # bounds as numbers (open ends NA) so an exported sheet can be recomputed on.
  area_ha_to_df <- function(ha, params) {
    if (is.null(params)) return(data.frame(Status = "Awaiting classification - press Apply to maps and statistics under Map Styling in the sidebar"))
    if (is.null(ha)) return(NULL)
    err <- attr(ha, "area_error")
    if (!is.null(err)) return(data.frame(Error = as.character(err)))
    fin <- function(x) ifelse(is.finite(x), x, NA_real_)
    n <- params$n_c
    # Unrounded: a small field's class can cover less than 0.005 ha, which
    # two decimals printed (and exported) as 0. The card formats it instead.
    data.frame(Class = params$labels, Range = params$ranges,
               `Lower Bound` = fin(params$brks[seq_len(n)]),
               `Upper Bound` = fin(params$brks[seq_len(n) + 1L]),
               Ha = as.numeric(ha), check.names = FALSE)
  }
  # The card shows the ranges; the numeric bounds are for the exported sheet.
  area_card <- function(df) {
    sci_dt(df[, setdiff(names(df), c("Lower Bound", "Upper Bound")), drop = FALSE],
           signif_cols = "Ha")
  }

  calc_area_df <- function(r_obj, r_id = NULL, surface = "act") {
    if (is.null(r_obj)) return(NULL)
    # error-only: catching `condition` here also unwound message() conditions
    # escaping classification_params(), aborting the reactive mid-evaluation
    # and poisoning it for the map renderers (Jenks fell back to continuous)
    params <- tryCatch(classification_params(surface), error = function(e) NULL)
    if (is.null(params)) return(area_ha_to_df(NULL, NULL))
    area_ha_to_df(class_area_ha(r_obj, params, r_id), params)
  }

  area_df_total_act <- reactive({
    req(rv$rast)
    ov <- (rv$bound_overlap_m2 %||% c(act = 0))[["act"]]
    if (!is.finite(ov)) return(data.frame(Status = "Locality overlap could not be measured; see the per-locality area tables."))
    if (ov > 0.5) return(data.frame(Status = sprintf(
      "Overlapping locality boundaries would count %.3g ha more than once; no combined total is reported. See the per-locality tables.", ov / 1e4)))
    params <- tryCatch(classification_params("act"), error = function(e) NULL)
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
    params <- tryCatch(classification_params("pre"), error = function(e) NULL)
    if (is.null(params)) return(area_ha_to_df(NULL, NULL))
    ha <- class_area_ha_sum(rv$rast_list_pre, "loc_pre", params)
    if (is.null(ha)) ha <- class_area_ha(rv$rast_pred, params, "total_pre_merged")
    area_ha_to_df(ha, params)
  })

  output$area_table_total_act <- DT::renderDataTable({ req(length(rv$loc_names) > 1, input$color_style %in% c("agro", "bin")); area_card(area_df_total_act()) })
  output$area_table_total_pre <- DT::renderDataTable({ req(length(rv$loc_names) > 1, input$color_style %in% c("agro", "bin")); area_card(area_df_total_pre()) })

  # The locality filter does not exist until the Scientific Analysis tab has
  # been opened, while these tables render eagerly (list below).
  output$area_table_loc_act <- DT::renderDataTable({
    loc <- input$sel_loc_stats
    req(rv$rast_list_act, loc, input$color_style %in% c("agro", "bin"))
    if(loc == "Total (Combined)") sci_dt(NULL) else area_card(calc_area_df(rv$rast_list_act[[loc]], paste0("loc_act_", loc), "act"))
  })
  output$area_table_loc_pre <- DT::renderDataTable({
    loc <- input$sel_loc_stats
    req(rv$rast_list_pre, loc, input$color_style %in% c("agro", "bin"))
    if(loc == "Total (Combined)") sci_dt(NULL) else area_card(calc_area_df(rv$rast_list_pre[[loc]], paste0("loc_pre_", loc), "pre"))
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
  # Each surface's sheets carry its own class breaks.
  observeEvent(list(tryCatch(classification_params("act"), error = function(e) NULL),
                    tryCatch(classification_params("pre"), error = function(e) NULL),
                    rv$results_rev), {
    req(rv$disp)
    reg <- isolate(rv$export_registry)
    stale <- grepl("^table_(area|kappa)_", names(reg))
    if (any(stale)) rv$export_registry <- reg[!stale]

    params_now <- tryCatch(classification_params("act"), error = function(e) NULL)
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
                tryCatch(calc_area_df(rv$rast_list_act[[l]], paste0("loc_act_", l), "act"),
                         error = function(e) NULL))
    }
    if (has_pre) {
      for (l in names(rv$rast_list_pre)) {
        if (is.null(rv$rast_list_pre[[l]])) next
        reg_if_df(paste0("table_area_pre_loc_", l), paste(l, "- Area Coverage (Predicted)"),
                  tryCatch(calc_area_df(rv$rast_list_pre[[l]], paste0("loc_pre_", l), "pre"),
                           error = function(e) NULL))
      }
    }

    # Agreement needs an uploaded prediction column AND agronomical classes;
    # quartile binning is a screen-side choice with no map counterpart, so the
    # export follows the map's class limits: the Actual surface's, one class
    # definition for both columns of every pair (agro_params).
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
      "knndm" = "kNNDM (map-matched 10-fold; LOOCV below n = 30)",
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
    # nothing else. IDW and TPS re-select a power or lambda set to Auto the same
    # way, and use a fixed one as set; the run's fit records say which.
    method_now <- rv$disp$method %||% ""
    fit_modes <- function(field) {
      modes <- unlist(lapply(rv$disp$regional_params, function(p) {
        vapply(paste0(field, c("act", "pre")), function(nm) {
          f <- p[[nm]]
          if (is.list(f) && is.character(f$mode)) f$mode else NA_character_
        }, character(1))
      }))
      unique(modes[!is.na(modes)])
    }
    reuse_txt <- switch(
      method_now,
      "OK" = " refits its variogram from each fold's training samples; the held-out sample contributes its coordinates only.",
      "CK" = " re-screens its covariates, re-standardizes them and refits the linear model of coregionalization in each fold.",
      "RK" = , "RFK" = " re-screens its covariates, re-interpolates them at the held-out samples, and refits the trend and the residual variogram in each fold.",
      "IDW" = {
        m <- fit_modes("idw_fit_")
        if (identical(m, "cv")) {
          " re-selects its distance power (Auto (CV)) from each fold's training samples before predicting the held-out samples. Scientific Guide \u00a74.2."
        } else if (!"cv" %in% m) {
          " predicts every fold with the fixed distance power, which was set, not selected on these data. Scientific Guide \u00a74.2."
        } else {
          " re-selects its distance power from each fold's training samples where it is on Auto (CV), and uses the fixed power elsewhere. Scientific Guide \u00a74.2."
        }
      },
      "TPS" = {
        m <- fit_modes("tps_fit_")
        if (identical(m, "gcv")) {
          " re-fits its spline and re-selects lambda by GCV from each fold's training samples. Scientific Guide \u00a74.3."
        } else if (!"gcv" %in% m) {
          " re-fits its spline in each fold with the fixed lambda, which was set, not selected on these data. Scientific Guide \u00a74.3."
        } else {
          " re-fits its spline in each fold, re-selecting lambda by GCV where it is on Auto (GCV) and keeping the fixed lambda elsewhere. Scientific Guide \u00a74.3."
        }
      },
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
    # A fold whose variogram fell back to the heuristic, kept a singular
    # candidate, or converged to a range outside the lag support scored a
    # different model than the one that drew the map. The run log names the
    # individual folds and what happened to each.
    vgm_txt <- {
      bad <- lapply(infos, function(x) {
        tb <- x$vgm_status
        if (is.null(tb)) NULL else tb[tb$status != "ok", , drop = FALSE]
      })
      hit <- vapply(bad, function(b) !is.null(b) && nrow(b) > 0, logical(1))
      if (any(hit)) {
        k <- sum(vapply(bad[hit], function(b) sum(b$n), numeric(1)))
        n <- sum(vapply(infos[hit], function(x) sum(x$vgm_status$n), numeric(1)))
        paste0(" In ", k, " of ", n, " folds (",
               paste(unique(names(infos)[hit]), collapse = ", "),
               ") the fold's variogram was degraded, or its range fell outside the lag",
               " support; see the Run Log.")
      }
    }
    cov_txt <- {
      short <- Filter(function(m) isTRUE((m$coverage %||% 1) < 1),
                      c(rv$cv_metrics_act, if (isTRUE(rv$has_predictions)) rv$cv_metrics_pre))
      if (length(short)) {
        " Some rows are INCOMPLETE: part or all of their samples received no cross-validation prediction (see the Run Log), and their metrics describe the predicted samples only."
      }
    }
    # Unseparated IDW/TPS: the Predicted surface's parameter is the Actual one.
    sep_txt <- if (isTRUE(rv$has_predictions) && isFALSE(rv$run_config_summary$sep_fit)) {
      switch(method_now,
        "IDW" = " The Predicted surface uses the Actual surface's power (Fit Actual/Predicted Separately is off).",
        "TPS" = " The Predicted surface uses the Actual surface's lambda; on Auto (GCV), the one GCV selects for the measured values, reselected in each fold (Fit Actual/Predicted Separately is off).",
        NULL)
    }
    refit_note <- if (!is.null(reuse_txt) || !is.null(cov_txt) || !is.null(vgm_txt)) {
      tags$div(
        style = "font-size: 0.82em; color: var(--mn-text-2); margin: -4px 0 8px 0;",
        tags$span(style = "color: var(--mn-text-3);",
                  paste0(get_method_label(rv$disp$method), reuse_txt %||% "",
                         cond_txt %||% "", shared_txt %||% "", sep_txt %||% "",
                         screen_txt %||% "", vgm_txt %||% "", cov_txt %||% ""))
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

  # ── Model Performance ─────────────────────────────────────────────────────
  # One reactive behind two outputs: the table and the footnote that explains
  # its markers. They must agree about which markers the table contains, so
  # neither re-derives them.
  #
  # Markers, all three distinct on purpose:
  #   NA*   the statistic could not be computed for this point set (too few
  #         points, no coordinates, a failed neighbour search) - it NEVER means
  #         "no spatial structure was found".
  #   n/a1  the metric does not apply to this target: mean- and
  #         percentage-normalised errors have no interpretation where the
  #         observed values span zero.
  #   NA†   not reported under a contiguous fold design (Spatial Block CV,
  #         kNNDM spatial folds), where Moran's reference distribution does
  #         not hold.
  metrics_rows <- reactive({
    req(input$sel_loc_stats)
    loc <- input$sel_loc_stats
    strat <- rv$cv_strategy_sel %||% "auto"

    # One definition of the Model Performance column set, shared by the empty
    # stub and by both populated branches so they cannot drift apart. The
    # labels and their order mirror the uploaded-prediction metrics table so the
    # two can be read side by side. Moran's I / p have no counterpart there
    # (uploaded predictions carry no CV residual field).
    # CV_METRIC_LABELS (ui_formatting.R) is that one definition; the export
    # flavour of this table reads the same vector. Moran's null expectation is
    # dropped here because it rides along as a per-row tooltip below - a file
    # cannot carry a tooltip, so the export keeps it as a column.
    keys <- setdiff(names(CV_METRIC_LABELS), "moran_e")
    metric_cols <- c("Source", unname(CV_METRIC_LABELS[keys]))
    # Displayed at four significant digits by the browser formatter; the two
    # scale-dependent metrics and the two Moran cells are character, because
    # they can carry a marker instead of a number.
    num_cols <- unname(CV_METRIC_LABELS[setdiff(keys, c("nrmse_mean", "smape",
                                                        "moran_i", "moran_p"))])
    na_marker <- '<span title="Not computable (see Run Log)">NA*</span>'
    na_scale <- '<span title="Not reported: the observed values span zero (see the note under the table)">n/a¹</span>'
    na_block <- function(mor) {
      sprintf('<span title="Not reported under %s (see the note under the table)">NA†</span>', mor$design)
    }

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

    row_spec <- function(cv_list, data_list, label, info_list, pooled) {
      markers <- character(0)
      if (loc == "Total (Combined)") {
        # Pooled in the auto-UTM zone of the combined centroid (pool_cv_sf):
        # pooled Moran's I uses these coordinates, and EPSG:3857 distances are
        # inflated by 1/cos(latitude). Computed once per run (pooled_cv_metrics).
        res <- pooled()
        # The pooled row mixes localities that need not share a fold design, so
        # its Moran reading is the contiguous one only when every pooled
        # locality resolved to the same contiguous design.
        types <- vapply(names(data_list), function(l) {
          applied_cv_plan(nrow(data_list[[l]]), strat, cv_list[[l]])$type
        }, character(1))
        mor <- moran_reading(types)
        design <- paste0("pooled per-locality CV",
                         if (mor$mixed) ", mixed fold designs" else "")
        src_label <- if (is.null(res)) paste0(label, " (pooled CV)") else {
          source_cell(label, design, res, pooled_cv_population(info_list[names(data_list)]))
        }
      } else {
        res <- cv_list[[loc]]
        n_obs <- if (!is.null(data_list[[loc]])) nrow(data_list[[loc]]) else NA
        # The APPLIED plan, not the requested strategy: a locality below
        # CV_BLOCK_MIN_N was scored by LOOCV, and a failed k-means clustering
        # left random folds. Neither may be reported as Spatial Block CV. A
        # kNNDM request reports the design it chose (random or spatial folds).
        plan <- applied_cv_plan(n_obs, strat, res)
        mor <- moran_reading(plan$type)
        src_label <- if (!is.null(res)) {
          source_cell(label, plan$label, res, info_list[[loc]])
        } else {
          # An all-NA row says that CV produced no metrics here; a plain "(CV)"
          # label would be indistinguishable from a computed row.
          paste0(label, " (CV unavailable - see Run Log)")
        }
      }

      has_cv <- !is.null(res)
      val <- function(k) {
        v <- if (has_cv) res[[k]] else NULL
        if (is.null(v) || length(v) != 1) NA_real_ else as.numeric(v)
      }
      # A metric that does not apply to this target says so; one that could not
      # be computed says that instead; neither is left blank.
      scale_cell <- function(k) {
        if (!has_cv) return("")
        if (isTRUE(res$signed_target)) { markers <<- c(markers, "scale"); return(na_scale) }
        v <- val(k)
        if (is.null(v) || length(v) != 1 || is.na(v)) { markers <<- c(markers, "na"); return(na_marker) }
        format_sig(v)
      }
      nrmse_cell <- scale_cell("nrmse_mean")
      smape_cell <- scale_cell("smape")

      moran_i <- val("moran_i"); moran_e <- val("moran_e"); moran_p <- val("moran_p")
      i_cell <- if (is.na(moran_i)) {
        markers <- c(markers, "na"); na_marker
      } else if (mor$block) {
        markers <- c(markers, "block")
        sprintf('<span title="%s: these residuals inherit the fold geometry and a shared extrapolation condition within each withheld group of samples. E[I] = -1/(n-1) = %s">%s†</span>',
                mor$label, if (is.na(moran_e)) "NA" else format_sig(moran_e), format_sig(moran_i))
      } else {
        # The null expectation rides along as a per-row tooltip: I is centred on
        # E[I] = -1/(n-1), not on 0, so an I marginally above zero is not
        # evidence of clustering at small n.
        sprintf('<span title="Expected I under no spatial autocorrelation: E[I] = -1/(n-1) = %s">%s</span>',
                if (is.na(moran_e)) "NA" else format_sig(moran_e), format_sig(moran_i))
      }
      p_cell <- if (!mor$report_p && !is.na(moran_i)) {
        markers <- c(markers, "block"); na_block(mor)
      } else if (is.na(moran_p)) {
        markers <- c(markers, "na"); na_marker
      } else {
        # Rendered like every other p in the app ("< 0.001" rather than a
        # rounded 0), HTML-escaped because this table renders with escape = FALSE.
        htmltools::htmlEscape(format_p_value(moran_p))
      }

      df <- data.frame(Source = src_label, stringsAsFactors = FALSE)
      for (k in keys) {
        lab <- unname(CV_METRIC_LABELS[[k]])
        df[[lab]] <- switch(k,
          nrmse_mean = nrmse_cell,
          smape      = smape_cell,
          moran_i    = i_cell,
          moran_p    = p_cell,
          val(k))
      }
      list(df = df, block = isTRUE(mor$block), label = mor$label,
           markers = unique(markers))
    }

    specs <- list(row_spec(rv$cv_metrics_act, rv$cv_data_act, "Actual Model", rv$cv_info_act,
                            pooled_cv_metrics$act))
    if (isTRUE(rv$has_predictions)) {
      specs <- c(specs, list(row_spec(rv$cv_metrics_pre, rv$cv_data_pre,
                                      "Predicted Model", rv$cv_info_pre,
                                      pooled_cv_metrics$pre)))
    }
    df <- do.call(rbind, lapply(specs, `[[`, "df"))
    # The column heading can only carry one reading, so it takes the
    # contiguous-design one only when every row in the table was scored under
    # the same contiguous design; otherwise those rows are marked in their own
    # cells.
    labels <- unique(vapply(specs, `[[`, character(1), "label"))
    block_all <- all(vapply(specs, `[[`, logical(1), "block")) && length(labels) == 1
    if (block_all) names(df)[names(df) == "Moran's I"] <- labels

    list(df = df, num_cols = num_cols,
         markers = unique(unlist(lapply(specs, `[[`, "markers"))))
  })

  run_cancelled <- reactive(identical(rv$run_config_summary$status, "cancelled"))

  output$metrics_table <- DT::renderDataTable({
    # A cancelled run produced no metrics. Rows reading "CV unavailable" would
    # describe a run that finished without cross-validation.
    if (run_cancelled()) return(sci_dt(data.frame(Status = RUN_CANCELLED_NOTE)))
    m <- metrics_rows()
    # escape = FALSE keeps the tooltip-bearing spans in the marker cells
    sci_dt(m$df, escape = FALSE, header_tooltips = sci_metric_tooltips(),
           signif_cols = m$num_cols)
  })

  # The markers' footnote, under the table rather than in it: a table cell can
  # carry the short form, the sentence that makes it readable cannot live there.
  output$metrics_table_notes <- renderUI({
    if (run_cancelled()) return(NULL)
    m <- metrics_rows()
    table_footnote(m$markers)
  })

  # ── Repeated cross-validation (opt-in) ────────────────────────────────────
  # The table above reports ONE fold realization (seed CV_FOLD_SEED). When the
  # user asked for repeated CV, this second table reports the mean and the
  # standard deviation of the same metrics across the alternative realizations,
  # which is the honest scale for comparing two methods: an RMSE gap smaller
  # than this SD is fold luck, not skill.
  cv_repeat_row <- function(summ, label) {
    if (is.null(summ)) return(NULL)
    # Every cell is the string "mean ± SD", so the browser formatter cannot
    # reach this table: the significant-digit rule is applied here instead.
    # Both terms share one notation - a mean in fixed notation beside an SD in
    # scientific reads as two different quantities - and the pair switches to
    # scientific only when a term would otherwise be quantized to zero.
    fmt <- function(m, s, key) {
      if (isTRUE(summ$signed_target) && key %in% c("nrmse_mean", "smape")) return("n/a¹")
      if (!is.finite(m)) return("NA")
      terms <- c(m, if (is.finite(s)) s)
      terms <- terms[terms != 0]
      num <- if (length(terms) && min(abs(terms)) < 1e-4) {
        function(x) formatC(x, format = "e", digits = 3)
      } else {
        digits <- if (abs(m) >= 100) 2 else 4
        function(x) formatC(round(x, digits), format = "f", digits = digits, drop0trailing = TRUE)
      }
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
      row[[k]] <- fmt(summ$mean[[k]], summ$sd[[k]], k)
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

  output$cv_repeats_notes <- renderUI({
    df <- cv_repeat_rows()
    req(df)
    table_footnote(if (any(vapply(df, function(col) any(col == "n/a¹"), logical(1)))) "scale")
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
          perf <- pred_perf_df(df$v, df$pv)
          if (is.null(perf)) return(sci_dt(NULL))
          # The Note column explains the NAs of a signed target in the exported
          # sheet; on screen that becomes the n/a¹ marker and one footnote.
          shown <- data.frame(Metric = perf$Metric,
                              Value = ifelse(nzchar(perf$Note), "n/a¹", format_sig(perf$Value)),
                              stringsAsFactors = FALSE)
          shown$Value[is.na(shown$Value)] <- "NA"
          sci_dt(shown)
        })

  output$uploaded_metrics_notes <- renderUI({
    req(rv$sf, input$sel_loc_stats)
    loc <- input$sel_loc_stats
    df <- rv$sf %>% st_drop_geometry() %>% filter(!is.na(v), !is.na(pv))
    if (loc != "Total (Combined)") df <- df %>% filter(loc == !!loc)
    perf <- if (nrow(df) >= 3) pred_perf_df(df$v, df$pv) else NULL
    req(!is.null(perf))
    # The model's own population, read off the run's CV record rather than
    # recomputed - it is what the Model Performance table above reports.
    model_n <- if (identical(loc, "Total (Combined)")) {
      vals <- vapply(rv$cv_metrics_act, function(m) as.numeric(m$n_expected %||% m$n %||% NA), numeric(1))
      if (length(vals) && any(is.finite(vals))) sum(vals, na.rm = TRUE) else NA_real_
    } else {
      m <- rv$cv_metrics_act[[loc]]
      as.numeric(m$n_expected %||% m$n %||% NA)
    }
    pop_note <- pred_pop_note(nrow(df), model_n)
    tagList(
      table_footnote(if (any(nzchar(perf$Note))) "scale"),
      if (!is.null(pop_note)) tags$div(class = "mn-table-note", tags$div(pop_note))
    )
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

    sci_dt(agreement_metrics_df(ag), signif_cols = "Value")
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
  for (out_id in c("vgm_params_table", "vgm_params_title", "vgm_params_note", "regional_params_table", "metrics_table",
                   "metrics_table_notes", "cv_repeats_notes", "uploaded_metrics_notes",
                   "cv_strategy_badge", "cv_repeats_table",
                   "stats_table_total", "stats_table_loc",
                   "area_table_total_act", "area_table_total_pre",
                   "area_table_loc_act", "area_table_loc_pre",
                   "uploaded_metrics_table", "kappa_table",
                   "run_config_display", "log_output", "run_warnings_card",
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
  log_warn_lines <- function(log) {
    grep("\\[WARN\\]", unlist(strsplit(log %||% "", "\n", fixed = TRUE)), value = TRUE)
  }
  # Called when an archived run is restored: its warnings were announced when
  # it ran, and its restored log must not announce them again.
  mark_log_warnings_announced <- function() {
    last_notified_warnings(union(last_notified_warnings(), log_warn_lines(rv$log)))
  }
  observeEvent(rv$log, {
    req(rv$log)
    warn_lines <- log_warn_lines(rv$log)
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
    if (identical(input$color_style, "agro") && is.null(agro_applied_now()))
      return("Agronomical classes are not applied to this variable yet. Press Apply to maps and statistics under Map Styling in the sidebar.")
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
    meta <- get_display_meta()
    if (is.null(meta)) return(NULL)

    view <- map_view_base()
    sources <- switch(view,
      "view_pred"  = list(list(r = rv$rast_pred, tag = "Predicted", surface = "pre")),
      "view_comp"  = list(list(r = rv$rast, tag = "Actual", surface = "act"),
                          list(r = rv$rast_pred, tag = "Predicted", surface = "pre")),
      list(list(r = rv$rast, tag = "Actual", surface = "act")))

    # Each surface is dissolved with its own classes, as the map draws it.
    parts <- lapply(sources, function(s) {
      params <- tryCatch(classification_params(s$surface), error = function(e) NULL)
      if (is.null(params)) return(NULL)
      labs <- if (isTruthy(input$color_style == "bin")) params$leg_labels else params$labels
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
