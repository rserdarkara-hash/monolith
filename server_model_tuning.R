# server_model_tuning.R (sourced with local = TRUE inside server) - TPS lambda
# and IDW power optimization, manual variogram tuning, expert auto-fit.
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

        # tps_gcv_item is a TOP-LEVEL function (spatial_helpers.R): an inline
        # lambda here would make future serialize the observer -> server env
        # chain (the whole session) to every worker on each click.
        res_list <- furrr::future_map(df_list, tps_gcv_item, current_crs = current_crs,
                                      .options = furrr::furrr_options(seed = 12345, packages = c("sf", "fields")))
        
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
    # A locality whose optimization failed never had set_regional_param called,
    # so get_regional_param returns the -1 Auto sentinel — not a lambda. Keep
    # sentinels out of the slider mean (one failure would drag it negative and
    # silently flip the global default to Auto). Per-locality stored values
    # still win at dispatch; this only sets the fallback slider position.
    ok_best <- all_best[is.finite(all_best) & all_best >= 0]
    if (length(ok_best) > 0) updateSliderInput(session, "tps_lambda", value = mean(ok_best))
    
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

        # idw_opt_item is a TOP-LEVEL function (spatial_helpers.R); see
        # tps_gcv_item note on why no inline lambda is used here.
        res_list <- furrr::future_map(df_list, idw_opt_item, current_crs = current_crs,
                                      idw_nmax_val = idw_nmax_val,
                                      .options = furrr::furrr_options(seed = 12345, packages = c("sf", "gstat")))

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
  # Last choice set pushed to each tuning-locality selector: the old code
  # compared the choices against the current SELECTION, which never matches
  # with 2+ localities, so every trigger re-issued the update and reset the
  # user's pick back to the first locality.
  tuning_selector_choices <- list()
  observeEvent(list(input$locality, rv$user_data, rv$mapping$loc), {
    req(input$locality, rv$user_data, rv$mapping$loc)
    locs <- resolve_selected_localities(input$locality, rv$user_data, rv$mapping$loc)

    update_selector <- function(id, current_locs) {
      if (identical(tuning_selector_choices[[id]], as.character(current_locs))) return()
      tuning_selector_choices[[id]] <<- as.character(current_locs)
      current_sel <- isolate(input[[id]])
      keep <- isTruthy(current_sel) && current_sel %in% current_locs
      updateSelectInput(session, id, choices = current_locs,
                        selected = if (keep) current_sel else if (length(current_locs) > 0) current_locs[1] else NULL)
    }

    update_selector("m_loc", locs)
    update_selector("idw_m_loc", locs)
    update_selector("tps_m_loc", locs)
  })

  observeEvent(input$vgm_mode, {
    if(input$vgm_mode == "manual") shinyjs::disable("auto_fit") else shinyjs::enable("auto_fit")
  })

  # Manual tuning happens on the Scientific Analysis tab's variogram panels,
  # which render the locality picked in ITS filter - keep that filter in sync
  # with the locality being tuned, or slider moves appear to do nothing while
  # the filter still shows "Total (Combined)".
  observeEvent(list(input$vgm_mode, input$m_loc), {
    req(identical(input$vgm_mode, "manual"), isTruthy(input$m_loc))
    if (!identical(input$sel_loc_stats, input$m_loc)) {
      updateSelectInput(session, "sel_loc_stats", selected = input$m_loc)
    }
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
      
      # autofit_vgm_item is a TOP-LEVEL function (spatial_helpers.R); see
      # tps_gcv_item note on why no inline lambda is used here.
      res_list <- furrr::future_map(df_list, autofit_vgm_item, current_crs = current_crs,
                                    .options = furrr::furrr_options(seed = 12345, packages = c("sf", "gstat")))
      
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


