# server_model_tuning.R (sourced with local = TRUE inside server) - TPS lambda
# and IDW power optimization, manual variogram tuning, expert auto-fit.
# CRITICAL: the three optimizer buttons dispatch through run_optimizer_async
# below; every reactive they need must be read into a plain value BEFORE the
# call, and every state write must happen in the on_success handler.

  # --- Shared async dispatcher for the three optimizer buttons ---------------
  # All three used to call furrr::future_map() straight from the observer.
  # future_map resolves its futures before returning, so the main R process sat
  # in value() until the last locality finished: no tab switch, no plot, no
  # other session event, and withProgress could not advance because the flush
  # never happened. They now use the same promise topology as the run pipeline
  # (server_execution.R) and the gov/classification modules.
  #
  # The nested escalation is NOT decoration. A future_promise body runs inside a
  # PSOCK worker, where future downgrades the nested plan to sequential
  # (nbrOfWorkers() == 1), so a bare future_map in there would walk the
  # localities one at a time - trading the freeze for an N-fold longer wait.
  # Cores are counted HERE in the main session (availableCores() inside a worker
  # reports 1), shipped as plain data, and the cluster is owned explicitly so
  # the finally block returns the reused promise worker to single-threaded
  # state. Numerics are plan-independent: furrr's fixed seed assigns one
  # L'Ecuyer stream per job whatever the topology, and none of the three workers
  # draws from the stream it is handed (optimize_idw_p builds its folds through
  # make_cv_folds, which is seeded inside a two-sided RNG sandbox, krige.cv with
  # an explicit nfold vector draws nothing, fields::Tps and fit.variogram are
  # deterministic).
  optimizer_button_labels <- list(
    opt_idw  = "OPTIMIZE IDW FACTORS",
    opt_tps  = "OPTIMIZE TPS LAMBDA",
    auto_fit = "OPTIMIZE ALL VARIOGRAMS"
  )

  run_optimizer_async <- function(btn_id, jobs, worker_name, worker_args,
                                  packages, busy_msg, on_success) {
    # Re-entry guard, the way rv$model_running guards the run button: the pool
    # is shared, so a second dispatch would queue behind the first.
    if (isTRUE(rv$opt_running)) {
      showNotification("An optimization is already running.", type = "warning")
      return(invisible(NULL))
    }
    # Cross-feature guard (mirrored in server_execution.R): a run and an
    # optimizer each escalate to their own makeClusterPSOCK(cores - 1), so two
    # concurrent dispatches oversubscribe the machine ~2x. Results would still
    # be correct (numerics are plan-independent); this is purely resource
    # protection for the single-user desktops the app targets.
    if (isTRUE(rv$model_running)) {
      showNotification("An interpolation run is in progress; start the optimization after it finishes.", type = "warning")
      return(invisible(NULL))
    }
    if (length(jobs) == 0) return(invisible(NULL))

    main_wd <- getwd()
    cores_hint <- tryCatch(as.integer(future::availableCores()), error = function(e) 1L)
    nested_workers <- if (length(jobs) > 1L) max(1L, min(cores_hint - 1L, length(jobs))) else 1L

    rv$opt_running <- TRUE
    shinyjs::disable(btn_id)
    updateActionButton(session, btn_id, label = "Optimizing...")
    showNotification(busy_msg, type = "message", duration = 4)

    p <- promises::future_promise({
      setwd(main_wd)
      # Define the full helper set in the promise worker's GLOBAL env, so the
      # worker function is globalenv-enclosed and furrr ships a lean value to
      # each nested worker instead of walking an observer's environment chain.
      source("spatial_helpers.R", local = FALSE)
      worker_fn <- match.fun(worker_name)

      nested_cl <- NULL
      old_mc_cores <- getOption("mc.cores")
      if (nested_workers >= 2L && future::nbrOfWorkers() == 1L) {
        # PSOCK workers report mc.cores = 1; tell parallelly what the main
        # session allocated to this batch before spawning, or its worker-count
        # guard misfires.
        options(mc.cores = nested_workers)
        nested_cl <- parallelly::makeClusterPSOCK(nested_workers)
        future::plan(future::cluster, workers = nested_cl)
      }

      tryCatch({
        do.call(furrr::future_map, c(
          list(.x = jobs, .f = worker_fn),
          worker_args,
          list(.options = furrr::furrr_options(seed = 12345, packages = packages))
        ))
      }, finally = {
        if (!is.null(nested_cl)) {
          future::plan(future::sequential)
          parallel::stopCluster(nested_cl)
          options(mc.cores = old_mc_cores)
        }
      })
    }, seed = 12345)

    p <- promises::then(
      p,
      # The success body carries its OWN tryCatch so a rejection genuinely means
      # "the parallel optimization failed" and not "applying the results
      # failed" - the same split the interpolation completion handler uses.
      onFulfilled = function(res_list) {
        tryCatch(on_success(res_list), error = function(e) {
          showNotification(paste("Optimization results could not be applied:",
                                 conditionMessage(e)), type = "error", duration = 10)
        })
      },
      onRejected = function(err) {
        showNotification(paste("Optimization failed:", conditionMessage(err)),
                         type = "error", duration = 10)
      }
    )

    # finally(), not the handlers: a rejection must never leave the button stuck
    # on "Optimizing..." or rv$opt_running latched TRUE.
    promises::finally(p, function() {
      rv$opt_running <- FALSE
      # auto_fit is ALSO gated by the vgm_mode observer below; do not re-enable
      # it if the user switched to manual fitting while the optimizer ran.
      if (!(identical(btn_id, "auto_fit") &&
            identical(isolate(input$vgm_mode), "manual"))) {
        shinyjs::enable(btn_id)
      }
      updateActionButton(session, btn_id, label = optimizer_button_labels[[btn_id]])
    })

    invisible(NULL)
  }

  # --- TPS Optimization ---
  # Lambda presets: the 0.001-step slider makes the special values -1 (Auto)
  # and 0 (exact interpolation) hard to hit by dragging.
  observeEvent(input$tps_preset_auto,    updateSliderInput(session, "tps_lambda",   value = -1))
  observeEvent(input$tps_preset_exact,   updateSliderInput(session, "tps_lambda",   value = 0))
  observeEvent(input$tps_m_preset_auto,  updateSliderInput(session, "tps_m_lambda", value = -1))
  observeEvent(input$tps_m_preset_exact, updateSliderInput(session, "tps_m_lambda", value = 0))

  tps_opt_vals <- reactiveVal(NULL)
  observeEvent(input$opt_tps, {
    req(rv$user_data, input$var_id, input$method == "TPS", rv$mapping$crs)
    locs <- resolve_selected_localities(input$locality, rv$user_data, rv$mapping$loc)
    meta <- get_current_meta(); req(meta)

    targets <- if (input$comp_mode || input$value_type != "actual") c("act", "pre") else "act"

    # Plain values captured BEFORE dispatch (no rv$/input$ may be read inside
    # the promise or its handlers). The act/pre loop is FLATTENED into one jobs
    # list so a single map covers both surfaces - two chained promises would be
    # the only alternative - and each job carries its own `target`, because the
    # observer's loop binding is long gone by the time the handler runs.
    current_crs <- rv$mapping$crs
    user_data <- rv$user_data
    loc_col <- rv$mapping$loc; x_col <- rv$mapping$x; y_col <- rv$mapping$y
    value_type <- input$value_type

    jobs <- unlist(lapply(targets, function(tg) {
      val_col <- if (tg == "act") meta$actual else if (value_type == "pred_ss") meta$pred_ss else meta$pred
      if (is.null(val_col) || !(val_col %in% colnames(user_data))) return(NULL)
      lapply(locs, function(l) {
        sub_df <- user_data %>% filter(!!sym(loc_col) == l) %>%
          select(x = !!sym(x_col), y = !!sym(y_col), v = !!sym(val_col)) %>% na.omit()
        list(l = l, target = tg, df = sub_df)
      })
    }), recursive = FALSE)
    req(length(jobs) > 0)

    rv$tps_gcv_data <- list()

    run_optimizer_async(
      btn_id = "opt_tps", jobs = jobs, worker_name = "tps_gcv_item",
      worker_args = list(current_crs = current_crs),
      packages = c("sf", "fields"),
      busy_msg = "Optimizing TPS lambda per region in the background; the dashboard stays usable.",
      on_success = function(res_list) {
        for (i in seq_along(res_list)) {
          res <- res_list[[i]]
          l <- jobs[[i]]$l
          target <- jobs[[i]]$target
          if (!is.null(res$err)) {
            rv$log <- paste0(rv$log, "\nTPS Opt Error (", l, "): ", res$err)
            showNotification(paste("TPS Optimization failed for", l, "- using fallback lambda."), type = "warning")
          } else {
            set_regional_param("TPS", l, target, res$best_lam)
            if (!is.null(res$gcv_data)) {
              rv$tps_gcv_data[[paste0(l, "_", target)]] <- res$gcv_data
            }
          }
        }

        all_best <- sapply(locs, function(l) get_regional_param("TPS", l, "act"))
        # A locality whose optimization failed never had set_regional_param
        # called, so get_regional_param returns the -1 Auto sentinel — not a
        # lambda. Keep sentinels out of the slider mean (one failure would drag
        # it negative and silently flip the global default to Auto).
        # Per-locality stored values still win at dispatch; this only sets the
        # fallback slider position.
        ok_best <- all_best[is.finite(all_best) & all_best >= 0]
        if (length(ok_best) > 0) updateSliderInput(session, "tps_lambda", value = mean(ok_best))

        tps_opt_vals(list(locs = locs, targets = targets))
        showNotification("TPS Optimization Complete. Per-region Lambdas stored.", type = "message")
      }
    )
  })
  
  # Shared builder for the per-locality optimization summary panels (TPS
  # lambdas / IDW power factors) - same table, different engine and format.
  render_opt_summary_panel <- function(engine, vals_reactive, fmt, heading) {
    renderUI({
      res <- vals_reactive(); if(is.null(res)) return(NULL)

      # The prediction column only exists when the run actually optimized a
      # predicted target; without one every cell was "N/A", so the column is
      # dropped rather than printed empty.
      has_pre <- "pre" %in% res$targets

      rows <- lapply(res$locs, function(l) {
        cells <- list(tags$td(l), tags$td(sprintf(fmt, get_regional_param(engine, l, "act"))))
        if (has_pre) cells <- c(cells, list(tags$td(sprintf(fmt, get_regional_param(engine, l, "pre")))))
        do.call(tags$tr, cells)
      })

      headers <- list(tags$th("Locality"), tags$th("Actual"))
      if (has_pre) headers <- c(headers, list(tags$th("Predicted")))

      div(style = "margin-top: 10px; padding: 10px; background-color: var(--mn-surface-2); color: var(--mn-text-2); border: 1px solid var(--mn-line); border-radius: 4px; font-size: 0.8em;",
          h5(heading),
          tags$table(class = "table table-condensed table-bordered", style = "background-color: var(--mn-surface); color: var(--mn-text);",
            tags$thead(do.call(tags$tr, headers)),
            tags$tbody(rows)
          )
      )
    })
  }

  output$tps_opt_panel <- render_opt_summary_panel("TPS", tps_opt_vals, "%.6f", "Optimization Summary (Best Lambdas):")

  idw_opt_vals <- reactiveVal(NULL)
  observeEvent(input$opt_idw, {
    req(rv$user_data, input$var_id, input$method == "IDW", input$locality, rv$mapping$crs)

    locs <- resolve_selected_localities(input$locality, rv$user_data, rv$mapping$loc)
    meta <- get_current_meta(); req(meta)

    targets <- if (input$comp_mode || input$value_type != "actual") c("act", "pre") else "act"

    # See the TPS observer: plain values only, act/pre flattened into one jobs
    # list, `target` carried per job.
    current_crs <- rv$mapping$crs
    idw_nmax_val <- input$idw_nmax
    # The power search shares the run's fold authority, so the strategy the
    # user selected has to be captured here (plain value) and shipped to the
    # worker like every other parameter.
    cv_strategy_val <- input$cv_strategy %||% "auto"
    user_data <- rv$user_data
    loc_col <- rv$mapping$loc; x_col <- rv$mapping$x; y_col <- rv$mapping$y
    value_type <- input$value_type

    jobs <- unlist(lapply(targets, function(tg) {
      val_col <- if (tg == "act") meta$actual else if (value_type == "pred_ss") meta$pred_ss else meta$pred
      if (is.null(val_col) || !(val_col %in% colnames(user_data))) return(NULL)
      lapply(locs, function(l) {
        sub_df <- user_data %>% filter(!!sym(loc_col) == l) %>%
          select(x = !!sym(x_col), y = !!sym(y_col), v = !!sym(val_col)) %>% na.omit()
        list(l = l, target = tg, df = sub_df)
      })
    }), recursive = FALSE)
    req(length(jobs) > 0)

    run_optimizer_async(
      btn_id = "opt_idw", jobs = jobs, worker_name = "idw_opt_item",
      worker_args = list(current_crs = current_crs, idw_nmax_val = idw_nmax_val,
                         cv_strategy = cv_strategy_val),
      packages = c("sf", "gstat"),
      busy_msg = "Calculating optimal IDW factors per region in the background; the dashboard stays usable.",
      on_success = function(res_list) {
        for (i in seq_along(res_list)) {
          set_regional_param("IDW", jobs[[i]]$l, jobs[[i]]$target, res_list[[i]]$best_f)
        }

        all_best <- sapply(locs, function(l) get_regional_param("IDW", l, "act"))
        # Unlike TPS there is no sentinel to filter (idw_opt_item falls back to
        # a legitimate power of 2.0 and get_regional_param defaults to the same),
        # so this only guards the slider against a non-finite value.
        ok_best <- all_best[is.finite(all_best)]
        if (length(ok_best) > 0) updateSliderInput(session, "idw_p", value = mean(ok_best))

        idw_opt_vals(list(locs = locs, targets = targets))
        showNotification(paste("IDW Optimization Complete for:", paste(locs, collapse = ", ")),
                         type = "message", duration = 5)
      }
    )
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
    # Never re-enable while an optimizer promise is in flight - run_optimizer_async
    # owns the button until its finally() fires (and re-checks vgm_mode there).
    if (input$vgm_mode == "manual") {
      shinyjs::disable("auto_fit")
    } else if (!isTRUE(rv$opt_running)) {
      shinyjs::enable("auto_fit")
    }
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
    req(rv$user_data, input$locality, rv$mapping$x, rv$mapping$y, rv$mapping$crs)
    locs <- resolve_selected_localities(input$locality, rv$user_data, rv$mapping$loc)
    meta <- get_current_meta()
    req(meta)
    rv$loc_names <- locs # Ensure selectors update

    # No flattening here: autofit_vgm_item already handles BOTH surfaces per
    # locality, so the job list is one entry per locality exactly as before and
    # the L'Ecuyer stream assignment is untouched. Everything else follows the
    # TPS/IDW pattern - plain values captured before dispatch, including
    # want_pre, which the diagnostics modal used to read from input$ after the
    # work had finished.
    current_crs <- rv$mapping$crs
    user_data <- rv$user_data
    loc_col <- rv$mapping$loc; x_col <- rv$mapping$x; y_col <- rv$mapping$y
    want_pre <- input$comp_mode || input$value_type != "actual"
    pred_col <- if (input$value_type == "pred_ss") meta$pred_ss else meta$pred

    jobs <- lapply(locs, function(l) {
      sub_a_raw <- user_data %>% filter(!!sym(loc_col) == l) %>%
        select(x = !!sym(x_col), y = !!sym(y_col), v = !!sym(meta$actual)) %>%
        na.omit()

      sub_p_raw <- NULL
      if (want_pre && !is.null(pred_col) && pred_col %in% colnames(user_data)) {
        sub_p_raw <- user_data %>% filter(!!sym(loc_col) == l) %>%
          select(x = !!sym(x_col), y = !!sym(y_col), v = !!sym(pred_col)) %>%
          na.omit()
      }
      list(l = l, act = sub_a_raw, pre = sub_p_raw)
    })
    req(length(jobs) > 0)

    run_optimizer_async(
      btn_id = "auto_fit", jobs = jobs, worker_name = "autofit_vgm_item",
      worker_args = list(current_crs = current_crs),
      packages = c("sf", "gstat"),
      busy_msg = "Optimizing variograms in the background; the dashboard stays usable.",
      on_success = function(res_list) {
        results <- list()
        rv$vgm_preview <- TRUE
        for (res in res_list) {
          l <- res$l
          if (!is.null(res$act$fit)) {
            rv$v_emp_list[[paste0(l, "_act")]] <- res$act$emp
            rv$v_fit_list[[paste0(l, "_act")]] <- res$act$fit
          }
          if (!is.null(res$pre$fit)) {
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

        res_tags <- lapply(names(results), function(l) {
          r <- results[[l]]
          txt <- paste0("<b>", l, "</b>: Actual: ", r$act_mod, " (SSE: ", r$act_sse, ")")
          if (want_pre) {
            txt <- paste0(txt, " | Predicted: ", r$pre_mod, " (SSE: ", r$pre_sse, ")")
          }
          tags$li(HTML(txt))
        })
        showModal(modalDialog(title = "Expert Auto-Fit: Variogram Diagnostics",
                              tags$ul(res_tags), easyClose = TRUE))
      }
    )
  })


