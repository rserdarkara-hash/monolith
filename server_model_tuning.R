# server_model_tuning.R (chunk F, sourced with local = TRUE inside server):
# per-locality IDW power and TPS lambda values, manual variogram tuning and the
# variogram auto-fit (OPTIMIZE ALL VARIOGRAMS). CRITICAL: the auto-fit goes
# through run_autofit_async(); every reactive it needs is read into a plain
# value BEFORE the call, and every state write happens in on_success.

  # --- Async dispatch of the variogram auto-fit -------------------------------
  # One autofit_vgm_item() job per locality on the run pipeline's promise
  # topology, so the session stays responsive. Inside a PSOCK worker future
  # downgrades the nested plan to sequential and availableCores() reports 1, so
  # cores are counted here and the worker owns an explicit nested cluster, torn
  # down in `finally`. Numerics are plan-independent (furrr's fixed seed; the
  # auto-fit draws no random numbers).
  run_autofit_async <- function(jobs, current_crs, on_success) {
    # Re-entry guard, the way rv$model_running guards the run button: the pool
    # is shared, so a second dispatch would queue behind the first.
    if (isTRUE(rv$opt_running)) {
      showNotification("An optimization is already running.", type = "warning")
      return(invisible(NULL))
    }
    # Cross-feature guard (mirrored in server_execution.R): a run and an
    # optimizer each spawn cores - 1 workers, so running both oversubscribes the
    # machine. Resource protection only; results would be the same.
    if (isTRUE(rv$model_running)) {
      showNotification(
        "An interpolation run is in progress; start the optimization after it finishes.",
        type = "warning"
      )
      return(invisible(NULL))
    }
    if (length(jobs) == 0) {
      return(invisible(NULL))
    }

    tuning_revision <- rv$tuning_revision %||% 0L
    main_wd <- getwd()
    cores_hint <- tryCatch(
      as.integer(parallelly::availableCores()),
      error = function(e) 1L
    )

    nested_workers <- if (length(jobs) > 1L) {
      max(1L, min(cores_hint - 1L, length(jobs)))
    } else {
      1L
    }

    rv$opt_running <- TRUE
    shinyjs::disable("auto_fit")
    updateActionButton(session, "auto_fit", label = "Optimizing...")
    showNotification("Optimizing variograms in the background; the dashboard stays usable.",
                     type = "message", duration = 4)

    p <- promises::future_promise(
      {
        setwd(main_wd)
        # Helpers in the worker's GLOBAL env, so furrr ships a lean,
        # globalenv-enclosed worker function.
        source("spatial_helpers.R", local = FALSE)
        # A reused pool worker may carry a plan pointing at a dead cluster, which
        # nbrOfWorkers() would report as already parallel: reset first.
        future::plan(future::sequential)
        # Looked up by name in the worker, where the line above defined it: named
        # as a symbol it would be shipped from this session with every helper it
        # calls.
        worker_fn <- match.fun("autofit_vgm_item")

        # No `nbrOfWorkers() == 1L` clause: the plan reset above makes it true
        # by construction; the cluster goes before the worker is reused.
        with_nested_cluster(nested_workers,
          furrr::future_map(jobs, worker_fn, current_crs = current_crs,
                            .options = furrr::furrr_options(seed = 12345, packages = c("sf", "gstat"))))
      },
      seed = 12345
    )

    p <- promises::then(
      p,
      # The success body has its own tryCatch, so a rejection means the
      # optimization failed, not that applying its results did.
      onFulfilled = function(res_list) {
        if (!identical(tuning_revision, rv$tuning_revision %||% 0L)) {
          showNotification("Data or mapping changed during optimization; please optimize the current data again.", type = "warning")
          return(invisible(NULL))
        }
        tryCatch(on_success(res_list), error = function(e) {
          showNotification(
            paste(
              "Optimization results could not be applied:",
              conditionMessage(e)
            ),
            type = "error",
            duration = 10
          )
        })
      },
      onRejected = function(err) {
        showNotification(
          paste("Optimization failed:", conditionMessage(err)),
          type = "error",
          duration = 10
        )
      }
    )

    # finally(), not the handlers: a rejection must never leave the button stuck
    # on "Optimizing..." or rv$opt_running latched TRUE.
    promises::finally(p, function() {
      rv$opt_running <- FALSE
      # The button is ALSO gated by the vgm_mode observer below; it is not
      # re-enabled if the user switched to manual fitting while the fit ran.
      if (!identical(isolate(input$vgm_mode), "manual")) shinyjs::enable("auto_fit")
      # Must match the label in ui_sidebar.R.
      updateActionButton(session, "auto_fit", label = "OPTIMIZE ALL VARIOGRAMS")
    })

    invisible(NULL)
  }

  # Tuning values describe one table and one coordinate/locality mapping. Any
  # write to rv$mapping invalidates this observer (variable metadata included),
  # so the handler compares the values that define the data before clearing.
  tuning_data_sig <- NULL
  observeEvent(list(rv$user_data, rv$mapping), {
    sig <- list(rv$user_data, rv$mapping$x, rv$mapping$y, rv$mapping$loc, rv$mapping$crs)
    first <- is.null(tuning_data_sig)
    if (identical(sig, tuning_data_sig)) return()
    tuning_data_sig <<- sig
    if (first) return()
    stores <- c("v_fit_list", "v_emp_list", "idw_factors", "tps_lambdas")
    had_values <- any(vapply(stores, function(nm) length(rv[[nm]]) > 0L, logical(1)))
    for (nm in stores) rv[[nm]] <- list()
    rv$tuning_revision <- (rv$tuning_revision %||% 0L) + 1L
    rv$vgm_preview <- FALSE
    if (had_values) showNotification("Stored tuning values cleared because the data or coordinate/locality mapping changed.", type = "message")
  })
  # The pooled CV of the DISPLAYED run, shown (in a box) only when it is an IDW
  # run (idw_panel_metrics()): the panel sits under the next run's IDW settings.
  output$idw_metrics_ui <- renderUI({
    req(idw_panel_metrics(rv$disp$method, rv$cv_metrics_act))
    div(style = "background-color: var(--mn-surface-2); border: 1px solid var(--mn-line); border-radius: 4px; padding: 10px; color: var(--mn-text);",
        tableOutput("idw_metrics_table"))
  })
  output$idw_metrics_table <- renderTable({
    out <- idw_panel_metrics(rv$disp$method, rv$cv_metrics_act)
    req(out)
    out$Value <- format_sig(out$Value)
    out
  }, caption = "Displayed IDW run (pooled CV)", caption.placement = "top")
  # Last choice set pushed to each tuning-locality selector; the update is
  # re-issued only when the set changes, so a user's pick is not reset.
  tuning_selector_choices <- list()
  observeEvent(list(input$locality, rv$user_data, rv$mapping$loc), {
    req(rv$user_data, rv$mapping$loc)
    locs <- resolve_selected_localities(
      input$locality,
      rv$user_data,
      rv$mapping$loc
    )

    update_selector <- function(id, current_locs) {
      if (identical(tuning_selector_choices[[id]], as.character(current_locs))) {
        return()
      }
      tuning_selector_choices[[id]] <<- as.character(current_locs)
      current_sel <- isolate(input[[id]])
      keep <- isTruthy(current_sel) && current_sel %in% current_locs
      updateSelectInput(
        session,
        id,
        choices = current_locs,
        selected = if (keep) {
          current_sel
        } else if (length(current_locs) > 0) {
          current_locs[1]
        } else {
          NULL
        }
      )
    }

    update_selector("m_loc", locs)
    update_selector("idw_m_loc", locs)
    update_selector("tps_m_loc", locs)
  })

  observeEvent(input$vgm_mode, {
    # Never re-enable while an auto-fit promise is in flight - run_autofit_async
    # owns the button until its finally() fires (and re-checks vgm_mode there).
    if (input$vgm_mode == "manual") {
      shinyjs::disable("auto_fit")
    } else if (!isTRUE(rv$opt_running)) {
      shinyjs::enable("auto_fit")
    }
  })

  # Manual tuning shows on the Scientific Analysis variogram panels, so their
  # locality filter follows the locality being tuned.
  observeEvent(list(input$vgm_mode, input$m_loc), {
    req(identical(input$vgm_mode, "manual"), isTruthy(input$m_loc))
    if (!identical(input$sel_loc_stats, input$m_loc)) {
      updateSelectInput(session, "sel_loc_stats", selected = input$m_loc)
    }
  })

  # The surface the manual variogram tools address, one rule for the preview,
  # the slider sync and Apply: the Target switch counts only while it is shown
  # (prediction context and separate fits); otherwise the Actual fit is tuned.
  manual_vgm_target <- function() {
    shown <- (isTRUE(input$comp_mode) ||
                isTRUE(input$value_type %in% c("pred", "pred_ss", "resid"))) &&
      isTRUE(input$sep_fit)
    if (shown && identical(input$m_target, "pre")) "pre" else "act"
  }

  manual_vgm_column <- function(target = manual_vgm_target()) {
    meta <- get_current_meta()
    if (is.null(meta)) return(NULL)
    if (target == "act") meta$actual else if (identical(input$value_type, "pred_ss")) meta$pred_ss else meta$pred
  }

  current_tuning_keys <- reactive({
    eff_subset <- effective_subset(input$value_type, input$subset, names(rv$user_data))
    c(act = tuning_key(manual_vgm_column("act"), eff_subset),
      pre = tuning_key(manual_vgm_column("pre"), eff_subset))
  })

  # Tuning panels show only the selected variable/subset and localities.
  tuning_vgm_entries <- function(store) {
    keys <- current_tuning_keys()
    locs <- resolve_selected_localities(input$locality, rv$user_data, rv$mapping$loc)
    wanted <- unlist(lapply(locs, function(l) paste0(l, "_", c("act", "pre"))))
    store <- store[intersect(names(store), wanted)]
    keep <- vapply(names(store), function(nm) {
      target <- if (endsWith(nm, "_act")) "act" else "pre"
      vgm_key_matches(store[[nm]], keys[[target]])
    }, logical(1))
    store[keep]
  }

  # The only owner of the manual sliders: bounds from the tuned locality's data
  # (the point set OPTIMIZE ALL VARIOGRAMS fits), values from a stored fit when
  # one exists (manual_vgm_slider_spec()).
  observeEvent(
    list(
      input$vgm_mode,
      input$m_loc,
      input$comp_mode,
      input$m_target,
      input$sep_fit,
      input$var_id,
      input$value_type,
      input$subset,
      rv$user_data,
      rv$mapping,
      rv$v_fit_list
    ),
    {
      req(input$vgm_mode == "manual", input$m_loc, rv$user_data,
          rv$mapping$x, rv$mapping$y, rv$mapping$crs)
      loc <- input$m_loc
      target <- manual_vgm_target()
      meta <- get_current_meta()
      req(meta)
      col <- manual_vgm_column(target)
      ud <- rv$user_data
      req(is_valid_col_ref(col), col %in% names(ud),
          rv$mapping$x %in% names(ud), rv$mapping$y %in% names(ud))
      eff_subset <- effective_subset(input$value_type, input$subset, names(ud))
      ud <- run_locality_rows(ud, rv$mapping$loc, loc, eff_subset)
      d <- stats::na.omit(data.frame(x = coord_values(ud[[rv$mapping$x]]), y = coord_values(ud[[rv$mapping$y]]),
                                     v = ud[[col]]))
      req(nrow(d) >= 3, is.numeric(d$v))
      pts <- tryCatch(
        validate_and_project_sf(sf::st_as_sf(d, coords = c("x", "y"), crs = rv$mapping$crs)),
        error = function(e) NULL
      )
      req(pts)
      pts <- merge_colocated(pts)
      bb <- sf::st_bbox(pts)
      stored <- rv$v_fit_list[[paste0(loc, "_", target)]]
      if (!vgm_key_matches(stored, current_tuning_keys()[[target]])) stored <- NULL
      spec <- manual_vgm_slider_spec(
        stats::var(pts$v),
        sqrt((bb[["xmax"]] - bb[["xmin"]])^2 + (bb[["ymax"]] - bb[["ymin"]])^2),
        stored
      )
      req(spec)

      if (!is.null(spec$model) && spec$model %in% c("Sph", "Exp", "Gau", "Mat")) {
        updateSelectInput(session, "k_mod", selected = spec$model)
      }
      for (id in c("nugget", "psill", "range")) {
        s <- spec[[id]]
        updateSliderInput(session, paste0("m_", id),
                          min = s$min, max = s$max, value = s$value, step = s$step)
      }
      if (isTRUE(spec$step_ok)) {
        removeNotification("m_slider_scale")
      } else {
        showNotification(
          paste0("The variance of this variable in ", loc, " is ", signif(stats::var(pts$v), 3),
                 ". The sliders cannot represent steps below 1e-6, so the nugget and partial ",
                 "sill cannot be tuned at this scale. Rescale the variable in the uploaded file ",
                 "(for example % to g/kg) to tune it manually."),
          id = "m_slider_scale", type = "warning", duration = NULL
        )
      }
    }
  )

  observeEvent(input$apply_manual, {
    req(input$vgm_mode == "manual", input$m_loc)
    loc <- input$m_loc
    target <- manual_vgm_target()
    invalid <- validate_manual_vgm(input$m_psill, input$m_nugget, input$m_range)
    if (!is.null(invalid)) {
      showNotification(paste("Manual model not applied:", invalid), type = "error")
      return()
    }

    model <- manual_vgm(
      input$m_psill,
      input$k_mod,
      input$m_range,
      input$m_nugget
    )
    key <- current_tuning_keys()[[target]]
    req(is_valid_col_ref(manual_vgm_column(target)), !is.na(key))
    rv$v_fit_list[[paste0(loc, "_", target)]] <- stamp_vgm(model, key, "manual")
    showNotification(
      paste("Manual model applied to", loc, "(", target, ")"),
      type = "message"
    )
    if (isTRUE(vgm_smooth_nugget_share(model) < VGM_SMOOTH_NUGGET_WARN_SHARE)) {
      showNotification(
        paste0("A ", input$k_mod, " model with a nugget below 5% of its sill makes kriging ",
               "unstable: predictions can fall far outside the observed range, or the ",
               "locality can come back empty. The model is used as applied; add a nugget ",
               "to avoid this."),
        type = "warning", duration = 15
      )
    }
  })

  # Per-locality IDW power and TPS lambda. The store holds the engine's encoded
  # value (-1 Auto; 0 exact TPS / equal-weights IDW; else fixed); the panel
  # shows a mode plus value, prefilled from the locality's value, else the
  # all-localities setting. An NA is an invalid Fixed all-localities value (the
  # run refuses it), so it prefills the mode only.
  # Each panel's input ids: the method's mode, the locality and surface picks,
  # the all-localities mode and value, the panel's own mode and value, Apply,
  # and the note; `store` is the rv field, `encode` the engine encoding.
  PARAM_PANELS <- list(
    IDW = list(mode = "idw_mode", loc = "idw_m_loc", target = "idw_m_target",
               global_mode = "idw_p_mode", global_value = "idw_p",
               own_mode = "idw_m_mode", own_value = "idw_m_p", apply = "apply_idw_manual",
               note = "idw_m_note", store = "idw_factors", what = "IDW power",
               encode = idw_param_value, fixed_ok = idw_fixed_ok, fallback = 2,
               invalid = sprintf("A fixed IDW power must be a number from 0 (equal weights) to %d; nothing was stored.",
                                 IDW_MAX_FINITE_POWER),
               no_value = "Fixed p, which has no valid value yet (the run will not start)"),
    TPS = list(mode = "tps_mode", loc = "tps_m_loc", target = "tps_m_target",
               global_mode = "tps_lambda_mode", global_value = "tps_lambda",
               own_mode = "tps_m_mode", own_value = "tps_m_lambda", apply = "apply_tps_manual",
               note = "tps_m_note", store = "tps_lambdas", what = "TPS λ",
               encode = tps_param_value, fixed_ok = tps_fixed_ok, fallback = NULL,
               invalid = "A fixed λ must be a number above 0 (Exact is λ = 0); nothing was stored.",
               no_value = "Fixed λ, which has no valid value yet (the run will not start)")
  )

  register_param_panel <- function(type, p) {
    force(type); force(p)
    panel_target <- function() manual_param_target(input$comp_mode, input$value_type, input[[p$target]], input$sep_fit)

    observeEvent(
      list(input[[p$mode]], input[[p$loc]], input$comp_mode, input[[p$target]],
           input$sep_fit, input$value_type, input$var_id, input$subset),
      {
        req(input[[p$mode]] == "manual", input[[p$loc]])
        target <- panel_target()
        val <- get_regional_param(type, input[[p$loc]], target,
                                  default = p$encode(input[[p$global_mode]], input[[p$global_value]]),
                                  key = current_tuning_keys()[[target]])
        mode <- if (is.na(val)) input[[p$global_mode]] else param_value_mode(type, val)
        updateRadioButtons(session, p$own_mode, selected = mode)
        if (identical(mode, "fixed") && !is.na(val)) updateNumericInput(session, p$own_value, value = val)
      }
    )

    observeEvent(input[[p$apply]], {
      req(input[[p$mode]] == "manual", input[[p$loc]])
      loc <- input[[p$loc]]
      target <- panel_target()
      key <- current_tuning_keys()[[target]]
      req(!is.na(key))
      if (identical(input[[p$own_mode]], "fixed") && !p$fixed_ok(input[[p$own_value]])) {
        showNotification(p$invalid, type = "error")
        return()
      }
      val <- p$encode(input[[p$own_mode]], input[[p$own_value]])
      set_regional_param(type, loc, target, val, key)
      showNotification(paste0(p$what, " for ", loc, " (", target, ": ", key, "): ",
                              param_setting_text(type, val), "."), type = "message")
    })

    # The note under the panel (per_locality_note()): which localities have
    # their own value and what the others run with (the hidden all-localities
    # setting the controls start from), so a prefilled control is not read as
    # stored.
    output[[p$note]] <- renderUI({
      req(identical(input[[p$mode]], "manual"), rv$user_data)
      global <- if (identical(input[[p$global_mode]], "fixed") && !p$fixed_ok(input[[p$global_value]])) {
        p$no_value
      } else {
        param_setting_text(type, p$encode(input[[p$global_mode]], input[[p$global_value]]) %||% p$fallback)
      }
      target <- panel_target()
      locs <- resolve_selected_localities(input$locality, rv$user_data, rv$mapping$loc)
      req(length(locs) > 0)
      lines <- per_locality_note(type, rv[[p$store]], target, locs, current_tuning_keys()[[target]],
                                 global, input[[p$loc]])
      tags$small(style = "display: block; color: var(--mn-text-3); margin: 0 0 8px 0;",
                 HTML(paste(htmltools::htmlEscape(lines), collapse = "<br>")))
    })
  }
  for (.type in names(PARAM_PANELS)) register_param_panel(.type, PARAM_PANELS[[.type]])

  observeEvent(input$auto_fit, {
    req(rv$user_data, rv$mapping$x, rv$mapping$y, rv$mapping$crs)
    locs <- resolve_selected_localities(
      input$locality,
      rv$user_data,
      rv$mapping$loc
    )
    meta <- get_current_meta()
    req(meta)

    # One job per locality (autofit_vgm_item() fits both surfaces). Every
    # reactive the worker and the diagnostics modal need is read here.
    current_crs <- rv$mapping$crs
    user_data <- rv$user_data
    loc_col <- rv$mapping$loc
    x_col <- rv$mapping$x
    y_col <- rv$mapping$y
    # Unticked "Fit Actual/Predicted separately": the Predicted surface is
    # kriged with the measured values' variogram, so none is fitted for it here.
    want_pre <- (input$comp_mode || input$value_type != "actual") && isTRUE(input$sep_fit)
    pred_col <- if (input$value_type == "pred_ss") meta$pred_ss else meta$pred
    eff_subset <- effective_subset(input$value_type, input$subset, names(user_data))

    jobs <- lapply(locs, function(l) {
      sub_df <- run_locality_rows(user_data, loc_col, l, eff_subset)
      # A surface's rows: position (a text cell in a coordinate column is
      # none) and value, complete.
      rows_of <- function(col) {
        stats::na.omit(data.frame(x = coord_values(sub_df[[x_col]]), y = coord_values(sub_df[[y_col]]),
                                  v = sub_df[[col]]))
      }
      list(l = l, act = rows_of(meta$actual),
           pre = if (want_pre && !is.null(pred_col) && pred_col %in% colnames(user_data)) rows_of(pred_col),
           key = c(act = tuning_key(meta$actual, eff_subset), pre = tuning_key(pred_col, eff_subset)))
    })
    req(length(jobs) > 0)

    run_autofit_async(
      jobs = jobs,
      current_crs = current_crs,
      on_success = function(res_list) {
        results <- list()
        rv$vgm_preview <- TRUE
        for (i in seq_along(res_list)) {
          res <- res_list[[i]]
          l <- res$l
          keys <- jobs[[i]]$key
          if (!is.null(res$act$fit)) {
            rv$v_emp_list[[paste0(l, "_act")]] <- stamp_vgm(res$act$emp, keys[["act"]], "autofit")
            rv$v_fit_list[[paste0(l, "_act")]] <- stamp_vgm(res$act$fit, keys[["act"]], "autofit")
          }
          if (!is.null(res$pre$fit)) {
            rv$v_emp_list[[paste0(l, "_pre")]] <- stamp_vgm(res$pre$emp, keys[["pre"]], "autofit")
            rv$v_fit_list[[paste0(l, "_pre")]] <- stamp_vgm(res$pre$fit, keys[["pre"]], "autofit")
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
          txt <- paste0(
            "<b>",
            htmltools::htmlEscape(l),
            "</b>: Actual: ",
            r$act_mod,
            " (SSE: ",
            r$act_sse,
            ")"
          )
          if (want_pre) {
            txt <- paste0(
              txt,
              " | Predicted: ",
              r$pre_mod,
              " (SSE: ",
              r$pre_sse,
              ")"
            )
          }
          tags$li(HTML(txt))
        })
        showModal(modalDialog(
          title = "Expert Auto-Fit: Variogram Diagnostics",
          tags$ul(res_tags),
          easyClose = TRUE
        ))
      }
    )
  })
