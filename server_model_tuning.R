# server_model_tuning.R (sourced with local = TRUE inside server) - per-locality
# IDW power and TPS lambda values, manual variogram tuning, and the expert
# variogram auto-fit (OPTIMIZE ALL VARIOGRAMS).
# CRITICAL: the auto-fit dispatches through run_optimizer_async below; every
# reactive it needs must be read into a plain value BEFORE the call, and every
# state write must happen in the on_success handler.

# --- Async dispatcher for the variogram auto-fit ----------------------------
# The promise topology is the run pipeline's (server_execution.R), so the main
# R process keeps serving the session while the localities are fitted.
#
# The nested escalation is NOT decoration. A future_promise body runs inside a
# PSOCK worker, where future downgrades the nested plan to sequential
# (nbrOfWorkers() == 1), so a bare future_map in there would walk the
# localities one at a time. Cores are counted HERE in the main session
# (availableCores() inside a worker reports 1), shipped as plain data, and the
# cluster is owned explicitly so the finally block returns the reused promise
# worker to single-threaded state. Numerics are plan-independent: furrr's fixed
# seed assigns one L'Ecuyer stream per job whatever the topology, and
# fit.variogram draws nothing from it.
optimizer_button_labels <- list(
  auto_fit = "OPTIMIZE ALL VARIOGRAMS"
)

run_optimizer_async <- function(
  btn_id,
  jobs,
  worker_name,
  worker_args,
  packages,
  busy_msg,
  on_success
) {
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
    as.integer(future::availableCores()),
    error = function(e) 1L
  )

  nested_workers <- if (length(jobs) > 1L) {
    max(1L, min(cores_hint - 1L, length(jobs)))
  } else {
    1L
  }

  rv$opt_running <- TRUE
  shinyjs::disable(btn_id)
  updateActionButton(session, btn_id, label = "Optimizing...")
  showNotification(busy_msg, type = "message", duration = 4)

  p <- promises::future_promise(
    {
      setwd(main_wd)
      # Define the full helper set in the promise worker's GLOBAL env, so the
      # worker function is globalenv-enclosed and furrr ships a lean value to
      # each nested worker instead of walking an observer's environment chain.
      source("spatial_helpers.R", local = FALSE)
      # A pool worker is REUSED across features, so this session may carry
      # whatever plan the last task left in it. A task whose teardown did not
      # complete leaves plan(cluster) pointing at a cluster nobody owns any
      # more, and nbrOfWorkers() then reports that dead cluster's size (2 in a
      # direct test, after its stopCluster): the guard below reads it as
      # "already parallel", skips building a live cluster, and the map runs
      # against those sockets. Start from a known state instead of trusting
      # the inherited one.
      future::plan(future::sequential)
      worker_fn <- match.fun(worker_name)

      nested_cl <- NULL
      old_mc_cores <- getOption("mc.cores")
      tryCatch(
        {
          # No `nbrOfWorkers() == 1L` clause: the plan reset above makes it
          # true by construction.
          if (nested_workers >= 2L) {
            # PSOCK workers report mc.cores = 1; tell parallelly what the main
            # session allocated to this batch before spawning, or its worker-count
            # guard misfires.
            options(mc.cores = nested_workers)
            nested_cl <- parallelly::makeClusterPSOCK(nested_workers)
            future::plan(future::cluster, workers = nested_cl)
          }
          do.call(
            furrr::future_map,
            c(
              list(.x = jobs, .f = worker_fn),
              worker_args,
              list(
                .options = furrr::furrr_options(
                  seed = 12345,
                  packages = packages
                )
              )
            )
          )
        },
        finally = {
          options(mc.cores = old_mc_cores)
          # Stop the cluster FIRST, and let neither step's failure cancel the
          # other. Switching the plan away from an unhealthy cluster can itself
          # throw, which would leave the session on the dead cluster even
          # though stopCluster had run - the exact state the reset at the top
          # of this body has to undo on the next task.
          if (!is.null(nested_cl)) {
            tryCatch(parallel::stopCluster(nested_cl), error = function(e) NULL)
          }
          tryCatch(future::plan(future::sequential), error = function(e) NULL)
        }
      )
    },
    seed = 12345
  )

  p <- promises::then(
    p,
    # The success body carries its OWN tryCatch so a rejection genuinely means
    # "the parallel optimization failed" and not "applying the results
    # failed" - the same split the interpolation completion handler uses.
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
    # auto_fit is ALSO gated by the vgm_mode observer below; do not re-enable
    # it if the user switched to manual fitting while the optimizer ran.
    if (
      !(identical(btn_id, "auto_fit") &&
        identical(isolate(input$vgm_mode), "manual"))
    ) {
      shinyjs::enable(btn_id)
    }
    updateActionButton(
      session,
      btn_id,
      label = optimizer_button_labels[[btn_id]]
    )
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
# The pooled cross-validation of the DISPLAYED run, shown only when that run
# is an IDW run (idw_panel_metrics): the panel sits under the next run's IDW
# settings, and another engine's figures must never appear there. The box is
# drawn only around such a table, so the panel carries no empty frame before
# an IDW run is on screen.
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
# Last choice set pushed to each tuning-locality selector. The update is
# re-issued only when the choice set changes: compared against the current
# SELECTION instead, it would never match with 2+ localities, and every trigger
# would reset the user's pick to the first locality.
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

# Surface the manual variogram tools address. The Target switch is shown in
# Comparison Mode or for a prediction value type, and only while the surfaces
# are fitted separately: otherwise the run kriges the predicted surface with
# the Actual fit (ui_sidebar.R). The red preview, the slider sync and Apply all
# resolve it with this one rule.
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

# The only owner of the manual sliders. Bounds come from the tuned locality's
# data for the tuned target (the same point set OPTIMIZE ALL VARIOGRAMS fits:
# NA-free, projected, deduplicated); values come from the stored fit when one
# exists. See manual_vgm_slider_spec().
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
    d <- stats::na.omit(data.frame(x = ud[[rv$mapping$x]], y = ud[[rv$mapping$y]], v = ud[[col]]))
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

# Per-locality IDW power and TPS lambda. The store holds the encoded value the
# engine reads (-1 Auto; 0 exact TPS or equal-weights IDW; else fixed;
# get_regional_param / set_regional_param) and the panel shows it as a mode
# plus a value, prefilled from the locality's stored value, else from the
# setting for all localities. A stored value is always valid, so an NA is a
# Fixed setting for all localities left without a valid number (the run
# refuses it): it prefills the mode only.
observeEvent(
  list(input$idw_mode, input$idw_m_loc, input$comp_mode, input$idw_m_target,
       input$sep_fit, input$value_type, input$var_id, input$subset),
  {
    req(input$idw_mode == "manual", input$idw_m_loc)
    loc <- input$idw_m_loc
    target <- manual_param_target(input$comp_mode, input$value_type, input$idw_m_target, input$sep_fit)
    val <- get_regional_param("IDW", loc, target,
                              default = idw_param_value(input$idw_p_mode, input$idw_p),
                              key = current_tuning_keys()[[target]])
    mode <- if (is.na(val)) input$idw_p_mode else param_value_mode("IDW", val)
    updateRadioButtons(session, "idw_m_mode", selected = mode)
    if (identical(mode, "fixed") && !is.na(val)) updateNumericInput(session, "idw_m_p", value = val)
  }
)

observeEvent(input$apply_idw_manual, {
  req(input$idw_mode == "manual", input$idw_m_loc)
  loc <- input$idw_m_loc
  target <- manual_param_target(input$comp_mode, input$value_type, input$idw_m_target, input$sep_fit)
  key <- current_tuning_keys()[[target]]
  req(!is.na(key))
  if (identical(input$idw_m_mode, "fixed") && !idw_fixed_ok(input$idw_m_p)) {
    showNotification(sprintf("A fixed IDW power must be a number from 0 (equal weights) to %d; nothing was stored.",
                             IDW_MAX_FINITE_POWER), type = "error")
    return()
  }
  val <- idw_param_value(input$idw_m_mode, input$idw_m_p)
  set_regional_param("IDW", loc, target, val, key)
  showNotification(paste0("IDW power for ", loc, " (", target, ": ", key, "): ",
                          param_setting_text("IDW", val), "."), type = "message")
})

observeEvent(
  list(input$tps_mode, input$tps_m_loc, input$comp_mode, input$tps_m_target,
       input$sep_fit, input$value_type, input$var_id, input$subset),
  {
    req(input$tps_mode == "manual", input$tps_m_loc)
    loc <- input$tps_m_loc
    target <- manual_param_target(input$comp_mode, input$value_type, input$tps_m_target, input$sep_fit)
    val <- get_regional_param("TPS", loc, target,
                              default = tps_param_value(input$tps_lambda_mode, input$tps_lambda),
                              key = current_tuning_keys()[[target]])
    mode <- if (is.na(val)) input$tps_lambda_mode else param_value_mode("TPS", val)
    updateRadioButtons(session, "tps_m_mode", selected = mode)
    if (identical(mode, "fixed") && !is.na(val)) updateNumericInput(session, "tps_m_lambda", value = val)
  }
)

observeEvent(input$apply_tps_manual, {
  req(input$tps_mode == "manual", input$tps_m_loc)
  loc <- input$tps_m_loc
  target <- manual_param_target(input$comp_mode, input$value_type, input$tps_m_target, input$sep_fit)
  key <- current_tuning_keys()[[target]]
  req(!is.na(key))
  if (identical(input$tps_m_mode, "fixed") && !tps_fixed_ok(input$tps_m_lambda)) {
    showNotification("A fixed λ must be a number above 0 (Exact is λ = 0); nothing was stored.", type = "error")
    return()
  }
  val <- tps_param_value(input$tps_m_mode, input$tps_m_lambda)
  set_regional_param("TPS", loc, target, val, key)
  showNotification(paste0("TPS λ for ", loc, " (", target, ": ", key, "): ",
                          param_setting_text("TPS", val), "."), type = "message")
})

# The note under a Per locality panel (per_locality_note): which localities
# have a value of their own for the tuned variable and data subset, and the
# setting the others run with. That setting lives in the All localities
# control, which is hidden while Per locality is selected, and the panel's
# controls start from it for a locality without a value, so without the note a
# prefilled control reads like a stored value.
per_locality_note_ui <- function(type, store, target_switch, selected, global_text) {
  req(rv$user_data)
  target <- manual_param_target(input$comp_mode, input$value_type, target_switch, input$sep_fit)
  locs <- resolve_selected_localities(input$locality, rv$user_data, rv$mapping$loc)
  req(length(locs) > 0)
  lines <- per_locality_note(type, store, target, locs, current_tuning_keys()[[target]],
                             global_text, selected)
  tags$small(style = "display: block; color: var(--mn-text-3); margin: 0 0 8px 0;",
             HTML(paste(htmltools::htmlEscape(lines), collapse = "<br>")))
}
output$idw_m_note <- renderUI({
  req(identical(input$idw_mode, "manual"))
  global <- if (identical(input$idw_p_mode, "fixed") && !idw_fixed_ok(input$idw_p)) {
    "Fixed p, which has no valid value yet (the run will not start)"
  } else {
    param_setting_text("IDW", idw_param_value(input$idw_p_mode, input$idw_p) %||% 2)
  }
  per_locality_note_ui("IDW", rv$idw_factors, input$idw_m_target, input$idw_m_loc, global)
})
output$tps_m_note <- renderUI({
  req(identical(input$tps_mode, "manual"))
  global <- if (identical(input$tps_lambda_mode, "fixed") && !tps_fixed_ok(input$tps_lambda)) {
    "Fixed λ, which has no valid value yet (the run will not start)"
  } else {
    param_setting_text("TPS", tps_param_value(input$tps_lambda_mode, input$tps_lambda))
  }
  per_locality_note_ui("TPS", rv$tps_lambdas, input$tps_m_target, input$tps_m_loc, global)
})

observeEvent(input$auto_fit, {
  req(rv$user_data, rv$mapping$x, rv$mapping$y, rv$mapping$crs)
  locs <- resolve_selected_localities(
    input$locality,
    rv$user_data,
    rv$mapping$loc
  )
  meta <- get_current_meta()
  req(meta)

  # autofit_vgm_item fits BOTH surfaces of a locality, so the job list holds
  # one entry per locality. Every reactive the worker and the diagnostics
  # modal need, want_pre included, is read into a plain value here, before
  # dispatch.
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
    sub_a_raw <- sub_df %>%
      select(x = !!sym(x_col), y = !!sym(y_col), v = !!sym(meta$actual)) %>%
      na.omit()

    sub_p_raw <- NULL
    if (want_pre && !is.null(pred_col) && pred_col %in% colnames(user_data)) {
      sub_p_raw <- sub_df %>%
        select(x = !!sym(x_col), y = !!sym(y_col), v = !!sym(pred_col)) %>%
        na.omit()
    }
    list(l = l, act = sub_a_raw, pre = sub_p_raw,
         key = c(act = tuning_key(meta$actual, eff_subset), pre = tuning_key(pred_col, eff_subset)))
  })
  req(length(jobs) > 0)

  run_optimizer_async(
    btn_id = "auto_fit",
    jobs = jobs,
    worker_name = "autofit_vgm_item",
    worker_args = list(current_crs = current_crs),
    packages = c("sf", "gstat"),
    busy_msg = "Optimizing variograms in the background; the dashboard stays usable.",
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
