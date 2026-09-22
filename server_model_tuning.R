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
  opt_idw = "OPTIMIZE IDW FACTORS",
  opt_tps = "OPTIMIZE TPS LAMBDA",
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

  # A TPS GCV search takes about 0.2 s per locality, less than a nested PSOCK
  # cluster costs to start (measured 1.7 s serial against 3.4 s on 7 workers),
  # so TPS stays asynchronous without a second worker layer. IDW, at about 4 s
  # per locality, gains from one.
  if (identical(worker_name, "tps_gcv_item")) {
    nested_workers <- 1L
  } else {
    nested_workers <- if (length(jobs) > 1L) {
      max(1L, min(cores_hint - 1L, length(jobs)))
    } else {
      1L
    }
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
          # throw, which used to leave the session on the dead cluster even
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

# Localities an optimizer skipped for too few distinct samples
# (OPTIMIZER_MIN_POINTS): nothing was stored for them, so the sidebar setting
# applies. Named in the run log and in one notification per optimization.
note_optimizer_skips <- function(res_list, jobs, engine) {
  idx <- which(vapply(res_list, function(r) !is.null(r$skipped), logical(1)))
  if (length(idx) == 0) return(invisible(NULL))
  lines <- vapply(idx, function(i) {
    paste0(jobs[[i]]$l, " (", jobs[[i]]$target, "): ", res_list[[i]]$skipped)
  }, character(1))
  rv$log <- paste0(rv$log, "\n[Tuning] ", engine, " optimizer skipped ",
                   paste(lines, collapse = "; "), ".")
  showNotification(
    paste0(engine, " optimizer: ", length(idx), " locality/target pair(s) have fewer than ",
           OPTIMIZER_MIN_POINTS, " distinct samples. Nothing was stored for them, so the ",
           "sidebar setting applies (details in the run log)."),
    type = "warning", duration = 10)
  invisible(NULL)
}

# --- TPS Optimization ---
# Lambda presets: the 0.001-step slider makes the special values -1 (Auto)
# and 0 (exact interpolation) hard to hit by dragging.
observeEvent(
  input$tps_preset_auto,
  updateSliderInput(session, "tps_lambda", value = -1)
)
observeEvent(
  input$tps_preset_exact,
  updateSliderInput(session, "tps_lambda", value = 0)
)
observeEvent(
  input$tps_m_preset_auto,
  updateSliderInput(session, "tps_m_lambda", value = -1)
)
observeEvent(
  input$tps_m_preset_exact,
  updateSliderInput(session, "tps_m_lambda", value = 0)
)

tps_opt_vals <- reactiveVal(NULL)
observeEvent(input$opt_tps, {
  req(rv$user_data, input$var_id, input$method == "TPS", rv$mapping$crs)
  locs <- resolve_selected_localities(
    input$locality,
    rv$user_data,
    rv$mapping$loc
  )
  meta <- get_current_meta()
  req(meta)

  # Unticked "Fit Actual/Predicted separately": the Predicted surface reuses
  # the measured values' parameter, so only the Actual target is searched.
  targets <- if ((input$comp_mode || input$value_type != "actual") && isTRUE(input$sep_fit)) {
    c("act", "pre")
  } else {
    "act"
  }

  # Plain values captured BEFORE dispatch (no rv$/input$ may be read inside
  # the promise or its handlers). The act/pre loop is FLATTENED into one jobs
  # list so a single map covers both surfaces - two chained promises would be
  # the only alternative - and each job carries its own `target`, because the
  # observer's loop binding is long gone by the time the handler runs.
  current_crs <- rv$mapping$crs
  user_data <- rv$user_data
  loc_col <- rv$mapping$loc
  x_col <- rv$mapping$x
  y_col <- rv$mapping$y
  value_type <- input$value_type
  eff_subset <- effective_subset(value_type, input$subset, names(user_data))
  keys <- c(act = tuning_key(meta$actual, eff_subset),
            pre = tuning_key(if (value_type == "pred_ss") meta$pred_ss else meta$pred, eff_subset))

  jobs <- unlist(
    lapply(targets, function(tg) {
      val_col <- if (tg == "act") {
        meta$actual
      } else if (value_type == "pred_ss") {
        meta$pred_ss
      } else {
        meta$pred
      }
      if (is.null(val_col) || !(val_col %in% colnames(user_data))) {
        return(NULL)
      }
      lapply(locs, function(l) {
        sub_df <- run_locality_rows(user_data, loc_col, l, eff_subset) %>%
          select(x = !!sym(x_col), y = !!sym(y_col), v = !!sym(val_col)) %>%
          na.omit()
        list(l = l, target = tg, key = keys[[tg]], df = sub_df)
      })
    }),
    recursive = FALSE
  )
  req(length(jobs) > 0)

  rv$tps_gcv_data <- list()

  run_optimizer_async(
    btn_id = "opt_tps",
    jobs = jobs,
    worker_name = "tps_gcv_item",
    worker_args = list(current_crs = current_crs),
    packages = c("sf", "fields"),
    busy_msg = "Optimizing TPS lambda per region in the background; the dashboard stays usable.",
    on_success = function(res_list) {
      note_optimizer_skips(res_list, jobs, "TPS")
      for (i in seq_along(res_list)) {
        res <- res_list[[i]]
        l <- jobs[[i]]$l
        target <- jobs[[i]]$target
        if (!is.null(res$skipped)) next
        if (!is.null(res$err)) {
          rv$log <- paste0(rv$log, "\nTPS Opt Error (", l, "): ", res$err)
          showNotification(
            paste("TPS Optimization failed for", l, "- using fallback lambda."),
            type = "warning"
          )
        } else {
          set_regional_param("TPS", l, target, res$best_lam, jobs[[i]]$key)
          if (!is.null(res$gcv_data)) {
            attr(res$gcv_data, "monolith_key") <- jobs[[i]]$key
            rv$tps_gcv_data[[paste0(l, "_", target)]] <- res$gcv_data
          }
        }
      }

      all_best <- sapply(locs, function(l) get_regional_param("TPS", l, "act", key = keys[["act"]]))
      # A locality whose optimization failed never had set_regional_param
      # called, so get_regional_param returns the -1 Auto sentinel — not a
      # lambda. Keep sentinels out of the slider mean (one failure would drag
      # it negative and silently flip the global default to Auto).
      # Per-locality stored values still win at dispatch; this only sets the
      # fallback slider position.
      ok_best <- all_best[is.finite(all_best) & all_best >= 0]
      if (length(ok_best) > 0) {
        updateSliderInput(session, "tps_lambda", value = mean(ok_best))
      }

      tps_opt_vals(list(locs = locs, targets = targets, keys = keys))
      showNotification(
        "TPS Optimization Complete. Per-region Lambdas stored.",
        type = "message"
      )
    }
  )
})

# Shared builder for the per-locality optimization summary panels (TPS
# lambdas / IDW power factors) - same table, different engine and format.
render_opt_summary_panel <- function(engine, vals_reactive, fmt, heading) {
  renderUI({
    res <- vals_reactive()
    if (is.null(res)) {
      return(NULL)
    }

    # The prediction column only exists when the run actually optimized a
    # predicted target; without one every cell was "N/A", so the column is
    # dropped rather than printed empty.
    has_pre <- "pre" %in% res$targets
    # Values stored for this optimization's keys only; a failed locality or a
    # value since replaced for another key shows N/A, never the sidebar value.
    stored_cell <- function(l, target) {
      val <- get_regional_param(engine, l, target, default = NA_real_, key = res$keys[[target]])
      tags$td(if (is.na(val)) "N/A" else sprintf(fmt, val))
    }

    rows <- lapply(res$locs, function(l) {
      cells <- list(tags$td(l), stored_cell(l, "act"))
      if (has_pre) {
        cells <- c(cells, list(stored_cell(l, "pre")))
      }
      do.call(tags$tr, cells)
    })

    headers <- list(tags$th("Locality"), tags$th("Actual"))
    if (has_pre) {
      headers <- c(headers, list(tags$th("Predicted")))
    }

    div(
      style = "margin-top: 10px; padding: 10px; background-color: var(--mn-surface-2); color: var(--mn-text-2); border: 1px solid var(--mn-line); border-radius: 4px; font-size: 0.8em;",
      h5(paste0(heading, " for ", res$keys[["act"]], ":")),
      tags$table(
        class = "table table-condensed table-bordered",
        style = "background-color: var(--mn-surface); color: var(--mn-text);",
        tags$thead(do.call(tags$tr, headers)),
        tags$tbody(rows)
      )
    )
  })
}

output$tps_opt_panel <- render_opt_summary_panel(
  "TPS",
  tps_opt_vals,
  "%.6f",
  "Optimization Summary (Best Lambdas)"
)

idw_opt_vals <- reactiveVal(NULL)
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
  stores <- c("v_fit_list", "v_emp_list", "idw_factors", "tps_lambdas", "tps_gcv_data")
  had_values <- any(vapply(stores, function(nm) length(rv[[nm]]) > 0L, logical(1)))
  for (nm in stores) rv[[nm]] <- list()
  rv$tuning_revision <- (rv$tuning_revision %||% 0L) + 1L
  rv$vgm_preview <- FALSE
  idw_opt_vals(NULL)
  tps_opt_vals(NULL)
  if (had_values) showNotification("Stored tuning values cleared because the data or coordinate/locality mapping changed.", type = "message")
})
observeEvent(input$opt_idw, {
  req(
    rv$user_data,
    input$var_id,
    input$method == "IDW",
    input$locality,
    rv$mapping$crs
  )

  locs <- resolve_selected_localities(
    input$locality,
    rv$user_data,
    rv$mapping$loc
  )
  meta <- get_current_meta()
  req(meta)

  # Unticked "Fit Actual/Predicted separately": the Predicted surface reuses
  # the measured values' parameter, so only the Actual target is searched.
  targets <- if ((input$comp_mode || input$value_type != "actual") && isTRUE(input$sep_fit)) {
    c("act", "pre")
  } else {
    "act"
  }

  # See the TPS observer: plain values only, act/pre flattened into one jobs
  # list, `target` carried per job.
  current_crs <- rv$mapping$crs
  idw_nmax_val <- input$idw_nmax
  # The power search shares the run's fold authority, so the strategy the
  # user selected has to be captured here (plain value) and shipped to the
  # worker like every other parameter.
  cv_strategy_val <- input$cv_strategy %||% "auto"
  user_data <- rv$user_data
  loc_col <- rv$mapping$loc
  x_col <- rv$mapping$x
  y_col <- rv$mapping$y
  value_type <- input$value_type
  eff_subset <- effective_subset(value_type, input$subset, names(user_data))
  keys <- c(act = tuning_key(meta$actual, eff_subset),
            pre = tuning_key(if (value_type == "pred_ss") meta$pred_ss else meta$pred, eff_subset))

  jobs <- unlist(
    lapply(targets, function(tg) {
      val_col <- if (tg == "act") {
        meta$actual
      } else if (value_type == "pred_ss") {
        meta$pred_ss
      } else {
        meta$pred
      }
      if (is.null(val_col) || !(val_col %in% colnames(user_data))) {
        return(NULL)
      }
      lapply(locs, function(l) {
        sub_df <- run_locality_rows(user_data, loc_col, l, eff_subset) %>%
          select(x = !!sym(x_col), y = !!sym(y_col), v = !!sym(val_col)) %>%
          na.omit()
        list(l = l, target = tg, key = keys[[tg]], df = sub_df)
      })
    }),
    recursive = FALSE
  )
  req(length(jobs) > 0)

  run_optimizer_async(
    btn_id = "opt_idw",
    jobs = jobs,
    worker_name = "idw_opt_item",
    worker_args = list(
      current_crs = current_crs,
      idw_nmax_val = idw_nmax_val,
      cv_strategy = cv_strategy_val
    ),
    packages = c("sf", "gstat"),
    busy_msg = "Calculating optimal IDW factors per region in the background; the dashboard stays usable.",
    on_success = function(res_list) {
      note_optimizer_skips(res_list, jobs, "IDW")
      for (i in seq_along(res_list)) {
        if (!is.null(res_list[[i]]$skipped)) next
        set_regional_param(
          "IDW",
          jobs[[i]]$l,
          jobs[[i]]$target,
          res_list[[i]]$best_f,
          jobs[[i]]$key
        )
      }

      # Powers stored for this run's key only: a skipped locality keeps the
      # sidebar power and must not pull the slider towards it.
      all_best <- sapply(locs, function(l) get_regional_param("IDW", l, "act", default = NA_real_, key = keys[["act"]]))
      ok_best <- all_best[is.finite(all_best)]
      if (length(ok_best) > 0) {
        updateSliderInput(session, "idw_p", value = mean(ok_best))
      }

      idw_opt_vals(list(locs = locs, targets = targets, keys = keys))
      showNotification(
        paste("IDW Optimization Complete for:", paste(locs, collapse = ", ")),
        type = "message",
        duration = 5
      )
    }
  )
})

output$idw_opt_panel <- render_opt_summary_panel(
  "IDW",
  idw_opt_vals,
  "%.1f",
  "Optimization Summary (Best Factors)"
)

output$idw_metrics_table <- renderTable({
  req(input$method == "IDW", rv$cv_metrics_act)
  m_act <- rv$cv_metrics_act
  if (length(m_act) == 0) {
    return(NULL)
  }

  ns <- sapply(m_act, function(x) x$n %||% 0)
  rmses <- sapply(m_act, function(x) x$rmse %||% NA)
  mes <- sapply(m_act, function(x) x$me %||% NA)
  # A locality with fewer than 2 predicted pairs has no RMSE to weight.
  ns[!is.finite(rmses)] <- 0

  total_n <- sum(ns, na.rm = TRUE)
  if (total_n == 0) {
    return(NULL)
  }

  avg_rmse <- sqrt(sum(ns * rmses^2, na.rm = TRUE) / total_n)
  avg_me <- sum(ns * mes, na.rm = TRUE) / total_n

  data.frame(
    Metric = c("Mean CV RMSE (Pooled)", "Mean Bias (ME)"),
    Value = format_sig(c(avg_rmse, avg_me))
  )
})
# Last choice set pushed to each tuning-locality selector: the old code
# compared the choices against the current SELECTION, which never matches
# with 2+ localities, so every trigger re-issued the update and reset the
# user's pick back to the first locality.
tuning_selector_choices <- list()
observeEvent(list(input$locality, rv$user_data, rv$mapping$loc), {
  req(input$locality, rv$user_data, rv$mapping$loc)
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
    pts <- pts[!duplicated(round(sf::st_coordinates(pts), 2)), ]
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

observeEvent(
  list(input$idw_mode, input$idw_m_loc, input$comp_mode, input$idw_m_target,
       input$sep_fit, input$value_type, input$var_id, input$subset),
  {
    req(input$idw_mode == "manual", input$idw_m_loc)
    loc <- input$idw_m_loc
    target <- manual_param_target(input$comp_mode, input$value_type, input$idw_m_target, input$sep_fit)
    val <- get_regional_param("IDW", loc, target, default = input$idw_p,
                              key = current_tuning_keys()[[target]])
    updateSliderInput(session, "idw_m_p", value = val)
  }
)

observeEvent(input$apply_idw_manual, {
  req(input$idw_mode == "manual", input$idw_m_loc)
  loc <- input$idw_m_loc
  target <- manual_param_target(input$comp_mode, input$value_type, input$idw_m_target, input$sep_fit)
  key <- current_tuning_keys()[[target]]
  req(!is.na(key))
  set_regional_param("IDW", loc, target, input$idw_m_p, key)
  showNotification(
    paste("Manual IDW Power applied to", loc, "(", target, ":", key, ")"),
    type = "message"
  )
})

observeEvent(
  list(input$tps_mode, input$tps_m_loc, input$comp_mode, input$tps_m_target,
       input$sep_fit, input$value_type, input$var_id, input$subset),
  {
    req(input$tps_mode == "manual", input$tps_m_loc)
    loc <- input$tps_m_loc
    target <- manual_param_target(input$comp_mode, input$value_type, input$tps_m_target, input$sep_fit)
    val <- get_regional_param("TPS", loc, target, default = input$tps_lambda,
                              key = current_tuning_keys()[[target]])
    updateSliderInput(session, "tps_m_lambda", value = val)
  }
)

observeEvent(input$apply_tps_manual, {
  req(input$tps_mode == "manual", input$tps_m_loc)
  loc <- input$tps_m_loc
  target <- manual_param_target(input$comp_mode, input$value_type, input$tps_m_target, input$sep_fit)
  key <- current_tuning_keys()[[target]]
  req(!is.na(key))
  set_regional_param("TPS", loc, target, input$tps_m_lambda, key)
  showNotification(
    paste("Manual TPS Lambda applied to", loc, "(", target, ":", key, ")"),
    type = "message"
  )
})

observeEvent(input$auto_fit, {
  req(rv$user_data, input$locality, rv$mapping$x, rv$mapping$y, rv$mapping$crs)
  locs <- resolve_selected_localities(
    input$locality,
    rv$user_data,
    rv$mapping$loc
  )
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
          l,
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
