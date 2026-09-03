# server_execution.R (sourced with local = TRUE inside server) - run estimates,
# archive/VIF gates and the future_promise interpolation pipeline. CRITICAL:
# run_params is built from reactives BEFORE the future_promise block; no rv$*/
# input$* may be referenced inside the future, and the nested
# parallelly::makeClusterPSOCK topology must be preserved as-is.
  calculate_run_estimates <- function() {
    meta <- get_current_meta()
    req(meta)
    
    loc_col <- rv$mapping$loc
    selected_locs <- resolve_selected_localities(input$locality, rv$user_data, loc_col)
    n_locs <- length(selected_locs)
    if (n_locs == 0) n_locs <- 1
    
    comp_mode <- isTruthy(input$comp_mode) || isTruthy(input$value_type != "actual")
    # mirror the nested-worker topology the run pipeline actually uses
    cores <- tryCatch(as.integer(future::availableCores()), error = function(e) 1L)
    if (is.null(cores) || is.na(cores) || cores < 1) cores <- 1L
    cores <- if (n_locs > 1) max(1L, min(cores - 1L, n_locs)) else 1L
    
    loc_sample_counts <- numeric(n_locs)
    if (length(selected_locs) > 0 && !is.null(rv$user_data) && !is.null(loc_col) && loc_col %in% colnames(rv$user_data)) {
      for (idx in seq_along(selected_locs)) {
        l <- selected_locs[idx]
        n_samples <- nrow(rv$user_data[rv$user_data[[loc_col]] == l, ])
        loc_sample_counts[idx] <- if (is.null(n_samples) || is.na(n_samples) || n_samples == 0) 50 else n_samples
      }
    } else {
      loc_sample_counts <- rep(50, n_locs)
    }
    
    est_res <- estimate_run_duration(loc_sample_counts, input$method, comp_mode, cores)
    
    return(list(
      meta = meta,
      n_locs = n_locs,
      estimate_text = est_res$estimate_text,
      is_long_run = est_res$is_long_run
    ))
  }

  # Archived registries hold wrapped rasters, so an unbounded archive
  # grows RAM run after run; keep only the most recent few.
  MAX_RUN_HISTORY <- 5L
  push_run_history <- function(entry, base = NULL) {
    hist <- c(list(entry), if (is.null(base)) rv$run_history else base)
    if (length(hist) > MAX_RUN_HISTORY) {
      n_drop <- length(hist) - MAX_RUN_HISTORY
      hist <- hist[seq_len(MAX_RUN_HISTORY)]
      showNotification(paste0("Run archive limit (", MAX_RUN_HISTORY, ") reached: ", n_drop,
                              " oldest archived run(s) discarded to free memory."),
                       type = "warning", duration = 8)
    }
    rv$run_history <- hist
  }

  archive_and_proceed <- function(action, meta, n_locs, estimate_text, is_long_run) {
    if (action == "archive") {
      current_cfg <- rv$run_config_summary
      current_reg <- rv$export_registry
      if (!is.null(current_cfg) && length(current_reg) > 0) {
        push_run_history(list(config = current_cfg, registry = current_reg))
      }
    } else if (action == "discard") {
      rv$v_fit_list <- list()
    }
    
    if (is_long_run) {
      showModal(modalDialog(
        title = "Ready to Run Interpolation",
        tags$p("You are about to start the spatial interpolation pipeline with the following parameters:"),
        div(style = "background-color: #f8f9fa; padding: 12px; border-radius: 5px; margin: 10px 0;",
          tags$strong("Method: "), tags$span(input$method), tags$br(),
          tags$strong("Localities: "), tags$span(n_locs), tags$br(),
          tags$strong("Variables: "), tags$span(meta$label)
        ),
        div(style = "background-color: #e8f4fd; color: #1d6fa5; padding: 10px; border-radius: 5px; margin: 10px 0;",
          icon("hourglass-half"), tags$strong(" Run Estimate: "), tags$span(estimate_text)
        ),
        footer = tagList(
          actionButton("confirm_start_run", "Start Interpolation", class = "btn-primary", icon = icon("play")),
          modalButton("Cancel")
        ),
        size = "m", easyClose = FALSE
      ))
    } else {
      rv$proceed_run <- runif(1)
    }
  }

  # Neither CRS selector has a default, so "not set yet" is the state every
  # user is in immediately after an upload. It has to REFUSE VISIBLY: a bare
  # req() on rv$mapping$crs made the Run button look broken, with no
  # notification, no modal and no run-log line. One closure, called from both
  # gates, so the message cannot exist only on a path that is never reached.
  crs_selection_gate <- function() {
    if (isTruthy(rv$mapping$crs) && isTruthy(input$crs_selection)) return(TRUE)
    missing <- if (!isTruthy(rv$mapping$crs)) "Input Data CRS" else "Target Mapping CRS"
    showModal(modalDialog(
      title = tags$div(style = "color: #d9534f; font-weight: bold;", icon("exclamation-triangle"), paste(missing, "Not Set")),
      tags$p(paste0("Set the ", missing, " on the Data Setup tab before running. Neither selector has a default: the Input Data CRS is the one your X/Y columns were recorded in and cannot be inferred from the coordinates alone, and the Target Mapping CRS is the one every exported raster and shapefile is written in.")),
      tags$p("Check the position printed under the mini-map once the Input Data CRS is selected: if it is not your study area, the CRS is wrong."),
      easyClose = TRUE,
      footer = modalButton("Dismiss")
    ))
    FALSE
  }

  # Identity of a suitability verdict the user has already been shown, so an
  # override cannot silently carry over to a different CRS, or to data that has
  # moved since. A change to either invalidates the acknowledgement.
  crs_gate_key <- function(crs, dev) paste0(as.character(crs)[1], "|", signif(dev, 6))

  # Target Mapping CRS suitability gate. Deliberately mirrors the collinearity
  # gate below: a refusal the user can overrule, with the decision recorded in
  # the run config. Returns TRUE when the run may proceed, FALSE when a modal
  # has been raised and control passes to its buttons.
  crs_suitability_gate <- function() {
    rv$crs_gate_state <- NULL
    pos <- crs_sample_positions(rv$user_data, rv$mapping$x, rv$mapping$y, rv$mapping$crs)
    if (is.null(pos)) return(TRUE)
    suit <- crs_target_suitability(input$crs_selection, pos$lon, pos$lat)
    rv$crs_gate_state <- suit
    if (!identical(suit$level, "block")) return(TRUE)
    if (identical(rv$crs_gate_ack, crs_gate_key(input$crs_selection, suit$dev))) return(TRUE)
    showModal(modalDialog(
      title = tags$div(style = "color: #d9534f; font-weight: bold;",
                       icon("exclamation-triangle"), suit$title),
      tags$p(suit$msg),
      tags$p(suit$detail),
      # Name the CRS that IS right rather than describing it, and name it here
      # too: a user who reaches the run gate without having read the Data Setup
      # advisory must not be sent back to work the answer out for themselves.
      local({
        rec <- crs_recommend_target(pos$lon, pos$lat)
        if (is.null(rec)) {
          tags$p("Choose the UTM zone or national grid the study area belongs to, or continue and accept that every distance this run reports carries that error.")
        } else {
          tags$p("Set the Target Mapping CRS to ", tags$b(sprintf("%s (%s)", rec$crs, rec$label)),
                 " on the Data Setup tab, or continue and accept that every distance this run reports carries that error.")
        }
      }),
      footer = tagList(
        actionButton("crs_gate_override_btn", "Use Anyway (Not Recommended)", class = "btn-warning"),
        modalButton("Cancel")
      ),
      easyClose = FALSE
    ))
    FALSE
  }

  # The covariate-collinearity screen, factored out of observeEvent(input$run)
  # so the CRS gate in front of it can hand control back here after an override
  # without the screen being written twice.
  run_collinearity_gate <- function() {
    if (input$method %in% c("RK", "RFK", "CK") && length(input$aux_vars) > 1 && is.null(rv$vif_choice_made)) {
       # Screen multicollinearity on the data the run will actually fit (the
       # selected localities), not the full table: covariates can be collinear
       # within one locality but not across all of them, and vice versa.
       df_vif <- sf::st_drop_geometry(rv$user_data)
       if (!is.null(rv$mapping$loc) && rv$mapping$loc %in% colnames(df_vif) &&
           length(input$locality) > 0 && !("ALL" %in% input$locality)) {
         df_vif <- df_vif[as.character(df_vif[[rv$mapping$loc]]) %in% input$locality, , drop = FALSE]
       }
       df_aux <- df_vif[, input$aux_vars, drop = FALSE]
       vif_res <- check_vif(df_aux, threshold = 10)

       if (length(vif_res$dropped) > 0) {
          showModal(modalDialog(
            title = tags$div(style = "color: #d9534f; font-weight: bold;", icon("exclamation-triangle"), "High Multicollinearity Detected"),
            tags$p("High correlation / multicollinearity detected among the selected variables within the selected localities. This may destabilize the spatial estimation model."),
            tags$p(tags$b("Variables recommended to be dropped:"), paste(vif_res$dropped, collapse=", ")),
            tags$p("What would you like to do?"),
            footer = tagList(
              actionButton("vif_drop_btn", "Auto-Drop and Continue", class = "btn-success"),
              actionButton("vif_keep_btn", "Keep All (Not Recommended)", class = "btn-warning"),
              modalButton("Cancel")
            ),
            easyClose = FALSE
          ))
          return(invisible(FALSE))
       }
    }

    rv$proceed_vif <- runif(1)
    invisible(TRUE)
  }

  observeEvent(input$crs_gate_override_btn, {
    removeModal()
    suit <- rv$crs_gate_state
    if (is.null(suit)) return()
    rv$crs_gate_ack <- crs_gate_key(input$crs_selection, suit$dev)
    run_collinearity_gate()
  })

  observeEvent(input$vif_drop_btn, {
    removeModal()
    rv$vif_choice_made <- 10
    rv$proceed_vif <- runif(1)
  })

  observeEvent(input$vif_keep_btn, {
    removeModal()
    rv$vif_choice_made <- Inf
    rv$proceed_vif <- runif(1)
  })

  # Locality is part of the reset list because the VIF screen in
  # run_collinearity_gate() runs on the SELECTED localities' data: a drop/keep
  # decision made for one spatial context must not silently carry over to
  # another.
  observeEvent(list(input$method, input$aux_vars, input$locality), {
    rv$vif_choice_made <- NULL
  })

  observeEvent(input$run, {
    if (isTRUE(rv$model_running)) {
      showNotification("A model run is already in progress.", type = "warning")
      return()
    }
    # Cross-feature guard (mirror of run_optimizer_async's model_running check):
    # both paths spawn their own nested PSOCK cluster of cores - 1 workers, so
    # running them concurrently oversubscribes the machine ~2x.
    if (isTRUE(rv$opt_running)) {
      showNotification("An optimization is running; start the interpolation after it finishes.", type = "warning")
      return()
    }
    req(rv$user_data, input$locality, rv$mapping$x, rv$mapping$y)
    if (!crs_selection_gate()) return()

    if (input$method %in% c("RK", "RFK", "CK") && (is.null(input$aux_vars) || length(input$aux_vars) == 0)) {
      showNotification("Please select at least one auxiliary variable for RK/RFK/CK model generation.", type = "error")
      return()
    }

    # Suitability of the Target Mapping CRS is asked FIRST: there is no point
    # settling a collinearity decision for a run that will not be allowed to
    # measure anything correctly, and only one modal is ever open at a time.
    if (!crs_suitability_gate()) return()

    run_collinearity_gate()
  })

  observeEvent(rv$proceed_vif, {
    vif_thresh <- if (!is.null(rv$vif_choice_made)) rv$vif_choice_made else 10
    rv$vif_choice_made <- NULL
    rv$active_vif_thresh <- vif_thresh
    
    est <- calculate_run_estimates()
    meta <- est$meta
    n_locs <- est$n_locs
    estimate_text <- est$estimate_text
    is_long_run <- est$is_long_run
    
    if (!is.null(rv$run_config_summary) && length(rv$export_registry) > 0) {
      if (rv$auto_archive_choice == "archive") {
        archive_and_proceed("archive", meta, n_locs, estimate_text, is_long_run)
      } else if (rv$auto_archive_choice == "discard") {
        archive_and_proceed("discard", meta, n_locs, estimate_text, is_long_run)
      } else {
        showModal(modalDialog(
          title = "Previous Results Detected",
          tags$p("A previous model run exists. What would you like to do with those results?"),
          div(style = "background-color: #f8f9fa; padding: 10px; border-radius: 5px; margin: 10px 0;",
            tags$strong(paste0("Run #", rv$run_config_summary$run_id, ": ",
              rv$run_config_summary$variable, " (", rv$run_config_summary$method, ")")),
            tags$br(),
            tags$small(paste0(rv$run_config_summary$localities, " | ",
              format(rv$run_config_summary$timestamp, "%H:%M:%S")))
          ),
          div(style = "background-color: #e8f4fd; color: #1d6fa5; padding: 10px; border-radius: 5px; margin: 10px 0;",
            icon("hourglass-half"), tags$strong(" Run Estimate: "), tags$span(estimate_text)
          ),
          checkboxInput("auto_archive_remember", "Remember my choice (apply automatically for future runs)", FALSE),
          footer = tagList(
            actionButton("archive_prev_run", "Archive & Continue", class = "btn-warning", icon = icon("archive")),
            actionButton("discard_prev_run", "Discard & Continue", class = "btn-danger", icon = icon("trash")),
            modalButton("Cancel")
          ),
          size = "m", easyClose = FALSE
        ))
      }
    } else {
      archive_and_proceed("none", meta, n_locs, estimate_text, is_long_run)
    }
  })

  observeEvent(input$archive_prev_run, {
    removeModal()
    if (isTRUE(input$auto_archive_remember)) {
      rv$auto_archive_choice <- "archive"
    }
    
    est <- calculate_run_estimates()
    meta <- est$meta
    n_locs <- est$n_locs
    estimate_text <- est$estimate_text
    is_long_run <- est$is_long_run
    
    archive_and_proceed("archive", meta, n_locs, estimate_text, is_long_run)
  })

  observeEvent(input$discard_prev_run, {
    removeModal()
    if (isTRUE(input$auto_archive_remember)) {
      rv$auto_archive_choice <- "discard"
    }
    
    est <- calculate_run_estimates()
    meta <- est$meta
    n_locs <- est$n_locs
    estimate_text <- est$estimate_text
    is_long_run <- est$is_long_run
    
    archive_and_proceed("discard", meta, n_locs, estimate_text, is_long_run)
  })

  observeEvent(input$confirm_start_run, {
    removeModal()
    rv$proceed_run <- runif(1)
  })

  output$reset_archive_choice_ui <- renderUI({
    if (rv$auto_archive_choice != "none") {
      actionButton("reset_archive_choice", "Reset Auto-Archive Decision", class = "btn-secondary btn-sm", style = "width: 100%; margin-top: 10px;", icon = icon("sync-alt"))
    } else {
      NULL
    }
  })

  observeEvent(input$reset_archive_choice, {
    rv$auto_archive_choice <- "none"
    showNotification("Auto-archive/discard setting has been reset. You will be prompted for future runs.", type = "message")
  })

  # Where a model run actually executes. input$run first passes through the
  # archive-confirmation gate and then flips rv$proceed_run; this observer picks
  # it up, validates the coordinate mapping, builds the per-locality point sets,
  # and dispatches the interpolation to the future_promise pipeline so the UI
  # stays responsive while localities are processed.
  observeEvent(rv$proceed_run, {
    if (isTRUE(rv$model_running)) {
      showNotification("A model run is already in progress.", type = "warning")
      return()
    }
    # Re-checked here, not only at input$run: the archive/estimate confirmation
    # modal sits between the two observers, and an optimizer can be started
    # while it is open.
    if (isTRUE(rv$opt_running)) {
      showNotification("An optimization is running; start the interpolation after it finishes.", type = "warning")
      return()
    }
    req(rv$user_data, input$locality, rv$mapping$x, rv$mapping$y);

    # Re-asked here, not only at input$run: the archive/estimate confirmation
    # modal sits between the two observers.
    if (!crs_selection_gate()) return()
    meta <- get_current_meta()
    req(meta)

    x_col_name <- rv$mapping$x
    y_col_name <- rv$mapping$y
    
    if (is.null(x_col_name) || is.null(y_col_name) || !(x_col_name %in% colnames(rv$user_data)) || !(y_col_name %in% colnames(rv$user_data))) {
      showModal(modalDialog(
        title = tags$div(style = "color: #d9534f; font-weight: bold;", icon("exclamation-triangle"), "Coordinate Mapping Error"),
        tags$p("The selected coordinate columns (X, Y) do not exist in the dataset. Please verify your variable mapping in the setup tab."),
        easyClose = TRUE,
        footer = modalButton("Dismiss")
      ))
      return()
    }
    
    x_vals <- rv$user_data[[x_col_name]]
    y_vals <- rv$user_data[[y_col_name]]
    x_num <- suppressWarnings(as.numeric(as.character(x_vals)))
    y_num <- suppressWarnings(as.numeric(as.character(y_vals)))
    
    valid_xy_count <- sum(!is.na(x_num) & !is.na(y_num))
    
    if (valid_xy_count < 3) {
      showModal(modalDialog(
        title = tags$div(style = "color: #d9534f; font-weight: bold;", icon("exclamation-triangle"), "Invalid Coordinate Data"),
        tags$p("The selected coordinate columns (X, Y) do not contain sufficient valid numeric values."),
        tags$p(paste0("Total rows with valid numeric coordinates: ", valid_xy_count, " (minimum 3 required).")),
        tags$p("Please verify that your selected coordinate columns are strictly numeric and contain no missing values (NAs) or text."),
        easyClose = TRUE,
        footer = modalButton("Dismiss")
      ))
      return()
    }
    
    current_method <- input$method
    aux_vars <- input$aux_vars
    if (current_method %in% c("RK", "RFK", "CK") && length(aux_vars) > 0) {
      missing_vars <- setdiff(aux_vars, colnames(rv$user_data))
      if (length(missing_vars) > 0) {
        showModal(modalDialog(
          title = tags$div(style = "color: #d9534f; font-weight: bold;", icon("exclamation-triangle"), "Missing Covariates"),
          tags$p("The following selected covariates do not exist in the dataset:"),
          tags$pre(style = "background-color: #f8d7da; color: #721c24; border: 1px solid #f5c6cb; padding: 10px; border-radius: 4px;", paste(missing_vars, collapse = ", ")),
          easyClose = TRUE,
          footer = modalButton("Dismiss")
        ))
        return()
      }
      
      non_numeric_vars <- c()
      for (v in aux_vars) {
        v_vals <- suppressWarnings(as.numeric(as.character(rv$user_data[[v]])))
        if (sum(!is.na(v_vals)) < 3) {
          non_numeric_vars <- c(non_numeric_vars, v)
        }
      }
      
      if (length(non_numeric_vars) > 0) {
        showModal(modalDialog(
          title = tags$div(style = "color: #d9534f; font-weight: bold;", icon("exclamation-triangle"), "Non-Numeric Covariates"),
          tags$p("The following selected covariates do not contain sufficient valid numeric values (minimum 3 required):"),
          tags$pre(style = "background-color: #f8d7da; color: #721c24; border: 1px solid #f5c6cb; padding: 10px; border-radius: 4px;", paste(non_numeric_vars, collapse = ", ")),
          easyClose = TRUE,
          footer = modalButton("Dismiss")
        ))
        return()
      }
    }

    # ── Every CRS refusal happens HERE, before a single piece of state moves ──
    # This is the last block of the observer's validation half. Everything
    # below it - the run counter, the raster caches, rv$disp, the export
    # registry, the CV lists - is COMMITTED, so an early return past this point
    # leaves the session with the previous run's results destroyed and a
    # run-config panel describing a run that never happened. There is no unwind
    # path on purpose: refuse before committing, and nothing needs unwinding.

    # require_metric: every distance this pipeline accepts or reports (grid
    # resolution, buffer radius, variogram range, the ruler's projected column)
    # is stated in metres while the engines work on the CRS's own axis units, so
    # a projected Target Mapping CRS on any other linear unit is refused here
    # rather than allowed to mean something else throughout the run.
    safe_crs <- validate_crs(input$crs_selection, "CRS Validation Error:", duration = 15,
                             require_metric = TRUE)
    if (is.null(safe_crs)) return()
    # Both selectors are free-typed (selectize create = TRUE), so an
    # unparseable entry reaches this point having passed every selection-time
    # advisory silently; catch it with a clear notification instead of letting
    # st_as_sf() fail deep inside the interpolation worker.
    safe_src_crs <- validate_crs(rv$mapping$crs, "Input Data CRS Validation Error:", duration = 15)
    if (is.null(safe_src_crs)) return()

    # Suitability enforcement. The gate at input$run is where the user is
    # asked; this is the check no path can bypass, re-measured here rather than
    # trusted. A "block" verdict stops the run unless that exact verdict was
    # explicitly overridden, and a verdict that cannot be reached ("cannot
    # answer") never stops anything.
    crs_suit_pos <- crs_sample_positions(rv$user_data, x_col_name, y_col_name, rv$mapping$crs)
    crs_suit <- if (is.null(crs_suit_pos)) NULL else
      crs_target_suitability(input$crs_selection, crs_suit_pos$lon, crs_suit_pos$lat)
    crs_override <- FALSE
    if (!is.null(crs_suit) && identical(crs_suit$level, "block")) {
      if (!identical(rv$crs_gate_ack, crs_gate_key(input$crs_selection, crs_suit$dev))) {
        showNotification(paste0(crs_suit$title, ". ", crs_suit$msg), type = "error", duration = 20)
        return()
      }
      crs_override <- TRUE
    }

    locs <- resolve_selected_localities(input$locality, rv$user_data, rv$mapping$loc)

    if (!is.null(rv$run_config_summary) && rv$run_config_summary$method != input$method) {
      rv$v_fit_list <- list()
    }

    # Switch tabs client-side: shinyjs messages reach the browser immediately,
    # whereas updateTabsetPanel queues an input message that is only flushed
    # after this whole observer (validation + data prep + future dispatch)
    # finishes - the pan would lag several seconds and yank the user back if
    # they had already navigated elsewhere in the meantime.
    shinyjs::runjs("$('#main_tabs a[data-value=\"tab_map\"]').tab('show');")

    shinyjs::disable("run")
    # Label swap must go through updateActionButton: Shiny 1.13 renders the
    # label into a .action-label child span, and raw shinyjs::html() here
    # destroys that span, so later updateActionButton restores (cancel/finish)
    # would APPEND their label next to the stale text instead of replacing it.
    updateActionButton(session, "run", label = "Interpolating...", icon = icon("spinner", class = "fa-spin"))

    shinyjs::show("map_processing_overlay")
    shinyjs::show("map_spinner")
    shinyjs::show("map_progress_bar_container")
    shinyjs::show("cancel_model_btn")
    shinyjs::hide("reveal_maps_btn")
    shinyjs::html("map_processing_title", "Processing...")
    update_premium_progress(5, "Initializing Spatial Analysis Engine...")

    # Per-RUN flag, not per-session: clearing one shared flag at the start of run
    # N+1 revoked the cancellation of run N's still-in-flight workers, which then
    # ran to completion alongside the new batch (their results discarded by the
    # run_token guard, their cores not). rv$run_counter is bumped a few lines
    # below, so the id for THIS run is the next value.
    cancel_file <- file.path(session_progress_dir,
                             paste0("cancel_flag_", rv$run_counter + 1L, ".txt"))
    if (file.exists(cancel_file)) tryCatch(file.remove(cancel_file), error = function(e) NULL)
    # Clear the previous run's status files, WARNINGS INCLUDED: the progress
    # panel simply lists every warn_ file it finds, so a warning left behind by
    # the last run is shown against this one - stating a strict buffer/cell-size
    # mismatch, or an engine fallback, that the user may have just fixed.
    old_files <- list.files(path = session_progress_dir, pattern = paste0("^(progress|warn)_", session_id, "_.*_.*\\.txt$"), full.names = TRUE)
    if (length(old_files) > 0) tryCatch(file.remove(old_files), error = function(e) NULL)
    rv$model_running <- TRUE
    rv$run_pct <- 0

    rv$run_counter <- rv$run_counter + 1L
    clear_raster_caches()
    method_params_list <- list(
      "IDW" = paste0("IDW Power: ", input$idw_p, " | Nmax: ", input$idw_nmax),
      "TPS" = paste0("TPS Lambda: ", input$tps_lambda),
      "OK"  = "Ordinary Kriging (auto variogram)",
      "RK"  = paste0("Regression Kriging | Aux: ", paste(input$aux_vars, collapse=", ")),
      "RFK" = paste0("Random Forest Kriging | Aux: ", paste(input$aux_vars, collapse=", ")),
      "CK"  = paste0("Co-Kriging | Aux: ", paste(input$aux_vars, collapse=", "), " | Nmax: ", input$ck_nmax %||% 15)
    )
    method_params_str <- method_params_list[[input$method]] %||% ""
    # The RFK trend forest has no sidebar control; it runs at randomForest's
    # package default via apply_kriging_pipeline. Pinning it here makes the
    # dispatch explicit and lets the run record state what was actually used
    # (identical numerically - the engine falls back to this same 200).
    rfk_ntree_val <- 200
    # Repeated CV is opt-in (it costs one extra full CV pass per repeat) and
    # collapses to 1 wherever the resolved plan is LOOCV, which is deterministic.
    cv_repeats_val <- if (isTRUE(input$cv_repeat_on)) {
      max(1L, min(25L, suppressWarnings(as.integer(input$cv_repeat_n %||% 5))))
    } else 1L
    if (is.na(cv_repeats_val)) cv_repeats_val <- 1L
    # Everything an archived run needs to be told apart from another one, and
    # everything a methods section has to state. Two runs that differ only in CV
    # strategy or in the collinearity decision used to look identical here.
    rv$run_config_summary <- list(
      run_id = rv$run_counter,
      timestamp = Sys.time(),
      app_version = app_version,
      variable = paste0(meta$label, " [", meta$actual, "]"),
      method = input$method,
      localities = paste(locs, collapse = ", "),
      subset = input$subset,
      value_type = input$value_type,
      crs = rv$mapping$crs,
      boundary_type = input$boundary_type,
      buffer_mode = input$buff_mode,
      buffer_dist = input$buff_dist,
      resolution = input$grid_res,
      res_mode = input$res_mode,
      comp_mode = input$comp_mode,
      sep_fit = input$sep_fit,
      cv_strategy = input$cv_strategy %||% "auto",
      cv_repeats = cv_repeats_val,
      covariates = if (input$method %in% c("RK", "RFK", "CK")) paste(input$aux_vars, collapse = ", ") else NA,
      # The RESOLVED gate the dispatch passes into run_params: Inf records the
      # user's "Keep All (Not Recommended)" choice in the collinearity modal.
      vif_threshold = if (input$method %in% c("RK", "RFK", "CK")) (rv$active_vif_thresh %||% 10) else NA,
      rf_ntree = if (input$method == "RFK") rfk_ntree_val else NA,
      rfk_uncertainty = if (input$method == "RFK") (input$rfk_uncertainty %||% "jackknife") else NA,
      ck_nmax = if (input$method == "CK") (input$ck_nmax %||% 15) else NA,
      method_params = method_params_str
    )

    # Committed display context: everything the Map Viewer and Scientific
    # Analysis tabs render is keyed to this snapshot (via get_display_meta or
    # rv$disp directly), never to the live sidebar inputs, so reconfiguring
    # the sidebar for the next run cannot alter the displayed results.
    # Superset of get_current_meta()'s fields so it is a drop-in replacement.
    rv$disp <- c(meta, list(
      var_id = meta$actual,
      method = input$method,
      value_type = input$value_type,
      comp_mode = isTRUE(input$comp_mode),
      localities = locs,
      # The CRS this run was computed in. The Map Viewer's ruler reports its
      # projected figure against it, so that figure keeps naming the system the
      # displayed surface, its variogram lags and its grid resolution live in
      # even after the sidebar has been retargeted for the next run.
      crs_sel = input$crs_selection,
      # The INPUT-side mapping this run was computed from. Every layer on the
      # Map Viewer is a snapshot of the last run, so changing the Input Data
      # CRS or the X/Y columns afterwards moves nothing there until Generate
      # is pressed again. Recording the mapping is what lets that divergence
      # be reported (map_crs_stale_note, server_map_viewer.R) instead of
      # leaving the control looking inert.
      map_crs = rv$mapping$crs,
      map_x = rv$mapping$x,
      map_y = rv$mapping$y
    ))

    tryCatch({
      rv$export_registry <- list()
      rv$rast_list_act <- list(); rv$rast_list_pre <- list(); sf_list <- list(); b_list <- list()
      rv$rast <- NULL; rv$rast_pred <- NULL; rv$rast_res <- NULL; rv$has_predictions <- FALSE
    rv$v_emp_list <- list(); rv$log <- paste0("[Run #", rv$run_counter, "] Starting spatial interpolation using method: ", input$method, "...")
    rv$model_summaries <- list(); rv$rf_models <- list(); rv$gstat_objs <- list()
    rv$cv_metrics_act <- list(); rv$cv_metrics_pre <- list() # Reset CV metrics
    rv$cv_data_act <- list(); rv$cv_data_pre <- list()
    rv$cv_repeats_act <- NULL; rv$cv_repeats_pre <- NULL
    rv$cv_strategy_sel <- input$cv_strategy %||% "auto"
    rv$cv_repeats_sel <- cv_repeats_val
    
    update_premium_progress(15, "Validating and Cleaning Spatial Input Data...")
    
    pred_col <- if(input$value_type == "pred_ss") meta$pred_ss else meta$pred
    aux_vars <- input$aux_vars
    
    update_premium_progress(25, "Preparing Neighborhood Search Grids...")
    
    current_method <- input$method
    current_crs <- rv$mapping$crs
    current_loc_col <- rv$mapping$loc
    current_x_col <- rv$mapping$x
    current_y_col <- rv$mapping$y
    val_type <- input$value_type
    subset_val <- input$subset
    actual_col <- meta$actual
    b_type <- input$boundary_type
    buff_mode <- input$buff_mode
    b_dist <- input$buff_dist
    shp_bound <- rv$shp_bound
    res_mode <- input$res_mode
    grid_res <- input$grid_res
    crs_sel <- input$crs_selection
    
    rv$run_config_summary$crs_scale_factor <- if (is.null(crs_suit)) NA_real_ else crs_suit$k
    rv$run_config_summary$crs_gate_override <- crs_override

    comp_mode <- input$comp_mode
    sep_fit <- input$sep_fit
    idw_p_val <- input$idw_p
    idw_nmax_val <- input$idw_nmax
    tps_lambda_val <- input$tps_lambda
    
    update_premium_progress(35, "Organizing Localized Data Chunks...")
    
    df_list <- lapply(locs, function(l) {
      sub_df <- rv$user_data %>% filter(!!sym(current_loc_col) == l)
      subset_col <- find_subset_column(colnames(sub_df))
      if (val_type == "pred_ss" && !is.na(subset_col) && subset_val != "all") {
        sub_df <- sub_df[!is.na(sub_df[[subset_col]]) & sub_df[[subset_col]] == subset_val, , drop = FALSE]
      }
      
      pts_data <- sub_df
      pts_data$x <- sub_df[[current_x_col]]
      pts_data$y <- sub_df[[current_y_col]]
      pts_data$v <- sub_df[[actual_col]]
      pts_data$pv <- if (!is.null(pred_col) && pred_col %in% colnames(sub_df)) sub_df[[pred_col]] else NA
      
      m_params <- list(
        idw_p_act = get_regional_param("IDW", l, "act", default = idw_p_val %||% 2),
        idw_p_pre = get_regional_param("IDW", l, "pre", default = idw_p_val %||% 2),
        idw_nmax = idw_nmax_val %||% 12,
        tps_lambda_act = get_regional_param("TPS", l, "act", default = tps_lambda_val),
        tps_lambda_pre = get_regional_param("TPS", l, "pre", default = tps_lambda_val),
        pre_fit_act = clean_gstat_env(rv$v_fit_list[[paste0(l, "_act")]]),
        pre_fit_pre = clean_gstat_env(if(sep_fit) rv$v_fit_list[[paste0(l, "_pre")]] else rv$v_fit_list[[paste0(l, "_act")]]),
        cv_strategy = input$cv_strategy %||% "auto",
        cv_repeats = cv_repeats_val,
        rfk_uncertainty = input$rfk_uncertainty %||% "jackknife",
        rf_ntree = rfk_ntree_val,
        ck_nmax = input$ck_nmax %||% 15
      )

      list(l = l, pts_data = pts_data, m_params = m_params)
    })

    # Snapshot the per-locality method params this run actually consumes so
    # display/export tables report them; the live tuning store holds no entry
    # for localities that fell back to the global slider value.
    rv$disp$regional_params <- setNames(
      lapply(df_list, function(item) item$m_params[c("idw_p_act", "idw_p_pre", "tps_lambda_act", "tps_lambda_pre")]),
      vapply(df_list, function(item) item$l, character(1))
    )

    # Manual variogram fits reach every engine in m_params, but only the OK
    # branch consumes them: RK/RFK refit the RESIDUAL variogram once the trend
    # is removed and CK fits an LMC (both correct - a value-scale model must not
    # be imposed on residuals). Say so instead of ignoring the user's tuning in
    # silence. Gated on Manual mode: stored fits also come from OPTIMIZE ALL
    # VARIOGRAMS and from previous runs, where nothing was hand-tuned.
    if (identical(input$vgm_mode, "manual") && current_method %in% c("RK", "RFK", "CK")) {
      fit_keys <- c(paste0(locs, "_act"), paste0(locs, "_pre"))
      if (any(fit_keys %in% names(rv$v_fit_list))) {
        manual_note <- paste0("Manual variogram fits are consumed by Ordinary Kriging only. ",
                              current_method, " fits its own variogram model (residual variogram for RK/RFK, linear model of coregionalization for CK), so the tuned fit will not be used in this run.")
        showNotification(manual_note, type = "warning", duration = 12)
        rv$log <- paste0(rv$log, "\n[Variogram] ", manual_note)
      }
    }

    update_premium_progress(50, "Executing Parallel Interpolation Algorithms...")

    rv$rast_list_act <- list(); rv$rast_list_pre <- list(); rv$rast_list_res <- list(); rv$rast_list_point_res <- list()

    main_wd <- getwd()
    progress_dir_val <- session_progress_dir
    session_id_val <- session_id
    cancel_file_val <- file.path(session_progress_dir,
                                 paste0("cancel_flag_", rv$run_counter, ".txt"))

    rv$run_token <- rv$run_token + 1L
    this_token <- rv$run_token

    log_start_time <- Sys.time()
    log_method <- current_method
    log_comp_mode <- comp_mode
    log_n_locs <- length(df_list)
    log_sample_counts <- sapply(df_list, function(x) nrow(x$pts_data))
    
    vif_thresh_local <- rv$active_vif_thresh

    # Everything the workers need is a plain-data list plus TOP-LEVEL
    # functions from spatial_helpers.R. The promise worker and each nested
    # worker source() that file themselves, so no function values have to be
    # shipped as globals at all (shipping monolith-defined closures used to
    # drag their source environments to every worker). interp_run_item is
    # referenced by name below: a plain named reference is shipped reliably
    # by automatic globals detection (future_promise does NOT honor future's
    # structure(TRUE, add=) globals idiom).
    run_params <- list(
      main_wd = main_wd,
      current_method = current_method, current_crs = current_crs, aux_vars = aux_vars,
      shp_bound = shp_bound, b_type = b_type, buff_mode = buff_mode, b_dist = b_dist,
      res_mode = res_mode, grid_res = grid_res, crs_sel = crs_sel,
      comp_mode = comp_mode, val_type = val_type,
      progress_dir_val = progress_dir_val, session_id_val = session_id_val,
      cancel_file_val = cancel_file_val, vif_threshold = vif_thresh_local
    )

    # Nested futures default to a sequential plan, so without an explicit
    # escalation all localities run one after another inside the single
    # future_promise worker. The nested worker count is decided HERE in the
    # main session (availableCores() introspection inside a PSOCK worker is
    # unreliable) and shipped into the worker as plain data. Numerics are
    # plan-independent: furrr's fixed seed assigns one L'Ecuyer stream per
    # locality regardless of topology.
    cores_hint <- tryCatch(as.integer(future::availableCores()), error = function(e) 1L)
    nested_workers <- if (length(df_list) > 1L) max(1L, min(cores_hint - 1L, length(df_list))) else 1L
    # record the ACTUAL parallelism in run_history.csv so the duration
    # estimator calibrates against what really happened
    log_cores <- nested_workers

    promises::future_promise({
      setwd(main_wd)
      # Define the full helper set (interp_run_item included) in the promise
      # worker's GLOBAL env; nested workers repeat this themselves inside
      # interp_run_item because they are fresh processes.
      source("spatial_helpers.R", local = FALSE)

      nested_cl <- NULL
      old_mc_cores <- getOption("mc.cores")
      if (nested_workers >= 2L && future::nbrOfWorkers() == 1L) {
        # PSOCK workers report mc.cores = 1; the main session allocated
        # nested_workers cores to this batch, so tell parallelly before
        # spawning or its worker-count guard misfires. Owning the cluster
        # explicitly (instead of plan(multisession)) guarantees a clean
        # teardown in the finally block below.
        options(mc.cores = nested_workers)
        nested_cl <- parallelly::makeClusterPSOCK(nested_workers)
        future::plan(future::cluster, workers = nested_cl)
      }

      tryCatch({
        # interp_run_item is TOP-LEVEL (globalenv-enclosed after the source()
        # above), so furrr ships a lean function value instead of a closure
        # over this future's evaluation environment; run parameters travel as
        # one plain-data argument.
        furrr::future_map(df_list, interp_run_item, run_params = run_params,
          .options = furrr::furrr_options(
            seed = 12345,
            # packages used UNQUALIFIED in the helper call graph; namespaced
            # calls (terra::, FNN::, randomForest::, ...) need no attaching
            packages = c("sf", "gstat", "dplyr")
          ))
      }, finally = {
        # tear the nested cluster down and restore mc.cores so the (reused)
        # promise worker returns to the plain single-threaded state other
        # future_promise tasks expect
        if (!is.null(nested_cl)) {
          future::plan(future::sequential)
          parallel::stopCluster(nested_cl)
          options(mc.cores = old_mc_cores)
        }
      })
    }, seed = 12345) %...>% (function(res_all) {
      if (this_token != rv$run_token) return()

      # Everything below runs in the MAIN session on results the workers already
      # returned successfully. It gets its own tryCatch because `p %...>% f
      # %...!% g` routes rejections from BOTH p and f to g: without this, a
      # failure in merge_wrapped_rasters, register_export_item, the leaflet
      # fitBounds or the kappa tables was reported as "Parallel Interpolation
      # Failed" with troubleshooting advice about coordinate columns and
      # collinear covariates — i.e. the user was sent to debug a worker that
      # had in fact finished. With the assembly body guarded here, `%...!%`
      # below genuinely means "the parallel run itself failed".
      tryCatch({

      tryCatch({
        batch_elapsed_sec <- as.numeric(difftime(Sys.time(), log_start_time, units = "secs"))
        # Same resolver the estimator reads, so writer and reader can never
        # disagree about where the history lives (user data dir, not the wd).
        history_file <- monolith_history_file()
        dir.create(dirname(history_file), recursive = TRUE, showWarnings = FALSE)

        total_samples <- sum(log_sample_counts)
        per_locality_share <- if (total_samples > 0) {
          batch_elapsed_sec * (log_sample_counts / total_samples)
        } else {
          rep(batch_elapsed_sec / log_n_locs, log_n_locs)
        }
        
        new_rows <- data.frame(
          timestamp = format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
          method = log_method,
          comp_mode = log_comp_mode,
          n_locs_in_batch = log_n_locs,
          n_samples = log_sample_counts,
          cores_used = log_cores,
          batch_elapsed_sec = batch_elapsed_sec,
          per_locality_share_sec = per_locality_share,
          stringsAsFactors = FALSE
        )
        
        if (file.exists(history_file)) {
          write.table(new_rows, history_file, append = TRUE, sep = ",", row.names = FALSE, col.names = FALSE)
        } else {
          write.table(new_rows, history_file, append = FALSE, sep = ",", row.names = FALSE, col.names = TRUE)
        }
      }, error = function(e) {})
      
      # Failures are collected and shown in ONE modal after the loop: a
      # showModal per locality replaced the previous dialog, so with several
      # failing regions the user saw only the last one (and the stacked
      # notifications auto-expire after 15s).
      failed_regions <- list()
      # Repeated-CV frames, gathered per locality so the pooled ("Total
      # (Combined)") repeat rows are built from the same locality set as the
      # pooled row of the metrics table. Localities whose plan degraded to
      # LOOCV contribute their single (deterministic) frame - see
      # build_cv_repeat_summary.
      reps_act <- list(); reps_pre <- list()

      for(res in res_all) {
          l <- res$l
          if(res$log_msg != "") {
              rv$log <- paste0(rv$log, res$log_msg)
              if(grepl("Error", res$log_msg)) {
                showNotification(paste("Error in region:", l, "-", res$log_msg), type = "error", duration = 15)
                failed_regions[[l]] <- res$log_msg
              }
          }
          if(!is.null(res$r_a)) rv$rast_list_act[[l]] <- res$r_a
          if(!is.null(res$r_p)) rv$rast_list_pre[[l]] <- res$r_p
          if(!is.null(res$r_res)) rv$rast_list_res[[l]] <- res$r_res
          if(!is.null(res$r_point_err)) rv$rast_list_point_res[[l]] <- res$r_point_err
          if(!is.null(res$bound)) b_list[[length(b_list)+1]] <- res$bound
          if(!is.null(res$pts)) sf_list[[length(sf_list)+1]] <- res$pts
          
          if(!is.null(res$v_emp_act)) rv$v_emp_list[[paste0(l, "_act")]] <- res$v_emp_act
          if(!is.null(res$v_fit_act)) rv$v_fit_list[[paste0(l, "_act")]] <- res$v_fit_act
          if(!is.null(res$cv_act)) rv$cv_metrics_act[[l]] <- res$cv_act
          if(!is.null(res$cv_obj_act)) rv$cv_data_act[[l]] <- res$cv_obj_act
          if(cv_repeats_val > 1) {
            reps_act[[l]] <- res$cv_reps_act %||% Filter(Negate(is.null), list(cv_repeat_frame(res$cv_obj_act)))
          }
          if(!is.null(res$summ_act)) rv$model_summaries[[paste0(l, "_act")]] <- res$summ_act
          if(!is.null(res$rf_act)) rv$rf_models[[paste0(l, "_act")]] <- res$rf_act
          if(!is.null(res$gstat_act)) rv$gstat_objs[[paste0(l, "_act")]] <- res$gstat_act
          
          if(!is.null(res$v_emp_pre)) rv$v_emp_list[[paste0(l, "_pre")]] <- res$v_emp_pre
          if(!is.null(res$v_fit_pre)) rv$v_fit_list[[paste0(l, "_pre")]] <- res$v_fit_pre
          if(!is.null(res$cv_pre)) rv$cv_metrics_pre[[l]] <- res$cv_pre
          if(!is.null(res$cv_obj_pre)) rv$cv_data_pre[[l]] <- res$cv_obj_pre
          if(cv_repeats_val > 1) {
            reps_pre[[l]] <- res$cv_reps_pre %||% Filter(Negate(is.null), list(cv_repeat_frame(res$cv_obj_pre)))
          }
          if(!is.null(res$summ_pre)) rv$model_summaries[[paste0(l, "_pre")]] <- res$summ_pre
          if(!is.null(res$rf_pre)) rv$rf_models[[paste0(l, "_pre")]] <- res$rf_pre
          if(!is.null(res$gstat_pre)) rv$gstat_objs[[paste0(l, "_pre")]] <- res$gstat_pre
      }

      if (cv_repeats_val > 1) {
        # Summarised once per run (not per render): the pooled rows reproject
        # and pool every locality's frames, which is far too much work to
        # repeat on each locality-filter change.
        rv$cv_repeats_act <- build_cv_repeat_summary(reps_act)
        # rv$has_predictions is only set further down this handler; the
        # collected frames are the reliable signal that a predicted surface ran.
        if (length(reps_pre) > 0) rv$cv_repeats_pre <- build_cv_repeat_summary(reps_pre)
        if (is.null(rv$cv_repeats_act)) {
          rv$log <- paste0(rv$log, "\n[Repeated CV] No locality produced more than one fold realization",
                           " (leave-one-out plans are deterministic); reporting single-realization metrics.")
        }
      }

      if (length(failed_regions) > 0) {
        showModal(modalDialog(
          title = tags$div(style = "color: #d9534f; font-weight: bold;", icon("exclamation-circle"),
                           sprintf("%d Region(s) Failed", length(failed_regions))),
          tags$p("An error occurred during modeling of the following localities:"),
          lapply(names(failed_regions), function(l) {
            tagList(
              tags$p(style = "margin-bottom: 4px;", tags$b(l)),
              tags$pre(style = "background-color: #f8d7da; color: #721c24; border: 1px solid #f5c6cb; padding: 15px; border-radius: 4px; overflow-x: auto; white-space: pre-wrap; font-family: monospace; font-size: 0.9em;", failed_regions[[l]])
            )
          }),
          easyClose = TRUE,
          footer = modalButton("Dismiss")
        ))
      }

    valid_a <- Filter(Negate(is.null), rv$rast_list_act)
    valid_p <- Filter(Negate(is.null), rv$rast_list_pre)
    valid_r <- Filter(Negate(is.null), rv$rast_list_res)
    valid_pr <- Filter(Negate(is.null), rv$rast_list_point_res)
    
    if(length(valid_a) > 0) {
      rv$rast <- merge_wrapped_rasters(valid_a)
      register_export_item("map_actual", paste(meta$label, "- Actual Map"), "map", rv$rast, meta$category)
      
      # Uncertainty products exist for the kriging engines only. IDW's var1.var
      # is all NA and TPS has none at all, so registering these for those
      # methods shipped two blank rasters into the export panel (the map
      # viewer's uncertainty toggle already carried this guard).
      temp_rast_a <- terra::unwrap(rv$rast)
      if (method_has_variance(current_method) && "var1.var" %in% names(temp_rast_a)) {
        uncert_var_a <- temp_rast_a[["var1.var"]]
        register_export_item("map_uncert_var_act", paste(meta$label, "- Uncertainty Map (Variance - Actual)"), "map", terra::wrap(uncert_var_a), meta$category, kind = "uncertainty")
        register_export_item("map_uncert_se_act", paste(meta$label, "- Uncertainty Map (SE - Actual)"), "map", terra::wrap(sqrt(uncert_var_a)), meta$category, kind = "uncertainty")
      }
    }
    if(length(valid_p) > 0) {
      rv$rast_pred <- merge_wrapped_rasters(valid_p)
      rv$has_predictions <- TRUE
      register_export_item("map_predicted", paste(meta$label, "- Predicted Map"), "map", rv$rast_pred, meta$category)
      
      temp_rast_p <- terra::unwrap(rv$rast_pred)
      if (method_has_variance(current_method) && "var1.var" %in% names(temp_rast_p)) {
        uncert_var_p <- temp_rast_p[["var1.var"]]
        register_export_item("map_uncert_var_pre", paste(meta$label, "- Uncertainty Map (Variance - Predicted)"), "map", terra::wrap(uncert_var_p), meta$category, kind = "uncertainty")
        register_export_item("map_uncert_se_pre", paste(meta$label, "- Uncertainty Map (SE - Predicted)"), "map", terra::wrap(sqrt(uncert_var_p)), meta$category, kind = "uncertainty")
      }
    }
    if(length(valid_r) > 0) {
      rv$rast_res <- merge_wrapped_rasters(valid_r)
      register_export_item("map_residuals", paste(meta$label, "- ML Predictions Residual Map (Delta)"), "map", rv$rast_res, meta$category, kind = "residual")
    }
    if(length(valid_pr) > 0) {
      rv$rast_point_res <- merge_wrapped_rasters(valid_pr)
      register_export_item("map_interp_point_errors", paste(meta$label, "- ML Predictions Interpolated Point Errors Map"), "map", rv$rast_point_res, meta$category, kind = "residual")
    }
    
    if(!is.null(rv$rast) && !is.null(rv$rast_pred)) {
       register_export_item("map_comparison", paste(meta$label, "- Actual vs Predicted Comparison"), "map_combined", list(act = rv$rast, pre = rv$rast_pred), meta$category)
    }
    
    if(length(sf_list) > 0) {
      target_crs <- sf::st_crs(sf_list[[1]])
      sf_list_aligned <- lapply(sf_list, function(x) {
        if (sf::st_crs(x) != target_crs) {
          sf::st_transform(x, target_crs)
        } else {
          x
        }
      })
      rv$sf <- do.call(rbind, sf_list_aligned)
    }
    valid_bounds <- Filter(function(x) !is.null(x) && inherits(x, "sf"), b_list)
    if(length(valid_bounds) > 0) {
      target_crs_b <- sf::st_crs(valid_bounds[[1]])
      b_list_aligned <- lapply(valid_bounds, function(x) {
        if (sf::st_crs(x) != target_crs_b) {
          sf::st_transform(x, target_crs_b)
        } else {
          x
        }
      })
      rv$bound <- do.call(rbind, unname(b_list_aligned)) %>% sf::st_union()
    }
    rv$loc_names <- names(valid_a)
    # Signals "this run's results are now in rv$..." to the cached Scientific
    # Analysis plots (their cache keys embed it).
    rv$results_rev <- rv$results_rev + 1L

    # Point error map: the discrete sample-location errors the Map Viewer's
    # Point Residuals panel shows, exported as points (not the IDW surface,
    # which is registered separately above as Interpolated Point Errors).
    if (!is.null(rv$sf) && "resid" %in% colnames(rv$sf) && any(!is.na(rv$sf$resid))) {
      pts_err <- rv$sf[!is.na(rv$sf$resid), c("resid", "loc")]
      register_export_item("map_point_residuals", paste(meta$label, "- ML Predictions Point Error Map"),
                           "map", list(pts = pts_err, bound = rv$bound), meta$category, kind = "residual")
    }
    
    if (!is.null(rv$bound)) {
      tryCatch({
        bbox <- sf::st_bbox(sf::st_transform(sf::st_as_sf(rv$bound), 4326))
        leafletProxy("main_map") %>% fitBounds(as.numeric(bbox$xmin), as.numeric(bbox$ymin), as.numeric(bbox$xmax), as.numeric(bbox$ymax))
        if (comp_mode || val_type != "actual") {
          leafletProxy("comp_map_left") %>% fitBounds(as.numeric(bbox$xmin), as.numeric(bbox$ymin), as.numeric(bbox$xmax), as.numeric(bbox$ymax))
          leafletProxy("comp_map_right") %>% fitBounds(as.numeric(bbox$xmin), as.numeric(bbox$ymin), as.numeric(bbox$xmax), as.numeric(bbox$ymax))
        }
      }, error = function(e) NULL)
    }

    # NOTE: a hand-rolled "Global Performance Metrics" export table used to sit
    # here. It reported four of the statistics "Total Prediction Performance"
    # reports below, off the same rv$sf filter, under near-identical labels; it
    # went once that table moved onto perform_cv(). Do not re-add a second
    # dictionary here.
    # NOTE: intentionally no get_current_meta() re-read here - the export
    # labels below must use the meta captured at dispatch, not whatever the
    # sidebar points at when the run finishes.

    if(!is.null(rv$sf)) {
      df_perf <- rv$sf %>% st_drop_geometry() %>% filter(!is.na(v), !is.na(pv))
      if(nrow(df_perf) >= 3) {
        # Same metric dictionary as the on-screen Prediction Performance card
        # (server_sci_analysis.R) and as Model Performance: perform_cv() owns
        # every definition, so an export cannot report a different CCC or an
        # Inf RPD than the screen. moran = FALSE: no CV residual field here.
        perf_m <- perform_cv(data.frame(var1.observed = df_perf$v, var1.pred = df_perf$pv),
                             moran = FALSE)
        perf_total <- data.frame(
          Metric = c("R2 (Trad)", "R2 (Corr)", "RMSE", "MBE (ML pred - observed)", "CCC", "RPD"),
          Value = c(perf_m$nse, perf_m$r2, perf_m$rmse, -perf_m$me, perf_m$ccc, perf_m$rpd)
        )
        register_export_item("table_perf_uploaded_total", paste(meta$label, "- Total Prediction Performance"), "table", perf_total, meta$category)
      }
      
      v_all <- rv$sf$v[!is.na(rv$sf$v)]
      if(length(v_all) > 0) {
        s_a <- summary(v_all)
        stats_total <- data.frame(Metric = names(s_a), Value = as.character(round(as.numeric(s_a), 3)))
        register_export_item("table_stats_total", paste(meta$label, "- Total Descriptive Statistics (Actual)"), "table", stats_total, meta$category)
      }
      
      if(comp_mode || val_type != "actual") {
        pv_all <- rv$sf$pv[!is.na(rv$sf$pv)]
        if(length(pv_all) > 0) {
          s_p <- summary(pv_all)
          stats_total_p <- data.frame(Metric = names(s_p), Value = as.character(round(as.numeric(s_p), 3)))
          register_export_item("table_stats_pre_total", paste(meta$label, "- Total Descriptive Statistics (Predicted)"), "table", stats_total_p, meta$category)
        }
      }
      
      # error-only handler: a blanket condition= also intercepts messages
      # raised inside the reactive, aborting its evaluation mid-flight and
      # poisoning the cached value for every later consumer (the Jenks bug)
      params_k <- tryCatch(agro_params(), error = function(e) NULL)
      if(!is.null(params_k) && (comp_mode || val_type != "actual")) {
        df_k <- df_perf
        brks_k <- c(-Inf, params_k$rcl_mat[-1, 1], Inf)
        # right = FALSE matches the map classification (terra::classify with
        # right = FALSE): classes are [low, high)
        df_k$act_bin <- cut(df_k$v, breaks = brks_k, labels = params_k$labels, include.lowest = TRUE, right = FALSE)
        df_k$pred_bin <- cut(df_k$pv, breaks = brks_k, labels = params_k$labels, include.lowest = TRUE, right = FALSE)
        df_k <- df_k[!is.na(df_k$act_bin) & !is.na(df_k$pred_bin), ]
        if(nrow(df_k) >= 3) {
          kappa_total <- data.frame(
            Metric = c("Accuracy", "Kappa (Unweighted)", "Weighted Kappa (Linear)", "MCC"),
            Value = c(
              round(yardstick::accuracy_vec(df_k$act_bin, df_k$pred_bin), 4),
              round(yardstick::kap_vec(df_k$act_bin, df_k$pred_bin), 4),
              round(yardstick::kap_vec(df_k$act_bin, df_k$pred_bin, weighting = "linear"), 4),
              round(yardstick::mcc_vec(df_k$act_bin, df_k$pred_bin), 4)
            )
          )
          register_export_item("table_kappa_total", paste(meta$label, "- Total Classification Performance - Map in Agro or Binned styling to see the stats"), "table", kappa_total, meta$category)
        }
      }
    }

    if(isTruthy(input$color_style %in% c("agro", "bin")) && !is.null(rv$rast)) {
       area_total <- area_df_total_act()
       if(is.data.frame(area_total)) register_export_item("table_area_total", paste(meta$label, "- Total Area Coverage"), "table", area_total, meta$category)
    }

    for(l in locs) {
       register_locality_assets(l, meta, comp_mode, val_type, current_method)
    }
    
    rv$log <- paste0(rv$log, "\n\n--- Run #", rv$run_counter, " Complete ---",
      "\nConfig: ", rv$run_config_summary$method, " | ", rv$run_config_summary$variable,
      " | ", rv$run_config_summary$localities,
      "\n", rv$run_config_summary$method_params)

    shinyjs::hide("map_spinner")
    shinyjs::html("map_processing_title", "Map Generation Complete")
    update_premium_progress(100, "Click below to reveal the updated geostatistical surfaces.")
    shinyjs::show("reveal_maps_btn")
    
    shinyjs::enable("run")
    updateActionButton(session, "run", label = "Interpolated", icon = icon("check"))
    
    rv$model_running <- FALSE
    old_files <- list.files(path = session_progress_dir, pattern = paste0("^(progress|warn)_", session_id, "_.*_.*\\.txt$"), full.names = TRUE)
    if(length(old_files) > 0) tryCatch(file.remove(old_files), error = function(e) NULL)

      }, error = function(e) {
        # The interpolation itself completed; assembling/registering its results
        # in the main session did not. Say exactly that, and still release the
        # run UI so the app is usable. Whatever was assembled before the failure
        # stays in rv$ — the Reveal button is offered so partial surfaces can be
        # inspected, with the modal warning that they may be incomplete.
        rv$log <- paste0(rv$log, "\n\n[ERROR] Results assembly failed after a successful run: ", conditionMessage(e))
        shinyjs::hide("map_spinner")
        shinyjs::html("map_processing_title", "Results Assembly Failed")
        shinyjs::show("reveal_maps_btn")
        shinyjs::enable("run")
        updateActionButton(session, "run", label = "Run Interpolation", icon = character(0))
        shinyjs::runjs("$('#run i').remove();")
        rv$model_running <- FALSE
        stale <- list.files(path = session_progress_dir, pattern = paste0("^(progress|warn)_", session_id, "_.*_.*\\.txt$"), full.names = TRUE)
        if (length(stale) > 0) tryCatch(file.remove(stale), error = function(e2) NULL)
        showModal(modalDialog(
          title = tags$div(style = "color: #d9534f; font-weight: bold;", icon("exclamation-triangle"), "Results Assembly Failed"),
          tags$p("The parallel interpolation finished, but an error occurred while assembling the results (merging rasters, building tables, or registering exports) in the main session:"),
          tags$pre(style = "background-color: #f8d7da; color: #721c24; border: 1px solid #f5c6cb; padding: 15px; border-radius: 4px; overflow-x: auto; white-space: pre-wrap; font-family: monospace; font-size: 0.9em;", conditionMessage(e)),
          tags$p(style = "margin-top: 15px;", "The model outputs themselves are not in question. Any surfaces already assembled can be revealed, but maps, tables and the export registry may be incomplete for this run."),
          easyClose = TRUE,
          footer = modalButton("Dismiss")
        ))
      })
    }) %...!% (function(err) {
      if (this_token != rv$run_token) return()
      shinyjs::hide("map_spinner")
      shinyjs::hide("map_progress_bar_container")
      shinyjs::hide("cancel_model_btn")
      shinyjs::hide("reveal_maps_btn")
      
      shinyjs::enable("run")
      updateActionButton(session, "run", label = "Run Interpolation", icon = character(0))
      shinyjs::runjs("$('#run i').remove();")
      
      if (grepl("cancelled", tolower(err$message))) {
        shinyjs::html("map_processing_title", "Interpolation Cancelled")
        shinyjs::html("map_progress_text", HTML("Please configure parameters in the left panel and click <b>'Run Interpolation'</b> to generate geostatistical maps and review diagnostic results."))
      } else {
        shinyjs::html("map_processing_title", "Interpolation Failed")
        shinyjs::html("map_progress_text", "An error occurred during parallel modeling. Please check the error message and click 'Run Interpolation' to try again.")
        showModal(modalDialog(
          title = tags$div(style = "color: #d9534f; font-weight: bold;", icon("exclamation-triangle"), "Parallel Interpolation Failed"),
          tags$p("An error occurred while executing the parallel interpolation algorithms:"),
          tags$pre(style = "background-color: #f8d7da; color: #721c24; border: 1px solid #f5c6cb; padding: 15px; border-radius: 4px; overflow-x: auto; white-space: pre-wrap; font-family: monospace; font-size: 0.9em;", err$message),
          tags$p(style = "margin-top: 15px; font-weight: bold;", "Recommended Troubleshooting Steps:"),
          tags$ul(
            tags$li("Verify that your selected coordinate columns (X, Y) are strictly numeric and contain no missing values (NAs)."),
            tags$li("Check for highly collinear covariates if using Regression Kriging (RK) or RFK. Try removing redundant variables."),
            tags$li("Ensure you have at least 3-5 unique data points per locality region to allow variogram fitting.")
          ),
          easyClose = TRUE,
          footer = modalButton("Dismiss")
        ))
      }
      
      rv$model_running <- FALSE
      old_files <- list.files(path = session_progress_dir, pattern = paste0("^(progress|warn)_", session_id, "_.*_.*\\.txt$"), full.names = TRUE)
      if(length(old_files) > 0) tryCatch(file.remove(old_files), error = function(e) NULL)
    })
    
    }, error = function(e) {
      shinyjs::hide("map_spinner")
      shinyjs::hide("map_progress_bar_container")
      shinyjs::hide("cancel_model_btn")
      shinyjs::html("map_processing_title", "Interpolation Failed")
      shinyjs::html("map_progress_text", paste("Model preparation failed:", e$message))
      showNotification(paste("Model preparation failed:", e$message), type = "error")
      rv$model_running <- FALSE
      
      shinyjs::enable("run")
      updateActionButton(session, "run", label = "Run Interpolation", icon = character(0))
      shinyjs::runjs("$('#run i').remove();")
    })
    
    NULL
  })

  observe({
    req(rv$model_running)
    # Everything about the run in progress comes from the committed context
    # (rv$disp), never the live sidebar: changing the locality selection while
    # a run executes must not move the expected-model count under the bar.
    d <- rv$disp
    selected_locs <- if (!is.null(d)) d$localities else NULL
    n_locs_calc <- max(1L, length(selected_locs))
    poll_interval <- if (n_locs_calc < 5) 250 else if (n_locs_calc <= 20) 1000 else 2000
    invalidateLater(poll_interval)

    comp_mode <- !is.null(d) && (isTRUE(d$comp_mode) || !identical(d$value_type, "actual"))
    expected_models <- n_locs_calc * (if(comp_mode) 2 else 1)

    files <- list.files(path = session_progress_dir, pattern = paste0("^progress_", session_id, "_.*_.*\\.txt$"), full.names = TRUE)
    if(length(files) > 0) {
      vals <- vapply(files, function(f) {
        val <- tryCatch(as.numeric(readLines(f, warn = FALSE)), error = function(e) NA_real_)
        if(length(val) == 0 || is.na(val)) 0 else val
      }, numeric(1))

      # Fixed denominator (the run's expected model count): progress files
      # appear as engines start, so dividing by the files seen so far made the
      # average - and the bar - jump BACKWARDS every time a new locality
      # joined. sum(vals) is non-decreasing and the denominator is constant,
      # so the bar now only ever fills. If an engine dies before its file
      # exists the bar parks below the cap until the completion handler
      # resolves, which is the honest reading of overall progress.
      avg_pct <- sum(vals, na.rm = TRUE) / max(1L, expected_models)
      
      bar_width <- 50 + (avg_pct * 0.5)
      bar_width <- max(50, min(99, bar_width)) # Cap at 99% until complete handler resolves

      update_premium_progress(bar_width)

      # Header chip: only assign on change so the chip does not re-render at
      # every poll tick.
      chip_pct <- max(0, min(99, round(avg_pct)))
      if (!identical(rv$run_pct, chip_pct)) rv$run_pct <- chip_pct
      
      progress_msgs <- c()
      for (f in files) {
        f_base <- basename(f)
        if (grepl("_act\\.txt$", f_base)) {
          loc_name <- gsub(paste0("^progress_", session_id, "_(.*)_act\\.txt$"), "\\1", f_base)
          type_suffix <- " (Actual)"
        } else if (grepl("_pre\\.txt$", f_base)) {
          loc_name <- gsub(paste0("^progress_", session_id, "_(.*)_pre\\.txt$"), "\\1", f_base)
          type_suffix <- " (Predicted)"
        } else {
          loc_name <- gsub(paste0("^progress_", session_id, "_(.*)_(act|pre)\\.txt$"), "\\1", f_base)
          type_suffix <- ""
        }
        
        loc_display <- gsub("_", " ", loc_name)
        val <- tryCatch(as.numeric(readLines(f, warn = FALSE)), error = function(e) NA_real_)
        if(length(val) > 0 && !is.na(val)) {
          progress_msgs <- c(progress_msgs, paste0("<b>", loc_display, type_suffix, "</b>: ", val, "%"))
        }
      }
      
      warn_files <- list.files(path = session_progress_dir, pattern = paste0("^warn_", session_id, "_.*_.*\\.txt$"), full.names = TRUE)
      warn_msgs <- c()
      if(length(warn_files) > 0) {
        for (wf in warn_files) {
          wf_base <- basename(wf)
          if (grepl("_act\\.txt$", wf_base)) {
            loc_name <- gsub(paste0("^warn_", session_id, "_(.*)_act\\.txt$"), "\\1", wf_base)
            type_suffix <- " (Actual)"
          } else if (grepl("_pre\\.txt$", wf_base)) {
            loc_name <- gsub(paste0("^warn_", session_id, "_(.*)_pre\\.txt$"), "\\1", wf_base)
            type_suffix <- " (Predicted)"
          } else {
            loc_name <- gsub(paste0("^warn_", session_id, "_(.*)_(act|pre)\\.txt$"), "\\1", wf_base)
            type_suffix <- ""
          }
          loc_display <- gsub("_", " ", loc_name)
          msg <- tryCatch(readLines(wf, warn = FALSE), error = function(e) "")
          if (length(msg) > 0 && msg != "") {
            warn_msgs <- c(warn_msgs, paste0("⚠️ <b>", loc_display, type_suffix, "</b>: ", msg))
          }
        }
      }
      
      warn_block <- ""
      if (length(warn_msgs) > 0) {
        warn_block <- paste0("<br/><span style='font-size: 0.85em; color: #e74c3c; margin-top: 5px; display: inline-block;'>", paste(warn_msgs, collapse = "<br/>"), "</span>")
      }
      
      if (length(progress_msgs) > 0) {
        shinyjs::html("map_progress_text", paste0("Executing Parallel Interpolation Algorithms...<br/><span style='font-size: 0.85em; opacity: 0.8;'>", paste(progress_msgs, collapse = " &nbsp;|&nbsp; "), "</span>", warn_block))
      }
    }
  })

  observeEvent(input$cancel_model_btn, {
    # Flags the run actually in flight; the next run gets its own file, so this
    # cancellation cannot be revoked by starting another run.
    cancel_file <- file.path(session_progress_dir,
                             paste0("cancel_flag_", rv$run_counter, ".txt"))
    file.create(cancel_file)
    rv$model_running <- FALSE
    rv$run_token <- rv$run_token + 1L
    
    shinyjs::hide("map_spinner")
    shinyjs::hide("map_progress_bar_container")
    shinyjs::hide("cancel_model_btn")
    shinyjs::hide("reveal_maps_btn")
    
    shinyjs::html("map_processing_title", "Interpolation Cancelled")
    shinyjs::html("map_progress_text", HTML("Please configure parameters in the left panel and click <b>'Run Interpolation'</b> to generate geostatistical maps and review diagnostic results."))
    
    showNotification("Model generation cancelled by user.", type = "warning")
    
    old_files <- list.files(path = session_progress_dir, pattern = paste0("^(progress|warn)_", session_id, "_.*_.*\\.txt$"), full.names = TRUE)
    if(length(old_files) > 0) tryCatch(file.remove(old_files), error = function(e) NULL)
    
    shinyjs::enable("run")
    updateActionButton(session, "run", label = "Run Interpolation", icon = character(0))
    shinyjs::runjs("$('#run i').remove();")
  })

  observeEvent(input$reveal_maps_btn, {
    shinyjs::hide("map_processing_overlay")
    # The displayed widget may still be the view-less placeholder rendered at
    # dispatch time (the raster re-render races with this button appearing);
    # an explicit fit + resize guarantees tiles and surfaces show immediately.
    fit_maps_to_data()
    showNotification("Maps and scientific analysis metrics are now available.", type = "message")
    
    updateActionButton(session, "run", label = "Run Interpolation", icon = character(0))
    shinyjs::runjs("$('#run i').remove();")
  })

  observeEvent(input$resid_info_btn, {
    showModal(modalDialog(
      title = "Residual Mapping & Diagnostics",
      size = "l",
      easyClose = TRUE,
      tags$div(
        h4("Mathematical Formula"),
        p(HTML("<b>Residual = Observed value (v) - ML Predicted value (pv)</b>")),
        p("A residual is the deviation of your machine learning model from the actual measured value at a given location. Both columns come from your uploaded dataset: the observed measurements and the predictions of the external ML model you supplied (e.g. Actual Nitrogen - ML Predicted Nitrogen). Residuals therefore diagnose that ML model's error, not the error of the interpolation performed in this dashboard."),
        hr(),
        h4("Available Residual Types"),
        tags$ul(
          tags$li(tags$b("Interpolated Delta (Surface Diff):"), " Calculated by subtracting the entire Predicted surface from the Actual surface [interpolate(Actual) - interpolate(Predicted)]. This shows the net difference between the two mapped geostatistical surfaces."),
          tags$li(tags$b("Point Errors:"), " The discrete error at each individual sample point location [Observed - Predicted], displayed as coloured markers at the exact sampling positions (right map of the Residuals view, and the 'Point Error Map' in the Export Panel)."),
          tags$li(tags$b("Interpolated Point Errors (Model Error):"), " The same local errors interpolated (IDW) into a continuous surface, available in the Export Panel as the 'Interpolated Point Errors Map'. This specifically maps the spatial structure of the model's inability to capture local variation.")
        ),
        hr(),
        h4("Interpretation Guide"),
        tags$ul(
          tags$li(tags$b("Positive Residual (Blue):"), " Under-prediction. The actual measured value is HIGHER than the predicted model value."),
          tags$li(tags$b("Negative Residual (Red):"), " Over-prediction. The actual measured value is LOWER than the predicted model value."),
          tags$li(tags$b("Zero (White):"), " Perfect prediction at that location.")
        ),
        hr(),
        h5("References"),
        tags$ul(
          tags$li("Hengl, T. (2009). A Practical Guide to Geostatistical Mapping."),
          tags$li("Isaaks, E. H., & Srivastava, R. M. (1989). Applied Geostatistics.")
        )
      )
    ))
  })

