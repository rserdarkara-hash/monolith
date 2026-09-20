# server_execution.R (sourced with local = TRUE inside server) - run estimates,
# archive/VIF gates and the future_promise interpolation pipeline. CRITICAL:
# run_params is built from reactives BEFORE the future_promise block; no rv$*/
# input$* may be referenced inside the future, and the nested
# parallelly::makeClusterPSOCK topology must be preserved as-is.

  # ── Run warnings ──────────────────────────────────────────────────────────
  # Workers report per-locality warnings as `warn_` files. They used to be read
  # only by the live progress poller and deleted by the completion handler, so
  # a message explaining why five metric cells are blank was on screen for a
  # few seconds and then gone - absent from rv$log and therefore from the
  # exported run log too. These two closures are the one path: everything the
  # poller shows is what the completion handler persists.
  read_run_warnings <- function() {
    files <- list.files(path = session_progress_dir,
                        pattern = paste0("^warn_", session_id, "_.*_.*\\.txt$"),
                        full.names = TRUE)
    out <- lapply(files, function(wf) {
      msg <- tryCatch(readLines(wf, warn = FALSE), error = function(e) character(0))
      msg <- paste(msg[nzchar(msg)], collapse = " ")
      if (!nzchar(msg)) return(NULL)
      c(status_file_parts(wf, session_id, kind = "warn"), list(message = msg))
    })
    Filter(Negate(is.null), out)
  }

  # Append this run's warnings to the run log, then clear the run's status
  # files. Called from every completion path, including the two failure
  # handlers - a run that failed is exactly when its warnings matter most.
  persist_run_warnings <- function() {
    warns <- tryCatch(read_run_warnings(), error = function(e) list())
    if (length(warns) > 0) {
      # A warning the worker also wrote to its own log (the grid-coarsening and
      # constant-target notes use both channels) is already in rv$log; adding
      # it again would print the same sentence twice.
      # Matched per LINE on message AND locality, so the same sentence logged
      # for one locality does not swallow another locality's warning. The file
      # name carries the sanitised locality, hence the underscore/space twin.
      log_lines <- strsplit(rv$log %||% "", "\n", fixed = TRUE)[[1]]
      already <- function(w) {
        hit <- grepl(w$message, log_lines, fixed = TRUE)
        any(hit & (grepl(w$locality, log_lines, fixed = TRUE) |
                   grepl(gsub("_", " ", w$locality), log_lines, fixed = TRUE)))
      }
      fresh <- Filter(Negate(already), warns)
      if (length(fresh) > 0) {
        lines <- vapply(fresh, function(w) paste0("\n[Warning] ", w$label, ": ", w$message), character(1))
        rv$log <- paste0(rv$log, paste(lines, collapse = ""))
      }
      rv$run_warnings <- vapply(warns, function(w) paste0(w$label, ": ", w$message), character(1))
    } else {
      rv$run_warnings <- character(0)
    }
    stale <- list.files(path = session_progress_dir,
                        pattern = paste0("^(progress|warn)_", session_id, "_.*_.*\\.txt$"),
                        full.names = TRUE)
    if (length(stale) > 0) tryCatch(file.remove(stale), error = function(e) NULL)
  }

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
    
    eff_subset <- effective_subset(input$value_type, input$subset, names(rv$user_data))
    loc_sample_counts <- numeric(n_locs)
    if (length(selected_locs) > 0 && !is.null(rv$user_data) && !is.null(loc_col) && loc_col %in% colnames(rv$user_data)) {
      for (idx in seq_along(selected_locs)) {
        l <- selected_locs[idx]
        n_samples <- nrow(run_locality_rows(rv$user_data, loc_col, l, eff_subset))
        loc_sample_counts[idx] <- if (is.null(n_samples) || is.na(n_samples) || n_samples == 0) 50 else n_samples
      }
    } else {
      loc_sample_counts <- rep(50, n_locs)
    }
    
    est_res <- estimate_run_duration(loc_sample_counts, input$method, comp_mode, cores)
    
    return(list(
      meta = meta,
      n_locs = n_locs,
      n_points = sum(loc_sample_counts),
      estimate_text = est_res$estimate_text,
      # The bare duration, for the one-line statement under the Run button;
      # estimate_text is the multi-paragraph version the confirmation dialog uses.
      est_time_str = est_res$est_time_str,
      is_long_run = est_res$is_long_run
    ))
  }

  # What the next run will cost, stated before it is started: duration, how many
  # locality models, how many points, and the method it would use. Recomputed
  # from the same estimator the confirmation dialog reads, so the two never
  # disagree; silent until a dataset has been mapped (calculate_run_estimates
  # req()s the run metadata).
  output$run_estimate_line <- renderUI({
    est <- tryCatch(calculate_run_estimates(), error = function(e) NULL)
    if (is.null(est)) return(NULL)
    has_data <- !is.null(rv$user_data)
    tags$div(
      class = "mn-dock-est",
      icon("clock"),
      tags$span(
        "Estimated ", tags$b(est$est_time_str),
        " · ", tags$b(est$n_locs), if (est$n_locs == 1) " locality" else " localities",
        if (has_data) tagList(" · ", tags$b(format(est$n_points, big.mark = ",")), " points"),
        " · ", tags$b(get_method_label(input$method))
      )
    )
  })

  # Archived entries hold a run's rasters, CV objects and fitted models, so an
  # unbounded archive grows RAM run after run; keep only the most recent few.
  # An entry is snapshot_display_state() (server_setup.R). Measured on the
  # 7-locality golden set (OK, comparison mode, Auto grid): the registry's
  # merged Actual and Predicted surfaces dominate an entry, beside 78 MB of
  # per-locality rasters and CV state, which is why the cap is 3, not 5. The
  # uncertainty maps add nothing to that: they are registered as derivations
  # of those surfaces (export_item_obj, global_utils.R), not as copies.
  MAX_RUN_HISTORY <- 3L
  push_run_history <- function(entry, base = NULL) {
    # A cancelled run produced nothing, so it is never archived as a result.
    if (identical(entry$config$status, "cancelled")) {
      if (!is.null(base)) rv$run_history <- base
      return(invisible(FALSE))
    }
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
      current <- snapshot_display_state()
      if (!is.null(current$config) && length(current$registry) > 0) {
        push_run_history(current)
      }
    }
    
    if (is_long_run) {
      showModal(modalDialog(
        title = "Ready to Run Interpolation",
        tags$p("You are about to start the spatial interpolation pipeline with the following parameters:"),
        div(style = "background-color: var(--mn-surface-2); padding: 12px; border-radius: 5px; margin: 10px 0;",
          tags$strong("Method: "), tags$span(input$method), tags$br(),
          tags$strong("Localities: "), tags$span(n_locs), tags$br(),
          tags$strong("Variables: "), tags$span(meta$label)
        ),
        div(class = "mn-notice",
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
      title = tags$div(style = "color: var(--mn-danger); font-weight: 600;", icon("exclamation-triangle"), paste(missing, "Not Set")),
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
      title = tags$div(style = "color: var(--mn-danger); font-weight: 600;",
                       icon("exclamation-triangle"), suit$title),
      tags$p(suit$msg),
      tags$p(suit$detail),
      # Name the CRS that IS right rather than describing it, and name it here
      # too: a user who reaches the run gate without having read the Data Setup
      # advisory must not be sent back to work the answer out for themselves.
      local({
        rec <- crs_recommend_target(pos$lon, pos$lat)
        if (is.null(rec)) {
          tags$p("Choose the UTM zone or national grid the study area belongs to, or continue and accept that the exported surface and the ruler's projected measurements carry that error.")
        } else {
          tags$p("Set the Target Mapping CRS to ", tags$b(sprintf("%s (%s)", rec$crs, rec$label)),
                 " on the Data Setup tab, or continue and accept that the exported surface and the ruler's projected measurements carry that error.")
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

  # Which runs need auxiliary variables. RK/RFK/CK model with them; OK does not
  # use them at all, but under the Comparable CV population they select the
  # samples OK is trained and scored on, so a run with none is a configuration
  # the user did not mean.
  run_uses_covariates <- function() {
    input$method %in% c("RK", "RFK", "CK") ||
      (identical(input$method, "OK") && identical(input$cv_population, "comparable"))
  }
  covariates_required_msg <- function() {
    if (identical(input$method, "OK"))
      "The Comparable cross-validation population is defined by the selected covariates; select at least one, or switch the population to Native."
    else "Please select at least one auxiliary variable for RK/RFK/CK model generation."
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
       locs <- resolve_selected_localities(input$locality, df_vif, rv$mapping$loc)
       eff_subset <- effective_subset(input$value_type, input$subset, names(df_vif))
       df_vif <- run_locality_rows(df_vif, rv$mapping$loc, locs, eff_subset)
       df_aux <- df_vif[, input$aux_vars, drop = FALSE]
       vif_res <- check_vif(df_aux, threshold = 10)

       if (length(vif_res$dropped) > 0) {
          showModal(modalDialog(
            title = tags$div(style = "color: var(--mn-danger); font-weight: 600;", icon("exclamation-triangle"), "High Multicollinearity Detected"),
            tags$p("High correlation / multicollinearity detected among the selected variables within the selected localities. This may destabilize the spatial estimation model."),
            tags$p(tags$b("Variables recommended to be dropped:"), paste(vif_res$dropped, collapse=", ")),
            # The answer is a rule, not a one-off edit to a covariate list: the
            # map applies it to every sample of a locality and each CV fold
            # applies it again to its own training samples, so a fold can keep
            # a different set than the map.
            tags$p(style = "font-size: 0.9em; color: var(--mn-text-2);",
                   "Your answer sets the rule. The map applies it to all samples of each locality; cross-validation applies it again inside every fold, using only that fold's training samples, so a fold can keep a different set. Constant covariates are always dropped."),
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

    if (run_uses_covariates() && (is.null(input$aux_vars) || length(input$aux_vars) == 0)) {
      showNotification(covariates_required_msg(), type = "error")
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
          div(style = "background-color: var(--mn-surface-2); padding: 10px; border-radius: 5px; margin: 10px 0;",
            tags$strong(paste0("Run #", rv$run_config_summary$run_id, ": ",
              rv$run_config_summary$variable, " (", rv$run_config_summary$method, ")")),
            tags$br(),
            tags$small(paste0(rv$run_config_summary$localities, " | ",
              format(rv$run_config_summary$timestamp, "%H:%M:%S")))
          ),
          div(class = "mn-notice",
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
        title = tags$div(style = "color: var(--mn-danger); font-weight: 600;", icon("exclamation-triangle"), "Coordinate Mapping Error"),
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
        title = tags$div(style = "color: var(--mn-danger); font-weight: 600;", icon("exclamation-triangle"), "Invalid Coordinate Data"),
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
    if (run_uses_covariates() && (is.null(aux_vars) || length(aux_vars) == 0)) {
      showNotification(covariates_required_msg(), type = "error")
      return()
    }
    if (run_uses_covariates() && length(aux_vars) > 0) {
      missing_vars <- setdiff(aux_vars, colnames(rv$user_data))
      if (length(missing_vars) > 0) {
        showModal(modalDialog(
          title = tags$div(style = "color: var(--mn-danger); font-weight: 600;", icon("exclamation-triangle"), "Missing Covariates"),
          tags$p("The following selected covariates do not exist in the dataset:"),
          tags$pre(style = "background-color: var(--mn-surface-2); color: var(--mn-text); border: 1px solid var(--mn-line); border-left: 2px solid var(--mn-danger); padding: 10px; border-radius: 4px;", paste(missing_vars, collapse = ", ")),
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
          title = tags$div(style = "color: var(--mn-danger); font-weight: 600;", icon("exclamation-triangle"), "Non-Numeric Covariates"),
          tags$p("The following selected covariates do not contain sufficient valid numeric values (minimum 3 required):"),
          tags$pre(style = "background-color: var(--mn-surface-2); color: var(--mn-text); border: 1px solid var(--mn-line); border-left: 2px solid var(--mn-danger); padding: 10px; border-radius: 4px;", paste(non_numeric_vars, collapse = ", ")),
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

    # Engines use metre-based Input Data CRS coordinates, or local UTM for
    # geographic/non-metre input. The Target CRS gate protects the resampled
    # surface, the resolution suggestion and the projected ruler.
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
    shinyjs::show("map_progress_bar_container")
    shinyjs::show("map_run_steps")
    shinyjs::show("cancel_model_btn")
    shinyjs::hide("reveal_maps_btn")
    shinyjs::html("map_processing_title",
                  paste("Interpolating", get_method_label(input$method), "surfaces"))
    # Only the kriging family fits a variogram; IDW and TPS fit their own model.
    shinyjs::html("map_step_2",
                  if (input$method %in% c("OK", "RK", "RFK", "CK")) "Fit variogram" else "Fit model")
    update_premium_progress(5, "Preparing the run. The interface stays responsive; you can keep working in other tabs.", step = 1)

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
      "OK"  = paste0("Ordinary Kriging | Variogram: ",
                     if (identical(input$vgm_mode, "manual")) "Manual (applied models only)" else "Auto-Fit"),
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
    eff_subset <- effective_subset(input$value_type, input$subset, names(rv$user_data))
    tuning_keys <- c(act = tuning_key(meta$actual, eff_subset),
                     pre = tuning_key(if (input$value_type == "pred_ss") meta$pred_ss else meta$pred, eff_subset))
    rv$run_config_summary <- list(
      run_id = rv$run_counter,
      timestamp = Sys.time(),
      app_version = app_version,
      variable = paste0(meta$label, " [", meta$actual, "]"),
      method = input$method,
      localities = paste(locs, collapse = ", "),
      subset = eff_subset,
      value_type = input$value_type,
      # The CRS the coordinates were read in, and the one the finished surface,
      # its exports and the Map Viewer's projected measures are produced in.
      input_crs = rv$mapping$crs,
      target_crs = input$crs_selection,
      boundary_type = input$boundary_type,
      buffer_mode = input$buff_mode,
      buffer_dist = input$buff_dist,
      # In the Auto modes the cell size follows each locality's boundary area
      # and is not known until the run has built them, so the sidebar slider
      # (which holds the global recommendation there, not a size any grid
      # uses) must not be recorded as the resolution. The completion handler
      # below replaces this with the sizes the run actually gridded at.
      resolution = if (identical(input$res_mode, "fixed")) input$grid_res else "set at run",
      res_mode = input$res_mode,
      comp_mode = input$comp_mode,
      # Whether the Predicted surface got its own model (variogram, IDW power,
      # TPS lambda) or reused the measured values' one. Meaningful only when
      # the run maps a Predicted surface; RK/RFK/CK always fit their own.
      sep_fit = if (input$method %in% c("OK", "IDW", "TPS") &&
                    (isTRUE(input$comp_mode) || !identical(input$value_type, "actual"))) isTRUE(input$sep_fit) else NA,
      cv_strategy = input$cv_strategy %||% "auto",
      cv_repeats = cv_repeats_val,
      # What the reported metrics were measured on, and what the folds
      # re-estimated. Two archived runs that differ only here used to look
      # identical in this record.
      cv_population = switch(input$method,
        "OK" = if (identical(input$cv_population, "comparable")) "Comparable (common rows)" else "Native (every measured sample)",
        "RK" = , "RFK" = , "CK" = "Common rows (target and every covariate)",
        NA_character_),
      cv_refit = if (input$method %in% c("OK", "RK", "RFK", "CK")) "per fold" else NA_character_,
      cv_covariate_screen = if (input$method %in% c("RK", "RFK", "CK")) "per fold" else NA_character_,
      # Manual variogram models are consumed by Ordinary Kriging only.
      vgm_mode = if (identical(input$method, "OK")) (input$vgm_mode %||% "auto") else NA_character_,
      # What the user selected. What each locality's model used, and what the
      # collinearity screen removed, is only known once the workers return:
      # covariates_retained / covariates_dropped are filled at completion.
      covariates_selected = if (run_uses_covariates()) paste(input$aux_vars, collapse = ", ") else NA,
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
      subset = eff_subset,
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
    rv$log <- paste0("[Run #", rv$run_counter, "] Starting spatial interpolation using method: ", input$method, "...")
    rv$run_warnings <- character(0)
    rv$model_summaries <- list(); rv$rf_models <- list(); rv$gstat_objs <- list()
    rv$cv_metrics_act <- list(); rv$cv_metrics_pre <- list() # Reset CV metrics
    rv$cv_data_act <- list(); rv$cv_data_pre <- list()
    rv$cv_repeats_act <- NULL; rv$cv_repeats_pre <- NULL
    rv$cv_info_act <- list(); rv$cv_info_pre <- list()
    rv$cv_strategy_sel <- input$cv_strategy %||% "auto"
    rv$cv_repeats_sel <- cv_repeats_val
    
    update_premium_progress(15, "Validating and cleaning the spatial input data.", step = 1)
    
    pred_col <- if(input$value_type == "pred_ss") meta$pred_ss else meta$pred
    aux_vars <- input$aux_vars
    
    update_premium_progress(25, "Preparing the prediction grid and neighbourhood search.", step = 1)
    
    current_method <- input$method
    current_crs <- rv$mapping$crs
    current_loc_col <- rv$mapping$loc
    current_x_col <- rv$mapping$x
    current_y_col <- rv$mapping$y
    val_type <- input$value_type
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
    sep_fit <- isTRUE(input$sep_fit)
    vgm_mode <- input$vgm_mode
    tuning_revision <- rv$tuning_revision %||% 0L
    idw_p_val <- input$idw_p
    idw_nmax_val <- input$idw_nmax
    tps_lambda_val <- input$tps_lambda
    
    update_premium_progress(35, "Organising the per-locality data chunks.", step = 1)
    
    # Row identity: the row number in the uploaded table, stamped BEFORE any
    # filter so a CV population can be named by the rows it holds whatever
    # filtered it. The worker strips it again before returning rv$sf.
    user_rows <- rv$user_data
    user_rows[[CV_ROW_ID_COL]] <- seq_len(nrow(user_rows))
    cv_population_sel <- if (identical(current_method, "OK")) (input$cv_population %||% "native") else "native"

    df_list <- lapply(locs, function(l) {
      sub_df <- run_locality_rows(user_rows, current_loc_col, l, eff_subset)
      
      pts_data <- sub_df
      pts_data$x <- sub_df[[current_x_col]]
      pts_data$y <- sub_df[[current_y_col]]
      pts_data$v <- sub_df[[actual_col]]
      # Only a run that maps a prediction side carries the uploaded prediction
      # column. pred_col is resolved from the variable's _cve/_ss column
      # whatever the view is, so an Actual-only run used to fill pv anyway and
      # then registered ML-prediction products (point-error surface, residual
      # map, uploaded-prediction card) for a run that predicted nothing.
      run_uses_pred <- isTRUE(comp_mode) || !identical(val_type, "actual")
      pts_data$pv <- if (run_uses_pred && !is.null(pred_col) &&
                         pred_col %in% colnames(sub_df)) sub_df[[pred_col]] else NA
      
      pre_fit_act <- resolve_stored_vgm(rv$v_fit_list[[paste0(l, "_act")]], vgm_mode, tuning_keys[["act"]])
      idw_p_act <- get_regional_param("IDW", l, "act", default = idw_p_val %||% 2, key = tuning_keys[["act"]])
      tps_lambda_act <- get_regional_param("TPS", l, "act", default = tps_lambda_val, key = tuning_keys[["act"]])
      # "Fit Actual/Predicted separately" unticked: the Predicted surface reuses
      # the measured values' model - their variogram, IDW power and TPS lambda.
      # (A TPS lambda on Auto is shared in the worker: the Predicted surface
      # takes the one GCV selects for the measured values.)
      m_params <- list(
        idw_p_act = idw_p_act,
        idw_p_pre = if (sep_fit) get_regional_param("IDW", l, "pre", default = idw_p_val %||% 2, key = tuning_keys[["pre"]]) else idw_p_act,
        idw_nmax = idw_nmax_val %||% 12,
        tps_lambda_act = tps_lambda_act,
        tps_lambda_pre = if (sep_fit) get_regional_param("TPS", l, "pre", default = tps_lambda_val, key = tuning_keys[["pre"]]) else tps_lambda_act,
        pre_fit_act = pre_fit_act,
        pre_fit_pre = if (sep_fit) resolve_stored_vgm(rv$v_fit_list[[paste0(l, "_pre")]], vgm_mode, tuning_keys[["pre"]]) else pre_fit_act,
        sep_fit = sep_fit,
        cv_strategy = input$cv_strategy %||% "auto",
        cv_population = cv_population_sel,
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

    # A stored IDW power / TPS lambda tuned for another variable or subset is
    # not used; say which value replaced it. Unseparated, the Predicted surface
    # reads the Actual slot, so only that slot is consulted.
    run_targets <- if (comp_mode || val_type != "actual") c("act", "pre") else "act"
    param_targets <- if (sep_fit) run_targets else "act"
    if (current_method %in% c("IDW", "TPS")) {
      store <- if (current_method == "IDW") rv$idw_factors else rv$tps_lambdas
      field <- if (current_method == "IDW") "idw_p_" else "tps_lambda_"
      for (item in df_list) for (target in param_targets) {
        entry <- store[[item$l]][[target]]
        if (!is.null(entry) && !identical(entry$key, tuning_keys[[target]])) {
          rv$log <- paste0(rv$log, "\n[Tuning] ", item$l, " (", target, "): the stored ", current_method,
            " value was tuned for ", entry$key %||% "another key", "; this run uses ", tuning_keys[[target]],
            ", so the sidebar value ", format_param_val(current_method, item$m_params[[paste0(field, target)]] %||% NA),
            " is used. Re-run the optimizer or apply a manual value for this variable.")
        }
      }
    }

    # GCV curves shown for this run: only those tuned for its keys, and for the
    # slots its surfaces consumed.
    gcv_names <- intersect(names(rv$tps_gcv_data), as.vector(outer(locs, param_targets, paste, sep = "_")))
    rv$disp$tps_gcv_data <- rv$tps_gcv_data[Filter(function(nm) {
      vgm_key_matches(rv$tps_gcv_data[[nm]], tuning_keys[[if (endsWith(nm, "_act")) "act" else "pre"]])
    }, gcv_names)]
    shp_shared <- tryCatch(shared_boundary_features(shp_bound, df_list, current_crs),
      error = function(e) {
        showNotification("Uploaded boundary sharing could not be checked; unnamed features will use the selected sidebar boundary.",
                         type = "warning", duration = 15)
        seq_len(nrow(shp_bound))
      })

    # Manual variogram fits reach every engine in m_params, but only the OK
    # branch consumes them: RK/RFK refit the RESIDUAL variogram once the trend
    # is removed and CK fits an LMC (both correct - a value-scale model must not
    # be imposed on residuals). Say so instead of ignoring the user's tuning in
    # silence. Gated on Manual mode: stored fits also come from OPTIMIZE ALL
    # VARIOGRAMS and from previous runs, where nothing was hand-tuned.
    if (identical(vgm_mode, "manual") && current_method %in% c("RK", "RFK", "CK")) {
      if (any(vapply(df_list, function(item) !is.null(item$m_params$pre_fit_act) ||
                       !is.null(item$m_params$pre_fit_pre), logical(1)))) {
        manual_note <- paste0("Manual variogram fits are consumed by Ordinary Kriging only. ",
                              current_method, " fits its own variogram model (residual variogram for RK/RFK, linear model of coregionalization for CK), so the tuned fit will not be used in this run.")
        showNotification(manual_note, type = "warning", duration = 12)
        rv$log <- paste0(rv$log, "\n[Variogram] ", manual_note)
      }
    }

    if (identical(vgm_mode, "manual") && current_method == "OK") {
      missing_models <- character(0)
      for (item in df_list) {
        for (target in if (comp_mode || val_type != "actual") c("act", "pre") else "act") {
          if (!is.null(item$m_params[[paste0("pre_fit_", target)]])) next
          stored_target <- if (target == "pre" && !sep_fit) "act" else target
          key <- tuning_keys[[stored_target]]
          stored <- rv$v_fit_list[[paste0(item$l, "_", stored_target)]]
          other <- if (identical(attr(stored, "monolith_source"), "manual") &&
                       !vgm_key_matches(stored, key)) paste0("; applied model belongs to ", attr(stored, "monolith_key")) else ""
          missing_models <- c(missing_models, paste0(item$l, " (", target, ": ", key, other, ")"))
        }
      }
      if (length(missing_models)) rv$log <- paste0(rv$log, "\n[Variogram] No applied model for ",
        paste(missing_models, collapse = ", "), ". These surfaces fit their own variogram; an unseparated Predicted surface shares the Actual fit.")
    }

    update_premium_progress(50, "Fitting and predicting per locality in parallel. The interface stays responsive; you can keep working in other tabs.", step = 2)

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
    # drag their source environments to every worker). The dispatch below
    # therefore PINS `globals =` to the four plain-data objects the body
    # reads. Automatic discovery would otherwise walk the whole 106-object
    # helper call graph recursively on every single run - 6 s of frozen main
    # session, measured, uncached - only to ship function values the worker's
    # own source() defines anyway. `packages =` replaces the attachment that
    # walk used to infer: the helper graph calls sf, gstat and dplyr
    # unqualified (the same set the nested furrr_options below declares).
    run_params <- list(
      main_wd = main_wd,
      current_method = current_method, current_crs = current_crs, aux_vars = aux_vars,
      shp_bound = shp_bound, shp_shared = shp_shared, b_type = b_type, buff_mode = buff_mode, b_dist = b_dist,
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
      # This pool worker is reused across features and may carry a plan its
      # previous task failed to tear down; nbrOfWorkers() would then report a
      # dead cluster's size and the guard below would skip building a live one.
      future::plan(future::sequential)
      # Auto (Global) needs every locality's boundary before any locality runs:
      # the shared cell size is the Auto resolution of the largest one. Built
      # here, in the worker, so the interface stays responsive.
      if (identical(run_params$res_mode, "global")) {
        run_params$shared_res <- shared_auto_resolution(df_list, run_params)
      }

      nested_cl <- NULL
      old_mc_cores <- getOption("mc.cores")
      tryCatch({
        # No `nbrOfWorkers() == 1L` clause: the plan reset above makes it
        # true by construction, and a tautology in a guard reads as if it
        # still protected something.
        if (nested_workers >= 2L) {
          # PSOCK workers report mc.cores = 1; the main session allocated
          # nested_workers cores to this batch, so tell parallelly before
          # spawning or its worker-count guard misfires. Owning the cluster
          # explicitly (instead of plan(multisession)) guarantees a clean
          # teardown in the finally block below.
          options(mc.cores = nested_workers)
          nested_cl <- parallelly::makeClusterPSOCK(nested_workers)
          future::plan(future::cluster, workers = nested_cl)
        }
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
        # future_promise tasks expect. Stop the cluster FIRST and swallow both
        # errors: switching the plan away from an unhealthy cluster can throw,
        # which would otherwise leave this worker on a dead cluster.
        options(mc.cores = old_mc_cores)
        if (!is.null(nested_cl)) {
          tryCatch(parallel::stopCluster(nested_cl), error = function(e) NULL)
        }
        tryCatch(future::plan(future::sequential), error = function(e) NULL)
      })
    }, globals = list(main_wd = main_wd, run_params = run_params,
                      df_list = df_list, nested_workers = nested_workers),
       packages = c("sf", "gstat", "dplyr"),
       seed = 12345) %...>% (function(res_all) {
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
      run_fits <- list(); run_emps <- list()

      grid_res_used <- list()
      for(res in res_all) {
          l <- res$l
          if (is.numeric(res$actual_res) && length(res$actual_res) == 1) grid_res_used[[l]] <- res$actual_res
          if (current_method == "TPS") {
            for (tgt in c("act", "pre")) {
              rv$disp$regional_params[[l]][[paste0("tps_fit_", tgt)]] <-
                res[[paste0("tps_fit_", tgt)]] %||% list(lambda = NA_real_, eff_df = NA_real_)
            }
          }
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
          if(!is.null(res$bound)) b_list[[l]] <- res$bound
          if(!is.null(res$pts)) sf_list[[length(sf_list)+1]] <- res$pts
          
          if(!is.null(res$v_emp_act)) run_emps[[paste0(l, "_act")]] <- res$v_emp_act
          if(!is.null(res$v_fit_act)) run_fits[[paste0(l, "_act")]] <- res$v_fit_act
          if(!is.null(res$cv_act)) rv$cv_metrics_act[[l]] <- res$cv_act
          if(!is.null(res$cv_obj_act)) rv$cv_data_act[[l]] <- res$cv_obj_act
          rv$cv_info_act[[l]] <- stamp_cv_population(res$cv_info_act, tuning_keys[["act"]])
          if(cv_repeats_val > 1) {
            reps_act[[l]] <- res$cv_reps_act %||% Filter(Negate(is.null), list(cv_repeat_frame(res$cv_obj_act)))
          }
          if(!is.null(res$summ_act)) rv$model_summaries[[paste0(l, "_act")]] <- res$summ_act
          if(!is.null(res$rf_act)) rv$rf_models[[paste0(l, "_act")]] <- res$rf_act
          if(!is.null(res$gstat_act)) rv$gstat_objs[[paste0(l, "_act")]] <- res$gstat_act
          
          if(!is.null(res$v_emp_pre)) run_emps[[paste0(l, "_pre")]] <- res$v_emp_pre
          if(!is.null(res$v_fit_pre)) run_fits[[paste0(l, "_pre")]] <- res$v_fit_pre
          if (current_method == "OK" && identical(tuning_revision, rv$tuning_revision %||% 0L)) {
            for (target in c("act", "pre")) {
              nm <- paste0(l, "_", target)
              fit <- run_fits[[nm]]
              # Unseparated, the Predicted surface borrowed the measured-value
              # model. It was tuned on the Actual column, so it is not stored
              # under the prediction column's key, where it would later be
              # offered (or applied) as that column's variogram. The run
              # snapshot below still records what the surface was kriged with.
              if (target == "pre" && !sep_fit && identical(fit, run_fits[[paste0(l, "_act")]])) fit <- NULL
              # A model the user applied for this key stays applied: a run,
              # Auto-Fit included, never replaces it (author decision
              # 2026-09-17). OPTIMIZE ALL VARIOGRAMS still does.
              if (!is.null(resolve_stored_vgm(rv$v_fit_list[[nm]], "manual", tuning_keys[[target]]))) fit <- NULL
              if (!is.null(fit)) rv$v_fit_list[[nm]] <- stamp_vgm(fit, tuning_keys[[target]], "run")
              if (!is.null(run_emps[[nm]])) rv$v_emp_list[[nm]] <- stamp_vgm(run_emps[[nm]], tuning_keys[[target]], "run")
            }
          }
          if(!is.null(res$cv_pre)) rv$cv_metrics_pre[[l]] <- res$cv_pre
          if(!is.null(res$cv_obj_pre)) rv$cv_data_pre[[l]] <- res$cv_obj_pre
          rv$cv_info_pre[[l]] <- stamp_cv_population(res$cv_info_pre, tuning_keys[["pre"]])
          if(cv_repeats_val > 1) {
            reps_pre[[l]] <- res$cv_reps_pre %||% Filter(Negate(is.null), list(cv_repeat_frame(res$cv_obj_pre)))
          }
          if(!is.null(res$summ_pre)) rv$model_summaries[[paste0(l, "_pre")]] <- res$summ_pre
          if(!is.null(res$rf_pre)) rv$rf_models[[paste0(l, "_pre")]] <- res$rf_pre
          if(!is.null(res$gstat_pre)) rv$gstat_objs[[paste0(l, "_pre")]] <- res$gstat_pre
      }
      rv$disp$v_fits <- run_fits
      rv$disp$v_emps <- run_emps
      # What each locality's model used after the collinearity screen, and
      # what the screen removed: the record the selected list cannot give.
      if (current_method %in% c("RK", "RFK", "CK")) {
        cov_rec <- covariate_screen_record(res_all)
        rv$run_config_summary$covariates_retained <- cov_rec$retained
        rv$run_config_summary$covariates_dropped <- cov_rec$dropped
      }
      # The cell size each locality was gridded at. In Auto modes the sidebar
      # cannot know it before the run (it follows the boundary area), so the
      # Map Viewer's resolution overlay and the run record read it from here.
      rv$disp$grid_res_used <- grid_res_used
      if (length(grid_res_used)) {
        used <- round(unlist(grid_res_used), 1)
        rv$run_config_summary$resolution <- if (length(unique(used)) == 1) {
          sprintf("%s m", format(used[1], trim = TRUE))
        } else {
          sprintf("%s-%s m across %d localities",
                  format(min(used), trim = TRUE), format(max(used), trim = TRUE), length(used))
        }
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
          title = tags$div(style = "color: var(--mn-danger); font-weight: 600;", icon("exclamation-circle"),
                           sprintf("%d Region(s) Failed", length(failed_regions))),
          tags$p("An error occurred during modeling of the following localities:"),
          lapply(names(failed_regions), function(l) {
            tagList(
              tags$p(style = "margin-bottom: 4px;", tags$b(l)),
              tags$pre(style = "background-color: var(--mn-surface-2); color: var(--mn-text); border: 1px solid var(--mn-line); border-left: 2px solid var(--mn-danger); padding: 15px; border-radius: 4px; overflow-x: auto; white-space: pre-wrap; font-family: monospace; font-size: 0.9em;", failed_regions[[l]])
            )
          }),
          easyClose = TRUE,
          footer = modalButton("Dismiss")
        ))
      }

    # Map labels double as the exported figure's title, which names the method
    # as the Map Viewer's title does.
    m_lab <- get_method_label(current_method)
    valid_a <- Filter(Negate(is.null), rv$rast_list_act)
    valid_p <- Filter(Negate(is.null), rv$rast_list_pre)
    valid_r <- Filter(Negate(is.null), rv$rast_list_res)
    valid_pr <- Filter(Negate(is.null), rv$rast_list_point_res)
    
    if(length(valid_a) > 0) {
      rv$rast <- merge_wrapped_rasters(valid_a)
      register_export_item("map_actual", paste(meta$label, "- Actual Map -", m_lab), "map", rv$rast, meta$category,
                           legend = map_legend_title(meta$label, meta$unit))
      
      # Uncertainty products exist for the kriging engines only. IDW's var1.var
      # is all NA and TPS has none at all, so registering these for those
      # methods shipped two blank rasters into the export panel (the map
      # viewer's SE/variance views already carried this guard).
      temp_rast_a <- terra::unwrap(rv$rast)
      if (method_has_variance(current_method) && "var1.var" %in% names(temp_rast_a)) {
        # The Map Viewer offers its SE/variance views on this flag, so the menu
        # and the export registry agree about whether the run has a variance
        # band at all - not just about whether its method normally would.
        rv$disp$has_variance <- TRUE
        # Registered as derivations of the surface above, not as copies of it:
        # the variance band is already in `rv$rast` and the SE is its square
        # root, so two more packed layers per surface would hold the same
        # values a third and a fourth time, here and in every archived run.
        # `name` is the layer name the GeoTIFF records as its band description,
        # so the square root must not ship describing itself as a variance.
        register_export_item("map_uncert_var_act", paste(meta$label, "- Uncertainty Map (Variance - Actual) -", m_lab), "map", NULL, meta$category, kind = "uncertainty",
                             legend = map_legend_title(meta$label, meta$unit, "var"),
                             derived = list(src = temp_rast_a, layer = "var1.var", name = "var1.var"))
        register_export_item("map_uncert_se_act", paste(meta$label, "- Uncertainty Map (SE - Actual) -", m_lab), "map", NULL, meta$category, kind = "uncertainty",
                             legend = map_legend_title(meta$label, meta$unit, "se"),
                             derived = list(src = temp_rast_a, layer = "var1.var", fun = "sqrt", name = "var1.se"))
      }
    }
    if(length(valid_p) > 0) {
      rv$rast_pred <- merge_wrapped_rasters(valid_p)
      rv$has_predictions <- TRUE
      register_export_item("map_predicted", paste(meta$label, "- Predicted Map -", m_lab), "map", rv$rast_pred, meta$category,
                           legend = map_legend_title(meta$label, meta$unit))
      
      temp_rast_p <- terra::unwrap(rv$rast_pred)
      if (method_has_variance(current_method) && "var1.var" %in% names(temp_rast_p)) {
        rv$disp$has_variance <- TRUE
        register_export_item("map_uncert_var_pre", paste(meta$label, "- Uncertainty Map (Variance - Predicted) -", m_lab), "map", NULL, meta$category, kind = "uncertainty",
                             legend = map_legend_title(meta$label, meta$unit, "var"),
                             derived = list(src = temp_rast_p, layer = "var1.var", name = "var1.var"))
        register_export_item("map_uncert_se_pre", paste(meta$label, "- Uncertainty Map (SE - Predicted) -", m_lab), "map", NULL, meta$category, kind = "uncertainty",
                             legend = map_legend_title(meta$label, meta$unit, "se"),
                             derived = list(src = temp_rast_p, layer = "var1.var", fun = "sqrt", name = "var1.se"))
      }
    }
    if(length(valid_r) > 0) {
      rv$rast_res <- merge_wrapped_rasters(valid_r)
      register_export_item("map_residuals", paste(meta$label, "- ML Predictions Residual Map (Delta) -", m_lab), "map", rv$rast_res, meta$category, kind = "residual",
                           legend = map_legend_title(meta$label, layer = "resid"))
    }
    if(length(valid_pr) > 0) {
      rv$rast_point_res <- merge_wrapped_rasters(valid_pr)
      register_export_item("map_interp_point_errors", paste(meta$label, "- ML Predictions Interpolated Point Errors Map"), "map", rv$rast_point_res, meta$category, kind = "residual",
                           legend = map_legend_title(meta$label, layer = "resid"))
    }
    
    if(!is.null(rv$rast) && !is.null(rv$rast_pred)) {
       register_export_item("map_comparison", paste(meta$label, "- Actual vs Predicted Comparison -", m_lab), "map_combined", list(act = rv$rast, pre = rv$rast_pred), meta$category,
                            legend = map_legend_title(meta$label, meta$unit))
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
    rv$bound_overlap_m2 <- c(
      act = tryCatch(locality_boundary_overlap(b_list[names(rv$rast_list_act)]), error = function(e) NA_real_),
      pre = tryCatch(locality_boundary_overlap(b_list[names(rv$rast_list_pre)]), error = function(e) NA_real_))
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
    # This run supersedes any standalone variogram preview: the curves on the
    # Scientific Analysis tab now belong to a run again.
    rv$vgm_preview <- FALSE
    # Signals "this run's results are now in rv$..." to the cached Scientific
    # Analysis plots (their cache keys embed it).
    rv$results_rev <- rv$results_rev + 1L

    # Point error map: the discrete sample-location errors the Map Viewer's
    # Point Residuals panel shows, exported as points (not the IDW surface,
    # which is registered separately above as Interpolated Point Errors).
    if (!is.null(rv$sf) && "resid" %in% colnames(rv$sf) && any(!is.na(rv$sf$resid))) {
      pts_err <- rv$sf[!is.na(rv$sf$resid), c("resid", "loc")]
      register_export_item("map_point_residuals", paste(meta$label, "- ML Predictions Point Error Map"),
                           "map", list(pts = pts_err, bound = rv$bound), meta$category, kind = "residual",
                           legend = map_legend_title(meta$label, layer = "point_resid"))
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

    # NOTE: intentionally no get_current_meta() re-read here - the export
    # labels below must use the meta captured at dispatch, not whatever the
    # sidebar points at when the run finishes.

    # Pooled per-locality cross-validation: the "Total (Combined)" row of the
    # Model Performance card. Only the per-locality rows were exportable
    # before, so the figure a reader quotes for the whole run had to be
    # retyped off the screen. Pooling happens in pool_cv_sf()'s auto-UTM zone,
    # the same way the card does it.
    pooled_cv <- function(data_list, metrics_list, label, infos) {
      res <- perform_pooled_cv(data_list, metrics_list)
      if(is.null(res)) return(NULL)
      # The pooled row's Moran reading follows the localities it pooled: the
      # block reading only where every one of them was scored under blocks.
      types <- vapply(names(data_list), function(l) {
        applied_cv_plan(nrow(data_list[[l]]), rv$cv_strategy_sel, metrics_list[[l]])$type
      }, character(1))
      mor <- moran_reading(types)
      # The population record is taken over the localities that actually
      # pooled, so the exported id names the rows behind the exported numbers.
      cv_metrics_export_df(res, label,
                           paste0("pooled per-locality CV",
                                  if (mor$mixed) ", mixed fold designs" else ""),
                           pooled_cv_population(infos[names(data_list)]), mor)
    }
    cv_tot_a <- pooled_cv(rv$cv_data_act, rv$cv_metrics_act, "Actual Model", rv$cv_info_act)
    if(!is.null(cv_tot_a)) {
      register_export_item("table_cv_total", paste(meta$label, "- Total Model CV Metrics (Actual)"), "table", cv_tot_a, meta$category)
    }
    if(comp_mode || val_type != "actual") {
      cv_tot_p <- pooled_cv(rv$cv_data_pre, rv$cv_metrics_pre, "Predicted Model", rv$cv_info_pre)
      if(!is.null(cv_tot_p)) {
        register_export_item("table_cv_pre_total", paste(meta$label, "- Total Model CV Metrics (Predicted)"), "table", cv_tot_p, meta$category)
      }
    }
    if(!is.null(rv$cv_repeats_act$total)) {
      register_export_item("table_cv_repeats_total", paste(meta$label, "- Total Fold-Realization Stability (Actual)"), "table", cv_repeats_export_df(rv$cv_repeats_act$total, "Actual Model"), meta$category)
    }
    if((comp_mode || val_type != "actual") && !is.null(rv$cv_repeats_pre$total)) {
      register_export_item("table_cv_repeats_pre_total", paste(meta$label, "- Total Fold-Realization Stability (Predicted)"), "table", cv_repeats_export_df(rv$cv_repeats_pre$total, "Predicted Model"), meta$category)
    }

    # Every fitted variogram of the run in one sheet, one row per
    # locality/target - the combined view of the Variogram Parameters card.
    vgm_par_total <- vgm_params_export_df(rv$disp$v_fits)
    if(!is.null(vgm_par_total)) {
      register_export_item("table_vgm_params_total", paste(meta$label, "- Variogram Parameters (all localities)"), "table", vgm_par_total, meta$category)
    }

    # Regional IDW power / TPS lambda for every locality of the run: the
    # per-locality sheets below carry one row each, this is the whole set.
    if(current_method %in% c("IDW", "TPS")) {
      params_total <- build_regional_params_df(current_method, "Total (Combined)",
                                               rv$disp$regional_params,
                                               has_pre = comp_mode || val_type != "actual", export = TRUE)
      if(!is.null(params_total)) {
        register_export_item("table_params_total", paste(meta$label, "- Model Parameters (all localities)"), "table", params_total, meta$category)
      }
    }

    if(!is.null(rv$sf)) {
      df_perf <- rv$sf %>% st_drop_geometry() %>% filter(!is.na(v), !is.na(pv))
      # Same builder as the on-screen Prediction Performance card
      # (server_sci_analysis.R): perform_cv() owns every definition, so an
      # export cannot report a different CCC or an Inf RPD than the screen.
      perf_total <- pred_perf_df(df_perf$v, df_perf$pv)
      if(!is.null(perf_total)) {
        register_export_item("table_perf_uploaded_total", paste(meta$label, "- Total Prediction Performance"), "table", perf_total, meta$category)
      }
    }

    # The on-screen Descriptive Statistics card's own frame and builder: the
    # uploaded rows of the run's localities, not rv$sf (which is
    # coordinate-deduplicated and would summarise a different sample).
    sv_total <- stats_table_vectors(rv$user_data, rv$disp, rv$mapping$loc, rv$disp$localities)
    if(!is.null(sv_total)) {
      stats_total <- summary_stats_df(sv_total$act, sv_total$pre,
                                      labels = c("Total_Actual", "Total_Predicted"))
      if(!is.null(stats_total)) {
        register_export_item("table_stats_total", paste(meta$label, "- Total Descriptive Statistics"), "table", stats_total, meta$category)
      }
    }

    # Class-area and class-agreement tables are NOT registered here. They exist
    # only once the surface is classified, and the classification is normally
    # applied after a run, so registering them at run completion caught only
    # the case where the styling happened to be set beforehand. One observer in
    # server_sci_analysis.R now registers both families whenever the committed
    # classification changes, which covers this run too (rv$results_rev, bumped
    # above, is one of its triggers).

    for(l in locs) {
       register_locality_assets(l, meta, comp_mode, val_type, current_method)
    }
    
    # Before the completion marker, so the warnings read as part of the run
    # rather than as a footnote after it. This also clears the status files.
    persist_run_warnings()

    rv$log <- paste0(rv$log, "\n\n--- Run #", rv$run_counter, " Complete ---",
      "\nConfig: ", rv$run_config_summary$method, " | ", rv$run_config_summary$variable,
      " | ", rv$run_config_summary$localities,
      "\n", rv$run_config_summary$method_params)

    shinyjs::html("map_processing_title", "Map Generation Complete")
    update_premium_progress(100, "Click below to reveal the updated geostatistical surfaces.", step = 5)
    shinyjs::hide("cancel_model_btn")
    shinyjs::show("reveal_maps_btn")

    # Deliberately NOT re-enabled here. The run finished but its surfaces are
    # still behind the reveal overlay, and a second run started from this state
    # would overwrite results the user has not seen. The reveal handler is what
    # hands the button back.
    updateActionButton(session, "run", label = "Interpolated", icon = icon("check"))

    rv$model_running <- FALSE

      }, error = function(e) {
        # The interpolation itself completed; assembling/registering its results
        # in the main session did not. Say exactly that, and still release the
        # run UI so the app is usable. Whatever was assembled before the failure
        # stays in rv$ — the Reveal button is offered so partial surfaces can be
        # inspected, with the modal warning that they may be incomplete.
        rv$log <- paste0(rv$log, "\n\n[ERROR] Results assembly failed after a successful run: ", conditionMessage(e))
        shinyjs::hide("map_run_steps")
        shinyjs::hide("cancel_model_btn")
        shinyjs::html("map_processing_title", "Results Assembly Failed")
        shinyjs::show("reveal_maps_btn")
        shinyjs::enable("run")
        updateActionButton(session, "run", label = "Run Interpolation", icon = character(0))
        shinyjs::runjs("$('#run i').remove();")
        rv$model_running <- FALSE
        persist_run_warnings()
        showModal(modalDialog(
          title = tags$div(style = "color: var(--mn-danger); font-weight: 600;", icon("exclamation-triangle"), "Results Assembly Failed"),
          tags$p("The parallel interpolation finished, but an error occurred while assembling the results (merging rasters, building tables, or registering exports) in the main session:"),
          tags$pre(style = "background-color: var(--mn-surface-2); color: var(--mn-text); border: 1px solid var(--mn-line); border-left: 2px solid var(--mn-danger); padding: 15px; border-radius: 4px; overflow-x: auto; white-space: pre-wrap; font-family: monospace; font-size: 0.9em;", conditionMessage(e)),
          tags$p(style = "margin-top: 15px;", "The model outputs themselves are not in question. Any surfaces already assembled can be revealed, but maps, tables and the export registry may be incomplete for this run."),
          easyClose = TRUE,
          footer = modalButton("Dismiss")
        ))
      })
    }) %...!% (function(err) {
      if (this_token != rv$run_token) return()
      shinyjs::hide("map_progress_bar_container")
      shinyjs::hide("map_run_steps")
      shinyjs::hide("cancel_model_btn")
      shinyjs::hide("reveal_maps_btn")
      
      shinyjs::enable("run")
      updateActionButton(session, "run", label = "Run Interpolation", icon = character(0))
      shinyjs::runjs("$('#run i').remove();")
      
      if (grepl("cancelled", tolower(err$message))) {
        mark_run_cancelled()
        shinyjs::html("map_processing_title", "Interpolation Cancelled")
        shinyjs::html("map_progress_text", HTML("Please configure parameters in the left panel and click <b>'Run Interpolation'</b> to generate geostatistical maps and review diagnostic results."))
      } else {
        shinyjs::html("map_processing_title", "Interpolation Failed")
        shinyjs::html("map_progress_text", "An error occurred during parallel modeling. Please check the error message and click 'Run Interpolation' to try again.")
        showModal(modalDialog(
          title = tags$div(style = "color: var(--mn-danger); font-weight: 600;", icon("exclamation-triangle"), "Parallel Interpolation Failed"),
          tags$p("An error occurred while executing the parallel interpolation algorithms:"),
          tags$pre(style = "background-color: var(--mn-surface-2); color: var(--mn-text); border: 1px solid var(--mn-line); border-left: 2px solid var(--mn-danger); padding: 15px; border-radius: 4px; overflow-x: auto; white-space: pre-wrap; font-family: monospace; font-size: 0.9em;", err$message),
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
      persist_run_warnings()
    })
    
    }, error = function(e) {
      shinyjs::hide("map_progress_bar_container")
      shinyjs::hide("map_run_steps")
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

      # The engines write a common set of checkpoints into their progress
      # files: entry at 10-20, the fitted model by 50, the surface by 55, then
      # the CV repeats up to 90. avg_pct is their mean, so these bands say
      # which phase the run as a whole is in. (IDW and TPS cross-validate
      # before writing their surface, so for those two the last two entries
      # light in the reverse order to the work.)
      run_step <- if (avg_pct < 50) 2L else if (avg_pct < 55) 3L else 4L
      update_premium_progress(bar_width, step = run_step)

      # Header chip: only assign on change so the chip does not re-render at
      # every poll tick.
      chip_pct <- max(0, min(99, round(avg_pct)))
      if (!identical(rv$run_pct, chip_pct)) rv$run_pct <- chip_pct
      
      progress_msgs <- c()
      for (f in files) {
        # One parser for both readers of these file names (status_file_parts,
        # global_utils.R); the display keeps its underscores-as-spaces reading.
        parts <- status_file_parts(f, session_id, kind = "progress")
        val <- tryCatch(as.numeric(readLines(f, warn = FALSE)), error = function(e) NA_real_)
        if(length(val) > 0 && !is.na(val)) {
          progress_msgs <- c(progress_msgs, paste0("<b>", gsub("_", " ", parts$locality),
                                                   parts$suffix, "</b>: ", val, "%"))
        }
      }

      warn_msgs <- vapply(read_run_warnings(), function(w) {
        paste0("⚠️ <b>", gsub("_", " ", w$locality),
               w$suffix, "</b>: ", w$message)
      }, character(1))
      
      warn_block <- ""
      if (length(warn_msgs) > 0) {
        warn_block <- paste0("<br/><span style='font-size: 0.85em; color: var(--mn-danger); margin-top: 5px; display: inline-block;'>", paste(warn_msgs, collapse = "<br/>"), "</span>")
      }
      
      if (length(progress_msgs) > 0) {
        shinyjs::html("map_progress_text", paste0("Executing Parallel Interpolation Algorithms...<br/><span style='font-size: 0.85em; opacity: 0.8;'>", paste(progress_msgs, collapse = " &nbsp;|&nbsp; "), "</span>", warn_block))
      }
    }
  })

  # The record of a run cancelled before it produced results. The previous
  # results were archived or discarded at dispatch, so there is nothing to fall
  # back to: the panels say what was requested and that it never finished, and
  # push_run_history() refuses to archive the record as a result.
  mark_run_cancelled <- function() {
    cfg <- rv$run_config_summary
    if (is.null(cfg) || identical(cfg$status, "cancelled")) return(invisible(NULL))
    rv$run_config_summary$status <- "cancelled"
    rv$run_config_summary$cancelled_at <- Sys.time()
    rv$log <- paste0(rv$log, "\n--- Run #", cfg$run_id, " Cancelled ---")
    invisible(NULL)
  }

  observeEvent(input$cancel_model_btn, {
    # Flags the run actually in flight; the next run gets its own file, so this
    # cancellation cannot be revoked by starting another run.
    cancel_file <- file.path(session_progress_dir,
                             paste0("cancel_flag_", rv$run_counter, ".txt"))
    file.create(cancel_file)
    rv$model_running <- FALSE
    rv$run_token <- rv$run_token + 1L
    mark_run_cancelled()
    
    shinyjs::hide("map_progress_bar_container")
    shinyjs::hide("map_run_steps")
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
    # The run button was held disabled from the moment the run completed;
    # revealing the results is what makes starting another run legitimate.
    shinyjs::enable("run")
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
