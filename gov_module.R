# The Tabular Data Metrics table of a governing-factors result: the model's
# quality first, then one row per covariate in decreasing importance. Value
# and Unit are separate because the rows differ in kind (importance = OOB MSE
# increase in squared target units; quality = a percentage). The scaled column
# is that increase over its SE across trees (%IncMSE; unitless, grows with
# ntree). Unrounded; `labels` is the suite's display-label map (NULL = names).
GOV_IMPORTANCE_UNIT <- "Increase in out-of-bag MSE (target units squared)"
GOV_SCALED_COL <- "Scaled (÷ SE)"
gov_summary_df <- function(res, labels = NULL) {
  if (is.null(res) || is.null(res$importance)) return(NULL)
  vip <- res$importance
  vip <- vip[order(vip$mse_increase, decreasing = TRUE), , drop = FALSE]
  out <- data.frame(
    `Governing Factor / Metric` = display_var_labels(as.character(vip$variable), labels),
    Value = as.numeric(vip$mse_increase),
    Unit = GOV_IMPORTANCE_UNIT,
    check.names = FALSE, stringsAsFactors = FALSE
  )
  out[[GOV_SCALED_COL]] <- as.numeric(vip$mse_increase_scaled %||% NA_real_)
  # Model quality: OOB % variance explained. The module keeps `oob_rsq` and
  # drops the forest after a run; a direct call still passes the whole result.
  oob_rsq <- res$oob_rsq
  if (is.null(oob_rsq)) oob_rsq <- tryCatch(utils::tail(res$model$rsq, 1), error = function(e) NULL)
  if (!is.null(oob_rsq) && length(oob_rsq) == 1 && is.finite(oob_rsq)) {
    quality <- data.frame(
      `Governing Factor / Metric` = "RF model quality: OOB variance explained",
      Value = 100 * as.numeric(oob_rsq), Unit = "% of variance (out-of-bag)",
      check.names = FALSE, stringsAsFactors = FALSE)
    quality[[GOV_SCALED_COL]] <- NA_real_
    out <- rbind(quality, out)
  }
  out
}

# The four plot cards of a result, in two rows of two: the key names the
# expand ids (gov_expand_<key>_btn, gov_<key>_expand_mode, ...), `kind` is
# gov_create_plot()'s plot type.
GOV_PLOT_CARDS <- list(
  list(key = "imp", title = "Global Importance", aria = "Expand global importance plot",
       plot_id = "gov_plot_importance", kind = "importance"),
  list(key = "inta", title = "SHAP Dependence (Marginal)", aria = "Expand SHAP dependence plot",
       plot_id = "gov_plot_interaction_a", kind = "interaction_a"),
  list(key = "eff", title = "Functional Effect", aria = "Expand functional effect plot",
       plot_id = "gov_plot_effect", kind = "effect"),
  list(key = "intb", title = "Feature vs Target Scatterplot", aria = "Expand feature versus target plot",
       plot_id = "gov_plot_interaction_b", kind = "interaction_b")
)

gov_factors_ui <- function(id) {
  ns <- shiny::NS(id)
  cards <- lapply(GOV_PLOT_CARDS, function(card) {
    shiny::column(6,
      shiny::h4(card$title),
      shiny::div(style = "position: relative;",
        expand_overlay_btn(ns(paste0("gov_expand_", card$key, "_btn")), card$aria),
        shiny::plotOutput(ns(card$plot_id), height = "300px")))
  })

  shiny::tagList(
    shiny::div(style = "padding: 10px;",
      shiny::fluidRow(
        shiny::column(3,
          shiny::div(style = "display: flex; align-items: center; margin-bottom: 10px;",
            shiny::h4("Analysis Configuration", style = "margin: 0;"),
            info_tooltip(ns("gov_config_info"), "The forest is fitted on the rows of the active groups. At least 50 of them must be complete (the target and every selected factor present) to fit it reliably.")
          ),
          shiny::uiOutput(ns("gov_target_ui")),
          shiny::uiOutput(ns("gov_predictors_ui")),
          shiny::sliderInput(ns("gov_ntree"), shiny::tags$span("Number of Trees (ntree)", info_tooltip(ns("gov_ntree_info"), "Importance is each predictor's permutation importance on the out-of-bag rows, averaged over the trees; more trees give a more stable estimate.")), min = 50, max = 500, value = 100, step = 50, ticks = FALSE),
          shiny::sliderInput(ns("gov_shap_size"), shiny::tags$span("SHAP Sample Size (Max)", info_tooltip(ns("gov_shap_info"), "Lower values calculate faster but are less stable; higher values take longer but represent the dataset better.")), min = 50, max = 1000, value = 100, step = 50, ticks = FALSE),
          shiny::actionButton(ns("gov_run_btn"), "Run Analysis", class = "btn-primary btn-block"),
          shiny::hr(),
          shiny::h4("Plot Settings"),
          shiny::radioButtons(ns("gov_effect_type"), "Functional Effect Plot:", choices = c("ALE" = "ale", "PDP" = "pdp"), inline = TRUE)
        ),
        shiny::column(9,
          # Shown by shinyjs custom messages, not a conditionalPanel on
          # output$gov_ready, which would only reach the browser after the run
          # observer returns.
          shinyjs::hidden(
            shiny::div(id = ns("gov_running_panel"),
              style = "text-align: center; padding: 100px 50px; background-color: var(--mn-surface); border-radius: 8px; border: 2px dashed var(--mn-text-3); margin-bottom: 20px; transition: all 0.3s ease;",
              shiny::icon("circle-notch", class = "fa-spin fa-4x", style = "color: var(--mn-text-3); margin-bottom: 20px;"),
              shiny::h3("Executing Machine Learning Analytics...", style = "color: var(--mn-text); font-weight: 600; margin-bottom: 10px;"),
              shiny::p("Fitting high-dimensional Random Forest models and extracting explanatory SHAP, PDP, and ALE profiles in the background.", style = "color: var(--mn-text-2); font-size: 1.1em;"),
              shiny::p("The dashboard stays responsive while the module is running.", style = "color: var(--mn-text-3); font-style: italic; font-size: 0.9em; margin-top: 15px;"),
              shiny::actionButton(ns("gov_cancel_btn"), "Cancel Run",
                                  icon = shiny::icon("stop"),
                                  class = "btn-danger btn-sm",
                                  style = "margin-top: 15px;")
            )
          ),

          shiny::conditionalPanel(
            condition = sprintf("output['%s'] == 'no'", ns("gov_ready")),
            shiny::div(id = ns("gov_idle_content"), style = "text-align: center; padding: 120px 50px; color: var(--mn-text-3);",
              shiny::icon("brain", class = "fa-4x", style = "margin-bottom: 20px; color: var(--mn-line-2);"),
              shiny::h3("Awaiting Machine Learning Analysis", style = "font-weight: 300; margin-bottom: 10px;"),
              shiny::p("Configure target and predictors on the left pane and click 'Run Analysis' to discover the factors governing the target.")
            )
          ),
          
          shiny::conditionalPanel(
            condition = sprintf("output['%s'] == 'yes'", ns("gov_ready")),
            shiny::fluidRow(cards[[1]], cards[[2]]),
            shiny::hr(),
            shiny::fluidRow(cards[[3]], cards[[4]]),
            shiny::hr(),
            sci_table(ns("gov_summary_table"), "Tabular Data Metrics", title_tag = shiny::h4,
              content = shiny::tagList(
                shiny::div(class = "table-container", DT::dataTableOutput(ns("gov_summary_table"))),
                shiny::div(class = "mn-table-note",
                  "Out-of-bag samples lie among the training samples, so with spatially autocorrelated samples the OOB variance explained overstates how well the forest predicts at new locations (Meyer et al. 2019; Ploton et al. 2020).")))
          )
        )
      )
    )
  )
}

# `data_reactive` gives the columns offered as target and factors;
# `rows_reactive` the rows a run is fitted on (the Analytics Engine's active
# groups), read when Run Analysis is pressed.
gov_factors_server <- function(id, data_reactive, rows_reactive, vars_metadata_reactive) {
  shiny::moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # The suite's display labels (Variable naming toggle included): labels
    # shared by two columns carry their column name, as on the other tabs.
    display_labels <- shiny::reactive(desc_var_labels(colnames(data_reactive()), vars_metadata_reactive()))
    lab <- function(v) display_var_labels(v, display_labels())
    numeric_choices <- shiny::reactive({
      df <- data_reactive()
      shiny::req(df)
      cols <- colnames(df)[vapply(df, is.numeric, logical(1))]
      stats::setNames(cols, lab(cols))
    })

    gov_rv <- shiny::reactiveValues(res = NULL, ready = "no")

    # Cooperative cancellation through a session-scoped flag file (the run
    # lives in a future worker), as in the classification module.
    gov_cancel_file <- file.path(tempdir(),
      paste0("gov_cancel_", substr(session$token, 1, 12), ".txt"))

    # One place that puts the run controls back to idle, so no failure path can
    # leave "Running..." on a button that is no longer running.
    reset_gov_run_ui <- function() {
      shinyjs::enable("gov_run_btn")
      shiny::updateActionButton(session, "gov_run_btn", label = "Run Analysis", icon = character(0))
      shinyjs::hide("gov_running_panel")
      shinyjs::show("gov_idle_content")
      shinyjs::enable("gov_cancel_btn")
    }

    shiny::observeEvent(input$gov_cancel_btn, {
      if (!identical(gov_rv$ready, "running")) return(NULL)
      tryCatch(file.create(gov_cancel_file), error = function(e) NULL)
      shinyjs::disable("gov_cancel_btn")
      shiny::showNotification(
        "Cancelling after the current step finishes...", type = "warning", duration = 4)
    })


    # isolate: a new upload or a Variable naming switch rebuilds the selectors,
    # which keep the picks whose columns are still there.
    output$gov_target_ui <- shiny::renderUI({
      choices <- numeric_choices()
      keep <- intersect(shiny::isolate(input$gov_target), choices)
      shiny::selectInput(ns("gov_target"), "Target Parameter", choices = choices,
                         selected = if (length(keep)) keep)
    })

    output$gov_predictors_ui <- shiny::renderUI({
      choices <- numeric_choices()
      shinyWidgets::pickerInput(
        ns("gov_predictors"), "Governing Factors",
        choices = choices, multiple = TRUE,
        selected = intersect(shiny::isolate(input$gov_predictors), choices),
        options = list(`actions-box` = TRUE, `live-search` = TRUE)
      )
    })
    
    shiny::observeEvent(input$gov_run_btn, {
      if (identical(gov_rv$ready, "running")) {
        return(NULL)
      }
      
      df <- rows_reactive()
      shiny::req(df, input$gov_target, input$gov_predictors)

      preds <- setdiff(input$gov_predictors, input$gov_target)
      if (length(preds) < 1) {
        shiny::showNotification("Select at least one governing factor other than the target.", type = "error")
        return()
      }
      # The forest is fitted on the rows complete across the target and every
      # factor, so those are the rows that count against the minimum.
      n_complete <- sum(stats::complete.cases(df[, unique(c(input$gov_target, preds)), drop = FALSE]))
      if (n_complete < 50) {
        shiny::showNotification(sprintf(
          "%d of the %d rows in the active groups have the target and every selected factor; at least 50 such rows are required.",
          n_complete, nrow(df)), type = "error")
        return()
      }
      
      # Clear any flag left by a previous cancelled run, or this run would abort
      # at its first checkpoint.
      if (file.exists(gov_cancel_file)) {
        tryCatch(file.remove(gov_cancel_file), error = function(e) NULL)
      }
      shinyjs::enable("gov_cancel_btn")

      shinyjs::disable("gov_run_btn")
      # updateActionButton, not shinyjs::html, which would destroy Shiny's
      # .action-label span.
      shiny::updateActionButton(session, "gov_run_btn", label = "Running...", icon = shiny::icon("spinner", class = "fa-spin"))
      # Feedback by custom message (see the panel's definition).
      shinyjs::hide("gov_idle_content")
      shinyjs::show("gov_running_panel")

      gov_rv$ready <- "running"

      target_col <- input$gov_target
      ntree_val <- input$gov_ntree
      shap_size_val <- input$gov_shap_size
      # Ship only the columns the worker uses (numerically identical).
      df <- df[, unique(c(target_col, preds)), drop = FALSE]
      # Read the core count here in the main session: inside the promise worker
      # availableCores() reports 1, which would suppress the SHAP escalation.
      cores_hint_val <- tryCatch(as.integer(parallelly::availableCores()), error = function(e) 1L)
      cancel_file_ship <- gov_cancel_file
      proj_root_ship <- getwd()

      # `globals =` pinned to plain data; the worker builds the helper set
      # itself, as the interpolation and classification workers do. Automatic
      # discovery also shipped masked base generics (spam, terra), making every
      # worker attach spam, terra, fields and sf for nothing.
      promises::future_promise({
        setwd(proj_root_ship)
        library(DALEX)
        library(randomForest)
        # compute_governing_factors() and gov_shap_item() are top level in a
        # fragment registered in spatial_helpers.R, so the worker defines them.
        source("spatial_helpers.R", local = FALSE)
        # Reset the shared pool worker's plan: compute_governing_factors()
        # reads nbrOfWorkers() to decide on SHAP escalation.
        future::plan(future::sequential)
        compute_governing_factors(
          df = df,
          target_col = target_col,
          predictors = preds,
          rf_ntree = ntree_val,
          shap_sample_size = shap_size_val,
          cores_hint = cores_hint_val,
          cancel_file = cancel_file_ship
        )
      },
      globals = list(proj_root_ship = proj_root_ship, df = df,
                     target_col = target_col, preds = preds,
                     ntree_val = ntree_val, shap_size_val = shap_size_val,
                     cores_hint_val = cores_hint_val,
                     cancel_file_ship = cancel_file_ship),
      # A parallel-safe L'Ecuyer stream for the worker (avoids future's
      # "UNRELIABLE VALUE" warning); compute_governing_factors() draws under
      # with_seed(), which names its generator, so its values match an
      # in-process call or test-governing-factors.R.
      seed = TRUE) %...>% (function(res) {
        reset_gov_run_ui()
        if (!is.null(res)) {
          # Commit the run's display context: the scatter panel keeps
          # describing THIS run's target and data whatever changes later.
          res$target_col <- target_col
          res$analysis_df <- df
          # The panels never read the forest or the DALEX explainer (~5-32 MB
          # kept for the tab's life), so keep the OOB R^2 and drop both.
          res$oob_rsq <- tryCatch(utils::tail(res$model$rsq, 1), error = function(e) NULL)
          res$model <- NULL
          res$explainer <- NULL
          gov_rv$res <- res
          gov_rv$ready <- "yes"
          shiny::showNotification("ML evaluation completed successfully!", type = "message")
        } else {
          gov_rv$ready <- "no"
          shiny::showNotification("Failed to calculate governing factors. Check data quality.", type = "error")
        }
      }) %...!% (function(err) {
        was_cancelled <- grepl("cancelled by user", err$message, fixed = TRUE)
        reset_gov_run_ui()
        gov_rv$ready <- "no"
        if (was_cancelled) {
          # A cancelled run is a user action, not a failure: say so plainly and
          # clear the flag so the next run is not aborted by a stale file.
          tryCatch(if (file.exists(gov_cancel_file)) file.remove(gov_cancel_file),
                   error = function(e) NULL)
          shiny::showNotification("Analysis cancelled.", type = "message")
        } else {
          shiny::showNotification(paste("Error running ML analysis:", err$message), type = "error")
        }
      })
      
      NULL
    })
    
    output$gov_ready <- shiny::reactive({
      gov_rv$ready
    })
    shiny::outputOptions(output, "gov_ready", suspendWhenHidden = FALSE)
    
    gov_create_plot <- function(plot_type = c("importance", "interaction_a", "effect", "interaction_b"), expanded = FALSE) {
      plot_type <- match.arg(plot_type)
      base_size <- if (expanded) 16 else 11
      
      if (plot_type == "importance") {
        build_gov_importance_plot(gov_rv$res, display_labels(), base_size = base_size)
      } else if (plot_type == "interaction_a") {
        shap_df <- gov_rv$res$shap
        top_var_label <- lab(gov_rv$res$top_var)
        
        p <- ggplot2::ggplot(shap_df, ggplot2::aes(x = feature_value, y = contribution))
        if (expanded) {
          p <- p + ggplot2::geom_point(color = "darkred", alpha = 0.6, size = 3) +
                   ggplot2::geom_smooth(method = "loess", color = "blue", se = FALSE, linewidth = 1.5)
        } else {
          p <- p + ggplot2::geom_point(color = "darkred", alpha = 0.6) +
                   ggplot2::geom_smooth(method = "loess", color = "blue", se = FALSE)
        }
        p + ggplot2::labs(title = paste("SHAP Dependence:", top_var_label), x = paste(top_var_label, "Value"), y = "SHAP Contribution") + 
            ggplot2::theme_minimal(base_size = base_size)
      } else if (plot_type == "effect") {
        top_var_label <- lab(gov_rv$res$top_var)
        lw <- if (expanded) 2 else 1
        
        if (!is.null(input$gov_effect_type) && input$gov_effect_type == "ale") {
          ale_df <- gov_rv$res$ale
          ggplot2::ggplot(ale_df, ggplot2::aes(x = `_x_`, y = `_yhat_`)) + 
            ggplot2::geom_line(color = "purple", linewidth = lw) +
            ggplot2::labs(title = paste("ALE Profile:", top_var_label), x = top_var_label, y = "ALE Effect") + 
            ggplot2::theme_minimal(base_size = base_size)
        } else {
          pdp_df <- gov_rv$res$pdp
          ggplot2::ggplot(pdp_df, ggplot2::aes(x = `_x_`, y = `_yhat_`)) + 
            ggplot2::geom_line(color = "darkorange", linewidth = lw) + 
            ggplot2::geom_rug(sides = "b", alpha = 0.3) +
            ggplot2::labs(title = paste("PDP Profile:", top_var_label), x = top_var_label, y = "Partial Dependence") + 
            ggplot2::theme_minimal(base_size = base_size)
        }
      } else if (plot_type == "interaction_b") {
        # Committed run context only, never live input$gov_target or
        # data_reactive(), so this panel matches the other panels' run.
        df <- gov_rv$res$analysis_df
        run_target <- gov_rv$res$target_col
        shiny::req(df, run_target)
        top_var_label <- lab(gov_rv$res$top_var)
        target_label <- lab(run_target)

        if (gov_rv$res$top_var %in% colnames(df) && run_target %in% colnames(df)) {
          p <- ggplot2::ggplot(df, ggplot2::aes(x = .data[[gov_rv$res$top_var]], y = .data[[run_target]]))
          if (expanded) {
            p <- p + ggplot2::geom_point(alpha = 0.5, size = 3) + 
                     ggplot2::geom_smooth(method = "lm", color = "red", linewidth = 1.5)
          } else {
            p <- p + ggplot2::geom_point(alpha = 0.5) + 
                     ggplot2::geom_smooth(method = "lm", color = "red")
          }
          p + ggplot2::labs(title = paste("Target vs Top Factor:", top_var_label), x = top_var_label, y = target_label) + 
              ggplot2::theme_minimal(base_size = base_size)
        } else {
          sci_placeholder("Data not available")
        }
      }
    }
    
    # Each card's plot and its expand modal (the expanded build is larger).
    # Fields are bound here: the modal reads its title only when first opened.
    for (.card in GOV_PLOT_CARDS) local({
      kind <- .card$kind
      key <- .card$key
      title <- .card$title
      output[[.card$plot_id]] <- shiny::renderPlot({
        shiny::req(gov_rv$res)
        gov_create_plot(kind, expanded = FALSE)
      })
      register_expanded_modal(
        input, output, session,
        btn_id = paste0("gov_expand_", key, "_btn"),
        mode_id = paste0("gov_", key, "_expand_mode"),
        ui_id = paste0("gov_", key, "_expanded_ui"),
        plot_static_id = paste0("gov_plot_", key, "_exp"),
        plot_plotly_id = paste0("gov_plot_", key, "_exp_plotly"),
        title_text = title,
        build_fn = function() { shiny::req(gov_rv$res); gov_create_plot(kind, expanded = TRUE) }
      )
    })

    output$gov_summary_table <- DT::renderDataTable({
      shiny::req(gov_rv$res)
      df <- gov_summary_df(gov_rv$res, display_labels())
      # paging off: dom = 't' shows no paging controls. Values stay numeric,
      # displayed at four significant digits.
      DT::datatable(df, options = list(dom = 't', paging = FALSE, scrollX = TRUE,
                                       columnDefs = sig_render_defs(df, c("Value", GOV_SCALED_COL))),
                    rownames = FALSE)
    })
  })
}
