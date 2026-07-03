gov_factors_ui <- function(id) {
  ns <- shiny::NS(id)
  
  shiny::tagList(
    shiny::div(style = "padding: 10px;",
      shiny::fluidRow(
        shiny::column(3,
          shiny::div(style = "display: flex; align-items: center; margin-bottom: 10px;",
            shiny::h4("Analysis Configuration", style = "margin: 0; margin-right: 8px;"),
            shiny::tags$i(class = "fa fa-info-circle", style = "color: #007bff; cursor: help;", title = "A minimum of 50 samples is required to reliably execute Machine Learning models.")
          ),
          shiny::uiOutput(ns("gov_target_ui")),
          shiny::uiOutput(ns("gov_predictors_ui")),
          shiny::sliderInput(ns("gov_permutations"), "Permutations (for RF importance)", min = 10, max = 100, value = 50, step = 10),
          shiny::actionButton(ns("gov_run_btn"), "Run Analysis", class = "btn-primary btn-block"),
          shiny::hr(),
          shiny::h4("Plot Settings"),
          shiny::radioButtons(ns("gov_effect_type"), "Functional Effect Plot:", choices = c("ALE" = "ale", "PDP" = "pdp"), inline = TRUE)
        ),
        shiny::column(9,
          shiny::conditionalPanel(
            condition = sprintf("output['%s'] == 'running'", ns("gov_ready")),
            shiny::div(style = "text-align: center; padding: 100px 50px; background-color: rgba(255,255,255,0.02); border-radius: 8px; border: 2px dashed #007bff; margin-bottom: 20px; transition: all 0.3s ease;",
              shiny::div(class = "premium-spinner", style = "margin: 0 auto 20px auto; border-top-color: #007bff; width: 60px; height: 60px; border-radius: 50%; border: 5px solid rgba(0,0,0,0.05); animation: premium-spin 1.2s cubic-bezier(0.5, 0, 0.5, 1) infinite;"),
              shiny::h3("Executing Machine Learning Analytics...", style = "color: #007bff; font-weight: bold; margin-bottom: 10px;"),
              shiny::p("Fitting high-dimensional Random Forest models and extracting explanatory SHAP, PDP, and ALE profiles in the background.", style = "color: #666; font-size: 1.1em;"),
              shiny::p("The dashboard remains fully responsive. You can view other tabs or start other operations.", style = "color: #888; font-style: italic; font-size: 0.9em; margin-top: 15px;")
            )
          ),
          
          shiny::conditionalPanel(
            condition = sprintf("output['%s'] == 'no'", ns("gov_ready")),
            shiny::div(style = "text-align: center; padding: 120px 50px; color: #888;",
              shiny::icon("brain", class = "fa-4x", style = "margin-bottom: 20px; color: #ccc;"),
              shiny::h3("Awaiting Machine Learning Analysis", style = "font-weight: 300; margin-bottom: 10px;"),
              shiny::p("Configure target and predictors on the left pane and click 'Run Analysis' to discover governing agronomical factors.")
            )
          ),
          
          shiny::conditionalPanel(
            condition = sprintf("output['%s'] == 'yes'", ns("gov_ready")),
            shiny::fluidRow(
              shiny::column(6, 
                shiny::h4("Global Importance"),
                shiny::div(style = "position: relative;",
                  shiny::tags$button(id = ns("gov_expand_imp_btn"), type = "button", class = "btn btn-default action-button expand-icon-btn", shiny::icon("expand")),
                  shiny::plotOutput(ns("gov_plot_importance"), height = "300px")
                )
              ),
              shiny::column(6, 
                shiny::h4("SHAP Dependence (Marginal)"),
                shiny::div(style = "position: relative;",
                  shiny::tags$button(id = ns("gov_expand_inta_btn"), type = "button", class = "btn btn-default action-button expand-icon-btn", shiny::icon("expand")),
                  shiny::plotOutput(ns("gov_plot_interaction_a"), height = "300px")
                )
              )
            ),
            shiny::hr(),
            shiny::fluidRow(
              shiny::column(6, 
                shiny::h4("Functional Effect"),
                shiny::div(style = "position: relative;",
                  shiny::tags$button(id = ns("gov_expand_eff_btn"), type = "button", class = "btn btn-default action-button expand-icon-btn", shiny::icon("expand")),
                  shiny::plotOutput(ns("gov_plot_effect"), height = "300px")
                )
              ),
              shiny::column(6, 
                shiny::h4("Feature vs Target Scatterplot"),
                shiny::div(style = "position: relative;",
                  shiny::tags$button(id = ns("gov_expand_intb_btn"), type = "button", class = "btn btn-default action-button expand-icon-btn", shiny::icon("expand")),
                  shiny::plotOutput(ns("gov_plot_interaction_b"), height = "300px")
                )
              )
            ),
            shiny::hr(),
            shiny::h4("Tabular Data Metrics"),
            DT::dataTableOutput(ns("gov_summary_table"))
          )
        )
      )
    )
  )
}

gov_factors_server <- function(id, data_reactive, vars_metadata_reactive) {
  shiny::moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    gov_rv <- shiny::reactiveValues(res = NULL, ready = "no")
    
    output$gov_target_ui <- shiny::renderUI({
      df <- data_reactive()
      shiny::req(df)
      cols <- colnames(df)
      num_cols <- cols[sapply(df, is.numeric)]
      
      num_named <- sapply(num_cols, function(v) get_var_label(v, vars_metadata_reactive()))
      names(num_cols) <- num_named
      
      shiny::selectInput(ns("gov_target"), "Target Parameter", choices = num_cols)
    })
    
    output$gov_predictors_ui <- shiny::renderUI({
      df <- data_reactive()
      shiny::req(df)
      cols <- colnames(df)
      num_cols <- cols[sapply(df, is.numeric)]
      
      num_named <- sapply(num_cols, function(v) get_var_label(v, vars_metadata_reactive()))
      names(num_cols) <- num_named
      
      shinyWidgets::pickerInput(
        ns("gov_predictors"), "Governing Factors", 
        choices = num_cols, multiple = TRUE, 
        options = list(`actions-box` = TRUE)
      )
    })
    
    shiny::observeEvent(input$gov_run_btn, {
      if (identical(gov_rv$ready, "running")) {
        return(NULL)
      }
      
      df <- data_reactive()
      shiny::req(df, input$gov_target, input$gov_predictors)
      
      preds <- setdiff(input$gov_predictors, input$gov_target)
      
      if (length(preds) < 1 || nrow(df) < 50) {
        shiny::showNotification("Insufficient data or predictors for analysis. At least 50 observations are required.", type = "error")
        return()
      }
      
      shinyjs::disable("gov_run_btn")
      shiny::updateActionButton(session, "gov_run_btn", label = "Running...", icon = shiny::icon("spinner", class = "fa-spin"))
      
      gov_rv$ready <- "running"
      
      target_col <- input$gov_target
      n_perms <- input$gov_permutations
      
      promises::future_promise({
        compute_governing_factors(
          df = df, 
          target_col = target_col, 
          predictors = preds, 
          n_permutations = n_perms
        )
      }) %...>% (function(res) {
        shinyjs::enable("gov_run_btn")
        shiny::updateActionButton(session, "gov_run_btn", label = "Run Analysis", icon = character(0))
        if (!is.null(res)) {
          gov_rv$res <- res
          gov_rv$ready <- "yes"
          shiny::showNotification("ML evaluation completed successfully!", type = "message")
        } else {
          gov_rv$ready <- "no"
          shiny::showNotification("Failed to calculate governing factors. Check data quality.", type = "error")
        }
      }) %...!% (function(err) {
        shinyjs::enable("gov_run_btn")
        shiny::updateActionButton(session, "gov_run_btn", label = "Run Analysis", icon = character(0))
        gov_rv$ready <- "no"
        shiny::showNotification(paste("Error running ML analysis:", err$message), type = "error")
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
        vip_df <- gov_rv$res$importance
        vip_df <- vip_df[!vip_df$variable %in% c("_baseline_", "_full_model_", "(Intercept)"), ]
        vip_df <- vip_df[order(vip_df$dropout_loss, decreasing = FALSE), ]
        vip_df$variable_label <- sapply(as.character(vip_df$variable), function(v) get_var_label(v, vars_metadata_reactive()))
        vip_df$variable_label <- factor(vip_df$variable_label, levels = vip_df$variable_label)
        
        ggplot2::ggplot(vip_df, ggplot2::aes(x = variable_label, y = dropout_loss)) + 
          ggplot2::geom_bar(stat = "identity", fill = "steelblue") +
          ggplot2::coord_flip() + 
          ggplot2::labs(title = "Global Variable Importance", x = "Variable", y = "Dropout Loss (RMSE increase)") + 
          ggplot2::theme_minimal(base_size = base_size)
      } else if (plot_type == "interaction_a") {
        shap_df <- gov_rv$res$shap
        top_var_label <- get_var_label(gov_rv$res$top_var, vars_metadata_reactive())
        
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
        top_var_label <- get_var_label(gov_rv$res$top_var, vars_metadata_reactive())
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
        df <- data_reactive()
        shiny::req(df)
        top_var_label <- get_var_label(gov_rv$res$top_var, vars_metadata_reactive())
        target_label <- get_var_label(input$gov_target, vars_metadata_reactive())
        
        if (gov_rv$res$top_var %in% colnames(df) && input$gov_target %in% colnames(df)) {
          p <- ggplot2::ggplot(df, ggplot2::aes(x = .data[[gov_rv$res$top_var]], y = .data[[input$gov_target]]))
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
          ggplot2::ggplot() + ggplot2::annotate("text", x=0, y=0, label="Data not available") + ggplot2::theme_void()
        }
      }
    }
    
    output$gov_plot_importance <- shiny::renderPlot({
      shiny::req(gov_rv$res)
      gov_create_plot("importance", expanded = FALSE)
    })
    
    output$gov_plot_interaction_a <- shiny::renderPlot({
      shiny::req(gov_rv$res)
      gov_create_plot("interaction_a", expanded = FALSE)
    })
    
    output$gov_plot_effect <- shiny::renderPlot({
      shiny::req(gov_rv$res)
      gov_create_plot("effect", expanded = FALSE)
    })
    
    output$gov_plot_interaction_b <- shiny::renderPlot({
      shiny::req(gov_rv$res, input$gov_target)
      gov_create_plot("interaction_b", expanded = FALSE)
    })
    
    output$gov_summary_table <- DT::renderDataTable({
      shiny::req(gov_rv$res)
      vip_df <- gov_rv$res$importance
      vip_df <- vip_df[order(vip_df$dropout_loss, decreasing = TRUE), ]
      
      vip_df$variable <- sapply(as.character(vip_df$variable), function(v) get_var_label(v, vars_metadata_reactive()))
      colnames(vip_df) <- c("Governing Factor", "Importance (Dropout Loss)")
      
      DT::datatable(vip_df, options = list(pageLength = 5, dom = 't', scrollX = TRUE), rownames = FALSE)
    })
    
    gov_build_imp_plot <- function() { shiny::req(gov_rv$res); gov_create_plot("importance", expanded = TRUE) }
    gov_build_inta_plot <- function() { shiny::req(gov_rv$res); gov_create_plot("interaction_a", expanded = TRUE) }
    gov_build_eff_plot <- function() { shiny::req(gov_rv$res); gov_create_plot("effect", expanded = TRUE) }
    gov_build_intb_plot <- function() { shiny::req(gov_rv$res, input$gov_target); gov_create_plot("interaction_b", expanded = TRUE) }
    
    register_expanded_modal(
      input, output, session,
      btn_id = "gov_expand_imp_btn",
      mode_id = "gov_imp_expand_mode",
      ui_id = "gov_imp_expanded_ui",
      plot_static_id = "gov_plot_imp_exp",
      plot_plotly_id = "gov_plot_imp_exp_plotly",
      title_text = "Global Importance",
      build_fn = gov_build_imp_plot
    )
    
    register_expanded_modal(
      input, output, session,
      btn_id = "gov_expand_inta_btn",
      mode_id = "gov_inta_expand_mode",
      ui_id = "gov_inta_expanded_ui",
      plot_static_id = "gov_plot_inta_exp",
      plot_plotly_id = "gov_plot_inta_exp_plotly",
      title_text = "SHAP Dependence (Marginal)",
      build_fn = gov_build_inta_plot
    )
    
    register_expanded_modal(
      input, output, session,
      btn_id = "gov_expand_eff_btn",
      mode_id = "gov_eff_expand_mode",
      ui_id = "gov_eff_expanded_ui",
      plot_static_id = "gov_plot_eff_exp",
      plot_plotly_id = "gov_plot_eff_exp_plotly",
      title_text = "Functional Effect",
      build_fn = gov_build_eff_plot
    )
    
    register_expanded_modal(
      input, output, session,
      btn_id = "gov_expand_intb_btn",
      mode_id = "gov_intb_expand_mode",
      ui_id = "gov_intb_expanded_ui",
      plot_static_id = "gov_plot_intb_exp",
      plot_plotly_id = "gov_plot_intb_exp_plotly",
      title_text = "Feature vs Target Scatterplot",
      build_fn = gov_build_intb_plot
    )
    
  })
}
