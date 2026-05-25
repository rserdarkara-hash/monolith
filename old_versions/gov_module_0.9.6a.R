# gov_module_0.9.5.R - Modularized Governing Factors Tab






# --- UI Module ---
gov_factors_ui <- function(id) {
  ns <- shiny::NS(id)
  
  shiny::tagList(
    shiny::div(style = "padding: 10px;",
      shiny::fluidRow(
        shiny::column(3,
          shiny::h4("Analysis Configuration"),
          shiny::uiOutput(ns("gov_target_ui")),
          shiny::uiOutput(ns("gov_predictors_ui")),
          shiny::sliderInput(ns("gov_permutations"), "Permutations (for RF importance)", min = 10, max = 100, value = 50, step = 10),
          shiny::actionButton(ns("gov_run_btn"), "Run Analysis", class = "btn-primary btn-block"),
          shiny::hr(),
          shiny::h4("Plot Settings"),
          shiny::radioButtons(ns("gov_effect_type"), "Functional Effect Plot:", choices = c("ALE" = "ale", "PDP" = "pdp"), inline = TRUE)
        ),
        shiny::column(9,
          # Namespace-aware conditional panel
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
                shiny::h4("Causality / Interaction (A)"),
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
                shiny::h4("Causality / Interaction (B)"),
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

# --- Server Module ---
gov_factors_server <- function(id, data_reactive, vars_metadata_reactive) {
  shiny::moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    # Internal state: decoupled from main server environment
    gov_rv <- shiny::reactiveValues(res = NULL, ready = FALSE)
    
    # Helper to retrieve metadata-defined label or original name
    gov_get_label <- function(v) {
      vars_metadata <- vars_metadata_reactive()
      if (is.null(vars_metadata)) return(v)
      match <- Filter(function(x) x$actual == v, vars_metadata)
      if (length(match) > 0 && !is.null(match[[1]]$label) && match[[1]]$label != "") {
        match[[1]]$label
      } else {
        v
      }
    }
    
    # Generate Target Parameter dropdown
    output$gov_target_ui <- shiny::renderUI({
      df <- data_reactive()
      shiny::req(df)
      cols <- colnames(df)
      num_cols <- cols[sapply(df, is.numeric)]
      
      num_named <- sapply(num_cols, gov_get_label)
      names(num_cols) <- num_named
      
      shiny::selectInput(ns("gov_target"), "Target Parameter", choices = num_cols)
    })
    
    # Generate Predictors picker
    output$gov_predictors_ui <- shiny::renderUI({
      df <- data_reactive()
      shiny::req(df)
      cols <- colnames(df)
      num_cols <- cols[sapply(df, is.numeric)]
      
      num_named <- sapply(num_cols, gov_get_label)
      names(num_cols) <- num_named
      
      shinyWidgets::pickerInput(
        ns("gov_predictors"), "Governing Factors", 
        choices = num_cols, multiple = TRUE, 
        options = list(`actions-box` = TRUE)
      )
    })
    
    # Run random forest analysis
    shiny::observeEvent(input$gov_run_btn, {
      df <- data_reactive()
      shiny::req(df, input$gov_target, input$gov_predictors)
      
      # Exclude target from predictors if mistakenly selected
      preds <- setdiff(input$gov_predictors, input$gov_target)
      
      if (length(preds) < 1 || nrow(df) < 10) {
        shiny::showNotification("Insufficient data or predictors for analysis.", type = "error")
        return()
      }
      
      shiny::withProgress(message = 'Calculating Governing Factors...', value = 0, {
        shiny::incProgress(0.2, detail = "Fitting Random Forest...")
        res <- compute_governing_factors(
          df, 
          target_col = input$gov_target, 
          predictors = preds, 
          n_permutations = input$gov_permutations
        )
        shiny::incProgress(0.8, detail = "Extracting ML Explanations...")
        
        if (!is.null(res)) {
          gov_rv$res <- res
          gov_rv$ready <- TRUE
        } else {
          gov_rv$ready <- FALSE
          shiny::showNotification("Failed to calculate governing factors. Check data quality.", type = "error")
        }
      })
    })
    
    # Ready status exposed to UI conditionalPanel
    output$gov_ready <- shiny::reactive({
      if (isTRUE(gov_rv$ready)) "yes" else "no"
    })
    shiny::outputOptions(output, "gov_ready", suspendWhenHidden = FALSE)
    
    # Global Importance Plot
    output$gov_plot_importance <- shiny::renderPlot({
      shiny::req(gov_rv$res)
      vip_df <- gov_rv$res$importance
      vip_df <- vip_df[order(vip_df$dropout_loss, decreasing = FALSE), ]
      
      vip_df$variable_label <- sapply(as.character(vip_df$variable), gov_get_label)
      vip_df$variable_label <- factor(vip_df$variable_label, levels = vip_df$variable_label)
      
      ggplot2::ggplot(vip_df, ggplot2::aes(x = variable_label, y = dropout_loss)) +
        ggplot2::geom_bar(stat = "identity", fill = "steelblue") +
        ggplot2::coord_flip() +
        ggplot2::labs(title = "Global Variable Importance", x = "Variable", y = "Dropout Loss (RMSE increase)") +
        ggplot2::theme_minimal()
    })
    
    # Causality / Interaction (A)
    output$gov_plot_interaction_a <- shiny::renderPlot({
      shiny::req(gov_rv$res)
      shap_df <- gov_rv$res$shap
      top_var_label <- gov_get_label(gov_rv$res$top_var)
      
      ggplot2::ggplot(shap_df, ggplot2::aes(x = feature_value, y = contribution)) +
        ggplot2::geom_point(color = "darkred", alpha = 0.6) +
        ggplot2::geom_smooth(method = "loess", color = "blue", se = FALSE) +
        ggplot2::labs(title = paste("SHAP Dependence:", top_var_label), x = paste(top_var_label, "Value"), y = "SHAP Contribution") +
        ggplot2::theme_minimal()
    })
    
    # Functional Effect Plot
    output$gov_plot_effect <- shiny::renderPlot({
      shiny::req(gov_rv$res)
      top_var_label <- gov_get_label(gov_rv$res$top_var)
      
      if (!is.null(input$gov_effect_type) && input$gov_effect_type == "ale") {
        ale_df <- gov_rv$res$ale
        ggplot2::ggplot(ale_df, ggplot2::aes(x = `_x_`, y = `_yhat_`)) +
          ggplot2::geom_line(color = "purple", linewidth = 1) +
          ggplot2::labs(title = paste("ALE Profile:", top_var_label), x = top_var_label, y = "ALE Effect") +
          ggplot2::theme_minimal()
      } else {
        pdp_df <- gov_rv$res$pdp
        ggplot2::ggplot(pdp_df, ggplot2::aes(x = `_x_`, y = `_yhat_`)) +
          ggplot2::geom_line(color = "darkorange", linewidth = 1) + 
          ggplot2::geom_rug(sides = "b", alpha = 0.3) +
          ggplot2::labs(title = paste("PDP Profile:", top_var_label), x = top_var_label, y = "Partial Dependence") +
          ggplot2::theme_minimal()
      }
    })
    
    # Causality / Interaction (B)
    output$gov_plot_interaction_b <- shiny::renderPlot({
      shiny::req(gov_rv$res, input$gov_target)
      df <- data_reactive()
      shiny::req(df)
      top_var_label <- gov_get_label(gov_rv$res$top_var)
      target_label <- gov_get_label(input$gov_target)
      
      if (gov_rv$res$top_var %in% colnames(df) && input$gov_target %in% colnames(df)) {
        ggplot2::ggplot(df, ggplot2::aes(x = .data[[gov_rv$res$top_var]], y = .data[[input$gov_target]])) +
          ggplot2::geom_point(alpha = 0.5) +
          ggplot2::geom_smooth(method = "lm", color = "red") +
          ggplot2::labs(title = paste("Target vs Top Factor:", top_var_label), x = top_var_label, y = target_label) +
          ggplot2::theme_minimal()
      } else {
        ggplot2::ggplot() + ggplot2::annotate("text", x=0, y=0, label="Data not available") + ggplot2::theme_void()
      }
    })
    
    # Tabular Data Metrics
    output$gov_summary_table <- DT::renderDataTable({
      shiny::req(gov_rv$res)
      vip_df <- gov_rv$res$importance
      vip_df <- vip_df[order(vip_df$dropout_loss, decreasing = TRUE), ]
      
      vip_df$variable <- sapply(as.character(vip_df$variable), gov_get_label)
      colnames(vip_df) <- c("Governing Factor", "Importance (Dropout Loss)")
      
      DT::datatable(vip_df, options = list(pageLength = 5, dom = 't', scrollX = TRUE), rownames = FALSE)
    })
    
    # --- Expanded View Reusable Plot Builders ---
    gov_build_imp_plot <- function() {
      vip_df <- gov_rv$res$importance
      vip_df <- vip_df[order(vip_df$dropout_loss, decreasing = FALSE), ]
      vip_df$variable_label <- sapply(as.character(vip_df$variable), gov_get_label)
      vip_df$variable_label <- factor(vip_df$variable_label, levels = vip_df$variable_label)
      
      ggplot2::ggplot(vip_df, ggplot2::aes(x = variable_label, y = dropout_loss)) + 
        ggplot2::geom_bar(stat = "identity", fill = "steelblue") +
        ggplot2::coord_flip() + 
        ggplot2::labs(title = "Global Variable Importance", x = "Variable", y = "Dropout Loss (RMSE increase)") + 
        ggplot2::theme_minimal(base_size = 16)
    }
    
    gov_build_inta_plot <- function() {
      shap_df <- gov_rv$res$shap
      top_var_label <- gov_get_label(gov_rv$res$top_var)
      
      ggplot2::ggplot(shap_df, ggplot2::aes(x = feature_value, y = contribution)) + 
        ggplot2::geom_point(color = "darkred", alpha = 0.6, size = 3) +
        ggplot2::geom_smooth(method = "loess", color = "blue", se = FALSE, linewidth = 1.5) +
        ggplot2::labs(title = paste("SHAP Dependence:", top_var_label), x = paste(top_var_label, "Value"), y = "SHAP Contribution") + 
        ggplot2::theme_minimal(base_size = 16)
    }
    
    gov_build_eff_plot <- function() {
      top_var_label <- gov_get_label(gov_rv$res$top_var)
      
      if (!is.null(input$gov_effect_type) && input$gov_effect_type == "ale") {
        ale_df <- gov_rv$res$ale
        ggplot2::ggplot(ale_df, ggplot2::aes(x = `_x_`, y = `_yhat_`)) + 
          ggplot2::geom_line(color = "purple", linewidth = 2) +
          ggplot2::labs(title = paste("ALE Profile:", top_var_label), x = top_var_label, y = "ALE Effect") + 
          ggplot2::theme_minimal(base_size = 16)
      } else {
        pdp_df <- gov_rv$res$pdp
        ggplot2::ggplot(pdp_df, ggplot2::aes(x = `_x_`, y = `_yhat_`)) + 
          ggplot2::geom_line(color = "darkorange", linewidth = 2) + 
          ggplot2::geom_rug(sides = "b", alpha = 0.3) +
          ggplot2::labs(title = paste("PDP Profile:", top_var_label), x = top_var_label, y = "Partial Dependence") + 
          ggplot2::theme_minimal(base_size = 16)
      }
    }
    
    gov_build_intb_plot <- function() {
      df <- data_reactive()
      shiny::req(df)
      top_var_label <- gov_get_label(gov_rv$res$top_var)
      target_label <- gov_get_label(input$gov_target)
      
      if (gov_rv$res$top_var %in% colnames(df) && input$gov_target %in% colnames(df)) {
        ggplot2::ggplot(df, ggplot2::aes(x = .data[[gov_rv$res$top_var]], y = .data[[input$gov_target]])) +
          ggplot2::geom_point(alpha = 0.5, size = 3) + 
          ggplot2::geom_smooth(method = "lm", color = "red", linewidth = 1.5) +
          ggplot2::labs(title = paste("Target vs Top Factor:", top_var_label), x = top_var_label, y = target_label) + 
          ggplot2::theme_minimal(base_size = 16)
      } else {
        ggplot2::ggplot() + ggplot2::annotate("text", x=0, y=0, label="Data not available") + ggplot2::theme_void()
      }
    }
    
    # --- Expanded Modal View Handlers ---
    # Global Importance
    shiny::observeEvent(input$gov_expand_imp_btn, {
      shiny::showModal(shiny::modalDialog(
        title = "Expanded View: Global Importance", size = "l", easyClose = TRUE,
        shiny::radioButtons(ns("gov_imp_expand_mode"), "View Mode:", choices = c("Static (High-Res)" = "static", "Interactive (Hover/Zoom)" = "interactive"), inline = TRUE),
        shiny::uiOutput(ns("gov_imp_expanded_ui")),
        footer = shiny::modalButton("Close")
      ))
    })
    output$gov_imp_expanded_ui <- shiny::renderUI({
      if (!is.null(input$gov_imp_expand_mode) && input$gov_imp_expand_mode == "interactive") {
        plotly::plotlyOutput(ns("gov_plot_imp_exp_plotly"), height = "700px")
      } else {
        shiny::plotOutput(ns("gov_plot_imp_exp"), height = "700px")
      }
    })
    output$gov_plot_imp_exp <- shiny::renderPlot({ shiny::req(gov_rv$res); gov_build_imp_plot() })
    output$gov_plot_imp_exp_plotly <- plotly::renderPlotly({ shiny::req(gov_rv$res); plotly::ggplotly(gov_build_imp_plot()) })
    
    # Interaction A
    shiny::observeEvent(input$gov_expand_inta_btn, {
      shiny::showModal(shiny::modalDialog(
        title = "Expanded View: Interaction (A)", size = "l", easyClose = TRUE,
        shiny::radioButtons(ns("gov_inta_expand_mode"), "View Mode:", choices = c("Static (High-Res)" = "static", "Interactive (Hover/Zoom)" = "interactive"), inline = TRUE),
        shiny::uiOutput(ns("gov_inta_expanded_ui")),
        footer = shiny::modalButton("Close")
      ))
    })
    output$gov_inta_expanded_ui <- shiny::renderUI({
      if (!is.null(input$gov_inta_expand_mode) && input$gov_inta_expand_mode == "interactive") {
        plotly::plotlyOutput(ns("gov_plot_inta_exp_plotly"), height = "700px")
      } else {
        shiny::plotOutput(ns("gov_plot_inta_exp"), height = "700px")
      }
    })
    output$gov_plot_inta_exp <- shiny::renderPlot({ shiny::req(gov_rv$res); gov_build_inta_plot() })
    output$gov_plot_inta_exp_plotly <- plotly::renderPlotly({ shiny::req(gov_rv$res); plotly::ggplotly(gov_build_inta_plot()) })
    
    # Functional Effect
    shiny::observeEvent(input$gov_expand_eff_btn, {
      shiny::showModal(shiny::modalDialog(
        title = "Expanded View: Functional Effect", size = "l", easyClose = TRUE,
        shiny::radioButtons(ns("gov_eff_expand_mode"), "View Mode:", choices = c("Static (High-Res)" = "static", "Interactive (Hover/Zoom)" = "interactive"), inline = TRUE),
        shiny::uiOutput(ns("gov_eff_expanded_ui")),
        footer = shiny::modalButton("Close")
      ))
    })
    output$gov_eff_expanded_ui <- shiny::renderUI({
      if (!is.null(input$gov_eff_expand_mode) && input$gov_eff_expand_mode == "interactive") {
        plotly::plotlyOutput(ns("gov_plot_eff_exp_plotly"), height = "700px")
      } else {
        shiny::plotOutput(ns("gov_plot_eff_exp"), height = "700px")
      }
    })
    output$gov_plot_eff_exp <- shiny::renderPlot({ shiny::req(gov_rv$res); gov_build_eff_plot() })
    output$gov_plot_eff_exp_plotly <- plotly::renderPlotly({ shiny::req(gov_rv$res); plotly::ggplotly(gov_build_eff_plot()) })
    
    # Interaction B
    shiny::observeEvent(input$gov_expand_intb_btn, {
      shiny::showModal(shiny::modalDialog(
        title = "Expanded View: Interaction (B)", size = "l", easyClose = TRUE,
        shiny::radioButtons(ns("gov_intb_expand_mode"), "View Mode:", choices = c("Static (High-Res)" = "static", "Interactive (Hover/Zoom)" = "interactive"), inline = TRUE),
        shiny::uiOutput(ns("gov_intb_expanded_ui")),
        footer = shiny::modalButton("Close")
      ))
    })
    output$gov_intb_expanded_ui <- shiny::renderUI({
      if (!is.null(input$gov_intb_expand_mode) && input$gov_intb_expand_mode == "interactive") {
        plotly::plotlyOutput(ns("gov_plot_intb_exp_plotly"), height = "700px")
      } else {
        shiny::plotOutput(ns("gov_plot_intb_exp"), height = "700px")
      }
    })
    output$gov_plot_intb_exp <- shiny::renderPlot({ shiny::req(gov_rv$res, input$gov_target); gov_build_intb_plot() })
    output$gov_plot_intb_exp_plotly <- plotly::renderPlotly({ shiny::req(gov_rv$res, input$gov_target); plotly::ggplotly(gov_build_intb_plot()) })
    
  })
}
