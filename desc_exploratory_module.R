
# Caveat shown with the Contribution / cos2 controls when the PCA was run
# without scaling: both diagnostics implicitly assume unit-variance inputs,
# so unscaled results are dominated by the high-variance variables.
unscaled_pca_note <- function() {
  shiny::helpText(
    shiny::icon("info-circle"),
    "PCA was run without scaling: contribution and cos2 values are dominated by high-variance variables and are not comparable across variables measured on different scales."
  )
}

compute_normality <- function(x) {
  default_res <- list(
    status = "insufficient",
    method = "None",
    statistic = NA,
    p_value = NA,
    n = 0
  )
  
  if (is.null(x) || !is.numeric(x)) {
    return(default_res)
  }
  
  clean_x <- x[!is.na(x)]
  n <- length(clean_x)
  default_res$n <- n
  
  if (n < 3) {
    return(default_res)
  }
  
  if (var(clean_x) == 0) {
    return(default_res)
  }
  
  tryCatch({
    if (n < 5000) {
      test_res <- shapiro.test(clean_x)
      method_name <- "Shapiro-Wilk Normality Test"
    } else {
      test_res <- nortest::lillie.test(clean_x)
      method_name <- "Lilliefors (Kolmogorov-Smirnov) Normality Test"
    }
    stat_val <- unname(test_res$statistic)
    
    p_val <- test_res$p.value
    status_val <- if (p_val >= 0.05) "normal" else "not_normal"
    
    list(
      status = status_val,
      method = method_name,
      statistic = stat_val,
      p_value = p_val,
      n = n
    )
  }, error = function(e) {
    warning("Normality computation failed: ", e$message)
    default_res
  })
}

desc_exploratory_ui <- function(id) {
  ns <- shiny::NS(id)
  
  shiny::tagList(
    shiny::div(style = "padding: 20px;",
        shiny::h2("Analytics Engine"),
        shiny::p("Explore your data with descriptive statistics, correlation mapping, and principal component analysis. Investigate governing factors on a specific parameter."),
        shinyWidgets::radioGroupButtons(ns("name_mode"), "Variable naming:",
                            choices = c("Variable labels" = "label", "Column names" = "colname"),
                            selected = "label", size = "sm"),
        shiny::hr(),
        shiny::fluidRow(
          shiny::column(12,
            shiny::wellPanel(
              shiny::h4("Data Grouping & Discretization"),
              shiny::fluidRow(
                shiny::column(6, shiny::selectInput(ns("analytics_group_vars"), "Grouping Variables (Max 5)", choices = NULL, multiple = TRUE)),
                shiny::column(6, shiny::uiOutput(ns("analytics_group_types_ui")))
              ),
              shiny::uiOutput(ns("analytics_group_filter_ui"))
            )
          )
        ),
        shiny::hr(),
        shiny::tabsetPanel(id = ns("scientific_analytics_tabs"),
          shiny::tabPanel("Descriptive Suite",
            shiny::div(style = "padding: 10px;",
              shiny::fluidRow(
                shiny::column(3,
                  shiny::selectInput(ns("desc_plot_type"), "Plot Type", 
                    choices = c("Histogram" = "histogram", 
                                "Density" = "density", 
                                "Boxplot" = "boxplot", 
                                "Violin" = "violin", 
                                "Scatterplot" = "scatter", 
                                "ECDF" = "ecdf",
                                "QQ Plot" = "qq",
                                "Sina-style Plot" = "sinaplot",
                                "Ridge/Joyplot" = "ridge",
                                "2D Density Heatmap" = "density_heatmap",
                                "Parallel Coordinates" = "parallel",
                                "Radar Chart" = "radar",
                                "XYZ Surface" = "xyz_surface")),
                  shiny::checkboxInput(ns("desc_ghosting"), "Enable Ghosting (Selected vs. Total)", value = FALSE),
                  shiny::selectInput(ns("desc_palette"), "Color Palette",
                    choices = desc_palette_choices),
                  shiny::uiOutput(ns("desc_plot_vars_ui"))
                ),
                shiny::column(9,
                  shiny::div(style = "position: relative;",
                      shiny::tags$button(id = ns("desc_expand_plot_btn"), type = "button", class = "btn btn-default action-button expand-icon-btn", shiny::icon("expand")),
                      shiny::plotOutput(ns("desc_main_plot"), height = "500px")
                  ),
                  shiny::hr(),
                  shiny::h4("Group Statistics"),
                  DT::dataTableOutput(ns("desc_summary_table"))
                )
              )
            )
          ),
          shiny::tabPanel("Correlation Analysis",
            shiny::div(style = "padding: 10px;",
              shiny::fluidRow(
                shiny::column(3,
                  shiny::selectInput(ns("corr_plot_type"), "Correlation Plot Type", 
                    choices = c("Hierarchical Heatmap" = "heatmap",
                                "Correlation Network" = "network",
                                "Partial Correlation" = "partial",
                                "Correlogram" = "correlogram",
                                "Lagged CCF" = "lagged")),
                  shiny::selectInput(ns("corr_method"), "Method", choices = c("pearson", "spearman", "kendall")),
                  shiny::uiOutput(ns("corr_vars_ui"))
                ),
                shiny::column(9,
                  shiny::div(style = "position: relative;",
                      shiny::tags$button(id = ns("corr_expand_plot_btn"), type = "button", class = "btn btn-default action-button expand-icon-btn", shiny::icon("expand")),
                      shiny::plotOutput(ns("corr_main_plot"), height = "500px")
                  ),
                  shiny::hr(),
                  shiny::h4("Correlation Matrix"),
                  DT::dataTableOutput(ns("corr_summary_table"))
                )
              )
            )
          ),
          shiny::tabPanel("PCA",
            shiny::div(style = "padding: 10px;",
              shiny::fluidRow(
                shiny::column(3,
                  shiny::h4("PCA Setup"),
                  shiny::uiOutput(ns("pca_vars_ui")),
                  shiny::actionButton(ns("run_pca_btn"), "Run PCA", class="btn-primary btn-block"),
                  shiny::hr(),
                  shiny::conditionalPanel(
                    condition = sprintf("input['%s'] == 'yes'", ns("pca_ready_flag")),
                    shiny::selectInput(ns("pca_plot_type"), "Plot Type",
                      choices = c("Scree Plot" = "scree",
                                  "Biplot (2D)" = "biplot",
                                  "Biplot (3D)" = "3d_biplot",
                                  "Loadings" = "loadings",
                                  "Contribution" = "contrib",
                                  "Quality of Rep. (Cos2)" = "cos2",
                                  "Cumulative Variance" = "cumvar",
                                  "Mahalanobis Distance" = "mahalanobis")),
                    shiny::uiOutput(ns("pca_plot_controls"))
                  )
                ),
                shiny::column(9,
                  shiny::uiOutput(ns("pca_collinearity_warning_ui")),
                  shiny::div(style = "position: relative;",
                      shiny::tags$button(id = ns("pca_expand_plot_btn"), type = "button", class = "btn btn-default action-button expand-icon-btn", shiny::icon("expand")),
                      shiny::uiOutput(ns("pca_main_plot_container"))
                  ),
                  shiny::hr(),
                  shiny::conditionalPanel(
                    condition = sprintf("input['%s'] == 'yes'", ns("pca_ready_flag")),
                    shiny::h4("PCA Results"),
                    DT::dataTableOutput(ns("pca_summary_table"))
                  )
                )
              )
            )
          ),
          shiny::tabPanel("Governing Factors",
            gov_factors_ui(ns("gov"))
          )
        ),
        shiny::conditionalPanel("false", shiny::textInput(ns("pca_ready_flag"), "", value = "no"))
    )
  )
}

desc_exploratory_server <- function(id, data_reactive, vars_metadata_reactive) {
  shiny::moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # Naming-mode switch: "label" feeds the uploaded variable metadata to every
    # dropdown/plot/table builder; "colname" feeds NULL, which makes
    # get_var_label()/apply_labels_to_df() fall back to raw column names.
    vmeta <- shiny::reactive({
      if (identical(input$name_mode, "colname")) NULL else vars_metadata_reactive()
    })

    shiny::observe({
      req(data_reactive())
      df <- data_reactive()
      cols <- colnames(df)
      valid_cols <- cols[!is_coord_col(cols)]
      
      vars_metadata <- vmeta()
      if (!is.null(vars_metadata)) {
        choices_named <- setNames(valid_cols, get_var_labels(valid_cols, vars_metadata))
      } else {
        choices_named <- valid_cols
      }
      
      curr_sel <- intersect(shiny::isolate(input$analytics_group_vars), choices_named)
      shiny::updateSelectInput(session, "analytics_group_vars", choices = choices_named, selected = curr_sel)
    })
    
    output$analytics_group_types_ui <- shiny::renderUI({
      req(input$analytics_group_vars, data_reactive())
      vars <- input$analytics_group_vars
      df <- data_reactive()
      lapply(seq_along(vars), function(i) {
        v <- vars[i]
        is_num <- is.numeric(df[[v]]) && length(unique(na.omit(df[[v]]))) > 10
        
        shiny::div(style="margin-bottom: 5px;",
            shiny::selectInput(ns(paste0("grp_type_", i)), paste("Type/Binning for:", v),
                         choices = c("Categorical" = "categorical", 
                                     "Numeric: Median" = "numeric_median",
                                     "Numeric: Mean" = "numeric_mean",
                                     "Numeric: Tertiles" = "numeric_tertiles",
                                     "Numeric: Quintiles" = "numeric_quintiles"),
                         selected = if(is_num) "numeric_median" else "categorical")
        )
      })
    })
    
    rv_analytics_data <- shiny::reactive({
      req(data_reactive())
      df <- data_reactive()
      vars <- input$analytics_group_vars
      
      if (is.null(vars) || length(vars) == 0) {
         df$group_id <- as.factor("All")
         return(df)
      }
      
      types <- sapply(seq_along(vars), function(i) {
        v <- vars[i]
        def <- if(is.numeric(df[[v]]) && length(unique(na.omit(df[[v]]))) > 10) "numeric_median" else "categorical"
        input[[paste0("grp_type_", i)]] %||% def
      })
      
      shiny::withProgress(message = "Applying Discretization and Grouping...", value = 0.5, {
         res <- process_grouping_vars(df, vars, types)
         res
      })
    })
    
    rv_filtered_analytics_data <- shiny::reactive({
      req(rv_analytics_data())
      df_local <- rv_analytics_data()
      active_groups <- input$analytics_active_group
      filter_active_groups(df_local, active_groups)
    })
    
    output$analytics_group_filter_ui <- shiny::renderUI({
      req(rv_analytics_data())
      df <- rv_analytics_data()
      if ("group_id" %in% colnames(df)) {
        levels_present <- levels(df$group_id)
        curr_sel <- intersect(shiny::isolate(input$analytics_active_group), levels_present)
        if (length(curr_sel) == 0) curr_sel <- levels_present
        shiny::selectInput(ns("analytics_active_group"), "Select Active Groups to Compare",
                    choices = levels_present, multiple = TRUE, selected = curr_sel)
      }
    })
    
    desc_vars_state <- shiny::reactiveValues(x = "", y = "", z = "", multi = NULL)
    
    output$desc_plot_vars_ui <- shiny::renderUI({
      req(data_reactive())
      df <- data_reactive()
      cols <- colnames(df)
      num_cols <- cols[sapply(df, is.numeric)]
      valid_cols <- cols[!is_coord_col(cols)]
      
      vars_metadata <- vmeta()
      if (!is.null(vars_metadata)) {
        valid_named <- setNames(valid_cols, get_var_labels(valid_cols, vars_metadata))
        num_named <- setNames(num_cols, get_var_labels(num_cols, vars_metadata))
      } else {
        valid_named <- valid_cols
        num_named <- num_cols
      }
      
      p_type <- input$desc_plot_type %||% "histogram"
      # isolate: selections are restored on re-render (plot type / data change) but
      # must not themselves invalidate this renderUI, or every click rebuilds the
      # inputs mid-interaction and fights the user (add/remove feedback loop)
      sel_state <- shiny::isolate(shiny::reactiveValuesToList(desc_vars_state))
      sel_x <- if(isTruthy(sel_state$x)) sel_state$x else valid_named[1]
      
      shiny::tagList(
        if (!(p_type %in% c("parallel", "radar"))) {
          shiny::div(style = "display: flex; align-items: center; gap: 5px;",
              shiny::selectInput(ns("desc_var_x"), "Primary Variable (X)", choices = valid_named, selected = sel_x, width = "calc(100% - 40px)"),
              shiny::actionButton(ns("clear_desc_vars"), "", icon = shiny::icon("times"), class = "btn-danger btn-sm", style = "margin-top: 10px;", title = "Clear selections")
          )
        },
        if (p_type %in% c("boxplot", "violin", "sinaplot", "scatter", "density_heatmap", "xyz_surface")) {
          choices_y <- if(p_type %in% c("boxplot", "violin", "sinaplot")) c("None" = "", valid_named) else valid_named
          sel_y <- if(isTruthy(sel_state$y)) sel_state$y else { if(p_type %in% c("boxplot", "violin", "sinaplot")) "" else valid_cols[2] }
          shiny::selectInput(ns("desc_var_y"), "Secondary Variable (Y)", choices = choices_y, selected = sel_y)
        },
        if (p_type %in% c("boxplot", "violin", "sinaplot")) {
          shiny::div(style="background-color: #f0f8ff; padding: 10px; border-radius: 5px; border: 1px solid #b8daff; margin-bottom: 10px;",
              shiny::h5(style="margin-top:0; color: #0056b3;",
                  "Statistical Significance Tests",
                  shiny::uiOutput(ns("desc_normality_indicator"), inline = TRUE)
              ),
              shiny::checkboxGroupInput(ns("desc_stat_tests"), 
                                 shiny::HTML("Select Test (Choose One) <span title='For non-normal data distributions, non-parametric Kruskal-Wallis is highly recommended.'>\u2139\uFE0F</span>:"), 
                                 choices = c("ANOVA" = "anova", "Duncan's" = "duncan", "Tukey's HSD" = "tukey", "Kruskal-Wallis" = "kruskal"), inline = TRUE),
              shiny::radioButtons(ns("desc_stat_letter_pos"), "Letter Placement:", choices = c("Above Data" = "above", "Top of Plot" = "top"), inline = TRUE)
          )
        },
        if (p_type %in% c("scatter")) {
          shiny::selectInput(ns("desc_scatter_fit"), "Add Trend Line", choices = c("None" = "none", "Linear (lm)" = "linear", "Loess" = "loess", "Polynomial (degree 2)" = "polynomial", "GAM" = "gam"))
        },
        if (p_type %in% c("xyz_surface")) {
          sel_z <- if(isTruthy(sel_state$z)) sel_state$z else num_cols[3]
          shiny::selectInput(ns("desc_var_z"), "Tertiary Variable (Z)", choices = num_named, selected = sel_z)
        },
        if (p_type %in% c("parallel", "radar")) {
          label_text <- ifelse(p_type == "radar", "Select Variables (Min 3)", "Select Variables (Min 2)")
          sel_m <- if(length(sel_state$multi) > 0) sel_state$multi else head(num_cols, 3)
          shiny::selectInput(ns("desc_vars_multi"), label_text, choices = num_named, multiple = TRUE, selected = sel_m)
        },
        if (p_type == "xyz_surface") {
          shiny::selectInput(ns("desc_xyz_fit"), "Surface Fit Model", 
                      choices = c("Linear" = "linear", "Loess" = "loess", "Polynomial" = "polynomial", "GAM" = "gam", "Thin Plate Splines" = "tps"))
        }
      )
    })
    
    # ignoreNULL = FALSE so deselections ("" from the None choice / Clear button,
    # NULL from emptying the multi-select) are recorded too; otherwise the isolated
    # renderUI above restores removed variables on the next plot-type change
    shiny::observeEvent(input$desc_var_x, { desc_vars_state$x <- input$desc_var_x }, ignoreNULL = FALSE)
    shiny::observeEvent(input$desc_var_y, { desc_vars_state$y <- input$desc_var_y }, ignoreNULL = FALSE)
    shiny::observeEvent(input$desc_var_z, { desc_vars_state$z <- input$desc_var_z }, ignoreNULL = FALSE)
    shiny::observeEvent(input$desc_vars_multi, { desc_vars_state$multi <- input$desc_vars_multi }, ignoreNULL = FALSE)
    output$desc_normality_indicator <- shiny::renderUI({
      req(rv_filtered_analytics_data())
      req(input$desc_var_x)
      
      p_type <- input$desc_plot_type %||% "histogram"
      if (!(p_type %in% c("boxplot", "violin", "sinaplot"))) {
        return(NULL)
      }
      
      df <- rv_filtered_analytics_data()
      var_name <- input$desc_var_x
      
      if (!(var_name %in% colnames(df)) || !is.numeric(df[[var_name]])) {
        return(NULL)
      }
      
      val <- df[[var_name]]
      
      group_breakdown <- ""
      if ("group_id" %in% colnames(df) && length(unique(df$group_id)) > 1) {
        group_results <- c()
        groups <- split(df[[var_name]], df$group_id)
        for (g_name in names(groups)) {
          g_val <- groups[[g_name]]
          g_res <- compute_normality(g_val)
          if (g_res$status == "insufficient") {
            group_results <- c(group_results, sprintf("- %s (n=%d): Insufficient data", g_name, g_res$n))
          } else {
            status_text <- if (g_res$status == "normal") "Normal" else "Not Normal"
            group_results <- c(group_results, sprintf("- %s (n=%d): p = %.4f (%s)", g_name, g_res$n, g_res$p_value, status_text))
          }
        }
        group_breakdown <- paste("\nGroup Breakdown:\n", paste(group_results, collapse = "\n"), sep = "")
      }
      
      used_residuals <- FALSE
      residual_err <- NULL
      if ("group_id" %in% colnames(df) && length(unique(df$group_id)) > 1) {
        tryCatch({
          val <- residuals(lm(val ~ group_id, data = df, na.action = na.exclude))
          used_residuals <- TRUE
        }, error = function(e) {
          residual_err <<- e$message
          warning("Residual extraction failed: ", e$message)
        })
      }
      
      res <- compute_normality(val)
      res_suffix <- if (used_residuals) " (on residuals)" else if (!is.null(residual_err)) paste0(" (on raw values - Residual extraction failed: ", residual_err, ")") else " (on raw values)"
      
      if (res$status == "insufficient") {
        icon_element <- shiny::icon("question-circle", style = "color: #6c757d; font-size: 14px; cursor: help;")
        tooltip_title <- sprintf(
          "Normality Test: Insufficient data (n = %d). Typically n >= 3 is required.%s",
          res$n,
          group_breakdown
        )
      } else if (res$status == "normal") {
        icon_element <- shiny::icon("check-circle", style = "color: #28a745; font-size: 14px; cursor: help;")
        tooltip_title <- sprintf(
          "Normality Passed: %s%s\nStatistic: %s = %.4f\np-value = %.4f\nSample Size: n = %d\nWithin-group residuals appear to be normally distributed (p >= 0.05).%s",
          res$method,
          res_suffix,
          ifelse(grepl("Shapiro-Wilk", res$method), "W", "D"),
          res$statistic,
          res$p_value,
          res$n,
          group_breakdown
        )
      } else {
        icon_element <- shiny::icon("exclamation-triangle", style = "color: #dc3545; font-size: 14px; cursor: help;")
        p_str <- if (res$p_value < 0.0001) "< 0.0001" else sprintf("= %.4f", res$p_value)
        tooltip_title <- sprintf(
          "Normality Failed: %s%s\nStatistic: %s = %.4f\np-value %s\nSample Size: n = %d\nWithin-group residuals deviate significantly from normality (p < 0.05).%s",
          res$method,
          res_suffix,
          ifelse(grepl("Shapiro-Wilk", res$method), "W", "D"),
          res$statistic,
          p_str,
          res$n,
          group_breakdown
        )
      }
      
      shiny::tags$span(
        style = "margin-left: 5px; display: inline-block; vertical-align: middle;",
        title = tooltip_title,
        icon_element
      )
    })
    
    desc_plot_obj <- shiny::reactive({
      req(rv_analytics_data())
      p_type <- input$desc_plot_type
      if (!(p_type %in% c("parallel", "radar"))) {
        req(input$desc_var_x)
      }
      
      shiny::withProgress(message = "Generating descriptive plot...", value = 0.5, {
        df_global <- data.frame(rv_analytics_data(), check.names = FALSE)
        df_local <- data.frame(rv_filtered_analytics_data(), check.names = FALSE)
      
      if (nrow(df_local) == 0) {
        p <- ggplot() + annotate("text", x=0, y=0, label="No data selected") + theme_void()
        return(p)
      }
      
      var_x_label <- get_var_label(input$desc_var_x, vmeta())
      var_y_label <- get_var_label(input$desc_var_y, vmeta())
      
      if(!is.null(input$desc_var_x) && input$desc_var_x != "") {
          colnames(df_global)[colnames(df_global) == input$desc_var_x] <- var_x_label
          colnames(df_local)[colnames(df_local) == input$desc_var_x] <- var_x_label
      }
      if(!is.null(input$desc_var_y) && input$desc_var_y != "") {
          colnames(df_global)[colnames(df_global) == input$desc_var_y] <- var_y_label
          colnames(df_local)[colnames(df_local) == input$desc_var_y] <- var_y_label
      }
      
      core_types <- c("histogram", "density", "boxplot", "violin", "scatter", "ecdf")
      
      if (p_type %in% core_types) {
        if (isTruthy(input$desc_ghosting) && nrow(df_local) < nrow(df_global)) {
          p <- generate_ghosted_plot(df_global, df_local, 
                                     var_name = var_x_label, 
                                     y_var = var_y_label, 
                                     group_col = "group_id", 
                                     plot_type = p_type)
        } else {
          p <- generate_core_plot(df_local,
                                  var_name = var_x_label,
                                  y_var = var_y_label,
                                  group_col = "group_id",
                                  plot_type = p_type,
                                  scatter_fit = input$desc_scatter_fit,
                                  stat_test = input$desc_stat_tests,
                                  stat_letter_pos = input$desc_stat_letter_pos)
        }
      } else {
        var_z_label <- get_var_label(input$desc_var_z, vmeta())
        if(!is.null(input$desc_var_z) && input$desc_var_z != "") {
            colnames(df_global)[colnames(df_global) == input$desc_var_z] <- var_z_label
            colnames(df_local)[colnames(df_local) == input$desc_var_z] <- var_z_label
        }
        
        multi_labels <- get_var_labels(input$desc_vars_multi, vmeta())
        if(!is.null(input$desc_vars_multi)) {
            df_global <- apply_labels_to_df(df_global, input$desc_vars_multi, vmeta())
            df_local <- apply_labels_to_df(df_local, input$desc_vars_multi, vmeta())
        }
        
        vars <- switch(p_type,
                       "qq" = var_x_label,
                       "sinaplot" = if(isTruthy(input$desc_var_y)) c(var_x_label, get_var_label(input$desc_var_y, vmeta())) else var_x_label,
                       "ridge" = var_x_label,
                       "density_heatmap" = c(var_x_label, var_y_label),
                       "xyz_surface" = c(var_x_label, var_y_label, var_z_label),
                       "parallel" = unname(multi_labels),
                       "radar" = unname(multi_labels),
                       var_x_label)
        
        p <- generate_advanced_plot(df_local, vars = vars, group_col = "group_id", plot_type = p_type, xyz_fit = input$desc_xyz_fit, stat_test = input$desc_stat_tests, stat_letter_pos = input$desc_stat_letter_pos)
      }
      
      # Only the XYZ surface has a continuous fill; the 2D density heatmap's
      # fill (geom_density_2d_filled) is an ordered factor, i.e. discrete
      p <- apply_desc_palette(p, input$desc_palette %||% "default",
                              continuous = identical(p_type, "xyz_surface"))

        p
      })
    })
    
    shiny::observeEvent(input$clear_desc_vars, {
      shiny::updateSelectInput(session, "desc_var_x", selected = "")
      shiny::updateSelectInput(session, "desc_var_y", selected = "")
      shiny::updateSelectInput(session, "desc_var_z", selected = "")
      shiny::updateSelectInput(session, "desc_vars_multi", selected = character(0))
    })
    
    output$desc_main_plot <- shiny::renderPlot({
      desc_plot_obj()
    })
    
    output$desc_summary_table <- DT::renderDataTable({
      req(rv_analytics_data())
      p_type <- input$desc_plot_type
      if (!(p_type %in% c("parallel", "radar"))) {
        req(input$desc_var_x)
      }
      df <- rv_filtered_analytics_data()
      
      if (nrow(df) == 0) return(NULL)
      
      if (p_type %in% c("parallel", "radar")) {
          return(data.frame(Message="Summary statistics table is not available for multi-variable plots."))
      }
      
      var <- input$desc_var_x
      if(!is.numeric(df[[var]])) return(data.frame(Message="Selected primary variable is not numeric."))
      
      # One grouped pass for all five statistics; the formula interface
      # na.omit()s beforehand, so each x arrives NA-free (same numbers as the
      # former five separate aggregate() calls).
      agg <- aggregate(df[[var]] ~ df$group_id,
                       FUN = function(x) c(n = length(x), mean = mean(x), sd = sd(x),
                                           min = min(x), max = max(x)))
      stats_mat <- agg[, 2]
      res <- data.frame(
        Group = agg[, 1],
        Count = as.integer(stats_mat[, "n"]),
        Mean = round(stats_mat[, "mean"], 3),
        SD = round(stats_mat[, "sd"], 3),
        Min = round(stats_mat[, "min"], 3),
        Max = round(stats_mat[, "max"], 3)
      )
      
      tot_mean <- mean(df[[var]], na.rm=TRUE)
      tot_sd <- sd(df[[var]], na.rm=TRUE)
      tot_n <- nrow(df[!is.na(df[[var]]), ])
      tot_min <- min(df[[var]], na.rm=TRUE)
      tot_max <- max(df[[var]], na.rm=TRUE)
      
      res <- rbind(res, data.frame(Group="TOTAL", Count=tot_n, Mean=round(tot_mean,3), SD=round(tot_sd,3), Min=round(tot_min,3), Max=round(tot_max,3)))
      
      if (input$desc_plot_type == "scatter" && !is.null(input$desc_scatter_fit) && input$desc_scatter_fit != "none") {
        y_var <- if(!is.null(input$desc_var_y) && input$desc_var_y != "") input$desc_var_y else NULL
        if (!is.null(y_var)) {
           r2_vals <- sapply(as.character(res$Group), function(g) {
              if (g == "TOTAL") sub_df <- df else sub_df <- df[as.character(df$group_id) == g,]
              if (nrow(sub_df) < 5) return(NA)
              f <- input$desc_scatter_fit
              tryCatch({
                 form_lin <- as.formula(paste0("`", y_var, "` ~ `", var, "`"))
                 form_poly <- as.formula(paste0("`", y_var, "` ~ poly(`", var, "`, 2)"))
                 form_gam <- as.formula(paste0("`", y_var, "` ~ s(`", var, "`, bs = 'cs')"))
                 
                 if (f == "linear") summary(lm(form_lin, data = sub_df))$r.squared
                 else if (f == "polynomial") { if(length(unique(sub_df[[var]])) > 3) summary(lm(form_poly, data = sub_df))$r.squared else NA }
                 else if (f == "loess") { mod <- loess(form_lin, data = sub_df, span=0.7); cor(sub_df[[y_var]], fitted(mod))^2 }
                 else if (f == "gam") { if(requireNamespace("mgcv", quietly=TRUE)) summary(mgcv::gam(form_gam, data = sub_df))$r.sq else NA }
                 else NA
              }, error = function(e) NA)
           })
           
           p_vals <- sapply(as.character(res$Group), function(g) {
              if (g == "TOTAL") sub_df <- df else sub_df <- df[as.character(df$group_id) == g,]
              if (nrow(sub_df) < 5) return(NA)
              f <- input$desc_scatter_fit
              tryCatch({
                 form_lin <- as.formula(paste0("`", y_var, "` ~ `", var, "`"))
                 form_poly <- as.formula(paste0("`", y_var, "` ~ poly(`", var, "`, 2)"))
                 form_gam <- as.formula(paste0("`", y_var, "` ~ s(`", var, "`, bs = 'cs')"))
                 
                 if (f == "linear") {
                     mod <- summary(lm(form_lin, data = sub_df))
                     if(!is.null(mod$fstatistic)) pf(mod$fstatistic[1], mod$fstatistic[2], mod$fstatistic[3], lower.tail=FALSE) else NA
                 } else if (f == "polynomial") { 
                     if(length(unique(sub_df[[var]])) > 3) {
                         mod <- summary(lm(form_poly, data = sub_df))
                         if(!is.null(mod$fstatistic)) pf(mod$fstatistic[1], mod$fstatistic[2], mod$fstatistic[3], lower.tail=FALSE) else NA
                     } else NA 
                 }
                 else if (f == "gam") { if(requireNamespace("mgcv", quietly=TRUE)) summary(mgcv::gam(form_gam, data = sub_df))$s.table[1, "p-value"] else NA }
                 else NA
              }, error = function(e) NA)
           })
           
           if (input$desc_scatter_fit == "loess") {
               res$`Squared Correlation (Not true R²)` <- round(as.numeric(r2_vals), 3)
           } else {
               res$Trend_R2 <- round(as.numeric(r2_vals), 3)
           }
           res$Trend_PVal <- format.pval(as.numeric(p_vals), digits = 3, eps = 0.001)
        }
      }
      
      DT::datatable(res, options = list(pageLength = 10, dom = 'tip', scrollX = TRUE))
    })
    
    output$corr_vars_ui <- shiny::renderUI({
      req(data_reactive())
      df <- data_reactive()
      cols <- colnames(df)
      num_cols <- cols[sapply(df, is.numeric)]
      
      vars_metadata <- vmeta()
      num_named <- if (!is.null(vars_metadata)) {
        setNames(num_cols, get_var_labels(num_cols, vars_metadata))
      } else { num_cols }
      
      p_type <- input$corr_plot_type %||% "heatmap"
      curr_multi <- isolate(input$corr_vars_multi)
      if (is.null(curr_multi) || length(curr_multi) == 0) curr_multi <- head(num_cols, 5)
      
      curr_var1 <- isolate(input$corr_var_1) %||% num_cols[1]
      curr_var2 <- isolate(input$corr_var_2) %||% (if(length(num_cols) > 1) num_cols[2] else num_cols[1])
      
      if (p_type == "lagged") {
        shiny::tagList(
          shiny::selectInput(ns("corr_var_1"), "Primary Variable", choices = num_named, selected = curr_var1),
          shiny::selectInput(ns("corr_var_2"), "Secondary Variable", choices = num_named, selected = curr_var2),
          shiny::numericInput(ns("corr_max_lag"), "Max Lag", value = 10, min = 1, max = 100)
        )
      } else {
        shiny::tagList(
          shiny::selectInput(ns("corr_vars_multi"), "Select Variables (Min 2)", choices = num_named, multiple = TRUE, selected = curr_multi),
          if (p_type == "partial") {
            curr_control <- isolate(input$corr_vars_control)
            shiny::selectInput(ns("corr_vars_control"), "Control Variables (Partial Out)", choices = num_named, multiple = TRUE, selected = curr_control)
          },
          if (p_type == "network") {
            curr_thresh <- isolate(input$corr_net_thresh) %||% 0.3
            shiny::numericInput(ns("corr_net_thresh"), "Correlation Threshold", value = curr_thresh, min = 0, max = 1, step = 0.05)
          }
        )
      }
    })
    
    corr_matrix_reactive <- shiny::reactive({
      req(rv_analytics_data())
      df <- rv_filtered_analytics_data()
      vars <- input$corr_vars_multi
      req(vars)
      if (length(vars) < 2) return(NULL)
      method <- input$corr_method %||% "pearson"
      df_labeled <- apply_labels_to_df(df, vars, vmeta())
      vars_lab <- get_var_labels(vars, vmeta())
      df_clean <- na.omit(df_labeled[, vars_lab, drop=FALSE])
      if (nrow(df_clean) < 3) return(NULL)
      cor(df_clean, method = method)
    })
    
    corr_plot_obj <- shiny::reactive({
      req(rv_analytics_data())
      df <- rv_filtered_analytics_data()
      
      if (nrow(df) == 0) {
        p <- ggplot() + annotate("text", x=0, y=0, label="No data selected") + theme_void()
        return(p)
      }
      
      p_type <- input$corr_plot_type
      method <- input$corr_method %||% "pearson"
      
      if (p_type == "lagged") {
        req(input$corr_var_1, input$corr_var_2)
        v1_lab <- get_var_label(input$corr_var_1, vmeta())
        v2_lab <- get_var_label(input$corr_var_2, vmeta())
        colnames(df)[colnames(df) == input$corr_var_1] <- v1_lab
        colnames(df)[colnames(df) == input$corr_var_2] <- v2_lab
        p <- generate_lagged_correlation(df, v1_lab, v2_lab, max_lag = input$corr_max_lag %||% 10)
      } else {
        req(input$corr_vars_multi)
        vars <- input$corr_vars_multi
        if (length(vars) < 2) return(ggplot() + annotate("text", x=0, y=0, label="Need >=2 variables"))
        
        df <- apply_labels_to_df(df, vars, vmeta())
        vars_lab <- get_var_labels(vars, vmeta())
        
        if (p_type == "heatmap") {
          p <- generate_correlation_heatmap(df, vars_lab, method = method, cormat = corr_matrix_reactive())
        } else if (p_type == "network") {
          p <- generate_correlation_network(df, vars_lab, threshold = input$corr_net_thresh %||% 0.3, method = method, cormat = corr_matrix_reactive())
        } else if (p_type == "partial") {
          c_vars <- input$corr_vars_control
          if(!is.null(c_vars) && length(c_vars) > 0) {
             df <- apply_labels_to_df(df, c_vars, vmeta())
             c_vars_lab <- get_var_labels(c_vars, vmeta())
          } else {
             c_vars_lab <- NULL
          }
          p <- generate_partial_correlation(df, vars_lab, control_vars = c_vars_lab, method = method)
        } else if (p_type == "correlogram") {
          p <- generate_correlogram(df, vars_lab, method = method, cormat = corr_matrix_reactive())
        }
      }
      return(p)
    })
    
    output$corr_main_plot <- shiny::renderPlot({
      corr_plot_obj()
    })
    
    output$corr_summary_table <- DT::renderDataTable({
      req(rv_analytics_data())
      df <- rv_filtered_analytics_data()
      
      if (nrow(df) < 3) return(NULL)
      
      p_type <- input$corr_plot_type
      method <- input$corr_method %||% "pearson"
      
      if (p_type == "lagged") {
        req(input$corr_var_1, input$corr_var_2)
        v1 <- input$corr_var_1
        v2 <- input$corr_var_2
        df_clean <- na.omit(df[, c(v1, v2)])
        if(nrow(df_clean) < 3) return(NULL)
        showNotification("Note: Lagged Cross-Correlation (CCF) is designed for sequentially ordered time-series. If data is strictly spatial or cross-sectional, interpretations may be invalid.", type = "warning", duration = 8)
        max_lag <- input$corr_max_lag %||% 10
        ccf_res <- ccf(df_clean[[v1]], df_clean[[v2]], lag.max = max_lag, plot = FALSE)
        res_df <- data.frame(Lag = ccf_res$lag[,1,1], CrossCorrelation = round(ccf_res$acf[,1,1], 3))
        return(DT::datatable(res_df, options = list(pageLength = 10, dom = 't', scrollX = TRUE)))
      } else {
        req(input$corr_vars_multi)
        vars <- input$corr_vars_multi
        if (length(vars) < 2) return(NULL)
        
        df <- apply_labels_to_df(df, vars, vmeta())
        vars_lab <- get_var_labels(vars, vmeta())
        
        n_controls <- 0
        if (p_type == "partial") {
          c_vars <- input$corr_vars_control
          if(!is.null(c_vars) && length(c_vars) > 0) {
             df <- apply_labels_to_df(df, c_vars, vmeta())
             c_vars_lab <- get_var_labels(c_vars, vmeta())

             all_vars <- unique(c(vars_lab, c_vars_lab))
             df_clean <- na.omit(df[, all_vars, drop=FALSE])
             if(nrow(df_clean) < 5) return(NULL)

             res_list <- list()
             for (v in vars_lab) {
               mod <- try(lm(as.formula(paste0("`", v, "` ~ ", paste(paste0("`", c_vars_lab, "`"), collapse=" + "))), data=df_clean), silent=TRUE)
               if(!inherits(mod, "try-error")) res_list[[v]] <- residuals(mod)
             }
             if(length(res_list) < length(vars_lab)) {
                 # Never fall back to raw correlations while the table is
                 # labelled partial: abort and tell the user which variables
                 # could not be residualized.
                 failed_vars <- setdiff(vars_lab, names(res_list))
                 showNotification(paste0("Partial correlation table aborted: could not residualize ",
                                         paste(failed_vars, collapse = ", "),
                                         " against the control variables."), type = "error", duration = 8)
                 return(NULL)
             }
             df_clean <- as.data.frame(res_list)
             n_controls <- length(c_vars_lab)
          } else {
             df_clean <- na.omit(df[, vars_lab, drop=FALSE])
          }
        } else {
          df_clean <- na.omit(df[, vars_lab, drop=FALSE])
        }
        
        if(nrow(df_clean) < 3) return(NULL)
        
        if (p_type %in% c("heatmap", "network", "correlogram", "partial")) {
           n_v <- ncol(df_clean)
           res_list <- list()
           for(i in 1:(n_v-1)) {
              for(j in (i+1):n_v) {
                 ct <- tryCatch(cor.test(df_clean[[i]], df_clean[[j]], method = method), error=function(e) NULL)
                 if(!is.null(ct)) {
                    p_val <- ct$p.value
                    if (n_controls > 0) {
                       # cor.test on residuals uses df = n - 2, ignoring the k
                       # control variables partialled out. Recompute the p-value
                       # with the partial-correlation df = n - 2 - k (same
                       # convention as ppcor::pcor.test).
                       r_est <- unname(ct$estimate)
                       n_obs <- nrow(df_clean)
                       if (method == "kendall") {
                          n_eff <- n_obs - n_controls
                          if (n_eff > 2) {
                             z_stat <- 3 * r_est * sqrt(n_eff * (n_eff - 1)) / sqrt(2 * (2 * n_eff + 5))
                             p_val <- 2 * pnorm(-abs(z_stat))
                          } else p_val <- NA_real_
                       } else {
                          df_t <- n_obs - 2 - n_controls
                          if (df_t > 0) {
                             p_val <- if (abs(r_est) >= 1) 0 else 2 * pt(-abs(r_est * sqrt(df_t / (1 - r_est^2))), df_t)
                          } else p_val <- NA_real_
                       }
                    }
                    res_list[[length(res_list)+1]] <- data.frame(
                        Variable_1 = colnames(df_clean)[i],
                        Variable_2 = colnames(df_clean)[j],
                        Correlation = round(ct$estimate, 3),
                        p_raw = p_val
                    )
                 }
              }
           }
           if(length(res_list) > 0) {
              res_df <- do.call(rbind, res_list)
              # Benjamini-Hochberg adjustment across all tested pairs (decided
              # 2026-07-05, user sign-off): raw p-values stay visible, the BH
              # column controls the false discovery rate over the whole table
              res_df$P_Value <- format.pval(res_df$p_raw, digits = 3, eps = 0.001)
              res_df$P_Adj <- format.pval(p.adjust(res_df$p_raw, method = "BH"), digits = 3, eps = 0.001)
              res_df$p_raw <- NULL
              colnames(res_df) <- c("Variable 1", "Variable 2", "Correlation", "P Value", "P Value (BH-adj.)")
              return(DT::datatable(res_df, options = list(pageLength = 10, dom = 'tip', scrollX = TRUE)))
           }
        }
        
        cormat <- corr_matrix_reactive()
        req(cormat)
        cormat <- round(cormat, 3)
        cormat_df <- as.data.frame(cormat)
        
        return(DT::datatable(cormat_df, options = list(pageLength = 10, dom = 't', scrollX = TRUE)))
      }
    })
    
    output$pca_vars_ui <- shiny::renderUI({
      req(data_reactive())
      df <- data_reactive()
      cols <- colnames(df)
      num_cols <- cols[sapply(df, is.numeric)]
      
      vars_metadata <- vmeta()
      num_named <- if (!is.null(vars_metadata)) {
        setNames(num_cols, get_var_labels(num_cols, vars_metadata))
      } else { num_cols }
      
      shiny::tagList(
        shiny::selectInput(ns("pca_vars"), "Variables for PCA (Min 3)", choices = num_named, multiple = TRUE, selected = head(num_cols, 5)),
        shiny::checkboxInput(ns("pca_scale"), "Scale & Center Data (Recommended)", value = TRUE)
      )
    })
    
    pca_rv <- shiny::reactiveValues(res = NULL, data = NULL, cols = NULL, groups = NULL, collinearity_warn = FALSE, collinear_pairs = NULL, scaled = TRUE)
    
    shiny::observeEvent(input$run_pca_btn, {
      req(rv_analytics_data(), input$pca_vars)
      df <- rv_filtered_analytics_data()
      
      if(nrow(df) < 5 || length(input$pca_vars) < 3) {
        showNotification("Insufficient data or variables for PCA.", type="error")
        return()
      }
      
      col_check <- check_collinearity(df, input$pca_vars, threshold = 0.95)
      
      if (col_check$has_collinearity) {
        pca_rv$collinearity_warn <- TRUE
        pca_rv$collinear_pairs <- col_check$pairs
        pca_rv$res <- NULL
        shiny::updateTextInput(session, "pca_ready_flag", value = "no")
      } else {
        pca_rv$collinearity_warn <- FALSE
        pca_rv$collinear_pairs <- NULL
        
        vars_lab <- get_var_labels(input$pca_vars, vmeta())
        keep <- stats::complete.cases(df[, input$pca_vars, drop=FALSE])
        df_clean <- df[keep, input$pca_vars, drop=FALSE]

        dropped_rows <- nrow(df) - nrow(df_clean)
        if (dropped_rows > 0) {
            showNotification(sprintf("Warning: %d rows were dropped due to missing values (NA) in the selected variables.", dropped_rows), type = "warning", duration = 10)
        }

        colnames(df_clean) <- vars_lab

        tryCatch({
          pca_rv$res <- prcomp(df_clean, scale. = input$pca_scale, center = TRUE)
          pca_rv$scaled <- isTRUE(input$pca_scale)
          pca_rv$data <- df_clean
          pca_rv$cols <- vars_lab
          pca_rv$groups <- if ("group_id" %in% colnames(df)) df$group_id[keep] else NULL
          shiny::updateTextInput(session, "pca_ready_flag", value = "yes")
        }, error = function(e) {
          showNotification(paste("PCA Failed:", e$message), type="error")
        })
      }
    })
    
    output$pca_collinearity_warning_ui <- shiny::renderUI({
      if (!pca_rv$collinearity_warn) return(NULL)
      
      shiny::div(class = "alert alert-warning",
          shiny::h4(shiny::icon("exclamation-triangle"), "High Collinearity Detected!"),
          shiny::p("The following variable pairs have a correlation > 0.95. This can severely distort PCA results (multicollinearity)."),
          shiny::tags$ul(
            lapply(seq_len(nrow(pca_rv$collinear_pairs)), function(i) {
              shiny::tags$li(paste0(pca_rv$collinear_pairs$var1[i], " & ", pca_rv$collinear_pairs$var2[i], " (r = ", round(pca_rv$collinear_pairs$r[i], 3), ")"))
            })
          ),
          shiny::p("You should either remove one of the correlated variables from your selection, or force execution if you know what you're doing."),
          shiny::actionButton(ns("pca_force_btn"), "Ignore Warning & Force PCA", class="btn-danger")
      )
    })
    
    shiny::observeEvent(input$pca_force_btn, {
      req(rv_analytics_data(), input$pca_vars)
      df <- rv_filtered_analytics_data()
      
      vars_lab <- get_var_labels(input$pca_vars, vmeta())
      keep <- stats::complete.cases(df[, input$pca_vars, drop=FALSE])
      df_clean <- df[keep, input$pca_vars, drop=FALSE]
      colnames(df_clean) <- vars_lab

      tryCatch({
        pca_rv$res <- prcomp(df_clean, scale. = input$pca_scale, center = TRUE)
        pca_rv$scaled <- isTRUE(input$pca_scale)
        pca_rv$data <- df_clean
        pca_rv$cols <- vars_lab
        pca_rv$groups <- if ("group_id" %in% colnames(df)) df$group_id[keep] else NULL
        pca_rv$collinearity_warn <- FALSE
        shiny::updateTextInput(session, "pca_ready_flag", value = "yes")
      }, error = function(e) {
        showNotification(paste("PCA Failed:", e$message), type="error")
      })
    })
    
    output$pca_plot_controls <- shiny::renderUI({
       req(pca_rv$res)
       n_pcs <- ncol(pca_rv$res$x)
       p_type <- input$pca_plot_type %||% "scree"
  
       if (p_type == "biplot") {
          shiny::tagList(
             shiny::numericInput(ns("pca_pc_x"), "X-Axis (PC)", value = 1, min = 1, max = n_pcs),
             shiny::numericInput(ns("pca_pc_y"), "Y-Axis (PC)", value = 2, min = 1, max = n_pcs)
          )
       } else if (p_type == "3d_biplot") {
          shiny::tagList(
             shiny::numericInput(ns("pca_pc_x"), "X-Axis (PC)", value = 1, min = 1, max = n_pcs),
             shiny::numericInput(ns("pca_pc_y"), "Y-Axis (PC)", value = 2, min = 1, max = n_pcs),
             shiny::numericInput(ns("pca_pc_z"), "Z-Axis (PC)", value = 3, min = 1, max = n_pcs)
          )
       } else if (p_type %in% c("loadings", "contrib")) {
          shiny::tagList(
             shiny::numericInput(ns("pca_pc_single"), "Select PC", value = 1, min = 1, max = n_pcs),
             if (p_type == "contrib" && !isTRUE(pca_rv$scaled)) unscaled_pca_note()
          )
       } else if (p_type == "cos2") {
          shiny::tagList(
             shiny::selectInput(ns("pca_cos2_axes"), "Select PCs to evaluate", choices = 1:n_pcs, multiple = TRUE, selected = 1:min(2, n_pcs)),
             if (!isTRUE(pca_rv$scaled)) unscaled_pca_note()
          )
       } else {
          NULL
       }
    })
  
    pca_plot_obj <- shiny::reactive({
       req(pca_rv$res)
       shiny::withProgress(message = "Generating PCA plot...", value = 0.5, {
         p_type <- input$pca_plot_type %||% "scree"
  
       if (p_type == "scree") {
          p <- generate_pca_scree(pca_rv$res)
       } else if (p_type == "biplot") {
          req(input$pca_pc_x, input$pca_pc_y)
          aligned_df <- data.frame(group_id = pca_rv$groups %||% factor(rep("All", nrow(pca_rv$res$x))))
          p <- generate_pca_biplot(pca_rv$res, aligned_df, pc_x = input$pca_pc_x, pc_y = input$pca_pc_y, group_col = "group_id")
       } else if (p_type == "loadings") {
          req(input$pca_pc_single)
          p <- generate_pca_loadings(pca_rv$res, pc = input$pca_pc_single)
       } else if (p_type == "contrib") {
          req(input$pca_pc_single)
          p <- generate_pca_contribution(pca_rv$res, pc = input$pca_pc_single)
       } else if (p_type == "cos2") {
          req(input$pca_cos2_axes)
          p <- generate_pca_cos2(pca_rv$res, axes = as.numeric(input$pca_cos2_axes))
       } else if (p_type == "cumvar") {
          p <- generate_pca_cumvar(pca_rv$res)
       } else if (p_type == "mahalanobis") {
          p <- generate_pca_mahalanobis(pca_rv$res)
       } else if (p_type == "3d_biplot") {
          # Same guard as the 2-D biplot: switching straight to 3D before the
          # axis controls render would otherwise throw a transient error.
          req(input$pca_pc_x, input$pca_pc_y, input$pca_pc_z)
          aligned_df <- data.frame(group_id = pca_rv$groups %||% factor(rep("All", nrow(pca_rv$res$x))))
          p <- generate_pca_biplot_3d(pca_rv$res, aligned_df, pc_x = input$pca_pc_x, pc_y = input$pca_pc_y, pc_z = input$pca_pc_z, group_col="group_id")
       }
          p
       })
    })
  
    output$pca_main_plot_container <- shiny::renderUI({
      p <- pca_plot_obj()
      if (inherits(p, "plotly")) {
        plotly::plotlyOutput(ns("pca_main_plotly_out"), height = "500px")
      } else {
        shiny::plotOutput(ns("pca_main_static_out"), height = "500px")
      }
    })
  
    output$pca_main_plotly_out <- plotly::renderPlotly({
      p <- pca_plot_obj()
      req(p)
      p
    })
  
    output$pca_main_static_out <- shiny::renderPlot({
      p <- pca_plot_obj()
      req(p)
      if (!inherits(p, "plotly")) return(p)
    })
    
    output$pca_summary_table <- DT::renderDataTable({
       req(pca_rv$res)
       var_explained <- pca_rv$res$sdev^2 / sum(pca_rv$res$sdev^2)
       cum_var <- cumsum(var_explained)
  
       df_res <- data.frame(
          PC = paste0("PC", 1:length(var_explained)),
          Eigenvalue = round(pca_rv$res$sdev^2, 3),
          Variance_Explained_Pct = round(var_explained * 100, 2),
          Cumulative_Variance_Pct = round(cum_var * 100, 2)
       )
  
       DT::datatable(df_res, options = list(pageLength = 10, dom = 't', scrollX = TRUE), rownames = FALSE)
    })
    
    register_expanded_modal(
      input, output, session,
      btn_id = "desc_expand_plot_btn",
      mode_id = "desc_expand_mode",
      ui_id = "desc_expanded_ui",
      plot_static_id = "desc_main_plot_expanded",
      plot_plotly_id = "desc_main_plot_expanded_plotly",
      title_text = "Descriptive Suite",
      build_fn = desc_plot_obj,
      radar_special = TRUE
    )
    
    register_expanded_modal(
      input, output, session,
      btn_id = "corr_expand_plot_btn",
      mode_id = "corr_expand_mode",
      ui_id = "corr_expanded_ui",
      plot_static_id = "corr_main_plot_expanded",
      plot_plotly_id = "corr_main_plot_expanded_plotly",
      title_text = "Correlation Analysis",
      build_fn = corr_plot_obj
    )
    
    register_expanded_modal(
      input, output, session,
      btn_id = "pca_expand_plot_btn",
      mode_id = "pca_expand_mode",
      ui_id = "pca_expanded_ui",
      plot_static_id = "pca_main_plot_expanded",
      plot_plotly_id = "pca_main_plot_expanded_plotly",
      title_text = "PCA",
      build_fn = pca_plot_obj,
      pca_3d_special = shiny::reactive({ input$pca_plot_type == "3d_biplot" })
    )
    
    gov_factors_server("gov", data_reactive = shiny::reactive(rv_analytics_data()), vars_metadata_reactive = vmeta)
    
    return(list(
      analytics_data = rv_analytics_data
    ))
  })
}
