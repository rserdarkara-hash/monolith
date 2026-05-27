# desc_exploratory_module_0.9.7c.R - Descriptive Exploratory Suite Server Module
# Handles Tab 5 analytics: grouping, descriptive stats, correlation, and PCA

desc_exploratory_ui <- function(id) {
  ns <- shiny::NS(id)
  
  shiny::tagList(
    shiny::div(style = "padding: 20px;",
        shiny::h2("Analytics Engine"),
        shiny::p("Explore your data with descriptive statistics, correlation mapping, and principal component analysis. Investigate governing factors on a specific parameter."),
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
                    choices = c("Default" = "default", "Viridis (Colorblind)" = "viridis", "Set1" = "Set1", "Set2" = "Set2", "Dark2" = "Dark2", "Pastel1" = "Pastel1")),
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
        # Hidden reactive input for conditionalPanel
        shiny::conditionalPanel("false", shiny::textInput(ns("pca_ready_flag"), "", value = "no"))
    )
  )
}

desc_exploratory_server <- function(id, data_reactive, vars_metadata_reactive) {
  shiny::moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    # --- Grouping & Discretization Server Logic ---
    shiny::observe({
      req(data_reactive())
      df <- data_reactive()
      cols <- colnames(df)
      valid_cols <- cols[!grepl("\\bx\\b|\\by\\b|lon|lat|latitude|longitude", cols, ignore.case=TRUE)]
      
      vars_metadata <- vars_metadata_reactive()
      if (!is.null(vars_metadata)) {
        choices_named <- setNames(valid_cols, sapply(valid_cols, function(v) {
          match <- Filter(function(x) x$actual == v, vars_metadata)
          if(length(match) > 0 && !is.null(match[[1]]$label) && match[[1]]$label != "") {
            match[[1]]$label
          } else {
            v
          }
        }))
      } else {
        choices_named <- valid_cols
      }
      
      # Retain selection
      curr_sel <- intersect(input$analytics_group_vars, choices_named)
      shiny::updateSelectInput(session, "analytics_group_vars", choices = choices_named, selected = curr_sel)
    })
    
    output$analytics_group_types_ui <- shiny::renderUI({
      req(input$analytics_group_vars, data_reactive())
      vars <- input$analytics_group_vars
      df <- data_reactive()
      lapply(seq_along(vars), function(i) {
        v <- vars[i]
        is_num <- is.numeric(df[[v]])
        
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
    
    # Reactive dataset with grouping applied
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
        def <- if(is.numeric(df[[v]])) "numeric_median" else "categorical"
        input[[paste0("grp_type_", i)]] %||% def
      })
      
      shiny::withProgress(message = "Applying Discretization and Grouping...", value = 0.5, {
         res <- process_grouping_vars(df, vars, types)
         res
      })
    })
    
    # Filtered dataset based on active groups (Cached)
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
        shiny::selectInput(ns("analytics_active_group"), "Select Active Groups to Compare", 
                    choices = levels_present, multiple = TRUE, selected = levels_present)
      }
    })
    
    # --- Descriptive Suite Logic ---
    desc_vars_state <- shiny::reactiveValues(x = "", y = "", z = "", multi = NULL)
    
    output$desc_plot_vars_ui <- shiny::renderUI({
      req(data_reactive())
      df <- data_reactive()
      cols <- colnames(df)
      num_cols <- cols[sapply(df, is.numeric)]
      valid_cols <- cols[!grepl("\\bx\\b|\\by\\b|lon|lat|latitude|longitude", cols, ignore.case=TRUE)]
      
      vars_metadata <- vars_metadata_reactive()
      if (!is.null(vars_metadata)) {
        valid_named <- setNames(valid_cols, sapply(valid_cols, function(v) {
          match <- Filter(function(x) x$actual == v, vars_metadata)
          if(length(match) > 0 && !is.null(match[[1]]$label) && match[[1]]$label != "") match[[1]]$label else v
        }))
        num_named <- setNames(num_cols, sapply(num_cols, function(v) {
          match <- Filter(function(x) x$actual == v, vars_metadata)
          if(length(match) > 0 && !is.null(match[[1]]$label) && match[[1]]$label != "") match[[1]]$label else v
        }))
      } else {
        valid_named <- valid_cols
        num_named <- num_cols
      }
      
      p_type <- input$desc_plot_type %||% "histogram"
      sel_x <- if(isTruthy(desc_vars_state$x)) desc_vars_state$x else valid_named[1]
      
      shiny::tagList(
        if (!(p_type %in% c("parallel", "radar"))) {
          shiny::div(style = "display: flex; align-items: center; gap: 5px;",
              shiny::selectInput(ns("desc_var_x"), "Primary Variable (X)", choices = valid_named, selected = sel_x, width = "calc(100% - 40px)"),
              shiny::actionButton(ns("clear_desc_vars"), "", icon = shiny::icon("times"), class = "btn-danger btn-sm", style = "margin-top: 10px;", title = "Clear selections")
          )
        },
        if (p_type %in% c("boxplot", "violin", "sinaplot", "scatter", "density_heatmap", "xyz_surface")) {
          choices_y <- if(p_type %in% c("boxplot", "violin", "sinaplot")) c("None" = "", valid_named) else valid_named
          sel_y <- if(isTruthy(desc_vars_state$y)) desc_vars_state$y else { if(p_type %in% c("boxplot", "violin", "sinaplot")) "" else valid_cols[2] }
          shiny::selectInput(ns("desc_var_y"), "Secondary Variable (Y)", choices = choices_y, selected = sel_y)
        },
        if (p_type %in% c("boxplot", "violin", "sinaplot")) {
          shiny::div(style="background-color: #f0f8ff; padding: 10px; border-radius: 5px; border: 1px solid #b8daff; margin-bottom: 10px;",
              shiny::h5("Statistical Significance Tests", style="margin-top:0; color: #0056b3;"),
              shiny::checkboxGroupInput(ns("desc_stat_tests"), "Select Test (Choose One):", 
                                 choices = c("ANOVA" = "anova", "Duncan's" = "duncan", "Tukey's HSD" = "tukey"), inline = TRUE),
              shiny::radioButtons(ns("desc_stat_letter_pos"), "Letter Placement:", choices = c("Above Data" = "above", "Top of Plot" = "top"), inline = TRUE)
          )
        },
        if (p_type %in% c("scatter")) {
          shiny::selectInput(ns("desc_scatter_fit"), "Add Trend Line", choices = c("None" = "none", "Linear (lm)" = "linear", "Loess" = "loess", "Polynomial (degree 2)" = "polynomial", "GAM" = "gam"))
        },
        if (p_type %in% c("xyz_surface")) {
          sel_z <- if(isTruthy(desc_vars_state$z)) desc_vars_state$z else num_cols[3]
          shiny::selectInput(ns("desc_var_z"), "Tertiary Variable (Z)", choices = num_named, selected = sel_z)
        },
        if (p_type %in% c("parallel", "radar")) {
          label_text <- ifelse(p_type == "radar", "Select Variables (Min 3)", "Select Variables (Min 2)")
          sel_m <- if(length(desc_vars_state$multi) > 0) desc_vars_state$multi else head(num_cols, 3)
          shiny::selectInput(ns("desc_vars_multi"), label_text, choices = num_named, multiple = TRUE, selected = sel_m)
        },
        if (p_type == "xyz_surface") {
          shiny::selectInput(ns("desc_xyz_fit"), "Surface Fit Model", 
                      choices = c("Linear" = "linear", "Loess" = "loess", "Polynomial" = "polynomial", "GAM" = "gam", "Thin Plate Splines" = "tps"))
        }
      )
    })
    
    shiny::observeEvent(input$desc_var_x, { desc_vars_state$x <- input$desc_var_x })
    shiny::observeEvent(input$desc_var_y, { desc_vars_state$y <- input$desc_var_y })
    shiny::observeEvent(input$desc_var_z, { desc_vars_state$z <- input$desc_var_z })
    shiny::observeEvent(input$desc_vars_multi, { desc_vars_state$multi <- input$desc_vars_multi })
    
    desc_plot_obj <- shiny::reactive({
      req(rv_analytics_data())
      p_type <- input$desc_plot_type
      if (!(p_type %in% c("parallel", "radar"))) {
        req(input$desc_var_x)
      }
      
      shiny::withProgress(message = "Generating descriptive plot...", value = 0.5, {
        df_global <- rv_analytics_data()
        df_local <- rv_filtered_analytics_data()
      
      if (nrow(df_local) == 0) {
        p <- ggplot() + annotate("text", x=0, y=0, label="No data selected") + theme_void()
        return(p)
      }
      
      var_x_label <- get_var_label(input$desc_var_x, vars_metadata_reactive())
      var_y_label <- get_var_label(input$desc_var_y, vars_metadata_reactive())
      
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
        var_z_label <- get_var_label(input$desc_var_z, vars_metadata_reactive())
        if(!is.null(input$desc_var_z) && input$desc_var_z != "") {
            colnames(df_global)[colnames(df_global) == input$desc_var_z] <- var_z_label
            colnames(df_local)[colnames(df_local) == input$desc_var_z] <- var_z_label
        }
        
        multi_labels <- get_var_labels(input$desc_vars_multi, vars_metadata_reactive())
        if(!is.null(input$desc_vars_multi)) {
            df_global <- apply_labels_to_df(df_global, input$desc_vars_multi, vars_metadata_reactive())
            df_local <- apply_labels_to_df(df_local, input$desc_vars_multi, vars_metadata_reactive())
        }
        
        vars <- switch(p_type,
                       "qq" = var_x_label,
                       "sinaplot" = if(isTruthy(input$desc_var_y)) c(var_x_label, get_var_label(input$desc_var_y, vars_metadata_reactive())) else var_x_label,
                       "ridge" = var_x_label,
                       "density_heatmap" = c(var_x_label, var_y_label),
                       "xyz_surface" = c(var_x_label, var_y_label, var_z_label),
                       "parallel" = unname(multi_labels),
                       "radar" = unname(multi_labels),
                       var_x_label)
        
        p <- generate_advanced_plot(df_local, vars = vars, group_col = "group_id", plot_type = p_type, xyz_fit = input$desc_xyz_fit, stat_test = input$desc_stat_tests, stat_letter_pos = input$desc_stat_letter_pos)
      }
      
      pal <- input$desc_palette %||% "default"
      if (pal != "default") {
         if (p_type %in% c("density_heatmap", "xyz_surface")) {
            if (pal == "viridis") {
               p <- p + scale_fill_viridis_c() + scale_color_viridis_c()
            } else {
               p <- p + scale_fill_distiller(palette = pal) + scale_color_distiller(palette = pal)
            }
         } else {
            if (pal == "viridis") {
               p <- p + scale_fill_viridis_d() + scale_color_viridis_d()
            } else {
               p <- p + scale_fill_brewer(palette = pal) + scale_color_brewer(palette = pal)
            }
         }
      }
      
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
      
      agg_mean <- aggregate(df[[var]] ~ df$group_id, FUN=function(x) mean(x, na.rm=TRUE))
      agg_sd <- aggregate(df[[var]] ~ df$group_id, FUN=function(x) sd(x, na.rm=TRUE))
      agg_n <- aggregate(df[[var]] ~ df$group_id, FUN=length)
      agg_min <- aggregate(df[[var]] ~ df$group_id, FUN=function(x) min(x, na.rm=TRUE))
      agg_max <- aggregate(df[[var]] ~ df$group_id, FUN=function(x) max(x, na.rm=TRUE))
      
      res <- data.frame(
        Group = agg_mean[,1], 
        Count = agg_n[,2], 
        Mean = round(agg_mean[,2], 3), 
        SD = round(agg_sd[,2], 3),
        Min = round(agg_min[,2], 3),
        Max = round(agg_max[,2], 3)
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
                 if (f == "linear") summary(lm(sub_df[[y_var]] ~ sub_df[[var]]))$r.squared
                 else if (f == "polynomial") { if(length(unique(sub_df[[var]])) > 2) summary(lm(sub_df[[y_var]] ~ poly(sub_df[[var]], 2)))$r.squared else NA }
                 else if (f == "loess") { mod <- loess(sub_df[[y_var]] ~ sub_df[[var]], span=0.7); cor(sub_df[[y_var]], fitted(mod))^2 }
                 else if (f == "gam") { if(requireNamespace("mgcv", quietly=TRUE)) summary(mgcv::gam(sub_df[[y_var]] ~ s(sub_df[[var]], bs = "cs")))$r.sq else NA }
                 else NA
              }, error = function(e) NA)
           })
           
           p_vals <- sapply(as.character(res$Group), function(g) {
              if (g == "TOTAL") sub_df <- df else sub_df <- df[as.character(df$group_id) == g,]
              if (nrow(sub_df) < 5) return(NA)
              f <- input$desc_scatter_fit
              tryCatch({
                 if (f == "linear") {
                     mod <- summary(lm(sub_df[[y_var]] ~ sub_df[[var]]))
                     if(!is.null(mod$fstatistic)) pf(mod$fstatistic[1], mod$fstatistic[2], mod$fstatistic[3], lower.tail=FALSE) else NA
                 } else if (f == "polynomial") { 
                     if(length(unique(sub_df[[var]])) > 2) {
                         mod <- summary(lm(sub_df[[y_var]] ~ poly(sub_df[[var]], 2)))
                         if(!is.null(mod$fstatistic)) pf(mod$fstatistic[1], mod$fstatistic[2], mod$fstatistic[3], lower.tail=FALSE) else NA
                     } else NA 
                 }
                 else if (f == "gam") { if(requireNamespace("mgcv", quietly=TRUE)) summary(mgcv::gam(sub_df[[y_var]] ~ s(sub_df[[var]], bs = "cs")))$s.table[1, "p-value"] else NA }
                 else NA
              }, error = function(e) NA)
           })
           
           res$Trend_R2 <- round(as.numeric(r2_vals), 3)
           res$Trend_PVal <- format.pval(as.numeric(p_vals), digits = 3, eps = 0.001)
        }
      }
      
      DT::datatable(res, options = list(pageLength = 10, dom = 'tip', scrollX = TRUE))
    })
    
    # --- Phase 4: Correlation Rendering ---
    output$corr_vars_ui <- shiny::renderUI({
      req(data_reactive())
      df <- data_reactive()
      cols <- colnames(df)
      num_cols <- cols[sapply(df, is.numeric)]
      
      vars_metadata <- vars_metadata_reactive()
      num_named <- if (!is.null(vars_metadata)) {
        setNames(num_cols, sapply(num_cols, function(v) {
          match <- Filter(function(x) x$actual == v, vars_metadata)
          if(length(match) > 0 && !is.null(match[[1]]$label) && match[[1]]$label != "") match[[1]]$label else v
        }))
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
        v1_lab <- get_var_label(input$corr_var_1, vars_metadata_reactive())
        v2_lab <- get_var_label(input$corr_var_2, vars_metadata_reactive())
        colnames(df)[colnames(df) == input$corr_var_1] <- v1_lab
        colnames(df)[colnames(df) == input$corr_var_2] <- v2_lab
        p <- generate_lagged_correlation(df, v1_lab, v2_lab, max_lag = input$corr_max_lag %||% 10)
      } else {
        req(input$corr_vars_multi)
        vars <- input$corr_vars_multi
        if (length(vars) < 2) return(ggplot() + annotate("text", x=0, y=0, label="Need >=2 variables"))
        
        df <- apply_labels_to_df(df, vars, vars_metadata_reactive())
        vars_lab <- get_var_labels(vars, vars_metadata_reactive())
        
        if (p_type == "heatmap") {
          p <- generate_correlation_heatmap(df, vars_lab, method = method)
        } else if (p_type == "network") {
          p <- generate_correlation_network(df, vars_lab, threshold = input$corr_net_thresh %||% 0.3, method = method)
        } else if (p_type == "partial") {
          c_vars <- input$corr_vars_control
          if(!is.null(c_vars) && length(c_vars) > 0) {
             df <- apply_labels_to_df(df, c_vars, vars_metadata_reactive())
             c_vars_lab <- get_var_labels(c_vars, vars_metadata_reactive())
          } else {
             c_vars_lab <- NULL
          }
          p <- generate_partial_correlation(df, vars_lab, control_vars = c_vars_lab, method = method)
        } else if (p_type == "correlogram") {
          p <- generate_correlogram(df, vars_lab, method = method)
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
        max_lag <- input$corr_max_lag %||% 10
        ccf_res <- ccf(df_clean[[v1]], df_clean[[v2]], lag.max = max_lag, plot = FALSE)
        res_df <- data.frame(Lag = ccf_res$lag[,1,1], CrossCorrelation = round(ccf_res$acf[,1,1], 3))
        return(DT::datatable(res_df, options = list(pageLength = 10, dom = 't', scrollX = TRUE)))
      } else {
        req(input$corr_vars_multi)
        vars <- input$corr_vars_multi
        if (length(vars) < 2) return(NULL)
        
        df <- apply_labels_to_df(df, vars, vars_metadata_reactive())
        vars_lab <- get_var_labels(vars, vars_metadata_reactive())
        
        if (p_type == "partial") {
          c_vars <- input$corr_vars_control
          if(!is.null(c_vars) && length(c_vars) > 0) {
             df <- apply_labels_to_df(df, c_vars, vars_metadata_reactive())
             c_vars_lab <- get_var_labels(c_vars, vars_metadata_reactive())
             
             all_vars <- unique(c(vars_lab, c_vars_lab))
             df_clean <- na.omit(df[, all_vars, drop=FALSE])
             if(nrow(df_clean) < 5) return(NULL)
             
             res_list <- list()
             for (v in vars_lab) {
               mod <- try(lm(as.formula(paste0("`", v, "` ~ ", paste(paste0("`", c_vars_lab, "`"), collapse=" + "))), data=df_clean), silent=TRUE)
               if(!inherits(mod, "try-error")) res_list[[v]] <- residuals(mod)
             }
             if(length(res_list) == length(vars_lab)) {
                 df_clean <- as.data.frame(res_list)
             }
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
                    res_list[[length(res_list)+1]] <- data.frame(
                        Variable_1 = colnames(df_clean)[i],
                        Variable_2 = colnames(df_clean)[j],
                        Correlation = round(ct$estimate, 3),
                        P_Value = format.pval(ct$p.value, digits=3, eps=0.001)
                    )
                 }
              }
           }
           if(length(res_list) > 0) {
              res_df <- do.call(rbind, res_list)
              return(DT::datatable(res_df, options = list(pageLength = 10, dom = 'tip', scrollX = TRUE)))
           }
        }
        
        cormat <- round(cor(df_clean, method = method), 3)
        cormat_df <- as.data.frame(cormat)
        
        return(DT::datatable(cormat_df, options = list(pageLength = 10, dom = 't', scrollX = TRUE)))
      }
    })
    
    # --- PCA Logic ---
    output$pca_vars_ui <- shiny::renderUI({
      req(data_reactive())
      df <- data_reactive()
      cols <- colnames(df)
      num_cols <- cols[sapply(df, is.numeric)]
      
      vars_metadata <- vars_metadata_reactive()
      num_named <- if (!is.null(vars_metadata)) {
        setNames(num_cols, sapply(num_cols, function(v) {
          match <- Filter(function(x) x$actual == v, vars_metadata)
          if(length(match) > 0 && !is.null(match[[1]]$label) && match[[1]]$label != "") match[[1]]$label else v
        }))
      } else { num_cols }
      
      shiny::tagList(
        shiny::selectInput(ns("pca_vars"), "Variables for PCA (Min 3)", choices = num_named, multiple = TRUE, selected = head(num_cols, 5)),
        shiny::checkboxInput(ns("pca_scale"), "Scale & Center Data (Recommended)", value = TRUE)
      )
    })
    
    pca_rv <- shiny::reactiveValues(res = NULL, data = NULL, cols = NULL, collinearity_warn = FALSE, collinear_pairs = NULL)
    
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
        
        vars_lab <- get_var_labels(input$pca_vars, vars_metadata_reactive())
        df_clean <- na.omit(df[, input$pca_vars, drop=FALSE])
        colnames(df_clean) <- vars_lab
        
        tryCatch({
          pca_rv$res <- prcomp(df_clean, scale. = input$pca_scale, center = input$pca_scale)
          pca_rv$data <- df_clean
          pca_rv$cols <- vars_lab
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
            lapply(1:nrow(pca_rv$collinear_pairs), function(i) {
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
      
      vars_lab <- get_var_labels(input$pca_vars, vars_metadata_reactive())
      df_clean <- na.omit(df[, input$pca_vars, drop=FALSE])
      colnames(df_clean) <- vars_lab
      
      tryCatch({
        pca_rv$res <- prcomp(df_clean, scale. = input$pca_scale, center = input$pca_scale)
        pca_rv$data <- df_clean
        pca_rv$cols <- vars_lab
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
          shiny::numericInput(ns("pca_pc_single"), "Select PC", value = 1, min = 1, max = n_pcs)
       } else if (p_type == "cos2") {
          shiny::selectInput(ns("pca_cos2_axes"), "Select PCs to evaluate", choices = 1:n_pcs, multiple = TRUE, selected = 1:min(2, n_pcs))
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
          p <- generate_pca_biplot(pca_rv$res, rv_analytics_data(), pc_x = input$pca_pc_x, pc_y = input$pca_pc_y, group_col = "group_id")
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
          p <- generate_pca_biplot_3d(pca_rv$res, rv_analytics_data(), pc_x = input$pca_pc_x, pc_y = input$pca_pc_y, pc_z = input$pca_pc_z, group_col="group_id")
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
    
    # --- Expandable Modal Dialogs Server Logic ---
    shiny::observeEvent(input$desc_expand_plot_btn, {
      shiny::showModal(shiny::modalDialog(
        title = "Expanded View: Descriptive Suite",
        size = "l",
        easyClose = TRUE,
        shiny::radioButtons(ns("desc_expand_mode"), "View Mode:", choices=c("Static (High-Res)"="static", "Interactive (Hover/Zoom)"="interactive"), inline=TRUE),
        shiny::uiOutput(ns("desc_expanded_ui")),
        footer = shiny::modalButton("Close")
      ))
    })
    
    output$desc_expanded_ui <- shiny::renderUI({
       if (input$desc_expand_mode == "interactive") {
          plotly::plotlyOutput(ns("desc_main_plot_expanded_plotly"), height = "700px")
       } else {
          shiny::plotOutput(ns("desc_main_plot_expanded"), height = "700px")
       }
    })
    
    output$desc_main_plot_expanded <- shiny::renderPlot({
       desc_plot_obj()
    })
    
    output$desc_main_plot_expanded_plotly <- plotly::renderPlotly({
       p <- desc_plot_obj()
       if (input$desc_plot_type == "radar" && inherits(p, "ggplot") && nrow(p$data) > 0 && "variable" %in% colnames(p$data)) {
          d <- p$data
          fig <- plotly::plot_ly(type = 'scatterpolar', mode = 'lines+markers')
          for(g in unique(d$group)) {
              dg <- d[d$group == g, ]
              dg <- rbind(dg, dg[1, ])
              fig <- plotly::add_trace(fig, r = dg$value, theta = dg$variable, name = g, fill = 'toself')
          }
          fig <- plotly::layout(fig, 
                                polar = list(radialaxis = list(visible = TRUE, range = c(0, max(d$value, na.rm=TRUE)))), 
                                showlegend = TRUE, 
                                title = list(text = "Radar Chart (Normalized Means)<br><sup>Note: Native plotly style used for interactive mode</sup>", x = 0.5))
          return(fig)
       }
       if(inherits(p, "ggplot")) plotly::ggplotly(p) else p
    })
  
    shiny::observeEvent(input$corr_expand_plot_btn, {
      shiny::showModal(shiny::modalDialog(
        title = "Expanded View: Correlation Analysis",
        size = "l",
        easyClose = TRUE,
        shiny::radioButtons(ns("corr_expand_mode"), "View Mode:", choices=c("Static (High-Res)"="static", "Interactive (Hover/Zoom)"="interactive"), inline=TRUE),
        shiny::uiOutput(ns("corr_expanded_ui")),
        footer = shiny::modalButton("Close")
      ))
    })
    
    output$corr_expanded_ui <- shiny::renderUI({
       if (input$corr_expand_mode == "interactive") {
          plotly::plotlyOutput(ns("corr_main_plot_expanded_plotly"), height = "700px")
       } else {
          shiny::plotOutput(ns("corr_main_plot_expanded"), height = "700px")
       }
    })
    
    output$corr_main_plot_expanded <- shiny::renderPlot({
       corr_plot_obj()
    })
    
    output$corr_main_plot_expanded_plotly <- plotly::renderPlotly({
       p <- corr_plot_obj()
       if(inherits(p, "ggplot")) plotly::ggplotly(p) else p
    })
  
    shiny::observeEvent(input$pca_expand_plot_btn, {
      shiny::showModal(shiny::modalDialog(
        title = "Expanded View: PCA",
        size = "l",
        easyClose = TRUE,
        if (input$pca_plot_type != "3d_biplot") {
           shiny::radioButtons(ns("pca_expand_mode"), "View Mode:", choices=c("Static (High-Res)"="static", "Interactive (Hover/Zoom)"="interactive"), inline=TRUE)
        },
        shiny::uiOutput(ns("pca_expanded_ui")),
        footer = shiny::modalButton("Close")
      ))
    })
  
    output$pca_expanded_ui <- shiny::renderUI({
       if (input$pca_plot_type == "3d_biplot") {
          plotly::plotlyOutput(ns("pca_main_plot_expanded_plotly_3d"), height = "700px")
       } else {
          if (!is.null(input$pca_expand_mode) && input$pca_expand_mode == "interactive") {
             plotly::plotlyOutput(ns("pca_main_plot_expanded_plotly"), height = "700px")
          } else {
             shiny::plotOutput(ns("pca_main_plot_expanded"), height = "700px")
          }
       }
    })
  
    output$pca_main_plot_expanded <- shiny::renderPlot({
       p <- pca_plot_obj()
       if(inherits(p, "ggplot")) return(p)
    })
  
    output$pca_main_plot_expanded_plotly <- plotly::renderPlotly({
       p <- pca_plot_obj()
       if(inherits(p, "ggplot")) plotly::ggplotly(p) else p
    })
  
    output$pca_main_plot_expanded_plotly_3d <- plotly::renderPlotly({
       p <- pca_plot_obj()
       if(inherits(p, "plotly")) return(p)
    })
    
    # Governing Factors Module integration
    gov_factors_server("gov", data_reactive = shiny::reactive(rv_analytics_data()), vars_metadata_reactive = vars_metadata_reactive)
    
    # Return processed reactive dataset to caller
    return(list(
      analytics_data = rv_analytics_data
    ))
  })
}
