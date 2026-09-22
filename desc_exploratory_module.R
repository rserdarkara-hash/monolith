
# Caveat shown with the Contribution / cos2 controls when the PCA was run
# without scaling. The two panels need DIFFERENT wording: contribution is a
# share of a COMPONENT, so an unscaled run makes it a ranking of raw variances;
# cos2 is normalised per variable (see generate_pca_cos2) and so stays a true
# quality-of-representation in both modes - what carries over there is that the
# components themselves are driven by the high-variance variables.
unscaled_pca_note <- function(panel = c("contrib", "cos2")) {
  panel <- match.arg(panel)
  shiny::helpText(
    shiny::icon("info-circle"),
    if (panel == "contrib") {
      "PCA was run without scaling: contribution values are dominated by high-variance variables and are not comparable across variables measured on different scales."
    } else {
      "PCA was run without scaling: cos2 is still each variable's own explained share (0-1), but the components it is measured against are dominated by the high-variance variables."
    }
  )
}

# Empty-state table for the module's DT outputs. Never hand a widget output
# NULL (the app-wide convention behind sci_dt's placeholder, ui_components.R);
# a silent blank table also hides WHY there is nothing to show.
desc_empty_dt <- function(msg = "No data for this selection.") {
  # mn-status-table: the copy button reports the message instead of copying it
  DT::datatable(data.frame(Message = msg), options = list(dom = "t"), rownames = FALSE,
                class = "display mn-status-table")
}

compute_normality <- function(x) {
  default_res <- list(
    status = "insufficient",
    method = "None",
    statistic = NA,
    p_value = NA,
    n = 0,
    # Why no test was run, so the verdict can say it rather than blaming the
    # sample size for a constant column.
    reason = "no numeric values"
  )

  if (is.null(x) || !is.numeric(x)) {
    return(default_res)
  }

  clean_x <- x[!is.na(x)]
  n <- length(clean_x)
  default_res$n <- n

  if (n < 3) {
    default_res$reason <- "fewer than 3 non-missing values"
    return(default_res)
  }

  if (var(clean_x) == 0) {
    default_res$reason <- "the values are constant"
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
    default_res$reason <- paste("the test failed:", e$message)
    default_res
  })
}

#' The normality verdict as one sentence, for reading and for copying.
#'
#' The verdict used to live only in an icon's `title`, so it could not be
#' copied into a report or read without hovering; the tooltip keeps the long
#' explanation and the per-group breakdown, this is the line on the page. It
#' names the test, its statistic with the symbol that test reports (W for
#' Shapiro-Wilk, D for Lilliefors), the p-value through the app's own
#' format_p_value(), the significance stars and n, and states the conclusion
#' at alpha = 0.05 in words.
normality_verdict_text <- function(res, on_residuals = FALSE) {
  what <- if (isTRUE(on_residuals)) "within-group residuals" else "raw values"
  if (identical(res$status, "insufficient")) {
    return(sprintf("Normality not tested on the %s: %s (n = %d).",
                   what, res$reason %||% "the test could not be run", res$n))
  }
  sym <- if (grepl("Shapiro-Wilk", res$method)) "W" else "D"
  stars <- signif_stars(res$p_value)
  sprintf("%s on the %s: %s = %s, p %s%s, n = %d. %s",
          res$method, what, sym, format_sig(res$statistic),
          if (grepl("<", format_p_value(res$p_value))) format_p_value(res$p_value)
            else paste("=", format_p_value(res$p_value)),
          if (nzchar(stars)) paste0(" ", stars) else "",
          res$n,
          if (identical(res$status, "normal")) {
            "No significant departure from normality at alpha = 0.05."
          } else {
            "Significant departure from normality at alpha = 0.05."
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
                      shiny::tags$button(id = ns("desc_expand_plot_btn"), type = "button", class = "btn btn-default action-button expand-icon-btn", title = "Expand plot", "aria-label" = "Expand descriptive plot", shiny::icon("expand")),
                      shiny::plotOutput(ns("desc_main_plot"), height = "500px")
                  ),
                  shiny::hr(),
                  sci_table(ns("desc_summary_table"), "Group Statistics", title_tag = shiny::h4)
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
                                "Spatial Cross-Correlogram" = "spatial_ccf")),
                  shiny::selectInput(ns("corr_method"), "Method", choices = c("pearson", "spearman", "kendall")),
                  shiny::uiOutput(ns("corr_vars_ui"))
                ),
                shiny::column(9,
                  shiny::div(style = "position: relative;",
                      shiny::tags$button(id = ns("corr_expand_plot_btn"), type = "button", class = "btn btn-default action-button expand-icon-btn", title = "Expand plot", "aria-label" = "Expand correlation plot", shiny::icon("expand")),
                      shiny::plotOutput(ns("corr_main_plot"), height = "500px")
                  ),
                  shiny::hr(),
                  sci_table(ns("corr_summary_table"), "Correlation Matrix", title_tag = shiny::h4)
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
                      shiny::tags$button(id = ns("pca_expand_plot_btn"), type = "button", class = "btn btn-default action-button expand-icon-btn", title = "Expand plot", "aria-label" = "Expand PCA plot", shiny::icon("expand")),
                      shiny::uiOutput(ns("pca_main_plot_container"))
                  ),
                  shiny::hr(),
                  shiny::conditionalPanel(
                    condition = sprintf("input['%s'] == 'yes'", ns("pca_ready_flag")),
                    sci_table(ns("pca_summary_table"), "PCA Results", title_tag = shiny::h4)
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

# `spatial_reactive` carries the confirmed coordinate mapping (x/y column names,
# source CRS, target mapping CRS) for the Spatial Cross-Correlogram panel, which
# bins point pairs by ground distance. It defaults to NULL so the module still
# runs headless (tests) and so every other panel stays independent of it.
desc_exploratory_server <- function(id, data_reactive, vars_metadata_reactive,
                                    spatial_reactive = shiny::reactive(NULL)) {
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
          shiny::div(class = "mn-input-with-clear",
              shiny::selectInput(ns("desc_var_x"), "Primary Variable (X)", choices = valid_named, selected = sel_x, width = "100%"),
              shiny::actionButton(ns("clear_desc_vars"), NULL, icon = shiny::icon("times"), class = "btn-danger btn-sm mn-clear-btn", title = "Clear selections", "aria-label" = "Clear variable selections")
          )
        },
        if (p_type %in% c("boxplot", "violin", "sinaplot", "scatter", "density_heatmap", "xyz_surface")) {
          choices_y <- if(p_type %in% c("boxplot", "violin", "sinaplot")) c("None" = "", valid_named) else valid_named
          sel_y <- if(isTruthy(sel_state$y)) sel_state$y else { if(p_type %in% c("boxplot", "violin", "sinaplot")) "" else valid_cols[2] }
          shiny::selectInput(ns("desc_var_y"), "Secondary Variable (Y)", choices = choices_y, selected = sel_y)
        },
        if (p_type %in% c("boxplot", "violin", "sinaplot")) {
          shiny::div(style="background-color: var(--mn-surface-2); padding: 10px; border-radius: 6px; border: 1px solid var(--mn-line); margin-bottom: 10px;",
              shiny::h5(style="margin-top:0; color: var(--mn-text);",
                  "Statistical Significance Tests"),
              # Its own line, not inside the heading: the verdict is a full
              # sentence now, and a heading is the wrong place to wrap one.
              shiny::uiOutput(ns("desc_normality_indicator")),
              # radioButtons, not a checkbox group: only ONE test is ever used
              # (add_stat_layer takes stat_test[1], which is CHOICE order, not
              # click order), so a multi-select control silently discarded the
              # user's second pick - and it did so in the direction that
              # matters, substituting a parametric post-hoc for a deliberately
              # chosen Kruskal-Wallis. "None" is the empty string, which
              # add_stat_layer/get_stat_letters treat as "draw nothing".
              shiny::div(class = "mn-seg-grid",
                shinyWidgets::radioGroupButtons(ns("desc_stat_tests"),
                                 shiny::HTML(paste0("Group comparison test", info_tooltip(ns("desc_stat_test_info"), "For non-normal data distributions, the non-parametric Kruskal-Wallis test is recommended. Duncan is liberal: it controls the comparison-wise, not the family-wise, error rate."), ":")),
                                 choices = c("None" = "", "ANOVA" = "anova", "Duncan's (liberal)" = "duncan", "Tukey's HSD" = "tukey", "Kruskal-Wallis" = "kruskal"),
                                 selected = "", size = "sm")),
              shiny::div(class = "mn-seg-grid",
                shinyWidgets::radioGroupButtons(ns("desc_stat_letter_pos"), "Letter Placement:",
                                 choices = c("Above Data" = "above", "Top of Plot" = "top"),
                                 selected = "above", size = "sm"))
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
        icon_element <- shiny::icon("circle-question", style = "color: var(--mn-text-3); font-size: 13px; cursor: help;")
        tooltip_title <- sprintf(
          "Normality Test: not run (%s; n = %d).%s",
          res$reason %||% "the test could not be run", res$n,
          group_breakdown
        )
      } else if (res$status == "normal") {
        icon_element <- shiny::icon("circle-check", style = "color: var(--mn-ok); font-size: 13px; cursor: help;")
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
        icon_element <- shiny::icon("circle-exclamation", style = "color: var(--mn-warn); font-size: 13px; cursor: help;")
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
      
      # The verdict is TEXT on the page, not only a hover: it is what a reader
      # copies into a report, so it has to be a complete sentence and to survive
      # a copy. The icon stays as the severity cue and the tooltip keeps the
      # long explanation and the per-group breakdown.
      shiny::tags$span(
        style = "margin-left: 5px; display: inline-block; vertical-align: middle;",
        title = tooltip_title,
        icon_element,
        shiny::tags$span(
          style = "margin-left: 5px; font-size: 0.78em; font-weight: 400; color: var(--mn-text-2);",
          normality_verdict_text(res, used_residuals)
        )
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

      if (nrow(df) == 0) return(desc_empty_dt("No data selected."))

      if (p_type %in% c("parallel", "radar")) {
          return(data.frame(Message="Summary statistics table is not available for multi-variable plots."))
      }
      
      var <- input$desc_var_x
      if(!is.numeric(df[[var]])) return(data.frame(Message="Selected primary variable is not numeric."))
      
      # Arithmetic (the per-group statistics, the TOTAL row and the trend fits)
      # lives in ui_formatting.R; this block selects the data and formats.
      res <- desc_summary_table(df[[var]], df$group_id)
      num_cols <- DESC_SUMMARY_STATS

      if (input$desc_plot_type == "scatter" && !is.null(input$desc_scatter_fit) && input$desc_scatter_fit != "none") {
        y_var <- if(!is.null(input$desc_var_y) && input$desc_var_y != "") input$desc_var_y else NULL
        if (!is.null(y_var)) {
           fits <- desc_group_fit_stats(df, var, y_var, input$desc_scatter_fit, res$Group)

           if (input$desc_scatter_fit == "loess") {
               res$`Squared Correlation (Not true R²)` <- as.numeric(fits$r2)
               num_cols <- c(num_cols, "Squared Correlation (Not true R²)")
           } else {
               res$Trend_R2 <- as.numeric(fits$r2)
               num_cols <- c(num_cols, "Trend_R2")
           }
           res$Trend_PVal <- format.pval(fits$p, digits = 3, eps = 0.001)
        }
      }

      # rownames default to TRUE here, so the column indexes shift by one.
      DT::datatable(res, options = list(pageLength = 10, dom = 'tip', scrollX = TRUE,
                                        columnDefs = sig_render_defs(res, num_cols,
                                                                     rownames = TRUE)))
      # server = FALSE: every page is in the browser, so the copy button can
      # read the whole table rather than the page on screen.
    }, server = FALSE)
    
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
      
      if (p_type == "spatial_ccf") {
        shiny::tagList(
          shiny::selectInput(ns("corr_var_1"), "Primary Variable", choices = num_named, selected = curr_var1),
          shiny::selectInput(ns("corr_var_2"), "Secondary Variable", choices = num_named, selected = curr_var2),
          shiny::numericInput(ns("corr_n_bins"), "Distance Bins", value = 15, min = 3, max = 50),
          shiny::helpText("Lags are ground distances between sample points, not table rows.")
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
      
      if (p_type == "spatial_ccf") {
        req(input$corr_var_1, input$corr_var_2)
        sp <- spatial_reactive()
        v1_lab <- get_var_label(input$corr_var_1, vmeta())
        v2_lab <- get_var_label(input$corr_var_2, vmeta())
        colnames(df)[colnames(df) == input$corr_var_1] <- v1_lab
        colnames(df)[colnames(df) == input$corr_var_2] <- v2_lab
        p <- generate_spatial_cross_correlogram(
          df, v1_lab, v2_lab,
          x_col = sp$x, y_col = sp$y, src_crs = sp$src_crs, proj_crs = sp$proj_crs,
          n_bins = input$corr_n_bins %||% 15, method = method)
      } else {
        req(input$corr_vars_multi)
        vars <- input$corr_vars_multi
        if (length(vars) < 2) return(ggplot() + annotate("text", x=0, y=0, label="Need >=2 variables"))
        
        df <- apply_labels_to_df(df, vars, vmeta())
        vars_lab <- get_var_labels(vars, vmeta())
        cc_vars <- vars_lab

        if (p_type == "heatmap") {
          p <- generate_correlation_heatmap(df, vars_lab, method = method, cormat = corr_matrix_reactive())
        } else if (p_type == "network") {
          p <- generate_correlation_network(df, vars_lab, threshold = input$corr_net_thresh %||% 0.3, method = method, cormat = corr_matrix_reactive())
        } else if (p_type == "partial") {
          c_vars <- input$corr_vars_control
          if(!is.null(c_vars) && length(c_vars) > 0) {
             df <- apply_labels_to_df(df, c_vars, vmeta())
             c_vars_lab <- get_var_labels(c_vars, vmeta())
             cc_vars <- unique(c(vars_lab, c_vars_lab))
          } else {
             c_vars_lab <- NULL
          }
          p <- generate_partial_correlation(df, vars_lab, control_vars = c_vars_lab, method = method)
        } else if (p_type == "correlogram") {
          p <- generate_correlogram(df, vars_lab, method = method, cormat = corr_matrix_reactive())
        }
        # These four panels are one matrix, estimated on the rows complete across
        # every variable involved (controls included). Carrying that n on the
        # figure keeps it with the plot when the plot is exported.
        if (inherits(p, "ggplot") && all(cc_vars %in% colnames(df))) {
          p <- p + labs(caption = complete_case_note(
            sum(stats::complete.cases(df[, cc_vars, drop = FALSE])), nrow(df)))
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

      if (nrow(df) < 3) return(desc_empty_dt("Insufficient data (fewer than 3 rows in the active groups)."))

      p_type <- input$corr_plot_type
      method <- input$corr_method %||% "pearson"
      
      if (p_type == "spatial_ccf") {
        req(input$corr_var_1, input$corr_var_2)
        sp <- spatial_reactive()
        v1 <- get_var_label(input$corr_var_1, vmeta())
        v2 <- get_var_label(input$corr_var_2, vmeta())
        colnames(df)[colnames(df) == input$corr_var_1] <- v1
        colnames(df)[colnames(df) == input$corr_var_2] <- v2
        # Same computation the plot uses, so the table can never disagree with it.
        res <- compute_spatial_cross_correlogram(
          df, v1, v2, x_col = sp$x, y_col = sp$y,
          src_crs = sp$src_crs, proj_crs = sp$proj_crs,
          n_bins = input$corr_n_bins %||% 15, method = method)
        # The plot names the reason it cannot be computed; the table used to
        # just vanish. Show the same reason instead.
        if (is.null(res$bins)) return(desc_empty_dt(gsub("\n", " ", res$message %||% "Cross-correlogram not computable for this selection.")))
        # Numbers stay numbers (sorting, copying); the display shows four
        # significant digits, as every result table does.
        res_df <- data.frame(
          Lag = res$bins$dist,
          Pairs = res$bins$np,
          CrossCorrelation = res$bins$rho,
          CrossSemivariance = res$bins$gamma
        )
        colnames(res_df) <- c(paste0("Lag distance (", res$unit, ")"), "Pairs",
                              "Cross-correlation", "Cross-semivariance (std.)")
        return(DT::datatable(res_df, options = list(pageLength = 10, dom = 'tip', scrollX = TRUE,
                                                    columnDefs = sig_render_defs(res_df, names(res_df)[-2]))))
      } else {
        req(input$corr_vars_multi)
        vars <- input$corr_vars_multi
        if (length(vars) < 2) return(desc_empty_dt("Select at least two variables."))

        df <- apply_labels_to_df(df, vars, vmeta())
        vars_lab <- get_var_labels(vars, vmeta())
        
        n_controls <- 0
        pcor <- NULL
        if (p_type == "partial") {
          c_vars <- input$corr_vars_control
          if(!is.null(c_vars) && length(c_vars) > 0) {
             df <- apply_labels_to_df(df, c_vars, vmeta())
             c_vars_lab <- get_var_labels(c_vars, vmeta())

             # Shared with the Partial Correlation heatmap: raw residuals for
             # pearson, rank residuals for spearman, inverted tau matrix for
             # kendall (ppcor conventions, matching the p-values below).
             pcor <- compute_partial_correlation(df, vars_lab, c_vars_lab, method = method)
             if (is.null(pcor$cormat) || length(pcor$failed) > 0) {
                 # Never fall back to raw correlations while the table is
                 # labelled partial: abort and say so.
                 if (length(pcor$failed) > 0) {
                   showNotification(paste0("Partial correlation table aborted: could not partial out the control variables for ",
                                           paste(pcor$failed, collapse = ", "), "."),
                                    type = "error", duration = 8)
                   return(desc_empty_dt(paste0("Could not partial out the control variables for ",
                                               paste(pcor$failed, collapse = ", "), ".")))
                 }
                 return(desc_empty_dt("Partial correlation could not be computed for this selection."))
             }
             if (pcor$n < 5) return(desc_empty_dt("Insufficient complete observations for partial correlation (n < 5)."))
             if (pcor$k == 0) {
               # Every named control is also one of the correlated variables, so
               # nothing is left to partial out (a variable never controls for
               # itself): report the plain correlations, with their own tests.
               pcor <- NULL
               df_clean <- na.omit(df[, vars_lab, drop = FALSE])
             } else {
               df_clean <- na.omit(df[, unique(c(vars_lab, c_vars_lab)), drop = FALSE])
               n_controls <- pcor$k
             }
          } else {
             df_clean <- na.omit(df[, vars_lab, drop=FALSE])
          }
        } else {
          df_clean <- na.omit(df[, vars_lab, drop=FALSE])
        }

        if(nrow(df_clean) < 3) return(desc_empty_dt("Insufficient complete observations (fewer than 3 rows without missing values)."))

        if (p_type %in% c("heatmap", "network", "correlogram", "partial")) {
           pair_vars <- if (!is.null(pcor)) vars_lab else colnames(df_clean)
           n_v <- length(pair_vars)
           res_list <- list()
           for(i in 1:(n_v-1)) {
              for(j in (i+1):n_v) {
                 ct <- if (!is.null(pcor)) {
                   list(estimate = pcor$cormat[pair_vars[i], pair_vars[j]], p.value = NA_real_)
                 } else {
                   tryCatch(cor.test(df_clean[[i]], df_clean[[j]], method = method), error=function(e) NULL)
                 }
                 if(!is.null(ct)) {
                    p_val <- ct$p.value
                    if (n_controls > 0) {
                       # The partial estimate carries no test of its own, and a
                       # cor.test on residuals would use df = n - 2, ignoring the
                       # k control variables partialled out. The p-value uses the
                       # partial-correlation df = n - 2 - k (same convention as
                       # ppcor::pcor.test).
                       r_est <- unname(ct$estimate)
                       n_obs <- pcor$n
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
                        Variable_1 = pair_vars[i],
                        Variable_2 = pair_vars[j],
                        Correlation = unname(ct$estimate),
                        p_raw = p_val,
                        stringsAsFactors = FALSE
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
              return(DT::datatable(res_df, options = list(pageLength = 10, dom = 'tip', scrollX = TRUE,
                                                          columnDefs = sig_render_defs(res_df, "Correlation")),
                                   caption = complete_case_note(nrow(df_clean), nrow(df))))
           }
        }

        cormat <- corr_matrix_reactive()
        req(cormat)
        cormat_df <- as.data.frame(cormat)

        # paging off: dom = 't' shows no paging controls, so a matrix of more
        # than ten variables lost its lower rows on screen and in a copy.
        return(DT::datatable(cormat_df, options = list(dom = 't', paging = FALSE, scrollX = TRUE,
                                                       columnDefs = sig_render_defs(cormat_df, names(cormat_df), rownames = TRUE)),
                             caption = complete_case_note(nrow(df_clean), nrow(df))))
      }
      # server = FALSE: the paged cross-correlogram and pairwise tables keep
      # every page in the browser, so the copy button reads them whole.
    }, server = FALSE)
    
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
    
    # guard: the advisory findings of check_collinearity() awaiting the user's
    # answer; refusal: why the PCA was not run; dropped_constant: the columns
    # the fitted PCA left out for having no variance.
    pca_rv <- shiny::reactiveValues(res = NULL, data = NULL, cols = NULL, groups = NULL,
                                    guard = NULL, refusal = NULL, dropped_constant = NULL,
                                    scaled = TRUE)

    # One path for Run PCA and for continuing past the guard. Complete-case
    # filter, the zero-variance exclusion and prcomp live in desc_pca_fit
    # (ui_formatting.R).
    run_pca <- function(df) {
      vars_lab <- get_var_labels(input$pca_vars, vmeta())
      fit <- tryCatch(desc_pca_fit(df, input$pca_vars, vars_lab, scale = input$pca_scale),
                      error = function(e) {
                        showNotification(paste("PCA Failed:", e$message), type = "error")
                        NULL
                      })
      if (is.null(fit)) return(invisible(NULL))
      pca_rv$guard <- NULL
      pca_rv$refusal <- fit$refusal
      pca_rv$dropped_constant <- fit$dropped_constant
      if (!is.null(fit$refusal)) {
        pca_rv$res <- NULL
        shiny::updateTextInput(session, "pca_ready_flag", value = "no")
        return(invisible(NULL))
      }
      if (fit$dropped > 0) {
        showNotification(sprintf("Warning: %d rows were dropped due to missing values (NA) in the selected variables.", fit$dropped), type = "warning", duration = 10)
      }
      pca_rv$res <- fit$res
      pca_rv$scaled <- isTRUE(input$pca_scale)
      pca_rv$data <- fit$data
      pca_rv$cols <- colnames(fit$data)
      pca_rv$groups <- if ("group_id" %in% colnames(df)) df$group_id[fit$keep] else NULL
      shiny::updateTextInput(session, "pca_ready_flag", value = "yes")
    }

    shiny::observeEvent(input$run_pca_btn, {
      req(rv_analytics_data(), input$pca_vars)
      df <- rv_filtered_analytics_data()

      if(nrow(df) < 5 || length(input$pca_vars) < 3) {
        showNotification("Insufficient data or variables for PCA.", type="error")
        return()
      }

      col_check <- check_collinearity(df, input$pca_vars, threshold = 0.95)

      if (col_check$has_collinearity) {
        # Correlated pairs and high VIF are judgement calls: stop and ask.
        pca_rv$guard <- col_check
        pca_rv$refusal <- NULL
        pca_rv$res <- NULL
        shiny::updateTextInput(session, "pca_ready_flag", value = "no")
      } else {
        run_pca(df)
      }
    })

    # Three kinds of finding, each under its own heading and sentence. Only the
    # first two are advisory, so the button to continue follows them; a
    # constant column is never a reason to stop, because it is excluded anyway.
    # The same slot carries a refusal, or, once a PCA is shown, the standing
    # note naming the columns it left out.
    output$pca_collinearity_warning_ui <- shiny::renderUI({
      lab <- function(v) get_var_labels(v, vmeta())
      if (!is.null(pca_rv$refusal)) {
        return(shiny::div(class = "alert alert-danger",
          shiny::h4(shiny::icon("ban"), "PCA not run"),
          shiny::p(pca_rv$refusal)))
      }
      g <- pca_rv$guard
      if (is.null(g)) {
        dc <- pca_rv$dropped_constant
        if (is.null(pca_rv$res) || !length(dc)) return(NULL)
        return(shiny::div(class = "mn-notice",
          shiny::icon("info-circle"),
          sprintf(" Excluded from this PCA (no variance over the analysed rows): %s. The remaining %d variables are %s.",
                  paste(dc, collapse = ", "), ncol(pca_rv$res$rotation),
                  if (isTRUE(pca_rv$scaled)) "standardised" else "centred")))
      }
      section <- function(title, sentence, items) {
        shiny::tagList(shiny::h5(style = "font-weight: 600; margin-top: 12px;", title),
                       shiny::p(sentence),
                       shiny::tags$ul(lapply(items, shiny::tags$li)))
      }
      shiny::div(class = "alert alert-warning",
        shiny::h4(shiny::icon("exclamation-triangle"), "Check the selected variables"),
        if (nrow(g$pairs) > 0) section(
          "Highly correlated pairs",
          "These pairs have |r| > 0.95. Near-duplicate variables dominate the first components and split their loadings, so the biplot understates every other variable.",
          sprintf("%s & %s (r = %s)", lab(g$pairs$var1), lab(g$pairs$var2), format_sig(g$pairs$r))),
        if (nrow(g$high_vif) > 0) section(
          "High multicollinearity (VIF > 10)",
          "These variables are predicted almost exactly by a combination of the others, so their contribution to a component is not separately identifiable.",
          sprintf("%s (VIF = %s)", lab(g$high_vif$variable),
                  ifelse(is.finite(g$high_vif$vif), format_sig(g$high_vif$vif),
                         "not finite: the correlation matrix is singular"))),
        if (length(g$constant) > 0) section(
          "No variance",
          "These variables are constant across the selected rows. They carry no information and cannot be standardised, so they are excluded from the PCA.",
          lab(g$constant)),
        shiny::p(style = "margin-top: 10px;",
                 "Remove the variables named above from the selection, or continue with them: correlated and high-VIF variables are advisory findings."),
        shiny::actionButton(ns("pca_force_btn"), "Ignore Warning & Force PCA", class = "btn-danger")
      )
    })

    shiny::observeEvent(input$pca_force_btn, {
      req(rv_analytics_data(), input$pca_vars)
      run_pca(rv_filtered_analytics_data())
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
             if (p_type == "contrib" && !isTRUE(pca_rv$scaled)) unscaled_pca_note("contrib")
          )
       } else if (p_type == "cos2") {
          shiny::tagList(
             shiny::selectInput(ns("pca_cos2_axes"), "Select PCs to evaluate", choices = 1:n_pcs, multiple = TRUE, selected = 1:min(2, n_pcs)),
             if (!isTRUE(pca_rv$scaled)) unscaled_pca_note("cos2")
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
          # The columns left out for having no variance travel with the figure,
          # including the PNG the expand modal downloads.
          dc <- pca_rv$dropped_constant
          if (inherits(p, "ggplot") && length(dc)) {
            p <- p + labs(caption = paste(c(p$labels$caption,
                                            paste("Excluded (no variance):", paste(dc, collapse = ", "))),
                                          collapse = "\n"))
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
  
       # Unrounded: a covariance PCA of small-unit variables has eigenvalues
       # that fixed decimals print as 0. The display formats them.
       df_res <- data.frame(
          PC = paste0("PC", 1:length(var_explained)),
          Eigenvalue = pca_rv$res$sdev^2,
          Variance_Explained_Pct = var_explained * 100,
          Cumulative_Variance_Pct = cum_var * 100
       )

       # paging off: dom = 't' shows no paging controls (components past the
       # tenth were unreachable on screen and missing from a copy)
       DT::datatable(df_res, options = list(dom = 't', paging = FALSE, scrollX = TRUE,
                                            columnDefs = sig_render_defs(df_res, names(df_res)[-1])),
                     rownames = FALSE)
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
