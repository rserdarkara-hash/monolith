# ui_helpers_0.9.8b.R - Modularized UI Helper Functions

# Melt a correlation matrix into a tidy dataframe
melt_cormat <- function(cormat, value_name = "Corr") {
  rn <- rownames(cormat)
  cn <- colnames(cormat)
  df <- data.frame(
    Var1 = rep(rn, each = length(cn)),
    Var2 = rep(cn, times = length(rn)),
    Value = as.vector(t(cormat)),
    stringsAsFactors = FALSE
  )
  colnames(df)[3] <- value_name
  df
}

# Universal Agronomical Colors
agro_colors <- c("#E69F00", "#F0E442", "#009E73") # Orange, Yellow, Green

get_agro_colors <- function(n) {
  if (n == 2) {
    c("#E69F00", "#009E73")
  } else if (n == 3) {
    c("#E69F00", "#F0E442", "#009E73")
  } else if (n == 4) {
    c("#E69F00", "#F0E442", "#56B4E9", "#009E73")
  } else if (n == 5) {
    c("#D55E00", "#E69F00", "#F0E442", "#56B4E9", "#009E73")
  } else {
    colorRampPalette(c("#E69F00", "#F0E442", "#009E73"))(n)
  }
}

# Classic Nutrient Palettes
nutrient_palettes <- list(
  TN = "Greens", P = "Blues", K = "Oranges", Ca = "YlOrRd", 
  Mg = "PuBuGn", Fe = "Purples", Mn = "GnBu", Cu = "YlGn", Zn = "YlOrBr"
)

# Classic Agronomical Limits
nutrient_limits <- list(
  TN = c(0.05, 0.10), P = c(8, 25), K = c(150, 300), Ca = c(1428, 2857),
  Mg = c(80, 160), Fe = c(4, 6), Mn = c(1.2, 3.5), Cu = c(0.3, 0.8), Zn = c(1, 3)
)

get_nut_key <- function(v) {
  v_up <- toupper(as.character(v))
  if (length(v_up) == 0 || is.na(v_up) || v_up == "") return(NULL)
  
  patterns <- c(
    TN = "\\bTN\\b|NITROGEN",
    P  = "\\bP\\b|PHOSPHORUS|OLSEN",
    K  = "\\bK\\b|POTASSIUM",
    Ca = "\\bCA\\b|CALCIUM",
    Mg = "\\bMG\\b|MAGNESIUM",
    Fe = "\\bFE\\b|IRON",
    Mn = "\\bMN\\b|MANGANESE",
    Cu = "\\bCU\\b|COPPER",
    Zn = "\\bZN\\b|ZINC"
  )
  
  matches <- sapply(patterns, function(pat) grepl(pat, v_up))
  if (any(matches)) return(names(patterns)[which(matches)[1]])
  return(NULL)
}

get_default_palette <- function(var_name, category = "Soil", label = NULL) {
  # Match nutrient shorthand robustly (check name then label)
  nut <- get_nut_key(var_name)
  if (is.null(nut) && !is.null(label)) nut <- get_nut_key(label)
  
  if (!is.null(nut)) return(nutrient_palettes[[nut]])
  
  # Category-based defaults
  if (is.null(category)) {
    return("YlOrRd")
  } else if (category == "Environmental Data") {
    "RdYlBu"
  } else if (category == "Landsat Data") {
    "viridis"
  } else if (category == "Sentinel Data") {
    "plasma"
  } else if (category == "Merged Data") {
    "inferno"
  } else if (category == "Terrain Data") {
    "BrBG"
  } else {
    "YlOrRd"
  }
}

method_labels <- c(
  "OK"  = "Ordinary Kriging",
  "UK"  = "Universal Kriging",
  "RK"  = "Regression Kriging",
  "RFK" = "Random Forest Kriging",
  "CK"  = "Co-Kriging",
  "IDW" = "IDW",
  "TPS" = "Thin Plate Spline"
)

get_method_label <- function(method) {
  if (is.null(method) || length(method) == 0 || is.na(method) || method == "") return("")
  if (method %in% names(method_labels)) {
    return(method_labels[[method]])
  }
  return(method)
}

buffer_multipliers <- c(
  "TPS" = 1.0,
  "IDW" = 2.0,
  "OK"  = 3.0,
  "CK"  = 3.0,
  "RK"  = 3.0,
  "RFK" = 3.0
)

get_buffer_multiplier <- function(method) {
  if (is.null(method) || length(method) == 0 || is.na(method) || method == "") return(2.0)
  if (method %in% names(buffer_multipliers)) {
    return(buffer_multipliers[[method]])
  }
  return(2.0)
}

tuning_ui <- function(id, label, 
                      global_slider_id, manual_slider_id, 
                      global_slider_args, manual_slider_args, 
                      optimize_btn_label = paste("OPTIMIZE", label),
                      manual_btn_label = paste("Apply Manual", label),
                      outer_style = NULL,
                      manual_style = "background-color: #fff9db; padding: 10px; border: 1px solid #fab005; border-radius: 4px; margin-bottom: 10px;",
                      top_extra_ui = NULL,
                      extra_ui = NULL) {
  
  content <- tagList(
    radioButtons(paste0(id, "_mode"), "Fitting Mode", 
                 choices = c("Auto-Fit" = "auto", "Manual" = "manual"), inline = TRUE),
    
    top_extra_ui,
    
    # Auto Panel
    conditionalPanel(
      condition = sprintf("input.%s_mode == 'auto'", id),
      actionButton(paste0("opt_", id), optimize_btn_label, class = "btn-info btn-block"),
      uiOutput(paste0(id, "_opt_panel")),
      do.call(sliderInput, c(list(inputId = global_slider_id), global_slider_args))
    ),
    
    # Manual Panel
    conditionalPanel(
      condition = sprintf("input.%s_mode == 'manual'", id),
      div(style = manual_style,
          h5("Manual Tuning"),
          selectInput(paste0(id, "_m_loc"), "Locality to Tune", choices = NULL),
          conditionalPanel(
              condition = "input.comp_mode == true",
              radioButtons(paste0(id, "_m_target"), "Target", 
                           choices = c("Actual" = "act", "Predicted" = "pre"), inline = TRUE)
          ),
          do.call(sliderInput, c(list(inputId = manual_slider_id), manual_slider_args)),
          actionButton(paste0("apply_", id, "_manual"), manual_btn_label, class = "btn-warning btn-block")
      )
    ),
    
    extra_ui
  )
  
  if (!is.null(outer_style)) {
    div(style = outer_style, content)
  } else {
    content
  }
}

# --- Unified Styling Engine (WYSIWYG) ---
generate_base_plot <- function(item, input, agro_params = NULL) {
  req(item)
  
  if (item$type == "map" || item$type == "map_combined") {
    
    build_map <- function(obj, label, is_tiled = FALSE) {
      if (inherits(obj, "PackedSpatRaster")) obj <- terra::unwrap(obj)
      pal_name <- input$palette_select %||% "YlOrRd"
      is_resid <- grepl("Residual", label)
      is_agro <- input$color_style == "agro" && !is_resid
      
      if (isTruthy(input$styler_high_contrast)) {
          if (!is_agro && !is_resid) pal_name <- "viridis"
      }
      
      # Strip redundant legend titles to maximize layout space
      leg_name <- NULL
      
      bp <- ggplot() + geom_spatraster(data = obj[[1]])
      
      if (is_resid) {
        vv <- as.vector(terra::values(obj[[1]], na.rm=TRUE))
        abs_max <- max(abs(vv), na.rm = TRUE)
        if(is.infinite(abs_max) || is.na(abs_max)) abs_max <- 1
        resid_pal <- if (isTruthy(input$styler_high_contrast)) "PuOr" else "RdBu"
        bp <- bp + scale_fill_distiller(palette = resid_pal, direction = 1, limits = c(-abs_max, abs_max), na.value = "transparent", name = leg_name) +
          coord_sf()
      } else if (is_agro && !is.null(agro_params)) {
        # Classification for agronomical maps
        obj_c <- terra::classify(obj[[1]], agro_params$rcl_mat, right = FALSE)
        names(obj_c) <- "category"
        
        # Add levels to help tidyterra/ggplot2 recognize it as categorical
        lvls <- data.frame(value = 1:agro_params$n_c, category = agro_params$labels)
        levels(obj_c) <- lvls
        
        a_cols <- agro_params$colors
        if (isTruthy(input$styler_high_contrast)) {
            a_cols <- viridis::viridis(agro_params$n_c)
        }
        
        bp <- ggplot() + 
          tidyterra::geom_spatraster(data = obj_c, aes(fill = category)) +
          scale_fill_manual(values = a_cols, 
                            labels = agro_params$leg_labels,
                            na.value = "transparent", name = leg_name, drop = FALSE) +
          coord_sf()
      } else {
        is_viridis <- pal_name %in% c("viridis", "magma", "inferno", "plasma", "cividis")
        if (input$color_style == "bin") {
          if(is_viridis) bp <- bp + scale_fill_viridis_b(option = pal_name, na.value = "transparent", n.breaks = 5, name = leg_name)
          else bp <- bp + scale_fill_fermenter(palette = pal_name, direction = 1, na.value = "transparent", n.breaks = 5, name = leg_name)
        } else {
          if(is_viridis) bp <- bp + scale_fill_viridis_c(option = pal_name, na.value = "transparent", name = leg_name)
          else bp <- bp + scale_fill_distiller(palette = pal_name, direction = 1, na.value = "transparent", name = leg_name)
        }
        bp <- bp + coord_sf()
      }
      bp
    }

    if (item$type == "map") {
      return(build_map(item$obj, item$label))
    } else {
      # map_combined
      p1 <- build_map(item$obj$act, "Actual", is_tiled = TRUE)
      p2 <- build_map(item$obj$pre, "Predicted", is_tiled = TRUE)
      return(list(p1 = p1, p2 = p2))
    }
    
  } else {
    return(item$obj)
  }
}

apply_styler_theme <- function(p_obj, input, calibration = 1, item_label = "", item_type = "plot") {
  req(p_obj)
  
  # 1. Scaling Logic
  s_title <- (input$styler_title_size %||% 16) * calibration
  s_base  <- (input$styler_base_size %||% 12) * calibration
  s_x     <- (input$styler_x_size %||% 12) * calibration
  s_y     <- (input$styler_y_size %||% 12) * calibration
  s_lab   <- (input$styler_label_size %||% 10) * calibration
  s_leg   <- (input$styler_legend_size %||% 10) * calibration
  
  font_f <- input$styler_font_family %||% "sans"
  
  is_combined <- identical(item_type, "map_combined")
  leg_pos <- input$styler_legend_pos %||% (if (is_combined) "bottom" else "right")
  leg_dir <- input$styler_legend_dir %||% (if (is_combined) "horizontal" else "auto")
  if (leg_dir == "auto") {
    leg_dir <- if (leg_pos %in% c("bottom", "top")) "horizontal" else "vertical"
  }
  leg_text_angle <- as.numeric(input$styler_legend_text_angle %||% (if (is_combined) 90 else 0))

  style_pane <- function(p, label, is_combined_pane = FALSE) {
    f_title <- label
    f_x <- if(isTruthy(input$styler_x_title)) input$styler_x_title else NULL
    f_y <- if(isTruthy(input$styler_y_title)) input$styler_y_title else NULL
    
    key_size <- input$styler_legend_key_size %||% 1.0
    margin_t <- (input$styler_margin_t %||% 10) * calibration
    margin_r <- (input$styler_margin_r %||% 10) * calibration
    margin_b <- (input$styler_margin_b %||% 10) * calibration
    margin_l <- (input$styler_margin_l %||% 15) * calibration
    
    if (is_combined_pane) {
      key_size <- key_size * 0.6
      margin_t <- margin_t * 0.3
      margin_r <- margin_r * 0.3
      margin_b <- margin_b * 0.3
      margin_l <- margin_l * 0.3
    }
    
    legend_theme <- if (is_combined_pane) {
      list(
        legend.key.size = unit(key_size, "cm"),
        legend.key.width = unit(key_size * 2.5, "cm"),
        legend.key.height = unit(key_size * 0.5, "cm")
      )
    } else {
      list(legend.key.size = unit(key_size, "cm"))
    }
    
    p + theme_minimal(base_size = s_base, base_family = font_f) +
      theme(
        plot.title = element_text(size = if(is_combined_pane) s_title * 0.85 else s_title, face = "bold"),
        plot.subtitle = element_text(size = s_title * 0.8),
        axis.title.x = element_text(size = s_x),
        axis.title.y = element_text(size = s_y),
        axis.text = element_text(size = s_lab),
        legend.text = element_text(
          size = if(is_combined_pane) s_leg * 0.85 else s_leg, 
          angle = leg_text_angle,
          hjust = if(leg_text_angle != 0) 0.5 else NULL,
          vjust = if(leg_text_angle != 0) 0.5 else NULL
        ),
        legend.title = element_text(size = if(is_combined_pane) s_leg * 0.85 else s_leg, face = "bold"),
        legend.position = leg_pos,
        legend.direction = leg_dir,
        panel.grid.major = if(isTRUE(input$styler_show_grid)) element_line(color = "grey90") else element_blank(),
        panel.grid.minor = if(isTRUE(input$styler_show_grid)) element_line(color = "grey95") else element_blank(),
        plot.margin = ggplot2::margin(margin_t, margin_r, margin_b, margin_l, unit = "mm"),
        axis.text.x = element_text(
          angle = as.numeric(input$styler_label_orient %||% 0),
          hjust = if(as.numeric(input$styler_label_orient %||% 0) != 0) 1 else 0.5,
          vjust = if(as.numeric(input$styler_label_orient %||% 0) != 0) 1 else 0.5
        )
      ) +
      do.call(theme, legend_theme) +
      labs(title = f_title, x = f_x, y = f_y)
  }

  if (item_type == "map_combined") {
    p1_s <- style_pane(p_obj$p1, "Actual", is_combined_pane = TRUE)
    p2_s <- style_pane(p_obj$p2, "Predicted", is_combined_pane = TRUE)
    
    main_t <- if(isTruthy(input$styler_title)) input$styler_title else item_label
    comb_key_size <- (input$styler_legend_key_size %||% 1.0) * 0.6
    
    return(p1_s + p2_s + plot_layout(ncol = 2, guides = "collect") & 
           theme(legend.position = leg_pos, 
                 legend.direction = leg_dir,
                 legend.key.size = unit(comb_key_size, "cm"),
                 legend.key.width = unit(comb_key_size * 2.5, "cm"),
                 legend.key.height = unit(comb_key_size * 0.5, "cm"),
                 legend.margin = ggplot2::margin(2, 2, 2, 2),
                 legend.box.margin = ggplot2::margin(0, 0, 0, 0),
                 legend.text = element_text(
                   size = s_leg * 0.85,
                   angle = leg_text_angle,
                   hjust = if(leg_text_angle != 0) 0.5 else NULL,
                   vjust = if(leg_text_angle != 0) 0.5 else NULL
                 )) & 
           plot_annotation(title = main_t, theme = theme(plot.title = element_text(size = s_title, face = "bold", family = font_f))))
           
  } else if (inherits(p_obj, "trellis")) {
    f_title <- if(isTruthy(input$styler_title)) input$styler_title else item_label
    f_x <- if(isTruthy(input$styler_x_title)) input$styler_x_title else "Distance"
    f_y <- if(isTruthy(input$styler_y_title)) input$styler_y_title else "Semivariance"
    
    return(update(p_obj, 
           par.settings = list(
             fontsize = list(text = s_base),
             fontfamily = font_f
           ),
           scales = list(x = list(rot = as.numeric(input$styler_label_orient %||% 0))),
           main = list(label = f_title, fontfamily = font_f, cex = s_title/s_base),
           xlab = list(label = f_x, fontfamily = font_f, cex = s_x/s_base),
           ylab = list(label = f_y, fontfamily = font_f, cex = s_y/s_base)))
           
  } else {
    t <- if(isTruthy(input$styler_title)) input$styler_title else item_label
    return(style_pane(p_obj, t))
  }
}

generate_styled_plot <- function(item, input, calibration = 1, agro_params = NULL) {
  base_p <- generate_base_plot(item, input, agro_params)
  apply_styler_theme(base_p, input, calibration, item_label = item$label, item_type = item$type)
}

# --- Shared Expanded Modal Factory (Deduplication) ---
register_expanded_modal <- function(input, output, session, btn_id, mode_id, ui_id, plot_static_id, plot_plotly_id, title_text, build_fn, radar_special = FALSE, pca_3d_special = FALSE) {
  ns <- session$ns
  
  # Dynamic evaluation helper
  is_pca_3d <- function() {
    if (is.function(pca_3d_special)) {
      pca_3d_special()
    } else if (shiny::is.reactive(pca_3d_special)) {
      pca_3d_special()
    } else {
      isTRUE(pca_3d_special)
    }
  }
  
  # Observer for button click
  shiny::observeEvent(input[[btn_id]], {
    # Special logic for 3D biplot where we don't want a View Mode selector
    mode_selector <- if (is_pca_3d()) {
      NULL
    } else {
      shiny::radioButtons(ns(mode_id), "View Mode:", choices = c("Static (High-Res)" = "static", "Interactive (Hover/Zoom)" = "interactive"), inline = TRUE)
    }
    
    shiny::showModal(shiny::modalDialog(
      title = paste0("Expanded View: ", title_text), size = "l", easyClose = TRUE,
      mode_selector,
      shiny::uiOutput(ns(ui_id)),
      footer = shiny::modalButton("Close")
    ))
  })
  
  # UI output
  output[[ui_id]] <- shiny::renderUI({
    if (is_pca_3d()) {
      plotly::plotlyOutput(ns(paste0(plot_plotly_id, "_3d")), height = "700px")
    } else {
      if (!is.null(input[[mode_id]]) && input[[mode_id]] == "interactive") {
        plotly::plotlyOutput(ns(plot_plotly_id), height = "700px")
      } else {
        shiny::plotOutput(ns(plot_static_id), height = "700px")
      }
    }
  })
  
  # Static plot
  output[[plot_static_id]] <- shiny::renderPlot({
    p <- build_fn()
    shiny::req(p)
    p
  })
  
  # Interactive plot
  output[[plot_plotly_id]] <- plotly::renderPlotly({
    p <- build_fn()
    shiny::req(p)
    # Radar chart special conversion
    if (radar_special && inherits(p, "ggplot") && nrow(p$data) > 0 && "variable" %in% colnames(p$data)) {
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
    if (inherits(p, "ggplot")) plotly::ggplotly(p) else p
  })
  
  # 3D plotly special
  output[[paste0(plot_plotly_id, "_3d")]] <- plotly::renderPlotly({
    p <- build_fn()
    shiny::req(p)
    if (inherits(p, "plotly")) return(p)
  })
}

# --- Documentation UI Components ---
render_docs_drawer <- function() {
  div(
    id = "docs_drawer",
    class = "docs-drawer",
    div(style = "display: flex; justify-content: space-between; align-items: center; border-bottom: 1px solid #eee; padding-bottom: 10px; margin-bottom: 15px;",
        h3("Documentation", style = "margin: 0;"),
        actionButton("close_docs_btn", icon("times"), class = "btn-light btn-sm", style = "border: none; background: transparent; font-size: 20px;")
    ),
    tabsetPanel(
      id = "docs_tabs",
      tabPanel("Scientific Guide", 
               uiOutput("render_scientific_guide")
      ),
      tabPanel("UI/UX Guide", 
               uiOutput("render_ui_ux_guide")
      )
    )
  )
}

info_tooltip <- function(id, text) {
  content_html <- paste0(text, "<br><br><div style='text-align: right;'><button type='button' class='btn btn-xs btn-outline-secondary' onclick='$(this).closest(\".popover\").popover(\"hide\");'>Close &times;</button></div>")
  
  tags$span(
    id = paste0(id, "_info_icon"),
    class = "info-icon",
    style = "cursor: pointer; color: #17a2b8; margin-left: 5px;",
    tabindex = "0",
    # Bootstrap 3
    `data-toggle` = "popover",
    `data-placement` = "auto",
    `data-trigger` = "focus",
    `data-content` = content_html,
    `data-html` = "true",
    # Bootstrap 4 & 5 Compatibility
    `data-bs-toggle` = "popover",
    `data-bs-placement` = "auto",
    `data-bs-trigger` = "focus",
    `data-bs-content` = content_html,
    `data-bs-html` = "true",
    onclick = "event.stopPropagation(); event.preventDefault(); if (typeof bootstrap !== 'undefined' && bootstrap.Popover) { new bootstrap.Popover(this).show(); }",
    icon("info-circle")
  )
}

# --- Shared Helpers for Labels & Group Filtering ---

get_var_label <- function(v, vars_metadata) {
  if (is.null(v) || is.na(v) || v == "") return(v)
  if (!is.null(vars_metadata)) {
    # Fallback to fuzzy match (which natively returns exact matches first)
    all_actuals <- sapply(vars_metadata, function(x) x$actual)
    fuzzy_actual <- fuzzy_match_column(v, all_actuals)
    if (!is.null(fuzzy_actual)) {
      match_fuzzy <- Filter(function(x) x$actual == fuzzy_actual, vars_metadata)
      if (length(match_fuzzy) > 0 && !is.null(match_fuzzy[[1]]$label) && match_fuzzy[[1]]$label != "") {
        return(match_fuzzy[[1]]$label)
      }
    }
  }
  return(v)
}

get_var_labels <- function(vars, vars_metadata) {
  if (is.null(vars)) return(NULL)
  sapply(vars, get_var_label, vars_metadata = vars_metadata)
}

update_premium_progress <- function(pct, message = NULL) {
  width_val <- if (is.numeric(pct)) {
    sprintf("%d%%", round(pct))
  } else if (grepl("%$", pct)) {
    pct
  } else {
    paste0(pct, "%")
  }
  
  shinyjs::runjs(sprintf("document.getElementById('map_progress_bar_inner').style.width = '%s';", width_val))
  
  if (!is.null(message)) {
    shinyjs::html("map_progress_text", message)
  }
}

fuzzy_match_column <- function(act_name, user_cols) {
  if (act_name %in% user_cols) {
    return(act_name)
  }
  if (tolower(act_name) %in% tolower(user_cols)) {
    return(user_cols[tolower(user_cols) == tolower(act_name)][1])
  }
  clean_act <- tolower(gsub("[^a-zA-Z0-9]", "", act_name))
  clean_user <- tolower(gsub("[^a-zA-Z0-9]", "", user_cols))
  if (clean_act %in% clean_user) {
    return(user_cols[clean_user == clean_act][1])
  }
  
  # Levenshtein distance based matching
  dists <- as.vector(adist(clean_act, clean_user))
  min_idx <- which.min(dists)
  if (length(min_idx) > 0) {
    min_dist <- dists[min_idx]
    # Allow small edits (up to 2 character differences) if it's not a huge fraction of the word length
    if (min_dist <= 2 && (min_dist / max(1, nchar(clean_act))) <= 0.3) {
      return(user_cols[min_idx])
    }
  }
  
  return(NULL)
}

apply_labels_to_df <- function(df, vars, vars_metadata) {
  if (is.null(df) || length(vars) == 0) return(df)
  
  labels <- get_var_labels(vars, vars_metadata)
  for (i in seq_along(vars)) {
    if (vars[i] %in% colnames(df)) {
      colnames(df)[colnames(df) == vars[i]] <- labels[i]
    }
  }
  return(df)
}

filter_active_groups <- function(df, active_groups) {
  if (is.null(df)) return(df)
  if ("group_id" %in% colnames(df)) {
    if (!is.null(active_groups) && length(active_groups) > 0) {
      df <- df[df$group_id %in% active_groups, , drop = FALSE]
    } else if (!is.null(active_groups) && length(active_groups) == 0) {
      df <- df[0, , drop = FALSE]
    }
  }
  return(df)
}

# Smart auto-detection helper for predicted columns
detect_pred_column <- function(target, candidates, type = "cve") {
  if (is.null(target) || is.na(target) || length(candidates) == 0) return(NA)
  
  patterns <- if (type == "cve") {
    c(
      paste0("^", target, "_cve$"),
      paste0("^", target, "_pred$"),
      paste0("^", target, "_predicted$"),
      paste0("^", target, "Pred$"),
      paste0("^", target, "Predicted$"),
      paste0("^pred_", target, "$"),
      paste0("^predicted_", target, "$")
    )
  } else {
    c(
      paste0("^", target, "_ss$"),
      paste0("^", target, "_split$"),
      paste0("^", target, "_test$"),
      paste0("^", target, "Split$"),
      paste0("^", target, "Test$")
    )
  }
  
  for (pat in patterns) {
    matches <- grep(pat, candidates, ignore.case = TRUE, value = TRUE)
    if (length(matches) > 0) return(matches[1])
  }
  
  return(NA)
}

match_metadata_columns <- function(m_df, user_cols) {
  cols <- colnames(m_df)
  col_act <- if (length(grep("actual|column|variable", cols, ignore.case=TRUE)) > 0) grep("actual|column|variable", cols, ignore.case=TRUE, value=TRUE)[1] else 1
  col_lab <- if (length(grep("label|name|display|ID", cols, ignore.case=TRUE)) > 0) grep("label|name|display|ID", cols, ignore.case=TRUE, value=TRUE)[1] else NA
  col_cat <- if (length(grep("cat|group|type", cols, ignore.case=TRUE)) > 0) grep("cat|group|type", cols, ignore.case=TRUE, value=TRUE)[1] else NA

  new_vars <- list()
  for (i in 1:nrow(m_df)) {
    act_name <- as.character(m_df[i, col_act])
    matched_col <- fuzzy_match_column(act_name, user_cols)

    if (!is.null(matched_col)) {
      cat_val <- if (!is.na(col_cat)) as.character(m_df[i, col_cat]) else "Uploaded Data"
      lab_val <- if (!is.na(col_lab)) as.character(m_df[i, col_lab]) else act_name

      if (is.na(act_name) || act_name == "") next

      already_mapped <- sapply(new_vars, function(x) x$actual)
      if (length(already_mapped) > 0 && matched_col %in% already_mapped) next

      p_cve <- detect_pred_column(matched_col, user_cols, "cve")
      p_ss  <- detect_pred_column(matched_col, user_cols, "ss")

      new_var <- list(
        actual = matched_col,
        pred = p_cve,
        pred_ss = p_ss,
        label = lab_val,
        category = cat_val,
        palette = "YlOrBr" 
      )
      
      new_var$palette <- get_default_palette(matched_col, cat_val, lab_val)
      
      new_vars[[length(new_vars) + 1]] <- new_var
    }
  }
  return(new_vars)
}

# --- Grouping & Discretization Logic (Phase 2) ---
discretize_numeric_var <- function(x, method = "median", custom_breaks = NULL, var_name = "") {
  prefix <- if(nchar(var_name) > 0) paste0(var_name, ": ") else ""
  if (all(is.na(x))) return(factor(rep(NA, length(x))))
  
  if (method == "median") {
    val <- median(x, na.rm = TRUE)
    lbls <- paste0(prefix, c("<= Median", "> Median"))
    return(factor(ifelse(x <= val, lbls[1], lbls[2]), levels = lbls))
  } else if (method == "mean") {
    val <- mean(x, na.rm = TRUE)
    lbls <- paste0(prefix, c("<= Mean", "> Mean"))
    return(factor(ifelse(x <= val, lbls[1], lbls[2]), levels = lbls))
  } else if (method == "tertiles") {
    q <- quantile(x, probs = c(0, 1/3, 2/3, 1), na.rm = TRUE)
    q <- unique(q)
    if (length(q) < 4) return(factor(rep(paste0(prefix, "Low Variation"), length(x))))
    lbls <- paste0(prefix, c("Low", "Medium", "High"))
    return(cut(x, breaks = q, include.lowest = TRUE, labels = lbls))
  } else if (method == "quintiles") {
    q <- quantile(x, probs = seq(0, 1, by = 0.2), na.rm = TRUE)
    q <- unique(q)
    if (length(q) < 6) return(factor(rep(paste0(prefix, "Low Variation"), length(x))))
    lbls <- paste0(prefix, c("Q1", "Q2", "Q3", "Q4", "Q5"))
    return(cut(x, breaks = q, include.lowest = TRUE, labels = lbls))
  } else if (method == "custom" && !is.null(custom_breaks)) {
    brks <- sort(unique(c(-Inf, custom_breaks, Inf)))
    lbls <- character(length(brks) - 1)
    for (i in 1:(length(brks)-1)) {
      if (i == 1) lbls[i] <- paste(prefix, "<=", brks[i+1])
      else if (i == length(brks)-1) lbls[i] <- paste(prefix, ">", brks[i])
      else lbls[i] <- paste0(prefix, "(", brks[i], "-", brks[i+1], "]")
    }
    return(cut(x, breaks = brks, include.lowest = TRUE, labels = lbls))
  }
  return(as.factor(x))
}

process_grouping_vars <- function(df, vars, types) {
  if (length(vars) == 0 || is.null(vars)) {
    df$group_id <- as.factor("All")
    return(df)
  }
  
  group_list <- list()
  for (i in seq_along(vars)) {
    v <- vars[i]
    t <- types[i]
    # Handle extended types like 'numeric_mean', 'numeric_tertiles', etc.
    if (t == "categorical") {
      group_list[[v]] <- as.factor(df[[v]])
    } else if (grepl("^numeric", t)) {
      method <- if(grepl("_", t)) sub("numeric_", "", t) else "median"
      group_list[[v]] <- discretize_numeric_var(df[[v]], method = method, var_name = v)
    } else {
      group_list[[v]] <- as.factor(df[[v]])
    }
  }
  
  if (length(vars) == 1) {
    df$group_id <- group_list[[1]]
  } else {
    df$group_id <- interaction(group_list, sep = " | ", drop = TRUE)
  }
  return(df)
}

# --- Plotting Logic (Phase 3) ---


get_stat_letters <- function(df, var_name, group_col, test_type) {
  # Standardize column names for processing
  df_proc <- df[!is.na(df[[var_name]]) & !is.na(df[[group_col]]), ]
  if (nrow(df_proc) < 3) return(NULL)
  
  df_proc[[group_col]] <- as.factor(as.character(df_proc[[group_col]]))
  n_groups <- length(levels(df_proc[[group_col]]))
  if (n_groups < 2) return(NULL)
  
  tryCatch({
    formula_str <- paste0("`", var_name, "` ~ `", group_col, "`")
    aov_res <- aov(as.formula(formula_str), data = df_proc)
    
    if (test_type == "tukey" && n_groups > 2 && requireNamespace("agricolae", quietly = TRUE)) {
      res <- agricolae::HSD.test(aov_res, group_col, console = FALSE)
      df_let <- data.frame(group = rownames(res$groups), letter = as.character(res$groups$groups))
      colnames(df_let)[1] <- group_col
      return(df_let)
    } else if (test_type == "duncan" && n_groups > 2 && requireNamespace("agricolae", quietly = TRUE)) {
      res <- agricolae::duncan.test(aov_res, group_col, console = FALSE)
      df_let <- data.frame(group = rownames(res$groups), letter = as.character(res$groups$groups))
      colnames(df_let)[1] <- group_col
      return(df_let)
    } else if (test_type == "anova" || n_groups == 2) {
      s_aov <- summary(aov_res)
      f_val <- s_aov[[1]][["F value"]][1]
      p_val <- s_aov[[1]][["Pr(>F)"]][1]
      df1 <- s_aov[[1]][["Df"]][1]
      df2 <- s_aov[[1]][["Df"]][2]
      
      p_label <- if(is.null(p_val) || is.na(p_val)) "p = N/A" else if(p_val < 0.001) "p < 0.001" else paste0("p = ", signif(p_val, 3))
      f_label <- if(is.null(f_val) || is.na(f_val)) "F = N/A" else paste0("F(", df1, ",", df2, ") = ", round(f_val, 2))
      full_label <- paste0("ANOVA: ", f_label, ", ", p_label)
      
      # Return for all groups but we will handle positioning in add_stat_layer
      df_let <- data.frame(group = levels(df_proc[[group_col]]), letter = as.character(full_label))
      colnames(df_let)[1] <- group_col
      return(df_let)
    }
  }, error = function(e) { return(NULL) })
  return(NULL)
}

add_stat_layer <- function(p, df, var_name, group_col, stat_test, stat_letter_pos, facet_var = NULL) {
   if (is.null(stat_test) || length(stat_test) == 0) return(p)
   if (!group_col %in% colnames(df)) return(p)
   
   if (!is.null(facet_var)) {
       vars_to_test <- unique(as.character(df[[facet_var]]))
       all_letters <- data.frame()
       for (v in vars_to_test) {
           sub_df <- df[df[[facet_var]] == v, ]
           l_df <- get_stat_letters(sub_df, "Value", group_col, stat_test[1])
           if (!is.null(l_df)) {
               if (stat_test[1] == "anova") {
                   # For ANOVA, only keep one label per facet to avoid clutter
                   l_df <- l_df[1, , drop=FALSE]
                   max_y <- max(sub_df$Value, na.rm = TRUE)
                   l_df$y_pos <- max_y + (max_y - min(sub_df$Value, na.rm=TRUE)) * 0.15
               } else if (stat_letter_pos == "top") {
                   max_y <- max(sub_df$Value, na.rm = TRUE)
                   l_df$y_pos <- max_y + (max_y - min(sub_df$Value, na.rm=TRUE)) * 0.1
               } else {
                   agg_df <- aggregate(Value ~ get(group_col), data = sub_df, max, na.rm = TRUE)
                   colnames(agg_df) <- c(group_col, "y_pos")
                   l_df <- merge(l_df, agg_df, by = group_col)
               }
               l_df[[facet_var]] <- v
               all_letters <- rbind(all_letters, l_df)
           }
       }
       if (nrow(all_letters) > 0) {
           # Position ANOVA in the center-top of each facet, Post-hocs above groups
           if (stat_test[1] == "anova") {
              p <- p + geom_text(data = all_letters, aes(x = -Inf, y = y_pos, label = letter), hjust = -0.1, vjust = 1, size = 3.5, fontface = "italic", inherit.aes = FALSE)
           } else {
              p <- p + geom_text(data = all_letters, aes(x = .data[[group_col]], y = y_pos, label = letter), vjust = -0.5, size = 4, fontface = "bold", inherit.aes = FALSE)
           }
       }
   } else {
       l_df <- get_stat_letters(df, var_name, group_col, stat_test[1])
       if (!is.null(l_df)) {
           if (stat_test[1] == "anova") {
               l_df <- l_df[1, , drop=FALSE]
               max_y <- max(df[[var_name]], na.rm = TRUE)
               y_pos <- max_y + (max_y - min(df[[var_name]], na.rm=TRUE)) * 0.15
               p <- p + annotate("text", x = -Inf, y = y_pos, label = l_df$letter[1], hjust = -0.1, vjust = 1, size = 4, fontface = "italic")
           } else {
               if (stat_letter_pos == "top") {
                   max_y <- max(df[[var_name]], na.rm = TRUE)
                   y_pos <- max_y + (max_y - min(df[[var_name]], na.rm=TRUE)) * 0.1
                   l_df$y_pos <- y_pos
               } else {
                   agg_df <- aggregate(df[[var_name]] ~ df[[group_col]], FUN=max, na.rm=TRUE)
                   colnames(agg_df) <- c(group_col, "y_pos")
                   l_df <- merge(l_df, agg_df, by = group_col)
               }
               p <- p + geom_text(data = l_df, aes(x = .data[[group_col]], y = y_pos, label = letter), vjust = -0.5, size = 4, fontface = "bold", inherit.aes = FALSE)
           }
       }
   }
   return(p)
}



generate_core_plot <- function(df, var_name, y_var = NULL, group_col = NULL, plot_type = "histogram", scatter_fit = "none", stat_test = NULL, stat_letter_pos = "above") {


  
  if (is.null(group_col) || !group_col %in% colnames(df)) {
    df$group_id <- factor("All")
    group_col <- "group_id"
  }
  
  p <- ggplot() + theme_minimal()
  
  if (plot_type %in% c("boxplot", "violin") && !is.null(y_var) && y_var != "") {
    # Dual variable facet for different scales

    df_long <- pivot_longer(df, cols = c(all_of(var_name), all_of(y_var)), names_to = "Variable", values_to = "Value")
    
    if (plot_type == "boxplot") {
      p <- ggplot(df_long, aes(x = .data[[group_col]], y = Value, fill = .data[[group_col]])) + geom_boxplot()
    } else {
      p <- ggplot(df_long, aes(x = .data[[group_col]], y = Value, fill = .data[[group_col]])) + geom_violin(alpha = 0.7)
    }
    p <- p + facet_wrap(~Variable, scales = "free_y") + theme_minimal() + 
         labs(title = paste(tools::toTitleCase(plot_type), "Comparison"), x = "Group", y = "Value", fill = "Group")
    p <- add_stat_layer(p, df_long, "Value", group_col, stat_test, stat_letter_pos, facet_var = "Variable")
  } else {
    # Normal plotting
    p <- ggplot(df) + theme_minimal()
    
    if (plot_type == "histogram") {
      p <- p + geom_histogram(aes(x = .data[[var_name]], fill = .data[[group_col]]), alpha = 0.7, position = "identity", bins = 30)
    } else if (plot_type == "density") {
      p <- p + geom_density(aes(x = .data[[var_name]], fill = .data[[group_col]], color = .data[[group_col]]), alpha = 0.5)
    } else if (plot_type == "boxplot") {
      p <- p + geom_boxplot(aes(x = .data[[group_col]], y = .data[[var_name]], fill = .data[[group_col]]))
    } else if (plot_type == "violin") {
      p <- p + geom_violin(aes(x = .data[[group_col]], y = .data[[var_name]], fill = .data[[group_col]]), alpha = 0.7)
    } else if (plot_type == "scatter") {
      if (!is.null(y_var) && y_var %in% colnames(df) && y_var != "") {
        p <- p + geom_point(aes(x = .data[[var_name]], y = .data[[y_var]], color = .data[[group_col]]), alpha = 0.8)
      } else {
        df$index_seq <- seq_len(nrow(df))
        p <- p + geom_point(aes(x = .data[["index_seq"]], y = .data[[var_name]], color = .data[[group_col]]), alpha = 0.8)
      }
      
      # Add fit line if requested
      if (!is.null(scatter_fit) && scatter_fit != "none") {
         fit_methods <- list(
           linear = list(method = "lm", se = FALSE),
           loess = list(method = "loess", se = FALSE),
           polynomial = list(method = "lm", formula = y ~ poly(x, 2), se = FALSE),
           gam = list(method = "gam", formula = y ~ s(x, bs = "cs"), se = FALSE)
         )
         fit_params <- fit_methods[[scatter_fit]]
         if (!is.null(fit_params)) {
           x_aes <- if (!is.null(y_var) && y_var != "") .data[[var_name]] else .data[["index_seq"]]
           y_aes <- if (!is.null(y_var) && y_var != "") .data[[y_var]] else .data[[var_name]]
           fit_params$mapping <- aes(x = x_aes, y = y_aes, color = .data[[group_col]])
           p <- p + do.call(geom_smooth, fit_params)
         }
      }
      
    } else if (plot_type == "ecdf") {
      p <- p + stat_ecdf(aes(x = .data[[var_name]], color = .data[[group_col]]), geom = "step", linewidth = 1)
    }
    
    if (plot_type %in% c("boxplot", "violin")) {
      p <- add_stat_layer(p, df, var_name, group_col, stat_test, stat_letter_pos)
    }
    
    p <- p + labs(title = paste(tools::toTitleCase(plot_type), "of", var_name),
                  fill = "Group", color = "Group")
  }
  
  return(p)
}

generate_ghosted_plot <- function(df_global, df_local, var_name, y_var = NULL, group_col = NULL, plot_type = "histogram") {

  
  if (is.null(group_col) || !group_col %in% colnames(df_local)) {
    df_local$group_id <- factor("All")
    df_global$group_id <- factor("All")
    group_col <- "group_id"
  }
  
  p <- ggplot() + theme_minimal()
  
  if (plot_type == "histogram") {
    p <- p + geom_histogram(data = df_global, aes(x = .data[[var_name]]), fill = "lightgray", alpha = 0.5, bins = 30) +
             geom_histogram(data = df_local, aes(x = .data[[var_name]], fill = .data[[group_col]]), alpha = 0.7, position = "identity", bins = 30)
  } else if (plot_type == "density") {
    p <- p + geom_density(data = df_global, aes(x = .data[[var_name]]), fill = "lightgray", color = "gray", alpha = 0.3) +
             geom_density(data = df_local, aes(x = .data[[var_name]], fill = .data[[group_col]], color = .data[[group_col]]), alpha = 0.5)
  } else if (plot_type == "boxplot") {
    p <- p + geom_boxplot(data = df_global, aes(x = "Global", y = .data[[var_name]]), fill = "lightgray", color = "gray") +
             geom_boxplot(data = df_local, aes(x = .data[[group_col]], y = .data[[var_name]], fill = .data[[group_col]]))
  } else if (plot_type == "violin") {
    p <- p + geom_violin(data = df_global, aes(x = "Global", y = .data[[var_name]]), fill = "lightgray", color = "gray") +
             geom_violin(data = df_local, aes(x = .data[[group_col]], y = .data[[var_name]], fill = .data[[group_col]]), alpha = 0.7)
  } else if (plot_type == "scatter") {
    if (!is.null(y_var) && y_var %in% colnames(df_local)) {
      p <- p + geom_point(data = df_global, aes(x = .data[[var_name]], y = .data[[y_var]]), color = "lightgray", alpha = 0.3) +
               geom_point(data = df_local, aes(x = .data[[var_name]], y = .data[[y_var]], color = .data[[group_col]]), alpha = 0.8)
    } else {
      df_global$index_seq <- seq_len(nrow(df_global))
      df_local$index_seq <- seq_len(nrow(df_local))
      p <- p + geom_point(data = df_global, aes(x = .data[["index_seq"]], y = .data[[var_name]]), color = "lightgray", alpha = 0.3) +
               geom_point(data = df_local, aes(x = .data[["index_seq"]], y = .data[[var_name]], color = .data[[group_col]]), alpha = 0.8)
    }
  } else if (plot_type == "ecdf") {
    p <- p + stat_ecdf(data = df_global, aes(x = .data[[var_name]]), geom = "step", color = "lightgray", linewidth = 1) +
             stat_ecdf(data = df_local, aes(x = .data[[var_name]], color = .data[[group_col]]), geom = "step", linewidth = 1)
  }
  
  p <- p + labs(title = paste("Ghosted", tools::toTitleCase(plot_type), "of", var_name),
                x = var_name, fill = "Group", color = "Group")
  
  return(p)
}

generate_advanced_plot <- function(df, vars, group_col = NULL, plot_type = "qq", xyz_fit = "linear", stat_test = NULL, stat_letter_pos = "above") {


  
  if (is.null(group_col) || !group_col %in% colnames(df)) {
    df$group_id <- factor("All")
    group_col <- "group_id"
  }
  
  p <- ggplot() + theme_minimal()
  v1 <- if(length(vars) > 0 && isTruthy(vars[1]) && vars[1] != "") vars[1] else NULL
  v2 <- if(length(vars) > 1 && isTruthy(vars[2]) && vars[2] != "") vars[2] else NULL
  v3 <- if(length(vars) > 2 && isTruthy(vars[3]) && vars[3] != "") vars[3] else NULL
  
  if (plot_type == "qq") {
    p <- ggplot(df, aes(sample = .data[[v1]], color = .data[[group_col]])) + 
         stat_qq() + stat_qq_line() + labs(x="Theoretical", y="Sample", title="QQ Plot")
  } else if (plot_type == "sinaplot") {
    if (!is.null(v2) && v2 != "") {
       # Secondary Variable Faceting Mode for Sina
       df_long <- tidyr::pivot_longer(df, cols = c(all_of(v1), all_of(v2)), names_to = "Variable", values_to = "Value")
       p <- ggplot(df_long, aes(x = .data[[group_col]], y = Value, fill = .data[[group_col]])) + 
            geom_violin(alpha=0.5, color=NA) + 
            geom_jitter(aes(color = .data[[group_col]]), width = 0.2, alpha=0.7) +
            facet_wrap(~Variable, scales="free_y") +
            labs(title="Sina-style Plot Comparison")
       
       p <- add_stat_layer(p, df_long, "Value", group_col, stat_test, stat_letter_pos, facet_var = "Variable")
    } else {
       p <- ggplot(df, aes(x = .data[[group_col]], y = .data[[v1]], fill = .data[[group_col]])) + 
            geom_violin(alpha=0.5, color=NA) + 
            geom_jitter(aes(color = .data[[group_col]]), width = 0.2, alpha=0.7) + 
            labs(title="Sina-style Plot")
       
       p <- add_stat_layer(p, df, v1, group_col, stat_test, stat_letter_pos)
    }
  } else if (plot_type %in% c("ridge", "joyplot")) {
    p <- ggplot(df, aes(x = .data[[v1]], fill = .data[[group_col]])) + 
         geom_density(alpha = 0.6) + 
         facet_grid(as.formula(paste(group_col, "~ ."))) +
         labs(title="Ridge/Joyplot Proxy")
  } else if (plot_type == "density_heatmap") {
    if (!is.null(v2)) {
      p <- ggplot(df, aes(x = .data[[v1]], y = .data[[v2]])) + 
           geom_density_2d_filled(alpha = 0.9) +
           labs(title="2D Density Heatmap")
    } else {
      p <- ggplot() + annotate("text", x=0, y=0, label="Density Heatmap requires two numeric variables")
    }
  } else if (plot_type == "parallel") {
    if (length(vars) > 1) {
      df_sub <- df[, c(vars, group_col), drop=FALSE]
      df_sub$id <- seq_len(nrow(df_sub))
      long_df <- data.frame(id = integer(), variable = character(), value = numeric(), group = character())
      for(v in vars) {
        if(is.numeric(df_sub[[v]])) {
          rng <- range(df_sub[[v]], na.rm=TRUE)
          norm_val <- if(diff(rng) > 0) (df_sub[[v]] - rng[1]) / diff(rng) else 0
          long_df <- rbind(long_df, data.frame(id=df_sub$id, variable=v, value=norm_val, group=as.character(df_sub[[group_col]])))
        }
      }
      p <- ggplot(long_df, aes(x=variable, y=value, group=id, color=group)) + 
           geom_line(alpha=0.4) + labs(title="Parallel Coordinates")
    } else {
      p <- ggplot() + annotate("text", x=0, y=0, label="Parallel coords requires >=2 vars")
    }
  } else if (plot_type == "radar") {
    if (length(vars) > 2) {
      agg <- aggregate(df[, vars, drop=FALSE], by=list(group=df[[group_col]]), FUN=mean, na.rm=TRUE)
      long_df <- data.frame(group = character(), variable = character(), value = numeric())
      for(v in vars) {
        rng <- range(df[[v]], na.rm=TRUE)
        norm_val <- if(diff(rng) > 0) (agg[[v]] - rng[1])/diff(rng) else 0
        long_df <- rbind(long_df, data.frame(group=agg$group, variable=v, value=norm_val))
      }
      p <- ggplot(long_df, aes(x=variable, y=value, group=group, color=group, fill=group)) +
           geom_polygon(alpha=0.2) + geom_point() + coord_polar() + 
           theme_minimal() + labs(title="Radar Chart (Normalized Means)")
    } else {
      p <- ggplot() + annotate("text", x=0, y=0, label="Radar requires >=3 vars")
    }
  } else if (plot_type == "xyz_surface") {
    if (!is.null(v1) && !is.null(v2) && !is.null(v3)) {
      df_clean <- na.omit(df[, c(v1, v2, v3)])
      if(nrow(df_clean) > 10) {
        grid_x <- seq(min(df_clean[[v1]]), max(df_clean[[v1]]), length.out=50)
        grid_y <- seq(min(df_clean[[v2]]), max(df_clean[[v2]]), length.out=50)
        grid <- expand.grid(x=grid_x, y=grid_y)
        # Safe names for modeling to avoid mgcv/loess parsing errors with special chars
        df_safe <- df_clean
        colnames(df_safe) <- c("var1_safe", "var2_safe", "var3_safe")
        
        # Grid prediction
        x_seq <- seq(min(df_safe[["var1_safe"]]), max(df_safe[["var1_safe"]]), length.out=50)
        y_seq <- seq(min(df_safe[["var2_safe"]]), max(df_safe[["var2_safe"]]), length.out=50)
        grid <- expand.grid(var1_safe=x_seq, var2_safe=y_seq)
        
        mod <- NULL
        try({
          if(xyz_fit == "linear") mod <- lm(var3_safe ~ var1_safe + var2_safe, data=df_safe)
          else if(xyz_fit == "loess") mod <- loess(var3_safe ~ var1_safe * var2_safe, data=df_safe, span=0.7)
          else if(xyz_fit == "polynomial") mod <- lm(var3_safe ~ poly(var1_safe,2) + poly(var2_safe,2), data=df_safe)
          else if(xyz_fit == "gam") {
            if(requireNamespace("mgcv", quietly=TRUE)) {
              mod <- mgcv::gam(var3_safe ~ s(var1_safe) + s(var2_safe), data=df_safe)
            } else {
              mod <- lm(var3_safe ~ var1_safe + var2_safe, data=df_safe)
            }
          } else if(xyz_fit == "tps") {
            if(requireNamespace("fields", quietly=TRUE)) {
              mod <- fields::Tps(df_safe[,c("var1_safe","var2_safe")], df_safe[["var3_safe"]])
            }
          }
        }, silent=TRUE)
        
        if(!is.null(mod)) {
          if(xyz_fit == "tps") {
            grid[["var3_safe"]] <- as.vector(predict(mod, x=as.matrix(grid[,c("var1_safe","var2_safe")])))
          } else {
            grid[["var3_safe"]] <- as.vector(predict(mod, newdata=grid))
          }
          
          # Map safe names back to original for plotting
          colnames(grid) <- c(v1, v2, v3)
          
          # Use geom_tile + geom_contour instead of geom_contour_filled for Plotly compatibility
          p <- ggplot(grid, aes(x=.data[[v1]], y=.data[[v2]], z=.data[[v3]])) +
               geom_tile(aes(fill=.data[[v3]])) +
               geom_contour(color="white", alpha=0.5) +
               scale_fill_viridis_c() +
               labs(title=paste("XYZ Surface (", xyz_fit, ")", sep=""))
        } else {
          p <- ggplot() + annotate("text", x=0, y=0, label="Model fitting failed")
        }
      } else {
        p <- ggplot() + annotate("text", x=0, y=0, label="Not enough data for surface")
      }
    } else {
      p <- ggplot() + annotate("text", x=0, y=0, label="XYZ Surface requires 3 numeric variables.\nPlease select 3 variables in the sidebar.") + theme_void()
    }
  }
  
  return(p)
}

# --- Phase 4: Correlation Logic ---

generate_correlation_heatmap <- function(df, vars, method = "pearson", cormat = NULL) {

  if (length(vars) < 2) return(ggplot() + annotate("text", x=0, y=0, label="Need >=2 variables"))
  
  df_clean <- na.omit(df[, vars, drop=FALSE])
  if (nrow(df_clean) < 3) return(ggplot() + annotate("text", x=0, y=0, label="Insufficient data"))
  
  if (is.null(cormat)) {
    cormat <- cor(df_clean, method = method)
  }
  
  # Hierarchical clustering
  distmat <- as.dist(1 - abs(cormat))
  hc <- hclust(distmat)
  ordered_vars <- vars[hc$order]
  
  # Melt manually using unified helper
  cormat_df <- melt_cormat(cormat, "Corr")
  
  cormat_df$Var1 <- factor(cormat_df$Var1, levels = ordered_vars)
  cormat_df$Var2 <- factor(cormat_df$Var2, levels = rev(ordered_vars))
  
  p <- ggplot(cormat_df, aes(x=Var1, y=Var2, fill=Corr)) + 
    geom_tile(color = "white") +
    geom_text(aes(label = round(Corr, 2)), color = ifelse(abs(cormat_df$Corr) > 0.5, "white", "black"), size=3) +
    scale_fill_gradient2(low = "red", high = "blue", mid = "white", midpoint = 0, limit = c(-1,1), name=paste(tools::toTitleCase(method), "\nCorrelation")) +
    theme_minimal() + 
    theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1)) +
    labs(x="", y="", title="Hierarchical Clustering Correlation Heatmap")
  return(p)
}

generate_correlation_network <- function(df, vars, threshold = 0.3, method = "pearson", cormat = NULL) {

  if (length(vars) < 2) return(ggplot() + annotate("text", x=0, y=0, label="Need >=2 variables"))
  
  df_clean <- na.omit(df[, vars, drop=FALSE])
  if (nrow(df_clean) < 3) return(ggplot() + annotate("text", x=0, y=0, label="Insufficient data"))
  
  if (is.null(cormat)) {
    cormat <- cor(df_clean, method = method)
  }
  
  n <- length(vars)
  angles <- seq(0, 2*pi, length.out = n + 1)[1:n]
  nodes <- data.frame(
    name = vars,
    x = cos(angles),
    y = sin(angles)
  )
  
  edges <- data.frame(from=character(), to=character(), x=numeric(), y=numeric(), xend=numeric(), yend=numeric(), weight=numeric(), sign=character())
  
  for(i in 1:(n-1)) {
    for(j in (i+1):n) {
      w <- cormat[i, j]
      if (abs(w) >= threshold) {
        edges <- rbind(edges, data.frame(
          from = vars[i], to = vars[j],
          x = nodes$x[i], y = nodes$y[i],
          xend = nodes$x[j], yend = nodes$y[j],
          weight = abs(w),
          sign = ifelse(w > 0, "Positive", "Negative")
        ))
      }
    }
  }
  
  p <- ggplot() + theme_void() + labs(title = paste("Correlation Network (threshold >", threshold, ")"))
  
  if (nrow(edges) > 0) {
    p <- p + geom_segment(data = edges, aes(x=x, y=y, xend=xend, yend=yend, color=sign, linewidth=weight), alpha=0.6) +
             scale_linewidth_continuous(range = c(0.5, 3)) +
             scale_color_manual(values = c("Positive" = "blue", "Negative" = "red"))
  }
  
  p <- p + geom_point(data = nodes, aes(x=x, y=y), size=10, color="lightblue") +
           geom_text(data = nodes, aes(x=x, y=y, label=name), fontface="bold")
           
  return(p)
}

generate_partial_correlation <- function(df, vars, control_vars = NULL, method = "pearson") {

  if (length(vars) < 2) return(ggplot() + annotate("text", x=0, y=0, label="Need >=2 variables to correlate"))
  
  all_vars <- unique(c(vars, control_vars))
  df_clean <- na.omit(df[, all_vars, drop=FALSE])
  if (nrow(df_clean) < 5) return(ggplot() + annotate("text", x=0, y=0, label="Insufficient data"))
  
  if (!is.null(control_vars) && length(control_vars) > 0) {
    # Partial out control_vars from vars using lm residuals
    res_list <- list()
    formula_rhs <- paste(control_vars, collapse=" + ")
    for (v in vars) {
      mod <- lm(as.formula(paste(v, "~", formula_rhs)), data=df_clean)
      res_list[[v]] <- residuals(mod)
    }
    df_clean <- as.data.frame(res_list)
  }
  
  cormat <- cor(df_clean[, vars, drop=FALSE], method = method)
  
  cormat_df <- melt_cormat(cormat, "pCorr")
  
  cormat_df$Var1 <- factor(cormat_df$Var1, levels = vars)
  cormat_df$Var2 <- factor(cormat_df$Var2, levels = rev(vars))
  
  p <- ggplot(cormat_df, aes(x=Var1, y=Var2, fill=pCorr)) + 
    geom_tile(color = "white") +
    geom_text(aes(label = round(pCorr, 2)), color = ifelse(abs(cormat_df$pCorr) > 0.5, "white", "black"), size=3) +
    scale_fill_gradient2(low = "red", high = "blue", mid = "white", midpoint = 0, limit = c(-1,1), name="Partial\nCorrelation") +
    theme_minimal() + 
    theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1)) +
    labs(x="", y="", title=ifelse(is.null(control_vars) || length(control_vars)==0, 
                                  "Standard Correlation Heatmap", 
                                  paste("Partial Correlation (Controlling for", length(control_vars), "vars)")))
  return(p)
}

generate_correlogram <- function(df, vars, method = "pearson", cormat = NULL) {

  if (length(vars) < 2) return(ggplot() + annotate("text", x=0, y=0, label="Need >=2 variables"))
  
  df_clean <- na.omit(df[, vars, drop=FALSE])
  if (nrow(df_clean) < 3) return(ggplot() + annotate("text", x=0, y=0, label="Insufficient data"))
  
  if (is.null(cormat)) {
    cormat <- cor(df_clean, method = method)
  }
  
  cormat_df <- melt_cormat(cormat, "Corr")
  
  cormat_df$Var1 <- factor(cormat_df$Var1, levels = vars)
  cormat_df$Var2 <- factor(cormat_df$Var2, levels = rev(vars))
  
  p <- ggplot(cormat_df, aes(x=Var1, y=Var2, color=Corr, size=abs(Corr))) + 
    geom_point() +
    scale_size_continuous(range = c(1, 15), guide="none") +
    scale_color_gradient2(low = "red", high = "blue", mid = "white", midpoint = 0, limit = c(-1,1)) +
    theme_minimal() + 
    theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1)) +
    labs(x="", y="", title="Correlogram")
  return(p)
}

generate_lagged_correlation <- function(df, var1, var2, max_lag = 10) {

  
  if (is.null(var1) || is.null(var2) || !var1 %in% colnames(df) || !var2 %in% colnames(df)) {
     return(ggplot() + annotate("text", x=0, y=0, label="Invalid variables specified"))
  }
  
  df_clean <- na.omit(df[, c(var1, var2)])
  if (nrow(df_clean) < max_lag + 3) return(ggplot() + annotate("text", x=0, y=0, label="Insufficient data for lags"))
  
  ccf_res <- ccf(df_clean[[var1]], df_clean[[var2]], lag.max = max_lag, plot = FALSE)
  
  plot_df <- data.frame(Lag = ccf_res$lag[,1,1], ACF = ccf_res$acf[,1,1])
  
  p <- ggplot(plot_df, aes(x=Lag, y=ACF)) + 
    geom_segment(aes(xend=Lag, yend=0), linewidth = 1) + 
    geom_hline(yintercept = 0, color="gray") +
    geom_hline(yintercept = c(-1.96/sqrt(nrow(df_clean)), 1.96/sqrt(nrow(df_clean))), linetype="dashed", color="blue") +
    theme_minimal() +
    labs(title=sprintf("Lagged Correlation (CCF): %s vs %s", var1, var2), y="Cross-Correlation")
  return(p)
}

# --- Phase 5: PCA Logic ---
check_collinearity <- function(df, vars, threshold = 0.95) {
  res <- detect_multicollinearity_engine(df, vars = vars, pairwise_threshold = threshold, vif_threshold = 10)
  
  has_coll <- res$has_collinearity || (length(res$dropped) > 0)
  
  pairs_df <- res$pairs
  if (length(res$dropped) > 0) {
    if (is.null(pairs_df)) {
      pairs_df <- data.frame(var1 = character(), var2 = character(), r = numeric(), stringsAsFactors = FALSE)
    }
    for (d_var in res$dropped) {
      pairs_df <- rbind(pairs_df, data.frame(
        var1 = d_var,
        var2 = "High VIF (> 10)",
        r = NA,
        stringsAsFactors = FALSE
      ))
    }
  }
  
  return(list(has_collinearity = has_coll, pairs = pairs_df))
}

generate_pca_scree <- function(pca_res) {

  var_explained <- pca_res$sdev^2 / sum(pca_res$sdev^2)
  df_scree <- data.frame(PC = 1:length(var_explained), Variance = var_explained)
  
  p <- ggplot(df_scree, aes(x = PC, y = Variance)) +
    geom_bar(stat = "identity", fill = "steelblue", alpha=0.7) +
    geom_line(color = "red", linewidth=1) +
    geom_point(color = "red", size=2) +
    scale_x_continuous(breaks = 1:nrow(df_scree)) +
    theme_minimal() +
    labs(title = "Scree Plot (Variance Explained by PC)", y = "Proportion of Variance", x = "Principal Component")
  return(p)
}

generate_pca_biplot <- function(pca_res, original_df, pc_x = 1, pc_y = 2, group_col = NULL) {

  scores <- as.data.frame(pca_res$x)
  
  if (!is.null(group_col) && group_col %in% colnames(original_df)) {
    scores$Group <- original_df[[group_col]]
  } else {
    scores$Group <- "All"
  }
  
  loadings <- as.data.frame(pca_res$rotation)
  var_exp <- round(pca_res$sdev^2 / sum(pca_res$sdev^2) * 100, 1)
  
  mult <- min(
    (max(scores[, pc_x]) - min(scores[, pc_x])) / (max(loadings[, pc_x]) - min(loadings[, pc_x])),
    (max(scores[, pc_y]) - min(scores[, pc_y])) / (max(loadings[, pc_y]) - min(loadings[, pc_y]))
  ) * 0.7
  
  loadings_scaled <- loadings * mult
  
  p <- ggplot() +
    geom_point(data = scores, aes(x = .data[[paste0("PC", pc_x)]], y = .data[[paste0("PC", pc_y)]], color = .data[["Group"]]), alpha = 0.6) +
    geom_segment(data = loadings_scaled, aes(x = 0, y = 0, xend = .data[[paste0("PC", pc_x)]], yend = .data[[paste0("PC", pc_y)]]), arrow = arrow(length = unit(0.2, "cm")), color = "red", alpha=0.8) +
    geom_text(data = loadings_scaled, aes(x = .data[[paste0("PC", pc_x)]], y = .data[[paste0("PC", pc_y)]], label = rownames(loadings_scaled)), color = "darkred", vjust = "outward", hjust = "outward", size=4) +
    theme_minimal() +
    labs(title = paste("Biplot (PC", pc_x, " vs PC", pc_y, ")", sep=""),
         x = paste("PC", pc_x, " (", var_exp[pc_x], "%)", sep=""),
         y = paste("PC", pc_y, " (", var_exp[pc_y], "%)", sep=""))
  
  return(p)
}

generate_pca_loadings <- function(pca_res, pc = 1) {

  loadings <- pca_res$rotation[, pc]
  df_load <- data.frame(Variable = names(loadings), Loading = loadings)
  df_load <- df_load[order(abs(df_load$Loading), decreasing = TRUE), ]
  df_load$Variable <- factor(df_load$Variable, levels = rev(df_load$Variable))
  
  p <- ggplot(df_load, aes(x = Variable, y = Loading, fill = Loading > 0)) +
    geom_bar(stat = "identity") +
    coord_flip() +
    theme_minimal() +
    scale_fill_manual(values = c("TRUE" = "steelblue", "FALSE" = "indianred"), guide="none") +
    labs(title = paste("Loadings for PC", pc), x = "Variable", y = "Loading Weight")
  return(p)
}

generate_pca_contribution <- function(pca_res, pc = 1) {

  # Contribution = (loading^2 / eigenvalue^2) * 100
  # For prcomp, sdev is sqrt(eigenvalue), rotation is loadings
  loadings <- pca_res$rotation[, pc]
  eig <- pca_res$sdev[pc]^2
  contrib <- (loadings^2 / eig) * 100
  
  df_cont <- data.frame(Variable = names(contrib), Contribution = contrib)
  df_cont <- df_cont[order(df_cont$Contribution, decreasing = TRUE), ]
  df_cont$Variable <- factor(df_cont$Variable, levels = rev(df_cont$Variable))
  
  # Expected average contribution
  exp_cont <- 100 / length(contrib)
  
  p <- ggplot(df_cont, aes(x = Variable, y = Contribution)) +
    geom_bar(stat = "identity", fill = "coral", alpha=0.8) +
    geom_hline(yintercept = exp_cont, linetype = "dashed", color = "red") +
    coord_flip() +
    theme_minimal() +
    labs(title = paste("Variable Contribution to PC", pc), 
         x = "Variable", 
         y = "Contribution (%)",
         caption = "Dashed line indicates expected average contribution")
  return(p)
}

generate_pca_cos2 <- function(pca_res, axes = 1:2) {

  # cos2 = loading^2 * eigenvalue / sum(eigenvalues) -> wait, cos2 = loading^2 for standardized PCA
  loadings <- pca_res$rotation[, axes, drop=FALSE]
  cos2 <- rowSums(loadings^2)
  
  df_cos2 <- data.frame(Variable = names(cos2), Cos2 = cos2)
  df_cos2 <- df_cos2[order(df_cos2$Cos2, decreasing = TRUE), ]
  df_cos2$Variable <- factor(df_cos2$Variable, levels = rev(df_cos2$Variable))
  
  p <- ggplot(df_cos2, aes(x = Variable, y = Cos2)) +
    geom_bar(stat = "identity", fill = "mediumseagreen", alpha=0.8) +
    coord_flip() +
    theme_minimal() +
    labs(title = paste("Quality of Representation (cos2) on PC", paste(axes, collapse=" & ")), 
         x = "Variable", y = "cos2")
  return(p)
}

generate_pca_cumvar <- function(pca_res) {

  var_explained <- pca_res$sdev^2 / sum(pca_res$sdev^2)
  cum_var <- cumsum(var_explained)
  
  df_cum <- data.frame(PC = 1:length(cum_var), CumVar = cum_var)
  
  p <- ggplot(df_cum, aes(x = PC, y = CumVar)) +
    geom_line(color = "darkblue", linewidth=1.2) +
    geom_point(color = "orange", size=3) +
    geom_hline(yintercept = 0.8, linetype="dashed", color="red", alpha=0.6) +
    scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
    scale_x_continuous(breaks = 1:nrow(df_cum)) +
    theme_minimal() +
    labs(title = "Cumulative Variance Explained", 
         x = "Principal Component", y = "Cumulative Variance",
         caption = "Dashed line indicates 80% threshold")
  return(p)
}

generate_pca_mahalanobis <- function(pca_res) {

  # Mahalanobis distance of observations in PCA space
  scores <- as.data.frame(pca_res$x)
  center <- colMeans(scores)
  cov_mat <- cov(scores)
  
  md <- mahalanobis(scores, center, cov_mat)
  df_md <- data.frame(Index = 1:length(md), Distance = md)
  
  # Threshold based on Chi-Square distribution
  thresh <- qchisq(0.975, df = ncol(scores))
  
  p <- ggplot(df_md, aes(x = Index, y = Distance)) +
    geom_point(aes(color = Distance > thresh), size=2, alpha=0.8) +
    geom_segment(aes(x=Index, xend=Index, y=0, yend=Distance, color=Distance > thresh)) +
    geom_hline(yintercept = thresh, linetype="dashed", color="red") +
    scale_color_manual(values = c("TRUE" = "red", "FALSE" = "black"), guide="none") +
    theme_minimal() +
    labs(title = "Mahalanobis Distance (Outlier Detection)",
         x = "Observation Index", y = "Distance",
         caption = "Dashed line indicates 97.5% Chi-Square threshold")
  return(p)
}

generate_pca_biplot_3d <- function(pca_res, df, pc_x=1, pc_y=2, pc_z=3, group_col=NULL) {

  
  scores <- as.data.frame(pca_res$x)
  if (!is.null(group_col) && group_col %in% colnames(df)) {
    scores$Group <- df[[group_col]]
  } else {
    scores$Group <- factor("All")
  }
  
  var_exp <- round(pca_res$sdev^2 / sum(pca_res$sdev^2) * 100, 1)
  
  p <- plot_ly(scores, x = ~get(paste0("PC", pc_x)), y = ~get(paste0("PC", pc_y)), z = ~get(paste0("PC", pc_z)), 
               color = ~Group, type = "scatter3d", mode = "markers",
               marker = list(size = 4, opacity = 0.8)) %>%
       layout(title = "3D PCA Biplot",
              scene = list(
                xaxis = list(title = paste0("PC", pc_x, " (", var_exp[pc_x], "%)")),
                yaxis = list(title = paste0("PC", pc_y, " (", var_exp[pc_y], "%)")),
                zaxis = list(title = paste0("PC", pc_z, " (", var_exp[pc_z], "%)"))
              ))
  return(p)
}

# --- Map Viewer UI Components ---
render_locality_pan_input <- function(loc_names) {
  choices <- c("Global View" = "global")
  if (length(loc_names) > 0) {
    choices <- c(choices, loc_names)
  }
  selectInput("locality_pan", NULL,
              choices = choices,
              selected = "global", width = "160px", selectize = FALSE)
}

# F2: Tableau10 palette constant (not in RColorBrewer)
TABLEAU10 <- c("#4e79a7","#f28e2b","#e15759","#76b7b2","#59a14f",
               "#edc948","#b07aa1","#ff9da7","#9c755f","#bab0ac")

# F2: Generate palette colors for a set of groups
generate_group_palette <- function(groups, palette_name = "Set1") {
  n <- length(groups)
  if (n == 0) return(character(0))

  if (palette_name == "Tableau10") {
    colors <- rep_len(TABLEAU10, n)
  } else {
    max_n <- RColorBrewer::brewer.pal.info[palette_name, "maxcolors"]
    colors <- RColorBrewer::brewer.pal(min(max(n, 3), max_n), palette_name)
    if (n > max_n) colors <- grDevices::colorRampPalette(colors)(n)
    colors <- colors[seq_len(n)]
  }
  stats::setNames(colors, groups)
}

# F2: Add styled sample points to a leaflet map
add_styled_points <- function(map, pts_sf, color_by = "none", custom_colors = NULL,
                              show_labels = FALSE, label_field = "none",
                              label_size = 11, marker_size = 3,
                              popup_fn = NULL) {

  pts_view <- if (sf::st_crs(pts_sf)$epsg != 4326) sf::st_transform(pts_sf, 4326) else pts_sf
  if (nrow(pts_view) == 0) return(map)

  # --- Determine point colors ---
  use_groups <- color_by != "none" && color_by %in% colnames(pts_view)

  if (use_groups && !is.null(custom_colors)) {
    grp_vals <- as.character(pts_view[[color_by]])
    groups <- sort(unique(grp_vals))
    # Ensure all groups have a color (fallback for unexpected values)
    missing <- setdiff(groups, names(custom_colors))
    if (length(missing) > 0) {
      extra <- generate_group_palette(missing, "Set1")
      custom_colors <- c(custom_colors, extra)
    }
    pal_fn <- leaflet::colorFactor(
      palette = unname(custom_colors[groups]),
      domain = groups
    )
    fill_colors <- pal_fn(grp_vals)
    border_color <- "white"
    fill_opacity <- 0.85

    # Add legend
    map <- map %>% leaflet::addLegend(
      position = "bottomleft",
      colors = unname(custom_colors[groups]),
      labels = groups,
      title = color_by,
      opacity = 0.9
    )
  } else {
    fill_colors <- "cyan"
    border_color <- "cyan"
    fill_opacity <- 0.5
  }

  # --- Popups ---
  popups <- NULL
  if (!is.null(popup_fn)) {
    df_clean <- sf::st_drop_geometry(pts_view)
    popups <- vapply(seq_len(nrow(df_clean)), function(i) popup_fn(df_clean[i, ]), character(1))
  }

  # --- Add circle markers ---
  map <- map %>% leaflet::addCircleMarkers(
    data = pts_view,
    radius = marker_size,
    color = border_color,
    weight = 1,
    fillColor = fill_colors,
    fillOpacity = fill_opacity,
    opacity = 1,
    popup = popups,
    group = "styled_points"
  )

  # --- Labels (separate layer for clarity) ---
  if (show_labels && label_field != "none" && label_field %in% colnames(pts_view)) {
    raw_vals <- pts_view[[label_field]]
    label_vals <- if (is.numeric(raw_vals)) {
      ifelse(is.na(raw_vals), NA_character_, sprintf("%.2f", raw_vals))
    } else {
      as.character(raw_vals)
    }
    map <- map %>% leaflet::addLabelOnlyMarkers(
      data = pts_view,
      label = label_vals,
      labelOptions = leaflet::labelOptions(
        noHide = TRUE, direction = "top", textOnly = TRUE,
        offset = c(0, -8),
        style = list(
          "font-size" = paste0(label_size, "px"),
          "font-weight" = "bold",
          "color" = "white",
          "text-shadow" = "1px 1px 2px rgba(0,0,0,0.9), -1px -1px 2px rgba(0,0,0,0.9), 1px -1px 2px rgba(0,0,0,0.9), -1px 1px 2px rgba(0,0,0,0.9)"
        )
      ),
      group = "styled_labels"
    )
  }

  map
}
