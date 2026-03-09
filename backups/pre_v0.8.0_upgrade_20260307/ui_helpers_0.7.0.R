library(shiny)

tuning_ui <- function(id, label, 
                      global_slider_id, manual_slider_id, 
                      global_slider_args, manual_slider_args, 
                      optimize_btn_label = paste("OPTIMIZE", label),
                      manual_btn_label = paste("Apply Manual", label),
                      outer_style = NULL,
                      manual_style = "background-color: #fff9db; padding: 10px; border: 1px solid #fab005; border-radius: 4px; margin-bottom: 10px;",
                      extra_ui = NULL) {
  
  content <- tagList(
    radioButtons(paste0(id, "_mode"), "Fitting Mode", 
                 choices = c("Auto-Fit" = "auto", "Manual" = "manual"), inline = TRUE),
    
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
generate_styled_plot <- function(item, input, calibration = 1, agro_params = NULL) {
  req(item)
  library(ggplot2)
  library(patchwork)
  library(tidyterra)
  
  # 1. Scaling Logic
  s_title <- (input$styler_title_size %||% 16) * calibration
  s_base  <- (input$styler_base_size %||% 12) * calibration
  s_x     <- (input$styler_x_size %||% 12) * calibration
  s_y     <- (input$styler_y_size %||% 12) * calibration
  s_lab   <- (input$styler_label_size %||% 10) * calibration
  s_leg   <- (input$styler_legend_size %||% 10) * calibration
  
  font_f <- input$styler_font_family %||% "sans"
  
  # Helper to style a single pane
  style_pane <- function(p, label) {
    f_title <- label
    f_x <- if(isTruthy(input$styler_x_title)) input$styler_x_title else NULL
    f_y <- if(isTruthy(input$styler_y_title)) input$styler_y_title else NULL
    
    p + theme_minimal(base_size = s_base, base_family = font_f) +
      theme(
        plot.title = element_text(size = s_title, face = "bold"),
        plot.subtitle = element_text(size = s_title * 0.8),
        axis.title.x = element_text(size = s_x),
        axis.title.y = element_text(size = s_y),
        axis.text = element_text(size = s_lab),
        legend.text = element_text(size = s_leg),
        legend.title = element_text(size = s_leg, face = "bold"),
        legend.key.size = unit(input$styler_legend_key_size %||% 1.0, "cm"),
        legend.position = input$styler_legend_pos %||% "right",
        plot.margin = ggplot2::margin(
          (input$styler_margin_t %||% 10) * calibration, 
          (input$styler_margin_r %||% 10) * calibration, 
          (input$styler_margin_b %||% 10) * calibration, 
          (input$styler_margin_l %||% 15) * calibration, 
          unit = "mm"),
        axis.text.x = element_text(
          angle = as.numeric(input$styler_label_orient %||% 0),
          hjust = if(as.numeric(input$styler_label_orient %||% 0) != 0) 1 else 0.5,
          vjust = if(as.numeric(input$styler_label_orient %||% 0) != 0) 1 else 0.5
        )
      ) +
      labs(title = f_title, x = f_x, y = f_y)
  }

  # 2. Build the specific plot type
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
        bp <- bp + scale_fill_distiller(palette = resid_pal, direction = 1, limits = c(-abs_max, abs_max), na.value = "transparent", name = leg_name)
      } else if (is_agro && !is.null(agro_params)) {
        obj_c <- terra::classify(obj[[1]], agro_params$rcl_mat, right = FALSE)
        df_p <- as.data.frame(obj_c, xy = TRUE)
        colnames(df_p) <- c("x", "y", "val")
        df_p$val <- factor(df_p$val, levels = 1:agro_params$n_c, labels = agro_params$leg_labels)
        
        a_cols <- agro_params$colors
        if (isTruthy(input$styler_high_contrast)) {
            a_cols <- viridis::viridis(agro_params$n_c)
        }
        
        bp <- ggplot(df_p, aes(x = x, y = y, fill = val)) + geom_tile() +
          scale_fill_manual(values = setNames(a_cols, agro_params$leg_labels), na.value = "transparent", name = leg_name, drop = FALSE) +
          coord_equal()
      } else {
        is_viridis <- pal_name %in% c("viridis", "magma", "inferno", "plasma", "cividis")
        if (input$color_style == "bin") {
          if(is_viridis) bp <- bp + scale_fill_viridis_b(option = pal_name, na.value = "transparent", n.breaks = 5, name = leg_name)
          else bp <- bp + scale_fill_fermenter(palette = pal_name, direction = 1, na.value = "transparent", n.breaks = 5, name = leg_name)
        } else {
          if(is_viridis) bp <- bp + scale_fill_viridis_c(option = pal_name, na.value = "transparent", name = leg_name)
          else bp <- bp + scale_fill_distiller(palette = pal_name, direction = 1, na.value = "transparent", name = leg_name)
        }
      }
      style_pane(bp, label)
    }

    if (item$type == "map") {
      t <- if(isTruthy(input$styler_title)) input$styler_title else item$label
      return(build_map(item$obj, t))
    } else {
      # map_combined - Optimized for tiling
      # We put the sub-titles on the plots, and main title as annotation
      p1 <- build_map(item$obj$act, "Actual", is_tiled = TRUE)
      p2 <- build_map(item$obj$pre, "Predicted", is_tiled = TRUE)
      
      main_t <- if(isTruthy(input$styler_title)) input$styler_title else item$label
      
      return(p1 + p2 + plot_layout(ncol = 2, guides = "collect") & 
             theme(legend.position = "bottom") & 
             plot_annotation(title = main_t, theme = theme(plot.title = element_text(size = s_title, face = "bold", family = font_f))))
    }
    
  } else if (inherits(item$obj, "trellis")) {
    # Lattice (Variograms)
    f_title <- if(isTruthy(input$styler_title)) input$styler_title else item$label
    f_x <- if(isTruthy(input$styler_x_title)) input$styler_x_title else "Distance"
    f_y <- if(isTruthy(input$styler_y_title)) input$styler_y_title else "Semivariance"
    
    return(update(item$obj, 
           par.settings = list(
             fontsize = list(text = s_base),
             fontfamily = font_f
           ),
           scales = list(x = list(rot = as.numeric(input$styler_label_orient %||% 0))),
           main = list(label = f_title, fontfamily = font_f, cex = s_title/s_base),
           xlab = list(label = f_x, fontfamily = font_f, cex = s_x/s_base),
           ylab = list(label = f_y, fontfamily = font_f, cex = s_y/s_base)))
           
  } else {
    # Standard ggplot
    return(style_pane(item$obj, item$label))
  }
}

# --- Documentation UI Components ---
render_docs_drawer <- function() {
  div(
    id = "docs_drawer",
    class = "docs-drawer",
    style = "position: fixed; right: -600px; top: 0; width: 600px; height: 100%; background: white; z-index: 1050; transition: right 0.3s ease; box-shadow: -2px 0 5px rgba(0,0,0,0.2); overflow-y: auto; padding: 20px;",
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
    `data-toggle` = "popover",
    `data-placement` = "auto",
    `data-trigger` = "focus",
    `data-content` = content_html,
    `data-html` = "true",
    onclick = "event.stopPropagation(); event.preventDefault();",
    icon("info-circle")
  )
}

match_metadata_columns <- function(m_df, user_cols) {
  cols <- colnames(m_df)
  col_act <- if (length(grep("actual|column|variable", cols, ignore.case=TRUE)) > 0) grep("actual|column|variable", cols, ignore.case=TRUE, value=TRUE)[1] else 1
  col_lab <- if (length(grep("label|name|display|ID", cols, ignore.case=TRUE)) > 0) grep("label|name|display|ID", cols, ignore.case=TRUE, value=TRUE)[1] else NA
  col_cat <- if (length(grep("cat|group|type", cols, ignore.case=TRUE)) > 0) grep("cat|group|type", cols, ignore.case=TRUE, value=TRUE)[1] else NA

  new_vars <- list()
  for (i in 1:nrow(m_df)) {
    act_name <- as.character(m_df[i, col_act])
    
    matched_col <- NULL
    if (act_name %in% user_cols) {
      matched_col <- act_name
    } else if (tolower(act_name) %in% tolower(user_cols)) {
      matched_col <- user_cols[tolower(user_cols) == tolower(act_name)][1]
    } else {
      clean_act <- tolower(gsub("[^a-zA-Z0-9]", "", act_name))
      clean_user <- tolower(gsub("[^a-zA-Z0-9]", "", user_cols))
      if (clean_act %in% clean_user) {
        matched_col <- user_cols[clean_user == clean_act][1]
      } else {
         first_word <- tolower(strsplit(act_name, " ")[[1]][1])
         if (!is.na(first_word) && first_word %in% clean_user) {
           matched_col <- user_cols[clean_user == first_word][1]
         }
      }
    }

    if (!is.null(matched_col)) {
      cat_val <- if (!is.na(col_cat)) as.character(m_df[i, col_cat]) else "Uploaded Data"
      lab_val <- if (!is.na(col_lab)) as.character(m_df[i, col_lab]) else act_name

      p_cve <- grep(paste0("^", matched_col, "_cve$"), user_cols, ignore.case=TRUE, value=TRUE)
      p_cve <- if(length(p_cve) > 0) p_cve[1] else NA
      p_ss  <- grep(paste0("^", matched_col, "_ss$"), user_cols, ignore.case=TRUE, value=TRUE)
      p_ss  <- if(length(p_ss) > 0) p_ss[1] else NA

      new_var <- list(
        actual = matched_col,
        pred = p_cve,
        pred_ss = p_ss,
        label = lab_val,
        category = cat_val,
        palette = "YlOrBr" 
      )
      
      if (exists("get_default_palette", mode="function")) {
          new_var$palette <- get_default_palette(matched_col, cat_val, matched_col)
      }
      
      new_vars[[length(new_vars) + 1]] <- new_var
    }
  }
  return(new_vars)
}
