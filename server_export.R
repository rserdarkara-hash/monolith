# server_export.R (sourced with local = TRUE inside server) - export registry,
# run-config/run-history panels, WYSIWYG styler and export download handlers.
  register_export_item <- function(id, label, type, obj, category = "General", kind = "value") {
    req(obj)
    clean_id <- gsub("[^a-zA-Z0-9_]", "_", id)

    new_item <- list(
      id = clean_id,
      label = label,
      type = type, # "plot", "table", "map"
      obj = obj,
      category = category,
      # For maps: "value" (concentration surface, may be agro/bin classified),
      # "residual" (diverging scale, never classified), "uncertainty"
      # (sequential continuous, never classified). Agronomic class limits are
      # defined on the variable's units, so classifying errors or variances
      # with them is scientifically meaningless.
      kind = kind,
      timestamp = Sys.time()
    )
    
    current_reg <- isolate(rv$export_registry)
    current_reg[[clean_id]] <- new_item
    rv$export_registry <- current_reg
    
    rv$log <- paste0(rv$log, "\n[Registry] Registered ", type, ": ", label)
  }
  
  output$export_registry_ui <- renderUI({
    req(rv$export_registry)
    reg <- rv$export_registry
    if (length(reg) == 0) return(tags$p("Registry is empty. Run models to populate."))
    
    choices <- setNames(names(reg), sapply(reg, function(x) {
      sprintf("%s [%s] - %s", x$label, x$type, format(x$timestamp, "%H:%M:%S"))
    }))
    
    checkboxGroupInput("selected_assets", NULL, choices = choices, width = "100%")
  })

  output$run_config_display <- renderUI({
    cfg <- rv$run_config_summary
    if (is.null(cfg)) return(NULL)
    div(style = "background-color: #e8f4fd; padding: 12px 15px; border-left: 4px solid #2196F3; border-radius: 4px; margin-bottom: 10px; font-family: monospace; font-size: 0.85em;",
      tags$strong(icon("info-circle"), paste0(" Run #", cfg$run_id, " Configuration (", format(cfg$timestamp, "%Y-%m-%d %H:%M:%S"), ")")),
      tags$br(),
      tags$span(paste0("Variable: ", cfg$variable, " | Method: ", cfg$method, " | Localities: ", cfg$localities)),
      tags$br(),
      tags$span(paste0("Subset: ", cfg$subset, " | View: ", cfg$value_type, " | CRS: ", cfg$crs)),
      tags$br(),
      tags$span(paste0("Boundary: ", cfg$boundary_type, " | Buffer: ", if (is.null(cfg$buffer_mode) || cfg$buffer_mode == "fixed") paste0(cfg$buffer_dist, "m") else "Dynamic", " | Resolution: ", cfg$resolution, " (", cfg$res_mode, ")")),
      if (!is.null(cfg$method_params) && nzchar(cfg$method_params)) tagList(tags$br(), tags$span(cfg$method_params))
    )
  })

  output$run_config_display_map <- renderUI({
    cfg <- rv$run_config_summary
    if (is.null(cfg)) return(NULL)
    div(style = "background-color: #e8f4fd; padding: 8px 12px; border-left: 4px solid #2196F3; border-radius: 4px; margin-bottom: 8px; font-size: 0.82em;",
      tags$strong(paste0("Run #", cfg$run_id, ": ")),
      tags$span(paste0(cfg$variable, " | ", cfg$method, " | ", cfg$localities, " | ", format(cfg$timestamp, "%H:%M:%S")))
    )
  })

  output$run_history_ui <- renderUI({
    hist <- rv$run_history
    if (length(hist) == 0) return(tags$p(style="color: #888;", "No archived runs yet. Previous runs will appear here when a new model generation begins."))

    run_panels <- lapply(seq_along(hist), function(i) {
      run <- hist[[i]]
      cfg <- run$config
      n_items <- length(run$registry)
      div(style = "background-color: #fff; padding: 12px; border: 1px solid #ddd; border-radius: 6px; margin-bottom: 8px;",
        fluidRow(
          column(8,
            tags$strong(paste0("Run #", cfg$run_id, " - ", cfg$variable, " (", cfg$method, ")")),
            tags$br(),
            tags$small(style="color: #666;", paste0(
              format(cfg$timestamp, "%Y-%m-%d %H:%M:%S"),
              " | ", cfg$localities,
              " | ", n_items, " registry items"
            ))
          ),
          column(4, style = "text-align: right;",
            actionButton(paste0("restore_run_", cfg$run_id), "Restore", class = "btn-sm btn-info", icon = icon("undo")),
            actionButton(paste0("delete_run_", cfg$run_id), "Remove", class = "btn-sm btn-danger", icon = icon("trash"))
          )
        )
      )
    })
    tagList(run_panels)
  })

  env_hist <- new.env(parent = emptyenv())
  env_hist$history_obs <- list()
  observeEvent(rv$run_history, {
    hist <- rv$run_history
    lapply(env_hist$history_obs, function(o) {
      if (!is.null(o)) o$destroy()
    })
    env_hist$history_obs <- list()
    
    lapply(seq_along(hist), function(i) {
      run <- hist[[i]]
      run_id <- run$config$run_id
      
      obs_restore <- observeEvent(input[[paste0("restore_run_", run_id)]], {
        history_list <- isolate(rv$run_history)
        idx <- which(sapply(history_list, function(x) x$config$run_id) == run_id)[1]
        if (is.na(idx)) return()
        
        run_to_restore <- history_list[[idx]]
        
        current_cfg <- isolate(rv$run_config_summary)
        current_reg <- isolate(rv$export_registry)
        if (!is.null(current_cfg) && length(current_reg) > 0) {
          push_run_history(list(config = current_cfg, registry = current_reg),
                           base = history_list[-idx])
        } else {
          rv$run_history <- history_list[-idx]
        }
        
        rv$export_registry <- run_to_restore$registry
        rv$run_config_summary <- run_to_restore$config
        
        showNotification(paste0("Restored Run #", run_to_restore$config$run_id, " to active session."), type = "message")
      }, ignoreInit = TRUE)

      obs_delete <- observeEvent(input[[paste0("delete_run_", run_id)]], {
        history_list <- isolate(rv$run_history)
        idx <- which(sapply(history_list, function(x) x$config$run_id) == run_id)[1]
        if (!is.na(idx)) {
          rv$run_history <- history_list[-idx]
          showNotification("Archived run removed.", type = "warning")
        }
      }, ignoreInit = TRUE)
      
      env_hist$history_obs[[paste0("restore_", run_id)]] <- obs_restore
      env_hist$history_obs[[paste0("delete_", run_id)]] <- obs_delete
    })
  }, ignoreNULL = FALSE)

  active_styler_item <- reactiveVal(NULL)
  
  observeEvent(input$selected_assets, {
    req(input$selected_assets)
    if (length(input$selected_assets) > 0) {
      active_styler_item(input$selected_assets[length(input$selected_assets)])
    }
  }, ignoreNULL = FALSE)
  
  observeEvent(active_styler_item(), {
    req(active_styler_item(), rv$export_registry)
    item <- rv$export_registry[[active_styler_item()]]
    req(item)
    if (item$type == "map_combined") {
      updateSelectInput(session, "styler_legend_pos", selected = "bottom")
      updateSelectInput(session, "styler_legend_dir", selected = "horizontal")
      updateSelectInput(session, "styler_legend_text_angle", selected = 90)
    } else {
      updateSelectInput(session, "styler_legend_pos", selected = "right")
      updateSelectInput(session, "styler_legend_dir", selected = "auto")
      updateSelectInput(session, "styler_legend_text_angle", selected = 0)
    }
  }, ignoreInit = TRUE)
  
  base_preview_plot <- reactive({
    req(active_styler_item(), rv$export_registry)
    item <- rv$export_registry[[active_styler_item()]]
    req(item)
    
    generate_base_plot(
      item = item,
      input = input,
      agro_params = tryCatch(agro_params(), error = function(e) NULL)
    )
  })
  
  styled_preview_obj <- reactive({
    req(active_styler_item(), rv$export_registry)
    item <- rv$export_registry[[active_styler_item()]]
    req(item)
    
    base_p <- base_preview_plot()
    req(base_p)
    
    apply_styler_theme(
      p = base_p,
      input = input,
      calibration = 1,
      item_label = item$label,
      item_type = item$type
    )
  })
  
  styled_preview_obj_d <- styled_preview_obj %>% debounce(500)
  
  output$styler_preview_dynamic_ui <- renderUI({
    req(input$styler_width, input$styler_height)
    w_px <- (if(isTruthy(input$styler_width)) input$styler_width else 10) * 96
    h_px <- (if(isTruthy(input$styler_height)) input$styler_height else 8) * 96
    
    scale <- min(1, 800 / w_px, 600 / h_px)
    w_disp <- w_px * scale
    h_disp <- h_px * scale
    
    div(style = sprintf("width: %fpx; height: %fpx; background-color: white; box-shadow: 0 4px 8px rgba(0,0,0,0.2);", w_disp, h_disp),
        plotOutput("styler_preview_plot", height = "100%", width = "100%")
    )
  })

  output$styler_preview_plot <- renderPlot({
    req(styled_preview_obj_d())
    styled_preview_obj_d()
  }, res = 96)
  
  observeEvent(input$select_all_assets, {
    req(rv$export_registry)
    updateCheckboxGroupInput(session, "selected_assets", selected = names(rv$export_registry))
  })
  
  observeEvent(input$deselect_all_assets, {
    updateCheckboxGroupInput(session, "selected_assets", selected = character(0))
  })
  
  observeEvent(input$clear_registry, {
    rv$export_registry <- list()
    showNotification("Export registry cleared.", type = "message")
  })
  
  observeEvent(input$quick_export_map, {
    # Export exactly what the viewer shows: the committed run's surface for
    # the currently selected view.
    meta <- get_display_meta(); req(meta)
    view <- input$map_view %||% "view_act"
    target <- switch(view,
      "view_pred"  = rv$rast_pred,
      "view_resid" = rv$rast_res,
      "view_comp"  = if (!is.null(rv$rast) && !is.null(rv$rast_pred)) list(act = rv$rast, pre = rv$rast_pred) else NULL,
      rv$rast)
    req(target)

    type <- if (view == "view_comp") "map_combined" else "map"
    kind <- if (view == "view_resid") "residual" else "value"
    view_lab <- switch(view,
      "view_pred" = "Predicted", "view_resid" = "Residuals",
      "view_comp" = "Actual vs Predicted", "Actual")

    id <- paste0("quick_", view, "_", meta$actual)
    label <- paste("Quick Export:", meta$label, "(", view_lab, ")")

    register_export_item(id, label, type, target, meta$category, kind = kind)
    active_styler_item(id)

    shinyjs::click("open_styler")
  })
  
  # Persist the styler config once per settled state (debounced), not on
  # every keystroke/slider tick in every styler field.
  styler_persist_cfg <- debounce(reactive({
    req(input$styler_title_size)
    lapply(styler_fields, function(field) {
      input[[field$name]]
    })
  }), 750)
  observeEvent(styler_persist_cfg(), {
    shinyjs::runjs(sprintf("localStorage.setItem('monolith_styler_config', JSON.stringify(%s));", jsonlite::toJSON(styler_persist_cfg(), auto_unbox = TRUE)))
  })

  observeEvent(input$open_styler, {
    # priority: 'event' forces the sync to re-fire on every open: Shiny
    # dedupes identical input payloads, and the modal re-creates its inputs
    # with default values each time, so a deduped (skipped) restore would let
    # the debounced persist observer overwrite the saved config with defaults.
    shinyjs::runjs("
      var cfg = localStorage.getItem('monolith_styler_config');
      if(cfg) {
        Shiny.setInputValue('styler_local_config', JSON.parse(cfg), {priority: 'event'});
      }
    ")
    
    showModal(modalDialog(
      title = "Monolith Export Styler",
      size = "l",
      easyClose = FALSE,
      fluidRow(
        column(4,
               tabsetPanel(
                 tabPanel("Basic",
                          wellPanel(
                            h4("1. Typography Overrides"),
                            textInput("styler_title", "Main Title", placeholder = "Auto-generated"),
                            fluidRow(
                              column(6, textInput("styler_x_title", "X-Axis Label", placeholder = "Default")),
                              column(6, textInput("styler_y_title", "Y-Axis Label", placeholder = "Default"))
                            ),
                            hr(),
                            h4("2. Output Quality"),
                            numericInput("styler_dpi", "Export DPI", value = 300, min = 72, max = 600),
                            selectInput("styler_format", "File Format",
                                        choices = c("PNG" = "png", "TIFF" = "tiff", "PDF" = "pdf", "JPEG" = "jpg")),
                            hr(),
                            h4("3. Residual / Error Maps"),
                            selectInput("styler_resid_palette", "Diverging Palette",
                                        choices = c("Red-Blue" = "RdBu", "Red-Yellow-Blue" = "RdYlBu",
                                                    "Purple-Orange" = "PuOr", "Brown-Teal" = "BrBG",
                                                    "Pink-Green" = "PiYG", "Purple-Green" = "PRGn",
                                                    "Spectral" = "Spectral", "Red-Yellow-Green" = "RdYlGn",
                                                    "Red-Grey" = "RdGy"),
                                        selected = "RdBu"),
                            tags$p(style = "font-size: 0.8em; color: #666; margin-top: -8px;",
                                   "Applies to residual and point error maps only. Zero is always centered.")
                          )
                 ),
                 tabPanel("Advanced",
                          wellPanel(
                            h4("Font Sizes (pt)"),
                            fluidRow(
                              column(6, sliderInput("styler_title_size", "Main Title", min = 6, max = 40, value = 15)),
                              column(6, sliderInput("styler_base_size", "Base Text", min = 4, max = 30, value = 13))
                            ),
                            fluidRow(
                              column(6, sliderInput("styler_x_size", "X-Axis Label Size", min = 4, max = 30, value = 13)),
                              column(6, sliderInput("styler_y_size", "Y-Axis Label Size", min = 4, max = 30, value = 13))
                            ),
                            fluidRow(
                              column(6, sliderInput("styler_label_size", "Axis Text", min = 4, max = 30, value = 15)),
                              column(6, sliderInput("styler_legend_size", "Legend Text", min = 4, max = 30, value = 15))
                            ),
                            fluidRow(
                              column(6, sliderInput("styler_legend_key_size", "Legend Element Size", min = 0.5, max = 5.0, value = 0.5, step = 0.1)),
                              column(6, selectInput("styler_font_family", "Font Family", 
                                          choices = c("sans", "serif", "mono", "Roboto", "Open Sans", "Lato", "Montserrat")))
                            ),
                            selectInput("styler_label_orient", "X-Label Orientation", 
                                        choices = c("Vertical" = 90, "Horizontal" = 0, "Angled (45)" = 45)),
                            hr(),
                            h4("Layout & Spacing"),
                            selectInput("styler_legend_pos", "Legend Position", 
                                        choices = c("Right" = "right", "Bottom" = "bottom", "Left" = "left", "Top" = "top", "None" = "none")),
                            selectInput("styler_legend_dir", "Legend Orientation", 
                                        choices = c("Automatic" = "auto", "Horizontal" = "horizontal", "Vertical" = "vertical")),
                            selectInput("styler_legend_text_angle", "Legend Text Orientation", 
                                        choices = c("Horizontal" = 0, "Vertical" = 90, "Angled (45)" = 45)),
                            fluidRow(
                              column(3, numericInput("styler_margin_t", "Top", value = 10)),
                              column(3, numericInput("styler_margin_r", "Right", value = 10)),
                              column(3, numericInput("styler_margin_b", "Bottom", value = 10)),
                              column(3, numericInput("styler_margin_l", "Left", value = 10))
                            ),
                            checkboxInput("styler_show_grid", "Show Coordinate Grid", TRUE),
                            hr(),
                            h4("Publication Modifiers"),
                            checkboxInput("styler_high_contrast", "High Contrast Palette (Colorblind Safe)", FALSE),
                            fluidRow(
                              column(6, numericInput("styler_width", "Export Width (in)", value = 10, min = 1, max = 50, step = 0.5)),
                              column(6, numericInput("styler_height", "Export Height (in)", value = 8, min = 1, max = 50, step = 0.5))
                            ),
                            numericInput("styler_aspect_ratio", "Custom Aspect Ratio (Width/Height)", value = 1.25, step = 0.1)
                          )
                 )
               )
        ),
        column(8,
               div(style = "background-color: #f0f0f0; border: 1px solid #ccc; height: 600px; display: flex; justify-content: center; align-items: center; overflow: auto;",
                   uiOutput("styler_preview_dynamic_ui")
               ),
               tags$p(style="font-size: 0.85em; color: #666; margin-top: 5px;",
                      "Preview aspect ratio and dimensions are now live. Final export uses 2.5x typographical density enhancement.")
        )
      ),
      footer = tagList(
        div(style = "float: left; display: flex; gap: 10px;",
            downloadButton("styler_download_config", "Save Config", class = "btn-secondary btn-sm"),
            fileInput("styler_upload_config", NULL, buttonLabel = "Load Config", accept = c(".json"), multiple = FALSE, placeholder = "No file")
        ),
        modalButton("Cancel"),
        downloadButton("confirm_export", "Finalize & Download", class = "btn-success", icon = icon("check"))
      )
    ))
  })
  
  observeEvent(input$styler_local_config, {
    cfg <- input$styler_local_config
    sync_styler_config(cfg, session)
  })

  observeEvent(input$styler_width, {
    req(input$styler_width, input$styler_height)
    new_ratio <- round(input$styler_width / input$styler_height, 2)
    if (abs(new_ratio - (input$styler_aspect_ratio %||% 0)) > 0.01) {
      updateNumericInput(session, "styler_aspect_ratio", value = new_ratio)
    }
  }, ignoreInit = TRUE)

  observeEvent(input$styler_height, {
    req(input$styler_width, input$styler_height)
    new_ratio <- round(input$styler_width / input$styler_height, 2)
    if (abs(new_ratio - (input$styler_aspect_ratio %||% 0)) > 0.01) {
      updateNumericInput(session, "styler_aspect_ratio", value = new_ratio)
    }
  }, ignoreInit = TRUE)

  observeEvent(input$styler_aspect_ratio, {
    req(input$styler_aspect_ratio, input$styler_width)
    new_height <- round(input$styler_width / input$styler_aspect_ratio, 2)
    if (abs(new_height - (input$styler_height %||% 0)) > 0.01) {
      updateNumericInput(session, "styler_height", value = new_height)
    }
  }, ignoreInit = TRUE)
  
  output$styler_download_config <- downloadHandler(
    filename = function() { paste0("styler_config_", format(Sys.time(), "%Y%m%d"), ".json") },
    content = function(file) {
      cfg <- lapply(styler_fields, function(field) {
        input[[field$name]]
      })
      names(cfg) <- sapply(styler_fields, function(f) f$name)
      write(jsonlite::toJSON(cfg, auto_unbox = TRUE), file)
    }
  )

  observeEvent(input$styler_upload_config, {
    req(input$styler_upload_config)
    tryCatch({
      cfg <- jsonlite::fromJSON(input$styler_upload_config$datapath)
      
      sync_styler_config(cfg, session)
      
      showNotification("Styler configuration loaded successfully.", type = "message")
    }, error = function(e) {
      showNotification("Failed to load configuration. Invalid JSON.", type = "error")
    })
  })
  
  output$confirm_export <- downloadHandler(
    filename = function() {
      req(active_styler_item())
      item <- rv$export_registry[[active_styler_item()]]
      timestamp <- format(Sys.time(), "%Y%m%d_%H%M%S")
      ext <- if(item$type %in% c("plot", "map", "map_combined")) (input$styler_format %||% "png") else "xlsx"
      sprintf("Export_%s_%s.%s", item$id, timestamp, ext)
    },
    content = function(file) {
      req(active_styler_item())
      item <- rv$export_registry[[active_styler_item()]]
      ext <- if(item$type %in% c("plot", "map", "map_combined")) (input$styler_format %||% "png") else "xlsx"
      
      withProgress(message = paste("Exporting", item$type, "..."), {
        tryCatch({
          if (item$type %in% c("plot", "map", "map_combined")) {
            p_obj <- generate_styled_plot(
              item, input, 
              calibration = 2.5, 
              agro_params = tryCatch(agro_params(), error = function(e) NULL)
            )

            export_plot_to_file(p_obj, file, ext, input)
          } else if (item$type == "table") {
            if (ext == "xlsx") {
              wb <- createWorkbook()
              addWorksheet(wb, "Data")
              writeData(wb, "Data", item$obj)
              saveWorkbook(wb, file, overwrite = TRUE)
            } else {
              write.csv(item$obj, file, row.names = FALSE)
            }
          }
          
          removeModal()
        }, error = function(e) {
          showNotification(paste("Export Failed:", e$message), type = "error")
        })
      })
    }
  )
  
  output$batch_export <- downloadHandler(
    filename = function() { paste0("Batch_Export_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".zip") },
    contentType = "application/zip",
    content = function(file) {
      req(input$selected_assets, length(input$selected_assets) > 0)
      
      temp_dir <- file.path(tempdir(), paste0("export_", as.integer(Sys.time())))
      dir.create(temp_dir, showWarnings = FALSE)
      
      withProgress(message = "Batch Exporting...", value = 0, {
        selected_ids <- input$selected_assets
        items <- rv$export_registry[selected_ids]
        
        table_items <- Filter(function(x) x$type == "table", items)
        plot_items <- Filter(function(x) x$type %in% c("plot", "map", "map_combined"), items)
        
        n_plots <- length(plot_items)
        has_tables <- length(table_items) > 0
        total_steps <- n_plots + (if(has_tables) 1 else 0)

        files_to_zip <- c()

        if(has_tables) {
          incProgress(1/total_steps, detail = "Compiling statistical tables into Excel...")
          
          timestamp <- format(Sys.time(), "%Y%m%d_%H%M%S")
          excel_name <- sprintf("Batch_Statistics_%s.xlsx", timestamp)
          excel_path <- file.path(temp_dir, excel_name)
          
          wb <- createWorkbook()
          used_sheet_names <- c()
          for(item in table_items) {
            clean_label <- gsub("[^a-zA-Z0-9 ]", "_", item$label)
            sheet_name <- substr(clean_label, 1, 31)
            
            if(sheet_name %in% used_sheet_names) {
              suffix <- if(grepl("_pre_|_pre$", item$id)) "_Pre" else "_2"
              counter <- 2
              candidate <- paste0(substr(sheet_name, 1, 31 - nchar(suffix)), suffix)
              while(candidate %in% used_sheet_names) {
                counter <- counter + 1
                suffix <- paste0("_", counter)
                candidate <- paste0(substr(sheet_name, 1, 31 - nchar(suffix)), suffix)
              }
              sheet_name <- candidate
            }
            used_sheet_names <- c(used_sheet_names, sheet_name)
            addWorksheet(wb, sheet_name)
            writeData(wb, sheet_name, item$obj)
          }
          saveWorkbook(wb, excel_path, overwrite = TRUE)
          files_to_zip <- c(files_to_zip, excel_name)
        }
        
        for (i in seq_along(plot_items)) {
          item <- plot_items[[i]]
          incProgress(1/total_steps, detail = paste("Exporting Plot:", item$label))
          
          timestamp <- format(Sys.time(), "%Y%m%d_%H%M%S")
          ext <- if(item$type %in% c("plot", "map", "map_combined")) (input$styler_format %||% "png") else "png"
          if(ext == "csv") ext <- "png" # Extra safety
          
          filename <- sprintf("Batch_%s_%s.%s", item$id, timestamp, ext)
          filepath <- file.path(temp_dir, filename)
          
          tryCatch({
            p <- generate_styled_plot(
              item, input, 
              calibration = 2.5, 
              agro_params = tryCatch(agro_params(), error = function(e) NULL)
            )
            
            export_plot_to_file(p, filepath, ext, input)
            files_to_zip <- c(files_to_zip, filename)
          }, error = function(e) {
            rv$log <- paste0(rv$log, "\n[Batch] Failed to export ", item$label, ": ", e$message)
          })
        }
      })
      
      if (length(files_to_zip) == 0) {
        unlink(temp_dir, recursive = TRUE)
        showNotification("Batch export failed: none of the selected assets could be exported. Check the Log tab for details.", type = "error", duration = 10)
        stop("No assets could be exported.")
      }

      zip_path <- file.path(temp_dir, "export.zip")
      zip::zip(zipfile = zip_path, files = files_to_zip, root = temp_dir)

      file.copy(zip_path, file)

      unlink(temp_dir, recursive = TRUE)
    }
  )
  
  
