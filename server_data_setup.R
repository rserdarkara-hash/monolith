# server_data_setup.R (sourced with local = TRUE inside server) - data/shp/
# metadata upload, CRS parsing + plausibility guards, variable mapping and the
# Setup-tab minimap.
  output$export_updated_data <- downloadHandler(
    filename = function() {
      paste0("updated_spatial_dataset_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".xlsx")
    },
    content = function(file) {
      req(rv$user_data)
      openxlsx::write.xlsx(rv$user_data, file = file)
    }
  )
  
  observeEvent(input$user_file, {
    req(input$user_file)
    ext <- tools::file_ext(input$user_file$name)
    
    if (!(tolower(ext) %in% c("csv", "xls", "xlsx"))) {
      showNotification("Invalid file type. Only CSV, XLS, and XLSX are supported.", type = "error")
      return()
    }
    
    fsize <- file.info(input$user_file$datapath)$size
    if (!is.null(fsize) && fsize > 30 * 1024 * 1024) {
      showNotification("File size exceeds 30MB limit.", type = "error")
      return()
    }
    
    df <- tryCatch({
      if (tolower(ext) == "csv") as.data.frame(data.table::fread(input$user_file$datapath))
      else if (tolower(ext) %in% c("xls", "xlsx")) readxl::read_excel(input$user_file$datapath)
      else NULL
    }, error = function(e) { 
      showNotification(paste("Error reading file:", e$message), type = "error")
      NULL
    })
    
    req(df); rv$user_data <- df
    
    cols <- colnames(df)
    updateSelectInput(session, "map_x", choices = cols, selected = grep("\\bx\\b|^lon|^longitude", cols, ignore.case=TRUE, value=TRUE)[1])
    updateSelectInput(session, "map_y", choices = cols, selected = grep("\\by\\b|^lat|^latitude", cols, ignore.case=TRUE, value=TRUE)[1])
    loc_guess <- grep("loc|site|farm|id|group", cols, ignore.case=TRUE, value=TRUE)[1]
    if (is.na(loc_guess)) loc_guess <- cols[1]
    updateSelectInput(session, "map_loc", choices = cols, selected = loc_guess)
    
    new_vars <- list()
    num_cols <- cols[sapply(df, is.numeric)]
    for (col in num_cols) {
      if (!is_coord_col(col)) {
        p_cve <- detect_pred_column(col, num_cols, "cve")
        p_ss  <- detect_pred_column(col, num_cols, "ss")
        new_vars[[length(new_vars) + 1]] <- list(
          actual = col, pred = p_cve, pred_ss = p_ss, label = col, category = "Uploaded Data",
          palette = get_default_palette(col, "Uploaded Data", col)
        )
      }
    }
    rv$mapping$vars <- new_vars
    
    rv$pop_up_vars <- num_cols[!is_coord_col(num_cols)]
    
    curr_locs <- isolate(input$locality)
    # input$map_loc is still the pre-updateSelectInput value here, so use
    # the freshly guessed column instead of the stale input.
    new_choices <- c("ALL", unique(df[[loc_guess]]))
    selected_locs <- intersect(curr_locs, new_choices)
    updateSelectInput(session, "locality", choices = new_choices, selected = selected_locs)

    subset_col <- find_subset_column(cols)
    subset_choices <- if (!is.na(subset_col)) {
      vals <- sort(unique(na.omit(as.character(df[[subset_col]]))))
      c("All" = "all", setNames(vals, vals))
    } else c("All" = "all")
    updateSelectInput(session, "subset", choices = subset_choices, selected = "all")

    shinyjs::runjs("setTimeout(function() { $('html, body').animate({ scrollTop: $('#map_x').offset().top - 20 }, 1000); }, 500);")
  })

  observeEvent(input$user_shp, {
    req(input$user_shp)
    temp_dir <- file.path(tempdir(), paste0("shp_upload_", as.integer(Sys.time())))
    dir.create(temp_dir, showWarnings = FALSE, recursive = TRUE)
    session$onSessionEnded(function() { unlink(temp_dir, recursive = TRUE) })
    
    for(i in seq_len(nrow(input$user_shp))) {
      file.copy(input$user_shp$datapath[i], file.path(temp_dir, input$user_shp$name[i]), overwrite = TRUE)
    }
    shp_file <- input$user_shp$name[grep("\\.shp$", input$user_shp$name, ignore.case = TRUE)]
    if(length(shp_file) == 0) { showNotification("No .shp file found.", type = "error"); return() }
    s <- tryCatch({ st_read(file.path(temp_dir, shp_file[1]), quiet = TRUE) }, error = function(e) { 
      showNotification(paste("Error reading shapefile:", e$message), type = "error"); NULL 
    })
    req(s)
    if (nrow(s) == 0) {
      showNotification("Uploaded shapefile contains zero features.", type = "error")
      return()
    }
    rv$shp_bound <- s
    
    geom_types <- unique(sf::st_geometry_type(s))
    if (!any(geom_types %in% c("POLYGON", "MULTIPOLYGON"))) {
      showNotification("Uploaded shapefile contains point/line geometry. Monolith will automatically generate boundary polygons (convex hulls) around these points/lines for interpolation.", type = "warning", duration = 12)
    } else {
      showNotification("Custom shapefile loaded successfully!", type = "message")
    }
    
    crs_obj <- sf::st_crs(s)
    crs_val <- NULL
    if (!is.null(crs_obj$epsg) && !is.na(crs_obj$epsg)) {
      crs_val <- paste0("EPSG:", crs_obj$epsg)
    } else if (!is.null(crs_obj$proj4string) && !is.na(crs_obj$proj4string) && crs_obj$proj4string != "") {
      crs_val <- crs_obj$proj4string
    } else if (!is.null(crs_obj$wkt) && !is.na(crs_obj$wkt) && crs_obj$wkt != "") {
      crs_val <- crs_obj$wkt
    }
    if(!is.null(crs_val)) {
      updateSelectizeInput(session, "crs_selection", selected = crs_val)
    } else {
      showNotification("The uploaded shapefile carries no CRS definition (.prj missing?). Its coordinates will be assumed to match the analysis CRS - if the boundary lands in the wrong place, re-export the shapefile with a .prj file.",
                       type = "warning", duration = 15)
    }
  })

  observeEvent(list(rv$user_data, input$map_x, input$map_y), {
    req(rv$user_data, input$map_x, input$map_y)
    if (!(input$map_x %in% colnames(rv$user_data) && input$map_y %in% colnames(rv$user_data))) return()
    
    df <- rv$user_data %>% select(x = !!sym(input$map_x), y = !!sym(input$map_y)) %>% na.omit()
    if (nrow(df) == 0) return()
    
    x_range <- range(df$x)
    y_range <- range(df$y)

    if (all(x_range >= -180 & x_range <= 180) && all(y_range >= -90 & y_range <= 90)) {
       suggested_crs <- "EPSG:4326"
    } else {
       # Projected-looking magnitudes: the zone / national grid CANNOT be
       # inferred from bare coordinates, so never auto-fill a specific EPSG
       # (the old EPSG:32635/5514 guesses were wrong anywhere outside those
       # zones). Ask the user instead; the CRS-plausibility guard below flags
       # unit mismatches if the selection stays wrong.
       showNotification(
         "Coordinates look projected (metre magnitudes). Please set the correct input CRS (e.g. your UTM zone or national grid) - it cannot be inferred from the coordinates alone.",
         type = "warning", duration = 10)
       return()
    }

    updateSelectizeInput(session, "map_crs", selected = suggested_crs)
    updateSelectizeInput(session, "crs_selection", selected = suggested_crs)
  })

  # Plausibility guard for the free-typed Input Data CRS: an unrecognized CRS,
  # or one whose unit family (degrees vs metres) cannot match the mapped X/Y
  # columns, is flagged immediately - before it silently corrupts every
  # downstream projection. Warn-only: it never blocks a run and never alters
  # any computed value.
  observeEvent(list(rv$user_data, input$map_x, input$map_y, input$map_crs), {
    req(rv$user_data, input$map_x, input$map_y, input$map_crs)
    if (!(input$map_x %in% colnames(rv$user_data) && input$map_y %in% colnames(rv$user_data))) return()
    df <- rv$user_data %>% select(x = !!sym(input$map_x), y = !!sym(input$map_y)) %>% na.omit()
    if (nrow(df) == 0) return()

    crs_obj <- tryCatch(sf::st_crs(input$map_crs), error = function(e) NULL)
    if (is.null(crs_obj) || is.na(crs_obj)) {
      showNotification(paste0("Input Data CRS '", input$map_crs, "' is not recognized. Enter a valid EPSG code (e.g. EPSG:32635), PROJ string, or WKT."),
                       type = "error", duration = 15, id = "crs_guard")
      return()
    }
    looks_geographic <- all(abs(df$x) <= 180) && all(abs(df$y) <= 90)
    is_longlat <- isTRUE(sf::st_is_longlat(crs_obj))
    if (is_longlat && !looks_geographic) {
      showNotification("The selected Input Data CRS is geographic (degrees), but the mapped X/Y columns fall outside +/-180 / +/-90. These coordinates are almost certainly projected: select the projected CRS they were recorded in, otherwise all conversions and distances will be wrong.",
                       type = "error", duration = 15, id = "crs_guard")
      return()
    }
    if (!is_longlat && looks_geographic) {
      showNotification("The selected Input Data CRS is projected (metric), but the mapped X/Y columns look like longitude/latitude degrees. If the data are lon/lat, choose WGS 84 (EPSG:4326) so they are converted correctly.",
                       type = "warning", duration = 12, id = "crs_guard")
      return()
    }
    removeNotification("crs_guard")
  })

  observeEvent(input$crs_selection, {
    req(input$crs_selection)

    # In fixed mode the user chose the value deliberately, so only refresh the
    # slider frame; in auto modes the suggestion observer overwrites the value
    # right after anyway.
    if (isTRUE(input$res_mode == "fixed")) {
      updateSliderInput(session, "grid_res", label = "Resolution (m)",
                        min = 1, max = 500, step = 1)
    } else {
      updateSliderInput(session, "grid_res", label = "Resolution (m)",
                        min = 1, max = 500, value = 50, step = 1)
    }
  })
  observeEvent(list(rv$user_data, input$map_x, input$map_y, input$crs_selection, input$locality, input$res_mode), {
    req(rv$user_data, input$map_x, input$map_y, input$crs_selection, input$locality, input$res_mode)
    if (!(input$map_x %in% colnames(rv$user_data) && input$map_y %in% colnames(rv$user_data))) return()
    
    df_raw <- rv$user_data %>% select(x = !!sym(input$map_x), y = !!sym(input$map_y), loc = !!sym(input$map_loc)) %>% na.omit()

    locs_scope <- resolve_selected_localities(input$locality, df_raw, "loc")

    if (input$res_mode == "fixed") {
      # No suggestion to compute, but the per-locality list still has to track
      # the current selection so the sidebar table and map overlay stay in sync.
      fixed_val <- input$grid_res %||% 50
      temp_res <- list()
      for (l in locs_scope) temp_res[[l]] <- fixed_val
      rv$loc_resolutions <- temp_res
      return()
    }

    df <- if (input$res_mode == "global") df_raw else df_raw %>% filter(loc %in% locs_scope)
    
    if (nrow(df) < 2) return()
    
    crs_obj <- validate_crs(input$crs_selection, "Invalid CRS provided:")
    req(crs_obj)

    pts <- tryCatch({
      st_as_sf(df, coords = c("x", "y"), crs = input$map_crs) %>% st_transform(input$crs_selection)
    }, error = function(e) NULL)
    req(pts)

    # The grid_res slider is metric and interpolation always runs in a
    # projected CRS, so the suggestion is measured in metres even when the
    # analysis CRS is geographic (calc_metric_spacing handles the conversion).
    spacing <- calc_metric_spacing(pts)
    req(is.finite(spacing$mean_nn))

    rec_res <- spacing$mean_nn * 0.5
    min_res_by_dim <- spacing$max_dim / 300

    final_rec <- max(rec_res, min_res_by_dim)
    final_rec <- max(0.1, min(500, round(final_rec, 1)))

    updateSliderInput(session, "grid_res", value = final_rec)
    
    if (input$res_mode == "local") {
        locs_to_calc <- locs_scope
        temp_res <- list()
        for (l in locs_to_calc) {
            sub_df <- df_raw %>% filter(loc == l)
            if (nrow(sub_df) < 2) next
            
            sub_pts <- tryCatch(st_as_sf(sub_df, coords=c("x","y"), crs=input$map_crs) %>% st_transform(input$crs_selection), error=function(e) { showNotification(paste("Projection failed for subset:", e$message), type = "error"); NULL })
            if(is.null(sub_pts)) next
            
            if (nrow(sub_pts) > 1) {
                 l_res <- calc_metric_spacing(sub_pts)$mean_nn * 0.5
            } else l_res <- final_rec

            l_res <- max(1, min(5000, l_res))

            temp_res[[l]] <- l_res        }
        rv$loc_resolutions <- temp_res
    } else {
        temp_res <- list()
        for (l in locs_scope) temp_res[[l]] <- final_rec
        rv$loc_resolutions <- temp_res
    }
  })

  # In fixed mode the stored per-locality values mirror the slider, so moving
  # it must refresh them or the map resolution overlay shows the old value.
  observeEvent(input$grid_res, {
    req(isTRUE(input$res_mode == "fixed"), length(rv$loc_resolutions) > 0)
    rv$loc_resolutions <- setNames(
      as.list(rep(input$grid_res, length(rv$loc_resolutions))),
      names(rv$loc_resolutions)
    )
  })

  observeEvent(input$meta_file, {
    req(input$meta_file, rv$user_data)
    ext <- tools::file_ext(input$meta_file$name)
    
    if (!(tolower(ext) %in% c("csv", "xls", "xlsx"))) {
      showNotification("Invalid metadata file type. Only CSV, XLS, and XLSX are supported.", type = "error")
      return()
    }
    
    fsize <- file.info(input$meta_file$datapath)$size
    if (!is.null(fsize) && fsize > 30 * 1024 * 1024) {
      showNotification("Metadata file size exceeds 30MB limit.", type = "error")
      return()
    }
    
    m_df <- tryCatch({
      if (tolower(ext) == "csv") read.csv(input$meta_file$datapath)
      else readxl::read_excel(input$meta_file$datapath)
    }, error = function(e) {
      showNotification(paste("Could not read the metadata file:", conditionMessage(e)), type = "error")
      NULL
    })

    req(m_df)
    user_cols <- colnames(rv$user_data)
    new_vars <- match_metadata_columns(m_df, user_cols)
    
    if (length(new_vars) > 0) {
      rv$mapping$vars <- new_vars
      showNotification(paste("Auto-mapped", length(new_vars), "variables with dual predictions."), type = "message")
    }
  })

  output$var_mapping_ui <- renderUI({
    req(rv$user_data)
    cols <- colnames(rv$user_data)
    num_cols <- cols[sapply(rv$user_data, is.numeric)]
    
    if (!is.null(rv$mapping$vars) && length(rv$mapping$vars) > 0) {
      targets <- sapply(rv$mapping$vars, function(x) x$actual)
    } else {
      targets <- num_cols[!is_coord_col(num_cols)]
      if (length(targets) > 30) {
        targets <- head(targets, 30)
        showNotification("Too many columns. Showing first 30 for mapping. Please use an Excel metadata file for bulk mapping.", type = "warning")
      }
    }
    
    get_map_val <- function(target, field) {
      match <- Filter(function(x) x$actual == target, rv$mapping$vars)
      if (length(match) > 0) {
         val <- match[[1]][[field]]
         if(is.null(val) || length(val) == 0) return(NULL)
         if(is.na(val)) return(NULL) else return(val)
      } else {
         return(NULL)
      }
    }

    tryCatch({
      tagList(
        lapply(seq_along(targets), function(i) {
          t <- targets[i]
          def_p_cve <- get_map_val(t, "pred")    %||% detect_pred_column(t, num_cols, "cve") %||% "None"
          def_p_ss  <- get_map_val(t, "pred_ss") %||% detect_pred_column(t, num_cols, "ss")  %||% "None"
          def_l     <- get_map_val(t, "label")    %||% t
          def_c     <- get_map_val(t, "category") %||% "Uploaded Data"
          
          if(is.na(def_p_cve)) def_p_cve <- "None"
          if(is.na(def_p_ss)) def_p_ss <- "None"
          if(is.na(def_l)) def_l <- t
          if(is.na(def_c)) def_c <- "Uploaded Data"
          
          div(style="border-bottom: 1px solid rgba(0,0,0,0.08); padding: 10px 0; margin-bottom: 10px;",
            fluidRow(
              column(2, tags$b(t)),
              column(3, selectInput(paste0("pair_pred_cve_", i), "Best Pred (_cve)", choices = c("None", num_cols), selected = def_p_cve)),
              column(3, selectInput(paste0("pair_pred_ss_", i),  "Split Pred (_ss)", choices = c("None", num_cols), selected = def_p_ss)),
              column(2, textInput(paste0("pair_label_", i), "Label", value = def_l)),
              column(2, textInput(paste0("pair_cat_", i), "Category", value = def_c))
            )
          )
        }),
        actionButton("confirm_mapping", "Confirm Variable Mapping", icon = icon("check-circle"), class = "btn-primary btn-block btn-pill")
      )
    }, error = function(e) {
      warning(paste("Error in var_mapping_ui:", e$message))
      h4(paste("Error rendering UI:", e$message), style="color:red;")
    })
  })

  observeEvent(input$confirm_mapping, {
    req(rv$user_data)
    cols <- colnames(rv$user_data)
    num_cols <- cols[sapply(rv$user_data, is.numeric)]
    
    if (!is.null(rv$mapping$vars) && length(rv$mapping$vars) > 0) {
      targets <- sapply(rv$mapping$vars, function(x) x$actual)
    } else {
      targets <- num_cols[!is_coord_col(num_cols)]
      if (length(targets) > 30) targets <- head(targets, 30)
    }
    
    new_vars <- list()
    for (i in seq_along(targets)) {
      p_cve <- input[[paste0("pair_pred_cve_", i)]]
      p_ss  <- input[[paste0("pair_pred_ss_", i)]]
      
      raw_cat <- input[[paste0("pair_cat_", i)]]
      cat_val <- if (is.null(raw_cat) || is.na(raw_cat) || raw_cat == "") "Uploaded Data" else raw_cat
      
      raw_lab <- input[[paste0("pair_label_", i)]]
      lab_val <- if (is.null(raw_lab) || is.na(raw_lab) || raw_lab == "") targets[i] else raw_lab
      
      new_vars[[length(new_vars) + 1]] <- list(
        actual = targets[i],
        pred = if (is.null(p_cve) || is.na(p_cve) || p_cve == "None") NULL else p_cve,
        pred_ss = if (is.null(p_ss) || is.na(p_ss) || p_ss == "None") NULL else p_ss,
        label = lab_val,
        category = cat_val,
        palette = get_default_palette(targets[i], cat_val, lab_val)
      )
    }
    rv$mapping$vars <- new_vars
    showNotification("Variable mapping saved!", type = "message")
  })

  # Rebuild the Context-panel locality selector, preserving the current
  # selection. Re-issued ONLY when the choice set actually changed: data
  # mutations that keep the same localities (drawn groups, discretized
  # columns) must not churn the selector mid-analysis.
  refresh_locality_choices <- function() {
    loc_col <- rv$mapping$loc
    if (is.null(rv$user_data) || is.null(loc_col) || !(loc_col %in% colnames(rv$user_data))) return(invisible(NULL))
    loc_choices <- unique(rv$user_data[[loc_col]])
    new_choices <- c("ALL", loc_choices)
    if (identical(session_state$locality_choices, new_choices)) return(invisible(NULL))
    session_state$locality_choices <- new_choices
    curr_locs <- isolate(input$locality)
    selected_locs <- intersect(curr_locs, new_choices)
    if (length(selected_locs) == 0) selected_locs <- loc_choices[1]
    updateSelectInput(session, "locality", choices = new_choices, selected = selected_locs)
  }

  observeEvent(list(input$map_x, input$map_y, input$map_loc, input$map_crs), {
    req(input$map_x, input$map_y, input$map_loc, input$map_crs)
    rv$mapping$x <- input$map_x
    rv$mapping$y <- input$map_y
    rv$mapping$loc <- input$map_loc
    rv$mapping$crs <- input$map_crs
    refresh_locality_choices()
  })

  # Data-driven refresh (new upload, added locality values); no-ops unless
  # the locality choice set changed.
  observeEvent(rv$user_data, {
    refresh_locality_choices()
  }, ignoreInit = TRUE)

  output$setup_minimap <- renderLeaflet({
    req(rv$user_data, rv$mapping$x, rv$mapping$y, rv$mapping$crs)

    color_by <- input$pt_color_by %||% "none"
    apply_mini <- isTRUE(input$pt_apply_minimap)
    loc_col <- rv$mapping$loc
    has_loc <- !is.null(loc_col) && loc_col %in% colnames(rv$user_data)

    needed <- c(rv$mapping$x, rv$mapping$y)
    if (apply_mini && color_by != "none" && color_by %in% colnames(rv$user_data)) needed <- c(needed, color_by)
    if (has_loc) needed <- c(needed, loc_col)
    needed <- unique(needed)

    df_map <- rv$user_data %>% dplyr::select(dplyr::all_of(needed)) %>% na.omit()
    if (nrow(df_map) == 0) return(NULL)


    pts <- tryCatch({
      st_as_sf(df_map, coords = c(rv$mapping$x, rv$mapping$y), crs = rv$mapping$crs) %>% st_transform(4326)
    }, error = function(e) NULL)
    req(pts)

    current_tiles <- input$base_map_layer %||% "Esri.WorldImagery"

    m <- leaflet(pts, options = leafletOptions(zoomControl = FALSE)) %>% addProviderTiles(current_tiles, layerId = "base_tiles")

    if (apply_mini && color_by != "none") {
      m <- add_styled_points(m, pts,
        color_by = color_by,
        custom_colors = rv$pt_style_colors,
        show_labels = FALSE,
        label_field = "none",
        label_size = 11,
        marker_size = input$pt_marker_size %||% 3
      )
    } else if (has_loc) {
      # Validation view: one colour per locality / geographic group so a
      # mis-mapped coordinate pair or CRS is immediately visible as points
      # landing in the wrong group.
      loc_vals <- as.character(pts[[loc_col]])
      groups <- sort(unique(loc_vals))
      pal <- generate_group_palette(groups, "Set1")
      m <- m %>% addCircleMarkers(radius = input$pt_marker_size %||% 3,
                                  color = "#222222", weight = 0.5,
                                  fillColor = unname(pal[loc_vals]), fillOpacity = 0.95,
                                  label = loc_vals) %>%
        leaflet::addLegend(colors = unname(pal), labels = names(pal),
                           title = "Locality", opacity = 0.9, position = "bottomright")
      # Permanent locality name over each group's centroid
      cen <- stats::aggregate(sf::st_coordinates(pts), by = list(loc = loc_vals), FUN = mean)
      m <- m %>% leaflet::addLabelOnlyMarkers(
        lng = cen$X, lat = cen$Y, label = cen$loc,
        labelOptions = leaflet::labelOptions(
          noHide = TRUE, direction = "top", textOnly = TRUE,
          style = list("font-weight" = "bold", "font-size" = "13px",
                       "color" = "#ffffff", "text-shadow" = "0 0 4px #000000, 0 0 2px #000000")
        )
      )
    } else {
      m <- m %>% addCircleMarkers(radius = input$pt_marker_size %||% 3, color = "cyan", opacity = 1)
    }
    session_state$minimap_rendered <- TRUE
    m
  })

