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
    # Whole-name matching (pick_coord_column, ui_formatting.R) on the same token
    # lists is_coord_col() uses: six other call sites already moved to that
    # policy, this one kept substring matching and pre-selected columns like
    # Longevity_index or Lateral_flow as X or Y. NULL when nothing matches
    # leaves the first column selected, exactly as the old NA did.
    updateSelectInput(session, "map_x", choices = cols, selected = pick_coord_column(cols, "x"))
    updateSelectInput(session, "map_y", choices = cols, selected = pick_coord_column(cols, "y"))
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
      set_target_crs(crs_val)
    } else {
      showNotification("The uploaded shapefile carries no CRS definition (.prj missing?). Its coordinates will be assumed to match the analysis CRS - if the boundary lands in the wrong place, re-export the shapefile with a .prj file.",
                       type = "warning", duration = 15)
    }
  })

  # selectize refuses a value that is not one of its options, so any CRS the
  # app sets programmatically (an identified EPSG code, a boundary's .prj, a
  # normalised free-typed entry) has to travel with its own choice entry.
  # Re-supplying `choices` re-creates the widget, which also means re-supplying
  # its options.
  # `record = FALSE` writes the value without claiming it as the app's own
  # choice: promoting a typed `32633` to `EPSG:32633` is the USER's CRS in a
  # parseable spelling, and recording it would let the identification observer
  # overwrite it on the next upload.
  set_crs_choice <- function(id, value, placeholder, base, record = TRUE) {
    ch <- if (value %in% base) base else c(base, setNames(value, value))
    if (isTRUE(record)) {
      session_state$crs_auto[[id]] <- unique(c(session_state$crs_auto[[id]], value))
    }
    updateSelectizeInput(session, id, choices = ch, selected = value,
                         options = list(create = TRUE, placeholder = placeholder))
  }
  # Did the USER put the current value there, or did the app?
  #
  # A plain `input$<id>` read cannot answer that. An updateSelectizeInput
  # message has not round-tripped when a second write lands in the same flush -
  # the shapefile observer writes rv$shp_bound and then the .prj's CRS, and the
  # identification observer that rv$shp_bound invalidates runs before either
  # reaches the browser - so the record is the set of every value the app has
  # written, not the last one.
  crs_user_chose <- function(id) {
    v <- input[[id]]
    isTruthy(v) && !(v %in% session_state$crs_auto[[id]])
  }
  crs_has_value <- function(id) {
    isTruthy(input[[id]]) || length(session_state$crs_auto[[id]]) > 0
  }
  # Two base lists: Web Mercator is offered for the input side only. A saved
  # config or a free-typed entry still travels with its own choice entry, so
  # restoring a run made under any CRS keeps working; the suitability gate,
  # not the dropdown, is what decides whether it may be run.
  set_input_crs  <- function(value, record = TRUE) set_crs_choice("map_crs", value, "Select the CRS your coordinates were recorded in", common_crs_input, record)
  set_target_crs <- function(value, record = TRUE) set_crs_choice("crs_selection", value, "Select the CRS for output maps and exports", common_crs_target, record)

  # A bare EPSG number typed into either selector (`32633`) is rejected by
  # sf::st_crs(); promote it to `EPSG:32633` in place so the free-text route
  # works the way users expect. No-ops for anything already parseable, so the
  # re-entry this update causes settles after one pass.
  observeEvent(input$map_crs, {
    req(input$map_crs)
    norm <- normalize_crs_input(input$map_crs)
    if (!identical(norm, input$map_crs)) set_input_crs(norm, record = FALSE)
  })
  observeEvent(input$crs_selection, {
    req(input$crs_selection)
    norm <- normalize_crs_input(input$crs_selection)
    if (!identical(norm, input$crs_selection)) set_target_crs(norm, record = FALSE)
  })

  # Input-CRS identification. Degrees are self-evident; projected coordinates
  # are identified ONLY from evidence carried by the upload (a companion
  # lon/lat pair, or a boundary .prj that the points fall inside). With no
  # evidence the app asks rather than guessing: a zone cannot be recovered
  # from bare eastings, and a guessed one places the whole survey in the wrong
  # country without raising a single error.
  observeEvent(list(rv$user_data, input$map_x, input$map_y, rv$shp_bound), {
    req(rv$user_data, input$map_x, input$map_y)
    if (!(input$map_x %in% colnames(rv$user_data) && input$map_y %in% colnames(rv$user_data))) return()
    # Nothing to identify from a column pair with no usable numbers; the
    # coordinate-validity modal at run time is what reports that.
    xs <- suppressWarnings(as.numeric(as.character(rv$user_data[[input$map_x]])))
    ys <- suppressWarnings(as.numeric(as.character(rv$user_data[[input$map_y]])))
    if (sum(is.finite(xs) & is.finite(ys)) < 1) return()

    ident <- identify_input_crs(rv$user_data, input$map_x, input$map_y, rv$shp_bound)
    if (is.null(ident)) {
      # Tier 3 takes over: the picker below asks where the data is and lists
      # the projections that put it there, instead of leaving the user to
      # supply an EPSG code they may not know.
      crs_pick$no_evidence <- TRUE
      crs_pick$rows <- NULL
      showNotification(
        "Coordinates look projected (metre magnitudes) and the file carries no evidence of which grid they are on - no lon/lat columns and no boundary shapefile. Use 'Locate your study area' below to narrow it down, or set the Input Data CRS yourself.",
        type = "warning", duration = 15, id = "crs_ident")
      return()
    }

    crs_pick$no_evidence <- FALSE
    # This observer re-fires on every upload and on every boundary shapefile,
    # so it must not revert a CRS the user set deliberately - picking a datum
    # sibling of the identified code (EPSG:25833 where the scoring reports
    # EPSG:32633) is a defensible choice, and it used to be silently undone.
    if (crs_user_chose("map_crs")) return()
    set_input_crs(ident$crs)
    # Never overwrite a Target Mapping CRS the user (or an uploaded .prj)
    # already chose; only fill it when it is still unset.
    if (!crs_has_value("crs_selection")) {
      set_target_crs(ident$crs)
    }
    showNotification(ident$message, type = "message", duration = 15, id = "crs_ident")
  })

  # ── Tier 3: locate the study area, then confirm a place ───────────────────
  # Monolith identifies your CRS when the file carries the evidence to prove
  # it, narrows it to a short list when it does not, and never assumes a zone.
  # This is the "narrows it" half. The zone is not recoverable from bare
  # eastings, so the question is turned around: the user clicks roughly where
  # the data was collected (or names a country) and the app reports which
  # projections read those coordinates as a position there. The user confirms
  # a PLACE, which they always know, instead of an EPSG code, which they often
  # do not.
  #
  # The search prunes the whole EPSG catalogue arithmetically before it
  # transforms anything (crs_candidate_shortlist(), global_utils.R), but the
  # surviving transforms still cost about a second, so it runs in a
  # future_promise: the tab stays live throughout, exactly as the optimizer
  # buttons and the run pipeline do. Everything the promise needs is read out
  # of the reactives BEFORE the call.
  crs_pick <- reactiveValues(no_evidence = FALSE, click = NULL, rows = NULL,
                             busy = FALSE, reopen = FALSE)

  crs_picker_visible <- reactive({
    isTRUE(crs_pick$no_evidence) && (!isTruthy(input$map_crs) || isTRUE(crs_pick$reopen))
  })

  output$crs_picker_ui <- renderUI({
    req(crs_picker_visible())
    div(style = "margin-top: 14px; border-top: 1px solid #e3e6ea; padding-top: 12px;",
        h5("Locate your study area", style = "font-weight: 600; margin-bottom: 4px;"),
        p(class = "setup-hint",
          "Bare eastings and northings do not carry their grid: the same pair is valid in every UTM zone, six degrees of longitude apart. Click roughly where the data was collected, or name the country, and Monolith will list the projections that put it there together with the position each one produces."),
        fluidRow(
          column(7, leafletOutput("crs_locator_map", height = "260px")),
          column(5,
                 textInput("crs_area_text", "Country or region (optional)",
                           placeholder = "e.g. Germany, Kansas"),
                 actionButton("crs_find_candidates", "Find matching projections",
                              icon = icon("search"), class = "btn-info btn-sm"),
                 uiOutput("crs_candidate_list"))
        ))
  })

  output$crs_locator_map <- renderLeaflet({
    # A plain street basemap, not the satellite layer the other maps default
    # to: this map is read for country outlines, not for ground detail.
    leaflet(options = leafletOptions(minZoom = 1, worldCopyJump = TRUE)) %>%
      addProviderTiles("CartoDB.Positron") %>%
      setView(lng = 10, lat = 25, zoom = 1)
  })

  observeEvent(input$crs_locator_map_click, {
    cl <- input$crs_locator_map_click
    req(cl$lng, cl$lat)
    # Panning past the date line returns longitudes outside +/-180; the extent
    # test in the registry is a plain comparison, so wrap first.
    lon <- ((cl$lng + 180) %% 360) - 180
    crs_pick$click <- list(lon = lon, lat = cl$lat)
    leafletProxy("crs_locator_map") %>%
      clearGroup("crs_click") %>%
      addMarkers(lng = lon, lat = cl$lat, group = "crs_click",
                 label = paste("Study area:", format_lonlat(lon, cl$lat)))
  })

  observeEvent(input$crs_find_candidates, {
    req(rv$user_data, input$map_x, input$map_y)
    if (isTRUE(crs_pick$busy)) return()
    if (!(input$map_x %in% colnames(rv$user_data) && input$map_y %in% colnames(rv$user_data))) return()
    click <- crs_pick$click
    txt <- trimws(input$crs_area_text %||% "")
    if (is.null(click) && !nzchar(txt)) {
      showNotification("Click the map where the data was collected, or type a country or region.",
                       type = "warning", duration = 8)
      return()
    }

    xs <- suppressWarnings(as.numeric(as.character(rv$user_data[[input$map_x]])))
    ys <- suppressWarnings(as.numeric(as.character(rv$user_data[[input$map_y]])))
    c_lon <- if (is.null(click)) NULL else click$lon
    c_lat <- if (is.null(click)) NULL else click$lat
    a_txt <- if (nzchar(txt)) txt else NULL

    crs_pick$busy <- TRUE
    crs_pick$rows <- NULL
    shinyjs::disable("crs_find_candidates")

    p <- promises::future_promise({
      crs_candidate_shortlist(xs, ys, lon = c_lon, lat = c_lat, area_text = a_txt)
    }, packages = "sf", seed = TRUE)

    p <- promises::then(
      p,
      onFulfilled = function(res) {
        crs_pick$rows <- if (is.null(res)) data.frame() else res
      },
      onRejected = function(err) {
        crs_pick$rows <- NULL
        showNotification(paste("CRS search failed:", conditionMessage(err)),
                         type = "error", duration = 10)
      }
    )
    # finally(), not the handlers: a rejection must never leave the button
    # disabled or the panel stuck on "Searching".
    promises::finally(p, function() {
      crs_pick$busy <- FALSE
      shinyjs::enable("crs_find_candidates")
    })
    invisible(NULL)
  })

  # Every candidate position on the locator map, numbered to match the list.
  observeEvent(crs_pick$rows, {
    rows <- crs_pick$rows
    prox <- leafletProxy("crs_locator_map") %>% clearGroup("crs_cand")
    if (is.null(rows) || !nrow(rows)) return()
    lbl <- sprintf("%d. EPSG:%d - %s", seq_len(nrow(rows)), rows$epsg, rows$text)
    prox <- prox %>% addCircleMarkers(
      lng = rows$lon, lat = rows$lat, group = "crs_cand",
      radius = 8, color = "#ffffff", weight = 2,
      fillColor = "#e17055", fillOpacity = 0.95,
      label = lbl, popup = lbl)
    click <- crs_pick$click
    lons <- c(rows$lon, if (!is.null(click)) click$lon)
    lats <- c(rows$lat, if (!is.null(click)) click$lat)
    if (diff(range(lons)) < 1e-6 && diff(range(lats)) < 1e-6) {
      prox %>% setView(lng = lons[1], lat = lats[1], zoom = 7)
    } else {
      prox %>% fitBounds(min(lons), min(lats), max(lons), max(lats))
    }
  }, ignoreNULL = FALSE, ignoreInit = TRUE)

  output$crs_candidate_list <- renderUI({
    if (isTRUE(crs_pick$busy)) {
      return(p(class = "setup-hint", style = "margin-top: 10px;",
               tags$b("Searching the EPSG registry...")))
    }
    rows <- crs_pick$rows
    if (is.null(rows)) return(NULL)
    # The text filter is capped (it does not bound the search the way a point
    # does), so a capped result must say so rather than read as exhaustive.
    capped <- if (isTRUE(attr(rows, "truncated")))
      p(class = "setup-hint", style = "margin-top: 8px; color: #b9770e;",
        "Your country or region text matches more projections than can be searched at once, so only the first 120 were tried. Narrow the text, or click the map to bound the search by position.") else NULL
    if (!nrow(rows)) {
      return(tagList(
        capped,
        p(class = "setup-hint", style = "margin-top: 10px; color: #c0392b;",
          if (isTRUE(attr(rows, "text_no_match")))
            "No projection covering the point you indicated matches that country or region text. Clear the text to see everything that reads these coordinates as a position there, or check the spelling."
          else
            "No projection in the EPSG registry reads these coordinates as a position near there. Check that X and Y are mapped to the right columns and not swapped, or indicate a point closer to the true study area.")))
    }
    tagList(
      capped,
      p(class = "setup-hint", style = "margin-top: 10px;",
        sprintf("%d position%s. The numbers match the markers on the map; equivalent codes are listed on each row.",
                nrow(rows), if (nrow(rows) == 1) "" else "s")),
      lapply(seq_len(nrow(rows)), function(i) {
        eq <- rows$equivalent[[i]]
        d <- rows$distance_km[i]
        nm <- if (is.na(rows$name[i])) "" else rows$name[i]
        div(style = "border: 1px solid #dfe6e9; border-radius: 6px; padding: 8px 10px; margin-bottom: 6px; background: #fbfcfd;",
            div(style = "display: flex; align-items: baseline; gap: 8px; flex-wrap: wrap;",
                tags$span(style = "display: inline-block; min-width: 20px; text-align: center; background: #e17055; color: #fff; border-radius: 10px; font-size: 11px; font-weight: 700; padding: 1px 6px;", i),
                tags$b(paste0("EPSG:", rows$epsg[i])),
                tags$span(style = "font-size: 12px; color: #636e72;", nm)),
            div(style = "font-size: 12px; margin: 4px 0;",
                "Plots at ", tags$b(rows$text[i]),
                if (is.finite(d)) sprintf(" - %s from the point you indicated",
                                          if (d < 1) "under 1 km" else sprintf("%.0f km", d))),
            # The fold is stated in metres. A grid's datum siblings land within a
            # few hundred metres of each other - too close for a map click to
            # separate, but not nothing - so the row says how far apart they
            # are instead of implying they are interchangeable.
            if (length(eq)) div(style = "font-size: 11px; color: #636e72; margin-bottom: 4px;",
                                sprintf("Same place to within %.0f m: EPSG:%s",
                                        rows$spread_m[i], paste(eq, collapse = ", EPSG:"))),
            # One shared input id rather than an observer per row: the rows are
            # rebuilt on every search, so per-row observers would accumulate.
            tags$button(
              type = "button", class = "btn btn-success btn-xs",
              onclick = sprintf("Shiny.setInputValue('crs_pick_epsg', %d, {priority: 'event'});",
                                rows$epsg[i]),
              "Use this CRS"))
      })
    )
  })

  observeEvent(input$crs_pick_epsg, {
    code <- suppressWarnings(as.integer(input$crs_pick_epsg))
    req(length(code) == 1, is.finite(code))
    val <- paste0("EPSG:", code)
    set_input_crs(val)
    # Never overwrite a Target Mapping CRS the user (or an uploaded .prj)
    # already chose; only fill it when it is still unset.
    if (!crs_has_value("crs_selection")) set_target_crs(val)
    crs_pick$reopen <- FALSE
    showNotification(
      paste0("Input Data CRS set to ", val,
             ". Confirm on the mini-map below - the position printed under it is where your points now plot."),
      type = "message", duration = 12, id = "crs_ident")
  })

  observeEvent(input$crs_reopen_picker, {
    crs_pick$reopen <- TRUE
  })

  # Plausibility guard for the free-typed Input Data CRS: an unrecognized CRS,
  # or one whose unit family (degrees vs metres) cannot match the mapped X/Y
  # columns, is flagged immediately - before it silently corrupts every
  # downstream projection. Warn-only: it never blocks a run and never alters
  # any computed value.
  observeEvent(list(rv$user_data, input$map_x, input$map_y, input$map_crs), {
    req(rv$user_data, input$map_x, input$map_y, input$map_crs)
    if (!(input$map_x %in% colnames(rv$user_data) && input$map_y %in% colnames(rv$user_data))) return()
    # Coerce before measuring, exactly as the CRS-identification observers above
    # already do: map_x/map_y offer EVERY column and pick_coord_column() matches
    # whole names only, so a file with POINT_X / X_coord gets no match and Shiny
    # falls back to the first column - often a character site/ID column. abs() on
    # that raised an uncaught error inside this observer and replaced the
    # guidance with a red Shiny error page.
    df <- data.frame(
      x = suppressWarnings(as.numeric(as.character(rv$user_data[[input$map_x]]))),
      y = suppressWarnings(as.numeric(as.character(rv$user_data[[input$map_y]])))
    )
    df <- df[stats::complete.cases(df), , drop = FALSE]
    if (nrow(df) == 0) {
      showNotification(
        paste0("The mapped X/Y columns ('", input$map_x, "' / '", input$map_y,
               "') hold no numeric coordinates. Map X and Y to the columns carrying ",
               "the easting/longitude and northing/latitude values."),
        type = "error", duration = 15, id = "crs_guard")
      return()
    }

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
    # Swapped axes: X within +/-90 while Y runs past it but stays inside
    # +/-180 is a latitude in the X column and a longitude in the Y column.
    if (all(abs(df$x) <= 90) && any(abs(df$y) > 90) && all(abs(df$y) <= 180)) {
      showNotification("The X column stays within +/-90 while the Y column runs past it: the two look swapped, i.e. X holds latitude and Y holds longitude. Map X to the longitude column and Y to the latitude column.",
                       type = "error", duration = 15, id = "crs_guard")
      return()
    }
    # Where the data actually lands is NOT announced here. A wrong zone produces
    # a perfectly plausible map in the wrong country and no error at all, so the
    # position has to be stated in degrees - but output$crs_landing_note prints
    # it permanently under the mini-map, two inches below, on every one of these
    # four inputs. A toast saying the same thing only queues alongside
    # crs_ident.
    removeNotification("crs_guard")
  })

  # Standing readout under the mini-map, so the landing position is visible
  # while the user compares the markers against the basemap.
  output$crs_landing_note <- renderUI({
    req(rv$user_data, rv$mapping$x, rv$mapping$y)
    if (!isTruthy(rv$mapping$crs)) {
      return(p(class = "setup-hint", style = "margin-top: 8px;",
               tags$b("Input Data CRS not set."),
               " Nothing is plotted until you choose the CRS your coordinates were recorded in."))
    }
    pos <- crs_landing_position(rv$user_data, rv$mapping$x, rv$mapping$y, rv$mapping$crs)
    req(pos)
    p(class = "setup-hint", style = "margin-top: 8px;",
      "Currently plotting at ", tags$b(pos$text), " under ", tags$b(rv$mapping$crs),
      ". If that is not your study area, the Input Data CRS is wrong - the Target Mapping CRS never moves points on the map.",
      # Only offered when the file carried no evidence; with proof in the file
      # the CRS was identified, not chosen from a list.
      if (isTRUE(crs_pick$no_evidence)) {
        tagList(" ", actionLink("crs_reopen_picker", "Locate it on the map again."))
      })
  })

  observeEvent(input$crs_selection, {
    req(input$crs_selection)

    # The metric-axis rule used to raise its own toast here. It is now one of
    # the states of output$crs_target_note below, because it is not independent
    # of the suitability verdict: a State Plane zone in US survey feet sits
    # inside its own area of use with k ~ 1, so a suitability verdict of "ok"
    # in one channel would have announced that a refused CRS suits the area
    # while the other channel refused it. One readout, one verdict.

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
  # ── Target Mapping CRS advisory ───────────────────────────────────────────
  # ONE persistent readout under the selectors, not a toast, and it always
  # names the CRS that IS right for the data.
  #
  # Three separate toasts used to carry this (identification, metric axis,
  # suitability); they could queue together, they vanished after 20 seconds,
  # and none of them prescribed anything. A user whose data was in Brandenburg
  # with the target left on UTM 35N was handed a bounding box in decimal
  # degrees and left to work out that the answer was EPSG:32633 - which the app
  # can derive from the data's own longitude, and had in fact already derived
  # for the Input Data CRS.
  #
  # crs_target_suitability() is the same judgement the run gate enforces
  # (server_execution.R), so the two can never state different rules. Silent
  # whenever the data's position cannot be computed: the gate never blocks, or
  # nags, on "cannot answer".
  crs_target_note_box <- function(tone, title, body, rec = NULL, why = NULL) {
    div(class = paste0("alert alert-", tone),
        style = "margin-top: 12px; margin-bottom: 0; padding: 10px 12px;",
        div(style = "font-weight: 600;", title,
            if (!is.null(why)) info_tooltip("crs_target_note", why)),
        div(style = "margin-top: 4px;", body),
        if (!is.null(rec))
          div(style = "margin-top: 8px;",
              "Recommended for your area: ",
              tags$b(sprintf("%s (%s)", rec$crs, rec$label)),
              if (is.finite(rec$dev))
                sprintf(", which measures to %+.3f%%.", 100 * (rec$k - 1)) else ".",
              # One shared input id and a plain button, the same route the
              # candidate picker above uses: the box is rebuilt on every input
              # change, so an actionButton observer would accumulate.
              tags$button(
                type = "button", class = "btn btn-success btn-xs",
                style = "margin-left: 8px;",
                onclick = sprintf("Shiny.setInputValue('crs_target_apply', %d, {priority: 'event'});",
                                  rec$code),
                "Use this CRS")))
  }

  output$crs_target_note <- renderUI({
    if (is.null(rv$user_data) || !isTruthy(input$map_crs) ||
        !isTruthy(input$map_x) || !isTruthy(input$map_y)) return(NULL)
    pos <- crs_sample_positions(rv$user_data, input$map_x, input$map_y, input$map_crs)
    if (is.null(pos)) return(NULL)
    rec <- crs_recommend_target(pos$lon, pos$lat)
    sel <- input$crs_selection

    # Offer the recommendation only when it beats what is selected. A study
    # area spanning three UTM zones has no zone inside tolerance at every
    # corner, and offering one there would leave the advisory red after the
    # button was pressed.
    offer <- function(cur_dev = NA_real_) {
      if (is.null(rec) || identical(rec$crs, normalize_crs_input(sel))) return(NULL)
      if (is.finite(cur_dev) && is.finite(rec$dev) && rec$dev >= cur_dev) return(NULL)
      rec
    }

    if (!isTruthy(sel)) {
      return(crs_target_note_box(
        "info", "Target Mapping CRS not set.",
        "It is the CRS every exported raster and shapefile is written in, and the one every distance the app reports is measured in.",
        rec = offer(), why = crs_measure_detail))
    }

    co <- suppressWarnings(tryCatch(sf::st_crs(sel), error = function(e) NULL))
    if (is.null(co) || is.na(co)) {
      return(crs_target_note_box(
        "danger", sprintf("Target Mapping CRS '%s' is not recognized.", sel),
        "Enter a valid EPSG code (e.g. EPSG:32633), PROJ string or WKT.",
        rec = offer(), why = crs_measure_detail))
    }

    # Ahead of the suitability verdict, because it is orthogonal to it: a State
    # Plane zone in US survey feet sits inside its own area of use with k ~ 1
    # and is still refused. validate_crs(require_metric = TRUE) is what
    # enforces this at run time.
    unit_factor <- crs_metre_factor(sel)
    if (!is.na(unit_factor) && abs(unit_factor - 1) > 1e-9) {
      return(crs_target_note_box(
        "danger",
        sprintf("'%s' is refused: its axis unit is '%s', not metres.",
                sel, as.character(co$units)),
        sprintf("Every distance it reports is %.4gx its true size.", 1 / unit_factor),
        rec = offer(), why = crs_measure_detail))
    }

    # A geographic target is legitimate and used to be answered with silence,
    # which taught the wrong lesson: users learned that EPSG:4326 makes the
    # warning go away, not that the pipeline picks the metric grid itself
    # (run_regional_interpolation(), spatial_pipeline.R). Say so.
    if (isTRUE(sf::st_is_longlat(co))) {
      return(crs_target_note_box(
        "info",
        sprintf("'%s' is geographic: maps and exports come out in longitude/latitude degrees.", sel),
        sprintf("Measurement is unaffected. The pipeline projects to %s, which it derives from your data, and every distance is computed there.",
                if (!is.null(rec)) sprintf("%s (%s)", rec$crs, rec$label) else "the UTM zone of your data")))
    }

    suit <- crs_target_suitability(sel, pos$lon, pos$lat)
    if (identical(suit$level, "ok")) {
      return(crs_target_note_box(
        "success", sprintf("'%s' suits your area.", sel),
        if (is.finite(suit$k))
          sprintf("Point scale factor k = %.6f at your data, so every distance it reports is %+.3f%% off - within the 0.1%% a grid holds across its own zone.",
                  suit$k, 100 * (suit$k - 1)) else
          "Your data falls inside the area of use it declares."))
    }

    tail_txt <- if (identical(suit$level, "block"))
      "A run with this CRS is refused unless you explicitly override it at run time."
    else if (is.finite(suit$dev) && suit$dev > 0.001)
      "A run is allowed, and every distance it reports carries that error."
    else if (is.finite(suit$k))
      sprintf("Distances here are still accurate (k = %.6f); what is wrong is the grid, whose coordinates will not line up with the datasets the region is normally mapped in.",
              suit$k)
    else
      "A run is allowed, but prefer the grid the area belongs to."

    crs_target_note_box(
      if (identical(suit$level, "block")) "danger" else "warning",
      paste0(suit$title, "."),
      paste(suit$msg, tail_txt),
      rec = offer(suit$dev), why = suit$detail)
  })

  observeEvent(input$crs_target_apply, {
    code <- suppressWarnings(as.integer(input$crs_target_apply))
    req(length(code) == 1, is.finite(code))
    set_target_crs(paste0("EPSG:", code))
  })

  observeEvent(list(rv$user_data, input$map_x, input$map_y, input$map_crs, input$crs_selection, input$locality, input$res_mode), {
    req(rv$user_data, input$map_x, input$map_y, input$map_crs, input$crs_selection, input$locality, input$res_mode)
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

  # The CRS is deliberately NOT required here. Gating the column mapping behind
  # it left rv$mapping$x/y NULL for as long as the (defaultless) Input Data CRS
  # was unset, which is the state every user is in right after an upload - and
  # every downstream req() on those names then aborted in silence, taking the
  # landing-position caption and the Run button with it.
  observeEvent(list(input$map_x, input$map_y, input$map_loc, input$map_crs), {
    req(input$map_x, input$map_y, input$map_loc)
    rv$mapping$x <- input$map_x
    rv$mapping$y <- input$map_y
    rv$mapping$loc <- input$map_loc
    rv$mapping$crs <- if (isTruthy(input$map_crs)) input$map_crs else NULL
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
    # req(), not return(NULL): leaflet's renderValue destroys the existing map
    # BEFORE it dereferences the payload, so a NULL payload wipes the widget and
    # then throws inside Shiny's message dispatch, dropping the rest of the batch.
    req(nrow(df_map) > 0)


    pts <- tryCatch({
      st_as_sf(df_map, coords = c(rv$mapping$x, rv$mapping$y), crs = rv$mapping$crs) %>% st_transform(4326)
    }, error = function(e) NULL)
    req(pts)

    current_tiles <- input$base_map_layer %||% "Esri.WorldImagery"

    m <- leaflet(pts, options = leafletOptions(zoomControl = FALSE)) %>% add_base_tiles(current_tiles)

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

    # Validation adornments, always on (this map exists to be checked against
    # reality; the Map Viewer's checkboxes are there because those maps are
    # exported, which this one is not). zoomControl is off, so topleft is free;
    # the locality legend sits bottomright and the scale bar bottomleft.
    m <- m %>%
      leaflet::addScaleBar(position = "bottomleft",
                           options = leaflet::scaleBarOptions(metric = TRUE, imperial = FALSE)) %>%
      leaflet::addControl(html = map_north_arrow_html(), position = "topleft",
                          layerId = "north_ctrl")

    session_state$minimap_rendered <- TRUE
    # Remember the sample bounds (WGS84) so the reveal handler in chunk H can
    # re-frame the map: Leaflet's auto-fit at creation is computed for whatever
    # size the container had at render time, and this map is routinely rendered
    # while its tab is hidden (a run re-renders it, and the run observer
    # switches to the Map Viewer at dispatch).
    session_state$minimap_bbox <- tryCatch({
      bb <- sf::st_bbox(pts)
      c(xmin = unname(bb[["xmin"]]), ymin = unname(bb[["ymin"]]),
        xmax = unname(bb[["xmax"]]), ymax = unname(bb[["ymax"]]))
    }, error = function(e) NULL)
    m
  })

