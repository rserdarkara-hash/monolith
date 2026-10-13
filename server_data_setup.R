# server_data_setup.R (chunk D, sourced with local = TRUE inside server): data,
# shapefile and metadata upload, CRS identification and plausibility guards,
# variable mapping and the Setup-tab minimap.
  output$export_updated_data <- downloadHandler(
    filename = function() {
      paste0("updated_spatial_dataset_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".xlsx")
    },
    content = function(file) {
      req(rv$user_data)
      openxlsx::write.xlsx(rv$user_data, file = file)
    }
  )
  
  # The shipped sample dataset, recognised by its OWN FILE NAME only, so no
  # preset can reach a user's dataset.
  is_sample_upload <- function() {
    nm <- isolate(input$user_file$name)
    isTruthy(nm) && grepl("^samp_data_1", tools::file_path_sans_ext(nm), ignore.case = TRUE)
  }
  # The sample's zone cannot be derived from bare eastings (34N-36N each place
  # them in their own span); its locality names (Denizli districts) settle it
  # at 35N. A documented fact about one shipped file, not a coordinate guess.
  SAMPLE_CRS <- "EPSG:32635"

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
    # Whole-name matching (pick_coord_column()); NULL leaves the first column.
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
          unit = "", palette = get_default_palette(col, "Uploaded Data", col)
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
    # A new dataset is a new CRS question: the Input Data CRS is cleared on
    # EVERY upload (a CRS typed for the previous file was never established for
    # this one); within a dataset a CRS the user set is never overridden. The
    # Target Mapping CRS names the output system, so a USER choice stays (the
    # suitability readout judges it against the new data) and an app-filled
    # value is re-identified. The sample preset below sets both.
    keep_target <- crs_user_chose("crs_selection") && !is_sample_upload()
    clear_input_crs()
    if (keep_target) {
      # The kept value is the user's; the app's record of what it wrote for the
      # previous file does not carry over to this one.
      session_state$crs_auto$crs_selection <- character(0)
      showNotification(paste0("Target Mapping CRS kept as ", crs_effective("crs_selection"),
                              " from your previous selection. Maps and exports for this dataset will be produced in that system. The note under the selector says whether it suits the new data."),
                       type = "message", duration = 15)
    } else {
      clear_target_crs()
    }

    # The app's only preset: the shipped sample opens ready to run, with the
    # two localities the manuscript examines and its UTM zone. Recorded as the
    # app's values (not user choices), so the CRS advisor still judges them;
    # clearing the Locality box still means every locality.
    if (is_sample_upload()) {
      preset <- character(0)
      if (length(selected_locs) == 0) {
        selected_locs <- intersect(c("Kale", "Yorga"), new_choices)
        if (length(selected_locs) > 0) {
          preset <- c(preset, paste0("Locality to ", paste(selected_locs, collapse = " and "),
                                     " (the two the accompanying manuscript examines in detail; clear the box for all ",
                                     length(new_choices) - 1L, ")"))
        }
      }
      set_input_crs(SAMPLE_CRS)
      set_target_crs(SAMPLE_CRS)
      preset <- c(preset, paste0("Input and Target CRS to ", SAMPLE_CRS, " (UTM 35N)"))
      showNotification(paste0("Sample dataset recognised - preset ", paste(preset, collapse = "; "), "."),
                       type = "message", duration = 12)
    }
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
    # One directory per upload: a name taken from the clock would let a second
    # upload within the same second mix its files with the first one's.
    temp_dir <- tempfile("shp_upload_")
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
    # Point/line layers are described by output$shp_boundary_note (convex hull).
    if (all(as.character(sf::st_geometry_type(s)) %in% c("POLYGON", "MULTIPOLYGON"))) {
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
      # A boundary's .prj says how that file was saved, not which grid suits the
      # analysis: the pipeline reprojects the boundary to the data's CRS. It
      # fills an empty Target Mapping CRS and never replaces one already set.
      if (!crs_has_value("crs_selection")) {
        set_target_crs(crs_val)
      } else {
        cur <- crs_effective("crs_selection")
        same <- isTRUE(tryCatch(sf::st_crs(cur) == crs_obj, error = function(e) FALSE))
        if (!same && nzchar(cur)) {
          shp_lab <- if (!is.null(crs_obj$epsg) && !is.na(crs_obj$epsg)) paste0("EPSG:", crs_obj$epsg) else (crs_obj$Name %||% "its own CRS")
          showNotification(sprintf("The boundary is stored in %s. It is reprojected to your data's CRS at run time; the Target Mapping CRS stays %s.",
                                   shp_lab, cur),
                           type = "message", duration = 12)
        }
      }
    } else {
      showNotification("The uploaded shapefile carries no CRS definition (.prj missing?). Its coordinates are assumed to be in the Input Data CRS, the system your sample coordinates are recorded in. If the note under the upload reports no samples inside it, re-export the shapefile with its .prj file.",
                       type = "warning", duration = 15)
    }
  })

  # Readout under the boundary upload: whether the samples fall inside it,
  # whatever the upload order. A boundary enclosing no sample is not applied
  # at run time, so the user is told here, not only in the run log.
  output$shp_boundary_note <- renderUI({
    s <- rv$shp_bound
    req(s)
    note <- function(tone, txt) div(class = paste0("alert alert-", tone),
                                    style = "margin: 8px 0 0 0; padding: 6px 10px;", txt)
    hull_txt <- "The layer holds points or lines, so its convex hull is used as the boundary."
    has_map <- !is.null(rv$user_data) && isTruthy(input$map_x) && isTruthy(input$map_y) &&
      isTruthy(input$map_crs) && all(c(input$map_x, input$map_y) %in% colnames(rv$user_data))
    ov <- if (has_map) shp_boundary_overlap(s, rv$user_data[[input$map_x]],
                                             rv$user_data[[input$map_y]], input$map_crs)
    if (is.null(ov)) {
      hull <- !all(as.character(sf::st_geometry_type(s)) %in% c("POLYGON", "MULTIPOLYGON"))
      return(if (hull) note("info", hull_txt))
    }
    if (!ov$usable)
      return(note("warning", "The layer's features do not enclose an area (fewer than 3 non-collinear points), so it cannot serve as a boundary; the sidebar's boundary type is used instead."))
    if (ov$inside == 0)
      return(note("warning", sprintf("None of your %d samples fall inside this boundary (it lies %s km from them), so it will not be applied: each locality uses the boundary type set in the sidebar.%s",
                                     ov$n, format(round(ov$dist_km, 1), big.mark = ","),
                                     if (ov$hull) paste0(" ", hull_txt) else "")))
    if (ov$inside < ov$n)
      return(note("info", sprintf("%d of your %d samples fall inside this boundary. A locality with no sample inside it uses the boundary type set in the sidebar.%s",
                                  ov$inside, ov$n, if (ov$hull) paste0(" ", hull_txt) else "")))
    if (ov$hull) note("info", hull_txt)
  })

  # selectize refuses a value that is not an option, so a CRS the app sets
  # travels with its own choice entry (re-supplying `choices` re-creates the
  # widget). `record = FALSE` writes the value without claiming it as the
  # app's: a typed `32633` promoted to `EPSG:32633` stays the user's choice.
  # crs_near: the UTM zones around the data, listed first; a reactiveVal so
  # the selectors rebuild only when the zones change.
  crs_near <- reactiveVal(integer(0))
  set_crs_choice <- function(id, value, placeholder, base, record = TRUE) {
    ch <- crs_choice_groups(base, isolate(crs_near()), extra = value)
    if (isTRUE(record)) {
      session_state$crs_auto[[id]] <- unique(c(session_state$crs_auto[[id]], value))
    }
    # A real write supersedes any pending clear: whatever the browser is still
    # showing, this is the value the selector now holds.
    session_state$crs_stale[[id]] <- NULL
    session_state$crs_last[[id]] <- value
    updateSelectizeInput(session, id, choices = ch, selected = value,
                         options = list(create = TRUE, placeholder = placeholder))
  }
  # What a selector effectively holds. An update has not round-tripped within
  # the same flush, so after a clear input$<id> still reports the PREVIOUS
  # dataset's CRS; the cleared value reads as empty until the browser confirms
  # it (the normalisation observers then drop the marker).
  crs_effective <- function(id) {
    v <- as.character(input[[id]] %||% "")
    if (nzchar(v) && identical(v, session_state$crs_stale[[id]] %||% "")) "" else v
  }
  # Did the USER put the current value there? The app's record holds every
  # value it wrote, not just the last, for the same round-trip reason.
  crs_user_chose <- function(id) {
    crs_chosen_by_user(input[[id]], session_state$crs_auto[[id]], session_state$crs_stale[[id]])
  }
  crs_has_value <- function(id) {
    nzchar(crs_effective(id)) || length(session_state$crs_auto[[id]]) > 0
  }
  # Two base lists: Web Mercator is an input option only. Saved configs and
  # typed entries still carry their own choice entry; the suitability gate
  # decides whether a CRS may be run.
  set_input_crs  <- function(value, record = TRUE) set_crs_choice("map_crs", value, "Select the CRS your coordinates were recorded in", common_crs_input, record)
  set_target_crs <- function(value, record = TRUE) set_crs_choice("crs_selection", value, "Select the CRS for output maps and exports", common_crs_target, record)
  # Back to the pristine selector (base choices, nothing selected), clearing
  # the recorded auto-values and remembering the cleared value for
  # crs_effective() until the clear lands.
  clear_crs_choice <- function(id, placeholder, base) {
    session_state$crs_auto[[id]] <- character(0)
    cur <- as.character(isolate(input[[id]]) %||% "")
    session_state$crs_stale[[id]] <- if (nzchar(cur)) cur else NULL
    session_state$crs_last[[id]] <- ""
    updateSelectizeInput(session, id, choices = crs_choice_groups(base, isolate(crs_near())), selected = "",
                         options = list(create = TRUE, placeholder = placeholder))
  }
  clear_input_crs  <- function() clear_crs_choice("map_crs", "Select the CRS your coordinates were recorded in", common_crs_input)
  clear_target_crs <- function() clear_crs_choice("crs_selection", "Select the CRS for output maps and exports", common_crs_target)

  # Promote a typed bare EPSG number (`32633`) to `EPSG:32633` in place; a
  # no-op for parseable values, so the re-entry settles. A NON-empty value
  # also retires the pending-clear marker: it is a real selection.
  observeEvent(input$map_crs, {
    session_state$crs_last$map_crs <- input$map_crs
    req(input$map_crs)
    session_state$crs_stale$map_crs <- NULL
    norm <- normalize_crs_input(input$map_crs)
    if (!identical(norm, input$map_crs)) set_input_crs(norm, record = FALSE)
  }, priority = 10)
  observeEvent(input$crs_selection, {
    session_state$crs_last$crs_selection <- input$crs_selection
    req(input$crs_selection)
    session_state$crs_stale$crs_selection <- NULL
    norm <- normalize_crs_input(input$crs_selection)
    if (!identical(norm, input$crs_selection)) set_target_crs(norm, record = FALSE)
  }, priority = 10)

  # Where the data is, for ordering the two selectors: the Tier-3 click while
  # the file carries no evidence of its grid, otherwise the data's centroid
  # read through the Input Data CRS (the same position the advisory uses).
  observe({
    pos <- if (isTRUE(crs_pick$no_evidence) && !is.null(crs_pick$click)) {
      crs_pick$click
    } else if (!is.null(rv$user_data) && isTruthy(input$map_crs) &&
               isTruthy(input$map_x) && isTruthy(input$map_y)) {
      p <- crs_sample_positions(rv$user_data, input$map_x, input$map_y, input$map_crs)
      if (!is.null(p)) list(lon = p$lon[1], lat = p$lat[1])
    }
    crs_near(if (is.null(pos)) integer(0) else crs_near_zone_codes(pos$lon, pos$lat))
  })
  # Rebuild both selectors when the zones change, re-sending the selection
  # from crs_last rather than input$ (a value set in this flush has not
  # round-tripped). The input observers run at priority 10 so a user's change
  # reaches crs_last before the rebuild it triggers.
  observeEvent(crs_near(), {
    near <- crs_near()
    rebuild <- function(id, placeholder, base) {
      cur <- session_state$crs_last[[id]] %||% ""
      updateSelectizeInput(session, id, choices = crs_choice_groups(base, near, extra = cur),
                           selected = cur,
                           options = list(create = TRUE, placeholder = placeholder))
    }
    rebuild("map_crs", "Select the CRS your coordinates were recorded in", common_crs_input)
    rebuild("crs_selection", "Select the CRS for output maps and exports", common_crs_target)
  }, ignoreInit = TRUE)

  # Input-CRS identification. Degrees are self-evident; projected coordinates
  # are identified ONLY from evidence in the upload (companion lon/lat pair,
  # boundary .prj containing the points). Otherwise the app asks: a guessed
  # zone places the survey in the wrong country without any error.
  observeEvent(list(rv$user_data, input$map_x, input$map_y, rv$shp_bound), {
    req(rv$user_data, input$map_x, input$map_y)
    if (!(input$map_x %in% colnames(rv$user_data) && input$map_y %in% colnames(rv$user_data))) return()
    # Nothing to identify from a column pair with no usable numbers; the
    # coordinate-validity modal at run time is what reports that.
    xs <- suppressWarnings(as.numeric(as.character(rv$user_data[[input$map_x]])))
    ys <- suppressWarnings(as.numeric(as.character(rv$user_data[[input$map_y]])))
    if (sum(is.finite(xs) & is.finite(ys)) < 1) return()

    ident <- identify_input_crs(rv$user_data, input$map_x, input$map_y, rv$shp_bound)
    # Degree ranges without evidence: EPSG:4326 is offered, never applied.
    crs_pick$degree_suggest <- !is.null(ident) && is.null(ident$crs)
    if (isTRUE(crs_pick$degree_suggest)) {
      crs_pick$no_evidence <- FALSE
      crs_pick$rows <- NULL
      if (!isTruthy(crs_effective("map_crs")))
        showNotification(ident$message, type = "warning", duration = 15, id = "crs_ident")
      return()
    }
    if (is.null(ident)) {
      # The shipped sample has no evidence either, but its zone is already set
      # by the upload preset.
      if (is_sample_upload() && crs_has_value("map_crs")) {
        crs_pick$no_evidence <- FALSE
        crs_pick$rows <- NULL
        return()
      }
      # Tier 3: the picker below lists the projections that put the data where
      # the user says it is.
      crs_pick$no_evidence <- TRUE
      crs_pick$rows <- NULL
      showNotification(
        "Coordinates look projected (metre magnitudes) and the file carries no evidence of which grid they are on - no lon/lat columns and no boundary shapefile. Use 'Locate your study area' below to narrow it down, or set the Input Data CRS yourself.",
        type = "warning", duration = 15, id = "crs_ident")
      return()
    }

    crs_pick$no_evidence <- FALSE
    # Never revert a CRS the user set deliberately (e.g. a datum sibling of the
    # identified code); this observer re-fires on uploads and boundaries.
    if (crs_user_chose("map_crs")) return()
    # A boundary upload re-runs this observer; when it confirms the CRS already
    # in place there is nothing new to announce.
    unchanged <- identical(crs_effective("map_crs"), ident$crs)
    if (!unchanged) set_input_crs(ident$crs)
    # Never overwrite a Target Mapping CRS the user (or an uploaded .prj)
    # already chose; only fill it when it is still unset.
    if (!crs_has_value("crs_selection")) {
      set_target_crs(ident$crs)
    }
    if (!unchanged)
      showNotification(ident$message, type = "message", duration = 15, id = "crs_ident")
  })

  # ── Tier 3: locate the study area, then confirm a place ───────────────────
  # Bare eastings do not reveal their zone, so the user clicks roughly where
  # the data was collected (or names a country) and confirms a PLACE among the
  # projections that put the data there. The search (crs_candidate_shortlist())
  # takes about a second, so it runs in a future_promise with every input read
  # BEFORE the call.
  crs_pick <- reactiveValues(no_evidence = FALSE, click = NULL, rows = NULL,
                             busy = FALSE, reopen = FALSE, degree_suggest = FALSE)

  crs_picker_visible <- reactive({
    isTRUE(crs_pick$no_evidence) && (!isTruthy(input$map_crs) || isTRUE(crs_pick$reopen))
  })

  output$crs_picker_ui <- renderUI({
    if (isTRUE(crs_pick$degree_suggest) && !isTruthy(input$map_crs)) {
      return(div(style = "margin-top: 14px; border-top: 1px solid var(--mn-line); padding-top: 12px;",
          h5("Are these coordinates longitude/latitude?", style = "font-weight: 600; margin-bottom: 4px;"),
          p(class = "setup-hint",
            sprintf("Every X value lies within -180 to 180 and every Y value within -90 to 90, so they could be degrees. The file does not confirm it: the '%s'/'%s' columns are not named as longitude/latitude and no boundary shapefile confirms their position. A local metre grid fits these ranges too: read as degrees, a field plot with X = 2-48 m and Y = 5-60 m becomes a domain thousands of kilometres wide.",
                    input$map_x, input$map_y)),
          actionButton("crs_use_degrees", "Use EPSG:4326 (WGS 84 longitude/latitude)",
                       icon = icon("check"), class = "btn-success btn-sm"),
          p(class = "setup-hint", style = "margin-top: 6px;",
            "If they are metres or another system, set the Input Data CRS above to the system they were recorded in.")))
    }
    req(crs_picker_visible())
    div(style = "margin-top: 14px; border-top: 1px solid var(--mn-line); padding-top: 12px;",
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
    # Fixed OpenStreetMap: this map is read for country borders, which the
    # other basemaps lack or need a CARTO key for.
    leaflet(options = leafletOptions(minZoom = 1, worldCopyJump = TRUE)) %>%
      addProviderTiles("OpenStreetMap") %>%
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
      p(class = "setup-hint", style = "margin-top: 8px; color: var(--mn-warn);",
        "Your country or region text matches more projections than can be searched at once, so only the first 120 were tried. Narrow the text, or click the map to bound the search by position.") else NULL
    if (!nrow(rows)) {
      return(tagList(
        capped,
        p(class = "setup-hint", style = "margin-top: 10px; color: var(--mn-danger);",
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
        div(style = "border: 1px solid var(--mn-line); border-radius: 6px; padding: 8px 10px; margin-bottom: 6px; background: var(--mn-surface-2);",
            div(style = "display: flex; align-items: baseline; gap: 8px; flex-wrap: wrap;",
                tags$span(style = "display: inline-block; min-width: 20px; text-align: center; background: var(--mn-accent-weak); color: var(--mn-accent); border-radius: 10px; font-size: 11px; font-weight: 700; padding: 1px 6px;", i),
                tags$b(paste0("EPSG:", rows$epsg[i])),
                tags$span(style = "font-size: 12px; color: var(--mn-text-3);", nm)),
            div(style = "font-size: 12px; margin: 4px 0;",
                "Plots at ", tags$b(rows$text[i]),
                if (is.finite(d)) sprintf(" - %s from the point you indicated",
                                          if (d < 1) "under 1 km" else sprintf("%.0f km", d))),
            # Datum siblings land within a few hundred metres: the row states
            # how far apart rather than implying they are interchangeable.
            if (length(eq)) div(style = "font-size: 11px; color: var(--mn-text-3); margin-bottom: 4px;",
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

  # The user's confirmation that degree-range coordinates are WGS 84 lon/lat.
  # record = FALSE: this is the user's choice, so a later re-identification
  # must not treat it as a value the app wrote and may replace.
  observeEvent(input$crs_use_degrees, {
    set_input_crs("EPSG:4326", record = FALSE)
    if (!crs_has_value("crs_selection")) set_target_crs("EPSG:4326")
    showNotification(
      "Input Data CRS set to EPSG:4326. Confirm on the mini-map below - the position printed under it is where your points now plot.",
      type = "message", duration = 12, id = "crs_ident")
  })

  # Plausibility guard for the Input Data CRS: an unrecognised CRS, or a unit
  # family (degrees vs metres) the mapped X/Y cannot match, is flagged at once.
  # Warn-only: never blocks a run or alters a value.
  observeEvent(list(rv$user_data, input$map_x, input$map_y, input$map_crs), {
    req(rv$user_data, input$map_x, input$map_y, input$map_crs)
    if (!(input$map_x %in% colnames(rv$user_data) && input$map_y %in% colnames(rv$user_data))) return()
    # Coerce first: a file without recognisable coordinate names maps the
    # first column, often character, and abs() on it would error here.
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
    # The landing position is not toasted here: output$crs_landing_note states
    # it permanently under the mini-map.
    removeNotification("crs_guard")
    removeNotification("input_crs_distortion")
    pos <- crs_sample_positions(rv$user_data, input$map_x, input$map_y, input$map_crs)
    if (!is_longlat && !is.null(pos)) {
      k <- crs_scale_factor(input$map_crs, pos$lon, pos$lat)
      if (!is.null(k) && isTRUE(k$dev > 0.01)) {
        showNotification(sprintf(
          "Input Data CRS has distance distortion up to %.2f%% here. Metre-based projected input is used for interpolation as supplied. Reproject the data to its local UTM zone before upload to reduce distortion; changing the CRS label alone does not reproject coordinates.",
          100 * k$dev), type = "warning", duration = 20, id = "input_crs_distortion")
      }
    }
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

    # The metric-axis rule has no toast of its own: it is one state of
    # output$crs_target_note, so one readout gives one verdict.

    # Fixed mode keeps the user's value and refreshes only the slider frame; in
    # the Auto modes the suggestion observer sets the value right after.
    if (isTRUE(input$res_mode == "fixed")) {
      updateSliderInput(session, "grid_res", label = "Resolution (m)",
                        min = 1, max = 500, step = 1)
    } else {
      updateSliderInput(session, "grid_res", label = "Resolution (m)",
                        min = 1, max = 500, value = 50, step = 1)
    }
  })
  # ── Target Mapping CRS advisory ───────────────────────────────────────────
  # ONE persistent readout under the selectors (not toasts, which queue and
  # vanish), always naming the CRS that IS right for the data, derived from
  # its longitude. crs_target_suitability() is the run gate's own judgement
  # (server_execution.R). Silent when the data's position cannot be computed.
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
              # One shared input id and a plain button, as in the candidate
              # picker: a rebuilt box would accumulate actionButton observers.
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

    # Offer the recommendation only when it beats the selection (a study area
    # spanning three zones has no zone within tolerance everywhere).
    offer <- function(cur_dev = NA_real_) {
      if (is.null(rec) || identical(rec$crs, normalize_crs_input(sel))) return(NULL)
      if (is.finite(cur_dev) && is.finite(rec$dev) && rec$dev >= cur_dev) return(NULL)
      rec
    }

    if (!isTruthy(sel)) {
      return(crs_target_note_box(
        "info", "Target Mapping CRS not set.",
        "It is the CRS the finished surface is resampled into and every exported raster and shapefile is written in. For projected data, choose the same UTM zone as the Input Data CRS.",
        rec = offer(), why = crs_measure_detail))
    }

    co <- suppressWarnings(tryCatch(sf::st_crs(sel), error = function(e) NULL))
    if (is.null(co) || is.na(co)) {
      return(crs_target_note_box(
        "danger", sprintf("Target Mapping CRS '%s' is not recognized.", sel),
        "Enter a valid EPSG code (e.g. EPSG:32633), PROJ string or WKT.",
        rec = offer(), why = crs_measure_detail))
    }

    # Before the suitability verdict, which it is orthogonal to (a State Plane
    # zone in US feet can have k ~ 1); validate_crs(require_metric = TRUE)
    # enforces it at run time.
    unit_factor <- crs_metre_factor(sel)
    if (!is.na(unit_factor) && abs(unit_factor - 1) > 1e-9) {
      return(crs_target_note_box(
        "danger",
        sprintf("'%s' is refused: its axis unit is '%s', not metres.",
                sel, as.character(co$units)),
        sprintf("Every distance it reports is %.4gx its true size.", 1 / unit_factor),
        rec = offer(), why = crs_measure_detail))
    }

    # A geographic target is answered, not met with silence: the pipeline
    # picks the metric grid itself (run_regional_interpolation()).
    if (isTRUE(sf::st_is_longlat(co))) {
      return(crs_target_note_box(
        "info",
        sprintf("'%s' is geographic: maps and exports come out in longitude/latitude degrees.", sel),
        if (isTRUE(tryCatch(sf::st_is_longlat(sf::st_crs(input$map_crs)), error = function(e) FALSE)))
          "The models are unaffected: the pipeline projects your geographic input to the UTM zone of each locality and computes every distance there."
        else
          sprintf("The models are fitted in your Input Data CRS (%s); only the finished surface is resampled into degrees.", input$map_crs)))
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

  # The dataset-wide cell-size suggestion that prefills the Fixed slider
  # (Scientific Guide 2.1), written in the Auto modes only: in Fixed mode the
  # slider holds the user's own value.
  observeEvent(list(rv$user_data, input$map_x, input$map_y, input$map_crs, input$crs_selection, input$locality, input$res_mode), {
    req(rv$user_data, input$map_x, input$map_y, input$map_crs, input$crs_selection, input$res_mode)
    if (input$res_mode == "fixed") return()
    if (!(input$map_x %in% colnames(rv$user_data) && input$map_y %in% colnames(rv$user_data))) return()

    df_raw <- rv$user_data %>% select(x = !!sym(input$map_x), y = !!sym(input$map_y), loc = !!sym(input$map_loc)) %>% na.omit()

    locs_scope <- resolve_selected_localities(input$locality, df_raw, "loc")

    df <- if (input$res_mode == "global") df_raw else df_raw %>% filter(loc %in% locs_scope)
    
    if (nrow(df) < 2) return()
    
    crs_obj <- validate_crs(input$crs_selection, "Invalid CRS provided:")
    req(crs_obj)

    pts <- tryCatch({
      st_as_sf(df, coords = c("x", "y"), crs = input$map_crs) %>% st_transform(input$crs_selection)
    }, error = function(e) NULL)
    req(pts)

    # The slider is metric and runs are projected, so the suggestion is in
    # metres even for a geographic analysis CRS (calc_metric_spacing()).
    spacing <- calc_metric_spacing(pts)
    req(is.finite(spacing$mean_nn))

    rec_res <- spacing$mean_nn * 0.5
    min_res_by_dim <- spacing$max_dim / 300

    final_rec <- max(rec_res, min_res_by_dim)
    # Clamped to the slider's own frame (1-500 m), so the figure prefilled here
    # is the figure the widget can hold.
    final_rec <- max(1, min(500, round(final_rec, 1)))

    updateSliderInput(session, "grid_res", value = final_rec)
  })

  # Per locality, the resolution the Auto dynamic buffer scales with, for the
  # sidebar's "Grid and buffer for the next run" table, built as the run
  # builds it (.locality_points(), locality_buffer_res()). NA where the run
  # would skip the locality. Fixed mode scales the buffer with the slider.
  observeEvent(list(rv$user_data, rv$mapping, input$locality, input$method, input$aux_vars,
                    input$value_type, input$subset), {
    req(rv$user_data)
    ud <- rv$user_data
    m <- rv$mapping
    cols_ok <- all(vapply(list(m$x, m$y, m$loc), function(col) {
      is_valid_col_ref(col) && col %in% names(ud)
    }, logical(1)))
    if (!cols_ok || !isTruthy(m$crs)) {
      rv$loc_buffer_res <- list()
      return()
    }
    locs <- resolve_selected_localities(input$locality, ud, m$loc)
    eff_subset <- effective_subset(input$value_type, input$subset, names(ud))
    method <- input$method %||% "OK"
    rv$loc_buffer_res <- stats::setNames(lapply(locs, function(l) {
      pts_data <- run_locality_rows(ud, m$loc, l, eff_subset)
      pts_data$x <- pts_data[[m$x]]
      pts_data$y <- pts_data[[m$y]]
      # The display set does not depend on the CV population, so no m_params.
      pp <- tryCatch(.locality_points(l, pts_data, m$crs, method, input$aux_vars, list()),
                     error = function(e) NULL)
      if (!isTRUE(pp$ok)) return(NA_real_)
      locality_buffer_res(pp$pts, "local", NA_real_)
    }), locs)
  })

  observeEvent(input$meta_file, {
    req(input$meta_file)
    # The variable list is matched against the dataset's columns, so the
    # dataset must be loaded first.
    if (is.null(rv$user_data)) {
      showNotification("Upload your dataset first - the variable list is matched against its column names.", type = "warning")
      return()
    }
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
          def_u     <- get_map_val(t, "unit")     %||% ""

          if(is.na(def_p_cve)) def_p_cve <- "None"
          if(is.na(def_p_ss)) def_p_ss <- "None"
          if(is.na(def_l)) def_l <- t
          if(is.na(def_c)) def_c <- "Uploaded Data"

          div(style="border-bottom: 1px solid var(--mn-line); padding: 10px 0; margin-bottom: 10px;",
            fluidRow(
              column(2, tags$b(t)),
              column(2, selectInput(paste0("pair_pred_cve_", i), "Best Pred (_cve)", choices = c("None", num_cols), selected = def_p_cve)),
              column(2, selectInput(paste0("pair_pred_ss_", i),  "Split Pred (_ss)", choices = c("None", num_cols), selected = def_p_ss)),
              column(2, textInput(paste0("pair_label_", i), "Label", value = def_l)),
              column(2, textInput(paste0("pair_cat_", i), "Category", value = def_c)),
              # The unit the values are recorded in: map legends, the GeoTIFF
              # MONOLITH_UNIT tag and the supervised class limits read it.
              column(2, textInput(paste0("pair_unit_", i), "Unit", value = def_u,
                                  placeholder = "e.g. mg/kg"))
            )
          )
        }),
        actionButton("confirm_mapping", "Confirm Variable Mapping", icon = icon("check-circle"), class = "btn-primary btn-block btn-pill")
      )
    }, error = function(e) {
      warning(paste("Error in var_mapping_ui:", e$message))
      h4(paste("Error rendering UI:", e$message), style = "color: var(--mn-danger);")
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

      raw_unit <- input[[paste0("pair_unit_", i)]]
      unit_val <- if (is.null(raw_unit) || is.na(raw_unit)) "" else trimws(raw_unit)

      new_vars[[length(new_vars) + 1]] <- list(
        actual = targets[i],
        pred = if (is.null(p_cve) || is.na(p_cve) || p_cve == "None") NULL else p_cve,
        pred_ss = if (is.null(p_ss) || is.na(p_ss) || p_ss == "None") NULL else p_ss,
        label = lab_val,
        category = cat_val,
        unit = unit_val,
        palette = get_default_palette(targets[i], cat_val, lab_val)
      )
    }
    rv$mapping$vars <- new_vars
    showNotification("Variable mapping saved!", type = "message")
  })

  # Rebuild the Context-panel locality selector, keeping the selection, only
  # when the choice set changed.
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

  # The CRS is deliberately NOT required: right after an upload the Input Data
  # CRS is unset, and gating the column mapping on it would silence every
  # downstream req(), the landing caption and the Run button included.
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
    # req(), not return(NULL): leaflet destroys the map before reading the
    # payload, so NULL wipes it and errors inside Shiny's dispatch.
    req(nrow(df_map) > 0)


    pts <- tryCatch({
      st_as_sf(df_map, coords = c(rv$mapping$x, rv$mapping$y), crs = rv$mapping$crs) %>% st_transform(4326)
    }, error = function(e) NULL)
    req(pts)

    current_tiles <- input$base_map_layer %||% "Esri.WorldImagery"

    m <- leaflet(pts, options = leafletOptions(zoomControl = FALSE)) %>%
      add_base_tiles(current_tiles, carto_api_key())

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
      # Validation view: one colour per locality, so a mis-mapped coordinate
      # pair or CRS shows as points landing in the wrong group.
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

    # Validation adornments, always on (this map is checked, never exported).
    # zoomControl is off, so topleft is free for the north arrow.
    m <- m %>%
      leaflet::addScaleBar(position = "bottomleft",
                           options = leaflet::scaleBarOptions(metric = TRUE, imperial = FALSE)) %>%
      leaflet::addControl(html = map_north_arrow_html(), position = "topleft",
                          layerId = "north_ctrl")

    session_state$minimap_rendered <- TRUE
    # Sample bounds (WGS84) for chunk H's reveal handler to re-frame the map,
    # which is often rendered while its tab is hidden.
    session_state$minimap_bbox <- tryCatch({
      bb <- sf::st_bbox(pts)
      c(xmin = unname(bb[["xmin"]]), ymin = unname(bb[["ymin"]]),
        xmax = unname(bb[["xmax"]]), ymax = unname(bb[["ymax"]]))
    }, error = function(e) NULL)
    m
  })

  # Header context strip: dataset, size, both CRSs and locality count, visible
  # from every tab.
  output$dataset_context <- renderUI({
    df <- rv$user_data
    if (is.null(df)) return(NULL)
    fname <- tryCatch(input$user_file$name, error = function(e) NULL)
    loc_col <- rv$mapping$loc
    n_locs <- if (!is.null(loc_col) && loc_col %in% colnames(df)) {
      length(unique(stats::na.omit(df[[loc_col]])))
    } else {
      NA_integer_
    }
    # Both systems, each named: the input CRS (coordinates, and the models when
    # metric) and the target CRS (outputs).
    crs_in <- normalize_crs_input(input$map_crs)
    crs_out <- normalize_crs_input(input$crs_selection)
    item <- function(label, value) {
      tags$span(class = "mn-ctx-item", label, tags$b(value))
    }
    tags$div(
      class = "mn-ctx",
      if (!is.null(fname)) item("Dataset", fname),
      item("Points", format(nrow(df), big.mark = ",")),
      if (isTruthy(crs_in)) item("Input CRS", crs_in),
      if (isTruthy(crs_out)) item("Target CRS", crs_out),
      if (!is.na(n_locs)) item("Localities", format(n_locs))
    )
  })
