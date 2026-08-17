# classif_module.R — Shiny module for the Classification Suite.
#
# Decoupled like gov_module.R / desc_exploratory_module.R: configuration + a
# background (future_promise) run of run_classification_pipeline(), then display
# of cross-validation performance, the confusion matrix, per-class accuracy, and
# predicted-class / probability / entropy maps with GeoTIFF + CSV export.
#
# The heavy compute returns only serialisable objects; terra rasters are built
# in the main session (classif_surface_to_rasters) for rendering and download.

# ── Target factor construction ──────────────────────────────────────────────
# Build the categorical target used for modelling. `cat` mode uses an existing
# categorical column verbatim; `bin` mode discretises a numeric column into
# ordered classes using classInt break styles (right = FALSE, matching the
# app's agronomic-class convention so intervals read as [low, high)).
classif_build_target <- function(df, mode, cat_col, num_col, n_classes = 4,
                                 style = "quantile") {
  if (identical(mode, "bin")) {
    x <- as.numeric(df[[num_col]])
    ci <- classInt::classIntervals(x[is.finite(x)], n = n_classes, style = style)
    brks <- unique(ci$brks)
    if (length(brks) < 3) return(NULL)
    # Distinct breaks can still collide after rounding (tight distributions),
    # and factor() errors on duplicated levels — widen the label precision
    # until every interval label is unique.
    lab_digits <- 2
    repeat {
      labs <- paste0("[", round(utils::head(brks, -1), lab_digits), ", ",
                     round(brks[-1], lab_digits), ")")
      if (anyDuplicated(labs) == 0 || lab_digits >= 10) break
      lab_digits <- lab_digits + 1
    }
    if (anyDuplicated(labs) > 0) return(NULL)
    factor(cut(x, breaks = brks, labels = labs, include.lowest = TRUE, right = FALSE),
           levels = labs)
  } else {
    factor(as.character(df[[cat_col]]))
  }
}

# Columns eligible to be a categorical target / categorical predictor:
# character/factor ONLY. Numeric columns are never auto-classified as
# categorical: coarse-resolution covariates (e.g. climate-raster precipitation
# metrics) legitimately carry very few distinct values, so any cardinality
# heuristic mislabels them, and treating an ordered quantity as nominal changes
# both the recipe (dummy encoding) and the grid transfer (nearest-neighbour
# instead of kriging). Numeric class codes are handled by the "Bin a continuous
# variable" target mode instead.
.classif_is_categorical <- function(x) {
  is.character(x) || is.factor(x)
}

# Constant header height for the four result cards: the titles differ in length
# and two of them carry an info icon, so a reserved header keeps all four plot
# frames on the same baseline even when one title wraps in a narrow window.
CLASSIF_CARD_HEAD_H <- "34px"

classif_ui <- function(id) {
  ns <- shiny::NS(id)

  shiny::tagList(
    shiny::div(style = "padding: 10px;",
      shiny::fluidRow(
        shiny::column(3,
          shiny::div(style = "display: flex; align-items: center; margin-bottom: 10px;",
            shiny::h4("Classification Setup", style = "margin: 0; margin-right: 8px;"),
            shiny::tags$i(class = "fa fa-info-circle", style = "color: #007bff; cursor: help;",
              title = "Supervised classification of soil / land classes from co-sampled covariates. A minimum of ~50 samples with several members per class is recommended.")
          ),

          # Default to binning: .classif_is_categorical accepts character /
          # factor columns only (the cardinality heuristic was removed after it
          # mislabelled coarse-resolution climate covariates), so a typical soil
          # dataset offers no categorical column at all and "cat" would open the
          # panel on nothing but "switch to binning".
          shiny::radioButtons(ns("target_mode"), "Target",
            choices = c("Categorical column" = "cat", "Bin a continuous variable" = "bin"),
            selected = "bin"),

          shiny::conditionalPanel(
            condition = sprintf("input['%s'] == 'cat'", ns("target_mode")),
            shiny::uiOutput(ns("target_cat_ui"))
          ),
          shiny::conditionalPanel(
            condition = sprintf("input['%s'] == 'bin'", ns("target_mode")),
            shiny::uiOutput(ns("target_num_ui")),
            shiny::sliderInput(ns("n_classes"), "Number of classes", min = 2, max = 6,
                               value = 4, step = 1, ticks = FALSE),
            shiny::radioButtons(ns("bin_style"), "Break style",
              choices = c("Quantile" = "quantile", "Equal interval" = "equal",
                          "Natural (Jenks)" = "jenks"),
              selected = "quantile", inline = TRUE)
          ),

          shiny::uiOutput(ns("predictors_ui")),
          # Live multicollinearity guardrail: recomputed on the SCOPED data, so
          # the warning reflects the localities/polygons actually being fitted.
          shiny::uiOutput(ns("vif_note")),

          shiny::checkboxInput(ns("class_weights"),
            shiny::tags$span("Balance classes (inverse-frequency weights)",
              shiny::tags$i(class = "fa fa-info-circle",
                title = "Weights each training sample by n / (k x class count) so rare classes contribute equally to the fit and are not drowned out by dominant ones. Reported metrics stay unweighted. Not supported by Multinomial Logistic Regression (fits unweighted).",
                style = "color: #007bff; cursor: help; margin-left: 5px;")),
            value = FALSE),
          shiny::uiOutput(ns("weights_note")),

          shiny::div(style = "margin: 4px 0 10px 0; padding: 8px 10px; border: 1px solid rgba(128,128,128,0.35); border-radius: 6px;",
            shiny::div(style = "display: flex; align-items: center; margin-bottom: 6px;",
              shiny::tags$b("Spatial Scope"),
              shiny::tags$i(class = "fa fa-info-circle",
                title = "Restrict where the classifier is trained, evaluated, and mapped. These settings are specific to the Classification Suite and independent of the interpolation sidebar. Polygons drawn on the map or uploaded as a shapefile can restrict the scope further.",
                style = "color: #007bff; cursor: help; margin-left: 6px;")
            ),
            shiny::uiOutput(ns("scope_loc_ui")),
            shiny::uiOutput(ns("scope_poly_ui")),
            # Boundary / buffer / grid resolution for the classification maps.
            # Module-local on purpose: the sidebar Spatial Engine settings
            # belong to the interpolation runs and are hidden on this tab.
            shiny::selectInput(ns("boundary_type"),
              shiny::tags$span("Boundary Type",
                shiny::tags$i(class = "fa fa-info-circle",
                  title = "How the predicted class surface is cropped around the scoped samples. Concave/Convex hull wrap the points; Wrapped adds a buffer around the hull; Strict buffers each point individually.",
                  style = "color: #007bff; cursor: help; margin-left: 5px;")),
              choices = c("Concave Hull" = "concave", "Convex Hull" = "convex",
                          "Wrapped (Buffered)" = "wrapped",
                          "Strict Measured (Point Buffer)" = "strict"),
              selected = "concave"),
            shiny::conditionalPanel(
              condition = sprintf("input['%s'] == 'wrapped'", ns("boundary_type")),
              shiny::radioButtons(ns("buff_mode"), "Buffer Logic",
                choices = c("Auto (Dynamic)" = "dynamic", "Fixed (Manual)" = "fixed"),
                selected = "dynamic", inline = TRUE)
            ),
            shiny::conditionalPanel(
              condition = sprintf("input['%s'] == 'strict' || (input['%s'] == 'wrapped' && input['%s'] == 'fixed')",
                                  ns("boundary_type"), ns("boundary_type"), ns("buff_mode")),
              shiny::numericInput(ns("buff_dist"), "Buffer Distance (m)", value = 250, min = 0)
            ),
            shiny::radioButtons(ns("res_mode"),
              shiny::tags$span("Grid Resolution",
                shiny::tags$i(class = "fa fa-info-circle",
                  title = "Auto derives a cell size from the scope boundary area (~50k cells). Fixed forces a specific cell size in metres.",
                  style = "color: #007bff; cursor: help; margin-left: 5px;")),
              choices = c("Auto" = "auto", "Fixed" = "fixed"),
              selected = "auto", inline = TRUE),
            shiny::conditionalPanel(
              condition = sprintf("input['%s'] == 'fixed'", ns("res_mode")),
              shiny::sliderInput(ns("grid_res"), "Manual Resolution (m)",
                                 min = 5, max = 500, value = 50)
            ),
            shiny::uiOutput(ns("scope_note"))
          ),

          shiny::selectInput(ns("method"), "Method",
                             choices = stats::setNames(names(classif_methods()), classif_methods()),
                             selected = "rf"),
          shiny::radioButtons(ns("cv_strategy"), shiny::tags$span("Cross-validation",
              shiny::tags$i(class = "fa fa-info-circle",
                title = "Spatial CV clusters nearby points into folds so accuracy reflects prediction into unsampled areas; random k-fold usually reports optimistic accuracy under spatial autocorrelation.",
                style = "color: #007bff; cursor: help; margin-left: 5px;")),
            choices = c("Spatial (blocked)" = "spatial", "Standard (random k-fold)" = "standard"),
            selected = "spatial"),
          shiny::selectInput(ns("tuning_depth"), shiny::tags$span("Hyperparameter tuning",
              shiny::tags$i(class = "fa fa-info-circle",
                title = "None fits sensible fixed defaults (fast, deterministic). Light/Full search a space-filling grid over the folds.",
                style = "color: #007bff; cursor: help; margin-left: 5px;")),
            choices = stats::setNames(names(classif_tuning_depths()), classif_tuning_depths()),
            selected = "none"),
          # Nested CV is only meaningful when something is tuned, so the
          # checkbox stays hidden at depth "none".
          shiny::conditionalPanel(
            condition = sprintf("input['%s'] != 'none'", ns("tuning_depth")),
            shiny::checkboxInput(ns("nested_cv"),
              shiny::tags$span("Use nested CV (slower)",
                shiny::tags$i(class = "fa fa-info-circle",
                  title = "Re-runs the hyperparameter search inside every cross-validation fold (5 inner folds built from that fold's training rows only), so the reported performance honestly includes the tuning step. Without it, the same folds both choose and score the hyperparameters, which is mildly optimistic. Expect roughly 5x the tuning runtime.",
                  style = "color: #007bff; cursor: help; margin-left: 5px;")),
              value = FALSE)
          ),

          shiny::radioButtons(ns("importance_mode"),
            shiny::tags$span("Feature importance scored on",
              shiny::tags$i(class = "fa fa-info-circle",
                title = "Out-of-fold reuses each cross-validation fold's own model and permutes predictors in the rows that fold never saw, so importance is measured under the same honest design as the reported accuracy. Training rows is the conventional default (vip), but it rewards a flexible model for memorising its training data and so overstates importance. Both cost the same.",
                style = "color: #007bff; cursor: help; margin-left: 5px;")),
            choices = c("Out-of-fold (recommended)" = "oof", "Training rows" = "training"),
            selected = "oof"),

          shiny::checkboxInput(ns("make_surface"), "Predict maps", value = TRUE),

          shiny::actionButton(ns("run_btn"), "Run Classification", class = "btn-primary btn-block"),
          shiny::hr(),
          shiny::uiOutput(ns("run_summary"))
        ),

        shiny::column(9,
          shinyjs::hidden(
            shiny::div(id = ns("running_panel"),
              style = "text-align: center; padding: 100px 50px; background-color: rgba(255,255,255,0.02); border-radius: 8px; border: 2px dashed #007bff; margin-bottom: 20px;",
              shiny::icon("circle-notch", class = "fa-spin fa-4x", style = "color: #007bff; margin-bottom: 20px;"),
              shiny::h3("Running classification...", style = "color: #007bff; font-weight: bold;"),
              shiny::p("Fitting the model, running spatial cross-validation, and predicting class / probability / entropy surfaces in the background.", style = "color: #666;"),
              shiny::div(style = "max-width: 420px; margin: 15px auto 0 auto; background-color: rgba(0,123,255,0.15); border-radius: 6px; height: 14px; overflow: hidden;",
                shiny::div(id = ns("classif_progress_fill"),
                           style = "width: 0%; height: 100%; background-color: #007bff; border-radius: 6px; transition: width 0.6s ease;")
              ),
              shiny::div(id = ns("classif_progress_text"),
                         style = "margin-top: 6px; font-size: 0.9em; color: #666;", "Starting..."),
              shiny::actionButton(ns("cancel_btn"), "Cancel Run",
                                  icon = shiny::icon("stop"),
                                  class = "btn-danger btn-sm",
                                  style = "margin-top: 15px;")
            )
          ),
          shiny::conditionalPanel(
            condition = sprintf("output['%s'] == 'no'", ns("ready")),
            shiny::div(style = "text-align: center; padding: 120px 50px; color: #888;",
              shiny::icon("layer-group", class = "fa-4x", style = "margin-bottom: 20px; color: #ccc;"),
              shiny::h3("Awaiting Classification", style = "font-weight: 300;"),
              shiny::p("Choose a target, predictors, and a method on the left, then click Run Classification.")
            )
          ),
          shiny::conditionalPanel(
            condition = sprintf("output['%s'] == 'yes'", ns("ready")),
            # Top UI: CV Badge and Map display options
            shiny::div(style = "display: flex; justify-content: space-between; align-items: center; flex-wrap: wrap; margin-bottom: 5px;",
              shiny::uiOutput(ns("cv_badge")),
              shiny::conditionalPanel(
                condition = sprintf("output['%s'] == 'yes'", ns("has_surface")),
                shiny::div(style = "display: flex; gap: 0px; align-items: center;",
                  shiny::div(style = "margin-right: 15px;",
                    shiny::checkboxInput(ns("map_adorn"),
                      shiny::tags$span("Scale bar & north arrow",
                        shiny::tags$i(class = "fa fa-info-circle",
                          title = "Adds a scale bar (bottom right) and a north arrow (top left) to all three maps, including the expanded views and the styled PNG export.",
                          style = "color: #007bff; cursor: help; margin-left: 5px;")),
                      value = FALSE)
                  ),
                  shiny::checkboxInput(ns("map_points"),
                    shiny::tags$span("Show sample points",
                      shiny::tags$i(class = "fa fa-info-circle",
                        title = "Overlays the scoped training samples (white circles) so you can judge where predictions are supported by data and where they extrapolate.",
                        style = "color: #007bff; cursor: help; margin-left: 5px;")),
                    value = FALSE)
                )
              )
            ),
            
            # Second Row: Confidence threshold and Prob class dropdown
            shiny::conditionalPanel(
              condition = sprintf("output['%s'] == 'yes'", ns("has_surface")),
              shiny::fluidRow(
                shiny::column(6,
                  shiny::sliderInput(ns("conf_thresh"),
                    shiny::tags$span("Confidence threshold (abstain below)",
                      shiny::tags$i(class = "fa fa-info-circle",
                        title = "Cells whose highest class probability falls below this value are left 'Unclassified' (grey) instead of receiving a weak best guess - candidate spots for additional field sampling. 0 disables abstention. Applies instantly to the class map, area table, and class GeoTIFF; no re-run needed. Values at or below 1/(number of classes) can never trigger.",
                        style = "color: #007bff; cursor: help; margin-left: 5px;")),
                    min = 0, max = 0.95, value = 0, step = 0.05, ticks = FALSE),
                  shiny::uiOutput(ns("abstain_note"))
                ),
                shiny::column(6,
                  shiny::uiOutput(ns("prob_class_ui"))
                )
              ),
              shiny::tags$small(style = "color:#888; display:block; margin: 2px 0 15px 0;",
                shiny::icon("search-plus"),
                " Click any map (or its expand icon) for a full-size, high-resolution view.")
            ),

            # 2x2 result grid. All four panels use the SAME card (shared with
            # the Scientific Analysis tab): equal frames, and the expand
            # control in a header row instead of floating over the figure.
            # Ids are pre-namespaced here, and the three map buttons keep their
            # historical names so their observers are untouched.
            # conditionalPanel wraps the whole card, never its contents, so a
            # surface-less run leaves no orphan header behind.
            shiny::fluidRow(
              shiny::column(6,
                shiny::conditionalPanel(
                  condition = sprintf("output['%s'] == 'yes'", ns("has_surface")),
                  sci_plot_card(ns("class_map"), "Predicted Class Map",
                                expand_id = ns("class_expand_btn"), download = FALSE,
                                click_id = ns("class_map_click"),
                                head_min_height = CLASSIF_CARD_HEAD_H)
                )
              ),
              shiny::column(6,
                shiny::conditionalPanel(
                  condition = sprintf("output['%s'] == 'yes'", ns("has_surface")),
                  sci_plot_card(ns("entropy_map"), "Prediction Uncertainty (Entropy)",
                                expand_id = ns("entropy_expand_btn"), download = FALSE,
                                click_id = ns("entropy_map_click"),
                                info = shiny::tags$i(class = "fa fa-info-circle",
                                  title = "Normalised Shannon entropy of the class probabilities: 0 = confident single class, 1 = uniform over classes.",
                                  style = "color: #007bff; cursor: help; margin-left: 5px;"),
                                head_min_height = CLASSIF_CARD_HEAD_H)
                )
              )
            ),

            shiny::fluidRow(
              shiny::column(6,
                shiny::conditionalPanel(
                  condition = sprintf("output['%s'] == 'yes'", ns("has_surface")),
                  sci_plot_card(ns("prob_map"), "Class Probability Map",
                                expand_id = ns("prob_expand_btn"), download = FALSE,
                                click_id = ns("prob_map_click"),
                                head_min_height = CLASSIF_CARD_HEAD_H)
                )
              ),
              shiny::column(6,
                sci_plot_card(ns("importance_plot"), "Feature Importance",
                              expand_id = ns("importance_expand_btn"), download = FALSE,
                              info = shiny::tags$i(class = "fa fa-info-circle",
                                title = "Permutation importance: how much the multiclass log-loss worsens when one covariate is randomly shuffled (mean of 5 shuffles). Scored either out-of-fold or on the final model's training rows, following the 'Feature importance scored on' setting - the axis label states which. Shares renormalise the positive importances to 100%. Correlated covariates split their importance between them, so read this alongside the collinearity note.",
                                style = "color: #007bff; cursor: help; margin-left: 5px;"),
                              head_min_height = CLASSIF_CARD_HEAD_H)
              )
            ),
            shiny::hr(),
            # Result tables: one visible at a time, chosen from the dropdown
            # (metrics first). Availability adapts to the run (per-area rows,
            # surface-dependent area accounting).
            shiny::uiOutput(ns("table_select_ui")),
            shiny::conditionalPanel(
              condition = sprintf("input['%s'] == 'metrics'", ns("table_selection")),
              shiny::h4("Model Performance"),
              DT::dataTableOutput(ns("metrics_table"))
            ),
            shiny::conditionalPanel(
              condition = sprintf("input['%s'] == 'confmat'", ns("table_selection")),
              shiny::h4("Confusion Matrix"),
              DT::dataTableOutput(ns("confmat_table"))
            ),
            shiny::conditionalPanel(
              condition = sprintf("input['%s'] == 'perclass'", ns("table_selection")),
              shiny::h4("Per-class Accuracy"),
              DT::dataTableOutput(ns("perclass_table"))
            ),
            shiny::conditionalPanel(
              condition = sprintf("input['%s'] == 'lift'", ns("table_selection")),
              shiny::h4(shiny::tags$span("Covariate Lift",
                shiny::tags$i(class = "fa fa-info-circle",
                  title = "Benchmarks the covariate model against two no-covariate baselines on the SAME cross-validation folds: always predicting the most common class (no-information rate), and a spatial-only nearest-neighbour classifier. The McNemar test checks whether the paired accuracy improvement over the spatial baseline is statistically significant.",
                  style = "color: #007bff; cursor: help; margin-left: 5px;"))),
              shiny::uiOutput(ns("lift_ui"))
            ),
            shiny::conditionalPanel(
              condition = sprintf("input['%s'] == 'groups'", ns("table_selection")),
              shiny::h4(shiny::tags$span("Performance by Area",
                shiny::tags$i(class = "fa fa-info-circle",
                  title = "Pooled out-of-fold predictions split by locality (or by polygon in polygons-only scope). NA appears where a metric is undefined for that area, e.g. a class that never occurs there. Small areas carry wide uncertainty.",
                  style = "color: #007bff; cursor: help; margin-left: 5px;"))),
              DT::dataTableOutput(ns("group_metrics_table"))
            ),
            shiny::conditionalPanel(
              condition = sprintf("input['%s'] == 'area'", ns("table_selection")),
              shiny::h4(shiny::tags$span("Class Area Coverage",
                shiny::tags$i(class = "fa fa-info-circle",
                  title = "Exact cell counts x cell area per predicted class, in hectares. Honours the live confidence threshold (abstained cells appear as 'Unclassified').",
                  style = "color: #007bff; cursor: help; margin-left: 5px;"))),
              DT::dataTableOutput(ns("area_table"))
            ),
            shiny::hr(),
            shiny::h4("Export"),
            shiny::div(
              shiny::conditionalPanel(
                condition = sprintf("output['%s'] == 'yes'", ns("has_surface")),
                style = "display: inline;",
                shiny::downloadButton(ns("dl_class"), "Class GeoTIFF", class = "btn-sm"),
                shiny::downloadButton(ns("dl_prob"), "Probabilities GeoTIFF", class = "btn-sm"),
                shiny::downloadButton(ns("dl_entropy"), "Entropy GeoTIFF", class = "btn-sm"),
                shiny::downloadButton(ns("dl_png"), "Styled Maps (PNG)", class = "btn-sm")
              ),
              shiny::downloadButton(ns("dl_report"), "Metrics CSV", class = "btn-sm"),
              shiny::conditionalPanel(
                condition = sprintf("output['%s'] == 'yes'", ns("has_model")),
                style = "display: inline;",
                shiny::downloadButton(ns("dl_model"),
                  shiny::tags$span("Download Model (.rds)",
                    shiny::tags$i(class = "fa fa-info-circle",
                      title = "Saves the trained tidymodels workflow plus its metadata. Reuse it in R without retraining: b <- readRDS(file); predict(b$workflow, new_data, type = 'prob'). new_data must contain the covariate columns listed in b$predictors. Requires the same package versions as this app (recorded in b$versions).",
                      style = "cursor: help; margin-left: 5px;")),
                  class = "btn-sm")
              )
            )
          )
        )
      )
    )
  )
}

classif_server <- function(id, data_reactive, vars_metadata_reactive, spatial_reactive,
                           polygons_reactive = shiny::reactive(NULL)) {
  shiny::moduleServer(id, function(input, output, session) {
    ns <- session$ns
    cl_rv <- shiny::reactiveValues(res = NULL, ready = "no", vif_decision = NULL,
                                   modal_map = NULL, cancelling = FALSE)

    # ── Run progress + cancel (file-based, parity with the interpolation
    # pipeline: the worker writes percent files via update_progress_file and
    # polls a cancel flag between folds/stages) ──────────────────────────────
    cls_session_id <- substr(session$token, 1, 8)
    cls_progress_dir <- file.path(tempdir(), paste0("classif_progress_", cls_session_id))
    cls_cancel_file <- file.path(cls_progress_dir, "classif_cancel.txt")
    cls_progress_file <- file.path(
      cls_progress_dir, paste0("progress_", cls_session_id, "_classification_cls.txt"))
    # Companion file naming the stage the worker is in, so the caption tracks
    # the real work (grid build, covariate interpolation, classifying cells)
    # instead of always claiming cross-validation.
    cls_stage_file <- file.path(
      cls_progress_dir, paste0("stage_", cls_session_id, "_classification_cls.txt"))

    # Shown while the (cold) future worker is attaching the modelling stack and
    # sourcing helpers, before the pipeline writes its first progress/stage
    # file. On the first run of a session this window is ~30s (tidymodels + sf
    # stack loading in a fresh R process); saying so stops it reading as a hang.
    cls_cold_start_msg <- "Starting (loading modelling libraries, first run ~30s)..."

    set_classif_progress <- function(pct, txt) {
      shinyjs::runjs(sprintf(
        "var el = document.getElementById('%s'); if (el) el.style.width = '%d%%';",
        ns("classif_progress_fill"), as.integer(pct)))
      shinyjs::html("classif_progress_text", txt)
    }

    # Restore the idle UI. Called from BOTH promise handlers so the Run button
    # can never stay stuck on "Running..." once the worker has settled.
    reset_run_ui <- function() {
      shinyjs::enable("run_btn")
      shiny::updateActionButton(session, "run_btn", label = "Run Classification",
                                icon = character(0))
      shinyjs::hide("running_panel")
      shinyjs::enable("cancel_btn")
      cl_rv$cancelling <- FALSE
    }

    shiny::observe({
      if (!identical(cl_rv$ready, "running")) return(invisible(NULL))
      shiny::invalidateLater(800, session)
      # While a cancel is pending, leave the notice alone: the old poller
      # overwrote it on the next tick, which is why "Cancelling..." flipped
      # straight back to a progress caption.
      if (isTRUE(cl_rv$cancelling)) return(invisible(NULL))
      pct <- tryCatch({
        v <- suppressWarnings(as.numeric(readLines(cls_progress_file, warn = FALSE)))
        if (length(v) == 0 || is.na(v[1])) 0 else v[1]
      }, error = function(e) 0)
      pct <- max(0, min(99, round(pct)))
      # file.exists first: the poller starts before the worker has written its
      # first stage line, and readLines() on a missing path emits a "cannot open
      # file" WARNING to the console before the error the tryCatch would catch.
      stage <- tryCatch({
        if (!file.exists(cls_stage_file)) NULL else {
          s <- suppressWarnings(readLines(cls_stage_file, warn = FALSE))
          if (length(s) == 0 || !nzchar(s[1])) NULL else s[1]
        }
      }, error = function(e) NULL)
      if (is.null(stage) && pct == 0) {
        # Pre-pipeline window: worker still loading packages / sourcing helpers,
        # no stage written yet. Keep the informative caption instead of "0%",
        # which otherwise reads like a hang on the first (cold) run.
        set_classif_progress(0, cls_cold_start_msg)
      } else {
        set_classif_progress(pct, sprintf("%s  %d%%", stage %||% "Working...", pct))
      }
    })

    shiny::observeEvent(input$cancel_btn, {
      if (!identical(cl_rv$ready, "running")) return()
      if (!dir.exists(cls_progress_dir)) dir.create(cls_progress_dir, recursive = TRUE, showWarnings = FALSE)
      tryCatch(file.create(cls_cancel_file), error = function(e) NULL)
      cl_rv$cancelling <- TRUE
      shinyjs::disable("cancel_btn")
      shinyjs::html("classif_progress_text",
                    "Cancelling, waiting for the current stage to reach a checkpoint...")
      shiny::showNotification("Cancelling classification run...", type = "message")
    })

    # Module-local boundary type, buffer logic, and grid resolution
    # (independent of the interpolation sidebar, which is hidden on this
    # tab), normalised with defaults. One source of truth: the live scope
    # preview and the run dispatch both read this reactive.
    bset <- shiny::reactive({
      list(
        type      = input$boundary_type %||% "concave",
        buff_mode = input$buff_mode %||% "dynamic",
        buff_dist = if (is.null(input$buff_dist) || is.na(input$buff_dist)) 250 else as.numeric(input$buff_dist),
        res_mode  = input$res_mode %||% "auto",
        res       = if (is.null(input$grid_res) || is.na(input$grid_res)) NULL else as.numeric(input$grid_res)
      )
    })

    # ── Spatial scope ────────────────────────────────────────────────────────
    # Localities available for scoping (NULL when no locality column is mapped).
    scope_choices <- shiny::reactive({
      df <- data_reactive(); sp <- spatial_reactive()
      if (is.null(df) || is.null(sp$loc) || !sp$loc %in% names(df)) return(NULL)
      sort(unique(as.character(stats::na.omit(df[[sp$loc]]))))
    })

    # Locality picker, specific to the Classification Suite (deliberately NOT
    # synced with the sidebar Context panel: the two selections used to drift
    # apart mid-session, leaving it ambiguous which one a run would honour).
    # Defaults to all localities; a previous selection survives re-renders.
    output$scope_loc_ui <- shiny::renderUI({
      ch <- scope_choices()
      if (is.null(ch)) return(NULL)
      prev <- shiny::isolate(input$scope_loc)
      sel <- if (is.null(prev) || length(prev) == 0) ch else intersect(prev, ch)
      if (length(sel) == 0) sel <- ch
      shinyWidgets::pickerInput(ns("scope_loc"),
        shiny::tags$span("Localities",
          shiny::tags$i(class = "fa fa-info-circle",
            title = "Localities this classification run is trained, evaluated, and mapped on. Independent of the sidebar Context panel. Empty = all localities.",
            style = "color: #007bff; cursor: help; margin-left: 5px;")),
        choices = ch, selected = sel, multiple = TRUE,
        options = list(`actions-box` = TRUE))
    })

    # Polygon scope: only offered once at least one polygon exists (drawn on
    # the map or uploaded as a shapefile).
    output$scope_poly_ui <- shiny::renderUI({
      pl <- polygons_reactive()
      n_drawn <- if (!is.null(pl$drawn)) nrow(pl$drawn) else 0L
      has_shp <- !is.null(pl$shp) && inherits(pl$shp, "sf") && nrow(pl$shp) > 0
      if (n_drawn == 0 && !has_shp) return(NULL)
      prev <- shiny::isolate(input$scope_poly)
      shiny::radioButtons(ns("scope_poly"),
        shiny::tags$span("Polygon scope",
          shiny::tags$i(class = "fa fa-info-circle",
            title = "Use polygons drawn on the map and/or the uploaded shapefile to bound the run. 'Within localities' keeps points inside BOTH the selected localities and the polygons; 'Polygons only' ignores the locality filter.",
            style = "color: #007bff; cursor: help; margin-left: 5px;")),
        choices = c("Ignore polygons" = "ignore",
                    "Within localities" = "intersect",
                    "Polygons only" = "only"),
        selected = if (is.null(prev)) "ignore" else prev)
    })

    # Resolve the active scope (fast: vector filter + point-in-polygon on the
    # sample points). Reused verbatim by the run dispatch so the preview count
    # and the actual run can never disagree.
    current_scope <- shiny::reactive({
      df <- data_reactive(); sp <- spatial_reactive()
      if (is.null(df) || is.null(sp$x) || is.null(sp$y) ||
          is.null(sp$src_crs) || is.null(sp$proj_crs)) return(NULL)
      if (!all(c(sp$x, sp$y) %in% names(df))) return(NULL)
      pl <- polygons_reactive()
      mode <- if (is.null(input$scope_poly)) "ignore" else input$scope_poly
      psf <- if (mode == "ignore") NULL else tryCatch(
        classif_scope_polygons(pl$drawn, pl$shp, target_crs = sp$proj_crs),
        error = function(e) NULL)
      bs <- bset()
      tryCatch(
        classif_resolve_scope(df, sp$x, sp$y, sp$src_crs, sp$proj_crs,
                              loc_col = sp$loc, localities = input$scope_loc,
                              poly_sf = psf, poly_mode = mode,
                              boundary_style = bs$type,
                              buffer_mode = bs$buff_mode,
                              buffer_dist = bs$buff_dist),
        error = function(e) NULL)
    })

    # ── Multicollinearity guardrail (scoped) ─────────────────────────────────
    # VIF + pairwise screening of the selected NUMERIC covariates, computed on
    # the points inside the CURRENT scope: correlations measured across all
    # localities can be spurious (or masked) within the subset actually being
    # fitted. Reuses detect_multicollinearity_engine, the same gate the RK/RFK
    # interpolation engines apply. The VIF threshold is method-aware: Random
    # Forest gets a stricter advisory screen (VIF > 5) because moderate
    # collinearity that barely hurts prediction still splits permutation
    # feature importance across the correlated covariates, making genuine
    # drivers look weak (Strobl et al. 2008); other learners keep the standard
    # VIF > 10 threshold. Advisory only: the user decides drop vs keep.
    active_vif_threshold <- shiny::reactive({
      if (identical(input$method, "rf")) 5 else 10
    })
    scoped_collinearity <- shiny::reactive({
      sc <- current_scope()
      preds <- input$predictors
      if (is.null(sc) || is.null(sc$df) || length(preds) < 2) return(NULL)
      num_preds <- preds[preds %in% names(sc$df)]
      num_preds <- num_preds[vapply(num_preds, function(p) is.numeric(sc$df[[p]]), logical(1))]
      if (length(num_preds) < 2) return(NULL)
      thr <- active_vif_threshold()
      chk <- tryCatch(
        suppressWarnings(detect_multicollinearity_engine(
          sc$df[, num_preds, drop = FALSE], vars = num_preds, vif_threshold = thr)),
        error = function(e) NULL)
      if (!is.null(chk)) chk$vif_threshold <- thr
      chk
    })

    output$vif_note <- shiny::renderUI({
      chk <- scoped_collinearity()
      if (is.null(chk) || length(chk$dropped) == 0) return(NULL)
      labs_of <- function(v) paste(vapply(v, function(x) get_var_label(x, vars_metadata_reactive()),
                                          character(1)), collapse = ", ")
      thr <- chk$vif_threshold %||% 10
      # Zero-variance covariates share the engine's `dropped` vector with the
      # collinear ones but are a different problem: they are dropped whatever
      # the user chooses, and calling them collinear sends the user looking for
      # a correlation that does not exist.
      vif_vars <- chk$dropped_vif %||% chk$dropped
      const_vars <- chk$dropped_constant %||% character(0)
      reason <- if (thr < 10) {
        " While often acceptable mathematically, these correlated covariates will split the Random Forest permutation importance between them, making the true drivers look weak - consider dropping them to see the real ranking."
      } else {
        " Redundant covariates dilute feature importance and destabilise multinomial fits."
      }
      shiny::tags$small(style = "color:#e0a800; display:block; margin: -4px 0 8px 0;",
        shiny::icon("exclamation-triangle"),
        if (length(vif_vars)) {
          sprintf(" Multicollinearity within the current scope (VIF > %s%s): %s.%s You will be asked to drop or keep them at run time.",
                  thr, if (thr < 10) ", stricter screen for Random Forest" else "",
                  labs_of(vif_vars), reason)
        },
        if (length(const_vars)) {
          sprintf(" Constant within the current scope (no variance, so no information to learn from): %s. These are dropped by the model recipe regardless of the choice above.",
                  labs_of(const_vars))
        })
    })

    output$weights_note <- shiny::renderUI({
      if (!isTRUE(input$class_weights) || !identical(input$method, "multinom")) return(NULL)
      shiny::tags$small(style = "color:#e0a800; display:block; margin: -4px 0 8px 0;",
        "Multinomial logistic (nnet) does not accept case weights - this run will fit unweighted.")
    })

    output$scope_note <- shiny::renderUI({
      sc <- current_scope()
      if (is.null(sc)) return(NULL)
      col <- if (sc$n_scoped < 20) "#dc3545" else "#888"
      shiny::tagList(
        shiny::tags$small(style = paste0("color:", col, ";"),
          sprintf("In scope: %d of %d georeferenced points.", sc$n_scoped, sc$n_input)),
        strict_note()
      )
    })

    # Live advisory: a Strict Measured buffer narrower than half the grid cell
    # diagonal discards the cells of isolated samples, so sampled points end up
    # over blank map (see strict_buffer_gap, spatial_pipeline.R). The scope
    # carries the boundary area and bbox, so the Auto resolution the run will
    # actually pick is reproduced exactly via classif_auto_res rather than
    # guessed. The run itself re-checks and reports through res$grid_warning.
    # Is the domain actually a union of per-point buffers? Polygons-only
    # scoping replaces the hull entirely with the user's polygons, so the
    # Boundary Type control (and this advisory) is inert there.
    strict_scope_active <- shiny::reactive({
      identical(bset()$type, "strict") &&
        !identical(input$scope_poly %||% "ignore", "only")
    })

    strict_note <- shiny::reactive({
      bs <- bset()
      if (!strict_scope_active()) return(NULL)
      sc <- current_scope()
      if (is.null(sc)) return(NULL)
      res_eff <- if (identical(bs$res_mode, "fixed")) {
        bs$res
      } else if (!is.null(sc$boundary_area_m2) && !is.null(sc$boundary_bbox)) {
        classif_auto_res(sc$boundary_area_m2, sc$boundary_bbox)
      } else NULL
      if (is.null(res_eff)) return(NULL)
      msg <- strict_buffer_message(bs$buff_dist, res_eff)
      if (is.null(msg)) return(NULL)
      shiny::tags$p(style = paste("font-size: 0.78em; margin-top: 8px;",
                                  "border-left: 3px solid #f59e0b; padding-left: 8px;",
                                  "color: #b45309; line-height: 1.35;"),
                    msg)
    })

    # ── Column choice helpers ────────────────────────────────────────────────
    output$target_cat_ui <- shiny::renderUI({
      df <- data_reactive(); shiny::req(df)
      sp <- spatial_reactive()
      excl <- c(sp$x, sp$y)
      cat_cols <- setdiff(names(df)[vapply(df, .classif_is_categorical, logical(1))], excl)
      shiny::validate(shiny::need(length(cat_cols) > 0,
        "No categorical column found. Switch to 'Bin a continuous variable'."))
      labs <- vapply(cat_cols, function(v) get_var_label(v, vars_metadata_reactive()), character(1))
      shiny::selectInput(ns("target_cat"), "Categorical target",
                         choices = stats::setNames(cat_cols, labs))
    })

    output$target_num_ui <- shiny::renderUI({
      df <- data_reactive(); shiny::req(df)
      sp <- spatial_reactive()
      num_cols <- setdiff(names(df)[vapply(df, is.numeric, logical(1))], c(sp$x, sp$y))
      labs <- vapply(num_cols, function(v) get_var_label(v, vars_metadata_reactive()), character(1))
      shiny::selectInput(ns("target_num"), "Continuous variable to bin",
                         choices = stats::setNames(num_cols, labs))
    })

    output$predictors_ui <- shiny::renderUI({
      df <- data_reactive(); shiny::req(df)
      sp <- spatial_reactive()
      target_col <- if (identical(input$target_mode, "bin")) input$target_num else input$target_cat
      excl <- c(sp$x, sp$y, target_col)
      cols <- setdiff(names(df), excl)
      # Eligible predictors: numeric (always treated as continuous) or
      # text/factor (dummy-encoded categoricals).
      elig <- cols[vapply(cols, function(c) is.numeric(df[[c]]) || .classif_is_categorical(df[[c]]), logical(1))]
      labs <- vapply(elig, function(v) get_var_label(v, vars_metadata_reactive()), character(1))
      shinyWidgets::pickerInput(ns("predictors"), "Covariates",
        choices = stats::setNames(elig, labs), multiple = TRUE,
        options = list(`actions-box` = TRUE, `live-search` = TRUE))
    })

    # ── Run ──────────────────────────────────────────────────────────────────
    # The run is dispatched through try_run() so the multicollinearity modal's
    # Drop/Keep buttons can re-enter the same path after recording a decision.
    # The decision is remembered until the covariates or the scope change.
    try_run <- function() {
      if (identical(cl_rv$ready, "running")) return(NULL)
      df <- data_reactive(); sp <- spatial_reactive()
      shiny::req(df, sp$x, sp$y, sp$src_crs, sp$proj_crs, input$predictors)

      target_src <- if (identical(input$target_mode, "bin")) input$target_num else input$target_cat
      preds <- setdiff(input$predictors, c(target_src, sp$x, sp$y))
      if (length(preds) < 1) {
        shiny::showNotification("Select at least one covariate.", type = "error"); return()
      }

      chk <- scoped_collinearity()
      flagged <- if (is.null(chk)) character(0) else intersect(chk$dropped, preds)
      if (length(flagged) > 0 && is.null(cl_rv$vif_decision)) {
        labs <- vapply(flagged, function(v) get_var_label(v, vars_metadata_reactive()), character(1))
        thr <- chk$vif_threshold %||% 10
        shiny::showModal(shiny::modalDialog(
          title = shiny::tags$div(style = "color: #d9534f; font-weight: bold;",
            shiny::icon("exclamation-triangle"), "High Multicollinearity In Scope"),
          shiny::tags$p(sprintf("Within the current spatial scope, some covariates are highly collinear (iterative VIF > %s). Near-duplicate covariates add no information, dilute permutation importance across the duplicates, and can destabilise the multinomial logistic fit.", thr)),
          if (thr < 10) shiny::tags$p(
            "Random Forest note: predictions are barely affected by keeping them, but the permutation feature importance will be artificially split between the correlated covariates, so genuinely important variables can look weak. Dropping them yields a cleaner importance ranking."),
          shiny::tags$p(shiny::tags$b("Recommended to drop:"), paste(labs, collapse = ", ")),
          # Constants travel in the same `dropped` vector; name them for what
          # they are rather than presenting them as a collinearity problem.
          if (length(intersect(flagged, chk$dropped_constant %||% character(0))) > 0) {
            shiny::tags$p(shiny::tags$b("Constant in this scope (no variance): "),
              paste(vapply(intersect(flagged, chk$dropped_constant),
                           function(v) get_var_label(v, vars_metadata_reactive()), character(1)),
                    collapse = ", "),
              " - these carry no information and are removed by the model recipe either way.")
          },
          footer = shiny::tagList(
            shiny::actionButton(ns("clvif_drop"), "Auto-Drop and Continue", class = "btn-success"),
            shiny::actionButton(ns("clvif_keep"), "Keep All (Not Recommended)", class = "btn-warning"),
            shiny::modalButton("Cancel")
          ),
          easyClose = FALSE
        ))
        return()
      }
      dropped <- if (identical(cl_rv$vif_decision, "drop")) flagged else character(0)
      launch_run(setdiff(preds, dropped), dropped)
    }

    shiny::observeEvent(input$run_btn, try_run())
    shiny::observeEvent(input$clvif_drop, {
      shiny::removeModal(); cl_rv$vif_decision <- "drop"; try_run()
    })
    shiny::observeEvent(input$clvif_keep, {
      shiny::removeModal(); cl_rv$vif_decision <- "keep"; try_run()
    })
    # Method is part of the reset list because the VIF threshold is
    # method-aware (5 for RF, 10 otherwise): switching learners can change
    # which covariates are flagged, so a stale decision must not carry over.
    shiny::observeEvent(list(input$predictors, input$scope_loc, input$scope_poly, input$method), {
      cl_rv$vif_decision <- NULL
    }, ignoreInit = TRUE)

    launch_run <- function(preds, dropped = character(0)) {
      df <- data_reactive(); sp <- spatial_reactive()
      if (length(preds) < 1) {
        shiny::showNotification("No covariates left after the collinearity drop.", type = "error"); return()
      }

      # Resolve the spatial scope FIRST: the target (including quantile/Jenks
      # bin breaks) and all CV metrics are then defined on the scoped points
      # only, and the prediction boundary comes from the same resolution.
      sc <- current_scope()
      if (is.null(sc) || sc$n_scoped < 1) {
        shiny::showNotification("No data points fall inside the selected spatial scope.", type = "error"); return()
      }
      sdf <- sc$df

      tvec <- tryCatch(
        classif_build_target(sdf, input$target_mode, input$target_cat, input$target_num,
                             input$n_classes, input$bin_style),
        error = function(e) NULL)
      if (is.null(tvec) || nlevels(droplevels(as.factor(tvec))) < 2) {
        shiny::showNotification("Target must resolve to at least two non-empty classes within the selected scope.", type = "error"); return()
      }

      # Assemble the analysis frame: coordinates + covariates + fixed-name
      # target and scope-group columns, so the worker sees generic inputs.
      adf <- sdf[, unique(c(sp$x, sp$y, preds)), drop = FALSE]
      adf[[".class_target"]] <- tvec
      adf[[".scope_group"]] <- sc$group
      # Adequacy guardrail: warn (not block) on a thin scope, naming exactly
      # which classes fall below the 3-sample minimum so the user knows what
      # to merge, exclude, or re-bin.
      adequacy_msg <- classif_scope_adequacy(tvec, sum(stats::complete.cases(adf)))
      if (!is.null(adequacy_msg)) {
        shiny::showNotification(adequacy_msg, type = "warning", duration = 12)
      }

      # Human-readable scope description for the CV badge and run summary.
      scope_label_v <- {
        mode_v <- if (is.null(input$scope_poly)) "ignore" else input$scope_poly
        ch <- scope_choices()
        if (mode_v == "only") {
          "polygons only"
        } else {
          base <- if (is.null(ch)) {
            "all data"
          } else {
            sel <- if (length(input$scope_loc)) intersect(input$scope_loc, ch) else ch
            if (length(sel) == 0) sel <- ch
            sprintf("%d/%d localities", length(sel), length(ch))
          }
          if (mode_v == "intersect") paste0(base, " within polygons") else base
        }
      }
      boundary_wkt_v <- sc$boundary_wkt

      # Fresh progress/cancel state for this run before the worker starts.
      if (!dir.exists(cls_progress_dir)) dir.create(cls_progress_dir, recursive = TRUE, showWarnings = FALSE)
      for (f in c(cls_cancel_file, cls_progress_file, cls_stage_file)) {
        if (file.exists(f)) tryCatch(file.remove(f), error = function(e) NULL)
      }
      cl_rv$cancelling <- FALSE
      shinyjs::enable("cancel_btn")
      set_classif_progress(0, cls_cold_start_msg)

      shinyjs::disable("run_btn")
      shiny::updateActionButton(session, "run_btn", label = "Running...",
                                icon = shiny::icon("spinner", class = "fa-spin"))
      shinyjs::show("running_panel")
      cl_rv$ready <- "running"

      method_v <- input$method; strategy_v <- input$cv_strategy; depth_v <- input$tuning_depth
      # Nested CV only exists when something is tuned; the checkbox is hidden
      # at depth "none", so also guard against a stale value.
      nested_v <- isTRUE(input$nested_cv) && !identical(depth_v, "none")
      make_surf <- isTRUE(input$make_surface)
      # Projected training coordinates (small: n x 2) for the optional
      # sample-point overlay on the prediction maps.
      train_xy_v <- tryCatch({
        p_sf <- sf::st_as_sf(sdf[, c(sp$x, sp$y)], coords = c(sp$x, sp$y), crs = sp$src_crs)
        sf::st_coordinates(sf::st_transform(p_sf, sp$proj_crs))
      }, error = function(e) NULL)
      # Boundary type, buffer, and grid resolution come from the module's own
      # Spatial Scope controls: fixed resolution mode uses the manual slider
      # value; auto lets the pipeline derive a resolution from the boundary
      # area.
      bs_v <- bset()
      gres <- if (identical(bs_v$res_mode, "fixed")) bs_v$res else NULL
      strict_scope_v <- strict_scope_active()
      weights_v <- isTRUE(input$class_weights)
      imp_mode_v <- input$importance_mode %||% "oof"
      x_v <- sp$x; y_v <- sp$y; src_v <- sp$src_crs; proj_v <- sp$proj_crs
      proj_root_ship <- getwd()
      progress_dir_ship <- cls_progress_dir
      session_id_ship <- cls_session_id
      cancel_file_ship <- cls_cancel_file
      # The trained workflow is persisted by the worker (same machine) into a
      # main-session tempfile, keeping the promise payload lean; the Download
      # Model button copies this file.
      model_path_ship <- tempfile(pattern = "classif_model_", fileext = ".rds")

      # Lean worker payload: bundle every input into ONE plain-data list built
      # here in the main session, then ship only that. run_classification_pipeline
      # is resolved on the worker by the in-worker source() below, so `globals`
      # is pinned explicitly and future does NOT auto-detect it (auto-detection
      # would walk and serialise the whole classif_helpers call graph on the
      # main thread at dispatch, and needlessly ship a function the worker
      # re-defines anyway). Mirrors the interpolation pipeline's plain-data
      # hand-off (server_execution.R): no rv$/input$/reactives cross the boundary.
      run_args <- list(
        proj_root = proj_root_ship,
        pipeline = list(
          df = adf, target = ".class_target", predictors = preds,
          x_col = x_v, y_col = y_v, src_crs = src_v, proj_crs = proj_v,
          method = method_v, strategy = strategy_v, depth = depth_v,
          v = 10L, grid_res = gres, boundary = bs_v$type,
          buffer_mode = bs_v$buff_mode, buffer_dist = bs_v$buff_dist,
          strict_scope = strict_scope_v,
          make_surface = make_surf,
          group_col = ".scope_group", boundary_wkt = boundary_wkt_v,
          class_weights = weights_v, model_rds_path = model_path_ship,
          nested = nested_v, importance_mode = imp_mode_v,
          progress_dir = progress_dir_ship, session_id = session_id_ship,
          cancel_file = cancel_file_ship
        )
      )

      promises::future_promise({
        setwd(run_args$proj_root)
        suppressPackageStartupMessages({
          library(sf); library(terra); library(gstat); library(automap); library(fields)
          library(spdep); library(FNN); library(concaveman); library(dplyr); library(classInt)
          library(recipes); library(parsnip); library(workflows); library(tune)
          library(rsample); library(dials); library(yardstick); library(spatialsample); library(hardhat)
        })
        # Source helpers in-worker so the full internal closure is present,
        # matching the interpolation pipeline's worker convention (T12).
        source("spatial_helpers.R"); source("classif_helpers.R")
        do.call(run_classification_pipeline, run_args$pipeline)
      }, globals = list(run_args = run_args),
         # seed = TRUE gives the worker a proper parallel-safe L'Ecuyer stream.
         # Without it future warns "UNEXPECTED VALUE: ... generated random
         # numbers without specifying argument 'seed'" on every run, because a
         # fresh PSOCK process otherwise seeds itself from process entropy.
         # Reproducibility is unaffected either way: every RNG-consuming step in
         # the pipeline runs inside .classif_with_seed()'s two-sided sandbox,
         # which sets its own seed and restores the caller's state.
         seed = TRUE) %...>% (function(res) {
        reset_run_ui()
        # Unique run token: keys the per-run plot cache (renderCachedPlot).
        res$run_id <- paste0(substr(session$token, 1, 8), "-",
                             format(Sys.time(), "%Y%m%d%H%M%OS3"))
        res$scope_label <- scope_label_v
        res$dropped_covariates <- dropped
        res$train_xy <- train_xy_v
        cl_rv$res <- res
        cl_rv$ready <- "yes"
        if (length(dropped) > 0) {
          shiny::showNotification(
            sprintf("Classification completed. Dropped collinear covariates: %s.",
                    paste(dropped, collapse = ", ")), type = "message")
        } else {
          shiny::showNotification("Classification completed.", type = "message")
        }
        # Grid-geometry caveat raised by classif_build_grid: the strict boundary
        # was narrower than half a cell diagonal, so isolated samples have no
        # mapped cell. The maps are valid, the support is just under-resolved.
        if (!is.null(res$grid_warning)) {
          shiny::showNotification(res$grid_warning, type = "warning", duration = 15)
        }
        # CV-design caveat, not a failure: some fold's training rows held none of
        # these classes, so that fold could not predict them and their held-out
        # samples score as misses by construction. Worth naming — it depresses
        # the reported accuracy/kappa for exactly those classes.
        gaps <- res$class_gaps
        if (!is.null(gaps) && nrow(gaps) > 0) {
          gl <- sort(unique(as.character(gaps$class)))
          shiny::showNotification(
            sprintf(paste("Some folds contained no training samples of %d class%s (%s):",
                          "those folds could not predict %s, so its held-out samples count",
                          "as errors and the reported skill for it is pessimistic.",
                          "Consider fewer classes, quantile breaks, random k-fold CV,",
                          "or a wider spatial scope."),
                    length(gl), if (length(gl) == 1) "" else "es",
                    paste(gl, collapse = ", "),
                    if (length(gl) == 1) "it" else "them"),
            type = "warning", duration = 15)
        }
      }) %...!% (function(err) {
        reset_run_ui()
        cl_rv$ready <- "no"
        if (grepl("cancelled by user", conditionMessage(err), fixed = TRUE)) {
          shiny::showNotification("Classification run cancelled.", type = "message")
        } else {
          shiny::showNotification(paste("Classification failed:", err$message), type = "error")
        }
      })
      NULL
    }

    output$ready <- shiny::reactive({ cl_rv$ready })
    shiny::outputOptions(output, "ready", suspendWhenHidden = FALSE)
    output$has_surface <- shiny::reactive({
      if (!is.null(cl_rv$res) && !is.null(cl_rv$res$surface_df)) "yes" else "no"
    })
    shiny::outputOptions(output, "has_surface", suspendWhenHidden = FALSE)
    output$has_model <- shiny::reactive({
      p <- cl_rv$res$model_path
      if (!is.null(p) && file.exists(p)) "yes" else "no"
    })
    shiny::outputOptions(output, "has_model", suspendWhenHidden = FALSE)
    # Dropdown driving which results table is shown (metrics first). Choices
    # adapt to the run: "Performance by area" only when the scope yields real
    # groups (redundant when everything collapses to one "All data" group),
    # "Class area coverage" only when a surface was predicted. The previous
    # selection is kept across runs when still available.
    output$table_select_ui <- shiny::renderUI({
      res <- cl_rv$res; shiny::req(res)
      ch <- c("Model performance metrics" = "metrics",
              "Confusion matrix" = "confmat",
              "Per-class accuracy" = "perclass",
              "Covariate lift vs baselines" = "lift")
      gm <- res$group_metrics
      if (!is.null(gm) && any(!gm$scope %in% c("Total", "All data"))) {
        ch <- c(ch, "Performance by area" = "groups")
      }
      if (!is.null(res$surface_df)) ch <- c(ch, "Class area coverage" = "area")
      prev <- shiny::isolate(input$table_selection)
      shiny::selectInput(ns("table_selection"),
        shiny::tags$span("Results table",
          shiny::tags$i(class = "fa fa-info-circle",
            title = "Choose which results table to display below. The Metrics CSV export always contains the full metric set regardless of this selection.",
            style = "color: #007bff; cursor: help; margin-left: 5px;")),
        choices = ch,
        selected = if (!is.null(prev) && prev %in% ch) prev else "metrics",
        width = "320px")
    })

    # ── Rasters (built in main session; terra pointers can't cross workers) ───
    # Plain cached reactive: rasterisation runs once per completed run (and per
    # confidence-threshold change). Never write reactiveValues from inside this
    # reactive — the write self-invalidates it and sends every map through an
    # extra recalculating wave. The threshold is debounced so dragging the
    # slider does not re-rasterise at every intermediate value.
    conf_thresh_d <- shiny::debounce(
      shiny::reactive(input$conf_thresh %||% 0), 400)
    get_rasters <- shiny::reactive({
      res <- cl_rv$res
      shiny::req(res, res$surface_df)
      classif_surface_to_rasters(
        res$surface_df, res = res$res, crs_wkt = res$crs_wkt, levels_order = res$levels,
        conf_threshold = conf_thresh_d())
    })

    # Coverage/selective-accuracy readout for the abstention threshold, derived
    # from the pooled out-of-fold predictions: what fraction of validation
    # points would remain classified at this threshold, and how accurate the
    # retained subset is (selective risk; the retained accuracy should sit at
    # or above the overall one).
    output$abstain_note <- shiny::renderUI({
      res <- cl_rv$res; shiny::req(res, res$cv_predictions)
      tau <- conf_thresh_d()
      if (is.null(tau) || tau <= 0) return(NULL)
      pr <- res$cv_predictions
      prob_cols <- paste0(".pred_", res$levels)
      prob_cols <- prob_cols[prob_cols %in% names(pr)]
      if (length(prob_cols) < 2) return(NULL)
      max_p <- do.call(pmax, c(pr[prob_cols], na.rm = TRUE))
      keep <- !is.na(max_p) & max_p >= tau
      truth <- as.character(pr[[res$target_col]])
      overall <- mean(as.character(pr$.pred_class) == truth)
      msg <- if (!any(keep)) {
        sprintf("At %.2f every validation point falls below the threshold - the whole map would be Unclassified.", tau)
      } else {
        sel <- mean(as.character(pr$.pred_class[keep]) == truth[keep])
        sprintf("CV check at %.2f: %.0f%% of validation points retained; accuracy among retained %.3f vs %.3f overall. Grey cells mark candidate re-sampling locations.",
                tau, 100 * mean(keep), sel, overall)
      }
      shiny::tags$small(style = "color:#888; display:block; margin: -6px 0 6px 0;", msg)
    })

    # ── Performance outputs ──────────────────────────────────────────────────
    output$cv_badge <- shiny::renderUI({
      res <- cl_rv$res; shiny::req(res)
      lbl <- if (identical(res$strategy, "spatial")) "Spatial blocked CV" else "Random k-fold CV"
      scope_part <- if (is.null(res$scope_label)) "" else sprintf(" | scope: %s", res$scope_label)
      wt_part <- if (isTRUE(res$weights_applied)) {
        " | class-weighted"
      } else if (isTRUE(res$weights_requested)) {
        " | weights unsupported (unweighted)"
      } else ""
      depth_part <- if (isTRUE(res$nested)) paste0(res$depth, " (nested CV)") else res$depth
      shiny::tags$span(class = "badge",
        style = "background:#007bff; color:#fff; padding:4px 8px; border-radius:4px;",
        sprintf("%s | %d folds | n = %d | tuning: %s%s%s",
                lbl, res$n_folds, res$n, depth_part, scope_part, wt_part))
    })

    # ── Covariate lift vs no-covariate baselines ─────────────────────────────
    output$lift_ui <- shiny::renderUI({
      res <- cl_rv$res; shiny::req(res, res$lift)
      lf <- res$lift
      fmt <- function(x) ifelse(is.na(x), "-", sprintf("%.3f", x))
      p_lbl <- if (is.na(lf$mcnemar_p)) {
        "McNemar test: not defined (no discordant pairs)."
      } else if (lf$mcnemar_p < 0.001) {
        "McNemar p < 0.001: the paired improvement over the spatial baseline is statistically significant."
      } else {
        sprintf("McNemar p = %.3f%s", lf$mcnemar_p,
                if (lf$mcnemar_p < 0.05) ": the paired improvement over the spatial baseline is statistically significant."
                else ": the improvement over the spatial baseline is NOT statistically significant - the covariates may add little beyond spatial position.")
      }
      dir_word <- if (lf$lift_abs >= 0) "improved" else "REDUCED"
      shiny::tagList(
        shiny::tags$table(class = "table table-condensed", style = "margin-bottom: 6px;",
          shiny::tags$thead(shiny::tags$tr(
            shiny::tags$th("Model"), shiny::tags$th("Accuracy"), shiny::tags$th("Kappa"))),
          shiny::tags$tbody(
            shiny::tags$tr(shiny::tags$td("Covariate model"),
                           shiny::tags$td(fmt(lf$model_acc)), shiny::tags$td(fmt(lf$model_kap))),
            shiny::tags$tr(shiny::tags$td("Spatial baseline (1-NN, no covariates)"),
                           shiny::tags$td(fmt(lf$baseline_acc)), shiny::tags$td(fmt(lf$baseline_kap))),
            shiny::tags$tr(shiny::tags$td("Majority class (no information)",
                             shiny::tags$i(class = "fa fa-info-circle",
                               title = "The accuracy achieved by always predicting the most frequent class in the scoped data - the 'no-information rate'. It is the floor any useful model must clearly exceed; with imbalanced classes it can be high even though the strategy has learnt nothing. Cohen's kappa corrects for exactly this, which is why this row scores kappa = 0 by construction.",
                               style = "color: #007bff; cursor: help; margin-left: 5px;")),
                           shiny::tags$td(fmt(lf$majority_acc)), shiny::tags$td("0.000"))
          )
        ),
        shiny::tags$p(style = "font-size: 0.88em;",
          sprintf("Covariates %s out-of-fold accuracy by %.1f points (%.3f vs %.3f) relative to using spatial position alone, on identical folds. %s",
                  dir_word, 100 * abs(lf$lift_abs), lf$model_acc, lf$baseline_acc, p_lbl))
      )
    })

    # ── Permutation feature importance ───────────────────────────────────────
    # One builder for the in-page card and its expanded modal.
    build_importance_plot <- function() {
      res <- cl_rv$res; shiny::req(res, res$importance)
      imp <- res$importance
      imp$label <- vapply(imp$predictor,
                          function(v) get_var_label(v, vars_metadata_reactive()), character(1))
      imp$label <- factor(imp$label, levels = rev(imp$label))
      imp$txt <- ifelse(is.na(imp$share_pct), "", sprintf("%.1f%%", imp$share_pct))
      # Name the evaluation design on the axis: an out-of-fold and a
      # training-row importance are different quantities and must never be
      # compared as if they were the same number. Kept short because ggplot
      # centres the axis title on the PANEL, not the device: long covariate
      # labels push the panel right, and the full phrase ran off the edge of
      # this half-width card. "Importance" is already the panel heading, so the
      # axis only has to name the quantity and the design.
      x_lab <- if (identical(imp$evaluated_on[1], "out-of-fold")) {
        "Permutation Δ log-loss (out-of-fold)"
      } else {
        "Permutation Δ log-loss (training rows)"
      }
      ggplot2::ggplot(imp, ggplot2::aes(x = importance, y = label)) +
        ggplot2::geom_col(fill = "#2c7fb8", width = 0.7) +
        ggplot2::geom_text(ggplot2::aes(label = txt), hjust = -0.15, size = 3.4) +
        ggplot2::scale_x_continuous(expand = ggplot2::expansion(mult = c(0.02, 0.18))) +
        # Wrap long metadata labels instead of letting them eat the panel width.
        ggplot2::scale_y_discrete(labels = scales::label_wrap(22)) +
        ggplot2::labs(x = x_lab, y = NULL) +
        ggplot2::theme_minimal(base_size = 13)
    }
    output$importance_plot <- shiny::renderPlot({ build_importance_plot() })

    output$group_metrics_table <- DT::renderDataTable({
      res <- cl_rv$res; shiny::req(res, res$group_metrics)
      gm <- res$group_metrics
      for (cn in c("accuracy", "kap", "bal_accuracy", "f_meas")) gm[[cn]] <- round(gm[[cn]], 3)
      colnames(gm) <- c("Area", "n", "Overall accuracy", "Cohen's kappa",
                        "Balanced accuracy", "F1 score (macro)")
      DT::datatable(gm, options = list(dom = 't', pageLength = 15, scrollX = TRUE),
                    rownames = FALSE)
    })

    output$metrics_table <- DT::renderDataTable({
      res <- cl_rv$res; shiny::req(res)
      m <- classif_label_metrics(res$cv_metrics)
      out <- data.frame(Metric = m$.metric_label,
                        Estimator = m$.estimator_label,
                        Value = round(m$.estimate, 4),
                        check.names = FALSE)
      DT::datatable(out, options = list(dom = 't', pageLength = 12, scrollX = TRUE), rownames = FALSE)
    })

    output$confmat_table <- DT::renderDataTable({
      res <- cl_rv$res; shiny::req(res)
      cm <- as.data.frame.matrix(res$conf_mat)
      DT::datatable(cm, options = list(dom = 't', scrollX = TRUE),
                    caption = "Rows = predicted, columns = actual")
    })

    output$perclass_table <- DT::renderDataTable({
      res <- cl_rv$res; shiny::req(res)
      pc <- res$per_class
      pc$producer_accuracy <- round(pc$producer_accuracy, 3)
      pc$user_accuracy <- round(pc$user_accuracy, 3)
      colnames(pc) <- c("Class", "n", "Producer acc. (recall)", "User acc. (precision)")
      DT::datatable(pc, options = list(dom = 't', scrollX = TRUE), rownames = FALSE)
    })

    # Area accounting follows the rasteriser (not the worker's res$area) so the
    # table honours the live confidence threshold, including the Unclassified row.
    output$area_table <- DT::renderDataTable({
      rl <- get_rasters()
      a <- rl$area; a$area_ha <- round(a$area_ha, 2)
      colnames(a) <- c("Class", "Cells", "Area (ha)")
      DT::datatable(a, options = list(dom = 't', scrollX = TRUE), rownames = FALSE)
    })

    output$run_summary <- shiny::renderUI({
      res <- cl_rv$res
      if (is.null(res)) return(NULL)
      acc <- res$cv_metrics$.estimate[res$cv_metrics$.metric == "accuracy"]
      kap <- res$cv_metrics$.estimate[res$cv_metrics$.metric == "kap"]
      drop_part <- if (length(res$dropped_covariates) > 0) {
        sprintf(" Collinear covariates dropped: %s.", paste(res$dropped_covariates, collapse = ", "))
      } else ""
      shiny::tagList(
        shiny::tags$small(style = "color:#888;",
          sprintf("Last run: %s, %d classes, scope: %s. Accuracy %.3f, kappa %.3f.%s",
                  classif_methods()[[res$method]], length(res$levels),
                  if (is.null(res$scope_label)) "all data" else res$scope_label,
                  ifelse(length(acc), acc, NA), ifelse(length(kap), kap, NA), drop_part))
      )
    })

    # ── Maps ─────────────────────────────────────────────────────────────────
    # Shared ggplot builders (screen renders + styled PNG export). maxcell
    # caps DISPLAY resampling only; exports of the data rasters stay full-res.
    # `export = TRUE` switches to publication styling: larger text throughout
    # and projected coordinate axes (grid numbers, Easting/Northing in metres)
    # via coord_sf(datum = <raster CRS>); on-screen maps stay minimal.
    map_theme <- function(export = FALSE) {
      if (!export) {
        return(ggplot2::theme_minimal(base_size = 12) +
                 ggplot2::theme(axis.text = ggplot2::element_blank()))
      }
      ggplot2::theme_minimal(base_size = 16) +
        ggplot2::theme(
          plot.title = ggplot2::element_text(face = "bold", size = 18),
          axis.text = ggplot2::element_text(size = 11, colour = "grey25"),
          axis.text.y = ggplot2::element_text(angle = 90, hjust = 0.5),
          axis.title = ggplot2::element_text(size = 13),
          legend.title = ggplot2::element_text(size = 14),
          legend.text = ggplot2::element_text(size = 12),
          panel.grid = ggplot2::element_line(colour = "grey85", linewidth = 0.3)
        )
    }
    export_axes <- function(export) {
      res <- cl_rv$res
      if (!export || is.null(res$crs_wkt)) return(NULL)
      list(
        ggplot2::coord_sf(datum = sf::st_crs(res$crs_wkt)),
        ggplot2::scale_x_continuous(labels = scales::label_number(big.mark = ",", accuracy = 1)),
        ggplot2::scale_y_continuous(labels = scales::label_number(big.mark = ",", accuracy = 1)),
        ggplot2::labs(x = "Easting (m)", y = "Northing (m)")
      )
    }
    # Named palette shared by the on-screen map and the GeoTIFF colour table:
    # viridis over the model classes plus neutral grey for abstained cells.
    class_fill_scale <- function(rl) {
      levs_r <- terra::levels(rl$class)[[1]]$class
      pal <- stats::setNames(viridisLite::viridis(sum(levs_r != "Unclassified")),
                             levs_r[levs_r != "Unclassified"])
      if ("Unclassified" %in% levs_r) pal <- c(pal, c(Unclassified = "#9E9E9E"))
      ggplot2::scale_fill_manual(name = "Class", values = pal,
                                 na.value = "transparent", drop = FALSE,
                                 na.translate = FALSE)
    }
    # Scoped training points as sf in the raster CRS, for the optional
    # sample-point overlay (coords shipped with the run result).
    train_pts_sf <- shiny::reactive({
      res <- cl_rv$res
      if (is.null(res$train_xy) || is.null(res$crs_wkt)) return(NULL)
      xy <- as.data.frame(res$train_xy)
      sf::st_as_sf(xy, coords = c("X", "Y"), crs = res$crs_wkt)
    })
    # Optional display layers (all default off): sample-point overlay and
    # ggspatial scale bar + north arrow. Purely cosmetic - they never touch
    # the data rasters, only what is drawn on screen / exported as PNG.
    map_overlays <- function(export = FALSE) {
      layers <- list()
      if (isTRUE(input$map_points)) {
        pts <- train_pts_sf()
        if (!is.null(pts)) {
          layers <- c(layers, list(ggplot2::geom_sf(
            data = pts, inherit.aes = FALSE, shape = 21,
            fill = "white", color = "black", size = if (export) 1.6 else 1)))
        }
      }
      if (isTRUE(input$map_adorn)) {
        layers <- c(layers, list(
          ggspatial::annotation_scale(location = "br"),
          ggspatial::annotation_north_arrow(
            location = "tl",
            height = ggplot2::unit(1.1, "cm"), width = ggplot2::unit(1.1, "cm"))))
      }
      layers
    }
    # maxcell caps geom_spatraster's display resampling: 5e4 for the small
    # in-grid panels, 4e5 for the expanded modal view (higher resolution).
    plot_class_map <- function(rl, export = FALSE, maxcell = 5e4) {
      ggplot2::ggplot() +
        tidyterra::geom_spatraster(data = rl$class, maxcell = maxcell) +
        class_fill_scale(rl) +
        map_overlays(export) +
        ggplot2::labs(title = "Predicted Class") + export_axes(export) + map_theme(export)
    }
    plot_entropy_map <- function(rl, export = FALSE, maxcell = 5e4) {
      ggplot2::ggplot() +
        tidyterra::geom_spatraster(data = rl$entropy, maxcell = maxcell) +
        ggplot2::scale_fill_viridis_c(name = "Entropy", option = "magma",
                                      na.value = "transparent", limits = c(0, 1)) +
        map_overlays(export) +
        ggplot2::labs(title = "Classification Uncertainty") + export_axes(export) + map_theme(export)
    }
    plot_prob_map <- function(rl, lyr, class_label, export = FALSE, maxcell = 5e4) {
      ggplot2::ggplot() +
        tidyterra::geom_spatraster(data = rl$prob[[lyr]], maxcell = maxcell) +
        ggplot2::scale_fill_viridis_c(name = "P", na.value = "transparent", limits = c(0, 1)) +
        map_overlays(export) +
        ggplot2::labs(title = paste("P(class =", class_label, ")")) + export_axes(export) + map_theme(export)
    }

    # ── Expanded (modal) map view ────────────────────────────────────────────
    # Clicking a map, or its expand icon, opens the same map full-size at
    # higher display resolution (larger canvas + more raster cells).
    show_map_modal <- function(which) {
      cl_rv$modal_map <- which
      ttl <- c(class = "Predicted Class Map",
               entropy = "Prediction Uncertainty (Entropy)",
               prob = "Class Probability Map")[[which]]
      shiny::showModal(shiny::modalDialog(
        title = paste0("Expanded View: ", ttl), size = "l", easyClose = TRUE,
        shiny::plotOutput(ns("modal_map"), height = "700px"),
        footer = shiny::modalButton("Close")
      ))
    }
    shiny::observeEvent(input$class_expand_btn,   show_map_modal("class"))
    shiny::observeEvent(input$class_map_click,    show_map_modal("class"))
    shiny::observeEvent(input$entropy_expand_btn, show_map_modal("entropy"))
    shiny::observeEvent(input$entropy_map_click,  show_map_modal("entropy"))
    shiny::observeEvent(input$prob_expand_btn,    show_map_modal("prob"))
    shiny::observeEvent(input$prob_map_click,     show_map_modal("prob"))
    # The importance panel is the one a reader most often wants enlarged (long
    # covariate labels, many bars), and after the card rework it would
    # otherwise be the only one of the four without an expand control.
    shiny::observeEvent(input$importance_expand_btn, {
      shiny::showModal(shiny::modalDialog(
        title = "Expanded View: Feature Importance", size = "l", easyClose = TRUE,
        shiny::plotOutput(ns("modal_importance"), height = "700px"),
        footer = shiny::modalButton("Close")
      ))
    })
    output$modal_importance <- shiny::renderPlot({ build_importance_plot() }, res = 96)
    output$modal_map <- shiny::renderPlot({
      which <- cl_rv$modal_map; shiny::req(which)
      rl <- get_rasters()
      switch(which,
        class   = plot_class_map(rl, maxcell = 4e5),
        entropy = plot_entropy_map(rl, maxcell = 4e5),
        prob    = {
          shiny::req(input$prob_class)
          lyr <- paste0("P_", input$prob_class)
          shiny::req(lyr %in% names(rl$prob))
          plot_prob_map(rl, lyr, input$prob_class, maxcell = 4e5)
        })
    }, res = 96)

    # Cached plots keyed on the run token (+ selected class for the prob map):
    # resizes and revisits serve the cached bitmap instead of a multi-second
    # geom_spatraster re-render, so the maps stop flickering "for no reason".
    output$class_map <- shiny::renderCachedPlot({
      plot_class_map(get_rasters())
    }, cacheKeyExpr = {
      res <- cl_rv$res; shiny::req(res, res$surface_df)
      list(res$run_id, conf_thresh_d(), isTRUE(input$map_adorn), isTRUE(input$map_points))
    })

    output$entropy_map <- shiny::renderCachedPlot({
      plot_entropy_map(get_rasters())
    }, cacheKeyExpr = {
      res <- cl_rv$res; shiny::req(res, res$surface_df)
      list(res$run_id, isTRUE(input$map_adorn), isTRUE(input$map_points))
    })

    output$prob_class_ui <- shiny::renderUI({
      res <- cl_rv$res; shiny::req(res)
      shiny::selectInput(ns("prob_class"), "Class Probability Map", choices = res$levels, selected = res$levels[1])
    })

    output$prob_map <- shiny::renderCachedPlot({
      rl <- get_rasters()
      shiny::req(input$prob_class)
      lyr <- paste0("P_", input$prob_class)
      shiny::req(lyr %in% names(rl$prob))
      plot_prob_map(rl, lyr, input$prob_class)
    }, cacheKeyExpr = {
      res <- cl_rv$res; shiny::req(res, res$surface_df, input$prob_class)
      list(res$run_id, input$prob_class, isTRUE(input$map_adorn), isTRUE(input$map_points))
    })

    # ── Downloads ────────────────────────────────────────────────────────────
    # The class GeoTIFF is written as INT1U so GDAL embeds its colour table
    # (viridis, matching the on-screen map) in the file itself; float rasters
    # (probability, entropy) are data layers and stay full-precision FLT4S —
    # GIS software styles those on load.
    dl_raster <- function(which_r, fname, datatype = NULL) {
      shiny::downloadHandler(
        filename = function() fname,
        content = function(file) {
          rl <- get_rasters()
          if (is.null(datatype)) {
            terra::writeRaster(rl[[which_r]], file, overwrite = TRUE)
          } else {
            terra::writeRaster(rl[[which_r]], file, overwrite = TRUE, datatype = datatype)
          }
        }
      )
    }
    output$dl_class <- dl_raster("class", "predicted_class.tif", datatype = "INT1U")
    output$dl_prob <- dl_raster("prob", "class_probabilities.tif")
    output$dl_entropy <- dl_raster("entropy", "class_entropy.tif")

    # Publication-style renders of the three maps: export styling (larger
    # text, projected coordinate axes) on a larger canvas than the screen
    # preview, honouring the live confidence threshold on the class map.
    output$dl_png <- shiny::downloadHandler(
      filename = function() "classification_maps.zip",
      content = function(file) {
        rl <- get_rasters(); res <- cl_rv$res
        cls <- if (!is.null(input$prob_class)) input$prob_class else res$levels[1]
        lyr <- paste0("P_", cls)
        tmp <- file.path(tempdir(), paste0("classif_png_", format(Sys.time(), "%H%M%OS3")))
        dir.create(tmp, showWarnings = FALSE, recursive = TRUE)
        on.exit(unlink(tmp, recursive = TRUE), add = TRUE)
        paths <- file.path(tmp, c("predicted_class.png", "prediction_entropy.png",
                                  sprintf("probability_%s.png", gsub("[^A-Za-z0-9._-]+", "_", cls))))
        with_showtext_dpi(300, {
          ggplot2::ggsave(paths[1], plot_class_map(rl, export = TRUE), width = 9, height = 7, dpi = 300)
          ggplot2::ggsave(paths[2], plot_entropy_map(rl, export = TRUE), width = 9, height = 7, dpi = 300)
          ggplot2::ggsave(paths[3], plot_prob_map(rl, lyr, cls, export = TRUE), width = 9, height = 7, dpi = 300)
        })
        zip::zip(zipfile = file, files = basename(paths), root = tmp, mode = "cherry-pick")
      }
    )

    # Trained model bundle: the worker saved the fitted workflow + metadata to
    # a tempfile in run_classification_pipeline; this simply hands the file over.
    output$dl_model <- shiny::downloadHandler(
      filename = function() {
        res <- cl_rv$res
        sprintf("classification_model_%s_%s.rds",
                if (is.null(res$method)) "model" else res$method,
                format(Sys.Date(), "%Y%m%d"))
      },
      content = function(file) {
        res <- cl_rv$res
        shiny::req(res$model_path, file.exists(res$model_path))
        file.copy(res$model_path, file, overwrite = TRUE)
      }
    )

    output$dl_report <- shiny::downloadHandler(
      filename = function() "classification_metrics.csv",
      content = function(file) {
        res <- cl_rv$res
        m <- classif_label_metrics(res$cv_metrics)
        out <- data.frame(scope = "Total", metric = m$.metric_label,
                          yardstick_id = m$.metric,
                          estimator = m$.estimator_label, value = m$.estimate)
        # Per-area rows (class metrics only), matching the Performance by Area
        # table; the Total row above already carries the full pooled metric set.
        gm <- res$group_metrics
        if (!is.null(gm)) {
          gm <- gm[gm$scope != "Total", , drop = FALSE]
          if (nrow(gm) > 0) {
            ml <- classif_metric_labels()
            ids <- c("accuracy", "kap", "bal_accuracy", "f_meas")
            ests <- c("Multiclass", "Multiclass", "Macro average", "Macro average")
            per_area <- do.call(rbind, lapply(seq_len(nrow(gm)), function(i) {
              data.frame(scope = gm$scope[i], metric = unname(ml[ids]),
                         yardstick_id = ids, estimator = ests,
                         value = as.numeric(gm[i, ids]))
            }))
            out <- rbind(out, per_area)
          }
        }
        # Baseline comparison rows (same CV folds as the model metrics above).
        if (!is.null(res$lift)) {
          lf <- res$lift
          out <- rbind(out, data.frame(
            scope = "Baseline comparison",
            metric = c("Spatial 1-NN baseline accuracy", "Spatial 1-NN baseline kappa",
                       "Majority-class accuracy (no-information rate)",
                       "Covariate lift (accuracy points vs spatial baseline)",
                       "McNemar p (model vs spatial baseline)"),
            yardstick_id = c("baseline_acc", "baseline_kap", "majority_acc",
                             "lift_abs", "mcnemar_p"),
            estimator = "Paired out-of-fold",
            value = c(lf$baseline_acc, lf$baseline_kap, lf$majority_acc,
                      lf$lift_abs, lf$mcnemar_p)))
        }
        # Permutation feature importance. The scope names the evaluation design
        # (each fold's model on its held-out rows, or the final model on its own
        # training rows) so a reader of the CSV alone cannot mistake one for the
        # other — they are not comparable numbers.
        if (!is.null(res$importance)) {
          imp <- res$importance
          out <- rbind(out, data.frame(
            scope = sprintf("Feature importance (%s)", imp$evaluated_on[1]),
            metric = imp$predictor,
            yardstick_id = "perm_delta_logloss",
            estimator = sprintf("share %.1f%%", imp$share_pct),
            value = imp$importance))
        }
        utils::write.csv(out, file, row.names = FALSE)
      }
    )
  })
}
