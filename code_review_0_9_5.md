# Technical Code Review: Spatial Analysis Dashboard (v0.9.5)

This document provides a detailed technical code review of the spatial analysis dashboard, focusing on the 4 provided source files (`monolith_ver_0.9.5.R`, `spatial_helpers_0.9.5.R`, `ui_helpers_0.9.5.R`, `theme_helpers_0.9.5.R`). 

---

## 1. Reactive Architecture

### Redundant or Over-triggering Reactives
- **[MAJOR] Excessive `observe()` blocks**: There are 14 distinct `observe({})` blocks in the monolith (e.g., lines 829, 2213, 2915, 3318, 5063). Using `observe()` instead of `observeEvent()` forces Shiny to infer dependencies implicitly. For instance, reacting to a button click or a specific UI input state should strictly use `observeEvent()` to prevent unintended multiple executions when unrelated reactive values change.
- **[MAJOR] Overcentralised `reactiveValues` State**: The application relies heavily on massive, monolithic `reactiveValues` objects: `rv` (line 2081), `pca_rv` (line 1535), and `gov_rv` (line 1790). Storing derived datasets (e.g., `rv$rast`, `rv$rast_pred`) imperatively via assignment (`rv$rast <- ...`) is an anti-pattern. This leads to spaghetti state management.
  - *Recommendation*: Use `reactive()` expressions to calculate derived data. Let Shiny's reactivity graph natively handle invalidation.

### Missing `req()` Guards
- **[CRITICAL] Null-Propagation Errors**: While `req(rv$user_data)` is well utilized, deep reactive chains and `tryCatch` blocks frequently return `NULL` (e.g., `tryCatch(st_crs(input$crs_selection), error = function(e) NULL)` at line 3126). If downstream observers do not use `req(crs_obj)` before evaluating it, the application will throw unhandled null-pointer or subsetting errors.

---

## 2. Modularisation

### Candidates for `moduleServer()` Extraction
- **[MAJOR] Monolithic Server Architecture**: `monolith_ver_0.9.5.R` contains over 3,500 lines of UI and server logic. While `theme_helpers_0.9.5.R` correctly uses `shiny::moduleServer`, the rest of the app is completely flat. 
- **[MAJOR] Module Boundary Mismatches**: `ui_helpers_0.9.5.R` provides UI factory functions, but their corresponding server logic remains trapped inside the monolith. 

### Clean Module Boundaries
- **Governing Factors Tab**: Can be entirely decoupled. `govServer("gov", data = reactive(rv_analytics_data()))`. It should manage its own `gov_rv` state internally rather than leaking it into the main server environment.

---

## 3. Performance and Memory

### Caching and Memoisation
- **[MAJOR] Missing `bindCache()`**: There is no utilization of `bindCache()` or memoisation for expensive operations. Toggling between previously generated plots, variograms, or heavy UI renders forces a full synchronous recalculation. 

### `terra::wrap` and `unwrap`

- **[MAJOR] Unnecessary Unwrapping**: The code redundantly unwraps the same raster across multiple renders. For example, `calc_area_df(terra::unwrap(rv$rast))` is computed separately in `output$area_table_total_act` (line 5557) and again for specific localities. 
  - *Recommendation*: Extract statistical summaries or dataframes into a `reactive()` *once* and have the tables consume the lightweight dataframe, rather than repeatedly unwrapping the raster in the UI renders.

---

## 4. Error Handling and User Feedback

### Unhandled Failure Modes
- **[MAJOR] Covariate Kriging**: The codebase implements a fallback mechanism (e.g., line 4117: `"Covariate %s kriging failed. Falling back to IDW"`), which is good. However, failures during file uploads (e.g., empty shapefiles) or singular variogram matrices are often just swallowed into `NULL`, breaking downstream execution without UI alerts.

---

## 5. Code Quality and Maintainability

### Abstraction and Styling
- **[MAJOR] Copy-Pasted Logic**: There are massive chunks of duplicated code for managing the `act` (actual) vs `pre` (predicted) rasters . These should be abstracted into a generic mapping and summarization function.
- **[MINOR] Inconsistent Naming**: The codebase switches between `snake_case` (e.g., `rv$desc_vars_state$x`), camelCase, and dot notation.
- **[MINOR] Global Variable Mutation**: Manipulating external environments (`env$res`) inside localized functions (line 4089) creates hidden side effects and breaks pure functional paradigms.

---

## 6. Security and Robustness

### File Handlers
- **[MAJOR] Upload Validation**: File uploads via `fileInput` are passed directly to `st_read` via `tryCatch` (line 3059). Missing robust validation for file extensions, mime types, and bounding box constraints before processing can lead to memory exhaustion on malicious or accidental massive uploads.

---
