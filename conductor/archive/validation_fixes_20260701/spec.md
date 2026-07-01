# Track Specification: Resolve Geostatistical Validation, SHAP Analysis, and Theme Sync Issues

## Overview
This track addresses the remaining five issues (Issues 4, 5, 6, 7, and 9) identified in `issues.md`. These issues affect the accuracy of TPS cross-validation, the correctness of SHAP analysis, the reproducibility of Governing Factors models, the re-entrancy of the Governing Factors UI, and the resilience of theme syncing across websocket reconnections.

## Functional & Technical Requirements
1. **TPS Cross-Validation NA Fix (Issue 4)**:
   * Remove lines 814–816 in `spatial_helpers_0.9.8b.R` that backfill failed folds with full-model fitted values.
   * Allow NAs to flow through so that they are correctly filtered out by `perform_cv()` without inflating reported metrics (RMSE, R², CCC, etc.).

2. **SHAP Variable Matching Regex Fix (Issue 5)**:
   * Modify line 942 in `spatial_helpers_0.9.8b.R` inside `compute_governing_factors`.
   * Replace the unanchored and unescaped `grepl` regex check with a strict exact match: `variable_name == top_var`.

3. **Governing Factors Reproducibility Fix (Issue 6)**:
   * Add `set.seed(12345)` before the randomForest fitting and before the SHAP sampling calls in `compute_governing_factors` inside `spatial_helpers_0.9.8b.R` to ensure identical runs yield identical feature importances and ALE/PDP plots.

4. **Governing Factors UI Re-entrancy Guard (Issue 7)**:
   * In `gov_module_0.9.8b.R`, implement a re-entrancy check at the start of `observeEvent(input$gov_run_btn, ...)` to check if the analysis is already running.
   * Disable the "Run Analysis" button (`gov_run_btn`) during processing and re-enable it upon success or failure callbacks.

5. **Theme localStorage Sync Fix (Issue 9)**:
   * In `theme_helpers_0.9.8b.R` line 396, change the `observeEvent(input$saved_theme_js, ...)` parameters.
   * Remove `once = TRUE` so that theme synchronization persists across Shiny websocket reconnections.

## Acceptance Criteria
* The TPS cross-validation report excludes failed folds instead of backfilling with full-model predictions.
* Variables containing wildcards (e.g. dots `.`) or similar names do not false-match unrelated columns in SHAP calculations.
* Repeated Governing Factors runs on identical inputs yield identical importances, variable selection, and plots.
* Multiple rapid clicks on the Governing Factors "Run Analysis" button are blocked.
* Theme sync from localStorage continues working after websocket reconnection.
