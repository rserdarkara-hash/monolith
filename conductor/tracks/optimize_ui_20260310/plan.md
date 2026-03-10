# Implementation Plan: Optimizing UI Content Depending on User Mapping Choice

## Phase 1: Environment Preparation and Checkpointing
- [ ] Task: Backup `monolith_ver_0.8.7.R` and its helper files (`improvements/spatial_helpers_0.8.7.R`, `improvements/theme_helpers_0.8.7.R`, `improvements/ui_helpers_0.8.7.R`) to the `backups/` directory.
- [ ] Task: Copy backed up files to create `monolith_ver_0.8.8.R` and corresponding `*_0.8.8.R` helper scripts in their respective locations (`improvements/` and root). Update internal `source()` links to point to `0.8.8` versions.
- [ ] Task: Conductor - User Manual Verification 'Environment Preparation and Checkpointing' (Protocol in workflow.md)

## Phase 2: Reactive UI State Implementation
- [ ] Task: Write failing tests for interpolation state detection logic (verifying a reactive variable that tracks if predicted data exists).
- [ ] Task: Implement the reactive state logic in `monolith_ver_0.8.8.R` (e.g., a reactive value `rv$has_predictions` initialized to `FALSE` and set to `TRUE` upon successful interpolation).
- [ ] Task: Conductor - User Manual Verification 'Reactive UI State Implementation' (Protocol in workflow.md)

## Phase 3: Dynamic Visibility for Summaries and Exports
- [ ] Task: Write failing tests for optimization summaries UI components ensuring they hide when `rv$has_predictions` is `FALSE`.
- [ ] Task: Implement `shinyjs::hide()` and `shinyjs::show()` wrappers around UI elements in the Optimization Summaries and Export registries depending on `rv$has_predictions` in `monolith_ver_0.8.8.R` or `ui_helpers_0.8.8.R`.
- [ ] Task: Write failing tests for data filtering logic ensuring "Predicted", "Residual", or "Comparison" metrics are excluded from dataframes exported when `rv$has_predictions` is `FALSE`.
- [ ] Task: Implement dynamic data omission for predicted-data columns/rows in export module tables and summary generation functions.
- [ ] Task: Conductor - User Manual Verification 'Dynamic Visibility for Summaries and Exports' (Protocol in workflow.md)