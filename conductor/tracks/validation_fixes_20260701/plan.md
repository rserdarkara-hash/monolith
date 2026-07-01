# Implementation Plan: Resolve Geostatistical Validation, SHAP Analysis, and Theme Sync Issues

## Phase 1: Geostatistical and Validation Fixes [checkpoint: b3bf151]

- [x] Task: Fix TPS CV backfill and SHAP regex matching [0158ba0]
    - [x] Write unit/verification test scripts for TPS fold NA propagation and SHAP name matching
    - [x] Remove full-model fitted values substitution inside `perform_cv` in `spatial_helpers_0.9.8b.R`
    - [x] Replace `grepl` regex with exact match in `compute_governing_factors` inside `spatial_helpers_0.9.8b.R`
    - [x] Verify that metrics are correctly computed excluding failed folds and SHAP names are matched exactly

- [x] Task: Implement seed for Governing Factors reproducibility [efc9951]
    - [x] Write a verification script to run two identical governing factors models and compare feature importances
    - [x] Add `set.seed(12345)` before randomForest fit and SHAP sampling in `spatial_helpers_0.9.8b.R`
    - [x] Verify that repeated runs yield 100% identical importance and plot outputs

- [x] Task: Conductor - User Manual Verification 'Phase 1: Geostatistical and Validation Fixes' (Protocol in workflow.md)

## Phase 2: UI Re-entrancy and Sync Fixes

- [x] Task: Implement Governing Factors re-entrancy guard [5f0c1b3]
    - [x] Write verification plan to simulate rapid clicks on the run analysis button
    - [x] Add re-entrancy status checks and button disabling/enabling in `gov_module_0.9.8b.R`
    - [x] Verify that multiple clicks are blocked and the UI handles completion and error cases cleanly

- [~] Task: Fix Theme localStorage reconnection sync
    - [ ] Write manual verification steps for theme sync after reconnecting websocket
    - [ ] Remove `once = TRUE` from `observeEvent(input$saved_theme_js, ...)` in `theme_helpers_0.9.8b.R`
    - [ ] Verify that theme localStorage sync persists after websocket reconnect

- [ ] Task: Conductor - User Manual Verification 'Phase 2: UI Re-entrancy and Sync Fixes' (Protocol in workflow.md)
