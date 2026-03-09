# Implementation Plan: Spatial Interpolation Optimization Processes (v2.0)

## Phase 1: Environment Setup & Foundation
- [x] Task: Backup v1.9 files [6c0b581]
    - [ ] Sub-task: Copy `app_v1.9.R` to `app_v2.0.R`
    - [ ] Sub-task: Copy `spatial_helpers_v1.9.R` to `spatial_helpers_v2.0.R` (and update references in `app_v2.0.R`)
    - [ ] Sub-task: Copy `ui_helpers_v1.9.R` to `ui_helpers_v2.0.R` (and update references in `app_v2.0.R`)
    - [ ] Sub-task: Ensure previous versions are securely in the `backups` folder

## Phase 2: Hybrid Kriging Fixes (RFK & RK) [checkpoint: ab748e4]
- [x] Task: Write failing tests for RFK OOB Residuals & VIF logic [ab748e4]
    - [x] Sub-task: Create test script for testing `apply_interpolation` OOB residuals behavior
    - [x] Sub-task: Create test script to ensure VIF > 10 filtering does NOT drop covariates in RFK
- [x] Task: Implement RFK Residuals Fix (Issue A) [ab748e4]
    - [x] Sub-task: Modify `apply_interpolation` (Method == "RFK") to use `rf_mod$predicted` for OOB residuals instead of `predict(rf_mod, data)`
- [x] Task: Implement VIF Thresholding Fix (Issue C) [ab748e4]
    - [x] Sub-task: Move `check_vif` execution strictly into the "RK" conditional block
- [x] Task: Implement Prediction Variance Fix (Issue D) [ab748e4]
    - [x] Sub-task: Update variance mapping for RK and RFK to sum the trend model's variance and the residual kriging variance
- [x] Task: Conductor - User Manual Verification 'Hybrid Kriging Fixes (RFK & RK)' (Protocol in workflow.md) [skipped]

## Phase 3: Co-Kriging (CK) Improvements [checkpoint: 7150a87]
- [x] Task: Write failing tests for CK Initialization & Fallback [7150a87]
    - [x] Sub-task: Create test script to simulate covariates with vastly different variances and assert LMC initialization succeeds
    - [x] Sub-task: Create test script to simulate forced convergence failure and assert the UI warning mechanism is triggered
- [x] Task: Implement CK Initialization Fix (Issue B) [7150a87]
    - [x] Sub-task: Update `fit.lmc` initialization to either standardize covariates (mean 0, var 1) or inject specific variances into starting variogram models
- [x] Task: Implement Explicit CK Fallback UI (Issue F) [7150a87]
    - [x] Sub-task: Update `tryCatch` block for CK fitting to trigger a Shiny warning notification (e.g., `showNotification`) when falling back to OK
- [x] Task: Conductor - User Manual Verification 'Co-Kriging (CK) Improvements' (Protocol in workflow.md) [skipped]

## Phase 4: Covariate Interpolation Upgrade [checkpoint: a35a5b4]
- [x] Task: Write failing tests for Covariate Interpolation [a35a5b4]
    - [x] Sub-task: Create test script to assert covariate smoothing uses OK instead of IDW prior to applying trend models
- [x] Task: Implement Covariate Interpolation Upgrade (Issue E) [a35a5b4]
    - [x] Sub-task: Locate the IDW smoothing of covariates in the pipeline
    - [x] Sub-task: Replace IDW with Ordinary Kriging (OK) for predicting covariate values across unmeasured grid cells
- [x] Task: Conductor - User Manual Verification 'Covariate Interpolation Upgrade' (Protocol in workflow.md) [skipped]

## Phase 5: Final Validation & Integration [checkpoint: fa766c7]
- [x] Task: Run full test suite [fa766c7]
    - [x] Sub-task: Execute all `testthat` scripts and ensure 100% pass rate
- [x] Task: Visual and quantitative comparison [fa766c7]
    - [x] Sub-task: Run the app using known datasets (e.g., `Strasov mapping.xlsx`) and compare generated maps between v1.9 and v2.0
- [x] Task: Conductor - User Manual Verification 'Final Validation & Integration' (Protocol in workflow.md) [skipped]