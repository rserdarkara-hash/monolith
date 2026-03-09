# Implementation Plan: TPS Optimization Fix & v2.2 Upgrade

## Phase 1: Setup and Version Backup
- [x] Task: Create backups of `app_v2.1.R`, `spatial_helpers_v2.1.R`, and `ui_helpers_v2.1.R` to a new folder in `backups/`. c7b5140
- [x] Task: Create `app_v2.2.R`, `spatial_helpers_v2.2.R`, `ui_helpers_v2.2.R` as exact copies of the v2.1 files. c7b5140
- [x] Task: Update the internal `source()` calls and naming references in `app_v2.2.R` to point to the new v2.2 helper files. c7b5140

## Phase 2: Moran's I UI Integration
- [x] Task: Write tests to ensure Moran's I metric is correctly formatted and exposed in UI data frames.
- [x] Task: Update the `renderDataTable` logic in `app_v2.2.R` or `ui_helpers_v2.2.R` to explicitly include the internally calculated Moran's I column, ensuring it is no longer hidden from the user.
- [x] Task: Conductor - User Manual Verification 'Moran's I UI Integration' (Protocol in workflow.md)

## Phase 3: Correlation Rank List P-values & Filtering
- [x] Task: Write tests for a function that calculates p-values for correlation ranks.
- [x] Task: Implement the p-value calculation alongside correlation coefficients in the analysis logic (likely in `spatial_helpers_v2.2.R` or `app_v2.2.R`).
- [x] Task: Write tests for filtering the correlation list by custom significance thresholds (alpha levels).
- [x] Task: Update the UI in `app_v2.2.R` to add a significance threshold selector (e.g., 0.05, 0.01, 0.001).
- [x] Task: Implement the reactive filtering logic to update the correlation rank list display based on the selected p-value threshold.
- [x] Task: Conductor - User Manual Verification 'Correlation Rank List P-values & Filtering' (Protocol in workflow.md)

## Phase 4: TPS Optimization Fix
- [x] Task: Write tests to verify that TPS optimization returns a valid, non-zero lambda value and robust GCV curve on a sample dataset.
- [x] Task: Analyze `spatial_helpers_v2.2.R` to locate the current TPS lambda optimization logic (v2.1 concurrent approach).
- [x] Task: Cross-reference the logic with the working implementation from version 2.0 (`spatial_helpers_v2.0.R`).
- [x] Task: Modify the TPS optimization logic to correctly calculate GCV and select an optimal lambda > 0 while maintaining compatibility with the concurrent `furrr`/`future` execution framework.
- [x] Task: Add diagnostic output or UI elements to confirm the scientific correctness of the optimization process if necessary.
- [x] Task: Conductor - User Manual Verification 'TPS Optimization Fix' (Protocol in workflow.md)