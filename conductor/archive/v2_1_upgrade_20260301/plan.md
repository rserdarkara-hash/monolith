# Implementation Plan: Covariate Fallback, Parallelization & Label Rendering (v2.1)

## Phase 1: Preparation & Setup
- [x] Task: Create new application files (`app_v2.1.R`, `spatial_helpers_v2.1.R`, `ui_helpers_v2.1.R`) by copying the v2.0 versions. Do not overwrite the originals.
- [x] Task: Integrate `furrr` package dependency in `app_v2.1.R` and configure `future::plan(multisession)` correctly.
- [x] Task: Conductor - User Manual Verification 'Phase 1: Preparation & Setup' (Protocol in workflow.md)

## Phase 2: Covariate Interpolation Safety Wrapper
- [x] Task: Create/update unit tests for the covariate kriging fallback logic simulating pure nugget errors.
- [x] Task: Implement `tryCatch` wrapper around Ordinary Kriging logic for auxiliary covariates in `app_v2.1.R` or `spatial_helpers_v2.1.R` to fallback to IDW.
- [x] Task: Implement UI notification mechanism (e.g., Shiny `showNotification`) that triggers during a covariate fallback event.
- [x] Task: Conductor - User Manual Verification 'Phase 2: Covariate Interpolation Safety Wrapper' (Protocol in workflow.md)

## Phase 3: Comprehensive Parallel Processing
- [x] Task: Create/update unit tests to verify interpolation pipelines run successfully and deterministically with `furrr` parallelism.
- [x] Task: Refactor the sequential multi-locality loops into `future_map()` or `future_lapply()`.
- [x] Task: Refactor IDW parameter optimization searches for parallel execution.
- [x] Task: Refactor structural model fitting and learning loops (RK/RFK) for parallel execution.
- [x] Task: Refactor the map generation process (actual, predicted, comparison, residuals) to run simultaneously via futures where applicable.
- [x] Task: Conductor - User Manual Verification 'Phase 3: Comprehensive Parallel Processing' (Protocol in workflow.md)

## Phase 4: Correlation Rank Module Enhancement
- [x] Task: Create/update unit tests for the correlation rank module's label resolution logic.
- [x] Task: Update the correlation rank module code to join/match column names with labels defined in `variable list.xlsx`.
- [x] Task: Implement fallback rendering logic to display raw column names when the label is missing or undefined.
- [x] Task: Conductor - User Manual Verification 'Phase 4: Correlation Rank Module Enhancement' (Protocol in workflow.md)