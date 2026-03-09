# Implementation Plan: Interactive Radar Plot Fix

## Phase 1: Environment Backup and Version Preparation
- [ ] Task: Backup existing files
    - [ ] Create a new backup directory in `backups/` (e.g., `pre_v0.8.3_upgrade_20260308`).
    - [ ] Copy `monolith_0.8.2.R` and its helper files (`improvements/spatial_helpers_0.8.2.R`, `improvements/theme_helpers_0.8.2.R`, `improvements/ui_helpers_0.8.2.R`) to the backup directory.
- [ ] Task: Create new version files
    - [ ] Copy `monolith_0.8.2.R` to `monolith_0.8.3.R`.
    - [ ] Copy `improvements/spatial_helpers_0.8.2.R` to `improvements/spatial_helpers_0.8.3.R`.
    - [ ] Copy `improvements/theme_helpers_0.8.2.R` to `improvements/theme_helpers_0.8.3.R`.
    - [ ] Copy `improvements/ui_helpers_0.8.2.R` to `improvements/ui_helpers_0.8.3.R`.
- [ ] Task: Update file references
    - [ ] Update `source()` calls in `monolith_0.8.3.R` to point to the new `0.8.3` helper files in the `improvements/` directory.
- [ ] Task: Conductor - User Manual Verification 'Environment Backup and Version Preparation' (Protocol in workflow.md)

## Phase 2: Radar Plot Interactive Bug Fix
- [ ] Task: Diagnose the interactive radar plot error
    - [ ] Search the codebase to locate where the radar plot is built and converted to `plotly` within the Scientific Analytics Panel.
    - [ ] Identify the specific `ggplotly()` layer or parameter causing the "attempt to apply non-function" error.
- [ ] Task: Implement the fix
    - [ ] Modify the plotting logic in `monolith_0.8.3.R` or the relevant helper file. If `coord_radar()` breaks `ggplotly()`, implement a workaround (e.g., using `plot_ly` with `type = 'scatterpolar'`, or safely bypassing the problematic layer).
    - [ ] Ensure that static radar plots are not degraded by the fix.
- [ ] Task: Refactor and Verify
    - [ ] Run test combinations with 'samp_data_1.xlsx' and 'samp_data_2.xlsx' to ensure the interactive radar plot works.
    - [ ] Verify that other plot types within the interactive modal are not broken.
- [ ] Task: Conductor - User Manual Verification 'Radar Plot Interactive Bug Fix' (Protocol in workflow.md)