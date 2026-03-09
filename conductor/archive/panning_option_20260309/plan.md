# Implementation Plan: Interactive Map Panning by Locality

## Phase 1: Setup and Backup
- [x] Task: Backup existing application files ad80d3a
    - [ ] Create a new directory in `backups/` (e.g., `pre_v0.8.6_panning_YYYYMMDD`).
    - [ ] Copy `monolith_0.8.5.R` to the new backup directory.
    - [ ] Copy `improvements/spatial_helpers_0.8.5.R`, `improvements/theme_helpers_0.8.5.R`, and `improvements/ui_helpers_0.8.5.R` to the new backup directory.
- [x] Task: Create new version files f57dc11
    - [ ] Copy `monolith_0.8.5.R` to `monolith_0.8.6.R`.
    - [ ] Copy `improvements/*_0.8.5.R` to `improvements/*_0.8.6.R`.
    - [ ] Update `source()` calls in `monolith_0.8.6.R` to point to the new `0.8.6` helpers.

## Phase 2: UI Implementation
- [x] Task: Write failing UI tests 0641a18
- [x] Task: Implement Locality Pan Dropdown in UI 0641a18
- [x] Task: Verify UI tests pass 0641a18
- [ ] Task: Conductor - User Manual Verification 'UI Implementation' (Protocol in workflow.md)

## Phase 3: Server Logic and Interactive Panning
- [x] Task: Write failing Server logic tests 2b965db
- [x] Task: Implement Server-Side Bounds Calculation 2b965db
- [x] Task: Implement `leafletProxy` Panning 2b965db
- [x] Task: Implement Single Locality Edge Case 2b965db
- [x] Task: Verify server tests pass 2b965db
- [ ] Task: Conductor - User Manual Verification 'Server Logic and Interactive Panning' (Protocol in workflow.md)

## Phase 4: Final Polish and Documentation
- [x] Task: Synchronize Project Documentation 1762834
- [x] Task: Final Manual Smoke Test 1762834
- [x] Task: Conductor - User Manual Verification 'Final Polish and Documentation' (Protocol in workflow.md) 1762834