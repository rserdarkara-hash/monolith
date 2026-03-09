# Implementation Plan: Clay Column Recognition Fix

## Phase 1: Preparation & Backup
- [x] Task: Create backup of version 2.6 (Commit: 193570955bec15125df50669108358dec4cdccb1)
    - [x] Copy `app_v2.6.R` and its corresponding helpers to the backups directory (e.g., `backups/pre_v2.6a_upgrade_YYYYMMDD/`)
- [x] Task: Scaffold version 2.6a (Commit: 193570955bec15125df50669108358dec4cdccb1)
    - [x] Create `app_v2.6a.R` and `_v2.6a.R` versions of the helper files
    - [x] Update internal source references in `app_v2.6a.R` to point to the newly created helper files

## Phase 2: Bug Fix Implementation
- [x] Task: Investigate column parsing and metadata mapping logic (Commit: 671a282c15ccb356442675737067c8f8bdd16433)
    - [x] Identify how dataframe columns are matched with the provided variable list metadata
    - [x] Write a failing test case replicating the 'clay' variable omission
- [x] Task: Implement generalized variable recognition (Commit: 671a282c15ccb356442675737067c8f8bdd16433)
    - [x] Update the parsing logic to incorporate a robust string matching or fallback mechanism for unmapped parameters (specifically addressing the 'clay' mismatch)
    - [x] Ensure the UI components that list available variables gracefully include these automatically recovered parameters

## Phase 3: Validation & Testing
- [x] Task: Verify functionality and regression testing
    - [x] Execute automated tests to ensure 'clay' can be mapped and other variables remain unaffected
    - [x] Confirm the spatial interpolation tools correctly accept the 'clay' parameter
- [x] Task: Conductor - User Manual Verification 'Validation & Testing' (Protocol in workflow.md)