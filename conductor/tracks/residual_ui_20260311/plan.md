# Implementation Plan: UI Content Optimization for Residual Mapping (v0.8.9)

## Phase 1: Setup and File Versioning
- [x] Task: Create backups of monolith_ver_0.8.8.R and all its helper files. [09195ae]
    - [x] Locate `monolith_ver_0.8.8.R` and all `_0.8.8.R` helper scripts (spatial, theme, ui).
    - [x] Copy these files into the `backups/` directory.
- [x] Task: Create new version 0.8.9 files. [278ff5a]
    - [x] Rename the main app file from `0.8.8.R` to `monolith_ver_0.8.9.R`.
    - [x] Rename all corresponding helper files from `_0.8.8.R` to `_0.8.9.R`.
    - [x] Update `source()` calls in `monolith_ver_0.8.9.R` to reference the new helper files.
- [x] Task: Conductor - User Manual Verification 'Setup and File Versioning' (Protocol in workflow.md)

## Phase 2: Modify UI for Residual Mapping
- [x] Task: Write tests for UI changes. [72973eb]
    - [x] Add a test in `tests/testthat/` to ensure the dropdown menu is not rendered when mapping residuals and the text is added.
    - [x] Verify test fails initially.
- [x] Task: Remove dropdown menu from the residual mapping UI.
    - [x] Locate the UI definition for the residual mapping dropdown.
    - [x] Remove or conditionally hide the dropdown.
- [x] Task: Add descriptive text to the control panel.
    - [x] Inject explicit descriptive text regarding "Interpolated Delta" and "Interpolated Point Errors" into the control panel where the dropdown was previously located.
- [x] Task: Update the layout to support side-by-side maps. [4c001a8]
    - [x] Modify the map output UI container to allow two Leaflet outputs to be rendered side-by-side.
- [x] Task: Conductor - User Manual Verification 'Modify UI for Residual Mapping' (Protocol in workflow.md)

## Phase 3: Update Server Logic for Simultaneous Map Rendering
- [x] Task: Write tests for server rendering logic. [a437e73]
    - [x] Write tests ensuring both residual maps are generated.
    - [x] Verify test fails initially.
- [x] Task: Render side-by-side residual maps. [a5587d0]
    - [x] Update the server logic to generate both residual maps simultaneously when requested.
    - [x] Send the two map objects to their respective side-by-side UI outputs.
- [x] Task: Synchronize map legends. [a5587d0]
    - [x] Ensure the color palettes for both residual maps use a standardized diverging scale (red for negative, blue for positive).
    - [x] Ensure the legends are properly attached to both maps.
- [x] Task: Conductor - User Manual Verification 'Update Server Logic for Simultaneous Map Rendering' (Protocol in workflow.md)