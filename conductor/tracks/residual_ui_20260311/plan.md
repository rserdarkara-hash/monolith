# Implementation Plan: UI Content Optimization for Residual Mapping (v0.8.9)

## Phase 1: Setup and File Versioning
- [ ] Task: Create backups of monolith_ver_0.8.8.R and all its helper files.
    - [ ] Locate `monolith_ver_0.8.8.R` and all `_0.8.8.R` helper scripts (spatial, theme, ui).
    - [ ] Copy these files into the `backups/` directory.
- [ ] Task: Create new version 0.8.9 files.
    - [ ] Rename the main app file from `0.8.8.R` to `monolith_ver_0.8.9.R`.
    - [ ] Rename all corresponding helper files from `_0.8.8.R` to `_0.8.9.R`.
    - [ ] Update `source()` calls in `monolith_ver_0.8.9.R` to reference the new helper files.
- [ ] Task: Conductor - User Manual Verification 'Setup and File Versioning' (Protocol in workflow.md)

## Phase 2: Modify UI for Residual Mapping
- [ ] Task: Write tests for UI changes.
    - [ ] Add a test in `tests/testthat/` to ensure the dropdown menu is not rendered when mapping residuals and the text is added.
    - [ ] Verify test fails initially.
- [ ] Task: Remove dropdown menu from the residual mapping UI.
    - [ ] Locate the UI definition for the residual mapping dropdown.
    - [ ] Remove or conditionally hide the dropdown.
- [ ] Task: Add descriptive text to the control panel.
    - [ ] Inject explicit descriptive text regarding "Interpolated Delta" and "Interpolated Point Errors" into the control panel where the dropdown was previously located.
- [ ] Task: Update the layout to support side-by-side maps.
    - [ ] Modify the map output UI container to allow two Leaflet outputs to be rendered side-by-side.
- [ ] Task: Conductor - User Manual Verification 'Modify UI for Residual Mapping' (Protocol in workflow.md)

## Phase 3: Update Server Logic for Simultaneous Map Rendering
- [ ] Task: Write tests for server rendering logic.
    - [ ] Write tests ensuring both residual maps are generated.
    - [ ] Verify test fails initially.
- [ ] Task: Render side-by-side residual maps.
    - [ ] Update the server logic to generate both residual maps simultaneously when requested.
    - [ ] Send the two map objects to their respective side-by-side UI outputs.
- [ ] Task: Synchronize map legends.
    - [ ] Ensure the color palettes for both residual maps use a standardized diverging scale (red for negative, blue for positive).
    - [ ] Ensure the legends are properly attached to both maps.
- [ ] Task: Conductor - User Manual Verification 'Update Server Logic for Simultaneous Map Rendering' (Protocol in workflow.md)