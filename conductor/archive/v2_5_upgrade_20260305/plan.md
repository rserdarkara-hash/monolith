# Implementation Plan: Track v2.4 to v2.5 Updates

## Phase 1: Project Initialization & Backup [checkpoint: 824dcf6]
- [x] Task: Duplicate `app_v2.4.R`, `spatial_helpers_v2.4.R`, `theme_helpers_v2.4.R`, and `ui_helpers_v2.4.R` into the `backups/` directory. 7bc251c
- [x] Task: Rename active working files to `app_v2.5.R`, `spatial_helpers_v2.5.R`, `theme_helpers_v2.5.R`, and `ui_helpers_v2.5.R`. 380c64d
- [x] Task: Update internal source references and imports within `app_v2.5.R` to point to the new `v2.5` helpers. a23d499
- [x] Task: Conductor - User Manual Verification 'Project Initialization & Backup' (Protocol in workflow.md) 824dcf6

## Phase 2: UI Fixes & Locality Resolution Reporting [checkpoint: 3a73a23]
- [x] Task: Write Tests: Verify Map Viewer titles lack brackets and map color dropdowns render preview swatches. c9d983c
- [x] Task: Implement: Map Viewer Title Cleanup (remove `[] ` placeholder strings). c9d983c
- [x] Task: Implement: Fix Map Colour Dropdown Previews (restore color swatches next to labels in the UI). c9d983c
- [x] Task: Write Tests: Verify multiple localities track and report separate spatial resolutions. f3d3013
- [x] Task: Implement: Modify Spatial Engine logic to calculate, track, and retain resolutions per individual locality. f3d3013
- [x] Task: Implement: Display a reactive table/list of calculated resolutions in the Spatial Engine UI tab. f3d3013
- [x] Task: Implement: Add a toggleable Map Viewer overlay (e.g., via `leaflet` control) for the selected locality resolution. f3d3013
- [x] Task: Conductor - User Manual Verification 'UI Fixes & Locality Resolution Reporting' (Protocol in workflow.md) 3a73a23

## Phase 3: Export Panel Enhancements
- [x] Task: Write Tests: Verify export plot title rendering and legend de-duplication. 4db19a0
- [x] Task: Implement: Strip redundant titles from legends and ensure exactly one clear map title per exported plot. 4db19a0
- [x] Task: Write Tests: Verify Styler UI tabs, default hiding of advanced options, and margin functionality. 4db19a0
- [x] Task: Implement: Refactor Styler Modal UI to use a tabbed interface (Basic vs. Advanced). 4db19a0
- [x] Task: Implement: Move text size sliders, text orientation, and plot margins to the 'Advanced' tab (hidden by default). 4db19a0
- [x] Task: Implement: Fix Plot Margins controls to properly apply `ggplot2` padding/margins to exported figures. 95a6807
- [x] Task: Implement: Research and introduce new publication modifiers (e.g., DPI overrides, high-res scales) into the Advanced tab. 95a6807
- [x] Task: Conductor - User Manual Verification 'Export Panel Enhancements' (Protocol in workflow.md) 89c0868

## Phase 4: Configuration Persistence & Fidelity [checkpoint: 8c0aa56]
- [x] Task: Write Tests: Verify Styler config save/load functionality (Local Storage & File-based). aa61d54
- [x] Task: Implement: Enable Save/Load Styler settings via browser Local Storage (e.g., using `shinyjs` or custom JS bindings). aa61d54
- [x] Task: Implement: Create UI and logic for Download/Upload of Styler configurations (JSON/RDS format). aa61d54
- [x] Task: Implement: Final visual audit and adjustments to guarantee WYSIWYG fidelity between UI previews and exported image files. aa61d54
- [x] Task: Conductor - User Manual Verification 'Configuration Persistence & Fidelity' (Protocol in workflow.md) 8c0aa56