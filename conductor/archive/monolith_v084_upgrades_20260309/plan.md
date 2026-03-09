# Implementation Plan: Monolith v0.8.4 Upgrades and Fixes

## Phase 1: Environment Setup & Backup
- [x] Task: Create backups of `monolith_0.8.3.R` and all its associated helper scripts (e.g., `spatial_helpers_0.8.3.R`, `ui_helpers_0.8.3.R`, etc.) to the `backups` directory.
- [x] Task: Create new files for version 0.8.4 (`monolith_0.8.4.R` and corresponding helper scripts) by copying the 0.8.3 versions.
- [x] Task: Update the main UI/server sourcing logic to point to the new 0.8.4 helper scripts.
- [x] Task: Conductor - User Manual Verification 'Phase 1: Environment Setup & Backup' (Protocol in workflow.md)

## Phase 2: Map Viewer Panel Adjustments (Quick Wins)
- [x] Task: In `ui_helpers_0.8.4.R` (or equivalent UI file), remove the "Base Map" text next to the map style dropdown.
- [x] Task: Reposition the map style dropdown to be adjacent to the "Show Points" and "Show Res" tickmarks.
- [x] Task: Add a new "Refresh Map Area" actionButton next to "Pop-up Settings" and "Quick Export". Ensure visual styling matches.
- [x] Task: In `monolith_0.8.4.R` (Server logic), add an `observeEvent` for the "Refresh Map Area" button to trigger Leaflet to redraw/reapply the active tile layer without recalculating metrics.
- [x] Task: Conductor - User Manual Verification 'Phase 2: Map Viewer Panel Adjustments' (Protocol in workflow.md)

## Phase 3: User Experience - Map Generation Progress & Reveal
- [x] Task: Implement the detailed progress bar overlay for the MapViewer panel. Hook it into the spatial interpolation functions (Ordinary Kriging, RK, RFK, etc.) to report specific algorithm steps.
- [x] Task: Create the "Click here to view maps and enable scientific analysis" blue actionButton overlay for the MapViewer panel.
- [x] Task: Implement server logic to hide the map and analysis metrics initially, show the blue button once background processing is complete, and reveal the map/metrics when the button is clicked. Ensure the descriptive suite remains usable during background generation.
- [x] Task: Conductor - User Manual Verification 'Phase 3: User Experience - Map Generation Progress & Reveal' (Protocol in workflow.md)

## Phase 4: Descriptive and Exploratory Suite Upgrades
- [x] Task: Update the UI for Box, Violin, and Sina plots to include checkboxes for ANOVA, Duncan's, Tukey's, and HSD tests, ensuring mutually exclusive selection.
- [x] Task: Add a UI toggle for significance letter placement ("Top of Plot" vs "Above Data").
- [x] Task: Implement statistical logic to calculate the selected tests. Add dynamic prioritization logic (F-test first for >2 groups, F-test only for 2 groups).
- [x] Task: Integrate significance letters into the `ggplot2`/`plotly` rendering logic based on the user's placement preference.
- [x] Task: Add secondary variable selection UI specifically for Sina-style plots.
- [x] Task: Add the checkbox for F-test on secondary variables and implement the logic to allow F-test between secondary variables (alongside or instead of primary group differences).
- [x] Task: Conductor - User Manual Verification 'Phase 4: Descriptive and Exploratory Suite Upgrades' (Protocol in workflow.md)

## Phase 5: Variable Selection Improvements
- [x] Task: Implement UI state persistence for variable selections across different descriptive analysis styles (Box plot, Violin plot, etc.).
- [x] Task: Add a "Clear" button to the variable list selection UI and implement its reset logic.
- [x] Task: Conductor - User Manual Verification 'Phase 5: Variable Selection Improvements' (Protocol in workflow.md)

## Phase 6: Final Review & Integration
- [x] Task: Ensure the `monolith_0.8.4.R` runs flawlessly with all new features.
- [x] Task: Verify that the old version 0.8.3 is perfectly intact and operational if needed.
- [x] Task: Conductor - User Manual Verification 'Phase 6: Final Review & Integration' (Protocol in workflow.md)