# Specification: Monolith v0.8.4 Upgrades and Fixes

## Overview
This track focuses on creating version 0.8.4 of the Monolith Spatial Soil Analysis Dashboard from version 0.8.3. The updates involve expanding the Descriptive and Exploratory Suite with advanced statistical tests (ANOVA, Duncan's, Tukey's, HSD, F-tests), improving the User Experience for Map Generation with a new reveal button and a detailed progress bar, and making adjustments to the Map Viewer Panel.

## Functional Requirements

### 1. Descriptive and Exploratory Suite Upgrades
*   **A1. Advanced Statistical Tests for Plots:**
    *   Box plots, violin plots, and Sina-style plots must dynamically support ANOVA with Duncan's, Tukey's, and HSD post-hoc tests.
    *   Provide individual tickmarks in the UI to select each test.
    *   Selecting one test must automatically disable the other two options.
    *   The selected test's significance letters must be dynamically displayed over the groups in the plots.
    *   **User Option for Letters:** The UI must provide a toggle for the user to choose the placement of significance letters: either "Top of Plot" (aligned at the top margin) or "Above Data" (directly above each box/violin).
*   **A1.1. Dynamic Test Prioritization:**
    *   If the selected grouping results in more than two groups, the system must prioritize the F-test followed by the ANOVA test.
    *   If only two groups are present, only the F-test should be applied and displayed.
*   **A2. Sina Plot Secondary Variables:**
    *   Sina-style plots must include an option to select a secondary variable.
*   **A3. Secondary Variable F-test:**
    *   When a secondary variable is active in a Sina plot, a new tickmark option should appear.
    *   If enabled, the system must perform an F-test between these secondary variables. The user must be able to choose whether to calculate differences between the primary groups, perform the F-test between secondary variables, or display both sets of statistical comparisons simultaneously if chosen.
*   **A4. Variable List Persistence:**
    *   When a user registers a specific list of variables for an analysis style within this suite, that list must persist even if the analysis style (e.g., switching from Box Plot to Violin Plot) is changed.
*   **A5. Clear Variables Button:**
    *   Add a small "Clear" button adjacent to the variable list selection area within the descriptive suite to easily reset selections.

### 2. User Experience: Map Generation and Results
*   **B1. Map Reveal Button:**
    *   When spatial interpolation and map generation processes are complete and ready for the MapViewer, display a modern blue button overlaying the panel with the text: "Click here to view maps and enable scientific analysis."
    *   Clicking this button will reveal the pre-calculated maps and unlock/make available the resulting scientific analysis metrics.
    *   The button operates as a "Reveal Only" mechanism, appearing only *after* background processing has finished.
*   **B2. Detailed Progress Bar:**
    *   Implement a modern, detailed progress bar displayed as a prominent overlay over the MapViewer panel during the map generation process.
    *   The progress bar must accurately reflect real-time backend processes, specifically displaying technical details of the Monolith algorithms being executed (e.g., "Fitting Variogram...", "Running Ordinary Kriging...").
    *   The descriptive and exploratory suite must remain independent of this loading process and continue to be active and usable while maps are generating.

### 3. Map Viewer Panel Adjustments
*   **C1. UI Reorganization:**
    *   Remove the bold "Base Map" text label next to the map style dropdown.
    *   Reposition the map style dropdown to be directly adjacent to the "Show Points" and "Show Res" tickmarks.
*   **C2. Refresh Map Area Button:**
    *   Add a small "Refresh Map Area" button next to the "Pop-up Settings" and "Quick Export" buttons.
    *   It must match the visual style of these existing buttons.
    *   Clicking this button will manually refresh the Leaflet area to resolve potential UI lag (e.g., gray maps). It should do this by re-triggering the current tile layer or style application to force Leaflet to redraw the already prepared maps without recalculating them.

## Non-Functional Requirements
*   **Preservation of Existing Logic:** Keeping all style, content, scientific, and automated functions of `monolith_0.8.3.R` is crucial.
*   **File Backups:** Never overwrite the existing app file (`monolith_0.8.3.R`). Always copy it and its helper files to produce the new version (`monolith_0.8.4.R`) and back them up to the `backups` folder before making any changes.
*   **Library Expansion:** The current tech stack is allowed to be expanded using R or Python libraries if necessary to meet the new requirements.

## Acceptance Criteria
*   Box/Violin/Sina plots correctly display significance letters from ANOVA, Duncan's, Tukey's, or HSD based on user selection, with an option to place letters at the top or above the data.
*   F-test correctly overrides ANOVA when only 2 groups are present.
*   Sina plots properly display secondary variables and can perform F-tests between them (and/or alongside primary group differences).
*   Variable selections persist across different descriptive analysis styles.
*   The blue "Click here to view maps..." button correctly appears *after* map generation and reveals the maps when clicked.
*   The detailed progress bar overlays the map panel and displays algorithmic steps during map generation.
*   The Map Viewer panel UI is updated without the "Base Map" text and with the new dropdown position.
*   The "Refresh Map Area" button successfully redraws grayed-out Leaflet maps without re-running calculations.
*   All changes are implemented in a new file `monolith_0.8.4.R` alongside updated helper files, leaving v0.8.3 intact.

## Out of Scope
*   Modifications to core Kriging algorithms or spatial interpolation logic unless strictly required to support the new progress bar reporting.