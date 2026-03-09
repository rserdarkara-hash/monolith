# Specification: Track v2.4 to v2.5 Updates

## Overview
This track focuses on incrementally upgrading the Monolith Spatial Analysis Dashboard from `v2.4` to `v2.5`. The primary objectives are to resolve minor UI/display issues, drastically improve the capabilities and usability of the Export Panel Styler for publication-ready figures, and correctly handle and report dynamic spatial resolutions when multiple localities are processed simultaneously. As an architectural mandate, the v2.4 files will be backed up untouched, and development will proceed on new v2.5 files.

## Functional Requirements

### 1. Pre-Implementation Workflow (Mandatory)
*   **Backup Protocol:** All existing `app_v2.4.R` and its corresponding helper scripts (e.g., `spatial_helpers_v2.4.R`, `theme_helpers_v2.4.R`, `ui_helpers_v2.4.R`) must be duplicated into the `backups` folder.
*   **Version Bump:** The active working files must be renamed to reflect version `2.5` (e.g., `app_v2.5.R`) before any modifications are made. All core functionality and styling from v2.4 must be perfectly preserved.

### 2. Map Viewer Improvements (1.1.A)
*   **Title Cleanup:** Remove the `[] ` placeholder brackets from all Map Viewer titles (e.g., "[] Total N (%) - Actual Data" becomes "Total N (%) - Actual Data").
*   **Map Colour Dropdown Previews:** Restore/fix color swatches next to labels in the map colour dropdown menu so that they accurately preview the theme colors (e.g., green shades for greens scale) rather than showing gray or generic colors.

### 3. Locality Resolution Reporting (1.1.B)
*   **Dynamic Tracking:** When multiple localities are processed simultaneously, the spatial engine must track and retain the specific calculated resolution (grid size) for *each* locality.
*   **UI Display:** Display a comprehensive list/table of these calculated resolutions within the Spatial Engine UI tab.
*   **Map Viewer Overlay:** Add a toggleable option in the Map Viewer to display the calculated resolution for the selected locality directly on the map overlay.

### 4. Export Panel & Styler UI Enhancements (1.1.C)
*   **Title & Legend De-duplication (C.1):** Refactor the exported map plots to ensure there is exactly *one* clear title per map. The legends must be stripped of redundant titles to maximize layout space.
*   **Tabbed Interface for Advanced Settings (C.2, C.3):** Reorganize the Styler Modal UI into a tabbed interface. Basic settings will be on the primary tab, while advanced text size sliders, text orientation options, and plot margins will be moved to an "Advanced" tab, hidden by default.
*   **Functional Plot Margins (C.4):** Fix the plot margin controls to ensure they demonstrably adjust the outer padding (Top, Right, Bottom, Left) of the exported figure.
*   **New Publication Modifiers:** Research and integrate additional useful graphical modifiers (e.g., DPI overrides, specific facet wrap controls, advanced color scales) suitable for publication-ready figures within the Advanced tab.
*   **Save/Load Default Settings (C.5):** Implement a dual-persistence system for styler configurations:
    1.  **Local Storage:** Automatically save user preferences to browser local storage so they persist across sessions.
    2.  **File-Based Config:** Add UI buttons to Download and Upload styler configuration settings as a local file (e.g., JSON/RDS).
*   **WYSIWYG Fidelity Assurance (C.6):** Ensure that the on-screen preview perfectly matches the stylistic output, sizing, aspect ratio, and quality of the final exported image file.

## Non-Functional Requirements
*   **Scientific Integrity:** None of the geostatistical models, parameter optimization functions, or core data structures may be altered.
*   **Tech Stack Extensibility:** Authorized to introduce new R or Python packages specifically for advanced plot styling and configuration management if the current stack is insufficient.

## Acceptance Criteria
- [ ] `app_v2.4.R` and its helpers are backed up and untampered; development occurs on new `2.5` files.
- [ ] Map Viewer titles display cleanly without brackets.
- [ ] Map colour dropdowns show appropriate preview swatches matching the active theme.
- [ ] When processing >1 locality, the resolution for *each* locality is visible in the Spatial tab and toggleable on the Map Viewer overlay.
- [ ] Exported plots contain a single main title and title-less legends.
- [ ] The Styler UI uses tabs, hiding advanced options (sizes, orientation, margins) by default.
- [ ] Plot margins can be actively adjusted and visually confirmed.
- [ ] Styler settings persist via browser local storage and can be exported/imported as a file.
- [ ] The generated export file precisely matches the UI preview.

## Out of Scope
*   Modifying the core kriging or machine learning algorithms.
*   Adding new spatial models.