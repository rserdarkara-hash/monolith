# Implementation Plan: Dynamic Theme Preview

## Phase 1: File Setup and Theme Engine Creation
- [x] Task: Backup existing `app_v2.2.R` and helper files to the `backups/` directory. [68cf9c0]
- [x] Task: Create new application files (`app_v2.2_theme.R`, `theme_helpers.R`) ensuring the original logic is preserved. [68cf9c0]
- [x] Task: Write tests for theme generation logic. [cab30f0]
- [x] Task: Implement `theme_helpers.R` to define the 10 distinct `fresh` theme objects. [cab30f0]
- [x] Task: Define the `manual_style` overrides and map tile associations for each theme. [cab30f0]
- [x] Task: Conductor - User Manual Verification 'Phase 1: File Setup and Theme Engine Creation' (Protocol in workflow.md) [checkpoint: f98631f]

## Phase 2: UI Implementation and Persistence
- [x] Task: Write tests for UI theme switcher and persistence logic (using `shinyjs` or similar). [2853785]
- [x] Task: Add a theme selection dropdown to the UI (`app_v2.2_theme.R`). [2853785]
- [x] Task: Implement the logic to save and load the selected theme from the browser. [2853785]
- [x] Task: Conductor - User Manual Verification 'Phase 2: UI Implementation and Persistence' (Protocol in workflow.md) [checkpoint: 721e307]

## Phase 3: Dynamic Rendering Integration
- [x] Task: Write tests for dynamic map updates and UI syncing. [a1027cb]
- [x] Task: Connect the theme selector to the `fresh::use_theme` component in the UI. [a1027cb]
- [x] Task: Implement `leafletProxy` observers to update the map base tiles without full re-render. [a1027cb]
- [x] Task: Ensure `manual_style` HTML/CSS overrides update reactively when a new theme is selected. [a1027cb]
- [x] Task: Conductor - User Manual Verification 'Phase 3: Dynamic Rendering Integration' (Protocol in workflow.md) [checkpoint: c34fbf3]