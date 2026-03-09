# Implementation Plan: Banner Integration (v0.8.7)

## Phase 1: Preparation and Versioning
- [x] Task: Back up `monolith_0.8.6.R` and its helpers (`spatial_helpers_0.8.6.R`, `theme_helpers_0.8.6.R`, `ui_helpers_0.8.6.R`) to `backups/pre_banner_addition_20260309/`. dafc944
- [x] Task: Create `monolith_0.8.7.R` and its helpers (`spatial_helpers_0.8.7.R`, `theme_helpers_0.8.7.R`, `ui_helpers_0.8.7.R`) by copying the 0.8.6 versions. dafc944
- [x] Task: Update version numbers and helper file references within `monolith_0.8.7.R`. dafc944
- [x] Task: Conductor - User Manual Verification 'Preparation' (Protocol in workflow.md) dafc944

## Phase 2: Banner UI Implementation (TDD)
- [x] Task: Write a failing test in `tests/testthat/test-ui-header.R` to check for the presence of an `img` tag with `src='banner.png'` and the absence of the 'Monolith' title text. 4826db1
- [x] Task: Locate the `titlePanel` or header `div` in `monolith_0.8.7.R` and replace the text title with the banner image. 4826db1
- [x] Task: Apply CSS for left-alignment (`float: left` or flexbox) and proportional scaling (`max-width: 100%`, `height: auto`). 4826db1
- [x] Task: Run tests and ensure they pass (Green Phase). 4826db1
- [x] Task: Implement server logic for `about_btn` to display version and project information in a modal. 1762834
- [x] Task: Conductor - User Manual Verification 'Banner UI' (Protocol in workflow.md) 4826db1

## Phase 3: Theme Integration & Adaptive Styling
- [x] Task: Update `theme_helpers_0.8.7.R` to include CSS rules for the banner container that adjust based on the active theme (e.g., adding a subtle drop shadow in Light mode and a thin border in Dark mode). 91e0ef8
- [x] Task: Implement a 'Styling Alternative' toggle or logic to ensure the banner looks integrated in all 4+ themes. 91e0ef8
- [x] Task: Verify theme-switching behavior manually to ensure the banner style updates correctly. 91e0ef8
- [x] Task: Conductor - User Manual Verification 'Theme Integration' (Protocol in workflow.md) 91e0ef8

## Phase 4: Final Validation & Regression
- [x] Task: Verify the banner's responsive scaling across multiple screen widths (Desktop, Tablet, Mobile emulation). d19081d
- [x] Task: Perform a full regression check of the 'Governing Factors' (v0.8.5) and 'Map Viewer' (v0.8.6) features. d19081d
- [x] Task: Conductor - User Manual Verification 'Final Validation' (Protocol in workflow.md) d19081d
