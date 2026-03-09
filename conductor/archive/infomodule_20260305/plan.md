# Implementation Plan: Information & Documentation Module (v2.6)

## Phase 1: Environment Setup & File Preparation
- [x] Task: Create backups of current v2.5 files into the `backups/pre_v2.6_docs` directory (`app_v2.5.R`, `spatial_helpers_v2.5.R`, `theme_helpers_v2.5.R`, `ui_helpers_v2.5.R`). 949578c
- [x] Task: Initialize new v2.6 application files (`app_v2.6.R`, `spatial_helpers_v2.6.R`, `theme_helpers_v2.6.R`, `ui_helpers_v2.6.R`) by copying the contents of the v2.5 files. d14ac47
- [x] Task: Create a new `docs/` directory in the project root to store the external Markdown files. 39b44d8
- [x] Task: Set up the base `docs/scientific_guide.md` and `docs/ui_ux_guide.md` files with empty structures. 39b44d8
- [x] Task: Update the `monolith_pre_release.Rproj` or main script sourcing logic (if any) to point to the new v2.6 files. (Done in app_v2.6.R)
- [x] Task: Conductor - User Manual Verification 'Phase 1: Environment Setup & File Preparation' (Protocol in workflow.md)

## Phase 2: Documentation Content Generation
- [x] Task: Draft the "Scientific & Analytical Methodology Guide" in `docs/scientific_guide.md` covering Spatial Interpolation Engines, Variogram Optimization, Validation Diagnostics (with agronomical examples and references), and Residual Analysis. 134cab4
- [x] Task: Draft the "Step-by-Step UI/UX User Guide" in `docs/ui_ux_guide.md` covering Data Setup, Spatial Engine & Tuning, Borders & Styling, and Export Styler workflows. 134cab4
- [x] Task: Conductor - User Manual Verification 'Phase 2: Documentation Content Generation' (Protocol in workflow.md)

## Phase 3: UI Architecture - Sliding Drawer
- [x] Task: Write tests in `tests/testthat/test-v2.6-docs-ui.R` verifying the presence and initial state of the sliding drawer UI components. 8ba0cd7
- [x] Task: Update `ui_helpers_v2.6.R` to implement a sliding drawer component (e.g., using `shinyBS::bsTooltip`, `shinyjs`, or custom HTML/CSS for a sliding panel).
- [x] Task: Integrate logic to dynamically read and parse `docs/scientific_guide.md` and `docs/ui_ux_guide.md` into HTML inside the sliding drawer using `markdown` or `knitr`.
- [x] Task: Add a toggle button (e.g., "Documentation" or "Info") to the main application header or sidebar in `app_v2.6.R` to open/close the sliding drawer.
- [x] Task: Run tests and ensure the sliding drawer renders correctly and parses the Markdown files without errors.
- [x] Task: Conductor - User Manual Verification 'Phase 3: UI Architecture - Sliding Drawer' (Protocol in workflow.md)

## Phase 4: Contextual Help Popovers
- [x] Task: Write tests in `tests/testthat/test-v2.6-docs-ui.R` verifying the presence of "i" icon popovers next to critical inputs.
- [x] Task: Update `ui_helpers_v2.6.R` and `app_v2.6.R` to inject small "i" icon elements next to inputs like "Resolution Logic", "Boundary Type", and "Comparison Mode".
- [x] Task: Implement click-based popover logic (using `shinyBS::bsPopover` or similar) attached to the "i" icons, pulling specific text snippets for each input.
- [x] Task: Run tests and verify popovers trigger correctly on click and stay open until dismissed.
- [x] Task: Conductor - User Manual Verification 'Phase 4: Contextual Help Popovers' (Protocol in workflow.md)

## Phase 5: Styling Integration & Final Review
- [x] Task: Update `theme_helpers_v2.6.R` to ensure the sliding drawer and popovers correctly inherit the active CSS theme (background colors, typography, border styles).
- [x] Task: Verify that switching themes dynamically updates the newly added documentation UI components.
- [x] Task: Perform a complete manual run-through of the application to ensure the presence of the new documentation module does not break any existing spatial analysis or export workflows.
- [x] Task: Conductor - User Manual Verification 'Phase 5: Styling Integration & Final Review' (Protocol in workflow.md)