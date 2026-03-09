# Specification: Interactive Radar Plot Fix

## Overview
This track addresses a specific bug in the Monolith application where expanding the radar map to "plotly" (interactive) mode triggers an "attempt to apply non-function" error. The radar plot renders correctly in static mode, suggesting the issue lies in the conversion to interactivity via `plotly::ggplotly()`, possibly due to unsupported layers like `coord_radar()` or a minor coding error. As part of this fix, the application and its helpers will be version-bumped from 0.8.2 to 0.8.3.

## Functional Requirements
- **Radar Plot Interactivity:** The radar plot must successfully render in interactive mode without throwing an "attempt to apply non-function" error.
- **Version Bump (0.8.3):**
  - Create `monolith_0.8.3.R` by copying `monolith_0.8.2.R`.
  - Create `spatial_helpers_0.8.3.R`, `theme_helpers_0.8.3.R`, and `ui_helpers_0.8.3.R` by copying their respective 0.8.2 versions (which are in the `improvements/` folder).
  - Ensure `monolith_0.8.3.R` correctly sources the new 0.8.3 helper files.
  - Back up all 0.8.2 files and the current application state to the `backups/` folder.

## Non-Functional Requirements
- **Preservation of Existing Functions:** All other graph types work perfectly and must remain unaffected.
- **Targeted Code Change:** The fix should be targeted specifically to the interactive radar plot rendering. If the root cause analysis indicates a widespread issue, the scope will be expanded, but otherwise, do not alter other graph types.
- **Scientific Integrity:** The style, content, and scientific accuracy of the Monolith 0.8.2 app must be strictly preserved.

## Acceptance Criteria
- [ ] A new `monolith_0.8.3.R` and its 0.8.3 helpers exist and are correctly linked.
- [ ] A backup of the `0.8.2` state exists in the `backups/` folder.
- [ ] Switching the radar plot to interactive mode (plotly) successfully displays the plot without throwing an error.
- [ ] Hover interactions and zooming work correctly on the interactive radar plot.
- [ ] Other plot types remain functional in both static and interactive modes.

## Out of Scope
- Modifying the static version of the radar plot (unless necessary to fix the plotly conversion).
- Refactoring the broader Scientific Analytics Engine beyond the targeted fix.