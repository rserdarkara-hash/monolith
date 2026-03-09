# Specification: Spatial Analysis Dashboard Fixes (v2.4)

## Overview
This track addresses several critical bugs and feature improvements identified in version 2.3 of the Spatial Soil Analysis Dashboard. The primary goal is to ensure visual and mathematical consistency between the interactive UI, the geostatistical interpolation engine, and the final exported reports. 

## Functional Requirements

### 1. Spatial Logic & Interpolation
- **Comparison Mode Scaling:** Fix the "Match scales" functionality to ensure color scales are perfectly synchronized across all maps in the Comparison UI.
- **Iron (Fe) Interpolation Audit:** Diagnose and fix the "invalid surface" issue specifically occurring with Ordinary Kriging for Iron (Fe) data, ensuring it matches the quality of other interpolation methods.

### 2. Export Engine & Visual Fidelity
- **Export Drift Elimination:** Synchronize all Export Panel edits with the final generated files (PNG, TIFF, PDF, JPEG).
- **WYSIWYG Preview:** Align the preview container's aspect ratio, DPI, and font-to-figure scaling logic with the export engine (2.5x typographical calibration).
- **Legend Optimization:** Remove redundant titles from individual map legends in tiled exports to allow for cleaner multi-map layouts.

### 3. Residual Validation & Mapping
- **Residual Audit:** Clarify and document whether current residuals represent interpolated points or parameter prediction errors.
- **Extended Residual Maps:** 
  - Add dedicated maps for predicted value residuals.
  - Add difference maps showing the delta between actual and predicted values for the uploaded dataset.

## Non-Functional Requirements
- **Version Integrity:** All changes must be implemented in a new `app_v2.4.R` file and associated `*_v2.4.R` helpers, preserving the `app_v2.3.R` ecosystem.
- **Performance:** Maintain the current asynchronous processing performance for interpolation and export.
- **Theme Consistency:** Ensure all fixes respect the "v2.2_theme" dynamic engine.

## Acceptance Criteria
- [ ] Comparison maps show identical scales when "Match scales" is enabled.
- [ ] Iron (Fe) Ordinary Kriging produces a valid, non-distorted raster surface.
- [ ] Exported files are visually identical to the UI preview (font size, layout, scaling).
- [ ] New residual maps are available in the UI and correctly reflect prediction error.
- [ ] Application successfully launches from `app_v2.4.R` with all features intact.

## Out of Scope
- Major architectural changes to the `tidymodels` or `gstat` backends (beyond specific bug fixes).
- Introduction of new machine learning models (unless required to fix Iron interpolation).