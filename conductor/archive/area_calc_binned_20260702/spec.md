# Specification: Area Coverage Calculations for Binned and Agronomical Styling

## Overview
This track addresses the area coverage (hectare sizes) calculations and their visibility when styling options are switched between **Continuous**, **Binned (5)**, and **Agronomical**.
Currently:
1. Area Coverage tables are only displayed when styling is set to `Agronomical`.
2. When the user switches to `Binned (5)`, no area calculations are shown or exported.
3. The leaflet map styling and ggplot export plots for binned data calculate breaks dynamically but do not synchronize with the area calculation engine.
4. If the agronomical classification algorithm (Jenks or K-means) returns duplicate breaks, the classification matrix contains `NA` values, causing errors in `classify()` and subsequent area calculations.

This track will:
1. Unify the classification engine to support both `Agronomical` and `Binned (5)` styling modes, ensuring that the leaflet map, ggplot preview/export, and Area Coverage tables all use identical break points and calculations.
2. Ensure mathematical and scientific correctness of all area (Ha) calculations across both styling options.
3. Fix the potential boundary error in Jenks and K-means when non-unique breaks are returned.

## Functional Requirements
1. **Unified Classification Engine**:
   - Refactor/extend the classification reactive logic (`classification_params()`) to handle both `"agro"` and `"bin"` color styles.
   - For `"bin"`, calculate 5 equal-width intervals based on the active/predicted raster values (or joint scale if `input$match_scales` is checked).
   - Maintain the existing agronomical classification algorithm support (supervised/Jenks/K-means) when styling is `"agro"`.
   - Safely handle cases where Jenks, K-means, or equal-width binning returns duplicate breaks by keeping only unique breaks and adjusting the number of classes `n_c` dynamically.

2. **Map & Preview Parity**:
   - Update Leaflet map rendering to use the exact computed breaks for `"bin"` styling.
   - Update ggplot preview rendering in `ui_helpers_0.9.8b.R` (`generate_base_plot`) to use the classified raster values and `scale_fill_manual` for `"bin"`, matching `"agro"` style but using 5 bins and the selected palette.
   - Update ggplot export plots in `monolith_ver_0.9.8b.R` to use the unified classification logic for `"bin"`.

3. **Area Coverage Table Visibility & Logic**:
   - Render the Area Coverage tables when styling is either `"agro"` or `"bin"`.
   - In `"bin"` mode, label the rows in the tables with their interval ranges (e.g. `< 12`, `12 - 24`, ..., `> 48`), matching the map legend labels.
   - Hide the Area Coverage tables and outputs when styling is set to `"cont"` (Continuous).

4. **Unified Export Registry**:
   - Ensure the binned Area Coverage tables are registered and exported correctly in the export registry.

## Acceptance Criteria
1. Switching to **Binned (5)** styling option displays the Area Coverage tables.
2. In **Binned (5)** styling option, the calculated hectares (Ha) in the Area Coverage table correspond to the areas shown on the map and ggplot export.
3. The row labels of the Area Coverage table in **Binned (5)** mode display correct numeric ranges (e.g., `< 12`, `12 - 24`, ..., `> 48`).
4. Switching to **Continuous** styling hides the Area Coverage tables.
5. Jenks and K-means classifications do not crash or produce NA values in classification matrix when duplicate breaks are returned.
6. All automated unit tests run and pass.
