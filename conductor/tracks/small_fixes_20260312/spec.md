# Specification: Small controls and fixes (monolith_ver_0.8.9.R to 0.9.0.R)

## 1. Overview
The purpose of this track is to implement several critical bug fixes and small controls in the Monolith Spatial Analysis Dashboard while bumping the version from 0.8.9.R to 0.9.0.R. The fixes target issues with the Moran's I metric, the resolution calculation in degrees vs meters, and the map refresh functionality.

## 2. Functional Requirements
1.  **Backup Protocol:**
    *   Before making any modifications, back up `monolith_ver_0.8.9.R` and its associated helper files into the `backups/` directory.
    *   Initialize the new working files as `monolith_ver_0.9.0.R` and corresponding helper copies.

2.  **Moran's I Evaluation:**
    *   Investigate why Moran's I sometimes evaluates to `NA`.
    *   If the calculation logic is correct (e.g., due to pure nugget effect or lack of spatial structure), gracefully handle the `NA` output in the UI.
    *   Provide a clear tooltip or message explaining the `NA` (e.g., "No Spatial Structure Detected") rather than showing an error.

3.  **Resolution Calculation (Meters):**
    *   Fix the resolution reporting issue observed in `samp_data_2.xlsx` where resolution in degrees is not properly converted or reported in meters.
    *   Implement a robust conversion strategy (using EPSG transformations or rigorous heuristics) so the UI consistently reports resolution in meters, regardless of the input data's projection.

4.  **Refresh Button Behavior:**
    *   Modify the Refresh button on the spatial map panel to perform a **Visual Reload Only**.
    *   The button should reload the leaflet map widget with the already prepared map layers without recalculating any geostatistical models.

## 3. Non-Functional Requirements
- Maintain backward compatibility with the existing functionality, science stack, and UI/UX defined in `monolith_ver_0.8.9.R`.
- No new external R dependencies outside of the approved Tech Stack should be strictly required unless necessary to fix the resolution CRS issue (e.g., using `sf` which is already approved).

## 4. Acceptance Criteria
- [ ] `monolith_ver_0.8.9.R` and helpers are backed up; new `0.9.0` files are created.
- [ ] Moran's I gracefully reports `NA` with an explanatory tooltip instead of breaking or showing raw NA when no spatial structure exists.
- [ ] Both `samp_data_1.xlsx` and `samp_data_2.xlsx` correctly report cell resolution in meters.
- [ ] Clicking "Refresh" on the map reloads the visual display instantly without triggering geostatistical recalculations.

## 5. Out of Scope
- Major architectural rewrites.
- Adding new predictive algorithms.