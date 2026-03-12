# Implementation Plan: Small controls and fixes (monolith_ver_0.8.9.R to 0.9.0.R)

## Phase 1: File Backup and Version Bump
- [x] Task: Back up `monolith_ver_0.8.9.R` and all 0.8.9.R helper scripts to the `backups/` directory. b576726
- [x] Task: Copy `monolith_ver_0.8.9.R` and its helpers to `monolith_ver_0.9.0.R` and update version headers in code. 17edfd5
- [x] Task: Conductor - User Manual Verification 'Phase 1: File Backup and Version Bump' (Protocol in workflow.md)

## Phase 2: Moran's I NA Handling
- [x] Task: Write Failing Tests - Write tests that mock a pure nugget effect triggering a Moran's I `NA` output to verify UI handles it poorly. a317a12
- [x] Task: Implement Fix - Modify the metric calculation and UI display layers to catch `NA` gracefully and display "No Spatial Structure Detected" with a tooltip instead of raw NA. a9e3c7e
- [x] Task: Conductor - User Manual Verification 'Phase 2: Moran's I NA Handling' (Protocol in workflow.md)

## Phase 3: Resolution Calculation Fix
- [x] Task: Write Failing Tests - Add test data mimicking `samp_data_2.xlsx` CRS (degrees) and assert that UI reports resolution strictly in meters. 6db6b96
- [ ] Task: Implement Fix - Add a strict EPSG heuristic or `sf::st_transform` logic inside the resolution calculation function to accurately convert degree measurements to meters.
- [ ] Task: Conductor - User Manual Verification 'Phase 3: Resolution Calculation Fix' (Protocol in workflow.md)

## Phase 4: Refresh Button Fix
- [ ] Task: Write Failing Tests - Create an integration test simulating the Refresh button click to assert geostatistical re-calculation functions are NOT called.
- [ ] Task: Implement Fix - Detach the Refresh action from the heavy re-render logic, replacing it with a leaflet map visual redraw (e.g. invalidate size or basic redraw function in Shiny).
- [ ] Task: Conductor - User Manual Verification 'Phase 4: Refresh Button Fix' (Protocol in workflow.md)