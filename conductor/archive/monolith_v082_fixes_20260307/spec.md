# Specification: Monolith v0.8.2 - Audit, Fixes & Map Layer Controls

## Overview
This track involves auditing and fixing issues reported in the v0.8.1 release, creating the `monolith_0.8.2.R` environment. The focus is on fixing the XYZ surface plot modeling error, adding dynamic base map layer selection to the Map Viewer, resolving a UI freeze issue when uploading specific variable list files, and conducting a general scientific and algorithmic audit of the descriptive/exploratory suite.

## Functional Requirements

### 1. Preparation & Backups (Crucial)
- Backup `monolith_0.8.1.R` and all its associated helper files to the `backups/` directory before any development begins.
- Create `monolith_0.8.2.R` and copy/rename all helper files accordingly to ensure a clean slate.

### 2. XYZ Surface Plot Fix
- Investigate the "model fitting failed" error within the XYZ Surface Plot module (located in the descriptive/exploratory suite).
- Apply an in-place fix to the fitting algorithms (`lm`, `loess`, `gam`, `tps`, etc.) or data handling pipelines to ensure the surface grids render successfully without failing silently.

### 3. Map Viewer Base Layer Selection
- Add a UI dropdown to the Map Viewer section allowing users to switch the Leaflet base map tiles.
- Incorporate existing base map tile providers used in the themes (e.g., Esri.WorldImagery, CartoDB.DarkMatter, OpenTopoMap, OpenStreetMap/Light versions).

### 4. Performance Issue Resolution (`samp_var_list_2.xlsx`)
- Debug the file parsing logic that handles variable list uploads.
- Identify the bottleneck or parsing error that causes the Shiny UI to dim/freeze when `samp_var_list_2.xlsx` is uploaded.
- Apply a fix to ensure the file is parsed efficiently and the UI remains responsive.

### 5. Scientific & Algorithmic Audit
- Conduct a review of the descriptive and exploratory suite plots and metrics.
- Ensure that the algorithms correctly calculate descriptive stats, correlation matrices, and groupings.

## Non-Functional Requirements
- **Tech Stack:** R and Shiny. Authorized to install or import new R/Python libraries if strictly necessary to resolve the issues.
- **Independence:** The `monolith_0.8.1.R` code must remain intact and unchanged; all work must occur within `0.8.2`.

## Acceptance Criteria
- [ ] A clean backup of v0.8.1 is created, and v0.8.2 files are scaffolded.
- [ ] The XYZ Surface Plot renders correctly for all selected fit types without throwing a "model fitting failed" error.
- [ ] The Map Viewer includes a functional dropdown to switch between at least 4 distinct base map styles (Dark, Light, Topo, Satellite).
- [ ] Uploading `samp_var_list_2.xlsx` does not freeze the application.
- [ ] The descriptive and exploratory suite calculations and plots have been audited and function as scientifically intended.