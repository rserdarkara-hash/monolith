# Implementation Plan: Monolith v0.8.2 - Audit, Fixes & Map Layer Controls

## Phase 1: Environment Setup & Backups
- [x] Task: Create directory `backups/pre_v0.8.2_upgrade_<date>`.
- [x] Task: Backup `monolith_0.8.1.R` and all `improvements/*_0.8.1.R` files to the backup directory.
- [x] Task: Duplicate `0.8.1` files to create `monolith_0.8.2.R` and corresponding `improvements/*_0.8.2.R` helper files. bbbe82e
- [x] Task: Update the `source()` calls within `monolith_0.8.2.R` to point to the new `0.8.2` helper files.
- [x] Task: Conductor - User Manual Verification 'Phase 1: Environment Setup & Backups' (Protocol in workflow.md)

## Phase 2: Debug & Fix Variable List Upload (`samp_var_list_2.xlsx`)
- [x] Task: Write a failing unit test or reproduction script that triggers the parsing logic used for `samp_var_list_2.xlsx`. b34550d
- [x] Task: Investigate the data ingestion/parsing logic in `monolith_0.8.2.R` (or relevant helper) to identify the infinite loop, memory leak, or unhandled exception. (Identified: `var_mapping_ui` creates DOM nodes for all numeric columns instead of mapped ones)
- [x] Task: Implement a fix to gracefully parse the variable list without locking the Shiny UI. 405149b
- [x] Task: Conductor - User Manual Verification 'Phase 2: Debug & Fix Variable List Upload' (Protocol in workflow.md) 08db3c0

## Phase 3: XYZ Surface Plot Error Fix
- [x] Task: Write failing unit tests specifically targeting the `generate_advanced_plot` function for `xyz_surface` plot types across different fitting algorithms (`linear`, `loess`, `gam`, `tps`, `polynomial`). e4fee3a
- [x] Task: Debug the data formatting, formula generation, or prediction grid logic within the `xyz_surface` section of `improvements/ui_helpers_0.8.2.R`. e4fee3a
- [x] Task: Apply in-place fixes to ensure the models converge and predictions are mapped to the grid without triggering the "model fitting failed" fallback. e4fee3a
- [x] Task: Conductor - User Manual Verification 'Phase 3: XYZ Surface Plot Error Fix' (Protocol in workflow.md) e4fee3a

## Phase 4: Dynamic Map Viewer Base Layers
- [x] Task: Identify the base layer definitions currently hardcoded or tied to themes. cdaf8a1
- [x] Task: Add a `selectInput` to the Map Viewer UI in `monolith_0.8.2.R` for Base Map selection (incorporating Esri, DarkMatter, OpenTopoMap, etc.). cdaf8a1
- [x] Task: Update the `leaflet` rendering logic to reactively apply the chosen base map provider instead of relying solely on the theme default. cdaf8a1
- [x] Task: Conductor - User Manual Verification 'Phase 4: Dynamic Map Viewer Base Layers' (Protocol in workflow.md)

## Phase 5: Scientific & Algorithmic Audit
- [x] Task: Review the implementation of grouping, auto-discretization, and correlation calculations in `ui_helpers_0.8.2.R`.
- [x] Task: Scan the entire codebase (`monolith_0.8.2.R` and `spatial_helpers_0.8.2.R`) for `as.formula` and fix formula generation logic to safely wrap variable names in backticks to prevent errors with special characters (e.g., in CoKriging, Regression Kriging, etc.). f8f76f9
- [x] Task: Write additional unit tests for edge cases (e.g., handling missing values, zero-variance columns in correlation).
- [x] Task: Implement necessary corrections to ensure mathematical rigor and UI consistency across the descriptive and exploratory suite. f8f76f9
- [x] Task: Conductor - User Manual Verification 'Phase 5: Scientific & Algorithmic Audit' (Protocol in workflow.md) 18d1acf