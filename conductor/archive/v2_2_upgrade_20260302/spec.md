# Specification: TPS Optimization Fix & v2.2 Upgrade

## 1. Overview
This track addresses a critical bug in the Thin Plate Spline (TPS) optimization where the lambda value is always selected as 0 in version 2.1. It aims to restore and ensure the scientific correctness of the TPS optimization logic from version 2.0 while adapting it to the concurrent architecture of v2.1. Additionally, this track introduces UI enhancements to display Moran's I values in the metrics table and allows filtering of correlation rank lists by customizable p-values. The final deliverable will be version 2.2 of the application (`app_v2.2.R` and associated helpers).

## 2. Goals & Scope
- **TPS Optimization Fix:** Restore the TPS lambda optimization to produce reasonable, scientifically sound values (non-zero) and robust GCV curves, referencing the working logic in v2.0 but adapting it for the new concurrent processing framework.
- **Moran's I Visibility:** Expose the internally calculated Moran's I values to the user by adding them to the main cross-validation metrics table.
- **Correlation Significance:** Enhance the correlation rank list by incorporating p-values and introducing a customizable filter (e.g., 0.05, 0.01, 0.001) to show only significant correlations.
- **Version Management:** Strictly enforce a file duplication and backup process before any modifications, yielding `app_v2.2.R` and maintaining backward compatibility without overwriting v2.1 files.

## 3. Functional Requirements
### 3.1 Setup & Backup (Pre-requisite)
- The system must back up existing v2.1 files (`app_v2.1.R`, `spatial_helpers_v2.1.R`, `ui_helpers_v2.1.R`) to the `backups/` directory before starting.
- New working files for version 2.2 (`app_v2.2.R`, `spatial_helpers_v2.2.R`, `ui_helpers_v2.2.R`) must be created as exact copies of v2.1.

### 3.2 TPS Optimization
- Implement an optimization routine that reliably converges on a scientifically correct and reasonable lambda value for TPS interpolation.
- Ensure the optimization logic works seamlessly within the `future`/`furrr` concurrent architecture introduced in v2.1.
- Provide a mechanism (e.g., logging or UI diagnostics) to confirm the robustness of the Generalized Cross-Validation (GCV) curve and chosen lambda.

### 3.3 Moran's I Metric Display
- Extract the currently calculated Moran's I values from the spatial pipeline.
- Present these values as a dedicated column in the main metrics data table (alongside RMSE, R2, CCC, etc.).

### 3.4 Correlation Rank List P-values
- Calculate the p-values for all variable correlations presented in the rank list.
- Implement a user-facing filter control (dropdown or numeric input) that allows users to filter the rank list based on custom alpha levels (e.g., 0.05, 0.01, 0.001).
- Ensure the filtered list updates dynamically.

## 4. Non-Functional Requirements
- **Scientific Rigor:** The TPS optimization fix must be mathematically sound and properly validated against domain geostatistical standards.
- **Performance:** Adding p-value calculations and Moran's I display should not significantly degrade application responsiveness. The concurrent performance gains of v2.1 must be preserved.
- **Code Preservation:** All styling, content, and working functions of `app_v2.1` must be strictly maintained in the v2.2 files.

## 5. Acceptance Criteria
- [ ] Existing v2.1 files are safely backed up, and `app_v2.2.R` is the active development target.
- [ ] Running TPS interpolation yields non-zero lambda values that minimize GCV correctly.
- [ ] Moran's I values are clearly visible for all evaluated models in the main metrics table.
- [ ] The correlation rank list includes p-values and a functional, customizable significance filter.
- [ ] The v2.2 application launches without errors and successfully executes a full spatial pipeline run with the example datasets (`tpad_5_preds.xlsx`, `Strasov mapping.xlsx`).

## 6. Out of Scope
- Major architectural changes to the asynchronous framework (`future`/`promises`).
- Adding new interpolation methods beyond the scope of fixing TPS.