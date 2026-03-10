# Track Specification: Optimizing UI Content Depending on User Mapping Choice

## 1. Overview
The goal of this track is to dynamically optimize the user interface by hiding elements related to predicted data (e.g., specific columns, rows, or sections in the Scientific Analysis and Optimization Summaries) when the user maps or analyzes only actual sampled data without running interpolation. This cleans up the UI and prevents the display of sections populated with `NA` values.

## 2. Functional Requirements
- **FR1: Version Checkpointing:** At the very start of the process, create a backup of `monolith_ver_0.8.7.R` and its helper scripts (e.g., `spatial_helpers_0.8.7.R`, `theme_helpers_0.8.7.R`, etc.) in the `backups/` folder. Create new iterative files (`monolith_ver_0.8.8.R`, etc.) for active development to ensure the v0.8.7 stable baseline is never overwritten.
- **FR2: Dynamic UI Visibility (Summaries & Exports):** Implement logic using `shinyjs::hide()` and `shinyjs::show()` to completely hide UI elements, tables, and sections within the Summaries and Exports modules that rely on interpolated (predicted) data.
- **FR3: State-Driven Reactivity:** The visibility of these UI elements must be tied to the *Interpolation State* (i.e., whether the user has successfully run the geostatistical interpolation models), rather than just toggling the visual map layers.
- **FR4: Data Filtering:** Ensure that optimization summary tables and export registries dynamically omit columns or rows associated with "Predicted", "Residual", or "Comparison" metrics if no prediction data is available in the current reactive state.

## 3. Non-Functional Requirements
- **NFR1:** Must use existing `shinyjs` implementations to prevent adding overhead to the UI reactivity.
- **NFR2:** Maintain strict backward compatibility with existing tests and functionality present in `monolith_ver_0.8.7.R`.
- **NFR3:** Ensure the UI updates seamlessly without blocking the main R thread or requiring manual page refreshes.

## 4. Acceptance Criteria
- [ ] `monolith_ver_0.8.7.R` and helpers are backed up, and new `0.8.8` files are correctly linked and operational.
- [ ] If an interpolation is *not* run or a dataset is loaded without predictive models, all summary tables and export modules perfectly omit predicted-data columns.
- [ ] The UI sections that strictly require predicted data disappear completely (via `shinyjs`) instead of showing errors or NAs.
- [ ] Running a successful interpolation correctly un-hides these sections and populates the predicted data.

## 5. Out of Scope
- Modifying the underlying geostatistical interpolation algorithms (Kriging, IDW, RF).
- Altering the Descriptive Suite, PCA, or Correlation Analysis UI elements (unless explicitly linked to the aforementioned Summaries & Exports).