# Specification: Spatial Soil Analysis Dashboard (v2.6a -> v2.7)

## Overview
This track prepares the Spatial Soil Analysis Dashboard for preview and deployment (from version 2.6a to 2.7). It introduces critical functional and structural fixes to resolve deployment issues, mathematical instability, and scoping dangers. The primary focus is ensuring stability for cloud deployment, robust execution of geostatistical optimizations, and safe parallel processing environments. The original source files must be meticulously backed up, and a new suite of files (`v2.7`) created.

## Functional Requirements
1.  **File Management and Versioning:**
    -   Create backups of `app_v2.6a.R` and its corresponding helper scripts (e.g., `spatial_helpers_v2.6a.R`, `ui_helpers_v2.6a.R`, `theme_helpers_v2.6a.R`) in a dedicated backup folder.
    -   Duplicate the `v2.6a` files to establish the foundation for `v2.7` (e.g., `app_v2.7.R`, `spatial_helpers_v2.7.R`, etc.).

2.  **Export Registry / Cloud Deployment Fix:**
    -   Replace the local file system save mechanism (`shinyDirChoose` and direct `ggsave`) with an architecture suitable for cloud environments.
    -   Implement `downloadHandler()` combined with the `zip` library.
    -   Bundle multiple user-selected spatial maps/plots into a `.zip` archive that is pushed directly to the user's browser for download.

3.  **IDW Optimization Stability:**
    -   Modify the `optimize_idw_p` function.
    -   Ensure `na.rm = TRUE` is strictly applied when calculating the Root Mean Square Error (RMSE) to prevent returning arrays of NAs if cross-validation fails for a specific spatial point.
    -   **Extended Scope:** Scan the spatial helper scripts for other potentially vulnerable statistical aggregate functions (like `mean()`, `sum()`) lacking `na.rm = TRUE` and patch them where mathematically sound.

4.  **Scoping Stability / Global Assignment:**
    -   Remove the superassignment operator (`<<-`) from the `tryCatch` error handlers within the `apply_interpolation` function.
    -   Refactor to return the error string locally and use standard assignment (`<-`) outside the block to append the message, protecting the execution environment and parallel workers.
    -   **Extended Scope:** Scan the codebase for other dangerous uses of the `<<-` operator and refactor to localized scoping appropriately.

## Non-Functional Requirements
-   **Dependencies:** Introduce the `zip` package to the project ecosystem to handle archive generation across multiple OS architectures reliably.
-   **Integrity:** The overarching aesthetics, scientific operations, automation flows, and existing functionality of `app_v2.6a` must be completely preserved.

## Out of Scope
-   Major architectural rewrites or UI redesigns unrelated to the specific fixes mentioned above.
-   Adding new interpolation algorithms beyond the scope of fixing current ones.

## Acceptance Criteria
-   `v2.6a` source files are preserved and backed up successfully.
-   `v2.7` files are created and the app runs normally.
-   The "Batch Export Selected" feature accurately generates a `.zip` file of selected resources via a browser download prompt.
-   The IDW interpolation does not silently fail (returns an error or gracefully skips) when evaluating zero distances/identical points.
-   The source code contains no vulnerable uses of the `<<-` operator that could cause cross-session variable pollution.