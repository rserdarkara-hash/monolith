# Specification: Covariate Fallback, Parallelization & Label Rendering (v2.1)

## Overview
This track addresses a critical bug regarding covariate interpolation rigidity during Regression Kriging (RK) and Random Forest Kriging (RFK), implements comprehensive parallel processing across the application utilizing `future` and `furrr`, and enhances UI readability by replacing raw column names with scientifically formatted labels in the correlation rank module.

## Functional Requirements
1.  **Covariate Interpolation Safety Wrapper:**
    -   Wrap the Ordinary Kriging interpolation of auxiliary covariates within `tryCatch()`.
    -   If variogram fitting or `krige()` encounters errors (e.g., pure nugget, singular matrix), fallback exclusively to Inverse Distance Weighting (IDW) for the problematic covariate.
    -   **Notification:** Trigger a Shiny UI notification (alert or toast) identifying the covariate that triggered the fallback mechanism.
2.  **Comprehensive Parallel Processing (`multisession` via `future`/`furrr`):**
    -   Replace sequential `for()` loops with parallel equivalents (e.g., `future_map()`, `future_lapply()`) in the main interpolation workflow.
    -   Prioritize parallelization across:
        -   Multi-locality processing loops.
        -   IDW parameter optimization searches.
        -   Model fitting and learning logic specifically for RK and RFK.
        -   Simultaneous map generation (Actual, Predicted, Residuals).
    -   Identify and apply parallelization to any other compute-intensive loops in the pipeline where practical.
3.  **Correlation Rank Module Labels:**
    -   Modify the correlation rank visualization/table to display semantic variable labels extracted from the configuration (`variable list.xlsx`) rather than raw column names.
    -   **Fallback:** If a variable label is missing or undefined, default to displaying its raw column name.
4.  **Version Control & Backup:**
    -   Create `app_v2.1.R`, `spatial_helpers_v2.1.R` (if needed), and `ui_helpers_v2.1.R` (if needed) by duplicating the `v2.0` suite.
    -   Ensure the legacy `v2.0` files are not overwritten.

## Non-Functional Requirements
-   **Integrity:** Maintain all existing UI styling, computational rigor, and functional features of the v2.0 application.
-   **Dependency Addition:** Utilize `furrr` package and appropriately integrate it.

## Acceptance Criteria
-   The application successfully interpolates datasets with pure-nugget auxiliary covariates without crashing, gracefully utilizing IDW.
-   A UI notification fires when a covariate triggers the IDW fallback.
-   Computational tasks like locality iterations and map generations successfully utilize multiple cores via the `future` architecture, exhibiting measurable efficiency gains or non-blocking UI behavior.
-   The correlation rank module correctly displays `variable list.xlsx` labels instead of variable database names.
-   The original `app_v2.0.R` and its helpers remain unaltered; new work is fully contained in `v2.1` prefixed files.