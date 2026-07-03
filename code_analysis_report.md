# Codebase Analysis: Dead Code and Redundancy Report

This report outlines areas in the R codebase that contain dead code and structural redundancies which can be easily refactored for better maintainability.

## 1. Dead Code

### `safe_concaveman` (in `monolith.R`)
- **Location:** `monolith.R` (around line 185)
- **Description:** A helper function designed to compute a concave hull using `concaveman` with a fallback to `sf::st_convex_hull`. 
- **Issue:** A global search of the codebase reveals that `safe_concaveman` is defined but never invoked anywhere within the `.R` scripts. 
- **Fix:** Remove the function definition entirely to clean up the global namespace.

## 2. Redundancies

### Kriging Wrapper Functions (in `spatial_helpers.R`)
- **Location:** `spatial_helpers.R` (lines 620-630)
- **Description:** The functions `apply_OK`, `apply_RK`, and `apply_RFK` are defined as single-line wrappers that simply invoke `apply_kriging_pipeline`, passing down their exact arguments alongside a specific method string (`"OK"`, `"RK"`, `"RFK"`).
- **Issue:** These thin wrappers introduce unnecessary layers of abstraction. 
- **Fix:** Remove these wrapper functions and update all callers to directly invoke `apply_kriging_pipeline(engine = "...", ...)` to streamline the code.

### IDW Fallback Logic inside TPS (in `spatial_helpers.R`)
- **Location:** `spatial_helpers.R` inside the `apply_TPS` error handler (around line 804).
- **Description:** When the Thin Plate Spline (TPS) model encounters an error, the `tryCatch` block falls back to Inverse Distance Weighting (IDW). 
- **Issue:** The fallback block duplicates the exact IDW model fitting (`idw(...)`) and cross-validation (`krige.cv(...)`) logic already implemented in the `apply_IDW` function.
- **Fix:** Replace the redundant block inside the `error = function(e)` block with a direct call to `apply_IDW(...)`, returning its results. This ensures any future IDW updates apply to the TPS fallback as well.

### PCA Bar Plot Generators (in `ui_helpers.R`)
- **Location:** `ui_helpers.R` (functions `generate_pca_loadings`, `generate_pca_contribution`, `generate_pca_cos2`)
- **Description:** These three functions create very similarly structured horizontal bar charts for PCA results.
- **Issue:** They share nearly identical boilerplate: extracting a vector, converting it to a dataframe, sorting it, leveling the factors in reverse, and generating a `ggplot` with `geom_bar(stat = "identity")` and `coord_flip()`.
- **Fix:** Consolidate this repeated visualization logic into a single generic helper (e.g., `generate_pca_bar_plot(df, x_var, y_var, fill_color, title_text, y_label)`). The three functions can then be simplified to data extraction wrappers that return the output of the generic helper.
