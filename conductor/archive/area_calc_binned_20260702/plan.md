# Implementation Plan: Area Coverage Calculations for Binned and Agronomical Styling

## Phase 1: Unit Testing and Setup (TDD Red Phase)
- [x] Task: Create Failing Unit Tests
    - [x] Create a new unit test file at `tests/test_area_calc_styling.R`.
    - [x] Write unit tests for `classification_params` to verify 5 equal-width bins are generated for `"bin"` styling.
    - [x] Write unit tests for `classification_params` to verify Jenks and K-means duplicate breaks are handled safely without NA bounds.
    - [x] Write unit tests for `calc_area_df` to verify correct area calculations and class labelling for both `"agro"` and `"bin"` styles.
- [x] Task: Verify Test Failure (Red Phase)
    - [x] Run the test suite using `C:\Program Files\R\R-4.5.2\bin\Rscript.exe tests/test_area_calc_styling.R` and confirm that the new tests fail.
- [x] Task: Conductor - User Manual Verification 'Phase 1: Unit Testing and Setup (TDD Red Phase)' (Protocol in workflow.md)

## Phase 2: Unified Classification and Area Calculations (TDD Green Phase)
- [x] Task: Refactor and Implement Classification Logic
    - [x] Create/update a unified classification params reactive function `classification_params` in `monolith_ver_0.9.8b.R` supporting both `"agro"` and `"bin"` color styles.
    - [x] Implement robust handling of duplicate breaks in Jenks and K-means, dynamically adjusting `n_c` to the actual number of unique intervals.
    - [x] Update `calc_area_df` in `monolith_ver_0.9.8b.R` to retrieve parameters from `classification_params()` instead of `agro_params()`.
- [x] Task: Update Area Table Renderers
    - [x] Modify `area_table_total_act`, `area_table_total_pre`, `area_table_loc_act`, and `area_table_loc_pre` in `monolith_ver_0.9.8b.R` to render when styling is `"agro"` or `"bin"`.
    - [x] Modify `calc_area_df` or row formatting to ensure `"bin"` style displays numeric ranges (e.g. `< 12`, `12 - 24`, ..., `> 48`) as row labels.
- [x] Task: Verify Test Success (Green Phase)
    - [x] Run the test suite and confirm that all tests in `tests/test_area_calc_styling.R` now pass.
- [x] Task: Conductor - User Manual Verification 'Phase 2: Unified Classification and Area Calculations (TDD Green Phase)' (Protocol in workflow.md)

## Phase 3: UI and Mapping Parity Integration
- [x] Task: Update Leaflet Map and Export Plots
    - [x] Update Leaflet map rendering in `monolith_ver_0.9.8b.R` to use `classification_params()` for `"bin"` styling.
    - [x] Update ggplot preview in `ui_helpers_0.9.8b.R` (`generate_base_plot`) to use classified raster and `scale_fill_manual` for `"bin"`.
    - [x] Update ggplot export plots in `monolith_ver_0.9.8b.R` to use classified raster and `scale_fill_manual` for `"bin"`.
    - [x] Ensure binned area tables are registered in the export registry (e.g., in `register_locality_assets`).
- [x] Task: Final Verification
    - [x] Run all test files in the `tests/` directory to ensure no regressions.
- [x] Task: Conductor - User Manual Verification 'Phase 3: UI and Mapping Parity Integration' (Protocol in workflow.md)
