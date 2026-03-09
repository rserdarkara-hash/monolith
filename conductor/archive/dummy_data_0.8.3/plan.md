# Plan: Dummy Data Creation for Monolith 0.8.3

## Phase 1: Preparation & Backup
- [x] Backup existing app files (`monolith_0.8.3.R`) and helpers (`spatial_helpers_0.8.3.R`, `theme_helpers_0.8.3.R`, `ui_helpers_0.8.3.R`) to `backups/pre_dummy_data_v0.8.3/` (1354dc9)
- [x] Analyze the structure and content of `samp_data_1.xlsx`, `samp_var_list.xlsx`, `samp_data_2.xlsx`, and `samp_var_list_2.xlsx` (a9ecdaa)

## Phase 2: Research & Strategy
- [x] Propose scientifically valid methods for dummy data generation (e.g., perturbation, synthetic generation based on distributions) (1354dc9)
- [x] Design the strategy for fake predictions (`_cve` and `_ss`) for nutrients (1354dc9)

## Phase 3: Implementation
- [x] Create script for dummy data generation
- [x] Generate dummy data for `samp_data_1.xlsx` and its nutrient predictions
- [x] Generate dummy data for `samp_data_2.xlsx`
- [x] Update app files to use the new dummy datasets if necessary (ensure versioning) - Skipped per user request

## Phase 4: Verification
- [x] Verify scientific plausibility of generated data
- [x] Ensure no real data is exposed
- [x] Run automated tests and perform manual verification with the dashboard - Skipped per user request
- [x] Finalize the "newer version" reproduction - Skipped per user request
