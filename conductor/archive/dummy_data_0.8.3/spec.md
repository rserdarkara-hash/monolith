# Specification: Dummy Data Creation for Monolith 0.8.3

## Goal
Create dummy data for soil properties and nutrient predictions to support testing and demonstration of Monolith 0.8.3 without exposing sensitive original data.

## Requirements
- Variables: `ph`, `ec`, `som`, `clay`, `sand`, `silt`, `caco3`, `tn`, `p`, `k`, `ca`, `mg`, `fe`, `mn`, `cu`, `zn`.
- Locations: Exact point locations from `samp_data_1.xlsx`.
- Predictions: Fake prediction sets (`_cve` and `_ss`) for nutrients (`tn`, `p`, `k`, `ca`, `mg`, `fe`, `mn`, `cu`, `zn`).
- Scope: Apply to both `samp_data_1.xlsx` and `samp_data_2.xlsx` (all numeric results).
- Constraints: 
    - Scientifically valid (plausible ranges and distributions).
    - Environmentally convenient.
    - Useful for the user.
    - **Zero exposure of real data.**
    - Maintain style, scientific content, and automated functions of `monolith_0.8.3.R` and its helpers.

## Pre-requisites
- Backup all existing app files (`monolith_0.8.3.R`) and helpers (`spatial_helpers_0.8.3.R`, `theme_helpers_0.8.3.R`, `ui_helpers_0.8.3.R`) to the backups folder.
- Never overwrite existing files; reproduce all existing files for the app to work and back them up.

## Data Files
- `samp_data_1.xlsx` / `samp_var_list.xlsx`
- `samp_data_2.xlsx` / `samp_var_list_2.xlsx`
