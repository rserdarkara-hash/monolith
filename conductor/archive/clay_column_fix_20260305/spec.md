# Specification: Clay Column Recognition Fix

## Overview
The 'clay' column present in the input dataset (`samp_data_1.xlsx`) and variable list metadata (`samp_var_list.xlsx`) is not being properly recognized, labeled, or mapped within the application's spatial mapping workflow. While it strangely appears in correlation rank lists, it is missing from the main variable selection UI and cannot be mapped. 

## Track Details
- **Type**: Bug Fix
- **Target Application Version**: `app_v2.6a` (creating a non-destructive iteration over `app_v2.6`).

## Functional Requirements
1. **Automated Backup & Scaffolding**: Before any logic changes, create a complete backup of `app_v2.6.R` and its associated helpers. The target application file will be `app_v2.6a.R` and associated helpers.
2. **Generalized Variable Recognition**: Implement a robust parsing and mapping fallback mechanism for unmatched soil parameters (fixing the 'clay' / 'Clay (%)' column mapping discrepancy).
3. **UI Variable List Resolution**: Ensure the unrecognized or improperly formatted 'clay' column correctly appears in the variable selection UI (context section) and is available for spatial mapping and interpolation workflows.
4. **Metadata Alignment**: The variable list parser must correctly bind metadata from the uploaded `samp_var_list.xlsx` with the data columns in `samp_data_1.xlsx`, accommodating minor mismatches.

## Non-Functional Requirements
- Maintain complete scientific and functional parity with the `app_v2.6` features.
- Ensure the fix generalizes gracefully to other similarly unmapped variables.

## Acceptance Criteria
- `app_v2.6a.R` and corresponding helper files are properly scaffolded from the `app_v2.6` versions.
- The 'clay' column can be selected from the primary mapping UI dropdowns.
- The map for 'Clay (%)' renders successfully without errors.
- The fix does not break the mapping functionality of previously working physicochemical parameters.

## Out of Scope
- Adding entirely new geostatistical models.
- Refactoring the correlation rank list implementation (as clay is reportedly already visible there).