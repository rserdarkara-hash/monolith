# Implementation Plan: Governing Factors Analysis Tab

## Phase 1: Environment Setup and Backup
- [x] Task: Back up existing `monolith_0.8.4.R` and helper files to the `backups/` directory. 4720691
- [x] Task: Create new version files (e.g., `monolith_0.8.5.R`, `spatial_helpers_0.8.5.R`, `theme_helpers_0.8.5.R`, `ui_helpers_0.8.5.R`). 4720691
- [x] Task: Update the internal sourcing paths in the new main `monolith` file to point to the new helper files. b4f8183
- [x] Task: Conductor - User Manual Verification 'Environment Setup and Backup' (Protocol in workflow.md) 34d226f

## Phase 2: Structural UI Integration (4th Tab)
- [x] Task: Write tests verifying the UI structural changes (e.g., checking for the new tab ID). 7ccf5c2
- [x] Task: Implement the "Governing Factors" tab in the `Scientific Analytics Engine` UI layout. 2b7c184
- [x] Task: Implement the sidebar configuration panel (`selectInput` for Target, `multiInput`/`pickerInput` for Factors, `sliderInput` for Permutations, and an `actionButton` for "Run Analysis"). 2b7c184
- [x] Task: Implement the 4-quadrant layout structure in the main panel and the tabular data section below it. 2b7c184
- [x] Task: Conductor - User Manual Verification 'Structural UI Integration (4th Tab)' (Protocol in workflow.md) f5683e7

## Phase 3: Analytical Engine Integration (Backend)
- [x] Task: Write tests for the Native R backend functions calculating feature importance, SHAP, and ALE values. 487e62f
- [x] Task: Implement data preparation logic that respects the existing grouping/discretization engine. cabfdf5
- [x] Task: Implement the Random Forest model fitting utilizing `tidymodels` based on selected Target and Factors. cabfdf5
- [x] Task: Implement the global importance, SHAP (using `DALEX`, `iml`, or `fastshap`), and ALE/PDP extraction logic. cabfdf5
- [x] Task: Conductor - User Manual Verification 'Analytical Engine Integration (Backend)' (Protocol in workflow.md) 8d0a97d

## Phase 4: Visualization and Rendering (Frontend)
- [x] Task: Write tests verifying the reactive rendering of the four plots and the data table. dca3628
- [x] Task: Implement the Global Importance Bar Chart (Top Left) renderer. 0359e85
- [x] Task: Implement Causality/Interaction Plots (Top Right, Bottom Right) renderers. 0359e85
- [x] Task: Implement the Functional Effect Plot (Bottom Left) renderer with the ALE/SHAP toggle switch logic. 0359e85
- [x] Task: Implement the Tabular Data View renderer, binding it to the extracted metrics. 0359e85
- [x] Task: Tie all renderers to the `actionButton` to ensure explicit execution. 0359e85
- [x] Task: Conductor - User Manual Verification 'Visualization and Rendering (Frontend)' (Protocol in workflow.md) 2252d15