# Specification: UI Content Optimization for Residual Mapping (v0.8.9)

## 1. Overview
This track addresses UI content optimization for the residual mapping functionality. The primary goal is to remove the unnecessary dropdown menu when a user maps residuals (pv-v) and instead display the two different residuals directly alongside explicit text information. Furthermore, this update marks the transition to version 0.8.9, which mandates that all core application and helper files are correctly backed up and renamed to reflect the new version number.

## 2. Functional Requirements
- **Version Bump & File Renaming (Pre-requisite):**
  - Create full backups of `monolith_ver_0.8.8.R` and all its helper files (spatial, theme, ui).
  - Copy and rename all core application and helper files to feature the `_0.8.9.R` suffix.
  - Ensure the new `monolith_ver_0.8.9.R` file sources the correctly renamed `_0.8.9.R` helper scripts.
- **Residual Mapping UI Overhaul:**
  - Remove the existing dropdown menu used to select different residual types (e.g., 'Interpolated Delta' vs 'Interpolated Point Errors').
  - **Text Information:** Place descriptive text information explaining the two different residuals directly into the control panel where the dropdown was previously located.
  - **Map Display:** Render both residual maps simultaneously in a side-by-side layout, allowing users to visually compare them directly without toggling.
  - **Legend Synchronization:** Ensure the legends of the two maps work similarly, showing negative values with red and positive values with blue on a standard diverging scale.

## 3. Non-Functional Requirements
- **Scientific & Functional Integrity:** The underlying geostatistical processing logic and existing automated functions must remain completely unchanged.
- **Aesthetic Continuity:** Ensure the new side-by-side map layout aligns with the project's existing UI/UX and dynamic theme engine.

## 4. Acceptance Criteria
- [ ] Application and helper files from v0.8.8 are securely backed up.
- [ ] New `monolith_ver_0.8.9.R` file exists, successfully sources `*_0.8.9.R` helper files, and runs without errors.
- [ ] Residual mapping mode no longer displays the dropdown menu in the UI.
- [ ] The control panel explicitly displays informative text about the two residual types.
- [ ] Selecting "Residuals" maps both 'Interpolated Delta' and 'Interpolated Point Errors' simultaneously side-by-side.
- [ ] The color palettes (legends) for both maps use a consistent red (negative) to blue (positive) diverging scale.

## 5. Out of Scope
- Modifying the underlying algorithms computing the residuals.
- Adjusting statistical evaluation metrics or the performance analytics engine.