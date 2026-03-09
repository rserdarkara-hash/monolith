# Implementation Plan: Spatial Soil Analysis Dashboard (v2.6a -> v2.7)

## Phase 1: Setup and Versioning
- [x] Task: Backup `v2.6a` files to `backups/pre_v2.7_upgrade_YYYYMMDD` directory. [715172d]
- [x] Task: Duplicate `v2.6a` files (app and helpers) to create `v2.7` equivalents in the root directory. [e897e0f]
- [x] Task: Update script sourcing paths in `app_v2.7.R` to point to the new `v2.7` helper files. [c46bf1a]
- [x] Task: Conductor - User Manual Verification 'Setup and Versioning' (Protocol in workflow.md)

## Phase 2: Cloud Deployment Fix (Export Registry)
- [x] Task: Research and analyze current `shinyDirChoose` and `ggsave` implementation in `app_v2.7.R` (and relevant helpers).
- [x] Task: Write Tests for new zip export functionality (mocking `downloadHandler` if possible, or testing the zip creation logic). [96d63e0]
- [x] Task: Implement `downloadHandler()` logic in `app_v2.7.R` to replace `shinyDirChoose`. [96d63e0]
- [x] Task: Integrate `zip` package to bundle selected exported files into a temporary `.zip` archive before pushing to user. [96d63e0]
- [x] Task: Conductor - User Manual Verification 'Cloud Deployment Fix (Export Registry)' (Protocol in workflow.md)

## Phase 3: Mathematical Stability (IDW & NA bugs)
- [x] Task: Research and analyze `optimize_idw_p` in spatial helpers.
- [x] Task: Write Tests that purposefully inject `NA` residuals into `optimize_idw_p` to verify failure without `na.rm`. [b8e3326]
- [x] Task: Implement `na.rm = TRUE` fix in `optimize_idw_p`'s RMSE calculation. [b8e3326]
- [x] Task: Scan spatial helpers for other vulnerable statistical functions (`mean`, `sum`, etc.) lacking `na.rm`.
- [x] Task: Implement `na.rm = TRUE` fixes across identified vulnerable functions. [b8e3326]
- [x] Task: Conductor - User Manual Verification 'Mathematical Stability (IDW & NA bugs)' (Protocol in workflow.md)

## Phase 4: Scoping Stability (Global Assignment `<<-`)
- [x] Task: Research and analyze `apply_interpolation` in spatial helpers for `<<-` usage.
- [x] Task: Write Tests for `apply_interpolation` error handling to ensure error strings are correctly returned. [3cc194c]
- [x] Task: Refactor `apply_interpolation` to remove `<<-` and use local assignment for `res$log_msg`. [3cc194c]
- [x] Task: Scan the entire codebase for other dangerous uses of `<<-`.
- [x] Task: Refactor other identified `<<-` usages to use localized scoping. [3cc194c]
- [x] Task: Conductor - User Manual Verification 'Scoping Stability (Global Assignment `<<-`)' (Protocol in workflow.md)