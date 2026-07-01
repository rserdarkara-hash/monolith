# Implementation Plan - Resolve Map Viewer Overlay and Documentation Drawer Layering Conflict

## Phase 1: Test Setup (Red Phase)
- [x] Task: Create Failing Test for Z-Index Styles (dad0052)
    - [x] Create a new unit test file at `tests/test_theme_z_index.R`.
    - [x] Write asserts checking that the `.docs-drawer` class has a `z-index` of `2500` in the manual theme CSS.
    - [x] Run the test using Rscript and verify that it fails (Confirm Red Phase).
- [x] Task: Expand Test for Modals and Backdrops (Red Phase) (854ea26)
    - [x] Update `tests/test_theme_z_index.R` to check `.modal` and `.modal-backdrop` z-index values (2610 and 2600 respectively).
    - [x] Run the test and verify it fails on the new assertions (Red Phase).
- [ ] Task: Conductor - User Manual Verification 'Phase 1: Test Setup' (Protocol in workflow.md)

## Phase 2: Theme Style Implementation (Green Phase)
- [ ] Task: Update Z-Index Values in CSS
    - [ ] Modify the z-index of `.docs-drawer` to `2500` inside `theme_helpers_0.9.8b.R`.
    - [ ] Add `.modal` and `.modal-backdrop` z-index overrides to `theme_helpers_0.9.8b.R` (z-index 2610 and 2600 respectively).
    - [ ] Rerun `tests/test_theme_z_index.R` and confirm it passes (Confirm Green Phase).
    - [ ] Run the full test suite (`tests/test_async_guards.R`, `tests/test_gov_factors.R`, etc.) to verify no regressions.
- [ ] Task: Conductor - User Manual Verification 'Phase 2: Theme Style Implementation' (Protocol in workflow.md)

## Phase 3: Quality Gates & Verification
- [ ] Task: Run Final Diagnostics & Checks
    - [ ] Execute all test scripts under `tests/` to guarantee everything is clean.
    - [ ] Perform a self-review checklist comparison against Quality Gates.
- [ ] Task: Conductor - User Manual Verification 'Phase 3: Quality Gates & Verification' (Protocol in workflow.md)
