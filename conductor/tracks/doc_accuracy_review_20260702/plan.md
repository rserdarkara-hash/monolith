# Implementation Plan: Monolith Documentation Accuracy Review

## Phase 1: Setup and Documentation Drawer Restructuring [checkpoint: 019508a]
- [x] Task: Create drawer UI test and write failing tests f46af2e
    - [x] Create test file `tests/test_docs_drawer.R`
    - [x] Write tests to verify `render_docs_drawer()` outputs three tabs: 'Scientific Guide', 'User Guide', and 'Descriptive & Exploratory Suite'
    - [x] Run test suite to verify tests fail as expected (Red Phase)
- [x] Task: Implement drawer restructuring and rename files d06e29d
    - [x] Rename `docs/ui_ux_guide.md` to `docs/user_guide.md`
    - [x] Create `docs/desc_exploratory_guide.md` as a new markdown file
    - [x] Update `render_docs_drawer()` in `ui_helpers_0.9.8b.R` to define three tabs
    - [x] Update server-side renderUI hooks in `monolith_ver_0.9.8b.R` to load and compile `docs/desc_exploratory_guide.md` as `output$render_desc_exploratory_guide`
    - [x] Run test suite to confirm tests pass (Green Phase)
- [x] Task: Conductor - User Manual Verification 'Phase 1: Setup and Documentation Drawer Restructuring' (Protocol in workflow.md)

## Phase 2: Documentation Migration and Accuracy Review
- [ ] Task: Content migration and validation tests
    - [ ] Create test `tests/test_docs_existence.R` verifying that all three markdown files are present in the `docs/` folder and contain content
    - [ ] Migrate the Descriptive & Exploratory Suite sections from `docs/user_guide.md` into `docs/desc_exploratory_guide.md`
    - [ ] Run test suite to verify all checks pass
- [ ] Task: Conduct codebase-to-docs accuracy review and update documentation
    - [ ] Verify geostatistical models, equations, and defaults in `docs/scientific_guide.md` (e.g. OK epsilon-nugget, CK/RK/RFK IDW fallback, etc.) match R code implementation
    - [ ] Verify UI menus, buttons, labels, and text inputs in `docs/user_guide.md` match `monolith_ver_0.9.8b.R` and `ui_helpers_0.9.8b.R` exactly
    - [ ] Verify statistical details (ANOVA post-hoc, PCA warnings, SHAP/ALE explainability) in `docs/desc_exploratory_guide.md` match `desc_exploratory_module_0.9.8b.R`
- [ ] Task: Conductor - User Manual Verification 'Phase 2: Documentation Migration and Accuracy Review' (Protocol in workflow.md)
