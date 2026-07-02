# Implementation Plan: Normality Testing Option for Descriptive Suite

## Phase 1: Setup and Test Suite (TDD)
- [ ] Task: Update Dependencies and Setup Test
    - [ ] Add "nortest" to `required_packages` in `global_0.9.8b.R`
    - [ ] Create test file `tests/test_normality_testing.R` defining the normality test selection logic (sample sizes, test methods, p-values, edge cases)
    - [ ] Run the tests and confirm they fail/warn as expected (Red Phase)
- [ ] Task: Conductor - User Manual Verification 'Phase 1: Setup and Test Suite (TDD)' (Protocol in workflow.md)

## Phase 2: Core Normality Test Implementation
- [ ] Task: Implement Normality Testing Function
    - [ ] Write helper function `compute_normality` in `desc_exploratory_module_0.9.8b.R` or a helper utility
    - [ ] Handle sample size $n < 3$ edge case (neutral status, no test run)
    - [ ] Handle $3 \le n < 50$ (Shapiro-Wilk test)
    - [ ] Handle $n \ge 50$ (Lilliefors test from `nortest` package)
    - [ ] Add `tryCatch` to handle errors (e.g. zero variance in data) gracefully
- [ ] Task: Verify Core Logic and Green Phase
    - [ ] Run the tests in `tests/test_normality_testing.R` and confirm they pass (Green Phase)
- [ ] Task: Conductor - User Manual Verification 'Phase 2: Core Normality Test Implementation' (Protocol in workflow.md)

## Phase 3: UI Integration and Tooltip Render
- [ ] Task: Add Normality Indicator to descriptive module UI
    - [ ] Add a `shiny::uiOutput` or inline conditional rendering for the normality icon next to the "Statistical Significance Tests" heading in `desc_plot_vars_ui` of `desc_exploratory_module_0.9.8b.R`
    - [ ] Create server logic to render the icon with dynamic coloring (green for normal, red/orange for not normal, gray for edge cases)
    - [ ] Embed tooltip details (test statistic, p-value, sample size, test type) in HTML/Bootstrap style via tag attributes (e.g., standard `title` or standard HTML tooltip)
- [ ] Task: Verification and Styling Polish
    - [ ] Verify reactive responsiveness (changing variables or plot types dynamically updates the normality icon and tooltip)
    - [ ] Add CSS style tweaks to align the icon nicely next to the heading
- [ ] Task: Conductor - User Manual Verification 'Phase 3: UI Integration and Tooltip Render' (Protocol in workflow.md)
