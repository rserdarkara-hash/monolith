# Specification: Normality Testing Option for Descriptive Suite

## Overview
This feature adds a passive normality testing option to the Descriptive Suite within the Monolith R Shiny application. When the user selects plot types that support Statistical Significance Tests (Boxplot, Violin Plot, and Sina-style Plot), the app will automatically perform a normality test on the selected numeric variable. A colored icon with a hover tooltip will be displayed next to the "Statistical Significance Tests" heading to inform the user whether the data meets normality assumptions. This is a passive feature; it does not restrict the user's choice of significance tests.

## Functional Requirements
1. **Target Plot Types:** The normality indicator icon will appear next to the "Statistical Significance Tests" heading when the selected plot type (`input$desc_plot_type`) is `boxplot`, `violin`, or `sinaplot`.
2. **Normality Test Logic:**
   - **Sample Size $n < 3$:** Treat as an edge case. Do not run any test; display a neutral/gray question mark icon.
   - **Sample Size $3 \le n < 50$:** Perform the Shapiro-Wilk test (`shapiro.test`).
   - **Sample Size $n \ge 50$:** Perform the Kolmogorov-Smirnov test with Lilliefors correction (`nortest::lillie.test`).
3. **Significance Threshold:**
   - If the test $p$-value is $\ge 0.05$, the data is classified as **Normal** (pass).
   - If the test $p$-value is $< 0.05$, the data is classified as **Not Normal** (fail).
4. **Visual Indicator:**
   - Render an icon to the right of the "Statistical Significance Tests" heading.
   - **Normality Passed (Normal):** Green checkmark icon (e.g., `shiny::icon("check-circle")` or similar) with success styling.
   - **Normality Failed (Not Normal):** Red/orange exclamation/warning icon (e.g., `shiny::icon("exclamation-triangle")` or similar) with warning/danger styling.
   - **Insufficient Data ($n < 3$):** Neutral/gray question mark icon (e.g., `shiny::icon("question-circle")`) with muted styling.
5. **Interactive Tooltip (Hover):**
   - On hovering over the icon, display a tooltip with the following details:
     - The normality test used (e.g., "Shapiro-Wilk Normality Test" or "Lilliefors (Kolmogorov-Smirnov) Normality Test")
     - Test statistic ($W$ or $D$)
     - $p$-value (formatted, e.g., `< 0.001` or the actual decimal value rounded to 4 decimal places)
     - Sample size ($n$)
     - An explanation (e.g., "Data is normally distributed ($p \ge 0.05$)" or "Data deviates from normality ($p < 0.05$)")
6. **Dependency Auto-loader:**
   - Add `"nortest"` to the `required_packages` list in `global_0.9.8b.R` to ensure Lilliefors test is available.
7. **Passive Behavior:**
   - The indicator is purely informational and does not block, auto-select, or change the behavior of the existing ANOVA, Duncan's, or Tukey's tests.

## Non-Functional Requirements
- **Performance:** Normality tests must run quickly on the reactive dataset (`rv_filtered_analytics_data()`) and not cause UI lag.
- **Robustness:** Tests must be wrapped in `tryCatch` to prevent app crashes if there are numerical edge cases (e.g., zero variance).

## Acceptance Criteria
- Selecting "Boxplot", "Violin", or "Sina-style Plot" renders the normality test icon next to "Statistical Significance Tests".
- The correct normality test is applied based on the sample size ($n < 3$, $3 \le n < 50$, $n \ge 50$).
- Normality classification ($p \ge 0.05$ vs $p < 0.05$) is visually represented by the correct colored icon.
- Hovering over the icon displays a tooltip showing the test name, statistic, p-value, sample size, and conclusion.
- The `nortest` package is loaded automatically on startup.
- Existing statistical significance tests (ANOVA, Duncan's, Tukey's HSD) function exactly as before.

## Out of Scope
- Auto-switching to non-parametric tests (e.g., Kruskal-Wallis) based on the normality result.
- Normality testing for multi-variable plots (Parallel, Radar) or scatter plots.
