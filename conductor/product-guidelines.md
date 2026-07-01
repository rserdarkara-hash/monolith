# Product Guidelines: Monolith

## 1. Prose & Documentation Style
* **Scientific & Precise**: Enforce standard academic terminology for all diagnostic, statistical, and spatial outputs.
* **Mathematical Rigor**: When discussing spatial interpolation (IDW, Kriging, TPS), present or reference clear equations, parameters (nugget, sill, range), and diagnostic metrics (Moran's I, NSE, CCC, RPD, RPIQ).
* **Self-Contained Explanations**: Provide clear, concise inline explanations or tooltips for complex geostatistical terms.

## 2. Design & Branding System
* **Clean Academic, Yet Modern**: High data density layouts prioritized for publication readiness.
* **Color Palette**: Sophisticated, muted academic colors for plots and tables, with subtle modern accents for highlights. High contrast to ensure readability.
* **Typography**: Clean, highly readable sans-serif fonts suitable for both interactive reading and printed publication figures.
* **Figure/Map Export**: Export options must maintain publication-grade quality (customizable DPI, output formats like .TIFF and .PDF, and customizable themes/labels).

## 3. UX & Interaction Principles
* **Guided Stepper**: Structure the workflow logically:
  1. Data Ingestion & Exploratory Analysis.
  2. Model Selection & Variogram Optimization.
  3. Spatial Validation & Diagnostics.
  4. Final Export Selection.
* **Expert-Centric Overrides**: Provide direct access to raw parameters (e.g., manual variogram fitting override, custom interpolation power for IDW) to allow expert calibration.
* **Rich Interactivity**: Enable cross-linking between tabular data, PCA graphs, and spatial maps, including detailed hover-state audits of data points.
