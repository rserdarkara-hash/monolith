# User Guide

## Workflow at a Glance

Monolith is organised as six tabs, worked left to right:

1. **Data Setup** loads the table, maps coordinates and variables, and confirms the CRS. Nothing else unlocks until this is confirmed.
2. **Map Viewer** shows the interpolated surfaces and carries the drawing and export toolbar.
3. **Scientific Analysis** holds the variograms, performance tables and residual diagnostics for the last run.
4. **Run Log** records what each run did, including every warning and fallback.
5. **Descriptive and Exploratory Suite** and **6. Classification Suite** branch straight off Data Setup and need only the confirmed table.

The sidebar (Context, Spatial Engine, Map Styling, Management) configures interpolation runs. It is hidden while the Classification Suite is active, which carries its own controls.

## In-App Documentation

The header's **info (i) button** opens this documentation in a slide-in drawer with three tabs (User Guide, Scientific Guide, Descriptive and Exploratory Suite). While the drawer is open:
  - Click anywhere **outside the drawer**, or the ✕ button, to close it.
  - A **floating button column** in the lower-right corner navigates the open document: jump to the top or end, or step to the previous or next section heading.

## Data Ingestion & Configuration

The Data Setup tab presents the configuration as sequential step cards; later steps appear once a dataset is loaded.

The application requires cleanly structured, georeferenced tabular data. Column headings (the first row) hold parameter titles (`tn`, `p`, `k`, `ph`, `som`, `clay`), categorical or grouping factors (`subset`, `texture_class`, `locality`), and coordinate labels (`x`, `y`, `lat`, `long`). Rows hold the data values.

**Step 1. Upload dataset**
   - Click `Browse...` to upload a `.csv` or `.xlsx` file. It must contain distinct columns for X coordinates, Y coordinates and at least one variable of interest.
   - A sample dataset ships in the app directory.

      **Using the sample data?** `samp_data_1.xlsx` (paired with the variable list `samp_var_list.xlsx`) is a dataset with an associated manuscript submitted for peer review. It is provided strictly for demonstration, evaluation and testing of Monolith, and is **not** licensed for third-party use until that manuscript is formally published. On publication these datasets will be released under the [Creative Commons Attribution 4.0 International (CC BY 4.0)](https://creativecommons.org/licenses/by/4.0/) license. Until then all rights are reserved and the restrictions apply to all third parties. Full terms: [sample_data/DATA_LICENSE](sample_data/DATA_LICENSE).

**Step 2. Assign variables**
   - **X/Y coordinates:** select the columns holding longitude/latitude or easting/northing. A column is pre-selected only when its **whole** heading is a recognised coordinate name: `x`, `lon`, `long`, `lng`, `longitude`, `easting` for X, and `y`, `lat`, `latitude`, `northing` for Y (case and surrounding spaces ignored). Partial matching is deliberately not used, so names merely containing a coordinate fragment (`Longevity_index`, `Lateral_flow`) stay available as analysable variables. If nothing matches, the first column stays selected and you pick the right ones. The same rule decides everywhere else in the app which columns are coordinates rather than variables.
   - **Locality / grouping:** headers containing `locality`, `loc`, `site`, `farm`, `id` or `group` are offered for the "Locality" or a "Grouping Factor" selector, which filters analysis subsets later.

**Step 3. Coordinate reference system (CRS)**
   - The app parses the CRS from the data where it can. Coordinates within ±180/±90 suggest `EPSG:4326` (WGS 84) automatically. Metre-scale projected coordinates cannot reveal their zone or national grid, so the app asks you to select the correct projected CRS rather than guessing.
   - Check the **Detected CRS** badge. If it reads `Unknown` or is wrong, select the right EPSG code from the dropdown (`WGS 84 (EPSG:4326)` for raw GPS, or a local UTM zone for metres). Spatial calculations such as variogram distances fail or distort badly under a wrong CRS.
   - Both CRS dropdowns accept **free-typed entries**: any EPSG code (`EPSG:25832`), PROJ string or WKT. Typed values are validated before a run starts.
   - The **Target Mapping CRS** must be metric if it is projected. Every distance the app accepts or prints is in metres (resolution, buffer distance, variogram ranges, on-map measurements) while the engines use the CRS's own axis units, so a CRS in feet would make a "50 m" grid 15 m wide and a variogram range 3.28x its stated size. Selecting one raises a notification immediately and a run with it is refused; pick the area's UTM zone or another metric grid instead. Your **input** CRS may use any unit, and a geographic Target CRS is fine, since the pipeline projects it to a metric UTM zone itself.
   - The selected input CRS is cross-checked against the mapped X/Y columns. A geographic (degree) CRS with coordinates outside ±180/±90 raises an error notification; a projected (metric) CRS with degree-like coordinates raises a caution. Fix the CRS before running. The guard never blocks or alters a computation itself.
   - Boundary shapefiles carrying no CRS definition (missing `.prj`) trigger a warning: their coordinates are assumed to be in the analysis CRS, so re-export with a `.prj` if the boundary lands in the wrong place.
   - The confirmed X/Y mapping and CRS are also what the **Spatial Cross-Correlogram** in tab 5 uses to bin point pairs by ground distance. Until the mapping is confirmed that panel shows a "Coordinates are not mapped yet" note; every other correlation panel is unaffected.

**Step 4. Metadata and automated pairing (optional)**

A secondary configuration file (for example `samp_var_list.xlsx`) supplies readable labels and variable categories.

  **A. Upload metadata context**
  - Select a secondary `.xlsx`, JSON or TXT file containing your variable definitions.
  - **Label mapping:** the app scans for headers like `label` or `name` to display on all maps and reports.
  - **Category grouping:** headers containing `cat` or `group` let the UI organise variables into folders such as "Physicochemistry", "Remote Sensing" or "Terrain".

  **B. Automated variable processing**
  - **Target isolation:** non-numeric columns and the coordinates identified in Step 2 are filtered out; the remainder become target variables. A column counts as a coordinate only when its **entire name** is a coordinate label, so `Precipitation_cumulative` and `correlation_index` remain mappable variables.
  - **Smart pairing:** a suffix search links observed data with model outputs. `_cve` matches cross-validation predictions, `_ss` matches single-split predictions. A successful match creates a triad (Actual, Pred_cve, Pred_ss) so you can toggle between ground-truth and residual maps without re-linking.

  **C. Final validation**
  - **Mini-map check:** the validation mini-map colours points **by locality**, one colour per geographic group, with a permanent name label over each group and a locality legend. A point far from its named group, or a group plotted in the wrong part of the world, reveals a swapped X/Y pair or a wrong CRS before any modelling starts. Custom point styling set in the Variable Mapping step takes precedence here. The mini-map always carries a north arrow (top-left) and a metric scale bar (bottom-left); unlike the Map Viewer's equivalents these are not optional, because this map exists to be checked against reality. Returning to this tab after a run re-frames the mini-map on the samples, so it always opens showing the full data extent.
  - **Review mapping:** check the table at the bottom of the configuration panel to verify labels, units and prediction pairs.
  - **Confirm:** press **Confirm Variable Mapping** at the end of the page, then proceed to the Spatial Engine.

---

## Mapping: 1. Context

**Sidebar layout**
* The **1. Context**, **2. Spatial Engine**, **Map Styling** and **3. Management** sections are collapsible: click a section title to fold it away. Each remembers its state in the browser between sessions.
* The **Run Interpolation** button stays pinned to the bottom of the sidebar while you scroll, and a **status chip** in the header shows the run state from any tab: grey *Idle*, amber *Running* with a live percentage, green *Run ready* with the method.
* The **Variable** dropdown supports live search: start typing to filter long lists.

**1.1 Locality (spatial grouping)**
* The designated grouping column partitions the dataset. Select a single field, or "ALL" to run a batch-parallelized interpolation across multiple spatial domains at once. Clearing the selection is treated as "ALL": every analysis path (runs, optimizers, auto-fit, resolution suggestions) resolves it to all localities. Rows with a missing locality value are excluded from "ALL"-type selections.
* This selection seeds the **Spatial Scope** of the Classification Suite, whose locality picker can then be adjusted independently.

**1.2 Variable selection and category**
* **Variable category:** variables are organised into folders ("Soil Physicochemistry", "Terrain", "Satellite Indices") from your metadata file or automated detection.
* **Variable:** selects the target "Actual" column. Suffix and label matching links a target such as `Total N (%)` to the column `tn` and to its prediction columns `tn_cve` and `tn_ss` where those exist.

**1.3 Primary view and comparison mode**
* **Actual Values:** the interpolated surface of your raw measured data.
* **Best ML Predictions (_cve):** the interpolated surface of the machine-learning model's cross-validated estimates.
* **Single Split ML Predictions (_ss):** the surface for a specific data-split prediction.
* **Residuals (v - pv) of ML Predictions:** the localized difference between what you measured and what your uploaded model predicted. These residuals diagnose the ML model, not Monolith's interpolation.
* ML prediction and residual views are offered only when the selected variable has the corresponding prediction column (`_cve` for Best ML Predictions and Residuals, `_ss` for Single Split). Without prediction columns the Primary View is limited to Actual Values.
* **Comparison mode:** splits the display into a synchronized dual-map view for a side-by-side audit of actual data against predictions.
* **Sidebar selections configure the NEXT run.** Once a map exists, changing the Variable, Primary View, Interpolation method or Comparison Mode does not alter the displayed map or its Scientific Analysis results; they update when you press **Run Interpolation** again. Styling controls (Continuous/Binned/Agronomical, class limits, palette) stay live and restyle the displayed map immediately. To switch between surfaces the last run already computed, use the **View** dropdown in the Map Viewer toolbar, which is instant and needs no re-run.

**1.4 Data subset (Single Split view only)**
* With the Primary View set to **Single Split ML Predictions (_ss)**, a **Data Subset** dropdown appears beneath it and restricts the mapped data to one modelling partition ("Train", "Test", "Validation") before interpolation.
* Choices are read from a `subset` column in the uploaded dataset, matched case-insensitively. Without such a column only "All" is offered.
* The filter applies to the Single Split view only; all other views use the full dataset.

**1.5 Drawing on the map: custom groups and exports**

The interactive maps carry a **drawing toolbar** on their left edge for sketching polygons, rectangles and point markers. This supports two workflows.

* **Assigning a custom locality or analysis group:**
    1. Draw a shape around the points you want to group.
    2. An **"Assign Locality / Analysis Group"** dialog opens when the shape is finished. Enter a group name and click **Save Group**.
    3. All points inside the shape are written to an `Assigned_Locality` column, and the app reports how many were captured. Select that column as the Locality/Grouping column in Data Setup to analyse your custom zones.
* **Exporting your work:** three buttons sit at the right end of the Map Viewer toolbar. The first two share the format dropdown to their left (Shapefile ZIP, GeoJSON, KML, GPKG). Both grey out whenever the export cannot be produced, and hovering a greyed-out button names the blocking state, so the requirement is visible before you click.
    * **Export Drawn Polygons:** active once at least one polygon has been drawn. Downloads *all* drawn polygons for reuse in a GIS. It exports the shapes **you** drew; the app's own classified zones have their own button.
    * **Export Class Zones:** downloads the class zones of the surface **currently displayed** as a GIS vector layer, one dissolved polygon per class, carrying the class label, its lower and upper break limits, its area in hectares, and which surface, variable and method produced it. It follows the view switcher, so Actual, Predicted, or both under the comparison view. It requires Agronomical or Binned styling (Agronomical also needs **APPLY TO MAPS & STATS** pressed first), so it stays inactive until a run has produced a surface, while the map uses the continuous colour ramp, and while the residual (Delta) view is showing, which is not classified into zones. The polygons are classified raster cells dissolved per class, so their boundaries are cell boundaries and are not smoothed, and their hectares are the numbers the Area Coverage table reports. Shapefile and GeoPackage keep the analysis CRS in metres; GeoJSON and KML are reprojected to WGS84 as those formats require.
        * **Which format to pick:** GeoPackage and Shapefile carry the attributes as queryable fields, so choose one of those to filter, join or symbolise by class in a GIS. KML stores only a name and a description per shape, so the export puts the class label in the name and the full attribute record in the description. Everything is there, but as text rather than columns.
    * **Export Updated Dataset:** downloads the current dataset as `.xlsx` including the `Assigned_Locality` column, so group assignments survive the session. Use it after saving drawn groups.
* Shapes can be edited or deleted with the toolbar's tools, and the polygon export always reflects the current set. Deleting a shape does **not** undo a group assignment already saved from it.

**1.6 Measuring distances and areas on the map**

A **ruler** sits in the bottom-left corner of every Map Viewer map, including both panels of the comparison and residual views, so the same feature can be measured on the Actual and the Predicted surface without switching views. (The Data Setup mini-map is a preview and has no ruler.)

1. Click the ruler icon, then **Create a new measurement**.
2. Click each point along the path. The panel keeps a running path distance, and two icons sit beneath it: a cross to abandon the measurement and a tick to finish it.
3. Click the last point again, or the tick, to finish. Three or more points close the shape, which is reported as an enclosed area together with its perimeter.

The result appears in the measurement's own pop-up, on the shape itself. Click any measurement to reopen it: each one carries its own figures, so several can sit on the map at once without their numbers being confused, and the pop-up holds **Center on this** and **Delete** links for that shape alone. Every figure is reported on two bases:

* **Ground (WGS84):** the geodesic distance on the WGS84 ellipsoid. This is the real distance over the ground and does not depend on the projection you chose.
* **Projected (your Target Mapping CRS):** the same path measured in the coordinate system the models work in. This is the number to compare against a variogram range, a grid resolution or a buffer radius, because those are all expressed in that system. It names the CRS of the run **on screen**, so changing the sidebar CRS for your next run does not relabel a measurement taken on this one.

The two normally agree to within a few hundredths of a percent, and up to about 0.1% near the edge of a UTM zone. That much is ordinary and means nothing is wrong. A gap far beyond it means the Target Mapping CRS distorts distances over your area, which is worth knowing before reading a variogram range: check that the CRS suits the region (Scientific Guide Section 3.5). When the Target Mapping CRS is geographic, only the ground figure is shown, because a length in degrees is not a length.

A path that crosses itself reports its perimeter but no area, because the enclosed area of a figure-eight is not well defined. Redraw it without the crossing to get one.

These figures replace the text the measure control writes for itself, which is a quicker spherical approximation, so a shape carries one set of numbers rather than two. An open two-point line reports a **Length**; from the third point the shape closes and reports a **Perimeter**, the boundary of the same ring the area is measured over. Measurements are for reading the map only: they are not stored, not exported, and change nothing about a run. A measurement stays on the map until you delete it or the map is redrawn by a new run or a view change.

## Mapping: 2. Spatial Engine Selection & Tuning

To view uncertainty maps instead of the interpolated parameter surface, tick the relevant option at the bottom of the Spatial Engine panel. It becomes available once a kriging map (OK, RK, RFK, CK) has been generated and restyles the displayed map instantly, without a re-run.

**1. Select the interpolation method**
   - **Deterministic (fast):** **IDW** weights by distance, **TPS** fits a smooth surface by minimizing bending energy. Both are ideal for rapid visualization and neither produces a prediction variance.
   - **Geostatistical (standard):** **Ordinary Kriging (OK)** models spatial autocorrelation through a variogram, giving the best linear unbiased predictor.
   - **Multivariate:** **Co-Kriging (CK)** exploits cross-correlation between the target and a densely sampled auxiliary variable, for example using sensor conductivity to improve a clay map. Selecting CK reveals a **CK Max Neighbors** slider (5 to 60, default 15) setting how many nearest samples of each variable enter every prediction. This is a modelling decision, not a speed setting: smaller values assume the field's mean and covariance are stable only locally, larger ones approach a global system and suit sparse, stationary data. If your sampling is unusually sparse or the fitted cross-variogram range is long, try raising it and compare the Model Performance metrics. The value used is recorded in the run-configuration summary. Scientific Guide Section 1.4.
   - **Hybrid / ML:** **Regression Kriging (RK)** and **Random Forest Kriging (RFK)** combine environmental trends (topography, satellite data) with kriging of the residuals.

**1.b. Choose the cross-validation strategy**
   - The **Cross-Validation Strategy** selector sits directly beneath the Spatial Engine dropdown. It changes the reported diagnostics only, **never the interpolated map**.
     - **Auto (default):** leave-one-out CV at 50 or fewer points, seeded random 10-fold above 50.
     - **Standard LOOCV:** full leave-one-out at any size. The most rigorous choice, slow beyond a few thousand points, especially for RK and RFK.
     - **Spatial Block CV:** holds out ten spatially contiguous k-means clusters, curbing the optimistic bias random folds show under spatial autocorrelation. Recommended for spatial validation; below 30 points it reverts to LOOCV.
   - The strategy actually applied is shown next to the **Model Performance** table and in each locality's *CV Type* row.
   - **Comparing engines on the same data.** What a fold re-estimates depends on the engine, and the note under the Model Performance table says so for the run on screen. RK and RFK refit the trend model *and* the residual variogram in every fold; OK and CK re-solve the kriging system per fold against a variogram fitted once on all the points, so each held-out point helped build the model that predicts it. The resulting optimism is small but systematic: **an OK or CK score that beats RK or RFK by a narrow margin is not yet evidence that the engine is better.** Comparisons within an engine are unaffected. IDW's power exponent and a fixed TPS lambda carry the same reuse. Scientific Guide Section 5.
   - **Repeated CV (fold-realization stability):** a checkbox under the strategy selector, off by default and hidden under Standard LOOCV. Left off, the metrics come from a single fold assignment, which is reproducible and keeps method comparisons paired. Tick it, pick 3, 5 or 10 realizations, and the cross-validation re-runs under that many fold assignments; the Scientific Analysis tab then shows a **Fold-Realization Stability** table where every metric reads *mean ± SD*. Use the SD as the resolution of your comparison: **if two methods' RMSE values differ by less than it, the ranking is fold luck, not skill.** Three practical points: the map and the Model Performance table are identical either way, because realization 1 is the reference run; it costs one full extra cross-validation per realization, so five realizations is roughly five times the cross-validation time for RK and RFK; and leave-one-out folds are deterministic, so localities validated by LOOCV contribute a single realization and the Run Log says so. The realization count is recorded in the run configuration and its downloadable JSON.
   - The summary tables on the Scientific Analysis tab (variogram parameters, model performance, prediction and classification metrics, area coverage, descriptive statistics) are sortable by column header. Hovering a **Model Performance** header shows a plain-language definition of that metric.
   - **Reading the Moran columns.** The table reports Moran's I of the cross-validation residuals and, beside it, a two-sided **Moran p**. Hover an I value to see that row's expected value under no spatial autocorrelation, which is −1/(n − 1) and therefore slightly *negative*, not zero: at 30 points a reading of +0.01 sits essentially on the null expectation. Check I against the tooltip value and the p-value before concluding the model missed a spatial trend. An `NA*` in either column means the statistic **could not be computed** for that point set (too few cross-validated points, missing coordinates, or a failed neighbour search) and never that no structure was found; the Run Log states which. The p column is also `NA*` on the rare fallback weighting, which has no significance test attached.
   - **Pooled rows are summaries, not scores.** With *Filter Analysis View* set to **Total (Combined)**, a note under the cross-validation badge reminds you that the pooled R² and NSE are computed against the pooled mean of all localities. Where localities sit at different levels that between-locality spread inflates both numbers, and the pooled row can look better than every individual locality. Judge model skill on the per-locality rows; pooled RMSE, MAE and Bias are unaffected.
   - A **Variable naming** radio next to *Filter Analysis View* switches every diagnostic on the tab (variogram titles, RF importance panels, cross-variogram headers, RK coefficient tables) between the human-readable **labels** from your metadata mapping (default) and the raw **column names**. The toggle is cosmetic; no computed value changes.
   - Every diagnostic plot sits in a card with two header buttons: **download** saves a 300-dpi PNG of exactly what is shown, on a 9 x 7 in page at the type sizes on screen, and **expand** opens a large modal with a *Static (High-Res)* / *Interactive (Hover/Zoom)* toggle. Interactive mode adds per-point hover readouts: lag distance, semivariance and pair counts on variograms; observed, predicted and residual on scatter plots.

**1.c. Covariate helpers (RK / RFK / CK)**
   - **Predictor Ranks (Correlation):** the *Calculate Correlations* button ranks candidate covariates by their correlation with the target, computed **within the localities selected in the Context panel** and stamped with that scope and sample count. Each rank is an independent bivariate screen, so it uses every sample where both the target and that candidate are present and carries its own **n** beside the coefficient; the scope's sample count is not the n behind any one rank (Scientific Guide Section 8.7). A candidate sharing fewer than three samples with the target is not ranked. Correlations across all samples can be driven by between-locality contrasts that do not hold inside a single locality, and can hide ones that do, so re-press the button after changing the locality selection. *Tip: if a covariate ranks high for "ALL" but drops when you select your target locality, the relationship is regional, not local.*
   - **Multicollinearity gate:** launching RK, RFK or CK with several covariates triggers an iterative VIF screen (threshold 10) **on the same selected-locality data**, with an Auto-Drop / Keep All choice. Your answer is remembered until the method, the covariate set or the locality selection changes, so each new spatial context is re-screened, and it applies to Co-Kriging as well. Choosing **Keep All** genuinely keeps every collinear covariate; the only exception is constant covariates, which are always excluded because they carry no information and would break the regression fit. "Constant" is judged relative to each covariate's own scale, so covariates that are simply small in magnitude (clay as a 0-1 fraction, normalized indices, values in km or Mg) are kept and modelled normally. The screen needs at least two covariates; a run whose **single** covariate turns out to be constant within a locality still proceeds, with the trend simply carrying no covariate information there, and the run warnings name the covariate.

**2.a. Variogram optimization (geostatistical engines only)**
   - Selecting a kriging method reveals the Variogram Panel.
   - **Auto-Fit:** click this first. The engine fits four theoretical models by least squares and selects the best. Review the plotted curve against the scatter points.
   - **The optimizer runs in the background.** **OPTIMIZE ALL VARIOGRAMS** greys out and reads *Optimizing…* while the search runs on parallel workers, and the rest of the app stays usable: you can switch tabs, inspect data, read the guides or open the descriptive panels. The fitted variograms and the diagnostics dialog appear when it finishes, and the button restores itself even if the run fails. Only one optimizer runs at a time, and an optimizer and an interpolation run cannot overlap: each parallelizes across most of your machine's cores, so starting one while the other works raises a notification asking you to wait.
   - **Manual override:** if the auto-fit misses the short-range variability (the points closest to the Y axis), switch to `Manual Tuning`. Raise **Nugget** for very noisy data, set **Partial Sill** to the upper plateau of the points, and set **Range** where the curve flattens.
   - Switching to Manual Tuning, or changing the *Locality to Tune*, moves the Scientific Analysis locality filter with it, so the variogram you are tuning is the one on screen: the fitted curve carries your manual model as a red dashed overlay with its SSE in the subtitle. This works straight after **OPTIMIZE ALL VARIOGRAMS**, before any interpolation has run.
   - **Manual fits apply to Ordinary Kriging only.** A hand-tuned model describes the variogram of the *measured values*, which is what OK kriges. RK and RFK remove a trend first and model the variogram of the **residuals**, and CK fits a linear model of coregionalization across the target and its covariates; imposing a value-scale model on either would be scientifically wrong, so both refit their own variogram automatically. The panel says so while RK, RFK or CK is selected, and starting such a run with stored manual fits raises a one-time notification and a Run Log line rather than ignoring your tuning silently.

**2.b. IDW optimization**
   - Selecting IDW reveals the related panel, which calculates optimal parameters per locality. When optimized, a table details the power factor chosen for each spatial domain.
   - Adjust the **Max Neighbors** slider (default 12) *before* optimizing, to keep distant, unrelated points from distorting local predictions.
   - The search uses a deterministic cross-validation scheme (leave-one-out up to 50 points, a seeded 5-fold partition above that), so repeated optimizations on the same data give identical results.
   - The chosen power is then evaluated on the same cross-validation data it was selected on, with no nested loop, so an optimized IDW model's reported metrics are mildly optimistic. Keep that in mind when comparing IDW against methods that were not tuned this way.
   - The search runs on projected metric coordinates. A geographic upload is transformed to its local UTM zone first, so the chosen power reflects true ground distances and matches the surface the run produces.
   - **OPTIMIZE IDW FACTORS runs in the background** (see 2.a): the dashboard stays responsive, the button reads *Optimizing…* until the per-locality powers are stored, and only one optimizer runs at a time.
   - Manual override is available.

**2.c. TPS optimization**
   - Selecting TPS reveals the related panel. The lambda slider defaults to `< 0` (Auto GCV), so generalized cross-validation determines the smoothing parameter natively during interpolation.
   - Click **Optimize TPS Lambda** to extract locality-specific lambda values explicitly and generate a table of them.
   - A lambda fixed this way, or typed in manually, is reused unchanged in every cross-validation fold, so it was chosen using the points it is later scored on and the reported metrics are mildly optimistic. Auto (GCV) has no such issue, because each fold re-optimizes lambda on its own training points. If the metrics are what you intend to report, prefer Auto and use the optimizer to inspect the GCV curve.
   - As with IDW, the search runs on projected coordinates, so a geographic upload is transformed to metres first and the extracted lambda agrees with the run.
   - **OPTIMIZE TPS LAMBDA runs in the background** (see 2.a), with the same responsiveness and one-at-a-time rules.
   - Manual override is available. Setting `Lambda = 0` forces exact interpolation with no smoothing.

**3. Define the grid resolution**
  Resolution is the pixel size of the final map. Resolutions and their suggestions are always in **metres**, even under a geographic mapping CRS, because interpolation always runs in a projected CRS.
* **Auto (Per Locality):** the app measures the average nearest-neighbour distance for *each field* and suggests a pixel size of half that distance, so dense plots get detailed maps and sparse regions stay efficient.
* **Auto (Global):** uses the point density of the *entire dataset* to suggest one uniform resolution.
* **What an Auto run actually builds:** the figures above are the sidebar suggestion and the per-locality table. When you press Run, each locality's grid is sized from that locality's **own mapped area**, about 100,000 pixels inside its boundary, never finer than 5 m and never coarser than 1000 m. Small fields still get fine grids and large ones coarser grids, and the nearest-neighbour figure continues to drive the dynamic buffer width. Auto resolution ignores the **Fixed** slider entirely.
* **Fixed:** manual control.
    * **Low values (1-5 m):** high detail, high RAM use, longer processing.
    * **High values (50-300 m):** faster, but local variation may be lost.
    * As a guard, a fixed resolution that would produce more than roughly 4 million grid cells across the map extent is coarsened automatically, with a warning in the run log. Ordinary resolutions are used exactly as set.
    * The opposite extreme is also caught: a resolution so coarse that no grid node lands inside a locality's boundary, easy to hit with a tight Strict Point Buffer, skips that locality with a run-log warning naming the cause instead of producing an empty map.

In every mode the per-locality summary table under the slider, and the on-map resolution overlay, follow the current locality selection.

**4. Borders and polygoning logic**
   Define the analysis envelope so the model does not interpolate indefinitely:
* **Concave Hull:** shrink-wraps the boundary to the outer perimeter of the points. Best for irregularly shaped fields.
* **Convex Hull:** a rubber-band boundary. Best for simple, rectangular fields.
* **Wrapped (Buffered):** a smoothed buffer around the concave hull, so the map covers the field edges.
* **Strict Measured (Point Buffer):** individual buffers around every point.
* **Buffer Distance (m):** how far the map extends beyond the outermost samples.

**Match the buffer to the cell size in Strict Measured mode.** A grid cell is drawn only when its centre lies inside the boundary, and in Strict Measured mode the boundary is just the circles around your points. A sample can sit up to half a cell diagonal from its own cell's centre, so keep the buffer at or above **resolution ÷ 1.41** (equivalently, resolution at or below **buffer × 1.41**). A 175 m buffer on a 350 m grid falls short, and roughly a fifth of isolated samples then appear as points sitting on blank map. The sidebar flags the mismatch under the resolution table when resolution is Fixed, and every run re-checks it against the resolution actually used, raising a notification and writing the corrective numbers to the run log. An uploaded boundary shapefile takes over wherever it matches a locality, so the advisory then concerns only the localities it does not cover. Lowering the resolution is usually the better fix, since widening the buffer enlarges the area the map claims to cover.

*The Boundary Type, Buffer Logic and Resolution Logic configured here apply to **interpolation runs only**. The Classification Suite has its own boundary, buffer and resolution controls in its Spatial Scope panel.*

**5. Execution and run estimation**
   - **Run estimate:** before execution the UI shows a dynamic indicator ("~1 locality model(s), ~1.5 minutes estimated").
     - **History-aware:** with enough prior runs logged for the chosen method, a linear model predicts runtime from sample counts.
     - **Cold start:** without history, base multipliers are used. RFK (1.0x), RK (1.0x) and CK (1.3x) are calibrated from real measurements; OK (0.5x), IDW (0.5x) and TPS (0.3x) are unverified estimates based on theoretical complexity.
   - Click **Run Interpolation** and move to the **Map Viewer** tab. The app cross-validates under the strategy selected in 1.b, generates the surface, and populates the performance table with RMSE, R², Moran's I and the rest. Both the cross-validation seed and the Moran neighbour count are hardcoded; see Scientific Guide Sections 9.1 and 9.2 to change them.
   - **Cancelling a run:** the cancel takes effect at the next checkpoint. Checkpoints sit before each locality's Actual and Predicted surfaces and, for RK and RFK, between covariates as they are interpolated onto the prediction grid. The covariate stage is often the longest stretch of a multi-covariate run, so a cancel there is picked up after the current covariate finishes.
   - **Too many covariates for the sample size:** Regression Kriging fits one coefficient per covariate plus an intercept, so both the main fit and each cross-validation fold need more points than that. If a locality or a held-out fold falls short, the run says so explicitly, naming how many points are needed against how many exist, instead of returning blank `NA` metrics. Reduce the covariate count, add samples, or pick a strategy with smaller held-out folds.
   - **Failed regions:** if one or more localities error out, a single summary dialog lists **every** failed region with its full error text. Per-region error notifications appear alongside it, and the full text is retained in the run log.
   - **Results assembly errors are reported separately from model errors:** once the parallel workers finish, the app still has to merge rasters, build tables and register export items in the main session. A failure in *that* stage opens a **Results Assembly Failed** dialog stating that the models themselves completed and that maps, tables or exports may be incomplete.
   - **Automatic fallbacks:** if an engine cannot be fitted for a locality (TPS on a degenerate point layout, or a CK/RK/RFK model failure), the app falls back to a simpler engine and reports this in the run log and as a warning notification. Check the log if a map looks different from the method you selected.
     - **The affected diagnostic panels say so in place.** A fallback leaves a locality without the diagnostics belonging to the method you chose: a fallen-back RK locality has no trend model, an RFK one has no forest. The Trend Model Summary, RK coefficient table and RF Variable Importance panels state that the locality's trend model was not fitted and point you at the log. The per-locality residual variogram is titled *Variogram of Measured Values - Ordinary Kriging Fallback*, because after a fallback the fitted variogram describes the measured values, not model residuals. A cross-validation row that could not be computed is labelled "CV unavailable" rather than shown as blanks. Likewise, if an uploaded boundary shapefile cannot be applied to a locality (projection failure or no spatial overlap), the run proceeds with the point-derived boundary and says so in the progress warnings.
   - **Responsive diagnostic views:** validation diagnostics that do not apply to the active engine or the current view (RF Variable Importance, internal residual variogram, TPS GCV plot) are hidden rather than rendered blank.
   - **Directional Variogram (anisotropy check):** a panel in the Scientific Analysis tab, shown for **every** engine including IDW and TPS, which fit no variogram of their own. It recomputes semivariance separately in four compass directions (N-S, NE-SW, E-W, NW-SE) instead of pooling all pairs regardless of orientation. Use the radio above the plot to compute it on the **measured values** or the run's **cross-validation residuals**; residuals are the more informative view for RK and RFK. If the four curves reach their plateau at clearly different distances, the field's structure is directional and a single range under-describes it. This is a diagnostic only: all engines remain omnidirectional, so nothing on the map changes because of what you see here. Treat a strong split as a caveat to report, not a setting to adjust. Select a locality to compare directions within one field, or "Total (Combined)" to pool every locality in the run.
   - **RK linear trend panel:** for Regression Kriging runs, the Scientific Analysis tab shows the trend model as fit-statistic chips (R², adjusted R², residual standard error, F statistic with p-value, sample size) followed by a sortable coefficient table with each covariate's estimate, standard error, 95% confidence interval, t value, p-value and significance code. Select a specific locality in the analysis filter to view it, since trend models are fitted per locality. The R console summary sits under the collapsible *Raw R model summary* link, and the exported coefficient table keeps full-precision values.

---

## Mapping: 3. Map Styling

**Colour palette configuration**
- Styling can be changed before or after generating the interpolation models. Switching between Continuous and Binned, changing the palette, or toggling uncertainty restyles the displayed maps **in place**: the base map, zoom level and overlays stay put and only the coloured surface layers and the legend are re-encoded. A pulse bar at the top of the page and spinners on the statistics tables show when the app is working.
- Choose a classification method:
    * **Continuous:** best for raw gradients.
    * **Binned (5):** splits the displayed value range into five equal-interval bands. Applies immediately.
    * **Agronomical:** class-based management zoning. Selecting it reveals the algorithm picker (Supervised thresholds, Jenks, or K-Means), the class-count slider, and for Supervised the limit boxes with a hint showing the displayed surface's value range. All are editable freely, because nothing recomputes while you adjust them. Press **APPLY TO MAPS & STATS** to commit: the maps restyle and the class-area and classification-performance tables recompute in one pass. The button greys out and reads "APPLYING..." until that pass finishes. An amber note appears whenever the staged settings differ from what the maps show. Applying re-encodes every visible map layer and recomputes class areas, so expect a few seconds of work, longer for multi-locality or comparison views.
        * Jenks and K-Means breaks are computed with a fixed internal seed (Jenks on a seeded subsample for large rasters), so re-applying the same settings reproduces the same class limits. Above roughly 500,000 cells the interactive viewer shows a resampled preview for responsiveness; exports, GeoTIFFs and all area and classification statistics always use the full-resolution raster.
        * Applying the agronomical classification enables the **classification performance table for uploaded parameter predictions** in the Scientific Analysis section.

---

## Finalizing & The Export Registry

The Export Registry standardizes outputs for reports and presentations.

- Open the export styler for individual downloads after choosing a single result, or select several results for a batch download.
- All tabular results selected are written into a single `.xlsx` file with one sheet per result category.
- **Download Run Configuration (.json)** writes a machine-readable record of the run currently in the registry: every model setting (method, localities, view, CRS, boundary and resolution logic, cross-validation strategy, the resolved collinearity gate including a "Keep All" override, and the method-specific parameters: IDW power and neighbours, TPS lambda, CK neighbourhood, RFK trees and uncertainty method), the per-locality tuning values the run consumed, and the software provenance (app version, R version and platform, and the versions of sf, gstat, automap, fields, randomForest and terra). Keep it beside the exported maps: it is what lets you reproduce a surface months later, and it supplies the parameter list a methods section has to state. The file always describes the **current** run, so download it before starting the next one, or restore an archived run from the Run History Archive first.

**1. Accessibility and themes**
   - In the bottom left corner of the export styler, under advanced settings, use **High-Contrast Mode** for colourblind compliance.
   - Residual and error maps use a zero-centered diverging palette selectable under **Residual / Error Maps** in the Basic tab (default Red-Blue). With High-Contrast Mode on, a non-colourblind-safe choice is replaced automatically by Purple-Orange.
   - Two error-map assets are registered per run with uploaded predictions: the **Point Error Map** (discrete errors at the exact sample locations, matching the Map Viewer's Point Residuals panel) and the **Interpolated Point Errors Map** (the IDW surface of those errors).
   - **Uncertainty products (Variance and SE maps) are registered for the kriging engines only:** OK, RK, RFK and CK. IDW and TPS are deterministic weighting and smoothing interpolators with no prediction-variance model, so a run using either has no uncertainty item in the registry and no uncertainty band in its GeoTIFF. This matches the Map Viewer, where "Map Uncertainty Instead of Interpolation" takes effect only for a kriging run.

**2. Typographical scaling and advanced options**
   - Advanced options give individual size and orientation control over every figure element, and the configuration can be saved for future sessions.
   - Sizes are **points** and margins are **millimetres**, both physical: a 16 pt title is 16 pt on the page. Nothing is rescaled at export time, so the size you set is the size you get at any DPI and any export dimensions.
   - The **preview is the export figure**, drawn at the width and height you set and then scaled as a whole to fit the preview pane. Nothing is cropped: if an axis label is crowded or clipped in the preview it will be crowded or clipped in the file, so widen the figure, shrink the type, or angle the labels until the preview looks right.
   - **Legend placement follows the item.** An Actual vs Predicted comparison opens with a horizontal legend beneath its two panels, where a right-hand legend has no room; every other item opens with the legend on the right. This is applied on top of the remembered styling each time the styler opens, so a legend position set by hand holds for that session but is not carried onto a differently shaped item. A configuration you load with **Load Config** overrides it.

**3. DPI configuration**
   - Choose the target resolution under **Output Quality**: `72 DPI` for quick sharing, `300 DPI` for internal reports and printing, `600 DPI` for journal submission.
   - DPI changes the pixel count only. Layout, type size and margins are identical at 72, 300 and 600 DPI.

**4. Download**
   - Select the format (`PNG`, `TIFF (image)`, `JPEG`, `PDF`, or `GeoTIFF (data)`) and click **Finalize and Download**.

**5. GeoTIFF: the data rather than a picture of it**
   - **GeoTIFF (data)** appears in the format list only for registry items that are a single raster surface: the Actual and Predicted maps, the residual Delta map, the Interpolated Point Errors map, the variance and standard-error surfaces (kriging runs only), and any Quick Export of those views. It writes the raster values, CRS and extent exactly as computed, in the run's projected analysis CRS, ready for QGIS or ArcGIS.
   - Because it carries values rather than a rendering of them, **no styling applies**: typography, palette, DPI, dimensions and the preview all switch off while it is selected. Symbolise the layer in your GIS instead. **Kriging** surfaces are written as multi-band files, band 1 the prediction and band 2 the variance, with layer names kept as band descriptions. **IDW and TPS** surfaces are single-band prediction rasters, since neither engine produces a variance.
   - Two map items are **not** single rasters and stay image-only: the **Actual vs Predicted Comparison** (a pair of surfaces) and the **Point Error Map** (point geometry). The styler says so when one is selected. Export the Actual and Predicted maps separately for the first; for vector geometry use **Export Class Zones** and **Export Drawn Polygons** on the Map Viewer toolbar.
   - In a batch export, GeoTIFF applies to every raster item selected; anything else in the same batch is written as PNG, and the Run Log records which items that happened to. Tables continue to be merged into the single Excel workbook.

## Classification Suite

Tab 6 trains a supervised multiclass classifier from co-sampled covariates, either on an existing categorical column or on a continuous variable binned into ordered classes (quantile, equal-interval or Jenks breaks). Method details: Scientific Guide Section 10.

The suite is self-contained: while this tab is active the interpolation sidebar is hidden and replaced by a short notice. Everything a classification run needs (target, predictors, localities, boundary, buffer, grid resolution) lives in the suite's own left-hand panel and never interacts with the interpolation configuration.

**Setup**
- The **Target** selector opens on *Bin a continuous variable*, because most soil datasets carry no text or factor column to classify. Switch to *Categorical column* if yours does.
- Choose the target, one or more covariates, and a method (Multinomial logistic, Random Forest, or XGBoost). For two-class targets the multinomial option automatically fits the statistically equivalent binomial logistic regression.
- Only text and factor columns are offered as categorical targets. Numeric columns are always treated as continuous, even when they carry few distinct values, as coarse climate-raster covariates often do. If your classes are stored as numeric codes, recode them to text or use the binned-target mode.
- **Collinearity guardrail:** an amber note under the covariate picker warns when the selected covariates are redundant *within the current spatial scope*, and at run time the same Auto-Drop / Keep All / Cancel dialog appears that the kriging engines use. The screen is method-aware: iterative VIF > 10 for most learners, but a stricter VIF > 5 when **Random Forest** is selected, because moderate collinearity that barely hurts RF predictions still splits the permutation importance between the correlated covariates and makes the true drivers look weak. *Tip: five flavours of the same elevation layer add no information and only smear the importance chart across near-duplicates. Prefer dropping.*
- **Balance classes (inverse-frequency weights):** when rare classes matter (100 "healthy" against 5 "diseased"), tick this so each class contributes equally to the fit instead of the model defaulting to the majority. Metrics stay unweighted; expect slightly lower overall accuracy in exchange for better rare-class recall. Not supported by the multinomial learner, which then fits unweighted and says so in the run badge. *Tip: weighting is a stopgap, not new information. The real fix for a 5-sample class is more field samples.*
- **Cross-validation:** Spatial (blocked) clusters nearby points into folds so reported accuracy reflects prediction into unsampled areas. Standard (random k-fold) is usually optimistic under spatial autocorrelation.
- **Hyperparameter tuning:** None fits fixed defaults (fast, deterministic); Light and Full search a grid over the folds. The search uses whichever cross-validation strategy you picked, for the scored folds *and* for the final model behind the maps and the exported bundle, so a Spatial CV run tunes under spatial folds throughout.
- **Use nested CV (slower):** appears once tuning is enabled. By default the same folds both choose and score the hyperparameters, which is mildly optimistic; nested CV instead re-runs the grid search inside every fold, on 5 inner folds built from that fold's training rows only, so the reported metrics honestly include the tuning step. Expect roughly 5x the tuning runtime, and the badge then shows "(nested CV)". *Tip: use it for the run you intend to report or publish; the quick estimate is fine while exploring.*
- **Feature importance scored on:** choose whether permutation importance is measured **Out-of-fold** (default) or on **Training rows**. Out-of-fold reuses each fold's own model and shuffles predictors only in the rows that fold never saw, so importance is measured under the same honest design as the reported accuracy. Training-row scoring is the conventional default but rewards a flexible model for memorising its training data. Both cost the same, and the design in force is written on the importance plot's axis and into the exported metrics CSV. Switching this control cannot change the reported performance: the shuffles run in their own isolated random stream, so accuracy, kappa, the confusion matrix and the per-class table come out identical either way. *Tip: the two designs are not comparable numbers, so do not read an out-of-fold importance against a training-row one.*
- If the scoped data are thin, the run warns, naming which classes have fewer than 3 samples and their counts, but still proceeds. Results for those classes will be unreliable: widen the scope, merge or exclude rare classes, or use fewer classes for a binned target.
- A second warning can appear when the run finishes, naming classes that **some cross-validation fold had no training samples of**. This is normal with Spatial (blocked) CV, whose folds are geographic blocks and cannot be balanced by class: a class confined to one part of the area ends up wholly inside one block, and the model trained without that block cannot predict it. Those samples count as errors, so the reported accuracy for that class reads worse than the model deserves. The run is still valid; read the per-class table with the warning in mind. To avoid it, use fewer classes, prefer quantile over equal-interval breaks (equal-interval bins can be nearly empty on a skewed variable), switch to Standard CV if an in-domain estimate is what you want, or widen the scope.
- Binned targets can produce an interval containing no samples at all, most often with equal-interval or Jenks breaks on a skewed variable. Empty classes are dropped automatically, so the class count behind the maps and tables can be lower than the "Number of classes" slider. The run summary always reports how many classes were actually modelled.
- **Predict maps** additionally predicts class, per-class probability and uncertainty (entropy) surfaces on a grid. Boundary type, buffer logic and grid resolution are set in the suite's own **Spatial Scope** panel. If a fixed resolution is too coarse for a small scope, so that no grid cell lands inside the boundary, the run stops with a clear message instead of predicting over the whole bounding box. Reduce the resolution or widen the scope.
- **Run progress and cancellation:** the running panel shows a live progress bar captioned with the stage actually running: hyperparameter tuning, cross-validation fold *k* of *n*, the final model fit, permutation importance, building the prediction grid, interpolating covariates onto the grid, and classifying the grid cells. On a large grid the last two stages are normally the longest part of a run, so the bar keeps moving there rather than parking near the end.
  - *First run of a session:* before any stage appears the caption reads "Starting (loading modelling libraries, first run ~30s)". The classifier runs in a background process that must load the modelling libraries from scratch the first time it is used, roughly 30 seconds; later runs in the same session reuse the warmed process and start almost immediately. The rest of the app stays open during this wait, though other on-screen analyses may respond sluggishly while the libraries load.
- The **Cancel Run** button takes effect at the next checkpoint. Checkpoints sit between cross-validation folds, between covariates during grid interpolation, and between blocks of grid cells during classification, so a cancel is usually picked up within a few seconds. The exceptions are the two black-box search steps, a tuning grid search and a single covariate's kriging, which must finish first. Once cancelled the run stops and no partial results are kept: the Run Classification button becomes available again and the previous run's results stay on screen.

**Spatial Scope**
- The **Localities** picker restricts the run (training, cross-validation, binning breaks and the predicted maps) to the chosen localities. It is specific to this suite: it starts with all localities selected and is never overwritten by the sidebar Context panel, so the scope you set here is the scope the run uses. Leaving it empty means all localities.
- **Boundary Type / Buffer Logic / Grid Resolution** live in this panel and apply to classification maps only: Concave Hull (default), Convex Hull, Wrapped (Buffered, with dynamic or fixed buffer distance), or Strict Point Buffer, applied per locality in scope. The grid is either Auto, a cell size derived from the boundary area targeting about 50,000 cells, or a Fixed cell size in metres.
- Once at least one polygon exists, drawn with the map toolbar or uploaded as a shapefile, a **Polygon scope** control appears: *Ignore polygons*, *Within localities* (points must lie inside both the selected localities and a polygon), or *Polygons only* (the locality filter is ignored).
- The live "In scope: x of y georeferenced points" note shows exactly what the run will use, and turns red below 20 points. Under Strict Point Buffer it is joined by a warning whenever the buffer is narrower than half a cell diagonal (**resolution ÷ 1.41**), which leaves isolated samples with no mapped cell beneath them; the note reads the effective cell size in both Auto and Fixed modes, and the finished run repeats it as a notification. It stays silent under *Polygons only*, where the domain is your polygons and the Boundary Type setting has no effect.
- The predicted maps cover the union of per-locality boundaries, or the polygons, so the classifier does not predict across unsampled gaps between distant localities.

**Results**
- The results panel is a 2 x 2 plot grid: Predicted Class Map and Prediction Uncertainty (entropy) on top, Class Probability Map and Feature Importance below. All four sit in identical cards with the title and an expand control in a header row above the figure, so the frames line up and nothing is drawn over a map. Below the grid, a **Results table** dropdown switches between the tables (Model performance metrics first, then Confusion matrix, Per-class accuracy, Covariate lift, Performance by area, and Class area coverage). Options that do not apply to the run are absent from the dropdown; the CSV export always contains the full metric set regardless.
- **Click any map**, or the expand icon in its card header, to open it full-size at higher display resolution. The Feature Importance plot has the same control, which is the easiest way to read long covariate names. Two default-off **map display options** sit under the maps: *Scale bar & north arrow* and *Show sample points* (the scoped training samples as white circles, useful for judging where the map is supported by data and where it extrapolates). Both apply to the on-screen maps, the expanded views and the styled PNG exports, and never alter the exported data rasters.
- **Model Performance** reports pooled cross-validated metrics with plain-language names: Overall accuracy, Cohen's kappa, Balanced accuracy, Precision / Recall / F1 (macro averages), ROC AUC (multiclass, Hand-Till), Log loss, and Brier score. The Estimator column states how each metric extends to the multiclass case. The blue badge states the CV design, the scope, whether tuning was nested, and whether class weights were applied.
- **Per-class Accuracy** lists producer accuracy (recall) and user accuracy (precision) per class. The confusion matrix and class area coverage in hectares are their own dropdown entries.
- **Feature Importance** shows which covariates drove the map: permutation importance, how much the model's log-loss worsens when one covariate is shuffled, with each bar's share of the total in percent. *Tip: read it together with the collinearity note, since correlated covariates split their importance. A bar near zero or negative is a covariate the model ignores.*
- **Covariate Lift** benchmarks the model against two no-covariate baselines on the same CV folds: always-predict-the-majority-class (the no-information rate, with an info icon for the interpretation), and a spatial-only nearest-neighbour classifier. It states the accuracy gain in points and whether the paired improvement is significant (McNemar test). *Tip: if the lift is not significant your covariates add little beyond spatial position, and the map may look plausible while the covariates do no real work.*
- **Performance by Area**, shown when the scope spans multiple localities or polygons in polygons-only mode, splits the same out-of-fold predictions per area: n, Overall accuracy, Cohen's kappa, Balanced accuracy and macro F1, plus a Total row matching the headline metrics. NA appears where a metric is undefined for an area, and small-n areas deserve caution.
- **Confidence threshold (abstain below):** a slider under the maps. Cells whose best class probability falls below the threshold turn grey ("Unclassified") instead of receiving a weak guess, because a 34/33/33% cell is ignorance, not a prediction. The map, area table and class GeoTIFF update instantly with no re-run, and a note reports how many validation points would survive the threshold and how accurate the survivors are. *Tips: values at or below 1/(number of classes) can never trigger; 0.5 is a sensible start for 3+ classes; treat the grey cells as your next field-sampling plan.*
- Maps render once per run and are cached. They redraw only when a new run completes, another probability class is selected, the confidence threshold changes, or a map display option is toggled.

**Exports**
- **Class GeoTIFF:** the predicted-class raster with the map's colour palette embedded, so it opens coloured in QGIS, ArcGIS and most image viewers. Honours the current confidence threshold.
- **Probabilities / Entropy GeoTIFF:** full-precision floating-point *data* rasters (values 0-1), never masked by the threshold. Generic image viewers show single-band float data in grayscale, so load them in GIS software and apply a colour ramp, or use the styled export below.
- **Styled Maps (PNG):** a zip of publication-style renders (300 DPI, 9 x 7 in) of the class, entropy and currently selected probability maps, with projected coordinate axes in metres. The scale bar, north arrow and sample-point options are honoured as toggled.
- **Metrics CSV:** the performance table with both the display name and the yardstick metric id per row, plus the baseline-comparison rows (spatial baseline, majority class, covariate lift, McNemar p) and the permutation importances. A `scope` column separates the sections.
- **Download Model (.rds):** saves the trained workflow with its metadata, so an hour of tuning is not lost when the app closes. Reuse it on new data in any R session with the same package versions: `b <- readRDS(file); predict(b$workflow, new_data, type = "prob")`. `new_data` needs the covariate columns listed in `b$predictors`, and all preprocessing replays automatically. *Tip: keep the bundle next to a note of your app version, since .rds files from different xgboost versions may not interchange.*
