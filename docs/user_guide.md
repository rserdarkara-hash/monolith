## Data Ingestion & Configuration

The application requires cleanly structured, georeferenced tabular data.
  - Column headings (the first row) need to be reserved for parameter titles  (e.g., `tn`, `p`, `k`, `ph`, `som`, `clay`), categorical/grouping factors if available (e.g., `subset`, `texture_class`, `locality` ), and coordinate labels (e.g., `x`, `y`, `lat`, `long`); rows are required to be the related data, numbering, coordinate values, locality names or category labels etc.

**Step 1. Upload Dataset:**
   - Click `Browse...` to upload your file. 
   - The system accepts `.csv` and `.xlsx` files. Your file must contain distinct columns for X coordinates, Y coordinates and at least one variable of interest.
   - A sample dataset is available in the app directory: 
      - `samp_data_1.xlsx` (paired with the variable list `samp_var_list.xlsx`) is a demonstration subset containing three localities (Kale, Yorga, Altinova) drawn from the soil dataset of Kara et al. (2026). It is provided only to exercise the app; the complete dataset is published openly under CC BY 4.0 on Mendeley Data (https://doi.org/10.17632/8548bmgxh9.1; inactive until publication), while the demo files' own usage terms are given in `sample_data/DATA_LICENSE`.
        - *Kara, R. S., Ongun, A. R., Almaz, C., Çiçek, G., Tepecik, M., & Yilgan, F. (2026). Diagnostic modeling of nutrients to support agroecosystem transitioning in tobacco soils: A stratified evaluation framework based on clay-organic matter-lime interactions [Manuscript submitted for publication]. Department of Soil Science and Soil Protection, Czech University of Life Sciences Prague*).

**Step 2. Assign Variables:**
   - **X/Y Coordinates:** Select the columns representing longitude/latitude or easting/northing. App searches for exact matches of `x`, `y`, or headers starting with `lon` or `longitude`, `lat` or `latitude`.
   * **Locality / Grouping:** Searches for headers containing `locality`, `loc`, `site`, `farm`, `id`, or `group`. It assigns this to the "Locality" or a "Grouping Factor" selector, which is used to filter analysis subsets later. See the sample data files for related columns.
   
**Step 3. Coordinate Reference System (CRS) Management:**
   - The app attempts to automatically parse the CRS from the data structure.
   - Look at the **Detected CRS** badge. If it says `Unknown` or is incorrect, manually select the appropriate EPSG code from the dropdown (e.g., `WGS 84 (EPSG:4326)` for raw GPS, or a localized UTM zone for precise meters). Spatial calculations (like distances in the variogram) will fail or be highly distorted if the CRS is incorrect.

**Step 4. Metadata & Automated Pairing - optional**

- Auto-map your data-set with readable labels, and analyze through different categories of results by providing a secondary configuration file (e.g., `samp_var_list.xlsx`).

  **A. Upload Metadata Context**

  - Upload File: Select a secondary .xlsx, JSON, or TXT file containing your variable definitions.
  - Label Mapping: The app scans for headers like label or name to display on all maps and reports.
  - Category Grouping: Headers containing cat or group allow the UI to organize variables into folders such as "Physicochemistry," "Remote Sensing," or "Terrain".

  **B. Automated Variable Processing**

  - Target Isolation: The system automatically filters out non-numeric columns and coordinates identified in Step 1, treating the remainder as target variables.
  - Smart Pairing & Folder Grouping: The engine performs a suffix search to link observed data with model outputs. Additionally, the UI groups variables dynamically into structural folders (e.g., 'Soil Physicochemistry', 'Terrain') based on the uploaded metadata, allowing users to seamlessly navigate high-dimensional datasets.
    * Cross-Validation: Matches target names with the `_cve` suffix; 
    * Single Split: Matches target names with the `_ss` suffix.
    * Successful matches create a "Triad" (Actual, Pred_cve, Pred_ss), allowing you to toggle between ground-truth maps and residual maps without manual re-linking.

  **C. Final Validation**

  - Review Mapping: Check the generated table at the bottom of the configuration panel to verify that labels, units, and prediction pairs are correctly assigned.
  - Confirm: Once the variable mapping is verified, confirm it at the end of the page, and proceed to the Spatial Engine to begin interpolation.

---

## Mapping: 1. Context

**1.1 Locality (Spatial Grouping)**
* **Locality Selection:** The app uses your designated "Grouping" column to partition the dataset (Step 2). You can select a single field (e.g., "Zone A") or "ALL" to run a batch-parallelized interpolation across multiple separate spatial domains simultaneously.

**1.2 Variable Selection & Category**
* **Variable Category:** To keep the interface clean, variables are organized into folders (e.g., "Soil Physicochemistry", "Environmental Data", "Terrain", "Satellite Indices") based on your metadata file or automated detection.
* **Variable:** This selects the target "Actual" variable column: As the app uses automated suffix and label matching, selecting a target like `Total N (%)` automatically links it to the column `tn`, and well as to its different prediction columns, `tn_cve` and `tn_ss` in the background if those columns are available.

**1.3 Primary View & Comparison Mode**
This determines the mathematical "lens" through which you see the field:
* **Actual Values:** Displays the interpolated surface of your raw measured data.
* **Best ML Predictions (_cve):** Displays the interpolated surface of the machine learning model's cross-validated estimates.
* **Single Split ML Predictions (_ss):** Displays the surface for a specific data-split prediction.
* **Residuals (v - pv) of ML Predictions:** Calculates the localized difference between what you measured (v) and what your uploaded machine learning model predicted (pv). These residuals diagnose the ML model, not the interpolation performed by the dashboard.
* The ML prediction and residual views are only offered when the selected variable actually has the corresponding prediction column in your uploaded data (`_cve` for Best ML Predictions and Residuals, `_ss` for Single Split ML Predictions). If a variable has no prediction columns, the Primary View is limited to **Actual Values**.
* **Comparison Mode:** When enabled for predictions, the dashboard splits into a synchronized dual-map view. This allows for a side-by-side "visual audit" of the Actual data vs. the Model's predictions to identify spatial bias.

**1.4 Data Subset (Single Split view only)**
* When the Primary View is set to **Single Split ML Predictions (_ss)**, a **Data Subset** dropdown appears directly beneath it. It restricts the mapped data to one modeling partition (e.g., "Train", "Test", or "Validation") before interpolation.
* The available choices are read from a `subset` column in your uploaded dataset (matched case-insensitively, so `Subset` or `SUBSET` also work). If the dataset has no such column, only "All" is offered and no filtering occurs.
* The filter applies exclusively to the Single Split view; all other views always use the full dataset.

**1.5 Drawing on the Map: Custom Groups & Exports**

The interactive maps (Map Viewer and both comparison maps) carry a **drawing toolbar** on the left edge of the map. It lets you sketch polygons, rectangles, and point markers directly on the map, and this is the basis for two workflows:

* **Assigning a custom locality / analysis group:**
    1. Draw a shape around the points you want to group (polygon or rectangle tools).
    2. When you finish the shape, an **"Assign Locality / Analysis Group"** dialog opens automatically. Enter a group name (e.g., "Zone A") and click **Save Group**.
    3. All sample points falling inside the shape are written to an `Assigned_Locality` column in your dataset (the app reports how many points were captured). You can then select this column as the Locality/Grouping column in the Data Setup tab to run analyses on your custom zones.
* **Exporting your work:** two buttons sit at the right end of the Map Viewer toolbar (hover over them in the app for a reminder of when they apply):
    * **Export Manually Drawn Polygon:** Available once you have drawn at least one polygon on the map. Downloads *all* drawn polygons in the format chosen in the adjacent dropdown (Shapefile ZIP, GeoJSON, KML, or GPKG), so you can reuse the boundaries in a GIS.
    * **Export Updated Dataset:** Use after you have modified the dataset inside the app, most commonly after saving one or more drawn groups as described above. Downloads the current dataset as `.xlsx`, including the `Assigned_Locality` column, so the group assignments survive beyond the session.
* Shapes can be edited or deleted with the toolbar's edit/delete tools; the polygon export always reflects the current set of shapes. Note that deleting a shape does **not** undo a group assignment already saved from it.

## Mapping: 2. Spatial Engine Selection & Tuning
Once selections are made, the main interface transitions to the analytical module. If you wish to generate uncertainity maps instead of running spatial interpolation of the parameters, tick the relevant mark at the bottom of the spatial engine.

**1. Select the Interpolation Method:**
   - Locate the **Spatial Engine** dropdown.
   - Choose your model based on the dataset.
     - **Deterministic (Fast):** **IDW** (Inverse Distance Weighting) and **TPS** (Thin Plate Spline) are ideal for rapid visualization. IDW uses distance-based weights, while TPS fits a smooth surface by minimizing "bending energy".
     - **Geostatistical (Standard):** **Ordinary Kriging (OK)** uses a Variogram to model spatial autocorrelation, providing the Best Linear Unbiased Predictor (BLUE).
     - **Multivariate (Co-Kriging)**: **Co-Kriging (CK)** exploits the cross-correlation between your primary target and a densely sampled auxiliary variable (e.g., using Sensor-based Conductivity to improve a Clay map).
     - **Hybrid/ML:** **Regression Kriging (RK)** and **Random Forest Kriging (RFK)** combine environmental trends (topography, satellite data) with Kriging of the residuals to capture complex soil patterns.

**1.b. Choose the Cross-Validation Strategy:**
   - Directly beneath the Spatial Engine dropdown, a **Cross-Validation Strategy** selector controls how the model's performance metrics are validated. It changes the reported diagnostics only, **never the interpolated map itself**.
     - **Auto (Default):** Leave-One-Out CV for 50 or fewer points, switching to a seeded random 10-fold CV above 50.
     - **Standard LOOCV:** Full Leave-One-Out for every dataset, the most rigorous choice, but slow beyond a few thousand points (especially RK/RFK).
     - **Spatial Block CV:** Holds out ten spatially contiguous k-means clusters, curbing the optimistic bias that random folds show when data are spatially autocorrelated. Recommended for spatial validation; below 30 points it reverts to LOOCV.
   - The strategy actually applied to a completed run is shown next to the **Model Performance** table (and in each locality's *CV Type* row), so you always know which validation produced the reported metrics.

**2.a. Variogram Optimization (Geostatistical Engines Only):**
   - If a Kriging method is selected, the Variogram Panel will appear.
   - **Auto-Fit Button:** Click this first. The system will attempt to fit four different models using least-squares optimization and will choose the best fit. Review the plotted curve against the scatter points.
   - **Manual Override:** If the auto-fit fails to capture the short-range variability (the points closest to the Y-axis), toggle to `Manual Tuning`. Use the sliders:
     - Adjust **Nugget** up if the data is extremely noisy.
     - Adjust **Partial Sill** to match the upper plateau of the points.
     - Adjust **Range** to define where the curve flattens out.

**2.b. IDW Optimization** 
     - If IDW method is selected, the related panel will appear.
     - The interface provides a dynamic slider panel that calculates optimal parameters on a per-locality basis. When optimized, a reactive table details the exact Power factor chosen for *each independent spatial domain*.
     - Adjust the `number of neighbors` before optimizing for the optimum `IDW factor`. A specific tooltip advises users to restrict `idw_nmax` (default 12) before optimization to prevent distant, unrelated points from distorting local predictions. 
     - The optimization process uses a deterministic cross-validation technique. This ensures that repeated optimizations on the same data yield identical results, guaranteeing complete reproducibility.
     - Manual override will be available if you deem it necessary.
     
**2.c. TPS Optimization** 
     - If TPS method is selected, the related panel will appear.
     - By default, the lambda slider is set to `< 0` (Auto GCV Optimization). This means the system automatically utilizes Generalized Cross-Validation to determine the optimal smoothing parameter natively during interpolation.
     - You can optionally click the "Optimize TPS Lambda" button to explicitly calculate and extract the locality-specific `lambda` values, generating a reactive table to view them.
     - Manual override is available if you deem it necessary. Note that setting `Lambda = 0` forces exact interpolation (no smoothing).
     
**3. Define the Grid Resolution:**
  The resolution determines the size of each pixel in your final map. Resolutions and their automatic suggestions are always in **metres**, even if you selected a geographic (degree-based) mapping CRS, the app measures point spacing in metres behind the scenes, because interpolation always runs in a projected CRS.
* **Auto (Per Locality):** The app calculates the average distance between nearest-neighbor samples for *each specific field* and sets the pixel size to 50% of that distance. This tries to enable high-density plots to get high-detail maps, while sparse regions stay computationally efficient.
* **Auto (Global):** Uses the average point density of the *entire dataset* to set a uniform resolution across all maps.
* **Fixed:** Provides manual control. 
    * **Low Values (e.g., 1-5m):** High detail, but high RAM usage and longer processing times.
    * **High Values (e.g., 50-300m):** Faster processing, but may lose local soil variations.
    
**4. Borders, Polygoning Logic:**
   To prevent the model from interpolating indefinitely, you must define the "Analysis Envelope":
* **Concave Hull:** Shrink-wraps the boundary to follow the outer perimeter of your points. Ideal for irregularly shaped fields.
* **Convex Hull:** Creates a "rubber band" boundary around the points. Best for simple, rectangular fields.
* **Wrapped (Buffered):** Creates a smoothed buffer around the concave hull to ensure the map covers the field edges.
* **Strict Measured (Point Buffer):** Creates individual buffers around every point.
* **Buffer Distance (m):** Defines how far the map extends beyond the outermost sample points.

**5. Execution & Run Estimation:**
   - **Run Estimate:** Before execution, the UI displays a dynamic "Run Estimate" indicator (e.g., "~1 locality model(s), ~1.5 minutes estimated").
     - **History-Aware Logic:** If sufficient prior runs exist in the `.csv` log for the chosen method, the engine uses a linear model to predict runtime based on sample counts.
     - **Cold-Start Method Multipliers:** If no history exists, it uses base multipliers: RFK (1.0x), RK (1.0x), and CK (1.3x) which are calibrated from real measurements. OK (0.5x), IDW (0.5x), and TPS (0.3x) are currently unverified estimates based on theoretical complexity.
   - Click **Run Interpolation** and proceed to **Map Viewer** tab. The system will perform cross-validation (Leave-One-Out CV for 50 or fewer observations, or 10-fold CV for larger datasets) *(Note: cross-validation utilizes a fixed random seed by default; see the Scientific Guide Section 9 for customization)*, generate the surface, and populate the Validation Diagnostics table with RMSE, R², and Moran's I metrics *(Note: Moran's I uses a hardcoded distance threshold multiplier; see the Scientific Guide Section 9 for customization)*.
   - **Automatic Fallbacks:** If an engine cannot be fitted for a locality (e.g., TPS on a degenerate point layout, or CK/RK/RFK model failures), the app automatically falls back to a simpler engine and reports this in the run log and as a warning notification; check the log if a map looks different from the method you selected.
   - **Responsive Diagnostic Views:** The UI employs `shinyjs` to dynamically toggle the visibility of complex validation diagnostics (like the RF Variable Importance Plot, Internal Residual Variogram, or TPS GCV Diagnostic Plot) based on the active Spatial Engine and whether the user is viewing `Actual` or `Predicted` data. This prevents empty plots from rendering and provides visual cleanliness.

---

## Mapping: 3. Map Styling 

**Color Palette Configuration:**
- Locate the **Styling** drop down menu.
- Styling can be changed both before and after generating the interpolation models. ***If you modify the styling after a model has already been generated**, please allow approximately one minute for the application to re-render the map and recalculate area coverage and classification statistics. During this processing time, the map area and statistics table will fade to a pale gray to indicate that the update is in progress.
- Choose a classification method:
    * **Continuous:** Best for visualizing raw gradients.
    * **Binned (Statistical):** Select Jenks Natural Breaks or K-Means clustering and choose the number of classes (e.g., 5). This forces the continuous data into easily readable color bands based on data distribution.
    * **Agronomical (Supervised):** Automatically inputs threshold values to create distinct management zones for macro and micro-nutrients.
        * Activating the agronomical classification enables the interpretation of **a detailed classification performance for the uploaded parameter predictions** in scientific analysis section.
---

## Finalizing & The Export Registry

The Export Registry standardizes outputs for reports and presentations. 

- Open export styler for individual downloads after choosing a single result, or select all necessary results for the batch download. 
- All tabular results selected will be generated within a single .xlsx file with corresponding sheets for each result category listed in the registry.

**1. Accessibility and Themes:**
   - In the bottom left corner of the export style (within advanced settings) use the toggle **High-Contrast Mode** if required for colorblind compliance.
   
**2. Typographical Scaling and Advanced Options:**

   - Advanced options in the styler provides comprehensive control by enabling individual size and orientation settings for the figure elements. 
   - The styling configurations can be saved for future sessions. 

**3. DPI Configuration:**
   - Choose your target resolution under **Output Quality**:
     - `72 DPI`: For quick sharing over email or Slack.
     - `300 DPI`: Standard for internal PDF reports and printing.
     - `600 DPI`: Required for submission to scientific journals.

**4. Download:**
   - Select your desired format (`.TIFF`, `.PNG`, `.JPEG`, `.PDF`).
   - Click **Finalize and Download**.
