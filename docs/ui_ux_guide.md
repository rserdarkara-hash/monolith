## Data Ingestion & Configuration

The application requires simply but cleanly structured, georeferenced tabular data.
  - Column headings (the first row) need to be reserved for parameters  (e.g., `tn`, `p`, `k`, `ph`, `som`, `clay`), categorical columns (e.g., `subset`, `texture_class`, `locality` ), and coordinates (e.g., `x`, `y`, `lat`, `long`); rows are required to be the related data, numbering, coordinate values, locality names or category labels etc.

**Step 1. Upload Dataset:**
   - Click `Browse...` to upload your file. 
   - The system accepts standard delimited formats (CSV, TXT) and Excel (XLSX). Ensure your file contains distinct columns for X coordinates, Y coordinates and at least one variable of interest.
   - Sample data sets are available in the app directory: 
      - the large data set is in `samp_data_1.xlsx` to be used with the variable list `samp_var_list.xlsx` (dummy soil phsysicochemistry data for Denizli/Türkiye - properties of tobacco growing soils with actual environmental, remote sensing and terrain data); 
      - the small data set is in `samp_data_2.xlsx` (dummy soil phsysicochemistry and hydrology data for Pardubice/Czech Republic - dummy data for a sunflower field under controlled drainage)

**Step 2. Assign Variables:**
   - **X/Y Coordinates:** Select the columns representing longitude/latitude or easting/northing. App searches for exact matches of `x`, `y`, or headers starting with `lon` or `longitude`, `lat` or `latitude`.
   * **Locality / Grouping:** Searches for headers containing `locality`, `loc`, `site`, `farm`, `id`, or `group`. It assigns this to the "Locality" selector, which is used to filter analysis subsets later.
   
**Step 3. Coordinate Reference System (CRS) Management:**
   - The app attempts to automatically parse the CRS from the data structure.
   - Look at the **Detected CRS** badge. If it says `Unknown` or is incorrect, manually select the appropriate EPSG code from the dropdown (e.g., `WGS 84 (EPSG:4326)` for raw GPS, or a localized UTM zone for precise meters). Spatial calculations (like distances in the variogram) will fail or be highly distorted if the CRS is incorrect.

**Step 4. Metadata & Automated Pairing - optional**

- Auto-map your data-set with readable labels, and analyze through different categories of results by providing a secondary configuration file (e.g., `samp_var_list.xlsx`).

  **A. Upload Metadata Context**

  - Upload File: Select a secondary .xlsx, JSON, or TXT file containing your variable definitions.

  - Label Mapping: The app scans for headers like label, name, or display to replace raw column codes (e.g., Soil_pH instead of ph_01) in all maps and reports.

  - Category Grouping: Headers containing cat or group allow the UI to organize variables into folders such as "Physicochemistry," "Remote Sensing," or "Terrain".

  **B. Automated Variable Processing**

  - Target Isolation: The system automatically filters out non-numeric columns and coordinates identified in Step 1, treating the remainder as target variables.

  - Smart Pairing: The engine performs a suffix search to link observed data with model outputs:

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

**1.2 Data Subsetting**
* **Interrogation Modes:** Depends on `subset` column availability. You can toggle between the full dataset ("All") or specific modeling splits such as "Train", "Test", or "Validation" if you are mapping actual and predicted pedo-parameters.

**1.3 Variable Selection & Category**
* **Variable Category:** To keep the interface clean, variables are organized into folders (e.g., "Soil Physicochemistry", "Environmental Data", "Terrain", "Satellite Indices") based on your metadata file or automated detection.
* **Variable:** This selects the target "Actual" variable column: As the app uses automated suffix and label matching, selecting a target like `Total N (%)` automatically links it to the column `tn`, and well as to its different prediction columns, `tn_cve` and `tn_ss` in the background if those columns are available.

**1.4 Primary View & Comparison Mode**
This determines the mathematical "lens" through which you see the field:
* **Actual Values:** Displays the interpolated surface of your raw measured data.
* **Best Predictions (_cve):** Displays the interpolated surface of the machine learning model's cross-validated estimates.
* **Single Split Predictions (_ss):** Displays the surface for a specific data-split prediction.
* **Residuals (v - pv):** It calculates the localized difference between what you measured and what the model you used to predicted that parameter predicted.
* **Comparison Mode:** When enabled for predictions, the dashboard splits into a synchronized dual-map view. This allows for a side-by-side "visual audit" of the Actual data vs. the Model's predictions to identify spatial bias.

## Mapping: 2. Spatial Engine Selection & Tuning
Once selections are made, the main interface transitions to the analytical module. If you wish to generate uncertainity maps instead of running spatial interpolation of the parameters, tick the relevant mark at the bottom of the spatial engine.

**1. Select the Interpolation Method:**
   - Locate the **Spatial Engine** dropdown.
   - Choose your model based on the dataset.
     - **Deterministic (Fast):** **IDW** (Inverse Distance Weighting) and **TPS** (Thin Plate Spline) are ideal for rapid visualization. IDW uses distance-based weights, while TPS fits a smooth surface by minimizing "bending energy".
     - **Geostatistical (Standard):** **Ordinary Kriging (OK)** uses a Variogram to model spatial autocorrelation, providing the Best Linear Unbiased Predictor (BLUE).
     - **Multivariate (Co-Kriging)**: **Co-Kriging (CK)** exploits the cross-correlation between your primary target and a densely sampled auxiliary variable (e.g., using Sensor-based Conductivity to improve a Clay map).
     - **Hybrid/ML (High-Precision):** **Regression Kriging (RK)** and **Random Forest Kriging (RFK)** combine environmental trends (topography, satellite data) with Kriging of the residuals to capture complex soil patterns.
     
**2.a. Variogram Optimization (Geostatistical Engines Only):**
   - If a Kriging method is selected, the Variogram Panel will appear.
   - **Auto-Fit Button:** Click this first. The system will attempt to fit four different models using least-squares optimization and will choose the best fit. Review the plotted curve against the scatter points.
   - **Manual Override:** If the auto-fit fails to capture the short-range variability (the points closest to the Y-axis), toggle to `Manual Tuning`. Use the sliders:
     - Adjust **Nugget** up if the data is extremely noisy.
     - Adjust **Partial Sill** to match the upper plateau of the points.
     - Adjust **Range** to define where the curve flattens out.

**2.b. IDW Optimization** 
     - If IDW method is selected, the related panel will appear.
     - Adjust the `number of neighbors` before optimizing for the optimum `IDW factor`. 
     - Manual override will be available if you deem it necessary.
     
**2.c. TPS Optimization** 
     - If TPS method is selected, the related panel will appear.
     - Simply click to button to achieve locality specific `lambda` values.
     - Manual override will be available if you deem it necessary.
     
**3. Define the Grid Resolution:**
  The resolution determines the size of each pixel in your final map.
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
* **Strict Measured:** Creates individual buffers around every point.
* **Buffer Distance (m):** Defines how far the map extends beyond the outermost sample points.

**5. Execution:**
   - Click **GENERATE MODELS**. The system will perform LOOCV, generate the surface, and populate the Validation Diagnostics table with RMSE, R², and Moran's I metrics.

---

## Mapping: 3. Map Styling 

**Color Palette Configuration:**
- Locate the **Styling** drop down menu.
- Styling can be changed both before and after generating the interpolation models.
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
   - In the bottom left corner of the export styler, use the toggle **High-Contrast Mode** if the map will be projected on a low-lumen projector or if required for colorblind compliance.
   
**2. Typographical Scaling and Advanced Options:**
   - In the Export Panel, locate the Text Scaling slider. Increase the scale (`1.2x`, `1.5x`) to enlarge the legend, axis labels, and titles.
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