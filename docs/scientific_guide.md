# Scientific & Analytical Methodology Guide

Monolith gives agronomists, soil scientists and geostatisticians a toolkit for mapping and explaining spatial variability. This guide states the mathematics behind each method, the modelling choices Monolith makes, and what the resulting numbers do and do not support.

> *Scope of this document: the methods implemented here (kriging variants, IDW, TPS, cross-validation metrics, PCA, statistical tests, supervised classification) are established published methods and are not original contributions of this software or its author. This guide describes how they are implemented in Monolith, not their theoretical origins. Section 11 lists the literature; inline package links specify software conventions. This guide describes the source checked on 2026-09-20, including its implementation limitations.*

---

## 1. Spatial Interpolation Engines

Spatial interpolation builds continuous prediction surfaces from discrete point samples. Monolith implements both deterministic weighting and smoothing methods and geostatistical models (spatial autocorrelation plus a statistical uncertainty model).

**NaN protection:** across all kriging methods, predictions (`var1.pred`) and theoretical variances (`var1.var`) are screened for NaN and infinite values and converted to `NA` before mapping.

### 1.1 Ordinary Kriging (OK)

**Mathematical intuition:** the value at an unsampled location <i>Z<sup>*</sup>(x<sub>0</sub>)</i> is a linear combination of the known surrounding values <i>Z(x<sub>i</sub>)</i>:
<br><br>
<div style="text-align:center;"><i>Z<sup>*</sup>(x<sub>0</sub>) = &sum; &lambda;<sub>i</sub> Z(x<sub>i</sub>)</i></div>
<br>
Unlike simple kriging, OK assumes an unknown but constant global mean (<i>&mu;</i>). The weights <i>&lambda;<sub>i</sub></i> minimize the estimation variance subject to <i>&sum; &lambda;<sub>i</sub> = 1</i>, and the covariance matrix that solves for them comes directly from the fitted theoretical variogram (Matheron 1963; Goovaerts 1997).

**Agronomical example:** predicting soil pH across a relatively uniform field where variation is driven by soil-forming processes rather than abrupt topography or management.

**Actual and Predicted surfaces.** A run that also kriges an uploaded prediction column fits that surface its own variogram by default. With **Fit Actual/Predicted Separately** unticked, the Predicted surface is kriged with the variogram of the *measured* values: the run's own Actual fit under Auto-Fit, or the applied Actual model under Manual (Section 4.1). Under Auto-Fit its cross-validation refits that measured-value variogram inside each fold, from the training samples that carry a measured value, and then kriges the predictions with it. This control also unifies parameter sharing under IDW and TPS (Sections 1.5, 1.6, 4.2, 4.3); RK, RFK and CK always fit each surface independently.

**Algorithmic stability:** a zero empirical nugget seed is replaced with `max(initial_sill * 1e-6, 1e-6)`. Auto-fit screens unstable candidates (Section 4.1); applied manual models are not modified. An all-undefined prediction surface is skipped with a warning. For OK, a Gaussian/Matérn nugget below 5% of total sill is named as a possible cause, not a universal singularity threshold.

### 1.2 Regression Kriging (RK)

**Mathematical intuition:** RK splits the variable into a deterministic trend and a stochastic residual.
<br><br>
<div style="text-align:center;"><i>Z(x) = m(x) + e(x)</i></div>
<br>
An ordinary least-squares linear model (<code>lm</code>) fits the trend <i>m(x)</i> from secondary covariates (elevation, NDVI). The residuals <i>e(x)</i> carry the spatially correlated variation the trend does not explain, and Ordinary Kriging is applied to them. The prediction is the sum of trend and kriged residual.

**Agronomical example:** mapping soil organic carbon, where elevation and satellite-derived moisture predict the baseline trend and kriged residuals recover localized accumulations the remote sensing misses.

**Reported trend diagnostics:** for each per-locality trend model the Scientific Analysis tab reports R², adjusted R², the residual standard error with its degrees of freedom, the overall F test with its p-value, and a coefficient table with standard errors, t statistics, p-values and 95% confidence intervals from the t distribution on the residual degrees of freedom. These describe the *trend component only*; the quality of the full RK prediction is assessed by the cross-validation metrics in the Model Performance table.

**Reading the trend p-values.** OLS standard errors, t tests, confidence intervals and the F test assume independent, homoscedastic errors; spatial dependence can invalidate them. Unbiased coefficients additionally require a correctly specified conditional mean and exogenous covariates. These are exploratory trend diagnostics, not spatially adjusted inference. The reported Moran test uses held-out errors of the full RK procedure, not OLS training residuals, so it does not directly validate the coefficient tests. Monolith fits two-step OLS-then-krige (Hengl et al. 2007), not joint GLS/universal kriging.

### 1.3 Random Forest Kriging (RFK)

**Mathematical intuition:** RFK follows the same two-step logic as RK but replaces the linear trend with a random forest ensemble, which captures non-linear interactions among covariates without a parametric functional form. Ordinary Kriging is then applied to the forest's residuals (Hengl et al. 2015).

**Agronomical example:** predicting crop yield across a heterogeneous landscape where the relationship between yield, slope, aspect and electrical conductivity is non-linear and interactive.

### 1.4 Co-Kriging (CK)

**Mathematical intuition:** CK extends OK by using one or more secondary variables to improve prediction of a primary variable, through the cross-variogram that models how the two co-vary in space (Wackernagel 2003):
<br><br>
<div style="text-align:center;"><i>&gamma;<sub>12</sub>(h) = (1 / 2N(h)) &sum; [Z<sub>1</sub>(x) - Z<sub>1</sub>(x+h)][Z<sub>2</sub>(x) - Z<sub>2</sub>(x+h)]</i></div>
<br>

**Agronomical example:** predicting nitrate jointly with co-sampled electrical conductivity. CK uses rows complete for the target and every selected covariate; it does not use a separate denser auxiliary network or covariate-only rows.

**Covariate kriging fallback (RK, RFK):** each covariate is kriged onto the prediction grid with its own auto-fitted variogram. When that fails for a covariate, the pipeline interpolates it with IDW instead, using the locality's IDW power and the **Max Neighbors** value from the sidebar (defaults p = 2, nmax = 12), and writes a warning to the run log. CK does not use covariate grids (Section 8.1).

**LMC stabilisation (CK):** `fit.lmc()` fits direct/cross variograms, projects sill matrices to positive semidefinite matrices by zeroing negative eigenvalues, and multiplies direct sill coefficients by `correct.diagonal = 1.01`. This reduces singularity risk but does not guarantee a solvable prediction system. The correction is logged. See the [gstat documentation](https://r-spatial.github.io/gstat/reference/fit.lmc.html).

**Covariate standardization:** every auxiliary variable is standardized to zero mean and unit variance before the LMC is fitted. The target is not rescaled, so predictions, metrics and plotted variograms stay in the variable's own units. Each cross-validation fold takes its means and sample standard deviations from its own training samples, so a held-out observation never contributes to the centring of the predictors used to predict it.

**Cross-validation holdout convention (full-row removal):** a CK fold removes the entire held-out row, the primary observation *and* its co-located covariate observations, and rebuilds the model from the training rows alone: the covariate screen, the standardization, the lag geometry, the primary variogram, the LMC family, its shared range and smoothness seed, and the coregionalization sills. The held-out rows contribute their coordinates and nothing else. This is what the map faces at a grid node, where the covariates are co-sampled laboratory measurements that do not exist there and are predicted jointly through the LMC. RK and RFK hold out the full row for the same reason: each fold kriges the held-out covariates from its training rows before evaluating the trend. Their maps also use covariate observations from rows with a missing target, which cannot enter a target-labelled fold. A fold whose LMC cannot be fitted contributes no prediction; it is reported through coverage (Section 5) and never filled from another engine.

**The LMC fits sills only; one range is shared and held fixed.** Monolith uses one non-nugget structure with a common family, range and smoothness across direct/cross variograms (Goovaerts 1997; Wackernagel 2003). A general LMC can contain several shared structures with different ranges. Here, `fit.lmc()` runs with `fit.ranges = FALSE` (Pebesma 2004). Two consequences follow, and they point in opposite directions. The starting **sill** is inert: with model type and range held fixed the variogram is *linear in its sill parameters*, so the weighted-least-squares solution does not depend on it (verified empirically, starting sills spanning 1e-6 to 1e12 on one empirical variogram return a bit-identical fitted sill). The starting **range**, by contrast, is not fitted and therefore *is* the final range of every variogram in the LMC. It is set to the range that weighted least squares fitted to the primary variable's own omnidirectional variogram, so the coregionalization length reflects the data's measured spatial structure. When that fit is itself the heuristic fallback, or returns no usable range, the seed falls back to the extent heuristic (half the variogram cutoff, i.e. a quarter of the bounding-box diagonal) and the run log says so. Read the CK range as the primary variable's range imposed on the whole system, not as a separately fitted quantity per covariate.

**Local search neighbourhood (nmax):** CK solves the system using only the nearest `nmax` observations of every variable, exposed as the **CK Max Neighbors** slider (5 to 60, default 15) in the Spatial Engine panel. This is a modelling choice, not a speed setting: `nmax` sets how local the stationarity assumption is. A small neighbourhood assumes the mean and covariance structure are constant only within a short radius (appropriate for non-stationary fields, and it avoids the matrix growth of global co-kriging); a large neighbourhood approaches a global system and suits genuinely stationary, sparsely sampled fields. Because the right value depends on point density and the fitted cross-variogram range, no single neighbourhood is optimal across datasets; 15 is the application default. OK, RK and RFK krige with a global neighbourhood; IDW has its own **Max Neighbors** control. The applied value is recorded in the run-configuration summary.

### 1.5 Inverse Distance Weighting (IDW)

**Mathematical intuition:** a purely deterministic weighted average in which the weight falls off with distance <i>d</i> raised to a power <i>p</i> (commonly <i>p</i> = 2; Shepard 1968):
<br><br>
<div style="text-align:center;"><i>&lambda;<sub>i</sub> = (1 / d<sub>i</sub><sup>p</sup>) / &sum; (1 / d<sub>i</sub><sup>p</sup>)</i></div>
<br>
IDW assumes nearer points are more similar. It does not account for data clustering (redundant sampling) or directional anisotropy, and it produces no prediction variance.

**Agronomical example:** quick, inexpensive mapping of localized rainfall from a scattered gauge network, where strict stationarity assumptions are not needed.

**Actual and Predicted surfaces (IDW).** When mapping an uploaded prediction column with **Fit Actual/Predicted Separately** unticked, the Predicted surface reuses the Actual surface's distance-decay power (Section 4.2). Automated power searches and manual power tuning then operate on the Actual target only. With the box ticked (the default), both surfaces optimize their power independently.

### 1.6 Thin Plate Spline (TPS)

**Mathematical intuition:** TPS balances squared fitting error against bending energy, the integral of squared second derivatives (Duchon 1977). Lambda = 0 interpolates; positive lambda smooths. Smoothness does not prevent overshoot or unreliable extrapolation.

**Agronomical example:** smooth elevation contours or temperature gradients where abrupt discontinuities are physically implausible.

**Engine fallback (TPS to IDW):** if the TPS fit fails for a locality (a degenerate point configuration, for instance), the pipeline substitutes IDW using the active IDW parameters. The substitution is recorded in the run log (`TPS failed: ... Falling back to IDW.`) and raised as a per-locality warning, so fallback output is never mistaken for a genuine TPS result.

**Actual and Predicted surfaces (TPS).** When mapping an uploaded prediction column with **Fit Actual/Predicted Separately** unticked, the Predicted surface takes the Actual surface's smoothing lambda (Section 4.3). Under Auto (GCV), cross-validation reselects lambda in each fold from the measured values of that fold's training rows. With the box ticked (the default), each surface determines its smoothing independently.

---

## 2. Grid Resolution Logic

Grid resolution is the spacing of predictions and the raster cell size. Engines predict at cell centres, not block averages over cell areas. A finer raster creates no additional observational support or information.

### 2.1 The spatial support metric (FNN nearest neighbour)

When resolution is determined dynamically, a fast nearest-neighbour search measures point density. The recommended resolution is half the mean nearest-neighbour distance:
<br><br>
<div style="text-align:center;"><i>R<sub>rec</sub> = 0.5 &times; Mean Nearest-Neighbor Distance (h<sub>NN</sub>)</i></div>
<br>
This is a density-based spacing heuristic, not a guarantee of scientifically resolvable detail. Per locality the value is clamped to [1, 5000] m. The global recommendation is additionally floored at 1/300 of the larger bounding-box dimension, max(0.5 h<sub>NN</sub>, extent / 300), and clamped to [1, 500] m.

Recommendations are measured on the points transformed to the Target Mapping CRS and are always expressed in **metres**. For a geographic (degree-based) CRS, distances and extents are approximated through a Web Mercator (EPSG:3857) projection multiplied by cos(mean latitude) (`calc_metric_spacing()`, `spatial_pipeline.R`).

### 2.2 Auto (per locality)

Each locality's run grid is sized from its own boundary area by the rule in Section 2.5, so a compact, densely sampled locality gets a fine grid and a large one a coarser grid.

### 2.3 Auto (global)

Every locality of the run is gridded at **one cell size**: the Auto resolution of the largest boundary in the run (Section 2.5), so no locality exceeds about 100,000 cells inside its boundary and all outputs share the cell size. Before any locality is interpolated, the boundaries are built exactly as the run builds them to find that size. The grids are also snapped to the lattice of multiples of the cell size in the working CRS, so localities sharing a working CRS share one cell lattice. Two limits apply: localities in different UTM zones (a geographic upload spanning zones) cannot share a lattice, and the final resampling into a Target Mapping CRS that differs from the working CRS moves every surface off it.

### 2.4 Fixed (manual)

The slider value overrides the dynamic recommendation, which is useful when a specific pixel size is required for cartographic consistency or printing.

### 2.5 Resolution actually used by a run

The nearest-neighbour rule drives the **sidebar suggestion** and the **dynamic buffer** width (Section 3.2). The prediction grid an interpolation *run* builds is derived from boundary area:

<div style="text-align:center;"><i>R</i><sub>auto</sub> = min( 1000, max( 5, &radic;( <i>A</i><sub>boundary</sub> / 100,000 ) ) ) &nbsp;[m]</div>

that is, a target of roughly 100,000 grid cells inside the boundary, floored at 5 m and capped at 1000 m. Auto (Per Locality) applies it to each locality's own boundary; Auto (Global) applies it to the largest boundary and uses the result everywhere. The grid, the boundary and its buffer are built in the run's working CRS (Section 3.6): the Input Data CRS when it is projected in metres, or the WGS 84 UTM zone of the locality's mean position when it is geographic or uses another linear unit. Grid and buffer lengths are in metres. The bound keeps the cost of predicting at every node finite regardless of extent.

**Cells are square and exactly the stated size.** The grid starts at the lower-left corner of the boundary's bounding box (Auto (Global): at the snapped lattice point below it) and extends by whole cells until it covers the box. Fitting a fixed number of cells into the box instead would stretch them, for example to 45.45 × 44.44 m for a requested 45 m. The Map Viewer's resolution overlay lists the cell size a run used for each locality; the run configuration records the same figures as one value, or as a range across the localities where they differ.

One guard applies in all three modes: a cell size that would generate more than about 4 million candidate cells over a boundary's bounding box is coarsened to that limit and the change is written to the run log, because every bounding-box cell centre is first materialized as a coordinate matrix before clipping. The Auto rule targets 100,000 cells inside the **boundary**, which is not a bound on the bounding box: a strict point-buffer boundary occupies a small fraction of its own box, so an Auto grid can exceed the cell budget as readily as a fine Fixed one.

**The Auto rule is self-contained: it does not read the manual-resolution slider.** The slider is hidden outside Fixed mode and, in Auto modes, carries the *global* nearest-neighbour recommendation. Letting it impose a floor would let a global-scale quantity override a per-locality one through a control the user cannot see, coarsening every compact locality in a dataset whose fields lie far apart.

---

## 3. Spatial Boundary & Dynamic Buffering Systems

The boundary determines the displayed prediction domain. Buffers are geometric choices, not statistically estimated limits of reliable prediction.

### 3.1 Boundary types

The sidebar's **Boundary Type** control offers four options, named below as it names them; an uploaded boundary shapefile can override any of them per locality.

* **Convex hull / Concave hull:** the convex hull encloses the samples with straight outer edges; the concave hull follows their outline more closely. They do not use or support buffer padding.
* **Buffered:** a concave hull inflated outwards by the buffer distance (<i>D</i><sub>b</sub>) so the mapped area covers the field's outer borders.
* **Point buffer:** individual buffer circles around every point, unioned together.
* **Uploaded boundary:** replaces the types above for a locality when it overlaps that locality's samples. Name-matched features are unioned and used if they intersect the samples. Without a name match, the first unshared intersecting feature is used. A failed name match goes to the sidebar boundary, not to an unnamed feature. An unnamed feature shared by several localities, including a point/line layer's combined hull, is skipped in favour of each locality's selected sidebar boundary. Use an attribute column of locality names for explicit assignments. A feature that intersects none of the samples is refused, including a name-matched one, because gridding it would move the prediction domain away from the data. The locality then uses the sidebar boundary type and the run log says so. Point and line layers are replaced by the convex hull of their features, and a hull of fewer than three non-collinear vertices encloses no area and is refused. A layer without a CRS definition is read in the Input Data CRS. The overlap test runs in the data's working CRS, the frame in which the grid is clipped.

### 3.2 Dynamic buffering (Buffered mode)

Instead of one universal buffer distance, padding scales with the active resolution.

* **Calculation:** under *Auto* resolution the buffer scales with the density-derived resolution (<i>R</i><sub>local</sub>); under *Fixed* resolution it scales with the slider value (<i>R</i><sub>manual</sub>), so adjusting the slider updates the buffer column live.
* **Method-specific ratios:**
  1. **TPS**, <i>D</i><sub>b</sub> = 1.0 &times; Resolution. The smallest multiplier limits the spline's displayed extrapolation domain.
  2. **IDW**, <i>D</i><sub>b</sub> = 2.0 &times; Resolution. This is intermediate padding. With finite `nmax`, distant predictions approach an average of the selected neighbours, not necessarily the global mean.
  3. **Kriging suite (OK, CK, RK, RFK)**, <i>D</i><sub>b</sub> = 3.0 &times; Resolution. This padding is independent of the fitted variogram range and does not establish safe extrapolation.
* **Bounds:** the dynamic buffer is clamped to [5, 2000] m.

### 3.3 Point buffer: manual enforcement

Dynamic buffering in this mode exaggerates coverage: at a coarse 200 m resolution a 3x multiplier draws 600 m circles around every point, claiming a vast mapped footprint the sampling does not support. **Point buffer** therefore has no dynamic buffer logic: the **Buffer Distance (m)** field is always used as the fixed radius around each point.

**Grid evaluation domain:** the prediction grid is generated over the boundary's bounding box, and nodes outside the boundary are excluded *before* the engines run rather than masked afterwards. All engines predict pointwise, so no within-boundary value changes; this only removes computation the mask would have discarded, a substantial saving for concave or multi-part boundaries.

### 3.4 Buffer radius and cell size must be coherent

A cell is kept when its **centre** falls inside the boundary. The engine grid uses a centre-in-boundary test; the later raster mask and output reprojection are separate operations, but under a Point buffer boundary it couples the buffer to the cell size, because there the boundary *is* the support claim: the union of circles of radius <i>D</i><sub>b</sub> around the samples.

For an ideal circular buffer and a square working grid, a sample lies anywhere within its own cell, so its distance to that cell's centre ranges from 0 to half the cell diagonal, <i>R</i>/&radic;2. On a square grid the containing cell's centre is also the *nearest* cell centre (each cell is the Voronoi region of its own centre), so an isolated sample paints no cell anywhere precisely when that one centre escapes its buffer. A sufficient worst-case condition for every isolated sample to keep its containing cell is

<div style="text-align:center;"><i>D</i><sub>b</sub> &ge; <i>R</i> / &radic;2 &nbsp;&nbsp;&hArr;&nbsp;&nbsp; <i>R</i> &le; <i>D</i><sub>b</sub> &radic;2</div>

Below that threshold, the share of in-cell sample positions whose cell centre escapes the buffer is the cell area left uncovered by a disc of radius <i>D</i><sub>b</sub> centred on the cell centre. That disc is inscribed in the cell only while <i>D</i><sub>b</sub> &le; <i>R</i>/2; wider than that it spills across the four edges, and the overspill (four circular segments) must be subtracted. With <i>t</i> = <i>D</i><sub>b</sub>/<i>R</i>:

<div style="text-align:center;"><i>f</i><sub>lost</sub> = 1 &minus; &pi;<i>t</i><sup>2</sup> &nbsp;&nbsp;&nbsp;for <i>t</i> &le; &frac12;</div>
<div style="text-align:center;"><i>f</i><sub>lost</sub> = 1 &minus; { &pi;<i>t</i><sup>2</sup> &minus; 4 [ <i>t</i><sup>2</sup> arccos(1 / 2<i>t</i>) &minus; &frac12; &radic;( <i>t</i><sup>2</sup> &minus; &frac14; ) ] } &nbsp;&nbsp;&nbsp;for &frac12; &lt; <i>t</i> &lt; 1/&radic;2</div>

Dropping the segment term would understate the loss above <i>D</i><sub>b</sub> = <i>R</i>/2 and report none at all above <i>D</i><sub>b</sub> = <i>R</i>/&radic;&pi;, inside the very range the threshold flags. With it, <i>f</i><sub>lost</sub> falls continuously to exactly zero at <i>t</i> = 1/&radic;2, so a flagged pair always carries a non-zero loss.

<i>f</i><sub>lost</sub> is the geometric probability for a uniformly positioned isolated sample within a cell, not the measured fraction of uploaded samples lost: samples inside dense clusters are covered by their neighbours' buffers, so the gaps appear only around isolated samples. At <i>D</i><sub>b</sub> = <i>R</i>/2 (for example a 175 m buffer on a 350 m grid) it reaches 1 &minus; &pi;/4 &asymp; 21%, and it is why an otherwise valid run can show sampled points sitting on blank map.

Monolith treats an incoherent pair as an advisory, not an error: the run is scientifically sound, its support is simply under-resolved relative to the claim being made. The Domain & Grid sidebar section flags the pair live under Fixed resolution, and every run re-checks it against the resolution actually used, raising the finding as a notification and writing it to the run log with the corrective values (&lceil;<i>R</i> / &radic;2&rceil; metres of buffer, and &lfloor;<i>D</i><sub>b</sub> &radic;2&rfloor; metres of cell size when that is a size the suite lets you set, 1 m here and 5 m in the Classification Suite; below it only the buffer arm is offered, since advice that cannot be acted on is worse than none). Lowering the resolution is generally preferable to widening the buffer, since widening it enlarges the area the map claims to support.

The same rule and the same advisory apply to the Classification Suite's Spatial Scope panel, which clips its prediction grid on the same centre-in-boundary test. The two differ only on a centre lying exactly on the boundary line: the interpolation grid keeps it (`st_intersects`) and the classification grid does not (`st_within`). Nothing in <i>f</i><sub>lost</sub> turns on that measure-zero case.

Hull boundaries are not flagged. Cells near their perimeter can lose sampled points, including interior points less than half a cell diagonal from the boundary. Dynamic Buffered-mode buffers use density-derived spacing in Auto mode, which can differ from the actual grid spacing, and are capped at 2000 m. They are not guaranteed to exceed half the actual cell diagonal.

### 3.5 On-map distance measurement (ruler)

The Map Viewer's ruler measures a path clicked on the map. A distance on a curved Earth has no single correct value until the surface and the metric are stated, so both figures the tool reports are named rather than merged into one number.

**Ground distance (WGS84 geodesic).** The path is measured as a sequence of geodesic segments on the WGS84 ellipsoid (<i>a</i> = 6378137 m, 1/<i>f</i> = 298.257223563), evaluated with Karney's algorithm through `terra`. This is the shortest surface distance between the clicked points and is independent of any map projection. The enclosed area of a closed path is the corresponding ellipsoidal area, the same `terra::expanse` call the class-zone export and the Area Coverage table use, so the ruler and the reported class areas rest on one convention.

From the third vertex the shape is a ring, and the reported length closes with it: the closing segment <i>v</i><sub>n</sub> &rarr; <i>v</i><sub>1</sub> is included, making the figure the perimeter of exactly the polygon whose area is reported beside it. Two vertices remain an open line and report a length.

A ring that crosses itself reports its perimeter but **no area**. Both engines integrate around the ring in traversal order (the shoelace sum &frac12; |&sum;<sub>i</sub> (<i>x</i><sub>i</sub> <i>y</i><sub>i+1</sub> &minus; <i>x</i><sub>i+1</sub> <i>y</i><sub>i</sub>)| in the plane, its geodesic counterpart on the ellipsoid), and on a figure-eight the two lobes are traversed in opposite senses, so the result is their *difference* rather than the area enclosed on the screen: a symmetric bowtie evaluates to zero. The crossing is detected on the clicked vertices themselves, so it is a property of the shape rather than of the projection. The perimeter is unaffected by a crossing and is still exact, so it is kept.

**Projected distance (Target Mapping CRS).** The same vertices are transformed into the Target Mapping CRS and measured planimetrically, &sum;<sub>i</sub> &radic;( (<i>x</i><sub>i+1</sub> &minus; <i>x</i><sub>i</sub>)<sup>2</sup> + (<i>y</i><sub>i+1</sub> &minus; <i>y</i><sub>i</sub>)<sup>2</sup> ), with the area taken by the shoelace formula. The engines compute variogram lags, IDW separation distances, TPS coordinates, grid resolution and buffer radii in the run's working CRS (Section 3.6), so this column measures in the engines' own metric when the Target Mapping CRS is that same system: the projected Input Data CRS, or, for geographic or non-metre input, the WGS 84 UTM zone of the data. Where the two systems differ, a measured separation and a fitted variogram range differ by the ratio of their scale factors at that place.

The projected figure is reported for the Target Mapping CRS of the **displayed run**, not for whatever the sidebar currently holds, so retargeting the sidebar for the next run does not relabel a measurement taken on the surface on screen.

The difference between the two is the projection's distance distortion at that place and orientation. For a conformal projection such as UTM it is governed by the point scale factor <i>k</i>, which on a transverse Mercator zone runs from <i>k</i><sub>0</sub> = 0.9996 on the central meridian to roughly 1.0010 at the zone edge. A gap of a few **hundredths** of a percent is therefore the normal state inside a zone, not a warning sign: measured at 39°N in zone 36N the projected length runs -0.040% against the ground figure on the central meridian, passes through zero about 2.1° from it, and reaches +0.043% at the zone edge, with the area gap twice that because it scales as <i>k</i><sup>2</sup>. Near the equator the zone is wider on the ground and the edge value approaches 0.1%. A discrepancy well beyond this is diagnostic rather than cosmetic: it indicates that the selected CRS is a poor fit for the area being analysed. When the Target Mapping CRS is geographic, no planar figure is produced at all, for the reason `validate_and_project_sf()` refuses to interpolate in degrees: a Euclidean length computed on longitude and latitude is not a length.

**Metric axis units are required of the Target Mapping CRS.** A projected Target Mapping CRS whose linear unit is not the metre is refused before a run starts (`validate_crs(require_metric = TRUE)`), and reported in the Target CRS panel as soon as it is selected. That panel carries this rule and the suitability verdict of §3.7 together, because the two are orthogonal and would otherwise contradict each other: a State Plane zone in US survey feet sits inside its own declared area with <i>k</i> &asymp; 1 and is still refused. In such a CRS the exported rasters, the sidebar resolution suggestion and the ruler's projected column would read feet as metres, a factor of 3.28. A geographic Target CRS is unaffected. Both interpolation and classification enforce the Target CRS unit gate. Geographic or non-metre projected Input Data CRSs are transformed to the locality's WGS 84 UTM zone before interpolation. Metre-based projected input is used as supplied; Data Setup warns when its measured distance distortion exceeds 1%. This warning does not transform the uploaded coordinates.

The interactive measure control computes on a sphere of radius <i>R</i> = 6371000 m: great-circle segment lengths and a spherical-excess area. That is convenient for the running readout it shows while points are being clicked, but it has no knowledge of the analysis CRS and it departs from the ellipsoidal value by up to about 0.5% in length (largest for meridional paths at low latitude, where the meridional radius of curvature <i>M</i> = 6335439 m is furthest below <i>R</i>). In area the departure follows <i>R</i><sup>2</sup> / (<i>MN</i>), so it vanishes near 35° latitude and grows to about +0.45% at the equator and -0.9% towards the poles. Its finished figures are therefore replaced by the server-side ones described above, computed for the shape and stored with it, so each measurement carries a single authoritative set of numbers that stays correct when it is reopened later. Measurement is a read-only aid: it records nothing, exports nothing and does not enter any model.

---

### 3.6 Identifying the input CRS

The Input Data CRS is the system the uploaded X/Y columns were recorded in, and it determines the working system for spatial interpolation: `run_regional_interpolation()` builds its points with it, projects them to the WGS 84 UTM zone of the locality's mean position when it is geographic or uses a non-metre linear unit (`validate_and_project_sf()`), and otherwise uses it as supplied. This is the interpolation run's working CRS. For these runs, the Target Mapping CRS is applied at the end, to reproject the finished surface for display and export. The Classification Suite uses its Target CRS for computation and applies the unit and suitability gates described above. It cannot be given a default, because a projected coordinate pair does not contain its own CRS.

**Why bare eastings cannot be resolved.** The 60 UTM zones are congruent: the same (<i>E</i>, <i>N</i>) pair is a valid position in every one of them, reproducing an identical latitude and a longitude displaced by exactly 6° per zone. Reading zone-33 coordinates as zone 35 therefore moves a survey 12° east, about 812 km at 52° N, with no coordinate falling outside its zone's valid range and no error raised anywhere. Any default zone is a silent mis-georeferencing for every user outside it, so Monolith ships with both CRS selectors empty and refuses to run until one is chosen.

**What the wrong zone does and does not affect.** Because the zones are congruent, every coordinate *difference* is preserved: separation distances, variogram lags, fitted ranges and all cross-validation metrics are numerically identical under a wrong zone of the same hemisphere. What is wrong is the georeferencing, so the basemap, the exported GeoTIFF and shapefile, and any polygon drawn on the map all describe the wrong place. The exception is the output reprojection: resampling into a zone the data does not belong to inflates distances by the point scale factor of §3.5, about 1.07% two zones off at 52° N.

**Identification from evidence.** Values inside &plusmn;180 / &plusmn;90 show only that the numbers *could* be longitude/latitude: a local metre grid (a field plot with <i>X</i> = 2 to 48, <i>Y</i> = 5 to 60) fits the same bounds, and read as degrees it becomes a domain thousands of kilometres wide. So a degree-range pair is identified as EPSG:4326 only with evidence that the numbers are degrees: X and Y columns whose whole names are longitude/latitude tokens (`lon`, `long`, `lng`, `longitude` and `lat`, `latitude`), or an uploaded boundary shapefile with a `.prj` that contains more than half of the points read as degrees (with the same 5% diagonal margin as below). Otherwise nothing is set and the user is asked to confirm EPSG:4326 or set the recorded CRS. Angular bounds cannot establish the datum either: the identification assumes WGS 84 and says so, and coordinates recorded on another geographic datum (for example ED50) need that CRS set explicitly. For projected coordinates the app tests a short candidate list rather than searching a catalogue: a companion `lon`/`lat` pair in the same table fixes the zone exactly through <i>z</i> = &lfloor;(&lambda; + 180) / 6&rfloor; + 1, leaving the WGS84 zone, its southern twin, the ETRS89 equivalent and Web Mercator as the four candidates. Each is scored by the largest great-circle distance between the transformed (<i>x</i>, <i>y</i>) and the reference pair. Candidates that place the data within 5 m of one another are collapsed first, since a WGS84 UTM zone and its ETRS89 counterpart are the same grid to under a metre and would otherwise tie with each other; the survivor is accepted only when it reproduces the reference pair to within 5 m and every remaining candidate is at least 100&times; worse. For coordinates at Potsdam read in UTM 33N, the nearest rival candidate lies about 988 km from the reference pair.

Where no geographic pair exists, an uploaded boundary shapefile with a `.prj` serves the same purpose. The boundary's own CRS is tested alongside the zone candidates, each is scored by the fraction of points falling inside the boundary, and one is accepted only when it holds at least half the points and no other candidate holds more than half of its fraction. **Narrowing when there is no evidence.** Failing both, the app does not guess. It asks where the data is and answers with the projections that put it there, which converts an unanswerable question about a code into an answerable one about a place. The candidate universe is the non-deprecated EPSG projected codes in the installed PROJ catalogue (`proj.db`) with an area of use; no region is assumed and no national-grid list is curated.

The selection runs in the order that makes it tractable. Transforming every catalogue entry is slow, so the first filter touches no geometry at all: a candidate is kept only if the lat/lon box it declares as its area of use contains the point the user indicated. That is a comparison on the query result, and narrows the candidates before transformation; counts depend on the installed catalogue. The survivors are then transformed and filtered twice more: the resulting position must fall inside the *same* candidate's declared area of use, which is what rejects a national grid the data does not belong to, and it must lie within 500 km of the indicated point, which is what rejects the world-scale display projections that satisfy any containment test trivially. Candidates whose positions agree to within 250 m are then collapsed onto one row, so each row of the shortlist is one *place* carrying its equivalent codes, ordered by distance from the indicated point. That tolerance is far wider than the 5 m used for identification above, deliberately: a grid's datum siblings differ by less than a map click can resolve (WGS 84, WGS 72, WGS 72BE and ED50 UTM zone 33N read the same Potsdam coordinates within 209 m of each other), so presenting them as competing rows ranked by their distance to that click lets click error, which is kilometres, decide between differences of metres, and an obsolete datum can lead. Folded onto one row they are ordered by preference instead (WGS 84 first, then ETRS89), and the row states the size of the fold rather than implying the codes are interchangeable. Two genuinely different answers, adjacent UTM zones, sit some 400 km apart and are never folded. Free text (a country or region) is matched against the CRS name and the area-of-use description, which names the countries each CRS covers, but it is a secondary path and is capped at 120 codes: unlike a point it does not bound the set, broad country names can match many codes, and a capped search is reported as capped. The shortlist shows at most 8 rows. Text supplied alongside a click narrows the pool the click has already bounded; text matching nothing inside it returns an empty shortlist, never a search widened back out.

On a Potsdam UTM 33N file the shortlist is a single position, EPSG:32633, carrying EPSG:3045, 25833, 10733, 23033, 32233 and 32433 as codes reading the same place to within 209 m; the neighbouring zones, which sit 406 km away, are not offered. This never becomes an identification claim. The app selects a candidate when its evidence thresholds are met, narrows it to a short list when it does not, and does not default to a projected zone. Identification is conditional on the candidate list and the supplied evidence. Where no candidate lands near the indicated point the shortlist is empty rather than a best guess. Throughout, the position the current selection produces is reported in degrees, so a wrong zone is visible before any model is fitted.

### 3.7 Choosing the Target Mapping CRS

The Target Mapping CRS is the system the finished surface is resampled into (`terra::project`), and it carries the exported rasters and their cell size, the sidebar resolution suggestion (§2.1) and the ruler's projected column. The models themselves are fitted in the run's working CRS (§3.6), which coincides with the Target Mapping CRS only when the two are the same system.

> **Recommended setting for projected data:** use the same UTM zone suited to the study area as both the Input Data CRS and the Target Mapping CRS. The working CRS is then the Target Mapping CRS, so the axis-unit rule and the scale-factor check below also apply to the grid, buffers and variogram lags, the final resampling leaves the surface unchanged, and the ruler's projected column is directly comparable with fitted ranges. A CRS is fit for the target role only where it measures correctly, and the axis-unit rule of §3.5 is necessary but not sufficient. Web Mercator's axis unit *is* the metre, and at 52° N it reports every distance 64% too long.

**The point scale factor.** A projection's local distance distortion is the point scale factor <i>k</i>, the ratio of a projected length to the true length of the same infinitesimal arc on the ellipsoid. Monolith measures it empirically rather than parsing projection parameters, so transformable projected CRSs can be screened where coordinate and unit checks succeed. Two short arcs are projected at each sample position and compared against differential approximations to their ellipsoidal ground lengths on WGS84 (<i>a</i> = 6378137 m, 1/<i>f</i> = 298.257223563):

<div style="text-align:center;"><i>s</i><sub>&lambda;</sub> = <i>N</i>(&phi;) cos&phi; &Delta;&lambda;, &nbsp;&nbsp;&nbsp; <i>N</i>(&phi;) = <i>a</i> / &radic;( 1 &minus; <i>e</i><sup>2</sup> sin<sup>2</sup>&phi; )</div>

<div style="text-align:center;"><i>s</i><sub>&phi;</sub> = <i>M</i>(&phi;) &Delta;&phi;, &nbsp;&nbsp;&nbsp; <i>M</i>(&phi;) = <i>a</i>( 1 &minus; <i>e</i><sup>2</sup> ) / ( 1 &minus; <i>e</i><sup>2</sup> sin<sup>2</sup>&phi; )<sup>3/2</sup></div>

Both arcs are measured, not one. For a conformal projection (transverse Mercator, Mercator, Lambert conformal conic) <i>k</i> is identical in every direction and either arc suffices, but a nonconformal projection has direction-dependent scale: EPSG:5070 (Albers, conterminous US) evaluated at Potsdam gives +3.17% along the parallel and -3.07% along the meridian. Equal-area systems are exactly what a user might wrongly choose for distance work, so the larger tested |<i>k</i> &minus; 1| decides. These directions need not be the principal distortion directions in a nonconformal projection; the screen does not bound distortion at every bearing. Each arc spans 0.001° centred on the position (&plusmn;0.0005°), about 111 m along a meridian, short enough that the result is the point scale factor rather than a finite-distance average.

**Where it is evaluated.** At the data centroid and the four corners of the data bounding box, taking the worst case. A large study area can be inside tolerance at its centre and outside it at an edge, which is the zone-boundary case precisely. All five positions come from the same `crs_landing_position()` used for the Data Setup readout of §3.6, so the gate and the readout can never disagree about where the data is.

**Thresholds.** |<i>k</i> &minus; 1| &gt; 0.1% raises a warning; &gt; 1% refuses the run, subject to an explicit override recorded in the run configuration. These are application policy thresholds, not universal scientific tolerances. As a reference, a UTM zone holds |<i>k</i> &minus; 1| below 0.1% across its own 6° of longitude (0.04% on its central meridian, just under 0.1% at the zone edge on the equator; §3.5), and a 1% distortion makes a 250 m length read as 252.5 m. Suitability also depends on purpose and study extent. Measured at Potsdam (12.958° E, 52.466° N):

| CRS | <i>k</i> | <i>k</i> &minus; 1 | verdict |
|---|---|---|---|
| EPSG:32633 (UTM 33N) | 0.999836 | -0.016% | fit, the correct zone |
| EPSG:25833 (ETRS89 / UTM 33N) | 0.999836 | -0.016% | fit |
| EPSG:3035 (LAEA Europe) | 1.000113 | +0.011% | fit |
| EPSG:5514 (S-JTSK Krovak) | 1.000356 | +0.036% | outside its declared area, measures correctly |
| EPSG:32634 (UTM 34N) | 1.003260 | +0.326% | warned |
| EPSG:32635 (UTM 35N) | 1.010729 | +1.073% | refused |
| EPSG:3857 (Web Mercator) | 1.642049 | +64.2% | refused |

**Area of use.** The check uses available EPSG area-of-use boxes, read from PROJ's own catalogue (§3.6) and from the WKT2 `BBOX` node where the catalogue has no row. A code may declare several boxes (for example, a zone strip and a broader national area), and their union is the area of use, so data inside any one of them is inside, which is the same rule the candidate filter of §3.6 applies when it offers that code. Data landing outside that box raises a warning and never refuses a run. The declared box is an authority's advisory bound, not a measurement: a study area straddling a zone boundary sits outside one box while still measuring correctly, and EPSG:5514 above is outside its area at Potsdam yet distorts distances by only 0.036%. The scale factor is what measures the harm, so the scale factor is what refuses.

**Limits of the suitability check.** Geographic targets bypass the scale-factor check. Missing extents or failed scale calculations can leave no warning; this is not evidence of suitability. Invalid CRSs are rejected by the separate parsing gate. Classification refuses a computed `block` verdict without an override; interpolation offers the recorded override above.

Web Mercator is therefore not offered as a Target Mapping CRS at all. It remains available as an *input* CRS, where data genuinely can arrive in it, and typing it into the target selector reaches the gate like any other entry. The check costs a handful of coordinate transforms and runs on the main thread; it decides which runs are permitted and alters no value a permitted run produces.

**The recommendation.** A verdict that a CRS is unsuitable is only half an answer, and the half the user cannot act on: the declared area of use is a lat/lon box, not a prescription. Monolith therefore states which CRS *is* fit for the data, computed from the data's own position rather than looked up. The longitude fixes the UTM zone exactly, zone = &lfloor;(&lambda; + 180) / 6&rfloor; + 1, and the mean latitude picks the hemisphere, giving EPSG 32600 + zone north and 32700 + zone south; PROJ's catalogue is consulted only for the code's proper name. WGS 84 / UTM is recommended in every region rather than the local national grid using a simple zone rule. UTM is intended for roughly 80°S to 84°N; this is not a polar-CRS selector. Within that domain, UTM holds |<i>k</i> &minus; 1| below 0.1% across its own zone (§3.5), and requires no judgement about which of several national systems an area belongs to. A user preferring a national grid enters it directly, and it is then judged on the same two rules as any other entry.

The zone is taken from the mean of the diagnostic positions (centroid and bounding-box corners), and its scale is checked at those positions, so a study area spanning zones reports the worst case of what is being offered. The recommendation is withheld when its deviation is no better than that of the current selection, which is the case for a study area wide enough that no single zone holds tolerance at all its corners: offering one there would move the user between two CRS that are both refused.


## 4. Automated Optimizations

### 4.1 Variogram optimization

The empirical semivariogram <i>&gamma;(h)</i> quantifies spatial dependence as half the average squared difference between value pairs separated by lag <i>h</i>:
<br><br>
<div style="text-align:center;"><i>&gamma;(h) = (1 / 2N(h)) &sum; [Z(x<sub>i</sub>) - Z(x<sub>i</sub> + h)]<sup>2</sup></i></div>
<br>
Geostatistical models require a theoretical curve fitted to this empirical scatter.

**Tuning parameters**

- **Nugget (<i>C<sub>0</sub></i>):** the y-intercept. In theory <i>&gamma;(0) = 0</i>, but measurement error and micro-scale variation create a discontinuity at the origin. A high nugget implies a noisy dataset.
- **Partial sill (<i>C</i>):** the structured spatial variance. The total sill (<i>C<sub>0</sub> + C</i>) is the a priori variance of the data, where the variogram flattens.
- **Range (<i>a</i>):** a family-specific parameter. Spherical covariance reaches zero at *a*; exponential, Gaussian and Matérn covariance decay asymptotically. Practical range indicates approximately negligible covariance, not general statistical independence.

**Auto-fit engine.** To avoid singular fits on difficult datasets, the auto-fit screens a grid of candidates rather than trusting a single fit. It fits four families, Sph (spherical), Exp (exponential), Gau (Gaussian) and Mat (Matern, &nu; = 1.5), from each of four starting ranges (`max_dist/10`, `/5`, `/4`, `/2`), and keeps the eligible candidate with the lowest weighted-least-squares error, in the preference order set out under *Auto-fit eligibility, identifiability and the order of preference* below. The weights are gstat's default (`fit.method = 7`), <i>N<sub>j</sub> / h<sub>j</sub><sup>2</sup></i> for a lag bin with <i>N<sub>j</sub></i> pairs at mean distance <i>h<sub>j</sub></i> (Pebesma 2004), which favour well-populated short lags. They are not the Cressie (1985) weights <i>N<sub>j</sub> / &gamma;(h<sub>j</sub>)<sup>2</sup></i> (`fit.method = 2`), which depend on the model being fitted; the same default applies to the co-kriging LMC fit (`fit.lmc`). If no candidate qualifies, a heuristic spherical model is returned rather than an error: nugget-dominated (95% nugget, range `max_dist/10`) when the empirical nugget already exceeds 80% of the sample variance, otherwise structured (80% partial sill, range `max_dist/2`). An empirical variogram with fewer than five lag bins skips fitting and takes the structured fallback directly, because `gstat::fit.variogram` can crash on one that short.

**Matern smoothness is fixed, not estimated.** The search fits nugget, partial sill and range by weighted least squares but holds the Matern smoothness &nu; (kappa) at **1.5**, the Matern 3/2 model (Minasny & McBratney 2005). `gstat::fit.variogram` estimates &nu; only when asked (`fit.kappa = TRUE`), and it is deliberately not asked: a free smoothness parameter would give the Matern candidate one more degree of freedom than the other three, making the cross-candidate residual-sum-of-squares comparison an unequal contest. Both Auto-Fit and Manual Tuning define "Mat" as Matern with &nu; = 1.5. The manual preview reports the same weighted criterion, Σ(N_j/h_j²)(γ_j − γ̂_j)², as Auto-Fit. If the empirical variogram suggests a markedly smoother or rougher process near the origin, use **Manual Tuning** to compare families directly: Gaussian is smoothest at the origin and exponential roughest, with spherical and Matern (&nu; = 1.5) between them, so short-lag behaviour can be bracketed without freeing a smoothness parameter.

**The nugget must be non-negative.** Candidates with a negative nugget are excluded before comparing fit errors: they imply negative semivariance sufficiently near the origin. This is a covariance-validity rule, independent of whether a numerical solve happens to return predictions.

**A positive nugget for Gaussian and Matern is an Auto-Fit safety rule.** Gaussian and the fixed Matérn 3/2 model have a quadratic leading term near the origin; the Gaussian process is especially smooth (Stein 1999). A zero-nugget Gaussian or Matérn is a mathematically valid variogram. Monolith nevertheless excludes it from automated selection because closely spaced samples can produce poor numerical conditioning (Posa 1989; Ababou et al. 1994), and negative kriging weights can produce overshoot without numerical singularity (Deutsch 1996). This is a workflow-specific numerical-safety policy supported by the observed failures in Monolith, not a claim that the covariance model is mathematically invalid. The rule applies throughout automated searches, including covariate and fold fits. Spherical/exponential candidates are unaffected; manual models remain as applied.

Condition number is not used as a candidate-selection criterion; an overshoot alone does not prove a numerically ill-conditioned solve.

**Auto-fit eligibility, identifiability and the order of preference.** Two different questions are asked of every fitted candidate, and they carry different consequences.

*Auto-fit eligibility* uses five rules: the weighted least-squares criterion must be finite; the structural partial sill must be finite and positive; the practical range must be finite and positive; the nugget must be finite and non-negative; and a Gaussian or Matern structure must carry more than an effectively zero nugget share. The three parameter rules define a mathematically valid finite candidate in the searched two-component form. A finite criterion is required for comparison. The smooth-origin nugget rule is the Monolith numerical-safety policy described above. A candidate failing any rule is ineligible for automated selection and is not compared on fit error; that wording does not classify every ineligible model as mathematically invalid.

*Identifiability* decides **interpretation**. The practical range of a candidate, the distance at which the model reaches about 95% of its sill, is compared with the span of lags the empirical variogram actually covers: it is *resolved* when it falls between one hundredth and twice the largest empirical lag distance. The comparison is made on the practical range and not on gstat's range parameter *a*, because *a* means a different ground distance in every family: the practical range is *a* for spherical, 3*a* for exponential, &radic;3&middot;*a* for Gaussian and about 4.75&middot;*a* for Matern with &nu; = 1.5 (Goovaerts 1997). A window applied to the raw *a* would judge families by different standards, admitting an exponential structure extending three times further than a spherical one on the same test.

A range outside that span is **not** grounds for rejecting the fit. For a field carrying a large-scale trend, <i>Z(s) = m(s) + &epsilon;(s)</i>, the raw variogram sees the trend as well as the residual covariance,
<br><br>
<div style="text-align:center;"><i>&gamma;<sub>Z</sub>(h) &asymp; &gamma;<sub>&epsilon;</sub>(h) + &frac12; E[(m(s+h) &minus; m(s))<sup>2</sup>]</i></div>
<br>
so it can keep rising to the cutoff. A sill that is not observed inside the lag window is therefore **ambiguous** between a genuinely long-range stationary process and non-stationarity, and the variogram alone cannot separate the two. Discarding a converged fit on that ground would leave a singular fit, or a heuristic guess, describing the data instead; presenting the same fit as a well-identified covariance model would claim support the lags never provided. Monolith does neither: it uses the fit and qualifies what it reports about it.

Among eligible candidates the selection runs in this order:

1. **Converged candidates first, and among them the fitting criterion decides.** The lowest weighted least-squares error wins, whatever its range. Range resolution is not a ranking: a converged candidate that describes the empirical variogram better is not beaten by a poorer one merely because the poorer one's range lies inside the lag window. It only breaks a tie, when a resolved candidate's criterion is numerically equal to the winner's (relative difference below 10<sup>&minus;8</sup>).
2. **Singular or non-converged candidates** only when no eligible candidate converged, under the same rule.
3. **The heuristic spherical model** only when no eligible candidate exists at all.

Within the sampled lags, near-tied fits describe <i>&gamma;(h)</i> almost identically, and ordinary kriging depends only on <i>&gamma;(h)</i>, so a preference margin for resolved ranges would buy little inside the data while overruling a measurably better fit. What an unresolved winner claims is qualified instead, as described below.

**The five states of a fitted variogram.** Every fit carries exactly one state, worst first, and the run log, the map banner and the parameter table all read it:

| State | Meaning |
|---|---|
| `fit_failed` | no candidate could be fitted, or the empirical variogram had fewer than five bins |
| `heuristic_fallback` | candidates were tried and none was eligible for automated selection |
| `singular_selected` | the winner is a singular or non-converged fit |
| `range_unresolved` | the winner converged, but its practical range lies outside the span the lags resolve (reported, never penalised) |
| `ok` | converged, and its range is inside that span |

**What may be claimed about the sill.** A second, stricter test governs interpretation: the sill is **resolved** only when the practical range is at most the largest empirical lag. A fit whose range the window resolves can still fail it, because the window admits a practical range up to twice that lag; the two thresholds are deliberately different because one describes how well the range is supported and the other what may be *claimed* about the sill. Where the sill is not resolved, Structural Dependency is reported as "Not reliably identified" rather than as a percentage, with the model-extrapolated figure available on hover, and the Practical Range cell carries the same qualifier. The exported variogram sheet keeps the numeric percentage, which a file must, and adds `Max Lag` and `Sill Resolved` beside it; both are blank for a manual model, where the user supplied the parameters and no empirical support was recorded.

**A sill that was never observed, with a variogram still rising.** When the sill is unresolved *and* the empirical variogram is still climbing near the cutoff (the mean of the last third of the bins exceeds the mean of the third before it by more than 5% of the observed gamma span), the map carries a hedged advisory: the range and sill are weakly constrained, and this can indicate either long-range spatial dependence or non-stationarity. It is not a detection of non-stationarity, and the app does not state one. The suggested response is to model the systematic trend with covariates (Regression Kriging) and fit the variogram to the residuals.

**A target with no usable variance.** Where a locality's target is constant, or varies only in noise digits (standard deviation at or below 1e-8 of the largest absolute value, the same rule the covariate screen uses), the fitted parameters describe the numerical noise floor: a sill many orders of magnitude below the data, and a structural dependency of 100% manufactured from it. Those parameters are not reported. The Variogram Parameters card states the cause instead, and the variogram panels draw the empirical points without a fitted curve. A variable whose spread sits just above that floor is a real, small variance and is reported normally.

**Manual override.** Automated fits can settle in local minima or chase outliers at long lags. **Manual Tuning** lets you prioritize the fit at short lags, which carry the greatest weight in kriging. The sliders are scaled to the tuned locality's data: nugget and partial sill run from 0 to twice the larger of the sample variance and the stored total sill, and range up to the larger of 1.5 times the bounding-box diagonal and three times the stored range, with steps approximately 1/200 of the axis span, rounded to two significant digits. Range has a positive lower bound of roughly 1/1000 of its upper bound. Where the variance is so small that the sill step falls below 1e-6, the sliders cannot represent it: a standing notification names the locality's variance and asks for the variable to be rescaled in the uploaded file, and the sliders are left in place rather than removed. A model whose nugget and partial sill are both zero has no covariance and is refused. A manual model is used as applied. In **Manual** fitting mode only a model saved with **Apply manual model** is treated as supplied: a locality without one fits its own variogram in the run and receives the same per-fold refit as Auto-Fit (Section 5). A stored model belongs to the variable and data subset it was tuned on, carries that key, and is offered only for that combination; uploading data, reassigning localities, or changing the X, Y, locality or Input CRS mapping clears every stored model. An applied model is replaced only by another applied model, by **OPTIMIZE ALL VARIOGRAMS**, or by that clearing; an interpolation run, Auto-Fit included, does not overwrite it. A Gaussian or Matern model with a nugget below 5% of its total sill is flagged when applied, in the variogram subtitle and on the map. This threshold is advisory and does not guarantee stability or bound overshoot on other data.

### 4.2 IDW optimization

* **Logic:** an automated search finds the **distance power** that minimizes interpolation error for each locality, testing values from 0.5 to 5.0.
* **Fold scheme:** the folds come from the same authority as every reported cross-validation, so the search honours the **Cross-Validation Strategy** selected in the sidebar (Section 5): LOOCV, seeded random 10-fold, or ten spatial k-means blocks, with the same small-*n* degradations. A power tuned on random folds while the metrics table reports Spatial Block CV would be optimistic for the number shown: a random split leaves each held-out point's near neighbours in the training set, which systematically favours a steeper decay. Selecting a strategy therefore changes the optimized power, and with it the IDW surface.
* **Reproducibility:** one deterministic fold-assignment vector (seed `CV_FOLD_SEED`) is shared across all candidate powers, so the comparison is paired and the winner is selected on distance-decay performance rather than fold noise.
* **Local adaptation:** the **OPTIMIZE IDW FACTORS** button computes a separate power for every selected locality, on the rows that locality's run uses, including the **Data Subset** of a Single-split view.
* **What a stored power applies to:** every stored power carries the variable and data subset it was tuned on and is used only for that combination; a run on any other target falls back to the sidebar value. Manual per-locality powers are stored for the Actual and the Predicted surface separately, and the target switch is available in every view that computes a Predicted surface while **Fit Actual/Predicted Separately** is ticked.
* **Unseparated Predicted surface:** with **Fit Actual/Predicted Separately** unticked, the Predicted surface uses the Actual surface's power, as Ordinary Kriging uses the measured-value variogram (Section 1.1): **OPTIMIZE IDW FACTORS** searches the Actual target only, and manual tuning writes the Actual slot only.
* **Projected search:** optimization uses projected metric coordinates, with local UTM conversion for geographic/non-metre input. Distances remain planar, not exact geodesic distances. The optimizer derives its projection from target-complete rows; the run uses all coordinate-valid locality rows, so missingness near a zone boundary can make their auto-selected zones differ.
* **Deduplicated search set:** co-located points (identical projected coordinates at centimetre precision) are removed exactly as the run removes them. Otherwise a held-out point's co-located twin predicts it at distance zero, a guaranteed exact hit for every candidate power, so the search would be scored on inflated skill and on a different point set than the run. This is a no-op for datasets without co-located samples.
* **Selection optimism:** optimized IDW reuses the same folds for power selection and reporting, without an outer evaluation loop. The selection bias is not quantified and need not be small. A power fixed independently of these data has no tuning reuse.

### 4.3 TPS optimization

* **Logic:** the **smoothing parameter** balances honouring individual points against a generalized regional trend. The lambda slider defaults to `< 0` (Auto GCV), applying generalized cross-validation natively during interpolation.
* **GCV diagnostics:** the optimum is found by generalized cross-validation inside `fields::Tps` (Craven & Wahba 1978; Nychka et al. 2021). The best lambda is the value with the lowest GCV score. Lambda = 0 is an exact interpolator (zero error at sample points); higher values give a smoothing spline, usually better for noisy sensor data. Regional Parameters reports the fitted lambda and effective degrees of freedom for each surface. An effective df below 3.5 triggers a near-planar warning: a two-dimensional TPS plane has df 3. The warning identifies GCV or fixed smoothing; the threshold is a descriptive flag, not a hypothesis test. Parameter exports store mode separately from full-precision numeric lambda and effective df.
* **Visualization:** clicking **OPTIMIZE TPS LAMBDA** calls `fields::Tps` for a GCV fit and retains its diagnostic grid on the rows the run uses, including the **Data Subset** of a Single-split view; it overrides Auto mode and plots the **GCV curve** in the Scientific Analysis tab, so you can check whether the search reached a clear minimum. The curve belongs to the variable and subset it was computed for, and is shown only for that combination.
* **What a stored lambda applies to:** as with the IDW power, a stored lambda carries the variable and data subset it was tuned on, applies only to that combination, and is stored per surface; the target switch is available in every view that computes a Predicted surface while **Fit Actual/Predicted Separately** is ticked.
* **Unseparated Predicted surface:** with **Fit Actual/Predicted Separately** unticked, the Predicted surface takes the Actual surface's smoothing, as Ordinary Kriging takes the measured-value variogram (Section 1.1). A fixed lambda is used as it is. Under Auto (GCV) the lambda is the one GCV selects for the *measured* values of the same rows, and cross-validation reselects it in every fold from the measured values of that fold's training rows, so no held-out value, measured or predicted, contributes to the smoothing that predicts it. **OPTIMIZE TPS LAMBDA** then searches the Actual target only.
* **Projected search:** as with IDW, geographic or non-metre input is projected to local UTM first. Both axes are divided by the larger coordinate extent and `fields::Tps(scale.type = "unscaled")` preserves that common scale. The spline is isotropic in the working coordinates; GCV fits rotate with the data.
* **Deduplicated search set:** co-located points are removed before the search, matching the run's point set. `fields::Tps` handles exact replicates through its pure-error machinery, which shifts the GCV curve, so without deduplication the stored lambda would be optimized on a different point set than the run.
* **Selection optimism (fixed lambda only):** a lambda selected using the full dataset is reused in every fold, so CV is conditional on that selection. A value fixed independently of these data does not itself leak held-out outcomes. Auto (GCV) reselects lambda per fold, but all TPS folds retain full-population coordinate normalization. Matching stored per-locality lambdas take precedence over the sidebar: selecting Auto on that slider does not override a stored fixed lambda.

---

## 5. Validation Diagnostics

Cross-validation produces the predicted-versus-observed pairs behind every performance metric. The **Cross-Validation Strategy** control, in the Spatial Engine panel below the **Interpolation** method buttons (and below the **CK Max Neighbors** slider when Co-Kriging is selected), governs how folds are formed. It affects the reported metrics only and **never the interpolated surface**, with one deliberate exception: the IDW power search (Section 4.2) builds its folds from this same control, so that the power selected and the metrics that score it share one validation design. Re-running the IDW optimizer under a different strategy can therefore return a different power, and the resulting surface changes.

- **Auto (default):** leave-one-out cross-validation (LOOCV) at 50 or fewer observations; a seeded, balanced random 10-fold above 50.
- **Standard LOOCV:** full leave-one-out at any sample size. Usually more fits than ten-fold CV, retaining near neighbours of each test point. It is not inherently more rigorous than spatial holdout; runtime depends on data, model and hardware.
- **Spatial Block CV:** ten spatially contiguous folds formed by k-means clustering of the sample coordinates. Random folds place a test point's near neighbours in the training set, so under spatial autocorrelation the model effectively interpolates from almost-collocated data and cross-validation overstates its skill. Holding out blocks reduces training/test proximity, but adjacent k-means blocks can still contain close neighbours because there is no separation buffer. This better represents spatial transfer when the block geometry matches the prediction task, the recommended design for digital soil mapping validation (Roberts et al. 2017; Ploton et al. 2020). Below 30 observations the blocks degenerate, so the engine falls back to LOOCV.

All fold assignments use a fixed seed (`CV_FOLD_SEED = 12345`); see Section 9.1 for where to change it.

> **What each engine refits inside a fold.**
>
> For fold *i*, the fitted components listed below are re-estimated from that fold's training samples, by the code the map uses. The assessment samples contribute their coordinates and nothing else.
>
> * **OK** re-derives the lag geometry, the empirical variogram and the full 16-candidate automated fit. With the Actual/Predicted box unticked, the Predicted surface refits the *measured-value* variogram from the training samples that carry a measured value (Section 1.1).
> * **CK** re-runs the covariate screen, recomputes the standardization means and standard deviations, and refits the primary variogram, the LMC family, its shared range and smoothness seed, and the coregionalization sills.
> * **RK and RFK** re-run the covariate screen, krige each held-out covariate from the fold's training samples (with the map's IDW fallback settings when needed), and refit the trend model (`lm` or `randomForest`) and the residual variogram on those samples. Each RFK fold draws its forest from its own seed, `CV_FOLD_SEED + <fold label>`.
> * **The OK fallback** that catches a failed RK, RFK or CK behaves exactly as OK, and never reuses a supplied model.
>
> **Per-fold variogram state.** An OK fold refits the variogram, and an RK or RFK fold the residual variogram, from its own training samples, so it can land in a different state than the map's fit: a heuristic fallback, a singular winner, or a converged fit whose range the fold's own lags do not resolve (Section 4.1). Every OK, RK and RFK fold reports one of the five states; a CK fold fits no variogram through this screen (the primary fit only seeds the LMC range) and reports a heuristic seed instead. When any fold is not `ok`, the run log names the locality and lists those folds individually with what happened to each, and the note under Model Performance says how many folds of how many were affected. A fold in a degraded state scored a different model than the one that drew the map, which is the reading those lines support.
>
> The global search neighbourhood, `ck_nmax`, `rf_ntree` and the collinearity threshold are settings, not estimates, and are held fixed. For **OK**, a variogram saved with **Apply manual model** is reused: it encodes a judgement that cannot be re-made from a subset, so it is reused unchanged and that locality's row is labelled *conditional on applied variogram*. RK/RFK residual variograms and CK LMCs are auto-fitted; they do not consume the applied OK model. IDW reuses one power and TPS a fixed lambda when supplied (Sections 4.2 and 4.3). CV is also conditional on the selected population, covariates and user settings; TPS retains full-population coordinate normalization.

> **Cross-validation population and coverage.** A cross-validation object holds one row per sample of its population, and the Model Performance table states which population that is, how many of its samples received a prediction, and the identifier of the experiment.
>
> * **Native** (Ordinary Kriging default): the deduplicated samples carrying the current surface's target, measured values for Actual and uploaded predictions for Predicted.
> * **Comparable** (the *CV Population* switch, Ordinary Kriging only): the samples carrying the target **and** every selected auxiliary variable, deduplicated and folded exactly as RK, RFK and CK fold them. OK is *trained and scored* on those samples, so their populations and folds can match. Their model inputs differ: OK uses no covariate values, while the other engines do. The OK map still uses every sample either way. RK, RFK and CK always use these common rows, because their covariates require it.
> * **CV population ID.** Eight hexadecimal characters identifying the target column with its data subset, the uploaded-table row numbers that were scored, and the fold each was assigned to. Matching IDs indicate matching recorded target keys, row indices and partitions, subject to collisions in this short hash. They do not hash data values, model settings or higher realizations, and are not dataset checksums; compare them only within the same unchanged upload. It is shown when hovering the *Source* cell and written to the metrics export. A Spatial Block partition is built from the population's own coordinates, so Native and Comparable fold differently by design.
> * **Coverage.** A fold that fails contributes `NA` predictions for its own samples and a line in the run log; nothing is ever filled in from another engine. The table reports *n = predicted of expected*, marks the row **INCOMPLETE** below 100%, and computes every metric on the predicted samples only; a kriging cross-validation that fails as a whole reports 0 of its expected samples, and the pooled *Total (Combined)* row counts those samples as expected too. Read an INCOMPLETE row as describing the samples it names, not the whole locality.

> **Why the covariate screen runs inside each fold.** RK, RFK and CK re-run the constant/VIF covariate screen on every fold's training samples, with the run's threshold, so a fold can keep a different covariate set than the map. Cross-validation estimates the error of a *procedure* only when every data-driven step of that procedure is repeated on the training data (Hastie, Tibshirani & Friedman 2009, §7.10.2; Varma & Simon 2006; Cawley & Talbot 2010). Even an unsupervised screen outside CV can use held-out information: here the held-out samples' measured covariates would help decide which covariates their prediction uses, and an unsampled grid location has no measured covariates at all. The cost is one correlation matrix per fold, and the invariant it buys holds for every engine: nothing about a held-out sample except its coordinates reaches its prediction. When a fold's screen differs from the map's, the run log names the covariates and the count, and the note under Model Performance says so.

**Repeated cross-validation (optional).** Every reported metric comes by default from **one** fold assignment. Because the split is itself random (for random k-fold and Spatial Block; LOOCV is deterministic), at moderate sample sizes the spread of a metric across alternative splits can be as large as the difference between two interpolation methods. The fixed seed makes the comparison **paired**, which removes fold luck from the *comparison* but not from each absolute number.

The **Repeated CV** checkbox quantifies that spread. Each engine re-runs its own cross-validation under additional fold assignments (seeds `CV_FOLD_SEED + 1`, `+ 2`, ...) and the Scientific Analysis tab gains a **Fold-Realization Stability** table reporting every displayed error metric as *mean ± standard deviation* across realizations.

* **Nothing else changes.** Realization 1 is the reference run, so the Model Performance table, the residual maps, the CV-residual write-back and the interpolated surface are identical whether the option is on or off. The feature only adds a table.
* **Only the partition varies.** A repeat re-draws the fold assignment and nothing else: data, variogram policy, neighbourhood and trend-model settings are held fixed; fitted models change with the training rows. Each RFK fold's forest is seeded from its own fold label rather than from the realization, so a new partition does not also move where a forest's random stream starts, and the SD isolates fold-assignment variance.
* **Deterministic plans never repeat.** Where the resolved plan is leave-one-out (Standard LOOCV at any n, Auto at n ≤ 50, Spatial Block below n = 30) the partition is unique, so that locality contributes a single realization and the run log says so. In a pooled row such a locality is carried unchanged into every repeat, which is exact for a deterministic plan.
* **How to read the SD.** It is the resolution of the comparison, not an error bar on a physical quantity: a difference smaller than this SD may be unstable. This is not a significance test; comparison requires paired differences and their dependence structure. Mean ± SD of a repeated k-fold is the conventional way to present resampling-based skill estimates (Kohavi 1995; Molinaro et al. 2005). The sample count is reported as a range across realizations wherever they differ, always against the expected count.
* **Cost.** One extra full cross-validation pass per repeat, per locality, per surface. Every kriging engine rebuilds its model in each fold, so five realizations is roughly five times the cross-validation time; prediction surfaces are computed once regardless.
* **Moran's I is not repeated.** It diagnoses one residual field and is reported for realization 1 only.

> **Training-size requirement for Regression Kriging.** Each fit needs at least the retained-covariate count plus two training rows (intercept and one residual degree of freedom). Too few rows cause a named fold failure, or an OK fallback for the main map fit. This size check does not guarantee full design-matrix rank: keeping collinear covariates can still produce aliased coefficients or non-estimable predictions. Reduce covariates or use folds with more training rows when appropriate.

> **Constant target in a locality.** Correlation R² and NSE are undefined for constant observations; Monolith also returns NA for CCC when either vector is constant. RPD is zero when observations are constant and RMSE is positive, and NA when RMSE is zero. RPIQ is NA when observed IQR or RMSE is zero. RMSE, MAE and bias remain defined with enough valid pairs. The constant-target warning groups these metrics together, but their actual denominator rules differ.

**Visualization of unmeasured points on maps.** If the point overlay is activated, The Map Viewer renders all coordinate-valid sample locations in the domain. Points in the upload that lack a valid measurement for the selected target variable render as **hollow circular markers**, are labelled as "Unmeasured" in the map point legend, and their popup states that the target value was not measured. These points do not enter variogram estimation, surface fitting or cross-validation; they remain on the map solely to display field survey geometry and auxiliary sampling support.

Dropping points according to the chosen partition yields the predicted-versus-actual pairs (<i>P<sub>i</sub></i> vs <i>O<sub>i</sub></i>) that `perform_cv` turns into the metric suite.

- **RMSE (root mean square error):**
  <div style="text-align:center;"><i>RMSE = &radic;( &sum; (P<sub>i</sub> - O<sub>i</sub>)<sup>2</sup> / n )</i></div>
  Absolute fit in the units of the variable. Smaller is better.

- **NRMSE (mean, %), i.e. CV(RMSE):**
  <div style="text-align:center;"><i>NRMSE (mean) = RMSE / |O<sub>mean</sub>| &times; 100</i></div>
  RMSE as a percentage of the <strong>absolute</strong> mean of the observations, making error magnitudes comparable across variables on different scales. The label names the normalisation because it is one of several in use (mean, SD, range, IQR); this is the mean-normalised form, also called CV(RMSE). The absolute value keeps the statistic positive for negative-mean variables: RMSE is non-negative, so a signed denominator would give those a meaningless negative error percentage. Undefined (NA) when the mean is 0, and **not reported at all for a target whose observed values span zero** (see the ratio-scale rule below). <strong>NMAE (mean, %)</strong>, reported only in the uploaded-prediction table, is <i>MAE / |O<sub>mean</sub>| &times; 100</i>, normalized the same way and suppressed under the same rule.

- **NRMSE (SD):**
  <div style="text-align:center;"><i>NRMSE (SD) = RMSE / SD<sub>actual</sub></i></div>
  The prediction error as a multiple of the target's own spread, reported as a ratio rather than a percentage. It exists for the case the mean-normalised form cannot serve: because <i>SD(y + c) = SD(y)</i>, it is unchanged by where the variable's origin sits, so it stays defined and comparable for anomaly, centred and other arbitrary-zero interval scales. Undefined (NA) when the observations carry no spread.

  Be exact about what it adds: with this app's definitions (RMSE on divisor <i>n</i>, SD on <i>n − 1</i>) it is a deterministic function of the Nash-Sutcliffe efficiency,
  <div style="text-align:center;"><i>NRMSE (SD) = &radic;( (1 − NSE) (n − 1) / n )</i></div>
  and it is exactly the reciprocal of RPD. It therefore carries **no independent evidence** about model quality beyond those two columns; it is reported because it states the error on a units-free scale that survives recentring, which is what a signed target needs. The author's preference order for signed data is SD, then IQR, then range, then mean; only the SD form is implemented, because one such metric serves that purpose and three near-duplicates would fill the table without adding information.

- **Traditional R² versus correlation R²:**
  - **Traditional R² (Nash-Sutcliffe efficiency; Nash & Sutcliffe 1970):** how well the model predicts relative to using the global mean.
    <div style="text-align:center;"><i>R&sup2; = 1 - &sum;(O<sub>i</sub> - P<sub>i</sub>)<sup>2</sup> / &sum;(O<sub>i</sub> - O<sub>mean</sub>)<sup>2</sup></i></div>
    It penalizes bias and goes negative when the model is worse than the mean.
  - **Correlation R² (Pearson):** linear correlation only. A model predicting exactly double the actual value scores 1.0 here while its traditional R² is at most 0 (exactly 0 for a zero-mean target). Traditional R² is the one to prioritize for spatial accuracy.

- **MBE (mean bias error):**
  <div style="text-align:center;"><i>MBE = &sum; (P<sub>i</sub> - O<sub>i</sub>) / n</i></div>
  Systemic bias: positive means the model generally overestimates.

  > **Two different bias statistics are reported, in opposite directions.** Tables comparing an **uploaded ML prediction column against the observed values** report this MBE, labelled **"MBE (ML pred - observed)"**, in the direction *predicted minus observed*. The **Model Performance** table for the interpolation engines reports **"Bias (ME)"**, the mean cross-validation error, computed as *observed minus predicted*. The two describe different models and carry **opposite signs for the same over-prediction**, so never read one against the other; the labels are deliberately distinct.

- **CCC (Lin's concordance correlation coefficient; Lin 1989):** agreement with the 45-degree line, computed as <i>2s<sub>xy</sub> / (s<sub>x</sub><sup>2</sup> + s<sub>y</sub><sup>2</sup> + (&mu;<sub>x</sub> - &mu;<sub>y</sub>)<sup>2</sup>)</i> using population moments (sample variances/covariance multiplied by (n-1)/n). Monolith reports NA when either vector is constant. This is an application convention: the direct formula equals zero when only one vector is constant or both are constant at different values; it is undefined for identical constants.

- **RPD (ratio of performance to deviation):** <i>RPD = SD<sub>actual</sub> / RMSE</i>. Dimensionless. Uses sample SD. Soil-spectroscopy thresholds (Chang et al. 2001) are context-dependent, not universal grades of prediction quality.

- **RPIQ (ratio of performance to interquartile distance):** <i>RPIQ = (Q3 - Q1) / RMSE</i>. Uses R's default type-7 quartiles. IQR is less sensitive than SD to extreme observations, but RMSE remains sensitive to large errors; common in soil properties such as salinity (Bellon-Maurel et al. 2010).

- **SMAPE (symmetric mean absolute percentage error; Makridakis 1993):** <i>(100/n) Σ 2|P − O|/(|O| + |P|)</i>, bounded from 0 to 200% for finite pairs, but still sensitive to small denominators. Where an observation and its prediction are *both* exactly zero the summand is 0/0; that term is defined as **0** by the usual convention rather than dropped, so sMAPE is averaged over the same sample count as every other metric in the table.

**Undefined metrics are reported as NA, never as infinity.** Zero denominators and selected degenerate cases are handled as follows; the core error metrics require at least two valid pairs:

| Metric | Undefined when | Interpretation |
|---|---|---|
| NSE | observations are constant (SST = 0) | there is no variance for the model to explain |
| NRMSE (mean) | mean of observations is 0 | a zero-mean (centred / anomaly) variable has no scale to normalise against |
| NRMSE (mean), NMAE (mean), SMAPE | observed values span zero | `n/a¹`: a ratio-scale metric on an arbitrary-zero variable (see below) |
| NRMSE (SD) | observations are constant (SD = 0) | there is no spread to express the error in |
| RPD | RMSE = 0 | undefined at zero error |
| RPIQ | RMSE = 0 or observed IQR = 0 | zero-IQR results are suppressed by application convention |
| CCC | either vector is constant | application convention; the direct moment formula can still equal zero |

Reporting `Inf` would propagate into the Model Performance table, the exported metrics CSV and the pooled "Total (Combined)" diagnostics, where it reads as a real and spectacular score. The tables show NA instead.

**Ratio-scale metrics are suppressed for a target whose observed values span zero.** Mean- and percentage-normalised errors need a scale whose zero means "none of the quantity". Where the observations cross zero, that origin is an arbitrary offset: the same fit reported against a differently centred version of the variable gives a different NRMSE (mean), and sMAPE's <i>|O| + |P|</i> denominator collapses at the crossing, so a single sign disagreement contributes its maximum term however small both values are. NRMSE (mean), NMAE (mean) and SMAPE are therefore reported as `n/a¹` rather than as numbers with no interpretation, with a footnote under the table; NRMSE (SD) is reported in their place. RMSE, MAE, both R², bias, CCC, RPD and RPIQ are unaffected and remain the primary metrics.

The operative rule is the **observed sign crossing** (min < 0 < max), because the variable list carries only `Actual`, `Label` and `Category`: the app has no scale semantics to read, and inventing a metadata field for it is out of scope. `|mean| < SD` is deliberately **not** the rule: a positive ratio-scale variable can have SD above its mean and still normalise correctly, while an interval-scale variable with a large mean would pass it and still be arbitrary. The exact-zero-mean guard above remains as a separate numerical branch.

**Precision.** Every metric is computed and exported at full precision; the display formats it to four significant digits, keeping every integer digit from 1000 up and switching to scientific notation below 1e-4, so a small but non-zero value never prints as 0. Rounding at computation time is what let a near-constant target report `RMSE 0` beside a negative NSE, two statements that cannot both describe one set of residuals.

**The two performance tables share one metric dictionary, and so do their exports.** Every statistic in both is computed by the same function (`perform_cv`), so a definition can never differ between them; the export registry builds its sheets from the same presentation builders the cards render (`cv_metrics_export_df()`, `pred_perf_df()`, `agreement_metrics_df()` in `ui_formatting.R`), so a downloaded workbook cannot report a different figure, or fewer of them, than the screen it was taken from. The **Model Performance** table (interpolation cross-validation) reports, in order: RMSE, NRMSE (mean, %), NRMSE (SD), MAE, R² (Corr), R² (NSE/Trad), Bias (ME), Lin's CCC (Agree), RPD (Prec), RPIQ, SMAPE (%), Moran's I and Moran p. The **uploaded-prediction** table reports the same statistics with three deliberate differences of content: it adds NMAE (mean, %), it reports MBE in the *predicted minus observed* direction, and it carries **no Moran's I or p**. It also presents them differently, one metric per row rather than as one wide row, and spells three labels out in full: its R² (NSE/Traditional), R² (Correlation) and RPD (Precision) are the Model Performance table's R² (NSE/Trad), R² (Corr) and RPD (Prec). The app reserves Moran diagnostics for interpolation CV errors. Spatial diagnostics of uploaded-model errors could be informative, but are not computed in that table and would depend on the predictions' provenance.

- **Moran's I (spatial autocorrelation of residuals; Moran 1950):**
  Tests whether cross-validation errors are randomly distributed across the field. Under ordinary cross-validation, significant residual spatial autocorrelation is **consistent with** spatial structure the prediction procedure did not capture; it is a diagnostic to follow up, not an instruction to change engine. Under Spatial Block CV it reads differently, and the paragraph after next states how. The neighbour structure is a **symmetric k-nearest-neighbour contiguity** (`k = 8`, capped at n − 1 for small samples), row-standardised (`spdep::nb2listw(style = "W")`; Bivand & Wong 2018). A kNN definition is scale-stable and avoids an arbitrary distance-band cutoff, which at typical field spacings is wide enough to dilute local autocorrelation toward zero. Because Moran's I is sensitive to the neighbour definition, read the reported value as *the residual autocorrelation under this fixed 8-NN weighting*. Duplicated coordinates are separated by a negligible, data-scaled jitter under a fixed internal seed, so the statistic is exactly reproducible between runs and the global RNG state is untouched (Section 9.2 explains why the displacement must scale with coordinate magnitude). The neighbour count is hardcoded; see Section 9.2 to change it. This is an approximate diagnostic: cross-validated errors can remain dependent because their training sets overlap, and the reported reference distribution does not account for that fitting process.

  **What the table reports.** Moran's I appears with its two-sided p-value in a separate column, and hovering the I cell reveals that row's null expectation, E[I] = −1/(n − 1). Reading I against zero is wrong: under the no-autocorrelation null the statistic is centred slightly *below* zero (−0.034 at n = 30, −0.010 at n = 100), so a marginally positive I is not by itself evidence of clustered errors. The p-value comes from `spdep::moran.test` under the **normality assumption** (`randomisation = FALSE`) and is **two-sided** (a deliberate departure from spdep's one-sided default): this is a diagnostic, and strongly *negative* residual autocorrelation, a checkerboard pattern, is as much a misspecification signal as positive clustering. When the spdep neighbour search fails and the all-pairs 1/d fallback computes I instead (possible only at n ≤ 500), the fallback computes no reference distribution, so the p column reports `NA*` while I and E[I] are still shown.

  **`NA*` means "not computable", never "no structure".** Both Moran cells fall back to `NA*` when the statistic could not be computed at all: fewer than three cross-validated points, no coordinate columns on the cross-validation object, or a neighbour-search failure the fallback could not rescue. This is a *missing measurement*, not a finding of spatial randomness, and the run log records which condition applied.

  **What the statistic means depends on the fold design, and the table says which.** Under Spatial Block CV the pooled out-of-fold residuals also inherit the spatial fold geometry and a shared extrapolation condition within each withheld block: whole neighbourhoods are predicted from training data that excludes their own surroundings, so their errors move together for a reason that has nothing to do with the model's specification. `spdep::moran.test` builds its reference distribution for values on a fixed spatial graph and knows nothing about that shared fold-training process. A large or significant Moran's I therefore indicates spatial clustering of block-CV errors, but **cannot by itself diagnose a missing spatial trend or justify switching to RK or RFK**. Accordingly, a row scored under Spatial Block CV is labelled **Block-CV residual clustering**, marked `†`, and its p-value is not reported (`NA†` on screen, empty in the export, with the `Moran Context` column naming what the statistic measures). Rows that degraded to LOOCV because the locality holds fewer than `CV_BLOCK_MIN_N` samples, and pooled rows that mix fold designs, keep the ordinary reading and their p-value.

  The pairing to carry away: **random-fold Moran is a model-error-structure diagnostic; block Moran is a transfer/extrapolation-error clustering diagnostic.** Neither direction proves the converse, either: an I near its expectation under random folds is not evidence of a clean model, because randomly split folds leave each held-out point's near neighbours in the training set and can mask exactly the residual structure the statistic looks for.

- **Classification performance of a continuous prediction.** Observed and predicted values are binned into classes and compared as a confusion problem, reporting overall accuracy, balanced accuracy, off-by-one accuracy, Matthews correlation coefficient (Matthews 1975), unweighted Cohen's kappa (Cohen 1960) and linearly weighted kappa (Cohen 1968). For **agronomical classes** the bin intervals are left-closed `[low, high)`, identical to the map classification (`terra::classify(..., right = FALSE)`), so a value lying exactly on a class boundary receives the same class in the tables and on the map. **Quartile** binning uses the conventional right-closed intervals on the observed quartiles. The two closures are deliberate and differ only on a value sitting exactly on a break: the agronomical breaks are class limits a map paints, while the quartile breaks are order statistics of the data with no map counterpart. The on-screen Agreement table and the exported *Total Classification Performance* table are computed by the same function, so they cannot report different agreement figures for the same run.

  **Quartile breaks come from the observed values and are unbounded at the ends.** The classes use unique quartile breaks from the **observed** column (ties can yield fewer than four classes), and that one set of breaks is applied to *both* vectors, because a confusion matrix requires a common class definition (breaks re-derived separately from each would compare different classes and make kappa meaningless). The outermost intervals extend to −∞ and +∞, so a prediction outside the observed range lands in Q1 or Q4 rather than being dropped. This keeps every point in the comparison, at the cost that extreme over- or under-predictions are clamped into the end classes: the matrix cannot show *how far* beyond the observed range a prediction went. Read RMSE and Bias alongside the kappa table when extrapolation is a concern.

**Residual semantics on CV failure:** interpolation-validation diagnostics (validation metrics, pooled CV residual variograms, Moran's I, the interpolation-CV point overlay) are computed strictly from cross-validation residuals, read off the cross-validation object and joined to the samples by that object's own coordinates. Training residuals are never substituted into these CV diagnostics; internal RK/RFK variograms intentionally use training-trend residuals, so a sample the cross-validation did not score carries no residual. Failure is reported at the fold: a fold that could not be evaluated leaves `NA` for its own samples, the locality keeps the residuals of every fold that succeeded, and coverage says how many that was. A cross-validation that failed outright leaves the locality's residuals empty.

### 5.1 Directional variogram (anisotropy check)

Every variogram the prediction engines fit is **omnidirectional**: pairs are binned by separation distance regardless of orientation, which assumes geometric isotropy. Real fields often violate this, since a floodplain, a prevailing wind, a tillage direction or a geological strike can give a variable a longer correlation range along one axis than across it.

The **Directional Variogram** panel tests that assumption. Semivariance is recomputed within four angular cones at bearings of 0°, 45°, 90° and 135° measured *clockwise from north* (N-S, NE-SW, E-W, NW-SE), each with a 22.5° half-angle tolerance. The four cones tile the half-circle exactly once, so every pair contributes to exactly one direction and the four curves are disjoint subsets of the omnidirectional one, with pair counts that sum to it (an invariant pinned in the test suite). Two controls select the point set. **Data** chooses the measured values or, for a run that computed a Predicted surface (Comparison Mode or prediction views), the uploaded ML prediction column; for an Actual-only run this switch is omitted and only measured values are analyzed. **Computed on** chooses those values themselves or the cross-validation residuals of that surface. The CV-residual view is available only when cross-validation for that specific surface completed successfully; where CV was skipped or failed, the panel explicitly reports that residuals are unavailable rather than substituting another surface's errors. The CV-residual view diagnoses errors of the full RK/RFK procedure. It is not the training-trend residual field whose variogram the engine actually kriges; that field has its own internal-residual panel. An uploaded prediction column is a field in its own right, and a model's output is typically smoother than the measurements it approximates, so its anisotropy is measured directly rather than inferred from the measured side: reading the two against each other says whether the ML model preserved the directional structure of the field it was trained on. Note that the two *Computed on* choices rest on different preconditions: the values are the uploaded column itself, whereas the residuals require that surface's interpolation and cross-validation to have completed, so one can be available while the other is legitimately absent.

*How to read it:* curves rising to a common sill at clearly different distances indicate **geometric anisotropy** (direction-dependent range); curves plateauing at clearly different sills indicate **zonal anisotropy**. Roughly coincident curves support the isotropy assumption. Coordinates are projected to a metric CRS before the cones are formed, because a bearing measured in degrees of longitude is not a bearing on the ground.

*Scope:* this panel is **strictly diagnostic**. No prediction path consumes it and all engines remain omnidirectional, so nothing on any map changes as a result. Where pronounced anisotropy is found, the honest interpretation is that the reported ranges are directional averages and the uncertainty surface is correspondingly smoothed across directions; acting on the finding requires an anisotropic model, which Monolith does not fit.

**Pooled "Total (Combined)" diagnostics CRS.** Each locality's CV object carries its working CRS, either the supplied metre-based projection or an auto-selected UTM zone, so pooling them for the combined residual variogram and pooled Moran's I requires one common metric CRS. The pooled set is reprojected to the **auto-UTM zone of the combined centroid**, the same zone rule used for geographic input data. Web Mercator is deliberately not used: its distances are inflated by 1/cos(latitude), about 40% at 45° N, which would stretch the pooled variogram's lag axis and distort pooled Moran neighbour distances. Per-locality diagnostics are unaffected. If the per-locality CV objects cannot be combined, the pooled diagnostics show their empty state rather than a partial pool that would misrepresent a subset as the combined result.

**Pooled variance-explained scores are inflated when localities differ in level.** The "Total (Combined)" row runs `perform_cv` on the pooled residuals, so its **R² (NSE/Trad)** and **R² (Corr)** are measured against the *pooled* mean of all localities. Where localities sit at genuinely different levels (different mean salinity, different nutrient status), that between-locality variance enters the total sum of squares and the model is credited with explaining it, even where it explains nothing within any locality. The pooled row can therefore report a markedly higher R² than *every* per-locality row above it. This is an aggregation artifact, not extra skill, the same effect that makes a regression fitted across heterogeneous groups look stronger than any within-group fit. Error-scale metrics in that row (RMSE, MAE, Bias) do not depend on the choice of centre and remain directly interpretable. **Judge model skill on the per-locality rows** and read the pooled row as a summary of overall error magnitude. A within-locality-centred variant is deliberately not computed: it would be a third quantity to explain, and the per-locality rows already answer the question.

### 5.2 What the exported tables carry

Every table the Scientific Analysis tab shows is registered for export, per locality and for the run as a whole, and each is built by the same function that renders the card. Every value is stored and exported at full precision; the card formats it for reading, to four significant digits, in scientific notation below 1e-4, exactly for a whole number and with every integer digit from 1000 up, so a small-unit variable keeps its digits on screen and a count keeps all of them. The clipboard copy carries the displayed form, the export carries the number. Three tables carry something the screen conveys in another way, which a file cannot:

- **Model CV Metrics** exports one wide row per source, matching the Model Performance card column for column, and adds what the card carries as a label or a hover: the resolved fold plan (*CV Design*: for example *LOOCV*, *Random 10-fold CV*, *Spatial Block CV*, or *pooled per-locality CV* for the run-wide row), the cross-validation population and its id (*CV Population*, *CV Population ID*), what the folds re-estimated (*CV Refit*, either *per fold* or *conditional on applied variogram*), the sample accounting (*n expected*, *n predicted*, *Coverage (%)*), Moran's null expectation E[I] = -1/(n - 1), what the Moran statistic measures under this fold design (*Moran Context*), and whether the target's observed values span zero (*Target spans zero*), which is what explains an empty NRMSE (mean) or SMAPE cell in the file.
- **Fold-Realization Stability** exports the mean and the standard deviation in separate numeric columns rather than as the *mean* +/- *SD* string the card prints, so both are directly usable, and carries *n expected* with the *min* and *max n predicted* across realizations.
- **RK Regression Coefficients** exports the panel's table with the p-value as a number (the panel prints "< 0.001") and the confidence interval as two numeric columns.

**Descriptive statistics** are computed on the uploaded rows of the run's localities and effective Single-split subset, co-located duplicates included, in the card and in the export alike. That is a different sample from the interpolation point set, which keeps one sample per coordinate rounded to the centimetre, so its n and moments can differ where duplicates exist. The summary table reports both parametric moments and order statistics: **Count**, **Mean**, **SD**, **Median**, **Q1**, **Q3**, **IQR** (interquartile range, type 7), **MAD** (median absolute deviation from the median, `stats::mad`), **Min**, and **Max**

**Variogram parameters.** The fitted model is exported alongside the empirical points it was fitted to: model family, nugget <i>C</i><sub>0</sub>, total sill <i>C</i><sub>0</sub> + <i>C</i>, the range parameter *a*, the practical range, and **structural dependency**, the partial-sill share of the total sill in percent,

<div style="text-align:center;"><i>SD% = C / (C<sub>0</sub> + C) &times; 100</i></div>

the complement of Cambardella's nugget-to-sill ratio: 100% is a purely spatially structured field and 0% a pure nugget. It is reported as a number rather than a percent-suffixed string, so the column sorts and averages. The range parameter *a* is not comparable across model families: the distance at which a model reaches 95% of its sill is *a* for the spherical (its exact sill distance), 3*a* for the exponential, &radic;3*a* for the Gaussian and about 4.75*a* for the Mat&eacute;rn with &nu; = 1.5 (§4.1). The **practical range** is that distance, and is the column to compare when localities were fitted with different families. The Mat&eacute;rn smoothness &kappa; is reported beside a Mat&eacute;rn model. A nested model lists every structure and reports the ranges of the one that reaches its sill last.

**Classification Performance Metrics** exports a unified multi-table CSV containing: (1) pooled macro performance metrics (Accuracy, Kappa, Balanced Accuracy, Macro Precision, Recall, F1, ROC AUC, Log-Loss, Brier); (2) the per-class table reporting Producer Accuracy (recall), User Accuracy (precision), reference sample support, and model prediction counts; (3) the out-of-fold confusion matrix; and (4) classified land-area coverage in hectares and percentages.

**Copying rather than exporting.** The copy button on each table puts the *rendered* table on the clipboard, every page of it, so it carries the displayed rounding rather than full precision. Use it for reading and drafting; use the export registry when the numbers will be recomputed on.

---

## 6. Residual Analysis

Metrics summarize global performance; residual analysis localizes model failure.

### 6.1 Interpolated delta (surface difference)

When a run computes a Predicted surface (in Comparison Mode or with a prediction value type selected), the actual measured data and the uploaded predicted data are interpolated into two separate surfaces with the chosen method, then subtracted: <i>Surface<sub>Actual</sub> &minus; Surface<sub>Predicted</sub></i>. For Actual-only runs without a comparison, no Predicted surface is interpolated and no delta surface is generated.

**Use case:** maps the net difference between the two surfaces, revealing broad regional zones where pre-calculated machine-learning predictions consistently over- or under-predict the true distribution.

### 6.2 Point errors and interpolated point errors

For runs carrying an uploaded prediction column and configured with a Predicted surface, the discrete error at each sampling location (<i>O<sub>i</sub> &minus; P<sub>i</sub></i>) is drawn as coloured point markers (the Map Viewer's Point Residuals panel, exported as the Point Error Map). An IDW interpolation of coordinate-valid rows with both uploaded values present produces the Interpolated Point Errors Map in the Export Panel. An Actual-only run produces no point-error or residual products, because no model predictions were mapped. Uploaded point coordinates are projected into the locality's working metric CRS before this step, so the error surface is valid for geographic uploads exactly as for projected ones. Uploaded point coordinates are projected into the locality's working metric CRS before this step, so the error surface is valid for geographic uploads exactly as for projected ones.

The error surface uses fixed p = 2 and the run's IDW neighbour count, retaining co-located rows. It can therefore use more rows than the map's deduplicated point markers.

**Use case:** shows the spatial structure of local model failure, where the model in question is the one that produced the uploaded predictions, not Monolith's spatial interpolator. Hotspots mark zones where that prediction model cannot capture the true variability.

---

## 7. Uncertainty Analysis & Confidence Mapping

Uncertainty analysis quantifies the reliability of the prediction at every pixel. It is available for the kriging methods only (OK, RK, RFK, CK), which provide a formal statistical error model. Monolith exports no prediction variance for IDW or TPS (this does not imply that spline uncertainty methods do not exist), so a run using either has no uncertainty product.

### 7.1 Theoretical basis

Kriging uncertainty is a function of the **spatial configuration** of the samples and the **variogram model**.
* **Geometric influence:** kriging variance is generally lower near samples. The point-kriging convention can give zero variance at an exact observed location; a nugget does not automatically leave nugget variance there. Away from an observed location the nugget *is* included, because the reported quantity is the prediction variance of a new measurement (Section 7.2).
* **Variogram influence:** nugget, sill, range, neighbourhood and geometry jointly determine kriging variance; changing one does not imply a uniform increase everywhere.

### 7.2 Uncertainty metrics

* **Kriging variance:** the theoretical mean squared error of the prediction, in the *squared* units of the variable (for example (mg/kg)² for potassium). Variance and concentration have different units and should not be compared by magnitude: an SE of 130 mg/kg corresponds to a variance of about 17,000 (mg/kg)². Conditional on the fitted covariance model; read alongside held-out prediction errors.
* **What the variance is the variance of.** The reported kriging variance is the prediction variance of a **new measurement** at that location, so it **includes the nugget**: it is <i>&sigma;<sup>2</sup><sub>OK</sub>(s) = C(0) &minus; &sum;&lambda;<sub>i</sub> C(s, s<sub>i</sub>) &minus; &mu;</i> evaluated with the fitted model's full sill. It is therefore bounded below by approximately the nugget everywhere except at an observed location, and a standard-error map does not approach zero between samples however densely they are placed. The alternative convention, the variance of the *filtered* (noise-free) value, excludes the nugget and is **not** what Monolith reports.
* **Standard error:** the square root of the variance, in the variable's own units.
* **Interval interpretation:** 2.0% nitrogen with SE 0.2 percentage points gives 2.0 ± 1.96 × 0.2, approximately 1.61% to 2.39%, only under an adequate Gaussian prediction-error model. Variogram estimation, model selection and covariate reconstruction add uncertainty not fully represented here; nominal 95% coverage is not established by the map.
* **Display:** uncertainty layers always render with a continuous, sequential colour scale and the legend states the metric and its unit (for example "Variance: K (mg/kg)^2"). Variance and SE are non-negative magnitudes with no meaningful midpoint, so a diverging palette choice, which would draw one, is replaced by Viridis on these layers in the Map Viewer and the Export Styler. Agronomic and binned class breaks are defined for concentrations and are not applied to uncertainty surfaces, and the class breaks and class areas of the concentration surfaces do not change while an uncertainty map is on screen. Under Match Scales, the Actual and Predicted uncertainty maps share one colour range, as the concentration maps do.

### 7.3 Hybrid model uncertainty (RK & RFK)

For RK and RFK the uncertainty surface combines two terms: **trend uncertainty**, the error in the relationship between the target and the predictors, and **residual uncertainty**, the kriging error of the unexplained variation. The Total map is their sum.

**The Total is an additive sum, assuming zero trend-residual covariance.** The combined variance is `Var(trend) + Var(residual kriging)`, which treats the trend-estimation error and the kriged-residual error as independent. This is the conventional two-step regression-kriging approximation: the trend is fitted first and its residuals kriged separately, so the two error terms are treated as additive. The exact joint treatment, carrying the covariance between the mean-surface estimate and the residual prediction explicitly, is Kriging with External Drift / Universal Kriging, which is not what the two-step engine does. Interpret the Total surface as approximate rather than exact.

**The trend-uncertainty term differs between RK and RFK.** For RK it is the parametric standard error of the linear-model prediction (`se.fit²`), the variance of the estimated mean surface. For RFK no closed form exists, so the term is selectable through the **RFK Uncertainty Method** control:
* **Infinitesimal jackknife (default):** the estimator of Wager, Hastie & Efron (2014) with the Monte-Carlo bias correction. It is the random-forest analogue of RK's `se.fit²`, the sampling variance of the *ensemble-mean* prediction, under its assumptions. The implementation subtracts a finite-tree Monte Carlo correction and clips negative estimates to zero; it does not establish interval calibration for the dataset.
* **Ensemble spread (fast):** the between-tree variance, the spread of individual trees around the ensemble mean. This reflects model *instability* rather than a formal predictive variance and has no guaranteed ordering relative to full predictive uncertainty; treat it as a relative "where is the model least stable?" measure, not an absolute interval.

Switching methods changes **only** the RFK uncertainty surface. The prediction map and the cross-validation metrics are identical either way.

**The kriged covariate surfaces are treated as error-free.** The trend term at a grid cell is evaluated on covariate values that are themselves kriging estimates (Section 8.1). Those surfaces carry their own kriging variance, but it is discarded: the trend variance in the Total is computed **as if the covariate values at each cell were measured without error**. The term a full error budget would add, roughly `Σ (∂trend/∂xⱼ)² · Var(x̂ⱼ)` plus covariate cross-terms, is therefore missing, and the reported RK/RFK uncertainty is **optimistic away from the sample points**, where the covariate surfaces are least certain. The effect is smallest near samples, where covariate estimates are usually best constrained, and grows into unsampled territory, which is exactly where the uncertainty map matters most. This is standard practice for two-step regression kriging with interpolated covariates and is the reason the Total surface should be read as a *relative* reliability map rather than a calibrated absolute interval. The current interpolation path constructs covariate surfaces from points, not directly from exhaustive rasters.

**The RFK trend forest is not hyperparameter-tuned.** It is grown with the `randomForest` package's regression defaults (Liaw & Wiener 2002) apart from the tree count, which is pinned at `ntree = 200`: `mtry = max(⌊p/3⌋, 1)` and `nodesize = 5`, with `importance = TRUE` and `keep.inbag = TRUE` for the variable-importance panel and the jackknife term. No search over `mtry`, `nodesize` or depth is performed and none adapts to sample size. This is a deliberate asymmetry with the Classification Suite (Section 10.6), whose learners do have a tuning registry: the RFK trend is one component of a two-step estimator whose residual variogram is refitted inside every cross-validation fold, so an inner tuning search would multiply an already heavy cost, and the reference random-forest soil maps of Hengl et al. (2015) likewise report no tuning search over the `randomForest` defaults. RFK's trend is therefore a *reasonable default* forest rather than an optimized one; where the covariate set is large relative to n, `mtry = p/3` may be conservative. Adding trend tuning would be a design change, not a parameter tweak.

**RFK residuals are out-of-bag while the trend surface is in-bag.** The residuals whose variogram RFK fits and kriges come from the forest's **out-of-bag** predictions (`randomForest$predicted`), whereas the trend surface at grid cells uses the **full in-bag ensemble**. This asymmetry is deliberate: in-bag residuals tend to be smaller and can understate generalization error, although they need not be zero or pure nugget. Two consequences follow. First, `trend + kriged residual` is not an exact decomposition of the measured value at a sample point, because the two terms come from different ensembles of trees. Second, OOB and full-ensemble training residuals have different distributions; neither the fitted sill nor total uncertainty is guaranteed conservative. RK has no equivalent asymmetry.

**Covariate surfaces are fitted independently of target missingness.** The covariate surfaces RK and RFK evaluate their trend on are kriged from the **full covariate-complete point set** (co-located points deduplicated, samples with a missing target retained), with lag width and cutoff from that same set. The target model is fitted on the point set for its own surface (target-`NA` rows removed first, then deduplicated), with lags derived from *that* set, so the two lag definitions can differ slightly. This is deliberate: a sample lacking a laboratory value for the target still carries valid covariate measurements, and discarding it would needlessly weaken the covariate surfaces. Because the bounding-box diagonal drives the cutoff the difference is usually negligible.

Both maps stay labelled "Variance" and "SE" for UI consistency, and RK versus RFK magnitudes are not directly comparable. Quantile regression forests are deliberately **not** offered: a QRF predictive interval already contains the irreducible residual scatter that this engine models separately through residual kriging, so summing the two under the additive decomposition would double-count that variance.

---

## 8. Data Analytics & PCA Protocols

### 8.1 Multicollinearity filter (PCA and spatial models)

Severe multicollinearity destabilizes multivariate models, so three gates screen for it.

* **PCA module:** before execution the numerical matrix is scanned for pairwise correlations above 0.95 and passed through the iterative VIF screen at VIF > 10. Either finding halts execution; the user drops variables or continues past it, since keeping a correlated variable can be a legitimate choice. The guard reports the correlated pairs (with *r*), the VIF-removed variables (with the VIF at the step each was removed; not finite where the correlation matrix is singular) and the constant variables in three separate sections, because they are different findings. A constant is not a collinearity finding and never halts anything: a column with exactly or effectively no variance over the analysed rows (the near-constant criterion below) carries no information and cannot be standardised, so it is excluded before `prcomp`, the remaining columns keep the requested scaling (an unscaled PCA is never substituted), and the exclusion is named above the results and in every PCA figure's caption. Fewer than two informative columns refuse the PCA with a message naming the excluded ones.
* **Geostatistical engine (RK, RFK, CK):** with more than one auxiliary variable selected, the constant/VIF screen runs on them before the interpolation launches. If it would drop anything, whether a covariate above VIF 10 or a constant one, a modal halts the process and offers **Auto-Drop and Continue**, **Keep All (Not Recommended)** or **Cancel**. Auto-dropped variables are purged from both the interpolation algorithms and the downstream diagnostics (variable-importance plots included). The choice is honoured end to end: keeping collinear covariates disables iterative pruning inside the engines entirely, in the interpolation and classification paths alike, and the collinearity is still reported.
* **Classification suite:** the same iterative-VIF engine screens the selected numeric covariates, with a live warning under the covariate picker and the same modal at run time. The threshold is method-aware: 10 for most learners, tightening to 5 when Random Forest is selected (Section 10.8 explains why).

**Constant handling differs by path.** The interpolation constant/VIF screen removes constants when at least two covariates are supplied, regardless of Keep/Drop. A sole covariate bypasses it: a constant sole covariate is logged and can yield an intercept-only trend or a named fallback. Classification can retain constants under Keep All, but its recipe removes zero-variance predictors during fitting. The near-constant criterion is sd ≤ 10⁻⁸ × max|x| after excluding nonfinite values: invariant to multiplicative unit changes, but not additive offsets.

* *Covariate surfaces:* the gate is resolved **before** the covariates are interpolated onto the prediction grid, so a dropped covariate is never kriged. It is evaluated separately for the Actual and Predicted surfaces, on exactly the point sets each engine fits, because the two can retain different samples and therefore different covariate sets; the grid carries the union of the two retained sets. This changes only which surfaces are built, never the fitted model.
* *Co-Kriging:* CK applies the same gate on the same point sets but does not consume kriged covariate grids, since it predicts target and covariates jointly through the LMC. The gate matters most here: the LMC fits a direct variogram for every covariate **plus every cross-variogram**, and collinear covariates drive the coregionalization matrices toward singularity, making `fit.lmc()` fail and dropping the run into the Ordinary Kriging fallback. A CK run whose covariates trip the threshold therefore gives different, better-conditioned results depending on the Drop/Keep answer.
* *Singular-matrix fallback:* the VIF is obtained by inverting the covariate correlation matrix. When the covariates are so tightly related that inversion fails outright, no VIF exists, and the engine falls back to the pairwise correlation matrix: it takes the most strongly correlated pair and drops the member with the **higher mean absolute correlation against all remaining covariates**, the more globally redundant of the two, repeating until the matrix inverts. Exact ties break on the alphabetically later variable name. Both rules make the outcome a property of the data alone rather than of column order: dropping whichever member appeared first among the columns would let a re-ordered upload change which covariate survives, and with it the fitted model. This path is reached only when `solve()` genuinely fails.
* *Cross-validation:* RK, RFK and CK screen each fold's training rows (Section 5). Classification follows the same rule when **Auto-Drop** is chosen: the screen is a step of the model recipe, and a recipe is re-estimated on every set it is fitted to, so the screen reruns on the training rows of each outer fold, each tuning resample and each nested inner fold. The reported CV therefore evaluates screening and fitting together, and held-out covariate values never help choose the predictors scored on them (Hastie et al. 2009, §7.10.2). The final model is fitted, without that recipe step, on the covariates the same screen keeps on all scoped rows; those are the covariates the map and the exported model use, and the run summary lists them together with how many folds dropped each covariate. The modal's list is computed on the scoped data before the run and can differ slightly from the final model's, which uses complete rows only. Tuning ranges bounded by the predictor count (the Random Forest `mtry`) count the covariates kept on all rows; a fold that keeps fewer caps the value at its own count. **Keep All** runs no screen anywhere.
* *Context sensitivity:* correlation structure is scope-dependent, and two covariates can be nearly collinear across the whole dataset yet independent within one locality, or the reverse. The interpolation gate uses the Context locality selection and the effective Single-split subset, the same rows the run fits, and the classification gate uses the module's resolved spatial scope. The auxiliary correlation screen also uses Context's localities, with independent target and SS partition controls described in Section 8.7; its target, scope and sample count are explicit. A recorded Drop/Keep decision resets whenever the method, the covariate set or the locality selection changes.

### 8.2 Multivariate outlier screening (Mahalanobis distance)

The PCA panel's outlier diagnostic computes the **squared** Mahalanobis distance (Mahalanobis 1936) of each observation in the space of the retained principal components. Because PC scores are uncorrelated by construction their covariance is the diagonal matrix of squared component standard deviations, and the distance over the retained components equals the full-space Mahalanobis distance restricted to that subspace. Components with numerically zero variance, which appear when PCA is force-executed on collinear inputs, are excluded from both the distance and the degrees of freedom of the &chi;<sup>2</sup> reference line (97.5% quantile, df = number of retained nondegenerate components). All nondegenerate fitted PCs are used, independently of biplot-axis selection. The chi-square cutoff is an approximate Gaussian reference, not an exact finite-sample test when centre and scatter are estimated from these same observations.

**The estimator is the classical one, and that is a real limitation.** The centre and scatter are the ordinary sample mean and covariance, which the candidate outliers themselves help define. A single extreme point inflates the covariance in its own direction, and a small cluster of outliers can inflate it enough to pull their distances back under the threshold. This is the classical **masking** effect. The standard remedy is a high-breakdown estimator such as the minimum covariance determinant (Rousseeuw & Van Driessen 1999), which the panel deliberately does not use, since it would introduce a further modelling dependency for a screening plot. The panel names its estimator in the title and subtitle. Read a flagged point as "worth inspecting", and treat an *unflagged* point in a suspicious cluster with caution rather than as evidence of normality.

### 8.3 Partial correlations

A partial correlation removes the linear influence of *k* control variables from both variables before measuring what remains. The estimator is method-dependent, using `ppcor`-style formulas (Kim 2015). For every method, each pair is conditioned on the explicit controls only, never on the other selected targets:

* **Pearson:** the raw values are regressed on the controls and the product-moment correlation of the residuals is reported. For each pair, this equals inversion of the matrix containing that pair and the explicit controls, not additional selected targets.
* **Spearman:** every variable, targets *and* controls, is **rank-transformed first**, and the partial correlation is the product-moment correlation of the *rank* residuals. A Spearman correlation of raw-value residuals is not a partial rank correlation, because the residualization it inherits is a least-squares fit on untransformed values. Since Spearman's coefficient is by definition the Pearson correlation of ranks, the partial version must residualize the ranks.
* **Kendall:** no residualization analogue exists, so for each pair the Kendall tau matrix of that pair and the explicit controls is inverted and the pair's entry is reported as −Pᵢⱼ/√(PᵢᵢPⱼⱼ). With one control this is Kendall's first-order partial tau. Adding a target to the table does not change the other pairs' estimates.

Every coefficient is computed on the rows complete for **every** selected target and control (listwise deletion), so all pairs share one *n*, reported in the table caption. That is the one way the target selection still reaches the other pairs: adding a target that has missing values removes those rows from every pair as well. Adding a target with no missing values leaves the other coefficients untouched.

The correlation table also reports Benjamini-Hochberg adjusted p-values over its tested pairs. These tests do not adjust for spatial dependence. Significance uses the partial degrees of freedom *n* − 2 − *k* (Pearson and Spearman t-statistic) or a normal approximation with effective sample size *n* − *k* (Kendall). A variable listed both as a target and as a control is silently removed from the control set, since controlling for itself would yield null residuals. Detected residualization/inversion failures produce an explicit message. Degenerate residual columns can still yield NA coefficients; no raw-correlation substitute is used for a failed controlled calculation.

### 8.4 Spatial cross-correlogram

The cross-correlation of two variables **as a function of the distance separating sample points**. Both are standardized to zero mean and unit variance, and the empirical cross variogram &gamma;<sub>12</sub>(*h*) is computed over projected metric coordinates up to the same cutoff every other variogram in the application uses (half the bounding-box diagonal). The cutoff is divided into the number of bins set by **Distance Bins** (3 to 50, default 15; the default reproduces the variogram lag width). At least 15 distinct locations are required. The estimator is:

<div style="text-align:center;"><i>&gamma;<sub>12</sub>(h) = (1 / 2N(h)) &sum; [Z<sub>1</sub>(x<sub>i</sub>) − Z<sub>1</sub>(x<sub>i</sub> + h)][Z<sub>2</sub>(x<sub>i</sub>) − Z<sub>2</sub>(x<sub>i</sub> + h)]</i></div>

Under second-order stationarity the cross variogram and cross-covariance are related by &gamma;<sub>12</sub>(*h*) = C<sub>12</sub>(0) − [C<sub>12</sub>(*h*) + C<sub>21</sub>(*h*)]/2, so for standardized variables the plotted spatial cross-correlation is

<div style="text-align:center;"><i>&rho;<sub>12</sub>(h) = r<sub>12</sub> − &gamma;<sub>12</sub>(h)</i></div>

with *r*<sub>12</sub> the ordinary non-spatial correlation, drawn as a dashed reference line. See Goovaerts (1997) and Isaaks & Srivastava (1989).

**How to read it.** A curve starting near *r*<sub>12</sub> at short lags and decaying toward zero means the two variables co-vary *locally*, sharing spatial structure at the scale where the curve stays above zero, which is the co-regionalization Co-Kriging exploits. A flat curve at roughly *r*<sub>12</sub> across all lags means the association carries no distance structure; a flat curve near zero means the variables are spatially unrelated at the sampled scales. A zero crossing is an empirical sign change, not a fitted practical range; noise and negative cross-structure can also produce it.

**Caveats.**
* Only the **symmetric** part of the cross-covariance is estimable. Bins are omnidirectional, so the pairs (*i*,*j*) and (*j*,*i*) fall in the same bin: unlike a time series, an unordered point set has no leads and lags and there is no negative-lag half of the plot. Directional cross-structure is not resolved; Section 5.1 covers the univariate anisotropy diagnostic.
* Each point is a bin average whose reliability follows its pair count. Bins with fewer than 30 pairs are greyed out, the conventional minimum (Journel & Huijbregts 1978), and point size encodes the count.
* Sampling noise can push an individual bin marginally outside [−1, 1]; these are moment estimates, not constrained coefficients.
* With `Spearman` or `Kendall` selected, both variables are rank-transformed before standardization and the curve is a rank-based cross-correlogram, labelled as such. Kendall has no distance-binned analogue of its own.
* Coordinates are deduplicated at centimetre precision after projection, the same convention the interpolation point sets use, so co-located samples do not stack the shortest bin.

### 8.5 PCA contribution and quality of representation (cos<sup>2</sup>)

Both panels are built from the **variable coordinates** *c*<sub>jk</sub> = *v*<sub>jk</sub> &middot; *s*<sub>k</sub>, where *v*<sub>jk</sub> is the loading of variable *j* on component *k* and *s*<sub>k</sub> that component's standard deviation.

* **Contribution** is the share of a *component* attributable to a variable: *v*<sub>jk</sub><sup>2</sup> &times; 100, summing to 100% over the variables of a component, since eigenvectors are unit-length in both PCA modes. The dashed reference line marks the uniform expectation 100/*p*.
* **cos<sup>2</sup>** is the share of a *variable* captured by the selected components: &sum;<sub>k &isin; axes</sub> *c*<sub>jk</sub><sup>2</sup> &divide; &sum;<sub>k</sub> *c*<sub>jk</sub><sup>2</sup>. The denominator is the variable's total variance, since &sum;<sub>k</sub> *c*<sub>jk</sub><sup>2</sup> = (**V S**<sup>2</sup> **V**<sup>T</sup>)<sub>jj</sub> = Var(*x*<sub>j</sub>), so the value is bounded in [0, 1] and reaches 1 across all components. This is the standard definition (Abdi & Williams 2010; Lê et al. 2008).

**Why the denominator matters.** For a correlation-matrix PCA (**Scale & Center Data** checked) every variable has unit variance, the denominator is 1, and the normalisation changes nothing. For a covariance-matrix PCA it is essential: the unnormalised sum is an absolute variance in the variable's own squared units, so a variable in mg&nbsp;kg<sup>-1</sup> and one on a 0-1 scale would be plotted against each other on an axis labelled cos<sup>2</sup> where the taller bar means nothing but the larger unit. With the normalisation, cos<sup>2</sup> answers the same question in both modes.

**What scaling still changes.** Contribution remains scale-sensitive by definition, and in an unscaled run it ranks variables largely by raw variance; the module says so inline. cos<sup>2</sup> is per-variable and therefore comparable, but the *components* it is measured against are themselves dominated by the high-variance variables in an unscaled run, so a low cos<sup>2</sup> there means "this variable is not what these components describe", not "this variable is unstructured".

### 8.6 Group-comparison tests and post-hoc letters

The descriptive module annotates grouped plots with one significance test at a time (**None** by default): one-way ANOVA (reported as *F*, df and *p* rather than letters), Tukey's HSD (Tukey 1949), Duncan's multiple range test (Duncan 1955), or Kruskal-Wallis (Kruskal & Wallis 1952). The control is single-select because only one test is ever applied, and letters come from `agricolae`.

* **Tukey's HSD** is the family-wise option (the control defaults to None): it controls the **family-wise** error rate across all pairwise comparisons of *k* groups.
* **Duncan's MRT** is labelled *liberal*. It controls only the **comparison-wise** error rate, so with *k* groups its effective family-wise rate grows toward 1 and it declares more differences than Tukey on the same data. It is offered because a large body of older agronomy literature reports it and reproducing that work requires it, not as an equivalent alternative. Where a result turns on a difference Duncan finds and Tukey does not, report both.
* **Kruskal-Wallis** compares rank distributions and still assumes independent observations; a location/median interpretation requires similarly shaped group distributions. The normality indicator alone cannot select a valid test. Its pairwise post-hoc comparisons are **Benjamini-Hochberg** adjusted (Benjamini & Hochberg 1995), consistent with the FDR policy used in the correlation table.
* ANOVA/Tukey/Duncan require appropriate residual and variance assumptions. None of these tests adjusts for spatial dependence. Shared letters mean a difference was not detected, not that groups are equivalent.
* Letters are computed on the plotted subset (the current locality or scope selection and any facet), so they describe exactly the data shown.
* With only two groups, Tukey and Duncan report the one-way ANOVA *F* test instead of letters, since a single pairwise comparison needs no multiple-comparison procedure.

### 8.7 Missing values: complete cases or pairwise

Which samples a correlation is computed on is decided by what the result is used for, and the rule is the same throughout the app.

**The main multivariate analyses use complete cases**, the rows with no missing value in *any* selected variable, controls included. This covers the correlation heatmap, network and correlogram, partial correlations, PCA, the Mahalanobis screen, the governing-factors forest and the VIF stage of the collinearity gate. Pairwise deletion would estimate each cell from a different subsample, and a matrix assembled that way is not guaranteed positive semi-definite: partial correlation inverts it and can return |r| > 1 or fail to solve, PCA can return negative eigenvalues, `diag(solve(R))` can return negative VIFs, and the heatmap's clustering order is built on 1 − |r| as a dissimilarity. Every cell would also carry its own *n* and degrees of freedom, so no single sample size and no comparable set of p-values would exist for the panel. Each of these panels states the sample it used and how many rows it dropped.

**Standalone bivariate screens use pairwise deletion**, every sample where *both* variables are present, so a variable measured on part of the dataset is still screened on what it has. This covers the **Predictor correlations** screen in the sidebar's Auxiliary Variables panel, reached with its **Rank by correlation** button, where each rank carries its own *n* beside its coefficient, and the pairwise-threshold stage of the collinearity gate, where each pair is read against the threshold on its own and nothing inverts the matrix. The correlation screen requires at least three finite pairs. The collinearity screen instead checks total row count, not each pair's shared count; insufficient overlap can create NA correlations and make that screen fail.

**Auxiliary predictor correlation screen.** It sits in the Auxiliary Variables panel, so it is available wherever that panel is: RK, RFK, CK, and OK under the Comparable CV population. The target is the mapped measured column in Actual view, or the mapped uploaded CVE/SS column by default in prediction views (including CVE in Residuals view). A source switch allows measured values on the same scope. CVE uses all rows in the selected localities; SS applies the correlation panel's chosen partition to either source, initialized from Context's subset. These controls affect only this screen. Each coefficient uses finite paired observations and `stats::cor.test(method = "pearson")`, with raw two-sided p-values and its own n; constant pairs are unavailable. All candidates are ordered by |r| and can be filtered by category or raw p. The screened target, selected Actual variable and mapped coordinates are excluded from ranks. Other uploaded prediction columns remain eligible candidates. Raw p-values assume independent observations and are unadjusted for multiple screening or spatial dependence, so they support exploratory screening rather than confirmed spatial inference. Pearson measures linear association; RFK can also use nonlinear relationships.

Neither rule is a treatment for missing data. If complete-case deletion removes a large share of the samples, that is information about the dataset, not a display artifact: narrow the variable set, or impute deliberately before uploading.

### 8.8 PCA fitting, normality and Governing Factors

PCA uses `stats::prcomp` on complete rows, after excluding zero-variance columns (Section 8.1), always centred. **Scale & Center Data** additionally scales each column to unit sample SD when checked; unchecked means centred covariance PCA, not uncentred PCA. Scree/cumulative plots use squared component SDs divided by their sum. The 80% cumulative reference line is a display aid, not automatic component selection.

The normality indicator removes missing values, requires at least three observations and nonzero variance, then uses Shapiro-Wilk below n = 5000 and Lilliefors at or above 5000. At p ≥ 0.05 the UI labels the result normal, meaning only that this test did not reject normality. Tests are not adjusted for spatial dependence or repeated testing across variables/groups.

**Governing Factors** fits a separate regression random forest on rows complete for the target and selected predictors (minimum 10). It does not perform spatial CV or residual kriging. Permutation importance is the increase in RMSE on the DALEX evaluation sample (`DALEX::model_parts(type = "difference")`), while the reported OOB variance explained is the forest's own OOB diagnostic. The Governing Factors table reports measurement units explicitly for each row and formats all metrics to four significant digits (or exact integers for whole counts). Neither statistic establishes spatial transfer performance or causation.

The predictor with greatest mean permutation importance is selected for three profiles: partial dependence averages predictions while changing that predictor; accumulated local effects aggregate local model differences across its observed distribution; SHAP estimates feature contributions for a seeded sample of observations. The code uses DALEX's aggregated SHAP rows (`B == 0`) for the selected predictor, not the sum over individual permutations. These describe the fitted model; correlated predictors, extrapolated predictor combinations and finite Monte Carlo sampling affect their interpretation. The controls set importance permutations, forest tree count and the maximum number of explained observations (Section 9.6).

---

## 9. Hardcoded Parameters and Where to Change Them

Certain statistical constants are fixed to keep automated execution predictable. Advanced users can change them in the source.

**Random number generator.** Every seeded step in the application runs inside the two-sided sandbox `with_seed()` (`spatial_vgm.R`), which *names* the generator before it seeds - Mersenne-Twister uniforms, Inversion normals, Rejection sampling, R's own defaults - and restores the caller's random state afterwards. Naming it is what makes one seed mean one stream: background workers run under an L'Ecuyer-CMRG stream, `set.seed()` keeps whatever generator is in force, and an unnamed generator therefore gave the application one result and the same seeded call in a script or a test another. This governs every seeded quantity in Monolith: fold assignment, the spatial k-means blocks, each RFK fold's forest, the Jenks subsample, the Moran duplicate-coordinate jitter, classification tuning and fitting, and the governing-factors forest and SHAP sample. Parallel independence is untouched, because the restored state carries the generator with it and each worker resumes its own stream.

### 9.1 Cross-validation random seed

Interpolation fold assignment uses `CV_FOLD_SEED = 12345` for realization 1; later realizations increment it. Classification independently defaults to seed 12345 in `classif_helpers.R`. Section 5 covers how to read a single fold realization and what the optional Repeated CV table adds.

* **Where:** `make_cv_folds` in `spatial_metrics.R` is the single source of fold assignments for the six interpolation engines and the IDW power search (Section 4.2). For the kriging engines it is reached through `build_cv_plan`, which folds the cross-validation population once per locality and surface and hands the same plan to OK, CK, RK, RFK and the OK fallback, so a population's folds do not depend on which engine scores it. Each RFK fold's forest is drawn from `CV_FOLD_SEED + <fold label>`: a forest's number of random draws depends on the data it is grown on, so one stream across the whole loop let earlier folds decide where a later fold's forest started, and a held-out sample could move its own prediction. Every seeded step runs in the two-sided RNG sandbox `with_seed()` (`spatial_vgm.R`), which restores the caller's random state.
* **How:** change the `CV_FOLD_SEED` constant in `spatial_metrics.R` to score the run on a different partition.

### 9.2 Moran's I neighbour count (k)

The residual-autocorrelation test defines neighbours by a symmetric k-nearest-neighbour contiguity with `k = 8`, capped at n − 1 for small samples (Section 5 explains the choice).

* **Where:** `calc_moran` in `spatial_metrics.R`.
* **How:** adjust the `8L` in `k_nn <- min(8L, nrow(coords) - 1L)` if the range of residual spatial autocorrelation calls for a more local or a wider neighbourhood.

**Duplicate-coordinate handling.** The neighbour search cannot accept exactly co-located points, so duplicates are separated by a tiny seeded displacement before the graph is built, with the RNG sandboxed so the statistic stays bit-reproducible. The displacement is **scaled to the data**: 1e-9 of the coordinate span, floored at 1e-12 of the coordinate magnitude and at 1e-8 map units absolute. A fixed absolute displacement is unsafe at projected coordinate magnitudes: at a UTM northing of about 4.5 × 10⁶ the spacing between representable doubles is about 1 × 10⁻⁹, so a 1e-8 nudge is only some ten representable steps and can round straight back onto the original value, silently leaving duplicates. All three floors stay orders of magnitude below any real sample spacing, so the neighbour definition is unaffected.

### 9.3 Cross-validation strategy thresholds

The strategy itself (Auto / Standard LOOCV / Spatial Block CV) is selectable in the UI. Its thresholds are hardcoded in one place.

* **Where:** `resolve_cv_plan` in `spatial_metrics.R`.
* **How:** under **Auto** the engine uses LOOCV at 50 or fewer observations and random 10-fold above 50; edit the `n > 50` test to move that boundary. **Spatial Block** degrades to LOOCV below `CV_BLOCK_MIN_N` (default `30`). The fold and block count is the `k = 10L` value.

### 9.4 Map-styling class breaks (Jenks / k-means)

Class limits for the agronomical styling algorithms are computed by `calc_class_breaks` (`spatial_pipeline.R`) rather than a bare `classInt::classIntervals` call, for two reasons.

* **Reproducibility.** Both `classInt` styles draw random numbers internally (k-means starts, Hartigan & Wong 1979; Jenks silently switches to an unseeded 3,000-value sample above n = 3,000), so the same map could legitimately produce different class limits on every restyle. `calc_class_breaks` runs under the app's two-sided seed sandbox (seed `12345`, caller RNG restored), making the breaks bit-reproducible.
* **Tractability.** The exact Jenks algorithm (Jenks 1967) is O(n²) and blocks for seconds on raster-sized vectors, so breaks are estimated on a seeded subsample capped at `max_n = 5000` cells. Estimating breaks from a sample is standard GIS practice, and the class *areas* reported in the Scientific Analysis tab are always computed by classifying the **full-resolution** raster with those breaks. The interactive Leaflet viewer may display a mean-aggregated preview above about 500,000 cells; this is display-only.
* **Where:** the `max_n = 5000L` default of `calc_class_breaks`, and `LEAFLET_DISPLAY_MAX_CELLS <- 5e5` in `server_setup.R`.
* **Commit semantics.** Agronomical sub-settings (algorithm, class count, supervised limits) are staged in the sidebar and take effect when **Apply to maps and statistics** is pressed, so the break computation runs once per commit rather than on every input tick. The computation itself is unaffected by the staging.

### 9.5 Class zones as vector polygons

**Combined class areas.** Totals add unrounded per-locality areas only when the corresponding locality domains are disjoint. Excess overlap above 0.5 m² (a numerical-sliver tolerance) withholds the combined table and its export; per-locality areas remain available. Overlap is measured separately for actual and predicted surfaces, converted to square metres, and includes explicit named-boundary overlaps. With three identical domains, excess coverage is twice one domain's area.

The **Class zones** export writes the displayed classified surface as a GIS vector layer (`build_class_zone_sf`, `spatial_pipeline.R`). It is a change of representation, not of method, and is built from the same two calls the on-screen map and the Area Coverage table already make.

* **Geometry.** `terra::classify(rcl_mat, right = FALSE)`, the identical classification including the left-closed interval convention, followed by `terra::as.polygons(dissolve = TRUE)`. One feature per class present in the surface. Polygon boundaries are therefore **cell boundaries**: the zones inherit the cells of the displayed surface (the interpolation grid resampled into the Target Mapping CRS) and are not smoothed, contoured or generalised, because any of those would move the boundary away from where the model places it.
* **Area.** Taken from `terra::expanse(unit = "ha", byValue = TRUE)`, the function the Area Coverage table calls, rather than measured off the exported geometry, so the exported `area_ha` equals the reported hectares by construction. `expanse` measures on the ellipsoid rather than in grid units, so thirty nominal 100 × 100 m cells need not total exactly 30 ha; the discrepancy depends on position and projection.
* **Coordinate reference system.** Shapefile and GeoPackage keep the run's Target Mapping CRS, the CRS of the displayed surface the areas were computed from. GeoJSON and KML are reprojected to WGS84, which those formats require by specification. A GIS that re-measures the polygons planimetrically will differ from `area_ha` by the projection's own area distortion (inside a UTM zone from −0.08% on the central meridian to about +0.2% at the zone edge, §10.3), because `expanse` measures on the ellipsoid; the attribute, not the re-measurement, is the number the app reports.
* **Open ends.** The outer breaks are `-Inf` and `Inf` by construction. To avoid unsupported infinite attribute values in GIS formats, so the `class_min` of the first class and the `class_max` of the last are written as NA rather than as a fabricated finite limit.
* **KML attribute carriage.** For compatibility with the KML writer used by the app, the export places the class label in the placemark name and serialises the full attribute record (`class`, `class_min`, `class_max`, `area_ha`, `surface`, `variable`, `method`) into the description, so nothing is silently dropped. Use GeoPackage or Shapefile when the attributes must arrive as queryable fields.

### 9.5.1 GeoTIFF metadata tags and band statistics

Every continuous and categorical GeoTIFF exported by Monolith embeds internal metadata tags and precalculated raster band statistics directly into the file header:
* **Embedded metadata tags:** Standardized GDAL tags (`MONOLITH_RUN_ID`, `MONOLITH_VARIABLE`, `MONOLITH_METHOD`, `MONOLITH_LAYER`, `MONOLITH_INPUT_CRS`, and `MONOLITH_TARGET_CRS`) record provenance inside the raster.
* **Precomputed band statistics:** Real minimum, maximum, mean and standard deviation are written to the TIFF tags (`STATISTICS_MINIMUM`, etc.). Cell values remain unaltered at full floating-point precision (`FLT4S`) or 8-bit unsigned integer (`INT1U`), allowing external GIS software to display maps with immediate, accurate contrast stretching without recalculating statistics across the grid.

### 9.6 Reference of fixed constants

Selected fixed constants affecting results, grouped by stage; package-internal defaults are not exhaustively listed. Values exposed as sidebar or panel controls are listed only where the control's default or range matters to the method. Sections in brackets give the rationale.

**Point sets and variograms**

| Constant | Value | Where |
|---|---|---|
| Co-located sample removal | coordinates rounded to 2 decimals (1 cm in a metric CRS), first eligible sample kept; target-missing rows removed before per-surface deduplication | `dedup_valid_points`, `run_regional_interpolation` (`spatial_pipeline.R`) |
| Minimum points per locality surface | 3 | `run_regional_interpolation` |
| Minimum samples in a cross-validation population | 3; below it the surface is mapped and its CV skipped with a named log line [5] | `.engine_cv_plan` (`spatial_kriging.R`) |
| CV population ID | first 8 hexadecimal characters of the MD5 of `<target key>\|<row ids>\|<realization-1 fold ids>` [5] | `cv_population_id` (`global_utils.R`) |
| Empirical variogram cutoff and lag width | cutoff = half the bounding-box diagonal; width = cutoff / 15 | `calc_scientific_lags` (`spatial_vgm.R`) |
| Auto-fit candidates | Sph, Exp, Gau, Mat (&nu; = 1.5) × starting ranges max lag / 10, / 5, / 4, / 2 [4.1] | `robust_vgm_fit` (`spatial_vgm.R`) |
| Candidate validity (eligibility) | finite fit criterion; partial sill > 0; practical range finite and > 0; nugget &ge; 0; Gau and Mat nugget > 0 [4.1] | `robust_vgm_fit` |
| Range resolved (diagnostic, never a ranking) | practical range in (max lag / 100, 2 × max lag) [4.1] | `robust_vgm_fit` |
| Numerical tie on the fitting criterion | relative SSErr difference below 10⁻⁸; only then does a resolved range win [4.1] | `VGM_SSE_TIE_REL`, `.vgm_pick_best` (`spatial_vgm.R`) |
| Sill resolved (what may be claimed) | practical range &le; max lag; stricter than the range-resolved window, and separate from it [4.1] | `robust_vgm_fit`, `vgm_params_row` (`ui_formatting.R`) |
| Rising at the cutoff (trend advisory) | mean gamma of the last third of the bins exceeds the previous third by more than 5% of the gamma span; NA below 2 &times; that third [4.1] | `.vgm_still_rising` (`spatial_vgm.R`) |
| Degenerate target (parameters suppressed) | sd &le; 10⁻⁸ × max\|x\| [4.1] | `.is_degenerate_covariate` (`spatial_kriging.R`) |
| Practical-range factors | Sph 1, Exp 3, Gau &radic;3, Mat (&nu; = 1.5) 4.75 [4.1] | `.vgm_practical_range_factor` (`spatial_vgm.R`) |
| Heuristic fallback variogram | fewer than 5 lag bins, or no ELIGIBLE candidate: Sph, 80% partial sill; nugget-dominated (95% nugget, range max lag / 10) when the smallest semivariance exceeds 80% of the variance [4.1] | `robust_vgm_fit` |
| Starting nugget when the smallest semivariance is zero | max(variance × 10⁻⁶, 10⁻⁶) [1.1] | `robust_vgm_fit` |
| Manual Gau/Mat nugget advisory | nugget below 5% of the total sill [4.1] | `VGM_SMOOTH_NUGGET_WARN_SHARE` (`spatial_vgm.R`) |
| Manual variogram sliders | nugget and partial sill 0 to 2 × max(variance, stored total sill); range to max(1.5 × bounding-box diagonal, 3 × stored range); approximately 200 steps with rounded step sizes [4.1] | `manual_vgm_slider_spec` (`ui_formatting.R`) |

**Engines**

| Constant | Value | Where |
|---|---|---|
| CK LMC correction | `correct.diagonal = 1.01`; range seed cutoff / 2 when the primary fit is a fallback [1.4] | `apply_CK` (`spatial_kriging.R`) |
| CK neighbourhood | slider 5 to 60, default 15 [1.4] | `ck_nmax` (`ui_sidebar.R`) |
| IDW power search | 0.5 to 5.0 in steps of 0.5; p = 2 when the search fails or a locality has fewer than 5 points [4.2] | `optimize_idw_p` (`spatial_kriging.R`), `idw_opt_item` (`spatial_pipeline.R`) |
| IDW neighbours | slider 4 to 50, default 12 | `idw_nmax` (`ui_sidebar.R`) |
| Covariate IDW fallback | locality IDW power and Max Neighbors (defaults 2 and 12) [1.4] | `krige_covariates` (`spatial_kriging.R`) |
| TPS lambda optimizer | fewer than 5 points returns lambda = 0 without a GCV curve; coordinates divided by the larger extent, with no per-axis rescaling (`scale.type = "unscaled"`); near-planar warning at effective df < 3.5 [4.3] | `tps_gcv_item` (`spatial_pipeline.R`), `apply_TPS` |
| RFK trend forest | `ntree = 200`; `mtry`, `nodesize` at `randomForest` regression defaults [7.3] | `rfk_ntree_val` (`server_execution.R`) |
| Interpolated point-error surface | IDW, `idp = 2`, run's Max Neighbors | `run_regional_interpolation` |
| Collinearity gate | VIF > 10; pairwise \|r\| > 0.95 reported; constant when sd &le; 10⁻⁸ × max\|x\| [8.1] | `detect_multicollinearity_engine`, `.is_degenerate_covariate` (`spatial_kriging.R`); `check_vif(threshold = 10)` (`server_execution.R`) |

**Grid and boundary**

| Constant | Value | Where |
|---|---|---|
| Auto run grid | about 100,000 cells inside the boundary, clamped to [5, 1000] m; Auto (Global) uses the largest boundary's value for every locality, on a lattice snapped to multiples of it [2.3, 2.5] | `auto_grid_resolution`, `shared_auto_resolution`, `run_regional_interpolation` (`spatial_pipeline.R`) |
| Grid cells | square, exactly the resolution; grid extended by whole cells from the bounding box's lower-left corner [2.5] | `grid_template` (`spatial_pipeline.R`), also used by `classif_build_grid` |
| Run grid, every mode | at most about 4 × 10⁶ bounding-box cells; Fixed absolute floor 0.1 m [2.5] | `cap_res` in `run_regional_interpolation` |
| Resolution suggestion | 0.5 × mean nearest-neighbour distance; global value floored at extent / 300 and clamped to [1, 500] m; per locality clamped to [1, 5000] m [2.1] | resolution observer (`server_data_setup.R`) |
| Dynamic buffer | TPS 1.0, IDW 2.0, OK/CK/RK/RFK 3.0 × manual cell size in Fixed mode or half mean nearest-neighbour distance in Auto modes, clamped to [5, 2000] m [3.2] | `buffer_multipliers`, `get_buffer_multiplier` (`spatial_pipeline.R`) |
| Sidebar defaults | Buffer Distance 250 m; Manual Resolution 50 m (slider 1 to 500 m) | `ui_sidebar.R` |
| Run archive limit | 3 completed runs (`MAX_RUN_HISTORY = 3L`); oldest archived run evicted to bound session memory | `server_execution.R` |

**Cross-validation and metrics**

| Constant | Value | Where |
|---|---|---|
| Spatial Block k-means | 10 centres, `nstart = 5`, `iter.max = 50`; seeded random 10-fold when k-means fails [5] | `make_cv_folds` (`spatial_metrics.R`) |
| Repeated CV | UI offers 3, 5 or 10 realizations; engine cap 25 [5] | `cv_repeat_n` (`ui_sidebar.R`), `cv_repeat_count` |
| Moran's I all-pairs fallback | only at n &le; 500 [5] | `calc_moran` |
| Stored metric precision | full precision; displays format to 4 significant digits [5.2] | `perform_cv`, `augment_metrics`, `format_sig` |

**Target Mapping CRS**

| Constant | Value | Where |
|---|---|---|
| Suitability thresholds | \|k − 1\| > 0.1% warns, > 1% refuses; arc 0.001° [3.7] | `crs_target_suitability`, `crs_scale_factor` (`global_utils.R`) |
| Identification from lon/lat | agreement within 5 m; rivals at least 100 × worse [3.6] | `identify_crs_from_lonlat` |
| Identification from a boundary | winner holds &ge; 50% of points; runner-up at most half of that [3.6] | `identify_crs_from_boundary` |
| Degree-range identification | lon/lat column names, or a boundary holding &ge; 50% of the points read as degrees; otherwise EPSG:4326 is offered, not set [3.6] | `identify_input_crs` |
| Candidate shortlist | within 500 km of a clicked point; positions within 250 m folded; 8 rows; text search capped at 120 codes [3.6] | `crs_candidate_shortlist`, `crs_registry_filter_area` |

**Map styling**

| Constant | Value | Where |
|---|---|---|
| Binned styling | 5 equal-interval classes over the displayed values | `classification_params` (`server_run_config.R`) |
| Agronomical classes | slider 2 to 5, default 3 | `agro_n_classes` (`ui_sidebar.R`) |

**Exploratory suite and Governing Factors**

| Constant | Value | Where |
|---|---|---|
| PCA collinearity screen | pairwise \|r\| > 0.95; VIF > 10 [8.1] | `check_collinearity` (`ui_plotting.R`) |
| Mahalanobis screen | &chi;² 97.5% quantile; components with sd &le; 10⁻⁸ × max sd excluded [8.2] | `generate_pca_mahalanobis` (`ui_plotting.R`) |
| Spatial cross-correlogram | at least 15 locations; bins greyed below 30 pairs; Distance Bins 3 to 50, default 15 [8.4] | `compute_spatial_cross_correlogram`, `generate_spatial_cross_correlogram` (`ui_plotting.R`) |
| Partial correlation | at least 5 complete rows [8.3] | correlation table (`desc_exploratory_module.R`) |
| Normality test | Shapiro-Wilk below n = 5000, Lilliefors at or above; &alpha; = 0.05; no test below n = 3 | `compute_normality` (`desc_exploratory_module.R`) |
| Governing Factors | at least 10 complete rows; seed 12345; Permutations 10 to 100 (default 50), trees 50 to 500 (default 100), SHAP sample 50 to 1000 (default 100) | `compute_governing_factors` (`spatial_pipeline.R`), `gov_module.R` |

Governing Factors permutation importance is the mean RMSE after shuffling a predictor minus the unshuffled forest's RMSE on the same DALEX evaluation sample (up to 1,000 complete-case rows by default). `DALEX::model_parts(type = "difference")` applies this baseline subtraction for each permutation. Zero indicates no observed increase under the shuffles; a negative value means the shuffled predictor reduced RMSE. The table reports OOB variance explained separately as a model-quality diagnostic.

**Classification Suite**

| Constant | Value | Where |
|---|---|---|
| Folds and seed | up to 10 folds; Standard capped at the smallest class count with a minimum of 2; seed 12345 [10.2] | `classif_make_fold_id` (`classif_helpers.R`) |
| Nested CV | 5 inner folds [10.6] | `run_classification_cv(inner_v = 5)` |
| Tuning grids | Light 10 points, Full 30 points [10.6] | `.classif_tuning_registry` |
| Learner defaults | multinomial `penalty = 0`; RF 500 trees; XGBoost 500 trees, depth 6, learn rate 0.05, `min_n = 2`, `loss_reduction = 0`, `sample_size = 1` [10.6] | `.classif_method_defs` |
| Permutation importance | 5 shuffles per covariate [10.8] | `run_classification_pipeline(importance_reps = 5)` |
| Collinearity screen | VIF > 10; VIF > 5 with Random Forest [10.8] | `active_vif_threshold` (`classif_module.R`) |
| McNemar test | exact binomial below 25 discordant pairs [10.9] | `classif_covariate_lift` |
| Scope adequacy warning | fewer than 20 complete rows, or a class with fewer than 3 samples | `classif_scope_adequacy` |
| Auto grid | about 50,000 cells, clamped to [5, 1000] m before the guard targeting about 4 × 10⁶ bounding-box cells in either mode [10.5] | `classif_auto_res`, `.CLASSIF_MAX_CANDIDATE_CELLS` |
| Dynamic buffer | 2.0 × 0.5 × mean nearest-neighbour distance, clamped to [5, 2000] m [10.5] | `.classif_scope_hulls` |
| Covariate IDW fallback | p = 2, nmax = 12 [10.4] | `build_classification_grid_aux` (`classif_helpers.R`) |
| Class GeoTIFF download package | zip containing INT1U `.tif` (embedded viridis colour table), `.tif.aux.xml` (GDAL category names), and `ID,class` legend CSV [10.3] | `classif_module.R` |

---

## 10. Supervised Classification Suite

The Classification Suite is a separate modelling paradigm from the interpolation engines: instead of predicting a continuous surface and thresholding it into agronomic zones, it trains a **multiclass classifier** directly on a categorical target using co-sampled covariates, in the tradition of categorical digital soil mapping. The engine lives in `classif_helpers.R` and is decoupled from the Shiny UI.

### 10.1 Targets and learners

Binned target breaks use a fixed internal seed (12345), with the caller RNG restored. Jenks sampling above 3,000 finite values therefore produces repeatable class definitions. The Classification Suite refuses non-metre projected Target CRSs and suitability verdicts of "block" (tested directional distortion above 1%), without an override.

The target is either an existing categorical column (soil class, land-use label) or a continuous variable discretised into ordered classes with `classInt` break styles (quantile, equal-interval, or Jenks), using left-closed `[low, high)` intervals for consistency with the app's agronomic-class convention. The target is either an existing categorical column (soil class, land-use label) or a continuous variable discretised into ordered classes with `classInt` break styles (quantile, equal-interval, or Jenks). Intervals are left-closed `[low, high)` for consistency with the app's agronomic-class convention, with the exception of the uppermost class which is closed on both ends `[low, high]` (`include.lowest = TRUE`) so that the maximum observed value is enclosed; generated bin labels reflect this interval structure directly.

**Numeric columns are never auto-detected as categorical:** coarse-resolution environmental covariates such as climate-raster precipitation metrics legitimately carry very few distinct values, and treating an ordered quantity as nominal both dummy-encodes it in the recipe and switches its grid transfer from kriging to nearest-neighbour assignment. Numeric class codes should be recoded to text or run through the binned-target mode.

Three learners share a common tidymodels (`parsnip` / `workflows`) backbone:
* **Multinomial logistic regression** (`nnet`; Venables & Ripley 2002), the parametric baseline. For **two-class targets** the engine substitutes binomial logistic regression (`glm`): the multinomial model with K = 2 reduces exactly to it, and parsnip's `multinom_reg` / `nnet` wrapper produces malformed probability output in the binary case. The substitution is statistically equivalent at the default `penalty = 0`, and penalty tuning is skipped for binary targets.
* **Random Forest** (`ranger`, probability forest; Breiman 2001; Wright & Ziegler 2017), the de-facto standard in categorical DSM.
* **Extreme gradient boosting** (`xgboost`; Chen & Guestrin 2016).

All three share one preprocessing recipe, trained within each fit: novel-level absorption, median and mode imputation, dummy encoding of categorical covariates, zero-variance removal, and standardisation of numeric predictors. Scaling aids conditioning and preserves tree split ordering apart from numerical effects. CV/final fitting first remove rows missing any selected predictor or target, so imputation does not rescue those excluded training rows; it remains relevant to reconstructed or external prediction inputs.

### 10.2 Spatial cross-validation and pooled metrics

The default validation is **spatial blocked CV** (`spatialsample::spatial_clustering_cv`, k-means on the projected coordinates), for the same reason Spatial Block CV exists in Section 5: random folds place a test point's near neighbours in the training set and overstate skill under spatial autocorrelation (Roberts et al. 2017; Ploton et al. 2020; Mahoney et al. 2023). Standard class-stratified random k-fold remains available for in-domain estimates. Fold assignment uses the app-wide seed (`12345`) under the two-sided RNG sandbox.

Headline performance predictions are collected **out-of-fold and pooled**; tuning separately computes per-fold accuracy and kappa: each fold's model predicts hard classes *and* full class-probability vectors on its held-out points, and metrics are evaluated once on the pooled set. Pooling avoids the undefined per-fold macro-metrics that arise when a spatially contiguous fold contains a single class, and it lets every metric, probability metrics included, be evaluated once over the full class set.

Each assessment point receives covariates built from that fold's training rows: numeric variables use the same kriging/IDW builder as the map, and categorical variables take the nearest training point's class. The measured covariates at held-out points are excluded. Outer-fold scoring, out-of-fold permutation importance and every tuning resample use this map input path, including the inner resamples of nested tuning. The spatial 1-NN baseline still uses only the training coordinates and labels. The map's covariate surfaces can additionally use rows with a missing target, while CV folds contain only target-labelled complete cases. CV therefore estimates the map task from the fold's available training data whether or not **Predict maps** is selected; predicting at new sites with measured covariates is a different task.

The reported sample size and exported model bundle's `n_train` count the rows entering CV and the final fit. For covariate models, these have valid coordinates, target and every selected predictor. For a covariate-free spatial 1-NN run, valid coordinates and a present target suffice. In a successful run, each included row receives one out-of-fold prediction. Classification fit/prediction failures can abort the run; this path has no interpolation-style partial-fold coverage table.

Reported metrics follow `yardstick`: overall accuracy, Cohen's kappa (Cohen 1960), balanced accuracy, precision, recall, F1, ROC AUC, log-loss and Brier score (Brier 1950). For more than two classes, precision/recall/F1 are macro averages and AUC uses Hand-Till (2001); for two classes they use binary estimators with the first factor level as the event.

**Macro averages are taken over a fixed class universe.** Every class represented in the pooled out-of-fold truth contributes exactly once to macro precision, macro recall and macro F1, and the reported estimator states that denominator (*Macro average (K = 3)*). A class the model never predicts has an undefined 0/0 precision; `yardstick` removes it from the multiclass average with a warning, which would put the three figures on different denominators: a macro F1 could then exceed both of its own components, and a model that ignores a minority class entirely could outscore one that finds some of it. Monolith scores that class **0** instead, the `zero_division = 0` convention, which keeps one denominator and is the only reading consistent with reporting macro metrics so that minority classes are not masked. The condition is never silent: the per-class table marks the class "no predictions", the run summary names it, and the metrics CSV carries its reference and predicted counts. The same zero-division rule applies to the two-class binary estimator, so a run whose model predicts no events reports precision and F1 of 0 rather than `NA`. A class with **no reference samples** is a different situation and stays different: its recall is not evaluable, so it returns `NA` and the macro recall becomes `NA` rather than silently averaging a zero. That case cannot arise for a pooled run, because empty target levels are dropped before fitting (next paragraph). Balanced accuracy is the mean of sensitivity and specificity, macro-averaged one-versus-rest for multiclass targets, rather than multiclass macro recall alone (contrast the class-recall definition in Brodersen et al. 2010). Brier uses (1/(2n))ΣᵢΣₖ(yᵢₖ − pᵢₖ)², in [0,1]. See [balanced accuracy](https://yardstick.tidymodels.org/reference/bal_accuracy.html) and [Brier score](https://yardstick.tidymodels.org/reference/brier_class.html). The confusion matrix is accompanied by per-class **producer accuracy** (recall, the omission-error complement) and **user accuracy** (precision, the commission-error complement), the standard per-class report in soil and land-cover classification (Congalton 1991).

**Classes absent from a fold's training rows.** Spatial folds are not class-stratified, since the blocks are defined by geometry, so a spatially clustered class can fall entirely inside one held-out block; class-stratified random folds have the same hole for a singleton class, which necessarily lands in one fold. That fold's model is then fitted on data containing none of that class and can never predict it. The suite treats this as a property of the validation design rather than an error: the absent class receives probability **0** from that fold, as an output-alignment convention, not a demonstrated true posterior, the held-out samples of that class score as misses, and the run reports which classes were affected. The reported producer accuracy for such a class reflects the training-class gap and the pooled log-loss carries the corresponding penalty; overall accuracy and kappa are depressed only in proportion to the affected sample count. The remedies are the usual ones: fewer classes, quantile rather than equal-interval breaks (which distribute samples evenly and rarely strand a class), standard random k-fold when an in-domain estimate is wanted, or a wider spatial scope.

**Empty target levels are dropped.** A binned target carries one level per interval, and an equal-interval or Jenks break can enclose no samples at all; a level can also be emptied by discarding rows with missing covariate values. Such a level is not a class the model can learn, so it is removed before fitting. Retaining it would leave macro recall undefined (balanced accuracy silently `NA`), add an all-zero row and column to the confusion matrix, and describe the prediction surface with a class for which no probability layer exists.

### 10.3 Prediction uncertainty: normalised Shannon entropy

The classifier analogue of the kriging-variance map is the normalised Shannon entropy (Shannon 1948) of the predicted class probabilities at each grid cell:

<div style="text-align:center;"><i>H* = - &sum;<sub>k</sub> p<sub>k</sub> ln p<sub>k</sub> / ln K</i></div>

where *K* is the number of classes. *H\** = 0 means the model is certain of a single class, *H\** = 1 that the probabilities are uniform. Entropy is a property of the model's probability output, not a formal error model: read it as a relative "where is the classifier least decided?" surface.

**Colour scales are absolute by default.** Entropy and the per-class probability maps are drawn on the full [0, 1] range, so probability maps of different classes are comparable with each other and entropy is comparable between runs. The cost is that a weak model, whose probabilities stay near 1/*K* and whose entropy stays near 1, produces a map of one flat colour. Each map's subtitle states the observed range, and an opt-in stretch rescales the colours to that range. Read a near-uniform class map together with the skill metrics (§10.2) and the no-information baseline (§10.9): a single class covering the whole grid is what an unskilled classifier is supposed to produce once the smoothed covariate surface no longer crosses any decision boundary, and stretching the colour scale reveals the residual structure without making it meaningful.

**The class raster ships with its legend.** The class GeoTIFF stores integer class codes (INT1U) with the map's colour table embedded. GDAL keeps the category names in a `.tif.aux.xml` sidecar beside the file, not inside it, so a bare `.tif` opens as unlabelled codes. The download is therefore a zip of the GeoTIFF, its sidecar and a two-column legend CSV (`ID,class`) that names the codes without depending on the sidecar.

**Class areas are ellipsoidal.** The per-class hectare figures come from `terra::expanse(unit = "ha", byValue = TRUE)`, the same call the interpolation Area Coverage table (§9.5) and the class-zone GIS export make, so all three rest on one convention. `expanse` measures true ground area on the ellipsoid rather than in the projection plane, and the two differ by the CRS's area scale factor <i>k</i><sup>2</sup>: about -0.08% on a UTM central meridian and +0.19% at a zone edge. A planimetric re-measurement in a GIS will therefore not reproduce these numbers exactly; the reported figure is the ground area.

### 10.4 Covariate-surface approximations

The classifier is trained at sample locations, but map prediction requires covariate values at every grid cell. Monolith derives these from the samples themselves, not from exhaustive rasters:
* **Numeric covariates** are interpolated to the grid with the same ordinary-kriging covariate builder RK and RFK use (`krige_covariates`, with an IDW fallback at p = 2, nmax = 12).
* **Categorical covariates** cannot be kriged; each grid cell inherits the class of its nearest training point.

Both transfers are approximations whose smoothing and blocking error propagates into the classified map, the probability surfaces and the entropy surface. Where wall-to-wall covariate rasters exist (DEM derivatives, satellite bands), sampling them at grid nodes outside the app remains the more rigorous DSM design. Cross-validation rebuilds these transfers from each fold's training rows, so its metrics include their prediction error. A model applied to measured covariates at new sites answers a different prediction task.

### 10.5 Spatial scope and the prediction domain

A run can be restricted to a **spatial scope** before anything is fitted:
* **Localities**, any subset of the mapped locality or grouping column;
* **Polygons**, shapes drawn on the map or uploaded as a shapefile, either intersected with the selected localities or used alone.

Scoping is applied *before* target construction, so binned-target break points are derived from the scoped data only, and all folds, metrics and hyperparameter searches see exclusively in-scope points. Binned breaks are constructed once on the full scope, not within CV; metrics are conditional on these class definitions.

If intersecting polygon and point-derived boundaries fails or yields an empty geometry, the implementation uses the polygon union. In polygons-only mode overlapping polygons assign each point to the first intersecting polygon for area-specific evaluation.

The prediction domain is built from the same scope resolution as a union of **per-locality boundaries** of the scoped points, intersected with or replaced by the polygon union when a polygon scope is active. Per-locality boundaries rather than one hull around everything keep the classifier from predicting into unsampled corridors between localities, where the covariate-surface approximations of Section 10.4 would be pure extrapolation.

**The domain is always metric.** Every quantity in this section is in metres: the boundary buffers, the grid resolution, the variogram lags of the covariate surfaces (Section 10.4), and spatial-CV distances (Section 10.2). Hectare areas are separately evaluated by `terra::expanse`. A classification run transforms the scoped points into the Target Mapping CRS and works there. The Target Mapping CRS may legitimately be geographic; in that case scope resolution first projects all coordinate-valid input rows onto the WGS 84 UTM zone of their mean position, then applies locality/polygon selection before any of that is computed, with the same zone rule the interpolation pipeline applies to geographic input (Section 3.6), and maps, rasters and exports are produced in that projection. Without it a resolution in metres would be applied to an extent in degrees and the grid would degenerate to a single cell covering the whole bounding box.

Boundary construction follows the **same geometric conventions as the interpolation engines** (Section 3) but is configured by the suite's own Spatial Scope controls (Boundary Type, Buffer Logic, Grid Resolution), independent of the interpolation sidebar, which is hidden while the suite is active. The settings apply per locality in scope. That panel keeps the longer names for the same two geometries the interpolation sidebar calls Buffered and Point buffer: **Wrapped (Buffered)** buffers each locality's concave hull, with the fixed distance or dynamically from that locality's mean nearest-neighbour spacing (× 0.5 × the generic 2.0 multiplier, clamped to [5, 2000] m; the method-specific multipliers of Section 3.2 are interpolation-domain heuristics and do not apply to a classifier). **Strict Measured (Point Buffer)** unions fixed-radius buffers around the sample points. Grid resolution is either **Fixed** or **Auto**, the latter targeting about 50,000 cells inside the domain (clamped to [5, 1000] m). Covariate kriging fills the whole buffered domain, so buffered boundaries entail covariate extrapolation beyond the outermost samples, whose reliability depends on the buffer width and data, the same trade-off as the interpolation engines' buffered modes.

**Both resolution modes impose a cell-size floor targeting at most about 4 M candidate cells.** The raster template spans the full bounding box before the boundary clip, and a multi-part scope covers far more of its box than its hulls do, so the cell count that has to be built is set by the box, not by the domain. Either mode is coarsened to &radic;( &Delta;<i>x</i> &Delta;<i>y</i> / (4 &times; 10<sup>6</sup>) ) when the requested cell size would exceed that budget, and a run that is coarsened says so. The domain itself is unchanged; only the cell size is. The grid is then built block-wise: cell centres are tested against the boundary a block at a time and the surviving centres are assembled once, so the entire bounding box is not materialized as an sf point layer at once.

### 10.6 Hyperparameter tuning

The **Hyperparameter tuning** selector sets how hard Monolith searches each learner's parameter space before the metrics of Section 10.2 are computed.

* **None (fixed defaults),** the default. Nothing is tuned; each learner is fit at hardcoded defaults (multinomial `penalty = 0`; Random Forest 500 trees with `ranger`'s built-in `mtry` and `min_n`; XGBoost 500 trees, `tree_depth = 6`, `learn_rate = 0.05`, `min_n = 2`). Fast, deterministic, and free of the optimism below.
* **Light,** a 10-point space-filling grid over a small set of high-leverage parameters: `penalty` (multinomial), `mtry` + `min_n` (Random Forest), `tree_depth` + `learn_rate` (XGBoost).
* **Full,** a 30-point grid over a wider set: the same parameters for multinomial and Random Forest, and for XGBoost additionally `mtry`, `min_n`, `loss_reduction` and `sample_size`.

At Light or Full the grid is by default evaluated over the *same* cross-validation folds and the best combination by accuracy is refitted. Because tuning and evaluation share those folds, tuned-depth metrics have selection optimism of unknown magnitude.

**The tuning search always uses the run's own resampling strategy.** Both the cross-validation loop and the final full-data fit build their tuning folds with the strategy selected for the run: spatial clustering under Spatial CV, class-stratified random folds under Standard CV. This matters because the final model's hyperparameters are what the class map, the entropy surface, the importance ranking and the exported `.rds` bundle are all built from. Tuning under random folds while reporting spatial-CV metrics would select the model that performs best under exactly the near-neighbour leakage spatial CV exists to remove, and every downstream product would inherit that choice while the metrics table advertised a leakage-free estimate. Runs at depth None are unaffected.

**Nested cross-validation** (the *Use nested CV (slower)* checkbox, visible only when a tuning depth is active) separates hyperparameter selection from outer-fold scoring. Instead of one global selection, each **outer** fold re-runs the full grid search on **5 inner folds** built exclusively from that fold's analysis rows, so inner fold construction, scoring and winner selection use the outer training rows. One candidate grid is shared across outer folds and its data-dependent parameter ranges are finalized against the full predictor frame, and the finalized workflow then predicts the held-out fold. The pooled metrics evaluate the search conditional on the full-scope class definitions and selected predictors (Sections 8.1 and 10.5), with the tuning-weight limitation in Section 10.11. Nested evaluation is the usual approach to assessing model selection (Varma & Simon 2006; Cawley & Talbot 2010). Inner folds reuse the outer resampling strategy, so the inner selection faces the same leakage regime as the outer estimate, the spatial analogue of the recommendation in Schratz et al. (2019). Consequences:

* No single "best" hyperparameter set exists under nesting. The per-outer-fold winners are collected (`nested_params`, one row per fold) and their fold-to-fold agreement is itself a stability diagnostic.
* The **deployed and exported model** is still tuned once on all training data, which is standard practice: nested CV estimates the procedure's skill, the final model uses everything. The bundle records that model's own hyperparameters.
* Cost multiplies by roughly the number of inner folds. Depth None is unaffected by the checkbox.

The depth, parameter and grid-size mapping lives in `.classif_tuning_registry()` in `classif_helpers.R` and is the single place to change tuning behaviour. Changes flow automatically into both the UI selector and the fitting path.

### 10.7 Per-area performance

When the scope contains more than one locality (or polygon, in polygons-only mode), the **Performance by Area** table splits the pooled out-of-fold predictions by area and reports n, overall accuracy, Cohen's kappa, balanced accuracy and macro F1 per area, plus a Total row reproducing the pooled headline metrics. F1 uses the binary estimator for two-class targets, as in Section 10.2. Two caveats apply:
* The fold structure is global. Spatial clusters need not respect locality borders, so a per-area subset is an *evaluation* slice of one jointly trained model, not an independently validated per-area model, unlike the interpolation engines which fit one model per locality.
* Small areas yield unstable estimates, and any metric undefined for an area (a class that never occurs there, for example) is reported as NA rather than imputed. Probability metrics are omitted from the per-area table for the same reason.

An area's macro F1 is averaged over the classes with **reference support in that area**: a class the pooled target holds but this area does not is not evaluable here and is excluded, while a class the area does hold and the model never predicts there still scores 0, by the fixed-universe rule of Section 10.2. Two areas' macro figures are therefore not necessarily over the same class set, so the table reports that denominator per row as **Classes (K)** (empty for a two-class run, which reports the event class's own F1 rather than an average).

### 10.8 Permutation feature importance

Every run reports **model-agnostic permutation importance** (Breiman 2001; Fisher, Rudin & Dominici 2019): one covariate at a time is randomly shuffled, severing its association with the target while preserving its marginal distribution, the model appropriate to the evaluation design re-predicts, and the increase in **multiclass log-loss** over the unpermuted baseline is recorded (mean of 5 shuffles under the fixed seed). Log-loss is used rather than accuracy because it consumes the full probability vector, so it registers importance even when a permutation rarely flips the arg-max class. Because the measure is computed on the raw covariates through the entire fitted workflow, it is directly comparable across all three learners, unlike engine-native measures (ranger impurity, xgboost gain, multinomial coefficients), which live on different scales.

**Evaluation design (selectable).** The permutation can be scored on either of two row sets, chosen with *Feature importance scored on*:

* **Out-of-fold (default).** Each cross-validation fold's own fitted model permutes the predictors of the rows that fold never saw, and the per-fold log-loss increases are pooled by fold size, which reproduces exactly the increase one evaluation over the whole out-of-fold set would give. This measures importance under the same honest design as the reported accuracy, and it costs no extra model fits, since the fold models already exist inside the CV loop and the number of rows predicted is the same as in the training-row design. Caveat: a predictor is shuffled *within* each assessment set, so with very small folds the shuffle decorrelates it less thoroughly and importance is slightly understated.
* **Training rows.** The final model scored on the rows it was fitted on, the conventional default (compare `vip::vi_permute`). Rankings are informative, but absolute magnitudes lean optimistic for flexible learners that partially memorise their training set.

**Only the covariates a model used are scored.** Under **Auto-Drop** the collinearity screen is part of each model (Section 8.1), so a covariate a fold's screen removed is not in that fold's model: permuting it would record a zero for a covariate that model never saw. Each fold therefore permutes only the covariates it kept, the pooled value of a covariate is the fold-size-weighted mean over the folds that kept it, and a covariate no fold kept is absent from the table rather than listed as unimportant. The training-row design likewise permutes the covariates the final model kept. Without Auto-Drop every model holds every covariate and the pooled value is the plain fold-size-weighted mean.

Both designs report the same quantity on different rows, and the design in force is written onto the plot axis, into the exported metrics CSV and into the results table, because an out-of-fold and a training-row importance are **not comparable numbers**. Negative values mean shuffling improved the evaluated score; they can reflect noise, dependence or model misspecification and do not prove a predictor is pure noise.

**The choice of design cannot move the reported performance metrics.** Each fold's importance shuffles run inside their **own** seeded sandbox derived from the run seed and the fold index, so the cross-validation loop consumes exactly the same random stream whether or not importance is requested: predictions, accuracy, kappa, the confusion matrix and the per-class table are identical under both settings, and each fold's importance is independently reproducible. Without that isolation the shuffles would advance the shared stream and change the random state every later fold's model was fitted from. That is invisible for learners that fit deterministically (the multinomial model starts its weights at zero, Random Forest pins its engine seed, and XGBoost does not subsample at depths None or Light) but real at depth **Full**, where XGBoost tunes `sample_size` and `mtry` and therefore draws random row and column subsamples. A diagnostic setting must never move a performance number, so the isolation is structural rather than dependent on which learners happen to be stochastic.

**Correlated covariates can distort importance** through substitution and unrealistic predictor combinations after shuffling (Strobl et al. 2008), which is why the multicollinearity gate of Section 8.1 runs first: on a VIF-cleaned covariate set the reported shares are far more interpretable. The displayed *share (%)* renormalises the positive importances to 100. Because this dilution sets in well below the classical VIF > 10 cutoff, the classification module's screen is **method-aware**: with Random Forest selected the advisory threshold tightens to **VIF > 5**, the conventional moderate-collinearity bound (James et al. 2013; O'Brien 2007 discusses why such rules are advisory rather than absolute). The gate remains a user decision, since predictions are largely insensitive to keeping the covariates and only the importance interpretation suffers.

### 10.9 No-covariate baselines and covariate lift

The suite compares covariate-model accuracy with a fold-evaluated spatial baseline and a descriptive no-information rate:
* **Majority-class rate:** the largest class proportion in the complete-case evaluation population. It is computed from all its labels, not fold-trained majority predictions. A constant prediction of that class would have kappa zero when at least two classes exist, but its kappa is not separately computed or exported.
* **Spatial 1-NN baseline,** each held-out point receives the class of its nearest analysis-set point (Euclidean distance, projected coordinates): the categorical analogue of nearest-neighbour interpolation, that is, what pure spatial proximity achieves with no covariate information. Under spatial blocked CV this is a demanding, honest baseline, since it must transfer across fold boundaries exactly like the model.

**Covariate lift** is the accuracy difference between the covariate model and the spatial baseline, reported in accuracy points on identical folds. Significance of the paired improvement is assessed with **McNemar's test** on the discordant pairs, the standard test for comparing two classifiers evaluated on the same samples (McNemar 1947; Dietterich 1998). Below 25 discordant pairs the exact binomial form is used, at 25 or more the continuity-corrected chi-square approximation; the discordant total is often small here, and the approximation is unreliable in that regime. Non-significance does not establish equivalence or absence of benefit. McNemar assumes independent paired outcomes; spatial dependence and overlapping CV training sets are not adjusted here, so its p-value is exploratory. The test is reported as NA when no discordant pairs exist.

McNemar's test is two-sided, so the reading takes its **direction from the lift and its significance from p**: p ≥ 0.05 means no demonstrable difference; p < 0.05 with a positive lift means the covariate model is more accurate; p < 0.05 with a negative lift means it is significantly *less* accurate than spatial proximity alone. In that last case the panel points to the no-covariate alternative that fits the target: for a **binned continuous** target, interpolating the continuous variable (Spatial Engine) and classifying the surface uses the full continuous information and the ordering of the classes; for a **categorical** target, whose codes cannot be interpolated, it points to the Spatial 1-NN map below. Under random k-fold CV the note adds that the 1-NN baseline is favoured, since each held-out point keeps its near neighbours in the training set (Roberts et al. 2017), so the comparison should be repeated under spatial blocked CV.

**Spatial 1-NN map (categorical targets).** The baseline is also available as a surface: every grid cell takes the class of its nearest sample, which is a Thiessen (Voronoi) mosaic of the samples' classes. It is built on the same prediction grid as the covariate model and from the same complete-case point set the baseline was scored on, so the *Spatial baseline (1-NN)* accuracy in the lift table is the cross-validated accuracy of this map. Class boundaries therefore run along cellwise approximations to the bisectors between samples of different classes and carry no information about where the class actually changes. Nearest-neighbour assignment gives hard classes only: there are no class probabilities, hence no entropy map, no probability map and no confidence threshold. The class-area table and the class GeoTIFF follow the map selected. Indicator kriging, which would give probabilities without covariates, is not implemented.

**Covariate-free runs (categorical targets).** With no covariate selected, the spatial 1-NN classifier is evaluated as the model itself: folds are built exactly as for a covariate run (same seed and strategy, on the points carrying a target value), each held-out point takes the class of its nearest training point, and the pooled out-of-fold predictions are scored with the class metrics of Section 10.2. Probability metrics are not reported because nearest-neighbour assignment produces none. On data without missing covariates these predictions are identical to the spatial baseline of a covariate run, so the two runs are directly comparable. The comparison shown is against the majority-class rate; no lift is computed. The run is restricted to categorical targets: for a binned continuous target, interpolating the continuous variable before binning uses information that nearest-neighbour assignment of bin labels discards.

### 10.10 Confidence thresholding (abstention)

The predicted-class map supports a **confidence threshold** implementing the classical reject option (Chow 1970): grid cells whose maximum class probability falls below the threshold are assigned an explicit **Unclassified** category, rendered grey with its own row in the area table, instead of a weak arg-max guess. A 34/33/33% cell is a statement of ignorance, not a prediction, and abstained regions are natural candidates for additional field sampling.

The threshold is applied at rasterisation time in the main session: it re-classifies the existing probability surface without refitting anything, so it can be explored interactively, and the exported class GeoTIFF honours the current setting (probability and entropy rasters are never masked). Thresholds at or below 1/K cannot fire, since the maximum of a K-class probability vector is at least 1/K. Alongside the map, a CV-based readout reports the **coverage and selective-accuracy trade-off** at the chosen threshold: the fraction of pooled out-of-fold points that would be retained and the accuracy among them. CV metrics themselves are always computed on all points, without abstention.

### 10.11 Class-imbalance weighting

The optional **Balance classes** switch applies inverse-frequency case weights, *w<sub>c</sub> = n / (K · n<sub>c</sub>)*, so each class contributes equally to the training loss regardless of its sample count (King & Zeng 2001; the `class_weight="balanced"` convention), countering the tendency of accuracy-driven learners to ignore rare but important classes. Design decisions:
* Weights affect **fitting only**; all reported metrics remain unweighted, since balanced accuracy and the macro averages already expose minority-class performance. Expect weighted runs to trade some overall accuracy for better minority-class recall.
* Weights are **recomputed from the analysis rows of every fit**: each outer fold, each tuning resample (non-nested, final-model and nested inner tuning alike) and the final model. No assessment row's label enters the class frequencies a fit is weighted by.
* Weights apply to Random Forest and XGBoost. Monolith's multinomial workflow does not expose case weights, and the run badge then reports "weights unsupported (unweighted)" rather than silently pretending.
* **SMOTE-style synthetic oversampling is deliberately not offered:** Monolith implements no spatially justified synthetic-sampling procedure. Any resampling must remain inside training folds and preserve the intended prediction task. Weighting does not replace representative observations or establish calibrated class probabilities.

### 10.12 Trained-model export

**Download Model (.rds)** saves the final fitted tidymodels workflow with its training metadata (method, covariates, class levels, weight usage, tuning depth and selected hyperparameters, projected CRS, n, timestamp, and the R, parsnip, workflows, recipes and engine package versions). The bundle loads in any R session with compatible package versions and applies to new covariate data without retraining:

```r
b <- readRDS("classification_model_rf_20260710.rds")
predict(b$workflow, new_data, type = "prob")   # or type = "class"
```

`new_data` must contain the covariate columns listed in `b$predictors`; the preprocessing recipe (imputation, dummy encoding, normalisation) is embedded in the workflow and replays automatically. Predictions match the in-app workflow when given the same reconstructed covariate inputs. The workflow does not rebuild spatial covariate surfaces, rasterize predictions or apply map confidence thresholds. Note that .rds serialisation is version-sensitive for the xgboost engine: reuse the bundle under the package versions recorded in `b$versions`.

---

## 11. References

Every work below is cited somewhere in this guide, and every citation in the text resolves here. Each entry carries a DOI, or an ISBN or proceedings reference where no DOI exists.

Ababou, R., Bagtzoglou, A. C., & Wood, E. F. (1994). On the condition number of covariance matrices in kriging, estimation, and simulation of random fields. *Mathematical Geology*, 26(1), 99-133. https://doi.org/10.1007/BF02065878

Abdi, H., & Williams, L. J. (2010). Principal component analysis. *WIREs Computational Statistics*, 2(4), 433-459. https://doi.org/10.1002/wics.101

Bellon-Maurel, V., Fernandez-Ahumada, E., Palagos, B., Roger, J.-M., & McBratney, A. (2010). Critical review of chemometric indicators commonly used for assessing the quality of the prediction of soil attributes by NIR spectroscopy. *TrAC Trends in Analytical Chemistry*, 29(9), 1073-1081. https://doi.org/10.1016/j.trac.2010.05.006

Benjamini, Y., & Hochberg, Y. (1995). Controlling the false discovery rate: a practical and powerful approach to multiple testing. *Journal of the Royal Statistical Society, Series B*, 57(1), 289-300. https://doi.org/10.1111/j.2517-6161.1995.tb02031.x

Bivand, R. S., & Wong, D. W. S. (2018). Comparing implementations of global and local indicators of spatial association. *TEST*, 27(3), 716-748. https://doi.org/10.1007/s11749-018-0599-x

Breiman, L. (2001). Random forests. *Machine Learning*, 45(1), 5-32. https://doi.org/10.1023/A:1010933404324

Brier, G. W. (1950). Verification of forecasts expressed in terms of probability. *Monthly Weather Review*, 78(1), 1-3. https://doi.org/10.1175/1520-0493(1950)078%3C0001:VOFEIT%3E2.0.CO;2

Brodersen, K. H., Ong, C. S., Stephan, K. E., & Buhmann, J. M. (2010). The balanced accuracy and its posterior distribution. *Proceedings of the 20th International Conference on Pattern Recognition*, 3121-3124. https://doi.org/10.1109/ICPR.2010.764

Cawley, G. C., & Talbot, N. L. C. (2010). On over-fitting in model selection and subsequent selection bias in performance evaluation. *Journal of Machine Learning Research*, 11, 2079-2107.

Chang, C.-W., Laird, D. A., Mausbach, M. J., & Hurburgh, C. R. (2001). Near-infrared reflectance spectroscopy: principal components regression analyses of soil properties. *Soil Science Society of America Journal*, 65(2), 480-490. https://doi.org/10.2136/sssaj2001.652480x

Chen, T., & Guestrin, C. (2016). XGBoost: a scalable tree boosting system. *Proceedings of the 22nd ACM SIGKDD International Conference on Knowledge Discovery and Data Mining*, 785-794. https://doi.org/10.1145/2939672.2939785

Chow, C. K. (1970). On optimum recognition error and reject tradeoff. *IEEE Transactions on Information Theory*, 16(1), 41-46. https://doi.org/10.1109/TIT.1970.1054406

Cohen, J. (1960). A coefficient of agreement for nominal scales. *Educational and Psychological Measurement*, 20(1), 37-46. https://doi.org/10.1177/001316446002000104

Cohen, J. (1968). Weighted kappa: nominal scale agreement with provision for scaled disagreement or partial credit. *Psychological Bulletin*, 70(4), 213-220. https://doi.org/10.1037/h0026256

Congalton, R. G. (1991). A review of assessing the accuracy of classifications of remotely sensed data. *Remote Sensing of Environment*, 37(1), 35-46. https://doi.org/10.1016/0034-4257(91)90048-B

Craven, P., & Wahba, G. (1978). Smoothing noisy data with spline functions: estimating the correct degree of smoothing by the method of generalized cross-validation. *Numerische Mathematik*, 31(4), 377-403. https://doi.org/10.1007/BF01404567

Cressie, N. (1985). Fitting variogram models by weighted least squares. *Journal of the International Association for Mathematical Geology*, 17(5), 563-586. https://doi.org/10.1007/BF01032109

Deutsch, C. V. (1996). Correcting for negative weights in ordinary kriging. *Computers & Geosciences*, 22(7), 765-773. https://doi.org/10.1016/0098-3004(96)00005-2

Dietterich, T. G. (1998). Approximate statistical tests for comparing supervised classification learning algorithms. *Neural Computation*, 10(7), 1895-1923. https://doi.org/10.1162/089976698300017197

Duchon, J. (1977). Splines minimizing rotation-invariant semi-norms in Sobolev spaces. In W. Schempp & K. Zeller (Eds.), *Constructive Theory of Functions of Several Variables* (Lecture Notes in Mathematics 571, pp. 85-100). Springer. https://doi.org/10.1007/BFb0086566

Duncan, D. B. (1955). Multiple range and multiple F tests. *Biometrics*, 11(1), 1-42. https://doi.org/10.2307/3001478

Fisher, A., Rudin, C., & Dominici, F. (2019). All models are wrong, but many are useful: learning a variable's importance by studying an entire class of prediction models simultaneously. *Journal of Machine Learning Research*, 20(177), 1-81.

Goovaerts, P. (1997). *Geostatistics for Natural Resources Evaluation*. Oxford University Press, New York. https://doi.org/10.1093/oso/9780195115383.001.0001

Hand, D. J., & Till, R. J. (2001). A simple generalisation of the area under the ROC curve for multiple class classification problems. *Machine Learning*, 45(2), 171-186. https://doi.org/10.1023/A:1010920819831

Hartigan, J. A., & Wong, M. A. (1979). Algorithm AS 136: a k-means clustering algorithm. *Journal of the Royal Statistical Society, Series C (Applied Statistics)*, 28(1), 100-108. https://doi.org/10.2307/2346830

Hengl, T., Heuvelink, G. B. M., & Rossiter, D. G. (2007). About regression-kriging: from equations to case studies. *Computers & Geosciences*, 33(10), 1301-1315. https://doi.org/10.1016/j.cageo.2007.05.001

Hengl, T., Heuvelink, G. B. M., Kempen, B., Leenaars, J. G. B., Walsh, M. G., Shepherd, K. D., Sila, A., MacMillan, R. A., Mendes de Jesus, J., Tamene, L., & Tondoh, J. E. (2015). Mapping soil properties of Africa at 250 m resolution: random forests significantly improve current predictions. *PLoS ONE*, 10(6), e0125814. https://doi.org/10.1371/journal.pone.0125814

Isaaks, E. H., & Srivastava, R. M. (1989). *An Introduction to Applied Geostatistics*. Oxford University Press, New York. ISBN 978-0-19-505013-4.

Hastie, T., Tibshirani, R., & Friedman, J. (2009). *The Elements of Statistical Learning: Data Mining, Inference, and Prediction* (2nd ed.). Springer, New York. https://doi.org/10.1007/978-0-387-84858-7

James, G., Witten, D., Hastie, T., & Tibshirani, R. (2013). *An Introduction to Statistical Learning*. Springer, New York. https://doi.org/10.1007/978-1-4614-7138-7

Jenks, G. F. (1967). The data model concept in statistical mapping. *International Yearbook of Cartography*, 7, 186-190.

Journel, A. G., & Huijbregts, C. J. (1978). *Mining Geostatistics*. Academic Press, London. ISBN 978-0-12-391050-1.

Kim, S. (2015). ppcor: an R package for a fast calculation to semi-partial correlation coefficients. *Communications for Statistical Applications and Methods*, 22(6), 665-674. https://doi.org/10.5351/CSAM.2015.22.6.665

King, G., & Zeng, L. (2001). Logistic regression in rare events data. *Political Analysis*, 9(2), 137-163. https://doi.org/10.1093/oxfordjournals.pan.a004868

Kohavi, R. (1995). A study of cross-validation and bootstrap for accuracy estimation and model selection. *Proceedings of the 14th International Joint Conference on Artificial Intelligence (IJCAI'95)*, Vol. 2, 1137-1143.

Kruskal, W. H., & Wallis, W. A. (1952). Use of ranks in one-criterion variance analysis. *Journal of the American Statistical Association*, 47(260), 583-621. https://doi.org/10.1080/01621459.1952.10483441

Lê, S., Josse, J., & Husson, F. (2008). FactoMineR: an R package for multivariate analysis. *Journal of Statistical Software*, 25(1), 1-18. https://doi.org/10.18637/jss.v025.i01

Liaw, A., & Wiener, M. (2002). Classification and regression by randomForest. *R News*, 2(3), 18-22.

Lin, L. I.-K. (1989). A concordance correlation coefficient to evaluate reproducibility. *Biometrics*, 45(1), 255-268. https://doi.org/10.2307/2532051

Mahalanobis, P. C. (1936). On the generalised distance in statistics. *Proceedings of the National Institute of Sciences of India*, 2(1), 49-55. (Reprinted in *Sankhya A*, 80(S1), 1-7. https://doi.org/10.1007/s13171-019-00164-5)

Mahoney, M. J., Johnson, L. K., Silge, J., Frick, H., Kuhn, M., & Beier, C. M. (2023). Assessing the performance of spatial cross-validation approaches for models of spatially structured data. *arXiv*:2303.07334. https://doi.org/10.48550/arXiv.2303.07334

Makridakis, S. (1993). Accuracy measures: theoretical and practical concerns. *International Journal of Forecasting*, 9(4), 527-529. https://doi.org/10.1016/0169-2070(93)90079-3

Matheron, G. (1963). Principles of geostatistics. *Economic Geology*, 58(8), 1246-1266. https://doi.org/10.2113/gsecongeo.58.8.1246

Matthews, B. W. (1975). Comparison of the predicted and observed secondary structure of T4 phage lysozyme. *Biochimica et Biophysica Acta (BBA) - Protein Structure*, 405(2), 442-451. https://doi.org/10.1016/0005-2795(75)90109-9

McNemar, Q. (1947). Note on the sampling error of the difference between correlated proportions or percentages. *Psychometrika*, 12(2), 153-157. https://doi.org/10.1007/BF02295996

Minasny, B., & McBratney, A. B. (2005). The Matérn function as a general model for soil variograms. *Geoderma*, 128(3-4), 192-207. https://doi.org/10.1016/j.geoderma.2005.04.003

Molinaro, A. M., Simon, R., & Pfeiffer, R. M. (2005). Prediction error estimation: a comparison of resampling methods. *Bioinformatics*, 21(15), 3301-3307. https://doi.org/10.1093/bioinformatics/bti499

Moran, P. A. P. (1950). Notes on continuous stochastic phenomena. *Biometrika*, 37(1/2), 17-23. https://doi.org/10.2307/2332142

Nash, J. E., & Sutcliffe, J. V. (1970). River flow forecasting through conceptual models part I: a discussion of principles. *Journal of Hydrology*, 10(3), 282-290. https://doi.org/10.1016/0022-1694(70)90255-6

Nychka, D., Furrer, R., Paige, J., & Sain, S. (2021). *fields: Tools for Spatial Data*. R package, University Corporation for Atmospheric Research, Boulder, CO. https://doi.org/10.5065/D6W957CT

O'Brien, R. M. (2007). A caution regarding rules of thumb for variance inflation factors. *Quality & Quantity*, 41(5), 673-690. https://doi.org/10.1007/s11135-006-9018-6

Pebesma, E. J. (2004). Multivariable geostatistics in S: the gstat package. *Computers & Geosciences*, 30(7), 683-691. https://doi.org/10.1016/j.cageo.2004.03.012

Ploton, P., Mortier, F., Réjou-Méchain, M., Barbier, N., Picard, N., Rossi, V., Dormann, C., Cornu, G., Viennois, G., Bayol, N., Lyapustin, A., Gourlet-Fleury, S., & Pélissier, R. (2020). Spatial validation reveals poor predictive performance of large-scale ecological mapping models. *Nature Communications*, 11, 4540. https://doi.org/10.1038/s41467-020-18321-y

Posa, D. (1989). Conditioning of the stationary kriging matrices for some well-known covariance models. *Mathematical Geology*, 21(7), 755-765. https://doi.org/10.1007/BF00893320

Roberts, D. R., Bahn, V., Ciuti, S., Boyce, M. S., Elith, J., Guillera-Arroita, G., Hauenstein, S., Lahoz-Monfort, J. J., Schröder, B., Thuiller, W., Warton, D. I., Wintle, B. A., Hartig, F., & Dormann, C. F. (2017). Cross-validation strategies for data with temporal, spatial, hierarchical, or phylogenetic structure. *Ecography*, 40(8), 913-929. https://doi.org/10.1111/ecog.02881

Rousseeuw, P. J., & Van Driessen, K. (1999). A fast algorithm for the minimum covariance determinant estimator. *Technometrics*, 41(3), 212-223. https://doi.org/10.1080/00401706.1999.10485670

Schratz, P., Muenchow, J., Iturritxa, E., Richter, J., & Brenning, A. (2019). Hyperparameter tuning and performance assessment of statistical and machine-learning algorithms using spatial data. *Ecological Modelling*, 406, 109-120. https://doi.org/10.1016/j.ecolmodel.2019.06.002

Shannon, C. E. (1948). A mathematical theory of communication. *Bell System Technical Journal*, 27(3), 379-423. https://doi.org/10.1002/j.1538-7305.1948.tb01338.x

Shepard, D. (1968). A two-dimensional interpolation function for irregularly-spaced data. *Proceedings of the 1968 23rd ACM National Conference*, 517-524. https://doi.org/10.1145/800186.810616

Stein, M. L. (1999). *Interpolation of Spatial Data: Some Theory for Kriging*. Springer, New York. https://doi.org/10.1007/978-1-4612-1494-6

Strobl, C., Boulesteix, A.-L., Kneib, T., Augustin, T., & Zeileis, A. (2008). Conditional variable importance for random forests. *BMC Bioinformatics*, 9, 307. https://doi.org/10.1186/1471-2105-9-307

Tukey, J. W. (1949). Comparing individual means in the analysis of variance. *Biometrics*, 5(2), 99-114. https://doi.org/10.2307/3001913

Varma, S., & Simon, R. (2006). Bias in error estimation when using cross-validation for model selection. *BMC Bioinformatics*, 7, 91. https://doi.org/10.1186/1471-2105-7-91

Venables, W. N., & Ripley, B. D. (2002). *Modern Applied Statistics with S* (4th ed.). Springer, New York. https://doi.org/10.1007/978-0-387-21706-2

Wackernagel, H. (2003). *Multivariate Geostatistics: An Introduction with Applications* (3rd ed.). Springer, Berlin. https://doi.org/10.1007/978-3-662-05294-5

Wager, S., Hastie, T., & Efron, B. (2014). Confidence intervals for random forests: the jackknife and the infinitesimal jackknife. *Journal of Machine Learning Research*, 15(48), 1625-1651.

Wright, M. N., & Ziegler, A. (2017). ranger: a fast implementation of random forests for high dimensional data in C++ and R. *Journal of Statistical Software*, 77(1), 1-17. https://doi.org/10.18637/jss.v077.i01
