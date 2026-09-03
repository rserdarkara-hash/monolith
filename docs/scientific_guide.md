# Scientific & Analytical Methodology Guide

Monolith gives agronomists, soil scientists and geostatisticians a toolkit for mapping and explaining spatial variability. This guide states the mathematics behind each method, the modelling choices Monolith makes, and what the resulting numbers do and do not support.

> *Scope of this document: the methods implemented here (kriging variants, IDW, TPS, cross-validation metrics, PCA, statistical tests, supervised classification) are established published methods and are not original contributions of this software or its author. This guide describes how they are implemented in Monolith, not their theoretical origins. Works cited in the text are listed in Section 11 and cover the estimators and conventions actually in use; for anything beyond those, consult the primary literature directly.*

---

## 1. Spatial Interpolation Engines

Spatial interpolation builds continuous prediction surfaces from discrete point samples. Monolith implements both deterministic methods (geometric proximity only) and geostatistical models (spatial autocorrelation plus a statistical uncertainty model).

**NaN protection:** across all kriging methods, predictions (`var1.pred`) and theoretical variances (`var1.var`) are screened for NaN and infinite values and converted to `NA` before mapping.

### 1.1 Ordinary Kriging (OK)

**Mathematical intuition:** the value at an unsampled location <i>Z<sup>*</sup>(x<sub>0</sub>)</i> is a linear combination of the known surrounding values <i>Z(x<sub>i</sub>)</i>:
<br><br>
<div style="text-align:center;"><i>Z<sup>*</sup>(x<sub>0</sub>) = &sum; &lambda;<sub>i</sub> Z(x<sub>i</sub>)</i></div>
<br>
Unlike simple kriging, OK assumes an unknown but constant global mean (<i>&mu;</i>). The weights <i>&lambda;<sub>i</sub></i> minimize the estimation variance subject to <i>&sum; &lambda;<sub>i</sub> = 1</i>, and the covariance matrix that solves for them comes directly from the fitted theoretical variogram.

**Agronomical example:** predicting soil pH across a relatively uniform field where variation is driven by soil-forming processes rather than abrupt topography or management.

**Algorithmic stability (epsilon nugget):** for variables with extremely low variance (trace micro-nutrients, for example), a tiny nugget (`max(initial_sill * 1e-6, 1e-6)`) is enforced whenever the empirical nugget is exactly zero. This prevents singular matrix inversion during kriging.

### 1.2 Regression Kriging (RK)

**Mathematical intuition:** RK splits the variable into a deterministic trend and a stochastic residual.
<br><br>
<div style="text-align:center;"><i>Z(x) = m(x) + e(x)</i></div>
<br>
An ordinary least-squares linear model (`lm`) fits the trend <i>m(x)</i> from secondary covariates (elevation, NDVI). The residuals <i>e(x)</i> carry the spatially correlated variation the trend does not explain, and Ordinary Kriging is applied to them. The prediction is the sum of trend and kriged residual.

**Agronomical example:** mapping soil organic carbon, where elevation and satellite-derived moisture predict the baseline trend and kriged residuals recover localized accumulations the remote sensing misses.

**Reported trend diagnostics:** for each per-locality trend model the Scientific Analysis tab reports R², adjusted R², the residual standard error with its degrees of freedom, the overall F test with its p-value, and a coefficient table with standard errors, t statistics, p-values and 95% confidence intervals from the t distribution on the residual degrees of freedom. These describe the *trend component only*; the quality of the full RK prediction is assessed by the cross-validation metrics in the Model Performance table.

**Reading the trend p-values.** The standard errors, t statistics, confidence intervals and F test come from OLS and assume independent residuals. RK kriges those residuals precisely because they are spatially autocorrelated, which is the condition that violates the assumption: positive residual autocorrelation lowers the effective sample size, biases the standard errors downward and makes the p-values anti-conservative, so covariate significance is overstated. The coefficient estimates themselves remain unbiased. Use the residual Moran's I and its expectation E[I] = -1/(n-1) in the Model Performance table to gauge the severity. The estimator is not changed for this: a GLS refit of the trend under the fitted residual variogram, iterated to convergence, is Universal Kriging / KED, a different method. The two-step OLS-then-krige formulation is standard practice (Hengl et al. 2007); the caveat is on the inference, not the prediction.

### 1.3 Random Forest Kriging (RFK)

**Mathematical intuition:** RFK follows the same two-step logic as RK but replaces the linear trend with a random forest ensemble, which captures non-linear interactions among covariates without a parametric functional form. Ordinary Kriging is then applied to the forest's residuals.

**Agronomical example:** predicting crop yield across a heterogeneous landscape where the relationship between yield, slope, aspect and electrical conductivity is non-linear and interactive.

### 1.4 Co-Kriging (CK)

**Mathematical intuition:** CK extends OK by using one or more secondary variables to improve prediction of a primary variable, through the cross-variogram that models how the two co-vary in space:
<br><br>
<div style="text-align:center;"><i>&gamma;<sub>12</sub>(h) = (1 / 2N(h)) &sum; [Z<sub>1</sub>(x) - Z<sub>1</sub>(x+h)][Z<sub>2</sub>(x) - Z<sub>2</sub>(x+h)]</i></div>
<br>

**Agronomical example:** sparse, expensive laboratory nitrate measurements paired with dense, cheap electrical-conductivity sensor readings. Because EC and nitrate co-vary, CK uses the dense EC points to sharpen the nitrate surface.

**Covariate kriging fallback (CK, RK, RFK):** when a covariate cannot be kriged onto the prediction grid (pure nugget, collinearity collapse), the pipeline falls back to IDW (p = 2, nmax = 12) inside a `tryCatch` so map generation still succeeds.

**Covariate standardization and its cross-validation leak:** every auxiliary variable is standardized to zero mean and unit variance before the LMC is fitted, using the mean and standard deviation of the **full** data set. The target is not rescaled, so predictions, metrics and plotted variograms stay in the variable's own units. Cross-validation then holds points out of an already centred frame, so each held-out observation contributed, by a factor of 1/n, to the centring of its own predictors. The optimism this creates is affine, second order and O(1/n): it cannot change the rank ordering of predictions and touches only the covariates. Removing it would require re-standardizing inside every fold, replacing `gstat.cv()` with a hand-rolled co-kriging loop; that cost was reviewed and declined in favour of documenting the approximation. Read CK metrics as marginally optimistic relative to OK metrics on the same data.

**Cross-validation holdout convention (full-row removal):** CK cross-validation removes the entire held-out row, the primary observation *and* its co-located covariate observations, from the co-kriging system (`gstat.cv(..., remove.all = TRUE)`). The gstat default removes only the primary variable, which is right when secondary data are exhaustively available (a collocated sensor raster). Here the covariates are co-sampled laboratory measurements, so at real grid locations CK has no covariate values and predicts them jointly through the LMC. Scoring with the covariates left in would evaluate the model under an information regime the map never enjoys, and would be inconsistent with RK/RFK, whose leave-one-out procedure removes full rows. CK metrics are therefore honest with respect to the prediction task, and correspondingly lower than under the gstat default.

**The LMC fits sills only; one range is shared and held fixed.** A linear model of coregionalization requires a *common* range across every direct and cross variogram, so `fit.lmc()` runs with `fit.ranges = FALSE`. Two consequences follow, and they point in opposite directions. The starting **sill** is inert: with model type and range held fixed the variogram is *linear in its sill parameters*, so the weighted-least-squares solution does not depend on it (verified empirically, starting sills spanning 1e-6 to 1e12 on one empirical variogram return a bit-identical fitted sill). The starting **range**, by contrast, is not fitted and therefore *is* the final range of every variogram in the LMC. It is set to the range that weighted least squares fitted to the primary variable's own omnidirectional variogram, so the coregionalization length reflects the data's measured spatial structure. When that fit is itself the heuristic fallback, or returns no usable range, the seed falls back to the extent heuristic (half the variogram cutoff, i.e. a quarter of the bounding-box diagonal) and the run log says so. Read the CK range as the primary variable's range imposed on the whole system, not as a separately fitted quantity per covariate.

**Local search neighbourhood (nmax):** CK solves the system using only the nearest `nmax` observations of every variable, exposed as the **CK Max Neighbors** slider (5 to 60, default 15) in the Spatial Engine panel. This is a modelling choice, not a speed setting: `nmax` sets how local the stationarity assumption is. A small neighbourhood assumes the mean and covariance structure are constant only within a short radius (appropriate for non-stationary fields, and it avoids the matrix growth of global co-kriging); a large neighbourhood approaches a global system and suits genuinely stationary, sparsely sampled fields. Because the right value depends on point density and the fitted cross-variogram range, no single number is defensible across datasets; 15 is a default, not a recommendation. OK, RK and RFK krige with a global neighbourhood; IDW has its own **Max Neighbors** control. The applied value is recorded in the run-configuration summary.

### 1.5 Inverse Distance Weighting (IDW)

**Mathematical intuition:** a purely deterministic weighted average in which the weight falls off with distance <i>d</i> raised to a power <i>p</i> (commonly <i>p</i> = 2):
<br><br>
<div style="text-align:center;"><i>&lambda;<sub>i</sub> = (1 / d<sub>i</sub><sup>p</sup>) / &sum; (1 / d<sub>i</sub><sup>p</sup>)</i></div>
<br>
IDW assumes nearer points are more similar. It does not account for data clustering (redundant sampling) or directional anisotropy, and it produces no prediction variance.

**Agronomical example:** quick, inexpensive mapping of localized rainfall from a scattered gauge network, where strict stationarity assumptions are not needed.

### 1.6 Thin Plate Spline (TPS)

**Mathematical intuition:** TPS bends a notional sheet to pass through the sampled points while minimizing bending energy (the integral of the squared second derivatives). It yields very smooth surfaces but overshoots badly in areas devoid of data.

**Agronomical example:** smooth elevation contours or temperature gradients where abrupt discontinuities are physically implausible.

**Engine fallback (TPS to IDW):** if the TPS fit fails for a locality (a degenerate point configuration, for instance), the pipeline substitutes IDW using the active IDW parameters. The substitution is recorded in the run log (`TPS failed: ... Falling back to IDW.`) and raised as a per-locality warning, so fallback output is never mistaken for a genuine TPS result.

---

## 2. Grid Resolution Logic

Grid resolution (cell size) is the spatial support of the model. Choosing it well captures the true scale of variation without inventing detail the sampling cannot support.

### 2.1 The spatial support metric (FNN nearest neighbour)

When resolution is determined dynamically, a fast nearest-neighbour search measures point density. The recommended resolution is half the expected nearest-neighbour distance:
<br><br>
<div style="text-align:center;"><i>R<sub>rec</sub> = 0.5 &times; Expected Nearest-Neighbor Distance (h<sub>NN</sub>)</i></div>
<br>
This keeps grid spacing proportional to physical sampling spacing, so the map does not manufacture unmeasured detail.

Recommendations are always expressed in **metres**, whatever the analysis CRS. For a geographic (degree-based) CRS, distances and extents are measured through a Web Mercator (EPSG:3857) projection corrected by cos(latitude) (`calc_metric_spacing()`, `spatial_pipeline.R`). Interpolation itself always runs in a projected CRS, so metric pixel sizes remain valid.

### 2.2 Auto (per locality)

The density analysis runs independently for each locality, so a densely sampled locality receives a fine resolution and a sparse one a coarse resolution. This is a  geostatistically defensible choice for multi-site studies.

### 2.3 Auto (global)

Density is evaluated over the entire dataset as one spatial unit and a single resolution is applied to every selected locality, so pixels match in size across all outputs.

### 2.4 Fixed (manual)

The slider value overrides the dynamic recommendation, which is useful when a specific pixel size is required for cartographic consistency or printing.

### 2.5 Resolution actually used by a run

The nearest-neighbour rule drives the **sidebar suggestion** and the per-locality resolution table. The prediction grid an interpolation *run* builds is derived independently, per locality, from that locality's own boundary area:

$$R_{actual} = \min\left(1000,\ \max\left(5,\ \sqrt{A_{boundary} / 100{,}000}\right)\right) \quad \text{[m]}$$

that is, a target of roughly 100,000 grid cells inside each boundary, floored at 5 m and capped at 1000 m. The bound keeps the cost of predicting at every node finite regardless of extent, and evaluating it per locality preserves the Auto (Per Locality) intent: a compact, densely sampled locality gets a fine grid, a large one a coarser grid. The nearest-neighbour figure still governs the **dynamic buffer** width (Section 3.2).

In **Fixed (Manual)** mode the slider value is used verbatim, with one guard: a value that would generate more than about 4 million candidate cells over the boundary's bounding box is coarsened to that limit and the change is written to the run log, because `terra::as.points()` materializes every bounding-box cell before the boundary clip.

**The Auto rule is self-contained: it does not read the manual-resolution slider.** The slider is hidden outside Fixed mode and, in Auto modes, carries the *global* nearest-neighbour recommendation. Letting it impose a floor would let a global-scale quantity override a per-locality one through a control the user cannot see, coarsening every compact locality in a dataset whose fields lie far apart.

---

## 3. Spatial Boundary & Dynamic Buffering Systems

The boundary determines the physical extent of prediction, and unconstrained interpolation invites high-risk extrapolation. Monolith scales each locality's boundary padding from the active resolution and the mathematical constraints of the selected method.

### 3.1 Boundary types

* **Convex hull / concave hull:** envelopes drawn tightly around the outer points, like a shrink-wrap. They do not use or support buffer padding.
* **Wrapped (buffered):** a concave hull inflated outwards by the buffer distance ($D_b$) so the mapped area covers the field's outer borders.
* **Strict measured (point buffer):** individual buffer circles around every point, unioned together.

### 3.2 Dynamic buffering (wrapped mode)

Instead of one universal buffer distance, padding scales with the active resolution.

* **Calculation:** under *Auto* resolution the buffer scales with the density-derived resolution ($R_{local}$); under *Fixed* resolution it scales with the slider value ($R_{manual}$), so adjusting the slider updates the buffer column live.
* **Method-specific ratios:**
  1. **TPS**, $D_b = 1.0 \times \text{Resolution}$. A global smoothing spline bends without constraint outside the sample bounds, producing runaway edge effects. A tight buffer crops the grid before those edges ruin the value scale.
  2. **IDW**, $D_b = 2.0 \times \text{Resolution}$. Weights decay with distance, so extrapolation beyond mean sample spacing produces flat artificial halos converging to the global mean. A medium buffer keeps the map inside the region of neighbour decay.
  3. **Kriging suite (OK, CK, RK, RFK)**, $D_b = 3.0 \times \text{Resolution}$. Predictions decay toward the global mean or trend with distance and the kriging variance records the loss of information, so a wider buffer matching the range of spatial correlation is safe.

### 3.3 Strict measured (point buffer) manual enforcement

Dynamic buffering in this mode exaggerates coverage: at a coarse 200 m resolution a 3x multiplier draws 600 m circles around every point, claiming a vast mapped footprint the sampling does not support. **Strict Measured** therefore disables dynamic buffering and operates only under **Fixed (Manual)** settings.

**Grid evaluation domain:** the prediction grid is generated over the boundary's bounding box, and nodes outside the boundary are excluded *before* the engines run rather than masked afterwards. All engines predict pointwise, so no within-boundary value changes; this only removes computation the mask would have discarded, a substantial saving for concave or multi-part boundaries.

### 3.4 Buffer radius and cell size must be coherent

A cell is kept when its **centre** falls inside the boundary. That is the standard raster clip rule (used identically at grid construction and at the final `terra::mask`), but under a Strict Measured boundary it couples the buffer to the cell size, because there the boundary *is* the support claim: the union of circles of radius $D_b$ around the samples.

A sample lies anywhere within its own cell, so its distance to that cell's centre ranges from 0 to half the cell diagonal, $R/\sqrt{2}$. On a square grid the containing cell's centre is also the *nearest* cell centre (each cell is the Voronoi region of its own centre), so an isolated sample paints no cell anywhere precisely when that one centre escapes its buffer. Every sample therefore keeps the cell it sits in only when

$$D_b \ \ge\ \frac{R}{\sqrt{2}} \qquad \Longleftrightarrow \qquad R \ \le\ D_b\sqrt{2}$$

Below that threshold, the share of in-cell sample positions whose cell centre escapes the buffer is the cell area left uncovered by a disc of radius $D_b$ centred on the cell centre. That disc is inscribed in the cell only while $D_b \le R/2$; wider than that it spills across the four edges, and the overspill (four circular segments) must be subtracted. With $t = D_b/R$:

$$f_{lost} \ =\ 1 - \begin{cases} \pi t^{2}, & t \le \tfrac{1}{2} \\[4pt] \pi t^{2} - 4\left[\, t^{2}\arccos\!\frac{1}{2t} \; - \; \tfrac{1}{2}\sqrt{t^{2}-\tfrac{1}{4}} \,\right], & \tfrac{1}{2} < t < \tfrac{1}{\sqrt{2}} \end{cases}$$

Dropping the segment term would understate the loss above $D_b = R/2$ and report none at all above $D_b = R/\sqrt{\pi}$, inside the very range the threshold flags. With it, $f_{lost}$ falls continuously to exactly zero at $t = 1/\sqrt{2}$, so a flagged pair always carries a non-zero loss.

$f_{lost}$ is an upper bound on the visible effect: samples inside dense clusters are covered by their neighbours' buffers, so the gaps appear only around isolated samples. At $D_b = R/2$ (for example a 175 m buffer on a 350 m grid) it reaches $1 - \pi/4 \approx 21\%$, and it is why an otherwise valid run can show sampled points sitting on blank map.

Monolith treats an incoherent pair as an advisory, not an error: the run is scientifically sound, its support is simply under-resolved relative to the claim being made. The Spatial Engine sidebar flags the pair live under Fixed resolution, and every run re-checks it against the resolution actually used, raising the finding as a notification and writing it to the run log with the corrective values ($\lceil R/\sqrt{2} \rceil$ metres of buffer, and $\lfloor D_b\sqrt{2} \rfloor$ metres of cell size when that is at or above the 5 m floor both resolution controls impose). Lowering the resolution is generally preferable to widening the buffer, since widening it enlarges the area the map claims to support.

The same rule and the same advisory apply to the Classification Suite's Spatial Scope panel, which builds its prediction grid with the identical centre-in-boundary clip.

Hull boundaries are not flagged. The exposure there is confined to the *perimeter*: a concave hull runs through the outermost samples themselves, so an edge sample's cell centre can fall on the outside, but every interior sample is surrounded by hull area and keeps its cell whatever the resolution. Under a Strict Measured boundary the risk instead applies to every isolated sample in the domain, which is what makes it worth naming. A wrapped hull under dynamic buffering is safe by construction, since its buffer is $1.0$ to $3.0 \times R$ and so never below $R/\sqrt{2}$.

### 3.5 On-map distance measurement (ruler)

The Map Viewer's ruler measures a path clicked on the map. A distance on a curved Earth has no single correct value until the surface and the metric are stated, so both figures the tool reports are named rather than merged into one number.

**Ground distance (WGS84 geodesic).** The path is measured as a sequence of geodesic segments on the WGS84 ellipsoid ($a = 6378137$ m, $1/f = 298.257223563$), evaluated with Karney's algorithm through `terra`. This is the shortest surface distance between the clicked points and is independent of any map projection. The enclosed area of a closed path is the corresponding ellipsoidal area, the same `terra::expanse` call the class-zone export and the Area Coverage table use, so the ruler and the reported class areas rest on one convention.

From the third vertex the shape is a ring, and the reported length closes with it: the closing segment $v_n \to v_1$ is included, making the figure the perimeter of exactly the polygon whose area is reported beside it. Two vertices remain an open line and report a length.

A ring that crosses itself reports its perimeter but **no area**. Both engines integrate around the ring in traversal order (the shoelace sum $\tfrac{1}{2}\left|\sum_i (x_i y_{i+1} - x_{i+1} y_i)\right|$ in the plane, its geodesic counterpart on the ellipsoid), and on a figure-eight the two lobes are traversed in opposite senses, so the result is their *difference* rather than the area enclosed on the screen: a symmetric bowtie evaluates to zero. The crossing is detected on the clicked vertices themselves, so it is a property of the shape rather than of the projection. The perimeter is unaffected by a crossing and is still exact, so it is kept.

**Projected distance (Target Mapping CRS).** The same vertices are transformed into the analysis CRS and measured planimetrically, $\sum_i \sqrt{(x_{i+1}-x_i)^2 + (y_{i+1}-y_i)^2}$, with the area taken by the shoelace formula. This is the metric every engine in Monolith actually operates in: variogram lags, IDW separation distances, TPS coordinates, grid resolution and buffer radii are all lengths in this system. A measured separation is therefore comparable with a fitted variogram range only in this column.

The projected figure is reported for the CRS the **displayed run** was computed in, not for whatever the sidebar currently holds. Retargeting the sidebar for the next run leaves the measurement describing the surface actually on screen, which is the only reading under which the comparison with that surface's variogram range is meaningful.

The difference between the two is the projection's distance distortion at that place and orientation. For a conformal projection such as UTM it is governed by the point scale factor $k$, which on a transverse Mercator zone runs from $k_0 = 0.9996$ on the central meridian to roughly $1.0010$ at the zone edge. A gap of a few **hundredths** of a percent is therefore the normal state inside a zone, not a warning sign: measured at 39°N in zone 36N the projected length runs $-0.040\%$ against the ground figure on the central meridian, passes through zero near 1.6° from it, and reaches $+0.039\%$ at the zone edge, with the area gap twice that because it scales as $k^2$. Near the equator the zone is wider on the ground and the edge value approaches $0.1\%$. A discrepancy well beyond this is diagnostic rather than cosmetic: it indicates that the selected CRS is a poor fit for the area being analysed, and the same distortion is present in every lag distance the variogram was fitted on. When the Target Mapping CRS is geographic, no planar figure is produced at all, for the reason `validate_and_project_sf()` refuses to interpolate in degrees: a Euclidean length computed on longitude and latitude is not a length.

**Metric axis units are required.** A projected Target Mapping CRS whose linear unit is not the metre is refused before a run starts (`validate_crs(require_metric = TRUE)`), and reported in the Target CRS panel as soon as it is selected. That panel carries this rule and the suitability verdict of §3.7 together, because the two are orthogonal and would otherwise contradict each other: a State Plane zone in US survey feet sits inside its own declared area with $k \approx 1$ and is still refused. The engines operate on the CRS's own axis units while every number the app accepts or prints is stated in metres, so a State Plane zone in US survey feet would silently turn a $50$ m grid into $15$ m, a $250$ m buffer into $76$ m and a fitted variogram range into a figure $3.28\times$ its stated size. Refusing the CRS keeps that identity true at one gate, rather than threading unit conversions through every engine, table and slider. A geographic Target CRS is unaffected, since the pipeline projects it to a metric UTM zone itself, and the **input** CRS may use any unit, because the pipeline projects out of it before measuring anything.

The interactive measure control computes on a sphere of radius $R = 6371000$ m: great-circle segment lengths and a spherical-excess area. That is convenient for the running readout it shows while points are being clicked, but it has no knowledge of the analysis CRS and it departs from the ellipsoidal value by up to about $0.5\%$ in length (largest for meridional paths at low latitude, where the meridional radius of curvature $M = 6335439$ m is furthest below $R$). In area the departure follows $R^2 / (MN)$, so it vanishes near 35° latitude and grows to about $+0.45\%$ at the equator and $-0.9\%$ towards the poles. Its finished figures are therefore replaced by the server-side ones described above, computed for the shape and stored with it, so each measurement carries a single authoritative set of numbers that stays correct when it is reopened later. Measurement is a read-only aid: it records nothing, exports nothing and does not enter any model.

---

### 3.6 Identifying the input CRS

The Input Data CRS is the system the uploaded X/Y columns were recorded in, and it is the system every engine works in: `run_regional_interpolation()` builds its points with it, and the Target Mapping CRS is applied only at the end, to reproject the finished surface for display and export. It cannot be given a default, because a projected coordinate pair does not contain its own CRS.

**Why bare eastings cannot be resolved.** The 60 UTM zones are congruent: the same $(E, N)$ pair is a valid position in every one of them, reproducing an identical latitude and a longitude displaced by exactly $6°$ per zone. Reading zone-33 coordinates as zone 35 therefore moves a survey $12°$ east, about $812$ km at $52°$ N, with no coordinate falling outside its zone's valid range and no error raised anywhere. Any default zone is a silent mis-georeferencing for every user outside it, so Monolith ships with both CRS selectors empty and refuses to run until one is chosen.

**What the wrong zone does and does not affect.** Because the zones are congruent, every coordinate *difference* is preserved: separation distances, variogram lags, fitted ranges and all cross-validation metrics are numerically identical under a wrong zone of the same hemisphere. What is wrong is the georeferencing, so the basemap, the exported GeoTIFF and shapefile, and any polygon drawn on the map all describe the wrong place. The exception is the output reprojection: resampling into a zone the data does not belong to inflates distances by the point scale factor of §3.5, about $1.07\%$ two zones off at $52°$ N.

**Identification from evidence.** Coordinates inside $\pm180 / \pm90$ are degrees and resolve to EPSG:4326. For projected coordinates the app tests a short candidate list rather than searching a catalogue: a companion `lon`/`lat` pair in the same table fixes the zone exactly through $z = \lfloor (\lambda + 180)/6 \rfloor + 1$, leaving the WGS84 zone, its southern twin, the ETRS89 equivalent and Web Mercator as the four candidates. Each is scored by the largest great-circle distance between the transformed $(x, y)$ and the reference pair. Candidates that place the data within $5$ m of one another are collapsed first, since a WGS84 UTM zone and its ETRS89 counterpart are the same grid to under a metre and would otherwise tie with each other; the survivor is accepted only when it reproduces the reference pair to within $5$ m and every remaining candidate is at least $100\times$ worse. On a Potsdam UTM 33N file that margin is $0.006$ m against $987{,}681$ m.

Where no geographic pair exists, an uploaded boundary shapefile with a `.prj` serves the same purpose, scored by the fraction of points falling inside the boundary (with a $5\%$ margin on its diagonal) and accepted only when one candidate exceeds half the points and no other reaches half of that. **Narrowing when there is no evidence.** Failing both, the app does not guess. It asks where the data is and answers with the projections that put it there, which converts an unanswerable question about a code into an answerable one about a place. The candidate universe is the complete EPSG projected set that PROJ's own catalogue (`proj.db`) declares with an area of use, $5{,}330$ non-deprecated codes worldwide; no region is assumed and no national-grid list is curated.

The selection runs in the order that makes it tractable. Transforming all $5{,}330$ candidates costs over two minutes, so the first filter touches no geometry at all: a candidate is kept only if the lat/lon box it declares as its area of use contains the point the user indicated. That is a comparison on the query result, and anywhere on earth it leaves $15$ to $50$ candidates (Potsdam $48$, Ankara $31$, Nairobi $19$, Perth $28$, Reykjavik $49$), reducing the transform bill to about one second. The survivors are then transformed and filtered twice more: the resulting position must fall inside the *same* candidate's declared area of use, which is what rejects a national grid the data does not belong to, and it must lie within $500$ km of the indicated point, which is what rejects the world-scale display projections that satisfy any containment test trivially. Candidates whose positions agree to within $250$ m are then collapsed onto one row, so each row of the shortlist is one *place* carrying its equivalent codes, ordered by distance from the indicated point. That tolerance is far wider than the $5$ m used for identification above, deliberately: a grid's datum siblings differ by less than a map click can resolve (WGS 84, WGS 72, WGS 72BE and ED50 UTM zone 33N read the same Potsdam coordinates within $209$ m of each other), so presenting them as competing rows ranked by their distance to that click lets click error, which is kilometres, decide between differences of metres, and an obsolete datum can lead. Folded onto one row they are ordered by preference instead (WGS 84 first, then ETRS89), and the row states the size of the fold rather than implying the codes are interchangeable. Two genuinely different answers, adjacent UTM zones, sit some $400$ km apart and are never folded. Free text (a country or region) is matched against the area-of-use description, which names the countries each CRS covers, but it is a secondary path and is capped: unlike a point it does not bound the set, "United States" alone matching $1{,}864$ codes, and a capped search is reported as capped. Text supplied alongside a click narrows the pool the click has already bounded; text matching nothing inside it returns an empty shortlist, never a search widened back out.

On a Potsdam UTM 33N file the shortlist is a single position, EPSG:32633, carrying EPSG:3045, 25833, 10733, 23033, 32233 and 32433 as codes reading the same place to within $209$ m; the neighbouring zones, which sit $406$ km away, are not offered. This never becomes an identification claim. The rule the app follows is that it identifies your CRS when the file carries the evidence to prove it, narrows it to a short list when it does not, and never assumes a zone. Where no candidate lands near the indicated point the shortlist is empty rather than a best guess. Throughout, the position the current selection produces is reported in degrees, so a wrong zone is visible before any model is fitted.

### 3.7 Choosing the Target Mapping CRS

The Target Mapping CRS is the system the finished surface is resampled into and the system every distance the app accepts or prints is expressed in: grid resolution, buffer radius, exported cell size, fitted variogram ranges and the ruler's projected column. A CRS is fit for that role only where it measures correctly, and the axis-unit rule of §3.5 is necessary but not sufficient. Web Mercator's axis unit *is* the metre, and at $52°$ N it reports every distance $64\%$ too long.

**The point scale factor.** A projection's local distance distortion is the point scale factor $k$, the ratio of a projected length to the true length of the same infinitesimal arc on the ellipsoid. Monolith measures it empirically rather than parsing projection parameters, so any CRS PROJ can transform into can be judged. Two short arcs are projected at each sample position and compared against their exact ellipsoidal ground lengths on WGS84 ($a = 6378137$ m, $1/f = 298.257223563$):

$$s_\lambda = N(\varphi)\cos\varphi\,\Delta\lambda, \qquad N(\varphi) = \frac{a}{\sqrt{1 - e^2\sin^2\varphi}}$$

$$s_\varphi = M(\varphi)\,\Delta\varphi, \qquad M(\varphi) = \frac{a(1 - e^2)}{(1 - e^2\sin^2\varphi)^{3/2}}$$

Both arcs are measured, not one. For a conformal projection (transverse Mercator, Mercator, Lambert conformal conic) $k$ is identical in every direction and either arc suffices, but an equal-area projection distorts the two by construction and in opposite senses: EPSG:5070 (Albers, conterminous US) evaluated at Potsdam gives $+3.17\%$ along the parallel and $-3.07\%$ along the meridian. Equal-area systems are exactly what a user might wrongly choose for distance work, so the larger $|k - 1|$ decides. The arcs are $0.001°$ half-arcs, about $111$ m, short enough that the result is the point scale factor rather than a finite-distance average.

**Where it is evaluated.** At the data centroid and the four corners of the data bounding box, taking the worst case. A large study area can be inside tolerance at its centre and outside it at an edge, which is the zone-boundary case precisely. All five positions come from the same `crs_landing_position()` used for the Data Setup readout of §3.6, so the gate and the readout can never disagree about where the data is.

**Thresholds.** $|k - 1| > 0.1\%$ raises a warning; $> 1\%$ refuses the run, subject to an explicit override recorded in the run configuration. The lower figure is the practical floor for spatial analysis: a UTM zone holds $|k - 1|$ under $0.04\%$ across its own $6°$ of longitude (§3.5), so anything worse means the CRS does not belong to the region. The upper figure is where the error exceeds any plausible tolerance, a $250$ m buffer becoming $252.5$ m and a fitted range misreported by the same factor. Measured at Potsdam ($12.958°$ E, $52.466°$ N):

| CRS | $k$ | $k - 1$ | verdict |
|---|---|---|---|
| EPSG:32633 (UTM 33N) | 0.999836 | $-0.016\%$ | fit, the correct zone |
| EPSG:25833 (ETRS89 / UTM 33N) | 0.999836 | $-0.016\%$ | fit |
| EPSG:3035 (LAEA Europe) | 1.000113 | $+0.011\%$ | fit |
| EPSG:5514 (S-JTSK Krovak) | 1.000356 | $+0.036\%$ | outside its declared area, measures correctly |
| EPSG:32634 (UTM 34N) | 1.003260 | $+0.326\%$ | warned |
| EPSG:32635 (UTM 35N) | 1.010729 | $+1.073\%$ | refused |
| EPSG:3857 (Web Mercator) | 1.642049 | $+64.2\%$ | refused |

**Area of use.** Every EPSG CRS declares the lat/lon box it is defined for, read from PROJ's own catalogue (§3.6) and from the WKT2 `BBOX` node where the catalogue has no row. A code may declare several boxes ($17$ do in this catalogue; EPSG:2393 declares both a $3°$ zone strip and the whole of onshore Finland), and their union is the area of use, so data inside any one of them is inside, which is the same rule the candidate filter of §3.6 applies when it offers that code. Data landing outside that box raises a warning and never refuses a run. The declared box is an authority's advisory bound, not a measurement: a study area straddling a zone boundary sits outside one box while still measuring correctly, and EPSG:5514 above is outside its area at Potsdam yet distorts distances by only $0.036\%$. The scale factor is what measures the harm, so the scale factor is what refuses.

**What is never refused.** A geographic Target CRS skips the check entirely: EPSG:4326 is not a projection, has no meaningful $k$, and the pipeline projects it to a metric UTM zone itself, the same exemption the axis-unit rule makes. Nothing is refused on a question that cannot be answered either, an unparseable CRS, one declaring no extent, one PROJ will not transform into: the verdict is a warning at most, matching the posture of the axis-unit check when the unit cannot be resolved.

Web Mercator is therefore not offered as a Target Mapping CRS at all. It remains available as an *input* CRS, where data genuinely can arrive in it, and typing it into the target selector reaches the gate like any other entry. The check costs a handful of coordinate transforms and runs on the main thread; it decides which runs are permitted and alters no value a permitted run produces.

**The recommendation.** A verdict that a CRS is unsuitable is only half an answer, and the half the user cannot act on: the declared area of use is a lat/lon box, not a prescription. Monolith therefore states which CRS *is* fit for the data, computed from the data's own position rather than looked up. The longitude fixes the UTM zone exactly, $\text{zone} = \lfloor (\lambda + 180)/6 \rfloor + 1$, and the mean latitude picks the hemisphere, giving EPSG $32600 + \text{zone}$ north and $32700 + \text{zone}$ south; PROJ's catalogue is consulted only for the code's proper name. WGS 84 / UTM is recommended in every region rather than the local national grid because it is defined for every longitude, holds $|k - 1| \le 0.04\%$ across its own zone (§3.5), and requires no judgement about which of several national systems an area belongs to. A user preferring a national grid enters it directly, and it is then judged on the same two rules as any other entry.

The zone is taken from the mean sample position while the recommendation's own $k$ is measured at every sample position, so a study area spanning zones reports the worst case of what is being offered. The recommendation is withheld when its deviation is no better than that of the current selection, which is the case for a study area wide enough that no single zone holds tolerance at all its corners: offering one there would move the user between two CRS that are both refused.


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
- **Range (<i>a</i>):** the distance beyond which points are statistically independent.

**Auto-fit engine.** To avoid singular fits on difficult datasets, the auto-fit screens a grid of candidates rather than trusting a single fit. It fits four families, Sph (spherical), Exp (exponential), Gau (Gaussian) and Mat (Matern, &nu; = 1.5), from each of four starting ranges (`max_dist/10`, `/5`, `/4`, `/2`), and keeps the eligible candidate with the lowest weighted-least-squares error, preferring converged fits over singular or non-converged ones. If no candidate qualifies, a heuristic spherical model is returned rather than an error: nugget-dominated (95% nugget, range `max_dist/10`) when the empirical nugget already exceeds 80% of the sample variance, otherwise structured (80% partial sill, range `max_dist/2`). An empirical variogram with fewer than five lag bins skips fitting and takes the structured fallback directly, because `gstat::fit.variogram` can crash on one that short.

**Matern smoothness is fixed, not estimated.** The search fits nugget, partial sill and range by weighted least squares but holds the Matern smoothness &nu; (kappa) at **1.5**, the Matern 3/2 model. `gstat::fit.variogram` estimates &nu; only when asked (`fit.kappa = TRUE`), and it is deliberately not asked: a free smoothness parameter would give the Matern candidate one more degree of freedom than the other three, making the cross-candidate residual-sum-of-squares comparison an unequal contest. Read the "Mat" candidate as "Matern with &nu; = 1.5". If the empirical variogram suggests a markedly smoother or rougher process near the origin, use **Manual Tuning** to compare families directly: Gaussian is smoothest at the origin and exponential roughest, with spherical and Matern (&nu; = 1.5) between them, so short-lag behaviour can be bracketed without freeing a smoothness parameter.

**The nugget must be non-negative.** A candidate whose fitted nugget <i>C<sub>0</sub></i> is below zero is refused outright, before any error comparison. Such a model makes <i>&gamma;(h)</i> negative for small <i>h</i>, so it is not a valid (conditionally negative definite) covariance model and the kriging system built from it has no solution. The failure is silent rather than loud: `gstat` does not raise there, it returns an undefined prediction at every location, which would surface as a blank locality and an empty metrics row behind a variogram panel reporting a clean converged fit. This is an *eligibility* rule, not a preference: an invalid model must not be comparable on fit error at all.

**Candidates are screened on the practical range.** Each fitted candidate must fall inside a sanity window (between one hundredth and twice the largest empirical lag distance) before it can win, and the test is applied to the **practical range**, the distance at which the model reaches about 95% of its sill. gstat's range parameter *a* means a different ground distance in every family: the practical range is *a* for spherical, 3*a* for exponential, &radic;3·*a* for Gaussian and about 4.75·*a* for Matern with &nu; = 1.5. A window applied to the raw *a* would therefore judge families by different standards, admitting an exponential structure extending three times further than a spherical one on the same test, so eligibility would depend on the family rather than on fit quality. Converting first makes the window mean the same physical distance for every candidate. This affects only which candidates are *eligible*; the winner among them is still the lowest weighted-least-squares error, converged fits preferred.

**Manual override.** Automated fits can settle in local minima or chase outliers at long lags. **Manual Tuning** lets you prioritize the fit at short lags, which carry the greatest weight in kriging.

### 4.2 IDW optimization

* **Logic:** an automated search finds the **distance power** that minimizes interpolation error for each locality, testing values from 0.5 to 5.0.
* **Fold scheme:** the folds come from the same authority as every reported cross-validation, so the search honours the **Cross-Validation Strategy** selected in the sidebar (Section 5): LOOCV, seeded random 10-fold, or ten spatial k-means blocks, with the same small-*n* degradations. A power tuned on random folds while the metrics table reports Spatial Block CV would be optimistic for the number shown: a random split leaves each held-out point's near neighbours in the training set, which systematically favours a steeper decay. Selecting a strategy therefore changes the optimized power, and with it the IDW surface.
* **Reproducibility:** one deterministic fold-assignment vector (seed `CV_FOLD_SEED`) is shared across all candidate powers, so the comparison is paired and the winner is selected on distance-decay performance rather than fold noise.
* **Local adaptation:** the "Optimize" button computes a separate power for every selected locality.
* **Projected search:** the search runs on the same projected metric coordinates the interpolation uses. A geographic upload is transformed to its local UTM zone first, so neighbour selection and distance decay use true ground distances and the stored power matches the run that consumes it.
* **Deduplicated search set:** co-located points (identical projected coordinates at centimetre precision) are removed exactly as the run removes them. Otherwise a held-out point's co-located twin predicts it at distance zero, a guaranteed exact hit for every candidate power, so the search would be scored on inflated skill and on a different point set than the run. This is a no-op for datasets without co-located samples.
* **Selection optimism:** the power is chosen and then evaluated on the same cross-validation data, with no nested outer loop, so the reported metrics for an optimized IDW model are mildly optimistic. A nested loop would multiply cost for a single tuning parameter on a coarse candidate grid, where the selection-induced optimism is small. Compare methods with this in mind.

### 4.3 TPS optimization

* **Logic:** the **smoothing parameter** balances honouring individual points against a generalized regional trend. The lambda slider defaults to `< 0` (Auto GCV), applying generalized cross-validation natively during interpolation.
* **GCV diagnostics:** the optimum is found by generalized cross-validation inside `fields::Tps`. The best lambda is the value with the lowest GCV score. Lambda = 0 is an exact interpolator (zero error at sample points); higher values give a smoothing spline, usually better for noisy sensor data.
* **Visualization:** clicking "Optimize TPS Lambda" runs an explicit grid search that overrides Auto mode and plots the **GCV curve** in the Scientific Analysis tab, so you can check whether the search reached a clear minimum.
* **Projected search:** as with IDW, a geographic upload is projected to its local UTM zone before coordinates are normalized to the unit square, so the spline geometry and its GCV-optimal lambda reflect ground distances and agree with the run.
* **Deduplicated search set:** co-located points are removed before the search, matching the run's point set. `fields::Tps` handles exact replicates through its pure-error machinery, which shifts the GCV curve, so without deduplication the stored lambda would be optimized on a different point set than the run.
* **Selection optimism (fixed lambda only):** a lambda produced by the button, or typed into the slider, is stored once and reused unchanged in every cross-validation fold, so it was chosen on data that includes each fold's held-out point and the reported metrics are mildly optimistic. **Auto (GCV) mode carries no such reuse:** every fold refits its own lambda on that fold's training points alone, so the smoothing parameter is never informed by the point it is scored on. If you optimize lambda mainly to inspect the GCV curve, return the slider to Auto for the run whose metrics you intend to report.

---

## 5. Validation Diagnostics

Cross-validation produces the predicted-versus-observed pairs behind every performance metric. The **Cross-Validation Strategy** control, directly beneath the Interpolation dropdown, governs how folds are formed. It affects the reported metrics only and **never the interpolated surface**, with one deliberate exception: the IDW power search (Section 4.2) builds its folds from this same control, so that the power selected and the metrics that score it share one validation design. Re-running the IDW optimizer under a different strategy can therefore return a different power, and the resulting surface changes.

- **Auto (default):** leave-one-out cross-validation (LOOCV) at 50 or fewer observations; a seeded, balanced random 10-fold above 50.
- **Standard LOOCV:** full leave-one-out at any sample size. The most rigorous option, but heavy beyond roughly 2000 points, since RK and RFK refit their trend model and residual variogram in every fold.
- **Spatial Block CV:** ten spatially contiguous folds formed by k-means clustering of the sample coordinates. Random folds place a test point's near neighbours in the training set, so under spatial autocorrelation the model effectively interpolates from almost-collocated data and cross-validation overstates its skill. Holding out whole blocks removes that leakage and gives a more honest estimate of prediction at genuinely unsampled locations, the recommended design for digital soil mapping validation (Roberts et al. 2017; Ploton et al. 2020). Below 30 observations the blocks degenerate, so the engine falls back to LOOCV.

All fold assignments use a fixed seed (`CV_FOLD_SEED = 12345`); see Section 9.1 for where to change it.

> **What each engine refits inside a fold. Read this before comparing engines.**
>
> * **RK and RFK** refit everything inside every fold: the trend model (`lm` or `randomForest`) *and* the residual variogram are estimated from the fold's training points alone (`perform_kriging_loocv`). Their metrics are a clean out-of-sample estimate of the whole modelling procedure.
> * **OK, CK and IDW** are cross-validated through `gstat::krige.cv` / `gstat::gstat.cv`, which re-solve the kriging system per fold but **do not refit the variogram**: the model fitted once on the full point set (an LMC for CK) is reused in every fold, so each held-out point contributed to the spatial-structure model that predicts it.
>
> The resulting optimism is second order and usually small, since a variogram summarises every pair in the data set and removing one point moves it little, but it is **systematic and one-directional**: OK and CK metrics are mildly optimistic *relative to* RK and RFK on the same data. Read a narrow RMSE or R² advantage for OK/CK accordingly. Comparisons within an engine, across localities or variables, are unaffected. IDW carries the same reuse for its power exponent (Section 4.2) and TPS for a fixed lambda (Section 4.3).

**Repeated cross-validation (optional).** Every reported metric comes by default from **one** fold assignment. Because the split is itself random (for random k-fold and Spatial Block; LOOCV is deterministic), at moderate sample sizes the spread of a metric across alternative splits can be as large as the difference between two interpolation methods. The fixed seed makes the comparison **paired**, which removes fold luck from the *comparison* but not from each absolute number.

The **Repeated CV** checkbox quantifies that spread. Each engine re-runs its own cross-validation under additional fold assignments (seeds `CV_FOLD_SEED + 1`, `+ 2`, ...) and the Scientific Analysis tab gains a **Fold-Realization Stability** table reporting every displayed error metric as *mean ± standard deviation* across realizations.

* **Nothing else changes.** Realization 1 is the reference run, so the Model Performance table, the residual maps, the CV-residual write-back and the interpolated surface are identical whether the option is on or off. The feature only adds a table.
* **Only the partition varies.** A repeat re-draws the fold assignment and nothing else: data, variogram policy, neighbourhood, trend model and every model-fitting seed are held fixed, so the SD isolates fold-assignment variance.
* **Deterministic plans never repeat.** Where the resolved plan is leave-one-out (Standard LOOCV at any n, Auto at n ≤ 50, Spatial Block below n = 30) the partition is unique, so that locality contributes a single realization and the run log says so. In a pooled row such a locality is carried unchanged into every repeat, which is exact for a deterministic plan.
* **How to read the SD.** It is the resolution of the comparison, not an error bar on a physical quantity: if two methods' RMSE values differ by less than this SD, the ranking between them is fold luck. Mean ± SD of a repeated k-fold is the conventional way to present resampling-based skill estimates (Kohavi 1995; Molinaro et al. 2005).
* **Cost.** One extra full cross-validation pass per repeat, per locality, per surface. For RK and RFK, five realizations is roughly five times the cross-validation time; prediction surfaces are computed once regardless.
* **Moran's I is not repeated.** It diagnoses one residual field and is reported for realization 1 only.

> **Rank requirement for Regression Kriging.** Every RK fold refits the linear trend on the fold's training rows, so each fold needs more training points than the model has coefficients (one per covariate, plus intercept, plus one residual degree of freedom). With many covariates, few samples, or a coarse blocking scheme on a small locality, the fold's `lm` becomes rank-deficient and predicts `NA`, which propagates through residual kriging and turns the reported metrics into `NA` with nothing indicating why. The engine checks this up front and **reports an explicit error naming the shortfall**; the same guard applies to the main RK fit, where falling short routes the locality through the Ordinary Kriging fallback with the reason logged. The remedy is fewer covariates, more samples, or a strategy with smaller held-out folds.

> **Constant target in a locality.** R², NSE, Lin's CCC, RPD and RPIQ are ratios against the *observed* variability of the target. When every sample in a locality carries the same value, that denominator is zero and the metrics are **undefined**, not zero: they are reported as `NA` (RMSE, MAE and Bias remain defined). The surface is flat by construction and the variogram degenerates to the heuristic fallback, so the run also writes an explicit warning naming the constant target. Read that, not the amber fallback banner, as the cause.

Dropping points according to the chosen partition yields the predicted-versus-actual pairs (<i>P<sub>i</sub></i> vs <i>O<sub>i</sub></i>) that `perform_cv` turns into the metric suite.

- **RMSE (root mean square error):**
  <div style="text-align:center;"><i>RMSE = &radic;( &sum; (P<sub>i</sub> - O<sub>i</sub>)<sup>2</sup> / n )</i></div>
  Absolute fit in the units of the variable. Smaller is better.

- **NRMSE (normalized RMSE, %):**
  <div style="text-align:center;"><i>NRMSE = RMSE / |O<sub>mean</sub>| &times; 100</i></div>
  RMSE as a percentage of the **absolute** mean of the observations, making error magnitudes comparable across variables on different scales. The absolute value keeps the statistic positive for negative-mean variables (anomalies, sub-zero temperatures, redox potentials): RMSE is non-negative, so a signed denominator would give those a meaningless negative error percentage. Undefined (NA) when the mean is 0. **NMAE (%)**, reported only in the uploaded-prediction table, is <i>MAE / |O<sub>mean</sub>| &times; 100</i>, normalized the same way and for the same reason.

- **Traditional R² versus correlation R²:**
  - **Traditional R² (Nash-Sutcliffe efficiency; Nash & Sutcliffe 1970):** how well the model predicts relative to using the global mean.
    <div style="text-align:center;"><i>R&sup2; = 1 - &sum;(O<sub>i</sub> - P<sub>i</sub>)<sup>2</sup> / &sum;(O<sub>i</sub> - O<sub>mean</sub>)<sup>2</sup></i></div>
    It penalizes bias and goes negative when the model is worse than the mean.
  - **Correlation R² (Pearson):** linear correlation only. A model predicting exactly double the actual value scores 1.0 here while its traditional R² is below 0. Traditional R² is the one to prioritize for spatial accuracy.

- **MBE (mean bias error):**
  <div style="text-align:center;"><i>MBE = &sum; (P<sub>i</sub> - O<sub>i</sub>) / n</i></div>
  Systemic bias: positive means the model generally overestimates.

  > **Two different bias statistics are reported, in opposite directions.** Tables comparing an **uploaded ML prediction column against the observed values** report this MBE, labelled **"MBE (ML pred - observed)"**, in the direction *predicted minus observed*. The **Model Performance** table for the interpolation engines reports **"Bias (ME)"**, the mean cross-validation error, computed as *observed minus predicted*. The two describe different models and carry **opposite signs for the same over-prediction**, so never read one against the other; the labels are deliberately distinct.

- **CCC (Lin's concordance correlation coefficient; Lin 1989):** how closely the paired data fall on the 45-degree line of perfect agreement, combining precision (Pearson's r) with accuracy (bias shift). Computed on **population** second moments, <i>&rho;<sub>c</sub> = 2s<sub>xy</sub> / (s<sub>x</sub><sup>2</sup> + s<sub>y</sub><sup>2</sup> + (&mu;<sub>x</sub> - &mu;<sub>y</sub>)<sup>2</sup>)</i> with each moment scaled by <i>(n-1)/n</i>, as Lin defines it and as `DescTools::CCC` computes it. The scaling matters because the squared bias term in the denominator is a population quantity: mixing it with sample <i>(n-1)</i> variances gives a statistic that is always at least as large as Lin's, by a margin that grows with bias relative to total variance and shrinks as 1/n. That margin falls exactly on the systematic offset CCC exists to penalise, so the population form is the honest one. Reported as **NA when either vector is constant**: the correlation term does not exist there, and for two identical constant vectors the formula degenerates to 0/0.

- **RPD (ratio of performance to deviation):** <i>RPD = SD<sub>actual</sub> / RMSE</i>. Dimensionless. Above 2.0 indicates an excellent predictive model, below 1.4 poor predictive capacity.

- **RPIQ (ratio of performance to interquartile distance):** <i>RPIQ = (Q3 - Q1) / RMSE</i>. More robust than RPD on skewed data, common in soil properties such as salinity.

- **SMAPE (symmetric mean absolute percentage error):** absolute errors as percentages, avoiding the extreme inflation that arises when actual values approach zero. Where an observation and its prediction are *both* exactly zero the summand is 0/0; that term is defined as **0** by the usual convention rather than dropped, so sMAPE is averaged over the same sample count as every other metric in the table.

**Undefined metrics are reported as NA, never as infinity.** Every ratio metric above has an input configuration that zeroes its denominator, and in each case the quantity is genuinely undefined rather than infinitely good or bad:

| Metric | Undefined when | Interpretation |
|---|---|---|
| NSE | observations are constant (SST = 0) | there is no variance for the model to explain |
| NRMSE (mean) | mean of observations is 0 | a zero-mean (centred / anomaly) variable has no scale to normalise against |
| RPD, RPIQ | RMSE = 0 (perfect prediction) | spread-to-error ratios are undefined at zero error (Chang et al. 2001) |
| CCC | either vector is constant | the correlation term does not exist |

Reporting `Inf` would propagate into the Model Performance table, the exported metrics CSV and the pooled "Total (Combined)" diagnostics, where it reads as a real and spectacular score. The tables show NA instead.

**The two performance tables share one metric dictionary.** Every statistic in both is computed by the same function (`perform_cv`), so a definition can never differ between them. The **Model Performance** table (interpolation cross-validation) reports, in order: RMSE, NRMSE (%), MAE, R² (Corr), R² (NSE/Trad), Bias (ME), Lin's CCC (Agree), RPD (Prec), RPIQ, SMAPE (%), Moran's I and Moran p. The **uploaded-prediction** table reports the same statistics under the same labels with three deliberate differences: it adds NMAE (%), it reports MBE in the *predicted minus observed* direction, and it carries **no Moran's I or p**. That omission is substantive. Moran's I here diagnoses *cross-validation* residuals, the errors a spatial model makes at held-out locations; the residuals of an externally supplied prediction column come from a model this application neither fitted nor resampled, so a spatial-autocorrelation test on them would validate nothing the dashboard controls.

- **Moran's I (spatial autocorrelation of residuals; Moran 1950):**
  Tests whether cross-validation errors are randomly distributed across the field. Significantly positive I means errors are clustered (consistent underestimation in the north, overestimation in the south, for example), indicating the model missed a macroscopic spatial trend and that RK or RFK may be required. The neighbour structure is a **symmetric k-nearest-neighbour contiguity** (`k = 8`, capped at n − 1 for small samples), row-standardised (`spdep::nb2listw(style = "W")`). A kNN definition is scale-stable and avoids an arbitrary distance-band cutoff, which at typical field spacings is wide enough to dilute local autocorrelation toward zero. Because Moran's I is sensitive to the neighbour definition, read the reported value as *the residual autocorrelation under this fixed 8-NN weighting*. Duplicated coordinates are separated by a negligible, data-scaled jitter under a fixed internal seed, so the statistic is exactly reproducible between runs and the global RNG state is untouched (Section 9.2 explains why the displacement must scale with coordinate magnitude). The neighbour count is hardcoded; see Section 9.2 to change it.

  **What the table reports.** Moran's I appears with its two-sided p-value in a separate column, and hovering the I cell reveals that row's null expectation, E[I] = −1/(n − 1). Reading I against zero is wrong: under the no-autocorrelation null the statistic is centred slightly *below* zero (−0.034 at n = 30, −0.010 at n = 100), so a marginally positive I is not by itself evidence of clustered errors. The p-value comes from `spdep::moran.test` under the **normality assumption** (`randomisation = FALSE`) and is **two-sided** (a deliberate departure from spdep's one-sided default): this is a diagnostic, and strongly *negative* residual autocorrelation, a checkerboard pattern typical of over-smoothing, is as much a misspecification signal as positive clustering. When the spdep neighbour search fails and the all-pairs 1/d fallback computes I instead (possible only at n ≤ 500), that weighting carries no sampling distribution, so the p column reports `NA*` while I and E[I] are still shown.

  **`NA*` means "not computable", never "no structure".** Both Moran cells fall back to `NA*` when the statistic could not be computed at all: fewer than three cross-validated points, no coordinate columns on the cross-validation object, or a neighbour-search failure the fallback could not rescue. This is a *missing measurement*, not a finding of spatial randomness, and the run log records which condition applied.

- **Classification performance of a continuous prediction.** Observed and predicted values are binned into classes and compared as a confusion problem, reporting overall accuracy, balanced accuracy, off-by-one accuracy, Matthews correlation coefficient (Matthews 1975), unweighted Cohen's kappa (Cohen 1960) and linearly weighted kappa. For **agronomical classes** the bin intervals are left-closed `[low, high)`, identical to the map classification (`terra::classify(..., right = FALSE)`), so a value lying exactly on a class boundary receives the same class in the tables and on the map. **Quartile** binning uses the conventional right-closed intervals on the observed quartiles.

  **Quartile breaks come from the observed values and are unbounded at the ends.** The four classes are defined by the quartiles of the **observed** column, and that one set of breaks is applied to *both* vectors, because a confusion matrix requires a common class definition (breaks re-derived separately from each would compare different classes and make kappa meaningless). The outermost intervals extend to −∞ and +∞, so a prediction outside the observed range lands in Q1 or Q4 rather than being dropped. This keeps every point in the comparison, at the cost that extreme over- or under-predictions are clamped into the end classes: the matrix cannot show *how far* beyond the observed range a prediction went. Read RMSE and Bias alongside the kappa table when extrapolation is a concern.

**Residual semantics on CV failure:** all residual-based diagnostics (validation metrics, pooled CV residual variograms, Moran's I) are computed strictly from cross-validation residuals. If cross-validation fails for a locality its residuals are left empty rather than substituted with model training residuals; CV and training residuals are never mixed.

### 5.1 Directional variogram (anisotropy check)

Every variogram the prediction engines fit is **omnidirectional**: pairs are binned by separation distance regardless of orientation, which assumes geometric isotropy. Real fields often violate this, since a floodplain, a prevailing wind, a tillage direction or a geological strike can give a variable a longer correlation range along one axis than across it.

The **Directional Variogram** panel tests that assumption. Semivariance is recomputed within four angular cones at bearings of 0°, 45°, 90° and 135° measured *clockwise from north* (N-S, NE-SW, E-W, NW-SE), each with a 22.5° half-angle tolerance. The four cones tile the half-circle exactly once, so every pair contributes to exactly one direction and the four curves are disjoint subsets of the omnidirectional one, with pair counts that sum to it (an invariant pinned in the test suite). The panel can be computed on the **measured values** or on the run's **cross-validation residuals**; the residual view is the more relevant one for RK and RFK, where the residual is what actually gets kriged.

*How to read it:* curves rising to a common sill at clearly different distances indicate **geometric anisotropy** (direction-dependent range); curves plateauing at clearly different sills indicate **zonal anisotropy**. Roughly coincident curves support the isotropy assumption. Coordinates are projected to a metric CRS before the cones are formed, because a bearing measured in degrees of longitude is not a bearing on the ground.

*Scope:* this panel is **strictly diagnostic**. No prediction path consumes it and all engines remain omnidirectional, so nothing on any map changes as a result. Where pronounced anisotropy is found, the honest interpretation is that the reported ranges are directional averages and the uncertainty surface is correspondingly smoothed across directions; acting on the finding requires an anisotropic model, which Monolith does not fit.

**Pooled "Total (Combined)" diagnostics CRS.** Each locality's CV object travels in its own local UTM zone, so pooling them for the combined residual variogram and pooled Moran's I requires one common metric CRS. The pooled set is reprojected to the **auto-UTM zone of the combined centroid**, the same zone rule used for geographic input data. Web Mercator is deliberately not used: its distances are inflated by 1/cos(latitude), about 40% at 45° N, which would stretch the pooled variogram's lag axis and distort pooled Moran neighbour distances. Per-locality diagnostics are unaffected. If the per-locality CV objects cannot be combined, the pooled diagnostics show their empty state rather than a partial pool that would misrepresent a subset as the combined result.

**Pooled variance-explained scores are inflated when localities differ in level.** The "Total (Combined)" row runs `perform_cv` on the pooled residuals, so its **R² (NSE/Trad)** and **R² (Corr)** are measured against the *pooled* mean of all localities. Where localities sit at genuinely different levels (different mean salinity, different nutrient status), that between-locality variance enters the total sum of squares and the model is credited with explaining it, even where it explains nothing within any locality. The pooled row can therefore report a markedly higher R² than *every* per-locality row above it. This is an aggregation artifact, not extra skill, the same effect that makes a regression fitted across heterogeneous groups look stronger than any within-group fit. Error-scale metrics in that row (RMSE, MAE, Bias) do not depend on the choice of centre and remain directly interpretable. **Judge model skill on the per-locality rows** and read the pooled row as a summary of overall error magnitude. A within-locality-centred variant is deliberately not computed: it would be a third quantity to explain, and the per-locality rows already answer the question.

---

## 6. Residual Analysis

Metrics summarize global performance; residual analysis localizes model failure.

### 6.1 Interpolated delta (surface difference)

The actual measured data and the uploaded predicted data are interpolated into two separate surfaces with the chosen method, then subtracted: <i>Surface<sub>Actual</sub> - Surface<sub>Predicted</sub></i>.

**Use case:** maps the net difference between the two surfaces, revealing broad regional zones where pre-calculated machine-learning predictions consistently over- or under-predict the true distribution.

### 6.2 Point errors and interpolated point errors

The discrete error at each sampling location (<i>O<sub>i</sub> - P<sub>i</sub></i>) is drawn as coloured point markers (the Map Viewer's Point Residuals panel, exported as the Point Error Map). An IDW interpolation of those error values produces the Interpolated Point Errors Map in the Export Panel. Uploaded point coordinates are projected into the locality's working metric CRS before this step, so the error surface is valid for geographic uploads exactly as for projected ones.

**Use case:** shows the spatial structure of local model failure, where the model in question is the one that produced the uploaded predictions, not Monolith's spatial interpolator. Hotspots mark zones where that prediction model cannot capture the true variability.

---

## 7. Uncertainty Analysis & Confidence Mapping

Uncertainty analysis quantifies the reliability of the prediction at every pixel. It is available for the kriging methods only (OK, RK, RFK, CK), which provide a formal statistical error model. IDW and TPS are deterministic weighting and smoothing interpolators with no prediction-variance model, so a run using either has no uncertainty product.

### 7.1 Theoretical basis

Kriging uncertainty is a function of the **spatial configuration** of the samples and the **variogram model**.
* **Geometric influence:** uncertainty is lowest at sample locations and rises into unsampled territory.
* **Variogram influence:** a high nugget or a short range raises uncertainty across the whole map.

### 7.2 Uncertainty metrics

* **Kriging variance:** the theoretical mean squared error of the prediction, in the *squared* units of the variable (for example (mg/kg)² for potassium). Because of the squaring its legend values are much larger than the variable's range: an SE of 130 mg/kg corresponds to a variance of about 17,000 (mg/kg)². Useful for comparing the relative stability of different variogram models.
* **Standard error:** the square root of the variance, in the variable's own units.
* **Use case:** a point predicting 2.0% nitrogen with a standard error of 0.2 puts the true value approximately within 1.6% to 2.4% at 95% confidence.
* **Display:** uncertainty layers always render with a continuous colour scale and the legend states the metric and its unit (for example "Variance: K (mg/kg)²"). Agronomic and binned class breaks are defined for concentrations and are not applied to uncertainty surfaces.

### 7.3 Hybrid model uncertainty (RK & RFK)

For RK and RFK the uncertainty surface combines two terms: **trend uncertainty**, the error in the relationship between the target and the predictors, and **residual uncertainty**, the kriging error of the unexplained variation. The Total map is their sum.

**The Total is an additive sum, assuming zero trend-residual covariance.** The combined variance is `Var(trend) + Var(residual kriging)`, which treats the trend-estimation error and the kriged-residual error as independent. This is the conventional two-step regression-kriging approximation: the trend is fitted first and its residuals kriged separately, so the two error terms are treated as additive. The exact joint treatment, carrying the covariance between the mean-surface estimate and the residual prediction explicitly, is Kriging with External Drift / Universal Kriging, which is not what the two-step engine does. Interpret the Total surface as approximate rather than exact.

**The trend-uncertainty term differs between RK and RFK.** For RK it is the parametric standard error of the linear-model prediction (`se.fit²`), the variance of the estimated mean surface. For RFK no closed form exists, so the term is selectable through the **RFK Uncertainty Method** control:
* **Infinitesimal jackknife (default, calibrated):** the estimator of Wager, Hastie & Efron (2014) with the Monte-Carlo bias correction. It is the random-forest analogue of RK's `se.fit²`, the sampling variance of the *ensemble-mean* prediction, and the better-calibrated trend-uncertainty term.
* **Ensemble spread (fast):** the between-tree variance, the spread of individual trees around the ensemble mean. This reflects model *instability* rather than a formal predictive variance and typically **understates** true predictive uncertainty; treat it as a relative "where is the model least stable?" measure, not an absolute interval.

Switching methods changes **only** the RFK uncertainty surface. The prediction map and the cross-validation metrics are identical either way.

**The kriged covariate surfaces are treated as error-free.** The trend term at a grid cell is evaluated on covariate values that are themselves kriging estimates (Section 8.1). Those surfaces carry their own kriging variance, but it is discarded: the trend variance in the Total is computed **as if the covariate values at each cell were measured without error**. The term a full error budget would add, roughly `Σ (∂trend/∂xⱼ)² · Var(x̂ⱼ)` plus covariate cross-terms, is therefore missing, and the reported RK/RFK uncertainty is **optimistic away from the sample points**, where the covariate surfaces are least certain. The effect is smallest near samples, where covariate kriging variance approaches its nugget, and grows into unsampled territory, which is exactly where the uncertainty map matters most. This is standard practice for two-step regression kriging with interpolated covariates and is the reason the Total surface should be read as a *relative* reliability map rather than a calibrated absolute interval. It does not apply where covariates come from an exhaustive raster such as a DEM.

**The RFK trend forest is not hyperparameter-tuned.** It is grown with `randomForest`'s regression defaults apart from the tree count, which is pinned at `ntree = 200`: `mtry = max(⌊p/3⌋, 1)` and `nodesize = 5`, with `importance = TRUE` and `keep.inbag = TRUE` for the variable-importance panel and the jackknife term. No search over `mtry`, `nodesize` or depth is performed and none adapts to sample size. This is a deliberate asymmetry with the Classification Suite (Section 10.6), whose learners do have a tuning registry: the RFK trend is one component of a two-step estimator whose residual variogram is refitted inside every cross-validation fold, so an inner tuning search would multiply an already heavy cost, and untuned forest defaults are the standard reference configuration in the regression-kriging literature (Hengl et al. 2015). RFK's trend is therefore a *reasonable default* forest rather than an optimized one; where the covariate set is large relative to n, `mtry = p/3` may be conservative. Adding trend tuning would be a design change, not a parameter tweak.

**RFK residuals are out-of-bag while the trend surface is in-bag.** The residuals whose variogram RFK fits and kriges come from the forest's **out-of-bag** predictions (`randomForest$predicted`), whereas the trend surface at grid cells uses the **full in-bag ensemble**. This asymmetry is deliberate and standard (Hengl et al. 2015): in-bag training residuals are near zero by construction, so a residual variogram fitted to them would be pure nugget and residual kriging would contribute nothing. Two consequences follow. First, `trend + kriged residual` is not an exact decomposition of the measured value at a sample point, because the two terms come from different ensembles of trees. Second, out-of-bag residuals are slightly inflated relative to the in-bag fit, so the residual variogram's sill, and with it the residual component of the uncertainty, is conservative. RK has no equivalent asymmetry.

**Covariate surfaces are fitted independently of target missingness.** The covariate surfaces RK and RFK evaluate their trend on are kriged from the **full covariate-complete point set** (co-located points deduplicated, samples with a missing target retained), with lag width and cutoff from that same set. The target model is fitted on the point set for its own surface (target-`NA` rows removed first, then deduplicated), with lags derived from *that* set, so the two lag definitions can differ slightly. This is deliberate: a sample lacking a laboratory value for the target still carries valid covariate measurements, and discarding it would needlessly weaken the covariate surfaces. Because the bounding-box diagonal drives the cutoff the difference is usually negligible.

Both maps stay labelled "Variance" and "SE" for UI consistency, and RK versus RFK magnitudes are not directly comparable. Quantile regression forests are deliberately **not** offered: a QRF predictive interval already contains the irreducible residual scatter that this engine models separately through residual kriging, so summing the two under the additive decomposition would double-count that variance.

---

## 8. Data Analytics & PCA Protocols

### 8.1 Multicollinearity filter (PCA and spatial models)

Severe multicollinearity destabilizes multivariate models, so three gates screen for it.

* **PCA module:** before execution the numerical matrix is scanned for pairwise correlations above 0.95. If any are found, execution halts and the user must drop variables or force the run. This prevents severe distortion of the loading vectors.
* **Geostatistical engine (RK, RFK, CK):** a variance inflation factor check runs on the selected auxiliary variables before the interpolation launches. If any VIF exceeds 10, a modal halts the process and offers "Auto-Drop and Continue" or a forced run. Auto-dropped variables are purged from both the interpolation algorithms and the downstream diagnostics (variable-importance plots included). The choice is honoured end to end: keeping collinear covariates disables iterative pruning inside the engines entirely, in the interpolation and classification paths alike, and the collinearity is still reported.
* **Classification suite:** the same iterative-VIF engine screens the selected numeric covariates, with a live warning under the covariate picker and the same modal at run time. The threshold is method-aware: 10 for most learners, tightening to 5 when Random Forest is selected (Section 10.8 explains why).

**Constant covariates are always excluded**, independently of the Drop/Keep answer, because a constant carries no regression information and makes the correlation matrix singular on its own. "Constant" is judged **relative to each covariate's own magnitude** (standard deviation below about 10⁻⁸ of the column's largest absolute value), never against an absolute variance floor. An absolute threshold would silently discard legitimately small-unit covariates: clay as a 0-1 fraction, normalized indices, ratios, or variables in km or Mg can all carry real signal at variances below 10⁻⁶, and since constants are dropped even under "Keep All" the user's override could not rescue them. The relative criterion drops only columns whose values differ in noise digits.

* *Covariate surfaces:* the gate is resolved **before** the covariates are interpolated onto the prediction grid, so a dropped covariate is never kriged. It is evaluated separately for the Actual and Predicted surfaces, on exactly the point sets each engine fits, because the two can retain different samples and therefore different covariate sets; the grid carries the union of the two retained sets. This changes only which surfaces are built, never the fitted model.
* *Co-Kriging:* CK applies the same gate on the same point sets but does not consume kriged covariate grids, since it predicts target and covariates jointly through the LMC. The gate matters most here: the LMC fits a direct variogram for every covariate **plus every cross-variogram**, and collinear covariates drive the coregionalization matrices toward singularity, making `fit.lmc()` fail and dropping the run into the Ordinary Kriging fallback. A CK run whose covariates trip the threshold therefore gives different, better-conditioned results depending on the Drop/Keep answer.
* *Singular-matrix fallback:* the VIF is obtained by inverting the covariate correlation matrix. When the covariates are so tightly related that inversion fails outright, no VIF exists, and the engine falls back to the pairwise correlation matrix: it takes the most strongly correlated pair and drops the member with the **higher mean absolute correlation against all remaining covariates**, the more globally redundant of the two, repeating until the matrix inverts. Exact ties break on the alphabetically later variable name. Both rules make the outcome a property of the data alone rather than of column order: dropping whichever member appeared first among the columns would let a re-ordered upload change which covariate survives, and with it the fitted model. This path is reached only when `solve()` genuinely fails.
* *Cross-validation:* the prune runs **once on the full auxiliary set before cross-validation**, not inside each fold. This does not inflate the reported skill, because the filter is **unsupervised**: it inspects only the covariate-covariate correlation matrix and never sees the target, so unlike supervised feature selection it introduces no leakage when performed outside the resampling loop. Keeping the retained set fixed across folds also keeps the model definition and its diagnostics interpretable.
* *Context sensitivity:* correlation structure is scope-dependent, and two covariates can be nearly collinear across the whole dataset yet independent within one locality, or the reverse. All three screens therefore run on the data the model will actually fit: the interpolation gate and the sidebar **Predictor Ranks (Correlation)** list use the localities selected in the Context panel (ranks are stamped with their scope and sample count), and the classification gate uses the module's resolved spatial scope. A recorded Drop/Keep decision resets whenever the method, the covariate set or the locality selection changes.

### 8.2 Multivariate outlier screening (Mahalanobis distance)

The PCA panel's outlier diagnostic computes the Mahalanobis distance of each observation in the space of the retained principal components. Because PC scores are uncorrelated by construction their covariance is the diagonal matrix of squared component standard deviations, and the distance over the retained components equals the full-space Mahalanobis distance restricted to that subspace. Components with numerically zero variance, which appear when PCA is force-executed on collinear inputs, are excluded from both the distance and the degrees of freedom of the &chi;<sup>2</sup> reference line (97.5% quantile, df = number of retained components).

**The estimator is the classical one, and that is a real limitation.** The centre and scatter are the ordinary sample mean and covariance, which the candidate outliers themselves help define. A single extreme point inflates the covariance in its own direction, and a small cluster of outliers can inflate it enough to pull their distances back under the threshold. This is the classical **masking** effect. The standard remedy is a high-breakdown estimator such as the minimum covariance determinant (Rousseeuw & Van Driessen 1999), which the panel deliberately does not use, since it would introduce a further modelling dependency for a screening plot. The panel names its estimator in the title and subtitle. Read a flagged point as "worth inspecting", and treat an *unflagged* point in a suspicious cluster with caution rather than as evidence of normality.

### 8.3 Partial correlations

A partial correlation removes the linear influence of *k* control variables from both variables before measuring what remains. The estimator follows the **`ppcor` conventions** (Kim 2015), which the reported p-values also assume, and is method-dependent:

* **Pearson:** the raw values are regressed on the controls and the product-moment correlation of the residuals is reported. This is algebraically identical to inverting the Pearson correlation matrix of all involved variables.
* **Spearman:** every variable, targets *and* controls, is **rank-transformed first**, and the partial correlation is the product-moment correlation of the *rank* residuals. A Spearman correlation of raw-value residuals is not a partial rank correlation, because the residualization it inherits is a least-squares fit on untransformed values. Since Spearman's coefficient is by definition the Pearson correlation of ranks, the partial version must residualize the ranks.
* **Kendall:** no residualization analogue exists for tau, so the coefficients come from inverting the Kendall tau matrix, &rho;<sub>ij·rest</sub> = −P<sub>ij</sub> / &radic;(P<sub>ii</sub>P<sub>jj</sub>) with **P** the inverse tau matrix. For a single control this reduces to Kendall's classical first-order partial tau.

Significance uses the partial degrees of freedom *n* − 2 − *k* (Pearson and Spearman t-statistic) or a normal approximation with effective sample size *n* − *k* (Kendall). A variable listed both as a target and as a control is silently removed from the control set, since controlling for itself would yield null residuals. If the residualization or matrix inversion fails, the table aborts with an explicit message rather than falling back to raw correlations under a "partial" label.

### 8.4 Spatial cross-correlogram

The cross-correlation of two variables **as a function of the distance separating sample points**. Both are standardized to zero mean and unit variance, and the empirical cross variogram &gamma;<sub>12</sub>(*h*) uses the same distance binning as every other variogram in the application (Section 4.1's lag logic, over projected metric coordinates):

<div style="text-align:center;"><i>&gamma;<sub>12</sub>(h) = (1 / 2N(h)) &sum; [Z<sub>1</sub>(x<sub>i</sub>) − Z<sub>1</sub>(x<sub>i</sub> + h)][Z<sub>2</sub>(x<sub>i</sub>) − Z<sub>2</sub>(x<sub>i</sub> + h)]</i></div>

Under second-order stationarity the cross variogram and cross-covariance are related by &gamma;<sub>12</sub>(*h*) = C<sub>12</sub>(0) − [C<sub>12</sub>(*h*) + C<sub>21</sub>(*h*)]/2, so for standardized variables the plotted spatial cross-correlation is

<div style="text-align:center;"><i>&rho;<sub>12</sub>(h) = r<sub>12</sub> − &gamma;<sub>12</sub>(h)</i></div>

with *r*<sub>12</sub> the ordinary non-spatial correlation, drawn as a dashed reference line. See Goovaerts (1997, Section 4.2.3) and Isaaks & Srivastava (1989, Ch. 4).

**How to read it.** A curve starting near *r*<sub>12</sub> at short lags and decaying toward zero means the two variables co-vary *locally*, sharing spatial structure at the scale where the curve stays above zero, which is the co-regionalization Co-Kriging exploits. A flat curve at roughly *r*<sub>12</sub> across all lags means the association carries no distance structure; a flat curve near zero means the variables are spatially unrelated at the sampled scales. The distance at which the curve reaches zero is the practical range of the cross-structure.

**Caveats.**
* Only the **symmetric** part of the cross-covariance is estimable. Bins are omnidirectional, so the pairs (*i*,*j*) and (*j*,*i*) fall in the same bin: unlike a time series, an unordered point set has no leads and lags and there is no negative-lag half of the plot. Directional cross-structure is not resolved; Section 5.1 covers the univariate anisotropy diagnostic.
* Each point is a bin average whose reliability follows its pair count. Bins with fewer than 30 pairs are greyed out, the conventional minimum (Journel & Huijbregts 1978), and point size encodes the count.
* Sampling noise can push an individual bin marginally outside [−1, 1]; these are moment estimates, not constrained coefficients.
* With `Spearman` or `Kendall` selected, both variables are rank-transformed before standardization and the curve is a rank-based cross-correlogram, labelled as such. Kendall has no distance-binned analogue of its own.
* Coordinates are deduplicated at centimetre precision after projection, the same convention the interpolation point sets use, so co-located samples do not stack the shortest bin.

### 8.5 PCA contribution and quality of representation (cos<sup>2</sup>)

Both panels are built from the **variable coordinates** *c*<sub>jk</sub> = *v*<sub>jk</sub> &middot; *s*<sub>k</sub>, where *v*<sub>jk</sub> is the loading of variable *j* on component *k* and *s*<sub>k</sub> that component's standard deviation.

* **Contribution** is the share of a *component* attributable to a variable: *v*<sub>jk</sub><sup>2</sup> &times; 100, summing to 100% over the variables of a component, since eigenvectors are unit-length in both PCA modes. The dashed reference line marks the uniform expectation 100/*p*.
* **cos<sup>2</sup>** is the share of a *variable* captured by the selected components: &sum;<sub>k &isin; axes</sub> *c*<sub>jk</sub><sup>2</sup> &divide; &sum;<sub>k</sub> *c*<sub>jk</sub><sup>2</sup>. The denominator is the variable's total variance, since &sum;<sub>k</sub> *c*<sub>jk</sub><sup>2</sup> = (**V S**<sup>2</sup> **V**<sup>T</sup>)<sub>jj</sub> = Var(*x*<sub>j</sub>), so the value is bounded in [0, 1] and reaches 1 across all components. This is the standard definition (Abdi & Williams 2010; `FactoMineR` / `factoextra`).

**Why the denominator matters.** For a correlation-matrix PCA (**Scale & Center Data** checked) every variable has unit variance, the denominator is 1, and the normalisation changes nothing. For a covariance-matrix PCA it is essential: the unnormalised sum is an absolute variance in the variable's own squared units, so a variable in mg&nbsp;kg<sup>-1</sup> and one on a 0-1 scale would be plotted against each other on an axis labelled cos<sup>2</sup> where the taller bar means nothing but the larger unit. With the normalisation, cos<sup>2</sup> answers the same question in both modes.

**What scaling still changes.** Contribution remains scale-sensitive by definition, and in an unscaled run it ranks variables largely by raw variance; the module says so inline. cos<sup>2</sup> is per-variable and therefore comparable, but the *components* it is measured against are themselves dominated by the high-variance variables in an unscaled run, so a low cos<sup>2</sup> there means "this variable is not what these components describe", not "this variable is unstructured".

### 8.6 Group-comparison tests and post-hoc letters

The descriptive module annotates grouped plots with one significance test at a time (**None** by default): one-way ANOVA (reported as *F*, df and *p* rather than letters), Tukey's HSD (Tukey 1949), Duncan's multiple range test (Duncan 1955), or Kruskal-Wallis (Kruskal & Wallis 1952). The control is single-select because only one test is ever applied, and letters come from `agricolae`.

* **Tukey's HSD** is the conservative default: it controls the **family-wise** error rate across all pairwise comparisons of *k* groups.
* **Duncan's MRT** is labelled *liberal*. It controls only the **comparison-wise** error rate, so with *k* groups its effective family-wise rate grows toward 1 and it declares more differences than Tukey on the same data. It is offered because a large body of older agronomy literature reports it and reproducing that work requires it, not as an equivalent alternative. Where a result turns on a difference Duncan finds and Tukey does not, report both.
* **Kruskal-Wallis** is the non-parametric route for data violating the ANOVA assumptions (the normality indicator beside the control flags this). Its pairwise post-hoc comparisons are **Benjamini-Hochberg** adjusted (Benjamini & Hochberg 1995), consistent with the FDR policy used in the correlation table.
* Letters are computed on the plotted subset (the current locality or scope selection and any facet), so they describe exactly the data shown.

### 8.7 Missing values: complete cases or pairwise

Which samples a correlation is computed on is decided by what the result is used for, and the rule is the same throughout the app.

**Anything that is a matrix uses complete cases**, the rows with no missing value in *any* selected variable, controls included. This covers the correlation heatmap, network and correlogram, partial correlations, PCA, the Mahalanobis screen, the governing-factors forest and the VIF stage of the collinearity gate. Pairwise deletion would estimate each cell from a different subsample, and a matrix assembled that way is not guaranteed positive semi-definite: partial correlation inverts it and can return |r| > 1 or fail to solve, PCA can return negative eigenvalues, `diag(solve(R))` can return negative VIFs, and the heatmap's clustering order is built on 1 − |r| as a dissimilarity. Every cell would also carry its own *n* and degrees of freedom, so no single sample size and no comparable set of p-values would exist for the panel. Each of these panels states the sample it used and how many rows it dropped.

**Standalone bivariate screens use pairwise deletion**, every sample where *both* variables are present, so a variable measured on part of the dataset is still screened on what it has. This covers the sidebar **Predictor Ranks (Correlation)**, where each rank carries its own *n* beside its coefficient, and the pairwise-threshold stage of the collinearity gate, where each pair is read against the threshold on its own and nothing inverts the matrix. A pair with fewer than three shared samples is skipped rather than reported.

Neither rule is a treatment for missing data. If complete-case deletion removes a large share of the samples, that is information about the dataset, not a display artifact: narrow the variable set, or impute deliberately before uploading.

---

## 9. Hardcoded Parameters and Where to Change Them

Certain statistical constants are fixed to keep automated execution predictable. Advanced users can change them in the source.

### 9.1 Cross-validation random seed

Every fold assignment, both the random k-fold partition and the Spatial Block k-means clustering, uses `CV_FOLD_SEED = 12345`. Section 5 covers how to read a single fold realization and what the optional Repeated CV table adds.

* **Where:** `make_cv_folds` in `spatial_metrics.R` is the single source of fold assignments for all engines and for the IDW power search (Section 4.2); `perform_kriging_loocv` additionally seeds its per-fold random forest draws for RFK. Both go through the two-sided RNG sandbox `with_seed()` (`spatial_vgm.R`), which restores the caller's random state, so seeding a helper never perturbs anything downstream.
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

* **Reproducibility.** Both `classInt` styles draw random numbers internally (k-means starts; Jenks silently switches to an unseeded 3,000-value sample above n = 3,000), so the same map could legitimately produce different class limits on every restyle. `calc_class_breaks` runs under the app's two-sided seed sandbox (seed `12345`, caller RNG restored), making the breaks bit-reproducible.
* **Tractability.** The exact Jenks algorithm (Jenks 1967) is O(n²) and blocks for seconds on raster-sized vectors, so breaks are estimated on a seeded subsample capped at `max_n = 5000` cells. Estimating breaks from a sample is standard GIS practice, and the class *areas* reported in the Scientific Analysis tab are always computed by classifying the **full-resolution** raster with those breaks. The interactive Leaflet viewer may display a mean-aggregated preview above about 500,000 cells; this is display-only.
* **Where:** the `max_n = 5000L` default of `calc_class_breaks`, and `LEAFLET_DISPLAY_MAX_CELLS <- 5e5` in `server_setup.R`.
* **Commit semantics.** Agronomical sub-settings (algorithm, class count, supervised limits) are staged in the sidebar and take effect when **APPLY TO MAPS & STATS** is pressed, so the break computation runs once per commit rather than on every input tick. The computation itself is unaffected by the staging.

### 9.5 Class zones as vector polygons

The **Export Class Zones** button writes the displayed classified surface as a GIS vector layer (`build_class_zone_sf`, `spatial_pipeline.R`). It is a change of representation, not of method, and is built from the same two calls the on-screen map and the Area Coverage table already make.

* **Geometry.** `terra::classify(rcl_mat, right = FALSE)`, the identical classification including the left-closed interval convention, followed by `terra::as.polygons(dissolve = TRUE)`. One feature per class present in the surface. Polygon boundaries are therefore **cell boundaries**: the zones inherit the interpolation grid resolution and are not smoothed, contoured or generalised, because any of those would move the boundary away from where the model places it.
* **Area.** Taken from `terra::expanse(unit = "ha", byValue = TRUE)`, the function the Area Coverage table calls, rather than measured off the exported geometry, so the exported `area_ha` equals the reported hectares by construction. `expanse` measures on the ellipsoid rather than in grid units, so a class covering thirty 100 × 100 m cells reports about 30.02 ha, not exactly 30.
* **Coordinate reference system.** Shapefile and GeoPackage keep the run's projected analysis CRS, so a GIS works in the projection the reported areas were computed in. GeoJSON and KML are reprojected to WGS84, which those formats require by specification. A GIS that re-measures the polygons planimetrically will differ from `area_ha` by the projection's own area distortion (order 0.01% for a UTM zone), because `expanse` measures on the ellipsoid; the attribute, not the re-measurement, is the number the app reports.
* **Open ends.** The outer breaks are `-Inf` and `Inf` by construction. No GIS attribute field can hold an infinity, so the `class_min` of the first class and the `class_max` of the last are written as NA rather than as a fabricated finite limit.
* **KML attribute carriage.** KML stores only a name and a description per placemark, GDAL's `KML` driver writes nothing else, and the `LIBKML` driver that would support arbitrary fields is absent from the GDAL distributed with **sf** on Windows. The export therefore places the class label in the placemark name and serialises the full attribute record (`class`, `class_min`, `class_max`, `area_ha`, `surface`, `variable`, `method`) into the description, so nothing is silently dropped. Use GeoPackage or Shapefile when the attributes must arrive as queryable fields.

---

## 10. Supervised Classification Suite

The Classification Suite is a separate modelling paradigm from the interpolation engines: instead of predicting a continuous surface and thresholding it into agronomic zones, it trains a **multiclass classifier** directly on a categorical target using co-sampled covariates, in the tradition of categorical digital soil mapping. The engine lives in `classif_helpers.R` and is decoupled from the Shiny UI.

### 10.1 Targets and learners

The target is either an existing categorical column (soil class, land-use label) or a continuous variable discretised into ordered classes with `classInt` break styles (quantile, equal-interval, or Jenks), using left-closed `[low, high)` intervals for consistency with the app's agronomic-class convention. **Numeric columns are never auto-detected as categorical:** coarse-resolution environmental covariates such as climate-raster precipitation metrics legitimately carry very few distinct values, and treating an ordered quantity as nominal both dummy-encodes it in the recipe and switches its grid transfer from kriging to nearest-neighbour assignment. Numeric class codes should be recoded to text or run through the binned-target mode.

Three learners share a common tidymodels (`parsnip` / `workflows`) backbone:
* **Multinomial logistic regression** (`nnet`), the parametric baseline. For **two-class targets** the engine substitutes binomial logistic regression (`glm`): the multinomial model with K = 2 reduces exactly to it, and parsnip's `multinom_reg` / `nnet` wrapper produces malformed probability output in the binary case. The substitution is statistically equivalent at the default `penalty = 0`, and penalty tuning is skipped for binary targets.
* **Random Forest** (`ranger`, probability forest), the de-facto standard in categorical DSM.
* **Extreme gradient boosting** (`xgboost`).

All three share one preprocessing recipe: novel-level absorption, median and mode imputation, dummy encoding of categorical covariates, zero-variance removal, and standardisation of numeric predictors (monotonic, so it aids multinomial convergence without affecting the tree learners).

### 10.2 Spatial cross-validation and pooled metrics

The default validation is **spatial blocked CV** (`spatialsample::spatial_clustering_cv`, k-means on the projected coordinates), for the same reason Spatial Block CV exists in Section 5: random folds place a test point's near neighbours in the training set and overstate skill under spatial autocorrelation (Roberts et al. 2017; Ploton et al. 2020). Standard class-stratified random k-fold remains available for in-domain estimates. Fold assignment uses the app-wide seed (`12345`) under the two-sided RNG sandbox.

Predictions are collected **out-of-fold and pooled** before any metric is computed: each fold's model predicts hard classes *and* full class-probability vectors on its held-out points, and metrics are evaluated once on the pooled set. Pooling avoids the undefined per-fold macro-metrics that arise when a spatially contiguous fold contains a single class, and it lets every metric, probability metrics included, be evaluated once over the full class set.

Reported metrics: overall accuracy, Cohen's kappa (Cohen 1960), balanced accuracy, macro-averaged precision, recall and F1 (so minority classes are not masked), multiclass ROC AUC (Hand & Till 2001), multiclass log-loss, and the Brier score. The confusion matrix is accompanied by per-class **producer accuracy** (recall, the omission-error complement) and **user accuracy** (precision, the commission-error complement), the standard per-class report in soil and land-cover classification.

**Classes absent from a fold's training rows.** Spatial folds are not class-stratified, since the blocks are defined by geometry, so a spatially clustered class can fall entirely inside one held-out block; class-stratified random folds have the same hole for a singleton class, which necessarily lands in one fold. That fold's model is then fitted on data containing none of that class and can never predict it. The suite treats this as a property of the validation design rather than an error: the absent class receives probability **0** from that fold, which is the model's genuine posterior since a class it never saw carries no mass, the held-out samples of that class score as misses, and the run reports which classes were affected. The reported producer accuracy for such a class is therefore pessimistic by construction and the pooled log-loss carries the corresponding penalty; overall accuracy and kappa are depressed only in proportion to the affected sample count. The remedies are the usual ones: fewer classes, quantile rather than equal-interval breaks (which distribute samples evenly and rarely strand a class), standard random k-fold when an in-domain estimate is wanted, or a wider spatial scope.

**Empty target levels are dropped.** A binned target carries one level per interval, and an equal-interval or Jenks break can enclose no samples at all; a level can also be emptied by discarding rows with missing covariate values. Such a level is not a class the model can learn, so it is removed before fitting. Retaining it would leave macro recall undefined (balanced accuracy silently `NA`), add an all-zero row and column to the confusion matrix, and describe the prediction surface with a class for which no probability layer exists.

### 10.3 Prediction uncertainty: normalised Shannon entropy

The classifier analogue of the kriging-variance map is the normalised Shannon entropy of the predicted class probabilities at each grid cell:

<div style="text-align:center;"><i>H* = - &sum;<sub>k</sub> p<sub>k</sub> ln p<sub>k</sub> / ln K</i></div>

where *K* is the number of classes. *H\** = 0 means the model is certain of a single class, *H\** = 1 that the probabilities are uniform. Entropy is a property of the model's probability output, not a formal error model: read it as a relative "where is the classifier least decided?" surface.

**Colour scales are absolute by default.** Entropy and the per-class probability maps are drawn on the full [0, 1] range, so probability maps of different classes are comparable with each other and entropy is comparable between runs. The cost is that a weak model, whose probabilities stay near 1/*K* and whose entropy stays near 1, produces a map of one flat colour. Each map's subtitle states the observed range, and an opt-in stretch rescales the colours to that range. Read a near-uniform class map together with the skill metrics (§10.2) and the no-information baseline (§10.9): a single class covering the whole grid is what an unskilled classifier is supposed to produce once the smoothed covariate surface no longer crosses any decision boundary, and stretching the colour scale reveals the residual structure without making it meaningful.

**Class areas are ellipsoidal.** The per-class hectare figures come from `terra::expanse(unit = "ha", byValue = TRUE)`, the same call the interpolation Area Coverage table (§9.5) and the class-zone GIS export make, so all three rest on one convention. `expanse` measures true ground area on the ellipsoid rather than in the projection plane, and the two differ by the CRS's area scale factor <i>k</i><sup>2</sup>: about -0.08% on a UTM central meridian and +0.19% at a zone edge. A planimetric re-measurement in a GIS will therefore not reproduce these numbers exactly; the reported figure is the ground area.

### 10.4 Covariate-surface approximations

The classifier is trained at sample locations, but map prediction requires covariate values at every grid cell. Monolith derives these from the samples themselves, not from exhaustive rasters:
* **Numeric covariates** are interpolated to the grid with the same ordinary-kriging covariate builder RK and RFK use (`krige_covariates`, IDW fallback).
* **Categorical covariates** cannot be kriged; each grid cell inherits the class of its nearest training point.

Both transfers are approximations whose smoothing and blocking error propagates into the classified map, the probability surfaces and the entropy surface. Where wall-to-wall covariate rasters exist (DEM derivatives, satellite bands), sampling them at grid nodes outside the app remains the more rigorous DSM design. The cross-validated performance metrics, computed strictly at sample locations, are unaffected by this approximation.

### 10.5 Spatial scope and the prediction domain

A run can be restricted to a **spatial scope** before anything is fitted:
* **Localities**, any subset of the mapped locality or grouping column;
* **Polygons**, shapes drawn on the map or uploaded as a shapefile, either intersected with the selected localities or used alone.

Scoping is applied *before* target construction, so binned-target break points are derived from the scoped data only, and all folds, metrics and hyperparameter searches see exclusively in-scope points.

The prediction domain is built from the same scope resolution as a union of **per-locality boundaries** of the scoped points, intersected with or replaced by the polygon union when a polygon scope is active. Per-locality boundaries rather than one hull around everything keep the classifier from predicting into unsampled corridors between localities, where the covariate-surface approximations of Section 10.4 would be pure extrapolation.

**The domain is always metric.** Every quantity in this section is in metres: the boundary buffers, the grid resolution, the variogram lags of the covariate surfaces (Section 10.4), the spatial-CV block sizes (Section 10.2), and the hectare areas of Section 10.3. The Target Mapping CRS may legitimately be geographic, so a classification run projects the scoped points onto the WGS 84 UTM zone of their own centroid before any of that is computed, exactly as the interpolation pipeline does (Section 3.1), and maps, rasters and exports are produced in that projection. Without it a resolution in metres would be applied to an extent in degrees and the grid would degenerate to a single cell covering the whole bounding box.

Boundary construction follows the **same geometric conventions as the interpolation engines** (Section 3) but is configured by the suite's own Spatial Scope controls (Boundary Type, Buffer Logic, Grid Resolution), independent of the interpolation sidebar, which is hidden while the suite is active. The settings apply per locality in scope. Wrapped mode buffers each locality's concave hull, with the fixed distance or dynamically from that locality's mean nearest-neighbour spacing (× 0.5 × the generic 2.0 multiplier, clamped to [5, 2000] m; the *method-specific* multipliers of Section 3.2 are an interpolation-variance concept and do not apply to a classifier). Strict mode unions fixed-radius buffers around the sample points. Grid resolution is either **Fixed** or **Auto**, the latter targeting about 50,000 cells inside the domain (clamped to [5, 1000] m). Covariate kriging fills the whole buffered domain, so buffered boundaries entail modest covariate extrapolation beyond the outermost samples, the same trade-off as the interpolation engines' buffered modes.

**Both resolution modes are floored at 4 M candidate cells.** The raster template spans the full bounding box before the boundary clip, and a multi-part scope covers far more of its box than its hulls do, so the cell count that has to be built is set by the box, not by the domain. Either mode is coarsened to $\sqrt{\Delta x\,\Delta y / 4\times10^6}$ when the requested cell size would exceed that budget, and a run that is coarsened says so. The domain itself is unchanged; only the cell size is. The grid is then built block-wise: cell centres are tested against the boundary a block at a time and the surviving centres are assembled once, so a wide bounding box is never materialised as a point layer.

### 10.6 Hyperparameter tuning

The **Hyperparameter tuning** selector sets how hard Monolith searches each learner's parameter space before the metrics of Section 10.2 are computed.

* **None (fixed defaults),** the default. Nothing is tuned; each learner is fit at hardcoded defaults (multinomial `penalty = 0`; Random Forest 500 trees with `ranger`'s built-in `mtry` and `min_n`; XGBoost 500 trees, `tree_depth = 6`, `learn_rate = 0.05`, `min_n = 2`). Fast, deterministic, and free of the optimism below.
* **Light,** a 10-point space-filling grid over a small set of high-leverage parameters: `penalty` (multinomial), `mtry` + `min_n` (Random Forest), `tree_depth` + `learn_rate` (XGBoost).
* **Full,** a 30-point grid over a wider set: the same parameters for multinomial and Random Forest, and for XGBoost additionally `mtry`, `min_n`, `loss_reduction` and `sample_size`.

At Light or Full the grid is by default evaluated over the *same* cross-validation folds and the best combination by accuracy is refitted. Because tuning and evaluation share those folds, tuned-depth metrics are mildly optimistic relative to None.

**The tuning search always uses the run's own resampling strategy.** Both the cross-validation loop and the final full-data fit build their tuning folds with the strategy selected for the run: spatial clustering under Spatial CV, class-stratified random folds under Standard CV. This matters because the final model's hyperparameters are what the class map, the entropy surface, the importance ranking and the exported `.rds` bundle are all built from. Tuning under random folds while reporting spatial-CV metrics would select the model that performs best under exactly the near-neighbour leakage spatial CV exists to remove, and every downstream product would inherit that choice while the metrics table advertised a leakage-free estimate. Runs at depth None are unaffected.

**Nested cross-validation** (the *Use nested CV (slower)* checkbox, visible only when a tuning depth is active) removes that optimism. Instead of one global selection, each **outer** fold re-runs the full grid search on **5 inner folds** built exclusively from that fold's analysis rows, so fold construction, grid evaluation and winner selection never see the outer held-out data, and the finalized workflow then predicts the held-out fold. The pooled metrics therefore estimate the generalisation error of the *entire procedure including the hyperparameter search* (Varma & Simon 2006; Cawley & Talbot 2010). Inner folds reuse the outer resampling strategy, so the inner selection faces the same leakage regime as the outer estimate, the spatial analogue of the recommendation in Schratz et al. (2019). Consequences:

* No single "best" hyperparameter set exists under nesting. The per-outer-fold winners are collected (`nested_params`, one row per fold) and their fold-to-fold agreement is itself a stability diagnostic.
* The **deployed and exported model** is still tuned once on all training data, which is standard practice: nested CV estimates the procedure's skill, the final model uses everything. The bundle records that model's own hyperparameters.
* Cost multiplies by roughly the number of inner folds. Depth None is unaffected by the checkbox.

The depth, parameter and grid-size mapping lives in `.classif_tuning_registry()` in `classif_helpers.R` and is the single place to change tuning behaviour. Changes flow automatically into both the UI selector and the fitting path.

### 10.7 Per-area performance

When the scope contains more than one locality (or polygon, in polygons-only mode), the **Performance by Area** table splits the pooled out-of-fold predictions by area and reports n, overall accuracy, Cohen's kappa, balanced accuracy and macro F1 per area, plus a Total row reproducing the pooled headline metrics. Two caveats apply:
* The fold structure is global. Spatial clusters need not respect locality borders, so a per-area subset is an *evaluation* slice of one jointly trained model, not an independently validated per-area model, unlike the interpolation engines which fit one model per locality.
* Small areas yield unstable estimates, and any metric undefined for an area (a class that never occurs there, for example) is reported as NA rather than imputed. Probability metrics are omitted from the per-area table for the same reason.

### 10.8 Permutation feature importance

Every run reports **model-agnostic permutation importance** (Breiman 2001; Fisher, Rudin & Dominici 2019): one covariate at a time is randomly shuffled, severing its association with the target while preserving its marginal distribution, the final model re-predicts, and the increase in **multiclass log-loss** over the unpermuted baseline is recorded (mean of 5 shuffles under the fixed seed). Log-loss is used rather than accuracy because it consumes the full probability vector, so it registers importance even when a permutation rarely flips the arg-max class. Because the measure is computed on the raw covariates through the entire fitted workflow, it is directly comparable across all three learners, unlike engine-native measures (ranger impurity, xgboost gain, multinomial coefficients), which live on different scales.

**Evaluation design (selectable).** The permutation can be scored on either of two row sets, chosen with *Feature importance scored on*:

* **Out-of-fold (default).** Each cross-validation fold's own fitted model permutes the predictors of the rows that fold never saw, and the per-fold log-loss increases are pooled by fold size, which reproduces exactly the increase one evaluation over the whole out-of-fold set would give. This measures importance under the same honest design as the reported accuracy, and it costs no extra model fits, since the fold models already exist inside the CV loop and the number of rows predicted is the same as in the training-row design. Caveat: a predictor is shuffled *within* each assessment set, so with very small folds the shuffle decorrelates it less thoroughly and importance is slightly understated.
* **Training rows.** The final model scored on the rows it was fitted on, the conventional default (compare `vip::vi_permute`). Rankings are informative, but absolute magnitudes lean optimistic for flexible learners that partially memorise their training set.

Both designs report the same quantity on different rows, and the design in force is written onto the plot axis, into the exported metrics CSV and into the results table, because an out-of-fold and a training-row importance are **not comparable numbers**. Negative values indicate pure noise covariates under either design.

**The choice of design cannot move the reported performance metrics.** Each fold's importance shuffles run inside their **own** seeded sandbox derived from the run seed and the fold index, so the cross-validation loop consumes exactly the same random stream whether or not importance is requested: predictions, accuracy, kappa, the confusion matrix and the per-class table are identical under both settings, and each fold's importance is independently reproducible. Without that isolation the shuffles would advance the shared stream and change the random state every later fold's model was fitted from. That is invisible for learners that fit deterministically (the multinomial model starts its weights at zero, Random Forest pins its engine seed, and XGBoost does not subsample at depths None or Light) but real at depth **Full**, where XGBoost tunes `sample_size` and `mtry` and therefore draws random row and column subsamples. A diagnostic setting must never move a performance number, so the isolation is structural rather than dependent on which learners happen to be stochastic.

**Correlated covariates split their importance** between them, since either can substitute for the other (Strobl et al. 2008), which is why the multicollinearity gate of Section 8.1 runs first: on a VIF-cleaned covariate set the reported shares are far more interpretable. The displayed *share (%)* renormalises the positive importances to 100. Because this dilution sets in well below the classical VIF > 10 cutoff, the classification module's screen is **method-aware**: with Random Forest selected the advisory threshold tightens to **VIF > 5**, the conventional moderate-collinearity bound (James et al. 2013; O'Brien 2007 discusses why such rules are advisory rather than absolute). The gate remains a user decision, since predictions are largely insensitive to keeping the covariates and only the importance interpretation suffers.

### 10.9 No-covariate baselines and covariate lift

To answer "did the covariates actually buy anything?", every run scores two reference models on the **same cross-validation folds** as the covariate model:
* **Majority-class baseline,** always predict the most frequent class. Its accuracy is the no-information rate and its kappa is 0 by construction.
* **Spatial 1-NN baseline,** each held-out point receives the class of its nearest analysis-set point (Euclidean distance, projected coordinates): the categorical analogue of nearest-neighbour interpolation, that is, what pure spatial proximity achieves with no covariate information. Under spatial blocked CV this is a demanding, honest baseline, since it must transfer across fold boundaries exactly like the model.

**Covariate lift** is the accuracy difference between the covariate model and the spatial baseline, reported in accuracy points on identical folds. Significance of the paired improvement is assessed with **McNemar's test** on the discordant pairs, the standard test for comparing two classifiers evaluated on the same samples (Dietterich 1998). Below 25 discordant pairs the exact binomial form is used, above it the continuity-corrected chi-square approximation; the discordant total is often small here, and the approximation is unreliable in that regime. A non-significant lift is a substantive scientific finding: the covariates add little beyond spatial position, and a simpler spatial model, or better covariates, should be considered. The test is reported as NA when no discordant pairs exist.

### 10.10 Confidence thresholding (abstention)

The predicted-class map supports a **confidence threshold** implementing the classical reject option (Chow 1970): grid cells whose maximum class probability falls below the threshold are assigned an explicit **Unclassified** category, rendered grey with its own row in the area table, instead of a weak arg-max guess. A 34/33/33% cell is a statement of ignorance, not a prediction, and abstained regions are natural candidates for additional field sampling.

The threshold is applied at rasterisation time in the main session: it re-classifies the existing probability surface without refitting anything, so it can be explored interactively, and the exported class GeoTIFF honours the current setting (probability and entropy rasters are never masked). Thresholds at or below 1/K cannot fire, since the maximum of a K-class probability vector is at least 1/K. Alongside the map, a CV-based readout reports the **coverage and selective-accuracy trade-off** at the chosen threshold: the fraction of pooled out-of-fold points that would be retained and the accuracy among them. CV metrics themselves are always computed on all points, without abstention.

### 10.11 Class-imbalance weighting

The optional **Balance classes** switch applies inverse-frequency case weights, *w<sub>c</sub> = n / (K · n<sub>c</sub>)*, so each class contributes equally to the training loss regardless of its sample count (King & Zeng 2001; the `class_weight="balanced"` convention), countering the tendency of accuracy-driven learners to ignore rare but important classes. Design decisions:
* Weights affect **fitting only**; all reported metrics remain unweighted, since balanced accuracy and the macro averages already expose minority-class performance. Expect weighted runs to trade some overall accuracy for better minority-class recall.
* In the out-of-fold loop, weights are **recomputed from each fold's analysis rows**, so no held-out class-prevalence information enters a fold's fit. The tuning pass, whose folds `tune_grid` controls, uses whole-training-set weights, a negligible deviation for a K-number summary.
* Weights apply to Random Forest and XGBoost. The multinomial engine does not accept case weights, and the run badge then reports "weights unsupported (unweighted)" rather than silently pretending.
* **SMOTE-style synthetic oversampling is deliberately not offered:** synthetic minority samples carry no valid spatial position, so they fabricate autocorrelation structure and break the leakage guarantees of spatial blocked CV (any resampling would also have to happen strictly inside each fold). Collecting more minority-class samples remains the only real remedy; weighting is the honest statistical stopgap.

### 10.12 Trained-model export

**Download Model (.rds)** saves the final fitted tidymodels workflow with its training metadata (method, covariates, class levels, weight usage, tuning depth and selected hyperparameters, projected CRS, n, timestamp, and the R, parsnip and engine package versions). The bundle loads in any R session with compatible package versions and applies to new covariate data without retraining:

```r
b <- readRDS("classification_model_rf_20260710.rds")
predict(b$workflow, new_data, type = "prob")   # or type = "class"
```

`new_data` must contain the covariate columns listed in `b$predictors`; the preprocessing recipe (imputation, dummy encoding, normalisation) is embedded in the workflow and replays automatically. Predictions from the exported model are identical to the in-app surface predictions, since it is the same fitted object. Note that .rds serialisation is version-sensitive for the xgboost engine: reuse the bundle under the package versions recorded in `b$versions`.

---

## 11. References

Works cited in this guide, covering the estimators and conventions Monolith implements. For the wider theory behind any method, consult the primary literature directly.

Abdi, H., & Williams, L. J. (2010). Principal component analysis. WIREs Computational Statistics, 2(4), 433-459.

Benjamini, Y., & Hochberg, Y. (1995). Controlling the false discovery rate: a practical and powerful approach to multiple testing. Journal of the Royal Statistical Society, Series B, 57(1), 289-300.

Breiman, L. (2001). Random forests. Machine Learning, 45(1), 5-32.

Cawley, G. C., & Talbot, N. L. C. (2010). On over-fitting in model selection and subsequent selection bias in performance evaluation. Journal of Machine Learning Research, 11, 2079-2107.

Chang, C.-W., Laird, D. A., Mausbach, M. J., & Hurburgh, C. R. (2001). Near-infrared reflectance spectroscopy: principal components regression analyses of soil properties. Soil Science Society of America Journal, 65(2), 480-490.

Chow, C. K. (1970). On optimum recognition error and reject tradeoff. IEEE Transactions on Information Theory, 16(1), 41-46.

Cohen, J. (1960). A coefficient of agreement for nominal scales. Educational and Psychological Measurement, 20(1), 37-46.

Dietterich, T. G. (1998). Approximate statistical tests for comparing supervised classification learning algorithms. Neural Computation, 10(7), 1895-1923.

Duncan, D. B. (1955). Multiple range and multiple F tests. Biometrics, 11(1), 1-42.

Fisher, A., Rudin, C., & Dominici, F. (2019). All models are wrong, but many are useful: learning a variable's importance by studying an entire class of prediction models simultaneously. Journal of Machine Learning Research, 20(177), 1-81.

Goovaerts, P. (1997). Geostatistics for Natural Resources Evaluation. Oxford University Press, New York.

Hand, D. J., & Till, R. J. (2001). A simple generalisation of the area under the ROC curve for multiple class classification problems. Machine Learning, 45(2), 171-186.

Hengl, T., Heuvelink, G. B. M., & Rossiter, D. G. (2007). About regression-kriging: from equations to case studies. Computers & Geosciences, 33(10), 1301-1315.

Hengl, T., Heuvelink, G. B. M., Kempen, B., Leenaars, J. G. B., Walsh, M. G., Shepherd, K. D., Sila, A., MacMillan, R. A., Mendes de Jesus, J., Tamene, L., & Tondoh, J. E. (2015). Mapping soil properties of Africa at 250 m resolution: random forests significantly improve current predictions. PLoS ONE, 10(6), e0125814.

Isaaks, E. H., & Srivastava, R. M. (1989). An Introduction to Applied Geostatistics. Oxford University Press, New York.

James, G., Witten, D., Hastie, T., & Tibshirani, R. (2013). An Introduction to Statistical Learning. Springer, New York.

Jenks, G. F. (1967). The data model concept in statistical mapping. International Yearbook of Cartography, 7, 186-190.

Journel, A. G., & Huijbregts, C. J. (1978). Mining Geostatistics. Academic Press, London.

Kim, S. (2015). ppcor: an R package for a fast calculation to semi-partial correlation coefficients. Communications for Statistical Applications and Methods, 22(6), 665-674.

King, G., & Zeng, L. (2001). Logistic regression in rare events data. Political Analysis, 9(2), 137-163.

Kohavi, R. (1995). A study of cross-validation and bootstrap for accuracy estimation and model selection. Proceedings of the 14th International Joint Conference on Artificial Intelligence, 1137-1143.

Kruskal, W. H., & Wallis, W. A. (1952). Use of ranks in one-criterion variance analysis. Journal of the American Statistical Association, 47(260), 583-621.

Lin, L. I.-K. (1989). A concordance correlation coefficient to evaluate reproducibility. Biometrics, 45(1), 255-268.

Matthews, B. W. (1975). Comparison of the predicted and observed secondary structure of T4 phage lysozyme. Biochimica et Biophysica Acta (BBA) - Protein Structure, 405(2), 442-451.

Molinaro, A. M., Simon, R., & Pfeiffer, R. M. (2005). Prediction error estimation: a comparison of resampling methods. Bioinformatics, 21(15), 3301-3307.

Moran, P. A. P. (1950). Notes on continuous stochastic phenomena. Biometrika, 37(1/2), 17-23.

Nash, J. E., & Sutcliffe, J. V. (1970). River flow forecasting through conceptual models part I: a discussion of principles. Journal of Hydrology, 10(3), 282-290.

O'Brien, R. M. (2007). A caution regarding rules of thumb for variance inflation factors. Quality & Quantity, 41(5), 673-690.

Ploton, P., Mortier, F., Réjou-Méchain, M., Barbier, N., Picard, N., Rossi, V., Dormann, C., Cornu, G., Viennois, G., Bayol, N., Lyapustin, A., Gourlet-Fleury, S., & Pélissier, R. (2020). Spatial validation reveals poor predictive performance of large-scale ecological mapping models. Nature Communications, 11, 4540.

Roberts, D. R., Bahn, V., Ciuti, S., Boyce, M. S., Elith, J., Guillera-Arroita, G., Hauenstein, S., Lahoz-Monfort, J. J., Schröder, B., Thuiller, W., Warton, D. I., Wintle, B. A., Hartig, F., & Dormann, C. F. (2017). Cross-validation strategies for data with temporal, spatial, hierarchical, or phylogenetic structure. Ecography, 40(8), 913-929.

Rousseeuw, P. J., & Van Driessen, K. (1999). A fast algorithm for the minimum covariance determinant estimator. Technometrics, 41(3), 212-223.

Schratz, P., Muenchow, J., Iturritxa, E., Richter, J., & Brenning, A. (2019). Hyperparameter tuning and performance assessment of statistical and machine-learning algorithms using spatial data. Ecological Modelling, 406, 109-120.

Strobl, C., Boulesteix, A.-L., Kneib, T., Augustin, T., & Zeileis, A. (2008). Conditional variable importance for random forests. BMC Bioinformatics, 9, 307.

Tukey, J. W. (1949). Comparing individual means in the analysis of variance. Biometrics, 5(2), 99-114.

Varma, S., & Simon, R. (2006). Bias in error estimation when using cross-validation for model selection. BMC Bioinformatics, 7, 91.

Wager, S., Hastie, T., & Efron, B. (2014). Confidence intervals for random forests: the jackknife and the infinitesimal jackknife. Journal of Machine Learning Research, 15, 1625-1651.
