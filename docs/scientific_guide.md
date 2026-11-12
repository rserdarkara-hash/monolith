# Scientific & Analytical Methodology Guide

Monolith maps and explains the spatial variability of point samples. This guide gives the mathematics of each method, the choices Monolith makes in applying it, and what the results can and cannot support.

> *Scope: the methods here (kriging variants, IDW, TPS, cross-validation metrics, PCA, statistical tests, supervised classification) are established published methods, not original contributions of this software or its author. This guide describes how Monolith implements them and the limits of that implementation; Section 12 lists the literature.*

---

## 1. Spatial Interpolation Engines

Monolith offers two deterministic methods, IDW and TPS, which weight or smooth the samples, and four geostatistical methods, OK, RK, RFK and CK, which model spatial autocorrelation through a variogram and give a prediction variance.

Kriging predictions and variances are screened for NaN and infinite values, which are mapped as `NA`. A kriging variance is also withheld (`NA`, never zero) where the variogram was not estimated from the data (Section 7.4).

### 1.1 Ordinary Kriging (OK)

The value at an unsampled location <i>x<sub>0</sub></i> is a weighted sum of the surrounding samples:

<div style="text-align:center;"><i>Z<sup>*</sup>(x<sub>0</sub>) = &sum; &lambda;<sub>i</sub> Z(x<sub>i</sub>)</i></div>

OK assumes an unknown but constant mean <i>&mu;</i>. The weights <i>&lambda;<sub>i</sub></i> minimise the estimation variance subject to <i>&sum; &lambda;<sub>i</sub> = 1</i>, and the covariances that set them come from the fitted variogram (Matheron 1963; Goovaerts 1997; fitting in Section 4.1). Every sample of the locality enters each prediction (a global neighbourhood), as in RK and RFK.

*Example:* soil pH across a fairly uniform field, where variation comes from soil-forming processes rather than abrupt topography or management.

A prediction surface that comes out entirely undefined is skipped with a warning; for a Gaussian or Matérn model with a nugget below 5% of the total sill, the warning names that as a possible cause.

### 1.2 Regression Kriging (RK)

RK splits the variable into a deterministic trend and a spatially correlated residual:

<div style="text-align:center;"><i>Z(x) = m(x) + e(x)</i></div>

An ordinary least-squares linear model (`lm`) fits the trend <i>m(x)</i> on the selected covariates, and Ordinary Kriging interpolates its residuals <i>e(x)</i>. The prediction is the trend plus the kriged residual. This is the two-step OLS-then-krige form (Hengl et al. 2007), not joint GLS or universal kriging.

*Example:* soil organic carbon, where elevation and satellite-derived moisture predict the broad trend and the kriged residuals recover local accumulations the covariates miss.

**Trend diagnostics.** For each locality's trend model the Scientific Analysis tab reports R², adjusted R², the residual standard error with its degrees of freedom, the overall F test, and a coefficient table with standard errors, t statistics, p-values and 95% confidence intervals (t distribution on the residual degrees of freedom). They describe the trend only; the cross-validation metrics in the Model Performance table measure the full RK prediction.

The OLS tests assume independent errors with constant variance. RK kriges the residuals because they are spatially correlated, and that correlation makes the standard errors too small, so significance is overstated: treat the p-values and intervals as exploratory. The Internal Residual Variogram is the variogram of these residuals; the larger the structured share of its sill, the more optimistic the table. Unbiased coefficients also require a correct trend form and exogenous covariates. The Moran's I in the Model Performance table is computed on cross-validation errors of the whole RK procedure, not on the OLS residuals, so it does not test the coefficient table.

**Minimum training size.** A trend fit needs at least the number of retained covariates plus two rows (the intercept and one residual degree of freedom). With fewer, the locality's map falls back to Ordinary Kriging, and a cross-validation fold fails by name. Enough rows do not guarantee a full-rank design: collinear covariates kept with **Keep All** can still give aliased coefficients.

**Fallback.** When RK, RFK or CK cannot be fitted for a locality (too few rows for the trend, no covariate left after screening, an LMC that cannot be fitted), the locality is mapped by Ordinary Kriging with its own automated variogram fit, never with an applied manual model, and the run warnings say so. The covariate surfaces RK and RFK evaluate their trend on are described in Section 1.7.

### 1.3 Random Forest Kriging (RFK)

RFK follows the two steps of RK with a random-forest trend in place of the linear model, which captures non-linear relationships and interactions among covariates without a parametric form. Ordinary Kriging then interpolates the forest's residuals (Hengl et al. 2015).

*Example:* crop yield across a heterogeneous landscape, where yield depends non-linearly and interactively on slope, aspect and electrical conductivity.

**The trend forest** is grown with the `randomForest` regression defaults (Liaw & Wiener 2002), `mtry = max(⌊p/3⌋, 1)` and `nodesize = 5`, with `ntree = 200`. It is not tuned: the residual variogram is refitted in every cross-validation fold, and an inner tuning search would multiply that cost (Hengl et al. 2015 also use the defaults). With many covariates relative to the sample size, `mtry = p/3` may be conservative. Each locality's map forest is seeded with 12345 and each cross-validation fold's forest with 12345 plus its fold number, so a locality's surface, variance and importance do not depend on which other localities the run includes or their order.

**Residuals.** The kriged residuals are the forest's out-of-bag residuals, while the trend surface uses the full in-bag ensemble, because in-bag residuals tend to understate the generalisation error. As a result, trend plus kriged residual does not exactly reproduce a sample's measured value, and neither the fitted sill nor the total variance is guaranteed to be conservative.

**RF Variable Importance panel.** It shows three measures for each locality's trend forest: the out-of-bag permutation importance, as the increase in out-of-bag MSE in squared target units, which orders the covariates; the same increase divided by its standard error across trees (randomForest's %IncMSE), which grows with the tree count and is not a z-score (Strobl & Zeileis 2008); and IncNodePurity, the decrease in residual sum of squares from splits on the covariate, computed on each tree's own training rows. The export has the same three columns, and Governing Factors reports the first two the same way (Section 8.8). RFK uncertainty is described in Section 7.3.

### 1.4 Co-Kriging (CK)

CK predicts a primary variable with the help of one or more secondary variables, through cross-variograms that model how the variables co-vary in space (Wackernagel 2003):

<div style="text-align:center;"><i>&gamma;<sub>12</sub>(h) = (1 / 2N(h)) &sum; [Z<sub>1</sub>(x) - Z<sub>1</sub>(x+h)][Z<sub>2</sub>(x) - Z<sub>2</sub>(x+h)]</i></div>

*Example:* nitrate from sparse laboratory samples beside a denser electrical-conductivity survey that also covers locations where nitrate was not measured.

**Heterotopic design.** CK uses every location of the locality, after co-located samples are merged (Section 1.7): the target locations, where the target is measured, and the covariate-only locations, where at least one selected covariate is measured but the target is not. Each variable is fitted on its own locations, and a covariate value at a location is the mean of every sample there that measured it, whether or not that sample carried the target. Co-kriging helps most when a covariate is sampled more densely than the target, such as an EC, NDVI or yield survey beside laboratory samples. When the covariates are measured only where the target is (an isotopic design) it seldom improves on Ordinary Kriging, and it gains nothing when all variables share one spatial structure (autokrigeability; Wackernagel 2003). The run log states each surface's design, for example `heterotopic design: target n = 80; ec: n = 300 (80 collocated)` or `isotopic design (n = 79)`, and the run configuration records it.

**Empirical variograms.** All use the primary variable's lag classes. A direct variogram uses its variable's own locations; a cross-variogram uses the locations that carry both variables. gstat's cross-variogram of variables observed at different locations, the pseudo cross-variogram <i>&frac12;E[(Z<sub>i</sub>(x+h) - Z<sub>j</sub>(x))<sup>2</sup>]</i>, is not used: it contains half the squared difference of the two means and half the variance of their difference at zero lag (Papritz, Künsch & Webster 1993), and with the target on its own scale and the covariates standardised it would mostly measure the offset between the means. Cross-variograms on shared locations keep the standard linear model of coregionalization (LMC) valid; on an isotopic data set they equal the cross-variograms of the whole data set. The Cross-Variogram panel and its export show the empirical variograms the LMC was fitted to.

**Minimum collocation.** A covariate enters only if it is measured together with the target at 10 or more locations, the minimum the directional variogram also applies; otherwise it is dropped for that surface and the run log names it with its count. The rule is checked on the map's rows; cross-validation folds do not re-apply it.

**Linear model of coregionalization.** `gstat::fit.lmc()` fits one non-nugget structure, with a common family, range and smoothness, to all direct and cross-variograms (Goovaerts 1997; Wackernagel 2003) by the weighted least-squares criterion of Section 4.1, with `fit.ranges = FALSE` (Pebesma 2004): only the sills are fitted. The shared range is the range fitted to the primary variable's own omnidirectional variogram, so read the CK range as the primary variable's range applied to the whole system, not as a range fitted for each covariate. When that fit is the stand-in of a failed fit or a pure nugget, the range falls back to half the variogram cutoff (a quarter of the bounding-box diagonal), and the run log says which. `fit.lmc()` makes each sill matrix positive semidefinite by zeroing negative eigenvalues and multiplies the direct sills by `correct.diagonal = 1.01` ([gstat documentation](https://r-spatial.github.io/gstat/reference/fit.lmc.html)); this lowers the risk of a singular system without removing it, and the correction is logged. When the LMC structure is Gaussian (or Matérn with &nu; &ge; 1) and a variable's nugget is below 5% of its sill, the run log warns that the system can be ill-conditioned (Posa 1989), more so the denser the data.

**Covariate standardisation.** Each covariate is standardised to zero mean and unit variance over the locations that measure it before the LMC is fitted. The target keeps its own scale, so predictions, metrics and plotted variograms stay in its units. Each cross-validation fold takes the means and standard deviations from its own training data.

**Covariate screen.** The constant/VIF screen (Section 8.1) runs on the locations that carry every selected covariate, covariate-only locations included. With fewer than three such locations no correlation can be estimated, and the covariates pass unscreened with a run-log note.

**Search neighbourhood.** CK solves each prediction from the nearest `nmax` observations of every variable, set by the **CK Max Neighbors** slider (5 to 60, default 15). This is a modelling choice: a small neighbourhood assumes the mean and covariance are constant only locally, which suits non-stationary fields and keeps the system small, while a large one approaches global co-kriging and suits stationary, sparsely sampled fields. The best value depends on sample density and the cross-variogram range; the default of 15 is not tuned to any data set. The value used is recorded in the run configuration. Cross-validation of CK is described in Section 5.2.

### 1.5 Inverse Distance Weighting (IDW)

A deterministic weighted mean of the nearest samples (**Max Neighbors**, 4 to 50, default 12), whose weights fall off with distance <i>d</i> raised to a power <i>p</i> (Shepard 1968):

<div style="text-align:center;"><i>&lambda;<sub>i</sub> = (1 / d<sub>i</sub><sup>p</sup>) / &sum; (1 / d<sub>i</sub><sup>p</sup>)</i></div>

*p* = 0 weights the neighbours equally (their mean); as *p* grows the nearest sample dominates, and *p* → ∞ is nearest-neighbour (Thiessen) assignment. IDW ignores clustering (redundant samples) and anisotropy and gives no prediction variance. The power is set or selected as described in Section 4.2.

*Example:* quick mapping of rainfall from a scattered gauge network, where stationarity assumptions are not needed.

### 1.6 Thin Plate Spline (TPS)

TPS minimises the squared fitting error plus lambda times the bending energy, the integral of squared second derivatives (Duchon 1977). Lambda = 0 interpolates the samples exactly; a positive lambda smooths. A smooth surface can still overshoot and extrapolate poorly. Lambda is set or selected as described in Section 4.3.

*Example:* smooth elevation or temperature gradients, where abrupt discontinuities are physically implausible.

**Fallback to IDW.** If the spline cannot be fitted for a locality (for example a degenerate point configuration), that locality is mapped by IDW with the active IDW settings. The run log records it (`TPS failed: ... Falling back to IDW.`) and a per-locality warning is raised.

### 1.7 Sample locations and covariate surfaces

**Co-located samples.** Samples whose coordinates agree to two decimals (1 cm in a metric CRS) are one location: their numeric values are averaged over the rows that carry them, their coordinates averaged, and the smallest row id and its other values kept. For a surface, rows missing its target are removed before merging. The run log counts merged locations. The Classification Suite merges at its own scope step (Section 10.2).

**Locations per surface.**

* **OK, IDW and TPS:** the locations with a measured target, or with an uploaded prediction for the Predicted surface.
* **RK and RFK:** the run's point set is the locations that carry every selected covariate; the trend and its residual variogram are fitted on those that also carry the target.
* **CK:** every location that carries the target or a selected covariate (Section 1.4).

A location without a measured target cannot enter the target's fit or its cross-validation, but its covariates still serve RK, RFK and CK, and its uploaded prediction the Predicted surface. The Map Viewer draws such locations as hollow points.

**Covariate surfaces (RK, RFK).** The trend is evaluated at grid cells on covariate surfaces kriged from the points. Each covariate is kriged with its own automated variogram fit (Section 4.1) from every location that carries all selected covariates, target-missing locations included, with lag width and cutoff from that set; the target's lags come from its own fitted set, so the two can differ slightly. When kriging fails for a covariate (a kriging error, a repeated location, or a solve that returns no prediction), it is interpolated by IDW at *p* = 2, whatever the target's IDW setting, with the run's **Max Neighbors**, and the run log says so. A covariate whose variogram is a pure nugget keeps its kriged surface, the sample mean away from sampled locations, which is the best linear unbiased predictor without spatial correlation; IDW would map noise into the covariate. The trend then cannot vary with that covariate on the map, and the run log says so. Cross-validation kriges each held-out location's covariates from this set less the held-out locations (Section 5.2); a location without a target is never held out, and the run log counts these locations. The RK and RFK variance treats the covariate surfaces as error-free (Section 7.3). CK uses no covariate grids: it predicts the covariates jointly through the LMC.

**Descriptive statistics.** The Descriptive Statistics card on the Scientific Analysis tab summarises the uploaded rows of the run's localities and data subset, co-located duplicates included, so its n and moments can differ from the interpolation point set. It is R's `summary()` (type 7 quartiles, with an NA's row when values are missing) of the measured column and, when the run mapped one, the prediction column. Governing Factors also reads the rows as uploaded (Section 8.8).

### 1.8 Actual and Predicted surfaces

A run that maps an uploaded prediction column builds a Predicted surface beside the Actual (measured) one. With **Fit Actual/Predicted Separately** ticked (the default), each surface gets its own model. Unticked, the Predicted surface reuses the model of the measured values:

* **OK:** the variogram of the measured values, which is the run's own Actual fit under Auto-Fit and the applied Actual model under Manual.
* **IDW and TPS:** the Actual setting. A Fixed power, or a Fixed or Exact lambda, is used as set; under Auto (CV) or Auto (GCV) the value is the one selected for the measured values on the same rows. The IDW fallback of a failed TPS fit follows the same rule.

In cross-validation the reused model is re-estimated in every fold from the measured values of that fold's training rows (the Auto-Fit variogram, the Auto (CV) power, the Auto (GCV) lambda), so no held-out value, measured or predicted, contributes to the model that predicts it. The run log, the selection panels and the Model Parameters export say when a Predicted surface uses the measured values' selection. Per-locality IDW powers and TPS lambdas are then set for the Actual surface only. RK, RFK and CK always fit each surface on its own.

---

## 2. Grid Resolution

The grid resolution is the spacing of the prediction points and the cell size of the output raster. Every engine predicts at cell centres (point prediction; cells are not block averages), so the cell size changes how finely the surface is drawn but no predicted value. A finer grid adds no information beyond the samples.

### 2.1 Resolution modes

* **Auto (Per Locality):** each locality is gridded at its own Auto cell size (Section 2.2), so a densely sampled locality gets a finer grid than a sparse one. Each grid starts at its own boundary's corner.
* **Auto (Global):** every locality is gridded at one cell size, the finest of the localities' own Auto sizes, computed on each locality's points and boundary as the run builds them. A coarser shared cell would merge a dense locality's samples into shared cells; a finer one costs computing time only.
* **Fixed:** the slider value, for when a set cell size is needed for cartographic consistency or printing. The slider is prefilled with a suggested value (Section 2.3).

Under Auto (Global) and Fixed, grids are snapped to a lattice of multiples of the cell size in the working CRS, so localities that share a working CRS share their cells and one GeoTIFF can hold them (Section 2.6). Localities in different UTM zones (a geographic upload spanning zones) cannot share a lattice, and a locality whose cell size the candidate-cell limit enlarged (Section 2.5) leaves it. A Target Mapping CRS that differs from the working CRS resamples every surface onto one new lattice of the same cell size (Section 3.6).

### 2.2 The Auto cell size

The Auto cell size follows the locality's sampling density (Hengl 2006):

<div style="text-align:center;"><i>R</i><sub>auto</sub> = min( 0.0791 &radic;( <i>A</i> / <i>N</i> ), <i>h</i><sub>NN</sub> / 2 ), bounded to [1, 1000] m</div>

where <i>A</i> is the boundary area, <i>N</i> the number of distinct sample locations after co-located samples are merged (Section 1.7), and <i>h</i><sub>NN</sub> their mean nearest-neighbour distance. Hengl's legible cell sizes for point samples range from 0.025 &radic;(<i>A</i>/<i>N</i>) (finest) to 0.25 &radic;(<i>A</i>/<i>N</i>) (coarsest); 0.0791 is their geometric mean, his recommended compromise, and gives about 160 cells per sample. The second term keeps the cell no coarser than half the sample spacing, the coarsest cell that does not merge neighbouring samples. It decides only for clustered designs such as transects, where samples sit much closer together than the area suggests, and the cell count can then exceed 160 per sample. <i>N</i> counts every distinct location of the locality's point set (for RK and RFK, the locations carrying every selected covariate), not only those measuring the mapped variable; where the target is sparse, this gives a finer grid. The 1 m floor is the Fixed slider's minimum.

For example, with concave-hull boundaries on the bundled sample data, Kale (79 samples, 2.71 km², <i>h</i><sub>NN</sub> = 139 m) is gridded at 14.6 m and Yorga (198 samples, 29.14 km², <i>h</i><sub>NN</sub> = 218 m) at 30.4 m; the density term decides for both.

The grid, the boundary and its buffer are built in the run's working CRS (Section 3.5), and all grid and buffer lengths are in metres.

### 2.3 The suggested Fixed resolution

The value that prefills the Fixed slider is half the mean nearest-neighbour distance, floored at 1/300 of the larger bounding-box dimension, max(0.5 <i>h</i><sub>NN</sub>, extent / 300), and clamped to [1, 500] m. Half the spacing is the coarsest legible cell for point samples (Hengl 2006); it follows sample density and does not guarantee that detail at that scale is resolved. It is measured on the points of the selected localities together, transformed to the Target Mapping CRS, and always in metres; for a geographic CRS, distances and extents are approximated through Web Mercator (EPSG:3857) scaled by cos(mean latitude). Because it describes the selected localities together rather than any one locality, the Auto modes do not use it.

### 2.4 Grid construction

Cells are square and exactly the stated size. A grid starts at the lower-left corner of the boundary's bounding box (under Fixed and Auto (Global), at the lattice point below it) and extends by whole cells until it covers the box. Cells whose centre falls outside the boundary are excluded before the engines run; since prediction is pointwise, this changes no value inside the boundary. The Map Viewer's **Cell Sizes** overlay lists the cell size used for each locality, and the run configuration records it as one value, or as a range where localities differ.

### 2.5 Candidate-cell limit

In every mode, a cell size that would create more than about 4 million candidate cells over a boundary's bounding box is coarsened to that limit, because every candidate centre is held in memory before clipping; the run log says so. Auto grids can reach the limit too, since the Auto rule counts cells inside the boundary, not its box: a point-buffer boundary fills a small part of its box, and Auto (Global) grids a large locality at a denser locality's size.

### 2.6 Several localities in one run

Each locality is interpolated on its own grid, and nothing resamples one locality onto another's cells. The Map Viewer, the figures, the class zones, the area tables and the GeoTIFFs all read these per-locality rasters.

* **GeoTIFF.** One file holds every locality only when the localities lie on one lattice (same CRS, cell size and origin, within a millionth of a cell; typically Fixed or Auto (Global) grids in a projected Target Mapping CRS), their boundaries overlap by no more than 0.5 m², and no cell is populated by two localities (a cell centre on the shared edge of adjacent boundaries belongs to both). Otherwise the export is a zip with one GeoTIFF per locality, each on its own grid and tagged with its locality.
* **Figures**, like the Map Viewer, draw each locality as its own layer under one colour scale or class scheme.
* **Class zones** are dissolved locality by locality from the rasters the Area Coverage table measures (Section 9.5); zones of overlapping localities overlap, as their maps do.
* **Pooled values.** Class breaks computed over several localities (Jenks, K-Means, the Supervised quantile defaults) and the shared colour range of **Match Scales** weight each cell value by the cell's exact ellipsoidal ground area, in projected and geographic CRSs alike (projected cells vary in ground area with the projection's scale, degree cells with latitude). Counted by cells instead, a locality gridded at 15 m would weigh its ground 36 times as much as one gridded at 90 m. Weights are dropped only when all cell areas agree within 10⁻⁹ (relative). Localities are pooled in name order, so no result depends on the order they were run in. The break algorithms are described in Section 9.1.
* **Run log.** A multi-locality run writes a `[Grid]` line saying whether its localities share one lattice, whether their boundaries overlap or share populated cells, and what a GeoTIFF export therefore holds.

---

## 3. Prediction Domain and Coordinate Systems

The boundary sets the displayed prediction domain. Buffers are geometric choices, not statistically estimated limits of reliable prediction.

### 3.1 Boundary types

The sidebar's **Boundary Type** offers four options; an uploaded boundary shapefile can replace them per locality.

* **Convex hull / Concave hull:** the convex hull encloses the samples with straight outer edges; the concave hull follows their outline more closely. No buffer is added.
* **Buffered:** the concave hull expanded outward by the buffer distance (<i>D</i><sub>b</sub>), so the map covers the field's outer edge.
* **Point buffer:** a circle of fixed radius around every sample, unioned.
* **Uploaded boundary:** used for a locality whose samples it overlaps. Features whose attribute value equals the locality's name are unioned and used; when none carries the name, the first intersecting feature not shared with another locality of the run is used (the combined hull of a point or line layer, which every locality shares, does not qualify). A feature that intersects none of the locality's samples is refused, even when its name matches, because it would move the prediction domain away from the data; the locality then uses the sidebar boundary type, and the run log says so. An attribute column of locality names makes the assignment explicit. Point and line layers are replaced by the convex hull of their features; a hull of fewer than three non-collinear vertices encloses no area and is refused. A layer without a CRS is read in the Input Data CRS, and the overlap test runs in the working CRS, where the grid is clipped.

### 3.2 Dynamic buffer (Buffered mode)

The buffer distance scales with a resolution: under Auto resolution, half the mean nearest-neighbour distance of the locality's distinct sample locations (not the Auto cell size of Section 2.2); under Fixed resolution, the slider value, so the sidebar's buffer column updates as the slider moves. That base is multiplied by a method-specific factor and clamped to [5, 2000] m:

| Method | <i>D</i><sub>b</sub> | Note |
|---|---|---|
| TPS | 1.0 &times; resolution | the smallest pad, limiting the spline's displayed extrapolation |
| IDW | 2.0 &times; resolution | with a finite neighbour count, distant predictions approach a mean of the selected neighbours, not necessarily the global mean |
| OK, CK, RK, RFK | 3.0 &times; resolution | independent of the fitted variogram range; it does not establish safe extrapolation |

The sidebar's *Grid and buffer for the next run* table computes the Auto figure as the run does, on the run's own point set (working CRS, co-located samples merged, and for RK and RFK the locations carrying every selected covariate).

### 3.3 Point buffer radius

Under **Point buffer** the **Buffer Distance (m)** is always the fixed radius around each point; no dynamic buffer applies. A dynamic buffer would exaggerate coverage here: at a 200 m resolution a 3&times; multiplier would draw 600 m circles around every point.

### 3.4 Point buffer radius and cell size

The grid keeps a cell when its **centre** falls inside the boundary. Under a Point buffer boundary, which is the union of circles of radius <i>D</i><sub>b</sub> around the samples, this ties the buffer to the cell size <i>R</i>. A sample lies anywhere in its own cell, up to half the cell diagonal, <i>R</i>/&radic;2, from the cell centre, and that centre is also its nearest cell centre. So every isolated sample keeps its own cell when

<div style="text-align:center;"><i>D</i><sub>b</sub> &ge; <i>R</i> / &radic;2 &nbsp;&nbsp;&hArr;&nbsp;&nbsp; <i>R</i> &le; <i>D</i><sub>b</sub> &radic;2</div>

Below that, the share of in-cell sample positions whose cell centre falls outside the buffer is the cell area not covered by a disc of radius <i>D</i><sub>b</sub> centred on the cell centre. With <i>t</i> = <i>D</i><sub>b</sub>/<i>R</i> (the second form subtracts the four circular segments that spill past the cell edges once <i>t</i> &gt; &frac12;):

<div style="text-align:center;"><i>f</i><sub>lost</sub> = 1 &minus; &pi;<i>t</i><sup>2</sup> &nbsp;&nbsp;&nbsp;for <i>t</i> &le; &frac12;</div>
<div style="text-align:center;"><i>f</i><sub>lost</sub> = 1 &minus; { &pi;<i>t</i><sup>2</sup> &minus; 4 [ <i>t</i><sup>2</sup> arccos(1 / 2<i>t</i>) &minus; &frac12; &radic;( <i>t</i><sup>2</sup> &minus; &frac14; ) ] } &nbsp;&nbsp;&nbsp;for &frac12; &lt; <i>t</i> &lt; 1/&radic;2</div>

<i>f</i><sub>lost</sub> falls continuously to zero at <i>t</i> = 1/&radic;2. It is the probability for an isolated sample placed uniformly within its cell, not the measured share of uploaded samples lost: samples in dense clusters are covered by their neighbours' buffers. At <i>D</i><sub>b</sub> = <i>R</i>/2 (for example a 175 m buffer on a 350 m grid) it reaches 1 &minus; &pi;/4 &asymp; 21%, which is why a valid run can show sampled points on blank map.

The run is valid either way, but its mapped support is incomplete. The Domain & Grid section flags an incoherent pair live under Fixed resolution, and every run re-checks it against the resolution actually used, with a notification and a run-log entry giving the corrective values: a buffer of &lceil;<i>R</i> / &radic;2&rceil; m, or a cell size of &lfloor;<i>D</i><sub>b</sub> &radic;2&rfloor; m where that is a settable size (at least 1 m; 5 m in the Classification Suite). A smaller cell size is usually the better correction, since a wider buffer enlarges the area the map claims to support.

The Classification Suite's Spatial Scope applies the same rule. The two grids differ only for a cell centre exactly on the boundary line: the interpolation grid keeps it (`st_intersects`), the classification grid does not (`st_within`).

Hull boundaries are not flagged, although cells along their edge can lose sampled points that lie within half a cell diagonal of the boundary. Buffered-mode buffers are not guaranteed to exceed half the cell diagonal either: in Auto mode they follow the sample spacing rather than the grid spacing, and they are capped at 2000 m.

### 3.5 Input Data CRS and the working CRS

The Input Data CRS is the system the uploaded X/Y columns were recorded in. It sets an interpolation run's **working CRS**, in which models are fitted and grids, buffers and distances are computed: when the Input Data CRS is geographic or uses a non-metre linear unit, the points are projected to the WGS 84 UTM zone of the locality's mean position; otherwise they are used as supplied, and Data Setup warns, without transforming them, when such metre-based input distorts distances by more than 1% at the data. The Target Mapping CRS is applied at the end, to reproject the finished surface (Section 3.6). The Classification Suite computes in its Target CRS (Section 10.5).

**No default zone.** The 60 UTM zones are congruent: the same (<i>E</i>, <i>N</i>) pair is a valid position in every zone, at the same latitude and a longitude 6° apart per zone. Reading zone-33 coordinates as zone 35 moves a survey 12° east, about 812 km at 52° N, with no coordinate leaving its zone's valid range and no error raised. Monolith therefore ships with both CRS selectors empty and does not run until one is chosen.

**Effect of a wrong zone.** Coordinate differences are preserved, so distances, variogram lags, fitted ranges and cross-validation metrics are identical under a wrong zone of the same hemisphere. The georeferencing is wrong: the basemap, the exported GeoTIFF and shapefile, and any polygon drawn on the map describe the wrong place. Reprojecting the output into a zone the data do not belong to also inflates distances by the point scale factor (Section 3.6), about 1.07% two zones off at 52° N.

**Identification from evidence.** Values within &plusmn;180 / &plusmn;90 only show that the numbers *could* be longitude and latitude: a local metre grid (a field plot with <i>X</i> = 2 to 48, <i>Y</i> = 5 to 60) fits the same bounds. A degree-range pair is identified as EPSG:4326 only when the X and Y column names are longitude/latitude tokens (`lon`, `long`, `lng`, `longitude` and `lat`, `latitude`), or when an uploaded boundary shapefile with a `.prj` contains more than half of the points read as degrees (with the 5% diagonal margin used below). Otherwise the user is asked to confirm EPSG:4326 or set the recorded CRS. Angular bounds cannot establish the datum: the identification assumes WGS 84 and says so, and coordinates on another geographic datum (for example ED50) need that CRS set explicitly.

For projected coordinates, a companion `lon`/`lat` pair in the same table fixes the zone, <i>z</i> = &lfloor;(&lambda; + 180) / 6&rfloor; + 1, leaving four candidates: the WGS 84 zone, its southern twin, the ETRS89 equivalent and Web Mercator. Each is scored by the largest great-circle distance between the transformed (<i>x</i>, <i>y</i>) and the reference pair. Candidates that place the data within 5 m of one another are merged first (a WGS 84 UTM zone and its ETRS89 counterpart agree to under a metre); the survivor is accepted when it reproduces the reference pair within 5 m and every other candidate is at least 100&times; worse. Without a lon/lat pair, an uploaded boundary shapefile with a `.prj` serves the same purpose: its own CRS is tested beside the zone candidates, each is scored by the share of points falling inside the boundary, and one is accepted when it holds at least half the points and no other candidate holds more than half of its share.

**Shortlist without evidence.** Failing both, Monolith asks where the data are and lists the projections that put them there. The candidates are the non-deprecated EPSG projected CRSs with an area of use in the installed PROJ catalogue; no region is assumed. A candidate is kept when its area of use contains the indicated point, the data read in it fall inside that area (which rejects national grids the data do not belong to), and that position lies within 500 km of the point (which rejects world-scale display projections). Candidates whose positions agree within 250 m are shown as one row with their equivalent codes, ordered WGS 84 first, then ETRS89, with the spread stated: datum variants of one grid differ by less than a map click can resolve (WGS 84, WGS 72 and ED50 UTM 33N place the same Potsdam coordinates within 209 m of each other). Adjacent UTM zones, about 400 km apart, are never merged. Rows are ordered by distance from the point, at most 8. A free-text search (country or region) matches the CRS name and area-of-use description: alone it returns at most 120 codes and says when it is capped; with a click it only narrows the click's list and returns an empty list rather than widening it.

Monolith sets a CRS only when the evidence thresholds above are met and never defaults to a projected zone. The position the current selection produces is always reported in degrees, so a wrong zone shows before any model is fitted.

### 3.6 Target Mapping CRS

The Target Mapping CRS is the system the finished surface is written in. It carries the exported rasters, the suggested Fixed resolution (Section 2.3) and the ruler's projected distance (Section 3.7). Models are fitted in the working CRS (Section 3.5).

**Reprojection.** When the Target Mapping CRS is the working CRS (the same system, however it is written), the surface is not resampled: it keeps the working grid and its values and takes the Target Mapping CRS definition, so an exported GeoTIFF carries that EPSG code. When it is another projected system, including the same projection on another datum, the surface is resampled bilinearly (`terra::project`) onto square cells of the run's resolution in that system's metres, on a lattice of multiples of the cell size from its coordinate origin, so localities of one cell size land on one lattice (Section 2.6). A geographic Target Mapping CRS keeps the cell size in degrees that the reprojection chooses.

> **Recommended setting for projected data:** use the UTM zone of the study area as both the Input Data CRS and the Target Mapping CRS. The working CRS is then the Target Mapping CRS: the checks below also cover the grid, buffers and variogram lags, the surface is written as computed with no final resampling, and the ruler's projected distances are directly comparable with fitted ranges.

**Metre units.** A projected Target Mapping CRS whose linear unit is not the metre is refused before a run starts, in interpolation and classification alike, and the Target CRS panel reports it as soon as it is selected: the rasters, the suggested resolution and the ruler would read feet as metres, a factor of 3.28. This rule is independent of the scale factor below (a State Plane zone in US survey feet has <i>k</i> &asymp; 1 yet fails it), and it is necessary but not sufficient: Web Mercator's unit is the metre, yet at 52° N it reports every distance 64% too long.

**Point scale factor.** A projection's local distance distortion is its point scale factor <i>k</i>, the ratio of a projected length to the true length of the same short arc on the ellipsoid. Monolith measures it numerically, so any transformable projected CRS can be screened. At each tested position it projects two arcs of 0.001° (about 111 m along a meridian), one along the parallel and one along the meridian, and compares them with their lengths on the WGS 84 ellipsoid (<i>a</i> = 6378137 m, 1/<i>f</i> = 298.257223563):

<div style="text-align:center;"><i>s</i><sub>&lambda;</sub> = <i>N</i>(&phi;) cos&phi; &Delta;&lambda;, &nbsp;&nbsp;&nbsp; <i>N</i>(&phi;) = <i>a</i> / &radic;( 1 &minus; <i>e</i><sup>2</sup> sin<sup>2</sup>&phi; )</div>

<div style="text-align:center;"><i>s</i><sub>&phi;</sub> = <i>M</i>(&phi;) &Delta;&phi;, &nbsp;&nbsp;&nbsp; <i>M</i>(&phi;) = <i>a</i>( 1 &minus; <i>e</i><sup>2</sup> ) / ( 1 &minus; <i>e</i><sup>2</sup> sin<sup>2</sup>&phi; )<sup>3/2</sup></div>

The larger |<i>k</i> &minus; 1| of the two decides. A conformal projection (transverse Mercator, Mercator, Lambert conformal conic) scales equally in every direction; a non-conformal one does not (EPSG:5070, Albers conterminous US, gives +3.17% along the parallel and &minus;3.07% along the meridian at Potsdam). The two arcs need not follow a non-conformal projection's principal directions, so the check does not bound distortion at every bearing. It is evaluated at the data centroid and the four corners of the data bounding box (the positions the Data Setup readout reports), taking the worst case, since a large study area can pass at its centre and fail at an edge.

**Thresholds.** |<i>k</i> &minus; 1| &gt; 0.1% raises a warning; &gt; 1% refuses the run unless an explicit override is given, which the run configuration records. Both thresholds are application choices: a UTM zone keeps |<i>k</i> &minus; 1| below 0.1% across its own 6° of longitude, and a 1% distortion makes a 250 m length read as 252.5 m. Measured at Potsdam (12.958° E, 52.466° N):

| CRS | <i>k</i> | <i>k</i> &minus; 1 | verdict |
|---|---|---|---|
| EPSG:32633 (UTM 33N) | 0.999836 | -0.016% | fit, the correct zone |
| EPSG:25833 (ETRS89 / UTM 33N) | 0.999836 | -0.016% | fit |
| EPSG:3035 (LAEA Europe) | 1.000113 | +0.011% | fit |
| EPSG:5514 (S-JTSK Krovak) | 1.000356 | +0.036% | outside its declared area, measures correctly |
| EPSG:32634 (UTM 34N) | 1.003260 | +0.326% | warned |
| EPSG:32635 (UTM 35N) | 1.010729 | +1.073% | refused |
| EPSG:3857 (Web Mercator) | 1.642049 | +64.2% | refused |

**Area of use.** Data outside the CRS's EPSG area of use (from PROJ's catalogue, or the WKT2 `BBOX` where the catalogue has no row; the union where a code declares several boxes) raise a warning but never refuse a run. The box is an advisory bound: a study area straddling a zone boundary sits outside one box while still measuring correctly, and EPSG:5514 above lies outside its area at Potsdam yet distorts distances by only 0.036%. Only the scale factor refuses.

**Limits of the check.** Geographic targets bypass the scale-factor check. A missing extent or a failed scale calculation can leave no warning, which is not evidence of suitability. Invalid CRSs are rejected earlier, when the CRS is parsed. The Classification Suite refuses a run with a refusal verdict outright; interpolation offers the recorded override. Web Mercator is not offered as a Target Mapping CRS; it remains available as an input CRS, and typed into the target selector it is checked like any other entry. The check decides which runs may proceed and changes no value a permitted run produces.

**Recommended zone.** With every verdict, Monolith also names a CRS fit for the data: the WGS 84 UTM zone from the mean longitude of the five tested positions, zone = &lfloor;(&lambda; + 180) / 6&rfloor; + 1, with the hemisphere from their mean latitude (EPSG 32600 + zone north, 32700 + zone south). WGS 84 / UTM is recommended everywhere rather than a national grid because, between about 80° S and 84° N, it holds |<i>k</i> &minus; 1| below 0.1% across its zone; it is not a polar-CRS selector. A national grid entered directly is judged by the same two rules as any other entry. The recommended zone is checked at the same five positions and withheld when it is no better than the current selection, as for a study area too wide for any single zone.

### 3.7 Distance measurement (ruler)

The Map Viewer's ruler measures a clicked path and reports two figures, because a distance on the curved Earth depends on the surface and metric used.

**Ground distance (WGS 84 geodesic).** The path is measured as geodesic segments on the WGS 84 ellipsoid with Karney's algorithm through `terra`: the shortest surface distance between the clicked points, independent of any projection. The area of a closed path is its ellipsoidal area, the same ground-area convention as the class areas (Section 9.4). From the third vertex the shape is closed: the reported length includes the closing segment, so it is the perimeter of the polygon whose area is shown. Two vertices give an open line and a length. A ring that crosses itself reports its perimeter but no area, because the two lobes of a figure-eight are traversed in opposite directions and their areas cancel; the crossing is detected on the clicked vertices, so it does not depend on the projection.

**Projected distance (Target Mapping CRS).** The same vertices are transformed into the Target Mapping CRS of the displayed run (not the sidebar's current setting) and measured in the plane, &sum;<sub>i</sub> &radic;( (<i>x</i><sub>i+1</sub> &minus; <i>x</i><sub>i</sub>)<sup>2</sup> + (<i>y</i><sub>i+1</sub> &minus; <i>y</i><sub>i</sub>)<sup>2</sup> ), with the area by the shoelace formula. This is the engines' own metric when the Target Mapping CRS is the working CRS (Section 3.5); where the two differ, a measured separation and a fitted range differ by the ratio of their scale factors at that place. A geographic Target Mapping CRS gives no projected figure, since a Euclidean length on longitude and latitude is not a length.

**Reading the difference.** The gap between the two figures is the projection's distance distortion at that place and bearing. In a UTM zone, <i>k</i> runs from 0.9996 on the central meridian to about 1.0010 at the zone edge, so a gap of a few hundredths of a percent is normal (the area gap is about twice the length gap, since area scales with <i>k</i><sup>2</sup>). A much larger gap means the Target Mapping CRS fits the area poorly (Section 3.6).

The measure tool's running readout uses a sphere of radius 6,371 km, which departs from the ellipsoidal values by up to about 0.5% in length and 1% in area; the finished figures replace it and are stored with the shape. A measurement records nothing, exports nothing and enters no model.

---

## 4. Model Fitting and Parameter Selection

### 4.1 Variogram fitting

The empirical semivariogram is half the mean squared difference between values separated by lag <i>h</i>:

<div style="text-align:center;"><i>&gamma;(h) = (1 / 2N(h)) &sum; [Z(x<sub>i</sub>) - Z(x<sub>i</sub> + h)]<sup>2</sup></i></div>

Kriging needs a model curve fitted to it, described by:

* **Nugget (<i>C<sub>0</sub></i>):** the intercept. In theory <i>&gamma;(0) = 0</i>, but measurement error and variation below the sample spacing create a jump at the origin; a high nugget indicates noisy data.
* **Partial sill (<i>C</i>):** the spatially structured variance. The total sill <i>C<sub>0</sub> + C</i> is the level where the variogram flattens, the a priori variance of the field.
* **Range (<i>a</i>):** a family-specific scale. The spherical model reaches its sill at *a*; exponential, Gaussian and Matérn models approach it asymptotically. The **practical range** is the distance at which the spherical model reaches its sill, or another family about 95% of its rise above the nugget: *a* for spherical, 3*a* for exponential, &radic;3&middot;*a* for Gaussian and about 4.75&middot;*a* for Matérn with &nu; = 1.5 (Goovaerts 1997). It is the range to compare between families. Beyond it covariance is negligible, which is not the same as statistical independence.
* **Structural dependency:** the partial-sill share of the total sill, <i>SD% = C / (C<sub>0</sub> + C) &times; 100</i>, the complement of the nugget-to-sill ratio of Cambardella et al. (1994): 100% is a fully structured field and 0% a pure nugget.

**Automatic fitting.** Three families are fitted, spherical (Sph), exponential (Exp) and Matérn (Mat, &nu; = 1.5), each by gstat's default weighted least-squares criterion (`fit.method = 7`; Pebesma 2004),

<div style="text-align:center;"><i>&sum;<sub>j</sub> N<sub>j</sub> / h<sub>j</sub><sup>2</sup> (&gamma;<sub>j</sub> &minus; &gamma;(h<sub>j</sub>))<sup>2</sup></i></div>

over lag bins with <i>N<sub>j</sub></i> pairs at mean distance <i>h<sub>j</sub></i>. Its weights favour well-populated short lags and, unlike the Cressie (1985) weights <i>N<sub>j</sub> / &gamma;(h<sub>j</sub>)<sup>2</sup></i> (`fit.method = 2`), do not depend on the model being fitted. The co-kriging LMC uses the same criterion. For a fixed range, the model <i>&gamma;(h) = C<sub>0</sub> + C &rho;(h/a)</i> is linear in the nugget and partial sill, so these come from a two-variable non-negative least squares (Lawson & Hanson 1974) and only the range is searched (variable projection; Golub & Pereyra 2003). The criterion is profiled over practical ranges from the largest lag / 100 to 10 &times; the largest lag at 200 log-spaced points (plus every lag distance for the spherical model, whose profile has kinks there), and every basin within 10<sup>&minus;3</sup> (relative) of the best grid value is refined by Brent's method (Brent 1973); the reported fit is taken from the equally-fitting set described below. The search uses no starting values, iteration caps or convergence flags, so the fit is a deterministic function of the empirical variogram, and since every threshold is relative, rescaling distances or values rescales the fit and changes nothing else. It is a finite search and is not proven to find the global minimum of every possible variogram, and a fit within rounding error of a threshold (such as the 10<sup>&minus;8</sup> tie band) can differ between machines or software versions. Bins with a missing semivariance, a zero distance or no pairs are dropped.

**Fitting rules.**

* The Matérn smoothness &nu; (kappa) is fixed at 1.5, the Matérn 3/2 model (Minasny & McBratney 2005), in Auto-Fit and Manual Tuning alike: a free smoothness would give the Matérn one more parameter than the other families. To probe a smoother or rougher process near the origin, compare families in Manual Tuning: the Gaussian is smoothest at the origin and the exponential roughest, with spherical and Matérn (&nu; = 1.5) between them.
* Nugget and partial sill are constrained to be non-negative; a negative nugget would mean negative semivariance near the origin.
* The Gaussian is never auto-selected and is available in Manual Tuning. It implies an analytic, infinitely differentiable field (Stein 1999), while the Matérn with &nu; = 1.5 covers smooth fields that are not analytic, and with a small nugget it tends to overshoot. This is a workflow policy; the Gaussian remains a valid model.
* A Matérn that fits as well with a zero nugget, judged over its equally-fitting set (below), is refused and contributes no candidate, even where a poorer Matérn with a nugget exists. A zero-nugget Matérn is a valid variogram, but its smooth origin makes the kriging system of closely spaced samples poorly conditioned (Posa 1989; Ababou et al. 1994), and negative kriging weights can cause overshoot even without numerical singularity (Deutsch 1996). The rule applies to every automated fit, covariate and fold fits included, and not to spherical or exponential fits or to manual models. An eligible Matérn whose nugget is positive but below 5% of its total sill is used, and the map banner flags it, as it flags an applied manual Gaussian or Matérn model in that state.

**Eligibility and choice.** A candidate is eligible when its criterion is finite, its partial sill and practical range are finite and positive, its nugget is finite and non-negative, and, for the Matérn, it passes the zero-nugget rule above. Among eligible families the lowest criterion wins, whatever its range. Only a numerical tie, criteria within 10<sup>&minus;8</sup> (relative) of the lowest, is broken: first in favour of a resolved range (below), then in the order spherical, exponential, Matérn. Tied fits need not predict alike, since kriging uses <i>&gamma;</i> at every separation, not only at the bin distances. When no family is eligible, the flat fit is used (pure nugget, below).

**Equally-fitting set.** The ranges whose criterion lies within 10<sup>&minus;8</sup> (relative) of a family's minimum form its equally-fitting set, reported as an interval of practical ranges and an interval of nugget shares (the nugget as a share of the total sill). Around an ordinary minimum the set is about 10<sup>&minus;4</sup> wide in log range, and the fit is its midpoint. The set can be wide: when only the first lag lies inside a spherical range, the model has just two fitted values, that lag and a plateau at the weighted mean of the others, so every range from where the nugget reaches zero up to the second lag distance fits identically while the nugget slides. The binned variogram cannot locate the nugget within that interval, so Monolith uses the member in the middle of the nugget interval (minimax for <i>&gamma;</i> near the origin, where the members differ most) and reports the interval. When the nugget-share interval is wider than one percentage point, the nugget is *not identified*: Structural Dependency is reported as an interval ("Not reliably identified (70.2 to 100%)"), the subtitle states the nugget interval and the map banner names the locality. More lags at short separations, or replicate samples (which bound the nugget from below by the measurement error), would identify it.

The equally-fitting set holds the fits this criterion cannot distinguish on the chosen bins; it is not a confidence interval. A narrow set does not show that the data estimate a parameter precisely, since noisy semivariances can fit one sharp minimum while the parameters stay very uncertain. It concerns the selected family only. Variogram uncertainty is not propagated into the kriging variance (Section 7.2).

**Range and sill resolution.** The practical range is *resolved* when it lies strictly between 1/100 and 2 times the largest empirical lag distance and the equally-fitting set stays clear of the search floor, a heuristic about lag support that makes no confidence statement. A range outside that window is reported and never penalised. For a field with a large-scale trend, <i>Z(s) = m(s) + &epsilon;(s)</i>, the raw variogram contains the trend as well as the residual covariance,

<div style="text-align:center;"><i>&gamma;<sub>Z</sub>(h) &asymp; &gamma;<sub>&epsilon;</sub>(h) + &frac12; E[(m(s+h) &minus; m(s))<sup>2</sup>]</i></div>

so it can keep rising to the cutoff, and a sill not reached within the lags is ambiguous between a long-range stationary process and non-stationarity. Monolith keeps the best fit and qualifies what it reports about it. The **sill** counts as resolved only when the practical range is at most the largest lag, a stricter test than the range window. Otherwise Structural Dependency and the Practical Range cell read "Not reliably identified", with the extrapolated figure on hover; the exported variogram sheet keeps the number and adds `Max Lag` and `Sill Resolved` (blank for a manual model).

**Range ceiling.** The search stops at a practical range of 10 &times; the largest lag. The ceiling is a numerical bound that the data do not determine: it binds only where the empirical variogram keeps rising, and such a variogram describes a trend as much as a covariance (even a linear mean <i>m(s) = b'&middot;s</i> adds <i>&frac12;(b'&middot;h)<sup>2</sup></i> to the raw variogram). A fit at the ceiling is reported as such: the map banner says the range reached the search limit, the subtitle and the Practical Range cell mark it, and the export sets `Range At Search Limit`.

**Variogram states.** Every fit carries one state, listed worst first. The map banner reports every state but `ok`, each variogram panel's subtitle names the stand-in of a failed fit, and the run log lists the cross-validation folds whose state is not `ok` (Section 5.2).

| State | Meaning |
|---|---|
| `fit_failed` | fewer than five usable lag bins, or every semivariance zero (a constant target, or a fold whose training values are all equal): nothing to fit, so a spherical stand-in seeded from the sample variance is used (80% partial sill, 20% nugget, range half the largest lag; sill 1 when the variance is zero). It kriges a constant exactly; its predictions are kept and its kriging variance withheld (Section 7.4) |
| `pure_nugget` | no eligible structured model (every family flat, or a zero-nugget Matérn refused): the flat fit |
| `range_unresolved` | the practical range lies outside the window the lags resolve, or the equally-fitting set reaches the search floor |
| `ok` | the range is inside that window |

**Pure nugget.** When no family is eligible the fit is flat: a pure nugget at the weighted mean semivariance. Kriging then predicts the sample mean away from the samples, the best linear unbiased predictor without spatial correlation. The fit records which of two causes led there: every family's best fit was flat (no partial sill), or the only structured fit was a Matérn at a zero nugget, refused by the rule above. In the first case the map banner says *no spatial structure detected*, with the number of pairs per lag; in the second it says *pure nugget used* and names the refused Matérn. On an RK or RFK residual variogram the first case reads *no residual spatial structure detected by the variogram fit*, and the map is the trend plus the mean residual. The flat fit is a model selection with no significance level, and it misses structure more easily the fewer pairs each lag holds (pairs that share a sample are dependent, so pair counts are a diagnostic, not a sample size). Because the weights favour the shortest lags, a variogram that rises only at long separations can come out flat.

**Rising-variogram advisory.** When the sill is not resolved, or the fit is a pure nugget, and the empirical variogram is still rising near the cutoff (the mean of the last third of the bins exceeds the mean of the third before it by more than 5% of the observed <i>&gamma;</i> span), the map shows an advisory: the range and sill are weakly constrained, which can indicate either long-range dependence or non-stationarity; the app does not detect non-stationarity. The advice depends on what the variogram was fitted to:

* On a value-scale variogram (OK, and a locality whose RK, RFK or CK fell back to OK): model the systematic trend with covariates and fit the variogram to the residuals, the regression-kriging design.
* On an RK or RFK residual variogram: the fitted trend has not removed the large-scale structure, for one of three reasons the variogram cannot separate: covariates that do not carry the large-scale variation, a trend form that cannot express the relationship, or a correlation length longer than the locality's sampled extent. The last has no remedy within the data, since the lag cutoff is fixed at half the bounding-box diagonal, and changing covariates only until the advisory disappears would be fitting noise.

A co-kriging LMC is not screened. The advisory qualifies interpretation only: the prediction is still the best linear unbiased one under the assumed model, and the cross-validation metrics measure its accuracy.

**Target with no usable variance.** A constant target has a zero empirical variogram and gets the failed-fit stand-in, which kriges the constant exactly. A target that varies only in noise digits (standard deviation at or below 10<sup>&minus;8</sup> of the largest absolute value, the covariate screen's rule) gives parameters that describe the numerical noise floor. Neither set of parameters is reported: the Variogram Parameters card states the cause, the export leaves the parameter columns blank and names the cause under *Fit Status*, and the variogram panels show the empirical points without a curve. RK and RFK residual variograms are judged against the target's magnitude, not their own, because residuals centre on zero: the residuals of such a target, or of a trend that reproduces the target within that floor, are rounding. No kriging variance is reported from any of these fits (Section 7.4). A variable whose spread sits just above the floor has a small but real variance and is reported normally.

**Manual Tuning.** Manual Tuning lets you favour the fit at short lags, which carry the most weight in kriging, or apply a Gaussian model. The sliders are scaled to the tuned locality's data: nugget and partial sill from 0 to twice the larger of the sample variance and the stored total sill; the range from about 1/1000 of its maximum to the larger of 1.5 &times; the bounding-box diagonal and 3 &times; the stored range; about 200 steps each. Where the variance is so small that a sill step would fall below 10<sup>&minus;6</sup>, a notification asks for the variable to be rescaled in the uploaded file. The preview reports the same weighted criterion as Auto-Fit. A model with zero nugget and zero partial sill has no covariance and is refused; any other manual model is used as applied. A Gaussian or Matérn model with a nugget below 5% of its total sill is flagged when applied, in the variogram subtitle and on the map; the threshold is advisory and does not bound overshoot.

In **Manual** mode only models saved with **Apply manual model** are used; a locality without one fits its own variogram in the run, with the per-fold refit of Auto-Fit (Section 5.2). A stored model belongs to the variable and data subset it was tuned on and is offered only for that combination. Uploading data, reassigning localities, or changing the X, Y, locality or Input CRS mapping clears every stored model. An applied model is replaced only by another applied model, by **OPTIMIZE ALL VARIOGRAMS**, by that clearing, or by **Load config**; no interpolation run overwrites it.

### 4.2 IDW power selection

**Modes.** The power *p* is **Fixed** (default 2; any typed value from 0 to 48, where 0 gives equal weights) or **Auto (CV)**, set for all localities or, under **Per locality**, for one locality and surface. A Fixed power is not chosen on the data, so its cross-validation carries no selection optimism.

**Auto (CV).** The map uses the candidate with the lowest pooled cross-validation RMSE among 37 candidates: equal weights (*p* = 0, the plain mean of the **Max Neighbors** nearest samples), 0.25 to 6 in steps of 0.25, then 7, 8, 9, 10, 12, 14, 16, 20, 24, 32 and 48, and the nearest-neighbour limit (*p* → ∞, each location takes its nearest sample's value). The steps widen above 6, where the RMSE profile flattens; at *p* = 48 a neighbour 1.5 times as far as the nearest keeps a (1/1.5)<sup>48</sup> ≈ 4 × 10⁻⁹ share of its weight, so the next candidate is the limit. Candidates are scored on the locality's samples under the sidebar **Cross-Validation Strategy** (Section 5), with one fold assignment for all candidates, so the comparison is paired; ties go to the smallest power.

**Nested in the reported cross-validation.** Every outer fold re-selects the power from its own training samples (inner folds under the same strategy and seed; under kNNDM, matched to the same map locations, Section 5.1) before predicting its held-out samples, so no held-out value helps choose the power that predicts it. The map power and the fold powers (median [min–max]) are reported in Regional Parameters, the run log and the **IDW Power Selection** panel, which plots the pooled CV RMSE of every candidate. Fold powers far apart, possibly at an end of the range, indicate a flat profile, and the reported cross-validation includes the cost of that instability. The profile scores each power held fixed in every fold, so its minimum is not the reported cross-validation, which includes the selection.

**Computation.** Candidates are scored with an exact nearest-neighbour IDW computation that reproduces gstat's IDW cross-validation to rounding; the map and every reported prediction come from `gstat::idw()` at the selected power (at the nearest-neighbour limit, from the single nearest sample). On a lattice, equidistant neighbours can be ranked differently from gstat; this can move only a near-tied selection, or, at the nearest-neighbour limit, make the profile's score differ from the reported cross-validation, and affects no reported prediction.

**Ends of the range.** A selection at either end is a finding about the data.

* *Equal weights:* distance weighting did not improve on the plain mean of the **Max Neighbors** nearest samples at this spacing. The map is that moving average, whose smoothing **Max Neighbors** sets (a single value over the whole map where it reaches every sample, as the note says); Ordinary Kriging would model such short-range variation as a nugget.
* *The nearest-neighbour limit:* each held-out sample was predicted best by its nearest sample alone. The map is a stepped (Thiessen) surface, raised as a warning in the run log and the progress panel; Ordinary Kriging or TPS give a continuous surface that honours the same short-range continuity.
* A selection from *p* = 24 up is read the same way and raises the same warning: a sample 10% farther than the nearest then keeps about a tenth of the nearest's weight ((1/1.1)<sup>24</sup> ≈ 0.10), so the map is practically stepped.

**Powers the data cannot separate.** A candidate is *within one standard error of the best* when its mean squared CV error exceeds the best one's by no more than one standard error of the per-sample squared-error differences on the same folds (a paired comparison). This set need not follow the height of the curve: a power close to the best but consistently slightly worse can fall outside it, while one farther off whose errors differ erratically stays inside. The **IDW Power Selection** panel draws the candidates inside it filled and the others hollow, and the panel and the run log state how many there are and whether *p* = 2 is among them (if it is, the data do not distinguish *p* = 2 from the selected power); the IDW Power CV Profile and Model Parameters exports carry the same flags. The standard error treats the samples as independent, so with spatially correlated errors more powers are indistinguishable than marked. The readout selects nothing; the selection is always the minimum.

**Other rules.**

* A row set with fewer than 5 samples, a locality or a fold's training set, is not searched and uses *p* = 2; so is a target without usable variance. The run log, Regional Parameters and the Model Parameters export say so for the map's selection, and a fold in that state appears among the fold powers.
* A per-locality value belongs to the variable and data subset it was applied for and is used only for that combination, and only under **Per locality**; under **All localities** every locality uses the sidebar setting. The surface switch is available wherever a Predicted surface is computed with **Fit Actual/Predicted Separately** ticked (Section 1.8).
* Selection and interpolation use the locality's metric working CRS (Section 3.5); distances are planar, not geodesic.

### 4.3 TPS smoothing selection

**Modes.** Lambda balances fidelity to individual samples against a smooth surface. It is **Auto (GCV)** (default), **Exact** (lambda = 0, an exact interpolator with zero error at the samples) or **Fixed** (any lambda > 0), set for all localities or, under **Per locality**, for one locality and surface, with the same scoping rules as the IDW power (Section 4.2).

**Auto (GCV).** Generalized cross-validation in `fields::Tps` (Craven & Wahba 1978; Nychka et al. 2021) chooses lambda on the locality's samples for the map, and again on each fold's training samples in cross-validation, so the reported metrics include the selection. A Fixed or Exact lambda is used unchanged in every fold. Every Auto run records the GCV curve of the fit that set its lambda; the **TPS Smoothing Selection (GCV)** panel plots it on a log axis with the fitted lambda marked. `fields` optimises lambda continuously and the curve is its coarse evaluation grid, so the mark can fall between grid points.

**Scale of lambda.** Both coordinate axes are divided by the larger coordinate extent, and `fields::Tps(scale.type = "unscaled")` keeps that common scale, so the spline is isotropic and rotates with the data. Lambda acts on these scaled coordinates; GCV typically selects values between about 10⁻⁶ and 10⁻¹. Bending energy scales with the square of the extent it is measured over, so the smoothing in ground units is lambda &times; s², s being the larger coordinate extent: two localities' lambdas are comparable only after that rescaling. Every fold keeps the full locality's coordinate scaling.

**Reported fit.** Regional Parameters gives the fitted lambda and the effective degrees of freedom of each surface. An effective df below 3.5 raises a near-planar warning (a TPS plane has df 3), naming GCV or fixed smoothing; it is a descriptive flag, and no test is made. Parameter exports store the mode separately from the full-precision lambda and effective df and, for a GCV selection, the values it was made on.

**Ends of the GCV grid.** `fields` evaluates GCV at 200 lambdas spaced evenly in effective df, from 3 (the least-squares plane) to 0.95*n* (the least smoothing), and refines the minimum between grid points.

* A minimum at the **plane end** is the smoothest member of the spline family: at this sampling, GCV selects no structure beyond a linear trend (a selection, which does not show that none exists), and the map is that trend (the near-planar warning says so).
* A minimum at the **least-smoothing end** has exact interpolation (lambda = 0) beyond it, which GCV cannot score (its residuals and trace term both vanish, 0/0). The run then cross-validates exact interpolation on the same folds and reports both RMSEs in the run log, the **TPS Smoothing Selection** panel and the Model Parameters export; where exact interpolation predicts the held-out samples better, a warning recommends **Exact (λ = 0)**. The run keeps GCV's lambda either way: choosing between the two on the folds that score them would add selection optimism. Where either cross-validation could not score every sample, no comparison is made and the note suggests running **Exact (λ = 0)** to compare.

Geographic or non-metre input is projected to its UTM zone first (Section 3.5).

### 4.4 Pooled and directional variograms

**Total (Combined) variograms.** With *Filter Analysis View* on **Total (Combined)**, every variogram panel (the Actual and Predicted Data Structure panels, the pooled CV-residual and internal-residual variograms, and the directional variogram) shows the **pooled within-locality variogram**: the classical (Matheron) estimator over the point pairs *within* each locality, on common lag classes,

<div style="text-align:center;"><i>&gamma;&#770;(h<sub>k</sub>) = &sum;<sub>l</sub> N<sub>l</sub>(h<sub>k</sub>) &gamma;&#770;<sub>l</sub>(h<sub>k</sub>) / &sum;<sub>l</sub> N<sub>l</sub>(h<sub>k</sub>)</i></div>

plotted at the pair-weighted mean distance of each class: the average variogram of McBratney & Pringle (1999). No pair joins two localities; a single variogram over the pooled points would bin pairs separated by the distance between fields, often tens of kilometres, and its long lags would describe differences between fields rather than any structure a model used. The common lag classes are the lag width and cutoff of the eligible locality with the smallest cutoff, so every locality contributes across the whole plotted range. A locality enters with at least 10 distinct located values, and the subtitle names any locality left out. Each locality is projected to its own metric CRS and its co-located samples merged first. The pooled CV-residual variogram carries a fitted curve: the automated fit of Section 4.1 on the pooled empirical variogram, with the residuals, centred on their own locality's mean, supplying the sample variance. The other pooled panels show the empirical points only.

**Directional variogram.** Every variogram the engines fit is omnidirectional, which assumes isotropy. Real fields often violate this: a floodplain, a prevailing wind, a tillage direction or a geological strike can make the correlation range longer along one axis than across it. The **Directional Variogram** panel checks the assumption. Semivariance is recomputed in four cones at bearings of 0°, 45°, 90° and 135° clockwise from north (N-S, NE-SW, E-W, NW-SE), each with a 22.5° half-angle tolerance. The cones tile the half-circle exactly once, so every pair falls in one direction and the four pair counts sum to the omnidirectional one. Coordinates are projected to a metric CRS first, since a bearing in degrees of longitude is not a bearing on the ground. On Total (Combined) each cone is pooled within localities on the common lag classes above.

* **Data** selects the measured values or, for a run that computed a Predicted surface, the uploaded prediction column (an Actual-only run has no switch). A model's output is usually smoother than the measurements it approximates, so comparing the two shows whether the prediction model kept the directional structure of the field.
* **Computed on** selects those values or the cross-validation residuals of that surface. The residual view is offered only when cross-validation of that surface completed; otherwise the panel says the residuals are unavailable rather than showing another surface's errors. For RK and RFK it shows errors of the full procedure, not the training-trend residuals the engine kriges, which have their own internal-residual panel.

*Reading it:* curves rising to a common sill at clearly different distances indicate **geometric anisotropy** (a direction-dependent range); curves levelling off at clearly different sills indicate **zonal anisotropy**; roughly coincident curves support isotropy. The panel is diagnostic only: the engines stay omnidirectional and no map changes. With pronounced anisotropy, read the reported ranges as directional averages and the uncertainty surface as smoothed across directions; modelling it needs an anisotropic model, which Monolith does not fit.

---

## 5. Cross-Validation and Performance Metrics

Cross-validation holds samples out, predicts them from the rest, and turns the predicted-versus-observed pairs (<i>P<sub>i</sub></i>, <i>O<sub>i</sub></i>) into the performance metrics. The **Cross-Validation Strategy** control decides how the folds are formed. It changes the reported metrics and leaves the interpolated surface as it is, with one exception: IDW **Auto (CV)** selects its power under the same strategy (Section 4.2), so a different strategy can select a different power and change the IDW map.

* **Auto (default):** leave-one-out cross-validation (LOOCV) at 50 or fewer observations; a seeded, balanced random 10-fold above 50.
* **kNNDM (Map-Matched):** ten folds whose held-out-to-training distances are matched to the distances from the map's cells to the samples (k-fold nearest-neighbour distance matching; Linnenbrink, Milà, Ludwig & Meyer 2024); Section 5.1. Below 30 observations it falls back to LOOCV.
* **Standard LOOCV:** leave-one-out at any sample size; one fit per sample, so slower than ten folds.
* **Spatial Block CV:** ten spatially contiguous folds formed by k-means clustering of the sample coordinates. There is no separation buffer, so adjacent blocks can still hold close neighbours. Below 30 observations it falls back to LOOCV.

All fold assignments use a fixed seed, `CV_FOLD_SEED = 12345` (Section 11).

**Choosing a design.** LOOCV and random k-fold hold samples out the way the map interpolates: each held-out point keeps its near neighbours in the training data. They estimate the accuracy of interpolation within the sampled domain, approximately without bias for a simple random sample (Wadoux, Heuvelink, de Bruin & Brus 2021). For a regular (grid) sample they are conservative: a held-out grid point is a full grid spacing from its nearest training sample, while no map cell is farther from a sample than half the diagonal of a grid square (about 0.7 of the spacing), and no fold design can hold a sample out closer to the training data than its nearest neighbour. Spatial Block CV holds out contiguous regions, so it estimates accuracy at larger distances than the map's own: the question for transfer to unsampled areas, or for a clustered sample whose gaps the map has to bridge (Roberts et al. 2017; Ploton et al. 2020), and a pessimistic answer for a map within the sampled field. With a clustered sample, random folds overstate accuracy in the gaps between clusters. The design whose held-out distances match the distances at which the map predicts answers the question asked; kNNDM builds that design from the samples and the boundary (Section 5.1).

### 5.1 kNNDM: folds matched to the map

A cross-validation error estimates the map's error when held-out samples are predicted from the distances at which the map predicts its cells (Milà, Mateu, Pebesma & Meyer 2022). kNNDM (Linnenbrink et al. 2024) builds its k = 10 folds to that end by comparing three nearest-neighbour distance distributions: from the map's locations to the samples, between the samples, and from each held-out sample to the training samples of its fold. The map's locations are about 1000 points of a regular lattice inside the locality's boundary, so the folds follow the boundary and its buffer but not the grid's cell size.

1. **Gate.** When the map's distances are not stochastically larger than the sample spacing (one-sided Kolmogorov-Smirnov test, p ≥ 0.05), random folds already match the map, and kNNDM returns the seeded random 10-fold that Auto uses above 50 samples.
2. **Candidates.** Otherwise the samples are clustered hierarchically (Ward's criterion, `ward.D2`; Murtagh & Legendre 2014) and the tree is cut into q clusters for 100 values of q, spaced logarithmically from 10 to n − 2. The clusters are ordered along the first principal axis of the sample coordinates; a cluster of at least n/10 samples becomes a fold of its own, and the others are dealt in turn over the remaining folds, so a fold gathers clusters from across the locality. A partition with a fold of more than half the samples is discarded.
3. **Choice.** The partition whose held-out distances are closest to the map's, by the Wasserstein distance W (the area between the two cumulative distributions, in map units), is kept. The seeded random 10-fold competes as well, and a cluster partition must have a strictly smaller W to replace it, so kNNDM never keeps folds that match the map worse than Auto's random folds. This is an addition to the reference implementation, which considers random folds only at the gate. Folds are chosen on coordinates only, never on measured values.

Coarse cuts give block-like folds and fine cuts approach random folds, so the design moves between the two with the data. Above 2000 samples, the tree is built on at most 2000 square cells of samples, weighted by their sizes, which gives exactly Ward's merges above the cell level (Lance & Williams 1967), with 30 candidate cuts beside the random partition. A cluster partition draws no random numbers and the principal axis's sign is fixed, so the folds are the same on every platform, and Repeated CV runs them once (Section 5.4). The fold count, the half-sample cap and the 1000 lattice points are the defaults of the reference implementation (CAST).

The Model Performance row states whether kNNDM chose random or spatial folds, and the **CV Distance Match** panel draws the three distributions, with W for the run's folds and for random 10-fold, under any strategy. Residuals of spatial folds are read like Spatial Block residuals (Section 5.6). With IDW Auto (CV), kNNDM also forms the inner folds of the nested power selection in every outer fold, while the map's own selection uses the reference folds.

**Limits.** No fold design can hold a sample out closer to the training data than its nearest neighbour: on a regular grid kNNDM returns random folds and is as conservative as they are. Nor can it create distances the samples do not contain: where the boundary reaches far beyond the samples every design stays optimistic, and W measures the remaining gap. It matches the nearest-neighbour distance only, not the whole neighbourhood a kriging prediction uses.

**Point buffers.** Under a Point buffer boundary (Section 3.1) the map predicts only within the buffer radius of a sample, usually closer to the samples than they lie to each other. The gate then finds random folds sufficient and kNNDM returns them; like every design, they are conservative for such a map, and the panel shows the held-out curve to the right of the map curve. Spatial folds arise where the boundary makes the map predict beyond the sample spacing: clustered samples, transects, or a hull or buffer drawn around sparse, unevenly spread samples. For a sparse or uneven locality, a point buffer keeps the map near the data and its random folds describe it conservatively, while a hull extends the map into unsampled ground and kNNDM's spatial folds report that larger error.

### 5.2 What each fold re-estimates

In each fold, every fitted component is re-estimated from that fold's training samples by the code the map uses. The held-out samples contribute their coordinates only, which is all the map has at a grid node.

* **OK** re-derives the lag geometry, the empirical variogram and the full automated fit (with **Fit Actual/Predicted Separately** unticked, see Section 1.8). An OK variogram saved with **Apply manual model** is reused unchanged in every fold, since it records a judgement that cannot be remade on a subset; that locality's row is labelled *conditional on applied variogram*.
* **RK and RFK** re-run the covariate screen, krige the held-out locations' covariates from the covariate locations of the map less the held-out ones (Section 1.7), using the map's IDW fallback where needed, and refit the trend (`lm` or `randomForest`) and the residual variogram on the fold's training samples. The run log counts, per covariate, the folds that used the fallback and those whose covariate variogram was a pure nugget. Each RFK fold grows its forest from the seed `CV_FOLD_SEED` + its fold number. Residual variograms are always auto-fitted; they never use an applied OK model.
* **CK** removes the whole held-out location, the target and its co-located covariate values, and re-runs the covariate screen, the standardisation, the lag geometry, the primary variogram, the LMC family, its shared range and the coregionalization sills. Covariate-only locations stay in every fold's training data, since they carry no target to score. CK is scored on every location with a measured target, the population Ordinary Kriging scores under *Native* (Section 5.3). A fold whose LMC cannot be fitted contributes no prediction and shows in the coverage (Section 5.3).
* **The OK fallback** of a failed RK, RFK or CK behaves as OK and never reuses an applied model.
* **IDW Auto (CV)** and **TPS Auto (GCV)** re-select the power or lambda in every fold; a Fixed power and a Fixed or Exact lambda are settings and stay fixed (Sections 4.2, 4.3).

The covariate screen runs inside each fold because cross-validation estimates the error of a procedure only when every data-driven step of it is repeated on the training data (Hastie, Tibshirani & Friedman 2009, §7.10.2; Varma & Simon 2006; Cawley & Talbot 2010). Otherwise the held-out samples' measured covariates would help decide which covariates predict them, while an unsampled grid cell has no measured covariates. A fold can therefore keep a different covariate set from the map; the run log names the covariates and the count, and the note under Model Performance says so.

Settings are not estimated and stay fixed: the global neighbourhood of OK, RK and RFK, the CK neighbourhood, the RFK tree count and the collinearity threshold. TPS folds keep the full locality's coordinate scaling. Cross-validation results are conditional on the selected population, covariates and settings.

**Fold variogram states.** OK, RK and RFK record each completed fold's variogram state (Section 4.1). The run log names the locality and each fold whose state is not `ok`, and the note under Model Performance gives their count; a fold's state describes the fit it was scored with and can differ from the map's. CK records instead whether a fold's LMC range came from the extent fallback because its primary variogram had no usable range, and the run log counts those folds. A fold kriged with a failed-fit stand-in, or with a fit to values constant up to rounding, still contributes predictions to the metrics, with `NA` kriging variance (Section 7.4).

### 5.3 Cross-validation population and coverage

A cross-validation holds one row per sample of its population. The Model Performance table states which population that is, how many of its samples received a prediction, and the identifier of the experiment.

* **Native** (Ordinary Kriging default): the deduplicated samples carrying the current surface's target, measured values for Actual and uploaded predictions for Predicted.
* **Comparable** (the *CV Population* switch, Ordinary Kriging only): the samples carrying the target and every selected covariate, deduplicated and folded as RK and RFK fold them, so OK is trained and scored on the same samples and folds as those engines (it still uses no covariate values). The OK map uses every sample either way. RK and RFK always use these rows, since their trend needs every covariate at each sample; CK is scored on the Native population (Section 5.2).
* **CV population ID:** eight hexadecimal characters (a short MD5 hash) identifying the target column with its data subset, the uploaded-table row numbers scored, and the fold each was assigned to. Matching IDs mean matching target keys, rows and partitions, subject to collisions in this short hash; they do not hash data values or model settings, so compare them only within one unchanged upload. The ID is shown on hovering the *Source* cell and written to the metrics export. Spatial Block folds are built from the population's own coordinates, so Native and Comparable fold differently. kNNDM folds also follow the boundary the map is clipped to, so runs with different boundaries or buffers fold the same samples differently and are different experiments with different IDs.
* **Coverage:** a fold that fails contributes `NA` predictions for its samples and a line in the run log; nothing is filled in from another engine. The table reports *n = predicted of expected*, marks a row below 100% **INCOMPLETE**, and computes every metric on the predicted samples only, so such a row describes the samples it names, not the whole locality. A kriging cross-validation that fails entirely reports 0 of its expected samples, and the pooled *Total (Combined)* row counts them as expected too.
* A population with fewer than 3 samples is mapped and its cross-validation skipped, with a named log line.

**Residuals.** Every validation diagnostic (the metrics, the pooled CV-residual variograms, Moran's I, the CV point overlay) uses cross-validation residuals only, joined to the samples by the cross-validation object's own coordinates; training residuals are never substituted. (The internal RK and RFK residual variograms use the training-trend residuals by design.) A sample the cross-validation did not score carries no residual, and a cross-validation that failed outright leaves the locality without residuals.

### 5.4 Repeated cross-validation

By default every metric comes from one fold assignment. Random k-fold, Spatial Block and kNNDM's random folds depend on a random split, and at moderate sample sizes the spread of a metric across splits can be as large as the difference between two methods. The fixed seed makes method comparisons paired, which removes fold luck from the comparison but not from each absolute number.

The **Repeated CV** checkbox (3, 5 or 10 realizations) re-runs each engine's cross-validation under further fold assignments (seeds `CV_FOLD_SEED + 1`, `+ 2`, ...) and adds a **Fold-Realization Stability** table that reports each error metric as mean ± standard deviation across realizations, the usual presentation of resampling-based skill (Kohavi 1995; Molinaro et al. 2005).

* Realization 1 is the reference run, so the Model Performance table, the residual maps, the CV-residual write-back and the surface are the same with the option on or off.
* Only the partition varies. Data, variogram policy, neighbourhood and trend settings stay fixed, and fitted models change with the training rows. RFK fold forests are seeded from their fold number, not the realization, so the SD isolates fold-assignment variance.
* Deterministic plans run once. Leave-one-out (Standard LOOCV, Auto at n ≤ 50, Spatial Block and kNNDM below 30 samples) and kNNDM's spatial folds draw no random numbers, so such a locality contributes a single realization, the run log says so, and pooled rows carry it unchanged into every repeat. Where kNNDM's first realization is random folds, each repeat re-runs the whole choice with its own seed's random partition as the candidate, and can return the spatial partition.
* Read the SD as the resolution of a comparison: a difference smaller than it may be unstable. It is not a significance test, which would need the paired differences and their dependence. The sample count is shown as a range where realizations differ.
* Each repeat costs one more full cross-validation per locality and surface; kriging engines refit in every fold, so five realizations take about five times the cross-validation time. Surfaces are computed once.
* Moran's I is reported for realization 1 only.

### 5.5 Error metrics

* **RMSE** (root mean square error):
  <div style="text-align:center;"><i>RMSE = &radic;( &sum; (P<sub>i</sub> - O<sub>i</sub>)<sup>2</sup> / n )</i></div>
  The error in the variable's units; smaller is better. <strong>MAE</strong>, the mean absolute error &sum;|P<sub>i</sub> − O<sub>i</sub>| / n, is less sensitive to large errors.
* **NRMSE (mean, %)**, also called CV(RMSE):
  <div style="text-align:center;"><i>NRMSE (mean) = RMSE / |O<sub>mean</sub>| &times; 100</i></div>
  The error as a percentage of the <strong>absolute</strong> mean, comparable across variables on different scales; the absolute value keeps it positive for negative-mean variables. NA when the mean is 0, and not reported when the observed values span zero (below). <strong>NMAE (mean, %)</strong>, in the uploaded-prediction table only, is <i>MAE / |O<sub>mean</sub>| &times; 100</i> under the same rules.
* **NRMSE (SD):**
  <div style="text-align:center;"><i>NRMSE (SD) = RMSE / SD<sub>actual</sub></i></div>
  The error as a multiple of the target's spread, a ratio. Since <i>SD(y + c) = SD(y)</i>, it does not depend on where the variable's zero lies, so it stays meaningful for centred, anomaly and other interval-scale variables. NA when the observations have no spread. With RMSE on divisor <i>n</i> and SD on <i>n − 1</i> it equals <i>&radic;( (1 − NSE)(n − 1) / n )</i> and is the reciprocal of RPD, so it adds no evidence beyond those two; it is reported because it survives recentring, which a target spanning zero needs.
* **R² (NSE/Trad)**, the Nash-Sutcliffe efficiency (Nash & Sutcliffe 1970): skill relative to predicting the mean,
  <div style="text-align:center;"><i>R&sup2; = 1 - &sum;(O<sub>i</sub> - P<sub>i</sub>)<sup>2</sup> / &sum;(O<sub>i</sub> - O<sub>mean</sub>)<sup>2</sup></i></div>
  It penalises bias and is negative when the model is worse than the mean. Use it to judge spatial accuracy.
* **R² (Corr)**, the squared Pearson correlation: linear association only. A model predicting exactly double the true value scores 1.0 here, while its NSE is at most 0 (exactly 0 for a zero-mean target).
* **Bias.** The Model Performance table reports **Bias (ME)**, the mean cross-validation error, computed as *observed minus predicted*. Tables comparing an uploaded prediction column with the observations report **MBE (ML pred - observed)**, <i>&sum; (P<sub>i</sub> - O<sub>i</sub>) / n</i>, *predicted minus observed*, positive when the model over-predicts. The two describe different models and have opposite signs for the same over-prediction, so do not read one against the other.
* **Lin's CCC** (Lin 1989): agreement with the 1:1 line, <i>2s<sub>xy</sub> / (s<sub>x</sub><sup>2</sup> + s<sub>y</sub><sup>2</sup> + (&mu;<sub>x</sub> - &mu;<sub>y</sub>)<sup>2</sup>)</i>, with population moments (sample variances and covariance multiplied by (n − 1)/n). Reported as NA when either vector is constant; this is an application convention, since the formula gives zero when only one vector is constant, or both are constant at different values, and is undefined for identical constants.
* **RPD** = <i>SD<sub>actual</sub> / RMSE</i>, with the sample SD. Soil-spectroscopy thresholds (Chang et al. 2001) depend on the context and do not grade prediction quality universally.
* **RPIQ** = <i>(Q3 - Q1) / RMSE</i>, with R's type-7 quartiles. The IQR is less sensitive than the SD to extreme observations, although RMSE stays sensitive to large errors; it is common for soil properties such as salinity (Bellon-Maurel et al. 2010).
* **SMAPE** (Makridakis 1993): <i>(100/n) Σ 2|P − O|/(|O| + |P|)</i>, bounded from 0 to 200% for finite pairs but sensitive to small denominators. A pair whose observation and prediction are both exactly zero contributes 0 rather than being dropped, so SMAPE is averaged over the same sample count as the other metrics.

**Observed vs Predicted plot.** Each cross-validated prediction is drawn on the x axis against its observation on the y axis, with the 1:1 line and the least-squares regression of observed on predicted, whose slope and intercept the subtitle prints. A slope of 1 and an intercept of 0 indicate linear calibration in the scored pairs, although nonlinear conditional bias can remain; a slope below 1 means the predictions spread too much, above 1 too little. The regression runs this way round because, for observed = predicted + an error uncorrelated with the prediction, predicted regressed on observed has slope Var(predicted) / [Var(predicted) + Var(error)], below 1 even under linear calibration (Piñeiro, Perelman, Guerschman & Paruelo 2008). Pairs of a failed fold are left out; predictions that do not vary leave no slope, and the subtitle says so.

**Undefined values.** Undefined metrics are reported as NA, never as infinity, which would read as a real score in the tables, the exported metrics and the pooled *Total (Combined)* row. The core error metrics need at least two valid pairs; RMSE, MAE and bias stay defined for a constant target.

| Metric | NA when |
|---|---|
| R² (NSE) | observations are constant (no variance to explain) |
| R² (Corr) | either vector is constant |
| NRMSE (mean) | the observed mean is 0 |
| NRMSE (mean), NMAE (mean), SMAPE | observed values span zero (shown as `n/a¹`, below) |
| NRMSE (SD) | observations are constant |
| RPD | RMSE = 0 (with constant observations and RMSE > 0, RPD is 0) |
| RPIQ | RMSE = 0 or the observed IQR is 0 |
| CCC | either vector is constant |

**Targets that span zero.** Mean- and percentage-normalised errors need a scale whose zero means "none of the quantity". Where the observations cross zero (min < 0 < max), the zero is arbitrary: the same fit reported against a recentred version of the variable gives a different NRMSE (mean), and SMAPE's <i>|O| + |P|</i> denominator collapses at the crossing, so a single sign disagreement contributes its maximum term however small both values are. NRMSE (mean), NMAE (mean) and SMAPE are then shown as `n/a¹`, with a footnote under the table, and NRMSE (SD) is reported in their place; RMSE, MAE, both R², bias, CCC, RPD and RPIQ are unaffected. The rule is the observed sign crossing because the app has no metadata separating ratio from interval scales; `|mean| < SD` would misjudge both kinds (a positive ratio-scale variable can have SD above its mean and still normalise correctly). The zero-mean rule above is a separate guard.

**Precision.** Every metric is computed and exported at full precision. Tables show four significant digits, keep every integer digit from 1000 up, and switch to scientific notation below 1e-4, so a small non-zero value never shows as 0.

**The two performance tables.** Both are computed by one function, and their exports are built from the same table builders as the cards, so a definition or a figure cannot differ between screen and file. The **Model Performance** table (interpolation cross-validation) reports, in order: RMSE, NRMSE (mean, %), NRMSE (SD), MAE, R² (Corr), R² (NSE/Trad), Bias (ME), Lin's CCC (Agree), RPD (Prec), RPIQ, SMAPE (%), Moran's I and Moran p. The **uploaded-prediction** table reports the same statistics, one per row, with three differences: it adds NMAE (mean, %), it reports MBE as predicted minus observed, and it has no Moran's I or p, which for uploaded predictions would depend on how they were made. Its R² (NSE/Traditional), R² (Correlation) and RPD (Precision) are the Model Performance table's R² (NSE/Trad), R² (Corr) and RPD (Prec).

### 5.6 Moran's I of cross-validation errors

Moran's I (Moran 1950) tests whether cross-validation errors are spatially autocorrelated. Under random folds and LOOCV, significant autocorrelation is consistent with spatial structure the prediction missed. It is a diagnostic to follow up and does not by itself call for another engine.

**Neighbours.** A symmetric k-nearest-neighbour graph with k = 8 (capped at n − 1 for small samples), row-standardised (`spdep::nb2listw(style = "W")`; Bivand & Wong 2018). A kNN definition is scale-stable and avoids an arbitrary distance band, which at typical field spacings is wide enough to dilute local autocorrelation toward zero. Moran's I depends on the neighbour definition, so read the value as the residual autocorrelation under this 8-NN weighting. Duplicated coordinates are separated by a negligible, data-scaled displacement under a fixed seed, so the statistic is reproducible (Section 11). If the spdep neighbour search fails, an all-pairs 1/d weighting computes I instead (only at n ≤ 500).

**Reported values.** The table gives I and its two-sided p-value; hovering the I cell shows the null expectation, E[I] = −1/(n − 1). It lies slightly below zero (−0.034 at n = 30, −0.010 at n = 100), so a marginally positive I is not by itself evidence of clustered errors. The p-value comes from a permutation test: the residuals are permuted over the fixed weights 999 times under a fixed seed, which is exact under the randomisation null and assumes no distribution for the residuals (the normal approximations need Gaussian residuals, and cross-validation residuals are often heavy-tailed). It is two-sided, because strongly negative autocorrelation (a checkerboard pattern) signals misspecification as much as clustering does. The observed arrangement counts as one of the permutations,

<div style="text-align:center;"><i>p = min(1, 2 &middot; min((1 + #{I* &ge; I}) / 1000, (1 + #{I* &le; I}) / 1000))</i></div>

(Davison & Hinkley 1997; Phipson & Smyth 2010), so p is never 0 and its smallest value is 0.002. A permuted statistic within rounding of the observed one counts as a tie; where every arrangement gives the same I (a complete neighbour graph, as the 8-NN graph of nine samples or fewer is), p = 1. The all-pairs fallback uses the same test on its own weights. The test is approximate in one respect: cross-validation errors can stay dependent because their training sets overlap, which the permutation null does not account for.

**`NA*`** in both Moran cells means the statistic could not be computed: fewer than three cross-validated points, no coordinates on the cross-validation object, or a neighbour-search failure the fallback could not rescue. It is a missing value and does not mean the errors lack autocorrelation; the run log records which condition applied.

**Spatial folds.** Under Spatial Block CV, and where kNNDM chose spatial folds, the pooled errors also share the extrapolation condition of each withheld group: whole neighbourhoods are predicted from training data that excludes their surroundings, so their errors move together whatever the model's specification. The permutation null does not know this, so a large or significant I there shows clustering of those errors but cannot by itself diagnose a missing trend or justify switching to RK or RFK. Such rows are labelled **Block-CV residual clustering** or **Spatial-fold residual clustering**, marked `†`, and their p-value is withheld (`NA†` on screen, empty in the export, with the `Moran Context` column naming what the statistic measures). Rows that fell back to LOOCV because the locality has fewer than 30 samples, rows where kNNDM chose random folds, and pooled rows that mix fold designs keep the ordinary reading and their p-value.

Under random folds, Moran's I therefore diagnoses the model's error structure; under block or spatial folds it measures clustering of transfer errors. An I near its expectation under random folds does not show a clean model either: random folds leave each held-out point's near neighbours in the training set and can hide the structure the statistic looks for.

### 5.7 Class agreement of a continuous prediction

Observed and predicted values are binned into classes and compared as a confusion matrix, reporting overall accuracy, balanced accuracy, off-by-one accuracy, the Matthews correlation coefficient (Matthews 1975), unweighted Cohen's kappa (Cohen 1960) and linearly weighted kappa (Cohen 1968). The on-screen Agreement table and the exported *Total Classification Performance* table are computed by the same function.

* **Agronomical classes:** each measured value and its uploaded prediction are binned with the Actual surface's classes (Section 9.1), with left-closed intervals `[low, high)`, as on the map, so a value lying on a class limit gets the same class in the tables and on the map.
* **Quartile classes:** right-closed intervals at the unique quartiles of the **observed** column (ties can leave fewer than four classes), applied to both vectors, since a confusion matrix needs one class definition. The outer intervals extend to −∞ and +∞, so a prediction outside the observed range falls into Q1 or Q4 rather than being dropped. The matrix then cannot show how far beyond the observed range a prediction went; read RMSE and bias beside the kappa table when extrapolation is a concern.

The two closures differ only for a value exactly on a break: agronomical limits are class limits a map paints, while quartiles are order statistics of the data with no map counterpart.

### 5.8 Pooled Total (Combined) rows

The *Total (Combined)* row pools the localities' cross-validation results.

* **CRS.** Each locality's results carry its working CRS, so the pooled Moran's I and the pooled observed-versus-predicted scatter reproject them to the UTM zone of the combined centroid, the same zone rule as for geographic input. Web Mercator would inflate distances by 1/cos(latitude), about 40% at 45° N. Per-locality diagnostics are unaffected. If the results cannot be combined, the pooled diagnostics show their empty state rather than a partial pool.
* **R² inflation.** The pooled R² (NSE/Trad) and R² (Corr) are measured against the pooled mean of all localities. Where localities sit at different levels (different mean salinity or nutrient status), that between-locality variance enters the total sum of squares and the model is credited with explaining it, even where it explains nothing within any locality, so the pooled row can show a higher R² than every locality row. Judge skill on the per-locality rows, and read the pooled row for the overall error size: RMSE, MAE and bias do not depend on the centre.

Pooled variograms are described in Section 4.4.

### 5.9 Exported tables

Every table of the Scientific Analysis tab is registered for export, per locality and for the run as a whole, built by the same function as its card and written at full precision (cards round for reading, Section 5.5). A table's copy button puts the rendered table, every page of it, on the clipboard with the displayed rounding: use it for reading and drafting, and the export registry when the numbers will be computed on. Some exports carry what the screen shows as labels or on hover:

* **Model CV Metrics:** one wide row per source, as on the card, plus the fold plan (*CV Design*: for example *LOOCV*, *Random 10-fold CV*, *Spatial Block CV*, *kNNDM CV [spatial folds]*, or *pooled per-locality CV* for the run-wide row), *CV Population* and *CV Population ID*, what the folds re-estimated (*CV Refit*: *per fold* or *conditional on applied variogram*), *n expected*, *n predicted*, *Coverage (%)*, Moran's E[I], *Moran Context*, and *Target spans zero*, which explains an empty NRMSE (mean) or SMAPE cell.
* **Fold-Realization Stability:** the mean and the standard deviation as separate numeric columns rather than the card's *mean* ± *SD* text, with *n expected* and the minimum and maximum *n predicted* across realizations.
* **CV Distance Match:** per locality and surface, the percentiles the panel draws (map cell to nearest sample, held-out sample to nearest training sample, sample to nearest other sample, every 1%), W for the run's folds and for random 10-fold, the sample count and the number of map points.
* **RK Regression Coefficients:** the p-value as a number (the panel prints "< 0.001") and the confidence interval as two numeric columns.
* **Variogram parameters:** with the empirical points, the model family, nugget <i>C</i><sub>0</sub>, total sill <i>C</i><sub>0</sub> + <i>C</i>, range parameter *a*, practical range and structural dependency (Section 4.1); the Matérn smoothness &kappa; beside a Matérn model; and, for a nested model, every structure, with the ranges of the one that reaches its sill last. Auto-fitted rows add the lag support (*Max Lag*, *Sill Resolved*) and the equally-fitting set: *Nugget Identified*, *Structural Dep. Low (%)* and *High (%)*, *Practical Range Low* and *High*, and *Range At Search Limit*. These diagnostics are blank for a manual model; a failed-fit stand-in keeps *Max Lag* when usable lag bins exist; a pure nugget has blank range columns. *Fit Status* says what each row is: *fitted*, *range unresolved by lag support*, *pure nugget, no structured fit selected*, *variogram fit failed* (the stand-in, whose listed parameters are seeds, not estimates), *applied manual model*, or *no usable variance, not estimated* (parameter columns blank, *Max Lag* kept when available).

---

## 6. Residual Analysis

Metrics summarise performance; residual maps show where a model fails. These products need a run with an uploaded prediction column and a Predicted surface (Comparison Mode or a prediction value type); an Actual-only run has none.

### 6.1 Interpolated delta (surface difference)

The measured values and the uploaded predictions are interpolated into two surfaces with the chosen method and subtracted: <i>Surface<sub>Actual</sub> &minus; Surface<sub>Predicted</sub></i>. The map shows broad zones where the uploaded predictions consistently over- or under-predict.

### 6.2 Point errors and interpolated point errors

The error at each sample location, <i>O<sub>i</sub> &minus; P<sub>i</sub></i>, is drawn as coloured points (the Map Viewer's Point Residuals panel, exported as the Point Error Map). The Interpolated Point Errors Map in the Export panel interpolates these errors by IDW at *p* = 2 with the run's IDW neighbour count, over the coordinate-valid rows that carry both values, co-located rows kept, so it can use more rows than the deduplicated point markers. The points are projected into the locality's working CRS first, so geographic uploads work as projected ones. The errors belong to the model that produced the uploaded predictions, not to Monolith's interpolator: hotspots mark where that model misses the local variability.

---

## 7. Prediction Uncertainty

Kriging (OK, RK, RFK, CK) gives a prediction variance from its statistical model; IDW and TPS give none, so their runs have no uncertainty maps. No variance is reported where the variogram was not estimated from the data (Section 7.4).

### 7.1 What drives kriging variance

Kriging variance depends on the **spatial configuration** of the samples and on the **variogram model**. It is generally lower near samples. Nugget, sill, range, neighbourhood and sample geometry act together, so changing one does not change the variance uniformly.

### 7.2 Variance and standard error

* **Kriging variance:** the model's mean squared prediction error, in the *squared* units of the variable (for example (mg/kg)² for potassium): an SE of 130 mg/kg corresponds to a variance of about 17,000 (mg/kg)², so the two are not compared by magnitude. It is conditional on the fitted covariance model; read it alongside the cross-validation errors.
* **Variance of a new measurement:** the reported variance is that of a new measurement at the location, so it **includes the nugget**: <i>&sigma;<sup>2</sup><sub>OK</sub>(s) = C(0) &minus; &sum;&lambda;<sub>i</sub> C(s, s<sub>i</sub>) &minus; &mu;</i>, evaluated with the fitted model's full sill. It is therefore at least about the nugget everywhere except at a sampled location, where point kriging can give zero, and a standard-error map does not approach zero between samples however dense they are. The variance of the noise-free (filtered) value, which excludes the nugget, is not reported.
* **Standard error:** the square root of the variance, in the variable's units. 2.0% nitrogen with SE 0.2 percentage points gives 2.0 ± 1.96 × 0.2, about 1.61% to 2.39%, only under an adequate Gaussian prediction-error model. Variogram estimation, model selection and covariate reconstruction add uncertainty the map does not include, so nominal 95% coverage is not established.
* **Display:** uncertainty layers always use a sequential palette, since variance and SE are non-negative with no meaningful midpoint; a diverging palette choice is drawn as viridis on these layers, in the Map Viewer and the Export Styler. The legend names the metric and the variable's recorded unit (for example "Variance: K (mg/kg)^2"; "(squared units)" when no unit is recorded). Class breaks apply to the value surfaces only, and those surfaces' breaks and class areas do not change while an uncertainty map is shown. Under **Match Scales**, the Actual and Predicted uncertainty maps share one colour range, as the value maps do.

### 7.3 RK and RFK uncertainty

The RK and RFK variance is the sum of a **trend** term, the error of the fitted relationship between target and covariates, and the **residual** kriging variance. The sum assumes the two errors are independent, the usual approximation of two-step regression kriging; carrying their covariance requires kriging with external drift or universal kriging, which the two-step engine does not do. Read the Total as approximate.

**Trend term.** For RK it is the linear model's prediction variance (`se.fit²`), the variance of the estimated mean surface. For RFK it is chosen with **RFK Uncertainty Method**:

* **Infinitesimal jackknife (default):** the estimator of Wager, Hastie & Efron (2014) with the Monte-Carlo bias correction: the sampling variance of the ensemble-mean prediction, the random-forest analogue of RK's `se.fit²`, under its assumptions. Negative estimates are clipped to zero; it does not establish calibrated intervals for the data set.
* **Ensemble spread (fast):** the between-tree variance, the spread of the trees around the ensemble mean. It measures model instability rather than predictive variance and has no guaranteed relation to it; use it as a relative map of where the model is least stable.

The method changes only the RFK uncertainty surface; the prediction map and the cross-validation metrics are the same either way. RK and RFK variances are not directly comparable.

**Covariate error.** The trend is evaluated on kriged covariate surfaces (Section 1.7) as if their values were measured without error, and the covariates' own kriging variance is discarded. The missing term, roughly `Σ (∂trend/∂xⱼ)² · Var(x̂ⱼ)` plus covariate cross-terms, is smallest near the samples and grows into unsampled ground, where the covariate surfaces are least certain, so the RK and RFK uncertainty is optimistic away from the samples. This is standard practice for two-step regression kriging with interpolated covariates; read the Total as a relative reliability map rather than a calibrated interval.

Quantile regression forests are not offered: a QRF interval already contains the residual scatter that residual kriging models separately, so adding the two would count that variance twice.

### 7.4 No variance without an estimated covariance

A kriging variance is only as good as the covariance it is computed from, and two kinds of fit do not estimate one:

* the failed-fit stand-in (fewer than five usable lag bins, or a variogram zero at every lag, as for a constant target; Section 4.1), whose nugget share and range are seeds and whose sill is the sample variance, or 1 for a constant target;
* a fit to values constant up to rounding (a target varying only in noise digits, or RK and RFK residuals within that floor of the target's magnitude; Section 4.1), which describes rounding.

Multiplying a covariance by a constant leaves the kriging weights unchanged and multiplies the kriging variance by the same constant, so such a variance only restates the assumed sill, or the rounding. The prediction is kept (the map banner flags a failed fit, and the run warnings a target without usable variance), and the variance is withheld as `NA`, never set to zero: constant samples do not establish certainty between them.

* **OK and the OK fallback** of a failed RK, RFK or CK: no variance where the map's variogram is of either kind, including a measured-values fit reused on the Predicted surface (Section 1.8), which says nothing about that surface's own values.
* **RK and RFK:** no residual kriging variance, so no total is reported; the trend variance alone is not the prediction variance.
* **CK:** no variance when the target's own variogram is the stand-in (the LMC range is then a seed, and a constant target's co-kriging variance comes out near zero) or when the target is constant up to rounding.
* **Cross-validation:** a fold kriged with either kind keeps its predictions, which are scored as usual, and carries `NA` variance.
* **Maps and exports:** such a locality is blank on the standard-error and variance maps and left out of their GeoTIFFs while the other localities keep theirs, and its own map GeoTIFF carries the prediction only. A surface on which no locality has a variance offers no uncertainty view or export. The run log names each such surface (*[Uncertainty]*), and the map banner's *Variogram fit failed* band says that no standard error or variance is reported where the stand-in was used.

An applied manual model is the user's own model: its kriging variance is reported and its cross-validation is labelled conditional on it (Section 5.2).

---

## 8. Exploratory Statistics and Covariate Screening

### 8.1 Multicollinearity screens

Severe multicollinearity destabilises regression-type models and weights one direction twice in a PCA. Three screens address it.

**PCA (Exploratory suite).** Before the PCA runs, the selected variables are scanned for near-duplicate pairs, |*r*| > 0.95. Such a pair measures one direction twice, which is then weighted double in the leading components, so the screen lists each pair with its *r* and asks the user to remove one variable of each pair or to run the PCA with all of them, since keeping both can be a legitimate choice. No VIF is computed: a variance inflation factor measures how well one variable is predicted by a regression on the others, a property of regression coefficients, while shared variance is what principal components summarise. Constant variables are reported separately and never halt anything: a column with no or effectively no variance over the analysed rows (the near-constant rule below) cannot be standardised, so it is excluded before `prcomp`, the other columns keep the requested scaling, and the exclusion is named above the results and in every PCA figure's caption. With fewer than two informative columns the PCA is refused, naming the excluded ones.

**Interpolation (RK, RFK, CK).** With more than one covariate selected, the constant/VIF screen runs before the run starts, on each selected locality's fitted point set and for each surface the run maps, as the run will screen them. When any locality would drop a covariate above VIF 10, a dialog lists per locality (and per surface where the two differ) the covariates its model would drop as collinear and those constant there, and offers **Auto-Drop and Continue**, **Keep All (Not Recommended)** or **Cancel**. The answer is a rule for the collinear covariates: the map applies it to each locality's samples, and every fold again to its training samples (Section 5.2). A covariate constant within a locality is dropped there whatever the answer, so constants alone open no dialog; they are named in the run log, and the message for a surface left without covariates names the cause (constant or collinear) and offers **Keep All** only where collinearity was the cause. Dropped covariates are removed from the models and the downstream diagnostics, variable-importance plots included. **Keep All** disables the iterative pruning inside the engines, in interpolation and classification alike, and the collinearity is still reported. A sole covariate bypasses the screen: a constant sole covariate is logged and can give an intercept-only trend or a named fallback. An answer applies to the run it was given for; the next run screens again.

**Classification Suite.** The same iterative-VIF screen checks the selected numeric covariates, with a live warning under the covariate picker and the same dialog at run time. The threshold is 10, or 5 when Random Forest is selected (Section 10.8). Under **Keep All** constants can remain, but the recipe removes zero-variance predictors during fitting.

**Screening rules.**

* A covariate is near-constant when sd ≤ 10⁻⁸ × max|x| over its finite values: invariant to multiplicative unit changes, not to additive offsets.
* VIFs come from inverting the covariate correlation matrix. When inversion fails because the covariates are too tightly related, the screen takes the most strongly correlated pair and drops the member with the higher mean absolute correlation with all remaining covariates, the more redundant of the two, repeating until the matrix inverts; exact ties drop the alphabetically later name, so the outcome does not depend on column order.
* The screen runs before the covariates are interpolated onto the grid, so a dropped covariate is never kriged. It is evaluated separately for the Actual and Predicted surfaces, on the point sets each engine fits, which can differ; the grid carries the union of the two retained sets.
* CK applies the screen inside its engine, on the locations carrying every candidate covariate, covariate-only locations included (Section 1.4). Its LMC fits a direct variogram for every covariate and every cross-variogram, and collinear covariates push the coregionalization matrices toward singularity, so `fit.lmc()` can fail and the locality falls back to Ordinary Kriging. A CK run whose covariates trip the threshold therefore gives different results depending on the Drop/Keep answer.
* In classification under **Auto-Drop**, the screen is a step of the model recipe, which is re-estimated on every set it is fitted to: it reruns on the training rows of each outer fold, each tuning resample and each nested inner fold, so the reported CV evaluates screening and fitting together and held-out covariate values never help choose the predictors scored on them (Hastie et al. 2009, §7.10.2). The final model is fitted on the covariates the same screen keeps on all scoped rows; those are the covariates the map and the exported model use, and the run summary lists them with how many folds dropped each one. The dialog's list, computed on the scoped data before the run, can differ slightly from the final model's, which uses complete rows only. Tuning ranges bounded by the predictor count (the Random Forest `mtry`) use the covariates kept on all rows; a fold that keeps fewer caps the value at its own count. **Keep All** runs no screen anywhere.
* Correlation structure depends on scope: two covariates can be nearly collinear across the whole data set and independent within one locality, or the reverse. The interpolation screen runs on each selected locality of the effective data subset on its own; the classification screen uses the suite's resolved spatial scope.

### 8.2 Multivariate outlier screen (Mahalanobis distance)

The PCA panel's outlier screen computes each observation's **squared** Mahalanobis distance (Mahalanobis 1936) in the space of the retained principal components. PC scores are uncorrelated, so their covariance is the diagonal matrix of squared component standard deviations, and the distance over the retained components equals the full-space Mahalanobis distance restricted to that subspace. All non-degenerate fitted components are used, whatever the biplot axes; components with numerically zero variance, which appear when a PCA is forced on collinear inputs, are excluded from the distance and from the degrees of freedom of the &chi;<sup>2</sup> reference line (97.5% quantile). The cutoff is an approximate Gaussian reference rather than an exact test, since the centre and scatter are estimated from the same observations.

The estimator is the classical one: the centre and scatter are the sample mean and covariance, which the candidate outliers help define. A single extreme point inflates the covariance in its own direction, and a small cluster of outliers can inflate it enough to pull their own distances back under the threshold (masking). A high-breakdown estimator such as the minimum covariance determinant (Rousseeuw & Van Driessen 1999) would avoid this but is not used; the panel names its estimator in the title and subtitle. Treat a flagged point as worth inspecting, and an unflagged point in a suspicious cluster with caution rather than as evidence of normality.

### 8.3 Partial correlations

A partial correlation removes the linear influence of *k* control variables from both variables before measuring their association. The estimator depends on the method, with `ppcor`-style formulas (Kim 2015), and each pair is conditioned on the explicit controls only, never on the other selected targets:

* **Pearson:** the raw values are regressed on the controls and the residuals are correlated; for each pair this equals inverting the matrix of that pair and the controls.
* **Spearman:** every variable, targets and controls, is rank-transformed first, and the partial correlation is the Pearson correlation of the rank residuals. Spearman's coefficient is the Pearson correlation of ranks, so the partial version must residualise the ranks, not the raw values.
* **Kendall:** no residual form exists, so for each pair the Kendall τ matrix of that pair and the controls is inverted and the pair's entry reported as −Pᵢⱼ/√(PᵢᵢPⱼⱼ). With one control this is Kendall's first-order partial τ. Adding a target to the table does not change the other pairs' estimates.

Every coefficient is computed on the rows complete for every selected target and control (listwise deletion), so all pairs share one *n*, given in the table caption. Adding a target with missing values therefore removes those rows from every pair; adding one without missing values leaves the other coefficients unchanged.

The table also reports Benjamini-Hochberg adjusted p-values over its tested pairs, without adjustment for spatial dependence. Significance uses *n* − 2 − *k* degrees of freedom (Pearson and Spearman t-statistic) or a normal approximation with effective sample size *n* − *k* (Kendall). A variable listed as both a target and a control is removed from the controls. For Pearson and Spearman, *k* is the QR rank of the control design (with its intercept) minus one, so redundant and constant controls cost no degrees of freedom. A target whose residual norm is at most 10⁻⁷ of its centred norm (the QR tolerance, on centred and scaled values, so units and offsets do not matter) is explained by the controls, has no estimable partial correlation, and is refused by name, as is a test with no residual degrees of freedom; Kendall refuses a singular matrix. A failed controlled calculation is never replaced by a raw correlation.

### 8.4 Spatial cross-correlogram

The cross-correlation of two variables as a function of the distance between sample points. Both are standardised to zero mean and unit variance, and the empirical cross-variogram &gamma;<sub>12</sub>(*h*) is computed on projected metric coordinates up to the cutoff every other variogram uses (half the bounding-box diagonal), divided into the number of bins set by **Distance Bins** (3 to 50, default 15, which reproduces the variogram lag width). At least 15 distinct locations are required. The estimator is

<div style="text-align:center;"><i>&gamma;<sub>12</sub>(h) = (1 / 2N(h)) &sum; [Z<sub>1</sub>(x<sub>i</sub>) − Z<sub>1</sub>(x<sub>i</sub> + h)][Z<sub>2</sub>(x<sub>i</sub>) − Z<sub>2</sub>(x<sub>i</sub> + h)]</i></div>

Under second-order stationarity the cross-variogram and cross-covariance are related by &gamma;<sub>12</sub>(*h*) = C<sub>12</sub>(0) − [C<sub>12</sub>(*h*) + C<sub>21</sub>(*h*)]/2, so for standardised variables the plotted spatial cross-correlation is

<div style="text-align:center;"><i>&rho;<sub>12</sub>(h) = r<sub>12</sub> − &gamma;<sub>12</sub>(h)</i></div>

with *r*<sub>12</sub>, the ordinary non-spatial correlation, drawn as a dashed reference line (Goovaerts 1997; Isaaks & Srivastava 1989).

**Reading it.** A curve that starts near *r*<sub>12</sub> at short lags and decays toward zero means the two variables co-vary locally, sharing spatial structure at the scale where the curve stays above zero; this is the coregionalization co-kriging uses. A flat curve at about *r*<sub>12</sub> means the association has no distance structure; a flat curve near zero means the variables are spatially unrelated at the sampled scales. A zero crossing is an empirical sign change and estimates no range: noise and negative cross-structure can also produce one.

**Caveats.**

* Only the symmetric part of the cross-covariance can be estimated: the bins are omnidirectional, so the pairs (*i*, *j*) and (*j*, *i*) fall in the same bin, and unlike a time series a point set has no leads and lags (there is no negative-lag half). Directional cross-structure is not resolved; Section 4.4 covers the univariate anisotropy check.
* Each point is a bin average whose reliability follows its pair count: bins with fewer than 30 pairs, the conventional minimum (Journel & Huijbregts 1978), are greyed out, and point size shows the count.
* Sampling noise can push a bin slightly outside [−1, 1], since these are unconstrained moment estimates.
* With `Spearman` or `Kendall` selected, both variables are rank-transformed before standardisation and the curve is a rank-based cross-correlogram, labelled as such; Kendall has no distance-binned analogue of its own.
* Co-located samples (coordinates agreeing at centimetre precision after projection) are merged into one location with both variables averaged, as in the interpolation point sets, so replicates do not stack the shortest bin.

### 8.5 PCA contribution and quality of representation (cos<sup>2</sup>)

Both panels are built from the **variable coordinates** *c*<sub>jk</sub> = *v*<sub>jk</sub> &middot; *s*<sub>k</sub>, where *v*<sub>jk</sub> is the loading of variable *j* on component *k* and *s*<sub>k</sub> that component's standard deviation.

* **Contribution** is the share of a *component* due to a variable: *v*<sub>jk</sub><sup>2</sup> &times; 100, summing to 100% over the variables of a component, since eigenvectors have unit length in both PCA modes. The dashed line marks the uniform expectation 100/*p*.
* **cos<sup>2</sup>** is the share of a *variable* captured by the selected components: &sum;<sub>k &isin; axes</sub> *c*<sub>jk</sub><sup>2</sup> &divide; &sum;<sub>k</sub> *c*<sub>jk</sub><sup>2</sup>. The denominator is the variable's total variance, since &sum;<sub>k</sub> *c*<sub>jk</sub><sup>2</sup> = (**V S**<sup>2</sup> **V**<sup>T</sup>)<sub>jj</sub> = Var(*x*<sub>j</sub>), so the value lies in [0, 1] and reaches 1 across all components (Abdi & Williams 2010; Lê et al. 2008).

In a correlation-matrix PCA (**Scale & Center Data** checked) every variable has unit variance, so the denominator is 1. In a covariance-matrix PCA it is needed: without it the sum is a variance in the variable's own squared units, and a variable in mg&nbsp;kg<sup>-1</sup> would be compared with one on a 0-1 scale by unit size alone. With it, cos<sup>2</sup> answers the same question in both modes.

Scaling still affects the other measures. Contribution remains scale-sensitive by definition, and in an unscaled run it ranks variables largely by raw variance; the module says so inline. cos<sup>2</sup> is per-variable and comparable, but in an unscaled run the components it is measured against are dominated by the high-variance variables, so a low cos<sup>2</sup> there means these components describe other variables; it does not mean the variable is unstructured.

### 8.6 Group-comparison tests and post-hoc letters

The descriptive module annotates grouped plots with one significance test at a time (**None** by default): one-way ANOVA (reported as *F*, df and *p* rather than letters), Tukey's HSD (Tukey 1949), Duncan's multiple range test (Duncan 1955) or Kruskal-Wallis (Kruskal & Wallis 1952). Letters come from `agricolae`.

* **Tukey's HSD** controls the family-wise error rate across all pairwise comparisons of *k* groups.
* **Duncan's MRT** is labelled *liberal*: it controls only the comparison-wise error rate, so with *k* groups its effective family-wise rate grows toward 1 and it declares more differences than Tukey on the same data. It is offered to reproduce the older agronomy literature that reports it and is not an equivalent alternative. Where a result depends on a difference Duncan finds and Tukey does not, report both.
* **Kruskal-Wallis** compares rank distributions and still assumes independent observations; reading it as a comparison of medians requires similarly shaped group distributions, and the normality indicator alone cannot select a valid test. Its pairwise comparisons are Benjamini-Hochberg adjusted (Benjamini & Hochberg 1995), as in the correlation table.
* ANOVA, Tukey and Duncan require suitable residual and variance assumptions. None of these tests adjusts for spatial dependence. Shared letters mean only that no difference was detected; they do not show that the groups are equivalent.
* Letters are computed on the plotted subset (the current locality or scope selection and any facet), so they describe the data shown.
* With only two groups, Tukey and Duncan report the one-way ANOVA *F* test instead of letters, since one pairwise comparison needs no multiple-comparison procedure.

### 8.7 Missing values: complete cases or pairwise

Which samples a correlation uses depends on what the result is for, and the rule is the same throughout the app. Neither rule treats missing data: if complete-case deletion removes a large share of the samples, narrow the variable set or impute with a suitable method before uploading.

**Multivariate analyses use complete cases**, the rows with no missing value in any variable of that analysis (for partial correlations, the controls included). This covers the correlation heatmap, network and correlogram, partial correlations, PCA, the Mahalanobis screen, the Governing Factors forest and the VIF stage of the collinearity screen. With pairwise deletion each cell would come from a different subsample, and such a matrix need not be positive semi-definite: partial correlation inverts it and could return |r| > 1 or fail, PCA could return negative eigenvalues, `diag(solve(R))` negative VIFs, and the heatmap's clustering uses 1 − |r| as a dissimilarity. Each cell would also carry its own *n*, so no single sample size or comparable set of p-values would exist. Each of these panels states the sample it used and how many rows it dropped.

**Standalone pairwise screens use pairwise deletion**, every sample where both variables are present, so a variable measured on part of the data set is still screened on what it has. This covers the **Predictor correlations** screen (below), where each rank carries its own *n*, and the pairwise-threshold stage of the collinearity screen, where each pair is compared with the threshold on its own and nothing inverts the matrix. The correlation screen requires at least three finite pairs. A pair with fewer than two shared finite observations, or no variation on its overlapping rows, has no correlation to screen: the collinearity screen skips it and never reads it as zero correlation. The VIF stage excludes columns that become constant on its complete-case population before inversion. The PCA advisory uses the same complete rows as its fit.

**Predictor correlations screen.** It sits in the sidebar's Auxiliary Variables panel (its **Rank by correlation** button), so it is available wherever that panel is: RK, RFK, CK, and OK under the Comparable CV population. Its target is the mapped measured column in Actual view, or by default the mapped uploaded CVE or SS column in prediction views (CVE also in Residuals view); a source switch selects the measured values on the same scope instead. CVE uses all rows of the selected localities; SS applies the panel's own partition choice to either source, initialised from Context's subset. These controls affect only this screen. Each coefficient is `stats::cor.test(method = "pearson")` on the finite pairs, with its raw two-sided p-value and own *n*; constant pairs are unavailable. Candidates are ordered by |r| and can be filtered by category or raw p. The screened target, the selected Actual variable and the mapped coordinates are excluded from the ranking; other uploaded prediction columns stay eligible. Raw p-values assume independent observations and are not adjusted for multiple screening or spatial dependence, so the screen is exploratory. Pearson measures linear association, while RFK can also use non-linear relationships.

### 8.8 PCA fitting, normality and Governing Factors

**PCA** runs on at least three selected variables and five complete rows in the active groups. It uses `stats::prcomp` on complete rows, after excluding zero-variance columns (Section 8.1), always centred; **Scale & Center Data** also scales each column to unit sample SD, so unchecked means a centred covariance PCA, not an uncentred one. Scree and cumulative plots use the squared component SDs divided by their sum; the 80% cumulative reference line is a display aid and selects no components. **3D PCA Scores** shows observations on three distinct components, without loading arrows; two-component fits remain available in the other views.

**Normality indicator.** Missing values are removed; at least three observations and non-zero variance are required. Shapiro-Wilk is used below n = 5000 and Lilliefors at or above. At p ≥ 0.05 the result is labelled normal, meaning only that this test did not reject normality. The tests are not adjusted for spatial dependence or for repeated testing across variables and groups.

**Governing Factors** fits a separate regression random forest on the rows complete for the target and the selected predictors, and requires at least 50 such rows. It reads the rows as uploaded, co-located samples included, because it is a non-spatial importance analysis; every spatial model, the Classification Suite included, merges them (Sections 1.7, 10.2). It performs no spatial cross-validation or residual kriging.

**Importance.** Each predictor's importance is Breiman's (2001) permutation importance: for every tree, the increase in the mean squared error of its out-of-bag predictions when that predictor's values are permuted, averaged over the trees (`randomForest::importance(type = 1, scale = FALSE)`; Liaw & Wiener 2002), in the target's squared units. Out-of-bag rows are those a tree never saw; permuting on the rows a forest was grown on would score what it memorised along with what it learned, giving a pure-noise predictor a clear importance. The unscaled value orders the factors: zero means no observed increase, and a negative value means permuting the predictor reduced the out-of-bag error. The table and plot also show the scaled value, the increase divided by its standard error across trees (randomForest's %IncMSE, as in the RFK importance panel); it grows with the tree count, roughly with its square root, and is not a z-score (Strobl & Zeileis 2008), so scaled values compare only between runs with the same number of trees. The reported OOB variance explained is the same forest's out-of-bag diagnostic. Out-of-bag samples lie among the training samples, so with spatially autocorrelated samples it overstates how well the forest predicts at new locations (Meyer et al. 2019; Ploton et al. 2020). Neither statistic establishes spatial transfer or causation. Correlated predictors distort the ranking: the forest divides their shared signal between them, and permuting one of them alone creates combinations of values the data do not contain (Strobl et al. 2008).

The most important predictor is profiled three ways: partial dependence averages predictions while that predictor changes; accumulated local effects aggregate local model differences across its observed distribution; SHAP estimates feature contributions (DALEX's aggregated values) for a seeded sample of observations. These describe the fitted model, and correlated predictors, extrapolated predictor combinations and finite Monte Carlo sampling affect their interpretation. The controls set the forest's tree count, which also sets how many trees the importance averages, and the maximum number of explained observations (Section 11).

---

## 9. Map Classes and Exports

### 9.1 Class breaks

Each surface is classed by **Binned** styling or by one of the agronomical algorithms, Jenks, K-Means or Supervised (User Guide, Map Styling).

* **Binned:** 5 equal-interval classes over the surface's value range (both surfaces pooled under **Match Scales**); they depend on the range only.
* **Jenks natural breaks:** the partition of the values into *k* classes of consecutive values that minimises the total within-class sum of squared deviations (Jenks 1967, 1977), an optimum Fisher (1958) showed can be found exactly. Monolith finds it by dynamic programming over every cell value of the surface (`Ckmeans.1d.dp`; Wang & Song 2011), deterministically and without sampling, so the breaks carry no sampling error. Where cells differ in ground area, each value is weighted by its cell's area (Section 2.6) and the weighted sum of squares is minimised, also exactly.
* **K-Means:** Lloyd's algorithm (Lloyd 1982) from one set of k-means++ starting centres (Arthur & Vassilvitskii 2007), drawn under the fixed seed 12345, so the breaks are reproducible. Each value joins its nearest centre (a value midway between two joins the lower one) and each centre moves to the mean of its class, until no value changes class (at most 1,000 iterations); a class left empty restarts at the value contributing most to the sum of squares. Values, class means and the k-means++ draws carry the same area weights as Jenks, so both pursue one criterion, and integer weights give exactly what repeating each value that many times gives. K-Means is the heuristic of the two: it stops at a partition in which every value is nearest the mean of its own class, which can be a local optimum that Jenks improves on.
* **Supervised:** limits typed into the boxes, which open with published agronomic limits or data quantiles (Section 9.2).

Jenks and K-Means place each break midway between the largest value of one class and the smallest value of the next, so the partition holds under the app's `[low, high)` convention: a break placed on a class's largest value would move that value, and every value tied with it, into the class above.

**Actual and Predicted classes.** The Actual and Predicted surfaces are each classified with breaks computed from their own values, whichever map is shown, so each map, legend and Area Coverage table describes its own surface, and the difference between the two maps is the difference between the surfaces. **Match Scales** computes one set of breaks from both surfaces pooled and applies it to both. Supervised limits are fixed numbers in one variable's units, shared by both surfaces and applied only to the variable they were set for; Jenks and K-Means breaks are recomputed for whichever surface is displayed. The agreement table bins each measured value and its uploaded prediction with the Actual surface's classes (Section 5.7); before a first run, the data column stands in for the Actual surface. Class settings take effect when **Apply to maps and statistics** is pressed.

### 9.2 Reference class limits (Supervised styling)

With the **Supervised** algorithm and **3 classes**, the limit boxes open with published agronomic limits when the variable's column name identifies one of the nutrients below. A name identifies a nutrient by its symbol as a whole word (`k`, `Fe_DTPA`) or by its name as a word or at either end of one (`Potassium`, `TotalNitrogen`), never inside a longer word (`Environment` names no iron). Words are separated by spaces, punctuation, underscores and dots, and bracketed text and units (`(mg/kg)`, `mg kg-1`) are removed first, since the "mg" of a unit is the symbol of magnesium. A name naming two nutrients (`Ca/Mg ratio`) identifies neither, because its values are not one nutrient's concentration. The classes are **Low** below the first limit, **Moderate** from the first limit up to the second, and **High** from the second up: every class holds its lower limit, as the map, the class areas and the agreement table classify (`right = FALSE`).

| Nutrient | Method, unit | Low < a ≤ Moderate < b ≤ High | Source |
|---|---|---|---|
| TN | Total N, % | 0.05, 0.10 | Çokuysal & Erbaş (2004) |
| P | Olsen P, mg kg⁻¹ | 8, 25 | Yüksel & Ekinci (2019) |
| K | NH₄OAc K, mg kg⁻¹ | 200, 300 | Çokuysal & Erbaş (2004) |
| Ca | NH₄OAc Ca, mg kg⁻¹ | 1428, 2857 | Çokuysal & Erbaş (2004) |
| Mg | NH₄OAc Mg, mg kg⁻¹ | 80, 160 | Çokuysal & Erbaş (2004) |
| Fe | DTPA Fe, mg kg⁻¹ | 4, 6 | Local limits: Çokuysal & Erbaş (2004); DTPA test: Lindsay & Norvell (1978) |
| Mn | DTPA Mn, mg kg⁻¹ | 1.2, 3.5 | Local limits: Çokuysal & Erbaş (2004); DTPA test: Lindsay & Norvell (1978) |
| Cu | DTPA Cu, mg kg⁻¹ | 0.3, 0.8 | Local limits: Çokuysal & Erbaş (2004); DTPA test: Lindsay & Norvell (1978) |
| Zn | DTPA Zn, mg kg⁻¹ | 1, 3 | Local limits: Çokuysal & Erbaş (2004); DTPA test: Lindsay & Norvell (1978) |

The micronutrient classes are local limits: Lindsay & Norvell (1978) developed the DTPA extraction and published single critical levels for it (Zn 0.8, Fe 4.5, Mn 1.0, Cu 0.2 mg kg⁻¹ for maize, the Mn and Cu levels tentative), not three classes; the Low/Moderate/High bands are the regional limits Çokuysal & Erbaş (2004) give for that test.

* **Units.** The limits are prefilled when the variable's recorded unit (the **Unit** of the variable list or of *Variable Mapping & Verification*) is the reference unit under any spelling (mg/kg, mg kg-1, mg kg⁻¹, ppm and µg/g for mg kg⁻¹; % for TN), and also when no unit is recorded, with a note naming the method and unit the limits assume. A variable recorded in another unit (K in cmol kg⁻¹, TN in g kg⁻¹) is not prefilled, since a limit in the wrong unit would classify the whole map wrongly; the note says so.
* **Other variables and class counts.** A variable with no reference, or a class count other than three, opens with the *k* − 1 equal-probability quantiles of the displayed Actual surface, weighted by its cells' ground area (the smallest values at which the cumulative area share reaches each probability; Section 2.6), or, before a run, the type 7 quantiles of the data column, labelled "Data quantiles (not agronomic limits)". They describe the data and carry no agronomic meaning.
* **Caveat.** Interpretation limits are crop-, soil- and region-specific. The defaults are only a starting point: replace them with the limits that apply to your crop, extraction method and region.

### 9.3 Default colour palettes

Each variable opens on a default palette until one is picked for it; a picked palette stays with its variable (User Guide, Map Styling). A recognised nutrient (by the rules of Section 9.2, applied to the column name and then to the label) opens on a ramp of its own; the categories *Environmental Data*, *Terrain Data*, *Landsat Data*, *Sentinel Data* and *Merged Data* (exact names) open on viridis; every other variable opens on YlOrRd.

* **Sequential defaults.** A diverging scheme gives equal emphasis to departures on either side of a critical middle value (Harrower & Brewer 2003), and the continuous maps place its middle colour at the midpoint of the surface's range, which is not a property of a concentration, an elevation or a temperature. Its colour order also carries meaning: RdYlBu runs from red at low values to blue at high ones, which would draw the warmest areas of a temperature map blue.
* **Covariate categories.** The Environmental and Terrain categories mix quantities whose colour conventions conflict (warm hues for temperature and radiation, blue for precipitation; brown uplands for elevation, blue for a wetness index), so no single hue family suits all of them. They take viridis, a perceptually uniform scale that stays readable under colour-vision deficiency (Crameri et al. 2020), as the satellite categories do.
* **Nutrient ramps** are sequential ColorBrewer schemes rated safe for colour-vision deficiency, light for low values and dark for high ones.
* The diverging schemes (RdYlBu, BrBG, Spectral) remain in the picker for variables with a meaningful centre, such as a curvature or a topographic position index around zero; uncertainty maps refuse them (Section 7.2).

### 9.4 Class areas

The class areas on the Scientific Analysis tab come from classifying the **full-resolution** raster with the breaks, and each Area Coverage table states every class's range. The interactive map may show a mean-aggregated preview above about 500,000 cells; this affects the display only.

Areas are ground areas: the sum of the exact ellipsoidal areas of each class's cells, the computation the class-zone export and the Classification Suite also use, and the convention of the ruler's areas (Section 3.7). They differ from areas measured in the projection plane by the area scale factor <i>k</i><sup>2</sup>, inside a UTM zone from about −0.08% on the central meridian to +0.2% at the zone edge, so a planimetric re-measurement in a GIS does not reproduce them exactly, and thirty nominal 100 × 100 m cells need not total exactly 30 ha.

The Total (Combined) area table adds the unrounded per-locality areas only when the locality domains are disjoint. A boundary overlap above 0.5 m² (a numerical-sliver tolerance; uploaded boundaries included), or a populated cell that two localities share on one lattice (as at touching boundaries), withholds the combined table and its export, judged separately for the Actual and Predicted surfaces; the per-locality areas remain available.

### 9.5 Class zones as vector polygons

The **Class zones** export writes the displayed classified surface as a vector layer, with the same classification the map and the Area Coverage table use: `terra::classify(rcl_mat, right = FALSE)`, left-closed intervals included, then `terra::as.polygons(dissolve = TRUE)`.

* **Geometry.** One feature per locality and class present, each locality dissolved from its own raster and named in the `locality` attribute (Section 2.6); in the Comparison view each surface is dissolved with its own classes (Section 9.1) and named in the `surface` attribute. Polygon boundaries are cell boundaries: the zones keep the cells of each locality's grid (resampled into the Target Mapping CRS when that is another system) and are not smoothed, contoured or generalised, since any of these would move a boundary away from where the model places it.
* **Area.** `area_ha` is the class's ellipsoidal area (Section 9.4), equal to the hectares the Area Coverage table reports.
* **CRS.** Shapefile and GeoPackage keep the run's Target Mapping CRS, in which the areas were computed; GeoJSON and KML are reprojected to WGS 84, which those formats require. A GIS that re-measures the polygons in the plane will differ from `area_ha` by the projection's area distortion; the attribute is the figure the app reports.
* **Open ends.** The outer breaks are `-Inf` and `Inf`, which GIS formats cannot store, so the first class's `class_min` and the last class's `class_max` are written as NA rather than as an invented finite limit.
* **KML.** The class label goes into the placemark name and the full attribute record (`class`, `class_min`, `class_max`, `area_ha`, `locality`, `surface`, `variable`, `method`) into the description. Use GeoPackage or Shapefile when the attributes must arrive as queryable fields.

### 9.6 GeoTIFF metadata and band statistics

Every exported GeoTIFF carries GDAL metadata tags recording the run: `MONOLITH_VARIABLE` (the variable's display label), `MONOLITH_PRODUCT`, `MONOLITH_METHOD`, `MONOLITH_RUN_ID`, `MONOLITH_APP_VERSION` and `MONOLITH_CREATED`; interpolation surfaces add `MONOLITH_UNIT`, `MONOLITH_LOCALITY` (the localities the file holds) and `MONOLITH_TARGET_CRS`. Continuous rasters keep their values at full floating-point precision (`FLT4S`) and carry their minimum, maximum, mean and standard deviation in the TIFF tags (`STATISTICS_MINIMUM` and so on), so GIS software can stretch the display without recomputing statistics. The Classification Suite's class raster carries the same tags beside its colour table, as 8-bit unsigned class codes (`INT1U`), with its category names in the `.tif.aux.xml` sidecar of the download (Section 10.12). How several localities are written is described in Section 2.6.

---

## 10. Supervised Classification Suite

The Classification Suite trains a **multiclass classifier** directly on a categorical target from co-sampled covariates, as in categorical digital soil mapping, instead of predicting a continuous surface and classifying it.

### 10.1 Targets and learners

The target is an existing categorical column (soil class, land-use label) or a continuous variable binned into ordered classes (quantile, equal-interval or Jenks natural breaks). Jenks breaks are the exact natural breaks over every scoped value (Section 9.1), the computation the map classes use; quantile (type 7) and equal-interval breaks come from `classInt`. None of the three draws random numbers. Intervals are left-closed `[low, high)`, as for the map classes, except the top class, closed at both ends `[low, high]` (`include.lowest = TRUE`) so that the maximum is included; the bin labels show this.

**Numeric columns** are never treated as categorical automatically. Coarse environmental covariates, such as climate-raster precipitation metrics, can carry very few distinct values, and treating an ordered quantity as nominal would dummy-encode it in the recipe and map it by nearest neighbour instead of kriging. Recode numeric class codes to text, or use the binned-target mode.

Three learners share one tidymodels (`parsnip` / `workflows`) backbone:

* **Multinomial logistic regression** (`nnet`; Venables & Ripley 2002), the parametric baseline. A two-class target uses binomial logistic regression (`glm`), to which the multinomial model reduces exactly at the default `penalty = 0`; penalty tuning is then skipped.
* **Random Forest** (`ranger` probability forest; Breiman 2001; Wright & Ziegler 2017), the de facto standard in categorical DSM.
* **Extreme gradient boosting** (`xgboost`; Chen & Guestrin 2016).

All three share one preprocessing recipe, trained within each fit: novel-level absorption, median and mode imputation, dummy encoding of categorical covariates, zero-variance removal, and standardisation of numeric predictors, which aids conditioning and leaves tree splits unchanged apart from numerical effects. Cross-validation and final fitting first drop rows missing any selected predictor or the target, so imputation applies only to reconstructed or external prediction inputs.

### 10.2 Cross-validation and metrics

**Fold designs.** The default is spatial blocked CV (`spatialsample::spatial_clustering_cv`, k-means on the projected coordinates). As in Section 5, the designs answer different questions. Blocks hold out contiguous regions and estimate accuracy away from the sampled clusters, the question for a clustered sample whose gaps the map bridges or for transfer to unsampled ground (Roberts et al. 2017; Ploton et al. 2020; Mahoney et al. 2023). **Standard** class-stratified random k-fold keeps each held-out point's near neighbours in the training set and estimates accuracy within the sampled domain: approximately without bias for a simple random sample (Wadoux et al. 2021), conservatively for a regular grid, and optimistically in the gaps of a clustered sample. Its folds take the samples of each class in random order, the classes one after another, and deal the fold ids in turn along that sequence: a class spreads over as many folds as it has samples (up to k), no held-out fold takes more than one sample of a class smaller than k, and fold sizes differ by at most one. The fold count stays at 10 whatever the class sizes (fewer only below 10 points): lowering it to the smallest class count would close no gap, since a class of two or more samples keeps a training sample in every fold at any k and a singleton misses from one fold at any k, and it would train every model on less data. Folds use the seed 12345.

**kNNDM (map-matched)** applies Section 5.1 to the classification scope: the map's locations are the lattice inside the scope boundary that clips the prediction grid (Section 10.5), and k is the fold count. Its random partition is the Standard folds for the same k and seed: the candidate a cluster partition must beat, the folds used where random folds win or kNNDM cannot search (below 30 points, no prediction domain, no valid partition), and the reference of the CV Distance Match panel under every strategy. Spatial kNNDM folds are used as built, without class stratification, so a class can be missing from a fold's training rows as under spatial blocking. The same search, against the same map locations, forms the nested inner folds (on each outer fold's training rows) and the final model's tuning folds, which equal the CV folds. The run badge and the metrics CSV name the design chosen.

**Unit of analysis: the sampled location.** Rows whose working-CRS coordinates agree to the centimetre are one location, merged at the scope step before the target is built. A numeric column (a covariate, or the variable to bin) takes the mean of the rows that measured it, so a binned target bins the location's mean, with breaks computed over the locations. A class column (a categorical target or covariate) takes the location's most frequent class among the rows that carry one; a tie leaves it missing, since any tie-break would depend on file order. A tied target leaves the location out of the run, a tied categorical covariate out of training, and the launch notification counts both. A covariate value is the mean of every row at the location that measured it, labelled or not, so the training pair takes the value the covariate surface has there. Co-located rows filed under different localities are one location and keep the first row's locality (and polygon); the interpolation pipeline, which runs per locality, keeps them apart. Input with exactly repeated coordinates is refused. The location is the unit because cross-validation estimates accuracy at new locations, so one location's rows must never sit on both sides of a split; McNemar's test (Section 10.9) needs independent pairs; the model is fitted on the support of the kriged covariate surfaces it is applied to, the location mean; and a kriging system with two observations at one location is singular (Section 10.4).

**Covariates at held-out points.** Each assessment point receives covariates built from its fold's training rows, by the path the map uses: numeric covariates by the kriging/IDW builder (Section 10.4), categorical covariates from the nearest training point's class; the measured covariates at held-out points are not used. Outer-fold scoring, out-of-fold permutation importance and every tuning resample, nested inner ones included, use this path, so cross-validation estimates the map task whether or not **Predict maps** is selected; predicting at new sites with measured covariates is a different task. The map's covariate surfaces are kriged from the scoped rows that carry every covariate the model reads, rows without a class included. Each fold does the same for the covariates its own screen keeps: beside its training rows it uses the scoped rows left out of the scored population (for a missing class or covariate) that carry those covariates, without their labels. Nested inner folds use the same extra rows, never the outer held-out locations, and the run summary gives the smallest and largest number of extra rows across the scored folds. The spatial 1-NN baseline uses only the training coordinates and labels.

**Sample size.** The reported n and the model bundle's `n_train` count the locations entering cross-validation and the final fit: valid coordinates, a target, and every selected predictor (for a covariate-free 1-NN run, coordinates and a target suffice). In a successful run each included location receives one out-of-fold prediction. A fitting or prediction failure can abort the run; there is no partial-fold coverage table here.

**Pooled out-of-fold metrics.** Each fold's model predicts hard classes and full class-probability vectors for its held-out points, and the metrics are computed once on the pooled set. Pooling avoids the undefined per-fold macro metrics of a spatial fold that holds a single class, and evaluates every metric, probability metrics included, over the full class set. Tuning computes per-fold accuracy and kappa separately. Reported metrics follow `yardstick`: overall accuracy, Cohen's kappa (Cohen 1960), balanced accuracy, precision, recall, F1, ROC AUC, log-loss and Brier score (Brier 1950). With more than two classes, precision, recall and F1 are macro averages and AUC is the Hand-Till (2001) form; with two classes they use binary estimators with the first factor level as the event. Balanced accuracy is the mean of sensitivity and specificity, macro-averaged one-versus-rest for multiclass targets, rather than multiclass macro recall alone (compare the class-recall definition in Brodersen et al. 2010). Brier is (1/(2n))ΣᵢΣₖ(yᵢₖ − pᵢₖ)², in [0, 1]; see [balanced accuracy](https://yardstick.tidymodels.org/reference/bal_accuracy.html) and [Brier score](https://yardstick.tidymodels.org/reference/brier_class.html). The confusion matrix comes with per-class **producer accuracy** (recall, the complement of omission error) and **user accuracy** (precision, the complement of commission error), the standard per-class report in soil and land-cover classification (Congalton 1991).

**Macro averages.** Every class in the pooled out-of-fold truth counts once in macro precision, recall and F1, and the reported estimator states that denominator (*Macro average (K = 3)*). A class the model never predicts has an undefined 0/0 precision. `yardstick` would drop it from the average with a warning, putting the three figures on different denominators: a macro F1 could then exceed both its components, and a model that ignores a minority class could outscore one that finds some of it. Monolith scores that precision as **0** (the `zero_division = 0` convention), so one denominator holds, and reports it: the per-class table marks the class "no predictions", the run summary names it, and the metrics CSV gives its reference and predicted counts. The same rule applies to the two-class estimator, so a model that predicts no events reports precision and F1 of 0 rather than `NA`. A class with **no reference samples** is a different case: its recall cannot be evaluated, so it and the macro recall are `NA`. This cannot occur in a pooled run, because empty target levels are dropped before fitting.

**Classes missing from a fold's training rows.** Spatial folds are not class-stratified, so a spatially clustered class can fall entirely inside one held-out block; stratified random folds have the same gap for a singleton class, which lands in one fold. That fold's model never saw the class and cannot predict it. The class receives probability **0** from that fold (an output-alignment convention; no posterior is estimated), its held-out samples score as misses, and the run reports which classes were affected. The class's producer accuracy and the pooled log-loss carry the gap; overall accuracy and kappa fall only in proportion to the affected samples. The remedies are fewer classes, quantile rather than equal-interval breaks (which spread samples evenly and rarely strand a class), Standard folds when an in-domain estimate is wanted, or a wider spatial scope.

**Empty target levels.** A binned target has one level per interval, and an equal-interval or Jenks interval can hold no samples, or be emptied when rows with missing covariates are removed. Such a level cannot be learned and is removed before fitting; keeping it would make macro recall and balanced accuracy `NA`, add an empty row and column to the confusion matrix, and leave a map class without a probability layer.

### 10.3 Prediction uncertainty: normalised Shannon entropy

The classifier's counterpart of the kriging-variance map is the normalised Shannon entropy (Shannon 1948) of the predicted class probabilities at each grid cell:

<div style="text-align:center;"><i>H* = - &sum;<sub>k</sub> p<sub>k</sub> ln p<sub>k</sub> / ln K</i></div>

where *K* is the number of classes. *H\** = 0 means the model is certain of a single class, *H\** = 1 that the probabilities are uniform. Entropy describes the model's probability output and is no formal error model: read it as a relative map of where the classifier is least decided.

**Colour scales.** Entropy and the class-probability maps are drawn on the full [0, 1] range, so the probability maps of different classes are comparable with each other and entropy is comparable between runs. A weak model, whose probabilities stay near 1/*K* and whose entropy stays near 1, then gives a map of one flat colour. Each map's subtitle states the observed range, and an opt-in stretch rescales the colours to it. Read a near-uniform class map together with the skill metrics (Section 10.2) and the baselines (Section 10.9): a single class covering the grid is what an unskilled classifier produces once the smoothed covariate surface no longer crosses any decision boundary, and stretching the colour scale shows the remaining structure without making it meaningful.

### 10.4 Covariate surfaces

The classifier is trained at sample locations, but map prediction needs covariate values at every grid cell. Monolith derives them from the samples, not from exhaustive rasters:

* **Numeric covariates** are kriged to the grid with the covariate builder RK and RFK use (Section 1.7), with an IDW fallback (*p* = 2, nmax = 12) on a kriging error, a repeated location, or a solve that returns no prediction. A repeated location makes the kriging system singular, so it is refused before any solve; the suite merges co-located samples first (Section 10.2), so it does not arise here. The run reports each fallback per covariate, on the prediction grid and in how many cross-validation folds. A covariate whose variogram is a pure nugget is mapped at its sample mean away from sampled locations, so the classifier cannot vary with it there; the run reports this the same way.
* **Categorical covariates** cannot be kriged; each grid cell takes the class of its nearest training point.

Both transfers smooth or block the covariates, and that error propagates into the class map, the probability surfaces and the entropy surface. Where wall-to-wall covariate rasters exist (DEM derivatives, satellite bands), sampling them at the grid nodes outside the app is the more rigorous design. Cross-validation rebuilds these transfers in every fold (Section 10.2), so its metrics include their error.

### 10.5 Spatial scope and the prediction domain

**Scope.** A run can be restricted, before anything is fitted, to **localities** (any subset of the locality or grouping column) and to **polygons** drawn on the map or uploaded as a shapefile, either intersected with the selected localities or used alone. Scoping comes before the target is built, so binned breaks are derived from the scoped data only, and all folds, metrics and tuning see only in-scope points. Binned breaks are computed once on the full scope, not within cross-validation, so the metrics are conditional on those class definitions. If intersecting the polygons with the point-derived boundaries fails or gives an empty geometry, the polygon union is used. In polygons-only mode, a point inside overlapping polygons belongs to the first polygon it intersects, for per-area evaluation.

**Prediction domain.** The union of **per-locality boundaries** of the scoped points, intersected with or replaced by the polygon union when a polygon scope is active. Per-locality boundaries, rather than one hull around everything, keep the classifier out of the unsampled corridors between localities, where the covariate surfaces of Section 10.4 would be pure extrapolation. The same boundary gives kNNDM its map locations (Section 10.2), so kNNDM folds follow the scope and its buffer, not the cell size.

**Boundary and grid settings.** The suite has its own Spatial Scope controls (Boundary Type, Buffer Logic, Grid Resolution), independent of the interpolation sidebar (hidden while the suite is active), applied per locality in scope and following the geometric conventions of Section 3. **Buffered** buffers each locality's concave hull by a fixed distance, or dynamically by 2.0 &times; 0.5 &times; that locality's mean nearest-neighbour spacing, clamped to [5, 2000] m (the method-specific multipliers of Section 3.2 are interpolation heuristics and do not apply to a classifier). **Point buffer** unions fixed-radius buffers around the sample points. Grid resolution is **Fixed** (5 to 500 m) or **Auto**, &radic;(<i>A</i> / 50,000) m for a domain of area <i>A</i>, about 50,000 cells inside it, clamped to [5, 1000] m (a 2 km² domain gives 6.32 m). Covariate kriging fills the whole buffered domain, so a buffer extrapolates the covariates beyond the outermost samples, with the same trade-off as the interpolation engines' buffered modes.

**Cell limit.** The raster template spans the full bounding box before the boundary clip, and a multi-part scope covers far more of its box than its hulls do, so in either mode the cell size is coarsened to &radic;( &Delta;<i>x</i> &Delta;<i>y</i> / (4 &times; 10<sup>6</sup>) ) when the box would hold more than about 4 million cells. The Spatial Scope note states the cell size a run will use, with the reason when it was coarsened, and a coarsened Fixed size is repeated when the run finishes. The domain itself is unchanged.

**Metric domain.** Every length here is in metres: the boundary buffers, the grid resolution, the variogram lags of the covariate surfaces and the spatial-CV distances; hectare areas sum exact ellipsoidal cell areas (Section 9.4). A classification run transforms the scoped points into the Target Mapping CRS and works there, so a projected Target Mapping CRS whose unit is not the metre, or whose suitability verdict is a refusal (distortion above 1%, Section 3.6), stops the run with no override. A geographic Target Mapping CRS is allowed: scope resolution then projects all coordinate-valid input rows onto the WGS 84 UTM zone of their mean position, the zone rule of Section 3.5, before the locality and polygon selection, and maps, rasters and exports are produced in that projection.

### 10.6 Hyperparameter tuning

The **Hyperparameter tuning** selector sets how far each learner's parameters are searched before the metrics of Section 10.2 are computed.

* **None (fixed defaults)**, the default. Each learner is fitted at fixed defaults: multinomial `penalty = 0`; Random Forest 500 trees with `ranger`'s built-in `mtry` and `min_n`; XGBoost 500 trees, `tree_depth = 6`, `learn_rate = 0.05`, `min_n = 2`. Fast, deterministic and free of selection optimism.
* **Light:** a 10-point space-filling grid over `penalty` (multinomial), `mtry` and `min_n` (Random Forest), and `tree_depth` and `learn_rate` (XGBoost).
* **Full:** a 30-point grid over the same parameters for multinomial and Random Forest, and for XGBoost also `mtry`, `min_n`, `loss_reduction` and `sample_size`.

At Light or Full the grid is by default evaluated on the same folds as the reported metrics and the best combination by accuracy is refitted, so tuned metrics carry selection optimism of unknown size.

The tuning folds always follow the run's resampling strategy, in the cross-validation loop and for the final full-data fit: spatial clustering under Spatial CV, the same map-matched folds under kNNDM, and class-stratified random folds under Standard CV. The final model's hyperparameters determine the class map, the entropy surface, the importance ranking and the exported model; tuning under random folds while reporting spatial-CV metrics would select the model that profits most from the near-neighbour leakage spatial CV removes.

**Nested cross-validation** (*Use nested CV (slower)*, shown when a tuning depth is active) separates tuning from scoring. Each outer fold reruns the grid search on **5 inner folds** built from that fold's training rows only, under the outer resampling strategy (the spatial analogue of the recommendation of Schratz et al. 2019), and the winning workflow predicts the held-out fold. One candidate grid is shared across outer folds, its data-dependent ranges finalised on the full predictor frame. Class weights are recomputed from each tuning split's own training rows (Section 10.11). The pooled metrics then evaluate the whole search, conditional on the full-scope class definitions and selected predictors (Sections 8.1, 10.5); nested evaluation is the usual way to assess model selection (Varma & Simon 2006; Cawley & Talbot 2010).

* No single best parameter set exists under nesting: the per-fold winners are collected (`nested_params`, one row per fold), and their agreement is itself a stability check.
* The deployed and exported model is still tuned once on all training data, the standard practice: nested CV estimates the skill of the procedure, and the final model uses all the data. The bundle records that model's own hyperparameters.
* Cost grows by about the number of inner folds. Depth None is unaffected by the checkbox.

### 10.7 Per-area performance

When the scope holds more than one locality (or polygon, in polygons-only mode), the **Performance by Area** table splits the pooled out-of-fold predictions by area and reports n, overall accuracy, Cohen's kappa, balanced accuracy and macro F1 per area, with a Total row reproducing the pooled metrics. F1 uses the binary estimator for two-class targets, as in Section 10.2.

* The folds are global, and spatial clusters need not respect locality borders, so an area row evaluates a slice of one jointly trained model, not an independently validated per-area model (unlike the interpolation engines, which fit one model per locality).
* Small areas give unstable estimates, and a metric undefined for an area (a class that never occurs there, for example) is NA rather than imputed. Probability metrics are left out of this table for the same reason.
* An area's macro F1 averages the classes with **reference samples in that area**: a class the pooled target holds but the area does not is excluded, while a class the area holds and the model never predicts there scores 0, by the fixed-set rule of Section 10.2. Two areas' macro figures can therefore average over different class sets, so each row reports its denominator as **Classes (K)** (empty for a two-class run, which reports the event class's own F1).

### 10.8 Permutation feature importance

Every run reports **model-agnostic permutation importance** (Breiman 2001; Fisher, Rudin & Dominici 2019): each covariate in turn is shuffled, which breaks its association with the target while keeping its distribution, the model re-predicts, and the increase in **multiclass log-loss** over the unshuffled baseline is recorded (mean of 5 shuffles under the fixed seed). Log-loss uses the full probability vector, so it registers importance even when a shuffle rarely changes the predicted class. Computed on the raw covariates through the whole fitted workflow, it is comparable across the three learners, unlike engine-native measures (ranger impurity, xgboost gain, multinomial coefficients), which have different scales.

**Rows scored** (*Feature importance scored on*):

* **Out-of-fold (default):** each cross-validation fold's own model shuffles the covariates of the rows it never saw, and the per-fold increases are pooled by fold size, which gives exactly the increase one evaluation over the whole out-of-fold set would give. It measures importance under the same design as the reported accuracy and needs no extra model fits. With very small folds the shuffle decorrelates a covariate less thoroughly, so importance is slightly understated.
* **Training rows:** the final model scored on the rows it was fitted on, the conventional default (compare `vip::vi_permute`). Rankings are informative, but magnitudes lean optimistic for flexible learners that partly memorise their training set.

The design in force is written on the plot axis, in the exported metrics CSV and in the results table, since out-of-fold and training-row importances are not comparable numbers. Negative values mean shuffling improved the score; they can reflect noise, dependence or misspecification and do not prove a predictor is noise.

Under **Auto-Drop** the collinearity screen is part of each model (Section 8.1), so each fold shuffles only the covariates it kept; a covariate's pooled value is the fold-size-weighted mean over the folds that kept it, and a covariate no fold kept is absent from the table rather than listed as unimportant. The training-row design shuffles the covariates the final model kept. Without Auto-Drop every model holds every covariate, and the pooled value is the plain fold-size-weighted mean.

Each fold's shuffles use their own seed, derived from the run seed and the fold index, so the predictions, metrics, confusion matrix and per-class table are the same whichever design is chosen, and each fold's importance is reproducible.

Correlated covariates distort importance through substitution and through unrealistic combinations after shuffling (Strobl et al. 2008), so the collinearity screen of Section 8.1 runs first. The displayed *share (%)* rescales the positive importances to 100. Because this dilution sets in well below VIF 10, the classification screen tightens to **VIF > 5** when Random Forest is selected, the usual moderate-collinearity bound (James et al. 2013; such rules are advisory, O'Brien 2007). Keeping the covariates hardly affects the predictions and mainly weakens the importance interpretation, so the screen stays the user's decision.

### 10.9 No-covariate baselines and covariate lift

The suite compares the covariate model's accuracy with a fold-evaluated spatial baseline and a no-information rate:

* **Majority-class rate:** the largest class share in the complete-case evaluation population, computed from all its labels, not from fold-trained majority predictions. A constant prediction of that class would have kappa 0 when there are at least two classes; that kappa is not computed.
* **Spatial 1-NN baseline:** each held-out point receives the class of its nearest training point (Euclidean distance, projected coordinates), the categorical counterpart of nearest-neighbour interpolation: what spatial proximity achieves without covariates. Under spatial blocked CV it must transfer across fold boundaries as the model does.

**Covariate lift** is the accuracy difference between the covariate model and the spatial baseline, in accuracy points on identical folds. **McNemar's test** on the discordant pairs (McNemar 1947; Dietterich 1998) tests the paired difference: the exact binomial form below 25 discordant pairs, where the approximation is unreliable, and the continuity-corrected &chi;² from 25 on; NA when there are no discordant pairs. McNemar assumes independent paired outcomes; the pairs are sampled locations (Section 10.2), so replicate rows never enter as separate pairs, but spatial dependence and overlapping training sets are not adjusted for, and the p-value is exploratory. Non-significance does not establish equivalence.

The test is two-sided, so direction comes from the lift and significance from p: p ≥ 0.05 means no demonstrable difference; p < 0.05 with a positive lift means the covariate model is more accurate; p < 0.05 with a negative lift means it is significantly *less* accurate than spatial proximity alone. In that case the panel points to the no-covariate alternative that suits the target: for a **binned continuous** target, interpolating the continuous variable (Spatial Engine) and classifying the surface, which uses the full continuous information and the order of the classes; for a **categorical** target, whose codes cannot be interpolated, the Spatial 1-NN map below. Under random k-fold the note adds that the 1-NN baseline is favoured, since each held-out point keeps its near neighbours in the training set (Roberts et al. 2017), so the comparison should be repeated under spatial blocked CV. Under kNNDM no such note is added: its folds hold points out at the distances the map predicts from, so a 1-NN advantage there is an advantage for this map.

**Spatial 1-NN map (categorical targets).** The baseline is also available as a surface: every grid cell takes the class of its nearest sample, a Thiessen (Voronoi) mosaic of the samples' classes, built on the same grid as the covariate model and from the point set the baseline was scored on, so the *Spatial baseline (1-NN)* accuracy in the lift table is this map's cross-validated accuracy. Its class boundaries follow cellwise approximations to the bisectors between samples of different classes and say nothing about where the class actually changes. Nearest-neighbour assignment gives hard classes only: no class probabilities, and hence no entropy map, probability map or confidence threshold. The class-area table and the class GeoTIFF follow the map selected. Indicator kriging, which would give probabilities without covariates, is not implemented.

**Covariate-free runs (categorical targets).** With no covariate selected, the spatial 1-NN classifier is evaluated as the model itself: folds are built as for a covariate run (same seed and strategy, on the points carrying a target), each held-out point takes the class of its nearest training point, and the pooled out-of-fold predictions are scored with the class metrics of Section 10.2, without probability metrics. On data without missing covariates these predictions are identical to the spatial baseline of a covariate run, so the two runs are directly comparable. The comparison shown is with the majority-class rate; no lift is computed. Binned continuous targets are excluded, since interpolating the continuous variable before binning uses information that nearest-neighbour assignment of bin labels discards.

### 10.10 Confidence thresholding (abstention)

The predicted-class map supports a **confidence threshold**, the classical reject option (Chow 1970): cells whose largest class probability falls below the threshold are assigned an explicit **Unclassified** category, drawn grey with its own row in the area table, instead of a weak most-probable class. Abstained areas are natural candidates for further sampling.

The threshold reclassifies the existing probability surface without refitting, so it can be explored interactively, and the exported class GeoTIFF follows the current setting (probability and entropy rasters are never masked). A threshold at or below 1/K never applies, since the largest of K class probabilities is at least 1/K. Beside the map, a readout reports from the pooled out-of-fold points the share that would be retained at the chosen threshold and their accuracy. Cross-validation metrics are always computed on all points, without abstention.

### 10.11 Class-imbalance weighting

The optional **Balance classes** switch applies inverse-frequency case weights, *w<sub>c</sub> = n / (K · n<sub>c</sub>)*, so each class contributes equally to the training loss whatever its sample count (King & Zeng 2001; the `class_weight="balanced"` convention), countering the tendency of accuracy-driven learners to ignore rare classes.

* Weights affect **fitting only**; all metrics stay unweighted, since balanced accuracy and the macro averages already show minority-class performance. Expect weighted runs to trade some overall accuracy for better minority-class recall.
* Weights are **recomputed from the training rows of every fit**: each outer fold, each tuning resample (non-nested, final-model and nested inner tuning alike) and the final model, so no held-out label enters the class frequencies.
* Weights apply to Random Forest and XGBoost. The multinomial workflow does not take case weights, and the run badge then reports "weights unsupported (unweighted)".
* Synthetic oversampling (SMOTE and similar) is not offered: Monolith has no spatially justified synthetic-sampling procedure, and any resampling would have to stay inside training folds and preserve the prediction task. Weighting does not replace representative observations or calibrate class probabilities.

### 10.12 Exports

**Class raster.** The class GeoTIFF stores integer class codes (`INT1U`) with the map's colour table and the metadata tags of Section 9.6. GDAL keeps the category names in a `.tif.aux.xml` sidecar beside the file, not inside it, so a bare `.tif` opens as unlabelled codes; the download is therefore a zip of the GeoTIFF, its sidecar and a two-column legend CSV (`ID,class`) that names the codes without the sidecar.

**Class areas.** The per-class hectares sum each cell's ellipsoidal ground area, as the interpolation Area Coverage table and the class-zone export do (Section 9.4).

**Classification Performance Metrics** is one CSV holding several tables: the pooled metrics (accuracy, kappa, balanced accuracy, macro precision, recall and F1, ROC AUC, log-loss, Brier); the per-class table (producer accuracy, user accuracy, reference support and prediction counts); the out-of-fold confusion matrix; the classified area in hectares and percent; and the fold design, with W for its folds and for random folds (Section 10.2).

**Trained model.** **Download Model (.rds)** saves the final fitted tidymodels workflow with its training metadata: method, covariates, the covariates the collinearity screen removed and its VIF threshold, class levels, weighting, tuning depth and selected hyperparameters, the CRS the run was computed in, n, a timestamp, and the R, parsnip, workflows, recipes and engine package versions. The bundle loads in any R session with compatible package versions and predicts from new covariate data without retraining:

```r
b <- readRDS("classification_model_rf_20260710.rds")
predict(b$workflow, new_data, type = "prob")   # or type = "class"
```

`new_data` must contain the covariate columns listed in `b$predictors`; the preprocessing recipe (imputation, dummy encoding, normalisation) is embedded in the workflow and replays automatically. Predictions match the app's for the same covariate inputs. The workflow does not rebuild covariate surfaces, rasterise predictions or apply the confidence threshold. `.rds` serialisation is version-sensitive for the xgboost engine: reuse the bundle under the package versions recorded in `b$versions`.

---

## 11. Fixed Constants and Where to Change Them

The constants below are fixed in the source so that runs are reproducible; advanced users can change them in the files named. Package defaults are not listed, and controls exposed in the interface appear only where their default or range affects the method. Bracketed numbers point to the section that explains each one.

**Random numbers.** Every seeded step runs inside `with_seed()` (`spatial_vgm.R`), which names the generator before seeding (R's defaults: Mersenne-Twister uniforms, Inversion normals, Rejection sampling) and restores the caller's random state afterwards. Naming it makes one seed give one stream: the app's background workers run an L'Ecuyer-CMRG stream, and `set.seed()` alone keeps whatever generator is in force, so the same seeded call would otherwise give one result in the app and another in a script or test. This covers fold assignment, the spatial k-means blocks, each RFK forest (map and fold), the K-Means class breaks, the Moran duplicate-coordinate displacement, classification tuning and fitting, and the Governing Factors forest and SHAP sample. Each worker resumes its own stream afterwards, so parallel runs stay independent.

**Cross-validation seed.** `CV_FOLD_SEED = 12345` in `spatial_metrics.R` seeds realization 1; later realizations add 1, 2, ... Change it to score a run on a different partition. `make_cv_folds()` (`spatial_metrics.R`) is the single source of fold assignments for the six interpolation engines and for the IDW power search, outer and inner folds alike. The kriging engines reach it through `build_cv_plan()`, which folds each cross-validation population once per locality and surface and hands the same plan to OK, CK, RK, RFK and the OK fallback, so a population's folds do not depend on the engine that scores it. kNNDM folds come from `knndm_folds()`, on the map locations `knndm_domain_points()` draws inside the surface's boundary; its random candidate is the seeded random 10-fold of `cv_random_folds()`, the folds Auto uses. Each RFK fold's forest is drawn from `CV_FOLD_SEED` + its fold label, because a forest's number of random draws depends on its data: one stream across all folds would let earlier folds decide where a later fold's forest starts. The Classification Suite uses its own default seed 12345 (`classif_helpers.R`).

**Moran's I neighbours.** Change the `8L` in `k_nn <- min(8L, nrow(coords) - 1L)` (`calc_moran()`, `spatial_metrics.R`) for a more local or a wider neighbourhood. The permutation count and seed are `MORAN_NSIM` (999) and `MORAN_PERM_SEED` (12345) in the same file; the smallest attainable p-value is 2 / (`MORAN_NSIM` + 1). Duplicated coordinates are displaced by 1e-9 of the coordinate span, floored at 1e-12 of the coordinate magnitude and at 1e-8 map units, because a fixed nudge can round back onto the original value at projected magnitudes (a UTM northing near 4.5 × 10⁶ is stored in steps of about 10⁻⁹); all three are orders of magnitude below any real sample spacing.

**Cross-validation thresholds.** In `resolve_cv_plan()` and the constants above it (`spatial_metrics.R`): Auto uses LOOCV at 50 or fewer observations (the `n > 50` test); Spatial Block falls back to LOOCV below `CV_BLOCK_MIN_N` (30) and kNNDM below `CV_KNNDM_MIN_N` (30); the fold and block count is `CV_FOLD_K` (10). kNNDM's own settings are the `KNNDM_*` constants listed below. Each changes only the folds of a kNNDM run, except `KNNDM_DOMAIN_N`, which also sets the map points the CV Distance Match panel reads under every strategy.

**Point sets and variograms**

| Constant | Value | Where |
|---|---|---|
| Co-located samples | coordinates agreeing to 2 decimals (1 cm in a metric CRS); numeric values and coordinates averaged, the smallest row id kept; in the Classification Suite, class columns by majority (missing on a tie) and the first row's locality [1.7, 10.2] | `merge_colocated`, `dedup_valid_points` (`spatial_pipeline.R`), `classif_resolve_scope` (`classif_helpers.R`) |
| Minimum points per locality surface | 3 | `run_regional_interpolation` |
| Minimum samples in a cross-validation population | 3; below it the surface is mapped and its CV skipped with a named log line [5.3] | `.engine_cv_plan` (`spatial_kriging.R`) |
| CV population ID | first 8 hexadecimal characters of the MD5 of `<target key>\|<row ids>\|<realization-1 fold ids>` [5.3] | `cv_population_id` (`global_utils.R`) |
| Empirical variogram cutoff and lag width | cutoff = half the bounding-box diagonal; width = cutoff / 15 | `calc_scientific_lags` (`spatial_vgm.R`) |
| Pooled within-locality variogram (Total) | a locality enters with at least 10 distinct located values; common lag classes of the eligible locality with the smallest cutoff [4.4] | `POOLED_VGM_MIN_N`, `pooled_within_variogram` (`spatial_vgm.R`) |
| Auto-fit families | Sph, Exp, Mat (&nu; = 1.5), in that order for a tie; the Gaussian is a manual model only [4.1] | `VGM_AUTOFIT_FAMILIES` (`spatial_vgm.R`) |
| Auto-fit range search | practical ranges from max lag / 100 to 10 × max lag; 200 log-spaced points, plus every lag distance for the spherical model; basins within 10⁻³ of the best refined by Brent's method (a flat run also over each end interval); nugget and partial sill by non-negative least squares [4.1] | `VGM_PROFILE_GRID_N`, `VGM_RANGE_CEILING_MULT`, `VGM_CONTENDER_REL`, `.vgm_profile_family`, `.vgm_nnls_cols` (`spatial_vgm.R`) |
| Shape-column floor | a range at which the model's shape has weighted variance &le; 10⁻¹² across the bins cannot be told from a nugget and is fitted as a pure nugget [4.1] | `.vgm_nnls_cols` |
| Equally-fitting set | ranges whose criterion lies within 10⁻⁸ (relative) of the minimum; represented by the middle of its nugget interval, or by the midpoint of its edges when no profile point lies inside it [4.1] | `VGM_SSE_TIE_REL`, `.vgm_profile_family` |
| Nugget identified | the equally-fitting nugget-share interval spans at most 1 percentage point; otherwise Structural Dependency is an interval [4.1] | `VGM_NUGGET_ID_WIDTH` (`spatial_vgm.R`) |
| Candidate validity (eligibility) | finite fit criterion; partial sill > 0; practical range finite and > 0; nugget &ge; 0; no zero nugget anywhere in a Matérn's equally-fitting set [4.1] | `.vgm_autofit_eligible` |
| Range resolved (diagnostic and tie-breaker) | practical range in (max lag / 100, 2 × max lag), and the equally-fitting set clear of the search floor [4.1] | `robust_vgm_fit` |
| Numerical tie on the fitting criterion | relative criterion difference below 10⁻⁸; only then do a resolved range, and then the family order, decide [4.1] | `VGM_SSE_TIE_REL`, `.vgm_pick_best` (`spatial_vgm.R`) |
| Sill resolved (what may be claimed) | practical range &le; max lag; stricter than the range-resolved window, and separate from it [4.1] | `robust_vgm_fit`, `vgm_params_row` (`ui_formatting.R`) |
| Rising at the cutoff (trend advisory) | mean &gamma; of the last third of the bins exceeds the previous third by more than 5% of the &gamma; span; each third at least 2 bins, NA with fewer than 4 bins or a flat variogram [4.1] | `.vgm_still_rising` (`spatial_vgm.R`) |
| Degenerate target (parameters suppressed, kriging variance withheld) | sd &le; 10⁻⁸ × max\|x\|; RK/RFK residuals against the target's max\|x\| [4.1, 7.4] | `.is_degenerate_covariate`, `.residuals_degenerate` (`spatial_kriging.R`) |
| Practical-range factors | Sph 1, Exp 3, Gau &radic;3, Mat (&nu; = 1.5) 4.75 [4.1] | `.vgm_practical_range_factor` (`spatial_vgm.R`) |
| Pure nugget (no eligible family) | the flat fit: nugget = weighted mean semivariance, no structure [4.1] | `robust_vgm_fit` |
| Stand-in variogram (`fit_failed`) | fewer than 5 usable lag bins, or every semivariance zero: Sph, 80% partial sill and 20% nugget of the sample variance (1 when it is zero), range max lag / 2; predictions kept, kriging variance withheld [4.1, 7.4] | `robust_vgm_fit`, `vgm_variance_withheld` (`spatial_vgm.R`) |
| Gau/Mat nugget advisory | nugget below 5% of the total sill: any fitted or applied model of the displayed run, on the map banner, and a manual model when applied [4.1]; any direct variogram of a co-kriging LMC, in the run log [1.4] | `VGM_SMOOTH_NUGGET_WARN_SHARE` (`spatial_vgm.R`) |
| Manual variogram sliders | nugget and partial sill 0 to 2 × max(variance, stored total sill); range to max(1.5 × bounding-box diagonal, 3 × stored range); about 200 steps with rounded step sizes [4.1] | `manual_vgm_slider_spec` (`ui_formatting.R`) |

**Engines**

| Constant | Value | Where |
|---|---|---|
| CK LMC correction | `correct.diagonal = 1.01`; range seed cutoff / 2 when the primary fit is the stand-in or a pure nugget [1.4] | `.ck_fit_lmc` (`spatial_kriging.R`) |
| CK neighbourhood | slider 5 to 60, default 15 [1.4] | `ck_nmax` (`ui_sidebar.R`) |
| CK covariate collocation | a covariate enters with at least 10 locations measured together with the target; checked on the map's rows, not per fold [1.4] | `CK_MIN_COLLOCATED` (`spatial_kriging.R`) |
| IDW power | Fixed: default 2, 0 to 48; Auto (CV): 37 candidates (0, 0.25 to 6 by 0.25, 7, 8, 9, 10, 12, 14, 16, 20, 24, 32, 48, ∞), smallest on ties; stepped-map warning from 24; p = 2 below 5 samples or without usable variance [4.2] | `IDW_POWER_GRID`, `IDW_MAX_FINITE_POWER`, `IDW_SELECT_MIN_N`, `IDW_STEEP_POWER`, `select_idw_power`, `idw_cv_profile` (`spatial_kriging.R`) |
| IDW neighbours | slider 4 to 50, default 12 [1.5] | `idw_nmax` (`ui_sidebar.R`) |
| Covariate IDW fallback | p = 2 and the run's Max Neighbors (default 12), on a kriging error, a repeated location or a solve that returns no prediction [1.7, 10.4] | `krige_covariates` (`spatial_kriging.R`), `run_regional_interpolation` |
| TPS lambda | Auto (GCV) default, Exact 0, Fixed > 0; coordinates divided by the larger extent (`scale.type = "unscaled"`); near-planar warning below 3.5 effective df; Exact cross-validated when GCV ends at 0.95*n* df [4.3] | `apply_TPS`, `tps_gcv_end` (`spatial_kriging.R`) |
| RFK trend forest | `ntree = 200`; `mtry`, `nodesize` at `randomForest` regression defaults; map forest seed 12345, fold forests 12345 + fold number [1.3] | `rfk_ntree_val` (`server_execution.R`), `CV_FOLD_SEED` (`spatial_metrics.R`) |
| Interpolated point-error surface | IDW, `idp = 2`, run's Max Neighbors [6.2] | `run_regional_interpolation` |
| Collinearity screen | VIF > 10; pairwise \|r\| > 0.95 reported; constant when sd &le; 10⁻⁸ × max\|x\| [8.1] | `detect_multicollinearity_engine`, `.is_degenerate_covariate` (`spatial_kriging.R`); `locality_screen_preview(vif_threshold = 10)` (`spatial_pipeline.R`), called by the run's screen (`server_execution.R`) |

**Grid and boundary**

| Constant | Value | Where |
|---|---|---|
| Auto run grid | min(0.0791 &radic;(A / N), h<sub>NN</sub> / 2), bounded to [1, 1000] m (Hengl 2006); Auto (Global) uses the finest locality value for every locality, on a lattice snapped to multiples of it [2.1, 2.2] | `HENGL_POINT_FACTOR`, `AUTO_RES_MIN`, `AUTO_RES_MAX`, `auto_grid_resolution`, `shared_auto_resolution` (`spatial_pipeline.R`) |
| Grid cells | square, exactly the resolution; grid extended by whole cells from the bounding box's lower-left corner [2.4] | `grid_template` (`spatial_pipeline.R`), also used by `classif_build_grid` |
| Run grid, every mode | at most about 4 × 10⁶ bounding-box cells; Fixed absolute floor 0.1 m [2.5] | `cap_res` in `run_regional_interpolation` |
| Resolution suggestion | 0.5 × mean nearest-neighbour distance, floored at extent / 300 and clamped to [1, 500] m; degree distances through Web Mercator × cos(mean latitude) [2.3] | resolution observer (`server_data_setup.R`), `calc_metric_spacing` (`spatial_pipeline.R`) |
| Dynamic buffer | TPS 1.0, IDW 2.0, OK/CK/RK/RFK 3.0 × the Fixed cell size, or × half the mean nearest-neighbour distance in Auto modes; clamped to [5, 2000] m [3.2] | `buffer_multipliers`, `get_buffer_multiplier`, `locality_buffer_res`, `dynamic_buffer_dist` (`spatial_pipeline.R`) |
| Sidebar defaults | Buffer Distance 250 m; Manual Resolution 50 m (slider 1 to 500 m) | `ui_sidebar.R` |
| Run archive limit | 3 completed runs (`MAX_RUN_HISTORY = 3L`); the oldest archived run is evicted | `server_execution.R` |

**Cross-validation and metrics**

| Constant | Value | Where |
|---|---|---|
| Fold count | 10 for every k-fold plan (random, kNNDM, Spatial Block) and for the random reference of the CV Distance Match [5, 5.1] | `CV_FOLD_K` (`spatial_metrics.R`) |
| Spatial Block k-means | 10 centres, `nstart = 5`, `iter.max = 50`; seeded random 10-fold when k-means fails [5] | `make_cv_folds` (`spatial_metrics.R`) |
| kNNDM floor and gate | LOOCV below 30 samples; one-sided Kolmogorov-Smirnov test, random folds at p ≥ 0.05 [5.1] | `CV_KNNDM_MIN_N`, `KNNDM_KS_ALPHA` (`spatial_metrics.R`) |
| kNNDM candidates | Ward (`ward.D2`) tree, 100 log-spaced cuts from 10 to n − 2; above 2000 samples, on at most 2000 size-weighted cells with 30 cuts; the seeded random partition replaced only by a strictly smaller W; no fold above half the samples [5.1] | `KNNDM_EXACT_MAX`, `KNNDM_NQ_EXACT`, `KNNDM_MAX_CELLS`, `KNNDM_NQ_CELLS`, `KNNDM_MAXP`, `knndm_folds` |
| kNNDM map locations | square lattice of about 1000 points inside the boundary, spacing &radic;(area / 1000); spacing divided by &radic;2, up to 5 times, while fewer than 500 fall inside; a thinning of the grid without a boundary [5.1] | `KNNDM_DOMAIN_N`, `knndm_domain_points` |
| CV Distance Match | percentiles 0 to 100% in steps of 1% of the three nearest-neighbour distance distributions; W (Wasserstein-1) for the run's folds and for the seeded random 10-fold (the Standard folds in the Classification Suite) [5.1, 10.2] | `cv_distance_summary`, `cv_wasserstein1` |
| Repeated CV | the interface offers 3, 5 or 10 realizations; engine cap 25 [5.4] | `cv_repeat_n` (`ui_sidebar.R`), `cv_repeat_count` |
| Moran's I p-value | two-sided permutation test, 999 permutations, seed 12345; smallest attainable p 0.002 [5.6] | `MORAN_NSIM`, `MORAN_PERM_SEED`, `moran_perm_p` (`spatial_metrics.R`) |
| Moran's I all-pairs fallback | only at n &le; 500 [5.6] | `calc_moran` |
| Stored metric precision | full precision; displays format to 4 significant digits [5.5] | `perform_cv`, `augment_metrics`, `format_sig` |

**Target Mapping CRS**

| Constant | Value | Where |
|---|---|---|
| Working CRS | the WGS 84 UTM zone of the locality's mean position for geographic or non-metre input; metre-based input used as supplied [3.5] | `validate_and_project_sf` (`spatial_pipeline.R`) |
| Metre units | a projected Target Mapping CRS must have the metre as its unit [3.6] | `validate_crs(require_metric = TRUE)` (`global_utils.R`) |
| Suitability thresholds | \|k − 1\| > 0.1% warns, > 1% refuses; arc 0.001° [3.6] | `crs_target_suitability`, `crs_scale_factor` (`global_utils.R`) |
| Identification from lon/lat | agreement within 5 m; rivals at least 100 × worse [3.5] | `identify_crs_from_lonlat` |
| Identification from a boundary | winner holds &ge; 50% of points; runner-up at most half of that [3.5] | `identify_crs_from_boundary` |
| Degree-range identification | lon/lat column names, or a boundary holding &ge; 50% of the points read as degrees; otherwise EPSG:4326 is offered, not set [3.5] | `identify_input_crs` |
| Candidate shortlist | within 500 km of a clicked point; positions within 250 m folded; 8 rows; text search capped at 120 codes [3.5] | `crs_candidate_shortlist`, `crs_registry_filter_area` |

**Map classes and exports**

| Constant | Value | Where |
|---|---|---|
| Binned styling | 5 equal-interval classes over each surface's own values (both surfaces pooled under Match Scales) [9.1] | `build_classification_params` (`server_run_config.R`) |
| Agronomical classes | slider 2 to 5, default 3 | `agro_n_classes` (`ui_sidebar.R`) |
| Jenks and K-Means breaks | Jenks exact by dynamic programming; K-Means from k-means++ starts under seed 12345, Lloyd iterations until no value changes class, at most 1,000 [9.1] | `natural_breaks`, `kmeans_breaks`, `KMEANS_MAX_ITER`, `calc_class_breaks` (`spatial_pipeline.R`) |
| Supervised limits, recognised nutrient | published three-class limits (total N, Olsen P, NH₄OAc K/Ca/Mg, DTPA Fe/Mn/Cu/Zn), offered at three classes when the unit matches or is empty [9.2] | `NUTRIENT_REFERENCE`, `get_nut_key`, `reference_unit_status`, `class_limit_defaults` (`ui_colors.R`); the note under the limit boxes is `class_limit_note` (`ui_formatting.R`) |
| Supervised limits, otherwise | quantiles of the Actual surface at probabilities 1/k, ..., (k − 1)/k, weighted by cell ground area; type 7 on the data column before a run [9.2, 2.6] | `class_limit_defaults`, `weighted_quantile` (`ui_colors.R`) |
| Pooled cell values | localities in name order; each value weighted by its cell's ground area where areas differ by more than 10⁻⁹ relative (exact ellipsoidal cell area in every CRS) [2.6] | `pooled_raster_values` (`spatial_pipeline.R`) |
| One raster for several localities | same CRS, cell size and origin within 10⁻⁶ of a cell, boundaries overlapping by at most 0.5 m², and no shared populated cells; else one GeoTIFF per locality [2.6] | `rasters_share_lattice`, `rasters_have_shared_cells`, `LOCALITY_OVERLAP_TOL_M2` (`spatial_pipeline.R`) |
| Combined class areas | withheld above a 0.5 m² boundary overlap or for a shared populated cell [9.4] | `LOCALITY_OVERLAP_TOL_M2` (`spatial_pipeline.R`) |
| Map display preview | mean-aggregated above about 500,000 cells, display only [9.4] | `LEAFLET_DISPLAY_MAX_CELLS` (`server_setup.R`) |
| Class zones | the map's classification dissolved per locality and class; areas from the exact ellipsoidal cell areas [9.5] | `build_class_zone_sf`, `raster_class_areas` (`spatial_pipeline.R`) |
| Default palettes | TN Greens, P Blues, K Oranges, Ca YlOrRd, Mg PuBuGn, Fe Purples, Mn GnBu, Cu YlGn, Zn YlOrBr; viridis for Environmental, Terrain, Landsat, Sentinel and Merged Data; YlOrRd otherwise [9.3] | `get_default_palette`, `nutrient_palettes`, `VIRIDIS_DEFAULT_CATEGORIES` (`ui_colors.R`); the picker's scales are `dashboard_palettes` (`global_utils.R`) |

**Exploratory suite and Governing Factors**

| Constant | Value | Where |
|---|---|---|
| PCA minimum | 3 selected variables and 5 complete rows; at least 2 variables with variance [8.1, 8.8] | PCA observer (`desc_exploratory_module.R`), `desc_pca_fit` (`ui_formatting.R`) |
| PCA near-duplicate screen | pairwise \|r\| > 0.95 [8.1] | `check_collinearity` (`ui_plotting.R`) |
| Mahalanobis screen | &chi;² 97.5% quantile; components with sd &le; 10⁻⁸ × max sd excluded [8.2] | `generate_pca_mahalanobis` (`ui_plotting.R`) |
| Spatial cross-correlogram | at least 15 locations; bins greyed below 30 pairs; Distance Bins 3 to 50, default 15 [8.4] | `compute_spatial_cross_correlogram`, `generate_spatial_cross_correlogram` (`ui_plotting.R`) |
| Partial correlation | at least 5 complete rows; Pearson/Spearman residual test df > 0 and residual norm > 10⁻⁷ × original centred norm [8.3] | correlation table (`desc_exploratory_module.R`) |
| Normality test | Shapiro-Wilk below n = 5000, Lilliefors at or above; &alpha; = 0.05; no test below n = 3 [8.8] | `compute_normality` (`desc_exploratory_module.R`) |
| Governing Factors | at least 50 complete rows; seed 12345; trees 50 to 500 (default 100), SHAP sample 50 to 1000 (default 100) [8.8] | `compute_governing_factors` (`spatial_pipeline.R`), `gov_module.R` |

**Classification Suite**

| Constant | Value | Where |
|---|---|---|
| Folds and seed | 10 folds (fewer only below 10 points), seed 12345; Standard folds class-stratified; kNNDM with the interpolation constants, k = the fold count and the Standard folds as its random partition [10.2] | `classif_make_fold_id`, `classif_stratified_folds` (`classif_helpers.R`) |
| Nested CV | 5 inner folds [10.6] | `run_classification_cv(inner_v = 5)` |
| Tuning grids | Light 10 points, Full 30 points; depths, parameters and grid sizes defined in one registry that feeds the selector and the fitting path [10.6] | `.classif_tuning_registry` (`classif_helpers.R`) |
| Learner defaults | multinomial `penalty = 0`; RF 500 trees; XGBoost 500 trees, depth 6, learn rate 0.05, `min_n = 2`, `loss_reduction = 0`, `sample_size = 1` [10.6] | `.classif_method_defs` |
| Permutation importance | 5 shuffles per covariate [10.8] | `run_classification_pipeline(importance_reps = 5)` |
| Collinearity screen | VIF > 10; VIF > 5 with Random Forest [10.8] | `active_vif_threshold` (`classif_module.R`) |
| McNemar test | exact binomial below 25 discordant pairs [10.9] | `classif_covariate_lift` |
| Unit of analysis | the sampled location: co-located rows (1 cm) merged before the target is built; numeric values averaged, class columns by majority, missing on a tie; exactly repeated coordinates refused by the pipeline [10.2] | `classif_resolve_scope`, `run_classification_pipeline` (`classif_helpers.R`) |
| Scope adequacy warning | fewer than 20 complete locations, or a class with fewer than 3 samples | `classif_scope_adequacy` |
| Auto grid | &radic;(A / 50,000) m, about 50,000 cells, clamped to [5, 1000] m before the limit of about 4 × 10⁶ bounding-box cells in either mode [10.5] | `classif_auto_res`, `.CLASSIF_MAX_CANDIDATE_CELLS` |
| Dynamic buffer | 2.0 × 0.5 × mean nearest-neighbour distance, clamped to [5, 2000] m [10.5] | `.classif_scope_hulls` |
| Covariate IDW fallback | p = 2, nmax = 12, on a kriging error, a repeated location or a solve that returns no prediction; reported per fold and on the grid [10.4] | `krige_covariates` (`spatial_kriging.R`), `.CLASSIF_COV_IDW` (`classif_helpers.R`) |
| Class GeoTIFF download | zip containing the INT1U `.tif` (embedded viridis colour table), `.tif.aux.xml` (GDAL category names) and an `ID,class` legend CSV [10.12] | `classif_module.R` |

---

## 12. References

Works cited in this guide, with a DOI where one exists.

Ababou, R., Bagtzoglou, A. C., & Wood, E. F. (1994). On the condition number of covariance matrices in kriging, estimation, and simulation of random fields. *Mathematical Geology*, 26(1), 99-133. https://doi.org/10.1007/BF02065878

Abdi, H., & Williams, L. J. (2010). Principal component analysis. *WIREs Computational Statistics*, 2(4), 433-459. https://doi.org/10.1002/wics.101

Arthur, D., & Vassilvitskii, S. (2007). k-means++: the advantages of careful seeding. In *Proceedings of the Eighteenth Annual ACM-SIAM Symposium on Discrete Algorithms* (pp. 1027-1035). Society for Industrial and Applied Mathematics. https://dl.acm.org/doi/10.5555/1283383.1283494

Bellon-Maurel, V., Fernandez-Ahumada, E., Palagos, B., Roger, J.-M., & McBratney, A. (2010). Critical review of chemometric indicators commonly used for assessing the quality of the prediction of soil attributes by NIR spectroscopy. *TrAC Trends in Analytical Chemistry*, 29(9), 1073-1081. https://doi.org/10.1016/j.trac.2010.05.006

Benjamini, Y., & Hochberg, Y. (1995). Controlling the false discovery rate: a practical and powerful approach to multiple testing. *Journal of the Royal Statistical Society, Series B*, 57(1), 289-300. https://doi.org/10.1111/j.2517-6161.1995.tb02031.x

Bivand, R. S., & Wong, D. W. S. (2018). Comparing implementations of global and local indicators of spatial association. *TEST*, 27(3), 716-748. https://doi.org/10.1007/s11749-018-0599-x

Breiman, L. (2001). Random forests. *Machine Learning*, 45(1), 5-32. https://doi.org/10.1023/A:1010933404324

Brent, R. P. (1973). *Algorithms for Minimization without Derivatives*. Prentice-Hall, Englewood Cliffs, NJ.

Brier, G. W. (1950). Verification of forecasts expressed in terms of probability. *Monthly Weather Review*, 78(1), 1-3. https://doi.org/10.1175/1520-0493(1950)078%3C0001:VOFEIT%3E2.0.CO;2

Brodersen, K. H., Ong, C. S., Stephan, K. E., & Buhmann, J. M. (2010). The balanced accuracy and its posterior distribution. *Proceedings of the 20th International Conference on Pattern Recognition*, 3121-3124. https://doi.org/10.1109/ICPR.2010.764

Cambardella, C. A., Moorman, T. B., Novak, J. M., Parkin, T. B., Karlen, D. L., Turco, R. F., & Konopka, A. E. (1994). Field-scale variability of soil properties in central Iowa soils. *Soil Science Society of America Journal*, 58(5), 1501-1511. https://doi.org/10.2136/sssaj1994.03615995005800050033x

Cawley, G. C., & Talbot, N. L. C. (2010). On over-fitting in model selection and subsequent selection bias in performance evaluation. *Journal of Machine Learning Research*, 11, 2079-2107.

Chang, C.-W., Laird, D. A., Mausbach, M. J., & Hurburgh, C. R. (2001). Near-infrared reflectance spectroscopy: principal components regression analyses of soil properties. *Soil Science Society of America Journal*, 65(2), 480-490. https://doi.org/10.2136/sssaj2001.652480x

Chen, T., & Guestrin, C. (2016). XGBoost: a scalable tree boosting system. *Proceedings of the 22nd ACM SIGKDD International Conference on Knowledge Discovery and Data Mining*, 785-794. https://doi.org/10.1145/2939672.2939785

Chow, C. K. (1970). On optimum recognition error and reject tradeoff. *IEEE Transactions on Information Theory*, 16(1), 41-46. https://doi.org/10.1109/TIT.1970.1054406

Cohen, J. (1960). A coefficient of agreement for nominal scales. *Educational and Psychological Measurement*, 20(1), 37-46. https://doi.org/10.1177/001316446002000104

Cohen, J. (1968). Weighted kappa: nominal scale agreement with provision for scaled disagreement or partial credit. *Psychological Bulletin*, 70(4), 213-220. https://doi.org/10.1037/h0026256

Çokuysal, B., & Erbaş, E. (2004). *Bitkilerde Besin Maddeleri Noksanlıkları ve Toprak Tahlillerinin Değerlendirilmesi* (Çiftçi Broşürü No. 65). Ege Üniversitesi Tarımsal Uygulama ve Araştırma Merkezi, İzmir.

Congalton, R. G. (1991). A review of assessing the accuracy of classifications of remotely sensed data. *Remote Sensing of Environment*, 37(1), 35-46. https://doi.org/10.1016/0034-4257(91)90048-B

Crameri, F., Shephard, G. E., & Heron, P. J. (2020). The misuse of colour in science communication. *Nature Communications*, 11, 5444. https://doi.org/10.1038/s41467-020-19160-7

Craven, P., & Wahba, G. (1978). Smoothing noisy data with spline functions: estimating the correct degree of smoothing by the method of generalized cross-validation. *Numerische Mathematik*, 31(4), 377-403. https://doi.org/10.1007/BF01404567

Cressie, N. (1985). Fitting variogram models by weighted least squares. *Journal of the International Association for Mathematical Geology*, 17(5), 563-586. https://doi.org/10.1007/BF01032109

Davison, A. C., & Hinkley, D. V. (1997). *Bootstrap Methods and their Application*. Cambridge University Press. https://doi.org/10.1017/CBO9780511802843

Deutsch, C. V. (1996). Correcting for negative weights in ordinary kriging. *Computers & Geosciences*, 22(7), 765-773. https://doi.org/10.1016/0098-3004(96)00005-2

Dietterich, T. G. (1998). Approximate statistical tests for comparing supervised classification learning algorithms. *Neural Computation*, 10(7), 1895-1923. https://doi.org/10.1162/089976698300017197

Duchon, J. (1977). Splines minimizing rotation-invariant semi-norms in Sobolev spaces. In W. Schempp & K. Zeller (Eds.), *Constructive Theory of Functions of Several Variables* (Lecture Notes in Mathematics 571, pp. 85-100). Springer. https://doi.org/10.1007/BFb0086566

Duncan, D. B. (1955). Multiple range and multiple F tests. *Biometrics*, 11(1), 1-42. https://doi.org/10.2307/3001478

Fisher, A., Rudin, C., & Dominici, F. (2019). All models are wrong, but many are useful: learning a variable's importance by studying an entire class of prediction models simultaneously. *Journal of Machine Learning Research*, 20(177), 1-81.

Fisher, W. D. (1958). On grouping for maximum homogeneity. *Journal of the American Statistical Association*, 53(284), 789-798. https://doi.org/10.1080/01621459.1958.10501479

Golub, G., & Pereyra, V. (2003). Separable nonlinear least squares: the variable projection method and its applications. *Inverse Problems*, 19(2), R1-R26. https://doi.org/10.1088/0266-5611/19/2/201

Goovaerts, P. (1997). *Geostatistics for Natural Resources Evaluation*. Oxford University Press, New York. https://doi.org/10.1093/oso/9780195115383.001.0001

Hand, D. J., & Till, R. J. (2001). A simple generalisation of the area under the ROC curve for multiple class classification problems. *Machine Learning*, 45(2), 171-186. https://doi.org/10.1023/A:1010920819831

Harrower, M., & Brewer, C. A. (2003). ColorBrewer.org: an online tool for selecting colour schemes for maps. *The Cartographic Journal*, 40(1), 27-37. https://doi.org/10.1179/000870403235002042

Hastie, T., Tibshirani, R., & Friedman, J. (2009). *The Elements of Statistical Learning: Data Mining, Inference, and Prediction* (2nd ed.). Springer, New York. https://doi.org/10.1007/978-0-387-84858-7

Hengl, T. (2006). Finding the right pixel size. *Computers & Geosciences*, 32(9), 1283-1298. https://doi.org/10.1016/j.cageo.2005.11.008

Hengl, T., Heuvelink, G. B. M., & Rossiter, D. G. (2007). About regression-kriging: from equations to case studies. *Computers & Geosciences*, 33(10), 1301-1315. https://doi.org/10.1016/j.cageo.2007.05.001

Hengl, T., Heuvelink, G. B. M., Kempen, B., Leenaars, J. G. B., Walsh, M. G., Shepherd, K. D., Sila, A., MacMillan, R. A., Mendes de Jesus, J., Tamene, L., & Tondoh, J. E. (2015). Mapping soil properties of Africa at 250 m resolution: random forests significantly improve current predictions. *PLoS ONE*, 10(6), e0125814. https://doi.org/10.1371/journal.pone.0125814

Isaaks, E. H., & Srivastava, R. M. (1989). *An Introduction to Applied Geostatistics*. Oxford University Press, New York. ISBN 978-0-19-505013-4.

James, G., Witten, D., Hastie, T., & Tibshirani, R. (2013). *An Introduction to Statistical Learning*. Springer, New York. https://doi.org/10.1007/978-1-4614-7138-7

Jenks, G. F. (1967). The data model concept in statistical mapping. *International Yearbook of Cartography*, 7, 186-190.

Jenks, G. F. (1977). *Optimal Data Classification for Choropleth Maps*. Occasional Paper No. 2, Department of Geography, University of Kansas, Lawrence.

Journel, A. G., & Huijbregts, C. J. (1978). *Mining Geostatistics*. Academic Press, London. ISBN 978-0-12-391050-1.

Kim, S. (2015). ppcor: an R package for a fast calculation to semi-partial correlation coefficients. *Communications for Statistical Applications and Methods*, 22(6), 665-674. https://doi.org/10.5351/CSAM.2015.22.6.665

King, G., & Zeng, L. (2001). Logistic regression in rare events data. *Political Analysis*, 9(2), 137-163. https://doi.org/10.1093/oxfordjournals.pan.a004868

Kohavi, R. (1995). A study of cross-validation and bootstrap for accuracy estimation and model selection. *Proceedings of the 14th International Joint Conference on Artificial Intelligence (IJCAI'95)*, Vol. 2, 1137-1143.

Kruskal, W. H., & Wallis, W. A. (1952). Use of ranks in one-criterion variance analysis. *Journal of the American Statistical Association*, 47(260), 583-621. https://doi.org/10.1080/01621459.1952.10483441

Lance, G. N., & Williams, W. T. (1967). A general theory of classificatory sorting strategies: 1. Hierarchical systems. *The Computer Journal*, 9(4), 373-380. https://doi.org/10.1093/comjnl/9.4.373

Lawson, C. L., & Hanson, R. J. (1974). *Solving Least Squares Problems*. Prentice-Hall, Englewood Cliffs, NJ.

Lê, S., Josse, J., & Husson, F. (2008). FactoMineR: an R package for multivariate analysis. *Journal of Statistical Software*, 25(1), 1-18. https://doi.org/10.18637/jss.v025.i01

Liaw, A., & Wiener, M. (2002). Classification and regression by randomForest. *R News*, 2(3), 18-22.

Lin, L. I.-K. (1989). A concordance correlation coefficient to evaluate reproducibility. *Biometrics*, 45(1), 255-268. https://doi.org/10.2307/2532051

Lindsay, W. L., & Norvell, W. A. (1978). Development of a DTPA soil test for zinc, iron, manganese, and copper. *Soil Science Society of America Journal*, 42(3), 421-428. https://doi.org/10.2136/sssaj1978.03615995004200030009x

Linnenbrink, J., Milà, C., Ludwig, M., & Meyer, H. (2024). kNNDM CV: k-fold nearest-neighbour distance matching cross-validation for map accuracy estimation. *Geoscientific Model Development*, 17(15), 5897-5912. https://doi.org/10.5194/gmd-17-5897-2024

Lloyd, S. P. (1982). Least squares quantization in PCM. *IEEE Transactions on Information Theory*, 28(2), 129-137. https://doi.org/10.1109/TIT.1982.1056489

Mahalanobis, P. C. (1936). On the generalised distance in statistics. *Proceedings of the National Institute of Sciences of India*, 2(1), 49-55. (Reprinted in *Sankhya A*, 80(S1), 1-7. https://doi.org/10.1007/s13171-019-00164-5)

Mahoney, M. J., Johnson, L. K., Silge, J., Frick, H., Kuhn, M., & Beier, C. M. (2023). Assessing the performance of spatial cross-validation approaches for models of spatially structured data. *arXiv*:2303.07334. https://doi.org/10.48550/arXiv.2303.07334

Makridakis, S. (1993). Accuracy measures: theoretical and practical concerns. *International Journal of Forecasting*, 9(4), 527-529. https://doi.org/10.1016/0169-2070(93)90079-3

Matheron, G. (1963). Principles of geostatistics. *Economic Geology*, 58(8), 1246-1266. https://doi.org/10.2113/gsecongeo.58.8.1246

Matthews, B. W. (1975). Comparison of the predicted and observed secondary structure of T4 phage lysozyme. *Biochimica et Biophysica Acta (BBA) - Protein Structure*, 405(2), 442-451. https://doi.org/10.1016/0005-2795(75)90109-9

McBratney, A. B., & Pringle, M. J. (1999). Estimating average and proportional variograms of soil properties and their potential use in precision agriculture. *Precision Agriculture*, 1(2), 125-152. https://doi.org/10.1023/A:1009995404447

McNemar, Q. (1947). Note on the sampling error of the difference between correlated proportions or percentages. *Psychometrika*, 12(2), 153-157. https://doi.org/10.1007/BF02295996

Meyer, H., Reudenbach, C., Wöllauer, S., & Nauss, T. (2019). Importance of spatial predictor variable selection in machine learning applications: moving from data reproduction to spatial prediction. *Ecological Modelling*, 411, 108815. https://doi.org/10.1016/j.ecolmodel.2019.108815

Milà, C., Mateu, J., Pebesma, E., & Meyer, H. (2022). Nearest neighbour distance matching Leave-One-Out Cross-Validation for map validation. *Methods in Ecology and Evolution*, 13(6), 1304-1316. https://doi.org/10.1111/2041-210X.13851

Minasny, B., & McBratney, A. B. (2005). The Matérn function as a general model for soil variograms. *Geoderma*, 128(3-4), 192-207. https://doi.org/10.1016/j.geoderma.2005.04.003

Molinaro, A. M., Simon, R., & Pfeiffer, R. M. (2005). Prediction error estimation: a comparison of resampling methods. *Bioinformatics*, 21(15), 3301-3307. https://doi.org/10.1093/bioinformatics/bti499

Moran, P. A. P. (1950). Notes on continuous stochastic phenomena. *Biometrika*, 37(1/2), 17-23. https://doi.org/10.2307/2332142

Murtagh, F., & Legendre, P. (2014). Ward's hierarchical agglomerative clustering method: which algorithms implement Ward's criterion? *Journal of Classification*, 31(3), 274-295. https://doi.org/10.1007/s00357-014-9161-z

Nash, J. E., & Sutcliffe, J. V. (1970). River flow forecasting through conceptual models part I: a discussion of principles. *Journal of Hydrology*, 10(3), 282-290. https://doi.org/10.1016/0022-1694(70)90255-6

Nychka, D., Furrer, R., Paige, J., & Sain, S. (2021). *fields: Tools for Spatial Data*. R package, University Corporation for Atmospheric Research, Boulder, CO. https://doi.org/10.5065/D6W957CT

O'Brien, R. M. (2007). A caution regarding rules of thumb for variance inflation factors. *Quality & Quantity*, 41(5), 673-690. https://doi.org/10.1007/s11135-006-9018-6

Papritz, A., Künsch, H. R., & Webster, R. (1993). On the pseudo cross-variogram. *Mathematical Geology*, 25(8), 1015-1026. https://doi.org/10.1007/BF00911547

Pebesma, E. J. (2004). Multivariable geostatistics in S: the gstat package. *Computers & Geosciences*, 30(7), 683-691. https://doi.org/10.1016/j.cageo.2004.03.012

Phipson, B., & Smyth, G. K. (2010). Permutation P-values should never be zero: calculating exact P-values when permutations are randomly drawn. *Statistical Applications in Genetics and Molecular Biology*, 9(1), Article 39. https://doi.org/10.2202/1544-6115.1585

Piñeiro, G., Perelman, S., Guerschman, J. P., & Paruelo, J. M. (2008). How to evaluate models: observed vs. predicted or predicted vs. observed? *Ecological Modelling*, 216(3-4), 316-322. https://doi.org/10.1016/j.ecolmodel.2008.05.006

Ploton, P., Mortier, F., Réjou-Méchain, M., Barbier, N., Picard, N., Rossi, V., Dormann, C., Cornu, G., Viennois, G., Bayol, N., Lyapustin, A., Gourlet-Fleury, S., & Pélissier, R. (2020). Spatial validation reveals poor predictive performance of large-scale ecological mapping models. *Nature Communications*, 11, 4540. https://doi.org/10.1038/s41467-020-18321-y

Posa, D. (1989). Conditioning of the stationary kriging matrices for some well-known covariance models. *Mathematical Geology*, 21(7), 755-765. https://doi.org/10.1007/BF00893320

Roberts, D. R., Bahn, V., Ciuti, S., Boyce, M. S., Elith, J., Guillera-Arroita, G., Hauenstein, S., Lahoz-Monfort, J. J., Schröder, B., Thuiller, W., Warton, D. I., Wintle, B. A., Hartig, F., & Dormann, C. F. (2017). Cross-validation strategies for data with temporal, spatial, hierarchical, or phylogenetic structure. *Ecography*, 40(8), 913-929. https://doi.org/10.1111/ecog.02881

Rousseeuw, P. J., & Van Driessen, K. (1999). A fast algorithm for the minimum covariance determinant estimator. *Technometrics*, 41(3), 212-223. https://doi.org/10.1080/00401706.1999.10485670

Schratz, P., Muenchow, J., Iturritxa, E., Richter, J., & Brenning, A. (2019). Hyperparameter tuning and performance assessment of statistical and machine-learning algorithms using spatial data. *Ecological Modelling*, 406, 109-120. https://doi.org/10.1016/j.ecolmodel.2019.06.002

Shannon, C. E. (1948). A mathematical theory of communication. *Bell System Technical Journal*, 27(3), 379-423. https://doi.org/10.1002/j.1538-7305.1948.tb01338.x

Shepard, D. (1968). A two-dimensional interpolation function for irregularly-spaced data. *Proceedings of the 1968 23rd ACM National Conference*, 517-524. https://doi.org/10.1145/800186.810616

Stein, M. L. (1999). *Interpolation of Spatial Data: Some Theory for Kriging*. Springer, New York. https://doi.org/10.1007/978-1-4612-1494-6

Strobl, C., Boulesteix, A.-L., Kneib, T., Augustin, T., & Zeileis, A. (2008). Conditional variable importance for random forests. *BMC Bioinformatics*, 9, 307. https://doi.org/10.1186/1471-2105-9-307

Strobl, C., & Zeileis, A. (2008). Danger: high power! Exploring the statistical properties of a test for random forest variable importance. In P. Brito (Ed.), *COMPSTAT 2008: Proceedings in Computational Statistics* (Vol. II, pp. 59-66). Physica-Verlag, Heidelberg.

Tukey, J. W. (1949). Comparing individual means in the analysis of variance. *Biometrics*, 5(2), 99-114. https://doi.org/10.2307/3001913

Varma, S., & Simon, R. (2006). Bias in error estimation when using cross-validation for model selection. *BMC Bioinformatics*, 7, 91. https://doi.org/10.1186/1471-2105-7-91

Venables, W. N., & Ripley, B. D. (2002). *Modern Applied Statistics with S* (4th ed.). Springer, New York. https://doi.org/10.1007/978-0-387-21706-2

Wackernagel, H. (2003). *Multivariate Geostatistics: An Introduction with Applications* (3rd ed.). Springer, Berlin. https://doi.org/10.1007/978-3-662-05294-5

Wadoux, A. M. J.-C., Heuvelink, G. B. M., de Bruin, S., & Brus, D. J. (2021). Spatial cross-validation is not the right way to evaluate map accuracy. *Ecological Modelling*, 457, 109692. https://doi.org/10.1016/j.ecolmodel.2021.109692

Wager, S., Hastie, T., & Efron, B. (2014). Confidence intervals for random forests: the jackknife and the infinitesimal jackknife. *Journal of Machine Learning Research*, 15(48), 1625-1651.

Wang, H., & Song, M. (2011). Ckmeans.1d.dp: optimal k-means clustering in one dimension by dynamic programming. *The R Journal*, 3(2), 29-33. https://doi.org/10.32614/RJ-2011-015

Wright, M. N., & Ziegler, A. (2017). ranger: a fast implementation of random forests for high dimensional data in C++ and R. *Journal of Statistical Software*, 77(1), 1-17. https://doi.org/10.18637/jss.v077.i01

Yüksel, O., & Ekinci, H. (2019). Determination of selected profile and productivity characteristics of soils formed on Meriç River (Turkey) terraces. *Turkish Journal of Agriculture - Food Science and Technology*, 7(9), 1397-1403. https://doi.org/10.24925/turjaf.v7i9.1397-1403.2666
