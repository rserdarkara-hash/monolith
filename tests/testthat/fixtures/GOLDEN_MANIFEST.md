# Golden fixture — what it is, what it guarantees, how to replace it

The golden fixture is a frozen, column-reduced copy of the repository's own
sample data. It exists so numeric tests can assert against a real soil survey
(real spatial structure, real multicollinearity, real class imbalance) instead
of synthetic `rnorm()` fixtures, which cannot produce the failure modes those
properties cause.

---

## What a green suite proves

Running the suite and getting `FAIL 0` establishes that, on this fixture:

- **Interpolation.** IDW is the Shepard weighted mean of `d^-p` and honours its
  neighbourhood limit. Ordinary Kriging is an exact interpolator: it returns the
  observation and zero variance at a sample location, and under a pure nugget it
  returns the global mean with variance `C0(1 + 1/n)`. RK and RFK surfaces are
  the regression trend plus the kriged residual, with variance the trend
  variance plus the kriging variance. TPS converges on the least-squares plane
  as lambda grows, and its roughness falls monotonically with lambda.
  Co-Kriging is exact at the samples and unmoved by a change of covariate units.
- **Variograms.** The empirical variogram is Matheron's estimator, bin for bin.
  The four directional cones partition every pair, so their count-weighted mean
  returns the omnidirectional value. A fit recovers the nugget, sill and range
  of the model a field was simulated from, and the fitted sill tracks the sample
  variance. Each family's practical-range factor is the 95 % correlation-decay
  solution.
- **Cross-validation and error metrics.** RMSE, MAE, ME, R², NSE, NRMSE, RPD,
  RPIQ, sMAPE and Lin's CCC equal their definitions; LOOCV is a genuine
  leave-one-out loop; spatial block folds are spatially compact where random
  folds are not.
- **Diagnostics.** VIF equals `1/(1 - R²)` and the pruning order is the
  regression VIF order at every step; Moran's I is the Cliff-Ord statistic on
  the documented symmetric 8-nearest-neighbour graph with `E[I] = -1/(n-1)`.
- **Classification.** Accuracy, Cohen's kappa, macro precision / recall / F1,
  balanced accuracy, MCC and the per-class producer and user accuracies equal
  the confusion matrix definitions, with the reported estimator the one
  actually computed.
- **Agreement and descriptive statistics.** The agreement table's accuracy,
  balanced accuracy, off-by-one accuracy, MCC and unweighted / linearly
  weighted kappa equal their confusion-matrix definitions; the agro bins land
  where `terra::classify(right = FALSE)` paints them and the quartile bins where
  `quantile` + right-closed `cut` put them. The descriptive summary's per-group
  and TOTAL statistics, the per-group trend R2 and the PCA spectrum match
  independent base-R routes.
- **Explanations, breaks and areas.** Partial dependence is the mean prediction
  with the feature held fixed; class breaks fall on the sample quantiles or on
  equal intervals; reported hectares are ground area, not projected area;
  post-hoc letters agree with `agricolae`.
- **The pipeline as a whole** still assembles the same surface from those parts
  (`ok_surface_digest`). Its variogram and its boundary are pinned by
  `golden_pin_vgm()` and `golden_pin_boundary()` rather than fitted and derived,
  so the lock measures the driver rather than two floating-point near-ties that
  follow the platform: which of sixteen screened variogram candidates wins, and
  whether a cell centre 1.2 m from the hull edge falls inside it. `helper.R`
  derives both pins from the data and records the measurements behind them.

### What it does not prove

This is **verification, not validation**: it establishes that the code computes
what the method says, not that the method is the right one for your science.
That argument lives in `docs/scientific_guide.md`.

Three concrete limits:

1. Reactive coverage is selective. The arithmetic behind the Agreement
   (kappa) table and the descriptive / PCA panels is now reachable
   (`compute_agreement_metrics`, `desc_summary_table`, `desc_group_fit_stats`,
   `desc_pca_fit`), with module tests for naming invariance, partial-correlation tables and PCA
   availability. These do not exercise every locality, binning and export path.
2. The end-to-end lock runs sequentially, so it does not cover the
   `future` / PSOCK dispatch layer.
3. Where a statistical quantity has no exact answer (variogram parameter
   recovery, permutation importance), the test uses a justified band. A small
   systematic change can pass inside one.

---

## Licence

**The same terms as `sample_data/DATA_LICENSE` apply to every file in this
directory.** These are derived datasets in the sense of that licence's clause 3
(no redistribution "in any modified or derived format"). They ship here because
the source they derive from already ships in this repository, so they add no
exposure a clone did not already have, and only for testing Monolith itself.

The fixture is committed rather than gitignored (decided 2026-09-05): a
gitignored fixture would leave a fresh clone unable to run the suite, and
regenerating it on demand would destroy the freezing guarantee the baselines
depend on.

---

## Source and provenance

| Source file | md5 |
|---|---|
| `sample_data/samp_data_1.xlsx` | `eb4b4e853b440c25abbb128ce7d2a297` |
| `sample_data/samp_var_list.xlsx` | `d4b8987cc984b3e0e6949a32f66b38da` |

`golden_meta.rds` carries the same provenance in machine-readable form.

## Files

| File | Contents |
|---|---|
| `golden_soil.rds` | the point table — authoritative |
| `golden_soil.csv` | the same table, for eyeballing and diffing only (not bit-exact for doubles) |
| `golden_meta.rds` | CRS, column roles, scope definitions, source provenance |
| `golden_varlist.rds` | variable dictionary (`vn`, `vid`, `cat`) |
| `golden_baselines.rds` | the values recorded for THIS golden set |
| `make_golden.R` | builds a fixture from a source dataset |
| `make_baselines.R` | records the baselines, gated on a clean suite |

## Columns

- **Keys** — `sample_no`, `locality`, `subset`, `data_from`, `texture`
- **Coordinates** — `x`, `y`, in the CRS named by `golden_meta()$crs`
  (EPSG:32635, UTM 35N, south-west Turkey, for the shipped set)
- **Soil** — `ph`, `ec`, `caco3`, `som`, `sand`, `silt`, `clay`, `tn`, `p`,
  `k`, `ca`, `mg`, `na`, `fe`, `cu`, `zn`, `mn`
- **Prediction columns** — `tn_cve`, `tn_ss`, `p_cve`, `p_ss`, `k_cve`, `k_ss`
  (the `_cve` / `_ss` pairs the app's `value_type` selector expects)
- **Covariates** — `v1`, `v10`, `v12` (bioclim), `v43`, `v61` (Landsat and
  Sentinel-2 NDVI), `v82`, `v83`, `v85`, `v86`, `v87` (terrain)

These names are **canonical**, not survey-specific: the tests read them
literally so they stay readable, and `make_golden.R`'s `roles` argument maps any
source dataset onto them (see [the role table](#the-role-table)).

Row order is fixed by `order(locality, x, y, sample_no)`. No RNG is used
anywhere in the derivation, so the `core` and `tiny` scopes (every k-th row) are
stable and spatially spread by construction.

## Scopes

| Scope | Shipped set |
|---|---|
| `full` | 1035 points, 7 localities |
| `core` | 144 points, 3 localities (Altinova every 6th, Karacasu and Yorga every 5th) |
| `tiny` | 40 points, Kale every 2nd — the most compact locality |

## Properties the suite relies on

`test-golden-fixture.R` distinguishes two kinds, and the difference matters if
you substitute your own data.

**Requirements** — hardcoded, because the rest of the suite would test nothing
without them: projected metric coordinates, no missing values, at least one
covariate pair above `|r| = 0.99` with a VIF over 100, a categorical column with
3+ classes of which one has fewer than 3 samples and one more than 100, at least
100 points across 2+ localities in `core` (each keeping 85 % of its locality's
east-west extent) and 30 in `tiny`, and, when a dictionary ships, a label for
every soil and covariate column.

**Recorded identity** — from `golden_baselines.rds`: row and column counts,
locality and class counts, coordinate ranges, scope sizes, dictionary
dimensions. Facts about this particular survey. For the shipped set: 1035 x 40,
localities Tavas 13 / Kale 79 / Acipayam 83 / Beyagac 87 / Yorga 198 /
Karacasu 220 / Altinova 355, extents from 0.9 x 3.0 km to 22.9 x 32.1 km,
`texture` running from Loam 289 down to Silty clay loam 1, `v1`~`v10` r = 0.996,
`v83`~`v86` r = 0.998, VIF up to 766.

**Recording provenance** — also in `golden_baselines.rds`, as an attribute:
the R version, the platform, the date, the GDAL/GEOS/PROJ that `sf` and `terra`
are linked against, and the versions of the four packages
that can move one of the recorded values (`gstat`, `sf`, `terra`, `Ckmeans.1d.dp`). A recorded value means *same code + same data + same packages -> same
numbers*; the fixture freezes the data and git holds the code, so without this
the package leg was unrecorded and a moved value could not be told apart from
an upstream change. Every baseline assertion now carries it in its failure
message, and `test-golden-fixture.R` fails outright when the session's versions
differ from the recorded ones — either restore them, or re-record after
confirming for yourself that the values did not move. The platform and the
system libraries are recorded but deliberately *not* compared: baselines are
recorded on Windows, whose binaries bundle their own GDAL/GEOS/PROJ, while CI
also runs Linux against Ubuntu's, and comparing them would make CI permanently
red over a difference that is real (the platform moved `ok_surface_digest`
once) but not a defect.

---

## Using your own golden set

Custom fixtures require the full canonical schema and the structural
requirements above. Locality selectors use the fixture's scopes, row counts
and spatial extents, with coordinate tie-breaks. The shipped populations stay
fixed: smallest core locality (Yorga: 40 rows in core, 198 in full), largest
core locality (Altinova: 60 in core), largest full locality (Altinova: 355),
compact full locality with at least 30 rows (Kale: 79), and smallest full
localities with at least 8 or 80 rows (Tavas: 13, Acipayam: 83). Names are
not selection criteria.

Tests draw hull and buffered boundaries on the compact full locality, the
smallest and largest core localities and the tiny scope. In a replacement set
these must be sampled densely and evenly enough for such boundaries, as Kale,
Yorga and Altinova are here; sparse or unevenly sampled localities (Tavas,
Acipayam, Beyagac, Karacasu here) suit only point buffers and are used by no
test that draws a hull.

Some tests require particular method behavior. `make_golden(test_cases = ...)`
can name `list(locality = "my site", target = "canonical_column")` for
`tps_plane` (GCV at the plane end), `tps_exact_better` (least-smoothing end
with Exact winning the independent CV comparison), `tps_smoothed_better`
(the same end with the selected smoothing winning), `tps_interior`
(an interior GCV optimum) and `vgm_zero_nugget` (a variable whose variogram
candidates, without the zero-nugget smooth rule, would select a Gaussian or
Matérn structure at nugget 0 whose kriged surface leaves the observed range;
the test krigs it inside the locality's convex hull). `knndm_random` names a
locality whose buffered hull makes the seeded random partition better than
every spatial candidate; it needs only `locality`. Defaults use the smallest
population with at least 8 rows (plane: ph; exact-better: mn), the smallest
with at least 80 rows (smoothed-better: caco3; interior: ph), and the compact
population with at least 30 rows (kNNDM; zero-nugget variogram: caco3). Each
test independently verifies its case property. A missing property is a test
failure: supply a suitable case or a fixture that covers it. No test is
silently skipped and the recorder's full-suite gate remains mandatory.

### The role table

`roles` names, for each canonical column, the source column that fills it. A
role left out means the source column already has the canonical name. The
three vectors are **positional**: the k-th name you give fills the k-th
canonical column, so an elevation column must sit sixth in `covariates` to
become `v82`. A role name the generator does not define, such as a scalar
`target`, is rejected, and each vector must have exactly the length shown.

| Role | Fills, in this order |
|---|---|
| `sample_no`, `locality`, `subset`, `data_from` | the key columns of the same name |
| `categorical` | `texture` (the class column) |
| `x`, `y`, `crs` | coordinates and their EPSG code (projected, metres) |
| `soil` (17) | `ph`, `ec`, `caco3`, `som`, `sand`, `silt`, `clay`, `tn`, `p`, `k`, `ca`, `mg`, `na`, `fe`, `cu`, `zn`, `mn` |
| `pred` (6) | `tn_cve`, `tn_ss`, `p_cve`, `p_ss`, `k_cve`, `k_ss` |
| `covariates` (10) | `v1` annual mean temperature, `v10` mean temperature of the warmest quarter, `v12` annual precipitation, `v43` Landsat NDVI, `v61` Sentinel-2 NDVI, `v82` elevation, `v83` slope, `v85` TPI, `v86` TRI, `v87` TWI |

A variable of your survey need not be the one its slot is named after: the
tests use the soil columns and covariates as numbers with the properties listed
above, not as pH or elevation. Keeping the meaning where you can keeps the test
names and messages readable.

The example below maps the locality, the coordinates, the class column, two
renamed soil properties and all ten covariates; the key columns and the
prediction columns are assumed to carry their canonical names already.

```r
source("tests/testthat/fixtures/make_golden.R")
make_golden(
  src_data = "my_survey.xlsx",
  src_meta = NULL,                       # or your own dictionary
  out_dir  = "tests/testthat/fixtures_mine",
  roles = list(
    locality = "site", x = "easting", y = "northing", crs = 25832,
    categorical = "usda_class",
    soil = c("pH_lab", "ec", "caco3", "carbon", "sand", "silt", "clay",
             "tn", "p", "k", "ca", "mg", "na", "fe", "cu", "zn", "mn"),
    covariates = c("temp", "temp_warm", "precip", "ndvi", "ndvi_s2",
                   "dem", "slope", "tpi", "tri", "twi")
  ))
```

then point the suite at it and record its baselines, both from the project
root and in the same R session:

```r
Sys.setenv(MONOLITH_GOLDEN_DIR = "tests/testthat/fixtures_mine")
source("tests/testthat/fixtures/make_baselines.R")
```

A separate `Rscript tests/testthat/fixtures/make_baselines.R` sees the fixture
only when `MONOLITH_GOLDEN_DIR` is set in the shell that launches it; an option
set in another R session does not reach it, and the recorder would then record
against the shipped fixture.

Most of the suite needs no baselines at all: those tests recompute their
reference from the fixture, subject to each test's input requirements. Only
four quantities are recorded, because they have no closed form to
check against — the fixture's identity, the VIF pruning order, the Jenks breaks
and the end-to-end surface digest. Until they are recorded those tests skip
rather than fail.

**`make_baselines.R` runs the full suite first and refuses to record anything
unless it is clean.** A baseline taken from a failing tree would pin the failure
as the expected answer, which is the one way a golden-file test can do harm.
Never run it to make a failing test pass: a moved baseline is something to
explain first.

If your data lacks one of the **requirements** above, `test-golden-fixture.R`
names the missing property. That is not a defect in your data; it means the
tests depending on that property would have passed without testing anything.

## Refreshing the shipped fixture

The fixture is **frozen on purpose**. `sample_data/` is hand-edited; if the
tests derived their reference values from it at run time, an edit there would
silently move every golden number.

```
Rscript tests/testthat/fixtures/make_golden.R      # rebuild from sample_data/
Rscript tests/testthat/fixtures/make_baselines.R   # re-record, gated
```

Then update the md5 table above, re-run the suite, and treat any moved value as
something to explain, not to accept.
