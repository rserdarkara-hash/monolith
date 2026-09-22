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

1. Coverage stops at the reactive layer. The arithmetic behind the Agreement
   (kappa) table and the descriptive / PCA panels is now reachable
   (`compute_agreement_metrics`, `desc_summary_table`, `desc_group_fit_stats`,
   `desc_pca_fit`), but the render blocks that call them are not: a test cannot
   prove the right locality subset or the right binning mode reaches them.
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
source dataset onto them. Your elevation column becomes `v82`.

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
covariate pair above `|r| = 0.99` with a VIF over 100, at least one class with
fewer than 3 samples, at least 100 points across 2+ localities in `core` and 30
in `tiny`, and a dictionary that labels every measured column.

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

Nothing in the suite is edited. Build a fixture from your data, point the suite
at it, record its baselines:

```r
source("tests/testthat/fixtures/make_golden.R")
make_golden(
  src_data = "my_survey.xlsx",
  src_meta = NULL,                       # or your own dictionary
  out_dir  = "tests/testthat/fixtures_mine",
  roles = list(
    locality = "site", x = "easting", y = "northing", crs = 25832,
    target = "pH_lab", target2 = "carbon", categorical = "usda_class",
    covariates = c("dem", "slope", "twi", "ndvi", "temp",
                   "precip", "tpi", "tri", "ndvi_s2", "temp_warm")
  ))
```

```r
options(monolith_golden_dir = "tests/testthat/fixtures_mine")
```

(or set `MONOLITH_GOLDEN_DIR` in the environment), then

```
Rscript tests/testthat/fixtures/make_baselines.R
```

Most of the suite needs no baselines at all: those tests recompute their
reference from whatever data they are handed, so they are correct for any golden
set. Only four quantities are recorded, because they have no closed form to
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
