# Contributing to Monolith

Thanks for your interest in Monolith. It is maintained by one person, so help of
any size is welcome. Bug reports from your own data are especially useful.

## Questions, bugs and ideas

- **Questions:** use [Discussions](../../discussions).
- **Bugs:** open an [issue](../../issues) describing what you did, what you
  expected and what happened. The method and CV strategy you used, your R
  version and operating system, and whether your packages came from current CRAN
  or from `renv::restore()` all help. If you cannot share your data, a small
  made-up table that shows the same problem is ideal.
- **Larger changes** (a new method, a new dependency, moving code around):
  please open an issue first, so we can agree on the approach before you spend
  time on it.

Documentation fixes, and a failing test that demonstrates a bug even without a
fix, can go straight to a pull request.

## Setting up

You need R 4.5.0 or newer. From the project root, start the app with
`shiny::runApp("monolith.R", launch.browser = TRUE)`. If packages are missing,
it lists them and asks before installing anything.

There are two ways to get the packages:

- **Current CRAN releases**, which the startup prompt offers. Quick, no compiler
  needed, and enough for most work.
- **The pinned versions in `renv.lock`**, with `install.packages("renv")` and
  then `renv::restore()`. The recorded test values were measured with these, so
  use them when your change affects any number. Versions that CRAN has since
  replaced are built from source, which needs Rtools on Windows, or a compiler
  and the system libraries listed in README section 1 on macOS and Linux.

The tests also need `testthat` and `withr`. Run the whole suite from the project
root (it takes about half an hour):

```sh
Rscript tests/testthat.R
```

or a single file from an R session in the project root:

```r
testthat::test_dir("tests/testthat", filter = "cv-metrics")   # test-cv-metrics.R
```

With current CRAN packages, one test in `test-golden-fixture.R` fails, naming
whichever of R, `gstat`, `sf`, `terra` and `Ckmeans.1d.dp` differs from the
versions the recorded values were taken with, and the lockfile check skips,
listing every package that differs from `renv.lock`. That is expected.
Setting the environment variable `MONOLITH_ALLOW_LATEST=true` turns off those
two version checks while every recorded value is still tested; it also lets the
app install missing packages from CRAN without asking.

`test-app-smoke.R` starts the app in a headless browser. It needs `shinytest2`,
`chromote` and Chrome or Edge, skips without them, and always skips on GitHub
Actions, so run it locally when you change the interface or how it is wired.

## How the code fits together

A few things about the layout are not obvious from the files:

- **The server is one shared environment.** `monolith.R` sources the nine
  `server_*.R` files in a fixed order with `source(local = TRUE)`, and later
  files use names defined in earlier ones. Keep that order, do not wrap them in
  `moduleServer()`, and search all `server_*.R` files before adding a top-level
  name, since a repeated name silently replaces the earlier one. The
  Exploratory and Classification tabs are ordinary Shiny modules.
- **Background jobs load only the model code.** Interpolation, variogram
  auto-fit and the other long jobs run in separate R processes that load
  `spatial_helpers.R`, which sources the four `spatial_*.R` files and nothing
  else (Classification runs also source `classif_helpers.R`). A function such a
  job calls has to live in one of those files, and a new `spatial_*.R` file has
  to be added to `spatial_helpers.R`. Otherwise the job fails with "object not
  found", while the tests, which run everything in one process, still pass.
  Read `input$` and `rv$` before the job starts, and hand it a top-level
  function with plain data: an inline function would carry the whole server
  environment to every worker.
- **Numbers shown to the user come from plain functions.** Put the calculation
  in a function outside the server files (`spatial_*.R`, `ui_formatting.R`,
  `classif_helpers.R` or `global_utils.R`) where a test can call it, and let the
  render block only format the result. A value shown on screen and written to
  an export should come from the same function. The interpolation
  cross-validation metrics come from `perform_cv()`.
- **Random numbers** are drawn inside `with_seed()` or `with_rng_sandbox()`
  (`spatial_vgm.R`), which restore the caller's random state afterwards.
  Parallel code sets `seed` in `furrr::furrr_options()`, so results do not depend
  on the number of workers.
- The `ui_*.R` files contain no reactive code.

## Style

- Two-space indentation, `<-` for assignment, `snake_case` names.
- Comments explain why the code does something.
- Write `shiny::validate(shiny::need(...))` in full. `jsonlite` is attached after
  `shiny` and its own `validate()` would be called otherwise; a test checks this.
- Interface colours are `var(--mn-*)` tokens from `theme_helpers.R`, never
  literal colours; `test-theme.R` checks the app's files for this.
- `#'` comments are plain documentation. There is no package build (no
  `NAMESPACE` or `man/`).
- A new dependency goes in `required_packages` in `global.R`, in `renv.lock` and
  in README section 3. Please check first whether a package already loaded can
  do the job.

## Tests

Please add a test for any change in behaviour, in the matching
`tests/testthat/test-<area>.R` file.

Where you can, compare the result with something independent of your code: a
hand calculation, the published formula or another package. The IDW tests, for
example, compare against a hand-written Shepard weighted mean, and the VIF tests
against `1/(1 - R²)` from `lm()`. A test that only records what a function
returns today would also preserve any bug it has.

Most numeric tests run on the golden fixture in `tests/testthat/fixtures/`, a
frozen extract of the sample survey. `golden_soil(scope)` and
`golden_sf(scope, ...)` load it at three sizes: `tiny` (40 points, 1 locality),
`core` (144 points, 3 localities) and `full` (1035 points, 7 localities). Use
the smallest that shows what you are testing. The fixture has no missing
values, so if your code handles NAs, build that case yourself. For simple
checks, the builders in `helper.R` such as `make_test_points()` are fine.
`GOLDEN_MANIFEST.md` in the fixtures folder describes the fixture in full.

Please do not edit or regenerate the fixture to make a test pass.

## When a recorded value changes

Most tests compute their expected value from the data. Four values have no
simple reference to compute, so they are stored in
`tests/testthat/fixtures/golden_baselines.rds` instead (`identity`,
`vif_drop_order`, `jenks_target_5`, `ok_surface_digest`), together with the R
and package versions they were taken with. If one of them fails:

1. Read the failure message. If it lists version differences, a package update
   is the more likely cause; try again under `renv::restore()`.
2. If the value moves only with current CRAN packages, open an issue about it
   and leave the baseline as it is.
3. If your change moves it on purpose, work out the new value independently (by
   hand, from the definition or with another implementation) instead of taking
   it from the new output. Then run
   `Rscript tests/testthat/fixtures/make_baselines.R`, which runs the whole suite
   first and records nothing if a test fails.

In the pull request, say which value moved, by how much and why the new one is
correct. If your change should not move any number, say that too.

A change to `sample_data/` means rebuilding the fixture; that is left to the
maintainer.

## Pull requests

- Work on a branch and target `main`.
- The `tests` workflow runs the suite with the pinned packages on Ubuntu and
  Windows, and has to pass on both. The weekly `upstream` workflow runs it
  against current CRAN to catch package updates that move a number; it never
  blocks a pull request.
- Add an entry to `CHANGELOG.md` under `Added`, `Changed`, `Fixed`, `Removed`,
  `Performance` or `Testing`, saying what a user would notice and why it
  changed. When you edit the guides in `docs/`, describe how the app works now.
- Leave the version number alone; the maintainer sets it in `DESCRIPTION` at
  release.
- Commit messages should say what changed.

## Data and licence

Contributions are released under the repository's licence, GPL-3.

Please do not add or change files in `sample_data/`. Its datasets are under
usage restrictions until the accompanying manuscript is published or
31 December 2027, whichever comes first (see `sample_data/DATA_LICENSE`). The
derived fixtures in `tests/testthat/fixtures/` are exempt and ship under GPL-3.
Do not commit data you do not have the right to share, or session output and
exported figures.

## Security

Monolith is a single-user app for your own computer. It has no login, and its
configuration loader can browse your home folder, so do not host it on a shared
or public server without access control in front of it. If you find a security
problem, such as an upload or configuration file that can read or run something
it should not, please email the address in `DESCRIPTION` instead of opening a
public issue.
