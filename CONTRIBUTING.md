# Contributing to Monolith

Monolith is scientific software: its output is meant to support claims about
real soil surveys, and a recorded test value is a claim that a number has not
moved. That shapes what this document asks for. Most of it is not style
policing; it is about not silently invalidating a result.

Read this before opening a pull request. The three sections that matter most are
[Architecture rules](#architecture-rules), [Testing](#testing) and
[When a number moves](#when-a-number-moves).

---

## Before you start

**Open an issue first** for anything beyond a typo or a broken link. Monolith has
a single maintainer and an accompanying manuscript; a large unannounced pull
request is likely to collide with work already in progress, and rejecting it
after the fact wastes more of your time than a two-line issue would have.
Questions go to [Discussions](../../discussions).

Useful things to say in that issue:

- What you observed, on what data, under which engine and which CV strategy.
- Whether a *reported number* changes, or only the interface around it.
- Your R version, platform, and whether you installed via `renv::restore()` or
  from current CRAN. These three decide whether a number is reproducible at all.

Contributions that are always welcome without prior discussion:

- Reproducible bug reports.
- Documentation fixes, including corrections to the guides in `docs/`.
- A failing test that demonstrates a defect, even with no fix attached. A test
  that pins a wrong answer is more useful than a report that describes it.

Contributions that will probably be declined:

- New interpolation engines or learners without a stated scientific case and a
  numeric test against an independent definition.
- Refactors that move code without changing behaviour, unless they make
  something testable that was not testable before. If you do propose one, show
  the old and the new implementation agreeing on the golden fixture (see
  [Testing](#testing)).
- Dependencies. `renv.lock` already resolves to about 250 packages. A new one has
  to earn its place against doing the same thing with what is already loaded,
  and needs an entry in `global.R`'s `required_packages`, a regenerated
  `renv.lock` and the tables in README section 3.

---

## Development environment

R 4.5.0 or newer is required (`global.R` refuses to start below it); the
recorded values were measured on R 4.5.2. When packages are missing, starting the
app names them and offers two routes, and installs nothing without an explicit
yes. A non-interactive session, including the test runner, stops with the same
instructions instead of prompting.

**Route A: current CRAN releases.** Pre-built binaries, no compiler. Correct for
developing features, fixing the interface, or writing tests that do not touch a
recorded baseline.

**Route B: the pinned versions, for anything numeric.**

```r
install.packages("renv")
renv::restore()
```

This installs the versions the recorded baselines were measured under. Any
version CRAN has since superseded builds from source, so you need a toolchain
(Rtools on Windows, Xcode command line tools on macOS, the `-dev` headers in
README section 1 on Linux), and `leaflet.extras` is fetched from GitHub. Slow, but
it is the only environment in which a baseline failure means *your change broke
something* rather than *gstat moved*.

The suite also needs `testthat` (>= 3.0.0) and `withr`. The end-to-end smoke tests
additionally need `shinytest2`, `chromote` and Chrome or Microsoft Edge; without
them that file skips.

Run the suite from the project root:

```sh
Rscript tests/testthat.R
```

Startup attaches the app's 59 packages before the first test runs; expect tens
of seconds.

What the suite tells you about your environment:

- Under Route B with the smoke-test dependencies installed, a clean run is
  `FAIL 0 | SKIP 0`.
- A **skip** from the lockfile comparison means your session no longer matches
  `renv.lock`; it lists the packages.
- A **failure** in `test-golden-fixture.R` naming package versions means one of
  `gstat`, `sf`, `terra` or `classInt` differs from the versions the
  baselines were recorded under. Under Route A this is expected. Setting
  `MONOLITH_ALLOW_LATEST=true` skips those two environment checks while every
  recorded value is still asserted; note that the same variable also lets
  `global.R` install missing packages from current CRAN without asking.

---

## Architecture rules

Monolith's shape is unusual, and the constraints below are not negotiable,
because the tests and the app both depend on them.

### The server is one environment, sourced in order

`monolith.R` builds the server by sourcing nine `server_*.R` chunks with
`source(local = TRUE)`. They share **one** evaluation environment: `rv`,
`session_state` and every helper closure defined in an earlier chunk are visible
to every later one.

Consequences you must respect:

- **Do not reorder the `source()` calls in `monolith.R`.** `server_setup.R` must
  be first, and later chunks read names the earlier ones define.
- **Do not wrap a chunk in `moduleServer()`.** It would namespace the inputs and
  sever every cross-chunk reference at once.
- **Do not shadow an existing name.** The nine chunks make well over a hundred
  top-level assignments with no name assigned twice, and nothing enforces that
  but discipline. Before adding a top-level assignment in a chunk, grep for the
  name across all of `server_*.R`.
- A function that needs no reactive state does not belong in a server chunk. Put
  it in `global_utils.R`, `ui_formatting.R`, or the relevant `spatial_*` /
  `classif_helpers.R` file, where a test can reach it.

The Exploratory and Classification tabs (`desc_exploratory_module.R`,
`classif_module.R`, with `gov_module.R` nested inside the former) **are** proper
Shiny modules. New self-contained features should follow that pattern.

### Parallel workers load only the spatial core

Interpolation runs inside `future` workers and nested PSOCK clusters. Each worker
loads the model code with one `source("spatial_helpers.R")`, which sources the
four `spatial_*.R` fragments and nothing else.

- A function a worker calls must live in a `spatial_*.R` fragment. Workers never
  load `global_utils.R` or any `ui_*.R` file; a helper placed there fails with
  "object not found" in parallel runs only, which the sequential test suite does
  not catch.
- A new fragment must be registered in `spatial_helpers.R`.
- Never read `rv$` or `input$` inside a future. Build the parameters as plain
  values before dispatch.
- Never pass an inline `function(...)` to `furrr`/`future` from server code: it
  closes over the server environment, which is then serialised to every worker.
  Use a top-level function in a `spatial_*.R` file that takes plain data.

The `ui_*.R` fragments are pure (no reactives, no `input$`, no `session`).
`global.R` sources `ui_helpers.R` before `spatial_helpers.R`, so a top-level
object in a `ui_*.R` file cannot reference one from a `spatial_*.R` file; a
reference inside a function body is fine.

### Anything a user reads as a number must be a pure function

This is the single most important rule in the repository.

If your change computes a value that appears in a table, a card, a plot label or
an export, that computation goes in a pure function in `spatial_metrics.R`,
`spatial_kriging.R`, `ui_formatting.R` or `classif_helpers.R`. The render block
calls it and formats the result. Nothing more.

The reason is recorded in the 1.1.0 changelog: arithmetic behind user-facing
numbers lived inside four reactive blocks where no test could reach it, and the
agreement-metrics export carried its own copy of that arithmetic, free to
disagree with the on-screen table.

Two corollaries:

- **Continuous metrics come from `perform_cv()`**, never from a direct
  `yardstick` call or a re-derived formula at a call site.
- **A card and its export call the same builder.** An exported workbook that
  reports a different figure from the screen it was taken from is a serious
  defect, not a formatting bug.

### Determinism

Anything that draws random numbers runs inside the two-sided seed sandbox:
`with_seed()`, or `with_rng_sandbox()` where a block seeds only conditionally.
Both restore the caller's `.Random.seed`. Parallel work uses
`furrr::furrr_options(seed = ...)` so each element gets its own L'Ecuyer stream
and the result does not depend on the `future` plan.

Do not call a bare `set.seed()` outside those wrappers, and do not introduce a
path whose result depends on worker count or scheduling order.

---

## Testing

Every behavioural change needs a test. The bar is higher than "it has a test",
though.

### Test against a definition, not against your own output

The existing suite is built this way and new tests must be too. IDW is checked
against a hand-computed Shepard weighted mean; the empirical variogram against
Matheron's estimator computed from every pair; VIF against `1/(1 - R²)` from an
actual regression; Moran's I against a hand-built symmetric 8-nearest-neighbour
weight matrix; LOOCV against a hand-written leave-one-out loop; RMSE, MAE and R²
against `yardstick`; CCC against its population-moment definition; Tukey and
Kruskal-Wallis letters against direct `agricolae` calls.

A test that runs your function and asserts it returns what your function
currently returns pins the bug along with the behaviour. If there is no closed
form, no independent implementation and no analytical property to check
(exactness at sample locations, a known limit, invariance under a unit change),
say so in a comment and record the value as a baseline instead (see below).

### Use the golden fixture

`tests/testthat/fixtures/` holds a frozen 1035 × 40 extract of the sample survey.
`helper.R` reaches it through `golden_soil(scope)`, `golden_sf(scope, localities,
crs)`, `golden_meta()` and `golden_varlist()`, with three scopes: `tiny`
(40 points, 1 locality), `core` (144 / 3) and `full` (1035 / 7).
`GOLDEN_MANIFEST.md` in that directory is the full reference.

- Use the smallest scope that exhibits the property you are testing; the suite
  is already slow.
- Write the test so it recomputes its reference from the data it is handed, so
  a fork can substitute its own golden set without editing tests.
- Real data produces conditions a synthetic spatial fixture rarely does: VIF up
  to 766, a class with a single sample, a 13-point locality beside a 355-point
  one, extents from 0.9 × 3.0 km to 22.9 × 32.1 km. For analytic identities, the
  small synthetic builders in `helper.R` (`make_test_points()` and friends) are
  fine; reuse them rather than adding new ones.
- The fixture contains **no missing values** and no RNG was used to derive it.
  A green suite therefore says nothing about NA handling: if your code has an NA
  path, construct that case explicitly.

The fixture is frozen deliberately and is **data, not code**: never regenerate it
to make a test pass.

---

## When a number moves

A recorded baseline is `same code + same data + same packages -> same numbers`.
`golden_baselines.rds` holds four recorded values (`identity`, `vif_drop_order`,
`jenks_target_5`, `ok_surface_digest`). It also records the environment they were
taken in: R version, platform, date, the versions of the four packages that can
move them (`gstat`, `sf`, `terra`, `classInt`), and the GDAL, GEOS and
PROJ that `sf` and `terra` are linked against. The platform and the libraries are
recorded but not compared.

**If a baseline assertion fails, do not re-record it to make the suite green.**
Work through this in order:

1. **Read the failure message.** It names any package whose version differs from
   the recording session. If one does, you are probably looking at upstream
   drift, not your change.
2. **Check the platform and libraries.** Baselines are recorded on Windows; CI
   runs Linux and Windows. `ok_surface_digest` runs on a fixed variogram and a
   fixed boundary because, unpinned, a near-tie in the variogram candidate search
   and grid cells about a metre from the boundary resolved differently on Linux.
   A new platform-dependent move means another near-tie: pin that input, do not
   loosen the tolerance.
3. **Reproduce it under `renv::restore()`.** If the value is stable under the
   pinned versions and moves only under current CRAN, it belongs in an issue
   about upstream, not in your pull request.
4. **Decide whether the new value is more correct**, and write down why. Establish
   it from the definition, an independent implementation or a hand computation,
   never by reading it off the new output.
5. Only then run `Rscript tests/testthat/fixtures/make_baselines.R`. It runs the
   full suite first and refuses to record unless it is clean: a baseline taken
   from a failing tree pins the failure as the expected answer.

Re-recording a baseline in a pull request requires an explicit paragraph in the
description saying which value moved, by how much, and why the new one is right.
A pull request that re-records silently will be closed.

If a change is *not* meant to move any number, say so explicitly and show your
work. The 1.0.9 entry does this by naming the analytical files that are
byte-identical to the previous release.

If `sample_data/` itself changes, the fixture is stale and is rebuilt with
`make_golden.R` followed by `make_baselines.R`, and the md5 table in
`GOLDEN_MANIFEST.md` is updated. That is a maintainer task.

---

## Commit messages

Name what changed; `revision`, `fix` or `update` alone say nothing. A commit
that changes a recorded value says so in its message. Work on a branch; pull
requests target `main`.

---

## CHANGELOG

Every user-visible change gets an entry in `CHANGELOG.md`, under `Added`,
`Changed`, `Fixed`, `Removed`, `Performance` or `Testing`.

The house style is to explain the *symptom and the reason*, not the diff. Compare:

> - Fixed export bug in descriptive statistics.

against what the file actually contains:

> **The descriptive-statistics export summarised a different sample from its
> card.** The card reads the uploaded rows of the run's localities; the export
> read the interpolation point set, which drops co-located samples, so a dataset
> with duplicates or replicate cores got a different n, mean and quartiles in the
> sheet.

Write the second kind. Name what the user saw, name the cause, and where a design
choice was made against an alternative, name the alternative and why it lost.
Six months later that is the only record of the decision. Describe the software
as it now behaves; do not narrate earlier versions in the guides under `docs/`.

`DESCRIPTION` is the single authority for the version; the app reads it at
startup. Do not bump it in a pull request: the maintainer sets it at release,
together with `CITATION.cff`, the README and the changelog heading.

---

## Data and licensing

- Code is GPL-3. By contributing you agree your contribution is released under it.
- **Do not commit new sample data.** `sample_data/` carries usage restrictions
  from five joint rights holders until the accompanying manuscript is published
  or 31 December 2027, whichever comes first; see `sample_data/DATA_LICENSE`.
  The derived fixtures and baselines in `tests/testthat/fixtures/` are exempt
  from those terms and are distributed under the repository's license.
- Do not commit anything derived from a dataset you do not have the right to
  redistribute, including a test fixture.
- Do not commit session output, exported figures or a local package library.

---

## Continuous integration

Two workflows, answering two different questions:

- **`tests.yaml`**: the pinned suite (`renv.lock`) on Ubuntu 24.04 and Windows,
  on every push to `main` and every pull request. It must be green before a pull
  request merges. The Linux runner is pinned to a release because GDAL, GEOS and
  PROJ come from Ubuntu's own packages there. Whether the Windows leg blocks is
  set by the `continue-on-error` line in that file; if it fails, read it, do not
  ignore it.
- **`upstream.yaml`**: the same numeric suite against today's CRAN, weekly and on
  manual dispatch. A failure here is **news, not a regression**: a package moved
  a number the fixture records. It never blocks anything. If you see it red,
  open an issue rather than changing a baseline.

The `shinytest2` smoke tests skip on CI by design. Run them locally before any
change that touches `ui_main.R`, the tab files or the wiring in `monolith.R`: a
duplicated input id or a UI element referencing a removed helper is invisible to
the unit tests and fatal at runtime.

---

## Style

No linter is configured. Functions carry `#'` comments as plain documentation;
there is no roxygen2 build (no `NAMESPACE`, no `man/`), so do not add one in the
same pull request as a behavioural change.

- Comment the **why**, not the what. About one line in five in the repository is
  a comment, and the comments carry the reasoning, including options that were
  considered and rejected. Keep that.
- Keep functions short; most are well under 50 lines. The handful over 200 lines
  (the module servers, `run_regional_interpolation`, the theme stylesheet) are
  not a length to copy.
- Interface colour is always a `var(--mn-*)` theme token, never a literal;
  `test-theme.R` enforces this across every root `.R` file.
- Always write `shiny::validate(shiny::need(...))`. `jsonlite` is attached after
  `shiny` and its `validate()` masks Shiny's; a test scans for the unqualified
  form.
- `tryCatch` around anything touching the filesystem, a projection or a user
  upload; `req()` in reactive contexts rather than silent `NULL` propagation.
- No new global state. No `<<-` unless you can explain why a closure will not do.
- Two-space indentation, `<-` for assignment, `snake_case` for names.

---

## Security

Monolith is built as a **single-user, local application**. It is not hardened for
multi-user hosting: the config loader browses the user's home directory through
`shinyFiles`, `future::plan(multisession)` is set globally, and there is no
authentication layer. Do not deploy it on a shared or public server without
putting your own access control in front of it.

If you find something that would be exploitable in that local context (a path
traversal through an upload, arbitrary code execution from a config file), email
the maintainer at the address in `DESCRIPTION` rather than opening a public
issue.

---

## Pull request checklist

- [ ] An issue exists and this is the change it describes.
- [ ] `Rscript tests/testthat.R` passes locally with no failures, and I have
      read every skip it reports.
- [ ] New behaviour has a test, and that test checks a definition rather than my
      own output.
- [ ] No recorded baseline changed, or, if one did, the description says which,
      by how much, and why the new value is right.
- [ ] Any user-facing number is computed by a pure function, and its card and its
      export call the same builder.
- [ ] Anything a parallel worker calls lives in a `spatial_*.R` fragment.
- [ ] `CHANGELOG.md` has an entry written in the house style.
- [ ] Commit messages say something.
- [ ] No new dependency, or a paragraph justifying one.
- [ ] No data added under `sample_data/`.
