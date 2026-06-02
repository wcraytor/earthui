## R CMD check results

0 errors | 0 warnings | 0 notes

(Locally a single NOTE "checking for future file timestamps ... unable to
verify current time" may appear; this is an environment-only artifact of the
check machine being unable to reach the time-verification service and does not
occur on CRAN's incoming checks.)

(On CRAN's incoming checks a "Days since last update" and/or version-increment
note may appear: 0.8.0 is a substantial feature release over the currently
published 0.1.3. If the math-rendering note recurs ("Skipping checking math
rendering: package 'V8' unavailable"), V8 is not required — math is rendered
by MathJax/KaTeX in the Shiny app and reports, not at check time.)

## Test environments

* Local: macOS Tahoe (aarch64), R 4.5.3
* GitHub Actions: ubuntu-latest (R release, R devel, R oldrel-1),
  macOS-latest (R release), windows-latest (R release)

## Dependencies

* Imports are limited to packages used by the exported R API and core
  computations (earth, ggplot2, shiny, DBI, RSQLite, jsonlite, openxlsx,
  plotly, readxl, stats, tools, utils).
* Packages used only by the bundled Shiny application and the report/vignette
  tooling are in Suggests (bslib, callr, DT, knitr, rmarkdown, shinyFiles,
  writexl, quarto, tinytex, showtext, sysfonts).

## Major changes in 0.8.0

* **Project model (regProj):** work is organized into first-class *projects*
  at a fixed, cross-OS location. Geo reference data and per-project settings
  travel with the project tree via two bundled SQLite databases
  (`geo.sqlite`, `projects.sqlite`).
* **Per-project settings:** the model configuration (target, predictors,
  earth() parameters, allowed-interactions matrix, effective date) is saved
  per (project, purpose) via explicit "Save current as default" buttons and
  restored when the project is reopened. Public API `get_project_settings()` /
  `set_project_settings()` for automation.
* **Database durability:** `projects.sqlite` opens in WAL mode where
  supported; the settings-schema migration runs in a single transaction with
  crash recovery (an interrupted upgrade rolls back cleanly).
* **Quarto reporting:** report generation is split into generate / convert /
  render steps, producing HTML, PDF, and Word output.
* **Batch execution:** Shiny workflows extracted into reusable `pkg/R/`
  functions so models can be fit and exported without the GUI.

## Bug fixes in 0.8.0

* A Date column used as a model predictor is now aligned to the trained
  frame at predict time (date -> numeric, factor levels matched), fixing a
  prediction error on the Intermediate Output and RCA exports.
* `fit_earth()` drops a non-zero `newvar.penalty` (with a message) when case
  weights are present, which `earth` does not support.
* Character date columns are parsed with a multi-format set, so any column
  that validates as a date also coerces correctly.

## Reverse dependencies

* None (this package has no reverse dependencies on CRAN).
