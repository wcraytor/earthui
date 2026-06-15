## R CMD check results

0 errors | 0 warnings | 0 notes

(Locally a single NOTE "checking for future file timestamps ... unable to
verify current time" may appear; this is an environment-only artifact of the
check machine being unable to reach the time-verification service and does not
occur on CRAN's incoming checks.)

(On CRAN's incoming checks a "Days since last update" note may appear: 0.9.0
is a feature release over the currently published 0.8.0. If the math-rendering
note recurs ("Skipping checking math rendering: package 'V8' unavailable"), V8
is not required — math is rendered by MathJax/KaTeX in the Shiny app and
reports, not at check time.)

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

## Major changes in 0.9.0

* **Trilogy coordination:** an optional "trilogy mode" lets earthUI act as the
  primary method in a comparative appraisal across the maintainer's sibling
  apps (glmnetUI, mgcvUI). A shared `trilogy.json` file in the project records
  the locked earth fit, shared inputs, and each method's value conclusion
  (`conclusion_<fit_ts>.json`). All coordination is file-based and confined to
  the active project tree; it is inert unless the app is launched in trilogy
  mode.
* **Fit timestamps:** every generated output (xlsx, qmd, docx, pdf, html, rds)
  embeds the model's fit time in its filename, so a run's artifacts group
  together and downstream tools can locate them by timestamp.
* **In-app disclaimers and support:** an always-visible appraisal/liability
  notice, an About dialog with the full disclaimer, and a Help dialog that
  composes a pre-addressed support email. These are UI-only additions.
* **RCA CQA input:** the subject CQA score field no longer ships a default
  value (it shows a prompt instead), carries an explanatory help popover,
  validates input to the 0.00-10.00 range, and disables the action until a
  valid value is entered.

## Bug fixes in 0.9.0

* The subject CQA score is validated and parsed numerically before use,
  preventing an invalid or blank entry from reaching the RCA computation.

## Reverse dependencies

* None (this package has no reverse dependencies on CRAN).
