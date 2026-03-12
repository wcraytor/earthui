## R CMD check results

0 errors | 0 warnings | 2 NOTEs

* NOTE: "Days since last update" — updating from 0.1.1 (accepted 2025-02-28)
  with new features and bug fixes.
* NOTE: "Skipping checking math rendering: package 'V8' unavailable" — V8 is
  not required; math rendering uses MathJax in the Shiny app.

## Test environments

* Local: macOS Tahoe (aarch64), R 4.5.2
* GitHub Actions: ubuntu-latest (R release, R devel, R oldrel-1),
  macOS-latest (R release), windows-latest (R release)

## Changes in v0.1.2

* Added Sales Comparison Grid output for appraisal mode with Excel formulas,
  sheet protection, and auto-recommended comp selection
* Added special column types: dom, concessions, actual_age, effective_age,
  lot_size, site_dimensions, area (in addition to existing types)
* Added grouped rows in Sales Grid (Location, Site Size, Age) with combined
  value contributions
* Added Sale Price / Concessions / Net SP row with formula-based Net SP
* Intermediate output (Step 6) now places ranking columns (residual_sf,
  cqa_sf, residual, cqa) first and formats them in Excel
* Fit Earth Model button no longer hidden inside collapsible section
* Improved notification display for long file paths
* Set minimum sidebar width to prevent predictor settings overlap
