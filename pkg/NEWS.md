# earthUI 0.1.2

## Sales Comparison Grid (Appraisal Mode)

* New Sales Comparison Grid output for appraisal workflows — generates
  multi-sheet Excel workbooks with subject and up to 30 comps (3 per sheet).
* Auto-recommended comp selection based on gross adjustment percentage
  (< 25%), sorted by gross adjustment.
* Sale Price / Concessions / Net SP row with formula-based Net SP.
* Grouped rows for Location (longitude, latitude, area), Site Size
  (lot_size, site_dimensions), and Age (actual_age, effective_age) with
  combined value contributions.
* CQA|Residual row with formula-based remaining residual that auto-decreases
  as the appraiser fills in residual feature breakdowns.
* Adjusted Sale Price formula: Net SP + all adjustments above Total VC row.
* Sheet protection with locked formulas and data cells; unlocked residual
  feature input cells for appraiser entry.
* Haversine-based subject proximity calculation for each comp.

## Special Column Types

* Added new special column types: `dom`, `concessions`, `actual_age`,
  `effective_age`, `lot_size`, `site_dimensions`, `area`.
* Special type badges displayed next to variable names in predictor settings.
* Single-assignment enforcement: only one column per special type (except
  `display_only` which allows multiple).

## Intermediate Output Improvements

* Ranking columns (`residual_sf`, `cqa_sf`, `residual`, `cqa`) moved to
  leftmost position in the Step 6 download for easier CQA evaluation.
* Excel number formatting: `residual_sf` as currency (2 dp), `residual` as
  currency (0 dp), `cqa_sf` and `cqa` as numeric (2 dp).

## UI Improvements

* Fit Earth Model button always visible (no longer inside collapsible section).
* Minimum sidebar width (500px) to prevent predictor settings column overlap.
* Wider notification popups (450px) with word-wrapping for long file paths.

## Bug Fixes

* Cross-validated R-squared now uses `model$cv.rsq` directly instead of
  parsing `cv.rsq.tab`.

# earthUI 0.1.0

## Core

* Shiny GUI for interactive earth (MARS) model building.
* Data import from CSV and Excel files with automatic snake_case column naming.
* Variable configuration with include, factor, and linear-only controls.
* Full exposure of all `earth()` parameters through the GUI with inline help
  popovers.
* Cross-validation automatically enabled for interaction and variance models.
* Settings persistence via `localStorage` across browser sessions.
* Light/dark mode toggle.

## Allowed Interactions

* Allowed interaction matrix for constraining degree >= 2 models.
* CSS sticky headers — top row and left column stay visible while scrolling.
* Click variable name to toggle all its interactions (symmetric toggle).
* Allow All / Clear All buttons.

## Multivariate Models

* Multi-response model support: select multiple target variables to fit
  `cbind()` earth models.
* Per-response equation display, summary metrics, and ANOVA decomposition.
* All plot functions accept `response_idx` to select which response to
  visualize.
* Observation weights via the `weights` parameter in `fit_earth()`.

## Model Fitting

* Asynchronous background fitting via `callr::r_bg()` with real-time trace
  display and elapsed timer.
* Automatic `nfold`/`ncross` adjustment for variance models.
* Synchronous fallback when callr is unavailable.

## Visualization

* Variable importance bar charts.
* Per-variable contribution plots with piecewise-linear slope labels and knot
  markers.
* g-function plots: 1D line, interactive 3D surface (plotly), static 3D
  perspective (`persp()`), and filled contour.
* Correlation matrix heatmap.
* Diagnostic plots: residuals vs fitted, Q-Q, actual vs predicted.
* Adaptive axis formatting — 3 decimal places for lat/long-scale data,
  commas for large values, dollar signs for target.
* Slope labels scaled to axis range (`/0.001` for small-range, `/unit` for
  large-range variables).

## Model Equation

* LaTeX equation with g-function notation rendered via MathJax in the app
  and native LaTeX in PDF reports.
* Full LaTeX special character escaping (`_`, `$`, `%`, `&`, `#`).

## Report Export

* Publication-quality HTML, PDF, and Word reports via Quarto.
* Both 3D perspective and contour plots for degree-2 groups in static reports.
* Earth model summary, ANOVA decomposition, and raw earth output sections.
