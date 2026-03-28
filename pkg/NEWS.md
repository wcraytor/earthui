# earthUI 0.7.0

## Per-Purpose Settings

* Switching purpose (General / Appraisal / Market) now fully clears all
  state: imported data, model results, tabs, variable configuration, and
  earth parameters. The app resets to a clean default state.
* Settings are now stored per purpose. localStorage keys include a purpose
  suffix (`_general`, `_appraisal`, `_market`), so each purpose maintains
  independent variable configuration, earth parameters, and interaction
  matrices for each imported file.
* SQLite backup keys encode purpose (`filename||purpose`), so settings
  persist across browser sessions per purpose+file combination.
* When a file is imported after a purpose switch, previously saved settings
  for that file+purpose combination are automatically restored.
* Backward-compatible migration detects old-format localStorage keys
  (without purpose suffix) and copies them to the correct purpose-suffixed
  key on first load.

## Fitting Modal Auto-Dismiss

* The fitting modal and backdrop now auto-dismiss 1.5 seconds after tabs
  finish rendering, preventing the overlay from blocking sidebar sections.
* On error, the backdrop is removed immediately while the modal stays
  visible with the close button so the user can read the error.

# earthUI 0.6.0

## Nord Theme Overhaul

* Navbar, sidebar, and all UI elements now use Bootstrap CSS variables
  (`var(--bs-body-bg)`, `var(--bs-body-color)`, `var(--bs-border-color)`)
  instead of hardcoded colors, so light and dark modes adapt correctly.
* Theme toggle button redesigned as a circular icon matching the glmnetUI
  and mgcvUI style.
* Custom `data-eui-theme` attribute replaces `data-bs-theme` to avoid
  conflicts with Bootstrap 5's built-in color mode system.
* Consistent Nord palette usage across earthUI, glmnetUI, and mgcvUI.

## Predictor Settings Table

* Column headers "Include", "Factor", and "Linear" now render vertically
  (rotated text), saving horizontal space.
* Checkbox columns grouped together (Include, Factor, Linear) with the
  Special dropdown moved after them.
* Header row uses `position: sticky` with a solid theme-aware background
  so it stays fixed while scrolling the variable list.
* Shows "Select a target variable above to configure predictors." when no
  target is selected, instead of an empty bordered bar.

## Data Tab Fix

* Appraisal mode data preview now shows all columns on initial load when
  no target or predictors are selected yet. Previously only the "row"
  column was visible.

## Section Info Icons

* Blue "?" info icons added to section headers (Sections 1-4, 6-9)
  with Bootstrap popovers describing each section's purpose.
* Info icons are right-justified within the sidebar using absolute
  positioning. Popovers open to the left to avoid overflow.

## Purpose Mode Improvements

* Switching purpose (General / Appraisal / Market) now clears all model
  results, RCA state, and report assets to prevent stale data.
* Purpose radio selection saved back to localStorage for persistence.

## Button Styling

* Buttons updated to use Frost-palette classes (`btn-primary`,
  `btn-outline-secondary`) instead of Aurora classes (`btn-success`,
  `btn-danger`) per the shared UI conventions.

# earthUI 0.2.0

## 3D Interaction Plot Fix

* Fixed plotly surface transposition bug — `add_surface()` uses `z[i,j]`
  at `(x[j], y[i])`, the transpose of base R `persp()`. Data point dots
  now correctly align with the surface in interaction contribution graphs.

## Report Rendering

* New `prepare_report_assets()` function pre-generates all plots (PNG + PDF)
  and pre-computes all data, so `render_report()` only runs Quarto/pandoc
  format conversion.
* `render_report()` gains `assets_dir` parameter to accept pre-generated
  assets for faster multi-format rendering.
* Report rendering now runs asynchronously via `callr::r_bg()` with a modal
  dialog showing elapsed time and Quarto progress. The app stays responsive.
* HTML reports now use KaTeX instead of MathJax for faster math rendering.
* Word reports now include "Page X of Y" page numbers in the footer.

## Post-Fit UX

* Results tabs appear immediately with "Waiting for processing to complete."
  messages while the model is fitting. The RCA Adjustments tab shows
  "7. Calculate RCA Adjustments & Download must first be initiated and
  completed."
* The Data tab persists across fits — same output IDs used before and after
  fitting, so the data table is never destroyed and recreated.
* Tab content renders on-demand (only the active tab computes).
* `auto_export_for_mgcv_()` (saveRDS) now runs in a background process to
  avoid blocking the UI after fit completion.
* Fit log and mgcv auto-export deferred via `session$onFlushed()` and
  `callr::r_bg()` so tabs appear instantly.

## Event Logging

* New session event log: `<filename>_earthui_log.txt` in the output folder
  records start/end timestamps and elapsed times for Model Fit, Download
  Output, RCA Adjustments, Sales Grid generation, and Report rendering.
  One file per data file, appended to across operations.

## Testing

* 71 new tests (1026 total):
  - `eval_g_function_()` direct unit tests for all component types (17 tests)
  - Plotly surface transposition verification (15 tests)
  - `prepare_report_assets()` and `render_report()` with `assets_dir` (15 tests)
  - Expanded export report error handling tests

# earthUI 0.1.3

## Locale & Regional Settings

* Country-based locale system with 31 country presets controlling CSV
  separator, decimal mark, thousands separator, date format ordering, and
  paper size.
* Country dropdown with individual override dropdowns for Paper, CSV sep,
  Decimal, and Date format.
* Global locale defaults saved via "Save as my default" (SQLite persistence),
  independent of per-file settings.
* Number formatting on plot axes and slope labels adapts to locale (e.g.,
  German uses period thousands, Finnish uses space, Swiss uses apostrophe).
* CSV import uses locale-aware separator and decimal mark.
* PDF reports respect locale paper size (Letter or A4).
* Date parsing tries locale-preferred format order first (MDY for US, DMY
  for Europe, YMD for Sweden/Japan).
* Currency symbols removed from all outputs --- earthUI is now
  currency-agnostic.

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
  commas for large values.
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
