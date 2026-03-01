# earthui 0.1.0

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
