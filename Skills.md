# Skills.md — earthui

Technical skills and capabilities demonstrated by the earthui package.

## R Package Development

- Full CRAN-compatible package structure with DESCRIPTION, NAMESPACE, roxygen2
  documentation, and testthat tests
- Proper dependency management (Imports vs Suggests) allowing graceful
  degradation when optional packages are absent
- Automated NAMESPACE generation via roxygen2

## Shiny Application

- Multi-tab reactive UI with Bootstrap 5 theming (bslib, Flatly)
- Light/dark mode toggle with CSS custom properties (`var(--bs-body-bg)`)
- Asynchronous background processing via `callr::r_bg()` with real-time
  stdout/stderr streaming to the browser through custom JS message handlers
- `localStorage` persistence of user settings across sessions
- Interactive data table with cell-click popups (DT)
- Inline help popovers (Bootstrap 5 Popover API) for every model parameter
- MathJax rendering of model equations with g-function notation
- Binary-safe file download handlers for HTML, PDF, and Word export
- Allowed Interactions matrix with CSS sticky headers and click-to-toggle on
  variable names

## Statistical Modeling

- Full wrapper around `earth::earth()` exposing all parameters through the GUI
- Automatic detection of categorical variables from data types
- Allowed interaction matrix construction with symmetric toggle by variable
- Cross-validation auto-enabled when needed (degree >= 2 or variance models)
- Variance model support (`varmod.method`) with auto nfold/ncross adjustment

## Visualization

- Variable importance bar charts (ggplot2)
- Partial dependence plots for continuous and categorical predictors
- Per-variable contribution plots with piecewise-linear slopes and knot markers
- Correlation matrix heatmap with coefficient labels
- Residuals vs fitted, Q-Q, and actual vs predicted diagnostic plots
- g-function visualizations:
  - 1D: contribution line plots with slope labels
  - 2D interactive: plotly 3D surface with data scatter overlay
  - 2D static: base R `persp()` 3D perspective and ggplot2 filled contour
- Adaptive axis formatting:
  - Small range (< 1, e.g. lat/long): 3 decimal places
  - Medium range (< 100): 1-2 decimal places
  - Large range (>= 100): integer with comma separators
  - Dollar formatting with sign handling (`-$200,000`)
- Adaptive slope labels: `/0.001` for lat/long-scale axes, `/unit` for
  large-range axes

## Mathematical Formatting

- LaTeX equation generation with g-function notation
  (`{}^{f}g^{j}_{k}` where f = factors, j = degree, k = position)
- Hinge functions: `max(0, x - c)` and `max(0, c - x)`
- Indicator functions for categorical levels: `I{var = level}`
- Array environment with aligned equations
- Full LaTeX special character escaping (`_`, `$`, `%`, `&`, `#`)
- MathJax rendering in Shiny; native LaTeX in PDF reports

## Report Generation

- Parameterized Quarto template supporting HTML, PDF, and Word output
- Sections: dataset description, model specification, results summary, model
  equation, coefficients, variable importance, g-function contributions,
  correlation matrix, diagnostics, ANOVA decomposition, raw earth output
- macOS symlink-safe temp directory handling for Quarto rendering
- Both 3D perspective and contour plots included for degree-2 groups in
  static reports

## Data Import

- CSV and Excel (.xlsx, .xls) file support via readxl
- Automatic column renaming to snake_case
- Categorical detection heuristics (factor, character, low-cardinality integer)
