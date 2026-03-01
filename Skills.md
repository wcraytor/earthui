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
- 3-column `\begin{array}{lrl}` layout for equations:
  - Column 1 (`l`): variable label — left-aligned
  - Column 2 (`r`): g-function notation + `=` — right-aligned so `=` signs
    line up flush against the equation column
  - Column 3 (`l`): equation terms — left-aligned
  - Continuation lines leave columns 1-2 empty, indent in column 3
  - **Key lesson**: `\begin{aligned}` right-aligns everything left of `&`,
    and two-column `{ll}` or `{rl}` arrays don't separate the label from the
    g-notation. The 3-column `{lrl}` approach is the only one that produces
    left-aligned labels with a natural gap before right-aligned g + `=`.
- Dual LaTeX output paths:
  - `latex`: for MathJax (Shiny app), KaTeX, and Pandoc (HTML/Word reports).
    Uses `\begin{array}{lrl}`. No `_`/`$` escaping inside `\text{}` —
    MathJax handles them natively.
  - `latex_pdf`: for native LaTeX (PDF reports). Same `{lrl}` structure with
    `\\[4pt]` row spacing. Post-processed by `latex_escape_for_pdf_()` which
    finds `\text{...}` blocks and escapes `_` → `\_` and `$` → `\$` inside
    them (required by pdflatex/xelatex but breaks MathJax).
- Full LaTeX special character escaping (`%`, `&`, `#` in both paths;
  `_` and `$` only in the PDF path via `latex_escape_for_pdf_()`)
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
