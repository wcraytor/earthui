# earthUI

Interactive GUI for Enhanced Adaptive Regression Through Hinges (EARTH) models.

`earthUI` provides a Shiny-based graphical interface for the
[earth](https://CRAN.R-project.org/package=earth) package, making it easy to
build, explore, and export multivariate adaptive regression spline models
without writing code.

## System Requirements

- **R** >= 4.1.0 (RStudio Desktop recommended)
- **All platforms**: HTML and Word reports work out of the box
- **PDF reports**: require a LaTeX installation. If not detected, the
  PDF option is automatically hidden. Install with:
  `tinytex::install_tinytex()`
- **Linux**: may need system libraries before package installation:
  `sudo apt install libcurl4-openssl-dev libssl-dev libxml2-dev
  libsqlite3-dev libfontconfig1-dev`

## Installation

```r
# Install remotes if needed
install.packages("remotes")

# Install earthUI from GitHub
remotes::install_github("wcraytor/earthUI")
```

### Optional: Report Export

To export reports (HTML, PDF, or Word), install the
[Quarto CLI](https://quarto.org/docs/get-started/) and the R package:

```r
install.packages("quarto")
```

For PDF reports, a LaTeX distribution is also required:

```r
install.packages("tinytex")
tinytex::install_tinytex()
```

## Quick Start

```r
library(earthUI)
launch()
```

This opens an interactive Shiny application where you can:

1. **Import data** from CSV or Excel files
2. **Configure variables** — select target, predictors, flag categoricals,
   and designate special column types (contract date, DOM, concessions,
   latitude/longitude, living area, lot size, age, etc.)
3. **Set model parameters** — degree, interaction constraints, advanced tuning
4. **View results** — coefficients, variable importance, partial dependence,
   diagnostics
5. **Export reports** — publication-quality HTML, PDF, or Word reports via Quarto

### Appraisal Mode

For real estate appraisal workflows, earthUI provides:

* **RCA adjustments** — regression-based comparable adjustments with CQA
  scoring and subject value interpolation
* **Sales Comparison Grid** — multi-sheet Excel workbook with formulas for
  Net Sale Price, grouped adjustments (location, site, age), residual feature
  breakdowns, and adjusted sale price. Protected sheets with unlocked input
  cells for appraiser entry.

## Demo Data

earthUI includes a demo appraisal dataset (`Appraisal_1.csv`) with
residential sales data. Access it with:

```r
demo_file <- system.file("extdata", "Appraisal_1.csv", package = "earthUI")
df <- import_data(demo_file)
```

## Programmatic Use

All analytical functions are available independently of the Shiny app:

```r
library(earthUI)

# Load the demo dataset
demo_file <- system.file("extdata", "Appraisal_1.csv", package = "earthUI")
df <- import_data(demo_file)
cats <- detect_categoricals(df)

# Fit a model
result <- fit_earth(df, target = "sale_price",
                    predictors = c("living_sqft", "lot_size", "age"))

# Examine results
format_summary(result)
format_variable_importance(result)

# Plot
plot_variable_importance(result)
plot_contribution(result, 1)
```

## License

AGPL-3
