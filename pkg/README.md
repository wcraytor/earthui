# earthui

Interactive GUI for Enhanced Adaptive Regression Through Hinges (EARTH) models.

`earthui` provides a Shiny-based graphical interface for the
[earth](https://CRAN.R-project.org/package=earth) package, making it easy to
build, explore, and export multivariate adaptive regression spline models
without writing code.

## Installation

```r
# Install from CRAN (when available)
install.packages("earthui")

# Or install the development version
# devtools::install_github("valengr/earthui")
```

## Quick Start

```r
library(earthui)
launch()
```

This opens an interactive Shiny application where you can:

1. **Import data** from CSV or Excel files
2. **Configure variables** — select target, predictors, and flag categoricals
3. **Set model parameters** — degree, interaction constraints, advanced tuning
4. **View results** — coefficients, variable importance, partial dependence,
   diagnostics
5. **Export reports** — publication-quality HTML, PDF, or Word reports via Quarto

## Programmatic Use

All analytical functions are available independently of the Shiny app:

```r
library(earthui)

# Import and detect categoricals
df <- import_data("my_data.csv")
cats <- detect_categoricals(df)

# Fit a model
result <- fit_earth(df, target = "price", predictors = names(df)[-1])

# Examine results
format_summary(result)
format_variable_importance(result)

# Plot
plot_variable_importance(result)
plot_partial_dependence(result, "sqft")
```

## License

GPL-3
