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

On Linux, the Roboto Condensed font must be installed as a system font
for PDF rendering:

```bash
sudo apt install -y fonts-roboto fonts-lmodern   # Ubuntu/Debian
fc-cache -fv
```

## Quick Start

```r
library(earthUI)
launch()
```

This opens an interactive Shiny application where you can:

1. **Pick or create a project** — projects are the unit of work; each lives at
   a fixed location in the regProj tree.
2. **Import data** from CSV or Excel files into the project's `in/` folder
3. **Configure variables** — select target, predictors, flag categoricals,
   and designate special column types (contract date, DOM, concessions,
   latitude/longitude, living area, lot size, age, etc.)
4. **Set model parameters** — degree, interaction constraints, advanced tuning
5. **View results** — coefficients, variable importance, partial dependence,
   diagnostics
6. **Export reports** — generate a Quarto source bundle, then convert to
   HTML, PDF, or Word

## Project Organization

earthUI organizes work as **projects** under a per-machine `regProj` root
folder. Set the location once via Settings → "regProj Root Folder" (defaults
to `~/regProj` on Mac/Linux, `C:/regProj` on Windows; can also be overridden
with the `REGPROJ_ROOT` environment variable).

Each project lives at:

```
<regProj root>/<purpose>/<flat-segment>/<os>_in/<file>            # input data
<regProj root>/<purpose>/<flat-segment>/<os>_out_<method>/<file>  # outputs
```

where:

- `<purpose>` is `gen` (general), `appr` (appraisal), or `mktarea` (market area).
- `<flat-segment>` is `<country>_<state>_<county>_<city>_<project_name>`
  (admin level depth varies per country; see `country_schema()`).
- `<os>` is `mac`, `ubuntu`, or `win11` — auto-detected. Each project
  scaffolds all three so a single project folder works whether you sync
  it across operating systems or not.
- `<method>` is `earth`, `glmnet`, `mgcv`, or `combined`.

Geographic codes (countries / states / counties / cities) are seeded into
`<regProj>/geo.sqlite` from comprehensive shipped data — US Census FIPS for
all incorporated places, plus GeoNames-derived data for GB, DE, IT, FR, SE,
and SG. Roughly 70,000 admin entries out of the box; users can add more via
the New Project modal.

Per-project model settings (target, predictors, parameters, interactions)
live in `<regProj>/projects.sqlite` keyed by project + filename. So a project
folder is fully self-contained: you can `tar` it up, sync it via rsync, or
hand it to a colleague — they get the data, outputs, and settings together.

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

### Working with the project model from R

```r
# List all projects under the active regProj root
list_df <- regproj_list_projects(sort_by = "recent")

# Read settings programmatically (e.g., for batch automation / ValEngr)
proj_path <- list_df$project_path[1L]
settings <- get_project_settings(proj_path, file_basename = "data.csv")

# Compose canonical project paths
in_dir  <- regproj_path("appr", "us", c("ca", "081", "burlin"),
                        "lakemerritt_2026", os = "mac", in_or_out = "in")
out_dir <- regproj_path("appr", "us", c("ca", "081", "burlin"),
                        "lakemerritt_2026", os = "mac",
                        in_or_out = "out", method = "earth")
```

### Generate / Convert Quarto reports

```r
# Generate a self-contained Quarto bundle (source + plots + reference.docx)
qmd <- generate_quarto_report(result, dest_dir = out_dir, base = "Appraisal_1")

# Convert any .qmd file (not just earthUI-generated) to HTML / Word / PDF
convert_quarto_file(qmd, formats = c("html", "docx"))
```

## License

AGPL-3
