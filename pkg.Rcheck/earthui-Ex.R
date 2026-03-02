pkgname <- "earthui"
source(file.path(R.home("share"), "R", "examples-header.R"))
options(warn = 1)
library('earthui')

base::assign(".oldSearch", base::search(), pos = 'CheckExEnv')
base::assign(".old_wd", base::getwd(), pos = 'CheckExEnv')
cleanEx()
nameEx("build_allowed_function")
### * build_allowed_function

flush(stderr()); flush(stdout())

### Name: build_allowed_function
### Title: Build an allowed function for earth()
### Aliases: build_allowed_function

### ** Examples

mat <- build_allowed_matrix(c("sqft", "bedrooms", "pool"))
mat["sqft", "pool"] <- FALSE
mat["pool", "sqft"] <- FALSE
func <- build_allowed_function(mat)



cleanEx()
nameEx("build_allowed_matrix")
### * build_allowed_matrix

flush(stderr()); flush(stdout())

### Name: build_allowed_matrix
### Title: Build an allowed interaction matrix
### Aliases: build_allowed_matrix

### ** Examples

mat <- build_allowed_matrix(c("sqft", "bedrooms", "pool"))
mat["sqft", "pool"] <- FALSE
mat["pool", "sqft"] <- FALSE
mat



cleanEx()
nameEx("detect_categoricals")
### * detect_categoricals

flush(stderr()); flush(stdout())

### Name: detect_categoricals
### Title: Detect likely categorical variables in a data frame
### Aliases: detect_categoricals

### ** Examples

df <- data.frame(
  price = c(100, 200, 300, 400),
  pool = c("Y", "N", "Y", "N"),
  bedrooms = c(2, 3, 2, 4),
  sqft = c(1200, 1500, 1300, 1800)
)
detect_categoricals(df)



cleanEx()
nameEx("fit_earth")
### * fit_earth

flush(stderr()); flush(stdout())

### Name: fit_earth
### Title: Fit an earth model
### Aliases: fit_earth

### ** Examples

df <- mtcars
result <- fit_earth(df, target = "mpg",
                    predictors = c("cyl", "disp", "hp", "wt"))
format_summary(result)



cleanEx()
nameEx("format_anova")
### * format_anova

flush(stderr()); flush(stdout())

### Name: format_anova
### Title: Format ANOVA decomposition
### Aliases: format_anova

### ** Examples

result <- fit_earth(mtcars, "mpg", c("cyl", "disp", "hp", "wt"))
format_anova(result)



cleanEx()
nameEx("format_model_equation")
### * format_model_equation

flush(stderr()); flush(stdout())

### Name: format_model_equation
### Title: Format earth model as LaTeX equation
### Aliases: format_model_equation

### ** Examples

result <- fit_earth(mtcars, "mpg", c("cyl", "disp", "hp", "wt"))
eq <- format_model_equation(result)
cat(eq$latex)



cleanEx()
nameEx("format_summary")
### * format_summary

flush(stderr()); flush(stdout())

### Name: format_summary
### Title: Format earth model summary
### Aliases: format_summary

### ** Examples

result <- fit_earth(mtcars, "mpg", c("cyl", "disp", "hp", "wt"))
summary_info <- format_summary(result)
summary_info$r_squared



cleanEx()
nameEx("format_variable_importance")
### * format_variable_importance

flush(stderr()); flush(stdout())

### Name: format_variable_importance
### Title: Format variable importance
### Aliases: format_variable_importance

### ** Examples

result <- fit_earth(mtcars, "mpg", c("cyl", "disp", "hp", "wt"))
format_variable_importance(result)



cleanEx()
nameEx("import_data")
### * import_data

flush(stderr()); flush(stdout())

### Name: import_data
### Title: Import data from CSV or Excel files
### Aliases: import_data

### ** Examples

# Create a temporary CSV for demonstration
tmp <- tempfile(fileext = ".csv")
write.csv(mtcars, tmp, row.names = FALSE)
df <- import_data(tmp)
head(df)
unlink(tmp)



cleanEx()
nameEx("launch")
### * launch

flush(stderr()); flush(stdout())

### Name: launch
### Title: Launch the earthui Shiny application
### Aliases: launch

### ** Examples

if (interactive()) {
  launch()
}



cleanEx()
nameEx("list_g_functions")
### * list_g_functions

flush(stderr()); flush(stdout())

### Name: list_g_functions
### Title: List g-function groups from a fitted earth model
### Aliases: list_g_functions

### ** Examples

result <- fit_earth(mtcars, "mpg", c("cyl", "disp", "hp", "wt"))
list_g_functions(result)



cleanEx()
nameEx("plot_actual_vs_predicted")
### * plot_actual_vs_predicted

flush(stderr()); flush(stdout())

### Name: plot_actual_vs_predicted
### Title: Plot actual vs predicted values
### Aliases: plot_actual_vs_predicted

### ** Examples

result <- fit_earth(mtcars, "mpg", c("cyl", "disp", "hp", "wt"))
plot_actual_vs_predicted(result)



cleanEx()
nameEx("plot_contribution")
### * plot_contribution

flush(stderr()); flush(stdout())

### Name: plot_contribution
### Title: Plot variable contribution
### Aliases: plot_contribution

### ** Examples

result <- fit_earth(mtcars, "mpg", c("cyl", "disp", "hp", "wt"))
plot_contribution(result, "wt")



cleanEx()
nameEx("plot_correlation_matrix")
### * plot_correlation_matrix

flush(stderr()); flush(stdout())

### Name: plot_correlation_matrix
### Title: Plot correlation matrix
### Aliases: plot_correlation_matrix

### ** Examples

result <- fit_earth(mtcars, "mpg", c("cyl", "disp", "hp", "wt"))
plot_correlation_matrix(result)



cleanEx()
nameEx("plot_g_contour")
### * plot_g_contour

flush(stderr()); flush(stdout())

### Name: plot_g_contour
### Title: Plot g-function as a static contour (for reports)
### Aliases: plot_g_contour

### ** Examples

result <- fit_earth(mtcars, "mpg", c("cyl", "disp", "hp", "wt"))
plot_g_contour(result, 1)



cleanEx()
nameEx("plot_g_function")
### * plot_g_function

flush(stderr()); flush(stdout())

### Name: plot_g_function
### Title: Plot g-function contribution
### Aliases: plot_g_function

### ** Examples

result <- fit_earth(mtcars, "mpg", c("cyl", "disp", "hp", "wt"))
plot_g_function(result, 1)



cleanEx()
nameEx("plot_partial_dependence")
### * plot_partial_dependence

flush(stderr()); flush(stdout())

### Name: plot_partial_dependence
### Title: Plot partial dependence
### Aliases: plot_partial_dependence

### ** Examples

result <- fit_earth(mtcars, "mpg", c("cyl", "disp", "hp", "wt"))
plot_partial_dependence(result, "wt")



cleanEx()
nameEx("plot_qq")
### * plot_qq

flush(stderr()); flush(stdout())

### Name: plot_qq
### Title: Plot Q-Q plot of residuals
### Aliases: plot_qq

### ** Examples

result <- fit_earth(mtcars, "mpg", c("cyl", "disp", "hp", "wt"))
plot_qq(result)



cleanEx()
nameEx("plot_residuals")
### * plot_residuals

flush(stderr()); flush(stdout())

### Name: plot_residuals
### Title: Plot residual diagnostics
### Aliases: plot_residuals

### ** Examples

result <- fit_earth(mtcars, "mpg", c("cyl", "disp", "hp", "wt"))
plot_residuals(result)



cleanEx()
nameEx("plot_variable_importance")
### * plot_variable_importance

flush(stderr()); flush(stdout())

### Name: plot_variable_importance
### Title: Plot variable importance
### Aliases: plot_variable_importance

### ** Examples

result <- fit_earth(mtcars, "mpg", c("cyl", "disp", "hp", "wt"))
plot_variable_importance(result)



cleanEx()
nameEx("render_report")
### * render_report

flush(stderr()); flush(stdout())

### Name: render_report
### Title: Render an earth model report
### Aliases: render_report

### ** Examples

## Not run: 
##D result <- fit_earth(mtcars, "mpg", c("cyl", "disp", "hp", "wt"))
##D render_report(result, output_format = "html", output_file = "report.html")
## End(Not run)



### * <FOOTER>
###
cleanEx()
options(digits = 7L)
base::cat("Time elapsed: ", proc.time() - base::get("ptime", pos = 'CheckExEnv'),"\n")
grDevices::dev.off()
###
### Local variables: ***
### mode: outline-minor ***
### outline-regexp: "\\(> \\)?### [*]+" ***
### End: ***
quit('no')
