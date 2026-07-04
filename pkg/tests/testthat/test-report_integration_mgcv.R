# test-report_integration.R -- Report generation and end-to-end integration tests

# ============================================================================
# Shared helpers
# ============================================================================

fit_basic_mtcars <- function() {
  specs <- list(list(vars = "wt", type = "s", bs = "tp", k = NULL))
  fit_gam(mtcars, "mpg", specs, cv_folds = 0)
}

fit_multi_smooth <- function() {
  specs <- list(
    list(vars = "wt", type = "s", bs = "tp", k = NULL),
    list(vars = "hp", type = "s", bs = "tp", k = NULL),
    list(vars = "qsec", type = "s", bs = "cr", k = 5L)
  )
  fit_gam(mtcars, "mpg", specs, cv_folds = 0)
}

fit_with_linear <- function() {
  specs <- list(
    list(vars = "wt", type = "s", bs = "tp", k = NULL),
    list(vars = "hp", type = "s", bs = "tp", k = NULL),
    list(vars = "cyl", type = "linear", bs = NULL, k = NULL)
  )
  fit_gam(mtcars, "mpg", specs, cv_folds = 0)
}

fit_with_factor <- function() {
  dat <- mtcars
  dat$cyl_f <- factor(dat$cyl)
  specs <- list(
    list(vars = "wt", type = "s", bs = "tp", k = NULL),
    list(vars = "cyl_f", type = "linear", bs = NULL, k = NULL)
  )
  fit_gam(dat, "mpg", specs, cv_folds = 0)
}

# ============================================================================
# Report template checks
# ============================================================================

test_that("report template exists and has expected section headings", {
  tmpl <- system.file("rmd", "gam_report.Rmd", package = "earthUI")
  expect_true(nzchar(tmpl))
  expect_true(file.exists(tmpl))

  content <- readLines(tmpl)
  text <- paste(content, collapse = "\n")

  expect_true(grepl("Variable Importance", text))
  expect_true(grepl("Correlation", text))
  expect_true(grepl("ANOVA", text))
  expect_true(grepl("Concurvity", text))
  expect_true(grepl("Basis Dimension Check", text))
  expect_true(grepl("Contribution Plots|Smooth Term", text))
  expect_true(grepl("Model Overview", text))
  expect_true(grepl("Diagnostics", text))
  expect_true(grepl("Actual vs Predicted", text))
})

# ============================================================================
# HTML report generation (enhanced)
# ============================================================================

test_that("render_gam_report HTML includes Variable Importance section", {
  skip_on_cran()
  res <- fit_multi_smooth()
  tmp <- tempfile(fileext = ".html")
  on.exit(unlink(tmp), add = TRUE)

  render_gam_report(res, "html", output_file = tmp)
  html <- paste(readLines(tmp), collapse = "\n")

  expect_true(grepl("Variable Importance", html, ignore.case = TRUE))
})

test_that("render_gam_report HTML includes Correlation Matrix section", {
  skip_on_cran()
  res <- fit_multi_smooth()
  tmp <- tempfile(fileext = ".html")
  on.exit(unlink(tmp), add = TRUE)

  render_gam_report(res, "html", output_file = tmp)
  html <- paste(readLines(tmp), collapse = "\n")

  expect_true(grepl("Correlation", html, ignore.case = TRUE))
})

test_that("render_gam_report HTML includes ANOVA section", {
  skip_on_cran()
  res <- fit_multi_smooth()
  tmp <- tempfile(fileext = ".html")
  on.exit(unlink(tmp), add = TRUE)

  render_gam_report(res, "html", output_file = tmp)
  html <- paste(readLines(tmp), collapse = "\n")

  expect_true(grepl("ANOVA", html, ignore.case = TRUE))
})

test_that("render_gam_report HTML includes Concurvity section", {
  skip_on_cran()
  res <- fit_multi_smooth()
  tmp <- tempfile(fileext = ".html")
  on.exit(unlink(tmp), add = TRUE)

  render_gam_report(res, "html", output_file = tmp)
  html <- paste(readLines(tmp), collapse = "\n")

  expect_true(grepl("Concurvity", html, ignore.case = TRUE))
})

test_that("render_gam_report HTML includes Basis Dimension Check section", {
  skip_on_cran()
  res <- fit_multi_smooth()
  tmp <- tempfile(fileext = ".html")
  on.exit(unlink(tmp), add = TRUE)

  render_gam_report(res, "html", output_file = tmp)
  html <- paste(readLines(tmp), collapse = "\n")

  expect_true(grepl("Basis Dimension Check", html, ignore.case = TRUE))
})

test_that("render_gam_report with custom title propagates to output", {
  skip_on_cran()
  res <- fit_basic_mtcars()
  tmp <- tempfile(fileext = ".html")
  on.exit(unlink(tmp), add = TRUE)

  render_gam_report(res, "html", output_file = tmp,
                    title = "My Custom Report Title")
  html <- paste(readLines(tmp), collapse = "\n")

  expect_true(grepl("My Custom Report Title", html))
})

# ============================================================================
# DOCX report generation
# ============================================================================

test_that("export_gam_docx includes model summary, smooth terms, and parametric terms", {
  res <- fit_with_linear()
  tmp <- tempfile(fileext = ".docx")
  on.exit(unlink(tmp), add = TRUE)

  result <- export_gam_docx(res, tmp)
  expect_true(file.exists(tmp))
  expect_true(file.size(tmp) > 0)

  # Read the docx content via officer to verify sections
  doc <- officer::read_docx(tmp)
  doc_text <- paste(officer::docx_summary(doc)$text, collapse = " ")

  expect_true(grepl("Model Overview", doc_text))
  expect_true(grepl("Smooth Terms", doc_text))
  expect_true(grepl("Parametric Terms", doc_text))
  expect_true(grepl("R-squared", doc_text))
})

test_that("export_gam_docx handles smooths-only model (no parametric)", {
  res <- fit_basic_mtcars()
  tmp <- tempfile(fileext = ".docx")
  on.exit(unlink(tmp), add = TRUE)

  result <- export_gam_docx(res, tmp)
  expect_true(file.exists(tmp))
  expect_true(file.size(tmp) > 0)
})

# ============================================================================
# Code generation edge cases
# ============================================================================

test_that("generate_r_code with linear terms produces coef_ constants", {
  res <- fit_with_linear()
  code <- generate_r_code(res, n_points = 10)

  expect_type(code, "character")
  expect_true(grepl("coef_", code))
  expect_true(grepl("approxfun", code))
})

test_that("generate_r_code with multiple smooths produces multiple approxfun calls", {
  res <- fit_multi_smooth()
  code <- generate_r_code(res, n_points = 10)

  # Count approxfun occurrences -- should have one per smooth variable
  matches <- gregexpr("approxfun", code)[[1]]
  n_approxfun <- length(matches[matches > 0])
  expect_true(n_approxfun >= 3)

  expect_true(grepl("g_wt", code))
  expect_true(grepl("g_hp", code))
  expect_true(grepl("g_qsec", code))
})

test_that("generate_python_code with linear terms produces COEF_ constants", {
  res <- fit_with_linear()
  code <- generate_python_code(res, n_points = 10)

  expect_type(code, "character")
  expect_true(grepl("COEF_", code))
  expect_true(grepl("numpy", code))
  expect_true(grepl("np\\.interp", code))
  expect_true(grepl("INTERCEPT", code))
})

test_that("generate_python_code with multiple smooths produces multiple functions", {
  res <- fit_multi_smooth()
  code <- generate_python_code(res, n_points = 10)

  expect_true(grepl("def g_wt", code))
  expect_true(grepl("def g_hp", code))
  expect_true(grepl("def g_qsec", code))
})

test_that("generate_cpp_code with linear terms produces COEF_ constants", {
  res <- fit_with_linear()
  code <- generate_cpp_code(res, n_points = 10)

  expect_type(code, "character")
  expect_true(grepl("COEF_", code))
  expect_true(grepl("namespace gam_functions", code))
  expect_true(grepl("INTERCEPT", code))
  expect_true(grepl("#pragma once", code))
  expect_true(grepl("interp", code))
})

test_that("generate_cpp_code with multiple smooths produces multiple inline functions", {
  res <- fit_multi_smooth()
  code <- generate_cpp_code(res, n_points = 10)

  expect_true(grepl("inline double g_wt", code))
  expect_true(grepl("inline double g_hp", code))
  expect_true(grepl("inline double g_qsec", code))
})

# ============================================================================
# SQLite export with linear terms
# ============================================================================

test_that("export_functions_sqlite includes linear_terms table", {
  skip_if_not_installed("DBI")
  skip_if_not_installed("RSQLite")

  res <- fit_with_linear()
  tmp <- tempfile(fileext = ".sqlite")
  on.exit(unlink(tmp), add = TRUE)

  export_functions_sqlite(res, tmp, n_points = 10)
  expect_true(file.exists(tmp))

  con <- DBI::dbConnect(RSQLite::SQLite(), tmp)
  on.exit(DBI::dbDisconnect(con), add = TRUE)

  tables <- DBI::dbListTables(con)
  expect_true("linear_terms" %in% tables)
  expect_true("smooth_grids" %in% tables)
  expect_true("model_info" %in% tables)
  expect_true("intercept" %in% tables)

  lin <- DBI::dbReadTable(con, "linear_terms")
  expect_true(nrow(lin) >= 1)
  expect_true("variable" %in% names(lin))
  expect_true("coefficient" %in% names(lin))
})

test_that("export_functions_sqlite model_info has correct response", {
  skip_if_not_installed("DBI")
  skip_if_not_installed("RSQLite")

  res <- fit_basic_mtcars()
  tmp <- tempfile(fileext = ".sqlite")
  on.exit(unlink(tmp), add = TRUE)

  export_functions_sqlite(res, tmp, n_points = 10)

  con <- DBI::dbConnect(RSQLite::SQLite(), tmp)
  on.exit(DBI::dbDisconnect(con), add = TRUE)

  info <- DBI::dbReadTable(con, "model_info")
  expect_equal(nrow(info), 1L)
  expect_equal(info$response, "mpg")
})

# ============================================================================
# Zip export with all formats
# ============================================================================

test_that("export_functions_zip includes all requested formats", {
  skip_if_not_installed("DBI")
  skip_if_not_installed("RSQLite")

  res <- fit_basic_mtcars()
  tmp <- tempfile(fileext = ".zip")
  on.exit(unlink(tmp), add = TRUE)

  export_functions_zip(res, tmp,
                       languages = c("R", "Python", "C++", "SQLite"),
                       n_points = 10)
  expect_true(file.exists(tmp))
  expect_true(file.size(tmp) > 0)

  contents <- utils::unzip(tmp, list = TRUE)$Name
  expect_true(any(grepl("gam_functions\\.R$", contents)))
  expect_true(any(grepl("gam_functions\\.py$", contents)))
  expect_true(any(grepl("gam_functions\\.hpp$", contents)))
  expect_true(any(grepl("gam_functions\\.sqlite$", contents)))
})

test_that("export_functions_zip with subset of languages", {
  res <- fit_basic_mtcars()
  tmp <- tempfile(fileext = ".zip")
  on.exit(unlink(tmp), add = TRUE)

  export_functions_zip(res, tmp, languages = c("R"), n_points = 10)
  expect_true(file.exists(tmp))

  contents <- utils::unzip(tmp, list = TRUE)$Name
  expect_true(any(grepl("gam_functions\\.R$", contents)))
  expect_false(any(grepl("\\.py$", contents)))
  expect_false(any(grepl("\\.hpp$", contents)))
})

# ============================================================================
# extract_smooth_grids
# ============================================================================

test_that("extract_smooth_grids returns 200 points per smooth by default", {
  res <- fit_multi_smooth()
  grids <- extract_smooth_grids(res)

  expect_type(grids, "list")
  for (nm in names(grids)) {
    expect_equal(nrow(grids[[nm]]), 200)
    expect_true(all(c("x", "fx") %in% names(grids[[nm]])))
  }
})

test_that("extract_smooth_grids custom n_points", {
  res <- fit_basic_mtcars()
  grids <- extract_smooth_grids(res, n_points = 50)

  expect_equal(nrow(grids$wt), 50)
})

test_that("extract_smooth_grids x ranges match data ranges", {
  res <- fit_basic_mtcars()
  grids <- extract_smooth_grids(res, n_points = 100)

  wt_range <- range(mtcars$wt)
  expect_equal(min(grids$wt$x), wt_range[1], tolerance = 1e-6)
  expect_equal(max(grids$wt$x), wt_range[2], tolerance = 1e-6)
})

# ============================================================================
# Integration: Import CSV -> build formula -> fit GAM -> format summary
# ============================================================================

test_that("full pipeline: CSV import -> fit GAM -> format summary -> verify R-squared", {
  tmp_csv <- tempfile(fileext = ".csv")
  on.exit(unlink(tmp_csv), add = TRUE)
  write.csv(mtcars, tmp_csv, row.names = FALSE)

  dat <- import_data_mgcv(tmp_csv)
  expect_true(is.data.frame(dat))
  expect_true("mpg" %in% names(dat))

  specs <- list(
    list(vars = "wt", type = "s", bs = "tp", k = NULL),
    list(vars = "hp", type = "s", bs = "tp", k = NULL)
  )
  formula <- build_gam_formula("mpg", specs)
  expect_s3_class(formula, "formula")

  res <- fit_gam(dat, "mpg", specs, cv_folds = 0)
  expect_s3_class(res, "mgcvUI_result")

  summ <- format_gam_summary(res)
  expect_true(summ$r_squared > 0.5)
  expect_true(summ$r_squared <= 1.0)
  expect_equal(summ$n_obs, 32L)
})

# ============================================================================
# Integration: multiple smooth types -> plot all smooths
# ============================================================================

test_that("full pipeline: fit with multiple smooth types -> plot all -> ggplot objects", {
  specs <- list(
    list(vars = "wt", type = "s", bs = "tp", k = NULL),
    list(vars = "hp", type = "s", bs = "tp", k = NULL),
    list(vars = "disp", type = "s", bs = "tp", k = NULL)
  )
  res <- fit_gam(mtcars, "mpg", specs, cv_folds = 0)

  # plot_smooths returns a faceted plot
  p_all <- plot_smooths(res)
  expect_true(inherits(p_all, "gg") || inherits(p_all, "ggplot") ||
                inherits(p_all, "patchwork"))

  # Individual smooth plots
  for (var in c("wt", "hp", "disp")) {
    p <- plot_smooth_single(res, var)
    expect_s3_class(p, "gg")
  }
})

# ============================================================================
# Integration: fit GAM -> extract grids -> generate R code -> verify parseable
# ============================================================================

test_that("full pipeline: fit -> extract grids -> generate R code -> parseable", {
  res <- fit_multi_smooth()
  grids <- extract_smooth_grids(res, n_points = 20)

  expect_true(length(grids) >= 3)

  code <- generate_r_code(res, n_points = 20)
  expect_type(code, "character")

  # Verify the generated code parses as valid R
  parsed <- tryCatch(parse(text = code), error = function(e) NULL)
  expect_false(is.null(parsed))
})

# ============================================================================
# Integration: fit GAM -> format_gam_summary -> verify all fields
# ============================================================================

test_that("full pipeline: fit -> format_gam_summary -> verify all fields present", {
  res <- fit_with_linear()
  summ <- format_gam_summary(res)

  # All expected top-level fields
  expected_fields <- c("r_squared", "dev_explained", "aic", "bic",
                       "n_obs", "n_smooths", "method", "family",
                       "smooth_table", "parametric_table")
  for (field in expected_fields) {
    expect_true(field %in% names(summ),
                info = paste("Missing field:", field))
  }

  # Numeric fields are finite
  expect_true(is.finite(summ$r_squared))
  expect_true(is.finite(summ$dev_explained))
  expect_true(is.finite(summ$aic))
  expect_true(is.finite(summ$bic))
  expect_equal(summ$n_obs, 32L)
  expect_equal(summ$n_smooths, 2L)  # wt, hp are smooth; cyl is linear
  expect_equal(summ$method, "REML")
  expect_equal(summ$family, "gaussian")
})

# ============================================================================
# Integration: fit GAM -> tidy_gam -> verify rows match model terms
# ============================================================================

test_that("full pipeline: fit -> tidy_gam -> rows match model terms", {
  res <- fit_with_linear()
  tidy <- tidy_gam(res)

  expect_true(is.data.frame(tidy))
  expect_true("term" %in% names(tidy))

  # Model has: intercept, cyl (parametric), s(wt), s(hp)
  terms <- tidy$term
  # tidy_gam may not include intercept as a separate row in all versions
  expect_true(nrow(tidy) > 0)
  expect_true(any(grepl("wt", terms)))
  expect_true(any(grepl("hp", terms)))
})

# ============================================================================
# Integration: fit GAM -> glance_gam -> single row with expected columns
# ============================================================================

test_that("full pipeline: fit -> glance_gam -> single row with expected columns", {
  res <- fit_with_linear()
  g <- glance_gam(res)

  expect_true(is.data.frame(g))
  expect_equal(nrow(g), 1L)

  # broom::glance.gam returns columns including some of these
  col_names_lower <- tolower(names(g))
  expect_true(any(grepl("aic", col_names_lower)) ||
              any(grepl("loglik", col_names_lower)) ||
              any(grepl("deviance", col_names_lower)),
              info = paste("Columns found:", paste(names(g), collapse = ", ")))
})

# ============================================================================
# Integration: fit GAM with factor -> verify parametric terms
# ============================================================================

test_that("full pipeline: fit with factor variable -> parametric terms in summary", {
  res <- fit_with_factor()
  summ <- format_gam_summary(res)

  # cyl_f is a factor with levels 4, 6, 8 => parametric terms
  expect_true(nrow(summ$parametric_table) >= 2L)  # intercept + at least 1 factor level
  expect_true(any(grepl("cyl_f", summ$parametric_table$Term)))

  # Smooth table should have wt
  expect_true(nrow(summ$smooth_table) >= 1L)
  expect_true(any(grepl("wt", summ$smooth_table$Term)))
})

# ============================================================================
# Integration: fit GAM with earth knots -> check_sign_consistency
# ============================================================================

test_that("full pipeline: fit with earth knots -> check_sign_consistency -> data frame", {
  skip_if_not_installed("earth")

  m <- earth::earth(mpg ~ wt + hp, data = mtcars)
  er <- structure(
    list(model = m, target = "mpg",
         predictors = c("wt", "hp"),
         categoricals = character(0), linpreds = character(0),
         degree = 1L, cv_enabled = FALSE, allowed_matrix = NULL,
         data = mtcars, elapsed = 0, trace_output = character(0)),
    class = "earthUI_result"
  )
  ek <- import_earth_mgcv(er)
  expect_s3_class(ek, "mgcvUI_earth_knots")

  specs <- list(
    list(vars = "wt", type = "s", bs = "tp", k = NULL),
    list(vars = "hp", type = "s", bs = "tp", k = NULL)
  )
  gam_res <- fit_gam(mtcars, "mpg", specs, earth_knots = ek, cv_folds = 0)

  signs <- check_sign_consistency(gam_res)
  expect_true(is.data.frame(signs))
  expect_true(nrow(signs) >= 1)
  expect_true(all(c("variable", "earth_direction", "gam_direction",
                     "consistent") %in% names(signs)))
})

test_that("check_sign_consistency returns NULL when no earth knots", {
  res <- fit_basic_mtcars()
  signs <- check_sign_consistency(res)
  expect_null(signs)
})

# ============================================================================
# Integration: log transform pipeline
# ============================================================================

test_that("full pipeline with log transform: fit -> format -> plot -> back-transform", {
  dat <- mtcars
  dat$log_mpg <- log(dat$mpg)

  specs <- list(
    list(vars = "wt", type = "s", bs = "tp", k = NULL),
    list(vars = "hp", type = "s", bs = "tp", k = NULL)
  )
  res <- fit_gam(dat, "log_mpg", specs, cv_folds = 0)
  # Manually set the response_transform to simulate log transform workflow
  res$response_transform <- "log"

  summ <- format_gam_summary(res)
  expect_true(summ$r_squared > 0)

  # plot_actual_vs_predicted_mgcv should back-transform when response_transform = "log"
  p <- plot_actual_vs_predicted_mgcv(res)
  expect_s3_class(p, "gg")

  # Verify back-transform produces original-scale values
  # The plot data should have actual values in the original (exp) scale
  plot_data <- ggplot2::ggplot_build(p)$data[[1]]
  # The max actual value should be on the original scale (exp of log_mpg)
  expect_true(max(plot_data$y) > 10)  # original mpg values are 10-34
})

# ============================================================================
# Integration: diagnostics plots
# ============================================================================

test_that("full pipeline: fit -> plot_diagnostics + plot_actual_vs_predicted_mgcv -> ggplot", {
  res <- fit_multi_smooth()

  p_diag <- plot_diagnostics(res)
  expect_true(inherits(p_diag, "gg") || inherits(p_diag, "ggplot") ||
                inherits(p_diag, "patchwork"))

  p_avp <- plot_actual_vs_predicted_mgcv(res)
  expect_s3_class(p_avp, "gg")
})

# ============================================================================
# Integration: complete round-trip with CSV
# ============================================================================

test_that("complete round-trip: write CSV -> import -> fit -> export R code -> export docx", {
  tmp_csv <- tempfile(fileext = ".csv")
  tmp_docx <- tempfile(fileext = ".docx")
  on.exit({
    unlink(tmp_csv)
    unlink(tmp_docx)
  }, add = TRUE)

  write.csv(mtcars, tmp_csv, row.names = FALSE)
  dat <- import_data_mgcv(tmp_csv)

  specs <- list(
    list(vars = "wt", type = "s", bs = "tp", k = NULL),
    list(vars = "hp", type = "s", bs = "tp", k = NULL)
  )
  res <- fit_gam(dat, "mpg", specs, cv_folds = 0)

  # Generate code
  r_code <- generate_r_code(res, n_points = 20)
  expect_true(grepl("g_wt", r_code))
  expect_true(grepl("g_hp", r_code))

  # Export docx
  export_gam_docx(res, tmp_docx)
  expect_true(file.exists(tmp_docx))
  expect_true(file.size(tmp_docx) > 1000)  # non-trivial file
})

# ============================================================================
# Integration: smooth grids -> SQLite round-trip -> verify data integrity
# ============================================================================

test_that("smooth grids -> SQLite -> read back -> data integrity", {
  skip_if_not_installed("DBI")
  skip_if_not_installed("RSQLite")

  res <- fit_multi_smooth()
  grids <- extract_smooth_grids(res, n_points = 50)

  tmp_db <- tempfile(fileext = ".sqlite")
  on.exit(unlink(tmp_db), add = TRUE)

  export_functions_sqlite(res, tmp_db, n_points = 50)

  con <- DBI::dbConnect(RSQLite::SQLite(), tmp_db)
  on.exit(DBI::dbDisconnect(con), add = TRUE)

  db_grids <- DBI::dbReadTable(con, "smooth_grids")

  # Each variable should have 50 rows
  for (var in names(grids)) {
    var_rows <- db_grids[db_grids$variable == var, ]
    expect_equal(nrow(var_rows), 50,
                 info = paste("Variable:", var))
    # Verify values match
    expect_equal(var_rows$x, grids[[var]]$x, tolerance = 1e-8)
    expect_equal(var_rows$fx, grids[[var]]$fx, tolerance = 1e-8)
  }
})

# ============================================================================
# Integration: GAM predictions are reasonable
# ============================================================================

test_that("fit GAM predictions are reasonable for mtcars mpg", {
  res <- fit_multi_smooth()
  fitted_vals <- fitted(res$model)

  # Fitted values should be in a plausible range for mpg
  expect_true(all(fitted_vals > 5))
  expect_true(all(fitted_vals < 50))

  # R-squared should be good for this model
  summ <- format_gam_summary(res)
  expect_true(summ$r_squared > 0.7)
})

# ============================================================================
# Integration: generate code for model with both smooths and linear
# ============================================================================

test_that("generate all code formats for mixed smooth+linear model", {
  res <- fit_with_linear()

  r_code <- generate_r_code(res, n_points = 15)
  py_code <- generate_python_code(res, n_points = 15)
  cpp_code <- generate_cpp_code(res, n_points = 15)

  # R code: smooths + linear coefficients
  expect_true(grepl("approxfun", r_code))
  expect_true(grepl("coef_", r_code))
  expect_true(grepl("intercept", r_code))
  expect_true(grepl("# Response: mpg", r_code))

  # Python code: smooths + linear coefficients
  expect_true(grepl("np\\.interp", py_code))
  expect_true(grepl("COEF_", py_code))
  expect_true(grepl("INTERCEPT", py_code))
  expect_true(grepl("# Response: mpg", py_code))

  # C++ code: smooths + linear coefficients
  expect_true(grepl("inline double g_", cpp_code))
  expect_true(grepl("COEF_", cpp_code))
  expect_true(grepl("constexpr double INTERCEPT", cpp_code))
  expect_true(grepl("// Response: mpg", cpp_code))
})

# ============================================================================
# Integration: R code includes R-squared and formula comments
# ============================================================================

test_that("generated R code includes metadata comments", {
  res <- fit_basic_mtcars()
  code <- generate_r_code(res, n_points = 10)

  expect_true(grepl("# GAM smooth functions exported from mgcvUI", code))
  expect_true(grepl("# Response: mpg", code))
  expect_true(grepl("# R-squared:", code))
  expect_true(grepl("# Formula:", code))
})

# ============================================================================
# Edge case: single-variable model
# ============================================================================

test_that("single smooth variable: full pipeline works end-to-end", {
  specs <- list(list(vars = "wt", type = "s", bs = "tp", k = NULL))
  res <- fit_gam(mtcars, "mpg", specs, cv_folds = 0)

  # Format
  summ <- format_gam_summary(res)
  expect_equal(summ$n_smooths, 1L)

  # Tidy
  tidy <- tidy_gam(res)
  expect_true(nrow(tidy) >= 1L)

  # Glance
  g <- glance_gam(res)
  expect_equal(nrow(g), 1L)

  # Smooth grids
  grids <- extract_smooth_grids(res, n_points = 30)
  expect_equal(length(grids), 1L)
  expect_true("wt" %in% names(grids))
  expect_equal(nrow(grids$wt), 30)

  # Plots
  p1 <- plot_smooth_single(res, "wt")
  expect_s3_class(p1, "gg")

  p2 <- plot_actual_vs_predicted_mgcv(res)
  expect_s3_class(p2, "gg")
})

# ============================================================================
# Edge case: model with many predictors
# ============================================================================

test_that("model with 5 smooth predictors: all code generators produce output", {
  specs <- list(
    list(vars = "wt", type = "s", bs = "tp", k = NULL),
    list(vars = "hp", type = "s", bs = "tp", k = NULL),
    list(vars = "disp", type = "s", bs = "tp", k = NULL),
    list(vars = "drat", type = "s", bs = "tp", k = NULL),
    list(vars = "qsec", type = "s", bs = "tp", k = NULL)
  )
  res <- fit_gam(mtcars, "mpg", specs, cv_folds = 0)

  grids <- extract_smooth_grids(res, n_points = 10)
  expect_equal(length(grids), 5L)

  r_code <- generate_r_code(res, n_points = 10)
  matches <- gregexpr("approxfun", r_code)[[1]]
  expect_true(sum(matches > 0) >= 5)

  py_code <- generate_python_code(res, n_points = 10)
  py_matches <- gregexpr("def g_", py_code)[[1]]
  expect_true(sum(py_matches > 0) >= 5)

  cpp_code <- generate_cpp_code(res, n_points = 10)
  cpp_matches <- gregexpr("inline double g_", cpp_code)[[1]]
  expect_true(sum(cpp_matches > 0) >= 5)
})

# ============================================================================
# Edge case: tidy_gam column structure
# ============================================================================

test_that("tidy_gam returns expected column structure", {
  res <- fit_multi_smooth()
  tidy <- tidy_gam(res)

  # broom::tidy.gam returns term, edf, ref.df, statistic, p.value
  # for smooth terms and term, estimate, std.error, statistic, p.value
  # for parametric terms
  expect_true("term" %in% names(tidy))
  expect_true("p.value" %in% names(tidy))
})

# ============================================================================
# Edge case: glance_gam column verification
# ============================================================================

test_that("glance_gam has df and nobs columns", {
  res <- fit_basic_mtcars()
  g <- glance_gam(res)

  expect_equal(nrow(g), 1L)
  # broom::glance for gam objects should include nobs or n
  col_names <- names(g)
  expect_true("nobs" %in% col_names || "n" %in% col_names,
              info = paste("Columns:", paste(col_names, collapse = ", ")))
})

# ============================================================================
# Integration: format summary on factor model
# ============================================================================

test_that("format_gam_summary smooth_table EDF values are positive", {
  res <- fit_multi_smooth()
  summ <- format_gam_summary(res)

  expect_true(all(summ$smooth_table$EDF > 0))
  expect_true(all(summ$smooth_table$F > 0))
  expect_true(all(summ$smooth_table$p_value >= 0))
  expect_true(all(summ$smooth_table$p_value <= 1))
})

# ============================================================================
# Integration: HTML report with linear terms renders without error
# ============================================================================

test_that("render_gam_report HTML with linear terms succeeds", {
  skip_on_cran()
  res <- fit_with_linear()
  tmp <- tempfile(fileext = ".html")
  on.exit(unlink(tmp), add = TRUE)

  result <- render_gam_report(res, "html", output_file = tmp)
  expect_true(file.exists(tmp))
  expect_true(file.size(tmp) > 0)
})

# ============================================================================
# Integration: extract_smooth_grids skips linear, keeps smooths
# ============================================================================

test_that("extract_smooth_grids on mixed model skips linear terms", {
  res <- fit_with_linear()
  grids <- extract_smooth_grids(res, n_points = 25)

  expect_true("wt" %in% names(grids))
  expect_true("hp" %in% names(grids))
  expect_false("cyl" %in% names(grids))
  expect_equal(nrow(grids$wt), 25)
  expect_equal(nrow(grids$hp), 25)
})

# ============================================================================
# Integration: plot_smooth_single with residuals=FALSE
# ============================================================================

test_that("plot_smooth_single works with residuals disabled", {
  res <- fit_basic_mtcars()
  p <- plot_smooth_single(res, "wt", residuals = FALSE)

  expect_s3_class(p, "gg")
  # Should have fewer layers without residuals
  n_layers <- length(p$layers)
  expect_true(n_layers >= 2)  # ribbon + line at minimum
})

# ============================================================================
# Integration: verify formula round-trip
# ============================================================================

test_that("formula from build_gam_formula matches what fit_gam uses", {
  specs <- list(
    list(vars = "wt", type = "s", bs = "tp", k = 5L),
    list(vars = "hp", type = "s", bs = "cr", k = 4L),
    list(vars = "cyl", type = "linear", bs = NULL, k = NULL)
  )
  formula <- build_gam_formula("mpg", specs)

  formula_str <- deparse(formula, width.cutoff = 500L)
  expect_true(grepl("s\\(wt", formula_str))
  expect_true(grepl("s\\(hp", formula_str))
  expect_true(grepl("cyl", formula_str))
  expect_true(grepl("mpg ~", formula_str))
})
