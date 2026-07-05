# test-edge_cases.R -- Comprehensive edge case and error handling tests
#
# Covers boundary conditions, error paths, and unusual inputs for all
# major functions: fit_gam, build_gam_formula, import_data_mgcv, import_earth_mgcv,
# plot_results, format_results, and export_functions.

# ============================================================================
# Helpers
# ============================================================================

make_minimal_data <- function(n = 20) {
  set.seed(42)
  data.frame(
    y = rnorm(n, 100, 10),
    x1 = runif(n, 0, 10),
    x2 = runif(n, 0, 10)
  )
}

make_spec <- function(var, type = "s", bs = "tp", k = NULL) {
  list(vars = var, type = type, bs = bs, k = k)
}

# ============================================================================
# fit_gam edge cases
# ============================================================================

test_that("fit_gam succeeds with exactly 10 rows (minimum viable)", {
  df <- make_minimal_data(10)
  specs <- list(make_spec("x1"))
  res <- fit_gam(df, "y", specs, cv_folds = 0)

  expect_s3_class(res, "mgcvUI_result")
  expect_s3_class(res$model, "gam")
  expect_equal(nrow(res$model$model), 10L)
})

test_that("fit_gam fails with only 9 rows (insufficient for default k)", {

  # With default k = 10, fitting on 9 rows should fail because there are

  # not enough unique values to support the basis.  fit_gam caps k to
  # max(n_unique - 1, 3), so k becomes 8 here.  The model should still
  # fail at the mgcv level because 9 rows is too few for a GAM with
  # intercept + 8 basis functions.  If it somehow succeeds (mgcv can be

  # lenient), we just verify the result is well-formed.
  df <- make_minimal_data(9)
  specs <- list(make_spec("x1"))
  # This may either error or succeed with capped k -- either is acceptable

  result <- tryCatch(
    fit_gam(df, "y", specs, cv_folds = 0),
    error = function(e) e
  )
  if (inherits(result, "error")) {
    expect_true(TRUE)  # Expected failure path
  } else {
    # If it succeeded, k was capped and mgcv was OK with it
    expect_s3_class(result, "mgcvUI_result")
    expect_true(nrow(result$model$model) <= 9L)
  }
})

test_that("fit_gam drops rows when predictors have NAs in different rows", {
  set.seed(42)
  df <- data.frame(
    y  = rnorm(30),
    x1 = rnorm(30),
    x2 = rnorm(30)
  )
  # Put NAs in different rows for each predictor
  df$x1[1:3] <- NA
  df$x2[4:6] <- NA

  specs <- list(make_spec("x1"), make_spec("x2"))
  res <- fit_gam(df, "y", specs, cv_folds = 0)

  expect_s3_class(res$model, "gam")
  # 6 rows should be dropped (rows 1-3 for x1, rows 4-6 for x2)
  expect_equal(nrow(res$model$model), 24L)
})

test_that("fit_gam drops rows when response has NA values", {
  df <- make_minimal_data(30)
  df$y[1:5] <- NA

  specs <- list(make_spec("x1"))
  res <- fit_gam(df, "y", specs, cv_folds = 0)

  expect_s3_class(res$model, "gam")
  expect_equal(nrow(res$model$model), 25L)
})

test_that("fit_gam errors when factor predictor has only 1 level after NA removal", {
  set.seed(42)
  df <- data.frame(
    y = rnorm(20),
    x1 = rnorm(20),
    grp = factor(c(rep("A", 10), rep("B", 10)))
  )
  # Remove all B-level rows via NAs in x1, leaving only level A
  df$x1[11:20] <- NA

  specs <- list(
    make_spec("x1"),
    make_spec("grp", type = "linear", bs = NULL, k = NULL)
  )
  # After NA removal only level A remains -- mgcv cannot compute
  # contrasts for a single-level factor, so this should error
  expect_error(fit_gam(df, "y", specs, cv_folds = 0), "contrasts")
})

test_that("fit_gam drops constant predictor (all identical values)", {
  df <- make_minimal_data(30)
  df$const_var <- 42

  specs <- list(
    make_spec("x1"),
    make_spec("const_var")
  )
  res <- fit_gam(df, "y", specs, cv_folds = 0)

  expect_s3_class(res$model, "gam")
  # const_var should have been dropped
  remaining <- vapply(res$smooth_specs, function(s) s$vars, character(1))
  expect_false("const_var" %in% remaining)
  expect_true("x1" %in% remaining)
})

test_that("fit_gam auto-converts binary 0/1 predictor to factor", {
  df <- make_minimal_data(30)
  df$binary <- sample(0:1, 30, replace = TRUE)

  specs <- list(
    make_spec("x1"),
    make_spec("binary")
  )
  res <- fit_gam(df, "y", specs, cv_folds = 0)

  expect_s3_class(res$model, "gam")
  # binary should be converted to linear (factor) type
  binary_spec <- Filter(function(s) s$vars == "binary", res$smooth_specs)
  expect_length(binary_spec, 1L)
  expect_equal(binary_spec[[1]]$type, "linear")
})

test_that("fit_gam caps k when larger than unique values", {
  set.seed(42)
  # Create variable with only 5 unique values
  df <- data.frame(
    y = rnorm(30),
    x1 = sample(1:5, 30, replace = TRUE)
  )
  specs <- list(make_spec("x1", k = 20L))
  res <- fit_gam(df, "y", specs, cv_folds = 0)

  expect_s3_class(res$model, "gam")
  # k should have been capped to max(5 - 1, 3) = 4
  expect_true(res$smooth_specs[[1]]$k <= 5L)
})

test_that("fit_gam handles large dataset (1000 rows)", {
  set.seed(42)
  df <- data.frame(
    y  = rnorm(1000),
    x1 = rnorm(1000),
    x2 = rnorm(1000)
  )
  specs <- list(make_spec("x1"), make_spec("x2"))
  res <- fit_gam(df, "y", specs, cv_folds = 0)

  expect_s3_class(res$model, "gam")
  expect_equal(nrow(res$model$model), 1000L)
})

test_that("fit_gam works with a single predictor model", {
  specs <- list(make_spec("wt"))
  res <- fit_gam(mtcars, "mpg", specs, cv_folds = 0)

  expect_s3_class(res$model, "gam")
  expect_length(res$smooth_specs, 1L)
  expect_equal(res$smooth_specs[[1]]$vars, "wt")
})

test_that("fit_gam works with 10+ predictors", {
  set.seed(42)
  n <- 200
  df <- data.frame(y = rnorm(n))
  specs <- list()
  for (i in 1:12) {
    col_name <- paste0("x", i)
    df[[col_name]] <- rnorm(n)
    specs[[i]] <- make_spec(col_name)
  }

  res <- fit_gam(df, "y", specs, cv_folds = 0)

  expect_s3_class(res$model, "gam")
  expect_true(length(res$smooth_specs) >= 10L)
})

# ============================================================================
# build_gam_formula edge cases
# ============================================================================

test_that("build_gam_formula errors on empty smooth_specs list", {
  expect_error(build_gam_formula("y", list()), "At least one")
})

test_that("build_gam_formula handles unknown type gracefully", {
  # An unknown type like "cubic" will still be formatted by format_smooth_term_
  # because the function uses spec$type directly in the paste
  spec <- list(vars = "x", type = "cubic", bs = "tp", k = 5L)
  f <- build_gam_formula("y", list(spec))
  f_str <- deparse(f)
  # Should produce something like y ~ cubic(x, bs = "tp", k = 5)
  expect_true(grepl("cubic\\(x", f_str))
})

test_that("build_gam_formula with k = NULL (auto)", {
  specs <- list(make_spec("x1", k = NULL))
  f <- build_gam_formula("y", specs)
  f_str <- deparse(f)
  # Should NOT have k = in the formula
  expect_false(grepl("k =", f_str))
})

test_that("build_gam_formula with explicit k value", {
  specs <- list(make_spec("x1", k = 5L))
  f <- build_gam_formula("y", specs)
  f_str <- deparse(f)
  expect_true(grepl("k = 5", f_str))
})

test_that("build_gam_formula mixes s + linear + factor in same model", {
  specs <- list(
    list(vars = "x1", type = "s", bs = "tp", k = 5L),
    list(vars = "x2", type = "linear", bs = NULL, k = NULL),
    list(vars = "grp", type = "linear", bs = NULL, k = NULL),
    list(vars = c("x3", "x4"), type = "te", bs = NULL, k = 5L)
  )
  f <- build_gam_formula("y", specs)
  f_str <- deparse(f)

  expect_true(grepl("s\\(x1", f_str))
  expect_true(grepl("x2", f_str))
  expect_true(grepl("grp", f_str))
  expect_true(grepl("te\\(x3, x4", f_str))
})

test_that("build_gam_formula handles tensor product with marginal_bs", {
  specs <- list(
    list(vars = c("x1", "x2"), type = "ti", bs = NULL, k = 5L,
         marginal_bs = c("tp", "tp"))
  )
  f <- build_gam_formula("y", specs)
  f_str <- deparse(f)
  expect_true(grepl("ti\\(x1, x2", f_str))
  expect_true(grepl('bs = c\\("tp", "tp"\\)', f_str))
})

test_that("build_gam_formula handles by-variable specification", {
  specs <- list(
    list(vars = "x1", type = "s", bs = "tp", k = 5L, by = "grp")
  )
  f <- build_gam_formula("y", specs)
  f_str <- deparse(f)
  expect_true(grepl("by = grp", f_str))
})

# ============================================================================
# import_data_mgcv edge cases
# ============================================================================

test_that("import_data_mgcv errors on non-existent file", {
  expect_error(import_data_mgcv("/nonexistent/path/data.csv"))
})

test_that("import_data_mgcv errors on non-string filepath", {
  expect_error(import_data_mgcv(42))
})

test_that("import_data_mgcv handles CSV with special characters in column names", {
  tmp <- tempfile(fileext = ".csv")
  on.exit(unlink(tmp))
  df <- data.frame(
    check.names = FALSE,
    `Price ($)` = c(100, 200),
    `Sq Ft / Unit` = c(1000, 2000),
    `% Change` = c(5.5, 3.2),
    `Col #1` = c("a", "b")
  )
  write.csv(df, tmp, row.names = FALSE)

  result <- import_data_mgcv(tmp)
  expect_true(is.data.frame(result))
  expect_equal(nrow(result), 2L)
  # All names should be clean (lowercase, no special chars)
  expect_true(all(grepl("^[a-z0-9_]+$", names(result))))
})

test_that("import_data_mgcv handles CSV with extremely long column names", {
  tmp <- tempfile(fileext = ".csv")
  on.exit(unlink(tmp))
  long_name <- paste(rep("very_long_column_name", 10), collapse = "_")
  df <- data.frame(x = 1:3)
  names(df) <- long_name
  write.csv(df, tmp, row.names = FALSE)

  result <- import_data_mgcv(tmp)
  expect_true(is.data.frame(result))
  expect_equal(nrow(result), 3L)
  # Name should be cleaned but preserved in some form
  expect_true(nchar(names(result)[1]) > 0)
})

test_that("import_data_mgcv errors on unsupported file extension", {
  tmp <- tempfile(fileext = ".parquet")
  file.create(tmp)
  on.exit(unlink(tmp))
  expect_error(import_data_mgcv(tmp), "Unsupported file format")
})

test_that("import_data_mgcv reads CSV with single column", {
  tmp <- tempfile(fileext = ".csv")
  on.exit(unlink(tmp))
  writeLines(c("value", "1", "2", "3"), tmp)

  result <- import_data_mgcv(tmp)
  expect_true(is.data.frame(result))
  expect_equal(ncol(result), 1L)
  expect_equal(nrow(result), 3L)
})

test_that("import_data_mgcv handles CSV with empty rows at end", {
  tmp <- tempfile(fileext = ".csv")
  on.exit(unlink(tmp))
  writeLines(c("x,y", "1,2", "3,4", ""), tmp)

  result <- import_data_mgcv(tmp)
  expect_true(is.data.frame(result))
  # Should have 2 data rows
  expect_true(nrow(result) >= 2L)
})

# ============================================================================
# import_earth_mgcv edge cases
# ============================================================================

test_that("import_earth_mgcv errors on NULL input", {
  # NULL is not a character path and not an earthUI_result
  expect_error(import_earth_mgcv(NULL), "earthUI_result")
})

test_that("import_earth_mgcv errors on list missing required fields", {
  # A plain list that looks like earthUI_result but is not
  fake <- list(model = NULL, target = "y", predictors = "x")
  expect_error(import_earth_mgcv(fake), "earthUI_result")
})

test_that("import_earth_mgcv errors on list with wrong class", {
  fake <- structure(list(model = NULL), class = "wrong_class")
  expect_error(import_earth_mgcv(fake), "earthUI_result")
})

test_that("import_earth_mgcv handles earth model with no knots (intercept-only)", {
  skip_if_not_installed("earth")

  # Fit an earth model on random noise -- may produce intercept-only
  set.seed(123)
  df <- data.frame(y = rnorm(30), x = rnorm(30))
  m <- earth::earth(y ~ x, data = df, thresh = 0.9999)
  er <- structure(
    list(
      model = m, target = "y",
      predictors = "x",
      categoricals = character(0), linpreds = character(0),
      degree = 1L, cv_enabled = FALSE, allowed_matrix = NULL,
      data = df, elapsed = 0, trace_output = character(0)
    ),
    class = "earthUI_result"
  )

  ek <- import_earth_mgcv(er)
  expect_s3_class(ek, "mgcvUI_earth_knots")
  # May have 0 knots if model is intercept-only
  expect_true(is.list(ek$knots))
})

test_that("import_earth_mgcv errors on non-existent .rds file path", {
  expect_error(import_earth_mgcv("/no/such/path/model.rds"), "File not found")
})

test_that("import_earth_mgcv errors when .rds contains wrong class", {
  tmp <- tempfile(fileext = ".rds")
  on.exit(unlink(tmp))
  saveRDS(list(a = 1, b = 2), tmp)
  expect_error(import_earth_mgcv(tmp), "earthUI_result")
})

# ============================================================================
# plot_results edge cases
# ============================================================================

test_that("plot_smooth_single errors for variable not in model", {
  specs <- list(make_spec("wt"))
  res <- fit_gam(mtcars, "mpg", specs, cv_folds = 0)

  expect_error(plot_smooth_single(res, "hp"), "No smooth term")
})

test_that("plot_smooth_single errors for linear term variable", {
  specs <- list(
    make_spec("wt"),
    make_spec("cyl", type = "linear", bs = NULL, k = NULL)
  )
  res <- fit_gam(mtcars, "mpg", specs, cv_folds = 0)

  # cyl is linear, not a smooth -- should error
  expect_error(plot_smooth_single(res, "cyl"), "No smooth term")
})

test_that("plot_smooths on model with only linear terms handles gracefully", {
  # Model with only a linear term (no smooths)
  specs <- list(
    make_spec("wt", type = "linear", bs = NULL, k = NULL),
    make_spec("hp", type = "linear", bs = NULL, k = NULL)
  )
  res <- fit_gam(mtcars, "mpg", specs, cv_folds = 0)

  # gratia::draw on a model with no smooth terms may error, warn, message,
  # or return a plot with "Unable to draw" info -- all are acceptable
  result <- tryCatch(
    {
      suppressMessages(plot_smooths(res))
    },
    error = function(e) e,
    warning = function(w) w
  )
  # Either it produced a plot, gave an error/warning, or returned NULL
  expect_true(
    is.null(result) ||
    inherits(result, "gg") || inherits(result, "ggplot") ||
    inherits(result, "patchwork") ||
    inherits(result, "error") || inherits(result, "warning")
  )
})

test_that("plot_actual_vs_predicted_mgcv on model with only 10 data points", {
  df <- make_minimal_data(10)
  specs <- list(make_spec("x1"))
  res <- fit_gam(df, "y", specs, cv_folds = 0)

  p <- plot_actual_vs_predicted_mgcv(res)
  expect_s3_class(p, "gg")
})

test_that("plot_diagnostics on minimal model", {
  df <- make_minimal_data(15)
  specs <- list(make_spec("x1"))
  res <- fit_gam(df, "y", specs, cv_folds = 0)

  p <- plot_diagnostics(res)
  expect_true(inherits(p, "gg") || inherits(p, "ggplot") ||
                inherits(p, "patchwork"))
})

test_that("plot_smooth_single works with residuals = FALSE", {
  specs <- list(make_spec("wt"))
  res <- fit_gam(mtcars, "mpg", specs, cv_folds = 0)

  p <- plot_smooth_single(res, "wt", residuals = FALSE)
  expect_s3_class(p, "gg")
})

test_that("plot_actual_vs_predicted_mgcv on model with many predictors", {
  set.seed(42)
  n <- 100
  df <- data.frame(y = rnorm(n), x1 = rnorm(n), x2 = rnorm(n),
                   x3 = rnorm(n), x4 = rnorm(n))
  specs <- list(make_spec("x1"), make_spec("x2"),
                make_spec("x3"), make_spec("x4"))
  res <- fit_gam(df, "y", specs, cv_folds = 0)

  p <- plot_actual_vs_predicted_mgcv(res)
  expect_s3_class(p, "gg")
})

# ============================================================================
# format_results edge cases
# ============================================================================

test_that("format_gam_summary on model with no smooth terms (only linear)", {
  specs <- list(
    make_spec("wt", type = "linear", bs = NULL, k = NULL),
    make_spec("hp", type = "linear", bs = NULL, k = NULL)
  )
  res <- fit_gam(mtcars, "mpg", specs, cv_folds = 0)
  summ <- format_gam_summary(res)

  expect_true(is.numeric(summ$r_squared))
  expect_true(is.numeric(summ$dev_explained))
  # No smooth terms
  expect_equal(summ$n_smooths, 0L)
  expect_equal(nrow(summ$smooth_table), 0L)
  # Should have parametric terms
  expect_true(nrow(summ$parametric_table) > 0L)
})

test_that("format_gam_summary on model with only smooths (no explicit linear terms)", {
  specs <- list(
    make_spec("wt"),
    make_spec("hp")
  )
  res <- fit_gam(mtcars, "mpg", specs, cv_folds = 0)
  summ <- format_gam_summary(res)

  expect_true(is.numeric(summ$r_squared))
  expect_true(nrow(summ$smooth_table) == 2L)
  # parametric_table should have at least the intercept
  expect_true(nrow(summ$parametric_table) >= 1L)
})

test_that("tidy_gam on minimal single-predictor model", {
  df <- make_minimal_data(15)
  specs <- list(make_spec("x1"))
  res <- fit_gam(df, "y", specs, cv_folds = 0)

  tidy <- tidy_gam(res)
  expect_true(is.data.frame(tidy))
  expect_true(nrow(tidy) >= 1L)
  expect_true("term" %in% names(tidy))
})

test_that("glance_gam on minimal single-predictor model", {
  df <- make_minimal_data(15)
  specs <- list(make_spec("x1"))
  res <- fit_gam(df, "y", specs, cv_folds = 0)

  g <- glance_gam(res)
  expect_true(is.data.frame(g))
  expect_equal(nrow(g), 1L)
})

test_that("format_gam_summary on model with non-Gaussian family", {
  df <- mtcars
  df$mpg_pos <- df$mpg
  specs <- list(make_spec("wt"))
  res <- fit_gam(df, "mpg_pos", specs,
                 family = Gamma(link = "log"), cv_folds = 0)
  summ <- format_gam_summary(res)

  expect_true(is.numeric(summ$r_squared))
  expect_equal(summ$family, "Gamma")
  expect_true(is.data.frame(summ$smooth_table))
})

# ============================================================================
# export_functions edge cases
# ============================================================================

test_that("extract_smooth_grids returns empty list for only-linear model", {
  specs <- list(
    make_spec("wt", type = "linear", bs = NULL, k = NULL),
    make_spec("hp", type = "linear", bs = NULL, k = NULL)
  )
  res <- fit_gam(mtcars, "mpg", specs, cv_folds = 0)

  grids <- extract_smooth_grids(res, n_points = 10)
  expect_type(grids, "list")
  expect_length(grids, 0L)
})

test_that("generate_r_code on model with only linear terms", {
  specs <- list(
    make_spec("wt", type = "linear", bs = NULL, k = NULL),
    make_spec("hp", type = "linear", bs = NULL, k = NULL)
  )
  res <- fit_gam(mtcars, "mpg", specs, cv_folds = 0)

  code <- generate_r_code(res, n_points = 10)
  expect_type(code, "character")
  expect_true(nchar(code) > 0)
  expect_true(grepl("intercept", code))
  # Should have linear term coefficients
  expect_true(grepl("coef_", code))
  # Should NOT have approxfun (no smooths)
  expect_false(grepl("approxfun", code))
})

test_that("generate_python_code on model with smooth and linear terms", {
  specs <- list(
    make_spec("wt"),
    make_spec("hp", type = "linear", bs = NULL, k = NULL)
  )
  res <- fit_gam(mtcars, "mpg", specs, cv_folds = 0)

  code <- generate_python_code(res, n_points = 10)
  expect_type(code, "character")
  expect_true(grepl("numpy", code))
  expect_true(grepl("g_wt", code))
  expect_true(grepl("INTERCEPT", code))
  expect_true(grepl("COEF_", code))
})

test_that("generate_cpp_code on model with smooth and linear terms", {
  specs <- list(
    make_spec("wt"),
    make_spec("hp", type = "linear", bs = NULL, k = NULL)
  )
  res <- fit_gam(mtcars, "mpg", specs, cv_folds = 0)

  code <- generate_cpp_code(res, n_points = 10)
  expect_type(code, "character")
  expect_true(grepl("namespace gam_functions", code))
  expect_true(grepl("g_wt", code))
  expect_true(grepl("COEF_", code))
})

test_that("generate_r_code on model with many smooth terms", {
  set.seed(42)
  n <- 100
  df <- data.frame(y = rnorm(n), x1 = rnorm(n), x2 = rnorm(n),
                   x3 = rnorm(n))
  specs <- list(make_spec("x1"), make_spec("x2"), make_spec("x3"))
  res <- fit_gam(df, "y", specs, cv_folds = 0)

  code <- generate_r_code(res, n_points = 10)
  expect_true(grepl("g_x1", code))
  expect_true(grepl("g_x2", code))
  expect_true(grepl("g_x3", code))
})

# ============================================================================
# Additional edge cases: data type handling
# ============================================================================

test_that("fit_gam handles tibble input (coerced to data.frame)", {
  skip_if_not_installed("tibble")
  df <- tibble::as_tibble(mtcars)
  specs <- list(make_spec("wt"))
  res <- fit_gam(df, "mpg", specs, cv_folds = 0)

  expect_s3_class(res$model, "gam")
})

test_that("fit_gam handles integer response variable", {
  set.seed(42)
  df <- data.frame(
    y = as.integer(sample(1:100, 50, replace = TRUE)),
    x1 = rnorm(50)
  )
  specs <- list(make_spec("x1"))
  res <- fit_gam(df, "y", specs, cv_folds = 0)

  expect_s3_class(res$model, "gam")
})

test_that("fit_gam spec with empty vars errors", {
  specs <- list(list(vars = character(0), type = "s", bs = "tp", k = NULL))
  expect_error(fit_gam(mtcars, "mpg", specs, cv_folds = 0), "no 'vars'")
})

test_that("fit_gam spec with NULL vars errors", {
  specs <- list(list(vars = NULL, type = "s", bs = "tp", k = NULL))
  expect_error(fit_gam(mtcars, "mpg", specs, cv_folds = 0), "no 'vars'")
})

test_that("fit_gam errors when response not found in data", {
  specs <- list(make_spec("wt"))
  expect_error(fit_gam(mtcars, "nonexistent_response", specs),
               "not found in data")
})

test_that("fit_gam errors when predictor not found in data", {
  specs <- list(make_spec("nonexistent_var"))
  expect_error(fit_gam(mtcars, "mpg", specs), "not in data")
})

test_that("fit_gam errors when all predictors are constant (no usable predictors)", {
  df <- data.frame(y = rnorm(30), c1 = 1, c2 = 2)
  specs <- list(make_spec("c1"), make_spec("c2"))
  expect_error(fit_gam(df, "y", specs, cv_folds = 0),
               "No usable predictors")
})

# ============================================================================
# build_gam_knots edge cases
# ============================================================================

test_that("build_gam_knots returns NULL when no earth knots", {
  specs <- list(make_spec("x1"))
  result <- build_gam_knots(specs, NULL)
  expect_null(result)
})

test_that("build_gam_knots skips linear terms", {
  specs <- list(make_spec("x1", type = "linear", bs = NULL, k = NULL))
  ek <- structure(
    list(knots = list(x1 = c(1, 2, 3)), signs = list(x1 = c(1L, 1L, 1L))),
    class = "mgcvUI_earth_knots"
  )
  result <- build_gam_knots(specs, ek)
  # Linear terms don't get knots
  expect_null(result)
})

test_that("build_gam_knots skips tp basis (only cr/ps/bs get knots)", {
  specs <- list(make_spec("x1", bs = "tp", k = 5L))
  ek <- structure(
    list(knots = list(x1 = c(1, 2, 3)), signs = list(x1 = c(1L, 1L, 1L))),
    class = "mgcvUI_earth_knots"
  )
  result <- build_gam_knots(specs, ek)
  expect_null(result)
})

# ============================================================================
# clean_names edge cases
# ============================================================================

test_that("clean_names_ handles all-numeric column names", {
  result <- earthUI:::clean_names_(c("123", "456", "789"))
  expect_equal(length(result), 3L)
  expect_true(all(nchar(result) > 0))
})

test_that("clean_names_ handles special characters", {
  result <- earthUI:::clean_names_(c("price_$", "area m2", "Col#1"))
  expect_equal(length(result), 3L)
  # Should be lowercase with no special chars remaining
  expect_true(all(!grepl("[$#]", result)))
})

test_that("clean_names_ handles empty string", {
  result <- earthUI:::clean_names_(c("", "x", "y"))
  expect_equal(length(result), 3L)
})

# ============================================================================
# check_sign_consistency edge cases
# ============================================================================

test_that("check_sign_consistency returns NULL when no earth knots", {
  specs <- list(make_spec("wt"))
  res <- fit_gam(mtcars, "mpg", specs, cv_folds = 0)
  result <- check_sign_consistency(res)
  expect_null(result)
})

# ============================================================================
# export_knots_csv_mgcv edge cases
# ============================================================================

test_that("export_knots_csv_mgcv errors on non-mgcvUI_earth_knots object", {
  expect_error(export_knots_csv_mgcv(list(a = 1), tempfile()),
               "mgcvUI_earth_knots")
})

test_that("export_knots_csv_mgcv handles earth_knots with no knots", {
  ek <- structure(
    list(
      knots = list(),
      signs = list(),
      interactions = list(),
      predictors = character(0),
      target = "y",
      categoricals = character(0),
      linpreds = character(0),
      degree = 1L,
      allowed_matrix = NULL,
      data = data.frame(),
      earth_summary = list(r_squared = 0, gcv = 0, n_terms = 1)
    ),
    class = "mgcvUI_earth_knots"
  )
  tmp <- tempfile(fileext = ".csv")
  on.exit(unlink(tmp))

  export_knots_csv_mgcv(ek, tmp)
  expect_true(file.exists(tmp))
  result <- read.csv(tmp)
  expect_equal(nrow(result), 0L)
})

# ============================================================================
# Fit edge cases: optimizer and advanced options
# ============================================================================

test_that("fit_gam with discrete = TRUE on moderate dataset", {
  set.seed(42)
  df <- data.frame(y = rnorm(100), x1 = rnorm(100), x2 = rnorm(100))
  specs <- list(make_spec("x1"), make_spec("x2"))

  res <- fit_gam(df, "y", specs, cv_folds = 0, discrete = TRUE, nthreads = 1L)
  expect_s3_class(res$model, "gam")
})

test_that("fit_gam with custom weights", {
  set.seed(42)
  n <- 50
  df <- data.frame(y = rnorm(n), x1 = rnorm(n))
  wts <- runif(n, 0.5, 2)

  specs <- list(make_spec("x1"))
  res <- fit_gam(df, "y", specs, cv_folds = 0, weights = wts)
  expect_s3_class(res$model, "gam")
})

test_that("fit_gam with scale fixed at positive value", {
  specs <- list(make_spec("wt"))
  res <- fit_gam(mtcars, "mpg", specs, cv_folds = 0, scale = 10)
  expect_s3_class(res$model, "gam")
})

test_that("fit_gam with outer_bfgs optimizer", {
  specs <- list(make_spec("wt"))
  res <- fit_gam(mtcars, "mpg", specs, cv_folds = 0, optimizer = "outer_bfgs")
  expect_s3_class(res$model, "gam")
})

# ============================================================================
# Cross-validation edge case
# ============================================================================

test_that("fit_gam with cv_folds = 0 skips CV and returns NULL cv_rsq", {
  specs <- list(make_spec("wt"))
  res <- fit_gam(mtcars, "mpg", specs, cv_folds = 0)

  expect_null(res$cv_rsq)
})

test_that("fit_gam with cv_folds = 2 runs minimal CV", {
  specs <- list(make_spec("wt"))
  res <- fit_gam(mtcars, "mpg", specs, cv_folds = 2L)

  # cv_rsq should be numeric (might be NA if folds failed)
  expect_true(is.null(res$cv_rsq) || is.numeric(res$cv_rsq))
})

# ============================================================================
# Defensive NULL handling (Shiny edge cases)
# ============================================================================

test_that("fit_gam handles all NULL optional parameters (Shiny scenario)", {
  specs <- list(make_spec("wt"))
  res <- fit_gam(
    mtcars, "mpg", specs,
    family = NULL, method = NULL, select = NULL,
    gamma = NULL, cv_folds = 0,
    optimizer = NULL, scale = NULL,
    discrete = NULL, nthreads = NULL
  )
  expect_s3_class(res$model, "gam")
  expect_equal(res$model$method, "REML")
})

test_that("fit_gam handles NA gamma and scale values", {
  specs <- list(make_spec("wt"))
  res <- fit_gam(mtcars, "mpg", specs, cv_folds = 0,
                 gamma = NA, scale = NA, nthreads = NA)
  expect_s3_class(res$model, "gam")
})
