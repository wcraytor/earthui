#' Extract Smooth Functions as Lookup Tables
#'
#' Evaluates each smooth term on a fine grid and returns x/fx pairs
#' suitable for interpolation in any language.
#'
#' @param gam_result An `mgcvUI_result` from [fit_gam()].
#' @param n_points Integer number of grid points per variable.
#'   Default `200`.
#' @return A named list of data frames, one per smooth variable.
#'   Each has columns `x` and `fx`.
#' @export
#' @examples
#' specs <- list(
#'   list(vars = "wt", type = "s", bs = "tp", k = NULL),
#'   list(vars = "hp", type = "s", bs = "tp", k = NULL)
#' )
#' res <- fit_gam(mtcars, "mpg", specs, cv_folds = 0)
#' grids <- extract_smooth_grids(res, n_points = 50)
#' names(grids)
extract_smooth_grids <- function(gam_result, n_points = 200L) {

  model <- gam_result$model
  model_data <- model$model
  grids <- list()

  for (spec in gam_result$smooth_specs) {
    if (spec$type == "linear") next
    if (length(spec$vars) != 1L) next

    var <- spec$vars
    if (!var %in% names(model_data)) next

    x_vals <- model_data[[var]]
    x_seq <- seq(min(x_vals, na.rm = TRUE), max(x_vals, na.rm = TRUE),
                 length.out = n_points)

    newdata <- model_data[rep(1L, n_points), , drop = FALSE]
    newdata[[var]] <- x_seq

    preds <- predict(model, newdata = newdata, type = "terms")
    smooth_col <- grep(paste0("s\\(", var), colnames(preds), value = TRUE)
    if (length(smooth_col) == 0L) next

    grids[[var]] <- data.frame(x = x_seq, fx = preds[, smooth_col[1L]])
  }

  grids
}


#' Export Smooth Functions to SQLite
#'
#' Writes smooth function grids to a SQLite database with tables
#' `smooth_grids` (variable, x, fx) and `model_info`.
#'
#' @param gam_result An `mgcvUI_result` from [fit_gam()].
#' @param file Character path for the output `.sqlite` file.
#' @param n_points Integer grid resolution. Default `200`.
#' @return The file path (invisibly).
#' @export
export_functions_sqlite <- function(gam_result, file, n_points = 200L) {
  if (!requireNamespace("DBI", quietly = TRUE) ||
        !requireNamespace("RSQLite", quietly = TRUE)) {
    stop("Packages 'DBI' and 'RSQLite' are required.", call. = FALSE)
  }

  grids <- extract_smooth_grids(gam_result, n_points)

  con <- DBI::dbConnect(RSQLite::SQLite(), file)
  on.exit(DBI::dbDisconnect(con), add = TRUE)

  # Smooth grids table
  rows <- lapply(names(grids), function(var) {
    g <- grids[[var]]
    data.frame(variable = var, x = g$x, fx = g$fx,
               stringsAsFactors = FALSE)
  })
  grid_df <- do.call(rbind, rows)
  DBI::dbWriteTable(con, "smooth_grids", grid_df, overwrite = TRUE)

  # Model info table
  sm <- summary(gam_result$model)
  info <- data.frame(
    response      = gam_result$response,
    r_squared     = sm$r.sq,
    cv_rsq        = if (!is.null(gam_result$cv_rsq)) {
      gam_result$cv_rsq
    } else {
      NA_real_
    },
    dev_explained = sm$dev.expl,
    aic           = AIC(gam_result$model),
    n_obs         = nrow(gam_result$model$model),
    family        = gam_result$model$family$family,
    method        = gam_result$model$method,
    formula       = deparse(gam_result$formula, width.cutoff = 500L),
    stringsAsFactors = FALSE
  )
  DBI::dbWriteTable(con, "model_info", info, overwrite = TRUE)

  # Linear terms table
  lin_specs <- Filter(function(s) s$type == "linear", gam_result$smooth_specs)
  if (length(lin_specs) > 0L) {
    coefs <- stats::coef(gam_result$model)
    lin_rows <- lapply(lin_specs, function(s) {
      var <- s$vars
      # Find matching coefficient(s)
      idx <- grep(paste0("^", var), names(coefs))
      if (length(idx) == 0L) return(NULL)
      data.frame(variable = names(coefs)[idx],
                 coefficient = unname(coefs[idx]),
                 stringsAsFactors = FALSE)
    })
    not_null <- function(x) !is.null(x)
    lin_rows <- Filter(not_null, lin_rows)
    if (length(lin_rows) > 0L) {
      lin_df <- do.call(rbind, lin_rows)
      DBI::dbWriteTable(con, "linear_terms", lin_df, overwrite = TRUE)
    }
  }

  # Intercept
  intercept <- stats::coef(gam_result$model)[["(Intercept)"]]
  DBI::dbWriteTable(con, "intercept",
                    data.frame(intercept = intercept),
                    overwrite = TRUE)

  invisible(file)
}


#' Generate R Code for Smooth Functions
#'
#' Creates a self-contained R script with `approxfun()` definitions
#' for each smooth term.
#'
#' @param gam_result An `mgcvUI_result` from [fit_gam()].
#' @param n_points Integer grid resolution. Default `200`.
#' @return Character string of R code.
#' @export
generate_r_code <- function(gam_result, n_points = 200L) {
  grids <- extract_smooth_grids(gam_result, n_points)
  intercept <- stats::coef(gam_result$model)[["(Intercept)"]]

  lines <- c(
    paste0("# GAM smooth functions exported from mgcvUI"),
    paste0("# Response: ", gam_result$response),
    paste0("# R-squared: ", round(summary(gam_result$model)$r.sq, 6)),
    if (!is.null(gam_result$cv_rsq))
      paste0("# CV R-squared: ", round(gam_result$cv_rsq, 6)),
    paste0("# Formula: ", deparse(gam_result$formula, width.cutoff = 500L)),
    "",
    paste0("intercept <- ", format(intercept, digits = 10)),
    ""
  )

  for (var in names(grids)) {
    g <- grids[[var]]
    x_str <- paste0("c(", paste(format(g$x, digits = 10), collapse = ", "), ")")
    fx_str <- paste0(
      "c(", paste(format(g$fx, digits = 10), collapse = ", "), ")"
    )
    lines <- c(lines,
      paste0("g_", var, " <- approxfun("),
      paste0("  x = ", x_str, ","),
      paste0("  y = ", fx_str, ","),
      "  rule = 2",
      ")",
      ""
    )
  }

  # Linear terms
  coefs <- stats::coef(gam_result$model)
  lin_specs <- Filter(function(s) s$type == "linear", gam_result$smooth_specs)
  for (s in lin_specs) {
    var <- s$vars
    idx <- grep(paste0("^", var), names(coefs))
    if (length(idx) == 0L) next
    for (j in idx) {
      cname <- gsub("[^[:alnum:]_]", "_", names(coefs)[j])
      lines <- c(lines,
        paste0("coef_", cname, " <- ", format(coefs[j], digits = 10)),
        ""
      )
    }
  }

  # Prediction function
  lines <- c(lines,
    "# Predict: sum intercept + g_var(value) for each smooth",
    "# + coef_var * value for each linear term",
    ""
  )

  paste(lines, collapse = "\n")
}


#' Generate Python Code for Smooth Functions
#'
#' Creates a self-contained Python script with `numpy.interp()`
#' functions for each smooth term.
#'
#' @param gam_result An `mgcvUI_result` from [fit_gam()].
#' @param n_points Integer grid resolution. Default `200`.
#' @return Character string of Python code.
#' @export
generate_python_code <- function(gam_result, n_points = 200L) {
  grids <- extract_smooth_grids(gam_result, n_points)
  intercept <- stats::coef(gam_result$model)[["(Intercept)"]]

  lines <- c(
    '"""GAM smooth functions exported from mgcvUI"""',
    "import numpy as np",
    "",
    paste0("# Response: ", gam_result$response),
    paste0("# R-squared: ", round(summary(gam_result$model)$r.sq, 6)),
    if (!is.null(gam_result$cv_rsq))
      paste0("# CV R-squared: ", round(gam_result$cv_rsq, 6)),
    paste0("# Formula: ", deparse(gam_result$formula, width.cutoff = 500L)),
    "",
    paste0("INTERCEPT = ", format(intercept, digits = 10)),
    ""
  )

  for (var in names(grids)) {
    g <- grids[[var]]
    x_str <- paste(format(g$x, digits = 10), collapse = ", ")
    fx_str <- paste(format(g$fx, digits = 10), collapse = ", ")
    lines <- c(lines,
      paste0("_x_", var, " = np.array([", x_str, "])"),
      paste0("_fx_", var, " = np.array([", fx_str, "])"),
      "",
      paste0("def g_", var, "(x):"),
      paste0("    return np.interp(x, _x_", var, ", _fx_", var, ")"),
      ""
    )
  }

  # Linear terms
  coefs <- stats::coef(gam_result$model)
  lin_specs <- Filter(function(s) s$type == "linear", gam_result$smooth_specs)
  for (s in lin_specs) {
    var <- s$vars
    idx <- grep(paste0("^", var), names(coefs))
    if (length(idx) == 0L) next
    for (j in idx) {
      cname <- gsub("[^[:alnum:]_]", "_", names(coefs)[j])
      lines <- c(lines,
        paste0("COEF_", toupper(cname), " = ", format(coefs[j], digits = 10)),
        ""
      )
    }
  }

  paste(lines, collapse = "\n")
}


#' Generate C++ Code for Smooth Functions
#'
#' Creates a self-contained C++ header with linear interpolation
#' functions for each smooth term.
#'
#' @param gam_result An `mgcvUI_result` from [fit_gam()].
#' @param n_points Integer grid resolution. Default `200`.
#' @return Character string of C++ code.
#' @export
generate_cpp_code <- function(gam_result, n_points = 200L) {
  grids <- extract_smooth_grids(gam_result, n_points)
  intercept <- stats::coef(gam_result$model)[["(Intercept)"]]

  lines <- c(
    "// GAM smooth functions exported from mgcvUI",
    paste0("// Response: ", gam_result$response),
    paste0("// R-squared: ", round(summary(gam_result$model)$r.sq, 6)),
    if (!is.null(gam_result$cv_rsq))
      paste0("// CV R-squared: ", round(gam_result$cv_rsq, 6)),
    "#pragma once",
    "#include <vector>",
    "#include <algorithm>",
    "",
    "namespace gam_functions {",
    "",
    paste0(
      "constexpr double INTERCEPT = ",
      format(intercept, digits = 10), ";"
    ),
    "",
    "// Linear interpolation helper",
    paste0(
      "inline double interp(double x, ",
      "const double* xs, const double* ys, int n) {"
    ),
    "    if (x <= xs[0]) return ys[0];",
    "    if (x >= xs[n-1]) return ys[n-1];",
    "    auto it = std::lower_bound(xs, xs + n, x);",
    "    int i = static_cast<int>(it - xs);",
    "    if (i == 0) i = 1;",
    "    double t = (x - xs[i-1]) / (xs[i] - xs[i-1]);",
    "    return ys[i-1] + t * (ys[i] - ys[i-1]);",
    "}",
    ""
  )

  for (var in names(grids)) {
    g <- grids[[var]]
    n <- nrow(g)
    x_str <- paste(format(g$x, digits = 10), collapse = ", ")
    fx_str <- paste(format(g$fx, digits = 10), collapse = ", ")
    lines <- c(lines,
      paste0("inline double g_", var, "(double x) {"),
      paste0("    static const double xs[", n, "] = {", x_str, "};"),
      paste0("    static const double ys[", n, "] = {", fx_str, "};"),
      paste0("    return interp(x, xs, ys, ", n, ");"),
      "}",
      ""
    )
  }

  # Linear terms
  coefs <- stats::coef(gam_result$model)
  lin_specs <- Filter(function(s) s$type == "linear", gam_result$smooth_specs)
  for (s in lin_specs) {
    var <- s$vars
    idx <- grep(paste0("^", var), names(coefs))
    if (length(idx) == 0L) next
    for (j in idx) {
      cname <- gsub("[^[:alnum:]_]", "_", names(coefs)[j])
      lines <- c(lines,
        paste0("constexpr double COEF_", toupper(cname),
               " = ", format(coefs[j], digits = 10), ";"),
        ""
      )
    }
  }

  lines <- c(lines, "} // namespace gam_functions", "")
  paste(lines, collapse = "\n")
}


#' Export Smooth Functions as a Zip Archive
#'
#' Generates code files in selected languages and bundles them
#' into a zip file for download.
#'
#' @param gam_result An `mgcvUI_result` from [fit_gam()].
#' @param file Character path for the output `.zip` file.
#' @param languages Character vector of languages to include.
#'   Options: `"R"`, `"Python"`, `"C++"`, `"SQLite"`.
#' @param n_points Integer grid resolution. Default `200`.
#' @return The zip file path (invisibly).
#' @export
export_functions_zip <- function(gam_result, file,
                                 languages = c("R", "Python", "C++", "SQLite"),
                                 n_points = 200L) {
  tmpdir <- tempfile("gam_export_")
  dir.create(tmpdir)
  on.exit(unlink(tmpdir, recursive = TRUE), add = TRUE)

  files <- character(0)

  if ("R" %in% languages) {
    f <- file.path(tmpdir, "gam_functions.R")
    writeLines(generate_r_code(gam_result, n_points), f)
    files <- c(files, f)
  }

  if ("Python" %in% languages) {
    f <- file.path(tmpdir, "gam_functions.py")
    writeLines(generate_python_code(gam_result, n_points), f)
    files <- c(files, f)
  }

  if ("C++" %in% languages) {
    f <- file.path(tmpdir, "gam_functions.hpp")
    writeLines(generate_cpp_code(gam_result, n_points), f)
    files <- c(files, f)
  }

  if ("SQLite" %in% languages) {
    f <- file.path(tmpdir, "gam_functions.sqlite")
    export_functions_sqlite(gam_result, f, n_points)
    files <- c(files, f)
  }

  utils::zip(file, files, flags = "-j")
  invisible(file)
}
