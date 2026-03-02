#' Validate declared column types against actual data
#'
#' Checks each selected predictor's actual data against the user-declared type.
#' Returns a list of errors (blocking), warnings (non-blocking), and any
#' Date/POSIXct columns that will be auto-converted to numeric.
#'
#' @param df A data frame.
#' @param type_map Named list or character vector. Names are column names,
#'   values are declared types (e.g., \code{"numeric"}, \code{"Date"}).
#' @param predictors Character vector of selected predictor column names.
#' @return A list with components:
#'   \describe{
#'     \item{ok}{Logical. \code{TRUE} if no blocking errors found.}
#'     \item{warnings}{Character vector of non-blocking warnings.}
#'     \item{errors}{Character vector of blocking errors.}
#'     \item{date_columns}{Character vector of Date/POSIXct predictor columns
#'       that will be auto-converted to numeric.}
#'   }
#'
#' @export
#' @examples
#' df <- data.frame(price = c(100, 200, 300), city = c("A", "B", "C"))
#' types <- list(price = "numeric", city = "character")
#' validate_types(df, types, predictors = c("price", "city"))
validate_types <- function(df, type_map, predictors) {
  if (!is.data.frame(df)) {
    stop("`df` must be a data frame.", call. = FALSE)
  }

  warnings_out <- character(0)
  errors_out   <- character(0)
  date_cols    <- character(0)

  for (col in predictors) {
    if (!(col %in% names(df))) {
      errors_out <- c(errors_out, sprintf("Column '%s' not found in data.", col))
      next
    }

    declared <- type_map[[col]]
    if (is.null(declared) || declared == "unknown") next

    actual <- df[[col]]
    actual_class <- class(actual)[1L]

    # Helper: format row numbers for messages (show up to 10)
    fmt_rows <- function(idx) {
      if (length(idx) <= 10L) {
        paste(idx, collapse = ", ")
      } else {
        paste0(paste(idx[1:10], collapse = ", "), ", ... (", length(idx), " total)")
      }
    }

    # Date / POSIXct: flag for auto-conversion, check row-level parse failures
    if (declared %in% c("Date", "POSIXct")) {
      date_cols <- c(date_cols, col)
      if (is.character(actual)) {
        non_na_idx <- which(!is.na(actual))
        if (length(non_na_idx) > 0L) {
          # Try parsing with common formats
          parsed <- NULL
          formats <- c("%Y-%m-%d", "%Y/%m/%d", "%m/%d/%Y", "%m-%d-%Y",
                       "%d/%m/%Y", "%d-%m-%Y", "%Y-%m-%d %H:%M:%S",
                       "%m/%d/%Y %H:%M:%S", "%Y-%m-%dT%H:%M:%S",
                       "%b %d, %Y", "%B %d, %Y")
          for (fmt in formats) {
            p <- suppressWarnings(as.Date(actual, format = fmt))
            if (sum(!is.na(p[non_na_idx])) / length(non_na_idx) >= 0.5) {
              parsed <- p
              break
            }
          }
          if (is.null(parsed)) {
            errors_out <- c(errors_out, sprintf(
              "'%s': declared %s but values don't parse as dates.", col, declared))
          } else {
            bad_rows <- non_na_idx[is.na(parsed[non_na_idx])]
            if (length(bad_rows) > 0L) {
              bad_vals <- utils::head(actual[bad_rows], 5L)
              warnings_out <- c(warnings_out, sprintf(
                "'%s': %d value(s) could not be parsed as %s (rows %s). Examples: %s. These will become NA.",
                col, length(bad_rows), declared, fmt_rows(bad_rows),
                paste0("\"", bad_vals, "\"", collapse = ", ")))
            }
          }
        }
      }
      next
    }

    # Numeric
    if (declared == "numeric") {
      if (!is.numeric(actual)) {
        if (is.character(actual)) {
          # Row-level: which values fail as.numeric()?
          non_na_idx <- which(!is.na(actual))
          parsed <- suppressWarnings(as.numeric(actual))
          bad_rows <- non_na_idx[is.na(parsed[non_na_idx])]
          if (length(bad_rows) > 0L) {
            bad_vals <- utils::head(actual[bad_rows], 5L)
            errors_out <- c(errors_out, sprintf(
              "'%s': declared numeric but %d value(s) are not numeric (rows %s). Examples: %s",
              col, length(bad_rows), fmt_rows(bad_rows),
              paste0("\"", bad_vals, "\"", collapse = ", ")))
          } else {
            errors_out <- c(errors_out, sprintf(
              "'%s': declared numeric but actual class is '%s'.", col, actual_class))
          }
        } else {
          errors_out <- c(errors_out, sprintf(
            "'%s': declared numeric but actual class is '%s'.", col, actual_class))
        }
      }
    }

    # Integer
    if (declared == "integer") {
      if (is.numeric(actual) && !is.integer(actual)) {
        warnings_out <- c(warnings_out, sprintf(
          "'%s': declared integer but stored as double. Will truncate.", col))
      } else if (!is.numeric(actual)) {
        errors_out <- c(errors_out, sprintf(
          "'%s': declared integer but actual class is '%s'.", col, actual_class))
      }
    }

    # Logical
    if (declared == "logical") {
      if (is.logical(actual)) {
        # Already logical — fine
      } else if (is.numeric(actual)) {
        non_na <- actual[!is.na(actual)]
        bad_idx <- which(!is.na(actual) & !(actual %in% c(0, 1)))
        if (length(bad_idx) > 0L) {
          bad_vals <- utils::head(actual[bad_idx], 5L)
          errors_out <- c(errors_out, sprintf(
            "'%s': declared logical but %d value(s) are not 0/1 (rows %s). Examples: %s",
            col, length(bad_idx), fmt_rows(bad_idx),
            paste(bad_vals, collapse = ", ")))
        }
      } else {
        errors_out <- c(errors_out, sprintf(
          "'%s': declared logical but actual class is '%s'.", col, actual_class))
      }
    }

    # Factor
    if (declared == "factor") {
      if (!is.factor(actual) && !is.character(actual) && !is.logical(actual)) {
        warnings_out <- c(warnings_out, sprintf(
          "'%s': declared factor but actual class is '%s'. Will convert.",
          col, actual_class))
      }
    }

    # Character
    if (declared == "character") {
      if (!is.character(actual) && !is.factor(actual)) {
        warnings_out <- c(warnings_out, sprintf(
          "'%s': declared character but actual class is '%s'. Will convert.",
          col, actual_class))
      }
    }
  }

  list(
    ok           = length(errors_out) == 0L,
    warnings     = warnings_out,
    errors       = errors_out,
    date_columns = date_cols
  )
}
