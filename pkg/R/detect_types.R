#' Detect column types in a data frame
#'
#' Inspects each column and returns a best-guess R type string.
#' Character columns are tested for common date patterns. Numeric columns
#' containing only 0/1 values (with both present) are flagged as logical.
#'
#' @param df A data frame.
#' @return A named character vector with one element per column.
#'   Possible values: \code{"numeric"}, \code{"integer"}, \code{"character"},
#'   \code{"logical"}, \code{"factor"}, \code{"Date"}, \code{"POSIXct"},
#'   \code{"unknown"}.
#'
#' @export
#' @examples
#' df <- data.frame(
#'   price = c(100.5, 200.3, 300.1),
#'   rooms = c(2L, 3L, 4L),
#'   pool  = c("Y", "N", "Y"),
#'   sold  = c(TRUE, FALSE, TRUE)
#' )
#' detect_types(df)
detect_types <- function(df) {
  if (!is.data.frame(df)) {
    stop("`df` must be a data frame.", call. = FALSE)
  }

  result <- vapply(df, function(col) {
    cls <- class(col)[1L]

    # Date / time classes (check before numeric since POSIXct is also numeric)
    if (inherits(col, "POSIXct") || inherits(col, "POSIXlt")) return("POSIXct")
    if (inherits(col, "Date")) return("Date")

    # Logical
    if (cls == "logical") return("logical")

    # Factor
    if (cls == "factor") return("factor")

    # Numeric / integer — check for 0/1 boolean pattern
    if (cls == "integer" || cls == "numeric" || cls == "double") {
      non_na <- col[!is.na(col)]
      if (length(non_na) > 0L &&
          all(non_na %in% c(0, 1)) &&
          length(unique(non_na)) == 2L) {
        return("logical")
      }
      if (cls == "integer") return("integer")
      return("numeric")
    }

    # Character — try to detect dates
    if (cls == "character") {
      non_na <- col[!is.na(col)]
      if (length(non_na) == 0L) return("character")
      sample_vals <- utils::head(non_na, 100L)
      if (is_date_like_(sample_vals)) return("Date")
      return("character")
    }

    "unknown"
  }, character(1L))

  names(result) <- names(df)
  result
}


# Internal: check if character values look like dates
#
# Tries common date formats on a sample. If >= 80% parse successfully
# with any single format, returns TRUE.
is_date_like_ <- function(x) {
  if (length(x) == 0L) return(FALSE)

  formats <- c(
    "%Y-%m-%d",             # 2024-01-15
    "%Y/%m/%d",             # 2024/01/15
    "%m/%d/%Y",             # 01/15/2024
    "%m-%d-%Y",             # 01-15-2024
    "%d/%m/%Y",             # 15/01/2024
    "%d-%m-%Y",             # 15-01-2024
    "%Y-%m-%d %H:%M:%S",   # 2024-01-15 10:30:00
    "%m/%d/%Y %H:%M:%S",   # 01/15/2024 10:30:00
    "%Y-%m-%dT%H:%M:%S",   # ISO 8601
    "%b %d, %Y",            # Jan 15, 2024
    "%B %d, %Y"             # January 15, 2024
  )

  threshold <- 0.8
  n <- length(x)

  for (fmt in formats) {
    parsed <- suppressWarnings(as.Date(x, format = fmt))
    if (sum(!is.na(parsed)) / n >= threshold) return(TRUE)
  }

  FALSE
}


# Internal: coerce columns to their declared types before fitting
#
# Converts Date/POSIXct to numeric (days/seconds since epoch),
# logical 0/1, and factor/character as needed. Operates on a copy
# of the data frame (R copy-on-modify semantics).
#
# @param df Data frame to coerce.
# @param type_map Named list or character vector mapping column names to types.
# @param predictors Character vector of predictor column names to process.
# @return Modified data frame.
coerce_types_ <- function(df, type_map, predictors) {
  for (col in predictors) {
    declared <- type_map[[col]]
    if (is.null(declared) || declared == "unknown") next
    if (!(col %in% names(df))) next

    if (declared %in% c("Date", "POSIXct")) {
      if (inherits(df[[col]], "Date")) {
        df[[col]] <- as.numeric(df[[col]])
      } else if (inherits(df[[col]], "POSIXct") || inherits(df[[col]], "POSIXlt")) {
        df[[col]] <- as.numeric(df[[col]])
      } else if (is.character(df[[col]])) {
        # Try to parse character dates, then convert to numeric
        parsed <- suppressWarnings(as.Date(df[[col]]))
        df[[col]] <- as.numeric(parsed)
      } else if (is.numeric(df[[col]])) {
        # Already numeric (e.g., Excel serial date) — leave as-is
      }
    } else if (declared == "logical") {
      if (is.numeric(df[[col]])) {
        df[[col]] <- as.logical(df[[col]])
      }
    } else if (declared == "factor") {
      if (!is.factor(df[[col]])) {
        df[[col]] <- as.factor(df[[col]])
      }
    } else if (declared == "integer") {
      if (is.numeric(df[[col]]) && !is.integer(df[[col]])) {
        df[[col]] <- as.integer(df[[col]])
      }
    } else if (declared == "character") {
      if (!is.character(df[[col]])) {
        df[[col]] <- as.character(df[[col]])
      }
    }
  }
  df
}
