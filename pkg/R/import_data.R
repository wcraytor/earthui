#' Import data from CSV or Excel files
#'
#' Reads a CSV (.csv) or Excel (.xlsx, .xls) file and returns a data frame.
#' Column names are converted to snake_case and duplicates are made unique.
#'
#' @param filepath Character string. Path to the data file. Supported formats:
#'   `.csv`, `.xlsx`, `.xls`.
#' @param sheet Character or integer. For Excel files, the sheet to read.
#'   Defaults to the first sheet. Ignored for CSV files.
#' @param ... Additional arguments passed to [utils::read.csv()] or
#'   [readxl::read_excel()].
#'
#' @return A data frame with column names converted to snake_case.
#'   Duplicate column names are made unique by appending numeric suffixes.
#'
#' @export
#' @examples
#' # Create a temporary CSV for demonstration
#' tmp <- tempfile(fileext = ".csv")
#' write.csv(mtcars, tmp, row.names = FALSE)
#' df <- import_data(tmp)
#' head(df)
#' unlink(tmp)
import_data <- function(filepath, sheet = 1, ...) {
  if (!is.character(filepath) || length(filepath) != 1L) {
    stop("`filepath` must be a single character string.", call. = FALSE)
  }
  if (!file.exists(filepath)) {
    stop("File not found: ", filepath, call. = FALSE)
  }

  ext <- tolower(tools::file_ext(filepath))

  df <- switch(ext,
    csv = utils::read.csv(filepath, stringsAsFactors = FALSE,
                          check.names = FALSE, ...),
    xlsx = ,
    xls = readxl::read_excel(filepath, sheet = sheet, ...),
    stop("Unsupported file format: .", ext,
         ". Supported formats: .csv, .xlsx, .xls", call. = FALSE)
  )

  # Ensure plain data.frame (not tibble) for consistent downstream handling
  df <- as.data.frame(df, stringsAsFactors = FALSE, check.names = FALSE)

  # Convert column names to snake_case
  col_names <- names(df)
  col_names <- gsub("[^[:alnum:]]+", "_", col_names)  # non-alphanumeric -> _
  col_names <- gsub("([a-z])([A-Z])", "\\1_\\2", col_names)  # camelCase -> camel_Case
  col_names <- tolower(col_names)  # lowercase
  col_names <- gsub("^_|_$", "", col_names)  # trim leading/trailing _
  col_names[col_names == ""] <- "col"
  names(df) <- col_names

  # Make duplicate column names unique
  col_names <- names(df)
  if (anyDuplicated(col_names)) {
    names(df) <- make.unique(col_names, sep = "_")
  }

  if (nrow(df) == 0L) {
    warning("Imported file has zero rows.", call. = FALSE)
  }
  if (ncol(df) == 0L) {
    stop("Imported file has zero columns.", call. = FALSE)
  }

  df
}
