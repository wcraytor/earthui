#' Import data from CSV or Excel files
#'
#' Reads a CSV (.csv) or Excel (.xlsx, .xls) file and returns a data frame
#' with column names preserved exactly as they appear in the source file.
#'
#' @param filepath Character string. Path to the data file. Supported formats:
#'   `.csv`, `.xlsx`, `.xls`.
#' @param sheet Character or integer. For Excel files, the sheet to read.
#'   Defaults to the first sheet. Ignored for CSV files.
#' @param ... Additional arguments passed to [utils::read.csv()] or
#'   [readxl::read_excel()].
#'
#' @return A data frame with column names preserved from the source file.
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

  if (nrow(df) == 0L) {
    warning("Imported file has zero rows.", call. = FALSE)
  }
  if (ncol(df) == 0L) {
    stop("Imported file has zero columns.", call. = FALSE)
  }

  df
}
