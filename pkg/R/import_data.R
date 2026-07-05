#' Import data from CSV or Excel files
#'
#' Reads a CSV (.csv) or 'Excel' (.xlsx, .xls) file and returns a data frame.
#' Column names are converted to snake_case and duplicates are made unique.
#'
#' @param filepath Character string. Path to the data file. Supported formats:
#'   `.csv`, `.xlsx`, `.xls`.
#' @param sheet Character or integer. For Excel files, the sheet to read.
#'   Defaults to the first sheet. Ignored for CSV files.
#' @param sep Character. Field separator for CSV files. Default `","`.
#'   Use `";"` for European-style CSVs.
#' @param dec Character. Decimal separator for CSV files. Default `"."`.
#'   Use `","` for European-style CSVs.
#' @param ... Additional arguments passed to [utils::read.csv()] or
#'   [readxl::read_excel()].
#'
#' @return A data frame with column names converted to snake_case.
#'   Duplicate column names are made unique by appending numeric suffixes.
#'
#' @export
#' @examples
#' # Load the included demo appraisal dataset
#' demo_file <- system.file("extdata", "Appraisal_1.csv", package = "earthUI")
#' df <- import_data(demo_file)
#' head(df)
import_data <- function(filepath, sheet = 1, sep = ",", dec = ".", ...) {
  if (!is.character(filepath) || length(filepath) != 1L) {
    stop("`filepath` must be a single character string.", call. = FALSE)
  }
  if (!file.exists(filepath)) {
    stop("File not found: ", filepath, call. = FALSE)
  }
  # Refuse files that plainly won't fit in memory (from the mgcv routine,
  # where a large xlsx exploding into a data frame used to freeze the
  # session; the guard applies equally to every routine).
  check_memory_for_file_(filepath, multiplier = 3)

  ext <- tolower(tools::file_ext(filepath))

  df <- switch(ext,
    csv = utils::read.csv(filepath, stringsAsFactors = FALSE,
                          check.names = FALSE, sep = sep, dec = dec, ...),
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


#' Check available memory before loading a file
#'
#' Compares file size (times a multiplier for in-memory expansion) against
#' available system memory. Stops with a clear message if insufficient.
#'
#' @param filepath Character path to the file.
#' @param multiplier Numeric factor for estimated in-memory size relative to
#'   file size on disk. CSV text expands ~3x; RDS objects can expand ~5x.
#' @return Invisible NULL. Stops with an error if memory is insufficient.
#' @noRd
check_memory_for_file_ <- function(filepath, multiplier = 3) {
  fsize <- tryCatch(file.size(filepath), error = function(e) NA_real_)
  if (is.na(fsize) || fsize == 0) return(invisible(NULL))

  avail <- available_memory_bytes_()
  if (is.na(avail)) return(invisible(NULL))  # can't determine, proceed

  estimated <- fsize * multiplier
  if (estimated > avail) {
    fsize_gb  <- round(fsize / 1024^3, 2)
    est_gb    <- round(estimated / 1024^3, 2)
    avail_gb  <- round(avail / 1024^3, 2)
    stop(sprintf(
      paste0("Insufficient memory to load this file.\n",
             "  File size on disk: %s GB\n",
             "  Estimated memory needed: %s GB\n",
             "  Available memory: %s GB\n",
             "Close other applications or use a smaller file."),
      fsize_gb, est_gb, avail_gb
    ), call. = FALSE)
  }
  invisible(NULL)
}


#' Query available system memory in bytes
#' @return Numeric bytes available, or NA if undetermined.
#' @noRd
available_memory_bytes_ <- function() {
  os <- Sys.info()[["sysname"]]
  tryCatch({
    if (os == "Darwin") {
      # macOS: use vm_stat for free + inactive pages
      vm <- system("vm_stat", intern = TRUE)
      page_size <- as.numeric(
        sub(".*page size of (\\d+) bytes.*", "\\1", vm[1])
      )
      parse_pages <- function(label) {
        line <- grep(label, vm, value = TRUE)
        if (length(line) == 0L) return(0)
        as.numeric(gsub("[^0-9]", "", line[1]))
      }
      free_pages     <- parse_pages("Pages free")
      inactive_pages <- parse_pages("Pages inactive")
      (free_pages + inactive_pages) * page_size
    } else if (os == "Linux") {
      # Linux: use /proc/meminfo MemAvailable
      mi <- readLines("/proc/meminfo", n = 10L)
      line <- grep("^MemAvailable:", mi, value = TRUE)
      if (length(line) == 0L) return(NA_real_)
      kb <- as.numeric(gsub("[^0-9]", "", line[1]))
      kb * 1024
    } else if (os == "Windows") {
      # Windows: use wmic or Sys.getenv
      out <- system("wmic OS get FreePhysicalMemory /value",
                     intern = TRUE)
      line <- grep("FreePhysicalMemory", out, value = TRUE)
      if (length(line) == 0L) return(NA_real_)
      kb <- as.numeric(gsub("[^0-9]", "", line[1]))
      kb * 1024
    } else {
      NA_real_
    }
  }, error = function(e) NA_real_)
}
