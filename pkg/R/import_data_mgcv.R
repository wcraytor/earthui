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


#' Import Data from CSV or Excel
#'
#' Reads a CSV or Excel file and returns a data frame with cleaned column
#' names (snake_case, no special characters).
#'
#' @param filepath Character path to a `.csv`, `.xlsx`, or `.xls` file.
#' @param sheet Integer or character sheet identifier for Excel files.
#'   Ignored for CSV.
#' @param sep Character. Field separator for CSV files. Default `","`.
#'   Use `";"` for European-style CSVs.
#' @param dec Character. Decimal separator for CSV files. Default `"."`.
#'   Use `","` for European-style CSVs.
#' @return A data frame.
#' @export
#' @examples
#' tmp <- tempfile(fileext = ".csv")
#' write.csv(mtcars, tmp, row.names = FALSE)
#' df <- import_data_mgcv(tmp)
#' head(df)
#' unlink(tmp)
import_data_mgcv <- function(filepath, sheet = 1L, sep = ",", dec = ".") {
  check_memory_for_file_(filepath, multiplier = 3)
  ext <- tolower(tools::file_ext(filepath))
  df <- switch(ext,
    csv = utils::read.csv(filepath, stringsAsFactors = FALSE,
                          check.names = FALSE, sep = sep, dec = dec),
    xlsx = readxl::read_excel(filepath, sheet = sheet),
    xls  = readxl::read_excel(filepath, sheet = sheet),
    stop("Unsupported file type: ", ext,
         ". Use .csv, .xlsx, or .xls", call. = FALSE)
  )
  df <- as.data.frame(df, stringsAsFactors = FALSE, check.names = FALSE)
  names(df) <- clean_names_(names(df))
  df
}

#' Clean column names to snake_case
#' @param nms Character vector of names.
#' @return Character vector of cleaned names.
#' @noRd
clean_names_ <- function(nms) {
  nms <- gsub("[^[:alnum:]_.]", "_", nms)
  nms <- gsub("\\.+", "_", nms)
  nms <- gsub("([a-z])([A-Z])", "\\1_\\2", nms)
  nms <- tolower(nms)
  nms <- gsub("_+", "_", nms)
  nms <- gsub("^_|_$", "", nms)
  make.unique(nms, sep = "_")
}
