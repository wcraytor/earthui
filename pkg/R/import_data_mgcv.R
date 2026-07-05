# The mgcv routine's importer. Since the 0.11.0 merge the actual reading,
# validation, memory guard, and name cleaning live in import_data() (see
# import_data.R) -- ONE implementation for all three routines. This wrapper
# keeps the mgcv routine's historical name and signature.

#' Import Data from CSV or Excel (mgcv routine)
#'
#' Thin wrapper around [import_data()], kept for the mgcv routine's call
#' sites and scripts. See [import_data()] for details.
#'
#' @param filepath Path to a `.csv`, `.xlsx`, or `.xls` file.
#' @param sheet For Excel files, the sheet to read. Default first sheet.
#' @param sep,dec CSV field and decimal separators.
#' @return A data frame with snake_case column names.
#' @export
import_data_mgcv <- function(filepath, sheet = 1L, sep = ",", dec = ".") {
  import_data(filepath, sheet = sheet, sep = sep, dec = dec)
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
