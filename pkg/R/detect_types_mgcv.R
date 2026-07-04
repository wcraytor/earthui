#' Detect Column Types from a Data Frame
#'
#' Returns the R type of each column as a named character vector.
#'
#' @param df A data frame.
#' @return Named character vector with values like `"numeric"`,
#'   `"integer"`, `"factor"`, `"character"`, `"logical"`, `"Date"`,
#'   `"POSIXct"`.
#' @export
#' @examples
#' detect_column_types_mgcv(mtcars)
#' df <- data.frame(
#'   x = 1:5, y = letters[1:5],
#'   d = Sys.Date() + 1:5,
#'   t = Sys.time() + 1:5
#' )
#' detect_column_types_mgcv(df)
detect_column_types_mgcv <- function(df) {
  vapply(df, function(col) {
    if (inherits(col, "POSIXct") || inherits(col, "POSIXlt")) return("POSIXct")
    if (inherits(col, "Date")) return("Date")
    if (is.factor(col)) return("factor")
    if (is.logical(col)) return("logical")
    if (is.integer(col)) return("integer")
    if (is.numeric(col)) return("numeric")
    if (is.character(col)) return("character")
    "character"
  }, character(1))
}
