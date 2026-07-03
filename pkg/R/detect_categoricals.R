#' Detect categorical variables in a data frame
#'
#' Flags columns that are categorical **by type** — character, factor, or
#' logical. These can only be treated as factors in a regression, so they are
#' the columns to pre-check in the Factor box. Numeric and integer columns are
#' **not** flagged: a numeric variable is a factor only when the user explicitly
#' designates it (the Factor checkbox), because by value alone a discrete
#' continuous predictor (e.g. `bath_count` = 0..5) is indistinguishable from a
#' numeric category code.
#'
#' @param df A data frame.
#'
#' @return A named logical vector with one element per column. `TRUE` indicates
#'   a character, factor, or logical column.
#'
#' @export
#' @examples
#' df <- data.frame(
#'   pool = c("Y", "N", "Y", "N"),
#'   bedrooms = c(2, 3, 2, 4),
#'   sqft = c(1200, 1500, 1300, 1800),
#'   stringsAsFactors = FALSE
#' )
#' detect_categoricals(df) # only `pool` is TRUE
detect_categoricals <- function(df) {
  if (!is.data.frame(df)) {
    stop("`df` must be a data frame.", call. = FALSE)
  }

  result <- vapply(df, function(col) {
    is.character(col) || is.factor(col) || is.logical(col)
  }, logical(1L))

  names(result) <- names(df)
  result
}
