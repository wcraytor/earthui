#' Detect likely categorical variables in a data frame
#'
#' Returns a logical named vector indicating which columns are likely
#' categorical. Character and factor columns are always flagged. Numeric
#' columns with fewer than `max_unique` unique values are also flagged.
#'
#' @param df A data frame.
#' @param max_unique Integer. Numeric columns with this many or fewer unique
#'   values are flagged as likely categorical. Default is 10.
#'
#' @return A named logical vector with one element per column. `TRUE` indicates
#'   the column is likely categorical.
#'
#' @export
#' @examples
#' df <- data.frame(
#'   price = c(100, 200, 300, 400),
#'   pool = c("Y", "N", "Y", "N"),
#'   bedrooms = c(2, 3, 2, 4),
#'   sqft = c(1200, 1500, 1300, 1800)
#' )
#' detect_categoricals(df)
detect_categoricals <- function(df, max_unique = 10L) {
  if (!is.data.frame(df)) {
    stop("`df` must be a data frame.", call. = FALSE)
  }
  max_unique <- as.integer(max_unique)

  result <- vapply(df, function(col) {
    if (is.character(col) || is.factor(col) || is.logical(col)) {
      return(TRUE)
    }
    if (is.numeric(col)) {
      n_unique <- length(unique(col[!is.na(col)]))
      return(n_unique <= max_unique)
    }
    # Default: flag as categorical for unknown types
    TRUE
  }, logical(1L))

  names(result) <- names(df)
  result
}
