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
#' Identifier-like text columns are not flagged either: when a character or
#' factor column has more than 10 unique values **and** more than half of its
#' non-NA values are distinct (street addresses, parcel numbers, MLS listing
#' ids), it is a per-row label rather than a usable categorical — one factor
#' level per data row carries no information. Logical columns are always
#' flagged (two values by construction).
#'
#' @param df A data frame.
#'
#' @return A named logical vector with one element per column. `TRUE` indicates
#'   a character, factor, or logical column that is usable as a categorical.
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
    if (is.logical(col)) return(TRUE)
    if (!(is.character(col) || is.factor(col))) return(FALSE)
    v <- col[!is.na(col)]
    n_u <- length(unique(v))
    # Identifier guard: (nearly) all-distinct text is a per-row label
    !(n_u > 10L && n_u > 0.5 * length(v))
  }, logical(1L))

  names(result) <- names(df)
  result
}
