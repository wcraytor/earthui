#' Compute sale age in integer days
#'
#' Given a vector of contract (sale) dates and a single effective date, returns
#' the difference in integer days (`effective_date - contract_date`). Contract
#' dates may be supplied as `POSIXct`, `Date`, character strings parseable by
#' [as.POSIXct()], or numeric Excel serial date numbers (origin 1899-12-30).
#'
#' @param contract_vals A vector of contract/sale dates. Accepted types:
#'   `POSIXct`, `Date`, `character`, or `numeric` (Excel serial dates).
#' @param effective_date The effective (appraisal) date. Accepted types:
#'   `Date`, `POSIXct`, or `character` parseable by [as.POSIXct()].
#'
#' @return An integer vector the same length as `contract_vals`, giving the
#'   number of whole days between `effective_date` and each contract date.
#'   `NA` contract values propagate to `NA` results.
#'
#' @details This function is the non-Shiny computation kernel used by the
#'   earthUI Shiny app when computing a `sale_age` column from a designated
#'   `contract_date` column. It is also suitable for use from batch scripts.
#'
#' @export
#' @examples
#' compute_sale_age(
#'   contract_vals = as.Date(c("2024-01-15", "2024-06-01")),
#'   effective_date = as.Date("2025-01-01")
#' )
compute_sale_age <- function(contract_vals, effective_date) {
  if (is.null(contract_vals)) {
    stop("`contract_vals` must not be NULL.", call. = FALSE)
  }
  if (is.null(effective_date)) {
    stop("`effective_date` must not be NULL.", call. = FALSE)
  }

  eff_date <- as.POSIXct(as.character(effective_date))

  if (inherits(contract_vals, "POSIXct")) {
    contract_posix <- contract_vals
  } else if (inherits(contract_vals, "Date")) {
    contract_posix <- as.POSIXct(contract_vals)
  } else if (is.character(contract_vals)) {
    contract_posix <- suppressWarnings(as.POSIXct(contract_vals))
    if (all(is.na(contract_posix[!is.na(contract_vals)]))) {
      stop("Cannot parse contract date values as dates.", call. = FALSE)
    }
  } else if (is.numeric(contract_vals)) {
    contract_posix <- as.POSIXct(as.Date(contract_vals, origin = "1899-12-30"))
  } else {
    stop("Contract date values cannot be interpreted as dates.", call. = FALSE)
  }

  as.integer(difftime(eff_date, contract_posix, units = "days"))
}
