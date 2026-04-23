#' Select comparable sales for the Sales Comparison Grid
#'
#' Given an RCA-adjusted data frame (subject in row 1, comps in rows 2+),
#' builds comp-summary tables, filters by weight, sorts by gross adjustment
#' percentage, splits into "recommended" (gross adjustment < threshold) and
#' "others", and caps the recommended list.
#'
#' This is the non-Shiny computation kernel used by the earthUI Shiny app's
#' Sales Grid modal, and is also suitable for use from batch scripts.
#'
#' @param rca_df A data frame produced by the RCA workflow. Row 1 is the
#'   subject; rows 2+ are comps. Expected columns (any may be missing; NA is
#'   substituted): `id`, `street_address`, `sale_price`, `weight`,
#'   `gross_adjustments`, plus a sale-age column named by `sale_age_col`.
#' @param sale_age_col Character scalar giving the column name that holds
#'   sale age in days. Defaults to `"sale_age"`.
#' @param min_weight Numeric. Comps with `weight` strictly greater than this
#'   value are eligible. Default `0`.
#' @param max_gross_adj_pct Numeric. Comps whose `gross_adj_pct` is strictly
#'   below this threshold are classed as "recommended". Default `0.25`
#'   (25%).
#' @param max_recommended Integer. Upper bound on the number of recommended
#'   comps returned. Default `30`.
#'
#' @return A named list with:
#'   \describe{
#'     \item{recommended}{Data frame of recommended comps, sorted by
#'       `sale_age` ascending, capped at `max_recommended` rows.}
#'     \item{others}{Data frame of eligible comps not in the recommended
#'       set, sorted by `gross_adj_pct` ascending.}
#'   }
#'   Each data frame has columns: `row`, `id`, `address`, `sale_price`,
#'   `sale_age`, `weight`, `gross_adj`, `gross_adj_pct`.
#'
#' @export
select_sales_grid_comps <- function(rca_df,
                                    sale_age_col      = "sale_age",
                                    min_weight        = 0,
                                    max_gross_adj_pct = 0.25,
                                    max_recommended   = 30L) {
  if (!is.data.frame(rca_df)) {
    stop("`rca_df` must be a data frame.", call. = FALSE)
  }
  n_total <- nrow(rca_df)
  if (n_total < 2L) {
    stop("Need at least 2 rows (subject + 1 comp).", call. = FALSE)
  }
  max_recommended <- as.integer(max_recommended)

  wt_col <- if ("weight" %in% colnames(rca_df)) {
    rca_df[["weight"]]
  } else {
    rep(1, n_total)
  }

  comp_info <- data.frame(
    row        = 2:n_total,
    id         = if ("id" %in% colnames(rca_df)) {
                    rca_df[["id"]][2:n_total]
                 } else 2:n_total,
    address    = if ("street_address" %in% colnames(rca_df)) {
                    rca_df[["street_address"]][2:n_total]
                 } else rep("", n_total - 1L),
    sale_price = if ("sale_price" %in% colnames(rca_df)) {
                    rca_df[["sale_price"]][2:n_total]
                 } else rep(NA_real_, n_total - 1L),
    sale_age   = if (sale_age_col %in% colnames(rca_df)) {
                    rca_df[[sale_age_col]][2:n_total]
                 } else rep(NA, n_total - 1L),
    weight     = wt_col[2:n_total],
    gross_adj  = if ("gross_adjustments" %in% colnames(rca_df)) {
                    rca_df[["gross_adjustments"]][2:n_total]
                 } else rep(0, n_total - 1L),
    stringsAsFactors = FALSE
  )

  comp_info$gross_adj_pct <- ifelse(
    !is.na(comp_info$sale_price) & comp_info$sale_price != 0,
    abs(comp_info$gross_adj / comp_info$sale_price),
    NA
  )

  eligible <- comp_info[!is.na(comp_info$weight) & comp_info$weight > min_weight, ]
  eligible <- eligible[order(eligible$gross_adj_pct, na.last = TRUE), ]

  recommended <- eligible[!is.na(eligible$gross_adj_pct) &
                          eligible$gross_adj_pct < max_gross_adj_pct, ]
  recommended <- recommended[order(recommended$sale_age, na.last = TRUE), ]
  if (nrow(recommended) > max_recommended) {
    recommended <- recommended[seq_len(max_recommended), ]
  }

  others <- eligible[is.na(eligible$gross_adj_pct) |
                     eligible$gross_adj_pct >= max_gross_adj_pct, ]
  others <- others[order(others$gross_adj_pct, na.last = TRUE), ]

  list(recommended = recommended, others = others)
}
