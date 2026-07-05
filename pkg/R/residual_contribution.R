# Residual-layer contribution estimation for rare / excluded features.
#
# Rationale (RCA two-channel doctrine): features too rare for the regression
# channel (a workshop present in 3 of 780 sales) are residual features. Their
# market contribution is estimated in residual space -- the model is fit
# WITHOUT them, and their value sits in the residuals of the flagged sales.
# A joint regression of residuals on several such features splits the credit
# between correlated amenities (e.g. outbuilding_sqft and workshop) instead
# of double-counting the same dollars in two breakdown rows.

#' Estimate residual-layer contributions of excluded features
#'
#' Regresses the fitted model's residuals on one or more columns that were
#' NOT in the model, giving a "better than nothing" market-contribution
#' estimate for rare features, for use in an RCA residual breakdown. For a
#' binary flag the coefficient is a lump-sum contribution; for a numeric
#' column it is a per-unit contribution.
#'
#' @param result An `earthUI_result` from [fit_earth()].
#' @param df The full project data frame the model was fit from (may contain
#'   columns the model never saw). Rows are matched to the fitted data by
#'   row name.
#' @param features Character vector of column names in `df` to estimate.
#'   None of them may already be a model predictor.
#' @return A list of class `earthUI_residual_contribution`:
#'   \describe{
#'     \item{table}{data.frame: feature, estimate, std_error, t_value,
#'       n_nonzero (obs where the feature is TRUE / non-zero / non-NA).}
#'     \item{flagged}{named list; for each near-binary feature, a data.frame
#'       of the flagged rows and their residuals.}
#'     \item{n}{number of observations used.}
#'     \item{fit}{the underlying `lm` object.}
#'     \item{note}{workfile documentation paragraph (character).}
#'   }
#' @export
estimate_residual_contribution <- function(result, df, features) {
  stopifnot(inherits(result, "earthUI_result"))
  features <- unique(features)
  missing_cols <- setdiff(features, names(df))
  if (length(missing_cols) > 0L) {
    stop("Not in data: ", paste(missing_cols, collapse = ", "), call. = FALSE)
  }
  overlap <- intersect(features, used_predictors_(result))
  if (length(overlap) > 0L) {
    stop("Used by the fitted model (refit without them first; the residual ",
         "method needs the model blind to them): ",
         paste(overlap, collapse = ", "), call. = FALSE)
  }

  res  <- as.numeric(stats::residuals(result$model))
  rows <- rownames(result$data)
  feat_df <- df[rows, features, drop = FALSE]

  for (f in features) {
    v <- feat_df[[f]]
    if (is.character(v)) feat_df[[f]] <- factor(v)
    if (is.logical(v))   feat_df[[f]] <- as.numeric(v)
  }

  dat <- cbind(data.frame(.resid = res), feat_df)
  dat <- stats::na.omit(dat)
  fit <- stats::lm(.resid ~ ., data = dat)
  sm  <- summary(fit)$coefficients

  is_flagish <- function(v) is.numeric(v) && all(v %in% c(0, 1), na.rm = TRUE)
  n_nonzero <- vapply(features, function(f) {
    v <- feat_df[[f]]
    if (is.numeric(v)) sum(v != 0, na.rm = TRUE) else sum(!is.na(v))
  }, numeric(1))

  # coefficient rows for our features (factors expand; match by prefix)
  keep <- setdiff(rownames(sm), "(Intercept)")
  tab <- data.frame(
    term      = keep,
    estimate  = sm[keep, "Estimate"],
    std_error = sm[keep, "Std. Error"],
    t_value   = sm[keep, "t value"],
    row.names = NULL
  )
  tab$n_nonzero <- vapply(tab$term, function(tm) {
    hit <- features[vapply(features, function(f) startsWith(tm, f), logical(1))]
    if (length(hit) > 0L) n_nonzero[[hit[1]]] else NA_real_
  }, numeric(1))

  flagged <- list()
  for (f in features) {
    v <- feat_df[[f]]
    if (is_flagish(v)) {
      idx <- which(v == 1)
      flagged[[f]] <- data.frame(row = rows[idx], residual = res[idx])
    }
  }

  low_n <- tab$term[tab$n_nonzero < 10 & !is.na(tab$n_nonzero)]
  note <- paste0(
    "Residual-layer contribution estimate (RCA residual method): model fit ",
    "without {", paste(features, collapse = ", "), "}; residuals of the ",
    nrow(dat), " modeled sales regressed jointly on these columns. ",
    paste(sprintf("%s: %s (SE %s, n=%d)", tab$term,
                  formatC(tab$estimate, format = "f", big.mark = ",", digits = 0),
                  formatC(tab$std_error, format = "f", big.mark = ",", digits = 0),
                  as.integer(tab$n_nonzero)), collapse = "; "), ".",
    if (length(low_n) > 0L) paste0(
      " CAUTION: ", paste(low_n, collapse = ", "),
      " below regression resolution (n < 10) - residual-layer estimate only;",
      " corroborate with depreciated cost or a widened remarks-flagged sample."
    ) else ""
  )

  structure(list(table = tab, flagged = flagged, n = nrow(dat),
                 fit = fit, note = note),
            class = "earthUI_residual_contribution")
}


#' Predictors actually used by the fitted earth model
#'
#' A predictor can be offered to [fit_earth()] yet never selected by the
#' forward pass (typical for rare flags). Only predictors that entered the
#' model are off-limits to the residual method.
#' @param result An `earthUI_result`.
#' @return Character vector of used predictor names.
#' @export
used_predictors_ <- function(result) {
  ev <- tryCatch(earth::evimp(result$model, trim = TRUE),
                 error = function(e) NULL)
  if (is.null(ev) || nrow(ev) == 0L) return(character(0))
  used_terms <- rownames(ev)
  result$predictors[vapply(result$predictors, function(p) {
    any(startsWith(used_terms, p))
  }, logical(1))]
}
