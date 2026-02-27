#' Format earth model summary
#'
#' Extracts key statistics from a fitted earth model including coefficients,
#' basis functions, R-squared, GCV, GRSq, and RSS.
#'
#' @param earth_result An object of class `"earthui_result"` as returned by
#'   [fit_earth()].
#'
#' @return A list containing:
#'   \describe{
#'     \item{coefficients}{Data frame of model coefficients and basis functions.}
#'     \item{r_squared}{Training R-squared.}
#'     \item{gcv}{Generalized cross-validation value.}
#'     \item{grsq}{Generalized R-squared (1 - GCV/variance).}
#'     \item{rss}{Residual sum of squares.}
#'     \item{n_terms}{Number of terms in the pruned model.}
#'     \item{n_predictors}{Number of predictors used in the final model.}
#'     \item{n_obs}{Number of observations.}
#'     \item{cv_rsq}{Cross-validated R-squared (if CV was used, else NA).}
#'   }
#'
#' @export
#' @examples
#' result <- fit_earth(mtcars, "mpg", c("cyl", "disp", "hp", "wt"))
#' summary_info <- format_summary(result)
#' summary_info$r_squared
format_summary <- function(earth_result) {
  validate_earthui_result(earth_result)
  model <- earth_result$model

  # Extract coefficients
  coefs <- as.data.frame(stats::coef(model))
  names(coefs) <- earth_result$target
  coefs$term <- rownames(coefs)
  rownames(coefs) <- NULL
  coefs <- coefs[, c("term", earth_result$target)]

  # Model statistics
  model_summary <- summary(model)
  rsq <- model_summary$rsq
  gcv <- model_summary$gcv
  grsq <- model_summary$grsq

  # RSS
  rss <- sum(stats::residuals(model)^2)

  # Number of terms and predictors
  n_terms <- length(model$selected.terms)
  n_preds <- length(unique(unlist(
    lapply(model$selected.terms[-1], function(i) {
      which(model$dirs[i, ] != 0)
    })
  )))

  # Cross-validated R-squared
  cv_rsq <- NA_real_
  if (earth_result$cv_enabled && !is.null(model$cv.rsq.tab)) {
    tab <- model$cv.rsq.tab
    last_row <- tab[nrow(tab), , drop = TRUE]
    # Column may be named "mean", "cv.rsq", or the target variable name
    if ("mean" %in% names(last_row)) {
      cv_rsq <- as.numeric(last_row["mean"])
    } else if (length(last_row) > 0L) {
      cv_rsq <- as.numeric(last_row[length(last_row)])
    }
  }

  list(
    coefficients  = coefs,
    r_squared     = as.numeric(rsq),
    gcv           = as.numeric(gcv),
    grsq          = as.numeric(grsq),
    rss           = rss,
    n_terms       = n_terms,
    n_predictors  = n_preds,
    n_obs         = nrow(earth_result$data),
    cv_rsq        = cv_rsq
  )
}

#' Format ANOVA decomposition
#'
#' Extracts the ANOVA table from a fitted earth model.
#'
#' @param earth_result An object of class `"earthui_result"` as returned by
#'   [fit_earth()].
#'
#' @return A data frame with the ANOVA decomposition showing which predictors
#'   contribute to each basis function and their importance.
#'
#' @export
#' @examples
#' result <- fit_earth(mtcars, "mpg", c("cyl", "disp", "hp", "wt"))
#' format_anova(result)
format_anova <- function(earth_result) {
  validate_earthui_result(earth_result)
  model <- earth_result$model

  # Capture the ANOVA output from earth
  anova_obj <- summary(model, style = "pmax")

  # Extract the dirs matrix to build our own ANOVA-like table
  dirs <- model$dirs[model$selected.terms, , drop = FALSE]
  coefs <- stats::coef(model)
  cuts <- model$cuts[model$selected.terms, , drop = FALSE]

  # Build basis function descriptions
  terms_desc <- vapply(seq_len(nrow(dirs)), function(i) {
    if (i == 1L) return("(Intercept)")
    active <- which(dirs[i, ] != 0)
    parts <- vapply(active, function(j) {
      var_name <- colnames(dirs)[j]
      direction <- if (dirs[i, j] == 1) "+" else "-"
      cut_val <- cuts[i, j]
      if (direction == "+") {
        sprintf("h(%s - %.4g)", var_name, cut_val)
      } else {
        sprintf("h(%.4g - %s)", cut_val, var_name)
      }
    }, character(1))
    paste(parts, collapse = " * ")
  }, character(1))

  # Variables involved in each term
  vars_involved <- vapply(seq_len(nrow(dirs)), function(i) {
    if (i == 1L) return("")
    active <- which(dirs[i, ] != 0)
    paste(colnames(dirs)[active], collapse = ", ")
  }, character(1))

  data.frame(
    term        = seq_len(nrow(dirs)),
    description = terms_desc,
    variables   = vars_involved,
    coefficient = as.numeric(coefs),
    stringsAsFactors = FALSE
  )
}

#' Format variable importance
#'
#' Extracts variable importance scores from a fitted earth model using
#' [earth::evimp()].
#'
#' @param earth_result An object of class `"earthui_result"` as returned by
#'   [fit_earth()].
#'
#' @return A data frame with columns `variable`, `nsubsets`, `gcv`, and `rss`,
#'   sorted by overall importance (nsubsets).
#'
#' @export
#' @examples
#' result <- fit_earth(mtcars, "mpg", c("cyl", "disp", "hp", "wt"))
#' format_variable_importance(result)
format_variable_importance <- function(earth_result) {
  validate_earthui_result(earth_result)
  model <- earth_result$model

  imp <- earth::evimp(model)

  if (is.null(imp) || length(imp) == 0L) {
    return(data.frame(
      variable = character(0),
      nsubsets = numeric(0),
      gcv = numeric(0),
      rss = numeric(0),
      stringsAsFactors = FALSE
    ))
  }

  # evimp returns a matrix-like object with class "evimp"
  imp_mat <- unclass(imp)
  imp_df <- as.data.frame(imp_mat, stringsAsFactors = FALSE)
  imp_df$variable <- rownames(imp_mat)
  rownames(imp_df) <- NULL

  # Select and ensure numeric columns
  keep_cols <- c("variable")
  for (col in c("nsubsets", "gcv", "rss")) {
    if (col %in% names(imp_df)) {
      imp_df[[col]] <- as.numeric(imp_df[[col]])
      keep_cols <- c(keep_cols, col)
    }
  }

  imp_df <- imp_df[, keep_cols, drop = FALSE]
  if ("nsubsets" %in% names(imp_df)) {
    imp_df <- imp_df[order(-imp_df$nsubsets), , drop = FALSE]
  }
  rownames(imp_df) <- NULL
  imp_df
}

#' Validate earthui_result object
#' @param x Object to validate.
#' @return Invisible NULL. Raises error if invalid.
#' @keywords internal
validate_earthui_result <- function(x) {
  if (!inherits(x, "earthui_result")) {
    stop("Expected an 'earthui_result' object from fit_earth().", call. = FALSE)
  }
  if (is.null(x$model)) {
    stop("Result object does not contain a model.", call. = FALSE)
  }
  invisible(NULL)
}
