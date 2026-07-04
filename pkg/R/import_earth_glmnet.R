#' Import an Earth Model for Use with glmnet
#'
#' Reads a saved `earthUI_result` object (`.rds` file) produced by the
#' \pkg{earthUI} package. The earth model is stored so that
#' [build_earth_basis()] can call `earth::model.matrix()` to generate
#' basis functions -- the standard approach for earth-to-glmnet
#' pipelines.
#'
#' When an earthUI result is imported, glmnet's own interaction matrix
#' is disabled. Interactions are handled exclusively by the earth
#' model's hinge basis functions, which are interpretable and
#' explainable to third parties -- even for higher-order interaction
#' terms.
#'
#' @param path Character path to an `.rds` file containing an
#'   `earthUI_result` object, or the object itself.
#' @return A `glmnetUI_earth_import` object (a list) with components:
#'   \describe{
#'     \item{model}{The fitted earth model object.}
#'     \item{predictors}{Character vector of predictor names.}
#'     \item{target}{Character vector of response variable name(s).}
#'     \item{categoricals}{Character vector of categorical variables.}
#'     \item{linpreds}{Character vector of variables forced linear.}
#'     \item{degree}{Integer max interaction degree.}
#'     \item{data}{Data frame used to fit the earth model.}
#'     \item{earth_summary}{List with r_squared, gcv, n_terms.}
#'   }
#' @export
#' @examples
#' if (requireNamespace("earth", quietly = TRUE)) {
#'   m <- earth::earth(mpg ~ wt + hp + disp, data = mtcars)
#'   er <- structure(
#'     list(model = m, target = "mpg",
#'          predictors = c("wt", "hp", "disp"),
#'          categoricals = character(0), linpreds = character(0),
#'          degree = 1L, cv_enabled = FALSE, allowed_matrix = NULL,
#'          data = mtcars, elapsed = 0, trace_output = character(0)),
#'     class = "earthUI_result"
#'   )
#'   ei <- import_earth_glmnet(er)
#'   bx <- build_earth_basis(mtcars, ei)
#'   str(bx)
#' }
import_earth_glmnet <- function(path) {
  if (is.character(path)) {
    if (!file.exists(path)) {
      stop("File not found: ", path, call. = FALSE)
    }
    obj <- readRDS(path)
  } else {
    obj <- path
  }

  if (!inherits(obj, "earthUI_result")) {
    stop("Expected an 'earthUI_result' object. ",
         "Save your earthUI result with saveRDS().", call. = FALSE)
  }

  if (!requireNamespace("earth", quietly = TRUE)) {
    stop("Package 'earth' is required to import earthUI results. ",
         "Install with: install.packages('earth')", call. = FALSE)
  }

  model <- obj$model

  earth_summ <- list(
    r_squared = model$rsq,
    gcv       = model$gcv,
    n_terms   = length(model$selected.terms)
  )

  structure(
    list(
      model          = model,
      predictors     = obj$predictors,
      target         = obj$target,
      categoricals   = obj$categoricals %||% character(0),
      linpreds       = obj$linpreds %||% character(0),
      degree         = obj$degree %||% 1L,
      data           = obj$data,
      earth_summary  = earth_summ
    ),
    class = "glmnetUI_earth_import"
  )
}


#' Build Earth Basis Matrix for glmnet
#'
#' Uses `earth::model.matrix()` to generate the basis function columns
#' from the earth model. This is the standard approach: earth defines
#' the basis (hinges, interactions, knots), glmnet does the
#' regularization.
#'
#' @param data Data frame with predictor columns.
#' @param earth_import A `glmnetUI_earth_import` object from
#'   [import_earth_glmnet()], or `NULL`.
#' @return A matrix of earth basis columns (intercept removed), or
#'   `NULL` if `earth_import` is `NULL`.
#' @export
build_earth_basis <- function(data, earth_import) {
  if (is.null(earth_import)) return(NULL)

  # Accept both old class name and new for backward compat
  if (!inherits(earth_import, "glmnetUI_earth_import") &&
      !inherits(earth_import, "glmnetUI_earth_knots")) {
    stop("Expected a glmnetUI_earth_import object.", call. = FALSE)
  }

  if (!requireNamespace("earth", quietly = TRUE)) {
    stop("Package 'earth' is required.", call. = FALSE)
  }

  model <- earth_import$model
  if (is.null(model)) {
    # Old glmnetUI_earth_knots object without model -- cannot proceed
    warning("Earth import object has no model. Re-import the .rds file.",
            call. = FALSE)
    return(NULL)
  }

  # model.matrix dispatches to earth's method for earth objects.
  # When data is NULL, use the training basis from the earth model.
  if (is.null(data)) {
    bx <- stats::model.matrix(model)
  } else {
    # Align column types with earth's expectations.
    # earth::model.matrix silently drops factors that aren't factors
    # in the new data, so we must match types exactly.
    xlevels <- model$xlevels
    if (!is.null(xlevels)) {
      for (cn in names(xlevels)) {
        if (cn %in% names(data)) {
          data[[cn]] <- factor(data[[cn]], levels = xlevels[[cn]])
        }
      }
    }
    # Also match types from earth's training data
    train_data <- earth_import$data
    if (!is.null(train_data)) {
      for (cn in intersect(names(data), names(train_data))) {
        if (is.factor(train_data[[cn]]) && !is.factor(data[[cn]])) {
          data[[cn]] <- factor(data[[cn]],
                               levels = levels(train_data[[cn]]))
        }
      }
    }
    bx <- tryCatch(
      stats::model.matrix(model, x = data),
      error = function(e) {
        stop("earth model.matrix failed: ", e$message,
             "\nEnsure the data has the same columns as the earth ",
             "training data.", call. = FALSE)
      }
    )
  }

  # Remove intercept column (glmnet adds its own)
  if (ncol(bx) > 1 && colnames(bx)[1] == "(Intercept)") {
    bx <- bx[, -1, drop = FALSE]
  }

  if (ncol(bx) == 0L) return(NULL)
  bx
}


#' Export Knot Positions to CSV
#'
#' Extracts knot positions from the earth model and writes a tidy CSV.
#'
#' @param earth_import A `glmnetUI_earth_import` object from
#'   [import_earth_glmnet()].
#' @param file Character path for the output CSV file.
#' @return The file path (invisibly).
#' @export
export_knots_csv_glmnet <- function(earth_import, file) {
  if (!inherits(earth_import, "glmnetUI_earth_import") &&
      !inherits(earth_import, "glmnetUI_earth_knots")) {
    stop("Expected a glmnetUI_earth_import object.", call. = FALSE)
  }

  model <- earth_import$model
  if (is.null(model)) {
    # Fallback for old objects that have knots list
    if (!is.null(earth_import$knots)) {
      return(export_knots_csv_legacy_(earth_import, file))
    }
    stop("No earth model available.", call. = FALSE)
  }

  sel   <- model$selected.terms
  dirs  <- model$dirs[sel, , drop = FALSE]
  cuts  <- model$cuts[sel, , drop = FALSE]
  col_names <- colnames(dirs)

  rows <- list()
  for (ti in seq_len(nrow(dirs))) {
    for (ci in seq_len(ncol(dirs))) {
      d <- dirs[ti, ci]
      if (d != 0 && d != 2) {
        rows[[length(rows) + 1L]] <- data.frame(
          variable  = col_names[ci],
          knot      = cuts[ti, ci],
          direction = if (d == 1) "forward" else "reverse",
          stringsAsFactors = FALSE
        )
      }
    }
  }

  if (length(rows) == 0L) {
    out <- data.frame(variable = character(0), knot = numeric(0),
                      direction = character(0), stringsAsFactors = FALSE)
  } else {
    out <- do.call(rbind, rows)
    out <- out[!duplicated(out), ]
  }
  rownames(out) <- NULL
  utils::write.csv(out, file, row.names = FALSE)
  invisible(file)
}


#' Legacy CSV export for old earth_knots objects
#' @noRd
export_knots_csv_legacy_ <- function(earth_knots, file) {
  rows <- list()
  for (var in names(earth_knots$knots)) {
    k <- earth_knots$knots[[var]]
    s <- earth_knots$signs[[var]]
    dir_label <- ifelse(s == 1L, "forward", "reverse")
    rows[[var]] <- data.frame(
      variable  = var, knot = k, direction = dir_label,
      stringsAsFactors = FALSE
    )
  }
  if (length(rows) == 0L) {
    out <- data.frame(variable = character(0), knot = numeric(0),
                      direction = character(0), stringsAsFactors = FALSE)
  } else {
    out <- do.call(rbind, rows)
  }
  rownames(out) <- NULL
  utils::write.csv(out, file, row.names = FALSE)
  invisible(file)
}
