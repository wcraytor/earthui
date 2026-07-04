#' Import an Earth Model and Extract Knot Structure
#'
#' Reads a saved `earthUI_result` object (`.rds` file) produced by the
#' \pkg{earthUI} package and extracts knot positions, hinge directions,
#' and variable metadata for use in GAM construction.
#'
#' @param path Character path to an `.rds` file containing an
#'   `earthUI_result` object, or the object itself.
#' @return An `mgcvUI_earth_knots` object (a list) with components:
#'   \describe{
#'     \item{knots}{Named list of numeric vectors -- knot positions per
#'       predictor variable.}
#'     \item{signs}{Named list of integer vectors -- hinge direction at
#'       each knot (`1` = forward / increasing, `-1` = reverse /
#'       decreasing).}
#'     \item{interactions}{Named list of detected interaction pairs
#'       (variable pairs with 2+ active terms in the earth model).}
#'     \item{predictors}{Character vector of predictor names used in the
#'       earth model.}
#'     \item{target}{Character vector of response variable name(s).}
#'     \item{categoricals}{Character vector of categorical variable names.}
#'     \item{linpreds}{Character vector of variables forced linear in earth.}
#'     \item{degree}{Integer max interaction degree from earth (1 = no
#'       interactions).}
#'     \item{allowed_matrix}{Matrix of allowed interactions from earthUI
#'       (or \code{NULL}).}
#'     \item{data}{Data frame used to fit the earth model.}
#'     \item{earth_summary}{List of summary statistics from the earth
#'       model (R-squared, GCV, number of terms).}
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
#'   knots <- import_earth_mgcv(er)
#'   str(knots$knots)
#' }
import_earth_mgcv <- function(path) {
  if (is.character(path)) {
    if (!file.exists(path)) {
      stop("File not found: ", path, call. = FALSE)
    }
    check_memory_for_file_(path, multiplier = 5)
    obj <- readRDS(path)
  } else {
    obj <- path
  }

  if (!inherits(obj, "earthUI_result")) {
    stop("Expected an 'earthUI_result' object. ",
         "Save your earthUI result with saveRDS().", call. = FALSE)
  }

  extract_earth_knots_(obj)
}


#' Extract knots from an earthUI_result
#' @param earth_result An earthUI_result object.
#' @return An mgcvUI_earth_knots object.
#' @noRd
extract_earth_knots_ <- function(earth_result) {
  model <- earth_result$model
  sel   <- model$selected.terms
  dirs  <- model$dirs[sel, , drop = FALSE]
  cuts  <- model$cuts[sel, , drop = FALSE]
  col_names <- colnames(dirs)

  knots_list <- list()
  signs_list <- list()

  for (var in earth_result$predictors) {
    col_idx <- which(col_names == var)
    if (length(col_idx) == 0L) next

    var_knots <- numeric(0)
    var_signs <- integer(0)

    for (ci in col_idx) {
      for (ti in seq_len(nrow(dirs))) {
        d <- dirs[ti, ci]
        if (d != 0 && d != 2) {
          var_knots <- c(var_knots, cuts[ti, ci])
          var_signs <- c(var_signs, as.integer(d))
        }
      }
    }

    if (length(var_knots) > 0L) {
      ord <- order(var_knots)
      uknots <- unique(var_knots[ord])
      # Take the sign of the first occurrence at each unique knot
      usigns <- var_signs[ord][!duplicated(var_knots[ord])]
      knots_list[[var]] <- uknots
      signs_list[[var]] <- usigns
    }
  }

  # Detect interactions (rows where 2+ predictors are active)
  interactions <- list()
  for (ti in seq_len(nrow(dirs))) {
    nonzero <- which(dirs[ti, ] != 0)
    if (length(nonzero) >= 2L) {
      vars_in_int <- sort(col_names[nonzero])
      key <- paste(vars_in_int, collapse = ":")
      if (!key %in% names(interactions)) {
        interactions[[key]] <- vars_in_int
      }
    }
  }

  earth_summ <- list(
    r_squared = model$rsq,
    gcv       = model$gcv,
    n_terms   = length(sel)
  )

  structure(
    list(
      knots          = knots_list,
      signs          = signs_list,
      interactions   = interactions,
      predictors     = earth_result$predictors,
      target         = earth_result$target,
      categoricals   = earth_result$categoricals %||% character(0),
      linpreds       = earth_result$linpreds %||% character(0),
      degree         = earth_result$degree %||% 1L,
      allowed_matrix = earth_result$allowed_matrix,
      data           = earth_result$data,
      earth_summary  = earth_summ
    ),
    class = "mgcvUI_earth_knots"
  )
}


#' Export Knot Positions to CSV
#'
#' Writes a tidy CSV with one row per knot, columns: variable, knot,
#' direction.
#'
#' @param earth_knots An `mgcvUI_earth_knots` object from
#'   [import_earth_mgcv()].
#' @param file Character path for the output CSV file.
#' @return The file path (invisibly).
#' @export
#' @examples
#' if (requireNamespace("earth", quietly = TRUE)) {
#'   m <- earth::earth(mpg ~ wt + hp, data = mtcars)
#'   er <- structure(
#'     list(model = m, target = "mpg",
#'          predictors = c("wt", "hp"),
#'          categoricals = character(0), linpreds = character(0),
#'          degree = 1L, cv_enabled = FALSE, allowed_matrix = NULL,
#'          data = mtcars, elapsed = 0, trace_output = character(0)),
#'     class = "earthUI_result"
#'   )
#'   knots <- import_earth_mgcv(er)
#'   tmp <- tempfile(fileext = ".csv")
#'   export_knots_csv_mgcv(knots, tmp)
#'   cat(readLines(tmp), sep = "\n")
#'   unlink(tmp)
#' }
export_knots_csv_mgcv <- function(earth_knots, file) {
  if (!inherits(earth_knots, "mgcvUI_earth_knots")) {
    stop("Expected an mgcvUI_earth_knots object.", call. = FALSE)
  }

  rows <- list()
  for (var in names(earth_knots$knots)) {
    k <- earth_knots$knots[[var]]
    s <- earth_knots$signs[[var]]
    dir_label <- ifelse(s == 1L, "forward", "reverse")
    rows[[var]] <- data.frame(
      variable  = var,
      knot      = k,
      direction = dir_label,
      stringsAsFactors = FALSE
    )
  }

  if (length(rows) == 0L) {
    out <- data.frame(
      variable = character(0),
      knot = numeric(0),
      direction = character(0),
      stringsAsFactors = FALSE
    )
  } else {
    out <- do.call(rbind, rows)
  }
  rownames(out) <- NULL
  utils::write.csv(out, file, row.names = FALSE)
  invisible(file)
}
