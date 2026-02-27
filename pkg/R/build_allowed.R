#' Build an allowed interaction matrix
#'
#' Creates a symmetric logical matrix indicating which pairs of predictors
#' are allowed to interact. By default, all interactions are allowed.
#'
#' @param variable_names Character vector of predictor variable names.
#' @param default Logical. Default value for all entries. Default is `TRUE`
#'   (all interactions allowed).
#'
#' @return A symmetric logical matrix with `variable_names` as both row and
#'   column names.
#'
#' @export
#' @examples
#' mat <- build_allowed_matrix(c("sqft", "bedrooms", "pool"))
#' mat["sqft", "pool"] <- FALSE
#' mat["pool", "sqft"] <- FALSE
#' mat
build_allowed_matrix <- function(variable_names, default = TRUE) {
  if (!is.character(variable_names) || length(variable_names) == 0L) {
    stop("`variable_names` must be a non-empty character vector.", call. = FALSE)
  }
  if (anyDuplicated(variable_names)) {
    stop("`variable_names` must not contain duplicates.", call. = FALSE)
  }

  n <- length(variable_names)
  mat <- matrix(default, nrow = n, ncol = n,
                dimnames = list(variable_names, variable_names))
  mat
}

#' Build an allowed function for earth()
#'
#' Converts an allowed interaction matrix into a function compatible with
#' the `allowed` parameter of [earth::earth()]. The function checks that
#' ALL pairwise combinations among the predictors in a proposed interaction
#' term are TRUE in the matrix.
#'
#' @param allowed_matrix A symmetric logical matrix as returned by
#'   [build_allowed_matrix()].
#'
#' @return A function with signature
#'   `function(degree, pred, parents, namesx, first)` suitable for the
#'   `allowed` parameter of [earth::earth()].
#'
#' @details
#' The returned function implements the standard `earth()` allowed function
#' contract. When earth proposes a new hinge function involving predictor
#' `pred` with existing parent predictors indicated by the `parents` logical
#' vector, the function checks that every pair of involved predictors is
#' allowed in the matrix.
#'
#' For a 3-way interaction between X, Y, Z, the function verifies that
#' (X,Y), (Y,Z), and (X,Z) are all TRUE in the matrix.
#'
#' @export
#' @examples
#' mat <- build_allowed_matrix(c("sqft", "bedrooms", "pool"))
#' mat["sqft", "pool"] <- FALSE
#' mat["pool", "sqft"] <- FALSE
#' func <- build_allowed_function(mat)
build_allowed_function <- function(allowed_matrix) {
  if (!is.matrix(allowed_matrix) || !is.logical(allowed_matrix)) {
    stop("`allowed_matrix` must be a logical matrix.", call. = FALSE)
  }
  if (nrow(allowed_matrix) != ncol(allowed_matrix)) {
    stop("`allowed_matrix` must be square.", call. = FALSE)
  }
  if (is.null(rownames(allowed_matrix)) || is.null(colnames(allowed_matrix))) {
    stop("`allowed_matrix` must have row and column names.", call. = FALSE)
  }

  # Capture the matrix in the closure
  mat <- allowed_matrix

  function(degree, pred, parents, namesx, first) {
    if (degree < 2L) return(TRUE)

    # pred is the index (1-based) of the candidate predictor
    # parents is a logical vector indicating which predictors are already
    # in the interaction term
    pred_name <- namesx[pred]
    parent_names <- namesx[parents]

    # All predictors involved in this term
    involved <- c(pred_name, parent_names)

    # Check all pairwise combinations are allowed
    for (i in seq_along(involved)) {
      for (j in seq_len(i - 1L)) {
        a <- involved[i]
        b <- involved[j]
        # Variables not in the matrix are allowed by default
        if (a %in% rownames(mat) && b %in% colnames(mat)) {
          if (!isTRUE(mat[a, b])) return(FALSE)
        }
      }
    }
    TRUE
  }
}
