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
#' @param block_degree1 Optional character vector of predictor names to
#'   block from entering the model as degree-1 (main effect) terms. These
#'   variables can still participate in interactions (degree >= 2). This is
#'   useful when a variable like `sale_age` should only enter through an
#'   interaction (e.g. with `living_area`) to capture size-varying time
#'   adjustments.
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
#' When `block_degree1` is specified, any predictor in that list is blocked
#' from entering as a degree-1 term but is allowed in higher-degree
#' interactions (subject to the allowed matrix).
#'
#' @export
#' @examples
#' mat <- build_allowed_matrix(c("sqft", "bedrooms", "pool"))
#' mat["sqft", "pool"] <- FALSE
#' mat["pool", "sqft"] <- FALSE
#' func <- build_allowed_function(mat)
#'
#' # Block sale_age from degree 1 (interaction only)
#' mat2 <- build_allowed_matrix(c("sale_age", "living_area", "lot_size"))
#' func2 <- build_allowed_function(mat2, block_degree1 = "sale_age")
build_allowed_function <- function(allowed_matrix, block_degree1 = NULL) {
  if (!is.matrix(allowed_matrix) || !is.logical(allowed_matrix)) {
    stop("`allowed_matrix` must be a logical matrix.", call. = FALSE)
  }
  if (nrow(allowed_matrix) != ncol(allowed_matrix)) {
    stop("`allowed_matrix` must be square.", call. = FALSE)
  }
  if (is.null(rownames(allowed_matrix)) || is.null(colnames(allowed_matrix))) {
    stop("`allowed_matrix` must have row and column names.", call. = FALSE)
  }

  # Capture the matrix and block list in the closure
  mat <- allowed_matrix
  mat_names <- rownames(mat)
  blk1 <- if (!is.null(block_degree1)) as.character(block_degree1) else character(0)

  # Map expanded factor names (e.g. "conditionGood") back to original
  # predictor names used in the matrix
  resolve_name_ <- function(nm) {
    if (nm %in% mat_names) return(nm)
    # Try prefix matching: earth expands factor "x" to "xLevel1", "xLevel2"
    for (mn in mat_names) {
      if (startsWith(nm, mn)) return(mn)
    }
    nm
  }

  function(degree, pred, parents, namesx, first) {
    # Block degree-1 terms for specified variables
    if (degree == 1L && length(blk1) > 0L) {
      pred_name <- resolve_name_(namesx[pred])
      if (pred_name %in% blk1) return(FALSE)
    }
    if (degree < 2L) return(TRUE)

    # pred is the index (1-based) of the candidate predictor
    # parents is an integer vector (length = ncol(x)); non-zero entries
    # indicate which predictors are already in the interaction term
    pred_name <- resolve_name_(namesx[pred])
    parent_names <- vapply(namesx[parents != 0], resolve_name_, character(1))

    # All predictors involved in this term
    involved <- unique(c(pred_name, parent_names))

    # Check all pairwise combinations are allowed
    for (i in seq_along(involved)) {
      for (j in seq_len(i - 1L)) {
        a <- involved[i]
        b <- involved[j]
        # Variables not in the matrix are allowed by default
        if (a %in% mat_names && b %in% mat_names) {
          if (!isTRUE(mat[a, b])) return(FALSE)
        }
      }
    }
    TRUE
  }
}
