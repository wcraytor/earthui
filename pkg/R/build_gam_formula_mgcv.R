#' Build a GAM Formula from Variable Specifications
#'
#' Constructs a [stats::formula] suitable for [mgcv::gam()] from a list
#' of smooth-term specifications.
#'
#' @param response Character scalar -- the response variable name.
#' @param smooth_specs A list of smooth-term specification lists. Each
#'   element should have:
#'   \describe{
#'     \item{vars}{Character vector of variable names (length 1 for
#'       univariate smooths, 2+ for tensor products).}
#'     \item{type}{`"s"`, `"te"`, `"ti"`, or `"linear"`.}
#'     \item{bs}{Basis type: `"tp"`, `"cr"`, `"ps"`, `"bs"`, etc.
#'       Ignored for `"linear"` and tensor types.}
#'     \item{k}{Integer basis dimension, or `NULL` for automatic.}
#'   }
#' @return A [stats::formula] object.
#' @export
#' @examples
#' specs <- list(
#'   list(vars = "wt", type = "s", bs = "cr", k = 5L),
#'   list(vars = "hp", type = "s", bs = "tp", k = NULL),
#'   list(vars = "cyl", type = "linear", bs = NULL, k = NULL)
#' )
#' build_gam_formula("mpg", specs)
build_gam_formula <- function(response, smooth_specs) {
  if (length(smooth_specs) == 0L) {
    stop("At least one predictor term is required.", call. = FALSE)
  }

  terms <- vapply(smooth_specs, format_smooth_term_, character(1))
  rhs <- paste(terms, collapse = " + ")
  as.formula(paste(response, "~", rhs))
}


#' Build the knots list for gam() from earth knot data
#'
#' When earth knots are available for variables using `"cr"` or `"ps"`
#' basis types, this function prepares the `knots` argument for
#' [mgcv::gam()]. If `k` exceeds the number of earth knots (because
#' the GAM should be at least as flexible as standalone), additional
#' knots are placed at data quantiles, preserving the earth-derived
#' positions as anchor points.
#'
#' @param smooth_specs List of smooth-term specs (same as
#'   [build_gam_formula()]).
#' @param earth_knots An `mgcvUI_earth_knots` object, or `NULL`.
#' @param data Data frame used for fitting. Needed to compute
#'   quantile-based filler knots when `k > length(earth_knots)`.
#'   If `NULL`, terms needing augmentation are skipped.
#' @return A named list suitable for the `knots` argument of
#'   [mgcv::gam()], or `NULL` if no knots apply.
#' @export
#' @examples
#' specs <- list(
#'   list(vars = "wt", type = "s", bs = "cr", k = 5L)
#' )
#' build_gam_knots(specs, NULL)
build_gam_knots <- function(smooth_specs, earth_knots, data = NULL) {
  if (is.null(earth_knots)) return(NULL)

  knots_out <- list()
  for (spec in smooth_specs) {
    if (spec$type == "linear") next
    if (length(spec$vars) != 1L) next
    # Only pass knots for basis types that use them
    if (!spec$bs %in% c("cr", "ps", "bs")) next
    var <- spec$vars
    ek <- earth_knots$knots[[var]]
    if (!is.null(ek) && !is.null(spec$k)) {
      n_knots <- length(ek)
      if (identical(spec$bs, "cr")) {
        # cr basis: k MUST equal length(knots)
        if (n_knots > spec$k) {
          # Too many earth knots — subsample to k
          idx <- round(seq(1, n_knots, length.out = spec$k))
          ek <- ek[idx]
        } else if (n_knots < spec$k) {
          # Fewer earth knots than k — augment with data quantiles
          ek <- augment_knots_(ek, spec$k, var, data)
          if (is.null(ek)) next
        }
      } else {
        # ps/bs: trim if too many
        if (n_knots > spec$k) {
          idx <- round(seq(1, n_knots, length.out = spec$k))
          ek <- ek[idx]
        }
      }
      knots_out[[var]] <- ek
    }
  }

  if (length(knots_out) == 0L) NULL else knots_out
}


#' Augment earth knots with data quantiles to reach target k
#'
#' Preserves earth knot positions as anchors and fills remaining
#' slots with quantiles of the observed data, avoiding near-duplicates.
#'
#' @param earth_k Numeric vector of earth-derived knots.
#' @param target_k Integer target number of knots.
#' @param var Character variable name.
#' @param data Data frame (or NULL).
#' @return Numeric vector of length target_k, or NULL on failure.
#' @noRd
augment_knots_ <- function(earth_k, target_k, var, data) {
  if (is.null(data) || !var %in% names(data)) return(NULL)

  x <- stats::na.omit(data[[var]])
  if (length(x) < target_k) return(NULL)

  x_range <- range(x)
  n_need <- target_k - length(earth_k)

  # Generate candidate knots at even quantiles across the data
  probs <- seq(0, 1, length.out = n_need + 2L)[-c(1L, n_need + 2L)]
  candidates <- unname(stats::quantile(x, probs = probs))

  # Remove candidates too close to existing earth knots
  # (within 1% of the data range)
  tol <- diff(x_range) * 0.01
  for (ek_val in earth_k) {
    candidates <- candidates[abs(candidates - ek_val) > tol]
  }

  # Combine earth knots + candidates, pick exactly target_k
  combined <- sort(unique(c(earth_k, candidates)))
  if (length(combined) >= target_k) {
    # Subsample to exact target_k, always keeping earth knots
    non_earth <- setdiff(combined, earth_k)
    n_fill <- target_k - length(earth_k)
    if (length(non_earth) >= n_fill) {
      idx <- round(seq(1, length(non_earth), length.out = n_fill))
      filler <- non_earth[idx]
    } else {
      filler <- non_earth
    }
    result <- sort(c(earth_k, filler))
    # Final trim/pad to exact k (floating point edge cases)
    if (length(result) > target_k) {
      result <- result[round(seq(1, length(result), length.out = target_k))]
    }
  } else {
    # Not enough unique candidates — fill with evenly spaced values
    n_extra <- target_k - length(combined)
    fillers <- seq(x_range[1L], x_range[2L], length.out = n_extra + 2L)
    fillers <- fillers[-c(1L, length(fillers))]
    combined <- sort(unique(c(combined, fillers)))
    result <- combined[round(seq(1, length(combined), length.out = target_k))]
  }

  if (length(result) != target_k) return(NULL)
  result
}


#' Format a single smooth term for a GAM formula
#' @param spec A smooth-term specification list.
#' @return Character string (e.g. `"s(wt, bs = \"cr\", k = 5)"`).
#' @noRd
format_smooth_term_ <- function(spec) {
  if (spec$type == "linear") {
    return(paste(spec$vars, collapse = " + "))
  }

  vars_str <- paste(spec$vars, collapse = ", ")
  parts <- vars_str

  # Basis type
  if (spec$type == "s" && !is.null(spec$bs)) {
    parts <- c(parts, paste0("bs = \"", spec$bs, "\""))
  } else if (spec$type %in% c("ti", "te") && !is.null(spec$marginal_bs)) {
    # Per-marginal basis specs (e.g. to force tp when earth knots are present)
    bs_vals <- paste0("\"", spec$marginal_bs, "\"", collapse = ", ")
    parts <- c(parts, paste0("bs = c(", bs_vals, ")"))
  }

  # Basis dimension
  if (!is.null(spec$k)) {
    parts <- c(parts, paste0("k = ", spec$k))
  }

  # by-variable (factor interaction)
  if (!is.null(spec$by) && nzchar(spec$by)) {
    parts <- c(parts, paste0("by = ", spec$by))
  }

  paste0(spec$type, "(", paste(parts, collapse = ", "), ")")
}
