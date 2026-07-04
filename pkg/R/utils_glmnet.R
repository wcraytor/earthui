#' Check Coefficient Sign Warnings
#'
#' Compares fitted coefficient signs against user-specified expected signs.
#' Returns a character vector of sign warning flags.
#'
#' @param coef_names Character vector of coefficient names (excluding intercept).
#' @param coef_values Numeric vector of coefficient values corresponding to
#'   \code{coef_names}.
#' @param expected_signs Named character vector where names are variable names
#'   and values are one of \code{"positive"}, \code{"negative"}, or
#'   \code{"either"}.
#'
#' @return A character vector the same length as \code{coef_names}. Each element
#'   is either \code{"OK"} or a descriptive warning string.
#'
#' @export
#' @examples
#' check_sign_warnings(
#'   coef_names = c("sqft", "age", "rooms"),
#'   coef_values = c(50, -2, -10),
#'   expected_signs = c(sqft = "positive", age = "negative", rooms = "positive")
#' )
check_sign_warnings <- function(coef_names, coef_values, expected_signs) {
  if (length(coef_names) != length(coef_values)) {
    stop("coef_names and coef_values must have the same length", call. = FALSE)
  }

  vapply(seq_along(coef_names), function(i) {
    nm <- coef_names[i]
    val <- coef_values[i]

    if (val == 0) return("OK")

    expected <- expected_signs[nm]
    if (is.null(expected) || is.na(expected) || expected == "either") {
      return("OK")
    }

    actual_sign <- if (val > 0) "positive" else "negative"
    if (actual_sign != expected) {
      paste0("Expected ", expected, ", got ", actual_sign)
    } else {
      "OK"
    }
  }, FUN.VALUE = character(1))
}

#' Build Coefficient Table from glmnet Model
#'
#' Extracts coefficients from a fitted glmnet or cv.glmnet model at a
#' given lambda value and returns a tidy data frame.
#'
#' @param model A fitted \code{glmnet} or \code{cv.glmnet} object.
#' @param lambda Numeric lambda value at which to extract coefficients.
#'   If \code{NULL} and model is a cv.glmnet object, uses \code{lambda.1se}.
#' @param gamma Numeric gamma value for relaxed fits (0 = fully relaxed,
#'   1 = regular glmnet). Only used for relaxed glmnet models. Default
#'   \code{NULL} (ignored for non-relaxed fits).
#'
#' @return A data frame with columns \code{variable} and \code{coefficient}.
#'
#' @export
#' @examples
#' x <- matrix(rnorm(100 * 5), 100, 5,
#'             dimnames = list(NULL, paste0("V", 1:5)))
#' y <- rnorm(100)
#' fit <- glmnet::cv.glmnet(x, y)
#' build_coef_table(fit)
build_coef_table <- function(model, lambda = NULL, gamma = NULL) {
  if (is.null(lambda)) {
    if (inherits(model, "cv.glmnet") || inherits(model, "cv.relaxed")) {
      lambda <- model$lambda.1se
    } else {
      stop("lambda must be specified for non-cv.glmnet models", call. = FALSE)
    }
  }

  coef_args <- list(model, s = lambda)
  if (!is.null(gamma)) coef_args$gamma <- gamma
  coefs <- do.call(stats::coef, coef_args)
  coef_df <- data.frame(
    variable = rownames(coefs),
    coefficient = as.numeric(coefs),
    stringsAsFactors = FALSE
  )
  coef_df[coef_df$coefficient != 0, , drop = FALSE]
}

#' Font Family Helper (internal)
#'
#' Returns \code{"Roboto Condensed"} if available via \pkg{sysfonts},
#' otherwise \code{"sans"}.
#'
#' @return A character string with the font family name.
#' @keywords internal
glmnet_font_family_ <- function() {
  if (requireNamespace("sysfonts", quietly = TRUE) &&
      "Roboto Condensed" %in% sysfonts::font_families()) {
    "Roboto Condensed"
  } else {
    "sans"
  }
}

#' Plot Dimension Helper (internal)
#'
#' Returns \code{width} and \code{height} functions that read the plot
#' container size from \code{session$clientData}. Used with
#' \code{renderPlot(width = d$width, height = d$height, res = 96)} to
#' normalise rendering across platforms (macOS, Windows, Linux).
#'
#' @param session The Shiny session object.
#' @param id Character output ID (un-namespaced; namespacing is handled
#'   automatically for modules).
#' @return A list with \code{width} and \code{height} functions.
#' @keywords internal
plot_dims_ <- function(session, id) {
  full_id <- if (!is.null(session$ns)) session$ns(id) else id
  w_key <- paste0("output_", full_id, "_width")
  h_key <- paste0("output_", full_id, "_height")
  list(
    width  = function() session$clientData[[w_key]],
    height = function() session$clientData[[h_key]]
  )
}

#' Null-coalescing operator (internal)
#'
#' Returns \code{a} unless it is \code{NULL}, in which case \code{b}. Defined
#' package-locally so the regProj helpers do not depend on \pkg{shiny} being
#' attached to the search path.
#'
#' @param a,b Values; \code{a} is returned unless \code{NULL}.
#' @return \code{a} if non-NULL, else \code{b}.
#' @noRd
`%||%` <- function(a, b) if (is.null(a)) b else a
