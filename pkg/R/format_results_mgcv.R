#' Format GAM Summary Statistics
#'
#' Extracts key statistics from a fitted GAM for display in the UI.
#'
#' @param gam_result An `mgcvUI_result` from [fit_gam()].
#' @return A list with components: `r_squared`, `dev_explained`,
#'   `aic`, `bic`, `n_obs`, `n_smooths`, `method`, `family`,
#'   `smooth_table`, `parametric_table`.
#' @export
#' @examples
#' specs <- list(list(vars = "wt", type = "s", bs = "tp", k = NULL))
#' res <- fit_gam(mtcars, "mpg", specs)
#' format_gam_summary(res)
format_gam_summary <- function(gam_result) {
  model <- gam_result$model
  sm <- summary(model)

  # Smooth terms table
  if (!is.null(sm$s.table) && nrow(sm$s.table) > 0L) {
    smooth_tbl <- as.data.frame(sm$s.table, check.names = FALSE)
    smooth_tbl$Term <- rownames(sm$s.table)
    # Handle both Gaussian ("F") and non-Gaussian ("Chi.sq") families
    stat_col <- if ("F" %in% names(smooth_tbl)) "F" else "Chi.sq"
    smooth_tbl <- data.frame(
      Term   = smooth_tbl$Term,
      EDF    = smooth_tbl[["edf"]],
      Ref.df = smooth_tbl[["Ref.df"]],
      F      = smooth_tbl[[stat_col]],
      p_value = smooth_tbl[["p-value"]],
      stringsAsFactors = FALSE
    )
  } else {
    smooth_tbl <- data.frame(
      Term = character(0), EDF = numeric(0), Ref.df = numeric(0),
      F = numeric(0), p_value = numeric(0)
    )
  }

  # Parametric terms table
  if (!is.null(sm$p.table) && nrow(sm$p.table) > 0L) {
    param_tbl <- as.data.frame(sm$p.table, check.names = FALSE)
    param_tbl$Term <- rownames(sm$p.table)
    # Handle both Gaussian ("t value") and non-Gaussian ("z value")
    stat_col <- if ("t value" %in% names(param_tbl)) "t value" else "z value"
    p_col <- if ("Pr(>|t|)" %in% names(param_tbl)) "Pr(>|t|)" else "Pr(>|z|)"
    param_tbl <- data.frame(
      Term      = param_tbl$Term,
      Estimate  = param_tbl[["Estimate"]],
      Std_Error = param_tbl[["Std. Error"]],
      t_value   = param_tbl[[stat_col]],
      p_value   = param_tbl[[p_col]],
      stringsAsFactors = FALSE
    )
  } else {
    param_tbl <- data.frame(
      Term = character(0), Estimate = numeric(0), Std_Error = numeric(0),
      t_value = numeric(0), p_value = numeric(0)
    )
  }

  list(
    r_squared     = sm$r.sq,
    cv_rsq        = gam_result$cv_rsq,
    dev_explained = sm$dev.expl,
    aic           = if (is.finite(AIC(model))) AIC(model) else model$aic,
    bic           = if (is.finite(BIC(model))) BIC(model) else NA_real_,
    n_obs         = nrow(model$model),
    n_smooths     = length(model$smooth),
    method        = model$method,
    family        = model$family$family,
    smooth_table  = smooth_tbl,
    parametric_table = param_tbl
  )
}


#' Tidy GAM Results with broom
#'
#' Returns a tidy data frame of model terms using [broom::tidy()].
#'
#' @param gam_result An `mgcvUI_result` from [fit_gam()].
#' @return A data frame (tibble) of tidied model terms.
#' @export
#' @examples
#' specs <- list(list(vars = "wt", type = "s", bs = "tp", k = NULL))
#' res <- fit_gam(mtcars, "mpg", specs)
#' tidy_gam(res)
tidy_gam <- function(gam_result) {
  broom::tidy(gam_result$model)
}


#' Glance at GAM Model Fit
#'
#' Returns a one-row data frame of model-level statistics using
#' [broom::glance()].
#'
#' @param gam_result An `mgcvUI_result` from [fit_gam()].
#' @return A one-row data frame (tibble).
#' @export
#' @examples
#' specs <- list(list(vars = "wt", type = "s", bs = "tp", k = NULL))
#' res <- fit_gam(mtcars, "mpg", specs)
#' glance_gam(res)
glance_gam <- function(gam_result) {
  broom::glance(gam_result$model)
}


#' Build the model-equation LaTeX (shared by the Equation tab and the report)
#'
#' Produces the same `f_i(...)` notation shown on the Equation tab, in two
#' forms: a single-line `inline` body, and a line-broken `aligned` body (one
#' term per line) that fits within a PDF text width. Also returns the smooth
#' function definitions. Neither body includes the surrounding `$$`.
#'
#' @param model A fitted `mgcv::gam` model.
#' @param response Character response-variable name.
#' @param response_transform LHS transform: "none", "log", or "log10".
#' @return `list(inline, aligned, defs)`, where `defs` is a list of
#'   `list(fn, label, desc, vars)`.
#' @noRd
format_gam_equation_ <- function(model, response, response_transform = "none") {
  fam <- model$family
  link_name <- fam$link
  # Escape LaTeX specials inside \text{} -- a raw '_' (e.g. sale_price) renders
  # fine in MathJax (the tab) but breaks real LaTeX/PDF ("Missing $ inserted").
  esc_ <- function(x) gsub("([_&%#$])", "\\\\\\1", x)
  resp_tex <- esc_(response)
  xform <- if (is.null(response_transform)) "none" else response_transform

  if (xform == "log10") {
    lhs <- paste0("\\log_{10}\\bigl(\\widehat{\\text{", resp_tex, "}}\\bigr)")
  } else if (xform == "log") {
    lhs <- paste0("\\ln\\bigl(\\widehat{\\text{", resp_tex, "}}\\bigr)")
  } else if (link_name == "identity") {
    lhs <- paste0("\\widehat{\\text{", resp_tex, "}}")
  } else if (link_name == "log") {
    lhs <- paste0("\\log\\bigl(\\widehat{\\text{", resp_tex, "}}\\bigr)")
  } else if (link_name == "inverse") {
    lhs <- paste0("\\frac{1}{\\widehat{\\text{", resp_tex, "}}}")
  } else {
    lhs <- paste0("\\text{", link_name, "}\\bigl(\\widehat{\\text{",
                  resp_tex, "}}\\bigr)")
  }

  # Each term: list(op = "+"/"-"/"" , tex). First (intercept) has op = "".
  terms <- list(list(op = "", tex = as.character(round(
    stats::coef(model)[["(Intercept)"]], 4))))

  defs <- list()
  for (idx in seq_along(model$smooth)) {
    sm <- model$smooth[[idx]]
    fn_label <- paste0("f_{", idx, "}")
    terms[[length(terms) + 1L]] <- list(
      op  = "+",
      tex = paste0(fn_label, "(\\text{",
                   esc_(paste(sm$term, collapse = ", ")), "})"))
    sm_class <- class(sm)[1L]
    bs_type <- if (grepl("tp", sm_class)) "thin plate regression spline"
      else if (grepl("cr", sm_class)) "cubic regression spline"
      else if (grepl("ps", sm_class)) "P-spline"
      else if (grepl("tensor", sm_class) || grepl("t2", sm_class))
        "tensor product smooth"
      else sm_class
    defs[[length(defs) + 1L]] <- list(
      fn    = paste0("f_", idx),
      label = sm$label,
      desc  = paste0(bs_type, ", k = ", sm$bs.dim),
      vars  = paste(sm$term, collapse = ", "))
  }

  sm_obj <- summary(model)
  if (!is.null(sm_obj$p.table) && nrow(sm_obj$p.table) > 1L) {
    param_names <- rownames(sm_obj$p.table)
    param_names <- param_names[param_names != "(Intercept)"]
    all_coefs <- stats::coef(model)
    mf <- stats::model.frame(model)
    factor_vars <- names(mf)[vapply(mf, is.factor, logical(1))]
    factors_shown <- character(0)
    for (pn in param_names) {
      matched_factor <- NULL
      for (fv in factor_vars) {
        if (startsWith(pn, fv)) { matched_factor <- fv; break }
      }
      if (!is.null(matched_factor)) {
        if (matched_factor %in% factors_shown) next
        factors_shown <- c(factors_shown, matched_factor)
        n_levels <- nlevels(mf[[matched_factor]])
        terms[[length(terms) + 1L]] <- list(op = "+", tex = paste0(
          "\\beta_{\\text{", esc_(matched_factor), "}} \\text{ (", n_levels,
          " levels)}"))
      } else {
        coef_val <- all_coefs[[pn]]
        if (coef_val >= 0) {
          terms[[length(terms) + 1L]] <- list(op = "+", tex = paste0(
            round(coef_val, 4), " \\cdot \\text{", esc_(pn), "}"))
        } else {
          terms[[length(terms) + 1L]] <- list(op = "-", tex = paste0(
            round(abs(coef_val), 4), " \\cdot \\text{", esc_(pn), "}"))
        }
      }
    }
  }

  inline <- paste0(lhs, " = ", terms[[1L]]$tex)
  for (k in seq_along(terms)[-1L])
    inline <- paste0(inline, " ", terms[[k]]$op, " ", terms[[k]]$tex)

  aligned <- paste0("\\begin{aligned}\n", lhs, " ={}& ", terms[[1L]]$tex)
  for (k in seq_along(terms)[-1L])
    aligned <- paste0(aligned, " \\\\\n  & ", terms[[k]]$op, " ", terms[[k]]$tex)
  aligned <- paste0(aligned, "\n\\end{aligned}")

  list(inline = inline, aligned = aligned, defs = defs)
}

#' Legend explaining the model-equation notation
#'
#' Returns a short, self-contained explanation of the notation used in the
#' Model Equation produced by `format_gam_equation_()` -- the smooth functions
#' \eqn{f_i} and parametric coefficients \eqn{\beta} as value-contribution
#' functions -- together with a citation to the RCA protocol. Used both by the
#' Shiny app's Equation tab and at the bottom of the Quarto report.
#'
#' @param format Either `"html"` (MathJax markup, for the Shiny app and the
#'   HTML report) or `"markdown"` (Pandoc markdown with `$`-delimited math, for
#'   the Quarto report rendering to HTML, PDF and Word).
#'
#' @return A length-one character string.
#'
#' @export
#' @examples
#' cat(g_function_legend_mgcv("markdown"))
g_function_legend_mgcv <- function(format = c("html", "markdown")) {
  format  <- match.arg(format)
  doi_url <- "https://doi.org/10.5281/zenodo.14787917"

  if (format == "markdown") {
    ref <- paste0(
      "Craytor, W. B. (2025). *Residual Constraint Approach (RCA): ",
      "Framework & Protocol*, \u00a710. Zenodo. ",
      "[doi:10.5281/zenodo.14787917](", doi_url, ").")
    paste0(
      "Each term of the equation above is a **value-contribution function**. ",
      "Smooth (non-linear) terms are written $f_i(\\cdot)$, where $i$ indexes ",
      "the smooth and the variables in parentheses are its inputs; the smooth ",
      "function definitions listed with the equation give each one's spline ",
      "type and basis dimension $k$. Parametric and categorical predictors ",
      "enter as linear coefficients $\\beta$. The fitted response is the ",
      "intercept plus the sum of all smooth and parametric terms \u2014 shown on ",
      "the model's link scale when a non-identity link or a response transform ",
      "is used:\n\n",
      "$$g(\\hat{y}) = \\beta_0 + \\sum_i f_i(\\cdot) + \\sum_p \\beta_p\\,x_p$$\n\n",
      "The value-contribution framework follows ", ref, "\n")
  } else {
    ref <- paste0(
      "Craytor, W. B. (2025). <em>Residual Constraint Approach (RCA): ",
      "Framework &amp; Protocol</em>, \u00a710. Zenodo. ",
      "<a href=\"", doi_url, "\">doi:10.5281/zenodo.14787917</a>.")
    paste0(
      "<div class=\"mui-gfn-legend\" style=\"margin-top:14px;padding:10px ",
      "14px;border-left:3px solid var(--bs-border-color,#ccc);",
      "font-size:0.92em;\">",
      "<p style=\"margin:0 0 6px;\">Each term above is a ",
      "<strong>value-contribution function</strong>. Smooth (non-linear) ",
      "terms are written \\(f_i(\\cdot)\\), where \\(i\\) indexes the smooth ",
      "and the variables in parentheses are its inputs; the smooth function ",
      "definitions listed with the equation give each one's spline type and ",
      "basis dimension \\(k\\). Parametric and categorical predictors enter as ",
      "linear coefficients \\(\\beta\\). The fitted response is the intercept ",
      "plus the sum of all smooth and parametric terms \u2014 shown on the ",
      "model's link scale when a non-identity link or a response transform is ",
      "used:</p>",
      "\\[g(\\hat{y}) = \\beta_0 + \\sum_i f_i(\\cdot) + \\sum_p \\beta_p\\,x_p\\]",
      "<p style=\"margin:6px 0 0;font-style:italic;\">The value-contribution ",
      "framework follows ", ref, "</p></div>")
  }
}
