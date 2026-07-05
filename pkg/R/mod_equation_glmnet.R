#' Model Equation Module UI
#'
#' @param id Module namespace ID.
#' @return A Shiny tagList.
#' @export
equationUI <- function(id) {
  ns <- shiny::NS(id)
  shiny::tagList(
    shiny::tags$div(
      style = "overflow-x: auto; padding: 10px 10px 10px 0;",
      shiny::uiOutput(ns("model_equation"))
    )
  )
}

#' Model Equation Module Server
#'
#' @param id Module namespace ID.
#' @param model_module Reactive list from [modelingServer()].
#' @param data_module Reactive list from [dataImportServer()].
#' @return A reactive list containing the equation LaTeX strings.
#' @export
equationServer <- function(id, model_module, data_module) {
  shiny::moduleServer(id, function(input, output, session) {

    equation <- shiny::reactive({
      shiny::req(model_module$fitted())
      model_module$fit_count()

      model  <- model_module$model()
      lambda <- model_module$lambda()
      gamma  <- model_module$gamma()
      resp   <- data_module$response()
      ei     <- model_module$earth_import()

      if (!is.null(ei)) {
        format_glmnet_earth_equation_(model, lambda, gamma, resp, ei)
      } else {
        format_glmnet_equation_(model, lambda, gamma, resp)
      }
    })

    output$model_equation <- shiny::renderUI({
      shiny::req(equation())
      eq <- equation()
      legend <- shiny::HTML(g_function_legend_glmnet("html", eq$notation %||% "linear"))
      shiny::withMathJax(shiny::tagList(shiny::HTML(eq$latex_inline), legend))
    })

    return(list(equation = equation))
  })
}

#' Legend explaining the model-equation notation
#'
#' Returns a short, self-contained explanation of the notation used in the
#' Model Equation, matched to how the model was fit, together with a citation to
#' the RCA protocol. Used both by the Shiny app's Equation tab and at the bottom
#' of the Quarto report.
#'
#' Two notations occur:
#' * `"g"` -- an elastic-net model fit on `earth` basis functions, displayed in
#'   the RCA value-contribution notation \eqn{{}^{f}g^{j}_{k}}.
#' * `"linear"` -- a plain elastic-net (glmnet) fit, displayed as ordinary linear
#'   \eqn{\beta x} terms.
#'
#' @param format Either `"html"` (MathJax markup, for the Shiny app and the
#'   HTML report) or `"markdown"` (Pandoc markdown with `$`-delimited math, for
#'   the Quarto report rendering to HTML, PDF and Word).
#' @param notation Either `"g"` or `"linear"` (see above).
#'
#' @return A length-one character string.
#'
#' @export
#' @examples
#' cat(g_function_legend_glmnet("markdown", "linear"))
g_function_legend_glmnet <- function(format = c("html", "markdown"),
                              notation = c("g", "linear")) {
  format   <- match.arg(format)
  notation <- match.arg(notation)
  doi_url  <- "https://doi.org/10.5281/zenodo.14787917"

  if (format == "markdown") {
    ref <- paste0(
      "Craytor, W. B. (2025). *Residual Constraint Approach (RCA): ",
      "Framework & Protocol*, \u00a710. Zenodo. ",
      "[doi:10.5281/zenodo.14787917](", doi_url, ").")
    if (notation == "g") {
      paste0(
        "Each line of the equation above is a **value-contribution function** ",
        "written ${}^{f}g^{\\,j}_{k}$:\n\n",
        "- $j$ \u2014 the **group**, i.e. the number of variables the term ",
        "involves: $j=0$ base constant, $j=1$ one variable, $j=2$ ",
        "two-variable interaction, $j=3$ three-variable interaction.\n",
        "- $k$ \u2014 the **position** of the function within group $j$ (its ",
        "$k$-th term).\n",
        "- $f$ (the leading superscript) \u2014 the number of **categorical ",
        "(factor)** variables in the term.\n\n",
        "A term needs $d = j - f$ axes to graph, because each factor variable ",
        "removes one dimension. The estimate is the base constant plus every ",
        "$g$ function:\n\n",
        "$$\\hat{y}={}^{f_{0,1}}g_1^{0}",
        "+\\sum_{q=1}^{n_1}{}^{f_{1,q}}g_q^{1}(x)",
        "+\\sum_{r=1}^{n_2}{}^{f_{2,r}}g_r^{2}(x,y)",
        "+\\sum_{s=1}^{n_3}{}^{f_{3,s}}g_s^{3}(x,y,z)$$\n\n",
        "Notation and protocol follow ", ref, "\n")
    } else {
      paste0(
        "Each line of the equation above is a **value contribution**. A plain ",
        "elastic-net (glmnet) fit is linear, so the estimate is the intercept ",
        "$\\beta_0$ plus a coefficient $\\beta_p$ times each retained ",
        "predictor $x_p$:\n\n",
        "$$\\hat{y}=\\beta_0+\\sum_{p}\\beta_p\\,x_p$$\n\n",
        "Each product $\\beta_p\\,x_p$ is the value contribution of predictor ",
        "$p$. Predictors whose coefficients were shrunk to zero are omitted, ",
        "and interactions appear as products ($x_1\\times x_2$).\n\n",
        "The value-contribution framework follows ", ref, "\n")
    }
  } else {
    ref <- paste0(
      "Craytor, W. B. (2025). <em>Residual Constraint Approach (RCA): ",
      "Framework &amp; Protocol</em>, \u00a710. Zenodo. ",
      "<a href=\"", doi_url, "\">doi:10.5281/zenodo.14787917</a>.")
    head_ <- paste0(
      "<div class=\"gui-gfn-legend\" style=\"margin-top:14px;padding:10px ",
      "14px;border-left:3px solid var(--bs-border-color,#ccc);",
      "font-size:0.92em;\">")
    if (notation == "g") {
      paste0(
        head_,
        "<p style=\"margin:0 0 6px;\">Each line above is a ",
        "<strong>value-contribution function</strong> written ",
        "\\({}^{f}g^{\\,j}_{k}\\):</p>",
        "<ul style=\"margin:0 0 6px;padding-left:1.2em;\">",
        "<li>\\(j\\) \u2014 the <strong>group</strong>, i.e. the number of ",
        "variables the term involves (\\(j=0\\) base constant, \\(j=1\\) one ",
        "variable, \\(j=2\\) two-variable interaction, \\(j=3\\) ",
        "three-variable interaction).</li>",
        "<li>\\(k\\) \u2014 the <strong>position</strong> of the function ",
        "within group \\(j\\).</li>",
        "<li>\\(f\\) (the leading superscript) \u2014 the number of ",
        "<strong>categorical (factor)</strong> variables in the term.</li>",
        "</ul>",
        "<p style=\"margin:0 0 6px;\">A term needs \\(d = j - f\\) axes to ",
        "graph, because each factor variable removes one dimension. The ",
        "estimate is the base constant plus every \\(g\\) function:</p>",
        "\\[\\hat{y}={}^{f_{0,1}}g_1^{0}",
        "+\\sum_{q=1}^{n_1}{}^{f_{1,q}}g_q^{1}(x)",
        "+\\sum_{r=1}^{n_2}{}^{f_{2,r}}g_r^{2}(x,y)",
        "+\\sum_{s=1}^{n_3}{}^{f_{3,s}}g_s^{3}(x,y,z)\\]",
        "<p style=\"margin:6px 0 0;font-style:italic;\">Notation and protocol ",
        "follow ", ref, "</p></div>")
    } else {
      paste0(
        head_,
        "<p style=\"margin:0 0 6px;\">Each line above is a ",
        "<strong>value contribution</strong>. A plain elastic-net (glmnet) ",
        "fit is linear, so the estimate is the intercept \\(\\beta_0\\) plus a ",
        "coefficient \\(\\beta_p\\) times each retained predictor ",
        "\\(x_p\\):</p>",
        "\\[\\hat{y}=\\beta_0+\\sum_{p}\\beta_p\\,x_p\\]",
        "<p style=\"margin:0 0 6px;\">Each product \\(\\beta_p\\,x_p\\) is the ",
        "value contribution of predictor \\(p\\). Predictors whose ",
        "coefficients were shrunk to zero are omitted, and interactions appear ",
        "as products (\\(x_1\\times x_2\\)).</p>",
        "<p style=\"margin:6px 0 0;font-style:italic;\">The ",
        "value-contribution framework follows ", ref, "</p></div>")
    }
  }
}

#' Format glmnet Model as LaTeX Equation
#'
#' @param model A glmnet or cv.glmnet model object.
#' @param lambda Selected lambda value.
#' @param gamma Selected gamma value (for relaxed models), or NULL.
#' @param response Name of the response variable.
#' @return A list with `latex` and `latex_inline` strings.
#' @noRd
format_glmnet_equation_ <- function(model, lambda, gamma, response) {
  # Extract coefficients
  coef_args <- list(model, s = lambda)
  if (!is.null(gamma)) coef_args$gamma <- gamma
  coef_sparse <- do.call(stats::coef, coef_args)
  coef_vec <- as.numeric(coef_sparse)
  coef_names <- rownames(coef_sparse)

  intercept <- coef_vec[1L]
  beta <- coef_vec[-1L]
  names(beta) <- coef_names[-1L]

  # Keep only non-zero coefficients
  nonzero <- beta[beta != 0]

  # In MathJax \text{}, underscores are safe as-is.
  # PDF reports handle escaping separately.
  esc <- function(x) x

  # Build LaTeX lines
  lines <- character(0)

  # First line: response = intercept
  resp_tex <- esc(response)
  lines[1L] <- sprintf("\\text{%s} \\;=\\; & %s",
                        resp_tex, format_coef_(intercept))

  # Subsequent lines: + beta * x

  if (length(nonzero) > 0) {
    for (i in seq_along(nonzero)) {
      b <- nonzero[i]
      nm <- names(nonzero)[i]

      sign_str <- if (b >= 0) "+" else "-"
      abs_b <- abs(b)

      # Format the variable term
      # IMPORTANT: earth basis columns use h() naming with *, (, ).
      # See memory/project_earth_integration.md.
      if (grepl("h(", nm, fixed = TRUE)) {
        # Earth basis term: format as math hinge notation
        var_tex <- format_earth_term_latex_(nm, esc)
      } else if (grepl(":", nm, fixed = TRUE)) {
        # Interaction term: x1:x2 -> \text{x1} \times \text{x2}
        parts <- strsplit(nm, ":", fixed = TRUE)[[1L]]
        var_tex <- paste(sprintf("\\text{%s}", vapply(parts, esc, character(1))),
                         collapse = " \\times ")
      } else {
        var_tex <- sprintf("\\text{%s}", esc(nm))
      }

      lines[length(lines) + 1L] <- sprintf(
        "& %s \\; %s \\cdot %s",
        sign_str, format_coef_(abs_b), var_tex
      )
    }
  }

  # Assemble array
  body <- paste(lines, collapse = " \\\\\n")
  latex <- sprintf("\\begin{array}{rl}\n%s\n\\end{array}", body)
  latex_inline <- paste0("$$\n", latex, "\n$$")

  # PDF version: escape underscores inside \text{} blocks
  latex_pdf <- gsub_text_underscores_(latex)

  list(
    latex = latex,
    latex_inline = latex_inline,
    latex_pdf = latex_pdf,
    notation = "linear"
  )
}

#' Escape underscores inside \\text{} blocks for LaTeX PDF output.
#' MathJax handles bare underscores in \\text{} fine, but native
#' LaTeX treats them as subscript operators.
#' @noRd
gsub_text_underscores_ <- function(x) {
  pattern <- "\\\\text\\{[^}]*\\}"
  m <- gregexpr(pattern, x, perl = TRUE)
  regmatches(x, m) <- lapply(regmatches(x, m), function(matches) {
    vapply(matches, function(txt) {
      inner <- sub("^\\\\text\\{(.*)\\}$", "\\1", txt)
      inner <- gsub("_", "\\_", inner, fixed = TRUE)
      paste0("\\text{", inner, "}")
    }, character(1))
  })
  x
}

#' Format Coefficient Value for LaTeX
#' @param x Numeric coefficient value.
#' @return Character string formatted for LaTeX.
#' @noRd
format_coef_ <- function(x) {
  if (abs(x) >= 1000) {
    formatC(x, format = "f", digits = 2, big.mark = ",")
  } else if (abs(x) >= 1) {
    formatC(x, format = "f", digits = 4)
  } else {
    formatC(x, format = "f", digits = 6)
  }
}

#' Format a single earth hinge component as LaTeX
#' e.g. "h(var-knot)" -> "h(\\text{var} - knot)"
#'      "h(knot-var)" -> "h(knot - \\text{var})"
#'      "var"         -> "\\text{var}" (linear entry)
#' @noRd
format_hinge_latex_ <- function(component, esc) {
  if (!grepl("^h\\(", component)) {
    # Linear entry (no hinge)
    return(sprintf("\\text{%s}", esc(component)))
  }
  inner <- sub("^h\\(", "", sub("\\)$", "", component))
  # Handle negative knots: h(-121.45-var) has inner "-121.45-var"
  # Use regex to find "variable - knot" or "knot - variable" pattern
  # Try matching known pattern: negative number followed by variable
  if (grepl("^-[0-9]", inner)) {
    # Starts with negative number: reverse hinge h(negative_knot - var)
    # Find where the variable name starts after the knot
    m <- regexpr("-[a-zA-Z]", inner)
    if (m > 0) {
      knot <- substring(inner, 1, m - 1)
      var_name <- substring(inner, m + 1)
      return(sprintf("h(%s - \\text{%s})", knot, esc(var_name)))
    }
  }
  parts <- strsplit(inner, "-", fixed = TRUE)[[1]]
  if (length(parts) >= 2) {
    first <- parts[1]
    rest <- paste(parts[-1], collapse = "-")
    if (nzchar(first) && suppressWarnings(!is.na(as.numeric(first)))) {
      # Reverse hinge: h(knot - var)
      sprintf("h(%s - \\text{%s})", first, esc(rest))
    } else if (nzchar(first)) {
      # Forward hinge: h(var - knot)
      sprintf("h(\\text{%s} - %s)", esc(first), rest)
    } else {
      # Empty first part (negative knot fallback)
      sprintf("h(%s)", esc(inner))
    }
  } else {
    sprintf("h(%s)", esc(inner))
  }
}

#' Format an earth basis term (possibly product) as LaTeX
#' e.g. "h(var1-k1)*h(var2-k2)" -> "h(...) \\times h(...)"
#' @noRd
format_earth_term_latex_ <- function(nm, esc) {
  components <- strsplit(nm, "*", fixed = TRUE)[[1]]
  parts_tex <- vapply(components, format_hinge_latex_, character(1), esc = esc)
  paste(parts_tex, collapse = " \\times ")
}

#' Format glmnet equation with earth g-function grouping
#'
#' Groups earth basis terms by variable/interaction and displays
#' each group as a g-function with glmnet's coefficients.
#' @noRd
format_glmnet_earth_equation_ <- function(model, lambda, gamma,
                                           response, earth_import) {
  # Extract glmnet coefficients

  coef_args <- list(model, s = lambda)
  if (!is.null(gamma)) coef_args$gamma <- gamma
  coef_sparse <- do.call(stats::coef, coef_args)
  coef_vec <- as.numeric(coef_sparse)
  coef_names <- rownames(coef_sparse)

  intercept <- coef_vec[1L]
  beta <- coef_vec[-1L]
  names(beta) <- coef_names[-1L]

  # Build g-function groups from earth model
  groups <- build_g_groups_(earth_import)

  # Map earth basis column names to glmnet coefficient names
  bx <- build_earth_basis(NULL, earth_import)
  bx_colnames <- if (!is.null(bx)) colnames(bx) else character(0)

  # Build a mapping: earth selected term index -> glmnet coefficient
  earth_model <- earth_import$model
  sel <- earth_model$selected.terms

  # Basis columns correspond to selected terms 2..n (intercept is term 1)
  # Map each basis column index to its selected term index
  glmnet_coef_for_term <- function(term_index) {
    # term_index is position in selected.terms (1-based, 1 = intercept)
    basis_idx <- term_index - 1L
    if (basis_idx < 1L || basis_idx > length(bx_colnames)) return(0)
    col_name <- bx_colnames[basis_idx]
    if (col_name %in% names(beta)) beta[col_name] else 0
  }

  # Format component as LaTeX (reusing earthUI's approach)
  fmt_comp_ <- function(comp) {
    var_tex <- comp$base_var
    if (comp$is_factor) {
      sprintf("I\\{\\text{%s} = \\text{%s}\\}", var_tex, comp$level)
    } else if (comp$dir == 2) {
      sprintf("\\text{%s}", var_tex)
    } else if (comp$dir == 1) {
      sprintf("\\max(0,\\, \\text{%s} - %s)",
              var_tex, format_coef_(comp$cut))
    } else {
      sprintf("\\max(0,\\, %s - \\text{%s})",
              format_coef_(comp$cut), var_tex)
    }
  }

  # Assign g-function indices by degree
  degree_counters <- integer(0)
  for (g_idx in seq_along(groups)) {
    j <- groups[[g_idx]]$degree
    j_key <- as.character(j)
    if (is.na(degree_counters[j_key])) {
      degree_counters[j_key] <- 1L
    } else {
      degree_counters[j_key] <- degree_counters[j_key] + 1L
    }
    groups[[g_idx]]$g_j <- j
    groups[[g_idx]]$g_k <- as.integer(degree_counters[j_key])
    groups[[g_idx]]$g_f <- groups[[g_idx]]$n_factors %||% 0L
  }

  # Build line data for each group
  lines <- character(0)
  active_g_labels <- character(0)

  for (grp in groups) {
    if (grp$degree == 0L) next

    # Collect terms with non-zero glmnet coefficients
    term_lines <- character(0)
    for (t_idx in seq_along(grp$terms)) {
      term <- grp$terms[[t_idx]]
      glm_coef <- glmnet_coef_for_term(term$index)
      if (glm_coef == 0) next

      # Build component product
      comp_strs <- vapply(term$components, fmt_comp_, character(1))
      product <- paste(comp_strs, collapse = " \\cdot ")

      is_first <- (length(term_lines) == 0L)
      if (is_first) {
        term_lines <- c(term_lines,
          sprintf("%s \\cdot %s", format_coef_(glm_coef), product))
      } else {
        sign_str <- if (glm_coef >= 0) "+" else "-"
        term_lines <- c(term_lines,
          sprintf("%s \\; %s \\cdot %s",
                  sign_str, format_coef_(abs(glm_coef)), product))
      }
    }

    if (length(term_lines) == 0L) next

    g_tex <- sprintf("{}^{%d}g^{%d}_{%d}",
                     grp$g_f, grp$g_j, grp$g_k)

    # Format label
    if (grp$degree > 1L) {
      var_parts <- vapply(grp$base_vars, function(v) {
        sprintf("\\text{%s}", v)
      }, character(1))
      label_latex <- paste0("\\{", paste(var_parts, collapse = ",\\, "), "\\}")
    } else {
      label_latex <- sprintf("\\text{%s}", grp$label)
    }

    active_g_labels <- c(active_g_labels, g_tex)

    # First term line gets the label and g = ...
    lines <- c(lines,
      sprintf("  %s & %s \\;=\\; & %s",
              label_latex, g_tex, term_lines[1L]))
    # Continuation lines
    if (length(term_lines) > 1L) {
      for (tl in term_lines[-1L]) {
        lines <- c(lines,
          sprintf("  & & \\quad %s", tl))
      }
    }
  }

  # Build top-level equation: response = intercept + g1 + g2 + ...
  # Wrap across multiple lines (max ~4 g-labels per line)
  resp_tex <- sprintf("\\text{%s}", response)
  top_lines <- character(0)
  top_lines[1L] <- sprintf("  %s & \\;=\\; & %s",
                             resp_tex, format_coef_(intercept))
  per_line <- 4L
  for (i in seq_along(active_g_labels)) {
    if ((i - 1L) %% per_line == 0L) {
      top_lines <- c(top_lines,
        sprintf("  & & + %s", active_g_labels[i]))
    } else {
      top_lines[length(top_lines)] <- paste0(
        top_lines[length(top_lines)], " + ", active_g_labels[i])
    }
  }

  # Combine: top-level equation, blank separator, then g-function definitions
  all_lines <- c(top_lines, "  & & \\\\", lines)
  body <- paste(all_lines, collapse = " \\\\\n")
  latex <- sprintf("\\small\n\\begin{array}{lrl}\n%s\n\\end{array}", body)
  latex_inline <- paste0("$$\n", latex, "\n$$")

  latex_pdf <- gsub_text_underscores_(latex)

  list(
    latex = latex,
    latex_inline = latex_inline,
    latex_pdf = latex_pdf,
    notation = "g"
  )
}
