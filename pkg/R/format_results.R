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
  coefs[[earth_result$target]] <- round(coefs[[earth_result$target]], 6)

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
    coefficient = round(as.numeric(coefs), 6),
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

#' Format earth model as LaTeX equation
#'
#' Converts a fitted earth model into a LaTeX-formatted mathematical
#' representation using g-function notation. Basis functions are grouped by
#' degree (constant, first-degree, second-degree, third-degree) and labeled
#' with indices that encode the group, position, and factor variable count.
#'
#' @param earth_result An object of class `"earthui_result"` as returned by
#'   [fit_earth()].
#' @param digits Integer. Number of significant digits for coefficients and
#'   cut points. Default is 7.
#'
#' @return A list containing:
#'   \describe{
#'     \item{latex}{Character string. LaTeX array environment.}
#'     \item{latex_inline}{Character string. Wrapped in display math
#'       delimiters for MathJax/HTML rendering.}
#'     \item{groups}{List of group structures for programmatic access.}
#'   }
#'
#' @export
#' @examples
#' result <- fit_earth(mtcars, "mpg", c("cyl", "disp", "hp", "wt"))
#' eq <- format_model_equation(result)
#' cat(eq$latex)
format_model_equation <- function(earth_result, digits = 7L) {
  validate_earthui_result(earth_result)
  model <- earth_result$model

  # Extract matrices for selected terms
  dirs  <- model$dirs[model$selected.terms, , drop = FALSE]
  cuts  <- model$cuts[model$selected.terms, , drop = FALSE]
  coefs <- as.numeric(stats::coef(model))
  col_names <- colnames(dirs)

  # Resolve dummy columns to base variables
  col_info <- resolve_columns_(col_names, earth_result$categoricals,
                               earth_result$data)

  # Build per-term metadata
  n_terms <- nrow(dirs)
  term_list <- vector("list", n_terms)

  for (i in seq_len(n_terms)) {
    active_cols <- which(dirs[i, ] != 0)

    if (length(active_cols) == 0L) {
      # Intercept
      term_list[[i]] <- list(
        index       = i,
        coefficient = coefs[i],
        components  = list(),
        base_vars   = character(0),
        var_set_key = "(Intercept)",
        degree      = 0L,
        n_factors   = 0L
      )
      next
    }

    components <- lapply(active_cols, function(j) {
      list(
        col_name  = col_names[j],
        base_var  = col_info$base_var[j],
        level     = col_info$level[j],
        is_factor = col_info$is_factor[j],
        dir       = dirs[i, j],
        cut       = cuts[i, j]
      )
    })

    base_vars <- sort(unique(vapply(components, `[[`, character(1), "base_var")))
    n_factors <- sum(!duplicated(vapply(components, function(c) {
      if (c$is_factor) c$base_var else ""
    }, character(1))) & vapply(components, `[[`, logical(1), "is_factor"))

    term_list[[i]] <- list(
      index       = i,
      coefficient = coefs[i],
      components  = components,
      base_vars   = base_vars,
      var_set_key = paste(base_vars, collapse = "_"),
      degree      = length(base_vars),
      n_factors   = n_factors
    )
  }

  # Group terms by variable set
  keys <- vapply(term_list, `[[`, character(1), "var_set_key")
  unique_keys <- unique(keys)

  groups <- lapply(unique_keys, function(k) {
    members <- term_list[keys == k]
    list(
      var_set_key = k,
      degree      = members[[1]]$degree,
      n_factors   = members[[1]]$n_factors,
      base_vars   = members[[1]]$base_vars,
      terms       = members
    )
  })

  # Sort by degree then by first appearance
  group_degrees <- vapply(groups, `[[`, integer(1), "degree")
  groups <- groups[order(group_degrees)]

  # Assign labels (space-separated for interactions, matching RCA Figure 1)
  for (g_idx in seq_along(groups)) {
    j <- groups[[g_idx]]$degree
    if (j == 0L) {
      groups[[g_idx]]$label <- "Basis"
    } else {
      groups[[g_idx]]$label <- paste(groups[[g_idx]]$base_vars, collapse = " ")
    }
  }

  # Assign g-function indices: {}^{f}g^{j}_{k}
  # j = degree, k = sequential position within degree, f = number of factors
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
    groups[[g_idx]]$g_f <- groups[[g_idx]]$n_factors
  }

  # Build LaTeX with g-function notation (for MathJax + Quarto/PDF)
  latex_lines <- character(0)
  for (g_idx in seq_along(groups)) {
    grp <- groups[[g_idx]]
    g_tex <- sprintf("{}^{%d}g^{%d}_{%d}", grp$g_f, grp$g_j, grp$g_k)

    for (t_idx in seq_along(grp$terms)) {
      term <- grp$terms[[t_idx]]
      is_first <- (t_idx == 1L)
      term_str <- format_term_latex_(term, is_first, digits)
      label_tex <- latex_escape_text_(grp$label)
      if (is_first) {
        line <- sprintf("  \\text{%s} & %s \\;=\\; %s",
                        label_tex, g_tex, term_str)
      } else {
        line <- sprintf("  & \\qquad %s", term_str)
      }
      latex_lines <- c(latex_lines, paste0(line, " \\\\[4pt]"))
    }
  }
  if (length(latex_lines) > 0L) {
    latex_lines[length(latex_lines)] <- sub(" \\\\\\\\\\[4pt\\]$", "",
                                            latex_lines[length(latex_lines)])
  }
  latex <- paste0(
    "\\small\n\\begin{array}{l@{\\;=\\;\\,}l}\n",
    paste(latex_lines, collapse = "\n"),
    "\n\\end{array}"
  )

  result <- list(
    latex        = latex,
    latex_inline = paste0("$$\n", latex, "\n$$"),
    groups       = groups
  )
  class(result) <- "earthui_equation"
  result
}

# --- Internal helpers for format_model_equation (not exported) ---

#' Map dummy column names to base variables and factor levels
#' @keywords internal
#' @noRd
resolve_columns_ <- function(col_names, categoricals, data) {
  info <- data.frame(
    col_name  = col_names,
    base_var  = col_names,
    level     = NA_character_,
    is_factor = FALSE,
    stringsAsFactors = FALSE
  )

  if (length(categoricals) == 0L) return(info)

  # Sort categoricals by name length descending to match longer names first
  categoricals <- categoricals[order(-nchar(categoricals))]

  for (cat_var in categoricals) {
    if (!cat_var %in% names(data)) next
    factor_col <- data[[cat_var]]
    lvls <- if (is.factor(factor_col)) levels(factor_col) else
      sort(unique(as.character(factor_col)))

    for (lvl in lvls) {
      for (sep in c("", ".")) {
        dummy_name <- paste0(cat_var, sep, lvl)
        idx <- which(info$col_name == dummy_name & !info$is_factor)
        if (length(idx) == 1L) {
          info$base_var[idx]  <- cat_var
          info$level[idx]     <- as.character(lvl)
          info$is_factor[idx] <- TRUE
        }
      }
    }
  }
  info
}

#' Escape special characters for LaTeX text mode
#' @keywords internal
#' @noRd
latex_escape_text_ <- function(x) {
  x <- gsub("%", "\\%", x, fixed = TRUE)
  x <- gsub("&", "\\&", x, fixed = TRUE)
  x <- gsub("#", "\\#", x, fixed = TRUE)
  x
}

#' Format a number for LaTeX display
#' @keywords internal
#' @noRd
format_number_ <- function(x, digits = 7L) {
  if (x == 0) return("0")
  trimws(formatC(x, format = "g", digits = digits))
}

#' Format one hinge/indicator/linear component as LaTeX
#' @keywords internal
#' @noRd
format_component_latex_ <- function(comp) {
  var_tex <- latex_escape_text_(comp$base_var)

  if (comp$is_factor) {
    sprintf("I\\{\\text{%s} = %s\\}", var_tex, comp$level)
  } else if (comp$dir == 2) {
    # Linear predictor (no hinge)
    sprintf("\\text{%s}", var_tex)
  } else if (comp$dir == 1) {
    sprintf("\\max(0, \\text{%s} - %s)", var_tex, format_number_(comp$cut))
  } else {
    # dir == -1
    sprintf("\\max(0, %s - \\text{%s})", format_number_(comp$cut), var_tex)
  }
}

#' Format a complete term (coefficient * components) as LaTeX
#' @keywords internal
#' @noRd
format_term_latex_ <- function(term_info, is_first, digits) {
  coef <- term_info$coefficient

  # Intercept
  if (term_info$degree == 0L) {
    return(format_number_(coef, digits))
  }

  # Build component strings
  comp_strs <- vapply(term_info$components, format_component_latex_, character(1))
  product <- paste(comp_strs, collapse = " \\cdot ")

  if (is_first) {
    coef_str <- format_number_(coef, digits)
    paste0(coef_str, " \\cdot ", product)
  } else {
    abs_coef_str <- format_number_(abs(coef), digits)
    if (coef >= 0) {
      paste0("+", abs_coef_str, " \\cdot ", product)
    } else {
      paste0("-", abs_coef_str, " \\cdot ", product)
    }
  }
}

#' Escape HTML special characters
#' @keywords internal
#' @noRd
html_escape_ <- function(x) {
  x <- gsub("&", "&amp;", x, fixed = TRUE)
  x <- gsub("<", "&lt;", x, fixed = TRUE)
  x <- gsub(">", "&gt;", x, fixed = TRUE)
  x
}

#' Format one hinge/indicator/linear component as plain text (HTML)
#' @keywords internal
#' @noRd
format_component_html_ <- function(comp) {
  var <- comp$base_var

  if (comp$is_factor) {
    sprintf("I{%s = %s}", var, comp$level)
  } else if (comp$dir == 2) {
    var
  } else if (comp$dir == 1) {
    sprintf("max(0, %s - %s)", var, format_number_(comp$cut))
  } else {
    sprintf("max(0, %s - %s)", format_number_(comp$cut), var)
  }
}

#' Format a complete term as plain text (HTML)
#' @keywords internal
#' @noRd
format_term_html_ <- function(term_info, is_first, digits) {
  coef <- term_info$coefficient

  if (term_info$degree == 0L) {
    return(html_escape_(format_number_(coef, digits)))
  }

  comp_strs <- vapply(term_info$components, format_component_html_, character(1))
  product <- paste(comp_strs, collapse = " * ")

  if (is_first) {
    txt <- paste0(format_number_(coef, digits), " * ", product)
  } else {
    abs_str <- format_number_(abs(coef), digits)
    sign <- if (coef >= 0) "+" else "-"
    txt <- paste0(sign, " ", abs_str, " * ", product)
  }
  html_escape_(txt)
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
