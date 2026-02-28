# Internal: format axis labels as $1,234 (dollar with commas, no decimals)
# Negative values: -$200,000 (sign before dollar sign)
dollar_format_ <- function(x) {
  ifelse(is.na(x), "",
         ifelse(x < 0,
                paste0("-$", formatC(abs(round(x)), format = "f",
                                     digits = 0, big.mark = ",")),
                paste0("$", formatC(round(x), format = "f",
                                    digits = 0, big.mark = ","))))
}

# Internal: format axis labels with commas (no dollar sign, no decimals)
comma_format_ <- function(x) {
  ifelse(is.na(x), "",
         formatC(round(x), format = "f", digits = 0, big.mark = ","))
}

#' Plot variable importance
#'
#' Creates a horizontal bar chart of variable importance from a fitted
#' earth model.
#'
#' @param earth_result An object of class `"earthui_result"` as returned by
#'   [fit_earth()].
#' @param type Character. Importance metric to plot: `"nsubsets"` (default),
#'   `"gcv"`, or `"rss"`.
#'
#' @return A [ggplot2::ggplot] object.
#'
#' @export
#' @examples
#' result <- fit_earth(mtcars, "mpg", c("cyl", "disp", "hp", "wt"))
#' plot_variable_importance(result)
plot_variable_importance <- function(earth_result, type = "nsubsets") {
  validate_earthui_result(earth_result)
  type <- match.arg(type, c("nsubsets", "gcv", "rss"))

  imp_df <- format_variable_importance(earth_result)

  if (nrow(imp_df) == 0L) {
    return(
      ggplot2::ggplot() +
        ggplot2::annotate("text", x = 0.5, y = 0.5,
                          label = "No variable importance data available") +
        ggplot2::theme_void()
    )
  }

  if (!type %in% names(imp_df)) {
    type <- "nsubsets"
  }

  imp_df$value <- imp_df[[type]]

  ggplot2::ggplot(imp_df,
                  ggplot2::aes(x = stats::reorder(.data$variable, .data$value),
                               y = .data$value)) +
    ggplot2::geom_bar(stat = "identity", fill = "#2c7bb6") +
    ggplot2::coord_flip() +
    ggplot2::labs(
      title = "Variable Importance",
      x = NULL,
      y = type
    ) +
    ggplot2::theme_minimal() +
    ggplot2::theme(
      plot.title = ggplot2::element_text(face = "bold", size = 14)
    )
}

#' Plot partial dependence
#'
#' Creates a partial dependence plot for a selected variable from a fitted
#' earth model.
#'
#' @param earth_result An object of class `"earthui_result"` as returned by
#'   [fit_earth()].
#' @param variable Character string. Name of the predictor variable to plot.
#' @param n_grid Integer. Number of grid points for the partial dependence
#'   calculation. Default is 50.
#'
#' @return A [ggplot2::ggplot] object.
#'
#' @export
#' @examples
#' result <- fit_earth(mtcars, "mpg", c("cyl", "disp", "hp", "wt"))
#' plot_partial_dependence(result, "wt")
plot_partial_dependence <- function(earth_result, variable, n_grid = 50L) {
  validate_earthui_result(earth_result)

  if (!is.character(variable) || length(variable) != 1L) {
    stop("`variable` must be a single character string.", call. = FALSE)
  }
  if (!variable %in% names(earth_result$data)) {
    stop("Variable '", variable, "' not found in model data.", call. = FALSE)
  }

  model <- earth_result$model
  data <- earth_result$data
  target <- earth_result$target

  var_col <- data[[variable]]

  if (is.factor(var_col) || is.character(var_col)) {
    # Categorical variable: use unique levels
    grid_vals <- sort(unique(var_col))
  } else {
    grid_vals <- seq(min(var_col, na.rm = TRUE),
                     max(var_col, na.rm = TRUE),
                     length.out = n_grid)
  }

  # Compute partial dependence by averaging predictions over all observations
  pd_values <- vapply(grid_vals, function(val) {
    temp_data <- data
    temp_data[[variable]] <- val
    mean(stats::predict(model, newdata = temp_data))
  }, numeric(1))

  pd_df <- data.frame(x = grid_vals, y = pd_values)

  p <- ggplot2::ggplot(pd_df, ggplot2::aes(x = .data$x, y = .data$y))

  if (is.factor(var_col) || is.character(var_col)) {
    p <- p + ggplot2::geom_bar(stat = "identity", fill = "#2c7bb6")
  } else {
    p <- p + ggplot2::geom_line(color = "#2c7bb6", linewidth = 1)
  }

  p +
    ggplot2::labs(
      title = paste("Partial Dependence:", variable),
      x = variable,
      y = paste("Predicted", target)
    ) +
    ggplot2::theme_minimal() +
    ggplot2::theme(
      plot.title = ggplot2::element_text(face = "bold", size = 14)
    )
}

#' Plot variable contribution
#'
#' Creates a scatter plot showing each variable's actual contribution to the
#' prediction. For each observation, the contribution is the sum of
#' coefficient * basis function value across all terms involving that variable.
#'
#' @param earth_result An object of class `"earthui_result"` as returned by
#'   [fit_earth()].
#' @param variable Character string. Name of the predictor variable to plot.
#'
#' @return A [ggplot2::ggplot] object.
#'
#' @export
#' @examples
#' result <- fit_earth(mtcars, "mpg", c("cyl", "disp", "hp", "wt"))
#' plot_contribution(result, "wt")
plot_contribution <- function(earth_result, variable) {
  validate_earthui_result(earth_result)

  if (!is.character(variable) || length(variable) != 1L) {
    stop("`variable` must be a single character string.", call. = FALSE)
  }

  model <- earth_result$model
  data <- earth_result$data

  # Identify which basis-function columns involve this variable
  dirs <- model$dirs[model$selected.terms, , drop = FALSE]
  col_names <- colnames(dirs)

  # For categorical variables, find all dummy columns for this base var
  matching_cols <- col_names == variable
  if (!any(matching_cols) && variable %in% earth_result$categoricals) {
    for (cn in col_names) {
      if (startsWith(cn, variable) && cn != variable) {
        matching_cols[col_names == cn] <- TRUE
      }
    }
  }

  if (!any(matching_cols)) {
    return(
      ggplot2::ggplot() +
        ggplot2::annotate("text", x = 0.5, y = 0.5,
                          label = paste("Variable", variable,
                                        "not used in model")) +
        ggplot2::theme_void()
    )
  }

  # Terms that involve this variable (any matching column has dir != 0)
  var_terms <- which(apply(dirs[, matching_cols, drop = FALSE] != 0, 1, any))

  # Compute per-term contributions: bx * coefficient
  bx <- model$bx
  coefs <- as.numeric(stats::coef(model))
  contributions <- sweep(bx, 2, coefs, "*")

  # Sum contributions for terms involving this variable
  var_contrib <- rowSums(contributions[, var_terms, drop = FALSE])

  var_col <- data[[variable]]

  if (is.factor(var_col) || is.character(var_col)) {
    plot_df <- data.frame(x = as.character(var_col), y = var_contrib,
                          stringsAsFactors = FALSE)
    ggplot2::ggplot(plot_df, ggplot2::aes(x = .data$x, y = .data$y)) +
      ggplot2::geom_boxplot(fill = "#2c7bb6", alpha = 0.5, outlier.shape = NA) +
      ggplot2::geom_jitter(color = "#2c7bb6", alpha = 0.5, width = 0.2) +
      ggplot2::scale_y_continuous(labels = dollar_format_) +
      ggplot2::labs(
        title = paste("Contribution:", variable),
        x = variable,
        y = paste("Contribution to", earth_result$target)
      ) +
      ggplot2::theme_minimal() +
      ggplot2::theme(
        plot.title = ggplot2::element_text(face = "bold", size = 14)
      )
  } else {
    plot_df <- data.frame(x = var_col, y = var_contrib)
    plot_df <- plot_df[order(plot_df$x), ]

    # Compute piecewise-linear slopes from knot points
    # Get knots for this variable from the selected terms
    cuts_mat <- model$cuts[model$selected.terms, , drop = FALSE]
    col_idx <- which(matching_cols)
    knots <- numeric(0)
    for (ti in var_terms) {
      for (ci in col_idx) {
        if (dirs[ti, ci] != 0 && dirs[ti, ci] != 2) {
          knots <- c(knots, cuts_mat[ti, ci])
        }
      }
    }
    knots <- sort(unique(knots))

    # Build evaluation points: data range boundaries + knots
    x_min <- min(var_col, na.rm = TRUE)
    x_max <- max(var_col, na.rm = TRUE)
    knots <- knots[knots > x_min & knots < x_max]
    breaks <- c(x_min, knots, x_max)

    # Evaluate contribution at each break using the model
    eval_contrib <- function(x_val) {
      # Compute basis function values for this x, summing relevant terms
      total <- 0
      for (ti in var_terms) {
        term_val <- coefs[ti]
        for (ci in col_idx) {
          d <- dirs[ti, ci]
          if (d == 0) next
          if (d == 2) {
            term_val <- term_val * x_val
          } else if (d == 1) {
            term_val <- term_val * max(0, x_val - cuts_mat[ti, ci])
          } else {
            term_val <- term_val * max(0, cuts_mat[ti, ci] - x_val)
          }
        }
        total <- total + term_val
      }
      total
    }

    break_y <- vapply(breaks, eval_contrib, numeric(1))

    # Compute slopes and build annotation data
    n_seg <- length(breaks) - 1L
    if (n_seg > 0L) {
      seg_df <- data.frame(
        x_mid  = (breaks[-length(breaks)] + breaks[-1]) / 2,
        y_mid  = (break_y[-length(break_y)] + break_y[-1]) / 2,
        slope  = diff(break_y) / diff(breaks)
      )
      # Format slope labels as $X,XXX/unit
      seg_df$label <- paste0(
        ifelse(seg_df$slope >= 0, "+$", "-$"),
        formatC(abs(seg_df$slope), format = "f", digits = 2, big.mark = ","),
        "/unit"
      )
    } else {
      seg_df <- data.frame(x_mid = numeric(0), y_mid = numeric(0),
                           slope = numeric(0), label = character(0))
    }

    # Build the piecewise-linear line from break points
    line_df <- data.frame(x = breaks, y = break_y)

    p <- ggplot2::ggplot(plot_df, ggplot2::aes(x = .data$x, y = .data$y)) +
      ggplot2::geom_point(alpha = 0.6, color = "#2c7bb6") +
      ggplot2::geom_line(data = line_df, ggplot2::aes(x = .data$x, y = .data$y),
                         color = "#d7191c", linewidth = 0.8)

    # Add slope labels
    if (nrow(seg_df) > 0L) {
      # Offset labels above or below the line
      y_range <- diff(range(plot_df$y, na.rm = TRUE))
      if (y_range == 0) y_range <- 1
      p <- p + ggplot2::geom_label(
        data = seg_df,
        ggplot2::aes(x = .data$x_mid, y = .data$y_mid, label = .data$label),
        size = 3.2, fill = "white", alpha = 0.85,
        label.padding = ggplot2::unit(0.15, "lines"),
        label.size = 0.3, color = "#333333"
      )
    }

    # Add vertical dashed lines at knot points
    if (length(knots) > 0L) {
      p <- p + ggplot2::geom_vline(xintercept = knots, linetype = "dashed",
                                   color = "grey50", alpha = 0.5)
    }

    p + ggplot2::scale_x_continuous(labels = comma_format_) +
      ggplot2::scale_y_continuous(labels = dollar_format_) +
      ggplot2::labs(
        title = paste("Contribution:", variable),
        x = variable,
        y = paste("Contribution to", earth_result$target)
      ) +
      ggplot2::theme_minimal() +
      ggplot2::theme(
        plot.title = ggplot2::element_text(face = "bold", size = 14)
      )
  }
}

#' Plot correlation matrix
#'
#' Creates a heatmap of pairwise correlations among the target variable and
#' numeric predictors, with cells colored by degree of correlation and values
#' printed in each cell.
#'
#' @param earth_result An object of class `"earthui_result"` as returned by
#'   [fit_earth()].
#'
#' @return A [ggplot2::ggplot] object.
#'
#' @export
#' @examples
#' result <- fit_earth(mtcars, "mpg", c("cyl", "disp", "hp", "wt"))
#' plot_correlation_matrix(result)
plot_correlation_matrix <- function(earth_result) {
  validate_earthui_result(earth_result)

  vars <- c(earth_result$target, earth_result$predictors)
  df <- earth_result$data[, vars, drop = FALSE]

  # Keep only numeric columns
  numeric_mask <- vapply(df, is.numeric, logical(1))
  df <- df[, numeric_mask, drop = FALSE]

  if (ncol(df) < 2L) {
    return(
      ggplot2::ggplot() +
        ggplot2::annotate("text", x = 0.5, y = 0.5,
                          label = "Need at least 2 numeric variables for correlation matrix") +
        ggplot2::theme_void()
    )
  }

  cor_mat <- stats::cor(df, use = "pairwise.complete.obs")

  # Reshape to long format
  n <- ncol(cor_mat)
  var_names <- colnames(cor_mat)
  long_df <- data.frame(
    Var1 = rep(var_names, each = n),
    Var2 = rep(var_names, times = n),
    value = as.vector(cor_mat),
    stringsAsFactors = FALSE
  )

  # Preserve variable order
  long_df$Var1 <- factor(long_df$Var1, levels = var_names)
  long_df$Var2 <- factor(long_df$Var2, levels = rev(var_names))

  # Scale text size based on number of variables
  txt_size <- if (n <= 6) 4.5 else if (n <= 10) 3.5 else if (n <= 15) 2.8 else 2.2
  axis_size <- if (n <= 10) 11 else if (n <= 15) 9 else 7

  text_color <- ifelse(abs(long_df$value) > 0.65, "white", "black")

  ggplot2::ggplot(long_df,
                  ggplot2::aes(x = .data$Var1, y = .data$Var2,
                               fill = .data$value)) +
    ggplot2::geom_tile(color = "white", linewidth = 1.2) +
    ggplot2::geom_text(ggplot2::aes(label = sprintf("%.2f", .data$value)),
                       size = txt_size, color = text_color) +
    ggplot2::scale_fill_gradient2(
      low = "#2166AC", mid = "white", high = "#B2182B",
      midpoint = 0, limits = c(-1, 1), name = "Correlation"
    ) +
    ggplot2::labs(
      title = "Correlation Matrix",
      x = NULL, y = NULL
    ) +
    ggplot2::theme_minimal() +
    ggplot2::theme(
      plot.title = ggplot2::element_text(face = "bold", size = 14),
      axis.text.x = ggplot2::element_text(angle = 45, hjust = 1, size = axis_size),
      axis.text.y = ggplot2::element_text(size = axis_size),
      panel.grid = ggplot2::element_blank(),
      legend.position = "right"
    ) +
    ggplot2::coord_fixed(expand = FALSE)
}

#' Plot residual diagnostics
#'
#' Creates a two-panel diagnostic plot: residuals vs fitted values and
#' a Q-Q plot of residuals.
#'
#' @param earth_result An object of class `"earthui_result"` as returned by
#'   [fit_earth()].
#'
#' @return A [ggplot2::ggplot] object showing residuals vs fitted values.
#'   Use [plot_qq()] for the Q-Q plot separately.
#'
#' @export
#' @examples
#' result <- fit_earth(mtcars, "mpg", c("cyl", "disp", "hp", "wt"))
#' plot_residuals(result)
plot_residuals <- function(earth_result) {
  validate_earthui_result(earth_result)
  model <- earth_result$model

  resid_df <- data.frame(
    fitted = as.numeric(stats::fitted(model)),
    residuals = as.numeric(stats::residuals(model))
  )

  ggplot2::ggplot(resid_df,
                  ggplot2::aes(x = .data$fitted, y = .data$residuals)) +
    ggplot2::geom_point(alpha = 0.6, color = "#2c7bb6") +
    ggplot2::geom_hline(yintercept = 0, linetype = "dashed", color = "red") +
    ggplot2::scale_x_continuous(labels = dollar_format_) +
    ggplot2::scale_y_continuous(labels = dollar_format_) +
    ggplot2::labs(
      title = "Residuals vs Fitted",
      x = "Fitted Values",
      y = "Residuals"
    ) +
    ggplot2::theme_minimal() +
    ggplot2::theme(
      plot.title = ggplot2::element_text(face = "bold", size = 14)
    )
}

#' Plot Q-Q plot of residuals
#'
#' Creates a normal Q-Q plot of the model residuals.
#'
#' @param earth_result An object of class `"earthui_result"` as returned by
#'   [fit_earth()].
#'
#' @return A [ggplot2::ggplot] object.
#'
#' @export
#' @examples
#' result <- fit_earth(mtcars, "mpg", c("cyl", "disp", "hp", "wt"))
#' plot_qq(result)
plot_qq <- function(earth_result) {
  validate_earthui_result(earth_result)
  model <- earth_result$model
  resids <- as.numeric(stats::residuals(model))

  qq_data <- data.frame(
    theoretical = stats::qqnorm(resids, plot.it = FALSE)$x,
    sample = sort(resids)
  )

  ggplot2::ggplot(qq_data,
                  ggplot2::aes(x = .data$theoretical, y = .data$sample)) +
    ggplot2::geom_point(alpha = 0.6, color = "#2c7bb6") +
    ggplot2::geom_abline(slope = stats::sd(resids),
                         intercept = mean(resids),
                         linetype = "dashed", color = "red") +
    ggplot2::scale_y_continuous(labels = dollar_format_) +
    ggplot2::labs(
      title = "Normal Q-Q Plot",
      x = "Theoretical Quantiles",
      y = "Sample Quantiles"
    ) +
    ggplot2::theme_minimal() +
    ggplot2::theme(
      plot.title = ggplot2::element_text(face = "bold", size = 14)
    )
}

#' Plot actual vs predicted values
#'
#' Creates a scatter plot of actual vs predicted values with a 1:1 reference
#' line.
#'
#' @param earth_result An object of class `"earthui_result"` as returned by
#'   [fit_earth()].
#'
#' @return A [ggplot2::ggplot] object.
#'
#' @export
#' @examples
#' result <- fit_earth(mtcars, "mpg", c("cyl", "disp", "hp", "wt"))
#' plot_actual_vs_predicted(result)
plot_actual_vs_predicted <- function(earth_result) {
  validate_earthui_result(earth_result)
  model <- earth_result$model
  target <- earth_result$target

  plot_df <- data.frame(
    actual = earth_result$data[[target]],
    predicted = as.numeric(stats::fitted(model))
  )

  ggplot2::ggplot(plot_df,
                  ggplot2::aes(x = .data$actual, y = .data$predicted)) +
    ggplot2::geom_point(alpha = 0.6, color = "#2c7bb6") +
    ggplot2::geom_abline(slope = 1, intercept = 0,
                         linetype = "dashed", color = "red") +
    ggplot2::scale_x_continuous(labels = dollar_format_) +
    ggplot2::scale_y_continuous(labels = dollar_format_) +
    ggplot2::labs(
      title = "Actual vs Predicted",
      x = paste("Actual", target),
      y = paste("Predicted", target)
    ) +
    ggplot2::theme_minimal() +
    ggplot2::theme(
      plot.title = ggplot2::element_text(face = "bold", size = 14)
    )
}

#' List g-function groups from a fitted earth model
#'
#' Returns a data frame describing each non-intercept g-function group from the
#' model equation, including degree, factor count, graph dimensionality, and
#' the number of terms. The g-function notation is
#' \eqn{{}^{f}g^{j}_{k}}{(f)g(j,k)} where f = number of factor variables
#' (top-left), j = degree of interaction (top-right), k = position within the
#' degree group (bottom-right).
#'
#' @param earth_result An object of class `"earthui_result"` as returned by
#'   [fit_earth()].
#'
#' @return A data frame with columns:
#'   \describe{
#'     \item{index}{Integer. Sequential index (1-based).}
#'     \item{label}{Character. Variable names in the group.}
#'     \item{g_j}{Integer. Degree of the g-function (top-right superscript).}
#'     \item{g_k}{Integer. Position within the degree (bottom-right subscript).}
#'     \item{g_f}{Integer. Number of factor variables (top-left superscript).}
#'     \item{d}{Integer. Graph dimensionality (degree minus factor count).}
#'     \item{n_terms}{Integer. Number of terms in the group.}
#'   }
#'
#' @export
#' @examples
#' result <- fit_earth(mtcars, "mpg", c("cyl", "disp", "hp", "wt"))
#' list_g_functions(result)
list_g_functions <- function(earth_result) {
  validate_earthui_result(earth_result)
  eq <- format_model_equation(earth_result)
  groups <- eq$groups
  non_intercept <- Filter(function(g) g$degree > 0L, groups)

  if (length(non_intercept) == 0L) {
    return(data.frame(
      index = integer(0), label = character(0),
      g_j = integer(0), g_k = integer(0), g_f = integer(0),
      d = integer(0), n_terms = integer(0),
      stringsAsFactors = FALSE
    ))
  }

  data.frame(
    index = seq_along(non_intercept),
    label = vapply(non_intercept, `[[`, character(1), "label"),
    g_j = vapply(non_intercept, `[[`, integer(1), "g_j"),
    g_k = vapply(non_intercept, `[[`, integer(1), "g_k"),
    g_f = vapply(non_intercept, `[[`, integer(1), "g_f"),
    d = vapply(non_intercept, function(g) g$degree - g$n_factors, integer(1)),
    n_terms = vapply(non_intercept, function(g) length(g$terms), integer(1)),
    stringsAsFactors = FALSE
  )
}

#' Plot g-function contribution
#'
#' Creates a contribution plot for a specific g-function group. For degree-1
#' groups (single variable), produces a 2D scatter + piecewise-linear plot with
#' slope labels and knot markers. For degree-2 groups (two variables), produces
#' a 3D surface plot using plotly if available, or a filled contour plot.
#'
#' @param earth_result An object of class `"earthui_result"` as returned by
#'   [fit_earth()].
#' @param group_index Integer. Index of the g-function group (1-based, from
#'   [list_g_functions()]).
#'
#' @return A [ggplot2::ggplot] object for d <= 1, or a plotly widget for d >= 2
#'   (when plotly is installed). Falls back to ggplot2 contour if plotly is not
#'   available.
#'
#' @export
#' @examples
#' result <- fit_earth(mtcars, "mpg", c("cyl", "disp", "hp", "wt"))
#' plot_g_function(result, 1)
plot_g_function <- function(earth_result, group_index) {
  validate_earthui_result(earth_result)
  eq <- format_model_equation(earth_result)
  non_intercept <- Filter(function(g) g$degree > 0L, eq$groups)

  group_index <- as.integer(group_index)
  if (group_index < 1L || group_index > length(non_intercept)) {
    stop("group_index must be between 1 and ", length(non_intercept),
         call. = FALSE)
  }

  grp <- non_intercept[[group_index]]
  d <- grp$degree - grp$n_factors

  if (d >= 2L) {
    plot_g_3d_(earth_result, grp)
  } else {
    plot_g_2d_(earth_result, grp)
  }
}

#' Plot g-function as a static contour (for reports)
#'
#' Creates a ggplot2 visualization for any g-function group. For d <= 1,
#' produces a 2D scatter plot (same as [plot_g_function()]). For d >= 2,
#' produces a filled contour plot suitable for static formats like PDF and Word.
#'
#' @inheritParams plot_g_function
#'
#' @return A [ggplot2::ggplot] object.
#'
#' @export
#' @examples
#' result <- fit_earth(mtcars, "mpg", c("cyl", "disp", "hp", "wt"))
#' plot_g_contour(result, 1)
plot_g_contour <- function(earth_result, group_index) {
  validate_earthui_result(earth_result)
  eq <- format_model_equation(earth_result)
  non_intercept <- Filter(function(g) g$degree > 0L, eq$groups)

  group_index <- as.integer(group_index)
  if (group_index < 1L || group_index > length(non_intercept)) {
    stop("group_index must be between 1 and ", length(non_intercept),
         call. = FALSE)
  }

  grp <- non_intercept[[group_index]]
  d <- grp$degree - grp$n_factors

  if (d >= 2L) {
    plot_g_contour_(earth_result, grp)
  } else {
    plot_g_2d_(earth_result, grp)
  }
}

# --- Internal: Evaluate g-function group on new data ---
# Returns numeric vector, one value per row of newdata
eval_g_function_ <- function(model, group, newdata) {
  coefs <- as.numeric(stats::coef(model))
  n <- nrow(newdata)
  total <- numeric(n)

  for (term in group$terms) {
    ti <- term$index
    term_val <- rep(coefs[ti], n)
    for (comp in term$components) {
      if (comp$is_factor) {
        x <- as.character(newdata[[comp$base_var]])
        term_val <- term_val * as.numeric(x == comp$level)
      } else {
        x <- newdata[[comp$base_var]]
        if (comp$dir == 2) {
          term_val <- term_val * x
        } else if (comp$dir == 1) {
          term_val <- term_val * pmax(0, x - comp$cut)
        } else {
          term_val <- term_val * pmax(0, comp$cut - x)
        }
      }
    }
    total <- total + term_val
  }
  total
}

# --- Internal: 2D plot for d <= 1 g-functions ---
plot_g_2d_ <- function(earth_result, grp) {
  model <- earth_result$model
  data <- earth_result$data
  target <- earth_result$target

  title <- sprintf("%s  (%d term%s)", grp$label,
                   length(grp$terms), if (length(grp$terms) > 1L) "s" else "")

  numeric_vars <- grp$base_vars[!grp$base_vars %in% earth_result$categoricals]

  if (length(numeric_vars) == 0L) {
    # All factors: bar chart
    contrib <- eval_g_function_(model, grp, data)
    fvar <- grp$base_vars[1]
    plot_df <- data.frame(x = as.character(data[[fvar]]), y = contrib,
                          stringsAsFactors = FALSE)
    return(
      ggplot2::ggplot(plot_df, ggplot2::aes(x = .data$x, y = .data$y)) +
        ggplot2::geom_boxplot(fill = "#2c7bb6", alpha = 0.5,
                              outlier.shape = NA) +
        ggplot2::geom_jitter(color = "#2c7bb6", alpha = 0.5, width = 0.2) +
        ggplot2::scale_y_continuous(labels = dollar_format_) +
        ggplot2::labs(title = title, x = fvar,
                      y = paste("Contribution to", target)) +
        ggplot2::theme_minimal() +
        ggplot2::theme(
          plot.title = ggplot2::element_text(face = "bold", size = 13))
    )
  }

  var <- numeric_vars[1]
  var_col <- data[[var]]

  # Compute training-data contributions using bx (exact match to model)
  term_indices <- vapply(grp$terms, function(t) t$index, integer(1))
  bx <- model$bx
  coefs <- as.numeric(stats::coef(model))
  contrib <- rowSums(sweep(bx[, term_indices, drop = FALSE], 2,
                           coefs[term_indices], "*"))

  plot_df <- data.frame(x = var_col, y = contrib)
  plot_df <- plot_df[order(plot_df$x), ]

  # Collect knots for this variable from the group's terms
  dirs <- model$dirs[model$selected.terms, , drop = FALSE]
  cuts <- model$cuts[model$selected.terms, , drop = FALSE]
  col_names <- colnames(dirs)

  matching_cols <- col_names == var
  if (!any(matching_cols)) {
    for (cn in col_names) {
      if (startsWith(cn, var) && cn != var) {
        matching_cols[col_names == cn] <- TRUE
      }
    }
  }

  knots <- numeric(0)
  for (ti in term_indices) {
    for (ci in which(matching_cols)) {
      if (dirs[ti, ci] != 0 && dirs[ti, ci] != 2) {
        knots <- c(knots, cuts[ti, ci])
      }
    }
  }
  knots <- sort(unique(knots))

  x_min <- min(var_col, na.rm = TRUE)
  x_max <- max(var_col, na.rm = TRUE)
  knots <- knots[knots > x_min & knots < x_max]

  # Evaluate g-function on a fine grid for the line
  grid_x <- seq(x_min, x_max, length.out = 200)
  eps <- (x_max - x_min) * 1e-6
  for (k in knots) {
    grid_x <- c(grid_x, k - eps, k, k + eps)
  }
  grid_x <- sort(unique(grid_x))
  grid_x <- grid_x[grid_x >= x_min & grid_x <= x_max]

  eval_row <- data[1L, , drop = FALSE]
  grid_y <- vapply(grid_x, function(xv) {
    eval_row[[var]] <- xv
    eval_g_function_(model, grp, eval_row)
  }, numeric(1))

  line_df <- data.frame(x = grid_x, y = grid_y)

  # Compute slopes between knot segments
  breaks <- c(x_min, knots, x_max)
  break_y <- vapply(breaks, function(xv) {
    eval_row[[var]] <- xv
    eval_g_function_(model, grp, eval_row)
  }, numeric(1))

  n_seg <- length(breaks) - 1L
  if (n_seg > 0L) {
    seg_df <- data.frame(
      x_mid = (breaks[-length(breaks)] + breaks[-1]) / 2,
      y_mid = (break_y[-length(break_y)] + break_y[-1]) / 2,
      slope = diff(break_y) / diff(breaks)
    )
    seg_df$label <- paste0(
      ifelse(seg_df$slope >= 0, "+$", "-$"),
      formatC(abs(seg_df$slope), format = "f", digits = 2, big.mark = ","),
      "/unit"
    )
  } else {
    seg_df <- data.frame(x_mid = numeric(0), y_mid = numeric(0),
                         slope = numeric(0), label = character(0))
  }

  p <- ggplot2::ggplot(plot_df, ggplot2::aes(x = .data$x, y = .data$y)) +
    ggplot2::geom_point(alpha = 0.6, color = "#2c7bb6") +
    ggplot2::geom_line(data = line_df, ggplot2::aes(x = .data$x, y = .data$y),
                       color = "#d7191c", linewidth = 0.8)

  if (nrow(seg_df) > 0L) {
    p <- p + ggplot2::geom_label(
      data = seg_df,
      ggplot2::aes(x = .data$x_mid, y = .data$y_mid, label = .data$label),
      size = 3.2, fill = "white", alpha = 0.85,
      label.padding = ggplot2::unit(0.15, "lines"),
      label.size = 0.3, color = "#333333"
    )
  }

  if (length(knots) > 0L) {
    p <- p + ggplot2::geom_vline(xintercept = knots, linetype = "dashed",
                                 color = "grey50", alpha = 0.5)
  }

  p + ggplot2::scale_x_continuous(labels = comma_format_) +
    ggplot2::scale_y_continuous(labels = dollar_format_) +
    ggplot2::labs(title = title, x = var,
                  y = paste("Contribution to", target)) +
    ggplot2::theme_minimal() +
    ggplot2::theme(
      plot.title = ggplot2::element_text(face = "bold", size = 13)
    )
}

# --- Internal: 3D plotly surface for d >= 2 g-functions ---
plot_g_3d_ <- function(earth_result, grp) {
  model <- earth_result$model
  data <- earth_result$data
  target <- earth_result$target

  numeric_vars <- grp$base_vars[!grp$base_vars %in% earth_result$categoricals]
  if (length(numeric_vars) < 2L) {
    return(plot_g_2d_(earth_result, grp))
  }

  var1 <- numeric_vars[1]
  var2 <- numeric_vars[2]
  title <- sprintf("%s  (%d term%s)", grp$label,
                   length(grp$terms), if (length(grp$terms) > 1L) "s" else "")

  n_grid <- 50L
  x1_seq <- seq(min(data[[var1]], na.rm = TRUE),
                max(data[[var1]], na.rm = TRUE), length.out = n_grid)
  x2_seq <- seq(min(data[[var2]], na.rm = TRUE),
                max(data[[var2]], na.rm = TRUE), length.out = n_grid)
  grid <- expand.grid(x1 = x1_seq, x2 = x2_seq)

  eval_data <- data[rep(1L, nrow(grid)), , drop = FALSE]
  eval_data[[var1]] <- grid$x1
  eval_data[[var2]] <- grid$x2
  z_vals <- eval_g_function_(model, grp, eval_data)
  z_mat <- matrix(z_vals, nrow = n_grid, ncol = n_grid)

  if (!requireNamespace("plotly", quietly = TRUE)) {
    grid$z <- z_vals
    return(
      ggplot2::ggplot(grid, ggplot2::aes(x = .data$x1, y = .data$x2)) +
        ggplot2::geom_contour_filled(ggplot2::aes(z = .data$z), bins = 15) +
        ggplot2::scale_x_continuous(labels = comma_format_) +
        ggplot2::scale_y_continuous(labels = comma_format_) +
        ggplot2::labs(title = title, x = var1, y = var2,
                      fill = paste("Contribution\nto", target)) +
        ggplot2::theme_minimal() +
        ggplot2::theme(
          plot.title = ggplot2::element_text(face = "bold", size = 13))
    )
  }

  # Training data contributions for scatter overlay
  term_indices <- vapply(grp$terms, function(t) t$index, integer(1))
  bx <- model$bx
  coefs <- as.numeric(stats::coef(model))
  contrib <- rowSums(sweep(bx[, term_indices, drop = FALSE], 2,
                           coefs[term_indices], "*"))

  plotly::plot_ly() |>
    plotly::add_surface(
      x = x1_seq, y = x2_seq, z = z_mat,
      colorscale = list(c(0, "#2166AC"), c(0.5, "#f7f7f7"), c(1, "#B2182B")),
      opacity = 0.85,
      colorbar = list(title = paste("Contribution\nto", target)),
      name = "Surface",
      showlegend = FALSE
    ) |>
    plotly::add_markers(
      x = data[[var1]], y = data[[var2]], z = contrib,
      marker = list(size = 3, color = "#333333", opacity = 0.4),
      name = "Data"
    ) |>
    plotly::layout(
      title = list(text = title, font = list(size = 14)),
      scene = list(
        xaxis = list(title = var1),
        yaxis = list(title = var2),
        zaxis = list(title = paste("Contribution to", target))
      )
    )
}

# --- Internal: Static contour for d >= 2 (PDF/Word reports) ---
plot_g_contour_ <- function(earth_result, grp) {
  model <- earth_result$model
  data <- earth_result$data
  target <- earth_result$target

  numeric_vars <- grp$base_vars[!grp$base_vars %in% earth_result$categoricals]
  if (length(numeric_vars) < 2L) {
    return(plot_g_2d_(earth_result, grp))
  }

  var1 <- numeric_vars[1]
  var2 <- numeric_vars[2]
  title <- sprintf("%s  (%d term%s)", grp$label,
                   length(grp$terms), if (length(grp$terms) > 1L) "s" else "")

  n_grid <- 80L
  x1_seq <- seq(min(data[[var1]], na.rm = TRUE),
                max(data[[var1]], na.rm = TRUE), length.out = n_grid)
  x2_seq <- seq(min(data[[var2]], na.rm = TRUE),
                max(data[[var2]], na.rm = TRUE), length.out = n_grid)

  grid <- expand.grid(x1 = x1_seq, x2 = x2_seq)
  eval_data <- data[rep(1L, nrow(grid)), , drop = FALSE]
  eval_data[[var1]] <- grid$x1
  eval_data[[var2]] <- grid$x2
  grid$z <- eval_g_function_(model, grp, eval_data)

  ggplot2::ggplot(grid, ggplot2::aes(x = .data$x1, y = .data$x2)) +
    ggplot2::geom_contour_filled(ggplot2::aes(z = .data$z), bins = 15) +
    ggplot2::scale_x_continuous(labels = comma_format_) +
    ggplot2::scale_y_continuous(labels = comma_format_) +
    ggplot2::labs(title = title, x = var1, y = var2,
                  fill = paste("Contribution\nto", target)) +
    ggplot2::theme_minimal() +
    ggplot2::theme(
      plot.title = ggplot2::element_text(face = "bold", size = 13)
    )
}
