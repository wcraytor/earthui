# Internal: format axis labels as $1,234 (dollar with commas, no decimals)
dollar_format_ <- function(x) {
  ifelse(is.na(x), "",
         paste0("$", formatC(round(x), format = "f", digits = 0, big.mark = ",")))
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
