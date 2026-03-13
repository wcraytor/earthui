# Internal: choose decimal places based on axis range
auto_digits_ <- function(x) {
  x <- x[is.finite(x)]
  if (length(x) < 2L) return(0L)
  rng <- diff(range(x))
  if (rng < 1)   3L
  else if (rng < 10)  2L
  else if (rng < 100) 1L
  else 0L
}

# Internal: format slope labels adapting the unit to x-axis range.
# For lat/long-scale variables (range < 1) the unit becomes 0.001.
format_slope_labels_ <- function(slopes, x_breaks) {
  d <- auto_digits_(x_breaks)
  bm <- locale_big_mark_()
  dm <- locale_dec_mark_()
  unit_size <- 10^(-d)          # 1, 0.1, 0.01, or 0.001
  scaled <- slopes * unit_size
  unit_label <- if (d == 0L) "/unit" else paste0("/", format(unit_size, scientific = FALSE))
  paste0(
    ifelse(scaled >= 0, "+", "-"),
    formatC(abs(scaled), format = "f", digits = 2, big.mark = bm, decimal.mark = dm),
    unit_label
  )
}

# Internal: format axis labels as 1,234 (with locale-aware separators)
# Adapts decimal places to value range (e.g. lat/long gets 3 dp)
dollar_format_ <- function(x) {
  d <- auto_digits_(x)
  bm <- locale_big_mark_()
  dm <- locale_dec_mark_()
  ifelse(is.na(x), "",
         formatC(x, format = "f", digits = d, big.mark = bm, decimal.mark = dm))
}

# Internal: format axis labels with locale-aware separators
# Adapts decimal places to value range (e.g. lat/long gets 3 dp)
comma_format_ <- function(x) {
  d <- auto_digits_(x)
  bm <- locale_big_mark_()
  dm <- locale_dec_mark_()
  ifelse(is.na(x), "",
         formatC(x, format = "f", digits = d, big.mark = bm, decimal.mark = dm))
}

# Internal: return font family with fallback for environments without showtext
eui_font_family_ <- function() {
  if (requireNamespace("sysfonts", quietly = TRUE) &&
      "Roboto Condensed" %in% sysfonts::font_families()) {
    "Roboto Condensed"
  } else {
    "sans"
  }
}

#' Plot variable importance
#'
#' Creates a horizontal bar chart of variable importance from a fitted
#' earth model.
#'
#' @param earth_result An object of class `"earthUI_result"` as returned by
#'   [fit_earth()].
#' @param type Character. Importance metric to plot: `"nsubsets"` (default),
#'   `"gcv"`, or `"rss"`.
#'
#' @return A [ggplot2::ggplot] object.
#'
#' @export
#' @examples
#' \donttest{
#' result <- fit_earth(mtcars, "mpg", c("cyl", "disp", "hp", "wt"))
#' plot_variable_importance(result)
#' }
plot_variable_importance <- function(earth_result, type = "nsubsets") {
  validate_earthUI_result(earth_result)
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
#' @param earth_result An object of class `"earthUI_result"` as returned by
#'   [fit_earth()].
#' @param variable Character string. Name of the predictor variable to plot.
#' @param n_grid Integer. Number of grid points for the partial dependence
#'   calculation. Default is 50.
#' @param response_idx Integer or `NULL`. For multivariate models, which
#'   response column to plot (1-based). Default `NULL` uses the first response.
#'
#' @return A [ggplot2::ggplot] object.
#'
#' @export
#' @examples
#' \donttest{
#' result <- fit_earth(mtcars, "mpg", c("cyl", "disp", "hp", "wt"))
#' plot_partial_dependence(result, "wt")
#' }
plot_partial_dependence <- function(earth_result, variable, n_grid = 50L,
                                    response_idx = NULL) {
  validate_earthUI_result(earth_result)

  if (!is.character(variable) || length(variable) != 1L) {
    stop("`variable` must be a single character string.", call. = FALSE)
  }
  if (!variable %in% names(earth_result$data)) {
    stop("Variable '", variable, "' not found in model data.", call. = FALSE)
  }

  model <- earth_result$model
  data <- earth_result$data
  targets <- earth_result$target
  multi <- length(targets) > 1L
  ri <- if (is.null(response_idx)) 1L else response_idx
  target_label <- targets[ri]

  var_col <- data[[variable]]

  if (is.factor(var_col) || is.character(var_col)) {
    grid_vals <- sort(unique(var_col))
  } else {
    grid_vals <- seq(min(var_col, na.rm = TRUE),
                     max(var_col, na.rm = TRUE),
                     length.out = n_grid)
  }

  pd_values <- vapply(grid_vals, function(val) {
    temp_data <- data
    temp_data[[variable]] <- val
    preds <- stats::predict(model, newdata = temp_data)
    if (multi) mean(preds[, ri]) else mean(preds)
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
      y = paste("Predicted", target_label)
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
#' @param earth_result An object of class `"earthUI_result"` as returned by
#'   [fit_earth()].
#' @param variable Character string. Name of the predictor variable to plot.
#' @param response_idx Integer or `NULL`. For multivariate models, which
#'   response column to plot (1-based). Default `NULL` uses the first response.
#'
#' @return A [ggplot2::ggplot] object.
#'
#' @export
#' @examples
#' \donttest{
#' result <- fit_earth(mtcars, "mpg", c("cyl", "disp", "hp", "wt"))
#' plot_contribution(result, "wt")
#' }
plot_contribution <- function(earth_result, variable, response_idx = NULL) {
  validate_earthUI_result(earth_result)

  if (!is.character(variable) || length(variable) != 1L) {
    stop("`variable` must be a single character string.", call. = FALSE)
  }

  model <- earth_result$model
  data <- earth_result$data
  targets <- earth_result$target
  multi <- length(targets) > 1L
  ri <- if (is.null(response_idx)) 1L else response_idx
  target_label <- targets[ri]

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
  coef_mat <- model$coefficients
  coefs <- if (multi) as.numeric(coef_mat[, ri]) else as.numeric(coef_mat)
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
        y = paste("Contribution to", target_label)
      ) +
      ggplot2::theme_minimal() +
      ggplot2::theme(
        plot.title = ggplot2::element_text(face = "bold", size = 14)
      )
  } else {
    plot_df <- data.frame(x = var_col, y = var_contrib)
    plot_df <- plot_df[order(plot_df$x), ]

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

    x_min <- min(var_col, na.rm = TRUE)
    x_max <- max(var_col, na.rm = TRUE)
    knots <- knots[knots > x_min & knots < x_max]
    breaks <- c(x_min, knots, x_max)

    eval_contrib <- function(x_val) {
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

    n_seg <- length(breaks) - 1L
    if (n_seg > 0L) {
      seg_df <- data.frame(
        x_mid  = (breaks[-length(breaks)] + breaks[-1]) / 2,
        y_mid  = (break_y[-length(break_y)] + break_y[-1]) / 2,
        slope  = diff(break_y) / diff(breaks)
      )
      seg_df$label <- format_slope_labels_(seg_df$slope, breaks)
    } else {
      seg_df <- data.frame(x_mid = numeric(0), y_mid = numeric(0),
                           slope = numeric(0), label = character(0))
    }

    line_df <- data.frame(x = breaks, y = break_y)

    p <- ggplot2::ggplot(plot_df, ggplot2::aes(x = .data$x, y = .data$y)) +
      ggplot2::geom_point(alpha = 0.6, color = "#2c7bb6") +
      ggplot2::geom_line(data = line_df, ggplot2::aes(x = .data$x, y = .data$y),
                         color = "#d7191c", linewidth = 0.8)

    if (nrow(seg_df) > 0L) {
      y_range <- diff(range(plot_df$y, na.rm = TRUE))
      if (y_range == 0) y_range <- 1
      p <- p + ggplot2::geom_label(
        data = seg_df,
        ggplot2::aes(x = .data$x_mid, y = .data$y_mid, label = .data$label),
        size = 3.2, fill = "white", alpha = 0.85,
        label.padding = ggplot2::unit(0.15, "lines"),
        label.size = 0.3, color = "#333333",
        family = eui_font_family_()
      )
    }

    if (length(knots) > 0L) {
      p <- p + ggplot2::geom_vline(xintercept = knots, linetype = "dashed",
                                   color = "grey50", alpha = 0.5)
    }

    p + ggplot2::scale_x_continuous(labels = comma_format_) +
      ggplot2::scale_y_continuous(labels = dollar_format_) +
      ggplot2::labs(
        title = paste("Contribution:", variable),
        x = variable,
        y = paste("Contribution to", target_label)
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
#' @param earth_result An object of class `"earthUI_result"` as returned by
#'   [fit_earth()].
#'
#' @return A [ggplot2::ggplot] object.
#'
#' @export
#' @examples
#' \donttest{
#' result <- fit_earth(mtcars, "mpg", c("cyl", "disp", "hp", "wt"))
#' plot_correlation_matrix(result)
#' }
plot_correlation_matrix <- function(earth_result) {
  validate_earthUI_result(earth_result)

  # target may be a vector for multivariate models
  vars <- c(earth_result$target, earth_result$predictors)
  vars <- unique(vars)
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
  # geom_text size is in mm; multiply by ~2.85 to approximate pt
  txt_size <- if (n <= 6) 7 else if (n <= 10) 5 else if (n <= 15) 4 else 3.2
  axis_size <- if (n <= 6) 14 else if (n <= 10) 13 else if (n <= 15) 11 else 9
  title_size <- 16

  text_color <- ifelse(abs(long_df$value) > 0.65, "white", "black")

  ggplot2::ggplot(long_df,
                  ggplot2::aes(x = .data$Var1, y = .data$Var2,
                               fill = .data$value)) +
    ggplot2::geom_tile(color = "white", linewidth = 1.2) +
    ggplot2::geom_text(ggplot2::aes(label = sprintf("%.2f", .data$value)),
                       size = txt_size, color = text_color,
                       family = eui_font_family_()) +
    ggplot2::scale_fill_gradient2(
      low = "#2166AC", mid = "white", high = "#B2182B",
      midpoint = 0, limits = c(-1, 1), name = "Correlation"
    ) +
    ggplot2::labs(
      title = "Correlation Matrix",
      x = NULL, y = NULL
    ) +
    ggplot2::theme_minimal(base_size = 14) +
    ggplot2::theme(
      plot.title = ggplot2::element_text(face = "bold", size = title_size),
      axis.text.x = ggplot2::element_text(angle = 45, hjust = 1, size = axis_size),
      axis.text.y = ggplot2::element_text(size = axis_size),
      panel.grid = ggplot2::element_blank(),
      legend.position = "right",
      legend.text = ggplot2::element_text(size = 11),
      legend.title = ggplot2::element_text(size = 12)
    ) +
    ggplot2::coord_fixed(expand = FALSE)
}

#' Plot residual diagnostics
#'
#' Creates a two-panel diagnostic plot: residuals vs fitted values and
#' a Q-Q plot of residuals.
#'
#' @param earth_result An object of class `"earthUI_result"` as returned by
#'   [fit_earth()].
#' @param response_idx Integer or `NULL`. For multivariate models, which
#'   response column to plot (1-based). Default `NULL` uses the first response.
#'
#' @return A [ggplot2::ggplot] object showing residuals vs fitted values.
#'   Use [plot_qq()] for the Q-Q plot separately.
#'
#' @export
#' @examples
#' \donttest{
#' result <- fit_earth(mtcars, "mpg", c("cyl", "disp", "hp", "wt"))
#' plot_residuals(result)
#' }
plot_residuals <- function(earth_result, response_idx = NULL) {
  validate_earthUI_result(earth_result)
  model <- earth_result$model
  targets <- earth_result$target
  multi <- length(targets) > 1L
  ri <- if (is.null(response_idx)) 1L else response_idx
  target_label <- targets[ri]

  fitted_mat <- stats::fitted(model)
  resid_mat <- stats::residuals(model)
  resid_df <- data.frame(
    fitted = if (multi) as.numeric(fitted_mat[, ri]) else as.numeric(fitted_mat),
    residuals = if (multi) as.numeric(resid_mat[, ri]) else as.numeric(resid_mat)
  )

  title <- if (multi) paste0("Residuals vs Fitted: ", target_label) else "Residuals vs Fitted"

  ggplot2::ggplot(resid_df,
                  ggplot2::aes(x = .data$fitted, y = .data$residuals)) +
    ggplot2::geom_point(alpha = 0.6, color = "#2c7bb6") +
    ggplot2::geom_hline(yintercept = 0, linetype = "dashed", color = "red") +
    ggplot2::scale_x_continuous(labels = dollar_format_) +
    ggplot2::scale_y_continuous(labels = dollar_format_) +
    ggplot2::labs(
      title = title,
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
#' @param earth_result An object of class `"earthUI_result"` as returned by
#'   [fit_earth()].
#' @param response_idx Integer or `NULL`. For multivariate models, which
#'   response column to plot (1-based). Default `NULL` uses the first response.
#'
#' @return A [ggplot2::ggplot] object.
#'
#' @export
#' @examples
#' \donttest{
#' result <- fit_earth(mtcars, "mpg", c("cyl", "disp", "hp", "wt"))
#' plot_qq(result)
#' }
plot_qq <- function(earth_result, response_idx = NULL) {
  validate_earthUI_result(earth_result)
  model <- earth_result$model
  targets <- earth_result$target
  multi <- length(targets) > 1L
  ri <- if (is.null(response_idx)) 1L else response_idx
  target_label <- targets[ri]

  resid_mat <- stats::residuals(model)
  resids <- if (multi) as.numeric(resid_mat[, ri]) else as.numeric(resid_mat)

  qq_data <- data.frame(
    theoretical = stats::qqnorm(resids, plot.it = FALSE)$x,
    sample = sort(resids)
  )

  title <- if (multi) paste0("Normal Q-Q Plot: ", target_label) else "Normal Q-Q Plot"

  ggplot2::ggplot(qq_data,
                  ggplot2::aes(x = .data$theoretical, y = .data$sample)) +
    ggplot2::geom_point(alpha = 0.6, color = "#2c7bb6") +
    ggplot2::geom_abline(slope = stats::sd(resids),
                         intercept = mean(resids),
                         linetype = "dashed", color = "red") +
    ggplot2::scale_y_continuous(labels = dollar_format_) +
    ggplot2::labs(
      title = title,
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
#' @param earth_result An object of class `"earthUI_result"` as returned by
#'   [fit_earth()].
#' @param response_idx Integer or `NULL`. For multivariate models, which
#'   response column to plot (1-based). Default `NULL` uses the first response.
#'
#' @return A [ggplot2::ggplot] object.
#'
#' @export
#' @examples
#' \donttest{
#' result <- fit_earth(mtcars, "mpg", c("cyl", "disp", "hp", "wt"))
#' plot_actual_vs_predicted(result)
#' }
plot_actual_vs_predicted <- function(earth_result, response_idx = NULL) {
  validate_earthUI_result(earth_result)
  model <- earth_result$model
  targets <- earth_result$target
  multi <- length(targets) > 1L
  ri <- if (is.null(response_idx)) 1L else response_idx
  target_label <- targets[ri]

  fitted_mat <- stats::fitted(model)
  plot_df <- data.frame(
    actual = earth_result$data[[target_label]],
    predicted = if (multi) as.numeric(fitted_mat[, ri]) else as.numeric(fitted_mat)
  )

  title <- if (multi) paste0("Actual vs Predicted: ", target_label) else "Actual vs Predicted"

  ggplot2::ggplot(plot_df,
                  ggplot2::aes(x = .data$actual, y = .data$predicted)) +
    ggplot2::geom_point(alpha = 0.6, color = "#2c7bb6") +
    ggplot2::geom_abline(slope = 1, intercept = 0,
                         linetype = "dashed", color = "red") +
    ggplot2::scale_x_continuous(labels = dollar_format_) +
    ggplot2::scale_y_continuous(labels = dollar_format_) +
    ggplot2::labs(
      title = title,
      x = paste("Actual", target_label),
      y = paste("Predicted", target_label)
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
#' @param earth_result An object of class `"earthUI_result"` as returned by
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
#' \donttest{
#' result <- fit_earth(mtcars, "mpg", c("cyl", "disp", "hp", "wt"))
#' list_g_functions(result)
#' }
list_g_functions <- function(earth_result) {
  validate_earthUI_result(earth_result)
  eq <- format_model_equation(earth_result, response_idx = 1L)
  # For multivariate, groups are shared; use first response's equation
  groups <- if (inherits(eq, "earthUI_equation_multi")) {
    eq$equations[[1]]$groups
  } else {
    eq$groups
  }
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
#' @param earth_result An object of class `"earthUI_result"` as returned by
#'   [fit_earth()].
#' @param group_index Integer. Index of the g-function group (1-based, from
#'   [list_g_functions()]).
#' @param response_idx Integer or `NULL`. For multivariate models, which
#'   response column to plot (1-based). Default `NULL` uses the first response.
#'
#' @return A [ggplot2::ggplot] object for d <= 1, or a plotly widget for d >= 2
#'   (when plotly is installed). Falls back to ggplot2 contour if plotly is not
#'   available.
#'
#' @export
#' @examples
#' \donttest{
#' result <- fit_earth(mtcars, "mpg", c("cyl", "disp", "hp", "wt"))
#' plot_g_function(result, 1)
#' }
plot_g_function <- function(earth_result, group_index, response_idx = NULL) {
  validate_earthUI_result(earth_result)
  eq <- format_model_equation(earth_result, response_idx = if (is.null(response_idx)) 1L else response_idx)
  groups <- if (inherits(eq, "earthUI_equation_multi")) {
    eq$equations[[1]]$groups
  } else {
    eq$groups
  }
  non_intercept <- Filter(function(g) g$degree > 0L, groups)

  group_index <- as.integer(group_index)
  if (group_index < 1L || group_index > length(non_intercept)) {
    stop("group_index must be between 1 and ", length(non_intercept),
         call. = FALSE)
  }

  grp <- non_intercept[[group_index]]
  d <- grp$degree - grp$n_factors

  if (d >= 2L) {
    plot_g_3d_(earth_result, grp, response_idx = response_idx)
  } else {
    plot_g_2d_(earth_result, grp, response_idx = response_idx)
  }
}

#' Plot g-function as a static 3D perspective (for reports)
#'
#' Creates a base R `persp()` 3D surface plot for g-function groups with d >= 2.
#' For d <= 1, produces a 2D scatter plot (same as [plot_g_function()]).
#' The surface is colored by contribution value using a blue-white-red scale.
#' Suitable for PDF and Word output where interactive plotly is not available.
#'
#' @inheritParams plot_g_function
#' @param theta Numeric. Azimuthal rotation angle in degrees. Default 30.
#' @param phi Numeric. Elevation angle in degrees. Default 25.
#'
#' @return Invisible `NULL` (base graphics). For d <= 1, returns a ggplot object.
#'
#' @export
#' @examples
#' \donttest{
#' result <- fit_earth(mtcars, "mpg", c("cyl", "disp", "hp", "wt"), degree = 2L)
#' plot_g_persp(result, 1)
#' }
plot_g_persp <- function(earth_result, group_index, theta = 30, phi = 25,
                         response_idx = NULL) {
  validate_earthUI_result(earth_result)
  eq <- format_model_equation(earth_result, response_idx = if (is.null(response_idx)) 1L else response_idx)
  groups <- if (inherits(eq, "earthUI_equation_multi")) {
    eq$equations[[1]]$groups
  } else {
    eq$groups
  }
  non_intercept <- Filter(function(g) g$degree > 0L, groups)

  group_index <- as.integer(group_index)
  if (group_index < 1L || group_index > length(non_intercept)) {
    stop("group_index must be between 1 and ", length(non_intercept),
         call. = FALSE)
  }

  grp <- non_intercept[[group_index]]
  d <- grp$degree - grp$n_factors

  if (d < 2L) {
    return(plot_g_2d_(earth_result, grp, response_idx = response_idx))
  }

  plot_g_persp_(earth_result, grp, theta = theta, phi = phi,
                response_idx = response_idx)
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
#' \donttest{
#' result <- fit_earth(mtcars, "mpg", c("cyl", "disp", "hp", "wt"))
#' plot_g_contour(result, 1)
#' }
plot_g_contour <- function(earth_result, group_index, response_idx = NULL) {
  validate_earthUI_result(earth_result)
  eq <- format_model_equation(earth_result, response_idx = if (is.null(response_idx)) 1L else response_idx)
  groups <- if (inherits(eq, "earthUI_equation_multi")) {
    eq$equations[[1]]$groups
  } else {
    eq$groups
  }
  non_intercept <- Filter(function(g) g$degree > 0L, groups)

  group_index <- as.integer(group_index)
  if (group_index < 1L || group_index > length(non_intercept)) {
    stop("group_index must be between 1 and ", length(non_intercept),
         call. = FALSE)
  }

  grp <- non_intercept[[group_index]]
  d <- grp$degree - grp$n_factors

  if (d >= 2L) {
    plot_g_contour_(earth_result, grp, response_idx = response_idx)
  } else {
    plot_g_2d_(earth_result, grp, response_idx = response_idx)
  }
}

# --- Internal: Evaluate g-function group on new data ---
# Returns numeric vector, one value per row of newdata
# response_idx: for multivariate models, which response column (1-based)
eval_g_function_ <- function(model, group, newdata, response_idx = NULL) {
  coef_mat <- model$coefficients
  multi <- ncol(coef_mat) > 1L
  ri <- if (is.null(response_idx)) 1L else response_idx
  coefs <- if (multi) as.numeric(coef_mat[, ri]) else as.numeric(coef_mat)
  n <- nrow(newdata)
  total <- numeric(n)

  for (term in group$terms) {
    ti <- term$index
    term_val <- rep(coefs[ti], n)
    for (comp in term$components) {
      if (comp$is_factor) {
        col_data <- newdata[[comp$base_var]]
        if (is.null(col_data)) {
          term_val <- rep(0, n)
          break
        }
        x <- as.character(col_data)
        term_val <- term_val * as.numeric(x == comp$level)
      } else {
        x <- newdata[[comp$base_var]]
        if (is.null(x)) {
          term_val <- rep(0, n)
          break
        }
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
plot_g_2d_ <- function(earth_result, grp, response_idx = NULL) {
  model <- earth_result$model
  data <- earth_result$data
  targets <- earth_result$target
  multi <- length(targets) > 1L
  ri <- if (is.null(response_idx)) 1L else response_idx
  target_label <- targets[ri]

  title <- sprintf("%s  (%d term%s)", grp$label,
                   length(grp$terms), if (length(grp$terms) > 1L) "s" else "")

  numeric_vars <- grp$base_vars[!grp$base_vars %in% earth_result$categoricals]
  # Safety: filter to variables that actually exist in the data
  numeric_vars <- numeric_vars[numeric_vars %in% names(data)]

  if (length(numeric_vars) == 0L) {
    contrib <- eval_g_function_(model, grp, data, response_idx = ri)
    fvar <- grp$base_vars[grp$base_vars %in% earth_result$categoricals]
    fvar <- fvar[fvar %in% names(data)]
    if (length(fvar) == 0L) {
      return(
        ggplot2::ggplot() +
          ggplot2::annotate("text", x = 0.5, y = 0.5,
                            label = paste("Cannot resolve variables:",
                                          paste(grp$base_vars, collapse = ", "))) +
          ggplot2::theme_void()
      )
    }

    if (length(fvar) >= 3L) {
      # Three or more factors with d = 0: faceted heatmap tiles
      fvar1 <- fvar[1]
      fvar2 <- fvar[2]
      fvar3 <- fvar[3]
      lvls1 <- sort(unique(as.character(data[[fvar1]])))
      lvls2 <- sort(unique(as.character(data[[fvar2]])))
      lvls3 <- sort(unique(as.character(data[[fvar3]])))
      combos <- expand.grid(f1 = lvls1, f2 = lvls2, f3 = lvls3,
                             stringsAsFactors = FALSE)
      combos$y <- vapply(seq_len(nrow(combos)), function(i) {
        eval_row <- data[1L, , drop = FALSE]
        eval_row[[fvar1]] <- combos$f1[i]
        eval_row[[fvar2]] <- combos$f2[i]
        eval_row[[fvar3]] <- combos$f3[i]
        eval_g_function_(model, grp, eval_row, response_idx = ri)
      }, numeric(1))
      return(
        ggplot2::ggplot(combos,
          ggplot2::aes(x = .data$f1, y = .data$f2, fill = .data$y)) +
          ggplot2::geom_tile(color = "white", linewidth = 1.5) +
          ggplot2::geom_text(
            ggplot2::aes(label = dollar_format_(.data$y)),
            size = 3.5, fontface = "bold",
            family = eui_font_family_()) +
          ggplot2::facet_wrap(~ f3, labeller = ggplot2::labeller(
            f3 = function(x) paste(fvar3, "=", x))) +
          ggplot2::scale_fill_gradient2(
            low = "#2166AC", mid = "white", high = "#B2182B",
            midpoint = 0, labels = dollar_format_) +
          ggplot2::labs(title = title, x = fvar1, y = fvar2,
                        fill = "Contribution") +
          ggplot2::theme_minimal() +
          ggplot2::theme(
            plot.title = ggplot2::element_text(face = "bold", size = 13),
            panel.grid = ggplot2::element_blank(),
            strip.text = ggplot2::element_text(face = "bold", size = 11))
      )
    }

    if (length(fvar) == 2L) {
      # Two factors with d = 0: heatmap tile chart
      fvar1 <- fvar[1]
      fvar2 <- fvar[2]
      plot_df <- data.frame(
        f1 = as.character(data[[fvar1]]),
        f2 = as.character(data[[fvar2]]),
        y  = contrib,
        stringsAsFactors = FALSE
      )
      agg <- stats::aggregate(y ~ f1 + f2, data = plot_df, FUN = mean)
      return(
        ggplot2::ggplot(agg,
          ggplot2::aes(x = .data$f1, y = .data$f2, fill = .data$y)) +
          ggplot2::geom_tile(color = "white", linewidth = 1.5) +
          ggplot2::geom_text(
            ggplot2::aes(label = dollar_format_(.data$y)),
            size = 4.5, fontface = "bold",
            family = eui_font_family_()) +
          ggplot2::scale_fill_gradient2(
            low = "#2166AC", mid = "white", high = "#B2182B",
            midpoint = 0, labels = dollar_format_) +
          ggplot2::labs(title = title, x = fvar1, y = fvar2,
                        fill = "Contribution") +
          ggplot2::theme_minimal() +
          ggplot2::theme(
            plot.title = ggplot2::element_text(face = "bold", size = 13),
            panel.grid = ggplot2::element_blank())
      )
    }

    # Single factor with d = 0: boxplot + jitter
    fvar <- fvar[1]
    plot_df <- data.frame(x = as.character(data[[fvar]]), y = contrib,
                          stringsAsFactors = FALSE)
    return(
      ggplot2::ggplot(plot_df, ggplot2::aes(x = .data$x, y = .data$y)) +
        ggplot2::geom_boxplot(fill = "#2c7bb6", alpha = 0.5,
                              outlier.shape = NA) +
        ggplot2::geom_jitter(color = "#2c7bb6", alpha = 0.5, width = 0.2) +
        ggplot2::scale_y_continuous(labels = dollar_format_) +
        ggplot2::labs(title = title, x = fvar,
                      y = paste("Contribution to", target_label)) +
        ggplot2::theme_minimal() +
        ggplot2::theme(
          plot.title = ggplot2::element_text(face = "bold", size = 13))
    )
  }

  var <- numeric_vars[1]
  var_col <- data[[var]]

  # Identify factor variables in the group
  factor_vars <- grp$base_vars[grp$base_vars %in% earth_result$categoricals]
  factor_vars <- factor_vars[factor_vars %in% names(data)]

  # Compute training-data contributions using bx (exact match to model)
  term_indices <- vapply(grp$terms, function(t) t$index, integer(1))
  bx <- model$bx
  coef_mat <- model$coefficients
  coefs <- if (multi) as.numeric(coef_mat[, ri]) else as.numeric(coef_mat)

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

  grid_x <- seq(x_min, x_max, length.out = 200)
  eps <- (x_max - x_min) * 1e-6
  for (k in knots) {
    grid_x <- c(grid_x, k - eps, k, k + eps)
  }
  grid_x <- sort(unique(grid_x))
  grid_x <- grid_x[grid_x >= x_min & grid_x <= x_max]

  # --- Factor-aware: separate line per factor level / combination ---
  if (length(factor_vars) == 1L) {
    # Single factor: color by factor level
    fvar <- factor_vars[1]
    levels_in_data <- sort(unique(as.character(data[[fvar]])))

    line_dfs <- list()
    label_dfs <- list()
    for (lvl in levels_in_data) {
      eval_row <- data[1L, , drop = FALSE]
      eval_row[[fvar]] <- lvl
      grid_y <- vapply(grid_x, function(xv) {
        eval_row[[var]] <- xv
        eval_g_function_(model, grp, eval_row, response_idx = ri)
      }, numeric(1))
      line_dfs[[lvl]] <- data.frame(x = grid_x, y = grid_y, level = lvl,
                                    stringsAsFactors = FALSE)

      breaks <- c(x_min, knots, x_max)
      break_y <- vapply(breaks, function(xv) {
        eval_row[[var]] <- xv
        eval_g_function_(model, grp, eval_row, response_idx = ri)
      }, numeric(1))
      n_seg <- length(breaks) - 1L
      if (n_seg > 0L) {
        sdf <- data.frame(
          x_mid = (breaks[-length(breaks)] + breaks[-1]) / 2,
          y_mid = (break_y[-length(break_y)] + break_y[-1]) / 2,
          slope = diff(break_y) / diff(breaks),
          level = lvl,
          stringsAsFactors = FALSE
        )
        sdf$label <- format_slope_labels_(sdf$slope, breaks)
        label_dfs[[lvl]] <- sdf
      }
    }

    all_lines <- do.call(rbind, line_dfs)
    all_labels <- if (length(label_dfs) > 0L) do.call(rbind, label_dfs) else
      data.frame(x_mid = numeric(0), y_mid = numeric(0), slope = numeric(0),
                 level = character(0), label = character(0))

    # Scatter points colored by factor level
    contrib <- rowSums(sweep(bx[, term_indices, drop = FALSE], 2,
                             coefs[term_indices], "*"))
    plot_df <- data.frame(x = var_col, y = contrib,
                          level = as.character(data[[fvar]]),
                          stringsAsFactors = FALSE)

    p <- ggplot2::ggplot(plot_df,
           ggplot2::aes(x = .data$x, y = .data$y, color = .data$level)) +
      ggplot2::geom_point(alpha = 0.4) +
      ggplot2::geom_line(data = all_lines,
                         ggplot2::aes(x = .data$x, y = .data$y,
                                      color = .data$level),
                         linewidth = 0.9)

    if (nrow(all_labels) > 0L) {
      p <- p + ggplot2::geom_label(
        data = all_labels,
        ggplot2::aes(x = .data$x_mid, y = .data$y_mid, label = .data$label,
                     color = .data$level),
        size = 3.0, fill = "white", alpha = 0.85,
        label.padding = ggplot2::unit(0.15, "lines"),
        label.size = 0.3, show.legend = FALSE,
        family = eui_font_family_()
      )
    }

    if (length(knots) > 0L) {
      p <- p + ggplot2::geom_vline(xintercept = knots, linetype = "dashed",
                                   color = "grey50", alpha = 0.5)
    }

    return(
      p + ggplot2::scale_x_continuous(labels = comma_format_) +
        ggplot2::scale_y_continuous(labels = dollar_format_) +
        ggplot2::labs(title = title, x = var,
                      y = paste("Contribution to", target_label),
                      color = fvar) +
        ggplot2::theme_minimal() +
        ggplot2::theme(
          plot.title = ggplot2::element_text(face = "bold", size = 13))
    )
  }

  if (length(factor_vars) >= 2L) {
    # Two+ factors: color by first factor, linetype by second
    fvar1 <- factor_vars[1]
    fvar2 <- factor_vars[2]
    lvls1 <- sort(unique(as.character(data[[fvar1]])))
    lvls2 <- sort(unique(as.character(data[[fvar2]])))
    combos <- expand.grid(f1 = lvls1, f2 = lvls2, stringsAsFactors = FALSE)

    line_dfs <- list()
    for (i in seq_len(nrow(combos))) {
      eval_row <- data[1L, , drop = FALSE]
      eval_row[[fvar1]] <- combos$f1[i]
      eval_row[[fvar2]] <- combos$f2[i]
      grid_y <- vapply(grid_x, function(xv) {
        eval_row[[var]] <- xv
        eval_g_function_(model, grp, eval_row, response_idx = ri)
      }, numeric(1))
      line_dfs[[i]] <- data.frame(x = grid_x, y = grid_y,
                                  f1 = combos$f1[i], f2 = combos$f2[i],
                                  stringsAsFactors = FALSE)
    }
    all_lines <- do.call(rbind, line_dfs)

    p <- ggplot2::ggplot(all_lines,
           ggplot2::aes(x = .data$x, y = .data$y,
                        color = .data$f1, linetype = .data$f2)) +
      ggplot2::geom_line(linewidth = 0.9)

    if (length(knots) > 0L) {
      p <- p + ggplot2::geom_vline(xintercept = knots, linetype = "dashed",
                                   color = "grey50", alpha = 0.5)
    }

    return(
      p + ggplot2::scale_x_continuous(labels = comma_format_) +
        ggplot2::scale_y_continuous(labels = dollar_format_) +
        ggplot2::labs(title = title, x = var,
                      y = paste("Contribution to", target_label),
                      color = fvar1, linetype = fvar2) +
        ggplot2::theme_minimal() +
        ggplot2::theme(
          plot.title = ggplot2::element_text(face = "bold", size = 13))
    )
  }

  # --- No factors: single line (original behavior) ---
  contrib <- rowSums(sweep(bx[, term_indices, drop = FALSE], 2,
                           coefs[term_indices], "*"))

  plot_df <- data.frame(x = var_col, y = contrib)
  plot_df <- plot_df[order(plot_df$x), ]

  eval_row <- data[1L, , drop = FALSE] # nolint: object_usage_linter. Used in closures below.
  grid_y <- vapply(grid_x, function(xv) {
    eval_row[[var]] <- xv
    eval_g_function_(model, grp, eval_row, response_idx = ri)
  }, numeric(1))

  line_df <- data.frame(x = grid_x, y = grid_y)

  breaks <- c(x_min, knots, x_max)
  break_y <- vapply(breaks, function(xv) {
    eval_row[[var]] <- xv
    eval_g_function_(model, grp, eval_row, response_idx = ri)
  }, numeric(1))

  n_seg <- length(breaks) - 1L
  if (n_seg > 0L) {
    seg_df <- data.frame(
      x_mid = (breaks[-length(breaks)] + breaks[-1]) / 2,
      y_mid = (break_y[-length(break_y)] + break_y[-1]) / 2,
      slope = diff(break_y) / diff(breaks)
    )
    seg_df$label <- format_slope_labels_(seg_df$slope, breaks)
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
      label.size = 0.3, color = "#333333",
      family = eui_font_family_()
    )
  }

  if (length(knots) > 0L) {
    p <- p + ggplot2::geom_vline(xintercept = knots, linetype = "dashed",
                                 color = "grey50", alpha = 0.5)
  }

  p + ggplot2::scale_x_continuous(labels = comma_format_) +
    ggplot2::scale_y_continuous(labels = dollar_format_) +
    ggplot2::labs(title = title, x = var,
                  y = paste("Contribution to", target_label)) +
    ggplot2::theme_minimal() +
    ggplot2::theme(
      plot.title = ggplot2::element_text(face = "bold", size = 13)
    )
}

# --- Internal: 3D plotly surface for d >= 2 g-functions ---
plot_g_3d_ <- function(earth_result, grp, response_idx = NULL) {
  model <- earth_result$model
  data <- earth_result$data
  targets <- earth_result$target
  multi <- length(targets) > 1L
  ri <- if (is.null(response_idx)) 1L else response_idx
  target_label <- targets[ri]

  numeric_vars <- grp$base_vars[!grp$base_vars %in% earth_result$categoricals]
  numeric_vars <- numeric_vars[numeric_vars %in% names(data)]
  if (length(numeric_vars) < 2L) {
    return(plot_g_2d_(earth_result, grp, response_idx = ri))
  }

  var1 <- numeric_vars[1]
  var2 <- numeric_vars[2]
  title <- sprintf("%s  (%d term%s)", grp$label,
                   length(grp$terms), if (length(grp$terms) > 1L) "s" else "")

  # Detect factor variables in the group
  factor_vars <- grp$base_vars[grp$base_vars %in% earth_result$categoricals]
  factor_vars <- factor_vars[factor_vars %in% names(data)]

  n_grid <- 50L
  x1_seq <- seq(min(data[[var1]], na.rm = TRUE),
                max(data[[var1]], na.rm = TRUE), length.out = n_grid)
  x2_seq <- seq(min(data[[var2]], na.rm = TRUE),
                max(data[[var2]], na.rm = TRUE), length.out = n_grid)
  grid <- expand.grid(x1 = x1_seq, x2 = x2_seq)

  # --- Plotly unavailable: fall back to faceted ggplot contour ---
  if (!requireNamespace("plotly", quietly = TRUE)) {
    return(plot_g_contour_(earth_result, grp, response_idx = ri))
  }

  # --- Factor-aware: dropdown to switch between factor levels ---
  if (length(factor_vars) > 0L) {
    fvar <- factor_vars[1]
    levels_in_data <- sort(unique(as.character(data[[fvar]])))
    n_lvl <- length(levels_in_data)

    fig <- plotly::plot_ly()
    for (i in seq_along(levels_in_data)) {
      lvl <- levels_in_data[i]
      eval_data <- data[rep(1L, nrow(grid)), , drop = FALSE]
      eval_data[[var1]] <- grid$x1
      eval_data[[var2]] <- grid$x2
      eval_data[[fvar]] <- lvl
      z_vals <- eval_g_function_(model, grp, eval_data, response_idx = ri)
      z_mat <- matrix(z_vals, nrow = n_grid, ncol = n_grid)

      fig <- fig |> plotly::add_surface(
        x = x1_seq, y = x2_seq, z = z_mat,
        colorscale = list(c(0, "#2166AC"), c(0.5, "#f7f7f7"), c(1, "#B2182B")),
        opacity = 0.85,
        colorbar = list(title = paste("Contribution\nto", target_label)),
        name = lvl,
        visible = (i == 1L),
        showlegend = FALSE
      )
    }

    buttons <- lapply(seq_along(levels_in_data), function(i) {
      vis <- rep(FALSE, n_lvl)
      vis[i] <- TRUE
      list(method = "update",
           args = list(list(visible = as.list(vis))),
           label = levels_in_data[i])
    })

    return(
      fig |> plotly::layout(
        font = list(family = eui_font_family_()),
        title = list(text = title, font = list(family = eui_font_family_(),
                                                size = 14)),
        updatemenus = list(list(
          type = "dropdown", active = 0L,
          buttons = buttons,
          x = 0.0, xanchor = "left", y = 1.12, yanchor = "top"
        )),
        annotations = list(list(
          text = fvar, x = 0.0, xref = "paper", xanchor = "right",
          y = 1.12, yref = "paper", yanchor = "top",
          showarrow = FALSE, font = list(size = 12)
        )),
        scene = list(
          xaxis = list(title = var1),
          yaxis = list(title = var2),
          zaxis = list(title = paste("Contribution to", target_label))
        )
      )
    )
  }

  # --- No factors: single surface (original behavior) ---
  eval_data <- data[rep(1L, nrow(grid)), , drop = FALSE]
  eval_data[[var1]] <- grid$x1
  eval_data[[var2]] <- grid$x2
  z_vals <- eval_g_function_(model, grp, eval_data, response_idx = ri)
  z_mat <- matrix(z_vals, nrow = n_grid, ncol = n_grid)

  term_indices <- vapply(grp$terms, function(t) t$index, integer(1))
  bx <- model$bx
  coef_mat <- model$coefficients
  coefs <- if (multi) as.numeric(coef_mat[, ri]) else as.numeric(coef_mat)
  contrib <- rowSums(sweep(bx[, term_indices, drop = FALSE], 2,
                           coefs[term_indices], "*"))

  plotly::plot_ly() |>
    plotly::add_surface(
      x = x1_seq, y = x2_seq, z = z_mat,
      colorscale = list(c(0, "#2166AC"), c(0.5, "#f7f7f7"), c(1, "#B2182B")),
      opacity = 0.85,
      colorbar = list(title = paste("Contribution\nto", target_label)),
      name = "Surface",
      showlegend = FALSE
    ) |>
    plotly::add_markers(
      x = data[[var1]], y = data[[var2]], z = contrib,
      marker = list(size = 3, color = "#333333", opacity = 0.4),
      name = "Data"
    ) |>
    plotly::layout(
      font = list(family = eui_font_family_()),
      title = list(text = title, font = list(family = eui_font_family_(),
                                              size = 14)),
      scene = list(
        xaxis = list(title = var1),
        yaxis = list(title = var2),
        zaxis = list(title = paste("Contribution to", target_label))
      )
    )
}

# --- Internal: Static contour for d >= 2 (PDF/Word reports) ---
plot_g_contour_ <- function(earth_result, grp, response_idx = NULL) {
  model <- earth_result$model
  data <- earth_result$data
  targets <- earth_result$target
  ri <- if (is.null(response_idx)) 1L else response_idx
  target_label <- targets[ri]

  numeric_vars <- grp$base_vars[!grp$base_vars %in% earth_result$categoricals]
  numeric_vars <- numeric_vars[numeric_vars %in% names(data)]
  if (length(numeric_vars) < 2L) {
    return(plot_g_2d_(earth_result, grp, response_idx = ri))
  }

  var1 <- numeric_vars[1]
  var2 <- numeric_vars[2]
  title <- sprintf("%s  (%d term%s)", grp$label,
                   length(grp$terms), if (length(grp$terms) > 1L) "s" else "")

  # Detect factor variables in the group
  factor_vars <- grp$base_vars[grp$base_vars %in% earth_result$categoricals]
  factor_vars <- factor_vars[factor_vars %in% names(data)]

  n_grid <- 80L
  x1_seq <- seq(min(data[[var1]], na.rm = TRUE),
                max(data[[var1]], na.rm = TRUE), length.out = n_grid)
  x2_seq <- seq(min(data[[var2]], na.rm = TRUE),
                max(data[[var2]], na.rm = TRUE), length.out = n_grid)

  if (length(factor_vars) > 0L) {
    # Factor-aware: separate contour per factor level, faceted
    fvar <- factor_vars[1]
    levels_in_data <- sort(unique(as.character(data[[fvar]])))
    all_grids <- list()
    for (lvl in levels_in_data) {
      grid <- expand.grid(x1 = x1_seq, x2 = x2_seq)
      eval_data <- data[rep(1L, nrow(grid)), , drop = FALSE]
      eval_data[[var1]] <- grid$x1
      eval_data[[var2]] <- grid$x2
      eval_data[[fvar]] <- lvl
      grid$z <- eval_g_function_(model, grp, eval_data, response_idx = ri)
      grid$level <- lvl
      all_grids[[lvl]] <- grid
    }
    all_grid <- do.call(rbind, all_grids)

    return(
      ggplot2::ggplot(all_grid, ggplot2::aes(x = .data$x1, y = .data$x2)) +
        ggplot2::geom_contour_filled(ggplot2::aes(z = .data$z), bins = 15) +
        ggplot2::facet_wrap(~ level) +
        ggplot2::scale_x_continuous(labels = comma_format_) +
        ggplot2::scale_y_continuous(labels = comma_format_) +
        ggplot2::labs(title = title, x = var1, y = var2,
                      fill = paste("Contribution\nto", target_label)) +
        ggplot2::theme_minimal() +
        ggplot2::theme(
          plot.title = ggplot2::element_text(face = "bold", size = 13))
    )
  }

  # No factors: single contour
  grid <- expand.grid(x1 = x1_seq, x2 = x2_seq)
  eval_data <- data[rep(1L, nrow(grid)), , drop = FALSE]
  eval_data[[var1]] <- grid$x1
  eval_data[[var2]] <- grid$x2
  grid$z <- eval_g_function_(model, grp, eval_data, response_idx = ri)

  ggplot2::ggplot(grid, ggplot2::aes(x = .data$x1, y = .data$x2)) +
    ggplot2::geom_contour_filled(ggplot2::aes(z = .data$z), bins = 15) +
    ggplot2::scale_x_continuous(labels = comma_format_) +
    ggplot2::scale_y_continuous(labels = comma_format_) +
    ggplot2::labs(title = title, x = var1, y = var2,
                  fill = paste("Contribution\nto", target_label)) +
    ggplot2::theme_minimal() +
    ggplot2::theme(
      plot.title = ggplot2::element_text(face = "bold", size = 13)
    )
}

# --- Internal: Static 3D perspective for d >= 2 (PDF/Word reports) ---
plot_g_persp_ <- function(earth_result, grp, theta = 30, phi = 25,
                           response_idx = NULL) {
  model <- earth_result$model
  data <- earth_result$data
  targets <- earth_result$target
  ri <- if (is.null(response_idx)) 1L else response_idx
  target_label <- targets[ri]

  numeric_vars <- grp$base_vars[!grp$base_vars %in% earth_result$categoricals]
  numeric_vars <- numeric_vars[numeric_vars %in% names(data)]
  if (length(numeric_vars) < 2L) {
    return(plot_g_2d_(earth_result, grp, response_idx = ri))
  }

  var1 <- numeric_vars[1]
  var2 <- numeric_vars[2]
  title <- sprintf("%s  (%d term%s)", grp$label,
                   length(grp$terms), if (length(grp$terms) > 1L) "s" else "")

  # Detect factor variables in the group
  factor_vars <- grp$base_vars[grp$base_vars %in% earth_result$categoricals]
  factor_vars <- factor_vars[factor_vars %in% names(data)]

  n_grid <- 50L
  x1_seq <- seq(min(data[[var1]], na.rm = TRUE),
                max(data[[var1]], na.rm = TRUE), length.out = n_grid)
  x2_seq <- seq(min(data[[var2]], na.rm = TRUE),
                max(data[[var2]], na.rm = TRUE), length.out = n_grid)
  grid <- expand.grid(x1 = x1_seq, x2 = x2_seq)

  # Blue (#2166AC) -> White (#F7F7F7) -> Red (#B2182B)
  n_cols <- 256L
  col_ramp <- grDevices::colorRampPalette(
    c("#2166AC", "#F7F7F7", "#B2182B")
  )(n_cols)

  # Helper: draw one persp panel given a z matrix and subtitle
  draw_persp_ <- function(z_mat, subtitle) {
    z_range_mat <- range(z_mat, na.rm = TRUE)
    if (diff(z_range_mat) == 0) {
      # Flat surface (e.g. reference level) — add tiny offset so persp() works
      z_mat[1, 1] <- z_range_mat[1] + 1e-6
    }
    z_facet <- z_mat[-1, -1] + z_mat[-n_grid, -1] +
               z_mat[-1, -n_grid] + z_mat[-n_grid, -n_grid]
    z_facet <- z_facet / 4
    z_range <- range(z_facet, na.rm = TRUE)
    if (diff(z_range) == 0) {
      z_norm <- rep(0.5, length(z_facet))
    } else {
      z_norm <- (z_facet - z_range[1]) / diff(z_range)
    }
    facet_col <- col_ramp[pmax(1L, pmin(n_cols,
                          as.integer(z_norm * (n_cols - 1L)) + 1L))]
    graphics::persp(
      x = x1_seq, y = x2_seq, z = z_mat,
      theta = theta, phi = phi,
      col = facet_col, shade = 0.3, border = NA,
      xlab = var1, ylab = var2,
      zlab = paste("Contribution to", target_label),
      main = subtitle,
      cex.main = 1.0, font.main = 2,
      ticktype = "detailed", nticks = 5
    )
  }

  # Save and restore par
  old_par <- graphics::par(no.readonly = TRUE)
  on.exit(graphics::par(old_par), add = TRUE)
  graphics::par(family = eui_font_family_())

  if (length(factor_vars) > 0L) {
    # Factor-aware: side-by-side persp panels per factor level
    fvar <- factor_vars[1]
    levels_in_data <- sort(unique(as.character(data[[fvar]])))
    n_lvl <- length(levels_in_data)

    graphics::par(mfrow = c(1L, n_lvl), mar = c(2, 1, 3, 1), oma = c(0, 0, 2, 0))

    for (lvl in levels_in_data) {
      eval_data <- data[rep(1L, nrow(grid)), , drop = FALSE]
      eval_data[[var1]] <- grid$x1
      eval_data[[var2]] <- grid$x2
      eval_data[[fvar]] <- lvl
      z_vals <- eval_g_function_(model, grp, eval_data, response_idx = ri)
      z_mat <- matrix(z_vals, nrow = n_grid, ncol = n_grid)
      draw_persp_(z_mat, paste0(fvar, " = ", lvl))
    }

    graphics::mtext(title, outer = TRUE, cex = 1.1, font = 2)
  } else {
    # No factors: single panel
    graphics::par(mar = c(2, 2, 3, 2))

    eval_data <- data[rep(1L, nrow(grid)), , drop = FALSE]
    eval_data[[var1]] <- grid$x1
    eval_data[[var2]] <- grid$x2
    z_vals <- eval_g_function_(model, grp, eval_data, response_idx = ri)
    z_mat <- matrix(z_vals, nrow = n_grid, ncol = n_grid)
    draw_persp_(z_mat, title)
  }

  invisible(NULL)
}
