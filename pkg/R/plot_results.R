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
