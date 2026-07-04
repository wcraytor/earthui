# Internal: enlarge axis tick labels and titles to match the earthUI graphs
# (axis.text 15, axis.title 16) so single-panel plots are legible in both the
# app tabs and the report. gratia multi-panel plots (draw/appraise) keep their
# smaller base_size to avoid crowding.
mgcv_axis_fonts_ <- function() {
  ggplot2::theme(
    axis.text  = ggplot2::element_text(size = 15),
    axis.title = ggplot2::element_text(size = 16)
  )
}

#' Plot GAM Smooth Terms
#'
#' Produces ggplot2-based smooth plots for each term in the fitted GAM,
#' using the \pkg{gratia} package for tidy extraction.
#'
#' @param gam_result An `mgcvUI_result` from [fit_gam()].
#' @param residuals Logical. If `TRUE`, overlay partial residuals on
#'   smooth plots. Default `TRUE`.
#' @param select Optional integer vector selecting which smooth terms to
#'   plot (by index). `NULL` (the default) plots all smooth terms.
#' @return A ggplot object showing all smooth terms in a faceted layout.
#' @export
#' @examples
#' specs <- list(
#'   list(vars = "wt", type = "s", bs = "tp", k = NULL),
#'   list(vars = "hp", type = "s", bs = "tp", k = NULL)
#' )
#' res <- fit_gam(mtcars, "mpg", specs)
#' plot_smooths(res)
plot_smooths <- function(gam_result, residuals = TRUE, select = NULL) {
  # 2 panels per row so each smooth is wide enough to read (the default ~4-wide
  # layout makes the panels too narrow and the axis labels collide). `select`
  # (integer smooth indices) lets the report draw a page-sized subset so many
  # smooths render large across pages instead of being shrunk into one image.
  args <- list(object = gam_result$model, residuals = residuals, ncol = 2)
  if (!is.null(select)) args$select <- select
  p <- tryCatch(
    do.call(gratia::draw, args),
    error = function(e) gratia::draw(gam_result$model, residuals = residuals))
  # gratia::draw() returns a patchwork of panels. Apply theming to EVERY panel
  # with `&` (not `+`, which only hits the last panel). Keep gratia's default
  # right-hand colourbars — moving them to the bottom makes them tall and
  # squeezes the panels. Just give each panel title a little bottom margin so
  # it doesn't crowd the legend header. The report scales the figure height by
  # the number of smooths so the panels stay readable.
  tryCatch(
    p & ggplot2::theme_minimal(base_size = 12) &
      ggplot2::theme(
        plot.title = ggplot2::element_text(margin = ggplot2::margin(b = 6))),
    error = function(e) p + ggplot2::theme_minimal(base_size = 12))
}


#' Plot a Single GAM Smooth Term
#'
#' Plots one smooth term from the fitted GAM with confidence band and
#' optional partial residuals.
#'
#' @param gam_result An `mgcvUI_result` from [fit_gam()].
#' @param variable Character name of the variable to plot.
#' @param residuals Logical. Overlay partial residuals? Default `TRUE`.
#' @param earth_knots Optional `mgcvUI_earth_knots` object. If provided,
#'   earth knot positions are marked on the plot as vertical dashed
#'   lines.
#' @return A ggplot object.
#' @export
#' @examples
#' specs <- list(
#'   list(vars = "wt", type = "s", bs = "tp", k = NULL),
#'   list(vars = "hp", type = "s", bs = "tp", k = NULL)
#' )
#' res <- fit_gam(mtcars, "mpg", specs)
#' plot_smooth_single(res, "wt")
plot_smooth_single <- function(gam_result, variable, residuals = TRUE,
                               earth_knots = NULL) {
  model <- gam_result$model

  # Find the smooth index for this variable
  smooth_idx <- NULL
  for (i in seq_along(model$smooth)) {
    sm <- model$smooth[[i]]
    # Check $term (variable names) first, fall back to $vn for older mgcv
    sm_vars <- if (!is.null(sm$term)) sm$term else sm$vn
    if (variable %in% sm_vars && sm$dim == 1L && is.null(sm$by.level)) {
      smooth_idx <- i
      break
    }
  }
  if (is.null(smooth_idx)) {
    stop("No smooth term found for variable: ", variable, call. = FALSE)
  }

  sm_sel <- model$smooth[[smooth_idx]]$label
  sm_data <- gratia::smooth_estimates(model, select = sm_sel)

  # Comma-formatted number labels
  comma_fmt <- function(x) {
    formatC(round(x), format = "f", big.mark = ",", digits = 0)
  }

  # Map the colour/fill to descriptive labels so the plot carries a legend.
  # Title is intentionally omitted — the report adds a section heading for the
  # term, so a plot title would duplicate it.
  p <- ggplot2::ggplot(
    sm_data,
    ggplot2::aes(x = .data[[variable]], y = .data$.estimate)
  ) +
    ggplot2::geom_ribbon(
      ggplot2::aes(ymin = .data$.estimate - 1.96 * .data$.se,
                   ymax = .data$.estimate + 1.96 * .data$.se,
                   fill = "95% confidence band"),
      alpha = 0.2
    ) +
    ggplot2::geom_line(ggplot2::aes(color = "Smooth estimate"),
                       linewidth = 1.2) +
    ggplot2::scale_y_continuous(labels = comma_fmt) +
    ggplot2::labs(y = paste0("Contribution to ", gam_result$response),
                  x = variable) +
    ggplot2::theme_minimal(base_size = 15) +
    ggplot2::theme(
      axis.title = ggplot2::element_text(size = 16),
      axis.text = ggplot2::element_text(size = 15),
      legend.position = "bottom",
      legend.title = ggplot2::element_blank()
    )

  # Add partial residuals
  has_points <- FALSE
  if (residuals) {
    pres <- gratia::partial_residuals(model)
    smooth_label <- paste0("s(", variable, ")")
    if (smooth_label %in% names(pres)) {
      res_df <- data.frame(
        x = model$model[[variable]],
        y = pres[[smooth_label]]
      )
      p <- p + ggplot2::geom_point(
        data = res_df,
        ggplot2::aes(x = .data$x, y = .data$y, color = "Partial residuals"),
        alpha = 0.3, size = 1.5
      )
      has_points <- TRUE
    }
  }

  # Add earth knot positions (no legend entry — kept visually distinct).
  if (!is.null(earth_knots) && variable %in% names(earth_knots$knots)) {
    knot_df <- data.frame(xintercept = earth_knots$knots[[variable]])
    p <- p + ggplot2::geom_vline(
      data = knot_df,
      ggplot2::aes(xintercept = .data$xintercept),
      linetype = "dashed", color = "red", alpha = 0.6
    )
  }

  # Manual scales + per-key glyphs so each legend entry looks like its layer
  # (a line for the estimate, a point for the residuals).
  col_breaks <- c("Smooth estimate",
                  if (has_points) "Partial residuals")
  p <- p +
    ggplot2::scale_color_manual(
      name = NULL, breaks = col_breaks,
      values = c("Smooth estimate" = "steelblue",
                 "Partial residuals" = "grey40")) +
    ggplot2::scale_fill_manual(
      name = NULL, values = c("95% confidence band" = "steelblue")) +
    ggplot2::guides(
      color = ggplot2::guide_legend(order = 1, override.aes = list(
        linetype = c("solid", if (has_points) "blank"),
        shape    = c(NA,      if (has_points) 16))),
      fill = ggplot2::guide_legend(order = 2))

  p
}


#' Interactive Smooth Plot with Slope Tooltip
#'
#' Creates a plotly smooth plot showing contribution to value with
#' hover tooltips displaying the slope (rate of change per unit).
#' For latitude/longitude variables, slope is computed per 0.001 degrees.
#'
#' @param gam_result An `mgcvUI_result` from [fit_gam()].
#' @param variable Character name of the variable to plot.
#' @param earth_knots Optional `mgcvUI_earth_knots` object.
#' @param dark_mode Logical; use dark theme styling. Default `FALSE`.
#' @return A plotly htmlwidget.
#' @export
plot_smooth_interactive <- function(
    gam_result, variable, earth_knots = NULL, dark_mode = FALSE) {
  model <- gam_result$model
  resp <- gam_result$response
  xform <- gam_result$response_transform %||% "none"

  # Check smooth exists
  smooth_idx <- NULL
  for (i in seq_along(model$smooth)) {
    sm <- model$smooth[[i]]
    sm_vars <- if (!is.null(sm$term)) sm$term else sm$vn
    if (variable %in% sm_vars && sm$dim == 1L && is.null(sm$by.level)) {
      smooth_idx <- i
      break
    }
  }
  if (is.null(smooth_idx)) {
    stop("No smooth term found for variable: ", variable, call. = FALSE)
  }

  sm_sel <- model$smooth[[smooth_idx]]$label
  sm_data <- gratia::smooth_estimates(model, select = sm_sel)

  x <- sm_data[[variable]]
  y <- sm_data$.estimate
  se_val <- sm_data$.se

  # Diagnostic logging for unexpected X ranges
  message("  plot_smooth_interactive '", variable, "': x range = [",
          round(min(x), 2), ", ", round(max(x), 2), "], y range = [",
          round(min(y), 4), ", ", round(max(y), 4), "]")

  # Compute slope (dy/dx)
  dx <- diff(x)
  dy <- diff(y)
  raw_slope <- dy / dx

  # Smooth the slope slightly for display
  slope <- c(raw_slope[1], (raw_slope[-length(raw_slope)] + raw_slope[-1]) / 2,
             raw_slope[length(raw_slope)])

  # Determine slope unit
  is_coord <- grepl("lat|lon", variable, ignore.case = TRUE)
  if (is_coord) {
    slope_display <- slope * 0.001
    unit_label <- "/ 0.001\u00b0"
  } else {
    slope_display <- slope
    unit_label <- paste0("/ ", variable, " unit")
  }

  comma_fmt <- function(x) {
    formatC(round(x), format = "f", big.mark = ",", digits = 0)
  }

  # Back-transform contributions to dollar values when log transform is used.
  # The smooth contribution c is on log scale; the dollar effect relative to
  # the mean predicted price is: mean_price * (10^c - 1) for log10.
  if (xform == "log10") {
    mean_resp <- mean(model$fitted.values, na.rm = TRUE)
    mean_price <- 10^mean_resp
    y_display <- mean_price * (10^y - 1)
    ymin_display <- mean_price * (10^(y - 1.96 * se_val) - 1)
    ymax_display <- mean_price * (10^(y + 1.96 * se_val) - 1)
    y_title <- paste0("$ Contribution to ", resp)
    y_tickfmt <- "$,.0f"
    y_ticksuffix <- ""
  } else if (xform == "log") {
    mean_resp <- mean(model$fitted.values, na.rm = TRUE)
    mean_price <- exp(mean_resp)
    y_display <- mean_price * (exp(y) - 1)
    ymin_display <- mean_price * (exp(y - 1.96 * se_val) - 1)
    ymax_display <- mean_price * (exp(y + 1.96 * se_val) - 1)
    y_title <- paste0("$ Contribution to ", resp)
    y_tickfmt <- "$,.0f"
    y_ticksuffix <- ""
  } else {
    y_display <- y
    ymin_display <- y - 1.96 * se_val
    ymax_display <- y + 1.96 * se_val
    y_title <- paste0("Contribution to ", resp)
    y_tickfmt <- ",.0f"
    y_ticksuffix <- ""
  }

  # Build hover text
  hover_text <- paste0(
    variable, ": ", round(x, 4), "\n",
    "Contribution: $", comma_fmt(y_display), "\n",
    "95% CI: [$", comma_fmt(ymin_display), " , $",
    comma_fmt(ymax_display), "]\n",
    "Rate: $", comma_fmt(slope_display), " ", unit_label
  )

  fig <- plotly::plot_ly() |>
    plotly::add_ribbons(
      x = x, ymin = ymin_display, ymax = ymax_display,
      fillcolor = "rgba(70,130,180,0.2)",
      line = list(color = "transparent"),
      showlegend = FALSE,
      hoverinfo = "skip"
    ) |>
    plotly::add_lines(
      x = x, y = y_display,
      line = list(color = "steelblue", width = 2.5),
      text = hover_text,
      hoverinfo = "text",
      showlegend = FALSE
    )

  # Add partial residuals (only when no transform — back-transformed
  # residuals produce extreme outliers that dominate the axis)
  if (xform == "none") {
    pres <- gratia::partial_residuals(model)
    smooth_label <- paste0("s(", variable, ")")
    if (smooth_label %in% names(pres)) {
      scatter_x <- model$model[[variable]]
      scatter_y <- pres[[smooth_label]]
      message("  DEBUG scatter '", variable, "': x class=",
              paste(class(scatter_x), collapse="/"),
              " range=[", min(scatter_x, na.rm=TRUE), ",",
              max(scatter_x, na.rm=TRUE), "]",
              " y range=[", round(min(scatter_y, na.rm=TRUE)),
              ",", round(max(scatter_y, na.rm=TRUE)), "]")
      fig <- fig |>
        plotly::add_markers(
          x = scatter_x,
          y = scatter_y,
          marker = list(color = "grey", opacity = 0.3, size = 4),
          showlegend = FALSE,
          hoverinfo = "text",
          text = paste0(variable, "=", round(scatter_x, 1),
                        " resid=", round(scatter_y, 0))
        )
    }
  }

  # Add earth knot lines
  if (!is.null(earth_knots) && variable %in% names(earth_knots$knots)) {
    for (kv in earth_knots$knots[[variable]]) {
      fig <- fig |>
        plotly::add_segments(
          x = kv, xend = kv,
          y = min(ymin_display), yend = max(ymax_display),
          line = list(color = "red", dash = "dash", width = 1),
          opacity = 0.6, showlegend = FALSE,
          hoverinfo = "skip"
        )
    }
  }

  font_color <- if (dark_mode) "#d8dee9" else "#2e3440"
  grid_color <- if (dark_mode) "rgba(67,76,94,0.6)" else "rgba(0,0,0,0.1)"

  fig |>
    plotly::layout(
      title = list(text = variable,
                   font = list(size = 16, color = font_color)),
      xaxis = list(title = variable,
                   color = font_color, gridcolor = grid_color),
      yaxis = list(
        title = y_title,
        tickformat = y_tickfmt,
        ticksuffix = y_ticksuffix,
        color = font_color, gridcolor = grid_color
      ),
      font = list(color = font_color),
      hovermode = "x unified",
      margin = list(t = 50, b = 50),
      plot_bgcolor = "rgba(0,0,0,0)",
      paper_bgcolor = "rgba(0,0,0,0)"
    )
}


#' Interactive Factor-by-Smooth Contribution Plot
#'
#' Creates a plotly line plot for a factor-by-smooth term,
#' showing one contribution curve per factor level.
#'
#' @param gam_result An `mgcvUI_result` from [fit_gam()].
#' @param variable Character name of the continuous variable.
#' @param by_var Character name of the factor variable.
#' @param dark_mode Logical; use dark theme styling. Default `FALSE`.
#' @return A plotly htmlwidget.
#' @export
plot_by_smooth_interactive <- function(
    gam_result, variable, by_var, dark_mode = FALSE) {
  model <- gam_result$model
  resp <- gam_result$response
  xform <- gam_result$response_transform %||% "none"

  # Find all smooth indices for this variable with by levels
  smooth_indices <- integer(0)
  for (i in seq_along(model$smooth)) {
    sm <- model$smooth[[i]]
    sm_vars <- if (!is.null(sm$term)) sm$term else sm$vn
    if (variable %in% sm_vars && sm$dim == 1L && !is.null(sm$by.level)) {
      smooth_indices <- c(smooth_indices, i)
    }
  }
  if (length(smooth_indices) == 0L) {
    stop("No factor-by-smooth terms found for variable: ", variable,
         ", by: ", by_var, call. = FALSE)
  }

  # Nord frost palette for factor levels
  level_colors <- c("#5e81ac", "#88c0d0", "#81a1c1", "#8fbcbb",
                    "#a3be8c", "#ebcb8b", "#d08770", "#bf616a")

  font_color <- if (dark_mode) "#d8dee9" else "#2e3440"
  grid_color <- if (dark_mode) "rgba(67,76,94,0.6)" else "rgba(0,0,0,0.1)"

  fig <- plotly::plot_ly()

  for (j in seq_along(smooth_indices)) {
    idx <- smooth_indices[j]
    sm <- model$smooth[[idx]]
    sm_data <- gratia::smooth_estimates(model, select = sm$label)
    x <- sm_data[[variable]]
    y <- sm_data$.estimate
    se_val <- sm_data$.se
    level_label <- sm$by.level

    # Back-transform
    if (xform == "log10") {
      mean_resp <- mean(model$fitted.values, na.rm = TRUE)
      mean_price <- 10^mean_resp
      y_display <- mean_price * (10^y - 1)
    } else if (xform == "log") {
      mean_resp <- mean(model$fitted.values, na.rm = TRUE)
      mean_price <- exp(mean_resp)
      y_display <- mean_price * (exp(y) - 1)
    } else {
      y_display <- y
    }

    color <- level_colors[((j - 1L) %% length(level_colors)) + 1L]

    fig <- fig |>
      plotly::add_lines(
        x = x, y = y_display,
        line = list(color = color, width = 2),
        name = paste0(by_var, "=", level_label),
        showlegend = TRUE,
        hovertemplate = paste0(
          variable, ": %{x:.2f}<br>",
          "Contribution: %{y:,.0f}<br>",
          by_var, "=", level_label,
          "<extra></extra>"
        )
      )
  }

  if (xform %in% c("log", "log10")) {
    y_title <- paste0("$ Contribution to ", resp)
    y_tickfmt <- "$,.0f"
  } else {
    y_title <- paste0("Contribution to ", resp)
    y_tickfmt <- ",.0f"
  }

  title_text <- paste0("s(", variable, ", by=", by_var, ")")

  fig |>
    plotly::layout(
      title = list(text = title_text,
                   font = list(size = 16, color = font_color)),
      xaxis = list(title = variable,
                   color = font_color, gridcolor = grid_color),
      yaxis = list(title = y_title, tickformat = y_tickfmt,
                   color = font_color, gridcolor = grid_color),
      font = list(color = font_color),
      legend = list(font = list(color = font_color)),
      hovermode = "x unified",
      margin = list(t = 50, b = 50),
      plot_bgcolor = "rgba(0,0,0,0)",
      paper_bgcolor = "rgba(0,0,0,0)"
    )
}


#' Interactive Interaction Contribution Plot
#'
#' Creates a plotly heatmap for a 2D interaction smooth (ti or te term),
#' showing the contribution surface over a grid of the two variables.
#'
#' @param gam_result An `mgcvUI_result` from [fit_gam()].
#' @param vars Character vector of length 2 giving the variable names.
#' @param type Character; the smooth type (`"ti"` or `"te"`).
#' @param dark_mode Logical; use dark theme styling. Default `FALSE`.
#' @return A plotly htmlwidget.
#' @export
plot_interaction_interactive <- function(
    gam_result, vars, type = "ti", dark_mode = FALSE) {
  model <- gam_result$model
  resp <- gam_result$response
  xform <- gam_result$response_transform %||% "none"

  # Find the matching smooth
  smooth_idx <- NULL
  for (i in seq_along(model$smooth)) {
    sm <- model$smooth[[i]]
    sm_vars <- if (!is.null(sm$term)) sm$term else sm$vn
    if (sm$dim == 2L && setequal(sm_vars, vars)) {
      smooth_idx <- i
      break
    }
  }
  if (is.null(smooth_idx)) {
    stop("No 2D smooth found for variables: ",
         paste(vars, collapse = ", "), call. = FALSE)
  }

  sm_sel <- model$smooth[[smooth_idx]]$label
  sm_data <- gratia::smooth_estimates(model, select = sm_sel, n = 50)

  x1 <- sm_data[[vars[1]]]
  x2 <- sm_data[[vars[2]]]
  z  <- sm_data$.estimate

  # Back-transform to dollar scale when log transform is used
  if (xform == "log10") {
    mean_resp <- mean(model$fitted.values, na.rm = TRUE)
    mean_price <- 10^mean_resp
    z_display <- mean_price * (10^z - 1)
    z_title <- paste0("$ Contribution to ", resp)
  } else if (xform == "log") {
    mean_resp <- mean(model$fitted.values, na.rm = TRUE)
    mean_price <- exp(mean_resp)
    z_display <- mean_price * (exp(z) - 1)
    z_title <- paste0("$ Contribution to ", resp)
  } else {
    z_display <- z
    z_title <- paste0("Contribution to ", resp)
  }

  # Pivot to matrix for heatmap
  x1_vals <- sort(unique(x1))
  x2_vals <- sort(unique(x2))
  z_mat <- matrix(NA_real_, nrow = length(x2_vals), ncol = length(x1_vals))
  for (k in seq_along(x1)) {
    ci <- match(x1[k], x1_vals)
    ri <- match(x2[k], x2_vals)
    z_mat[ri, ci] <- z_display[k]
  }

  font_color <- if (dark_mode) "#d8dee9" else "#2e3440"
  title_text <- paste0(type, "(", paste(vars, collapse = ", "), ")")

  plotly::plot_ly(
    x = x1_vals, y = x2_vals, z = z_mat,
    type = "heatmap",
    colorscale = "RdBu", reversescale = TRUE,
    colorbar = list(title = list(text = z_title,
                                 font = list(color = font_color)),
                    tickfont = list(color = font_color)),
    hovertemplate = paste0(
      vars[1], ": %{x:.2f}<br>",
      vars[2], ": %{y:.2f}<br>",
      "Contribution: %{z:,.0f}<extra></extra>"
    )
  ) |>
    plotly::layout(
      title = list(text = title_text,
                   font = list(size = 16, color = font_color)),
      xaxis = list(title = vars[1],
                   color = font_color,
                   gridcolor = if (dark_mode) "rgba(67,76,94,0.6)"
                               else "rgba(0,0,0,0.1)"),
      yaxis = list(title = vars[2],
                   color = font_color,
                   gridcolor = if (dark_mode) "rgba(67,76,94,0.6)"
                               else "rgba(0,0,0,0.1)"),
      font = list(color = font_color),
      margin = list(t = 50, b = 50),
      plot_bgcolor = "rgba(0,0,0,0)",
      paper_bgcolor = "rgba(0,0,0,0)"
    )
}


#' Interactive Parametric Contribution Plot
#'
#' Creates a plotly plot for a parametric (linear or factor) model term,
#' showing its contribution to the predicted value. Numeric terms get a
#' scatter/line plot; factor terms get a bar chart.
#'
#' @param gam_result An `mgcvUI_result` from [fit_gam()].
#' @param term Character; the parametric term name as it appears in
#'   `predict(type = "terms")` column names.
#' @param dark_mode Logical; use dark theme styling. Default `FALSE`.
#' @return A plotly htmlwidget.
#' @export
plot_parametric_interactive <- function(
    gam_result, term, dark_mode = FALSE) {
  model <- gam_result$model
  resp <- gam_result$response
  xform <- gam_result$response_transform %||% "none"

  preds <- predict(model, type = "terms")
  if (!term %in% colnames(preds)) {
    stop("Term '", term, "' not found in model predictions.", call. = FALSE)
  }

  contrib <- as.numeric(preds[, term])
  mf <- model$model

  # Back-transform to dollar scale
  if (xform == "log10") {
    mean_resp <- mean(model$fitted.values, na.rm = TRUE)
    mean_price <- 10^mean_resp
    contrib_display <- mean_price * (10^contrib - 1)
    y_title <- paste0("$ Contribution to ", resp)
    y_tickfmt <- "$,.0f"
  } else if (xform == "log") {
    mean_resp <- mean(model$fitted.values, na.rm = TRUE)
    mean_price <- exp(mean_resp)
    contrib_display <- mean_price * (exp(contrib) - 1)
    y_title <- paste0("$ Contribution to ", resp)
    y_tickfmt <- "$,.0f"
  } else {
    contrib_display <- contrib
    y_title <- paste0("Contribution to ", resp)
    y_tickfmt <- ",.0f"
  }

  font_color <- if (dark_mode) "#d8dee9" else "#2e3440"
  grid_color <- if (dark_mode) "rgba(67,76,94,0.6)" else "rgba(0,0,0,0.1)"

  # Determine if term is numeric or factor
  is_factor <- term %in% names(mf) && is.factor(mf[[term]])

  if (is_factor) {
    # Bar chart: mean contribution per level
    lvls <- levels(mf[[term]])
    level_means <- vapply(lvls, function(lv) {
      mean(contrib_display[mf[[term]] == lv], na.rm = TRUE)
    }, numeric(1))

    fig <- plotly::plot_ly(
      x = lvls, y = level_means, type = "bar",
      marker = list(color = "#5e81ac"),
      hovertemplate = paste0(
        term, ": %{x}<br>",
        "Contribution: %{y:,.0f}<extra></extra>"
      )
    )
    x_title <- term
  } else {
    # Scatter plot: data points with linear fit line
    x_vals <- if (term %in% names(mf)) mf[[term]] else seq_along(contrib)

    # Sort for line
    ord <- order(x_vals)
    x_sorted <- x_vals[ord]
    y_sorted <- contrib_display[ord]

    fig <- plotly::plot_ly() |>
      plotly::add_markers(
        x = x_vals, y = contrib_display,
        marker = list(color = "#5e81ac", opacity = 0.5, size = 5),
        showlegend = FALSE,
        hovertemplate = paste0(
          term, ": %{x:.2f}<br>",
          "Contribution: %{y:,.0f}<extra></extra>"
        )
      ) |>
      plotly::add_lines(
        x = x_sorted, y = y_sorted,
        line = list(color = "#bf616a", width = 2),
        showlegend = FALSE,
        hoverinfo = "skip"
      )
    x_title <- term
  }

  fig |>
    plotly::layout(
      title = list(text = paste0(term, " (linear)"),
                   font = list(size = 16, color = font_color)),
      xaxis = list(title = x_title,
                   color = font_color, gridcolor = grid_color),
      yaxis = list(title = y_title, tickformat = y_tickfmt,
                   color = font_color, gridcolor = grid_color),
      font = list(color = font_color),
      margin = list(t = 50, b = 50),
      plot_bgcolor = "rgba(0,0,0,0)",
      paper_bgcolor = "rgba(0,0,0,0)"
    )
}


#' Static Interaction Contribution Plot
#'
#' Creates a ggplot heatmap for a 2D interaction smooth (ti or te term).
#' Suitable for PDF/Word reports.
#'
#' @param gam_result An `mgcvUI_result` from [fit_gam()].
#' @param vars Character vector of length 2 giving the variable names.
#' @param type Character; the smooth type (`"ti"` or `"te"`).
#' @return A ggplot object.
#' @export
plot_interaction_single <- function(gam_result, vars, type = "ti") {
  model <- gam_result$model

  smooth_idx <- NULL
  for (i in seq_along(model$smooth)) {
    sm <- model$smooth[[i]]
    sm_vars <- if (!is.null(sm$term)) sm$term else sm$vn
    if (sm$dim == 2L && setequal(sm_vars, vars)) {
      smooth_idx <- i
      break
    }
  }
  if (is.null(smooth_idx)) {
    stop("No 2D smooth found for variables: ",
         paste(vars, collapse = ", "), call. = FALSE)
  }

  sm_sel <- model$smooth[[smooth_idx]]$label
  sm_data <- gratia::smooth_estimates(model, select = sm_sel, n = 50)

  # Title omitted — the report adds a section heading for the term.
  ggplot2::ggplot(sm_data,
    ggplot2::aes(x = .data[[vars[1]]], y = .data[[vars[2]]],
                 fill = .data$.estimate)) +
    ggplot2::geom_tile() +
    ggplot2::scale_fill_gradient2(
      low = "#2166AC", mid = "white", high = "#B2182B",
      midpoint = 0, name = "Contribution") +
    ggplot2::labs(x = vars[1], y = vars[2]) +
    ggplot2::theme_minimal(base_size = 15) +
    ggplot2::theme(
      legend.position = "bottom",
      legend.key.width = ggplot2::unit(1.5, "cm")
    ) +
    mgcv_axis_fonts_()
}


#' Static Factor-by-Smooth Contribution Plot
#'
#' Creates a ggplot line plot for a factor-by-smooth term,
#' showing one curve per factor level.
#'
#' @param gam_result An `mgcvUI_result` from [fit_gam()].
#' @param variable Character name of the continuous variable.
#' @param by_var Character name of the factor variable.
#' @return A ggplot object.
#' @export
plot_by_smooth_single <- function(gam_result, variable, by_var) {
  model <- gam_result$model

  smooth_indices <- integer(0)
  for (i in seq_along(model$smooth)) {
    sm <- model$smooth[[i]]
    sm_vars <- if (!is.null(sm$term)) sm$term else sm$vn
    if (variable %in% sm_vars && sm$dim == 1L && !is.null(sm$by.level)) {
      smooth_indices <- c(smooth_indices, i)
    }
  }
  if (length(smooth_indices) == 0L) {
    stop("No factor-by-smooth terms found for: ", variable, call. = FALSE)
  }

  all_data <- do.call(rbind, lapply(smooth_indices, function(idx) {
    sm <- model$smooth[[idx]]
    sm_data <- gratia::smooth_estimates(model, select = sm$label)
    sm_data$level <- sm$by.level
    sm_data
  }))

  # Title omitted — the report adds a section heading for the term.
  ggplot2::ggplot(all_data,
    ggplot2::aes(x = .data[[variable]], y = .data$.estimate,
                 color = .data$level)) +
    ggplot2::geom_line(linewidth = 1) +
    ggplot2::labs(x = variable,
                  y = paste0("Contribution to ", gam_result$response),
                  color = by_var) +
    ggplot2::theme_minimal(base_size = 15) +
    ggplot2::theme(legend.position = "bottom") +
    mgcv_axis_fonts_()
}


#' Static Parametric Contribution Plot
#'
#' Creates a ggplot for a parametric model term. Numeric terms get a
#' scatter plot; factor terms get a bar chart.
#'
#' @param gam_result An `mgcvUI_result` from [fit_gam()].
#' @param term Character; the parametric term name.
#' @return A ggplot object.
#' @export
plot_parametric_single <- function(gam_result, term) {
  model <- gam_result$model
  resp <- gam_result$response

  preds <- predict(model, type = "terms")
  if (!term %in% colnames(preds)) {
    stop("Term '", term, "' not found in model predictions.", call. = FALSE)
  }

  contrib <- as.numeric(preds[, term])
  mf <- model$model
  is_factor <- term %in% names(mf) && is.factor(mf[[term]])

  if (is_factor) {
    lvls <- levels(mf[[term]])
    level_means <- vapply(lvls, function(lv) {
      mean(contrib[mf[[term]] == lv], na.rm = TRUE)
    }, numeric(1))
    df <- data.frame(level = factor(lvls, levels = lvls), contrib = level_means)
    ggplot2::ggplot(df, ggplot2::aes(x = .data$level, y = .data$contrib)) +
      ggplot2::geom_col(fill = "#5e81ac", alpha = 0.85) +
      ggplot2::labs(title = paste0(term, " (factor)"),
                    x = term, y = paste0("Contribution to ", resp)) +
      ggplot2::theme_minimal(base_size = 15) +
      ggplot2::theme(plot.title = ggplot2::element_text(face = "bold", size = 16)) +
      mgcv_axis_fonts_()
  } else {
    x_vals <- if (term %in% names(mf)) mf[[term]] else seq_along(contrib)
    df <- data.frame(x = x_vals, contrib = contrib)
    ord <- order(df$x)
    ggplot2::ggplot(df, ggplot2::aes(x = .data$x, y = .data$contrib)) +
      ggplot2::geom_point(color = "#5e81ac", alpha = 0.5, size = 1.5) +
      ggplot2::geom_line(data = df[ord, ], color = "#bf616a", linewidth = 1) +
      ggplot2::labs(title = paste0(term, " (linear)"),
                    x = term, y = paste0("Contribution to ", resp)) +
      ggplot2::theme_minimal(base_size = 15) +
      ggplot2::theme(plot.title = ggplot2::element_text(face = "bold", size = 16)) +
      mgcv_axis_fonts_()
  }
}


#' Plot GAM Residual Diagnostics
#'
#' Produces a 2x2 panel of diagnostic plots: residuals vs fitted,
#' Q-Q plot, histogram of residuals, and response vs fitted.
#'
#' @param gam_result An `mgcvUI_result` from [fit_gam()].
#' @return A ggplot object (patchwork-assembled if available, otherwise
#'   individual plots returned as a list).
#' @export
#' @examples
#' specs <- list(list(vars = "wt", type = "s", bs = "tp", k = NULL))
#' res <- fit_gam(mtcars, "mpg", specs)
#' plot_diagnostics(res)
plot_diagnostics <- function(gam_result) {
  tryCatch(
    # appraise() returns a patchwork of four panels; theme with `&` so every
    # panel is styled (`+` hits only the last one). Axis fonts sized for a
    # 2x2 grid — a step below the single-panel 15/16 standard to avoid
    # crowding, but far above ggplot's ~9pt default the tab used to show.
    gratia::appraise(gam_result$model) &
      ggplot2::theme_minimal(base_size = 13) &
      ggplot2::theme(
        axis.text  = ggplot2::element_text(size = 13),
        axis.title = ggplot2::element_text(size = 14)
      ),
    error = function(e) {
      message("gratia::appraise failed: ", e$message,
              "; returning placeholder")
      ggplot2::ggplot() +
        ggplot2::annotate("text", x = 0.5, y = 0.5,
          label = paste0("Diagnostics plot unavailable:\n", e$message,
                         "\n\nUse mgcv::gam.check() in the console."),
          size = 4, hjust = 0.5) +
        ggplot2::theme_void()
    }
  )
}


#' Plot Actual vs Predicted
#'
#' @param gam_result An `mgcvUI_result` from [fit_gam()].
#' @return A ggplot object.
#' @export
#' @examples
#' specs <- list(list(vars = "wt", type = "s", bs = "tp", k = NULL))
#' res <- fit_gam(mtcars, "mpg", specs)
#' plot_actual_vs_predicted_mgcv(res)
plot_actual_vs_predicted_mgcv <- function(gam_result) {
  model <- gam_result$model
  xform <- gam_result$response_transform %||% "none"

  actual_raw <- model$model[[gam_result$response]]
  predicted_raw <- fitted(model)

  # Back-transform to original scale for display
  df <- data.frame(
    actual    = back_transform_(actual_raw, xform),
    predicted = back_transform_(predicted_raw, xform)
  )

  ggplot2::ggplot(df, ggplot2::aes(x = .data$predicted,
                                   y = .data$actual)) +
    ggplot2::geom_point(alpha = 0.5, color = "steelblue") +
    ggplot2::geom_abline(intercept = 0, slope = 1,
                         linetype = "dashed", color = "grey40") +
    ggplot2::labs(x = "Predicted", y = "Actual",
                  title = "Actual vs Predicted (original scale)") +
    ggplot2::theme_minimal(base_size = 15) +
    mgcv_axis_fonts_()
}
