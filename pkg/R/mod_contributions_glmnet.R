#' Contributions Module UI
#'
#' @param id Module namespace ID.
#' @return A Shiny tagList.
#' @export
contributionsUI <- function(id) {
  ns <- shiny::NS(id)
  shiny::tagList(
    shiny::selectInput(ns("contrib_var"), "Select Variable", choices = NULL),
    shiny::uiOutput(ns("contrib_plot_ui")),
    shiny::uiOutput(ns("contrib_persp_ui"))
  )
}

# Internal: choose decimal places based on axis range
#' @noRd
contrib_auto_digits_ <- function(x) {
  x <- x[is.finite(x)]
  if (length(x) < 2L) return(0L)
  rng <- diff(range(x))
  if (rng < 1) 3L
  else if (rng < 10) 2L
  else if (rng < 100) 1L
  else 0L
}

# Internal: format slope label adapting units to x-axis range
#' @noRd
contrib_slope_label_ <- function(slope, x_breaks) {
  d <- contrib_auto_digits_(x_breaks)
  bm <- locale_big_mark_()
  dm <- locale_dec_mark_()
  unit_size <- 10^(-d)
  scaled <- slope * unit_size
  unit_label <- if (d == 0L) "/unit" else paste0(
    "/", format(unit_size, scientific = FALSE))
  paste0(
    ifelse(scaled >= 0, "+", "-"),
    formatC(abs(scaled), format = "f", digits = 2,
            big.mark = bm, decimal.mark = dm),
    unit_label
  )
}

#' Contributions Module Server
#'
#' @param id Module namespace ID.
#' @param model_module Reactive list from [modelingServer()].
#' @param data_module Reactive list from [dataImportServer()].
#' @return No return value, called for side effects (renders UI outputs).
#' @export
contributionsServer <- function(id, model_module, data_module) {
  shiny::moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # Build per-variable contributions reactive
    contrib_data <- shiny::reactive({
      shiny::req(model_module$fitted())
      model_module$fit_count()

      model  <- model_module$model()
      lambda <- model_module$lambda()
      gamma  <- model_module$gamma()
      x_mat  <- model_module$x_matrix()
      preds  <- data_module$predictors()
      df     <- data_module$data()

      # Extract coefficients (excluding intercept)
      coef_args <- list(model, s = lambda)
      if (!is.null(gamma)) coef_args$gamma <- gamma
      coef_sparse <- do.call(stats::coef, coef_args)
      coef_vec <- as.numeric(coef_sparse)[-1L]
      coef_names <- rownames(coef_sparse)[-1L]
      names(coef_vec) <- coef_names
      intercept <- as.numeric(coef_sparse)[1L]

      # Map model matrix columns to original predictor names.
      # Map each column to its parent predictor.
      # With earth basis, columns include h() hinges, factor dummies,
      # and interaction products. Use earth's predictor list for
      # factor dummy prefix matching.
      col_to_pred <- character(length(coef_names))

      hinge_var_ <- function(h) {
        inner <- sub("^h\\(", "", sub("\\)$", "", h))
        # Handle negative knots: h(-121.45-var) has inner "-121.45-var"
        for (p in parent_preds_sorted) {
          if (grepl(p, inner, fixed = TRUE)) return(p)
        }
        parts <- strsplit(inner, "-", fixed = TRUE)[[1]]
        parts <- parts[nzchar(parts)]
        for (pt in parts) {
          if (suppressWarnings(is.na(as.numeric(pt)))) return(pt)
        }
        inner
      }

      # Use earth predictors if available, otherwise glmnetUI predictors
      earth_import <- model_module$earth_import()
      parent_preds <- if (!is.null(earth_import)) {
        earth_import$predictors
      } else {
        preds
      }
      # Sort by length descending so longer names match first
      parent_preds_sorted <- parent_preds[order(-nchar(parent_preds))]

      component_parent_ <- function(comp) {
        if (grepl("^h[(]", comp)) return(hinge_var_(comp))
        for (p in parent_preds_sorted) {
          if (startsWith(comp, p)) return(p)
        }
        comp
      }

      for (cn in coef_names) {
        idx <- which(coef_names == cn)
        # Split on both * (earth format) and : (formula interaction format)
        components <- strsplit(cn, "[*:]")[[1]]
        parent_vars <- vapply(components, component_parent_, character(1))
        parent_vars <- unique(parent_vars)
        if (length(parent_vars) == 1L) {
          col_to_pred[idx] <- parent_vars
        } else {
          col_to_pred[idx] <- paste(sort(parent_vars), collapse = ":")
        }
      }

      # Compute contribution per predictor: sum of beta_k * x_k for all
      # model matrix columns belonging to that predictor
      unique_preds <- unique(col_to_pred)
      unique_preds <- unique_preds[nzchar(unique_preds)]

      contribs <- list()
      for (pred_name in unique_preds) {
        idx <- which(col_to_pred == pred_name)
        if (length(idx) == 0) next
        if (all(coef_vec[idx] == 0)) next
        contrib_vec <- rep(0, nrow(x_mat))
        for (j in idx) {
          contrib_vec <- contrib_vec + coef_vec[j] * x_mat[, j]
        }
        contribs[[pred_name]] <- contrib_vec
      }

      # For the x-axis, use original data values for numeric predictors.
      # With earth basis, x_mat has hinge columns, not raw predictors,
      # so we look up the original variable from the appropriate source.
      x_values <- list()
      slopes <- list()
      # Determine source for raw predictor values
      earth_data <- NULL
      earth_import <- model_module$earth_import()
      if (!is.null(earth_import)) earth_data <- earth_import$data

      # Build row mask to align df rows with x_mat rows.
      # x_mat has only complete-case, non-zero-weight rows.
      n_xmat <- nrow(x_mat)
      df_aligned <- NULL
      if (nrow(df) == n_xmat) {
        df_aligned <- df
      } else {
        # Reconstruct row mask: complete cases of predictor + response columns
        resp <- setdiff(names(df), preds)
        resp <- resp[resp %in% names(df)][1]  # first non-predictor = response
        use_cols <- intersect(c(resp, preds), names(df))
        mask <- stats::complete.cases(df[, use_cols, drop = FALSE])
        if (sum(mask) == n_xmat) {
          df_aligned <- df[mask, , drop = FALSE]
        } else if (sum(mask) > n_xmat) {
          # Also exclude weight=0 rows
          df_sub <- df[mask, , drop = FALSE]
          # Try to find weight column
          wt_candidates <- c("weight", "wt")
          for (wc in names(df_sub)) {
            if (wc %in% wt_candidates || identical(wc, "weight")) {
              wt_vals <- as.numeric(df_sub[[wc]])
              keep <- !is.na(wt_vals) & wt_vals != 0
              if (sum(keep) == n_xmat) {
                df_aligned <- df_sub[keep, , drop = FALSE]
                break
              }
            }
          }
        }
      }

      fac_vars_check <- data_module$fac_vars()
      # Earth model knows which variables are factors via xlevels
      if (!is.null(earth_import) && !is.null(earth_import$model$xlevels)) {
        fac_vars_check <- union(fac_vars_check,
                                names(earth_import$model$xlevels))
      }
      for (pred_name in unique_preds) {
        if (grepl(":", pred_name, fixed = TRUE)) next
        # Skip factor variables — they get box plots, not scatter
        if (pred_name %in% fac_vars_check) next
        idx <- which(col_to_pred == pred_name)
        if (length(idx) == 1L && !grepl("h\\(", coef_names[idx])) {
          # Single formula column = numeric predictor
          x_values[[pred_name]] <- x_mat[, idx]
          slopes[[pred_name]] <- coef_vec[idx]
        } else if (pred_name %in% names(df) && is.numeric(df[[pred_name]])) {
          # Multi-column predictor: use aligned data for x-axis
          if (!is.null(df_aligned)) {
            x_values[[pred_name]] <- df_aligned[[pred_name]]
          }
        }
      }

      fac_vars <- data_module$fac_vars()
      if (!is.null(earth_import) && !is.null(earth_import$model$xlevels)) {
        fac_vars <- union(fac_vars, names(earth_import$model$xlevels))
      }

      list(
        contribs    = contribs,
        x_values    = x_values,
        slopes      = slopes,
        intercept   = intercept,
        preds       = unique_preds,
        col_to_pred = col_to_pred,
        df_aligned  = df_aligned,
        fac_vars    = fac_vars
      )
    })

    # Update variable selector when model changes
    shiny::observeEvent(contrib_data(), {
      cd <- contrib_data()
      shiny::updateSelectInput(session, "contrib_var",
                               choices = cd$preds,
                               selected = cd$preds[1L])
    })

    # Dynamic plot height: taller for interactions (two stacked plots)
    output$contrib_plot_ui <- shiny::renderUI({
      var_name <- input$contrib_var
      h <- if (!is.null(var_name) && grepl(":", var_name, fixed = TRUE)) {
        "1200px"
      } else {
        "450px"
      }
      shiny::plotOutput(ns("contrib_plot"), height = h)
    })

    d_ <- plot_dims_(session, "contrib_plot")
    output$contrib_plot <- shiny::renderPlot({
      shiny::req(contrib_data(), input$contrib_var)

      cd <- contrib_data()
      var_name <- input$contrib_var
      shiny::req(var_name %in% names(cd$contribs))

      contrib_vec <- cd$contribs[[var_name]]
      font_fam <- glmnet_font_family_()

      # Use model matrix x-values for numeric predictors (perfectly aligned)
      if (var_name %in% names(cd$x_values)) {
        x_vals <- cd$x_values[[var_name]]
        plot_df <- data.frame(x = x_vals, contribution = contrib_vec)

        slope_val <- cd$slopes[[var_name]]

        p <- ggplot2::ggplot(plot_df,
                             ggplot2::aes(x = .data$x,
                                          y = .data$contribution)) +
          ggplot2::geom_point(ggplot2::aes(color = "Observations"),
                              alpha = 0.3, size = 1)

        if (!is.null(slope_val) && !is.na(slope_val) && slope_val != 0) {
          # Single-coefficient predictor: draw exact linear line
          x_range <- range(x_vals, na.rm = TRUE)
          line_df <- data.frame(
            x = x_range,
            contribution = slope_val * x_range
          )
          slope_lbl <- contrib_slope_label_(slope_val, x_vals)
          x_mid <- mean(x_range)
          y_mid <- slope_val * x_mid
          label_df <- data.frame(x = x_mid, y = y_mid, label = slope_lbl)

          p <- p +
            ggplot2::geom_line(data = line_df,
                               ggplot2::aes(color = "Coefficient line"),
                               linewidth = 1) +
            ggplot2::geom_label(
              data = label_df,
              ggplot2::aes(x = .data$x, y = .data$y, label = .data$label),
              inherit.aes = FALSE,
              size = 4, fill = "white", alpha = 0.85,
              label.padding = ggplot2::unit(0.2, "lines"),
              label.size = 0.3, color = "#333333",
              family = font_fam
            )
        } else {
          # Multi-column predictor: use OLS fit line through contributions
          p <- p +
            ggplot2::geom_smooth(method = "lm", formula = y ~ x,
                                  ggplot2::aes(color = "Fitted trend"),
                                  linewidth = 1, se = FALSE)
        }

        if (!is.null(slope_val) && !is.na(slope_val) && slope_val != 0) {
          p <- p +
            ggplot2::scale_color_manual(
              name = NULL,
              values = c("Observations" = "#5e81ac",
                         "Coefficient line" = "#bf616a"),
              guide = ggplot2::guide_legend(
                override.aes = list(
                  shape = c(16, NA),
                  linetype = c(NA, 1),
                  linewidth = c(NA, 1),
                  alpha = c(0.6, 1)
                )
              )
            )
        } else {
          p <- p +
            ggplot2::scale_color_manual(
              name = NULL,
              values = c("Observations" = "#5e81ac",
                         "Fitted trend" = "#bf616a"),
              guide = ggplot2::guide_legend(
                override.aes = list(
                  shape = c(16, NA),
                  linetype = c(NA, 1),
                  linewidth = c(NA, 1),
                  alpha = c(0.6, 1)
                )
              )
            )
        }

        p +
          ggplot2::labs(
            title = paste("Contribution:", var_name),
            subtitle = paste0("Intercept (basis): ",
                              round(cd$intercept, 2)),
            x = var_name,
            y = "Contribution to Prediction"
          ) +
          ggplot2::scale_x_continuous(labels = glmnet_axis_labels_,
                                      n.breaks = 20) +
          ggplot2::scale_y_continuous(labels = glmnet_axis_labels_,
                                      n.breaks = 15) +
          glmnet_diag_theme_(font_fam) +
          ggplot2::theme(
            plot.subtitle = ggplot2::element_text(size = 13)
          )
      } else if (grepl(":", var_name, fixed = TRUE) &&
                 !is.null(cd$df_aligned)) {
        # Interaction: scatter plot colored by second variable + heatmap
        int_vars <- strsplit(var_name, ":", fixed = TRUE)[[1]]
        dfa <- cd$df_aligned
        var1 <- int_vars[1]
        var2 <- if (length(int_vars) >= 2) int_vars[2] else NULL

        if (!is.null(var2) &&
            var1 %in% names(dfa) && var2 %in% names(dfa) &&
            is.numeric(dfa[[var1]]) && is.numeric(dfa[[var2]])) {
          # Both numeric: scatter + heatmap (3D persp in separate output)
          x1_vals <- dfa[[var1]]
          x2_vals <- dfa[[var2]]

          plot_df <- data.frame(x = x1_vals, y = x2_vals,
                                contribution = contrib_vec)

          p1 <- ggplot2::ggplot(plot_df,
                   ggplot2::aes(x = .data$x, y = .data$y,
                                color = .data$contribution)) +
            ggplot2::geom_point(alpha = 0.6, size = 1.5) +
            ggplot2::scale_color_gradient2(
              low = "#2166AC", mid = "#d0d0d0", high = "#B2182B",
              midpoint = 0, name = "Contrib.") +
            ggplot2::labs(
              title = paste("Scatter:", var1, "\u00d7", var2),
              x = var1, y = var2) +
            ggplot2::scale_x_continuous(labels = glmnet_axis_labels_) +
            ggplot2::scale_y_continuous(labels = glmnet_axis_labels_) +
            glmnet_diag_theme_(font_fam)

          p2 <- ggplot2::ggplot(plot_df,
                   ggplot2::aes(x = .data$x, y = .data$y,
                                z = .data$contribution)) +
            ggplot2::stat_summary_2d(bins = 20, fun = mean) +
            ggplot2::scale_fill_gradient2(
              low = "#2166AC", mid = "#d0d0d0", high = "#B2182B",
              midpoint = 0, name = "Mean\nContrib.") +
            ggplot2::labs(
              title = paste("Heatmap:", var1, "\u00d7", var2),
              x = var1, y = var2) +
            ggplot2::scale_x_continuous(labels = glmnet_axis_labels_) +
            ggplot2::scale_y_continuous(labels = glmnet_axis_labels_) +
            glmnet_diag_theme_(font_fam)

          print(p1)
          print(p2)
        } else {
          # One or both non-numeric: scatter vs the numeric one
          num_var <- if (var1 %in% names(dfa) && is.numeric(dfa[[var1]])) var1
                     else if (!is.null(var2) && var2 %in% names(dfa) &&
                              is.numeric(dfa[[var2]])) var2
                     else NULL
          if (!is.null(num_var)) {
            plot_df <- data.frame(x = dfa[[num_var]],
                                  contribution = contrib_vec)
            ggplot2::ggplot(plot_df,
                            ggplot2::aes(x = .data$x,
                                         y = .data$contribution)) +
              ggplot2::geom_point(alpha = 0.3, color = "#5e81ac", size = 1) +
              ggplot2::geom_smooth(method = "lm", formula = y ~ x,
                                    color = "#bf616a", se = FALSE) +
              ggplot2::labs(
                title = paste("Interaction:", var_name),
                x = num_var, y = "Contribution"
              ) +
              glmnet_diag_theme_(font_fam)
          } else {
            # No numeric variable available
            plot_df <- data.frame(contribution = contrib_vec)
            ggplot2::ggplot(plot_df,
                            ggplot2::aes(x = .data$contribution)) +
              ggplot2::geom_histogram(bins = 30, fill = "#5e81ac",
                                       color = "white") +
              ggplot2::labs(title = paste("Contribution:", var_name),
                            x = "Contribution", y = "Count") +
              glmnet_diag_theme_(font_fam)
          }
        }
      } else if (!is.null(cd$df_aligned) &&
                 var_name %in% names(cd$df_aligned) &&
                 (is.factor(cd$df_aligned[[var_name]]) ||
                  var_name %in% cd$fac_vars)) {
        # Factor: box plot by level, sorted by median contribution
        plot_df <- data.frame(
          level = as.factor(cd$df_aligned[[var_name]]),
          contribution = contrib_vec
        )
        plot_df$level <- stats::reorder(plot_df$level, plot_df$contribution,
                                         FUN = stats::median)
        ggplot2::ggplot(plot_df,
                        ggplot2::aes(x = .data$level,
                                     y = .data$contribution)) +
          ggplot2::geom_boxplot(fill = "#5e81ac", alpha = 0.6) +
          ggplot2::labs(
            title = paste("Contribution:", var_name),
            x = var_name, y = "Contribution"
          ) +
          ggplot2::scale_y_continuous(labels = glmnet_axis_labels_) +
          glmnet_diag_theme_(font_fam) +
          ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 45, hjust = 1))
      } else {
        # Fallback: histogram
        plot_df <- data.frame(contribution = contrib_vec)
        ggplot2::ggplot(plot_df,
                        ggplot2::aes(x = .data$contribution)) +
          ggplot2::geom_histogram(bins = 30, fill = "#5e81ac",
                                   color = "white", alpha = 0.8) +
          ggplot2::labs(
            title = paste("Contribution:", var_name),
            x = "Contribution", y = "Count"
          ) +
          ggplot2::scale_x_continuous(labels = glmnet_axis_labels_) +
          glmnet_diag_theme_(font_fam)
      }
    }, width = d_$width, height = d_$height, res = 96)

    # 3D surface for interactions: plotly if available, else static persp
    output$contrib_persp_ui <- shiny::renderUI({
      var_name <- input$contrib_var
      if (is.null(var_name) || !grepl(":", var_name, fixed = TRUE)) return(NULL)
      if (requireNamespace("plotly", quietly = TRUE)) {
        plotly::plotlyOutput(ns("contrib_3d"), height = "500px")
      } else {
        shiny::plotOutput(ns("contrib_persp_static"), height = "400px")
      }
    })

    if (requireNamespace("plotly", quietly = TRUE)) {
      output$contrib_3d <- plotly::renderPlotly({
        shiny::req(contrib_data(), input$contrib_var)
        cd <- contrib_data()
        var_name <- input$contrib_var
        if (!grepl(":", var_name, fixed = TRUE)) return(NULL)
        shiny::req(var_name %in% names(cd$contribs))
        shiny::req(!is.null(cd$df_aligned))

        int_vars <- strsplit(var_name, ":", fixed = TRUE)[[1]]
        dfa <- cd$df_aligned
        var1 <- int_vars[1]
        var2 <- if (length(int_vars) >= 2) int_vars[2] else NULL
        if (is.null(var2)) return(NULL)
        if (!var1 %in% names(dfa) || !var2 %in% names(dfa)) return(NULL)
        if (!is.numeric(dfa[[var1]]) || !is.numeric(dfa[[var2]])) return(NULL)

        x1_vals <- dfa[[var1]]
        x2_vals <- dfa[[var2]]
        contrib_vec <- cd$contribs[[var_name]]

        x1_seq <- seq(min(x1_vals, na.rm = TRUE),
                      max(x1_vals, na.rm = TRUE), length.out = 30)
        x2_seq <- seq(min(x2_vals, na.rm = TRUE),
                      max(x2_vals, na.rm = TRUE), length.out = 30)

        surf_df <- data.frame(x1 = x1_vals, x2 = x2_vals, z = contrib_vec)
        surf_fit <- tryCatch(
          stats::loess(z ~ x1 * x2, data = surf_df, span = 0.75),
          error = function(e) NULL)
        if (is.null(surf_fit)) return(NULL)

        grid_df <- expand.grid(x1 = x1_seq, x2 = x2_seq)
        z_mat <- tryCatch(
          matrix(stats::predict(surf_fit, grid_df), nrow = length(x1_seq)),
          error = function(e) NULL)
        if (is.null(z_mat)) return(NULL)

        plotly::plot_ly(x = x1_seq, y = x2_seq, z = z_mat,
                        type = "surface",
                        colorscale = list(c(0, "#2166AC"), c(0.5, "#d0d0d0"),
                                          c(1, "#B2182B"))) |>
          plotly::layout(
            title = paste("3D Surface:", var1, "\u00d7", var2),
            scene = list(
              xaxis = list(title = var1),
              yaxis = list(title = var2),
              zaxis = list(title = "Contribution")
            )
          )
      })
    }

    d_ <- plot_dims_(session, "contrib_persp_static")
    output$contrib_persp_static <- shiny::renderPlot({
      shiny::req(contrib_data(), input$contrib_var)
      cd <- contrib_data()
      var_name <- input$contrib_var
      if (!grepl(":", var_name, fixed = TRUE)) return(NULL)
      shiny::req(var_name %in% names(cd$contribs))
      shiny::req(!is.null(cd$df_aligned))

      int_vars <- strsplit(var_name, ":", fixed = TRUE)[[1]]
      dfa <- cd$df_aligned
      var1 <- int_vars[1]
      var2 <- if (length(int_vars) >= 2) int_vars[2] else NULL
      if (is.null(var2)) return(NULL)
      if (!var1 %in% names(dfa) || !var2 %in% names(dfa)) return(NULL)
      if (!is.numeric(dfa[[var1]]) || !is.numeric(dfa[[var2]])) return(NULL)

      x1_vals <- dfa[[var1]]
      x2_vals <- dfa[[var2]]
      contrib_vec <- cd$contribs[[var_name]]

      x1_seq <- seq(min(x1_vals, na.rm = TRUE),
                    max(x1_vals, na.rm = TRUE), length.out = 30)
      x2_seq <- seq(min(x2_vals, na.rm = TRUE),
                    max(x2_vals, na.rm = TRUE), length.out = 30)

      surf_df <- data.frame(x1 = x1_vals, x2 = x2_vals, z = contrib_vec)
      surf_fit <- tryCatch(
        stats::loess(z ~ x1 * x2, data = surf_df, span = 0.75),
        error = function(e) NULL)
      if (is.null(surf_fit)) return(NULL)

      grid_df <- expand.grid(x1 = x1_seq, x2 = x2_seq)
      z_mat <- tryCatch(
        matrix(stats::predict(surf_fit, grid_df), nrow = length(x1_seq)),
        error = function(e) NULL)
      if (is.null(z_mat)) return(NULL)

      col_palette <- grDevices::colorRampPalette(
        c("#2166AC", "#d0d0d0", "#B2182B"))(50)
      z_range <- range(z_mat, na.rm = TRUE)
      z_scaled <- (z_mat - z_range[1]) /
                  max(z_range[2] - z_range[1], 1e-10)
      z_cols <- col_palette[pmax(1, pmin(50, as.integer(z_scaled * 49) + 1))]
      facet_col <- matrix(z_cols, nrow = nrow(z_mat) - 1,
                          ncol = ncol(z_mat) - 1)

      old_par <- graphics::par(mar = c(2, 2, 3, 1))
      on.exit(graphics::par(old_par))
      old_scipen <- options(scipen = 999)
      on.exit(options(old_scipen), add = TRUE)
      graphics::persp(x1_seq, x2_seq, z_mat,
                      theta = 30, phi = 25, expand = 0.6,
                      col = facet_col, border = NA,
                      shade = 0.3, ticktype = "detailed",
                      xlab = var1, ylab = var2,
                      zlab = "Contribution",
                      main = paste("3D Surface:", var1, "\u00d7", var2))
    }, width = d_$width, height = d_$height, res = 96)
  })
}
