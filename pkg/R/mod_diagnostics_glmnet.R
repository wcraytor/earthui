#' Diagnostics and Plots Module UI
#'
#' UI component for diagnostic plots.
#'
#' @param id Module namespace ID.
#'
#' @return A Shiny \code{tagList} containing the module UI elements.
#'
#' @export
#' @examples
#' if (interactive()) {
#'   ui <- shiny::fluidPage(diagnosticsUI("diag"))
#' }
diagnosticsUI <- function(id) {

  ns <- shiny::NS(id)
  shiny::tagList(
    shiny::h4("Diagnostic Plots"),
    shiny::tabsetPanel(
      id = ns("plot_tabs"),
      shiny::tabPanel(
        "Coefficient Path",
        shiny::plotOutput(ns("coef_path_plot"), height = "500px"),
        shiny::downloadButton(ns("dl_coef_path"), "Download PNG")
      ),
      shiny::tabPanel(
        "CV Error",
        shiny::plotOutput(ns("cv_plot"), height = "500px"),
        shiny::downloadButton(ns("dl_cv"), "Download PNG")
      ),
      shiny::tabPanel(
        "Actual vs Predicted",
        shiny::plotOutput(ns("avp_plot"), height = "500px"),
        shiny::downloadButton(ns("dl_avp"), "Download PNG")
      ),
      shiny::tabPanel(
        "Residuals vs Fitted",
        shiny::plotOutput(ns("resid_plot"), height = "500px"),
        shiny::downloadButton(ns("dl_resid"), "Download PNG")
      )
    )
  )
}

# Axis label formatter: no scientific notation, with comma separators
#' @noRd
glmnet_axis_labels_ <- function(x) {
  # Compact labels (e.g. -4M, 500K) so large contributions don't make very wide
  # tick labels that collide with the axis title / run off the margin.
  if (requireNamespace("scales", quietly = TRUE)) {
    scales::label_number(scale_cut = scales::cut_short_scale())(x)
  } else {
    formatC(x, format = "f", digits = 0, big.mark = ",")
  }
}

# Common theme for diagnostic plots
#' @noRd
glmnet_diag_theme_ <- function(font_fam) {
  ggplot2::theme_minimal(base_size = 16, base_family = font_fam) +
    ggplot2::theme(
      plot.title = ggplot2::element_text(face = "bold", size = 18),
      axis.text = ggplot2::element_text(size = 15),
      axis.title = ggplot2::element_text(size = 16),
      legend.text = ggplot2::element_text(size = 12)
    )
}

#' Diagnostics and Plots Module Server
#'
#' Server logic for diagnostic plots using ggplot2.
#'
#' @param id Module namespace ID.
#' @param model_module Reactive list returned by [modelingServer()].
#'
#' @return \code{NULL} (side effects only: renders plots and downloads).
#'
#' @export
#' @examples
#' if (interactive()) {
#'   server <- function(input, output, session) {
#'     data_out <- dataImportServer("data", shiny::reactiveVal("general"))
#'     model_out <- modelingServer("model", data_out)
#'     diagnosticsServer("diag", model_out)
#'   }
#' }
diagnosticsServer <- function(id, model_module) {
  shiny::moduleServer(id, function(input, output, session) {

    coef_path_gg <- shiny::reactive({
      shiny::req(model_module$fitted())
      model_module$fit_count()
      model <- model_module$model()
      font_fam <- glmnet_font_family_()

      fit_obj <- if (inherits(model, "cv.glmnet") ||
                       inherits(model, "cv.relaxed")) {
        model$glmnet.fit
      } else if (inherits(model, "relaxed")) {
        model$relaxed
      } else {
        model
      }

      beta_mat <- as.matrix(fit_obj$beta)
      log_lambda <- log(fit_obj$lambda)

      df_long <- data.frame(
        log_lambda = rep(log_lambda, each = nrow(beta_mat)),
        coefficient = as.numeric(beta_mat),
        variable = rep(rownames(beta_mat), times = ncol(beta_mat)),
        stringsAsFactors = FALSE
      )

      ggplot2::ggplot(df_long,
                      ggplot2::aes(x = .data$log_lambda,
                                   y = .data$coefficient,
                                   color = .data$variable)) +
        ggplot2::geom_line() +
        ggplot2::scale_y_continuous(labels = glmnet_axis_labels_,
                                    n.breaks = 15) +
        ggplot2::scale_x_continuous(n.breaks = 15) +
        ggplot2::labs(x = "Log(Lambda)", y = "Coefficient",
                      title = "Coefficient Path") +
        glmnet_diag_theme_(font_fam) +
        ggplot2::theme(legend.position = "right")
    })

    cv_gg <- shiny::reactive({
      shiny::req(model_module$fitted())
      model_module$fit_count()
      model <- model_module$model()
      font_fam <- glmnet_font_family_()
      shiny::req(inherits(model, "cv.glmnet") ||
                   inherits(model, "cv.relaxed"))

      df_cv <- data.frame(
        log_lambda = log(model$lambda),
        cvm = model$cvm,
        cvup = model$cvup,
        cvlo = model$cvlo,
        stringsAsFactors = FALSE
      )

      ggplot2::ggplot(df_cv, ggplot2::aes(x = .data$log_lambda,
                                          y = .data$cvm)) +
        ggplot2::geom_point(color = "#bf616a", size = 1.5) +
        ggplot2::geom_errorbar(ggplot2::aes(ymin = .data$cvlo,
                                            ymax = .data$cvup),
                               alpha = 0.4) +
        ggplot2::geom_vline(xintercept = log(model$lambda.min),
                            linetype = "dashed", color = "#5e81ac") +
        ggplot2::geom_vline(xintercept = log(model$lambda.1se),
                            linetype = "dashed", color = "#a3be8c") +
        ggplot2::scale_y_continuous(labels = glmnet_axis_labels_,
                                    n.breaks = 15) +
        ggplot2::scale_x_continuous(n.breaks = 15) +
        ggplot2::labs(x = "Log(Lambda)",
                      y = "Cross-Validation Error",
                      title = "CV Error Plot") +
        ggplot2::annotate("text", x = log(model$lambda.min),
                          y = max(df_cv$cvup),
                          label = "lambda.min", hjust = -0.1,
                          color = "#5e81ac", size = 4.5,
                          family = font_fam) +
        ggplot2::annotate("text", x = log(model$lambda.1se),
                          y = max(df_cv$cvup),
                          label = "lambda.1se", hjust = -0.1,
                          color = "#a3be8c", size = 4.5,
                          family = font_fam) +
        glmnet_diag_theme_(font_fam)
    })

    avp_gg <- shiny::reactive({
      shiny::req(model_module$fitted())
      model_module$fit_count()
      model <- model_module$model()
      lambda <- model_module$lambda()
      gamma <- model_module$gamma()
      x_mat <- model_module$x_matrix()
      y_vec <- model_module$y_vector()
      font_fam <- glmnet_font_family_()

      pred_args <- list(model, newx = x_mat, s = lambda, type = "response")
      if (!is.null(gamma)) pred_args$gamma <- gamma
      preds <- as.numeric(do.call(stats::predict, pred_args))

      df_avp <- data.frame(actual = y_vec, predicted = preds)

      ggplot2::ggplot(df_avp, ggplot2::aes(x = .data$actual,
                                           y = .data$predicted)) +
        ggplot2::geom_point(alpha = 0.5, color = "#5e81ac") +
        ggplot2::geom_abline(slope = 1, intercept = 0,
                             color = "#bf616a", linetype = "dashed") +
        ggplot2::scale_x_continuous(labels = glmnet_axis_labels_,
                                    n.breaks = 15) +
        ggplot2::scale_y_continuous(labels = glmnet_axis_labels_,
                                    n.breaks = 15) +
        ggplot2::labs(x = "Actual", y = "Predicted",
                      title = "Actual vs Predicted") +
        glmnet_diag_theme_(font_fam)
    })

    resid_gg <- shiny::reactive({
      shiny::req(model_module$fitted())
      model_module$fit_count()
      model <- model_module$model()
      lambda <- model_module$lambda()
      gamma <- model_module$gamma()
      x_mat <- model_module$x_matrix()
      y_vec <- model_module$y_vector()
      font_fam <- glmnet_font_family_()

      pred_args <- list(model, newx = x_mat, s = lambda, type = "response")
      if (!is.null(gamma)) pred_args$gamma <- gamma
      preds <- as.numeric(do.call(stats::predict, pred_args))
      resids <- y_vec - preds

      df_resid <- data.frame(fitted = preds, residuals = resids)

      ggplot2::ggplot(df_resid, ggplot2::aes(x = .data$fitted,
                                             y = .data$residuals)) +
        ggplot2::geom_point(alpha = 0.5, color = "#5e81ac") +
        ggplot2::geom_hline(yintercept = 0, color = "#bf616a",
                            linetype = "dashed") +
        ggplot2::scale_x_continuous(labels = glmnet_axis_labels_,
                                    n.breaks = 15) +
        ggplot2::scale_y_continuous(labels = glmnet_axis_labels_,
                                    n.breaks = 15) +
        ggplot2::labs(x = "Fitted Values", y = "Residuals",
                      title = "Residuals vs Fitted") +
        glmnet_diag_theme_(font_fam)
    })

    d_ <- plot_dims_(session, "coef_path_plot")
    output$coef_path_plot <- shiny::renderPlot({ coef_path_gg() },
      width = d_$width, height = d_$height, res = 96)
    d_ <- plot_dims_(session, "cv_plot")
    output$cv_plot <- shiny::renderPlot({ cv_gg() },
      width = d_$width, height = d_$height, res = 96)
    d_ <- plot_dims_(session, "avp_plot")
    output$avp_plot <- shiny::renderPlot({ avp_gg() },
      width = d_$width, height = d_$height, res = 96)
    d_ <- plot_dims_(session, "resid_plot")
    output$resid_plot <- shiny::renderPlot({ resid_gg() },
      width = d_$width, height = d_$height, res = 96)

    output$dl_coef_path <- shiny::downloadHandler(
      filename = function() "coefficient_path.png",
      content = function(file) {
        ggplot2::ggsave(file, plot = coef_path_gg(),
                        width = 10, height = 6, dpi = 150)
      }
    )
    output$dl_cv <- shiny::downloadHandler(
      filename = function() "cv_error.png",
      content = function(file) {
        ggplot2::ggsave(file, plot = cv_gg(),
                        width = 10, height = 6, dpi = 150)
      }
    )
    output$dl_avp <- shiny::downloadHandler(
      filename = function() "actual_vs_predicted.png",
      content = function(file) {
        ggplot2::ggsave(file, plot = avp_gg(),
                        width = 10, height = 6, dpi = 150)
      }
    )
    output$dl_resid <- shiny::downloadHandler(
      filename = function() "residuals_vs_fitted.png",
      content = function(file) {
        ggplot2::ggsave(file, plot = resid_gg(),
                        width = 10, height = 6, dpi = 150)
      }
    )

    invisible(NULL)
  })
}
