#' Report Export Module UI
#'
#' UI component for report generation and export.
#'
#' @param id Module namespace ID.
#'
#' @return A Shiny \code{tagList} containing the module UI elements.
#'
#' @export
#' @examples
#' if (interactive()) {
#'   ui <- shiny::fluidPage(reportUI("report"))
#' }
reportUI <- function(id) {
  ns <- shiny::NS(id)
  shiny::tagList(
    shiny::h4("Report Export"),
    shiny::textInput(ns("appraiser_name"), "Appraiser Name", value = ""),
    shiny::textInput(ns("property_address"), "Property Address", value = ""),
    shiny::dateInput(ns("report_date"), "Report Date", value = Sys.Date()),
    shiny::textInput(ns("file_number"), "File Number", value = ""),
    shiny::hr(),
    shiny::downloadButton(ns("dl_word"), "Export to Word (.docx)",
                          class = "btn-primary"),
    shiny::br(), shiny::br(),
    shiny::downloadButton(ns("dl_pdf"), "Export to PDF",
                          class = "btn-info")
  )
}

#' Report Export Module Server
#'
#' Server logic for generating Word, PDF, and HTML reports.
#'
#' @param id Module namespace ID.
#' @param model_module Reactive list returned by [modelingServer()].
#' @param coef_module Reactive list returned by [coefficientsServer()].
#' @param data_module Reactive list returned by [dataImportServer()].
#'
#' @return \code{NULL} (side effects only: report downloads).
#'
#' @export
#' @examples
#' if (interactive()) {
#'   server <- function(input, output, session) {
#'     data_out <- dataImportServer("data", shiny::reactiveVal("general"))
#'     model_out <- modelingServer("model", data_out)
#'     coef_out <- coefficientsServer("coefs", model_out, data_out)
#'     reportServer("report", model_out, coef_out, data_out)
#'   }
#' }
reportServer <- function(id, model_module, coef_module, data_module) {
  shiny::moduleServer(id, function(input, output, session) {

    make_plots <- function(tmpdir) {
      model <- model_module$model()
      lambda <- model_module$lambda()
      gamma <- model_module$gamma()
      x_mat <- model_module$x_matrix()
      y_vec <- model_module$y_vector()
      fit_obj <- if (inherits(model, "cv.glmnet") ||
                       inherits(model, "cv.relaxed")) {
        model$glmnet.fit
      } else if (inherits(model, "relaxed")) {
        model$relaxed
      } else {
        model
      }

      pred_args <- list(model, newx = x_mat, s = lambda, type = "response")
      if (!is.null(gamma)) pred_args$gamma <- gamma
      preds <- as.numeric(do.call(stats::predict, pred_args))
      resids <- y_vec - preds

      # Coefficient path
      beta_mat <- as.matrix(fit_obj$beta)
      log_lambda <- log(fit_obj$lambda)
      df_long <- data.frame(
        log_lambda = rep(log_lambda, each = nrow(beta_mat)),
        coefficient = as.numeric(beta_mat),
        variable = rep(rownames(beta_mat), times = ncol(beta_mat)),
        stringsAsFactors = FALSE
      )
      p1 <- ggplot2::ggplot(df_long,
                            ggplot2::aes(x = .data$log_lambda,
                                         y = .data$coefficient,
                                         color = .data$variable)) +
        ggplot2::geom_line() +
        ggplot2::labs(x = "Log(Lambda)", y = "Coefficient",
                      title = "Coefficient Path") +
        ggplot2::theme_minimal(base_family = glmnet_font_family_())
      ggplot2::ggsave(file.path(tmpdir, "coef_path.png"), p1,
                      width = 8, height = 5, dpi = 150)

      # CV plot
      if (inherits(model, "cv.glmnet") || inherits(model, "cv.relaxed")) {
        df_cv <- data.frame(
          log_lambda = log(model$lambda),
          cvm = model$cvm,
          cvup = model$cvup,
          cvlo = model$cvlo,
          stringsAsFactors = FALSE
        )
        p2 <- ggplot2::ggplot(df_cv, ggplot2::aes(x = .data$log_lambda,
                                                  y = .data$cvm)) +
          ggplot2::geom_point(color = "red", size = 1.5) +
          ggplot2::geom_errorbar(ggplot2::aes(ymin = .data$cvlo,
                                              ymax = .data$cvup),
                                 alpha = 0.4) +
          ggplot2::geom_vline(xintercept = log(model$lambda.min),
                              linetype = "dashed", color = "blue") +
          ggplot2::geom_vline(xintercept = log(model$lambda.1se),
                              linetype = "dashed", color = "darkgreen") +
          ggplot2::labs(x = "Log(Lambda)", y = "CV Error",
                        title = "Cross-Validation Error") +
          ggplot2::theme_minimal(base_family = glmnet_font_family_())
        ggplot2::ggsave(file.path(tmpdir, "cv_error.png"), p2,
                        width = 8, height = 5, dpi = 150)
      }

      # Actual vs predicted
      df_avp <- data.frame(actual = y_vec, predicted = preds)
      p3 <- ggplot2::ggplot(df_avp, ggplot2::aes(x = .data$actual,
                                                 y = .data$predicted)) +
        ggplot2::geom_point(alpha = 0.5) +
        ggplot2::geom_abline(slope = 1, intercept = 0,
                             color = "red", linetype = "dashed") +
        ggplot2::labs(x = "Actual", y = "Predicted",
                      title = "Actual vs Predicted") +
        ggplot2::theme_minimal(base_family = glmnet_font_family_())
      ggplot2::ggsave(file.path(tmpdir, "actual_vs_predicted.png"), p3,
                      width = 8, height = 5, dpi = 150)

      # Residuals vs fitted
      df_resid <- data.frame(fitted = preds, residuals = resids)
      p4 <- ggplot2::ggplot(df_resid, ggplot2::aes(x = .data$fitted,
                                                   y = .data$residuals)) +
        ggplot2::geom_point(alpha = 0.5) +
        ggplot2::geom_hline(yintercept = 0, color = "red",
                            linetype = "dashed") +
        ggplot2::labs(x = "Fitted Values", y = "Residuals",
                      title = "Residuals vs Fitted") +
        ggplot2::theme_minimal(base_family = glmnet_font_family_())
      ggplot2::ggsave(file.path(tmpdir, "residuals_vs_fitted.png"), p4,
                      width = 8, height = 5, dpi = 150)
    }

    build_word_report <- function(file) {
      shiny::req(model_module$fitted())
      tmpdir <- tempdir()
      make_plots(tmpdir)

      coef_df <- coef_module$coef_df()
      model <- model_module$model()
      lambda <- model_module$lambda()

      doc <- officer::read_docx()
      doc <- officer::body_add_par(doc, "glmnetUI Model Report",
                                   style = "heading 1")
      doc <- officer::body_add_par(doc, paste("Appraiser:", input$appraiser_name))
      doc <- officer::body_add_par(doc, paste("Property:", input$property_address))
      doc <- officer::body_add_par(doc, paste("Date:", as.character(input$report_date)))
      doc <- officer::body_add_par(doc, paste("File Number:", input$file_number))
      doc <- officer::body_add_par(doc, "")

      # Model summary
      doc <- officer::body_add_par(doc, "Model Summary", style = "heading 2")
      alpha_val <- if (inherits(model, "cv.glmnet")) {
        model$glmnet.fit$call$alpha
      } else {
        model$call$alpha
      }
      if (!is.null(alpha_val)) {
        doc <- officer::body_add_par(doc, paste("Alpha:", alpha_val))
      }
      doc <- officer::body_add_par(doc, paste("Lambda:", signif(lambda, 4)))
      if (inherits(model, "cv.glmnet") || inherits(model, "cv.relaxed")) {
        doc <- officer::body_add_par(
          doc, paste("lambda.min:", signif(model$lambda.min, 4))
        )
        doc <- officer::body_add_par(
          doc, paste("lambda.1se:", signif(model$lambda.1se, 4))
        )
      }

      # Coefficient table
      doc <- officer::body_add_par(doc, "Coefficients", style = "heading 2")
      ft <- officer::body_add_table(doc, value = coef_df, style = "table_template")
      doc <- ft

      # Plots
      doc <- officer::body_add_par(doc, "Diagnostic Plots", style = "heading 2")
      plot_files <- c("coef_path.png", "cv_error.png",
                      "actual_vs_predicted.png", "residuals_vs_fitted.png")
      for (pf in plot_files) {
        fp <- file.path(tmpdir, pf)
        if (file.exists(fp)) {
          doc <- officer::body_add_img(doc, src = fp,
                                       width = 6, height = 3.75)
          doc <- officer::body_add_par(doc, "")
        }
      }

      print(doc, target = file)
    }

    output$dl_word <- shiny::downloadHandler(
      filename = function() {
        paste0("glmnet_report_", fit_stamp_(model_module$fit_ts()), ".docx")
      },
      content = function(file) {
        message("[glmnetUI MOD] dl_word handler: file=", file)
        tryCatch(
          generate_report_(file, "docx", "dl_word"),
          error = function(e) {
            message("[glmnetUI MOD] Word export ERROR: ", e$message)
            shiny::showNotification(paste("Word export error:", e$message),
                                    type = "error")
          }
        )
      }
    )

    # Helper: generate report via prepare_report_assets_glmnet + render_report_glmnet
    # render_report_glmnet() now has built-in Quarto→rmarkdown fallback
    generate_report_ <- function(file, fmt, btn_id = NULL) {
      message("[glmnetUI MOD] generate_report_() called: fmt=", fmt,
              " file=", file)
      model <- model_module$model()
      lambda <- model_module$lambda()
      gamma <- model_module$gamma()
      x_mat <- model_module$x_matrix()
      y_vec <- model_module$y_vector()
      coef_df <- coef_module$coef_df()

      alpha_val <- if (inherits(model, "cv.glmnet")) {
        a <- model$glmnet.fit$call$alpha
        if (is.null(a)) 1 else a
      } else {
        a <- model$call$alpha
        if (is.null(a)) 1 else a
      }

      message("[glmnetUI MOD] Calling prepare_report_assets_glmnet()...")
      assets_dir <- prepare_report_assets_glmnet(
        model = model, lambda = lambda, gamma = gamma,
        x_mat = x_mat, y_vec = y_vec, coef_df = coef_df,
        predictors = data_module$predictors() %||% character(0),
        response = data_module$response() %||% "y",
        data = data_module$data(),
        data_file_name = data_module$file_name() %||% ""
      )
      message("[glmnetUI MOD] assets_dir=", assets_dir)

      message("[glmnetUI MOD] Calling render_report_glmnet()...")
      render_report_glmnet(output_format = fmt, output_file = file,
                    assets_dir = assets_dir)
      message("[glmnetUI MOD] render_report_glmnet() done. file exists: ",
              file.exists(file))
      # Green check mark on the export button (cleared by JS on the next fit).
      if (!is.null(btn_id)) {
        session$sendCustomMessage("btn_done", list(id = session$ns(btn_id)))
      }
    }

    output$dl_pdf <- shiny::downloadHandler(
      filename = function() {
        paste0("glmnet_report_", fit_stamp_(model_module$fit_ts()), ".pdf")
      },
      content = function(file) {
        message("[glmnetUI MOD] dl_pdf handler: file=", file)
        tryCatch(
          generate_report_(file, "pdf", "dl_pdf"),
          error = function(e) {
            message("[glmnetUI MOD] PDF export ERROR: ", e$message)
            shiny::showNotification(paste("PDF export error:", e$message),
                                    type = "error")
          }
        )
      }
    )

    invisible(NULL)
  })
}
