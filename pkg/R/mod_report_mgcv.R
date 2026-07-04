#' Report Export Module -- UI
#'
#' Compact download controls for sidebar embedding.
#'
#' @param id Shiny module namespace ID.
#' @return A [shiny::tagList].
#' @export
#' @examples
#' if (interactive()) {
#'   ui <- fluidPage(mod_report_ui("report1"))
#' }
mod_report_ui <- function(id) {
  ns <- NS(id)
  tagList(
    fluidRow(
      column(7,
        selectInput(ns("format"), NULL,
                    choices = c("HTML" = "html",
                                "Word (.docx)" = "docx",
                                "PDF" = "pdf"),
                    width = "100%")
      ),
      column(5,
        downloadButton(ns("download"), "Report",
                       style = "width: 100%; margin-top: 25px;")
      )
    ),
    downloadButton(ns("download_model"), "Save Model (.rds)",
                   class = "btn-sm btn-outline-secondary",
                   style = "width: 100%; margin-bottom: 4px;"),
    downloadButton(ns("download_predictions"), "Predictions (.csv)",
                   class = "btn-sm btn-outline-secondary",
                   style = "width: 100%; margin-bottom: 8px;"),
    hr(),
    tags$strong("Export Functions", style = "font-size: 0.9em;"),
    checkboxGroupInput(ns("export_langs"), NULL, inline = TRUE,
                       choices = c("R", "Python", "C++", "SQLite"),
                       selected = c("R", "Python")),
    downloadButton(ns("download_functions"), "Download Functions (.zip)",
                   class = "btn-sm btn-outline-primary",
                   style = "width: 100%;")
  )
}


#' Report Export Module -- Server
#'
#' @param id Shiny module namespace ID.
#' @param gam_result_r A reactive returning an `mgcvUI_result`.
#' @param data_r A reactive returning the data frame.
#' @return No return value; called for its side effects (wires up the
#'   report-export module's Shiny outputs and download handlers).
#' @export
mod_report_server <- function(id, gam_result_r, data_r) {
  moduleServer(id, function(input, output, session) {

    output$download <- downloadHandler(
      filename = function() {
        ext <- switch(input$format,
                      html = ".html", pdf = ".pdf", docx = ".docx")
        paste0("gam_report_", fit_stamp_(gam_result_r()$fit_ts), ext)
      },
      content = function(file) {
        res <- gam_result_r()
        req(res)
        if (input$format == "docx") {
          export_gam_docx(res, file)
        } else {
          render_gam_report(res, output_format = input$format,
                            output_file = file)
        }
      }
    )

    output$download_model <- downloadHandler(
      filename = function() {
        paste0("gam_model_", fit_stamp_(gam_result_r()$fit_ts), ".rds")
      },
      content = function(file) {
        res <- gam_result_r()
        req(res)
        saveRDS(res, file)
      }
    )

    output$download_predictions <- downloadHandler(
      filename = function() {
        paste0("gam_predictions_", fit_stamp_(gam_result_r()$fit_ts), ".csv")
      },
      content = function(file) {
        res <- gam_result_r()
        df <- data_r()
        req(res, df)
        preds <- predict(res$model, newdata = df, type = "response")
        out <- df
        out$.predicted <- as.numeric(preds)
        out$.residual <- out[[res$response]] - out$.predicted
        utils::write.csv(out, file, row.names = FALSE)
      }
    )

    output$download_functions <- downloadHandler(
      filename = function() {
        paste0("gam_functions_", fit_stamp_(gam_result_r()$fit_ts), ".zip")
      },
      content = function(file) {
        res <- gam_result_r()
        req(res)
        langs <- input$export_langs
        if (is.null(langs) || length(langs) == 0L) {
          langs <- c("R", "Python")
        }
        export_functions_zip(res, file, languages = langs)
      }
    )
  })
}
