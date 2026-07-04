#' Coefficient Output Module UI
#'
#' UI component for displaying coefficients with sign warnings.
#'
#' @param id Module namespace ID.
#'
#' @return A Shiny \code{tagList} containing the module UI elements.
#'
#' @export
#' @examples
#' if (interactive()) {
#'   ui <- shiny::fluidPage(coefficientsUI("coefs"))
#' }
coefficientsUI <- function(id) {

  ns <- shiny::NS(id)
  shiny::tagList(
    shiny::h4("Coefficient Table"),
    shiny::helpText(
      "Rows highlighted in red indicate coefficients with unexpected signs."
    ),
    DT::DTOutput(ns("coef_table"))
  )
}

#' Coefficient Output Module Server
#'
#' Server logic for the coefficient output module. Extracts coefficients
#' and applies sign warning logic.
#'
#' @param id Module namespace ID.
#' @param model_module Reactive list returned by [modelingServer()].
#' @param data_module Reactive list returned by [dataImportServer()].
#'
#' @return A reactive list containing:
#' \describe{
#'   \item{coef_df}{Data frame of coefficients with sign warnings.}
#' }
#'
#' @export
#' @examples
#' if (interactive()) {
#'   server <- function(input, output, session) {
#'     data_out <- dataImportServer("data", shiny::reactiveVal("general"))
#'     model_out <- modelingServer("model", data_out)
#'     coef_out <- coefficientsServer("coefs", model_out, data_out)
#'   }
#' }
coefficientsServer <- function(id, model_module, data_module) {
  shiny::moduleServer(id, function(input, output, session) {

    coef_data <- shiny::reactive({
      shiny::req(model_module$fitted())
      # Depend on fit_count to re-invalidate on every new fit
      model_module$fit_count()

      model <- model_module$model()
      lambda <- model_module$lambda()
      gamma <- model_module$gamma()
      expected <- data_module$expected_signs()

      coef_tbl <- build_coef_table(model, lambda, gamma)
      non_intercept <- coef_tbl[coef_tbl$variable != "(Intercept)", ,
                                drop = FALSE]
      intercept_row <- coef_tbl[coef_tbl$variable == "(Intercept)", ,
                                drop = FALSE]

      if (nrow(non_intercept) > 0) {
        warnings <- check_sign_warnings(
          coef_names = non_intercept$variable,
          coef_values = non_intercept$coefficient,
          expected_signs = expected
        )
        non_intercept$sign_warning <- warnings
      } else {
        non_intercept$sign_warning <- character(0)
      }

      if (nrow(intercept_row) > 0) {
        intercept_row$sign_warning <- "OK"
      }

      result <- rbind(intercept_row, non_intercept)
      result$coefficient <- round(result$coefficient, 6)
      result
    })

    output$coef_table <- DT::renderDT({
      shiny::req(coef_data())
      df <- coef_data()

      flagged <- which(df$sign_warning != "OK")

      dt <- DT::datatable(
        df,
        colnames = c("Variable", "Coefficient", "Sign Warning"),
        rownames = FALSE,
        options = list(
          scrollX = TRUE,
          pageLength = 50,
          dom = "t"
        )
      )

      if (length(flagged) > 0) {
        dt <- DT::formatStyle(
          dt,
          columns = seq_len(ncol(df)),
          target = "row",
          backgroundColor = DT::styleRow(flagged, "#ffcccc")
        )
      }

      dt
    })

    return(list(
      coef_df = coef_data
    ))
  })
}
