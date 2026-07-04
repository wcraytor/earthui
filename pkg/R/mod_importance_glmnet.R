#' Variable Importance Module UI
#'
#' @param id Module namespace ID.
#' @return A Shiny tagList.
#' @export
importanceUI <- function(id) {
  ns <- shiny::NS(id)
  shiny::tagList(
    shiny::uiOutput(ns("importance_plot_ui")),
    shiny::tags$br(),
    DT::DTOutput(ns("importance_table"))
  )
}

#' Variable Importance Module Server
#'
#' @param id Module namespace ID.
#' @param model_module Reactive list from [modelingServer()].
#' @param data_module Reactive list from [dataImportServer()].
#' @return No return value, called for side effects (renders UI outputs).
#' @export
importanceServer <- function(id, model_module, data_module) {
  shiny::moduleServer(id, function(input, output, session) {

    has_plotly <- requireNamespace("plotly", quietly = TRUE)

    importance_df <- shiny::reactive({
      shiny::req(model_module$fitted())
      model_module$fit_count()

      model  <- model_module$model()
      lambda <- model_module$lambda()
      gamma  <- model_module$gamma()
      x_mat  <- model_module$x_matrix()
      preds  <- data_module$predictors()
      col_types <- data_module$col_types()

      # Extract coefficients (excluding intercept)
      coef_args <- list(model, s = lambda)
      if (!is.null(gamma)) coef_args$gamma <- gamma
      coef_sparse <- do.call(stats::coef, coef_args)
      coef_vec <- as.numeric(coef_sparse)[-1L]
      coef_names <- rownames(coef_sparse)[-1L]
      names(coef_vec) <- coef_names

      # Map model matrix columns back to original predictor names
      col_to_pred <- character(length(coef_names))
      for (cn in coef_names) {
        if (grepl(":", cn, fixed = TRUE)) {
          col_to_pred[which(coef_names == cn)] <- cn
          next
        }
        matched <- FALSE
        for (p in preds) {
          if (cn == p || startsWith(cn, paste0(p))) {
            suffix <- sub(paste0("^", p), "", cn)
            if (nzchar(suffix)) {
              longer_match <- FALSE
              for (op in setdiff(preds, p)) {
                if (startsWith(cn, paste0(op)) && nchar(op) > nchar(p)) {
                  longer_match <- TRUE
                  break
                }
              }
              if (longer_match) next
            }
            col_to_pred[which(coef_names == cn)] <- p
            matched <- TRUE
            break
          }
        }
        if (!matched) {
          col_to_pred[which(coef_names == cn)] <- cn
        }
      }

      # Compute importance: |beta * sd(x)| for each model matrix column
      x_sds <- apply(x_mat, 2, stats::sd, na.rm = TRUE)
      raw_importance <- abs(coef_vec * x_sds)

      # Aggregate by predictor
      unique_preds <- unique(col_to_pred)
      unique_preds <- unique_preds[nzchar(unique_preds)]

      imp_data <- data.frame(
        variable   = character(0),
        importance = numeric(0),
        coefficient = numeric(0),
        stringsAsFactors = FALSE
      )

      for (pred_name in unique_preds) {
        idx <- which(col_to_pred == pred_name)
        total_imp <- sum(raw_importance[idx], na.rm = TRUE)
        max_coef <- coef_vec[idx][which.max(abs(coef_vec[idx]))]
        imp_data <- rbind(imp_data, data.frame(
          variable    = pred_name,
          importance  = total_imp,
          coefficient = max_coef,
          stringsAsFactors = FALSE
        ))
      }

      # Remove zero-importance variables and sort
      imp_data <- imp_data[imp_data$importance > 0, , drop = FALSE]
      imp_data <- imp_data[order(-imp_data$importance), , drop = FALSE]

      # Relative importance (percentage)
      total <- sum(imp_data$importance)
      imp_data$pct <- if (total > 0) {
        round(imp_data$importance / total * 100, 2)
      } else {
        0
      }

      rownames(imp_data) <- NULL
      imp_data
    })

    # Dynamic UI: plotly if available, else static ggplot
    output$importance_plot_ui <- shiny::renderUI({
      ns <- session$ns
      if (has_plotly) {
        plotly::plotlyOutput(ns("importance_plotly"), height = "500px")
      } else {
        shiny::plotOutput(ns("importance_plot"), height = "500px")
      }
    })

    # Interactive plotly version
    if (has_plotly) {
      output$importance_plotly <- plotly::renderPlotly({
        shiny::req(importance_df())
        df <- importance_df()
        if (nrow(df) == 0L) return(plotly::plotly_empty())

        font_fam <- glmnet_font_family_()

        # Order variables by importance (least to most for horizontal bars)
        df$variable <- factor(df$variable,
                              levels = rev(df$variable))

        # Format hover text with full precision, no scientific notation
        df$hover <- paste0(
          df$variable,
          "\nImportance: ", formatC(df$importance, format = "f",
                                    digits = 2, big.mark = ","),
          "\nCoefficient: ", formatC(df$coefficient, format = "f",
                                     digits = 6, big.mark = ","),
          "\nRelative: ", df$pct, "%"
        )

        # Compute tick values: ~20 breaks across the range
        max_val <- max(df$importance, na.rm = TRUE)
        tick_step <- pretty(c(0, max_val), n = 20)

        plotly::plot_ly(
          df,
          y = ~variable,
          x = ~importance,
          type = "bar",
          orientation = "h",
          marker = list(color = "#5e81ac"),
          text = ~hover,
          hoverinfo = "text"
        ) |>
          plotly::layout(
            title = list(
              text = "Variable Importance",
              font = list(size = 18, family = font_fam)
            ),
            xaxis = list(
              title = list(
                text = "Importance: |\u03b2| \u00d7 sd(x)",
                font = list(size = 14, family = font_fam)
              ),
              tickfont = list(size = 13, family = font_fam),
              tickvals = tick_step,
              tickformat = ",",
              exponentformat = "none"
            ),
            yaxis = list(
              title = "",
              tickfont = list(size = 13, family = font_fam),
              automargin = TRUE
            ),
            margin = list(l = 10, r = 20, t = 50, b = 50)
          ) |>
          plotly::config(displayModeBar = FALSE)
      })
    }

    # Static ggplot fallback
    d_ <- plot_dims_(session, "importance_plot")
    output$importance_plot <- shiny::renderPlot({
      shiny::req(importance_df())
      df <- importance_df()

      if (nrow(df) == 0L) {
        graphics::plot.new()
        graphics::text(0.5, 0.5, "No variable importance data", cex = 1.2)
        return()
      }

      font_fam <- glmnet_font_family_()
      max_val <- max(df$importance, na.rm = TRUE)
      breaks <- pretty(c(0, max_val), n = 20)

      # Format labels without scientific notation
      labels <- formatC(breaks, format = "f", digits = 0, big.mark = ",")

      ggplot2::ggplot(
        df,
        ggplot2::aes(
          x = stats::reorder(.data$variable, .data$importance),
          y = .data$importance
        )
      ) +
        ggplot2::geom_bar(stat = "identity", fill = "#5e81ac") +
        ggplot2::coord_flip() +
        ggplot2::scale_y_continuous(breaks = breaks, labels = labels) +
        ggplot2::labs(
          title = "Variable Importance",
          subtitle = "Standardized coefficient magnitude: |\u03b2| \u00d7 sd(x)",
          x = NULL,
          y = "Importance"
        ) +
        ggplot2::theme_minimal(base_size = 16, base_family = font_fam) +
        ggplot2::theme(
          plot.title = ggplot2::element_text(face = "bold", size = 18),
          plot.subtitle = ggplot2::element_text(size = 13),
          axis.text = ggplot2::element_text(size = 15),
          axis.title = ggplot2::element_text(size = 16)
        )
    }, width = d_$width, height = d_$height, res = 96)

    output$importance_table <- DT::renderDT({
      shiny::req(importance_df())
      df <- importance_df()

      DT::datatable(
        df,
        colnames = c("Variable", "Importance", "Coefficient", "Rel. %"),
        rownames = FALSE,
        options = list(scrollX = TRUE, pageLength = 30, dom = "t")
      ) |>
        DT::formatRound(c("importance", "coefficient"), digits = 6) |>
        DT::formatRound("pct", digits = 2)
    })
  })
}
