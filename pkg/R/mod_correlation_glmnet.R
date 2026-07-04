#' Correlation Matrix Module UI
#'
#' @param id Module namespace ID.
#' @return A Shiny tagList.
#' @export
correlationUI <- function(id) {
  ns <- shiny::NS(id)
  shiny::tagList(
    shiny::uiOutput(ns("correlation_plot_ui"))
  )
}

#' Correlation Matrix Module Server
#'
#' @param id Module namespace ID.
#' @param data_module Reactive list from [dataImportServer()].
#' @return No return value, called for side effects (renders UI outputs).
#' @export
correlationServer <- function(id, data_module) {
  shiny::moduleServer(id, function(input, output, session) {

    output$correlation_plot_ui <- shiny::renderUI({
      shiny::req(data_module$data())
      ns <- session$ns
      shiny::plotOutput(ns("correlation_plot"), height = "700px", width = "100%")
    })

    d_ <- plot_dims_(session, "correlation_plot")
    output$correlation_plot <- shiny::renderPlot({
      shiny::req(data_module$data())

      df <- data_module$data()
      resp <- data_module$response()
      preds <- data_module$predictors()

      # Include response + active predictors
      vars <- unique(c(resp, preds))
      vars <- intersect(vars, names(df))
      if (length(vars) == 0) vars <- names(df)

      sub_df <- df[, vars, drop = FALSE]

      # Keep only numeric columns
      numeric_mask <- vapply(sub_df, is.numeric, logical(1))
      sub_df <- sub_df[, numeric_mask, drop = FALSE]

      if (ncol(sub_df) < 2L) {
        graphics::plot.new()
        graphics::text(0.5, 0.5,
                       "Need at least 2 numeric variables for correlation matrix",
                       cex = 1.2)
        return()
      }

      cor_mat <- stats::cor(sub_df, use = "pairwise.complete.obs")

      # Reshape to long format
      n <- ncol(cor_mat)
      var_names <- colnames(cor_mat)
      long_df <- data.frame(
        Var1  = rep(var_names, each = n),
        Var2  = rep(var_names, times = n),
        value = as.vector(cor_mat),
        stringsAsFactors = FALSE
      )
      long_df$Var1 <- factor(long_df$Var1, levels = var_names)
      long_df$Var2 <- factor(long_df$Var2, levels = rev(var_names))

      # Scale text/axis size by variable count
      txt_size  <- if (n <= 6) 7 else if (n <= 10) 5 else if (n <= 15) 4 else 3.2
      axis_size <- if (n <= 6) 14 else if (n <= 10) 13 else if (n <= 15) 11 else 9

      text_color <- ifelse(abs(long_df$value) > 0.65, "white", "black")
      font_fam <- glmnet_font_family_()

      ggplot2::ggplot(long_df,
                      ggplot2::aes(x = .data$Var1, y = .data$Var2,
                                   fill = .data$value)) +
        ggplot2::geom_tile(color = "white", linewidth = 1.2) +
        ggplot2::geom_text(
          ggplot2::aes(label = sprintf("%.2f", .data$value)),
          size = txt_size, color = text_color, family = font_fam
        ) +
        ggplot2::scale_fill_gradient2(
          low = "#2166AC", mid = "white", high = "#B2182B",
          midpoint = 0, limits = c(-1, 1), name = "Correlation"
        ) +
        ggplot2::labs(title = "Correlation Matrix", x = NULL, y = NULL) +
        ggplot2::theme_minimal(base_size = 14, base_family = font_fam) +
        ggplot2::theme(
          plot.title = ggplot2::element_text(face = "bold", size = 16),
          axis.text.x = ggplot2::element_text(angle = 45, hjust = 1,
                                               size = axis_size),
          axis.text.y = ggplot2::element_text(size = axis_size),
          panel.grid   = ggplot2::element_blank(),
          legend.position = "right"
        ) +
        ggplot2::coord_fixed(expand = FALSE)
    }, width = d_$width, height = d_$height, res = 96)
  })
}
