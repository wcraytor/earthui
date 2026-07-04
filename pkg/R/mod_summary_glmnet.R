#' Model Summary Module UI
#'
#' @param id Module namespace ID.
#' @return A Shiny tagList.
#' @export
summaryUI <- function(id) {
  ns <- shiny::NS(id)
  shiny::tagList(
    shiny::uiOutput(ns("summary_metrics")),
    shiny::tags$h5("Coefficients"),
    DT::DTOutput(ns("summary_coef_table"))
  )
}

#' Model Summary Module Server
#'
#' @param id Module namespace ID.
#' @param model_module Reactive list from [modelingServer()].
#' @param data_module Reactive list from [dataImportServer()].
#' @param purpose Reactive returning current purpose mode.
#' @return A reactive list with summary statistics.
#' @export
summaryServer <- function(id, model_module, data_module, purpose) {
  shiny::moduleServer(id, function(input, output, session) {

    summary_stats <- shiny::reactive({
      shiny::req(model_module$fitted())
      model_module$fit_count()

      model  <- model_module$model()
      lambda <- model_module$lambda()
      gamma  <- model_module$gamma()
      x_mat  <- model_module$x_matrix()
      y_vec  <- model_module$y_vector()

      # Predictions
      pred_args <- list(model, newx = x_mat, s = lambda, type = "response")
      if (!is.null(gamma)) pred_args$gamma <- gamma
      predicted <- as.numeric(do.call(stats::predict, pred_args))
      residuals <- y_vec - predicted

      n <- length(y_vec)
      ss_res <- sum(residuals^2)
      ss_tot <- sum((y_vec - mean(y_vec))^2)

      # Coefficients
      coef_args <- list(model, s = lambda)
      if (!is.null(gamma)) coef_args$gamma <- gamma
      coefs <- do.call(stats::coef, coef_args)
      n_nonzero <- sum(as.numeric(coefs) != 0) - 1L
      p <- n_nonzero

      # R-squared
      r_squared <- 1 - ss_res / ss_tot

      # Adjusted R-squared
      adj_r_squared <- if (n - p - 1 > 0) {
        1 - (1 - r_squared) * (n - 1) / (n - p - 1)
      } else {
        NA_real_
      }

      # GR-squared (Generalized): use glmnet dev.ratio at selected lambda
      gr_squared <- NA_real_
      fit_obj <- if (inherits(model, "cv.glmnet") ||
                       inherits(model, "cv.relaxed")) {
        model$glmnet.fit
      } else if (inherits(model, "relaxed")) {
        model$relaxed
      } else {
        model
      }
      if (!is.null(fit_obj$dev.ratio) && !is.null(fit_obj$lambda)) {
        idx <- which.min(abs(fit_obj$lambda - lambda))
        if (length(idx) == 1L) gr_squared <- fit_obj$dev.ratio[idx]
      }

      # CV R-squared
      cv_r2 <- NA_real_
      is_cv <- inherits(model, "cv.glmnet") || inherits(model, "cv.relaxed")
      if (is_cv) {
        idx <- which(model$lambda == lambda)
        cv_mse <- if (length(idx) == 1L) {
          model$cvm[idx]
        } else {
          tryCatch(
            stats::approx(log(model$lambda), model$cvm,
                          xout = log(lambda))$y,
            error = function(e) NA
          )
        }
        if (!is.na(cv_mse)) cv_r2 <- 1 - cv_mse / stats::var(y_vec)
      }

      # Error metrics
      rmse <- sqrt(ss_res / n)
      mae  <- mean(abs(residuals))

      # Ratio-based metrics (appraisal: predicted / actual)
      ratios <- predicted / y_vec
      valid_ratios <- ratios[is.finite(ratios)]

      median_ratio <- if (length(valid_ratios) > 0) {
        stats::median(valid_ratios)
      } else NA_real_

      cod <- if (length(valid_ratios) > 0 && !is.na(median_ratio) &&
                   median_ratio != 0) {
        mean(abs(valid_ratios - median_ratio)) / median_ratio * 100
      } else NA_real_

      prd <- if (length(valid_ratios) > 0 && sum(y_vec) != 0) {
        mean(valid_ratios) / (sum(predicted) / sum(y_vec))
      } else NA_real_

      list(
        r_squared     = r_squared,
        adj_r_squared = adj_r_squared,
        gr_squared    = gr_squared,
        cv_r2         = cv_r2,
        rmse          = rmse,
        mae           = mae,
        median_ratio  = median_ratio,
        cod           = cod,
        prd           = prd,
        n_obs         = n,
        n_predictors  = ncol(x_mat),
        n_nonzero     = n_nonzero,
        is_cv         = is_cv
      )
    })

    output$summary_metrics <- shiny::renderUI({
      shiny::req(summary_stats())
      s <- summary_stats()
      appraiser <- purpose() %in% c("appraisal", "market")

      fmt <- function(x, digits = 4) {
        if (is.na(x)) "N/A" else sprintf(paste0("%.", digits, "f"), x)
      }

      card <- function(label, value) {
        shiny::tags$div(
          class = "col-md-3",
          shiny::tags$div(
            class = "card text-center",
            style = "padding: 10px; margin-bottom: 8px;",
            shiny::tags$h6(label, style = "margin-bottom: 2px; color: var(--bs-secondary-color);"),
            shiny::tags$h4(value, style = "margin: 0;")
          )
        )
      }

      # Row 1: R-squared metrics
      row1 <- shiny::tags$div(
        class = "row",
        card("R\u00b2", fmt(s$r_squared)),
        card("Adj R\u00b2", fmt(s$adj_r_squared)),
        card("GR\u00b2", fmt(s$gr_squared)),
        card("CV R\u00b2", fmt(s$cv_r2))
      )

      # Row 2: Error metrics
      row2 <- shiny::tags$div(
        class = "row",
        card("RMSE", fmt(s$rmse, 2)),
        card("MAE", fmt(s$mae, 2)),
        card("Observations", as.character(s$n_obs)),
        card("Non-zero Coefs", paste0(s$n_nonzero, " / ", s$n_predictors))
      )

      # Row 3: Appraisal metrics (conditional)
      row3 <- if (appraiser) {
        shiny::tags$div(
          class = "row",
          card("Median Ratio", fmt(s$median_ratio)),
          card("COD", fmt(s$cod, 2)),
          card("PRD", fmt(s$prd))
        )
      }

      # Overfitting warning
      warning_div <- if (s$is_cv && !is.na(s$cv_r2) &&
                           (s$r_squared - s$cv_r2) > 0.1) {
        shiny::tags$div(
          class = "alert alert-warning",
          style = "font-size: 0.9em; margin-top: 8px;",
          shiny::tags$strong("Possible overfitting: "),
          sprintf("Training R\u00b2 (%.4f) is substantially higher than CV R\u00b2 (%.4f)",
                  s$r_squared, s$cv_r2)
        )
      }

      shiny::tagList(row1, row2, row3, warning_div)
    })

    output$summary_coef_table <- DT::renderDT({
      shiny::req(model_module$fitted())
      model_module$fit_count()

      model  <- model_module$model()
      lambda <- model_module$lambda()
      gamma  <- model_module$gamma()

      coef_tbl <- build_coef_table(model, lambda, gamma)
      expected <- data_module$expected_signs()

      non_int <- coef_tbl[coef_tbl$variable != "(Intercept)", , drop = FALSE]
      int_row <- coef_tbl[coef_tbl$variable == "(Intercept)", , drop = FALSE]

      if (nrow(non_int) > 0) {
        non_int$sign_warning <- check_sign_warnings(
          non_int$variable, non_int$coefficient, expected
        )
      } else {
        non_int$sign_warning <- character(0)
      }
      if (nrow(int_row) > 0) int_row$sign_warning <- "OK"

      df <- rbind(int_row, non_int)
      df$coefficient <- round(df$coefficient, 6)

      flagged <- which(df$sign_warning != "OK")
      dt <- DT::datatable(
        df,
        colnames = c("Variable", "Coefficient", "Sign Warning"),
        rownames = FALSE,
        options = list(scrollX = TRUE, pageLength = 50, dom = "t")
      )
      if (length(flagged) > 0) {
        dt <- DT::formatStyle(
          dt, columns = seq_len(ncol(df)), target = "row",
          backgroundColor = DT::styleRow(flagged, "#ffcccc")
        )
      }
      dt
    })

    return(list(summary_stats = summary_stats))
  })
}
