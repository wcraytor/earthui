#' ANOVA Decomposition Module UI
#'
#' @param id Module namespace ID.
#' @return A Shiny tagList.
#' @export
anovaUI <- function(id) {
  ns <- shiny::NS(id)
  shiny::tagList(
    shiny::helpText(
      "Variance decomposition by predictor variable. ",
      "SS = sum of squared deviations of each variable's contribution ",
      "from its mean. Due to predictor correlations, individual SS ",
      "values may not sum to total model SS."
    ),
    DT::DTOutput(ns("anova_table"))
  )
}

#' ANOVA Decomposition Module Server
#'
#' @param id Module namespace ID.
#' @param model_module Reactive list from [modelingServer()].
#' @param data_module Reactive list from [dataImportServer()].
#' @return No return value, called for side effects (renders UI outputs).
#' @export
anovaServer <- function(id, model_module, data_module) {
  shiny::moduleServer(id, function(input, output, session) {

    output$anova_table <- DT::renderDT({
      shiny::req(model_module$fitted())
      model_module$fit_count()

      model  <- model_module$model()
      lambda <- model_module$lambda()
      gamma  <- model_module$gamma()
      x_mat  <- model_module$x_matrix()
      y_vec  <- model_module$y_vector()
      preds  <- data_module$predictors()

      # Extract coefficients (excluding intercept)
      coef_args <- list(model, s = lambda)
      if (!is.null(gamma)) coef_args$gamma <- gamma
      coef_sparse <- do.call(stats::coef, coef_args)
      coef_vec <- as.numeric(coef_sparse)[-1L]
      coef_names <- rownames(coef_sparse)[-1L]
      names(coef_vec) <- coef_names
      intercept <- as.numeric(coef_sparse)[1L]

      # Map model matrix columns to original predictor names
      col_to_pred <- character(length(coef_names))
      for (cn in coef_names) {
        # Interaction terms (contain ":") are their own group
        if (grepl(":", cn, fixed = TRUE)) {
          col_to_pred[which(coef_names == cn)] <- cn
          next
        }
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
            break
          }
        }
      }

      # Predictions
      pred_args <- list(model, newx = x_mat, s = lambda, type = "response")
      if (!is.null(gamma)) pred_args$gamma <- gamma
      predicted <- as.numeric(do.call(stats::predict, pred_args))
      y_hat_mean <- mean(predicted)

      # Total model SS
      ss_model <- sum((predicted - y_hat_mean)^2)

      # Per-variable contribution and SS
      unique_preds <- unique(col_to_pred)
      unique_preds <- unique_preds[nzchar(unique_preds)]

      rows <- list()
      for (pred_name in unique_preds) {
        idx <- which(col_to_pred == pred_name)
        if (length(idx) == 0) next

        # Contribution: sum of beta_k * x_k for columns belonging to pred
        contrib <- rep(0, nrow(x_mat))
        for (j in idx) {
          contrib <- contrib + coef_vec[j] * x_mat[, j]
        }

        ss_var <- sum((contrib - mean(contrib))^2)
        pct <- if (ss_model > 0) ss_var / ss_model * 100 else 0

        # Representative coefficient (max abs for factor, single for numeric)
        main_coef <- coef_vec[idx[which.max(abs(coef_vec[idx]))]]

        rows[[length(rows) + 1L]] <- data.frame(
          variable    = pred_name,
          coefficient = round(main_coef, 6),
          ss          = round(ss_var, 2),
          pct_model   = round(pct, 2),
          stringsAsFactors = FALSE
        )
      }

      if (length(rows) == 0L) {
        result <- data.frame(
          variable = character(0), coefficient = numeric(0),
          ss = numeric(0), pct_model = numeric(0),
          stringsAsFactors = FALSE
        )
      } else {
        result <- do.call(rbind, rows)
        result <- result[order(-result$ss), , drop = FALSE]
        rownames(result) <- NULL
      }

      # Add intercept and total rows
      intercept_row <- data.frame(
        variable = "(Intercept)", coefficient = round(intercept, 6),
        ss = NA_real_, pct_model = NA_real_,
        stringsAsFactors = FALSE
      )
      total_row <- data.frame(
        variable = "TOTAL MODEL", coefficient = NA_real_,
        ss = round(ss_model, 2), pct_model = NA_real_,
        stringsAsFactors = FALSE
      )

      result <- rbind(intercept_row, result, total_row)

      DT::datatable(
        result,
        colnames = c("Variable", "Coefficient", "SS", "% of Model SS"),
        rownames = FALSE,
        options = list(scrollX = TRUE, pageLength = 50, dom = "t")
      )
    })
  })
}
