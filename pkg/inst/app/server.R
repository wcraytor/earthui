function(input, output, session) {

  # --- Reactive values ---
  rv <- reactiveValues(
    data = NULL,
    categoricals = NULL,
    result = NULL
  )

  # --- Data Import ---
  observeEvent(input$file_input, {
    req(input$file_input)
    rv$data <- import_data(input$file_input$datapath)
    rv$categoricals <- detect_categoricals(rv$data)
    rv$result <- NULL
  })

  output$data_loaded <- reactive(!is.null(rv$data))
  outputOptions(output, "data_loaded", suspendWhenHidden = FALSE)

  output$model_fitted <- reactive(!is.null(rv$result))
  outputOptions(output, "model_fitted", suspendWhenHidden = FALSE)

  output$data_preview_info <- renderUI({
    req(rv$data)
    tags$div(
      class = "alert alert-info",
      style = "font-size: 0.85em; padding: 8px;",
      sprintf("%d rows, %d columns", nrow(rv$data), ncol(rv$data))
    )
  })

  output$data_preview <- DT::renderDataTable({
    req(rv$data)
    DT::datatable(rv$data, options = list(pageLength = 10, scrollX = TRUE),
                  rownames = FALSE)
  })

  # --- Variable Configuration ---
  output$target_selector <- renderUI({
    req(rv$data)
    selectInput("target", "Target (response) variable",
                choices = names(rv$data))
  })

  output$predictor_selector <- renderUI({
    req(rv$data, input$target)
    candidates <- setdiff(names(rv$data), input$target)
    checkboxGroupInput("predictors", "Predictor variables",
                       choices = candidates, selected = candidates)
  })

  output$categorical_selector <- renderUI({
    req(rv$data, input$predictors, rv$categoricals)
    # Only show predictors that were selected
    preds <- input$predictors
    auto_cats <- names(rv$categoricals[rv$categoricals])
    default_cats <- intersect(auto_cats, preds)

    checkboxGroupInput("categoricals", "Flag as categorical",
                       choices = preds, selected = default_cats)
  })

  # --- Allowed Interaction Matrix ---
  output$allowed_matrix_ui <- renderUI({
    req(input$predictors)
    preds <- input$predictors
    if (length(preds) < 2) {
      return(p("Need at least 2 predictors for interactions."))
    }

    # Build upper triangular checkboxes
    n <- length(preds)
    rows <- list()
    for (i in seq_len(n - 1)) {
      for (j in (i + 1):n) {
        id <- paste0("allowed_", i, "_", j)
        rows <- c(rows, list(
          div(
            style = "display: inline-block; margin: 2px 6px;",
            checkboxInput(id,
                          label = paste(preds[i], "\u2194", preds[j]),
                          value = TRUE)
          )
        ))
      }
    }
    div(
      style = "max-height: 250px; overflow-y: auto; border: 1px solid #ddd; padding: 8px; border-radius: 4px;",
      rows
    )
  })

  # Helper: collect the allowed matrix from UI checkboxes
  get_allowed_matrix <- reactive({
    req(input$predictors)
    preds <- input$predictors
    mat <- build_allowed_matrix(preds)

    if (length(preds) >= 2 && as.integer(input$degree) >= 2) {
      n <- length(preds)
      for (i in seq_len(n - 1)) {
        for (j in (i + 1):n) {
          id <- paste0("allowed_", i, "_", j)
          val <- input[[id]]
          if (!is.null(val) && !isTRUE(val)) {
            mat[preds[i], preds[j]] <- FALSE
            mat[preds[j], preds[i]] <- FALSE
          }
        }
      }
    }
    mat
  })

  # --- Fit Model ---
  observeEvent(input$run_model, {
    req(rv$data, input$target, input$predictors)

    degree <- as.integer(input$degree)

    # Build allowed function if degree >= 2
    allowed_func <- NULL
    if (degree >= 2) {
      allowed_func <- build_allowed_function(get_allowed_matrix())
    }

    # Collect advanced parameters (NA -> NULL)
    na_to_null <- function(x) if (is.na(x) || is.null(x)) NULL else x

    nprune <- na_to_null(input$nprune)
    thresh <- na_to_null(input$thresh)
    penalty <- na_to_null(input$penalty)
    minspan <- na_to_null(input$minspan)
    endspan <- na_to_null(input$endspan)
    fast_k <- na_to_null(input$fast_k)
    nfold <- na_to_null(input$nfold_override)

    glm_arg <- NULL
    if (input$glm_family != "none") {
      glm_arg <- list(family = input$glm_family)
    }

    pmethod <- input$pmethod

    tryCatch({
      rv$result <- fit_earth(
        df = rv$data,
        target = input$target,
        predictors = input$predictors,
        categoricals = input$categoricals,
        degree = degree,
        allowed_func = allowed_func,
        nfold = nfold,
        nprune = nprune,
        thresh = thresh,
        penalty = penalty,
        minspan = minspan,
        endspan = endspan,
        fast.k = fast_k,
        pmethod = pmethod,
        glm = glm_arg
      )
    }, error = function(e) {
      showNotification(paste("Model error:", e$message),
                       type = "error", duration = 10)
    })
  })

  # --- Results: Summary ---
  output$summary_metrics <- renderUI({
    req(rv$result)
    s <- format_summary(rv$result)

    metrics <- tagList(
      tags$div(
        class = "row",
        style = "margin-bottom: 15px;",
        tags$div(class = "col-md-3",
                 tags$div(class = "card text-center",
                          style = "padding: 10px;",
                          tags$h6("R\u00b2"), tags$h4(sprintf("%.4f", s$r_squared)))),
        tags$div(class = "col-md-3",
                 tags$div(class = "card text-center",
                          style = "padding: 10px;",
                          tags$h6("GRSq"), tags$h4(sprintf("%.4f", s$grsq)))),
        tags$div(class = "col-md-3",
                 tags$div(class = "card text-center",
                          style = "padding: 10px;",
                          tags$h6("GCV"), tags$h4(sprintf("%.2f", s$gcv)))),
        tags$div(class = "col-md-3",
                 tags$div(class = "card text-center",
                          style = "padding: 10px;",
                          tags$h6("Terms"), tags$h4(s$n_terms)))
      )
    )

    if (rv$result$cv_enabled && !is.na(s$cv_rsq)) {
      metrics <- tagList(
        metrics,
        tags$div(
          class = "alert alert-info",
          style = "font-size: 0.9em;",
          sprintf("Cross-validated R\u00b2: %.4f  |  Training R\u00b2: %.4f",
                  s$cv_rsq, s$r_squared),
          if (s$r_squared - s$cv_rsq > 0.1) {
            tags$span(style = "color: red; font-weight: bold;",
                      " \u26a0 Possible overfitting detected")
          }
        )
      )
    }

    metrics
  })

  output$summary_table <- DT::renderDataTable({
    req(rv$result)
    s <- format_summary(rv$result)
    DT::datatable(s$coefficients, options = list(pageLength = 20),
                  rownames = FALSE)
  })

  # --- Results: Variable Importance ---
  output$importance_plot <- renderPlot({
    req(rv$result)
    plot_variable_importance(rv$result)
  })

  output$importance_table <- DT::renderDataTable({
    req(rv$result)
    DT::datatable(format_variable_importance(rv$result),
                  options = list(pageLength = 20), rownames = FALSE)
  })

  # --- Results: Partial Dependence ---
  output$pd_variable_selector <- renderUI({
    req(rv$result)
    imp <- format_variable_importance(rv$result)
    vars <- if (nrow(imp) > 0) imp$variable else rv$result$predictors
    selectInput("pd_variable", "Select variable", choices = vars)
  })

  output$pd_plot <- renderPlot({
    req(rv$result, input$pd_variable)
    plot_partial_dependence(rv$result, input$pd_variable)
  })

  # --- Results: Diagnostics ---
  output$residuals_plot <- renderPlot({
    req(rv$result)
    plot_residuals(rv$result)
  })

  output$qq_plot <- renderPlot({
    req(rv$result)
    plot_qq(rv$result)
  })

  output$actual_vs_predicted_plot <- renderPlot({
    req(rv$result)
    plot_actual_vs_predicted(rv$result)
  })

  # --- Results: ANOVA ---
  output$anova_table <- DT::renderDataTable({
    req(rv$result)
    DT::datatable(format_anova(rv$result),
                  options = list(pageLength = 20), rownames = FALSE)
  })

  # --- Export Report ---
  output$export_report <- downloadHandler(
    filename = function() {
      ext <- input$export_format
      paste0("earth_report_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".", ext)
    },
    content = function(file) {
      req(rv$result)
      tryCatch({
        render_report(rv$result,
                      output_format = input$export_format,
                      output_file = file)
      }, error = function(e) {
        showNotification(paste("Export error:", e$message),
                         type = "error", duration = 10)
      })
    }
  )
}
