function(input, output, session) {

  # --- Reactive values ---
  rv <- reactiveValues(
    data = NULL,
    categoricals = NULL,
    result = NULL,
    file_name = NULL,
    sheets = NULL,
    file_path = NULL,
    file_ext = NULL
  )

  # --- Data Import ---
  observeEvent(input$file_input, {
    req(input$file_input)
    message("earthui: file upload received: ", input$file_input$name)
    message("earthui: datapath = ", input$file_input$datapath)
    message("earthui: file exists = ", file.exists(input$file_input$datapath))
    ext <- tolower(tools::file_ext(input$file_input$name))
    rv$file_ext <- ext
    rv$file_path <- input$file_input$datapath
    rv$file_name <- input$file_input$name
    if (ext %in% c("xlsx", "xls")) {
      rv$sheets <- readxl::excel_sheets(input$file_input$datapath)
    } else {
      rv$sheets <- NULL
    }
    tryCatch({
      rv$data <- import_data(input$file_input$datapath, sheet = 1)
      rv$categoricals <- detect_categoricals(rv$data)
      rv$result <- NULL
      message("earthui: import OK, ", nrow(rv$data), " rows, ", ncol(rv$data), " cols")
    }, error = function(e) {
      message("earthui: IMPORT ERROR: ", e$message)
      showNotification(paste("Import error:", e$message),
                       type = "error", duration = 15)
    })
  })

  output$sheet_selector <- renderUI({
    req(rv$sheets)
    selectInput("sheet", "Sheet", choices = rv$sheets, selected = rv$sheets[1])
  })

  observeEvent(input$sheet, {
    req(rv$file_path, input$sheet)
    rv$data <- import_data(rv$file_path, sheet = input$sheet)
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

  # Shared DataTable callback for click-to-popup on cells
  cell_popup_js <- DT::JS("
    table.on('click', 'td', function() {
      var text = $(this).text();
      if (text.length > 0) {
        var $popup = $('#eui-cell-popup');
        if (!$popup.length) {
          $popup = $('<div id=\"eui-cell-popup\">' +
            '<div class=\"eui-popup-backdrop\"></div>' +
            '<div class=\"eui-popup-content\"><pre></pre>' +
            '<button class=\"btn btn-sm btn-secondary eui-popup-close\">Close</button></div></div>');
          $('body').append($popup);
          $popup.on('click', '.eui-popup-backdrop, .eui-popup-close', function() {
            $popup.hide();
          });
        }
        $popup.find('pre').text(text);
        $popup.show();
      }
    });
  ")

  output$data_preview <- DT::renderDataTable({
    req(rv$data)
    DT::datatable(rv$data,
                  options = list(pageLength = 10, scrollX = TRUE),
                  rownames = FALSE,
                  class = "compact stripe",
                  callback = cell_popup_js)
  })

  output$data_preview_tab <- DT::renderDataTable({
    req(rv$data)
    DT::datatable(rv$data,
                  options = list(pageLength = 10, scrollX = TRUE),
                  rownames = FALSE,
                  class = "compact stripe",
                  callback = cell_popup_js)
  })

  # --- Variable Configuration ---
  output$target_selector <- renderUI({
    req(rv$data)
    storage_key <- if (is.null(rv$file_name)) "default" else rv$file_name

    # JavaScript: persist target variable + advanced parameters in localStorage
    js <- tags$script(HTML(sprintf("
      (function() {
        var storageKey = 'earthui_settings_' + %s;
        var selectIds = ['target', 'degree', 'pmethod', 'glm_family'];
        var numericIds = ['nprune', 'thresh', 'penalty', 'minspan', 'endspan',
                          'fast_k', 'nfold_override'];
        var allIds = selectIds.concat(numericIds);

        var saved = null;
        try { saved = JSON.parse(localStorage.getItem(storageKey)); } catch(e) {}

        function restoreSettings() {
          if (!saved) return;
          selectIds.forEach(function(id) {
            if (saved[id] !== undefined && saved[id] !== null) {
              var el = document.getElementById(id);
              if (el && el.selectize) {
                if (id === 'target') {
                  if (el.selectize.options[saved[id]]) {
                    el.selectize.setValue(saved[id]);
                  }
                } else {
                  el.selectize.setValue(saved[id]);
                }
              }
            }
          });
          numericIds.forEach(function(id) {
            if (saved[id] !== undefined) {
              var $el = $('#' + id);
              if ($el.length) { $el.val(saved[id]).trigger('change'); }
            }
          });
        }

        function saveSettings() {
          var state = {};
          selectIds.forEach(function(id) {
            var el = document.getElementById(id);
            if (el && el.selectize) { state[id] = el.selectize.getValue(); }
          });
          numericIds.forEach(function(id) {
            state[id] = $('#' + id).val();
          });
          try { localStorage.setItem(storageKey, JSON.stringify(state)); } catch(e) {}
        }

        // Restore after selectize initializes
        setTimeout(restoreSettings, 500);

        // Save on any tracked input change
        $(document).off('shiny:inputchanged.euisettings')
                   .on('shiny:inputchanged.euisettings', function(event) {
          if (allIds.indexOf(event.name) >= 0) { saveSettings(); }
        });
      })();
    ", jsonlite::toJSON(storage_key, auto_unbox = TRUE))))

    tagList(
      selectInput("target", "Target (response) variable",
                  choices = names(rv$data)),
      js
    )
  })

  output$variable_table <- renderUI({
    req(rv$data, input$target)
    candidates <- setdiff(names(rv$data), input$target)
    nrows <- nrow(rv$data)

    # Storage key for remembering settings
    storage_key <- if (is.null(rv$file_name)) "default" else rv$file_name

    # Header row
    header <- tags$div(
      style = "display: flex; align-items: center; padding: 4px 0; border-bottom: 2px solid #ccc; font-weight: bold; font-size: 0.85em;",
      tags$div(style = "flex: 1; min-width: 120px;", "Variable"),
      tags$div(style = "width: 45px; text-align: center;", "Inc?"),
      tags$div(style = "width: 55px; text-align: center;", "Factor"),
      tags$div(style = "width: 55px; text-align: center;", "Linear"),
      tags$div(style = "width: 50px; text-align: right; padding-right: 4px;", "NAs")
    )

    # Build rows using numeric index for IDs
    rows <- lapply(seq_along(candidates), function(i) {
      col <- candidates[i]
      n_na <- sum(is.na(rv$data[[col]]))
      pct_na <- n_na / nrows
      na_style <- if (pct_na > 0.3) "color: red;" else ""

      tags$div(
        style = "display: flex; align-items: center; padding: 2px 0; border-bottom: 1px solid #eee;",
        tags$div(style = "flex: 1; min-width: 120px; font-size: 0.82em; overflow: hidden; text-overflow: ellipsis; white-space: nowrap;",
                 title = col, col),
        tags$div(style = "width: 45px; text-align: center;",
                 tags$input(type = "checkbox", id = paste0("eui_inc_", i),
                            class = "eui-var-cb")),
        tags$div(style = "width: 55px; text-align: center;",
                 tags$input(type = "checkbox", id = paste0("eui_fac_", i),
                            class = "eui-var-cb")),
        tags$div(style = "width: 55px; text-align: center;",
                 tags$input(type = "checkbox", id = paste0("eui_lin_", i),
                            class = "eui-var-cb")),
        tags$div(style = paste0("width: 50px; text-align: right; font-size: 0.8em; padding-right: 4px;", na_style),
                 if (n_na > 0L) as.character(n_na) else "")
      )
    })

    # JavaScript: sync checkboxes <-> Shiny inputs, with localStorage persistence
    col_json <- jsonlite::toJSON(candidates, auto_unbox = FALSE)
    n_cols <- length(candidates)
    js <- tags$script(HTML(sprintf("
      (function() {
        var cols = %s;
        var n = %d;
        var storageKey = 'earthui_vars_' + %s;

        function gatherState() {
          var inc = [], fac = [], lin = [];
          for (var i = 1; i <= n; i++) {
            if ($('#eui_inc_' + i).is(':checked')) inc.push(cols[i-1]);
            if ($('#eui_fac_' + i).is(':checked')) fac.push(cols[i-1]);
            if ($('#eui_lin_' + i).is(':checked')) lin.push(cols[i-1]);
          }
          Shiny.setInputValue('predictors', inc.length > 0 ? inc : null);
          Shiny.setInputValue('categoricals', fac.length > 0 ? fac : null);
          Shiny.setInputValue('linpreds', lin.length > 0 ? lin : null);
        }

        function saveState() {
          var state = {};
          for (var i = 1; i <= n; i++) {
            state[cols[i-1]] = {
              inc: $('#eui_inc_' + i).is(':checked'),
              fac: $('#eui_fac_' + i).is(':checked'),
              lin: $('#eui_lin_' + i).is(':checked')
            };
          }
          try { localStorage.setItem(storageKey, JSON.stringify(state)); } catch(e) {}
        }

        function restoreState() {
          var saved = null;
          try { saved = JSON.parse(localStorage.getItem(storageKey)); } catch(e) {}
          if (saved) {
            for (var i = 1; i <= n; i++) {
              var s = saved[cols[i-1]];
              if (s) {
                $('#eui_inc_' + i).prop('checked', s.inc);
                $('#eui_fac_' + i).prop('checked', s.fac);
                $('#eui_lin_' + i).prop('checked', s.lin);
              }
            }
          }
        }

        // Restore saved state, then sync to Shiny
        restoreState();
        setTimeout(gatherState, 200);

        // On any checkbox change, save and sync
        $(document).off('change.euivar').on('change.euivar', '.eui-var-cb', function() {
          saveState();
          gatherState();
        });
      })();
    ", col_json, n_cols, jsonlite::toJSON(storage_key, auto_unbox = TRUE))))

    tagList(header, rows, js)
  })

  # --- Allowed Interaction Matrix ---
  output$allowed_matrix_ui <- renderUI({
    req(input$predictors)
    preds <- input$predictors
    if (length(preds) < 2) {
      return(p("Need at least 2 predictors for interactions."))
    }

    n <- length(preds)

    # Column headers: empty corner cell + variable names
    header_cells <- list(tags$th(style = "padding: 2px;", ""))
    for (j in seq_len(n)) {
      header_cells <- c(header_cells, list(
        tags$th(style = "padding: 2px 4px; font-size: 0.75em; text-align: center; writing-mode: vertical-lr; transform: rotate(180deg); max-height: 100px; overflow: hidden;",
                title = preds[j], preds[j])
      ))
    }
    header_row <- tags$tr(header_cells)

    # Build matrix rows
    table_rows <- list(header_row)
    for (i in seq_len(n)) {
      cells <- list(
        tags$td(style = "padding: 2px 4px; font-size: 0.75em; white-space: nowrap; overflow: hidden; text-overflow: ellipsis; max-width: 100px;",
                title = preds[i], preds[i])
      )
      for (j in seq_len(n)) {
        if (j > i) {
          # Upper triangle: checkbox
          id <- paste0("allowed_", i, "_", j)
          cells <- c(cells, list(
            tags$td(style = "text-align: center; padding: 2px;",
                    tags$input(type = "checkbox", id = id,
                               class = "eui-interaction-cb", checked = "checked",
                               style = "margin: 0;"))
          ))
        } else {
          # Diagonal and lower triangle: empty
          cells <- c(cells, list(
            tags$td(style = "padding: 2px;",
                    if (i == j) "\u00b7" else "")
          ))
        }
      }
      table_rows <- c(table_rows, list(tags$tr(cells)))
    }

    # JavaScript to sync checkboxes with Shiny inputs
    js <- tags$script(HTML(sprintf("
      $(document).off('change.euimatrix').on('change.euimatrix', '.eui-interaction-cb', function() {
        var n = %d;
        for (var i = 1; i < n; i++) {
          for (var j = i + 1; j <= n; j++) {
            var id = 'allowed_' + i + '_' + j;
            Shiny.setInputValue(id, $('#' + id).is(':checked'));
          }
        }
      });
      // Initial sync
      setTimeout(function() {
        var n = %d;
        for (var i = 1; i < n; i++) {
          for (var j = i + 1; j <= n; j++) {
            var id = 'allowed_' + i + '_' + j;
            Shiny.setInputValue(id, $('#' + id).is(':checked'));
          }
        }
      }, 200);
    ", n, n)))

    div(
      style = "max-height: 300px; overflow: auto; border: 1px solid #ddd; padding: 4px; border-radius: 4px;",
      tags$table(style = "border-collapse: collapse;", table_rows),
      js
    )
  })

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

    allowed_func <- NULL
    if (degree >= 2) {
      allowed_func <- build_allowed_function(get_allowed_matrix())
    }

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
    linpreds <- input$linpreds

    tryCatch({
      rv$result <- fit_earth(
        df = rv$data,
        target = input$target,
        predictors = input$predictors,
        categoricals = input$categoricals,
        linpreds = linpreds,
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

  # --- Results: Model Equation ---
  output$model_equation <- renderUI({
    req(rv$result)
    eq <- format_model_equation(rv$result)
    withMathJax(HTML(eq$latex_inline))
  })

  output$summary_table <- DT::renderDataTable({
    req(rv$result)
    s <- format_summary(rv$result)
    dt <- DT::datatable(s$coefficients, options = list(pageLength = 20),
                        rownames = FALSE, class = "compact stripe")
    numeric_cols <- names(s$coefficients)[vapply(s$coefficients, is.numeric, logical(1))]
    if (length(numeric_cols) > 0) dt <- DT::formatRound(dt, numeric_cols, digits = 6)
    dt
  })

  # --- Results: Variable Importance ---
  output$importance_plot <- renderPlot({
    req(rv$result)
    plot_variable_importance(rv$result)
  })

  output$importance_table <- DT::renderDataTable({
    req(rv$result)
    imp_df <- format_variable_importance(rv$result)
    dt <- DT::datatable(imp_df, options = list(pageLength = 20),
                        rownames = FALSE, class = "compact stripe")
    numeric_cols <- names(imp_df)[vapply(imp_df, is.numeric, logical(1))]
    if (length(numeric_cols) > 0) dt <- DT::formatRound(dt, numeric_cols, digits = 6)
    dt
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
    anova_df <- format_anova(rv$result)
    dt <- DT::datatable(anova_df, options = list(pageLength = 20),
                        rownames = FALSE, class = "compact stripe")
    numeric_cols <- names(anova_df)[vapply(anova_df, is.numeric, logical(1))]
    if (length(numeric_cols) > 0) dt <- DT::formatRound(dt, numeric_cols, digits = 6)
    dt
  })

  # --- Export Report ---
  output$export_report <- downloadHandler(
    filename = function() {
      ext <- input$export_format
      paste0("earth_report_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".", ext)
    },
    content = function(file) {
      req(rv$result)
      fmt <- input$export_format
      withProgress(message = "Rendering report...", value = 0.3, {
        tryCatch({
          render_report(rv$result,
                        output_format = fmt,
                        output_file = file)
          setProgress(1, detail = "Done")
        }, error = function(e) {
          message("earthui export error: ", e$message)
          # Write error to a text file so the download doesn't silently 404
          writeLines(paste("Report generation failed:", e$message), file)
        })
      })
    }
  )
}
