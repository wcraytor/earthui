#' Model Fit Button -- UI (sidebar)
#'
#' @param id Shiny module namespace ID.
#' @return A [shiny::tagList].
#' @export
#' @examples
#' if (interactive()) {
#'   ui <- fluidPage(mod_model_fit_ui("model1"))
#' }
mod_model_fit_ui <- function(id) {
  ns <- NS(id)
  tagList(
    actionButton(ns("fit"), "Fit Model",
                 class = "btn-primary btn-lg",
                 style = "width: 100%;"),
    textOutput(ns("fit_status"))
  )
}


#' Model Results -- UI (main panel)
#'
#' Tabbed output panel for model results, plots, and diagnostics.
#'
#' @param id Shiny module namespace ID.
#' @return A [shiny::tagList].
#' @export
mod_model_results_ui <- function(id) {
  ns <- NS(id)
  div(class = "shiny-panel-conditional",
    `data-display-if` = "output.data_imported",
    `data-ns-prefix` = "",
    style = "display:none;",
    tabsetPanel(
      id = ns("results_tabs"),
      tabPanel("Data",
        br(),
        # Appraisal/Market: split into Subject Property (row 1) and Comparable
        # Sales (rows 2+), matching earthUI / glmnetUI. General: single table.
        conditionalPanel(
          condition = "input.purpose === 'appraisal' || input.purpose === 'market'",
          h5("Subject Property"),
          DT::DTOutput(ns("preview_subjects")),
          h5("Comparable Sales"),
          DT::DTOutput(ns("preview_comps"))
        ),
        conditionalPanel(
          condition = "input.purpose === 'general'",
          DT::DTOutput(ns("data_preview_table"))
        )
      ),
      tabPanel("Equation",
        br(),
        div(style = "overflow-x: auto; padding: 10px 10px 10px 0;",
            uiOutput(ns("equation_display"))),
        hr(),
        h5("Smooth Function Definitions"),
        DT::DTOutput(ns("smooth_specs_table"))
      ),
      tabPanel("Summary",
        br(),
        uiOutput(ns("summary_metrics")),
        hr(),
        h5("Smooth Terms"),
        DT::DTOutput(ns("smooth_table")),
        h5("Parametric Terms"),
        DT::DTOutput(ns("param_table"))
      ),
      tabPanel("Variable Importance",
        br(),
        plotOutput(ns("importance_plot"), height = "400px"),
        br(),
        DT::DTOutput(ns("importance_table"))
      ),
      tabPanel("Contribution",
        br(),
        uiOutput(ns("smooth_plots_container"))
      ),
      tabPanel("Correlation",
        br(),
        uiOutput(ns("correlation_plot_ui"))
      ),
      tabPanel("Diagnostics",
        br(),
        plotOutput(ns("diagnostics_plot"), height = "550px"),
        hr(),
        plotOutput(ns("avp_plot"), height = "400px")
      ),
      tabPanel("RCA Adjustments",
        br(),
        conditionalPanel(
          condition = "!output.rca_computed",
          tags$div(
            style = "text-align:center; padding:40px 20px;",
            tags$p("Run RCA Adjustments (Step 8) to see analysis.",
                   style = "color: var(--bs-secondary-color);")
          )
        ),
        conditionalPanel(
          condition = "output.rca_computed",
          plotOutput("rca_resid_pct_plot", height = "350px"),
          hr(),
          plotOutput("rca_net_pct_plot", height = "350px"),
          hr(),
          plotOutput("rca_gross_pct_plot", height = "350px")
        )
      ),
      tabPanel("Anova",
        br(),
        DT::DTOutput(ns("anova_table"))
      ),
      tabPanel("Mgcv Output",
        br(),
        div(style = "overflow-x: auto;",
            verbatimTextOutput(ns("mgcv_output")))
      ),
      tabPanel("Sign Check",
        br(),
        DT::DTOutput(ns("sign_table")),
        textOutput(ns("sign_note"))
      ),
      tabPanel("Concurvity",
        br(),
        tags$p("Values above ~0.8 in the 'worst' row indicate near-concurvity (smooth analog of multicollinearity). Consider dropping or collapsing correlated terms.",
               style = "font-size: 0.85em; color: var(--bs-secondary-color); margin-bottom: 8px;"),
        h5("Overall Concurvity"),
        DT::DTOutput(ns("concurvity_full")),
        hr(),
        h5("Pairwise Concurvity (worst case)"),
        DT::DTOutput(ns("concurvity_pairwise"))
      ),
      tabPanel("Basis Check",
        br(),
        verbatimTextOutput(ns("gam_check"))
      )
    )
  )
}


#' Model Module -- Server (shared by fit button and results)
#'
#' @param id Shiny module namespace ID.
#' @param data_r A reactive returning the data frame.
#' @param var_config_r A reactive returning the variable configuration.
#' @param earth_knots_r A reactive returning earth knots (or `NULL`).
#' @param dark_mode_r A reactive returning `TRUE` when dark mode is active.
#' @param purpose_r A reactive giving the modeling purpose: `"general"`,
#'   `"appraisal"`, or `"market"`.
#' @return A list with `result` (reactive containing the `mgcvUI_result` or
#'   `NULL`) and `reset` (function to clear model state).
#' @export
mod_model_server <- function(id, data_r, var_config_r,
                             earth_knots_r = reactive(NULL),
                             dark_mode_r = reactive(FALSE),
                             purpose_r = reactive("general")) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    result <- reactiveVal(NULL)
    rv <- reactiveValues(bg_proc = NULL, fitting = FALSE, xform = "none",
                         aborted = FALSE)

    output$has_model <- reactive(!is.null(result()))
    outputOptions(output, "has_model", suspendWhenHidden = FALSE)

    # Double-click any preview cell to open a popup with its full text.
    # Single click is reserved for row selection (Select extension), so we
    # bind on dblclick. CSS clips the visible cell to one line; the DOM still
    # holds the full value, so .text() returns it in full.
    cell_popup_js <- DT::JS("
      table.on('dblclick', 'td', function() {
        var text = $(this).text();
        if (text.length > 0) {
          var $popup = $('#mgcv-cell-popup');
          if (!$popup.length) {
            $popup = $('<div id=\"mgcv-cell-popup\">' +
              '<div class=\"mgcv-popup-backdrop\"></div>' +
              '<div class=\"mgcv-popup-content\"><pre></pre>' +
              '<button class=\"btn btn-sm btn-secondary mgcv-popup-close\">Close</button></div></div>');
            $('body').append($popup);
            $popup.on('click', '.mgcv-popup-backdrop, .mgcv-popup-close', function() {
              $popup.hide();
            });
          }
          $popup.find('pre').text(text);
          $popup.show();
        }
      });
    ")

    # General mode: single table of all rows.
    output$data_preview_table <- DT::renderDT({
      req(data_r(), purpose_r() == "general")
      DT::datatable(data_r(),
                    extensions = "Select",
                    selection = "none",  # let the Select extension drive it
                    options = list(
                      scrollX = TRUE, pageLength = 15,
                      # OS-style: click = single, Ctrl/Cmd-click = toggle,
                      # Shift-click = range.
                      select = list(style = "os", items = "row")
                    ),
                    rownames = FALSE,
                    callback = cell_popup_js)
    })

    # Appraisal/Market mode: Subject Property (row 1).
    output$preview_subjects <- DT::renderDT({
      req(data_r(), purpose_r() %in% c("appraisal", "market"),
          nrow(data_r()) >= 1L)
      subj <- data_r()[1L, , drop = FALSE]
      DT::datatable(subj,
                    options = list(scrollX = TRUE, dom = "t"),
                    rownames = FALSE,
                    callback = cell_popup_js)
    })

    # Appraisal/Market mode: Comparable Sales (rows 2+).
    output$preview_comps <- DT::renderDT({
      req(data_r(), purpose_r() %in% c("appraisal", "market"),
          nrow(data_r()) >= 2L)
      comps <- data_r()[2:nrow(data_r()), , drop = FALSE]
      DT::datatable(comps,
                    extensions = "Select",
                    selection = "none",
                    options = list(
                      scrollX = TRUE, pageLength = 15,
                      select = list(style = "os", items = "row")
                    ),
                    rownames = FALSE,
                    callback = cell_popup_js)
    })

    observeEvent(input$fit, {
      df <- isolate(data_r())
      cfg <- isolate(var_config_r())
      req(df, cfg$response, cfg$smooth_specs)

      if (length(cfg$smooth_specs) == 0L) {
        showNotification("Select at least one predictor (check Inc).",
                         type = "warning")
        return()
      }

      # Pre-fit check: how many complete cases?
      all_vars <- unique(c(cfg$response,
        unlist(lapply(cfg$smooth_specs, `[[`, "vars"))))
      n_total <- nrow(df)
      n_complete <- sum(stats::complete.cases(df[, all_vars, drop = FALSE]))
      pct <- round(n_complete / n_total * 100, 1)

      if (n_complete < 10L) {
        showNotification(
          paste0("Only ", n_complete, " complete rows (", pct,
                 "%). Too few to fit. Exclude variables with many NAs."),
          type = "error", duration = 15)
        return()
      }

      if (pct < 50) {
        na_counts <- vapply(all_vars, function(v) sum(is.na(df[[v]])),
                            integer(1))
        na_counts <- sort(na_counts[na_counts > 0], decreasing = TRUE)
        culprits <- paste(names(na_counts), " (", na_counts, " NAs)",
                          sep = "", collapse = ", ")
        showNotification(
          paste0("Warning: only ", n_complete, " of ", n_total,
                 " rows (", pct, "%) are complete. ",
                 "NA culprits: ", culprits,
                 ". Consider excluding or unchecking these variables."),
          type = "warning", duration = 20)
      }

      rv$aborted <- FALSE
      session$sendCustomMessage("fitting_start", list())

      resp_col <- cfg$response
      xform <- cfg$response_transform %||% "none"
      fit_df <- df
      fit_specs <- cfg$smooth_specs
      fit_family <- cfg$family
      fit_method <- cfg$method
      fit_select <- cfg$select
      fit_gamma <- cfg$gamma
      fit_cv <- if (isTRUE(cfg$cv)) 10L else 0L
      fit_earth <- isolate(earth_knots_r())
      fit_optimizer <- cfg$optimizer
      fit_scale <- cfg$scale %||% 0
      fit_discrete <- cfg$discrete %||% FALSE
      fit_nthreads <- cfg$nthreads %||% 1L

      wt <- NULL
      if (!is.null(cfg$weights_col) && cfg$weights_col %in% names(fit_df)) {
        wt <- fit_df[[cfg$weights_col]]
      }

      # Apply response transform
      if (xform == "log") {
        vals <- fit_df[[resp_col]]
        if (any(vals <= 0, na.rm = TRUE)) {
          fit_df <- fit_df[vals > 0 | is.na(vals), , drop = FALSE]
        }
        fit_df[[resp_col]] <- log(fit_df[[resp_col]])
      } else if (xform == "log10") {
        vals <- fit_df[[resp_col]]
        if (any(vals <= 0, na.rm = TRUE)) {
          fit_df <- fit_df[vals > 0 | is.na(vals), , drop = FALSE]
        }
        fit_df[[resp_col]] <- log10(fit_df[[resp_col]])
      }

      message("mgcvUI: fitting with ", length(fit_specs),
              " terms, response='", resp_col, "'",
              ", transform='", xform, "'")

      rv$xform <- xform

      # Build args list for background process
      fit_args <- list(
        data = fit_df, response = resp_col,
        smooth_specs = fit_specs, family = fit_family,
        method = fit_method, earth_knots = fit_earth,
        select = fit_select, gamma = fit_gamma,
        cv_folds = fit_cv, weights = wt,
        optimizer = fit_optimizer, scale = fit_scale,
        discrete = fit_discrete, nthreads = fit_nthreads
      )

      if (requireNamespace("callr", quietly = TRUE)) {
        # Async path: spawn background R process
        rv$bg_proc <- callr::r_bg(
          function(args) {
            res <- do.call(earthUI::fit_gam, args)
            cat(sprintf("Completed in %.1f seconds\n", res$elapsed))
            res
          },
          args = list(args = fit_args),
          stdout = "|", stderr = "|",
          supervise = TRUE,
          wd = tempdir()
        )
        rv$fitting <- TRUE
      } else {
        # Sync fallback
        session$onFlushed(function() {
          tryCatch({
            res <- do.call(fit_gam, fit_args)
            res$response_transform <- rv$xform
            res$fit_ts <- Sys.time()   # canonical fit time for all outputs
            result(res)
            session$sendCustomMessage("mgcv_show_check",
                                      list(id = ns("fit")))
            session$sendCustomMessage("fitting_done",
              list(text = paste0("Done in ", round(res$elapsed, 1), "s")))
          }, error = function(e) {
            session$sendCustomMessage("fitting_done",
              list(text = paste0("Error: ", e$message)))
          })
        }, once = TRUE)
      }
    })

    # --- Abort handler ---
    observeEvent(input$abort_fit, {
      rv$aborted <- TRUE
      proc <- rv$bg_proc
      if (!is.null(proc) && proc$is_alive()) {
        proc$kill()
        message("mgcvUI: model fit aborted by user")
      }
      rv$bg_proc <- NULL
      rv$fitting <- FALSE
      # Discard any result from this aborted run so the tabs don't populate,
      # even if the background process finished a beat before the abort landed.
      result(NULL)
      session$sendCustomMessage("fitting_done", list(text = "Aborted"))
    })

    # --- Polling observer for background process ---
    observe({
      req(rv$fitting)
      invalidateLater(300)
      proc <- rv$bg_proc
      if (is.null(proc)) return()

      # Read stdout lines and send to trace log
      out_lines <- tryCatch(proc$read_output_lines(), error = function(e) character(0))
      for (ln in out_lines) {
        if (nzchar(ln)) {
          session$sendCustomMessage("trace_line", list(text = ln))
          message("  ", ln)
        }
      }

      # Read stderr lines too
      err_lines <- tryCatch(proc$read_error_lines(), error = function(e) character(0))
      for (ln in err_lines) {
        if (nzchar(ln)) {
          session$sendCustomMessage("trace_line", list(text = ln))
          message("  ", ln)
        }
      }

      # Check if process finished
      if (!proc$is_alive()) {
        rv$fitting <- FALSE
        rv$bg_proc <- NULL
        # User aborted: discard the result so the tabs do NOT populate.
        if (isTRUE(rv$aborted)) {
          tryCatch(proc$kill(), error = function(e) NULL)
          return()
        }
        tryCatch({
          res <- proc$get_result()
          res$response_transform <- rv$xform
          res$fit_ts <- Sys.time()   # canonical fit time for all outputs
          result(res)
          session$sendCustomMessage("mgcv_show_check",
                                    list(id = ns("fit")))
          session$sendCustomMessage("fitting_done",
            list(text = paste0("Done in ", round(res$elapsed, 1), "s")))
        }, error = function(e) {
          message("mgcvUI fitting error: ", e$message)
          session$sendCustomMessage("fitting_done",
            list(text = paste0("Error: ", e$message)))
        })
        rv$bg_proc <- NULL
      }
    })

    output$fit_status <- renderText({
      res <- result()
      if (is.null(res)) return("")
      tryCatch({
        summ <- format_gam_summary(res)
        cv_part <- if (!is.null(summ$cv_rsq)) {
          paste0("  CVR\u00b2=", round(summ$cv_rsq, 4))
        } else {
          ""
        }
        xform <- res$response_transform %||% "none"
        scale_note <- if (xform != "none") paste0(" (", xform, " scale)") else ""
        paste0("R\u00b2=", round(summ$r_squared, 4),
               cv_part,
               "  Dev=", round(summ$dev_explained * 100, 1), "%",
               "  AIC=", round(summ$aic, 1),
               "  n=", summ$n_obs, scale_note)
      }, error = function(e) {
        message("mgcvUI format_gam_summary error: ", e$message)
        paste("Summary error:", e$message)
      })
    })

    # --- Equation tab ---
    output$equation_display <- renderUI({
      res <- result()
      req(res)
      model <- res$model
      response <- res$response

      fam <- model$family
      link_name <- fam$link

      # Shared equation builder (identical notation in the Quarto report).
      eq <- format_gam_equation_(model, response, res$response_transform)
      smooth_defs <- eq$defs
      latex <- paste0("$$", eq$inline, "$$")

      family_info <- paste0("Family: ", fam$family,
                            "(link = \"", link_name, "\")")
      method_info <- paste0("Method: ", model$method)

      # Build definitions section
      def_items <- lapply(smooth_defs, function(d) {
        tags$li(
          style = "margin-bottom: 4px; font-size: 0.9em;",
          withMathJax(HTML(paste0("\\(", d$fn, "\\)"))),
          HTML(paste0(" &rarr; <code>", htmltools::htmlEscape(d$label),
                      "</code> &mdash; ", htmltools::htmlEscape(d$desc)))
        )
      })

      tagList(
        tags$p(
          tags$span(family_info, style = "margin-right: 20px;"),
          tags$span(method_info),
          style = "color: var(--bs-secondary-color); margin-bottom: 10px;"
        ),
        withMathJax(HTML(latex)),
        if (length(def_items) > 0L) {
          tagList(
            tags$h6("Smooth function definitions:",
                    style = "margin-top: 16px; margin-bottom: 6px;"),
            tags$ul(style = "list-style: none; padding-left: 8px;", def_items)
          )
        },
        withMathJax(HTML(g_function_legend_mgcv("html")))
      )
    })

    # --- Equation tab: smooth function definitions table ---
    output$smooth_specs_table <- DT::renderDT({
      res <- result()
      req(res)
      model <- res$model
      specs <- res$smooth_specs

      rows <- lapply(specs, function(sp) {
        var_name <- paste(sp$vars, collapse = ", ")
        term_type <- sp$type  # "s", "te", "ti", "linear"

        # Basis type
        bs <- if (!is.null(sp$bs)) sp$bs else "tp"

        # k value
        k_val <- if (!is.null(sp$k)) as.character(sp$k) else "default"

        # Knot positions from earth (if any)
        knots <- ""
        if (!is.null(res$earth_knots) &&
            sp$vars[1] %in% names(res$earth_knots$knots)) {
          kv <- res$earth_knots$knots[[sp$vars[1]]]
          knots <- paste(round(kv, 4), collapse = ", ")
        }

        # Build the R formula term
        if (term_type == "linear") {
          formula_term <- var_name
        } else {
          args <- var_name
          if (bs != "tp") args <- paste0(args, ", bs=\"", bs, "\"")
          if (k_val != "default") args <- paste0(args, ", k=", k_val)
          formula_term <- paste0(term_type, "(", args, ")")
        }

        data.frame(
          Term = formula_term,
          Variable = var_name,
          Type = term_type,
          Basis = if (term_type == "linear") "-" else bs,
          k = k_val,
          Knots = if (nzchar(knots)) knots else "-",
          stringsAsFactors = FALSE
        )
      })

      spec_df <- do.call(rbind, rows)

      DT::datatable(spec_df, rownames = FALSE,
                    options = list(dom = "t", scrollX = TRUE,
                                  pageLength = 50),
                    class = "compact stripe")
    })

    # --- Summary tab: metric cards ---
    output$summary_metrics <- renderUI({
      res <- result()
      req(res)
      summ <- format_gam_summary(res)

      cv_text <- if (!is.null(summ$cv_rsq)) {
        sprintf("%.4f", summ$cv_rsq)
      } else {
        "N/A"
      }

      fluidRow(
        column(2, div(class = "card text-center", style = "padding: 8px;",
          tags$h6("R\u00b2"), tags$h4(sprintf("%.4f", summ$r_squared))
        )),
        column(2, div(class = "card text-center", style = "padding: 8px;",
          tags$h6("CV R\u00b2"), tags$h4(cv_text)
        )),
        column(2, div(class = "card text-center", style = "padding: 8px;",
          tags$h6("Dev. Expl."),
          tags$h4(paste0(round(summ$dev_explained * 100, 1), "%"))
        )),
        column(2, div(class = "card text-center", style = "padding: 8px;",
          tags$h6("AIC"), tags$h4(round(summ$aic, 1))
        )),
        column(2, div(class = "card text-center", style = "padding: 8px;",
          tags$h6("n"), tags$h4(summ$n_obs)
        )),
        column(2, div(class = "card text-center", style = "padding: 8px;",
          tags$h6("Smooths"), tags$h4(summ$n_smooths)
        ))
      )
    })

    output$smooth_table <- DT::renderDT({
      req(result())
      summ <- format_gam_summary(result())
      if (nrow(summ$smooth_table) == 0L) return(NULL)
      st <- summ$smooth_table
      st$p_value <- format.pval(st$p_value, digits = 3)
      DT::datatable(st, rownames = FALSE,
                    options = list(dom = "t", scrollX = TRUE))
    })

    output$param_table <- DT::renderDT({
      req(result())
      summ <- format_gam_summary(result())
      if (nrow(summ$parametric_table) == 0L) return(NULL)
      pt <- summ$parametric_table
      pt$p_value <- format.pval(pt$p_value, digits = 3)
      DT::datatable(pt, rownames = FALSE,
                    options = list(dom = "t", scrollX = TRUE))
    })

    smooth_vars_r <- reactive({
      res <- result()
      req(res)
      # Only univariate s() terms
      sv <- vapply(res$smooth_specs, function(s) {
        if (s$type == "s" && length(s$vars) == 1L) s$vars[1] else NA_character_
      }, character(1))
      unique(sv[!is.na(sv)])
    })

    interaction_terms_r <- reactive({
      res <- result()
      req(res)
      # 2D interaction terms: ti() and te() with exactly 2 variables
      its <- Filter(function(s) {
        s$type %in% c("ti", "te") && length(s$vars) == 2L
      }, res$smooth_specs)
      # Deduplicate by variable pair
      seen <- character(0)
      unique_its <- list()
      for (s in its) {
        key <- paste(sort(s$vars), collapse = ",")
        if (!key %in% seen) {
          seen <- c(seen, key)
          unique_its <- c(unique_its, list(s))
        }
      }
      unique_its
    })

    by_smooth_terms_r <- reactive({
      res <- result()
      req(res)
      # Factor-by-smooth terms: s(var, by=factor)
      bys <- Filter(function(s) {
        s$type == "s" && length(s$vars) == 1L && !is.null(s$by) && nzchar(s$by)
      }, res$smooth_specs)
      # Deduplicate by var+by pair
      seen <- character(0)
      unique_bys <- list()
      for (s in bys) {
        key <- paste0(s$vars, ":by:", s$by)
        if (!key %in% seen) {
          seen <- c(seen, key)
          unique_bys <- c(unique_bys, list(s))
        }
      }
      unique_bys
    })

    parametric_terms_r <- reactive({
      res <- result()
      req(res)
      model <- res$model
      # Get term names from predict(type="terms") that are NOT smooth terms
      pred_cols <- colnames(predict(model, type = "terms"))
      smooth_labels <- vapply(model$smooth, function(sm) sm$label,
                              character(1))
      setdiff(pred_cols, smooth_labels)
    })

    output$smooth_plots_container <- renderUI({
      sv <- smooth_vars_r()
      its <- interaction_terms_r()
      bys <- by_smooth_terms_r()
      pts <- parametric_terms_r()
      if (length(sv) == 0L && length(its) == 0L &&
          length(bys) == 0L && length(pts) == 0L) {
        return(tags$p("No model terms to plot."))
      }
      border_style <- paste("border: 2px solid var(--bs-border-color);",
                            "border-radius: 6px; padding: 8px;",
                            "margin-bottom: 24px;")
      plot_outputs <- lapply(sv, function(var) {
        id <- ns(paste0("plotly_", var))
        tags$div(style = border_style,
                 plotly::plotlyOutput(id, height = "400px"))
      })
      by_outputs <- lapply(seq_along(bys), function(j) {
        id <- ns(paste0("plotly_by_", j))
        tags$div(style = border_style,
                 plotly::plotlyOutput(id, height = "400px"))
      })
      int_outputs <- lapply(seq_along(its), function(j) {
        id <- ns(paste0("plotly_int_", j))
        tags$div(style = border_style,
                 plotly::plotlyOutput(id, height = "450px"))
      })
      param_outputs <- lapply(pts, function(term) {
        safe_id <- gsub("[^a-zA-Z0-9_]", "_", term)
        id <- ns(paste0("plotly_param_", safe_id))
        tags$div(style = border_style,
                 plotly::plotlyOutput(id, height = "400px"))
      })
      do.call(tagList, c(plot_outputs, by_outputs, int_outputs, param_outputs))
    })

    observe({
      res <- result()
      req(res)
      sv <- smooth_vars_r()
      ek <- isolate(earth_knots_r())

      lapply(sv, function(var) {
        local({
          v <- var
          my_res <- res
          my_ek <- ek
          output[[paste0("plotly_", v)]] <- plotly::renderPlotly({
            is_dark <- dark_mode_r()
            tryCatch(
              plot_smooth_interactive(my_res, v, earth_knots = my_ek,
                                     dark_mode = is_dark),
              error = function(e) {
                message("mgcvUI: plot error for '", v, "': ", e$message)
                plotly::plot_ly(type = "scatter", mode = "lines",
                                x = 0, y = 0) |>
                  plotly::layout(
                    title = paste("Error:", e$message),
                    xaxis = list(title = v),
                    yaxis = list(title = "")
                  )
              }
            )
          })
        })
      })
    })

    observe({
      res <- result()
      req(res)
      bys <- by_smooth_terms_r()
      lapply(seq_along(bys), function(j) {
        local({
          idx <- j
          spec <- bys[[idx]]
          my_res <- res
          output[[paste0("plotly_by_", idx)]] <- plotly::renderPlotly({
            is_dark <- dark_mode_r()
            tryCatch(
              plot_by_smooth_interactive(
                my_res, spec$vars, by_var = spec$by, dark_mode = is_dark),
              error = function(e) {
                message("mgcvUI: by-smooth plot error for '",
                        spec$vars, " by ", spec$by, "': ", e$message)
                plotly::plot_ly(type = "scatter", mode = "lines",
                                x = 0, y = 0) |>
                  plotly::layout(
                    title = paste("Error:", e$message),
                    xaxis = list(title = spec$vars),
                    yaxis = list(title = "")
                  )
              }
            )
          })
        })
      })
    })

    observe({
      res <- result()
      req(res)
      its <- interaction_terms_r()
      lapply(seq_along(its), function(j) {
        local({
          idx <- j
          spec <- its[[idx]]
          my_res <- res
          output[[paste0("plotly_int_", idx)]] <- plotly::renderPlotly({
            is_dark <- dark_mode_r()
            tryCatch(
              plot_interaction_interactive(
                my_res, spec$vars, type = spec$type, dark_mode = is_dark),
              error = function(e) {
                message("mgcvUI: interaction plot error for '",
                        paste(spec$vars, collapse = ","), "': ", e$message)
                plotly::plot_ly(type = "scatter", mode = "lines",
                                x = 0, y = 0) |>
                  plotly::layout(
                    title = paste("Error:", e$message),
                    xaxis = list(title = paste(spec$vars, collapse = ", ")),
                    yaxis = list(title = "")
                  )
              }
            )
          })
        })
      })
    })

    observe({
      res <- result()
      req(res)
      pts <- parametric_terms_r()
      lapply(pts, function(term) {
        local({
          my_term <- term
          my_res <- res
          safe_id <- gsub("[^a-zA-Z0-9_]", "_", my_term)
          output[[paste0("plotly_param_", safe_id)]] <- plotly::renderPlotly({
            is_dark <- dark_mode_r()
            tryCatch(
              plot_parametric_interactive(my_res, my_term, dark_mode = is_dark),
              error = function(e) {
                message("mgcvUI: parametric plot error for '",
                        my_term, "': ", e$message)
                plotly::plot_ly(type = "scatter", mode = "lines",
                                x = 0, y = 0) |>
                  plotly::layout(
                    title = paste("Error:", e$message),
                    xaxis = list(title = my_term),
                    yaxis = list(title = "")
                  )
              }
            )
          })
        })
      })
    })

    output$diagnostics_plot <- renderPlot({
      req(result())
      plot_diagnostics(result())
    })

    output$avp_plot <- renderPlot({
      req(result())
      plot_actual_vs_predicted_mgcv(result())
    })

    output$sign_table <- DT::renderDT({
      req(result())
      signs <- check_sign_consistency(result())
      if (is.null(signs)) {
        return(DT::datatable(
          data.frame(Note = "No earth model imported."),
          rownames = FALSE, options = list(dom = "t")
        ))
      }
      DT::datatable(signs, rownames = FALSE,
                    options = list(dom = "t"))
    })

    output$sign_note <- renderText({
      req(result())
      if (is.null(earth_knots_r())) {
        return("Import an earthUI result to enable sign checks.")
      }
      signs <- check_sign_consistency(result())
      if (is.null(signs)) return("")
      n_warn <- sum(!signs$consistent)
      if (n_warn == 0L) {
        "All directions consistent."
      } else {
        paste(n_warn, "variable(s) show direction inconsistency.")
      }
    })

    output$gam_check <- renderPrint({
      req(result())
      mgcv::gam.check(result()$model)
    })

    # --- Concurvity tab ---
    output$concurvity_full <- DT::renderDT({
      res <- result()
      req(res)
      model <- res$model
      if (length(model$smooth) == 0L) return(NULL)
      conc <- tryCatch(
        mgcv::concurvity(model, full = TRUE),
        error = function(e) NULL
      )
      if (is.null(conc)) return(NULL)
      conc_df <- as.data.frame(round(conc, 4))
      conc_df$Measure <- rownames(conc_df)
      conc_df <- conc_df[, c("Measure", setdiff(names(conc_df), "Measure")),
                         drop = FALSE]
      DT::datatable(conc_df, rownames = FALSE,
                    options = list(dom = "t", scrollX = TRUE)) |>
        DT::formatStyle(
          columns = setdiff(names(conc_df), "Measure"),
          backgroundColor = DT::styleInterval(
            c(0.5, 0.8), c("white", "#fff3cd", "#f8d7da")
          )
        )
    })

    output$concurvity_pairwise <- DT::renderDT({
      res <- result()
      req(res)
      model <- res$model
      if (length(model$smooth) < 2L) {
        return(DT::datatable(
          data.frame(Note = "Need 2+ smooth terms for pairwise concurvity."),
          rownames = FALSE, options = list(dom = "t")
        ))
      }
      conc <- tryCatch(
        mgcv::concurvity(model, full = FALSE),
        error = function(e) NULL
      )
      if (is.null(conc)) return(NULL)
      # concurvity(full=FALSE) returns a list of 3 matrices: worst, observed, estimate
      worst <- conc$worst
      if (is.null(worst)) return(NULL)
      worst_df <- as.data.frame(round(worst, 4))
      worst_df$Term <- rownames(worst_df)
      worst_df <- worst_df[, c("Term", setdiff(names(worst_df), "Term")),
                           drop = FALSE]
      DT::datatable(worst_df, rownames = FALSE,
                    options = list(dom = "t", scrollX = TRUE)) |>
        DT::formatStyle(
          columns = setdiff(names(worst_df), "Term"),
          backgroundColor = DT::styleInterval(
            c(0.5, 0.8), c("white", "#fff3cd", "#f8d7da")
          )
        )
    })

    # --- Variable Importance tab ---
    output$importance_plot <- renderPlot({
      res <- result()
      req(res)
      summ <- format_gam_summary(res)

      imp_df <- data.frame(
        Term = character(0), Statistic = numeric(0),
        Type = character(0), stringsAsFactors = FALSE
      )

      if (nrow(summ$smooth_table) > 0) {
        imp_df <- rbind(imp_df, data.frame(
          Term = summ$smooth_table$Term,
          Statistic = summ$smooth_table$F,
          Type = "Smooth",
          stringsAsFactors = FALSE
        ))
      }

      if (nrow(summ$parametric_table) > 0) {
        pt <- summ$parametric_table[
          summ$parametric_table$Term != "(Intercept)", , drop = FALSE
        ]
        if (nrow(pt) > 0) {
          imp_df <- rbind(imp_df, data.frame(
            Term = pt$Term,
            Statistic = abs(pt$t_value),
            Type = "Parametric",
            stringsAsFactors = FALSE
          ))
        }
      }

      if (nrow(imp_df) == 0) return(NULL)

      imp_df$Term <- factor(imp_df$Term,
                            levels = imp_df$Term[order(imp_df$Statistic)])

      ggplot2::ggplot(imp_df,
                      ggplot2::aes(x = .data$Statistic, y = .data$Term,
                                   fill = .data$Type)) +
        ggplot2::geom_col(alpha = 0.85) +
        ggplot2::scale_fill_manual(
          values = c("Smooth" = "#5e81ac", "Parametric" = "#a3be8c")
        ) +
        ggplot2::labs(title = "Variable Importance (F / |t| statistic)",
                      x = "Statistic", y = NULL) +
        ggplot2::theme_minimal(base_size = 15) +
        mgcv_axis_fonts_()
    })

    output$importance_table <- DT::renderDT({
      res <- result()
      req(res)
      summ <- format_gam_summary(res)

      imp_rows <- list()

      if (nrow(summ$smooth_table) > 0) {
        for (i in seq_len(nrow(summ$smooth_table))) {
          imp_rows[[length(imp_rows) + 1L]] <- data.frame(
            Term = summ$smooth_table$Term[i],
            Type = "Smooth",
            EDF = round(summ$smooth_table$EDF[i], 2),
            Statistic = round(summ$smooth_table$F[i], 4),
            p_value = summ$smooth_table$p_value[i],
            stringsAsFactors = FALSE
          )
        }
      }

      if (nrow(summ$parametric_table) > 0) {
        pt <- summ$parametric_table[
          summ$parametric_table$Term != "(Intercept)", , drop = FALSE
        ]
        if (nrow(pt) > 0) {
          for (i in seq_len(nrow(pt))) {
            imp_rows[[length(imp_rows) + 1L]] <- data.frame(
              Term = pt$Term[i],
              Type = "Parametric",
              EDF = 1.0,
              Statistic = round(abs(pt$t_value[i]), 4),
              p_value = pt$p_value[i],
              stringsAsFactors = FALSE
            )
          }
        }
      }

      if (length(imp_rows) == 0) return(NULL)

      imp_df <- do.call(rbind, imp_rows)
      imp_df <- imp_df[order(imp_df$Statistic, decreasing = TRUE), ]
      imp_df$p_value <- format.pval(imp_df$p_value, digits = 3)

      DT::datatable(imp_df, rownames = FALSE,
                    options = list(dom = "t", scrollX = TRUE,
                                  pageLength = 50),
                    class = "compact stripe")
    })

    # --- Correlation tab ---
    output$correlation_plot_ui <- renderUI({
      req(result())
      plotOutput(ns("correlation_plot"), height = "700px", width = "700px")
    })

    output$correlation_plot <- renderPlot({
      res <- result()
      req(res)
      model <- res$model

      # Get numeric predictors from model frame
      mf <- model$model
      pred_data <- mf[, setdiff(names(mf), res$response), drop = FALSE]

      num_cols <- vapply(pred_data, is.numeric, logical(1))
      pred_data <- pred_data[, num_cols, drop = FALSE]

      if (ncol(pred_data) < 2L) {
        graphics::plot.new()
        graphics::text(0.5, 0.5,
                       "Need at least 2 numeric predictors for correlation.",
                       cex = 1.2)
        return()
      }

      cor_mat <- stats::cor(pred_data, use = "pairwise.complete.obs")

      vars <- colnames(cor_mat)
      cor_long <- expand.grid(Var1 = vars, Var2 = vars,
                              stringsAsFactors = FALSE)
      cor_long$value <- as.numeric(cor_mat)
      cor_long$Var1 <- factor(cor_long$Var1, levels = rev(vars))
      cor_long$Var2 <- factor(cor_long$Var2, levels = vars)

      n_vars <- length(vars)
      # Sizing and label color match earthUI's correlation matrix. The cell
      # label color is chosen PER CELL by tile luminance (white on saturated
      # red/blue, black on pale tiles) and set explicitly — the tile fills
      # never change with the app theme, so the labels must not follow the
      # dark-mode text color either (light grey on a white tile is invisible).
      txt_size <- if (n_vars <= 6) 7 else if (n_vars <= 10) 5
                  else if (n_vars <= 15) 4 else 3.2
      axis_size <- if (n_vars <= 6) 14 else if (n_vars <= 10) 13
                   else if (n_vars <= 15) 11 else 9
      text_color <- ifelse(abs(cor_long$value) > 0.65, "white", "black")

      ggplot2::ggplot(cor_long,
                      ggplot2::aes(x = .data$Var2, y = .data$Var1,
                                   fill = .data$value)) +
        ggplot2::geom_tile(color = "white", linewidth = 0.5) +
        ggplot2::geom_text(
          ggplot2::aes(label = sprintf("%.2f", .data$value)),
          size = txt_size, color = text_color
        ) +
        ggplot2::scale_fill_gradient2(
          low = "#2166AC", mid = "white", high = "#B2182B",
          midpoint = 0, limits = c(-1, 1), name = "Correlation"
        ) +
        ggplot2::coord_fixed() +
        ggplot2::labs(title = "Correlation Matrix", x = NULL, y = NULL) +
        ggplot2::theme_minimal(base_size = 14) +
        ggplot2::theme(
          plot.title = ggplot2::element_text(face = "bold", size = 16),
          axis.text.x = ggplot2::element_text(angle = 45, hjust = 1,
                                              size = axis_size),
          axis.text.y = ggplot2::element_text(size = axis_size),
          panel.grid = ggplot2::element_blank(),
          legend.text = ggplot2::element_text(size = 11),
          legend.title = ggplot2::element_text(size = 12)
        )
    }, res = 120)

    # --- Anova tab ---
    output$anova_table <- DT::renderDT({
      res <- result()
      req(res)

      aov <- summary(res$model)
      parts <- list()

      # Parametric terms
      if (!is.null(aov$p.table) && nrow(aov$p.table) > 0) {
        pt <- as.data.frame(aov$p.table)
        pt$Term <- rownames(pt)
        pt$Type <- "parametric"
        parts <- c(parts, list(pt))
      }

      # Smooth terms
      if (!is.null(aov$s.table) && nrow(aov$s.table) > 0) {
        st <- as.data.frame(aov$s.table)
        st$Term <- rownames(st)
        st$Type <- "smooth"
        parts <- c(parts, list(st))
      }

      if (length(parts) == 0) return(NULL)

      # Merge parametric & smooth (different columns -- fill NA)
      all_cols <- unique(unlist(lapply(parts, names)))
      aov_df <- do.call(rbind, lapply(parts, function(p) {
        missing <- setdiff(all_cols, names(p))
        for (m in missing) p[[m]] <- NA
        p[, all_cols, drop = FALSE]
      }))

      # Move Term and Type to front
      front <- c("Term", "Type")
      aov_df <- aov_df[, c(front, setdiff(names(aov_df), front)),
                        drop = FALSE]
      rownames(aov_df) <- NULL

      # Format p-values if present
      pcol <- grep("p-value|Pr\\(", names(aov_df), value = TRUE)
      for (pc in pcol) {
        aov_df[[pc]] <- format.pval(aov_df[[pc]], digits = 3)
      }

      DT::datatable(aov_df, rownames = FALSE,
                    options = list(dom = "t", scrollX = TRUE,
                                  pageLength = 50),
                    class = "compact stripe")
    })

    # --- Mgcv Output tab ---
    output$mgcv_output <- renderPrint({
      res <- result()
      req(res)

      tryCatch({
        cat(sprintf("== Timing: %.2f seconds ==\n\n", res$elapsed))

        cat("== Formula ==\n\n")
        print(res$formula)

        cat("\n\n== Model ==\n\n")
        print(res$model)

        cat("\n\n== Summary ==\n\n")
        print(summary(res$model))

        cat("\n\n== Family ==\n\n")
        print(res$model$family)

        cat("\n\n== Concurvity (overall) ==\n\n")
        if (length(res$model$smooth) > 0L) {
          print(mgcv::concurvity(res$model, full = TRUE))
        } else {
          cat("No smooth terms.\n")
        }
      }, error = function(e) {
        cat("Error rendering output: ", e$message, "\n")
      })
    })

    output$data_preview <- DT::renderDT({
      req(data_r())
      DT::datatable(
        utils::head(data_r(), 100L),
        rownames = FALSE,
        options = list(scrollX = TRUE, pageLength = 10L),
        callback = cell_popup_js
      )
    })

    list(
      result = reactive(result()),
      reset  = function() {
        result(NULL)
        rv$bg_proc <- NULL
        rv$fitting <- FALSE
      }
    )
  })
}
