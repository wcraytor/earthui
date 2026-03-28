function(input, output, session) {

  # --- SQLite settings database ---
  settings_con <- earthUI:::settings_db_connect_()
  session$onSessionEnded(function() {
    earthUI:::settings_db_disconnect_(settings_con)
  })

  # --- Nord theme switching ---
  observe({
    mode <- input$dark_mode
    req(mode)
    tryCatch(
      session$setCurrentTheme(
        if (mode == "dark") nord_dark else nord_light
      ),
      error = function(e) {
        message("Theme switch error (non-fatal): ", conditionMessage(e))
      }
    )
  })

  # --- Event log: appends to <filename>_earthui_log.txt in output folder ---
  # Logs start/end times and elapsed duration for:
  #   5. Fit Earth Model (earth call + tab completions)
  #   6. Download Estimated Sales Prices & Residuals
  #   7. Calculate RCA Adjustments & Download
  #   8. Generate Sales Grid & Download
  #   9. Download Report (per format)
  # One log file per data file, appended incrementally.
  eui_log_ <- local({
    timers <- list()

    get_path <- function() {
      folder <- isolate(input$output_folder)
      fname  <- isolate(rv$file_name)
      if (is.null(folder) || !nzchar(folder)) folder <- path.expand("~/Downloads")
      if (!dir.exists(folder)) dir.create(folder, recursive = TRUE)
      base <- tools::file_path_sans_ext(fname %||% "earthui")
      file.path(folder, paste0(base, "_earthui_log.txt"))
    }

    write_ <- function(line) {
      tryCatch(cat(line, "\n", file = get_path(), append = TRUE),
               error = function(e) NULL)
    }

    list(
      start = function(section) {
        timers[[section]] <<- Sys.time()
        write_(sprintf("[%s] %s — STARTED",
                       format(Sys.time(), "%Y-%m-%d %H:%M:%S"), section))
      },
      end = function(section) {
        t0 <- timers[[section]]
        t1 <- Sys.time()
        elapsed <- if (!is.null(t0)) {
          sprintf("%.1f seconds", as.numeric(difftime(t1, t0, units = "secs")))
        } else "unknown"
        timers[[section]] <<- NULL
        write_(sprintf("[%s] %s — COMPLETED — elapsed %s",
                       format(t1, "%Y-%m-%d %H:%M:%S"), section, elapsed))
      },
      err = function(section, msg) {
        t0 <- timers[[section]]
        t1 <- Sys.time()
        elapsed <- if (!is.null(t0)) {
          sprintf("%.1f seconds", as.numeric(difftime(t1, t0, units = "secs")))
        } else "unknown"
        timers[[section]] <<- NULL
        write_(sprintf("[%s] %s — ERROR after %s — %s",
                       format(t1, "%Y-%m-%d %H:%M:%S"), section, elapsed, msg))
      }
    )
  })

  # --- Write fitting log to output folder ---
  write_fit_log_ <- function(output_folder, lines, file_name) {
    tryCatch({
      folder <- if (is.null(output_folder) || !nzchar(output_folder)) {
        path.expand("~/Downloads")
      } else {
        output_folder
      }
      if (!dir.exists(folder)) dir.create(folder, recursive = TRUE)
      base <- tools::file_path_sans_ext(file_name %||% "earthui")
      log_path <- file.path(folder, paste0(base, "_earth_log_",
                            format(Sys.time(), "%Y%m%d_%H%M%S"), ".txt"))
      writeLines(c(paste("earthUI fitting log:", Sys.time()), "", lines), log_path)
    }, error = function(e) {
      message("earthUI: failed to write log: ", e$message)
    })
  }

  # --- Auto-export earth result for mgcvUI (degree <= 2) ---
  auto_export_for_mgcv_ <- function(result, output_folder, file_name) {
    tryCatch({
      deg <- result$degree %||% 1L
      if (deg > 2L) return(invisible(NULL))
      folder <- if (is.null(output_folder) || !nzchar(output_folder)) {
        path.expand("~/Downloads")
      } else {
        output_folder
      }
      if (!dir.exists(folder)) dir.create(folder, recursive = TRUE)
      base <- tools::file_path_sans_ext(file_name %||% "earth")
      out_path <- file.path(folder, paste0(base, "_earthUI_result_",
                            format(Sys.time(), "%Y%m%d_%H%M%S"), ".rds"))
      saveRDS(result, out_path)
      # Verify the file is readable and contains a valid earthUI_result
      verify <- tryCatch({
        obj <- readRDS(out_path)
        if (!inherits(obj, "earthUI_result")) stop("not an earthUI_result")
        if (is.null(obj$model)) stop("model is NULL")
        stats::predict(obj$model, newdata = obj$data[1L, , drop = FALSE])
        TRUE
      }, error = function(e) {
        message("earthUI: RDS verification FAILED: ", e$message,
                " — deleting corrupt file")
        unlink(out_path)
        FALSE
      })
      if (verify) {
        message("earthUI: auto-exported result for mgcvUI to ", out_path)
      }
    }, error = function(e) {
      message("earthUI: auto-export for mgcvUI failed: ", e$message)
    })
  }

  # Seed history per file (last 5, most recent first)
  rv_seed_history <- reactiveVal(integer(0))

  # --- Reactive values ---
  rv <- reactiveValues(
    data = NULL,
    categoricals = NULL,
    col_types = NULL,
    result = NULL,
    file_name = NULL,
    sheets = NULL,
    file_path = NULL,
    file_ext = NULL,
    fitting = FALSE,
    bg_proc = NULL,
    trace_lines = character(0),
    user_varmod = "lm",       # user's explicit varmod.method choice
    wp_weights = NULL,         # per-target response weights (numeric vector or NULL)
    subset_conditions = list(), # condition rows for subset filter builder
    rca_df = NULL,             # RCA export data for histogram plots
    rca_targets = NULL,        # target variable names for RCA plots
    sg_recommended = NULL,     # recommended comps for sales grid
    sg_others = NULL,          # other comps for sales grid
    current_purpose = "general"   # track current purpose for settings keying
  )

  # Track user's explicit varmod.method changes
  observeEvent(input$varmod_method, {
    # Only record if single target (multi-target forces "none" in the UI)
    if (length(input$target) <= 1L) {
      rv$user_varmod <- input$varmod_method
    }
  })

  # When target count changes, update the varmod dropdown
  observeEvent(input$target, {
    if (length(input$target) > 1L) {
      updateSelectInput(session, "varmod_method", selected = "none")
    } else {
      updateSelectInput(session, "varmod_method", selected = rv$user_varmod)
    }
  })

  # --- Locale ---
  # Load user's locale defaults from SQLite on startup
  locale_defaults <- earthUI:::settings_db_read_(settings_con, "__locale_defaults__")
  if (!is.null(locale_defaults) && length(locale_defaults$settings) > 0L) {
    ld <- locale_defaults$settings
    if (!is.null(ld$locale_country))  updateSelectInput(session, "locale_country",  selected = ld$locale_country)
    if (!is.null(ld$locale_paper))    updateSelectInput(session, "locale_paper",    selected = ld$locale_paper)
    if (!is.null(ld$locale_import))   updateSelectInput(session, "locale_import",   selected = ld$locale_import)
    message("earthUI: restored locale defaults from SQLite")
  }

  # Save locale as user default
  observeEvent(input$locale_save_default, {
    locale_settings <- list(
      locale_country = input$locale_country,
      locale_paper   = input$locale_paper,
      locale_import  = input$locale_import
    )
    earthUI:::settings_db_write_(
      settings_con, "__locale_defaults__",
      settings = jsonlite::toJSON(locale_settings, auto_unbox = TRUE)
    )
    showNotification("Locale saved as default for all new files.",
                     type = "message", duration = 4)
  })

  # When Settings country changes, apply to locale env and sync import locale
  observeEvent(input$locale_country, {
    country <- input$locale_country %||% "us"
    presets <- earthUI:::locale_country_presets_()
    preset <- presets[[country]] %||% presets[["us"]]
    updateSelectInput(session, "locale_paper",  selected = preset$paper)
    updateSelectInput(session, "locale_import", selected = country)
    earthUI:::set_locale_(country)
  })

  # When import locale or paper changes, update locale env
  observe({
    # Import locale determines CSV/decimal/date for file import
    import_country <- input$locale_import %||% input$locale_country %||% "us"
    # Settings country determines big_mark for display formatting
    settings_country <- input$locale_country %||% "us"
    paper <- input$locale_paper %||% "letter"
    presets <- earthUI:::locale_country_presets_()
    import_preset <- presets[[import_country]] %||% presets[["us"]]
    settings_preset <- presets[[settings_country]] %||% presets[["us"]]
    earthUI:::set_locale_(settings_country,
                          csv_sep = import_preset$csv_sep,
                          csv_dec = import_preset$csv_dec,
                          big_mark = settings_preset$big_mark,
                          dec_mark = settings_preset$dec_mark,
                          date_fmt = import_preset$date_fmt,
                          paper = paper)
  })

  # --- Data Import ---
  observeEvent(input$file_input, {
    req(input$file_input)
    message("earthUI: file upload received: ", input$file_input$name)
    message("earthUI: datapath = ", input$file_input$datapath)
    message("earthUI: file exists = ", file.exists(input$file_input$datapath))
    ext <- tolower(tools::file_ext(input$file_input$name))
    rv$file_ext <- ext
    rv$file_path <- input$file_input$datapath
    rv$file_name <- input$file_input$name

    # Restore saved settings from SQLite into localStorage (purpose-aware)
    purpose <- input$purpose %||% "general"
    db_key <- paste0(rv$file_name, "||", purpose)
    saved <- earthUI:::settings_db_read_(settings_con, db_key)
    if (!is.null(saved)) {
      session$sendCustomMessage("restore_all_settings", list(
        filename     = rv$file_name,
        purpose      = purpose,
        settings     = saved$settings,
        variables    = saved$variables,
        interactions = saved$interactions
      ))
      message("earthUI: restored settings from SQLite for: ", rv$file_name,
              " (", purpose, ")")
    }

    if (ext %in% c("xlsx", "xls")) {
      rv$sheets <- readxl::excel_sheets(input$file_input$datapath)
    } else {
      rv$sheets <- NULL
    }
    tryCatch({
      rv$data <- import_data(input$file_input$datapath, sheet = 1,
                               sep = earthUI:::locale_csv_sep_(),
                               dec = earthUI:::locale_csv_dec_())
      rv$categoricals <- detect_categoricals(rv$data)
      rv$col_types <- detect_types(rv$data)
      rv$result <- NULL
      if (!is.null(rv_report$assets_proc) &&
          inherits(rv_report$assets_proc, "r_process") &&
          rv_report$assets_proc$is_alive()) {
        rv_report$assets_proc$kill()
      }
      rv_report$assets_proc <- NULL
      if (!is.null(rv_report$assets_dir)) {
        unlink(rv_report$assets_dir, recursive = TRUE)
        rv_report$assets_dir <- NULL
      }
      rv_seed_history(integer(0))
      message("earthUI: import OK, ", nrow(rv$data), " rows, ", ncol(rv$data), " cols")
    }, error = function(e) {
      message("earthUI: IMPORT ERROR: ", e$message)
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
    rv$data <- import_data(rv$file_path, sheet = input$sheet,
                             sep = earthUI:::locale_csv_sep_(),
                             dec = earthUI:::locale_csv_dec_())
    rv$categoricals <- detect_categoricals(rv$data)
    rv$col_types <- detect_types(rv$data)
    rv$result <- NULL
    rv$computed <- NULL
    if (!is.null(rv_report$assets_dir)) {
      unlink(rv_report$assets_dir, recursive = TRUE)
      rv_report$assets_dir <- NULL
    }
  })

  # Seed history dropdown (shows last 5 seeds used, clicking one fills the input)
  output$seed_history_ui <- renderUI({
    hist <- rv_seed_history()
    if (length(hist) == 0L) return(NULL)
    tags$div(style = "margin-top: -8px; margin-bottom: 8px;",
      tags$label("Recent seeds (last used is first):", style = "font-size: 0.8em; color: var(--bs-secondary-color);"),
      tags$div(style = "display: flex; flex-wrap: wrap; gap: 4px;",
        lapply(hist, function(s) {
          actionLink(
            paste0("seed_recall_", s),
            label = as.character(s),
            style = "font-size: 0.8em; padding: 1px 6px; border: 1px solid #ccc; border-radius: 3px; text-decoration: none;")
        })
      )
    )
  })

  # Click a recent seed to fill the input
  observe({
    hist <- rv_seed_history()
    lapply(hist, function(s) {
      observeEvent(input[[paste0("seed_recall_", s)]], {
        updateTextInput(session, "random_seed", value = as.character(s))
      }, ignoreInit = TRUE)
    })
  })

  # Toggle tab waiting messages via JS (not uiOutput — suspended tabs don't render)
  observe({
    ready <- !is.null(rv$result)
    session$sendCustomMessage("eui_tabs_ready", list(ready = ready))
  })
  observe({
    ready <- !is.null(rv$rca_df)
    session$sendCustomMessage("eui_rca_ready", list(ready = ready))
  })

  # When the purpose radio changes: clear ALL state (data, results, tabs).
  # User re-imports a file; file+purpose settings restore from localStorage.

  observeEvent(input$purpose, {
    rv$current_purpose <- input$purpose

    # Clear imported data
    rv$data      <- NULL
    rv$file_name <- NULL
    rv$file_ext  <- NULL
    rv$file_path <- NULL
    rv$sheets    <- NULL

    # Clear all result / RCA / sales-grid state
    rv$result       <- NULL
    rv$rca_df       <- NULL
    rv$rca_targets  <- NULL
    rv$sg_recommended <- NULL
    rv$sg_others    <- NULL
    rv$trace_lines  <- character(0)
    rv$wp_weights   <- NULL
    rv$subset_conditions <- list()

    # Clear tabs
    session$sendCustomMessage("eui_tabs_ready", list(ready = FALSE))

    # Kill any in-flight report asset process and clean up its temp dir
    if (!is.null(rv_report$assets_proc) &&
        inherits(rv_report$assets_proc, "r_process") &&
        rv_report$assets_proc$is_alive()) {
      rv_report$assets_proc$kill()
    }
    rv_report$assets_proc <- NULL
    if (!is.null(rv_report$assets_dir)) {
      unlink(rv_report$assets_dir, recursive = TRUE)
      rv_report$assets_dir <- NULL
    }

    message("earthUI: purpose switched to '", input$purpose, "' — all data cleared")
  }, ignoreInit = TRUE)

  # Update weights dropdown with numeric column names when data loads
  observe({
    req(rv$data)
    num_cols <- names(rv$data)[vapply(rv$data, is.numeric, logical(1))]
    choices <- c("NULL (none)" = "null", stats::setNames(num_cols, num_cols))
    updateSelectInput(session, "weights_col", choices = choices, selected = "null")
  })

  output$data_loaded <- reactive(!is.null(rv$data))
  outputOptions(output, "data_loaded", suspendWhenHidden = FALSE)

  output$model_fitted <- reactive(!is.null(rv$result))
  outputOptions(output, "model_fitted", suspendWhenHidden = FALSE)



  output$report_heading <- renderUI({
    n <- if (identical(input$purpose, "appraisal")) "9" else "7"
    h4(paste0(n, ". Download Report"))
  })

  output$download_heading <- renderUI({
    label <- if (identical(input$purpose, "general")) {
      "6. Download Estimated Target Variable(s) & Residuals"
    } else {
      "6. Download Estimated Sale Prices & Residuals"
    }
    h4(label)
  })

  # ── Subset Filter Builder Dialog ───────────────────────────────────
  observeEvent(input$subset_builder_btn, {
    req(rv$data)
    rv$subset_conditions <- list(list(col = names(rv$data)[1], op = "==", val = ""))
    show_subset_modal_()
  })

  show_subset_modal_ <- function() {
    conds <- rv$subset_conditions
    df <- rv$data
    col_choices <- names(df)

    cond_ui <- lapply(seq_along(conds), function(i) {
      cond <- conds[[i]]
      col_name <- cond$col
      col_vals <- df[[col_name]]
      col_class <- class(col_vals)[1]

      # Type-aware value input
      val_input <- if (inherits(col_vals, "Date")) {
        dateInput(paste0("subset_val_", i), NULL,
                  value = if (nzchar(cond$val)) as.Date(cond$val) else Sys.Date())
      } else if (inherits(col_vals, "POSIXct") || inherits(col_vals, "POSIXlt")) {
        dateInput(paste0("subset_val_", i), NULL,
                  value = if (nzchar(cond$val)) as.Date(cond$val) else Sys.Date())
      } else if (is.numeric(col_vals)) {
        numericInput(paste0("subset_val_", i), NULL,
                     value = if (nzchar(cond$val)) as.numeric(cond$val) else NA)
      } else {
        # Character/factor: selectInput with unique values
        uvals <- sort(unique(as.character(col_vals[!is.na(col_vals)])))
        selectInput(paste0("subset_val_", i), NULL,
                    choices = uvals,
                    selected = if (nzchar(cond$val) && cond$val %in% uvals) cond$val else uvals[1])
      }

      connector <- NULL
      if (i > 1) {
        connector <- radioButtons(paste0("subset_conn_", i), NULL,
                                  choices = c("AND" = "&", "OR" = "|"),
                                  selected = if (!is.null(cond$conn)) cond$conn else "&",
                                  inline = TRUE)
      }

      tagList(
        if (!is.null(connector)) tags$div(style = "margin: 4px 0;", connector),
        fluidRow(
          column(4, selectInput(paste0("subset_col_", i), NULL,
                                choices = col_choices, selected = col_name)),
          column(2, selectInput(paste0("subset_op_", i), NULL,
                                choices = c("<", ">", "<=", ">=", "==", "!="),
                                selected = cond$op)),
          column(4, val_input),
          column(2, tags$button("X", class = "btn btn-outline-secondary btn-sm",
                                style = "margin-top: 25px;",
                                onclick = sprintf("Shiny.setInputValue('subset_remove_idx', %d, {priority: 'event'});", i)))
        )
      )
    })

    # Build preview expression
    expr_text <- build_subset_expr_()
    preview <- ""
    if (nzchar(expr_text)) {
      n_match <- tryCatch({
        mask <- as.list(df)
        mask[["TRUE"]] <- TRUE; mask[["FALSE"]] <- FALSE
        mask$as.Date <- as.Date; mask$as.POSIXct <- as.POSIXct
        rows <- eval(parse(text = expr_text), envir = mask, enclos = emptyenv())
        if (is.logical(rows)) sum(rows & !is.na(rows)) else "?"
      }, error = function(e) paste("Error:", e$message))
      preview <- sprintf("%s of %d rows match", n_match, nrow(df))
    }

    # JS to detect column dropdown changes
    n_conds <- length(conds)
    col_change_js <- tags$script(HTML(sprintf("
      $(function() {
        for (var i = 1; i <= %d; i++) {
          (function(idx) {
            $('#subset_col_' + idx).off('change.subsetcol').on('change.subsetcol', function() {
              Shiny.setInputValue('subset_col_changed',
                {idx: idx, col: $(this).val(), t: Date.now()},
                {priority: 'event'});
            });
          })(i);
        }
      });
    ", n_conds)))

    showModal(modalDialog(
      title = "Build Subset Filter",
      size = "l",
      tags$div(id = "subset_conditions_container", cond_ui),
      col_change_js,
      actionButton("subset_add_condition", "+ Add condition",
                   class = "btn-outline-primary btn-sm",
                   style = "margin-top: 8px;"),
      hr(),
      tags$div(
        tags$strong("Expression: "),
        tags$code(if (nzchar(expr_text)) expr_text else "(none)")
      ),
      tags$div(
        style = "margin-top: 4px; font-size: 0.9em;",
        tags$strong("Preview: "), preview
      ),
      footer = tagList(
        actionButton("subset_apply", "Apply", class = "btn-primary"),
        modalButton("Cancel")
      )
    ))
  }

  build_subset_expr_ <- function() {
    conds <- rv$subset_conditions
    if (length(conds) == 0) return("")
    df <- rv$data
    parts <- character(0)
    for (i in seq_along(conds)) {
      cond <- conds[[i]]
      col_name <- cond$col
      op <- cond$op
      val <- cond$val
      if (!nzchar(val) || is.na(val)) next

      col_vals <- df[[col_name]]
      # Format value based on column type
      if (inherits(col_vals, "POSIXct") || inherits(col_vals, "POSIXlt")) {
        val_str <- paste0('as.POSIXct("', val, '")')
      } else if (inherits(col_vals, "Date")) {
        val_str <- paste0('as.Date("', val, '")')
      } else if (is.numeric(col_vals)) {
        val_str <- val
      } else {
        val_str <- paste0('"', gsub('"', '\\\\"', val), '"')
      }

      expr_part <- paste0(col_name, " ", op, " ", val_str)
      if (i > 1 && !is.null(cond$conn)) {
        parts <- c(parts, cond$conn, expr_part)
      } else {
        parts <- c(parts, expr_part)
      }
    }
    paste(parts, collapse = " ")
  }

  # Helper: read current condition values from modal inputs into rv
  sync_subset_inputs_ <- function() {
    conds <- rv$subset_conditions
    for (i in seq_along(conds)) {
      conds[[i]]$col <- input[[paste0("subset_col_", i)]] %||% conds[[i]]$col
      conds[[i]]$op <- input[[paste0("subset_op_", i)]] %||% conds[[i]]$op
      raw_val <- input[[paste0("subset_val_", i)]]
      conds[[i]]$val <- if (is.null(raw_val) || identical(raw_val, "")) "" else as.character(raw_val)
      if (i > 1) {
        conds[[i]]$conn <- input[[paste0("subset_conn_", i)]] %||% "&"
      }
    }
    rv$subset_conditions <- conds
  }

  # Add condition
  observeEvent(input$subset_add_condition, {
    sync_subset_inputs_()
    conds <- rv$subset_conditions
    conds[[length(conds) + 1]] <- list(col = names(rv$data)[1], op = "==", val = "", conn = "&")
    rv$subset_conditions <- conds
    removeModal()
    show_subset_modal_()
  })

  # Remove condition buttons — use JS to send which index to remove
  observeEvent(input$subset_remove_idx, {
    idx <- input$subset_remove_idx
    sync_subset_inputs_()
    conds <- rv$subset_conditions
    if (length(conds) > 1 && idx <= length(conds)) {
      conds[[idx]] <- NULL
      rv$subset_conditions <- conds
      removeModal()
      show_subset_modal_()
    }
  }, ignoreInit = TRUE)

  # Column change — use JS to notify which index changed
  observeEvent(input$subset_col_changed, {
    idx <- input$subset_col_changed$idx
    new_col <- input$subset_col_changed$col
    conds <- rv$subset_conditions
    if (idx <= length(conds) && !identical(conds[[idx]]$col, new_col)) {
      sync_subset_inputs_()
      conds <- rv$subset_conditions
      conds[[idx]]$col <- new_col
      conds[[idx]]$val <- ""
      rv$subset_conditions <- conds
      removeModal()
      show_subset_modal_()
    }
  }, ignoreInit = TRUE)

  # Apply subset filter
  observeEvent(input$subset_apply, {
    sync_subset_inputs_()
    expr_text <- build_subset_expr_()
    updateTextInput(session, "subset_arg", value = expr_text)
    removeModal()
  })

  # ── Response Weights (wp) Dialog ───────────────────────────────────

  # Reset wp when target changes to single
  observeEvent(input$target, {
    if (length(input$target) <= 1L) {
      rv$wp_weights <- NULL
      session$sendCustomMessage("update_wp_display",
        list(text = "NULL (equal weights)"))
    }
  })

  # Disable wp button when single target
  observe({
    if (length(input$target) > 1L) {
      shinyjs_run <- function(code) {
        session$sendCustomMessage("wp_btn_state", list(disabled = FALSE))
      }
      session$sendCustomMessage("wp_btn_state", list(disabled = FALSE))
    } else {
      session$sendCustomMessage("wp_btn_state", list(disabled = TRUE))
    }
  })

  observeEvent(input$wp_set_btn, {
    targets <- input$target
    if (length(targets) <= 1L) {
      showNotification("Response weights require multiple target variables.",
                       type = "warning", duration = 4)
      return()
    }

    # Build one numericInput per target
    weight_inputs <- lapply(seq_along(targets), function(i) {
      current_val <- if (!is.null(rv$wp_weights) && i <= length(rv$wp_weights)) {
        rv$wp_weights[i]
      } else {
        1
      }
      numericInput(paste0("wp_val_", i), targets[i],
                   value = current_val, min = 0, step = 0.1)
    })

    showModal(modalDialog(
      title = "Response Weights (wp)",
      tags$p("Set a numeric weight for each target variable.",
             style = "font-size: 0.9em; color: #666;"),
      weight_inputs,
      footer = tagList(
        actionButton("wp_apply", "Apply", class = "btn-primary"),
        modalButton("Cancel")
      )
    ))
  })

  observeEvent(input$wp_apply, {
    targets <- input$target
    weights <- vapply(seq_along(targets), function(i) {
      val <- input[[paste0("wp_val_", i)]]
      if (is.null(val) || is.na(val)) 1 else as.numeric(val)
    }, numeric(1))
    rv$wp_weights <- weights
    display <- paste0(targets, " = ", weights, collapse = ", ")
    session$sendCustomMessage("update_wp_display", list(text = display))
    # Persist to localStorage
    fn <- rv$file_name %||% "default"
    wp_data <- stats::setNames(as.list(weights), targets)
    session$sendCustomMessage("save_wp_weights", list(filename = fn, weights = wp_data))
    removeModal()
  })

  # Restore wp weights from localStorage when data/target changes
  observeEvent(input$wp_weights_restored, {
    restored <- input$wp_weights_restored
    if (!is.null(restored) && length(restored) > 0 && length(input$target) > 1L) {
      targets <- input$target
      weights <- vapply(targets, function(t) {
        val <- restored[[t]]
        if (is.null(val)) 1 else as.numeric(val)
      }, numeric(1))
      rv$wp_weights <- weights
      display <- paste0(targets, " = ", weights, collapse = ", ")
      session$sendCustomMessage("update_wp_display", list(text = display))
    }
  }, ignoreInit = TRUE)

  # --- Persist settings to SQLite (debounced from JS) ---
  observeEvent(input$eui_save_trigger, {
    payload <- input$eui_save_trigger
    req(payload$filename)
    # Encode purpose in the SQLite key: "filename||purpose"
    purpose <- if (!is.null(payload$purpose)) payload$purpose else "general"
    db_key <- paste0(payload$filename, "||", purpose)
    tryCatch({
      earthUI:::settings_db_write_(
        settings_con,
        filename     = db_key,
        settings     = if (!is.null(payload$settings))     payload$settings     else "{}",
        variables    = if (!is.null(payload$variables))    payload$variables    else "{}",
        interactions = if (!is.null(payload$interactions)) payload$interactions else "{}"
      )
    }, error = function(e) {
      message("earthUI: SQLite save error: ", e$message)
    })
  }, ignoreInit = TRUE)

  # --- Default settings (save/restore via SQLite with key "__defaults__") ---

  # Radio: choose mode (last per-file settings vs saved defaults)
  observeEvent(input$eui_defaults_action, {
    action <- input$eui_defaults_action
    message("earthUI: defaults radio changed to: '", action, "'")
    req(rv$file_name)

    if (action == "use_default") {
      defaults <- earthUI:::settings_db_read_(settings_con, "__defaults__")
      if (!is.null(defaults)) {
        session$sendCustomMessage("restore_all_settings", list(
          filename     = rv$file_name,
          settings     = defaults$settings,
          variables    = defaults$variables,
          interactions = defaults$interactions,
          apply        = TRUE
        ))
        showNotification("Default settings applied.", type = "message",
                         duration = 3)
      } else {
        showNotification("No default settings saved yet. Use 'Save current as default' first.",
                         type = "warning", duration = 4)
        updateRadioButtons(session, "eui_defaults_action", selected = "last")
      }

    } else if (action == "earth_defaults") {
      session$sendCustomMessage("apply_earth_defaults", list())
      showNotification("Earth default parameters applied.", type = "message",
                       duration = 3)
      updateRadioButtons(session, "eui_defaults_action", selected = "last")
    }
    # "last" = do nothing, use whatever localStorage has
  }, ignoreInit = TRUE)

  # Button: save current settings as the default
  observeEvent(input$eui_save_defaults, {
    req(rv$file_name)
    message("earthUI: saving current settings as defaults for: ", rv$file_name)
    session$sendCustomMessage("collect_and_save_defaults", list(
      filename = rv$file_name
    ))
    showNotification("Current settings saved as defaults.", type = "message",
                     duration = 3)
  })

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

  preview_data_ <- function() {
    req(rv$data)
    df <- rv$data
    if (input$purpose != "appraisal" && isTRUE(input$skip_subject_row) && nrow(df) >= 2L) {
      df <- df[2:nrow(df), , drop = FALSE]
    }
    df
  }

  output$data_preview <- DT::renderDataTable({
    df <- preview_data_()
    DT::datatable(df,
                  options = list(pageLength = 10, scrollX = TRUE),
                  rownames = FALSE,
                  class = "compact stripe",
                  callback = cell_popup_js)
  })

  output$data_preview_tab <- DT::renderDataTable({
    df <- preview_data_()
    DT::datatable(df,
                  options = list(pageLength = 10, scrollX = TRUE),
                  rownames = FALSE,
                  class = "compact stripe",
                  callback = cell_popup_js)
  })

  # --- Appraisal: subject (row 1) and comps (rows 2+) ---
  render_subjects_ <- function() {
    req(rv$data, input$purpose == "appraisal", nrow(rv$data) >= 1L)
    tgt <- input$target
    preds <- input$predictors
    # Include display_only columns
    display_cols <- character(0)
    specials <- input$col_specials
    if (!is.null(specials)) {
      for (nm in names(specials)) {
        if (specials[[nm]] == "display_only") display_cols <- c(display_cols, nm)
      }
    }
    show_cols <- unique(c(display_cols, tgt, preds))
    show_cols <- intersect(show_cols, names(rv$data))
    # Show all columns if no target/predictors selected yet
    if (length(show_cols) == 0L) show_cols <- names(rv$data)
    subj <- rv$data[1L, show_cols, drop = FALSE]
    if (!is.null(tgt) && length(tgt) > 0L) {
      for (t in tgt) {
        if (t %in% names(subj)) subj[[t]] <- NA
      }
    }
    subj <- cbind(data.frame(row = 1L, check.names = FALSE), subj)
    DT::datatable(subj,
                  options = list(pageLength = 1, scrollX = TRUE, dom = "t"),
                  rownames = FALSE, class = "compact stripe",
                  callback = cell_popup_js)
  }

  render_comps_ <- function() {
    req(rv$data, input$purpose == "appraisal", nrow(rv$data) >= 2L)
    tgt <- input$target
    preds <- input$predictors
    display_cols <- character(0)
    specials <- input$col_specials
    if (!is.null(specials)) {
      for (nm in names(specials)) {
        if (specials[[nm]] == "display_only") display_cols <- c(display_cols, nm)
      }
    }
    show_cols <- unique(c(display_cols, tgt, preds))
    show_cols <- intersect(show_cols, names(rv$data))
    # Show all columns if no target/predictors selected yet
    if (length(show_cols) == 0L) show_cols <- names(rv$data)
    comps <- rv$data[2:nrow(rv$data), show_cols, drop = FALSE]
    comps <- cbind(data.frame(row = seq_len(nrow(comps)), check.names = FALSE), comps)
    low_rows <- integer(0)
    if (!is.null(tgt) && length(tgt) > 0L && tgt[1L] %in% names(comps)) {
      col <- comps[[tgt[1L]]]
      if (is.numeric(col)) {
        low_rows <- which(col <= 100 | is.na(col))
      }
    }
    dt <- DT::datatable(comps,
                        options = list(pageLength = 10, scrollX = TRUE),
                        rownames = FALSE, class = "compact stripe",
                        callback = cell_popup_js)
    if (length(low_rows) > 0L) {
      dt <- DT::formatStyle(dt, columns = 0, target = "row",
              backgroundColor = DT::styleRow(low_rows, "rgba(255,0,0,0.15)"))
    }
    dt
  }

  output$data_subjects     <- DT::renderDataTable(render_subjects_())
  output$data_comps        <- DT::renderDataTable(render_comps_())
  output$data_subjects_tab <- DT::renderDataTable(render_subjects_())
  output$data_comps_tab    <- DT::renderDataTable(render_comps_())

  # --- Variable Configuration ---
  output$target_selector <- renderUI({
    req(rv$data)
    # Depend on purpose so this re-renders and restores per-purpose settings
    purpose <- input$purpose
    storage_key <- if (is.null(rv$file_name)) "default" else rv$file_name

    # JavaScript: persist target variable + advanced parameters in localStorage
    js <- tags$script(HTML(sprintf("
      (function() {
        var storageKeyRaw = %s;
        function getSettingsKey() {
          return window.euiPurposeKey('earthUI_settings_', storageKeyRaw);
        }
        var selectIds = ['target', 'weights_col',
                         'degree', 'pmethod', 'glm_family', 'trace',
                         'varmod_method'];
        var numericIds = ['nprune', 'thresh', 'penalty', 'minspan', 'endspan',
                          'fast_k', 'nfold_override', 'nk', 'newvar_penalty',
                          'fast_beta', 'ncross', 'varmod_exponent', 'varmod_conv',
                          'varmod_clamp', 'varmod_minspan', 'adjust_endspan',
                          'exhaustive_tol', 'output_folder', 'subset_arg'];
        var checkboxIds = ['stratify', 'keepxy', 'scale_y', 'auto_linpreds',
                           'use_beta_cache', 'force_xtx_prune', 'get_leverages',
                           'force_weights', 'skip_subject_row'];
        var radioIds = [];
        var dateIds = ['effective_date'];
        var allIds = selectIds.concat(numericIds).concat(checkboxIds).concat(radioIds).concat(dateIds);

        var saved = null;
        try { saved = JSON.parse(localStorage.getItem(getSettingsKey())); } catch(e) {}

        function restoreSettings() {
          if (!saved) return;
          selectIds.forEach(function(id) {
            if (saved[id] !== undefined && saved[id] !== null) {
              var el = document.getElementById(id);
              if (el && el.selectize) {
                if (id === 'target') {
                  // target can be a single value or an array (multi-select)
                  var vals = Array.isArray(saved[id]) ? saved[id] : [saved[id]];
                  var valid = vals.filter(function(v) { return el.selectize.options[v]; });
                  if (valid.length > 0) el.selectize.setValue(valid);
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
          checkboxIds.forEach(function(id) {
            if (saved[id] !== undefined) {
              var $el = $('#' + id);
              if ($el.length) {
                $el.prop('checked', saved[id]);
                $el.trigger('change');
              }
            }
          });
          radioIds.forEach(function(id) {
            if (saved[id] !== undefined) {
              $('input[name=' + id + '][value=' + saved[id] + ']').prop('checked', true).trigger('change');
            }
          });
          dateIds.forEach(function(id) {
            if (saved[id] !== undefined && saved[id] !== null) {
              var $inp = $('#' + id + ' input');
              if ($inp.length) {
                $inp.val(saved[id]).trigger('change');
              } else {
                $('#' + id).val(saved[id]).trigger('change');
              }
            }
          });
        }

        function saveSettings() {
          if (window.euiPurposeSwitching) return;
          var state = {};
          selectIds.forEach(function(id) {
            var el = document.getElementById(id);
            if (el && el.selectize) { state[id] = el.selectize.getValue(); }
          });
          numericIds.forEach(function(id) {
            state[id] = $('#' + id).val();
          });
          checkboxIds.forEach(function(id) {
            state[id] = $('#' + id).is(':checked');
          });
          radioIds.forEach(function(id) {
            state[id] = $('input[name=' + id + ']:checked').val();
          });
          dateIds.forEach(function(id) {
            var $inp = $('#' + id + ' input');
            state[id] = $inp.length ? $inp.val() : $('#' + id).val();
          });
          try { localStorage.setItem(getSettingsKey(), JSON.stringify(state)); } catch(e) {}
        }

        // Restore after selectize initializes (retry until target AND degree are ready)
        // Block saving until restore is complete to prevent defaults overwriting saved values
        var restoreComplete = false;
        var attempts = 0;
        function tryRestore() {
          var targetEl = document.getElementById('target');
          var degreeEl = document.getElementById('degree');
          var targetReady = targetEl && targetEl.selectize && targetEl.selectize.isSetup;
          var degreeReady = degreeEl && degreeEl.selectize && degreeEl.selectize.isSetup;
          if (targetReady && degreeReady) {
            restoreSettings();
            // Restore wp weights from localStorage
            window.euiCurrentFilename = storageKeyRaw;
            setTimeout(function() {
              try {
                var wpSaved = JSON.parse(localStorage.getItem(window.euiPurposeKey('earthUI_wp_', storageKeyRaw)));
                if (wpSaved && Object.keys(wpSaved).length > 0) {
                  Shiny.setInputValue('wp_weights_restored', wpSaved, {priority: 'event'});
                }
              } catch(e) {}
            }, 600);
            restoreComplete = true;
          } else if (attempts < 40) {
            attempts++;
            setTimeout(tryRestore, 250);
          } else {
            restoreComplete = true;  // give up waiting, allow saves
          }
        }
        tryRestore();

        // Save on any tracked input change (only after restore is done)
        $(document).off('shiny:inputchanged.euisettings')
                   .on('shiny:inputchanged.euisettings', function(event) {
          if (restoreComplete && allIds.indexOf(event.name) >= 0) {
            saveSettings();
            if (typeof window.euiSaveToServer === 'function') window.euiSaveToServer(%s);
          }
        });
      })();
    ", jsonlite::toJSON(storage_key, auto_unbox = TRUE),
       jsonlite::toJSON(storage_key, auto_unbox = TRUE))))

    tagList(
      selectInput("target", "Target (response) variable(s)",
                  choices = names(rv$data), multiple = TRUE),
      js
    )
  })

  output$predictor_hint_text <- renderUI({
    hint <- "Type = column data type, Inc = include as predictor, Factor = treat as categorical, Linear = linear-only (no hinges)"
    if (input$purpose %in% c("appraisal", "market")) {
      hint <- paste0(hint, ", Special = column role (e.g. contract_date)")
    }
    tags$p(class = "text-muted", style = "font-size: 0.8em; margin-bottom: 5px;", hint)
  })

  output$variable_table <- renderUI({
    req(rv$data)
    if (is.null(input$target) || length(input$target) == 0L) {
      return(tags$p(class = "text-muted", style = "padding: 12px; text-align: center;",
                    "Select a target variable above to configure predictors."))
    }
    candidates <- setdiff(names(rv$data), input$target)
    nrows <- nrow(rv$data)

    # Storage key for remembering settings
    storage_key <- if (is.null(rv$file_name)) "default" else rv$file_name

    # Type options for dropdown
    type_options <- c("numeric", "integer", "character", "logical",
                      "factor", "Date", "POSIXct", "unknown")

    appraiser <- input$purpose %in% c("appraisal", "market")

    # Special column options
    special_options <- c("no", "actual_age", "area", "concessions",
                         "contract_date", "display_only", "dom",
                         "effective_age", "latitude", "listing_date",
                         "living_area", "longitude", "lot_size",
                         "sale_age", "site_dimensions", "weight")

    # Header row — vertical labels for checkboxes, like glmnetUI
    angled_hdr <- "text-align:center; font-size:0.85em; writing-mode:vertical-lr; transform:rotate(180deg); height:55px; line-height:1; font-weight:bold;"
    header_cols <- list(
      tags$div(style = "flex: 1; min-width: 60px; font-weight: bold; font-size: 0.85em;", "Variable"),
      tags$div(style = "width: 75px; text-align: center; font-weight: bold; font-size: 0.85em;", "Type"),
      tags$div(style = paste0("width: 20px;", angled_hdr), "Include"),
      tags$div(style = paste0("width: 20px;", angled_hdr), "Factor"),
      tags$div(style = paste0("width: 20px;", angled_hdr), "Linear")
    )
    if (appraiser) {
      header_cols <- c(header_cols, list(
        tags$div(style = "width: 80px; text-align: center; font-weight: bold; font-size: 0.85em;", "Special")
      ))
    }
    header_cols <- c(header_cols, list(
      tags$div(style = "width: 32px; text-align: right; padding-right: 4px; font-weight: bold; font-size: 0.85em;", "NAs")
    ))
    header <- tags$div(
      style = "display: flex; align-items: flex-end; padding: 4px 0; border-bottom: 2px solid var(--bs-border-color, #ccc); position: sticky; top: 0; z-index: 1; background: var(--bs-tertiary-bg, var(--bs-body-bg, #f0f0f0)); gap: 2px;",
      header_cols
    )

    # Build rows using numeric index for IDs
    rows <- lapply(seq_along(candidates), function(i) {
      col <- candidates[i]
      n_na <- sum(is.na(rv$data[[col]]))
      pct_na <- n_na / nrows
      na_style <- if (pct_na > 0.3) "color: red;" else ""

      # Auto-detected type for this column
      detected_type <- if (!is.null(rv$col_types) && col %in% names(rv$col_types)) {
        rv$col_types[[col]]
      } else {
        "unknown"
      }

      # Build <option> tags with auto-detected type selected
      option_tags <- lapply(type_options, function(opt) {
        if (opt == detected_type) {
          tags$option(value = opt, selected = "selected", opt)
        } else {
          tags$option(value = opt, opt)
        }
      })

      # Build row cells — checkboxes grouped together (Include, Factor, Linear)
      row_cells <- list(
        tags$div(style = "flex: 1; min-width: 60px; font-size: 0.82em; overflow: hidden; text-overflow: ellipsis; white-space: nowrap;",
                 title = col, col,
                 tags$span(id = paste0("eui_special_badge_", i),
                           style = "font-size: 0.7em; color: #0d6efd; font-style: italic; margin-left: 4px;")),
        tags$div(style = "width: 75px; text-align: center;",
                 tags$select(id = paste0("eui_type_", i),
                             class = "eui-type-select",
                             style = "width: 70px; font-size: 0.75em; padding: 1px 2px; border: 1px solid var(--bs-border-color, #ccc); border-radius: 3px; background: var(--bs-body-bg, #fff); color: var(--bs-body-color, #333);",
                             option_tags)),
        tags$div(style = "width: 20px; text-align: center;",
                 tags$input(type = "checkbox", id = paste0("eui_inc_", i),
                            class = "eui-var-cb")),
        tags$div(style = "width: 20px; text-align: center;",
                 tags$input(type = "checkbox", id = paste0("eui_fac_", i),
                            class = "eui-var-cb")),
        tags$div(style = "width: 20px; text-align: center;",
                 tags$input(type = "checkbox", id = paste0("eui_lin_", i),
                            class = "eui-var-cb"))
      )
      if (appraiser) {
        special_option_tags <- lapply(special_options, function(opt) {
          tags$option(value = opt, opt)
        })
        row_cells <- c(row_cells, list(
          tags$div(style = "width: 80px; text-align: center;",
                   tags$select(id = paste0("eui_special_", i),
                               class = "eui-special-select",
                               style = "width: 74px; font-size: 0.75em; padding: 1px 2px; border: 1px solid var(--bs-border-color, #ccc); border-radius: 3px; background: var(--bs-body-bg, #fff); color: var(--bs-body-color, #333);",
                               special_option_tags))
        ))
      }
      row_cells <- c(row_cells, list(
        tags$div(style = paste0("width: 32px; text-align: right; font-size: 0.8em; padding-right: 4px;", na_style),
                 if (n_na > 0L) as.character(n_na) else "")
      ))

      tags$div(
        style = "display: flex; align-items: center; padding: 2px 0; border-bottom: 1px solid var(--bs-border-color, #eee); gap: 2px;",
        row_cells
      )
    })

    # Detected types as JSON for JS (used to reset to defaults)
    detected_types_list <- vapply(candidates, function(col) {
      if (!is.null(rv$col_types) && col %in% names(rv$col_types)) {
        rv$col_types[[col]]
      } else {
        "unknown"
      }
    }, character(1L))
    detected_types_json <- jsonlite::toJSON(
      as.list(stats::setNames(detected_types_list, candidates)),
      auto_unbox = TRUE
    )

    # JavaScript: sync checkboxes + type dropdowns <-> Shiny inputs, with localStorage persistence
    col_json <- jsonlite::toJSON(candidates, auto_unbox = FALSE)
    n_cols <- length(candidates)
    storage_key_json <- jsonlite::toJSON(storage_key, auto_unbox = TRUE)
    appraiser_json <- if (appraiser) "true" else "false"
    js <- tags$script(HTML(sprintf("
      (function() {
        var cols = %s;
        var n = %d;
        var storageKeyRaw = %s;
        function getVarsKey() {
          return window.euiPurposeKey('earthUI_vars_', storageKeyRaw);
        }
        var detectedTypes = %s;
        var appraiser = %s;

        function gatherState() {
          var inc = [], fac = [], lin = [];
          var types = {};
          var specials = {};
          for (var i = 1; i <= n; i++) {
            var sp = appraiser ? ($('#eui_special_' + i).val() || 'no') : 'no';
            if (appraiser) specials[cols[i-1]] = sp;
            if ($('#eui_inc_' + i).is(':checked') && sp !== 'display_only') inc.push(cols[i-1]);
            if ($('#eui_fac_' + i).is(':checked')) fac.push(cols[i-1]);
            if ($('#eui_lin_' + i).is(':checked')) lin.push(cols[i-1]);
            types[cols[i-1]] = $('#eui_type_' + i).val();
          }
          Shiny.setInputValue('predictors', inc.length > 0 ? inc : null);
          Shiny.setInputValue('categoricals', fac.length > 0 ? fac : null);
          Shiny.setInputValue('linpreds', lin.length > 0 ? lin : null);
          Shiny.setInputValue('col_types', types);
          if (appraiser) {
            Shiny.setInputValue('col_specials', specials);
          }
        }

        function saveState() {
          var state = {};
          for (var i = 1; i <= n; i++) {
            var entry = {
              inc: $('#eui_inc_' + i).is(':checked'),
              fac: $('#eui_fac_' + i).is(':checked'),
              lin: $('#eui_lin_' + i).is(':checked'),
              type: $('#eui_type_' + i).val()
            };
            if (appraiser) {
              var sp = $('#eui_special_' + i).val();
              if (sp) entry.special = sp;
            }
            state[cols[i-1]] = entry;
          }
          if (window.euiPurposeSwitching) return;
          try { localStorage.setItem(getVarsKey(), JSON.stringify(state)); } catch(e) {}
        }

        function restoreState() {
          var saved = null;
          try { saved = JSON.parse(localStorage.getItem(getVarsKey())); } catch(e) {}
          if (saved) {
            for (var i = 1; i <= n; i++) {
              var s = saved[cols[i-1]];
              if (s) {
                $('#eui_inc_' + i).prop('checked', s.inc);
                $('#eui_fac_' + i).prop('checked', s.fac);
                $('#eui_lin_' + i).prop('checked', s.lin);
                if (s.type) {
                  $('#eui_type_' + i).val(s.type);
                }
                if (appraiser && s.special) {
                  $('#eui_special_' + i).val(s.special);
                }
              }
            }
          }
        }

        // Restore saved state, then sync to Shiny
        restoreState();
        updateBadges();
        setTimeout(gatherState, 200);

        // On any checkbox change, save and sync
        $(document).off('change.euivar').on('change.euivar', '.eui-var-cb', function() {
          saveState();
          gatherState();
          if (typeof window.euiSaveToServer === 'function') window.euiSaveToServer(storageKeyRaw);
        });

        // On type dropdown change: auto-link Factor, save and sync
        $(document).off('change.euitype').on('change.euitype', '.eui-type-select', function() {
          var idx = this.id.replace('eui_type_', '');
          var val = $(this).val();
          if (val === 'character' || val === 'factor') {
            $('#eui_fac_' + idx).prop('checked', true);
          }
          saveState();
          gatherState();
          if (typeof window.euiSaveToServer === 'function') window.euiSaveToServer(storageKeyRaw);
        });

        // Update special type badges next to variable names
        function updateBadges() {
          if (!appraiser) return;
          for (var j = 1; j <= n; j++) {
            var sp = $('#eui_special_' + j).val() || 'no';
            var $badge = $('#eui_special_badge_' + j);
            if ($badge.length) {
              $badge.text(sp !== 'no' ? '[' + sp + ']' : '');
            }
          }
        }
        window.euiUpdateBadges = updateBadges;

        // On special dropdown change: enforce single per special type, save and sync
        $(document).off('change.euispecial').on('change.euispecial', '.eui-special-select', function() {
          var idx = parseInt(this.id.replace('eui_special_', ''));
          var val = $(this).val();
          if (val !== 'no' && val !== 'display_only') {
            // Only one column per special type (except display_only allows multiple)
            for (var j = 1; j <= n; j++) {
              if (j !== idx && $('#eui_special_' + j).val() === val) {
                $('#eui_special_' + j).val('no');
              }
            }
          }
          updateBadges();
          saveState();
          gatherState();
          if (typeof window.euiSaveToServer === 'function') window.euiSaveToServer(storageKeyRaw);
        });

        // Expose detectedTypes for earth defaults reset
        window.euiDetectedTypes = detectedTypes;
        window.euiCols = cols;
      })();
    ", col_json, n_cols, storage_key_json, detected_types_json, appraiser_json)))

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
        tags$th(class = "eui-matrix-varlabel",
                `data-var-idx` = j,
                style = "padding: 2px 4px; font-size: 0.75em; text-align: center; writing-mode: vertical-lr; transform: rotate(180deg); max-height: 100px; overflow: hidden; cursor: pointer;",
                title = preds[j], preds[j])
      ))
    }
    header_row <- tags$tr(class = "eui-matrix-header", header_cells)

    # Build matrix rows
    body_rows <- list()
    for (i in seq_len(n)) {
      cells <- list(
        tags$td(class = "eui-matrix-rowlabel eui-matrix-varlabel",
                `data-var-idx` = i,
                style = "padding: 2px 4px; font-size: 0.75em; white-space: nowrap; overflow: hidden; text-overflow: ellipsis; max-width: 100px; cursor: pointer;",
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
      body_rows <- c(body_rows, list(tags$tr(cells)))
    }

    # JavaScript to sync checkboxes with Shiny inputs + localStorage persistence
    storage_key <- if (is.null(rv$file_name)) "default" else rv$file_name
    js <- tags$script(HTML(sprintf("
      (function() {
        var n = %d;
        var interStorageKeyRaw = %s;
        function getInterKey() {
          return window.euiPurposeKey('earthUI_interactions_', interStorageKeyRaw);
        }

        function saveState() {
          if (window.euiPurposeSwitching) return;
          var state = {};
          for (var i = 1; i < n; i++) {
            for (var j = i + 1; j <= n; j++) {
              state[i + '_' + j] = $('#allowed_' + i + '_' + j).is(':checked');
            }
          }
          try { localStorage.setItem(getInterKey(), JSON.stringify(state)); } catch(e) {}
        }

        function restoreState() {
          var saved = null;
          try { saved = JSON.parse(localStorage.getItem(getInterKey())); } catch(e) {}
          if (!saved) return;
          for (var i = 1; i < n; i++) {
            for (var j = i + 1; j <= n; j++) {
              var key = i + '_' + j;
              if (saved[key] !== undefined) {
                $('#allowed_' + i + '_' + j).prop('checked', saved[key]);
              }
            }
          }
        }

        function syncToShiny() {
          for (var i = 1; i < n; i++) {
            for (var j = i + 1; j <= n; j++) {
              var id = 'allowed_' + i + '_' + j;
              Shiny.setInputValue(id, $('#' + id).is(':checked'));
            }
          }
        }

        // Restore saved state, then sync
        restoreState();
        setTimeout(function() {
          syncToShiny();
          // Update Allow All / Clear All checkboxes
          var all = $('.eui-interaction-cb').length;
          var checked = $('.eui-interaction-cb:checked').length;
          $('#eui_allow_all').prop('checked', checked === all);
          $('#eui_clear_all').prop('checked', checked === 0);
        }, 200);

        // On any interaction checkbox change, save and sync
        $(document).off('change.euimatrix').on('change.euimatrix', '.eui-interaction-cb', function() {
          saveState();
          syncToShiny();
          if (typeof window.euiSaveToServer === 'function') window.euiSaveToServer(%s);
        });

        // Click variable name (row or column header) to toggle all its interactions
        $(document).off('click.euivarlabel').on('click.euivarlabel', '.eui-matrix-varlabel', function() {
          var k = parseInt($(this).attr('data-var-idx'));
          if (isNaN(k)) return;
          // Collect all checkboxes involving variable k
          var cbs = [];
          for (var i = 1; i <= n; i++) {
            if (i === k) continue;
            var lo = Math.min(i, k), hi = Math.max(i, k);
            var $cb = $('#allowed_' + lo + '_' + hi);
            if ($cb.length) cbs.push($cb);
          }
          // Toggle: if all checked, uncheck all; otherwise check all
          var allChecked = cbs.every(function($cb) { return $cb.is(':checked'); });
          cbs.forEach(function($cb) { $cb.prop('checked', !allChecked); });
          // Trigger change to save and sync
          if (cbs.length > 0) cbs[0].trigger('change');
        });

        // --- Block Degree 1: right-click variable name to toggle ---
        var preds = %s;
        var blk1KeyRaw = %s;
        function getBlk1Key() {
          return window.euiPurposeKey('earthUI_blk1_', blk1KeyRaw);
        }
        var blk1 = {};
        // Restore saved block-degree-1 state
        try { var saved = JSON.parse(localStorage.getItem(getBlk1Key())); if (saved) blk1 = saved; } catch(e) {}

        function updateBlk1Labels() {
          $('.eui-matrix-varlabel').each(function() {
            var idx = parseInt($(this).attr('data-var-idx'));
            if (isNaN(idx)) return;
            var varName = preds[idx - 1];
            var indicator = $(this).find('.blk1-indicator');
            if (blk1[varName]) {
              if (indicator.length === 0) {
                $(this).append('<span class=\"blk1-indicator\" style=\"color:var(--bs-body-color, #000); font-weight:bold; font-size:0.85em;\"> 1</span>');
              }
            } else {
              indicator.remove();
            }
          });
        }

        function syncBlk1() {
          var blocked = [];
          for (var v in blk1) { if (blk1[v]) blocked.push(v); }
          Shiny.setInputValue('block_degree1', blocked.length > 0 ? blocked : null);
          try { localStorage.setItem(getBlk1Key(), JSON.stringify(blk1)); } catch(e) {}
        }

        updateBlk1Labels();
        setTimeout(syncBlk1, 250);

        $(document).off('contextmenu.euiblk1').on('contextmenu.euiblk1', '.eui-matrix-varlabel', function(e) {
          e.preventDefault();
          var idx = parseInt($(this).attr('data-var-idx'));
          if (isNaN(idx)) return;
          var varName = preds[idx - 1];
          blk1[varName] = !blk1[varName];
          updateBlk1Labels();
          syncBlk1();
          if (typeof window.euiSaveToServer === 'function') window.euiSaveToServer(%s);
        });
      })();
    ", n, jsonlite::toJSON(storage_key, auto_unbox = TRUE),
       jsonlite::toJSON(storage_key, auto_unbox = TRUE),
       jsonlite::toJSON(preds, auto_unbox = FALSE),
       jsonlite::toJSON(storage_key, auto_unbox = TRUE),
       jsonlite::toJSON(storage_key, auto_unbox = TRUE))))

    div(
      style = "max-height: 300px; overflow: auto; border: 1px solid #ddd; padding: 4px; border-radius: 4px;",
      tags$table(style = "border-collapse: collapse;",
                 tags$thead(header_row),
                 tags$tbody(body_rows)),
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

  # --- Recompute sale_age when Effective Date changes ---
  observeEvent(input$effective_date, {
    req(rv$data, input$purpose %in% c("appraisal", "market"))

    # Find contract_date column from specials
    specials <- input$col_specials
    contract_col <- NULL
    sale_age_col <- NULL
    if (!is.null(specials)) {
      for (nm in names(specials)) {
        if (specials[[nm]] == "contract_date") contract_col <- nm
        if (specials[[nm]] == "sale_age") sale_age_col <- nm
      }
    }

    # If sale_age is a user-designated column, don't overwrite it
    if (!is.null(sale_age_col)) return()

    # Only recompute if we have a contract_date column and sale_age already exists
    # (i.e., it was previously computed)
    if (is.null(contract_col)) return()
    if (!("sale_age" %in% names(rv$data))) return()

    eff_date <- as.POSIXct(as.character(input$effective_date))

    contract_vals <- rv$data[[contract_col]]
    if (inherits(contract_vals, "POSIXct")) {
      contract_posix <- contract_vals
    } else if (inherits(contract_vals, "Date")) {
      contract_posix <- as.POSIXct(contract_vals)
    } else if (is.character(contract_vals)) {
      contract_posix <- suppressWarnings(as.POSIXct(contract_vals))
      if (all(is.na(contract_posix[!is.na(contract_vals)]))) return()
    } else if (is.numeric(contract_vals)) {
      contract_posix <- as.POSIXct(as.Date(contract_vals, origin = "1899-12-30"))
    } else {
      return()
    }

    sale_age <- as.integer(difftime(eff_date, contract_posix, units = "days"))
    rv$data[["sale_age"]] <- sale_age
    showNotification(
      paste0("Recomputed sale_age for new effective date (", input$effective_date, ")."),
      type = "message", duration = 5
    )
  }, ignoreInit = TRUE)

  # --- Fit Model ---
  build_fit_args_ <- function(df = rv$data) {
    na_to_null <- function(x) if (is.na(x) || is.null(x)) NULL else x

    # Evaluate subset expression (if any) to filter rows
    subset_expr <- trimws(input$subset_arg %||% "")
    if (nzchar(subset_expr)) {
      subset_result <- tryCatch({
        expr <- parse(text = subset_expr)
        # Only allow column names from the data as variables
        mask <- as.list(df)
        mask[["TRUE"]] <- TRUE; mask[["FALSE"]] <- FALSE
        mask$as.Date <- as.Date; mask$as.POSIXct <- as.POSIXct
        rows <- eval(expr, envir = mask, enclos = emptyenv())
        if (!is.logical(rows)) stop("Expression must evaluate to TRUE/FALSE")
        rows[is.na(rows)] <- FALSE
        rows
      }, error = function(e) {
        showNotification(paste("Subset error:", e$message),
                         type = "error", duration = 10)
        NULL
      })
      if (!is.null(subset_result)) {
        n_before <- nrow(df)
        df <- df[subset_result, , drop = FALSE]
        message("earthUI: subset filter: ", sum(subset_result), " of ", n_before, " rows selected")
      }
    }

    degree <- as.integer(input$degree)

    allowed_func <- NULL
    allowed_matrix_arg <- NULL
    blk1 <- input$block_degree1
    if (degree >= 2 || length(blk1) > 0L) {
      allowed_matrix_arg <- get_allowed_matrix()
      allowed_func <- build_allowed_function(allowed_matrix_arg,
                                              block_degree1 = blk1)
    }

    glm_arg <- NULL
    if (input$glm_family != "none") {
      glm_arg <- list(family = input$glm_family)
    }

    # Weights column → numeric vector (or NULL)
    # Check special type "weight" first, then fall back to weights_col dropdown
    weights_arg <- NULL
    weight_col_name <- NULL
    if (!is.null(input$col_specials)) {
      for (nm in names(input$col_specials)) {
        if (input$col_specials[[nm]] == "weight") { weight_col_name <- nm; break }
      }
    }
    if (!is.null(weight_col_name) && weight_col_name %in% names(df)) {
      weights_arg <- df[[weight_col_name]]
    } else if (!is.null(input$weights_col) && input$weights_col != "null" &&
               input$weights_col %in% names(df)) {
      weights_arg <- df[[input$weights_col]]
    }

    # wp (response weights) — per-target numeric vector from dialog
    # Only use wp for multi-target models (earth rejects wp with Scale.y)
    wp_arg <- if (length(input$target) > 1L) rv$wp_weights else NULL

    # Collect type_map from JS dropdown state
    type_map_arg <- input$col_types  # named list from gatherState()

    list(
      df              = df,
      target          = input$target,
      predictors      = input$predictors,
      categoricals    = input$categoricals,
      linpreds        = input$linpreds,
      type_map        = type_map_arg,
      degree          = degree,
      allowed_func    = allowed_func,
      allowed_matrix  = allowed_matrix_arg,
      nfold           = na_to_null(input$nfold_override),
      nprune          = na_to_null(input$nprune),
      thresh          = na_to_null(input$thresh),
      penalty         = na_to_null(input$penalty),
      minspan         = na_to_null(input$minspan),
      endspan         = na_to_null(input$endspan),
      fast.k          = na_to_null(input$fast_k),
      pmethod         = input$pmethod,
      glm             = glm_arg,
      trace           = as.numeric(input$trace),
      nk              = na_to_null(input$nk),
      newvar.penalty  = na_to_null(input$newvar_penalty),
      fast.beta       = na_to_null(input$fast_beta),
      ncross          = na_to_null(input$ncross),
      stratify        = input$stratify,
      varmod.method   = if (length(input$target) > 1L) "none" else input$varmod_method,
      varmod.exponent = na_to_null(input$varmod_exponent),
      varmod.conv     = na_to_null(input$varmod_conv),
      varmod.clamp    = na_to_null(input$varmod_clamp),
      varmod.minspan  = na_to_null(input$varmod_minspan),
      keepxy          = input$keepxy,
      Scale.y         = input$scale_y,
      Adjust.endspan  = na_to_null(input$adjust_endspan),
      Auto.linpreds   = input$auto_linpreds,
      Force.weights   = input$force_weights,
      Use.beta.cache  = input$use_beta_cache,
      Force.xtx.prune = input$force_xtx_prune,
      Get.leverages   = input$get_leverages,
      Exhaustive.tol  = na_to_null(input$exhaustive_tol),
      wp              = wp_arg,
      weights         = weights_arg
    )
  }


  # Report assets are generated on-demand when the user clicks Download Report.
  # No eager pre-generation — saveRDS of large result objects is too slow
  # to run in the same reactive cycle as tab rendering.

  # Polling observer for background asset generation
  observe({
    req(rv_report$assets_proc)
    invalidateLater(1000)
    proc <- rv_report$assets_proc
    if (!inherits(proc, "r_process")) return()
    if (!proc$is_alive()) {
      tryCatch({
        assets_path <- proc$get_result()
        rv_report$assets_dir <- assets_path
        message("earthUI: report assets ready at ", assets_path)
      }, error = function(e) {
        message("earthUI: asset prep error: ", e$message)
        rv_report$assets_dir <- NULL
      })
      rv_report$assets_proc <- NULL
    }
  })

  observeEvent(input$run_model, {
    message("earthUI: Fit clicked. data=", !is.null(rv$data),
            " target=", paste(input$target, collapse=","),
            " predictors=", paste(input$predictors, collapse=","),
            " purpose=", input$purpose)
    req(rv$data, input$target, input$predictors)

    # --- Appraiser: round latitude/longitude to 3 decimal places ---
    if (input$purpose %in% c("appraisal", "market") && !is.null(input$col_specials)) {
      for (nm in names(input$col_specials)) {
        if (input$col_specials[[nm]] %in% c("latitude", "longitude") &&
            nm %in% names(rv$data) && is.numeric(rv$data[[nm]])) {
          rv$data[[nm]] <- round(rv$data[[nm]], 3L)
        }
      }
    }

    # --- Appraiser: compute or identify sale_age ---
    if (input$purpose %in% c("appraisal", "market")) {
      # Check if a column is already designated as sale_age
      specials <- input$col_specials
      sale_age_col <- NULL
      contract_col <- NULL
      if (!is.null(specials)) {
        for (nm in names(specials)) {
          if (specials[[nm]] == "sale_age") sale_age_col <- nm
          if (specials[[nm]] == "contract_date") contract_col <- nm
        }
      }

      # If no sale_age designated and no pre-existing numeric sale_age column,
      # compute from contract_date
      if (is.null(sale_age_col) &&
          !("sale_age" %in% names(rv$data) && is.numeric(rv$data[["sale_age"]]))) {
        if (is.null(contract_col)) {
          showNotification(
            "No 'sale_age' or 'contract_date' column designated — skipping sale_age calculation.",
            type = "message", duration = 6
          )
        } else {

        # Parse effective_date
        eff_date <- as.POSIXct(as.character(input$effective_date))

        # Parse contract date values
        contract_vals <- rv$data[[contract_col]]
        if (inherits(contract_vals, "POSIXct")) {
          contract_posix <- contract_vals
        } else if (inherits(contract_vals, "Date")) {
          contract_posix <- as.POSIXct(contract_vals)
        } else if (is.character(contract_vals)) {
          contract_posix <- suppressWarnings(as.POSIXct(contract_vals))
          if (all(is.na(contract_posix[!is.na(contract_vals)]))) {
            showNotification(
              paste0("Cannot parse '", contract_col, "' as dates for sale_age calculation."),
              type = "error", duration = 10
            )
            return()
          }
        } else if (is.numeric(contract_vals)) {
          # Excel serial date number -> Date -> POSIXct
          contract_posix <- as.POSIXct(as.Date(contract_vals, origin = "1899-12-30"))
        } else {
          showNotification(
            paste0("Column '", contract_col, "' cannot be interpreted as dates for sale_age."),
            type = "error", duration = 10
          )
          return()
        }

        # Calculate sale_age in integer days
        sale_age <- as.integer(difftime(eff_date, contract_posix, units = "days"))
        rv$data[["sale_age"]] <- sale_age
        rv$col_types[["sale_age"]] <- "integer"

        # Pre-seed sale_age as included in localStorage so it appears checked
        session$sendCustomMessage("sale_age_added", list(
          filename = rv$file_name
        ))

        showNotification(
          paste0("Added 'sale_age' column (", sum(!is.na(sale_age)),
                 " values, integer days). Click Fit again to include it."),
          type = "message", duration = 8
        )
        return()
        }
      }
    }

    # --- Skip subject row for Appraisal / Market Area Analysis ---
    skip_first <- FALSE
    if (input$purpose == "appraisal") {
      skip_first <- TRUE
    } else if (input$purpose != "appraisal" && isTRUE(input$skip_subject_row)) {
      skip_first <- TRUE
    }
    if (skip_first && nrow(rv$data) >= 2L) {
      fit_data <- rv$data[2:nrow(rv$data), , drop = FALSE]
      showNotification(
        paste0("Skipping row 1 (subject). Fitting on ", nrow(fit_data), " rows."),
        type = "message", duration = 4)
    } else {
      fit_data <- rv$data
    }

    fit_args <- build_fit_args_(df = fit_data)

    # --- Type validation before fitting ---
    if (!is.null(fit_args$type_map)) {
      validation <- validate_types(fit_data, fit_args$type_map, fit_args$predictors)

      if (length(validation$warnings) > 0L) {
        showNotification(
          paste("Type warnings:", paste(validation$warnings, collapse = "; ")),
          type = "warning", duration = 8
        )
      }

      if (!validation$ok) {
        showNotification(
          HTML(paste0("<strong>Type errors (fix before fitting):</strong><br>",
                      paste(validation$errors, collapse = "<br>"))),
          type = "error", duration = 15
        )
        return()
      }

      if (length(validation$date_columns) > 0L) {
        showNotification(
          paste("Date columns converted to numeric (days since epoch):",
                paste(validation$date_columns, collapse = ", ")),
          type = "message", duration = 6
        )
      }
    }

    # --- Random seed for reproducibility ---
    user_seed <- suppressWarnings(as.integer(input$random_seed))
    if (is.null(user_seed) || is.na(user_seed)) {
      seed <- sample.int(.Machine$integer.max, 1L)
    } else {
      seed <- user_seed
    }
    fit_args$.seed <- seed

    # Save to history (last 5 per file) and auto-fill new seed for next run.
    # Use Sys.time() + PID to generate next seed (not R's RNG which may not
    # have advanced in the async path since earth runs in a child process).
    hist <- rv_seed_history()
    hist <- c(seed, setdiff(hist, seed))
    if (length(hist) > 5L) hist <- hist[1:5]
    rv_seed_history(hist)
    next_seed <- as.integer(
      (as.numeric(Sys.time()) * 1000 + Sys.getpid()) %% .Machine$integer.max)
    updateTextInput(session, "random_seed", value = as.character(next_seed))

    use_async <- requireNamespace("callr", quietly = TRUE)

    if (use_async) {
      # --- Async path: run earth in background process ---
      fit_args$.capture_trace <- FALSE
      # Ensure trace >= 1 so the user sees progress in the fitting log
      if (is.null(fit_args$trace) || fit_args$trace < 1) {
        fit_args$trace <- 1
      }
      rv$trace_lines <- character(0)
      rv$result <- NULL
      if (!is.null(rv_report$assets_proc) &&
          inherits(rv_report$assets_proc, "r_process") &&
          rv_report$assets_proc$is_alive()) {
        rv_report$assets_proc$kill()
      }
      rv_report$assets_proc <- NULL
      if (!is.null(rv_report$assets_dir)) {
        unlink(rv_report$assets_dir, recursive = TRUE)
        rv_report$assets_dir <- NULL
      }
      rv$rca_df <- NULL
      rv$rca_targets <- NULL

      rv$bg_proc <- callr::r_bg(
        function(args) {
          seed <- args$.seed
          args$.seed <- NULL
          set.seed(seed)
          cat(sprintf("Dataset: %d obs, %d predictors, degree=%d, seed=%d\n",
                      nrow(args$df), length(args$predictors), args$degree, seed))
          if (!is.null(args$nfold) && args$nfold > 0)
            cat(sprintf("Cross-validation: %d folds\n", args$nfold))
          cat("Running forward pass...\n")
          flush(stdout())
          result <- do.call(earthUI::fit_earth, args)
          result$seed <- seed
          cat(sprintf("Completed in %.1f seconds\n", result$elapsed))
          flush(stdout())
          result
        },
        args = list(args = fit_args),
        stdout = "|", stderr = "|",
        supervise = TRUE,
        wd = tempdir()
      )
      rv$fitting <- TRUE
      eui_log_$start("5. Fit Earth Model")
      session$sendCustomMessage("fitting_start", list())

    } else {
      # --- Sync fallback (no callr) ---
      set.seed(seed)
      eui_log_$start("5. Fit Earth Model")
      session$sendCustomMessage("fitting_start", list())
      withProgress(message = "Fitting Earth model...", value = 0.2, {
        tryCatch({
          setProgress(0.3, detail = "Running forward pass")
          rv$result <- fit_earth(
            df = fit_args$df,
            target = fit_args$target,
            predictors = fit_args$predictors,
            categoricals = fit_args$categoricals,
            linpreds = fit_args$linpreds,
            degree = fit_args$degree,
            allowed_func = fit_args$allowed_func,
            allowed_matrix = fit_args$allowed_matrix,
            nfold = fit_args$nfold,
            nprune = fit_args$nprune,
            thresh = fit_args$thresh,
            penalty = fit_args$penalty,
            minspan = fit_args$minspan,
            endspan = fit_args$endspan,
            fast.k = fit_args$fast.k,
            pmethod = fit_args$pmethod,
            glm = fit_args$glm,
            trace = fit_args$trace,
            nk = fit_args$nk,
            newvar.penalty = fit_args$newvar.penalty,
            fast.beta = fit_args$fast.beta,
            ncross = fit_args$ncross,
            stratify = fit_args$stratify,
            varmod.method = fit_args$varmod.method,
            varmod.exponent = fit_args$varmod.exponent,
            varmod.conv = fit_args$varmod.conv,
            varmod.clamp = fit_args$varmod.clamp,
            varmod.minspan = fit_args$varmod.minspan,
            keepxy = fit_args$keepxy,
            Scale.y = fit_args$Scale.y,
            Adjust.endspan = fit_args$Adjust.endspan,
            Auto.linpreds = fit_args$Auto.linpreds,
            Force.weights = fit_args$Force.weights,
            Use.beta.cache = fit_args$Use.beta.cache,
            Force.xtx.prune = fit_args$Force.xtx.prune,
            Get.leverages = fit_args$Get.leverages,
            Exhaustive.tol = fit_args$Exhaustive.tol,
            wp = fit_args$wp,
            weights = fit_args$weights
          )
          rv$result$seed <- seed
          elapsed <- rv$result$elapsed
          setProgress(1, detail = "Done")
          session$sendCustomMessage("fitting_done",
            list(text = sprintf("Done in %.1fs",
                                elapsed)))
          # Defer file I/O so tab content renders first.
          # write_fit_log_ in onFlushed (fast).
          # auto_export_for_mgcv_ (saveRDS) must not block tab rendering:
          #   Unix/macOS: parallel::mcparallel() forks without serialization
          #   Windows: later::later() delays until tabs render, then blocks briefly
          # Must NOT use callr::r_bg — double-serialization corrupts the object.
          eui_log_$end("5. Fit Earth Model")
          local({
            res <- rv$result
            traces <- isolate(rv$trace_lines)
            folder <- input$output_folder
            fname <- rv$file_name
            session$onFlushed(function() {
              write_fit_log_(folder, traces, fname)
            }, once = TRUE)
            if (.Platform$OS.type == "unix") {
              parallel::mcparallel({
                auto_export_for_mgcv_(res, folder, fname)
              })
            } else {
              later::later(function() {
                auto_export_for_mgcv_(res, folder, fname)
              }, delay = 10)
            }
          })
        }, error = function(e) {
          eui_log_$err("5. Fit Earth Model", e$message)
          session$sendCustomMessage("fitting_done",
            list(text = "Error"))
          showNotification(paste("Model error:", e$message),
                           type = "error", duration = 10)
          write_fit_log_(input$output_folder, c(paste("ERROR:", e$message)), rv$file_name)
        })
      })
    }
  })

  # --- Background process polling observer ---
  send_trace_lines_ <- function(lines, truncate_at = 0L) {
    for (line in lines) {
      if (nzchar(trimws(line))) {
        display <- if (truncate_at > 0L && nchar(line) > truncate_at) {
          paste0(substr(line, 1L, truncate_at), "...")
        } else {
          line
        }
        session$sendCustomMessage("trace_line", list(text = display))
      }
    }
  }

  observe({
    req(rv$fitting)
    invalidateLater(300)
    isolate({
      proc <- rv$bg_proc
      if (is.null(proc)) return()

      # stdout = earth trace output
      new_out <- tryCatch(proc$read_output_lines(), error = function(e) character(0))
      # stderr = messages, warnings, errors
      new_err <- tryCatch(proc$read_error_lines(), error = function(e) character(0))

      if (length(new_out) > 0L) {
        rv$trace_lines <- c(rv$trace_lines, new_out)
        send_trace_lines_(new_out)
      }
      if (length(new_err) > 0L) {
        rv$trace_lines <- c(rv$trace_lines, new_err)
        send_trace_lines_(new_err)
      }

      # Check if process has finished
      if (!proc$is_alive()) {
        # Read any remaining output
        final_out <- tryCatch(proc$read_output_lines(), error = function(e) character(0))
        final_err <- tryCatch(proc$read_error_lines(), error = function(e) character(0))
        if (length(final_out) > 0L) {
          rv$trace_lines <- c(rv$trace_lines, final_out)
          send_trace_lines_(final_out)
        }
        if (length(final_err) > 0L) {
          rv$trace_lines <- c(rv$trace_lines, final_err)
          send_trace_lines_(final_err)
        }

        tryCatch({
          result <- proc$get_result()
          # Store captured trace lines from polling
          result$trace_output <- rv$trace_lines
          rv$result <- result
          eui_log_$end("5. Fit Earth Model")
          session$sendCustomMessage("fitting_done",
            list(text = sprintf("Done in %.1fs",
                                result$elapsed)))
          # Defer file I/O until AFTER the flush so tabs appear immediately.
          # saveRDS runs in onFlushed (not callr::r_bg) to avoid double-serialization
          # which corrupts environments/formulas in the earth model object.
          local({
            res <- result
            traces <- rv$trace_lines
            folder <- input$output_folder
            fname <- rv$file_name
            session$onFlushed(function() {
              write_fit_log_(folder, traces, fname)
              auto_export_for_mgcv_(res, folder, fname)
            }, once = TRUE)
          })
        }, error = function(e) {
          eui_log_$err("5. Fit Earth Model", e$message)
          # Extract the real error from callr's wrapper
          err_msg <- e$message
          if (!is.null(e$parent)) {
            err_msg <- e$parent$message
          } else {
            # Also check stderr for error details
            err_lines <- rv$trace_lines[grepl("^Error", rv$trace_lines)]
            if (length(err_lines) > 0L) {
              err_msg <- sub("^Error *:? *", "", err_lines[length(err_lines)])
            }
          }
          session$sendCustomMessage("fitting_done", list(text = "Error"))
          showNotification(paste("Model error:", err_msg),
                           type = "error", duration = 15)
          # Write log file on error
          log_lines <- c(rv$trace_lines, paste("ERROR:", err_msg))
          write_fit_log_(input$output_folder, log_lines, rv$file_name)
        })

        rv$fitting <- FALSE
        rv$bg_proc <- NULL
      }
    })
  })

  # --- Abort Fitting ---
  observeEvent(input$abort_fit, {
    proc <- rv$bg_proc
    if (!is.null(proc) && inherits(proc, "r_process") && proc$is_alive()) {
      proc$kill()
      rv$trace_lines <- c(rv$trace_lines, "Aborted by user.")
      eui_log_$err("5. Fit Earth Model", "Aborted by user")
      write_fit_log_(input$output_folder, rv$trace_lines, rv$file_name)
    }
    rv$fitting <- FALSE
    rv$bg_proc <- NULL
    session$sendCustomMessage("fitting_done", list(text = "Aborted by user."))
  })

  # --- Parameter Info Modal ---
  observeEvent(input$param_info, {
    showModal(modalDialog(
      title = "Earth Model Parameters",
      size = "l",
      easyClose = TRUE,
      div(style = "max-height: 70vh; overflow-y: auto; font-size: 0.9em;",
        h5("Forward Pass"),
        tags$dl(
          tags$dt("degree"), tags$dd("Maximum degree of interaction. Default 1 (additive, no interactions)."),
          tags$dt("nk"), tags$dd("Maximum number of model terms before pruning (includes intercept). Default is semi-automatically calculated."),
          tags$dt("thresh"), tags$dd("Forward stepping threshold (default 0.001). Forward pass terminates if adding a term changes RSq by less than thresh."),
          tags$dt("penalty"), tags$dd("GCV penalty per knot. Default is 3 if degree>1, else 2. Values of 0 or -1 have special meaning. Typical range: 2-4."),
          tags$dt("minspan"), tags$dd("Minimum observations between knots. Default 0 (auto-calculated). Use 1 with endspan=1 to consider all x values. Negative values specify max knots per predictor (e.g., -3 = three evenly spaced knots)."),
          tags$dt("endspan"), tags$dd("Minimum observations before first and after last knot. Default 0 (auto-calculated). Be wary of reducing this, especially for predictions near data limits."),
          tags$dt("newvar.penalty"), tags$dd("Penalty for adding a new variable (Friedman's gamma). Default 0. Non-zero values (0.01-0.2) make the model prefer reusing existing variables."),
          tags$dt("fast.k"), tags$dd("Max parent terms considered per forward step (Fast MARS). Default 20. Set 0 to disable Fast MARS. Lower = faster, higher = potentially better model."),
          tags$dt("fast.beta"), tags$dd("Fast MARS ageing coefficient. Default 1. A value of 0 sometimes gives better results."),
          tags$dt("Auto.linpreds"), tags$dd("Default TRUE. If the best knot is at the predictor minimum, add the predictor linearly (no hinge). Only affects predictions outside training data range."),
          tags$dt("linpreds"), tags$dd("Predictors that enter linearly (no hinge functions), set via the 'Linear' checkbox in Variable Configuration.")
        ),
        h5("Pruning"),
        tags$dl(
          tags$dt("pmethod"), tags$dd("Pruning method: backward (default), none, exhaustive, forward, seqrep, cv. Use 'cv' with nfold to select terms by cross-validation."),
          tags$dt("nprune"), tags$dd("Maximum terms (including intercept) in pruned model. Default NULL (all terms from forward pass, after pruning).")
        ),
        h5("Cross Validation"),
        tags$dl(
          tags$dt("nfold"), tags$dd("Number of CV folds. Default 0 (no CV). Auto-set to 10 when degree >= 2. Use trace=0.5 to trace CV."),
          tags$dt("ncross"), tags$dd("Number of cross-validations (each has nfold folds). Default 1. Use higher values (e.g., 30) with variance models."),
          tags$dt("stratify"), tags$dd("Default TRUE. Stratify CV samples so each fold has approximately equal response distribution.")
        ),
        h5("Variance Model"),
        tags$dl(
          tags$dt("varmod.method"), tags$dd("none (default), const, lm, rlm, earth, gam, power, power0, x.lm, x.rlm, x.earth, x.gam. Requires nfold and ncross. Use trace=0.3 to trace. See 'Variance models in earth' by Stephen Milborrow."),
          tags$dt("varmod.exponent"), tags$dd("Power transform for residual regression. Default 1. Use 0.5 if std dev increases with square root of response."),
          tags$dt("varmod.conv"), tags$dd("Convergence criterion (percent) for IRLS in variance model. Default 1. Negative values force that many iterations."),
          tags$dt("varmod.clamp"), tags$dd("Minimum estimated std dev = varmod.clamp * mean(sd(residuals)). Default 0.1. Prevents negative or tiny std dev estimates."),
          tags$dt("varmod.minspan"), tags$dd("minspan for internal earth call in variance model. Default -3 (three evenly spaced knots per predictor).")
        ),
        h5("GLM"),
        tags$dl(
          tags$dt("glm family"), tags$dd("Optional GLM family applied to earth basis functions. Choices: none, gaussian, binomial, poisson. Example use: binomial for binary outcomes.")
        ),
        h5("Other"),
        tags$dl(
          tags$dt("trace"), tags$dd("Trace level: 0=none, 0.3=variance model, 0.5=CV, 1=overview, 2=forward pass, 3=pruning, 4=model mats/pruning details, 5=full details."),
          tags$dt("keepxy"), tags$dd("Default FALSE. Set TRUE to retain x, y, subset, weights in the model object. Required for some cv. statistics. Makes CV slower."),
          tags$dt("Scale.y"), tags$dd("Default TRUE. Scale response internally (subtract mean, divide by sd). Provides better numeric stability."),
          tags$dt("Adjust.endspan"), tags$dd("In interaction terms, endspan is multiplied by this value. Default 2. Reduces overfitting at data boundaries."),
          tags$dt("Exhaustive.tol"), tags$dd("Default 1e-10. If reciprocal condition number of bx < this, forces pmethod='backward'. Only applies with pmethod='exhaustive'."),
          tags$dt("Use.beta.cache"), tags$dd("Default TRUE. Caches regression coefficients in forward pass for speed (20%+ faster). Uses more memory."),
          tags$dt("Force.xtx.prune"), tags$dd("Default FALSE. Force use of X'X-based subset evaluation in pruning (instead of QR-based leaps). Only for advanced use."),
          tags$dt("Get.leverages"), tags$dd("Default TRUE (unless >100k cases). Calculate diagonal hat values for linear regression of y on bx. Needed for some diagnostics."),
          tags$dt("Force.weights"), tags$dd("Default FALSE. For testing: force weighted code path even without weights.")
        )
      )
    ))
  })

  # REQUIREMENT: Tab waiting messages are handled via static HTML + JS toggling.
  # See ui.R: each tab has .eui-tab-waiting (visible by default) and
  # .eui-tab-content (hidden by default). The observers above send
  # 'eui_tabs_ready' and 'eui_rca_ready' JS messages to toggle them.
  # Do NOT use uiOutput wrappers (suspended in inactive tabs, never update).
  # Do NOT use conditionalPanel (causes all tabs to render simultaneously).

  # --- Results: Summary ---
  output$summary_metrics <- renderUI({
    req(rv$result)
    s <- format_summary(rv$result)

    if (isTRUE(s$multi)) {
      # Per-response metrics cards
      targets <- rv$result$target
      resp_cards <- lapply(seq_along(targets), function(i) {
        tgt <- targets[i]
        tagList(
          tags$h6(tgt, style = "margin-top: 10px; font-weight: bold;"),
          tags$div(
            class = "row", style = "margin-bottom: 10px;",
            tags$div(class = "col-md-3",
                     tags$div(class = "card text-center",
                              style = "padding: 8px;",
                              tags$h6("R\u00b2"),
                              tags$h4(sprintf("%.4f", s$r_squared[i])))),
            tags$div(class = "col-md-3",
                     tags$div(class = "card text-center",
                              style = "padding: 8px;",
                              tags$h6("GRSq"),
                              tags$h4(sprintf("%.4f", s$grsq[i])))),
            tags$div(class = "col-md-3",
                     tags$div(class = "card text-center",
                              style = "padding: 8px;",
                              tags$h6("CV R\u00b2"),
                              tags$h4(if (!is.na(s$cv_rsq[i])) sprintf("%.4f", s$cv_rsq[i]) else "N/A"))),
            tags$div(class = "col-md-3",
                     tags$div(class = "card text-center",
                              style = "padding: 8px;",
                              tags$h6("Terms"),
                              tags$h4(s$n_terms)))
          )
        )
      })

      metrics <- tagList(resp_cards)

      # CV info (per-response)
      if (rv$result$cv_enabled && !all(is.na(s$cv_rsq))) {
        cv_lines <- vapply(seq_along(targets), function(i) {
          if (!is.na(s$cv_rsq[i])) {
            sprintf("%s: CV R\u00b2 = %.4f | Training R\u00b2 = %.4f",
                    targets[i], s$cv_rsq[i], s$r_squared[i])
          } else {
            ""
          }
        }, character(1))
        cv_lines <- cv_lines[nzchar(cv_lines)]
        if (length(cv_lines) > 0L) {
          metrics <- tagList(
            metrics,
            tags$div(
              class = "alert alert-info",
              style = "font-size: 0.9em;",
              HTML(paste(cv_lines, collapse = "<br>"))
            )
          )
        }
      }
    } else {
      # Single-response metrics
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
                            tags$h6("CV R\u00b2"), tags$h4(if (!is.na(s$cv_rsq)) sprintf("%.4f", s$cv_rsq) else "N/A"))),
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
    }

    metrics
  })

  # --- Results: Model Equation ---
  output$model_equation <- renderUI({
    req(rv$result)
    eq <- format_model_equation(rv$result)
    if (inherits(eq, "earthUI_equation_multi")) {
      # Show one equation per response with a heading
      eq_blocks <- lapply(seq_along(eq$targets), function(i) {
        sub_eq <- eq$equations[[i]]
        tagList(
          tags$h5(eq$targets[i], style = "margin-top: 16px;"),
          withMathJax(HTML(sub_eq$latex_inline))
        )
      })
      do.call(tagList, eq_blocks)
    } else {
      withMathJax(HTML(eq$latex_inline))
    }
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

  # --- Response selector for multivariate models ---
  output$response_selector_diag <- renderUI({
    req(rv$result)
    targets <- rv$result$target
    if (length(targets) <= 1L) return(NULL)
    choices <- stats::setNames(seq_along(targets), targets)
    selectInput("diag_response", "Response variable",
                choices = choices, selected = 1L)
  })

  output$response_selector_contrib <- renderUI({
    req(rv$result)
    targets <- rv$result$target
    if (length(targets) <= 1L) return(NULL)
    choices <- stats::setNames(seq_along(targets), targets)
    selectInput("contrib_response", "Response variable",
                choices = choices, selected = 1L)
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

  # --- Results: Contribution (g-function plots) ---
  output$contrib_g_selector <- renderUI({
    req(rv$result)
    gf <- list_g_functions(rv$result)
    if (nrow(gf) == 0L) return(p("No g-functions in model."))

    # Build display labels: "1: sq_ft_total" or "6: sq_ft_total x beds [3D]"
    display_label <- ifelse(
      gf$d >= 2L,
      gsub(" ", " \u00d7 ", gf$label),
      gf$label
    )
    labels <- paste0(gf$index, ": ", display_label,
                     ifelse(gf$d >= 2L, " [3D]", ""))
    choices <- stats::setNames(gf$index, labels)
    selectInput("contrib_g_index", "Select g-function", choices = choices)
  })

  output$contrib_plot_container <- renderUI({
    req(rv$result, input$contrib_g_index)
    gf <- list_g_functions(rv$result)
    idx <- as.integer(input$contrib_g_index)
    if (idx < 1L || idx > nrow(gf)) return(NULL)

    if (gf$d[idx] >= 2L && requireNamespace("plotly", quietly = TRUE)) {
      tagList(
        plotly::plotlyOutput("contrib_plot_3d", height = "500px"),
        hr(),
        plotOutput("contrib_plot_contour", height = "400px")
      )
    } else {
      plotOutput("contrib_plot_2d", height = "400px")
    }
  })

  output$contrib_plot_2d <- renderPlot({
    req(rv$result, input$contrib_g_index)
    ri <- if (length(rv$result$target) > 1L && !is.null(input$contrib_response)) {
      as.integer(input$contrib_response)
    } else {
      NULL
    }
    tryCatch(
      plot_g_function(rv$result, as.integer(input$contrib_g_index),
                      response_idx = ri),
      error = function(e) {
        message("earthUI: g-function 2D plot error: ", e$message)
        plot.new()
        text(0.5, 0.5, paste("Error:", e$message), cex = 1.2)
      }
    )
  })

  if (requireNamespace("plotly", quietly = TRUE)) {
    output$contrib_plot_3d <- plotly::renderPlotly({
      req(rv$result, input$contrib_g_index)
      ri <- if (length(rv$result$target) > 1L && !is.null(input$contrib_response)) {
        as.integer(input$contrib_response)
      } else {
        NULL
      }
      tryCatch(
        plot_g_function(rv$result, as.integer(input$contrib_g_index),
                        response_idx = ri),
        error = function(e) {
          message("earthUI: g-function 3D plot error: ", e$message)
          plotly::plot_ly() |>
            plotly::layout(title = paste("Error:", e$message))
        }
      )
    })
  }

  output$contrib_plot_contour <- renderPlot({
    req(rv$result, input$contrib_g_index)
    ri <- if (length(rv$result$target) > 1L && !is.null(input$contrib_response)) {
      as.integer(input$contrib_response)
    } else {
      NULL
    }
    tryCatch(
      plot_g_contour(rv$result, as.integer(input$contrib_g_index),
                     response_idx = ri),
      error = function(e) {
        message("earthUI: g-function contour plot error: ", e$message)
        plot.new()
        text(0.5, 0.5, paste("Error:", e$message), cex = 1.2)
      }
    )
  })

  # --- Results: Correlation Matrix ---
  output$correlation_plot_ui <- renderUI({
    req(rv$result)
    plotOutput("correlation_plot", height = "800px", width = "800px")
  })

  output$correlation_plot <- renderPlot({
    req(rv$result)
    tryCatch(
      plot_correlation_matrix(rv$result),
      error = function(e) {
        message("earthUI: correlation plot error: ", e$message)
        plot.new()
        text(0.5, 0.5, paste("Error:", e$message), cex = 1.2)
      }
    )
  }, res = 120)

  # --- Results: Diagnostics ---
  output$residuals_plot <- renderPlot({
    req(rv$result)
    ri <- if (length(rv$result$target) > 1L && !is.null(input$diag_response)) {
      as.integer(input$diag_response)
    } else {
      NULL
    }
    plot_residuals(rv$result, response_idx = ri)
  })

  output$qq_plot <- renderPlot({
    req(rv$result)
    ri <- if (length(rv$result$target) > 1L && !is.null(input$diag_response)) {
      as.integer(input$diag_response)
    } else {
      NULL
    }
    plot_qq(rv$result, response_idx = ri)
  })

  output$actual_vs_predicted_plot <- renderPlot({
    req(rv$result)
    ri <- if (length(rv$result$target) > 1L && !is.null(input$diag_response)) {
      as.integer(input$diag_response)
    } else {
      NULL
    }
    plot_actual_vs_predicted(rv$result, response_idx = ri)
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

  # --- Results: RCA Adjustment Percentage Histograms ---
  output$rca_plots_ui <- renderUI({
    req(rv$rca_df, rv$rca_targets)
    df <- rv$rca_df
    targets <- rv$rca_targets

    # Build plot outputs for each target
    plot_tags <- list()
    for (ti in seq_along(targets)) {
      tgt <- targets[ti]
      if (ti == 1L) {
        pct_cols <- list(
          list(col = "residual_pct",  label = "Residual Adj %"),
          list(col = "net_adj_pct",   label = "Net Adj %"),
          list(col = "gross_adj_pct", label = "Gross Adj %")
        )
      } else {
        pct_cols <- list(
          list(col = paste0(tgt, "_residual_pct"),  label = paste0(tgt, " Residual Adj %")),
          list(col = paste0(tgt, "_net_adj_pct"),   label = paste0(tgt, " Net Adj %")),
          list(col = paste0(tgt, "_gross_adj_pct"), label = paste0(tgt, " Gross Adj %"))
        )
      }
      for (pc in pct_cols) {
        plot_id <- paste0("rca_hist_", gsub("[^a-zA-Z0-9]", "_", pc$col))
        local({
          col_name <- pc$col
          plot_label <- pc$label
          output[[plot_id]] <- renderPlot({
            vals <- rv$rca_df[[col_name]]
            vals <- vals[!is.na(vals) & is.finite(vals)]
            if (length(vals) == 0L) return(NULL)
            pct_vals <- vals * 100
            avg_val <- mean(pct_vals)
            med_val <- stats::median(pct_vals)
            sd_val  <- stats::sd(pct_vals)
            bin_width <- 5
            rng <- range(pct_vals)
            brks <- seq(floor(rng[1] / bin_width) * bin_width,
                        ceiling(rng[2] / bin_width) * bin_width,
                        by = bin_width)
            if (length(brks) < 2L) brks <- c(brks[1], brks[1] + bin_width)
            hist_data <- graphics::hist(pct_vals, breaks = brks, plot = FALSE)
            y_max <- max(hist_data$counts) * 1.25
            graphics::par(mar = c(5, 4, 4, 2) + 0.1)
            graphics::hist(pct_vals, breaks = brks, col = "#4A90D9", border = "white",
                           main = plot_label,
                           xlab = "Percentage (%)", ylab = "Frequency",
                           las = 1, ylim = c(0, y_max))
            graphics::abline(v = avg_val, col = "#E74C3C", lwd = 2, lty = 2)
            graphics::abline(v = med_val, col = "#2ECC71", lwd = 2, lty = 2)
            graphics::legend("topright",
                             legend = c(
                               sprintf("Mean: %.2f%%", avg_val),
                               sprintf("Median: %.2f%%", med_val),
                               sprintf("Std Dev: %.2f%%", sd_val)
                             ),
                             col = c("#E74C3C", "#2ECC71", NA),
                             lwd = c(2, 2, NA), lty = c(2, 2, NA),
                             bty = "n", cex = 1.1)
          }, res = 120)
        })
        plot_tags <- c(plot_tags, list(
          plotOutput(plot_id, height = "350px"),
          tags$br()
        ))
      }
      if (ti < length(targets)) {
        plot_tags <- c(plot_tags, list(tags$hr()))
      }
    }
    do.call(tagList, plot_tags)
  })

  # --- Results: Earth Output ---
  output$earth_output <- renderPrint({
    req(rv$result)
    model <- rv$result$model

    cat(sprintf("== Timing: %.2f seconds ==\n", rv$result$elapsed))
    if (!is.null(rv$result$seed)) {
      cat(sprintf("== Random Seed: %d ==\n", rv$result$seed))
    }
    cat("\n")

    cat("== Model ==\n\n")
    print(model)

    cat("\n\n== Summary ==\n\n")
    print(summary(model))

    if (!is.null(model$varmod)) {
      cat("\n\n== Variance Model ==\n\n")
      print(model$varmod)
    }

    if (length(rv$result$trace_output) > 0L) {
      trace_lines <- rv$result$trace_output
      trace_lines <- trace_lines[!grepl("^Removed .* rows with miss", trace_lines)]
      trace_lines <- trace_lines[!grepl("^CV fold ", trace_lines)]
      if (length(trace_lines) > 0L) {
        cat("\n\n== Trace Log ==\n\n")
        for (line in trace_lines) {
          if (nchar(line) > 25L) {
            cat(substr(line, 1L, 25L), "...\n")
          } else {
            cat(line, "\n")
          }
        }
      }
    }
  })

  # --- Export Report (saves to output folder, async via callr) ---
  rv_report <- reactiveValues(proc = NULL, out_path = NULL, assets_dir = NULL,
                              assets_proc = NULL)

  observeEvent(input$export_report_btn, {
    req(rv$result)
    # Don't start a new render if one is already running
    if (!is.null(rv_report$proc)) {
      showNotification("Report rendering already in progress.",
                       type = "warning", duration = 5)
      return()
    }

    fmt <- input$export_format
    folder <- input$output_folder
    if (is.null(folder) || !nzchar(folder)) {
      folder <- path.expand("~/Downloads")
    }
    if (!dir.exists(folder)) dir.create(folder, recursive = TRUE)

    base <- tools::file_path_sans_ext(rv$file_name %||% "earth")
    out_name <- paste0(base, "_report_",
                       format(Sys.time(), "%Y%m%d_%H%M%S"), ".", fmt)
    out_path <- file.path(folder, out_name)
    rv_report$out_path <- out_path
    rv_report$fmt <- fmt

    eui_log_$start(paste0("9. Download Report (", toupper(fmt), ")"))
    session$sendCustomMessage("report_start",
      list(text = paste0("Rendering ", toupper(fmt), " report...")))

    assets <- rv_report$assets_dir
    paper <- earthUI:::locale_paper_()

    use_async <- requireNamespace("callr", quietly = TRUE)
    if (use_async) {
      rv_report$proc <- callr::r_bg(
        function(assets_dir, output_format, output_file, paper_size, result) {
          earthUI::render_report(result,
                                 output_format = output_format,
                                 output_file = output_file,
                                 paper_size = paper_size,
                                 assets_dir = assets_dir)
        },
        args = list(assets_dir = assets, output_format = fmt,
                     output_file = out_path, paper_size = paper,
                     result = rv$result),
        stdout = "|", stderr = "|",
        supervise = TRUE,
        wd = tempdir()
      )
    } else {
      # Sync fallback
      tryCatch({
        render_report(rv$result,
                      output_format = fmt,
                      output_file = out_path,
                      paper_size = paper,
                      assets_dir = assets)
        session$sendCustomMessage("download_check_multi",
                                  list(id = "export_report_btn"))
        session$sendCustomMessage("report_done",
          list(text = paste0("Report saved to: ", out_path), error = FALSE))
        eui_log_$end(paste0("9. Download Report (", toupper(rv_report$fmt), ")"))
        message("earthUI: report saved to ", out_path)
      }, error = function(e) {
        message("earthUI export error: ", e$message)
        session$sendCustomMessage("report_done",
          list(text = paste("Error:", e$message), error = TRUE))
      })
      rv_report$proc <- NULL
    }
  })

  # Polling observer for async report rendering
  observe({
    req(rv_report$proc)
    invalidateLater(500)
    proc <- rv_report$proc
    if (!inherits(proc, "r_process")) return()

    # Read stdout/stderr for progress
    out_lines <- tryCatch(proc$read_output_lines(), error = function(e) character(0))
    err_lines <- tryCatch(proc$read_error_lines(), error = function(e) character(0))
    for (line in c(out_lines, err_lines)) {
      if (nzchar(trimws(line))) {
        session$sendCustomMessage("report_line", list(text = line, error = FALSE))
      }
    }

    if (!proc$is_alive()) {
      # Collect any remaining output
      final_out <- tryCatch(proc$read_output_lines(), error = function(e) character(0))
      final_err <- tryCatch(proc$read_error_lines(), error = function(e) character(0))
      for (line in c(final_out, final_err)) {
        if (nzchar(trimws(line))) {
          session$sendCustomMessage("report_line", list(text = line, error = FALSE))
        }
      }

      tryCatch({
        proc$get_result()
        out_path <- rv_report$out_path
        if (file.exists(out_path)) {
          session$sendCustomMessage("download_check",
                                    list(id = "export_report_btn"))
          session$sendCustomMessage("report_done",
            list(text = paste0("Report saved to: ", out_path), error = FALSE))
          eui_log_$end(paste0("9. Download Report (", toupper(rv_report$fmt), ")"))
        message("earthUI: report saved to ", out_path)
        } else {
          session$sendCustomMessage("report_done",
            list(text = "Error: output file not found", error = TRUE))
        }
      }, error = function(e) {
        err_msg <- if (!is.null(e$parent)) e$parent$message else e$message
        message("earthUI export error: ", err_msg)
        session$sendCustomMessage("report_done",
          list(text = paste("Error:", err_msg), error = TRUE))
      })
      rv_report$proc <- NULL
    }
  })

  # --- 8. Generate Sales Grid & Download ---
  # Step 1: Button click shows modal with recommended comps
  observeEvent(input$sales_grid_btn, {
    req(rv$rca_df)
    rca <- rv$rca_df
    n_total <- nrow(rca)
    if (n_total < 2) {
      showNotification("Need at least 2 rows (subject + 1 comp).",
                       type = "error", duration = 8)
      return()
    }

    # Compute gross_adj_pct for all weight > 0 rows (exclude subject row 1)
    has_gross_pct <- "gross_adjustments" %in% colnames(rca) &&
                     "sale_price" %in% colnames(rca)
    wt_col <- if ("weight" %in% colnames(rca)) rca[["weight"]] else rep(1, n_total)

    # Find the sale_age column from specials or default to "sale_age"
    sg_specials <- input$col_specials
    sa_col_name <- "sale_age"
    if (!is.null(sg_specials)) {
      for (nm in names(sg_specials)) {
        if (sg_specials[[nm]] == "sale_age") { sa_col_name <- nm; break }
      }
    }

    # Build comp info table (rows 2..n_total with weight > 0)
    comp_info <- data.frame(
      row       = 2:n_total,
      id        = if ("id" %in% colnames(rca)) rca[["id"]][2:n_total] else 2:n_total,
      address   = if ("street_address" %in% colnames(rca)) {
                    rca[["street_address"]][2:n_total]
                  } else rep("", n_total - 1),
      sale_price = if ("sale_price" %in% colnames(rca)) {
                     rca[["sale_price"]][2:n_total]
                   } else rep(NA, n_total - 1),
      sale_age  = if (sa_col_name %in% colnames(rca)) {
                    rca[[sa_col_name]][2:n_total]
                  } else rep(NA, n_total - 1),
      weight    = wt_col[2:n_total],
      gross_adj = if ("gross_adjustments" %in% colnames(rca)) {
                    rca[["gross_adjustments"]][2:n_total]
                  } else rep(0, n_total - 1),
      stringsAsFactors = FALSE
    )

    # Compute gross_adj_pct
    comp_info$gross_adj_pct <- ifelse(
      !is.na(comp_info$sale_price) & comp_info$sale_price != 0,
      abs(comp_info$gross_adj / comp_info$sale_price),
      NA
    )

    # Filter: weight > 0 only
    eligible <- comp_info[!is.na(comp_info$weight) & comp_info$weight > 0, ]

    # Sort all eligible by gross_adj_pct ascending
    eligible <- eligible[order(eligible$gross_adj_pct, na.last = TRUE), ]

    # Recommended: gross_adj_pct < 0.25, then sort by sale_age ascending
    recommended <- eligible[!is.na(eligible$gross_adj_pct) &
                            eligible$gross_adj_pct < 0.25, ]
    recommended <- recommended[order(recommended$sale_age, na.last = TRUE), ]

    # Cap at 30
    if (nrow(recommended) > 30) recommended <- recommended[1:30, ]

    # Others not recommended (for "add more" section)
    others <- eligible[is.na(eligible$gross_adj_pct) |
                       eligible$gross_adj_pct >= 0.25, ]
    others <- others[order(others$gross_adj_pct, na.last = TRUE), ]

    # Store for the confirm handler
    rv$sg_recommended <- recommended
    rv$sg_others <- others

    # Build modal UI
    rec_checks <- if (nrow(recommended) > 0) {
      lapply(seq_len(nrow(recommended)), function(i) {
        r <- recommended[i, ]
        lbl <- sprintf("Row %d | %s | SP: $%s | Age: %s | Gross: %.1f%%",
                       r$row,
                       substr(as.character(r$address), 1, 30),
                       formatC(r$sale_price, format = "f", digits = 0,
                               big.mark = ","),
                       as.character(r$sale_age),
                       r$gross_adj_pct * 100)
        tags$div(
          checkboxInput(paste0("sg_rec_", r$row), lbl, value = TRUE),
          style = "margin-bottom: 0px;"
        )
      })
    } else {
      tags$p("No comps with gross adjustment < 25% found.",
             style = "color: var(--bs-secondary-color);")
    }

    other_checks <- if (nrow(others) > 0) {
      lapply(seq_len(min(nrow(others), 50)), function(i) {
        r <- others[i, ]
        pct_str <- if (!is.na(r$gross_adj_pct)) {
          sprintf("%.1f%%", r$gross_adj_pct * 100)
        } else "N/A"
        lbl <- sprintf("Row %d | %s | SP: $%s | Age: %s | Gross: %s",
                       r$row,
                       substr(as.character(r$address), 1, 30),
                       formatC(r$sale_price, format = "f", digits = 0,
                               big.mark = ","),
                       as.character(r$sale_age),
                       pct_str)
        tags$div(
          checkboxInput(paste0("sg_rec_", r$row), lbl, value = FALSE),
          style = "margin-bottom: 0px;"
        )
      })
    } else NULL

    showModal(modalDialog(
      title = "Sales Grid — Select Comparables (max 30)",
      size = "l",
      tags$div(
        style = "max-height: 500px; overflow-y: auto;",
        tags$h5(paste0("Recommended Comps (gross adj < 25%, ",
                       "sorted by sale age) — ",
                       nrow(recommended), " found")),
        rec_checks,
        if (!is.null(other_checks)) {
          tagList(
            hr(),
            tags$h5("Additional Comps (gross adj >= 25%)"),
            other_checks
          )
        }
      ),
      footer = tagList(
        modalButton("Cancel"),
        actionButton("sg_confirm", "Generate Sales Grid",
                     class = "btn-primary")
      )
    ))
  })

  # Step 2: Confirm button in modal — generate the grid

  observeEvent(input$sg_confirm, {
    req(rv$rca_df)
    removeModal()

    # Collect checked rows from both recommended and others
    all_candidate_rows <- c(
      if (!is.null(rv$sg_recommended) && nrow(rv$sg_recommended) > 0)
        rv$sg_recommended$row else integer(0),
      if (!is.null(rv$sg_others) && nrow(rv$sg_others) > 0)
        rv$sg_others$row[seq_len(min(nrow(rv$sg_others), 50))] else integer(0)
    )
    comp_rows <- integer(0)
    for (r in all_candidate_rows) {
      cb_val <- input[[paste0("sg_rec_", r)]]
      if (!is.null(cb_val) && isTRUE(cb_val)) {
        comp_rows <- c(comp_rows, r)
      }
    }

    if (length(comp_rows) == 0) {
      showNotification("No comps selected.", type = "warning", duration = 8)
      return()
    }
    if (length(comp_rows) > 30) {
      comp_rows <- comp_rows[1:30]
      showNotification("Capped at 30 comps.", type = "warning", duration = 5)
    }

    # Sort selected comps by gross_adj_pct ascending
    rca <- rv$rca_df
    sp <- if ("sale_price" %in% colnames(rca)) rca[["sale_price"]][comp_rows] else rep(NA, length(comp_rows))
    gross <- if ("gross_adjustments" %in% colnames(rca)) rca[["gross_adjustments"]][comp_rows] else rep(0, length(comp_rows))
    gap <- ifelse(!is.na(sp) & sp != 0, abs(gross / sp), NA)
    comp_rows <- comp_rows[order(gap, na.last = TRUE)]

    folder <- input$output_folder
    if (is.null(folder) || !nzchar(folder)) folder <- path.expand("~/Downloads")
    if (!dir.exists(folder)) dir.create(folder, recursive = TRUE)

    out_path <- file.path(folder, paste0("SalesGrid_",
                          format(Sys.time(), "%Y%m%d_%H%M%S"), ".xlsx"))

    message("earthUI: Sales grid with ", length(comp_rows),
            " comps (rows: ", paste(comp_rows, collapse = ","), ")")

    tryCatch({
      tmp_adj <- tempfile(fileext = ".xlsx")
      writexl::write_xlsx(rv$rca_df, tmp_adj)

      grid_script <- system.file("app", "sales_grid.R", package = "earthUI")
      if (!nzchar(grid_script)) {
        showNotification("Sales grid script not found in package.",
                         type = "error", duration = 10)
        return()
      }
      source(grid_script, local = TRUE)

      # Build specials named list from designations
      sg_specials_map <- list()
      sg_input <- input$col_specials
      if (!is.null(sg_input)) {
        for (nm in names(sg_input)) {
          sp_type <- sg_input[[nm]]
          if (sp_type != "no") sg_specials_map[[sp_type]] <- nm
        }
      }

      n_comp <- length(comp_rows)
      n_sheet <- ceiling(n_comp / 3)
      eui_log_$start("8. Generate Sales Grid & Download")
      withProgress(
        message = "Generating Sales Grid",
        detail = sprintf("0 of %d comps processed", n_comp),
        value = 0, {
        generate_sales_grid(
          adjusted_file     = tmp_adj,
          comp_rows         = comp_rows,
          output_file       = out_path,
          specials          = sg_specials_map,
          progress_fn       = function(sheet, total_sheets, comps_done, total_comps) {
            setProgress(
              value = comps_done / total_comps,
              detail = sprintf("Sheet %d of %d — %d of %d comps processed",
                               sheet, total_sheets, comps_done, total_comps))
          }
        )
      })
      unlink(tmp_adj)

      eui_log_$end("8. Generate Sales Grid & Download")
      showNotification(paste0("Sales grid saved to: ", out_path,
                              " (", length(comp_rows), " comps, ",
                              ceiling(length(comp_rows) / 3), " sheets)"),
                       type = "message", duration = 10)
      session$sendCustomMessage("download_check",
        list(id = "sales_grid_btn"))
    }, error = function(e) {
      showNotification(paste("Sales grid error:", e$message),
                       type = "error", duration = 10)
    })
  })

  # --- Export Data (Excel) ---
  export_data_filename_ <- function() {
    base <- tools::file_path_sans_ext(rv$file_name %||% "data")
    paste0(base, "_modified_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".xlsx")
  }

  observeEvent(input$export_data, {
    req(rv$data)
    folder <- input$output_folder
    if (is.null(folder) || !nzchar(folder)) folder <- path.expand("~/Downloads")
    if (!dir.exists(folder)) dir.create(folder, recursive = TRUE)
    out_path <- file.path(folder, export_data_filename_())
    message("earthUI: export_data clicked, writing to ", out_path)
    eui_log_$start("6. Download Intermediate Output")
    export_data_content_(out_path, "export_data")
    eui_log_$end("6. Download Intermediate Output")
    if (file.exists(out_path)) {
      showNotification(paste0("Output saved to: ", out_path),
                       type = "message", duration = 8)
    }
  })

  observeEvent(input$export_data_nonadj, {
    req(rv$data)
    folder <- input$output_folder
    if (is.null(folder) || !nzchar(folder)) folder <- path.expand("~/Downloads")
    if (!dir.exists(folder)) dir.create(folder, recursive = TRUE)
    out_path <- file.path(folder, export_data_filename_())
    export_data_content_(out_path, "export_data_nonadj")
    showNotification(paste0("Output saved to: ", out_path),
                     type = "message", duration = 8)
  })

  export_data_content_ <- function(file, btn_id = "export_data") {
    tryCatch({
      req(rv$data)
      if (!requireNamespace("writexl", quietly = TRUE)) {
        showNotification(
          "Package 'writexl' required. Install with: install.packages('writexl')",
          type = "error", duration = 10)
        return()
      }

      export_df <- rv$data

      # --- Append model columns if a model is fitted ---
      if (!is.null(rv$result)) {
        model   <- rv$result$model
        targets <- rv$result$target
        eq      <- format_model_equation(rv$result)

        # Find living_area column from specials
        la_col <- NULL
        specials <- input$col_specials
        if (!is.null(specials)) {
          for (nm in names(specials)) {
            if (specials[[nm]] == "living_area" && nm %in% names(export_df)) {
              la_col <- nm
              break
            }
          }
        }

        multi <- length(targets) > 1L

        # Align factor levels in export_df with training data so predict() works
        # for rows with unseen levels, predictions will be NA
        train_df <- rv$result$data
        pred_df  <- export_df
        for (col in names(train_df)) {
          if (is.factor(train_df[[col]]) && col %in% names(pred_df)) {
            pred_df[[col]] <- factor(pred_df[[col]],
                                     levels = levels(train_df[[col]]))
          }
        }
        pred_mat <- stats::predict(model, newdata = pred_df)

        for (ri in seq_along(targets)) {
          tgt <- targets[ri]
          suffix <- if (multi) paste0("_", ri) else ""

          # Get equation groups for this response
          if (multi) {
            eq_ri <- eq$equations[[ri]]
          } else {
            eq_ri <- eq
          }
          groups <- eq_ri$groups

          # --- est_<target> from predict() ---
          actual <- export_df[[tgt]]
          # In appraisal mode, subject (row 1) has no real sale price
          if (input$purpose == "appraisal" && nrow(export_df) >= 1L) {
            actual[1L] <- NA_real_
          }
          predicted <- if (multi) as.numeric(pred_mat[, ri]) else as.numeric(pred_mat)

          est_col <- paste0("est_", tgt)
          export_df[[est_col]] <- round(predicted, 1)

          # residual = actual - predicted (from model)
          resid_col <- paste0("residual", suffix)
          export_df[[resid_col]] <- round(actual - predicted, 1)

          # CQA: % of comps with smaller signed residual / 10
          # Large positive residual = high CQA (~10), large negative = low CQA (~0)
          resid_vals <- export_df[[resid_col]]
          n_valid <- sum(!is.na(resid_vals))
          cqa_col <- paste0("cqa", suffix)
          cqa_vals <- vapply(resid_vals, function(r) {
            if (is.na(r)) return(NA_real_)
            sum(resid_vals < r, na.rm = TRUE) / n_valid * 10
          }, numeric(1))
          export_df[[cqa_col]] <- round(cqa_vals, 2)

          # residual_sf = residual / living_area
          if (!is.null(la_col)) {
            la <- export_df[[la_col]]
            resid_sf_col <- paste0("residual_sf", suffix)
            export_df[[resid_sf_col]] <- round(export_df[[resid_col]] / la, 1)

            # CQA_SF: % of comps with smaller signed residual_sf / 10
            resid_sf_vals <- export_df[[resid_sf_col]]
            n_valid_sf <- sum(!is.na(resid_sf_vals))
            cqa_sf_col <- paste0("cqa_sf", suffix)
            cqa_sf_vals <- vapply(resid_sf_vals, function(r) {
              if (is.na(r)) return(NA_real_)
              sum(resid_sf_vals < r, na.rm = TRUE) / n_valid_sf * 10
            }, numeric(1))
            export_df[[cqa_sf_col]] <- round(cqa_sf_vals, 2)
          }

          # --- Per-g-function contributions ---
          intercept_group <- NULL
          contrib_groups  <- list()
          for (grp in groups) {
            if (grp$degree == 0L) {
              intercept_group <- grp
            } else {
              contrib_groups <- c(contrib_groups, list(grp))
            }
          }

          # Basis (intercept) contribution — constant for all rows
          basis_val <- if (!is.null(intercept_group)) {
            intercept_group$terms[[1]]$coefficient
          } else {
            0
          }

          contrib_total <- rep(basis_val, nrow(export_df))
          for (grp in contrib_groups) {
            col_label <- gsub(" ", "_", grp$label)
            col_name  <- paste0(col_label, "_contribution", suffix)
            contrib   <- earthUI:::eval_g_function_(model, grp, pred_df,
                                                     response_idx = if (multi) ri else NULL)
            export_df[[col_name]] <- round(contrib, 1)
            contrib_total <- contrib_total + contrib
          }

          export_df[[paste0("basis", suffix)]] <- round(basis_val, 1)

          # calc_residual = actual - (basis + all contributions)
          calc_resid_col <- paste0("calc_residual", suffix)
          export_df[[calc_resid_col]] <- round(actual - contrib_total, 1)

        }
      }

      # Sort by residual_sf (or residual) descending for appraisal/market
      if (input$purpose %in% c("appraisal", "market") && !is.null(rv$result)) {
        has_subject <- (input$purpose == "appraisal") ||
                       (input$purpose == "market" && isTRUE(input$skip_subject_row))
        sort_col <- if ("residual_sf" %in% names(export_df)) "residual_sf" else "residual"
        if (sort_col %in% names(export_df)) {
          if (has_subject && nrow(export_df) >= 2L) {
            comps <- export_df[2:nrow(export_df), , drop = FALSE]
            comps <- comps[order(comps[[sort_col]], decreasing = TRUE, na.last = TRUE), , drop = FALSE]
            export_df <- rbind(export_df[1L, , drop = FALSE], comps)
          } else {
            export_df <- export_df[order(export_df[[sort_col]], decreasing = TRUE, na.last = TRUE), , drop = FALSE]
          }
        }
      }

      # Move ranking columns to the left: residual_sf, cqa_sf, residual, cqa
      rank_cols <- c("residual_sf", "cqa_sf", "residual", "cqa")
      rank_cols <- rank_cols[rank_cols %in% names(export_df)]
      if (length(rank_cols) > 0L) {
        other_cols <- setdiff(names(export_df), rank_cols)
        export_df <- export_df[, c(rank_cols, other_cols), drop = FALSE]
      }

      # Write with openxlsx for cell formatting
      wb <- openxlsx::createWorkbook()
      openxlsx::addWorksheet(wb, "Data")
      openxlsx::writeData(wb, "Data", export_df)
      # Apply number formats to specific columns
      col_names <- names(export_df)
      fmt_map <- list(
        residual_sf = "#,##0.00",
        cqa_sf      = "0.00",
        residual    = "#,##0",
        cqa         = "0.00"
      )
      for (cn in names(fmt_map)) {
        ci <- match(cn, col_names)
        if (!is.na(ci)) {
          openxlsx::addStyle(wb, "Data",
            style = openxlsx::createStyle(numFmt = fmt_map[[cn]]),
            rows = 2:(nrow(export_df) + 1L), cols = ci,
            gridExpand = TRUE, stack = TRUE)
        }
      }
      openxlsx::saveWorkbook(wb, file, overwrite = TRUE)
      session$sendCustomMessage("download_check", list(id = btn_id))
    }, error = function(e) {
      msg <- paste("Download error:", conditionMessage(e))
      message(msg)  # to R console
      showNotification(msg, type = "error", duration = 15)
    })
  }

  # --- RCA Raw Output ---

  # Show modal when button is clicked
  observeEvent(input$rca_output_btn, {
    req(rv$result, input$purpose == "appraisal")
    eui_log_$start("7. Calculate RCA Adjustments & Download")

    # Check if living_area is designated (for CQA_SF option)
    has_la <- FALSE
    specials <- input$col_specials
    if (!is.null(specials)) {
      for (nm in names(specials)) {
        if (specials[[nm]] == "living_area" && nm %in% names(rv$data)) {
          has_la <- TRUE
          break
        }
      }
    }

    cqa_choices <- c("CQA" = "cqa")
    if (has_la) cqa_choices <- c(cqa_choices, "CQA per SF" = "cqa_sf")

    showModal(modalDialog(
      title = "RCA Raw Output — Subject CQA Score",
      radioButtons("rca_cqa_type", "Score type:",
                   choices = cqa_choices, selected = "cqa", inline = TRUE),
      numericInput("rca_cqa_value", "CQA score for subject (0.00–9.99):",
                   value = 5.00, min = 0, max = 9.99, step = 0.01),
      footer = tagList(
        modalButton("Cancel"),
        actionButton("export_rca", "Generate", class = "btn-primary")
      ),
      size = "s"
    ))
  })

  # RCA download handler
  observeEvent(input$export_rca, {
      req(rv$data, rv$result, input$purpose == "appraisal", nrow(rv$data) >= 2L)
      removeModal()

      folder <- input$output_folder
      if (is.null(folder) || !nzchar(folder)) folder <- path.expand("~/Downloads")
      if (!dir.exists(folder)) dir.create(folder, recursive = TRUE)
      base <- tools::file_path_sans_ext(rv$file_name %||% "data")
      file <- file.path(folder, paste0(base, "_adjusted_",
                                       format(Sys.time(), "%Y%m%d_%H%M%S"), ".xlsx"))

      if (!requireNamespace("writexl", quietly = TRUE)) {
        showNotification("Package 'writexl' required.", type = "error", duration = 10)
        return()
      }

    tryCatch({

      model   <- rv$result$model
      targets <- rv$result$target
      tgt     <- targets[1L]
      eq      <- format_model_equation(rv$result)
      eq_ri   <- if (length(targets) > 1L) eq$equations[[1L]] else eq
      groups  <- eq_ri$groups
      multi   <- length(targets) > 1L
      ri      <- 1L

      # Find living_area column
      la_col <- NULL
      specials <- input$col_specials
      if (!is.null(specials)) {
        for (nm in names(specials)) {
          if (specials[[nm]] == "living_area" && nm %in% names(rv$data)) {
            la_col <- nm
            break
          }
        }
      }

      export_df <- rv$data

      # Align factor levels with training data
      train_df <- rv$result$data
      pred_df  <- export_df
      for (col in names(train_df)) {
        if (is.factor(train_df[[col]]) && col %in% names(pred_df)) {
          pred_df[[col]] <- factor(pred_df[[col]],
                                   levels = levels(train_df[[col]]))
        }
      }

      # --- Predict on all rows ---
      pred_mat <- stats::predict(model, newdata = pred_df)
      predicted <- if (multi) as.numeric(pred_mat[, ri]) else as.numeric(pred_mat)
      export_df[[paste0("est_", tgt)]] <- round(predicted, 1)

      # Actual sale prices (subject = NA)
      actual <- export_df[[tgt]]
      actual[1L] <- NA_real_

      # Residuals for comps
      residuals_val <- actual - predicted
      export_df[["residual"]] <- round(residuals_val, 1)

      # Per-SF residuals
      if (!is.null(la_col)) {
        la <- export_df[[la_col]]
        export_df[["residual_sf"]] <- round(residuals_val / la, 1)
      }

      # CQA scores (comps only, subject = NA)
      comp_resid <- residuals_val[-1L]
      n_comps <- sum(!is.na(comp_resid))
      cqa_all <- vapply(residuals_val, function(r) {
        if (is.na(r)) return(NA_real_)
        sum(comp_resid < r, na.rm = TRUE) / n_comps * 10
      }, numeric(1))
      export_df[["cqa"]] <- round(cqa_all, 2)

      if (!is.null(la_col)) {
        resid_sf_vals <- export_df[["residual_sf"]]
        comp_resid_sf <- resid_sf_vals[-1L]
        n_comps_sf <- sum(!is.na(comp_resid_sf))
        cqa_sf_all <- vapply(resid_sf_vals, function(r) {
          if (is.na(r)) return(NA_real_)
          sum(comp_resid_sf < r, na.rm = TRUE) / n_comps_sf * 10
        }, numeric(1))
        export_df[["cqa_sf"]] <- round(cqa_sf_all, 2)
      }

      # --- Step A: Interpolate subject residual from user CQA ---
      use_sf <- (input$rca_cqa_type == "cqa_sf" && !is.null(la_col))
      user_cqa <- input$rca_cqa_value

      if (use_sf) {
        comp_cqa_vals  <- export_df[["cqa_sf"]][-1L]
        comp_resid_for_interp <- export_df[["residual_sf"]][-1L]
      } else {
        comp_cqa_vals  <- export_df[["cqa"]][-1L]
        comp_resid_for_interp <- export_df[["residual"]][-1L]
      }

      # Remove NAs, sort by CQA ascending for interpolation
      valid <- !is.na(comp_cqa_vals) & !is.na(comp_resid_for_interp)
      cqa_sorted   <- comp_cqa_vals[valid]
      resid_sorted <- comp_resid_for_interp[valid]
      ord <- order(cqa_sorted)
      cqa_sorted   <- cqa_sorted[ord]
      resid_sorted <- resid_sorted[ord]

      # Linear interpolation
      subject_resid <- stats::approx(cqa_sorted, resid_sorted,
                                     xout = user_cqa, rule = 2)$y

      if (use_sf) {
        # Convert per-SF residual back to total residual
        subject_la <- export_df[[la_col]][1L]
        subject_resid_total <- subject_resid * subject_la
      } else {
        subject_resid_total <- subject_resid
      }

      subject_est <- predicted[1L] + subject_resid_total
      actual[1L] <- subject_est
      residuals_val[1L] <- subject_resid_total
      export_df[["residual"]][1L]           <- round(subject_resid_total, 1)
      export_df[[paste0("est_", tgt)]][1L]  <- round(predicted[1L], 1)
      export_df[["subject_value"]]          <- NA_real_
      export_df[["subject_value"]][1L]      <- round(subject_est, 1)
      export_df[["subject_cqa"]]            <- NA_real_
      export_df[["subject_cqa"]][1L]        <- user_cqa

      if (use_sf && !is.null(la_col)) {
        export_df[["residual_sf"]][1L] <- round(subject_resid, 1)
      }

      # For weight-0 rows (rows 2+): use subject_value (est + subject residual)
      # so the last 4 RCA columns can be computed.
      # Sale price is left as-is; subject_value holds the conclusion.
      zero_wt <- integer(0)
      # Resolve weight column: special type "weight" first, then weights_col
      rca_wt_col <- NULL
      if (!is.null(input$col_specials)) {
        for (nm in names(input$col_specials)) {
          if (input$col_specials[[nm]] == "weight") { rca_wt_col <- nm; break }
        }
      }
      if (is.null(rca_wt_col) && !is.null(input$weights_col) &&
          input$weights_col != "null") {
        rca_wt_col <- input$weights_col
      }
      if (!is.null(rca_wt_col) && rca_wt_col %in% names(export_df)) {
        wvals <- export_df[[rca_wt_col]]
        message("earthUI RCA: weight col class=", class(wvals),
                ", unique vals=", paste(head(sort(unique(wvals)), 10), collapse=","),
                ", n_zero=", sum(wvals == 0, na.rm = TRUE),
                ", n_na=", sum(is.na(wvals)))
        zero_wt <- which(wvals == 0)
      }
      if (length(zero_wt) > 0L) {
        sv <- predicted[zero_wt] + subject_resid_total
        export_df[["subject_value"]][zero_wt] <- round(sv, 1)
        actual[zero_wt] <- sv
        residuals_val <- actual - predicted
        export_df[["residual"]][zero_wt] <- round(residuals_val[zero_wt], 1)
        if (!is.null(la_col)) {
          la <- export_df[[la_col]]
          export_df[["residual_sf"]][zero_wt] <- round(residuals_val[zero_wt] / la[zero_wt], 1)
        }
      }

      # --- Step B: Per-g-function contributions and adjustments ---
      intercept_group <- NULL
      contrib_groups  <- list()
      for (grp in groups) {
        if (grp$degree == 0L) {
          intercept_group <- grp
        } else {
          contrib_groups <- c(contrib_groups, list(grp))
        }
      }

      basis_val <- if (!is.null(intercept_group)) {
        intercept_group$terms[[1]]$coefficient
      } else {
        0
      }
      export_df[["basis"]] <- round(basis_val, 1)

      # Compute contributions for all rows, then adjustments = subject - comp
      adj_sum    <- rep(0, nrow(export_df))
      gross_sum  <- rep(0, nrow(export_df))
      contrib_labels <- character(0)

      for (grp in contrib_groups) {
        col_label <- gsub(" ", "_", grp$label)
        contrib_col <- paste0(col_label, "_contribution")
        adj_col     <- paste0(col_label, "_adjustment")

        contrib <- earthUI:::eval_g_function_(model, grp, pred_df,
                                               response_idx = if (multi) ri else NULL)
        export_df[[contrib_col]] <- round(contrib, 1)

        # Adjustment = subject contribution - comp contribution
        subject_contrib <- contrib[1L]
        adjustment <- subject_contrib - contrib
        export_df[[adj_col]] <- round(adjustment, 1)

        adj_sum   <- adj_sum + adjustment
        gross_sum <- gross_sum + abs(adjustment)
        contrib_labels <- c(contrib_labels, col_label)
      }

      # residual_adjustment = subject residual - comp residual
      resid_adj <- subject_resid_total - residuals_val
      export_df[["residual_adjustment"]] <- round(resid_adj, 1)
      adj_sum   <- adj_sum + resid_adj
      gross_sum <- gross_sum + abs(resid_adj)

      # adjusted_sale_price = sale_price + net_adjustments
      # Use 'actual' which has imputed prices for weight-0 rows
      export_df[["net_adjustments"]]      <- round(adj_sum, 1)
      export_df[["gross_adjustments"]]    <- round(gross_sum, 1)

      # Percentage columns (adjustment / comparable sale price)
      export_df[["residual_pct"]]   <- round(resid_adj / actual, 4)
      export_df[["net_adj_pct"]]    <- round(adj_sum / actual, 4)
      export_df[["gross_adj_pct"]]  <- round(gross_sum / actual, 4)

      export_df[["adjusted_sale_price"]]  <- round(actual + adj_sum, 1)

      # --- Additional targets (e.g., rent) for weight-0 rows only ---
      message("earthUI RCA: multi=", multi, ", length(targets)=", length(targets),
              ", length(zero_wt)=", length(zero_wt),
              ", weights_col=", input$weights_col %||% "NULL")
      if (multi && length(zero_wt) > 0L) {
        for (ri2 in 2:length(targets)) {
          tgt2 <- targets[ri2]
          eq_ri2 <- eq$equations[[ri2]]
          groups2 <- eq_ri2$groups

          # Column name prefix for this target (e.g., "rent")
          tp <- tgt2

          # Predictions for this target (all rows)
          predicted2 <- as.numeric(pred_mat[, ri2])
          export_df[[paste0("est_", tp)]] <- round(predicted2, 1)

          # Residuals for comps with weight > 0 (for CQA interpolation)
          actual2 <- export_df[[tgt2]]
          actual2[1L] <- NA_real_
          resid2 <- actual2 - predicted2

          # CQA on this target (comps with weight > 0 only)
          comp_resid2 <- resid2[-1L]
          comp_resid2_valid <- comp_resid2[!is.na(comp_resid2)]
          n_comps2 <- length(comp_resid2_valid)
          cqa2 <- vapply(resid2, function(r) {
            if (is.na(r)) return(NA_real_)
            sum(comp_resid2_valid < r, na.rm = TRUE) / n_comps2 * 10
          }, numeric(1))

          # Interpolate subject residual for this target using same CQA score
          if (use_sf) {
            resid2_sf <- resid2 / la
            comp_cqa2 <- cqa2[-1L]
            comp_resid2_interp <- resid2_sf[-1L]
          } else {
            comp_cqa2 <- cqa2[-1L]
            comp_resid2_interp <- resid2[-1L]
          }
          valid2 <- !is.na(comp_cqa2) & !is.na(comp_resid2_interp)
          cqa2_sorted <- comp_cqa2[valid2]
          resid2_sorted <- comp_resid2_interp[valid2]
          ord2 <- order(cqa2_sorted)
          cqa2_sorted <- cqa2_sorted[ord2]
          resid2_sorted <- resid2_sorted[ord2]

          subj_resid2 <- stats::approx(cqa2_sorted, resid2_sorted,
                                        xout = user_cqa, rule = 2)$y
          if (use_sf) {
            subj_resid2_total <- subj_resid2 * export_df[[la_col]][1L]
          } else {
            subj_resid2_total <- subj_resid2
          }

          # Subject value for this target
          subj_est2 <- predicted2[1L] + subj_resid2_total
          sv_col <- paste0("subject_", tp, "_value")
          export_df[[sv_col]] <- NA_real_
          export_df[[sv_col]][1L] <- round(subj_est2, 1)

          # Weight-0 rows: impute actual from est + subject residual
          sv2 <- predicted2[zero_wt] + subj_resid2_total
          export_df[[sv_col]][zero_wt] <- round(sv2, 1)
          actual2[1L] <- subj_est2
          actual2[zero_wt] <- sv2
          resid2 <- actual2 - predicted2

          export_df[[paste0(tp, "_residual")]] <- round(resid2, 1)

          # Per-g-function contributions and adjustments for this target
          intercept2 <- NULL
          contrib_groups2 <- list()
          for (grp in groups2) {
            if (grp$degree == 0L) {
              intercept2 <- grp
            } else {
              contrib_groups2 <- c(contrib_groups2, list(grp))
            }
          }

          basis2 <- if (!is.null(intercept2)) intercept2$terms[[1]]$coefficient else 0
          export_df[[paste0(tp, "_basis")]] <- round(basis2, 1)

          adj_sum2 <- rep(0, nrow(export_df))
          gross_sum2 <- rep(0, nrow(export_df))

          for (grp in contrib_groups2) {
            col_label <- gsub(" ", "_", grp$label)
            contrib_col2 <- paste0(tp, "_", col_label, "_contribution")
            adj_col2 <- paste0(tp, "_", col_label, "_adjustment")

            contrib2 <- earthUI:::eval_g_function_(model, grp, pred_df,
                                                     response_idx = ri2)
            export_df[[contrib_col2]] <- round(contrib2, 1)

            subj_contrib2 <- contrib2[1L]
            adj2 <- subj_contrib2 - contrib2
            export_df[[adj_col2]] <- round(adj2, 1)

            adj_sum2 <- adj_sum2 + adj2
            gross_sum2 <- gross_sum2 + abs(adj2)
          }

          resid_adj2 <- subj_resid2_total - resid2
          export_df[[paste0(tp, "_residual_adjustment")]] <- round(resid_adj2, 1)
          adj_sum2 <- adj_sum2 + resid_adj2
          gross_sum2 <- gross_sum2 + abs(resid_adj2)

          export_df[[paste0(tp, "_net_adjustments")]]   <- round(adj_sum2, 1)
          export_df[[paste0(tp, "_gross_adjustments")]]  <- round(gross_sum2, 1)

          # Percentage columns for additional targets
          export_df[[paste0(tp, "_residual_pct")]]  <- round(resid_adj2 / actual2, 4)
          export_df[[paste0(tp, "_net_adj_pct")]]    <- round(adj_sum2 / actual2, 4)
          export_df[[paste0(tp, "_gross_adj_pct")]]  <- round(gross_sum2 / actual2, 4)

          export_df[[paste0("adjusted_", tp)]] <- round(actual2 + adj_sum2, 1)
        }
      }

      writexl::write_xlsx(export_df, file)
      rv$rca_df <- export_df
      rv$rca_targets <- targets
      session$sendCustomMessage("download_check", list(id = "rca_output_btn"))
      eui_log_$end("7. Calculate RCA Adjustments & Download")
      showNotification(paste0("RCA output saved to: ", file),
                       type = "message", duration = 8)
    }, error = function(e) {
      msg <- paste("RCA export error:", conditionMessage(e))
      message(msg)
      showNotification(msg, type = "error", duration = 15)
    })
  })
}
