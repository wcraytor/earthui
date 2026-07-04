#' Data Import Module -- UI
#'
#' Compact file picker for embedding in a sidebar. Lists the CSV/Excel files
#' in the active regProj project's `in/` folder (files are added to the
#' project via Finder/Explorer or the New-Project dialog). A per-import
#' locale selector controls CSV separator and decimal conventions.
#'
#' @param id Shiny module namespace ID.
#' @return A [shiny::tagList].
#' @export
#' @examples
#' if (interactive()) {
#'   ui <- fluidPage(mod_data_ui("data1"))
#'   server <- function(input, output, session) {
#'     mod_data_server("data1", reactive(NULL))
#'   }
#'   shinyApp(ui, server)
#' }
mod_data_ui <- function(id) {
  ns <- NS(id)
  tagList(
    tags$div(style = "display:flex; align-items:center; justify-content:space-between; margin-bottom:4px;",
      tags$label(class = "control-label",
                 "Pick a file from the project's in/"),
      tags$div(style = "display:flex; align-items:center; gap:6px;",
        tags$label(class = "control-label", style = "margin-bottom:0;", "Locale"),
        tags$div(style = "width:150px; margin-bottom:0;",
          selectInput(ns("locale_import"), NULL,
                      choices = locale_country_choices_(),
                      selected = "us", width = "100%")
        )
      )
    ),
    uiOutput(ns("file_picker")),
    actionLink(ns("files_refresh"), "Refresh file list",
               style = "font-size: 0.85em; color: #5e81ac; display: block; margin-top: 4px;"),
    uiOutput(ns("sheet_selector")),
    uiOutput(ns("data_preview_info"))
  )
}


#' Data Import Module -- Server
#'
#' Lists and loads CSV/Excel files from the active regProj project's `<os>_in/`
#' folder. The selected file is remembered per project via the
#' `.regproj-last_<os>` marker and auto-loaded when the project is reopened.
#'
#' @param id Shiny module namespace ID.
#' @param active_project_r A reactive returning the active project (a one-row
#'   data frame with a `project_path` column) or `NULL` when no project is open.
#' @return A list with `data` (reactive data frame), `filename` (reactive
#'   original file basename), and `reset` (function to clear state).
#' @export
mod_data_server <- function(id, active_project_r = reactive(NULL)) {
  moduleServer(id, function(input, output, session) {
    imported      <- reactiveVal(NULL)
    orig_filename <- reactiveVal(NULL)
    file_path     <- reactiveVal(NULL)
    sheets        <- reactiveVal(NULL)
    loaded_key    <- reactiveVal(NULL)
    refresh_token <- reactiveVal(0L)

    # --- Restore saved import locale on startup ---
    locale_defaults <- settings_db_read_locale_mgcv()
    if (!is.null(locale_defaults) && length(locale_defaults) > 0L) {
      ld <- locale_defaults
      if (!is.null(ld$locale_import))
        updateSelectInput(session, "locale_import", selected = ld$locale_import)
      else if (!is.null(ld$locale_country))
        updateSelectInput(session, "locale_import", selected = ld$locale_country)
    }

    import_preset_ <- function() {
      import_country <- input$locale_import %||% "us"
      presets <- locale_country_presets_()
      presets[[import_country]] %||% presets[["us"]]
    }

    # --- Files in the active project's in/ folder ---
    project_in_files_ <- reactive({
      refresh_token()                     # bump to force a re-walk
      p <- active_project_r()
      if (is.null(p)) return(character(0))
      regproj_in_files(p$project_path)
    })

    observeEvent(input$files_refresh, {
      refresh_token(refresh_token() + 1L)
    })

    # When the active project changes, clear per-file state so the picker
    # re-renders and (re)loads the new project's last-used file.
    observeEvent(active_project_r(), ignoreNULL = FALSE, {
      imported(NULL)
      orig_filename(NULL)
      file_path(NULL)
      sheets(NULL)
      loaded_key(NULL)
    })

    output$file_picker <- renderUI({
      files <- project_in_files_()
      p <- active_project_r()
      ns <- session$ns
      if (is.null(p)) {
        return(tags$div(class = "small text-muted",
                        "(open a project first)"))
      }
      last <- regproj_last_file_get(p$project_path)
      if (length(files) == 0L) {
        return(tags$div(class = "small text-muted",
          "(no files in this project's in/ folder yet \u2014 drop CSV/Excel files into ",
          tags$code(file.path(p$project_path, paste0(os_detect(), "_in"))),
          " and click Refresh)"))
      }
      sel <- if (!is.null(last) && last %in% files) last else files[[1L]]
      selectInput(ns("file_pick"), NULL,
                  choices = files, selected = sel, width = "100%")
    })

    # Loading the selected file. Keyed on (project || file) rather than
    # observeEvent(input$file_pick) so switching to a different project whose
    # in/ folder holds a file of the SAME basename still re-imports.
    observe({
      f <- input$file_pick
      p <- active_project_r()
      files <- project_in_files_()
      if (is.null(p) || is.null(f) || !nzchar(f)) return()
      if (!(f %in% files)) return()       # stale picker value mid-switch
      in_dir <- file.path(p$project_path, paste0(os_detect(), "_in"))
      full <- file.path(in_dir, f)
      if (!file.exists(full)) {
        showNotification(sprintf("File not found: %s", full),
                         type = "error", duration = 6); return()
      }
      # Fold mtime into the key so editing the file IN PLACE (same project,
      # same name) changes the key and forces a re-import. "Refresh file list"
      # re-fires this observe (via project_in_files_), which re-reads mtime.
      mt <- tryCatch(as.numeric(file.mtime(full)), error = function(e) NA_real_)
      key <- paste(p$project_path, f, mt, sep = "||")
      if (identical(isolate(loaded_key()), key)) return()
      loaded_key(key)
      regproj_last_file_set(p$project_path, f)
      ext <- tolower(tools::file_ext(f))
      preset <- import_preset_()
      tryCatch({
        df <- import_data_mgcv(full, sheet = 1L,
                          sep = preset$csv_sep, dec = preset$csv_dec)
        imported(df)
        orig_filename(f)
        file_path(full)
        sheets(if (ext %in% c("xlsx", "xls")) readxl::excel_sheets(full) else NULL)
        showNotification(
          paste("Loaded", nrow(df), "rows,", ncol(df), "columns"),
          type = "message")
      }, error = function(e) {
        showNotification(paste("Import error:", e$message),
                         type = "error", duration = 15)
        imported(NULL)
        orig_filename(NULL)
      })
    })

    output$sheet_selector <- renderUI({
      req(sheets())
      ns <- session$ns
      selectInput(ns("sheet"), "Sheet", choices = sheets(),
                  selected = sheets()[1])
    })

    observeEvent(input$sheet, {
      req(file_path(), input$sheet)
      preset <- import_preset_()
      tryCatch({
        imported(import_data_mgcv(file_path(), sheet = input$sheet,
                             sep = preset$csv_sep, dec = preset$csv_dec))
      }, error = function(e) {
        showNotification(paste("Import error:", e$message),
                         type = "error", duration = 15)
      })
    })

    output$data_preview_info <- renderUI({
      df <- imported()
      if (is.null(df)) return(NULL)
      tags$div(
        class = "alert alert-info",
        style = "font-size: 0.85em; padding: 8px;",
        sprintf("%d rows, %d columns", nrow(df), ncol(df))
      )
    })

    list(
      data     = reactive(imported()),
      filename = reactive(orig_filename()),
      reset    = function() {
        imported(NULL)
        orig_filename(NULL)
        file_path(NULL)
        sheets(NULL)
        loaded_key(NULL)
      }
    )
  })
}
