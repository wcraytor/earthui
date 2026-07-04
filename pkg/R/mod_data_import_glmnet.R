#' Data Import Module UI
#'
#' UI component for the data import module. Provides file upload
#' and variable role assignment controls with per-variable
#' include checkboxes following the earthUI pattern.
#'
#' @param id Module namespace ID.
#'
#' @return A Shiny \code{tagList} containing the module UI elements.
#'
#' @export
#' @examples
#' if (interactive()) {
#'   ui <- shiny::fluidPage(dataImportUI("data"))
#' }
dataImportUI <- function(id) {

  ns <- shiny::NS(id)

  shiny::tagList(
    shiny::tags$div(
      class = "glmnet-locale-row",
      style = "display:flex; align-items:center; justify-content:space-between; margin-bottom:4px;",
      shiny::tags$label(class = "control-label",
                        "Pick a file from the project's in/"),
      shiny::tags$div(
        style = "display:flex; align-items:center; gap:6px;",
        shiny::tags$label(class = "control-label",
                          style = "margin-bottom:0;", "Locale"),
        shiny::tags$div(
          style = "width:150px; margin-bottom:0;",
          shiny::selectInput(ns("locale_import"), NULL,
                             choices = locale_country_choices_(),
                             selected = "us", width = "100%")
        )
      )
    ),
    shiny::uiOutput(ns("file_picker")),
    shiny::tags$div(
      style = "display:flex; gap:12px; align-items:center; margin-top:4px;",
      shinyFiles::shinyFilesButton(
        ns("file_browse"), "Browse\u2026",
        title = "Select a CSV or Excel file to import into this project",
        multiple = FALSE,
        class = "btn btn-outline-secondary btn-sm"),
      shiny::actionLink(ns("files_refresh"), "Refresh file list",
                        style = "font-size: 0.85em; color: #5e81ac;")
    ),
    shiny::uiOutput(ns("in_folder_caption")),
    shiny::uiOutput(ns("sheet_selector"))
  )
}

#' Variable Configuration Module UI
#'
#' UI component for variable configuration: response, weight, and
#' predictor settings table. Uses the same module namespace as
#' [dataImportUI()] / [dataImportServer()].
#'
#' @param id Module namespace ID (must match the ID used in
#'   \code{dataImportServer}).
#'
#' @return A Shiny \code{tagList} containing the variable configuration UI.
#'
#' @export
#' @examples
#' if (interactive()) {
#'   ui <- shiny::fluidPage(variableConfigUI("data"))
#' }
variableConfigUI <- function(id) {

  ns <- shiny::NS(id)

  help_icon <- function(text) {
    shiny::tags$span(
      class = "glmnet-help-icon",
      `data-bs-toggle` = "popover",
      `data-bs-trigger` = "hover focus",
      `data-bs-content` = text,
      `data-bs-placement` = "left",
      "?"
    )
  }

  label_with_help <- function(label, help_text) {
    shiny::tags$span(label, help_icon(help_text))
  }

  shiny::tagList(
    shiny::selectInput(
      ns("response"),
      label_with_help(
        "Response (Target) Variable",
        paste(
          "The variable you want to predict.",
          "Must be numeric for gaussian family.",
          "All other variables can be selected as predictors."
        )
      ),
      choices = NULL
    ),
    shiny::uiOutput(ns("response_type")),
    shiny::tags$h5("Predictor Settings"),
    shiny::helpText(
      style = "font-size:0.85em;",
      "Type = column data type. ",
      shiny::tags$b("Inc"), " = include as predictor. ",
      shiny::tags$b("Factor"), " = treat as categorical. ",
      shiny::tags$b("Force"), " = guarantee variable stays in the model. ",
      shiny::tags$b("Sign"), " = expected coefficient direction."
    ),
    shiny::tags$div(
      class = "glmnet-var-table",
      style = paste0("max-height: 400px; overflow-y: auto; ",
                     "border: 1px solid #ddd; border-radius: 4px;"),
      shiny::uiOutput(ns("variable_table"))
    ),
    shiny::actionButton(ns("save_varconfig"),
                        "Save current settings as default",
                        class = "btn-outline-primary btn-sm",
                        style = "width:100%; margin-top:8px;")
  )
}

#' Data Import Module - Data Preview UI
#'
#' UI component for the data preview table, intended for the main panel.
#'
#' @param id Module namespace ID (must match the ID used in
#'   \code{dataImportServer}).
#'
#' @return A Shiny \code{tagList} containing the data preview table.
#'
#' @export
#' @examples
#' if (interactive()) {
#'   ui <- shiny::fluidPage(dataPreviewUI("data"))
#' }
dataPreviewUI <- function(id) {
  ns <- shiny::NS(id)
  shiny::tagList(
    # Keep every preview row a single line tall: no wrapping, cap each
    # column's width and clip overflow with an ellipsis. Full text stays
    # available via the hover tooltip and double-click popup.
    shiny::tags$style(shiny::HTML(paste(
      ".glmnet-preview table.dataTable td,",
      ".glmnet-preview table.dataTable th {",
      "  white-space: nowrap;",
      "  max-width: 40ch;",
      "  overflow: hidden;",
      "  text-overflow: ellipsis;",
      "}"))),
    shiny::div(
      class = "glmnet-preview",
      shiny::conditionalPanel(
        condition = "input.purpose === 'appraisal' || input.purpose === 'market'",
        shiny::h5("Subject Property"),
        DT::DTOutput(ns("preview_subjects")),
        shiny::h5("Comparable Sales"),
        DT::DTOutput(ns("preview_comps"))
      ),
      shiny::conditionalPanel(
        condition = "input.purpose === 'general'",
        DT::DTOutput(ns("preview_table"))
      )
    )
  )
}

#' Data Import Module Server
#'
#' Server logic for the data import module. Handles file reading,
#' snake_case column name conversion, column type detection, variable
#' role management via per-variable include checkboxes, and settings
#' persistence via localStorage keyed by input filename.
#'
#' @param id Module namespace ID.
#' @param purpose Reactive returning the current purpose mode
#'   (\code{"general"}, \code{"appraisal"}, or \code{"market"}).
#' @param active_project_r Reactive returning the active regProj project (a
#'   one-row data frame with a \code{project_path} column) or \code{NULL} when
#'   no project is open. Files are listed/loaded from the project's
#'   \code{<os>_in/} folder.
#' @param earth_import_r Reactive returning the imported earth model (a
#'   \code{glmnetUI_earth_import}) or \code{NULL}. When non-NULL, the predictor
#'   set is dictated by the earth model, so the variable table locks Include
#'   and Type, disables rows for predictors earth did not use, and keeps Force
#'   and Sign editable for earth's predictors.
#'
#' @return A reactive list containing:
#' \describe{
#'   \item{data}{The imported data frame.}
#'   \item{response}{Selected response variable name.}
#'   \item{predictors}{Selected predictor variable names.}
#'   \item{expected_signs}{Named vector of expected signs per predictor.}
#'   \item{valid}{Logical, whether selections are valid for modeling.}
#'   \item{col_specials}{Named character vector of special column roles.}
#' }
#'
#' @export
#' @examples
#' if (interactive()) {
#'   server <- function(input, output, session) {
#'     data_out <- dataImportServer("data",
#'                                  shiny::reactiveVal("general"))
#'   }
#' }
dataImportServer <- function(id, purpose = shiny::reactiveVal("general"),
                             active_project_r = shiny::reactiveVal(NULL),
                             earth_import_r = shiny::reactive(NULL)) {
  shiny::moduleServer(id, function(input, output, session) {
    ns <- session$ns

    rv <- shiny::reactiveValues(
      data = NULL,
      col_types = NULL,
      sheets = NULL,
      file_name = NULL
    )
    file_path     <- shiny::reactiveVal(NULL)   # full path of the loaded file
    loaded_key    <- shiny::reactiveVal(NULL)   # "<project>||<file>" guard
    refresh_token <- shiny::reactiveVal(0L)

    # Parse + populate helper (reads from the project's in/ folder).
    load_file_ <- function(path, name, sheet = 1L) {
      ext <- tolower(tools::file_ext(name))
      if (ext == "csv") {
        rv$data <- utils::read.csv(
          path, stringsAsFactors = FALSE,
          check.names = FALSE,
          sep = locale_csv_sep_(), dec = locale_csv_dec_()
        )
        rv$sheets <- NULL
      } else if (ext %in% c("xls", "xlsx")) {
        rv$sheets <- readxl::excel_sheets(path)
        sh <- if (is.character(sheet)) sheet else rv$sheets[sheet]
        rv$data <- as.data.frame(
          readxl::read_excel(path, sheet = sh)
        )
      } else {
        stop("Unsupported file type.")
      }

      rv$file_name <- name
      names(rv$data) <- to_snake_case(names(rv$data))
      rv$data <- auto_parse_dates_(rv$data)
      rv$col_types <- detect_column_types_glmnet(rv$data)
      all_cols <- names(rv$data)

      default_response <- if (length(all_cols) > 0) all_cols[1] else NULL
      # earthUI is the source of truth for the Response (its §3 target). If it
      # saved a target that exists in this data, use it instead of column 1.
      p_cf <- tryCatch(active_project_r(), error = function(e) NULL)
      cf <- earth_carryforward_(
        if (is.null(p_cf)) NULL else p_cf$project_path, purpose())
      if (!is.null(cf$response) && cf$response %in% all_cols)
        default_response <- cf$response
      shiny::updateSelectInput(session, "response",
                               choices = all_cols,
                               selected = default_response)
      rv$data
    }

    # --- Files in the active project's in/ folder ---
    project_in_files_ <- shiny::reactive({
      refresh_token()                     # bump to force a re-walk
      p <- active_project_r()
      if (is.null(p)) return(character(0))
      regproj_in_files(p$project_path)
    })

    shiny::observeEvent(input$files_refresh, {
      refresh_token(refresh_token() + 1L)
    })

    # --- Browse to any folder, copy the chosen file into the project's in/
    #     folder, and load it. Lets users import from anywhere while keeping
    #     the file tied to the project. ---
    volumes <- c(Home = path.expand("~"), shinyFiles::getVolumes()())
    if (.Platform$OS.type == "unix" && dir.exists("/Volumes"))
      volumes <- c(volumes, Volumes = "/Volumes")
    if (.Platform$OS.type == "unix" && dir.exists("/media"))
      volumes <- c(volumes, Media = "/media")
    if (.Platform$OS.type == "unix" && dir.exists("/mnt"))
      volumes <- c(volumes, Mounts = "/mnt")
    shinyFiles::shinyFileChoose(input, "file_browse", roots = volumes,
                                session = session,
                                filetypes = c("csv", "xls", "xlsx"))

    shiny::observeEvent(input$file_browse, {
      p <- active_project_r()
      if (is.null(p)) {
        shiny::showNotification("Open a project first, then browse for a file.",
                                type = "warning", duration = 5)
        return()
      }
      fi <- shinyFiles::parseFilePaths(volumes, input$file_browse)
      if (nrow(fi) == 0L) return()
      src <- as.character(fi$datapath[1L])
      if (!nzchar(src) || !file.exists(src)) return()
      nm <- basename(src)
      in_dir <- file.path(p$project_path, paste0(os_detect(), "_in"))
      if (!dir.exists(in_dir))
        dir.create(in_dir, recursive = TRUE, showWarnings = FALSE)
      dest <- file.path(in_dir, nm)
      ok <- tryCatch({
        if (normalizePath(src, mustWork = FALSE) !=
            normalizePath(dest, mustWork = FALSE)) {
          file.copy(src, dest, overwrite = TRUE)
        }
        TRUE
      }, error = function(e) {
        shiny::showNotification(paste("Copy failed:", e$message),
                                type = "error", duration = 8); FALSE
      })
      if (!ok) return()
      regproj_last_file_set(p$project_path, nm)
      refresh_token(refresh_token() + 1L)   # re-walk so the dropdown lists it
      file_path(dest)
      loaded_key(paste(p$project_path, nm, sep = "||"))
      tryCatch({
        load_file_(dest, nm, sheet = 1L)
        shiny::showNotification(
          paste0("Imported '", nm, "' (", nrow(rv$data), " rows) into ", in_dir),
          type = "message", duration = 6)
      }, error = function(e) {
        shiny::showNotification(paste("Import error:", e$message),
                                type = "error", duration = 15)
      })
    })

    # Show the project's import folder so its location is never hidden.
    output$in_folder_caption <- shiny::renderUI({
      p <- active_project_r()
      if (is.null(p)) return(NULL)
      in_dir <- file.path(p$project_path, paste0(os_detect(), "_in"))
      shiny::tags$div(
        class = "small text-muted",
        style = "margin-top:4px; word-break:break-all;",
        "Import folder: ", shiny::tags$code(in_dir))
    })

    # When the active project changes, clear per-file state so the picker
    # re-renders and (re)loads the new project's last-used file.
    shiny::observeEvent(active_project_r(), ignoreNULL = FALSE, {
      rv$data <- NULL
      rv$file_name <- NULL
      rv$col_types <- NULL
      rv$sheets <- NULL
      file_path(NULL)
      loaded_key(NULL)
    })

    output$file_picker <- shiny::renderUI({
      files <- project_in_files_()
      p <- active_project_r()
      ns <- session$ns
      if (is.null(p)) {
        return(shiny::tags$div(class = "small text-muted",
                               "(open a project first)"))
      }
      last <- regproj_last_file_get(p$project_path)
      if (length(files) == 0L) {
        return(shiny::tags$div(class = "small text-muted",
          "(no files in this project's in/ folder yet \u2014 drop CSV/Excel files into ",
          shiny::tags$code(file.path(p$project_path, paste0(os_detect(), "_in"))),
          " and click Refresh)"))
      }
      sel <- if (!is.null(last) && last %in% files) last else files[[1L]]
      shiny::selectInput(ns("file_pick"), NULL,
                         choices = files, selected = sel, width = "100%")
    })

    # Load the selected file. Keyed on (project || file) so switching to a
    # different project whose in/ holds a same-named file still re-imports.
    shiny::observe({
      f <- input$file_pick
      p <- active_project_r()
      files <- project_in_files_()
      if (is.null(p) || is.null(f) || !nzchar(f)) return()
      if (!(f %in% files)) return()       # stale picker value mid-switch
      in_dir <- file.path(p$project_path, paste0(os_detect(), "_in"))
      full <- file.path(in_dir, f)
      if (!file.exists(full)) {
        shiny::showNotification(sprintf("File not found: %s", full),
                                type = "error", duration = 6); return()
      }
      # Fold mtime into the key so editing the file IN PLACE (same project,
      # same name) changes the key and forces a re-import. "Refresh file list"
      # re-fires this observe (via project_in_files_), which re-reads mtime.
      mt <- tryCatch(as.numeric(file.mtime(full)), error = function(e) NA_real_)
      key <- paste(p$project_path, f, mt, sep = "||")
      if (identical(shiny::isolate(loaded_key()), key)) return()
      loaded_key(key)
      regproj_last_file_set(p$project_path, f)
      file_path(full)
      tryCatch({
        load_file_(full, f, sheet = 1L)
        shiny::showNotification(
          paste("Loaded", nrow(rv$data), "rows,", ncol(rv$data), "columns"),
          type = "message")
      }, error = function(e) {
        shiny::showNotification(paste("Import error:", e$message),
                                type = "error", duration = 15)
      })
    })

    output$sheet_selector <- shiny::renderUI({
      shiny::req(rv$sheets)
      ns <- session$ns
      shiny::selectInput(ns("sheet"), "Excel Sheet", choices = rv$sheets,
                         selected = rv$sheets[1])
    })

    shiny::observeEvent(input$sheet, {
      shiny::req(file_path(), input$sheet)
      ext <- tolower(tools::file_ext(file_path()))
      if (!ext %in% c("xls", "xlsx")) return()
      tryCatch({
        load_file_(file_path(), rv$file_name, sheet = input$sheet)
      }, error = function(e) {
        shiny::showNotification(paste("Sheet error:", e$message),
                                type = "error", duration = 15)
      })
    })

    # --- Section 4: "Save current settings as default" (variable config only) ---
    shiny::observeEvent(input$save_varconfig, {
      shiny::req(rv$file_name)
      session$sendCustomMessage("collect_and_save_varconfig",
                                list(filename = rv$file_name))
      shiny::showNotification("Variable configuration saved as default.",
                              type = "message", duration = 3)
    })

    # --- Candidate columns: everything except response ---
    candidates <- shiny::reactive({
      shiny::req(rv$data, input$response)
      setdiff(names(rv$data), input$response)
    })

    # --- Response type indicator ---
    output$response_type <- shiny::renderUI({
      shiny::req(rv$data, input$response, rv$col_types)
      resp <- input$response
      rtype <- rv$col_types[resp]
      is_ok <- rtype %in% c("numeric", "integer")
      color <- if (is_ok) "var(--bs-success)" else "var(--bs-danger)"
      msg <- if (is_ok) {
        paste0("Type: ", rtype)
      } else {
        paste0("Type: ", rtype, " \u2014 must be numeric for modeling")
      }
      shiny::tags$div(
        style = paste0("font-size:12px; color:", color,
                       "; margin-top:-8px; margin-bottom:8px;"),
        msg
      )
    })

    # --- Variable table with Inc? checkboxes, type, expected sign, NAs ---
    output$variable_table <- shiny::renderUI({
      cols <- candidates()
      if (length(cols) == 0) return(shiny::helpText("No candidate predictors."))

      types <- rv$col_types
      df <- rv$data
      file_key <- rv$file_name

      # Pre-check the Factor box only for unambiguous (non-numeric) categoricals
      # (text/factor/logical). Numeric factors are the appraiser's call. The
      # user can still uncheck; saved per-file state takes precedence (restored
      # by JS after render).
      fac_auto <- detect_categoricals_(df)

      # When an earthUI model is imported, the predictor set is dictated by the
      # earth model: glmnet fits on earth's basis expansion (mod_modeling.R),
      # not the Include checkboxes. We reflect that here by locking Include and
      # Type/Factor (earth-controlled) and disabling rows for predictors earth
      # did not use. Force-in and Sign stay editable for earth's predictors,
      # since those still map onto the earth basis at fit time.
      ek <- tryCatch(earth_import_r(), error = function(e) NULL)
      earth_active <- !is.null(ek)
      earth_preds <- if (earth_active) intersect(ek$predictors, cols)
                     else character(0)

      # CSS for flexbox rows (colors handled via classes for dark mode)
      row_css <- "display:flex; align-items:center; padding:2px 0; border-bottom:1px solid #eee; gap:2px;"
      hdr_css <- "display:flex; align-items:center; padding:4px 0; border-bottom:2px solid #ccc; font-weight:bold; font-size:0.85em; gap:2px;"

      all_types <- c("numeric", "integer", "character",
                     "Date", "POSIXct")

      appraiser <- purpose() %in% c("appraisal", "market")
      # Keep this list identical to mgcvUI / earthUI's special-column options so
      # the three sibling apps share the same designations.
      special_options <- special_roles_(appraiser)

      angled_hdr <- "text-align:center; font-size:1.0em; writing-mode:vertical-lr; transform:rotate(180deg); height:60px; line-height:1;"
      hdr_cols <- list(
        shiny::tags$div(style = "flex:1; min-width:60px;", "Variable"),
        shiny::tags$div(style = "width:62px; text-align:center;", "Type"),
        shiny::tags$div(style = paste0("width:20px;", angled_hdr), "Include"),
        shiny::tags$div(style = paste0("width:20px;", angled_hdr), "Factor"),
        shiny::tags$div(style = paste0("width:20px;", angled_hdr), "Force")
      )
      if (appraiser) {
        hdr_cols <- c(hdr_cols, list(
          shiny::tags$div(style = "width:72px; text-align:center;", "Special")
        ))
      }
      hdr_cols <- c(hdr_cols, list(
        shiny::tags$div(style = "width:52px; text-align:center;", "Sign"),
        shiny::tags$div(style = "width:28px; text-align:right; padding-right:2px;", "NAs")
      ))
      header <- shiny::tags$div(
        class = "glmnet-var-hdr",
        style = hdr_css,
        hdr_cols
      )

      rows <- lapply(seq_along(cols), function(i) {
        col_name <- cols[i]
        col_type <- types[col_name]
        na_count <- sum(is.na(df[[col_name]]))
        na_style <- if (na_count > nrow(df) * 0.3) "color:red;" else ""

        # Earth lock state for this row. `lock_attr` (NA -> the boolean HTML
        # attribute, NULL -> omitted) disables earth-controlled inputs;
        # `fs_lock` disables Force/Sign/Special only for predictors earth did
        # not select; `inc_checked` forces earth's predictors checked.
        in_earth     <- earth_active && (col_name %in% earth_preds)
        lock_attr    <- if (earth_active) NA else NULL
        fs_lock      <- if (earth_active && !in_earth) NA else NULL
        inc_checked  <- if (in_earth) NA else NULL
        row_dim      <- if (earth_active && !in_earth) "opacity:0.5;" else ""

        type_options <- lapply(all_types, function(tp) {
          if (tp == col_type) {
            shiny::tags$option(value = tp, selected = "selected", tp)
          } else {
            shiny::tags$option(value = tp, tp)
          }
        })
        type_el <- shiny::tags$select(
          id = ns(paste0("type_", col_name)),
          class = "form-control glmnet-type-sel",
          style = "width:58px; padding:1px 2px; font-size:0.72em; border:1px solid var(--bs-border-color, #4c566a); border-radius:3px; background:var(--bs-body-bg, #fff); color:var(--bs-body-color, #333);",
          `data-col` = col_name,
          disabled = lock_attr,
          type_options
        )

        sign_el <- shiny::tags$select(
          id = ns(paste0("sign_", col_name)),
          class = "form-control glmnet-sign-sel",
          style = "width:52px; padding:1px 2px; font-size:0.72em; border:1px solid var(--bs-border-color, #4c566a); border-radius:3px; background:var(--bs-body-bg, #fff); color:var(--bs-body-color, #333);",
          `data-col` = col_name,
          disabled = fs_lock,
          shiny::tags$option(value = "either", "either"),
          shiny::tags$option(value = "positive", "positive"),
          shiny::tags$option(value = "negative", "negative")
        )

        # Pre-select the Special tag most similar to the column name (or "no"
        # if none is a reasonable match). Saved values are restored by JS
        # afterwards and take precedence.
        def_special <- special_default_for_(col_name, special_options)
        special_opts <- lapply(special_options, function(sp) {
          if (identical(sp, def_special))
            shiny::tags$option(value = sp, sp, selected = NA)
          else
            shiny::tags$option(value = sp, sp)
        })
        special_el <- shiny::tags$div(
          style = "width:72px; text-align:center;",
          shiny::tags$select(
            id = ns(paste0("special_", col_name)),
            class = "form-control glmnet-special-sel",
            style = "width:68px; padding:1px 2px; font-size:0.75em; border:1px solid var(--bs-border-color, #4c566a); border-radius:3px; background:var(--bs-body-bg, #fff); color:var(--bs-body-color, #333);",
            `data-col` = col_name,
            disabled = fs_lock,
            special_opts
          )
        )

        row_cells <- list(
          shiny::tags$div(
            style = "flex:1; min-width:60px; font-size:0.82em; overflow:hidden; text-overflow:ellipsis; white-space:nowrap;",
            title = col_name, col_name
          ),
          shiny::tags$div(
            style = "width:62px; text-align:center;",
            type_el
          ),
          shiny::tags$div(
            style = "width:20px; text-align:center;",
            shiny::tags$input(
              type = "checkbox",
              id = ns(paste0("inc_", col_name)),
              class = "glmnet-var-cb",
              `data-col` = col_name,
              checked = inc_checked,
              disabled = lock_attr
            )
          ),
          shiny::tags$div(
            style = "width:20px; text-align:center;",
            do.call(shiny::tags$input, c(
              list(
                type = "checkbox",
                id = ns(paste0("fac_", col_name)),
                class = "glmnet-fac-cb",
                `data-col` = col_name,
                disabled = lock_attr
              ),
              # With an earth import active, show the EARTH model's factor
              # treatment: checked only for the earth model's own categorical
              # predictors. A variable earth did not use plays no role in the
              # fit -- a checked-but-locked box would falsely imply it did.
              # Without an import, pre-check unambiguous (non-numeric)
              # categoricals; numeric columns are factors only if the
              # appraiser ticks this box.
              if (if (earth_active) in_earth && (col_name %in% ek$categoricals)
                  else isTRUE(fac_auto[[col_name]]))
                list(checked = NA)
            ))
          ),
          shiny::tags$div(
            style = "width:20px; text-align:center;",
            shiny::tags$input(
              type = "checkbox",
              id = ns(paste0("force_", col_name)),
              class = "glmnet-force-cb",
              `data-col` = col_name,
              disabled = fs_lock
            )
          )
        )
        if (appraiser) {
          row_cells <- c(row_cells, list(special_el))
        }
        row_cells <- c(row_cells, list(
          shiny::tags$div(
            style = "width:52px; text-align:center;",
            sign_el
          ),
          shiny::tags$div(
            style = paste0("width:28px; text-align:right;",
                           " font-size:0.8em; padding-right:2px;", na_style),
            if (na_count > 0L) as.character(na_count) else ""
          )
        ))

        shiny::tags$div(
          class = "glmnet-var-row",
          style = paste0(row_css, row_dim),
          row_cells
        )
      })

      # JavaScript: sync state, save/restore from localStorage by filename
      col_names_json <- jsonlite::toJSON(cols, auto_unbox = FALSE)
      file_key_json <- jsonlite::toJSON(file_key, auto_unbox = TRUE)
      ns_prefix <- ns("")
      js_code <- paste0('
        (function() {
          var cols = ', col_names_json, ';
          var nsPrefix = "', ns_prefix, '";
          var fileKey = ', file_key_json, ';
          function getVarStorageKey() {
            var p = document.querySelector("input[name=purpose]:checked");
            return "glmnetUI_vars_" + fileKey + "_" + (p ? p.value : "general");
          }

          function gatherState() {
            var preds = [];
            var types = {};
            var forceVars = [];
            var specials = {};
            var facVars = [];
            for (var i = 0; i < cols.length; i++) {
              var cb = document.getElementById(nsPrefix + "inc_" + cols[i]);
              var tp = document.getElementById(nsPrefix + "type_" + cols[i]);
              var fcb = document.getElementById(nsPrefix + "force_" + cols[i]);
              var sp = document.getElementById(nsPrefix + "special_" + cols[i]);
              var fac = document.getElementById(nsPrefix + "fac_" + cols[i]);
              if (cb && cb.checked) {
                preds.push(cols[i]);
              }
              if (tp) {
                types[cols[i]] = tp.value;
              }
              if (fcb && fcb.checked) forceVars.push(cols[i]);
              if (fac && fac.checked) facVars.push(cols[i]);
              if (sp) specials[cols[i]] = sp.value;
            }
            Shiny.setInputValue(nsPrefix + "predictors", preds,
                                {priority: "event"});
            Shiny.setInputValue(nsPrefix + "col_types_override", types,
                                {priority: "event"});
            Shiny.setInputValue(nsPrefix + "force_vars", forceVars,
                                {priority: "event"});
            Shiny.setInputValue(nsPrefix + "fac_vars", facVars,
                                {priority: "event"});
            Shiny.setInputValue(nsPrefix + "col_specials", specials,
                                {priority: "event"});
          }

          function saveState() {
            var state = {};
            for (var i = 0; i < cols.length; i++) {
              var cb = document.getElementById(nsPrefix + "inc_" + cols[i]);
              var sel = document.getElementById(nsPrefix + "sign_" + cols[i]);
              var tp = document.getElementById(nsPrefix + "type_" + cols[i]);
              var fcb = document.getElementById(nsPrefix + "force_" + cols[i]);
              var sp = document.getElementById(nsPrefix + "special_" + cols[i]);
              var fac = document.getElementById(nsPrefix + "fac_" + cols[i]);
              state[cols[i]] = {
                inc: cb ? cb.checked : false,
                fac: fac ? fac.checked : false,
                sign: sel ? sel.value : "either",
                type: tp ? tp.value : "character",
                force: fcb ? fcb.checked : false,
                special: sp ? sp.value : "no"
              };
            }
            try {
              var sk = getVarStorageKey();
              console.log("[glmnetUI vars] saveState key=" + sk);
              localStorage.setItem(sk, JSON.stringify(state));
            } catch(e) {}
          }

          function restoreState() {
            var saved = null;
            var usedKey = getVarStorageKey();
            try {
              var raw = localStorage.getItem(usedKey);
              if (!raw) {
                usedKey = "glmnetUI_vars_" + fileKey;
                raw = localStorage.getItem(usedKey);
              }
              if (raw) saved = JSON.parse(raw);
            } catch(e) {}
            console.log("[glmnetUI vars] restoreState key=" + usedKey +
                        " found=" + (saved !== null));

            for (var i = 0; i < cols.length; i++) {
              var cb = document.getElementById(nsPrefix + "inc_" + cols[i]);
              var sel = document.getElementById(nsPrefix + "sign_" + cols[i]);
              var tp = document.getElementById(nsPrefix + "type_" + cols[i]);
              var fcb = document.getElementById(nsPrefix + "force_" + cols[i]);
              var fac = document.getElementById(nsPrefix + "fac_" + cols[i]);
              if (saved && saved[cols[i]]) {
                // Skip any control the server locked (disabled) for the earth
                // import, so saved settings never override the earth lock.
                if (cb && !cb.disabled) cb.checked = saved[cols[i]].inc;
                if (fac && !fac.disabled) fac.checked = !!saved[cols[i]].fac;
                if (sel && !sel.disabled) sel.value = saved[cols[i]].sign || "either";
                if (tp && !tp.disabled && saved[cols[i]].type) tp.value = saved[cols[i]].type;
                if (fcb && !fcb.disabled && saved[cols[i]].force) fcb.checked = saved[cols[i]].force;
                var sp = document.getElementById(nsPrefix + "special_" + cols[i]);
                if (sp && !sp.disabled && saved[cols[i]].special) sp.value = saved[cols[i]].special;
              }
              // Sync sign selects to Shiny
              if (sel) {
                Shiny.setInputValue(nsPrefix + "sign_" + cols[i], sel.value);
              }
            }
          }

          // Event handlers
          $(document).off("change.glmnetvar").on("change.glmnetvar",
            ".glmnet-var-cb", function() {
              gatherState();
              saveState();
            });

          $(document).off("change.glmnetsign").on("change.glmnetsign",
            ".glmnet-sign-sel", function() {
              var col = $(this).data("col");
              Shiny.setInputValue(nsPrefix + "sign_" + col, this.value);
              saveState();
            });

          $(document).off("change.glmnettype").on("change.glmnettype",
            ".glmnet-type-sel", function() {
              gatherState();
              saveState();
            });

          $(document).off("change.glmnetforce").on("change.glmnetforce",
            ".glmnet-force-cb", function() {
              gatherState();
              saveState();
            });

          $(document).off("change.glmnetfac").on("change.glmnetfac",
            ".glmnet-fac-cb", function() {
              gatherState();
              saveState();
            });

          $(document).off("change.glmnetspecial").on("change.glmnetspecial",
            ".glmnet-special-sel", function() {
              // Special is a column role only (area, contract_date, ...). It
              // does NOT imply factor — factor designation is the Factor box.
              gatherState();
              saveState();
            });

          // --- Response variable save/restore (selectize API) ---
          var respKey = "glmnetUI_response_" + fileKey;
          var respId = nsPrefix + "response";

          // Save response on change via selectize
          var respAttempts = 0;
          function getSelectize() {
            var $el = $("#" + CSS.escape(respId));
            if ($el.length && $el[0].selectize) return $el[0].selectize;
            return null;
          }

          function bindRespSave() {
            var sz = getSelectize();
            if (sz) {
              sz.on("change", function(value) {
                try {
                  localStorage.setItem(respKey, value);
                } catch(e) {}
              });
              return true;
            }
            return false;
          }

          // Restore response with polling (earthUI pattern)
          function tryRestoreResponse() {
            var sz = getSelectize();
            if (sz && sz.isSetup && Object.keys(sz.options).length > 0) {
              bindRespSave();
              var savedResp = null;
              try { savedResp = localStorage.getItem(respKey); } catch(e) {}
              if (savedResp && sz.options[savedResp] &&
                  sz.getValue() !== savedResp) {
                sz.setValue(savedResp, false);
              }
            } else if (respAttempts < 40) {
              respAttempts++;
              setTimeout(tryRestoreResponse, 250);
            }
          }
          tryRestoreResponse();

          // Restore saved predictor state, then sync
          restoreState();
          setTimeout(gatherState, 50);
        })();
      ')

      earth_banner <- if (earth_active) {
        shiny::tags$div(
          class = "glmnet-earth-lock-note",
          style = paste0("font-size:0.78em; padding:4px 6px; margin-bottom:4px;",
                         " border-left:3px solid var(--bs-info, #88c0d0);",
                         " background:var(--bs-tertiary-bg, #eceff4);"),
          shiny::HTML(paste0(
            "Predictor set is determined by the imported earth model. ",
            "<b>Include</b>, <b>Type</b> and <b>Factor</b> are locked; you ",
            "can still set <b>Force</b> and <b>Sign</b> for earth's ",
            "predictors. <b>Factor</b> shows the earth model's treatment: ",
            "checked only for the earth model's categorical predictors. ",
            "Dimmed variables are ones the earth model did not use — they ",
            "play no role in this fit, so their boxes stay empty."))
        )
      }

      shiny::tagList(
        earth_banner,
        header,
        rows,
        shiny::tags$script(shiny::HTML(js_code))
      )
    })

    # --- Reactive outputs for conditionalPanel ---
    output$data_loaded <- shiny::reactive({ !is.null(rv$data) })
    shiny::outputOptions(output, "data_loaded", suspendWhenHidden = FALSE)

    output$has_sheets <- shiny::reactive({ !is.null(rv$sheets) })
    shiny::outputOptions(output, "has_sheets", suspendWhenHidden = FALSE)

    # --- Data preview (rendered in main panel via dataPreviewUI) ---
    # Truncate long free-text cells (e.g. remarks) in the display so a single
    # 1000+ char value can't blow up a whole row. Full text is kept in a
    # hover title. Numeric cells pass through unchanged.
    # Attach the full value as a hover title and cap the DOM payload; the
    # visible single-line clipping + ellipsis is handled by CSS at the
    # column's max-width (see .glmnet-preview in dataPreviewUI).
    preview_columndefs_ <- list(list(
      targets = "_all",
      render = DT::JS(
        "function(data, type, row, meta) {",
        "  if (type !== 'display' || data === null || data === undefined)",
        "    return data;",
        "  var s = String(data);",
        "  if (s.length <= 40) return data;",
        "  return '<span title=\"' + s.replace(/\"/g,'&quot;') + '\">' +",
        "         s.substr(0, 200) + '</span>';",
        "}")
    ))

    # Double-click any cell to open a modal with its full, untruncated text.
    # Truncated cells carry the full value in their title attribute; short
    # cells use their visible text.
    preview_callback_ <- DT::JS(sprintf(
      paste0(
        "table.on('dblclick', 'tbody td', function() {",
        "  var cell = $(this);",
        "  var full = cell.find('span[title]').attr('title');",
        "  if (full === undefined || full === null) full = cell.text();",
        "  Shiny.setInputValue('%s', {value: full, nonce: Math.random()},",
        "                      {priority: 'event'});",
        "});"),
      session$ns("cell_fulltext")))

    # Show full cell text in a modal on double-click.
    shiny::observeEvent(input$cell_fulltext, {
      txt <- input$cell_fulltext$value
      if (is.null(txt) || !nzchar(txt)) return()
      shiny::showModal(shiny::modalDialog(
        title = "Cell contents",
        shiny::tags$div(
          style = paste("white-space: pre-wrap; word-break: break-word;",
                        "max-height: 60vh; overflow-y: auto;"),
          txt),
        easyClose = TRUE, size = "l",
        footer = shiny::modalButton("Close")))
    })

    # General mode: single table
    output$preview_table <- DT::renderDT({
      shiny::req(rv$data, purpose() == "general")
      DT::datatable(rv$data,
                    options = list(scrollX = TRUE, pageLength = 15,
                                   columnDefs = preview_columndefs_),
                    callback = preview_callback_,
                    rownames = FALSE, class = "compact stripe")
    })

    # Appraisal/Market mode: Subject Property (row 1)
    output$preview_subjects <- DT::renderDT({
      shiny::req(rv$data, purpose() %in% c("appraisal", "market"),
                 nrow(rv$data) >= 1L)
      resp <- input$response

      subj <- rv$data[1L, , drop = FALSE]
      # Set response to NA for subject
      if (!is.null(resp) && resp %in% names(subj)) subj[[resp]] <- NA

      DT::datatable(subj,
                    options = list(scrollX = TRUE, dom = "t",
                                   columnDefs = preview_columndefs_),
                    callback = preview_callback_,
                    rownames = FALSE, class = "compact stripe")
    })

    # Appraisal/Market mode: Comparable Sales (rows 2+)
    output$preview_comps <- DT::renderDT({
      shiny::req(rv$data, purpose() %in% c("appraisal", "market"),
                 nrow(rv$data) >= 2L)

      comps <- rv$data[2:nrow(rv$data), , drop = FALSE]

      DT::datatable(comps,
                    options = list(scrollX = TRUE, pageLength = 15,
                                   columnDefs = preview_columndefs_),
                    callback = preview_callback_,
                    rownames = FALSE, class = "compact stripe")
    })

    # --- Effective column types (auto-detected, then user overrides) ---
    effective_types <- shiny::reactive({
      base <- rv$col_types
      overrides <- input$col_types_override
      if (!is.null(overrides) && is.list(overrides)) {
        for (nm in names(overrides)) {
          if (nm %in% names(base)) {
            base[nm] <- overrides[[nm]]
          }
        }
      }
      # Factor designation lives in the Factor checkbox (NOT the Type dropdown,
      # which has no "factor" option, and NOT the Special role). Fold the
      # checked columns into the effective type so they reach glmnet (and the
      # sign/correlation logic) as factors rather than numeric predictors.
      facs <- input$fac_vars
      if (length(facs)) {
        base[intersect(facs, names(base))] <- "factor"
      }
      base
    })

    # --- Derived reactives ---
    expected_signs <- shiny::reactive({
      preds <- input$predictors
      etypes <- effective_types()
      if (is.null(preds) || is.null(etypes)) {
        return(stats::setNames(character(0), character(0)))
      }
      numeric_preds <- preds[
        preds %in% names(etypes)[etypes %in% c("numeric", "integer")]
      ]
      if (length(numeric_preds) == 0) {
        return(stats::setNames(character(0), character(0)))
      }
      signs <- vapply(numeric_preds, function(var) {
        val <- input[[paste0("sign_", var)]]
        if (is.null(val)) "either" else val
      }, FUN.VALUE = character(1))
      signs
    })

    is_valid <- shiny::reactive({
      !is.null(rv$data) &&
        !is.null(input$response) && nzchar(input$response) &&
        !is.null(input$predictors) && length(input$predictors) > 0
    })

    force_in <- shiny::reactive({
      fv <- input$force_vars
      if (is.null(fv)) character(0) else fv
    })

    fac_vars <- shiny::reactive({
      fv <- input$fac_vars
      if (is.null(fv)) character(0) else fv
    })

    col_specials <- shiny::reactive({
      sp <- input$col_specials
      if (is.null(sp) || !is.list(sp)) {
        return(stats::setNames(character(0), character(0)))
      }
      unlist(sp)
    })

    return(list(
      data = shiny::reactive(rv$data),
      response = shiny::reactive(input$response),
      predictors = shiny::reactive(input$predictors),
      expected_signs = expected_signs,
      valid = is_valid,
      col_types = effective_types,
      weight_col = shiny::reactive({
        sp <- col_specials()
        if (is.null(sp) || length(sp) == 0) return(NULL)
        wt_idx <- which(sp == "weight")
        if (length(wt_idx) == 0) return(NULL)
        names(sp)[wt_idx[1L]]
      }),
      force_in = force_in,
      fac_vars = fac_vars,
      col_specials = col_specials,
      file_name = shiny::reactive(rv$file_name),
      rv = rv
    ))
  })
}

#' Convert Column Names to snake_case
#'
#' Converts column names to snake_case by replacing spaces, dots,
#' camelCase boundaries, and other non-alphanumeric characters with
#' underscores, then lowercasing.
#'
#' @param nms Character vector of column names.
#'
#' @return Character vector of snake_case names. Duplicate names are
#'   made unique with numeric suffixes.
#'
#' @export
#' @examples
#' to_snake_case(c("SalePrice", "Lot.Size", "Total Sq Ft", "AGE"))
to_snake_case <- function(nms) {
  out <- nms
  # Insert underscore before uppercase preceded by lowercase (camelCase)
  out <- gsub("([a-z0-9])([A-Z])", "\\1_\\2", out)
  # Replace spaces, dots, dashes, and other separators with underscore
  out <- gsub("[^A-Za-z0-9]+", "_", out)
  # Lowercase
  out <- tolower(out)
  # Remove leading/trailing underscores
  out <- gsub("^_+|_+$", "", out)
  # Collapse multiple underscores
  out <- gsub("_+", "_", out)
  # Ensure unique
  make.unique(out, sep = "_")
}

#' Detect Column Types
#'
#' Classifies columns using R type names: \code{"numeric"},
#' \code{"integer"}, \code{"Date"}, \code{"POSIXct"},
#' \code{"factor"}, or \code{"character"}.
#'
#' @param df A data frame.
#'
#' @return A named character vector of R type names, one per column.
#'
#' @export
#' @examples
#' df <- data.frame(x = 1:10, y = letters[1:10],
#'                  d = Sys.Date() + 1:10, stringsAsFactors = FALSE)
#' detect_column_types_glmnet(df)
detect_column_types_glmnet <- function(df) {
  vapply(df, function(col) {
    if (inherits(col, "POSIXct") || inherits(col, "POSIXlt")) {
      "POSIXct"
    } else if (inherits(col, "Date")) {
      "Date"
    } else if (is.integer(col)) {
      "integer"
    } else if (is.numeric(col)) {
      "numeric"
    } else if (is.factor(col)) {
      "factor"
    } else {
      n_unique <- length(unique(stats::na.omit(col)))
      if (n_unique <= 10 && n_unique < length(col) / 2) {
        "factor"
      } else {
        "character"
      }
    }
  }, FUN.VALUE = character(1))
}

# Flag columns that are UNAMBIGUOUSLY categorical by type: character, factor,
# and logical. Numeric columns are NOT auto-flagged — a numeric variable is a
# factor only if the appraiser ticks the Factor box (the sole factor control;
# Special is unrelated — it is a downstream role, e.g. for the Sales Grid).
# Counting unique values can't distinguish a discrete continuous predictor
# (e.g. bath_count 0-5) from a numeric category code, so it must not drive the
# default. Identifier guard: text where (nearly) every value is distinct
# (street addresses, parcel/MLS ids) is a per-row label, not a usable
# categorical — one level per data row — so it is not pre-checked either.
# Used to pre-check the Factor box in Predictor Settings. Must stay
# behaviorally identical to the exported detect_categoricals().
#' @noRd
detect_categoricals_ <- function(df) {
  res <- vapply(df, function(col) {
    if (is.logical(col)) return(TRUE)
    if (!(is.character(col) || is.factor(col))) return(FALSE)
    v <- col[!is.na(col)]
    n_u <- length(unique(v))
    !(n_u > 10L && n_u > 0.5 * length(v))
  }, logical(1))
  names(res) <- names(df)
  res
}

# Try to parse character columns as POSIXct dates.
# Modifies the data frame in place and returns it.
#' @noRd
auto_parse_dates_ <- function(df) {
  date_fmts <- locale_date_formats_()
  for (nm in names(df)) {
    col <- df[[nm]]
    if (!is.character(col)) next
    vals <- stats::na.omit(col)
    if (length(vals) == 0L) next
    # Date strings are short; skip long free-text columns (e.g. remarks).
    # as.POSIXct()/strptime() also errors ("input string is too long") on
    # very long inputs, which would otherwise abort the whole import.
    if (max(nchar(as.character(vals), type = "bytes")) > 40L) next
    # Sample up to 20 non-NA values for detection
    sample_vals <- if (length(vals) > 20L) vals[1:20] else vals

    # Detect if years are 2-digit: check if any date-separator-delimited
    # part is <= 2 digits where a year would be.
    # If sample contains only short tokens (no 4-digit year), prefer %y.
    has_4digit_year <- any(grepl("\\b\\d{4}\\b", sample_vals))

    # Build format list: if no 4-digit year found, try 2-digit first
    try_fmts <- if (has_4digit_year) {
      date_fmts
    } else {
      # Put 2-digit year formats first
      two_digit <- grep("%y(?!%)", date_fmts, perl = TRUE, value = TRUE)
      four_digit <- setdiff(date_fmts, two_digit)
      c(two_digit, four_digit)
    }

    # Try each format -- require non-NA AND dates in 1900-2100
    # to avoid %Y parsing 2-digit years as year 0025
    min_date <- as.POSIXct("1900-01-01")
    max_date <- as.POSIXct("2100-12-31")
    for (fmt in try_fmts) {
      parsed <- tryCatch(
        suppressWarnings(as.POSIXct(sample_vals, format = fmt)),
        error = function(e) rep(as.POSIXct(NA), length(sample_vals)))
      ok <- !is.na(parsed) & parsed >= min_date & parsed <= max_date
      if (all(ok)) {
        # Format matches sample -- parse the full column
        df[[nm]] <- tryCatch(
          suppressWarnings(as.POSIXct(col, format = fmt)),
          error = function(e) col)
        break
      }
    }
  }
  df
}
