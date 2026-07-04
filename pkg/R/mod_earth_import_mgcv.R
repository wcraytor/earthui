#' Earth Import Module -- UI
#'
#' Compact widget for importing an earthUI result (.rds file) via
#' file path. Reads directly from disk (no Shiny upload size limits).
#'
#' @param id Shiny module namespace ID.
#' @return A [shiny::tagList].
#' @export
#' @examples
#' if (interactive()) {
#'   ui <- fluidPage(mod_earth_import_ui("earth1"))
#'   server <- function(input, output, session) {
#'     mod_earth_import_server("earth1")
#'   }
#'   shinyApp(ui, server)
#' }
mod_earth_import_ui <- function(id) {
  ns <- NS(id)
  tagList(
    tags$label("earthUI Result (.rds)", class = "control-label"),
    tags$div(
      class = "input-group",
      style = "margin-bottom:8px;",
      shinyFiles::shinyFilesButton(
        ns("earth_browse"), "Browse...",
        title = "Select earthUI Result (.rds)",
        multiple = FALSE,
        class = "btn-secondary"
      ),
      tags$span(
        class = "form-control",
        style = "font-size: 0.9em; overflow: hidden; text-overflow: ellipsis; white-space: nowrap;",
        textOutput(ns("loaded_path"), inline = TRUE)
      )
    ),
    conditionalPanel(
      condition = sprintf("output['%s']", ns("has_earth")),
      verbatimTextOutput(ns("earth_summary")),
      tags$div(
        style = "display:flex; gap:6px; margin-top:4px;",
        downloadButton(ns("download_knots"), "Export Knots CSV",
                       class = "btn-sm btn-outline-secondary",
                       style = "flex:1;"),
        actionButton(ns("clear_earth"), "Clear",
                     class = "btn-sm btn-outline-secondary",
                     style = "flex:0 0 auto;")
      )
    )
  )
}


#' Earth Import Module -- Server
#'
#' @param id Shiny module namespace ID.
#' @return A reactive containing an `mgcvUI_earth_knots` object, or
#'   `NULL`.
#' @export
mod_earth_import_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    earth_knots <- reactiveVal(NULL)
    loaded_file <- reactiveVal(NULL)

    # Cache directory for persisting last-used path
    cache_dir <- file.path(tools::R_user_dir("earthUI", "data"), "cache")
    if (!dir.exists(cache_dir)) dir.create(cache_dir, recursive = TRUE)

    output$has_earth <- reactive(!is.null(earth_knots()))
    outputOptions(output, "has_earth", suspendWhenHidden = FALSE)

    output$loaded_path <- renderText({
      f <- loaded_file()
      if (is.null(f)) "No file chosen" else f
    })

    # Load helper -- reads directly from disk, no copy needed
    load_earth_ <- function(path, name) {
      ek <- import_earth_mgcv(path)
      earth_knots(ek)
      loaded_file(name)
      # Save name and directory for next session
      tryCatch({
        writeLines(name, file.path(cache_dir, ".last_earth"))
        writeLines(dirname(path), file.path(cache_dir, ".last_earth_dir"))
        writeLines(normalizePath(path, mustWork = FALSE),
                   file.path(cache_dir, ".last_earth_path"))
      }, error = function(e) NULL)
      ek
    }

    # Trilogy mode: auto-import the locked earth model on startup (canonical
    # .rds from the project's trilogy.json, set by earthUI's "Lock Model
    # Output"). The earth seed/knots then apply automatically.
    local({
      ctx <- getOption("earthUI.trilogy")
      if (is.null(ctx)) return(invisible())
      pp <- ctx$project_path %||% NULL
      lock <- if (!is.null(pp)) trilogy_get_lock(pp) else NULL
      rds <- lock$rds %||% ""
      if (isTRUE(lock$locked) && nzchar(rds) && file.exists(rds)) {
        ek <- tryCatch(import_earth_mgcv(rds), error = function(e) NULL)
        if (!is.null(ek)) {
          earth_knots(ek)
          loaded_file(basename(rds))
        }
      }
    })

    # Restore last-used directory for file browser
    last_dir <- path.expand("~")
    last_dir_file <- file.path(cache_dir, ".last_earth_dir")
    if (file.exists(last_dir_file)) {
      saved_dir <- trimws(readLines(last_dir_file, n = 1L, warn = FALSE))
      if (nzchar(saved_dir) && dir.exists(saved_dir)) {
        last_dir <- saved_dir
      }
    }

    # Show last-used filename (but do NOT auto-load -- wait for Browse)
    last_name_file <- file.path(cache_dir, ".last_earth")
    last_path_file <- file.path(cache_dir, ".last_earth_path")
    if (file.exists(last_name_file)) {
      last_name <- trimws(readLines(last_name_file, n = 1L, warn = FALSE))
      if (nzchar(last_name)) {
        last_path <- if (file.exists(last_path_file))
          trimws(readLines(last_path_file, n = 1L, warn = FALSE)) else ""
        if (nzchar(last_path) && file.exists(last_path)) {
          loaded_file(paste0("Last: ", last_name))
        } else {
          loaded_file(paste0("Last: ", last_name, " (not found)"))
        }
      }
    }

    # File browser with last-used directory as default
    volumes <- c(Home = path.expand("~"), Root = "/")
    default_root <- "Home"
    default_path <- ""
    home <- path.expand("~")
    if (startsWith(last_dir, home) && last_dir != home) {
      default_path <- sub(paste0("^", home, "/?"), "", last_dir)
    } else if (last_dir != home) {
      volumes <- c("Last Folder" = last_dir, volumes)
      default_root <- "Last Folder"
    }
    shinyFiles::shinyFileChoose(input, "earth_browse", roots = volumes,
                                defaultRoot = default_root,
                                defaultPath = default_path,
                                filetypes = "rds", session = session)

    observeEvent(input$earth_browse, {
      file_info <- shinyFiles::parseFilePaths(volumes, input$earth_browse)
      if (nrow(file_info) == 0) return()
      path <- as.character(file_info$datapath)
      tryCatch({
        ek <- load_earth_(path, basename(path))
        n_knots <- sum(vapply(ek$knots, length, integer(1)))
        showNotification(
          paste("Imported:", length(ek$knots), "variables,",
                n_knots, "knots"),
          type = "message"
        )
      }, error = function(e) {
        showNotification(paste("Import error:", e$message),
                         type = "error")
        earth_knots(NULL)
      })
    })

    output$earth_summary <- renderPrint({
      ek <- earth_knots()
      req(ek)
      cat("Target:", paste(ek$target, collapse = ", "), "\n")
      cat("R-sq:", round(ek$earth_summary$r_squared, 4), "\n")
      cat("Terms:", ek$earth_summary$n_terms, "\n")
      for (var in names(ek$knots)) {
        cat(var, ": ",
            paste(round(ek$knots[[var]], 2), collapse = ", "),
            "\n")
      }
    })

    output$download_knots <- downloadHandler(
      filename = function() {
        paste0("earth_knots_", format(Sys.Date(), "%Y%m%d"), ".csv")
      },
      content = function(file) {
        ek <- earth_knots()
        req(ek)
        export_knots_csv_mgcv(ek, file)
      }
    )

    observeEvent(input$clear_earth, {
      earth_knots(NULL)
      loaded_file(NULL)
      tryCatch(unlink(file.path(cache_dir, ".last_earth")),
               error = function(e) NULL)
      showNotification("earthUI import cleared.",
                       type = "message", duration = 3)
    })

    list(
      knots = reactive(earth_knots()),
      reset = function() {
        earth_knots(NULL)
        loaded_file(NULL)
      }
    )
  })
}
