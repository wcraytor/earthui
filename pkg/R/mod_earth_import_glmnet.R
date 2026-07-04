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
#'   ui <- shiny::fluidPage(earthImportUI("earth1"))
#'   server <- function(input, output, session) {
#'     earthImportServer("earth1")
#'   }
#'   shinyApp(ui, server)
#' }
earthImportUI <- function(id) {
  ns <- shiny::NS(id)
  shiny::tagList(
    shiny::tags$div(
      style = "margin-bottom:8px;",
      shiny::tags$label("earthUI Result (.rds)", class = "control-label"),
      shiny::tags$div(
        class = "input-group glmnet-earth-browse",
        shinyFiles::shinyFilesButton(
          ns("earth_browse"), "Browse...",
          title = "Select earthUI Result (.rds)",
          multiple = FALSE
        ),
        shiny::tags$input(
          type = "text", class = "form-control", readonly = "readonly",
          id = ns("earth_path_display"),
          placeholder = "No file selected"
        )
      )
    ),
    shiny::conditionalPanel(
      condition = sprintf("output['%s']", ns("has_earth")),
      shiny::verbatimTextOutput(ns("earth_summary")),
      shiny::tags$div(
        style = "display:flex; gap:6px; margin-top:4px;",
        shiny::downloadButton(ns("download_knots"), "Export Knots CSV",
                              class = "btn-sm btn-outline-secondary",
                              style = "flex:1;"),
        shiny::actionButton(ns("clear_earth"), "Clear",
                            class = "btn-sm btn-outline-secondary",
                            style = "flex:0 0 auto;")
      )
    )
  )
}


#' Earth Import Module -- Server
#'
#' @param id Shiny module namespace ID.
#' @return A list with components:
#'   \describe{
#'     \item{earth_import}{Reactive containing a `glmnetUI_earth_import`
#'       object, or `NULL`.}
#'     \item{reset}{Function to clear the imported earth data.}
#'   }
#' @export
earthImportServer <- function(id) {
  shiny::moduleServer(id, function(input, output, session) {
    earth_import_rv <- shiny::reactiveVal(NULL)
    loaded_file <- shiny::reactiveVal(NULL)

    # Cache directory for persisting uploaded files across sessions
    cache_dir <- file.path(tools::R_user_dir("earthUI", "data"), "cache")
    if (!dir.exists(cache_dir)) dir.create(cache_dir, recursive = TRUE)

    output$has_earth <- shiny::reactive(!is.null(earth_import_rv()))
    shiny::outputOptions(output, "has_earth", suspendWhenHidden = FALSE)

    output$loaded_path <- shiny::renderText({
      f <- loaded_file()
      if (is.null(f)) "" else paste0("Loaded: ", f)
    })

    shiny::observe({
      f <- loaded_file()
      val <- if (is.null(f)) "" else basename(f)
      session$sendCustomMessage("glmnet-update-input", list(
        id = session$ns("earth_path_display"), value = val
      ))
    })

    # Load and cache helper
    load_earth_ <- function(path, name) {
      ek <- import_earth_glmnet(path)
      earth_import_rv(ek)
      loaded_file(name)
      # Save full path and directory for next session
      tryCatch({
        writeLines(name, file.path(cache_dir, ".last_earth"))
        writeLines(dirname(path), file.path(cache_dir, ".last_earth_dir"))
      }, error = function(e) NULL)
      ek
    }

    # Trilogy mode: auto-import the locked earth model on startup, so the user
    # does not browse for it. Reads the canonical .rds from the project's
    # trilogy.json (set by earthUI's "Lock Model Output"). The existing
    # earth-import lock on the variable table then applies automatically.
    local({
      ctx <- getOption("earthUI.trilogy")
      if (is.null(ctx)) return(invisible())
      pp <- ctx$project_path %||% NULL
      lock <- if (!is.null(pp)) trilogy_get_lock(pp) else NULL
      rds <- lock$rds %||% ""
      if (isTRUE(lock$locked) && nzchar(rds) && file.exists(rds)) {
        ek <- tryCatch(import_earth_glmnet(rds), error = function(e) NULL)
        if (!is.null(ek)) {
          earth_import_rv(ek)
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

    # Show last-used filename
    last_name_file <- file.path(cache_dir, ".last_earth")
    if (file.exists(last_name_file)) {
      last_name <- trimws(readLines(last_name_file, n = 1L, warn = FALSE))
      if (nzchar(last_name)) {
        loaded_file(paste0("Last: ", last_name, " (not loaded)"))
      }
    }

    # File browser. Expose Home + every mounted volume so the user can always
    # navigate anywhere; the last-used directory is only a starting point and
    # can always be overridden via the root selector.
    volumes <- c(Home = path.expand("~"), shinyFiles::getVolumes()())
    if (.Platform$OS.type == "unix" && dir.exists("/Volumes"))
      volumes <- c(volumes, Volumes = "/Volumes")
    if (.Platform$OS.type == "unix" && dir.exists("/media"))
      volumes <- c(volumes, Media = "/media")
    if (.Platform$OS.type == "unix" && dir.exists("/mnt"))
      volumes <- c(volumes, Mounts = "/mnt")
    default_root <- "Home"
    default_path <- ""
    home <- path.expand("~")
    if (nzchar(last_dir) && dir.exists(last_dir) && last_dir != home) {
      if (startsWith(last_dir, home)) {
        default_path <- sub(paste0("^", home, "/?"), "", last_dir)
      } else {
        # Add as a convenience root, but Home + volumes stay available.
        volumes <- c("Last Folder" = last_dir, volumes)
        default_root <- "Last Folder"
      }
    }
    shinyFiles::shinyFileChoose(input, "earth_browse", roots = volumes,
                                defaultRoot = default_root,
                                defaultPath = default_path,
                                filetypes = "rds", session = session)

    shiny::observeEvent(input$earth_browse, {
      file_info <- shinyFiles::parseFilePaths(volumes, input$earth_browse)
      if (nrow(file_info) == 0) return()
      path <- as.character(file_info$datapath)
      tryCatch({
        ek <- load_earth_(path, basename(path))
        shiny::showNotification(
          paste("Imported:", ek$earth_summary$n_terms, "terms,",
                length(ek$predictors), "predictors"),
          type = "message"
        )
      }, error = function(e) {
        shiny::showNotification(paste("Import error:", e$message),
                                type = "error")
        earth_import_rv(NULL)
      })
    })

    output$earth_summary <- shiny::renderPrint({
      ek <- earth_import_rv()
      shiny::req(ek)
      cat("Target:", paste(ek$target, collapse = ", "), "\n")
      cat("R-sq:", round(ek$earth_summary$r_squared, 4), "\n")
      cat("Terms:", ek$earth_summary$n_terms, "\n")
      cat("Degree:", ek$degree, "\n")
      cat("Predictors:", paste(ek$predictors, collapse = ", "), "\n")
      # Show basis column names
      bx <- build_earth_basis(ek$data, ek)
      if (!is.null(bx)) {
        cat("Basis columns:", ncol(bx), "\n")
      }
    })

    output$download_knots <- shiny::downloadHandler(
      filename = function() {
        paste0("earth_knots_", format(Sys.Date(), "%Y%m%d"), ".csv")
      },
      content = function(file) {
        ek <- earth_import_rv()
        shiny::req(ek)
        export_knots_csv_glmnet(ek, file)
      }
    )

    shiny::observeEvent(input$clear_earth, {
      earth_import_rv(NULL)
      # Remove cached last-earth pointer so it won't auto-load next session
      last_file <- file.path(cache_dir, ".last_earth")
      tryCatch(unlink(last_file), error = function(e) NULL)
      shiny::showNotification("earthUI import cleared.",
                              type = "message", duration = 3)
    })

    list(
      earth_import = shiny::reactive(earth_import_rv()),
      reset = function() {
        earth_import_rv(NULL)
        loaded_file(NULL)
      }
    )
  })
}
