#' Launch the earthUI Shiny application
#'
#' Opens an interactive 'shiny' GUI for building and exploring 'earth'
#' (MARS-style) models. The application provides data import, variable
#' configuration, model fitting, result visualization, and report export.
#'
#' @param port Integer. Port number for the Shiny app. Defaults to 7878.
#'   A fixed port ensures browser localStorage (saved settings) persists
#'   across sessions.
#' @param ... Additional arguments passed to [shiny::runApp()].
#'
#' @return This function does not return a value; it launches the Shiny app.
#'
#' @export
#' @examples
#' if (interactive()) {
#'   launch()
#' }
launch <- function(port = 7878L, ...) {
  if (getRversion() < "4.1.0") {
    stop("earthUI requires R >= 4.1.0 (you have ", getRversion(), "). ",
         "Please update R from https://cran.r-project.org/", call. = FALSE)
  }
  for (pkg in c("bslib", "DT", "jsonlite")) {
    if (!requireNamespace(pkg, quietly = TRUE)) {
      stop("Package '", pkg, "' is required for the Shiny app. ",
           "Install it with: install.packages('", pkg, "')",
           call. = FALSE)
    }
  }
  # Optional: SQLite settings persistence
  for (pkg in c("DBI", "RSQLite")) {
    if (!requireNamespace(pkg, quietly = TRUE)) {
      message("earthUI: '", pkg, "' not installed. ",
              "Settings will be stored in browser localStorage only. ",
              "Install with: install.packages('", pkg, "')")
    }
  }

  # Kill any existing process on the port (avoids "address already in use")
  if (.Platform$OS.type == "unix") {
    tryCatch(
      system2("lsof", c("-ti", paste0(":", port)),
              stdout = TRUE, stderr = FALSE),
      error = function(e) character(0)
    ) -> pids
    pids <- pids[nzchar(pids)]
    if (length(pids) > 0L) {
      message("earthUI: killing existing process on port ", port,
              " (PIDs: ", paste(pids, collapse = ", "), ")")
      for (pid in pids) {
        tryCatch(tools::pskill(as.integer(pid)), error = function(e) NULL)
      }
      Sys.sleep(0.5)
    }
  }

  app_dir <- system.file("app", package = "earthUI")
  if (app_dir == "") {
    stop("Could not find the Shiny app directory. ",
         "Try reinstalling the 'earthUI' package.", call. = FALSE)
  }
  shiny::runApp(app_dir, port = port, ...)
}
