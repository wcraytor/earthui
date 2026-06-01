#' Launch the earthUI Shiny application
#'
#' Opens an interactive 'shiny' GUI for building and exploring 'earth'
#' (MARS-style) models. The application provides data import, variable
#' configuration, model fitting, result visualization, and report export.
#'
#' @param port Integer. Port number for the Shiny app. Defaults to 7878.
#'   A fixed port keeps browser-side UI preferences (theme, last-used purpose)
#'   consistent across sessions. (Model configuration is saved server-side in
#'   the project database, not in the browser.)
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
  # UI-critical optional packages (declared in Suggests): the app cannot
  # build its interface without these.
  for (pkg in c("bslib", "DT", "shinyFiles")) {
    if (!requireNamespace(pkg, quietly = TRUE)) {
      stop("Package '", pkg, "' is required to run the earthUI app. ",
           "Install it with: install.packages('", pkg, "')",
           call. = FALSE)
    }
  }
  # Settings persistence lives in the project database (projects.sqlite).
  for (pkg in c("DBI", "RSQLite")) {
    if (!requireNamespace(pkg, quietly = TRUE)) {
      message("earthUI: '", pkg, "' not installed - per-project settings ",
              "save/restore will be disabled. ",
              "Install with: install.packages('", pkg, "')")
    }
  }
  # Feature-level optional packages: the app launches without them, but the
  # corresponding feature is unavailable until installed.
  feature_pkgs <- c(
    callr     = "asynchronous model fitting (otherwise falls back to synchronous)",
    writexl   = "Excel downloads",
    knitr     = "report generation",
    rmarkdown = "report generation",
    quarto    = "Quarto report rendering"
  )
  missing_feat <- names(feature_pkgs)[!vapply(
    names(feature_pkgs), requireNamespace, logical(1), quietly = TRUE)]
  if (length(missing_feat) > 0L) {
    message("earthUI: optional package(s) not installed - the related ",
            "feature stays unavailable until you install them:")
    for (pkg in missing_feat) {
      message("  - ", pkg, ": ", feature_pkgs[[pkg]],
              "  (install.packages('", pkg, "'))")
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
