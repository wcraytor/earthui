#' Launch the earthui Shiny application
#'
#' Opens an interactive Shiny GUI for building and exploring earth
#' (MARS-style) models. The application provides data import, variable
#' configuration, model fitting, result visualization, and report export.
#'
#' @param ... Additional arguments passed to [shiny::runApp()].
#'
#' @return This function does not return a value; it launches the Shiny app.
#'
#' @export
#' @examples
#' if (interactive()) {
#'   launch()
#' }
launch <- function(...) {
  for (pkg in c("bslib", "DT", "jsonlite")) {
    if (!requireNamespace(pkg, quietly = TRUE)) {
      stop("Package '", pkg, "' is required for the Shiny app. ",
           "Install it with: install.packages('", pkg, "')",
           call. = FALSE)
    }
  }
  app_dir <- system.file("app", package = "earthui")
  if (app_dir == "") {
    stop("Could not find the Shiny app directory. ",
         "Try reinstalling the 'earthui' package.", call. = FALSE)
  }
  shiny::runApp(app_dir, ...)
}
