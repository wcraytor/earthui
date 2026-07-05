#' Launch the glmnet Post Processing Module
#'
#' Starts an interactive Shiny application for elastic net regression
#' modeling using \pkg{glmnet}. The application provides point-and-click
#' data import, variable selection, model fitting with cross-validation,
#' coefficient sign warnings, diagnostic plots, and report export.
#'
#' @param port Integer port number for the Shiny app. Defaults to 7879.
#' @param trilogy Trilogy context, or `NULL` (the default) for a normal
#'   standalone run. When non-`NULL`, the app runs in "trilogy mode": it shows
#'   "(Trilogy Mode)" after the title and (in future steps) reads the locked
#'   earth model + settings for the run. Normally set by the Trilogy UI, not by
#'   hand. The value is exposed to the app via `getOption("earthUI.trilogy")`.
#' @param ... Additional arguments passed to [shiny::runApp()].
#'
#' @return This function does not return a value; it launches a Shiny
#'   application in the user's default browser.
#'
#' @export
#' @examples
#' if (interactive()) {
#'   launch_glmnet()
#' }
launch_glmnet <- function(port = 7879L, trilogy = NULL, ...) {
  if (getRversion() < "4.1.0") {
    stop("glmnetUI requires R >= 4.1.0 (you have ", getRversion(), "). ",
         "Please update R from https://cran.r-project.org/", call. = FALSE)
  }
  required <- c("shiny", "glmnet", "readxl", "DT",
                "ggplot2", "officer", "rmarkdown", "jsonlite",
                "bslib", "shinyFiles", "DBI", "RSQLite")
  missing <- required[!vapply(required, requireNamespace,
                              quietly = TRUE, FUN.VALUE = logical(1))]
  if (length(missing) > 0L) {
    stop("The following required packages are not installed: ",
         paste(missing, collapse = ", "),
         "\nInstall them with: install.packages(c(",
         paste0('"', missing, '"', collapse = ", "), "))",
         call. = FALSE)
  }

  app_dir <- system.file("app_glmnet", package = "earthUI")
  if (!nzchar(app_dir)) {
    stop("Could not find the glmnetUI app directory. ",
         "Is the package installed correctly?", call. = FALSE)
  }

  # Expose the trilogy context to the app (ui.R/server.R read it via
  # getOption); restore on exit so a standalone run later is unaffected.
  op <- options(earthUI.trilogy = trilogy)
  on.exit(options(op), add = TRUE)

  shiny::runApp(app_dir, port = port, ...)
}
