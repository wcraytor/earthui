#' Launch the mgcv Post Processing Module
#'
#' Opens the interactive GAM builder in your default web browser.
#'
#' @param port Integer port number. Defaults to 7880.
#' @param launch.browser Logical; open the app in a browser. Defaults to
#'   `TRUE` in interactive sessions, `FALSE` otherwise.
#' @param trilogy Trilogy context, or `NULL` (the default) for a normal
#'   standalone run. When non-`NULL`, the app runs in "trilogy mode": it shows
#'   "(Trilogy Mode)" after the title and (in future steps) reads the locked
#'   earth model + settings for the run. Normally set by the Trilogy UI, not by
#'   hand. Exposed to the app via `getOption("earthUI.trilogy")`.
#' @param ... Additional arguments passed to [shiny::runApp()].
#' @return Called for its side effect (launches the app). Returns the
#'   value of [shiny::runApp()] invisibly.
#' @export
#' @examples
#' if (interactive()) {
#'   launch_mgcv()
#' }
launch_mgcv <- function(port = 7880L, launch.browser = interactive(),
                   trilogy = NULL, ...) {
  if (getRversion() < "4.1.0") {
    stop("mgcvUI requires R >= 4.1.0 (you have ", getRversion(), "). ",
         "Please update R from https://cran.r-project.org/", call. = FALSE)
  }
  app_dir <- system.file("app_mgcv", package = "earthUI")
  if (!nzchar(app_dir)) {
    stop("Could not find the mgcvUI app directory. ",
         "Try re-installing the package.", call. = FALSE)
  }
  # Expose the trilogy context to the app (app.R reads it via getOption);
  # restore on exit so a standalone run later is unaffected.
  op <- options(earthUI.trilogy = trilogy)
  on.exit(options(op), add = TRUE)

  shiny::runApp(app_dir, port = port, launch.browser = launch.browser, ...)
}
