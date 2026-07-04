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
#' @param trilogy Trilogy context, or `NULL` (the default) for a normal
#'   standalone run. When non-`NULL`, the app runs in "trilogy mode": it shows
#'   "(Trilogy Mode)" after the title and (in future steps) the model-lock
#'   workflow for the run. Normally set by the Trilogy UI, not by hand. Exposed
#'   to the app via `getOption("earthUI.trilogy")`.
#' @param ... Additional arguments passed to [shiny::runApp()].
#'
#' @return This function does not return a value; it launches the Shiny app.
#'
#' @export
#' @examples
#' if (interactive()) {
#'   launch()
#' }
launch <- function(port = 7878L, trilogy = NULL, ...) {
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

  # External command-line tools for report rendering (PDF/HTML/Word): quarto +
  # pandoc + a LaTeX engine, reachable on PATH. Model fitting and Excel/CSV
  # export do NOT need these. Warn (do not stop) with install hints so the user
  # gets a clear message up front instead of a cryptic failure when they click
  # Generate Report. Common bin dirs are also checked because the app extends
  # PATH at session start (so a tool in e.g. /opt/homebrew/bin is still found).
  extra_bin <- c("/usr/local/bin", "/opt/homebrew/bin")
  have_tool <- function(tool) {
    if (nzchar(Sys.which(tool))) return(TRUE)
    exe <- if (.Platform$OS.type == "windows") paste0(tool, ".exe") else tool
    any(file.exists(file.path(extra_bin, exe)))
  }
  report_missing <- character(0)
  if (!have_tool("quarto")) {
    report_missing <- c(report_missing,
      "quarto  - the Quarto CLI (https://quarto.org/docs/get-started/)")
  }
  pandoc_ok <- have_tool("pandoc") ||
    (requireNamespace("rmarkdown", quietly = TRUE) &&
       isTRUE(tryCatch(rmarkdown::pandoc_available(), error = function(e) FALSE)))
  if (!pandoc_ok) {
    report_missing <- c(report_missing,
      "pandoc  - https://pandoc.org/installing.html (also bundled with RStudio)")
  }
  latex_ok <- (requireNamespace("tinytex", quietly = TRUE) &&
                 isTRUE(tryCatch(tinytex::is_tinytex(), error = function(e) FALSE))) ||
    have_tool("lualatex") || have_tool("xelatex") || have_tool("pdflatex")
  if (!latex_ok) {
    report_missing <- c(report_missing,
      "LaTeX   - for PDF reports; install with tinytex::install_tinytex()")
  }
  if (length(report_missing) > 0L) {
    message("earthUI: report generation (PDF/HTML/Word) needs external tools ",
            "not currently found - the app still runs, but the report ",
            "features will fail until these are installed:")
    for (m in report_missing) message("    ", m)
    message("    (Model fitting, plots, and Excel/CSV export do not need these.)")
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

  app_dir <- system.file("app_earth", package = "earthUI")
  if (app_dir == "") {
    stop("Could not find the Shiny app directory. ",
         "Try reinstalling the 'earthUI' package.", call. = FALSE)
  }
  # Expose the trilogy context to the app (ui.R/server.R read it via
  # getOption); restore on exit so a standalone run later is unaffected.
  op <- options(earthUI.trilogy = trilogy)
  on.exit(options(op), add = TRUE)

  shiny::runApp(app_dir, port = port, ...)
}
