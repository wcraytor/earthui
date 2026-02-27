#' Render an earth model report
#'
#' Renders a parameterized Quarto report from the fitted earth model results.
#' Requires the `quarto` R package and a Quarto installation.
#'
#' @param earth_result An object of class `"earthui_result"` as returned by
#'   [fit_earth()].
#' @param output_format Character. Output format: `"html"`, `"pdf"`, or
#'   `"docx"`. Default is `"html"`.
#' @param output_file Character. Path for the output file. If `NULL`, a
#'   temporary file is created.
#'
#' @return The path to the rendered output file (invisibly).
#'
#' @export
#' @examples
#' \dontrun{
#' result <- fit_earth(mtcars, "mpg", c("cyl", "disp", "hp", "wt"))
#' render_report(result, output_format = "html", output_file = "report.html")
#' }
render_report <- function(earth_result, output_format = "html",
                          output_file = NULL) {
  validate_earthui_result(earth_result)

  if (!requireNamespace("quarto", quietly = TRUE)) {
    stop("The 'quarto' package is required for report rendering. ",
         "Install it with: install.packages('quarto')", call. = FALSE)
  }

  output_format <- match.arg(output_format, c("html", "pdf", "docx"))

  template <- system.file("quarto", "earth_report.qmd", package = "earthui")
  if (template == "") {
    stop("Quarto template not found. Try reinstalling 'earthui'.",
         call. = FALSE)
  }

  if (is.null(output_file)) {
    output_file <- tempfile(fileext = paste0(".", output_format))
  }

  # Create a temporary directory for rendering.
  # Use normalizePath to resolve symlinks (e.g. /var -> /private/var on macOS)
  # which prevents Quarto cleanup errors with path mismatch.
  tmp_dir <- tempfile("earthui_report_")
  dir.create(tmp_dir, recursive = TRUE)
  tmp_dir <- normalizePath(tmp_dir)
  on.exit(unlink(tmp_dir, recursive = TRUE), add = TRUE)

  # Copy template
  tmp_qmd <- file.path(tmp_dir, "earth_report.qmd")
  file.copy(template, tmp_qmd)

  # Save result object for the template to load
  result_file <- file.path(tmp_dir, "earth_result.rds")
  saveRDS(earth_result, result_file)

  # Map format
  quarto_format <- switch(output_format,
    html = "html",
    pdf  = "pdf",
    docx = "docx"
  )

  # Quarto may error during cleanup on macOS due to symlink path mismatches.
  # Catch that error and check if the output was actually produced.
  tryCatch(
    quarto::quarto_render(
      input = tmp_qmd,
      output_format = quarto_format,
      execute_params = list(result_file = result_file)
    ),
    error = function(e) {
      # Check if the rendered file exists despite the error
      rendered <- list.files(tmp_dir,
                             pattern = paste0("\\.", output_format, "$"))
      if (length(rendered) == 0L) stop(e)
    }
  )

  # Find the rendered file
  rendered <- list.files(tmp_dir, pattern = paste0("\\.", output_format, "$"),
                         full.names = TRUE)
  if (length(rendered) == 0L) {
    stop("Report rendering failed: output file not found.", call. = FALSE)
  }

  file.copy(rendered[1], output_file, overwrite = TRUE)
  invisible(output_file)
}
