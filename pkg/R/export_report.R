#' Render an earth model report
#'
#' Renders a parameterized 'Quarto' report from the fitted 'earth' model results.
#' Requires the 'quarto' R package and a 'Quarto' installation.
#'
#' @param earth_result An object of class `"earthUI_result"` as returned by
#'   [fit_earth()].
#' @param output_format Character. Output format: `"html"`, `"pdf"`, or
#'   `"docx"`. Default is `"html"`.
#' @param output_file Character. Path for the output file. If `NULL`, a
#'   temporary file is created.
#' @param paper_size Character. Paper size for PDF output: `"letter"` (US) or
#'   `"a4"` (European). Default is `"letter"`. Ignored for HTML/Word.
#'
#' @return The path to the rendered output file (invisibly).
#'
#' @export
#' @examples
#' \donttest{
#' result <- fit_earth(mtcars, "mpg", c("cyl", "disp", "hp", "wt"))
#' render_report(result, output_format = "html",
#'               output_file = tempfile(fileext = ".html"))
#' }
render_report <- function(earth_result, output_format = "html",
                          output_file = NULL, paper_size = "letter") {
  validate_earthUI_result(earth_result)

  if (!requireNamespace("quarto", quietly = TRUE)) {
    stop("The 'quarto' package is required for report rendering. ",
         "Install it with: install.packages('quarto')", call. = FALSE)
  }

  output_format <- match.arg(output_format, c("html", "pdf", "docx"))

  template <- system.file("quarto", "earth_report.qmd", package = "earthUI")
  if (template == "") {
    stop("Quarto template not found. Try reinstalling 'earthUI'.",
         call. = FALSE)
  }

  if (is.null(output_file)) {
    output_file <- tempfile(fileext = paste0(".", output_format))
  }

  # Create a temporary directory for rendering.
  # Use normalizePath to resolve symlinks (e.g. /var -> /private/var on macOS)
  # which prevents Quarto cleanup errors with path mismatch.
  tmp_dir <- tempfile("earthUI_report_")
  dir.create(tmp_dir, recursive = TRUE)
  tmp_dir <- normalizePath(tmp_dir)
  on.exit(unlink(tmp_dir, recursive = TRUE), add = TRUE)

  # Copy template and supporting files
  tmp_qmd <- file.path(tmp_dir, "earth_report.qmd")
  file.copy(template, tmp_qmd)
  ref_docx <- system.file("quarto", "reference.docx", package = "earthUI")
  if (ref_docx != "") file.copy(ref_docx, tmp_dir)

  # Inject paper size for PDF output (replace default letter with a4 if needed)
  paper_size <- match.arg(paper_size, c("letter", "a4"))
  if (output_format == "pdf" && paper_size == "a4") {
    qmd_text <- readLines(tmp_qmd, warn = FALSE)
    # Add papersize to the pdf format block in YAML
    idx <- grep("^  pdf:", qmd_text, fixed = FALSE)
    if (length(idx) > 0L) {
      insert_line <- paste0("    papersize: ", paper_size)
      qmd_text <- append(qmd_text, insert_line, after = idx[1L])
      writeLines(qmd_text, tmp_qmd)
    }
  }

  # Save result object for the template to load
  result_file <- file.path(tmp_dir, "earth_result.rds")
  saveRDS(earth_result, result_file)

  # Map format
  quarto_format <- switch(output_format,
    html = "html",
    pdf  = "pdf",
    docx = "docx"
  )

  # Ensure QUARTO_R points to the running R (may be stale from a prior install)
  old_quarto_r <- Sys.getenv("QUARTO_R", unset = NA)
  Sys.setenv(QUARTO_R = file.path(R.home("bin"), "R"))
  on.exit({
    if (is.na(old_quarto_r)) Sys.unsetenv("QUARTO_R") else Sys.setenv(QUARTO_R = old_quarto_r)
  }, add = TRUE)

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
