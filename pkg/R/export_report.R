#' Check if a LaTeX installation is available
#' @return Logical TRUE if pdflatex, xelatex, lualatex, or TinyTeX is found.
#' @noRd
has_latex_ <- function() {
  nzchar(Sys.which("pdflatex")) ||
    nzchar(Sys.which("xelatex")) ||
    nzchar(Sys.which("lualatex")) ||
    (requireNamespace("tinytex", quietly = TRUE) &&
       tryCatch(tinytex::is_tinytex(), error = function(e) FALSE))
}


#' Prepare report assets
#'
#' Pre-generates all plots and data for the earth model report. Returns the
#' path to a directory containing all assets. This directory can be passed to
#' [render_report()] to avoid re-computing anything during rendering.
#'
#' @param earth_result An object of class `"earthUI_result"` as returned by
#'   [fit_earth()].
#' @param assets_dir Character. Path to write assets. If `NULL`, a temporary
#'   directory is created.
#'
#' @return The path to the assets directory (invisibly).
#'
#' @export
#' @examples
#' \donttest{
#' result <- fit_earth(mtcars, "mpg", c("cyl", "disp", "hp", "wt"))
#' assets <- prepare_report_assets(result)
#' }
prepare_report_assets <- function(earth_result, assets_dir = NULL) {
  validate_earthUI_result(earth_result)

  if (is.null(assets_dir)) {
    assets_dir <- tempfile("earthUI_assets_")
  }
  dir.create(assets_dir, recursive = TRUE, showWarnings = FALSE)
  assets_dir <- normalizePath(assets_dir)
  plots_dir <- file.path(assets_dir, "plots")
  dir.create(plots_dir, showWarnings = FALSE)

  # --- Pre-compute all data ---
  summary_info <- format_summary(earth_result)
  equation <- format_model_equation(earth_result)
  importance <- format_variable_importance(earth_result)
  gf <- list_g_functions(earth_result)
  anova_df <- format_anova(earth_result)

  multi <- isTRUE(summary_info$multi)
  targets <- earth_result$target
  n_responses <- if (multi) length(targets) else 1L

  # Save pre-computed data. Strip the heavy earth model + raw data from
  # `result` — the report only reads small metadata (predictors, degree,
  # cv_enabled, allowed_matrix, target, categoricals) and lets pre-saved
  # plot files supply everything else. Without this, report_data.rds
  # bloats to hundreds of MB on CV-heavy fits and readRDS can fail.
  lean_result <- earth_result[c("target", "predictors", "categoricals",
                                "linpreds", "degree", "cv_enabled",
                                "allowed_matrix")]
  class(lean_result) <- class(earth_result)
  saveRDS(list(
    result       = lean_result,
    summary_info = summary_info,
    equation     = equation,
    importance   = importance,
    gf           = gf,
    anova        = anova_df,
    multi        = multi,
    targets      = targets
  ), file.path(assets_dir, "report_data.rds"), compress = "xz")

  # --- Set up font for plots (no network) ---
  use_showtext <- requireNamespace("sysfonts", quietly = TRUE) &&
    requireNamespace("showtext", quietly = TRUE)
  if (use_showtext) {
    if (!"Roboto Condensed" %in% sysfonts::font_families()) {
      tryCatch(
        sysfonts::font_add_google("Roboto Condensed", "Roboto Condensed"),
        error = function(e) NULL
      )
    }
    showtext::showtext_auto()
    ggplot2::theme_set(
      ggplot2::theme_minimal(base_family = "Roboto Condensed"))
  } else {
    ggplot2::theme_set(ggplot2::theme_minimal(base_family = "sans"))
  }

  # --- Generate all plots as both PNG and PDF ---
  save_plot_ <- function(name, plot_fn, width = 8, height = 5) {
    for (ext in c("png", "pdf")) {
      path <- file.path(plots_dir, paste0(name, ".", ext))
      if (ext == "png") {
        grDevices::png(path, width = width, height = height,
                       units = "in", res = 150)
      } else {
        grDevices::pdf(path, width = width, height = height)
      }
      tryCatch({
        result <- plot_fn()
        if (inherits(result, "ggplot")) print(result)
      }, error = function(e) {
        graphics::plot.new()
        graphics::text(0.5, 0.5, paste("Error:", e$message), cex = 1.0)
      })
      grDevices::dev.off()
    }
  }

  save_ggplot_ <- function(name, p, width = 8, height = 5) {
    for (ext in c("png", "pdf")) {
      path <- file.path(plots_dir, paste0(name, ".", ext))
      tryCatch(
        ggplot2::ggsave(path, plot = p, width = width, height = height,
                        dpi = 150),
        error = function(e) {
          if (ext == "png") {
            grDevices::png(path, width = width, height = height,
                           units = "in", res = 150)
          } else {
            grDevices::pdf(path, width = width, height = height)
          }
          graphics::plot.new()
          graphics::text(0.5, 0.5, paste("Error:", e$message), cex = 1.0)
          grDevices::dev.off()
        }
      )
    }
  }

  # Variable importance plot
  save_plot_("importance", function() plot_variable_importance(earth_result),
             width = 8, height = 5)

  # Correlation matrix
  save_plot_("correlation", function() plot_correlation_matrix(earth_result),
             width = 10, height = 8)

  # g-function plots (per response for multi-target)
  if (nrow(gf) > 0L) {
    for (ri in seq_len(n_responses)) {
      ri_arg <- if (multi) ri else NULL
      suffix <- if (multi) paste0("_r", ri) else ""
      for (i in seq_len(nrow(gf))) {
        plot_name <- paste0("gfunc_", i, suffix)
        if (gf$d[i] >= 2L) {
          # 3D persp plot (base R)
          persp_fn <- (function(idx, ri) {
            force(idx); force(ri)
            function() plot_g_persp(earth_result, idx, response_idx = ri)
          })(i, ri_arg)
          save_plot_(paste0(plot_name, "_persp"), persp_fn,
                     width = 8, height = 6)
        }
        # 2D/contour plot (ggplot2)
        p <- tryCatch(
          plot_g_contour(earth_result, i, response_idx = ri_arg),
          error = function(e) NULL
        )
        if (!is.null(p)) {
          save_ggplot_(paste0(plot_name, "_contour"), p, width = 8, height = 6)
        }
      }

      # Diagnostics plots
      p <- tryCatch(plot_residuals(earth_result, response_idx = ri_arg),
                     error = function(e) NULL)
      if (!is.null(p))
        save_ggplot_(paste0("residuals", suffix), p, width = 8, height = 5)

      p <- tryCatch(plot_qq(earth_result, response_idx = ri_arg),
                     error = function(e) NULL)
      if (!is.null(p))
        save_ggplot_(paste0("qq", suffix), p, width = 8, height = 5)

      p <- tryCatch(
        plot_actual_vs_predicted(earth_result, response_idx = ri_arg),
        error = function(e) NULL
      )
      if (!is.null(p))
        save_ggplot_(paste0("actual_vs_predicted", suffix), p,
                     width = 8, height = 5)
    }
  }

  if (use_showtext) showtext::showtext_auto(FALSE)
  invisible(assets_dir)
}


#' Generate a Quarto report source bundle (without rendering)
#'
#' Writes a self-contained Quarto project under `<dest_dir>/<base>_qmd/`
#' containing the populated `<base>.qmd` source, all pre-generated plots
#' (PNG + PDF), the report data RDS, and `reference.docx` for Word
#' rendering. The resulting bundle can be edited or combined with other
#' Quarto sources, then rendered to HTML / Word / PDF via
#' [convert_quarto_file()].
#'
#' Use this when you want the Quarto source as a first-class artifact —
#' e.g., to combine reports from multiple projects into a master document
#' before publishing.
#'
#' @param earth_result An object of class `"earthUI_result"` as returned
#'   by [fit_earth()].
#' @param dest_dir Directory to write the bundle into. The bundle itself
#'   lives at `<dest_dir>/<base>_qmd/`.
#' @param base Bundle base name (no extension). The .qmd file inside is
#'   named `<base>.qmd`.
#'
#' @return Invisibly, the absolute path to the generated `.qmd` file.
#'
#' @export
generate_quarto_report <- function(earth_result, dest_dir, base = "earth_report") {
  validate_earthUI_result(earth_result)
  dest_dir <- path.expand(dest_dir)
  bundle_dir <- file.path(dest_dir, paste0(base, "_qmd"))
  dir.create(bundle_dir, recursive = TRUE, showWarnings = FALSE)
  bundle_dir <- normalizePath(bundle_dir)

  # Build assets directly inside the bundle
  prepare_report_assets(earth_result, assets_dir = bundle_dir)

  # Copy the Quarto template + reference.docx into the bundle
  template <- system.file("quarto", "earth_report.qmd", package = "earthUI")
  if (template == "")
    stop("Quarto template not found. Try reinstalling 'earthUI'.",
         call. = FALSE)
  qmd_path <- file.path(bundle_dir, paste0(base, ".qmd"))
  file.copy(template, qmd_path, overwrite = TRUE)

  # Make the bundle self-rendering: inline default params so the .qmd
  # finds report_data.rds + plots/ as siblings without external args.
  qmd_text <- readLines(qmd_path, warn = FALSE)
  qmd_text <- sub('^  data_file: ""$', '  data_file: "report_data.rds"',
                  qmd_text)
  writeLines(qmd_text, qmd_path)

  ref_docx <- system.file("quarto", "reference.docx", package = "earthUI")
  if (ref_docx != "")
    file.copy(ref_docx, file.path(bundle_dir, "reference.docx"),
              overwrite = TRUE)

  message("earthUI: Quarto report bundle ready at ", bundle_dir)
  invisible(qmd_path)
}

#' Convert a Quarto source file to one or more output formats
#'
#' Renders any `.qmd` file (not just earthUI-generated ones) to the
#' requested formats via `quarto::quarto_render()`. Useful for
#' converting hand-edited or manually-combined Quarto reports — e.g.,
#' a master document that uses `{{< include >}}` to pull in multiple
#' project reports.
#'
#' @param qmd_path Path to a Quarto source (`.qmd`) file.
#' @param formats Character vector of output formats. Any subset of
#'   `c("html", "docx", "pdf")`.
#' @param output_dir Directory to write the rendered output(s). Defaults
#'   to the same directory as `qmd_path`.
#' @param paper_size Character: `"letter"` or `"a4"`. Used only for PDF
#'   output. Default `"letter"`.
#'
#' @return Invisibly, a character vector of output file paths.
#'
#' @export
convert_quarto_file <- function(qmd_path,
                                formats = c("html"),
                                output_dir = NULL,
                                paper_size = "letter") {
  if (!requireNamespace("quarto", quietly = TRUE)) {
    stop("The 'quarto' package is required. ",
         "Install it with: install.packages('quarto')", call. = FALSE)
  }
  qmd_path <- path.expand(qmd_path)
  if (!file.exists(qmd_path))
    stop("Quarto file not found: ", qmd_path, call. = FALSE)
  formats <- match.arg(formats, c("html", "docx", "pdf"), several.ok = TRUE)
  paper_size <- match.arg(paper_size, c("letter", "a4"))

  if (is.null(output_dir)) output_dir <- dirname(qmd_path)
  output_dir <- path.expand(output_dir)
  if (!dir.exists(output_dir))
    dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)

  base <- tools::file_path_sans_ext(basename(qmd_path))
  out_paths <- character(0)
  for (fmt in formats) {
    out_file <- file.path(output_dir, paste0(base, ".", fmt))
    message(sprintf("earthUI: rendering %s -> %s", fmt, out_file))
    quarto::quarto_render(input = qmd_path,
                          output_format = fmt,
                          output_file   = basename(out_file),
                          quiet         = FALSE)
    # quarto_render writes to the qmd's directory; move if different
    src <- file.path(dirname(qmd_path), basename(out_file))
    if (normalizePath(src, mustWork = FALSE) !=
        normalizePath(out_file, mustWork = FALSE)) {
      if (file.exists(src)) file.rename(src, out_file)
    }
    out_paths <- c(out_paths, out_file)
  }
  invisible(out_paths)
}


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
#' @param assets_dir Character or `NULL`. Path to pre-generated assets from
#'   [prepare_report_assets()]. If `NULL`, assets are generated on-the-fly.
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
                          output_file = NULL, paper_size = "letter",
                          assets_dir = NULL) {
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

  # Prepare assets if not already done
  own_assets <- FALSE
  if (is.null(assets_dir)) {
    assets_dir <- prepare_report_assets(earth_result)
    own_assets <- TRUE
  }

  # Create a temporary directory for rendering.
  tmp_dir <- tempfile("earthUI_report_")
  dir.create(tmp_dir, recursive = TRUE)
  tmp_dir <- normalizePath(tmp_dir)
  on.exit({
    unlink(tmp_dir, recursive = TRUE)
    if (own_assets) unlink(assets_dir, recursive = TRUE)
  }, add = TRUE)

  # Copy template and supporting files
  tmp_qmd <- file.path(tmp_dir, "earth_report.qmd")
  file.copy(template, tmp_qmd)
  ref_docx <- system.file("quarto", "reference.docx", package = "earthUI")
  if (ref_docx != "") file.copy(ref_docx, tmp_dir)

  # Copy assets into rendering directory
  file.copy(file.path(assets_dir, "report_data.rds"), tmp_dir)
  plots_src <- file.path(assets_dir, "plots")
  if (dir.exists(plots_src)) {
    plots_dst <- file.path(tmp_dir, "plots")
    dir.create(plots_dst, showWarnings = FALSE)
    file.copy(list.files(plots_src, full.names = TRUE), plots_dst)
  }

  # Inject paper size for PDF output
  paper_size <- match.arg(paper_size, c("letter", "a4"))
  if (output_format == "pdf" && paper_size == "a4") {
    qmd_text <- readLines(tmp_qmd, warn = FALSE)
    idx <- grep("^  pdf:", qmd_text, fixed = FALSE)
    if (length(idx) > 0L) {
      insert_line <- paste0("    papersize: ", paper_size)
      qmd_text <- append(qmd_text, insert_line, after = idx[1L])
      writeLines(qmd_text, tmp_qmd)
    }
  }

  quarto_format <- switch(output_format,
    html = "html", pdf = "pdf", docx = "docx"
  )

  # Ensure QUARTO_R points to the running R
  old_quarto_r <- Sys.getenv("QUARTO_R", unset = NA)
  Sys.setenv(QUARTO_R = file.path(R.home("bin"), "R"))
  on.exit({
    if (is.na(old_quarto_r)) Sys.unsetenv("QUARTO_R")
    else Sys.setenv(QUARTO_R = old_quarto_r)
  }, add = TRUE)

  # On Windows, Quarto CLI may not be on PATH. Search common locations.
  if (.Platform$OS.type == "windows" && !nzchar(Sys.which("quarto"))) {
    quarto_search <- c(
      file.path(Sys.getenv("LOCALAPPDATA"), "Programs", "Quarto", "bin"),
      file.path(Sys.getenv("ProgramFiles"), "Quarto", "bin"),
      file.path(Sys.getenv("USERPROFILE"), "AppData", "Local", "Programs",
                "Quarto", "bin"),
      "C:/Program Files/Quarto/bin"
    )
    for (qdir in quarto_search) {
      qexe <- file.path(qdir, "quarto.exe")
      if (file.exists(qexe)) {
        old_path <- Sys.getenv("PATH")
        Sys.setenv(PATH = paste(normalizePath(qdir, winslash = "/"),
                                old_path, sep = ";"))
        Sys.setenv(QUARTO_PATH = normalizePath(qexe, winslash = "/"))
        on.exit(Sys.setenv(PATH = old_path), add = TRUE)
        message("earthUI: found Quarto at ", qexe)
        break
      }
    }
  }

  tryCatch(
    quarto::quarto_render(
      input = tmp_qmd,
      output_format = quarto_format,
      execute_params = list(data_file = file.path(tmp_dir, "report_data.rds"))
    ),
    error = function(e) {
      rendered <- list.files(tmp_dir,
                             pattern = paste0("\\.", output_format, "$"))
      if (length(rendered) == 0L) stop(e)
    }
  )

  rendered <- list.files(tmp_dir, pattern = paste0("\\.", output_format, "$"),
                         full.names = TRUE)
  if (length(rendered) == 0L) {
    stop("Report rendering failed: output file not found.", call. = FALSE)
  }

  file.copy(rendered[1], output_file, overwrite = TRUE)
  invisible(output_file)
}
