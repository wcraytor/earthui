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

  # Save pre-computed data
  saveRDS(list(
    result       = earth_result,
    summary_info = summary_info,
    equation     = equation,
    importance   = importance,
    gf           = gf,
    anova        = anova_df,
    multi        = multi,
    targets      = targets
  ), file.path(assets_dir, "report_data.rds"))

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
