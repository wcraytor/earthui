

#' Render a GAM Report
#'
#' Generates an HTML, PDF, or Word report for a fitted GAM model.
#'
#' @param gam_result An `mgcvUI_result` from [fit_gam()].
#' @param output_format Character: `"html"`, `"pdf"`, or `"docx"`.
#'   Default `"html"`.
#' @param output_file Character path for the output file. If `NULL`
#'   (default), a temporary file is created.
#' @param title Character report title. Default
#'   `"GAM Model Report"`.
#' @return The output file path (invisibly).
#' @export
#' @examples
#' if (interactive()) {
#'   specs <- list(list(vars = "wt", type = "s", bs = "tp", k = NULL))
#'   res <- fit_gam(mtcars, "mpg", specs)
#'   render_gam_report(res, "html")
#' }
render_gam_report <- function(gam_result, output_format = "html",
                              output_file = NULL, title = "GAM Model Report") {
  template <- system.file("rmd", "gam_report.Rmd", package = "earthUI")
  if (!nzchar(template)) {
    stop("Report template not found. Try re-installing mgcvUI.",
         call. = FALSE)
  }

  fmt <- switch(output_format,
    html = "html_document",
    pdf  = "pdf_document",
    docx = "word_document",
    stop("Unsupported format: ", output_format,
         ". Use html, pdf, or docx.", call. = FALSE)
  )

  if (is.null(output_file)) {
    ext <- switch(output_format, html = ".html", pdf = ".pdf", docx = ".docx")
    output_file <- tempfile(fileext = ext)
  }

  # Save result object to temp file for the Rmd to load
  result_path <- tempfile(fileext = ".rds")
  saveRDS(gam_result, result_path)

  rmarkdown::render(
    input       = template,
    output_format = fmt,
    output_file = output_file,
    params      = list(
      result_path = result_path,
      title       = title
    ),
    envir       = new.env(parent = globalenv()),
    quiet       = TRUE
  )

  unlink(result_path)
  invisible(output_file)
}


#' Export GAM Summary to Word (officer)
#'
#' Creates a Word document with model summary tables using
#' \pkg{officer}.
#'
#' @param gam_result An `mgcvUI_result` from [fit_gam()].
#' @param file Character output file path.
#' @return The file path (invisibly).
#' @export
#' @examples
#' if (interactive()) {
#'   specs <- list(list(vars = "wt", type = "s", bs = "tp", k = NULL))
#'   res <- fit_gam(mtcars, "mpg", specs)
#'   export_gam_docx(res, tempfile(fileext = ".docx"))
#' }
export_gam_docx <- function(gam_result, file) {
  summ <- format_gam_summary(gam_result)
  doc <- officer::read_docx()

  doc <- officer::body_add_par(doc, "GAM Model Report",
                               style = "heading 1")
  doc <- officer::body_add_par(doc, paste("Date:", Sys.Date()))
  doc <- officer::body_add_par(doc, "")

  # Model overview
  doc <- officer::body_add_par(doc, "Model Overview", style = "heading 2")
  overview <- data.frame(
    Metric = c("R-squared", "Deviance Explained", "AIC", "BIC",
               "Observations", "Smooth Terms", "Method", "Family"),
    Value = c(
      round(summ$r_squared, 4),
      paste0(round(summ$dev_explained * 100, 1), "%"),
      round(summ$aic, 1),
      round(summ$bic, 1),
      summ$n_obs,
      summ$n_smooths,
      summ$method,
      summ$family
    ),
    stringsAsFactors = FALSE
  )
  doc <- officer::body_add_table(doc, overview, style = "table_template")

  # Smooth terms
  if (nrow(summ$smooth_table) > 0L) {
    doc <- officer::body_add_par(doc, "Smooth Terms", style = "heading 2")
    st <- summ$smooth_table
    st$EDF <- round(st$EDF, 2)
    st$Ref.df <- round(st$Ref.df, 2)
    st$F <- round(st$F, 2)
    st$p_value <- format.pval(st$p_value, digits = 3)
    doc <- officer::body_add_table(doc, st, style = "table_template")
  }

  # Parametric terms
  if (nrow(summ$parametric_table) > 0L) {
    doc <- officer::body_add_par(doc, "Parametric Terms",
                                 style = "heading 2")
    pt <- summ$parametric_table
    pt$Estimate <- round(pt$Estimate, 4)
    pt$Std_Error <- round(pt$Std_Error, 4)
    pt$t_value <- round(pt$t_value, 3)
    pt$p_value <- format.pval(pt$p_value, digits = 3)
    doc <- officer::body_add_table(doc, pt, style = "table_template")
  }

  print(doc, target = file)
  invisible(file)
}


# ===== Two-step Quarto report (generate bundle -> convert) =================

#' Validate an mgcvUI_result object
#' @noRd
validate_mgcvUI_result_ <- function(x) {
  if (!inherits(x, "mgcvUI_result") || is.null(x$model)) {
    stop("`gam_result` must be an mgcvUI_result with a fitted model.",
         call. = FALSE)
  }
  invisible(TRUE)
}

#' Prepare GAM report assets
#'
#' Pre-generates all plots (PNG + PDF) and a `report_data.rds` payload for the
#' GAM report, into `assets_dir`. The heavy fitted model is stripped from the
#' saved payload — the report reads only small precomputed tables/text and the
#' pre-rendered plot files, so the bundle stays compact.
#'
#' @param gam_result An `mgcvUI_result` from [fit_gam()].
#' @param assets_dir Directory to write into. If `NULL`, a temp dir is created.
#' @return The assets directory path (invisibly).
#' @export
prepare_report_assets_mgcv <- function(gam_result, assets_dir = NULL) {
  validate_mgcvUI_result_(gam_result)
  if (is.null(assets_dir)) assets_dir <- tempfile("mgcvUI_assets_")
  dir.create(assets_dir, recursive = TRUE, showWarnings = FALSE)
  assets_dir <- normalizePath(assets_dir)
  plots_dir <- file.path(assets_dir, "plots")
  dir.create(plots_dir, showWarnings = FALSE)

  model <- gam_result$model
  summary_info <- format_gam_summary(gam_result)

  # Variable-importance table: smooth terms by F/Chi.sq, parametric by |t|.
  imp_rows <- list()
  st <- summary_info$smooth_table
  if (nrow(st) > 0L)
    imp_rows[[length(imp_rows) + 1L]] <-
      data.frame(Term = st$Term, Importance = abs(st$F),
                 Kind = "smooth", stringsAsFactors = FALSE)
  pt <- summary_info$parametric_table
  if (nrow(pt) > 0L) {
    pt2 <- pt[pt$Term != "(Intercept)", , drop = FALSE]
    if (nrow(pt2) > 0L)
      imp_rows[[length(imp_rows) + 1L]] <-
        data.frame(Term = pt2$Term, Importance = abs(pt2$t_value),
                   Kind = "parametric", stringsAsFactors = FALSE)
  }
  importance <- if (length(imp_rows)) do.call(rbind, imp_rows) else
    data.frame(Term = character(0), Importance = numeric(0),
               Kind = character(0))

  equation <- tryCatch(paste(deparse(gam_result$formula), collapse = " "),
                       error = function(e) "")
  # Rich LaTeX equation (same f_i notation as the Equation tab), in the
  # line-broken `aligned` form so it fits the PDF text width.
  eq_parts <- tryCatch(
    format_gam_equation_(model, gam_result$response,
                         gam_result$response_transform),
    error = function(e) NULL)
  summary_text <- tryCatch(
    paste(utils::capture.output(print(summary(model))), collapse = "\n"),
    error = function(e) "")

  predictors <- tryCatch(
    unique(unlist(lapply(gam_result$smooth_specs, function(s) s$vars))),
    error = function(e) character(0))

  lean_result <- list(
    response     = gam_result$response,
    predictors   = predictors,
    smooth_specs = gam_result$smooth_specs,
    formula_str  = equation,
    family       = summary_info$family,
    method       = summary_info$method,
    n_obs        = summary_info$n_obs
  )
  class(lean_result) <- "mgcvUI_result_lean"

  n_smooth_chunks <- {
    n <- tryCatch(length(model$smooth), error = function(e) 0L)
    if (n > 0L) as.integer(ceiling(n / 6)) else 0L
  }
  saveRDS(list(
    result        = lean_result,
    summary_info  = summary_info,
    equation      = equation,
    equation_latex = if (!is.null(eq_parts)) eq_parts$aligned else NULL,
    equation_defs = if (!is.null(eq_parts)) eq_parts$defs else NULL,
    importance    = importance,
    summary_text  = summary_text,
    interaction_matrix = build_interaction_matrix_mgcv(gam_result),
    n_smooth_chunks = n_smooth_chunks
  ), file.path(assets_dir, "report_data.rds"), compress = "xz")

  # --- Fonts (offline-safe) ---
  use_showtext <- requireNamespace("sysfonts", quietly = TRUE) &&
    requireNamespace("showtext", quietly = TRUE)
  if (use_showtext) {
    if (!"Roboto Condensed" %in% sysfonts::font_families())
      tryCatch(sysfonts::font_add_google("Roboto Condensed", "Roboto Condensed"),
               error = function(e) NULL)
    showtext::showtext_auto()
    ggplot2::theme_set(ggplot2::theme_minimal(base_family = "Roboto Condensed"))
  } else {
    ggplot2::theme_set(ggplot2::theme_minimal(base_family = "sans"))
  }
  on.exit(if (use_showtext) showtext::showtext_auto(FALSE), add = TRUE)

  save_plot_ <- function(name, plot_fn, width = 8, height = 5) {
    for (ext in c("png", "pdf")) {
      path <- file.path(plots_dir, paste0(name, ".", ext))
      if (ext == "png")
        grDevices::png(path, width = width, height = height, units = "in", res = 150)
      else
        grDevices::pdf(path, width = width, height = height)
      tryCatch({
        res <- plot_fn()
        if (inherits(res, c("ggplot", "patchwork", "gg", "ggalign")) ||
            inherits(res, "htmlwidget")) print(res)
      }, error = function(e) {
        graphics::plot.new(); graphics::text(0.5, 0.5, paste("Error:", e$message))
      })
      grDevices::dev.off()
    }
  }

  # Combined smooth-effect plots (gratia::draw)
  # Smooth Effects: 2 panels per row, split into page-sized chunks (6 smooths =
  # 3 rows ~= one page) so each renders LARGE and they flow across pages,
  # instead of one tall image that LaTeX shrinks to fit a single page.
  n_sm_ <- tryCatch(length(gam_result$model$smooth), error = function(e) 0L)
  sm_per_chunk_ <- 6L
  n_smooth_chunks_ <- if (n_sm_ > 0L)
    as.integer(ceiling(n_sm_ / sm_per_chunk_)) else 0L
  if (n_smooth_chunks_ > 0L) {
    sm_chunks_ <- split(seq_len(n_sm_),
                        ceiling(seq_len(n_sm_) / sm_per_chunk_))
    for (ci_ in seq_along(sm_chunks_)) {
      idx_  <- sm_chunks_[[ci_]]
      rows_ <- ceiling(length(idx_) / 2)
      save_plot_(paste0("smooths_", ci_),
                 function() plot_smooths(gam_result, residuals = TRUE,
                                         select = idx_),
                 width = 9, height = max(3.4, rows_ * 3.0))
    }
  }

  # Per-term plots
  specs <- gam_result$smooth_specs
  if (length(specs) > 0L) {
    for (i in seq_along(specs)) {
      sp <- specs[[i]]
      vars <- sp$vars
      typ  <- sp$type %||% "s"
      nm <- paste0("term_", i)
      if (identical(typ, "linear")) next
      if (length(vars) >= 2L) {
        save_plot_(nm, function() plot_interaction_single(gam_result, vars, type = typ),
                   width = 8, height = 6)
      } else if (!is.null(sp$by) && nzchar(sp$by)) {
        save_plot_(nm, function() plot_by_smooth_single(gam_result, vars, sp$by),
                   width = 8, height = 5)
      } else {
        save_plot_(nm, function() plot_smooth_single(gam_result, vars,
                     residuals = TRUE, earth_knots = gam_result$earth_knots),
                   width = 8, height = 5)
      }
    }
  }

  # Variable importance bar chart
  if (nrow(importance) > 0L) {
    save_plot_("importance", function() {
      imp <- importance[order(importance$Importance), , drop = FALSE]
      imp$Term <- factor(imp$Term, levels = imp$Term)
      ggplot2::ggplot(imp, ggplot2::aes(x = .data$Importance, y = .data$Term,
                                        fill = .data$Kind)) +
        ggplot2::geom_col() +
        ggplot2::labs(x = "Importance (|F| / |t|)", y = NULL, fill = NULL) +
        # Match the axis font sizes used by the smooth/contribution plots
        # (plot_smooth_single) so the term labels and axis are equally legible.
        ggplot2::theme_minimal(base_size = 15) +
        ggplot2::theme(
          axis.title = ggplot2::element_text(size = 16),
          axis.text = ggplot2::element_text(size = 15)
        )
    }, width = 8, height = 5)
  }

  # Correlation matrix of numeric predictors
  save_plot_("correlation", function() {
    mf <- model$model
    num <- mf[, vapply(mf, is.numeric, logical(1)), drop = FALSE]
    if (ncol(num) < 2L) { graphics::plot.new(); return(invisible(NULL)) }
    cm <- stats::cor(num, use = "pairwise.complete.obs")
    dd <- expand.grid(Var1 = rownames(cm), Var2 = colnames(cm))
    dd$value <- as.vector(cm)
    ggplot2::ggplot(dd, ggplot2::aes(.data$Var1, .data$Var2, fill = .data$value)) +
      ggplot2::geom_tile() +
      ggplot2::geom_text(ggplot2::aes(label = sprintf("%.2f", .data$value)),
                         size = 3) +
      ggplot2::scale_fill_gradient2(low = "#bf616a", mid = "#eceff4",
                                    high = "#5e81ac", midpoint = 0,
                                    limits = c(-1, 1)) +
      ggplot2::labs(x = NULL, y = NULL, fill = "r") +
      ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 45, hjust = 1))
  }, width = 9, height = 7)

  # Diagnostics (base 4-panel) + actual vs predicted (ggplot)
  save_plot_("diagnostics", function() plot_diagnostics(gam_result),
             width = 9, height = 7)
  save_plot_("actual_vs_predicted",
             function() plot_actual_vs_predicted_mgcv(gam_result),
             width = 8, height = 5)

  invisible(assets_dir)
}

#' Generate a Quarto report bundle (without rendering)
#'
#' Writes a self-contained Quarto bundle under `<dest_dir>/<base>_qmd/`
#' containing the populated `<base>.qmd`, all pre-generated plots, the report
#' data RDS, and `reference.docx`. Render it to HTML / Word / PDF with
#' [convert_quarto_file()].
#'
#' @param gam_result An `mgcvUI_result` from [fit_gam()].
#' @param dest_dir Directory to write the bundle into.
#' @param base Bundle base name (no extension).
#' @return Invisibly, the absolute path to the generated `.qmd` file.
#' @export
generate_quarto_report_mgcv <- function(gam_result, dest_dir, base = "gam_report") {
  validate_mgcvUI_result_(gam_result)
  dest_dir <- path.expand(dest_dir)
  bundle_dir <- file.path(dest_dir, paste0(base, "_qmd"))
  dir.create(bundle_dir, recursive = TRUE, showWarnings = FALSE)
  bundle_dir <- normalizePath(bundle_dir)

  prepare_report_assets_mgcv(gam_result, assets_dir = bundle_dir)

  template <- system.file("quarto", "gam_report.qmd", package = "earthUI")
  if (template == "")
    stop("Quarto template not found. Try reinstalling 'mgcvUI'.", call. = FALSE)
  qmd_path <- file.path(bundle_dir, paste0(base, ".qmd"))
  file.copy(template, qmd_path, overwrite = TRUE)

  qmd_text <- readLines(qmd_path, warn = FALSE)
  qmd_text <- sub('^  data_file: ""$', '  data_file: "report_data.rds"', qmd_text)
  writeLines(qmd_text, qmd_path)

  ref_docx <- system.file("quarto", "reference.docx", package = "earthUI")
  if (ref_docx != "")
    file.copy(ref_docx, file.path(bundle_dir, "reference.docx"), overwrite = TRUE)

  message("mgcvUI: Quarto report bundle ready at ", bundle_dir)
  invisible(qmd_path)
}

# Build the enabled-interaction matrix for the report: a logical matrix
# (predictors on both axes) with TRUE where a pairwise interaction term was
# included in the GAM. Two kinds count: tensor interactions (`ti`/`te` specs
# with two variables) and factor-by-smooth interactions (a smooth spec with a
# `by` factor). Returns NULL when there are no interactions. Mirrors earthUI's
# allowed-interaction matrix.
#' @noRd
build_interaction_matrix_mgcv <- function(gam_result) {
  specs <- gam_result$smooth_specs
  if (is.null(specs) || length(specs) == 0L) return(NULL)
  pairs <- list()
  for (s in specs) {
    if (!is.null(s$by) && nzchar(s$by) && length(s$vars) >= 1L) {
      pairs <- c(pairs, list(c(s$vars[1L], s$by)))           # factor-by-smooth
    } else if (length(s$vars) == 2L && isTRUE(s$type %in% c("ti", "te"))) {
      pairs <- c(pairs, list(c(s$vars[1L], s$vars[2L])))     # tensor interaction
    }
  }
  if (length(pairs) == 0L) return(NULL)
  vars <- unique(c(unlist(lapply(specs, function(s) s$vars)), unlist(pairs)))
  n <- length(vars)
  mat <- matrix(FALSE, n, n, dimnames = list(vars, vars))
  for (pr in pairs) {
    if (all(pr %in% vars) && pr[1L] != pr[2L]) {
      mat[pr[1L], pr[2L]] <- TRUE
      mat[pr[2L], pr[1L]] <- TRUE
    }
  }
  mat
}
