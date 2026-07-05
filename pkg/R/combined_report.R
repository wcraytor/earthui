# Combined (comparative) report assembly. Grafted from TrilogyWF in merge
# pass 6; TrilogyWF's manager role is absorbed by earthUI.
#
# Gathers the three methods' outputs for one regProj appraisal project -- by the
# per-method fit timestamps recorded in trilogy.json (run.fit_ts) -- and renders
# a single 1-page HTML summary: the earth value conclusion leads, with the glmnet
# and mgcv Post Processing Modules shown as corroborating support (no weighting). Each method's Sales
# Grid is linked. Written to <project>/trilogy/<os>_out/combined/.

# Find a method's files for a given fit-stamp anywhere under the project.
trilogy_find_method_files_ <- function(project_path, fit_ts) {
  if (is.null(fit_ts) || !nzchar(fit_ts)) return(list(grid = NULL, conclusion = NULL))
  files <- list.files(path.expand(project_path), pattern = fit_ts,
                      recursive = TRUE, full.names = TRUE)
  # Exclude anything already under trilogy/ (we write the report there).
  files <- files[!grepl("[/\\\\]trilogy[/\\\\]", files)]
  bn <- tolower(basename(files))
  grid <- files[grepl("salesgrid|sales_grid", bn)]
  concl <- files[grepl("conclusion", bn) & grepl("\\.json$", bn)]
  list(grid = if (length(grid)) grid[1] else NULL,
       conclusion = if (length(concl)) concl[1] else NULL)
}

# Read a conclusion.json (subject_value, metrics, ...) or NULL.
trilogy_read_conclusion_ <- function(path) {
  if (is.null(path) || !file.exists(path)) return(NULL)
  if (!requireNamespace("jsonlite", quietly = TRUE)) return(NULL)
  tryCatch(jsonlite::read_json(path, simplifyVector = TRUE),
           error = function(e) NULL)
}

# Pretty currency / number, tolerant of NULL.
fmt_money_ <- function(x) {
  x <- suppressWarnings(as.numeric(x))
  if (length(x) != 1L || is.na(x)) return("\u2014")
  paste0("$", formatC(round(x), format = "d", big.mark = ","))
}

#' Build the combined (comparative) appraisal report for a project
#'
#' Reads `trilogy.json`, gathers each method's Sales Grid and `conclusion.json`
#' by its registered fit timestamp, and writes a 1-page HTML summary into
#' `<project>/trilogy/<os>_out/combined/`. The earth conclusion is presented as
#' the value conclusion; the glmnet and mgcv Post Processing Modules appear
#' as independent corroborating fits, unweighted.
#'
#' @param project_path Absolute path to the regProj appraisal project.
#' @param os OS output segment (e.g. "mac"). Defaults to a best guess.
#' @return Invisibly, the path to the written HTML report.
#' @export
build_combined_report <- function(project_path, os = trilogy_os_()) {
  if (!requireNamespace("jsonlite", quietly = TRUE))
    stop("Package 'jsonlite' is required.", call. = FALSE)
  t <- trilogy_read(project_path)
  fts <- t$run$fit_ts %||% list()
  methods <- c(earth = "earth (MARS)",
               glmnet = "glmnet Post Processing Module (elastic net)",
               mgcv = "mgcv Post Processing Module (GAM)")

  gathered <- lapply(names(methods), function(m) {
    ts <- fts[[m]]
    files <- trilogy_find_method_files_(project_path, ts)
    list(key = m, label = methods[[m]], fit_ts = ts,
         grid = files$grid,
         conclusion = trilogy_read_conclusion_(files$conclusion))
  })
  names(gathered) <- names(methods)

  # --- Assemble the 1-page HTML ---
  subj_row <- function(g, primary = FALSE) {
    c <- g$conclusion
    val <- if (!is.null(c)) fmt_money_(c$subject_value) else "\u2014 (pending)"
    metrics <- if (!is.null(c) && !is.null(c$metrics)) {
      paste(sprintf("R\u00b2 %.2f", c$metrics$r2 %||% NA),
            sprintf("RMSE %s", fmt_money_(c$metrics$rmse)), sep = "  \u00b7  ")
    } else "\u2014"
    grid_link <- if (!is.null(g$grid))
      sprintf("<a href='file://%s'>Sales Grid</a>", g$grid) else "<span style='color:#bf616a;'>no grid found</span>"
    sprintf(
      "<tr%s><td><b>%s</b>%s</td><td style='text-align:right;font-size:1.1em;'>%s</td><td>%s</td><td>%s</td></tr>",
      if (primary) " style='background:rgba(136,192,208,0.18);'" else "",
      g$label, if (primary) " <small>(value conclusion)</small>" else " <small>(support)</small>",
      val, metrics,
      if (is.null(g$fit_ts)) "<i>not run</i>" else
        paste0(grid_link, " <small style='color:#888;'>", g$fit_ts, "</small>"))
  }

  rows <- paste0(
    subj_row(gathered$earth, primary = TRUE),
    subj_row(gathered$glmnet),
    subj_row(gathered$mgcv))

  lock <- t$earth_lock %||% list()
  shared <- t$shared %||% list()
  html <- sprintf("<!DOCTYPE html><html><head><meta charset='utf-8'>
<title>Combined Appraisal Report</title>
<style>
  body{font-family:'Roboto Condensed',Arial,sans-serif;color:#2e3440;
       max-width:820px;margin:30px auto;padding:0 16px;}
  h1{color:#5e81ac;} table{width:100%%;border-collapse:collapse;margin-top:12px;}
  th,td{padding:8px 10px;border-bottom:1px solid #d8dee9;text-align:left;}
  th{color:#4c566a;border-bottom:2px solid #5e81ac;}
  .note{color:#4c566a;font-size:0.9em;} .muted{color:#888;}
</style></head><body>
<h1>Combined Appraisal Report</h1>
<p class='note'>Project: <code>%s</code><br>
Subject value uses the <b>earth</b> conclusion (its Sales Grid is preferred);
the glmnet and mgcv Post Processing Modules provide independent
corroborating fits. Locked earth fit:
<code>%s</code>.</p>
<table>
<tr><th>Method</th><th>Subject value</th><th>Fit metrics</th><th>Outputs</th></tr>
%s
</table>
<p class='muted'>CQA: %s (%s). Generated by earthUI.</p>
</body></html>",
    basename(project_path), lock$fit_ts %||% "\u2014", rows,
    shared$cqa %||% "\u2014", shared$cqa_mode %||% "\u2014")

  outdir <- file.path(path.expand(project_path), "trilogy",
                      paste0(os, "_out"), "combined")
  dir.create(outdir, recursive = TRUE, showWarnings = FALSE)
  stamp <- t$run$fit_ts$earth %||% "report"
  outfile <- file.path(outdir, paste0("Trilogy_Report_", stamp, ".html"))
  writeLines(html, outfile)
  invisible(outfile)
}

# OS output segment, mirroring the apps' os_detect() ("mac"/"ubuntu"/"win11").
trilogy_os_ <- function() {
  switch(Sys.info()[["sysname"]],
         Darwin = "mac", Linux = "ubuntu", Windows = "win11", "mac")
}
