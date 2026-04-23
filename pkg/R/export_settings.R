#' Export saved earthUI settings for one file+purpose to a JSON file
#'
#' The Shiny app persists per-file, per-purpose settings in a SQLite DB
#' keyed by `"<filename>||<purpose>"`. `export_settings()` reads that row
#' and writes a single JSON file containing the full settings bundle
#' (target, earth parameters, variable selections, type/special overrides,
#' and interactions), plus an `rca` block for batch RCA inputs — the
#' subject CQA score and CQA score type.
#'
#' If `output_json` already exists, the `rca` block of the existing file
#' is preserved — re-exporting from the UI does not clobber hand-edited
#' CQA inputs.
#'
#' @param filename Character scalar. The filename as stored in the DB
#'   (e.g. `"Appraisal_1.csv"`, without any path prefix).
#' @param purpose Character scalar: `"general"`, `"appraisal"`, or
#'   `"market"`.
#' @param output_json Character scalar. Destination file path (`.json`).
#'
#' @return Invisibly, the `output_json` path.
#'
#' @details
#' The emitted `rca` block has two fields:
#' \describe{
#'   \item{cqa_score}{`null` or a number in `[0.00, 10.00]`.}
#'   \item{cqa_score_type}{`"CQA/sf"` (based on residual / living-area,
#'     default) or `"CQA"` (based on residual).}
#' }
#'
#' The emitted `reports` field is an array of formats to render in batch
#' mode: any subset of `"html"`, `"pdf"`, `"docx"`. An empty array `[]`
#' (default) means no reports are generated.
#'
#' @export
#' @examples
#' \dontrun{
#' export_settings("Appraisal_1.csv", "appraisal",
#'                 "~/configs/Appraisal_1.json")
#' }
export_settings <- function(filename, purpose, output_json) {
  if (!requireNamespace("jsonlite", quietly = TRUE)) {
    stop("Package 'jsonlite' is required.", call. = FALSE)
  }
  con <- settings_db_connect_()
  if (is.null(con)) {
    stop("Cannot open earthUI settings database. Install DBI and RSQLite.",
         call. = FALSE)
  }
  on.exit(settings_db_disconnect_(con), add = TRUE)

  key    <- paste0(filename, "||", purpose)
  record <- settings_db_read_(con, key)
  if (is.null(record)) {
    stop(sprintf("No saved settings found for '%s' (purpose='%s').",
                 filename, purpose), call. = FALSE)
  }

  # Preserve existing `rca` and `reports` blocks if the output file already
  # exists, so re-exporting from the UI does not clobber hand-edited values.
  rca_block     <- list(cqa_score = NULL, cqa_score_type = "CQA/sf")
  reports_block <- character(0)
  if (file.exists(output_json)) {
    prev <- tryCatch(
      jsonlite::fromJSON(output_json, simplifyVector = FALSE),
      error = function(e) NULL
    )
    if (!is.null(prev$rca))     rca_block     <- prev$rca
    if (!is.null(prev$reports)) reports_block <- unlist(prev$reports)
  }

  bundle <- list(
    filename     = filename,
    purpose      = purpose,
    exported_at  = format(Sys.time(), "%Y-%m-%dT%H:%M:%S%z"),
    settings     = record$settings,
    variables    = record$variables,
    interactions = record$interactions,
    rca          = rca_block,
    reports      = I(reports_block)
  )

  dir <- dirname(output_json)
  if (!dir.exists(dir)) dir.create(dir, recursive = TRUE)

  jsonlite::write_json(bundle, output_json,
                       pretty    = TRUE,
                       auto_unbox = TRUE,
                       null      = "null")
  invisible(output_json)
}
