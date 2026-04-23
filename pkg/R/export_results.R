#' Write an earthUI fitting log to a text file
#'
#' Writes the timestamped contents of an earth fitting trace to
#' `<file_name>_earth_log_<timestamp>.txt` in `output_folder`.
#' If `output_folder` is `NULL` or empty, `~/Downloads` is used. The folder
#' is created if it doesn't exist. Errors are caught and reported via
#' `message()` so batch pipelines don't fail on logging issues.
#'
#' @param output_folder Character scalar. Directory in which to write the
#'   log. May be `NULL` or empty.
#' @param lines Character vector of log lines.
#' @param file_name Character scalar. Used to derive the log filename (the
#'   extension is stripped).
#'
#' @return Invisibly, `NULL`.
#'
#' @export
write_fit_log <- function(output_folder, lines, file_name) {
  tryCatch({
    folder <- if (is.null(output_folder) || !nzchar(output_folder)) {
      path.expand("~/Downloads")
    } else {
      output_folder
    }
    if (!dir.exists(folder)) dir.create(folder, recursive = TRUE)
    base <- tools::file_path_sans_ext(file_name %||% "earthui")
    log_path <- file.path(folder, paste0(base, "_earth_log_",
                          format(Sys.time(), "%Y%m%d_%H%M%S"), ".txt"))
    writeLines(c(paste("earthUI fitting log:", Sys.time()), "", lines), log_path)
  }, error = function(e) {
    message("earthUI: failed to write log: ", e$message)
  })
  invisible(NULL)
}

#' Write an earth model summary to a text file
#'
#' Writes the model print-out, `summary.earth()`, and optional variance
#' model / trace log to
#' `<file_name>_earth_output_<timestamp>.txt` in `output_folder`.
#'
#' @param result A fit result list as returned by [fit_earth()] (must have
#'   `$model` at minimum; `$elapsed`, `$seed`, `$trace_output` are optional).
#' @param output_folder Character scalar. May be `NULL` or empty
#'   (defaults to `~/Downloads`).
#' @param file_name Character scalar. Used to derive the output filename.
#'
#' @return Invisibly, `NULL`.
#'
#' @export
write_earth_output <- function(result, output_folder, file_name) {
  tryCatch({
    folder <- if (is.null(output_folder) || !nzchar(output_folder)) {
      path.expand("~/Downloads")
    } else {
      output_folder
    }
    if (!dir.exists(folder)) dir.create(folder, recursive = TRUE)
    base <- tools::file_path_sans_ext(file_name %||% "earthui")
    out_path <- file.path(folder, paste0(base, "_earth_output_",
                          format(Sys.time(), "%Y%m%d_%H%M%S"), ".txt"))
    model <- result$model
    lines <- utils::capture.output({
      cat(sprintf("earthUI output: %s\n", Sys.time()))
      if (!is.null(result$elapsed))
        cat(sprintf("Timing: %.2f seconds\n", result$elapsed))
      if (!is.null(result$seed))
        cat(sprintf("Random Seed: %d\n", result$seed))
      cat("\n== Model ==\n\n")
      print(model)
      cat("\n\n== Summary ==\n\n")
      print(summary(model))
      if (!is.null(model$varmod)) {
        cat("\n\n== Variance Model ==\n\n")
        print(model$varmod)
      }
      if (length(result$trace_output) > 0L) {
        trace_lines <- result$trace_output
        trace_lines <- trace_lines[nzchar(trimws(trace_lines))]
        if (length(trace_lines) > 0L) {
          cat("\n\n== Trace Log ==\n\n")
          cat(paste(trace_lines, collapse = "\n"), "\n")
        }
      }
    })
    writeLines(lines, out_path)
  }, error = function(e) {
    message("earthUI: failed to write earth output: ", e$message)
  })
  invisible(NULL)
}

#' Export an earthUI result as RDS for consumption by mgcvUI
#'
#' Saves the result via [saveRDS()] to
#' `<file_name>_earthUI_result_<timestamp>.rds` and verifies the file is
#' readable and can produce a prediction. If verification fails, the file
#' is deleted. Skipped silently for models with `degree > 2` (mgcvUI only
#' supports pairwise interactions).
#'
#' @param result A fit result list as returned by [fit_earth()] (class
#'   `earthUI_result`).
#' @param output_folder Character scalar. May be `NULL` or empty
#'   (defaults to `~/Downloads`).
#' @param file_name Character scalar. Used to derive the output filename.
#'
#' @return Invisibly, `NULL`.
#'
#' @export
auto_export_for_mgcv <- function(result, output_folder, file_name) {
  tryCatch({
    deg <- result$degree %||% 1L
    if (deg > 2L) return(invisible(NULL))
    folder <- if (is.null(output_folder) || !nzchar(output_folder)) {
      path.expand("~/Downloads")
    } else {
      output_folder
    }
    if (!dir.exists(folder)) dir.create(folder, recursive = TRUE)
    base <- tools::file_path_sans_ext(file_name %||% "earth")
    out_path <- file.path(folder, paste0(base, "_earthUI_result_",
                          format(Sys.time(), "%Y%m%d_%H%M%S"), ".rds"))
    saveRDS(result, out_path)
    verify <- tryCatch({
      obj <- readRDS(out_path)
      if (!inherits(obj, "earthUI_result")) stop("not an earthUI_result")
      if (is.null(obj$model)) stop("model is NULL")
      stats::predict(obj$model, newdata = obj$data[1L, , drop = FALSE])
      TRUE
    }, error = function(e) {
      message("earthUI: RDS verification FAILED: ", e$message,
              " — deleting corrupt file")
      unlink(out_path)
      FALSE
    })
    if (verify) {
      message("earthUI: auto-exported result for mgcvUI to ", out_path)
    }
  }, error = function(e) {
    message("earthUI: auto-export for mgcvUI failed: ", e$message)
  })
  invisible(NULL)
}
