# Trilogy coordination file (trilogy.json) for a regProj appraisal project.
#
# Shared verbatim across the sibling apps (earthUI, glmnetUI, mgcvUI): it is the
# single source of truth for a Trilogy run -- the earth model lock, the shared
# settings (target, CQA + raw/per-SF mode), per-app settings, and the trio of
# per-method fit timestamps that group a run's files. Lives at
# `<project>/trilogy/trilogy.json`. Timestamps are the fit-stamp strings used in
# output filenames ("%Y%m%d_%H%M%S"), so a Trilogy report can gather files by
# matching them.

#' Path to a project's `trilogy.json`
#'
#' @param project_path Absolute path to the regProj appraisal project (the flat
#'   segment folder under `<root>/appr/`).
#' @return Character scalar: `<project_path>/trilogy/trilogy.json`.
#' @export
trilogy_json_path <- function(project_path) {
  file.path(path.expand(project_path), "trilogy", "trilogy.json")
}

# Default (empty) trilogy.json skeleton.
trilogy_default_ <- function() {
  list(
    schema     = 2L,
    subject    = list(),
    shared     = list(),
    earth_lock = list(locked = FALSE),
    earth      = list(),
    glmnet     = list(),
    mgcv       = list(),
    run        = list(methods = list(), fit_ts = list())
  )
}

#' Read a project's `trilogy.json`
#'
#' Returns a default skeleton (with `earth_lock$locked = FALSE`) when the file is
#' absent or unreadable, so callers never have to special-case "no trilogy yet".
#'
#' @inheritParams trilogy_json_path
#' @return A named list (the parsed trilogy.json, or the default skeleton).
#' @export
trilogy_read <- function(project_path) {
  p <- trilogy_json_path(project_path)
  if (!file.exists(p)) return(trilogy_default_())
  if (!requireNamespace("jsonlite", quietly = TRUE)) return(trilogy_default_())
  tryCatch(jsonlite::read_json(p, simplifyVector = FALSE),
           error = function(e) trilogy_default_())
}

#' Write a project's `trilogy.json` (atomic; creates `trilogy/` if needed)
#'
#' @inheritParams trilogy_json_path
#' @param trilogy A named list to serialize (typically a modified
#'   [trilogy_read()] result).
#' @return Invisibly, the path written.
#' @export
trilogy_write <- function(project_path, trilogy) {
  if (!requireNamespace("jsonlite", quietly = TRUE)) {
    stop("Package 'jsonlite' is required.", call. = FALSE)
  }
  p <- trilogy_json_path(project_path)
  dir.create(dirname(p), recursive = TRUE, showWarnings = FALSE)
  tmp <- paste0(p, ".tmp")
  jsonlite::write_json(trilogy, tmp, pretty = TRUE, auto_unbox = TRUE,
                       null = "null")
  file.rename(tmp, p)
  invisible(p)
}

#' Lock the canonical earth model for a Trilogy run
#'
#' Records which earth `.rds` (and its fit-stamp) is the one glmnetUI and mgcvUI
#' import, along with the settings that locking earth imposes downstream and the
#' shared settings (target, CQA, raw/per-SF). Mirrors today's earth-import lock
#' plus the CQA additions.
#'
#' @inheritParams trilogy_json_path
#' @param fit_ts Fit-stamp string of the locked earth fit (`"%Y%m%d_%H%M%S"`).
#' @param rds Path to the locked earth result `.rds` (relative to the project,
#'   or absolute).
#' @param downstream_locked Named list of settings locked downstream (e.g.
#'   `predictors`, `factor`, `linear`).
#' @param shared Optional named list of shared settings (target, `cqa_mode`,
#'   `cqa`, ...); left unchanged when `NULL`.
#' @return Invisibly, the path written.
#' @export
trilogy_set_lock <- function(project_path, fit_ts, rds,
                             downstream_locked = list(), shared = NULL) {
  obj <- trilogy_read(project_path)
  obj$earth_lock <- list(
    locked            = TRUE,
    fit_ts            = fit_ts,
    rds               = rds,
    locked_at         = format(Sys.time(), "%Y-%m-%dT%H:%M:%S%z"),
    downstream_locked = downstream_locked
  )
  if (!is.null(shared)) obj$shared <- shared
  trilogy_write(project_path, obj)
}

#' Get the earth lock from a project's `trilogy.json`
#' @inheritParams trilogy_json_path
#' @return The `earth_lock` list (`list(locked = FALSE)` when none).
#' @export
trilogy_get_lock <- function(project_path) {
  lock <- trilogy_read(project_path)$earth_lock
  if (is.null(lock)) list(locked = FALSE) else lock
}

#' Is the earth model locked for this project?
#' @inheritParams trilogy_json_path
#' @return Logical scalar.
#' @export
trilogy_is_locked <- function(project_path) {
  isTRUE(trilogy_get_lock(project_path)$locked)
}

#' Clear the earth lock
#' @inheritParams trilogy_json_path
#' @return Invisibly, the path written.
#' @export
trilogy_clear_lock <- function(project_path) {
  obj <- trilogy_read(project_path)
  obj$earth_lock <- list(locked = FALSE)
  trilogy_write(project_path, obj)
}

#' Register a method's fit timestamp for the current Trilogy run
#'
#' Records the fit-stamp of `method`'s latest run so the combined report can
#' gather that method's files. earthUI's stamp is normally set by the lock.
#'
#' @inheritParams trilogy_json_path
#' @param method One of `"earth"`, `"glmnet"`, `"mgcv"`.
#' @param fit_ts Fit-stamp string (`"%Y%m%d_%H%M%S"`).
#' @return Invisibly, the path written.
#' @export
trilogy_register_fit <- function(project_path, method, fit_ts) {
  method <- match.arg(method, c("earth", "glmnet", "mgcv"))
  obj <- trilogy_read(project_path)
  if (is.null(obj$run)) obj$run <- list(methods = list(), fit_ts = list())
  if (is.null(obj$run$fit_ts)) obj$run$fit_ts <- list()
  obj$run$fit_ts[[method]] <- fit_ts
  trilogy_write(project_path, obj)
}

#' Write a method's value conclusion as `conclusion_<fit_ts>.json`
#'
#' Each app writes one of these into its own output folder so the combined
#' report can pick up the subject value + key metrics by fit timestamp. The
#' filename embeds `fit_ts` (matching the rest of the fit's outputs).
#'
#' @param out_dir Directory to write into (the method's output folder).
#' @param method One of `"earth"`, `"glmnet"`, `"mgcv"`.
#' @param fit_ts Fit-stamp string (`"%Y%m%d_%H%M%S"`).
#' @param subject_value The subject property's indicated value.
#' @param metrics Optional named list of fit metrics (e.g. `r2`, `rmse`).
#' @param extra Optional named list of extra fields to include.
#' @return Invisibly, the path written.
#' @export
trilogy_write_conclusion <- function(out_dir, method, fit_ts, subject_value,
                                     metrics = list(), extra = list()) {
  if (!requireNamespace("jsonlite", quietly = TRUE)) {
    stop("Package 'jsonlite' is required.", call. = FALSE)
  }
  method <- match.arg(method, c("earth", "glmnet", "mgcv"))
  sv <- suppressWarnings(as.numeric(subject_value))
  obj <- c(list(method = method, fit_ts = fit_ts,
                subject_value = if (length(sv) == 1L && !is.na(sv)) sv else NULL,
                metrics = metrics), extra)
  dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)
  p <- file.path(out_dir, paste0("conclusion_", fit_ts, ".json"))
  jsonlite::write_json(obj, p, pretty = TRUE, auto_unbox = TRUE, null = "null")
  invisible(p)
}

#' Basic in-sample fit metrics from actual + residual vectors (internal)
#'
#' Computes R-squared and RMSE for a conclusion.json `metrics` field. The caller
#' passes the comparables only (subject row excluded).
#'
#' @param actual,resid Numeric vectors (actual values and their residuals).
#' @return Named list with `r2` and `rmse` (each may be NULL when undefined).
#' @noRd
trilogy_fit_metrics_ <- function(actual, resid) {
  actual <- suppressWarnings(as.numeric(actual))
  resid  <- suppressWarnings(as.numeric(resid))
  ok <- is.finite(actual) & is.finite(resid)
  actual <- actual[ok]; resid <- resid[ok]
  if (length(resid) < 2L) return(list())
  ss_tot <- sum((actual - mean(actual))^2)
  list(
    r2   = if (ss_tot > 0) round(1 - sum(resid^2) / ss_tot, 4) else NULL,
    rmse = round(sqrt(mean(resid^2)), 1))
}
