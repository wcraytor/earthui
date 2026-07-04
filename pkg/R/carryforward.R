# Cross-app carry-forward (absorbed from valengrCore 2026-07-03): fields earthUI saves for a project that the sibling
# apps (glmnetUI, mgcvUI) read so the same values flow through. earthUI is the
# source of truth and persists them in the shared regProj projects DB under
# method = "earth".

#' Read carry-forward fields earthUI saved for a project
#'
#' earthUI is the source of truth for the project-level Effective Date, the
#' Response/target, and the RCA subject-CQA type + value across the sibling
#' apps; it persists them in the shared regProj projects DB (under
#' \code{method = "earth"}). glmnetUI and mgcvUI read them so the same values
#' flow through.
#'
#' @param project_path Full project path (e.g.
#'   \code{<root>/<purpose>/<flat>}). The regProj root is derived from this
#'   full path, never from a per-app preference or home-dir default.
#' @param purpose One of \code{"general"}, \code{"appraisal"}, \code{"market"}.
#' @param root regProj root holding \code{projects.sqlite}. Defaults to the
#'   root derived from \code{project_path} (its grandparent dir). Supply
#'   explicitly only to override.
#' @return A list with \code{effective_date} (a \code{Date} or \code{NULL}),
#'   \code{response} (character scalar or \code{NULL}), \code{cqa_type}
#'   (\code{"cqa"} / \code{"cqa_sf"} or \code{NULL}), \code{cqa_value}
#'   (numeric scalar or \code{NULL}), and \code{living_area} (the column
#'   earthUI tagged as the \code{living_area} special, or \code{NULL}).
#' @export
earth_carryforward_ <- function(project_path, purpose = "general", root = NULL) {
  out <- list(effective_date = NULL, response = NULL,
              cqa_type = NULL, cqa_value = NULL, living_area = NULL)
  if (is.null(project_path) || !nzchar(project_path)) return(out)
  # Always resolve from the FULL project path -- never a per-app preference or
  # a home-dir default, which can point at the wrong place.
  if (is.null(root)) root <- dirname(dirname(path.expand(project_path)))
  if (!file.exists(file.path(root, "projects.sqlite"))) return(out)
  s <- tryCatch(
    get_project_settings(project_path, method = "earth", purpose = purpose,
                         root = root),
    error = function(e) NULL)
  if (is.null(s) || is.null(s$settings)) return(out)
  cfg <- tryCatch(jsonlite::fromJSON(s$settings), error = function(e) NULL)
  if (is.null(cfg)) return(out)

  if (!is.null(cfg$effective_date)) {
    d <- suppressWarnings(as.Date(as.character(cfg$effective_date)[1L]))
    if (!is.na(d)) out$effective_date <- d
  }
  if (!is.null(cfg$target) && length(cfg$target) >= 1L)
    out$response <- as.character(cfg$target)[1L]
  if (!is.null(cfg$cqa_type) && length(cfg$cqa_type) >= 1L)
    out$cqa_type <- as.character(cfg$cqa_type)[1L]
  if (!is.null(cfg$cqa_value) && length(cfg$cqa_value) >= 1L) {
    v <- suppressWarnings(as.numeric(cfg$cqa_value)[1L])
    if (!is.na(v)) out$cqa_value <- v
  }

  # living_area column comes from the per-variable special designations
  # (earthUI's `variables` blob), so glmnetUI/mgcvUI can offer "CQA per SF"
  # without the analyst re-tagging it.
  if (!is.null(s$variables)) {
    vars <- tryCatch(jsonlite::fromJSON(s$variables, simplifyVector = FALSE),
                     error = function(e) NULL)
    if (is.list(vars)) {
      for (nm in names(vars)) {
        if (identical(vars[[nm]]$special, "living_area")) {
          out$living_area <- nm; break
        }
      }
    }
  }
  out
}
