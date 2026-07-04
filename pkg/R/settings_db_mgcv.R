#' Settings Persistence (SQLite, keyed by input file name)
#'
#' Stores and retrieves per-file variable configuration so that
#' settings are remembered when the same data file is re-opened.
#'
#' @name settings_db
#' @return No return value; this page documents the internal
#'   settings-persistence helpers.
#' @keywords internal
NULL


#' Path to the settings database
#' @return Character path.
#' @noRd
settings_db_path__mgcv <- function() {
  # Allow tests to redirect to a temp DB
  override <- getOption("mgcvUI.settings_db_path")
  if (!is.null(override)) return(override)
  dir <- tools::R_user_dir("earthUI", "data")
  if (!dir.exists(dir)) {
    tryCatch(dir.create(dir, recursive = TRUE), error = function(e) {
      message("mgcvUI: cannot create settings directory: ", e$message)
    })
  }
  file.path(dir, "settings.sqlite")
}


#' Open (or create) the settings database
#' @return A DBI connection, or NULL if RSQLite not available.
#' @noRd
settings_db_connect__mgcv <- function() {
  if (!requireNamespace("RSQLite", quietly = TRUE)) return(NULL)
  if (!requireNamespace("DBI", quietly = TRUE)) return(NULL)

  con <- tryCatch(
    DBI::dbConnect(RSQLite::SQLite(), settings_db_path__mgcv()),
    error = function(e) {
      message("mgcvUI: settings database unavailable (", e$message,
              "). Settings will not persist.")
      NULL
    }
  )
  if (is.null(con)) return(NULL)
  DBI::dbExecute(con, "
    CREATE TABLE IF NOT EXISTS settings_v2 (
      filename    TEXT PRIMARY KEY,
      response    TEXT,
      variables   TEXT,
      family      TEXT DEFAULT 'gaussian',
      method      TEXT DEFAULT 'REML',
      sel         INTEGER DEFAULT 0,
      gamma       REAL DEFAULT 1.0,
      updated_at  TEXT DEFAULT (datetime('now'))
    )
  ")

  # Add columns introduced after initial schema
  existing <- DBI::dbListFields(con, "settings_v2")
  if (!"output_folder" %in% existing) {
    DBI::dbExecute(con, "ALTER TABLE settings_v2 ADD COLUMN output_folder TEXT DEFAULT ''")
  }
  if (!"effective_date" %in% existing) {
    DBI::dbExecute(con, "ALTER TABLE settings_v2 ADD COLUMN effective_date TEXT DEFAULT ''")
  }
  if (!"purpose" %in% existing) {
    DBI::dbExecute(con, "ALTER TABLE settings_v2 ADD COLUMN purpose TEXT DEFAULT 'general'")
  }
  if (!"weights_col" %in% existing) {
    DBI::dbExecute(con, "ALTER TABLE settings_v2 ADD COLUMN weights_col TEXT DEFAULT ''")
  }
  if (!"response_transform" %in% existing) {
    DBI::dbExecute(con, "ALTER TABLE settings_v2 ADD COLUMN response_transform TEXT DEFAULT 'none'")
  }
  if (!"cv" %in% existing) {
    DBI::dbExecute(con, "ALTER TABLE settings_v2 ADD COLUMN cv INTEGER DEFAULT 1")
  }
  if (!"default_basis" %in% existing) {
    DBI::dbExecute(con, "ALTER TABLE settings_v2 ADD COLUMN default_basis TEXT DEFAULT 'tp'")
  }
  if (!"default_k" %in% existing) {
    DBI::dbExecute(con, "ALTER TABLE settings_v2 ADD COLUMN default_k INTEGER DEFAULT 0")
  }
  if (!"tensor_type" %in% existing) {
    DBI::dbExecute(con, "ALTER TABLE settings_v2 ADD COLUMN tensor_type TEXT DEFAULT 'ti'")
  }
  if (!"optimizer" %in% existing) {
    DBI::dbExecute(con, "ALTER TABLE settings_v2 ADD COLUMN optimizer TEXT DEFAULT 'outer_newton'")
  }
  if (!"scale" %in% existing) {
    DBI::dbExecute(con, "ALTER TABLE settings_v2 ADD COLUMN scale REAL DEFAULT 0")
  }
  if (!"discrete" %in% existing) {
    DBI::dbExecute(con, "ALTER TABLE settings_v2 ADD COLUMN discrete INTEGER DEFAULT 0")
  }
  if (!"nthreads" %in% existing) {
    DBI::dbExecute(con, "ALTER TABLE settings_v2 ADD COLUMN nthreads INTEGER DEFAULT 1")
  }
  con
}


#' Save Settings for a File
#'
#' @param filename Character -- the original file name (basename).
#' @param config List with response, variables (named list of per-var
#'   settings), family, method, select, gamma.
#' @noRd
settings_db_write__mgcv <- function(filename, config) {
  con <- settings_db_connect__mgcv()
  if (is.null(con)) return(invisible(NULL))
  on.exit(DBI::dbDisconnect(con), add = TRUE)

  vars_json <- jsonlite_encode_(config$variables)

  DBI::dbExecute(con, "
    INSERT INTO settings_v2 (filename, response, variables,
                             family, method, sel, gamma,
                             output_folder, effective_date, purpose,
                             weights_col, response_transform,
                             cv, default_basis, default_k,
                             tensor_type, optimizer, scale,
                             discrete, nthreads, updated_at)
    VALUES (?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, datetime('now'))
    ON CONFLICT(filename) DO UPDATE SET
      response           = excluded.response,
      variables          = excluded.variables,
      family             = excluded.family,
      method             = excluded.method,
      sel                = excluded.sel,
      gamma              = excluded.gamma,
      output_folder      = excluded.output_folder,
      effective_date     = excluded.effective_date,
      purpose            = excluded.purpose,
      weights_col        = excluded.weights_col,
      response_transform = excluded.response_transform,
      cv                 = excluded.cv,
      default_basis      = excluded.default_basis,
      default_k          = excluded.default_k,
      tensor_type        = excluded.tensor_type,
      optimizer          = excluded.optimizer,
      scale              = excluded.scale,
      discrete           = excluded.discrete,
      nthreads           = excluded.nthreads,
      updated_at         = excluded.updated_at
  ", params = list(filename,
                   config$response %||% "",
                   vars_json,
                   config$family %||% "gaussian",
                   config$method %||% "REML",
                   as.integer(config$select %||% FALSE),
                   config$gamma %||% 1,
                   config$output_folder %||% "",
                   config$effective_date %||% "",
                   config$purpose %||% "general",
                   config$weights_col %||% "",
                   config$response_transform %||% "none",
                   as.integer(config$cv %||% TRUE),
                   config$default_basis %||% "tp",
                   as.integer(config$default_k %||% 0),
                   config$tensor_type %||% "ti",
                   config$optimizer %||% "outer_newton",
                   config$scale %||% 0,
                   as.integer(config$discrete %||% FALSE),
                   as.integer(config$nthreads %||% 1)))

  settings_db_evict__mgcv(con, max_files = 100L)
  invisible(NULL)
}


#' Load Settings for a File
#'
#' @param filename Character -- the original file name (basename).
#' @return A list with the saved config, or NULL if not found.
#' @noRd
settings_db_read__mgcv <- function(filename) {
  con <- settings_db_connect__mgcv()
  if (is.null(con)) return(NULL)
  on.exit(DBI::dbDisconnect(con), add = TRUE)

  row <- DBI::dbGetQuery(con,
    "SELECT * FROM settings_v2 WHERE filename = ? LIMIT 1",
    params = list(filename)
  )

  if (nrow(row) == 0L) return(NULL)

  variables <- jsonlite_decode_(row$variables)

  safe_field <- function(field, default = NULL) {
    if (field %in% names(row) && !is.na(row[[field]]) && nzchar(row[[field]])) {
      row[[field]]
    } else {
      default
    }
  }

  list(
    response           = if (nzchar(row$response)) row$response else NULL,
    variables          = variables,
    family             = row$family,
    method             = row$method,
    select             = as.logical(row$sel),
    gamma              = row$gamma,
    output_folder      = safe_field("output_folder"),
    effective_date     = safe_field("effective_date"),
    purpose            = safe_field("purpose"),
    weights_col        = safe_field("weights_col"),
    response_transform = safe_field("response_transform", "none"),
    cv                 = if ("cv" %in% names(row)) as.logical(row$cv) else TRUE,
    default_basis      = safe_field("default_basis", "tp"),
    default_k          = if ("default_k" %in% names(row)) as.integer(row$default_k) else 0L,
    tensor_type        = safe_field("tensor_type", "ti"),
    optimizer          = safe_field("optimizer", "outer_newton"),
    scale              = if ("scale" %in% names(row)) row$scale else 0,
    discrete           = if ("discrete" %in% names(row)) as.logical(row$discrete) else FALSE,
    nthreads           = if ("nthreads" %in% names(row)) as.integer(row$nthreads) else 1L
  )
}


#' Evict old entries beyond max_files
#' @param con A DBI connection.
#' @param max_files Integer.
#' @noRd
settings_db_evict__mgcv <- function(con, max_files = 100L) {
  n <- DBI::dbGetQuery(con, "SELECT COUNT(*) AS n FROM settings_v2")$n
  if (n > max_files) {
    DBI::dbExecute(con, "
      DELETE FROM settings_v2 WHERE filename NOT IN (
        SELECT filename FROM settings_v2 ORDER BY updated_at DESC LIMIT ?
      )
    ", params = list(max_files))
  }
}


#' Save locale defaults (not per-file, global user preference)
#' @param locale_settings Named list with locale_country, locale_paper, etc.
#' @noRd
settings_db_write_locale__mgcv <- function(locale_settings) {
  con <- settings_db_connect__mgcv()
  if (is.null(con)) return(invisible(NULL))
  on.exit(DBI::dbDisconnect(con), add = TRUE)

  vars_json <- jsonlite_encode_(locale_settings)

  DBI::dbExecute(con, "
    INSERT INTO settings_v2 (filename, variables, updated_at)
    VALUES ('__locale_defaults__', ?, datetime('now'))
    ON CONFLICT(filename) DO UPDATE SET
      variables  = excluded.variables,
      updated_at = excluded.updated_at
  ", params = list(vars_json))
  invisible(NULL)
}


#' Load locale defaults
#' @return Named list with locale settings, or NULL if not found.
#' @noRd
settings_db_read_locale__mgcv <- function() {
  con <- settings_db_connect__mgcv()
  if (is.null(con)) return(NULL)
  on.exit(DBI::dbDisconnect(con), add = TRUE)

  row <- DBI::dbGetQuery(con,
    "SELECT variables FROM settings_v2 WHERE filename = '__locale_defaults__' LIMIT 1"
  )
  if (nrow(row) == 0L) return(NULL)
  jsonlite_decode_(row$variables)
}


#' Minimal JSON encode
#' @param x An R object.
#' @return Character JSON string.
#' @noRd
jsonlite_encode_ <- function(x) {
  if (is.null(x) || length(x) == 0L) return("{}")
  if (requireNamespace("jsonlite", quietly = TRUE)) {
    return(jsonlite::toJSON(x, auto_unbox = TRUE))
  }
  deparse(x, control = "all")
}


#' Minimal JSON decode
#' @param json Character JSON string.
#' @return An R list.
#' @noRd
jsonlite_decode_ <- function(json) {
  if (is.null(json) || !nzchar(json) || json %in% c("{}", "[]")) {
    return(list())
  }
  if (requireNamespace("jsonlite", quietly = TRUE)) {
    tryCatch(
      jsonlite::fromJSON(json, simplifyVector = FALSE),
      error = function(e) list()
    )
  } else {
    tryCatch(eval(parse(text = json)), error = function(e) list())
  }
}
