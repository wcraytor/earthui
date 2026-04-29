# SQLite backends for regProj geo codes and project settings.
# Both DBs live inside <REGPROJ_ROOT>/ so they sync with the project tree.


# ---------- geo.sqlite ----------

#' Path to the regProj geo SQLite database
#'
#' `<REGPROJ_ROOT>/geo.sqlite` — holds country codes and a flexible,
#' variable-depth `admin_entries` table for state / county / city /
#' deeper-level admin codes per country. Travels with the regProj tree.
#'
#' @param root regProj root. Defaults to [default_regproj_root()].
#' @return Character scalar.
#' @export
regproj_geo_db_path <- function(root = default_regproj_root()) {
  file.path(path.expand(root), "geo.sqlite")
}

#' Open (and initialize if needed) the regProj geo database
#'
#' Returns a `DBI` connection. Creates the schema on first call. The
#' caller is responsible for `DBI::dbDisconnect()`.
#'
#' On first creation, the tables are seeded from:
#' * the shipped reference data ([regproj_reference()]) — 24 countries,
#'   51 US states, 3,076 US counties.
#' * the shipped places data (`pkg/inst/extdata/regproj_geo.rds`) — US
#'   incorporated places + GeoNames-derived city/admin data for GB, DE,
#'   IT, FR, SE, SG.
#' * any pre-existing `<REGPROJ_ROOT>/.regproj-index.json` (legacy
#'   migration).
#'
#' @inheritParams regproj_geo_db_path
#' @return A `DBIConnection` to the SQLite database.
#' @export
regproj_geo_db_connect <- function(root = default_regproj_root()) {
  if (!requireNamespace("DBI", quietly = TRUE) ||
      !requireNamespace("RSQLite", quietly = TRUE)) {
    stop("Packages 'DBI' and 'RSQLite' are required.", call. = FALSE)
  }
  root <- path.expand(root)
  if (!dir.exists(root)) dir.create(root, recursive = TRUE, showWarnings = FALSE)
  p <- regproj_geo_db_path(root)
  fresh <- !file.exists(p)
  con <- DBI::dbConnect(RSQLite::SQLite(), p)
  DBI::dbExecute(con, "PRAGMA foreign_keys = ON;")
  regproj_geo_db_init_(con)
  if (fresh) regproj_geo_db_seed_(con, root)
  con
}

# Internal: create tables if missing.
# Two tables only:
#   countries   — code/name, top-level (no parent)
#   admin_entries — variable-depth admin levels per country
regproj_geo_db_init_ <- function(con) {
  DBI::dbExecute(con, "
    CREATE TABLE IF NOT EXISTS countries (
      code TEXT PRIMARY KEY,
      name TEXT NOT NULL
    );")
  DBI::dbExecute(con, "
    CREATE TABLE IF NOT EXISTS admin_entries (
      country      TEXT NOT NULL,
      level        INTEGER NOT NULL,        -- 1 = top admin (state/regione/...)
      parent_codes TEXT NOT NULL,            -- '' for level 1, 'ca' for level 2, 'ca/081' for level 3
      code         TEXT NOT NULL,
      name         TEXT NOT NULL,
      PRIMARY KEY (country, level, parent_codes, code)
    );")
  DBI::dbExecute(con, "
    CREATE INDEX IF NOT EXISTS idx_admin_lookup
      ON admin_entries (country, level, parent_codes);")
  invisible(NULL)
}

# Internal: bootstrap from shipped reference data + shipped places +
# legacy JSON index.
regproj_geo_db_seed_ <- function(con, root = default_regproj_root()) {
  ref <- tryCatch(regproj_reference(), error = function(e) NULL)
  DBI::dbWithTransaction(con, {
    if (!is.null(ref)) {
      # countries
      if (length(ref$countries) > 0L) {
        df <- data.frame(code = names(ref$countries),
                         name = unname(unlist(ref$countries)),
                         stringsAsFactors = FALSE)
        DBI::dbWriteTable(con, "countries", df, append = TRUE)
      }
      # US states (level 1)
      us_states <- ref$states$us
      if (!is.null(us_states) && length(us_states) > 0L) {
        df <- data.frame(country = "us", level = 1L, parent_codes = "",
                         code = names(us_states),
                         name = unname(unlist(us_states)),
                         stringsAsFactors = FALSE)
        DBI::dbWriteTable(con, "admin_entries", df, append = TRUE)
      }
      # US counties (level 2)
      us_counties <- ref$counties$us
      if (!is.null(us_counties) && length(us_counties) > 0L) {
        rows <- list()
        for (sc in names(us_counties)) {
          cl <- us_counties[[sc]]; if (length(cl) == 0L) next
          rows[[length(rows) + 1L]] <- data.frame(
            country = "us", level = 2L, parent_codes = sc,
            code = names(cl), name = unname(unlist(cl)),
            stringsAsFactors = FALSE)
        }
        if (length(rows) > 0L)
          DBI::dbWriteTable(con, "admin_entries",
                            do.call(rbind, rows), append = TRUE)
      }
    }
    # Shipped places (US cities + GeoNames international data)
    places_path <- system.file("extdata", "regproj_geo.rds",
                               package = "earthUI")
    if (nzchar(places_path) && file.exists(places_path)) {
      places <- tryCatch(readRDS(places_path), error = function(e) NULL)
      if (!is.null(places) && nrow(places) > 0L) {
        DBI::dbWriteTable(con, "admin_entries", places, append = TRUE)
      }
    }
    # One-shot migration: import any pre-existing JSON index (overrides
    # shipped entries on conflict, since user choices win)
    json_path <- file.path(path.expand(root), ".regproj-index.json")
    if (file.exists(json_path) &&
        requireNamespace("jsonlite", quietly = TRUE)) {
      idx <- tryCatch(jsonlite::fromJSON(json_path, simplifyVector = FALSE),
                      error = function(e) NULL)
      if (!is.null(idx)) regproj_geo_db_import_json_(con, idx)
    }
  })
  invisible(NULL)
}

# Internal: import legacy JSON into admin_entries
regproj_geo_db_import_json_ <- function(con, idx) {
  for (scope in names(idx)) {
    entries <- idx[[scope]]
    if (length(entries) == 0L) next
    if (!nzchar(scope)) {
      # countries
      for (nm in names(entries)) {
        DBI::dbExecute(con,
          "INSERT OR IGNORE INTO countries (code, name) VALUES (?, ?)",
          params = list(entries[[nm]], nm))
      }
      next
    }
    parts <- strsplit(scope, "/", fixed = TRUE)[[1L]]
    cc <- parts[1L]
    level <- length(parts)              # parts of length 1 -> level 1, etc.
    parent_codes <- if (length(parts) > 1L) {
                      paste(parts[-1L], collapse = "/")
                    } else ""
    for (nm in names(entries)) {
      DBI::dbExecute(con,
        "INSERT OR REPLACE INTO admin_entries (country, level, parent_codes, code, name) VALUES (?, ?, ?, ?, ?)",
        params = list(cc, level, parent_codes, entries[[nm]], nm))
    }
  }
  invisible(NULL)
}

# Internal: parse a legacy scope string ("us/ca/081") into
# (country, level, parent_codes) for the new schema.
# Returns NULL for "" (countries — handled separately).
regproj_geo_parse_scope_ <- function(scope) {
  if (is.null(scope) || !nzchar(scope)) return(NULL)
  parts <- strsplit(scope, "/", fixed = TRUE)[[1L]]
  cc <- parts[1L]
  level <- length(parts)               # "us" -> level 1, "us/ca" -> level 2, etc.
  parent_codes <- if (length(parts) > 1L) {
                    paste(parts[-1L], collapse = "/")
                  } else ""
  list(country = cc, level = level, parent_codes = parent_codes)
}


# ---------- projects.sqlite (unchanged) ----------

#' Path to the regProj projects SQLite database
#'
#' `<REGPROJ_ROOT>/projects.sqlite` — one row per project, keyed by the
#' flat segment (e.g. `"us_ca_081_burlin_20251231_j"`). Holds project
#' metadata and per-method settings as JSON blobs.
#'
#' @inheritParams regproj_geo_db_path
#' @return Character scalar.
#' @export
regproj_projects_db_path <- function(root = default_regproj_root()) {
  file.path(path.expand(root), "projects.sqlite")
}

#' Open (and initialize if needed) the regProj projects database
#'
#' Returns a `DBI` connection. Creates the schema on first call. The
#' caller is responsible for `DBI::dbDisconnect()`.
#'
#' @inheritParams regproj_geo_db_path
#' @return A `DBIConnection` to the SQLite database.
#' @export
regproj_projects_db_connect <- function(root = default_regproj_root()) {
  if (!requireNamespace("DBI", quietly = TRUE) ||
      !requireNamespace("RSQLite", quietly = TRUE)) {
    stop("Packages 'DBI' and 'RSQLite' are required.", call. = FALSE)
  }
  root <- path.expand(root)
  if (!dir.exists(root)) dir.create(root, recursive = TRUE, showWarnings = FALSE)
  p <- regproj_projects_db_path(root)
  con <- DBI::dbConnect(RSQLite::SQLite(), p)
  DBI::dbExecute(con, "PRAGMA foreign_keys = ON;")
  regproj_projects_db_init_(con)
  con
}

regproj_projects_db_init_ <- function(con) {
  DBI::dbExecute(con, "
    CREATE TABLE IF NOT EXISTS projects (
      flat_segment      TEXT PRIMARY KEY,
      purpose           TEXT NOT NULL,
      country           TEXT NOT NULL,
      state             TEXT,
      county            TEXT,
      city              TEXT,
      project_name      TEXT NOT NULL,
      created_at        INTEGER,
      last_used_at      INTEGER,
      last_file         TEXT
    );")
  # Per-file settings within a project. A project can hold multiple input
  # files (different cleanings / variants); each gets its own row.
  DBI::dbExecute(con, "
    CREATE TABLE IF NOT EXISTS project_file_settings (
      flat_segment       TEXT NOT NULL,
      file_basename      TEXT NOT NULL,
      earth_settings     TEXT,
      earth_variables    TEXT,
      earth_interactions TEXT,
      glmnet_settings    TEXT,
      glmnet_variables   TEXT,
      mgcv_settings      TEXT,
      mgcv_variables     TEXT,
      updated_at         INTEGER,
      PRIMARY KEY (flat_segment, file_basename)
    );")
  invisible(NULL)
}

# ---------- per-file settings CRUD (internal) ----------

project_file_settings_read_ <- function(con, flat_segment, file_basename,
                                         method = "earth") {
  row <- DBI::dbGetQuery(con,
    sprintf("SELECT %s_settings, %s_variables%s
              FROM project_file_settings
              WHERE flat_segment = ? AND file_basename = ?",
            method, method,
            if (method == "earth") ", earth_interactions" else ""),
    params = list(flat_segment, file_basename))
  if (nrow(row) == 0L) return(NULL)
  vals <- list(
    settings  = row[[paste0(method, "_settings")]][1L],
    variables = row[[paste0(method, "_variables")]][1L]
  )
  if (method == "earth") {
    vals$interactions <- row$earth_interactions[1L]
  }
  # If every method-specific column is NA/empty, the row was written by
  # a different method and has no data for this one — treat as absent.
  is_empty <- function(x) is.null(x) || is.na(x) || !nzchar(x)
  if (all(vapply(vals, is_empty, logical(1)))) return(NULL)
  # NA → NULL for any individually missing field
  vals[vapply(vals, is_empty, logical(1))] <- list(NULL)
  vals
}

project_file_settings_write_ <- function(con, flat_segment, file_basename,
                                          settings = NULL, variables = NULL,
                                          interactions = NULL,
                                          method = "earth") {
  cols <- c(paste0(method, "_settings"), paste0(method, "_variables"))
  vals <- list(settings, variables)
  if (method == "earth") {
    cols <- c(cols, "earth_interactions")
    vals <- c(vals, list(interactions))
  }
  ts <- as.integer(Sys.time())
  # UPSERT: insert if missing, update the relevant cols if present
  set_clause <- paste(paste0(cols, " = ?"), collapse = ", ")
  insert_cols <- c("flat_segment", "file_basename", cols, "updated_at")
  insert_vals <- c(list(flat_segment, file_basename), vals, list(ts))
  placeholders <- paste(rep("?", length(insert_cols)), collapse = ", ")
  DBI::dbExecute(con,
    sprintf("INSERT INTO project_file_settings (%s) VALUES (%s)
              ON CONFLICT (flat_segment, file_basename)
              DO UPDATE SET %s, updated_at = excluded.updated_at",
            paste(insert_cols, collapse = ", "),
            placeholders, set_clause),
    params = c(insert_vals, vals))
  invisible(NULL)
}


# ---------- public API for ValEngr / external callers ----------

#' Get per-file model settings for a project
#'
#' Reads model settings for a specific file within a project from
#' `<REGPROJ_ROOT>/projects.sqlite`. Used by the Shiny UI to restore
#' settings on file open, and by external tools (ValEngr, batch scripts)
#' to inspect project state.
#'
#' @param project_path Absolute path to the project root.
#' @param file_basename The data file basename (e.g. `"data.csv"`).
#' @param method One of `"earth"`, `"glmnet"`, `"mgcv"`. Default `"earth"`.
#' @param root regProj root. Defaults to [default_regproj_root()].
#' @return Named list with `settings`, `variables`, and (for `earth`)
#'   `interactions` (each a JSON string), or `NULL` if no row exists.
#' @export
get_project_settings <- function(project_path, file_basename,
                                 method = "earth",
                                 root = default_regproj_root()) {
  flat <- basename(path.expand(project_path))
  con <- regproj_projects_db_connect(root)
  on.exit(DBI::dbDisconnect(con), add = TRUE)
  project_file_settings_read_(con, flat, file_basename, method = method)
}

#' Set per-file model settings for a project
#'
#' Writes model settings for a specific file within a project to
#' `<REGPROJ_ROOT>/projects.sqlite`. Used by the Shiny UI to persist
#' settings on every change, and by external tools to seed projects
#' programmatically.
#'
#' @inheritParams get_project_settings
#' @param settings,variables,interactions JSON strings (or `NULL` to
#'   leave unchanged).
#' @return Invisibly, `NULL`.
#' @export
set_project_settings <- function(project_path, file_basename,
                                 settings = NULL, variables = NULL,
                                 interactions = NULL,
                                 method = "earth",
                                 root = default_regproj_root()) {
  flat <- basename(path.expand(project_path))
  con <- regproj_projects_db_connect(root)
  on.exit(DBI::dbDisconnect(con), add = TRUE)
  project_file_settings_write_(con, flat, file_basename,
                                settings = settings, variables = variables,
                                interactions = interactions, method = method)
}
