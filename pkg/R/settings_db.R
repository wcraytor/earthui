# Internal SQLite-backed settings persistence
#
# Stores per-file model settings, variable selections, and interaction matrices
# in a SQLite database at tools::R_user_dir("earthui", "data")/settings.sqlite.
# Falls back gracefully to localStorage-only if DBI/RSQLite are not installed.

# --- Path to the settings database file ---
settings_db_path_ <- function() {
  dir <- tools::R_user_dir("earthui", "data")
  if (!dir.exists(dir)) dir.create(dir, recursive = TRUE)
  file.path(dir, "settings.sqlite")
}

# --- Open connection and ensure schema exists ---
settings_db_connect_ <- function() {
  if (!requireNamespace("DBI", quietly = TRUE) ||
      !requireNamespace("RSQLite", quietly = TRUE)) {
    message("earthui: DBI/RSQLite not available, settings persistence disabled")
    return(NULL)
  }

  db_path <- settings_db_path_()
  con <- DBI::dbConnect(RSQLite::SQLite(), db_path)

  DBI::dbExecute(con, "
    CREATE TABLE IF NOT EXISTS file_settings (
      filename     TEXT PRIMARY KEY,
      settings     TEXT NOT NULL DEFAULT '{}',
      variables    TEXT NOT NULL DEFAULT '{}',
      interactions TEXT NOT NULL DEFAULT '{}',
      accessed_at  TEXT NOT NULL DEFAULT (strftime('%Y-%m-%dT%H:%M:%f','now'))
    )
  ")
  DBI::dbExecute(con, "
    CREATE INDEX IF NOT EXISTS idx_accessed ON file_settings(accessed_at)
  ")

  con
}

# --- Read all settings for a filename ---
# Returns list(settings, variables, interactions) or NULL if not found.
settings_db_read_ <- function(con, filename) {
  if (is.null(con)) return(NULL)

  row <- DBI::dbGetQuery(con,
    "SELECT settings, variables, interactions FROM file_settings WHERE filename = ?",
    params = list(filename)
  )
  if (nrow(row) == 0L) return(NULL)

  # Touch accessed_at for LRU ordering
  DBI::dbExecute(con,
    "UPDATE file_settings SET accessed_at = strftime('%Y-%m-%dT%H:%M:%f','now') WHERE filename = ?",
    params = list(filename)
  )

  list(
    settings     = jsonlite::fromJSON(row$settings[1L],     simplifyVector = FALSE),
    variables    = jsonlite::fromJSON(row$variables[1L],    simplifyVector = FALSE),
    interactions = jsonlite::fromJSON(row$interactions[1L], simplifyVector = FALSE)
  )
}

# --- Write (upsert) settings for a filename, then evict old entries ---
settings_db_write_ <- function(con, filename,
                                settings = "{}",
                                variables = "{}",
                                interactions = "{}") {
  if (is.null(con)) return(invisible(NULL))

  DBI::dbExecute(con, "
    INSERT INTO file_settings (filename, settings, variables, interactions, accessed_at)
    VALUES (?, ?, ?, ?, strftime('%Y-%m-%dT%H:%M:%f','now'))
    ON CONFLICT(filename) DO UPDATE SET
      settings     = excluded.settings,
      variables    = excluded.variables,
      interactions = excluded.interactions,
      accessed_at  = excluded.accessed_at
  ", params = list(filename, settings, variables, interactions))

  settings_db_evict_(con, max_files = 100L)
  invisible(NULL)
}

# --- LRU eviction: keep only the most recent max_files entries ---
settings_db_evict_ <- function(con, max_files = 100L) {
  if (is.null(con)) return(invisible(NULL))

  count <- DBI::dbGetQuery(con,
    "SELECT COUNT(*) AS n FROM file_settings"
  )$n
  if (count > max_files) {
    DBI::dbExecute(con, "
      DELETE FROM file_settings WHERE filename NOT IN (
        SELECT filename FROM file_settings
        ORDER BY accessed_at DESC
        LIMIT ?
      )
    ", params = list(max_files))
  }
  invisible(NULL)
}

# --- Safe disconnect ---
settings_db_disconnect_ <- function(con) {
  if (!is.null(con) && DBI::dbIsValid(con)) {
    DBI::dbDisconnect(con)
  }
  invisible(NULL)
}
