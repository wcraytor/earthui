# Country admin schemas (variable depth per country) ---------------------

# Each entry: ordered character vector of admin level labels, top to bottom.
# These labels drive the UI cascade (one dropdown per level) and serve as
# documentation of what a `levels` value in a JSON or path actually means.
# Empty character vector = country has no admin subdivisions in the package's
# default schema (e.g., a city-state); user can still add levels manually.

country_schemas_ <- list(
  us = c("state", "county", "city"),
  gb = c("region", "district", "place"),
  fr = c("region", "departement", "commune"),
  de = c("bundesland", "kreis", "gemeinde"),
  it = c("regione", "provincia", "comune"),
  es = c("comunidad", "provincia", "municipio"),
  nl = c("provincie", "gemeente", "buurt"),
  be = c("region", "provincie", "gemeente"),
  at = c("bundesland", "bezirk", "gemeinde"),
  ch = c("kanton", "bezirk", "gemeinde"),
  se = c("lan", "kommun", "stadsdel"),
  no = c("fylke", "kommune", "bydel"),
  dk = c("region", "kommune", "sogn"),
  fi = c("maakunta", "kunta", "kaupunginosa"),
  pl = c("wojewodztwo", "powiat", "gmina"),
  ie = c("province", "county", "town"),
  pt = c("distrito", "concelho", "freguesia"),
  jp = c("prefecture", "city_or_ward", "district"),
  br = c("estado", "municipio", "bairro"),
  ar = c("provincia", "departamento", "municipio"),
  cl = c("region", "provincia", "comuna"),
  co = c("departamento", "municipio", "barrio"),
  pe = c("departamento", "provincia", "distrito"),
  uy = c("departamento", "municipio", "barrio"),
  sg = c("planning_area")
)

# Country display names (English, for the UI dropdown)
country_names_ <- c(
  us = "United States",
  gb = "United Kingdom",
  fr = "France",
  de = "Germany",
  it = "Italy",
  es = "Spain",
  nl = "Netherlands",
  be = "Belgium",
  at = "Austria",
  ch = "Switzerland",
  se = "Sweden",
  no = "Norway",
  dk = "Denmark",
  fi = "Finland",
  pl = "Poland",
  ie = "Ireland",
  pt = "Portugal",
  jp = "Japan",
  br = "Brazil",
  ar = "Argentina",
  cl = "Chile",
  co = "Colombia",
  pe = "Peru",
  uy = "Uruguay",
  sg = "Singapore"
)


# Public helpers ----------------------------------------------------------

#' Get the admin-level schema for a country
#'
#' Returns the ordered character vector of admin level labels for the
#' given ISO 3166-1 alpha-2 country code. Used by the UI cascade and by
#' [regproj_path()] to validate path depth.
#'
#' Unknown country codes return a generic 2-level fallback
#' (`c("region", "city")`) so users in countries not yet covered by the
#' shipped table still get a sensible cascade.
#'
#' @param cc Character scalar. Lowercase ISO 3166-1 alpha-2 country code
#'   (e.g. `"us"`, `"jp"`, `"it"`).
#' @return Character vector of admin level labels, top to bottom.
#' @export
country_schema <- function(cc) {
  cc <- tolower(as.character(cc))
  if (cc %in% names(country_schemas_)) country_schemas_[[cc]]
  else c("region", "city")
}

#' List all countries with shipped admin schemas
#'
#' Returns a named character vector of country display names indexed by
#' ISO 3166-1 alpha-2 code. Suitable for a `selectInput` choices list.
#'
#' @return Named character vector. Names are display names, values are
#'   lowercase 2-letter codes.
#' @export
country_choices <- function() {
  setNames(names(country_names_), unname(country_names_))
}

#' Path to the per-user earthUI preferences file
#'
#' Returns the path to `<R_user_dir("earthUI","config")>/prefs.json`. The
#' file holds user-level configuration that lives outside the regProj
#' tree itself — most importantly, the location of the regProj root.
#'
#' @return Character scalar.
#' @export
earthui_prefs_path <- function() {
  file.path(tools::R_user_dir("earthUI", "config"), "prefs.json")
}

#' Read user preferences (returns empty list if file missing)
#' @return Named list.
#' @export
earthui_prefs_read <- function() {
  p <- earthui_prefs_path()
  if (!file.exists(p)) return(list())
  if (!requireNamespace("jsonlite", quietly = TRUE)) return(list())
  tryCatch(jsonlite::fromJSON(p, simplifyVector = FALSE),
           error = function(e) list())
}

#' Write user preferences (atomic; creates the config dir if needed)
#' @param prefs Named list to save.
#' @return Invisibly, the prefs path.
#' @export
earthui_prefs_write <- function(prefs) {
  if (!requireNamespace("jsonlite", quietly = TRUE)) {
    stop("Package 'jsonlite' is required.", call. = FALSE)
  }
  p <- earthui_prefs_path()
  dir.create(dirname(p), recursive = TRUE, showWarnings = FALSE)
  tmp <- paste0(p, ".tmp")
  jsonlite::write_json(prefs, tmp, pretty = TRUE, auto_unbox = TRUE,
                       null = "null")
  file.rename(tmp, p)
  invisible(p)
}

#' Default regProj root for the current OS
#'
#' Resolution order:
#' \enumerate{
#'   \item `REGPROJ_ROOT` environment variable.
#'   \item `regproj_root` field in user prefs ([earthui_prefs_path()]).
#'   \item Per-OS default: `C:/regProj` on Windows; `~/regProj` elsewhere.
#' }
#'
#' @return Character scalar. Absolute path.
#' @export
default_regproj_root <- function() {
  env <- Sys.getenv("REGPROJ_ROOT", unset = NA_character_)
  if (!is.na(env) && nzchar(env)) return(path.expand(env))
  prefs <- earthui_prefs_read()
  if (!is.null(prefs$regproj_root) && nzchar(prefs$regproj_root)) {
    return(path.expand(prefs$regproj_root))
  }
  if (.Platform$OS.type == "windows") "C:/regProj"
  else path.expand("~/regProj")
}

#' Detect the current operating system as a regProj segment
#'
#' Returns `"mac"` on Darwin, `"ubuntu"` on Linux, `"win11"` on Windows.
#' This is the value used as the `<os>` segment in regProj paths so that
#' multi-OS output can be merged cleanly on a developer's machine.
#'
#' @return Character scalar: `"mac"`, `"ubuntu"`, or `"win11"`.
#' @export
os_detect <- function() {
  sys <- Sys.info()[["sysname"]]
  switch(sys,
    Darwin  = "mac",
    Linux   = "ubuntu",
    Windows = "win11",
    "unknown"
  )
}

# Reference data + central index ------------------------------------------

#' Load the shipped regProj reference data
#'
#' Reads `pkg/inst/extdata/regproj_reference.json` once per session and
#' caches the result. Contains country names, US states, and US counties
#' (with FIPS codes). Used by the UI cascades to populate dropdowns.
#'
#' @return A nested list with components `version`, `countries`, `states`,
#'   `counties`.
#' @export
regproj_reference <- function() {
  cache <- getOption("earthui.regproj_reference_cache", NULL)
  if (!is.null(cache)) return(cache)
  if (!requireNamespace("jsonlite", quietly = TRUE)) {
    stop("Package 'jsonlite' is required.", call. = FALSE)
  }
  path <- system.file("extdata", "regproj_reference.json", package = "earthUI")
  if (!nzchar(path) || !file.exists(path)) {
    stop("regproj_reference.json not found in installed package.",
         call. = FALSE)
  }
  ref <- jsonlite::fromJSON(path, simplifyVector = FALSE)
  options(earthui.regproj_reference_cache = ref)
  ref
}

#' Path to the central regProj name↔code index (legacy)
#'
#' The geo data is now stored in `<REGPROJ_ROOT>/geo.sqlite` (see
#' [regproj_geo_db_path()]). This path returns the location of the
#' legacy `.regproj-index.json` file, which is migrated into the
#' SQLite DB on first connect and is no longer written to.
#'
#' @param root regProj root. Defaults to [default_regproj_root()].
#' @return Character scalar.
#' @export
regproj_index_path <- function(root = default_regproj_root()) {
  file.path(path.expand(root), ".regproj-index.json")
}

#' Read the central regProj index from the geo SQLite DB
#'
#' Returns the entire geo DB as a nested list keyed by scope (slash-
#' separated parent-code path), for backward compatibility with callers
#' that iterate. New code should prefer [regproj_index_get()] or direct
#' DB queries via [regproj_geo_db_connect()].
#'
#' @inheritParams regproj_index_path
#' @return Named list. Outer key is scope (`""` for countries, then
#'   slash-separated codes per admin level). Inner key is full name;
#'   value is the code.
#' @export
regproj_index_read <- function(root = default_regproj_root()) {
  if (!file.exists(regproj_geo_db_path(root))) {
    con <- tryCatch(regproj_geo_db_connect(root), error = function(e) NULL)
    if (is.null(con)) return(list())
    DBI::dbDisconnect(con)
  }
  con <- regproj_geo_db_connect(root)
  on.exit(DBI::dbDisconnect(con), add = TRUE)
  out <- list()

  countries <- DBI::dbGetQuery(con, "SELECT code, name FROM countries")
  if (nrow(countries) > 0L) {
    out[[""]] <- as.list(setNames(countries$code, countries$name))
  }

  ae <- DBI::dbGetQuery(con,
    "SELECT country, level, parent_codes, code, name FROM admin_entries")
  if (nrow(ae) == 0L) return(out)

  ae$scope <- ifelse(nzchar(ae$parent_codes),
                     paste(ae$country, ae$parent_codes, sep = "/"),
                     ae$country)
  for (sc in unique(ae$scope)) {
    sub <- ae[ae$scope == sc, ]
    out[[sc]] <- as.list(setNames(sub$code, sub$name))
  }
  out
}

#' Deprecated. The geo data is now in SQLite; this is a no-op kept for
#' backward compatibility.
#'
#' @inheritParams regproj_index_path
#' @param idx Ignored.
#' @return Invisibly, the geo DB path.
#' @export
regproj_index_write <- function(idx, root = default_regproj_root()) {
  warning("`regproj_index_write()` is deprecated; use `regproj_index_put()` ",
          "to write individual entries to the geo SQLite DB.",
          call. = FALSE)
  invisible(regproj_geo_db_path(root))
}

#' Get a code for a full name within a scope
#'
#' Looks up `full_name` under `scope` (e.g. `"us/ca"`) in the geo
#' SQLite DB (which is seeded with shipped reference data on first
#' creation). Returns `NULL` if not found.
#'
#' @param scope Character scalar. Slash-separated path of parent codes
#'   (e.g. `"us/ca"` for California's counties; `"us/ca/001"` for
#'   Alameda's cities). Empty string for top-level (countries).
#' @param full_name Character scalar. The display name to look up.
#' @param root regProj root. Defaults to [default_regproj_root()].
#' @return Character scalar code, or `NULL`.
#' @export
regproj_index_get <- function(scope, full_name,
                              root = default_regproj_root()) {
  con <- regproj_geo_db_connect(root)
  on.exit(DBI::dbDisconnect(con), add = TRUE)
  if (is.null(scope) || !nzchar(scope)) {
    res <- DBI::dbGetQuery(con,
      "SELECT code FROM countries WHERE name = ?",
      params = list(full_name))
  } else {
    ps <- regproj_geo_parse_scope_(scope)
    if (is.null(ps)) return(NULL)
    res <- DBI::dbGetQuery(con,
      "SELECT code FROM admin_entries
        WHERE country = ? AND level = ? AND parent_codes = ? AND name = ?",
      params = list(ps$country, ps$level, ps$parent_codes, full_name))
  }
  if (nrow(res) == 0L) return(NULL)
  res$code[1L]
}

#' Set a name↔code mapping in the geo SQLite DB
#'
#' Inserts (or replaces) the mapping under the given scope.
#'
#' @inheritParams regproj_index_get
#' @param code Character scalar. The path code to assign.
#' @return Invisibly, the code.
#' @export
regproj_index_put <- function(scope, full_name, code,
                              root = default_regproj_root()) {
  con <- regproj_geo_db_connect(root)
  on.exit(DBI::dbDisconnect(con), add = TRUE)
  if (is.null(scope) || !nzchar(scope)) {
    DBI::dbExecute(con,
      "INSERT OR REPLACE INTO countries (code, name) VALUES (?, ?)",
      params = list(code, full_name))
  } else {
    ps <- regproj_geo_parse_scope_(scope)
    if (is.null(ps)) stop("Invalid scope: ", scope, call. = FALSE)
    # Delete any existing row with the same NAME in this scope first
    # (regardless of code), so renaming is name-based, not code-based.
    DBI::dbExecute(con,
      "DELETE FROM admin_entries
        WHERE country = ? AND level = ? AND parent_codes = ? AND name = ?",
      params = list(ps$country, ps$level, ps$parent_codes, full_name))
    DBI::dbExecute(con,
      "INSERT OR REPLACE INTO admin_entries
         (country, level, parent_codes, code, name) VALUES (?, ?, ?, ?, ?)",
      params = list(ps$country, ps$level, ps$parent_codes, code, full_name))
  }
  invisible(code)
}

#' List input files in an active project's in/ folder
#'
#' Returns the basenames of regular files in `<project_path>/<os>_in/`.
#' Hidden dotfiles and subdirectories are excluded. Sorted alphabetically.
#'
#' @param project_path Absolute path to the project root (the flat
#'   segment folder).
#' @param os OS segment to use. Defaults to [os_detect()].
#' @return Character vector of file basenames.
#' @export
regproj_in_files <- function(project_path, os = os_detect()) {
  in_dir <- file.path(project_path, paste0(os, "_in"))
  if (!dir.exists(in_dir)) return(character(0))
  files <- list.files(in_dir, full.names = FALSE, recursive = FALSE,
                      no.. = TRUE)
  files <- files[!startsWith(files, ".")]
  if (length(files) == 0L) return(character(0))
  full <- file.path(in_dir, files)
  files <- files[!file.info(full)$isdir]
  sort(files)
}

#' Last-used input file marker (per project)
#'
#' Stores the basename of the most-recently-selected input file in
#' `<project>/<os>/.regproj-last`, so it can be auto-selected the next
#' time the project is opened.
#'
#' @param project_path Absolute project path.
#' @param os OS segment. Defaults to [os_detect()].
#' @return Path / basename / invisible path.
#' @name regproj_last_file
NULL

#' @rdname regproj_last_file
#' @export
regproj_last_file_path <- function(project_path, os = os_detect()) {
  file.path(project_path, paste0(".regproj-last_", os))
}

#' @rdname regproj_last_file
#' @export
regproj_last_file_get <- function(project_path, os = os_detect()) {
  p <- regproj_last_file_path(project_path, os)
  if (!file.exists(p)) return(NULL)
  v <- tryCatch(readLines(p, n = 1L, warn = FALSE),
                error = function(e) character(0))
  if (length(v) == 0L || !nzchar(v[[1L]])) return(NULL)
  v[[1L]]
}

#' @rdname regproj_last_file
#' @param basename File basename to remember.
#' @export
regproj_last_file_set <- function(project_path, basename,
                                  os = os_detect()) {
  p <- regproj_last_file_path(project_path, os)
  dir.create(dirname(p), recursive = TRUE, showWarnings = FALSE)
  writeLines(basename, p)
  invisible(p)
}


#' List all projects under a regProj root
#'
#' Walks `<root>/<purpose>/<country>/<state>/<county>/<city>/<project_name>/`
#' and returns a data frame describing each project found, with its mtime
#' (most recent of the in/ and out/ trees, falling back to the project
#' folder itself).
#'
#' Recognized purposes: `gen`, `appr`, `mktarea`. Other top-level
#' subdirectories under `<root>` are ignored, including any leading-dot
#' files like `.regproj-index.json`.
#'
#' @param root regProj root. Defaults to [default_regproj_root()].
#' @param sort_by `"recent"` (mtime descending, default) or
#'   `"alpha"` (project name ascending).
#' @return Data frame with columns: `project_path` (absolute), `purpose`,
#'   `country`, `state`, `county`, `city`, `project_name`, `mtime`
#'   (POSIXct). Empty data frame (correct columns, zero rows) if no
#'   projects exist or root is missing.
#' @export
regproj_list_projects <- function(root    = default_regproj_root(),
                                  sort_by = c("recent", "alpha")) {
  sort_by <- match.arg(sort_by)
  empty <- data.frame(
    project_path = character(0), purpose = character(0),
    country      = character(0), state   = character(0),
    county       = character(0), city    = character(0),
    project_name = character(0), mtime   = as.POSIXct(character(0)),
    stringsAsFactors = FALSE
  )
  root <- path.expand(root)
  if (!dir.exists(root)) return(empty)

  purposes <- c("gen", "appr", "mktarea")
  rows <- list()

  for (p in purposes) {
    p_dir <- file.path(root, p)
    if (!dir.exists(p_dir)) next
    flat_segs <- list.dirs(p_dir, recursive = FALSE, full.names = FALSE)
    for (seg in flat_segs) {
      parsed <- regproj_parse_flat(seg)
      if (is.null(parsed)) next  # not a regProj flat segment
      proj_path <- file.path(p_dir, seg)
      mt <- file.info(proj_path)$mtime
      sub <- list.files(proj_path, recursive = TRUE, full.names = TRUE,
                        all.files = FALSE, no.. = TRUE)
      if (length(sub) > 0L) {
        mts <- file.info(sub)$mtime
        mts <- mts[!is.na(mts)]
        if (length(mts) > 0L) mt <- max(c(mt, mts), na.rm = TRUE)
      }
      # Pad levels to 4 columns (state/county/city) for backwards-compat
      lv <- parsed$levels
      st <- if (length(lv) >= 1L) lv[[1L]] else NA_character_
      cn <- if (length(lv) >= 2L) lv[[2L]] else NA_character_
      ci <- if (length(lv) >= 3L) lv[[3L]] else NA_character_
      rows[[length(rows) + 1L]] <- data.frame(
        project_path = proj_path, purpose = p,
        country      = parsed$country, state = st,
        county       = cn, city  = ci,
        project_name = parsed$project_name, mtime = as.POSIXct(mt),
        stringsAsFactors = FALSE
      )
    }
  }

  if (length(rows) == 0L) return(empty)
  out <- do.call(rbind, rows)
  out <- if (sort_by == "recent") {
    out[order(out$mtime, decreasing = TRUE), , drop = FALSE]
  } else {
    out[order(out$project_name), , drop = FALSE]
  }
  rownames(out) <- NULL
  out
}

#' Test whether a path is a regProj project leaf folder
#'
#' Returns TRUE if `path` exists, sits two directories deep under `root`
#' (`<purpose>/<flat_segment>/`), and the flat segment parses cleanly.
#'
#' @param path Absolute path to check.
#' @param root regProj root. Defaults to [default_regproj_root()].
#' @return Logical scalar.
#' @export
is_project_dir <- function(path, root = default_regproj_root()) {
  if (!is.character(path) || length(path) != 1L || !dir.exists(path)) {
    return(FALSE)
  }
  rel <- sub(paste0("^", path.expand(root), "/?"), "",
             path.expand(path), perl = TRUE)
  if (rel == path.expand(path)) return(FALSE)  # not under root
  parts <- strsplit(rel, "/", fixed = TRUE)[[1L]]
  if (length(parts) != 2L) return(FALSE)
  if (!parts[1L] %in% c("gen", "appr", "mktarea")) return(FALSE)
  !is.null(regproj_parse_flat(parts[2L]))
}


#' Generate a city path code from a full name
#'
#' Applies the rule: lowercase + strip all non-alphanumerics + first 6
#' characters. If the resulting code already exists in `existing_codes`,
#' append `_1`, `_2`, ... until unique.
#'
#' @param full_name Character scalar (e.g. `"Carmel-by-the-Sea"`).
#' @param existing_codes Character vector of codes already in the parent
#'   folder. Defaults to `character(0)`.
#' @return Character scalar.
#' @export
city_abbreviation <- function(full_name, existing_codes = character(0)) {
  base <- tolower(full_name)
  base <- gsub("[^a-z0-9]+", "", base)
  if (!nzchar(base)) base <- "city"
  base <- substr(base, 1L, 6L)
  if (!base %in% existing_codes) return(base)
  i <- 1L
  while (TRUE) {
    cand <- paste0(base, "_", i)
    if (!cand %in% existing_codes) return(cand)
    i <- i + 1L
  }
}


#' Encode the flat regProj segment from components
#'
#' Joins country, admin levels, and project name with `_` to produce the
#' single hierarchy-encoding folder name used at the project level under
#' `<root>/<purpose>/`. Validates each component: admin codes must match
#' `^[a-z0-9-]+$` (no internal underscores), project name allows
#' `^[A-Za-z0-9_-]+$`.
#'
#' @inheritParams regproj_path
#' @return Character scalar (e.g. `"us_ca_081_burlin_20251231_j"`).
#' @export
regproj_flat_segment <- function(country, levels, project_name) {
  country <- tolower(as.character(country))
  if (!grepl("^[a-z]{2}$", country))
    stop("`country` must be a 2-letter ISO code.", call. = FALSE)
  levels <- as.character(levels)
  bad <- !grepl("^[a-z0-9-]+$", levels)
  if (any(bad)) {
    stop("Each level code must match ^[a-z0-9-]+$ (no underscores). Bad: ",
         paste(levels[bad], collapse = ", "), call. = FALSE)
  }
  if (!grepl("^[A-Za-z0-9_-]+$", project_name) || nchar(project_name) > 24L) {
    stop("`project_name` must match ^[A-Za-z0-9_-]+$ and be <= 24 chars.",
         call. = FALSE)
  }
  paste(c(country, levels, project_name), collapse = "_")
}

#' Decode a flat regProj segment back into components
#'
#' Inverse of [regproj_flat_segment()]. Splits on `_`, takes the first
#' token as country, then the next `length(country_schema(country))`
#' tokens as admin levels; the remaining tokens (joined by `_`) are the
#' project name. Returns `NULL` on parse failure.
#'
#' @param segment Character scalar.
#' @return Named list with `country`, `levels` (character vector),
#'   `project_name` — or `NULL` if parsing failed.
#' @export
regproj_parse_flat <- function(segment) {
  if (!is.character(segment) || length(segment) != 1L || !nzchar(segment))
    return(NULL)
  toks <- strsplit(segment, "_", fixed = TRUE)[[1L]]
  if (length(toks) < 2L) return(NULL)
  cc <- toks[[1L]]
  schema <- country_schema(cc)
  n_admin <- length(schema)
  if (length(toks) < 2L + n_admin - 1L) return(NULL)
  if (length(toks) < 1L + n_admin + 1L) return(NULL)
  levels <- toks[seq_len(n_admin) + 1L]
  proj   <- paste(toks[(n_admin + 2L):length(toks)], collapse = "_")
  list(country = cc, levels = levels, project_name = proj)
}

#' Build a regProj-canonical path
#'
#' Composes the path
#' `<root>/<purpose>/<flat_segment>/<os>_<in|out>[_<method>]` from its
#' components. The hierarchy (country / admin levels / project name) is
#' concatenated into a single folder under `<purpose>/` to keep the tree
#' shallow. Pure path computation — does not create any directories
#' unless `create = TRUE`.
#'
#' @param purpose `"gen"`, `"appr"`, or `"mktarea"`.
#' @param country Lowercase ISO 3166-1 alpha-2 country code (e.g. `"us"`).
#' @param levels Character vector of admin codes, ordered top to bottom.
#'   Length must match `length(country_schema(country))`. Each element
#'   must satisfy `^[a-z0-9-]+$` (no underscores — they are the segment
#'   separator).
#' @param project_name Project leaf name. Must satisfy `^[A-Za-z0-9_-]+$`,
#'   max 24 characters. Underscores are allowed because the parser uses
#'   the country schema to know how many tokens are admin vs project.
#' @param os One of `"mac"`, `"ubuntu"`, `"win11"`. Defaults to
#'   [os_detect()].
#' @param in_or_out One of `"in"` or `"out"`. Defaults to `"out"`.
#' @param method Optional method subdir (e.g. `"earth"`, `"glmnet"`,
#'   `"mgcv"`, `"combined"`). When `in_or_out = "out"` and `method` is
#'   non-empty, the leaf becomes `<os>_out_<method>`. Ignored for `"in"`.
#' @param root Optional explicit regProj root. Defaults to
#'   [default_regproj_root()].
#' @param create Logical. If `TRUE`, the directory tree is created with
#'   `dir.create(recursive = TRUE)`. Defaults to `FALSE`.
#'
#' @return Character scalar. Absolute normalized path.
#' @export
regproj_path <- function(purpose,
                         country,
                         levels,
                         project_name,
                         os         = os_detect(),
                         in_or_out  = c("out", "in"),
                         method     = "earth",
                         root       = default_regproj_root(),
                         create     = FALSE) {
  in_or_out <- match.arg(in_or_out)

  if (!purpose %in% c("gen", "appr", "mktarea"))
    stop("`purpose` must be one of: gen, appr, mktarea", call. = FALSE)
  if (!is.character(country) || length(country) != 1L || !nzchar(country))
    stop("`country` must be a single non-empty string.", call. = FALSE)
  country <- tolower(country)

  expected_depth <- length(country_schema(country))
  levels <- as.character(levels)
  if (length(levels) != expected_depth) {
    stop(sprintf("`levels` for country '%s' must have %d entries (got %d).",
                 country, expected_depth, length(levels)), call. = FALSE)
  }
  if (!os %in% c("mac", "ubuntu", "win11"))
    stop("`os` must be one of: mac, ubuntu, win11", call. = FALSE)

  flat <- regproj_flat_segment(country, levels, project_name)
  leaf <- if (in_or_out == "out" && !is.null(method) && nzchar(method)) {
    paste0(os, "_out_", method)
  } else if (in_or_out == "out") {
    paste0(os, "_out")
  } else {
    paste0(os, "_in")
  }
  p <- file.path(root, purpose, flat, leaf)
  p <- normalizePath(p, mustWork = FALSE, winslash = "/")
  if (isTRUE(create) && !dir.exists(p)) {
    dir.create(p, recursive = TRUE, showWarnings = FALSE)
  }
  p
}
