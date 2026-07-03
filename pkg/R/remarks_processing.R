# Prolog remarks parsing + comma-list splitting for the optional
# "Execute Prolog Processing" step. The remarks parser shells into the
# self-contained `vProlog` package (Trealla engine) and runs the bundled
# DCG grammar; the list splitter is pure R. Both are idempotent.

# --- helpers ----------------------------------------------------------------

# Directory holding the bundled Prolog grammar. Installed location first, then
# a dev fallback so the functions work from a source checkout.
prolog_dir_ <- function() {
  d <- system.file("prolog", package = "earthUI")
  if (nzchar(d) && file.exists(file.path(d, "mls_remarks.pl"))) return(d)
  dev <- file.path("pkg", "inst", "prolog")
  if (file.exists(file.path(dev, "mls_remarks.pl"))) return(normalizePath(dev))
  ""
}

#' Is the optional vProlog engine available?
#'
#' @return `TRUE` if the `vProlog` package and the bundled grammar are both
#'   present, otherwise `FALSE`.
#' @export
vProlog_available <- function() {
  requireNamespace("vProlog", quietly = TRUE) &&
    nzchar(prolog_dir_())
}

# snake_case a free-text token: lowercase, runs of non-alphanumerics -> "_".
snake_token_ <- function(x) {
  x <- tolower(as.character(x))
  x <- gsub("[^a-z0-9]+", "_", x)
  x <- gsub("^_+|_+$", "", x)
  x
}

#' Column prefix for a remarks column.
#'
#' First letter of each underscore-separated word, e.g. `public_remarks`
#' becomes `pr_`, `agent_remark` becomes `ar_`, a lone `remarks` becomes `r_`.
#'
#' @param colname Column name.
#' @return A short prefix ending in `_`.
#' @export
remarks_prefix_ <- function(colname) {
  words <- strsplit(tolower(colname), "[^a-z0-9]+")[[1]]
  words <- words[nzchar(words)]
  if (!length(words)) return("r_")
  paste0(paste0(substr(words, 1, 1), collapse = ""), "_")
}

# Coerce a writeq'd Prolog value string to a typed R scalar; returns NULL for
# list-valued features ("[...]") which are skipped for remarks columns.
parse_pl_value_ <- function(v) {
  v <- trimws(v)
  if (startsWith(v, "[")) return(NULL)                 # list feature -> skip
  if (v %in% c("true", "false")) return(v == "true")
  if (grepl("^-?[0-9]+(\\.[0-9]+)?$", v)) return(as.numeric(v))
  gsub("^'|'$", "", v)                                 # unquote atom
}

# --- remarks -> feature columns (via vProlog) -------------------------------

#' Parse a vector of remarks into a data frame of feature columns.
#'
#' Runs the bundled DCG grammar over each remark via the `vProlog` engine and
#' returns one column per scalar feature (list-valued features are omitted).
#' Returns `NULL` if `vProlog` is unavailable.
#'
#' @param remarks Character vector of remark text (NAs treated as empty).
#' @param prefix Column-name prefix (e.g. `"pr_"`).
#' @param progress Optional `function(fraction, detail)` progress callback.
#' @param base,span Map this call's local 0..1 progress onto `base + span*frac`.
#' @return A data frame with `length(remarks)` rows, or `NULL`.
#' @export
remarks_to_columns_ <- function(remarks, prefix = "pr_",
                                progress = NULL, base = 0, span = 1) {
  if (!vProlog_available()) return(NULL)
  n <- length(remarks)
  if (n == 0L) return(NULL)
  pdir    <- prolog_dir_()
  grammar <- file.path(pdir, "mls_remarks.pl")
  helper  <- file.path(pdir, "mls_analyze.pl")

  # one temp file per remark (robust escaping via file_to_atom)
  files <- vapply(seq_len(n), function(i) tempfile(fileext = ".txt"), "")
  on.exit(unlink(files), add = TRUE)
  txt <- ifelse(is.na(remarks), "", as.character(remarks))
  for (i in seq_len(n)) writeLines(txt[i], files[i], useBytes = TRUE)
  outfile <- tempfile(fileext = ".tsv")
  on.exit(unlink(outfile), add = TRUE)

  pl <- vProlog::prolog_open()
  vProlog::pl_consult(pl, grammar)
  vProlog::pl_consult(pl, helper)
  jobs <- paste0(sprintf("job(%d,'%s').", seq_len(n), files), collapse = "\n")
  driver <- paste0(
    jobs, "\n",
    "run(Lo,Hi,Out) :- open(Out, write, S),",
    " forall((job(I,F), I >= Lo, I =< Hi), emit(I,F,S)), close(S).\n",
    "emit(I,F,S) :- file_to_atom(F,A), analyze_remarks(A,Ft),",
    " forall(member(K-V,Ft), format(S, \"~w\\t~w\\t~q~n\", [I,K,V])).\n")
  vProlog::pl_consult_text(pl, driver)

  # Process in chunks so the progress bar advances during the parse.
  chunk <- 250L
  parts <- list(); s <- 1L
  while (s <= n) {
    e <- min(s + chunk - 1L, n)
    cout <- tempfile(fileext = ".tsv")
    ok <- vProlog::pl_query(pl, sprintf("run(%d,%d,'%s')", s, e, cout))
    if (isTRUE(ok) && file.exists(cout) && file.info(cout)$size > 0)
      parts[[length(parts) + 1L]] <- utils::read.table(
        cout, sep = "\t", quote = "", comment.char = "",
        col.names = c("row", "key", "value"),
        colClasses = "character", stringsAsFactors = FALSE)
    unlink(cout)
    if (is.function(progress))
      progress(base + span * (e / n), sprintf("Parsing remarks (%d/%d rows)", e, n))
    s <- e + 1L
  }
  long <- if (length(parts)) do.call(rbind, parts) else NULL
  if (is.null(long) || !nrow(long)) return(data.frame(row.names = seq_len(n)))
  long$row <- as.integer(long$row)
  out <- data.frame(row.names = seq_len(n))
  for (k in unique(long$key)) {
    sub <- long[long$key == k, , drop = FALSE]
    vals <- lapply(sub$value, parse_pl_value_)
    if (all(vapply(vals, is.null, logical(1)))) next        # list feature
    col <- rep(NA, n)
    keep <- !vapply(vals, is.null, logical(1))
    col[sub$row[keep]] <- unlist(vals[keep])
    out[[paste0(prefix, k)]] <- if (is.logical(unlist(vals[keep]))) as.logical(col) else col
  }
  out
}

# --- list column -> one-hot columns (pure R) --------------------------------

#' Split a comma-delimited string column into one-hot TRUE/FALSE columns.
#'
#' Each distinct value becomes a `<colname>_<snake_value>` logical column.
#'
#' @param values Character vector of comma-delimited strings.
#' @param colname Base name for the generated columns.
#' @param sep Delimiter (default `","`).
#' @return A data frame of logical columns, or `NULL` if no values found.
#' @export
split_list_column_ <- function(values, colname, sep = ",") {
  v <- ifelse(is.na(values), "", as.character(values))
  toks <- lapply(strsplit(v, sep, fixed = TRUE), function(x) {
    t <- unique(snake_token_(trimws(x))); t[nzchar(t)]
  })
  vocab <- sort(unique(unlist(toks)))
  if (!length(vocab)) return(NULL)
  out <- data.frame(row.names = seq_along(v))
  for (tok in vocab) {
    out[[paste0(colname, "_", tok)]] <-
      vapply(toks, function(t) tok %in% t, logical(1))
  }
  out
}

# --- user-scripted derivation rules (via vProlog) ---------------------------

# Type a character vector of writeq'd Prolog values: true/false -> logical,
# all-numeric -> numeric, otherwise character.
type_pl_column_ <- function(raw) {
  raw  <- trimws(raw)
  nona <- raw[!is.na(raw)]
  if (length(nona) && all(nona %in% c("true", "false"))) return(raw == "true")
  num <- suppressWarnings(as.numeric(raw))
  if (length(nona) && !any(is.na(num[!is.na(raw)]))) return(num)
  gsub("^'|'$", "", raw)
}

#' Apply user-scripted Prolog derivation rules to a data frame.
#'
#' Each row's columns are asserted as `cell(Name, Value)` facts; every
#' `derive(Col, Val)` solution becomes a new column (first value per Col wins).
#' Derived columns are typed: `true`/`false` -> logical, numeric -> numeric
#' (continuous), otherwise character. Returns `NULL` if `vProlog` is
#' unavailable or `rules_text` is empty.
#'
#' @param df A data frame (typically the data after grammar/list processing).
#' @param rules_text User Prolog source defining `derive/2` (a single string).
#' @param progress,base,span Optional progress callback and 0..1 mapping.
#' @return A data frame of derived columns (`nrow(df)` rows), or `NULL`.
#' @export
apply_user_rules_ <- function(df, rules_text, progress = NULL,
                              base = 0, span = 1) {
  if (!vProlog_available()) return(NULL)
  if (is.null(rules_text) || !nzchar(trimws(rules_text))) return(NULL)
  pl <- vProlog::prolog_open()
  vProlog::pl_consult(pl, file.path(prolog_dir_(), "mls_rules.pl"))
  vProlog::pl_consult_text(pl, rules_text)

  n <- nrow(df); cols <- names(df)
  qcols <- paste0("'", gsub("'", "''", cols), "'")
  enc <- lapply(df, function(v) {
    if (is.logical(v)) ifelse(is.na(v), NA_character_, ifelse(v, "true", "false"))
    else if (is.numeric(v)) ifelse(is.na(v), NA_character_, as.character(v))
    else { s <- as.character(v)
           ifelse(is.na(s), NA_character_, paste0("'", gsub("'", "''", s), "'")) }
  })
  derived <- list()
  step <- max(1L, n %/% 20L)
  for (i in seq_len(n)) {
    asserts <- character(0)
    for (j in seq_along(cols)) {
      val <- enc[[j]][i]
      if (!is.na(val)) asserts <- c(asserts,
        sprintf("assertz(cell(%s,%s))", qcols[j], val))
    }
    goal <- paste0("retractall(cell(_,_))",
      if (length(asserts)) paste0(",", paste(asserts, collapse = ",")) else "")
    vProlog::pl_query(pl, goal)
    sols <- vProlog::pl_findall(pl, "C-V", "derive(C,V)")
    seen <- character(0)
    for (s in sols) {
      m <- regmatches(s, regexec("^\\s*'?([A-Za-z0-9_.]+)'?\\s*-\\s*(.*)$", s))[[1]]
      if (length(m) < 3) next
      cn <- m[2]
      if (cn %in% seen) next
      seen <- c(seen, cn)
      if (is.null(derived[[cn]])) derived[[cn]] <- rep(NA_character_, n)
      derived[[cn]][i] <- trimws(m[3])
    }
    if (is.function(progress) && (i %% step == 0 || i == n))
      progress(base + span * (i / n), sprintf("Applying rules (%d/%d rows)", i, n))
  }
  # Columns the rules asked to discard (drop/1 facts), attached as an attribute.
  drop_cols <- unique(gsub("^'|'$", "", trimws(
    vProlog::pl_findall(pl, "C", "drop(C)"))))
  out <- data.frame(row.names = seq_len(n))
  for (cn in names(derived)) out[[cn]] <- type_pl_column_(derived[[cn]])
  attr(out, "drop_cols") <- drop_cols
  out
}

# Encode row `i` of `df` as a vector of "'name',value" cell/2 argument strings.
encode_row_cells_ <- function(df, i) {
  cols <- names(df); out <- character(0)
  for (j in seq_along(cols)) {
    v <- df[[j]][i]
    val <- if (is.logical(v)) { if (is.na(v)) next else if (isTRUE(v)) "true" else "false" }
           else if (is.numeric(v)) { if (is.na(v)) next else as.character(v) }
           else { s <- as.character(v); if (is.na(s)) next else paste0("'", gsub("'", "''", s), "'") }
    out <- c(out, sprintf("'%s',%s", gsub("'", "''", cols[j]), val))
  }
  out
}

#' Validate user derivation rules (syntax + load + dry-run).
#'
#' In a throwaway vProlog engine: (1) a term-by-term **syntax** scan reporting
#' the offending clause and error kind; (2) checks `derive/2` actually
#' **loaded**; (3) a **dry-run** of `derive/2` (against row 1 of `df` if given)
#' to surface runtime errors such as unknown predicates / typo'd helpers.
#'
#' @param rules_text User Prolog rules (a string).
#' @param df Optional data frame; row 1 supplies dry-run `cell/2` facts.
#' @return `list(ok = <logical>, message = <character>)`.
#' @export
validate_rules_ <- function(rules_text, df = NULL) {
  if (!vProlog_available())
    return(list(ok = TRUE, message = "vProlog not installed — rules not validated."))
  if (is.null(rules_text) || !nzchar(trimws(rules_text)))
    return(list(ok = TRUE, message = "No rules."))

  rf <- tempfile(fileext = ".pl"); on.exit(unlink(rf), add = TRUE)
  writeLines(rules_text, rf, useBytes = TRUE)
  rfp <- gsub("\\\\", "/", rf)

  pl <- vProlog::prolog_open()
  vProlog::pl_consult(pl, file.path(prolog_dir_(), "mls_rules.pl"))
  vProlog::pl_consult_text(pl, paste(
    "scan_terms(File, R) :- open(File, read, S), sloop_(S, 0, R), close(S).",
    "sloop_(S, N, R) :- catch(read_term(S, T, []), Err, true),",
    "  ( var(Err) -> ( T == end_of_file -> R = ok(N) ; N1 is N+1, sloop_(S,N1,R) )",
    "             ; N1 is N+1, R = err(N1, Err) ).",
    "dry_run(R) :- catch((findall(_-_, derive(_,_), _), R = ok), E, R = error(E)).",
    sep = "\n"))

  # 1. syntax scan
  s <- vProlog::pl_findall(pl, "R", sprintf("scan_terms('%s', R)", rfp))
  s <- if (length(s)) s[1] else "ok(0)"
  if (grepl("^err\\(", s)) {
    n <- sub("^err\\(([0-9]+),.*$", "\\1", s)
    kind <- if (grepl("syntax_error\\(", s))
              gsub("_", " ", sub(".*syntax_error\\(([^),]*)\\).*", "\\1", s)) else "syntax error"
    return(list(ok = FALSE, message = sprintf("Syntax error in rule #%s: %s.", n, kind)))
  }
  nc <- sub("^ok\\(([0-9]+)\\)$", "\\1", s)

  # 2. consult rules + check derive/2 or drop/1 is present
  vProlog::pl_consult_text(pl, rules_text)
  has_derive <- isTRUE(vProlog::pl_query(pl, "catch(current_predicate(derive/2),_,fail)"))
  has_drop   <- isTRUE(vProlog::pl_query(pl, "catch(current_predicate(drop/1),_,fail)"))
  if (!has_derive && !has_drop)
    return(list(ok = FALSE, message = "No derive/2 or drop/1 rules are defined."))
  n_drop <- if (has_drop) length(vProlog::pl_findall(pl, "C", "drop(C)")) else 0L

  # 3. dry-run on row 1 (catch runtime errors, e.g. unknown predicates)
  if (has_derive) {
    if (!is.null(df) && nrow(df) >= 1) {
      enc <- encode_row_cells_(df, 1L)
      if (length(enc))
        vProlog::pl_query(pl, paste0("retractall(cell(_,_)),",
          paste(sprintf("assertz(cell(%s))", enc), collapse = ",")))
    }
    d <- vProlog::pl_findall(pl, "R", "dry_run(R)")
    d <- if (length(d)) d[1] else "ok"
    if (grepl("^error\\(", d)) {
      proc <- if (grepl("existence_error\\(procedure,", d))
                sub(".*existence_error\\(procedure,([^),]*/[0-9]+).*", "\\1", d) else ""
      return(list(ok = FALSE, message = if (nzchar(proc)) sprintf("Unknown predicate: %s.", proc)
                  else "Runtime error while applying rules (see console)."))
    }
  }
  list(ok = TRUE, message = sprintf("Rules OK — %s clause(s)%s.", nc,
    if (n_drop > 0L) sprintf(" (%d drop)", n_drop) else ""))
}

#' Default `<project>_rules.pl` template (commented examples).
#'
#' Written into a project's rules file when none exists yet; the appraiser
#' uncomments / adapts the examples. Every example line is commented, so an
#' untouched template derives nothing.
#'
#' @return A single character string of Prolog source.
#' @export
default_rules_template_ <- function() {
  paste(
    "% <project>_rules.pl - user feature-derivation rules for earthUI.",
    "%",
    "% During \"Execute Prolog Processing\", each data row's columns are asserted",
    "% as cell(Name, Value) facts (Value is true/false, a number, or an atom).",
    "% Write derive(Col, Val) rules - every solution becomes a new column, and",
    "% the FIRST value per Col wins (so a fallback clause may follow). Helpers:",
    "%   count_true(+Cols,-N)         count_false(+Cols,-N)",
    "%   count_true_prefix(+Pre,-N)   count_false_prefix(+Pre,-N)",
    "%   cell_or(+Col,+Default,-Value)   is_na(+Col)",
    "% A value that was NA in the data has NO cell fact this row, so cell(Col,_)",
    "% fails for it; use cell_or/3 to supply a default (e.g. 0) or is_na/1 to test.",
    "%",
    "% Uncomment and adapt the examples below.",
    "",
    "% --- merge a collinear cluster into one flag (logical) ---",
    "% derive(solar_any, B) :-",
    "%     ( cell(pr_has_solar, true)",
    "%     ; cell(pr_solar_owned, true)",
    "%     ; cell(pr_has_battery, true) ) -> B = true ; B = false.",
    "",
    "% --- count instead of OR (keeps \"how many\"; numeric column) ---",
    "% derive(solar_count, N)   :- count_true([pr_has_solar, pr_solar_owned, pr_has_battery], N).",
    "% derive(amenity_count, N) :- count_true_prefix(pr_has_, N).",
    "",
    "% --- threshold -> boolean ---",
    "% derive(big_outbuilding, true)  :- cell(pr_outbuilding_sqft, S), S >= 1000.",
    "% derive(big_outbuilding, false).",
    "",
    "% --- arithmetic -> continuous column ---",
    "% derive(price_per_sqft, V) :- cell(sale_price, P), cell(living_area, A), A > 0, V is P / A.",
    "",
    "% --- NA handling (cell_or supplies a default; is_na tests for missing) ---",
    "% derive(bedrooms_filled, V) :- cell_or(bedrooms, 0, V).  % NA -> 0 (new column)",
    "% derive(bedrooms, V)        :- cell_or(bedrooms, 0, V).  % NA -> 0 (overwrite/impute)",
    "% derive(lot_missing, B)     :- ( is_na(lot_size) -> B = true ; B = false ).",
    "",
    "% --- trash columns: drop(Col) removes that column after the rules run ---",
    "% (handy to discard the raw granular columns once you've consolidated them)",
    "% drop(pr_price_solar_lease).",
    "% drop(pr_price_solar_payment).",
    "",
    sep = "\n")
}

# --- orchestrator -----------------------------------------------------------

#' Execute Prolog/list processing on a data frame.
#'
#' For every column named in `remarks_cols`, parse its remarks via `vProlog`
#' and add `<prefix><feature>` columns. For every column in `list_cols`, split
#' its comma-delimited values into one-hot columns. Generated columns overwrite
#' existing same-named columns (idempotent refresh); for list columns, existing
#' `<col>_*` columns whose value is no longer present are set all-FALSE.
#'
#' @param df A data frame.
#' @param remarks_cols Character vector of column names with special "remarks".
#' @param list_cols Character vector of column names with special "list".
#' @param rules_text Optional user Prolog `derive/2` rules (a string) applied as
#'   a final phase over the augmented row (see [apply_user_rules_()]).
#' @param progress Optional `function(fraction, detail)` progress callback,
#'   called with `fraction` in 0..1 as columns are processed.
#' @return `list(data = <augmented df>, added = <new column names>,
#'   remarks_ok = <logical: was vProlog used>)`.
#' @export
execute_prolog_processing <- function(df, remarks_cols = character(0),
                                      list_cols = character(0),
                                      rules_text = NULL, progress = NULL) {
  added <- character(0)
  remarks_ok <- TRUE
  dropped <- 0L
  rcols <- intersect(remarks_cols, names(df))
  lcols <- intersect(list_cols, names(df))
  has_rules <- !is.null(rules_text) && nzchar(trimws(rules_text))
  total <- length(rcols) + length(lcols) + (if (has_rules) 1L else 0L)
  done  <- 0L
  report <- function(detail) if (is.function(progress) && total > 0)
    progress(done / total, detail)

  for (col in rcols) {
    report(sprintf("Parsing remarks: %s", col))
    feats <- remarks_to_columns_(df[[col]], prefix = remarks_prefix_(col),
                                 progress = progress, base = done / total,
                                 span = 1 / total)
    done <- done + 1L
    if (is.null(feats)) { remarks_ok <- FALSE; next }
    for (nm in names(feats)) { df[[nm]] <- feats[[nm]]; added <- c(added, nm) }
  }

  for (col in lcols) {
    report(sprintf("Splitting list: %s", col))
    oh <- split_list_column_(df[[col]], col)
    done <- done + 1L
    if (is.null(oh)) next
    # stale <col>_* columns not in the new vocab -> all FALSE (keep, no churn)
    existing <- grep(paste0("^", col, "_"), names(df), value = TRUE)
    stale <- setdiff(existing, names(oh))
    for (nm in stale) df[[nm]] <- FALSE
    for (nm in names(oh)) { df[[nm]] <- oh[[nm]]; added <- c(added, nm) }
  }

  # Phase 3: user-scripted derivation rules over the augmented row, then any
  # drop/1 columns the rules asked to trash.
  if (has_rules) {
    report("Applying derivation rules")
    der <- apply_user_rules_(df, rules_text, progress = progress,
                             base = done / total, span = 1 / total)
    done <- done + 1L
    if (!is.null(der)) {
      if (ncol(der))
        for (nm in names(der)) { df[[nm]] <- der[[nm]]; added <- c(added, nm) }
      dc <- intersect(attr(der, "drop_cols"), names(df))
      for (nm in dc) df[[nm]] <- NULL
      dropped <- dropped + length(dc)
    }
  }

  if (is.function(progress) && total > 0) progress(1, "Done")
  list(data = df, added = intersect(unique(added), names(df)),
       remarks_ok = remarks_ok, dropped = dropped)
}
