#!/usr/bin/env Rscript
# Generate developer .cmt commentary skeletons for earthUI sources.
#   Rscript tools/cmt/generate_cmt.R [--force]
# Writes <base>_r.cmt next to each .R file and <base>_pl.cmt next to each
# .pl file (never overwrites an existing .cmt unless --force). .cmt files
# are developer-only: gitignored and Rbuildignored, never on GitHub/CRAN.

force <- "--force" %in% commandArgs(trailingOnly = TRUE)

hdr <- function(title, ch = "-") paste0(title, "\n", strrep(ch, nchar(title)))

# ---- R files ----------------------------------------------------------------

# comment block (roxygen or plain #) immediately above a line index
roxy_above <- function(lines, i) {
  j <- i - 1
  block <- character(0)
  while (j >= 1 && grepl("^\\s*#", lines[j])) {
    block <- c(sub("^\\s*#'?\\s?", "", lines[j]), block)
    j <- j - 1
  }
  block
}

# build an analyzable closure from an UNevaluated `function` call
mk_fn_ <- function(fcall) {
  tryCatch(eval(call("function", fcall[[2]], fcall[[3]]), baseenv()),
           error = function(e) NULL)
}

# named `x <- function(...)` definitions anywhere inside an expression
find_nested_fns_ <- function(e) {
  out <- list()
  rec <- function(x) {
    if (is.call(x)) {
      if (as.character(x[[1]])[1] %in% c("<-", "=") && length(x) >= 3 &&
          is.name(x[[2]]) && is.call(x[[3]]) &&
          identical(x[[3]][[1]], as.name("function"))) {
        out[[length(out) + 1]] <<- list(name = as.character(x[[2]]),
                                        fcall = x[[3]])
      }
      lst <- as.list(x)
      for (j in seq_along(lst)) {
        if (identical(lst[[j]], quote(expr = ))) next
        rec(lst[[j]])
      }
    }
  }
  rec(e)
  out
}
roxy_title <- function(block) {
  block <- block[!grepl("^@", block)]
  block <- block[nzchar(trimws(block))]
  if (length(block)) trimws(block[1]) else ""
}
roxy_param <- function(block, p) {
  hits <- grep(paste0("^@param\\s+", p, "([, ]|$)"), block)
  if (!length(hits)) return("")
  # continuation lines until next @tag
  out <- sub(paste0("^@param\\s+[^ ]+\\s*"), "", block[hits[1]])
  k <- hits[1] + 1
  while (k <= length(block) && !grepl("^@", block[k])) {
    out <- paste(out, trimws(block[k])); k <- k + 1
  }
  trimws(out)
}

# every symbol assigned inside a function body (internal variables)
internals_of <- function(fn) {
  found <- character(0)
  walk <- function(e) {
    if (is.call(e)) {
      op <- as.character(e[[1]])[1]
      if (op %in% c("<-", "=", "<<-") && length(e) >= 3) {
        t <- e[[2]]
        while (is.call(t)) t <- t[[2]]   # x$y <- / x[[i]] <- -> x
        if (is.name(t)) found <<- c(found, as.character(t))
      }
      lst <- as.list(e)
      for (j in seq_along(lst)) {
        if (identical(lst[[j]], quote(expr = ))) next
        walk(lst[[j]])
      }
    } else if (is.function(e)) {
      walk(body(e))
    }
  }
  try(walk(body(fn)), silent = TRUE)
  setdiff(unique(found), "")
}

# params mutated in place (env / reactiveValues / list element assignment)
both_dir_params <- function(fn) {
  ps <- names(formals(fn)); found <- character(0)
  walk <- function(e) {
    if (is.call(e)) {
      op <- as.character(e[[1]])[1]
      if (op %in% c("<-", "=", "<<-") && length(e) >= 3 && is.call(e[[2]])) {
        t <- e[[2]]
        while (is.call(t)) t <- t[[2]]
        if (is.name(t) && as.character(t) %in% ps)
          found <<- c(found, as.character(t))
      }
      lst <- as.list(e)
      for (j in seq_along(lst)) {
        if (identical(lst[[j]], quote(expr = ))) next
        walk(lst[[j]])
      }
    }
  }
  try(walk(body(fn)), silent = TRUE)
  unique(found)
}

type_guess <- function(default) {
  if (identical(default, quote(expr = ))) return("<type?>")
  d <- deparse(default)[1]
  if (grepl("^[0-9.]+L$", d)) "integer"
  else if (grepl("^[0-9.eE+-]+$", d)) "numeric"
  else if (grepl('^"', d)) "character"
  else if (d %in% c("TRUE", "FALSE")) "logical"
  else if (d == "NULL") "<type?> or NULL"
  else "<type?>"
}

fn_section_ <- function(nm, val, block, nested_names = character(0)) {
  ps <- formals(val)
  both <- both_dir_params(val)
  param_lines <- if (length(ps)) vapply(names(ps), function(p) {
    desc <- roxy_param(block, p)
    sprintf("    %-16s (%s, %s) — %s", p, type_guess(ps[[p]]),
            if (p %in% both) "both" else "input",
            if (nzchar(desc)) desc else "TODO")
  }, character(1)) else "    (none)"
  iv <- setdiff(internals_of(val), c(names(ps), nested_names))
  iv_lines <- if (length(iv)) paste0("    ", sort(iv), " — TODO")
              else "    (none)"
  nested_note <- if (length(nested_names))
    paste0("  Nested functions (documented in their own sections): ",
           paste(sort(nested_names), collapse = ", "), "\n") else ""
  title <- roxy_title(block)
  paste0(
    nm, "(", paste(names(ps), collapse = ", "), ")\n",
    "  Description: ", if (nzchar(title)) title else "TODO", "\n",
    "  Parameters:\n", paste(param_lines, collapse = "\n"), "\n",
    "  Returns: TODO\n", nested_note,
    "  Internal variables:\n", paste(iv_lines, collapse = "\n"), "\n",
    "  References: (none)\n")
}

# comment block above the first line matching a nested definition header
nested_block_ <- function(lines, nm) {
  hit <- grep(paste0("^\\s*", nm, "\\s*(<-|=)\\s*function"), lines)
  if (length(hit)) roxy_above(lines, hit[1]) else character(0)
}

cmt_for_r <- function(path) {
  lines <- readLines(path, warn = FALSE)
  exprs <- tryCatch(parse(path, keep.source = TRUE),
                    error = function(e) NULL)
  if (is.null(exprs)) return(NULL)
  env <- new.env()
  refs <- utils::getSrcref(exprs)

  globals <- character(0)
  fun_sections <- character(0)

  emit_fn_and_nested <- function(nm, fcall, block) {
    val <- mk_fn_(fcall)
    if (is.null(val)) return(invisible())
    nested <- find_nested_fns_(fcall[[3]])
    nested_names <- vapply(nested, function(x) x$name, character(1))
    fun_sections <<- c(fun_sections,
                       fn_section_(nm, val, block, unique(nested_names)))
    for (nf in nested) {
      nval <- mk_fn_(nf$fcall)
      if (is.null(nval)) next
      sub_nested <- vapply(find_nested_fns_(nf$fcall[[3]]),
                           function(x) x$name, character(1))
      fun_sections <<- c(fun_sections,
        fn_section_(paste0(nm, " > ", nf$name), nval,
                    nested_block_(lines, nf$name), unique(sub_nested)))
    }
  }

  for (k in seq_along(exprs)) {
    e <- exprs[[k]]
    line1 <- utils::getSrcLocation(refs[[k]], "line")

    # bare top-level function literal (classic ui.R/server.R app files)
    if (is.call(e) && identical(e[[1]], as.name("function"))) {
      emit_fn_and_nested(paste0("(", basename(path), " top-level function)"),
                         e, roxy_above(lines, line1))
      next
    }
    if (!(is.call(e) && as.character(e[[1]])[1] %in% c("<-", "=") &&
          is.name(e[[2]]))) next
    nm <- as.character(e[[2]])

    if (is.call(e[[3]]) && identical(e[[3]][[1]], as.name("function"))) {
      emit_fn_and_nested(nm, e[[3]], roxy_above(lines, line1))
    } else {
      val <- tryCatch(eval(e[[3]], env), error = function(err) NULL)
      assign(nm, val, envir = env)
      globals <- c(globals, sprintf("%-24s — %s%s", nm,
        if (is.null(val)) "<unevaluated>" else paste(class(val), collapse = "/"),
        if (is.environment(val)) " (package-level state environment)" else ""))
    }
  }

  paste0(
    hdr(paste0("DEVELOPER COMMENTARY — ", basename(path)), "="), "\n",
    "Generated ", format(Sys.Date()), " by tools/cmt/generate_cmt.R.\n",
    "Developer-only: never committed to GitHub, never in the CRAN tarball.\n\n",
    hdr("GLOBAL VARIABLES"), "\n",
    if (length(globals)) paste(globals, collapse = "\n") else "(none)", "\n\n",
    hdr("FUNCTIONS"), "\n\n",
    paste(fun_sections, collapse = "\n"))
}

# ---- Prolog files -----------------------------------------------------------

cmt_for_pl <- function(path) {
  lines <- readLines(path, warn = FALSE)
  heads <- grep("^[a-z][a-zA-Z0-9_]*\\(.*(:-|\\.\\s*$)", lines, value = TRUE)
  preds <- unique(sub("\\(.*$", "", heads))
  dyn <- grep("dynamic", lines, value = TRUE)
  paste0(
    hdr(paste0("DEVELOPER COMMENTARY — ", basename(path)), "="), "\n",
    "Generated ", format(Sys.Date()), " by tools/cmt/generate_cmt.R.\n",
    "Developer-only: never committed to GitHub, never in the CRAN tarball.\n\n",
    hdr("GLOBAL VARIABLES (dynamic predicates)"), "\n",
    if (length(dyn)) paste(trimws(dyn), collapse = "\n") else "(none)", "\n\n",
    hdr("PREDICATES"), "\n\n",
    paste(vapply(preds, function(p) paste0(
      p, "/?\n  Description: TODO\n  Arguments: TODO (mode +input / -output / ?both)\n",
      "  References: (none)\n"), character(1)), collapse = "\n"))
}

# ---- drive ------------------------------------------------------------------

targets <- list(
  list(files = c(list.files("pkg/R", "\\.R$", full.names = TRUE),
                 list.files("pkg/inst", "\\.R$", full.names = TRUE,
                            recursive = TRUE)),
       suffix = "_r", make = cmt_for_r),
  list(files = list.files("pkg/inst/prolog", "\\.pl$", full.names = TRUE),
       suffix = "_pl", make = cmt_for_pl))

n_new <- 0L; n_skip <- 0L
for (t in targets) for (f in t$files) {
  out <- file.path(dirname(f),
                   paste0(tools::file_path_sans_ext(basename(f)),
                          t$suffix, ".cmt"))
  if (file.exists(out) && !force) { n_skip <- n_skip + 1L; next }
  txt <- tryCatch(t$make(f), error = function(e) NULL)
  if (is.null(txt)) { message("skipped (parse): ", f); next }
  writeLines(txt, out); n_new <- n_new + 1L
}
cat(sprintf("cmt: %d written, %d existing kept (use --force to regenerate)\n",
            n_new, n_skip))
