#!/usr/bin/env Rscript
# ==============================================================================
# batch_run.R
# ==============================================================================
# Drive the earthUI workflow end-to-end on one or more market areas,
# config-driven. Each market-area job points at a JSON file previously
# exported via `earthUI::export_settings()` (plus an explicit data_file
# path, since the JSON only stores the filename as a key).
#
# Pipeline per market area:
#   import_data()                 — CSV/Excel load + snake_case names
#   compute_sale_age()            — contract_date -> sale_age (if needed)
#   fit_earth()                   — model fit (params from JSON settings)
#   write_fit_log()               — trace log
#   write_earth_output()          — print(model) + summary + varmod
#   auto_export_for_mgcv()        — RDS for mgcvUI consumption
#   compute_intermediate_output() — residual / CQA / contribution columns
#   compute_rca_adjustments()     — appraisal adjustments (if rca.cqa_score)
#   select_sales_grid_comps()     — recommended + other comps
#   render_report()               — optional Quarto report
#
# Usage:
#   Rscript scripts/batch_run.R
#
# Edit the `market_areas` list at the bottom.
# ==============================================================================

suppressPackageStartupMessages({
  library(earthUI)
  library(jsonlite)
})

# -------- turn saved JSON into a flat cfg list --------------------------------

cfg_from_json <- function(json_path, data_file,
                          output_folder = NULL) {
  bundle <- jsonlite::fromJSON(json_path, simplifyVector = FALSE)

  s  <- bundle$settings    %||% list()
  v  <- bundle$variables   %||% list()
  rc <- bundle$rca         %||% list()
  rp <- bundle$reports     %||% list()
  purpose <- bundle$purpose %||% "general"

  # target (stored as a character vector)
  target <- unlist(s$target)

  # Walk variables[col] to derive predictor / categorical / linpred / special
  # / type lists. Exclude the target from predictors.
  predictors   <- character(0)
  categoricals <- character(0)
  linpreds     <- character(0)
  col_specials <- list()
  type_map     <- list()
  for (nm in names(v)) {
    meta <- v[[nm]]
    sp <- meta$special %||% "no"
    # Predictors: inc && not the target && not a display-only column.
    # Matches the UI's JS logic at ui.R:1196 — display_only columns are
    # shown in the data table but excluded from fitting.
    if (isTRUE(meta$inc) && !(nm %in% target) && sp != "display_only") {
      predictors <- c(predictors, nm)
    }
    if (isTRUE(meta$fac)) categoricals <- c(categoricals, nm)
    if (isTRUE(meta$lin)) linpreds     <- c(linpreds, nm)
    if (nzchar(sp) && sp != "no") col_specials[[nm]] <- sp
    if (!is.null(meta$type) && nzchar(meta$type)) type_map[[nm]] <- meta$type
  }

  # Resolve living_area / weight columns from specials
  la_col <- NULL; wt_col <- NULL; contract_col <- NULL
  for (nm in names(col_specials)) {
    if (col_specials[[nm]] == "living_area"  && is.null(la_col))       la_col <- nm
    if (col_specials[[nm]] == "weight"       && is.null(wt_col))       wt_col <- nm
    if (col_specials[[nm]] == "contract_date"&& is.null(contract_col)) contract_col <- nm
  }
  if (is.null(wt_col) && !is.null(s$weights_col) && s$weights_col != "null") {
    wt_col <- s$weights_col
  }

  # Coerce numerics that the UI stores as strings
  as_num <- function(x) {
    if (is.null(x) || identical(x, "")) return(NULL)
    v <- suppressWarnings(as.numeric(x)); if (is.na(v)) NULL else v
  }
  as_int <- function(x) { v <- as_num(x); if (is.null(v)) NULL else as.integer(v) }

  fit_args <- list(
    target          = target,
    predictors      = predictors,
    categoricals    = if (length(categoricals)) categoricals else NULL,
    linpreds        = if (length(linpreds))     linpreds     else NULL,
    type_map        = if (length(type_map))     type_map     else NULL,
    degree          = as_int(s$degree) %||% 1L,
    pmethod         = s$pmethod %||% "backward",
    glm             = if (!is.null(s$glm_family) && s$glm_family != "none")
                        list(family = s$glm_family) else NULL,
    trace           = as_num(s$trace) %||% 0,
    varmod.method   = s$varmod_method %||% "none",
    nprune          = as_int(s$nprune),
    thresh          = as_num(s$thresh),
    penalty         = as_num(s$penalty),
    minspan         = as_num(s$minspan),
    endspan         = as_num(s$endspan),
    fast.k          = as_int(s$fast_k),
    nfold           = as_int(s$nfold_override),
    nk              = as_int(s$nk),
    newvar.penalty  = as_num(s$newvar_penalty),
    fast.beta       = as_num(s$fast_beta),
    ncross          = as_int(s$ncross),
    varmod.exponent = as_num(s$varmod_exponent),
    varmod.conv     = as_num(s$varmod_conv),
    varmod.clamp    = as_num(s$varmod_clamp),
    varmod.minspan  = as_num(s$varmod_minspan),
    Adjust.endspan  = as_num(s$adjust_endspan),
    Exhaustive.tol  = as_num(s$exhaustive_tol),
    stratify        = isTRUE(s$stratify),
    keepxy          = isTRUE(s$keepxy),
    Scale.y         = isTRUE(s$scale_y),
    Auto.linpreds   = isTRUE(s$auto_linpreds),
    Force.weights   = isTRUE(s$force_weights),
    Use.beta.cache  = isTRUE(s$use_beta_cache),
    Force.xtx.prune = isTRUE(s$force_xtx_prune),
    Get.leverages   = isTRUE(s$get_leverages)
  )
  fit_args <- Filter(Negate(is.null), fit_args)

  # Normalize CQA score type ("CQA" / "CQA/sf" -> "cqa" / "cqa_sf")
  raw_type <- rc$cqa_score_type %||% "CQA/sf"
  cqa_type_internal <- if (identical(raw_type, "CQA/sf")) "cqa_sf" else "cqa"

  list(
    data_file         = data_file,
    output_folder     = output_folder %||% (s$output_folder %||%
                                            path.expand("~/earthui_batch")),
    purpose           = purpose,
    skip_subject_row  = isTRUE(s$skip_subject_row),
    effective_date    = if (!is.null(s$effective_date) && nzchar(s$effective_date))
                          as.Date(s$effective_date) else NULL,
    contract_date_col = contract_col,
    living_area_col   = la_col,
    weight_col        = wt_col,
    col_specials      = col_specials,
    subset_expr       = s$subset_arg %||% "",
    cqa_score         = rc$cqa_score,
    cqa_score_type    = cqa_type_internal,
    reports           = tolower(unlist(rp)),
    fit_args          = fit_args
  )
}

# -------- run one market area -------------------------------------------------

run_market_area <- function(cfg) {
  t0 <- Sys.time()
  # stderr is unbuffered — use it for progress/diagnostics so output
  # appears immediately even when stdout is piped.
  logf <- function(...) cat(..., file = stderr())
  logf(sprintf("\n==== %s ====\n[START] %s\n",
               cfg$data_file,
               format(t0, "%Y-%m-%d %H:%M:%S")))
  logf("[stage] A post-START\n")
  if (!dir.exists(cfg$output_folder)) {
    dir.create(cfg$output_folder, recursive = TRUE)
  }
  logf("[stage] B post-mkdir\n")

  # Background ticker — prints "[+Ns elapsed]" every 10 seconds while the
  # rest of this function runs. Unix/macOS only.
  # Write PID to a temp file so we don't need intern=TRUE (which would
  # block waiting for EOF on R's stdout pipe). Subshell redirects stdin
  # and stdout away from R; stderr stays so ticks reach the terminal.
  ticker_pid <- NULL
  if (.Platform$OS.type == "unix") {
    pid_file <- tempfile("ticker_", fileext = ".pid")
    cmd <- paste0(
      "( while :; do sleep 10; printf '  [+%ds elapsed]\\n' $SECONDS 1>&2; done ",
      "</dev/null >/dev/null ) & echo $! > ", shQuote(pid_file)
    )
    system(cmd, wait = FALSE)
    # Small grace period for the PID file to appear
    for (i in 1:50) {
      if (file.exists(pid_file) && file.info(pid_file)$size > 0L) break
      Sys.sleep(0.02)
    }
    if (file.exists(pid_file)) {
      ticker_pid <- suppressWarnings(as.integer(readLines(pid_file, warn = FALSE)[1L]))
      unlink(pid_file)
    }
  }
  logf("[stage] C post-ticker pid=", ticker_pid %||% "NULL", "\n", sep="")
  on.exit({
    if (!is.null(ticker_pid) && !is.na(ticker_pid)) {
      # Use shell `kill` — more reliable than tools::pskill on nested
      # shell subprocesses. SIGTERM then SIGKILL for good measure.
      tryCatch(system(sprintf("kill %d 2>/dev/null; sleep 0.2; kill -9 %d 2>/dev/null",
                              ticker_pid, ticker_pid), wait = TRUE),
               error = function(e) NULL)
    }
    t1 <- Sys.time()
    cat(sprintf("[END]   %s   total %.1fs\n",
                format(t1, "%Y-%m-%d %H:%M:%S"),
                as.numeric(difftime(t1, t0, units = "secs"))),
        file = stderr())
  }, add = TRUE)

  # 1. Load
  logf("[stage] import_data ...\n")
  data <- import_data(cfg$data_file)
  logf("[stage] imported nrow=", nrow(data), " ncol=", ncol(data), "\n", sep="")

  # 2. Optional subset filter
  if (nzchar(cfg$subset_expr %||% "")) {
    logf("[stage] applying subset_expr\n")
    mask <- as.list(data)
    mask[["TRUE"]] <- TRUE; mask[["FALSE"]] <- FALSE
    mask$as.Date <- as.Date; mask$as.POSIXct <- as.POSIXct
    rows <- eval(parse(text = cfg$subset_expr), envir = mask, enclos = emptyenv())
    rows[is.na(rows)] <- FALSE
    data <- data[rows, , drop = FALSE]
  }

  # 3. Derive sale_age if not already present
  if (!is.null(cfg$contract_date_col) && !is.null(cfg$effective_date) &&
      !("sale_age" %in% names(data))) {
    logf("[stage] computing sale_age\n")
    data[["sale_age"]] <- compute_sale_age(
      data[[cfg$contract_date_col]],
      cfg$effective_date
    )
  }

  # 4. Subject-row handling for fitting
  skip_subject <- cfg$purpose == "appraisal" ||
                  (cfg$purpose == "market" && isTRUE(cfg$skip_subject_row))
  fit_data <- if (skip_subject && nrow(data) >= 2L) {
    data[-1L, , drop = FALSE]
  } else data
  logf("[stage] fit_data nrow=", nrow(fit_data), "\n", sep="")

  # 5. Fit (weights come from the resolved weight column, if any)
  fit_args <- cfg$fit_args
  if (!is.null(cfg$weight_col) && cfg$weight_col %in% names(fit_data)) {
    fit_args$weights <- fit_data[[cfg$weight_col]]
  }

  # --- Debug dump: exactly what we send to fit_earth (stderr = unbuffered) ---
  logf("--- fit_args summary ---\n")
  logf("rows=", nrow(fit_data), "  target=", paste(fit_args$target, collapse=","),
       "  predictors(", length(fit_args$predictors), ")=",
       paste(fit_args$predictors, collapse=","), "\n", sep="")
  logf("degree=", fit_args$degree %||% 1,
       "  pmethod=", fit_args$pmethod %||% "(default)",
       "  glm=", if (!is.null(fit_args$glm)) fit_args$glm$family else "none",
       "  nk=", fit_args$nk %||% "default",
       "  nprune=", fit_args$nprune %||% "auto",
       "  pen=", fit_args$penalty %||% "default",
       "  minspan=", fit_args$minspan %||% "default",
       "  endspan=", fit_args$endspan %||% "default",
       "  fast.k=", fit_args$fast.k %||% "default", "\n", sep="")
  logf("CV: nfold=", fit_args$nfold %||% 0, "  ncross=", fit_args$ncross %||% 1,
       "  stratify=", isTRUE(fit_args$stratify),
       "  varmod=", fit_args$varmod.method %||% "none", "\n", sep="")
  logf("categoricals(", length(fit_args$categoricals %||% character()), ")=",
       paste(fit_args$categoricals %||% character(), collapse=","),
       "  linpreds(", length(fit_args$linpreds %||% character()), ")=",
       paste(fit_args$linpreds %||% character(), collapse=","), "\n", sep="")
  if (!is.null(fit_args$weights)) {
    w <- fit_args$weights
    logf("weights: n=", length(w), " unique=", length(unique(w)),
         " n_zero=", sum(w == 0, na.rm=TRUE), "\n", sep="")
  } else {
    logf("weights: NULL\n")
  }
  logf("------------------------\n")

  result <- do.call(fit_earth, c(list(df = fit_data), fit_args))

  # 6. Logs + RDS
  base   <- tools::file_path_sans_ext(basename(cfg$data_file))
  traces <- result$trace_output %||% character(0)
  write_fit_log(cfg$output_folder, traces, base)
  write_earth_output(result, cfg$output_folder, base)
  auto_export_for_mgcv(result, cfg$output_folder, base)

  # 7. Intermediate output
  io_df <- compute_intermediate_output(
    data             = data,
    result           = result,
    purpose          = cfg$purpose,
    skip_subject_row = isTRUE(cfg$skip_subject_row),
    living_area_col  = cfg$living_area_col
  )
  if (requireNamespace("writexl", quietly = TRUE)) {
    io_path <- file.path(cfg$output_folder, paste0(base, "_intermediate.xlsx"))
    writexl::write_xlsx(io_df, io_path)
    cat("wrote:", io_path, "\n")
  }

  # 8. RCA (only if appraisal and a CQA score is supplied)
  rca_df <- NULL
  if (cfg$purpose == "appraisal" && !is.null(cfg$cqa_score)) {
    rca_df <- compute_rca_adjustments(
      data            = data,
      result          = result,
      user_cqa        = cfg$cqa_score,
      cqa_type        = cfg$cqa_score_type,
      living_area_col = cfg$living_area_col,
      weight_col      = cfg$weight_col
    )
    if (requireNamespace("writexl", quietly = TRUE)) {
      rca_path <- file.path(cfg$output_folder, paste0(base, "_adjusted.xlsx"))
      writexl::write_xlsx(rca_df, rca_path)
      cat("wrote:", rca_path, "\n")
    }
  } else if (cfg$purpose == "appraisal") {
    cat("skipped RCA: cqa_score is null in config\n")
  }

  # 9. Sales grid comp selection + workbook
  if (!is.null(rca_df)) {
    sel <- select_sales_grid_comps(rca_df)
    cat(sprintf("sales-grid comps: %d recommended, %d other eligible\n",
                nrow(sel$recommended), nrow(sel$others)))
    if (nrow(sel$recommended) > 0L &&
        requireNamespace("openxlsx", quietly = TRUE)) {
      sg_path <- file.path(cfg$output_folder, paste0(base, "_salesgrid.xlsx"))
      tryCatch({
        build_sales_grid(
          rca_df      = rca_df,
          comp_rows   = sel$recommended$row,
          output_file = sg_path,
          specials    = cfg$col_specials %||% list()
        )
        cat("wrote:", sg_path, "\n")
      }, error = function(e) {
        cat(sprintf("ERROR building sales grid: %s\n", conditionMessage(e)))
      })
    }
  }

  # 10. Reports — render each format listed in the JSON `reports` array
  for (fmt in cfg$reports) {
    ext <- switch(fmt, pdf = ".pdf", docx = ".docx", html = ".html", NA_character_)
    if (is.na(ext)) {
      cat("skipped report: unknown format '", fmt, "'\n", sep = "")
      next
    }
    out <- file.path(cfg$output_folder, paste0(base, ext))
    cat("rendering", fmt, "report ...\n")
    tryCatch({
      render_report(result, output_format = fmt, output_file = out)
      cat("wrote:", out, "\n")
    }, error = function(e) {
      cat(sprintf("ERROR rendering %s: %s\n", fmt, conditionMessage(e)))
    })
  }

  invisible(list(result = result, intermediate = io_df, rca = rca_df))
}

# ------------------------------------------------------------------------------
# Market-area jobs. Each entry points at a JSON previously exported from the
# UI, plus the actual data file to run against. Duplicate for more areas.
# ------------------------------------------------------------------------------

market_areas <- list(

  list(
    config_json = path.expand("~/earthui_configs/Appraisal_1.json"),
    data_file   = "/Volumes/Nvme_1/ClaudeCode/earthUI/demo_mls/Appraisal_1.csv"
    # output_folder = ...   # optional override; defaults to JSON's output_folder
  )

  # , list(
  #   config_json = path.expand("~/earthui_configs/Market_B.json"),
  #   data_file   = "/path/to/Market_B.csv"
  # )

)

for (job in market_areas) {
  cfg <- cfg_from_json(job$config_json,
                       data_file     = job$data_file,
                       output_folder = job$output_folder)
  tryCatch(
    run_market_area(cfg),
    error = function(e) {
      cat(sprintf("ERROR on %s: %s\n", cfg$data_file, conditionMessage(e)))
    }
  )
}
