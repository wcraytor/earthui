function(input, output, session) {

  # --- Plot dimension helper (clientData width/height, fixed res=96) ---
  plot_dims_ <- function(session, id) {
    full_id <- if (!is.null(session$ns)) session$ns(id) else id
    w_key <- paste0("output_", full_id, "_width")
    h_key <- paste0("output_", full_id, "_height")
    list(
      width  = function() session$clientData[[w_key]],
      height = function() session$clientData[[h_key]]
    )
  }

  # --- Nord theme switching ---
  shiny::observe({
    mode <- input$dark_mode
    shiny::req(mode)
    message("[glmnetUI] Theme switch requested: mode = ", mode)
    new_theme <- if (mode == "dark") nord_dark else nord_light
    message("[glmnetUI] Theme object class: ", paste(class(new_theme), collapse = ", "))
    tryCatch({
      session$setCurrentTheme(new_theme)
      message("[glmnetUI] Theme switch completed successfully")
    },
      error = function(e) {
        message("Theme switch error (non-fatal): ", conditionMessage(e))
      }
    )
  })

  # --- Locale handling ---
  # Restore saved locale defaults from localStorage
  shiny::observeEvent(input$glmnet_locale_defaults, {
    ld <- input$glmnet_locale_defaults
    if (!is.null(ld$locale_country))
      shiny::updateSelectInput(session, "locale_country", selected = ld$locale_country)
    if (!is.null(ld$locale_paper))
      shiny::updateSelectInput(session, "locale_paper", selected = ld$locale_paper)
    if (!is.null(ld$locale_import))
      shiny::updateSelectInput(session, "data-locale_import", selected = ld$locale_import)
  })

  # When Settings country changes, sync import locale and paper
  shiny::observeEvent(input$locale_country, {
    country <- input$locale_country %||% "us"
    presets <- earthUI:::locale_country_presets_()
    preset <- presets[[country]] %||% presets[["us"]]
    shiny::updateSelectInput(session, "locale_paper", selected = preset$paper)
    shiny::updateSelectInput(session, "data-locale_import", selected = country)
    earthUI:::set_locale_(country)
  })

  # Sync import locale with settings locale
  shiny::observe({
    import_country <- input[["data-locale_import"]] %||%
      input$locale_country %||% "us"
    settings_country <- input$locale_country %||% "us"
    paper <- input$locale_paper %||% "letter"
    presets <- earthUI:::locale_country_presets_()
    import_preset <- presets[[import_country]] %||% presets[["us"]]
    settings_preset <- presets[[settings_country]] %||% presets[["us"]]
    earthUI:::set_locale_(settings_country,
                           csv_sep = import_preset$csv_sep,
                           csv_dec = import_preset$csv_dec,
                           big_mark = settings_preset$big_mark,
                           dec_mark = settings_preset$dec_mark,
                           date_fmt = import_preset$date_fmt,
                           paper = paper)
  })

  # Settings "Save": persist locale defaults + the regProj root, and keep the
  # Settings panel open so the user can review/adjust the other fields.
  shiny::observeEvent(input$settings_save, {
    # 1) Locale defaults -> localStorage
    session$sendCustomMessage("save_locale_defaults", list(
      locale_country = input$locale_country,
      locale_paper   = input$locale_paper,
      locale_import  = input[["data-locale_import"]]
    ))
    # 2) regProj root -> per-user prefs (only if a non-empty, usable path)
    p <- trimws(input$regproj_root %||% "")
    root_msg <- ""
    if (nzchar(p)) {
      p <- path.expand(p)
      ok <- dir.exists(p) || tryCatch({ dir.create(p, recursive = TRUE); TRUE },
                                      error = function(e) FALSE,
                                      warning = function(w) FALSE)
      if (ok && dir.exists(p)) {
        prefs <- earthui_prefs_read()
        prefs$regproj_root <- p
        earthui_prefs_write(prefs)
        rv_proj$project_refresh_token <- rv_proj$project_refresh_token + 1L
        root_msg <- paste0(" regProj root: ", p)
      } else {
        shiny::showNotification(sprintf("Cannot create or access: %s", p),
                                type = "error", duration = 6)
        return()
      }
    }
    shiny::showNotification(paste0("Settings saved.", root_msg),
                            type = "message", duration = 4)
  })

  # Settings "Close": dismiss the Settings panel (does not save).
  shiny::observeEvent(input$settings_close, {
    session$sendCustomMessage("close_settings_dropdown", list())
  })

  # Help / technical-support dialog (emails support@valuation-engineer.com)
  shiny::observeEvent(input$show_help, {
    session$sendCustomMessage("close_settings_dropdown", list())
    shiny::showModal(earthUI:::support_request_modal_("glmnetUI"))
  })

  # About / disclaimer dialog
  shiny::observeEvent(input$show_about, {
    session$sendCustomMessage("close_settings_dropdown", list())
    shiny::showModal(shiny::modalDialog(
      title = shiny::HTML("&#9888; About glmnetUI"),
      size = "l", easyClose = TRUE,
      earthUI:::appraisal_disclaimer_html_(),
      shiny::tags$hr(),
      shiny::tags$p(class = "text-muted small",
                    sprintf("glmnetUI %s - AGPL-3.0 license.",
                            as.character(utils::packageVersion("earthUI")))),
      footer = shiny::modalButton("Close")))
  })

  # Wrap global inputs as reactives for module consumption
  purpose <- shiny::reactive(input$purpose)
  effective_date <- shiny::reactive(input$effective_date)
  skip_first_row <- shiny::reactive({
    if (identical(input$purpose, "appraisal")) TRUE
    else isTRUE(input$skip_subject_row)
  })

  # ===== regProj project state (shared root + DBs with earthUI/mgcvUI) =====
  rv_proj <- shiny::reactiveValues(
    active_project        = NULL,   # one-row data.frame or NULL
    project_sort          = "recent",
    project_refresh_token = 0L
  )
  active_project_r <- shiny::reactive(rv_proj$active_project)

  # Per-session DB connections (shared geo.sqlite / projects.sqlite).
  geo_con_      <- tryCatch(regproj_geo_db_connect(),
                            error = function(e) NULL)
  projects_con_ <- tryCatch(regproj_projects_db_connect(),
                            error = function(e) NULL)
  session$onSessionEnded(function() {
    if (!is.null(geo_con_))      try(DBI::dbDisconnect(geo_con_),      silent = TRUE)
    if (!is.null(projects_con_)) try(DBI::dbDisconnect(projects_con_), silent = TRUE)
  })

  output$has_active_project <- shiny::reactive(!is.null(rv_proj$active_project))
  shiny::outputOptions(output, "has_active_project", suspendWhenHidden = FALSE)

  earth_mod <- earthImportServer("earth")
  earth_import_r <- earth_mod$earth_import
  data_out <- dataImportServer("data", purpose, active_project_r,
                               earth_import_r)
  model_out <- modelingServer("model", data_out, purpose, effective_date,
                               skip_first_row, earth_import_r)

  # Trilogy mode: after each fit, register this method's fit-stamp in the
  # project's trilogy.json so the combined report can group the run's files.
  shiny::observeEvent(model_out$fit_count(), {
    ctx <- getOption("glmnetUI.trilogy")
    if (is.null(ctx) || !isTRUE(model_out$fit_count() > 0L)) return()
    pp <- ctx$project_path %||% NULL
    ts <- model_out$fit_ts()
    if (!is.null(pp) && !is.null(ts)) {
      try(earthUI::trilogy_register_fit(pp, "glmnet",
            earthUI:::fit_stamp_(ts)), silent = TRUE)
    }
  }, ignoreInit = TRUE)

  coef_out <- coefficientsServer("coefs", model_out, data_out)
  equationServer("eq", model_out, data_out)
  summaryServer("summ", model_out, data_out, purpose)
  correlationServer("corr", data_out)
  importanceServer("imp", model_out, data_out)
  contributionsServer("contrib", model_out, data_out)
  anovaServer("anova", model_out, data_out)
  diagnosticsServer("diag", model_out)
  reportServer("report", model_out, coef_out, data_out)

  # When the active project changes (open / close / switch): set the hidden
  # Purpose radio + the derived output folder (<os>_out_glmnet) and reset all
  # per-file state. mod_data picks up the project's last-used file and loads
  # it. Forward references to rv_rca are fine — this observer fires during
  # flush, after the server function has finished defining it.
  shiny::observeEvent(rv_proj$active_project, ignoreNULL = FALSE, {
    p <- rv_proj$active_project
    pur <- if (is.null(p)) "general" else
      switch(p$purpose, gen = "general", appr = "appraisal",
             mktarea = "market", "general")
    shiny::updateRadioButtons(session, "purpose", selected = pur)

    # earthUI is the source of truth for the Effective Date: apply earthUI's
    # saved value on every project open (the JS no longer restores it from
    # localStorage, so earthUI always wins). Falls back to the input default
    # (today) when earthUI has none.
    cf_proj <- earthUI::earth_carryforward_(
      if (is.null(p)) NULL else p$project_path, pur)
    if (!is.null(cf_proj$effective_date))
      shiny::updateDateInput(session, "effective_date",
                             value = cf_proj$effective_date)

    data_out$rv$data <- NULL
    data_out$rv$file_name <- NULL
    data_out$rv$col_types <- NULL
    data_out$rv$sheets <- NULL
    earth_mod$reset()
    model_out$reset()
    rv_rca$pct_data <- NULL
    rv_rca$rca_df <- NULL
    rv_rca$sg_recommended <- NULL
    rv_rca$sg_others <- NULL
    # Reset earth path display
    session$sendCustomMessage("glmnet-update-input",
                              list(id = "earth-earth_path_display", value = ""))

    if (is.null(p)) {
      shiny::updateTextInput(session, "output_folder", value = "")
    } else {
      parsed <- regproj_parse_flat(basename(p$project_path))
      # Derive the output folder from the project's ACTUAL path so it lands
      # under the project's real regProj root. (regproj_path() recomputes from
      # default_regproj_root(), which sends output to the wrong root when the
      # active project lives under a custom root.)
      out_dir <- file.path(p$project_path, paste0(os_detect(), "_out_glmnet"))
      if (!dir.exists(out_dir))
        dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)
      shiny::updateTextInput(session, "output_folder", value = out_dir)
      # Keep the projects DB row fresh (shared with earthUI/mgcvUI).
      tryCatch(
        register_project(p$project_path, p$purpose, parsed$country,
                         parsed$levels, parsed$project_name),
        error = function(e) NULL)
    }
  })

  # --- Model fitted flag for conditionalPanel ---
  output$model_fitted <- shiny::reactive(isTRUE(model_out$fitted()))
  shiny::outputOptions(output, "model_fitted", suspendWhenHidden = FALSE)

  # --- Glmnet Output tab ---
  output$glmnet_output <- shiny::renderPrint({
    shiny::req(model_out$fitted())
    model <- model_out$model()
    lambda <- model_out$lambda()
    gamma <- model_out$gamma()
    seed <- model_out$seed_used()

    if (!is.null(seed)) {
      cat(sprintf("== Random Seed: %d ==\n\n", seed))
    }

    cat("== Model ==\n\n")
    print(model)

    cat("\n\n== Selected Lambda ==\n\n")
    cat(sprintf("Lambda: %s\n", signif(lambda, 6)))
    if (inherits(model, "cv.glmnet") || inherits(model, "cv.relaxed")) {
      cat(sprintf("lambda.min: %s\n", signif(model$lambda.min, 6)))
      cat(sprintf("lambda.1se: %s\n", signif(model$lambda.1se, 6)))
    }
    if (!is.null(gamma)) {
      cat(sprintf("Gamma (relaxed): %s\n", signif(gamma, 4)))
    }

    cat("\n\n== Coefficients ==\n\n")
    coef_args <- list(model, s = lambda)
    if (!is.null(gamma)) coef_args$gamma <- gamma
    coef_sparse <- do.call(stats::coef, coef_args)
    print(coef_sparse)
  })

  # --- RCA percentage data (stored after RCA export) ---
  rv_rca <- shiny::reactiveValues(pct_data = NULL, rca_df = NULL)

  output$rca_computed <- shiny::reactive(!is.null(rv_rca$pct_data))
  shiny::outputOptions(output, "rca_computed", suspendWhenHidden = FALSE)

  # ===== Settings: regProj root folder =====

  # Drive browser (shared by regProj root + the Convert .qmd picker).
  volumes <- c(Home = path.expand("~"), shinyFiles::getVolumes()())
  if (.Platform$OS.type == "unix" && dir.exists("/Volumes"))
    volumes <- c(volumes, Volumes = "/Volumes")
  if (.Platform$OS.type == "unix" && dir.exists("/media"))
    volumes <- c(volumes, Media = "/media")
  if (.Platform$OS.type == "unix" && dir.exists("/mnt"))
    volumes <- c(volumes, Mounts = "/mnt")

  # Populate the regProj root field once at session start.
  shiny::observe({
    shiny::isolate({
      if (is.null(input$regproj_root) || !nzchar(input$regproj_root %||% ""))
        shiny::updateTextInput(session, "regproj_root",
                               value = default_regproj_root())
    })
  })

  shinyFiles::shinyDirChoose(input, "regproj_root_browse", roots = volumes,
                             session = session,
                             restrictions = system.file(package = "base"))
  shiny::observeEvent(input$regproj_root_browse, {
    d <- shinyFiles::parseDirPath(volumes, input$regproj_root_browse)
    if (length(d) > 0 && nzchar(d))
      shiny::updateTextInput(session, "regproj_root", value = as.character(d))
  })

  # ===== Section 1: Project picker / creator =====

  # Pending location prefill for the New Project modal.
  np_prefill_ <- shiny::reactiveVal(NULL)

  level_has_shipped_ <- function(country, level_idx) {
    country == "us" && level_idx %in% c(1L, 2L)
  }

  # Choices for a level dropdown: "Full Name (code)" -> code.
  build_level_choices_ <- function(country, parent_codes, level_idx) {
    if (is.null(geo_con_)) return(character(0))
    parent_path <- paste(parent_codes, collapse = "/")
    res <- DBI::dbGetQuery(geo_con_,
      "SELECT code, name FROM admin_entries
        WHERE country = ? AND level = ? AND parent_codes = ?
        ORDER BY name",
      params = list(country, as.integer(level_idx), parent_path))
    if (nrow(res) == 0L) return(character(0))
    stats::setNames(res$code, paste0(res$name, " (", res$code, ")"))
  }

  projects_df_ <- shiny::reactive({
    rv_proj$project_refresh_token
    regproj_list_projects(sort_by = rv_proj$project_sort)
  })

  output$regproj_project_ui <- shiny::renderUI({
    df <- projects_df_()
    active <- rv_proj$active_project
    if (!is.null(active)) {
      pur_label <- switch(active$purpose, gen = "General", appr = "Appraisal",
                          mktarea = "Market Area", active$purpose)
      loc_friendly <- paste(pur_label, "·", toupper(active$country), "·",
                            toupper(active$state), "·", active$county, "·",
                            active$city)
      return(shiny::tagList(
        shiny::tags$div(style = "margin-bottom:4px;",
                        shiny::strong(active$project_name)),
        shiny::tags$div(class = "small text-muted",
                 style = "margin-bottom:8px; word-break: break-all;",
                 loc_friendly),
        shiny::tags$div(style = "display:flex; gap:6px; flex-wrap: wrap;",
          shiny::actionButton("regproj_project_close", "Close Project",
                       class = "btn btn-outline-secondary btn-sm"),
          shiny::actionButton("regproj_project_info", "Info",
                       class = "btn btn-outline-secondary btn-sm"),
          shiny::actionButton("regproj_project_new", "+ New…",
                       class = "btn btn-outline-primary btn-sm"))
      ))
    }
    if (nrow(df) == 0L) {
      return(shiny::tagList(
        shiny::tags$div(class = "small text-muted", style = "margin-bottom:8px;",
                 "(no projects yet — click + New to create one)"),
        shiny::actionButton("regproj_project_new", "+ New…",
                     class = "btn btn-outline-primary btn-sm")))
    }
    labels <- paste0(df$project_name, "  —  ", df$purpose, " / ", df$country,
                     " / ", df$state, " / ", df$county, " / ", df$city)
    choices <- stats::setNames(df$project_path, labels)
    shiny::tagList(
      shiny::selectizeInput("regproj_project_pick", NULL,
                     choices = c("(select a project)" = "", choices),
                     selected = "", width = "100%"),
      shiny::tags$div(style = "display:flex; gap:6px; align-items:center;",
        shiny::radioButtons("regproj_project_sort", NULL,
                     choices = c("Recent" = "recent", "A-Z" = "alpha"),
                     selected = rv_proj$project_sort, inline = TRUE),
        shiny::actionButton("regproj_project_new", "+ New…",
                     class = "btn btn-outline-primary btn-sm"))
    )
  })

  shiny::observeEvent(input$regproj_project_sort, {
    rv_proj$project_sort <- input$regproj_project_sort
  }, ignoreInit = TRUE)

  shiny::observeEvent(input$regproj_project_pick, {
    p <- input$regproj_project_pick
    if (is.null(p) || !nzchar(p)) return()
    df <- projects_df_()
    row <- df[df$project_path == p, , drop = FALSE]
    if (nrow(row) == 1L) rv_proj$active_project <- row
  }, ignoreInit = TRUE)

  shiny::observeEvent(input$regproj_project_close, {
    rv_proj$active_project <- NULL
  })

  # Trilogy mode: auto-open the trilogy project at startup, so the project's
  # data is available and the fit registers to the right project.
  shiny::observeEvent(TRUE, {
    ctx <- getOption("glmnetUI.trilogy")
    pp <- ctx$project_path %||% NULL
    if (is.null(pp)) return()
    df <- tryCatch(regproj_list_projects(sort_by = "recent"),
                   error = function(e) NULL)
    if (is.data.frame(df)) {
      row <- df[df$project_path == pp, , drop = FALSE]
      if (nrow(row) == 1L) rv_proj$active_project <- row
    }
  }, once = TRUE)

  shiny::observeEvent(input$regproj_project_new, {
    src <- rv_proj$active_project
    if (is.null(src)) {
      recent <- tryCatch(regproj_list_projects(sort_by = "recent"),
                         error = function(e) NULL)
      if (!is.null(recent) && nrow(recent) > 0L)
        src <- recent[1L, , drop = FALSE]
    }
    parsed <- if (!is.null(src))
                regproj_parse_flat(basename(src$project_path)) else NULL
    pf_country <- if (!is.null(parsed)) parsed$country else "us"
    pf_levels  <- if (!is.null(parsed)) as.character(parsed$levels) else character(0)
    np_prefill_(if (length(pf_levels) > 0L)
                  list(country = pf_country, levels = pf_levels) else NULL)
    shiny::showModal(shiny::modalDialog(
      title = "Create New Project", size = "l", easyClose = FALSE,
      footer = shiny::tagList(shiny::modalButton("Cancel"),
        shiny::actionButton("np_create", "Create Project", class = "btn-primary")),
      shiny::tags$div(class = "small text-muted", style = "margin-bottom: 12px;",
        "Country, State, County, City, and Purpose are locked once the project is created."),
      shiny::textInput("np_project_name", "Project Name * (max 8 chars)",
                placeholder = "max 8 chars; a date-time prefix is added automatically",
                width = "100%"),
      shiny::radioButtons("np_purpose", "Purpose *",
                   choices = c("General" = "gen", "For Appraisal" = "appr",
                               "Market Area Analysis" = "mktarea"),
                   selected = "appr", inline = TRUE),
      shiny::hr(), shiny::tags$h5("Project Location"),
      shiny::uiOutput("np_country_ui"), shiny::uiOutput("np_levels_ui"),
      shiny::hr(), shiny::tags$h5("Initial Data File", style = "margin-bottom:4px;"),
      shiny::tags$div(class = "small text-muted", style = "margin-bottom:8px;",
        "Optional — copied into the project's in/ folder; the name is preserved."),
      shiny::fileInput("np_data_file", NULL, accept = c(".csv", ".xlsx", ".xls"),
                width = "100%")
    ))
  })

  output$np_country_ui <- shiny::renderUI({
    ref <- regproj_reference()
    labels <- paste0(unlist(ref$countries), " (", names(ref$countries), ")")
    choices <- stats::setNames(names(ref$countries), labels)
    pf <- np_prefill_()
    sel_cc <- if (!is.null(pf) && nzchar(pf$country %||% "")) pf$country else "us"
    shiny::selectInput("np_country", "Country *", choices = choices,
                selected = sel_cc, width = "100%")
  })

  output$np_levels_ui <- shiny::renderUI({
    cc <- input$np_country %||% "us"
    schema <- country_schema(cc)
    if (length(schema) == 0L)
      return(shiny::tags$div(class = "small text-muted",
                      "(no admin levels for this country)"))
    pf <- np_prefill_()
    pf_levels <- if (!is.null(pf) && identical(pf$country, cc))
                   pf$levels else character(0)
    last_idx <- length(schema)
    parent_codes <- character(0)
    inputs <- lapply(seq_along(schema), function(i) {
      lbl <- tools::toTitleCase(gsub("_", " ", schema[i]))
      if (i < last_idx) {
        sel_val <- input[[paste0("np_level_", i)]]
        if ((is.null(sel_val) || !nzchar(sel_val)) &&
            length(pf_levels) >= i && nzchar(pf_levels[i]))
          sel_val <- pf_levels[i]
        if (!is.null(sel_val) && nzchar(sel_val)) parent_codes[i] <<- sel_val
        sel <- if (!is.null(sel_val) && nzchar(sel_val)) sel_val else NULL
      } else {
        sel <- NULL
        if (length(pf_levels) >= i && nzchar(pf_levels[i]) &&
            (i == 1L || isTRUE(all(parent_codes[seq_len(i - 1L)] ==
                                   pf_levels[seq_len(i - 1L)]))))
          sel <- pf_levels[i]
      }
      ch <- build_level_choices_(cc, parent_codes[seq_len(i - 1L)], i)
      if (level_has_shipped_(cc, i)) {
        shiny::selectInput(paste0("np_level_", i), paste0(lbl, " *"),
                    choices = c("(pick…)" = "", ch), selected = sel,
                    width = "100%")
      } else if (i == last_idx) {
        ch_pairs <- ch
        names_only <- character(0)
        choices <- if (length(ch_pairs) == 0L) {
          c("(pick a city or type new)" = "")
        } else {
          labels <- names(ch_pairs)
          names_only <- sub(" \\([^)]*\\)$", "", labels)
          c("(pick a city or type new)" = "", stats::setNames(names_only, labels))
        }
        sel_city <- ""
        if (!is.null(sel) && nzchar(sel) && length(ch_pairs) > 0L) {
          midx <- match(sel, unname(ch_pairs))
          if (!is.na(midx)) sel_city <- names_only[midx]
        }
        shiny::selectizeInput(paste0("np_level_", i), paste0(lbl, " *"),
                       choices = choices, selected = sel_city,
                       options = list(create = TRUE,
                         placeholder = "Pick existing or type a new full name"),
                       width = "100%")
      } else {
        shiny::selectizeInput(paste0("np_level_", i), paste0(lbl, " *"),
                       choices = ch, selected = sel,
                       options = list(create = TRUE,
                         placeholder = "Pick existing or type a new name"),
                       width = "100%")
      }
    })
    do.call(shiny::tagList, inputs)
  })

  shiny::observeEvent(input$np_create, {
    cc <- input$np_country %||% ""
    schema <- country_schema(cc)
    last_idx <- length(schema)
    pur <- input$np_purpose %||% "appr"
    # Validate the typed name (<= 8 chars) and prepend the creation timestamp.
    # Country-agnostic: works for any country's admin-level depth.
    proj <- tryCatch(
      regproj_new_project_name(trimws(input$np_project_name %||% "")),
      error = function(e) {
        shiny::showNotification(conditionMessage(e),
                                type = "error", duration = 5)
        NULL
      })
    if (is.null(proj)) return()
    if (length(schema) == 0L) {
      shiny::showNotification("This country has no admin levels defined.",
                       type = "error", duration = 5); return()
    }
    levels <- character(length(schema))
    parent_codes <- character(0)
    for (i in seq_along(schema)) {
      val <- input[[paste0("np_level_", i)]]
      val <- if (is.null(val)) "" else trimws(as.character(val))
      if (level_has_shipped_(cc, i)) {
        if (!nzchar(val)) {
          shiny::showNotification(sprintf("Missing %s.", schema[i]),
                           type = "error", duration = 5); return()
        }
        levels[i] <- val
      } else if (i == last_idx) {
        full_name <- val
        if (!nzchar(full_name)) {
          shiny::showNotification("City is required.", type = "error", duration = 5); return()
        }
        scope <- paste(c(cc, parent_codes), collapse = "/")
        existing_code <- regproj_index_get(scope, full_name)
        if (!is.null(existing_code)) {
          levels[i] <- existing_code
        } else {
          existing <- unname(unlist(build_level_choices_(cc, parent_codes, i)))
          new_code <- city_abbreviation(full_name, existing)
          regproj_index_put(scope, full_name, new_code)
          levels[i] <- new_code
        }
      } else {
        ch <- build_level_choices_(cc, parent_codes, i)
        if (val %in% ch) {
          levels[i] <- val
        } else {
          existing <- unname(unlist(ch))
          code <- city_abbreviation(val, existing)
          regproj_index_put(paste(c(cc, parent_codes), collapse = "/"), val, code)
          levels[i] <- code
        }
      }
      parent_codes[i] <- levels[i]
    }
    flat <- tryCatch(regproj_flat_segment(cc, levels, proj),
                     error = function(e) {
                       shiny::showNotification(paste("Path error:", conditionMessage(e)),
                                        type = "error", duration = 5); NULL
                     })
    if (is.null(flat)) return()
    proj_root <- file.path(default_regproj_root(), pur, flat)
    if (dir.exists(proj_root)) {
      shiny::showNotification(sprintf("A project named '%s' already exists in this location.", proj),
                       type = "error", duration = 5); return()
    }
    # Create the full multi-OS / multi-method tree so any sibling app can use it.
    in_dir <- NULL
    for (os_seg in c("mac", "ubuntu", "win11")) {
      io_in <- regproj_path(pur, cc, levels, proj, os = os_seg,
                            in_or_out = "in", create = TRUE)
      for (m in c("earth", "glmnet", "mgcv", "combined"))
        regproj_path(pur, cc, levels, proj, os = os_seg,
                     in_or_out = "out", method = m, create = TRUE)
      if (os_seg == os_detect()) in_dir <- io_in
    }
    fi <- input$np_data_file
    last_file <- NULL
    if (!is.null(fi) && nrow(fi) >= 1L && !is.null(in_dir)) {
      last_file <- fi$name[1L]
      file.copy(fi$datapath[1L], file.path(in_dir, last_file), overwrite = FALSE)
    }
    tryCatch(register_project(proj_root, pur, cc, levels, proj,
                              last_file = last_file),
             error = function(e) NULL)
    np_prefill_(NULL)
    rv_proj$project_refresh_token <- rv_proj$project_refresh_token + 1L
    df <- regproj_list_projects(sort_by = rv_proj$project_sort)
    new_row <- df[df$project_path == proj_root, , drop = FALSE]
    if (nrow(new_row) == 1L) rv_proj$active_project <- new_row
    shiny::removeModal()
    shiny::showNotification(sprintf("Created project '%s'", proj),
                     type = "message", duration = 6)
  })

  shiny::observeEvent(input$regproj_project_info, {
    p <- rv_proj$active_project
    if (is.null(p)) return()
    pur_label <- switch(p$purpose, gen = "General", appr = "For Appraisal",
                        mktarea = "Market Area Analysis", p$purpose)
    shiny::showModal(shiny::modalDialog(
      title = "Project Info", easyClose = TRUE, footer = shiny::modalButton("Close"),
      shiny::tags$dl(
        shiny::tags$dt("Project"), shiny::tags$dd(p$project_name),
        shiny::tags$dt("Purpose"), shiny::tags$dd(pur_label),
        shiny::tags$dt("Location"),
        shiny::tags$dd(paste(toupper(p$country), "·", toupper(p$state), "·",
                      p$county, "·", p$city)),
        shiny::tags$dt("Folder"),
        shiny::tags$dd(shiny::tags$code(p$project_path)),
        shiny::tags$dt("Inputs"),
        shiny::tags$dd(shiny::tags$code(file.path(p$project_path,
                                    paste0(os_detect(), "_in")))),
        shiny::tags$dt("Outputs (glmnet)"),
        shiny::tags$dd(shiny::tags$code(file.path(p$project_path,
                                    paste0(os_detect(), "_out_glmnet"))))
      )
    ))
  })

  # --- Dynamic step headings ---
  output$download_heading <- shiny::renderUI({
    label <- if (identical(input$purpose, "general")) {
      "7. Download Estimated Target Variable(s) & Residuals"
    } else {
      "7. Download Estimated Sale Prices & Residuals"
    }
    shiny::h4(
      label,
      shiny::tags$span(class = "glmnet-section-info",
        `data-bs-toggle` = "popover", `data-bs-trigger` = "hover",
        `data-bs-content` = "Download an Excel file with predicted values, residuals, CQA scores, and per-variable contributions for all observations.",
        "?"),
      style = "display:inline;")
  })

  output$generate_qmd_heading <- shiny::renderUI({
    n <- switch(input$purpose %||% "general",
                appraisal = "10", market = "9", "8")
    shiny::h4(
      paste0(n, ". Generate Quarto Report"),
      shiny::tags$span(class = "glmnet-section-info",
        `data-bs-toggle` = "popover", `data-bs-trigger` = "hover",
        `data-bs-content` = "Write a self-contained Quarto bundle (.qmd source + plots + report_data.rds + reference.docx) to the project's glmnet output folder. Convert it to HTML / Word / PDF in the next section.",
        "?"),
      style = "display:inline;")
  })

  output$convert_qmd_heading <- shiny::renderUI({
    n <- switch(input$purpose %||% "general",
                appraisal = "11", market = "10", "9")
    shiny::h4(
      paste0(n, ". Convert Quarto Report"),
      shiny::tags$span(class = "glmnet-section-info",
        `data-bs-toggle` = "popover", `data-bs-trigger` = "hover",
        `data-bs-content` = "Render any Quarto .qmd to HTML / Word / PDF. Defaults to the most recently generated bundle, but you can browse to any .qmd.",
        "?"),
      style = "display:inline;")
  })

  # ── Shared helper: build full model matrix & coefficients ──────────
  # Returns list(x_full, complete, coefs, intercept, term_labels, assign_attr)
  # or NULL on failure.
  build_export_matrix_ <- function() {
    export_df <- data_out$data()
    model     <- model_out$model()
    lambda    <- model_out$lambda()
    gamma     <- model_out$gamma()
    preds_col <- data_out$predictors()
    col_types <- data_out$col_types()
    x_train   <- model_out$x_matrix()
    train_colnames <- colnames(x_train)

    # --- Extract coefficients at selected lambda ---
    coef_args <- list(model, s = lambda)
    if (!is.null(gamma)) coef_args$gamma <- gamma
    coef_sparse <- do.call(stats::coef, coef_args)
    coef_vec    <- as.numeric(coef_sparse)
    coef_names  <- rownames(coef_sparse)
    intercept   <- coef_vec[1L]
    beta        <- coef_vec[-1L]
    names(beta) <- coef_names[-1L]

    # --- Prepare predictor data frame with aligned factor levels ---
    x_df <- export_df[, preds_col, drop = FALSE]
    fac_vars <- data_out$fac_vars()
    for (p in preds_col) {
      is_factor <- (p %in% fac_vars) ||
                   (!is.null(col_types[p]) && col_types[p] %in% c("factor", "character"))
      if (is_factor) {
        prefix <- paste0(p)
        matched <- grep(paste0("^", prefix), train_colnames, value = TRUE)
        other_preds <- setdiff(preds_col, p)
        for (op in other_preds) {
          matched <- matched[!grepl(paste0("^", op), matched)]
        }
        train_lvls <- sub(paste0("^", prefix), "", matched)
        all_lvls <- unique(c(train_lvls, as.character(x_df[[p]])))
        all_lvls <- all_lvls[!is.na(all_lvls) & nzchar(all_lvls)]
        x_df[[p]] <- factor(x_df[[p]], levels = all_lvls)
      } else if (!is.null(col_types[p]) && col_types[p] == "numeric") {
        x_df[[p]] <- as.numeric(x_df[[p]])
      } else if (!is.null(col_types[p]) && col_types[p] == "integer") {
        x_df[[p]] <- as.integer(x_df[[p]])
      }
    }

    # --- Build formula (main effects; interactions detected from training) ---
    int_cols <- train_colnames[grepl(":", train_colnames)]
    # Extract unique interaction term labels (e.g., "x1:x2")
    int_terms <- unique(vapply(strsplit(int_cols, ":"), function(parts) {
      # Map back to original predictor names
      # Each part is either a predictor name or a factor dummy (predictor + level)
      map_part <- function(part) {
        # Find the predictor whose name is a prefix of this part
        for (p in preds_col) {
          if (grepl(paste0("^", p), part)) return(p)
        }
        part
      }
      paste(unique(vapply(parts, map_part, character(1))), collapse = ":")
    }, character(1)))
    formula_parts <- preds_col
    if (length(int_terms) > 0) {
      formula_parts <- c(formula_parts, int_terms)
    }
    formula_str <- paste("~", paste(formula_parts, collapse = " + "), "- 1")

    # --- Determine complete rows ---
    # With an earth import, completeness must be judged on the columns the
    # earth basis actually consumes (ek$predictors), NOT glmnet's selected
    # predictors. preds_col can include display-only columns (e.g. address,
    # city) that the earth basis never uses; requiring them complete would
    # wrongly gate out rows (and zero out the whole set if any is all-NA).
    ek <- tryCatch(earth_import_r(), error = function(e) NULL)
    if (!is.null(ek)) {
      earth_complete_cols <- intersect(ek$predictors, names(export_df))
      complete <- if (length(earth_complete_cols) > 0L) {
        stats::complete.cases(export_df[, earth_complete_cols, drop = FALSE])
      } else {
        stats::complete.cases(x_df)
      }
    } else {
      complete <- stats::complete.cases(x_df)
    }
    x_full <- NULL
    assign_attr <- NULL

    if (any(complete)) {
      if (!is.null(ek)) {
        # Earth basis needs ALL earth predictor columns, not just
        # glmnetUI's selected predictors. build_earth_basis handles
        # factor/type alignment internally.
        earth_cols <- intersect(ek$predictors, names(export_df))
        earth_df <- export_df[complete, earth_cols, drop = FALSE]
        x_full <- tryCatch(
          build_earth_basis(earth_df, ek),
          error = function(e) {
            stop("Earth basis for export failed: ", e$message)
          }
        )
        # Align columns with training matrix (glmnet uses position)
        if (!is.null(x_full) &&
            !identical(colnames(x_full), train_colnames)) {
          x_full <- x_full[, train_colnames, drop = FALSE]
        }
      } else {
        # Standard formula model matrix
        x_full <- tryCatch(
          stats::model.matrix(stats::as.formula(formula_str),
                              data = x_df[complete, , drop = FALSE]),
          error = function(e) {
            stop("model.matrix failed: ", e$message,
                 "\nFormula: ", formula_str,
                 "\nComplete rows: ", sum(complete),
                 "\nPredictors: ", paste(preds_col, collapse = ", "))
          }
        )
        assign_attr <- attr(x_full, "assign")

        # Align columns with training matrix
        missing_cols <- setdiff(train_colnames, colnames(x_full))
        if (length(missing_cols) > 0) {
          zero_mat <- matrix(0, nrow = nrow(x_full),
                             ncol = length(missing_cols),
                             dimnames = list(NULL, missing_cols))
          x_full <- cbind(x_full, zero_mat)
        }
        extra_cols <- setdiff(colnames(x_full), train_colnames)
        if (length(extra_cols) > 0) {
          x_full <- x_full[, intersect(colnames(x_full), train_colnames),
                           drop = FALSE]
        }
        if (!all(train_colnames %in% colnames(x_full))) {
          still_missing <- setdiff(train_colnames, colnames(x_full))
          stop("Column mismatch after alignment. Missing: ",
               paste(still_missing, collapse = ", "))
        }
        x_full <- x_full[, train_colnames, drop = FALSE]

        assign_attr <- match(train_colnames, colnames(
          stats::model.matrix(stats::as.formula(formula_str),
                              data = x_df[complete, , drop = FALSE])))
      }
    }

    # Term labels from the formula
    tt <- stats::terms(stats::as.formula(formula_str))
    term_labels <- attr(tt, "term.labels")

    if (is.null(x_full)) {
      has_earth <- !is.null(ek)
      ek_class <- if (has_earth) paste(class(ek), collapse = "/") else "NULL"
      ek_info <- ""
      if (has_earth) {
        ek_info <- paste0(", ek_preds=",
          paste(ek$predictors, collapse = ","),
          ", ek_data_rows=", if (!is.null(ek$data)) nrow(ek$data) else "NULL")
      }
      # Identify the columns driving the incompleteness so the message is
      # actionable (which predictor is all/mostly NA).
      check_cols <- if (has_earth)
        intersect(ek$predictors, names(export_df)) else names(x_df)
      na_counts <- vapply(check_cols, function(cc)
        sum(is.na(export_df[[cc]])), integer(1))
      worst <- sort(na_counts[na_counts > 0L], decreasing = TRUE)
      na_info <- if (length(worst))
        paste0(", NA-columns=", paste(sprintf("%s(%d)", names(worst), worst),
                                      collapse = ", "))
      else ", NA-columns=none"
      stop("build_export_matrix_ returned x_full=NULL. ",
           "complete_rows=", sum(complete), "/", length(complete),
           ", earth_import=", has_earth,
           ", ek_class=", ek_class, ek_info, na_info,
           ", formula=", formula_str,
           ", train_cols=", length(train_colnames))
    }

    list(x_full = x_full, complete = complete, beta = beta,
         intercept = intercept, term_labels = term_labels,
         formula_str = formula_str, x_df = x_df,
         train_colnames = train_colnames,
         earth_import = ek)
  }

  # ── Compute per-term contributions ───────────────────────────────
  # Returns a named list: term_label -> numeric vector (length = nrow(x_full))
  # Helper: extract parent variable from an earth column name
  hinge_var_ <- function(h) {
    inner <- sub("^h\\(", "", sub("\\)$", "", h))
    # Handle negative knots: h(-121.45-var) has inner "-121.45-var"
    parts <- strsplit(inner, "-", fixed = TRUE)[[1]]
    parts <- parts[nzchar(parts)]
    for (pt in parts) {
      if (suppressWarnings(is.na(as.numeric(pt)))) return(pt)
    }
    inner
  }

  compute_contributions_ <- function(x_full, beta, train_colnames,
                                     term_labels, formula_str, x_df_complete) {
    all_colnames <- colnames(x_full)
    ek <- earth_import_r()

    if (!is.null(ek)) {
      # Earth path: glmnet's coefficients * basis columns,
      # grouped by parent predictor. Positional indexing only.
      beta_pos <- as.numeric(beta)
      col_parent <- earthUI:::earth_col_to_pred_(all_colnames, ek$predictors)
      unique_parents <- unique(col_parent[nzchar(col_parent)])
      contribs <- list()
      for (parent in unique_parents) {
        idx <- which(col_parent == parent)
        if (all(beta_pos[idx] == 0)) next
        contrib_vec <- rep(0, nrow(x_full))
        for (j in idx) {
          contrib_vec <- contrib_vec + beta_pos[j] * x_full[, j]
        }
        contribs[[parent]] <- contrib_vec
      }
      return(contribs)
    }

    # Standard formula path (no earth import)
    x_ref <- stats::model.matrix(stats::as.formula(formula_str),
                                 data = x_df_complete)
    ref_assign <- attr(x_ref, "assign")
    ref_colnames <- colnames(x_ref)

    col_to_term <- integer(length(train_colnames))
    for (j in seq_along(train_colnames)) {
      idx <- match(train_colnames[j], ref_colnames)
      if (!is.na(idx)) {
        col_to_term[j] <- ref_assign[idx]
      } else {
        col_to_term[j] <- NA_integer_
      }
    }

    contribs <- list()
    for (ti in seq_along(term_labels)) {
      cols_idx <- which(col_to_term == ti)
      if (length(cols_idx) == 0) {
        contribs[[term_labels[ti]]] <- rep(0, nrow(x_full))
      } else {
        term_contrib <- rep(0, nrow(x_full))
        for (j in cols_idx) {
          b <- beta[train_colnames[j]]
          if (!is.na(b) && b != 0) {
            term_contrib <- term_contrib + b * x_full[, j]
          }
        }
        contribs[[term_labels[ti]]] <- term_contrib
      }
    }
    contribs
  }

  # ── Find living_area column from specials ────────────────────────
  find_living_area_ <- function() {
    specials <- data_out$col_specials()
    if (is.null(specials) || length(specials) == 0) return(NULL)
    la_idx <- which(specials == "living_area")
    if (length(la_idx) == 0) return(NULL)
    names(specials)[la_idx[1L]]
  }

  # living_area for the RCA: glmnetUI's own tag first; otherwise the column
  # earthUI tagged as living_area (carried forward), but only when an earthUI
  # model is imported and the column exists in the loaded data. Enables the
  # "CQA per SF" option without re-tagging in glmnetUI.
  resolve_living_area_ <- function() {
    la <- find_living_area_()
    if (!is.null(la)) return(la)
    if (is.null(tryCatch(earth_import_r(), error = function(e) NULL)) ||
        is.null(rv_proj$active_project)) return(NULL)
    la_e <- earthUI::earth_carryforward_(
      rv_proj$active_project$project_path, purpose())$living_area
    if (!is.null(la_e) && la_e %in% names(data_out$data())) la_e else NULL
  }

  # --- 6. Download Output (Excel) ---
  observeEvent(input$export_data, {
    shiny::req(model_out$fitted(), data_out$data())
    if (!requireNamespace("writexl", quietly = TRUE)) {
      shiny::showNotification(
        "Package 'writexl' required. Install with: install.packages('writexl')",
        type = "error", duration = 10)
      return()
    }

    # Derive the output folder from the ACTIVE project's real path so it can
    # never go stale (the output_folder text field could lag a project switch).
    folder <- local({
      .ap <- rv_proj$active_project
      if (!is.null(.ap))
        file.path(.ap$project_path, paste0(os_detect(), "_out_glmnet"))
      else input$output_folder
    })
    if (is.null(folder) || !nzchar(folder)) folder <- path.expand("~/Downloads")
    if (!dir.exists(folder)) dir.create(folder, recursive = TRUE)

    tryCatch({
      export_df <- data_out$data()
      model     <- model_out$model()
      lambda    <- model_out$lambda()
      gamma     <- model_out$gamma()
      response  <- data_out$response()

      em <- build_export_matrix_()
      x_full         <- em$x_full
      complete       <- em$complete
      beta           <- em$beta
      intercept      <- em$intercept
      term_labels    <- em$term_labels
      train_colnames <- em$train_colnames

      n <- nrow(export_df)
      est_col   <- rep(NA_real_, n)
      resid_col <- rep(NA_real_, n)

      if (!is.null(x_full) && any(complete)) {
        pred_args <- list(model, newx = x_full, s = lambda, type = "response")
        if (!is.null(gamma)) pred_args$gamma <- gamma
        pv <- as.numeric(do.call(stats::predict, pred_args))
        est_col[complete] <- pv
        resid_col[complete] <- export_df[[response]][complete] - pv

        # --- Per-term contributions ---
        ek_export <- em$earth_import
        if (!is.null(ek_export)) {
          beta_num <- as.numeric(beta)
          cp <- earthUI:::earth_col_to_pred_(
            colnames(x_full), ek_export$predictors)
          contribs <- list()
          for (p in unique(cp[nzchar(cp)])) {
            idx <- which(cp == p)
            if (all(beta_num[idx] == 0)) next
            v <- rep(0, nrow(x_full))
            for (j in idx) v <- v + beta_num[j] * x_full[, j]
            contribs[[p]] <- v
          }
        } else {
          contribs <- compute_contributions_(
            x_full, beta, train_colnames, term_labels,
            em$formula_str, em$x_df[complete, , drop = FALSE])
        }

        export_df[["basis"]] <- NA_real_
        export_df[["basis"]][complete] <- round(intercept, 1)
        for (tl in names(contribs)) {
          col_name <- paste0(gsub(":", "_x_", tl), "_contribution")
          export_df[[col_name]] <- NA_real_
          export_df[[col_name]][complete] <- round(contribs[[tl]], 1)
        }

        # Verification column
        contrib_total <- intercept + Reduce(`+`, contribs)
        export_df[["calc_residual"]] <- NA_real_
        export_df[["calc_residual"]][complete] <-
          round(export_df[[response]][complete] - contrib_total, 1)
      }

      export_df[[paste0("est_", response)]] <- round(est_col, 1)
      export_df[["residual"]] <- round(resid_col, 1)

      # --- CQA scores ---
      la_col <- resolve_living_area_()
      comp_rows <- if (identical(input$purpose, "appraisal")) -1L else seq_len(n)
      comp_resid <- resid_col[comp_rows]
      comp_resid <- comp_resid[!is.na(comp_resid)]
      n_comps <- length(comp_resid)
      if (n_comps > 0) {
        cqa <- vapply(resid_col, function(r) {
          if (is.na(r)) return(NA_real_)
          sum(comp_resid < r, na.rm = TRUE) / n_comps * 10
        }, numeric(1))
        export_df[["cqa"]] <- round(cqa, 2)

        if (!is.null(la_col) && la_col %in% names(export_df)) {
          resid_sf <- resid_col / export_df[[la_col]]
          export_df[["residual_sf"]] <- round(resid_sf, 4)
          comp_resid_sf <- resid_sf[comp_rows]
          comp_resid_sf <- comp_resid_sf[!is.na(comp_resid_sf)]
          n_sf <- length(comp_resid_sf)
          if (n_sf > 0) {
            cqa_sf <- vapply(resid_sf, function(r) {
              if (is.na(r)) return(NA_real_)
              sum(comp_resid_sf < r, na.rm = TRUE) / n_sf * 10
            }, numeric(1))
            export_df[["cqa_sf"]] <- round(cqa_sf, 2)
          }
        }
      }

      # In appraisal mode, set subject row (row 1) actual/residual to NA
      if (identical(input$purpose, "appraisal")) {
        resid_col[1L] <- NA_real_
        export_df[["residual"]][1L] <- NA_real_
        if ("cqa" %in% names(export_df)) export_df[["cqa"]][1L] <- NA_real_
        if ("cqa_sf" %in% names(export_df)) export_df[["cqa_sf"]][1L] <- NA_real_
      }

      # Sort by residual_sf descending for appraisal/market
      if (input$purpose %in% c("appraisal", "market") &&
          "residual_sf" %in% names(export_df)) {
        ord <- order(export_df[["residual_sf"]], decreasing = TRUE,
                     na.last = TRUE)
        export_df <- export_df[ord, , drop = FALSE]
      }

      # Convert Date/POSIXct columns to character for clean Excel output
      for (cn in names(export_df)) {
        if (inherits(export_df[[cn]], "POSIXct") ||
            inherits(export_df[[cn]], "POSIXlt") ||
            inherits(export_df[[cn]], "Date")) {
          export_df[[cn]] <- as.character(export_df[[cn]])
        }
      }

      base <- tools::file_path_sans_ext(data_out$file_name() %||% "glmnetui")
      out_path <- file.path(folder, paste0(base, "_output_",
                            earthUI:::fit_stamp_(model_out$fit_ts()), ".xlsx"))
      writexl::write_xlsx(export_df, out_path)
      shiny::showNotification(paste0("Output saved to: ", out_path),
                              type = "message", duration = 8)
      session$sendCustomMessage("btn_done", list(id = "export_data"))
    }, error = function(e) {
      shiny::showNotification(paste("Export error:", e$message),
                              type = "error", duration = 10)
    })
  })

  # --- 7. Calculate RCA Adjustments & Download ---
  observeEvent(input$rca_output_btn, {
    shiny::req(model_out$fitted(), data_out$data())

    # Carry the subject-CQA (type + value) AND the living_area column forward
    # from earthUI ONLY when an earthUI model has been imported (Section 2).
    # earthUI saves them to the shared regProj settings; a Trilogy lock is a
    # fallback for the CQA value. Without an earth import the field starts blank.
    cqa_type_pre <- NULL; cqa_value_pre <- NULL
    if (!is.null(tryCatch(earth_import_r(), error = function(e) NULL)) &&
        !is.null(rv_proj$active_project)) {
      cf_cqa <- earthUI::earth_carryforward_(
        rv_proj$active_project$project_path, purpose())
      cqa_type_pre  <- cf_cqa$cqa_type
      cqa_value_pre <- cf_cqa$cqa_value
      if (is.null(cqa_type_pre) && !is.null(getOption("glmnetUI.trilogy"))) {
        tl <- trilogy_get_lock(rv_proj$active_project$project_path)$shared
        cqa_type_pre  <- tl$cqa_mode
        tlv <- suppressWarnings(as.numeric(tl$cqa))
        if (length(tlv) == 1L && !is.na(tlv)) cqa_value_pre <- tlv
      }
    }

    la_col <- resolve_living_area_()
    cqa_choices <- c("CQA" = "cqa")
    if (!is.null(la_col)) {
      cqa_choices <- c(cqa_choices, "CQA per SF" = "cqa_sf")
    }
    cqa_sel <- cqa_type_pre %||% unname(cqa_choices[1])
    if (!cqa_sel %in% cqa_choices) cqa_sel <- unname(cqa_choices[1])
    cqa_val <- if (!is.null(cqa_value_pre) && !is.na(cqa_value_pre))
      as.character(cqa_value_pre) else ""

    cqa_sf_help <- shiny::tags$span(
      `data-bs-toggle` = "popover", `data-bs-trigger` = "hover focus",
      `data-bs-content` = paste0(
        "This CQA value is based on the ranked Residual/SF in order to ",
        "reduce the impact of home size on the residual value."),
      title = "CQA per SF",
      style = paste0("display:inline-block; width:16px; height:16px;",
                     " margin-left:5px; border-radius:50%; background:#88c0d0;",
                     " color:#fff; font-size:10px; font-weight:bold;",
                     " line-height:16px; text-align:center; cursor:pointer;"),
      "?")
    cqa_names <- list("CQA")
    if (!is.null(la_col))
      cqa_names <- c(cqa_names, list(shiny::tagList("CQA per SF", cqa_sf_help)))

    shiny::showModal(shiny::modalDialog(
      title = "RCA Raw Output \u2014 Subject CQA Score",
      shiny::radioButtons("rca_cqa_type", "Score type:",
                          choiceNames = cqa_names,
                          choiceValues = unname(cqa_choices),
                          selected = cqa_sel, inline = TRUE),
      shiny::textInput("rca_cqa_value", "Enter CQA score for subject:",
                       value = cqa_val,
                       placeholder = "[Enter a CQA value between 0.00 and 10.00]"),
      shiny::tags$script(shiny::HTML("
        (function(){
          function rcaValidate(){
            var el = document.getElementById('rca_cqa_value');
            var btn = document.getElementById('export_rca');
            if(!el || !btn) return;
            var v = parseFloat(el.value);
            btn.disabled = !(!isNaN(v) && v >= 0 && v <= 10);
          }
          $(document).off('input.glmnet_rca').on('input.glmnet_rca', '#rca_cqa_value', rcaValidate);
          setTimeout(rcaValidate, 120);
        })();
      ")),
      footer = shiny::tagList(
        shiny::modalButton("Cancel"),
        shiny::actionButton("export_rca", "Generate",
                            class = "btn-primary")
      ),
      easyClose = TRUE
    ))
  })

  observeEvent(input$export_rca, {
    # Validate the CQA score (0.00-10.00); keep the modal open and show an
    # error if it is blank or out of range.
    cqa_num <- suppressWarnings(as.numeric(input$rca_cqa_value))
    if (length(cqa_num) != 1L || is.na(cqa_num) || cqa_num < 0 || cqa_num > 10) {
      shiny::showNotification("Please enter a CQA score between 0.00 and 10.00.",
                              type = "error", duration = 6)
      return()
    }
    shiny::removeModal()
    shiny::req(model_out$fitted(), data_out$data())
    if (!requireNamespace("writexl", quietly = TRUE)) {
      shiny::showNotification(
        "Package 'writexl' required. Install with: install.packages('writexl')",
        type = "error", duration = 10)
      return()
    }

    # Derive the output folder from the ACTIVE project's real path so it can
    # never go stale (the output_folder text field could lag a project switch).
    folder <- local({
      .ap <- rv_proj$active_project
      if (!is.null(.ap))
        file.path(.ap$project_path, paste0(os_detect(), "_out_glmnet"))
      else input$output_folder
    })
    if (is.null(folder) || !nzchar(folder)) folder <- path.expand("~/Downloads")
    if (!dir.exists(folder)) dir.create(folder, recursive = TRUE)

    tryCatch({
      export_df <- data_out$data()
      model     <- model_out$model()
      lambda    <- model_out$lambda()
      gamma     <- model_out$gamma()
      response  <- data_out$response()
      user_cqa  <- cqa_num
      n         <- nrow(export_df)

      if (n < 2L) {
        shiny::showNotification("Need at least 2 rows (subject + 1 comp).",
                                type = "error")
        return()
      }

      em <- build_export_matrix_()
      if (is.null(em) || is.null(em$x_full)) {
        stop("Could not build model matrix for export. ",
             "Check that predictor columns match the fitted model.")
      }
      x_full         <- em$x_full
      complete       <- em$complete
      beta           <- em$beta
      intercept      <- em$intercept
      term_labels    <- em$term_labels
      train_colnames <- em$train_colnames

      # --- Predictions for all rows ---
      pred_args <- list(model, newx = x_full, s = lambda, type = "response")
      if (!is.null(gamma)) pred_args$gamma <- gamma
      pv_complete <- as.numeric(do.call(stats::predict, pred_args))

      predicted   <- rep(NA_real_, n)
      actual      <- export_df[[response]]
      predicted[complete] <- pv_complete
      residuals_val <- actual - predicted

      # Subject row (row 1): sale price treated as NA
      actual[1L] <- NA_real_
      residuals_val[1L] <- NA_real_

      export_df[[paste0("est_", response)]] <- round(predicted, 1)

      # --- CQA scores on comps (rows 2+) ---
      la_col <- resolve_living_area_()
      comp_resid <- residuals_val[-1L]
      comp_resid_valid <- comp_resid[!is.na(comp_resid)]
      n_comps <- length(comp_resid_valid)

      cqa_col <- rep(NA_real_, n)
      if (n_comps > 0) {
        cqa_col <- vapply(residuals_val, function(r) {
          if (is.na(r)) return(NA_real_)
          sum(comp_resid_valid < r, na.rm = TRUE) / n_comps * 10
        }, numeric(1))
      }
      export_df[["residual"]] <- round(residuals_val, 1)
      export_df[["cqa"]] <- round(cqa_col, 2)

      # CQA_SF if living_area designated
      resid_sf <- NULL
      cqa_sf_col <- NULL
      if (!is.null(la_col) && la_col %in% names(export_df)) {
        resid_sf <- residuals_val / export_df[[la_col]]
        export_df[["residual_sf"]] <- round(resid_sf, 4)
        comp_resid_sf <- resid_sf[-1L]
        comp_resid_sf_valid <- comp_resid_sf[!is.na(comp_resid_sf)]
        n_sf <- length(comp_resid_sf_valid)
        if (n_sf > 0) {
          cqa_sf_col <- vapply(resid_sf, function(r) {
            if (is.na(r)) return(NA_real_)
            sum(comp_resid_sf_valid < r, na.rm = TRUE) / n_sf * 10
          }, numeric(1))
          export_df[["cqa_sf"]] <- round(cqa_sf_col, 2)
        }
      }

      # --- Interpolate subject residual from CQA ---
      use_sf <- (input$rca_cqa_type == "cqa_sf" && !is.null(la_col))
      if (use_sf) {
        comp_cqa_vals  <- cqa_sf_col[-1L]
        comp_resid_for_interp <- resid_sf[-1L]
      } else {
        comp_cqa_vals  <- cqa_col[-1L]
        comp_resid_for_interp <- residuals_val[-1L]
      }

      valid <- !is.na(comp_cqa_vals) & !is.na(comp_resid_for_interp)
      cqa_sorted   <- comp_cqa_vals[valid]
      resid_sorted <- comp_resid_for_interp[valid]
      ord <- order(cqa_sorted)
      cqa_sorted   <- cqa_sorted[ord]
      resid_sorted <- resid_sorted[ord]

      subject_resid <- stats::approx(cqa_sorted, resid_sorted,
                                     xout = user_cqa, rule = 2)$y

      # Convert per-SF back to total if needed
      if (use_sf) {
        subject_la <- export_df[[la_col]][1L]
        subject_resid_total <- subject_resid * subject_la
      } else {
        subject_resid_total <- subject_resid
      }

      # Subject value = model estimate + interpolated residual
      subject_est <- predicted[1L] + subject_resid_total
      residuals_val[1L] <- subject_resid_total
      export_df[["residual"]][1L] <- round(subject_resid_total, 1)
      export_df[["subject_value"]] <- NA_real_
      export_df[["subject_value"]][1L] <- round(subject_est, 1)
      export_df[["subject_cqa"]] <- NA_real_
      export_df[["subject_cqa"]][1L] <- user_cqa

      # Weight-0 rows also get subject_value
      wt_col <- data_out$weight_col()
      zero_wt <- integer(0)
      if (!is.null(wt_col) && wt_col %in% names(export_df)) {
        wvals <- export_df[[wt_col]]
        zero_wt <- which(wvals == 0 & seq_len(n) != 1L)
        for (zw in zero_wt) {
          if (!is.na(predicted[zw])) {
            export_df[["subject_value"]][zw] <-
              round(predicted[zw] + subject_resid_total, 1)
          }
        }
      }

      # --- Per-term contributions ---
      # Compute directly: beta[j] * x_full[,j] grouped by parent predictor.
      # No separate function, no reactive lookups.
      ek_for_contrib <- em$earth_import
      beta_numeric <- as.numeric(beta)

      if (!is.null(ek_for_contrib)) {
        col_parent <- earthUI:::earth_col_to_pred_(
          colnames(x_full), ek_for_contrib$predictors)
        parents <- unique(col_parent[nzchar(col_parent)])
        contribs <- list()
        for (p in parents) {
          idx <- which(col_parent == p)
          if (all(beta_numeric[idx] == 0)) next
          v <- rep(0, nrow(x_full))
          for (j in idx) v <- v + beta_numeric[j] * x_full[, j]
          contribs[[p]] <- v
        }
      } else {
        contribs <- compute_contributions_(
          x_full, beta, train_colnames, term_labels,
          em$formula_str, em$x_df[complete, , drop = FALSE])
      }

      basis_val <- intercept

      # Expand contributions to full data (NA for incomplete rows)
      contribs_full <- list()
      for (tl in names(contribs)) {
        v <- rep(NA_real_, n)
        v[complete] <- contribs[[tl]]
        contribs_full[[tl]] <- v
      }

      export_df[["basis"]] <- NA_real_
      export_df[["basis"]][complete] <- round(basis_val, 1)

      for (tl in names(contribs_full)) {
        col_name <- paste0(gsub(":", "_x_", tl), "_contribution")
        export_df[[col_name]] <- round(contribs_full[[tl]], 1)
      }

      # --- RCA Adjustments ---
      adj_sum   <- rep(0, n)
      gross_sum <- rep(0, n)

      for (tl in names(contribs_full)) {
        adj_col_name <- paste0(gsub(":", "_x_", tl), "_adjustment")
        subject_contrib <- contribs_full[[tl]][1L]
        adjustment <- subject_contrib - contribs_full[[tl]]
        export_df[[adj_col_name]] <- round(adjustment, 1)
        # Accumulate UNROUNDED adjustments for accurate totals
        adj_sum   <- adj_sum + ifelse(is.na(adjustment), 0, adjustment)
        gross_sum <- gross_sum + ifelse(is.na(adjustment), 0, abs(adjustment))
      }

      # Residual adjustment
      resid_adj <- subject_resid_total - residuals_val
      export_df[["residual_adjustment"]] <- round(resid_adj, 1)
      adj_sum   <- adj_sum + ifelse(is.na(resid_adj), 0, resid_adj)
      gross_sum <- gross_sum + ifelse(is.na(resid_adj), 0, abs(resid_adj))

      export_df[["net_adjustments"]]     <- round(adj_sum, 1)
      export_df[["gross_adjustments"]]   <- round(gross_sum, 1)

      # Adjustment percentages (adjustment / comparable sale price)
      sale_price <- export_df[[response]]
      export_df[["residual_adj_pct"]] <- round(resid_adj / sale_price * 100, 1)
      export_df[["net_adj_pct"]]      <- round(adj_sum / sale_price * 100, 1)
      export_df[["gross_adj_pct"]]    <- round(gross_sum / sale_price * 100, 1)

      export_df[["adjusted_sale_price"]] <- round(actual + adj_sum, 1)

      # Subject row: adjustments are zero (subject vs self)
      adj_cols <- grep("_adjustment$|net_adjustments|gross_adjustments|adjusted_sale_price|_adj_pct$",
                       names(export_df), value = TRUE)
      for (ac in adj_cols) {
        export_df[[ac]][1L] <- NA_real_
      }
      export_df[["adjusted_sale_price"]][1L] <- round(subject_est, 1)

      # Sort comps (rows 2+): weight > 0 first sorted by gross_adj_pct
      # ascending, then weight = 0 at the end
      if (nrow(export_df) > 1L) {
        comps <- export_df[-1L, , drop = FALSE]
        wt_col_name <- data_out$weight_col()
        if (!is.null(wt_col_name) && wt_col_name %in% names(comps)) {
          wt <- as.numeric(comps[[wt_col_name]])
          wt[is.na(wt)] <- 0
          # Sort: weight > 0 first (by gross_adj_pct), then weight = 0
          comps <- comps[order(wt == 0, comps[["gross_adj_pct"]],
                               na.last = TRUE), ]
        } else {
          comps <- comps[order(comps[["gross_adj_pct"]],
                               na.last = TRUE), ]
        }
        export_df <- rbind(export_df[1L, , drop = FALSE], comps)
        rownames(export_df) <- NULL
      }

      # Convert Date/POSIXct columns to character so the sales grid
      # (which reads this file back) doesn't get date-formatted cells
      for (cn in names(export_df)) {
        if (inherits(export_df[[cn]], "POSIXct") ||
            inherits(export_df[[cn]], "POSIXlt") ||
            inherits(export_df[[cn]], "Date")) {
          export_df[[cn]] <- as.character(export_df[[cn]])
        }
      }

      # Store full RCA data frame for Sales Grid
      rv_rca$rca_df <- export_df

      # Store pct data for RCA Analysis plots (comps only, exclude subject)
      rv_rca$pct_data <- data.frame(
        residual_adj_pct = export_df[["residual_adj_pct"]][-1L],
        net_adj_pct      = export_df[["net_adj_pct"]][-1L],
        gross_adj_pct    = export_df[["gross_adj_pct"]][-1L],
        stringsAsFactors = FALSE
      )

      base <- tools::file_path_sans_ext(data_out$file_name() %||% "glmnetui")
      out_path <- file.path(folder, paste0(base, "_adjusted_",
                            earthUI:::fit_stamp_(model_out$fit_ts()), ".xlsx"))
      writexl::write_xlsx(export_df, out_path)
      # Trilogy mode: emit this method's value conclusion (the subject's
      # indicated value, row 1 of adjusted_sale_price) for the combined report.
      if (!is.null(getOption("glmnetUI.trilogy"))) {
        m <- earthUI:::trilogy_fit_metrics_(export_df[[response]][-1L],
                                             residuals_val[-1L])
        try(earthUI::trilogy_write_conclusion(
          folder, "glmnet", earthUI:::fit_stamp_(model_out$fit_ts()),
          subject_value = export_df[["adjusted_sale_price"]][1L],
          metrics = m), silent = TRUE)
      }
      shiny::showNotification(paste0("RCA output saved to: ", out_path),
                              type = "message", duration = 8)
      session$sendCustomMessage("btn_done", list(id = "rca_output_btn"))
    }, error = function(e) {
      shiny::showNotification(paste("RCA error:", e$message),
                              type = "error", duration = 15)
    })
  })

  # --- RCA Analysis Plots ---
  rca_pct_histogram_ <- function(vals, title, xlab, fill_color) {
    vals <- vals[!is.na(vals)]
    if (length(vals) == 0) return(NULL)

    avg_val    <- mean(vals)
    median_val <- stats::median(vals)
    sd_val     <- stats::sd(vals)
    font_fam   <- glmnet_font_family_()

    # Bins in 20% increments
    lo <- floor(min(vals) / 20) * 20
    hi <- ceiling(max(vals) / 20) * 20
    breaks <- seq(lo, hi, by = 20)
    if (length(breaks) < 2) breaks <- c(lo, lo + 20)

    df <- data.frame(x = vals)
    subtitle <- sprintf("Mean: %.2f%%    Median: %.2f%%    Std Dev: %.2f%%",
                        avg_val, median_val, sd_val)

    ggplot2::ggplot(df, ggplot2::aes(x = .data$x)) +
      ggplot2::geom_histogram(breaks = breaks, fill = fill_color,
                              color = "white", alpha = 0.85) +
      ggplot2::geom_vline(xintercept = avg_val, linetype = "dashed",
                          color = "#2e3440", linewidth = 0.8) +
      ggplot2::geom_vline(xintercept = median_val, linetype = "dotted",
                          color = "#5e81ac", linewidth = 0.8) +
      ggplot2::annotate("text", x = avg_val, y = Inf, label = "Mean",
                        vjust = 2, hjust = -0.15, size = 3.5,
                        color = "#2e3440", family = font_fam) +
      ggplot2::annotate("text", x = median_val, y = Inf, label = "Median",
                        vjust = 3.5, hjust = -0.15, size = 3.5,
                        color = "#5e81ac", family = font_fam) +
      ggplot2::scale_x_continuous(breaks = breaks,
                                  labels = paste0(breaks, "%")) +
      ggplot2::labs(title = title, subtitle = subtitle,
                    x = xlab, y = "Frequency") +
      ggplot2::theme_minimal(base_family = font_fam) +
      ggplot2::theme(
        plot.title = ggplot2::element_text(size = 14, face = "bold"),
        plot.subtitle = ggplot2::element_text(size = 11,
                                              color = "#4c566a"),
        axis.text.x = ggplot2::element_text(angle = 0)
      )
  }

  d_ <- plot_dims_(session, "rca_resid_pct_plot")
  output$rca_resid_pct_plot <- shiny::renderPlot({
    shiny::req(rv_rca$pct_data)
    rca_pct_histogram_(rv_rca$pct_data$residual_adj_pct,
                       "Residual Adjustment %",
                       "Residual Adj. %", "#88c0d0")
  }, width = d_$width, height = d_$height, res = 96)

  d_ <- plot_dims_(session, "rca_net_pct_plot")
  output$rca_net_pct_plot <- shiny::renderPlot({
    shiny::req(rv_rca$pct_data)
    rca_pct_histogram_(rv_rca$pct_data$net_adj_pct,
                       "Net Adjustment %",
                       "Net Adj. %", "#5e81ac")
  }, width = d_$width, height = d_$height, res = 96)

  d_ <- plot_dims_(session, "rca_gross_pct_plot")
  output$rca_gross_pct_plot <- shiny::renderPlot({
    shiny::req(rv_rca$pct_data)
    rca_pct_histogram_(rv_rca$pct_data$gross_adj_pct,
                       "Gross Adjustment %",
                       "Gross Adj. %", "#a3be8c")
  }, width = d_$width, height = d_$height, res = 96)

  # --- 8. Generate Sales Grid & Download (Appraisal only) ---
  observeEvent(input$sales_grid_btn, {
    shiny::req(rv_rca$rca_df)
    rca <- rv_rca$rca_df
    n_total <- nrow(rca)
    if (n_total < 2L) {
      shiny::showNotification("Need at least 2 rows (subject + 1 comp).",
                              type = "error", duration = 8)
      return()
    }

    # Build specials map from data module
    sg_specials <- data_out$col_specials()

    # Find sale_age column name
    sa_col_name <- "sale_age"
    if (!is.null(sg_specials)) {
      for (nm in names(sg_specials)) {
        if (sg_specials[[nm]] == "sale_age") { sa_col_name <- nm; break }
      }
    }

    # Weight column
    wt_col_name <- data_out$weight_col()
    wt_vals <- if (!is.null(wt_col_name) && wt_col_name %in% colnames(rca)) {
      rca[[wt_col_name]]
    } else {
      rep(1, n_total)
    }

    # Build comp info (rows 2..n)
    comp_info <- data.frame(
      row        = 2:n_total,
      address    = if ("street_address" %in% colnames(rca)) {
                     rca[["street_address"]][2:n_total]
                   } else rep("", n_total - 1L),
      sale_price = if ("sale_price" %in% colnames(rca)) {
                     rca[["sale_price"]][2:n_total]
                   } else rep(NA_real_, n_total - 1L),
      sale_age   = if (sa_col_name %in% colnames(rca)) {
                     rca[[sa_col_name]][2:n_total]
                   } else rep(NA_real_, n_total - 1L),
      weight     = wt_vals[2:n_total],
      gross_adj  = if ("gross_adjustments" %in% colnames(rca)) {
                     rca[["gross_adjustments"]][2:n_total]
                   } else rep(0, n_total - 1L),
      stringsAsFactors = FALSE
    )
    comp_info$gross_adj_pct <- ifelse(
      !is.na(comp_info$sale_price) & comp_info$sale_price != 0,
      abs(comp_info$gross_adj / comp_info$sale_price), NA_real_
    )

    eligible <- comp_info[!is.na(comp_info$weight) & comp_info$weight > 0, ]
    eligible <- eligible[order(eligible$gross_adj_pct, na.last = TRUE), ]

    recommended <- eligible[!is.na(eligible$gross_adj_pct) &
                            eligible$gross_adj_pct < 0.25, ]
    recommended <- recommended[order(recommended$sale_age, na.last = TRUE), ]
    if (nrow(recommended) > 30L) recommended <- recommended[1:30, ]

    others <- eligible[is.na(eligible$gross_adj_pct) |
                       eligible$gross_adj_pct >= 0.25, ]
    others <- others[order(others$gross_adj_pct, na.last = TRUE), ]

    rv_rca$sg_recommended <- recommended
    rv_rca$sg_others <- others

    rec_checks <- if (nrow(recommended) > 0L) {
      lapply(seq_len(nrow(recommended)), function(i) {
        r <- recommended[i, ]
        lbl <- sprintf("Row %d | %s | SP: $%s | Age: %s | Gross: %.1f%%",
                       r$row,
                       substr(as.character(r$address), 1, 30),
                       formatC(r$sale_price, format = "f", digits = 0,
                               big.mark = ","),
                       as.character(r$sale_age),
                       r$gross_adj_pct * 100)
        shiny::tags$div(
          shiny::checkboxInput(paste0("sg_rec_", r$row), lbl, value = TRUE),
          style = "margin-bottom: 0px;"
        )
      })
    } else {
      shiny::tags$p("No comps with gross adjustment < 25% found.",
                    style = "color: var(--bs-secondary-color);")
    }

    other_checks <- if (nrow(others) > 0L) {
      lapply(seq_len(min(nrow(others), 50L)), function(i) {
        r <- others[i, ]
        pct_str <- if (!is.na(r$gross_adj_pct)) {
          sprintf("%.1f%%", r$gross_adj_pct * 100)
        } else "N/A"
        lbl <- sprintf("Row %d | %s | SP: $%s | Age: %s | Gross: %s",
                       r$row,
                       substr(as.character(r$address), 1, 30),
                       formatC(r$sale_price, format = "f", digits = 0,
                               big.mark = ","),
                       as.character(r$sale_age),
                       pct_str)
        shiny::tags$div(
          shiny::checkboxInput(paste0("sg_rec_", r$row), lbl, value = FALSE),
          style = "margin-bottom: 0px;"
        )
      })
    } else NULL

    shiny::showModal(shiny::modalDialog(
      title = "Sales Grid \u2014 Select Comparables (max 30)",
      size = "l",
      shiny::tags$div(
        style = "max-height: 500px; overflow-y: auto;",
        shiny::tags$h5(paste0("Recommended Comps (gross adj < 25%, ",
                              "sorted by sale age) \u2014 ",
                              nrow(recommended), " found")),
        rec_checks,
        if (!is.null(other_checks)) {
          shiny::tagList(
            shiny::hr(),
            shiny::tags$h5("Additional Comps (gross adj >= 25%)"),
            other_checks
          )
        }
      ),
      footer = shiny::tagList(
        shiny::modalButton("Cancel"),
        shiny::actionButton("sg_confirm", "Generate Sales Grid",
                            class = "btn-primary")
      )
    ))
  })

  observeEvent(input$sg_confirm, {
    shiny::req(rv_rca$rca_df)
    shiny::removeModal()

    all_candidate_rows <- c(
      if (!is.null(rv_rca$sg_recommended) && nrow(rv_rca$sg_recommended) > 0L)
        rv_rca$sg_recommended$row else integer(0),
      if (!is.null(rv_rca$sg_others) && nrow(rv_rca$sg_others) > 0L)
        rv_rca$sg_others$row[seq_len(min(nrow(rv_rca$sg_others), 50L))]
      else integer(0)
    )
    comp_rows <- integer(0)
    for (r in all_candidate_rows) {
      cb_val <- input[[paste0("sg_rec_", r)]]
      if (!is.null(cb_val) && isTRUE(cb_val)) {
        comp_rows <- c(comp_rows, r)
      }
    }

    if (length(comp_rows) == 0L) {
      shiny::showNotification("No comps selected.",
                              type = "warning", duration = 8)
      return()
    }
    if (length(comp_rows) > 30L) {
      comp_rows <- comp_rows[1:30]
      shiny::showNotification("Capped at 30 comps.",
                              type = "warning", duration = 5)
    }

    # Sort by gross_adj_pct ascending
    rca <- rv_rca$rca_df
    sp <- if ("sale_price" %in% colnames(rca)) {
      rca[["sale_price"]][comp_rows]
    } else rep(NA_real_, length(comp_rows))
    gross <- if ("gross_adjustments" %in% colnames(rca)) {
      rca[["gross_adjustments"]][comp_rows]
    } else rep(0, length(comp_rows))
    gap <- ifelse(!is.na(sp) & sp != 0, abs(gross / sp), NA_real_)
    comp_rows <- comp_rows[order(gap, na.last = TRUE)]

    # Derive the output folder from the ACTIVE project's real path so it can
    # never go stale (the output_folder text field could lag a project switch).
    folder <- local({
      .ap <- rv_proj$active_project
      if (!is.null(.ap))
        file.path(.ap$project_path, paste0(os_detect(), "_out_glmnet"))
      else input$output_folder
    })
    if (is.null(folder) || !nzchar(folder)) folder <- path.expand("~/Downloads")
    if (!dir.exists(folder)) dir.create(folder, recursive = TRUE)

    out_path <- file.path(folder, paste0("SalesGrid_",
                          earthUI:::fit_stamp_(model_out$fit_ts()), ".xlsx"))

    tryCatch({
      if (!requireNamespace("openxlsx", quietly = TRUE)) {
        shiny::showNotification(
          "Package 'openxlsx' required. Install with: install.packages('openxlsx')",
          type = "error", duration = 10)
        return()
      }
      if (!requireNamespace("writexl", quietly = TRUE)) {
        shiny::showNotification(
          "Package 'writexl' required. Install with: install.packages('writexl')",
          type = "error", duration = 10)
        return()
      }

      tmp_adj <- tempfile(fileext = ".xlsx")
      writexl::write_xlsx(rv_rca$rca_df, tmp_adj)

      grid_script <- system.file("app_glmnet", "sales_grid.R", package = "earthUI")
      if (!nzchar(grid_script)) {
        shiny::showNotification("Sales grid script not found in package.",
                                type = "error", duration = 10)
        return()
      }
      source(grid_script, local = TRUE)

      # Build specials map from data module designations
      sg_specials_map <- list()
      sg_input <- data_out$col_specials()
      if (!is.null(sg_input)) {
        for (nm in names(sg_input)) {
          sp_type <- sg_input[[nm]]
          if (sp_type != "no") sg_specials_map[[sp_type]] <- nm
        }
      }

      n_comp <- length(comp_rows)
      shiny::withProgress(
        message = "Generating Sales Grid",
        detail = sprintf("0 of %d comps processed", n_comp),
        value = 0, {
        generate_sales_grid(
          adjusted_file = tmp_adj,
          comp_rows     = comp_rows,
          output_file   = out_path,
          specials      = sg_specials_map,
          progress_fn   = function(sheet, total_sheets, comps_done, total_comps) {
            shiny::setProgress(
              value = comps_done / total_comps,
              detail = sprintf("Sheet %d of %d \u2014 %d of %d comps processed",
                               sheet, total_sheets, comps_done, total_comps))
          }
        )
      })
      unlink(tmp_adj)

      shiny::showNotification(
        paste0("Sales grid saved to: ", out_path,
               " (", length(comp_rows), " comps, ",
               ceiling(length(comp_rows) / 3), " sheets)"),
        type = "message", duration = 10)
      session$sendCustomMessage("btn_done", list(id = "sales_grid_btn"))
    }, error = function(e) {
      shiny::showNotification(paste("Sales grid error:", e$message),
                              type = "error", duration = 10)
    })
  })

  # --- 9. Download Report (to output folder) ---
  # --- Section 10: Generate Quarto Report (writes <base>_qmd bundle into the
  #     project's <os>_out_glmnet folder) ---
  observeEvent(input$generate_qmd_btn, {
    shiny::req(model_out$fitted())

    folder <- local({
      .ap <- rv_proj$active_project
      if (!is.null(.ap))
        file.path(.ap$project_path, paste0(os_detect(), "_out_glmnet"))
      else input$output_folder
    })
    if (is.null(folder) || !nzchar(folder)) {
      shiny::showNotification(
        "Open a project first — the report bundle is written to its glmnet output folder.",
        type = "error", duration = 6)
      return()
    }
    if (!dir.exists(folder)) dir.create(folder, recursive = TRUE)
    # Stamp the bundle with the fit time so the .qmd bundle and everything it
    # renders to (HTML/Word/PDF) carry the same fit timestamp as the other
    # outputs of this fit (groups a fit's files; lets a Trilogy run gather them).
    base <- paste0(
      tools::file_path_sans_ext(data_out$file_name() %||% "glmnet_report"),
      "_", earthUI:::fit_stamp_(model_out$fit_ts()))

    session$sendCustomMessage("report_timer", list(action = "start"))
    # Heavy work inside tryCatch returns the qmd path (or the error); all
    # reactive/state updates happen in straight-line code afterwards.
    qmd_path <- tryCatch({
      model <- model_out$model()
      lambda <- model_out$lambda()
      gamma <- model_out$gamma()
      alpha_val <- tryCatch({
        a <- if (inherits(model, "cv.glmnet")) {
          model$glmnet.fit$call$alpha
        } else {
          model$call$alpha
        }
        if (is.null(a) || is.language(a)) 1 else as.numeric(a)
      }, error = function(e) 1)
      assets_args <- list(
        model         = model,
        lambda        = lambda,
        gamma         = gamma,
        x_mat         = model_out$x_matrix(),
        y_vec         = model_out$y_vector(),
        coef_df       = coef_out$coef_df(),
        predictors    = data_out$predictors() %||% character(0),
        response      = data_out$response() %||% "y",
        data          = data_out$data(),
        col_types     = data_out$col_types(),
        purpose       = as.character(input$purpose %||% "general"),
        alpha         = as.numeric(alpha_val %||% 1),
        family        = as.character(model_out$family() %||% "gaussian"),
        standardize   = TRUE,
        relaxed       = !is.null(gamma),
        lambda_method = if (inherits(model, "cv.glmnet")) "cv" else "manual",
        data_file_name = as.character(data_out$file_name() %||% ""),
        earth_import  = tryCatch(earth_import_r(), error = function(e) NULL)
      )
      generate_quarto_report_glmnet(assets_args, dest_dir = folder, base = base)
    }, error = function(e) e)
    session$sendCustomMessage("report_timer", list(action = "stop"))
    session$sendCustomMessage("btn_done", list(id = "generate_qmd_btn"))

    if (inherits(qmd_path, "error")) {
      shiny::showModal(shiny::modalDialog(
        title = "Report Generation Error",
        shiny::tags$pre(style = "white-space: pre-wrap; max-height: 400px; overflow-y: auto;",
                        conditionMessage(qmd_path)),
        easyClose = TRUE, footer = shiny::modalButton("Close")))
      return()
    }
    shiny::updateTextInput(session, "convert_qmd_path", value = qmd_path)
    shiny::showNotification(
      paste0("Quarto bundle generated. Source: ", qmd_path),
      type = "message", duration = 8)
  })

  # --- Section 11: Convert Quarto Report (.qmd -> HTML/Word/PDF) ---
  shinyFiles::shinyFileChoose(input, "convert_qmd_browse", roots = volumes,
                              session = session, filetypes = c("qmd"))
  observeEvent(input$convert_qmd_browse, {
    fi <- shinyFiles::parseFilePaths(volumes, input$convert_qmd_browse)
    if (nrow(fi) >= 1L)
      shiny::updateTextInput(session, "convert_qmd_path",
                             value = as.character(fi$datapath[1L]))
  })

  observeEvent(input$convert_qmd_btn, {
    qmd <- trimws(input$convert_qmd_path %||% "")
    if (!nzchar(qmd) || !file.exists(qmd)) {
      shiny::showNotification("Choose a Quarto (.qmd) source first.",
                              type = "error", duration = 5)
      return()
    }
    fmts <- input$convert_formats
    if (is.null(fmts) || length(fmts) == 0L) {
      shiny::showNotification("Pick at least one output format.",
                              type = "error", duration = 5)
      return()
    }
    session$sendCustomMessage("report_timer", list(action = "start"))
    res <- tryCatch(
      convert_quarto_file(qmd, formats = fmts,
                          paper_size = earthUI:::locale_paper_()),
      error = function(e) e)
    session$sendCustomMessage("report_timer", list(action = "stop"))
    session$sendCustomMessage("btn_done", list(id = "convert_qmd_btn"))

    if (inherits(res, "error")) {
      shiny::showModal(shiny::modalDialog(
        title = "Report Conversion Error",
        shiny::tags$pre(style = "white-space: pre-wrap; max-height: 400px; overflow-y: auto;",
                        conditionMessage(res)),
        easyClose = TRUE, footer = shiny::modalButton("Close")))
      return()
    }
    shiny::showNotification(
      paste0("Rendered: ", paste(basename(res), collapse = ", ")),
      type = "message", duration = 8)
  })

  # --- Settings defaults radio ---
  shiny::observeEvent(input$glmnet_defaults_action, {
    action <- input$glmnet_defaults_action
    shiny::req(data_out$file_name())

    if (action == "use_default") {
      session$sendCustomMessage("apply_saved_defaults", list(
        filename = data_out$file_name()
      ))
    } else if (action == "glmnet_defaults") {
      session$sendCustomMessage("apply_glmnet_defaults", list())
      shiny::showNotification("glmnet default parameters applied.",
                              type = "message", duration = 3)
      shiny::updateRadioButtons(session, "glmnet_defaults_action",
                                selected = "last")
    }
  }, ignoreInit = TRUE)

  # No saved defaults notification
  shiny::observeEvent(input$glmnet_no_defaults, {
    shiny::showNotification(
      "No default settings saved yet. Use 'Save current as default' first.",
      type = "warning", duration = 4
    )
    shiny::updateRadioButtons(session, "glmnet_defaults_action",
                              selected = "last")
  })

  # --- Save current settings as default ---
  shiny::observeEvent(input$glmnet_save_defaults, {
    shiny::req(data_out$file_name())
    session$sendCustomMessage("collect_and_save_defaults", list(
      filename = data_out$file_name()
    ))
    shiny::showNotification("Current settings saved as defaults.",
                            type = "message", duration = 3)
  })
}
