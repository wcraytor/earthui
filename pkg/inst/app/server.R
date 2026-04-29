function(input, output, session) {

  # --- Ensure standard Mac/Homebrew binary locations are on PATH so the
  # quarto / pandoc / latex CLIs are findable from this R process even when
  # the launcher (RStudio, launchd, etc.) stripped them from PATH. ---
  local({
    cur <- strsplit(Sys.getenv("PATH"), ":", fixed = TRUE)[[1L]]
    extra <- c("/usr/local/bin", "/opt/homebrew/bin",
               file.path(Sys.getenv("HOME"), ".TinyTeX", "bin", "x86_64-darwin"),
               file.path(Sys.getenv("HOME"), ".TinyTeX", "bin", "universal-darwin"),
               file.path(Sys.getenv("HOME"), "bin"))
    extra <- extra[dir.exists(extra) & !extra %in% cur]
    if (length(extra) > 0L) {
      Sys.setenv(PATH = paste(c(cur, extra), collapse = ":"))
      message("earthUI: extended PATH with ", paste(extra, collapse = ", "))
    }
  })

  # --- Plot dimension helper (clientData width/height, fixed res=96) ---
  plot_dims_ <- function(id) {
    full_id <- if (!is.null(session$ns)) session$ns(id) else id
    w_key <- paste0("output_", full_id, "_width")
    h_key <- paste0("output_", full_id, "_height")
    list(
      width  = function() session$clientData[[w_key]],
      height = function() session$clientData[[h_key]]
    )
  }

  # --- SQLite settings database ---
  settings_con <- earthUI:::settings_db_connect_()
  session$onSessionEnded(function() {
    earthUI:::settings_db_disconnect_(settings_con)
  })

  # --- Nord theme switching ---
  observe({
    mode <- input$dark_mode
    req(mode)
    tryCatch(
      session$setCurrentTheme(
        if (mode == "dark") nord_dark else nord_light
      ),
      error = function(e) {
        message("Theme switch error (non-fatal): ", conditionMessage(e))
      }
    )
  })

  # --- Event log: appends to <filename>_earthui_log.txt in output folder ---
  # Logs start/end times and elapsed duration for:
  #   5. Fit Earth Model (earth call + tab completions)
  #   6. Download Estimated Sales Prices & Residuals
  #   7. Calculate RCA Adjustments & Download
  #   8. Generate Sales Grid & Download
  #   9. Download Report (per format)
  # One log file per data file, appended incrementally.
  eui_log_ <- local({
    timers <- list()

    get_path <- function() {
      folder <- isolate(input$output_folder)
      fname  <- isolate(rv$file_name)
      if (is.null(folder) || !nzchar(folder)) folder <- path.expand("~/Downloads")
      if (!dir.exists(folder)) dir.create(folder, recursive = TRUE)
      base <- tools::file_path_sans_ext(fname %||% "earthui")
      file.path(folder, paste0(base, "_earthui_log.txt"))
    }

    write_ <- function(line) {
      tryCatch(cat(line, "\n", file = get_path(), append = TRUE),
               error = function(e) NULL)
    }

    list(
      start = function(section) {
        timers[[section]] <<- Sys.time()
        write_(sprintf("[%s] %s — STARTED",
                       format(Sys.time(), "%Y-%m-%d %H:%M:%S"), section))
      },
      end = function(section) {
        t0 <- timers[[section]]
        t1 <- Sys.time()
        elapsed <- if (!is.null(t0)) {
          sprintf("%.1f seconds", as.numeric(difftime(t1, t0, units = "secs")))
        } else "unknown"
        timers[[section]] <<- NULL
        write_(sprintf("[%s] %s — COMPLETED — elapsed %s",
                       format(t1, "%Y-%m-%d %H:%M:%S"), section, elapsed))
      },
      err = function(section, msg) {
        t0 <- timers[[section]]
        t1 <- Sys.time()
        elapsed <- if (!is.null(t0)) {
          sprintf("%.1f seconds", as.numeric(difftime(t1, t0, units = "secs")))
        } else "unknown"
        timers[[section]] <<- NULL
        write_(sprintf("[%s] %s — ERROR after %s — %s",
                       format(t1, "%Y-%m-%d %H:%M:%S"), section, elapsed, msg))
      }
    )
  })

  # Seed history per file (last 5, most recent first)
  rv_seed_history <- reactiveVal(integer(0))

  # --- Reactive values ---
  rv <- reactiveValues(
    data = NULL,
    categoricals = NULL,
    col_types = NULL,
    result = NULL,
    file_name = NULL,
    sheets = NULL,
    file_path = NULL,
    file_ext = NULL,
    fitting = FALSE,
    bg_proc = NULL,
    trace_lines = character(0),
    user_varmod = "lm",       # user's explicit varmod.method choice
    wp_weights = NULL,         # per-target response weights (numeric vector or NULL)
    subset_conditions = list(), # condition rows for subset filter builder
    rca_df = NULL,             # RCA export data for histogram plots
    rca_targets = NULL,        # target variable names for RCA plots
    sg_recommended = NULL,     # recommended comps for sales grid
    sg_others = NULL,          # other comps for sales grid
    current_purpose = "general",   # track current purpose for settings keying
    active_project = NULL,         # active regProj project (1-row data.frame) or NULL
    project_sort = "recent",       # "recent" or "alpha"
    project_refresh_token = 0L     # bump to force re-walk of regProj tree
  )

  # --- Geo DB connection (per-session, not per-render) ---
  geo_con_ <- tryCatch(regproj_geo_db_connect(),
                       error = function(e) NULL)
  # Projects DB connection (per-session) for per-file settings.
  projects_con_ <- tryCatch(regproj_projects_db_connect(),
                            error = function(e) NULL)
  session$onSessionEnded(function() {
    if (!is.null(geo_con_))      try(DBI::dbDisconnect(geo_con_),      silent = TRUE)
    if (!is.null(projects_con_)) try(DBI::dbDisconnect(projects_con_), silent = TRUE)
  })

  # --- Output folder directory browser ---
  volumes <- c(Home = path.expand("~"), shinyFiles::getVolumes()())
  # Add /Volumes on macOS so external/NVMe drives appear
  if (.Platform$OS.type == "unix" && dir.exists("/Volumes")) {
    volumes <- c(volumes, Volumes = "/Volumes")
  }
  # Add /media and /mnt on Linux for mounted drives
  if (.Platform$OS.type == "unix" && dir.exists("/media")) {
    volumes <- c(volumes, Media = "/media")
  }
  if (.Platform$OS.type == "unix" && dir.exists("/mnt")) {
    volumes <- c(volumes, Mounts = "/mnt")
  }
  shinyFiles::shinyDirChoose(input, "output_folder_browse",
                             roots = volumes, session = session,
                             restrictions = system.file(package = "base"))
  observeEvent(input$output_folder_browse, {
    dir_info <- shinyFiles::parseDirPath(volumes, input$output_folder_browse)
    if (length(dir_info) > 0 && nzchar(dir_info)) {
      updateTextInput(session, "output_folder", value = as.character(dir_info))
    }
  })

  # --- regProj root folder (Settings) ---
  # Populate the field at session start from default_regproj_root().
  observe({
    req(session)
    # only set once, on first connect
    isolate({
      if (is.null(input$regproj_root) || !nzchar(input$regproj_root %||% "")) {
        updateTextInput(session, "regproj_root", value = default_regproj_root())
      }
    })
  })

  shinyFiles::shinyDirChoose(input, "regproj_root_browse",
                             roots = volumes, session = session,
                             restrictions = system.file(package = "base"))
  observeEvent(input$regproj_root_browse, {
    dir_info <- shinyFiles::parseDirPath(volumes, input$regproj_root_browse)
    if (length(dir_info) > 0 && nzchar(dir_info)) {
      updateTextInput(session, "regproj_root", value = as.character(dir_info))
    }
  })

  observeEvent(input$regproj_root_save, {
    p <- trimws(input$regproj_root %||% "")
    if (!nzchar(p)) {
      showNotification("regProj root folder is empty.",
                       type = "error", duration = 5); return()
    }
    p <- path.expand(p)
    if (!dir.exists(p)) {
      ok <- tryCatch({ dir.create(p, recursive = TRUE); TRUE },
                     error = function(e) FALSE,
                     warning = function(w) FALSE)
      if (!ok || !dir.exists(p)) {
        showNotification(sprintf("Cannot create or access: %s", p),
                         type = "error", duration = 6); return()
      }
    }
    prefs <- earthui_prefs_read()
    prefs$regproj_root <- p
    earthui_prefs_write(prefs)
    rv$project_refresh_token <- rv$project_refresh_token + 1L
    showNotification(sprintf("regProj root saved: %s", p),
                     type = "message", duration = 5)
  })

  # --- regProj Project Location cascade (Phase B) ---

  # Helper: returns TRUE if this level has shipped reference data
  # (US state and US county currently). Such levels render as plain
  # selectInput with no free-typing; others render as selectizeInput
  # with create=TRUE so users can pick existing or type new.
  level_has_shipped_ <- function(country, level_idx) {
    country == "us" && level_idx %in% c(1L, 2L)
  }

  # Helper: build choices for a level dropdown via a single targeted
  # query against admin_entries. Returns "Full Name (code)" -> code.
  # Uses the per-session connection (geo_con_) and the
  # (country, level, parent_codes) index for fast lookup.
  build_level_choices_ <- function(country, parent_codes, level_idx) {
    if (is.null(geo_con_)) return(character(0))
    parent_path <- paste(parent_codes, collapse = "/")
    res <- DBI::dbGetQuery(geo_con_,
      "SELECT code, name FROM admin_entries
        WHERE country = ? AND level = ? AND parent_codes = ?
        ORDER BY name",
      params = list(country, as.integer(level_idx), parent_path))
    if (nrow(res) == 0L) return(character(0))
    labels <- paste0(res$name, " (", res$code, ")")
    setNames(res$code, labels)
  }

  # (Sidebar Project Location cascade + auto-populate observer removed
  # in Phase E — replaced by the active-project model. The active-project
  # observer below sets input$output_folder from the project's path.)

  # --- Project picker (Phase B) ---

  # Reactive list of projects on disk (re-walks when refresh_token bumps
  # or when active_project / project_sort change).
  projects_df_ <- reactive({
    rv$project_refresh_token
    regproj_list_projects(sort_by = rv$project_sort)
  })

  # Render the Project section: dropdown + sort + [+ New…] / [Close Project]
  # / empty-state message.
  output$regproj_project_ui <- renderUI({
    df <- projects_df_()
    active <- rv$active_project

    # Active project banner (when one is selected)
    if (!is.null(active)) {
      pur_label <- switch(active$purpose,
                          gen = "General", appr = "Appraisal",
                          mktarea = "Market Area", active$purpose)
      loc_friendly <- paste(pur_label, "·",
                            toupper(active$country), "·",
                            toupper(active$state), "·",
                            active$county, "·", active$city)
      return(tagList(
        tags$div(style = "margin-bottom:4px;", strong(active$project_name)),
        tags$div(class = "small text-muted",
                 style = "margin-bottom:8px; word-break: break-all;",
                 loc_friendly),
        tags$div(style = "display:flex; gap:6px; flex-wrap: wrap;",
          actionButton("regproj_project_close", "Close Project",
                       class = "btn btn-outline-secondary btn-sm"),
          actionButton("regproj_project_info", "Info",
                       class = "btn btn-outline-secondary btn-sm"),
          actionButton("regproj_project_new", "+ New…",
                       class = "btn btn-outline-primary btn-sm")
        )
      ))
    }

    # No active project: show picker / empty-state + [+ New…]
    if (nrow(df) == 0L) {
      return(tagList(
        tags$div(class = "small text-muted", style = "margin-bottom:8px;",
                 "(no projects yet — click + New to create one)"),
        actionButton("regproj_project_new", "+ New…",
                     class = "btn btn-outline-primary btn-sm")
      ))
    }

    # Build choices: "Project Name — purpose / country / .../ city" -> path
    labels <- paste0(df$project_name, "  —  ",
                     df$purpose, " / ", df$country, " / ", df$state,
                     " / ", df$county, " / ", df$city)
    choices <- setNames(df$project_path, labels)
    tagList(
      selectizeInput("regproj_project_pick", NULL,
                     choices  = c("(select a project)" = "", choices),
                     selected = "", width = "100%"),
      tags$div(style = "display:flex; gap:6px; align-items:center;",
        radioButtons("regproj_project_sort", NULL,
                     choices = c("Recent" = "recent", "A-Z" = "alpha"),
                     selected = rv$project_sort, inline = TRUE),
        actionButton("regproj_project_new", "+ New…",
                     class = "btn btn-outline-primary btn-sm")
      )
    )
  })

  # Sort toggle observer
  observeEvent(input$regproj_project_sort, {
    rv$project_sort <- input$regproj_project_sort
  }, ignoreInit = TRUE)

  # Picking a project from the dropdown sets it active
  observeEvent(input$regproj_project_pick, {
    p <- input$regproj_project_pick
    if (is.null(p) || !nzchar(p)) return()
    df <- projects_df_()
    row <- df[df$project_path == p, , drop = FALSE]
    if (nrow(row) == 1L) rv$active_project <- row
  }, ignoreInit = TRUE)

  # [Close Project] returns to the empty / picker state
  observeEvent(input$regproj_project_close, {
    rv$active_project <- NULL
  })

  # [+ New…] — open the New Project modal. Reset inputs first so previous
  # state (state/county/city selections, project name, etc.) doesn't leak.
  observeEvent(input$regproj_project_new, {
    updateRadioButtons(session, "np_purpose", selected = "appr")
    updateTextInput(session, "np_project_name", value = "")
    updateSelectInput(session, "np_country", selected = "us")
    schema <- country_schema("us")
    for (i in seq_along(schema)) {
      lid <- paste0("np_level_", i)
      if (level_has_shipped_("us", i)) {
        updateSelectInput(session, lid, selected = "")
      } else {
        updateSelectizeInput(session, lid, selected = "")
      }
    }
    showModal(modalDialog(
      title = "Create New Project",
      size = "l",
      easyClose = FALSE,
      footer = tagList(
        modalButton("Cancel"),
        actionButton("np_create", "Create Project", class = "btn-primary")
      ),

      tags$div(class = "small text-muted",
               style = "margin-bottom: 12px;",
               "Country, State, County, City, and Purpose are locked once the project is created. Project Name can be renamed later (v1.5)."),

      textInput("np_project_name", "Project Name *",
                placeholder = "lowercase, _-, max 24 chars (e.g. lakemerritt_20260424)",
                width = "100%"),

      radioButtons("np_purpose", "Purpose *",
                   choices = c("General" = "gen",
                               "For Appraisal" = "appr",
                               "Market Area Analysis" = "mktarea"),
                   selected = "appr", inline = TRUE),

      hr(),
      tags$h5("Project Location"),

      uiOutput("np_country_ui"),
      uiOutput("np_levels_ui"),

      hr(),
      tags$h5("Initial Data File", style = "margin-bottom:4px;"),
      tags$div(class = "small text-muted", style = "margin-bottom:8px;",
               "Optional — you can also import files later. The file is copied into the project's in/ folder; the original name is preserved."),
      fileInput("np_data_file", NULL,
                accept = c(".csv", ".xlsx", ".xls"),
                width = "100%")
    ))
  })

  # Country dropdown for the New Project modal
  output$np_country_ui <- renderUI({
    ref <- regproj_reference()
    labels <- paste0(unlist(ref$countries), " (", names(ref$countries), ")")
    choices <- setNames(names(ref$countries), labels)
    selectInput("np_country", "Country *",
                choices = choices, selected = "us", width = "100%")
  })

  # Dynamic admin-level cascade for the New Project modal. Mirrors the
  # sidebar cascade but uses np_level_<n> input IDs to avoid collisions.
  output$np_levels_ui <- renderUI({
    cc <- input$np_country %||% "us"
    schema <- country_schema(cc)
    if (length(schema) == 0L) {
      return(tags$div(class = "small text-muted",
                      "(no admin levels for this country)"))
    }

    last_idx <- length(schema)
    parent_codes <- character(0)

    inputs <- lapply(seq_along(schema), function(i) {
      lbl <- tools::toTitleCase(gsub("_", " ", schema[i]))

      # For PARENT levels (1..N-1): read input reactively so cascading
      # dropdowns update when the parent changes. For the LEAF level (N):
      # do NOT read input$np_level_<N> or input$np_level_<N>_name — that
      # would re-render the cascade on every keystroke and lose focus.
      if (i < last_idx) {
        sel_val <- input[[paste0("np_level_", i)]]
        if (!is.null(sel_val) && nzchar(sel_val)) parent_codes[i] <<- sel_val
        sel <- if (!is.null(sel_val) && nzchar(sel_val)) sel_val else NULL
      } else {
        sel <- NULL
      }
      ch <- build_level_choices_(cc, parent_codes[seq_len(i - 1L)], i)

      if (level_has_shipped_(cc, i)) {
        selectInput(paste0("np_level_", i), paste0(lbl, " *"),
                    choices = c("(pick…)" = "", ch),
                    selected = sel, width = "100%")
      } else if (i == last_idx) {
        # City leaf: selectize+create populated from cities table for the
        # chosen county (shipped + user-added). Display "Name (code)";
        # value is the full name. On Create we resolve name->code via the
        # cities table, or auto-abbreviate for new typed names.
        ch_pairs <- ch
        choices <- if (length(ch_pairs) == 0L) {
          c("(pick a city or type new)" = "")
        } else {
          labels <- names(ch_pairs)
          names_only <- sub(" \\([^)]*\\)$", "", labels)
          c("(pick a city or type new)" = "",
            setNames(names_only, labels))
        }
        selectizeInput(paste0("np_level_", i), paste0(lbl, " *"),
                       choices  = choices,
                       selected = "",
                       options  = list(create = TRUE,
                                       placeholder = "Pick existing or type a new full name"),
                       width    = "100%")
      } else {
        selectizeInput(paste0("np_level_", i), paste0(lbl, " *"),
                       choices  = ch,
                       selected = sel,
                       options  = list(create = TRUE,
                                       placeholder = "Pick existing or type a new name"),
                       width    = "100%")
      }
    })
    do.call(tagList, inputs)
  })

  # Create Project — validate everything, mkdir, write index entries,
  # copy the data file (if any), and set as active.
  observeEvent(input$np_create, {
    cc <- input$np_country %||% ""
    schema <- country_schema(cc)
    last_idx <- length(schema)
    proj <- trimws(input$np_project_name %||% "")
    pur <- input$np_purpose %||% "appr"

    # Field validation
    if (!nzchar(proj) || !grepl("^[A-Za-z0-9_-]+$", proj) || nchar(proj) > 24L) {
      showNotification("Project Name must match ^[A-Za-z0-9_-]+$ (letters, digits, _ and -) and be at most 24 chars.",
                       type = "error", duration = 5); return()
    }
    if (length(schema) == 0L) {
      showNotification("This country has no admin levels defined.",
                       type = "error", duration = 5); return()
    }

    # Collect level codes (with city-leaf full-name → short-name handling)
    levels <- character(length(schema))
    parent_codes <- character(0)
    for (i in seq_along(schema)) {
      val <- input[[paste0("np_level_", i)]]
      val <- if (is.null(val)) "" else trimws(as.character(val))

      if (level_has_shipped_(cc, i)) {
        if (!nzchar(val)) {
          showNotification(sprintf("Missing %s.", schema[i]),
                           type = "error", duration = 5); return()
        }
        levels[i] <- val
      } else if (i == last_idx) {
        # City leaf: val is the full name string (from selectize+create).
        # Look it up by name in the cities table for this scope; if found
        # use the existing code, else auto-derive a new code and persist.
        full_name <- val
        if (!nzchar(full_name)) {
          showNotification("City is required.",
                           type = "error", duration = 5); return()
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
        # Free-typed admin (selectize+create): if val is not in shipped/index,
        # treat as new name and abbreviate
        ch <- build_level_choices_(cc, parent_codes, i)
        if (val %in% ch) {
          levels[i] <- val
        } else {
          existing <- unname(unlist(ch))
          code <- city_abbreviation(val, existing)
          regproj_index_put(paste(c(cc, parent_codes), collapse = "/"),
                            val, code)
          levels[i] <- code
        }
      }
      parent_codes[i] <- levels[i]
    }

    # Compose flat segment + project root, then uniqueness check
    flat <- tryCatch(regproj_flat_segment(cc, levels, proj),
                     error = function(e) {
                       showNotification(paste("Path error:", conditionMessage(e)),
                                        type = "error", duration = 5); NULL
                     })
    if (is.null(flat)) return()
    proj_root <- file.path(default_regproj_root(), pur, flat)
    if (dir.exists(proj_root)) {
      showNotification(sprintf("A project named '%s' already exists in this location.", proj),
                       type = "error", duration = 5); return()
    }

    # Create the full multi-OS directory tree as flat siblings under the
    # project root: <project>/{mac_in, mac_out_earth, mac_out_glmnet, ...,
    # ubuntu_in, ubuntu_out_earth, ..., win11_in, win11_out_earth, ...}.
    # Any user on any OS can drop input files later without mkdir.
    in_dir <- NULL
    for (os_seg in c("mac", "ubuntu", "win11")) {
      io_in <- regproj_path(pur, cc, levels, proj, os = os_seg,
                            in_or_out = "in", create = TRUE)
      for (m in c("earth", "glmnet", "mgcv", "combined")) {
        regproj_path(pur, cc, levels, proj, os = os_seg,
                     in_or_out = "out", method = m, create = TRUE)
      }
      if (os_seg == os_detect()) in_dir <- io_in
    }

    # Copy initial data file (if provided) into the current OS's in/ folder
    fi <- input$np_data_file
    if (!is.null(fi) && nrow(fi) >= 1L && !is.null(in_dir)) {
      orig_name <- fi$name[1L]
      file.copy(fi$datapath[1L], file.path(in_dir, orig_name), overwrite = FALSE)
    }

    # Refresh project list and set the new one active
    rv$project_refresh_token <- rv$project_refresh_token + 1L
    df <- regproj_list_projects(sort_by = rv$project_sort)
    new_row <- df[df$project_path == proj_root, , drop = FALSE]
    if (nrow(new_row) == 1L) rv$active_project <- new_row

    removeModal()
    showNotification(sprintf("Created project '%s' at %s", proj, proj_root),
                     type = "message", duration = 6)
  })

  # [Info] — read-only metadata view for the active project. All fields
  # locked in v1; project rename arrives in v1.5 (folder rename + settings
  # DB key update).
  observeEvent(input$regproj_project_info, {
    p <- rv$active_project
    if (is.null(p)) return()
    pur_label <- switch(p$purpose,
                        gen = "General", appr = "For Appraisal",
                        mktarea = "Market Area Analysis", p$purpose)
    ref <- tryCatch(regproj_reference(), error = function(e) NULL)
    cn_name <- if (!is.null(ref)) ref$countries[[p$country]] %||% p$country
               else p$country
    st_name <- if (!is.null(ref) && !is.null(ref$states[[p$country]]))
                 ref$states[[p$country]][[p$state]] %||% p$state
               else p$state
    county_name <- if (!is.null(ref) &&
                       !is.null(ref$counties[[p$country]]) &&
                       !is.null(ref$counties[[p$country]][[p$state]]))
                     ref$counties[[p$country]][[p$state]][[p$county]] %||% p$county
                   else p$county

    row <- function(lbl, val) tags$tr(
      tags$td(strong(paste0(lbl, ":")), style = "padding:4px 12px 4px 0; vertical-align:top;"),
      tags$td(val, style = "padding:4px 0;")
    )

    showModal(modalDialog(
      title = "Project Information",
      tags$table(
        row("Project Name", tags$code(p$project_name)),
        row("Purpose",      pur_label),
        row("Country",      sprintf("%s (%s)", cn_name, p$country)),
        row("State",        sprintf("%s (%s)", st_name, p$state)),
        row("County",       sprintf("%s (%s)", county_name, p$county)),
        row("City",         tags$code(p$city)),
        row("Last activity", format(p$mtime, "%Y-%m-%d %H:%M:%S")),
        row("Path",
            tags$code(style = "word-break:break-all; font-size:0.85em;",
                      p$project_path))
      ),
      tags$div(class = "small text-muted",
               style = "margin-top:12px;",
               "Project metadata is locked. Folder rename support coming in v1.5."),
      easyClose = TRUE,
      footer = modalButton("Close")
    ))
  })

  # Boolean output for conditionalPanel: is there an active project?
  output$has_active_project <- reactive({ !is.null(rv$active_project) })
  outputOptions(output, "has_active_project", suspendWhenHidden = FALSE)

  # When the active project changes (open / close / switch): set the
  # hidden Purpose radio to match the project, then reset all per-file
  # state. The picker observer below will pick up the new project's
  # last-used file and load it.
  observeEvent(rv$active_project, ignoreNULL = FALSE, {
    p <- rv$active_project
    pur <- if (is.null(p)) "general" else
      switch(p$purpose, gen = "general", appr = "appraisal",
             mktarea = "market", "general")
    updateRadioButtons(session, "purpose", selected = pur)

    # Clear all per-file state on project change
    rv$data        <- NULL
    rv$file_name   <- NULL
    rv$file_ext    <- NULL
    rv$file_path   <- NULL
    rv$sheets      <- NULL
    rv$result      <- NULL
    rv$rca_df      <- NULL
    rv$rca_targets <- NULL
    rv$sg_recommended <- NULL
    rv$sg_others   <- NULL
    rv$trace_lines <- character(0)
    rv$wp_weights  <- NULL
    rv$subset_conditions <- list()
    session$sendCustomMessage("eui_tabs_ready", list(ready = FALSE))
    if (!is.null(rv_report$assets_proc) &&
        inherits(rv_report$assets_proc, "r_process") &&
        rv_report$assets_proc$is_alive()) {
      rv_report$assets_proc$kill()
    }
    rv_report$assets_proc <- NULL
    if (!is.null(rv_report$assets_dir)) {
      unlink(rv_report$assets_dir, recursive = TRUE)
      rv_report$assets_dir <- NULL
    }
    if (is.null(p)) {
      updateTextInput(session, "output_folder", value = "")
      message("earthUI: project closed — all data cleared")
    } else {
      out_dir <- file.path(p$project_path, paste0(os_detect(), "_out_earth"))
      if (!dir.exists(out_dir)) {
        dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)
      }
      updateTextInput(session, "output_folder", value = out_dir)

      message("earthUI: project '", p$project_name,
              "' active — all per-file state cleared, output_folder=",
              out_dir)
      # Auto-load the project's last-used file (if any)
      last <- regproj_last_file_get(p$project_path)
      if (!is.null(last)) {
        in_dir <- file.path(p$project_path, paste0(os_detect(), "_in"))
        full   <- file.path(in_dir, last)
        if (file.exists(full)) load_data_file_(full, last)
      }
    }
  })

  # Track user's explicit varmod.method changes
  observeEvent(input$varmod_method, {
    # Only record if single target (multi-target forces "none" in the UI)
    if (length(input$target) <= 1L) {
      rv$user_varmod <- input$varmod_method
    }
  })

  # When target count changes, update the varmod dropdown
  observeEvent(input$target, {
    if (length(input$target) > 1L) {
      updateSelectInput(session, "varmod_method", selected = "none")
    } else {
      updateSelectInput(session, "varmod_method", selected = rv$user_varmod)
    }
  })

  # --- Locale ---
  # Load user's locale defaults from SQLite on startup
  locale_defaults <- earthUI:::settings_db_read_(settings_con, "__locale_defaults__")
  if (!is.null(locale_defaults) && length(locale_defaults$settings) > 0L) {
    ld <- locale_defaults$settings
    if (!is.null(ld$locale_country))  updateSelectInput(session, "locale_country",  selected = ld$locale_country)
    if (!is.null(ld$locale_paper))    updateSelectInput(session, "locale_paper",    selected = ld$locale_paper)
    if (!is.null(ld$locale_import))   updateSelectInput(session, "locale_import",   selected = ld$locale_import)
    message("earthUI: restored locale defaults from SQLite")
  }

  # Save locale as user default
  observeEvent(input$locale_save_default, {
    locale_settings <- list(
      locale_country = input$locale_country,
      locale_paper   = input$locale_paper,
      locale_import  = input$locale_import
    )
    earthUI:::settings_db_write_(
      settings_con, "__locale_defaults__",
      settings = jsonlite::toJSON(locale_settings, auto_unbox = TRUE)
    )
    showNotification("Locale saved as default for all new files.",
                     type = "message", duration = 4)
  })

  # When Settings country changes, apply to locale env and sync import locale
  observeEvent(input$locale_country, {
    country <- input$locale_country %||% "us"
    presets <- earthUI:::locale_country_presets_()
    preset <- presets[[country]] %||% presets[["us"]]
    updateSelectInput(session, "locale_paper",  selected = preset$paper)
    updateSelectInput(session, "locale_import", selected = country)
    earthUI:::set_locale_(country)
  })

  # When import locale or paper changes, update locale env
  observe({
    # Import locale determines CSV/decimal/date for file import
    import_country <- input$locale_import %||% input$locale_country %||% "us"
    # Settings country determines big_mark for display formatting
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

  # --- Data Import ---
  # Shared file-load helper used by both the project-bound file picker
  # and any legacy upload paths. Reads `path` (a real on-disk file) and
  # presents it under the user-friendly `name`.
  load_data_file_ <- function(path, name) {
    message("earthUI: loading file: ", name, "  <- ", path)
    if (!file.exists(path)) {
      showNotification(sprintf("File not found: %s", path),
                       type = "error", duration = 6)
      return(invisible(NULL))
    }
    ext <- tolower(tools::file_ext(name))
    rv$file_ext  <- ext
    rv$file_path <- path
    rv$file_name <- name

    purpose <- input$purpose %||% "general"
    saved <- NULL
    p <- rv$active_project
    if (!is.null(p) && !is.null(projects_con_)) {
      flat <- basename(p$project_path)
      saved <- tryCatch(
        earthUI:::project_file_settings_read_(projects_con_, flat, rv$file_name),
        error = function(e) {
          message("earthUI: project settings read error: ", e$message); NULL
        })
    }
    if (!is.null(saved)) {
      session$sendCustomMessage("restore_all_settings", list(
        filename     = rv$file_name,
        purpose      = purpose,
        settings     = saved$settings,
        variables    = saved$variables,
        interactions = saved$interactions
      ))
      message("earthUI: restored project settings for: ", rv$file_name,
              " (project=", basename(p$project_path), ")")
    }

    rv$sheets <- if (ext %in% c("xlsx", "xls")) {
      readxl::excel_sheets(path)
    } else NULL

    tryCatch({
      rv$data <- import_data(path, sheet = 1,
                             sep = earthUI:::locale_csv_sep_(),
                             dec = earthUI:::locale_csv_dec_())
      rv$categoricals <- detect_categoricals(rv$data)
      rv$col_types    <- detect_types(rv$data)
      rv$result       <- NULL
      if (!is.null(rv_report$assets_proc) &&
          inherits(rv_report$assets_proc, "r_process") &&
          rv_report$assets_proc$is_alive()) {
        rv_report$assets_proc$kill()
      }
      rv_report$assets_proc <- NULL
      if (!is.null(rv_report$assets_dir)) {
        unlink(rv_report$assets_dir, recursive = TRUE)
        rv_report$assets_dir <- NULL
      }
      rv_seed_history(integer(0))
      message("earthUI: import OK, ", nrow(rv$data), " rows, ",
              ncol(rv$data), " cols")
    }, error = function(e) {
      message("earthUI: IMPORT ERROR: ", e$message)
      showNotification(paste("Import error:", e$message),
                       type = "error", duration = 15)
    })
  }   # end load_data_file_

  # --- Project-bound file picker ---
  # Lists files in active project's <os>/in/. Refreshes on project change
  # and on the explicit Refresh link.
  project_in_files_ <- reactive({
    rv$project_refresh_token  # also bumps when refresh link is clicked
    p <- rv$active_project
    if (is.null(p)) return(character(0))
    regproj_in_files(p$project_path)
  })

  observeEvent(input$regproj_files_refresh, {
    rv$project_refresh_token <- rv$project_refresh_token + 1L
  })

  output$regproj_file_picker_ui <- renderUI({
    files <- project_in_files_()
    p <- rv$active_project
    last <- if (!is.null(p)) regproj_last_file_get(p$project_path) else NULL
    if (length(files) == 0L) {
      return(tags$div(class = "small text-muted",
                      "(no files in this project's in/ folder yet — drop CSV/Excel files into ",
                      tags$code(file.path(p$project_path, paste0(os_detect(), "_in"))),
                      " and click Refresh)"))
    }
    sel <- if (!is.null(last) && last %in% files) last else files[[1L]]
    selectInput("regproj_file_pick", NULL,
                choices = files, selected = sel, width = "100%")
  })

  # Selecting a file from the picker loads it.
  observeEvent(input$regproj_file_pick, {
    f <- input$regproj_file_pick
    p <- rv$active_project
    if (is.null(p) || is.null(f) || !nzchar(f)) return()
    in_dir <- file.path(p$project_path, paste0(os_detect(), "_in"))
    full <- file.path(in_dir, f)
    if (!file.exists(full)) {
      showNotification(sprintf("File not found: %s", full),
                       type = "error", duration = 6); return()
    }
    regproj_last_file_set(p$project_path, f)
    load_data_file_(full, f)
  })

  output$sheet_selector <- renderUI({
    req(rv$sheets)
    selectInput("sheet", "Sheet", choices = rv$sheets, selected = rv$sheets[1])
  })

  observeEvent(input$sheet, {
    req(rv$file_path, input$sheet)
    rv$data <- import_data(rv$file_path, sheet = input$sheet,
                             sep = earthUI:::locale_csv_sep_(),
                             dec = earthUI:::locale_csv_dec_())
    rv$categoricals <- detect_categoricals(rv$data)
    rv$col_types <- detect_types(rv$data)
    rv$result <- NULL
    rv$computed <- NULL
    if (!is.null(rv_report$assets_dir)) {
      unlink(rv_report$assets_dir, recursive = TRUE)
      rv_report$assets_dir <- NULL
    }
  })

  # Seed history dropdown (shows last 5 seeds used, clicking one fills the input)
  output$seed_history_ui <- renderUI({
    hist <- rv_seed_history()
    if (length(hist) == 0L) return(NULL)
    tags$div(style = "margin-top: -8px; margin-bottom: 8px;",
      tags$label("Recent seeds (last used is first):", style = "font-size: 0.8em; color: var(--bs-secondary-color);"),
      tags$div(style = "display: flex; flex-wrap: wrap; gap: 4px;",
        lapply(hist, function(s) {
          actionLink(
            paste0("seed_recall_", s),
            label = as.character(s),
            style = "font-size: 0.8em; padding: 1px 6px; border: 1px solid #ccc; border-radius: 3px; text-decoration: none;")
        })
      )
    )
  })

  # Click a recent seed to fill the input
  observe({
    hist <- rv_seed_history()
    lapply(hist, function(s) {
      observeEvent(input[[paste0("seed_recall_", s)]], {
        updateTextInput(session, "random_seed", value = as.character(s))
      }, ignoreInit = TRUE)
    })
  })

  # Toggle tab waiting messages via JS (not uiOutput — suspended tabs don't render)
  observe({
    ready <- !is.null(rv$result)
    session$sendCustomMessage("eui_tabs_ready", list(ready = ready))
  })
  observe({
    ready <- !is.null(rv$rca_df)
    session$sendCustomMessage("eui_rca_ready", list(ready = ready))
  })

  # In the project model, purpose changes only via project switching.
  # Just track the current value; state cleanup is handled by the
  # active_project observer below (so the load-after-switch sequence
  # doesn't race with a wipe triggered by updateRadioButtons).
  observeEvent(input$purpose, {
    rv$current_purpose <- input$purpose
  }, ignoreInit = TRUE)

  # Update weights dropdown with numeric column names when data loads
  observe({
    req(rv$data)
    num_cols <- names(rv$data)[vapply(rv$data, is.numeric, logical(1))]
    choices <- c("NULL (none)" = "null", stats::setNames(num_cols, num_cols))
    updateSelectInput(session, "weights_col", choices = choices, selected = "null")
  })

  output$data_loaded <- reactive(!is.null(rv$data))
  outputOptions(output, "data_loaded", suspendWhenHidden = FALSE)

  output$model_fitted <- reactive(!is.null(rv$result))
  outputOptions(output, "model_fitted", suspendWhenHidden = FALSE)



  output$report_heading <- renderUI({
    n <- if (identical(input$purpose, "appraisal")) "9" else "7"
    h4(paste0(n, ". Generate Quarto Report"))
  })

  output$download_heading <- renderUI({
    label <- if (identical(input$purpose, "general")) {
      "6. Download Estimated Target Variable(s) & Residuals"
    } else {
      "6. Download Estimated Sale Prices & Residuals"
    }
    h4(label)
  })

  # ── Subset Filter Builder Dialog ───────────────────────────────────
  observeEvent(input$subset_builder_btn, {
    req(rv$data)
    rv$subset_conditions <- list(list(col = names(rv$data)[1], op = "==", val = ""))
    show_subset_modal_()
  })

  show_subset_modal_ <- function() {
    conds <- rv$subset_conditions
    df <- rv$data
    col_choices <- names(df)

    cond_ui <- lapply(seq_along(conds), function(i) {
      cond <- conds[[i]]
      col_name <- cond$col
      col_vals <- df[[col_name]]
      col_class <- class(col_vals)[1]

      # Type-aware value input
      val_input <- if (inherits(col_vals, "Date")) {
        dateInput(paste0("subset_val_", i), NULL,
                  value = if (nzchar(cond$val)) as.Date(cond$val) else Sys.Date())
      } else if (inherits(col_vals, "POSIXct") || inherits(col_vals, "POSIXlt")) {
        dateInput(paste0("subset_val_", i), NULL,
                  value = if (nzchar(cond$val)) as.Date(cond$val) else Sys.Date())
      } else if (is.numeric(col_vals)) {
        numericInput(paste0("subset_val_", i), NULL,
                     value = if (nzchar(cond$val)) as.numeric(cond$val) else NA)
      } else {
        # Character/factor: selectInput with unique values
        uvals <- sort(unique(as.character(col_vals[!is.na(col_vals)])))
        selectInput(paste0("subset_val_", i), NULL,
                    choices = uvals,
                    selected = if (nzchar(cond$val) && cond$val %in% uvals) cond$val else uvals[1])
      }

      connector <- NULL
      if (i > 1) {
        connector <- radioButtons(paste0("subset_conn_", i), NULL,
                                  choices = c("AND" = "&", "OR" = "|"),
                                  selected = if (!is.null(cond$conn)) cond$conn else "&",
                                  inline = TRUE)
      }

      tagList(
        if (!is.null(connector)) tags$div(style = "margin: 4px 0;", connector),
        fluidRow(
          column(4, selectInput(paste0("subset_col_", i), NULL,
                                choices = col_choices, selected = col_name)),
          column(2, selectInput(paste0("subset_op_", i), NULL,
                                choices = c("<", ">", "<=", ">=", "==", "!="),
                                selected = cond$op)),
          column(4, val_input),
          column(2, tags$button("X", class = "btn btn-outline-secondary btn-sm",
                                style = "margin-top: 25px;",
                                onclick = sprintf("Shiny.setInputValue('subset_remove_idx', %d, {priority: 'event'});", i)))
        )
      )
    })

    # Build preview expression
    expr_text <- build_subset_expr_()
    preview <- ""
    if (nzchar(expr_text)) {
      n_match <- tryCatch({
        mask <- as.list(df)
        mask[["TRUE"]] <- TRUE; mask[["FALSE"]] <- FALSE
        mask$as.Date <- as.Date; mask$as.POSIXct <- as.POSIXct
        rows <- eval(parse(text = expr_text), envir = mask, enclos = emptyenv())
        if (is.logical(rows)) sum(rows & !is.na(rows)) else "?"
      }, error = function(e) paste("Error:", e$message))
      preview <- sprintf("%s of %d rows match", n_match, nrow(df))
    }

    # JS to detect column dropdown changes
    n_conds <- length(conds)
    col_change_js <- tags$script(HTML(sprintf("
      $(function() {
        for (var i = 1; i <= %d; i++) {
          (function(idx) {
            $('#subset_col_' + idx).off('change.subsetcol').on('change.subsetcol', function() {
              Shiny.setInputValue('subset_col_changed',
                {idx: idx, col: $(this).val(), t: Date.now()},
                {priority: 'event'});
            });
          })(i);
        }
      });
    ", n_conds)))

    showModal(modalDialog(
      title = "Build Subset Filter",
      size = "l",
      tags$div(id = "subset_conditions_container", cond_ui),
      col_change_js,
      actionButton("subset_add_condition", "+ Add condition",
                   class = "btn-outline-primary btn-sm",
                   style = "margin-top: 8px;"),
      hr(),
      tags$div(
        tags$strong("Expression: "),
        tags$code(if (nzchar(expr_text)) expr_text else "(none)")
      ),
      tags$div(
        style = "margin-top: 4px; font-size: 0.9em;",
        tags$strong("Preview: "), preview
      ),
      footer = tagList(
        actionButton("subset_apply", "Apply", class = "btn-primary"),
        modalButton("Cancel")
      )
    ))
  }

  build_subset_expr_ <- function() {
    conds <- rv$subset_conditions
    if (length(conds) == 0) return("")
    df <- rv$data
    parts <- character(0)
    for (i in seq_along(conds)) {
      cond <- conds[[i]]
      col_name <- cond$col
      op <- cond$op
      val <- cond$val
      if (!nzchar(val) || is.na(val)) next

      col_vals <- df[[col_name]]
      # Format value based on column type
      if (inherits(col_vals, "POSIXct") || inherits(col_vals, "POSIXlt")) {
        val_str <- paste0('as.POSIXct("', val, '")')
      } else if (inherits(col_vals, "Date")) {
        val_str <- paste0('as.Date("', val, '")')
      } else if (is.numeric(col_vals)) {
        val_str <- val
      } else {
        val_str <- paste0('"', gsub('"', '\\\\"', val), '"')
      }

      expr_part <- paste0(col_name, " ", op, " ", val_str)
      if (i > 1 && !is.null(cond$conn)) {
        parts <- c(parts, cond$conn, expr_part)
      } else {
        parts <- c(parts, expr_part)
      }
    }
    paste(parts, collapse = " ")
  }

  # Helper: read current condition values from modal inputs into rv
  sync_subset_inputs_ <- function() {
    conds <- rv$subset_conditions
    for (i in seq_along(conds)) {
      conds[[i]]$col <- input[[paste0("subset_col_", i)]] %||% conds[[i]]$col
      conds[[i]]$op <- input[[paste0("subset_op_", i)]] %||% conds[[i]]$op
      raw_val <- input[[paste0("subset_val_", i)]]
      conds[[i]]$val <- if (is.null(raw_val) || identical(raw_val, "")) "" else as.character(raw_val)
      if (i > 1) {
        conds[[i]]$conn <- input[[paste0("subset_conn_", i)]] %||% "&"
      }
    }
    rv$subset_conditions <- conds
  }

  # Add condition
  observeEvent(input$subset_add_condition, {
    sync_subset_inputs_()
    conds <- rv$subset_conditions
    conds[[length(conds) + 1]] <- list(col = names(rv$data)[1], op = "==", val = "", conn = "&")
    rv$subset_conditions <- conds
    removeModal()
    show_subset_modal_()
  })

  # Remove condition buttons — use JS to send which index to remove
  observeEvent(input$subset_remove_idx, {
    idx <- input$subset_remove_idx
    sync_subset_inputs_()
    conds <- rv$subset_conditions
    if (length(conds) > 1 && idx <= length(conds)) {
      conds[[idx]] <- NULL
      rv$subset_conditions <- conds
      removeModal()
      show_subset_modal_()
    }
  }, ignoreInit = TRUE)

  # Column change — use JS to notify which index changed
  observeEvent(input$subset_col_changed, {
    idx <- input$subset_col_changed$idx
    new_col <- input$subset_col_changed$col
    conds <- rv$subset_conditions
    if (idx <= length(conds) && !identical(conds[[idx]]$col, new_col)) {
      sync_subset_inputs_()
      conds <- rv$subset_conditions
      conds[[idx]]$col <- new_col
      conds[[idx]]$val <- ""
      rv$subset_conditions <- conds
      removeModal()
      show_subset_modal_()
    }
  }, ignoreInit = TRUE)

  # Apply subset filter
  observeEvent(input$subset_apply, {
    sync_subset_inputs_()
    expr_text <- build_subset_expr_()
    updateTextInput(session, "subset_arg", value = expr_text)
    removeModal()
  })

  # ── Response Weights (wp) Dialog ───────────────────────────────────

  # Reset wp when target changes to single
  observeEvent(input$target, {
    if (length(input$target) <= 1L) {
      rv$wp_weights <- NULL
      session$sendCustomMessage("update_wp_display",
        list(text = "NULL (equal weights)"))
    }
  })

  # Disable wp button when single target
  observe({
    if (length(input$target) > 1L) {
      shinyjs_run <- function(code) {
        session$sendCustomMessage("wp_btn_state", list(disabled = FALSE))
      }
      session$sendCustomMessage("wp_btn_state", list(disabled = FALSE))
    } else {
      session$sendCustomMessage("wp_btn_state", list(disabled = TRUE))
    }
  })

  observeEvent(input$wp_set_btn, {
    targets <- input$target
    if (length(targets) <= 1L) {
      showNotification("Response weights require multiple target variables.",
                       type = "warning", duration = 4)
      return()
    }

    # Build one numericInput per target
    weight_inputs <- lapply(seq_along(targets), function(i) {
      current_val <- if (!is.null(rv$wp_weights) && i <= length(rv$wp_weights)) {
        rv$wp_weights[i]
      } else {
        1
      }
      numericInput(paste0("wp_val_", i), targets[i],
                   value = current_val, min = 0, step = 0.1)
    })

    showModal(modalDialog(
      title = "Response Weights (wp)",
      tags$p("Set a numeric weight for each target variable.",
             style = "font-size: 0.9em; color: #666;"),
      weight_inputs,
      footer = tagList(
        actionButton("wp_apply", "Apply", class = "btn-primary"),
        modalButton("Cancel")
      )
    ))
  })

  observeEvent(input$wp_apply, {
    targets <- input$target
    weights <- vapply(seq_along(targets), function(i) {
      val <- input[[paste0("wp_val_", i)]]
      if (is.null(val) || is.na(val)) 1 else as.numeric(val)
    }, numeric(1))
    rv$wp_weights <- weights
    display <- paste0(targets, " = ", weights, collapse = ", ")
    session$sendCustomMessage("update_wp_display", list(text = display))
    # Persist to localStorage
    fn <- rv$file_name %||% "default"
    wp_data <- stats::setNames(as.list(weights), targets)
    session$sendCustomMessage("save_wp_weights", list(filename = fn, weights = wp_data))
    removeModal()
  })

  # Restore wp weights from localStorage when data/target changes
  observeEvent(input$wp_weights_restored, {
    restored <- input$wp_weights_restored
    if (!is.null(restored) && length(restored) > 0 && length(input$target) > 1L) {
      targets <- input$target
      weights <- vapply(targets, function(t) {
        val <- restored[[t]]
        if (is.null(val)) 1 else as.numeric(val)
      }, numeric(1))
      rv$wp_weights <- weights
      display <- paste0(targets, " = ", weights, collapse = ", ")
      session$sendCustomMessage("update_wp_display", list(text = display))
    }
  }, ignoreInit = TRUE)

  # --- Persist settings to projects.sqlite (debounced from JS) ---
  # Settings are scoped to (active project, file basename). With no active
  # project, saves are skipped — there's no place to write them.
  observeEvent(input$eui_save_trigger, {
    payload <- input$eui_save_trigger
    req(payload$filename)
    p <- rv$active_project
    if (is.null(p) || is.null(projects_con_)) return()
    flat <- basename(p$project_path)
    tryCatch({
      earthUI:::project_file_settings_write_(
        projects_con_, flat, payload$filename,
        settings     = if (!is.null(payload$settings))     payload$settings     else "{}",
        variables    = if (!is.null(payload$variables))    payload$variables    else "{}",
        interactions = if (!is.null(payload$interactions)) payload$interactions else "{}",
        method       = "earth"
      )
    }, error = function(e) {
      message("earthUI: project settings save error: ", e$message)
    })
  }, ignoreInit = TRUE)

  # --- Default settings (save/restore via SQLite with key "__defaults__") ---

  # Radio: choose mode (last per-file settings vs saved defaults)
  observeEvent(input$eui_defaults_action, {
    action <- input$eui_defaults_action
    message("earthUI: defaults radio changed to: '", action, "'")
    req(rv$file_name)

    if (action == "use_default") {
      defaults <- earthUI:::settings_db_read_(settings_con, "__defaults__")
      if (!is.null(defaults)) {
        session$sendCustomMessage("restore_all_settings", list(
          filename     = rv$file_name,
          settings     = defaults$settings,
          variables    = defaults$variables,
          interactions = defaults$interactions,
          apply        = TRUE
        ))
        showNotification("Default settings applied.", type = "message",
                         duration = 3)
      } else {
        showNotification("No default settings saved yet. Use 'Save current as default' first.",
                         type = "warning", duration = 4)
        updateRadioButtons(session, "eui_defaults_action", selected = "last")
      }

    } else if (action == "earth_defaults") {
      session$sendCustomMessage("apply_earth_defaults", list())
      showNotification("Earth default parameters applied.", type = "message",
                       duration = 3)
      updateRadioButtons(session, "eui_defaults_action", selected = "last")
    }
    # "last" = do nothing, use whatever localStorage has
  }, ignoreInit = TRUE)

  # Button: save current settings as the default
  observeEvent(input$eui_save_defaults, {
    req(rv$file_name)
    message("earthUI: saving current settings as defaults for: ", rv$file_name)
    session$sendCustomMessage("collect_and_save_defaults", list(
      filename = rv$file_name
    ))
    showNotification("Current settings saved as defaults.", type = "message",
                     duration = 3)
  })

  output$data_preview_info <- renderUI({
    req(rv$data)
    tags$div(
      class = "alert alert-info",
      style = "font-size: 0.85em; padding: 8px;",
      sprintf("%d rows, %d columns", nrow(rv$data), ncol(rv$data))
    )
  })

  # Shared DataTable callback for click-to-popup on cells
  cell_popup_js <- DT::JS("
    table.on('click', 'td', function() {
      var text = $(this).text();
      if (text.length > 0) {
        var $popup = $('#eui-cell-popup');
        if (!$popup.length) {
          $popup = $('<div id=\"eui-cell-popup\">' +
            '<div class=\"eui-popup-backdrop\"></div>' +
            '<div class=\"eui-popup-content\"><pre></pre>' +
            '<button class=\"btn btn-sm btn-secondary eui-popup-close\">Close</button></div></div>');
          $('body').append($popup);
          $popup.on('click', '.eui-popup-backdrop, .eui-popup-close', function() {
            $popup.hide();
          });
        }
        $popup.find('pre').text(text);
        $popup.show();
      }
    });
  ")

  preview_data_ <- function() {
    req(rv$data)
    df <- rv$data
    if (input$purpose != "appraisal" && isTRUE(input$skip_subject_row) && nrow(df) >= 2L) {
      df <- df[2:nrow(df), , drop = FALSE]
    }
    df
  }

  output$data_preview <- DT::renderDataTable({
    df <- preview_data_()
    DT::datatable(df,
                  options = list(pageLength = 10, scrollX = TRUE),
                  rownames = FALSE,
                  class = "compact stripe",
                  callback = cell_popup_js)
  })

  output$data_preview_tab <- DT::renderDataTable({
    df <- preview_data_()
    DT::datatable(df,
                  options = list(pageLength = 10, scrollX = TRUE),
                  rownames = FALSE,
                  class = "compact stripe",
                  callback = cell_popup_js)
  })

  # --- Appraisal: subject (row 1) and comps (rows 2+) ---
  render_subjects_ <- function() {
    req(rv$data, input$purpose == "appraisal", nrow(rv$data) >= 1L)
    tgt <- input$target
    preds <- input$predictors
    # Include display_only columns
    display_cols <- character(0)
    specials <- input$col_specials
    if (!is.null(specials)) {
      for (nm in names(specials)) {
        if (specials[[nm]] == "display_only") display_cols <- c(display_cols, nm)
      }
    }
    show_cols <- unique(c(display_cols, tgt, preds))
    show_cols <- intersect(show_cols, names(rv$data))
    # Show all columns if no target/predictors selected yet
    if (length(show_cols) == 0L) show_cols <- names(rv$data)
    subj <- rv$data[1L, show_cols, drop = FALSE]
    if (!is.null(tgt) && length(tgt) > 0L) {
      for (t in tgt) {
        if (t %in% names(subj)) subj[[t]] <- NA
      }
    }
    subj <- cbind(data.frame(row = 1L, check.names = FALSE), subj)
    DT::datatable(subj,
                  options = list(pageLength = 1, scrollX = TRUE, dom = "t"),
                  rownames = FALSE, class = "compact stripe",
                  callback = cell_popup_js)
  }

  render_comps_ <- function() {
    req(rv$data, input$purpose == "appraisal", nrow(rv$data) >= 2L)
    tgt <- input$target
    preds <- input$predictors
    display_cols <- character(0)
    specials <- input$col_specials
    if (!is.null(specials)) {
      for (nm in names(specials)) {
        if (specials[[nm]] == "display_only") display_cols <- c(display_cols, nm)
      }
    }
    show_cols <- unique(c(display_cols, tgt, preds))
    show_cols <- intersect(show_cols, names(rv$data))
    # Show all columns if no target/predictors selected yet
    if (length(show_cols) == 0L) show_cols <- names(rv$data)
    comps <- rv$data[2:nrow(rv$data), show_cols, drop = FALSE]
    comps <- cbind(data.frame(row = seq_len(nrow(comps)), check.names = FALSE), comps)
    low_rows <- integer(0)
    if (!is.null(tgt) && length(tgt) > 0L && tgt[1L] %in% names(comps)) {
      col <- comps[[tgt[1L]]]
      if (is.numeric(col)) {
        low_rows <- which(col <= 100 | is.na(col))
      }
    }
    dt <- DT::datatable(comps,
                        options = list(pageLength = 10, scrollX = TRUE),
                        rownames = FALSE, class = "compact stripe",
                        callback = cell_popup_js)
    if (length(low_rows) > 0L) {
      dt <- DT::formatStyle(dt, columns = 0, target = "row",
              backgroundColor = DT::styleRow(low_rows, "rgba(255,0,0,0.15)"))
    }
    dt
  }

  output$data_subjects     <- DT::renderDataTable(render_subjects_())
  output$data_comps        <- DT::renderDataTable(render_comps_())
  output$data_subjects_tab <- DT::renderDataTable(render_subjects_())
  output$data_comps_tab    <- DT::renderDataTable(render_comps_())

  # --- Variable Configuration ---
  output$target_selector <- renderUI({
    req(rv$data)
    # Depend on purpose so this re-renders and restores per-purpose settings
    purpose <- input$purpose
    storage_key <- if (is.null(rv$file_name)) "default" else rv$file_name

    # JavaScript: persist target variable + advanced parameters in localStorage
    js <- tags$script(HTML(sprintf("
      (function() {
        var storageKeyRaw = %s;
        function getSettingsKey() {
          return window.euiPurposeKey('earthUI_settings_', storageKeyRaw);
        }
        var selectIds = ['target', 'weights_col',
                         'degree', 'pmethod', 'glm_family', 'trace',
                         'varmod_method'];
        // output_folder excluded — owned by the active project, not by saved settings
        var numericIds = ['nprune', 'thresh', 'penalty', 'minspan', 'endspan',
                          'fast_k', 'nfold_override', 'nk', 'newvar_penalty',
                          'fast_beta', 'ncross', 'varmod_exponent', 'varmod_conv',
                          'varmod_clamp', 'varmod_minspan', 'adjust_endspan',
                          'exhaustive_tol', 'subset_arg'];
        var checkboxIds = ['stratify', 'keepxy', 'scale_y', 'auto_linpreds',
                           'use_beta_cache', 'force_xtx_prune', 'get_leverages',
                           'force_weights', 'skip_subject_row'];
        var radioIds = [];
        var dateIds = ['effective_date'];
        var allIds = selectIds.concat(numericIds).concat(checkboxIds).concat(radioIds).concat(dateIds);

        var saved = null;
        try { saved = JSON.parse(localStorage.getItem(getSettingsKey())); } catch(e) {}

        function restoreSettings() {
          if (!saved) return;
          selectIds.forEach(function(id) {
            if (saved[id] !== undefined && saved[id] !== null) {
              var el = document.getElementById(id);
              if (el && el.selectize) {
                if (id === 'target') {
                  // target can be a single value or an array (multi-select)
                  var vals = Array.isArray(saved[id]) ? saved[id] : [saved[id]];
                  var valid = vals.filter(function(v) { return el.selectize.options[v]; });
                  if (valid.length > 0) el.selectize.setValue(valid);
                } else {
                  el.selectize.setValue(saved[id]);
                }
              }
            }
          });
          numericIds.forEach(function(id) {
            if (saved[id] !== undefined) {
              var $el = $('#' + id);
              if ($el.length) { $el.val(saved[id]).trigger('change'); }
            }
          });
          checkboxIds.forEach(function(id) {
            if (saved[id] !== undefined) {
              var $el = $('#' + id);
              if ($el.length) {
                $el.prop('checked', saved[id]);
                $el.trigger('change');
              }
            }
          });
          radioIds.forEach(function(id) {
            if (saved[id] !== undefined) {
              $('input[name=' + id + '][value=' + saved[id] + ']').prop('checked', true).trigger('change');
            }
          });
          dateIds.forEach(function(id) {
            if (saved[id] !== undefined && saved[id] !== null) {
              var $inp = $('#' + id + ' input');
              if ($inp.length) {
                $inp.val(saved[id]).trigger('change');
              } else {
                $('#' + id).val(saved[id]).trigger('change');
              }
            }
          });
        }

        function saveSettings() {
          if (window.euiPurposeSwitching) return;
          var state = {};
          selectIds.forEach(function(id) {
            var el = document.getElementById(id);
            if (el && el.selectize) { state[id] = el.selectize.getValue(); }
          });
          numericIds.forEach(function(id) {
            state[id] = $('#' + id).val();
          });
          checkboxIds.forEach(function(id) {
            state[id] = $('#' + id).is(':checked');
          });
          radioIds.forEach(function(id) {
            state[id] = $('input[name=' + id + ']:checked').val();
          });
          dateIds.forEach(function(id) {
            var $inp = $('#' + id + ' input');
            state[id] = $inp.length ? $inp.val() : $('#' + id).val();
          });
          try { localStorage.setItem(getSettingsKey(), JSON.stringify(state)); } catch(e) {}
        }

        // Restore after selectize initializes (retry until target AND degree are ready)
        // Block saving until restore is complete to prevent defaults overwriting saved values
        var restoreComplete = false;
        var attempts = 0;
        function tryRestore() {
          var targetEl = document.getElementById('target');
          var degreeEl = document.getElementById('degree');
          var targetReady = targetEl && targetEl.selectize && targetEl.selectize.isSetup;
          var degreeReady = degreeEl && degreeEl.selectize && degreeEl.selectize.isSetup;
          if (targetReady && degreeReady) {
            restoreSettings();
            // Restore wp weights from localStorage
            window.euiCurrentFilename = storageKeyRaw;
            setTimeout(function() {
              try {
                var wpSaved = JSON.parse(localStorage.getItem(window.euiPurposeKey('earthUI_wp_', storageKeyRaw)));
                if (wpSaved && Object.keys(wpSaved).length > 0) {
                  Shiny.setInputValue('wp_weights_restored', wpSaved, {priority: 'event'});
                }
              } catch(e) {}
            }, 600);
            restoreComplete = true;
          } else if (attempts < 40) {
            attempts++;
            setTimeout(tryRestore, 250);
          } else {
            restoreComplete = true;  // give up waiting, allow saves
          }
        }
        tryRestore();

        // Save on any tracked input change (only after restore is done)
        $(document).off('shiny:inputchanged.euisettings')
                   .on('shiny:inputchanged.euisettings', function(event) {
          if (restoreComplete && allIds.indexOf(event.name) >= 0) {
            saveSettings();
            if (typeof window.euiSaveToServer === 'function') window.euiSaveToServer(%s);
          }
        });
      })();
    ", jsonlite::toJSON(storage_key, auto_unbox = TRUE),
       jsonlite::toJSON(storage_key, auto_unbox = TRUE))))

    tagList(
      selectInput("target", "Target (response) variable(s)",
                  choices = names(rv$data), multiple = TRUE),
      js
    )
  })

  output$predictor_hint_text <- renderUI({
    hint <- "Type = column data type, Inc = include as predictor, Factor = treat as categorical, Linear = linear-only (no hinges)"
    if (input$purpose %in% c("appraisal", "market")) {
      hint <- paste0(hint, ", Special = column role (e.g. contract_date)")
    }
    tags$p(class = "text-muted", style = "font-size: 0.8em; margin-bottom: 5px;", hint)
  })

  output$variable_table <- renderUI({
    req(rv$data)
    if (is.null(input$target) || length(input$target) == 0L) {
      return(tags$p(class = "text-muted", style = "padding: 12px; text-align: center;",
                    "Select a target variable above to configure predictors."))
    }
    candidates <- setdiff(names(rv$data), input$target)
    nrows <- nrow(rv$data)

    # Storage key for remembering settings
    storage_key <- if (is.null(rv$file_name)) "default" else rv$file_name

    # Type options for dropdown
    type_options <- c("numeric", "integer", "character", "logical",
                      "factor", "Date", "POSIXct", "unknown")

    appraiser <- input$purpose %in% c("appraisal", "market")

    # Special column options
    special_options <- c("no", "actual_age", "area", "concessions",
                         "contract_date", "display_only", "dom",
                         "effective_age", "latitude", "listing_date",
                         "living_area", "longitude", "lot_size",
                         "sale_age", "site_dimensions", "weight")

    # Header row — vertical labels for checkboxes, like glmnetUI
    angled_hdr <- "text-align:center; font-size:0.85em; writing-mode:vertical-lr; transform:rotate(180deg); height:55px; line-height:1; font-weight:bold;"
    header_cols <- list(
      tags$div(style = "flex: 1; min-width: 60px; font-weight: bold; font-size: 0.85em;", "Variable"),
      tags$div(style = "width: 75px; text-align: center; font-weight: bold; font-size: 0.85em;", "Type"),
      tags$div(style = paste0("width: 20px;", angled_hdr), "Include"),
      tags$div(style = paste0("width: 20px;", angled_hdr), "Factor"),
      tags$div(style = paste0("width: 20px;", angled_hdr), "Linear")
    )
    if (appraiser) {
      header_cols <- c(header_cols, list(
        tags$div(style = "width: 80px; text-align: center; font-weight: bold; font-size: 0.85em;", "Special")
      ))
    }
    header_cols <- c(header_cols, list(
      tags$div(style = "width: 32px; text-align: right; padding-right: 4px; font-weight: bold; font-size: 0.85em;", "NAs")
    ))
    header <- tags$div(
      style = "display: flex; align-items: flex-end; padding: 4px 0; border-bottom: 2px solid var(--bs-border-color, #ccc); position: sticky; top: 0; z-index: 1; background: var(--bs-tertiary-bg, var(--bs-body-bg, #f0f0f0)); gap: 2px;",
      header_cols
    )

    # Build rows using numeric index for IDs
    rows <- lapply(seq_along(candidates), function(i) {
      col <- candidates[i]
      n_na <- sum(is.na(rv$data[[col]]))
      pct_na <- n_na / nrows
      na_style <- if (pct_na > 0.3) "color: red;" else ""

      # Auto-detected type for this column
      detected_type <- if (!is.null(rv$col_types) && col %in% names(rv$col_types)) {
        rv$col_types[[col]]
      } else {
        "unknown"
      }

      # Build <option> tags with auto-detected type selected
      option_tags <- lapply(type_options, function(opt) {
        if (opt == detected_type) {
          tags$option(value = opt, selected = "selected", opt)
        } else {
          tags$option(value = opt, opt)
        }
      })

      # Build row cells — checkboxes grouped together (Include, Factor, Linear)
      row_cells <- list(
        tags$div(style = "flex: 1; min-width: 60px; font-size: 0.82em; overflow: hidden; text-overflow: ellipsis; white-space: nowrap;",
                 title = col, col,
                 tags$span(id = paste0("eui_special_badge_", i),
                           style = "font-size: 0.7em; color: #0d6efd; font-style: italic; margin-left: 4px;")),
        tags$div(style = "width: 75px; text-align: center;",
                 tags$select(id = paste0("eui_type_", i),
                             class = "eui-type-select",
                             style = "width: 70px; font-size: 0.75em; padding: 1px 2px; border: 1px solid var(--bs-border-color, #ccc); border-radius: 3px; background: var(--bs-body-bg, #fff); color: var(--bs-body-color, #333);",
                             option_tags)),
        tags$div(style = "width: 20px; text-align: center;",
                 tags$input(type = "checkbox", id = paste0("eui_inc_", i),
                            class = "eui-var-cb")),
        tags$div(style = "width: 20px; text-align: center;",
                 tags$input(type = "checkbox", id = paste0("eui_fac_", i),
                            class = "eui-var-cb")),
        tags$div(style = "width: 20px; text-align: center;",
                 tags$input(type = "checkbox", id = paste0("eui_lin_", i),
                            class = "eui-var-cb"))
      )
      if (appraiser) {
        special_option_tags <- lapply(special_options, function(opt) {
          tags$option(value = opt, opt)
        })
        row_cells <- c(row_cells, list(
          tags$div(style = "width: 80px; text-align: center;",
                   tags$select(id = paste0("eui_special_", i),
                               class = "eui-special-select",
                               style = "width: 74px; font-size: 0.75em; padding: 1px 2px; border: 1px solid var(--bs-border-color, #ccc); border-radius: 3px; background: var(--bs-body-bg, #fff); color: var(--bs-body-color, #333);",
                               special_option_tags))
        ))
      }
      row_cells <- c(row_cells, list(
        tags$div(style = paste0("width: 32px; text-align: right; font-size: 0.8em; padding-right: 4px;", na_style),
                 if (n_na > 0L) as.character(n_na) else "")
      ))

      tags$div(
        style = "display: flex; align-items: center; padding: 2px 0; border-bottom: 1px solid var(--bs-border-color, #eee); gap: 2px;",
        row_cells
      )
    })

    # Detected types as JSON for JS (used to reset to defaults)
    detected_types_list <- vapply(candidates, function(col) {
      if (!is.null(rv$col_types) && col %in% names(rv$col_types)) {
        rv$col_types[[col]]
      } else {
        "unknown"
      }
    }, character(1L))
    detected_types_json <- jsonlite::toJSON(
      as.list(stats::setNames(detected_types_list, candidates)),
      auto_unbox = TRUE
    )

    # JavaScript: sync checkboxes + type dropdowns <-> Shiny inputs, with localStorage persistence
    col_json <- jsonlite::toJSON(candidates, auto_unbox = FALSE)
    n_cols <- length(candidates)
    storage_key_json <- jsonlite::toJSON(storage_key, auto_unbox = TRUE)
    appraiser_json <- if (appraiser) "true" else "false"
    js <- tags$script(HTML(sprintf("
      (function() {
        var cols = %s;
        var n = %d;
        var storageKeyRaw = %s;
        function getVarsKey() {
          return window.euiPurposeKey('earthUI_vars_', storageKeyRaw);
        }
        var detectedTypes = %s;
        var appraiser = %s;

        function gatherState() {
          var inc = [], fac = [], lin = [];
          var types = {};
          var specials = {};
          for (var i = 1; i <= n; i++) {
            var sp = appraiser ? ($('#eui_special_' + i).val() || 'no') : 'no';
            if (appraiser) specials[cols[i-1]] = sp;
            if ($('#eui_inc_' + i).is(':checked') && sp !== 'display_only') inc.push(cols[i-1]);
            if ($('#eui_fac_' + i).is(':checked')) fac.push(cols[i-1]);
            if ($('#eui_lin_' + i).is(':checked')) lin.push(cols[i-1]);
            types[cols[i-1]] = $('#eui_type_' + i).val();
          }
          Shiny.setInputValue('predictors', inc.length > 0 ? inc : null);
          Shiny.setInputValue('categoricals', fac.length > 0 ? fac : null);
          Shiny.setInputValue('linpreds', lin.length > 0 ? lin : null);
          Shiny.setInputValue('col_types', types);
          if (appraiser) {
            Shiny.setInputValue('col_specials', specials);
          }
        }

        function saveState() {
          var state = {};
          for (var i = 1; i <= n; i++) {
            var entry = {
              inc: $('#eui_inc_' + i).is(':checked'),
              fac: $('#eui_fac_' + i).is(':checked'),
              lin: $('#eui_lin_' + i).is(':checked'),
              type: $('#eui_type_' + i).val()
            };
            if (appraiser) {
              var sp = $('#eui_special_' + i).val();
              if (sp) entry.special = sp;
            }
            state[cols[i-1]] = entry;
          }
          if (window.euiPurposeSwitching) return;
          try { localStorage.setItem(getVarsKey(), JSON.stringify(state)); } catch(e) {}
        }

        function restoreState() {
          var saved = null;
          try { saved = JSON.parse(localStorage.getItem(getVarsKey())); } catch(e) {}
          if (saved) {
            for (var i = 1; i <= n; i++) {
              var s = saved[cols[i-1]];
              if (s) {
                $('#eui_inc_' + i).prop('checked', s.inc);
                $('#eui_fac_' + i).prop('checked', s.fac);
                $('#eui_lin_' + i).prop('checked', s.lin);
                if (s.type) {
                  $('#eui_type_' + i).val(s.type);
                }
                if (appraiser && s.special) {
                  $('#eui_special_' + i).val(s.special);
                }
              }
            }
          }
        }

        // Restore saved state, then sync to Shiny
        restoreState();
        updateBadges();
        setTimeout(gatherState, 200);

        // On any checkbox change, save and sync
        $(document).off('change.euivar').on('change.euivar', '.eui-var-cb', function() {
          saveState();
          gatherState();
          if (typeof window.euiSaveToServer === 'function') window.euiSaveToServer(storageKeyRaw);
        });

        // On type dropdown change: auto-link Factor, save and sync
        $(document).off('change.euitype').on('change.euitype', '.eui-type-select', function() {
          var idx = this.id.replace('eui_type_', '');
          var val = $(this).val();
          if (val === 'character' || val === 'factor') {
            $('#eui_fac_' + idx).prop('checked', true);
          }
          saveState();
          gatherState();
          if (typeof window.euiSaveToServer === 'function') window.euiSaveToServer(storageKeyRaw);
        });

        // Update special type badges next to variable names
        function updateBadges() {
          if (!appraiser) return;
          for (var j = 1; j <= n; j++) {
            var sp = $('#eui_special_' + j).val() || 'no';
            var $badge = $('#eui_special_badge_' + j);
            if ($badge.length) {
              $badge.text(sp !== 'no' ? '[' + sp + ']' : '');
            }
          }
        }
        window.euiUpdateBadges = updateBadges;

        // On special dropdown change: enforce single per special type, save and sync
        $(document).off('change.euispecial').on('change.euispecial', '.eui-special-select', function() {
          var idx = parseInt(this.id.replace('eui_special_', ''));
          var val = $(this).val();
          if (val !== 'no' && val !== 'display_only') {
            // Only one column per special type (except display_only allows multiple)
            for (var j = 1; j <= n; j++) {
              if (j !== idx && $('#eui_special_' + j).val() === val) {
                $('#eui_special_' + j).val('no');
              }
            }
          }
          updateBadges();
          saveState();
          gatherState();
          if (typeof window.euiSaveToServer === 'function') window.euiSaveToServer(storageKeyRaw);
        });

        // Expose detectedTypes for earth defaults reset
        window.euiDetectedTypes = detectedTypes;
        window.euiCols = cols;
      })();
    ", col_json, n_cols, storage_key_json, detected_types_json, appraiser_json)))

    tagList(header, rows, js)
  })

  # --- Recommended parameter values (Section 4) ---
  # Helper: styling for recommendation labels (Nord Frost #81A1C1)
  rec_style_ <- "font-size: 0.78em; color: #81A1C1; margin-top: -8px; margin-bottom: 6px; padding-left: 2px;"

  # Reactive: effective n (rows used for fitting)
  fit_n_ <- reactive({
    req(rv$data)
    n <- nrow(rv$data)
    skip <- input$purpose == "appraisal" ||
      (input$purpose != "appraisal" && isTRUE(input$skip_subject_row))
    if (skip && n >= 2L) n - 1L else n
  })

  # Reactive: number of selected predictors
  fit_p_ <- reactive({
    p <- length(input$predictors)
    if (p < 1L) NULL else p
  })

  output$rec_penalty <- renderUI({
    d <- as.integer(input$degree)
    val <- if (!is.null(d) && d > 1L) 3 else 2
    tags$div(style = rec_style_, paste0("Recommended: ", val,
      " (", if (val == 3) "degree > 1" else "degree = 1", ")"))
  })

  output$rec_nk <- renderUI({
    n <- fit_n_()
    p <- fit_p_()
    req(n, p)
    val <- min(100L, max(21L, 2L * p + 1L, as.integer(floor(n / 10))))
    tags$div(style = rec_style_, paste0("Recommended: ", val,
      " = min(100, max(21, 2\u00D7", p, "+1, \u230A", n, "/10\u230B))"))
  })

  output$rec_minspan <- renderUI({
    n <- fit_n_()
    req(n)
    val <- min(16L, as.integer(floor(5 + n / 50)))
    tags$div(style = rec_style_, paste0("Recommended: ", val,
      " = min(16, \u230A5 + ", n, "/50\u230B)"))
  })

  output$rec_endspan <- renderUI({
    n <- fit_n_()
    req(n)
    val <- min(16L, as.integer(floor(5 + n / 28)))
    tags$div(style = rec_style_, paste0("Recommended: ", val,
      " = min(16, \u230A5 + ", n, "/28\u230B)"))
  })

  output$rec_pmethod <- renderUI({
    tags$div(style = rec_style_, "Recommended: backward (deterministic, GCV-based)")
  })

  output$rec_nprune <- renderUI({
    tags$div(style = rec_style_, "Recommended: leave empty (let GCV decide)")
  })

  output$rec_nfold <- renderUI({
    n <- fit_n_()
    req(n)
    val <- min(15L, max(10L, as.integer(round(n / 100))))
    tags$div(style = rec_style_, paste0("Recommended: ", val,
      " = min(15, max(10, \u230A", n, "/100\u230B))"))
  })

  output$rec_ncross <- renderUI({
    n <- fit_n_()
    req(n)
    val <- max(3L, as.integer(ceiling(100 / n)))
    tags$div(style = rec_style_, paste0("Recommended: ", val,
      " = max(3, \u2308100/", n, "\u2309)"))
  })

  output$rec_varmod <- renderUI({
    tags$div(style = rec_style_, "Recommended: lm (prediction intervals via linear variance model)")
  })

  # --- Allowed Interaction Matrix ---
  output$allowed_matrix_ui <- renderUI({
    req(input$predictors)
    preds <- input$predictors
    if (length(preds) < 2) {
      return(p("Need at least 2 predictors for interactions."))
    }

    n <- length(preds)

    # Column headers: empty corner cell + variable names
    header_cells <- list(tags$th(style = "padding: 2px;", ""))
    for (j in seq_len(n)) {
      header_cells <- c(header_cells, list(
        tags$th(class = "eui-matrix-varlabel",
                `data-var-idx` = j,
                style = "padding: 2px 4px; font-size: 0.75em; text-align: center; writing-mode: vertical-lr; transform: rotate(180deg); max-height: 100px; overflow: hidden; cursor: pointer;",
                title = preds[j], preds[j])
      ))
    }
    header_row <- tags$tr(class = "eui-matrix-header", header_cells)

    # Build matrix rows
    body_rows <- list()
    for (i in seq_len(n)) {
      cells <- list(
        tags$td(class = "eui-matrix-rowlabel eui-matrix-varlabel",
                `data-var-idx` = i,
                style = "padding: 2px 4px; font-size: 0.75em; white-space: nowrap; overflow: hidden; text-overflow: ellipsis; max-width: 100px; cursor: pointer;",
                title = preds[i], preds[i])
      )
      for (j in seq_len(n)) {
        if (j > i) {
          # Upper triangle: checkbox
          id <- paste0("allowed_", i, "_", j)
          cells <- c(cells, list(
            tags$td(style = "text-align: center; padding: 2px;",
                    tags$input(type = "checkbox", id = id,
                               class = "eui-interaction-cb", checked = "checked",
                               style = "margin: 0;"))
          ))
        } else {
          # Diagonal and lower triangle: empty
          cells <- c(cells, list(
            tags$td(style = "padding: 2px;",
                    if (i == j) "\u00b7" else "")
          ))
        }
      }
      body_rows <- c(body_rows, list(tags$tr(cells)))
    }

    # JavaScript to sync checkboxes with Shiny inputs + localStorage persistence
    storage_key <- if (is.null(rv$file_name)) "default" else rv$file_name
    js <- tags$script(HTML(sprintf("
      (function() {
        var n = %d;
        var interStorageKeyRaw = %s;
        function getInterKey() {
          return window.euiPurposeKey('earthUI_interactions_', interStorageKeyRaw);
        }

        function saveState() {
          if (window.euiPurposeSwitching) return;
          var state = {};
          for (var i = 1; i < n; i++) {
            for (var j = i + 1; j <= n; j++) {
              state[i + '_' + j] = $('#allowed_' + i + '_' + j).is(':checked');
            }
          }
          try { localStorage.setItem(getInterKey(), JSON.stringify(state)); } catch(e) {}
        }

        function restoreState() {
          var saved = null;
          try { saved = JSON.parse(localStorage.getItem(getInterKey())); } catch(e) {}
          if (!saved) return;
          for (var i = 1; i < n; i++) {
            for (var j = i + 1; j <= n; j++) {
              var key = i + '_' + j;
              if (saved[key] !== undefined) {
                $('#allowed_' + i + '_' + j).prop('checked', saved[key]);
              }
            }
          }
        }

        function syncToShiny() {
          for (var i = 1; i < n; i++) {
            for (var j = i + 1; j <= n; j++) {
              var id = 'allowed_' + i + '_' + j;
              Shiny.setInputValue(id, $('#' + id).is(':checked'));
            }
          }
        }

        // Restore saved state, then sync
        restoreState();
        setTimeout(function() {
          syncToShiny();
          // Update Allow All / Clear All checkboxes
          var all = $('.eui-interaction-cb').length;
          var checked = $('.eui-interaction-cb:checked').length;
          $('#eui_allow_all').prop('checked', checked === all);
          $('#eui_clear_all').prop('checked', checked === 0);
        }, 200);

        // On any interaction checkbox change, save and sync
        $(document).off('change.euimatrix').on('change.euimatrix', '.eui-interaction-cb', function() {
          saveState();
          syncToShiny();
          if (typeof window.euiSaveToServer === 'function') window.euiSaveToServer(%s);
        });

        // Click variable name (row or column header) to toggle all its interactions
        $(document).off('click.euivarlabel').on('click.euivarlabel', '.eui-matrix-varlabel', function() {
          var k = parseInt($(this).attr('data-var-idx'));
          if (isNaN(k)) return;
          // Collect all checkboxes involving variable k
          var cbs = [];
          for (var i = 1; i <= n; i++) {
            if (i === k) continue;
            var lo = Math.min(i, k), hi = Math.max(i, k);
            var $cb = $('#allowed_' + lo + '_' + hi);
            if ($cb.length) cbs.push($cb);
          }
          // Toggle: if all checked, uncheck all; otherwise check all
          var allChecked = cbs.every(function($cb) { return $cb.is(':checked'); });
          cbs.forEach(function($cb) { $cb.prop('checked', !allChecked); });
          // Trigger change to save and sync
          if (cbs.length > 0) cbs[0].trigger('change');
        });

        // --- Block Degree 1: right-click variable name to toggle ---
        var preds = %s;
        var blk1KeyRaw = %s;
        function getBlk1Key() {
          return window.euiPurposeKey('earthUI_blk1_', blk1KeyRaw);
        }
        var blk1 = {};
        // Restore saved block-degree-1 state
        try { var saved = JSON.parse(localStorage.getItem(getBlk1Key())); if (saved) blk1 = saved; } catch(e) {}

        function updateBlk1Labels() {
          $('.eui-matrix-varlabel').each(function() {
            var idx = parseInt($(this).attr('data-var-idx'));
            if (isNaN(idx)) return;
            var varName = preds[idx - 1];
            var indicator = $(this).find('.blk1-indicator');
            if (blk1[varName]) {
              if (indicator.length === 0) {
                $(this).append('<span class=\"blk1-indicator\" style=\"color:var(--bs-body-color, #000); font-weight:bold; font-size:0.85em;\"> 1</span>');
              }
            } else {
              indicator.remove();
            }
          });
        }

        function syncBlk1() {
          var blocked = [];
          for (var v in blk1) { if (blk1[v]) blocked.push(v); }
          Shiny.setInputValue('block_degree1', blocked.length > 0 ? blocked : null);
          try { localStorage.setItem(getBlk1Key(), JSON.stringify(blk1)); } catch(e) {}
        }

        updateBlk1Labels();
        setTimeout(syncBlk1, 250);

        $(document).off('contextmenu.euiblk1').on('contextmenu.euiblk1', '.eui-matrix-varlabel', function(e) {
          e.preventDefault();
          var idx = parseInt($(this).attr('data-var-idx'));
          if (isNaN(idx)) return;
          var varName = preds[idx - 1];
          blk1[varName] = !blk1[varName];
          updateBlk1Labels();
          syncBlk1();
          if (typeof window.euiSaveToServer === 'function') window.euiSaveToServer(%s);
        });
      })();
    ", n, jsonlite::toJSON(storage_key, auto_unbox = TRUE),
       jsonlite::toJSON(storage_key, auto_unbox = TRUE),
       jsonlite::toJSON(preds, auto_unbox = FALSE),
       jsonlite::toJSON(storage_key, auto_unbox = TRUE),
       jsonlite::toJSON(storage_key, auto_unbox = TRUE))))

    div(
      style = "max-height: 300px; overflow: auto; border: 1px solid #ddd; padding: 4px; border-radius: 4px;",
      tags$table(style = "border-collapse: collapse;",
                 tags$thead(header_row),
                 tags$tbody(body_rows)),
      js
    )
  })

  get_allowed_matrix <- reactive({
    req(input$predictors)
    preds <- input$predictors
    mat <- build_allowed_matrix(preds)

    if (length(preds) >= 2 && as.integer(input$degree) >= 2) {
      n <- length(preds)
      for (i in seq_len(n - 1)) {
        for (j in (i + 1):n) {
          id <- paste0("allowed_", i, "_", j)
          val <- input[[id]]
          if (!is.null(val) && !isTRUE(val)) {
            mat[preds[i], preds[j]] <- FALSE
            mat[preds[j], preds[i]] <- FALSE
          }
        }
      }
    }
    mat
  })

  # --- Recompute sale_age when Effective Date changes ---
  observeEvent(input$effective_date, {
    req(rv$data, input$purpose %in% c("appraisal", "market"))

    # Find contract_date column from specials
    specials <- input$col_specials
    contract_col <- NULL
    sale_age_col <- NULL
    if (!is.null(specials)) {
      for (nm in names(specials)) {
        if (specials[[nm]] == "contract_date") contract_col <- nm
        if (specials[[nm]] == "sale_age") sale_age_col <- nm
      }
    }

    # If sale_age is a user-designated column, don't overwrite it
    if (!is.null(sale_age_col)) return()

    # Only recompute if we have a contract_date column and sale_age already exists
    # (i.e., it was previously computed)
    if (is.null(contract_col)) return()
    if (!("sale_age" %in% names(rv$data))) return()

    eff_date <- as.POSIXct(as.character(input$effective_date))

    contract_vals <- rv$data[[contract_col]]
    if (inherits(contract_vals, "POSIXct")) {
      contract_posix <- contract_vals
    } else if (inherits(contract_vals, "Date")) {
      contract_posix <- as.POSIXct(contract_vals)
    } else if (is.character(contract_vals)) {
      contract_posix <- suppressWarnings(as.POSIXct(contract_vals))
      if (all(is.na(contract_posix[!is.na(contract_vals)]))) return()
    } else if (is.numeric(contract_vals)) {
      contract_posix <- as.POSIXct(as.Date(contract_vals, origin = "1899-12-30"))
    } else {
      return()
    }

    sale_age <- as.integer(difftime(eff_date, contract_posix, units = "days"))
    rv$data[["sale_age"]] <- sale_age
    showNotification(
      paste0("Recomputed sale_age for new effective date (", input$effective_date, ")."),
      type = "message", duration = 5
    )
  }, ignoreInit = TRUE)

  # --- Fit Model ---
  build_fit_args_ <- function(df = rv$data) {
    na_to_null <- function(x) if (is.na(x) || is.null(x)) NULL else x

    # Evaluate subset expression (if any) to filter rows
    subset_expr <- trimws(input$subset_arg %||% "")
    if (nzchar(subset_expr)) {
      subset_result <- tryCatch({
        expr <- parse(text = subset_expr)
        # Only allow column names from the data as variables
        mask <- as.list(df)
        mask[["TRUE"]] <- TRUE; mask[["FALSE"]] <- FALSE
        mask$as.Date <- as.Date; mask$as.POSIXct <- as.POSIXct
        rows <- eval(expr, envir = mask, enclos = emptyenv())
        if (!is.logical(rows)) stop("Expression must evaluate to TRUE/FALSE")
        rows[is.na(rows)] <- FALSE
        rows
      }, error = function(e) {
        showNotification(paste("Subset error:", e$message),
                         type = "error", duration = 10)
        NULL
      })
      if (!is.null(subset_result)) {
        n_before <- nrow(df)
        df <- df[subset_result, , drop = FALSE]
        message("earthUI: subset filter: ", sum(subset_result), " of ", n_before, " rows selected")
      }
    }

    degree <- as.integer(input$degree)

    allowed_func <- NULL
    allowed_matrix_arg <- NULL
    blk1 <- input$block_degree1
    if (degree >= 2 || length(blk1) > 0L) {
      allowed_matrix_arg <- get_allowed_matrix()
      allowed_func <- build_allowed_function(allowed_matrix_arg,
                                              block_degree1 = blk1)
    }

    glm_arg <- NULL
    if (input$glm_family != "none") {
      glm_arg <- list(family = input$glm_family)
    }

    # Weights column → numeric vector (or NULL)
    # Check special type "weight" first, then fall back to weights_col dropdown
    weights_arg <- NULL
    weight_col_name <- NULL
    if (!is.null(input$col_specials)) {
      for (nm in names(input$col_specials)) {
        if (input$col_specials[[nm]] == "weight") { weight_col_name <- nm; break }
      }
    }
    if (!is.null(weight_col_name) && weight_col_name %in% names(df)) {
      weights_arg <- df[[weight_col_name]]
    } else if (!is.null(input$weights_col) && input$weights_col != "null" &&
               input$weights_col %in% names(df)) {
      weights_arg <- df[[input$weights_col]]
    }

    # wp (response weights) — per-target numeric vector from dialog
    # Only use wp for multi-target models (earth rejects wp with Scale.y)
    wp_arg <- if (length(input$target) > 1L) rv$wp_weights else NULL

    # Collect type_map from JS dropdown state
    type_map_arg <- input$col_types  # named list from gatherState()

    list(
      df              = df,
      target          = input$target,
      predictors      = input$predictors,
      categoricals    = input$categoricals,
      linpreds        = input$linpreds,
      type_map        = type_map_arg,
      degree          = degree,
      allowed_func    = allowed_func,
      allowed_matrix  = allowed_matrix_arg,
      nfold           = na_to_null(input$nfold_override),
      nprune          = na_to_null(input$nprune),
      thresh          = na_to_null(input$thresh),
      penalty         = na_to_null(input$penalty),
      minspan         = na_to_null(input$minspan),
      endspan         = na_to_null(input$endspan),
      fast.k          = na_to_null(input$fast_k),
      pmethod         = input$pmethod,
      glm             = glm_arg,
      trace           = as.numeric(input$trace),
      nk              = na_to_null(input$nk),
      newvar.penalty  = na_to_null(input$newvar_penalty),
      fast.beta       = na_to_null(input$fast_beta),
      ncross          = na_to_null(input$ncross),
      stratify        = input$stratify,
      varmod.method   = if (length(input$target) > 1L) "none" else input$varmod_method,
      varmod.exponent = na_to_null(input$varmod_exponent),
      varmod.conv     = na_to_null(input$varmod_conv),
      varmod.clamp    = na_to_null(input$varmod_clamp),
      varmod.minspan  = na_to_null(input$varmod_minspan),
      keepxy          = input$keepxy,
      Scale.y         = input$scale_y,
      Adjust.endspan  = na_to_null(input$adjust_endspan),
      Auto.linpreds   = input$auto_linpreds,
      Force.weights   = input$force_weights,
      Use.beta.cache  = input$use_beta_cache,
      Force.xtx.prune = input$force_xtx_prune,
      Get.leverages   = input$get_leverages,
      Exhaustive.tol  = na_to_null(input$exhaustive_tol),
      wp              = wp_arg,
      weights         = weights_arg
    )
  }


  # Report assets are generated on-demand when the user clicks Download Report.
  # No eager pre-generation — saveRDS of large result objects is too slow
  # to run in the same reactive cycle as tab rendering.

  # Polling observer for background asset generation
  observe({
    req(rv_report$assets_proc)
    invalidateLater(1000)
    proc <- rv_report$assets_proc
    if (!inherits(proc, "r_process")) return()
    if (!proc$is_alive()) {
      tryCatch({
        assets_path <- proc$get_result()
        rv_report$assets_dir <- assets_path
        message("earthUI: report assets ready at ", assets_path)
      }, error = function(e) {
        message("earthUI: asset prep error: ", e$message)
        rv_report$assets_dir <- NULL
      })
      rv_report$assets_proc <- NULL
    }
  })

  observeEvent(input$run_model, {
    message("earthUI: Fit clicked. data=", !is.null(rv$data),
            " target=", paste(input$target, collapse=","),
            " predictors=", paste(input$predictors, collapse=","),
            " purpose=", input$purpose)
    req(rv$data, input$target, input$predictors)

    # --- Appraiser: round latitude/longitude to 3 decimal places ---
    if (input$purpose %in% c("appraisal", "market") && !is.null(input$col_specials)) {
      for (nm in names(input$col_specials)) {
        if (input$col_specials[[nm]] %in% c("latitude", "longitude") &&
            nm %in% names(rv$data) && is.numeric(rv$data[[nm]])) {
          rv$data[[nm]] <- round(rv$data[[nm]], 3L)
        }
      }
    }

    # --- Appraiser: compute or identify sale_age ---
    if (input$purpose %in% c("appraisal", "market")) {
      # Check if a column is already designated as sale_age
      specials <- input$col_specials
      sale_age_col <- NULL
      contract_col <- NULL
      if (!is.null(specials)) {
        for (nm in names(specials)) {
          if (specials[[nm]] == "sale_age") sale_age_col <- nm
          if (specials[[nm]] == "contract_date") contract_col <- nm
        }
      }

      # If no sale_age designated and no pre-existing numeric sale_age column,
      # compute from contract_date
      if (is.null(sale_age_col) &&
          !("sale_age" %in% names(rv$data) && is.numeric(rv$data[["sale_age"]]))) {
        if (is.null(contract_col)) {
          showNotification(
            "No 'sale_age' or 'contract_date' column designated — skipping sale_age calculation.",
            type = "message", duration = 6
          )
        } else {
        sale_age <- tryCatch(
          compute_sale_age(rv$data[[contract_col]], input$effective_date),
          error = function(e) {
            showNotification(
              paste0("Column '", contract_col, "': ", e$message),
              type = "error", duration = 10
            )
            NULL
          }
        )
        if (is.null(sale_age)) return()

        rv$data[["sale_age"]] <- sale_age
        rv$col_types[["sale_age"]] <- "integer"

        # Pre-seed sale_age as included in localStorage so it appears checked
        session$sendCustomMessage("sale_age_added", list(
          filename = rv$file_name
        ))

        showNotification(
          paste0("Added 'sale_age' column (", sum(!is.na(sale_age)),
                 " values, integer days). Click Fit again to include it."),
          type = "message", duration = 8
        )
        return()
        }
      }
    }

    # --- Skip subject row for Appraisal / Market Area Analysis ---
    skip_first <- FALSE
    if (input$purpose == "appraisal") {
      skip_first <- TRUE
    } else if (input$purpose != "appraisal" && isTRUE(input$skip_subject_row)) {
      skip_first <- TRUE
    }
    if (skip_first && nrow(rv$data) >= 2L) {
      fit_data <- rv$data[2:nrow(rv$data), , drop = FALSE]
      showNotification(
        paste0("Skipping row 1 (subject). Fitting on ", nrow(fit_data), " rows."),
        type = "message", duration = 4)
    } else {
      fit_data <- rv$data
    }

    fit_args <- build_fit_args_(df = fit_data)

    # --- Type validation before fitting ---
    if (!is.null(fit_args$type_map)) {
      validation <- validate_types(fit_data, fit_args$type_map, fit_args$predictors)

      if (length(validation$warnings) > 0L) {
        showNotification(
          paste("Type warnings:", paste(validation$warnings, collapse = "; ")),
          type = "warning", duration = 8
        )
      }

      if (!validation$ok) {
        showNotification(
          HTML(paste0("<strong>Type errors (fix before fitting):</strong><br>",
                      paste(validation$errors, collapse = "<br>"))),
          type = "error", duration = 15
        )
        return()
      }

      if (length(validation$date_columns) > 0L) {
        showNotification(
          paste("Date columns converted to numeric (days since epoch):",
                paste(validation$date_columns, collapse = ", ")),
          type = "message", duration = 6
        )
      }
    }

    # --- Random seed for reproducibility ---
    user_seed <- suppressWarnings(as.integer(input$random_seed))
    if (is.null(user_seed) || is.na(user_seed)) {
      seed <- sample.int(.Machine$integer.max, 1L)
    } else {
      seed <- user_seed
    }
    fit_args$.seed <- seed

    # Save to history (last 5 per file) and auto-fill new seed for next run.
    # Use Sys.time() + PID to generate next seed (not R's RNG which may not
    # have advanced in the async path since earth runs in a child process).
    hist <- rv_seed_history()
    hist <- c(seed, setdiff(hist, seed))
    if (length(hist) > 5L) hist <- hist[1:5]
    rv_seed_history(hist)
    next_seed <- as.integer(
      (as.numeric(Sys.time()) * 1000 + Sys.getpid()) %% .Machine$integer.max)
    updateTextInput(session, "random_seed", value = as.character(next_seed))

    use_async <- requireNamespace("callr", quietly = TRUE)

    if (use_async) {
      # --- Async path: run earth in background process ---
      fit_args$.capture_trace <- FALSE
      # Ensure trace >= 1 so the user sees progress in the fitting log
      if (is.null(fit_args$trace) || fit_args$trace < 1) {
        fit_args$trace <- 1
      }
      rv$trace_lines <- character(0)
      rv$result <- NULL
      if (!is.null(rv_report$assets_proc) &&
          inherits(rv_report$assets_proc, "r_process") &&
          rv_report$assets_proc$is_alive()) {
        rv_report$assets_proc$kill()
      }
      rv_report$assets_proc <- NULL
      if (!is.null(rv_report$assets_dir)) {
        unlink(rv_report$assets_dir, recursive = TRUE)
        rv_report$assets_dir <- NULL
      }
      rv$rca_df <- NULL
      rv$rca_targets <- NULL

      rv$bg_proc <- callr::r_bg(
        function(args) {
          seed <- args$.seed
          args$.seed <- NULL
          set.seed(seed)
          cat(sprintf("Dataset: %d obs, %d predictors, degree=%d, seed=%d\n",
                      nrow(args$df), length(args$predictors), args$degree, seed))
          if (!is.null(args$nfold) && args$nfold > 0)
            cat(sprintf("Cross-validation: %d folds\n", args$nfold))
          cat("Running forward pass...\n")
          flush(stdout())
          result <- do.call(earthUI::fit_earth, args)
          result$seed <- seed
          cat(sprintf("Completed in %.1f seconds\n", result$elapsed))
          flush(stdout())
          result
        },
        args = list(args = fit_args),
        stdout = "|", stderr = "|",
        supervise = TRUE,
        wd = tempdir()
      )
      rv$fitting <- TRUE
      eui_log_$start("5. Fit Earth Model")
      session$sendCustomMessage("fitting_start", list())

    } else {
      # --- Sync fallback (no callr) ---
      set.seed(seed)
      eui_log_$start("5. Fit Earth Model")
      session$sendCustomMessage("fitting_start", list())
      withProgress(message = "Fitting Earth model...", value = 0.2, {
        tryCatch({
          setProgress(0.3, detail = "Running forward pass")
          rv$result <- fit_earth(
            df = fit_args$df,
            target = fit_args$target,
            predictors = fit_args$predictors,
            categoricals = fit_args$categoricals,
            linpreds = fit_args$linpreds,
            degree = fit_args$degree,
            allowed_func = fit_args$allowed_func,
            allowed_matrix = fit_args$allowed_matrix,
            nfold = fit_args$nfold,
            nprune = fit_args$nprune,
            thresh = fit_args$thresh,
            penalty = fit_args$penalty,
            minspan = fit_args$minspan,
            endspan = fit_args$endspan,
            fast.k = fit_args$fast.k,
            pmethod = fit_args$pmethod,
            glm = fit_args$glm,
            trace = fit_args$trace,
            nk = fit_args$nk,
            newvar.penalty = fit_args$newvar.penalty,
            fast.beta = fit_args$fast.beta,
            ncross = fit_args$ncross,
            stratify = fit_args$stratify,
            varmod.method = fit_args$varmod.method,
            varmod.exponent = fit_args$varmod.exponent,
            varmod.conv = fit_args$varmod.conv,
            varmod.clamp = fit_args$varmod.clamp,
            varmod.minspan = fit_args$varmod.minspan,
            keepxy = fit_args$keepxy,
            Scale.y = fit_args$Scale.y,
            Adjust.endspan = fit_args$Adjust.endspan,
            Auto.linpreds = fit_args$Auto.linpreds,
            Force.weights = fit_args$Force.weights,
            Use.beta.cache = fit_args$Use.beta.cache,
            Force.xtx.prune = fit_args$Force.xtx.prune,
            Get.leverages = fit_args$Get.leverages,
            Exhaustive.tol = fit_args$Exhaustive.tol,
            wp = fit_args$wp,
            weights = fit_args$weights
          )
          rv$result$seed <- seed
          elapsed <- rv$result$elapsed
          setProgress(1, detail = "Done")
          session$sendCustomMessage("fitting_done",
            list(text = sprintf("Done in %.1fs",
                                elapsed)))
          # Defer file I/O so tab content renders first.
          # write_fit_log in onFlushed (fast).
          # auto_export_for_mgcv (saveRDS) must not block tab rendering:
          #   Unix/macOS: parallel::mcparallel() forks without serialization
          #   Windows: later::later() delays until tabs render, then blocks briefly
          # Must NOT use callr::r_bg — double-serialization corrupts the object.
          eui_log_$end("5. Fit Earth Model")
          local({
            res <- rv$result
            traces <- isolate(rv$trace_lines)
            folder <- input$output_folder
            fname <- rv$file_name
            session$onFlushed(function() {
              write_fit_log(folder, traces, fname)
              write_earth_output(res, folder, fname)
            }, once = TRUE)
            if (.Platform$OS.type == "unix") {
              parallel::mcparallel({
                auto_export_for_mgcv(res, folder, fname)
              })
            } else {
              later::later(function() {
                auto_export_for_mgcv(res, folder, fname)
              }, delay = 10)
            }
          })
        }, error = function(e) {
          eui_log_$err("5. Fit Earth Model", e$message)
          session$sendCustomMessage("fitting_done",
            list(text = "Error"))
          showNotification(paste("Model error:", e$message),
                           type = "error", duration = 10)
          write_fit_log(input$output_folder, c(paste("ERROR:", e$message)), rv$file_name)
        })
      })
    }
  })

  # --- Background process polling observer ---
  send_trace_lines_ <- function(lines, truncate_at = 0L) {
    for (line in lines) {
      if (nzchar(trimws(line))) {
        display <- if (truncate_at > 0L && nchar(line) > truncate_at) {
          paste0(substr(line, 1L, truncate_at), "...")
        } else {
          line
        }
        session$sendCustomMessage("trace_line", list(text = display))
      }
    }
  }

  observe({
    req(rv$fitting)
    invalidateLater(300)
    isolate({
      proc <- rv$bg_proc
      if (is.null(proc)) return()

      # stdout = earth trace output
      new_out <- tryCatch(proc$read_output_lines(), error = function(e) character(0))
      # stderr = messages, warnings, errors
      new_err <- tryCatch(proc$read_error_lines(), error = function(e) character(0))

      if (length(new_out) > 0L) {
        rv$trace_lines <- c(rv$trace_lines, new_out)
        send_trace_lines_(new_out)
      }
      if (length(new_err) > 0L) {
        rv$trace_lines <- c(rv$trace_lines, new_err)
        send_trace_lines_(new_err)
      }

      # Check if process has finished
      if (!proc$is_alive()) {
        # Read any remaining output
        final_out <- tryCatch(proc$read_output_lines(), error = function(e) character(0))
        final_err <- tryCatch(proc$read_error_lines(), error = function(e) character(0))
        if (length(final_out) > 0L) {
          rv$trace_lines <- c(rv$trace_lines, final_out)
          send_trace_lines_(final_out)
        }
        if (length(final_err) > 0L) {
          rv$trace_lines <- c(rv$trace_lines, final_err)
          send_trace_lines_(final_err)
        }

        tryCatch({
          result <- proc$get_result()
          # Store captured trace lines from polling
          result$trace_output <- rv$trace_lines
          rv$result <- result
          eui_log_$end("5. Fit Earth Model")
          session$sendCustomMessage("fitting_done",
            list(text = sprintf("Done in %.1fs",
                                result$elapsed)))
          # Defer file I/O until AFTER the flush so tabs appear immediately.
          # saveRDS runs in onFlushed (not callr::r_bg) to avoid double-serialization
          # which corrupts environments/formulas in the earth model object.
          local({
            res <- result
            traces <- rv$trace_lines
            folder <- input$output_folder
            fname <- rv$file_name
            session$onFlushed(function() {
              write_fit_log(folder, traces, fname)
              write_earth_output(res, folder, fname)
              auto_export_for_mgcv(res, folder, fname)
            }, once = TRUE)
          })
        }, error = function(e) {
          eui_log_$err("5. Fit Earth Model", e$message)
          # Extract the real error from callr's wrapper
          err_msg <- e$message
          if (!is.null(e$parent)) {
            err_msg <- e$parent$message
          } else {
            # Also check stderr for error details
            err_lines <- rv$trace_lines[grepl("^Error", rv$trace_lines)]
            if (length(err_lines) > 0L) {
              err_msg <- sub("^Error *:? *", "", err_lines[length(err_lines)])
            }
          }
          session$sendCustomMessage("fitting_done", list(text = "Error"))
          showNotification(paste("Model error:", err_msg),
                           type = "error", duration = 15)
          # Write log file on error
          log_lines <- c(rv$trace_lines, paste("ERROR:", err_msg))
          write_fit_log(input$output_folder, log_lines, rv$file_name)
        })

        rv$fitting <- FALSE
        rv$bg_proc <- NULL
      }
    })
  })

  # --- Abort Fitting ---
  observeEvent(input$abort_fit, {
    proc <- rv$bg_proc
    if (!is.null(proc) && inherits(proc, "r_process") && proc$is_alive()) {
      proc$kill()
      rv$trace_lines <- c(rv$trace_lines, "Aborted by user.")
      eui_log_$err("5. Fit Earth Model", "Aborted by user")
      write_fit_log(input$output_folder, rv$trace_lines, rv$file_name)
    }
    rv$fitting <- FALSE
    rv$bg_proc <- NULL
    session$sendCustomMessage("fitting_done", list(text = "Aborted by user."))
  })

  # --- Parameter Info Modal ---
  observeEvent(input$param_info, {
    showModal(modalDialog(
      title = "Earth Model Parameters",
      size = "l",
      easyClose = TRUE,
      div(style = "max-height: 70vh; overflow-y: auto; font-size: 0.9em;",
        h5("Forward Pass"),
        tags$dl(
          tags$dt("degree"), tags$dd("Maximum degree of interaction. Default 1 (additive, no interactions)."),
          tags$dt("nk"), tags$dd("Maximum number of model terms before pruning (includes intercept). Default is semi-automatically calculated."),
          tags$dt("thresh"), tags$dd("Forward stepping threshold (default 0.001). Forward pass terminates if adding a term changes RSq by less than thresh."),
          tags$dt("penalty"), tags$dd("GCV penalty per knot. Default is 3 if degree>1, else 2. Values of 0 or -1 have special meaning. Typical range: 2-4."),
          tags$dt("minspan"), tags$dd("Minimum observations between knots. Default 0 (auto-calculated). Use 1 with endspan=1 to consider all x values. Negative values specify max knots per predictor (e.g., -3 = three evenly spaced knots)."),
          tags$dt("endspan"), tags$dd("Minimum observations before first and after last knot. Default 0 (auto-calculated). Be wary of reducing this, especially for predictions near data limits."),
          tags$dt("newvar.penalty"), tags$dd("Penalty for adding a new variable (Friedman's gamma). Default 0. Non-zero values (0.01-0.2) make the model prefer reusing existing variables."),
          tags$dt("fast.k"), tags$dd("Max parent terms considered per forward step (Fast MARS). Default 20. Set 0 to disable Fast MARS. Lower = faster, higher = potentially better model."),
          tags$dt("fast.beta"), tags$dd("Fast MARS ageing coefficient. Default 1. A value of 0 sometimes gives better results."),
          tags$dt("Auto.linpreds"), tags$dd("Default TRUE. If the best knot is at the predictor minimum, add the predictor linearly (no hinge). Only affects predictions outside training data range."),
          tags$dt("linpreds"), tags$dd("Predictors that enter linearly (no hinge functions), set via the 'Linear' checkbox in Variable Configuration.")
        ),
        h5("Pruning"),
        tags$dl(
          tags$dt("pmethod"), tags$dd("Pruning method: backward (default), none, exhaustive, forward, seqrep, cv. Use 'cv' with nfold to select terms by cross-validation."),
          tags$dt("nprune"), tags$dd("Maximum terms (including intercept) in pruned model. Default NULL (all terms from forward pass, after pruning).")
        ),
        h5("Cross Validation"),
        tags$dl(
          tags$dt("nfold"), tags$dd("Number of CV folds. Default 0 (no CV). Auto-set to 10 when degree >= 2. Use trace=0.5 to trace CV."),
          tags$dt("ncross"), tags$dd("Number of cross-validations (each has nfold folds). Default 1. Use higher values (e.g., 30) with variance models."),
          tags$dt("stratify"), tags$dd("Default TRUE. Stratify CV samples so each fold has approximately equal response distribution.")
        ),
        h5("Variance Model"),
        tags$dl(
          tags$dt("varmod.method"), tags$dd("none (default), const, lm, rlm, earth, gam, power, power0, x.lm, x.rlm, x.earth, x.gam. Requires nfold and ncross. Use trace=0.3 to trace. See 'Variance models in earth' by Stephen Milborrow."),
          tags$dt("varmod.exponent"), tags$dd("Power transform for residual regression. Default 1. Use 0.5 if std dev increases with square root of response."),
          tags$dt("varmod.conv"), tags$dd("Convergence criterion (percent) for IRLS in variance model. Default 1. Negative values force that many iterations."),
          tags$dt("varmod.clamp"), tags$dd("Minimum estimated std dev = varmod.clamp * mean(sd(residuals)). Default 0.1. Prevents negative or tiny std dev estimates."),
          tags$dt("varmod.minspan"), tags$dd("minspan for internal earth call in variance model. Default -3 (three evenly spaced knots per predictor).")
        ),
        h5("GLM"),
        tags$dl(
          tags$dt("glm family"), tags$dd("Optional GLM family applied to earth basis functions. Choices: none, gaussian, binomial, poisson. Example use: binomial for binary outcomes.")
        ),
        h5("Other"),
        tags$dl(
          tags$dt("trace"), tags$dd("Trace level: 0=none, 0.3=variance model, 0.5=CV, 1=overview, 2=forward pass, 3=pruning, 4=model mats/pruning details, 5=full details."),
          tags$dt("keepxy"), tags$dd("Default FALSE. Set TRUE to retain x, y, subset, weights in the model object. Required for some cv. statistics. Makes CV slower."),
          tags$dt("Scale.y"), tags$dd("Default TRUE. Scale response internally (subtract mean, divide by sd). Provides better numeric stability."),
          tags$dt("Adjust.endspan"), tags$dd("In interaction terms, endspan is multiplied by this value. Default 2. Reduces overfitting at data boundaries."),
          tags$dt("Exhaustive.tol"), tags$dd("Default 1e-10. If reciprocal condition number of bx < this, forces pmethod='backward'. Only applies with pmethod='exhaustive'."),
          tags$dt("Use.beta.cache"), tags$dd("Default TRUE. Caches regression coefficients in forward pass for speed (20%+ faster). Uses more memory."),
          tags$dt("Force.xtx.prune"), tags$dd("Default FALSE. Force use of X'X-based subset evaluation in pruning (instead of QR-based leaps). Only for advanced use."),
          tags$dt("Get.leverages"), tags$dd("Default TRUE (unless >100k cases). Calculate diagonal hat values for linear regression of y on bx. Needed for some diagnostics."),
          tags$dt("Force.weights"), tags$dd("Default FALSE. For testing: force weighted code path even without weights.")
        )
      )
    ))
  })

  # REQUIREMENT: Tab waiting messages are handled via static HTML + JS toggling.
  # See ui.R: each tab has .eui-tab-waiting (visible by default) and
  # .eui-tab-content (hidden by default). The observers above send
  # 'eui_tabs_ready' and 'eui_rca_ready' JS messages to toggle them.
  # Do NOT use uiOutput wrappers (suspended in inactive tabs, never update).
  # Do NOT use conditionalPanel (causes all tabs to render simultaneously).

  # --- Cached computations (avoid duplicate work across tab outputs) ---
  cached_summary_ <- reactive({
    req(rv$result)
    format_summary(rv$result)
  })
  cached_equation_ <- reactive({
    req(rv$result)
    format_model_equation(rv$result)
  })
  cached_g_functions_ <- reactive({
    req(rv$result)
    list_g_functions(rv$result)
  })
  cached_importance_ <- reactive({
    req(rv$result)
    format_variable_importance(rv$result)
  })
  cached_fitted_ <- reactive({
    req(rv$result)
    stats::fitted(rv$result$model)
  })
  cached_residuals_ <- reactive({
    req(rv$result)
    stats::residuals(rv$result$model)
  })

  # --- Results: Summary ---
  output$summary_metrics <- renderUI({
    req(rv$result)
    s <- cached_summary_()

    if (isTRUE(s$multi)) {
      # Per-response metrics cards
      targets <- rv$result$target
      resp_cards <- lapply(seq_along(targets), function(i) {
        tgt <- targets[i]
        tagList(
          tags$h6(tgt, style = "margin-top: 10px; font-weight: bold;"),
          tags$div(
            class = "row", style = "margin-bottom: 10px;",
            tags$div(class = "col-md-3",
                     tags$div(class = "card text-center",
                              style = "padding: 8px;",
                              tags$h6("R\u00b2"),
                              tags$h4(sprintf("%.4f", s$r_squared[i])))),
            tags$div(class = "col-md-3",
                     tags$div(class = "card text-center",
                              style = "padding: 8px;",
                              tags$h6("GRSq"),
                              tags$h4(sprintf("%.4f", s$grsq[i])))),
            tags$div(class = "col-md-3",
                     tags$div(class = "card text-center",
                              style = "padding: 8px;",
                              tags$h6("CV R\u00b2"),
                              tags$h4(if (!is.na(s$cv_rsq[i])) sprintf("%.4f", s$cv_rsq[i]) else "N/A"))),
            tags$div(class = "col-md-3",
                     tags$div(class = "card text-center",
                              style = "padding: 8px;",
                              tags$h6("Terms"),
                              tags$h4(s$n_terms)))
          )
        )
      })

      metrics <- tagList(resp_cards)

      # CV info (per-response)
      if (rv$result$cv_enabled && !all(is.na(s$cv_rsq))) {
        cv_lines <- vapply(seq_along(targets), function(i) {
          if (!is.na(s$cv_rsq[i])) {
            sprintf("%s: CV R\u00b2 = %.4f | Training R\u00b2 = %.4f",
                    targets[i], s$cv_rsq[i], s$r_squared[i])
          } else {
            ""
          }
        }, character(1))
        cv_lines <- cv_lines[nzchar(cv_lines)]
        if (length(cv_lines) > 0L) {
          metrics <- tagList(
            metrics,
            tags$div(
              class = "alert alert-info",
              style = "font-size: 0.9em;",
              HTML(paste(cv_lines, collapse = "<br>"))
            )
          )
        }
      }
    } else {
      # Single-response metrics
      metrics <- tagList(
        tags$div(
          class = "row",
          style = "margin-bottom: 15px;",
          tags$div(class = "col-md-3",
                   tags$div(class = "card text-center",
                            style = "padding: 10px;",
                            tags$h6("R\u00b2"), tags$h4(sprintf("%.4f", s$r_squared)))),
          tags$div(class = "col-md-3",
                   tags$div(class = "card text-center",
                            style = "padding: 10px;",
                            tags$h6("GRSq"), tags$h4(sprintf("%.4f", s$grsq)))),
          tags$div(class = "col-md-3",
                   tags$div(class = "card text-center",
                            style = "padding: 10px;",
                            tags$h6("CV R\u00b2"), tags$h4(if (!is.na(s$cv_rsq)) sprintf("%.4f", s$cv_rsq) else "N/A"))),
          tags$div(class = "col-md-3",
                   tags$div(class = "card text-center",
                            style = "padding: 10px;",
                            tags$h6("Terms"), tags$h4(s$n_terms)))
        )
      )

      if (rv$result$cv_enabled && !is.na(s$cv_rsq)) {
        metrics <- tagList(
          metrics,
          tags$div(
            class = "alert alert-info",
            style = "font-size: 0.9em;",
            sprintf("Cross-validated R\u00b2: %.4f  |  Training R\u00b2: %.4f",
                    s$cv_rsq, s$r_squared),
            if (s$r_squared - s$cv_rsq > 0.1) {
              tags$span(style = "color: red; font-weight: bold;",
                        " \u26a0 Possible overfitting detected")
            }
          )
        )
      }
    }

    metrics
  })

  # --- Results: Model Equation ---
  output$model_equation <- renderUI({
    req(rv$result)
    eq <- cached_equation_()
    if (inherits(eq, "earthUI_equation_multi")) {
      # Show one equation per response with a heading
      eq_blocks <- lapply(seq_along(eq$targets), function(i) {
        sub_eq <- eq$equations[[i]]
        tagList(
          tags$h5(eq$targets[i], style = "margin-top: 16px;"),
          withMathJax(HTML(sub_eq$latex_inline))
        )
      })
      do.call(tagList, eq_blocks)
    } else {
      withMathJax(HTML(eq$latex_inline))
    }
  })

  output$summary_table <- DT::renderDataTable({
    req(rv$result)
    s <- cached_summary_()
    dt <- DT::datatable(s$coefficients, options = list(pageLength = 20),
                        rownames = FALSE, class = "compact stripe")
    numeric_cols <- names(s$coefficients)[vapply(s$coefficients, is.numeric, logical(1))]
    if (length(numeric_cols) > 0) dt <- DT::formatRound(dt, numeric_cols, digits = 6)
    dt
  })

  # --- Response selector for multivariate models ---
  output$response_selector_diag <- renderUI({
    req(rv$result)
    targets <- rv$result$target
    if (length(targets) <= 1L) return(NULL)
    choices <- stats::setNames(seq_along(targets), targets)
    selectInput("diag_response", "Response variable",
                choices = choices, selected = 1L)
  })

  output$response_selector_contrib <- renderUI({
    req(rv$result)
    targets <- rv$result$target
    if (length(targets) <= 1L) return(NULL)
    choices <- stats::setNames(seq_along(targets), targets)
    selectInput("contrib_response", "Response variable",
                choices = choices, selected = 1L)
  })

  # --- Results: Variable Importance ---
  d_ <- plot_dims_("importance_plot")
  output$importance_plot <- renderPlot({
    req(rv$result)
    plot_variable_importance(rv$result)
  }, width = d_$width, height = d_$height, res = 96)

  output$importance_table <- DT::renderDataTable({
    req(rv$result)
    imp_df <- cached_importance_()
    dt <- DT::datatable(imp_df, options = list(pageLength = 20),
                        rownames = FALSE, class = "compact stripe")
    numeric_cols <- names(imp_df)[vapply(imp_df, is.numeric, logical(1))]
    if (length(numeric_cols) > 0) dt <- DT::formatRound(dt, numeric_cols, digits = 6)
    dt
  })

  # --- Results: Contribution (g-function plots) ---
  output$contrib_g_selector <- renderUI({
    req(rv$result)
    gf <- cached_g_functions_()
    if (nrow(gf) == 0L) return(p("No g-functions in model."))

    # Build display labels: "1: sq_ft_total" or "6: sq_ft_total x beds [3D]"
    display_label <- ifelse(
      gf$d >= 2L,
      gsub(" ", " \u00d7 ", gf$label),
      gf$label
    )
    labels <- paste0(gf$index, ": ", display_label,
                     ifelse(gf$d >= 2L, " [3D]", ""))
    choices <- stats::setNames(gf$index, labels)
    selectInput("contrib_g_index", "Select g-function", choices = choices)
  })

  output$contrib_plot_container <- renderUI({
    req(rv$result, input$contrib_g_index)
    gf <- cached_g_functions_()
    idx <- as.integer(input$contrib_g_index)
    if (idx < 1L || idx > nrow(gf)) return(NULL)

    if (gf$d[idx] >= 2L && requireNamespace("plotly", quietly = TRUE)) {
      tagList(
        plotly::plotlyOutput("contrib_plot_3d", height = "500px"),
        hr(),
        plotOutput("contrib_plot_contour", height = "400px")
      )
    } else {
      plotOutput("contrib_plot_2d", height = "400px")
    }
  })

  d_ <- plot_dims_("contrib_plot_2d")
  output$contrib_plot_2d <- renderPlot({
    req(rv$result, input$contrib_g_index)
    ri <- if (length(rv$result$target) > 1L && !is.null(input$contrib_response)) {
      as.integer(input$contrib_response)
    } else {
      NULL
    }
    tryCatch(
      plot_g_function(rv$result, as.integer(input$contrib_g_index),
                      response_idx = ri),
      error = function(e) {
        message("earthUI: g-function 2D plot error: ", e$message)
        plot.new()
        text(0.5, 0.5, paste("Error:", e$message), cex = 1.2)
      }
    )
  }, width = d_$width, height = d_$height, res = 96)

  if (requireNamespace("plotly", quietly = TRUE)) {
    output$contrib_plot_3d <- plotly::renderPlotly({
      req(rv$result, input$contrib_g_index)
      ri <- if (length(rv$result$target) > 1L && !is.null(input$contrib_response)) {
        as.integer(input$contrib_response)
      } else {
        NULL
      }
      tryCatch({
        p <- plot_g_function(rv$result, as.integer(input$contrib_g_index),
                             response_idx = ri)
        # If plot_g_function returned a ggplot (2D fallback from 3D), convert
        # safely — suppress geom_label warnings that plotly doesn't support.
        if (ggplot2::is.ggplot(p)) {
          tryCatch(
            plotly::ggplotly(p),
            error = function(e2) {
              message("earthUI: ggplotly conversion error: ", e2$message)
              # Strip geom_label layers and retry
              p$layers <- Filter(function(l) !inherits(l$geom, "GeomLabel"), p$layers)
              plotly::ggplotly(p)
            }
          )
        } else {
          p
        }
      },
        error = function(e) {
          message("earthUI: g-function 3D plot error: ", e$message)
          plotly::plot_ly() |>
            plotly::add_annotations(
              text = paste("3D plot error:", e$message),
              x = 0.5, y = 0.5, showarrow = FALSE,
              xref = "paper", yref = "paper"
            )
        }
      )
    })
  } else {
    message("earthUI: plotly not available, 3D plots will use 2D fallback")
  }

  d_ <- plot_dims_("contrib_plot_contour")
  output$contrib_plot_contour <- renderPlot({
    req(rv$result, input$contrib_g_index)
    ri <- if (length(rv$result$target) > 1L && !is.null(input$contrib_response)) {
      as.integer(input$contrib_response)
    } else {
      NULL
    }
    tryCatch(
      plot_g_contour(rv$result, as.integer(input$contrib_g_index),
                     response_idx = ri),
      error = function(e) {
        message("earthUI: g-function contour plot error: ", e$message)
        plot.new()
        text(0.5, 0.5, paste("Error:", e$message), cex = 1.2)
      }
    )
  }, width = d_$width, height = d_$height, res = 96)

  # --- Results: Correlation Matrix ---
  output$correlation_plot_ui <- renderUI({
    req(rv$result)
    plotOutput("correlation_plot", height = "800px", width = "800px")
  })

  d_ <- plot_dims_("correlation_plot")
  output$correlation_plot <- renderPlot({
    req(rv$result)
    tryCatch(
      plot_correlation_matrix(rv$result),
      error = function(e) {
        message("earthUI: correlation plot error: ", e$message)
        plot.new()
        text(0.5, 0.5, paste("Error:", e$message), cex = 1.2)
      }
    )
  }, width = d_$width, height = d_$height, res = 96)

  # --- Results: Diagnostics ---
  d_ <- plot_dims_("residuals_plot")
  output$residuals_plot <- renderPlot({
    req(rv$result)
    ri <- if (length(rv$result$target) > 1L && !is.null(input$diag_response)) {
      as.integer(input$diag_response)
    } else {
      NULL
    }
    plot_residuals(rv$result, response_idx = ri)
  }, width = d_$width, height = d_$height, res = 96)

  d_ <- plot_dims_("qq_plot")
  output$qq_plot <- renderPlot({
    req(rv$result)
    ri <- if (length(rv$result$target) > 1L && !is.null(input$diag_response)) {
      as.integer(input$diag_response)
    } else {
      NULL
    }
    plot_qq(rv$result, response_idx = ri)
  }, width = d_$width, height = d_$height, res = 96)

  d_ <- plot_dims_("actual_vs_predicted_plot")
  output$actual_vs_predicted_plot <- renderPlot({
    req(rv$result)
    ri <- if (length(rv$result$target) > 1L && !is.null(input$diag_response)) {
      as.integer(input$diag_response)
    } else {
      NULL
    }
    plot_actual_vs_predicted(rv$result, response_idx = ri)
  }, width = d_$width, height = d_$height, res = 96)

  # --- Results: ANOVA ---
  output$anova_table <- DT::renderDataTable({
    req(rv$result)
    anova_df <- format_anova(rv$result)
    dt <- DT::datatable(anova_df, options = list(pageLength = 20),
                        rownames = FALSE, class = "compact stripe")
    numeric_cols <- names(anova_df)[vapply(anova_df, is.numeric, logical(1))]
    if (length(numeric_cols) > 0) dt <- DT::formatRound(dt, numeric_cols, digits = 6)
    dt
  })

  # --- Results: RCA Adjustment Percentage Histograms ---
  output$rca_plots_ui <- renderUI({
    req(rv$rca_df, rv$rca_targets)
    df <- rv$rca_df
    targets <- rv$rca_targets

    # Build plot outputs for each target
    plot_tags <- list()
    for (ti in seq_along(targets)) {
      tgt <- targets[ti]
      if (ti == 1L) {
        pct_cols <- list(
          list(col = "residual_pct",  label = "Residual Adj %"),
          list(col = "net_adj_pct",   label = "Net Adj %"),
          list(col = "gross_adj_pct", label = "Gross Adj %")
        )
      } else {
        pct_cols <- list(
          list(col = paste0(tgt, "_residual_pct"),  label = paste0(tgt, " Residual Adj %")),
          list(col = paste0(tgt, "_net_adj_pct"),   label = paste0(tgt, " Net Adj %")),
          list(col = paste0(tgt, "_gross_adj_pct"), label = paste0(tgt, " Gross Adj %"))
        )
      }
      for (pc in pct_cols) {
        plot_id <- paste0("rca_hist_", gsub("[^a-zA-Z0-9]", "_", pc$col))
        local({
          col_name <- pc$col
          plot_label <- pc$label
          d_ <- plot_dims_(plot_id)
          output[[plot_id]] <- renderPlot({
            vals <- rv$rca_df[[col_name]]
            vals <- vals[!is.na(vals) & is.finite(vals)]
            if (length(vals) == 0L) return(NULL)
            pct_vals <- vals * 100
            avg_val <- mean(pct_vals)
            med_val <- stats::median(pct_vals)
            sd_val  <- stats::sd(pct_vals)
            bin_width <- 5
            rng <- range(pct_vals)
            brks <- seq(floor(rng[1] / bin_width) * bin_width,
                        ceiling(rng[2] / bin_width) * bin_width,
                        by = bin_width)
            if (length(brks) < 2L) brks <- c(brks[1], brks[1] + bin_width)
            hist_data <- graphics::hist(pct_vals, breaks = brks, plot = FALSE)
            y_max <- max(hist_data$counts) * 1.25
            graphics::par(mar = c(5, 4, 4, 2) + 0.1)
            graphics::hist(pct_vals, breaks = brks, col = "#4A90D9", border = "white",
                           main = plot_label,
                           xlab = "Percentage (%)", ylab = "Frequency",
                           las = 1, ylim = c(0, y_max))
            graphics::abline(v = avg_val, col = "#E74C3C", lwd = 2, lty = 2)
            graphics::abline(v = med_val, col = "#2ECC71", lwd = 2, lty = 2)
            graphics::legend("topright",
                             legend = c(
                               sprintf("Mean: %.2f%%", avg_val),
                               sprintf("Median: %.2f%%", med_val),
                               sprintf("Std Dev: %.2f%%", sd_val)
                             ),
                             col = c("#E74C3C", "#2ECC71", NA),
                             lwd = c(2, 2, NA), lty = c(2, 2, NA),
                             bty = "n", cex = 1.1)
          }, width = d_$width, height = d_$height, res = 96)
        })
        plot_tags <- c(plot_tags, list(
          plotOutput(plot_id, height = "350px"),
          tags$br()
        ))
      }
      if (ti < length(targets)) {
        plot_tags <- c(plot_tags, list(tags$hr()))
      }
    }
    do.call(tagList, plot_tags)
  })

  # --- Results: Earth Output ---
  output$earth_output <- renderPrint({
    req(rv$result)
    model <- rv$result$model

    cat(sprintf("== Timing: %.2f seconds ==\n", rv$result$elapsed))
    if (!is.null(rv$result$seed)) {
      cat(sprintf("== Random Seed: %d ==\n", rv$result$seed))
    }
    cat("\n")

    cat("== Model ==\n\n")
    print(model)

    cat("\n\n== Summary ==\n\n")
    print(summary(model))

    if (!is.null(model$varmod)) {
      cat("\n\n== Variance Model ==\n\n")
      print(model$varmod)
    }

    if (length(rv$result$trace_output) > 0L) {
      trace_lines <- rv$result$trace_output
      trace_lines <- trace_lines[!grepl("^Removed .* rows with miss", trace_lines)]
      trace_lines <- trace_lines[!grepl("^CV fold ", trace_lines)]
      if (length(trace_lines) > 0L) {
        cat("\n\n== Trace Log ==\n\n")
        for (line in trace_lines) {
          if (nchar(line) > 25L) {
            cat(substr(line, 1L, 25L), "...\n")
          } else {
            cat(line, "\n")
          }
        }
      }
    }
  })

  # --- Export Report (saves to output folder, async via callr) ---
  rv_report <- reactiveValues(proc = NULL, out_path = NULL, assets_dir = NULL,
                              assets_proc = NULL)

  # --- 9. Generate Quarto Report (writes .qmd + assets) ---
  observeEvent(input$generate_qmd_btn, {
    req(rv$result)
    folder <- input$output_folder
    if (is.null(folder) || !nzchar(folder)) {
      showNotification("Active project's output folder is not set.",
                       type = "error", duration = 6); return()
    }
    base <- tools::file_path_sans_ext(rv$file_name %||% "earth_report")
    eui_log_$start("9. Generate Quarto Report")
    tryCatch({
      qmd_path <- generate_quarto_report(rv$result, dest_dir = folder,
                                          base = base)
      bundle_dir <- dirname(qmd_path)
      eui_log_$end("9. Generate Quarto Report")
      showNotification(paste0("Quarto bundle written to: ", bundle_dir),
                       type = "message", duration = 8)
      # Auto-fill the Convert section's source field with the new .qmd
      updateTextInput(session, "convert_qmd_path", value = qmd_path)
      session$sendCustomMessage("download_check",
                                list(id = "generate_qmd_btn"))
    }, error = function(e) {
      eui_log_$err("9. Generate Quarto Report", e$message)
      showNotification(paste("Generate error:", e$message),
                       type = "error", duration = 12)
    })
  })

  # --- 10. Convert Quarto Report (any .qmd → HTML/Word/PDF) ---
  shinyFiles::shinyFileChoose(input, "convert_qmd_browse",
                              roots = volumes, session = session,
                              filetypes = c("qmd"))
  observeEvent(input$convert_qmd_browse, {
    sel <- shinyFiles::parseFilePaths(volumes, input$convert_qmd_browse)
    if (nrow(sel) >= 1L && nzchar(sel$datapath[1L])) {
      updateTextInput(session, "convert_qmd_path",
                      value = as.character(sel$datapath[1L]))
    }
  })

  observeEvent(input$convert_qmd_btn, {
    qmd <- trimws(input$convert_qmd_path %||% "")
    if (!nzchar(qmd) || !file.exists(qmd)) {
      showNotification("Pick a valid .qmd file first.",
                       type = "error", duration = 6); return()
    }
    fmts <- input$convert_formats
    if (length(fmts) == 0L) {
      showNotification("Select at least one output format.",
                       type = "error", duration = 6); return()
    }
    paper <- earthUI:::locale_paper_()
    eui_log_$start(paste0("10. Convert Quarto (", paste(fmts, collapse = ","), ")"))
    tryCatch({
      out_paths <- convert_quarto_file(qmd, formats = fmts,
                                        paper_size = paper)
      eui_log_$end(paste0("10. Convert Quarto (", paste(fmts, collapse = ","), ")"))
      showNotification(paste0("Wrote ", length(out_paths), " file(s):\n",
                              paste(out_paths, collapse = "\n")),
                       type = "message", duration = 12)
      session$sendCustomMessage("download_check_multi",
                                list(id = "convert_qmd_btn"))
    }, error = function(e) {
      eui_log_$err(paste0("10. Convert Quarto (", paste(fmts, collapse = ","), ")"),
                   e$message)
      showNotification(paste("Convert error:", e$message),
                       type = "error", duration = 15)
    })
  })

  # --- Legacy single-shot Download Report (kept for back-compat) ---
  observeEvent(input$export_report_btn, {
    req(rv$result)
    # Don't start a new render if one is already running
    if (!is.null(rv_report$proc)) {
      showNotification("Report rendering already in progress.",
                       type = "warning", duration = 5)
      return()
    }

    fmt <- input$export_format
    folder <- input$output_folder
    if (is.null(folder) || !nzchar(folder)) {
      folder <- path.expand("~/Downloads")
    }
    if (!dir.exists(folder)) dir.create(folder, recursive = TRUE)

    base <- tools::file_path_sans_ext(rv$file_name %||% "earth")
    out_name <- paste0(base, "_report_",
                       format(Sys.time(), "%Y%m%d_%H%M%S"), ".", fmt)
    out_path <- file.path(folder, out_name)
    rv_report$out_path <- out_path
    rv_report$fmt <- fmt

    eui_log_$start(paste0("9. Download Report (", toupper(fmt), ")"))
    session$sendCustomMessage("report_start",
      list(text = paste0("Rendering ", toupper(fmt), " report...")))

    assets <- rv_report$assets_dir
    paper <- earthUI:::locale_paper_()

    use_async <- requireNamespace("callr", quietly = TRUE)
    if (use_async) {
      rv_report$proc <- callr::r_bg(
        function(assets_dir, output_format, output_file, paper_size, result) {
          earthUI::render_report(result,
                                 output_format = output_format,
                                 output_file = output_file,
                                 paper_size = paper_size,
                                 assets_dir = assets_dir)
        },
        args = list(assets_dir = assets, output_format = fmt,
                     output_file = out_path, paper_size = paper,
                     result = rv$result),
        stdout = "|", stderr = "|",
        supervise = TRUE,
        wd = tempdir()
      )
    } else {
      # Sync fallback
      tryCatch({
        render_report(rv$result,
                      output_format = fmt,
                      output_file = out_path,
                      paper_size = paper,
                      assets_dir = assets)
        session$sendCustomMessage("download_check_multi",
                                  list(id = "export_report_btn"))
        session$sendCustomMessage("report_done",
          list(text = paste0("Report saved to: ", out_path), error = FALSE))
        eui_log_$end(paste0("9. Download Report (", toupper(rv_report$fmt), ")"))
        message("earthUI: report saved to ", out_path)
      }, error = function(e) {
        message("earthUI export error: ", e$message)
        session$sendCustomMessage("report_done",
          list(text = paste("Error:", e$message), error = TRUE))
      })
      rv_report$proc <- NULL
    }
  })

  # Polling observer for async report rendering
  observe({
    req(rv_report$proc)
    invalidateLater(500)
    proc <- rv_report$proc
    if (!inherits(proc, "r_process")) return()

    # Read stdout/stderr for progress
    out_lines <- tryCatch(proc$read_output_lines(), error = function(e) character(0))
    err_lines <- tryCatch(proc$read_error_lines(), error = function(e) character(0))
    for (line in c(out_lines, err_lines)) {
      if (nzchar(trimws(line))) {
        session$sendCustomMessage("report_line", list(text = line, error = FALSE))
      }
    }

    if (!proc$is_alive()) {
      # Collect any remaining output
      final_out <- tryCatch(proc$read_output_lines(), error = function(e) character(0))
      final_err <- tryCatch(proc$read_error_lines(), error = function(e) character(0))
      for (line in c(final_out, final_err)) {
        if (nzchar(trimws(line))) {
          session$sendCustomMessage("report_line", list(text = line, error = FALSE))
        }
      }

      tryCatch({
        proc$get_result()
        out_path <- rv_report$out_path
        if (file.exists(out_path)) {
          session$sendCustomMessage("download_check",
                                    list(id = "export_report_btn"))
          session$sendCustomMessage("report_done",
            list(text = paste0("Report saved to: ", out_path), error = FALSE))
          eui_log_$end(paste0("9. Download Report (", toupper(rv_report$fmt), ")"))
        message("earthUI: report saved to ", out_path)
        } else {
          session$sendCustomMessage("report_done",
            list(text = "Error: output file not found", error = TRUE))
        }
      }, error = function(e) {
        err_msg <- if (!is.null(e$parent)) e$parent$message else e$message
        message("earthUI export error: ", err_msg)
        session$sendCustomMessage("report_done",
          list(text = paste("Error:", err_msg), error = TRUE))
      })
      rv_report$proc <- NULL
    }
  })

  # --- 8. Generate Sales Grid & Download ---
  # Step 1: Button click shows modal with recommended comps
  observeEvent(input$sales_grid_btn, {
    req(rv$rca_df)
    rca <- rv$rca_df
    n_total <- nrow(rca)
    if (n_total < 2) {
      showNotification("Need at least 2 rows (subject + 1 comp).",
                       type = "error", duration = 8)
      return()
    }

    # Find the sale_age column from specials or default to "sale_age"
    sg_specials <- input$col_specials
    sa_col_name <- "sale_age"
    if (!is.null(sg_specials)) {
      for (nm in names(sg_specials)) {
        if (sg_specials[[nm]] == "sale_age") { sa_col_name <- nm; break }
      }
    }

    sel <- select_sales_grid_comps(rca, sale_age_col = sa_col_name)
    recommended <- sel$recommended
    others      <- sel$others

    # Store for the confirm handler
    rv$sg_recommended <- recommended
    rv$sg_others <- others

    # Build modal UI
    rec_checks <- if (nrow(recommended) > 0) {
      lapply(seq_len(nrow(recommended)), function(i) {
        r <- recommended[i, ]
        lbl <- sprintf("Row %d | %s | SP: $%s | Age: %s | Gross: %.1f%%",
                       r$row,
                       substr(as.character(r$address), 1, 30),
                       formatC(r$sale_price, format = "f", digits = 0,
                               big.mark = ","),
                       as.character(r$sale_age),
                       r$gross_adj_pct * 100)
        tags$div(
          checkboxInput(paste0("sg_rec_", r$row), lbl, value = TRUE),
          style = "margin-bottom: 0px;"
        )
      })
    } else {
      tags$p("No comps with gross adjustment < 25% found.",
             style = "color: var(--bs-secondary-color);")
    }

    other_checks <- if (nrow(others) > 0) {
      lapply(seq_len(min(nrow(others), 50)), function(i) {
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
        tags$div(
          checkboxInput(paste0("sg_rec_", r$row), lbl, value = FALSE),
          style = "margin-bottom: 0px;"
        )
      })
    } else NULL

    showModal(modalDialog(
      title = "Sales Grid — Select Comparables (max 30)",
      size = "l",
      tags$div(
        style = "max-height: 500px; overflow-y: auto;",
        tags$h5(paste0("Recommended Comps (gross adj < 25%, ",
                       "sorted by sale age) — ",
                       nrow(recommended), " found")),
        rec_checks,
        if (!is.null(other_checks)) {
          tagList(
            hr(),
            tags$h5("Additional Comps (gross adj >= 25%)"),
            other_checks
          )
        }
      ),
      footer = tagList(
        modalButton("Cancel"),
        actionButton("sg_confirm", "Generate Sales Grid",
                     class = "btn-primary")
      )
    ))
  })

  # Step 2: Confirm button in modal — generate the grid

  observeEvent(input$sg_confirm, {
    req(rv$rca_df)
    removeModal()

    # Collect checked rows from both recommended and others
    all_candidate_rows <- c(
      if (!is.null(rv$sg_recommended) && nrow(rv$sg_recommended) > 0)
        rv$sg_recommended$row else integer(0),
      if (!is.null(rv$sg_others) && nrow(rv$sg_others) > 0)
        rv$sg_others$row[seq_len(min(nrow(rv$sg_others), 50))] else integer(0)
    )
    comp_rows <- integer(0)
    for (r in all_candidate_rows) {
      cb_val <- input[[paste0("sg_rec_", r)]]
      if (!is.null(cb_val) && isTRUE(cb_val)) {
        comp_rows <- c(comp_rows, r)
      }
    }

    if (length(comp_rows) == 0) {
      showNotification("No comps selected.", type = "warning", duration = 8)
      return()
    }
    if (length(comp_rows) > 30) {
      comp_rows <- comp_rows[1:30]
      showNotification("Capped at 30 comps.", type = "warning", duration = 5)
    }

    # Sort selected comps by gross_adj_pct ascending
    rca <- rv$rca_df
    sp <- if ("sale_price" %in% colnames(rca)) rca[["sale_price"]][comp_rows] else rep(NA, length(comp_rows))
    gross <- if ("gross_adjustments" %in% colnames(rca)) rca[["gross_adjustments"]][comp_rows] else rep(0, length(comp_rows))
    gap <- ifelse(!is.na(sp) & sp != 0, abs(gross / sp), NA)
    comp_rows <- comp_rows[order(gap, na.last = TRUE)]

    folder <- input$output_folder
    if (is.null(folder) || !nzchar(folder)) folder <- path.expand("~/Downloads")
    if (!dir.exists(folder)) dir.create(folder, recursive = TRUE)

    out_path <- file.path(folder, paste0("SalesGrid_",
                          format(Sys.time(), "%Y%m%d_%H%M%S"), ".xlsx"))

    message("earthUI: Sales grid with ", length(comp_rows),
            " comps (rows: ", paste(comp_rows, collapse = ","), ")")

    tryCatch({
      sg_specials_map <- list()
      sg_input <- input$col_specials
      if (!is.null(sg_input)) {
        for (nm in names(sg_input)) {
          sp_type <- sg_input[[nm]]
          if (sp_type != "no") sg_specials_map[[sp_type]] <- nm
        }
      }

      n_comp <- length(comp_rows)
      n_sheet <- ceiling(n_comp / 3)
      eui_log_$start("8. Generate Sales Grid & Download")
      withProgress(
        message = "Generating Sales Grid",
        detail = sprintf("0 of %d comps processed", n_comp),
        value = 0, {
        build_sales_grid(
          rca_df       = rv$rca_df,
          comp_rows    = comp_rows,
          output_file  = out_path,
          specials     = sg_specials_map,
          progress_fn  = function(sheet, total_sheets, comps_done, total_comps) {
            setProgress(
              value = comps_done / total_comps,
              detail = sprintf("Sheet %d of %d — %d of %d comps processed",
                               sheet, total_sheets, comps_done, total_comps))
          }
        )
      })

      eui_log_$end("8. Generate Sales Grid & Download")
      showNotification(paste0("Sales grid saved to: ", out_path,
                              " (", length(comp_rows), " comps, ",
                              ceiling(length(comp_rows) / 3), " sheets)"),
                       type = "message", duration = 10)
      session$sendCustomMessage("download_check",
        list(id = "sales_grid_btn"))
    }, error = function(e) {
      showNotification(paste("Sales grid error:", e$message),
                       type = "error", duration = 10)
    })
  })

  # --- Export Data (Excel) ---
  export_data_filename_ <- function() {
    base <- tools::file_path_sans_ext(rv$file_name %||% "data")
    paste0(base, "_modified_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".xlsx")
  }

  observeEvent(input$export_data, {
    req(rv$data)
    folder <- input$output_folder
    if (is.null(folder) || !nzchar(folder)) folder <- path.expand("~/Downloads")
    if (!dir.exists(folder)) dir.create(folder, recursive = TRUE)
    out_path <- file.path(folder, export_data_filename_())
    message("earthUI: export_data clicked, writing to ", out_path)
    eui_log_$start("6. Download Intermediate Output")
    export_data_content_(out_path, "export_data")
    eui_log_$end("6. Download Intermediate Output")
    if (file.exists(out_path)) {
      showNotification(paste0("Output saved to: ", out_path),
                       type = "message", duration = 8)
    }
  })

  observeEvent(input$export_data_nonadj, {
    req(rv$data)
    folder <- input$output_folder
    if (is.null(folder) || !nzchar(folder)) folder <- path.expand("~/Downloads")
    if (!dir.exists(folder)) dir.create(folder, recursive = TRUE)
    out_path <- file.path(folder, export_data_filename_())
    export_data_content_(out_path, "export_data_nonadj")
    showNotification(paste0("Output saved to: ", out_path),
                     type = "message", duration = 8)
  })

  export_data_content_ <- function(file, btn_id = "export_data") {
    tryCatch({
      req(rv$data)
      if (!requireNamespace("writexl", quietly = TRUE)) {
        showNotification(
          "Package 'writexl' required. Install with: install.packages('writexl')",
          type = "error", duration = 10)
        return()
      }

      # Find living_area column from specials
      la_col <- NULL
      specials <- input$col_specials
      if (!is.null(specials)) {
        for (nm in names(specials)) {
          if (specials[[nm]] == "living_area" && nm %in% names(rv$data)) {
            la_col <- nm
            break
          }
        }
      }

      export_df <- compute_intermediate_output(
        data             = rv$data,
        result           = rv$result,
        purpose          = input$purpose,
        skip_subject_row = isTRUE(input$skip_subject_row),
        living_area_col  = la_col
      )

      # Write with openxlsx for cell formatting
      wb <- openxlsx::createWorkbook()
      openxlsx::addWorksheet(wb, "Data")
      openxlsx::writeData(wb, "Data", export_df)
      # Apply number formats to specific columns
      col_names <- names(export_df)
      fmt_map <- list(
        residual_sf = "#,##0.00",
        cqa_sf      = "0.00",
        residual    = "#,##0",
        cqa         = "0.00"
      )
      for (cn in names(fmt_map)) {
        ci <- match(cn, col_names)
        if (!is.na(ci)) {
          openxlsx::addStyle(wb, "Data",
            style = openxlsx::createStyle(numFmt = fmt_map[[cn]]),
            rows = 2:(nrow(export_df) + 1L), cols = ci,
            gridExpand = TRUE, stack = TRUE)
        }
      }
      openxlsx::saveWorkbook(wb, file, overwrite = TRUE)
      session$sendCustomMessage("download_check", list(id = btn_id))
    }, error = function(e) {
      msg <- paste("Download error:", conditionMessage(e))
      message(msg)  # to R console
      showNotification(msg, type = "error", duration = 15)
    })
  }

  # --- RCA Raw Output ---

  # Show modal when button is clicked
  observeEvent(input$rca_output_btn, {
    req(rv$result, input$purpose == "appraisal")
    eui_log_$start("7. Calculate RCA Adjustments & Download")

    # Check if living_area is designated (for CQA_SF option)
    has_la <- FALSE
    specials <- input$col_specials
    if (!is.null(specials)) {
      for (nm in names(specials)) {
        if (specials[[nm]] == "living_area" && nm %in% names(rv$data)) {
          has_la <- TRUE
          break
        }
      }
    }

    cqa_choices <- c("CQA" = "cqa")
    if (has_la) cqa_choices <- c(cqa_choices, "CQA per SF" = "cqa_sf")

    showModal(modalDialog(
      title = "RCA Raw Output — Subject CQA Score",
      radioButtons("rca_cqa_type", "Score type:",
                   choices = cqa_choices, selected = "cqa", inline = TRUE),
      textInput("rca_cqa_value", "CQA score for subject (0.00\u20139.99):",
                value = "5.00", placeholder = "e.g. 5.00"),
      tags$script(HTML("
        $(document).off('click.eui_rca').on('click.eui_rca', '#export_rca', function() {
          Shiny.setInputValue('export_rca_trigger', Math.random());
        });
      ")),
      footer = tagList(
        modalButton("Cancel"),
        tags$button(id = "export_rca", type = "button",
                    class = "btn btn-primary action-button",
                    "Generate")
      ),
      size = "s"
    ))
  })

  # RCA download handler — triggered via JS setInputValue for reliable
  # cross-platform firing (actionButton in modals can fail on Windows)
  observeEvent(input$export_rca_trigger, ignoreInit = TRUE, ignoreNULL = TRUE, {
      message("earthUI RCA: Generate button clicked, purpose=", input$purpose,
              ", data=", !is.null(rv$data), ", result=", !is.null(rv$result))
      req(rv$data, rv$result, input$purpose == "appraisal", nrow(rv$data) >= 2L)
      removeModal()

      folder <- input$output_folder
      if (is.null(folder) || !nzchar(folder)) folder <- path.expand("~/Downloads")
      if (!dir.exists(folder)) dir.create(folder, recursive = TRUE)
      base <- tools::file_path_sans_ext(rv$file_name %||% "data")
      file <- file.path(folder, paste0(base, "_adjusted_",
                                       format(Sys.time(), "%Y%m%d_%H%M%S"), ".xlsx"))

      if (!requireNamespace("writexl", quietly = TRUE)) {
        showNotification("Package 'writexl' required.", type = "error", duration = 10)
        return()
      }

    tryCatch({

      # Find living_area column
      la_col <- NULL
      specials <- input$col_specials
      if (!is.null(specials)) {
        for (nm in names(specials)) {
          if (specials[[nm]] == "living_area" && nm %in% names(rv$data)) {
            la_col <- nm
            break
          }
        }
      }

      # Resolve weight column: special type "weight" first, then weights_col
      rca_wt_col <- NULL
      if (!is.null(input$col_specials)) {
        for (nm in names(input$col_specials)) {
          if (input$col_specials[[nm]] == "weight") { rca_wt_col <- nm; break }
        }
      }
      if (is.null(rca_wt_col) && !is.null(input$weights_col) &&
          input$weights_col != "null") {
        rca_wt_col <- input$weights_col
      }

      export_df <- compute_rca_adjustments(
        data            = rv$data,
        result          = rv$result,
        user_cqa        = input$rca_cqa_value,
        cqa_type        = input$rca_cqa_type,
        living_area_col = la_col,
        weight_col      = rca_wt_col
      )

      writexl::write_xlsx(export_df, file)
      rv$rca_df <- export_df
      rv$rca_targets <- rv$result$target
      session$sendCustomMessage("download_check", list(id = "rca_output_btn"))
      eui_log_$end("7. Calculate RCA Adjustments & Download")
      showNotification(paste0("RCA output saved to: ", file),
                       type = "message", duration = 8)
    }, error = function(e) {
      msg <- paste("RCA export error:", conditionMessage(e))
      message(msg)
      showNotification(msg, type = "error", duration = 15)
    })
  })

}
