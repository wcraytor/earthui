library(earthUI)

# Allow uploads up to 3 GB
options(shiny.maxRequestSize = 3 * 1024^3)

# Load Roboto Condensed for R graphics (ggplot2 + base R)
# Wrapped in tryCatch: font_add_google needs network access and can fail
# on offline machines, firewalled servers, or minimal Docker containers.
font_loaded <- tryCatch({
  if (requireNamespace("sysfonts", quietly = TRUE) &&
      requireNamespace("showtext", quietly = TRUE)) {
    sysfonts::font_add_google("Roboto Condensed", "Roboto Condensed")
    showtext::showtext_auto()
    TRUE
  } else {
    FALSE
  }
}, error = function(e) {
  message("mgcvUI: could not load Roboto Condensed font: ", e$message,
          ". Using system sans-serif.")
  FALSE
})
if (font_loaded) {
  ggplot2::theme_set(ggplot2::theme_minimal(base_family = "Roboto Condensed"))
} else {
  ggplot2::theme_set(ggplot2::theme_minimal(base_family = "sans"))
}

# Enable thematic so ggplot2 auto-adapts to current theme
if (requireNamespace("thematic", quietly = TRUE)) {
  thematic::thematic_shiny()
}

# Font family helper
mgcv_font_family_ <- function() {
  if (requireNamespace("sysfonts", quietly = TRUE) &&
      "Roboto Condensed" %in% sysfonts::font_families()) {
    "Roboto Condensed"
  } else {
    "sans"
  }
}

# Nord Light theme
nord_light <- bslib::bs_theme(
  version = 5,
  bg = "#eceff4",
  fg = "#2e3440",

  primary = "#5e81ac",
  secondary = "#81a1c1",
  success = "#a3be8c",
  info = "#88c0d0",
  warning = "#ebcb8b",
  danger = "#bf616a",
  base_font = bslib::font_google("Roboto Condensed")
)

# Nord Dark theme
nord_dark <- bslib::bs_theme(
  version = 5,

  bg = "#2e3440",
  fg = "#d8dee9",
  primary = "#88c0d0",
  secondary = "#81a1c1",
  success = "#a3be8c",
  info = "#5e81ac",
  warning = "#ebcb8b",
  danger = "#bf616a",
  base_font = bslib::font_google("Roboto Condensed")
) |>
  bslib::bs_add_rules("
    .navbar { background-color: #242933 !important; }
    .card   { background-color: #3b4252 !important; border-color: #434c5e; }
  ")

ui <- fluidPage(
  theme = nord_light,
  withMathJax(),
  tags$head(
    tags$link(rel = "icon", type = "image/png", href = "favicon.png"),
    # Declare a color-scheme so Chrome's "Auto Dark Mode for Web Contents"
    # stops force-darkening this app (it leaves pages that declare a scheme
    # alone). The app manages its own light/dark theming.
    tags$meta(name = "color-scheme", content = "light dark"),
    tags$style(HTML("
    /* color-scheme follows the app's own theme so native form controls match.
       The `only` keyword forbids the user agent from overriding the page's
       scheme, which is the strongest opt-out from Chrome's Auto Dark Mode
       (chrome://flags/#enable-force-dark) force-darkening the page. */
    :root, body[data-mgcv-theme='light'] { color-scheme: only light; }
    body[data-mgcv-theme='dark'] { color-scheme: only dark; }

    /* --- Full-width container (match earthUI) --- */
    .container-fluid { max-width: 100% !important; padding: 0 15px; }

    /* Sidebar width matched to earthUI (one-third column, 500px floor). */
    .col-sm-4 { min-width: 500px; }
    /* Minimum app width so the results tabs are always visible (they need
       ~1520px; a horizontal scrollbar appears on narrower screens). */
    body { min-width: 1600px; }

    /* --- Variable table --- */
    .mgcv-var-row {
      display: flex; align-items: center;
      padding: 3px 0; border-bottom: 1px solid var(--bs-border-color);
    }
    .mgcv-var-header {
      font-weight: bold; border-bottom: 2px solid var(--bs-border-color);
      background: var(--bs-tertiary-bg); padding: 6px 0;
      position: sticky; top: 0; z-index: 2;
    }
    .mgcv-cell-name   { flex: 1; min-width: 80px; overflow: hidden;
                        text-overflow: ellipsis; white-space: nowrap;
                        padding: 0 6px;
                        font-family: 'Roboto Condensed', 'Arial Narrow', Helvetica, Arial, sans-serif;
                        font-size: 0.85em;
                        border: 1px solid var(--bs-border-color); border-radius: 3px;
                        margin-right: 2px; }
    .mgcv-cell-type   { width: 85px; }
    .mgcv-cell-inc    { width: 20px; text-align: center; }
    .mgcv-cell-factor { width: 20px; text-align: center; }
    .mgcv-cell-linear { width: 20px; text-align: center; }
    .mgcv-cell-special { width: 110px; }
    .mgcv-cell-na     { width: 75px; text-align: right; padding: 0 4px;
                        font-size: 0.75em; white-space: nowrap;
                        font-family: 'Roboto Condensed', 'Arial Narrow', Helvetica, Arial, sans-serif;
                        border: 1px solid var(--bs-border-color); border-radius: 3px;
                        margin-left: 2px; }
    .mgcv-var-row select {
      width: 100%; padding: 1px 2px; font-size: 0.8em;
      border: 1px solid var(--bs-border-color); border-radius: 3px;
      appearance: auto; -webkit-appearance: auto;
      background-color: var(--bs-body-bg); color: var(--bs-body-color);
    }
    .mgcv-var-header .mgcv-cell-name,
    .mgcv-var-header .mgcv-cell-na { border-color: transparent; }
    .mgcv-var-row input[type='checkbox'] {
      width: 15px; height: 15px; cursor: pointer;
    }

    /* --- DataTables --- */
    .dataTable td, .dataTable th { padding: 4px 8px !important; }
    .dataTables_wrapper { font-size: 0.9em; overflow-x: auto; }
    /* Keep rows one line tall: cap column width, clip with an ellipsis.
       Full text is available by double-clicking a cell (see cell_popup_js). */
    .dataTable td { max-width: 40ch; overflow: hidden;
                    text-overflow: ellipsis; white-space: nowrap;
                    cursor: pointer; }

    /* Full-cell-text popup (opened on double-click) */
    #mgcv-cell-popup { display: none; }
    .mgcv-popup-backdrop { position: fixed; top: 0; left: 0; width: 100%;
                           height: 100%; background: rgba(0,0,0,0.3);
                           z-index: 9998; }
    .mgcv-popup-content { position: fixed; top: 50%; left: 50%;
                          transform: translate(-50%, -50%);
                          background: var(--bs-body-bg, #eceff4);
                          color: var(--bs-body-color, #2e3440);
                          border-radius: 8px; padding: 20px; max-width: 80vw;
                          max-height: 70vh; overflow: auto; z-index: 9999;
                          box-shadow: 0 4px 20px rgba(0,0,0,0.3); }
    .mgcv-popup-content pre { white-space: pre-wrap; word-wrap: break-word;
                              margin-bottom: 12px; font-size: 0.9em; }
    .mgcv-popup-close { float: right; }
    [data-mgcv-theme='dark'] .mgcv-popup-content {
      background: #3b4252; color: #d8dee9; }

    /* DT DataTables: adapt to current theme */
    .dataTables_wrapper { color: var(--bs-body-color); }
    table.dataTable { color: var(--bs-body-color); border-color: var(--bs-border-color); }
    table.dataTable thead th,
    table.dataTable thead td {
      background-color: var(--bs-tertiary-bg) !important;
      color: var(--bs-body-color) !important;
      border-color: var(--bs-border-color) !important;
    }
    table.dataTable tbody td {
      background-color: var(--bs-body-bg);
      color: var(--bs-body-color);
      border-color: var(--bs-border-color);
    }
    table.dataTable tbody tr:hover td { background-color: var(--bs-tertiary-bg); }
    table.dataTable tbody tr.odd td { background-color: var(--bs-secondary-bg); }
    .dataTables_info, .dataTables_length, .dataTables_filter, .dataTables_paginate {
      color: var(--bs-body-color);
    }
    .dataTables_paginate .paginate_button {
      color: var(--bs-body-color) !important;
      background: var(--bs-body-bg); border-color: var(--bs-border-color);
    }
    .dataTables_paginate .paginate_button.current {
      color: var(--bs-body-color) !important;
      background: var(--bs-tertiary-bg) !important;
      border-color: var(--bs-border-color) !important;
    }
    .dataTables_filter input, .dataTables_length select {
      background-color: var(--bs-body-bg);
      color: var(--bs-body-color);
      border-color: var(--bs-border-color);
    }

    /* --- Collapsible sections --- */
    .mgcv-section > summary { cursor: pointer; list-style: none; position: relative; padding-right: 28px; }
    .mgcv-section > summary::-webkit-details-marker { display: none; }
    .mgcv-section-info {
      position: absolute; right: 0; top: 50%; transform: translateY(-50%);
      display: inline-flex; align-items: center; justify-content: center;
      width: 18px; height: 18px; border-radius: 50%;
      background: #88c0d0; color: #fff;
      font-size: 11px; font-weight: bold; line-height: 18px;
      cursor: pointer;
    }
    .mgcv-section-info:hover { background: #5e81ac; }
    .mgcv-section > summary h4::before {
      content: '\\25B6\\00a0\\00a0\\00a0';
      font-size: 0.7em;
      transition: transform 0.2s;
      display: inline-block;
    }
    .mgcv-section[open] > summary h4::before { transform: rotate(90deg); }

    /* --- Interaction matrix: earth-sourced cells --- */
    .mgcv-int-earth-cell {
      background-color: rgba(235, 203, 139, 0.35);
    }
    .mgcv-int-earth {
      cursor: not-allowed;
      opacity: 0.8;
    }
    [data-mgcv-theme='dark'] .mgcv-int-earth-cell {
      background-color: rgba(235, 203, 139, 0.2);
    }

    /* --- Top navbar --- */
    .mgcv-navbar { display: flex; align-items: center; padding: 10px 15px; gap: 8px; flex-wrap: wrap; }
    .mgcv-navbar .mgcv-brand { font-size: 1.3em; font-weight: bold; margin-right: 8px; white-space: nowrap; margin: 0; }
    .mgcv-navbar .mgcv-brand small { font-size: 0.55em; color: var(--bs-secondary-color); font-weight: normal; }
    .mgcv-navbar .mgcv-brand img { height: 26px; margin-right: 6px; vertical-align: middle; }
    .mgcv-navbar .dropdown { position: relative; }
    .mgcv-navbar .mgcv-menu-btn { background: none; border: 1px solid var(--bs-border-color); color: var(--bs-body-color); font-size: 0.9em; padding: 6px 12px; cursor: pointer; border-radius: 4px; }
    .mgcv-navbar .mgcv-menu-btn:hover { background: var(--bs-tertiary-bg); }
    .mgcv-navbar .mgcv-dropdown-menu { display: none; position: absolute; top: 100%; left: 0; background: var(--bs-body-bg); border: 1px solid var(--bs-border-color); border-radius: 6px; padding: 12px 16px; min-width: 280px; z-index: 10001; box-shadow: 0 4px 16px rgba(0,0,0,0.2); }
    .mgcv-navbar .dropdown.open .mgcv-dropdown-menu { display: block; }
    /* File/dir chooser modals must sit ABOVE the open Settings dropdown
       (z-index 10001) so their folder list and Select button are clickable. */
    .modal { z-index: 20002 !important; }
    .modal-backdrop { z-index: 20001 !important; }
    .mgcv-navbar .mgcv-spacer { flex: 1; }
    #mgcv-theme-toggle {
      width: 38px; height: 38px; border-radius: 50%;
      border: 2px solid var(--bs-border-color);
      background: var(--bs-body-bg); color: var(--bs-body-color);
      font-size: 18px; cursor: pointer;
      display: flex; align-items: center; justify-content: center;
      box-shadow: 0 2px 6px rgba(0,0,0,0.15); transition: all 0.3s;
    }
    #mgcv-theme-toggle:hover { box-shadow: 0 2px 10px rgba(0,0,0,0.25); }

    /* --- Dark mode --- */
    [data-mgcv-theme='dark'] .mgcv-var-header { background: #3b4252; }
    [data-mgcv-theme='dark'] .mgcv-var-row { border-color: #434c5e; }
    [data-mgcv-theme='dark'] .mgcv-var-row select {
      background: #3b4252 !important; color: #d8dee9 !important;
      border-color: #434c5e !important;
    }
    [data-mgcv-theme='dark'] details > summary { color: #d8dee9 !important; }
    [data-mgcv-theme='dark'] .nav-tabs .nav-link.active {
      color: #d8dee9 !important; background-color: #2e3440 !important;
      border-color: #434c5e #434c5e #2e3440 !important;
    }
    [data-mgcv-theme='dark'] .nav-tabs .nav-link { color: #81a1c1; }
    [data-mgcv-theme='dark'] .nav-tabs .nav-link:hover {
      color: #d8dee9; border-color: #434c5e;
    }

    /* --- DataTables (DT) row colours follow the active bslib theme --- */
    /* DT paints the row colour on the CELLS (inset box-shadow), so we set the
       cell background and clear the shadow. Using bslib theme variables makes
       this correct in BOTH light and dark mode automatically (bslib swaps the
       variables when the theme changes). Selected rows are left untouched so
       DT's blue highlight still works. */
    table.dataTable > tbody > tr:not(.selected) > * {
      background-color: var(--bs-body-bg, #fff) !important;
      box-shadow: none !important;
      color: var(--bs-body-color, #2e3440) !important;
    }
    /* Zebra + hover as a NEUTRAL translucent overlay (not the themed
       --bs-secondary-bg, which is tinted by the Frost `secondary` colour and
       made even rows look like bright bars). Neutral gray darkens slightly in
       light mode and lightens slightly in dark mode — subtle in both. */
    table.dataTable.stripe > tbody > tr.odd:not(.selected) > *,
    table.dataTable.display > tbody > tr.odd:not(.selected) > * {
      box-shadow: inset 0 0 0 9999px rgba(128, 128, 128, 0.10) !important;
    }
    table.dataTable.hover > tbody > tr:hover:not(.selected) > *,
    table.dataTable.display > tbody > tr:hover:not(.selected) > * {
      box-shadow: inset 0 0 0 9999px rgba(128, 128, 128, 0.18) !important;
    }
    table.dataTable thead th, table.dataTable thead td {
      color: var(--bs-emphasis-color, inherit) !important;
      border-color: var(--bs-border-color) !important;
    }
    table.dataTable tbody td, table.dataTable tbody th {
      border-color: var(--bs-border-color) !important;
    }

    /* --- Parameter help icons (match earthUI) --- */
    .mgcv-param-help {
      position: absolute; top: 0; right: 0;
      width: 18px; height: 18px; border-radius: 50%;
      background: #88c0d0; color: #fff;
      font-size: 11px; font-weight: bold;
      text-align: center; line-height: 18px;
      cursor: pointer; z-index: 10;
    }
    .mgcv-param-help:hover { background: #5e81ac; }

    /* --- Wrap long file paths in notifications --- */
    .shiny-notification { word-wrap: break-word; overflow-wrap: anywhere; }

  "))),

  # --- Top Menu Bar ---
  tags$nav(class = "mgcv-navbar",
    tags$h2(class = "mgcv-brand",
      tags$img(src = "logo.png"),
      "mgcvUI",
      tags$small(" - GAM Builder"),
      if (!is.null(getOption("mgcvUI.trilogy")))
        tags$span(" (Trilogy Mode)",
          style = paste0("font-size: 0.55em; font-weight: bold;",
                         " color: #5e81ac; margin-left: 8px;")),
      style = "margin: 0;"
    ),
    tags$div(class = "dropdown", id = "mgcv-settings-dropdown",
      tags$button(class = "mgcv-menu-btn",
                  onclick = "mgcvToggleDropdown('mgcv-settings-dropdown')",
                  HTML("&#9881; Settings")),
      tags$div(class = "mgcv-dropdown-menu",
        selectInput("locale_country", "Country",
                    choices = earthUI:::locale_country_choices_(),
                    selected = "us", width = "100%"),
        selectInput("locale_paper", "Paper",
                    choices = c("Letter" = "letter", "A4" = "a4"),
                    selected = "letter", width = "100%"),
        tags$hr(style = "margin: 10px 0;"),
        tags$label(class = "control-label", "regProj root folder"),
        tags$div(style = "display:flex; gap:6px; align-items:flex-end;",
          tags$div(style = "flex:1;",
            textInput("regproj_root", NULL, value = "", width = "100%")),
          shinyFiles::shinyDirButton("regproj_root_browse", "Browse…",
                                     "Choose the regProj root folder",
                                     class = "btn btn-outline-secondary btn-sm",
                                     style = "margin-bottom:0;")
        ),
        tags$p(style = "font-size: 0.75em; color: var(--bs-secondary-color); margin-top: 4px;",
               "Shared with earthUI / glmnetUI. Projects, input files, and outputs live here."),
        tags$hr(style = "margin: 10px 0;"),
        tags$div(style = "display:flex; gap:6px;",
          actionButton("settings_save", "Save",
                       class = "btn-primary btn-sm", style = "flex:1;"),
          actionButton("settings_close", "Close",
                       class = "btn-outline-secondary btn-sm", style = "flex:1;")
        ),
        tags$div(style = "margin-top: 8px; display:flex; gap:14px;",
          actionLink("show_help", HTML("&#128231; Help"), class = "small"),
          actionLink("show_about", HTML("&#9888; About &amp; Disclaimer"),
                     class = "small")
        )
      )
    ),
    tags$div(class = "mgcv-spacer"),
    tags$button(id = "mgcv-theme-toggle", onclick = "mgcvToggleTheme()",
                HTML("&#9790;"))
  ),
  tags$script(HTML("
    var mgcvCurrentMode = 'light';

    function mgcvToggleDropdown(id) {
      var el = document.getElementById(id);
      if (el) el.classList.toggle('open');
    }
    // Settings stays open until you click the gear again or press
    // \"Save and Close\" — it does NOT auto-close on outside clicks, so using
    // the Browse/Select file dialog can never dismiss it.
    // Allow the server to close the Settings dropdown (Save and Close).
    Shiny.addCustomMessageHandler('close_settings_dropdown', function(msg) {
      var el = document.getElementById('mgcv-settings-dropdown');
      if (el) el.classList.remove('open');
    });

    function mgcvToggleTheme() {
      mgcvCurrentMode = (mgcvCurrentMode === 'dark') ? 'light' : 'dark';
      document.body.setAttribute('data-mgcv-theme', mgcvCurrentMode);
      Shiny.setInputValue('dark_mode', mgcvCurrentMode, {priority: 'event'});
      var btn = document.getElementById('mgcv-theme-toggle');
      if (btn) btn.innerHTML = (mgcvCurrentMode === 'dark') ? '\\u2600' : '\\u263E';
      try { localStorage.setItem('mgcvUI_theme', mgcvCurrentMode); } catch(e) {}
    }

    // Toggle element visibility
    Shiny.addCustomMessageHandler('mgcv_toggle_el', function(msg) {
      var el = document.getElementById(msg.id);
      if (el) el.style.display = msg.show ? 'block' : 'none';
    });

    // Update preset description text
    Shiny.addCustomMessageHandler('mgcv_preset_desc', function(msg) {
      var el = document.getElementById('vars-preset_desc');
      if (el) el.textContent = msg;
    });

    // Append white checkmark inside a button (no duplicates)
    function mgcvAddCheck(btnId) {
      var btn = document.getElementById(btnId);
      if (btn && !btn.querySelector('.mgcv-check')) {
        btn.insertAdjacentHTML('beforeend',
          ' <span class=\"mgcv-check\" style=\"color:#fff;\">&#10003;</span>');
      }
    }
    Shiny.addCustomMessageHandler('mgcv_show_check', function(msg) {
      mgcvAddCheck(msg.id);
    });
    Shiny.addCustomMessageHandler('download_check', function(msg) {
      mgcvAddCheck(msg.id);
    });
    Shiny.addCustomMessageHandler('mgcv_clear_checks', function(msg) {
      document.querySelectorAll('.mgcv-check').forEach(function(el) { el.remove(); });
    });

    // Reset a fileInput widget to its initial state
    Shiny.addCustomMessageHandler('mgcv_reset_file_input', function(msg) {
      var el = document.getElementById(msg.id);
      if (el) {
        var input = el.querySelector('input[type=\"file\"]');
        if (input) input.value = '';
        var label = el.querySelector('.form-control');
        if (label) label.textContent = 'No file chosen';
      }
    });

    // --- Fitting progress modal (matches earthUI) ---
    Shiny.addCustomMessageHandler('fitting_start', function(msg) {
      $('#mgcv-fitting-modal, #mgcv-fitting-backdrop').remove();
      clearInterval(window.mgcvTimerInterval);
      clearInterval(window.mgcvFittingTabsPoll);
      document.querySelectorAll('.mgcv-check').forEach(function(el) { el.remove(); });

      var start = Date.now();
      var modal = $(
        '<div id=\\\"mgcv-fitting-modal\\\" style=\\\"position:fixed;top:50%;left:50%;transform:translate(-50%,-50%);z-index:10001;' +
        'background:#2e3440;color:#d8dee9;border-radius:8px;box-shadow:0 4px 24px rgba(0,0,0,0.5);' +
        'width:520px;max-width:90vw;font-family:monospace;overflow:hidden;\\\">' +
          '<div style=\\\"background:#3b4252;padding:10px 16px;display:flex;justify-content:space-between;align-items:center;\\\">' +
            '<span style=\\\"font-size:0.95em;font-weight:bold;\\\">Fitting GAM Model</span>' +
            '<span id=\\\"mgcv-timer\\\" style=\\\"font-size:0.85em;color:#81a1c1;\\\">0s</span>' +
            '&nbsp;&nbsp;<span id=\\\"mgcv-fitting-abort\\\" style=\\\"color:#bf616a;cursor:pointer;' +
              'font-size:0.85em;font-weight:bold;margin-left:8px;\\\">Abort</span>' +
            '<span id=\\\"mgcv-fitting-close\\\" style=\\\"display:none;color:#81a1c1;cursor:pointer;' +
              'font-size:1.3em;line-height:1;margin-left:12px;\\\">&times;</span>' +
          '</div>' +
          '<div id=\\\"mgcv-trace-log\\\" style=\\\"padding:8px 12px;height:300px;overflow-y:auto;font-size:0.78em;line-height:1.5;\\\"></div>' +
        '</div>'
      );
      $('<div id=\\\"mgcv-fitting-backdrop\\\" style=\\\"position:fixed;top:0;left:0;width:100%;height:100%;' +
        'background:rgba(0,0,0,0.4);z-index:10000;\\\"></div>').appendTo('body');
      modal.appendTo('body');

      $('#mgcv-fitting-close').on('click', function() {
        clearInterval(window.mgcvTimerInterval);
        clearInterval(window.mgcvFittingTabsPoll);
        window.mgcvTimerInterval = null;
        window.mgcvFittingTabsPoll = null;
        $('#mgcv-fitting-modal, #mgcv-fitting-backdrop').fadeOut(300, function(){ $(this).remove(); });
      });

      $('#mgcv-fitting-abort').on('click', function() {
        Shiny.setInputValue('model-abort_fit', Date.now(), {priority: 'event'});
        $('#mgcv-trace-log').append($('<div style=\\\"color:#bf616a;font-weight:bold;margin-top:4px;\\\">').text('Aborting...'));
        $(this).css({opacity: 0.5, cursor: 'default'}).off('click');
      });

      $('#mgcv-trace-log').append($('<div style=\\\"color:#88c0d0;\\\">').text('Starting model fit...'));

      window.mgcvTimerInterval = setInterval(function() {
        var s = Math.floor((Date.now() - start) / 1000);
        var m = Math.floor(s / 60);
        var label = m > 0 ? m + 'm ' + (s % 60) + 's' : s + 's';
        $('#mgcv-timer').text(label);
      }, 1000);
    });

    Shiny.addCustomMessageHandler('trace_line', function(msg) {
      var $log = $('#mgcv-trace-log');
      if ($log.length) {
        var color = '#a3be8c';
        if (msg.text.match(/^Starting|^Fitting/)) color = '#88c0d0';
        else if (msg.text.match(/^\\\\s*CV fold/i)) color = '#ebcb8b';
        else if (msg.text.match(/error|fail/i)) color = '#bf616a';
        var $line = $('<div style=\\\"color:' + color + ';\\\">').text(msg.text);
        $log.append($line);
        $log.scrollTop($log[0].scrollHeight);
      }
    });

    Shiny.addCustomMessageHandler('fitting_done', function(msg) {
      var $log = $('#mgcv-trace-log');
      var hasError = msg.text === 'Error' || (msg.text && msg.text.match(/error|fail/i));
      var aborted = msg.text === 'Aborted' || (msg.text && msg.text.match(/abort/i));
      var color = hasError ? '#bf616a' : (aborted ? '#ebcb8b' : '#a3be8c');
      $log.append($('<div style=\\\"color:' + color + ';font-weight:bold;margin-top:4px;\\\">').text(msg.text));
      $log.scrollTop($log[0].scrollHeight);
      if (hasError || aborted) {
        // Terminal: do NOT run the 'completing the tabs' sequence on abort/error.
        if (window.mgcvFittingTabsPoll) { clearInterval(window.mgcvFittingTabsPoll); window.mgcvFittingTabsPoll = null; }
        clearInterval(window.mgcvTimerInterval);
        window.mgcvTimerInterval = null;
        $('#mgcv-fitting-close').show();
      } else {
        $log.append($('<div style=\\\"color:#88c0d0;margin-top:2px;\\\">').text('Now completing the tabs.'));
        $log.scrollTop($log[0].scrollHeight);
        var tabPollCount = 0;
        window.mgcvFittingTabsPoll = setInterval(function() {
          tabPollCount++;
          var still = $('.recalculating').length;
          if (still === 0 || tabPollCount > 100) {
            clearInterval(window.mgcvFittingTabsPoll);
            window.mgcvFittingTabsPoll = null;
            clearInterval(window.mgcvTimerInterval);
            window.mgcvTimerInterval = null;
            var elapsed = $('#mgcv-timer').text();
            $log.append($('<div style=\\\"color:#a3be8c;font-weight:bold;margin-top:2px;\\\">').text('Tabs complete. (' + elapsed + ')'));
            $log.append($('<div style=\\\"color:#a3be8c;font-weight:bold;margin-top:8px;\\\">').text('Done - all sections processed. Nothing else is coming; you can close this window.'));
            var $doneBtn = $('<button type=\\\"button\\\" style=\\\"margin-top:8px;background:#5e81ac;color:#eceff4;border:none;border-radius:4px;padding:6px 18px;cursor:pointer;font-family:inherit;font-size:0.9em;\\\">Close</button>');
            $doneBtn.on('click', function(){ $('#mgcv-fitting-close').click(); });
            $log.append($doneBtn);
            $log.scrollTop($log[0].scrollHeight);
            $('#mgcv-fitting-close').show();
          }
        }, 300);
      }
    });

    $(document).on('shiny:connected', function() {
      var saved = null;
      try { saved = localStorage.getItem('mgcvUI_theme'); } catch(e) {}
      if (saved === 'dark') {
        mgcvCurrentMode = 'dark';
        document.body.setAttribute('data-mgcv-theme', 'dark');
        var btn = document.getElementById('mgcv-theme-toggle');
        if (btn) btn.innerHTML = '\\u2600';
        Shiny.setInputValue('dark_mode', 'dark', {priority: 'event'});
      } else {
        document.body.setAttribute('data-mgcv-theme', 'light');
        Shiny.setInputValue('dark_mode', 'light', {priority: 'event'});
      }

      // Restore last-used purpose
      var lastPurpose = null;
      try { lastPurpose = localStorage.getItem('mgcvUI_last_purpose'); } catch(e) {}
      if (lastPurpose) {
        var radio = $(\"input[name='purpose'][value='\" + lastPurpose + \"']\");
        if (radio.length) {
          radio.prop('checked', true).trigger('change');
        }
      }
    });

    // Save purpose whenever it changes
    $(document).on('change', \"input[name='purpose']\", function() {
      try { localStorage.setItem('mgcvUI_last_purpose', $(this).val()); } catch(e) {}
    });

    // Initialize Bootstrap 5 popovers for param help icons
    $(document).on('shiny:value', function() {
      setTimeout(function() {
        var els = document.querySelectorAll('[data-bs-toggle=popover]');
        els.forEach(function(el) {
          if (!bootstrap.Popover.getInstance(el)) {
            new bootstrap.Popover(el, { html: false });
          }
        });
      }, 300);
    });
    // Also init on page load
    $(function() {
      setTimeout(function() {
        var els = document.querySelectorAll('[data-bs-toggle=popover]');
        els.forEach(function(el) {
          new bootstrap.Popover(el, { html: false });
        });
      }, 500);
    });
  ")),

  sidebarLayout(
    sidebarPanel(
      width = 4,

      # --- Purpose Mode (hidden — driven by the active project; kept in the
      # DOM so existing input$purpose-based logic continues to work) ---
      conditionalPanel(condition = "false",
        radioButtons("purpose", "Purpose:",
                     choices = c("General" = "general",
                                 "For Appraisal" = "appraisal",
                                 "Market Area Analysis" = "market"),
                     selected = "general", inline = TRUE)
      ),

      # --- 1. Project ---
      tags$details(class = "mgcv-section", open = NA,
        tags$summary(h4("1. Project"),
          tags$span(class = "mgcv-section-info",
            `data-bs-toggle` = "popover", `data-bs-trigger` = "hover focus",
            `data-bs-placement` = "left", onclick = "event.stopPropagation();",
            `data-bs-content` = "Active project sets the location (purpose, country, state, county, city) and the input/output folders. Pick an existing project or create a new one. Projects are shared with earthUI and glmnetUI. Click Close Project to switch.",
            "?")),
        uiOutput("regproj_project_ui")
      ),
      hr(),

      conditionalPanel(condition = "output.has_active_project",
        # --- 2. Import Data (from the project's in/ folder) ---
        tags$details(class = "mgcv-section", open = NA,
          tags$summary(h4("2. Import Data"),
            tags$span(class = "mgcv-section-info",
              `data-bs-toggle` = "popover", `data-bs-trigger` = "hover focus",
              `data-bs-placement` = "left", onclick = "event.stopPropagation();",
              `data-bs-content` = "Pick a CSV or Excel file from the active project's in/ folder. Each row is an observation (comparable sale) and each column is a variable (sale price, living area, age, etc.). Add files to the project's <os>_in/ folder via Finder/Explorer, then click Refresh.",
              "?")),
          mod_data_ui("data")
        ),
        hr(),

        # --- 3. Import from earthUI (optional) ---
        tags$details(class = "mgcv-section", open = NA,
          tags$summary(
            h4(style = "display:inline;",
              "3. Import from earthUI (optional)"),
            tags$span(class = "mgcv-section-info",
              `data-bs-toggle` = "popover", `data-bs-trigger` = "hover focus",
              `data-bs-placement` = "left", onclick = "event.stopPropagation();",
              `data-bs-content` = "Optionally import an earthUI result (.rds file) to seed the GAM with knot positions discovered by MARS. Earth finds data-driven change points (hinges) that become anchor knots for the GAM splines, giving the model a head start. Also imports variable selections, interaction structure, and linear/factor designations from the earth model.",
              "?")),
          div(style = "padding-top: 6px;",
            mod_earth_import_ui("earth")
          )
        ),
        hr()
      ),

      div(class = "shiny-panel-conditional",
        `data-display-if` = "output.data_imported",
        `data-ns-prefix` = "",
        style = "display:none;",

        # output_folder is derived from the active project (hidden in the DOM
        # so existing input$output_folder readers keep working).
        conditionalPanel(condition = "false",
          textInput("output_folder", NULL, value = "")
        ),

        # --- 4. Variable Configuration ---
        tags$details(class = "mgcv-section",
          tags$summary(h4("4. Variable Configuration"),
            tags$span(class = "mgcv-section-info",
              `data-bs-toggle` = "popover", `data-bs-trigger` = "hover focus",
              `data-bs-placement` = "left", onclick = "event.stopPropagation();",
              `data-bs-content` = "Select the target (response) variable and configure predictors. Inc = include in model. Factor = treat as categorical. Linear = force a straight-line relationship (no smooth). Special = assign a column role (e.g. contract_date, weight, living_area) for appraisal-specific features.",
              "?")),
          conditionalPanel(
            condition = "input.purpose !== 'general'",
            dateInput("effective_date", "Effective Date",
                      value = Sys.Date())
          ),
          mod_variables_ui("vars")
        ),
        hr(),

        # --- 5. Mgcv Call Parameters ---
        tags$details(class = "mgcv-section", open = NA,
          tags$summary(h4("5. Mgcv Call Parameters"),
            tags$span(class = "mgcv-section-info",
              `data-bs-toggle` = "popover", `data-bs-trigger` = "hover focus",
              `data-bs-placement` = "left", onclick = "event.stopPropagation();",
              `data-bs-content` = "Configure the mgcv::gam() call: distribution family, smoothing method, penalty strength (gamma), basis type for splines, basis dimension (k), variable selection, and interaction structure. These control how flexible and regularized the fitted model will be.",
              "?")),
          mod_variables_params_ui("vars")
        ),
        hr(),

        # --- 6. Fit Mgcv GAM Model ---
        tags$details(class = "mgcv-section", open = NA,
          tags$summary(h4("6. Fit Mgcv GAM Model"),
            tags$span(class = "mgcv-section-info",
              `data-bs-toggle` = "popover", `data-bs-trigger` = "hover focus",
              `data-bs-placement` = "left", onclick = "event.stopPropagation();",
              `data-bs-content` = "Fit the Generalized Additive Model using mgcv. The model estimates smooth nonlinear relationships between each predictor and the response. Results appear in the tabs on the right: summary statistics, contribution plots, diagnostics, and more.",
              "?")),
          mod_model_fit_ui("model")
        ),

        # --- 7. Download Estimated Sale Prices & Residuals ---
        div(class = "shiny-panel-conditional",
          `data-display-if` = "output.model_fitted",
          `data-ns-prefix` = "",
          style = "display:none;",
          hr(),
          tags$details(class = "mgcv-section",
            tags$summary(uiOutput("download_heading", inline = TRUE),
              tags$span(class = "mgcv-section-info",
                `data-bs-toggle` = "popover", `data-bs-trigger` = "hover focus",
                `data-bs-placement` = "left", onclick = "event.stopPropagation();",
                `data-bs-content` = "Downloads an Excel file with model predictions, residuals, CQA scores, per-variable contributions, and basis values appended to your data.",
                "?")),
            actionButton("export_data", "Download Output (Excel)",
                         class = "btn-primary",
                         style = "width: 100%;")
          )
        ),

        # --- 8. Calculate RCA Adjustments (Appraisal/Market only) ---
        div(class = "shiny-panel-conditional",
          `data-display-if` = "output.model_fitted && (input.purpose === 'appraisal' || input.purpose === 'market')",
          `data-ns-prefix` = "",
          style = "display:none;",
          hr(),
          tags$details(class = "mgcv-section",
            tags$summary(h4("8. Calculate RCA Adjustments & Download"),
              tags$span(class = "mgcv-section-info",
                `data-bs-toggle` = "popover", `data-bs-trigger` = "hover focus",
                `data-bs-placement` = "left", onclick = "event.stopPropagation();",
                `data-bs-content` = "Calculates Residual Constraint Approach (RCA). Choose CQA or CQA per SF (if living area is designated), enter the subject's score to interpolate its residual, then computes per-variable and net/gross adjustments for each comparable sale.",
                "?")),
            actionButton("rca_output_btn",
                         "Calculate RCA Adjustments & Download",
                         class = "btn-primary",
                         style = "width: 100%;")
          )
        ),

        # --- 9. Generate Sales Grid & Download (Appraisal only) ---
        div(class = "shiny-panel-conditional",
          `data-display-if` = "output.model_fitted && input.purpose === 'appraisal'",
          `data-ns-prefix` = "",
          style = "display:none;",
          hr(),
          tags$details(class = "mgcv-section",
            tags$summary(h4("9. Generate Sales Grid & Download"),
              tags$span(class = "mgcv-section-info",
                `data-bs-toggle` = "popover", `data-bs-trigger` = "hover focus",
                `data-bs-placement` = "left", onclick = "event.stopPropagation();",
                `data-bs-content` = "Generates a formatted Excel sales comparison grid. Recommends comps with gross adjustments under 25%, sorted by sale age. Each sheet shows the subject and up to 3 comps with contributions and adjustments.",
                "?")),
            tags$p("Recommends comps with gross adjustment < 25%, ",
                   "sorted by sale age. You can add or remove comps.",
                   style = "font-size: 0.85em; color: var(--bs-secondary-color);"),
            actionButton("sales_grid_btn",
                         "Generate Sales Grid & Download",
                         class = "btn-primary",
                         style = "width: 100%;")
          )
        ),

        # --- 10. Generate Quarto Report (writes .qmd bundle + assets) ---
        div(class = "shiny-panel-conditional",
          `data-display-if` = "output.model_fitted",
          `data-ns-prefix` = "",
          style = "display:none;",
          hr(),
          tags$details(class = "mgcv-section",
            tags$summary(uiOutput("generate_qmd_heading", inline = TRUE),
              tags$span(class = "mgcv-section-info",
                `data-bs-toggle` = "popover", `data-bs-trigger` = "hover focus",
                `data-bs-placement` = "left", onclick = "event.stopPropagation();",
                `data-bs-content` = "Writes a self-contained Quarto report bundle (.qmd source + plots + data + reference.docx) to the project's mgcv output folder. The bundle can be edited or combined with other Quarto sources before being rendered in the next section.",
                "?")),
            actionButton("generate_qmd_btn", "Generate Quarto Report",
                         class = "btn-primary",
                         style = "width: 100%;")
          )
        ),

        # --- 11. Convert Quarto Report (any .qmd) -> HTML/Word/PDF ---
        conditionalPanel(condition = "output.has_active_project",
          hr(),
          tags$details(class = "mgcv-section",
            tags$summary(uiOutput("convert_qmd_heading", inline = TRUE),
              tags$span(class = "mgcv-section-info",
                `data-bs-toggle` = "popover", `data-bs-trigger` = "hover focus",
                `data-bs-placement` = "left", onclick = "event.stopPropagation();",
                `data-bs-content` = "Renders any Quarto .qmd file to HTML / Word / PDF. Defaults to the most recently generated bundle, but you can browse to any .qmd.",
                "?")),
            tags$div(style = "display:flex; gap:6px; align-items:flex-end;",
              tags$div(style = "flex:1;",
                textInput("convert_qmd_path", "Quarto source (.qmd)", value = "")),
              shinyFiles::shinyFilesButton("convert_qmd_browse", "Browse…",
                                           "Choose a Quarto file", multiple = FALSE,
                                           class = "btn btn-outline-secondary btn-sm",
                                           style = "margin-bottom:15px;")
            ),
            checkboxGroupInput("convert_formats", "Output formats",
                               choices = {
                                 fmts <- c("HTML" = "html", "Word" = "docx")
                                 if (earthUI:::has_latex_()) fmts <- c(fmts, "PDF" = "pdf")
                                 fmts
                               },
                               selected = "html", inline = TRUE),
            if (!earthUI:::has_latex_())
              tags$p(style = "font-size: 0.78em; color: #81A1C1; margin-top: -8px;",
                     "PDF requires LaTeX: ", tags$code("tinytex::install_tinytex()")),
            actionButton("convert_qmd_btn", "Convert",
                         class = "btn-primary",
                         style = "width: 100%;")
          )
        )
      )
    ),

    mainPanel(
      width = 8,
      conditionalPanel(
        condition = "!output.data_imported",
        div(
          class = "text-muted",
          style = "text-align: center; padding: 80px 20px;",
          h3("Welcome to mgcvUI"),
          p("Upload a CSV or Excel file, configure variables, ",
            "and fit a GAM model."),
          p("Designed for real estate appraisers who need transparent, ",
            "defensible nonlinear models.")
        )
      ),
      mod_model_results_ui("model")
    )
  ),

  # Footer
  tags$hr(style = "margin-top: 30px; margin-bottom: 10px;"),
  tags$footer(
    style = paste("text-align: center; padding: 10px 15px 15px;",
                  "font-size: 0.8em; color: var(--bs-secondary-color);"),
    tags$p(style = "margin: 2px 0; font-weight: 600;",
      HTML(paste0(
        "&#9888; For analysis only &mdash; not an appraisal. Any value ",
        "conclusion must be independently reviewed and signed by a qualified, ",
        "licensed appraiser per applicable standards (e.g. USPAP). ")),
      tags$a(href = "#",
             onclick = paste0("event.preventDefault();var e=document.getElementById",
                              "('show_about');if(e)e.click();return false;"),
             "Full disclaimer")
    ),
    tags$p(style = "margin: 2px 0;",
      HTML(paste0("mgcvUI v", utils::packageVersion("earthUI")))
    ),
    tags$p(style = "margin: 2px 0;",
      "Licensed under the ",
      tags$a(href = "https://www.gnu.org/licenses/agpl-3.0.html",
             target = "_blank",
             "GNU Affero General Public License v3.0"),
      " or later (AGPL-3)."
    )
  )
)

server <- function(input, output, session) {
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

  # --- Helper: reveal file in Finder on macOS ---
  reveal_in_finder_ <- function(path) {
    if (Sys.info()[["sysname"]] == "Darwin" && file.exists(path)) {
      tryCatch(
        system2("open", c("-R", shQuote(path)), wait = FALSE),
        error = function(e) NULL
      )
    }
  }

  # --- Settings locale ---
  # Guard flag: suppress DB writes until startup restoration is complete
  locale_ready <- reactiveVal(FALSE)

  # Load user's locale defaults from SQLite on startup
  locale_defaults <- earthUI:::settings_db_read_locale_mgcv()
  if (!is.null(locale_defaults) && length(locale_defaults) > 0L) {
    ld <- locale_defaults
    if (!is.null(ld$locale_country))
      updateSelectInput(session, "locale_country", selected = ld$locale_country)
    if (!is.null(ld$locale_paper))
      updateSelectInput(session, "locale_paper", selected = ld$locale_paper)
    if (!is.null(ld$locale_import))
      updateSelectInput(session, "data-locale_import", selected = ld$locale_import)
    if (!is.null(ld$effective_date) && nzchar(ld$effective_date))
      updateDateInput(session, "effective_date",
                      value = as.Date(ld$effective_date))
    # output_folder and purpose are now derived from the active regProj
    # project, not restored from locale defaults.
    message("mgcvUI: restored locale defaults from SQLite")
  }

  # Mark locale as ready after the first flush so restoration completes first
  session$onFlushed(function() {
    locale_ready(TRUE)
    message("mgcvUI: locale guard released")
  }, once = TRUE)

  # Settings "Save": persist locale defaults + the regProj root, and keep the
  # Settings panel open so the user can review/adjust the other fields.
  observeEvent(input$settings_save, {
    # 1) Locale defaults -> settings DB
    locale_settings <- list(
      locale_country = input$locale_country,
      locale_paper   = input$locale_paper,
      locale_import  = input[["data-locale_import"]]
    )
    earthUI:::settings_db_write_locale_mgcv(locale_settings)
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
        showNotification(sprintf("Cannot create or access: %s", p),
                         type = "error", duration = 6)
        return()
      }
    }
    showNotification(paste0("Settings saved.", root_msg),
                     type = "message", duration = 4)
  })

  # Settings "Close": dismiss the Settings panel (does not save).
  observeEvent(input$settings_close, {
    session$sendCustomMessage("close_settings_dropdown", list())
  })

  # Help / technical-support dialog (emails support@valuation-engineer.com)
  observeEvent(input$show_help, {
    session$sendCustomMessage("close_settings_dropdown", list())
    showModal(earthUI:::support_request_modal_("mgcvUI"))
  })

  # About / disclaimer dialog
  observeEvent(input$show_about, {
    session$sendCustomMessage("close_settings_dropdown", list())
    showModal(modalDialog(
      title = HTML("&#9888; About mgcvUI"),
      size = "l", easyClose = TRUE,
      earthUI:::appraisal_disclaimer_html_(),
      tags$hr(),
      tags$p(class = "text-muted small",
             sprintf("mgcvUI %s - AGPL-3.0 license.",
                     as.character(utils::packageVersion("earthUI")))),
      footer = modalButton("Close")))
  })

  # When Settings country changes, sync import locale and update env
  observeEvent(input$locale_country, {
    country <- input$locale_country %||% "us"
    presets <- earthUI:::locale_country_presets_()
    preset <- presets[[country]] %||% presets[["us"]]
    updateSelectInput(session, "locale_paper", selected = preset$paper)
    # Sync the per-file import locale dropdown in the data module
    updateSelectInput(session, "data-locale_import", selected = country)
    earthUI:::set_locale_(country)
    # Only persist if startup restoration is done
    if (isTRUE(isolate(locale_ready()))) {
      earthUI:::settings_db_write_locale_mgcv(list(
        locale_country = country,
        locale_paper   = preset$paper,
        locale_import  = country
      ))
    }
  })

  # When import locale or paper changes, update locale env
  observe({
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
    # Only persist if startup restoration is done
    if (isTRUE(isolate(locale_ready()))) {
      earthUI:::settings_db_write_locale_mgcv(list(
        locale_country = settings_country,
        locale_paper   = paper,
        locale_import  = import_country
      ))
    }
  })

  # Persist effective date to global defaults
  observeEvent(input$effective_date, {
    if (!isTRUE(locale_ready())) return()
    ed <- input$effective_date
    if (is.null(ed)) return()
    ld <- earthUI:::settings_db_read_locale_mgcv() %||% list()
    ld$effective_date <- as.character(ed)
    earthUI:::settings_db_write_locale_mgcv(ld)
  })

  # ===== regProj project state (shared root + DBs with earthUI/glmnetUI) =====
  rv_proj <- reactiveValues(
    active_project        = NULL,   # one-row data.frame or NULL
    project_sort          = "recent",
    project_refresh_token = 0L
  )
  active_project_r <- reactive(rv_proj$active_project)

  # Per-session DB connections.
  geo_con_      <- tryCatch(earthUI::regproj_geo_db_connect(),
                            error = function(e) NULL)
  projects_con_ <- tryCatch(earthUI::regproj_projects_db_connect(),
                            error = function(e) NULL)
  session$onSessionEnded(function() {
    if (!is.null(geo_con_))      try(DBI::dbDisconnect(geo_con_),      silent = TRUE)
    if (!is.null(projects_con_)) try(DBI::dbDisconnect(projects_con_), silent = TRUE)
  })

  output$has_active_project <- reactive(!is.null(rv_proj$active_project))
  outputOptions(output, "has_active_project", suspendWhenHidden = FALSE)

  # Data import - reads files from the active project's in/ folder
  data_mod <- mod_data_server("data", active_project_r)

  # --- Data imported flag ---
  output$data_imported <- reactive(!is.null(data_mod$data()))
  outputOptions(output, "data_imported", suspendWhenHidden = FALSE)

  # Optional earth import
  earth_mod <- mod_earth_import_server("earth")
  earth_knots_r <- earth_mod$knots

  # When the active project changes (open / close / switch): set the hidden
  # Purpose radio + the derived output folder (<os>_out_mgcv) and reset all
  # per-file state. mod_data picks up the project's last-used file and loads
  # it. Forward references to model_mod / rv_rca are fine — this observer
  # fires during flush, after the server function has finished defining them.
  observeEvent(rv_proj$active_project, ignoreNULL = FALSE, {
    p <- rv_proj$active_project
    pur <- if (is.null(p)) "general" else
      switch(p$purpose, gen = "general", appr = "appraisal",
             mktarea = "market", "general")
    updateRadioButtons(session, "purpose", selected = pur)

    # earthUI is the source of truth for the Effective Date: apply earthUI's
    # saved value on every project open (falls back to the existing value when
    # earthUI has none).
    cf_proj <- earthUI::earth_carryforward_(
      if (is.null(p)) NULL else p$project_path, pur)
    if (!is.null(cf_proj$effective_date))
      updateDateInput(session, "effective_date",
                      value = cf_proj$effective_date)

    data_mod$reset()
    earth_mod$reset()
    model_mod$reset()
    rv_rca$pct_data       <- NULL
    rv_rca$rca_df         <- NULL
    rv_rca$sg_recommended <- NULL
    rv_rca$sg_others      <- NULL
    session$sendCustomMessage("mgcv_clear_checks", list())

    if (is.null(p)) {
      updateTextInput(session, "output_folder", value = "")
    } else {
      parsed <- earthUI::regproj_parse_flat(basename(p$project_path))
      # Derive the output folder from the project's ACTUAL path so it lands
      # under the project's real regProj root (not a default-root recomputation,
      # which goes to the wrong root for a custom regProj root).
      out_dir <- file.path(p$project_path,
                           paste0(earthUI::os_detect(), "_out_mgcv"))
      if (!dir.exists(out_dir))
        dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)
      updateTextInput(session, "output_folder", value = out_dir)
      # Keep the projects DB row fresh (shared with earthUI/glmnetUI).
      tryCatch(
        earthUI::register_project(p$project_path, p$purpose, parsed$country,
                                 parsed$levels, parsed$project_name),
        error = function(e) NULL)
    }
  })

  # ===== Project section (Section 1) =====

  # Drive browser (shared by regProj root + any folder pickers).
  volumes <- c(Home = path.expand("~"), shinyFiles::getVolumes()())
  if (.Platform$OS.type == "unix" && dir.exists("/Volumes"))
    volumes <- c(volumes, Volumes = "/Volumes")
  if (.Platform$OS.type == "unix" && dir.exists("/media"))
    volumes <- c(volumes, Media = "/media")
  if (.Platform$OS.type == "unix" && dir.exists("/mnt"))
    volumes <- c(volumes, Mounts = "/mnt")

  # Populate the regProj root field once at session start.
  observe({
    isolate({
      if (is.null(input$regproj_root) || !nzchar(input$regproj_root %||% ""))
        updateTextInput(session, "regproj_root", value = default_regproj_root())
    })
  })

  shinyFiles::shinyDirChoose(input, "regproj_root_browse", roots = volumes,
                             session = session,
                             restrictions = system.file(package = "base"))
  observeEvent(input$regproj_root_browse, {
    d <- shinyFiles::parseDirPath(volumes, input$regproj_root_browse)
    if (length(d) > 0 && nzchar(d))
      updateTextInput(session, "regproj_root", value = as.character(d))
  })

  # Pending location prefill for the New Project modal.
  np_prefill_ <- reactiveVal(NULL)

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

  projects_df_ <- reactive({
    rv_proj$project_refresh_token
    regproj_list_projects(sort_by = rv_proj$project_sort)
  })

  output$regproj_project_ui <- renderUI({
    df <- projects_df_()
    active <- rv_proj$active_project
    if (!is.null(active)) {
      pur_label <- switch(active$purpose, gen = "General", appr = "Appraisal",
                          mktarea = "Market Area", active$purpose)
      loc_friendly <- paste(pur_label, "·", toupper(active$country), "·",
                            toupper(active$state), "·", active$county, "·",
                            active$city)
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
                       class = "btn btn-outline-primary btn-sm"))
      ))
    }
    if (nrow(df) == 0L) {
      return(tagList(
        tags$div(class = "small text-muted", style = "margin-bottom:8px;",
                 "(no projects yet — click + New to create one)"),
        actionButton("regproj_project_new", "+ New…",
                     class = "btn btn-outline-primary btn-sm")))
    }
    labels <- paste0(df$project_name, "  —  ", df$purpose, " / ", df$country,
                     " / ", df$state, " / ", df$county, " / ", df$city)
    choices <- stats::setNames(df$project_path, labels)
    tagList(
      selectizeInput("regproj_project_pick", NULL,
                     choices = c("(select a project)" = "", choices),
                     selected = "", width = "100%"),
      tags$div(style = "display:flex; gap:6px; align-items:center;",
        radioButtons("regproj_project_sort", NULL,
                     choices = c("Recent" = "recent", "A-Z" = "alpha"),
                     selected = rv_proj$project_sort, inline = TRUE),
        actionButton("regproj_project_new", "+ New…",
                     class = "btn btn-outline-primary btn-sm"))
    )
  })

  observeEvent(input$regproj_project_sort, {
    rv_proj$project_sort <- input$regproj_project_sort
  }, ignoreInit = TRUE)

  observeEvent(input$regproj_project_pick, {
    p <- input$regproj_project_pick
    if (is.null(p) || !nzchar(p)) return()
    df <- projects_df_()
    row <- df[df$project_path == p, , drop = FALSE]
    if (nrow(row) == 1L) rv_proj$active_project <- row
  }, ignoreInit = TRUE)

  observeEvent(input$regproj_project_close, {
    rv_proj$active_project <- NULL
  })

  # Trilogy mode: auto-open the trilogy project at startup, so the project's
  # data is available and the fit registers to the right project.
  observeEvent(TRUE, {
    ctx <- getOption("mgcvUI.trilogy")
    pp <- ctx$project_path %||% NULL
    if (is.null(pp)) return()
    df <- tryCatch(regproj_list_projects(sort_by = "recent"),
                   error = function(e) NULL)
    if (is.data.frame(df)) {
      row <- df[df$project_path == pp, , drop = FALSE]
      if (nrow(row) == 1L) rv_proj$active_project <- row
    }
  }, once = TRUE)

  observeEvent(input$regproj_project_new, {
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
    showModal(modalDialog(
      title = "Create New Project", size = "l", easyClose = FALSE,
      footer = tagList(modalButton("Cancel"),
        actionButton("np_create", "Create Project", class = "btn-primary")),
      tags$div(class = "small text-muted", style = "margin-bottom: 12px;",
        "Country, State, County, City, and Purpose are locked once the project is created."),
      textInput("np_project_name", "Project Name * (max 8 chars)",
                placeholder = "max 8 chars; a date-time prefix is added automatically",
                width = "100%"),
      radioButtons("np_purpose", "Purpose *",
                   choices = c("General" = "gen", "For Appraisal" = "appr",
                               "Market Area Analysis" = "mktarea"),
                   selected = "appr", inline = TRUE),
      hr(), tags$h5("Project Location"),
      uiOutput("np_country_ui"), uiOutput("np_levels_ui"),
      hr(), tags$h5("Initial Data File", style = "margin-bottom:4px;"),
      tags$div(class = "small text-muted", style = "margin-bottom:8px;",
        "Optional — copied into the project's in/ folder; the name is preserved."),
      fileInput("np_data_file", NULL, accept = c(".csv", ".xlsx", ".xls"),
                width = "100%")
    ))
  })

  output$np_country_ui <- renderUI({
    ref <- regproj_reference()
    labels <- paste0(unlist(ref$countries), " (", names(ref$countries), ")")
    choices <- stats::setNames(names(ref$countries), labels)
    pf <- np_prefill_()
    sel_cc <- if (!is.null(pf) && nzchar(pf$country %||% "")) pf$country else "us"
    selectInput("np_country", "Country *", choices = choices,
                selected = sel_cc, width = "100%")
  })

  output$np_levels_ui <- renderUI({
    cc <- input$np_country %||% "us"
    schema <- country_schema(cc)
    if (length(schema) == 0L)
      return(tags$div(class = "small text-muted",
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
        selectInput(paste0("np_level_", i), paste0(lbl, " *"),
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
        selectizeInput(paste0("np_level_", i), paste0(lbl, " *"),
                       choices = choices, selected = sel_city,
                       options = list(create = TRUE,
                         placeholder = "Pick existing or type a new full name"),
                       width = "100%")
      } else {
        selectizeInput(paste0("np_level_", i), paste0(lbl, " *"),
                       choices = ch, selected = sel,
                       options = list(create = TRUE,
                         placeholder = "Pick existing or type a new name"),
                       width = "100%")
      }
    })
    do.call(tagList, inputs)
  })

  observeEvent(input$np_create, {
    cc <- input$np_country %||% ""
    schema <- country_schema(cc)
    last_idx <- length(schema)
    pur <- input$np_purpose %||% "appr"
    # Validate the typed name (<= 8 chars) and prepend the creation timestamp.
    # Country-agnostic: works for any country's admin-level depth.
    proj <- tryCatch(
      regproj_new_project_name(trimws(input$np_project_name %||% "")),
      error = function(e) {
        showNotification(conditionMessage(e), type = "error", duration = 5)
        NULL
      })
    if (is.null(proj)) return()
    if (length(schema) == 0L) {
      showNotification("This country has no admin levels defined.",
                       type = "error", duration = 5); return()
    }
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
        full_name <- val
        if (!nzchar(full_name)) {
          showNotification("City is required.", type = "error", duration = 5); return()
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
                       showNotification(paste("Path error:", conditionMessage(e)),
                                        type = "error", duration = 5); NULL
                     })
    if (is.null(flat)) return()
    proj_root <- file.path(default_regproj_root(), pur, flat)
    if (dir.exists(proj_root)) {
      showNotification(sprintf("A project named '%s' already exists in this location.", proj),
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
    removeModal()
    showNotification(sprintf("Created project '%s'", proj),
                     type = "message", duration = 6)
  })

  observeEvent(input$regproj_project_info, {
    p <- rv_proj$active_project
    if (is.null(p)) return()
    pur_label <- switch(p$purpose, gen = "General", appr = "For Appraisal",
                        mktarea = "Market Area Analysis", p$purpose)
    showModal(modalDialog(
      title = "Project Info", easyClose = TRUE, footer = modalButton("Close"),
      tags$dl(
        tags$dt("Project"), tags$dd(p$project_name),
        tags$dt("Purpose"), tags$dd(pur_label),
        tags$dt("Location"),
        tags$dd(paste(toupper(p$country), "·", toupper(p$state), "·",
                      p$county, "·", p$city)),
        tags$dt("Folder"),
        tags$dd(tags$code(p$project_path)),
        tags$dt("Inputs"),
        tags$dd(tags$code(file.path(p$project_path,
                                    paste0(os_detect(), "_in")))),
        tags$dt("Outputs (mgcv)"),
        tags$dd(tags$code(file.path(p$project_path,
                                    paste0(os_detect(), "_out_mgcv"))))
      )
    ))
  })

  # Variable selection - returns config list
  var_config_r <- mod_variables_server("vars",
                                       data_r           = data_mod$data,
                                       filename_r       = data_mod$filename,
                                       earth_knots_r    = earth_knots_r,
                                       purpose_r        = reactive(input$purpose),
                                       active_project_r = active_project_r)

  # Derived data reactive: recompute sale_age when effective_date changes
  app_data_r <- reactive({
    df <- data_mod$data()
    req(df)
    cfg <- var_config_r()
    eff_date <- input$effective_date

    specials <- cfg$specials
    if (is.null(specials) || is.null(eff_date)) return(df)

    contract_col <- names(which(specials == "contract_date"))
    sale_age_col <- names(which(specials == "sale_age"))

    if (length(contract_col) == 1L && length(sale_age_col) == 1L &&
        contract_col %in% names(df) && sale_age_col %in% names(df)) {
      # Parse contract dates
      contract_dates <- df[[contract_col]]
      if (!inherits(contract_dates, "Date")) {
        fmts <- earthUI:::locale_date_formats_()
        for (fmt in fmts) {
          parsed <- as.Date(contract_dates, format = fmt)
          # Require >50% non-NA AND dates in a sane range (1900-2100)
          # to avoid %Y parsing 2-digit years as year 0025
          ok <- !is.na(parsed) & parsed >= as.Date("1900-01-01") &
                parsed <= as.Date("2100-12-31")
          if (sum(ok) > length(contract_dates) * 0.5) {
            contract_dates <- parsed
            break
          }
        }
      }
      if (inherits(contract_dates, "Date")) {
        df[[sale_age_col]] <- as.numeric(
          difftime(as.Date(eff_date), contract_dates, units = "days")
        )
      }
    }
    df
  })

  # Model fitting + results display
  model_mod <- mod_model_server("model",
                                data_r        = app_data_r,
                                var_config_r  = var_config_r,
                                earth_knots_r = earth_knots_r,
                                dark_mode_r   = reactive(identical(input$dark_mode, "dark")),
                                purpose_r     = reactive(input$purpose))
  gam_result_r <- model_mod$result

  # Trilogy mode: after each fit, register this method's fit-stamp in the
  # project's trilogy.json so the combined report can group the run's files.
  observeEvent(gam_result_r(), {
    ctx <- getOption("mgcvUI.trilogy")
    if (is.null(ctx)) return()
    pp <- ctx$project_path %||% NULL
    res <- gam_result_r()
    ts <- res$fit_ts %||% NULL
    if (!is.null(pp) && !is.null(ts)) {
      try(earthUI::trilogy_register_fit(pp, "mgcv", earthUI:::fit_stamp_(ts)),
          silent = TRUE)
    }
  }, ignoreInit = TRUE)

  # Report export (existing module — kept for function export features)
  mod_report_server("report",
                    gam_result_r = gam_result_r,
                    data_r       = app_data_r)

  # --- Model fitted flag for conditionalPanel ---
  output$model_fitted <- reactive(!is.null(gam_result_r()))
  outputOptions(output, "model_fitted", suspendWhenHidden = FALSE)

  # --- RCA percentage data (stored after RCA export) ---
  rv_rca <- reactiveValues(pct_data = NULL, rca_df = NULL)

  output$rca_computed <- reactive(!is.null(rv_rca$pct_data))
  outputOptions(output, "rca_computed", suspendWhenHidden = FALSE)

  # --- Dynamic step headings ---
  output$download_heading <- renderUI({
    label <- if (identical(input$purpose, "general")) {
      "7. Download Estimated Target Variable(s) & Residuals"
    } else {
      "7. Download Estimated Sale Prices & Residuals"
    }
    h4(label, style = "display:inline;")
  })

  output$generate_qmd_heading <- renderUI({
    n <- if (identical(input$purpose, "appraisal")) 10L
         else if (identical(input$purpose, "market")) 9L
         else 8L
    h4(paste0(n, ". Generate Quarto Report"), style = "display:inline;")
  })
  output$convert_qmd_heading <- renderUI({
    n <- if (identical(input$purpose, "appraisal")) 11L
         else if (identical(input$purpose, "market")) 10L
         else 9L
    h4(paste0(n, ". Convert Quarto Report"), style = "display:inline;")
  })

  # ---- Helper: find living_area column from variable specials ----
  find_living_area_ <- function() {
    cfg <- var_config_r()
    if (is.null(cfg) || is.null(cfg$specials)) return(NULL)
    sp <- cfg$specials
    la_idx <- which(sp == "living_area")
    if (length(la_idx) == 0) return(NULL)
    names(sp)[la_idx[1L]]
  }

  # living_area for the RCA: mgcvUI's own tag first; otherwise the column
  # earthUI tagged as living_area (carried forward), but only when an earthUI
  # model is imported and the column exists in the loaded data. Enables the
  # "CQA per SF" option without re-tagging in mgcvUI.
  resolve_living_area_ <- function() {
    la <- find_living_area_()
    if (!is.null(la)) return(la)
    if (is.null(tryCatch(earth_mod$knots(), error = function(e) NULL)) ||
        is.null(rv_proj$active_project)) return(NULL)
    la_e <- earthUI::earth_carryforward_(
      rv_proj$active_project$project_path, input$purpose)$living_area
    if (!is.null(la_e) && la_e %in% names(data_mod$data())) la_e else NULL
  }

  # ---- Helper: compute per-smooth-term contributions ----
  # Uses predict(model, type = "terms") which returns a matrix of
  # per-term contributions. For GAMs this is native and simple.
  compute_gam_contributions_ <- function(model, newdata) {
    # type = "terms" returns a matrix with one column per model term
    preds <- predict(model, newdata = newdata, type = "terms")
    # Convert to named list of numeric vectors
    contribs <- list()
    for (col in colnames(preds)) {
      contribs[[col]] <- as.numeric(preds[, col])
    }
    contribs
  }

  # --- 6. Download Output (Excel) ---
  observeEvent(input$export_data, {
    req(gam_result_r(), data_mod$data())
    if (!requireNamespace("writexl", quietly = TRUE)) {
      showNotification(
        "Package 'writexl' required. Install with: install.packages('writexl')",
        type = "error", duration = 10)
      return()
    }

    folder <- local({
      .ap <- rv_proj$active_project
      if (!is.null(.ap))
        file.path(.ap$project_path, paste0(earthUI::os_detect(), "_out_mgcv"))
      else input$output_folder
    })
    if (is.null(folder) || !nzchar(folder)) folder <- path.expand("~/Downloads")
    if (!dir.exists(folder)) dir.create(folder, recursive = TRUE)

    tryCatch({
      export_df <- data_mod$data()
      res       <- gam_result_r()
      model     <- res$model
      response  <- res$response
      xform     <- res$response_transform %||% "none"
      n         <- nrow(export_df)
      # Coerce a copy the same way fit_gam did, so predict() works on the
      # original data (dates -> numeric, factor-designated -> factor).
      pred_df   <- earthUI:::prepare_gam_data_(export_df, res$smooth_specs)

      # Predictions (model predicts on transformed scale, back-transform)
      est_col   <- rep(NA_real_, n)
      resid_col <- rep(NA_real_, n)

      complete <- tryCatch({
        pv <- predict(model, newdata = pred_df, type = "response")
        est_col <- earthUI:::back_transform_(as.numeric(pv), xform)
        resid_col <- export_df[[response]] - est_col
        rep(TRUE, n)
      }, error = function(e) {
        message("Prediction error on some rows: ", e$message)
        ok <- logical(n)
        for (i in seq_len(n)) {
          tryCatch({
            pv <- predict(model, newdata = pred_df[i, , drop = FALSE],
                          type = "response")
            est_col[i] <<- earthUI:::back_transform_(as.numeric(pv), xform)
            resid_col[i] <<- export_df[[response]][i] - est_col[i]
            ok[i] <- TRUE
          }, error = function(e2) NULL)
        }
        ok
      })

      # Per-term contributions (remain on transformed scale)
      if (any(complete)) {
        contribs <- compute_gam_contributions_(model,
                                                pred_df[complete, , drop = FALSE])
        intercept <- stats::coef(model)[["(Intercept)"]]

        # Label basis/contributions with scale info
        contrib_suffix <- if (xform != "none") paste0(" (", xform, " scale)") else ""
        export_df[["basis"]] <- NA_real_
        export_df[["basis"]][complete] <- round(intercept, 4)

        for (tl in names(contribs)) {
          col_name <- gsub("^s\\((.+)\\)$", "\\1", tl)
          col_name <- gsub("^te\\((.+)\\)$", "\\1", col_name)
          col_name <- gsub("^ti\\((.+)\\)$", "\\1", col_name)
          col_name <- paste0(col_name, "_contribution")
          export_df[[col_name]] <- NA_real_
          export_df[[col_name]][complete] <- round(contribs[[tl]], 4)
        }

        # Verification: back-transform sum of contributions vs est
        contrib_total <- intercept + Reduce(`+`, contribs)
        est_from_contribs <- earthUI:::back_transform_(contrib_total, xform)
        export_df[["calc_residual"]] <- NA_real_
        export_df[["calc_residual"]][complete] <-
          round(export_df[[response]][complete] - est_from_contribs, 1)
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
        export_df[["residual"]][1L] <- NA_real_
        if ("cqa" %in% names(export_df)) export_df[["cqa"]][1L] <- NA_real_
        if ("cqa_sf" %in% names(export_df)) export_df[["cqa_sf"]][1L] <- NA_real_
      }

      # Sort by residual_sf descending for appraisal/market
      if (input$purpose %in% c("appraisal", "market")) {
        has_subject <- identical(input$purpose, "appraisal")
        sort_col <- if ("residual_sf" %in% names(export_df)) "residual_sf" else "residual"
        if (sort_col %in% names(export_df)) {
          if (has_subject && nrow(export_df) >= 2L) {
            comps <- export_df[2:nrow(export_df), , drop = FALSE]
            comps <- comps[order(comps[[sort_col]], decreasing = TRUE,
                                 na.last = TRUE), , drop = FALSE]
            export_df <- rbind(export_df[1L, , drop = FALSE], comps)
          } else {
            export_df <- export_df[order(export_df[[sort_col]],
                                         decreasing = TRUE,
                                         na.last = TRUE), , drop = FALSE]
          }
        }
      }

      # Move ranking columns to the left
      rank_cols <- c("residual_sf", "cqa_sf", "residual", "cqa")
      rank_cols <- rank_cols[rank_cols %in% names(export_df)]
      if (length(rank_cols) > 0L) {
        other_cols <- setdiff(names(export_df), rank_cols)
        export_df <- export_df[, c(rank_cols, other_cols), drop = FALSE]
      }

      base <- tools::file_path_sans_ext(data_mod$filename() %||% "mgcvui")
      out_path <- file.path(folder, paste0(base, "_output_",
                            earthUI:::fit_stamp_(gam_result_r()$fit_ts), ".xlsx"))
      writexl::write_xlsx(export_df, out_path)
      session$sendCustomMessage("download_check", list(id = "export_data"))
      showNotification(paste0("Output saved to: ", out_path),
                       type = "message", duration = 8)
      reveal_in_finder_(out_path)
    }, error = function(e) {
      showNotification(paste("Export error:", e$message),
                       type = "error", duration = 10)
    })
  })

  # --- 7. Calculate RCA Adjustments & Download ---
  observeEvent(input$rca_output_btn, {
    req(gam_result_r(), data_mod$data())

    la_col <- resolve_living_area_()
    cqa_choices <- c("CQA" = "cqa")
    if (!is.null(la_col)) {
      cqa_choices <- c(cqa_choices, "CQA per SF" = "cqa_sf")
    }

    # Carry the subject-CQA type + value forward from earthUI ONLY when an
    # earthUI model has been imported. earthUI saves them to the shared regProj
    # settings; a Trilogy lock is used as a fallback. Without an earth import
    # the field starts blank (placeholder) so the analyst enters a value.
    cqa_type_pre <- NULL; cqa_value_pre <- NULL
    if (!is.null(tryCatch(earth_mod$knots(), error = function(e) NULL)) &&
        !is.null(rv_proj$active_project)) {
      cf_cqa <- earthUI::earth_carryforward_(
        rv_proj$active_project$project_path, input$purpose)
      cqa_type_pre  <- cf_cqa$cqa_type
      cqa_value_pre <- cf_cqa$cqa_value
      if (is.null(cqa_type_pre) && !is.null(getOption("mgcvUI.trilogy"))) {
        tl <- trilogy_get_lock(rv_proj$active_project$project_path)$shared
        cqa_type_pre  <- tl$cqa_mode
        tlv <- suppressWarnings(as.numeric(tl$cqa))
        if (length(tlv) == 1L && !is.na(tlv)) cqa_value_pre <- tlv
      }
    }
    cqa_sel <- cqa_type_pre %||% unname(cqa_choices[1])
    if (!cqa_sel %in% cqa_choices) cqa_sel <- unname(cqa_choices[1])
    cqa_val <- if (!is.null(cqa_value_pre) && !is.na(cqa_value_pre))
      as.character(cqa_value_pre) else ""

    cqa_sf_help <- tags$span(
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
      cqa_names <- c(cqa_names, list(tagList("CQA per SF", cqa_sf_help)))

    showModal(modalDialog(
      title = "RCA Raw Output \u2014 Subject CQA Score",
      radioButtons("rca_cqa_type", "Score type:",
                   choiceNames = cqa_names, choiceValues = unname(cqa_choices),
                   selected = cqa_sel, inline = TRUE),
      textInput("rca_cqa_value", "Enter CQA score for subject:",
                value = cqa_val,
                placeholder = "[Enter a CQA value between 0.00 and 10.00]"),
      tags$script(HTML("
        (function(){
          function rcaValidate(){
            var el = document.getElementById('rca_cqa_value');
            var btn = document.getElementById('export_rca');
            if(!el || !btn) return;
            var v = parseFloat(el.value);
            btn.disabled = !(!isNaN(v) && v >= 0 && v <= 10);
          }
          $(document).off('input.mgcv_rca').on('input.mgcv_rca', '#rca_cqa_value', rcaValidate);
          setTimeout(rcaValidate, 120);
        })();
      ")),
      footer = tagList(
        modalButton("Cancel"),
        actionButton("export_rca", "Generate", class = "btn-primary")
      ),
      easyClose = TRUE
    ))
  })

  observeEvent(input$export_rca, {
    # Validate the CQA score (0.00-10.00); keep the modal open and show an
    # error if it is blank or out of range.
    cqa_num <- suppressWarnings(as.numeric(input$rca_cqa_value))
    if (length(cqa_num) != 1L || is.na(cqa_num) || cqa_num < 0 || cqa_num > 10) {
      showNotification("Please enter a CQA score between 0.00 and 10.00.",
                       type = "error", duration = 6)
      return()
    }
    removeModal()
    req(gam_result_r(), data_mod$data())
    if (!requireNamespace("writexl", quietly = TRUE)) {
      showNotification(
        "Package 'writexl' required. Install with: install.packages('writexl')",
        type = "error", duration = 10)
      return()
    }

    folder <- local({
      .ap <- rv_proj$active_project
      if (!is.null(.ap))
        file.path(.ap$project_path, paste0(earthUI::os_detect(), "_out_mgcv"))
      else input$output_folder
    })
    if (is.null(folder) || !nzchar(folder)) folder <- path.expand("~/Downloads")
    if (!dir.exists(folder)) dir.create(folder, recursive = TRUE)

    tryCatch({
      export_df <- data_mod$data()
      res       <- gam_result_r()
      model     <- res$model
      response  <- res$response
      xform     <- res$response_transform %||% "none"
      user_cqa  <- cqa_num
      n         <- nrow(export_df)
      # Coerce a copy the same way fit_gam did so predict() works.
      pred_df   <- earthUI:::prepare_gam_data_(export_df, res$smooth_specs)

      if (n < 2L) {
        showNotification("Need at least 2 rows (subject + 1 comp).",
                         type = "error")
        return()
      }

      # Predictions (back-transform to original scale)
      predicted_log <- as.numeric(predict(model, newdata = pred_df,
                                          type = "response"))
      predicted <- earthUI:::back_transform_(predicted_log, xform)
      actual    <- export_df[[response]]
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
        comp_cqa_vals <- cqa_sf_col[-1L]
        comp_resid_for_interp <- resid_sf[-1L]
      } else {
        comp_cqa_vals <- cqa_col[-1L]
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

      if (use_sf && !is.null(la_col)) {
        export_df[["residual_sf"]][1L] <- round(subject_resid, 1)
      }

      # Handle weight-0 rows: use subject_value so RCA columns can be computed
      wt_col_name <- var_config_r()$weights_col
      zero_wt <- integer(0)
      if (!is.null(wt_col_name) && wt_col_name %in% names(export_df)) {
        wvals <- export_df[[wt_col_name]]
        zero_wt <- which(wvals == 0)
      }
      if (length(zero_wt) > 0L) {
        sv <- predicted[zero_wt] + subject_resid_total
        export_df[["subject_value"]][zero_wt] <- round(sv, 1)
        actual[zero_wt] <- sv
        residuals_val <- actual - predicted
        export_df[["residual"]][zero_wt] <- round(residuals_val[zero_wt], 1)
        if (!is.null(la_col) && la_col %in% names(export_df)) {
          la <- export_df[[la_col]]
          export_df[["residual_sf"]][zero_wt] <-
            round(residuals_val[zero_wt] / la[zero_wt], 1)
        }
      }

      # --- Per-term contributions ---
      contribs <- compute_gam_contributions_(model, pred_df)
      intercept <- stats::coef(model)[["(Intercept)"]]

      # Back-transform basis to dollar scale for display
      export_df[["basis"]] <- round(
        earthUI:::back_transform_(intercept, xform), 1)

      clean_term_name_ <- function(tl) {
        nm <- gsub("^s\\((.+)\\)$", "\\1", tl)
        nm <- gsub("^te\\((.+)\\)$", "\\1", nm)
        nm <- gsub("^ti\\((.+)\\)$", "\\1", nm)
        nm
      }

      # For log-transformed models: convert log-scale contributions
      # to dollar-scale value contributions using the linearized approach.
      # Each comp's dollar contribution for a term =
      #   predicted_dollars * (10^contrib_i - 1) / (10^sum_contribs - 1)
      #   ... simplified: we use back_transform on cumulative sums.
      # Simpler approach: contribution_dollars = predicted * contrib_log / sum_log
      # Best approach: for each row, dollar VC = back_transform(intercept + contribs)
      #   allocated proportionally.

      if (xform != "none") {
        # Log-scale total per row
        log_total <- intercept + Reduce(`+`, contribs)
        dollar_pred <- earthUI:::back_transform_(log_total, xform)
        # Baseline: predicted value with zero contribution from all smooths
        dollar_base <- earthUI:::back_transform_(intercept, xform)
        # Total dollar effect of all terms
        dollar_effect <- dollar_pred - dollar_base

        # Proportional allocation: each term's dollar contribution is its
        # share of the log-scale sum × the total dollar effect.
        # This avoids the multicollinearity blow-up from leave-one-out.
        log_sum <- Reduce(`+`, contribs)
        for (tl in names(contribs)) {
          col_name <- paste0(clean_term_name_(tl), "_contribution")
          # Proportion of log-scale total attributable to this term
          share <- ifelse(abs(log_sum) > 1e-10,
                          contribs[[tl]] / log_sum, 0)
          export_df[[col_name]] <- round(dollar_effect * share, 1)
        }
      } else {
        for (tl in names(contribs)) {
          col_name <- paste0(clean_term_name_(tl), "_contribution")
          export_df[[col_name]] <- round(contribs[[tl]], 1)
        }
      }

      # --- RCA Adjustments (always in dollars) ---
      adj_sum   <- rep(0, n)
      gross_sum <- rep(0, n)

      if (xform != "none") {
        # Adjustments: subject's contribution minus comp's contribution
        # Use proportional allocation (same method as contributions above)
        log_total <- intercept + Reduce(`+`, contribs)
        dollar_pred <- earthUI:::back_transform_(log_total, xform)
        dollar_base <- earthUI:::back_transform_(intercept, xform)
        dollar_effect <- dollar_pred - dollar_base
        log_sum <- Reduce(`+`, contribs)

        for (tl in names(contribs)) {
          adj_col_name <- paste0(clean_term_name_(tl), "_adjustment")
          share <- ifelse(abs(log_sum) > 1e-10,
                          contribs[[tl]] / log_sum, 0)
          dollar_contrib <- dollar_effect * share
          # Adjustment = subject's dollar contribution - comp's
          adjustment <- dollar_contrib[1L] - dollar_contrib
          export_df[[adj_col_name]] <- round(adjustment, 1)
          adj_sum   <- adj_sum + ifelse(is.na(adjustment), 0, adjustment)
          gross_sum <- gross_sum + ifelse(is.na(adjustment), 0, abs(adjustment))
        }
      } else {
        for (tl in names(contribs)) {
          adj_col_name <- paste0(clean_term_name_(tl), "_adjustment")
          subject_contrib <- contribs[[tl]][1L]
          adjustment <- subject_contrib - contribs[[tl]]
          export_df[[adj_col_name]] <- round(adjustment, 1)
          adj_sum   <- adj_sum + ifelse(is.na(adjustment), 0, adjustment)
          gross_sum <- gross_sum + ifelse(is.na(adjustment), 0, abs(adjustment))
        }
      }

      # Residual adjustment (always in dollars)
      resid_adj <- subject_resid_total - residuals_val
      export_df[["residual_adjustment"]] <- round(resid_adj, 1)
      adj_sum   <- adj_sum + ifelse(is.na(resid_adj), 0, resid_adj)
      gross_sum <- gross_sum + ifelse(is.na(resid_adj), 0, abs(resid_adj))

      export_df[["net_adjustments"]]   <- round(adj_sum, 1)
      export_df[["gross_adjustments"]] <- round(gross_sum, 1)

      sale_price <- export_df[[response]]
      export_df[["residual_adj_pct"]] <- round(resid_adj / sale_price * 100, 2)
      export_df[["net_adj_pct"]]      <- round(adj_sum / sale_price * 100, 2)
      export_df[["gross_adj_pct"]]    <- round(gross_sum / sale_price * 100, 2)

      export_df[["adjusted_sale_price"]] <- round(actual + adj_sum, 1)

      # Subject row: adjustments are zero (subject vs self)
      adj_cols <- grep(
        "_adjustment$|net_adjustments|gross_adjustments|adjusted_sale_price|_adj_pct$",
        names(export_df), value = TRUE)
      for (ac in adj_cols) {
        export_df[[ac]][1L] <- NA_real_
      }
      export_df[["adjusted_sale_price"]][1L] <- round(subject_est, 1)

      # Store pct data for RCA Analysis plots (comps only, exclude subject)
      rv_rca$pct_data <- data.frame(
        residual_adj_pct = export_df[["residual_adj_pct"]][-1L],
        net_adj_pct      = export_df[["net_adj_pct"]][-1L],
        gross_adj_pct    = export_df[["gross_adj_pct"]][-1L],
        stringsAsFactors = FALSE
      )
      rv_rca$rca_df <- export_df

      # Move ranking columns to the left
      rank_cols <- c("residual_sf", "cqa_sf", "residual", "cqa")
      rank_cols <- rank_cols[rank_cols %in% names(export_df)]
      if (length(rank_cols) > 0L) {
        other_cols <- setdiff(names(export_df), rank_cols)
        export_df <- export_df[, c(rank_cols, other_cols), drop = FALSE]
      }

      base <- tools::file_path_sans_ext(data_mod$filename() %||% "mgcvui")
      out_path <- file.path(folder, paste0(base, "_adjusted_",
                            earthUI:::fit_stamp_(gam_result_r()$fit_ts), ".xlsx"))
      writexl::write_xlsx(export_df, out_path)
      # Trilogy mode: emit this method's value conclusion (subject's indicated
      # value, row 1 of adjusted_sale_price) for the combined report.
      if (!is.null(getOption("mgcvUI.trilogy"))) {
        m <- earthUI:::trilogy_fit_metrics_(export_df[[response]][-1L],
                                           residuals_val[-1L])
        try(earthUI::trilogy_write_conclusion(
          folder, "mgcv", earthUI:::fit_stamp_(gam_result_r()$fit_ts),
          subject_value = export_df[["adjusted_sale_price"]][1L],
          metrics = m), silent = TRUE)
      }
      session$sendCustomMessage("download_check",
                                list(id = "rca_output_btn"))
      showNotification(paste0("RCA output saved to: ", out_path),
                       type = "message", duration = 8)
    }, error = function(e) {
      showNotification(paste("RCA error:", e$message),
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
    font_fam   <- mgcv_font_family_()

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
      ggplot2::theme_minimal(base_size = 15, base_family = font_fam) +
      # The plot canvas stays white in both light and dark mode, so all text
      # is forced dark explicitly — theme-following text turns light grey in
      # dark mode and becomes unreadable on the white panel.
      ggplot2::theme(
        plot.title = ggplot2::element_text(size = 16, face = "bold",
                                           color = "black"),
        plot.subtitle = ggplot2::element_text(size = 12, color = "#4c566a"),
        axis.text  = ggplot2::element_text(size = 15, color = "black"),
        axis.title = ggplot2::element_text(size = 16, color = "black"),
        axis.text.x = ggplot2::element_text(angle = 0, size = 15,
                                            color = "black")
      )
  }

  output$rca_resid_pct_plot <- renderPlot({
    req(rv_rca$pct_data)
    rca_pct_histogram_(rv_rca$pct_data$residual_adj_pct,
                       "Residual Adjustment %",
                       "Residual Adj. %", "#88c0d0")
  })

  output$rca_net_pct_plot <- renderPlot({
    req(rv_rca$pct_data)
    rca_pct_histogram_(rv_rca$pct_data$net_adj_pct,
                       "Net Adjustment %",
                       "Net Adj. %", "#5e81ac")
  })

  output$rca_gross_pct_plot <- renderPlot({
    req(rv_rca$pct_data)
    rca_pct_histogram_(rv_rca$pct_data$gross_adj_pct,
                       "Gross Adjustment %",
                       "Gross Adj. %", "#a3be8c")
  })

  # --- 9. Generate Sales Grid & Download ---
  # Step 1: Button click shows modal with recommended comps
  observeEvent(input$sales_grid_btn, {
    req(rv_rca$rca_df)
    rca <- rv_rca$rca_df
    n_total <- nrow(rca)
    if (n_total < 2) {
      showNotification("Need at least 2 rows (subject + 1 comp).",
                       type = "error", duration = 8)
      return()
    }

    has_gross_pct <- "gross_adjustments" %in% colnames(rca) &&
                     "sale_price" %in% colnames(rca)

    # Find sale_age column from specials
    cfg <- var_config_r()
    sg_specials <- cfg$specials

    # Warn about missing special types needed for the sales grid
    required_specials <- c("contract_date", "living_area")
    recommended_specials <- c("latitude", "longitude", "lot_size")
    designated <- if (!is.null(sg_specials)) unique(sg_specials) else character(0)
    missing_req <- setdiff(required_specials, designated)
    missing_rec <- setdiff(recommended_specials, designated)
    if (length(missing_req) > 0L) {
      showNotification(
        paste0("Sales Grid: required special types not designated: ",
               paste(missing_req, collapse = ", "),
               ". Set these in Variable Configuration > Special column."),
        type = "warning", duration = 15)
    }
    if (length(missing_rec) > 0L) {
      showNotification(
        paste0("Sales Grid: recommended special types not designated: ",
               paste(missing_rec, collapse = ", "),
               ". Grid will work but some fields will be blank."),
        type = "message", duration = 10)
    }
    sa_col_name <- "sale_age"
    if (!is.null(sg_specials)) {
      sa_idx <- which(sg_specials == "sale_age")
      if (length(sa_idx) > 0L) sa_col_name <- names(sg_specials)[sa_idx[1L]]
    }

    wt_col <- if (!is.null(cfg$weights_col) && cfg$weights_col %in% colnames(rca))
      rca[[cfg$weights_col]] else rep(1, n_total)

    # Build comp info table (rows 2..n_total with weight > 0)
    response <- cfg$response
    comp_info <- data.frame(
      row       = 2:n_total,
      address   = if ("street_address" %in% colnames(rca))
                    rca[["street_address"]][2:n_total] else rep("", n_total - 1),
      sale_price = if (response %in% colnames(rca))
                     rca[[response]][2:n_total] else rep(NA, n_total - 1),
      sale_age  = if (sa_col_name %in% colnames(rca))
                    rca[[sa_col_name]][2:n_total] else rep(NA, n_total - 1),
      weight    = wt_col[2:n_total],
      gross_adj = if ("gross_adjustments" %in% colnames(rca))
                    rca[["gross_adjustments"]][2:n_total] else rep(0, n_total - 1),
      stringsAsFactors = FALSE
    )

    comp_info$gross_adj_pct <- ifelse(
      !is.na(comp_info$sale_price) & comp_info$sale_price != 0,
      abs(comp_info$gross_adj / comp_info$sale_price),
      NA
    )

    eligible <- comp_info[!is.na(comp_info$weight) & comp_info$weight > 0, ]
    eligible <- eligible[order(eligible$gross_adj_pct, na.last = TRUE), ]

    recommended <- eligible[!is.na(eligible$gross_adj_pct) &
                            eligible$gross_adj_pct < 0.25, ]
    recommended <- recommended[order(recommended$sale_age, na.last = TRUE), ]
    if (nrow(recommended) > 30) recommended <- recommended[1:30, ]

    others <- eligible[is.na(eligible$gross_adj_pct) |
                       eligible$gross_adj_pct >= 0.25, ]
    others <- others[order(others$gross_adj_pct, na.last = TRUE), ]

    rv_rca$sg_recommended <- recommended
    rv_rca$sg_others <- others

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
      title = "Sales Grid \u2014 Select Comparables (max 30)",
      size = "l",
      tags$div(
        style = "max-height: 500px; overflow-y: auto;",
        tags$h5(paste0("Recommended Comps (gross adj < 25%, ",
                       "sorted by sale age) \u2014 ",
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
    req(rv_rca$rca_df)
    removeModal()

    all_candidate_rows <- c(
      if (!is.null(rv_rca$sg_recommended) && nrow(rv_rca$sg_recommended) > 0)
        rv_rca$sg_recommended$row else integer(0),
      if (!is.null(rv_rca$sg_others) && nrow(rv_rca$sg_others) > 0)
        rv_rca$sg_others$row[seq_len(min(nrow(rv_rca$sg_others), 50))]
        else integer(0)
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
    rca <- rv_rca$rca_df
    response <- var_config_r()$response
    sp <- if (response %in% colnames(rca)) rca[[response]][comp_rows]
          else rep(NA, length(comp_rows))
    gross <- if ("gross_adjustments" %in% colnames(rca))
               rca[["gross_adjustments"]][comp_rows] else rep(0, length(comp_rows))
    gap <- ifelse(!is.na(sp) & sp != 0, abs(gross / sp), NA)
    comp_rows <- comp_rows[order(gap, na.last = TRUE)]

    folder <- local({
      .ap <- rv_proj$active_project
      if (!is.null(.ap))
        file.path(.ap$project_path, paste0(earthUI::os_detect(), "_out_mgcv"))
      else input$output_folder
    })
    if (is.null(folder) || !nzchar(folder)) folder <- path.expand("~/Downloads")
    if (!dir.exists(folder)) dir.create(folder, recursive = TRUE)

    out_path <- file.path(folder, paste0("SalesGrid_",
                          format(Sys.time(), "%Y%m%d_%H%M%S"), ".xlsx"))

    message("mgcvUI: Sales grid with ", length(comp_rows),
            " comps (rows: ", paste(comp_rows, collapse = ","), ")")

    tryCatch({
      tmp_adj <- tempfile(fileext = ".xlsx")
      # Ensure all columns are atomic (no list columns)
      rca_export <- rv_rca$rca_df
      for (cn in names(rca_export)) {
        if (is.list(rca_export[[cn]])) {
          rca_export[[cn]] <- vapply(rca_export[[cn]], paste, character(1),
                                     collapse = ",")
        }
      }
      writexl::write_xlsx(rca_export, tmp_adj)
      message("mgcvUI: wrote RCA temp file: ", tmp_adj,
              " (", nrow(rca_export), " rows, ", ncol(rca_export), " cols)")

      grid_script <- system.file("app_mgcv", "sales_grid.R", package = "earthUI")
      if (!nzchar(grid_script)) {
        showNotification("Sales grid script not found in package.",
                         type = "error", duration = 10)
        return()
      }
      source(grid_script, local = TRUE)

      # Build specials named list from designations
      cfg <- var_config_r()
      sg_specials_map <- list()
      if (!is.null(cfg$specials)) {
        for (nm in names(cfg$specials)) {
          sp_type <- cfg$specials[[nm]]
          if (sp_type != "no") sg_specials_map[[sp_type]] <- nm
        }
      }

      n_comp <- length(comp_rows)
      withProgress(
        message = "Generating Sales Grid",
        detail = sprintf("0 of %d comps processed", n_comp),
        value = 0, {
        generate_sales_grid(
          adjusted_file     = tmp_adj,
          comp_rows         = comp_rows,
          output_file       = out_path,
          specials          = sg_specials_map,
          progress_fn       = function(sheet, total_sheets,
                                       comps_done, total_comps) {
            setProgress(
              value = comps_done / total_comps,
              detail = sprintf("Sheet %d of %d \u2014 %d of %d comps processed",
                               sheet, total_sheets, comps_done, total_comps))
          }
        )
      })
      unlink(tmp_adj)

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

  # --- 10. Download Report (to output folder) ---
  # --- 10. Generate Quarto Report (writes a self-contained .qmd bundle) ---
  observeEvent(input$generate_qmd_btn, {
    req(gam_result_r())
    folder <- local({
      .ap <- rv_proj$active_project
      if (!is.null(.ap))
        file.path(.ap$project_path, paste0(earthUI::os_detect(), "_out_mgcv"))
      else input$output_folder
    })
    if (is.null(folder) || !nzchar(folder)) {
      showNotification("Open a project first (output folder not set).",
                       type = "error", duration = 6); return()
    }
    if (!dir.exists(folder)) dir.create(folder, recursive = TRUE, showWarnings = FALSE)
    # Stamp the bundle with the fit time so the .qmd and everything it renders
    # to (HTML/Word/PDF) share this fit's timestamp (groups a fit's files; lets
    # a Trilogy run gather them).
    base <- paste0(
      tools::file_path_sans_ext(data_mod$filename() %||% "gam_report"),
      "_", earthUI:::fit_stamp_(gam_result_r()$fit_ts))
    showNotification("Generating Quarto report bundle...", type = "message",
                     duration = 3, id = "qmd_progress")
    tryCatch({
      qmd_path <- generate_quarto_report_mgcv(gam_result_r(), dest_dir = folder,
                                         base = base)
      updateTextInput(session, "convert_qmd_path", value = qmd_path)
      session$sendCustomMessage("download_check", list(id = "generate_qmd_btn"))
      showNotification(paste0("Quarto bundle written to: ", dirname(qmd_path)),
                       type = "message", duration = 8)
    }, error = function(e) {
      showNotification(paste("Generate error:", e$message),
                       type = "error", duration = 12)
    })
  })

  # --- 11. Convert Quarto Report (any .qmd) -> HTML/Word/PDF ---
  shinyFiles::shinyFileChoose(input, "convert_qmd_browse", roots = volumes,
                              session = session, filetypes = c("qmd"))
  observeEvent(input$convert_qmd_browse, {
    sel <- shinyFiles::parseFilePaths(volumes, input$convert_qmd_browse)
    if (nrow(sel) >= 1L && nzchar(sel$datapath[1L]))
      updateTextInput(session, "convert_qmd_path",
                      value = as.character(sel$datapath[1L]))
  })

  observeEvent(input$convert_qmd_btn, {
    qmd <- trimws(input$convert_qmd_path %||% "")
    if (!nzchar(qmd) || !file.exists(qmd)) {
      showNotification("Pick a valid .qmd file first.", type = "error",
                       duration = 6); return()
    }
    fmts <- input$convert_formats
    if (length(fmts) == 0L) {
      showNotification("Select at least one output format.", type = "error",
                       duration = 6); return()
    }
    paper <- earthUI:::locale_paper_()
    showNotification("Rendering report...", type = "message", duration = 3,
                     id = "convert_progress")
    tryCatch({
      out_paths <- convert_quarto_file(qmd, formats = fmts, paper_size = paper)
      session$sendCustomMessage("download_check", list(id = "convert_qmd_btn"))
      showNotification(paste0("Wrote ", length(out_paths), " file(s):\n",
                              paste(basename(out_paths), collapse = "\n")),
                       type = "message", duration = 12)
    }, error = function(e) {
      showNotification(paste("Convert error:", e$message),
                       type = "error", duration = 15)
    })
  })
}

shinyApp(ui, server)
