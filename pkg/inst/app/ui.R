fluidPage(
  withMathJax(),
  theme = nord_light,
  tags$head(
    tags$link(rel = "icon", type = "image/png", href = "favicon.png"),
    tags$script(type = "text/x-mathjax-config", HTML("
      MathJax.Hub.Config({
        'HTML-CSS': { preferredFont: 'TeX', scale: 90 }
      });
    ")),
    tags$style(HTML("
    .dataTable td, .dataTable th { padding: 4px 8px !important; }
    .dataTables_wrapper { font-size: 0.9em; overflow-x: auto; }
    .dataTable td { max-width: 300px; overflow: hidden; text-overflow: ellipsis; white-space: nowrap; cursor: pointer; }
    #eui-cell-popup { display: none; }
    .eui-popup-backdrop { position: fixed; top: 0; left: 0; width: 100%; height: 100%; background: rgba(0,0,0,0.3); z-index: 9998; }
    .eui-popup-content { position: fixed; top: 50%; left: 50%; transform: translate(-50%, -50%); background: var(--bs-body-bg, #eceff4); color: var(--bs-body-color, #2e3440); border-radius: 8px; padding: 20px; max-width: 80vw; max-height: 70vh; overflow: auto; z-index: 9999; box-shadow: 0 4px 20px rgba(0,0,0,0.3); }
    .eui-popup-content pre { white-space: pre-wrap; word-wrap: break-word; margin-bottom: 12px; font-size: 0.9em; }
    .eui-popup-close { float: right; }
    .MathJax_Display { text-align: left !important; margin-left: 1em !important; }
    .eui-param-help { position: absolute; top: 0; right: 0; width: 18px; height: 18px; border-radius: 50%; background: #88c0d0; color: #fff; font-size: 11px; font-weight: bold; text-align: center; line-height: 18px; cursor: pointer; z-index: 10; }
    .eui-param-help:hover { background: #5e81ac; }
    #eui-theme-toggle { position: fixed; top: 12px; right: 20px; z-index: 10000; width: 38px; height: 38px; border-radius: 50%; border: 2px solid var(--bs-border-color); background: var(--bs-body-bg); color: var(--bs-body-color); font-size: 18px; cursor: pointer; display: flex; align-items: center; justify-content: center; box-shadow: 0 2px 6px rgba(0,0,0,0.15); transition: all 0.3s; }
    #eui-theme-toggle:hover { box-shadow: 0 2px 10px rgba(0,0,0,0.25); }
    [data-bs-theme='dark'] #eui-theme-toggle { background: #3b4252; border-color: #434c5e; }
    [data-bs-theme='dark'] .eui-popup-content { background: #3b4252; color: #d8dee9; }
    [data-bs-theme='dark'] details > summary { color: #d8dee9 !important; }
    [data-bs-theme='dark'] .nav-tabs .nav-link.active { color: #d8dee9 !important; background-color: #2e3440 !important; border-color: #434c5e #434c5e #2e3440 !important; }
    [data-bs-theme='dark'] .nav-tabs .nav-link { color: #81a1c1; }
    [data-bs-theme='dark'] .nav-tabs .nav-link:hover { color: #d8dee9; border-color: #434c5e; }
    #earth_output { font-family: 'Roboto Condensed', sans-serif; font-size: 0.9em; line-height: 1.5; }
    .eui-matrix-header th { position: sticky; top: 0; z-index: 2; background: var(--bs-body-bg, #fff); }
    .eui-matrix-header th:first-child { position: sticky; left: 0; z-index: 3; }
    .eui-matrix-rowlabel { position: sticky; left: 0; z-index: 1; background: var(--bs-body-bg, #fff); }
    .eui-type-select { appearance: auto; -webkit-appearance: auto; }
    [data-bs-theme='dark'] .eui-type-select { background: #3b4252 !important; color: #d8dee9 !important; border-color: #434c5e !important; }
    .eui-special-select { appearance: auto; -webkit-appearance: auto; }
    [data-bs-theme='dark'] .eui-special-select { background: #3b4252 !important; color: #d8dee9 !important; border-color: #434c5e !important; }
    details.eui-section > summary { cursor: pointer; list-style: none; }
    details.eui-section > summary::-webkit-details-marker { display: none; }
    details.eui-section > summary h4 { display: inline; }
    details.eui-section > summary::before { content: '\\25B6'; margin-right: 6px; font-size: 0.75em; transition: transform 0.2s; display: inline-block; }
    details.eui-section[open] > summary::before { transform: rotate(90deg); }
    .radio-inline { margin-right: 16px; }
    .shiny-input-radiogroup input[type='radio'] { appearance: none; -webkit-appearance: none; width: 18px; height: 18px; border: 3px solid #2e3440; border-radius: 50%; margin-right: 5px; vertical-align: middle; cursor: pointer; position: relative; }
    .shiny-input-radiogroup input[type='radio']:checked { border-color: #2e3440; }
    .shiny-input-radiogroup input[type='radio']:checked::after { content: ''; position: absolute; top: 2px; left: 2px; width: 8px; height: 8px; border-radius: 50%; background: #2e3440; }
    [data-bs-theme='dark'] .shiny-input-radiogroup input[type='radio'] { border-color: #d8dee9; }
    [data-bs-theme='dark'] .shiny-input-radiogroup input[type='radio']:checked { border-color: #d8dee9; }
    [data-bs-theme='dark'] .shiny-input-radiogroup input[type='radio']:checked::after { background: #d8dee9; }
    [data-bs-theme='dark'] .modal .btn-outline-primary { color: #88c0d0; border-color: #88c0d0; }
    [data-bs-theme='dark'] .modal .btn-outline-primary:hover { background: #88c0d0; color: #2e3440; }
    [data-bs-theme='dark'] .modal .btn-outline-secondary { color: #81a1c1; border-color: #81a1c1; }
    [data-bs-theme='dark'] .modal .btn-outline-secondary:hover { background: #81a1c1; color: #2e3440; }
    [data-bs-theme='dark'] .modal .btn-outline-danger { color: #bf616a; border-color: #bf616a; }
    [data-bs-theme='dark'] .modal .btn-outline-danger:hover { background: #bf616a; color: #eceff4; }
  "))),
  tags$script(HTML("
    $(document).on('shiny:connected', function() {
      function initPopovers() {
        var els = document.querySelectorAll('[data-bs-toggle=\"popover\"]');
        els.forEach(function(el) {
          if (!bootstrap.Popover.getInstance(el)) {
            new bootstrap.Popover(el, { html: false, container: 'body' });
          }
        });
      }
      initPopovers();
      var obs = new MutationObserver(function() { setTimeout(initPopovers, 200); });
      obs.observe(document.body, { childList: true, subtree: true });
    });
  ")),
  tags$button(id = "eui-theme-toggle", onclick = "toggleTheme()", HTML("&#9790;")),
  tags$script(HTML("
    var euiCurrentMode = 'light';

    function toggleTheme() {
      euiCurrentMode = (euiCurrentMode === 'dark') ? 'light' : 'dark';
      Shiny.setInputValue('dark_mode', euiCurrentMode, {priority: 'event'});
      var btn = document.getElementById('eui-theme-toggle');
      if (btn) btn.innerHTML = (euiCurrentMode === 'dark') ? '\\u2600' : '\\u263E';
      try { localStorage.setItem('earthUI_theme', euiCurrentMode); } catch(e) {}
    }

    $(document).on('shiny:connected', function() {
      var saved = null;
      try { saved = localStorage.getItem('earthUI_theme'); } catch(e) {}
      if (saved === 'dark') {
        euiCurrentMode = 'dark';
        var btn = document.getElementById('eui-theme-toggle');
        if (btn) btn.innerHTML = '\\u2600';
        Shiny.setInputValue('dark_mode', 'dark', {priority: 'event'});
      } else {
        Shiny.setInputValue('dark_mode', 'light', {priority: 'event'});
      }
    });
  ")),
  tags$script(HTML("
    // --- Checkmark helper ---
    function addCheck(btnId) {
      var $btn = $('#' + btnId);
      if ($btn.length && !$btn.find('.eui-check').length) {
        $btn.append(' <span class=\"eui-check\" style=\"color:#fff;\">&#10003;</span>');
      }
    }
    function removeCheck(btnId) {
      $('#' + btnId + ' .eui-check').remove();
    }

    Shiny.addCustomMessageHandler('fitting_start', function(msg) {
      // Remove any previous overlays and checkmarks
      $('#eui-fitting-modal').remove();
      clearInterval(window.euiTimerInterval);
      removeCheck('run_model');
      removeCheck('export_data');
      removeCheck('export_data_nonadj');
      removeCheck('rca_output_btn');

      var start = Date.now();
      var modal = $(
        '<div id=\"eui-fitting-modal\" style=\"position:fixed;top:50%;left:50%;transform:translate(-50%,-50%);z-index:10001;' +
        'background:#2e3440;color:#d8dee9;border-radius:8px;box-shadow:0 4px 24px rgba(0,0,0,0.5);' +
        'width:520px;max-width:90vw;font-family:monospace;overflow:hidden;\">' +
          '<div style=\"background:#3b4252;padding:10px 16px;display:flex;justify-content:space-between;align-items:center;\">' +
            '<span style=\"font-size:0.95em;font-weight:bold;\">Fitting Earth Model</span>' +
            '<span id=\"eui-timer\" style=\"font-size:0.85em;color:#81a1c1;margin-right:30px;\">0s</span>' +
          '</div>' +
          '<div id=\"eui-trace-log\" style=\"padding:8px 12px;height:300px;overflow-y:auto;font-size:0.78em;line-height:1.5;\"></div>' +
        '</div>'
      );
      // Add semi-transparent backdrop
      $('<div id=\"eui-fitting-backdrop\" style=\"position:fixed;top:0;left:0;width:100%;height:100%;' +
        'background:rgba(0,0,0,0.4);z-index:10000;\"></div>').appendTo('body');
      modal.appendTo('body');

      // Add initial message
      $('#eui-trace-log').append($('<div style=\"color:#88c0d0;\">').text('Starting model fit...'));

      window.euiTimerInterval = setInterval(function() {
        var s = Math.floor((Date.now() - start) / 1000);
        var m = Math.floor(s / 60);
        var label = m > 0 ? m + 'm ' + (s % 60) + 's' : s + 's';
        $('#eui-timer').text(label);
      }, 1000);
    });

    Shiny.addCustomMessageHandler('trace_line', function(msg) {
      var $log = $('#eui-trace-log');
      if ($log.length) {
        var color = '#a3be8c';
        if (msg.text.match(/^\\s*(CV|cross)/i)) color = '#ebcb8b';
        else if (msg.text.match(/error|fail/i)) color = '#bf616a';
        var $line = $('<div style=\"color:' + color + ';\">').text(msg.text);
        $log.append($line);
        $log.scrollTop($log[0].scrollHeight);
      }
    });

    Shiny.addCustomMessageHandler('fitting_done', function(msg) {
      clearInterval(window.euiTimerInterval);
      var $log = $('#eui-trace-log');
      var hasError = msg.text === 'Error' || $log.find('div').filter(function() {
        return $(this).text().match(/error|fail/i);
      }).length > 0;
      var color = hasError ? '#bf616a' : '#a3be8c';
      $log.append($('<div style=\"color:' + color + ';font-weight:bold;margin-top:4px;\">').text(msg.text));
      $log.scrollTop($log[0].scrollHeight);
      if (!hasError) addCheck('run_model');
      // Add close X button — user dismisses manually
      if (!$('#eui-fitting-close').length) {
        var $btn = $('<span id=\"eui-fitting-close\" style=\"position:absolute;top:8px;right:12px;' +
          'color:#81a1c1;cursor:pointer;font-size:1.3em;line-height:1;\">&times;</span>');
        $btn.on('click', function() {
          $('#eui-fitting-modal, #eui-fitting-backdrop').fadeOut(300, function(){ $(this).remove(); });
        });
        $('#eui-fitting-modal').css('position', 'fixed').append($btn);
      }
    });

    // Check download buttons on completion
    Shiny.addCustomMessageHandler('download_check', function(msg) {
      addCheck(msg.id);
    });

    // --- wp display, button state, and persistence ---
    Shiny.addCustomMessageHandler('update_wp_display', function(msg) {
      $('#wp_display').text(msg.text);
    });
    Shiny.addCustomMessageHandler('wp_btn_state', function(msg) {
      if (msg.disabled) {
        $('#wp_set_btn').prop('disabled', true).addClass('disabled');
      } else {
        $('#wp_set_btn').prop('disabled', false).removeClass('disabled');
      }
    });
    Shiny.addCustomMessageHandler('save_wp_weights', function(msg) {
      try {
        localStorage.setItem('earthUI_wp_' + msg.filename, JSON.stringify(msg.weights));
      } catch(e) {}
    });
    // Restore wp weights when target selectize changes
    $(document).on('change', '#target + .selectize-control', function() {
      setTimeout(function() {
        var fn = window.euiCurrentFilename || 'default';
        try {
          var saved = JSON.parse(localStorage.getItem('earthUI_wp_' + fn));
          if (saved && Object.keys(saved).length > 0) {
            Shiny.setInputValue('wp_weights_restored', saved, {priority: 'event'});
          }
        } catch(e) {}
      }, 500);
    });

    // --- SQLite settings bridge ---

    // Restore settings from SQLite into localStorage (and optionally apply to DOM)
    Shiny.addCustomMessageHandler('restore_all_settings', function(msg) {
      var fn = msg.filename;
      window.euiCurrentFilename = fn;
      if (msg.settings && Object.keys(msg.settings).length > 0) {
        try { localStorage.setItem('earthUI_settings_' + fn, JSON.stringify(msg.settings)); } catch(e) {}
      }
      if (msg.variables && Object.keys(msg.variables).length > 0) {
        try { localStorage.setItem('earthUI_vars_' + fn, JSON.stringify(msg.variables)); } catch(e) {}
      }
      if (msg.interactions && Object.keys(msg.interactions).length > 0) {
        try { localStorage.setItem('earthUI_interactions_' + fn, JSON.stringify(msg.interactions)); } catch(e) {}
      }

      // If apply flag set, push settings directly into Shiny inputs
      if (msg.apply) {
        setTimeout(function() {
          // Apply model parameters
          if (msg.settings) {
            var s = msg.settings;
            // Selectize inputs
            ['target','degree','pmethod','glm_family','trace','varmod_method'].forEach(function(id) {
              if (s[id] !== undefined) {
                var el = document.getElementById(id);
                if (el && el.selectize) {
                  if (id === 'target' && Array.isArray(s[id])) {
                    var valid = s[id].filter(function(v) { return el.selectize.options[v]; });
                    if (valid.length > 0) el.selectize.setValue(valid, true);
                  } else {
                    el.selectize.setValue(s[id], true);
                  }
                }
              }
            });
            // Numeric inputs
            ['nprune','thresh','penalty','minspan','endspan','fast_k','nfold_override',
             'nk','newvar_penalty','fast_beta','ncross','varmod_exponent','varmod_conv',
             'varmod_clamp','varmod_minspan','adjust_endspan','exhaustive_tol',
             'output_folder'].forEach(function(id) {
              if (s[id] !== undefined) $('#' + id).val(s[id]).trigger('change');
            });
            // Radio button inputs
            if (s['purpose'] !== undefined) {
              $('input[name=purpose][value=' + s['purpose'] + ']').prop('checked', true).trigger('change');
            }
            // Checkbox inputs
            ['stratify','keepxy','scale_y','auto_linpreds','use_beta_cache',
             'force_xtx_prune','get_leverages','force_weights',
             'skip_subject_row'].forEach(function(id) {
              if (s[id] !== undefined) $('#' + id).prop('checked', s[id]).trigger('change');
            });
            // Date inputs (Shiny dateInput wraps <input> in a <div>)
            ['effective_date'].forEach(function(id) {
              if (s[id] !== undefined && s[id] !== null) {
                var $inp = $('#' + id + ' input');
                if ($inp.length) { $inp.val(s[id]).trigger('change'); }
                else { $('#' + id).val(s[id]).trigger('change'); }
              }
            });
          }
          // Apply variable checkboxes
          if (msg.variables) {
            var v = msg.variables;
            var cols = [];
            $('[id^=eui_inc_]').each(function() { cols.push(this.id.replace('eui_inc_','')); });
            // Get column names from the table
            var $rows = $('#variable_table .eui-var-cb');
            // Simpler: trigger restore by re-reading localStorage
            var storageKey = 'earthUI_vars_' + fn;
            var saved = null;
            try { saved = JSON.parse(localStorage.getItem(storageKey)); } catch(e) {}
            if (saved) {
              var n = $('[id^=eui_inc_]').length;
              for (var i = 1; i <= n; i++) {
                var colName = $('#eui_inc_' + i).closest('tr').find('td:first').text().trim();
                if (!colName) continue;
                var sv = saved[colName];
                if (sv) {
                  $('#eui_inc_' + i).prop('checked', sv.inc);
                  $('#eui_fac_' + i).prop('checked', sv.fac);
                  $('#eui_lin_' + i).prop('checked', sv.lin);
                  if (sv.type) {
                    $('#eui_type_' + i).val(sv.type);
                  }
                  if (sv.special) {
                    $('#eui_special_' + i).val(sv.special);
                  }
                }
              }
              $('.eui-var-cb').first().trigger('change');
            }
          }
          // Apply interaction matrix
          if (msg.interactions) {
            var storageKey2 = 'earthUI_interactions_' + fn;
            var saved2 = null;
            try { saved2 = JSON.parse(localStorage.getItem(storageKey2)); } catch(e) {}
            if (saved2) {
              for (var key in saved2) {
                var $cb = $('#allowed_' + key);
                if ($cb.length) $cb.prop('checked', saved2[key]);
              }
              $('.eui-interaction-cb').first().trigger('change');
            }
          }
        }, 300);
      }
    });

    // Debounced save-to-server: collect localStorage and send to R/SQLite
    window.euiSaveTimer = null;
    window.euiSaveToServer = function(fn) {
      clearTimeout(window.euiSaveTimer);
      window.euiSaveTimer = setTimeout(function() {
        var payload = { filename: fn, settings: null, variables: null, interactions: null };
        try { payload.settings     = localStorage.getItem('earthUI_settings_' + fn); } catch(e) {}
        try { payload.variables    = localStorage.getItem('earthUI_vars_' + fn); } catch(e) {}
        try { payload.interactions = localStorage.getItem('earthUI_interactions_' + fn); } catch(e) {}
        Shiny.setInputValue('eui_save_trigger', payload, {priority: 'deferred'});
      }, 2000);
    };

    // Apply earth() factory defaults to all parameter inputs
    Shiny.addCustomMessageHandler('apply_earth_defaults', function(msg) {
      // Selectize inputs (includes new params 1-4)
      var selects = {
        weights_col: 'null',
        degree: '1', pmethod: 'backward', glm_family: 'none',
        trace: '0', varmod_method: 'lm'
      };
      for (var id in selects) {
        var el = document.getElementById(id);
        if (el && el.selectize) el.selectize.setValue(selects[id], true);
      }
      // Numeric inputs ('' = NA/auto)
      var nInc = $('[id^=eui_inc_]:checked').length || 1;
      var nkVal = Math.max(1, 3 * nInc);
      var numerics = {
        penalty: 2, nk: nkVal, thresh: 0.001, minspan: 0, endspan: 0,
        newvar_penalty: 0, fast_k: 20, fast_beta: 1,
        nprune: '', nfold_override: 10, ncross: 20,
        varmod_exponent: 1, varmod_conv: 1, varmod_clamp: 0.1, varmod_minspan: -3,
        adjust_endspan: 2, exhaustive_tol: 1e-10,
        subset_arg: ''
      };
      for (var id in numerics) {
        $('#' + id).val(numerics[id]).trigger('change');
      }
      // Reset purpose radio to General
      $('input[name=purpose][value=general]').prop('checked', true).trigger('change');
      // Checkbox inputs
      var checks = {
        keepxy: false, stratify: true, scale_y: true, auto_linpreds: true,
        use_beta_cache: true, force_xtx_prune: false, get_leverages: true,
        force_weights: false, skip_subject_row: false
      };
      for (var id in checks) {
        $('#' + id).prop('checked', checks[id]).trigger('change');
      }
      // Reset linpreds: uncheck all Linear checkboxes
      $('[id^=eui_lin_]').prop('checked', false);
      // Reset type dropdowns to auto-detected values
      if (window.euiDetectedTypes && window.euiCols) {
        for (var i = 0; i < window.euiCols.length; i++) {
          var dt = window.euiDetectedTypes[window.euiCols[i]];
          if (dt) {
            $('#eui_type_' + (i + 1)).val(dt);
          }
        }
      }
      // Reset special dropdowns to 'no'
      $('[id^=eui_special_]').val('no');
      // Check all interaction checkboxes (allowed = NULL)
      $('.eui-interaction-cb').prop('checked', true);
      // Trigger change events
      if ($('.eui-var-cb').length) $('.eui-var-cb').first().trigger('change');
      if ($('.eui-type-select').length) $('.eui-type-select').first().trigger('change');
      if ($('.eui-interaction-cb').length) $('.eui-interaction-cb').first().trigger('change');
    });

    // Pre-seed sale_age as included in localStorage when computed
    Shiny.addCustomMessageHandler('sale_age_added', function(msg) {
      var fn = msg.filename || 'default';
      var storageKey = 'earthUI_vars_' + fn;
      var saved = {};
      try { saved = JSON.parse(localStorage.getItem(storageKey)) || {}; } catch(e) {}
      saved['sale_age'] = { inc: true, fac: false, lin: false, type: 'integer', special: 'no' };
      try { localStorage.setItem(storageKey, JSON.stringify(saved)); } catch(e) {}
    });

    // Collect current settings from localStorage and save as defaults
    Shiny.addCustomMessageHandler('collect_and_save_defaults', function(msg) {
      var fn = msg.filename;
      var payload = { filename: '__defaults__', settings: null, variables: null, interactions: null };
      try { payload.settings     = localStorage.getItem('earthUI_settings_' + fn); } catch(e) {}
      try { payload.variables    = localStorage.getItem('earthUI_vars_' + fn); } catch(e) {}
      try { payload.interactions = localStorage.getItem('earthUI_interactions_' + fn); } catch(e) {}
      Shiny.setInputValue('eui_save_trigger', payload, {priority: 'deferred'});
    });

  ")),
  tags$div(
    style = "padding: 10px 15px;",
    tags$h2(
      tags$img(src = "logo.png", height = "32px",
               style = "margin-right: 8px; vertical-align: middle;"),
      "earthUI",
      tags$small(" - Interactive Earth Model Builder",
                 style = "font-size: 0.6em; color: var(--bs-secondary-color);")
    )
  ),

  sidebarLayout(
    sidebarPanel(
      width = 4,

      # --- Purpose ---
      tags$div(
        style = "font-weight: bold;",
        radioButtons("purpose", "Purpose:",
                     choices = c("General" = "general",
                                 "For Appraisal" = "appraisal",
                                 "Market Area Analysis" = "market"),
                     selected = "general", inline = TRUE)
      ),
      conditionalPanel(
        condition = "input.purpose === 'market'",
        checkboxInput("skip_subject_row", "Skip first row (subject property)", value = FALSE)
      ),
      hr(),

      # --- Data Import ---
      h4("1. Import Data"),
      fileInput("file_input", "Choose CSV or Excel file",
                accept = c(".csv", ".xlsx", ".xls")),
      uiOutput("sheet_selector"),
      uiOutput("data_preview_info"),
      hr(),

      # --- 2. Output Folder ---
      conditionalPanel(
        condition = "output.data_loaded",
        h4("2. Project Output Folder"),
        textInput("output_folder", NULL,
                  value = path.expand("~/Downloads")),
        hr()
      ),

      # --- Variable Configuration ---
      conditionalPanel(
        condition = "output.data_loaded",
        tags$details(class = "eui-section",
          tags$summary(h4("3. Variable Configuration")),
          uiOutput("target_selector"),
          conditionalPanel(
            condition = "input.purpose !== 'general'",
            dateInput("effective_date", "Effective Date", value = Sys.Date())
          ),
          h5("Predictor Settings"),
          uiOutput("predictor_hint_text"),
          div(style = "max-height: 400px; overflow-y: auto; border: 1px solid #ddd; border-radius: 4px; padding: 4px;",
              uiOutput("variable_table"))
        ),
        hr(),

        # --- Earth Call Parameters ---
        tags$details(class = "eui-section",
          tags$summary(h4("4. Earth Call Parameters")),
        tags$div(
          style = "margin-bottom: 4px; font-size: 0.85em;",
          radioButtons("eui_defaults_action", NULL,
                       choices = c("Use last settings for input file" = "last",
                                   "Use default settings" = "use_default",
                                   "Earth defaults" = "earth_defaults"),
                       selected = "last", inline = TRUE)
        ),
        actionButton("eui_save_defaults", "Save current as default",
                     class = "btn-dark btn-sm",
                     style = "padding: 2px 8px; font-size: 0.85em; margin-bottom: 8px;"),
        actionButton("param_info", "Parameter Info",
                     class = "btn-info btn-sm",
                     style = "margin-bottom: 10px; width: 100%;"),

        # 1. subset
        param_with_help(
          tagList(
            textInput("subset_arg", "subset", value = "",
                      placeholder = "e.g. sale_age < 1000 & age > 20"),
            actionButton("subset_builder_btn", "Build filter...",
                         class = "btn-outline-secondary btn-sm",
                         style = "margin-top: -10px; margin-bottom: 8px; width: 100%;")
          ),
          "Row filter expression using column names. Leave blank to use all rows. Examples: sale_age < 1000, area_id != 460, sale_price > 100000 & age < 50"),

        # 2. weights
        param_with_help(
          selectInput("weights_col", "weights",
                      choices = c("NULL (none)" = "null"),
                      selected = "null"),
          "Case weights. Select a numeric column or NULL for no weighting."),

        # 3. wp (response weights — per-target numeric, not a column)
        param_with_help(
          tagList(
            tags$label("wp (response weights)", style = "font-weight: bold; font-size: 0.85em;"),
            tags$div(id = "wp_display",
                     style = "font-size: 0.85em; color: var(--bs-body-color, #666); margin-bottom: 4px;",
                     "NULL (equal weights)"),
            actionButton("wp_set_btn", "Set weights...",
                         class = "btn-outline-secondary btn-sm",
                         style = "margin-bottom: 8px; width: 100%;")
          ),
          "Response weights — one numeric weight per target variable. Only for multi-target models. Default NULL (equal weights)."),

        # 4. keepxy (na.action removed — always na.fail)
        param_with_help(
          checkboxInput("keepxy", "Keep x,y in model (keepxy)", value = FALSE),
          "Default FALSE. Retain x, y, subset, weights in model object. Required for some CV statistics. Makes CV slower."),

        # 6. trace
        param_with_help(
          selectInput("trace", "Trace level (trace)",
                      choices = c("0" = "0", "0.3" = "0.3", "0.5" = "0.5",
                                  "1" = "1", "2" = "2", "3" = "3",
                                  "4" = "4", "5" = "5"),
                      selected = "0"),
          "0=none, 0.3=variance model, 0.5=CV, 1=overview, 2=forward pass, 3=pruning, 4=model mats/pruning details, 5=full details."),

        # 7. glm
        param_with_help(
          selectInput("glm_family", "GLM family (glm)",
                      choices = c("none", "gaussian", "binomial", "poisson"),
                      selected = "none"),
          "Optional GLM family applied to earth basis functions. Example: 'binomial' for binary outcomes."),

        # 8. degree
        param_with_help(
          selectInput("degree", "Max interaction order (degree)",
                      choices = 1:4, selected = 1),
          "Maximum degree of interaction. 1 = no interactions. When >= 2, cross-validation is automatically enabled."),

        # 9. penalty
        param_with_help(
          numericInput("penalty", "GCV penalty per knot (penalty)", value = 2, min = -1, step = 0.5),
          "GCV penalty per knot. Default is 3 if degree>1, else 2. Use 0 to penalize only terms. Use -1 for no penalty (GCV = RSS/n)."),

        # 10. nk
        param_with_help(
          numericInput("nk", "Max terms before pruning (nk)", value = NA, min = 1, step = 1),
          "Maximum number of model terms before pruning. Default = 3 x number of selected predictors."),

        # 11. thresh
        param_with_help(
          numericInput("thresh", "Forward step threshold (thresh)", value = 0.001, min = 0, step = 0.0001),
          "Forward pass terminates if adding a term changes RSq by less than this value. Default 0.001."),

        # 12. minspan
        param_with_help(
          numericInput("minspan", "Min span (minspan)", value = 0, step = 1),
          "Minimum observations between knots. 0 = auto-calculated. Negative values specify max knots per predictor (e.g., -3 = three evenly spaced knots)."),

        # 13. endspan
        param_with_help(
          numericInput("endspan", "End span (endspan)", value = 0, min = 0, step = 1),
          "Minimum observations before first and after last knot. 0 = auto-calculated. Be wary of reducing this for predictions near data limits."),

        # 14. newvar.penalty
        param_with_help(
          numericInput("newvar_penalty", "New variable penalty (newvar.penalty)", value = 0, min = 0, step = 0.01),
          "Penalty for adding a new variable (Friedman's gamma). Default 0. Non-zero values (0.01-0.2) prefer reusing existing variables, simplifying interpretation."),

        # 15. fast.k
        param_with_help(
          numericInput("fast_k", "fast.k", value = 20, min = 0, step = 1),
          "Max parent terms per forward step (Fast MARS). Default 20. Set 0 to disable. Lower = faster, higher = potentially better model."),

        # 16. fast.beta
        param_with_help(
          numericInput("fast_beta", "fast.beta", value = 1, min = 0, step = 0.1),
          "Fast MARS ageing coefficient. Default 1. A value of 0 sometimes gives better results."),

        # 17. linpreds
        tags$div(
          style = "position: relative; margin-bottom: 10px;",
          tags$label("linpreds", style = "font-weight: bold; font-size: 0.85em;"),
          tags$p(class = "text-muted", style = "font-size: 0.8em; margin: 0;",
                 "Controlled by the Linear column in Variable Configuration above. Default FALSE (no forced linear predictors).")
        ),

        # 18. allowed
        conditionalPanel(
          condition = "input.degree >= 2",
          div(
            class = "alert alert-warning",
            style = "font-size: 0.85em;",
            strong("Note:"),
            "Interaction terms increase the risk of overfitting.",
            "Cross-validation has been enabled (10-fold)."
          ),
          h6("allowed (Interaction Matrix)", style = "border-bottom: 1px solid #ccc;"),
          p("Uncheck pairs to disallow specific interactions. Default NULL (all allowed).",
            class = "text-muted", style = "font-size: 0.85em;"),
          div(style = "margin-bottom: 6px;",
            tags$label(style = "font-size: 0.85em; margin-right: 12px; cursor: pointer;",
              tags$input(type = "checkbox", id = "eui_allow_all",
                         class = "eui-interaction-toggle", checked = "checked",
                         style = "margin-right: 4px;"),
              "Allow All"),
            tags$label(style = "font-size: 0.85em; cursor: pointer;",
              tags$input(type = "checkbox", id = "eui_clear_all",
                         class = "eui-interaction-toggle",
                         style = "margin-right: 4px;"),
              "Clear All")
          ),
          tags$script(HTML("
            $(document).on('change', '#eui_allow_all', function() {
              if ($(this).is(':checked')) {
                $('#eui_clear_all').prop('checked', false);
                $('.eui-interaction-cb').prop('checked', true).trigger('change');
              }
            });
            $(document).on('change', '#eui_clear_all', function() {
              if ($(this).is(':checked')) {
                $('#eui_allow_all').prop('checked', false);
                $('.eui-interaction-cb').prop('checked', false).trigger('change');
              }
            });
            $(document).on('change', '.eui-interaction-cb', function() {
              var all = $('.eui-interaction-cb').length;
              var checked = $('.eui-interaction-cb:checked').length;
              $('#eui_allow_all').prop('checked', checked === all);
              $('#eui_clear_all').prop('checked', checked === 0);
            });
          ")),
          uiOutput("allowed_matrix_ui")
        ),

        # --- For Pruning Pass (collapsible) ---
        tags$details(
          tags$summary(style = "cursor: pointer; font-weight: bold; margin-top: 8px;",
                       class = "text-body", "For Pruning Pass"),
          div(style = "padding-top: 6px;",
            # 19. pmethod
            param_with_help(
              selectInput("pmethod", "Pruning method (pmethod)",
                          choices = c("backward", "none", "exhaustive", "forward", "seqrep", "cv"),
                          selected = "backward"),
              "Pruning method. Default 'backward'. Use 'cv' with nfold to select terms by cross-validation. Use 'none' to retain all forward pass terms."),
            # 20. nprune
            param_with_help(
              numericInput("nprune", "Max terms after pruning (nprune)", value = NA, min = 1, step = 1),
              "Maximum terms (including intercept) in pruned model. Default NULL = all terms from forward pass, after pruning.")
          )
        ),

        # --- For Cross Validation (collapsible) ---
        tags$details(
          tags$summary(style = "cursor: pointer; font-weight: bold; margin-top: 8px;",
                       class = "text-body", "For Cross Validation"),
          div(style = "padding-top: 6px;",
            # 21. nfold
            param_with_help(
              numericInput("nfold_override", "CV folds (nfold)", value = 10, min = 0, step = 1),
              "Number of CV folds. Default 10. Auto-set to 10 when degree >= 2. Use trace=0.5 to trace CV."),
            # 22. ncross
            param_with_help(
              numericInput("ncross", "ncross", value = 20, min = 1, step = 1),
              "Number of cross-validations (each has nfold folds). Default 20. Use higher values with variance models."),
            # 23. stratify
            param_with_help(
              checkboxInput("stratify", "Stratify CV samples (stratify)", value = TRUE),
              "Default TRUE. Stratify CV samples so each fold has approximately equal response distribution.")
          )
        ),

        # --- For Variance Models (collapsible) ---
        tags$details(
          tags$summary(style = "cursor: pointer; font-weight: bold; margin-top: 8px;",
                       class = "text-body", "For Variance Models"),
          div(style = "padding-top: 6px;",
            # 24. varmod.method
            param_with_help(
              selectInput("varmod_method", "varmod.method",
                          choices = c("none", "const", "lm", "rlm", "earth", "gam",
                                      "power", "power0", "x.lm", "x.rlm", "x.earth", "x.gam"),
                          selected = "lm"),
              "Variance model method. Requires nfold and ncross. Use trace=0.3 to trace. 'lm','rlm','earth','gam' regress on predicted response; 'x.*' variants regress on predictors."),
            # 25. varmod.exponent
            param_with_help(
              numericInput("varmod_exponent", "varmod.exponent", value = 1, min = 0, step = 0.1),
              "Power transform for residual regression. Default 1. Use 0.5 if std dev increases with square root of response."),
            # 26. varmod.conv
            param_with_help(
              numericInput("varmod_conv", "varmod.conv", value = 1, step = 0.1),
              "Convergence criterion (%) for IRLS in variance model. Default 1. Negative values force that many iterations."),
            # 27. varmod.clamp
            param_with_help(
              numericInput("varmod_clamp", "varmod.clamp", value = 0.1, min = 0, step = 0.01),
              "Min estimated std dev = varmod.clamp * mean(sd(residuals)). Default 0.1. Prevents negative or tiny std dev estimates."),
            # 28. varmod.minspan
            param_with_help(
              numericInput("varmod_minspan", "varmod.minspan", value = -3, step = 1),
              "minspan for internal earth call in variance model. Default -3 (three evenly spaced knots per predictor).")
          )
        ),

        # --- Advanced Use (collapsible) ---
        tags$details(
          tags$summary(style = "cursor: pointer; font-weight: bold; margin-top: 8px;",
                       class = "text-body", "Advanced Use"),
          div(style = "padding-top: 6px;",
            # 29. Scale.y
            param_with_help(
              checkboxInput("scale_y", "Scale response (Scale.y)", value = TRUE),
              "Default TRUE. Scale response internally (subtract mean, divide by sd). Provides better numeric stability."),
            # 30. Adjust.endspan
            param_with_help(
              numericInput("adjust_endspan", "Adjust.endspan", value = 2, min = 1, step = 0.5),
              "In interaction terms, endspan is multiplied by this value. Default 2. Reduces overfitting at data boundaries."),
            # 31. Auto.linpreds
            param_with_help(
              checkboxInput("auto_linpreds", "Auto.linpreds", value = TRUE),
              "Default TRUE. If the best knot is at the predictor minimum, add the predictor linearly (no hinge). Only affects predictions outside training data range."),
            # 32. Force.weights
            param_with_help(
              checkboxInput("force_weights", "Force.weights", value = FALSE),
              "Default FALSE. For testing: force weighted code path even without weights."),
            # 33. Use.beta.cache
            param_with_help(
              checkboxInput("use_beta_cache", "Use.beta.cache", value = TRUE),
              "Default TRUE. Cache regression coefficients in forward pass for 20%+ speed improvement. Uses more memory."),
            # 34. Force.xtx.prune
            param_with_help(
              checkboxInput("force_xtx_prune", "Force.xtx.prune", value = FALSE),
              "Default FALSE. Force X'X-based subset evaluation in pruning instead of QR-based leaps. Advanced use only."),
            # 35. Get.leverages
            param_with_help(
              checkboxInput("get_leverages", "Get.leverages", value = TRUE),
              "Default TRUE (unless >100k cases). Calculate diagonal hat values for diagnostics."),
            # 36. Exhaustive.tol
            param_with_help(
              numericInput("exhaustive_tol", "Exhaustive.tol", value = 1e-10, min = 0, step = 1e-11),
              "Default 1e-10. If reciprocal condition number < this, forces pmethod='backward'. Only applies with pmethod='exhaustive'.")
          )
        )),
        hr(),

        # --- 5. Fit ---
        tags$details(class = "eui-section",
          tags$summary(h4("5. Fit Earth Model")),
          actionButton("run_model", "Fit Earth Model",
                       class = "btn-success btn-lg",
                       style = "width: 100%; margin-top: 10px;")
        ),

        # --- 6. Download Estimated ... & Residuals ---
        conditionalPanel(
          condition = "output.data_loaded",
          hr(),
          tags$details(class = "eui-section",
            tags$summary(uiOutput("download_heading", inline = TRUE)),
            conditionalPanel(
              condition = "input.purpose === 'appraisal'",
              actionButton("export_data", "Download Intermediate Output (Excel)",
                           class = "btn-success",
                           style = "width: 100%;")
            ),
            conditionalPanel(
              condition = "input.purpose !== 'appraisal'",
              actionButton("export_data_nonadj", "Download Output (Excel)",
                           class = "btn-success",
                           style = "width: 100%;")
            )
          )
        ),

        # --- 7. Calculate RCA Adjustments (Appraisal only) ---
        conditionalPanel(
          condition = "output.model_fitted && input.purpose === 'appraisal'",
          hr(),
          h4("7. Calculate RCA Adjustments & Download"),
          actionButton("rca_output_btn", "Calculate RCA Adjustments & Download",
                       class = "btn-success",
                       style = "width: 100%;")
        ),

        # --- Download Report ---
        conditionalPanel(
          condition = "output.model_fitted",
          hr(),
          tags$details(class = "eui-section",
            tags$summary(uiOutput("report_heading", inline = TRUE)),
            selectInput("export_format", "Format",
                        choices = c("HTML" = "html", "Word" = "docx",
                                    "PDF" = "pdf")),
            actionButton("export_report_btn", "Download Report",
                         class = "btn-success",
                         style = "width: 100%;")
          )
        ),

        # --- Export for mgcvUI (degree <= 2 only) ---
        conditionalPanel(
          condition = "output.model_fitted && output.mgcv_export_ok",
          hr(),
          tags$details(class = "eui-section",
            tags$summary(h4("Export for mgcvUI", style = "display:inline;")),
            tags$p("Save earthUI result as .rds for import into mgcvUI.",
                   style = "font-size: 0.85em; color: #888;"),
            actionButton("export_mgcv_btn", "Export for mgcvUI (.rds)",
                         class = "btn-info",
                         style = "width: 100%;")
          )
        )
      )
    ),

    mainPanel(
      width = 8,
      conditionalPanel(
        condition = "!output.data_loaded",
        div(
          class = "text-muted",
          style = "text-align: center; padding: 80px 20px;",
          h3("Welcome to earthUI"),
          p("Upload a CSV or Excel file to get started."),
          p("Build and explore Earth (MARS-style) models interactively.")
        )
      ),
      conditionalPanel(
        condition = "output.data_loaded && !output.model_fitted",
        h4("Data Preview"),
        conditionalPanel(
          condition = "input.purpose === 'appraisal'",
          h5("Subject Property"),
          DT::dataTableOutput("data_subjects"),
          h5("Comparable Sales"),
          DT::dataTableOutput("data_comps")
        ),
        conditionalPanel(
          condition = "input.purpose !== 'appraisal'",
          DT::dataTableOutput("data_preview")
        )
      ),
      conditionalPanel(
        condition = "output.model_fitted",
        tabsetPanel(
          id = "results_tabs",
          tabPanel(
            "Data",
            br(),
            conditionalPanel(
              condition = "input.purpose === 'appraisal'",
              h5("Subject Property"),
              DT::dataTableOutput("data_subjects_tab"),
              h5("Comparable Sales"),
              DT::dataTableOutput("data_comps_tab")
            ),
            conditionalPanel(
              condition = "input.purpose !== 'appraisal'",
              DT::dataTableOutput("data_preview_tab")
            )
          ),
          tabPanel(
            "Equation",
            br(),
            div(style = "overflow-x: auto; padding: 10px 10px 10px 0;",
                uiOutput("model_equation"))
          ),
          tabPanel(
            "Summary",
            br(),
            uiOutput("summary_metrics"),
            h5("Coefficients & Basis Functions"),
            DT::dataTableOutput("summary_table")
          ),
          tabPanel(
            "Variable Importance",
            br(),
            plotOutput("importance_plot", height = "400px"),
            br(),
            DT::dataTableOutput("importance_table")
          ),
          tabPanel(
            "Contribution",
            br(),
            uiOutput("response_selector_contrib"),
            uiOutput("contrib_g_selector"),
            uiOutput("contrib_plot_container")
          ),
          tabPanel(
            "Correlation",
            br(),
            uiOutput("correlation_plot_ui")
          ),
          tabPanel(
            "Diagnostics",
            br(),
            uiOutput("response_selector_diag"),
            fluidRow(
              column(6, plotOutput("residuals_plot", height = "350px")),
              column(6, plotOutput("qq_plot", height = "350px"))
            ),
            br(),
            plotOutput("actual_vs_predicted_plot", height = "400px")
          ),
          tabPanel(
            "RCA Adjustments",
            br(),
            uiOutput("rca_plots_ui")
          ),
          tabPanel(
            "ANOVA",
            br(),
            DT::dataTableOutput("anova_table")
          ),
          tabPanel(
            "Earth Output",
            br(),
            div(style = "overflow-x: auto;",
                verbatimTextOutput("earth_output"))
          )
        )
      )
    )
  ),

  # ── AGPL-3 legal footer ──────────────────────────────────────────────
  tags$hr(style = "margin-top: 30px; margin-bottom: 10px;"),
  tags$footer(
    style = paste(
      "text-align: center; padding: 10px 15px 15px;",
      "font-size: 0.8em; color: #7f8c8d;"
    ),
    tags$p(
      style = "margin: 2px 0;",
      HTML(paste0(
        "earthUI v", utils::packageVersion("earthUI"),
        " &mdash; Copyright &copy; 2026 Pacific Vista Net / William Craytor"
      ))
    ),
    tags$p(
      style = "margin: 2px 0;",
      "Licensed under the ",
      tags$a(
        href = "https://www.gnu.org/licenses/agpl-3.0.html",
        target = "_blank",
        "GNU Affero General Public License v3.0"
      ),
      " or later (AGPL-3)."
    ),
    tags$p(
      style = "margin: 2px 0;",
      HTML("This software is provided &ldquo;as is&rdquo;, without warranty of any kind.")
    ),
    tags$p(
      style = "margin: 2px 0;",
      "Source code: ",
      tags$a(
        href = "https://github.com/wcraytor/earthUI",
        target = "_blank",
        "github.com/wcraytor/earthUI"
      )
    )
  )
)
