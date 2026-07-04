// glmnetUI — Settings, theme, locale, and defaults management
// This file is loaded by ui.R via tags$script(src = "glmnetui.js")

// Settings dropdown toggle. Settings stays open until you click the gear
// again or press "Save and Close" — it does NOT auto-close on outside clicks,
// so using the Browse/Select file dialog can never dismiss it.
function glmnetToggleDropdown(id) {
  var el = document.getElementById(id);
  if (el) el.classList.toggle("open");
}
// Allow the server to close the Settings dropdown (e.g. on Save and Close).
Shiny.addCustomMessageHandler("close_settings_dropdown", function(msg) {
  var el = document.getElementById("glmnet-settings-dropdown");
  if (el) el.classList.remove("open");
});

var glmnetCurrentMode = "light";

function glmnetToggleTheme() {
  glmnetCurrentMode = (glmnetCurrentMode === "dark") ? "light" : "dark";
  console.log("[glmnetUI] Toggle clicked, new mode:", glmnetCurrentMode);
  document.body.setAttribute("data-glmnet-theme", glmnetCurrentMode);
  Shiny.setInputValue("dark_mode", glmnetCurrentMode, {priority: "event"});
  glmnetUpdateIcon(glmnetCurrentMode);
  try { localStorage.setItem("glmnetUI_theme", glmnetCurrentMode); } catch(e) {}
}

function glmnetUpdateIcon(mode) {
  var btn = document.getElementById("glmnet-theme-toggle");
  if (btn) btn.innerHTML = (mode === "dark") ? "\u2600" : "\u263E";
}

$(document).on("shiny:connected", function() {
  var btn = document.getElementById("glmnet-theme-toggle");
  if (btn) btn.onclick = glmnetToggleTheme;

  var saved = null;
  try { saved = localStorage.getItem("glmnetUI_theme"); } catch(e) {}
  if (saved === "dark") {
    glmnetCurrentMode = "dark";
    document.body.setAttribute("data-glmnet-theme", "dark");
    glmnetUpdateIcon("dark");
    Shiny.setInputValue("dark_mode", "dark", {priority: "event"});
  } else {
    document.body.setAttribute("data-glmnet-theme", "light");
    Shiny.setInputValue("dark_mode", "light", {priority: "event"});
  }
});

// Report generation timer: floating overlay showing elapsed time
var _reportTimer = null;
var _reportTimerStart = 0;
var _reportTimerDiv = null;
Shiny.addCustomMessageHandler("report_timer", function(msg) {
  if (msg.action === "start") {
    _reportTimerStart = Date.now();
    if (_reportTimer) clearInterval(_reportTimer);
    // Create floating timer overlay
    if (!_reportTimerDiv) {
      _reportTimerDiv = document.createElement("div");
      _reportTimerDiv.id = "glmnet-report-timer";
      _reportTimerDiv.style.cssText =
        "position:fixed; top:50%; left:50%; transform:translate(-50%,-50%);" +
        "background:rgba(0,0,0,0.85); color:white; padding:20px 40px;" +
        "border-radius:10px; font-size:18px; z-index:99999;" +
        "text-align:center; font-family:sans-serif;";
      document.body.appendChild(_reportTimerDiv);
    }
    _reportTimerDiv.style.display = "block";
    _reportTimerDiv.innerHTML = "Generating report...<br><span style='font-size:28px; font-weight:bold;'>0s</span>";

    _reportTimer = setInterval(function() {
      var elapsed = Math.floor((Date.now() - _reportTimerStart) / 1000);
      var min = Math.floor(elapsed / 60);
      var sec = elapsed % 60;
      var timeStr = min > 0
        ? min + ":" + (sec < 10 ? "0" : "") + sec
        : sec + "s";
      if (_reportTimerDiv) {
        _reportTimerDiv.innerHTML =
          "Generating report...<br><span style='font-size:28px; font-weight:bold;'>" +
          timeStr + "</span>";
      }
    }, 1000);
  } else if (msg.action === "stop") {
    if (_reportTimer) { clearInterval(_reportTimer); _reportTimer = null; }
    if (_reportTimerDiv) _reportTimerDiv.style.display = "none";
  }
});

// Update a text input value from server
Shiny.addCustomMessageHandler("glmnet-update-input", function(msg) {
  var el = document.getElementById(msg.id);
  if (el) el.value = msg.value || "";
});

// Reset a fileInput widget (clear filename display and file reference)
Shiny.addCustomMessageHandler("glmnet-reset-file-input", function(msg) {
  var container = document.getElementById(msg.id);
  if (!container) return;
  var fileInput = container.querySelector("input[type='file']");
  if (fileInput) fileInput.value = "";
  var textInput = container.querySelector("input[type='text']");
  if (textInput) textInput.value = "";
});

// Show white checkmark on button after successful action
Shiny.addCustomMessageHandler("btn_done", function(msg) {
  var btn = document.getElementById(msg.id);
  if (!btn) return;
  if (btn.querySelector(".glmnet-check")) return;
  var chk = document.createElement("span");
  chk.className = "glmnet-check";
  chk.style.cssText = "margin-left:8px; font-size:1.1em;";
  chk.textContent = "\u2713";
  btn.appendChild(chk);
});

// Clear checkmarks on all download buttons when a new model is fit
$(document).on("click", "#model-fit_btn", function() {
  document.querySelectorAll(".glmnet-check").forEach(function(el) {
    el.remove();
  });
});

// Helper: get purpose-keyed localStorage key
function glmnetStorageKey(prefix, filename) {
  var purpose = $("input[name='purpose']:checked").val() || "general";
  return prefix + filename + "_" + purpose;
}

// Pre-seed sale_age as included in localStorage when computed
Shiny.addCustomMessageHandler("sale_age_added", function(msg) {
  if (!msg.filename) return;
  var storageKey = glmnetStorageKey("glmnetUI_vars_", msg.filename);
  var saved = {};
  try { saved = JSON.parse(localStorage.getItem(storageKey)) || {}; } catch(e) {}
  saved["sale_age"] = { inc: true, fac: false, type: "integer", force: false, sign: "either", special: "no" };
  try { localStorage.setItem(storageKey, JSON.stringify(saved)); } catch(e) {}
});

// --- Locale persistence ---
// Restore saved locale defaults on connect
$(document).on("shiny:connected", function() {
  var ld = null;
  try { ld = JSON.parse(localStorage.getItem("glmnetUI_locale_defaults")); } catch(e) {}
  if (ld) {
    Shiny.setInputValue("glmnet_locale_defaults", ld, {priority: "event"});
  }

  // Restore last-used purpose
  var lastPurpose = null;
  try { lastPurpose = localStorage.getItem("glmnetUI_last_purpose"); } catch(e) {}
  if (lastPurpose) {
    var radio = $("input[name='purpose'][value='" + lastPurpose + "']");
    if (radio.length) {
      radio.prop("checked", true).trigger("change");
    }
  }
});

// Save purpose whenever it changes
$(document).on("change", "input[name='purpose']", function() {
  try { localStorage.setItem("glmnetUI_last_purpose", $(this).val()); } catch(e) {}
});

// Save locale defaults to localStorage
Shiny.addCustomMessageHandler("save_locale_defaults", function(msg) {
  try { localStorage.setItem("glmnetUI_locale_defaults", JSON.stringify(msg)); } catch(e) {}
});

// --- Global settings apply function ---
window.glmnetApplySettings = function(s) {
  // Radio buttons (namespaced and global)
  ["model-alpha_method", "model-lambda_method", "model-lambda_choice"
  ].forEach(function(id) {
    if (s[id] !== undefined) {
      $("input[name=\"" + id + "\"][value=\"" + s[id] + "\"]")
        .prop("checked", true).trigger("change");
    }
  });
  // Sliders (ionRangeSlider)
  ["model-alpha", "model-gamma"].forEach(function(id) {
    if (s[id] !== undefined) {
      var irs = $(document.getElementById(id)).data("ionRangeSlider");
      if (irs) irs.update({from: s[id]});
    }
  });
  // Range slider
  if (s["model-alpha_range"] !== undefined) {
    var irs = $(document.getElementById("model-alpha_range"))
      .data("ionRangeSlider");
    if (irs) {
      var v = s["model-alpha_range"];
      irs.update({from: v[0], to: v[1]});
    }
  }
  // Numeric inputs
  ["model-n_alphas", "model-lambda_manual", "model-nfolds"]
    .forEach(function(id) {
      if (s[id] !== undefined) {
        $(document.getElementById(id)).val(s[id]).trigger("change");
      }
    });
  // Select (selectize)
  if (s["model-family"] !== undefined) {
    var el = document.getElementById("model-family");
    if (el && el.selectize) el.selectize.setValue(s["model-family"], true);
    else if (el) $(el).val(s["model-family"]).trigger("change");
  }
  // Checkboxes
  ["model-standardize", "model-enforce_signs", "model-relaxed"]
    .forEach(function(id) {
      if (s[id] !== undefined) {
        $(document.getElementById(id)).prop("checked", s[id])
          .trigger("change");
      }
    });
  // Text inputs
  if (s["output_folder"] !== undefined) {
    $(document.getElementById("output_folder"))
      .val(s["output_folder"]).trigger("change");
  }
  // Effective Date is intentionally NOT restored from localStorage: earthUI
  // is the source of truth and the server applies it on project open.
};

// Apply variables from saved state to DOM
window.glmnetApplyVariables = function(saved) {
  for (var colName in saved) {
    var sv = saved[colName];
    var $inc = $(document.getElementById("data-inc_" + colName));
    var $fac = $(document.getElementById("data-fac_" + colName));
    var $force = $(document.getElementById("data-force_" + colName));
    var $sign = $(document.getElementById("data-sign_" + colName));
    var $type = $(document.getElementById("data-type_" + colName));
    var $special = $(document.getElementById("data-special_" + colName));

    // Skip controls locked (disabled) by an active earth import, so applying
    // saved settings never overrides the earth lock.
    if ($inc.length && !$inc.prop("disabled") && sv.inc !== undefined) $inc.prop("checked", sv.inc);
    if ($fac.length && !$fac.prop("disabled") && sv.fac !== undefined) $fac.prop("checked", sv.fac);
    if ($force.length && !$force.prop("disabled") && sv.force !== undefined) $force.prop("checked", sv.force);
    if ($sign.length && !$sign.prop("disabled") && sv.sign) $sign.val(sv.sign);
    if ($type.length && !$type.prop("disabled") && sv.type) $type.val(sv.type);
    if ($special.length && !$special.prop("disabled") && sv.special) $special.val(sv.special);
  }
  if ($(".glmnet-var-cb").length) $(".glmnet-var-cb").first().trigger("change");
};

// Apply interactions from saved state to DOM
window.glmnetApplyInteractions = function(saved) {
  for (var key in saved) {
    var $cb = $(document.getElementById("model-int_" + key));
    if ($cb.length) $cb.prop("checked", saved[key]);
  }
  if ($(".glmnet-interaction-cb").length) {
    $(".glmnet-interaction-cb").first().trigger("change");
  }
};

// --- Apply saved defaults from localStorage ---
Shiny.addCustomMessageHandler("apply_saved_defaults", function(msg) {
  var fn = msg.filename || "default";
  var purpose = $("input[name='purpose']:checked").val() || "general";
  var defSettings = null;
  var defVars = null;
  var defInts = null;
  try { defSettings = JSON.parse(localStorage.getItem("glmnetUI_settings___defaults__")); } catch(e) {}
  try { defVars = JSON.parse(localStorage.getItem("glmnetUI_vars___defaults__")); } catch(e) {}
  try { defInts = JSON.parse(localStorage.getItem("glmnetUI_interactions___defaults__")); } catch(e) {}

  if (!defSettings) {
    Shiny.setInputValue("glmnet_no_defaults", Math.random(),
      {priority: "event"});
    return;
  }

  window.glmnetApplySettings(defSettings);
  // Save to current file+purpose localStorage too
  var sfx = fn + "_" + purpose;
  try { localStorage.setItem("glmnetUI_settings_" + sfx, JSON.stringify(defSettings)); } catch(e) {}

  if (defVars) {
    try { localStorage.setItem("glmnetUI_vars_" + sfx, JSON.stringify(defVars)); } catch(e) {}
    setTimeout(function() { window.glmnetApplyVariables(defVars); }, 200);
  }
  if (defInts) {
    try { localStorage.setItem("glmnetUI_interactions_" + sfx, JSON.stringify(defInts)); } catch(e) {}
    setTimeout(function() { window.glmnetApplyInteractions(defInts); }, 200);
  }
});

// --- Apply glmnet factory defaults ---
Shiny.addCustomMessageHandler("apply_glmnet_defaults", function(msg) {
  var defaults = {
    "model-alpha_method": "fixed",
    "model-alpha": 1,
    "model-alpha_range": [0, 1],
    "model-n_alphas": 11,
    "model-lambda_method": "cv",
    "model-lambda_manual": 0.01,
    "model-lambda_choice": "1se",
    "model-nfolds": 10,
    "model-family": "gaussian",
    "model-standardize": true,
    "model-enforce_signs": false,
    "model-relaxed": false,
    "model-gamma": 0
  };
  window.glmnetApplySettings(defaults);
  // Reset variable table: check all inc, uncheck force, set sign=either.
  // ":not(:disabled)" leaves earth-locked controls untouched.
  $(".glmnet-var-cb:not(:disabled)").prop("checked", true);
  $(".glmnet-force-cb:not(:disabled)").prop("checked", false);
  $(".glmnet-sign-sel:not(:disabled)").val("either");
  $(".glmnet-special-sel:not(:disabled)").val("no");
  if ($(".glmnet-var-cb").length) $(".glmnet-var-cb").first().trigger("change");
  // Clear all interactions (default is unchecked)
  $(".glmnet-interaction-cb").prop("checked", false);
  if ($(".glmnet-interaction-cb").length) {
    $(".glmnet-interaction-cb").first().trigger("change");
  }
});

// --- Interaction matrix: init click/right-click/persistence ---
Shiny.addCustomMessageHandler("glmnet_init_matrix", function(msg) {
  var n = msg.n;
  var nsPrefix = msg.nsPrefix;
  var purpose = $("input[name='purpose']:checked").val() || "general";
  var storageKey = "glmnetUI_interactions_" + msg.storageKey + "_" + purpose;
  var glmnetPreds = msg.preds;
  var blk1Key = "glmnetUI_blk1_" + msg.storageKey + "_" + purpose;

  function saveState() {
    var state = {};
    for (var i = 1; i < n; i++) {
      for (var j = i + 1; j <= n; j++) {
        var id = nsPrefix + "int_" + i + "_" + j;
        state[i + "_" + j] = $("#" + CSS.escape(id)).is(":checked");
      }
    }
    try { localStorage.setItem(storageKey, JSON.stringify(state)); } catch(e) {}
  }

  function restoreState() {
    var saved = null;
    try {
      saved = JSON.parse(localStorage.getItem(storageKey));
      // Fall back to old key format (without purpose suffix)
      if (!saved) saved = JSON.parse(localStorage.getItem("glmnetUI_interactions_" + msg.storageKey));
    } catch(e) {}
    if (!saved) return;
    for (var i = 1; i < n; i++) {
      for (var j = i + 1; j <= n; j++) {
        var key = i + "_" + j;
        if (saved[key] !== undefined) {
          var id = nsPrefix + "int_" + i + "_" + j;
          $("#" + CSS.escape(id)).prop("checked", saved[key]);
        }
      }
    }
  }

  function syncToShiny() {
    for (var i = 1; i < n; i++) {
      for (var j = i + 1; j <= n; j++) {
        var id = nsPrefix + "int_" + i + "_" + j;
        Shiny.setInputValue(id, $("#" + CSS.escape(id)).is(":checked"));
      }
    }
  }

  // Delay restore until checkboxes are in the DOM
  // (sendCustomMessage fires before renderUI output is rendered)
  setTimeout(function() {
    restoreState();
    setTimeout(function() {
      syncToShiny();
      var all = $(".glmnet-interaction-cb").length;
      var checked = $(".glmnet-interaction-cb:checked").length;
      $("#" + CSS.escape(nsPrefix + "allow_all")).prop("checked", checked === all);
      $("#" + CSS.escape(nsPrefix + "clear_all")).prop("checked", checked === 0);
    }, 200);
  }, 500);

  // Checkbox change: save and sync
  $(document).off("change.glmnetmatrix").on("change.glmnetmatrix",
    ".glmnet-interaction-cb", function() {
      saveState();
      syncToShiny();
    });

  // Left-click variable name: toggle all its interactions
  $(document).off("click.glmnetvarlabel").on("click.glmnetvarlabel",
    ".glmnet-matrix-varlabel", function() {
      var k = parseInt($(this).attr("data-var-idx"));
      if (isNaN(k)) return;
      var cbs = [];
      for (var i = 1; i <= n; i++) {
        if (i === k) continue;
        var lo = Math.min(i, k), hi = Math.max(i, k);
        var id = nsPrefix + "int_" + lo + "_" + hi;
        var $cb = $("#" + CSS.escape(id));
        if ($cb.length) cbs.push($cb);
      }
      var allChecked = cbs.every(function($cb) { return $cb.is(":checked"); });
      cbs.forEach(function($cb) { $cb.prop("checked", !allChecked); });
      if (cbs.length > 0) cbs[0].trigger("change");
    });

  // Block from main effect: right-click variable name
  var blk1 = {};
  try {
    var s = JSON.parse(localStorage.getItem(blk1Key));
    if (!s) s = JSON.parse(localStorage.getItem("glmnetUI_blk1_" + msg.storageKey));
    if (s) blk1 = s;
  } catch(e) {}

  function updateBlk1Labels() {
    $(".glmnet-matrix-varlabel").each(function() {
      var idx = parseInt($(this).attr("data-var-idx"));
      if (isNaN(idx)) return;
      var varName = glmnetPreds[idx - 1];
      var $ind = $(this).find(".blk1-indicator");
      if (blk1[varName]) {
        if ($ind.length === 0) {
          $(this).append('<span class="blk1-indicator" style="color:var(--bs-body-color, #000);font-weight:bold;font-size:0.85em;"> 1</span>');
        }
      } else {
        $ind.remove();
      }
    });
  }

  function syncBlk1() {
    var blocked = [];
    for (var v in blk1) { if (blk1[v]) blocked.push(v); }
    Shiny.setInputValue(nsPrefix + "block_main_effect",
      blocked.length > 0 ? blocked : null, {priority: "event"});
    try { localStorage.setItem(blk1Key, JSON.stringify(blk1)); } catch(e) {}
  }

  updateBlk1Labels();
  setTimeout(syncBlk1, 250);

  $(document).off("contextmenu.glmnetBlk1").on("contextmenu.glmnetBlk1",
    ".glmnet-matrix-varlabel", function(e) {
      e.preventDefault();
      e.stopPropagation();
      var idx = parseInt($(this).attr("data-var-idx"));
      if (isNaN(idx)) return false;
      var varName = glmnetPreds[idx - 1];
      blk1[varName] = !blk1[varName];
      updateBlk1Labels();
      syncBlk1();
      return false;
    });
});

// --- Collect and save current settings as defaults ---
Shiny.addCustomMessageHandler("collect_and_save_defaults", function(msg) {
  var fn = msg.filename || "default";
  var sfx = fn + "_" + ($("input[name='purpose']:checked").val() || "general");
  try {
    var s = localStorage.getItem("glmnetUI_settings_" + sfx);
    if (s) localStorage.setItem("glmnetUI_settings___defaults__", s);
  } catch(e) {}
  try {
    var v = localStorage.getItem("glmnetUI_vars_" + sfx);
    if (v) localStorage.setItem("glmnetUI_vars___defaults__", v);
  } catch(e) {}
  try {
    var i = localStorage.getItem("glmnetUI_interactions_" + sfx);
    if (i) localStorage.setItem("glmnetUI_interactions___defaults__", i);
  } catch(e) {}
});

// --- Save ONLY the variable configuration as default (Section 4 button) ---
Shiny.addCustomMessageHandler("collect_and_save_varconfig", function(msg) {
  var fn = msg.filename || "default";
  var sfx = fn + "_" + ($("input[name='purpose']:checked").val() || "general");
  try {
    var v = localStorage.getItem("glmnetUI_vars_" + sfx);
    if (v) localStorage.setItem("glmnetUI_vars___defaults__", v);
  } catch(e) {}
});

// --- Save purpose per filename ---
Shiny.addCustomMessageHandler("save_purpose", function(msg) {
  if (!msg.filename) return;
  try {
    localStorage.setItem("glmnetUI_purpose_" + msg.filename, msg.purpose);
  } catch(e) {}
});

// --- Restore purpose for a filename ---
Shiny.addCustomMessageHandler("restore_purpose", function(msg) {
  if (!msg.filename) return;
  try {
    var saved = localStorage.getItem("glmnetUI_purpose_" + msg.filename);
    if (saved) {
      Shiny.setInputValue("glmnet_restored_purpose", saved, {priority: "event"});
    }
  } catch(e) {}
});
