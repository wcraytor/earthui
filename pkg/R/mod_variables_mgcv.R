# The appraisal "Special" column roles and the default-guesser
# (special_roles_(), special_default_for_()) live in specials.R, absorbed
# from valengrCore (2026-07-03) and kept identical across the sibling apps.

#' Variable Selection Module -- UI
#'
#' Compact variable table with Inc, Type, and Linear checkboxes,
#' plus an Allowed Interactions matrix. Designed for sidebar embedding.
#'
#' @param id Shiny module namespace ID.
#' @return A [shiny::tagList].
#' @export
#' @examples
#' if (interactive()) {
#'   ui <- fluidPage(mod_variables_ui("vars1"))
#' }
mod_variables_ui <- function(id) {
  ns <- NS(id)
  tagList(
    selectInput(ns("response"), "Target (response)",
                choices = NULL, width = "100%"),
    selectInput(ns("response_transform"), NULL,
                choices = c("None (raw)" = "none",
                            "Log (natural)" = "log",
                            "Log10" = "log10"),
                selected = "none", width = "100%"),
    tags$p(id = ns("transform_hint"),
           "Log transform makes time adjustments proportional (%) instead of absolute ($).",
           style = "font-size: 0.75em; color: #888; margin-top: -8px; margin-bottom: 6px; display: none;"),
    tags$label("Predictor Settings", class = "control-label",
               style = "font-weight: bold; margin-bottom: 2px;"),
    tags$p("Type = data type. Include = include in model. Factor = treat as factor. Linear = force linear. Special = column role.",
           style = "font-size: 0.8em; color: #888; margin-bottom: 4px;"),
    div(style = paste("max-height: 400px; overflow-y: auto;",
                      "border: 1px solid #ddd; border-radius: 4px;",
                      "padding: 4px;"),
      uiOutput(ns("var_table"))
    ),
    actionButton(ns("save_varconfig"), "Save current settings as default",
                 class = "btn-outline-primary btn-sm",
                 style = "width:100%; margin-top:8px;")
  )
}


#' Mgcv Call Parameters -- UI
#'
#' Model fitting parameters for the GAM call. Uses the same module
#' namespace as [mod_variables_ui()] so inputs are shared.
#'
#' @param id Shiny module namespace ID (must match [mod_variables_ui()]).
#' @return A [shiny::tagList].
#' @export
mod_variables_params_ui <- function(id) {
  ns <- NS(id)

  # Helper: wrap an input with a "?" popover icon (matches earthUI style)
  ph <- function(input_el, help_text) {
    tags$div(
      style = "position: relative;",
      input_el,
      tags$span(
        class = "mgcv-param-help",
        `data-bs-toggle` = "popover",
        `data-bs-trigger` = "hover focus",
        `data-bs-content` = help_text,
        `data-bs-placement` = "left",
        "?"
      )
    )
  }

  tagList(
    ph(
      selectInput(ns("preset"), "Parameter Preset",
                  choices = c("Standalone (discovery)" = "standalone",
                              "Earth Pipeline (refinement)" = "earth"),
                  selected = "standalone", width = "100%"),
      "Standalone: general-purpose defaults (tp basis, k=auto, gamma=1.2). Earth Pipeline: uses cr basis seeded with earth knot positions augmented by data quantiles (k=auto, gamma=1.4). Earth knots anchor the spline at MARS-discovered change points while the GAM retains full flexibility."
    ),
    tags$p(id = ns("preset_desc"),
           style = "font-size: 0.8em; color: #888; margin-top: -8px; margin-bottom: 8px;"),
    ph(
      selectInput(ns("family"), "Family",
                  choices = c("gaussian", "Gamma", "poisson",
                              "binomial", "inverse.gaussian"),
                  selected = "gaussian", width = "100%"),
      "The distribution family for the response variable. gaussian = normal errors (continuous response, e.g. sale price). Gamma = positive continuous data with variance proportional to mean squared. poisson = count data. binomial = binary/proportion outcomes."
    ),
    ph(
      selectInput(ns("method"), "Method",
                  choices = c("REML", "GCV.Cp", "ML", "P-REML", "P-ML",
                              "GACV.Cp"),
                  selected = "REML", width = "100%"),
      "Smoothing parameter estimation method. REML (Restricted Maximum Likelihood) is the recommended default \u2014 most stable and resistant to overfitting. GCV.Cp (Generalized Cross Validation) tends to undersmooth. ML (Maximum Likelihood) is useful for comparing models with different fixed effects. P-REML/P-ML add a penalty to prevent zero smoothing parameters."
    ),
    ph(
      numericInput(ns("gamma"), "Gamma (smoothing penalty)",
                   value = 1.2, min = 0.1, max = 10, step = 0.1,
                   width = "100%"),
      "Multiplier on the effective degrees of freedom in the GCV/UBRE/REML score. Values > 1 increase the smoothing penalty, producing smoother (less wiggly) fits. 1.0 = no extra penalty. 1.2\u20131.4 = mild regularization (recommended). Higher values guard against overfitting at the cost of potential underfitting."
    ),
    ph(
      checkboxInput(ns("cv"), "Cross-validate (10-fold CV R\u00b2)",
                    value = TRUE),
      "When checked, performs 10-fold cross-validation after fitting and reports the out-of-sample R-squared. Useful for assessing how well the model generalizes to unseen data. Adds fitting time but does not change the final model."
    ),
    ph(
      checkboxInput(ns("select"), "Select (shrink-to-zero penalty)",
                    value = TRUE),
      "Adds an extra shrinkage penalty that can zero out smooth terms entirely, effectively performing variable selection. Recommended when you are unsure which predictors matter. If a smooth is estimated at ~0 edf, it contributes nothing to the model."
    ),
    ph(
      selectInput(ns("default_basis"), "Default basis",
                  choices = c("tp", "cr", "ps", "bs"),
                  selected = "tp", width = "100%"),
      "The spline basis used for smooth terms. tp (thin plate regression spline) = the default, isotropic, good all-round choice. cr (cubic regression spline) = knot-based, efficient, good with earth-derived knots. ps (P-spline) = B-spline basis with difference penalty. bs (B-spline) = standard B-spline basis."
    ),
    ph(
      numericInput(ns("default_k"), "Default k (0 = auto)",
                   value = 0, min = 0, max = 100, step = 1,
                   width = "100%"),
      "Maximum basis dimension (number of basis functions) for each smooth term. Controls the upper bound on wiggliness \u2014 the actual complexity is determined by the smoothing penalty. 0 = auto (mgcv default of 10). Increase k if gam.check() reports k is too low. Decrease for simpler models or small datasets."
    ),
    ph(
      selectInput(ns("tensor_type"), "Tensor type for interactions",
                  choices = c("ti (tensor interaction)" = "ti",
                              "te (tensor product)" = "te"),
                  selected = "ti", width = "100%"),
      "How smooth interactions between two continuous variables are constructed. ti (tensor interaction) = models only the pure interaction effect, excluding main effects (preferred when main effects are already in the model). te (tensor product) = models the full joint smooth including main effects \u2014 use when you want one term to capture the entire bivariate relationship."
    ),
    tags$details(
      tags$summary(
        style = "cursor: pointer; font-weight: bold; font-size: 0.9em;",
        "Advanced Parameters"
      ),
      div(style = "padding-top: 6px;",
        ph(
          selectInput(ns("optimizer"), "Optimizer",
                      choices = c("outer/newton" = "outer_newton",
                                  "outer/bfgs"   = "outer_bfgs",
                                  "efs"          = "efs"),
                      selected = "outer_newton", width = "100%"),
          "Numerical optimizer for smoothing parameter estimation. outer/newton = default, fast and reliable for most models. outer/bfgs = quasi-Newton, can help when newton struggles to converge. efs = Extended Fellner-Schall, very stable for complex models or when other optimizers fail."
        ),
        ph(
          numericInput(ns("scale"), "Scale (0 = estimate)",
                       value = 0, min = -1, max = 1000, step = 0.1,
                       width = "100%"),
          "The scale (dispersion) parameter. 0 = estimate from data (default for gaussian/Gamma). For gaussian family this is the residual variance. Set to 1 for Poisson or binomial (known scale). A fixed positive value overrides estimation."
        ),
        ph(
          checkboxInput(ns("discrete"), "Discrete covariate method (fast)",
                        value = FALSE),
          "Uses the discretized covariate method (bam-style) for faster fitting on large datasets. Groups covariate values into bins to reduce computation. Recommended for datasets with >10,000 rows. May slightly reduce accuracy for small datasets."
        ),
        ph(
          numericInput(ns("nthreads"), "Threads",
                       value = 1L, min = 1L, max = 32L, step = 1L,
                       width = "100%"),
          "Number of parallel threads for model fitting. Only effective when 'Discrete covariate method' is enabled. More threads can speed up fitting on large datasets. Set to 1 for single-threaded (default)."
        )
      )
    ),
    br(),
    tags$details(
      tags$summary(
        style = "cursor: pointer; font-weight: bold; font-size: 0.9em;",
        "Allowed Interactions"
      ),
      div(style = "padding-top: 6px;",
        tags$label("Tensor Interactions (continuous \u00d7 continuous)",
                   style = "font-weight: bold; font-size: 0.85em;"),
        fluidRow(
          column(6, actionButton(ns("allow_all_int"), "Allow All",
                                 class = "btn-sm btn-outline-primary",
                                 style = "width:100%;")),
          column(6, actionButton(ns("clear_all_int"), "Clear All",
                                 class = "btn-sm btn-outline-secondary",
                                 style = "width:100%;"))
        ),
        br(),
        uiOutput(ns("allowed_matrix_ui")),
        hr(),
        tags$label("Factor-by-Smooth Interactions (factor \u00d7 continuous)",
                   style = "font-weight: bold; font-size: 0.85em;"),
        tags$p("Fits a separate smooth for each factor level: s(x, by=factor)",
               style = "font-size: 0.75em; color: #888; margin-bottom: 4px;"),
        uiOutput(ns("factor_by_smooth_ui"))
      )
    ),
    actionButton(ns("save_params"), "Save current settings as default",
                 class = "btn-outline-primary btn-sm",
                 style = "width:100%; margin-top:8px;")
  )
}


#' Variable Selection Module -- Server
#'
#' @param id Shiny module namespace ID.
#' @param data_r A reactive returning the imported data frame.
#' @param filename_r A reactive returning the original filename.
#' @param earth_knots_r A reactive returning an `mgcvUI_earth_knots`
#'   object (or `NULL`).
#' @param purpose_r A reactive returning the purpose mode string
#'   (`"general"`, `"appraisal"`, or `"market"`).
#' @param active_project_r A reactive returning the active regProj project
#'   (a one-row data frame with `project_path`/`purpose`) or `NULL`. Used to
#'   save/restore per-(project, purpose) settings via the "Save current
#'   settings as default" buttons.
#' @return A reactive list with components: `response`, `predictors`,
#'   `smooth_specs`, `family`, `method`, `select`, `gamma`.
#' @export
mod_variables_server <- function(id, data_r, filename_r = reactive(NULL),
                                 earth_knots_r = reactive(NULL),
                                 purpose_r = reactive("general"),
                                 active_project_r = reactive(NULL)) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    var_state <- reactiveVal(list())
    restoring_ <- reactiveVal(FALSE)

    # --- Per-project settings (shared projects.sqlite, method = "mgcv") ---
    # Settings follow the active project + purpose, saved explicitly via the
    # "Save current settings as default" buttons (matching earthUI).
    proj_purpose_ <- function() {
      p <- active_project_r()
      if (!is.null(p) && !is.null(p$purpose)) {
        switch(p$purpose, gen = "general", appr = "appraisal",
               mktarea = "market", "general")
      } else {
        purpose_r() %||% "general"
      }
    }

    # Read saved config for the active project, merged into the flat structure
    # the restore code below expects (response/params + per-column variables).
    read_saved_ <- function() {
      p <- active_project_r()
      if (is.null(p)) return(NULL)
      got <- tryCatch(
        get_project_settings(p$project_path, method = "mgcv",
                             purpose = proj_purpose_()),
        error = function(e) NULL)
      if (is.null(got)) return(NULL)
      saved <- list()
      if (!is.null(got$variables)) {
        v <- tryCatch(jsonlite::fromJSON(got$variables, simplifyVector = FALSE),
                      error = function(e) NULL)
        if (is.list(v)) saved <- utils::modifyList(saved, v)
      }
      if (!is.null(got$settings)) {
        s <- tryCatch(jsonlite::fromJSON(got$settings, simplifyVector = FALSE),
                      error = function(e) NULL)
        if (is.list(s)) saved <- utils::modifyList(saved, s)
      }
      if (length(saved) == 0L) NULL else saved
    }

    # --- Parameter presets ---
    apply_preset_ <- function(preset) {
      if (preset == "earth") {
        updateSelectInput(session, "default_basis", selected = "cr")
        updateNumericInput(session, "default_k", value = 0)
        updateNumericInput(session, "gamma", value = 1.4)
        updateCheckboxInput(session, "select", value = TRUE)
        session$sendCustomMessage("mgcv_preset_desc",
          "Earth refinement: cr basis, k=auto(10), gamma=1.4, select=TRUE. Earth knots are preserved and augmented with data quantiles.")
      } else {
        updateSelectInput(session, "default_basis", selected = "tp")
        updateNumericInput(session, "default_k", value = 0)
        updateNumericInput(session, "gamma", value = 1.2)
        updateCheckboxInput(session, "select", value = TRUE)
        session$sendCustomMessage("mgcv_preset_desc",
          "Discovery mode: tp basis, k=auto(10), gamma=1.2, select=TRUE")
      }
    }

    # Show/hide transform hint
    observeEvent(input$response_transform, {
      show <- !identical(input$response_transform, "none")
      session$sendCustomMessage("mgcv_toggle_el",
        list(id = ns("transform_hint"), show = show))
    })

    # Switch preset when user changes it
    observeEvent(input$preset, {
      apply_preset_(input$preset)
    })

    # Auto-switch to earth preset when earth knots arrive
    observeEvent(earth_knots_r(), {
      ek <- earth_knots_r()
      if (!is.null(ek)) {
        updateSelectInput(session, "preset", selected = "earth")
      }
    })

    # Update response choices when data changes
    observeEvent(data_r(), {
      df <- data_r()
      req(df)
      num_vars <- names(df)[vapply(df, is.numeric, logical(1))]
      fname <- filename_r()
      saved <- read_saved_()

      # earthUI is the source of truth for the Response: prefer its saved
      # target when it is a numeric column here; otherwise fall back to this
      # app's own saved response.
      ap_ <- active_project_r()
      earth_resp <- if (is.null(ap_)) NULL else
        earth_carryforward_(ap_$project_path, purpose_r())$response
      sel_resp <- if (!is.null(earth_resp) && earth_resp %in% num_vars)
                    earth_resp
                  else if (!is.null(saved) && !is.null(saved$response) &&
                           saved$response %in% num_vars) saved$response
                  else NULL
      if (!is.null(sel_resp)) {
        restoring_(TRUE)
        session$onFlushed(function() restoring_(FALSE), once = TRUE)
        updateSelectInput(session, "response", choices = num_vars,
                          selected = sel_resp)
        if (!is.null(saved$response_transform)) {
          updateSelectInput(session, "response_transform",
                            selected = saved$response_transform)
        }
        if (!is.null(saved$family)) {
          updateSelectInput(session, "family", selected = saved$family)
        }
        if (!is.null(saved$method)) {
          updateSelectInput(session, "method", selected = saved$method)
        }
        if (!is.null(saved$select)) {
          updateCheckboxInput(session, "select", value = saved$select)
        }
        if (!is.null(saved$gamma)) {
          updateNumericInput(session, "gamma", value = saved$gamma)
        }
        if (!is.null(saved$cv)) {
          updateCheckboxInput(session, "cv", value = saved$cv)
        }
        if (!is.null(saved$default_basis)) {
          updateSelectInput(session, "default_basis", selected = saved$default_basis)
        }
        if (!is.null(saved$default_k)) {
          updateNumericInput(session, "default_k", value = saved$default_k)
        }
        if (!is.null(saved$tensor_type)) {
          updateSelectInput(session, "tensor_type", selected = saved$tensor_type)
        }
        if (!is.null(saved$optimizer)) {
          updateSelectInput(session, "optimizer", selected = saved$optimizer)
        }
        if (!is.null(saved$scale)) {
          updateNumericInput(session, "scale", value = saved$scale)
        }
        if (!is.null(saved$discrete)) {
          updateCheckboxInput(session, "discrete", value = saved$discrete)
        }
        if (!is.null(saved$nthreads)) {
          updateNumericInput(session, "nthreads", value = saved$nthreads)
        }
      } else {
        updateSelectInput(session, "response", choices = num_vars)
      }

    })

    # Render the variable table
    output$var_table <- renderUI({
      df <- data_r()
      req(df)
      resp <- input$response
      cols <- names(df)
      # Exclude the target variable from the predictor table
      if (!is.null(resp) && nzchar(resp)) {
        cols <- setdiff(cols, resp)
      }
      detected <- detect_column_types_mgcv(df)
      detected <- detected[cols]
      # Pre-check the Factor box only for unambiguous (non-numeric) categoricals
      # (text/factor/logical). Numeric factors are the appraiser's call. A saved
      # per-variable choice still wins (restored below).
      fac_auto <- detect_categoricals_(df)

      fname <- filename_r()
      saved <- read_saved_()
      ek <- earth_knots_r()

      type_choices <- c("numeric", "integer", "character", "factor",
                        "logical", "Date", "POSIXct")
      appraiser <- purpose_r() %in% c("appraisal", "market")
      # Kept identical across the three sibling apps (specials.R). Covers
      # both appraisal and Market Area Analysis (appraiser) modes.
      special_choices <- special_roles_(appraiser)

      rows <- lapply(seq_along(cols), function(i) {
        var <- cols[i]
        det_type <- unname(detected[i])

        # Defaults: unchecked, auto-detected type, not linear. Factor is
        # pre-checked only for unambiguous (non-numeric) categoricals; numeric
        # columns are factors only if the appraiser ticks the box.
        # Special defaults to the tag most similar to the column name (or "no").
        inc_val     <- FALSE
        type_val    <- det_type
        lin_val     <- FALSE
        special_val <- special_default_for_(var, special_choices)
        fac_val     <- isTRUE(fac_auto[[var]])

        # Restore from saved settings
        has_saved_var <- !is.null(saved) && !is.null(saved$variables[[var]])
        if (has_saved_var) {
          sv <- saved$variables[[var]]
          if (!is.null(sv$inc)) inc_val <- isTRUE(sv$inc)
          if (!is.null(sv$type) && sv$type %in% type_choices) {
            type_val <- sv$type
          }
          if (!is.null(sv$linear)) lin_val <- isTRUE(sv$linear)
          if (!is.null(sv$special) && sv$special %in% special_choices) {
            special_val <- sv$special
          }
          if (!is.null(sv$factor)) fac_val <- isTRUE(sv$factor)
        }

        # Earth import SEEDS selections for variables the user has not yet
        # configured. A saved per-variable choice wins (e.g. you unselected a
        # variable and clicked "Save current settings as default"), so the
        # earth auto-include can't silently re-add it. Once a config is saved,
        # it already captures the earth-seeded selections, so nothing is lost.
        if (!is.null(ek) && !has_saved_var) {
          if (var %in% ek$predictors) {
            inc_val <- TRUE
          }
          if (var %in% ek$linpreds) {
            lin_val <- TRUE
          }
          if (var %in% ek$categoricals) {
            fac_val <- TRUE
          }
        }

        # Auto-exclude variables with > 50% NAs
        pct_na_var <- sum(is.na(df[[var]])) / nrow(df)
        if (pct_na_var > 0.5) {
          inc_val <- FALSE
        }

        type_opts <- paste(vapply(type_choices, function(t) {
          sel <- if (t == type_val) " selected" else ""
          sprintf("<option value=\"%s\"%s>%s</option>", t, sel, t)
        }, character(1)), collapse = "")

        special_opts <- paste(vapply(special_choices, function(s) {
          sel <- if (s == special_val) " selected" else ""
          sprintf("<option value=\"%s\"%s>%s</option>", s, sel, s)
        }, character(1)), collapse = "")

        inc_chk <- if (inc_val) " checked" else ""
        fac_chk <- if (fac_val) " checked" else ""
        lin_chk <- if (lin_val) " checked" else ""

        # NA count display
        n_na <- sum(is.na(df[[var]]))
        pct_na <- round(n_na / nrow(df) * 100, 0)
        na_color <- if (pct_na >= 50) "#bf616a"
                    else if (pct_na >= 20) "#d08770"
                    else if (n_na > 0) "#888"
                    else "#aaa"
        na_label <- if (n_na == 0) "\u2014"
                    else paste0(n_na, " (", pct_na, "%)")
        na_html <- sprintf(
          "<span style='color:%s;' title='%d of %d rows are NA'>%s</span>",
          na_color, n_na, nrow(df), na_label)

        tags$div(
          class = "mgcv-var-row", `data-var` = var,
          tags$div(class = "mgcv-cell mgcv-cell-name", title = var, var),
          tags$div(
            class = "mgcv-cell mgcv-cell-type",
            HTML(paste0("<select class='mgcv-type'",
                        " data-var='", var, "'>",
                        type_opts, "</select>"))
          ),
          tags$div(
            class = "mgcv-cell mgcv-cell-inc",
            HTML(paste0("<input type='checkbox'",
                        " class='mgcv-inc'",
                        " data-var='", var, "'",
                        inc_chk, ">"))
          ),
          tags$div(
            class = "mgcv-cell mgcv-cell-factor",
            HTML(paste0("<input type='checkbox'",
                        " class='mgcv-fac'",
                        " data-var='", var, "'",
                        fac_chk, ">"))
          ),
          tags$div(
            class = "mgcv-cell mgcv-cell-linear",
            HTML(paste0("<input type='checkbox'",
                        " class='mgcv-linear'",
                        " data-var='", var, "'",
                        lin_chk, ">"))
          ),
          if (appraiser) tags$div(
            class = "mgcv-cell mgcv-cell-special",
            HTML(paste0("<select class='mgcv-special'",
                        " data-var='", var, "'>",
                        special_opts, "</select>"))
          ),
          tags$div(class = "mgcv-cell mgcv-cell-na", HTML(na_html))
        )
      })

      angled <- "text-align:center; writing-mode:vertical-lr; transform:rotate(180deg); height:60px; line-height:1;"
      header <- tags$div(
        class = "mgcv-var-row mgcv-var-header",
        tags$div(class = "mgcv-cell mgcv-cell-name", "Variable"),
        tags$div(class = "mgcv-cell mgcv-cell-type", "Type"),
        tags$div(class = "mgcv-cell mgcv-cell-inc", style = angled, "Include"),
        tags$div(class = "mgcv-cell mgcv-cell-factor", style = angled, "Factor"),
        tags$div(class = "mgcv-cell mgcv-cell-linear", style = angled, "Linear"),
        if (appraiser) tags$div(class = "mgcv-cell mgcv-cell-special", "Special"),
        tags$div(class = "mgcv-cell mgcv-cell-na", "NAs")
      )

      js <- tags$script(HTML(sprintf("
        (function() {
          var ns = '%s';
          function gatherState() {
            var state = {};
            document.querySelectorAll('.mgcv-var-row[data-var]').forEach(
              function(row) {
                var v = row.getAttribute('data-var');
                var specialEl = row.querySelector('.mgcv-special');
                state[v] = {
                  inc:     row.querySelector('.mgcv-inc').checked,
                  type:    row.querySelector('.mgcv-type').value,
                  linear:  row.querySelector('.mgcv-linear').checked,
                  special: specialEl ? specialEl.value : 'no',
                  factor:  row.querySelector('.mgcv-fac').checked
                };
              }
            );
            Shiny.setInputValue(ns + 'var_state', state, {priority:'event'});
          }
          $(document).off('change.mgcvVars')
            .on('change.mgcvVars',
                '.mgcv-inc, .mgcv-type, .mgcv-linear, .mgcv-special, .mgcv-fac',
                gatherState);
          setTimeout(gatherState, 200);
        })();
      ", ns(""))))

      # Non-locking note: earth seeds the variable config (and spline knots),
      # but every selection stays editable -- unlike glmnetUI, mgcvUI fits the
      # GAM from the user's selections, so nothing is locked.
      earth_seed_note <- if (!is.null(ek)) {
        tags$div(
          class = "mgcv-earth-seed-note",
          style = paste0("font-size:0.8em; padding:4px 6px; margin-bottom:4px;",
                         " border-left:3px solid #5e81ac;",
                         " background:rgba(136,192,208,0.12);"),
          HTML(paste0(
            "Variable selections (Include / Linear / Factor) and spline knots ",
            "were <b>seeded</b> from the imported earth model. These are a ",
            "starting point \u2014 you can still change any of them."))
        )
      }

      tagList(earth_seed_note, header, rows, js)
    })

    # When earth knots arrive, re-set response
    observeEvent(earth_knots_r(), {
      ek <- earth_knots_r()
      if (!is.null(ek) && length(ek$target) == 1L) {
        df <- data_r()
        if (!is.null(df) && ek$target %in% names(df)) {
          updateSelectInput(session, "response", selected = ek$target)
        }
      }
    })

    # --- Allowed Interactions Matrix ---
    output$allowed_matrix_ui <- renderUI({
      st <- var_state()
      resp <- input$response
      if (length(st) == 0L) return(NULL)

      # Included smooth (non-linear, non-factor numeric) variables
      included <- character(0)
      for (var in names(st)) {
        s <- st[[var]]
        if (isTRUE(s$inc) && !identical(var, resp)) {
          typ <- s$type %||% "numeric"
          if (typ %in% c("numeric", "integer", "Date", "POSIXct") &&
              !isTRUE(s$linear) && !isTRUE(s$factor)) {
            included <- c(included, var)
          }
        }
      }

      if (length(included) < 2L) {
        return(tags$p("Need 2+ smooth predictors for interactions.",
                      style = "color: #888; font-size: 0.85em;"))
      }

      n <- length(included)
      ek <- earth_knots_r()

      # Info message when earth used degree == 1
      earth_deg1_msg <- NULL
      if (!is.null(ek) && identical(ek$degree, 1L) &&
            length(ek$interactions) == 0L) {
        earth_deg1_msg <- tags$p(
          "Earth model used degree = 1 (no interactions detected).",
          style = "color: #b8860b; font-size: 0.85em; margin-bottom: 6px;"
        )
      }

      # Header row with rotated column names (clickable to toggle)
      header_cells <- list(tags$th(""))
      for (j in seq_len(n)) {
        header_cells[[j + 1L]] <- tags$th(
          tags$div(included[j],
            class = "mgcv-int-var-toggle",
            `data-var` = included[j],
            style = paste("writing-mode: vertical-rl;",
                          "transform: rotate(180deg);",
                          "font-size: 0.75em;",
                          "white-space: nowrap;",
                          "padding: 4px 2px;",
                          "cursor: pointer;")),
          style = "text-align: center; vertical-align: bottom;"
        )
      }

      # Body rows
      body_rows <- list()
      for (i in seq_len(n)) {
        cells <- list(
          tags$td(
            tags$span(included[i],
              class = "mgcv-int-var-toggle",
              `data-var` = included[i]),
            style = paste("font-size: 0.8em; padding: 2px 6px;",
                          "white-space: nowrap; font-weight: 500;",
                          "cursor: pointer;"))
        )
        for (j in seq_len(n)) {
          if (j <= i) {
            cells[[j + 1L]] <- tags$td(
              if (i == j) "\u00b7" else "",
              style = "text-align: center; padding: 2px; color: #aaa;"
            )
          } else {
            # Check if earth actually detected this interaction
            pair <- sort(c(included[i], included[j]))
            key <- paste(pair, collapse = ":")
            from_earth <- !is.null(ek) &&
              !is.null(ek$interactions) &&
              key %in% names(ek$interactions)
            chk <- if (from_earth) " checked" else ""
            disabled <- if (from_earth) " disabled" else ""
            earth_cls <- if (from_earth) " mgcv-int-earth" else ""

            cells[[j + 1L]] <- tags$td(
              style = "text-align: center; padding: 2px;",
              class = if (from_earth) "mgcv-int-earth-cell" else "",
              HTML(paste0("<input type='checkbox' class='mgcv-int-cb",
                          earth_cls, "'",
                          " data-var1='", included[i], "'",
                          " data-var2='", included[j], "'",
                          chk, disabled, ">"))
            )
          }
        }
        body_rows[[i]] <- do.call(tags$tr, cells)
      }

      # JavaScript to gather checkbox state
      js <- tags$script(HTML(sprintf("
        (function() {
          var ns = '%s';
          function gatherInteractions() {
            var state = {};
            document.querySelectorAll('.mgcv-int-cb').forEach(function(cb) {
              var v1 = cb.getAttribute('data-var1');
              var v2 = cb.getAttribute('data-var2');
              var pair = [v1, v2].sort();
              state[pair.join(':')] = cb.checked;
            });
            Shiny.setInputValue(ns + 'interaction_matrix',
                                state, {priority:'event'});
          }
          Shiny.addCustomMessageHandler('mgcv_toggle_all_int',
            function(checked) {
              document.querySelectorAll('.mgcv-int-cb').forEach(
                function(cb) {
                  if (!cb.classList.contains('mgcv-int-earth')) {
                    cb.checked = checked;
                  }
                });
              gatherInteractions();
            });
          $(document).off('change.mgcvInts')
            .on('change.mgcvInts', '.mgcv-int-cb', gatherInteractions);
          $(document).off('click.mgcvVarToggle')
            .on('click.mgcvVarToggle', '.mgcv-int-var-toggle', function() {
              var varName = $(this).attr('data-var');
              var cbs = document.querySelectorAll(
                '.mgcv-int-cb[data-var1=\"' + varName + '\"],' +
                '.mgcv-int-cb[data-var2=\"' + varName + '\"]'
              );
              // If any are unchecked, check all; otherwise uncheck all
              var anyUnchecked = false;
              cbs.forEach(function(cb) { if (!cb.checked) anyUnchecked = true; });
              cbs.forEach(function(cb) { cb.checked = anyUnchecked; });
              gatherInteractions();
            });

          // --- Block from main effect: right-click variable label ---
          var mgcvBlk1 = {};
          try {
            var saved = JSON.parse(localStorage.getItem('mgcvUI_blk1'));
            if (saved) mgcvBlk1 = saved;
          } catch(e) {}

          function updateMgcvBlk1Labels() {
            document.querySelectorAll('.mgcv-int-var-toggle').forEach(function(el) {
              var v = el.getAttribute('data-var');
              var ind = el.querySelector('.blk1-indicator');
              if (mgcvBlk1[v]) {
                if (!ind) {
                  var sp = document.createElement('span');
                  sp.className = 'blk1-indicator';
                  sp.style.cssText = 'color:#000;font-weight:bold;font-size:0.85em;';
                  sp.textContent = ' 1';
                  el.appendChild(sp);
                }
              } else if (ind) {
                ind.remove();
              }
            });
          }

          function syncMgcvBlk1() {
            var blocked = [];
            for (var v in mgcvBlk1) { if (mgcvBlk1[v]) blocked.push(v); }
            Shiny.setInputValue(ns + 'block_main_effect',
              blocked.length > 0 ? blocked : null, {priority:'event'});
            try { localStorage.setItem('mgcvUI_blk1', JSON.stringify(mgcvBlk1)); } catch(e) {}
          }

          updateMgcvBlk1Labels();
          setTimeout(syncMgcvBlk1, 350);

          $(document).off('contextmenu.mgcvBlk1')
            .on('contextmenu.mgcvBlk1', '.mgcv-int-var-toggle', function(e) {
              e.preventDefault();
              var varName = $(this).attr('data-var');
              mgcvBlk1[varName] = !mgcvBlk1[varName];
              updateMgcvBlk1Labels();
              syncMgcvBlk1();
            });

          setTimeout(gatherInteractions, 300);
        })();
      ", ns(""))))

      tagList(
        earth_deg1_msg,
        tags$div(
          style = paste("overflow-x: auto; max-height: 300px;",
                        "overflow-y: auto; border: 1px solid #ddd;",
                        "border-radius: 4px; padding: 4px;"),
          tags$table(
            style = "border-collapse: collapse;",
            tags$thead(do.call(tags$tr, header_cells)),
            tags$tbody(body_rows)
          )
        ),
        js
      )
    })

    # Allow All / Clear All buttons
    observeEvent(input$allow_all_int, {
      session$sendCustomMessage("mgcv_toggle_all_int", TRUE)
    })
    observeEvent(input$clear_all_int, {
      session$sendCustomMessage("mgcv_toggle_all_int", FALSE)
    })

    # --- Factor-by-Smooth Interactions ---
    output$factor_by_smooth_ui <- renderUI({
      st <- var_state()
      resp <- input$response
      if (length(st) == 0L) return(NULL)

      # Identify included factor vars and smooth vars
      factor_vars <- character(0)
      smooth_vars <- character(0)
      for (var in names(st)) {
        s <- st[[var]]
        if (!isTRUE(s$inc) || identical(var, resp)) next
        typ <- s$type %||% "numeric"
        is_numeric <- typ %in% c("numeric", "integer", "Date", "POSIXct")
        if (isTRUE(s$factor) || !is_numeric) {
          factor_vars <- c(factor_vars, var)
        } else if (!isTRUE(s$linear)) {
          smooth_vars <- c(smooth_vars, var)
        }
      }

      if (length(factor_vars) == 0L || length(smooth_vars) == 0L) {
        return(tags$p(
          "Need 1+ factor and 1+ smooth predictor for by-interactions.",
          style = "color: #888; font-size: 0.85em;"))
      }

      # Build checkbox grid: rows = smooth vars, columns = factor vars
      header_cells <- list(tags$th("", style = "min-width: 80px;"))
      for (fv in factor_vars) {
        header_cells[[length(header_cells) + 1L]] <- tags$th(
          fv, style = "font-size: 0.75em; text-align: center; padding: 2px 6px;")
      }

      body_rows <- list()
      for (sv in smooth_vars) {
        cells <- list(tags$td(sv,
          style = "font-size: 0.8em; padding: 2px 6px; font-weight: 500;"))
        for (fv in factor_vars) {
          cells[[length(cells) + 1L]] <- tags$td(
            style = "text-align: center; padding: 2px;",
            HTML(paste0("<input type='checkbox' class='mgcv-by-cb'",
                        " data-smooth='", sv, "'",
                        " data-factor='", fv, "'>"))
          )
        }
        body_rows[[length(body_rows) + 1L]] <- do.call(tags$tr, cells)
      }

      js <- tags$script(HTML(sprintf("
        (function() {
          var ns = '%s';
          function gatherByInteractions() {
            var state = {};
            document.querySelectorAll('.mgcv-by-cb').forEach(function(cb) {
              var sv = cb.getAttribute('data-smooth');
              var fv = cb.getAttribute('data-factor');
              var key = sv + ':by:' + fv;
              state[key] = cb.checked;
            });
            Shiny.setInputValue(ns + 'by_interactions',
                                state, {priority:'event'});
          }
          $(document).off('change.mgcvBy')
            .on('change.mgcvBy', '.mgcv-by-cb', gatherByInteractions);
          setTimeout(gatherByInteractions, 300);
        })();
      ", ns(""))))

      tagList(
        tags$div(
          style = paste("overflow-x: auto; max-height: 250px;",
                        "overflow-y: auto; border: 1px solid #ddd;",
                        "border-radius: 4px; padding: 4px;"),
          tags$table(
            style = "border-collapse: collapse;",
            tags$thead(do.call(tags$tr, header_cells)),
            tags$tbody(body_rows)
          )
        ),
        js
      )
    })

    # Track the per-column variable state pushed from the client.
    observeEvent(input$var_state, {
      var_state(input$var_state)
    })

    # --- "Save current settings as default" buttons ---
    # Persist to the shared projects.sqlite under the mgcv columns, keyed by
    # (project, purpose). Variable configuration and parameters are saved
    # independently (two buttons, two columns), matching earthUI.
    observeEvent(input$save_varconfig, {
      p <- active_project_r()
      if (is.null(p)) {
        showNotification("Open a project first.", type = "error", duration = 5)
        return()
      }
      vars_blob <- list(
        response           = input$response %||% "",
        response_transform = input$response_transform %||% "none",
        variables          = input$var_state %||% list()
      )
      tryCatch({
        set_project_settings(p$project_path,
          variables = jsonlite::toJSON(vars_blob, auto_unbox = TRUE, null = "null"),
          method = "mgcv", purpose = proj_purpose_())
        showNotification("Variable configuration saved as the project default.",
                         type = "message", duration = 4)
      }, error = function(e)
        showNotification(paste("Save error:", e$message), type = "error",
                         duration = 8))
    })

    observeEvent(input$save_params, {
      p <- active_project_r()
      if (is.null(p)) {
        showNotification("Open a project first.", type = "error", duration = 5)
        return()
      }
      params_blob <- list(
        family = input$family, method = input$method, select = input$select,
        gamma = input$gamma, cv = input$cv, default_basis = input$default_basis,
        default_k = input$default_k, tensor_type = input$tensor_type,
        optimizer = input$optimizer, scale = input$scale,
        discrete = input$discrete, nthreads = input$nthreads
      )
      tryCatch({
        set_project_settings(p$project_path,
          settings = jsonlite::toJSON(params_blob, auto_unbox = TRUE, null = "null"),
          method = "mgcv", purpose = proj_purpose_())
        showNotification("Parameters saved as the project default.",
                         type = "message", duration = 4)
      }, error = function(e)
        showNotification(paste("Save error:", e$message), type = "error",
                         duration = 8))
    })

    # Return configuration
    reactive({
      st <- var_state()
      resp <- input$response
      default_bs <- input$default_basis %||% "tp"
      default_k_raw <- input$default_k %||% 0
      default_k <- if (default_k_raw == 0) NULL else as.integer(default_k_raw)

      if (length(st) == 0L) {
        return(list(
          response = resp, predictors = character(0),
          smooth_specs = list(), family = input$family,
          method = input$method, select = input$select,
          gamma = input$gamma, cv = input$cv
        ))
      }

      ek <- earth_knots_r()
      included <- character(0)
      specials <- character(0)
      for (var in names(st)) {
        if (isTRUE(st[[var]]$inc) && !identical(var, resp)) {
          included <- c(included, var)
        }
        sp <- st[[var]]$special %||% "no"
        if (sp != "no") {
          specials <- c(specials, stats::setNames(sp, var))
        }
      }

      # Variables blocked from main effect (interaction only)
      blocked <- input$block_main_effect
      if (is.null(blocked)) blocked <- character(0)

      specs <- lapply(included, function(var) {
        # Skip main effect for blocked variables
        if (var %in% blocked) return(NULL)

        s <- st[[var]]
        typ <- s$type %||% "numeric"
        is_numeric <- typ %in% c("numeric", "integer", "Date", "POSIXct")

        if (isTRUE(s$factor) || !is_numeric || isTRUE(s$linear)) {
          # Parametric term. Dummy-code it as a factor when the user checked
          # Factor, or when the column is a non-numeric categorical type. A
          # plain numeric "Linear" term stays numeric (a single slope).
          is_factor <- isTRUE(s$factor) ||
            typ %in% c("character", "factor", "logical")
          list(vars = var, type = "linear", bs = NULL, k = NULL,
               factor = is_factor)
        } else {
          # Use earth knots basis if available
          has_knots <- !is.null(ek) && var %in% names(ek$knots)
          bs <- if (has_knots) "cr" else default_bs
          k <- if (has_knots) {
            length(ek$knots[[var]])
          } else {
            default_k
          }
          list(vars = var, type = "s", bs = bs, k = k)
        }
      })
      specs <- specs[!vapply(specs, is.null, logical(1))]

      # Add interaction specs from allowed matrix
      int_matrix <- input$interaction_matrix
      tensor_type <- input$tensor_type %||% "ti"
      if (!is.null(int_matrix) && length(int_matrix) > 0L) {
        for (key in names(int_matrix)) {
          if (isTRUE(int_matrix[[key]])) {
            pair <- strsplit(key, ":")[[1L]]
            if (all(pair %in% included)) {
              specs <- c(specs, list(
                list(vars = pair, type = tensor_type, bs = NULL, k = NULL)
              ))
            }
          }
        }
      }

      # Add factor-by-smooth interaction specs
      by_matrix <- input$by_interactions
      if (!is.null(by_matrix) && length(by_matrix) > 0L) {
        for (key in names(by_matrix)) {
          if (isTRUE(by_matrix[[key]])) {
            parts <- strsplit(key, ":by:")[[1L]]
            smooth_var <- parts[1L]
            factor_var <- parts[2L]
            if (smooth_var %in% included && factor_var %in% included) {
              # Find the existing smooth spec for this var to inherit bs/k
              existing_spec <- NULL
              for (sp in specs) {
                if (identical(sp$vars, smooth_var) && sp$type == "s") {
                  existing_spec <- sp
                  break
                }
              }
              bs <- if (!is.null(existing_spec)) existing_spec$bs else default_bs
              k <- if (!is.null(existing_spec)) existing_spec$k else default_k
              specs <- c(specs, list(
                list(vars = smooth_var, type = "s", bs = bs, k = k,
                     by = factor_var)
              ))
            }
          }
        }
      }

      # Derive weights column from specials
      wt_col <- names(which(specials == "weight"))
      if (length(wt_col) == 0L) wt_col <- NULL else wt_col <- wt_col[1L]

      list(
        response           = resp,
        response_transform = input$response_transform %||% "none",
        predictors         = included,
        smooth_specs       = specs,
        family             = input$family,
        method             = input$method,
        select             = input$select,
        gamma              = input$gamma,
        cv                 = input$cv,
        specials           = specials,
        weights_col        = wt_col,
        optimizer          = input$optimizer,
        scale              = input$scale,
        discrete           = input$discrete,
        nthreads           = input$nthreads
      )
    })
  })
}
