fluidPage(
  withMathJax(),
  theme = bslib::bs_theme(version = 5, bootswatch = "flatly"),
  tags$head(
    tags$script(type = "text/x-mathjax-config", HTML("
      MathJax.Hub.Config({
        'HTML-CSS': { preferredFont: 'TeX', scale: 90 }
      });
    ")),
    tags$style(HTML("
    .dataTable td, .dataTable th { padding: 4px 8px !important; }
    .dataTables_wrapper { font-size: 0.9em; }
    .dataTable td { max-width: 300px; overflow: hidden; text-overflow: ellipsis; white-space: nowrap; cursor: pointer; }
    #eui-cell-popup { display: none; }
    .eui-popup-backdrop { position: fixed; top: 0; left: 0; width: 100%; height: 100%; background: rgba(0,0,0,0.3); z-index: 9998; }
    .eui-popup-content { position: fixed; top: 50%; left: 50%; transform: translate(-50%, -50%); background: #fff; border-radius: 8px; padding: 20px; max-width: 80vw; max-height: 70vh; overflow: auto; z-index: 9999; box-shadow: 0 4px 20px rgba(0,0,0,0.3); }
    .eui-popup-content pre { white-space: pre-wrap; word-wrap: break-word; margin-bottom: 12px; font-size: 0.9em; }
    .eui-popup-close { float: right; }
    .MathJax_Display { text-align: left !important; margin-left: 1em !important; }
    .eui-param-help { position: absolute; top: 0; right: 0; width: 18px; height: 18px; border-radius: 50%; background: #5bc0de; color: #fff; font-size: 11px; font-weight: bold; text-align: center; line-height: 18px; cursor: pointer; z-index: 10; }
    .eui-param-help:hover { background: #31b0d5; }
    #eui-theme-toggle { position: fixed; top: 12px; right: 20px; z-index: 10000; width: 38px; height: 38px; border-radius: 50%; border: 2px solid #ccc; background: #fff; font-size: 18px; cursor: pointer; display: flex; align-items: center; justify-content: center; box-shadow: 0 2px 6px rgba(0,0,0,0.15); transition: all 0.3s; }
    #eui-theme-toggle:hover { box-shadow: 0 2px 10px rgba(0,0,0,0.25); }
    [data-bs-theme='dark'] #eui-theme-toggle { background: #2c3e50; border-color: #555; }
    [data-bs-theme='dark'] .eui-popup-content { background: #2c3e50; color: #ecf0f1; }
    [data-bs-theme='dark'] details > summary { color: #ecf0f1 !important; }
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
    function toggleTheme() {
      var html = document.documentElement;
      var btn = document.getElementById('eui-theme-toggle');
      if (html.getAttribute('data-bs-theme') === 'dark') {
        html.removeAttribute('data-bs-theme');
        btn.innerHTML = '\\u263E';
        try { localStorage.setItem('earthui_theme', 'light'); } catch(e) {}
      } else {
        html.setAttribute('data-bs-theme', 'dark');
        btn.innerHTML = '\\u2600';
        try { localStorage.setItem('earthui_theme', 'dark'); } catch(e) {}
      }
    }
    (function() {
      var saved = null;
      try { saved = localStorage.getItem('earthui_theme'); } catch(e) {}
      if (saved === 'dark') {
        document.documentElement.setAttribute('data-bs-theme', 'dark');
        var btn = document.getElementById('eui-theme-toggle');
        if (btn) btn.innerHTML = '\\u2600';
      }
    })();
  ")),
  titlePanel("earthui - Interactive Earth Model Builder"),

  sidebarLayout(
    sidebarPanel(
      width = 4,

      # --- Data Import ---
      h4("1. Import Data"),
      fileInput("file_input", "Choose CSV or Excel file",
                accept = c(".csv", ".xlsx", ".xls")),
      uiOutput("sheet_selector"),
      uiOutput("data_preview_info"),
      hr(),

      # --- Variable Configuration ---
      conditionalPanel(
        condition = "output.data_loaded",
        h4("2. Variable Configuration"),
        uiOutput("target_selector"),
        h5("Predictor Settings"),
        tags$p(style = "font-size: 0.8em; color: #666; margin-bottom: 5px;",
               "Inc = include as predictor, Factor = treat as categorical, Linear = linear-only (no hinges)"),
        div(style = "max-height: 400px; overflow-y: auto; border: 1px solid #ddd; border-radius: 4px; padding: 4px;",
            uiOutput("variable_table")),
        hr(),

        # --- Model Configuration ---
        h4("3. Model Configuration"),
        selectInput("degree", "Max interaction order (degree)",
                    choices = 1:4, selected = 1),
        conditionalPanel(
          condition = "input.degree >= 2",
          div(
            class = "alert alert-warning",
            style = "font-size: 0.85em;",
            strong("Note:"),
            "Interaction terms increase the risk of overfitting.",
            "Cross-validation has been enabled (10-fold)."
          ),
          h5("Allowed Interactions"),
          p("Uncheck pairs to disallow specific interactions.",
            style = "font-size: 0.85em; color: #666;"),
          uiOutput("allowed_matrix_ui")
        ),

        # Advanced parameters (collapsible)
        tags$details(
          tags$summary(
            style = "cursor: pointer; color: #2c3e50; font-weight: bold;",
            "Advanced Parameters"
          ),
          div(
            style = "padding-top: 10px;",
            actionButton("param_info", "Parameter Info",
                         class = "btn-info btn-sm",
                         style = "margin-bottom: 10px; width: 100%;"),

            h6("Forward Pass", style = "margin-top: 8px; border-bottom: 1px solid #ccc;"),
            param_with_help(
              numericInput("nk", "Max terms before pruning (nk)", value = NA, min = 1, step = 1),
              "Maximum number of model terms before pruning (includes intercept). Default is semi-automatically calculated from the number of predictors."),
            param_with_help(
              numericInput("thresh", "Forward step threshold (thresh)", value = 0.001, min = 0, step = 0.0001),
              "Forward pass terminates if adding a term changes RSq by less than this value. Default 0.001."),
            param_with_help(
              numericInput("penalty", "GCV penalty per knot (penalty)", value = NA, min = -1, step = 0.5),
              "GCV penalty per knot. Default is 3 if degree>1, else 2. Use 0 to penalize only terms. Use -1 for no penalty (GCV = RSS/n)."),
            param_with_help(
              numericInput("minspan", "Min span (minspan)", value = NA, step = 1),
              "Minimum observations between knots. Default 0 = auto-calculated. Negative values specify max knots per predictor (e.g., -3 = three evenly spaced knots)."),
            param_with_help(
              numericInput("endspan", "End span (endspan)", value = NA, min = 0, step = 1),
              "Minimum observations before first and after last knot. Default 0 = auto-calculated. Be wary of reducing this for predictions near data limits."),
            param_with_help(
              numericInput("newvar_penalty", "New variable penalty (newvar.penalty)", value = 0, min = 0, step = 0.01),
              "Penalty for adding a new variable (Friedman's gamma). Default 0. Non-zero values (0.01-0.2) prefer reusing existing variables, simplifying interpretation."),
            param_with_help(
              numericInput("fast_k", "fast.k", value = 20, min = 0, step = 1),
              "Max parent terms per forward step (Fast MARS). Default 20. Set 0 to disable. Lower = faster, higher = potentially better model."),
            param_with_help(
              numericInput("fast_beta", "fast.beta", value = 1, min = 0, step = 0.1),
              "Fast MARS ageing coefficient. Default 1. A value of 0 sometimes gives better results."),
            param_with_help(
              checkboxInput("auto_linpreds", "Auto.linpreds", value = TRUE),
              "Default TRUE. If the best knot is at the predictor minimum, add the predictor linearly (no hinge). Only affects predictions outside training data range."),

            h6("Pruning", style = "margin-top: 8px; border-bottom: 1px solid #ccc;"),
            param_with_help(
              selectInput("pmethod", "Pruning method (pmethod)",
                          choices = c("backward", "none", "exhaustive", "forward", "seqrep", "cv"),
                          selected = "backward"),
              "Pruning method. Default 'backward'. Use 'cv' with nfold to select terms by cross-validation. Use 'none' to retain all forward pass terms."),
            param_with_help(
              numericInput("nprune", "Max terms after pruning (nprune)", value = NA, min = 1, step = 1),
              "Maximum terms (including intercept) in pruned model. Default NULL = all terms from forward pass, after pruning."),

            h6("Cross Validation", style = "margin-top: 8px; border-bottom: 1px solid #ccc;"),
            param_with_help(
              numericInput("nfold_override", "CV folds (nfold)", value = NA, min = 0, step = 1),
              "Number of CV folds. Default 0 (no CV). Auto-set to 10 when degree >= 2. Use trace=0.5 to trace CV."),
            param_with_help(
              numericInput("ncross", "ncross", value = 1, min = 1, step = 1),
              "Number of cross-validations (each has nfold folds). Default 1. Use higher values (e.g., 30) with variance models."),
            param_with_help(
              checkboxInput("stratify", "Stratify CV samples (stratify)", value = TRUE),
              "Default TRUE. Stratify CV samples so each fold has approximately equal response distribution."),

            h6("Variance Model", style = "margin-top: 8px; border-bottom: 1px solid #ccc;"),
            param_with_help(
              selectInput("varmod_method", "varmod.method",
                          choices = c("none", "const", "lm", "rlm", "earth", "gam",
                                      "power", "power0", "x.lm", "x.rlm", "x.earth", "x.gam"),
                          selected = "none"),
              "Variance model method. Requires nfold and ncross. Use trace=0.3 to trace. 'lm','rlm','earth','gam' regress on predicted response; 'x.*' variants regress on predictors."),
            param_with_help(
              numericInput("varmod_exponent", "varmod.exponent", value = 1, min = 0, step = 0.1),
              "Power transform for residual regression. Default 1. Use 0.5 if std dev increases with square root of response."),
            param_with_help(
              numericInput("varmod_conv", "varmod.conv", value = 1, step = 0.1),
              "Convergence criterion (%) for IRLS in variance model. Default 1. Negative values force that many iterations."),
            param_with_help(
              numericInput("varmod_clamp", "varmod.clamp", value = 0.1, min = 0, step = 0.01),
              "Min estimated std dev = varmod.clamp * mean(sd(residuals)). Default 0.1. Prevents negative or tiny std dev estimates."),
            param_with_help(
              numericInput("varmod_minspan", "varmod.minspan", value = -3, step = 1),
              "minspan for internal earth call in variance model. Default -3 (three evenly spaced knots per predictor)."),

            h6("GLM", style = "margin-top: 8px; border-bottom: 1px solid #ccc;"),
            param_with_help(
              selectInput("glm_family", "GLM family (glm)",
                          choices = c("none", "gaussian", "binomial", "poisson"),
                          selected = "none"),
              "Optional GLM family applied to earth basis functions. Example: 'binomial' for binary outcomes."),

            h6("Other", style = "margin-top: 8px; border-bottom: 1px solid #ccc;"),
            param_with_help(
              selectInput("trace", "Trace level (trace)",
                          choices = c("0" = "0", "0.3" = "0.3", "0.5" = "0.5",
                                      "1" = "1", "2" = "2", "3" = "3",
                                      "4" = "4", "5" = "5"),
                          selected = "0"),
              "0=none, 0.3=variance model, 0.5=CV, 1=overview, 2=forward pass, 3=pruning, 4=model mats/pruning details, 5=full details."),
            param_with_help(
              checkboxInput("keepxy", "Keep x,y in model (keepxy)", value = FALSE),
              "Default FALSE. Retain x, y, subset, weights in model object. Required for some CV statistics. Makes CV slower."),
            param_with_help(
              checkboxInput("scale_y", "Scale response (Scale.y)", value = TRUE),
              "Default TRUE. Scale response internally (subtract mean, divide by sd). Provides better numeric stability."),
            param_with_help(
              numericInput("adjust_endspan", "Adjust.endspan", value = 2, min = 1, step = 0.5),
              "In interaction terms, endspan is multiplied by this value. Default 2. Reduces overfitting at data boundaries."),
            param_with_help(
              numericInput("exhaustive_tol", "Exhaustive.tol", value = 1e-10, min = 0, step = 1e-11),
              "Default 1e-10. If reciprocal condition number < this, forces pmethod='backward'. Only applies with pmethod='exhaustive'."),
            param_with_help(
              checkboxInput("use_beta_cache", "Use.beta.cache", value = TRUE),
              "Default TRUE. Cache regression coefficients in forward pass for 20%+ speed improvement. Uses more memory."),
            param_with_help(
              checkboxInput("force_xtx_prune", "Force.xtx.prune", value = FALSE),
              "Default FALSE. Force X'X-based subset evaluation in pruning instead of QR-based leaps. Advanced use only."),
            param_with_help(
              checkboxInput("get_leverages", "Get.leverages", value = TRUE),
              "Default TRUE (unless >100k cases). Calculate diagonal hat values for diagnostics."),
            param_with_help(
              checkboxInput("force_weights", "Force.weights", value = FALSE),
              "Default FALSE. For testing: force weighted code path even without weights.")
          )
        ),
        hr(),

        # --- Run ---
        actionButton("run_model", "Fit Earth Model",
                     class = "btn-primary btn-lg",
                     style = "width: 100%; margin-top: 10px;"),
        hr(),

        # --- Export ---
        conditionalPanel(
          condition = "output.model_fitted",
          h4("5. Export Report"),
          selectInput("export_format", "Format",
                      choices = c("HTML" = "html", "PDF" = "pdf",
                                  "Word" = "docx")),
          downloadButton("export_report", "Download Report",
                         class = "btn-success",
                         style = "width: 100%;")
        )
      )
    ),

    mainPanel(
      width = 8,
      conditionalPanel(
        condition = "!output.data_loaded",
        div(
          style = "text-align: center; padding: 80px 20px; color: #888;",
          h3("Welcome to earthui"),
          p("Upload a CSV or Excel file to get started."),
          p("Build and explore Earth (MARS-style) models interactively.")
        )
      ),
      conditionalPanel(
        condition = "output.data_loaded && !output.model_fitted",
        h4("Data Preview"),
        DT::dataTableOutput("data_preview")
      ),
      conditionalPanel(
        condition = "output.model_fitted",
        tabsetPanel(
          id = "results_tabs",
          tabPanel(
            "Data",
            br(),
            DT::dataTableOutput("data_preview_tab")
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
            uiOutput("contrib_variable_selector"),
            plotOutput("contrib_plot", height = "400px")
          ),
          tabPanel(
            "Correlation",
            br(),
            plotOutput("correlation_plot", height = "550px")
          ),
          tabPanel(
            "Diagnostics",
            br(),
            fluidRow(
              column(6, plotOutput("residuals_plot", height = "350px")),
              column(6, plotOutput("qq_plot", height = "350px"))
            ),
            br(),
            plotOutput("actual_vs_predicted_plot", height = "400px")
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
  )
)
