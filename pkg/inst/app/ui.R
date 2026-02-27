fluidPage(
  theme = bslib::bs_theme(version = 5, bootswatch = "flatly"),
  tags$head(tags$style(HTML("
    .dataTable td, .dataTable th { padding: 4px 8px !important; }
    .dataTables_wrapper { font-size: 0.9em; }
    .dataTable td { max-width: 300px; overflow: hidden; text-overflow: ellipsis; white-space: nowrap; cursor: pointer; }
    #eui-cell-popup { display: none; }
    .eui-popup-backdrop { position: fixed; top: 0; left: 0; width: 100%; height: 100%; background: rgba(0,0,0,0.3); z-index: 9998; }
    .eui-popup-content { position: fixed; top: 50%; left: 50%; transform: translate(-50%, -50%); background: #fff; border-radius: 8px; padding: 20px; max-width: 80vw; max-height: 70vh; overflow: auto; z-index: 9999; box-shadow: 0 4px 20px rgba(0,0,0,0.3); }
    .eui-popup-content pre { white-space: pre-wrap; word-wrap: break-word; margin-bottom: 12px; font-size: 0.9em; }
    .eui-popup-close { float: right; }
    .eui-equation { margin-left: 1in; border-collapse: collapse; font-family: 'Cambria Math', 'Latin Modern Math', 'STIX Two Math', 'Times New Roman', Georgia, serif; font-size: 1.05em; }
    .eui-eq-label { vertical-align: top; white-space: nowrap; padding-right: 2em; text-align: left; font-weight: bold; padding-bottom: 0.4em; }
    .eui-eq-expr { text-align: left; vertical-align: top; padding-bottom: 0.4em; }
  "))),
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
        selectInput("degree", "Degree (max interaction order)",
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
            numericInput("nprune", "Max terms (nprune)",
                         value = NA, min = 1, step = 1),
            numericInput("thresh", "Forward step threshold",
                         value = 0.001, min = 0, step = 0.0001),
            numericInput("penalty", "GCV penalty per knot (d)",
                         value = NA, min = 0, step = 0.5),
            numericInput("minspan", "Min span", value = NA, min = 0, step = 1),
            numericInput("endspan", "End span", value = NA, min = 0, step = 1),
            numericInput("fast_k", "fast.k", value = 20, min = 0, step = 1),
            numericInput("nfold_override", "CV folds (override)",
                         value = NA, min = 0, step = 1),
            selectInput("pmethod", "Pruning method",
                        choices = c("backward", "none", "exhaustive",
                                    "forward", "seqrep", "cv"),
                        selected = "backward"),
            selectInput("glm_family", "GLM family (optional)",
                        choices = c("none", "gaussian", "binomial", "poisson"),
                        selected = "none")
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
            div(style = "overflow-x: auto; padding: 10px;",
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
            "Partial Dependence",
            br(),
            uiOutput("pd_variable_selector"),
            plotOutput("pd_plot", height = "400px")
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
          )
        )
      )
    )
  )
)
