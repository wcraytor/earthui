fluidPage(
  theme = bslib::bs_theme(version = 5, bootswatch = "flatly"),
  titlePanel("earthui - Interactive Earth Model Builder"),

  sidebarLayout(
    sidebarPanel(
      width = 4,

      # --- Data Import ---
      h4("1. Import Data"),
      fileInput("file_input", "Choose CSV or Excel file",
                accept = c(".csv", ".xlsx", ".xls")),
      uiOutput("data_preview_info"),
      hr(),

      # --- Variable Configuration ---
      conditionalPanel(
        condition = "output.data_loaded",
        h4("2. Variable Configuration"),
        uiOutput("target_selector"),
        uiOutput("predictor_selector"),
        uiOutput("categorical_selector"),
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
