library(earthui)

# Helper: wrap an input with a "?" popover icon
param_with_help <- function(input_el, help_text) {
  tags$div(
    style = "position: relative;",
    input_el,
    tags$span(
      class = "eui-param-help",
      `data-bs-toggle` = "popover",
      `data-bs-trigger` = "hover focus",
      `data-bs-content` = help_text,
      `data-bs-placement` = "left",
      "?"
    )
  )
}
