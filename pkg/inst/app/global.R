library(earthui)

# Load Roboto Condensed for R graphics (ggplot2 + base R)
if (requireNamespace("sysfonts", quietly = TRUE) &&
    requireNamespace("showtext", quietly = TRUE)) {
  sysfonts::font_add_google("Roboto Condensed", "Roboto Condensed")
  showtext::showtext_auto()
}

# Set global ggplot2 theme with Roboto Condensed
ggplot2::theme_set(
  ggplot2::theme_minimal(base_family = "Roboto Condensed")
)

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
