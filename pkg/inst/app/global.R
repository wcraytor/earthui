library(earthUI)

# Load Roboto Condensed for R graphics (ggplot2 + base R)
if (requireNamespace("sysfonts", quietly = TRUE) &&
    requireNamespace("showtext", quietly = TRUE)) {
  sysfonts::font_add_google("Roboto Condensed", "Roboto Condensed")
  showtext::showtext_auto()
  ggplot2::theme_set(
    ggplot2::theme_minimal(base_family = "Roboto Condensed")
  )
} else {
  ggplot2::theme_set(ggplot2::theme_minimal(base_family = "sans"))
}

# Enable thematic so ggplot2 auto-adapts to current theme
if (requireNamespace("thematic", quietly = TRUE)) {
  thematic::thematic_shiny()
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
