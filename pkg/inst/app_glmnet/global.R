library(earthUI)
library(shiny)
library(glmnet)
library(ggplot2)
library(DT)
library(bslib)

# Allow uploads up to 3 GB (earthUI result files can be very large)
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
  message("glmnetUI: could not load Roboto Condensed font: ", e$message,
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

# Helper: font family for plots (consistent fallback)
glmnet_font_family_ <- function() {
  if (requireNamespace("sysfonts", quietly = TRUE) &&
      "Roboto Condensed" %in% sysfonts::font_families()) {
    "Roboto Condensed"
  } else {
    "sans"
  }
}
