# Appraisal / liability disclaimer shown in the About box. Kept in R/ (ASCII,
# HTML-entity encoded) so the same wording is reused by the app and stays in one
# place. Mirrors the short strip shown at the top of the UI.

#' Full appraisal / liability disclaimer as HTML (internal)
#'
#' Used by the app's About dialog. The short, always-visible strip at the top of
#' the UI carries a condensed version of the same message.
#' @return A [shiny::HTML()] fragment.
#' @noRd
appraisal_disclaimer_html_ <- function() {
  shiny::HTML(paste0(
    "<p><b>This software is provided for analytical, research, and educational ",
    "purposes only. It does not produce an appraisal and is not a substitute ",
    "for the judgment of a qualified, licensed or certified real estate ",
    "appraiser.</b></p>",
    "<p>The models generate <b>statistical estimates</b> from the data you ",
    "supply. Those estimates:</p>",
    "<ul>",
    "<li>are <b>not</b> an appraisal, valuation, or formal opinion of value;</li>",
    "<li>must <b>not</b> be used as the basis for lending, mortgage, tax, ",
    "insurance, legal, investment, or transactional decisions;</li>",
    "<li>may be inaccurate, incomplete, or unsuitable for any particular ",
    "property or purpose; and</li>",
    "<li>depend entirely on the quality, accuracy, and representativeness of ",
    "the data, comparables, and settings chosen by the user.</li>",
    "</ul>",
    "<p>Any value conclusion intended for professional or transactional use ",
    "<b>must be independently developed, reviewed, and signed by a qualified ",
    "appraiser</b> in accordance with all applicable laws and professional ",
    "standards (for example, the Uniform Standards of Professional Appraisal ",
    "Practice (USPAP) in the United States, or the equivalent standards in your ",
    "jurisdiction).</p>",
    "<p><b>No warranty.</b> This program is distributed in the hope that it ",
    "will be useful, but WITHOUT ANY WARRANTY &mdash; without even the implied ",
    "warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE, as set ",
    "out in the GNU Affero General Public License v3.0. To the maximum extent ",
    "permitted by law, the authors and contributors accept no liability for any ",
    "loss or damage arising from the use of, or reliance on, this software or ",
    "its output. <b>You use it at your own risk and are solely responsible for ",
    "verifying all inputs, assumptions, and results.</b></p>"))
}

#' Help / technical-support dialog (internal)
#'
#' Builds the Help pop-up: a pre-addressed support email opened via a `mailto:`
#' link (no mail server required). All technical-assistance requests and bug
#' reports go to support@valuation-engineer.com.
#' @param app Package/app name (e.g. "earthUI"); used in the subject + version.
#' @return A [shiny::modalDialog()].
#' @noRd
support_request_modal_ <- function(app) {
  addr <- "support@valuation-engineer.com"
  ver  <- tryCatch(as.character(utils::packageVersion(app)),
                   error = function(e) "")
  os   <- tryCatch(paste(Sys.info()[["sysname"]], Sys.info()[["release"]]),
                   error = function(e) "")
  subj <- paste0(app, " ", ver, " - Support request")
  body <- paste0(
    "Please describe your issue or request below.\n\n",
    "If you are reporting a bug, include the steps to reproduce:\n",
    "1.\n2.\n3.\n\n",
    "What you expected to happen:\n\n",
    "What actually happened:\n\n",
    "--------------------------------\n",
    "App: ", app, " ", ver, "\n",
    "OS: ", os, "\n")
  href <- paste0("mailto:", addr,
                 "?subject=", utils::URLencode(subj, reserved = TRUE),
                 "&body=", utils::URLencode(body, reserved = TRUE))
  shiny::modalDialog(
    title = shiny::HTML("&#128231; Help &amp; Technical Support"),
    size = "l", easyClose = TRUE,
    shiny::HTML(paste0(
      "<p>For technical assistance and bug reports, email our support team. ",
      "Clicking the button below opens a pre-addressed message in your email ",
      "application &mdash; just add your details and send.</p>",
      "<p>Please include:</p>",
      "<ul><li>what you were doing and what went wrong;</li>",
      "<li>the steps to reproduce the problem;</li>",
      "<li>any error message that was shown; and</li>",
      "<li>a screenshot, if helpful.</li></ul>")),
    shiny::tags$p(
      shiny::tags$a(href = href, class = "btn btn-primary",
                    shiny::HTML("&#9993; Email support"))),
    shiny::tags$p(class = "text-muted small",
      shiny::HTML(paste0("Or email us directly at <b>", addr, "</b>."))),
    footer = shiny::modalButton("Close"))
}
