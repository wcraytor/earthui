# Appraisal "Special" column-role definitions and the default-guesser used
# by the predictor settings table. Absorbed from valengrCore (2026-07-03) so
# earthUI is self-contained; glmnetUI/mgcvUI carry their own copies.

#' Special column-role options for the predictor settings dropdown
#'
#' The role tags offered in the "Special" dropdown of the variable
#' configuration table. The full appraisal set is offered in the appraiser
#' modes --- appraisal *and* Market Area Analysis (`purpose %in%
#' c("appraisal", "market")`); other purposes expose only the universal roles.
#'
#' @param appraiser Logical; `TRUE` for appraisal or market-area-analysis
#'   modes. Defaults to `TRUE`.
#' @return A character vector of role tags.
#' @export
special_roles_ <- function(appraiser = TRUE) {
  if (isTRUE(appraiser)) {
    c("actual_age", "area", "concessions", "contract_date",
      "display_only", "dom", "effective_age", "latitude",
      "listing_date", "living_area", "longitude", "lot_size",
      "no", "sale_age", "sale_type", "site_dimensions", "weight")
  } else {
    c("no", "weight")
  }
}

#' Guess the default Special tag from a column name
#'
#' Returns the role tag most similar to a column name (or `"no"` if none is a
#' reasonable match). Conservative: generic words (`"area"`, `"age"`) match a
#' whole name only, never a sub-token, so `area_id` / `garage_spaces` stay
#' `"no"`.
#'
#' @param name A column name.
#' @param tags Candidate role tags (typically [special_roles_()]).
#' @return The best-matching tag, or `"no"`.
#' @export
special_default_for_ <- function(name, tags) {
  cand <- setdiff(tags, c("no", "display_only"))
  if (length(cand) == 0L || is.null(name) || !nzchar(name)) return("no")

  norm <- function(x) gsub("[^a-z0-9]+", "", tolower(x))
  tok <- function(x) {
    x <- gsub("([a-z0-9])([A-Z])", "\\1 \\2", x)        # camelCase -> space
    parts <- strsplit(tolower(x), "[^a-z0-9]+")[[1L]]
    parts[nzchar(parts)]
  }
  # Known abbreviations / synonyms -> canonical tag.
  syn <- list(
    latitude        = c("lat"),
    longitude       = c("long", "lon", "lng"),
    living_area     = c("livingarea", "gla", "sqft", "livingsqft", "livingsf",
                        "grosslivingarea", "livarea", "livsf", "livingsq"),
    lot_size        = c("lotsize", "lotsf", "lotsqft", "lotarea"),
    site_dimensions = c("sitedimensions", "sitedim", "sitedims"),
    actual_age      = c("age", "actualage"),
    effective_age   = c("effectiveage", "effage"),
    sale_age        = c("saleage", "ageofsale", "daystosale"),
    sale_type       = c("saletype", "typeofsale"),
    contract_date   = c("contractdate", "kdate", "saledate"),
    listing_date    = c("listingdate", "listdate"),
    dom             = c("daysonmarket", "daysmarket"),
    concessions     = c("concession", "conc", "sellerconcessions",
                        "saleconcessions"),
    weight          = c("wt", "wgt")
  )
  generic <- c("area", "age")  # match whole-name only, never a sub-token

  keys_for <- function(tag) {
    k <- unique(c(norm(tag), norm(syn[[tag]])))
    k[nzchar(k)]
  }
  nn   <- norm(name)
  toks <- norm(tok(name))

  # 1) whole-name equals a tag or synonym
  for (tag in cand) if (nn %in% keys_for(tag)) return(tag)
  # 2) a name token equals a specific (non-generic, >= 3 char) key
  for (tag in cand) {
    k <- setdiff(keys_for(tag), generic)
    k <- k[nchar(k) >= 3L]
    if (length(k) && any(toks %in% k)) return(tag)
  }
  "no"
}
