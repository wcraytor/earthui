# Internal locale environment for number/date formatting
eui_locale_env_ <- new.env(parent = emptyenv())
eui_locale_env_$csv_sep   <- ","
eui_locale_env_$csv_dec   <- "."
eui_locale_env_$big_mark  <- ","
eui_locale_env_$dec_mark  <- "."
eui_locale_env_$date_fmt  <- "mdy"
eui_locale_env_$paper     <- "letter"

# Country presets: csv_sep, csv_dec, big_mark, dec_mark, date_fmt, paper
#' @noRd
locale_country_presets_ <- function() {
  list(
    us = list(csv_sep = ",", csv_dec = ".", big_mark = ",",  dec_mark = ".", date_fmt = "mdy", paper = "letter"),
    ca = list(csv_sep = ",", csv_dec = ".", big_mark = ",",  dec_mark = ".", date_fmt = "ymd", paper = "letter"),
    gb = list(csv_sep = ",", csv_dec = ".", big_mark = ",",  dec_mark = ".", date_fmt = "dmy", paper = "a4"),
    ie = list(csv_sep = ",", csv_dec = ".", big_mark = ",",  dec_mark = ".", date_fmt = "dmy", paper = "a4"),
    au = list(csv_sep = ",", csv_dec = ".", big_mark = ",",  dec_mark = ".", date_fmt = "dmy", paper = "a4"),
    nz = list(csv_sep = ",", csv_dec = ".", big_mark = ",",  dec_mark = ".", date_fmt = "dmy", paper = "a4"),
    de = list(csv_sep = ";", csv_dec = ",", big_mark = ".",  dec_mark = ",", date_fmt = "dmy", paper = "a4"),
    at = list(csv_sep = ";", csv_dec = ",", big_mark = ".",  dec_mark = ",", date_fmt = "dmy", paper = "a4"),
    ch = list(csv_sep = ";", csv_dec = ",", big_mark = "'",  dec_mark = ",", date_fmt = "dmy", paper = "a4"),
    fr = list(csv_sep = ";", csv_dec = ",", big_mark = " ",  dec_mark = ",", date_fmt = "dmy", paper = "a4"),
    be = list(csv_sep = ";", csv_dec = ",", big_mark = ".",  dec_mark = ",", date_fmt = "dmy", paper = "a4"),
    nl = list(csv_sep = ";", csv_dec = ",", big_mark = ".",  dec_mark = ",", date_fmt = "dmy", paper = "a4"),
    it = list(csv_sep = ";", csv_dec = ",", big_mark = ".",  dec_mark = ",", date_fmt = "dmy", paper = "a4"),
    es = list(csv_sep = ";", csv_dec = ",", big_mark = ".",  dec_mark = ",", date_fmt = "dmy", paper = "a4"),
    pt = list(csv_sep = ";", csv_dec = ",", big_mark = ".",  dec_mark = ",", date_fmt = "dmy", paper = "a4"),
    tr = list(csv_sep = ";", csv_dec = ",", big_mark = ".",  dec_mark = ",", date_fmt = "dmy", paper = "a4"),
    fi = list(csv_sep = ";", csv_dec = ",", big_mark = " ",  dec_mark = ",", date_fmt = "dmy", paper = "a4"),
    se = list(csv_sep = ";", csv_dec = ",", big_mark = " ",  dec_mark = ",", date_fmt = "ymd", paper = "a4"),
    no = list(csv_sep = ";", csv_dec = ",", big_mark = " ",  dec_mark = ",", date_fmt = "dmy", paper = "a4"),
    dk = list(csv_sep = ";", csv_dec = ",", big_mark = ".",  dec_mark = ",", date_fmt = "dmy", paper = "a4"),
    pl = list(csv_sep = ";", csv_dec = ",", big_mark = " ",  dec_mark = ",", date_fmt = "dmy", paper = "a4"),
    cz = list(csv_sep = ";", csv_dec = ",", big_mark = " ",  dec_mark = ",", date_fmt = "dmy", paper = "a4"),
    lt = list(csv_sep = ";", csv_dec = ",", big_mark = " ",  dec_mark = ",", date_fmt = "ymd", paper = "a4"),
    lv = list(csv_sep = ";", csv_dec = ",", big_mark = " ",  dec_mark = ",", date_fmt = "dmy", paper = "a4"),
    ee = list(csv_sep = ";", csv_dec = ",", big_mark = " ",  dec_mark = ",", date_fmt = "dmy", paper = "a4"),
    ua = list(csv_sep = ";", csv_dec = ",", big_mark = " ",  dec_mark = ",", date_fmt = "dmy", paper = "a4"),
    ru = list(csv_sep = ";", csv_dec = ",", big_mark = " ",  dec_mark = ",", date_fmt = "dmy", paper = "a4"),
    jp = list(csv_sep = ",", csv_dec = ".", big_mark = ",",  dec_mark = ".", date_fmt = "ymd", paper = "a4"),
    kr = list(csv_sep = ",", csv_dec = ".", big_mark = ",",  dec_mark = ".", date_fmt = "ymd", paper = "a4"),
    mx = list(csv_sep = ",", csv_dec = ".", big_mark = ",",  dec_mark = ".", date_fmt = "dmy", paper = "letter"),
    br = list(csv_sep = ";", csv_dec = ",", big_mark = ".",  dec_mark = ",", date_fmt = "dmy", paper = "a4")
  )
}

# Country display names (for UI dropdown)
#' @noRd
locale_country_choices_ <- function() {
  c("United States"  = "us", "Canada"        = "ca",
    "United Kingdom"  = "gb", "Ireland"       = "ie",
    "Australia"       = "au", "New Zealand"   = "nz",
    "Germany"         = "de", "Austria"       = "at",
    "Switzerland"     = "ch", "France"        = "fr",
    "Belgium"         = "be", "Netherlands"   = "nl",
    "Italy"           = "it", "Spain"         = "es",
    "Portugal"        = "pt", "Turkey"        = "tr",
    "Finland"         = "fi", "Sweden"        = "se",
    "Norway"          = "no", "Denmark"       = "dk",
    "Poland"          = "pl", "Czech Republic" = "cz",
    "Lithuania"       = "lt", "Latvia"        = "lv",
    "Estonia"         = "ee", "Ukraine"       = "ua",
    "Russia"          = "ru", "Japan"         = "jp",
    "South Korea"     = "kr", "Mexico"        = "mx",
    "Brazil"          = "br")
}

#' @noRd
set_locale_ <- function(country = "us", csv_sep = NULL, csv_dec = NULL,
                        big_mark = NULL, dec_mark = NULL,
                        date_fmt = NULL, paper = NULL) {
  presets <- locale_country_presets_()
  preset <- presets[[country]] %||% presets[["us"]]

  # Apply country defaults, then overrides
  eui_locale_env_$csv_sep  <- csv_sep  %||% preset$csv_sep
  eui_locale_env_$csv_dec  <- csv_dec  %||% preset$csv_dec
  eui_locale_env_$big_mark <- big_mark %||% preset$big_mark
  eui_locale_env_$dec_mark <- dec_mark %||% preset$dec_mark
  eui_locale_env_$date_fmt <- date_fmt %||% preset$date_fmt
  eui_locale_env_$paper    <- paper    %||% preset$paper
}

#' @noRd
get_locale_ <- function() {
  list(
    csv_sep  = eui_locale_env_$csv_sep,
    csv_dec  = eui_locale_env_$csv_dec,
    big_mark = eui_locale_env_$big_mark,
    dec_mark = eui_locale_env_$dec_mark,
    date_fmt = eui_locale_env_$date_fmt,
    paper    = eui_locale_env_$paper
  )
}

# Convenience accessors
#' @noRd
locale_csv_sep_ <- function() eui_locale_env_$csv_sep

#' @noRd
locale_csv_dec_ <- function() eui_locale_env_$csv_dec

#' @noRd
locale_big_mark_ <- function() eui_locale_env_$big_mark

#' @noRd
locale_dec_mark_ <- function() eui_locale_env_$dec_mark

#' @noRd
locale_paper_ <- function() eui_locale_env_$paper

# Date format (for parsing ambiguous dates — try locale-preferred first)
#' @noRd
locale_date_formats_ <- function() {
  fmt <- eui_locale_env_$date_fmt %||% "mdy"
  # Locale-preferred formats first, then universal fallbacks
  preferred <- switch(fmt,
    mdy = c("%m/%d/%Y", "%m-%d-%Y", "%m/%d/%Y %H:%M:%S"),
    dmy = c("%d/%m/%Y", "%d-%m-%Y", "%d.%m.%Y", "%d/%m/%Y %H:%M:%S"),
    ymd = c("%Y-%m-%d", "%Y/%m/%d", "%Y-%m-%d %H:%M:%S"),
    c("%m/%d/%Y", "%d/%m/%Y")
  )
  # Always include ISO and other fallbacks
  all_fmts <- c("%Y-%m-%d", "%Y/%m/%d",
                "%m/%d/%Y", "%m-%d-%Y",
                "%d/%m/%Y", "%d-%m-%Y", "%d.%m.%Y",
                "%Y-%m-%d %H:%M:%S", "%m/%d/%Y %H:%M:%S",
                "%d/%m/%Y %H:%M:%S", "%Y-%m-%dT%H:%M:%S",
                "%b %d, %Y", "%B %d, %Y")
  unique(c(preferred, all_fmts))
}
