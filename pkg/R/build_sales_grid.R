# Internal helpers --------------------------------------------------------

col_val_ <- function(df, row, col, default = "") {
  if (col %in% colnames(df)) {
    v <- df[[col]][row]
    if (is.null(v) || length(v) == 0 || is.na(v)) return(default)
    return(v)
  }
  default
}

col_num_ <- function(df, row, col, digits = 0, default = 0) {
  v <- col_val_(df, row, col, default = NA)
  if (is.na(v)) return(default)
  round(as.numeric(v), digits = digits)
}

haversine_miles_ <- function(lat1, lon1, lat2, lon2) {
  if (any(is.na(c(lat1, lon1, lat2, lon2)))) return(NA_real_)
  R <- 3958.8
  dlat <- (lat2 - lat1) * pi / 180
  dlon <- (lon2 - lon1) * pi / 180
  a <- sin(dlat / 2)^2 + cos(lat1 * pi / 180) * cos(lat2 * pi / 180) * sin(dlon / 2)^2
  R * 2 * asin(sqrt(a))
}

compute_dom_ <- function(df, row) {
  cd <- col_val_(df, row, "contract_date", default = NA)
  ld <- col_val_(df, row, "listing_date", default = NA)
  if (is.na(cd) || is.na(ld)) return(NA_integer_)
  cd <- tryCatch(as.Date(cd), error = function(e) NA)
  ld <- tryCatch(as.Date(ld), error = function(e) NA)
  if (is.na(cd) || is.na(ld)) return(NA_integer_)
  as.integer(cd - ld)
}

detect_model_vars_ <- function(df) {
  contrib_cols <- grep("_contribution$", colnames(df), value = TRUE)
  var_labels <- sub("_contribution$", "", contrib_cols)
  var_labels <- var_labels[!grepl("^rent_", var_labels)]
  contrib_cols <- paste0(var_labels, "_contribution")
  adjust_cols  <- paste0(var_labels, "_adjustment")
  keep <- adjust_cols %in% colnames(df)
  list(
    labels     = var_labels[keep],
    contrib    = contrib_cols[keep],
    adjustment = adjust_cols[keep]
  )
}

format_label_ <- function(lbl) {
  abbrevs <- c(
    "living_sqft"       = "Living SF",
    "lot_size"          = "Lot Size",
    "sale_age"          = "Sale Age",
    "beds_total"        = "Beds",
    "baths_total"       = "Baths",
    "garage_spaces"     = "Garage",
    "fp_count"          = "Fireplaces",
    "no_of_stories"     = "Stories",
    "year_built"        = "Year Built",
    "days_on_market"    = "DOM",
    "contract_date"     = "Contract Date",
    "area_id"           = "Area",
    "area_text"         = "Area",
    "latitude"          = "Latitude",
    "longitude"         = "Longitude",
    "age"               = "Age"
  )
  if (lbl %in% names(abbrevs)) return(abbrevs[[lbl]])
  result <- lbl
  for (nm in names(abbrevs)) {
    if (grepl(nm, result, fixed = TRUE)) {
      result <- sub(nm, abbrevs[[nm]], result, fixed = TRUE)
    }
  }
  result <- gsub("_", " ", result)
  result <- trimws(result)
  if (nchar(result) > 28) result <- paste0(substr(result, 1, 25), "...")
  result
}

col_letter_ <- function(n) {
  if (n <= 26) {
    return(LETTERS[n])
  } else {
    return(paste0(LETTERS[(n - 1) %/% 26], LETTERS[((n - 1) %% 26) + 1]))
  }
}

sp_col_ <- function(specials, type) {
  if (!is.null(specials[[type]])) specials[[type]] else NULL
}

sum_contribs_ <- function(df, row, var_names) {
  total <- 0
  for (vn in var_names) {
    cc <- paste0(vn, "_contribution")
    if (cc %in% colnames(df)) {
      total <- total + col_num_(df, row, cc)
    }
  }
  total
}


# Exported function -------------------------------------------------------

#' Build a Sales Comparison Grid Excel workbook
#'
#' Generates a multi-sheet xlsx workbook formatted as a Sales Comparison
#' Grid from an RCA-adjusted data frame. Each sheet shows the subject and
#' up to three comparable sales side by side, with factual values, value
#' contributions, and adjustments per regression variable, plus rows for
#' grouped variables (location, site, age), residual feature inputs, and
#' an Adjusted Sale Price formula.
#'
#' This is the non-Shiny computation kernel used by the earthUI Shiny app's
#' Sales Grid download button, and is also suitable for use from batch
#' scripts that already have `rca_df` in memory.
#'
#' @param rca_df A data frame produced by the RCA workflow. Row 1 is the
#'   subject; rows 2+ are comps. Must contain columns produced by
#'   [compute_rca_adjustments()] (`basis`, `residual`, `cqa`, `<var>_contribution`,
#'   `<var>_adjustment`, `net_adjustments`, `gross_adjustments`,
#'   `subject_cqa`).
#' @param comp_rows Integer vector of row numbers (>= 2) to include in the
#'   grid. Maximum 30 (10 sheets, 3 comps per sheet).
#' @param output_file Character scalar. Destination xlsx path.
#' @param specials Named list mapping a special type
#'   (e.g. `"contract_date"`, `"dom"`, `"latitude"`, `"longitude"`,
#'   `"area"`, `"concessions"`, `"lot_size"`, `"site_dimensions"`,
#'   `"actual_age"`, `"effective_age"`, `"living_area"`, `"sale_age"`)
#'   to the corresponding column name in `rca_df`. Default `list()`.
#' @param title_prefix Character scalar. Sheet-title prefix. Defaults to
#'   `"Intermediate Sales Comparable Grid"`.
#' @param progress_fn Optional function called after each sheet is written
#'   with arguments `sheet`, `total_sheets`, `comps_done`, `total_comps`.
#'   Used by the Shiny app to drive `withProgress()`. Default `NULL`.
#'
#' @return Invisibly, the `output_file` path.
#'
#' @export
build_sales_grid <- function(rca_df,
                             comp_rows,
                             output_file,
                             specials     = list(),
                             title_prefix = "Intermediate Sales Comparable Grid",
                             progress_fn  = NULL) {

  if (!requireNamespace("openxlsx", quietly = TRUE)) {
    stop("Package 'openxlsx' is required for build_sales_grid().",
         call. = FALSE)
  }
  if (!is.data.frame(rca_df)) {
    stop("`rca_df` must be a data frame.", call. = FALSE)
  }
  if (missing(output_file) || is.null(output_file) || !nzchar(output_file)) {
    stop("`output_file` is required.", call. = FALSE)
  }

  df <- rca_df
  n_total <- nrow(df)

  comp_rows <- as.integer(comp_rows)
  if (any(comp_rows < 2 | comp_rows > n_total)) {
    stop("comp_rows must be between 2 and ", n_total, call. = FALSE)
  }
  if (length(comp_rows) > 30) {
    stop("Maximum 30 comps supported (10 sheets)", call. = FALSE)
  }

  mv <- detect_model_vars_(df)

  dom_col     <- sp_col_(specials, "dom")
  cd_col      <- sp_col_(specials, "contract_date")
  sa_col      <- sp_col_(specials, "sale_age")
  if (is.null(sa_col) && "sale_age" %in% colnames(df)) sa_col <- "sale_age"
  lat_col     <- sp_col_(specials, "latitude")
  lon_col     <- sp_col_(specials, "longitude")
  area_col    <- sp_col_(specials, "area")
  conc_col    <- sp_col_(specials, "concessions")
  lot_col     <- sp_col_(specials, "lot_size")
  sitedim_col <- sp_col_(specials, "site_dimensions")
  actage_col  <- sp_col_(specials, "actual_age")
  effage_col  <- sp_col_(specials, "effective_age")
  la_col      <- sp_col_(specials, "living_area")

  loc_vars <- c(lon_col, lat_col, area_col)
  loc_vars <- loc_vars[!is.null(loc_vars)]
  loc_model_vars <- intersect(loc_vars, mv$labels)
  has_loc_row <- length(loc_model_vars) > 0

  site_vars <- c(lot_col, sitedim_col)
  site_vars <- site_vars[!is.null(site_vars)]
  site_model_vars <- intersect(site_vars, mv$labels)
  has_site_row <- length(site_model_vars) > 0

  age_vars <- c(actage_col, effage_col)
  age_vars <- age_vars[!is.null(age_vars)]
  age_model_vars <- intersect(age_vars, mv$labels)
  has_age_row <- length(age_model_vars) > 0

  grouped_vars <- c(loc_model_vars, site_model_vars, age_model_vars)

  mv_filtered_idx <- which(!mv$labels %in% grouped_vars)
  n_vars <- length(mv_filtered_idx)

  resid_named   <- c("View", "Design", "Quality of Construction",
                     "Condition", "Functional Utility")
  n_resid_named <- length(resid_named)
  n_resid_blank <- 6
  n_resid_rows  <- n_resid_named + n_resid_blank  # 11 total

  row_title       <- 1
  row_headers     <- 2
  row_address     <- 3
  row_apn         <- 4
  row_sale_price  <- 5
  row_regr_hdr    <- 6
  row_base_value  <- 7
  row_date_info   <- 8

  next_row <- 9
  row_loc <- if (has_loc_row) { r <- next_row; next_row <- next_row + 1; r } else NULL
  row_site <- if (has_site_row) { r <- next_row; next_row <- next_row + 1; r } else NULL
  row_age <- if (has_age_row) { r <- next_row; next_row <- next_row + 1; r } else NULL

  row_vars_start  <- next_row
  row_vars_end    <- row_vars_start + n_vars - 1
  row_blank1      <- row_vars_end + 1
  row_resid_hdr   <- row_blank1 + 1
  row_cqa         <- row_resid_hdr + 1
  row_resid_start <- row_cqa + 1
  row_resid_end   <- row_resid_start + n_resid_rows - 1
  row_net_adj     <- row_resid_end + 1
  row_net_pct     <- row_net_adj + 1
  row_gross_pct   <- row_net_pct + 1
  row_adj_sp      <- row_gross_pct + 1
  row_copyright   <- row_adj_sp + 1

  grouped_adj_rows <- c(row_loc, row_site, row_age)
  grouped_adj_rows <- grouped_adj_rows[!is.null(grouped_adj_rows)]

  title_style <- openxlsx::createStyle(
    fontSize = 11, textDecoration = "bold",
    fontColour = "#FFFFFF", fgFill = "#002060",
    halign = "center", valign = "center",
    border = "TopBottomLeftRight", borderColour = "#002060",
    borderStyle = "thick"
  )
  section_hdr_style <- openxlsx::createStyle(
    textDecoration = "bold",
    fgFill = "#CCCCFF", halign = "center", valign = "center",
    border = "TopBottomLeftRight", borderColour = "#002060",
    borderStyle = "thin"
  )
  label_style <- openxlsx::createStyle(
    textDecoration = "bold",
    fgFill = "#CCCCFF", halign = "left", valign = "center",
    border = "TopBottomLeftRight", borderColour = "#002060",
    borderStyle = "thin"
  )
  green_hdr_style <- openxlsx::createStyle(
    textDecoration = "bold",
    fgFill = "#E2EFDA", halign = "center", valign = "center",
    border = "TopBottomLeftRight", borderColour = "#002060",
    borderStyle = "thin"
  )
  body_style <- openxlsx::createStyle(
    halign = "center", valign = "center",
    border = "TopBottomLeftRight", borderColour = "#002060",
    borderStyle = "thin", fgFill = "#FFFFFF"
  )
  curr_style <- openxlsx::createStyle(
    numFmt = "#,##0", halign = "right", valign = "center",
    border = "TopBottomLeftRight", borderColour = "#002060",
    borderStyle = "thin", fgFill = "#FFFFFF"
  )
  pct_style <- openxlsx::createStyle(
    numFmt = "0.0%", halign = "center", valign = "center",
    border = "TopBottomLeftRight", borderColour = "#002060",
    borderStyle = "thin", fgFill = "#FFFFFF"
  )
  cqa_style <- openxlsx::createStyle(
    numFmt = "0.00", halign = "center", valign = "center",
    border = "TopBottomLeftRight", borderColour = "#002060",
    borderStyle = "thin", fgFill = "#FFFFFF"
  )
  copyright_style <- openxlsx::createStyle(
    halign = "center", valign = "center",
    fgFill = "#CCCCFF",
    border = "TopBottomLeftRight", borderColour = "#002060",
    borderStyle = "thick"
  )
  adj_sp_style <- openxlsx::createStyle(
    numFmt = "#,##0", halign = "center", valign = "center",
    textDecoration = "bold",
    border = "TopBottomLeftRight", borderColour = "#002060",
    borderStyle = "thin", fgFill = "#CCCCFF"
  )
  remaining_style <- openxlsx::createStyle(
    numFmt = "#,##0", halign = "right", valign = "center",
    textDecoration = "bold", fontColour = "#C00000",
    border = "TopBottomLeftRight", borderColour = "#002060",
    borderStyle = "thin", fgFill = "#FFF2CC"
  )
  resid_input_style <- openxlsx::createStyle(
    numFmt = "#,##0", halign = "right", valign = "center",
    border = "TopBottomLeftRight", borderColour = "#002060",
    borderStyle = "thin", fgFill = "#FFFFDD"
  )
  grouped_style <- openxlsx::createStyle(
    numFmt = "#,##0", halign = "right", valign = "center",
    border = "TopBottomLeftRight", borderColour = "#002060",
    borderStyle = "thin", fgFill = "#DAEEF3"
  )

  wb <- openxlsx::createWorkbook()
  openxlsx::modifyBaseFont(wb, fontSize = 9, fontName = "Arial Narrow")

  n_comps <- length(comp_rows)
  n_sheets <- ceiling(n_comps / 3)

  col_widths <- c(29, 11, 8, 4, 12, 11, 8, 7, 12, 11,
                  11, 8, 7, 12, 11, 11, 8, 7, 12, 11)

  for (s in seq_len(n_sheets)) {
    idx_start <- (s - 1) * 3 + 1
    idx_end   <- min(s * 3, n_comps)
    sheet_comps <- comp_rows[idx_start:idx_end]
    n_on_sheet  <- length(sheet_comps)

    comp_first <- idx_start
    sheet_name <- paste0("Comps ", comp_first, "-", comp_first + 2)

    openxlsx::addWorksheet(wb, sheet_name)
    openxlsx::setColWidths(wb, s, cols = 1:20, widths = col_widths)

    # === Row 1: Title ===
    openxlsx::mergeCells(wb, s, cols = 1:20, rows = row_title)
    openxlsx::writeData(wb, s, paste0(title_prefix, ": Comps ", comp_first, "-",
                                      comp_first + 2),
                        startRow = row_title, startCol = 1)
    openxlsx::addStyle(wb, s, title_style, rows = row_title, cols = 1:20,
                       gridExpand = TRUE, stack = TRUE)

    # === Row 2: Group headers ===
    openxlsx::mergeCells(wb, s, cols = 2:5,   rows = row_headers)
    openxlsx::mergeCells(wb, s, cols = 6:10,  rows = row_headers)
    openxlsx::mergeCells(wb, s, cols = 11:15, rows = row_headers)
    openxlsx::mergeCells(wb, s, cols = 16:20, rows = row_headers)
    openxlsx::writeData(wb, s, "Subject",
                        startRow = row_headers, startCol = 2)
    for (ci in seq_len(n_on_sheet)) {
      comp_num <- idx_start + ci - 1
      col_start <- 1 + ci * 5
      openxlsx::writeData(wb, s, paste("Comparable Sale No.", comp_num),
                          startRow = row_headers, startCol = col_start)
    }
    openxlsx::addStyle(wb, s, section_hdr_style, rows = row_headers, cols = 1:20,
                       gridExpand = TRUE, stack = TRUE)

    # === Row 3: Address ===
    openxlsx::writeData(wb, s, "Street, City, State Zip",
                        startRow = row_address, startCol = 1)
    subj_addr <- paste0(col_val_(df, 1, "street_address"), ", ",
                        col_val_(df, 1, "city_name"), " ",
                        col_val_(df, 1, "postal_code"))
    openxlsx::mergeCells(wb, s, cols = 2:5, rows = row_address)
    openxlsx::writeData(wb, s, subj_addr, startRow = row_address, startCol = 2)
    for (ci in seq_len(n_on_sheet)) {
      r <- sheet_comps[ci]
      col_start <- 1 + ci * 5
      comp_addr <- paste0(col_val_(df, r, "street_address"), ", ",
                          col_val_(df, r, "city_name"), " ",
                          col_val_(df, r, "postal_code"))
      openxlsx::mergeCells(wb, s, cols = col_start:(col_start + 4), rows = row_address)
      openxlsx::writeData(wb, s, comp_addr, startRow = row_address, startCol = col_start)
    }
    openxlsx::addStyle(wb, s, green_hdr_style, rows = row_address, cols = 1:20,
                       gridExpand = TRUE, stack = TRUE)
    openxlsx::addStyle(wb, s, label_style, rows = row_address, cols = 1, stack = TRUE)

    # === Row 4: APN | MLS# | DOM | Subj.Prox ===
    openxlsx::writeData(wb, s, "APN | MLS# | DOM | Subj.Prox",
                        startRow = row_apn, startCol = 1)
    openxlsx::mergeCells(wb, s, cols = 2:3, rows = row_apn)
    openxlsx::writeData(wb, s, col_val_(df, 1, "parcel_number"),
                        startRow = row_apn, startCol = 2)
    subj_dom <- if (!is.null(dom_col) && dom_col %in% colnames(df)) {
      v <- col_val_(df, 1, dom_col, default = NA)
      if (!is.na(v)) as.integer(v) else NA_integer_
    } else {
      compute_dom_(df, 1)
    }
    if (!is.na(subj_dom)) {
      openxlsx::writeData(wb, s, subj_dom, startRow = row_apn, startCol = 4)
    }
    openxlsx::writeData(wb, s, "0.00 mi", startRow = row_apn, startCol = 5)
    subj_lat <- if (!is.null(lat_col) && lat_col %in% colnames(df)) as.numeric(col_val_(df, 1, lat_col, NA)) else NA
    subj_lon <- if (!is.null(lon_col) && lon_col %in% colnames(df)) as.numeric(col_val_(df, 1, lon_col, NA)) else NA
    for (ci in seq_len(n_on_sheet)) {
      r <- sheet_comps[ci]
      col_start <- 1 + ci * 5
      openxlsx::writeData(wb, s, col_val_(df, r, "parcel_number"),
                          startRow = row_apn, startCol = col_start)
      openxlsx::writeData(wb, s, col_val_(df, r, "listing_id"),
                          startRow = row_apn, startCol = col_start + 1)
      comp_dom <- if (!is.null(dom_col) && dom_col %in% colnames(df)) {
        v <- col_val_(df, r, dom_col, default = NA)
        if (!is.na(v)) as.integer(v) else NA_integer_
      } else {
        compute_dom_(df, r)
      }
      if (!is.na(comp_dom)) {
        openxlsx::writeData(wb, s, comp_dom, startRow = row_apn, startCol = col_start + 2)
      }
      comp_lat <- if (!is.null(lat_col) && lat_col %in% colnames(df)) as.numeric(col_val_(df, r, lat_col, NA)) else NA
      comp_lon <- if (!is.null(lon_col) && lon_col %in% colnames(df)) as.numeric(col_val_(df, r, lon_col, NA)) else NA
      prox <- haversine_miles_(subj_lat, subj_lon, comp_lat, comp_lon)
      openxlsx::mergeCells(wb, s, cols = (col_start + 3):(col_start + 4), rows = row_apn)
      if (!is.na(prox)) {
        openxlsx::writeData(wb, s, sprintf("%.2f mi", prox),
                            startRow = row_apn, startCol = col_start + 3)
      }
    }
    openxlsx::addStyle(wb, s, body_style, rows = row_apn, cols = 1:20,
                       gridExpand = TRUE, stack = TRUE)
    openxlsx::addStyle(wb, s, label_style, rows = row_apn, cols = 1, stack = TRUE)

    # === Row 5: Sales Price | Concess. | Net SP ===
    openxlsx::writeData(wb, s, "Sales Price | Concess. | Net SP",
                        startRow = row_sale_price, startCol = 1)
    openxlsx::writeData(wb, s, "N/A", startRow = row_sale_price, startCol = 2)
    if (!is.null(conc_col) && conc_col %in% colnames(df)) {
      subj_conc <- col_num_(df, 1, conc_col)
      openxlsx::writeData(wb, s, subj_conc, startRow = row_sale_price, startCol = 3)
      openxlsx::addStyle(wb, s, curr_style, rows = row_sale_price, cols = 3, stack = TRUE)
    }
    openxlsx::mergeCells(wb, s, cols = 4:5, rows = row_sale_price)
    openxlsx::writeData(wb, s, "N/A", startRow = row_sale_price, startCol = 4)

    for (ci in seq_len(n_on_sheet)) {
      r <- sheet_comps[ci]
      col_start <- 1 + ci * 5
      sp <- col_num_(df, r, "sale_price")
      openxlsx::writeData(wb, s, sp, startRow = row_sale_price, startCol = col_start)
      openxlsx::addStyle(wb, s, curr_style, rows = row_sale_price,
                         cols = col_start, stack = TRUE)
      comp_conc <- 0
      if (!is.null(conc_col) && conc_col %in% colnames(df)) {
        comp_conc <- col_num_(df, r, conc_col)
      }
      openxlsx::writeData(wb, s, comp_conc, startRow = row_sale_price,
                          startCol = col_start + 1)
      openxlsx::addStyle(wb, s, curr_style, rows = row_sale_price,
                         cols = col_start + 1, stack = TRUE)
      sp_l   <- col_letter_(col_start)
      conc_l <- col_letter_(col_start + 1)
      net_sp_formula <- paste0(sp_l, row_sale_price, "-", conc_l, row_sale_price)
      openxlsx::mergeCells(wb, s, cols = (col_start + 2):(col_start + 4), rows = row_sale_price)
      openxlsx::writeFormula(wb, s, x = net_sp_formula,
                             startRow = row_sale_price, startCol = col_start + 2)
      openxlsx::addStyle(wb, s, curr_style, rows = row_sale_price,
                         cols = col_start + 2, stack = TRUE)
    }
    openxlsx::addStyle(wb, s, body_style, rows = row_sale_price, cols = 1:20,
                       gridExpand = TRUE, stack = TRUE)
    openxlsx::addStyle(wb, s, label_style, rows = row_sale_price, cols = 1,
                       stack = TRUE)

    # === Regression Features header ===
    openxlsx::writeData(wb, s, "Regression Features",
                        startRow = row_regr_hdr, startCol = 1)
    openxlsx::mergeCells(wb, s, cols = 2:3, rows = row_regr_hdr)
    openxlsx::writeData(wb, s, "Factual Value",
                        startRow = row_regr_hdr, startCol = 2)
    openxlsx::writeData(wb, s, "Value Contrib.",
                        startRow = row_regr_hdr, startCol = 5)
    for (ci in seq_len(n_on_sheet)) {
      col_start <- 1 + ci * 5
      openxlsx::mergeCells(wb, s, cols = col_start:(col_start + 2), rows = row_regr_hdr)
      openxlsx::writeData(wb, s, "Factual Value",
                          startRow = row_regr_hdr, startCol = col_start)
      openxlsx::writeData(wb, s, "Value Contrib.",
                          startRow = row_regr_hdr, startCol = col_start + 3)
      openxlsx::writeData(wb, s, "Adjustment",
                          startRow = row_regr_hdr, startCol = col_start + 4)
    }
    openxlsx::addStyle(wb, s, green_hdr_style, rows = row_regr_hdr, cols = 1:20,
                       gridExpand = TRUE, stack = TRUE)
    openxlsx::addStyle(wb, s, label_style, rows = row_regr_hdr, cols = 1,
                       stack = TRUE)

    # === Base Value (intercept) ===
    openxlsx::writeData(wb, s, "BASE VALUE",
                        startRow = row_base_value, startCol = 1)
    subj_basis <- col_num_(df, 1, "basis")
    openxlsx::writeData(wb, s, subj_basis,
                        startRow = row_base_value, startCol = 5)
    openxlsx::addStyle(wb, s, curr_style, rows = row_base_value, cols = 5,
                       stack = TRUE)
    for (ci in seq_len(n_on_sheet)) {
      r <- sheet_comps[ci]
      col_start <- 1 + ci * 5
      comp_basis <- col_num_(df, r, "basis")
      openxlsx::writeData(wb, s, comp_basis,
                          startRow = row_base_value, startCol = col_start + 3)
      openxlsx::addStyle(wb, s, curr_style, rows = row_base_value,
                         cols = col_start + 3, stack = TRUE)
    }
    openxlsx::addStyle(wb, s, body_style, rows = row_base_value, cols = 1:20,
                       gridExpand = TRUE, stack = TRUE)
    openxlsx::addStyle(wb, s, label_style, rows = row_base_value, cols = 1,
                       stack = TRUE)

    # === Date of Sale | OffMkt | OnMkt row ===
    openxlsx::writeData(wb, s, "Date of Sale | OffMkt | OnMkt",
                        startRow = row_date_info, startCol = 1)
    if (!is.null(cd_col) && cd_col %in% colnames(df)) {
      subj_cd <- col_val_(df, 1, cd_col, default = "")
      openxlsx::writeData(wb, s, subj_cd, startRow = row_date_info, startCol = 2)
    }
    if (!is.null(sa_col) && sa_col %in% colnames(df)) {
      openxlsx::writeData(wb, s, col_num_(df, 1, sa_col),
                          startRow = row_date_info, startCol = 3)
    }
    if (!is.null(dom_col) && dom_col %in% colnames(df)) {
      openxlsx::writeData(wb, s, col_num_(df, 1, dom_col),
                          startRow = row_date_info, startCol = 4)
    }
    for (ci in seq_len(n_on_sheet)) {
      r <- sheet_comps[ci]
      col_start <- 1 + ci * 5
      if (!is.null(cd_col) && cd_col %in% colnames(df)) {
        openxlsx::writeData(wb, s, col_val_(df, r, cd_col, default = ""),
                            startRow = row_date_info, startCol = col_start)
      }
      if (!is.null(sa_col) && sa_col %in% colnames(df)) {
        openxlsx::writeData(wb, s, col_num_(df, r, sa_col),
                            startRow = row_date_info, startCol = col_start + 1)
      }
      if (!is.null(dom_col) && dom_col %in% colnames(df)) {
        openxlsx::writeData(wb, s, col_num_(df, r, dom_col),
                            startRow = row_date_info, startCol = col_start + 2)
      }
    }
    openxlsx::addStyle(wb, s, body_style, rows = row_date_info, cols = 1:20,
                       gridExpand = TRUE, stack = TRUE)
    openxlsx::addStyle(wb, s, label_style, rows = row_date_info, cols = 1,
                       stack = TRUE)

    # === GROUPED ROWS (Location, Site, Age) ===
    write_grouped_row <- function(rw, label, var_cols, model_vars_in_group) {
      openxlsx::writeData(wb, s, label, startRow = rw, startCol = 1)

      for (fi in seq_along(var_cols)) {
        vc <- var_cols[fi]
        if (!is.null(vc) && vc %in% colnames(df)) {
          openxlsx::writeData(wb, s, col_val_(df, 1, vc),
                              startRow = rw, startCol = 1 + fi)
        }
      }

      subj_combined_vc <- sum_contribs_(df, 1, model_vars_in_group)
      openxlsx::writeData(wb, s, round(subj_combined_vc),
                          startRow = rw, startCol = 5)
      openxlsx::addStyle(wb, s, grouped_style, rows = rw, cols = 5, stack = TRUE)

      for (ci in seq_len(n_on_sheet)) {
        r <- sheet_comps[ci]
        col_start <- 1 + ci * 5
        for (fi in seq_along(var_cols)) {
          vc <- var_cols[fi]
          if (!is.null(vc) && vc %in% colnames(df)) {
            openxlsx::writeData(wb, s, col_val_(df, r, vc),
                                startRow = rw, startCol = col_start + fi - 1)
          }
        }
        comp_combined_vc <- sum_contribs_(df, r, model_vars_in_group)
        openxlsx::writeData(wb, s, round(comp_combined_vc),
                            startRow = rw, startCol = col_start + 3)
        openxlsx::addStyle(wb, s, grouped_style, rows = rw,
                           cols = col_start + 3, stack = TRUE)
        adj <- round(subj_combined_vc - comp_combined_vc)
        openxlsx::writeData(wb, s, adj,
                            startRow = rw, startCol = col_start + 4)
        openxlsx::addStyle(wb, s, curr_style, rows = rw,
                           cols = col_start + 4, stack = TRUE)
      }
      openxlsx::addStyle(wb, s, body_style, rows = rw, cols = 1:20,
                         gridExpand = TRUE, stack = TRUE)
      openxlsx::addStyle(wb, s, label_style, rows = rw, cols = 1, stack = TRUE)
      openxlsx::addStyle(wb, s, grouped_style, rows = rw, cols = 5, stack = TRUE)
      for (ci in seq_len(n_on_sheet)) {
        col_start <- 1 + ci * 5
        openxlsx::addStyle(wb, s, grouped_style, rows = rw,
                           cols = col_start + 3, stack = TRUE)
        openxlsx::addStyle(wb, s, curr_style, rows = rw,
                           cols = col_start + 4, stack = TRUE)
      }
    }

    if (has_loc_row) {
      write_grouped_row(row_loc, "Loc: Long | Lat | Area",
                        c(lon_col, lat_col, area_col), loc_model_vars)
    }
    if (has_site_row) {
      write_grouped_row(row_site, "Site Size | Dimensions",
                        c(lot_col, sitedim_col), site_model_vars)
    }
    if (has_age_row) {
      write_grouped_row(row_age, "Actual Age | Effective Age",
                        c(actage_col, effage_col), age_model_vars)
    }

    # === Model variable rows (excluding grouped vars) ===
    for (vi_idx in seq_along(mv_filtered_idx)) {
      vi <- mv_filtered_idx[vi_idx]
      rw <- row_vars_start + vi_idx - 1
      var_label <- mv$labels[vi]
      contrib_c <- mv$contrib[vi]
      adjust_c  <- mv$adjustment[vi]

      openxlsx::writeData(wb, s, format_label_(var_label),
                          startRow = rw, startCol = 1)

      if (var_label %in% colnames(df)) {
        fv <- col_val_(df, 1, var_label)
        openxlsx::mergeCells(wb, s, cols = 2:4, rows = rw)
        openxlsx::writeData(wb, s, fv, startRow = rw, startCol = 2)
      }
      subj_contrib <- col_num_(df, 1, contrib_c)
      openxlsx::writeData(wb, s, subj_contrib, startRow = rw, startCol = 5)
      openxlsx::addStyle(wb, s, curr_style, rows = rw, cols = 5, stack = TRUE)

      for (ci in seq_len(n_on_sheet)) {
        r <- sheet_comps[ci]
        col_start <- 1 + ci * 5
        if (var_label %in% colnames(df)) {
          fv <- col_val_(df, r, var_label)
          openxlsx::mergeCells(wb, s, cols = col_start:(col_start + 2), rows = rw)
          openxlsx::writeData(wb, s, fv, startRow = rw, startCol = col_start)
        }
        comp_contrib <- col_num_(df, r, contrib_c)
        comp_adj     <- col_num_(df, r, adjust_c)
        openxlsx::writeData(wb, s, comp_contrib,
                            startRow = rw, startCol = col_start + 3)
        openxlsx::writeData(wb, s, comp_adj,
                            startRow = rw, startCol = col_start + 4)
        openxlsx::addStyle(wb, s, curr_style, rows = rw,
                           cols = c(col_start + 3, col_start + 4), stack = TRUE)
      }
      openxlsx::addStyle(wb, s, body_style, rows = rw, cols = 1:20,
                         gridExpand = TRUE, stack = TRUE)
      openxlsx::addStyle(wb, s, label_style, rows = rw, cols = 1, stack = TRUE)
    }

    # === Blank separator ===
    openxlsx::addStyle(wb, s, body_style, rows = row_blank1, cols = 1:20,
                       gridExpand = TRUE, stack = TRUE)
    openxlsx::addStyle(wb, s, label_style, rows = row_blank1, cols = 1, stack = TRUE)

    # === Residual section header ===
    openxlsx::writeData(wb, s, "Residual Features",
                        startRow = row_resid_hdr, startCol = 1)
    openxlsx::mergeCells(wb, s, cols = 2:3, rows = row_resid_hdr)
    openxlsx::writeData(wb, s, "CQA / Description",
                        startRow = row_resid_hdr, startCol = 2)
    openxlsx::writeData(wb, s, "Value Contrib.",
                        startRow = row_resid_hdr, startCol = 5)
    for (ci in seq_len(n_on_sheet)) {
      col_start <- 1 + ci * 5
      openxlsx::mergeCells(wb, s, cols = col_start:(col_start + 2), rows = row_resid_hdr)
      openxlsx::writeData(wb, s, "Description",
                          startRow = row_resid_hdr, startCol = col_start)
      openxlsx::writeData(wb, s, "Value Contrib.",
                          startRow = row_resid_hdr, startCol = col_start + 3)
      openxlsx::writeData(wb, s, "Adjustment",
                          startRow = row_resid_hdr, startCol = col_start + 4)
    }
    openxlsx::addStyle(wb, s, green_hdr_style, rows = row_resid_hdr, cols = 1:20,
                       gridExpand = TRUE, stack = TRUE)
    openxlsx::addStyle(wb, s, label_style, rows = row_resid_hdr, cols = 1,
                       stack = TRUE)

    # === CQA | Residual row ===
    openxlsx::writeData(wb, s, "CQA | Residual",
                        startRow = row_cqa, startCol = 1)
    subj_cqa   <- col_num_(df, 1, "subject_cqa", digits = 2)
    subj_resid <- col_num_(df, 1, "residual")
    openxlsx::writeData(wb, s, subj_cqa, startRow = row_cqa, startCol = 2)
    openxlsx::addStyle(wb, s, section_hdr_style, rows = row_cqa, cols = 3:4,
                       gridExpand = TRUE, stack = TRUE)
    subj_vc_col <- col_letter_(5)
    subj_vc_formula <- paste0(subj_resid, "-SUM(",
                              subj_vc_col, row_resid_start, ":",
                              subj_vc_col, row_resid_end, ")")
    openxlsx::writeFormula(wb, s, x = subj_vc_formula,
                           startRow = row_cqa, startCol = 5)
    openxlsx::addStyle(wb, s, remaining_style, rows = row_cqa, cols = 5,
                       stack = TRUE)
    openxlsx::addStyle(wb, s, cqa_style, rows = row_cqa, cols = 2, stack = TRUE)
    for (ci in seq_len(n_on_sheet)) {
      r <- sheet_comps[ci]
      col_start <- 1 + ci * 5
      comp_cqa   <- col_num_(df, r, "cqa", digits = 2)
      comp_resid <- col_num_(df, r, "residual")
      openxlsx::writeData(wb, s, comp_cqa,
                          startRow = row_cqa, startCol = col_start)
      openxlsx::addStyle(wb, s, cqa_style, rows = row_cqa,
                         cols = col_start, stack = TRUE)
      vc_col <- col_letter_(col_start + 3)
      comp_vc_formula <- paste0(comp_resid, "-SUM(",
                                vc_col, row_resid_start, ":",
                                vc_col, row_resid_end, ")")
      openxlsx::writeFormula(wb, s, x = comp_vc_formula,
                             startRow = row_cqa, startCol = col_start + 3)
      openxlsx::addStyle(wb, s, remaining_style, rows = row_cqa,
                         cols = col_start + 3, stack = TRUE)
      comp_adj_formula <- paste0(subj_vc_col, row_cqa, "-",
                                 vc_col, row_cqa)
      openxlsx::writeFormula(wb, s, x = comp_adj_formula,
                             startRow = row_cqa, startCol = col_start + 4)
      openxlsx::addStyle(wb, s, remaining_style, rows = row_cqa,
                         cols = col_start + 4, stack = TRUE)
    }
    openxlsx::addStyle(wb, s, body_style, rows = row_cqa, cols = 1:20,
                       gridExpand = TRUE, stack = TRUE)
    openxlsx::addStyle(wb, s, label_style, rows = row_cqa, cols = 1, stack = TRUE)
    openxlsx::addStyle(wb, s, section_hdr_style, rows = row_cqa, cols = 3:4,
                       gridExpand = TRUE, stack = TRUE)
    openxlsx::addStyle(wb, s, cqa_style, rows = row_cqa, cols = 2, stack = TRUE)
    openxlsx::addStyle(wb, s, remaining_style, rows = row_cqa, cols = 5, stack = TRUE)

    # === Residual feature rows (named + blank for appraiser entry) ===
    all_resid_labels <- c(resid_named, rep("", n_resid_blank))
    subj_vc_letter <- col_letter_(5)
    for (ri in seq_along(all_resid_labels)) {
      rw <- row_resid_start + ri - 1
      if (nzchar(all_resid_labels[ri])) {
        openxlsx::writeData(wb, s, all_resid_labels[ri], startRow = rw, startCol = 1)
      }
      openxlsx::writeData(wb, s, 0, startRow = rw, startCol = 5)
      openxlsx::addStyle(wb, s, resid_input_style, rows = rw, cols = 5,
                         stack = TRUE)
      for (ci in seq_len(n_on_sheet)) {
        col_start <- 1 + ci * 5
        openxlsx::writeData(wb, s, 0, startRow = rw, startCol = col_start + 3)
        openxlsx::addStyle(wb, s, resid_input_style, rows = rw,
                           cols = col_start + 3, stack = TRUE)
        comp_vc_letter <- col_letter_(col_start + 3)
        adj_formula <- paste0(subj_vc_letter, rw, "-", comp_vc_letter, rw)
        openxlsx::writeFormula(wb, s, x = adj_formula,
                               startRow = rw, startCol = col_start + 4)
        openxlsx::addStyle(wb, s, resid_input_style, rows = rw,
                           cols = col_start + 4, stack = TRUE)
      }
      openxlsx::addStyle(wb, s, body_style, rows = rw, cols = 1:20,
                         gridExpand = TRUE, stack = TRUE)
      openxlsx::addStyle(wb, s, label_style, rows = rw, cols = 1, stack = TRUE)
      openxlsx::addStyle(wb, s, resid_input_style, rows = rw, cols = 5,
                         stack = TRUE)
      for (ci in seq_len(n_on_sheet)) {
        col_start <- 1 + ci * 5
        openxlsx::addStyle(wb, s, resid_input_style, rows = rw,
                           cols = col_start + 3, stack = TRUE)
        openxlsx::addStyle(wb, s, curr_style, rows = rw,
                           cols = col_start + 4, stack = TRUE)
      }
    }

    # === Net Adjustment ===
    openxlsx::writeData(wb, s, "Total VC / Net Adjustment",
                        startRow = row_net_adj, startCol = 1)
    subj_total_vc <- subj_basis
    for (vi in seq_len(length(mv$labels))) {
      subj_total_vc <- subj_total_vc + col_num_(df, 1, mv$contrib[vi])
    }
    openxlsx::writeData(wb, s, round(subj_total_vc), startRow = row_net_adj, startCol = 5)
    openxlsx::addStyle(wb, s, curr_style, rows = row_net_adj, cols = 5, stack = TRUE)
    for (ci in seq_len(n_on_sheet)) {
      r <- sheet_comps[ci]
      col_start <- 1 + ci * 5
      net_adj <- col_num_(df, r, "net_adjustments")
      openxlsx::writeData(wb, s, net_adj,
                          startRow = row_net_adj, startCol = col_start + 4)
      openxlsx::addStyle(wb, s, curr_style, rows = row_net_adj,
                         cols = col_start + 4, stack = TRUE)
    }
    openxlsx::addStyle(wb, s, body_style, rows = row_net_adj, cols = 1:20,
                       gridExpand = TRUE, stack = TRUE)
    openxlsx::addStyle(wb, s, label_style, rows = row_net_adj, cols = 1, stack = TRUE)

    # === Net Adj % ===
    openxlsx::writeData(wb, s, "Net Adjustment %",
                        startRow = row_net_pct, startCol = 1)
    for (ci in seq_len(n_on_sheet)) {
      r <- sheet_comps[ci]
      col_start <- 1 + ci * 5
      sp <- col_num_(df, r, "sale_price", default = NA)
      net <- col_num_(df, r, "net_adjustments")
      if (!is.na(sp) && sp != 0) {
        openxlsx::writeData(wb, s, round(net / sp, 3),
                            startRow = row_net_pct, startCol = col_start + 4)
        openxlsx::addStyle(wb, s, pct_style, rows = row_net_pct,
                           cols = col_start + 4, stack = TRUE)
      }
    }
    openxlsx::addStyle(wb, s, body_style, rows = row_net_pct, cols = 1:20,
                       gridExpand = TRUE, stack = TRUE)
    openxlsx::addStyle(wb, s, label_style, rows = row_net_pct, cols = 1, stack = TRUE)

    # === Gross Adj % ===
    openxlsx::writeData(wb, s, "Gross Adjustment %",
                        startRow = row_gross_pct, startCol = 1)
    for (ci in seq_len(n_on_sheet)) {
      r <- sheet_comps[ci]
      col_start <- 1 + ci * 5
      sp <- col_num_(df, r, "sale_price", default = NA)
      gross <- col_num_(df, r, "gross_adjustments")
      if (!is.na(sp) && sp != 0) {
        openxlsx::writeData(wb, s, round(gross / sp, 3),
                            startRow = row_gross_pct, startCol = col_start + 4)
        openxlsx::addStyle(wb, s, pct_style, rows = row_gross_pct,
                           cols = col_start + 4, stack = TRUE)
      }
    }
    openxlsx::addStyle(wb, s, body_style, rows = row_gross_pct, cols = 1:20,
                       gridExpand = TRUE, stack = TRUE)
    openxlsx::addStyle(wb, s, label_style, rows = row_gross_pct, cols = 1,
                       stack = TRUE)

    # === Adjusted Sale Price ===
    openxlsx::writeData(wb, s, "Adjusted Sale Price",
                        startRow = row_adj_sp, startCol = 1)

    subj_vc_l <- col_letter_(5)
    first_adj_row <- if (has_loc_row) row_loc
                     else if (has_site_row) row_site
                     else if (has_age_row) row_age
                     else if (n_vars > 0) row_vars_start
                     else row_cqa
    subj_asp_formula <- paste0("SUM(", subj_vc_l, row_base_value, ":",
                               subj_vc_l, row_resid_end, ")")
    openxlsx::mergeCells(wb, s, cols = 2:5, rows = row_adj_sp)
    openxlsx::writeFormula(wb, s, x = subj_asp_formula,
                           startRow = row_adj_sp, startCol = 2)
    openxlsx::addStyle(wb, s, adj_sp_style, rows = row_adj_sp, cols = 2,
                       stack = TRUE)

    for (ci in seq_len(n_on_sheet)) {
      r <- sheet_comps[ci]
      col_start <- 1 + ci * 5
      adj_col_l <- col_letter_(col_start + 4)
      net_sp_col_l <- col_letter_(col_start + 2)
      asp_formula <- paste0(net_sp_col_l, row_sale_price,
                            "+SUM(", adj_col_l, first_adj_row, ":",
                            adj_col_l, row_resid_end, ")")
      openxlsx::mergeCells(wb, s, cols = col_start:(col_start + 4), rows = row_adj_sp)
      openxlsx::writeFormula(wb, s, x = asp_formula,
                             startRow = row_adj_sp, startCol = col_start)
      openxlsx::addStyle(wb, s, adj_sp_style, rows = row_adj_sp,
                         cols = col_start, stack = TRUE)
    }
    openxlsx::addStyle(wb, s, adj_sp_style, rows = row_adj_sp, cols = 1:20,
                       gridExpand = TRUE, stack = TRUE)
    openxlsx::addStyle(wb, s, label_style, rows = row_adj_sp, cols = 1, stack = TRUE)

    # === Copyright ===
    openxlsx::mergeCells(wb, s, cols = 1:20, rows = row_copyright)
    openxlsx::writeData(wb, s,
                        paste0("Generated by earthUI ", format(Sys.time(), "%Y-%m-%d"),
                               "  |  Copyright 2022-",
                               format(Sys.Date(), "%Y"),
                               ", Pacific Vista Net"),
                        startRow = row_copyright, startCol = 1)
    openxlsx::addStyle(wb, s, copyright_style, rows = row_copyright, cols = 1:20,
                       gridExpand = TRUE, stack = TRUE)

    # === Sheet protection ===
    unlocked <- openxlsx::createStyle(locked = FALSE)
    resid_rows <- row_resid_start:row_resid_end
    openxlsx::addStyle(wb, s, unlocked, rows = resid_rows, cols = 5,
                       gridExpand = TRUE, stack = TRUE)
    for (ci in seq_len(n_on_sheet)) {
      col_start <- 1 + ci * 5
      openxlsx::addStyle(wb, s, unlocked, rows = resid_rows, cols = col_start + 3,
                         gridExpand = TRUE, stack = TRUE)
    }
    openxlsx::protectWorksheet(wb, s, protect = TRUE,
                               lockFormattingCells = FALSE, lockFormattingColumns = FALSE,
                               lockInsertingColumns = TRUE, lockInsertingRows = TRUE,
                               lockDeletingColumns = TRUE, lockDeletingRows = TRUE)

    if (is.function(progress_fn)) {
      progress_fn(sheet = s, total_sheets = n_sheets,
                  comps_done = min(s * 3, n_comps), total_comps = n_comps)
    }

  } # end sheet loop

  openxlsx::saveWorkbook(wb, output_file, overwrite = TRUE)
  message("Sales grid saved to: ", output_file)
  invisible(output_file)
}
