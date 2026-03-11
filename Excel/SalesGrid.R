#!/usr/bin/env Rscript
###############################################################################
# SalesGrid.R — Generate a formatted Sales Comparison Grid from earthUI output
#
# Author:  Wm. Bert Craytor / Claude Code
# License: AGPL-3.0
#
# Usage:
#   source("SalesGrid.R")
#   generate_sales_grid(
#     adjusted_file = "Output/Appraisal_1_adjusted_20260309_013940.xlsx",
#     comp_rows     = c(2, 3, 4),        # row numbers of comps to include
#     output_file   = "Output/SalesComparison.xlsx"
#   )
#
# The adjusted_file is the Excel output from earthUI Step 7
# (Calculate RCA Adjustments & Download).
#
# comp_rows: numeric vector of row numbers (2-based, since row 1 is subject).
#   Up to 12 comps supported (3 per sheet, 4 sheets max).
###############################################################################

library(openxlsx)
library(readxl)

# --- Helper: safe column lookup ---
col_val <- function(df, row, col, default = "") {
  if (col %in% colnames(df)) {
    v <- df[[col]][row]
    if (is.null(v) || length(v) == 0 || is.na(v)) return(default)
    return(v)
  }
  default
}

col_num <- function(df, row, col, digits = 0, default = 0) {
  v <- col_val(df, row, col, default = NA)
  if (is.na(v)) return(default)
  round(as.numeric(v), digits = digits)
}

# --- Haversine distance (miles) between two lat/lon points ---
haversine_miles <- function(lat1, lon1, lat2, lon2) {
  if (any(is.na(c(lat1, lon1, lat2, lon2)))) return(NA_real_)
  R <- 3958.8
  dlat <- (lat2 - lat1) * pi / 180
  dlon <- (lon2 - lon1) * pi / 180
  a <- sin(dlat / 2)^2 + cos(lat1 * pi / 180) * cos(lat2 * pi / 180) * sin(dlon / 2)^2
  R * 2 * asin(sqrt(a))
}

# --- Compute DOM (days on market) ---
compute_dom <- function(df, row) {
  cd <- col_val(df, row, "contract_date", default = NA)
  ld <- col_val(df, row, "listing_date", default = NA)
  if (is.na(cd) || is.na(ld)) return(NA_integer_)
  cd <- tryCatch(as.Date(cd), error = function(e) NA)
  ld <- tryCatch(as.Date(ld), error = function(e) NA)
  if (is.na(cd) || is.na(ld)) return(NA_integer_)
  as.integer(cd - ld)
}

# --- Detect contribution/adjustment columns ---
detect_model_vars <- function(df) {
  contrib_cols <- grep("_contribution$", colnames(df), value = TRUE)
  var_labels <- sub("_contribution$", "", contrib_cols)
  # Filter out rent_ prefixed (secondary target)
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

# --- Format variable label for display (abbreviated for grid) ---
format_label <- function(lbl) {
  # Common abbreviations for appraisal variables
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
  # Check exact match first
  if (lbl %in% names(abbrevs)) return(abbrevs[[lbl]])
  # For interaction terms (e.g., "area_id_living_sqft"), abbreviate each part
  parts <- strsplit(lbl, "_")[[1]]
  # Try to match known multi-word variable names within the interaction
  result <- lbl
  for (nm in names(abbrevs)) {
    if (grepl(nm, result, fixed = TRUE)) {
      result <- sub(nm, abbrevs[[nm]], result, fixed = TRUE)
    }
  }
  # Clean up remaining underscores and title-case
  result <- gsub("_", " ", result)
  result <- trimws(result)
  # Truncate to 28 chars for column width
  if (nchar(result) > 28) result <- paste0(substr(result, 1, 25), "...")
  result
}

# --- Excel column letter from number ---
col_letter <- function(n) {
  # Handles columns A-Z and AA-AZ (up to col 52)
  if (n <= 26) {
    return(LETTERS[n])
  } else {
    return(paste0(LETTERS[(n - 1) %/% 26], LETTERS[((n - 1) %% 26) + 1]))
  }
}

###############################################################################
# Main function
###############################################################################
generate_sales_grid <- function(adjusted_file,
                                comp_rows,
                                output_file = NULL,
                                title_prefix = "Intermediate Sales Comparable Grid",
                                dom_col = NULL,
                                contract_date_col = NULL,
                                progress_fn = NULL) {

  if (!file.exists(adjusted_file)) {
    stop("Adjusted file not found: ", adjusted_file)
  }

  df <- readxl::read_excel(adjusted_file)
  n_total <- nrow(df)

  # Validate comp_rows
  comp_rows <- as.integer(comp_rows)
  if (any(comp_rows < 2 | comp_rows > n_total)) {
    stop("comp_rows must be between 2 and ", n_total)
  }
  if (length(comp_rows) > 30) {
    stop("Maximum 30 comps supported (10 sheets)")
  }

  # Detect model variables
  mv <- detect_model_vars(df)
  n_vars <- length(mv$labels)

  # Default output file
  if (is.null(output_file)) {
    base <- tools::file_path_sans_ext(basename(adjusted_file))
    output_file <- file.path(dirname(adjusted_file),
                             paste0(base, "_salesgrid_",
                                    format(Sys.time(), "%Y%m%d_%H%M%S"),
                                    ".xlsx"))
  }

  # --- Residual feature labels ---
  resid_named   <- c("View", "Design", "Quality of Construction",
                      "Condition", "Functional Utility")
  n_resid_named <- length(resid_named)
  n_resid_blank <- 6
  n_resid_rows  <- n_resid_named + n_resid_blank  # 11 total

  # --- Detect lat/lon columns for proximity ---
  lat_col <- NULL; lon_col <- NULL
  for (cn in colnames(df)) {
    cl <- tolower(cn)
    if (is.null(lat_col) && cl %in% c("latitude", "lat")) lat_col <- cn
    if (is.null(lon_col) && cl %in% c("longitude", "lon", "lng")) lon_col <- cn
  }

  # --- Layout constants (dynamic based on model variables) ---
  row_title       <- 1
  row_headers     <- 2
  row_address     <- 3
  row_apn         <- 4
  row_sale_price  <- 5
  row_regr_hdr    <- 6
  row_base_value  <- 7
  row_date_info   <- 8
  row_vars_start  <- 9
  row_vars_end    <- row_vars_start + n_vars - 1
  row_blank1      <- row_vars_end + 1
  row_resid_hdr   <- row_blank1 + 1
  row_cqa         <- row_resid_hdr + 1
  row_remaining   <- row_cqa + 1          # Remaining Residual (formula row)
  row_resid_start <- row_remaining + 1    # First residual feature row
  row_resid_end   <- row_resid_start + n_resid_rows - 1
  row_net_adj     <- row_resid_end + 1
  row_net_pct     <- row_net_adj + 1
  row_gross_pct   <- row_net_pct + 1
  row_adj_sp      <- row_gross_pct + 1
  row_copyright   <- row_adj_sp + 1

  # Column layout per comp block (5 cols each):
  # Subject:  cols 1-5  (label, fv1, fv2, fv3, vc)
  # Comp 1:   cols 6-10 (fv1, fv2, fv3, vc, adj)
  # Comp 2:   cols 11-15
  # Comp 3:   cols 16-20

  # --- Styles ---
  title_style <- createStyle(
    fontSize = 11, textDecoration = "bold",
    fontColour = "#FFFFFF", fgFill = "#002060",
    halign = "center", valign = "center",
    border = "TopBottomLeftRight", borderColour = "#002060",
    borderStyle = "thick"
  )
  section_hdr_style <- createStyle(
    textDecoration = "bold",
    fgFill = "#CCCCFF", halign = "center", valign = "center",
    border = "TopBottomLeftRight", borderColour = "#002060",
    borderStyle = "thin"
  )
  label_style <- createStyle(
    textDecoration = "bold",
    fgFill = "#CCCCFF", halign = "left", valign = "center",
    border = "TopBottomLeftRight", borderColour = "#002060",
    borderStyle = "thin"
  )
  green_hdr_style <- createStyle(
    textDecoration = "bold",
    fgFill = "#E2EFDA", halign = "center", valign = "center",
    border = "TopBottomLeftRight", borderColour = "#002060",
    borderStyle = "thin"
  )
  body_style <- createStyle(
    halign = "center", valign = "center",
    border = "TopBottomLeftRight", borderColour = "#002060",
    borderStyle = "thin", fgFill = "#FFFFFF"
  )
  curr_style <- createStyle(
    numFmt = "$#,##0", halign = "right", valign = "center",
    border = "TopBottomLeftRight", borderColour = "#002060",
    borderStyle = "thin", fgFill = "#FFFFFF"
  )
  pct_style <- createStyle(
    numFmt = "0.0%", halign = "center", valign = "center",
    border = "TopBottomLeftRight", borderColour = "#002060",
    borderStyle = "thin", fgFill = "#FFFFFF"
  )
  cqa_style <- createStyle(
    numFmt = "0.00", halign = "center", valign = "center",
    border = "TopBottomLeftRight", borderColour = "#002060",
    borderStyle = "thin", fgFill = "#FFFFFF"
  )
  copyright_style <- createStyle(
    halign = "center", valign = "center",
    fgFill = "#CCCCFF",
    border = "TopBottomLeftRight", borderColour = "#002060",
    borderStyle = "thick"
  )
  adj_sp_style <- createStyle(
    numFmt = "$#,##0", halign = "center", valign = "center",
    textDecoration = "bold",
    border = "TopBottomLeftRight", borderColour = "#002060",
    borderStyle = "thin", fgFill = "#CCCCFF"
  )
  remaining_style <- createStyle(
    numFmt = "$#,##0", halign = "right", valign = "center",
    textDecoration = "bold", fontColour = "#C00000",
    border = "TopBottomLeftRight", borderColour = "#002060",
    borderStyle = "thin", fgFill = "#FFF2CC"
  )
  resid_input_style <- createStyle(
    numFmt = "$#,##0", halign = "right", valign = "center",
    border = "TopBottomLeftRight", borderColour = "#002060",
    borderStyle = "thin", fgFill = "#FFFFDD"
  )

  # --- Create workbook ---
  wb <- createWorkbook()
  modifyBaseFont(wb, fontSize = 9, fontName = "Arial Narrow")

  n_comps <- length(comp_rows)
  n_sheets <- ceiling(n_comps / 3)

  col_widths <- c(29, 11, 8, 4, 12, 11, 8, 7, 12, 11,
                  11, 8, 7, 12, 11, 11, 8, 7, 12, 11)

  for (s in seq_len(n_sheets)) {
    # Comps for this sheet
    idx_start <- (s - 1) * 3 + 1
    idx_end   <- min(s * 3, n_comps)
    sheet_comps <- comp_rows[idx_start:idx_end]
    n_on_sheet  <- length(sheet_comps)

    comp_first <- idx_start
    sheet_name <- paste0("Comps ", comp_first, "-", comp_first + 2)

    addWorksheet(wb, sheet_name)
    setColWidths(wb, s, cols = 1:20, widths = col_widths)

    # === Row 1: Title ===
    mergeCells(wb, s, cols = 1:20, rows = row_title)
    writeData(wb, s, paste0(title_prefix, ": Comps ", comp_first, "-",
                            comp_first + 2),
              startRow = row_title, startCol = 1)
    addStyle(wb, s, title_style, rows = row_title, cols = 1:20,
             gridExpand = TRUE, stack = TRUE)

    # === Row 2: Group headers ===
    mergeCells(wb, s, cols = 2:5,   rows = row_headers)
    mergeCells(wb, s, cols = 6:10,  rows = row_headers)
    mergeCells(wb, s, cols = 11:15, rows = row_headers)
    mergeCells(wb, s, cols = 16:20, rows = row_headers)
    writeData(wb, s, "Subject",
              startRow = row_headers, startCol = 2)
    for (ci in seq_len(n_on_sheet)) {
      comp_num <- idx_start + ci - 1
      col_start <- 1 + ci * 5
      writeData(wb, s, paste("Comparable Sale No.", comp_num),
                startRow = row_headers, startCol = col_start)
    }
    addStyle(wb, s, section_hdr_style, rows = row_headers, cols = 1:20,
             gridExpand = TRUE, stack = TRUE)

    # === Row 3: Address ===
    writeData(wb, s, "Street, City, State Zip",
              startRow = row_address, startCol = 1)
    subj_addr <- paste0(col_val(df, 1, "street_address"), ", ",
                        col_val(df, 1, "city_name"), " ",
                        col_val(df, 1, "postal_code"))
    mergeCells(wb, s, cols = 2:5, rows = row_address)
    writeData(wb, s, subj_addr, startRow = row_address, startCol = 2)
    for (ci in seq_len(n_on_sheet)) {
      r <- sheet_comps[ci]
      col_start <- 1 + ci * 5
      comp_addr <- paste0(col_val(df, r, "street_address"), ", ",
                          col_val(df, r, "city_name"), " ",
                          col_val(df, r, "postal_code"))
      mergeCells(wb, s, cols = col_start:(col_start + 4), rows = row_address)
      writeData(wb, s, comp_addr, startRow = row_address, startCol = col_start)
    }
    addStyle(wb, s, green_hdr_style, rows = row_address, cols = 1:20,
             gridExpand = TRUE, stack = TRUE)
    addStyle(wb, s, label_style, rows = row_address, cols = 1, stack = TRUE)

    # === Row 4: APN | MLS# | DOM | Subj.Prox ===
    writeData(wb, s, "APN | MLS# | DOM | Subj.Prox",
              startRow = row_apn, startCol = 1)
    # Subject: APN in cols 2-3, DOM in col 4, Subj.Prox = 0 in col 5
    mergeCells(wb, s, cols = 2:3, rows = row_apn)
    writeData(wb, s, col_val(df, 1, "parcel_number"),
              startRow = row_apn, startCol = 2)
    subj_dom <- if (!is.null(dom_col)) {
      v <- col_val(df, 1, dom_col, default = NA)
      if (!is.na(v)) as.integer(v) else NA_integer_
    } else {
      compute_dom(df, 1)
    }
    if (!is.na(subj_dom)) {
      writeData(wb, s, subj_dom, startRow = row_apn, startCol = 4)
    }
    writeData(wb, s, "0.00 mi", startRow = row_apn, startCol = 5)
    # Subject lat/lon for proximity calc
    subj_lat <- if (!is.null(lat_col)) as.numeric(col_val(df, 1, lat_col, NA)) else NA
    subj_lon <- if (!is.null(lon_col)) as.numeric(col_val(df, 1, lon_col, NA)) else NA
    for (ci in seq_len(n_on_sheet)) {
      r <- sheet_comps[ci]
      col_start <- 1 + ci * 5
      # APN in col_start, MLS# in col_start+1
      writeData(wb, s, col_val(df, r, "parcel_number"),
                startRow = row_apn, startCol = col_start)
      writeData(wb, s, col_val(df, r, "listing_id"),
                startRow = row_apn, startCol = col_start + 1)
      # DOM in col_start+2
      comp_dom <- if (!is.null(dom_col)) {
        v <- col_val(df, r, dom_col, default = NA)
        if (!is.na(v)) as.integer(v) else NA_integer_
      } else {
        compute_dom(df, r)
      }
      if (!is.na(comp_dom)) {
        writeData(wb, s, comp_dom, startRow = row_apn, startCol = col_start + 2)
      }
      # Subj.Prox in col_start+3:col_start+4
      comp_lat <- if (!is.null(lat_col)) as.numeric(col_val(df, r, lat_col, NA)) else NA
      comp_lon <- if (!is.null(lon_col)) as.numeric(col_val(df, r, lon_col, NA)) else NA
      prox <- haversine_miles(subj_lat, subj_lon, comp_lat, comp_lon)
      mergeCells(wb, s, cols = (col_start + 3):(col_start + 4), rows = row_apn)
      if (!is.na(prox)) {
        writeData(wb, s, sprintf("%.2f mi", prox),
                  startRow = row_apn, startCol = col_start + 3)
      }
    }
    addStyle(wb, s, body_style, rows = row_apn, cols = 1:20,
             gridExpand = TRUE, stack = TRUE)
    addStyle(wb, s, label_style, rows = row_apn, cols = 1, stack = TRUE)

    # === Sale Price ===
    writeData(wb, s, "Sale Price",
              startRow = row_sale_price, startCol = 1)
    mergeCells(wb, s, cols = 2:5, rows = row_sale_price)
    writeData(wb, s, "N/A", startRow = row_sale_price, startCol = 2)
    for (ci in seq_len(n_on_sheet)) {
      r <- sheet_comps[ci]
      col_start <- 1 + ci * 5
      mergeCells(wb, s, cols = col_start:(col_start + 4),
                 rows = row_sale_price)
      sp <- col_num(df, r, "sale_price")
      writeData(wb, s, sp, startRow = row_sale_price, startCol = col_start)
      addStyle(wb, s, curr_style, rows = row_sale_price,
               cols = col_start, stack = TRUE)
    }
    addStyle(wb, s, body_style, rows = row_sale_price, cols = 1:20,
             gridExpand = TRUE, stack = TRUE)
    addStyle(wb, s, label_style, rows = row_sale_price, cols = 1,
             stack = TRUE)

    # === Row 6: Regression Features header ===
    writeData(wb, s, "Regression Features",
              startRow = row_regr_hdr, startCol = 1)
    mergeCells(wb, s, cols = 2:3, rows = row_regr_hdr)
    writeData(wb, s, "Factual Value",
              startRow = row_regr_hdr, startCol = 2)
    writeData(wb, s, "Value Contrib.",
              startRow = row_regr_hdr, startCol = 5)
    for (ci in seq_len(n_on_sheet)) {
      col_start <- 1 + ci * 5
      mergeCells(wb, s, cols = col_start:(col_start + 2), rows = row_regr_hdr)
      writeData(wb, s, "Factual Value",
                startRow = row_regr_hdr, startCol = col_start)
      writeData(wb, s, "Value Contrib.",
                startRow = row_regr_hdr, startCol = col_start + 3)
      writeData(wb, s, "Adjustment",
                startRow = row_regr_hdr, startCol = col_start + 4)
    }
    addStyle(wb, s, green_hdr_style, rows = row_regr_hdr, cols = 1:20,
             gridExpand = TRUE, stack = TRUE)
    addStyle(wb, s, label_style, rows = row_regr_hdr, cols = 1,
             stack = TRUE)

    # === Row 7: Base Value (intercept) ===
    writeData(wb, s, "BASE VALUE",
              startRow = row_base_value, startCol = 1)
    subj_basis <- col_num(df, 1, "basis")
    writeData(wb, s, subj_basis,
              startRow = row_base_value, startCol = 5)
    addStyle(wb, s, curr_style, rows = row_base_value, cols = 5,
             stack = TRUE)
    for (ci in seq_len(n_on_sheet)) {
      r <- sheet_comps[ci]
      col_start <- 1 + ci * 5
      comp_basis <- col_num(df, r, "basis")
      writeData(wb, s, comp_basis,
                startRow = row_base_value, startCol = col_start + 3)
      addStyle(wb, s, curr_style, rows = row_base_value,
               cols = col_start + 3, stack = TRUE)
    }
    addStyle(wb, s, body_style, rows = row_base_value, cols = 1:20,
             gridExpand = TRUE, stack = TRUE)
    addStyle(wb, s, label_style, rows = row_base_value, cols = 1,
             stack = TRUE)

    # === Date of Sale | OffMkt | OnMkt row ===
    writeData(wb, s, "Date of Sale | OffMkt | OnMkt",
              startRow = row_date_info, startCol = 1)
    cd_col <- contract_date_col
    if (!is.null(cd_col) && cd_col %in% colnames(df)) {
      subj_cd <- col_val(df, 1, cd_col, default = "")
      writeData(wb, s, subj_cd, startRow = row_date_info, startCol = 2)
    }
    if ("sale_age" %in% colnames(df)) {
      writeData(wb, s, col_num(df, 1, "sale_age"),
                startRow = row_date_info, startCol = 3)
    }
    if (!is.null(dom_col) && dom_col %in% colnames(df)) {
      writeData(wb, s, col_num(df, 1, dom_col),
                startRow = row_date_info, startCol = 4)
    }
    for (ci in seq_len(n_on_sheet)) {
      r <- sheet_comps[ci]
      col_start <- 1 + ci * 5
      if (!is.null(cd_col) && cd_col %in% colnames(df)) {
        writeData(wb, s, col_val(df, r, cd_col, default = ""),
                  startRow = row_date_info, startCol = col_start)
      }
      if ("sale_age" %in% colnames(df)) {
        writeData(wb, s, col_num(df, r, "sale_age"),
                  startRow = row_date_info, startCol = col_start + 1)
      }
      if (!is.null(dom_col) && dom_col %in% colnames(df)) {
        writeData(wb, s, col_num(df, r, dom_col),
                  startRow = row_date_info, startCol = col_start + 2)
      }
    }
    addStyle(wb, s, body_style, rows = row_date_info, cols = 1:20,
             gridExpand = TRUE, stack = TRUE)
    addStyle(wb, s, label_style, rows = row_date_info, cols = 1,
             stack = TRUE)

    # === Model variable rows (factual values + contributions) ===
    for (vi in seq_len(n_vars)) {
      rw <- row_vars_start + vi - 1
      var_label <- mv$labels[vi]
      contrib_c <- mv$contrib[vi]
      adjust_c  <- mv$adjustment[vi]

      writeData(wb, s, format_label(var_label),
                startRow = rw, startCol = 1)

      # Subject: factual value + contribution
      if (var_label %in% colnames(df)) {
        fv <- col_val(df, 1, var_label)
        mergeCells(wb, s, cols = 2:4, rows = rw)
        writeData(wb, s, fv, startRow = rw, startCol = 2)
      }
      subj_contrib <- col_num(df, 1, contrib_c)
      writeData(wb, s, subj_contrib, startRow = rw, startCol = 5)
      addStyle(wb, s, curr_style, rows = rw, cols = 5, stack = TRUE)

      # Comps: factual value + contribution + adjustment
      for (ci in seq_len(n_on_sheet)) {
        r <- sheet_comps[ci]
        col_start <- 1 + ci * 5
        if (var_label %in% colnames(df)) {
          fv <- col_val(df, r, var_label)
          mergeCells(wb, s, cols = col_start:(col_start + 2), rows = rw)
          writeData(wb, s, fv, startRow = rw, startCol = col_start)
        }
        comp_contrib <- col_num(df, r, contrib_c)
        comp_adj     <- col_num(df, r, adjust_c)
        writeData(wb, s, comp_contrib,
                  startRow = rw, startCol = col_start + 3)
        writeData(wb, s, comp_adj,
                  startRow = rw, startCol = col_start + 4)
        addStyle(wb, s, curr_style, rows = rw,
                 cols = c(col_start + 3, col_start + 4), stack = TRUE)
      }
      addStyle(wb, s, body_style, rows = rw, cols = 1:20,
               gridExpand = TRUE, stack = TRUE)
      addStyle(wb, s, label_style, rows = rw, cols = 1, stack = TRUE)
    }

    # === Blank separator ===
    addStyle(wb, s, body_style, rows = row_blank1, cols = 1:20,
             gridExpand = TRUE, stack = TRUE)
    addStyle(wb, s, label_style, rows = row_blank1, cols = 1, stack = TRUE)

    # === Residual section header ===
    writeData(wb, s, "Residual Features",
              startRow = row_resid_hdr, startCol = 1)
    mergeCells(wb, s, cols = 2:3, rows = row_resid_hdr)
    writeData(wb, s, "CQA / Description",
              startRow = row_resid_hdr, startCol = 2)
    writeData(wb, s, "Value Contrib.",
              startRow = row_resid_hdr, startCol = 5)
    for (ci in seq_len(n_on_sheet)) {
      col_start <- 1 + ci * 5
      mergeCells(wb, s, cols = col_start:(col_start + 2), rows = row_resid_hdr)
      writeData(wb, s, "Description",
                startRow = row_resid_hdr, startCol = col_start)
      writeData(wb, s, "Value Contrib.",
                startRow = row_resid_hdr, startCol = col_start + 3)
      writeData(wb, s, "Adjustment",
                startRow = row_resid_hdr, startCol = col_start + 4)
    }
    addStyle(wb, s, green_hdr_style, rows = row_resid_hdr, cols = 1:20,
             gridExpand = TRUE, stack = TRUE)
    addStyle(wb, s, label_style, rows = row_resid_hdr, cols = 1,
             stack = TRUE)

    # === CQA | Residual row ===
    writeData(wb, s, "CQA | Residual",
              startRow = row_cqa, startCol = 1)
    subj_cqa   <- col_num(df, 1, "subject_cqa", digits = 2)
    subj_resid <- col_num(df, 1, "residual")
    writeData(wb, s, subj_cqa, startRow = row_cqa, startCol = 2)
    writeData(wb, s, subj_resid, startRow = row_cqa, startCol = 5)
    addStyle(wb, s, cqa_style, rows = row_cqa, cols = 2, stack = TRUE)
    addStyle(wb, s, curr_style, rows = row_cqa, cols = 5, stack = TRUE)
    for (ci in seq_len(n_on_sheet)) {
      r <- sheet_comps[ci]
      col_start <- 1 + ci * 5
      comp_cqa   <- col_num(df, r, "cqa", digits = 2)
      comp_resid <- col_num(df, r, "residual")
      comp_resid_adj <- col_num(df, r, "residual_adjustment")
      writeData(wb, s, comp_cqa,
                startRow = row_cqa, startCol = col_start)
      writeData(wb, s, comp_resid,
                startRow = row_cqa, startCol = col_start + 3)
      writeData(wb, s, comp_resid_adj,
                startRow = row_cqa, startCol = col_start + 4)
      addStyle(wb, s, cqa_style, rows = row_cqa,
               cols = col_start, stack = TRUE)
      addStyle(wb, s, curr_style, rows = row_cqa,
               cols = c(col_start + 3, col_start + 4), stack = TRUE)
    }
    addStyle(wb, s, body_style, rows = row_cqa, cols = 1:20,
             gridExpand = TRUE, stack = TRUE)
    addStyle(wb, s, label_style, rows = row_cqa, cols = 1, stack = TRUE)

    # === Remaining Residual row (formula: residual - SUM of feature entries) ===
    writeData(wb, s, "Remaining Residual",
              startRow = row_remaining, startCol = 1)

    # Subject remaining residual formula:
    #   = E{row_cqa} - SUM(E{row_resid_start}:E{row_resid_end})
    subj_vc_col <- col_letter(5)  # column E
    subj_formula <- paste0(subj_vc_col, row_cqa, "-SUM(",
                           subj_vc_col, row_resid_start, ":",
                           subj_vc_col, row_resid_end, ")")
    writeFormula(wb, s, x = subj_formula,
                 startRow = row_remaining, startCol = 5)
    addStyle(wb, s, remaining_style, rows = row_remaining, cols = 5,
             stack = TRUE)

    # Comp remaining residual formulas
    for (ci in seq_len(n_on_sheet)) {
      col_start <- 1 + ci * 5
      # Value contrib column = col_start + 3
      vc_col <- col_letter(col_start + 3)
      # Adjustment column = col_start + 4
      adj_col <- col_letter(col_start + 4)

      # Remaining VC = comp residual - SUM(residual feature VCs)
      vc_formula <- paste0(vc_col, row_cqa, "-SUM(",
                           vc_col, row_resid_start, ":",
                           vc_col, row_resid_end, ")")
      writeFormula(wb, s, x = vc_formula,
                   startRow = row_remaining, startCol = col_start + 3)
      addStyle(wb, s, remaining_style, rows = row_remaining,
               cols = col_start + 3, stack = TRUE)

      # Remaining adjustment = residual_adj - SUM(residual feature adjs)
      adj_formula <- paste0(adj_col, row_cqa, "-SUM(",
                            adj_col, row_resid_start, ":",
                            adj_col, row_resid_end, ")")
      writeFormula(wb, s, x = adj_formula,
                   startRow = row_remaining, startCol = col_start + 4)
      addStyle(wb, s, remaining_style, rows = row_remaining,
               cols = col_start + 4, stack = TRUE)
    }
    addStyle(wb, s, body_style, rows = row_remaining, cols = 1:20,
             gridExpand = TRUE, stack = TRUE)
    addStyle(wb, s, label_style, rows = row_remaining, cols = 1,
             stack = TRUE)

    # === Residual feature rows (named + blank for appraiser entry) ===
    all_resid_labels <- c(resid_named, rep("", n_resid_blank))
    for (ri in seq_along(all_resid_labels)) {
      rw <- row_resid_start + ri - 1
      if (nzchar(all_resid_labels[ri])) {
        writeData(wb, s, all_resid_labels[ri], startRow = rw, startCol = 1)
      }
      # Style the value contrib & adjustment cells as editable (light yellow)
      addStyle(wb, s, resid_input_style, rows = rw, cols = 5,
               stack = TRUE)
      for (ci in seq_len(n_on_sheet)) {
        col_start <- 1 + ci * 5
        addStyle(wb, s, resid_input_style, rows = rw,
                 cols = c(col_start + 3, col_start + 4), stack = TRUE)
      }
      addStyle(wb, s, body_style, rows = rw, cols = 1:20,
               gridExpand = TRUE, stack = TRUE)
      addStyle(wb, s, label_style, rows = rw, cols = 1, stack = TRUE)
      # Re-apply input style on top (stack)
      addStyle(wb, s, resid_input_style, rows = rw, cols = 5,
               stack = TRUE)
      for (ci in seq_len(n_on_sheet)) {
        col_start <- 1 + ci * 5
        addStyle(wb, s, resid_input_style, rows = rw,
                 cols = c(col_start + 3, col_start + 4), stack = TRUE)
      }
    }

    # === Net Adjustment ===
    writeData(wb, s, "Total VC / Net Adjustment",
              startRow = row_net_adj, startCol = 1)
    # Subject total VC = basis + sum of var contribs + sum of residual contribs
    subj_total_vc <- subj_basis
    for (vi in seq_len(n_vars)) {
      subj_total_vc <- subj_total_vc + col_num(df, 1, mv$contrib[vi])
    }
    writeData(wb, s, round(subj_total_vc), startRow = row_net_adj, startCol = 5)
    addStyle(wb, s, curr_style, rows = row_net_adj, cols = 5, stack = TRUE)
    for (ci in seq_len(n_on_sheet)) {
      r <- sheet_comps[ci]
      col_start <- 1 + ci * 5
      net_adj <- col_num(df, r, "net_adjustments")
      writeData(wb, s, net_adj,
                startRow = row_net_adj, startCol = col_start + 4)
      addStyle(wb, s, curr_style, rows = row_net_adj,
               cols = col_start + 4, stack = TRUE)
    }
    addStyle(wb, s, body_style, rows = row_net_adj, cols = 1:20,
             gridExpand = TRUE, stack = TRUE)
    addStyle(wb, s, label_style, rows = row_net_adj, cols = 1, stack = TRUE)

    # === Net Adj % ===
    writeData(wb, s, "Net Adjustment %",
              startRow = row_net_pct, startCol = 1)
    for (ci in seq_len(n_on_sheet)) {
      r <- sheet_comps[ci]
      col_start <- 1 + ci * 5
      sp <- col_num(df, r, "sale_price", default = NA)
      net <- col_num(df, r, "net_adjustments")
      if (!is.na(sp) && sp != 0) {
        writeData(wb, s, round(net / sp, 3),
                  startRow = row_net_pct, startCol = col_start + 4)
        addStyle(wb, s, pct_style, rows = row_net_pct,
                 cols = col_start + 4, stack = TRUE)
      }
    }
    addStyle(wb, s, body_style, rows = row_net_pct, cols = 1:20,
             gridExpand = TRUE, stack = TRUE)
    addStyle(wb, s, label_style, rows = row_net_pct, cols = 1, stack = TRUE)

    # === Gross Adj % ===
    writeData(wb, s, "Gross Adjustment %",
              startRow = row_gross_pct, startCol = 1)
    for (ci in seq_len(n_on_sheet)) {
      r <- sheet_comps[ci]
      col_start <- 1 + ci * 5
      sp <- col_num(df, r, "sale_price", default = NA)
      gross <- col_num(df, r, "gross_adjustments")
      if (!is.na(sp) && sp != 0) {
        writeData(wb, s, round(gross / sp, 3),
                  startRow = row_gross_pct, startCol = col_start + 4)
        addStyle(wb, s, pct_style, rows = row_gross_pct,
                 cols = col_start + 4, stack = TRUE)
      }
    }
    addStyle(wb, s, body_style, rows = row_gross_pct, cols = 1:20,
             gridExpand = TRUE, stack = TRUE)
    addStyle(wb, s, label_style, rows = row_gross_pct, cols = 1,
             stack = TRUE)

    # === Adjusted Sale Price ===
    writeData(wb, s, "Adjusted Sale Price",
              startRow = row_adj_sp, startCol = 1)
    subj_val <- col_num(df, 1, "subject_value")
    if (subj_val != 0) {
      mergeCells(wb, s, cols = 2:5, rows = row_adj_sp)
      writeData(wb, s, subj_val, startRow = row_adj_sp, startCol = 2)
      addStyle(wb, s, adj_sp_style, rows = row_adj_sp, cols = 2,
               stack = TRUE)
    }
    for (ci in seq_len(n_on_sheet)) {
      r <- sheet_comps[ci]
      col_start <- 1 + ci * 5
      adj_sp <- col_num(df, r, "adjusted_sale_price")
      mergeCells(wb, s, cols = col_start:(col_start + 4), rows = row_adj_sp)
      writeData(wb, s, adj_sp,
                startRow = row_adj_sp, startCol = col_start)
      addStyle(wb, s, adj_sp_style, rows = row_adj_sp,
               cols = col_start, stack = TRUE)
    }
    addStyle(wb, s, adj_sp_style, rows = row_adj_sp, cols = 1:20,
             gridExpand = TRUE, stack = TRUE)
    addStyle(wb, s, label_style, rows = row_adj_sp, cols = 1, stack = TRUE)

    # === Copyright ===
    mergeCells(wb, s, cols = 1:20, rows = row_copyright)
    writeData(wb, s,
              paste0("Generated by earthUI ", format(Sys.time(), "%Y-%m-%d"),
                     "  |  Copyright 2022-",
                     format(Sys.Date(), "%Y"),
                     ", Pacific Vista Net"),
              startRow = row_copyright, startCol = 1)
    addStyle(wb, s, copyright_style, rows = row_copyright, cols = 1:20,
             gridExpand = TRUE, stack = TRUE)

    # Report progress
    if (is.function(progress_fn)) {
      progress_fn(sheet = s, total_sheets = n_sheets,
                  comps_done = min(s * 3, n_comps), total_comps = n_comps)
    }

  } # end sheet loop

  # --- Save ---
  saveWorkbook(wb, output_file, overwrite = TRUE)
  message("Sales grid saved to: ", output_file)
  invisible(output_file)
}


###############################################################################
# If run as a script, show usage
###############################################################################
if (!interactive() && identical(sys.nframe(), 0L)) {
  cat("Usage:\n")
  cat('  source("SalesGrid.R")\n')
  cat('  generate_sales_grid(\n')
  cat('    adjusted_file = "Output/Appraisal_1_adjusted_20260309_013940.xlsx",\n')
  cat('    comp_rows     = c(2, 3, 4),\n')
  cat('    output_file   = "Output/SalesComparison.xlsx"\n')
  cat('  )\n')
}
