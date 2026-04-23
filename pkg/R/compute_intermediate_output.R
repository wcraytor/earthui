#' Compute intermediate output data frame
#'
#' Returns an enhanced copy of `data` with per-target model columns appended:
#' `est_<target>`, `residual`, `cqa`, optional `residual_sf` / `cqa_sf` (when
#' a living-area column is supplied), per-g-function `<var>_contribution`
#' columns, `basis`, and `calc_residual`. For appraisal or market purposes,
#' rows are sorted by `residual_sf` (or `residual`) descending, with the
#' subject row (row 1) pinned on top when applicable. Ranking columns
#' (`residual_sf`, `cqa_sf`, `residual`, `cqa`) are moved to the leftmost
#' positions.
#'
#' This is the non-Shiny computation kernel used by the earthUI Shiny app's
#' Download Intermediate Output button, and is also suitable for use from
#' batch scripts.
#'
#' @param data A data frame (the raw imported data). Must contain the target
#'   column(s) named in `result$target`.
#' @param result A fit result list as returned by [fit_earth()], or `NULL`.
#'   When `NULL`, `data` is returned unchanged (the Shiny app's "non-adjusted"
#'   export path).
#' @param purpose Character scalar: `"general"`, `"appraisal"`, or
#'   `"market"`. Controls subject-row handling and sort behavior. Default
#'   `"general"`.
#' @param skip_subject_row Logical. In `"market"` mode, indicates whether
#'   row 1 should be treated as a subject (pinned on top during sort).
#'   Ignored for other purposes. Default `FALSE`.
#' @param living_area_col Character scalar or `NULL`. If supplied and present
#'   in `data`, per-SF residual and CQA_SF columns are added. Default `NULL`.
#'
#' @return A data frame.
#'
#' @export
compute_intermediate_output <- function(data,
                                        result           = NULL,
                                        purpose          = c("general", "appraisal", "market"),
                                        skip_subject_row = FALSE,
                                        living_area_col  = NULL) {
  if (!is.data.frame(data)) {
    stop("`data` must be a data frame.", call. = FALSE)
  }
  purpose <- match.arg(purpose)

  export_df <- data

  if (is.null(result)) {
    return(export_df)
  }

  model   <- result$model
  targets <- result$target
  eq      <- format_model_equation(result)
  multi   <- length(targets) > 1L

  la_col <- if (!is.null(living_area_col) && living_area_col %in% names(export_df)) {
    living_area_col
  } else {
    NULL
  }

  # Align factor levels in export_df with training data so predict() works;
  # rows with unseen levels produce NA predictions.
  train_df <- result$data
  pred_df  <- export_df
  for (col in names(train_df)) {
    if (is.factor(train_df[[col]]) && col %in% names(pred_df)) {
      pred_df[[col]] <- factor(pred_df[[col]], levels = levels(train_df[[col]]))
    }
  }
  pred_mat <- stats::predict(model, newdata = pred_df)

  for (ri in seq_along(targets)) {
    tgt    <- targets[ri]
    suffix <- if (multi) paste0("_", ri) else ""

    eq_ri  <- if (multi) eq$equations[[ri]] else eq
    groups <- eq_ri$groups

    # est_<target>
    actual <- export_df[[tgt]]
    if (purpose == "appraisal" && nrow(export_df) >= 1L) {
      actual[1L] <- NA_real_
    }
    predicted <- if (multi) as.numeric(pred_mat[, ri]) else as.numeric(pred_mat)

    est_col <- paste0("est_", tgt)
    export_df[[est_col]] <- round(predicted, 1)

    # residual
    resid_col <- paste0("residual", suffix)
    export_df[[resid_col]] <- round(actual - predicted, 1)

    # CQA: % of comps with smaller signed residual / 10
    resid_vals <- export_df[[resid_col]]
    n_valid    <- sum(!is.na(resid_vals))
    cqa_col    <- paste0("cqa", suffix)
    cqa_vals   <- vapply(resid_vals, function(r) {
      if (is.na(r)) return(NA_real_)
      sum(resid_vals < r, na.rm = TRUE) / n_valid * 10
    }, numeric(1))
    export_df[[cqa_col]] <- round(cqa_vals, 2)

    # residual_sf and cqa_sf (requires living_area column)
    if (!is.null(la_col)) {
      la           <- export_df[[la_col]]
      resid_sf_col <- paste0("residual_sf", suffix)
      export_df[[resid_sf_col]] <- round(export_df[[resid_col]] / la, 1)

      resid_sf_vals <- export_df[[resid_sf_col]]
      n_valid_sf    <- sum(!is.na(resid_sf_vals))
      cqa_sf_col    <- paste0("cqa_sf", suffix)
      cqa_sf_vals   <- vapply(resid_sf_vals, function(r) {
        if (is.na(r)) return(NA_real_)
        sum(resid_sf_vals < r, na.rm = TRUE) / n_valid_sf * 10
      }, numeric(1))
      export_df[[cqa_sf_col]] <- round(cqa_sf_vals, 2)
    }

    # Per-g-function contributions + basis + calc_residual
    intercept_group <- NULL
    contrib_groups  <- list()
    for (grp in groups) {
      if (grp$degree == 0L) {
        intercept_group <- grp
      } else {
        contrib_groups <- c(contrib_groups, list(grp))
      }
    }

    basis_val <- if (!is.null(intercept_group)) {
      intercept_group$terms[[1]]$coefficient
    } else {
      0
    }

    contrib_total <- rep(basis_val, nrow(export_df))
    for (grp in contrib_groups) {
      col_label <- gsub(" ", "_", grp$label)
      col_name  <- paste0(col_label, "_contribution", suffix)
      contrib   <- eval_g_function_(model, grp, pred_df,
                                    response_idx = if (multi) ri else NULL)
      export_df[[col_name]] <- round(contrib, 1)
      contrib_total <- contrib_total + contrib
    }

    export_df[[paste0("basis", suffix)]] <- round(basis_val, 1)

    calc_resid_col <- paste0("calc_residual", suffix)
    export_df[[calc_resid_col]] <- round(actual - contrib_total, 1)
  }

  # Sort by residual_sf (or residual) descending for appraisal/market
  if (purpose %in% c("appraisal", "market")) {
    has_subject <- (purpose == "appraisal") ||
                   (purpose == "market" && isTRUE(skip_subject_row))
    sort_col <- if ("residual_sf" %in% names(export_df)) "residual_sf" else "residual"
    if (sort_col %in% names(export_df)) {
      if (has_subject && nrow(export_df) >= 2L) {
        comps <- export_df[2:nrow(export_df), , drop = FALSE]
        comps <- comps[order(comps[[sort_col]], decreasing = TRUE, na.last = TRUE), , drop = FALSE]
        export_df <- rbind(export_df[1L, , drop = FALSE], comps)
      } else {
        export_df <- export_df[order(export_df[[sort_col]], decreasing = TRUE, na.last = TRUE), , drop = FALSE]
      }
    }
  }

  # Move ranking columns to the left: residual_sf, cqa_sf, residual, cqa
  rank_cols <- c("residual_sf", "cqa_sf", "residual", "cqa")
  rank_cols <- rank_cols[rank_cols %in% names(export_df)]
  if (length(rank_cols) > 0L) {
    other_cols <- setdiff(names(export_df), rank_cols)
    export_df  <- export_df[, c(rank_cols, other_cols), drop = FALSE]
  }

  export_df
}
