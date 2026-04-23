#' Compute RCA (Residual Comparable Adjustment) output for an appraisal
#'
#' Starting from the raw data (subject in row 1, comps in rows 2+) and a
#' fitted earth model, produces the adjusted comparables data frame used by
#' the Sales Comparison Grid. The user-supplied subject CQA score is
#' converted into a subject residual via linear interpolation of the comp
#' CQA/residual curve; per-g-function contributions and adjustments are
#' then added for each comp, along with net/gross adjustments, percentages,
#' and the final `adjusted_sale_price`.
#'
#' For multi-target models, the primary target drives the subject residual
#' calculation. Additional targets receive their own residual interpolation
#' and adjustment columns, imputed only for zero-weight rows.
#'
#' This is the non-Shiny computation kernel used by the earthUI Shiny app's
#' RCA Raw Output button, and is also suitable for use from batch scripts.
#'
#' @param data A data frame (subject + comps) matching `result$data` in
#'   schema. Row 1 is the subject.
#' @param result A fit result list as returned by [fit_earth()].
#' @param user_cqa Numeric scalar in `[0, 9.99]` — the subject CQA score.
#' @param cqa_type Character: `"cqa"` (default) uses total-residual CQA;
#'   `"cqa_sf"` uses per-SF CQA (requires `living_area_col`).
#' @param living_area_col Character scalar or `NULL`. If supplied and
#'   present in `data`, `residual_sf` and `cqa_sf` columns are added and
#'   `cqa_type = "cqa_sf"` is supported.
#' @param weight_col Character scalar or `NULL`. If supplied and present in
#'   `data`, rows with `weight == 0` are treated as additional "subject-like"
#'   rows that receive imputed values instead of real comp values.
#'
#' @return An enhanced data frame with model columns, contributions,
#'   adjustments, `subject_value`, `subject_cqa`, and
#'   `adjusted_sale_price`.
#'
#' @export
compute_rca_adjustments <- function(data,
                                    result,
                                    user_cqa,
                                    cqa_type        = c("cqa", "cqa_sf"),
                                    living_area_col = NULL,
                                    weight_col      = NULL) {
  if (!is.data.frame(data)) {
    stop("`data` must be a data frame.", call. = FALSE)
  }
  if (is.null(result)) {
    stop("`result` must be a fit_earth() result.", call. = FALSE)
  }
  if (nrow(data) < 2L) {
    stop("`data` must have at least 2 rows (subject + 1 comp).", call. = FALSE)
  }
  cqa_type <- match.arg(cqa_type)
  user_cqa <- as.numeric(user_cqa)
  if (is.na(user_cqa) || user_cqa < 0 || user_cqa > 9.99) {
    stop("CQA score must be a number between 0.00 and 9.99.", call. = FALSE)
  }

  model   <- result$model
  targets <- result$target
  tgt     <- targets[1L]
  eq      <- format_model_equation(result)
  multi   <- length(targets) > 1L
  eq_ri   <- if (multi) eq$equations[[1L]] else eq
  groups  <- eq_ri$groups
  ri      <- 1L

  la_col <- if (!is.null(living_area_col) && living_area_col %in% names(data)) {
    living_area_col
  } else {
    NULL
  }
  use_sf <- (cqa_type == "cqa_sf" && !is.null(la_col))

  export_df <- data

  # Align factor levels with training data
  train_df <- result$data
  pred_df  <- export_df
  for (col in names(train_df)) {
    if (is.factor(train_df[[col]]) && col %in% names(pred_df)) {
      pred_df[[col]] <- factor(pred_df[[col]], levels = levels(train_df[[col]]))
    }
  }

  # Predict on all rows
  pred_mat  <- stats::predict(model, newdata = pred_df)
  predicted <- if (multi) as.numeric(pred_mat[, ri]) else as.numeric(pred_mat)
  export_df[[paste0("est_", tgt)]] <- round(predicted, 1)

  # Actual sale prices (subject = NA)
  actual <- export_df[[tgt]]
  actual[1L] <- NA_real_

  residuals_val <- actual - predicted
  export_df[["residual"]] <- round(residuals_val, 1)

  if (!is.null(la_col)) {
    la <- export_df[[la_col]]
    export_df[["residual_sf"]] <- round(residuals_val / la, 1)
  }

  # CQA (comps only)
  comp_resid <- residuals_val[-1L]
  n_comps    <- sum(!is.na(comp_resid))
  cqa_all    <- vapply(residuals_val, function(r) {
    if (is.na(r)) return(NA_real_)
    sum(comp_resid < r, na.rm = TRUE) / n_comps * 10
  }, numeric(1))
  export_df[["cqa"]] <- round(cqa_all, 2)

  if (!is.null(la_col)) {
    resid_sf_vals <- export_df[["residual_sf"]]
    comp_resid_sf <- resid_sf_vals[-1L]
    n_comps_sf    <- sum(!is.na(comp_resid_sf))
    cqa_sf_all    <- vapply(resid_sf_vals, function(r) {
      if (is.na(r)) return(NA_real_)
      sum(comp_resid_sf < r, na.rm = TRUE) / n_comps_sf * 10
    }, numeric(1))
    export_df[["cqa_sf"]] <- round(cqa_sf_all, 2)
  }

  # --- Step A: interpolate subject residual from user CQA ---
  if (use_sf) {
    comp_cqa_vals         <- export_df[["cqa_sf"]][-1L]
    comp_resid_for_interp <- export_df[["residual_sf"]][-1L]
  } else {
    comp_cqa_vals         <- export_df[["cqa"]][-1L]
    comp_resid_for_interp <- export_df[["residual"]][-1L]
  }
  valid        <- !is.na(comp_cqa_vals) & !is.na(comp_resid_for_interp)
  cqa_sorted   <- comp_cqa_vals[valid]
  resid_sorted <- comp_resid_for_interp[valid]
  ord <- order(cqa_sorted)
  cqa_sorted   <- cqa_sorted[ord]
  resid_sorted <- resid_sorted[ord]

  subject_resid <- stats::approx(cqa_sorted, resid_sorted,
                                 xout = user_cqa, rule = 2)$y

  if (use_sf) {
    subject_la          <- export_df[[la_col]][1L]
    subject_resid_total <- subject_resid * subject_la
  } else {
    subject_resid_total <- subject_resid
  }

  subject_est <- predicted[1L] + subject_resid_total
  actual[1L] <- subject_est
  residuals_val[1L] <- subject_resid_total
  export_df[["residual"]][1L]           <- round(subject_resid_total, 1)
  export_df[[paste0("est_", tgt)]][1L]  <- round(predicted[1L], 1)
  export_df[["subject_value"]]          <- NA_real_
  export_df[["subject_value"]][1L]      <- round(subject_est, 1)
  export_df[["subject_cqa"]]            <- NA_real_
  export_df[["subject_cqa"]][1L]        <- user_cqa

  if (use_sf && !is.null(la_col)) {
    export_df[["residual_sf"]][1L] <- round(subject_resid, 1)
  }

  # Zero-weight rows — impute actual from predicted + subject residual
  zero_wt <- integer(0)
  if (!is.null(weight_col) && weight_col %in% names(export_df)) {
    wvals   <- export_df[[weight_col]]
    zero_wt <- which(wvals == 0)
  }
  if (length(zero_wt) > 0L) {
    sv <- predicted[zero_wt] + subject_resid_total
    export_df[["subject_value"]][zero_wt] <- round(sv, 1)
    actual[zero_wt] <- sv
    residuals_val <- actual - predicted
    export_df[["residual"]][zero_wt] <- round(residuals_val[zero_wt], 1)
    if (!is.null(la_col)) {
      la <- export_df[[la_col]]
      export_df[["residual_sf"]][zero_wt] <-
        round(residuals_val[zero_wt] / la[zero_wt], 1)
    }
  }

  # --- Step B: per-g-function contributions and adjustments ---
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
  export_df[["basis"]] <- round(basis_val, 1)

  adj_sum   <- rep(0, nrow(export_df))
  gross_sum <- rep(0, nrow(export_df))

  for (grp in contrib_groups) {
    col_label   <- gsub(" ", "_", grp$label)
    contrib_col <- paste0(col_label, "_contribution")
    adj_col     <- paste0(col_label, "_adjustment")

    contrib <- eval_g_function_(model, grp, pred_df,
                                response_idx = if (multi) ri else NULL)
    export_df[[contrib_col]] <- round(contrib, 1)

    subject_contrib <- contrib[1L]
    adjustment      <- subject_contrib - contrib
    export_df[[adj_col]] <- round(adjustment, 1)

    adj_sum   <- adj_sum + adjustment
    gross_sum <- gross_sum + abs(adjustment)
  }

  resid_adj <- subject_resid_total - residuals_val
  export_df[["residual_adjustment"]] <- round(resid_adj, 1)
  adj_sum   <- adj_sum + resid_adj
  gross_sum <- gross_sum + abs(resid_adj)

  export_df[["net_adjustments"]]   <- round(adj_sum, 1)
  export_df[["gross_adjustments"]] <- round(gross_sum, 1)

  export_df[["residual_pct"]]  <- round(resid_adj / actual, 4)
  export_df[["net_adj_pct"]]   <- round(adj_sum / actual, 4)
  export_df[["gross_adj_pct"]] <- round(gross_sum / actual, 4)

  export_df[["adjusted_sale_price"]] <- round(actual + adj_sum, 1)

  # --- Additional targets (multi-target only) ---
  if (multi && length(zero_wt) > 0L) {
    for (ri2 in 2:length(targets)) {
      tgt2    <- targets[ri2]
      eq_ri2  <- eq$equations[[ri2]]
      groups2 <- eq_ri2$groups
      tp      <- tgt2

      predicted2 <- as.numeric(pred_mat[, ri2])
      export_df[[paste0("est_", tp)]] <- round(predicted2, 1)

      actual2 <- export_df[[tgt2]]
      actual2[1L] <- NA_real_
      resid2  <- actual2 - predicted2

      comp_resid2_valid <- resid2[-1L][!is.na(resid2[-1L])]
      n_comps2 <- length(comp_resid2_valid)
      cqa2 <- vapply(resid2, function(r) {
        if (is.na(r)) return(NA_real_)
        sum(comp_resid2_valid < r, na.rm = TRUE) / n_comps2 * 10
      }, numeric(1))

      if (use_sf) {
        resid2_sf          <- resid2 / la
        comp_cqa2          <- cqa2[-1L]
        comp_resid2_interp <- resid2_sf[-1L]
      } else {
        comp_cqa2          <- cqa2[-1L]
        comp_resid2_interp <- resid2[-1L]
      }
      valid2 <- !is.na(comp_cqa2) & !is.na(comp_resid2_interp)
      cqa2_sorted   <- comp_cqa2[valid2]
      resid2_sorted <- comp_resid2_interp[valid2]
      ord2          <- order(cqa2_sorted)
      cqa2_sorted   <- cqa2_sorted[ord2]
      resid2_sorted <- resid2_sorted[ord2]

      subj_resid2 <- stats::approx(cqa2_sorted, resid2_sorted,
                                   xout = user_cqa, rule = 2)$y
      subj_resid2_total <- if (use_sf) {
        subj_resid2 * export_df[[la_col]][1L]
      } else {
        subj_resid2
      }

      subj_est2 <- predicted2[1L] + subj_resid2_total
      sv_col    <- paste0("subject_", tp, "_value")
      export_df[[sv_col]]    <- NA_real_
      export_df[[sv_col]][1L] <- round(subj_est2, 1)

      sv2 <- predicted2[zero_wt] + subj_resid2_total
      export_df[[sv_col]][zero_wt] <- round(sv2, 1)
      actual2[1L]      <- subj_est2
      actual2[zero_wt] <- sv2
      resid2 <- actual2 - predicted2

      export_df[[paste0(tp, "_residual")]] <- round(resid2, 1)

      intercept2      <- NULL
      contrib_groups2 <- list()
      for (grp in groups2) {
        if (grp$degree == 0L) {
          intercept2 <- grp
        } else {
          contrib_groups2 <- c(contrib_groups2, list(grp))
        }
      }
      basis2 <- if (!is.null(intercept2)) intercept2$terms[[1]]$coefficient else 0
      export_df[[paste0(tp, "_basis")]] <- round(basis2, 1)

      adj_sum2   <- rep(0, nrow(export_df))
      gross_sum2 <- rep(0, nrow(export_df))

      for (grp in contrib_groups2) {
        col_label    <- gsub(" ", "_", grp$label)
        contrib_col2 <- paste0(tp, "_", col_label, "_contribution")
        adj_col2     <- paste0(tp, "_", col_label, "_adjustment")

        contrib2 <- eval_g_function_(model, grp, pred_df, response_idx = ri2)
        export_df[[contrib_col2]] <- round(contrib2, 1)

        subj_contrib2 <- contrib2[1L]
        adj2          <- subj_contrib2 - contrib2
        export_df[[adj_col2]] <- round(adj2, 1)

        adj_sum2   <- adj_sum2 + adj2
        gross_sum2 <- gross_sum2 + abs(adj2)
      }

      resid_adj2 <- subj_resid2_total - resid2
      export_df[[paste0(tp, "_residual_adjustment")]] <- round(resid_adj2, 1)
      adj_sum2   <- adj_sum2 + resid_adj2
      gross_sum2 <- gross_sum2 + abs(resid_adj2)

      export_df[[paste0(tp, "_net_adjustments")]]   <- round(adj_sum2, 1)
      export_df[[paste0(tp, "_gross_adjustments")]] <- round(gross_sum2, 1)

      export_df[[paste0(tp, "_residual_pct")]]  <- round(resid_adj2 / actual2, 4)
      export_df[[paste0(tp, "_net_adj_pct")]]   <- round(adj_sum2 / actual2, 4)
      export_df[[paste0(tp, "_gross_adj_pct")]] <- round(gross_sum2 / actual2, 4)

      export_df[[paste0("adjusted_", tp)]] <- round(actual2 + adj_sum2, 1)
    }
  }

  export_df
}
