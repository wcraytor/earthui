

#' Prepare Report Assets
#'
#' Pre-generates all plots and data for the glmnetUI model report.
#' Returns the path to a directory containing all assets. This
#' directory can be passed to [render_report_glmnet()] to avoid re-computing
#' anything during rendering.
#'
#' @param model A glmnet or cv.glmnet model object.
#' @param lambda Selected lambda value.
#' @param gamma Selected gamma value (for relaxed models), or NULL.
#' @param x_mat The model matrix used for fitting.
#' @param y_vec The response vector.
#' @param coef_df Data frame of coefficients from [build_coef_table()].
#' @param predictors Character vector of predictor names.
#' @param response Character name of the response variable.
#' @param data The full data frame (for correlation and contributions).
#' @param col_types Named character vector of column types.
#' @param purpose Character: "general", "appraisal", or "market".
#' @param alpha The alpha value used.
#' @param family The family used.
#' @param standardize Logical: was standardize used.
#' @param relaxed Logical: was relaxed lasso used.
#' @param lambda_method Character: "cv" or "manual".
#' @param data_file_name Character: original data file name.
#' @param earth_import Optional earthUI import object (parsed `.rds`
#'   result), or NULL when no earth model is associated with the report.
#' @param assets_dir Character path to write assets. If NULL, a
#'   temporary directory is created.
#' @return The path to the assets directory (invisibly).
#' @export
prepare_report_assets_glmnet <- function(model, lambda, gamma, x_mat, y_vec,
                                  coef_df, predictors, response, data,
                                  col_types = NULL, purpose = "general",
                                  alpha = 1, family = "gaussian",
                                  standardize = TRUE, relaxed = FALSE,
                                  lambda_method = "cv",
                                  data_file_name = "",
                                  earth_import = NULL,
                                  assets_dir = NULL) {
  message("[glmnetUI ASSETS] prepare_report_assets_glmnet() called")
  message("[glmnetUI ASSETS] model class: ", paste(class(model), collapse = ", "))
  message("[glmnetUI ASSETS] lambda=", lambda, " gamma=", gamma,
          " response=", response, " #predictors=", length(predictors))
  message("[glmnetUI ASSETS] x_mat dim: ", nrow(x_mat), "x", ncol(x_mat),
          " y_vec length: ", length(y_vec))
  message("[glmnetUI ASSETS] coef_df rows: ", nrow(coef_df))

  if (is.null(assets_dir)) {
    assets_dir <- tempfile("glmnetUI_assets_")
  }
  dir.create(assets_dir, recursive = TRUE, showWarnings = FALSE)
  assets_dir <- normalizePath(assets_dir)
  message("[glmnetUI ASSETS] assets_dir: ", assets_dir)
  plots_dir <- file.path(assets_dir, "plots")
  dir.create(plots_dir, showWarnings = FALSE)

  # --- Font setup ---
  font_fam <- glmnet_font_family_()
  if (requireNamespace("sysfonts", quietly = TRUE) &&
      requireNamespace("showtext", quietly = TRUE)) {
    if (!"Roboto Condensed" %in% sysfonts::font_families()) {
      tryCatch(
        sysfonts::font_add_google("Roboto Condensed", "Roboto Condensed"),
        error = function(e) NULL)
    }
    showtext::showtext_auto()
    ggplot2::theme_set(ggplot2::theme_minimal(base_size = 15, base_family = font_fam) + ggplot2::theme(axis.text = ggplot2::element_text(size = 15), axis.title = ggplot2::element_text(size = 16)))
  } else {
    ggplot2::theme_set(ggplot2::theme_minimal(base_family = "sans"))
  }

  # --- Align data to x_mat rows ---
  # x_mat may have fewer rows than data (complete cases, weight != 0, etc.)
  n_xmat <- nrow(x_mat)
  if (nrow(data) != n_xmat) {
    use_cols <- intersect(c(response, predictors), names(data))
    mask <- stats::complete.cases(data[, use_cols, drop = FALSE])
    if (sum(mask) > n_xmat) {
      # Also exclude weight=0 rows
      for (cn in names(data)) {
        if (cn == "weight" || (!is.null(col_types) &&
            !is.na(col_types[cn]) && col_types[cn] == "weight")) {
          wt <- as.numeric(data[[cn]])
          mask <- mask & (!is.na(wt) & wt != 0)
        }
      }
    }
    if (sum(mask) == n_xmat) {
      data_aligned <- data[mask, , drop = FALSE]
    } else {
      data_aligned <- data[seq_len(min(nrow(data), n_xmat)), , drop = FALSE]
    }
  } else {
    data_aligned <- data
  }
  message("[glmnetUI ASSETS] data rows=", nrow(data),
          " x_mat rows=", n_xmat, " aligned rows=", nrow(data_aligned))

  # --- Predictions and residuals ---
  pred_args <- list(model, newx = x_mat, s = lambda, type = "response")
  if (!is.null(gamma)) pred_args$gamma <- gamma
  preds <- as.numeric(do.call(stats::predict, pred_args))
  resids <- y_vec - preds

  # --- Summary statistics ---
  ss_res <- sum(resids^2)
  ss_tot <- sum((y_vec - mean(y_vec))^2)
  n <- length(y_vec)
  p <- sum(coef_df$Coefficient != 0) - 1L  # exclude intercept
  r_sq <- 1 - ss_res / ss_tot
  adj_r_sq <- 1 - (1 - r_sq) * (n - 1) / (n - p - 1)
  rmse <- sqrt(mean(resids^2))
  mae <- mean(abs(resids))

  # Generalized R^2 (deviance ratio from cv.glmnet)
  gen_r_sq <- r_sq
  if (inherits(model, "cv.glmnet") || inherits(model, "cv.relaxed")) {
    fit_obj <- if (inherits(model, "cv.glmnet")) model$glmnet.fit else model
    dev_ratio <- fit_obj$dev.ratio
    idx <- which.min(abs(fit_obj$lambda - lambda))
    if (length(idx) == 1L) gen_r_sq <- dev_ratio[idx]
  }

  # CV R^2
  cv_r_sq <- NA_real_
  if (inherits(model, "cv.glmnet") || inherits(model, "cv.relaxed")) {
    idx <- which.min(abs(model$lambda - lambda))
    if (length(idx) == 1L) {
      cv_mse <- model$cvm[idx]
      cv_r_sq <- 1 - cv_mse / stats::var(y_vec)
    }
  }

  # --- Equation ---
  eq <- if (!is.null(earth_import)) {
    format_glmnet_earth_equation_(model, lambda, gamma, response, earth_import)
  } else {
    format_glmnet_equation_(model, lambda, gamma, response)
  }

  # --- Variable importance ---
  coef_args <- list(model, s = lambda)
  if (!is.null(gamma)) coef_args$gamma <- gamma
  coef_sparse <- do.call(stats::coef, coef_args)
  coef_vec <- as.numeric(coef_sparse)[-1L]
  coef_names <- rownames(coef_sparse)[-1L]
  names(coef_vec) <- coef_names

  imp_df <- if (!is.null(earth_import)) {
    compute_importance_earth_(coef_vec, x_mat, earth_import)
  } else {
    compute_importance_(coef_vec, x_mat, predictors, col_types)
  }
  message("[glmnetUI ASSETS] importance computed: ", nrow(imp_df), " rows")

  # --- ANOVA ---
  anova_df <- compute_anova_(coef_vec, x_mat, y_vec, predictors, col_types)
  message("[glmnetUI ASSETS] ANOVA computed: ", nrow(anova_df), " rows")

  # --- Contribution data for plots ---
  contrib_info <- compute_contrib_for_report_(
    coef_vec, x_mat, predictors, data_aligned, col_types,
    earth_import = earth_import)

  # --- Save model for on-the-fly printing in template ---
  # Strip Call: from model before saving -- cv.relaxed stores the full
  # function body (200K+ chars) which blows up LaTeX bufsize.
  model_clean <- model
  model_clean$call <- quote(cv.glmnet(...))
  if (!is.null(model_clean$glmnet.fit))
    model_clean$glmnet.fit$call <- quote(glmnet(...))

  # --- Save all pre-computed data ---
  saveRDS(list(
    model          = model_clean,
    data_file_name = data_file_name,
    n_obs          = n,
    response       = response,
    predictors     = predictors,
    purpose        = purpose,
    alpha          = alpha,
    lambda         = lambda,
    lambda_method  = lambda_method,
    family         = family,
    standardize    = standardize,
    relaxed        = relaxed,
    n_nonzero      = sum(coef_df$Coefficient != 0),
    n_predictors   = p,
    r_squared      = r_sq,
    adj_r_squared  = adj_r_sq,
    gen_r_squared  = gen_r_sq,
    cv_r_squared   = cv_r_sq,
    rmse           = rmse,
    mae            = mae,
    equation_latex = eq$latex_pdf %||% eq$latex,
    equation_notation = eq$notation %||% "linear",
    coef_df        = coef_df,
    importance_df  = imp_df,
    anova_df       = anova_df,
    interaction_matrix = build_interaction_matrix__glmnet(coef_df, predictors),
    contrib_names  = names(contrib_info$contribs)
  ), file.path(assets_dir, "report_data.rds"))
  message("[glmnetUI ASSETS] report_data.rds saved to: ",
          file.path(assets_dir, "report_data.rds"))

  # --- Generate plots ---
  save_ggplot_ <- function(name, p, width = 8, height = 5) {
    for (ext in c("png", "pdf")) {
      path <- file.path(plots_dir, paste0(name, ".", ext))
      tryCatch(
        ggplot2::ggsave(path, plot = p, width = width, height = height,
                        dpi = 150),
        error = function(e) {
          grDevices::png(path, width = width, height = height,
                         units = "in", res = 150)
          graphics::plot.new()
          graphics::text(0.5, 0.5, paste("Error:", e$message), cex = 0.8)
          grDevices::dev.off()
        }
      )
    }
  }

  # Variable importance plot (landscape for many variables)
  if (nrow(imp_df) > 0) {
    n_vars <- nrow(imp_df)
    # Height scales with the number of bars; small enough that a single/few
    # bars render as bars rather than a panel-filling block.
    imp_height <- max(2, min(9, n_vars * 0.55 + 1))
    p_imp <- ggplot2::ggplot(
      imp_df,
      ggplot2::aes(x = stats::reorder(.data$Variable, .data$Importance),
                   y = .data$Importance)) +
      ggplot2::geom_col(fill = "#5e81ac", width = 0.7) +
      ggplot2::scale_x_discrete(expand = ggplot2::expansion(add = 0.6)) +
      ggplot2::coord_flip() +
      ggplot2::labs(x = NULL, y = "Importance (|coef| \u00d7 sd(x))",
                    title = "Variable Importance") +
      ggplot2::theme_minimal(base_size = 15, base_family = font_fam) + ggplot2::theme(axis.text = ggplot2::element_text(size = 15), axis.title = ggplot2::element_text(size = 16))
    save_ggplot_("importance", p_imp, width = 10, height = imp_height)
  }

  # Correlation matrix
  numeric_preds <- predictors[vapply(predictors, function(p) {
    p %in% names(data_aligned) && is.numeric(data_aligned[[p]])
  }, logical(1))]
  if (length(numeric_preds) >= 2) {
    cor_mat <- stats::cor(data_aligned[, numeric_preds, drop = FALSE],
                          use = "pairwise.complete.obs")
    cor_long <- as.data.frame(as.table(cor_mat))
    names(cor_long) <- c("Var1", "Var2", "value")
    p_cor <- ggplot2::ggplot(cor_long,
               ggplot2::aes(x = .data$Var1, y = .data$Var2,
                            fill = .data$value)) +
      ggplot2::geom_tile() +
      ggplot2::geom_text(ggplot2::aes(label = sprintf("%.2f", .data$value)),
                         size = 3) +
      ggplot2::scale_fill_gradient2(low = "#2166AC", mid = "white",
                                     high = "#B2182B", midpoint = 0) +
      ggplot2::labs(x = NULL, y = NULL, title = "Correlation Matrix") +
      ggplot2::theme_minimal(base_size = 15, base_family = font_fam) + ggplot2::theme(axis.text = ggplot2::element_text(size = 15), axis.title = ggplot2::element_text(size = 16)) +
      ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 45, hjust = 1))
    save_ggplot_("correlation", p_cor, width = 10, height = 8)
  }

  # Contribution plots -- match the rendering in mod_contributions.R:
  #   Numeric: scatter + slope/fit line
  #   Factor:  box plot by level
  #   Interaction: scatter colored by 2nd var + heatmap + static 3D persp
  fac_cols <- if (!is.null(col_types)) {
    names(col_types)[col_types %in% c("factor", "character")]
  } else {
    character(0)
  }

  for (vname in names(contrib_info$contribs)) {
    safe_name <- gsub("[^a-zA-Z0-9_]", "_", vname)
    cv <- contrib_info$contribs[[vname]]
    xv <- contrib_info$x_values[[vname]]
    is_interaction <- grepl(":", vname, fixed = TRUE)
    earth_fac <- if (!is.null(earth_import)) names(earth_import$model$xlevels) else character(0)
    is_factor <- vname %in% fac_cols ||
      vname %in% earth_fac ||
      (vname %in% names(data) &&
       (is.factor(data[[vname]]) || is.character(data[[vname]])))

    if (is_interaction) {
      # --- Interaction: scatter + heatmap + 3D persp ---
      int_vars <- strsplit(vname, ":", fixed = TRUE)[[1]]
      var1 <- int_vars[1]; var2 <- int_vars[2]
      if (var1 %in% names(data_aligned) && var2 %in% names(data_aligned) &&
          is.numeric(data_aligned[[var1]]) && is.numeric(data_aligned[[var2]])) {
        x1 <- data_aligned[[var1]]; x2 <- data_aligned[[var2]]
        if (length(x1) == length(cv)) {
          plot_df <- data.frame(x = x1, y = x2, contribution = cv)

          # Scatter colored by contribution
          p_scatter <- ggplot2::ggplot(plot_df,
            ggplot2::aes(x = .data$x, y = .data$y,
                         color = .data$contribution)) +
            ggplot2::geom_point(alpha = 0.8, size = 2.5) +
            ggplot2::scale_color_gradient2(
              low = "#2166AC", mid = "#d0d0d0", high = "#B2182B",
              midpoint = 0, name = "Contrib.",
              labels = glmnet_axis_labels_) +
            ggplot2::labs(title = "Scatter", x = var1, y = var2) +
            ggplot2::scale_x_continuous(labels = glmnet_axis_labels_) +
            ggplot2::scale_y_continuous(labels = glmnet_axis_labels_) +
            glmnet_diag_theme_(font_fam)
          save_ggplot_(paste0("contrib_", safe_name, "_scatter"), p_scatter)

          # Heatmap
          p_heat <- ggplot2::ggplot(plot_df,
            ggplot2::aes(x = .data$x, y = .data$y,
                         z = .data$contribution)) +
            ggplot2::stat_summary_2d(bins = 20, fun = mean) +
            ggplot2::scale_fill_gradient2(
              low = "#2166AC", mid = "#d0d0d0", high = "#B2182B",
              midpoint = 0, name = "Mean\nContrib.",
              labels = glmnet_axis_labels_) +
            ggplot2::labs(title = "Heatmap", x = var1, y = var2) +
            ggplot2::scale_x_continuous(labels = glmnet_axis_labels_) +
            ggplot2::scale_y_continuous(labels = glmnet_axis_labels_) +
            glmnet_diag_theme_(font_fam) +
            ggplot2::theme(
              legend.key.height = ggplot2::unit(1.5, "cm"),
              legend.key.width = ggplot2::unit(0.6, "cm"),
              legend.title = ggplot2::element_text(size = 13),
              legend.text = ggplot2::element_text(size = 12)
            )
          save_ggplot_(paste0("contrib_", safe_name, "_heatmap"), p_heat)

          # Static 3D persp (snapshot of interactive surface)
          tryCatch({
            x1_seq <- seq(min(x1, na.rm = TRUE), max(x1, na.rm = TRUE),
                          length.out = 30)
            x2_seq <- seq(min(x2, na.rm = TRUE), max(x2, na.rm = TRUE),
                          length.out = 30)
            surf_df <- data.frame(x1 = x1, x2 = x2, z = cv)
            surf_fit <- stats::loess(z ~ x1 * x2, data = surf_df, span = 0.75)
            grid_df <- expand.grid(x1 = x1_seq, x2 = x2_seq)
            z_mat <- matrix(stats::predict(surf_fit, grid_df),
                            nrow = length(x1_seq))

            col_pal <- grDevices::colorRampPalette(
              c("#2166AC", "white", "#B2182B"))(50)
            # persp() needs ONE colour per facet ((nr-1) x (nc-1)), not one per
            # grid vertex (nr x nc). Colour each facet by the mean height of its
            # four corners. (The old code reshaped nr*nc vertex colours into an
            # (nr-1) x (nc-1) matrix, which recycled -> the "data length not a
            # multiple" warning and mis-aligned the surface colours.)
            nr <- nrow(z_mat); nc <- ncol(z_mat)
            zfacet <- (z_mat[-1, -1] + z_mat[-1, -nc] +
                       z_mat[-nr, -1] + z_mat[-nr, -nc]) / 4
            z_range <- range(zfacet, na.rm = TRUE)
            z_scaled <- (zfacet - z_range[1]) /
              max(z_range[2] - z_range[1], 1e-10)
            facet_col <- matrix(
              col_pal[pmax(1L, pmin(50L, as.integer(z_scaled * 49) + 1L))],
              nrow = nr - 1, ncol = nc - 1)

            for (ext in c("png", "pdf")) {
              path <- file.path(plots_dir,
                paste0("contrib_", safe_name, "_3d.", ext))
              if (ext == "png") {
                grDevices::png(path, width = 8, height = 6,
                               units = "in", res = 150)
              } else {
                grDevices::pdf(path, width = 8, height = 6)
              }
              old_par <- graphics::par(mar = c(2, 2, 3, 1))
              old_scipen <- options(scipen = 999)
              graphics::persp(x1_seq, x2_seq, z_mat,
                theta = 30, phi = 25, expand = 0.6,
                col = facet_col, border = NA, shade = 0.3,
                ticktype = "detailed",
                xlab = var1, ylab = var2, zlab = "Contribution",
                main = "3D Surface")
              options(old_scipen)
              graphics::par(old_par)
              grDevices::dev.off()
            }
          }, error = function(e) {
            message("[glmnetUI ASSETS] 3D persp error for ", vname, ": ",
                    e$message)
          })
        }
      }
    } else if (is_factor && vname %in% names(data_aligned)) {
      # --- Factor: box plot by level, sorted by median contribution ---
      fac_vals <- as.factor(data_aligned[[vname]])
      if (length(fac_vals) == length(cv)) {
        plot_df <- data.frame(level = fac_vals, contribution = cv)
        # Sort levels by median contribution
        med_order <- stats::reorder(plot_df$level, plot_df$contribution,
                                     FUN = stats::median)
        plot_df$level <- med_order
        p_c <- ggplot2::ggplot(plot_df,
          ggplot2::aes(x = .data$level, y = .data$contribution)) +
          ggplot2::geom_boxplot(fill = "#5e81ac", alpha = 0.6) +
          ggplot2::labs(x = vname, y = "Contribution") +
          ggplot2::scale_y_continuous(labels = glmnet_axis_labels_) +
          glmnet_diag_theme_(font_fam) +
          ggplot2::theme(axis.text.x = ggplot2::element_text(
            angle = 45, hjust = 1))
        save_ggplot_(paste0("contrib_", safe_name), p_c)
      }
    } else if (!is.null(xv) && length(xv) == length(cv)) {
      # --- Numeric: scatter with fitted trend and legend ---
      plot_df <- data.frame(x = xv, contribution = cv)
      p_c <- ggplot2::ggplot(plot_df,
        ggplot2::aes(x = .data$x, y = .data$contribution)) +
        ggplot2::geom_point(ggplot2::aes(color = "Observations"),
                            alpha = 0.35, size = 1.4) +
        ggplot2::geom_smooth(method = "lm", formula = y ~ x,
                              ggplot2::aes(color = "Fitted trend"),
                              linewidth = 1.1, se = FALSE) +
        ggplot2::scale_color_manual(
          name = NULL,
          breaks = c("Observations", "Fitted trend"),
          values = c("Observations" = "#5e81ac",
                     "Fitted trend" = "#bf616a"),
          guide = ggplot2::guide_legend(
            override.aes = list(
              linetype  = c("blank", "solid"),
              shape     = c(16, NA),
              linewidth = c(NA, 1.1),
              alpha     = c(0.9, 1)
            )
          )
        ) +
        ggplot2::labs(x = vname, y = "Contribution") +
        ggplot2::scale_x_continuous(labels = glmnet_axis_labels_) +
        ggplot2::scale_y_continuous(labels = glmnet_axis_labels_) +
        glmnet_diag_theme_(font_fam) +
        # Legend at the bottom (so it can't be clipped by the right edge) and a
        # right margin so the y-axis title/values have room.
        ggplot2::theme(legend.position = "bottom",
                       plot.margin = ggplot2::margin(8, 16, 8, 10))
      save_ggplot_(paste0("contrib_", safe_name), p_c)
    }
  }

  # Diagnostic plots
  fit_obj <- if (inherits(model, "cv.glmnet") ||
                   inherits(model, "cv.relaxed")) {
    model$glmnet.fit
  } else if (inherits(model, "relaxed")) {
    model$relaxed
  } else {
    model
  }

  # Coefficient path
  beta_mat <- as.matrix(fit_obj$beta)
  log_lambda <- log(fit_obj$lambda)
  df_long <- data.frame(
    log_lambda = rep(log_lambda, each = nrow(beta_mat)),
    coefficient = as.numeric(beta_mat),
    variable = rep(rownames(beta_mat), times = ncol(beta_mat)),
    stringsAsFactors = FALSE
  )
  p_coef <- ggplot2::ggplot(df_long,
              ggplot2::aes(x = .data$log_lambda,
                           y = .data$coefficient,
                           color = .data$variable)) +
    ggplot2::geom_line(show.legend = FALSE) +
    ggplot2::labs(x = "Log(Lambda)", y = "Coefficient",
                  title = "Coefficient Path") +
    ggplot2::theme_minimal(base_size = 15, base_family = font_fam) + ggplot2::theme(axis.text = ggplot2::element_text(size = 15), axis.title = ggplot2::element_text(size = 16))
  save_ggplot_("coef_path", p_coef)

  # CV error plot
  if (inherits(model, "cv.glmnet") || inherits(model, "cv.relaxed")) {
    df_cv <- data.frame(
      log_lambda = log(model$lambda),
      cvm = model$cvm, cvup = model$cvup, cvlo = model$cvlo,
      stringsAsFactors = FALSE)
    p_cv <- ggplot2::ggplot(df_cv, ggplot2::aes(x = .data$log_lambda,
                                                  y = .data$cvm)) +
      ggplot2::geom_point(color = "#bf616a", size = 1.5) +
      ggplot2::geom_errorbar(ggplot2::aes(ymin = .data$cvlo,
                                           ymax = .data$cvup), alpha = 0.4) +
      ggplot2::geom_vline(xintercept = log(model$lambda.min),
                           linetype = "dashed", color = "#5e81ac") +
      ggplot2::geom_vline(xintercept = log(model$lambda.1se),
                           linetype = "dashed", color = "#a3be8c") +
      ggplot2::labs(x = "Log(Lambda)", y = "CV Error",
                    title = "Cross-Validation Error") +
      ggplot2::theme_minimal(base_size = 15, base_family = font_fam) + ggplot2::theme(axis.text = ggplot2::element_text(size = 15), axis.title = ggplot2::element_text(size = 16))
    save_ggplot_("cv_error", p_cv)
  }

  # Actual vs Predicted
  df_avp <- data.frame(actual = y_vec, predicted = preds)
  p_avp <- ggplot2::ggplot(df_avp,
             ggplot2::aes(x = .data$actual, y = .data$predicted)) +
    ggplot2::geom_point(alpha = 0.5) +
    ggplot2::geom_abline(slope = 1, intercept = 0,
                          color = "#bf616a", linetype = "dashed") +
    ggplot2::labs(x = "Actual", y = "Predicted",
                  title = "Actual vs Predicted") +
    ggplot2::theme_minimal(base_size = 15, base_family = font_fam) + ggplot2::theme(axis.text = ggplot2::element_text(size = 15), axis.title = ggplot2::element_text(size = 16))
  save_ggplot_("actual_vs_predicted", p_avp)

  # Residuals vs Fitted
  df_resid <- data.frame(fitted = preds, residuals = resids)
  p_resid <- ggplot2::ggplot(df_resid,
               ggplot2::aes(x = .data$fitted, y = .data$residuals)) +
    ggplot2::geom_point(alpha = 0.5) +
    ggplot2::geom_hline(yintercept = 0, color = "#bf616a",
                         linetype = "dashed") +
    ggplot2::labs(x = "Fitted Values", y = "Residuals",
                  title = "Residuals vs Fitted") +
    ggplot2::theme_minimal(base_size = 15, base_family = font_fam) + ggplot2::theme(axis.text = ggplot2::element_text(size = 15), axis.title = ggplot2::element_text(size = 16))
  save_ggplot_("residuals_vs_fitted", p_resid)

  # Q-Q plot
  qq_data <- stats::qqnorm(resids, plot.it = FALSE)
  df_qq <- data.frame(theoretical = qq_data$x, sample = qq_data$y)
  p_qq <- ggplot2::ggplot(df_qq,
            ggplot2::aes(x = .data$theoretical, y = .data$sample)) +
    ggplot2::geom_point(alpha = 0.5) +
    ggplot2::geom_abline(slope = stats::sd(resids),
                          intercept = mean(resids),
                          color = "#bf616a", linetype = "dashed") +
    ggplot2::labs(x = "Theoretical Quantiles", y = "Sample Quantiles",
                  title = "Normal Q-Q Plot") +
    ggplot2::theme_minimal(base_size = 15, base_family = font_fam) + ggplot2::theme(axis.text = ggplot2::element_text(size = 15), axis.title = ggplot2::element_text(size = 16))
  save_ggplot_("qq", p_qq)

  all_plots <- list.files(plots_dir, full.names = FALSE)
  message("[glmnetUI ASSETS] All plots generated (", length(all_plots), "): ",
          paste(all_plots, collapse = ", "))
  message("[glmnetUI ASSETS] prepare_report_assets_glmnet() DONE. assets_dir=",
          assets_dir)
  invisible(assets_dir)
}


#' Render glmnetUI Report
#'
#' Renders a model report in the requested format using pre-generated
#' assets from [prepare_report_assets_glmnet()].  Tries Quarto first; if Quarto
#' is unavailable or fails, falls back to rmarkdown with the bundled
#' \code{report_template.Rmd}.
#'
#' @param output_format Character: "html", "pdf", or "docx".
#' @param output_file Character path for the output file.
#' @param assets_dir Character path to pre-generated assets from
#'   [prepare_report_assets_glmnet()].
#' @return The output file path (invisibly).
#' @export
render_report_glmnet <- function(output_format = "html", output_file,
                          assets_dir) {
  message("[glmnetUI REPORT] render_report_glmnet() called: format=", output_format,
          " output_file=", output_file, " assets_dir=", assets_dir)

  # --- Try Quarto first, then fall back to rmarkdown ---
  quarto_ok <- requireNamespace("quarto", quietly = TRUE)
  message("[glmnetUI REPORT] quarto package available: ", quarto_ok)

  rendered_ok <- FALSE

  if (quarto_ok) {
    rendered_ok <- tryCatch({
      render_report_quarto_(output_format, output_file, assets_dir)
      TRUE
    }, error = function(e) {
      message("[glmnetUI REPORT] Quarto rendering FAILED: ", e$message)
      FALSE
    })
  }

  if (!rendered_ok) {
    message("[glmnetUI REPORT] Falling back to rmarkdown rendering...")
    render_report_rmarkdown_(output_format, output_file, assets_dir)
  }

  if (!file.exists(output_file)) {
    stop("Report file was not created at: ", output_file, call. = FALSE)
  }
  message("[glmnetUI REPORT] Report successfully created: ", output_file,
          " (", file.size(output_file), " bytes)")
  invisible(output_file)
}


#' Render report via Quarto
#' @noRd
render_report_quarto_ <- function(output_format, output_file, assets_dir) {
  template <- system.file("quarto", "glmnet_report.qmd",
                           package = "earthUI")
  message("[glmnetUI REPORT] Quarto template path: '", template, "'")
  if (!nzchar(template)) {
    stop("Quarto report template not found in package.", call. = FALSE)
  }

  # Create temp rendering directory
  render_dir <- tempfile("glmnetUI_render_")
  dir.create(render_dir, recursive = TRUE)
  on.exit(unlink(render_dir, recursive = TRUE), add = TRUE)

  # Copy template and assets
  qmd_dst <- file.path(render_dir, "glmnet_report.qmd")
  file.copy(template, qmd_dst)
  message("[glmnetUI REPORT] QMD copied: ", file.exists(qmd_dst))

  rds_src <- file.path(assets_dir, "report_data.rds")
  rds_dst <- file.path(render_dir, "report_data.rds")
  file.copy(rds_src, rds_dst)
  message("[glmnetUI REPORT] RDS copied: ", file.exists(rds_dst),
          " (source existed: ", file.exists(rds_src), ")")

  plots_src <- file.path(assets_dir, "plots")
  if (dir.exists(plots_src)) {
    plots_dst <- file.path(render_dir, "plots")
    dir.create(plots_dst, showWarnings = FALSE)
    plot_files <- list.files(plots_src, full.names = TRUE)
    file.copy(plot_files, plots_dst)
    message("[glmnetUI REPORT] Copied ", length(plot_files), " plot files")
  } else {
    message("[glmnetUI REPORT] WARNING: plots_src does not exist: ", plots_src)
  }

  qmd_format <- switch(output_format,
    html = "html", pdf = "pdf", docx = "docx", "html")
  message("[glmnetUI REPORT] Quarto format: ", qmd_format)

  # Render
  old_env <- Sys.getenv("QUARTO_R")
  Sys.setenv(QUARTO_R = file.path(R.home("bin"), "R"))
  on.exit(Sys.setenv(QUARTO_R = old_env), add = TRUE)

  message("[glmnetUI REPORT] Calling quarto::quarto_render()...")
  quarto::quarto_render(
    input = file.path(render_dir, "glmnet_report.qmd"),
    output_format = qmd_format,
    execute_params = list(
      data_file = file.path(render_dir, "report_data.rds")
    ),
    # quiet = TRUE: avoid the `quarto` package echoing CLI output through cli,
    # where a `{captions}` in PDF metadata is mis-read as a glue expression
    # ("object 'captions' not found").
    quiet = TRUE
  )

  # Find output file
  out_ext <- switch(output_format, html = ".html", pdf = ".pdf",
                    docx = ".docx", ".html")
  rendered <- file.path(render_dir, paste0("glmnet_report", out_ext))
  message("[glmnetUI REPORT] Expected rendered file: ", rendered,
          " exists: ", file.exists(rendered))

  if (!file.exists(rendered)) {
    # List what's actually in the render dir for debugging
    all_files <- list.files(render_dir, recursive = TRUE)
    message("[glmnetUI REPORT] Files in render_dir: ",
            paste(all_files, collapse = ", "))
    stop("Quarto rendered but output file not found: ", rendered,
         call. = FALSE)
  }

  file.copy(rendered, output_file, overwrite = TRUE)
  message("[glmnetUI REPORT] Quarto output copied to: ", output_file)
}


#' Render report via rmarkdown (fallback)
#' @noRd
render_report_rmarkdown_ <- function(output_format, output_file, assets_dir) {
  message("[glmnetUI REPORT] rmarkdown fallback: format=", output_format)

  rmd_template <- system.file("app_glmnet", "report_template.Rmd",
                               package = "earthUI")
  message("[glmnetUI REPORT] Rmd template path: '", rmd_template, "'")
  if (!nzchar(rmd_template)) {
    stop("Rmd report template not found in package.", call. = FALSE)
  }

  rmd_copy <- file.path(tempdir(), "glmnetUI_report.Rmd")
  file.copy(rmd_template, rmd_copy, overwrite = TRUE)

  # Load the pre-computed assets to pass as params
  data_file <- file.path(assets_dir, "report_data.rds")
  message("[glmnetUI REPORT] Assets RDS: ", data_file,
          " exists: ", file.exists(data_file))

  rmd_out_format <- if (output_format == "html") {
    rmarkdown::html_document(self_contained = TRUE)
  } else if (output_format == "docx") {
    rmarkdown::word_document()
  } else {
    rmarkdown::pdf_document(latex_engine = "xelatex")
  }

  message("[glmnetUI REPORT] Calling rmarkdown::render()...")
  rmarkdown::render(
    rmd_copy,
    output_format = rmd_out_format,
    output_file = output_file,
    params = list(
      appraiser_name   = "",
      property_address = "",
      report_date      = as.character(Sys.Date()),
      file_number      = "",
      lambda           = 0,
      lambda_min       = NA,
      lambda_1se       = NA,
      coef_df          = NULL,
      plot_dir         = assets_dir
    ),
    envir = new.env(parent = globalenv()),
    # quiet = TRUE: avoid the `quarto` package echoing CLI output through cli,
    # where a `{captions}` in PDF metadata is mis-read as a glue expression
    # ("object 'captions' not found").
    quiet = TRUE
  )
  message("[glmnetUI REPORT] rmarkdown::render() completed, output exists: ",
          file.exists(output_file))
}


# --- Internal helpers for report asset generation ---

#' Compute variable importance for report
#' @noRd
compute_importance_ <- function(coef_vec, x_mat, predictors, col_types) {
  imp <- data.frame(
    Variable = character(0),
    Importance = numeric(0),
    Relative = numeric(0),
    stringsAsFactors = FALSE
  )

  col_names <- colnames(x_mat)
  for (pred in predictors) {
    # Find columns belonging to this predictor
    idx <- which(col_names == pred |
                   startsWith(col_names, paste0(pred)))
    # Filter to avoid matching longer predictor names
    idx <- Filter(function(j) {
      cn <- col_names[j]
      if (cn == pred) return(TRUE)
      suffix <- sub(paste0("^", pred), "", cn)
      !any(startsWith(cn, paste0(setdiff(predictors, pred))))
    }, idx)

    if (length(idx) == 0) next
    # Importance = sum of |coef| * sd(x) for each column
    total_imp <- 0
    for (j in idx) {
      b <- coef_vec[col_names[j]]
      if (!is.na(b) && b != 0) {
        total_imp <- total_imp + abs(b) * stats::sd(x_mat[, j])
      }
    }
    if (total_imp > 0) {
      imp <- rbind(imp, data.frame(
        Variable = pred, Importance = total_imp, Relative = 0,
        stringsAsFactors = FALSE
      ))
    }
  }

  if (nrow(imp) > 0) {
    max_imp <- max(imp$Importance)
    if (max_imp > 0) imp$Relative <- round(imp$Importance / max_imp * 100, 1)
    imp <- imp[order(-imp$Importance), ]
    rownames(imp) <- NULL
  }
  imp
}

# Variable importance for an EARTH-imported model. The glmnet model is fit on
# the earth basis, whose column names (e.g. h(living_sqft-3335)) don't match the
# original predictor names, so the plain compute_importance_() only ever credits
# factor dummies. Here we attribute |coef| * sd(basis_col) to each g-function
# group (variable / interaction set), the same grouping used by the equation.
compute_importance_earth_ <- function(coef_vec, x_mat, earth_import) {
  groups <- tryCatch(build_g_groups_(earth_import), error = function(e) NULL)
  bx <- tryCatch(build_earth_basis(NULL, earth_import), error = function(e) NULL)
  bx_colnames <- if (!is.null(bx)) colnames(bx) else colnames(x_mat)
  if (is.null(groups) || length(bx_colnames) == 0L) {
    return(data.frame(Variable = character(0), Importance = numeric(0),
                      Relative = numeric(0), stringsAsFactors = FALSE))
  }
  xcols <- colnames(x_mat)
  rows <- list()
  for (grp in groups) {
    if (isTRUE(grp$degree == 0L)) next  # intercept
    total_imp <- 0
    for (term in grp$terms) {
      pos <- term$index - 1L            # basis columns exclude the intercept
      if (pos < 1L || pos > length(bx_colnames)) next
      cn <- bx_colnames[pos]
      if (!cn %in% xcols) next
      b <- coef_vec[cn]
      if (!is.na(b) && b != 0) total_imp <- total_imp + abs(b) * stats::sd(x_mat[, cn])
    }
    if (total_imp > 0) {
      lbl <- if (isTRUE(grp$degree > 1L)) {
        paste(grp$base_vars, collapse = " \u00d7 ")
      } else grp$label
      rows[[length(rows) + 1L]] <- data.frame(
        Variable = lbl, Importance = total_imp, Relative = 0,
        stringsAsFactors = FALSE)
    }
  }
  imp <- if (length(rows)) do.call(rbind, rows) else
    data.frame(Variable = character(0), Importance = numeric(0),
               Relative = numeric(0), stringsAsFactors = FALSE)
  if (nrow(imp) > 0) {
    mx <- max(imp$Importance)
    if (mx > 0) imp$Relative <- round(imp$Importance / mx * 100, 1)
    imp <- imp[order(-imp$Importance), ]
    rownames(imp) <- NULL
  }
  imp
}


#' Compute ANOVA decomposition for report
#' @noRd
compute_anova_ <- function(coef_vec, x_mat, y_vec, predictors, col_types) {
  col_names <- colnames(x_mat)
  n <- length(y_vec)
  intercept <- 0  # glmnet handles intercept separately

  rows <- list()
  for (pred in predictors) {
    idx <- which(col_names == pred |
                   startsWith(col_names, paste0(pred)))
    idx <- Filter(function(j) {
      cn <- col_names[j]
      cn == pred || !any(startsWith(cn, paste0(setdiff(predictors, pred))))
    }, idx)

    if (length(idx) == 0) next
    contrib <- rep(0, n)
    for (j in idx) {
      b <- coef_vec[col_names[j]]
      if (!is.na(b) && b != 0) {
        contrib <- contrib + b * x_mat[, j]
      }
    }
    ss <- sum(contrib^2)
    rows[[pred]] <- data.frame(
      Variable = pred,
      `Sum of Squares` = round(ss, 2),
      `% of Total` = 0,
      stringsAsFactors = FALSE,
      check.names = FALSE
    )
  }

  if (length(rows) == 0) {
    return(data.frame(Variable = character(0),
                      `Sum of Squares` = numeric(0),
                      `% of Total` = numeric(0),
                      stringsAsFactors = FALSE,
                      check.names = FALSE))
  }

  anova_df <- do.call(rbind, rows)
  total_ss <- sum(anova_df$`Sum of Squares`)
  if (total_ss > 0) {
    anova_df$`% of Total` <- round(anova_df$`Sum of Squares` / total_ss * 100, 1)
  }
  rownames(anova_df) <- NULL
  anova_df
}


#' Compute contribution data for report plots
#' @noRd
compute_contrib_for_report_ <- function(coef_vec, x_mat, predictors,
                                         data, col_types,
                                         earth_import = NULL) {
  col_names <- colnames(x_mat)
  contribs <- list()
  x_values <- list()

  # Earth path: use earth_col_to_pred_ for column-to-parent mapping
  if (!is.null(earth_import)) {
    earth_preds <- earth_import$predictors
    col_to_pred <- earth_col_to_pred_(col_names, earth_preds)
    beta_pos <- as.numeric(coef_vec)
    unique_parents <- unique(col_to_pred[nzchar(col_to_pred)])
    earth_fac <- names(earth_import$model$xlevels)
    for (parent in unique_parents) {
      idx <- which(col_to_pred == parent)
      # Skip groups where all coefficients are zero
      if (all(beta_pos[idx] == 0)) next
      cv <- rep(0, nrow(x_mat))
      for (j in idx) cv <- cv + beta_pos[j] * x_mat[, j]
      contribs[[parent]] <- cv
      # X-axis: use original data for non-factor single predictors
      base_var <- strsplit(parent, ":", fixed = TRUE)[[1]][1]
      if (!grepl(":", parent, fixed = TRUE) &&
          base_var %in% names(data) &&
          !base_var %in% earth_fac &&
          is.numeric(data[[base_var]]) &&
          length(data[[base_var]]) == nrow(x_mat)) {
        x_values[[parent]] <- data[[base_var]]
      }
    }
    return(list(contribs = contribs, x_values = x_values))
  }

  # Standard formula path: map each model matrix column to its parent predictor
  preds_sorted <- predictors[order(-nchar(predictors))]
  col_to_pred <- vapply(col_names, function(cn) {
    # Interaction columns contain ":"
    if (grepl(":", cn, fixed = TRUE)) return("")
    for (p in preds_sorted) {
      if (cn == p || startsWith(cn, p)) return(p)
    }
    ""
  }, character(1), USE.NAMES = FALSE)

  # Single predictors
  for (pred in predictors) {
    idx <- which(col_to_pred == pred)
    if (length(idx) == 0) next
    contrib_vec <- rep(0, nrow(x_mat))
    has_nonzero <- FALSE
    for (j in idx) {
      b <- coef_vec[col_names[j]]
      if (!is.na(b) && b != 0) {
        contrib_vec <- contrib_vec + b * x_mat[, j]
        has_nonzero <- TRUE
      }
    }
    if (!has_nonzero) next
    contribs[[pred]] <- contrib_vec

    # X-axis values for scatter plot
    if (pred %in% names(data) && is.numeric(data[[pred]])) {
      xv <- data[[pred]]
      if (length(xv) == nrow(x_mat)) {
        x_values[[pred]] <- xv
      }
    }
  }

  # Interaction terms (columns with ":")
  int_cols <- which(grepl(":", col_names, fixed = TRUE))
  if (length(int_cols) > 0) {
    # Group interaction columns by their parent predictor pair
    int_groups <- list()
    for (j in int_cols) {
      parts <- strsplit(col_names[j], ":", fixed = TRUE)[[1]]
      parent_vars <- vapply(parts, function(comp) {
        for (p in preds_sorted) {
          if (comp == p || startsWith(comp, p)) return(p)
        }
        comp
      }, character(1))
      key <- paste(sort(unique(parent_vars)), collapse = ":")
      int_groups[[key]] <- c(int_groups[[key]], j)
    }

    for (key in names(int_groups)) {
      idx <- int_groups[[key]]
      contrib_vec <- rep(0, nrow(x_mat))
      has_nonzero <- FALSE
      for (j in idx) {
        b <- coef_vec[col_names[j]]
        if (!is.na(b) && b != 0) {
          contrib_vec <- contrib_vec + b * x_mat[, j]
          has_nonzero <- TRUE
        }
      }
      if (has_nonzero) contribs[[key]] <- contrib_vec
    }
  }

  list(contribs = contribs, x_values = x_values)
}


#' Generate a Quarto report bundle (without rendering)
#'
#' Writes a self-contained Quarto bundle under `<dest_dir>/<base>_qmd/`
#' containing the populated `<base>.qmd`, all pre-generated plots, the report
#' data RDS (`report_data.rds`), and `reference.docx`. Render it to HTML /
#' Word / PDF with [convert_quarto_file()].
#'
#' @param assets_args Named list of arguments forwarded to
#'   [prepare_report_assets_glmnet()] (`model`, `lambda`, `gamma`, `x_mat`, `y_vec`,
#'   `coef_df`, `predictors`, `response`, `data`, and the optional extras).
#'   `assets_dir` is supplied by this function and must not be included.
#' @param dest_dir Directory to write the bundle into (typically the project's
#'   `<os>_out_glmnet` folder).
#' @param base Bundle base name (no extension). Defaults to `"glmnet_report"`.
#' @return Invisibly, the absolute path to the generated `.qmd` file.
#' @export
generate_quarto_report_glmnet <- function(assets_args, dest_dir,
                                   base = "glmnet_report") {
  dest_dir <- path.expand(dest_dir)
  bundle_dir <- file.path(dest_dir, paste0(base, "_qmd"))
  dir.create(bundle_dir, recursive = TRUE, showWarnings = FALSE)
  bundle_dir <- normalizePath(bundle_dir)

  do.call(prepare_report_assets_glmnet,
          c(assets_args, list(assets_dir = bundle_dir)))

  template <- system.file("quarto", "glmnet_report.qmd", package = "earthUI")
  if (template == "")
    stop("Quarto template not found. Try reinstalling 'glmnetUI'.",
         call. = FALSE)
  qmd_path <- file.path(bundle_dir, paste0(base, ".qmd"))
  file.copy(template, qmd_path, overwrite = TRUE)

  # Point the bundle's qmd at its co-located report_data.rds by default.
  qmd_text <- readLines(qmd_path, warn = FALSE)
  qmd_text <- sub('^  data_file: ""$', '  data_file: "report_data.rds"',
                  qmd_text)
  writeLines(qmd_text, qmd_path)

  ref_docx <- system.file("quarto", "reference.docx", package = "earthUI")
  if (ref_docx != "")
    file.copy(ref_docx, file.path(bundle_dir, "reference.docx"),
              overwrite = TRUE)

  message("glmnetUI: Quarto report bundle ready at ", bundle_dir)
  invisible(qmd_path)
}

# Build the enabled-interaction matrix for the report: an n x n logical matrix
# (predictors on both axes) with TRUE where a pairwise interaction term was
# included in the model. Derived from the coefficient table: interaction
# columns contain ":" (e.g. "x1:x2" or "x1:x2Level"); each part is mapped back
# to its parent predictor. Mirrors earthUI's allowed-interaction matrix.
#' @noRd
build_interaction_matrix__glmnet <- function(coef_df, predictors) {
  n <- length(predictors)
  mat <- matrix(FALSE, n, n, dimnames = list(predictors, predictors))
  if (n < 2L || is.null(coef_df) || !"Variable" %in% names(coef_df)) {
    return(mat)
  }
  preds_sorted <- predictors[order(-nchar(predictors))]
  int_cols <- coef_df$Variable[grepl(":", coef_df$Variable, fixed = TRUE)]
  for (cn in int_cols) {
    parts <- strsplit(cn, ":", fixed = TRUE)[[1L]]
    parents <- unique(vapply(parts, function(comp) {
      for (p in preds_sorted) if (comp == p || startsWith(comp, p)) return(p)
      comp
    }, character(1)))
    if (length(parents) == 2L && all(parents %in% predictors)) {
      mat[parents[1L], parents[2L]] <- TRUE
      mat[parents[2L], parents[1L]] <- TRUE
    }
  }
  mat
}
