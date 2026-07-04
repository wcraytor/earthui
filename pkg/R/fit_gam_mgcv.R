# Common date formats tried when parsing character date columns.
gam_date_formats_ <- c("%m/%d/%y", "%m/%d/%Y", "%Y-%m-%d", "%Y/%m/%d",
                       "%d/%m/%y", "%d/%m/%Y", "%m-%d-%y", "%m-%d-%Y",
                       "%d-%m-%Y", "%d.%m.%Y", "%b %d, %Y", "%B %d, %Y")

#' Coerce a data frame so it matches what the GAM was fit on
#'
#' Applies the same type coercions at fit time and predict time so the model is
#' predictable on the original data:
#' * `Date`/`POSIXct` columns become numeric.
#' * Character/factor columns used in a SMOOTH term are parsed as dates to
#'   numeric (e.g. an earth-imported `contract_date`); a non-date character
#'   column in a smooth raises a clear error.
#' * Columns in a parametric (`"linear"`) term that were designated Factor
#'   (`spec$factor`) or are character become factors, so they are dummy-coded.
#'
#' @param data A data frame.
#' @param smooth_specs The model's smooth specifications.
#' @return The coerced data frame.
#' @noRd
prepare_gam_data_ <- function(data, smooth_specs) {
  data <- as.data.frame(data)
  for (col in names(data)) {
    if (inherits(data[[col]], c("POSIXct", "POSIXlt", "Date"))) {
      data[[col]] <- as.numeric(data[[col]])
    }
  }
  for (spec in smooth_specs) {
    if (identical(spec$type, "linear")) {
      for (v in spec$vars) {
        if (v %in% names(data) &&
            (isTRUE(spec$factor) || is.character(data[[v]]))) {
          data[[v]] <- as.factor(data[[v]])
        }
      }
    } else {
      for (v in spec$vars) {
        if (!v %in% names(data)) next
        col <- data[[v]]
        if (!is.character(col) && !is.factor(col)) next
        x <- as.character(col)
        parsed <- NULL
        for (fmt in gam_date_formats_) {
          d <- as.Date(x, format = fmt)
          ok <- !is.na(d) & d >= as.Date("1900-01-01") &
            d <= as.Date("2100-12-31")
          if (sum(ok, na.rm = TRUE) > length(x) * 0.5) { parsed <- d; break }
        }
        if (!is.null(parsed)) {
          data[[v]] <- as.numeric(parsed)
        } else {
          stop("Cannot smooth non-numeric variable '", v,
               "'. Designate it as Factor or Linear (or remove it).",
               call. = FALSE)
        }
      }
    }
  }
  data
}

#' Fit a GAM Model
#'
#' Wrapper around [mgcv::gam()] that accepts the specification objects
#' produced by the mgcvUI interface and returns a structured result.
#'
#' @param data Data frame of observations.
#' @param response Character name of the response variable.
#' @param smooth_specs List of smooth-term specifications (see
#'   [build_gam_formula()]).
#' @param family A family object or character string (e.g. `"gaussian"`,
#'   `"Gamma"`). Default is `gaussian()`.
#' @param method Character fitting method. Default `"REML"`.
#' @param earth_knots Optional `mgcvUI_earth_knots` object from
#'   [import_earth_mgcv()]. When provided, knot positions are passed to
#'   [mgcv::gam()].
#' @param select Logical. If `TRUE`, adds an extra penalty to each
#'   smooth term so that it can be penalised to zero (variable
#'   selection). Default `FALSE`.
#' @param gamma Numeric inflation factor for the effective degrees of
#'   freedom in the GCV/UBRE score. Values greater than 1 produce
#'   smoother fits. Default `1`.
#' @param cv_folds Integer number of cross-validation folds. Set to 0
#'   to skip CV. Default `10`.
#' @param weights Optional numeric vector of prior weights (one per
#'   observation). Passed to [mgcv::gam()].
#' @param optimizer Character optimizer key: `"outer_newton"` (default),
#'   `"outer_bfgs"`, or `"efs"`.
#' @param scale Numeric scale parameter. `0` (default) means estimate;
#'   a positive value fixes it.
#' @param discrete Logical. If `TRUE`, uses discretized covariate
#'   method for faster fitting on large datasets. Default `FALSE`.
#' @param nthreads Integer number of threads for parallel computation.
#'   Default `1`.
#' @return An `mgcvUI_result` object (a list) with components:
#'   \describe{
#'     \item{model}{The fitted [mgcv::gam] object.}
#'     \item{formula}{The formula used.}
#'     \item{response}{Response variable name.}
#'     \item{smooth_specs}{The smooth-term specs used.}
#'     \item{earth_knots}{The earth knots used (or `NULL`).}
#'     \item{elapsed}{Fitting time in seconds.}
#'     \item{cv_rsq}{Cross-validated R-squared (or `NULL` if CV skipped).}
#'   }
#' @export
#' @examples
#' specs <- list(
#'   list(vars = "wt", type = "s", bs = "tp", k = NULL),
#'   list(vars = "hp", type = "s", bs = "tp", k = NULL)
#' )
#' result <- fit_gam(mtcars, "mpg", specs)
#' summary(result$model)
fit_gam <- function(data, response, smooth_specs, family = gaussian(),
                    method = "REML", earth_knots = NULL,
                    select = TRUE, gamma = 1.2, cv_folds = 10L,
                    weights = NULL, optimizer = NULL, scale = 0,
                    discrete = FALSE, nthreads = 1L) {

  # Ensure mgcv namespace is loaded (needed in callr subprocesses).
  # When discrete=TRUE, gam() internally calls bam() by name, which

  # requires mgcv on the search path -- not just loaded as a namespace.
  if (!isNamespaceLoaded("mgcv")) loadNamespace("mgcv")
  if (isTRUE(discrete) && !"package:mgcv" %in% search()) {
    attachNamespace("mgcv")
  }

  # Defensive defaults for NULL parameters (Shiny inputs may be NULL)
  if (is.null(method)) method <- "REML"
  if (is.null(select)) select <- TRUE
  if (is.null(gamma) || is.na(gamma)) gamma <- 1.2
  if (is.null(family)) family <- gaussian()
  if (is.null(scale) || is.na(scale)) scale <- 0
  if (is.null(discrete)) discrete <- FALSE
  if (is.null(nthreads) || is.na(nthreads)) nthreads <- 1L

  # Ensure plain data.frame
  data <- as.data.frame(data)

  # Validate smooth_specs structure
  for (i in seq_along(smooth_specs)) {
    spec <- smooth_specs[[i]]
    if (is.null(spec$vars) || length(spec$vars) == 0L) {
      stop("smooth_specs[[", i, "]] has no 'vars'.", call. = FALSE)
    }
    if (!all(spec$vars %in% names(data))) {
      missing <- setdiff(spec$vars, names(data))
      stop("Variable(s) not in data: ", paste(missing, collapse = ", "),
           call. = FALSE)
    }
    if (!response %in% names(data)) {
      stop("Response '", response, "' not found in data.", call. = FALSE)
    }
  }

  # Coerce types consistently (dates/char-dates -> numeric for smooths,
  # factor-designated/character -> factor for parametric terms). The SAME
  # transform must be applied at predict time (export, RCA, plots) so the model
  # is predictable on the original data -- see [prepare_gam_data_()].
  data <- prepare_gam_data_(data, smooth_specs)

  # Handle low-cardinality variables
  drop_idx <- integer(0)
  for (i in seq_along(smooth_specs)) {
    spec <- smooth_specs[[i]]
    if (spec$type == "linear") next
    if (length(spec$vars) != 1L) next
    n_unique <- length(unique(stats::na.omit(data[[spec$vars]])))
    if (n_unique <= 1L) {
      # Constant -- drop from model
      message("  dropping '", spec$vars, "': only ", n_unique,
              " unique value(s)")
      drop_idx <- c(drop_idx, i)
    } else if (n_unique == 2L) {
      # Binary -- treat as factor
      message("  '", spec$vars, "': 2 unique values, converting to factor")
      data[[spec$vars]] <- as.factor(data[[spec$vars]])
      smooth_specs[[i]]$type <- "linear"
      smooth_specs[[i]]$bs <- NULL
      smooth_specs[[i]]$k <- NULL
    } else {
      k <- spec$k
      if (is.null(k)) k <- 10L  # mgcv default
      if (k >= n_unique) {
        smooth_specs[[i]]$k <- max(n_unique - 1L, 3L)
      }
    }
  }
  if (length(drop_idx) > 0L) {
    smooth_specs <- smooth_specs[-drop_idx]
  }
  if (length(smooth_specs) == 0L) {
    stop("No usable predictors remain after filtering.", call. = FALSE)
  }

  # Reconcile k with earth knot counts for cr/ps bases
  smooth_specs <- reconcile_knots_(smooth_specs, earth_knots)

  # Final safety: cap k to unique covariate combinations
  # Must use complete cases across ALL model variables (mgcv uses na.action)
  all_model_vars <- unique(c(response,
    unlist(lapply(smooth_specs, `[[`, "vars"))))
  complete_rows <- stats::complete.cases(data[, all_model_vars, drop = FALSE])
  complete_data <- data[complete_rows, , drop = FALSE]
  n_complete <- nrow(complete_data)
  message("  complete cases: ", n_complete, " of ", nrow(data))

  for (i in seq_along(smooth_specs)) {
    spec <- smooth_specs[[i]]
    if (spec$type == "linear") next
    k_eff <- spec$k %||% 10L  # mgcv default is 10
    if (length(spec$vars) == 1L) {
      n_unique <- length(unique(complete_data[[spec$vars]]))
    } else {
      # Multi-variable: count unique combinations
      sub <- complete_data[, spec$vars, drop = FALSE]
      n_unique <- nrow(unique(sub))
    }
    if (k_eff >= n_unique) {
      new_k <- max(n_unique - 1L, 3L)
      message("  capping k for ", paste(spec$vars, collapse = ","),
              ": ", k_eff, " -> ", new_k, " (unique=", n_unique, ")")
      smooth_specs[[i]]$k <- new_k
      # For cr basis with earth knots, capping k breaks the k==length(knots)
      # requirement -- switch to tp to avoid the mismatch
      if (identical(spec$bs, "cr") && !is.null(earth_knots) &&
          length(spec$vars) == 1L &&
          !is.null(earth_knots$knots[[spec$vars]])) {
        message("  switching ", spec$vars, " from cr to tp (k was capped)")
        smooth_specs[[i]]$bs <- "tp"
      }
    }
  }

  # When earth knots are supplied, tensor product marginals default to "cr"

  # basis in mgcv. The global knots list applies to ALL terms using that
  # variable, so ti/te marginals would get the earth knots with a mismatched k.
  # Fix: force marginals to "tp" for variables that have earth knots.
  if (!is.null(earth_knots)) {
    knot_vars <- names(earth_knots$knots)
    for (i in seq_along(smooth_specs)) {
      spec <- smooth_specs[[i]]
      if (!spec$type %in% c("ti", "te")) next
      if (any(spec$vars %in% knot_vars)) {
        smooth_specs[[i]]$marginal_bs <- rep("tp", length(spec$vars))
      }
    }
  }

  formula <- build_gam_formula(response, smooth_specs)
  knots  <- build_gam_knots(smooth_specs, earth_knots, data = complete_data)

  if (is.character(family)) {
    family <- get(family, mode = "function")()
  }

  # (Type coercion, including factor designation, was applied up-front by
  # prepare_gam_data_.)

  # Log column classes for debugging
  # Include `by` variables (factor-by-smooth) so the model frame retains them.
  all_vars <- unique(c(response,
                       unlist(lapply(smooth_specs, `[[`, "vars")),
                       unlist(lapply(smooth_specs, `[[`, "by"))))
  for (v in all_vars) {
    message("  col '", v, "': class=", paste(class(data[[v]]), collapse = "/"),
            " length=", length(data[[v]]),
            " NAs=", sum(is.na(data[[v]])))
  }
  message("  formula: ", deparse(formula))

  # Debug: log k vs unique for each smooth term (using complete cases)
  for (spec in smooth_specs) {
    if (spec$type == "linear") next
    k_val <- spec$k %||% "default(10)"
    if (length(spec$vars) == 1L) {
      nu <- length(unique(complete_data[[spec$vars]]))
    } else {
      sub <- complete_data[, spec$vars, drop = FALSE]
      nu <- nrow(unique(sub))
    }
    message("  term: ", spec$type, "(", paste(spec$vars, collapse = ","),
            ") bs=", spec$bs %||% "NULL", " k=", k_val,
            " unique(complete)=", nu)
  }

  # Subset data to only model-relevant columns to prevent na.action

  # from dropping rows due to NAs in unrelated columns
  model_data <- data[, all_vars, drop = FALSE]

  # Parse optimizer setting
  opt <- if (!is.null(optimizer) && nzchar(optimizer)) {
    switch(optimizer,
      outer_newton = c("outer", "newton"),
      outer_bfgs   = c("outer", "bfgs"),
      efs          = c("efs"),
      c("outer", "newton")  # default
    )
  } else {
    c("outer", "newton")
  }

  cat("Fitting main GAM model...\n")
  t0 <- proc.time()
  gam_args <- list(
    formula   = formula,
    data      = model_data,
    family    = family,
    method    = method,
    knots     = knots,
    select    = select,
    gamma     = gamma,
    optimizer = opt,
    scale     = scale,
    discrete  = discrete,
    control   = mgcv::gam.control(nthreads = as.integer(nthreads))
  )
  if (!is.null(weights)) {
    gam_args$weights <- weights
  }
  # The default outer/newton optimizer can diverge on ill-conditioned or
  # over-parameterized models (NaN gradient -> "missing value where TRUE/FALSE
  # needed"). Fall back to the more robust EFS optimizer, then to a clear error.
  model <- tryCatch(
    do.call(mgcv::gam, gam_args),
    error = function(e) {
      message("  primary optimizer failed (", conditionMessage(e),
              "); retrying with optimizer='efs'")
      args2 <- gam_args
      args2$optimizer <- "efs"
      tryCatch(
        do.call(mgcv::gam, args2),
        error = function(e2)
          stop("GAM fit failed (", conditionMessage(e2), "). The model may be ",
               "over-parameterized for the data -- reduce the number of ",
               "smooths/interactions (e.g. avoid several overlapping tensor ",
               "terms on the same variable, or collinear predictors like ",
               "contract_date and sale_age), or raise gamma.", call. = FALSE)
      )
    }
  )
  elapsed <- (proc.time() - t0)[["elapsed"]]
  cat(sprintf("Main GAM fit complete in %.1fs\n", elapsed))

  # Cross-validated R-squared
  cv_folds <- if (is.null(cv_folds)) 0L else as.integer(cv_folds)
  cv_rsq <- NULL
  if (cv_folds >= 2L) {
    cat(sprintf("Starting %d-fold cross-validation...\n", cv_folds))
    cv_rsq <- tryCatch(
      cv_rsq_(data, formula, family, method, select, gamma,
              cv_folds, response, weights),
      error = function(e) {
        message("  CV R-sq failed: ", e$message)
        NULL
      }
    )
  }

  structure(
    list(
      model        = model,
      formula      = formula,
      response     = response,
      smooth_specs = smooth_specs,
      earth_knots  = earth_knots,
      elapsed      = elapsed,
      cv_rsq       = cv_rsq
    ),
    class = "mgcvUI_result"
  )
}


#' Check for Sign Discrepancies Between Earth and GAM
#'
#' Compares the direction implied by earth hinge functions with the
#' estimated GAM smooth derivatives.
#'
#' @param gam_result An `mgcvUI_result` from [fit_gam()].
#' @return A data frame with columns: `variable`, `earth_direction`,
#'   `gam_direction`, `consistent`, `warning`. Returns `NULL` if no
#'   earth knots are available.
#' @export
#' @examples
#' if (requireNamespace("earth", quietly = TRUE)) {
#'   m <- earth::earth(mpg ~ wt + hp, data = mtcars)
#'   er <- structure(
#'     list(model = m, target = "mpg",
#'          predictors = c("wt", "hp"),
#'          categoricals = character(0), linpreds = character(0),
#'          degree = 1L, cv_enabled = FALSE, allowed_matrix = NULL,
#'          data = mtcars, elapsed = 0, trace_output = character(0)),
#'     class = "earthUI_result"
#'   )
#'   ek <- import_earth_mgcv(er)
#'   specs <- list(
#'     list(vars = "wt", type = "s", bs = "tp", k = NULL),
#'     list(vars = "hp", type = "s", bs = "tp", k = NULL)
#'   )
#'   gam_res <- fit_gam(mtcars, "mpg", specs, earth_knots = ek)
#'   check_sign_consistency(gam_res)
#' }
check_sign_consistency <- function(gam_result) {
  ek <- gam_result$earth_knots
  if (is.null(ek)) return(NULL)

  model <- gam_result$model
  rows <- list()

  for (var in names(ek$knots)) {
    knot_vals <- ek$knots[[var]]
    earth_signs <- ek$signs[[var]]

    # Determine dominant earth direction for this variable
    earth_dir <- if (all(earth_signs == 1L)) {
      "increasing"
    } else if (all(earth_signs == -1L)) {
      "decreasing"
    } else {
      "mixed"
    }

    # Estimate GAM smooth direction at the knot points
    gam_dir <- estimate_smooth_direction_(model, var)

    consistent <- (earth_dir == "mixed") ||
      (earth_dir == gam_dir) ||
      (gam_dir == "mixed")

    warning_msg <- if (!consistent) {
      paste0("Earth suggests '", earth_dir,
             "' but GAM smooth is '", gam_dir, "' for ", var)
    } else {
      NA_character_
    }

    rows[[var]] <- data.frame(
      variable        = var,
      earth_direction = earth_dir,
      gam_direction   = gam_dir,
      consistent      = consistent,
      warning         = warning_msg,
      stringsAsFactors = FALSE
    )
  }

  if (length(rows) == 0L) return(NULL)
  out <- do.call(rbind, rows)
  rownames(out) <- NULL
  out
}


#' Estimate the dominant direction of a GAM smooth
#' @param model A fitted gam object.
#' @param variable Character name of the variable.
#' @return Character: "increasing", "decreasing", or "mixed".
#' @noRd
estimate_smooth_direction_ <- function(model, variable) {
  data <- model$model
  if (!variable %in% names(data)) return("mixed")

  x_vals <- data[[variable]]
  x_seq <- seq(min(x_vals, na.rm = TRUE), max(x_vals, na.rm = TRUE),
               length.out = 100L)

  newdata <- data[rep(1L, 100L), , drop = FALSE]
  newdata[[variable]] <- x_seq

  preds <- predict(model, newdata = newdata, type = "terms")

  # Find the column corresponding to this smooth
  smooth_col <- grep(paste0("s\\(", variable), colnames(preds), value = TRUE)
  if (length(smooth_col) == 0L) return("mixed")

  vals <- preds[, smooth_col[1L]]
  diffs <- diff(vals)

  pos_frac <- mean(diffs > 0)
  if (pos_frac > 0.7) {
    "increasing"
  } else if (pos_frac < 0.3) {
    "decreasing"
  } else {
    "mixed"
  }
}


#' Reconcile smooth spec k values with earth knot counts
#'
#' For cr basis, mgcv requires k == length(knots). Rather than capping k
#' to the number of earth knots (which over-constrains the GAM), this
#' sets k = max(n_earth_knots, user_k) so the GAM retains at least as
#' much flexibility as it would have standalone. The knots list is later
#' augmented with data quantiles in [build_gam_knots()] to fill the gap.
#'
#' @param smooth_specs List of smooth-term spec lists.
#' @param earth_knots An mgcvUI_earth_knots object, or NULL.
#' @return Updated smooth_specs with reconciled k values.
#' @noRd
reconcile_knots_ <- function(smooth_specs, earth_knots) {
  if (is.null(earth_knots)) return(smooth_specs)

  for (i in seq_along(smooth_specs)) {
    spec <- smooth_specs[[i]]
    if (spec$type == "linear") next
    if (length(spec$vars) != 1L) next

    var <- spec$vars
    ek <- earth_knots$knots[[var]]
    if (is.null(ek)) next

    n_knots <- length(ek)

    if (identical(spec$bs, "cr")) {
      if (n_knots < 3L) {
        # Too few knots for cr basis -- fall back to tp, ignore knots
        smooth_specs[[i]]$bs <- "tp"
        smooth_specs[[i]]$k <- NULL
      } else {
        # cr basis: k must equal length(knots). Use the larger of the
        # earth knot count and the user/default k so the smooth is at
        # least as flexible as standalone.
        user_k <- spec$k %||% 10L
        smooth_specs[[i]]$k <- max(n_knots, user_k)
      }
    } else if (identical(spec$bs, "ps") || identical(spec$bs, "bs")) {
      # ps/bs: k must be >= number of knots; set to knots + 2 if too low
      if (!is.null(spec$k) && spec$k < n_knots) {
        smooth_specs[[i]]$k <- n_knots + 2L
      }
    }
  }

  smooth_specs
}


#' K-fold cross-validated R-squared
#' @param data Data frame.
#' @param formula Formula object.
#' @param family Family object.
#' @param method Character fitting method.
#' @param select Logical.
#' @param gamma Numeric.
#' @param k Integer number of folds.
#' @param response Character response variable name.
#' @return Numeric CV R-squared.
#' @noRd
cv_rsq_ <- function(data, formula, family, method, select,
                    gamma, k, response, weights = NULL) {
  # Subset to model columns only (avoid na.action dropping unrelated NAs)
  used_vars <- all.vars(formula)
  data <- data[, used_vars, drop = FALSE]
  complete <- stats::complete.cases(data)
  data <- data[complete, , drop = FALSE]
  if (!is.null(weights)) weights <- weights[complete]
  n <- nrow(data)

  # Drop unused factor levels to keep fold models consistent
  for (v in names(data)) {
    if (is.factor(data[[v]])) data[[v]] <- droplevels(data[[v]])
  }

  folds <- sample(rep(seq_len(k), length.out = n))
  preds <- numeric(n)
  failed <- 0L

  for (fold in seq_len(k)) {
    cat(sprintf("CV fold %d/%d...\n", fold, k))
    train <- data[folds != fold, , drop = FALSE]
    test  <- data[folds == fold, , drop = FALSE]
    train_wt <- if (!is.null(weights)) weights[folds != fold] else NULL

    # Drop factor levels absent from training fold
    for (v in names(train)) {
      if (is.factor(train[[v]])) {
        train[[v]] <- droplevels(train[[v]])
        # Align test factor levels with training levels
        test[[v]] <- factor(test[[v]], levels = levels(train[[v]]))
      }
    }

    gam_args <- list(
      formula = formula,
      data    = train,
      family  = family,
      method  = method,
      knots   = NULL,  # let mgcv auto-place knots per fold
      select  = select,
      gamma   = gamma
    )
    if (!is.null(train_wt)) gam_args$weights <- train_wt

    fit <- tryCatch(
      do.call(mgcv::gam, gam_args),
      error = function(e) NULL
    )

    n_test <- sum(folds == fold)
    if (is.null(fit)) {
      failed <- failed + 1L
      preds[folds == fold] <- NA_real_
    } else {
      p <- tryCatch(
        predict(fit, newdata = test, type = "response"),
        error = function(e) rep(NA_real_, n_test)
      )
      # Safety: ensure prediction length matches test set
      if (length(p) != n_test) p <- rep(NA_real_, n_test)
      # Replace non-finite predictions (Inf, -Inf, NaN) with NA
      p[!is.finite(p)] <- NA_real_
      n_na <- sum(is.na(p))
      p_range <- if (any(!is.na(p))) range(p, na.rm = TRUE) else c(NA, NA)
      a_range <- range(test[[response]], na.rm = TRUE)
      cat(sprintf("  fold %d: pred range [%.2f, %.2f] actual [%.2f, %.2f] NAs=%d/%d\n",
                  fold, p_range[1], p_range[2], a_range[1], a_range[2],
                  n_na, n_test))
      preds[folds == fold] <- p
    }
  }

  if (failed > k / 2) {
    stop("Too many CV folds failed (", failed, "/", k, ").", call. = FALSE)
  }

  valid <- !is.na(preds)
  n_valid <- sum(valid)
  n_dropped <- length(preds) - n_valid
  if (n_dropped > 0L) {
    cat(sprintf("CV: %d of %d predictions were NA/non-finite (dropped)\n",
                n_dropped, length(preds)))
  }
  cat(sprintf("CV complete (%d/%d folds succeeded, %d valid predictions)\n",
              k - failed, k, n_valid))

  if (n_valid < length(preds) * 0.5) {
    message("  CV R-sq unreliable: too many predictions failed")
    return(NA_real_)
  }

  actual <- data[[response]][valid]
  p <- preds[valid]
  # Use overall mean (not just valid subset) for proper R^2 denominator
  overall_mean <- mean(data[[response]])
  ss_res <- sum((actual - p)^2)
  ss_tot <- sum((actual - overall_mean)^2)
  1 - ss_res / ss_tot
}


#' Back-transform a value from log or log10 scale
#' @param x Numeric vector on the transformed scale.
#' @param transform Character: "none", "log", or "log10".
#' @return Numeric vector on the original scale.
#' @noRd
back_transform_ <- function(x, transform) {
  switch(transform %||% "none",
    log   = exp(x),
    log10 = 10^x,
    x
  )
}

#' Apply a transform to a value
#' @param x Numeric vector on the original scale.
#' @param transform Character: "none", "log", or "log10".
#' @return Numeric vector on the transformed scale.
#' @noRd
apply_transform_ <- function(x, transform) {
  switch(transform %||% "none",
    log   = log(x),
    log10 = log10(x),
    x
  )
}
