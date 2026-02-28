#' Fit an earth model
#'
#' Wrapper around [earth::earth()] with parameter validation and automatic
#' cross-validation when interaction terms are enabled.
#'
#' @param df A data frame containing the modeling data.
#' @param target Character string. Name of the response variable.
#' @param predictors Character vector. Names of predictor variables.
#' @param categoricals Character vector. Names of predictors to treat as
#'   categorical (converted to factors before fitting). Default is `NULL`.
#' @param linpreds Character vector. Names of predictors constrained to enter
#'   the model linearly (no hinge functions). Default is `NULL`.
#' @param degree Integer. Maximum degree of interaction. Default is 1
#'   (no interactions). When >= 2, cross-validation is automatically enabled.
#' @param allowed_func Function or `NULL`. An allowed function as returned by
#'   [build_allowed_function()]. Only used when `degree >= 2`.
#' @param nfold Integer. Number of cross-validation folds. Automatically set
#'   to 10 when `degree >= 2` unless explicitly provided. Set to 0 to disable.
#' @param nprune Integer or `NULL`. Maximum number of terms in the pruned model.
#' @param thresh Numeric. Forward stepping threshold. Default is earth's default
#'   (0.001).
#' @param penalty Numeric. Generalized cross-validation penalty per knot.
#'   Default is earth's default (if `degree > 1`, `3`; otherwise `2`).
#' @param minspan Integer or `NULL`. Minimum number of observations between
#'   knots.
#' @param endspan Integer or `NULL`. Minimum number of observations before
#'   the first and after the last knot.
#' @param fast.k Integer. Maximum number of parent terms considered at each
#'   step of the forward pass. Default is earth's default (20).
#' @param pmethod Character. Pruning method. One of `"backward"`, `"none"`,
#'   `"exhaustive"`, `"forward"`, `"seqrep"`, `"cv"`. Default is `"backward"`.
#' @param glm List or `NULL`. If provided, passed to earth's `glm` argument
#'   to fit a GLM on the earth basis functions.
#' @param trace Numeric. Trace earth's execution. 0 (default) = none,
#'   0.3 = variance model, 0.5 = cross validation, 1-5 = increasing detail.
#' @param nk Integer or `NULL`. Maximum number of model terms before pruning.
#' @param newvar.penalty Numeric or `NULL`. Penalty for adding a new variable
#'   in the forward pass (Friedman's gamma). Default 0.
#' @param fast.beta Numeric or `NULL`. Fast MARS ageing coefficient. Default 1.
#' @param ncross Integer or `NULL`. Number of cross-validations. Default 1.
#' @param stratify Logical or `NULL`. Stratify cross-validation samples.
#'   Default `TRUE`.
#' @param varmod.method Character or `NULL`. Variance model method. One of
#'   `"none"`, `"const"`, `"lm"`, `"rlm"`, `"earth"`, `"gam"`, `"power"`,
#'   `"power0"`, `"x.lm"`, `"x.rlm"`, `"x.earth"`, `"x.gam"`.
#' @param varmod.exponent Numeric or `NULL`. Power transform for variance model.
#' @param varmod.conv Numeric or `NULL`. Convergence criterion for IRLS.
#' @param varmod.clamp Numeric or `NULL`. Minimum estimated standard deviation.
#' @param varmod.minspan Integer or `NULL`. minspan for internal variance model.
#' @param keepxy Logical or `NULL`. Retain x, y in model object. Default `FALSE`.
#' @param Scale.y Logical or `NULL`. Scale response internally. Default `TRUE`.
#' @param Adjust.endspan Numeric or `NULL`. Interaction endspan multiplier.
#'   Default 2.
#' @param Auto.linpreds Logical or `NULL`. Auto-detect linear predictors.
#'   Default `TRUE`.
#' @param Force.weights Logical or `NULL`. Force weighted code path. Default
#'   `FALSE`.
#' @param Use.beta.cache Logical or `NULL`. Cache coefficients in forward pass.
#'   Default `TRUE`.
#' @param Force.xtx.prune Logical or `NULL`. Force X'X-based pruning. Default
#'   `FALSE`.
#' @param Get.leverages Logical or `NULL`. Calculate hat values. Default `TRUE`.
#' @param Exhaustive.tol Numeric or `NULL`. Condition number threshold for
#'   exhaustive pruning. Default 1e-10.
#' @param wp Numeric vector or `NULL`. Response weights.
#' @param ... Additional arguments passed to [earth::earth()].
#'
#' @return A list with class `"earthui_result"` containing:
#'   \describe{
#'     \item{model}{The fitted earth model object.}
#'     \item{target}{Name of the response variable.}
#'     \item{predictors}{Names of predictor variables used.}
#'     \item{categoricals}{Names of categorical predictors.}
#'     \item{degree}{Degree of interaction used.}
#'     \item{cv_enabled}{Logical; whether cross-validation was used.}
#'     \item{data}{The data frame used for fitting.}
#'   }
#'
#' @export
#' @examples
#' df <- mtcars
#' result <- fit_earth(df, target = "mpg",
#'                     predictors = c("cyl", "disp", "hp", "wt"))
#' format_summary(result)
fit_earth <- function(df, target, predictors, categoricals = NULL,
                      linpreds = NULL, degree = 1L, allowed_func = NULL,
                      nfold = NULL, nprune = NULL, thresh = NULL,
                      penalty = NULL, minspan = NULL, endspan = NULL,
                      fast.k = NULL, pmethod = NULL, glm = NULL,
                      trace = NULL, nk = NULL, newvar.penalty = NULL,
                      fast.beta = NULL, ncross = NULL, stratify = NULL,
                      varmod.method = NULL, varmod.exponent = NULL,
                      varmod.conv = NULL, varmod.clamp = NULL,
                      varmod.minspan = NULL, keepxy = NULL,
                      Scale.y = NULL, Adjust.endspan = NULL,
                      Auto.linpreds = NULL, Force.weights = NULL,
                      Use.beta.cache = NULL, Force.xtx.prune = NULL,
                      Get.leverages = NULL, Exhaustive.tol = NULL,
                      wp = NULL, ...) {

  # --- Input validation ---
  if (!is.data.frame(df)) {
    stop("`df` must be a data frame.", call. = FALSE)
  }
  if (!is.character(target) || length(target) != 1L) {
    stop("`target` must be a single character string.", call. = FALSE)
  }
  if (!target %in% names(df)) {
    stop("Target variable '", target, "' not found in data frame.", call. = FALSE)
  }
  if (!is.character(predictors) || length(predictors) == 0L) {
    stop("`predictors` must be a non-empty character vector.", call. = FALSE)
  }
  missing_preds <- setdiff(predictors, names(df))
  if (length(missing_preds) > 0L) {
    stop("Predictor(s) not found in data frame: ",
         paste(missing_preds, collapse = ", "), call. = FALSE)
  }
  if (target %in% predictors) {
    stop("Target variable must not be in the predictors list.", call. = FALSE)
  }

  degree <- as.integer(degree)
  if (degree < 1L || degree > 10L) {
    stop("`degree` must be between 1 and 10.", call. = FALSE)
  }

  # --- Prepare data ---
  model_df <- df[, c(target, predictors), drop = FALSE]

  # Convert categoricals to factors
  if (!is.null(categoricals)) {
    categoricals <- intersect(categoricals, predictors)
    for (col in categoricals) {
      model_df[[col]] <- as.factor(model_df[[col]])
    }
  }

  # Remove rows with NA in target or predictors
  complete <- stats::complete.cases(model_df)
  n_removed <- sum(!complete)
  if (n_removed > 0L) {
    message("Removed ", n_removed, " rows with missing values.")
    model_df <- model_df[complete, , drop = FALSE]
  }
  if (nrow(model_df) < 10L) {
    na_counts <- vapply(model_df, function(col) sum(is.na(col)), integer(1L))
    na_info <- na_counts[na_counts > 0L]
    detail <- if (length(na_info) > 0L) {
      paste0(" Columns with NAs: ",
             paste(sprintf("%s (%d)", names(na_info), na_info), collapse = ", "),
             ".")
    } else {
      ""
    }
    stop("Insufficient data: need at least 10 complete observations, have ",
         nrow(model_df), " (from ", nrow(df), " original rows).", detail,
         call. = FALSE)
  }

  # Drop factor/character columns with fewer than 2 unique values (causes contrasts error)
  drop_cols <- character(0)
  for (col in names(model_df)) {
    if (col == target) next
    if (is.factor(model_df[[col]])) {
      if (nlevels(droplevels(model_df[[col]])) < 2L) {
        drop_cols <- c(drop_cols, col)
      }
    } else if (is.character(model_df[[col]])) {
      if (length(unique(model_df[[col]])) < 2L) {
        drop_cols <- c(drop_cols, col)
      }
    }
  }
  if (length(drop_cols) > 0L) {
    message("Dropped factor columns with < 2 levels: ",
            paste(drop_cols, collapse = ", "))
    model_df <- model_df[, !names(model_df) %in% drop_cols, drop = FALSE]
    predictors <- setdiff(predictors, drop_cols)
    if (!is.null(categoricals)) {
      categoricals <- setdiff(categoricals, drop_cols)
    }
    if (length(predictors) == 0L) {
      stop("No predictors remaining after dropping single-level factors.",
           call. = FALSE)
    }
  }

  # Drop unused factor levels
  for (col in names(model_df)) {
    if (is.factor(model_df[[col]])) {
      model_df[[col]] <- droplevels(model_df[[col]])
    }
  }

  # --- Build earth arguments ---
  formula <- stats::as.formula(paste("`", target, "` ~ .", sep = ""))

  earth_args <- list(formula = formula, data = model_df, degree = degree)

  # Auto-enable CV when degree >= 2
  cv_enabled <- FALSE
  if (!is.null(nfold)) {
    if (nfold > 0L) {
      earth_args$nfold <- as.integer(nfold)
      cv_enabled <- TRUE
    }
  } else if (degree >= 2L) {
    earth_args$nfold <- 10L
    cv_enabled <- TRUE
  }

  # Linear predictors (no hinge functions)
  if (!is.null(linpreds) && length(linpreds) > 0L) {
    linpreds <- intersect(linpreds, names(model_df))
    if (length(linpreds) > 0L) {
      # earth expects column indices (1-based, relative to predictor columns)
      pred_cols <- setdiff(names(model_df), target)
      lin_idx <- match(linpreds, pred_cols)
      lin_idx <- lin_idx[!is.na(lin_idx)]
      if (length(lin_idx) > 0L) {
        earth_args$linpreds <- lin_idx
      }
    }
  }

  if (!is.null(nprune))          earth_args$nprune          <- as.integer(nprune)
  if (!is.null(thresh))          earth_args$thresh           <- thresh
  if (!is.null(penalty))         earth_args$penalty          <- penalty
  if (!is.null(minspan))         earth_args$minspan          <- as.integer(minspan)
  if (!is.null(endspan))         earth_args$endspan          <- as.integer(endspan)
  if (!is.null(fast.k))          earth_args$fast.k           <- as.integer(fast.k)
  if (!is.null(pmethod))         earth_args$pmethod          <- pmethod
  if (!is.null(glm))             earth_args$glm              <- glm
  if (!is.null(trace))           earth_args$trace            <- trace
  if (!is.null(nk))              earth_args$nk               <- as.integer(nk)
  if (!is.null(newvar.penalty))  earth_args$newvar.penalty   <- newvar.penalty
  if (!is.null(fast.beta))       earth_args$fast.beta        <- fast.beta
  if (!is.null(ncross))          earth_args$ncross           <- as.integer(ncross)
  if (!is.null(stratify))        earth_args$stratify         <- stratify
  if (!is.null(varmod.method) && varmod.method != "none") {
    earth_args$varmod.method <- varmod.method
  }
  if (!is.null(varmod.exponent)) earth_args$varmod.exponent  <- varmod.exponent
  if (!is.null(varmod.conv))     earth_args$varmod.conv      <- varmod.conv
  if (!is.null(varmod.clamp))    earth_args$varmod.clamp     <- varmod.clamp
  if (!is.null(varmod.minspan))  earth_args$varmod.minspan   <- as.integer(varmod.minspan)
  if (!is.null(keepxy))          earth_args$keepxy           <- keepxy
  if (!is.null(Scale.y))         earth_args$Scale.y          <- Scale.y
  if (!is.null(Adjust.endspan))  earth_args$Adjust.endspan   <- Adjust.endspan
  if (!is.null(Auto.linpreds))   earth_args$Auto.linpreds    <- Auto.linpreds
  if (!is.null(Force.weights))   earth_args$Force.weights    <- Force.weights
  if (!is.null(Use.beta.cache))  earth_args$Use.beta.cache   <- Use.beta.cache
  if (!is.null(Force.xtx.prune)) earth_args$Force.xtx.prune  <- Force.xtx.prune
  if (!is.null(Get.leverages))   earth_args$Get.leverages    <- Get.leverages
  if (!is.null(Exhaustive.tol))  earth_args$Exhaustive.tol   <- Exhaustive.tol
  if (!is.null(wp))              earth_args$wp               <- wp

  if (degree >= 2L && !is.null(allowed_func)) {
    earth_args$allowed <- allowed_func
  }

  # Merge additional ... args
  dots <- list(...)
  earth_args <- c(earth_args, dots)

  # --- Fit model (with timing and trace capture) ---
  start_time <- proc.time()
  trace_output <- utils::capture.output({
    model <- do.call(earth::earth, earth_args)
  })
  elapsed <- as.numeric((proc.time() - start_time)["elapsed"])

  # --- Return structured result ---
  result <- list(
    model        = model,
    target       = target,
    predictors   = predictors,
    categoricals = if (is.null(categoricals)) character(0) else categoricals,
    linpreds     = if (is.null(linpreds)) character(0) else linpreds,
    degree       = degree,
    cv_enabled   = cv_enabled,
    data         = model_df,
    elapsed      = elapsed,
    trace_output = trace_output
  )
  class(result) <- "earthui_result"
  result
}
