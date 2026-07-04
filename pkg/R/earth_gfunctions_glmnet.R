#' Build G-Function Groups from Earth Model
#'
#' Groups earth's selected basis terms into g-functions by variable set,
#' using the same logic as earthUI's `format_model_equation()`.
#' Each group represents one variable (or interaction) and contains
#' the earth basis terms with their original coefficients.
#'
#' @param earth_import A `glmnetUI_earth_import` object.
#' @return A list of g-function groups, each with `label`, `base_vars`,
#'   `degree`, and `terms`.
#' @noRd
build_g_groups_ <- function(earth_import) {
  model <- earth_import$model
  categoricals <- earth_import$categoricals
  data <- earth_import$data

  dirs  <- model$dirs[model$selected.terms, , drop = FALSE]
  cuts  <- model$cuts[model$selected.terms, , drop = FALSE]
  coefs <- as.numeric(model$coefficients)
  col_names <- colnames(dirs)

  # Resolve dummy columns to base variables
  col_info <- resolve_earth_columns_(col_names, categoricals, data)

  # Build per-term metadata
  n_terms <- nrow(dirs)
  term_list <- vector("list", n_terms)

  for (i in seq_len(n_terms)) {
    active_cols <- which(dirs[i, ] != 0)

    if (length(active_cols) == 0L) {
      term_list[[i]] <- list(
        index       = i,
        coefficient = coefs[i],
        components  = list(),
        base_vars   = character(0),
        var_set_key = "(Intercept)",
        degree      = 0L
      )
      next
    }

    components <- lapply(active_cols, function(j) {
      list(
        col_name  = col_names[j],
        base_var  = col_info$base_var[j],
        level     = col_info$level[j],
        is_factor = col_info$is_factor[j],
        dir       = dirs[i, j],
        cut       = cuts[i, j]
      )
    })

    base_vars <- sort(unique(vapply(components, `[[`, character(1), "base_var")))

    term_list[[i]] <- list(
      index       = i,
      coefficient = coefs[i],
      components  = components,
      base_vars   = base_vars,
      var_set_key = paste(base_vars, collapse = "_"),
      degree      = length(base_vars)
    )
  }

  # Group terms by variable set
  keys <- vapply(term_list, `[[`, character(1), "var_set_key")
  unique_keys <- unique(keys)

  groups <- lapply(unique_keys, function(k) {
    members <- term_list[keys == k]
    list(
      var_set_key = k,
      degree      = members[[1]]$degree,
      base_vars   = members[[1]]$base_vars,
      label       = if (members[[1]]$degree == 0L) "Basis"
                    else paste(members[[1]]$base_vars, collapse = " "),
      terms       = members
    )
  })

  # Sort by degree
  group_degrees <- vapply(groups, `[[`, integer(1), "degree")
  groups[order(group_degrees)]
}


#' Resolve Earth Dummy Column Names to Base Variables
#'
#' Maps column names like `area_id471` to base variable `area_id`
#' with level `471`.
#'
#' @param col_names Character vector of earth model column names.
#' @param categoricals Character vector of categorical variable names.
#' @param data Data frame used to fit the model.
#' @return Data frame with columns: col_name, base_var, level, is_factor.
#' @noRd
resolve_earth_columns_ <- function(col_names, categoricals, data) {
  info <- data.frame(
    col_name  = col_names,
    base_var  = col_names,
    level     = NA_character_,
    is_factor = FALSE,
    stringsAsFactors = FALSE
  )

  if (length(categoricals) == 0L) return(info)

  categoricals <- categoricals[order(-nchar(categoricals))]

  for (cat_var in categoricals) {
    if (!cat_var %in% names(data)) next
    factor_col <- data[[cat_var]]
    lvls <- if (is.factor(factor_col)) levels(factor_col) else
      sort(unique(as.character(factor_col)))

    for (lvl in lvls) {
      lvl_str <- as.character(lvl)
      clean_lvl <- gsub("[[:space:]]+", ".", lvl_str)
      candidates <- unique(c(
        paste0(cat_var, lvl_str),
        paste0(cat_var, ".", lvl_str),
        paste0(cat_var, clean_lvl),
        paste0(cat_var, ".", clean_lvl)
      ))
      for (cand in candidates) {
        idx <- which(info$col_name == cand & !info$is_factor)
        if (length(idx) == 1L) {
          info$base_var[idx]  <- cat_var
          info$level[idx]     <- lvl_str
          info$is_factor[idx] <- TRUE
          break
        }
      }
    }
  }

  # Fallback: unresolved columns that look like factor dummies
  actual_cols <- names(data)
  for (i in which(!info$is_factor)) {
    cn <- info$col_name[i]
    if (cn %in% actual_cols) next
    for (cat_var in categoricals) {
      if (cat_var %in% actual_cols &&
          nchar(cn) > nchar(cat_var) &&
          startsWith(cn, cat_var)) {
        info$base_var[i] <- cat_var
        remainder <- substring(cn, nchar(cat_var) + 1L)
        if (startsWith(remainder, ".")) remainder <- substring(remainder, 2L)
        info$level[i] <- remainder
        info$is_factor[i] <- TRUE
        break
      }
    }
  }

  info
}


#' Compute Earth G-Function Contributions for RCA
#'
#' Uses earth's original coefficients and g-function grouping to
#' compute per-variable contributions. Only includes terms whose
#' corresponding basis columns have non-zero glmnet coefficients.
#'
#' @param earth_import A `glmnetUI_earth_import` object.
#' @param data Data frame to evaluate on.
#' @param glmnet_coefs Named numeric vector of glmnet coefficients
#'   (excluding intercept). Names must match `build_earth_basis()`
#'   column names. Used to determine which basis terms survived
#'   regularization. Pass NULL to include all earth terms.
#' @return Named list of contribution vectors, one per g-function
#'   (keyed by variable label like "living_sqft" or "beds_total lot_size").
#' @export
compute_earth_contributions <- function(earth_import, data,
                                        glmnet_coefs = NULL) {
  groups <- build_g_groups_(earth_import)
  model <- earth_import$model

  # Map earth basis column names to selected term indices
  # so we can filter by glmnet's selection
  bx <- build_earth_basis(NULL, earth_import)
  bx_colnames <- if (!is.null(bx)) colnames(bx) else character(0)

  # Build a set of selected term indices from glmnet's non-zero coefs
  selected_terms <- NULL
  if (!is.null(glmnet_coefs) && length(bx_colnames) > 0) {
    # Map each basis column to its earth term index
    sel <- model$selected.terms
    dirs <- model$dirs[sel, , drop = FALSE]
    # bx columns correspond to terms 2..n_terms (intercept excluded)
    # term_indices[j] = which selected term produced basis column j
    nonzero_basis <- which(glmnet_coefs[bx_colnames] != 0)
    # Basis columns correspond to selected terms 2, 3, ... (skip intercept)
    selected_terms <- nonzero_basis + 1L  # offset by 1 for intercept
  }

  contribs <- list()
  intercept <- 0

  for (grp in groups) {
    if (grp$degree == 0L) {
      # Intercept
      intercept <- grp$terms[[1]]$coefficient
      next
    }

    if (!is.null(selected_terms)) {
      # Filter group to only terms selected by glmnet
      keep_terms <- Filter(function(t) t$index %in% selected_terms, grp$terms)
      if (length(keep_terms) == 0L) next
      filtered_grp <- grp
      filtered_grp$terms <- keep_terms
    } else {
      filtered_grp <- grp
    }

    label <- gsub(" ", "_", filtered_grp$label)
    contrib <- eval_g_function_(model, filtered_grp, data)
    contribs[[label]] <- contrib
  }

  attr(contribs, "intercept") <- intercept
  contribs
}
