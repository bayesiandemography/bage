
## HAS_TESTS
#' Derive default prior from name and length of term
#'
#' @param nm_term Name of model term
#' @param var_age Name of age variable, or NULL
#' @param var_time Name of time variable, or NULL
#' @param length_effect Number of elements in term
#'
#' @returns A list of objects of class "bage_prior"
#'
#' @noRd
default_prior <- function(nm_term, var_age, var_time, length_effect) {
  is_length_le_2 <- length_effect <= 2L
  nm_term_split <- strsplit(nm_term, split = ":")[[1L]]
  if (is.null(var_age))
    is_age <- FALSE
  else
    is_age <- var_age %in% nm_term_split
  if (is.null(var_time))
    is_time <- FALSE
  else
    is_time <- var_time %in% nm_term_split
  if (is_length_le_2)
    return(NFix())
  if (is_age || is_time)
    return(RW())
  N()
}


## HAS_TESTS
#' Convert Dimnames for a Term to Labels
#'
#' Handles intercept correctly.
#'
#' @param dimnames Named list of labels
#'
#' @returns A character vector
#'
#' @noRd
dimnames_to_levels <- function(dimnames) {
  if (length(dimnames) > 0L) {
    dimnames <- lapply(dimnames, as.character)
    ans <- expand.grid(dimnames,
                       KEEP.OUT.ATTRS = FALSE,
                       stringsAsFactors = FALSE)
    ans <- Reduce(paste_dot, ans)
  }
  else
    ans <- "(Intercept)"
  ans
}


## HAS_TESTS
#' Convert Dimnames for a Term to the Name of the Term
#'
#' Handles intercept correctly.
#' 
#' @param dimnames Named list of labels
#'
#' @returns A string
#'
#' @noRd
dimnames_to_nm <- function(dimnames) {
  ans <- dimnames_to_nm_split(dimnames)
  ans <- paste(ans, collapse = ":")
  ans
}


## HAS_TESTS
#' Convert Dimnames for a Term to the Names of the Dimensions
#'
#' Handles intercept correctly.
#' 
#' @param dimnames Named list of labels
#'
#' @returns A string
#'
#' @noRd
dimnames_to_nm_split <- function(dimnames) {
  if (length(dimnames) > 0L)
    names(dimnames)
  else
    "(Intercept)"
}
  

## HAS_TESTS
#' Evaluate Formula to Create Offset
#'
#' @param vname_offset Formula passed by user, turned into a string
#' @param data Data frame
#'
#' @returns A numeric vector
#'
#' @noRd
eval_offset_formula <- function(vname_offset, data) {
  vname_offset <- sub("^~", "", vname_offset)
  vname_offset <- parse(text = vname_offset)
  eval(vname_offset, envir = data)
}


## HAS_TESTS
#' Get Number of Components of Spline Prior
#'
#' If user did not supply number, derive one
#' from length of 'along' dimension.
#'
#' @param prior Object of class 'bage_prior'
#' @param n_along Length of 'along' dimension
#'
#' @returns An integer
#'
#' @noRd
get_n_comp_spline <- function(prior, n_along) {
  n_comp <- prior$specific$n_comp
  if (is.null(n_comp)) {
    n_comp <- 0.7 * n_along
    n_comp <- ceiling(n_comp)
    n_comp <- as.integer(n_comp)
    n_comp <- max(n_comp, 4L)
  }
  n_comp
}


## HAS_TESTS
#' Get Offset Used When Printing Priors
#'
#' @returns An integer
#'
#' @noRd
get_print_prior_n_offset <- function() 8L



## HAS_TESTS
#' Infer the name of the age variable
#'
#' Given a formula, guess which term, if
#' any, is the age variable.
#'
#' @param formula Model formula
#'
#' @returns A string or NULL.
#' 
#' @noRd
infer_var_age <- function(formula) {
    factors <- attr(stats::terms(formula), "factors")
    has_no_terms <- identical(factors, integer())
    if (has_no_terms)
        return(NULL)
    factors <- factors[-1L, , drop = FALSE]
    nms <- rownames(factors)
    poputils::find_var_age(nms)
}


## HAS_TESTS
#' Infer the name of the sex or gender variable
#'
#' Given a formula, guess which term, if
#' any, is the age variable.
#'
#' @param formula Model formula
#'
#' @returns A string or NULL.
#' 
#' @noRd
infer_var_sexgender <- function(formula) {
    factors <- attr(stats::terms(formula), "factors")
    has_no_terms <- identical(factors, integer())
    if (has_no_terms)
        return(NULL)
    factors <- factors[-1L, , drop = FALSE]
    nms <- rownames(factors)
    poputils::find_var_sexgender(nms)
}


## HAS_TESTS
#' Infer the name of the time variable
#'
#' Given a formula, guess which term, if
#' any, is the time variable.
#'
#' @param formula Model formula
#'
#' @returns A string or NULL.
#' 
#' @noRd
infer_var_time <- function(formula) {
    factors <- attr(stats::terms(formula), "factors")
    has_no_terms <- identical(factors, integer())
    if (has_no_terms)
        return(NULL)
    factors <- factors[-1L, , drop = FALSE]
    nms <- rownames(factors)
    poputils::find_var_time(nms)
}


## HAS_TESTS
#' Classify a Term, Based on the Name
#'
#' Decide whether an intercept, main effect,
#' or interaction is
#' - an age main effect
#' - an interaction involving age and sex/gender only
#' - an interaction involving age and other (non sex/gender) dimensions
#' - an interaction involvling age, sex/gender and other dimensions
#' - anything else
#' based on the name of the term,
#' plus 'var_age' and 'var_sexgender'.
#' In cases involving age and sex, the return value
#' "age:sex" implies that age comes first,
#' and "sex:age" implies that sex comes first.
#'
#' @param nm Name of the term. A string.
#' WARNING. This must be the name that
#' is used internally, which is not necessarily
#' the one that appears in the original
#' formula, as stats::terms() and friends
#' switch dimension order.
#' @param var_age Name of the age variable. A string.
#' @param var_sexgender Name of the sex/gender
#' variable. A string.
#'
#' @returns A string. One of "age", "age:sex",
#' "sex:age", "age:other", "age:sex:other",
#' "sex:age:other", "other"
#'
#' @noRd
make_agesex <- function(nm, var_age, var_sexgender) {
  nm_split <- strsplit(nm, split = ":")[[1L]]
  has_var_age <- !is.null(var_age)
  has_var_sex <- !is.null(var_sexgender)
  is_age_in_nm <- has_var_age && var_age %in% nm_split
  is_sex_in_nm <- has_var_sex && var_sexgender %in% nm_split
  age_first <- match(var_age, nm_split) < match(var_sexgender, nm_split)
  if (!is_age_in_nm)
    return("other")
  n <- length(nm_split)
  if (n == 1L) {
    "age"
  }
  else if (n == 2L) {
    if (is_sex_in_nm) {
      if (age_first)
        "age:sex"
      else
        "sex:age"
    }
    else
      "age:other"
  }
  else {
    if (is_sex_in_nm) {
      if (age_first)
        "age:sex:other"
      else
        "sex:age:other"
    }
    else
      "age:other"
  }
}


## HAS_TESTS
#' Make 'const'
#'
#' Make vector to hold real-valued constants for priors.
#'
#' We generate 'const' when function 'fit'
#' is called, rather than storing it in the
#' 'bage_mod' object, to avoid having to update
#' it when priors change  via 'set_prior'.
#' 
#' @param mod Object of class "bage_mod"
#'
#' @returns A vector of doubles.
#'
#' @noRd
make_const <- function(mod) {
    priors <- mod$priors
    ans <- lapply(priors, const)
    ans <- unlist(ans)
    ans
}


## HAS_TESTS
#' Make Dimnames for Terms in Model
#'
#' @param mod Object of class 'bage_mod'
#'
#' @returns A named list of lists
#'
#' @noRd
make_dimnames_terms <- function(formula, data) {
  ans <- list("(Intercept)" = list())
  factors <- attr(stats::terms(formula), "factors")
  if (length(factors) > 0L) {
    factors <- factors[-1L, , drop = FALSE]
    factors <- factors > 0L
    nms_vars <- rownames(factors)
    nms_terms <- colnames(factors)
    ans_terms <- vector(mode = "list", length = length(nms_terms))
    for (i_term in seq_along(nms_terms)) {
      nms_vars_term <- nms_vars[factors[, i_term]]
      data_term <- data[nms_vars_term]
      data_term <- lapply(data_term, to_factor)
      dimnames <- lapply(data_term, levels)
      ans_terms[[i_term]] <- dimnames
    }
    names(ans_terms) <- nms_terms
    ans <- c(ans, ans_terms)
  }
  ans
}


## HAS_TESTS
#' Make vector containing parameters for
#' intercept, main effects, and interactions
#'
#' Return value is 0 where a parameter is being estimated,
#' and potentially non-zero where a parameter is
#' being treated as known.
#'
#' @param mod Object of class "bage_mod"
#'
#' @returns A vector of doubles.
#'
#' @noRd
make_effectfree <- function(mod) {
    priors <- mod$priors
    lengths_effectfree <- make_lengths_effectfree(mod)
    ans <- lapply(lengths_effectfree, function(n) rep(0, times = n))
    for (i_term in seq_along(priors)) {
        prior <- priors[[i_term]]
        is_known <- is_known(prior)
        if (is_known) {
            values <- values_known(prior)
            ans[[i_term]] <- values
        }
    }
    ans <- unlist(ans)
    names(ans) <- rep(names(priors), times = lengths_effectfree)
    ans
}


## HAS_TESTS
#' Make 'hyper'
#'
#' Make vector to hold hyper-parameters
#' for priors.
#'
#' We generate 'hyper' when function 'fit'
#' is called, rather than storing it in the
#' 'bage_mod' object, to avoid having to update
#' it when priors change  via 'set_prior'.
#' 
#' @param mod Object of class "bage_mod"
#'
#' @returns A vector of zeros, of type 'double'.
#'
#' @noRd
make_hyper <- function(mod) {
    priors <- mod$priors
    ans <- rep(0, times = length(priors))
    names(ans) <- names(priors)
    lengths <- make_lengths_hyper(mod)
    ans <- rep(ans, times = lengths)
    ans
}


## HAS_TESTS
#' Make 'hyperrand'
#'
#' Make Vector to Hold Hyper-Parameters
#' for Priors that can be Treated as Random Effects.
#'
#' We generate 'hyperrand' when function 'fit'
#' is called, rather than storing it in the
#' 'bage_mod' object, to avoid having to update
#' it when priors change  via 'set_prior'.
#' 
#' @param mod Object of class "bage_mod"
#'
#' @returns A vector of zeros, of type 'double'.
#'
#' @noRd
make_hyperrand <- function(mod) {
  priors <- mod$priors
  ans <- rep(0, times = length(priors))
  names(ans) <- names(priors)
  lengths <- make_lengths_hyperrand(mod)
  ans <- rep(ans, times = lengths)
  ans
}


## HAS_TESTS
#' Make 'i_prior'
#'
#' Make 'i_prior' a vector of integers used to
#' choose appropriate 'logprior' function
#' in the TMB template
#'
#' We generate 'i_prior' when function 'fit'
#' is called, rather than storing it in the
#' 'bage_mod' object, to avoid having to update
#' it when priors change  via 'set_prior'.
#' 
#' @param mod Object of class "bage_mod"
#'
#' @returns An named integer vector.
#'
#' @noRd
make_i_prior <- function(mod) {
    priors <- mod$priors
    vapply(priors, function(x) x$i_prior, 0L)
}


## HAS_TESTS
#' Make vector of indicators showing whether
#' cell contributes to likelihood
#'
#' @param mod Object of class "bage_mod"
#'
#' @returns A vector of 1Ls and 0Ls.
#'
#' @noRd
make_is_in_lik <- function(mod) {
    outcome <- mod$outcome
    offset <- mod$offset
    ans <- (!is.na(outcome)
        & !is.na(offset)
        & (offset > 0))
    as.integer(ans)
}


## HAS_TESTS
#' Lengths of vectors of parameters
#' 
#' @param matrices_effect_outcome Named list of Matrix matrices
#'
#' @returns A named integer vector.
#'
#' @noRd
make_lengths_effect <- function(matrices_effect_outcome) {
    matrices <- lapply(matrices_effect_outcome, Matrix::as.matrix)
    vapply(matrices, ncol, 1L)
}


## HAS_TESTS
#' Lengths of vectors of free parameters
#' 
#' @param mod Object of class "bage_mod"
#'
#' @returns A named integer vector.
#'
#' @noRd
make_lengths_effectfree <- function(mod) {
    priors <- mod$priors
    matrices <- make_matrices_effectfree_effect(mod)
    matrices <- lapply(matrices, Matrix::as.matrix)
    ans <- vapply(matrices, ncol, 1L)
    names(ans) <- names(priors)
    ans
}


## HAS_TESTS
#' Lengths of Vectors of Ordinary Hyper-Parameters
#'
#' @param mod Object of class "bage_mod"
#'
#' @returns A named integer vector
#'
#' @noRd
make_lengths_hyper <- function(mod) {
  priors <- mod$priors
  levels <- lapply(priors, levels_hyper)
  ans <- lengths(levels)
  names(ans) <- names(priors)
  ans
}


## HAS_TESTS
#' Lengths of Vectors of Hyper-Parameters that Can
#' be Treated as Random Effects
#'
#' @param mod Object of class "bage_mod"
#'
#' @returns A named integer vector
#'
#' @noRd
make_lengths_hyperrand <- function(mod) {
  priors <- mod$priors
  var_time <- mod$var_time
  var_age <- mod$var_age
  dimnames_terms <- mod$dimnames_terms
  levels_effect <- mod$levels_effect
  terms_effect <- mod$terms_effect
  levels_effect <- split(levels_effect, terms_effect)
  levels <- .mapply(levels_hyperrand,
                    dots = list(prior = priors,
                                dimnames_term = dimnames_terms,
                                levels_effect = levels_effect),
                    MoreArgs = list(var_time = var_time,
                                    var_age = var_age))
  ans <- lengths(levels)
  names(ans) <- names(priors)
  ans
}


## HAS_TESTS
#' Extract Age Labels
#'
#' @param mod Object of class 'bage_mod'
#'
#' @returns A character vector or NULL
#'
#' @noRd
make_levels_age <- function(mod) {
  data <- mod$data
  var_age <- mod$var_age
  if (is.null(var_age))
    NULL
  else
    unique(data[[var_age]])
}


## HAS_TESTS
#' Make levels associated with each element of 'effect'
#'
#' Make levels for each term, eg ages, times.
#' 'make_levels_effect' works with the matrices
#' used to map levels to the outcome, to
#' ensure that the levels are correct (rather than
#' relying on undocumented properties of 'xtabs' etc),
#'
#' @param matrices_effect_outcome List of matrices mapping
#'
#' @returns A character vector.
#'
#' @noRd
make_levels_effect <- function(matrices_effect_outcome) {
  ans <- lapply(matrices_effect_outcome, colnames)
  nms <- names(matrices_effect_outcome)
  if (identical(nms[[1L]], "(Intercept)"))
    ans[[1L]] <- "(Intercept)"
  ans <- unlist(ans, use.names = FALSE)
  ans
}


## HAS_TESTS
#' Make Levels for a Forecasted Terms
#'
#' Make levels for each main effect and interaction
#' that is forecasted (ie that has a time dimension.)
#'
#' @param mod Object of class 'bage_mod'
#' @param labels_forecast Character vector
#' with labels for time dimension during forecast
#'
#' @returns A list of NULLs (for terms not forecasted)
#' and character vectors (for terms forecasted)
#'
#' @noRd
make_levels_forecast_all <- function(mod, labels_forecast) {
  formula <- mod$formula
  data <- mod$data
  var_time <- mod$var_time
  ans <- list("(Intercept)" = NULL)
  factors <- attr(stats::terms(formula), "factors")
  if (length(factors) > 0L) {
    factors <- factors[-1L, , drop = FALSE]
    factors <- factors > 0L
    nms_vars <- rownames(factors)
    nms_terms <- colnames(factors)
    ans_terms <- rep(list(NULL), times = length(nms_terms))
    names(ans_terms) <- nms_terms
    for (i_term in seq_along(nms_terms)) {
      nms_vars_term <- nms_vars[factors[, i_term]]
      i_time <- match(var_time, nms_vars_term, nomatch = 0L)
      if (i_time > 0L) {
        data_term <- data[nms_vars_term]
        dimnames <- lapply(data_term, unique)
        dimnames[[i_time]] <- labels_forecast
        levels <- expand.grid(dimnames, KEEP.OUT.ATTRS = FALSE)
        levels <- Reduce(paste_dot, levels)
        levels <- as.character(levels)
        ans_terms[[i_term]] <- levels
      }
    }
    ans <- c(ans, ans_terms)
  }
  ans
}


## HAS_TESTS
#' Extract Sex/Gender Labels
#'
#' @param mod Object of class 'bage_mod'
#'
#' @returns A character vector or NULL
#'
#' @noRd
make_levels_sexgender <- function(mod) {
  data <- mod$data
  var_sexgender <- mod$var_sexgender
  if (is.null(var_sexgender))
    NULL
  else
    unique(data[[var_sexgender]])
}


## HAS_TESTS
#' Make mapping used by MakeADFun
#'
#' Make 'map' argument to be passed to MakeADFun.
#' Return value is non-NULL if
#' (i) any priors are "bage_prior_known", or
#' (ii) 'mean_disp' is 0, or
#'
#' @param mod Object of class "bage_mod"
#'
#' @returns NULL or a named list
#'
#' @noRd
make_map <- function(mod) {
    priors <- mod$priors
    mean_disp <- mod$mean_disp
    ## determine whether any parameters fixed
    is_known <- vapply(priors, is_known, FALSE)
    is_effectfree_fixed <- any(is_known)
    is_disp_fixed <- mean_disp == 0
    ## return NULL if nothing fixed
    if (!is_effectfree_fixed && !is_disp_fixed)
        return(NULL)
    ## otherwise construct named list
    ans <- list()
    if (is_effectfree_fixed)
      ans$effectfree <- make_map_effectfree_fixed(mod)
    if (is_disp_fixed)
        ans$log_disp <- factor(NA)
    ans
}


## HAS_TESTS
#' Make 'effectfree' component of 'map'
#' argument to MakeADFun
#'
#' Only called when model has Known prior.
#'
#' @param mod Object of class "bage_mod"
#'
#' @returns A factor vector of NAs
#'
#' @noRd
make_map_effectfree_fixed <- function(mod) {
    priors <- mod$priors
    is_known <- vapply(priors, is_known, FALSE)
    lengths <- make_lengths_effectfree(mod)
    ans <- ifelse(is_known, NA, 0L)
    ans <- rep(ans, times = lengths)
    n <- length(ans)
    is_na <- is.na(ans)
    n_na <- sum(is_na)
    ans[!is_na] <- seq_len(n - n_na)
    ans <- factor(ans)
    ans
}


## HAS_TESTS
#' Convert 'matrix_agesex' or 'matrix_along_by'
#' to Sparse Index Matrix
#'
#' @param m Matrix produced by
#' 'make_matrix_agesex' or 'make_matrix_along_by'
#'
#' @returns A sparse matrix consisting of
#' 1s and 0s
#'
#' @noRd
make_index_matrix <- function(m) {
  n <- length(m)
  i <- as.integer(m) + 1L
  j <- seq_len(n)
  x <- rep.int(1L, times = n)
  ans <- Matrix::sparseMatrix(i = i,
                              j = j,
                              x = x)
  rn_old <- rownames(m)
  cn_old <- colnames(m)
  if (is.null(cn_old))
    cn_new <- rep(rn_old, times = ncol(m))
  else
    cn_new <- paste(rn_old, rep(cn_old, each = length(rn_old)), sep = ".")
  rn_new <- cn_new[match(seq_len(n), m + 1L)]
  dimnames(ans) <- list(rn_new, cn_new)
  ans
}


## HAS_TESTS
#' Make Matrices Mapping Free Parameters to Along and By Dimensions
#'
#' Similar to 'matrix_along_by', but only to free parameters
#' (not to whole effect.) Differs in case of spline and SVD priors.
#' 
#' @param mod Object of class 'bage_mod'
#'
#' @returns A named list of matrices
#'
#' @noRd
make_matrices_along_by_effectfree <- function(mod) {
  priors <- mod$priors
  var_time <- mod$var_time
  var_age <- mod$var_age
  var_sexgender <- mod$var_sexgender
  dimnames_terms <- mod$dimnames_terms
  ans <- .mapply(make_matrix_along_by_effectfree,
                 dots = list(prior = priors,
                             dimnames_term = dimnames_terms),
                 MoreArgs = list(var_time = var_time,
                                 var_age = var_age,
                                 var_sexgender = var_sexgender))
  names(ans) <- names(priors)
  ans
}


## HAS_TESTS
#' Make List of Matrices Mapping Values
#' of 'along' and 'by' to Positions
#' in Main Effects and Interactions for Forecasts
#'
#' If a term does not include time,
#' return NULL for that term
#' (since these are the only terms that are
#' expanded during forecasting.)
#'
#' If a term has time, assume that time is
#' the "along" dimension. (Assume that
#' 'check_along_is_time' has been called
#' on 'mod'.)
#'
#' @param mod Object of class 'bage_mod'
#' @param labels_forecast Character vector with
#' labels for future time periods.
#'
#' @returns A list containing NULLs and matrices.
#'
#' @noRd
make_matrices_along_by_forecast <- function(mod, labels_forecast) {
  formula <- mod$formula
  data <- mod$data
  var_time <- mod$var_time
  ans <- list("(Intercept)" = NULL)
  factors <- attr(stats::terms(formula), "factors")
  if (length(factors) > 1L) {
    factors <- factors[-1L, , drop = FALSE]
    factors <- factors > 0L
    nms_vars <- rownames(factors)
    nms_terms <- colnames(factors)
    ans_terms <- vector(mode = "list", length = length(nms_terms))
    for (i_term in seq_along(nms_terms)) {
      nms_vars_term <- nms_vars[factors[, i_term]]
      i_time <- match(var_time, nms_vars_term, nomatch = 0L)
      has_time <- i_time > 0L
      if (has_time) {
        data_term <- data[nms_vars_term]
        data_term <- lapply(data_term, to_factor)
        dimnames <- lapply(data_term, levels)
        dimnames[[i_time]] <- labels_forecast
        dim <- lengths(dimnames)
        ans_terms[[i_term]] <- make_matrix_along_by(i_along = i_time,
                                                    dim = dim,
                                                    dimnames = dimnames)
      }
    }
    names(ans_terms) <- nms_terms
    ans <- c(ans, ans_terms)
  }
  ans
}


## HAS_TESTS
#' Make list of matrices mapping terms to outcome vector
#'
#' Make list of matrices mapping main effects or
#' interactions to vector holding outcome
#' (or, equivalently, the linear predictor.)
#' 
#' @param data Data frame
#' @param dimnames_terms Dimnames for array
#' representation of terms
#'
#' @returns A named list
#'
#' @noRd
make_matrices_effect_outcome <- function(data, dimnames_terms) {
  n_term <- length(dimnames_terms)
  ans <- vector(mode = "list", length = n_term)
  names(ans) <- names(dimnames_terms)
  ## make intercept
  n_data <- nrow(data)
  i <- seq_len(n_data)
  j <- rep.int(1L, times = n_data)
  x <- rep.int(1L, times = n_data)
  ans[[1L]] <- Matrix::sparseMatrix(i = i, j = j, x = x)
  ## make other terms
  if (n_term > 1L) {
    for (i_term in seq.int(from = 2L, to = n_term)) {
      dimnames_term <- dimnames_terms[[i_term]]
      nm_split <- dimnames_to_nm_split(dimnames_term)
      nm <- dimnames_to_nm(dimnames_term)
      data_term <- data[nm_split]
      data_term[] <- lapply(data_term, to_factor)
      contrasts_term <- lapply(data_term, stats::contrasts, contrast = FALSE)
      formula_term <- paste0("~", nm, "-1")
      formula_term <- stats::as.formula(formula_term)
      m_term <- Matrix::sparse.model.matrix(formula_term,
                                            data = data_term,
                                            contrasts.arg = contrasts_term,
                                            row.names = FALSE)
      colnames(m_term) <- dimnames_to_levels(dimnames_term)
      ans[[i_term]] <- m_term
    }
  }
  ans        
}



## HAS_TESTS
#' Make list of matrices mapping effectfree to effect
#'
#' Make list of matrices mapping free parameters
#' for main effects or interactions to
#' full parameter vectors
#' 
#' @param mod Object of class 'bage_mod'
#'
#' @returns A named list of matrices
#'
#' @noRd
make_matrices_effectfree_effect <- function(mod) {
  priors <- mod$priors
  dimnames_terms <- mod$dimnames_terms
  var_time <- mod$var_time
  var_age <- mod$var_age
  var_sexgender <- mod$var_sexgender
  ans <- .mapply(make_matrix_effectfree_effect,
                 dots = list(prior = priors,
                             dimnames_term = dimnames_terms),
                 MoreArgs = list(var_time = var_time,
                                 var_age = var_age,
                                 var_sexgender = var_sexgender))
  names(ans) <- names(priors)
  ans    
}


## HAS_TESTS
#' Create along-by Matrix for a Single Dimension
#' of a Main Effect or Interaction
#'
#' Create a matrix where entry (i,j) is
#' the position in an array (using
#' zero-based indexing) of 'along'
#' value i and 'by' value j.
#'
#' @param i_along Index of the along dimension(s)
#' @param dim Dimensions of the array
#'
#' @returns A matrix of integers.
#'
#' @noRd
make_matrix_along_by <- function(i_along, dim, dimnames) {
  n_dim <- length(dim)
  i <- seq.int(from = 0L, length.out = prod(dim))
  a <- array(i, dim = dim)
  s <- seq_along(dim)
  perm <- c(i_along, s[-i_along])
  ans <- aperm(a, perm = perm)
  ans <- matrix(ans, nrow = prod(dim[i_along]))
  rownames <- expand.grid(dimnames[i_along])
  rownames <- Reduce(paste_dot, rownames)
  rownames(ans) <- rownames
  names(dimnames(ans))[[1L]] <- paste(names(dimnames)[i_along], collapse = ".")
  if (length(dim) > 1L) {
    colnames <- expand.grid(dimnames[-i_along])
    colnames <- Reduce(paste_dot, colnames)
    colnames(ans) <- colnames
    names(dimnames(ans))[[2L]] <- paste(names(dimnames)[-i_along], collapse = ".")
  }
  ans
}


## HAS_TESTS
#' Make Matrix Mapping Effectfree to Effect for Priors with SVD
#'
#' Make matrices mapping free parameters
#' for main effects or interactions to
#' full parameter vectors for priors with SVDs
#' 
#' @param prior Object of class 'bage_prior'
#' @param dimnames_term Dimnames of array representation of term
#' @param var_time Name of time variable
#' @param var_age Name of age variable
#' @param var_sexgender Name of sex/gender variable
#'
#' @returns A sparse matrix.
#'
#' @noRd
make_matrix_effectfree_effect_svd <- function(prior,
                                              dimnames_term,
                                              var_time,
                                              var_age,
                                              var_sexgender) {
  ssvd <- prior$specific$ssvd
  indep <- prior$specific$indep
  n_comp <- prior$specific$n_comp
  nm <- dimnames_to_nm(dimnames_term)
  nm_split <- dimnames_to_nm_split(dimnames_term)
  has_age <- !is.null(var_age)
  has_sexgender <- !is.null(var_sexgender)
  agesex <- make_agesex(nm = nm,
                        var_age = var_age,
                        var_sexgender = var_sexgender)
  matrix_agesex <- make_matrix_agesex(dimnames_term = dimnames_term,
                                      var_age = var_age,
                                      var_sexgender = var_sexgender)
  n_by <- ncol(matrix_agesex) ## special meaning of 'by': excludes age and sex
  levels_age <- if (is.null(var_age)) NULL else dimnames_term[[var_age]]
  levels_sexgender <- if (is.null(var_sexgender)) NULL else dimnames_term[[var_sexgender]]
  levels_effect <- dimnames_to_levels(dimnames_term)
  if (has_sexgender) {
    term_has_sexgender <- var_sexgender %in% nm_split
    if (term_has_sexgender)
      joint <- !indep
    else
      joint <- NULL
  }
  else
    joint <- NULL
  m <- get_matrix_or_offset_svd(ssvd = ssvd,
                                levels_age,
                                levels_sexgender,
                                joint = joint,
                                agesex = agesex,
                                get_matrix = TRUE,
                                n_comp = n_comp)
  I <- Matrix::.sparseDiagonal(n_by)
  m_all_by <- Matrix::kronecker(I, m)
  agesex_to_standard <- make_index_matrix(matrix_agesex)
  agesex_to_standard %*% m_all_by
}


## HAS_TESTS
#' Make Offset used in converting effectfree to effect for SVD Priors
#'
#' @param prior Object of class 'bage_prior'
#' @param dimnames_term Dimnames of array representation of term
#' @param var_time Name of time variable
#' @param var_age Name of age variable
#' @param var_sexgender Name of sex/gender variable
#'
#' @returns A vector.
#'
#' @noRd
make_offset_effectfree_effect_svd <- function(prior,
                                              dimnames_term,
                                              var_time,
                                              var_age,
                                              var_sexgender) {
  ssvd <- prior$specific$ssvd
  indep <- prior$specific$indep
  n_comp <- prior$specific$n_comp
  nm <- dimnames_to_nm(dimnames_term)
  nm_split <- dimnames_to_nm_split(dimnames_term)
  has_age <- !is.null(var_age)
  has_sexgender <- !is.null(var_sexgender)
  agesex <- make_agesex(nm = nm,
                        var_age = var_age,
                        var_sexgender = var_sexgender)
  matrix_agesex <- make_matrix_agesex(dimnames_term = dimnames_term,
                                      var_age = var_age,
                                      var_sexgender = var_sexgender)
  n_by <- ncol(matrix_agesex)  ## special meaning of 'n_by': excludes age and sex
  levels_age <- if (has_age) dimnames_term[[var_age]] else NULL
  levels_sexgender <- if (has_sexgender) dimnames_term[[var_sexgender]] else NULL
  levels_effect <- dimnames_to_levels(dimnames_term)
  if (has_sexgender) {
    term_has_sexgender <- var_sexgender %in% nm_split
    if (term_has_sexgender)
      joint <- !indep
    else
      joint <- NULL
  }
  else
    joint <- NULL
  b <- get_matrix_or_offset_svd(ssvd = ssvd,
                                levels_age,
                                levels_sexgender,
                                joint = joint,
                                agesex = agesex,
                                get_matrix = FALSE,
                                n_comp = n_comp)
  ones <- Matrix::sparseMatrix(i = seq_len(n_by),
                               j = rep.int(1L, times = n_by),
                               x = rep.int(1L, times = n_by))
  b_all_by <- Matrix::kronecker(ones, b)
  agesex_to_standard <- make_index_matrix(matrix_agesex)
  ans <- agesex_to_standard %*% b_all_by
  ans <- Matrix::drop(ans)
  names(ans) <- levels_effect
  ans
}


## HAS_TESTS
#' Make vector holding offset variable
#'
#' @param vname_offset Name of the offset variable.
#' @param data A data frame
#'
#' @returns An vector of doubles.
#'
#' @noRd
make_offset <- function(vname_offset, data) {
  is_offset_formula <- startsWith(vname_offset, "~")
  if (is_offset_formula)
    ans <- eval_offset_formula(vname_offset = vname_offset, data = data)
  else
    ans <- data[[vname_offset]]
  ans <- as.double(ans)
  ans
}


## HAS_TESTS
#' Make offset consisting entirely of 1s,
#' the same length as outcome
#'
#' @param data Data frame
#'
#' @returns Vector of doubles.
#'
#' @noRd
make_offset_ones <- function(data) {
    rep(1.0, times = nrow(data))
}


## HAS_TESTS
#' Make combined vector offsets using in converting
#' effectfree to effect
#'
#' Make combined vector of offsets used in converting free parameters
#' for main effects or interactions to
#' full parameter vectors. Note that in TMB itself,
#' the combined vector is split into pieces using
#' terms_effectfree.
#' 
#' @param mod Object of class 'bage_mod'
#'
#' @returns A named vector of doubles.
#'
#' @noRd
make_offsets_effectfree_effect <- function(mod) {
  priors <- mod$priors
  dimnames_terms <- mod$dimnames_terms
  var_time <- mod$var_time
  var_age <- mod$var_age
  var_sexgender <- mod$var_sexgender
  ans <- .mapply(make_offset_effectfree_effect,
                 dots = list(prior = priors,
                             dimnames_term = dimnames_terms),
                 MoreArgs = list(var_time = var_time,
                                 var_age = var_age,
                                 var_sexgender = var_sexgender))
  names(ans) <- names(priors)
  ans    
}


## HAS_TESTS
#' Make vector holding outcome variable
#'
#' Extracts the outcome variable from 'data'.
#'
#' @param formula Formula specifying model
#' @param data A data frame
#'
#' @returns A vector of doubles.
#'
#' @noRd
make_outcome <- function(formula, data) {
    nm_response <- deparse1(formula[[2L]])
    nms_data <- names(data)
    ans <- data[[match(nm_response, nms_data)]]
    ans <- as.double(ans)
    ans
}


## HAS_TESTS
#' Make default priors
#'
#' Make named list holding default priors.
#' Default prior is N(0, 1) for
#' all terms except
#' - intercept, where it is N(0, 10^2),
#' - age variable, where it is RW()
#' - time variable, where it is RW()
#'
#' @param formula Formula specifying model
#' @param var_age Name of age variable, or NULL
#' @param var_time Name of time variable, or NULL
#' @param lengths_effect Number of elements in each term
#'
#' @returns Named list of objects with class
#' 'bage_prior'.
#'
#' @noRd
make_priors <- function(formula, var_age, var_time, lengths_effect) {
    nms_terms <- attr(stats::terms(formula), "term.labels")
    has_intercept <- attr(stats::terms(formula), "intercept")
    if (has_intercept)
        nms_terms <- c("(Intercept)", nms_terms)
    ans <- .mapply(default_prior,
                   dots = list(nm_term = nms_terms,
                               length_effect = lengths_effect),
                   MoreArgs = list(var_age = var_age,
                                   var_time = var_time))
    names(ans) <- nms_terms
    ans
}        


## HAS_TESTS
#' Make 'random' argument to MakeADFun function
#'
#' Return value always includes "effectfree".
#'
#' @param mod Object of class "bage_mod"
#'
#' @returns A character vector
#'
#' @noRd
make_random <- function(mod) {
  priors <- mod$priors
  has_hyper <- any(make_lengths_hyper(mod) > 0L)
  has_hyperrand <- any(vapply(priors, has_hyperrand, FALSE))
  if (!has_hyper && !has_hyperrand)
    ans <- NULL
  else {
    ans <- "effectfree"
    if (has_hyperrand)
      ans <- c(ans, "hyperrand")
  }
  ans
}


## HAS_TESTS
#' Randomly Generate an Integer Suitable for
#' Using as a Random Seed
#'
#' @returns An integer between 1 and max integer.
#'
#' @noRd
make_seed <- function()
    sample.int(n = .Machine$integer.max, size = 1L)


## HAS_TESTS
#' Make a matrix of B-spline basis functions
#'
#' Based on Eilers and Marx (1996). Flexible Smoothing
#' with B-splines and Penalties.
#' Statistical Science, 11(2), 89-121.
#'
#' @param n_along Number of elements of dimension being modelled
#' @param n_comp Number of columns in spline matrix
#'
#' @returns Matrix with 'n_along' rows and 'n_comp' columns
#'
#' @noRd
make_spline_matrix <- function(n_along, n_comp) {
  n_interval <- n_comp - 3L
  interval_length <- (n_along - 1L) / n_interval
  start <- 1 - 3 * interval_length
  end <- n_along + 3 * interval_length
  x <- seq(from = start, to = end, by = 0.001)
  base <- splines::bs(x = x, df = n_comp + 5L)
  i_keep <- findInterval(seq_len(n_along), x)
  j_keep <- seq.int(from = 3L, length.out = n_comp)
  ans <- base[i_keep, j_keep]
  colmeans <- colMeans(ans)
  ans <- ans - rep(colmeans, each = nrow(ans))
  Matrix::sparseMatrix(i = row(ans),
                       j = col(ans),
                       x = as.double(ans))
}


## HAS_TESTS
#' Make factor identifying components of 'const'
#'
#' Make factor the same length as 'const',
#' giving the name of the term
#' that the each element belongs to.
#' Note that the levels of the factor
#' includes all priors, not just those
#' with constants.
#'
#' We generate 'terms_const' when function 'fit'
#' is called, rather than storing it in the
#' 'bage_mod' object, to avoid having to update
#' it when priors change  via 'set_prior'.
#'
#' @param mod Object of class "bage_mod"
#'
#' @returns A factor, the same length
#' as 'const'.
#'
#' @noRd
make_terms_const <- function(mod) {
    priors <- mod$priors
    nms_terms <- names(priors)
    consts <- lapply(priors, const)
    lengths <- lengths(consts)
    ans <- rep(nms_terms, times = lengths)
    ans <- factor(ans, levels = nms_terms)
    ans
}


## HAS_TESTS
#' Make factor identifying components of
#' combined parameter vector
#'
#' Make factor the same length as
#' a combined parameter vector
#' giving the name of the term
#' that the each element belongs to.
#'
#' @param matrices_effect_outcome Named list of Matrix objects
#'
#' @returns A factor.
#'
#' @noRd
make_terms_effect <- function(matrices_effect_outcome) {
    nms <- names(matrices_effect_outcome)
    matrices_effect_outcome <- lapply(matrices_effect_outcome, Matrix::as.matrix)
    lengths <- vapply(matrices_effect_outcome, ncol, 1L)
    ans <- rep(nms, times = lengths)
    ans <- factor(ans, levels = nms)
    ans
}


## HAS_TESTS
#' Make factor identifying components of 'effectfree'
#'
#' Make factor the same length as 'effectfree',
#' giving the name of the term
#' that the each element belongs to.
#'
#' @param mod Object of class "bage_mod"
#'
#' @returns A factor, the same length
#' as 'effectfree'.
#'
#' @noRd
make_terms_effectfree <- function(mod) {
    priors <- mod$priors
    matrices <- make_matrices_effectfree_effect(mod)
    matrices <- lapply(matrices, Matrix::as.matrix)
    lengths <- vapply(matrices, ncol, 1L)
    nms <- names(priors)
    ans <- rep(nms, times = lengths)
    ans <- factor(ans, levels = nms)
    ans
}


## HAS_TESTS
#' Make factor identifying components of 'hyper'
#'
#' Make factor the same length as 'hyper',
#' giving the name of the term
#' that the each element belongs to.
#' Note that the levels of the factor
#' includes all priors, not just those
#' with hyper.
#'
#' We generate 'terms_hyper' when function 'fit'
#' is called, rather than storing it in the
#' 'bage_mod' object, to avoid having to update
#' it when priors change  via 'set_prior'.
#'
#' @param mod Object of class "bage_mod"
#'
#' @returns A factor, the same length
#' as 'hyper'.
#'
#' @noRd
make_terms_hyper <- function(mod) {
    priors <- mod$priors
    nms_terms <- names(priors)
    lengths <- make_lengths_hyper(mod)
    ans <- rep(nms_terms, times = lengths)
    ans <- factor(ans, levels = nms_terms)
    ans
}


## HAS_TESTS
#' Make Factor Identifying Components of 'hyperrand'
#'
#' Make factor the same length as 'hyperrand',
#' giving the name of the term
#' that the each element belongs to.
#' Note that the levels of the factor
#' includes all priors, not just those
#' with hyperrand.
#'
#' We generate 'terms_hyperrand' when function 'fit'
#' is called, rather than storing it in the
#' 'bage_mod' object, to avoid having to update
#' it when priors change  via 'set_prior'.
#'
#' @param mod Object of class "bage_mod"
#'
#' @returns A factor, the same length
#' as 'hyperrand'.
#'
#' @noRd
make_terms_hyperrand <- function(mod) {
  priors <- mod$priors
  nms_terms <- names(priors)
  lengths <- make_lengths_hyperrand(mod)
  ans <- rep(nms_terms, times = lengths)
  ans <- factor(ans, levels = nms_terms)
  ans
}


## HAS_TESTS
#' Make integer vector of flags for whether
#' each prior uses hyper-parameters
#'
#' @param mod Object of class 'bage_mod'
#'
#' @returns An integer vector
#'
#' @noRd
make_uses_hyper <- function(mod) {
  lengths <- make_lengths_hyper(mod)
  ans <- lengths > 0L
  ans <- 1L * ans
  ans
}


## HAS_TESTS
#' Make Integer Vector of Flags for Whether
#' Each Prior Uses Hyper-Parameters that
#' Can Be Treated as Random Effects
#'
#' @param mod Object of class 'bage_mod'
#'
#' @returns An integer vector
#'
#' @noRd
make_uses_hyperrand <- function(mod) {
  priors <- mod$priors
  1L * vapply(priors, uses_hyperrand, FALSE)
}


## HAS_TESTS
#' Make integer vector of flags for whether
#' each prior uses a matrix mapping effectfree to effect
#'
#' @param mod Object of class 'bage_mod'
#'
#' @returns An integer vector
#'
#' @noRd
make_uses_matrix_effectfree_effect <- function(mod) {
    priors <- mod$priors
    ans <- vapply(priors, uses_matrix_effectfree_effect, TRUE)
    ans <- as.integer(ans)
    names(ans) <- names(priors)
    ans    
}


## HAS_TESTS
#' Make integer vector of flags for whether
#' each prior uses an offset to convert
#' effectfree to effect
#'
#' @param mod Object of class 'bage_mod'
#'
#' @returns An integer vector
#'
#' @noRd
make_uses_offset_effectfree_effect <- function(mod) {
    priors <- mod$priors
    ans <- vapply(priors, uses_offset_effectfree_effect, TRUE)
    ans <- as.integer(ans)
    names(ans) <- names(priors)
    ans    
}


## HAS_TESTS
#' Prepare Number of Components Argument for SVD Prior
#'
#' @param n_comp Value for number provided by user
#' @param nm_n_comp Name for 'n_comp' to be used in error messages
#' @param ssvd Object of class "bage_ssvd"
#'
#' @returns Number of components - an integer scalar
#'
#' @noRd
n_comp_svd <- function(n_comp, nm_n_comp, ssvd) {
  n_comp_ssvd <- get_n_comp(ssvd)
  if (is.null(n_comp))
    n_comp <- ceiling(n_comp_ssvd / 2)
  else {
    check_n(n = n_comp,
            nm_n = nm_n_comp,
            min = 1L,
            max = NULL,
            null_ok = FALSE)
    if (n_comp > n_comp_ssvd)
      cli::cli_abort(c("{.arg {nm_n_comp}} larger than number of components of {.arg ssvd}.",
                       i = "{.arg {nm_n_comp}}: {.val {n_comp}}.",
                       i = "Number of components: {.val {n_comp_ssvd}}."))
  }
  as.integer(n_comp)
}


## HAS_TESTS
#' Standard Printing of Prior
#'
#' @param prior Object of class 'bage_mod'
#' @param nms User-visible names of attributes printed
#' @param slots Internal names of attributes printed
#'
#' @returns 'prior', invisibly
#'
#' @noRd
print_prior <- function(prior, nms, slots) {
  print_prior_header(prior)
  for (i in seq_along(nms)) {
    nm <- nms[[i]]
    slot <- slots[[i]]
    print_prior_slot(prior = prior, nm = nm, slot = slot)
  }
  invisible(prior)
}

## HAS_TESTS
#' Print the First Line of a Description of a Prior
#'
#' @param prior Object of class 'bage_prior'
#'
#' @returns NULL
#'
#' @noRd
print_prior_header <- function(prior)
  cat(" ", str_call_prior(prior), "\n")

## HAS_TESTS
#' Print Single Slot from Prior
#'
#' @param prior Object of class 'bage_prior'
#' @param nm User-visible name for slot
#' @param slot Internal name for slot
#'
#' @returns NULL
#'
#' @noRd
print_prior_slot <- function(prior, nm, slot) {
  n_offset <- get_print_prior_n_offset()
  val_slot <- prior[["specific"]][[slot]]
  if (is.null(val_slot))
    val_slot <- "NULL"
  cat(sprintf("% *s: %s\n", n_offset, nm, val_slot))
}  

## HAS_TESTS
#' Compile Args for 'along' Part of Prior for 'str_call_prior'
#'
#' @param prior Prior with 'along' components
#'
#' @returns A character vector
#'
#' @noRd
str_call_args_along <- function(prior) {
  along <- prior$specific$along
  if  (is.null(along))
    ""
  else
    sprintf('along="%s"', along)
}

## HAS_TESTS
#' Compile Args for AR Part of Prior for 'str_call_prior'
#'
#' Does not include 'along'.
#'
#' @param prior Prior with AR components
#'
#' @returns A character vector
#'
#' @noRd
str_call_args_ar <- function(prior) {
  specific <- prior$specific
  n_coef <- specific$n_coef
  min <- specific$min
  max <- specific$max
  scale <- specific$scale
  along <- prior$specific$along
  nm <- specific$nm
  is_ar1 <- grepl("AR1", nm)
  if (is_ar1) {
    ans <- character(3L)
    if (min != 0.8)
      ans[[1L]] <- sprintf("min=%s", min)
    if (max != 0.98)
      ans[[2L]] <- sprintf("max=%s", max)
    if (scale != 1)
      ans[[3L]] <- sprintf("s=%s", scale)
  }
  else {
    ans <- character(2L)
    if (n_coef != 2L)
      ans[[1L]] <- sprintf("n_coef=%d", n_coef)
    if (scale != 1)
      ans[[2L]] <- sprintf("s=%s", scale)
  }
  ans
}

## HAS_TESTS
#' Compile Args for Lin Part of Prior for 'str_call_prior'
#'
#' Does not include 'along'.
#'
#' @param prior Prior with AR components
#'
#' @returns A character vector
#'
#' @noRd
str_call_args_lin <- function(prior) {
  sd_slope <- prior$specific$sd_slope
  if (identical(sd_slope, 1))
    ""
  else
    sprintf("sd=%s", sd_slope)
}

## HAS_TESTS
#' Compile Args for 'n_comp' Part of Prior for 'str_call_prior'
#'
#' @param prior Prior with 'n_comp' parameter
#'
#' @returns A character vector
#'
#' @noRd
str_call_args_n_comp <- function(prior) {
  n_comp <- prior$specific$n_comp
  if (is.null(n_comp))
    ""
  else
    sprintf("n_comp=%s", n_comp)
}


## HAS_TESTS
#' Compile Args for 'n_seas' Part of Prior for 'str_call_prior'
#'
#' Does not include 'along'.
#'
#' @param prior Prior with seasonal component
#'
#' @returns A character vector
#'
#' @noRd
str_call_args_n_seas <- function(prior) {
  n_seas <- prior$specific$n_seas
  sprintf("n_seas=%s", n_seas)
}

## HAS_TESTS
#' Compile Args for 's_seas' Part of Prior for 'str_call_prior'
#'
#' @param prior Prior with 's_seas' parameter
#'
#' @returns A character vector
#'
#' @noRd
str_call_args_s_seas <- function(prior) {
  scale_seas <- prior$specific$scale_seas
  if (identical(scale_seas, 1))
    ""
  else
    sprintf("s_seas=%s", scale_seas)
}

## HAS_TESTS
#' Compile Args for 'scale' Part of Prior for 'str_call_prior'
#'
#' @param prior Prior with 'scale' parameter
#'
#' @returns A character vector
#'
#' @noRd
str_call_args_scale <- function(prior) {
  scale <- prior$specific$scale
  if (identical(scale, 1))
    ""
  else
    sprintf("s=%s", scale)
}

## HAS_TESTS
#' Compile Args for SVD Part of Prior for 'str_call_prior'
#'
#' Does not include 'along'.
#'
#' @param prior Prior with SVD components
#'
#' @returns A character vector
#'
#' @noRd
str_call_args_svd <- function(prior) {
  specific <- prior$specific
  ssvd <- specific$ssvd
  nm_ssvd <- specific$nm_ssvd
  n_comp <- specific$n_comp
  indep <- specific$indep
  ans <- character(3L)
  ans[[1L]] <- nm_ssvd
  n_comp_ssvd <- get_n_comp(ssvd)
  n_default <- ceiling(n_comp_ssvd / 2)
  if (n_comp != n_default)
    ans[[2L]] <- sprintf("n_comp=%s", n_comp)
  if (!indep)
    ans[[3L]] <- "indep=FALSE"
  ans
}


## HAS_TESTS
#' Function Used to Convert Variables in Data to Factors
#'
#' If a variable is already a factor, leave it unchanged.
#' If a variable is numeric, order levels by value.
#' Otherwise, order levels by first appearance.
#'
#' @param x A vector
#'
#' @returns A factor
#'
#' @noRd
to_factor <- function(x) {
  if (is.factor(x))
    x
  else if (is.numeric(x))
    factor(x)
  else
    factor(x, levels = unique(x))
}  
