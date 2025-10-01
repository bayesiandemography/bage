
## HAS_TESTS
#' Alert User that Replacing Data Model
#'
#' @param datamod_new The new data model. Object of class bage_datamod.
#' @param datamod_old The old data model. Object of class bage_datamod.
#'
#' @returns NULL, invisibly
#'
#' @noRd
alert_replacing_existing_datamod <- function(datamod_new,
                                             datamod_old) {
  descr_new <- datamod_descr(datamod_new)
  descr_old <- datamod_descr(datamod_old)
  cli::cli_alert(paste("Replacing existing {.val {descr_old}} data model",
                       "with new {.val {descr_new}} data model"))
  invisible(NULL)
}


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
#' Raise an Error if Offset Specified Via Formula
#'
#' @param nm_offset_data Name of offset used in 'data', or formula
#' @param nm_offset_mod Name of offset used in documentation
#' @param nm_fun Name of function being called
#'
#' @returns TRUE, invisibly
#'
#' @noRd
error_offset_formula_used <- function(nm_offset_data, nm_offset_mod, nm_fun) {
  is_formula <- !is.null(nm_offset_data) && startsWith(nm_offset_data, "~")
  if (is_formula) {
    if (nm_offset_mod == "exposure")
      msg2 <- paste("In {.fun mod_pois}, please specify {nm_offset_mod} using",
                    "the name of a variable in {.arg data}, or {.val {1}}.")
    else if (nm_offset_mod == "size")
      msg2 <- paste("In {.fun mod_binom}, please specify {nm_offset_mod} using",
                    "the name of a variable in {.arg data}.")
    else if (nm_offset_mod == "weights")
      msg2 <- paste("In {.fun mod_norm}, please specify {nm_offset_mod} using",
                    "the name of a variable in {.arg data}, or {.val {1}}.")
    else
      cli::cli_abort("Internal error: Invalid value for nm_offset_mod.")
    cli::cli_abort(c(paste("{.fun {nm_fun}} cannot be used with models where",
                           "{nm_offset_mod} specified using formula."),
                     i = msg2,
                     i = "Current specification of {nm_offset_mod}: {.code {nm_offset_data}}."))
  }
  invisible(TRUE)
}


## HAS_TESTS
#' Evaluate Formula to Create Offset
#'
#' @param nm_offset_data Formula passed by user, turned into a string
#' @param data Data frame
#'
#' @returns A numeric vector
#'
#' @noRd
eval_offset_formula <- function(nm_offset_data, data) {
  nm_offset_data <- sub("^~", "", nm_offset_data)
  nm_offset_data <- parse(text = nm_offset_data)
  eval(nm_offset_data, envir = data)
}


## HAS_TESTS
#' Get a Matrix or Offset from an SVD Prior
#'
#' @param prior Object of class 'bage_prior'
#' @param dimnames_term Dimnames of array representation of term
#' @param var_age Name of age variable
#' @param var_sexgender Name of sex/gender variable
#'
#' @returns A sparse matrix.
#'
#' @noRd
get_matrix_or_offset_svd_prior <- function(prior,
                                           dimnames_term,
                                           var_age,
                                           var_sexgender,
                                           get_matrix) {
  ssvd <- prior$specific$ssvd
  indep <- prior$specific$indep
  n_comp <- prior$specific$n_comp
  levels_age <- dimnames_term[[var_age]]
  has_sexgender <- !is.null(var_sexgender)
  if (has_sexgender) {
    levels_sexgender <- dimnames_term[[var_sexgender]]
    nm_split <- dimnames_to_nm_split(dimnames_term)
    term_has_sexgender <- var_sexgender %in% nm_split
    if (term_has_sexgender)
      joint <- !indep
    else
      joint <- NULL
  }
  else {
    levels_sexgender <- NULL
    joint <- NULL
  }
  nm <- dimnames_to_nm(dimnames_term)
  agesex <- make_agesex(nm = nm,
                        var_age = var_age,
                        var_sexgender = var_sexgender)
  get_matrix_or_offset_svd(ssvd = ssvd,
                           levels_age = levels_age,
                           levels_sexgender = levels_sexgender,
                           joint = joint,
                           agesex = agesex,
                           get_matrix = get_matrix,
                           n_comp = n_comp)
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
get_print_prior_n_offset <- function() 10L



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


#' Initial Value for Standard Deviation Hyperparameter
#'
#' @returns A double
#'
#' @noRd
init_val_sd <- function() log(0.05)


## HAS_TESTS
#' Test Whether Row of 'data' is Included in Likelihood
#'
#' @param mod Object of class 'bage_mod'
#'
#' @returns A logical vector, with length
#' equal to `nrow(data)`.
#'
#' @noRd
get_is_in_lik <- function(mod) {
  (get_is_in_lik_covariates(mod) &
    get_is_in_lik_effects(mod)
    & get_is_in_lik_offset(mod)
    & get_is_in_lik_outcome(mod))
}


#' Test Whether Row of 'data' is Included in Likelihood
#' - Focussing on Covariates
#'
#' @param mod Object of class 'bage_mod'
#'
#' @returns A logical vector, with length
#' equal to `nrow(data)`.
#'
#' @noRd
get_is_in_lik_covariates <- function(mod) {
  data <- mod$data
  if (has_covariates(mod)) {
    formula <- mod$formula_covariates
    vars <- rownames(attr(stats::terms(formula), "factors"))
    data_vars <- data[vars]
    stats::complete.cases(data_vars)
  }
  else
    rep(TRUE, times = nrow(data))
}


## HAS_TESTS
#' Test Whether Row of 'data' is Included in Likelihood
#' - Focussing on 'effects'
#'
#' @param mod Object of class 'bage_mod'
#'
#' @returns A logical vector, with length
#' equal to `nrow(data)`.
#'
#' @noRd
get_is_in_lik_effects <- function(mod) {
  formula <- mod$formula
  data <- mod$data
  vars <- all.vars(formula[-2L])
  data_vars <- data[vars]
  stats::complete.cases(data_vars)
}


## HAS_TESTS
#' Test Whether Row of 'data' is Included in Likelihood
#' - Focussing on 'offset'
#'
#' @param mod Object of class 'bage_mod'
#'
#' @returns A logical vector, with length
#' equal to `nrow(data)`.
#'
#' @noRd
get_is_in_lik_offset <- function(mod) {
  offset <- mod$offset
  !is.na(offset) & (offset > 0)
}


## HAS_TESTS
#' Test Whether Row of 'data' is Included in Likelihood
#' - Focussing on 'outcome'
#'
#' @param mod Object of class 'bage_mod'
#'
#' @returns A logical vector, with length
#' equal to `nrow(data)`.
#'
#' @noRd
get_is_in_lik_outcome <- function(mod) {
  outcome <- mod$outcome
  !is.na(outcome)
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
#' Make Vector to Hold Coefficients for Covariates
#'
#' We generate 'coef_covariates' when function 'fit'
#' is called, rather than storing it in the
#' 'bage_mod' object, to avoid having to update
#' it when covariates set via `set_covariates()`
#' 
#' @param mod Object of class "bage_mod"
#'
#' @returns A vector of zeros, of type 'double'.
#'
#' @noRd
make_coef_covariates <- function(mod) {
  has_covariates <- has_covariates(mod)
  if (has_covariates) {
    covariates_nms <- mod$covariates_nms
    n_covariates <- length(covariates_nms)
    ans <- rep(0, times = n_covariates)
    names(ans) <- covariates_nms
  }
  else
    ans <- double()
  ans
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
  if (length(ans) > 0L)
    ans <- unlist(ans)
  else
    ans <- double()
  ans
}


## HAS_TESTS
#' Assemble Model Data into a Data Frame
#'
#' Assemble cleaned version of data into a data frame,
#' and omit rows that do not contribute to the likelihood.
#' 
#' @param mod Object of class 'bage_mod'
#'
#' @returns A data frame
#'
#' @noRd
make_data_df <- function(mod) {
  ans <- mod$data
  nm_outcome_data <- get_nm_outcome_data(mod)
  nm_offset_data <- get_nm_offset_data(mod)
  has_varying_offset <- has_varying_offset(mod)
  is_in_lik <- get_is_in_lik(mod)
  ans[[nm_outcome_data]] <- mod$outcome
  if (has_varying_offset)
    ans[[nm_offset_data]] <- mod$offset
  ans <- ans[is_in_lik, , drop = FALSE]
  ans <- tibble::tibble(ans)
  ans
}


## HAS_TESTS
#' Make the Levels Vector to be Used in a Data Model Object
#'
#'
#' @param data Data frame with data from main model
#' @param by_val Data frame with 'by' variables.
#' @param nm_component Name of component. Used
#' as when 'by_var' is empty.
#'
#' @returns A character vector
#'
#' @noRd
make_datamod_levels <- function(data, by_val, nm_component) {
  if (length(by_val) > 0L) {
    nms <- names(by_val)
    key_data <- Reduce(paste_dot, data[nms])
    key_val <- Reduce(paste_dot, by_val)
    intersect(key_val, key_data)
  }
  else {
    nm_component
  }
}


## HAS_TESTS
#' Make the Measure Variable to be Used in a Data Model Object
#'
#' Remove values that do not map on to 'data'
#'
#' @param data Data frame with data from main model
#' @param by_val Data frame with 'by' variables
#' for measure variable for data model
#' @param measure Numeric vector with values for
#' measure variable.
#'
#' @returns A numeric vector
#'
#' @noRd
make_datamod_measure <- function(data, by_val, measure) {
  if (length(by_val) > 0L) {
    nms <- names(by_val)
    key_data <- Reduce(paste_dot, data[nms])
    key_val <- Reduce(paste_dot, by_val)
    is_keep <- key_val %in% key_data
    measure[is_keep]
  }
  else {
    measure
  }
}


## HAS_TESTS
#' Make Dimnames for Terms in Model
#'
#' Handles case where formula does not have intercept.
#'
#' @param mod Object of class 'bage_mod'
#'
#' @returns A named list of lists
#'
#' @noRd
make_dimnames_terms <- function(formula, data) {
  ans <- list()
  terms_formula <- stats::terms(formula)
  has_intercept <- attr(terms_formula, "intercept")
  if (has_intercept)
    ans <- c(ans, list("(Intercept)" = list()))
  factors <- attr(terms_formula, "factors")
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
      lengths <- lengths(dimnames)
      i_length_1 <- match(1L, lengths, nomatch = 0L)
      if (i_length_1 > 0L) {
        nm <- nms_vars_term[[i_length_1]]
        val <- data[[nm]][[1L]]
        cli::cli_abort(c("{.arg formula} includes variable with single value.",
                         i = "Variable: {.var {nm}}.",
                         i = "Value: {.val {val}}.",
                         i = "Formula: {.code {deparse1(formula)}}."))
      }
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
  if (length(ans) > 0L) {
    ans <- unlist(ans)
    names(ans) <- rep(names(priors), times = lengths_effectfree)
  }
  else
    ans <- double()
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
  ans <- lapply(priors, make_param_hyper)
  ans <- unlist(ans)
  ans
}


## HAS_TESTS
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
make_hyperrandfree <- function(mod) {
  priors <- mod$priors
  ans <- rep(0, times = length(priors))
  names(ans) <- names(priors)
  lengths <- make_lengths_hyperrandfree(mod)
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
#' Lengths of vectors of parameters
#' 
#' @param dimnames_terms Named list with
#' dimnames for array representation of terms
#'
#' @returns A named integer vector.
#'
#' @noRd
make_lengths_effect <- function(dimnames_terms) {
  ans <- lapply(dimnames_terms, lengths)
  ans <- vapply(ans, prod, 1)
  ans <- vapply(ans, as.integer, 1L)
  nms <- names(dimnames_terms)
  i_intercept <- match("(Intercept)", nms, nomatch = 0L)
  if (i_intercept > 0L)
    ans[[i_intercept]] <- 1L
  ans
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
  vapply(matrices, n_col, 1L)
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
make_lengths_hyperrandfree <- function(mod) {
  priors <- mod$priors
  var_time <- mod$var_time
  var_age <- mod$var_age
  var_sexgender <- mod$var_sexgender
  dimnames_terms <- mod$dimnames_terms
  ans <- .mapply(length_hyperrandfree,
                 dots = list(prior = priors,
                             dimnames_term = dimnames_terms),
                 MoreArgs = list(var_time = var_time,
                                 var_age = var_age,
                                 var_sexgender = var_sexgender))
  ans <- unlist(ans)
  names(ans) <- names(priors)
  ans
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
#' @param dimnames_terms List with dimnames from
#' array representation of terms
#'
#' @returns A character vector.
#'
#' @noRd
make_levels_effects <- function(dimnames_terms) {
  ans <- lapply(dimnames_terms, dimnames_to_levels)
  if (length(ans) > 0L)
    ans <- unlist(ans, use.names = FALSE)
  else
    ans <- character()
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
  ans <- list()
  terms_formula <- stats::terms(formula)
  has_intercept <- attr(terms_formula, "intercept")
  if (has_intercept)
    ans <- c(ans, list("(Intercept)" = NULL))
  factors <- attr(terms_formula, "factors")
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
  ans <- list()
  terms_formula <- stats::terms(formula)
  has_intercept <- attr(terms_formula, "intercept")
  if (has_intercept)
    ans <- c(ans, list("(Intercept)" = NULL))
  factors <- attr(terms_formula, "factors")
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
  for (i_term in seq_len(n_term)) {
    dimnames_term <- dimnames_terms[[i_term]]
    nm <- dimnames_to_nm(dimnames_term)
    is_intercept <- nm == "(Intercept)"
    if (is_intercept) {
      n_data <- nrow(data)
      i <- seq_len(n_data)
      j <- rep.int(1L, times = n_data)
      x <- rep.int(1L, times = n_data)
      m_term <- Matrix::sparseMatrix(i = i, j = j, x = x)
    }
    else {
      nm_split <- dimnames_to_nm_split(dimnames_term)
      data_term <- data[nm_split]
      data_term[] <- .mapply(factor,
                             dots = list(x = data_term, levels = dimnames_term),
                             MoreArgs = list())
      contrasts_term <- lapply(data_term, stats::contrasts, contrast = FALSE)
      formula_term <- paste0("~", nm, "-1")
      formula_term <- stats::as.formula(formula_term)
      m_term <- Matrix::sparse.model.matrix(formula_term,
                                            data = data_term,
                                            contrasts.arg = contrasts_term,
                                            row.names = FALSE)
      colnames(m_term) <- dimnames_to_levels(dimnames_term)
    }
    ans[[i_term]] <- m_term
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
#' Make Matrix with Covariates
#'
#' Numeric variables are scaled, and treatment contrasts
#' are applied to categorical variables. There
#' is no intercept.
#'
#' @param formula One-sided formula describing covariates
#' @param data Data frame
#'
#' @returns A model matrix (excluding attributes)
#'
#' @noRd
make_matrix_covariates <- function(formula, data) {
  is_numeric <- vapply(data, is.numeric, FALSE)
  all_numeric <- all(is_numeric)
  has_intercept <- attr(stats::terms(formula), "intercept")
  standardize <- function(x) as.numeric(scale(x))
  data[is_numeric] <- lapply(data[is_numeric], standardize)
  if (all_numeric) {
    formula <- stats::update(formula, ~.-1)
    ans <- stats::model.matrix(formula, data = data)
  }
  else {
    if (!has_intercept)
      formula <- stats::update(formula, ~ . + 1)
    ans <- stats::model.matrix(formula, data = data)
    if (!identical(colnames(ans)[[1L]], "(Intercept)"))
      cli::cli_abort("Internal error: First column is not intercept.") ## nocov
    ans <- ans[, -1L, drop = FALSE]
  }
  attr(ans, "assign") <- NULL
  attr(ans, "contrasts") <- NULL
  rownames(ans) <- NULL
  ans
}


## HAS_TESTS
#' Make vector holding offset variable
#'
#' @param nm_offset_data Name of the offset variable.
#' @param data A data frame
#'
#' @returns An vector of doubles.
#'
#' @noRd
make_offset <- function(nm_offset_data, data) {
  is_offset_formula <- startsWith(nm_offset_data, "~")
  if (is_offset_formula)
    ans <- eval_offset_formula(nm_offset_data = nm_offset_data, data = data)
  else
    ans <- data[[nm_offset_data]]
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
#' Extracts the outcome variable from 'data'
#' and checks for infinite and NaN.
#' (Do this here because we know the name
#' *Rof the variable.)
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
  i_response <- match(nm_response, nms_data, nomatch = 0L)
  if (i_response == 0L)
    cli::cli_abort(paste("Internal error: response {.val {nm_response}}",
                         "not found in {.arg data}."))
  ans <- data[[i_response]]
  ans <- as.double(ans)
  check_inf(x = ans, nm_x = nm_response)
  check_nan(x = ans, nm_x = nm_response)
  ans
}


## HAS_TESTS
#' Make 'outcome', 'offset', 'matrices_effect_outcome', and
#' 'matrix_covariates' Components
#' of 'data' Argument in 'fit_default'
#'
#' @param mod Object of class 'bage_mod'
#' @param aggregate Logical. Whether to aggregate cells.
#'
#' @returns A named list
#'
#' @noRd
make_outcome_offset_matrices <- function(mod, aggregate) {
  dimnames_terms <- mod$dimnames_terms
  nm_outcome_data <- get_nm_outcome_data(mod)
  nm_offset_data <- get_nm_offset_data(mod)
  has_varying_offset <- has_varying_offset(mod)
  data_df <- make_data_df(mod)
  has_covariates <- has_covariates(mod)
  has_varying_offset <- has_varying_offset(mod)
  if (has_covariates)
    formula_covariates <- mod$formula_covariates
  if (aggregate) {
    fun_ag_outcome <- get_fun_ag_outcome(mod)
    fun_ag_offset <- get_fun_ag_offset(mod)
    formula <- mod$formula
    vars <- all.vars(formula[-2L])
    if (has_covariates) {
      vars_covariates <- all.vars(formula_covariates)
      vars <- union(vars, vars_covariates)
    }
    outcome_df <- stats::aggregate(data_df[nm_outcome_data], data_df[vars], fun_ag_outcome)
    outcome <- outcome_df[[nm_outcome_data]]
    if (has_varying_offset) {
      offset_df <- stats::aggregate(data_df[nm_offset_data], data_df[vars], fun_ag_offset)
      offset <- offset_df[[nm_offset_data]]
    }
    else {
      ones <- rep.int(1, times = nrow(data_df))
      offset_df <- stats::aggregate(ones, data_df[vars], fun_ag_offset)
      offset <- offset_df[[length(offset_df)]]
    }
    data_df <- offset_df
  }
  else {
    outcome <- data_df[[nm_outcome_data]]
    if (has_varying_offset)
      offset <- data_df[[nm_offset_data]]
    else
      offset <- rep.int(1, times = nrow(data_df))
  }
  matrices_effect_outcome <- make_matrices_effect_outcome(data = data_df,
                                                          dimnames_terms = dimnames_terms)
  if (has_covariates)
    matrix_covariates <- make_matrix_covariates(formula = formula_covariates,
                                                data = data_df)
  else
    matrix_covariates <- matrix(NA_real_, nrow = 0, ncol = 0)
  list(outcome = outcome,
       offset = offset,
       matrices_effect_outcome = matrices_effect_outcome,
       matrix_covariates = matrix_covariates)
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


#' Make Data Frame Giving the Class of Each Prior in a Model
#'
#' @param mod Object of class 'bage_mod'
#'
#' @returns A tibble
#'
#' @noRd
make_prior_class <- function(mod) {
  priors <- mod$priors
  nms <- names(priors)
  get_class <- function(x) class(x)[[1L]]
  class <- vapply(priors, get_class, "bage_prior_norm", USE.NAMES = FALSE)
  tibble::tibble(term = nms,
                 class = class)
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
#' @param dimnames_terms Named list with
#' dimnames for array representation of terms
#'
#' @returns A factor.
#'
#' @noRd
make_terms_effects <- function(dimnames_terms) {
  nms <- names(dimnames_terms)
  lengths <- make_lengths_effect(dimnames_terms)
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
    lengths <- vapply(matrices, n_col, 1L)
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
#' Make Factor Identifying Components of 'hyperrandfree'
#'
#' Make factor the same length as 'hyperrandfree',
#' giving the name of the term
#' that the each element belongs to.
#' Note that the levels of the factor
#' includes all priors, not just those
#' with hyperrandfree.
#'
#' We generate 'terms_hyperrandfree' when function 'fit'
#' is called, rather than storing it in the
#' 'bage_mod' object, to avoid having to update
#' it when priors change  via 'set_prior'.
#'
#' @param mod Object of class "bage_mod"
#'
#' @returns A factor, the same length
#' as 'hyperrandfree'.
#'
#' @noRd
make_terms_hyperrandfree <- function(mod) {
  priors <- mod$priors
  nms_terms <- names(priors)
  lengths <- make_lengths_hyperrandfree(mod)
  ans <- rep(nms_terms, times = lengths)
  ans <- factor(ans, levels = nms_terms)
  ans
}


## HAS_TESTS
#' Use 'vars_inner' to Construct 'use_term'
#'
#' @param mod Object of class 'bage_mod'
#' @param vars_inner Character vectors with
#' names of variables
#'
#' @returns A logical vector
#'
#' @noRd
make_use_term <- function(mod, vars_inner) {
  check_vars_inner(vars_inner)
  formula <- mod$formula
  priors <- mod$priors
  nms_priors <- names(priors)
  terms_formula <- stats::terms(formula) 
  factors <- attr(terms_formula, "factors")
  factors <- factors[-1L, ] ## drop response
  vars <- rownames(factors)
  in_vars <- vars_inner %in% vars
  n_not_in_mod <- sum(!in_vars)
  if (n_not_in_mod > 0L) {
    cli::cli_abort(c(paste("{.arg vars_inner} has {cli::qty(n_not_in_mod)} variable{?s}",
                           "not found in model."),
                     i = "{.arg vars_inner}: {.val {vars_inner}}.",
                     i = "Variables in model: {.val {vars}}."))
  }
  ans <- apply(factors > 0, 2L, function(i) all(vars[i] %in% vars_inner))
  terms_model <- setdiff(nms_priors, "(Intercept)")
  if (!any(ans))
    cli::cli_abort(c("No terms in model can be formed from {.arg vars_inner}.",
                     i = "Terms in model: {.val {terms_model}}.",
                     i = "{.arg var_inner}: {.val {vars_inner}}."))
  if (all(ans))
    cli::cli_abort(c("All terms in model can be formed from {.arg vars_inner}.",
                     i = "No terms left over to use in 'outer' model.",
                     i = "Terms in model: {.val {terms_model}}.",
                     i = "{.arg var_inner}: {.val {vars_inner}}."))
  has_intercept <- attr(terms_formula, "intercept")
  if (has_intercept)
    ans <- c(TRUE, ans)
    ## ans <- c(FALSE, ans)
  names(ans) <- nms_priors
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
make_uses_hyperrandfree <- function(mod) {
  priors <- mod$priors
  1L * vapply(priors, uses_hyperrandfree, FALSE)
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
#' Construct Value for 'vars_inner' Argument
#'
#' Use values for var_age, var_sexgender, and var_time.
#' Alert users if some or all of these not available.
#'
#' @param mod Object of class 'bage_mod'.
#'
#' @returns A character vector.
#'
#' @noRd
make_vars_inner <- function(mod) {
  var_age <- mod$var_age
  var_sexgender <- mod$var_sexgender
  var_time <- mod$var_time
  dimnames_terms <- mod$dimnames_terms
  ans <- c(var_age,
           var_sexgender,
           var_time)
  if (is.null(ans)) {
    nms_terms <- names(dimnames_terms)
    cli::cli_abort(c("Unable to infer {.arg vars_inner}.",
                     i = "Model term{?s}: {.val {nms_terms}}.",
                     i = paste("Use {.fun set_var_age}, {.fun set_var_sexgender}",
                               "{.fun set_var_time} to specify age, sex/gender,",
                               "and time variables?"),
                     i = "Or use other variables?"))
  }
  if (length(ans) < 3L)
    cli::cli_alert("Setting {.arg vars_inner} to {.val {ans}}.")
  ans
}


#' Emit a Message if Rates Suspiciously High
#'
#' Print a message if the one or more rates are
#' greater than 'mult_high_rate' times the 95th quantile,
#' and there are at least 20 observations.
#'
#' @param outcome Outcome variable. Numeric vector.
#' @param exposure Exposure variable. Numeric vector, same
#' length as 'outcome'.
#' @param mult_high_rate A positive number, Inf, or NULL.
#'
#' @returns NULL invisibly
#'
#' @noRd
message_suspicious_rates <- function(outcome, exposure, mult_high_rate) {
  min_obs <- 20L
  check_mult_high_rate(mult_high_rate)
  is_obs <- !is.na(outcome) & !is.na(exposure) & (exposure > 0)
  n_obs <- sum(is_obs)
  if (n_obs >= min_obs) {
    rate <- outcome / exposure
    q95 <- stats::quantile(rate, probs = 0.95, na.rm = TRUE)
    is_gt_q95 <- is_obs & (rate > q95 * mult_high_rate)
    n_gt_q95 <- sum(is_gt_q95)
    if (n_gt_q95 > 0L) {
      cli::cli_alert_warning(paste("{cli::qty(n_gt_q95)}{.arg data} has",
                                   "row{?s} with unexpectedly high rate{?s}."))
      i_max <- which.max(rate)
      val_outcome <- outcome[[i_max]]
      val_expose <- exposure[[i_max]]
      val_rate <- rate[[i_max]]
      msg <- "Row {.val {i_max}}"
      if (n_gt_q95 > 1L)
        msg <- paste0(msg, ", for example,")
      msg <- paste(msg,
                   "has",
                   "outcome {.val {val_outcome}},",
                   "exposure {.val {val_expose}}, and",
                   "rate {.val {val_rate}}.")
      cli::cli_alert_info(msg)
    }
  }
  invisible(NULL)
}


## HAS_TESTS
#' Number of Columns of Matrix
#'
#' Alternative to 'ncol' that works on sparse
#' matrices, and does not require converting
#' to a dense matrix, or loading the Matrix
#' package. Note that 'Matrix' does not export
#' a method for 'ncol' or 'dim', hence the
#' need to explicitly access the 'Dim' slot.
#'
#' @param x A matrix, possibly sparse
#'
#' @returns An integer
#'
#' @noRd
n_col <- function(x) {
  if (methods::.hasSlot(x, "Dim"))
    x@Dim[[2L]]
  else
    ncol(x)
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
    poputils::check_n(n = n_comp,
                      nm_n = nm_n_comp,
                      min = 1L,
                      max = NULL,
                      divisible_by = NULL)
    if (n_comp > n_comp_ssvd)
      cli::cli_abort(c("{.arg {nm_n_comp}} larger than number of components of {.arg ssvd}.",
                       i = "{.arg {nm_n_comp}}: {.val {n_comp}}.",
                       i = "Number of components: {.val {n_comp_ssvd}}."))
  }
  as.integer(n_comp)
}


## HAS_TESTS
#' Discard Terms from a Model
#'
#' Discard terms for which 'use_term' is FALSE.
#'
#' @param mod Object of class `bage_mod`
#' @param use_term Logical vector
#'
#' @returns Modified version of 'mod'
#' 
#' @noRd
reduce_model_terms <- function(mod, use_term) {
  mod <- unfit(mod)
  mod$priors <- mod$priors[use_term]
  mod$dimnames_terms <- mod$dimnames_terms[use_term]
  formula <- mod$formula
  nms_terms <- names(mod$priors)
  has_intercept <- "(Intercept)" %in% nms_terms
  nms_terms <- setdiff(nms_terms, "(Intercept)")
  n_nm <- length(nms_terms)
  if (n_nm == 0L)
    formula_new <- ". ~ 1"
  else {
    formula_new <- paste(". ~", paste(nms_terms, collapse = " + "))
    if (!has_intercept)
      formula_new <- paste(formula_new, "- 1")
  }
  formula_new <- stats::as.formula(formula_new)
  formula <- stats::update(formula, formula_new)
  mod$formula <- formula
  mod
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
#' Replace Existing Priors with "Known" Priors
#'
#' @param mod Object of class 'bage_mod'
#' @param prior_values Named list with 'values'
#' vectors for priors
#'
#' @returns A modified version of 'mod'
#'
#' @noRd
set_priors_known <- function(mod, prior_values) {
  mod <- unfit(mod)
  priors <- mod$priors
  nms <- names(prior_values)
  for (nm in nms) {
    values <- prior_values[[nm]]
    priors[[nm]] <- Known(values)
  }
  mod$priors <- priors
  mod
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
  scale <- specific$scale
  shape1 <- specific$shape1
  shape2 <- specific$shape2
  min <- specific$min
  max <- specific$max
  nm <- specific$nm
  is_ar1 <- grepl("AR1", nm)
  if (is_ar1) {
    ans <- character(5L)
    if (scale != 1)
      ans[[1L]] <- sprintf("s=%s", scale)
    if (shape1 != 5)
      ans[[2L]] <- sprintf("shape1=%s", shape1)
    if (shape2 != 5)
      ans[[3L]] <- sprintf("shape2=%s", shape2)
    if (min != 0.8)
      ans[[4L]] <- sprintf("min=%s", min)
    if (max != 0.98)
      ans[[5L]] <- sprintf("max=%s", max)
  }
  else {
    ans <- character(4L)
    if (n_coef != 2L)
      ans[[1L]] <- sprintf("n_coef=%d", n_coef)
    if (scale != 1)
      ans[[2L]] <- sprintf("s=%s", scale)
    if (shape1 != 5)
      ans[[3L]] <- sprintf("shape1=%s", shape1)
    if (shape2 != 5)
      ans[[4L]] <- sprintf("shape2=%s", shape2)
  }
  ans
}

## HAS_TESTS
#' Compile Args for 'con' Part of Prior for 'str_call_prior'
#'
#' @param prior Prior with 'con' 
#'
#' @returns A character vector
#'
#' @noRd
str_call_args_con <- function(prior) {
  con <- prior$specific$con
  if (con == "by")
    sprintf('con="%s"', con)
  else
    ""
}

## HAS_TESTS
#' Compile Args for Lin Part of Prior for 'str_call_prior'
#'
#' Does not include 'along'.
#'
#' @param prior Prior with linear components
#'
#' @returns A character vector
#'
#' @noRd
str_call_args_lin <- function(prior) {
  mean_slope <- prior$specific$mean_slope
  sd_slope <- prior$specific$sd_slope
  args <- character(2L)
  if (!identical(mean_slope, 0))
    args[[1L]] <- sprintf("mean_slope=%s", mean_slope)
  if (!identical(sd_slope, 1))
    args[[2L]] <- sprintf("sd_slope=%s", sd_slope)
  args
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
#' Compile Args for 'sd' Part of Prior for 'str_call_prior'
#'
#' @param prior Prior with 'sd' parameter
#'
#' @returns A character vector
#'
#' @noRd
str_call_args_sd <- function(prior) {
  sd <- prior$specific$sd
  if (identical(sd, 1))
    ""
  else
    sprintf("sd=%s", sd)
}


## HAS_TESTS
#' Compile Args for 'sd_seas' Part of Prior for 'str_call_prior'
#'
#' @param prior Prior with 'sd_seas' parameter
#'
#' @returns A character vector
#'
#' @noRd
str_call_args_sd_seas <- function(prior) {
  sd_seas <- prior$specific$sd_seas
  if (identical(sd_seas, 1))
    ""
  else
    sprintf("sd_seas=%s", sd_seas)
}


## HAS_TESTS
#' Compile Args for sd_slope Part of Prior for 'str_call_prior'
#'
#' @param prior Prior with sd_slope'
#'
#' @returns A character vector
#'
#' @noRd
str_call_args_sd_slope <- function(prior) {
  sd_slope <- prior$specific$sd_slope
  if (identical(sd_slope, 1))
    ""
  else
    sprintf("sd_slope=%s", sd_slope)
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


  
