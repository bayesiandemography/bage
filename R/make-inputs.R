
## HAS_TESTS
#' Choose Values for 'matrix_along_by'
#' for All Priors to Pass to TMB
#'
#' @param x Object of class 'bage_mod'
#'
#' @returns A named list of matrices
#'
#' @noRd
choose_matrices_along_by <- function(x) {
  priors <- x$priors
  matrices_along_by <- x$matrices_along_by
  var_time <- x$var_time
  var_age <- x$var_age
  ans <- .mapply(choose_matrix_along_by,
                 dots = list(prior = priors,
                             matrices = matrices_along_by),
                 MoreArgs = list(var_time = var_time,
                                 var_age = var_age))
  names(ans) <- names(priors)
  ans
}
                      

## HAS_TESTS
#' Choose Value for 'matrix_along_by'
#' for Single Prior to Pass to TMB
#'
#' @param prior Object of class 'bage_prior'
#' @param matrices Named list of matrices
#' @param var_time Name of time variable
#' @param var_age Name of age variable
#'
#' @returns A matrix
#'
#' @noRd
choose_matrix_along_by <- function(prior, matrices, var_time, var_age) {
  if (!uses_along(prior))
    return(matrices[[1L]])
  n <- length(matrices)
  if (n == 1L)
    return(matrices[[1L]])
  nms <- names(matrices)
  nm_prior <- paste(nms, collapse = ":")
  along <- prior$specific$along
  if (is.null(along)) {
    has_time <- !is.null(var_time)
    if (has_time) {
      i_time <- match(var_time, nms, nomatch = 0L)
      if (i_time > 0L)
        return(matrices[[i_time]])
    }
    has_age <- !is.null(var_age)
    if (has_age) {
      i_age <- match(var_age, nms, nomatch = 0L)
      if (i_age > 0L)
        return(matrices[[i_age]])
    }
    msg <- c("Prior for {.var {nm_prior}} does not have a value for {.arg along}.",
             i = "Choices for {.arg along}: {.val {nms}}.")
    if (!has_time)
      msg <- c(msg,
               i = "Can't default to time variable, since {.var var_time} not specified.")
    if (!has_age)
      msg <- c(msg,
               i = "Can't default to age variable, since {.var var_age} not specified.")
    cli::cli_abort(msg)
  }
  else {
    i_along <- match(along, nms, nomatch = 0L)
    if (i_along > 0L)
      return(matrices[[i_along]])
    cli::cli_abort(c("Prior for {.var {nm_prior}} has invalid value for {.arg along}.",
                     i = "Value supplied: {.val {along}}.",
                     i = "Valid choices: {.val {nms}}."))
  }
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
  if (is.null(var_age)) {
    is_age_maineffect <- FALSE
    is_age_interact <- FALSE
  }
  else {
    is_age_maineffect <- identical(nm_term, var_age)
    is_age_interact <- !is_age_maineffect && (var_age %in% nm_term_split)
  }
  if (is.null(var_time)) {
    is_time_maineffect <- FALSE
    is_time_interact <- FALSE
  }
  else {
    is_time_maineffect <- identical(nm_term, var_time)
    is_time_interact <- !is_time_maineffect && (var_time %in% nm_term_split)
  }
  if (is_length_le_2)
    return(NFix())
  if (is_age_maineffect || is_time_maineffect)
    return(RW())
  if (is_age_interact || is_time_interact)
    return(ERW())
  N()
}


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
#' Identify Age Main effects and Age-Sex/Gender Interactions
#'
#' Classify terms as "age", "age:sex", "sex:age", and "other".
#'
#' @param mod Object of class 'bage_mod'
#'
#' @returns A named list.
#'
#' @noRd
make_agesex <- function(mod) {
  priors <- mod$priors
  var_age <- mod$var_age
  var_sexgender <- mod$var_sexgender
  nms <- names(priors)
  ans <- lapply(nms,
                make_agesex_inner,
                var_age = var_age,
                var_sexgender = var_sexgender)
  names(ans) <- nms
  ans
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
make_agesex_inner <- function(nm, var_age, var_sexgender) {
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
#' Make 'along' Value for 'compose' Priors
#'
#' @param priors List of objects of class 'bage_prior'
#'
#' @returns NULL or a string
#'
#' @noRd
make_compose_along <- function(priors) {
  ans <- NULL
  for (prior in priors) {
    if (uses_along(prior)) {
      along <- prior$specific$along
      if (!is.null(along)) {
        if (is.null(ans)) {
          str_ans <- str_call_prior(prior)
          ans <- along
        }
        else {
          if (!identical(along, ans)) {
            str_oth <- str_call_prior(prior)
            cli::cli_abort("{.var {str_ans}} and {.var {str_oth}} have different 'along' dimensions.")
          }
        }
      }
    }
  }
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
    ans <- unlist(ans)
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
#' Make Indices to Priors Used within 'compose' Priors
#'
#' @param mod Object of class "bage_mod".
#'
#' @returns An integer vector
#'
#' @noRd
make_indices_priors <- function(mod) {
  priors <- mod$priors
  matrices_along_by <- choose_matrices_along_by(mod)
  ans <- .mapply(indices_priors,
                 dots = list(priors,
                             matrix_along_by = matrices_along_by),
                 MoreArgs = list())
  names(ans) <- names(priors)
  ans <- unlist(ans)
  ans
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


#' Given Vectors of Terms and Levels, Determine Which
#' Parameters are Time-Varying
#'
#' @param term Character vector
#' @param level Character vector
#' @param var_time String
#'
#' @returns A logical vector
#'
#' @noRd
make_is_time_varying <- function(term, level, var_time) {
  ## reserved levels are labels for parameters that are
  ## non-time-varying, even if they occur in time terms
  p_reserved_level <- "sd$|slope$|mslope$|msd$|*coef[0-9]*$"
  term_split <- lapply(term, strsplit, split = ":")
  has_var_time <- function(x) var_time %in% x[[1L]]
  is_time_term <- vapply(term_split, has_var_time, FALSE)
  is_reserved <- grepl(p_reserved_level, level)
  is_time_term & !is_reserved
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
  levels_effect <- mod$levels_effect
  terms_effect <- mod$terms_effect
  matrices_along_by <- choose_matrices_along_by(mod)
  levels_effect <- split(levels_effect, terms_effect)
  levels <- .mapply(levels_hyperrand,
                    dots = list(prior = priors,
                                matrix_along_by = matrices_along_by,
                                levels_effect = levels_effect),
                    MoreArgs = list())
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
make_levels_forecast <- function(mod, labels_forecast) {
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
        paste_dot <- function(x, y) paste(x, y, sep = ".")
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
#' Make Matrices Giving Mapping Between Term and Age-Sex
#'
#' Make matrices giving mapping between position in term
#' and position on age dimension, or combination of age
#' and sex dimension (in the order that they appear).
#' If a term does not include age, then no matrix
#' is created.
#'
#' @param mod Object of class 'bage_mod'
#'
#' @returns A named list of matrices and NULLs
#'
#' @noRd
make_matrices_agesex <- function(mod) {
  formula <- mod$formula
  data <- mod$data
  var_age <- mod$var_age
  var_sexgender <- mod$var_sexgender
  has_var_age <- !is.null(var_age)
  has_var_sexgender <- !is.null(var_sexgender)
  ans <- list("(Intercept)" = NULL)
  factors <- attr(stats::terms(formula), "factors")
  if ((length(factors) > 0L) && has_var_age) {
    factors <- factors[-1L, , drop = FALSE]
    factors <- factors > 0L
    nms_vars <- rownames(factors)
    nms_terms <- colnames(factors)
    ans_terms <- rep(list(NULL), times = length(nms_terms))
    names(ans_terms) <- nms_terms
    for (i_term in seq_along(ans_terms)) {
      nms_vars_term <- nms_vars[factors[, i_term]]
      i_age <- match(var_age, nms_vars_term, nomatch = 0L)
      if (i_age > 0L) {
        data_term <- data[nms_vars_term]
        dimnames <- lapply(data_term, unique)
        dim <- lengths(dimnames)
        i_along <- i_age
        if (has_var_sexgender) {
          i_sex <- match(var_sexgender, nms_vars_term, nomatch = 0L)
          if (i_sex > 0L)
            i_along <- c(i_along, i_sex)
          i_along <- sort(i_along)
        }
        ans_terms[[i_term]] <- make_matrix_along_by(i_along = i_along,
                                                    dim = dim,
                                                    dimnames = dimnames)
      }
    }
    ans <- c(ans, ans_terms)
  }
  ans
}


## HAS_TESTS
#' Convert 'matrix_agesex' to Sparse Index Matrix
#'
#' @param matrix_agesex Matrix produced by
#' 'make_matrix_agesex'
#'
#' @returns A sparse matrix consisting of
#' 1s and 0s
#'
#' @noRd
make_matrix_agesex_index <- function(matrix_agesex) {
  n <- length(matrix_agesex)
  i <- as.integer(matrix_agesex) + 1L
  j <- seq_len(n)
  x <- rep.int(1L, times = n)
  ans <- Matrix::sparseMatrix(i = i,
                              j = j,
                              x = x)
  rn_old <- rownames(matrix_agesex)
  cn_old <- colnames(matrix_agesex)
  cn_new <- paste(rn_old, rep(cn_old, each = length(rn_old)), sep = ".")
  rn_new <- cn_new[match(seq_len(n), matrix_agesex + 1L)]
  dimnames(ans) <- list(rn_new, cn_new)
  ans
}


## HAS_TESTS
#' Make Matrices Mapping Values of 'along' and 'by'
#' to Positions in Intercepts, Main Effects,
#' and Interactions
#'
#' @param formula An R formula
#' @param data A data frame
#'
#' @returns A named list of named lists of matrices
#' 
#' @noRd
make_matrices_along_by <- function(formula, data) {
  ## matrix for intercept
  val <- list("(Intercept)" = matrix(0L, nrow = 1L))
  ans <- list("(Intercept)" = val)
  ## matrices for other terms
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
      dimnames <- lapply(data_term, unique)
      dim <- lengths(dimnames)
      i_along <- seq_along(dim)
      val <- lapply(i_along,
                    make_matrix_along_by,
                    dim = dim,
                    dimnames = dimnames)
      names(val) <- nms_vars_term
      ans_terms[[i_term]] <- val
    }
    names(ans_terms) <- nms_terms
    ans <- c(ans, ans_terms)
  }
  ans
}


## HAS_TESTS
#' Make List of Matrices Mapping Values
#' of 'along' and 'by' to Positions
#' in Main Effects and Interactions for Forecasts
#'
#' Only creates matrices for main effects
#' and interactions that include time
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
        dimnames <- lapply(data_term, unique)
        dimnames[[i_time]] <- labels_forecast
        dim <- lengths(dimnames)
        ans[[i_term + 1L]] <- make_matrix_along_by(i_along = i_time,
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
#' @param formula Formula specifying model
#' @param data Data frame
#'
#' @returns A named list
#'
#' @noRd
make_matrices_effect_outcome <- function(formula, data) {
  ## make intercept
  n_data <- nrow(data)
  i <- seq_len(n_data)
  j <- rep.int(1L, times = n_data)
  x <- rep.int(1L, times = n_data)
  m <- Matrix::sparseMatrix(i = i,
                            j = j,
                            x = x)
  colnames(m) <- "(Intercept)"
  ans <- list("(Intercept)" = m)
  ## make other terms
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
      data_term[] <- lapply(data_term, to_factor)
      contrasts_term <- lapply(data_term, stats::contrasts, contrast = FALSE)
      nm_term <- nms_terms[[i_term]]
      formula_term <- paste0("~", nm_term, "-1")
      formula_term <- stats::as.formula(formula_term)
      m_term <- Matrix::sparse.model.matrix(formula_term,
                                            data = data_term,
                                            contrasts.arg = contrasts_term,
                                            row.names = FALSE)
      colnames(m_term) <- levels(interaction(data_term))
      ans_terms[[i_term]] <- m_term
    }
    names(ans_terms) <- nms_terms
    ans <- c(ans, ans_terms)
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
    levels_effect <- mod$levels_effect
    terms_effect <- mod$terms_effect
    agesex <- make_agesex(mod)
    matrices_agesex <- make_matrices_agesex(mod)
    levels_age <- make_levels_age(mod)
    levels_sexgender <- make_levels_sexgender(mod)
    levels_effect <- split(levels_effect, terms_effect)
    ans <- .mapply(make_matrix_effectfree_effect,
                   dots = list(prior = priors,
                               levels_effect = levels_effect,
                               agesex = agesex,
                               matrix_agesex = matrices_agesex),
                   MoreArgs = list(levels_age = levels_age,
                                   levels_sexgender = levels_sexgender))
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
  paste_dot <- function(x, y) paste(x, y, sep = ".")
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
#' Make vector holding offset variable
#'
#' @param vname_offset Name of the offset variable.
#' @param data A data frame
#'
#' @returns An vector of doubles.
#'
#' @noRd
make_offset <- function(vname_offset, data) {
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
    levels_effect <- mod$levels_effect
    terms_effect <- mod$terms_effect
    levels_effect <- split(levels_effect, terms_effect)
    agesex <- make_agesex(mod)
    matrices_agesex <- make_matrices_agesex(mod)
    levels_age <- make_levels_age(mod)
    levels_sexgender <- make_levels_sexgender(mod)
    ans <- .mapply(make_offset_effectfree_effect,
                   dots = list(prior = priors,
                               levels_effect = levels_effect,
                               agesex = agesex,
                               matrix_agesex = matrices_agesex),
                   MoreArgs = list(levels_age = levels_age,
                                   levels_sexgender = levels_sexgender))
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
#' @param length_effect Number of elements in main
#' effect.
#' @param n_spline Number of columns in spline matrix
#'
#' @returns Matrix with 'length_effect' rows and 'n_spline' columns
#'
#' @noRd
make_spline_matrix <- function(length_effect, n_spline) {
    n_interval <- n_spline - 3L
    interval_length <- (length_effect - 1L) / n_interval
    start <- 1 - 3 * interval_length
    end <- length_effect + 3 * interval_length
    x <- seq(from = start, to = end, by = 0.001)
    base <- splines::bs(x = x, df = n_spline + 5L)
    i_keep <- findInterval(seq_len(length_effect), x)
    j_keep <- seq.int(from = 3L, length.out = n_spline)
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
#' Make Factor Indentifying Components of 'indices_priors'
#'
#' @param mod Object of class "bage_mod".
#'
#' @returns A factor
#'
#' @noRd
make_terms_indices_priors <- function(mod) {
  priors <- mod$priors
  matrices_along_by <- choose_matrices_along_by(mod)
  nms_terms <- names(priors)
  indices <- .mapply(indices_priors,
                     dots = list(priors,
                                 matrix_along_by = matrices_along_by),
                     MoreArgs = list())
  lengths <- lengths(indices)
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
#' Make Integer Vector of Flags for Whether
#' Each Prior Uses 'indices_priors'
#'
#' Currently only 'compose' priors
#' use 'indices_priors'
#'
#' @param mod Object of class 'bage_mod'
#'
#' @returns An integer vector
#'
#' @noRd
make_uses_indices_priors <- function(mod) {
  priors <- mod$priors
  matrices_along_by <- choose_matrices_along_by(mod)
  indices_priors <- .mapply(indices_priors,
                            dots = list(priors,
                                        matrix_along_by = matrices_along_by),
                            MoreArgs = list())
  lengths <- lengths(indices_priors)
  ans <- lengths > 0L
  ans <- 1L * ans
  names(ans) <- names(priors)
  ans
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
