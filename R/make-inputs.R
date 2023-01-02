
## HAS_TESTS
#' Extract 'n_hyper' attribute from
#' object of class 'bage_prior'
#'
#' 'n_hyper' is the number of hyper-parameters.
#'
#' @param prior An object of class 'bage_prior'.
#'
#' @returns An integer.
#'
#' @noRd
get_n_hyper <- function(prior) {
    prior$n_hyper
}


## HAS_TESTS
#' Make 'consts'
#'
#' Make vector to hold constants for priors.
#'
#' We generate 'consts' when function 'fit'
#' is called, rather than storing it in the
#' 'bage_sysmod' object, to avoid having to update
#' it when priors change  via 'set_prior'.
#' 
#' @param priors Named list of objects
#' of class 'bage_prior'.
#'
#' @returns A vector of doubles.
#'
#' @noRd
make_consts <- function(priors) {
    vapply(priors,
           FUN = function(x) x$consts,
           FUN.VALUE = 0.0,
           USE.NAMES = FALSE)
}


## HAS_TESTS
#' Make 'hyper'
#'
#' Make vector to hold hyper-parameters
#' for priors.
#'
#' We generate 'hyper' when function 'fit'
#' is called, rather than storing it in the
#' 'bage_sysmod' object, to avoid having to update
#' it when priors change  via 'set_prior'.
#' 
#' @param priors Named list of objects
#' of class 'bage_prior'.
#'
#' @returns A vector of zeros, of type 'double'.
#'
#' @noRd
make_hyper <- function(priors) {
    ans <- rep(0, times = length(priors))
    lengths <- vapply(priors, get_n_hyper, 0L)
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
#' 'bage_sysmod' object, to avoid having to update
#' it when priors change  via 'set_prior'.
#' 
#' @param priors Named list of objects
#' of class 'bage_prior'.
#'
#' @returns An named integer vector.
#'
#' @noRd
make_i_prior <- function(priors) {
    vapply(priors, function(x) x$i_prior, 0L)
}


## HAS_TESTS
#' Make list of matrices mapping terms to full array
#'
#' Make list of matrices mapping main effects or
#' interactions to array holding full rates (or,
#' equivalently, the outcome variable, or the
#' linear predictor.)
#' 
#' @param formula Formula specifying model
#' @param outcome Array holding values for response
#' variable
#'
#' @returns A named list
#'
#' @noRd
make_matrices_par <- function(formula, outcome) {
    factors <- attr(stats::terms(formula), "factors")
    factors <- factors[-1L, , drop = FALSE] ## exclude reponse
    factors <- factors > 0L
    dim_outcome <- dim(outcome)
    ans <- apply(factors,
                 MARGIN = 2L,
                 FUN = make_matrix_par,
                 dim = dim_outcome,
                 simplify = FALSE)
    names(ans) <- colnames(factors)
    has_intercept <- attr(stats::terms(formula), "intercept")
    if (has_intercept) {
        m <- matrix(integer(), nrow = 0L, ncol = 0L)
        m <- methods::as(m, "sparseMatrix")
        ans <- c(list("(Intercept)" = m), ans)
    }
    ans
}


## HAS_TESTS
#' Make matrix mapping term to full array
#'
#' Make sparse matrix mapping the elements of a
#' main effect or interaction to an array holding
#' the full rates (or equivalently, the outcome
#' variable, or the linear predictor.)
#'
#' @param dim `dim` attribute of array holding rates.
#' @param is_in_term Whether the term includes
#' the dimension.
#'
#' @returns A sparse matrix.
#'
#' @noRd
make_matrix_par <- function(dim, is_in_term) {
    make_submatrix <- function(d, is_in) {
        if (is_in) diag(d) else matrix(1L, nrow = d, ncol = 1L)
    }
    submatrices <- mapply(make_submatrix,
                          d = as.list(dim),
                          is_in = as.list(is_in_term),
                          SIMPLIFY = FALSE,
                          USE.NAMES = FALSE)
    submatrices <- rev(submatrices)
    ans <- Reduce(kronecker, submatrices)
    ans <- methods::as(ans, "sparseMatrix")
    ans
}


## HAS_TESTS
#' Make array holding offset variable
#' cross-classified by predictors
#'
#' Unlike in `xtabs()`, NAs are not converted
#' to 0s.
#' 
#' @param formula Formula specifying model
#' @param vname_offset Name of the offset variable.
#' @param data A data frame
#'
#' @returns An array (with named dimnames)
#'
#' @noRd
make_offset <- function(formula, vname_offset, data) {
    factors <- attr(stats::terms(formula), "factors")
    nms_vars <- rownames(factors)
    formula_xtabs <- paste0(vname_offset,
                            "~",
                            paste(nms_vars[-1L], collapse = "+"))
    formula_xtabs <- as.formula(formula_xtabs)
    ans <- stats::xtabs(formula_xtabs, data = data)
    ans <- array(as.double(ans),
                 dim = dim(ans),
                 dimnames = dimnames(ans))
    formula_na <- paste0("is.na(",
                         vname_offset,
                         ")~",
                         paste(nms_vars[-1L], collapse = "+"))
    formula_na <- as.formula(formula_na)
    is_na <- stats::xtabs(formula_na, data = data) > 0L
    ans[is_na] <- NA_real_
    ans
}


## HAS_TESTS
#' Make offset consisting entirely of 1s,
#' the same size as outcome
#'
#' @param outcome Array holding outcome.
#'
#' @returns An array (with named dimnames)
#'
#' @noRd
make_offset_ones <- function(outcome) {
    ans <- outcome
    ans[] <- 1.0
    ans
}


## HAS_TESTS
#' Make array holding outcome variable
#' cross-classified by predictors
#'
#' Unlike in `xtabs()`, NAs are not converted
#' to 0s.
#'
#' @param formula Formula specifying model
#' @param data A data frame
#' @param nm_distn Name of distribution:
#' "pois", "binom", or "norm".
#'
#' @returns An array (with named dimnames)
#'
#' @noRd
make_outcome <- function(formula, data, nm_distn) {
    factors <- attr(stats::terms(formula), "factors")
    nms_vars <- rownames(factors)
    formula_xtabs <- paste0(nms_vars[[1L]],
                            "~",
                            paste(nms_vars[-1L], collapse = "+"))
    formula_xtabs <- as.formula(formula_xtabs)
    ans <- stats::xtabs(formula_xtabs, data = data)
    ans <- array(as.double(ans),
                 dim = dim(ans),
                 dimnames = dimnames(ans))
    formula_na <- paste0("is.na(",
                         nms_vars[[1L]],
                         ")~",
                         paste(nms_vars[-1L], collapse = "+"))
    formula_na <- as.formula(formula_na)
    is_na <- stats::xtabs(formula_na, data = data) > 0L
    ans[is_na] <- NA_real_
    standardise <- identical(nm_distn, "norm") && (sum(!is.na(ans)) >= 2L)
    if (standardise)
        ans <- (ans - mean(ans, na.rm = TRUE)) / sd(ans, na.rm = TRUE)
    ans
}


## HAS_TESTS
#' Make default priors
#'
#' Make named list holding default priors.
#' Default prior is standard normal for
#' all terms expect intercept, where it
#' is N(0, 10^2).
#'
#' @param formula Formula specifying model
#'
#' @returns Named list.
#'
#' @noRd
make_priors <- function(formula) {
    scale_intercept <- 10
    nms <- attr(stats::terms(formula), "term.labels")
    ans <- rep(list(new_bage_prior_norm()), times = length(nms))
    names(ans) <- nms
    has_intercept <- attr(stats::terms(formula), "intercept")
    if (has_intercept) {
        prior_intercept <- new_bage_prior_norm(scale = scale_intercept)
        ans <- c(list("(Intercept)" = prior_intercept), ans)
    }
    ans
}


## HAS_TESTS
#' Make factor identifying components of 'consts'
#'
#' Make factor the same length as 'consts',
#' giving the name of the term
#' that the each element belongs to.
#'
#' We generate 'term_consts' when function 'fit'
#' is called, rather than storing it in the
#' 'bage_sysmod' object, to avoid having to update
#' it when priors change  via 'set_prior'.
#'
#' @param priors Named list of objects
#' of class 'bage_prior'.
#'
#' @returns A factor, the same length
#' as 'consts'.
#'
#' @noRd
make_term_consts <- function(priors) {
    nms_terms <- names(priors)
    lengths <- vapply(priors, function(x) length(x$consts), 0L)
    ans <- rep(nms_terms, times = lengths)
    ans <- factor(ans, levels = nms_terms)
    ans
}


## HAS_TESTS
#' Make factor identifying components of 'hyper'
#'
#' Make factor the same length as 'hyper',
#' giving the name of the term
#' that the each element belongs to.
#'
#' We generate 'term_hyper' when function 'fit'
#' is called, rather than storing it in the
#' 'bage_sysmod' object, to avoid having to update
#' it when priors change  via 'set_prior'.
#'
#' @param priors Named list of objects
#' of class 'bage_prior'.
#'
#' @returns A factor, the same length
#' as 'hyper'.
#'
#' @noRd
make_term_hyper <- function(priors) {
    nms_terms <- names(priors)
    lengths <- vapply(priors, get_n_hyper, 0L)
    ans <- rep(nms_terms, times = lengths)
    ans <- factor(ans, levels = nms_terms)
    ans
}


## HAS_TESTS
#' Make factor identifying components of 'par'
#'
#' Make factor vector the same length as 'par',
#' giving the name of the term
#' that the each element belongs to.
#'
#' We generate 'term_par' when the 'bage_sysmod'
#' object is first created, since the number
#' and lengths of the terms is fixed from that
#' point.
#'
#' @param formula Formula specifying model
#' @param outcome Array holding values for response
#' variable
#'
#' @returns A factor.
#'
#' @noRd
make_term_par <- function(formula, outcome) {
    factors <- attr(stats::terms(formula), "factors")
    factors <- factors[-1L, , drop = FALSE] ## exclude reponse
    factors <- factors > 0L
    nms_terms <- colnames(factors)
    dim_outcome <- dim(outcome)
    lengths <- apply(factors, 2L, function(i) prod(dim_outcome[i]))
    has_intercept <- attr(stats::terms(formula), "intercept")
    if (has_intercept) {
        lengths <- c(1L, lengths)
        nms_terms <- c("(Intercept)", nms_terms)
    }
    ans <- rep(nms_terms, times = lengths)
    ans <- factor(ans, levels = nms_terms)
    ans
}



    
