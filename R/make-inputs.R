
#' Make vector identifying components of 'hyper'
#'
#' Make factor the same length as 'hyper',
#' giving the index (0-based) of the prior
#' that the each element belongs to.
#'
#' We generate this on the fly within function 'fit',
#' to avoid having to update it when 'hyper' changes
#' (eg via 'set_prior').
#'
#' @param priors Named list of objects
#' of class 'bage_prior'.
#'
#' @returns An integer vector.
#'
#' @noRd
make_index_hyper <- function(priors) {
    ans <- seq.int(from = 0L, along.with = priors)
    lengths <- vapply(priors, get_n_hyper, 0L)
    ans <- rep(ans, times = lengths)
    ans
}

#' Make 'hyper'
#'
#' Make vector to hold hyper-parameters
#' for priors.
#'
#' @param priors Named list of objects
#' of class 'bage_prior'.
#'
#' @returns A vector of zeros, of type 'double'.
#'
#' @noRd
make_hyper <- function(index_hyper) {
    make_hyper_one_prior <- function(pr)
        rep(0, times = get_n_hyper(pr))
    ans <- lapply(priors, make_hyper_one_prior)
    ans <- unlist(ans)
    ans
}

make_index_par <- function(formula, outcome) {
    factors <- attr(stats::terms(formula), "factors")
    factors <- factors[-1L, ] ## exclude reponse
    factors <- factors > 0L
    dim_outcome <- dim(outcome)
    lengths <- vapply(factors, function(i) prod(dim_outcome[i]), 0L)
    ans <- colnames(factors)
    has_intercept <- attr(stats::terms(formula), "intercept")
    if (has_intercept) {
        lengths <- c(1L, lengths)
        ans <- c("(Intercept)", ans)
    }
    ans <- rep(ans, times = lengths)
    ans <- factor(ans, levels = unique(ans))
    ans
}

## HAS_TESTS
#' Make list of matrices mapping terms to full array
#'
#' Make list of matrices mapping main effects or
#' interactions to array holding full rates (or,
#' equivalently, the outcome variable, or the
#' linear predictor.)
#' 
#' @param formula Formula specifying terms
#' @param outcome Array holding values for response
#' variable
#'
#' @returns A named list
#'
#' @noRd
make_matrices_par <- function(formula, outcome) {
    factors <- attr(stats::terms(formula), "factors")
    factors <- factors[-1L, ] ## exclude reponse
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

make_par <- function(index_par) {
    tab <- table(index_par)
    lapply(tab, function(n) rep(0, times = n))
}

make_priors <- function(formula) {
    scale_intercept <- 10
    nms <- attr(stats::term(formula), "term.labels")
    ans <- rep(list(new_bage_prior_norm()), times = length(nms))
    has_intercept <- attr(stats::term(formula), "intercept")
    if (has_intercept) {
        prior_intercept <- new_bage_prior_norm(scale = scale_intercept)
        ans <- c(list("(Intercept)" = prior_intercept), ans)
    }
    ans
}


## HAS_TESTS
#' Make array holding outcome variable
#' cross-classified by predictors
#'
#' @param formula Formula specifying terms
#' @param data A data frame
#'
#' @returns An array (with named dimnames)
#'
#' @noRd
make_outcome <- function(formula, data) {
    factors <- attr(stats::terms(formula), "factors")
    nms_vars <- rownames(factors)
    formula_xtabs <- paste0(nms_vars[[1L]], "~", paste(nms_vars[-1L], collapse = "+"))
    formula_xtabs <- as.formula(formula_xtabs)
    ans <- stats::xtabs(formula_xtabs, data = data)
    ans <- array(as.double(ans),
                 dim = dim(ans),
                 dimnames = dimnames(ans))
    ans
}
