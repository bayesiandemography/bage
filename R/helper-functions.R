
## HAS_TESTS
#' Make list of matrices mapping terms to full array
#'
#' Make list of matrices mapping main effects or
#' interactions to array holding full rates (or,
#' equivalently, the outcome variable, or the
#' linear predictor.)
#' 
#' @param formula Formula specifying terms
#' @param y Array holding values for response
#' variable
#'
#' @returns A named list
#'
#' @noRd
make_map_matrices <- function(formula, y) {
    factors <- attr(stats::terms(formula), "factors")
    factors <- factors[-1L, ] ## exclude reponse
    factors <- factors > 0L
    dim_y <- dim(y)
    ans <- apply(factors,
                 MARGIN = 2L,
                 FUN = make_map_matrix,
                 dim = dim_y,
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
make_map_matrix <- function(dim, is_in_term) {
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


make_params <- function(formula, y) {
    factors <- attr(stats::terms(formula), "factors")
    factors <- factors[-1L, ] ## exclude reponse
    factors <- factors > 0L
    dim_y <- dim(y)
    dn_y <- dimnames(y)
    make_param <- function(i)
        array(0, dim = dim_y[i], dimnames = dn_y[i])
    ans <- apply(factors, 2L, make_param, simplify = FALSE)
    names(ans) <- colnames(factors)
    has_intercept <- attr(stats::terms(formula), "intercept")
    if (has_intercept)
        ans <- c(list("(Intercept)" = 0), ans)
    ans
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



make_y <- function(formula, data) {
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
