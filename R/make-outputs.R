    
## HAS_TESTS
#' Get function to align vector or matrix to data
#'
#' Get function to align a vector or a matrix
#' to the data frame 'data'. If the 'outcome'
#' of 'mod' is a a plain vector, then no alignment
#' is needed, and none is done. Otherwise,
#' the function assumes that elements of the vector
#' or the rows of the matrix are in the same
#' order as the cells of 'outcome'.
#'
#' @param mod An object of class 'bage_mod'
#'
#' @returns A function
#' 
#' @noRd
get_align_to_data <- function(mod) {
    outcome <- mod$outcome
    if (is.array(outcome)) {
        data <- mod$data
        dim <- dim(outcome)
        n_dim <- length(dim)
        dn <- dimnames(outcome)
        nms <- names(dn)
        nms_data <- names(data)
        index <- vector(mode = "list", length = n_dim)
        for (i_dim in seq_len(n_dim)) {
            nm <- nms[[i_dim]]
            index[[i_dim]] <- match(data[[nm]], dn[[nm]])
        }
        index <- do.call(cbind, index)
        if (n_dim > 1L) {
            mult <- c(1L, cumprod(dim[-n_dim]))
            index <- ((index - 1L) %*% mult) + 1L
        }
        index <- as.integer(index)
        ans <- function(x) {
            if (is.matrix(x))
                x[index, ]
            else
                x[index]
        }
    }
    else
        ans <- function(x) x
    ans
}


## HAS_TESTS
#' Get function to calculate inverse tranformation
#'
#' @param mod An object of class 'bage_mod'
#'
#' @returns A function
#'
#' @noRd
get_inv_transform <- function(mod) {
    nm_distn <- mod$nm_distn
    if (nm_distn == "pois")
        exp
    else if (nm_distn == "binom")
        function(x) ifelse(x > 0, 1 / (1 + exp(-x)), exp(x) / (1 + exp(x)))
    else if (nm_distn == "norm")
        function(x) x
    else
        stop(gettextf("invalid value for 'nm_distn' : \"%s\"",
                      nm_distn),
             call. = FALSE)
}


## HAS_TESTS
#' Extract point estimates for terms
#'
#' @param mod A fitted model
#'
#' @returns A named list.
#'
#' @noRd
make_terms_est <- function(mod) {
    term_par <- mod$term_par
    est <- mod$est$par
    split(est, term_par)
}


## HAS_TESTS
#' Extract standard errors for terms
#'
#' @param mod A fitted model
#'
#' @returns A named list.
#'
#' @noRd
make_terms_std <- function(mod) {
    term_par <- mod$term_par
    std <- mod$std$par
    split(std, term_par)
}


## HAS_TESTS
#' Combine map matrices for individual terms
#'
#' Combine map matrices for individual terms to
#' create a matrix that maps all elements of
#' 'par' to 'outcome'.
#'
#' @param An object of class 'bage_mod'
#'
#' @returns A sparse matrix.
#'
#' @noRd
make_combined_matrix_par <- function(mod) {
    matrices_par <- mod$matrices_par
    Reduce(Matrix::cbind2, matrices_par)
}


## HAS_TESTS
#' Make draws from posterior distribution
#' of vector of all terms combined
#'
#' Make draws of intercept, main effects,
#' and interactions, but not hyper-parameters.
#' Number of draws governed by 'n_draw'.
#'
#' @param mod A fitted object of class 'bage_mod'.
#'
#' @returns A tibble with 'n_draw' columns.
#'
#' @noRd
make_draws_par <- function(mod) {
    mean_par <- mod$est$par
    prec <- mod$prec
    n_draw <- mod$n_draw
    mean_par <- unlist(mean_par, use.names = FALSE)
    n_par <- length(mean_par)
    s_par <- seq_len(n_par)
    prec_par <- prec[s_par, s_par, drop = FALSE]
    rmvn(n = n_draw,
         mean = mean_par,
         prec = prec_par)
}


## HAS_TESTS
#' Make draws from posterior distribution of
#' linear predictor
#'
#' @param mod A fitted object of class 'bage_mod'
#'
#' @returns A tibble with 'n_draw' columns.
#'
#' @noRd
make_draws_linear_pred <- function(mod) {
    draws_par <- make_draws_par(mod)
    matrix_par <- make_combined_matrix_par(mod)
    ans <- matrix_par %*% draws_par
    ans <- matrix(as.double(ans), nrow = nrow(ans))
    ans
}


## HAS_TESTS
#' Make draws from posterior distribution
#' of lowest-level rates/probabilities/values
#'
#' @param mod A fitted object of class 'bage_mod'
#'
#' @returns A tibble with 'n_draw' columns.
#'
#' @noRd
make_draws_fitted <- function(mod) {
    draws_linear_pred <- make_draws_linear_pred(mod)
    inv_transform_fun <- get_inv_transform(mod)
    align_to_data_fun <- get_align_to_data(mod)
    ans <- inv_transform_fun(draws_linear_pred)
    ans <- align_to_data_fun(ans)
    ans
}

## HAS_TESTS
#' Make direct estimates
#'
#' @param A fitted 'bage_mod' object.
#'
#' @returns A vector of doubles.
#'
#' @noRd
make_observed <- function(mod) {
    outcome <- mod$outcome
    offset <- mod$offset
    align_to_data <- get_align_to_data(mod)
    ans <- as.double(outcome / offset)
    ans <- align_to_data(ans)
    ans
}


## HAS_TESTS
#' Draw from a multivariate normal distribution
#'
#' Code based partly on MASS::mvrnorm.
#' 
#' @param n Number of draws
#' @param mean Mean of MVN distribution
#' @param prec Precision of MVN distribution
#'
#' @returns Matrix with length(mean)
#' rows and n columns.
#'
#' @noRd
rmvn <- function(n, mean, prec) {
    n_val <- length(mean)
    ch <- chol(prec)
    I <- diag(n_val)
    sd <- backsolve(ch, I)
    Z <- matrix(stats::rnorm(n = n_val * n),
                nrow = n_val,
                ncol = n)
    mean + sd %*% Z
}
