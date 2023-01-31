

## 'components_hyper' -----------------------------------------------------------

#' Create data data frame holding hyper-parameters
#'
#' Helper function for 'components'
#'
#' @param mod A fitted 'bage_mod' object
#'
#' @returns A tibble
#'
#' @noRd
components_hyper <- function(mod) {
    hyper_trans <- mod$est$hyper
    priors <- mod$priors
    terms <- make_terms_hyper(priors)
    transforms <- lapply(priors, transform_hyper)
    transforms <- unlist(transforms)
    hyper <- double(length = length(hyper_trans))
    for (i in seq_along(hyper))
        hyper[[i]] <- transforms[[i]](hyper_trans[[i]])
    levels <- lapply(priors, levels_hyper)
    levels <- unlist(levels, use.names = FALSE)
    terms <- as.character(terms)
    tibble::tibble(term = terms,
                   level = levels,
                   .fitted = hyper)
}


## 'components_par' -----------------------------------------------------------

#' Create data data frame holding parameters
#'
#' Helper function for 'components'
#'
#' @param mod A fitted 'bage_mod' object
#'
#' @returns A tibble
#'
#' @noRd
components_par <- function(mod) {
    par <- mod$est$par
    terms_par <- mod$terms_par
    levels_par <- make_levels_par(mod)
    terms_par <- as.character(terms_par)
    tibble::tibble(term = terms_par,
                   level = levels_par,
                   .fitted = par)
}




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
get_fun_align_to_data <- function(mod) {
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
#' Need to allow the fact that, if Known
#' priors used, some parameters are treated
#' as fixed and known.
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
    priors <- mod$priors
    terms_par <- mod$terms_par
    is_known <- vapply(priors, is_known, FALSE)
    mean_par <- split(mean_par, terms_par)
    mean_par_unknown <- mean_par[!is_known]
    mean_par_unknown <- unlist(mean_par_unknown, use.names = FALSE)
    n_par_unknown <- length(mean_par_unknown)
    s_par_unknown <- seq_len(n_par_unknown)
    V1 <- prec[s_par_unknown, s_par_unknown, drop = FALSE]
    V2 <- prec[-s_par_unknown, -s_par_unknown, drop = FALSE]
    R <- prec[-s_par_unknown, s_par_unknown, drop = FALSE]
    prec_par_unknown <- V1 - Matrix::crossprod(R, Matrix::solve(V2, R))
    ans <- rmvn(n = n_draw,
                mean = mean_par_unknown,
                prec = prec_par_unknown)
    if (any(is_known)) {
        par_known <- unlist(mean_par[is_known], use.names = FALSE)
        ans_known <- matrix(par_known,
                            nrow = length(par_known),
                            ncol = n_draw)
        ans <- rbind(ans, ans_known)
        n_ans <- nrow(ans)
        i_current <- seq_len(n_ans)
        i_current <- split(i_current, terms_par)
        i_current <- c(i_current[!is_known], i_current[is_known])
        i_current <- unlist(i_current, use.names = FALSE)
        i_target <- seq_len(n_ans)
        row <- match(i_target, i_current)
        ans <- ans[row, ]
    }
    ans    
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
    inv_transform <- get_fun_inv_transform(mod)
    align_to_data <- get_fun_align_to_data(mod)
    ans <- inv_transform(draws_linear_pred)
    ans <- align_to_data(ans)
    ans
}


#' Make levels associated with each element of 'par'
#'
#' Make levels for each term, eg ages, times.
#' 'make_levels_par' works with the matrices
#' used to map levels to the outcome, to
#' ensure that the levels are correct (rather than
#' relying on undocumented properties of 'xtabs' etc),
#' though this makes the function a bit complicated.
#'
#' @param mod A fitted object of class 'bage_mod'.
#'
#' @returns A character vector.
#'
#' @noRd
make_levels_par <- function(mod) {
    formula <- mod$formula
    matrices_par <- mod$matrices_par
    outcome <- mod$outcome
    data <- mod$data
    nms <- names(matrices_par)
    n <- length(nms)
    factors <- attr(stats::terms(formula), "factors")
    factors <- factors[-1L, , drop = FALSE] ## exclude reponse
    factors <- factors > 0L
    if (is.array(outcome))
        dim_levels <- expand.grid(dimnames(outcome))
    else
        dim_levels <- data[rownames(factors)]
    ans <- vector(mode = "list", length = n)
    for (i in seq_len(n)) {
        nm <- nms[[i]]
        if (nm == "(Intercept)")
            ans[[i]] <- "(Intercept)"
        else {
            i_dim <- factors[, nm, drop = TRUE]
            paste_dot <- function(...) paste(..., sep = ".")
            term_levels <- do.call(paste_dot, dim_levels[i_dim])
            matrix_par <- matrices_par[[i]]
            i_term_level <- apply(matrix_par, 2L, function(x) match(1L, x))
            ans[[i]] <- term_levels[i_term_level]
        }
    }
    ans <- unlist(ans, use.names = FALSE)
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
    align_to_data <- get_fun_align_to_data(mod)
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
