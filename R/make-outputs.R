
## HAS_TESTS
#' Create data frame holding hyper-parameters
#'
#' Helper function for 'components'
#'
#' @param mod A fitted 'bage_mod' object
#'
#' @returns A tibble
#'
#' @noRd
components_hyper <- function(mod) {
    term <- make_terms_hyper(mod)
    term <- as.character(term)
    level <- make_levels_hyper(mod)
    draws <- make_draws_hyper(mod)
    .fitted <- rvec::rvec_dbl(draws)
    tibble::tibble(component = "hyper",
                   term = term,
                   level = level,
                   .fitted = .fitted)
}


## HAS_TESTS
#' Create data frame holding parameters
#'
#' Helper function for 'components'
#'
#' @param mod A fitted 'bage_mod' object
#'
#' @returns A tibble
#'
#' @noRd
components_par <- function(mod) {
    term <- make_terms_par(mod)
    term <- as.character(term)    
    level <- make_levels_par(mod)
    draws <- make_draws_par(mod)
    draws <- as.matrix(draws) ## cast from dgeMatrix
    .fitted <- rvec::rvec_dbl(draws)
    tibble::tibble(component = "par",
                   term = term,
                   level = level,
                   .fitted = .fitted)
}


## HAS_TESTS
#' Create data frame holding seasonal effect
#'
#' Helper function for 'components'
#'
#' @param mod A fitted 'bage_mod' object
#'
#' @returns A tibble or NULL
#'
#' @noRd
components_season <- function(mod) {
    if (!has_season(mod))
        return(NULL)
    term <- make_terms_season(mod)
    term <- as.character(term)
    level <- make_levels_season(mod)
    draws <- make_draws_season(mod)
    .fitted <- rvec::rvec_dbl(draws)
    tibble::tibble(component = "season",
                   term = term,
                   level = level,
                   .fitted = .fitted)
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
        ans <- identity
    ans
}


## HAS_TESTS
#' Create combined matrix from par to outcome
#'
#' Combine matrices for individual terms to
#' create a matrix that maps all elements of
#' 'par' to 'outcome'.
#'
#' @param An object of class 'bage_mod'
#'
#' @returns A sparse matrix.
#'
#' @noRd
make_combined_matrix_par_outcome <- function(mod) {
    matrices_par_outcome <- mod$matrices_par_outcome
    Reduce(Matrix::cbind2, matrices_par_outcome)
}


## HAS_TESTS
#' Create combined matrix from parfree to par
#'
#' Combine matrices for individual terms to
#' create a matrix that maps all elements of
#' 'parfree' to 'pr'.
#'
#' @param An object of class 'bage_mod'
#'
#' @returns A sparse matrix.
#'
#' @noRd
make_combined_matrix_parfree_par <- function(mod) {
    matrices <- make_matrices_parfree_par(mod)
    Matrix::.bdiag(matrices)
}


## HAS_TESTS
#' Create combined offset from parfree to par
#'
#' Combine offsets for individual terms to
#' create a offset that maps all elements of
#' 'parfree' to 'pr'.
#'
#' @param An object of class 'bage_mod'
#'
#' @returns A numeric vector
#'
#' @noRd
make_combined_offset_parfree_par <- function(mod) {
    offsets <- make_offsets_parfree_par(mod)
    do.call(c, offsets)
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
    matrix_par_outcome <- make_combined_matrix_par_outcome(mod)
    ans <- matrix_par_outcome %*% draws_par
    ans <- matrix(as.double(ans), nrow = nrow(ans))
    ans
}


## HAS_TESTS
#' Make draws from posterior distribution
#' of lowest-level rates/probabilities/values
#'
#' @param mod A fitted object of class 'bage_mod'
#'
#' @returns A matrix 'n_draw' columns.
#'
#' @noRd
make_draws_fitted <- function(mod) {
    draws_linear_pred <- make_draws_linear_pred(mod)
    inv_transform <- get_fun_inv_transform(mod)
    align_to_data <- get_fun_align_to_data(mod)
    scale_outcome <- get_fun_scale_outcome(mod)
    ans <- inv_transform(draws_linear_pred)
    ans <- align_to_data(ans)
    ans <- scale_outcome(ans)
    ans
}


## HAS_TESTS
#' Make draws from posterior distribution
#' of hyperparameters
#'
#' Make draws of hyperparameters
#' of priors for main effects and interactions.
#' Number of draws governed by 'n_draw'.
#'
#' @param mod A fitted object of class 'bage_mod'.
#'
#' @returns A list of vectors, each with 'n_draw' elements.
#'
#' @noRd
make_draws_hyper <- function(mod) {
    priors <- mod$priors
    mean_par <- mod$est$par
    mean_hyper <- mod$est$hyper
    prec_all <- mod$prec ## includes parfree, hyper, potentially par_season, hyper_season
    n_draw <- mod$n_draw
    terms_hyper <- make_terms_hyper(mod)
    n_par <- length(mean_par)
    n_hyper <- length(mean_hyper)
    s_hyper <- seq.int(from = n_par + 1L, length.out = n_hyper)
    V1 <- prec_all[s_hyper, s_hyper, drop = FALSE]
    V2 <- prec_all[-s_hyper, -s_hyper, drop = FALSE]
    R <- prec_all[-s_hyper, s_hyper, drop = FALSE]
    prec_hyper <- V1 - Matrix::crossprod(R, Matrix::solve(V2, R))
    ans <- rmvn(n = n_draw,
                mean = mean_hyper,
                prec = prec_hyper)
    transforms <- lapply(priors, transform_hyper)
    transforms <- unlist(transforms, recursive = FALSE)
    ans <- apply(ans, 1L, identity, simplify = FALSE)
    for (i in seq_along(ans)) {
        transform <- transforms[[i]]
        ans[[i]] <- transform(ans[[i]])
    }
    ans
}


## HAS_TESTS
#' Make draws from posterior distribution
#' of vector all parameters
#'
#' Make draws of parameters for
#' intercept, main effects,
#' and interactions, but not hyper-parameters.
#' Number of draws governed by 'n_draw'.
#'
#' @param mod A fitted object of class 'bage_mod'.
#'
#' @returns A matrix with 'n_draw' columns.
#'
#' @noRd
make_draws_par <- function(mod) {
    draws <- make_draws_parfree(mod)
    matrix <- make_combined_matrix_parfree_par(mod)
    offset <- make_offsets_parfree_par(mod)
    matrix %*% draws + offset
}


## HAS_TESTS
#' Make draws from posterior distribution
#' of vector all free parameters
#'
#' Make draws of free parameters for
#' intercept, main effects,
#' and interactions, but not hyper-parameters.
#' Number of draws governed by 'n_draw'.
#'
#' @param mod A fitted object of class 'bage_mod'.
#'
#' @returns A matrix with 'n_draw' columns.
#'
#' @noRd
make_draws_parfree <- function(mod) {
    mean_parfree <- mod$est$parfree
    prec_all <- mod$prec ## includes known, hyper
    n_draw <- mod$n_draw
    priors <- mod$priors
    terms_parfree <- make_terms_parfree(mod)
    is_known <- vapply(priors, is_known, FALSE)
    ## draw from multivariate normal, which is
    ## complicated because we have a precision
    ## matrix, not a variance matrix
    ## https://www.apps.stat.vt.edu/leman/VTCourses/Precision.pdf
    mean_parfree <- split(mean_parfree, terms_parfree)
    mean_parfree_unknown <- mean_parfree[!is_known]
    mean_parfree_unknown <- unlist(mean_parfree_unknown, use.names = FALSE)
    n_unknown <- length(mean_parfree_unknown)
    s_unknown <- seq_len(n_unknown)
    V1 <- prec_all[s_unknown, s_unknown, drop = FALSE]
    V2 <- prec_all[-s_unknown, -s_unknown, drop = FALSE]
    R <- prec_all[-s_unknown, s_unknown, drop = FALSE]
    prec_parfree_unknown <- V1 - Matrix::crossprod(R, Matrix::solve(V2, R))
    ans <- rmvn(n = n_draw,
                mean = mean_parfree_unknown,
                prec = prec_parfree_unknown)
    ## insert any known values
    if (any(is_known)) {
        parfree_known <- unlist(mean_parfree[is_known], use.names = FALSE)
        ans_known <- matrix(parfree_known,
                            nrow = length(parfree_known),
                            ncol = n_draw)
        ans <- rbind(ans, ans_known)
        n_ans <- nrow(ans)
        i_current <- seq_len(n_ans)
        i_current <- split(i_current, terms_parfree)
        i_current <- c(i_current[!is_known], i_current[is_known])
        i_current <- unlist(i_current, use.names = FALSE)
        i_target <- seq_len(n_ans)
        row <- match(i_target, i_current)
        ans <- ans[row, ]
    }
    ans
}


## HAS_TESTS
#' Make draws from posterior distribution
#' of seasonal effect parameters and hyper-parameter.
#'
#' Number of draws governed by 'n_draw'.
#'
#' @param mod A fitted object of class 'bage_mod'.
#'
#' @returns A list of vectors, each with 'n_draw' elements.
#'
#' @noRd
make_draws_season <- function(mod) {
    transform_hyper <- exp ## log_sd
    priors <- mod$priors
    mean_par_season <- mod$est$par_season
    mean_hyper_season <- mod$est$hyper_season
    prec_all <- mod$prec
    n_draw <- mod$n_draw
    mean_season <- c(mean_par_season, mean_hyper_season)
    n_season <- length(mean_season)
    n_all <- nrow(prec_all)
    s_season <- seq.int(to = n_all, length.out = n_season)
    V1 <- prec_all[s_season, s_season, drop = FALSE]
    V2 <- prec_all[-s_season, -s_season, drop = FALSE]
    R <- prec_all[-s_season, s_season, drop = FALSE]
    prec_season <- V1 - Matrix::crossprod(R, Matrix::solve(V2, R))
    ans <- rmvn(n = n_draw,
                mean = mean_season,
                prec = prec_season)
    ans <- apply(ans, 1L, identity, simplify = FALSE)
    n_ans <- length(ans)
    ans[[n_ans]] <- transform_hyper(ans[[n_ans]])
    ans
}


## HAS_TESTS
#' Make levels associated with each element of 'hyper'
#'
#' Make levels for hyperparameters for each term
#'
#' @param mod A fitted object of class 'bage_mod'.
#'
#' @returns A character vector.
#'
#' @noRd
make_levels_hyper <- function(mod) {
    priors <- mod$priors
    ans <- lapply(priors, levels_hyper)
    ans <- unlist(ans, use.names = FALSE)
    ans
}


## HAS_TESTS
#' Make levels for parameters and hyper-parameter of
#' seasonal effect
#'
#' @param mod A fitted object of class 'bage_mod'.
#'
#' @returns A character vector.
#'
#' @noRd
make_levels_season <- function(mod) {
    level_hyper <- "sd"
    if (!has_season(mod))
        return(character())
    var_time <- mod$var_time
    levels_par <- make_levels_par(mod)
    terms_par <- make_terms_par(mod)
    levels_time <- levels_par[terms_par == var_time]
    ans <- c(levels_time, level_hyper)
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
#' Make factor identifying components of 'season'
#'
#' Make factor distinguishing parameters
#' and hyper-parmeters in 'season'
#'
#' @param mod Object of class "bage_mod"
#'
#' @returns A factor.
#'
#' @noRd
make_terms_season <- function(mod) {
    if (!has_season(mod))
        return(factor())
    n_time <- n_time(mod)
    levels <- c("par", "hyper")
    times <- c(n_time, 1L)
    ans <- rep(levels, times = times)
    ans <- factor(ans, levels = levels)
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
    Z <- matrix(stats::rnorm(n = n_val * n),
                nrow = n_val,
                ncol = n)
    ch <- chol(prec)
    mean + backsolve(ch, Z)
}

## rmvn <- function(n, mean, prec) {
##     n_val <- length(mean)
##     ch <- chol(prec)
##     I <- diag(n_val)
##     sd <- backsolve(ch, I)
##     Z <- matrix(stats::rnorm(n = n_val * n),
##                 nrow = n_val,
##                 ncol = n)
##     mean + sd %*% Z
## }
