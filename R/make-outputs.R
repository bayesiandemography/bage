
## NO_TESTS
#' Create data frame holding dispersion parameter
#'
#' Helper function for 'components'
#'
#' @param mod A fitted 'bage_mod' object
#'
#' @returns A tibble
#'
#' @noRd
components_disp <- function(mod) {
    draws <- make_draws_disp(mod)
    .fitted <- rvec::rvec_dbl(draws)
    tibble::tibble(component = "disp",
                   term = "disp",
                   level = "disp",
                   .fitted = .fitted)
}


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
#' of 'mod' is a plain vector, then no alignment
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
#' Insert values for fixed parameters into
#' random draws for non-fixed parameters
#'
#' @param draws A matrix containing draws
#' for non-fixed parameters. Each row is one
#' variable and each column is one draw.
#' @param est Mean values for (fixed and unfixed)
#' parameters. Output from TMB.
#' @param is_fixed Logical vector indicator whether
#' each (unlisted) value in 'est' is fixed
#'
#' @returns A matrix with more rows than 'draws',
#' but same number of columns.
#'
#' @noRd
insert_draws_known <- function(draws, est, is_fixed) {
    est <- unlist(est)
    ans <- matrix(nrow = length(is_fixed),
                  ncol = ncol(draws))
    ans[!is_fixed, ] <- draws
    ans[is_fixed, ] <- est[is_fixed]
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
#' Create combined matrix from seasonal effect to outcome
#'
#' Combine matrices for individual terms to
#' create a matrix that maps all elements of
#' seaonal effect to 'outcome'.
#'
#' @param An object of class 'bage_mod'
#'
#' @returns A sparse matrix.
#'
#' @noRd
make_combined_matrix_season_outcome <- function(mod) {
    var_time <- mod$var_time
    matrices <- mod$matrices_par_outcome
    matrices[[var_time]]
}


## HAS_TESTS
#' Create combined matrix from parfree to par
#'
#' Combine matrices for individual terms to
#' create a matrix that maps all elements of
#' 'parfree' to 'par'.
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
#' Make variable identifying component in 'components'
#'
#' Helper function for function 'components'
#'
#' @param mod Object of class 'bage_mod'
#'
#' @returns A character vector
#'
#' @noRd
make_comp_component <- function(mod) {
    est <- mod$est
    offset <- make_offsets_parfree_par(mod)
    has_disp <- has_disp(mod)
    has_season <- has_season(mod)
    vals <- c("par", "hyper", "disp", "season")
    n_par <- length(offset)
    n_hyper <- length(est$hyper)
    n_disp <- as.integer(has_disp)
    if (has_season)
        n_season <- length(est$par_season) + length(est$hyper_season)
    else
        n_season <- 0L
    times <- c(n_par, n_hyper, n_disp, n_season)
    rep(vals, times = times)
}



## HAS_TESTS
#' Make initial draws for 'components' function
#'
#' Make draws of variables included in 'prec'
#' matrix of TMB model output. Instead of using
#' 'prec' itself, use results from Cholesky
#' decomposition.
#'
#' @param mod Object of class 'bage_mod'
#'
#' @returns A matrix
#'
#' @noRd
make_draws_comp_raw <- function(mod) {
    est <- mod$est
    R_prec <- mod$R_prec
    is_fixed <- mod$is_fixed
    n_draw <- mod$n_draw
    n_val <- nrow(R_prec)
    Z <- matrix(stats::rnorm(n = n_val * n_draw),
                nrow = n_val,
                ncol = n_draw)
    mean <- unlist(est)[!is_fixed]
    mean + backsolve(R_prec, Z)
}


## HAS_TESTS
#' Make draws from posterior distribution of
#' all parameters other than lowest-level
#' rates/probs/means
#'
#' @param mod Object of class 'bage_mod'
#'
#' @returns A matrix
#'
#' @noRd
make_draws_components <- function(mod) {
    est <- mod$est
    is_fixed <- mod$is_fixed
    matrix_parfree_par <- make_combined_matrix_parfree_par(mod)
    offset_parfree_par <- make_offsets_parfree_par(mod)
    transforms_hyper <- make_transforms_hyper(mod)
    ## WARNING The order of the following functions matters:
    ## each makes assumptions about the rows of 'draws'
    draws <- make_draws_comp_raw(mod)
    draws <- insert_draws_known(draws = draws,
                                est = est,
                                is_fixed = is_fixed)
    draws <- transform_draws_hyper(draws = draws,
                                   transforms = transforms_hyper)
    draws <- transform_draws_par(draws = draws,
                                 matrix = matrix_parfree_par,
                                 offset = offset_parfree_par)
    n <- nrow(draws)
    keep <- rep(TRUE, n)
    n_season <- length(est$par_season) + length(est$hyper_season)
    if (!has_disp(mod)) {
        i_disp <- n - n_season
        keep[i_disp] <- FALSE
    }
    if (!has_season(mod)) {
        i_season <- seq.int(to = n, length.out = n_season)
        keep[i_season] <- FALSE
    }
    draws <- draws[keep, , drop = FALSE]
    draws
}



## NO_TESTS
#' Make draws from posterior distribution
#' of dispersion parameter
#'
#' Number of draws governed by 'n_draw'.
#'
#' @param mod A fitted object of class 'bage_mod'.
#'
#' @returns A list of length 1, holding a vector
#' with 'n_draw' elements.
#'
#' @noRd
make_draws_disp <- function(mod) {
    mean_par <- mod$est$par
    mean_hyper <- mod$est$hyper
    mean_disp <- mod$est$log_disp
    prec_all <- mod$prec
    n_draw <- mod$n_draw
    i_disp <- length(mean_par) + length(mean_hyper) + 1L
    V1 <- prec_all[i_disp, i_disp, drop = FALSE]
    V2 <- prec_all[-i_disp, -i_disp, drop = FALSE]
    R <- prec_all[-i_disp, i_disp, drop = FALSE]
    prec_disp <- V1 - Matrix::crossprod(R, Matrix::solve(V2, R))
    prec_disp <- as.numeric(prec_disp)
    sd_disp <- 1 / sqrt(prec_disp)
    ans <- rnorm(n = n_draw, 
                 mean = mean_disp,
                 sd = sd_disp)
    ans <- exp(ans)
    ans <- list(ans)
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
    prec_all <- mod$prec ## includes parfree, hyper, disp, maybe par_season, hyper_season
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
#' Make logical vector indicating whether
#' an element of 'est' is fixed
#'
#' @param est Mean values for posterior.
#' Returned by TMB function 'sdreport()'.
#' @param Named list or NULL. Argument to
#' TMB function 'MakeADFun()'.
#'
#' @returns A logical vector
#' 
#' @noRd
make_is_fixed <- function(est, map) {
    if (is.null(map)) {
        n <- length(unlist(est))
        ans <- rep(FALSE, times = n)
    }
    else {
        ans <- est
        nms_est <- names(est)
        nms_map <- names(map)
        for (nm in nms_est) {
            if (nm %in% nms_map)
                ans[[nm]] <- is.na(map[[nm]])
            else
                ans[[nm]] <- rep(FALSE, times = length(ans[[nm]]))
        }
        ans <- unlist(ans)
    }
    ans
}


## HAS_TESTS
#' Make variable identifying level within each component
#' of 'components'
#'
#' Helper function for function 'components'
#'
#' @param mod Object of class 'bage_mod'
#'
#' @returns A character vector
#'
#' @noRd
make_level_components <- function(mod) {
    par <- make_levels_par(mod)
    hyper <- make_levels_hyper(mod)
    par <- as.character(par)
    hyper <- as.character(hyper)
    ans <- c(par, hyper)
    if (has_disp(mod))
        ans <- c(ans, "disp")
    if (has_season(mod)) {
        season <- make_levels_season(mod)
        season <- as.character(season)
        ans <- c(ans, season)
    }
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
#' Make variable identifying term within each component
#' of 'components'
#'
#' Helper function for function 'components'
#'
#' @param mod Object of class 'bage_mod'
#'
#' @returns A character vector
#'
#' @noRd
make_term_components <- function(mod) {
    par <- make_terms_par(mod)
    hyper <- make_terms_hyper(mod)
    par <- as.character(par)
    hyper <- as.character(hyper)
    ans <- c(par, hyper)
    if (has_disp(mod))
        ans <- c(ans, "disp")
    if (has_season(mod)) {
        season <- make_terms_season(mod)
        season <- as.character(season)
        ans <- c(ans, season)
    }
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
#' Make list of transforms to be applied to
#' hyper-parameters 
#'
#' Includes dispersion, if present,
#' and hyper-parameters for season effect.
#' Result applied to *all* parameters, and
#' includes NULLs for parameters that are
#' not hyper-parameters.
#'
#' @param mod Object of class 'bage_mod'
#'
#' @returns A list of functions or NULL
#'
#' @noRd
make_transforms_hyper <- function(mod) {
    est <- mod$est
    priors <- mod$priors
    has_disp <- has_disp(mod)
    has_season <- has_season(mod)
    n_parfree <- length(est$parfree)
    n_par_season <- length(est$par_season)
    n_hyper_season <- length(est$hyper_season)
    nms_parfree <- names(est$parfree)
    nms_par_season <- names(est$par_season)
    nms_hyper_season <- names(est$hyper_season)
    ans_parfree <- rep(list(NULL), times = n_parfree)
    names(ans_parfree) <- nms_parfree
    ans_hyper <- lapply(priors, transform_hyper)
    ans_hyper <- unlist(ans_hyper, recursive = FALSE)
    ans_log_disp <- if (has_disp) exp else NULL
    ans_log_disp = list(log_disp = ans_log_disp)
    ans_par_season <- rep(list(NULL), times = n_par_season)
    names(ans_par_season) <- nms_par_season
    ans_hyper_season <- if (has_season) exp else NULL
    ans_hyper_season <- rep(list(ans_hyper_season), times = n_hyper_season)
    names(ans_hyper_season) <- nms_hyper_season
    c(ans_parfree,
      ans_hyper,
      ans_log_disp,
      ans_par_season,
      ans_hyper_season)
}


    
## HAS_TESTS
#' Draw from a multivariate normal distribution
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
    z <- matrix(stats::rnorm(n = n_val * n),
                nrow = n_val,
                ncol = n)
    ch <- chol(prec)
    mean + backsolve(ch, z)
}


## HAS_TESTS
#' Transform hyper-parameters, and possibly dispersion
#'
#' Transform hyper-parameters, including any
#' hyper-parameters for season, as well as 
#' any dispersion term.
#'
#' @param mod Object of class 'bage_mod'
#'
#' @returns A list of functions or NULL
#'
#' @noRd
transform_draws_hyper <- function(draws, transforms) {
    for (i in seq_along(transforms)) {
        transform <- transforms[[i]]
        if (!is.null(transform))
            draws[i, ] <- transform(draws[i, ])
    }
    draws
}


## HAS_TESTS
#' Convert 'parfree' part of draws to 'par'
#'
#' Use matrices and offset to convert free parameters
#' (including ones with Known priors)
#' into complete effects/interactions
#'
#' **Warning** this function must be called
#' after `insert_draws_known()`.
#'
#' @param draws Matrix of draws from posterior
#' distribution of all parameters, including
#' parameters with Known priors.
#' @param matrix Matrix mapping all
#' parfree to par
#' @param offset Offset converting all
#' parfree to par
#'
#' @returns A matrix
#'
#' @noRd
transform_draws_par <- function(draws, matrix, offset) {
    n_parfree <- ncol(matrix)
    n_val <- nrow(draws)
    is_parfree <- seq_len(n_val) <= n_parfree
    parfree <- draws[is_parfree, , drop = FALSE]
    not_parfree <- draws[!is_parfree, , drop = FALSE]
    par <- matrix %*% parfree + offset
    rbind(par, not_parfree)
}
    



make_linear_pred <- function(mod, components) {
    matrix_par_outcome <- make_combined_matrix_par_outcome(mod)
    is_par <- components$component == "par"
    par <- components[is_par, ".fitted"]
    ans <- matrix_par_outcome %*% par
    if (has_season(mod)) {
        matrix_season_outcome <- make_combined_matrix_season_outcome(mod)
        is_season <- components$component == "season"
        season <- components[is_season, ".fitted"]
        ans <- ans + matrix_season_outcome %*% season
    }
    ans
}

## make_expected <- function(


##     inv_transform <- get_fun_inv_transform(mod)
##     align_to_data <- get_fun_align_to_data(mod)
##     scale_outcome <- get_fun_scale_outcome(mod)
##     ans <- inv_transform(draws_linear_pred)
##     ans <- align_to_data(ans)
##     ans <- scale_outcome(ans)

    
## make_fitted.bage_mod_pois <- function(mod, expected, disp) {
##     align_to_data <- get_fun_align_to_data(mod)
##     outcome <- align_to_data(mod$outcome)
##     offset <- align_to_data(mod$offset)
##     rvec::rvec_rgamma(n = length(outcome),
##                       shape = outcome + 1 / disp
##                       rate = offset + 1 / (disp * expected))
## }
    


## augment.bage_mod <- function(x, ...) {
##     ans <- x$data
##     ans[[".observed"]] <- make_observed(x)
##     is_fitted <- is_fitted(x)
##     if (is_fitted) {
##         components <- components(x)
##         expected <- make_expected(mod = x,
##                                   components = components)
##         if (has_disp) {
##             disp <- make_disp(components)
##             fitted <- make_fitted(mod = x,
##                                   expected = expected,
##                                   disp = disp)
##             ans[[".fitted"]] <- fitted
##             ans[[".expected"]] <- expected
##         }
##         else
##             ans[[".fitted"]] <- expected
##     }
##     ans <- tibble(ans)
##     ans
## }

    
    
    
    
