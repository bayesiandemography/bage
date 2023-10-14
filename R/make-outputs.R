
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
#' Make copies of the original data to use
#' for replicate data
#'
#' @param data Original data supplied to model. A tibble.
#' @param n Number of replicates
#'
#' @returns A tibble
#'
#' @noRd
make_copies_repdata <- function(data, n) {
    n_row_data <- nrow(data)
    .replicate <- make_levels_replicate(n = n, n_row_data = n_row_data)
    .replicate <- tibble::tibble(.replicate = .replicate)
    data <- rep(list(data), times = n + 1L) # n replicates, plus original
    data <- vctrs::vec_rbind(!!!data)
    vctrs::vec_cbind(.replicate, data)
}
    

## HAS_TESTS
#' Make initial draws for 'components' function
#'
#' Make draws of variables included in 'prec'
#' matrix of TMB model output.
#'
#' If the Cholesky decomposition of
#' 'prec' was successful, then use that.
#' Otherwise, use the eigen decomposition.
#'
#' @param mod Object of class 'bage_mod'
#'
#' @returns A matrix
#'
#' @noRd
make_draws_comp_raw <- function(mod) {
    est <- mod$est
    R_prec <- mod$R_prec
    scaled_eigen <- mod$scaled_eigen
    is_fixed <- mod$is_fixed
    n_draw <- mod$n_draw
    mean <- unlist(est)[!is_fixed]
    if (is.matrix(R_prec))
        rmvnorm_chol(n = n_draw,
                     mean = mean,
                     R_prec = R_prec)
    else
        rmvnorm_eigen(n = n_draw,
                      mean = mean,
                      scaled_eigen = scaled_eigen)
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


## 'make_fitted_disp' ---------------------------------------------------------

## HAS_TESTS
#' Make modelled estimates for models
#' with dispersion term
#'
#' @param x A fitted 'bage_mod' object.
#' @param expected An rvec with posterior
#' distribution of expected values,
#' based on (transformed) linear predictor.
#' Aligned to data (not outcome.)
#' @param disp An rvec of length 1 with
#' posterior distribution for
#' dispersion term.
#'
#' @returns A vector of doubles.
#'
#' @noRd
make_fitted_disp <- function(x, expected, disp) {
    outcome <- x$outcome
    offset <- x$offset
    seed_fitted <- x$seed_fitted
    outcome <- as.double(outcome) ## so 'align_to_data' works correctly
    offset <- as.double(offset)   ## so 'align_to_data' works correctly
    align_to_data <- get_fun_align_to_data(x)
    outcome <- align_to_data(outcome)
    offset <- align_to_data(offset)
    n_val <- length(outcome)
    n_draw <- rvec::n_draw(expected)
    ans <- rvec::rvec_dbl(matrix(NA, nrow = n_val, ncol = n_draw))
    is_na <- is.na(outcome) | is.na(offset)
    outcome <- outcome[!is_na]
    offset <- offset[!is_na]
    expected <- expected[!is_na]
    seed_restore <- make_seed()
    set.seed(seed_fitted)
    ans[!is_na] <- make_fitted_disp_inner(x = x,
                                          outcome = outcome,
                                          offset = offset,
                                          expected = expected,
                                          disp = disp)
    set.seed(seed_restore)
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
    par <- mod$levels_par
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


## HAS_TEST
#' Make labels for replicate datasets
#'
#' @param n Number of replicate datasets
#' @param n_row_data Number of rows in a single
#' replicate dataset
#'
#' @returns A character vector with length n x n_row_dataset
#'
#' @noRd
make_levels_replicate <- function(n, n_row_data) {
    ans <- paste("Replicate", seq_len(n))
    ans <- c("Original", ans)
    ans <- rep(ans, each = n_row_data)
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
    matrix <- mod$matrix_season_outcome
    levels_par <- colnames(matrix)
    ans <- c(levels_par, level_hyper)
    ans
}


## HAS_TESTS
#' Make linear predictor from par.
#'
#' Does not include seasonal effect.
#'
#' Return value aligned to outcome, not data.
#'
#' @param mod Object of class "bage_mod"
#' @param components Data frame produced by
#' call to `components()`.
#'
#' @returns An rvec
#'
#' @noRd
make_linpred_par <- function(mod, components) {
    matrix_par_outcome <- make_combined_matrix_par_outcome(mod)
    matrix_par_outcome <- Matrix::as.matrix(matrix_par_outcome)
    is_par <- components$component == "par"
    par <- components$.fitted[is_par]
    matrix_par_outcome %*% par
}


## HAS_TESTS
#' Make linear predictor from season.
#'
#' Return value aligned to outcome, not data.
#'
#' @param mod Object of class "bage_mod"
#' @param components Data frame produced by
#' call to `components()`.
#'
#' @returns An rvec
#'
#' @noRd
make_linpred_season <- function(mod, components) {
    matrix_season_outcome <- mod$matrix_season_outcome
    matrix_season_outcome <- Matrix::as.matrix(matrix_season_outcome)
    is_season <- ((components$component == "season")
        & (components$term == "par"))
    season <- components$.fitted[is_season]
    matrix_season_outcome %*% season
}


## HAS_TESTS
#' Use a precision matrix to construct scaled eigen vectors
#'
#' The scaled eigen vectors can be used to draw from a
#' multivariate normal distribution with the given
#' pecision matrix.
#'
#' @param prec A symmetric positive definite matrix.
#'
#' @returns A matrix with the same dimensions as 'prec'
#'
#' @noRd
make_scaled_eigen <- function(prec) {
    tolerance <- 1e-6
    eigen <- eigen(prec, symmetric = TRUE)
    vals <- eigen$values
    vecs <- eigen$vectors
    min_valid_val <- -tolerance * abs(vals[[1L]]) ## based on MASS::mvrnorm
    if (any(vals < min_valid_val))
        cli::cli_abort("Estimated precision matrix not positive definite.")
    vals <- pmax(vals, abs(min_valid_val))
    sqrt_inv_vals <- sqrt(1 / vals)
    vecs %*% diag(sqrt_inv_vals)
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
    par <- mod$terms_par
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
    par <- make_par_season(mod)
    n_par <- length(par)
    levels <- c("par", "hyper")
    times <- c(n_par, 1L)
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
#' Draw from multivariate normal, using results
#' from a Cholesky decomposition
#'
#' @param n Number of draws
#' @param mean Mean of distribution
#' @param R_prec Cholesky decomposition of precision matrix
#'
#' @returns A matrix, with each columns being one draw
#'
#' @noRd
rmvnorm_chol <- function(n, mean, R_prec) {
    n_val <- length(mean)
    Z <- matrix(stats::rnorm(n = n_val * n),
                nrow = n_val,
                ncol = n)
    mean + backsolve(R_prec, Z)
}


## HAS_TESTS
#' Draw from multivariate normal, using results
#' from an eigen decomposition
#'
#' @param n Number of draws
#' @param mean Mean of distribution
#' @param scaled_eigen Matrix of scaled eigenvalues
#'
#' @returns A matrix, with each columns being one draw
#'
#' @noRd
rmvnorm_eigen <- function(n, mean, scaled_eigen) {
    n_val <- length(mean)
    Z <- matrix(stats::rnorm(n = n_val * n),
                nrow = n_val,
                ncol = n)
    mean + scaled_eigen %*% Z
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
    


    
    
    
    
