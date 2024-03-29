
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


#' Test Whether Two Objects Have the Same Class
#'
#' @param x,y Objects
#'
#' @returns TRUE or FALSE
#'
#' @noRd
is_same_class <- function(x, y)
    identical(class(x)[[1L]], class(y)[[1L]])
    


## HAS_TESTS
#' Create combined matrix from effect to outcome
#'
#' Combine matrices for individual terms to
#' create a matrix that maps all elements of
#' 'effect' to 'outcome'.
#'
#' @param An object of class 'bage_mod'
#'
#' @returns A sparse matrix.
#'
#' @noRd
make_combined_matrix_effect_outcome <- function(mod) {
    matrices_effect_outcome <- mod$matrices_effect_outcome
    Reduce(Matrix::cbind2, matrices_effect_outcome)
}


## HAS_TESTS
#' Create combined matrix from effectfree to effect
#'
#' Combine matrices for individual terms to
#' create a matrix that maps all elements of
#' 'effectfree' to 'effect'.
#'
#' @param An object of class 'bage_mod'
#'
#' @returns A sparse matrix.
#'
#' @noRd
make_combined_matrix_effectfree_effect <- function(mod) {
    matrices <- make_matrices_effectfree_effect(mod)
    Matrix::.bdiag(matrices)
}


## HAS_TESTS
#' Create combined offset from effectfree to effect
#'
#' Combine offsets for individual terms to
#' create a offset that maps all elements of
#' 'effectfree' to 'effect'.
#'
#' @param An object of class 'bage_mod'
#'
#' @returns A numeric vector
#'
#' @noRd
make_combined_offset_effectfree_effect <- function(mod) {
    offsets <- make_offsets_effectfree_effect(mod)
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
make_comp_components <- function(mod) {
  est <- mod$est
  terms_effect <- mod$terms_effect
  has_disp <- has_disp(mod)
  vals <- c("effect", "hyper", "hyperrand", "disp")
  n_effect <- length(terms_effect)
  n_hyper <- length(est$hyper)
  n_hyperrand <- length(est$hyperrand)
  n_disp <- as.integer(has_disp)
  times <- c(n_effect, n_hyper, n_hyperrand, n_disp)
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
  matrices_effect_outcome <- mod$matrices_effect_outcome
  matrix_effectfree_effect <- make_combined_matrix_effectfree_effect(mod)
  offset_effectfree_effect <- make_combined_offset_effectfree_effect(mod)
  transforms_hyper <- make_transforms_hyper(mod)
  ## transforms_hyperrand <- make_transforms_hyperrand(mod)
  ## WARNING The order of the following functions matters:
  ## each makes assumptions about the rows of 'draws'
  draws <- make_draws_comp_raw(mod)
  draws <- insert_draws_known(draws = draws,
                              est = est,
                              is_fixed = is_fixed)
  draws <- transform_draws_hyper(draws = draws,
                                 transforms = transforms_hyper)
  draws <- transform_draws_effect(draws = draws,
                                  matrix_effectfree_effect = matrix_effectfree_effect,
                                  offset_effectfree_effect = offset_effectfree_effect,
                                  matrices_effect_outcome = matrices_effect_outcome)
  if (!has_disp(mod))
    draws <- draws[-nrow(draws), , drop = FALSE]
  rownames(draws) <- NULL
  draws
}


## 'make_par_disp' ------------------------------------------------------------

## HAS_TESTS
#' Make modelled estimates for models
#' with dispersion term
#'
#' @param x A fitted 'bage_mod' object.
#' @param meanpar An rvec with posterior
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
make_par_disp <- function(x, meanpar, disp) {
    outcome <- x$outcome
    offset <- x$offset
    n_val <- length(outcome)
    n_draw <- rvec::n_draw(meanpar)
    ans <- rvec::rvec_dbl(matrix(NA, nrow = n_val, ncol = n_draw))
    is_na <- is.na(outcome) | is.na(offset)
    outcome <- outcome[!is_na]
    offset <- offset[!is_na]
    meanpar <- meanpar[!is_na]
    ans[!is_na] <- make_par_disp_inner(x = x,
                                       outcome = outcome,
                                       offset = offset,
                                       meanpar = meanpar,
                                       disp = disp)
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
  effect <- mod$levels_effect
  hyper <- make_levels_hyper(mod)
  hyperrand <- make_levels_hyperrand(mod)
  effect <- as.character(effect)
  hyper <- as.character(hyper)
  hyperrand <- as.character(hyperrand)
  ans <- c(effect, hyper, hyperrand)
  if (has_disp(mod))
    ans <- c(ans, "disp")
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
  ans <- unlist(ans)
  ans
}


## HAS_TESTS
#' Make Levels Associated with Each Element of 'hyperrand'
#'
#' Make levels for hyperparameters for each term
#'
#' @param mod An object of class 'bage_mod'.
#'
#' @returns A character vector.
#'
#' @noRd
make_levels_hyperrand <- function(mod) {
  priors <- mod$priors
  levels_effect <- mod$levels_effect
  terms_effect <- mod$terms_effect
  matrices_along_by <- choose_matrices_along_by(mod)
  levels_effect <- split(levels_effect, terms_effect)
  ans <- .mapply(levels_hyperrand,
                 dots = list(prior = priors,
                             matrix_along_by = matrices_along_by,
                             levels_effect = levels_effect),
                 MoreArgs = list())
  ans <- unlist(ans)
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
    ans <- factor(rep(ans, each = n_row_data),
                  levels = ans)
    ans
}


## HAS_TESTS
#' Make linear predictor from effect.
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
make_linpred_effect <- function(mod, components) {
    matrix_effect_outcome <- make_combined_matrix_effect_outcome(mod)
    matrix_effect_outcome <- Matrix::as.matrix(matrix_effect_outcome)
    is_effect <- components$component == "effect"
    effect <- components$.fitted[is_effect]
    matrix_effect_outcome %*% effect
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
        cli::cli_abort("Estimated precision matrix not positive definite.")  ## nocov
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
  effect <- mod$terms_effect
  hyper <- make_terms_hyper(mod)
  hyperrand <- make_terms_hyperrand(mod)
  effect <- as.character(effect)
  hyper <- as.character(hyper)
  hyperrand <- as.character(hyperrand)
  ans <- c(effect, hyper, hyperrand)
  if (has_disp(mod))
    ans <- c(ans, "disp")
  ans
}


## HAS_TESTS
#' Make List of Transforms to be Applied to
#' Hyper-Parameters
#'
#' Includes ordinary hyper-parameters
#' ("hyper"), hyper-parameters that can be
#' treated as random effects ("hyperrand")
#' and disperion, if present.
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
  matrices_along_by <- choose_matrices_along_by(mod)
  has_disp <- has_disp(mod)
  n_effectfree <- length(est$effectfree)
  nms_effectfree <- names(est$effectfree)
  ans_effectfree <- rep(list(NULL), times = n_effectfree)
  names(ans_effectfree) <- nms_effectfree
  ans_hyper <- lapply(priors, transform_hyper)
  ans_hyper <- unlist(ans_hyper, recursive = FALSE)
  ans_hyperrand <- .mapply(transform_hyperrand,
                           dots = list(prior = priors,
                                       matrix_along_by = matrices_along_by),
                           MoreArgs = list())
  ans_hyperrand <- unlist(ans_hyperrand, recursive = FALSE)
  ans_log_disp <- if (has_disp) exp else NULL
  ans_log_disp = list(log_disp = ans_log_disp)
  c(ans_effectfree,
    ans_hyper,
    ans_hyperrand,
    ans_log_disp)
}


## HAS_TESTS
#' Reformat Parts of 'components' Data Frame Dealing with
#' Hyper-Parameters that are Treated as Random Effects
#'
#' @param components A data frame
#' @param mod An object of class 'bage_mod'.
#'
#' @returns A modified versino of 'components'
#'
#' @noRd
reformat_hyperrand <- function(components, mod) {
  priors <- mod$priors
  nms_priors <- names(priors)
  matrices_along_by <- choose_matrices_along_by(mod)
  for (i_prior in seq_along(priors)) {
    prior <- priors[[i_prior]]
    nm_prior <- nms_priors[[i_prior]]
    matrix_along_by <- matrices_along_by[[i_prior]]
    components <- reformat_hyperrand_one(prior = prior,
                                         nm_prior = nm_prior,
                                         matrix_along_by = matrix_along_by,
                                         components = components)
  }
  components
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
#' Convert Rvec Columns to Numeric Columns by Taking Means
#'
#' @param data A data frame
#'
#' @return A data frame
#'
#' @noRd
rvec_to_mean <- function(data) {
  is_rvec <- vapply(data, rvec::is_rvec, TRUE)
  data[is_rvec] <- lapply(data[is_rvec], rvec::draws_mean)
  data
}


## HAS_TESTS
#' Order 'components' by 'term' and Then By 'term'
#'
#' @param components A tibble - typically the output
#' from function 'components'
#'
#' @returns A reordered version of 'components'
#'
#' @noRd
sort_components <- function(components, mod) {
  levels_component <- c("effect",
                        "trend", "cyclical", "seasonal", "error",
                        "disp",
                        "hyper")
  formula <- mod$formula
  term <- components$term
  component <- components$component
  terms_formula <- stats::terms(formula)
  levels_term <- attr(terms_formula, "term.labels")
  if (attr(terms_formula, "intercept"))
    levels_term <- c("(Intercept)", levels_term)
  i_term <- match(term, levels_term)
  i_comp <- match(components$component, levels_component, nomatch = 0L)
  i_invalid_comp <- match(0L, i_comp, nomatch = 0L)
  if (i_invalid_comp > 0L) {
    val <- components$component[[i_invalid_comp]]
    cli::cli_abort("Internal error: {.val {val}} not a valid value for {.var component}.")  ## nocov
  }
  ord <- order(i_term, i_comp)
  components[ord, , drop = FALSE]
}
  
## HAS_TESTS
#' Convert 'effectfree' part of draws to 'effect'
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
#' effectfree to effect
#' @param offset Offset converting all
#' effectfree to effect
#'
#' @returns A matrix
#'
#' @noRd
transform_draws_effect <- function(draws,
                                   matrix_effectfree_effect,
                                   offset_effectfree_effect,
                                   matrices_effect_outcome) {
  ## extract 'effectfree'
  n_effectfree <- ncol(matrix_effectfree_effect)
  n_val <- nrow(draws)
  is_effectfree <- seq_len(n_val) <= n_effectfree
  effectfree <- draws[is_effectfree, , drop = FALSE]
  not_effectfree <- draws[!is_effectfree, , drop = FALSE]
  ## transform 'effectfree' into 'effect'
  effects_raw <- matrix_effectfree_effect %*% effectfree + offset_effectfree_effect
  ## standardise effects
  matrix_effect_outcome <- Reduce(Matrix::cbind2, matrices_effect_outcome)
  matrix_effect_outcome <- Matrix::as.matrix(matrix_effect_outcome)
  linpred <- matrix_effect_outcome %*% effects_raw
  n_effect <- length(matrices_effect_outcome)
  effects_standard <- vector(mode = "list", length = n_effect)
  for (i_effect in seq_len(n_effect)) {
    M <- matrices_effect_outcome[[i_effect]]
    n <- Matrix::colSums(M)
    effect <- (Matrix::t(M) %*% linpred) / n ## works with Matrix
    linpred <- linpred - M %*% effect
    effects_standard[[i_effect]] <- Matrix::as.matrix(effect)
  }
  if (any(abs(linpred) > 0.001))
    cli::cli_abort("Internal error: Final residual not 0")  ## nocov
  effects_standard <- do.call(rbind, effects_standard)
  ## combine and return
  rbind(effects_standard,
        not_effectfree)
}


## HAS_TESTS
#' Transform hyper-parameters, and possibly dispersion
#'
#' Transform hyper-parameters, including
#' hyper-parameters that can be treated as
#' random effects and any dispersion term.
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

