

## HAS_TESTS
#' Center Values Within and Across Each Combination of By Variables
#'
#' @param x A numeric vector or rvec
#' @param matrix_along_by Mapping matrix for 'x'
#'
#' @returns A modifed version of 'x'
#'
#' @noRd
center_within_across_by <- function(x, matrix_along_by) {
  n_along <- nrow(matrix_along_by)
  n_by <- ncol(matrix_along_by)
  for (i_by in seq_len(n_by)) {
    i_along <- matrix_along_by[, i_by] + 1L
    x[i_along] <- x[i_along] - mean(x[i_along])
  }
  if (n_by > 1L) {
    for (i_along in seq_len(n_along)) {
      i_by <- matrix_along_by[i_along, ] + 1L
      x[i_by] <- x[i_by] - mean(x[i_by])
    }
  }
  x
}


## HAS_TESTS
#' Version of 'center_within_across_by' for Matrix of Draws
#'
#' @param draws A matrix, each column of which is a draw
#' @param matrix_along_by Mapping matrix for a column of 'draws'
#'
#' @returns A modified version of 'draws'
#'
#' @noRd
center_within_across_by_draws <- function(draws, matrix_along_by) {
  n_along <- nrow(matrix_along_by)
  n_by <- ncol(matrix_along_by)
  for (i_by in seq_len(n_by)) {
    indices_along <- matrix_along_by[, i_by] + 1L
    means <- colMeans(draws[indices_along, , drop = FALSE])
    draws[indices_along, ] <- draws[indices_along, ] - rep(means, each = n_along)
  }
  if (n_by > 1L) {
    for (i_along in seq_len(n_along)) {
      indices_by <- matrix_along_by[i_along, ] + 1L
      means <- colMeans(draws[indices_by, , drop = FALSE])
      draws[indices_by, ] <- draws[indices_by, ] - rep(means, each = n_by)
    }
  }
  draws
}


## HAS_TESTS
#' Return Values for Higher-Level Parameters from Fitted Model
#'
#' @param mod A fitted object of class 'bage_mod'
#' @param standardize Whether to standardize
#' estimates
#'
#' @returns A tibble
#'
#' @noRd
draw_vals_components_fitted <- function(mod, standardize) {
  term <- make_term_components(mod)
  comp <- make_comp_components(mod)
  level <- make_level_components(mod)
  draws <- make_draws_components(mod, standardize = standardize)
  ans <- tibble::tibble(term = term,
                        component = comp,
                        level = level,
                        .fitted = draws)
  ans <- reformat_hyperrand(components = ans, mod = mod)
  ans
}


## HAS_TESTS
#' Extract Estimates for Non-Time-Varying Effects
#'
#' @param components Output from 'components' function
#' @param mod Object of class 'bage_mod'
#'
#' @returns A tibble
#'
#' @noRd
get_comp_nontime_effects <- function(components, mod) {
  priors <- mod$priors
  var_time <- mod$var_time
  nms <- names(priors)
  is_time_varying_one <- function(nm) var_time %in% strsplit(nm, split = ":")[[1L]]
  is_time_varying <- vapply(nms, is_time_varying_one, TRUE)
  nms_time_varying <- nms[is_time_varying]
  is_non_time_varying <- !(components$term %in% nms_time_varying)
  is_effect <- components$component == "effect"
  components[is_non_time_varying & is_effect, , drop = FALSE]
}


## HAS_TESTS
#' Extract Values for Dispersion
#'
#' Works with fitted or unfitted models
#'
#' @param mod Object of class 'bage_mod'
#'
#' @returns A tibble
#'
#' @noRd
get_disp <- function(mod) {
  if (is_fitted(mod)) {
    ans <- mod$draws_disp
    ans <- matrix(ans, nrow = 1L)
    ans <- rvec::rvec_dbl(ans)
  }
  else {
    n_draw <- mod$n_draw
    ans <- draw_vals_disp(mod, n_sim = n_draw)
  }
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
  terms_spline <- make_term_spline(mod)
  terms_svd <- make_term_svd(mod)
  has_disp <- has_disp(mod)
  vals <- c("effect", "hyper", "hyperrand", "spline", "svd", "disp")
  n_effect <- length(terms_effect)
  n_hyper <- length(est$hyper)
  n_hyperrand <- length(est$hyperrand)
  n_spline <- length(terms_spline)
  n_svd <- length(terms_svd)
  n_disp <- as.integer(has_disp)
  times <- c(n_effect, n_hyper, n_hyperrand, n_spline, n_svd, n_disp)
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
#' Make draws from posterior distribution of
#' all parameters other than lowest-level
#' rates/probs/means
#'
#' @param mod Object of class 'bage_mod'
#' @param standardize Whether to standardize
#' estimates
#'
#' @returns A matrix
#'
#' @noRd
make_draws_components <- function(mod, standardize) {
  ## effects
  effectfree <- mod$draws_effectfree
  ans_effects <- make_effects(mod = mod, effectfree = effectfree)
  if (standardize)
    ans_effects <- standardize_effects(mod = mod, effects = ans_effects)
  ans_effects <- rvec::rvec_dbl(ans_effects)
  ## hyper
  hyper <- mod$draws_hyper
  ans_hyper <- rvec::rvec_dbl(hyper)
  ## hyperrand
  hyperrand <- mod$draws_hyperrand
  ans_hyperrand <- rvec::rvec_dbl(hyperrand)
  ## spline
  ans_spline <- make_spline(mod = mod, effectfree = effectfree)
  if (standardize)
    ans_spline <- standardize_spline(mod = mod, spline = ans_spline)
  ans_spline <- rvec::rvec_dbl(ans_spline)
  ## svd
  ans_svd <- make_svd(mod = mod, effectfree = effectfree)
  if (standardize)
    ans_svd <- standardize_svd(mod = mod, svd = ans_svd)
  ans_svd <- rvec::rvec_dbl(ans_svd)
  ## combine
  ans <- c(ans_effects, ans_hyper, ans_hyperrand, ans_spline, ans_svd)
  ## disp
  if (has_disp(mod)) {
    disp <- mod$draws_disp
    disp <- matrix(disp, nrow = 1L)
    ans_disp <- rvec::rvec_dbl(disp)
    ans <- c(ans, ans_disp)
  }
  ## return
  ans <- unname(ans)
  ans
}


## HAS_TESTS
#' Make Draws for Dispersion Parameter
#'
#' Includes transforming back to natural units.
#'
#' @param mod A fitted object of class "bage_mod"
#' @param draws_post Posterior draws for all parameters
#' estimated in TMB. Output from 'make_draws_post'.
#'
#' @returns A numeric vector.
#' 
#' @noRd
make_draws_disp <- function(mod, draws_post) {
  n_val <- nrow(draws_post)
  ans <- draws_post[n_val, ]
  ans <- exp(ans)
  ans
}
 

## HAS_TESTS
#' Make Draws from Free Effect Parameters
#'
#' Make draws from 'effectfree' (as opposed
#' to 'effect', the values that users see.)
#
#' @param mod A fitted object of class "bage_mod"
#' @param draws_post Posterior draws for all parameters
#' estimated in TMB. Output from 'make_draws_post'.
#'
#' @returns A matrix
#' 
#' @noRd
make_draws_effectfree <- function(mod, draws_post) {
  n_effectfree <- length(mod$est$effectfree)
  i_effectfree <- seq_len(n_effectfree)
  draws_post[i_effectfree, , drop = FALSE]
}


## HAS_TESTS
#' Make Draws from Hyper-Parameters
#'
#' Does not include hyperrand or disp.
#' Includes transforming back to natural units.
#'
#' @param mod A fitted object of class "bage_mod"
#' @param draws_post Posterior draws for all parameters
#' estimated in TMB. Output from 'make_draws_post'.
#'
#' @returns A matrix
#' 
#' @noRd
make_draws_hyper <- function(mod, draws_post) {
  n_effectfree <- length(mod$est$effectfree)
  n_hyper <- length(mod$est$hyper)
  i_hyper <- seq.int(from = n_effectfree + 1L, length.out = n_hyper)
  ans <- draws_post[i_hyper, , drop = FALSE]
  transforms <- make_transforms_hyper(mod)
  for (i in seq_along(transforms)) {
    transform <- transforms[[i]]
    if (!is.null(transform))
      ans[i, ] <- transform(ans[i, ])
  }
  ans
}


## HAS_TESTS
#' Make Draws from Hyper-Parameters that can be
#' Treated as Random Effects
#'
#' @param mod A fitted object of class "bage_mod"
#' @param draws_post Posterior draws for all parameters
#' estimated in TMB. Output from 'make_draws_post'.
#'
#' @returns A matrix
#' 
#' @noRd
make_draws_hyperrand <- function(mod, draws_post) {
  n_effectfree <- length(mod$est$effectfree)
  n_hyper <- length(mod$est$hyper)
  n_hyperrand <- length(mod$est$hyperrand)
  i_hyperrand <- seq.int(from = n_effectfree + n_hyper + 1L,
                         length.out = n_hyperrand)
  draws_post[i_hyperrand, , drop = FALSE]
}


## ## HAS_TESTS
## #' Make Draws from the Linear Predictor Formed from 'effectfree'
## #'
## #' @param mod A fitted object of class "bage_mod"
## #' @param draws_post Posterior draws for all parameters
## #' estimated in TMB. Output from 'make_draws_post'.
## #'
## #' @returns A matrix
## #' 
## #' @noRd
## make_linpred <- function(mod, effectfree) {
##   matrix_effectfree_effect <- make_combined_matrix_effectfree_effect(mod)
##   offset_effectfree_effect <- make_combined_offset_effectfree_effect(mod)
##   matrix_effect_outcome <- make_combined_matrix_effect_outcome(mod)
##   n_effectfree <- ncol(matrix_effectfree_effect)
##   n_val <- nrow(draws_post)
##   is_effectfree <- seq_len(n_val) <= n_effectfree
##   effectfree <- draws_post[is_effectfree, , drop = FALSE]
##   effect <- matrix_effectfree_effect %*% effectfree + offset_effectfree_effect
##   ans <- matrix_effect_outcome %*% effect
##   ans <- Matrix::as.matrix(ans)
##   ans
## }


## NO_TESTS
#' Construct Estimates of Effects from Estimates of Free Parameters
#'
#' @param mod A fitted object of class "bage_mod"
#' @param effectfree Posterior draws for free parameters
#'
#' @returns A matrix
#' 
#' @noRd
make_effects <- function(mod, effectfree) {
  matrix_effectfree_effect <- make_combined_matrix_effectfree_effect(mod)
  offset_effectfree_effect <- make_combined_offset_effectfree_effect(mod)
  ans <- matrix_effectfree_effect %*% effectfree + offset_effectfree_effect
  ans <- Matrix::as.matrix(ans)
  ans
}


## HAS_TESTS
#' Make Initial Draws from Posterior Distribution
#'
#' Make draws of variables included in 'prec'
#' matrix of TMB model output.
#'
#' If the Cholesky decomposition of
#' 'prec' was successful, then use that.
#' Otherwise, use the eigen decomposition.
#' Insert values for parameters that were
#' fixed via the 'map' function
#'
#' @param mod Object of class 'bage_mod'
#'
#' @returns A matrix
#'
#' @noRd
make_draws_post <- function(mod) {
  est <- unlist(mod$est)
  R_prec <- mod$R_prec
  scaled_eigen <- mod$scaled_eigen
  is_fixed <- mod$is_fixed
  n_draw <- mod$n_draw
  ans <- matrix(nrow = length(is_fixed),
                ncol = n_draw)
  mean <- est[!is_fixed]
  if (is.matrix(R_prec))
    draws_nonfixed <- rmvnorm_chol(n = n_draw,
                                   mean = mean,
                                   R_prec = R_prec)
  else
    draws_nonfixed <- rmvnorm_eigen(n = n_draw,
                                    mean = mean,
                                    scaled_eigen = scaled_eigen)
  ans[!is_fixed, ] <- draws_nonfixed
  ans[is_fixed, ] <- est[is_fixed]
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
  spline <- make_levels_spline(mod, unlist = TRUE)
  svd <- make_levels_svd(mod, unlist = TRUE)
  effect <- as.character(effect)
  hyper <- as.character(hyper)
  hyperrand <- as.character(hyperrand)
  spline <- as.character(spline)
  svd <- as.character(svd)
  ans <- c(effect, hyper, hyperrand, spline, svd)
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
#' Make Levels for 'spline' Coefficients
#'
#' @param mod An object of class 'bage_mod'.
#' @param unlist Whether to return a charcter vector
#'
#' @returns A named list or a character vector
#'
#' @noRd
make_levels_spline <- function(mod, unlist) {
  priors <- mod$priors
  matrices_along_by_free <- make_matrices_along_by_free(mod)
  ans <- vector(mode = "list", length = length(priors))
  for (i in seq_along(priors)) {
    prior <- priors[[i]]
    if (is_spline(prior)) {
      m <- matrices_along_by_free[[i]]
      labels_spline <- rownames(m)
      if (ncol(m) > 1L)
        labels_spline <- paste(labels_spline,
                               rep(colnames(m), each = nrow(m)),
                               sep = ".")
      ans[[i]] <- labels_spline
    }
  }
  if (unlist)
    ans <- unlist(ans, use.names = FALSE)
  else
    names(ans) <- names(priors)
  ans
}


## HAS_TESTS
#' Make Levels for 'svd' Coefficients
#'
#' @param mod An object of class 'bage_mod'.
#'
#' @returns A named list.
#'
#' @noRd
make_levels_svd <- function(mod, unlist) {
  priors <- mod$priors
  nms_priors <- names(priors)
  var_age <- mod$var_age
  var_sexgender <- mod$var_sexgender
  dimnames <- make_dimnames_terms(mod)
  ans <- vector(mode = "list", length = length(priors))
  paste_dot <- function(x, y) paste(x, y, sep = ".")
  for (i in seq_along(priors)) {
    prior <- priors[[i]]
    if (is_svd(prior)) {
      n_comp <- prior$specific$n_comp
      joint <- prior$specific$joint
      is_indep <- !is.null(joint) && !joint
      labels_svd <- paste0("comp", seq_len(n_comp))
      if (is_indep) {
        levels_sexgender <- dimnames[[i]][[var_sexgender]]
        n_sexgender <- length(levels_sexgender)
        labels_svd <- paste(rep(levels_sexgender, each = n_comp),
                            labels_svd,
                            sep = ".")
      }
      nm_split <- strsplit(nms_priors[[i]], split = ":")[[1L]]
      nm_split_noagesex <- setdiff(nm_split, c(var_age, var_sexgender))
      dn <- dimnames[[i]][nm_split_noagesex]
      dn <- c(list(labels_svd), dn)
      levels <- expand.grid(dn, KEEP.OUT.ATTRS = FALSE, stringsAsFactors = FALSE)
      levels <- Reduce(paste_dot, levels)
      ans[[i]] <- levels
    }
  }
  if (unlist)
    ans <- unlist(ans, use.names = FALSE)
  else
    names(ans) <- names(priors)
  ans
}


## HAS_TESTS
#' Make Linear Predictor from 'draws_effectfree'
#'
#' Can only be used with fitted models.
#'
#' Return value aligned to outcome, not data.
#'
#' @param mod Object of class "bage_mod"
#'
#' @returns An rvec
#'
#' @noRd
make_linpred_fitted <- function(mod) {
  matrix_effect_outcome <- make_combined_matrix_effect_outcome(mod)
  effectfree <- mod$draws_effectfree
  effect <- make_effects(mod = mod, effectfree = effectfree)
  ans <- matrix_effect_outcome %*% effect
  ans <- Matrix::as.matrix(ans)
  ans <- rvec::rvec(ans)
  ans
}


## HAS_TESTS
#' Make Linear Predictor from 'components'
#'
#' Only used with unfitted models.
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
make_linpred_unfitted <- function(mod, components) {
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
#' Extract Posterior Draws for Free Parameters used in Spline Priors
#'
#' @param mod Fitted object of class 'bage_mod'
#' @param mod effectfree Matrix with posterior draws of free parameters
#'
#' @returns A matrix
#'
#' @noRd
make_spline <- function(mod, effectfree) {
  priors <- mod$priors
  is_spline <- vapply(priors, is_spline, FALSE)
  lengths_effectfree <- make_lengths_effectfree(mod)
  is_spline <- rep(is_spline, times = lengths_effectfree)
  effectfree[is_spline, , drop = FALSE]
}


## HAS_TESTS
#' Extract Posterior Draws for Free Parameters used in SVD Priors
#'
#' @param mod Fitted object of class 'bage_mod'
#' @param mod effectfree Matrix with posterior draws of free parameters
#'
#' @returns A matrix
#'
#' @noRd
make_svd <- function(mod, effectfree) {
  priors <- mod$priors
  is_svd <- vapply(priors, is_svd, FALSE)
  lengths_effectfree <- make_lengths_effectfree(mod)
  is_svd <- rep(is_svd, times = lengths_effectfree)
  effectfree[is_svd, , drop = FALSE]
}


## HAS_TESTS
#' Make Factor Identifying Components
#' of Spline Parameter Vector
#'
#' Make factor the same length as
#' the estimates of spline coefficients,
#' giving the name of the term
#' that the each element belongs to.
#'
#' @param mod Object of class 'bage_mod'
#'
#' @returns A factor.
#'
#' @noRd
make_term_spline <- function(mod) {
  priors <- mod$priors
  nms_terms <- names(priors)
  lengths_effectfree <- make_lengths_effectfree(mod)
  is_spline <- vapply(priors, is_spline, FALSE)
  nms_terms_spline <- nms_terms[is_spline]
  lengths_spline <- lengths_effectfree[is_spline]
  ans <- rep(nms_terms_spline, times = lengths_spline)
  ans <- factor(ans, levels = nms_terms_spline)
  ans
}


## HAS_TESTS
#' Make Factor Identifying Components
#' of SVD Parameter Vector
#'
#' Make factor the same length as
#' the estimates of SVD coefficients,
#' giving the name of the term
#' that the each element belongs to.
#'
#' @param mod Object of class 'bage_mod'
#'
#' @returns A factor.
#'
#' @noRd
make_term_svd <- function(mod) {
  priors <- mod$priors
  nms_terms <- names(priors)
  lengths_effectfree <- make_lengths_effectfree(mod)
  is_svd <- vapply(priors, is_svd, FALSE)
  nms_terms_svd <- nms_terms[is_svd]
  lengths_svd <- lengths_effectfree[is_svd]
  ans <- rep(nms_terms_svd, times = lengths_svd)
  ans <- factor(ans, levels = nms_terms_svd)
  ans
}


## HAS_TESTS
#' Standardize Posterior Draws for Intercept, Main Effects, and Interactions
#'
#' @param mod Object of class 'bage_mod'.
#' @param effects A matrix of unstandardized effects,
#' where each column is a draw.
#' 
#' @returns A matrix
#'
#' @noRd
standardize_effects <- function(mod, effects) {
  eps <- 0.0001
  max_iter <- 100L
  matrices_effect_outcome <- mod$matrices_effect_outcome
  matrices_effect_outcome <- lapply(matrices_effect_outcome, Matrix::as.matrix)
  matrix_effect_outcome <- make_combined_matrix_effect_outcome(mod)
  is_rvec <- rvec::is_rvec(effects)
  if (is_rvec)
    effects <- as.matrix(effects)
  linpred <- matrix_effect_outcome %*% effects
  n_effect <- length(matrices_effect_outcome)
  n_outcome <- nrow(linpred)
  n_draw <- ncol(linpred)
  n_element <- vapply(matrices_effect_outcome, ncol, 1L)
  mult <- n_element / n_outcome
  ans <- .mapply(matrix,
                 dots = list(nrow = n_element),
                 MoreArgs = list(data = 0, ncol = n_draw))
  names(ans) <- names(mod$priors)
  for (i_effect in seq_len(n_effect))
    rownames(ans[[i_effect]]) <- colnames(matrices_effect_outcome[[i_effect]])
  found_ans <- FALSE
  for (i_iter in seq_len(max_iter)) {
    for (i_effect in seq_len(n_effect)) {
      M <- matrices_effect_outcome[[i_effect]]
      effect <- mult[[i_effect]] * Matrix::crossprod(M, linpred)
      linpred <- linpred - M %*% effect
      ans[[i_effect]] <- ans[[i_effect]] + effect
    }
    max_remainder <- max(abs(linpred))
    if (max_remainder < eps) {
      found_ans <- TRUE
      break
    }
  }
  if (!found_ans)
    cli::cli(c("Internal error: Unable to standardize effects.",   ## nocov
               i = "Maximum remainder: {.val {max_remainder}}."))  ## nocov
  ans <- lapply(ans, Matrix::as.matrix)
  ans <- Reduce(rbind, ans)
  if (is_rvec)
    ans <- rvec::rvec_dbl(ans)
  ans
}


## HAS_TESTS
#' Standardize Spline Coefficients
#'
#' @param mod Object of class 'bage_mod'.
#' @param svd Matrix of Unstandardized coefficients
#' 
#' @returns A matrix
#'
#' @noRd
standardize_spline <- function(mod, spline) {
  if (length(spline) == 0L)
    return(spline)
  is_rvec <- rvec::is_rvec(spline)
  if (is_rvec)
    spline <- as.matrix(spline)
  priors <- mod$priors
  lengths_effectfree <- make_lengths_effectfree(mod)
  matrices_along_by_free <- make_matrices_along_by_free(mod)
  is_spline <- vapply(priors, is_spline, FALSE)
  lengths_spline <- lengths_effectfree[is_spline]
  matrices_along_by_spline <- matrices_along_by_free[is_spline]
  start <- 1L
  for (i in seq_along(lengths_spline)) {
    length <- lengths_spline[[i]]
    stop <- start + length - 1L
    s <- seq.int(from = start, to = stop)
    matrix_along_by <- matrices_along_by_spline[[i]]
    spline[s, ] <- center_within_across_by_draws(spline[s, ],
                                                 matrix_along_by = matrix_along_by)
    start <- start + length
  }
  if (is_rvec)
    spline <- rvec::rvec_dbl(spline)
  spline
}


## HAS_TESTS
#' Standardize SVD Coefficients
#'
#' @param mod Object of class 'bage_mod'.
#' @param svd Matrix of Unstandardized coefficients
#' 
#' @returns A matrix
#'
#' @noRd
standardize_svd <- function(mod, svd) {
  if (length(svd) == 0L)
    return(svd)
  is_rvec <- rvec::is_rvec(svd)
  if (is_rvec)
    svd <- as.matrix(svd)
  priors <- mod$priors
  lengths_effectfree <- make_lengths_effectfree(mod)
  matrices_along_by_free <- make_matrices_along_by_free(mod)
  is_svd <- vapply(priors, is_svd, FALSE)
  lengths_svd <- lengths_effectfree[is_svd]
  matrices_along_by_svd <- matrices_along_by_free[is_svd]
  start <- 1L
  for (i in seq_along(lengths_svd)) {
    length <- lengths_svd[[i]]
    stop <- start + length - 1L
    s <- seq.int(from = start, to = stop)
    matrix_along_by <- matrices_along_by_svd[[i]]
    svd[s, ] <- center_within_across_by_draws(svd[s, ], matrix_along_by = matrix_along_by)
    start <- start + length
  }
  if (is_rvec)
    svd <- rvec::rvec_dbl(svd)
  svd
}


## HAS_TESTS
#' Make Draws Stored as Part of Model Object
#'
#' Draws created are
#' - 'draws_effectfree',
#' - 'draws_hyper',
#' - 'draws_hyperrand', and, optionally,
#' - 'draws_disp'.
#'
#' Reproducibility is achieved via 'seed_stored_draws'.
#'
#' @param mod A fitted 'bage_mod' object
#'
#' @returns Modified version of 'mod'
#'
#' @noRd
make_stored_draws <- function(mod) {
  if (!is_fitted(mod))
    cli::cli_abort("Internal error: Can't make stored draws for an unfitted model.")
  seed_stored_draws <- mod$seed_stored_draws
  seed_restore <- make_seed() ## create randomly-generated seed
  set.seed(seed_stored_draws) ## set pre-determined seed
  draws_post <- make_draws_post(mod)
  mod$draws_effectfree <- make_draws_effectfree(mod = mod, draws_post = draws_post)
  mod$draws_hyper <- make_draws_hyper(mod = mod, draws_post = draws_post)
  mod$draws_hyperrand <- make_draws_hyperrand(mod = mod, draws_post = draws_post)
  if (has_disp(mod))
    mod$draws_disp <- make_draws_disp(mod = mod, draws_post = draws_post)
  set.seed(seed_restore) ## set randomly-generated seed, to restore randomness
  mod
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
  spline <- make_term_spline(mod)
  svd <- make_term_svd(mod)
  effect <- as.character(effect)
  hyper <- as.character(hyper)
  hyperrand <- as.character(hyperrand)
  spline <- as.character(spline)
  svd <- as.character(svd)
  ans <- c(effect, hyper, hyperrand, spline, svd)
  if (has_disp(mod))
    ans <- c(ans, "disp")
  ans
}


## HAS_TESTS
#' Make List of Transforms to be Applied to
#' Hyper-Parameters
#'
#' Does not include hyper-parameters
#' that can be treated as random effects
#'  ("hyperrand") or dispersion.
#'
#' @param mod Object of class 'bage_mod'
#'
#' @returns A list, each element of which is a function or NULL
#'
#' @noRd
make_transforms_hyper <- function(mod) {
  est <- mod$est
  priors <- mod$priors
  matrices_along_by <- choose_matrices_along_by(mod)
  has_disp <- has_disp(mod)
  ans <- lapply(priors, transform_hyper)
  ans <- unlist(ans, recursive = FALSE)
  ans
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
#' Reformat 'Components' Output for Terms with Fixed Seasonal Effect
#'
#' @param prior Object of class 'bage_prior'.
#' @param nm_prior Name of the prior (ie name of the term).
#' @param matrix_along_by Matrix with mapping for along, by dimensions
#' @param components A data frame.
#'
#' @returns A modifed version of 'components'
#'
#' @noRd
reformat_hyperrand_seasfix <- function(prior,
                                         nm_prior,
                                         matrix_along_by,
                                         components) {
  n_seas <- prior$specific$n_seas
  n_along <- nrow(matrix_along_by)
  n_by <- ncol(matrix_along_by)
  is_seas <- with(components,
                  term == nm_prior & component == "hyperrand")
  is_effect <- with(components,
                    term == nm_prior & component == "effect")  
  seas <- components$.fitted[is_seas]
  effect <- components$.fitted[is_effect]
  level <- components$level[is_effect]
  matrix_along_by_seas <- matrix(seq_along(seas) - 1L, nrow = n_seas, ncol = n_by)
  seas <- center_within_across_by(x = seas,
                                  matrix_along_by = matrix_along_by_seas)
  seas_extend <- rep(seas[[1L]], times = n_along * n_by)
  for (i_by in seq_len(n_by)) {
    for (i_along in seq_len(n_along)) {
      i_seas <- ((i_along - 1L) %% n_seas) + (i_by - 1L) * n_seas + 1L
      i_seas_extend <- matrix_along_by[i_along, i_by] + 1L
      seas_extend[i_seas_extend] <- seas[i_seas]
    }
  }
  seasonal <- tibble::tibble(term = nm_prior,
                             component = "seasonal",
                             level = level,
                             .fitted = seas_extend)
  trend <- effect - seas_extend
  trend <- tibble::tibble(term = nm_prior,
                          component = "trend",
                          level = level,
                          .fitted = trend)
  ## combine
  components <- components[!is_seas, , drop = FALSE]
  vctrs::vec_rbind(components, seasonal, trend)
}


## HAS_TESTS
#' Reformat 'Components' Output for Terms with Fixed Seasonal Effect
#'
#' @param prior Object of class 'bage_prior'.
#' @param nm_prior Name of the prior (ie name of the term).
#' @param matrix_along_by Matrix with mapping for along, by dimensions
#' @param components A data frame.
#'
#' @returns A modifed version of 'components'
#'
#' @noRd
reformat_hyperrand_seasvary <- function(prior,
                                        nm_prior,
                                        matrix_along_by,
                                        components) {
  ## seasonal
  is_seas <- with(components,
                  term == nm_prior & component == "hyperrand")
  seas <- components$.fitted[is_seas]
  seas <- center_within_across_by(x = seas,
                                  matrix_along_by = matrix_along_by)
  components$.fitted[is_seas] <- seas
  components$component[is_seas] <- "seasonal"
  ## trend
  is_effect <- with(components,
                    term == nm_prior & component == "effect")  
  effect <- components$.fitted[is_effect]
  trend <- effect - seas
  level <- components$level[is_effect]
  trend <- tibble::tibble(term = nm_prior,
                          component = "trend",
                          level = level,
                          .fitted = trend)
  ## combine
  vctrs::vec_rbind(components, trend)
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
#' Order 'components' Data Frame by 'term' and 'component'
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
                        "spline", "svd",
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
  


