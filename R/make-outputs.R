
## HAS_TESTS
#' Center Effects, SVD, Spline, Trend, Cyclical, Seasonal, Error
#'
#' @param components Data frame with estimates of hyper parameters
#' @param priors Named list of objects of class 'bage_prior'
#' @param dimnames_terms Dimnames for array representation of terms
#' @param var_time Name of time variable
#' @param var_age Name of age variable
#' @param var_sexgender Name of sex/gender variable
#' @param center_along Whether to center along the 'along' dimension
#' 
#' @returns A modifed version of 'components'
#'
#' @noRd
center_all <- function(components,
                       priors,
                       dimnames_terms,
                       var_time,
                       var_age,
                       var_sexgender,
                       center_along) {
  components <- center_effects(components = components,
                               priors = priors,
                               dimnames_terms = dimnames_terms,
                               var_time = var_time,
                               var_age = var_age,
                               center_along = center_along)
  components <- center_svd_spline(components = components,
                                  priors = priors,
                                  dimnames_terms = dimnames_terms,
                                  var_time = var_time,
                                  var_age = var_age,
                                  var_sexgender = var_sexgender,
                                  center_along = center_along)
  components <- center_trend_cyc_seas_err(components = components,
                                          priors = priors,
                                          dimnames_terms = dimnames_terms,
                                          var_time = var_time,
                                          var_age = var_age,
                                          center_along = center_along)
  components
}


## HAS_TESTS
#' Center Effects Along 'by' and (Optionally) 'along' Dimensions
#'
#' @param components Data frame with estimates of hyper parameters
#' @param priors Named list of objects of class 'bage_prior'
#' @param dimnames_terms Dimnames for array representation of terms
#' @param var_time Name of time variable
#' @param var_age Name of age variable
#' @param center_along Whether to center along the 'along' dimension
#' 
#' @returns A modifed version of 'components'
#'
#' @noRd
center_effects <- function(components,
                           priors,
                           dimnames_terms,
                           var_time,
                           var_age,
                           center_along) {
  key_comp <- with(components, paste(term, component, level))
  for (i_term in seq_along(priors)) {
    dimnames_term <- dimnames_terms[[i_term]]
    prior <- priors[[i_term]]
    nm_term_split <- dimnames_to_nm_split(dimnames_term)
    nm_term <- dimnames_to_nm(dimnames_term)
    levels_term <- dimnames_to_levels(dimnames_term)
    key_effect <- paste(nm_term, "effect", levels_term)
    i_effect <- match(key_effect, key_comp, nomatch = 0L)
    has_val <- any(i_effect > 0L)
    if (has_val) {
      effect <- components$.fitted[i_effect]
      is_intercept <- nm_term == "(Intercept)"
      if (!is_intercept) {
        uses_along <- uses_along(prior)
        if (uses_along) {
          along <- prior$specific$along
          matrix_along_by <- make_matrix_along_by_effect(along = along,
                                                         dimnames_term = dimnames_term,
                                                         var_time = var_time,
                                                         var_age = var_age)
          effect <- center_within_along_by(x = effect,
                                           matrix_along_by = matrix_along_by,
                                           center_along = center_along)
        }
        else {
          i_along <- 1L
          matrix_along_by <- make_matrix_along_by_inner(i_along = i_along,
                                                        dimnames_term = dimnames_term)
          effect <- center_within_along_by(x = effect,
                                           matrix_along_by = matrix_along_by,
                                           center_along = TRUE)
        }
      }
      components$.fitted[i_effect] <- effect
    }
  }
  components
}


## HAS_TESTS
#' Center SVD and Spline Coefficients
#'
#' @param components Data frame with estimates of hyper parameters
#' @param priors Named list of objects of class 'bage_prior'
#' @param dimnames_terms Dimnames for array representation of terms
#' @param var_time Name of time variable
#' @param var_age Name of age variable
#' @param var_sexgender Name of sex/gender variable
#' @param center_along Whether to center along the 'along' dimension
#' 
#' @returns A modifed version of 'components'
#'
#' @noRd
center_svd_spline <- function(components,
                              priors,
                              dimnames_terms,
                              var_time,
                              var_age,
                              var_sexgender,
                              center_along) {
  key_components <- with(components, paste(term, component, level))
  for (i_term in seq_along(priors)) {
    prior <- priors[[i_term]]
    is_svd <- is_svd(prior)
    is_spline <- is_spline(prior)
    if (is_svd || is_spline) {
      dimnames_term <- dimnames_terms[[i_term]]
      nm_split <- dimnames_to_nm_split(dimnames_term)
      nm <- dimnames_to_nm(dimnames_term)
      if (is_svd) {
        levels <- make_levels_svd_term(prior = prior,
                                       dimnames_term = dimnames_term,
                                       var_age = var_age,
                                       var_sexgender = var_sexgender)
        key_term <- "svd"
      }
      else {
        levels <- make_levels_spline_term(prior = prior,
                                          dimnames_term = dimnames_term,
                                          var_time = var_time,
                                          var_age = var_age)
        key_term <- "spline"
      }
      key_term <- paste(nm, key_term, levels)
      indices_comp <- match(key_term, key_components)
      val_term <- components$.fitted[indices_comp]
      matrix_along_by <- make_matrix_along_by_effectfree(prior = prior,
                                                         dimnames_term = dimnames_term,
                                                         var_time = var_time,
                                                         var_age = var_age,
                                                         var_sexgender = var_sexgender)
      val_term <- center_within_along_by(x = val_term, 
                                         matrix_along_by = matrix_along_by,
                                         center_along)
      components$.fitted[indices_comp] <- val_term
    }
  }
  components
}


## HAS_TESTS
#' Center Trend, Cyclical, Seasonal, and Error Components of Terms
#'
#' @param components Data frame with estimates of hyper parameters
#' @param priors Named list of objects of class 'bage_prior'
#' @param dimnames_terms Dimnames for array representation of terms
#' @param var_time Name of time variable
#' @param var_age Name of age variable
#' @param center_along Whether to center along the 'along' dimension
#' 
#' @returns A modifed version of 'components'
#'
#' @noRd
center_trend_cyc_seas_err <- function(components,
                                      priors,
                                      dimnames_terms,
                                      var_time,
                                      var_age,
                                      center_along) {
  nms_tcse <- c("trend", "cyclical", "seasonal", "error")
  key_comp <- with(components, paste(term, component, level))
  for (i_term in seq_along(priors)) {
    dimnames_term <- dimnames_terms[[i_term]]
    nm_term_split <- dimnames_to_nm_split(dimnames_term)
    nm_term <- dimnames_to_nm(dimnames_term)
    levels_term <- dimnames_to_levels(dimnames_term)
    for (nm_tcse in nms_tcse) {
      key_tcse <- paste(nm_term, nm_tcse, levels_term)
      i_tcse <- match(key_tcse, key_comp, nomatch = 0L)
      has_val <- any(i_tcse > 0L)
      if (has_val) {
        prior <- priors[[i_term]]
        dimnames_term <- dimnames_terms[[i_term]]
        tcse <- components$.fitted[i_tcse]
        if (!uses_along(prior))
          cli::cli_abort("Internal error: Prior for term {.val {nm_term}} does not use along.")
        along <- prior$specific$along
        matrix_along_by <- make_matrix_along_by_effect(along = along,
                                                       dimnames_term = dimnames_term,
                                                       var_time = var_time,
                                                       var_age = var_age)
        tcse <- center_within_along_by(x = tcse,
                                       matrix_along_by = matrix_along_by,
                                       center_along = center_along)
        components$.fitted[i_tcse] <- tcse
      }
    }
  }
  components
} 


## HAS_TESTS
#' Center Values Within Each Value of 'along'
#' Variable and (Optionally) Each Value
#' of 'by' Variable
#' 
#' @param x A numeric vector or rvec
#' @param matrix_along_by Mapping matrix for 'x'
#' @param center_along Whether to center within 'along'
#' variable
#'
#' @returns A modifed version of 'x'
#'
#' @noRd
center_within_along_by <- function(x, matrix_along_by, center_along) {
  n_along <- nrow(matrix_along_by)
  n_by <- ncol(matrix_along_by)
  if (center_along) {
    for (i_by in seq_len(n_by)) {
      i_along <- matrix_along_by[, i_by] + 1L
      x[i_along] <- x[i_along] - mean(x[i_along])
    }
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
  data <- mod$data
  priors <- mod$priors
  dimnames_terms <- mod$dimnames_terms
  var_time <- mod$var_time
  var_age <- mod$var_age
  var_sexgender <- mod$var_sexgender
  term <- make_term_components(mod)
  comp <- make_comp_components(mod)
  level <- make_level_components(mod)
  draws <- make_draws_components(mod)
  ans <- tibble::tibble(term = term,
                        component = comp,
                        level = level,
                        .fitted = draws)
  ans <- infer_trend_cyc_seas_err(components = ans,
                                  priors = priors,
                                  dimnames_terms = dimnames_terms,
                                  var_time = var_time,
                                  var_age = var_age)
  if (standardize == "terms") {
    ans <- center_all(components = ans,
                      priors = priors,
                      dimnames_terms = dimnames_terms,
                      var_time = var_time,
                      var_age = var_age,
                      var_sexgender = var_sexgender,
                      center_along = TRUE)
  }
  else if (standardize == "anova") {
    linpred <- make_linpred_comp(components = ans,
                                 data = data,
                                 dimnames_terms = dimnames_terms)
    ans <- standardize_anova(components = ans,
                             data = data,
                             linpred = linpred,
                             dimnames_terms = dimnames_terms)
  }
  else if (standardize == "none") {
    NULL
  }
  else
    cli::cli_abort("Internal error: Invalid value for {.arg standardize}.")
  ans
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


#' Insert a Into a Data Frame
#'
#' Insert a variable into a dataframe, immediately
#' after another variable.
#'
#' Currently assumes that not inserting
#' at start of 'df'.
#'
#' @param df A data frame (including a tibble)
#' @param nm_after Name of the variable that the
#' 'x' should come after
#' @param x New variable
#' @param nm_x Name of new variable
#'
#' @returns A modified version of 'df'
#'
#' @noRd
insert_after <- function(df, nm_after, x, nm_x) {
  nms_df <- names(df)
  n_df <- length(nms_df)
  i_after <- match(nm_after, names(df))
  if (i_after < n_df) {
    s_before <- seq_len(i_after)
    s_after <- seq.int(from = i_after + 1L, to = n_df)
    ans <- vctrs::vec_cbind(df[s_before],
                            x,
                            df[s_after],
                            .name_repair = "universal_quiet")
  }
  else {
    ans <- vctrs::vec_cbind(df,
                            x,
                            .name_repair = "universal_quiet")
  }
  names(ans)[[i_after + 1L]] <- nm_x
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
#'
#' @returns A matrix
#'
#' @noRd
make_draws_components <- function(mod) {
  ## effects
  effectfree <- mod$draws_effectfree
  ans_effects <- make_effects(mod = mod, effectfree = effectfree)
  ans_effects <- rvec::rvec_dbl(ans_effects)
  ## hyper
  hyper <- mod$draws_hyper
  ans_hyper <- rvec::rvec_dbl(hyper)
  ## hyperrand
  hyperrand <- mod$draws_hyperrand
  ans_hyperrand <- rvec::rvec_dbl(hyperrand)
  ## spline
  ans_spline <- make_spline(mod = mod, effectfree = effectfree)
  ans_spline <- rvec::rvec_dbl(ans_spline)
  ## svd
  ans_svd <- make_svd(mod = mod, effectfree = effectfree)
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
  dimnames(vctrs::field(ans, "data")) <- NULL ## TODO - remove this when rvec updated
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


## HAS_TESTS
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
  dimnames_terms <- mod$dimnames_terms
  var_time <- mod$var_time
  var_age <- mod$var_age
  levels_effect <- mod$levels_effect
  terms_effect <- mod$terms_effect
  levels_effect <- split(levels_effect, terms_effect)
  ans <- .mapply(levels_hyperrand,
                 dots = list(prior = priors,
                             dimnames_term = dimnames_terms,
                             levels_effect = levels_effect),
                 MoreArgs = list(var_time = var_time,
                                 var_age = var_age))
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
  dimnames_terms <- mod$dimnames_terms
  var_time <- mod$var_time
  var_age <- mod$var_age
  ans <- vector(mode = "list", length = length(priors))
  for (i in seq_along(priors)) {
    prior <- priors[[i]]
    if (is_spline(prior)) {
      dimnames_term <- dimnames_terms[[i]]
      ans[[i]] <- make_levels_spline_term(prior = prior,
                                          dimnames_term = dimnames_term,
                                          var_time = var_time,
                                          var_age = var_age)
    }
  }
  if (unlist)
    ans <- unlist(ans, use.names = FALSE)
  else
    names(ans) <- names(priors)
  ans
}


#' Make Levels for Spline
#'
#' @param prior Object of class 'bage_prior_spline'
#' @param dimnames_term Dimnames for array representation
#' of term
#' @param var_time Name of time variable
#' @param var_age Name of age variable
#'
#' @returns A character vector
#'
#' @noRd
make_levels_spline_term <- function(prior,
                                    dimnames_term,
                                    var_time,
                                    var_age) {
  along <- prior$specific$along
  i_along <- make_i_along(along = along,
                          dimnames_term = dimnames_term,
                          var_time = var_time,
                          var_age = var_age)
  n_along <- length(dimnames_term[[i_along]])
  n_comp <- get_n_comp_spline(prior = prior,
                              n_along = n_along)
  levels_along <- paste0("comp", seq_len(n_comp))
  dimnames_term <- c(list(.spline = levels_along),
                     dimnames_term[-i_along])
  dimnames_to_levels(dimnames_term)
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
  dimnames_terms <- mod$dimnames_terms
  ans <- vector(mode = "list", length = length(priors))
  for (i in seq_along(priors)) {
    prior <- priors[[i]]
    if (is_svd(prior)) {
      dimnames_term <- dimnames_terms[[i]]
      ans[[i]] <- make_levels_svd_term(prior = prior,
                                       dimnames_term = dimnames_term,
                                       var_age = var_age,
                                       var_sexgender = var_sexgender)
    }
  }
  if (unlist)
    ans <- unlist(ans, use.names = FALSE)
  else
    names(ans) <- names(priors)
  ans
}


## HAS_TESTS
#' Make Levels for 'svd' Coefficients for a Single Term
#'
#' Term is assumed to have a SVD-based prior
#'
#' @param prior Object of class 'bage_prior'
#' @param dimnames_term Dimnames for array representation of term
#' @param var_age Name of age variable
#' @param var_sexgender Name of sex/gender variable
#'
#' @returns A character vector.
#'
#' @noRd
make_levels_svd_term <- function(prior,
                                 dimnames_term,
                                 var_age,
                                 var_sexgender) {
  labels_svd <- get_labels_svd(prior = prior,
                               dimnames_term = dimnames_term,
                               var_sexgender = var_sexgender)
  nms <- names(dimnames_term)
  nms_noagesex <- setdiff(nms, c(var_age, var_sexgender))
  dimnames_noagesex <- dimnames_term[nms_noagesex]
  dimnames_svd <- c(list(.svd = labels_svd), dimnames_noagesex)
  dimnames_to_levels(dimnames_svd)
}


## HAS_TESTS
#' Calculate Trend Values for a Line or Lines
#'
#' @param intercept Intercept(s) of line(s). An rvec.
#' @param slope Slope(s) of line(s). An rvec.
#' @param matrix_along_by Matrix mapping
#' along and by dimensions to position in estimates
#'
#' @returns An rvec
#'
#' @noRd
make_lin_trend <- function(intercept,
                           slope,
                           matrix_along_by) {
  n_along <- nrow(matrix_along_by)
  n_by <- ncol(matrix_along_by)
  intercept <- rep(intercept, each = n_along)
  slope <- rep(slope, each = n_along)
  s <- rep(seq_len(n_along), times = n_by)
  ans <- intercept + slope * s
  i <- match(sort(matrix_along_by), matrix_along_by)
  ans[i]
}


#' Make Linear Predictor from Components
#'
#' @param components Data frame with estimates for hyper-parameters
#' @param data Data frame with raw data
#' @param dimnames_terms Dimnames for array representation of terms
#'
#' @returns An rvec
#'
#' @noRd
make_linpred_comp <- function(components, data, dimnames_terms) {
  key_comp <- with(components, paste(term, component, level))
  fitted <- components$.fitted
  data_has_intercept <- "(Intercept)" %in% names(data)
  if (!data_has_intercept)
    data[["(Intercept)"]] <- "(Intercept)"
  n_draw <- rvec::n_draw(fitted)
  ans <- matrix(0, nrow = nrow(data), ncol = n_draw)
  ans <- rvec::rvec_dbl(ans)
  for (i_term in seq_along(dimnames_terms)) {
    dimnames_term <- dimnames_terms[[i_term]]
    nm_split <- dimnames_to_nm_split(dimnames_term)
    nm <- dimnames_to_nm(dimnames_term)
    levels_term <- dimnames_to_levels(dimnames_term)
    key_term <- paste(nm, "effect", levels_term)
    indices_comp <- match(key_term, key_comp)
    val_term <- fitted[indices_comp]
    levels_data <- Reduce(paste_dot, data[nm_split])
    indices_term <- match(levels_data, levels_term)
    val_term_linpred <- val_term[indices_term]
    ans <- ans + val_term_linpred
  }
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
make_linpred_raw <- function(mod) {
  matrix_effect_outcome <- make_combined_matrix_effect_outcome(mod)
  effectfree <- mod$draws_effectfree
  effect <- make_effects(mod = mod, effectfree = effectfree)
  ans <- matrix_effect_outcome %*% effect
  ans <- Matrix::as.matrix(ans)
  ans <- rvec::rvec(ans)
  ans
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



#' Make Standardized Version of a Single Effect
#'
#' @param linpred Linear predictor. An rvec
#' @param indices_linpred Indices into linear predictor
#' from elements of term. An integer vector.
#'
#' @returns An rvec.
#'
#' @noRd
make_standardized_effect <- function(linpred, indices_linpred) {
  n_element <- max(indices_linpred)
  n_draw <- rvec::n_draw(linpred)
  linpred <- as.matrix(linpred)
  ans <- matrix(nrow = n_element, ncol = n_draw)
  for (i_element in seq_len(n_element)) {
    maps_to_element <- indices_linpred == i_element
    ans[i_element, ] <- matrixStats::colMeans2(linpred,
                                               rows = maps_to_element,
                                               useNames = FALSE)
  }
  ans <- rvec::rvec_dbl(ans)
  ans
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


#' Paste Two Vectors Separated by a Dot
#'
#' @param x A vector
#' @param y A vector
#'
#' @returns A character vector
#'
#' @noRd
paste_dot <- function(x, y) paste(x, y, sep = ".")


#' Given Estimates of Effect and Slope, Rescale Intercept for Lines
#'
#' @param slope Rvec of length n_by holding slope estimates
#' @param effect Rvec holding estimates for effect
#' @param matrix_along_by Mapping matrix
#'
#' @returns An rvec of length 'n_by'
#'
#' @noRd
rescale_lin_intercept <- function(slope, effect, matrix_along_by) {
  n_along <- nrow(matrix_along_by)
  n_by <- ncol(matrix_along_by)
  ans <- slope
  for (i_by in seq_len(n_by)) {
    i_along <- matrix_along_by[, i_by] + 1L
    mean_effect <- mean(effect[i_along])
    mean_incr <- 0.5 * (1 + n_along) * slope[[i_by]]
    ans[[i_by]] <- mean_effect - mean_incr
  }
  ans
}


## HAS_TESTS
#' Standardize Posterior Draws for Intercept, Main Effects, and Interactions
#' following 'ANOVA' Conventions
#'
#' @param components Data frame with estimates of hyper-parameters
#' @param data Data frame with original data
#' @param linpred Rvec with linear predictor formed from effects
#' @param dimnames_terms Dimnames of array representation of terms.
#' A named list.
#'
#' @returns Modifed version of 'components'
#'
#' @noRd
standardize_anova <- function(components, data, linpred, dimnames_terms) {
  max_resid_permitted <- 0.001
  key_comp <- with(components, paste(term, component, level))
  data_has_intercept <- "(Intercept)" %in% names(data)
  if (!data_has_intercept)
    data[["(Intercept)"]] <- "(Intercept)"
  for (i_term in seq_along(dimnames_terms)) {
    dimnames_term <- dimnames_terms[[i_term]]
    nm_split <- dimnames_to_nm_split(dimnames_term)
    nm <- dimnames_to_nm(dimnames_term)
    levels_term <- dimnames_to_levels(dimnames_term)
    levels_data <- Reduce(paste_dot, data[nm_split])
    indices_linpred <- match(levels_data, levels_term)
    standardized_effect <- make_standardized_effect(linpred = linpred,
                                                    indices_linpred = indices_linpred)
    standardized_effect_linpred <- standardized_effect[indices_linpred]
    linpred <- linpred - standardized_effect_linpred
    key_term <- paste(nm, "effect", levels_term)
    indices_comp <- match(key_term, key_comp)
    components$.fitted[indices_comp] <- standardized_effect
  }
  max_resid <- max(abs(as.matrix(linpred)))
  if (max_resid > max_resid_permitted)
    cli::cli_alert_warning(paste("Standardized values do not exactly reproduce", ## nocov
                                 "linear predictor:",                            ## nocov
                                 "Maximum difference is {.val {max_resid}}."))   ## nocov
  components
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
  priors <- mod$priors
  ans <- lapply(priors, transform_hyper)
  ans <- unlist(ans, recursive = FALSE)
  ans
}


## HAS_TESTS
#' Derive Parts of 'components' Data Frame Dealing with
#' Hyper-Parameters that are Treated as Random Effects - Estimates
#'
#' @param components A data frame
#' @param priors List of objects of class 'bage_prior'.
#' @param dimnames_terms Dimnames for array representations of terms
#' @param var_time Name of time variable
#' @param var_age Name of age variable
#'
#' @returns A modified version of 'components'
#'
#' @noRd
infer_trend_cyc_seas_err <- function(components,
                                     priors,
                                     dimnames_terms,
                                     var_time,
                                     var_age) {
  for (i_prior in seq_along(priors)) {
    prior <- priors[[i_prior]]
    dimnames_term <- dimnames_terms[[i_prior]]
    components <- infer_trend_cyc_seas_err_one(prior = prior,
                                               dimnames_term = dimnames_term,
                                               var_time = var_time,
                                               var_age = var_age,
                                               components = components)
  }
  components
}


## HAS_TESTS
#' Reformat Parts of Forecasted 'components' Data Frame Dealing with
#' Hyper-Parameters that are Treated as Random Effects
#'
#' @param components_forecast A data frame with forecasted
#' (time-varying) parameters
#' @param components A data frame with estimated
#' (time-varying and non-time-varying) parameters
#' @param priors List of objects of class 'bage_prior'.
#' @param dimnames_terms Dimnames for array representations of terms
#' being forecast
#' @param var_time Name of time variable
#' @param var_age Name of age variable
#'
#' @returns A modified version of 'components'
#'
#' @noRd
infer_trend_cyc_seas_err_forecast <- function(components,
                                              priors,
                                              dimnames_terms,
                                              var_time,
                                              var_age) {
  nms <- names(dimnames_terms)
  for (nm in nms) {
    prior <- priors[[nm]]
    dimnames_term <- dimnames_terms[[nm]]
    components <- infer_trend_cyc_seas_err_forecast_one(prior = prior,
                                                        dimnames_term = dimnames_term,
                                                        var_time = var_time,
                                                        var_age = var_age,
                                                        components = components)
  }
  components
}


## HAS_TESTS
#' Reformat 'Components' Output for Terms with Fixed Seasonal Effect
#' - Estimates
#'
#' Derive 'trend' from 'effect' and 'seasonal'
#' 
#' @param prior Object of class 'bage_prior'.
#' @param dimnames_term Dimnames for array representation of term
#' @param var_time Name of time variable
#' @param var_age Name of age variable
#' @param components A data frame.
#'
#' @returns A modifed version of 'components'
#'
#' @noRd
infer_trend_cyc_seas_err_seasfix <- function(prior,
                                             dimnames_term,
                                             var_time,
                                             var_age,
                                             components) {
  n_seas <- prior$specific$n_seas
  along <- prior$specific$along
  nm <- dimnames_to_nm(dimnames_term)
  is_seasonal <- with(components, (term == nm) & (component == "hyperrand"))
  is_effect <- with(components, (term == nm) & (component == "effect"))
  seas <- components$.fitted[is_seasonal]
  effect <- components$.fitted[is_effect]
  matrix_along_by_effect <- make_matrix_along_by_effect(along = along,
                                                        dimnames_term = dimnames_term,
                                                        var_time = var_time,
                                                        var_age = var_age)
  n_along <- nrow(matrix_along_by_effect)
  n_by <- ncol(matrix_along_by_effect)
  matrix_along_by_seas <- matrix(seq_along(seas) - 1L, nrow = n_seas, ncol = n_by)
  seasonal <- rep(seas[[1L]], times = n_along * n_by)
  for (i_by in seq_len(n_by)) {
    for (i_along in seq_len(n_along)) {
      i_seas <- ((i_along - 1L) %% n_seas) + (i_by - 1L) * n_seas + 1L
      i_seasonal <- matrix_along_by_effect[i_along, i_by] + 1L
      seasonal[i_seasonal] <- seas[i_seas]
    }
  }
  trend <- effect - seasonal
  level <- components$level[is_effect]
  seasonal <- tibble::tibble(term = nm,
                             component = "seasonal",
                             level = level,
                             .fitted = seasonal)
  trend <- tibble::tibble(term = nm,
                          component = "trend",
                          level = level,
                          .fitted = trend)
  ans <- components[!is_seasonal, , drop = FALSE]
  ans <- vctrs::vec_rbind(ans, seasonal, trend)
  ans
}


## HAS_TESTS
#' Derive 'Components' Output for Terms with Fixed Seasonal Effect
#' - Forecasts
#'
#' Derive 'seasonal' from 'effect' and 'trend'
#'
#' @param prior Object of class 'bage_prior'.
#' @param dimnames_term Dimnames for array representation of term
#' @param var_time Name of time variable
#' @param var_age Name of age variable
#' @param components A data frame.
#'
#' @returns A modifed version of 'components'
#'
#' @noRd
infer_trend_cyc_seas_err_seasfix_forecast <- function(prior,
                                                      dimnames_term,
                                                      var_time,
                                                      var_age,
                                                      components) {
  nm <- dimnames_to_nm(dimnames_term)
  is_seasonal <- with(components, (term == nm) & (component == "seasonal"))
  is_trend <- with(components, (term == nm) & (component == "trend"))
  is_effect <- with(components, (term == nm) & (component == "effect"))
  trend <- components$.fitted[is_trend]
  effect <- components$.fitted[is_effect]
  seasonal <- effect - trend
  components$.fitted[is_seasonal] <- seasonal
  components
}



## HAS_TESTS
#' Derive 'Components' Output for Terms with Varying Seasonal Effect - Estimates
#'
#' Derive 'trend' from 'effect' and 'seasonal'
#'
#' @param prior Object of class 'bage_prior'.
#' @param dimnames_term Dimnames for array representation of term
#' @param var_time Name of time variable
#' @param var_age Name of age variable
#' @param components A data frame.
#'
#' @returns A modifed version of 'components'
#'
#' @noRd
infer_trend_cyc_seas_err_seasvary <- function(prior,
                                              dimnames_term,
                                              var_time,
                                              var_age,
                                              components) {
  nm <- dimnames_to_nm(dimnames_term)
  is_seasonal <- with(components, (term == nm) & (component == "hyperrand"))
  is_effect <- with(components, (term == nm) & (component == "effect"))
  seasonal <- components$.fitted[is_seasonal]
  effect <- components$.fitted[is_effect]
  trend <- effect - seasonal
  components$component[is_seasonal] <- "seasonal"
  level <- components$level[is_effect]
  trend <- tibble::tibble(term = nm,
                          component = "trend",
                          level = level,
                          .fitted = trend)
  ## combine
  vctrs::vec_rbind(components, trend)
}


## HAS_TESTS
#' Derive 'Components' Output for Terms with Varying Seasonal Effect - Forecasts
#'
#' Derive 'seasonal' from 'effect' and 'trend'
#'
#' @param prior Object of class 'bage_prior'.
#' @param dimnames_term Dimnames for array representation of term
#' @param var_time Name of time variable
#' @param var_age Name of age variable
#' @param components A data frame.
#'
#' @returns A modifed version of 'components'
#'
#' @noRd
infer_trend_cyc_seas_err_seasvary_forecast <- function(prior,
                                                       dimnames_term,
                                                       var_time,
                                                       var_age,
                                                       components) {
  nm <- dimnames_to_nm(dimnames_term)
  is_trend <- with(components, (term == nm) & (component == "trend"))
  is_seasonal <- with(components, (term == nm) & (component == "seasonal"))
  is_effect <- with(components, (term == nm) & (component == "effect"))
  trend <- components$.fitted[is_trend]
  effect <- components$.fitted[is_effect]
  seasonal <- effect - trend
  components$.fitted[is_seasonal] <- seasonal
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
  

## HAS_TESTS
#' Create Functions Needed to Transform Hyper-Parameters
#' from AR-Based Prior
#'
#' @param prior Object of class 'bage_prior'
#'
#' @returns A named list
#'
#' @noRd
transform_hyper_ar <- function(prior) {
  specific <- prior$specific
  n_coef <- specific$n_coef
  min <- specific$min
  max <- specific$max
  shifted_inv_logit <- function(x) {
    ans_raw <- exp(x) / (1 + exp(x))
    ans <- (max - min) * ans_raw + min
    ans
  }
  rep(list(coef = shifted_inv_logit, sd = exp),
      times = c(n_coef, 1L))
}

  
