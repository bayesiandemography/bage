
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
        matrix_along_by <- make_matrix_along_by_svd(prior = prior,
                                                    dimnames_term = dimnames_term,
                                                    var_time = var_time,
                                                    var_age = var_age,
                                                    var_sexgender = var_sexgender,
                                                    drop_first_along = FALSE)
      }
      else {
        levels <- make_levels_spline_term(prior = prior,
                                          dimnames_term = dimnames_term,
                                          var_time = var_time,
                                          var_age = var_age)
        key_term <- "spline"
        matrix_along_by <- make_matrix_along_by_spline(prior = prior,
                                                       dimnames_term = dimnames_term,
                                                       var_time = var_time,
                                                       var_age = var_age,
                                                       drop_first_along = FALSE)
      }
      key_term <- paste(nm, key_term, levels)
      indices_comp <- match(key_term, key_components)
      val_term <- components$.fitted[indices_comp]
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
#' Combined Stored Draws and Point EStimates from Two Models
#'
#' Term used from first model if 'use_term' is TRUE; otherwise
#' from second model. Dispersion ignored.
#'
#' @param mod Model receiving the draws
#' @param mod_inner Model for which use_term is TRUE
#' @param mod_outer Model for which use_term is FALSE
#' @param use_term Logical vector
#'
#' @returns Modified version of 'mod'
#' 
#' @noRd
combine_stored_draws_point_inner_outer <- function(mod, mod_inner, mod_outer, use_term) { 
  priors <- mod$priors
  n_term <- length(priors)
  nms_term <- names(priors)
  terms_effectfree_inner <- make_terms_effectfree(mod_inner)
  terms_effectfree_outer <- make_terms_effectfree(mod_outer)
  terms_hyper_inner <- make_terms_hyper(mod_inner)
  terms_hyper_outer <- make_terms_hyper(mod_outer)
  terms_hyperrand_inner <- make_terms_hyperrand(mod_inner)
  terms_hyperrand_outer <- make_terms_hyperrand(mod_outer)
  draws_effectfree_inner <- mod_inner$draws_effectfree
  draws_effectfree_outer <- mod_outer$draws_effectfree
  draws_hyper_inner <- mod_inner$draws_hyper
  draws_hyper_outer <- mod_outer$draws_hyper
  draws_hyperrand_inner <- mod_inner$draws_hyperrand
  draws_hyperrand_outer <- mod_outer$draws_hyperrand
  point_effectfree_inner <- mod_inner$point_effectfree
  point_effectfree_outer <- mod_outer$point_effectfree
  point_hyper_inner <- mod_inner$point_hyper
  point_hyper_outer <- mod_outer$point_hyper
  point_hyperrand_inner <- mod_inner$point_hyperrand
  point_hyperrand_outer <- mod_outer$point_hyperrand
  draws_effectfree <- vector(mode = "list", length = n_term)
  draws_hyper <- vector(mode = "list", length = n_term)
  draws_hyperrand <- vector(mode = "list", length = n_term)
  point_effectfree <- double()
  point_hyper <- double()
  point_hyperrand <- double()
  for (i_term in seq_len(n_term)) {
    nm_term <- nms_term[[i_term]]
    use_inner <- use_term[[i_term]]
    if (use_inner) {
      is_term_effectfree <- terms_effectfree_inner == nm_term
      draws_effectfree[[i_term]] <- draws_effectfree_inner[is_term_effectfree, , drop = FALSE]
      point_effectfree <- c(point_effectfree, point_effectfree_inner[is_term_effectfree])
      is_term_hyper <- terms_hyper_inner == nm_term
      draws_hyper[[i_term]] <- draws_hyper_inner[is_term_hyper, , drop = FALSE]
      point_hyper <- c(point_hyper, point_hyper_inner[is_term_hyper])
      is_term_hyperrand <- terms_hyperrand_inner == nm_term
      draws_hyperrand[[i_term]] <- draws_hyperrand_inner[is_term_hyperrand, , drop = FALSE]
      point_hyperrand <- c(point_hyperrand, point_hyperrand_inner[is_term_hyperrand])
    }
    else {
      is_term_effectfree <- terms_effectfree_outer == nm_term
      draws_effectfree[[i_term]] <- draws_effectfree_outer[is_term_effectfree, , drop = FALSE]
      point_effectfree <- c(point_effectfree, point_effectfree_outer[is_term_effectfree])
      is_term_hyper <- terms_hyper_outer == nm_term
      draws_hyper[[i_term]] <- draws_hyper_outer[is_term_hyper, , drop = FALSE]
      point_hyper <- c(point_hyper, point_hyper_outer[is_term_hyper])
      is_term_hyperrand <- terms_hyperrand_outer == nm_term
      draws_hyperrand[[i_term]] <- draws_hyperrand_outer[is_term_hyperrand, , drop = FALSE]
      point_hyperrand <- c(point_hyperrand, point_hyperrand_outer[is_term_hyperrand])
    }
  }
  draws_effectfree <- do.call(rbind, draws_effectfree)
  draws_hyper <- do.call(rbind, draws_hyper)
  draws_hyperrand <- do.call(rbind, draws_hyperrand)
  mod$draws_effectfree <- draws_effectfree
  mod$draws_hyper <- draws_hyper
  mod$draws_hyperrand <- draws_hyperrand
  mod$point_effectfree <- point_effectfree
  mod$point_hyper <- point_hyper
  mod$point_hyperrand <- point_hyperrand
  mod
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

  
#' Default Method for Fitting a Model
#'
#' @param object A `bage_mod` object.
#' typically created with [mod_pois()],
#' [mod_binom()], or [mod_norm()].
#'
#' @returns A `bage_mod` object
#'
#' @noRd
fit_default <- function(mod, aggregate = TRUE) {
  mod <- unfit(mod)
  ## data
  if (aggregate) {
    vals_ag <- make_vals_ag(mod)
    outcome <- vals_ag$outcome
    offset <- vals_ag$offset
    matrices_effect_outcome <- vals_ag$matrices_effect_outcome
  }
  else {
    vals_in_lik <- make_vals_in_lik(mod)
    outcome <- vals_in_lik$outcome
    offset <- vals_in_lik$offset
    matrices_effect_outcome <- vals_in_lik$matrices_effect_outcome
  }
  dimnames_terms <- mod$dimnames_terms
  terms_effect <- make_terms_effects(dimnames_terms)
  i_lik <- make_i_lik_mod(mod)
  terms_effectfree <- make_terms_effectfree(mod)
  uses_matrix_effectfree_effect <- make_uses_matrix_effectfree_effect(mod)
  matrices_effectfree_effect <- make_matrices_effectfree_effect(mod)
  uses_offset_effectfree_effect <- make_uses_offset_effectfree_effect(mod)
  offsets_effectfree_effect <- make_offsets_effectfree_effect(mod)
  i_prior <- make_i_prior(mod)
  uses_hyper <- make_uses_hyper(mod)
  terms_hyper <- make_terms_hyper(mod)
  uses_hyperrand <- make_uses_hyperrand(mod)
  terms_hyperrand <- make_terms_hyperrand(mod)
  const <- make_const(mod)
  terms_const <- make_terms_const(mod)
  matrices_along_by_effectfree <- make_matrices_along_by_effectfree(mod)
  mean_disp <- mod$mean_disp
  has_disp <- mean_disp > 0
  data <- list(i_lik = i_lik,
               outcome = outcome,
               offset = offset,
               terms_effect = terms_effect,
               terms_effectfree = terms_effectfree,
               uses_matrix_effectfree_effect = uses_matrix_effectfree_effect,
               matrices_effectfree_effect = matrices_effectfree_effect,
               uses_offset_effectfree_effect = uses_offset_effectfree_effect,
               offsets_effectfree_effect = offsets_effectfree_effect,
               matrices_effect_outcome = matrices_effect_outcome,
               i_prior = i_prior,
               uses_hyper = uses_hyper,
               terms_hyper = terms_hyper,
               uses_hyperrand = uses_hyperrand,
               terms_hyperrand = terms_hyperrand,
               consts = const, ## 'const' is reserved word in C
               terms_consts = terms_const,
               matrices_along_by_effectfree = matrices_along_by_effectfree,
               mean_disp = mean_disp)
  ## parameters
  effectfree <- make_effectfree(mod)
  hyper <- make_hyper(mod)
  hyperrand <- make_hyperrand(mod)
  log_disp <- 0
  parameters <- list(effectfree = effectfree,   
                     hyper = hyper,
                     hyperrand = hyperrand,
                     log_disp = log_disp)
  ## MakeADFun
  map <- make_map(mod)
  random <- make_random(mod)
  has_random_effects <- !is.null(random)
  f <- TMB::MakeADFun(data = data,
                      parameters = parameters,
                      map = map,
                      DLL = "bage",
                      random = random,
                      silent = TRUE)
  ## optimise
  stats::nlminb(start = f$par,
                objective = f$fn,
                gradient = f$gr,
                silent = TRUE)
  ## extract results
  if (has_random_effects)
    sdreport <- TMB::sdreport(f,
                              bias.correct = TRUE,
                              getJointPrecision = TRUE)
  else
    sdreport <- TMB::sdreport(f) 
  est <- as.list(sdreport, what = "Est")
  if (has_random_effects)
    prec <- sdreport$jointPrecision
  else
    prec <- solve(sdreport$cov.fixed) ## should be very low dimension
  mod <- make_stored_draws(mod = mod,
                           est = est,
                           prec = prec,
                           map = map)
  mod <- make_stored_point(mod = mod,
                           est = est)
  mod
}



#' Two-Step Method for Fitting a Model
#'
#' @param object A `bage_mod` object.
#' @param vars_inner Variables used
#' in inner model.
#'
#' @returns A `bage_mod` object
#'
#' @noRd
fit_inner_outer <- function(mod, vars_inner) {
  if (is.null(vars_inner))
    vars_inner <- make_vars_inner(mod)
  else
    check_vars_inner(vars_inner)
  use_term <- make_use_term(mod = mod,
                            vars_inner = vars_inner)
  mod_inner <- make_mod_inner(mod = mod,
                              use_term = use_term)
  mod_inner <- fit_default(mod = mod_inner,
                           aggregate = TRUE)
  mod_outer <- make_mod_outer(mod = mod,
                              mod_inner = mod_inner,
                              use_term = use_term)
  mod_outer <- fit_default(mod = mod_outer,
                           aggregate = TRUE)
  mod <- combine_stored_draws_point_inner_outer(mod = mod,
                                                mod_inner = mod_inner,
                                                mod_outer = mod_outer,
                                                use_term = use_term)
  if (has_disp(mod)) {
    mod_disp <- make_mod_disp(mod)
    mod_disp <- fit_default(mod = mod_disp,
                            aggregate = FALSE)
    mod <- transfer_draws_disp(mod = mod,
                               mod_disp = mod_disp)
  }
  mod
}


## HAS_TESTS
#' Helper Function for 'generate' Methods for 'bage_prior'
#'
#' @param n Number of elements in term
#' @param n_draw Number of draws
#'
#' @returns A named list
#'
#' @noRd
generate_prior_helper <- function(n, n_draw) {
  poputils::check_n(n = n,
                    nm_n = "n",
                    min = 1L,
                    max = NULL,
                    divisible_by = NULL)
  poputils::check_n(n = n_draw,
                    nm_n = "n_draw",
                    min = 1L,
                    max = NULL,
                    divisible_by = NULL)
  levels_effect <- seq_len(n)
  matrix_along_by <- matrix(seq_len(n) - 1L,
                            ncol = 1L,
                            dimnames = list(levels_effect, NULL))
  x <- rep(seq_len(n), times = n_draw)
  draw <- rep(seq_len(n_draw), each = n)
  ans <- tibble::tibble(x = x, draw = draw)
  list(matrix_along_by = matrix_along_by,
       levels_effect = levels_effect,
       ans = ans)
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
  data <- mod$data
  dimnames_terms <- mod$dimnames_terms
  nms_terms <- names(dimnames_terms)
  matrices_effect_outcome <- make_matrices_effect_outcome(data = data,
                                                          dimnames_terms = dimnames_terms)
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
  dimnames_terms <- mod$dimnames_terms
  terms_effects <- make_terms_effects(dimnames_terms)
  terms_spline <- make_term_spline(mod)
  terms_svd <- make_term_svd(mod)
  has_disp <- has_disp(mod)
  vals <- c("effect", "hyper", "hyperrand", "spline", "svd", "disp")
  n_effect <- length(terms_effects)
  n_hyper <- length(make_hyper(mod))
  n_hyperrand <- length(make_hyperrand(mod))
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
#' @param draws_post Posterior draws for all parameters
#' estimated in TMB. Output from 'make_draws_post'.
#'
#' @returns A numeric vector.
#' 
#' @noRd
make_draws_disp <- function(draws_post) {
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
#' @param est Named list. Output from TMB.
#' @param draws_post Posterior draws for all parameters
#' estimated in TMB. Output from 'make_draws_post'.
#'
#' @returns A matrix
#' 
#' @noRd
make_draws_effectfree <- function(est, draws_post) {
  n_effectfree <- length(est$effectfree)
  i_effectfree <- seq_len(n_effectfree)
  draws_post[i_effectfree, , drop = FALSE]
}


## HAS_TESTS
#' Make Draws from Hyper-Parameters
#'
#' Does not include hyperrand or disp.
#' Includes transforming back to natural units.
#'
#' @param est Named list. Output from TMB.
#' @param transforms_hyper List of transforms to be
#' applied to hyper-parameters
#' @param draws_post Posterior draws for all parameters
#' estimated in TMB. Output from 'make_draws_post'.
#'
#' @returns A matrix
#' 
#' @noRd
make_draws_hyper <- function(est, transforms_hyper, draws_post) {
  n_effectfree <- length(est$effectfree)
  n_hyper <- length(est$hyper)
  i_hyper <- seq.int(from = n_effectfree + 1L, length.out = n_hyper)
  ans <- draws_post[i_hyper, , drop = FALSE]
  for (i in seq_along(transforms_hyper)) {
    transform <- transforms_hyper[[i]]
    if (!is.null(transform))
      ans[i, ] <- transform(ans[i, ])
  }
  ans
}


## HAS_TESTS
#' Make Draws from Hyper-Parameters that can be
#' Treated as Random Effects
#'
#' @param est Named list. Output from TMB.
#' @param draws_post Posterior draws for all parameters
#' estimated in TMB. Output from 'make_draws_post'.
#'
#' @returns A matrix
#' 
#' @noRd
make_draws_hyperrand <- function(est, draws_post) {
  n_effectfree <- length(est$effectfree)
  n_hyper <- length(est$hyper)
  n_hyperrand <- length(est$hyperrand)
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
#' 'prec' is successful, then use that.
#' Otherwise, use the eigen decomposition.
#' Insert values for parameters that were
#' fixed via the 'map' function
#'
#' @param est Named list with estimates returned by TMB
#' @param prec Precision matrix returned by TMB
#' @param map 'map' argument to TMB
#' @param n_draw Number of posterior draws
#'
#' @returns A matrix
#'
#' @noRd
make_draws_post <- function(est, prec, map, n_draw) {
  is_fixed <- make_is_fixed(est = est, map = map)
  est_unlist <- unlist(est)
  mean <- est_unlist[!is_fixed]
  R_prec <- tryCatch(chol(prec), error = function(e) NULL)
  if (is.matrix(R_prec))
    draws_nonfixed <- rmvnorm_chol(n = n_draw,
                                   mean = mean,
                                   R_prec = R_prec)
  else {
    scaled_eigen <- make_scaled_eigen(prec)
    draws_nonfixed <- rmvnorm_eigen(n = n_draw,
                                    mean = mean,
                                    scaled_eigen = scaled_eigen)
  }
  ans <- matrix(nrow = length(is_fixed), ncol = n_draw)
  ans[!is_fixed, ] <- draws_nonfixed
  ans[is_fixed, ] <- est_unlist[is_fixed]
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
  dimnames_terms <- mod$dimnames_terms
  effect <- make_levels_effects(dimnames_terms)
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
  ans <- .mapply(levels_hyperrand,
                 dots = list(prior = priors,
                             dimnames_term = dimnames_terms),
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


## HAS_TESTS
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
  dimnames_term[[i_along]] <- levels_along
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
#' @param slope Slope(s) of line(s). An rvec.
#' @param matrix_along_by Matrix mapping
#' along and by dimensions to position in estimates
#'
#' @returns An rvec
#'
#' @noRd
make_lin_trend <- function(slope,
                           matrix_along_by) {
  n_along <- nrow(matrix_along_by)
  n_by <- ncol(matrix_along_by)
  slope <- rep(slope, each = n_along)
  intercept <- -0.5 * (n_along + 1) * slope
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
#' @param point Whether to return point estimates
#' or draws from the posterior.
#'
#' @returns An rvec if 'point' is FALSE, otherwise a vector of doubles
#'
#' @noRd
make_linpred_raw <- function(mod, point) {
  matrix_effect_outcome <- make_combined_matrix_effect_outcome(mod)
  if (point)
    effectfree <- mod$point_effectfree
  else
    effectfree <- mod$draws_effectfree
  effect <- make_effects(mod = mod, effectfree = effectfree)
  ans <- matrix_effect_outcome %*% effect
  if (point)
    ans <- as.double(ans)
  else {
    ans <- Matrix::as.matrix(ans)
    ans <- rvec::rvec(ans)
  }
  ans
}


## HAS_TESTS
#' Derive Point Estimates for Effects
#'
#' @param mod A fitted object of class 'bage_mod'
#'
#' @returns A named list of numeric vectors
#'
#' @noRd
make_point_est_effects <- function(mod) {
  if (!is_fitted(mod))
    cli::cli_abort("Internal error: Model not fitted.")
  point_effectfree <- mod$point_effectfree
  dimnames_terms <- mod$dimnames_terms
  terms_effects <- make_terms_effects(dimnames_terms)
  point_effects <- make_effects(mod = mod, effectfree = point_effectfree)
  point_effects <- as.double(point_effects)
  ans <- split(x = point_effects, f = terms_effects)
  ans <- ans[unique(terms_effects)] ## 'split' orders result
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
  dimnames_terms <- mod$dimnames_terms
  var_time <- mod$var_time
  var_age <- mod$var_age
  var_sexgender <- mod$var_sexgender
  n_term <- length(priors)
  ans <- vector(mode = "list", length = n_term)
  lengths_effectfree <- make_lengths_effectfree(mod)
  to <- 0L
  for (i_term in seq_len(n_term)) {
    length_effectfree <- lengths_effectfree[[i_term]]
    to <- to + length_effectfree
    prior <- priors[[i_term]]
    dimnames_term <- dimnames_terms[[i_term]]
    if (is_spline(prior)) {
      s <- seq.int(to = to, length.out = length_effectfree)
      vals <- effectfree[s, , drop = FALSE]
      m_along_by <- make_matrix_along_by_effectfree(prior = prior,
                                                    dimnames_term = dimnames_term,
                                                    var_time = var_time,
                                                    var_age = var_age,
                                                    var_sexgender = var_sexgender)
      m_append_zero <- make_matrix_append_zero(m_along_by)
      vals <- m_append_zero %*% vals
      vals <- as.matrix(vals)
      ans[[i_term]] <- vals
    }
  }
  ans <- do.call(rbind, ans)
  ans
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
  dimnames_terms <- mod$dimnames_terms
  var_time <- mod$var_time
  var_age <- mod$var_age
  var_sexgender <- mod$var_sexgender
  n_term <- length(priors)
  ans <- vector(mode = "list", length = n_term)
  lengths_effectfree <- make_lengths_effectfree(mod)
  to <- 0L
  for (i_term in seq_len(n_term)) {
    length_effectfree <- lengths_effectfree[[i_term]]
    to <- to + length_effectfree
    prior <- priors[[i_term]]
    if (is_svd(prior)) {
      s <- seq.int(to = to, length.out = length_effectfree)
      vals <- effectfree[s, , drop = FALSE]
      if (is_drop_first_along(prior)) {
        dimnames_term <- dimnames_terms[[i_term]]
        m_along_by <- make_matrix_along_by_effectfree(prior = prior,
                                                      dimnames_term = dimnames_term,
                                                      var_time = var_time,
                                                      var_age = var_age,
                                                      var_sexgender = var_sexgender)
        m_append_zero <- make_matrix_append_zero(m_along_by)
        vals <- m_append_zero %*% vals
      }
      vals <- as.matrix(vals)
      ans[[i_term]] <- vals
    }
  }
  ans <- do.call(rbind, ans)
  ans
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
  is_spline <- vapply(priors, is_spline, FALSE)
  nms_terms_spline <- nms_terms[is_spline]
  levels_spline <- make_levels_spline(mod, unlist = FALSE)
  lengths_spline <- lengths(levels_spline[is_spline])
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
  levels <- make_levels_svd(mod, unlist = FALSE)
  lengths_levels <- lengths(levels)
  nms_terms <- names(priors)
  is_svd <- vapply(priors, is_svd, FALSE)
  lengths_svd <- lengths_levels[is_svd]
  nms_svd <- nms_terms[is_svd]
  ans <- rep(nms_svd, times = lengths_svd)
  ans <- factor(ans, levels = nms_svd)
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
make_stored_draws <- function(mod, est, prec, map) {
  n_draw <- mod$n_draw
  transforms_hyper <- make_transforms_hyper(mod)
  seed_stored_draws <- mod$seed_stored_draws
  seed_restore <- make_seed() ## create randomly-generated seed
  set.seed(seed_stored_draws) ## set pre-determined seed
  draws_post <- make_draws_post(est = est,
                                prec = prec,
                                map = map,
                                n_draw = n_draw)
  set.seed(seed_restore) ## set randomly-generated seed, to restore randomness
  mod$draws_effectfree <- make_draws_effectfree(est = est,
                                                draws_post = draws_post)
  mod$draws_hyper <- make_draws_hyper(est = est,
                                      transforms_hyper = transforms_hyper,
                                      draws_post = draws_post)
  mod$draws_hyperrand <- make_draws_hyperrand(est = est,
                                              draws_post = draws_post)
  if (has_disp(mod))
    mod$draws_disp <- make_draws_disp(draws_post)
  mod
}


## NO_TESTS
#' Make Point Estimates Stored as Part of Model Object
#'
#' Draws created are
#' - 'point_effectfree',
#' - 'point_hyper',
#' - 'point_hyperrand', and, optionally,
#' - 'point_disp'.
#'
#' @param mod A fitted 'bage_mod' object
#'
#' @returns Modified version of 'mod'
#'
#' @noRd
make_stored_point <- function(mod, est) {
  transforms_hyper <- make_transforms_hyper(mod)
  mod$point_effectfree <- est$effectfree
  point_hyper <- est$hyper
  for (i in seq_along(point_hyper))
    point_hyper[[i]] <- transforms_hyper[[i]](point_hyper[[i]])
  mod$point_hyper <- point_hyper
  mod$point_hyperrand <- est$hyperrand
  if (has_disp(mod))
    mod$point_disp <- exp(est$log_disp)
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
  dimnames_terms <- mod$dimnames_terms
  effect <- make_terms_effects(dimnames_terms)
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
  n_draw <- rvec::n_draw(seas)
  seasonal <- rvec::new_rvec(length = n_along * n_by, n_draw = n_draw)
  for (i_by in seq_len(n_by)) {
    seas_sum <- 0
    for (i_along in seq_len(n_along)) {
      i_seasonal <- matrix_along_by_effect[i_along, i_by] + 1L
      if (i_along == 1L) {
        effect_first <- effect[[i_seasonal]]
        seasonal[[i_seasonal]] <- effect_first
      }
      else if (i_along < n_seas) {
        i_seas <- i_along - 1L + (i_by - 1L) * (n_seas - 2L)
        seasonal[[i_seasonal]] <- seas[[i_seas]] + effect_first
        seas_sum <- seas_sum + seas[[i_seas]]
      }
      else if (i_along == n_seas) {
        seasonal[[i_seasonal]] <- -seas_sum - (n_seas - 1) * effect_first
      }
      else {
        i_prev <- matrix_along_by_effect[i_along - n_seas, i_by] + 1L
        seasonal[[i_seasonal]] <- seasonal[[i_prev]]
      }
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
  ans <- vctrs::vec_rbind(ans, trend, seasonal)
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
  n_along_effect <- nrow(matrix_along_by_effect)
  n_by <- ncol(matrix_along_by_effect)
  n_draw <- rvec::n_draw(seas)
  seasonal <- rvec::new_rvec(length = n_along_effect * n_by, n_draw = n_draw)
  n_along_seas <- length(seas) %/% n_by
  for (i_by in seq_len(n_by)) {
    i_effect_first <- matrix_along_by_effect[1L, i_by] + 1L
    effect_first <- effect[[i_effect_first]]
    i_along_seas <- 1L
    seas_sum <- 0
    for (i_along_effect in seq_len(n_along_effect)) {
      is_first_element <- i_along_effect == 1L
      index_seas <- (i_along_effect - 1L) %% n_seas
      is_last_seas <- index_seas == n_seas - 1L
      i_seasonal <- matrix_along_by_effect[i_along_effect, i_by] + 1L
      if (is_first_element)
        seasonal[[i_seasonal]] <- effect_first
      if (!is_first_element && !is_last_seas) {
        i_seas <- i_along_seas + (i_by - 1L) * n_along_seas
        i_along_seas <- i_along_seas + 1L
        seasonal[[i_seasonal]] <- seas[[i_seas]] + effect_first
        seas_sum <- seas_sum + seas[[i_seas]]
      }
      if (is_last_seas) {
        seasonal[[i_seasonal]] <- -seas_sum - (n_seas - 1) * effect_first
        seas_sum <- 0
      }
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
  ans <- vctrs::vec_rbind(ans, trend, seasonal)
  ans
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
#' Transfer Draws for Dispersion between Models
#'
#'
#' @param mod Model receiving the draws
#' @param mod_disp Model giving the draws
#'
#' @returns Modified version of 'mod'
#' 
#' @noRd
transfer_draws_disp <- function(mod, mod_disp) { 
  mod$draws_disp <- mod_disp$draws_disp
  mod
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

  
