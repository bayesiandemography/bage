
## Note that methods require an '@export' tag,
## even when the generic function is not exported
## https://github.com/r-lib/devtools/issues/2293


## 'comp_hyperrand' -----------------------------------------------------------

#' Type of Hyper-Parameter
#'
#' Names for type of hyper-parameter represented by
#' hyperrand. Choices include "trend", "season", "error".
#' Used for creating output for 'components' function.
#' 
#' @param prior An object of class 'bage_prior'.
#' @param dimnames_term Dimnames for array representation of term
#' @param var_time Name of time variable
#' @param var_age Name of age variable
#'
#' @returns A character vector.
#'
#' @noRd
comp_hyperrand <- function(prior, dimnames_term, var_age, var_time) {
  UseMethod("comp_hyperrand")
}

## HAS_TESTS
#' @export
comp_hyperrand.bage_prior <- function(prior, dimnames_term, var_age, var_time) {
  character()
}

## HAS_TESTS
#' @export
comp_hyperrand.bage_prior_lin <- function(prior, dimnames_term, var_age, var_time) {
  i_along <- make_i_along(prior = prior,
                          dimnames_term = dimnames_term,
                          var_time = var_time,
                          var_age = var_age)
  n_by <- prod(lengths(dimnames_term[-i_along]))
  n_effect <- prod(lengths(dimnames_term))
  rep(c("hyper", "trend", "error"), times = c(n_by, n_effect, n_effect))
}

## HAS_TESTS
#' @export
comp_hyperrand.bage_prior_linar <- function(prior, dimnames_term, var_age, var_time) {
  i_along <- make_i_along(prior = prior,
                          dimnames_term = dimnames_term,
                          var_time = var_time,
                          var_age = var_age)
  n_by <- prod(lengths(dimnames_term[-i_along]))
  n_effect <- prod(lengths(dimnames_term))
  rep(c("hyper", "trend", "error"), times = c(n_by, n_effect, n_effect))
}

## HAS_TESTS
#' @export
comp_hyperrand.bage_prior_rwrandomseasfix <- function(prior, dimnames_term, var_age, var_time) {
  n <- prod(lengths(dimnames_term))
  rep(c("trend", "season"), each = n)
}

## HAS_TESTS
#' @export
comp_hyperrand.bage_prior_rwrandomseasvary <- function(prior, dimnames_term, var_age, var_time) {
  n <- prod(lengths(dimnames_term))
  rep(c("trend", "season"), each = n)
}

## HAS_TESTS
#' @export
comp_hyperrand.bage_prior_rwzeroseasfix <- function(prior, dimnames_term, var_age, var_time) {
  n <- prod(lengths(dimnames_term))
  rep(c("trend", "season"), each = n)
}

## HAS_TESTS
#' @export
comp_hyperrand.bage_prior_rwzeroseasvary <- function(prior, dimnames_term, var_age, var_time) {
  n <- prod(lengths(dimnames_term))
  rep(c("trend", "season"), each = n)
}

## HAS_TESTS
#' @export
comp_hyperrand.bage_prior_rw2randomseasfix <- function(prior, dimnames_term, var_age, var_time) {
  n <- prod(lengths(dimnames_term))
  rep(c("trend", "season"), each = n)
}

## HAS_TESTS
#' @export
comp_hyperrand.bage_prior_rw2randomseasvary <- function(prior, dimnames_term, var_age, var_time) {
  n <- prod(lengths(dimnames_term))
  rep(c("trend", "season"), each = n)
}

## HAS_TESTS
#' @export
comp_hyperrand.bage_prior_rw2zeroseasfix <- function(prior, dimnames_term, var_age, var_time) {
  n <- prod(lengths(dimnames_term))
  rep(c("trend", "season"), each = n)
}

## HAS_TESTS
#' @export
comp_hyperrand.bage_prior_rw2zeroseasvary <- function(prior, dimnames_term, var_age, var_time) {
  n <- prod(lengths(dimnames_term))
  rep(c("trend", "season"), each = n)
}


## 'const' -------------------------------------------------------------------

#' Extract Constants from Prior Spec
#'
#' @param prior Object of class 'bage_prior'
#'
#' @returns A named numeric vector.
#'
#' @noRd
const <- function(prior) {
  UseMethod("const")
}

## HAS_TESTS
#' @export
const.bage_prior <- function(prior) prior$const


## 'draw_vals_effect' ------------------------------------------------------------

#' Draw Values for Main Effect or Interactions
#'
#' Effect is centered, unless it has a Known or SVD prior.
#' 
#' @param prior Object of class 'bage_prior'
#' @param vals_hyper Named list with values of ordinary hyper-parameters
#' @param vals_hyperrand Named list with values of hyper-parameters
#' that can be treated as random effects
#' @param vals_spline Named list of values for free parameters
#' for spline-based priors
#' @param vals_svd Named list of values for free parameters
#' for SVD-based priors
#' @param dimnames_term Dimnames for array representation of term
#' @param var_time Name of time variable
#' @param var_age Name of age variable
#' @param var_sexgender Name of sex/gender variable
#' @param n_sim Number of draws
#'
#' @returns A named list.
#'
#' @noRd
draw_vals_effect <- function(prior,
                             vals_hyper,
                             vals_hyperrand,
                             vals_spline,
                             vals_svd,
                             dimnames_term,
                             var_time,
                             var_age,
                             var_sexgender,
                             n_sim) {
  UseMethod("draw_vals_effect")
}


## HAS_TESTS
#' @export
draw_vals_effect.bage_prior_ar <- function(prior,
                                           vals_hyper,
                                           vals_hyperrand,
                                           vals_spline,
                                           vals_svd,
                                           dimnames_term,
                                           var_time,
                                           var_age,
                                           var_sexgender,
                                           n_sim) {
  con <- prior$specific$con
  coef <- vals_hyper$coef
  sd <- vals_hyper$sd
  i_along <- make_i_along(prior = prior,
                          dimnames_term = dimnames_term,
                          var_time = var_time,
                          var_age = var_age)
  dim <- lengths(dimnames_term)
  matrix_along_by_effect <- make_matrix_along_by_inner(i_along = i_along,
                                                       dim = dim)
  levels_effect <- dimnames_to_levels(dimnames_term)
  ans <- draw_vals_ar(coef = coef,
                      sd = sd,
                      matrix_along_by = matrix_along_by_effect,
                      levels_effect = levels_effect)
  if (con == "by") {
    m <- make_matrix_con_by(i_along = i_along,
                              dim = dim)
    ans <- m %*% ans
    ans <- Matrix::as.matrix(ans)
  }
  ans
}

## HAS_TESTS
#' @export
draw_vals_effect.bage_prior_known <- function(prior,
                                              vals_hyper,
                                              vals_hyperrand,
                                              vals_spline,
                                              vals_svd,
                                              dimnames_term,
                                              var_time,
                                              var_age,
                                              var_sexgender,
                                              n_sim) {
  values <- prior$specific$values
  levels_effect <- dimnames_to_levels(dimnames_term)
  n_effect <- length(levels_effect)
  matrix(values,
         nrow = n_effect,
         ncol = n_sim,
         dimnames = list(levels_effect, NULL))
}

## HAS_TESTS
#' @export
draw_vals_effect.bage_prior_lin <- function(prior,
                                            vals_hyper,
                                            vals_hyperrand,
                                            vals_spline,
                                            vals_svd,
                                            dimnames_term,
                                            var_time,
                                            var_age,
                                            var_sexgender,
                                            n_sim) {
  trend <- vals_hyperrand$trend
  error <- vals_hyperrand$error
  trend + error
}

## HAS_TESTS
#' @export
draw_vals_effect.bage_prior_linar <- function(prior,
                                              vals_hyper,
                                              vals_hyperrand,
                                              vals_spline,
                                              vals_svd,
                                              dimnames_term,
                                              var_time,
                                              var_age,
                                              var_sexgender,
                                              n_sim) {
  trend <- vals_hyperrand$trend
  error <- vals_hyperrand$error
  trend + error
}

## HAS_TESTS
#' @export
draw_vals_effect.bage_prior_linex <- function(prior,
                                              vals_hyper,
                                              vals_hyperrand,
                                              vals_spline,
                                              vals_svd,
                                              dimnames_term,
                                              var_time,
                                              var_age,
                                              var_sexgender,
                                              n_sim) {
  con <- prior$specific$con
  sd_slope <- prior$specific$sd_slope
  mean_slope <- prior$specific$mean_slope
  i_along <- make_i_along(prior = prior,
                          dimnames_term = dimnames_term,
                          var_time = var_time,
                          var_age = var_age)
  dim <- lengths(dimnames_term)
  matrix_along_by <- make_matrix_along_by(i_along = i_along,
                                          dim = dim,
                                          dimnames = dimnames_term)
  slope <- draw_vals_slope(mean_slope = mean_slope,
                           sd_slope = sd_slope,
                           matrix_along_by = matrix_along_by,
                           n_sim = n_sim)
  levels_effect <- dimnames_to_levels(dimnames_term)
  ans <- draw_vals_lintrend(slope = slope,
                            matrix_along_by = matrix_along_by,
                            labels = levels_effect)
  if (con == "by") {
    m <- make_matrix_con_by(i_along = i_along,
                              dim = dim)
    ans <- m %*% ans
    ans <- Matrix::as.matrix(ans)
  }
  ans  
}

## HAS_TESTS
#' @export
draw_vals_effect.bage_prior_norm <- function(prior,
                                             vals_hyper,
                                             vals_hyperrand,
                                             vals_spline,
                                             vals_svd,
                                             dimnames_term,
                                             var_time,
                                             var_age,
                                             var_sexgender,
                                             n_sim) {
  sd <- vals_hyper$sd
  levels_effect <- dimnames_to_levels(dimnames_term)
  draw_vals_norm(sd = sd, labels = levels_effect)
}

## HAS_TESTS
#' @export
draw_vals_effect.bage_prior_normfixed <- function(prior,
                                                  vals_hyper,
                                                  vals_hyperrand,
                                                  vals_spline,
                                                  vals_svd,
                                                  dimnames_term,
                                                  var_time,
                                                  var_age,
                                                  var_sexgender,
                                                  n_sim) {
  sd <- prior$specific$sd
  levels_effect <- dimnames_to_levels(dimnames_term)
  n_effect <- length(levels_effect)
  n <- n_effect * n_sim
  ans <- stats::rnorm(n = n, sd = sd)
  ans <- matrix(ans,
                nrow = n_effect,
                ncol = n_sim,
                dimnames = list(levels_effect, NULL))
  ans
}

## HAS_TESTS
#' @export
draw_vals_effect.bage_prior_rwrandom <- function(prior,
                                               vals_hyper,
                                               vals_hyperrand,
                                               vals_spline,
                                               vals_svd,
                                               dimnames_term,
                                               var_time,
                                               var_age,
                                               var_sexgender,
                                               n_sim) {
  sd_init <- prior$specific$sd
  con <- prior$specific$con
  sd <- vals_hyper$sd
  i_along <- make_i_along(prior = prior,
                          dimnames_term = dimnames_term,
                          var_time = var_time,
                          var_age = var_age)
  dim <- lengths(dimnames_term)
  matrix_along_by_effect <- make_matrix_along_by_inner(i_along = i_along,
                                                       dim = dim)
  levels_effect <- dimnames_to_levels(dimnames_term)
  ans <- draw_vals_rw(sd = sd,
                      sd_init = sd_init,
                      matrix_along_by = matrix_along_by_effect,
                      levels_effect = levels_effect)
  if (con == "by") {
    m <- make_matrix_con_by(i_along = i_along,
                              dim = dim)
    ans <- m %*% ans
    ans <- Matrix::as.matrix(ans)
  }
  ans  
}

## HAS_TESTS
#' @export
draw_vals_effect.bage_prior_rwrandomseasfix <- function(prior,
                                                  vals_hyper,
                                                  vals_hyperrand,
                                                  vals_spline,
                                                  vals_svd,
                                                  dimnames_term,
                                                  var_time,
                                                  var_age,
                                                  var_sexgender,
                                                  n_sim) {
  trend <- vals_hyperrand$trend
  season <- vals_hyperrand$season
  trend + season
}

## HAS_TESTS
#' @export
draw_vals_effect.bage_prior_rwrandomseasvary <- function(prior,
                                                   vals_hyper,
                                                   vals_hyperrand,
                                                   vals_spline,
                                                   vals_svd,
                                                   dimnames_term,
                                                   var_time,
                                                   var_age,
                                                   var_sexgender,
                                                   n_sim) {
  trend <- vals_hyperrand$trend
  season <- vals_hyperrand$season
  trend + season
}


## HAS_TESTS
#' @export
draw_vals_effect.bage_prior_rwzero <- function(prior,
                                               vals_hyper,
                                               vals_hyperrand,
                                               vals_spline,
                                               vals_svd,
                                               dimnames_term,
                                               var_time,
                                               var_age,
                                               var_sexgender,
                                               n_sim) {
  con <- prior$specific$con
  sd <- vals_hyper$sd
  i_along <- make_i_along(prior = prior,
                          dimnames_term = dimnames_term,
                          var_time = var_time,
                          var_age = var_age)
  dim <- lengths(dimnames_term)
  matrix_along_by_effect <- make_matrix_along_by_inner(i_along = i_along,
                                                       dim = dim)
  levels_effect <- dimnames_to_levels(dimnames_term)
  ans <- draw_vals_rw(sd = sd,
                      sd_init = 0,
                      matrix_along_by = matrix_along_by_effect,
                      levels_effect = levels_effect)
  if (con == "by") {
    m <- make_matrix_con_by(i_along = i_along,
                              dim = dim)
    ans <- m %*% ans
    ans <- Matrix::as.matrix(ans)
  }
  ans  
}

## HAS_TESTS
#' @export
draw_vals_effect.bage_prior_rwzeroseasfix <- function(prior,
                                                  vals_hyper,
                                                  vals_hyperrand,
                                                  vals_spline,
                                                  vals_svd,
                                                  dimnames_term,
                                                  var_time,
                                                  var_age,
                                                  var_sexgender,
                                                  n_sim) {
  trend <- vals_hyperrand$trend
  season <- vals_hyperrand$season
  trend + season
}

## HAS_TESTS
#' @export
draw_vals_effect.bage_prior_rwzeroseasvary <- function(prior,
                                                   vals_hyper,
                                                   vals_hyperrand,
                                                   vals_spline,
                                                   vals_svd,
                                                   dimnames_term,
                                                   var_time,
                                                   var_age,
                                                   var_sexgender,
                                                   n_sim) {
  trend <- vals_hyperrand$trend
  season <- vals_hyperrand$season
  trend + season
}

## HAS_TESTS
#' @export
draw_vals_effect.bage_prior_rw2infant <- function(prior,
                                                  vals_hyper,
                                                  vals_hyperrand,
                                                  vals_spline,
                                                  vals_svd,
                                                  dimnames_term,
                                                  var_time,
                                                  var_age,
                                                  var_sexgender,
                                                  n_sim) {
  con <- prior$specific$con
  sd_slope <- prior$specific$sd_slope
  sd <- vals_hyper$sd
  i_along <- make_i_along(prior = prior,
                          dimnames_term = dimnames_term,
                          var_time = var_time,
                          var_age = var_age)
  dim <- lengths(dimnames_term)
  matrix_along_by_effect <- make_matrix_along_by_inner(i_along = i_along,
                                                       dim = dim)
  levels_effect <- dimnames_to_levels(dimnames_term)
  ans <- draw_vals_rw2(sd = sd,
                       sd_init = 0,
                       sd_slope = sd_slope,
                       matrix_along_by = matrix_along_by_effect,
                       levels_effect = levels_effect)
  matrix_along_by <- make_matrix_along_by_inner(i_along = i_along,
                                                dim = dim)
  i_infant <- matrix_along_by[1L, ] + 1L
  n_by <- ncol(matrix_along_by)
  ans[i_infant] <- stats::rnorm(n = n_by)
  if (con == "by") {
    m <- make_matrix_con_by(i_along = i_along,
                              dim = dim)
    ans <- m %*% ans
    ans <- Matrix::as.matrix(ans)
  }
  ans  
}

## HAS_TESTS
#' @export
draw_vals_effect.bage_prior_rw2random <- function(prior,
                                                vals_hyper,
                                                vals_hyperrand,
                                                vals_spline,
                                                vals_svd,
                                                dimnames_term,
                                                var_time,
                                                var_age,
                                                var_sexgender,
                                                n_sim) {
  sd_init <- prior$specific$sd
  sd_slope <- prior$specific$sd_slope
  con <- prior$specific$con
  sd <- vals_hyper$sd
  i_along <- make_i_along(prior = prior,
                          dimnames_term = dimnames_term,
                          var_time = var_time,
                          var_age = var_age)
  dim <- lengths(dimnames_term)
  matrix_along_by_effect <- make_matrix_along_by_inner(i_along = i_along,
                                                       dim = dim)
  levels_effect <- dimnames_to_levels(dimnames_term)
  ans <- draw_vals_rw2(sd = sd,
                       sd_init = sd_init,
                       sd_slope = sd_slope,
                       matrix_along_by = matrix_along_by_effect,
                       levels_effect = levels_effect)
  if (con == "by") {
    m <- make_matrix_con_by(i_along = i_along,
                              dim = dim)
    ans <- m %*% ans
    ans <- Matrix::as.matrix(ans)
  }
  ans  
}

## HAS_TESTS
#' @export
draw_vals_effect.bage_prior_rw2randomseasfix <- function(prior,
                                                       vals_hyper,
                                                       vals_hyperrand,
                                                       vals_spline,
                                                       vals_svd,
                                                       dimnames_term,
                                                       var_time,
                                                       var_age,
                                                       var_sexgender,
                                                       n_sim) {
  trend <- vals_hyperrand$trend
  season <- vals_hyperrand$season
  trend + season
}

## HAS_TESTS
#' @export
draw_vals_effect.bage_prior_rw2randomseasvary <- function(prior,
                                                          vals_hyper,
                                                          vals_hyperrand,
                                                          vals_spline,
                                                          vals_svd,
                                                          dimnames_term,
                                                          var_time,
                                                          var_age,
                                                          var_sexgender,
                                                          n_sim) {
  trend <- vals_hyperrand$trend
  season <- vals_hyperrand$season
  trend + season
}

## HAS_TESTS
#' @export
draw_vals_effect.bage_prior_rw2zero <- function(prior,
                                                vals_hyper,
                                                vals_hyperrand,
                                                vals_spline,
                                                vals_svd,
                                                dimnames_term,
                                                var_time,
                                                var_age,
                                                var_sexgender,
                                                n_sim) {
  con <- prior$specific$con
  sd_slope <- prior$specific$sd_slope
  sd <- vals_hyper$sd
  i_along <- make_i_along(prior = prior,
                          dimnames_term = dimnames_term,
                          var_time = var_time,
                          var_age = var_age)
  dim <- lengths(dimnames_term)
  matrix_along_by_effect <- make_matrix_along_by_inner(i_along = i_along,
                                                       dim = dim)
  levels_effect <- dimnames_to_levels(dimnames_term)
  ans <- draw_vals_rw2(sd = sd,
                       sd_init = 0,
                       sd_slope = sd_slope,
                       matrix_along_by = matrix_along_by_effect,
                       levels_effect = levels_effect)
  if (con == "by") {
    m <- make_matrix_con_by(i_along = i_along,
                              dim = dim)
    ans <- m %*% ans
    ans <- Matrix::as.matrix(ans)
  }
  ans  
}

## HAS_TESTS
#' @export
draw_vals_effect.bage_prior_rw2zeroseasfix <- function(prior,
                                                       vals_hyper,
                                                       vals_hyperrand,
                                                       vals_spline,
                                                       vals_svd,
                                                       dimnames_term,
                                                       var_time,
                                                       var_age,
                                                       var_sexgender,
                                                       n_sim) {
  trend <- vals_hyperrand$trend
  season <- vals_hyperrand$season
  trend + season
}

## HAS_TESTS
#' @export
draw_vals_effect.bage_prior_rw2zeroseasvary <- function(prior,
                                                        vals_hyper,
                                                        vals_hyperrand,
                                                        vals_spline,
                                                        vals_svd,
                                                        dimnames_term,
                                                        var_time,
                                                        var_age,
                                                        var_sexgender,
                                                        n_sim) {
  trend <- vals_hyperrand$trend
  season <- vals_hyperrand$season
  trend + season
}

## HAS_TESTS
#' @export
draw_vals_effect.bage_prior_spline <- function(prior,
                                               vals_hyper,
                                               vals_hyperrand,
                                               vals_spline,
                                               vals_svd,
                                               dimnames_term,
                                               var_time,
                                               var_age,
                                               var_sexgender,
                                               n_sim) {
  m <- make_matrix_effectfree_effect(prior = prior,
                                     dimnames_term = dimnames_term,
                                     var_time = var_time,
                                     var_age = var_age,
                                     var_sexgender = var_sexgender)
  ans <- m %*% vals_spline
  ans <- Matrix::as.matrix(ans)
  rownames(ans) <- dimnames_to_levels(dimnames_term)
  ans
}

## HAS_TESTS
#' @export
draw_vals_effect.bage_prior_svd <- function(prior,
                                            vals_hyper,
                                            vals_hyperrand,
                                            vals_spline,
                                            vals_svd,
                                            dimnames_term,
                                            var_time,
                                            var_age,
                                            var_sexgender,
                                            n_sim) {
  matrix <- make_matrix_effectfree_effect(prior = prior,
                                          dimnames_term = dimnames_term,
                                          var_time = var_time,
                                          var_age = var_age,
                                          var_sexgender = var_sexgender)
  offset <- make_offset_effectfree_effect(prior = prior,
                                          dimnames_term = dimnames_term,
                                          var_time = var_time,
                                          var_age = var_age,
                                          var_sexgender = var_sexgender)
  ans <- matrix %*% vals_svd + offset
  ans <- Matrix::as.matrix(ans)
  rownames(ans) <- dimnames_to_levels(dimnames_term)
  ans
}

## HAS_TESTS
#' @export
draw_vals_effect.bage_prior_svd_ar <- function(prior,
                                               vals_hyper,
                                               vals_hyperrand,
                                               vals_spline,
                                               vals_svd,
                                               dimnames_term,
                                               var_time,
                                               var_age,
                                               var_sexgender,
                                               n_sim) {
  matrix <- make_matrix_effectfree_effect(prior = prior,
                                          dimnames_term = dimnames_term,
                                          var_time = var_time,
                                          var_age = var_age,
                                          var_sexgender = var_sexgender)
  offset <- make_offset_effectfree_effect(prior = prior,
                                          dimnames_term = dimnames_term,
                                          var_time = var_time,
                                          var_age = var_age,
                                          var_sexgender = var_sexgender)
  ans <- matrix %*% vals_svd + offset
  ans <- Matrix::as.matrix(ans)
  rownames(ans) <- dimnames_to_levels(dimnames_term)
  ans
}

## HAS_TESTS
#' @export
draw_vals_effect.bage_prior_svd_rwrandom <- function(prior,
                                               vals_hyper,
                                               vals_hyperrand,
                                               vals_spline,
                                               vals_svd,
                                               dimnames_term,
                                               var_time,
                                               var_age,
                                               var_sexgender,
                                               n_sim) {
  con <- prior$specific$con
  matrix <- make_matrix_effectfree_effect_inner(prior = prior,
                                                dimnames_term = dimnames_term,
                                                var_time = var_time,
                                                var_age = var_age,
                                                var_sexgender = var_sexgender,
                                                append_zero = FALSE,
                                                con = con)
  offset <- make_offset_effectfree_effect_svd(prior = prior,
                                              dimnames_term = dimnames_term,
                                              var_time = var_time,
                                              var_age = var_age,
                                              var_sexgender = var_sexgender)
  ans <- matrix %*% vals_svd + offset
  ans <- Matrix::as.matrix(ans)
  rownames(ans) <- dimnames_to_levels(dimnames_term)
  ans
}

## HAS_TESTS
#' @export
draw_vals_effect.bage_prior_svd_rwzero <- function(prior,
                                                   vals_hyper,
                                                   vals_hyperrand,
                                                   vals_spline,
                                                   vals_svd,
                                                   dimnames_term,
                                                   var_time,
                                                   var_age,
                                                   var_sexgender,
                                                   n_sim) {
  con <- prior$specific$con
  matrix <- make_matrix_effectfree_effect_inner(prior = prior,
                                                dimnames_term = dimnames_term,
                                                var_time = var_time,
                                                var_age = var_age,
                                                var_sexgender = var_sexgender,
                                                append_zero = FALSE,
                                                con = con)
  offset <- make_offset_effectfree_effect_svd(prior = prior,
                                              dimnames_term = dimnames_term,
                                              var_time = var_time,
                                              var_age = var_age,
                                              var_sexgender = var_sexgender)
  ans <- matrix %*% vals_svd + offset
  ans <- Matrix::as.matrix(ans)
  rownames(ans) <- dimnames_to_levels(dimnames_term)
  ans
}

## HAS_TESTS
#' @export
draw_vals_effect.bage_prior_svd_rw2random <- function(prior,
                                                      vals_hyper,
                                                      vals_hyperrand,
                                                      vals_spline,
                                                      vals_svd,
                                                      dimnames_term,
                                                      var_time,
                                                      var_age,
                                                      var_sexgender,
                                                      n_sim) {
  con <- prior$specific$con
  matrix <- make_matrix_effectfree_effect_inner(prior = prior,
                                                dimnames_term = dimnames_term,
                                                var_time = var_time,
                                                var_age = var_age,
                                                var_sexgender = var_sexgender,
                                                append_zero = FALSE,
                                                con = con)
  offset <- make_offset_effectfree_effect_svd(prior = prior,
                                              dimnames_term = dimnames_term,
                                              var_time = var_time,
                                              var_age = var_age,
                                              var_sexgender = var_sexgender)
  ans <- matrix %*% vals_svd + offset
  ans <- Matrix::as.matrix(ans)
  rownames(ans) <- dimnames_to_levels(dimnames_term)
  ans
}


## HAS_TESTS
#' @export
draw_vals_effect.bage_prior_svd_rw2zero <- function(prior,
                                                    vals_hyper,
                                                    vals_hyperrand,
                                                    vals_spline,
                                                    vals_svd,
                                                    dimnames_term,
                                                    var_time,
                                                    var_age,
                                                    var_sexgender,
                                                    n_sim) {
  con <- prior$specific$con
  matrix <- make_matrix_effectfree_effect_inner(prior = prior,
                                                dimnames_term = dimnames_term,
                                                var_time = var_time,
                                                var_age = var_age,
                                                var_sexgender = var_sexgender,
                                                append_zero = FALSE,
                                                con = con)
  offset <- make_offset_effectfree_effect_svd(prior = prior,
                                              dimnames_term = dimnames_term,
                                              var_time = var_time,
                                              var_age = var_age,
                                              var_sexgender = var_sexgender)
  ans <- matrix %*% vals_svd + offset
  ans <- Matrix::as.matrix(ans)
  rownames(ans) <- dimnames_to_levels(dimnames_term)
  ans
}


## 'draw_vals_hyper' ----------------------------------------------------------

#' Draw values for hyper-parameters
#'
#' @param prior Object of class 'bage_prior'
#' @param n_sim Number of simulation draws
#'
#' @returns A named list.
#'
#' @noRd
draw_vals_hyper <- function(prior, n_sim) {
  UseMethod("draw_vals_hyper")
}

## HAS_TESTS
#' @export
draw_vals_hyper.bage_prior_ar <- function(prior, n_sim) {
  coef <- draw_vals_coef(prior = prior, n_sim = n_sim)
  sd <- draw_vals_sd(prior = prior, n_sim = n_sim)
  list(coef = coef,
       sd = sd)
}

## HAS_TESTS
#' @export
draw_vals_hyper.bage_prior_known <- function(prior, n_sim)
  list()

## HAS_TESTS
#' @export
draw_vals_hyper.bage_prior_lin <- function(prior, n_sim) {
  sd <- draw_vals_sd(prior = prior, n_sim = n_sim)
  list(sd = sd)
}

## HAS_TESTS
#' @export
draw_vals_hyper.bage_prior_linar <- function(prior, n_sim) {
  coef <- draw_vals_coef(prior = prior, n_sim = n_sim)
  sd <- draw_vals_sd(prior = prior, n_sim = n_sim)
  list(coef = coef,
       sd = sd)
}

## HAS_TESTS
#' @export
draw_vals_hyper.bage_prior_norm <- function(prior, n_sim) {
  sd <- draw_vals_sd(prior = prior, n_sim = n_sim)
  list(sd = sd)
}

## HAS_TESTS
#' @export
draw_vals_hyper.bage_prior_normfixed <- function(prior, n_sim)
  list()

## HAS_TESTS
#' @export
draw_vals_hyper.bage_prior_rwrandom <- function(prior, n_sim) {
  sd <- draw_vals_sd(prior = prior, n_sim = n_sim)
  list(sd = sd)
}

## HAS_TESTS
#' @export
draw_vals_hyper.bage_prior_rwrandomseasfix <- function(prior, n_sim) {
  sd <- draw_vals_sd(prior = prior, n_sim = n_sim)
  list(sd = sd)
}

## HAS_TESTS
#' @export
draw_vals_hyper.bage_prior_rwrandomseasvary <- function(prior, n_sim) {
  sd_seas <- draw_vals_sd_seas(prior = prior, n_sim = n_sim)
  sd <- draw_vals_sd(prior = prior, n_sim = n_sim)
  list(sd_seas = sd_seas,
       sd = sd)
}

## HAS_TESTS
#' @export
draw_vals_hyper.bage_prior_rwzero <- function(prior, n_sim) {
  sd <- draw_vals_sd(prior = prior, n_sim = n_sim)
  list(sd = sd)
}

## HAS_TESTS
#' @export
draw_vals_hyper.bage_prior_rwzeroseasfix <- function(prior, n_sim) {
  sd <- draw_vals_sd(prior = prior, n_sim = n_sim)
  list(sd = sd)
}

## HAS_TESTS
#' @export
draw_vals_hyper.bage_prior_rwzeroseasvary <- function(prior, n_sim) {
  sd_seas <- draw_vals_sd_seas(prior = prior, n_sim = n_sim)
  sd <- draw_vals_sd(prior = prior, n_sim = n_sim)
  list(sd_seas = sd_seas,
       sd = sd)
}

## HAS_TESTS
#' @export
draw_vals_hyper.bage_prior_rw2infant <- function(prior, n_sim) {
  sd <- draw_vals_sd(prior = prior, n_sim = n_sim)
  list(sd = sd)
}

## HAS_TESTS
#' @export
draw_vals_hyper.bage_prior_rw2random <- function(prior, n_sim) {
  sd <- draw_vals_sd(prior = prior, n_sim = n_sim)
  list(sd = sd)
}

## HAS_TESTS
#' @export
draw_vals_hyper.bage_prior_rw2randomseasfix <- function(prior, n_sim) {
  sd <- draw_vals_sd(prior = prior, n_sim = n_sim)
  list(sd = sd)
}

## HAS_TESTS
#' @export
draw_vals_hyper.bage_prior_rw2randomseasvary <- function(prior, n_sim) {
  sd_seas <- draw_vals_sd_seas(prior = prior, n_sim = n_sim)
  sd <- draw_vals_sd(prior = prior, n_sim = n_sim)
  list(sd_seas = sd_seas,
       sd = sd)
}

## HAS_TESTS
#' @export
draw_vals_hyper.bage_prior_rw2zero <- function(prior, n_sim) {
  sd <- draw_vals_sd(prior = prior, n_sim = n_sim)
  list(sd = sd)
}

## HAS_TESTS
#' @export
draw_vals_hyper.bage_prior_rw2zeroseasfix <- function(prior, n_sim) {
  sd <- draw_vals_sd(prior = prior, n_sim = n_sim)
  list(sd = sd)
}

## HAS_TESTS
#' @export
draw_vals_hyper.bage_prior_rw2zeroseasvary <- function(prior, n_sim) {
  sd_seas <- draw_vals_sd_seas(prior = prior, n_sim = n_sim)
  sd <- draw_vals_sd(prior = prior, n_sim = n_sim)
  list(sd_seas = sd_seas,
       sd = sd)
}

## HAS_TESTS
#' @export
draw_vals_hyper.bage_prior_spline <- function(prior, n_sim) {
  sd <- draw_vals_sd(prior = prior, n_sim = n_sim)
  list(sd = sd)
}

## HAS_TESTS
#' @export
draw_vals_hyper.bage_prior_svd <- function(prior, n_sim)
  list()

## HAS_TESTS
#' @export
draw_vals_hyper.bage_prior_svd_ar <- function(prior, n_sim) {
  coef <- draw_vals_coef(prior = prior, n_sim = n_sim)
  sd <- draw_vals_sd(prior = prior, n_sim = n_sim)
  list(coef = coef,
       sd = sd)
}

## HAS_TESTS
#' @export
draw_vals_hyper.bage_prior_svd_rwrandom <- function(prior, n_sim) {
  sd <- draw_vals_sd(prior = prior, n_sim = n_sim)
  list(sd = sd)
}

## HAS_TESTS
#' @export
draw_vals_hyper.bage_prior_svd_rwzero <- function(prior, n_sim) {
  sd <- draw_vals_sd(prior = prior, n_sim = n_sim)
  list(sd = sd)
}

## HAS_TESTS
#' @export
draw_vals_hyper.bage_prior_svd_rw2random <- function(prior, n_sim) {
  sd <- draw_vals_sd(prior = prior, n_sim = n_sim)
  list(sd = sd)
}

## HAS_TESTS
#' @export
draw_vals_hyper.bage_prior_svd_rw2zero <- function(prior, n_sim) {
  sd <- draw_vals_sd(prior = prior, n_sim = n_sim)
  list(sd = sd)
}


## 'draw_vals_hyperrand' ------------------------------------------------------

#' Draw Values for Hyper-Parameters that can be Treated as Random Effects
#'
#' @param prior Object of class 'bage_prior'
#' @param vals_hyper Named list with values of ordinary hyper-parameters
#' @param dimnames_term Dimnames for array representation of term
#' @param var_time Name of time variable
#' @param var_age Name of age variable
#' @param n_sim Number of simulation draws
#'
#' @returns A named list.
#'
#' @noRd
draw_vals_hyperrand <- function(prior,
                                vals_hyper,
                                dimnames_term,
                                var_time,
                                var_age,
                                n_sim) {
  UseMethod("draw_vals_hyperrand")
}

## HAS_TESTS
#' @export
draw_vals_hyperrand.bage_prior <- function(prior,
                                           vals_hyper,
                                           dimnames_term,
                                           var_time,
                                           var_age,
                                           n_sim) {
  list()
}

## HAS_TESTS
#' @export
draw_vals_hyperrand.bage_prior_lin <- function(prior,
                                               vals_hyper,
                                               dimnames_term,
                                               var_time,
                                               var_age,
                                               n_sim) {
  con <- prior$specific$con
  mean_slope <- prior$const[["mean_slope"]]
  sd_slope <- prior$const[["sd_slope"]]
  sd <- vals_hyper$sd
  matrix_along_by_effect <- make_matrix_along_by_effect(prior = prior,
                                                        dimnames_term = dimnames_term,
                                                        var_time = var_time,
                                                        var_age = var_age)
  slope <- draw_vals_slope(mean_slope = mean_slope,
                           sd_slope = sd_slope,
                           matrix_along_by = matrix_along_by_effect,
                           n_sim = n_sim)
  labels <- dimnames_to_levels(dimnames_term)
  trend <- draw_vals_lintrend(slope = slope,
                              matrix_along_by = matrix_along_by_effect,
                              labels = labels)
  error <- draw_vals_norm(sd = sd,
                          labels = labels)
  if (con == "by") {
    i_along <- make_i_along(prior = prior,
                            dimnames_term = dimnames_term,
                            var_time = var_time,
                            var_age = var_age)
    dim <- lengths(dimnames_term)
    m <- make_matrix_con_by(i_along = i_along,
                              dim = dim)
    trend <- m %*% trend
    error <- m %*% error
    trend <- Matrix::as.matrix(trend)
    error <- Matrix::as.matrix(error)
    n_by <- ncol(matrix_along_by_effect)
    slope <- matrix(nrow = n_by, ncol = n_sim)
    for (i_by in seq_len(n_by)) {
      i_1 <- matrix_along_by_effect[1L, i_by] + 1L
      i_2 <- matrix_along_by_effect[2L, i_by] + 1L
      slope[i_by, ] <- trend[i_2, ] - trend[i_1, ]
    }
  }
  list(slope = slope,
       trend = trend,
       error = error)
}

## HAS_TESTS
#' @export
draw_vals_hyperrand.bage_prior_linar <- function(prior,
                                                 vals_hyper,
                                                 dimnames_term,
                                                 var_time,
                                                 var_age,
                                                 n_sim) {
  con <- prior$specific$con
  coef <- vals_hyper$coef
  sd <- vals_hyper$sd
  mean_slope <- prior$const[["mean_slope"]]
  sd_slope <- prior$const[["sd_slope"]]
  matrix_along_by_effect <- make_matrix_along_by_effect(prior = prior,
                                                        dimnames_term = dimnames_term,
                                                        var_time = var_time,
                                                        var_age = var_age)
  slope <- draw_vals_slope(mean_slope = mean_slope,
                           sd_slope = sd_slope,
                           matrix_along_by = matrix_along_by_effect,
                           n_sim = n_sim)
  labels <- dimnames_to_levels(dimnames_term)
  trend <- draw_vals_lintrend(slope = slope,
                              matrix_along_by = matrix_along_by_effect,
                              labels = labels)
  error <- draw_vals_ar(coef = coef,
                        sd = sd,
                        matrix_along_by = matrix_along_by_effect,
                        levels_effect = labels)
  if (con == "by") {
    i_along <- make_i_along(prior = prior,
                            dimnames_term = dimnames_term,
                            var_time = var_time,
                            var_age = var_age)
    dim <- lengths(dimnames_term)
    m <- make_matrix_con_by(i_along = i_along,
                              dim = dim)
    trend <- m %*% trend
    error <- m %*% error
    trend <- Matrix::as.matrix(trend)
    error <- Matrix::as.matrix(error)
    n_by <- ncol(matrix_along_by_effect)
    slope <- matrix(nrow = n_by, ncol = n_sim)
    for (i_by in seq_len(n_by)) {
      i_1 <- matrix_along_by_effect[1L, i_by] + 1L
      i_2 <- matrix_along_by_effect[2L, i_by] + 1L
      slope[i_by, ] <- trend[i_2, ] - trend[i_1, ]
    }
  }
  list(slope = slope,
       trend = trend,
       error = error)
}

## HAS_TESTS
#' @export
draw_vals_hyperrand.bage_prior_rwrandomseasfix <- function(prior,
                                                     vals_hyper,
                                                     dimnames_term,
                                                     var_time,
                                                     var_age,
                                                     n_sim) {
  con <- prior$specific$con
  n_seas <- prior$specific$n_seas
  sd_init <- prior$specific$sd
  sd_init_seas <- prior$specific$sd_seas
  sd <- vals_hyper$sd
  matrix_along_by_effect <- make_matrix_along_by_effect(prior = prior,
                                                        dimnames_term = dimnames_term,
                                                        var_time = var_time,
                                                        var_age = var_age)
  labels <- dimnames_to_levels(dimnames_term)
  trend <- draw_vals_rw(sd = sd,
                        sd_init = sd_init,
                        matrix_along_by = matrix_along_by_effect,
                        levels_effect = labels)
  season <- draw_vals_seasfix(n_seas = n_seas,
                              sd_init = sd_init_seas,
                              matrix_along_by = matrix_along_by_effect,
                              n_sim = n_sim)
  if (con == "by") {
    i_along <- make_i_along(prior = prior,
                            dimnames_term = dimnames_term,
                            var_time = var_time,
                            var_age = var_age)
    dim <- lengths(dimnames_term)
    m <- make_matrix_con_by(i_along = i_along,
                              dim = dim)
    trend <- m %*% trend
    season <- m %*% season
    trend <- Matrix::as.matrix(trend)
    season <- Matrix::as.matrix(season)
  }
  list(trend = trend,
       season = season)
}

## HAS_TESTS
#' @export
draw_vals_hyperrand.bage_prior_rwrandomseasvary <- function(prior,
                                                            vals_hyper,
                                                            dimnames_term,
                                                            var_time,
                                                            var_age,
                                                            n_sim) {
  con <- prior$specific$con
  n_seas <- prior$specific$n_seas
  sd <- vals_hyper$sd
  sd_init <- prior$specific$sd
  sd_init_seas <- prior$specific$sd_seas
  sd_innov_seas <- vals_hyper$sd_seas
  matrix_along_by_effect <- make_matrix_along_by_effect(prior = prior,
                                                        dimnames_term = dimnames_term,
                                                        var_time = var_time,
                                                        var_age = var_age)
  labels <- dimnames_to_levels(dimnames_term)
  trend <- draw_vals_rw(sd = sd,
                        sd_init = sd_init,
                        matrix_along_by = matrix_along_by_effect,
                        levels_effect = labels)
  season <- draw_vals_seasvary(n_seas = n_seas,
                               sd_init = sd_init_seas,
                               sd_innov = sd_innov_seas,
                               matrix_along_by = matrix_along_by_effect)
  if (con == "by") {
    i_along <- make_i_along(prior = prior,
                            dimnames_term = dimnames_term,
                            var_time = var_time,
                            var_age = var_age)
    dim <- lengths(dimnames_term)
    m <- make_matrix_con_by(i_along = i_along,
                              dim = dim)
    trend <- m %*% trend
    season <- m %*% season
    trend <- Matrix::as.matrix(trend)
    season <- Matrix::as.matrix(season)
  }
  list(trend = trend,
       season = season)
}

## HAS_TESTS
#' @export
draw_vals_hyperrand.bage_prior_rwzeroseasfix <- function(prior,
                                                         vals_hyper,
                                                         dimnames_term,
                                                         var_time,
                                                         var_age,
                                                         n_sim) {
  con <- prior$specific$con
  n_seas <- prior$specific$n_seas
  sd_init_seas <- prior$specific$sd_seas
  sd <- vals_hyper$sd
  matrix_along_by_effect <- make_matrix_along_by_effect(prior = prior,
                                                        dimnames_term = dimnames_term,
                                                        var_time = var_time,
                                                        var_age = var_age)
  labels <- dimnames_to_levels(dimnames_term)
  trend <- draw_vals_rw(sd = sd,
                        sd_init = 0,
                        matrix_along_by = matrix_along_by_effect,
                        levels_effect = labels)
  season <- draw_vals_seasfix(n_seas = n_seas,
                              sd_init = sd_init_seas,
                              matrix_along_by = matrix_along_by_effect,
                              n_sim = n_sim)
  if (con == "by") {
    i_along <- make_i_along(prior = prior,
                            dimnames_term = dimnames_term,
                            var_time = var_time,
                            var_age = var_age)
    dim <- lengths(dimnames_term)
    m <- make_matrix_con_by(i_along = i_along,
                              dim = dim)
    trend <- m %*% trend
    season <- m %*% season
    trend <- Matrix::as.matrix(trend)
    season <- Matrix::as.matrix(season)
  }
  list(trend = trend,
       season = season)
}

## HAS_TESTS
#' @export
draw_vals_hyperrand.bage_prior_rwzeroseasvary <- function(prior,
                                                          vals_hyper,
                                                          dimnames_term,
                                                          var_time,
                                                          var_age,
                                                          n_sim) {
  con <- prior$specific$con
  n_seas <- prior$specific$n_seas
  sd <- vals_hyper$sd
  sd_init_seas <- prior$specific$sd_seas
  sd_innov_seas <- vals_hyper$sd_seas
  matrix_along_by_effect <- make_matrix_along_by_effect(prior = prior,
                                                        dimnames_term = dimnames_term,
                                                        var_time = var_time,
                                                        var_age = var_age)
  labels <- dimnames_to_levels(dimnames_term)
  trend <- draw_vals_rw(sd = sd,
                        sd_init = 0,
                        matrix_along_by = matrix_along_by_effect,
                        levels_effect = labels)
  season <- draw_vals_seasvary(n_seas = n_seas,
                               sd_init = sd_init_seas,
                               sd_innov = sd_innov_seas,
                               matrix_along_by = matrix_along_by_effect)
  if (con == "by") {
    i_along <- make_i_along(prior = prior,
                            dimnames_term = dimnames_term,
                            var_time = var_time,
                            var_age = var_age)
    dim <- lengths(dimnames_term)
    m <- make_matrix_con_by(i_along = i_along,
                              dim = dim)
    trend <- m %*% trend
    season <- m %*% season
    trend <- Matrix::as.matrix(trend)
    season <- Matrix::as.matrix(season)
  }
  list(trend = trend,
       season = season)
}

## HAS_TESTS
#' @export
draw_vals_hyperrand.bage_prior_rw2randomseasfix <- function(prior,
                                                      vals_hyper,
                                                      dimnames_term,
                                                      var_time,
                                                      var_age,
                                                      n_sim) {
  con <- prior$specific$con
  n_seas <- prior$specific$n_seas
  sd_init <- prior$specific$sd
  sd_slope <- prior$specific$sd_slope
  sd_init_seas <- prior$specific$sd_seas
  sd <- vals_hyper$sd
  matrix_along_by_effect <- make_matrix_along_by_effect(prior = prior,
                                                        dimnames_term = dimnames_term,
                                                        var_time = var_time,
                                                        var_age = var_age)
  labels <- dimnames_to_levels(dimnames_term)
  trend <- draw_vals_rw2(sd = sd,
                         sd_init = sd_init,
                         sd_slope = sd_slope,
                         matrix_along_by = matrix_along_by_effect,
                         levels_effect = labels)
  season <- draw_vals_seasfix(n_seas = n_seas,
                              sd_init = sd_init_seas,
                              matrix_along_by = matrix_along_by_effect,
                              n_sim = n_sim)
  if (con == "by") {
    i_along <- make_i_along(prior = prior,
                            dimnames_term = dimnames_term,
                            var_time = var_time,
                            var_age = var_age)
    dim <- lengths(dimnames_term)
    m <- make_matrix_con_by(i_along = i_along,
                              dim = dim)
    trend <- m %*% trend
    season <- m %*% season
    trend <- Matrix::as.matrix(trend)
    season <- Matrix::as.matrix(season)
  }
  list(trend = trend,
       season = season)
}

## HAS_TESTS
#' @export
draw_vals_hyperrand.bage_prior_rw2randomseasvary <- function(prior,
                                                             vals_hyper,
                                                             dimnames_term,
                                                             var_time,
                                                             var_age,
                                                             n_sim) {
  con <- prior$specific$con
  n_seas <- prior$specific$n_seas
  sd_init <- prior$specific$sd
  sd_slope <- prior$specific$sd_slope
  sd_init_seas <- prior$specific$sd_seas
  sd_innov_seas <- vals_hyper$sd_seas
  sd <- vals_hyper$sd
  matrix_along_by_effect <- make_matrix_along_by_effect(prior = prior,
                                                        dimnames_term = dimnames_term,
                                                        var_time = var_time,
                                                        var_age = var_age)
  labels <- dimnames_to_levels(dimnames_term)
  trend <- draw_vals_rw2(sd = sd,
                         sd_init = sd_init,
                         sd_slope = sd_slope,
                         matrix_along_by = matrix_along_by_effect,
                         levels_effect = labels)
  season <- draw_vals_seasvary(n_seas = n_seas,
                               sd_init = sd_init_seas,
                               sd_innov = sd_innov_seas,
                               matrix_along_by = matrix_along_by_effect)
  if (con == "by") {
    i_along <- make_i_along(prior = prior,
                            dimnames_term = dimnames_term,
                            var_time = var_time,
                            var_age = var_age)
    dim <- lengths(dimnames_term)
    m <- make_matrix_con_by(i_along = i_along,
                              dim = dim)
    trend <- m %*% trend
    season <- m %*% season
    trend <- Matrix::as.matrix(trend)
    season <- Matrix::as.matrix(season)
  }
  list(trend = trend,
       season = season)
}


## HAS_TESTS
#' @export
draw_vals_hyperrand.bage_prior_rw2zeroseasfix <- function(prior,
                                                          vals_hyper,
                                                          dimnames_term,
                                                          var_time,
                                                          var_age,
                                                          n_sim) {
  con <- prior$specific$con
  n_seas <- prior$specific$n_seas
  sd_slope <- prior$specific$sd_slope
  sd_init_seas <- prior$specific$sd_seas
  sd <- vals_hyper$sd
  matrix_along_by_effect <- make_matrix_along_by_effect(prior = prior,
                                                        dimnames_term = dimnames_term,
                                                        var_time = var_time,
                                                        var_age = var_age)
  labels <- dimnames_to_levels(dimnames_term)
  trend <- draw_vals_rw2(sd = sd,
                         sd_init = 0,
                         sd_slope = sd_slope,
                         matrix_along_by = matrix_along_by_effect,
                         levels_effect = labels)
  season <- draw_vals_seasfix(n_seas = n_seas,
                              sd_init = sd_init_seas,
                              matrix_along_by = matrix_along_by_effect,
                              n_sim = n_sim)
  if (con == "by") {
    i_along <- make_i_along(prior = prior,
                            dimnames_term = dimnames_term,
                            var_time = var_time,
                            var_age = var_age)
    dim <- lengths(dimnames_term)
    m <- make_matrix_con_by(i_along = i_along,
                              dim = dim)
    trend <- m %*% trend
    season <- m %*% season
    trend <- Matrix::as.matrix(trend)
    season <- Matrix::as.matrix(season)
  }
  list(trend = trend,
       season = season)
}

## HAS_TESTS
#' @export
draw_vals_hyperrand.bage_prior_rw2zeroseasvary <- function(prior,
                                                           vals_hyper,
                                                           dimnames_term,
                                                           var_time,
                                                           var_age,
                                                           n_sim) {
  con <- prior$specific$con
  n_seas <- prior$specific$n_seas
  sd_slope <- prior$specific$sd_slope
  sd_init_seas <- prior$specific$sd_seas
  sd_innov_seas <- vals_hyper$sd_seas
  sd <- vals_hyper$sd
  matrix_along_by_effect <- make_matrix_along_by_effect(prior = prior,
                                                        dimnames_term = dimnames_term,
                                                        var_time = var_time,
                                                        var_age = var_age)
  labels <- dimnames_to_levels(dimnames_term)
  trend <- draw_vals_rw2(sd = sd,
                         sd_init = 0,
                         sd_slope = sd_slope,
                         matrix_along_by = matrix_along_by_effect,
                         levels_effect = labels)
  season <- draw_vals_seasvary(n_seas = n_seas,
                               sd_init = sd_init_seas,
                               sd_innov = sd_innov_seas,
                               matrix_along_by = matrix_along_by_effect)
  if (con == "by") {
    i_along <- make_i_along(prior = prior,
                            dimnames_term = dimnames_term,
                            var_time = var_time,
                            var_age = var_age)
    dim <- lengths(dimnames_term)
    m <- make_matrix_con_by(i_along = i_along,
                              dim = dim)
    trend <- m %*% trend
    season <- m %*% season
    trend <- Matrix::as.matrix(trend)
    season <- Matrix::as.matrix(season)
  }
  list(trend = trend,
       season = season)
}


## 'draw_vals_spline' ---------------------------------------------------------

#' Draw Subspace Coefficients for Spline Priors
#'
#' If return value is matrix, the matrix
#' has 'n_effectfree' rows
#' and 'n_sim' columns.
#' 
#' @param prior Object of class 'bage_prior'
#' @param vals_hyper Named list with values of ordinary hyper-parameters
#' @param dimnames_term Dimnames for array
#' representing term
#' @param var_time Name of time variable, or NULL
#' @param var_age Name of age variable, or NULL
#' @param levels_spline Labels for spline coefficients
#' @param n_sim Number of simulation draws
#'
#' @returns A matrix or NULL
#'
#' @noRd
draw_vals_spline <- function(prior,
                             vals_hyper,
                             dimnames_term,
                             var_time,
                             var_age,
                             levels_spline,
                             n_sim) {
  UseMethod("draw_vals_spline")
}

## HAS_TESTS
#' @export
draw_vals_spline.bage_prior <- function(prior,
                                        vals_hyper,
                                        dimnames_term,
                                        var_time,
                                        var_age,
                                        levels_spline,
                                        n_sim) {
  NULL
}

## HAS_TESTS
#' @export
draw_vals_spline.bage_prior_spline <- function(prior,
                                               vals_hyper,
                                               dimnames_term,
                                               var_time,
                                               var_age,
                                               levels_spline,
                                               n_sim) {
  sd_init <- prior$specific$sd
  sd_slope <- prior$specific$sd_slope
  sd <- vals_hyper$sd
  con <- prior$specific$con
  i_along <- make_i_along(prior = prior,
                          dimnames_term = dimnames_term,
                          var_time = var_time,
                          var_age = var_age)
  dim <- lengths(dimnames_term)
  if (con == "by")
    dim[-i_along] <- dim[-i_along] - 1L
  n_along <- dim[[i_along]]
  n_comp <- get_n_comp_spline(prior = prior,
                              n_along = n_along)
  dim[[i_along]] <- n_comp
  matrix_along_by <- make_matrix_along_by_inner(i_along = i_along,
                                                dim = dim)
  draw_vals_rw2(sd = sd,
                sd_init = sd_init,
                sd_slope = sd_slope,
                matrix_along_by = matrix_along_by,
                levels_effect = levels_spline)
}


## 'draw_vals_svd' -----------------------------------------------------------

#' Draw Values in Subspace for SVD-Based Priors
#'
#' If return value is matrix, the matrix
#' has 'n_effectfree' rows
#' and 'n_sim' columns.
#' 
#' @param prior Object of class 'bage_prior'
#' @param vals_hyper Named list with values of ordinary hyper-parameters
#' @param dimnames_term Dimnames for array
#' representing term
#' @param var_time Name of time variable, or NULL
#' @param var_age Name of age variable, or NULL
#' @param var_sexgender Name of sex/gender variable, or NULL
#' @param levels_svd Labels for spline coefficients
#' @param n_sim Number of simulation draws
#'
#' @returns A matrix or NULL
#'
#' @noRd
draw_vals_svd <- function(prior,
                          vals_hyper,
                          dimnames_term,
                          var_time,
                          var_age,
                          var_sexgender,
                          levels_svd,
                          n_sim) {
  UseMethod("draw_vals_svd")
}

## HAS_TESTS
#' @export
draw_vals_svd.bage_prior <- function(prior,
                                     vals_hyper,
                                     dimnames_term,
                                     var_time,
                                     var_age,
                                     var_sexgender,
                                     levels_svd,
                                     n_sim) {
  NULL
}

## HAS_TESTS
#' @export
draw_vals_svd.bage_prior_svd <- function(prior,
                                         vals_hyper,
                                         dimnames_term,
                                         var_time,
                                         var_age,
                                         var_sexgender,
                                         levels_svd,
                                         n_sim) {
  n_svd <- length(levels_svd)
  matrix(stats::rnorm(n = n_svd * n_sim),
         nrow = n_svd,
         ncol = n_sim,
         dimnames = list(levels_svd, NULL))
}

## HAS_TESTS
#' @export
draw_vals_svd.bage_prior_svd_ar <- function(prior,
                                            vals_hyper,
                                            dimnames_term,
                                            var_time,
                                            var_age,
                                            var_sexgender,
                                            levels_svd,
                                            n_sim) {
  coef <- vals_hyper$coef
  sd <- vals_hyper$sd
  matrix_along_by <- make_matrix_along_by_effectfree(prior = prior,
                                                     dimnames_term = dimnames_term,
                                                     var_time = var_time,
                                                     var_age = var_age,
                                                     var_sexgender = var_sexgender)
  draw_vals_ar(coef = coef,
               sd = sd,
               matrix_along_by = matrix_along_by,
               levels_effect = levels_svd)
}

## HAS_TESTS
#' @export
draw_vals_svd.bage_prior_svd_rwrandom <- function(prior,
                                            vals_hyper,
                                            dimnames_term,
                                            var_time,
                                            var_age,
                                            var_sexgender,
                                            levels_svd,
                                            n_sim) {
  sd <- vals_hyper$sd
  sd_init <- prior$specific$sd
  matrix_along_by <- make_matrix_along_by_effectfree_inner(prior = prior,
                                                           dimnames_term = dimnames_term,
                                                           var_time = var_time,
                                                           var_age = var_age,
                                                           var_sexgender = var_sexgender,
                                                           append_zero = FALSE)
  draw_vals_rw(sd = sd,
               sd_init = sd_init,
               matrix_along_by = matrix_along_by,
               levels_effect = levels_svd)
}

## HAS_TESTS
#' @export
draw_vals_svd.bage_prior_svd_rwzero <- function(prior,
                                            vals_hyper,
                                            dimnames_term,
                                            var_time,
                                            var_age,
                                            var_sexgender,
                                            levels_svd,
                                            n_sim) {
  sd <- vals_hyper$sd
  matrix_along_by <- make_matrix_along_by_effectfree_inner(prior = prior,
                                                           dimnames_term = dimnames_term,
                                                           var_time = var_time,
                                                           var_age = var_age,
                                                           var_sexgender = var_sexgender,
                                                           append_zero = FALSE)
  draw_vals_rw(sd = sd,
               sd_init = 0,
               matrix_along_by = matrix_along_by,
               levels_effect = levels_svd)
}

## HAS_TESTS
#' @export
draw_vals_svd.bage_prior_svd_rw2random <- function(prior,
                                                   vals_hyper,
                                                   dimnames_term,
                                                   var_time,
                                                   var_age,
                                                   var_sexgender,
                                                   levels_svd,
                                                   n_sim) {
  sd <- vals_hyper$sd
  sd_init <- prior$specific$sd
  sd_slope <- prior$specific$sd_slope
  matrix_along_by <- make_matrix_along_by_effectfree_inner(prior = prior,
                                                           dimnames_term = dimnames_term,
                                                           var_time = var_time,
                                                           var_age = var_age,
                                                           var_sexgender = var_sexgender,
                                                           append_zero = FALSE)
  draw_vals_rw2(sd = sd,
                sd_init = sd_init,
                sd_slope = sd_slope,
                matrix_along_by = matrix_along_by,
                levels_effect = levels_svd)
}

## HAS_TESTS
#' @export
draw_vals_svd.bage_prior_svd_rw2zero <- function(prior,
                                                 vals_hyper,
                                                 dimnames_term,
                                                 var_time,
                                                 var_age,
                                                 var_sexgender,
                                                 levels_svd,
                                                 n_sim) {
  sd <- vals_hyper$sd
  sd_slope <- prior$specific$sd_slope
  matrix_along_by <- make_matrix_along_by_effectfree_inner(prior = prior,
                                                           dimnames_term = dimnames_term,
                                                           var_time = var_time,
                                                           var_age = var_age,
                                                           var_sexgender = var_sexgender,
                                                           append_zero = FALSE)
  draw_vals_rw2(sd = sd,
                sd_init = 0,
                sd_slope = sd_slope,
                matrix_along_by = matrix_along_by,
                levels_effect = levels_svd)
}


## 'forecast_term' ------------------------------------------------------------

#' Forecast a Main Effect or Interaction
#'
#' Forecast values for a main effect or interaction,
#' given values for hyper-parameters. If the term
#' is an interaction and contains an "along"
#' dimension, then that dimension is guaranteed
#' to be time (checked via function
#' 'check_along_is_time'.
#'
#' @param prior Object of class 'bage_prior'
#' @param dimnames_term Dimnames for array representation of term
#' @param var_time Name of time variable
#' @param var_age Name of age variable
#' @param var_sexgender Name of sex/gender variable
#' @param components Tibble with with output
#' from function 'components'
#' @param labels_forecast Vector
#' with labels for future time periods.
#'
#' @returns A tibble
#'
#' @noRd
forecast_term <- function(prior,
                          dimnames_term,
                          var_time,
                          var_age,
                          var_sexgender,
                          components,
                          labels_forecast) {
  UseMethod("forecast_term")
}

## HAS_TESTS
#' @export
forecast_term.bage_prior <- function(prior,
                                     dimnames_term,
                                     var_time,
                                     var_age,
                                     var_sexgender,
                                     components,
                                     labels_forecast) {
  nm <- dimnames_to_nm(dimnames_term)
  cli::cli_abort(c("Can't forecast term {.val {nm}}.",
                   i = "Term {.val {nm}} has a {.val {str_nm_prior(prior)}} prior.",
                   i = "Terms with a {.val {str_nm_prior(prior)}} prior cannot be forecasted.",
                   i = "For a list of priors that can be forecasted, see {.topic bage::priors}."))
}

## HAS_TESTS
#' @export
forecast_term.bage_prior_ar <- function(prior,
                                        dimnames_term,
                                        var_time,
                                        var_age,
                                        var_sexgender,
                                        components,
                                        labels_forecast) {
  con <- prior$specific$con
  nm <- dimnames_to_nm(dimnames_term)
  dimnames_forecast <- replace(dimnames_term, var_time, list(labels_forecast))
  levels_forecast <- dimnames_to_levels(dimnames_forecast)
  matrix_along_by_est <- make_matrix_along_by_effect(prior = prior,
                                                     dimnames_term = dimnames_term,
                                                     var_time = var_time,
                                                     var_age = var_age)
  matrix_along_by_forecast <- make_matrix_along_by_effect(prior = prior,
                                                          dimnames_term = dimnames_forecast,
                                                          var_time = var_time,
                                                          var_age = var_age)
  is_ar <- with(components,
                term == nm & component == "effect")
  is_coef <- with(components,
                  term == nm & component == "hyper" & startsWith(level, "coef"))
  is_sd <- with(components,
                term == nm & component == "hyper" & level == "sd")
  ar_est <- components$.fitted[is_ar]
  coef <- components$.fitted[is_coef]
  sd <- components$.fitted[is_sd]
  .fitted <- forecast_ar(ar_est = ar_est,
                         coef = coef,
                         sd = sd,
                         matrix_along_by_est = matrix_along_by_est,
                         matrix_along_by_forecast = matrix_along_by_forecast)
  if (con == "by")
    .fitted <- con_by_fitted(prior = prior,
                               fitted = .fitted,
                               dimnames_term = dimnames_forecast,
                               var_time = var_time,
                               var_age = var_age)
  tibble::tibble(term = nm,
                 component = "effect",
                 level = levels_forecast,
                 .fitted = .fitted)
}

## HAS_TESTS
#' @export
forecast_term.bage_prior_lin <- function(prior,
                                         dimnames_term,
                                         var_time,
                                         var_age,
                                         var_sexgender,
                                         components,
                                         labels_forecast) {
  con <- prior$specific$con
  nm <- dimnames_to_nm(dimnames_term)
  dimnames_forecast <- replace(dimnames_term, var_time, list(labels_forecast))
  levels_forecast <- dimnames_to_levels(dimnames_forecast)
  matrix_along_by_est <- make_matrix_along_by_effect(prior = prior,
                                                     dimnames_term = dimnames_term,
                                                     var_time = var_time,
                                                     var_age = var_age)
  matrix_along_by_forecast <- make_matrix_along_by_effect(prior = prior,
                                                          dimnames_term = dimnames_forecast,
                                                          var_time = var_time,
                                                          var_age = var_age)
  is_slope <- with(components, term == nm & component == "hyper" & startsWith(level, "slope"))
  is_sd <- with(components, term == nm & component == "hyper" & level == "sd")
  slope <- components$.fitted[is_slope]
  sd <- components$.fitted[is_sd]
  .fitted <- forecast_lin(slope = slope,
                          sd = sd,
                          matrix_along_by_est = matrix_along_by_est,
                          matrix_along_by_forecast = matrix_along_by_forecast)
  if (con == "by")
    .fitted <- con_by_fitted(prior = prior,
                               fitted = .fitted,
                               dimnames_term = dimnames_forecast,
                               var_time = var_time,
                               var_age = var_age)
  tibble::tibble(term = nm,
                 component = "effect",
                 level = levels_forecast,
                 .fitted = .fitted)
}

## HAS_TESTS
#' @export
forecast_term.bage_prior_linar <- function(prior,
                                           dimnames_term,
                                           var_time,
                                           var_age,
                                           var_sexgender,
                                           components,
                                           labels_forecast) {
  con <- prior$specific$con
  nm <- dimnames_to_nm(dimnames_term)
  dimnames_forecast <- replace(dimnames_term, var_time, list(labels_forecast))
  levels_forecast <- dimnames_to_levels(dimnames_forecast)
  matrix_along_by_est <- make_matrix_along_by_effect(prior = prior,
                                                     dimnames_term = dimnames_term,
                                                     var_time = var_time,
                                                     var_age = var_age)
  matrix_along_by_forecast <- make_matrix_along_by_effect(prior = prior,
                                                          dimnames_term = dimnames_forecast,
                                                          var_time = var_time,
                                                          var_age = var_age)
  is_effect <- with(components, term == nm & component == "effect")
  is_coef <- with(components, term == nm & component == "hyper" & startsWith(level, "coef"))
  is_slope <- with(components, term == nm & component == "hyper" & startsWith(level, "slope"))
  is_sd <- with(components, term == nm & component == "hyper" & level == "sd")
  effect_est <- components$.fitted[is_effect]
  coef <- components$.fitted[is_coef]
  slope <- components$.fitted[is_slope]
  sd <- components$.fitted[is_sd]
  trend_est <- make_lin_trend(slope = slope,
                              matrix_along_by = matrix_along_by_est)
  trend_forecast <- forecast_lin_trend(slope = slope,
                                       matrix_along_by_est = matrix_along_by_est,
                                       matrix_along_by_forecast = matrix_along_by_forecast)
  error_est <- effect_est - trend_est
  error_forecast <- forecast_ar(ar_est = error_est,
                                coef = coef,
                                sd = sd,
                                matrix_along_by_est = matrix_along_by_est,
                                matrix_along_by_forecast = matrix_along_by_forecast)
  effect_forecast <- trend_forecast + error_forecast
  if (con == "by") {
    effect_forecast_old <- effect_forecast
    effect_forecast <- con_by_fitted(prior = prior,
                                       fitted = effect_forecast_old,
                                       dimnames_term = dimnames_forecast,
                                       var_time = var_time,
                                       var_age = var_age)
    diff <- effect_forecast - effect_forecast_old
    error_forecast <- error_forecast + diff
  }
  component <- rep(c("effect", "trend", "error"), each = length(effect_forecast))
  level <- rep(levels_forecast, times = 3L)
  .fitted <- c(effect_forecast, trend_forecast, error_forecast)
  tibble::tibble(term = nm,
                 component = component,
                 level = level,
                 .fitted = .fitted)
}

## HAS_TESTS
#' @export
forecast_term.bage_prior_linex <- function(prior,
                                           dimnames_term,
                                           var_time,
                                           var_age,
                                           var_sexgender,
                                           components,
                                           labels_forecast) {
  con <- prior$specific$con
  nm <- dimnames_to_nm(dimnames_term)
  dimnames_forecast <- replace(dimnames_term, var_time, list(labels_forecast))
  levels_forecast <- dimnames_to_levels(dimnames_forecast)
  matrix_along_by_est <- make_matrix_along_by_effect(prior = prior,
                                                     dimnames_term = dimnames_term,
                                                     var_time = var_time,
                                                     var_age = var_age)
  matrix_along_by_forecast <- make_matrix_along_by_effect(prior = prior,
                                                          dimnames_term = dimnames_forecast,
                                                          var_time = var_time,
                                                          var_age = var_age)
  is_effect <- with(components, term == nm & component == "effect")
  effect <- components$.fitted[is_effect]
  n_by <- ncol(matrix_along_by_est)
  n_draw <- rvec::n_draw(effect)
  slope <- rvec::new_rvec(length = n_by, n_draw = n_draw)
  for (i_by in seq_len(n_by)) {
    i1 <- matrix_along_by_est[1L, i_by] + 1L
    i2 <- matrix_along_by_est[2L, i_by] + 1L
    slope[[i_by]] <- effect[[i2]] - effect[[i1]]
  }
  .fitted <- forecast_lin_trend(slope = slope,
                                matrix_along_by_est = matrix_along_by_est,
                                matrix_along_by_forecast = matrix_along_by_forecast)
  if (con == "by")
    .fitted <- con_by_fitted(prior = prior,
                               fitted = .fitted,
                               dimnames_term = dimnames_forecast,
                               var_time = var_time,
                               var_age = var_age)
  tibble::tibble(term = nm,
                 component = "effect",
                 level = levels_forecast,
                 .fitted = .fitted)
}

## HAS_TESTS
#' @export
forecast_term.bage_prior_norm <- function(prior,
                                        dimnames_term,
                                        var_time,
                                        var_age,
                                        var_sexgender,
                                        components,
                                        labels_forecast) {
  nm <- dimnames_to_nm(dimnames_term)
  dimnames_forecast <- replace(dimnames_term, var_time, list(labels_forecast))
  levels_forecast <- dimnames_to_levels(dimnames_forecast)
  n_forecast <- length(levels_forecast)
  is_sd <- with(components, term == nm & component == "hyper" & level == "sd")
  sd <- components$.fitted[is_sd]
  .fitted <- rvec::rnorm_rvec(n = n_forecast, sd = sd)
  tibble::tibble(term = nm,
                 component = "effect",
                 level = levels_forecast,
                 .fitted = .fitted)
}

## HAS_TESTS
#' @export
forecast_term.bage_prior_normfixed <- function(prior,
                                               dimnames_term,
                                               var_time,
                                               var_age,
                                               var_sexgender,
                                               components,
                                               labels_forecast) {
  nm <- dimnames_to_nm(dimnames_term)
  dimnames_forecast <- replace(dimnames_term, var_time, list(labels_forecast))
  levels_forecast <- dimnames_to_levels(dimnames_forecast)
  n_forecast <- length(levels_forecast)
  sd <- prior$specific$sd
  n_draw <- rvec::n_draw(components$.fitted[[1L]])
  .fitted <- rvec::rnorm_rvec(n = n_forecast,
                              sd = sd,
                              n_draw = n_draw)
  tibble::tibble(term = nm,
                 component = "effect",
                 level = levels_forecast,
                 .fitted = .fitted)
}

## HAS_TESTS
#' @export
forecast_term.bage_prior_rwrandom <- function(prior,
                                              dimnames_term,
                                              var_time,
                                              var_age,
                                              var_sexgender,
                                              components,
                                              labels_forecast) {
  con <- prior$specific$con
  nm <- dimnames_to_nm(dimnames_term)
  dimnames_forecast <- replace(dimnames_term, var_time, list(labels_forecast))
  levels_forecast <- dimnames_to_levels(dimnames_forecast)
  matrix_along_by_est <- make_matrix_along_by_effect(prior = prior,
                                                     dimnames_term = dimnames_term,
                                                     var_time = var_time,
                                                     var_age = var_age)
  matrix_along_by_forecast <- make_matrix_along_by_effect(prior = prior,
                                                          dimnames_term = dimnames_forecast,
                                                          var_time = var_time,
                                                          var_age = var_age)
  is_effect <- with(components, term == nm & component == "effect")
  is_sd <- with(components, term == nm & component == "hyper" & level == "sd")
  rw_est <- components$.fitted[is_effect]
  sd <- components$.fitted[is_sd]
  .fitted <- forecast_rw(rw_est = rw_est,
                         sd = sd,
                         matrix_along_by_est = matrix_along_by_est,
                         matrix_along_by_forecast = matrix_along_by_forecast)
  if (con == "by")
    .fitted <- con_by_fitted(prior = prior,
                             fitted = .fitted,
                             dimnames_term = dimnames_forecast,
                             var_time = var_time,
                             var_age = var_age)
  tibble::tibble(term = nm,
                 component = "effect",
                 level = levels_forecast,
                 .fitted = .fitted)
}

## HAS_TESTS
#' @export
forecast_term.bage_prior_rwrandomseasfix <- function(prior,
                                                   dimnames_term,
                                                   var_time,
                                                   var_age,
                                                   var_sexgender,
                                                   components,
                                                   labels_forecast) {
  con <- prior$specific$con
  n_seas <- prior$specific$n_seas
  nm <- dimnames_to_nm(dimnames_term)
  dimnames_forecast <- replace(dimnames_term, var_time, list(labels_forecast))
  levels_forecast <- dimnames_to_levels(dimnames_forecast)
  matrix_along_by_est <- make_matrix_along_by_effect(prior = prior,
                                                     dimnames_term = dimnames_term,
                                                     var_time = var_time,
                                                     var_age = var_age)
  matrix_along_by_forecast <- make_matrix_along_by_effect(prior = prior,
                                                          dimnames_term = dimnames_forecast,
                                                          var_time = var_time,
                                                          var_age = var_age)
  is_trend <- with(components, term == nm & component == "trend")
  is_season <- with(components, term == nm & component == "season")
  is_sd <- with(components, term == nm & component == "hyper" & level == "sd")
  rw_est <- components$.fitted[is_trend]
  seas_est <- components$.fitted[is_season]
  sd <- components$.fitted[is_sd]
  rw_forecast <- forecast_rw(rw_est = rw_est,
                             sd = sd,
                             matrix_along_by_est = matrix_along_by_est,
                             matrix_along_by_forecast = matrix_along_by_forecast)
  seas_forecast <- forecast_seasfix(n_seas = n_seas,
                                    seas_est = seas_est,
                                    matrix_along_by_est = matrix_along_by_est,
                                    matrix_along_by_forecast = matrix_along_by_forecast)
  
  if (con == "by") {
    rw_forecast <- con_by_fitted(prior = prior,
                                   fitted = rw_forecast,
                                   dimnames_term = dimnames_forecast,
                                   var_time = var_time,
                                   var_age = var_age)
    seas_forecast <- con_by_fitted(prior = prior,
                                     fitted = seas_forecast,
                                     dimnames_term = dimnames_forecast,
                                     var_time = var_time,
                                     var_age = var_age)
  }
  effect_forecast <- rw_forecast + seas_forecast
  n_level <- length(effect_forecast)
  tibble::tibble(term = nm,
                 component = rep(c("effect", "trend", "season"), each = n_level),
                 level = rep(levels_forecast, times = 3L),
                 .fitted = c(effect_forecast, rw_forecast, seas_forecast))
}

## HAS_TESTS
#' @export
forecast_term.bage_prior_rwrandomseasvary <- function(prior,
                                                dimnames_term,
                                                var_time,
                                                var_age,
                                                var_sexgender,
                                                components,
                                                labels_forecast) {
  con <- prior$specific$con
  n_seas <- prior$specific$n_seas
  nm <- dimnames_to_nm(dimnames_term)
  dimnames_forecast <- replace(dimnames_term, var_time, list(labels_forecast))
  levels_forecast <- dimnames_to_levels(dimnames_forecast)
  matrix_along_by_est <- make_matrix_along_by_effect(prior = prior,
                                                     dimnames_term = dimnames_term,
                                                     var_time = var_time,
                                                     var_age = var_age)
  matrix_along_by_forecast <- make_matrix_along_by_effect(prior = prior,
                                                          dimnames_term = dimnames_forecast,
                                                          var_time = var_time,
                                                          var_age = var_age)
  is_trend <- with(components, term == nm & component == "trend")
  is_season <- with(components, term == nm & component == "season")
  is_sd_seas <- with(components, term == nm & component == "hyper" & level == "sd_seas")
  is_sd <- with(components, term == nm & component == "hyper" & level == "sd")
  rw_est <- components$.fitted[is_trend]
  seas_est <- components$.fitted[is_season]
  sd_seas <- components$.fitted[is_sd_seas]
  sd <- components$.fitted[is_sd]
  rw_forecast <- forecast_rw(rw_est = rw_est,
                             sd = sd,
                             matrix_along_by_est = matrix_along_by_est,
                             matrix_along_by_forecast = matrix_along_by_forecast)
  seas_forecast <- forecast_seasvary(n_seas = n_seas,
                                     seas_est = seas_est,
                                     sd = sd_seas,
                                     matrix_along_by_est = matrix_along_by_est,
                                     matrix_along_by_forecast = matrix_along_by_forecast)
  if (con == "by") {
    rw_forecast <- con_by_fitted(prior = prior,
                                   fitted = rw_forecast,
                                   dimnames_term = dimnames_forecast,
                                   var_time = var_time,
                                   var_age = var_age)
    seas_forecast <- con_by_fitted(prior = prior,
                                     fitted = seas_forecast,
                                     dimnames_term = dimnames_forecast,
                                     var_time = var_time,
                                     var_age = var_age)
  }
  effect_forecast <- rw_forecast + seas_forecast
  n_level <- length(effect_forecast)
  tibble::tibble(term = nm,
                 component = rep(c("effect", "trend", "season"), each = n_level),
                 level = rep(levels_forecast, times = 3L),
                 .fitted = c(effect_forecast, rw_forecast, seas_forecast))
}



## HAS_TESTS
#' @export
forecast_term.bage_prior_rwzero <- function(prior,
                                        dimnames_term,
                                        var_time,
                                        var_age,
                                        var_sexgender,
                                        components,
                                        labels_forecast) {
  con <- prior$specific$con
  nm <- dimnames_to_nm(dimnames_term)
  dimnames_forecast <- replace(dimnames_term, var_time, list(labels_forecast))
  levels_forecast <- dimnames_to_levels(dimnames_forecast)
  matrix_along_by_est <- make_matrix_along_by_effect(prior = prior,
                                                     dimnames_term = dimnames_term,
                                                     var_time = var_time,
                                                     var_age = var_age)
  matrix_along_by_forecast <- make_matrix_along_by_effect(prior = prior,
                                                          dimnames_term = dimnames_forecast,
                                                          var_time = var_time,
                                                          var_age = var_age)
  is_effect <- with(components, term == nm & component == "effect")
  is_sd <- with(components, term == nm & component == "hyper" & level == "sd")
  rw_est <- components$.fitted[is_effect]
  sd <- components$.fitted[is_sd]
  .fitted <- forecast_rw(rw_est = rw_est,
                         sd = sd,
                         matrix_along_by_est = matrix_along_by_est,
                         matrix_along_by_forecast = matrix_along_by_forecast)
  if (con == "by")
    .fitted <- con_by_fitted(prior = prior,
                               fitted = .fitted,
                               dimnames_term = dimnames_forecast,
                               var_time = var_time,
                               var_age = var_age)
  tibble::tibble(term = nm,
                 component = "effect",
                 level = levels_forecast,
                 .fitted = .fitted)
}

## HAS_TESTS
#' @export
forecast_term.bage_prior_rwzeroseasfix <- function(prior,
                                                   dimnames_term,
                                                   var_time,
                                                   var_age,
                                                   var_sexgender,
                                                   components,
                                                   labels_forecast) {
  con <- prior$specific$con
  n_seas <- prior$specific$n_seas
  nm <- dimnames_to_nm(dimnames_term)
  dimnames_forecast <- replace(dimnames_term, var_time, list(labels_forecast))
  levels_forecast <- dimnames_to_levels(dimnames_forecast)
  matrix_along_by_est <- make_matrix_along_by_effect(prior = prior,
                                                     dimnames_term = dimnames_term,
                                                     var_time = var_time,
                                                     var_age = var_age)
  matrix_along_by_forecast <- make_matrix_along_by_effect(prior = prior,
                                                          dimnames_term = dimnames_forecast,
                                                          var_time = var_time,
                                                          var_age = var_age)
  is_trend <- with(components, term == nm & component == "trend")
  is_season <- with(components, term == nm & component == "season")
  is_sd <- with(components, term == nm & component == "hyper" & level == "sd")
  rw_est <- components$.fitted[is_trend]
  seas_est <- components$.fitted[is_season]
  sd <- components$.fitted[is_sd]
  rw_forecast <- forecast_rw(rw_est = rw_est,
                             sd = sd,
                             matrix_along_by_est = matrix_along_by_est,
                             matrix_along_by_forecast = matrix_along_by_forecast)
  seas_forecast <- forecast_seasfix(n_seas = n_seas,
                                    seas_est = seas_est,
                                    matrix_along_by_est = matrix_along_by_est,
                                    matrix_along_by_forecast = matrix_along_by_forecast)
  
  if (con == "by") {
    rw_forecast <- con_by_fitted(prior = prior,
                                   fitted = rw_forecast,
                                   dimnames_term = dimnames_forecast,
                                   var_time = var_time,
                                   var_age = var_age)
    seas_forecast <- con_by_fitted(prior = prior,
                                     fitted = seas_forecast,
                                     dimnames_term = dimnames_forecast,
                                     var_time = var_time,
                                     var_age = var_age)
  }
  effect_forecast <- rw_forecast + seas_forecast
  n_level <- length(effect_forecast)
  tibble::tibble(term = nm,
                 component = rep(c("effect", "trend", "season"), each = n_level),
                 level = rep(levels_forecast, times = 3L),
                 .fitted = c(effect_forecast, rw_forecast, seas_forecast))
}

## HAS_TESTS
#' @export
forecast_term.bage_prior_rwzeroseasvary <- function(prior,
                                                dimnames_term,
                                                var_time,
                                                var_age,
                                                var_sexgender,
                                                components,
                                                labels_forecast) {
  con <- prior$specific$con
  n_seas <- prior$specific$n_seas
  nm <- dimnames_to_nm(dimnames_term)
  dimnames_forecast <- replace(dimnames_term, var_time, list(labels_forecast))
  levels_forecast <- dimnames_to_levels(dimnames_forecast)
  matrix_along_by_est <- make_matrix_along_by_effect(prior = prior,
                                                     dimnames_term = dimnames_term,
                                                     var_time = var_time,
                                                     var_age = var_age)
  matrix_along_by_forecast <- make_matrix_along_by_effect(prior = prior,
                                                          dimnames_term = dimnames_forecast,
                                                          var_time = var_time,
                                                          var_age = var_age)
  is_trend <- with(components, term == nm & component == "trend")
  is_season <- with(components, term == nm & component == "season")
  is_sd_seas <- with(components, term == nm & component == "hyper" & level == "sd_seas")
  is_sd <- with(components, term == nm & component == "hyper" & level == "sd")
  rw_est <- components$.fitted[is_trend]
  seas_est <- components$.fitted[is_season]
  sd_seas <- components$.fitted[is_sd_seas]
  sd <- components$.fitted[is_sd]
  rw_forecast <- forecast_rw(rw_est = rw_est,
                             sd = sd,
                             matrix_along_by_est = matrix_along_by_est,
                             matrix_along_by_forecast = matrix_along_by_forecast)
  seas_forecast <- forecast_seasvary(n_seas = n_seas,
                                     seas_est = seas_est,
                                     sd = sd_seas,
                                     matrix_along_by_est = matrix_along_by_est,
                                     matrix_along_by_forecast = matrix_along_by_forecast)
  if (con == "by") {
    rw_forecast <- con_by_fitted(prior = prior,
                                   fitted = rw_forecast,
                                   dimnames_term = dimnames_forecast,
                                   var_time = var_time,
                                   var_age = var_age)
    seas_forecast <- con_by_fitted(prior = prior,
                                     fitted = seas_forecast,
                                     dimnames_term = dimnames_forecast,
                                     var_time = var_time,
                                     var_age = var_age)
  }
  effect_forecast <- rw_forecast + seas_forecast
  n_level <- length(effect_forecast)
  tibble::tibble(term = nm,
                 component = rep(c("effect", "trend", "season"), each = n_level),
                 level = rep(levels_forecast, times = 3L),
                 .fitted = c(effect_forecast, rw_forecast, seas_forecast))
}





## HAS_TESTS
#' @export
forecast_term.bage_prior_rw2random <- function(prior,
                                               dimnames_term,
                                               var_time,
                                               var_age,
                                               var_sexgender,
                                               components,
                                               labels_forecast) {
  con <- prior$specific$con
  nm <- dimnames_to_nm(dimnames_term)
  dimnames_forecast <- replace(dimnames_term, var_time, list(labels_forecast))
  levels_forecast <- dimnames_to_levels(dimnames_forecast)
  matrix_along_by_est <- make_matrix_along_by_effect(prior = prior,
                                                     dimnames_term = dimnames_term,
                                                     var_time = var_time,
                                                     var_age = var_age)
  matrix_along_by_forecast <- make_matrix_along_by_effect(prior = prior,
                                                          dimnames_term = dimnames_forecast,
                                                          var_time = var_time,
                                                          var_age = var_age)
  is_effect <- with(components, term == nm & component == "effect")
  is_sd <- with(components, term == nm & component == "hyper" & level == "sd")
  rw2_est <- components$.fitted[is_effect]
  sd <- components$.fitted[is_sd]
  .fitted <- forecast_rw2(rw2_est = rw2_est,
                          sd = sd,
                          matrix_along_by_est = matrix_along_by_est,
                          matrix_along_by_forecast = matrix_along_by_forecast)
  if (con == "by")
    .fitted <- con_by_fitted(prior = prior,
                               fitted = .fitted,
                               dimnames_term = dimnames_forecast,
                               var_time = var_time,
                               var_age = var_age)
  tibble::tibble(term = nm,
                 component = "effect",
                 level = levels_forecast,
                 .fitted = .fitted)
}

## HAS_TESTS
#' @export
forecast_term.bage_prior_rw2randomseasfix <- function(prior,
                                                      dimnames_term,
                                                      var_time,
                                                      var_age,
                                                      var_sexgender,
                                                      components,
                                                      labels_forecast) {
  con <- prior$specific$con
  n_seas <- prior$specific$n_seas
  nm <- dimnames_to_nm(dimnames_term)
  dimnames_forecast <- replace(dimnames_term, var_time, list(labels_forecast))
  levels_forecast <- dimnames_to_levels(dimnames_forecast)
  matrix_along_by_est <- make_matrix_along_by_effect(prior = prior,
                                                     dimnames_term = dimnames_term,
                                                     var_time = var_time,
                                                     var_age = var_age)
  matrix_along_by_forecast <- make_matrix_along_by_effect(prior = prior,
                                                          dimnames_term = dimnames_forecast,
                                                          var_time = var_time,
                                                          var_age = var_age)
  is_trend <- with(components, term == nm & component == "trend")
  is_season <- with(components, term == nm & component == "season")
  is_sd <- with(components, term == nm & component == "hyper" & level == "sd")
  rw2_est <- components$.fitted[is_trend]
  seas_est <- components$.fitted[is_season]
  sd <- components$.fitted[is_sd]
  rw2_forecast <- forecast_rw2(rw2_est = rw2_est,
                               sd = sd,
                               matrix_along_by_est = matrix_along_by_est,
                               matrix_along_by_forecast = matrix_along_by_forecast)
  seas_forecast <- forecast_seasfix(n_seas = n_seas,
                                    seas_est = seas_est,
                                    matrix_along_by_est = matrix_along_by_est,
                                    matrix_along_by_forecast = matrix_along_by_forecast)
  if (con == "by") {
    rw2_forecast <- con_by_fitted(prior = prior,
                                    fitted = rw2_forecast,
                                    dimnames_term = dimnames_forecast,
                                    var_time = var_time,
                                    var_age = var_age)
    seas_forecast <- con_by_fitted(prior = prior,
                                     fitted = seas_forecast,
                                     dimnames_term = dimnames_forecast,
                                     var_time = var_time,
                                     var_age = var_age)
  }
  effect_forecast <- rw2_forecast + seas_forecast
  n_level <- length(effect_forecast)
  tibble::tibble(term = nm,
                 component = rep(c("effect", "trend", "season"), each = n_level),
                 level = rep(levels_forecast, times = 3L),
                 .fitted = c(effect_forecast, rw2_forecast, seas_forecast))
}

## HAS_TESTS
#' @export
forecast_term.bage_prior_rw2randomseasvary <- function(prior,
                                                       dimnames_term,
                                                       var_time,
                                                       var_age,
                                                       var_sexgender,
                                                       components,
                                                       labels_forecast) {
  con <- prior$specific$con
  n_seas <- prior$specific$n_seas
  nm <- dimnames_to_nm(dimnames_term)
  dimnames_forecast <- replace(dimnames_term, var_time, list(labels_forecast))
  levels_forecast <- dimnames_to_levels(dimnames_forecast)
  matrix_along_by_est <- make_matrix_along_by_effect(prior = prior,
                                                     dimnames_term = dimnames_term,
                                                     var_time = var_time,
                                                     var_age = var_age)
  matrix_along_by_forecast <- make_matrix_along_by_effect(prior = prior,
                                                          dimnames_term = dimnames_forecast,
                                                          var_time = var_time,
                                                          var_age = var_age)
  is_trend <- with(components, term == nm & component == "trend")
  is_season <- with(components, term == nm & component == "season")
  is_sd_seas <- with(components, term == nm & component == "hyper" & level == "sd_seas")
  is_sd <- with(components, term == nm & component == "hyper" & level == "sd")
  rw2_est <- components$.fitted[is_trend]
  seas_est <- components$.fitted[is_season]
  sd_seas <- components$.fitted[is_sd_seas]
  sd <- components$.fitted[is_sd]
  rw2_forecast <- forecast_rw2(rw2_est = rw2_est,
                               sd = sd,
                               matrix_along_by_est = matrix_along_by_est,
                               matrix_along_by_forecast = matrix_along_by_forecast)
  seas_forecast <- forecast_seasvary(n_seas = n_seas,
                                     seas_est = seas_est,
                                     sd = sd_seas,
                                     matrix_along_by_est = matrix_along_by_est,
                                     matrix_along_by_forecast = matrix_along_by_forecast)
  if (con == "by") {
    rw2_forecast <- con_by_fitted(prior = prior,
                                    fitted = rw2_forecast,
                                    dimnames_term = dimnames_forecast,
                                    var_time = var_time,
                                    var_age = var_age)
    seas_forecast <- con_by_fitted(prior = prior,
                                     fitted = seas_forecast,
                                     dimnames_term = dimnames_forecast,
                                     var_time = var_time,
                                     var_age = var_age)
  }
  effect_forecast <- rw2_forecast + seas_forecast
  n_level <- length(effect_forecast)
  tibble::tibble(term = nm,
                 component = rep(c("effect", "trend", "season"), each = n_level),
                 level = rep(levels_forecast, times = 3L),
                 .fitted = c(effect_forecast, rw2_forecast, seas_forecast))
}

## HAS_TESTS
#' @export
forecast_term.bage_prior_rw2zero <- function(prior,
                                             dimnames_term,
                                             var_time,
                                             var_age,
                                             var_sexgender,
                                             components,
                                             labels_forecast) {
  con <- prior$specific$con
  nm <- dimnames_to_nm(dimnames_term)
  dimnames_forecast <- replace(dimnames_term, var_time, list(labels_forecast))
  levels_forecast <- dimnames_to_levels(dimnames_forecast)
  matrix_along_by_est <- make_matrix_along_by_effect(prior = prior,
                                                     dimnames_term = dimnames_term,
                                                     var_time = var_time,
                                                     var_age = var_age)
  matrix_along_by_forecast <- make_matrix_along_by_effect(prior = prior,
                                                          dimnames_term = dimnames_forecast,
                                                          var_time = var_time,
                                                          var_age = var_age)
  is_effect <- with(components, term == nm & component == "effect")
  is_sd <- with(components, term == nm & component == "hyper" & level == "sd")
  rw2_est <- components$.fitted[is_effect]
  sd <- components$.fitted[is_sd]
  .fitted <- forecast_rw2(rw2_est = rw2_est,
                          sd = sd,
                          matrix_along_by_est = matrix_along_by_est,
                          matrix_along_by_forecast = matrix_along_by_forecast)
  if (con == "by")
    .fitted <- con_by_fitted(prior = prior,
                               fitted = .fitted,
                               dimnames_term = dimnames_forecast,
                               var_time = var_time,
                               var_age = var_age)
  tibble::tibble(term = nm,
                 component = "effect",
                 level = levels_forecast,
                 .fitted = .fitted)
}

## HAS_TESTS
#' @export
forecast_term.bage_prior_rw2zeroseasfix <- function(prior,
                                                    dimnames_term,
                                                    var_time,
                                                    var_age,
                                                    var_sexgender,
                                                    components,
                                                    labels_forecast) {
  con <- prior$specific$con
  n_seas <- prior$specific$n_seas
  nm <- dimnames_to_nm(dimnames_term)
  dimnames_forecast <- replace(dimnames_term, var_time, list(labels_forecast))
  levels_forecast <- dimnames_to_levels(dimnames_forecast)
  matrix_along_by_est <- make_matrix_along_by_effect(prior = prior,
                                                     dimnames_term = dimnames_term,
                                                     var_time = var_time,
                                                     var_age = var_age)
  matrix_along_by_forecast <- make_matrix_along_by_effect(prior = prior,
                                                          dimnames_term = dimnames_forecast,
                                                          var_time = var_time,
                                                          var_age = var_age)
  is_trend <- with(components, term == nm & component == "trend")
  is_season <- with(components, term == nm & component == "season")
  is_sd <- with(components, term == nm & component == "hyper" & level == "sd")
  rw2_est <- components$.fitted[is_trend]
  seas_est <- components$.fitted[is_season]
  sd <- components$.fitted[is_sd]
  rw2_forecast <- forecast_rw2(rw2_est = rw2_est,
                               sd = sd,
                               matrix_along_by_est = matrix_along_by_est,
                               matrix_along_by_forecast = matrix_along_by_forecast)
  seas_forecast <- forecast_seasfix(n_seas = n_seas,
                                    seas_est = seas_est,
                                    matrix_along_by_est = matrix_along_by_est,
                                    matrix_along_by_forecast = matrix_along_by_forecast)
  if (con == "by") {
    rw2_forecast <- con_by_fitted(prior = prior,
                                    fitted = rw2_forecast,
                                    dimnames_term = dimnames_forecast,
                                    var_time = var_time,
                                    var_age = var_age)
    seas_forecast <- con_by_fitted(prior = prior,
                                     fitted = seas_forecast,
                                     dimnames_term = dimnames_forecast,
                                     var_time = var_time,
                                     var_age = var_age)
  }
  effect_forecast <- rw2_forecast + seas_forecast
  n_level <- length(effect_forecast)
  tibble::tibble(term = nm,
                 component = rep(c("effect", "trend", "season"), each = n_level),
                 level = rep(levels_forecast, times = 3L),
                 .fitted = c(effect_forecast, rw2_forecast, seas_forecast))
}

## HAS_TESTS
#' @export
forecast_term.bage_prior_rw2zeroseasvary <- function(prior,
                                                     dimnames_term,
                                                     var_time,
                                                     var_age,
                                                     var_sexgender,
                                                     components,
                                                     labels_forecast) {
  con <- prior$specific$con
  n_seas <- prior$specific$n_seas
  nm <- dimnames_to_nm(dimnames_term)
  dimnames_forecast <- replace(dimnames_term, var_time, list(labels_forecast))
  levels_forecast <- dimnames_to_levels(dimnames_forecast)
  matrix_along_by_est <- make_matrix_along_by_effect(prior = prior,
                                                     dimnames_term = dimnames_term,
                                                     var_time = var_time,
                                                     var_age = var_age)
  matrix_along_by_forecast <- make_matrix_along_by_effect(prior = prior,
                                                          dimnames_term = dimnames_forecast,
                                                          var_time = var_time,
                                                          var_age = var_age)
  is_trend <- with(components, term == nm & component == "trend")
  is_season <- with(components, term == nm & component == "season")
  is_sd_seas <- with(components, term == nm & component == "hyper" & level == "sd_seas")
  is_sd <- with(components, term == nm & component == "hyper" & level == "sd")
  rw2_est <- components$.fitted[is_trend]
  seas_est <- components$.fitted[is_season]
  sd_seas <- components$.fitted[is_sd_seas]
  sd <- components$.fitted[is_sd]
  rw2_forecast <- forecast_rw2(rw2_est = rw2_est,
                               sd = sd,
                               matrix_along_by_est = matrix_along_by_est,
                               matrix_along_by_forecast = matrix_along_by_forecast)
  seas_forecast <- forecast_seasvary(n_seas = n_seas,
                                     seas_est = seas_est,
                                     sd = sd_seas,
                                     matrix_along_by_est = matrix_along_by_est,
                                     matrix_along_by_forecast = matrix_along_by_forecast)
  if (con == "by") {
    rw2_forecast <- con_by_fitted(prior = prior,
                                    fitted = rw2_forecast,
                                    dimnames_term = dimnames_forecast,
                                    var_time = var_time,
                                    var_age = var_age)
    seas_forecast <- con_by_fitted(prior = prior,
                                     fitted = seas_forecast,
                                     dimnames_term = dimnames_forecast,
                                     var_time = var_time,
                                     var_age = var_age)
  }
  effect_forecast <- rw2_forecast + seas_forecast
  n_level <- length(effect_forecast)
  tibble::tibble(term = nm,
                 component = rep(c("effect", "trend", "season"), each = n_level),
                 level = rep(levels_forecast, times = 3L),
                 .fitted = c(effect_forecast, rw2_forecast, seas_forecast))
}

## HAS_TESTS
#' @export
forecast_term.bage_prior_svd_ar <- function(prior,
                                            dimnames_term,
                                            var_time,
                                            var_age,
                                            var_sexgender,
                                            components,
                                            labels_forecast) {
  dimnames_forecast <- replace(dimnames_term, var_time, list(labels_forecast))
  svd_forecast <- forecast_ar_svd(prior = prior,
                                  dimnames_term = dimnames_term,
                                  dimnames_forecast = dimnames_forecast,
                                  var_time = var_time,
                                  var_age = var_age,
                                  var_sexgender = var_sexgender,
                                  components = components,
                                  labels_forecast)
  matrix <- make_matrix_effectfree_effect(prior = prior,
                                          dimnames_term = dimnames_forecast,
                                          var_time = var_time,
                                          var_age = var_age,
                                          var_sexgender = var_sexgender)
  offset <- make_offset_effectfree_effect(prior = prior,
                                          dimnames_term = dimnames_forecast,
                                          var_time = var_time,
                                          var_age = var_age,
                                          var_sexgender = var_sexgender)
  ## tricky combining rvecs with sparse matrices
  effect_forecast <- matrix %*% as.matrix(svd_forecast) + offset
  effect_forecast <- rvec::rvec_dbl(as.matrix(effect_forecast))
  nm <- dimnames_to_nm(dimnames_term)
  levels_effect <- dimnames_to_levels(dimnames_forecast)
  levels_svd <- make_levels_svd_term(prior = prior,
                                     dimnames_term = dimnames_forecast,
                                     var_time = var_time,
                                     var_age = var_age,
                                     var_sexgender = var_sexgender)
  svd <- tibble::tibble(term = nm,
                        component = "svd",
                        level = levels_svd,
                        .fitted = svd_forecast)
  effect <- tibble::tibble(term = nm,
                           component = "effect",
                           level = levels_effect,
                           .fitted = effect_forecast)
  vctrs::vec_rbind(effect, svd)
}

## HAS_TESTS
#' @export
forecast_term.bage_prior_svd_rwrandom <- function(prior,
                                            dimnames_term,
                                            var_time,
                                            var_age,
                                            var_sexgender,
                                            components,
                                            labels_forecast) {
  con <- prior$specific$con
  dimnames_forecast <- replace(dimnames_term, var_time, list(labels_forecast))
  svd_forecast <- forecast_rw_svd(prior = prior,
                                  dimnames_term = dimnames_term,
                                  dimnames_forecast = dimnames_forecast,
                                  var_time = var_time,
                                  var_age = var_age,
                                  var_sexgender = var_sexgender,
                                  components = components,
                                  labels_forecast)
  matrix <- make_matrix_effectfree_effect_inner(prior = prior,
                                                dimnames_term = dimnames_forecast,
                                                var_time = var_time,
                                                var_age = var_age,
                                                var_sexgender = var_sexgender,
                                                append_zero = FALSE,
                                                con = con)
  offset <- make_offset_effectfree_effect_svd(prior = prior,
                                              dimnames_term = dimnames_forecast,
                                              var_time = var_time,
                                              var_age = var_age,
                                              var_sexgender = var_sexgender)
  ## tricky combining rvecs with sparse matrices
  effect_forecast <- matrix %*% as.matrix(svd_forecast) + offset
  effect_forecast <- rvec::rvec_dbl(as.matrix(effect_forecast))
  nm <- dimnames_to_nm(dimnames_term)
  levels_effect <- dimnames_to_levels(dimnames_forecast)
  levels_svd <- make_levels_svd_term(prior = prior,
                                     dimnames_term = dimnames_forecast,
                                     var_time = var_time,
                                     var_age = var_age,
                                     var_sexgender = var_sexgender)
  svd <- tibble::tibble(term = nm,
                        component = "svd",
                        level = levels_svd,
                        .fitted = svd_forecast)
  effect <- tibble::tibble(term = nm,
                           component = "effect",
                           level = levels_effect,
                           .fitted = effect_forecast)
  vctrs::vec_rbind(effect, svd)
}

## HAS_TESTS
#' @export
forecast_term.bage_prior_svd_rwzero <- function(prior,
                                            dimnames_term,
                                            var_time,
                                            var_age,
                                            var_sexgender,
                                            components,
                                            labels_forecast) {
  con <- prior$specific$con
  dimnames_forecast <- replace(dimnames_term, var_time, list(labels_forecast))
  svd_forecast <- forecast_rw_svd(prior = prior,
                                  dimnames_term = dimnames_term,
                                  dimnames_forecast = dimnames_forecast,
                                  var_time = var_time,
                                  var_age = var_age,
                                  var_sexgender = var_sexgender,
                                  components = components,
                                  labels_forecast)
  matrix <- make_matrix_effectfree_effect_inner(prior = prior,
                                                dimnames_term = dimnames_forecast,
                                                var_time = var_time,
                                                var_age = var_age,
                                                var_sexgender = var_sexgender,
                                                append_zero = FALSE,
                                                con = con)
  offset <- make_offset_effectfree_effect_svd(prior = prior,
                                              dimnames_term = dimnames_forecast,
                                              var_time = var_time,
                                              var_age = var_age,
                                              var_sexgender = var_sexgender)
  ## tricky combining rvecs with sparse matrices
  effect_forecast <- matrix %*% as.matrix(svd_forecast) + offset
  effect_forecast <- rvec::rvec_dbl(as.matrix(effect_forecast))
  nm <- dimnames_to_nm(dimnames_term)
  levels_effect <- dimnames_to_levels(dimnames_forecast)
  levels_svd <- make_levels_svd_term(prior = prior,
                                     dimnames_term = dimnames_forecast,
                                     var_time = var_time,
                                     var_age = var_age,
                                     var_sexgender = var_sexgender)
  svd <- tibble::tibble(term = nm,
                        component = "svd",
                        level = levels_svd,
                        .fitted = svd_forecast)
  effect <- tibble::tibble(term = nm,
                           component = "effect",
                           level = levels_effect,
                           .fitted = effect_forecast)
  vctrs::vec_rbind(effect, svd)
}


## HAS_TESTS
#' @export
forecast_term.bage_prior_svd_rw2random <- function(prior,
                                             dimnames_term,
                                             var_time,
                                             var_age,
                                             var_sexgender,
                                             components,
                                             labels_forecast) {
  con <- prior$specific$con
  dimnames_forecast <- replace(dimnames_term, var_time, list(labels_forecast))
  svd_forecast <- forecast_rw2_svd(prior = prior,
                                   dimnames_term = dimnames_term,
                                   dimnames_forecast = dimnames_forecast,
                                   var_time = var_time,
                                   var_age = var_age,
                                   var_sexgender = var_sexgender,
                                   components = components,
                                   labels_forecast)
  matrix <- make_matrix_effectfree_effect_inner(prior = prior,
                                                dimnames_term = dimnames_forecast,
                                                var_time = var_time,
                                                var_age = var_age,
                                                var_sexgender = var_sexgender,
                                                append_zero = FALSE,
                                                con = con)
  offset <- make_offset_effectfree_effect_svd(prior = prior,
                                              dimnames_term = dimnames_forecast,
                                              var_time = var_time,
                                              var_age = var_age,
                                              var_sexgender = var_sexgender)
  ## tricky combining rvecs with sparse matrices
  effect_forecast <- matrix %*% as.matrix(svd_forecast) + offset
  effect_forecast <- rvec::rvec_dbl(as.matrix(effect_forecast))
  nm <- dimnames_to_nm(dimnames_term)
  levels_effect <- dimnames_to_levels(dimnames_forecast)
  levels_svd <- make_levels_svd_term(prior = prior,
                                     dimnames_term = dimnames_forecast,
                                     var_time = var_time,
                                     var_age = var_age,
                                     var_sexgender = var_sexgender)
  svd <- tibble::tibble(term = nm,
                        component = "svd",
                        level = levels_svd,
                        .fitted = svd_forecast)
  effect <- tibble::tibble(term = nm,
                           component = "effect",
                           level = levels_effect,
                           .fitted = effect_forecast)
  vctrs::vec_rbind(effect, svd)
}

## HAS_TESTS
#' @export
forecast_term.bage_prior_svd_rw2zero <- function(prior,
                                                 dimnames_term,
                                                 var_time,
                                                 var_age,
                                                 var_sexgender,
                                                 components,
                                                 labels_forecast) {
  con <- prior$specific$con
  dimnames_forecast <- replace(dimnames_term, var_time, list(labels_forecast))
  svd_forecast <- forecast_rw2_svd(prior = prior,
                                   dimnames_term = dimnames_term,
                                   dimnames_forecast = dimnames_forecast,
                                   var_time = var_time,
                                   var_age = var_age,
                                   var_sexgender = var_sexgender,
                                   components = components,
                                   labels_forecast)
  matrix <- make_matrix_effectfree_effect_inner(prior = prior,
                                                dimnames_term = dimnames_forecast,
                                                var_time = var_time,
                                                var_age = var_age,
                                                var_sexgender = var_sexgender,
                                                append_zero = FALSE,
                                                con = con)
  offset <- make_offset_effectfree_effect_svd(prior = prior,
                                              dimnames_term = dimnames_forecast,
                                              var_time = var_time,
                                              var_age = var_age,
                                              var_sexgender = var_sexgender)
  ## tricky combining rvecs with sparse matrices
  effect_forecast <- matrix %*% as.matrix(svd_forecast) + offset
  effect_forecast <- rvec::rvec_dbl(as.matrix(effect_forecast))
  nm <- dimnames_to_nm(dimnames_term)
  levels_effect <- dimnames_to_levels(dimnames_forecast)
  levels_svd <- make_levels_svd_term(prior = prior,
                                     dimnames_term = dimnames_forecast,
                                     var_time = var_time,
                                     var_age = var_age,
                                     var_sexgender = var_sexgender)
  svd <- tibble::tibble(term = nm,
                        component = "svd",
                        level = levels_svd,
                        .fitted = svd_forecast)
  effect <- tibble::tibble(term = nm,
                           component = "effect",
                           level = levels_effect,
                           .fitted = effect_forecast)
  vctrs::vec_rbind(effect, svd)
}


## 'generate' -----------------------------------------------------------------

#' @importFrom generics generate
#' @export
generics::generate

## HAS_TESTS
#' Generate Values from Priors
#'
#' Generate draws from priors for model terms.
#'
#' Some priors distinguish between 'along' and 'by'
#' dimensions, while others do not: see [priors]
#' for a complete list. Arguments `n_along` and `n_by`
#' are used with priors that make the distinction,
#' and argument `n_element` is used with priors that do not.
#'
#' @param x Object of class `"bage_prior"`
#' @param n_element Number of elements in term,
#' in priors that do not distinguish
#' 'along' and 'by' dimensions.
#' Default is `20`.
#' @param n_along Number of elements of
#' 'along' dimension. Default is `20`.
#' @param n_by Number of combinations of
#' 'by' variables. Default is `1`.
#' @param n_draw Number of draws. Default
#' is `25`.
#' @param ... Unused. Included for generic consistency only.
#'
#' @returns A [tibble][tibble::tibble()]
#'
#' @seealso
#' - [priors] Overview of priors implemented in **bage**
#'
#' @examples
#' ## prior that distinguishes 'along' and 'by'
#' x <- RW()
#' generate(x, n_along = 10, n_by = 2)
#'
#' ## prior that does not distinguish
#' x <- N()
#' generate(x, n_element = 20)
#'
#' ## SVD_AR(), SVD_RW(), and SVD_RW2()
#' ## distinguish 'along' and 'by'
#' x <- SVD_AR(HFD)
#' generate(x, n_along = 5, n_by = 2)
#'
#' ## SVD() does not
#' x <- SVD(HFD)
#' generate(x, n_element = 10)
#' @export
generate.bage_prior_ar <- function(x,
                                   n_along = 20,
                                   n_by = 1,
                                   n_draw = 25,
                                   ...) {
  check_has_no_dots(...)
  con <- x$specific$con
  l <- generate_prior_helper(x = x,
                             n_along = n_along,
                             n_by = n_by,
                             n_draw = n_draw)
  ans <- l$ans
  matrix_along_by <- l$matrix_along_by
  levels_effect <- l$levels_efect
  coef <- draw_vals_coef(prior = x, n_sim = n_draw)
  sd <- draw_vals_sd(prior = x, n_sim = n_draw)
  value <- draw_vals_ar(coef = coef,
                        sd = sd,
                        matrix_along_by = matrix_along_by,
                        levels_effect = levels_effect)
  if (con == "by") {
    m <- make_matrix_con_by(i_along = 1L, dim = c(n_along, n_by))
    value <- m %*% value
  }
  ans$value <- as.double(value)
  ans
}

## HAS_TESTS
#' @rdname generate.bage_prior_ar
#' @export
generate.bage_prior_known <- function(x,
                                      n_element = 20,
                                      n_draw = 25,
                                      ...) {
  if (!isTRUE(all.equal(n_element, 20))) {
    str <- str_nm_prior(x)
    cli::cli_alert("Non-default value of {.arg n_element} ignored with {.val {str}} prior.")
  }
  check_has_no_dots(...)
  value <- x$specific$values
  n_element <- length(value)
  l <- generate_prior_helper(x = x,
                             n_element = n_element,
                             n_draw = n_draw)
  value <- rep(value, times = n_draw)
  ans <- l$ans
  ans$value <- value
  ans
}

## HAS_TESTS
#' @rdname generate.bage_prior_ar
#' @export
generate.bage_prior_lin <- function(x,
                                    n_along = 20,
                                    n_by = 1,
                                    n_draw = 25,
                                    ...) {
  check_has_no_dots(...)
  mean_slope <- x$specific$mean_slope
  sd_slope <- x$specific$sd_slope
  con <- x$specific$con
  l <- generate_prior_helper(x = x,
                             n_along = n_along,
                             n_by = n_by,
                             n_draw = n_draw)
  ans <- l$ans
  matrix_along_by <- l$matrix_along_by
  levels_effect <- l$levels_effect
  slope <- draw_vals_slope(mean_slope = mean_slope,
                           sd_slope = sd_slope,
                           matrix_along_by = matrix_along_by,
                           n_sim = n_draw)
  sd <- draw_vals_sd(prior = x, n_sim = n_draw)
  value <- draw_vals_lin(slope = slope,
                         sd = sd,
                         matrix_along_by = matrix_along_by,
                         labels = levels_effect)
  if (con == "by") {
    m <- make_matrix_con_by(i_along = 1L, dim = c(n_along, n_by))
    value <- m %*% value
  }
  ans$value <- as.double(value)
  ans
}


## HAS_TESTS
#' @rdname generate.bage_prior_ar
#' @export
generate.bage_prior_linar <- function(x,
                                      n_along = 20,
                                      n_by = 1,
                                      n_draw = 25,
                                      ...) {
  check_has_no_dots(...)
  mean_slope <- x$specific$mean_slope
  sd_slope <- x$specific$sd_slope
  con <- x$specific$con
  l <- generate_prior_helper(x = x,
                             n_along = n_along,
                             n_by = n_by,
                             n_draw = n_draw)
  ans <- l$ans
  matrix_along_by <- l$matrix_along_by
  levels_effect <- l$levels_effect
  slope <- draw_vals_slope(mean_slope = mean_slope,
                           sd_slope = sd_slope,
                           matrix_along_by = matrix_along_by,
                           n_sim = n_draw)
  sd <- draw_vals_sd(prior = x, n_sim = n_draw)
  coef <- draw_vals_coef(prior = x, n_sim = n_draw)
  value <- draw_vals_linar(slope = slope,
                           sd = sd,
                           coef = coef,
                           matrix_along_by = matrix_along_by,
                           labels = levels_effect)
  if (con == "by") {
    m <- make_matrix_con_by(i_along = 1L, dim = c(n_along, n_by))
    value <- m %*% value
  }
  ans$value <- as.double(value)
  ans
}


## HAS_TESTS
#' @rdname generate.bage_prior_ar
#' @export
generate.bage_prior_linex <- function(x,
                                      n_along = 20,
                                      n_by = 1,
                                      n_draw = 25,
                                      ...) {
  check_has_no_dots(...)
  mean_slope <- x$specific$mean_slope
  sd_slope <- x$specific$sd_slope
  con <- x$specific$con
  l <- generate_prior_helper(x = x,
                             n_along = n_along,
                             n_by = n_by,
                             n_draw = n_draw)
  ans <- l$ans
  matrix_along_by <- l$matrix_along_by
  levels_effect <- l$levels_effect
  slope <- draw_vals_slope(mean_slope = mean_slope,
                           sd_slope = sd_slope,
                           matrix_along_by = matrix_along_by,
                           n_sim = n_draw)
  value <- draw_vals_lintrend(slope = slope,
                              matrix_along_by = matrix_along_by,
                              labels = levels_effect)
  if (con == "by") {
    m <- make_matrix_con_by(i_along = 1L, dim = c(n_along, n_by))
    value <- m %*% value
  }
  value <- as.double(value)
  ans$value <- value
  ans
}

## HAS_TESTS
#' @rdname generate.bage_prior_ar
#' @export
generate.bage_prior_norm <- function(x,
                                     n_element = 20,
                                     n_draw = 25,
                                     ...) {
  check_has_no_dots(...)
  l <- generate_prior_helper(x = x,
                             n_element = n_element,
                             n_draw = n_draw)
  ans <- l$ans
  sd <- draw_vals_sd(prior = x, n_sim = n_draw)
  sd <- rep(sd, each = n_element)
  value <- stats::rnorm(n = n_element * n_draw, sd = sd)
  ans$value <- value
  ans
}

## HAS_TESTS
#' @rdname generate.bage_prior_ar
#' @export
generate.bage_prior_normfixed <- function(x,
                                          n_element = 20,
                                          n_draw = 25,
                                          ...) {
  check_has_no_dots(...)
  l <- generate_prior_helper(x = x,
                             n_element = n_element,
                             n_draw = n_draw)
  ans <- l$ans
  sd <- x$specific$sd
  value <- stats::rnorm(n = n_element * n_draw, sd = sd)
  ans$value <- value
  ans
}

## HAS_TESTS
#' @rdname generate.bage_prior_ar
#' @export
generate.bage_prior_rwrandom <- function(x,
                                         n_along = 20,
                                         n_by = 1,
                                         n_draw = 25,
                                         ...) {
  check_has_no_dots(...)
  sd_init <- x$specific$sd
  con <- x$specific$con
  l <- generate_prior_helper(x = x,
                             n_along = n_along,
                             n_by = n_by,
                             n_draw = n_draw)
  ans <- l$ans
  matrix_along_by <- l$matrix_along_by
  levels_effect <- l$levels_effect
  sd_innov <- draw_vals_sd(prior = x, n_sim = n_draw)
  value <- draw_vals_rw(sd = sd_innov,
                        sd_init = sd_init,
                        matrix_along_by = matrix_along_by,
                        levels_effect = levels_effect)
  if (con == "by") {
    m <- make_matrix_con_by(i_along = 1L, dim = c(n_along, n_by))
    value <- m %*% value
  }
  value <- as.double(value)
  ans$value <- value
  ans
}

## HAS_TESTS
#' @rdname generate.bage_prior_ar
#' @export
generate.bage_prior_rwrandomseasfix <- function(x,
                                                n_along = 20,
                                                n_by = 1,
                                                n_draw = 25,
                                                ...) {
  check_has_no_dots(...)
  n_seas <- x$specific$n_seas
  sd_init <- x$specific$sd
  sd_init_seas <- x$specific$sd_seas
  con <- x$specific$con
  l <- generate_prior_helper(x = x,
                             n_along = n_along,
                             n_by = n_by,
                             n_draw = n_draw)
  ans <- l$ans
  matrix_along_by <- l$matrix_along_by
  levels_effect <- l$levels_effect
  sd_innov <- draw_vals_sd(prior = x, n_sim = n_draw)
  alpha <- draw_vals_rw(sd = sd_innov,
                        sd_init = sd_init,
                        matrix_along_by = matrix_along_by,
                        levels_effect = levels_effect)
  seas <- draw_vals_seasfix(n_seas = n_seas,
                            sd_init = sd_init_seas,
                            matrix_along_by = matrix_along_by,
                            n_sim = n_draw)
  value <- alpha + seas
  if (con == "by") {
    m <- make_matrix_con_by(i_along = 1L, dim = c(n_along, n_by))
    value <- m %*% value
  }
  value <- as.double(value)
  ans$value <- value
  ans
}

## HAS_TESTS
#' @rdname generate.bage_prior_ar
#' @export
generate.bage_prior_rwrandomseasvary <- function(x,
                                                 n_along = 20,
                                                 n_by = 1,
                                                 n_draw = 25,
                                                 ...) {
  check_has_no_dots(...)
  n_seas <- x$specific$n_seas
  sd_init <- x$specific$sd
  sd_init_seas <- x$specific$sd_seas
  con <- x$specific$con
  l <- generate_prior_helper(x = x,
                             n_along = n_along,
                             n_by = n_by,
                             n_draw = n_draw)
  ans <- l$ans
  matrix_along_by <- l$matrix_along_by
  levels_effect <- l$levels_effect
  sd_innov <- draw_vals_sd(prior = x, n_sim = n_draw)
  sd_innov_seas <- draw_vals_sd_seas(prior = x, n_sim = n_draw)
  alpha <- draw_vals_rw(sd = sd_innov,
                        sd_init = sd_init,
                        matrix_along_by = matrix_along_by,
                        levels_effect = levels_effect)
  seas <- draw_vals_seasvary(n_seas = n_seas,
                             sd_init = sd_init_seas,
                             sd_innov = sd_innov_seas,
                             matrix_along_by = matrix_along_by)
  value <- alpha + seas
  if (con == "by") {
    m <- make_matrix_con_by(i_along = 1L, dim = c(n_along, n_by))
    value <- m %*% value
  }
  value <- as.double(value)
  ans$value <- value
  ans
}

## HAS_TESTS
#' @rdname generate.bage_prior_ar
#' @export
generate.bage_prior_rwzero <- function(x,
                                       n_along = 20,
                                       n_by = 1,
                                       n_draw = 25,
                                       ...) {
  check_has_no_dots(...)
  con <- x$specific$con
  l <- generate_prior_helper(x = x,
                             n_along = n_along,
                             n_by = n_by,
                             n_draw = n_draw)
  ans <- l$ans
  matrix_along_by <- l$matrix_along_by
  levels_effect <- l$levels_effect
  sd_innov <- draw_vals_sd(prior = x, n_sim = n_draw)
  value <- draw_vals_rw(sd = sd_innov,
                        sd_init = 0,
                        matrix_along_by = matrix_along_by,
                        levels_effect = levels_effect)
  if (con == "by") {
    m <- make_matrix_con_by(i_along = 1L, dim = c(n_along, n_by))
    value <- m %*% value
  }
  value <- as.double(value)
  ans$value <- value
  ans
}

## HAS_TESTS
#' @rdname generate.bage_prior_ar
#' @export
generate.bage_prior_rwzeroseasfix <- function(x,
                                              n_along = 20,
                                              n_by = 1,
                                              n_draw = 25,
                                              ...) {
  check_has_no_dots(...)
  n_seas <- x$specific$n_seas
  sd_init <- x$specific$sd_seas
  con <- x$specific$con
  l <- generate_prior_helper(x = x,
                             n_along = n_along,
                             n_by = n_by,
                             n_draw = n_draw)
  ans <- l$ans
  matrix_along_by <- l$matrix_along_by
  levels_effect <- l$levels_effect
  sd_innov <- draw_vals_sd(prior = x, n_sim = n_draw)
  alpha <- draw_vals_rw(sd = sd_innov,
                        sd_init = 0,
                        matrix_along_by = matrix_along_by,
                        levels_effect = levels_effect)
  seas <- draw_vals_seasfix(n_seas = n_seas,
                            sd_init = sd_init,
                            matrix_along_by = matrix_along_by,
                            n_sim = n_draw)
  value <- alpha + seas
  if (con == "by") {
    m <- make_matrix_con_by(i_along = 1L, dim = c(n_along, n_by))
    value <- m %*% value
  }
  value <- as.double(value)
  ans$value <- value
  ans
}

## HAS_TESTS
#' @rdname generate.bage_prior_ar
#' @export
generate.bage_prior_rwzeroseasvary <- function(x,
                                               n_along = 20,
                                               n_by = 1,
                                               n_draw = 25,
                                               ...) {
  check_has_no_dots(...)
  n_seas <- x$specific$n_seas
  sd_init_seas <- x$specific$sd_seas
  con <- x$specific$con
  l <- generate_prior_helper(x = x,
                             n_along = n_along,
                             n_by = n_by,
                             n_draw = n_draw)
  ans <- l$ans
  matrix_along_by <- l$matrix_along_by
  levels_effect <- l$levels_effect
  sd_innov <- draw_vals_sd(prior = x, n_sim = n_draw)
  sd_innov_seas <- draw_vals_sd_seas(prior = x, n_sim = n_draw)
  alpha <- draw_vals_rw(sd = sd_innov,
                        sd_init = 0,
                        matrix_along_by = matrix_along_by,
                        levels_effect = levels_effect)
  seas <- draw_vals_seasvary(n_seas = n_seas,
                             sd_init = sd_init_seas,
                             sd_innov = sd_innov_seas,
                             matrix_along_by = matrix_along_by)
  value <- alpha + seas
  if (con == "by") {
    m <- make_matrix_con_by(i_along = 1L, dim = c(n_along, n_by))
    value <- m %*% value
  }
  value <- as.double(value)
  ans$value <- value
  ans
}

## HAS_TESTS
#' @rdname generate.bage_prior_ar
#' @export
generate.bage_prior_rw2random <- function(x,
                                          n_along = 20,
                                          n_by = 1,
                                          n_draw = 25,
                                          ...) {
  check_has_no_dots(...)
  con <- x$specific$con
  sd_init <- x$specific$sd
  sd_slope <- x$specific$sd_slope
  l <- generate_prior_helper(x = x,
                             n_along = n_along,
                             n_by = n_by,
                             n_draw = n_draw)
  ans <- l$ans
  matrix_along_by <- l$matrix_along_by
  levels_effect <- l$levels_effect
  sd <- draw_vals_sd(prior = x, n_sim = n_draw)
  value <- draw_vals_rw2(sd = sd,
                         sd_init = sd_init,
                         sd_slope = sd_slope,
                         matrix_along_by = matrix_along_by,
                         levels_effect = levels_effect)
  if (con == "by") {
    m <- make_matrix_con_by(i_along = 1L, dim = c(n_along, n_by))
    value <- m %*% value
  }
  value <- as.double(value)
  ans$value <- value
  ans
}

## HAS_TESTS
#' @rdname generate.bage_prior_ar
#' @export
generate.bage_prior_rw2randomseasfix <- function(x,
                                                 n_along = 20,
                                                 n_by = 1,
                                                 n_draw = 25,
                                                 ...) {
  check_has_no_dots(...)
  n_seas <- x$specific$n_seas
  sd_init <- x$specific$sd
  sd_slope <- x$specific$sd_slope
  sd_init_seas <- x$specific$sd_seas
  con <- x$specific$con
  l <- generate_prior_helper(x = x,
                             n_along = n_along,
                             n_by = n_by,
                             n_draw = n_draw)
  ans <- l$ans
  matrix_along_by <- l$matrix_along_by
  levels_effect <- l$levels_effect
  sd_innov <- draw_vals_sd(prior = x, n_sim = n_draw)
  alpha <- draw_vals_rw2(sd = sd_innov,
                         sd_init = sd_init,
                         sd_slope = sd_slope,
                         matrix_along_by = matrix_along_by,
                         levels_effect = levels_effect)
  seas <- draw_vals_seasfix(n_seas = n_seas,
                            sd_init = sd_init_seas,
                            matrix_along_by = matrix_along_by,
                            n_sim = n_draw)
  value <- alpha + seas
  if (con == "by") {
    m <- make_matrix_con_by(i_along = 1L, dim = c(n_along, n_by))
    value <- m %*% value
  }
  value <- as.double(value)
  ans$value <- value
  ans
}

## HAS_TESTS
#' @rdname generate.bage_prior_ar
#' @export
generate.bage_prior_rw2randomseasvary <- function(x,
                                                  n_along = 20,
                                                  n_by = 1,
                                                  n_draw = 25,
                                                  ...) {
  check_has_no_dots(...)
  n_seas <- x$specific$n_seas
  sd_init <- x$specific$sd
  sd_slope <- x$specific$sd_slope
  sd_init_seas <- x$specific$sd_seas
  con <- x$specific$con
  l <- generate_prior_helper(x = x,
                             n_along = n_along,
                             n_by = n_by,
                             n_draw = n_draw)
  ans <- l$ans
  matrix_along_by <- l$matrix_along_by
  levels_effect <- l$levels_effect
  sd_innov <- draw_vals_sd(prior = x, n_sim = n_draw)
  sd_innov_seas <- draw_vals_sd_seas(prior = x, n_sim = n_draw)
  alpha <- draw_vals_rw2(sd = sd_innov,
                         sd_init = sd_init,
                         sd_slope = sd_slope,
                         matrix_along_by = matrix_along_by,
                         levels_effect = levels_effect)
  seas <- draw_vals_seasvary(n_seas = n_seas,
                             sd_init = sd_init_seas,
                             sd_innov = sd_innov_seas,
                             matrix_along_by = matrix_along_by)
  value <- alpha + seas
  if (con == "by") {
    m <- make_matrix_con_by(i_along = 1L, dim = c(n_along, n_by))
    value <- m %*% value
  }
  value <- as.double(value)
  ans$value <- value
  ans
}

## HAS_TESTS
#' @rdname generate.bage_prior_ar
#' @export
generate.bage_prior_rw2zero <- function(x,
                                        n_along = 20,
                                        n_by = 1,
                                        n_draw = 25,
                                        ...) {
  check_has_no_dots(...)
  sd_slope <- x$specific$sd_slope
  con <- x$specific$con
  l <- generate_prior_helper(x = x,
                             n_along = n_along,
                             n_by = n_by,
                             n_draw = n_draw)
  ans <- l$ans
  matrix_along_by <- l$matrix_along_by
  levels_effect <- l$levels_effect
  sd <- draw_vals_sd(prior = x, n_sim = n_draw)
  value <- draw_vals_rw2(sd = sd,
                         sd_init = 0,
                         sd_slope = sd_slope,
                         matrix_along_by = matrix_along_by,
                         levels_effect = levels_effect)
  if (con == "by") {
    m <- make_matrix_con_by(i_along = 1L, dim = c(n_along, n_by))
    value <- m %*% value
  }
  value <- as.double(value)
  ans$value <- value
  ans
}

## HAS_TESTS
#' @rdname generate.bage_prior_ar
#' @export
generate.bage_prior_rw2zeroseasfix <- function(x,
                                               n_along = 20,
                                               n_by = 1,
                                               n_draw = 25,
                                               ...) {
  check_has_no_dots(...)
  n_seas <- x$specific$n_seas
  sd_slope <- x$specific$sd_slope
  sd_init_seas <- x$specific$sd_seas
  con <- x$specific$con
  l <- generate_prior_helper(x = x,
                             n_along = n_along,
                             n_by = n_by,
                             n_draw = n_draw)
  ans <- l$ans
  matrix_along_by <- l$matrix_along_by
  levels_effect <- l$levels_effect
  sd_innov <- draw_vals_sd(prior = x, n_sim = n_draw)
  alpha <- draw_vals_rw2(sd = sd_innov,
                         sd_init = 0,
                         sd_slope = sd_slope,
                         matrix_along_by = matrix_along_by,
                         levels_effect = levels_effect)
  seas <- draw_vals_seasfix(n_seas = n_seas,
                            sd_init = sd_init_seas,
                            matrix_along_by = matrix_along_by,
                            n_sim = n_draw)
  value <- alpha + seas
  if (con == "by") {
    m <- make_matrix_con_by(i_along = 1L, dim = c(n_along, n_by))
    value <- m %*% value
  }
  value <- as.double(value)
  ans$value <- value
  ans
}

## HAS_TESTS
#' @rdname generate.bage_prior_ar
#' @export
generate.bage_prior_rw2zeroseasvary <- function(x,
                                                n_along = 20,
                                                n_by = 1,
                                                n_draw = 25,
                                                ...) {
  check_has_no_dots(...)
  n_seas <- x$specific$n_seas
  sd_slope <- x$specific$sd_slope
  sd_init_seas <- x$specific$sd_seas
  con <- x$specific$con
  l <- generate_prior_helper(x = x,
                             n_along = n_along,
                             n_by = n_by,
                             n_draw = n_draw)
  ans <- l$ans
  matrix_along_by <- l$matrix_along_by
  levels_effect <- l$levels_effect
  sd_innov <- draw_vals_sd(prior = x, n_sim = n_draw)
  sd_innov_seas <- draw_vals_sd_seas(prior = x, n_sim = n_draw)
  alpha <- draw_vals_rw2(sd = sd_innov,
                         sd_slope = sd_slope,
                         sd_init = 0,
                         matrix_along_by = matrix_along_by,
                         levels_effect = levels_effect)
  seas <- draw_vals_seasvary(n_seas = n_seas,
                             sd_init = sd_init_seas,
                             sd_innov = sd_innov_seas,
                             matrix_along_by = matrix_along_by)
  value <- alpha + seas
  if (con == "by") {
    m <- make_matrix_con_by(i_along = 1L, dim = c(n_along, n_by))
    value <- m %*% value
  }
  value <- as.double(value)
  ans$value <- value
  ans
}

## HAS_TESTS
#' @rdname generate.bage_prior_ar
#' @export
generate.bage_prior_spline <- function(x,
                                       n_along = 20,
                                       n_by = 1,
                                       n_draw = 25,
                                       ...) {
  check_has_no_dots(...)
  con <- x$specific$con
  sd_init <- x$specific$sd
  sd_slope <- x$specific$sd_slope
  l <- generate_prior_helper(x = x,
                             n_along = n_along,
                             n_by = n_by,
                             n_draw = n_draw)
  ans <- l$ans
  sd <- draw_vals_sd(prior = x, n_sim = n_draw)
  n_comp <- get_n_comp_spline(prior = x, n_along = n_along)
  matrix_along_by <- matrix(seq_len(n_comp * n_by) - 1L,
                            nrow = n_comp,
                            ncol = n_by)
  levels_spline <- seq_len(n_comp * n_by)
  rw2 <- draw_vals_rw2(sd = sd,
                       sd_init = sd_init,
                       sd_slope = sd_slope,
                       matrix_along_by = matrix_along_by,
                       levels_effect = levels_spline)
  rw2 <- matrix(rw2, nrow = n_comp, ncol = n_by * n_draw)
  m <- make_matrix_spline(n_along = n_along,
                          n_comp = n_comp)
  value <- m %*% rw2
  if (con == "by") {
    value <- matrix(value, nrow = n_along * n_by, ncol = n_draw)
    m <- make_matrix_con_by(i_along = 1L, dim = c(n_along, n_by))
    value <- m %*% value
  }
  value <- as.double(value)
  ans$value <- value
  ans
}

## HAS_TESTS
#' @rdname generate.bage_prior_ar
#' @export
generate.bage_prior_svd <- function(x,
                                    n_element = 1,
                                    n_draw = 25,
                                    ...) {
  check_has_no_dots(...)
  l <- generate_prior_svd_helper(x = x,
                                 n_element = n_element,
                                 n_draw = n_draw)
  ans <- l$ans
  matrix <- l$matrix
  offset <- l$offset
  sd <- rep(1, times = n_draw)
  labels <- seq_len(ncol(matrix))
  alpha <- draw_vals_norm(sd = sd, labels = labels)
  value <- matrix %*% alpha + offset
  ans$value <- as.double(value)
  ans
}


## HAS_TESTS
#' @rdname generate.bage_prior_ar
#' @export
generate.bage_prior_svd_ar <- function(x,
                                       n_along = 5,
                                       n_by = 1,
                                       n_draw = 25,
                                       ...) {
  check_has_no_dots(...)
  con <- x$specific$con
  l <- generate_prior_svd_helper(x = x,
                                 n_along = n_along,
                                 n_by = n_by,
                                 n_draw = n_draw)
  ans <- l$ans
  matrix <- l$matrix
  offset <- l$offset
  matrix_along_by <- l$matrix_along_by
  coef <- draw_vals_coef(prior = x, n_sim = n_draw)
  sd <- draw_vals_sd(prior = x, n_sim = n_draw)
  n_level_svd <- length(matrix_along_by)
  levels_effect_svd <- seq_len(n_level_svd)
  alpha <- draw_vals_ar(coef = coef,
                        sd = sd,
                        matrix_along_by = matrix_along_by,
                        levels_effect = levels_effect_svd)
  value <- matrix %*% alpha + offset
  if (con == "by") {
    n_level_agesex <- nrow(matrix)
    n_agesex <- n_level_agesex %/% (n_by * n_along)
    m <- make_matrix_con_by(i_along = 3L, dim = c(n_agesex, n_by, n_along))
    value <- m %*% value
  }
  ans$value <- as.double(value)
  ans
}

## HAS_TESTS
#' @rdname generate.bage_prior_ar
#' @export
generate.bage_prior_svd_rwrandom <- function(x,
                                             n_along = 5,
                                             n_by = 1,
                                             n_draw = 25,
                                             ...) {
  check_has_no_dots(...)
  sd_init <- x$specific$sd
  con <- x$specific$con
  l <- generate_prior_svd_helper(x = x,
                                 n_along = n_along,
                                 n_by = n_by,
                                 n_draw = n_draw)
  ans <- l$ans
  matrix <- l$matrix
  offset <- l$offset
  matrix_along_by <- l$matrix_along_by
  sd_innov <- draw_vals_sd(prior = x, n_sim = n_draw)
  n_level_svd <- length(matrix_along_by)
  levels_effect_svd <- seq_len(n_level_svd)
  alpha <- draw_vals_rw(sd = sd_innov,
                        sd_init = sd_init,
                        matrix_along_by = matrix_along_by,
                        levels_effect = levels_effect_svd)
  value <- matrix %*% alpha + offset
  if (con == "by") {
    n_level_agesex <- nrow(matrix)
    n_agesex <- n_level_agesex %/% (n_by * n_along)
    m <- make_matrix_con_by(i_along = 3L, dim = c(n_agesex, n_by, n_along))
    value <- m %*% value
  }
  ans$value <- as.double(value)
  ans
}

## HAS_TESTS
#' @rdname generate.bage_prior_ar
#' @export
generate.bage_prior_svd_rwzero <- function(x,
                                           n_along = 5,
                                           n_by = 1,
                                           n_draw = 25,
                                           ...) {
  check_has_no_dots(...)
  con <- x$specific$con
  l <- generate_prior_svd_helper(x = x,
                                 n_along = n_along,
                                 n_by = n_by,
                                 n_draw = n_draw)
  ans <- l$ans
  matrix <- l$matrix
  offset <- l$offset
  matrix_along_by <- l$matrix_along_by
  sd_innov <- draw_vals_sd(prior = x, n_sim = n_draw)
  n_level_svd <- length(matrix_along_by)
  levels_effect_svd <- seq_len(n_level_svd)
  alpha <- draw_vals_rw(sd = sd_innov,
                        sd_init = 0,
                        matrix_along_by = matrix_along_by,
                        levels_effect = levels_effect_svd)
  value <- matrix %*% alpha + offset
  if (con == "by") {
    n_level_agesex <- nrow(matrix)
    n_agesex <- n_level_agesex %/% (n_by * n_along)
    m <- make_matrix_con_by(i_along = 3L, dim = c(n_agesex, n_by, n_along))
    value <- m %*% value
  }
  ans$value <- as.double(value)
  ans
}

## HAS_TESTS
#' @rdname generate.bage_prior_ar
#' @export
generate.bage_prior_svd_rw2random <- function(x,
                                              n_along = 5,
                                              n_by = 1,
                                              n_draw = 25,
                                              ...) {
  check_has_no_dots(...)
  sd_init <- x$specific$sd
  sd_slope <- x$specific$sd_slope
  con <- x$specific$con
  l <- generate_prior_svd_helper(x = x,
                                 n_along = n_along,
                                 n_by = n_by,
                                 n_draw = n_draw)
  ans <- l$ans
  matrix <- l$matrix
  offset <- l$offset
  matrix_along_by <- l$matrix_along_by
  sd_innov <- draw_vals_sd(prior = x, n_sim = n_draw)
  n_level_svd <- length(matrix_along_by)
  levels_effect_svd <- seq_len(n_level_svd)
  alpha <- draw_vals_rw2(sd = sd_innov,
                         sd_init = sd_init,
                         sd_slope = sd_slope,
                         matrix_along_by = matrix_along_by,
                         levels_effect = levels_effect_svd)
  value <- matrix %*% alpha + offset
  if (con == "by") {
    n_level_agesex <- nrow(matrix)
    n_agesex <- n_level_agesex %/% (n_by * n_along)
    m <- make_matrix_con_by(i_along = 3L, dim = c(n_agesex, n_by, n_along))
    value <- m %*% value
  }
  ans$value <- as.double(value)
  ans
}

## HAS_TESTS
#' @rdname generate.bage_prior_ar
#' @export
generate.bage_prior_svd_rw2zero <- function(x,
                                            n_along = 5,
                                            n_by = 1,
                                            n_draw = 25,
                                            ...) {
  check_has_no_dots(...)
  sd_slope <- x$specific$sd_slope
  con <- x$specific$con
  l <- generate_prior_svd_helper(x = x,
                                 n_along = n_along,
                                 n_by = n_by,
                                 n_draw = n_draw)
  ans <- l$ans
  matrix <- l$matrix
  offset <- l$offset
  matrix_along_by <- l$matrix_along_by
  sd_innov <- draw_vals_sd(prior = x, n_sim = n_draw)
  n_level_svd <- length(matrix_along_by)
  levels_effect_svd <- seq_len(n_level_svd)
  alpha <- draw_vals_rw2(sd = sd_innov,
                         sd_init = 0,
                         sd_slope = sd_slope,
                         matrix_along_by = matrix_along_by,
                         levels_effect = levels_effect_svd)
  value <- matrix %*% alpha + offset
  if (con == "by") {
    n_level_agesex <- nrow(matrix)
    n_agesex <- n_level_agesex %/% (n_by * n_along)
    m <- make_matrix_con_by(i_along = 3L, dim = c(n_agesex, n_by, n_along))
    value <- m %*% value
  }
  ans$value <- as.double(value)
  ans
}


## 'has_hyperrandfree' ------------------------------------------------------------

#' Has Hyper-Parameters that can be Treated as Random Effects
#'
#' @param prior Object of class 'bage_prior'
#'
#' @returns TRUE or FALSE.
#'
#' @noRd
has_hyperrandfree <- function(prior) {
  UseMethod("has_hyperrandfree")
}

## HAS_TESTS
#' @export
has_hyperrandfree.bage_prior <- function(prior) FALSE


## HAS_TESTS
#' @export
has_hyperrandfree.bage_prior_lin <- function(prior) TRUE

## HAS_TESTS
#' @export
has_hyperrandfree.bage_prior_linar <- function(prior) TRUE

## HAS_TESTS
#' @export
has_hyperrandfree.bage_prior_rwrandomseasfix <- function(prior) TRUE

## HAS_TESTS
#' @export
has_hyperrandfree.bage_prior_rwrandomseasvary <- function(prior) TRUE

## HAS_TESTS
#' @export
has_hyperrandfree.bage_prior_rwzeroseasfix <- function(prior) TRUE

## HAS_TESTS
#' @export
has_hyperrandfree.bage_prior_rwzeroseasvary <- function(prior) TRUE

## HAS_TESTS
#' @export
has_hyperrandfree.bage_prior_rw2randomseasfix <- function(prior) TRUE

## HAS_TESTS
#' @export
has_hyperrandfree.bage_prior_rw2randomseasvary <- function(prior) TRUE

## HAS_TESTS
#' @export
has_hyperrandfree.bage_prior_rw2zeroseasfix <- function(prior) TRUE

## HAS_TESTS
#' @export
has_hyperrandfree.bage_prior_rw2zeroseasvary <- function(prior) TRUE


## 'infer_trend_cyc_seas_err_forecast_one' ---------------------------------------------------

#' Derive Parts of 'Components' Output Dealing with a
#' Prior that has Hyper-Parameters Treated as Random Effects - Forecasts
#'
#' In all priors with 'hyperrand' elements, the reformatting involves
#' renaming columns. 
#'
#' @param prior Object of class 'bage_prior'.
#' @param dimnames_term Dimnames for array representation of term
#' @param var_time Name of time variable
#' @param var_age Name of age variable
#' @param matrix_along_by Matrix with mapping for along, by dimensions
#' @param components A data frame.
#'
#' @returns A modifed version of 'components'
#'
#' @noRd
infer_trend_cyc_seas_err_forecast_one <- function(prior,
                                                  dimnames_term,
                                                  var_time,
                                                  var_age,
                                                  components) {
  UseMethod("infer_trend_cyc_seas_err_forecast_one")
}

## HAS_TESTS
#' @export
infer_trend_cyc_seas_err_forecast_one.bage_prior <- function(prior,
                                                             dimnames_term,
                                                             var_time,
                                                             var_age,
                                                             components)
  components

## HAS_TESTS
#' @export
infer_trend_cyc_seas_err_forecast_one.bage_prior_linar <- function(prior,
                                                                   dimnames_term,
                                                                   var_time,
                                                                   var_age,
                                                                   components) {
  nm <- dimnames_to_nm(dimnames_term)
  is_effect <- with(components, (term == nm) & (component == "effect"))
  is_trend <- with(components, (term == nm) & (component == "trend"))
  is_error <- with(components, (term == nm) & (component == "error"))
  effect <- components$.fitted[is_effect]
  trend <- components$.fitted[is_trend]
  error <- effect - trend
  components$.fitted[is_error] <- error
  components
}

## HAS_TESTS
#' @export
infer_trend_cyc_seas_err_forecast_one.bage_prior_rwrandomseasfix <- function(prior,
                                                                             dimnames_term,
                                                                             var_time,
                                                                             var_age,
                                                                             components)
  infer_trend_cyc_seas_err_seasfix_forecast(prior = prior,
                                            dimnames_term = dimnames_term,
                                            var_time = var_time,
                                            var_age = var_age,
                                            components = components)

## HAS_TESTS
#' @export
infer_trend_cyc_seas_err_forecast_one.bage_prior_rwrandomseasvary <- function(prior,
                                                                              dimnames_term,
                                                                              var_time,
                                                                              var_age,
                                                                              components)
  infer_trend_cyc_seas_err_seasvary_forecast(prior = prior,
                                             dimnames_term = dimnames_term,
                                             var_time = var_time,
                                             var_age = var_age,
                                             components = components)
## HAS_TESTS
#' @export
infer_trend_cyc_seas_err_forecast_one.bage_prior_rwzeroseasfix <- function(prior,
                                                                           dimnames_term,
                                                                           var_time,
                                                                           var_age,
                                                                           components)
  infer_trend_cyc_seas_err_seasfix_forecast(prior = prior,
                                            dimnames_term = dimnames_term,
                                            var_time = var_time,
                                            var_age = var_age,
                                            components = components)

## HAS_TESTS
#' @export
infer_trend_cyc_seas_err_forecast_one.bage_prior_rwzeroseasvary <- function(prior,
                                                                            dimnames_term,
                                                                            var_time,
                                                                            var_age,
                                                                            components)
  infer_trend_cyc_seas_err_seasvary_forecast(prior = prior,
                                             dimnames_term = dimnames_term,
                                             var_time = var_time,
                                             var_age = var_age,
                                             components = components)

## HAS_TESTS
#' @export
infer_trend_cyc_seas_err_forecast_one.bage_prior_rw2randomseasfix <- function(prior,
                                                                              dimnames_term,
                                                                              var_time,
                                                                              var_age,
                                                                              components)
  infer_trend_cyc_seas_err_seasfix_forecast(prior = prior,
                                            dimnames_term = dimnames_term,
                                            var_time = var_time,
                                            var_age = var_age,
                                            components = components)

## HAS_TESTS
#' @export
infer_trend_cyc_seas_err_forecast_one.bage_prior_rw2randomseasvary <- function(prior,
                                                                               dimnames_term,
                                                                               var_time,
                                                                               var_age,
                                                                               components)
  infer_trend_cyc_seas_err_seasvary_forecast(prior = prior,
                                             dimnames_term = dimnames_term,
                                             var_time = var_time,
                                             var_age = var_age,
                                             components = components)

## HAS_TESTS
#' @export
infer_trend_cyc_seas_err_forecast_one.bage_prior_rw2zeroseasfix <- function(prior,
                                                                            dimnames_term,
                                                                            var_time,
                                                                            var_age,
                                                                            components)
  infer_trend_cyc_seas_err_seasfix_forecast(prior = prior,
                                            dimnames_term = dimnames_term,
                                            var_time = var_time,
                                            var_age = var_age,
                                            components = components)

## HAS_TESTS
#' @export
infer_trend_cyc_seas_err_forecast_one.bage_prior_rw2zeroseasvary <- function(prior,
                                                                             dimnames_term,
                                                                             var_time,
                                                                             var_age,
                                                                             components)
  infer_trend_cyc_seas_err_seasvary_forecast(prior = prior,
                                             dimnames_term = dimnames_term,
                                             var_time = var_time,
                                             var_age = var_age,
                                             components = components)


## 'is_known' -----------------------------------------------------------------

#' Test whether a prior treats an intercept,
#' main effect, or interaction as known and fixed
#'
#' @param prior An object of class 'bage_prior'.
#'
#' @returns TRUE or FALSE
#'
#' @noRd
is_known <- function(prior) {
  UseMethod("is_known")
}

## HAS_TESTS
#' @export
is_known.bage_prior <- function(prior) FALSE

## HAS_TESTS
#' @export
is_known.bage_prior_known <- function(prior) TRUE


## 'is_prior_ok_for_term' -----------------------------------------------------

#' Test whether a prior can be used with
#' a particular effect or interaction
#'
#' @param prior Object of class 'bage_prior'
#' @param dimnames_term Dimnames for array representation of term
#' @param var_time Name of time variable
#' @param var_age Name of age variable
#' @param var_sexgender Name of sex/gender variable
#'
#' @returns TRUE or raises an error
#'
#' @noRd
is_prior_ok_for_term <- function(prior,
                                 dimnames_term,
                                 var_time,
                                 var_age,
                                 var_sexgender) {
  UseMethod("is_prior_ok_for_term")
}

## HAS_TESTS
#' @export
is_prior_ok_for_term.bage_prior_ar <- function(prior,
                                               dimnames_term,
                                               var_time,
                                               var_age,
                                               var_sexgender) {
  n_coef <- prior$specific$n_coef
  con <- prior$specific$con
  nm <- dimnames_to_nm(dimnames_term)
  matrix_along_by_effect <- make_matrix_along_by_effect(prior = prior,
                                                        dimnames_term = dimnames_term,
                                                        var_time = var_time,
                                                        var_age = var_age)
  n_along <- nrow(matrix_along_by_effect)
  n_by <- ncol(matrix_along_by_effect)
  check_n_along_ge(n_along = n_along,
                   min = n_coef + 1L,
                   nm = nm,
                   prior = prior)
  check_con_n_by(con = con,
                 n_by = n_by,
                 nm = nm)
  invisible(TRUE)
}

## HAS_TESTS
#' @export
is_prior_ok_for_term.bage_prior_known <- function(prior,
                                                  dimnames_term,
                                                  var_time,
                                                  var_age,
                                                  var_sexgender) {
  values <- prior$specific$values
  nm <- dimnames_to_nm(dimnames_term)
  n_values <- length(values)
  length_effect <- prod(lengths(dimnames_term))
  if (n_values != length_effect) {
    str <- str_call_prior(prior)
    cli::cli_abort(c("{.var {str}} prior for {.var {nm}} term invalid.",
                     i = "Prior specifies {n_values} element{?s}.",
                     i = "{.var {nm}} has {length_effect} element{?s}."))
  }
  invisible(TRUE)
}

## HAS_TESTS
#' @export
is_prior_ok_for_term.bage_prior_lin <- function(prior,
                                                dimnames_term,
                                                var_time,
                                                var_age,
                                                var_sexgender) {
  con <- prior$specific$con  
  nm <- dimnames_to_nm(dimnames_term)
  matrix_along_by_effect <- make_matrix_along_by_effect(prior = prior,
                                                        dimnames_term = dimnames_term,
                                                        var_time = var_time,
                                                        var_age = var_age)
  n_along <- nrow(matrix_along_by_effect)
  n_by <- ncol(matrix_along_by_effect)
  check_n_along_ge(n_along = n_along,
                   min = 2L,
                   nm = nm,
                   prior = prior)
  check_con_n_by(con = con,
                 n_by = n_by,
                 nm = nm)
  invisible(TRUE)
}

## HAS_TESTS
#' @export
is_prior_ok_for_term.bage_prior_linar <- function(prior,
                                                  dimnames_term,
                                                  var_time,
                                                  var_age,
                                                  var_sexgender) {
  con <- prior$specific$con  
  n_coef <- prior$specific$n_coef
  nm <- dimnames_to_nm(dimnames_term)
  matrix_along_by_effect <- make_matrix_along_by_effect(prior = prior,
                                                        dimnames_term = dimnames_term,
                                                        var_time = var_time,
                                                        var_age = var_age)
  n_along <- nrow(matrix_along_by_effect)
  n_by <- ncol(matrix_along_by_effect)
  check_n_along_ge(n_along = n_along,
                   min = n_coef + 1L,
                   nm = nm,
                   prior = prior)
  check_con_n_by(con = con,
                 n_by = n_by,
                 nm = nm)
  invisible(TRUE)
}

## HAS_TESTS
#' @export
is_prior_ok_for_term.bage_prior_linex <- function(prior,
                                                  dimnames_term,
                                                  var_time,
                                                  var_age,
                                                  var_sexgender) {
  con <- prior$specific$con  
  nm <- dimnames_to_nm(dimnames_term)
  matrix_along_by_effect <- make_matrix_along_by_effect(prior = prior,
                                                        dimnames_term = dimnames_term,
                                                        var_time = var_time,
                                                        var_age = var_age)
  n_along <- nrow(matrix_along_by_effect)
  n_by <- ncol(matrix_along_by_effect)
  check_n_along_ge(n_along = n_along,
                   min = 2L,
                   nm = nm,
                   prior = prior)
  check_con_n_by(con = con,
                 n_by = n_by,
                 nm = nm)
  invisible(TRUE)
}

## HAS_TESTS
#' @export
is_prior_ok_for_term.bage_prior_norm <- function(prior,
                                                 dimnames_term,
                                                 var_time,
                                                 var_age,
                                                 var_sexgender) {
  nm <- dimnames_to_nm(dimnames_term)
  length_effect <- prod(lengths(dimnames_term))
  check_length_effect_ge(length_effect = length_effect,
                         min = 1L,
                         nm = nm,
                         prior = prior)
  invisible(TRUE)
}

## HAS_TESTS
#' @export
is_prior_ok_for_term.bage_prior_normfixed <- function(prior,
                                                      dimnames_term,
                                                      var_time,
                                                      var_age,
                                                      var_sexgender) {
  nm <- dimnames_to_nm(dimnames_term)
  length_effect <- prod(lengths(dimnames_term))
  check_length_effect_ge(length_effect = length_effect,
                         min = 1L,
                         nm = nm,
                         prior = prior)
  invisible(TRUE)
}

## HAS_TESTS
#' @export
is_prior_ok_for_term.bage_prior_rwrandom <- function(prior,
                                               dimnames_term,
                                               var_time,
                                               var_age,
                                               var_sexgender) {
  con <- prior$specific$con  
  nm <- dimnames_to_nm(dimnames_term)
  matrix_along_by_effect <- make_matrix_along_by_effect(prior = prior,
                                                        dimnames_term = dimnames_term,
                                                        var_time = var_time,
                                                        var_age = var_age)
  n_along <- nrow(matrix_along_by_effect)
  n_by <- ncol(matrix_along_by_effect)
  check_n_along_ge(n_along = n_along,
                   min = 3L,
                   nm = nm,
                   prior = prior)
  check_con_n_by(con = con,
                 n_by = n_by,
                 nm = nm)
  invisible(TRUE)
}

## HAS_TESTS
#' @export
is_prior_ok_for_term.bage_prior_rwrandomseasfix <- function(prior,
                                                            dimnames_term,
                                                            var_time,
                                                            var_age,
                                                            var_sexgender) {
  n_seas <- prior$specific$n_seas
  con <- prior$specific$con  
  nm <- dimnames_to_nm(dimnames_term)
  matrix_along_by_effect <- make_matrix_along_by_effect(prior = prior,
                                                        dimnames_term = dimnames_term,
                                                        var_time = var_time,
                                                        var_age = var_age)
  n_along <- nrow(matrix_along_by_effect)
  n_by <- ncol(matrix_along_by_effect)
  check_n_along_ge(n_along = n_along,
                   min = max(n_seas, 3L),
                   nm = nm,
                   prior = prior)
  check_con_n_by(con = con,
                 n_by = n_by,
                 nm = nm)
  invisible(TRUE)
}

## HAS_TESTS
#' @export
is_prior_ok_for_term.bage_prior_rwrandomseasvary <- function(prior,
                                                             dimnames_term,
                                                             var_time,
                                                             var_age,
                                                             var_sexgender) {
  n_seas <- prior$specific$n_seas
  con <- prior$specific$con  
  nm <- dimnames_to_nm(dimnames_term)
  matrix_along_by_effect <- make_matrix_along_by_effect(prior = prior,
                                                        dimnames_term = dimnames_term,
                                                        var_time = var_time,
                                                        var_age = var_age)
  n_along <- nrow(matrix_along_by_effect)
  n_by <- ncol(matrix_along_by_effect)
  check_n_along_ge(n_along = n_along,
                   min = max(n_seas, 3L),
                   nm = nm,
                   prior = prior)
  check_con_n_by(con = con,
                 n_by = n_by,
                 nm = nm)
  invisible(TRUE)
}

## HAS_TESTS
#' @export
is_prior_ok_for_term.bage_prior_rwzero <- function(prior,
                                               dimnames_term,
                                               var_time,
                                               var_age,
                                               var_sexgender) {
  con <- prior$specific$con  
  nm <- dimnames_to_nm(dimnames_term)
  matrix_along_by_effect <- make_matrix_along_by_effect(prior = prior,
                                                        dimnames_term = dimnames_term,
                                                        var_time = var_time,
                                                        var_age = var_age)
  n_along <- nrow(matrix_along_by_effect)
  n_by <- ncol(matrix_along_by_effect)
  check_n_along_ge(n_along = n_along,
                   min = 3L,
                   nm = nm,
                   prior = prior)
  check_con_n_by(con = con,
                 n_by = n_by,
                 nm = nm)
  invisible(TRUE)
}

## HAS_TESTS
#' @export
is_prior_ok_for_term.bage_prior_rwzeroseasfix <- function(prior,
                                                            dimnames_term,
                                                            var_time,
                                                            var_age,
                                                            var_sexgender) {
  n_seas <- prior$specific$n_seas
  con <- prior$specific$con  
  nm <- dimnames_to_nm(dimnames_term)
  matrix_along_by_effect <- make_matrix_along_by_effect(prior = prior,
                                                        dimnames_term = dimnames_term,
                                                        var_time = var_time,
                                                        var_age = var_age)
  n_along <- nrow(matrix_along_by_effect)
  n_by <- ncol(matrix_along_by_effect)
  check_n_along_ge(n_along = n_along,
                   min = max(n_seas, 3L),
                   nm = nm,
                   prior = prior)
  check_con_n_by(con = con,
                 n_by = n_by,
                 nm = nm)
  invisible(TRUE)
}

## HAS_TESTS
#' @export
is_prior_ok_for_term.bage_prior_rwzeroseasvary <- function(prior,
                                                             dimnames_term,
                                                             var_time,
                                                             var_age,
                                                             var_sexgender) {
  n_seas <- prior$specific$n_seas
  con <- prior$specific$con  
  nm <- dimnames_to_nm(dimnames_term)
  matrix_along_by_effect <- make_matrix_along_by_effect(prior = prior,
                                                        dimnames_term = dimnames_term,
                                                        var_time = var_time,
                                                        var_age = var_age)
  n_along <- nrow(matrix_along_by_effect)
  n_by <- ncol(matrix_along_by_effect)
  check_n_along_ge(n_along = n_along,
                   min = max(n_seas, 3L),
                   nm = nm,
                   prior = prior)
  check_con_n_by(con = con,
                 n_by = n_by,
                 nm = nm)
  invisible(TRUE)
}

## HAS_TESTS
#' @export
is_prior_ok_for_term.bage_prior_rw2infant <- function(prior,
                                                      dimnames_term,
                                                      var_time,
                                                      var_age,
                                                      var_sexgender) {
  con <- prior$specific$con  
  nm <- dimnames_to_nm(dimnames_term)
  matrix_along_by_effect <- make_matrix_along_by_effect(prior = prior,
                                                        dimnames_term = dimnames_term,
                                                        var_time = var_time,
                                                        var_age = var_age)
  n_along <- nrow(matrix_along_by_effect)
  n_by <- ncol(matrix_along_by_effect)
  check_prior_age(prior = prior,
                  nm = nm,
                  var_age = var_age)
  check_n_along_ge(n_along = n_along,
                   min = 4L,
                   nm = nm,
                   prior = prior)
  check_con_n_by(con = con,
                 n_by = n_by,
                 nm = nm)
  invisible(TRUE)
}

## HAS_TESTS
#' @export
is_prior_ok_for_term.bage_prior_rw2random <- function(prior,
                                                      dimnames_term,
                                                      var_time,
                                                      var_age,
                                                      var_sexgender) {
  con <- prior$specific$con  
  nm <- dimnames_to_nm(dimnames_term)
  matrix_along_by_effect <- make_matrix_along_by_effect(prior = prior,
                                                        dimnames_term = dimnames_term,
                                                        var_time = var_time,
                                                        var_age = var_age)
  n_along <- nrow(matrix_along_by_effect)
  n_by <- ncol(matrix_along_by_effect)
  check_n_along_ge(n_along = n_along,
                   min = 4L,
                   nm = nm,
                   prior = prior)
  check_con_n_by(con = con,
                 n_by = n_by,
                 nm = nm)
  invisible(TRUE)
}


## HAS_TESTS
#' @export
is_prior_ok_for_term.bage_prior_rw2randomseasfix <- function(prior,
                                                             dimnames_term,
                                                             var_time,
                                                             var_age,
                                                             var_sexgender) {
  n_seas <- prior$specific$n_seas
  con <- prior$specific$con  
  nm <- dimnames_to_nm(dimnames_term)
  matrix_along_by_effect <- make_matrix_along_by_effect(prior = prior,
                                                        dimnames_term = dimnames_term,
                                                        var_time = var_time,
                                                        var_age = var_age)
  n_along <- nrow(matrix_along_by_effect)
  n_by <- ncol(matrix_along_by_effect)
  check_n_along_ge(n_along = n_along,
                   min = max(n_seas, 4L),
                   nm = nm,
                   prior = prior)
  check_con_n_by(con = con,
                 n_by = n_by,
                 nm = nm)
  invisible(TRUE)
}

## HAS_TESTS
#' @export
is_prior_ok_for_term.bage_prior_rw2randomseasvary <- function(prior,
                                                              dimnames_term,
                                                              var_time,
                                                              var_age,
                                                              var_sexgender) {
  n_seas <- prior$specific$n_seas
  con <- prior$specific$con  
  nm <- dimnames_to_nm(dimnames_term)
  matrix_along_by_effect <- make_matrix_along_by_effect(prior = prior,
                                                        dimnames_term = dimnames_term,
                                                        var_time = var_time,
                                                        var_age = var_age)
  n_along <- nrow(matrix_along_by_effect)
  n_by <- ncol(matrix_along_by_effect)
  check_n_along_ge(n_along = n_along,
                   min = max(n_seas, 4L),
                   nm = nm,
                   prior = prior)
  check_con_n_by(con = con,
                 n_by = n_by,
                 nm = nm)
  invisible(TRUE)
}

## HAS_TESTS
#' @export
is_prior_ok_for_term.bage_prior_rw2zero <- function(prior,
                                                      dimnames_term,
                                                      var_time,
                                                      var_age,
                                                      var_sexgender) {
  con <- prior$specific$con  
  nm <- dimnames_to_nm(dimnames_term)
  matrix_along_by_effect <- make_matrix_along_by_effect(prior = prior,
                                                        dimnames_term = dimnames_term,
                                                        var_time = var_time,
                                                        var_age = var_age)
  n_along <- nrow(matrix_along_by_effect)
  n_by <- ncol(matrix_along_by_effect)
  check_n_along_ge(n_along = n_along,
                   min = 4L,
                   nm = nm,
                   prior = prior)
  check_con_n_by(con = con,
                 n_by = n_by,
                 nm = nm)
  invisible(TRUE)
}


## HAS_TESTS
#' @export
is_prior_ok_for_term.bage_prior_rw2zeroseasfix <- function(prior,
                                                             dimnames_term,
                                                             var_time,
                                                             var_age,
                                                             var_sexgender) {
  n_seas <- prior$specific$n_seas
  con <- prior$specific$con  
  nm <- dimnames_to_nm(dimnames_term)
  matrix_along_by_effect <- make_matrix_along_by_effect(prior = prior,
                                                        dimnames_term = dimnames_term,
                                                        var_time = var_time,
                                                        var_age = var_age)
  n_along <- nrow(matrix_along_by_effect)
  n_by <- ncol(matrix_along_by_effect)
  check_n_along_ge(n_along = n_along,
                   min = max(n_seas, 4L),
                   nm = nm,
                   prior = prior)
  check_con_n_by(con = con,
                 n_by = n_by,
                 nm = nm)
  invisible(TRUE)
}

## HAS_TESTS
#' @export
is_prior_ok_for_term.bage_prior_rw2zeroseasvary <- function(prior,
                                                              dimnames_term,
                                                              var_time,
                                                              var_age,
                                                              var_sexgender) {
  n_seas <- prior$specific$n_seas
  con <- prior$specific$con  
  nm <- dimnames_to_nm(dimnames_term)
  matrix_along_by_effect <- make_matrix_along_by_effect(prior = prior,
                                                        dimnames_term = dimnames_term,
                                                        var_time = var_time,
                                                        var_age = var_age)
  n_along <- nrow(matrix_along_by_effect)
  n_by <- ncol(matrix_along_by_effect)
  check_n_along_ge(n_along = n_along,
                   min = max(n_seas, 4L),
                   nm = nm,
                   prior = prior)
  check_con_n_by(con = con,
                 n_by = n_by,
                 nm = nm)
  invisible(TRUE)
}

## HAS_TESTS
#' @export
is_prior_ok_for_term.bage_prior_spline <- function(prior,
                                                   dimnames_term,
                                                   var_time,
                                                   var_age,
                                                   var_sexgender) {
  con <- prior$specific$con  
  nm <- dimnames_to_nm(dimnames_term)
  matrix_along_by_effect <- make_matrix_along_by_effect(prior = prior,
                                                        dimnames_term = dimnames_term,
                                                        var_time = var_time,
                                                        var_age = var_age)
  n_along <- nrow(matrix_along_by_effect)
  n_by <- ncol(matrix_along_by_effect)
  check_length_effect_ge(length_effect = n_along,
                         min = 4L,
                         nm = nm,
                         prior = prior)
  check_con_n_by(con = con,
                 n_by = n_by,
                 nm = nm)
  invisible(TRUE)
}

## HAS_TESTS
#' @export
is_prior_ok_for_term.bage_prior_svd <- function(prior,
                                                dimnames_term,
                                                var_time,
                                                var_age,
                                                var_sexgender) {
  nm <- dimnames_to_nm(dimnames_term)
  agesex <- make_agesex(nm = nm,
                        var_age = var_age,
                        var_sexgender = var_sexgender)
  check_svd_agesex(prior = prior,
                   nm = nm,
                   var_age = var_age,
                   agesex = agesex)
  invisible(TRUE)
}

## HAS_TESTS
#' @export
is_prior_ok_for_term.bage_prior_svd_ar <- function(prior,
                                                   dimnames_term,
                                                   var_time,
                                                   var_age,
                                                   var_sexgender) {
  nm <- dimnames_to_nm(dimnames_term)
  agesex <- make_agesex(nm = nm,
                        var_age = var_age,
                        var_sexgender = var_sexgender)
  check_svd_agesex(prior = prior,
                   nm = nm,
                   var_age = var_age,
                   agesex = agesex)
  check_prior_time(prior = prior,
                 nm = nm,
                 var_time = var_time)
  con <- prior$specific$con  
  n_coef <- prior$specific$n_coef
  matrix_along_by_effect <- make_matrix_along_by_effect(prior = prior,
                                                        dimnames_term = dimnames_term,
                                                        var_time = var_time,
                                                        var_age = var_age)
  n_along <- nrow(matrix_along_by_effect)
  n_by <- ncol(matrix_along_by_effect)
  check_n_along_ge(n_along = n_along,
                   min = n_coef + 1L,
                   nm = nm,
                   prior = prior)
  check_con_n_by(con = con,
                 n_by = n_by,
                 nm = nm)
  invisible(TRUE)
}

## HAS_TESTS
#' @export
is_prior_ok_for_term.bage_prior_svd_rwrandom <- function(prior,
                                                   dimnames_term,
                                                   var_time,
                                                   var_age,
                                                   var_sexgender) {
  nm <- dimnames_to_nm(dimnames_term)
  agesex <- make_agesex(nm = nm,
                        var_age = var_age,
                        var_sexgender = var_sexgender)
  check_svd_agesex(prior = prior,
                   nm = nm,
                   var_age = var_age,
                   agesex = agesex)
  check_prior_time(prior = prior,
                   nm = nm,
                   var_time = var_time)
  con <- prior$specific$con  
  matrix_along_by_effect <- make_matrix_along_by_effect(prior = prior,
                                                        dimnames_term = dimnames_term,
                                                        var_time = var_time,
                                                        var_age = var_age)
  n_along <- nrow(matrix_along_by_effect)
  n_by <- ncol(matrix_along_by_effect)
  check_n_along_ge(n_along = n_along,
                   min = 3L,
                   nm = nm,
                   prior = prior)
  check_con_n_by(con = con,
                 n_by = n_by,
                 nm = nm)
  invisible(TRUE)
}


## HAS_TESTS
#' @export
is_prior_ok_for_term.bage_prior_svd_rwzero <- function(prior,
                                                       dimnames_term,
                                                       var_time,
                                                       var_age,
                                                       var_sexgender) {
  nm <- dimnames_to_nm(dimnames_term)
  agesex <- make_agesex(nm = nm,
                        var_age = var_age,
                        var_sexgender = var_sexgender)
  check_svd_agesex(prior = prior,
                   nm = nm,
                   var_age = var_age,
                   agesex = agesex)
  check_prior_time(prior = prior,
                   nm = nm,
                   var_time = var_time)
  con <- prior$specific$con  
  matrix_along_by_effect <- make_matrix_along_by_effect(prior = prior,
                                                        dimnames_term = dimnames_term,
                                                        var_time = var_time,
                                                        var_age = var_age)
  n_along <- nrow(matrix_along_by_effect)
  n_by <- ncol(matrix_along_by_effect)
  check_n_along_ge(n_along = n_along,
                   min = 3L,
                   nm = nm,
                   prior = prior)
  check_con_n_by(con = con,
                 n_by = n_by,
                 nm = nm)
  invisible(TRUE)
}


## HAS_TESTS
#' @export
is_prior_ok_for_term.bage_prior_svd_rw2random <- function(prior,
                                                          dimnames_term,
                                                          var_time,
                                                          var_age,
                                                          var_sexgender) {
  nm <- dimnames_to_nm(dimnames_term)
  agesex <- make_agesex(nm = nm,
                        var_age = var_age,
                        var_sexgender = var_sexgender)
  check_svd_agesex(prior = prior,
                   nm = nm,
                   var_age = var_age,
                   agesex = agesex)
  check_prior_time(prior = prior,
                   nm = nm,
                   var_time = var_time)
  con <- prior$specific$con  
  matrix_along_by_effect <- make_matrix_along_by_effect(prior = prior,
                                                        dimnames_term = dimnames_term,
                                                        var_time = var_time,
                                                        var_age = var_age)
  n_along <- nrow(matrix_along_by_effect)
  n_by <- ncol(matrix_along_by_effect)
  check_n_along_ge(n_along = n_along,
                   min = 4L,
                   nm = nm,
                   prior = prior)
  check_con_n_by(con = con,
                 n_by = n_by,
                 nm = nm)
  invisible(TRUE)
}

## HAS_TESTS
#' @export
is_prior_ok_for_term.bage_prior_svd_rw2zero <- function(prior,
                                                        dimnames_term,
                                                        var_time,
                                                        var_age,
                                                        var_sexgender) {
  nm <- dimnames_to_nm(dimnames_term)
  agesex <- make_agesex(nm = nm,
                        var_age = var_age,
                        var_sexgender = var_sexgender)
  check_svd_agesex(prior = prior,
                   nm = nm,
                   var_age = var_age,
                   agesex = agesex)
  check_prior_time(prior = prior,
                   nm = nm,
                   var_time = var_time)
  con <- prior$specific$con  
  matrix_along_by_effect <- make_matrix_along_by_effect(prior = prior,
                                                        dimnames_term = dimnames_term,
                                                        var_time = var_time,
                                                        var_age = var_age)
  n_along <- nrow(matrix_along_by_effect)
  n_by <- ncol(matrix_along_by_effect)
  check_n_along_ge(n_along = n_along,
                   min = 4L,
                   nm = nm,
                   prior = prior)
  check_con_n_by(con = con,
                 n_by = n_by,
                 nm = nm)
  invisible(TRUE)
}


## 'is_spline' -------------------------------------------------------------------

#' Test Whether Prior is Spline Prior
#'
#' @param prior An object of class 'bage_prior'.
#'
#' @returns TRUE or FALSE
#'
#' @noRd
is_spline <- function(prior) {
  UseMethod("is_spline")
}

## HAS_TESTS
#' @export
is_spline.bage_prior <- function(prior) FALSE

## HAS_TESTS
#' @export
is_spline.bage_prior_spline <- function(prior) TRUE


## 'is_svd' -------------------------------------------------------------------

#' Test Whether Prior is SVD Prior
#'
#' @param prior An object of class 'bage_prior'.
#'
#' @returns TRUE or FALSE
#'
#' @noRd
is_svd <- function(prior) {
  UseMethod("is_svd")
}

## HAS_TESTS
#' @export
is_svd.bage_prior <- function(prior) FALSE

## HAS_TESTS
#' @export
is_svd.bage_prior_svd <- function(prior) TRUE

## HAS_TESTS
#' @export
is_svd.bage_prior_svd_ar <- function(prior) TRUE

## HAS_TESTS
#' @export
is_svd.bage_prior_svd_rwrandom <- function(prior) TRUE

## HAS_TESTS
#' @export
is_svd.bage_prior_svd_rwzero <- function(prior) TRUE

## HAS_TESTS
#' @export
is_svd.bage_prior_svd_rw2random <- function(prior) TRUE

## HAS_TESTS
#' @export
is_svd.bage_prior_svd_rw2zero <- function(prior) TRUE


## 'length_hyperrandfree' -----------------------------------------------------

#' Lengths of Hyper-Parameters that Can Be Treated as Random Effects
#'
#' Most priors don't have hyper-parameters that can be treated
#' as random effects, so default value 0
#' 
#' @param prior An object of class 'bage_prior'.
#' @param dimnames_term Dimnames for array representation of term
#' @param var_time Name of time variable
#' @param var_age Name of age variable
#' @param var_sexgender Name of sex/gender variable
#'
#' @returns A character vector.
#'
#' @noRd
length_hyperrandfree <- function(prior,
                                 dimnames_term,
                                 var_time,
                                 var_age,
                                 var_sexgender) {
  UseMethod("length_hyperrandfree")
}

## HAS_TESTS
#' @export
length_hyperrandfree.bage_prior <- function(prior,
                                            dimnames_term,
                                            var_time,
                                            var_age,
                                            var_sexgender) {
  0L
}

## HAS_TESTS
#' @export
length_hyperrandfree.bage_prior_lin <- function(prior,
                                                dimnames_term,
                                                var_time,
                                                var_age,
                                                var_sexgender) {
  matrix_along_by_effectfree <- make_matrix_along_by_effectfree(prior = prior,
                                                                dimnames_term = dimnames_term,
                                                                var_time = var_time,
                                                                var_age = var_age,
                                                                var_sexgender = var_sexgender)
  ncol(matrix_along_by_effectfree)
}

## HAS_TESTS
#' @export
length_hyperrandfree.bage_prior_linar <- function(prior,
                                                  dimnames_term,
                                                  var_time,
                                                  var_age,
                                                  var_sexgender) {
  matrix_along_by_effectfree <- make_matrix_along_by_effectfree(prior = prior,
                                                                dimnames_term = dimnames_term,
                                                                var_time = var_time,
                                                                var_age = var_age,
                                                                var_sexgender = var_sexgender)
  ncol(matrix_along_by_effectfree)
}

## HAS_TESTS
#' @export
length_hyperrandfree.bage_prior_rwrandomseasfix <- function(prior,
                                                      dimnames_term,
                                                      var_time,
                                                      var_age,
                                                      var_sexgender) {
  n_seas <- prior$specific$n_seas
  matrix_along_by_effectfree <- make_matrix_along_by_effectfree(prior = prior,
                                                                dimnames_term = dimnames_term,
                                                                var_time = var_time,
                                                                var_age = var_age,
                                                                var_sexgender = var_sexgender)
  n_by <- ncol(matrix_along_by_effectfree)
  (n_seas - 1L) * n_by
}

## HAS_TESTS
#' @export
length_hyperrandfree.bage_prior_rwrandomseasvary <- function(prior,
                                                   dimnames_term,
                                                   var_time,
                                                   var_age,
                                 var_sexgender) {
  n_seas <- prior$specific$n_seas
  matrix_along_by_effectfree <- make_matrix_along_by_effectfree(prior = prior,
                                                                dimnames_term = dimnames_term,
                                                                var_time = var_time,
                                                                var_age = var_age,
                                                                var_sexgender = var_sexgender)
  n_along <- nrow(matrix_along_by_effectfree)
  n_by <- ncol(matrix_along_by_effectfree)
  s <- seq_len(n_along) - 1L
  is_last_season <- (s %% n_seas) == n_seas - 1L
  n_seas_free <- sum(!is_last_season)
  n_seas_free * n_by
}

## HAS_TESTS
#' @export
length_hyperrandfree.bage_prior_rwzeroseasfix <- function(prior,
                                                      dimnames_term,
                                                      var_time,
                                                      var_age,
                                                      var_sexgender) {
  n_seas <- prior$specific$n_seas
  matrix_along_by_effectfree <- make_matrix_along_by_effectfree(prior = prior,
                                                                dimnames_term = dimnames_term,
                                                                var_time = var_time,
                                                                var_age = var_age,
                                                                var_sexgender = var_sexgender)
  n_by <- ncol(matrix_along_by_effectfree)
  (n_seas - 2L) * n_by
}

## HAS_TESTS
#' @export
length_hyperrandfree.bage_prior_rwzeroseasvary <- function(prior,
                                                   dimnames_term,
                                                   var_time,
                                                   var_age,
                                 var_sexgender) {
  n_seas <- prior$specific$n_seas
  matrix_along_by_effectfree <- make_matrix_along_by_effectfree(prior = prior,
                                                                dimnames_term = dimnames_term,
                                                                var_time = var_time,
                                                                var_age = var_age,
                                                                var_sexgender = var_sexgender)
  n_along <- nrow(matrix_along_by_effectfree)
  n_by <- ncol(matrix_along_by_effectfree)
  s <- seq_len(n_along) - 1L
  is_first_period <- s == 0L
  is_last_season <- (s %% n_seas) == n_seas - 1L
  n_seas_free <- sum(!is_first_period & !is_last_season)
  n_seas_free * n_by
}

## HAS_TESTS
#' @export
length_hyperrandfree.bage_prior_rw2randomseasfix <- function(prior,
                                                       dimnames_term,
                                                       var_time,
                                                       var_age,
                                                       var_sexgender) {
  n_seas <- prior$specific$n_seas
  matrix_along_by_effectfree <- make_matrix_along_by_effectfree(prior = prior,
                                                                dimnames_term = dimnames_term,
                                                                var_time = var_time,
                                                                var_age = var_age,
                                                                var_sexgender = var_sexgender)
  n_by <- ncol(matrix_along_by_effectfree)
  (n_seas - 1L) * n_by
}

## HAS_TESTS
#' @export
length_hyperrandfree.bage_prior_rw2randomseasvary <- function(prior,
                                                        dimnames_term,
                                                        var_time,
                                                        var_age,
                                                        var_sexgender) {
  n_seas <- prior$specific$n_seas
  matrix_along_by_effectfree <- make_matrix_along_by_effectfree(prior = prior,
                                                                dimnames_term = dimnames_term,
                                                                var_time = var_time,
                                                                var_age = var_age,
                                                                var_sexgender = var_sexgender)
  n_along <- nrow(matrix_along_by_effectfree)
  n_by <- ncol(matrix_along_by_effectfree)
  s <- seq_len(n_along) - 1L
  is_last_season <- (s %% n_seas) == n_seas - 1L
  n_seas_free <- sum(!is_last_season)
  n_seas_free * n_by
}

## HAS_TESTS
#' @export
length_hyperrandfree.bage_prior_rw2zeroseasfix <- function(prior,
                                                       dimnames_term,
                                                       var_time,
                                                       var_age,
                                                       var_sexgender) {
  n_seas <- prior$specific$n_seas
  matrix_along_by_effectfree <- make_matrix_along_by_effectfree(prior = prior,
                                                                dimnames_term = dimnames_term,
                                                                var_time = var_time,
                                                                var_age = var_age,
                                                                var_sexgender = var_sexgender)
  n_by <- ncol(matrix_along_by_effectfree)
  (n_seas - 2L) * n_by
}

## HAS_TESTS
#' @export
length_hyperrandfree.bage_prior_rw2zeroseasvary <- function(prior,
                                                        dimnames_term,
                                                        var_time,
                                                        var_age,
                                                        var_sexgender) {
  n_seas <- prior$specific$n_seas
  matrix_along_by_effectfree <- make_matrix_along_by_effectfree(prior = prior,
                                                                dimnames_term = dimnames_term,
                                                                var_time = var_time,
                                                                var_age = var_age,
                                                                var_sexgender = var_sexgender)
  n_along <- nrow(matrix_along_by_effectfree)
  n_by <- ncol(matrix_along_by_effectfree)
  s <- seq_len(n_along) - 1L
  is_first_period <- s == 0L
  is_last_season <- (s %% n_seas) == n_seas - 1L
  n_seas_free <- sum(!is_first_period & !is_last_season)
  n_seas_free * n_by
}
## 'levels_hyper' -------------------------------------------------------------

#' Names of hyper-parameters
#'
#' @param prior An object of class 'bage_prior'.
#'
#' @returns A character vector.
#'
#' @noRd
levels_hyper <- function(prior) {
  UseMethod("levels_hyper")
}

## HAS_TESTS
#' @export
levels_hyper.bage_prior_ar <- function(prior) {
  n_coef <- prior$specific$n_coef
  if (n_coef == 1L)
    coef <- "coef"
  else
    coef <- paste0("coef", seq_len(n_coef))
  c(coef, "sd")
}

## HAS_TESTS
#' @export
levels_hyper.bage_prior_known <- function(prior)
  character()

## HAS_TESTS
#' @export
levels_hyper.bage_prior_lin <- function(prior) {
  "sd"
}

## HAS_TESTS
#' @export
levels_hyper.bage_prior_linar <- function(prior) {
  n_coef <- prior$specific$n_coef
  if (n_coef == 1L)
    coef <- "coef"
  else
    coef <- paste0("coef", seq_len(n_coef))
  c(coef, "sd")
}

## HAS_TESTS
#' @export
levels_hyper.bage_prior_linex <- function(prior) {
  character()
}

## HAS_TESTS
#' @export
levels_hyper.bage_prior_norm <- function(prior)
  "sd"

## HAS_TESTS
#' @export
levels_hyper.bage_prior_normfixed <- function(prior)
  character()

## HAS_TESTS
#' @export
levels_hyper.bage_prior_rwrandom <- function(prior)
  "sd"

## HAS_TESTS
#' @export
levels_hyper.bage_prior_rwrandomseasfix <- function(prior)
  "sd"

## HAS_TESTS
#' @export
levels_hyper.bage_prior_rwrandomseasvary <- function(prior)
  c("sd_seas", "sd")

## HAS_TESTS
#' @export
levels_hyper.bage_prior_rwzero <- function(prior)
  "sd"

## HAS_TESTS
#' @export
levels_hyper.bage_prior_rwzeroseasfix <- function(prior)
  "sd"

## HAS_TESTS
#' @export
levels_hyper.bage_prior_rwzeroseasvary <- function(prior)
  c("sd_seas", "sd")

## HAS_TESTS
#' @export
levels_hyper.bage_prior_rw2infant <- function(prior)
  "sd"

## HAS_TESTS
#' @export
levels_hyper.bage_prior_rw2random <- function(prior)
  "sd"

## HAS_TESTS
#' @export
levels_hyper.bage_prior_rw2randomseasfix <- function(prior)
  "sd"

## HAS_TESTS
#' @export
levels_hyper.bage_prior_rw2randomseasvary <- function(prior)
  c("sd_seas", "sd")

## HAS_TESTS
#' @export
levels_hyper.bage_prior_rw2zero <- function(prior)
  "sd"

## HAS_TESTS
#' @export
levels_hyper.bage_prior_rw2zeroseasfix <- function(prior)
  "sd"

## HAS_TESTS
#' @export
levels_hyper.bage_prior_rw2zeroseasvary <- function(prior)
  c("sd_seas", "sd")

## HAS_TESTS
#' @export
levels_hyper.bage_prior_spline <- function(prior)
  "sd"

## HAS_TESTS
#' @export
levels_hyper.bage_prior_svd <- function(prior)
  character()

## HAS_TESTS
#' @export
levels_hyper.bage_prior_svd_ar <- function(prior) {
  n_coef <- prior$specific$n_coef
  if (n_coef == 1L)
    coef <- "coef"
  else
    coef <- paste0("coef", seq_len(n_coef))
  c(coef, "sd")
}

## HAS_TESTS
#' @export
levels_hyper.bage_prior_svd_rwrandom <- function(prior)
  "sd"

## HAS_TESTS
#' @export
levels_hyper.bage_prior_svd_rwzero <- function(prior)
  "sd"

## HAS_TESTS
#' @export
levels_hyper.bage_prior_svd_rw2random <- function(prior)
  "sd"

## HAS_TESTS
#' @export
levels_hyper.bage_prior_svd_rw2zero <- function(prior)
  "sd"


## 'levels_hyperrand' ---------------------------------------------------------

#' Names of Hyper-Parameters that Can Be Treated as Random Effects
#'
#' Most priors don't have hyper-parameters that can be treated
#' as random effects, so default value is a character vector
#' of length 0.
#' 
#' @param prior An object of class 'bage_prior'.
#' @param dimnames_term Dimnames for array representation of term
#' @param var_time Name of time variable
#' @param var_age Name of age variable
#'
#' @returns A character vector.
#'
#' @noRd
levels_hyperrand <- function(prior, dimnames_term, var_age, var_time) {
  UseMethod("levels_hyperrand")
}

## HAS_TESTS
#' @export
levels_hyperrand.bage_prior <- function(prior, dimnames_term, var_age, var_time) {
  character()
}

## HAS_TESTS
#' @export
levels_hyperrand.bage_prior_lin <- function(prior, dimnames_term, var_age, var_time) {
  i_along <- make_i_along(prior = prior,
                          dimnames_term = dimnames_term,
                          var_time = var_time,
                          var_age = var_age)
  if (length(dimnames_term) > 1L) {
    levels_by <- dimnames_to_levels(dimnames_term[-i_along])
    slope <- paste("slope", levels_by, sep = ".")
  }
  else
    slope <- "slope"
  levels <- dimnames_to_levels(dimnames_term)
  c(slope, levels, levels)
}

## HAS_TESTS
#' @export
levels_hyperrand.bage_prior_linar <- function(prior, dimnames_term, var_age, var_time) {
  i_along <- make_i_along(prior = prior,
                          dimnames_term = dimnames_term,
                          var_time = var_time,
                          var_age = var_age)
  if (length(dimnames_term) > 1L) {
    levels_by <- dimnames_to_levels(dimnames_term[-i_along])
    slope <- paste("slope", levels_by, sep = ".")
  }
  else
    slope <- "slope"
  levels <- dimnames_to_levels(dimnames_term)
  c(slope, levels, levels)
}

## HAS_TESTS
#' @export
levels_hyperrand.bage_prior_rwrandomseasfix <- function(prior, dimnames_term, var_age, var_time) {
  levels <- dimnames_to_levels(dimnames_term)
  c(levels, levels)
}

## HAS_TESTS
#' @export
levels_hyperrand.bage_prior_rwrandomseasvary <- function(prior, dimnames_term, var_age, var_time) {
  levels <- dimnames_to_levels(dimnames_term)
  c(levels, levels)
}

## HAS_TESTS
#' @export
levels_hyperrand.bage_prior_rwzeroseasfix <- function(prior, dimnames_term, var_age, var_time) {
  levels <- dimnames_to_levels(dimnames_term)
  c(levels, levels)
}

## HAS_TESTS
#' @export
levels_hyperrand.bage_prior_rwzeroseasvary <- function(prior, dimnames_term, var_age, var_time) {
  levels <- dimnames_to_levels(dimnames_term)
  c(levels, levels)
}

## HAS_TESTS
#' @export
levels_hyperrand.bage_prior_rw2randomseasfix <- function(prior, dimnames_term, var_age, var_time) {
  levels <- dimnames_to_levels(dimnames_term)
  c(levels, levels)
}

## HAS_TESTS
#' @export
levels_hyperrand.bage_prior_rw2randomseasvary <- function(prior, dimnames_term, var_age, var_time) {
  levels <- dimnames_to_levels(dimnames_term)
  c(levels, levels)
}

## HAS_TESTS
#' @export
levels_hyperrand.bage_prior_rw2zeroseasfix <- function(prior, dimnames_term, var_age, var_time) {
  levels <- dimnames_to_levels(dimnames_term)
  c(levels, levels)
}

## HAS_TESTS
#' @export
levels_hyperrand.bage_prior_rw2zeroseasvary <- function(prior, dimnames_term, var_age, var_time) {
  levels <- dimnames_to_levels(dimnames_term)
  c(levels, levels)
}


## 'make_hyperrand_one' -------------------------------------------------------

#' Derive Values From Hyper-Parameters Treated as Random Effects
#'
#' @param prior Object of class 'bage_prior'.
#' @param hyperrandfree Values for unconstrained hyper-parameters. An rvec.
#' @param effectfree Values for unconstrained effect. An rvec.
#' @param dimnames_term Dimnames for array representation of term
#' @param var_time Name of time variable
#' @param var_age Name of age variable
#' @param var_sexgender Name of sex/gender variable
#'
#' @returns An rvec or NULL
#'
#' @noRd
make_hyperrand_one <- function(prior,
                               hyperrandfree,
                               effectfree,
                               dimnames_term,
                               var_time,
                               var_age,
                               var_sexgender) {
  UseMethod("make_hyperrand_one")
}

## HAS_TESTS
#' @export
make_hyperrand_one.bage_prior <- function(prior,
                                          hyperrandfree,
                                          effectfree,
                                          dimnames_term,
                                          var_time,
                                          var_age,
                                          var_sexgender)
  NULL

## HAS_TESTS
#' @export
make_hyperrand_one.bage_prior_lin <- function(prior,
                                              hyperrandfree,
                                              effectfree,
                                              dimnames_term,
                                              var_time,
                                              var_age,
                                              var_sexgender)
  make_hyperrand_lin(prior = prior,
                     hyperrandfree = hyperrandfree,
                     effectfree = effectfree,
                     dimnames_term = dimnames_term,
                     var_time = var_time,
                     var_age = var_age,
                     var_sexgender = var_sexgender)


## HAS_TESTS
#' @export
make_hyperrand_one.bage_prior_linar <- function(prior,
                                                hyperrandfree,
                                                effectfree,
                                                dimnames_term,
                                                var_time,
                                                var_age,
                                                var_sexgender)
  make_hyperrand_lin(prior = prior,
                     hyperrandfree = hyperrandfree,
                     effectfree = effectfree,
                     dimnames_term = dimnames_term,
                     var_time = var_time,
                     var_age = var_age,
                     var_sexgender = var_sexgender)
  

## HAS_TESTS
#' @export
make_hyperrand_one.bage_prior_rwrandomseasfix <- function(prior,
                                                          hyperrandfree,
                                                          effectfree,
                                                          dimnames_term,
                                                          var_time,
                                                          var_age,
                                                          var_sexgender)
  make_hyperrand_randomseasfix(prior = prior,
                               hyperrandfree = hyperrandfree,
                               effectfree = effectfree,
                               dimnames_term = dimnames_term,
                               var_time = var_time,
                               var_age = var_age,
                               var_sexgender = var_sexgender)

## HAS_TESTS
#' @export
make_hyperrand_one.bage_prior_rwrandomseasvary <- function(prior,
                                                           hyperrandfree,
                                                           effectfree,
                                                           dimnames_term,
                                                           var_time,
                                                           var_age,
                                                           var_sexgender)
  make_hyperrand_randomseasvary(prior = prior,
                                hyperrandfree = hyperrandfree,
                                effectfree = effectfree,
                                dimnames_term = dimnames_term,
                                var_time = var_time,
                                var_age = var_age,
                                var_sexgender = var_sexgender)

## HAS_TESTS
#' @export
make_hyperrand_one.bage_prior_rwzeroseasfix <- function(prior,
                                                        hyperrandfree,
                                                        effectfree,
                                                        dimnames_term,
                                                        var_time,
                                                        var_age,
                                                        var_sexgender)
  make_hyperrand_zeroseasfix(prior = prior,
                             hyperrandfree = hyperrandfree,
                             effectfree = effectfree,
                             dimnames_term = dimnames_term,
                             var_time = var_time,
                             var_age = var_age,
                             var_sexgender = var_sexgender)

## HAS_TESTS
#' @export
make_hyperrand_one.bage_prior_rwzeroseasvary <- function(prior,
                                                         hyperrandfree,
                                                         effectfree,
                                                         dimnames_term,
                                                         var_time,
                                                         var_age,
                                                         var_sexgender)
  make_hyperrand_zeroseasvary(prior = prior,
                              hyperrandfree = hyperrandfree,
                              effectfree = effectfree,
                              dimnames_term = dimnames_term,
                              var_time = var_time,
                              var_age = var_age,
                              var_sexgender = var_sexgender)

## HAS_TESTS
#' @export
make_hyperrand_one.bage_prior_rw2randomseasfix <- function(prior,
                                                           hyperrandfree,
                                                           effectfree,
                                                           dimnames_term,
                                                           var_time,
                                                           var_age,
                                                           var_sexgender)
  make_hyperrand_randomseasfix(prior = prior,
                               hyperrandfree = hyperrandfree,
                               effectfree = effectfree,
                               dimnames_term = dimnames_term,
                               var_time = var_time,
                               var_age = var_age,
                               var_sexgender = var_sexgender)

## HAS_TESTS
#' @export
make_hyperrand_one.bage_prior_rw2randomseasvary <- function(prior,
                                                            hyperrandfree,
                                                            effectfree,
                                                            dimnames_term,
                                                            var_time,
                                                            var_age,
                                                            var_sexgender)
  make_hyperrand_randomseasvary(prior = prior,
                                hyperrandfree = hyperrandfree,
                                effectfree = effectfree,
                                dimnames_term = dimnames_term,
                                var_time = var_time,
                                var_age = var_age,
                                var_sexgender = var_sexgender)

## HAS_TESTS
#' @export
make_hyperrand_one.bage_prior_rw2zeroseasfix <- function(prior,
                                                         hyperrandfree,
                                                         effectfree,
                                                         dimnames_term,
                                                         var_time,
                                                         var_age,
                                                         var_sexgender)
  make_hyperrand_zeroseasfix(prior = prior,
                             hyperrandfree = hyperrandfree,
                             effectfree = effectfree,
                             dimnames_term = dimnames_term,
                             var_time = var_time,
                             var_age = var_age,
                             var_sexgender = var_sexgender)

## HAS_TESTS
#' @export
make_hyperrand_one.bage_prior_rw2zeroseasvary <- function(prior,
                                                          hyperrandfree,
                                                          effectfree,
                                                          dimnames_term,
                                                          var_time,
                                                          var_age,
                                                          var_sexgender)
  make_hyperrand_zeroseasvary(prior = prior,
                              hyperrandfree = hyperrandfree,
                              effectfree = effectfree,
                              dimnames_term = dimnames_term,
                              var_time = var_time,
                              var_age = var_age,
                              var_sexgender = var_sexgender)


## 'make_i_along' -------------------------------------------------------------

## HAS_TESTS
#' Find the Index of the Along Dimension,
#' Throwing an Error If Cannot be Found
#'
#' If 'along' is non-NULL, look for a dimension
#' with that name. Otherwise, use the dimension
#' identified by 'var_time', or by 'var_age'.
#'
#' @param Object of class 'bage_prior'
#' @param dimnames_term Dimnames for array
#' representing term
#' @param var_time Name of the time dimension, or NULL
#' @param var_age Name of the age dimension, or NULL
#'
#' @returns An integer
#'
#' @noRd
make_i_along <- function(prior, dimnames_term, var_time, var_age) {
  UseMethod("make_i_along")
}

## HAS_TESTS
#' @export
make_i_along.bage_prior <- function(prior, dimnames_term, var_time, var_age) {
  1L
}

## HAS_TESTS
#' @export
make_i_along.bage_prior_ar <- function(prior, dimnames_term, var_time, var_age) {
  along <- prior$specific$along
  make_i_along_inner(along = along,
                     dimnames_term = dimnames_term,
                     var_time = var_time,
                     var_age = var_age)
}

## HAS_TESTS
#' @export
make_i_along.bage_prior_lin <- function(prior, dimnames_term, var_time, var_age) {
  along <- prior$specific$along
  make_i_along_inner(along = along,
                     dimnames_term = dimnames_term,
                     var_time = var_time,
                     var_age = var_age)
}

## HAS_TESTS
#' @export
make_i_along.bage_prior_linar <- function(prior, dimnames_term, var_time, var_age) {
  along <- prior$specific$along
  make_i_along_inner(along = along,
                     dimnames_term = dimnames_term,
                     var_time = var_time,
                     var_age = var_age)
}

## HAS_TESTS
#' @export
make_i_along.bage_prior_linex <- function(prior, dimnames_term, var_time, var_age) {
  along <- prior$specific$along
  make_i_along_inner(along = along,
                     dimnames_term = dimnames_term,
                     var_time = var_time,
                     var_age = var_age)
}

## HAS_TESTS
#' @export
make_i_along.bage_prior_rwrandom <- function(prior, dimnames_term, var_time, var_age) {
  along <- prior$specific$along
  make_i_along_inner(along = along,
                     dimnames_term = dimnames_term,
                     var_time = var_time,
                     var_age = var_age)
}

## HAS_TESTS
#' @export
make_i_along.bage_prior_rwrandomseasfix <- function(prior, dimnames_term, var_time, var_age) {
  along <- prior$specific$along
  make_i_along_inner(along = along,
                     dimnames_term = dimnames_term,
                     var_time = var_time,
                     var_age = var_age)
}

## HAS_TESTS
#' @export
make_i_along.bage_prior_rwrandomseasvary <- function(prior, dimnames_term, var_time, var_age) {
  along <- prior$specific$along
  make_i_along_inner(along = along,
                     dimnames_term = dimnames_term,
                     var_time = var_time,
                     var_age = var_age)
}

## HAS_TESTS
#' @export
make_i_along.bage_prior_rwzero <- function(prior, dimnames_term, var_time, var_age) {
  along <- prior$specific$along
  make_i_along_inner(along = along,
                     dimnames_term = dimnames_term,
                     var_time = var_time,
                     var_age = var_age)
}

## HAS_TESTS
#' @export
make_i_along.bage_prior_rwzeroseasfix <- function(prior, dimnames_term, var_time, var_age) {
  along <- prior$specific$along
  make_i_along_inner(along = along,
                     dimnames_term = dimnames_term,
                     var_time = var_time,
                     var_age = var_age)
}

## HAS_TESTS
#' @export
make_i_along.bage_prior_rwzeroseasvary <- function(prior, dimnames_term, var_time, var_age) {
  along <- prior$specific$along
  make_i_along_inner(along = along,
                     dimnames_term = dimnames_term,
                     var_time = var_time,
                     var_age = var_age)
}

## HAS_TESTS
#' @export
make_i_along.bage_prior_rw2infant <- function(prior, dimnames_term, var_time, var_age) {
  make_i_along_agetime(prior = prior,
                       dimnames_term = dimnames_term,
                       var_time = var_time,
                       var_age = var_age,
                       agetime = "age")
}

## HAS_TESTS
#' @export
make_i_along.bage_prior_rw2random <- function(prior, dimnames_term, var_time, var_age) {
  along <- prior$specific$along
  make_i_along_inner(along = along,
                     dimnames_term = dimnames_term,
                     var_time = var_time,
                     var_age = var_age)
}

## HAS_TESTS
#' @export
make_i_along.bage_prior_rw2randomseasfix <- function(prior, dimnames_term, var_time, var_age) {
  along <- prior$specific$along
  make_i_along_inner(along = along,
                     dimnames_term = dimnames_term,
                     var_time = var_time,
                     var_age = var_age)
}

## HAS_TESTS
#' @export
make_i_along.bage_prior_rw2randomseasvary <- function(prior, dimnames_term, var_time, var_age) {
  along <- prior$specific$along
  make_i_along_inner(along = along,
                     dimnames_term = dimnames_term,
                     var_time = var_time,
                     var_age = var_age)
}

## HAS_TESTS
#' @export
make_i_along.bage_prior_rw2zero <- function(prior, dimnames_term, var_time, var_age) {
  along <- prior$specific$along
  make_i_along_inner(along = along,
                     dimnames_term = dimnames_term,
                     var_time = var_time,
                     var_age = var_age)
}

## HAS_TESTS
#' @export
make_i_along.bage_prior_rw2zeroseasfix <- function(prior, dimnames_term, var_time, var_age) {
  along <- prior$specific$along
  make_i_along_inner(along = along,
                     dimnames_term = dimnames_term,
                     var_time = var_time,
                     var_age = var_age)
}

## HAS_TESTS
#' @export
make_i_along.bage_prior_rw2zeroseasvary <- function(prior, dimnames_term, var_time, var_age) {
  along <- prior$specific$along
  make_i_along_inner(along = along,
                     dimnames_term = dimnames_term,
                     var_time = var_time,
                     var_age = var_age)
}

## HAS_TESTS
#' @export
make_i_along.bage_prior_spline <- function(prior, dimnames_term, var_time, var_age) {
  along <- prior$specific$along
  make_i_along_inner(along = along,
                     dimnames_term = dimnames_term,
                     var_time = var_time,
                     var_age = var_age)
}

## HAS_TESTS
#' @export
make_i_along.bage_prior_svd_ar <- function(prior, dimnames_term, var_time, var_age) {
  make_i_along_agetime(prior = prior,
                       dimnames_term = dimnames_term,
                       var_time = var_time,
                       var_age = var_age,
                       agetime = "time")
}

## HAS_TESTS
#' @export
make_i_along.bage_prior_svd_rwrandom <- function(prior, dimnames_term, var_time, var_age) {
  make_i_along_agetime(prior = prior,
                       dimnames_term = dimnames_term,
                       var_time = var_time,
                       var_age = var_age,
                       agetime = "time")
}

## HAS_TESTS
#' @export
make_i_along.bage_prior_svd_rwzero <- function(prior, dimnames_term, var_time, var_age) {
  make_i_along_agetime(prior = prior,
                       dimnames_term = dimnames_term,
                       var_time = var_time,
                       var_age = var_age,
                       agetime = "time")
}

## HAS_TESTS
#' @export
make_i_along.bage_prior_svd_rw2random <- function(prior, dimnames_term, var_time, var_age) {
  make_i_along_agetime(prior = prior,
                       dimnames_term = dimnames_term,
                       var_time = var_time,
                       var_age = var_age,
                       agetime = "time")
}

## HAS_TESTS
#' @export
make_i_along.bage_prior_svd_rw2zero <- function(prior, dimnames_term, var_time, var_age) {
  make_i_along_agetime(prior = prior,
                       dimnames_term = dimnames_term,
                       var_time = var_time,
                       var_age = var_age,
                       agetime = "time")
}


## 'make_matrix_along_by_effectfree' ------------------------------------------

#' Make a 'matrix_along_by' Matrix for Free Parameters for One Term
#'
#' @param prior Object of class 'bage_prior'
#' @param dimnames_term Dimnames for array
#' representing term
#' @param var_time Name of time variable, or NULL
#' @param var_age Name of age variable, or NULL
#' @param var_sexgender Name of sexgender variable, or NULL
#'
#' @returns A matrix.
#'
#' @noRd
make_matrix_along_by_effectfree <- function(prior,
                                            dimnames_term,
                                            var_time,
                                            var_age,
                                            var_sexgender) {
  UseMethod("make_matrix_along_by_effectfree")
}

## HAS_TESTS
#' @export
make_matrix_along_by_effectfree.bage_prior <- function(prior,
                                                       dimnames_term,
                                                       var_time,
                                                       var_age,
                                                       var_sexgender) {
  n_dim <- length(dimnames_term)
  if (n_dim == 0L) {
    matrix(0L, nrow = 1L)
  }
  else {
    dim <- lengths(dimnames_term)
    make_matrix_along_by_inner(i_along = 1L,
                               dim = dim)
  }
}

## HAS_TESTS
#' @export
make_matrix_along_by_effectfree.bage_prior_ar <- function(prior,
                                                          dimnames_term,
                                                          var_time,
                                                          var_age,
                                                          var_sexgender) {
  make_matrix_along_by_effectfree_inner(prior = prior,
                                        dimnames_term = dimnames_term,
                                        var_time = var_time,
                                        var_age = var_age,
                                        var_sexgender = var_sexgender,
                                        append_zero = FALSE)
}

## HAS_TESTS
#' @export
make_matrix_along_by_effectfree.bage_prior_lin <- function(prior,
                                                          dimnames_term,
                                                          var_time,
                                                          var_age,
                                                          var_sexgender) {
  make_matrix_along_by_effectfree_inner(prior = prior,
                                        dimnames_term = dimnames_term,
                                        var_time = var_time,
                                        var_age = var_age,
                                        var_sexgender = var_sexgender,
                                        append_zero = FALSE)
}

## HAS_TESTS
#' @export
make_matrix_along_by_effectfree.bage_prior_linar <- function(prior,
                                                          dimnames_term,
                                                          var_time,
                                                          var_age,
                                                          var_sexgender) {
  make_matrix_along_by_effectfree_inner(prior = prior,
                                        dimnames_term = dimnames_term,
                                        var_time = var_time,
                                        var_age = var_age,
                                        var_sexgender = var_sexgender,
                                        append_zero = FALSE)
}

## HAS_TESTS
#' @export
make_matrix_along_by_effectfree.bage_prior_linex <- function(prior,
                                                          dimnames_term,
                                                          var_time,
                                                          var_age,
                                                          var_sexgender) {
  make_matrix_along_by_effectfree_inner(prior = prior,
                                        dimnames_term = dimnames_term,
                                        var_time = var_time,
                                        var_age = var_age,
                                        var_sexgender = var_sexgender,
                                        append_zero = FALSE)
}

## HAS_TESTS
#' @export
make_matrix_along_by_effectfree.bage_prior_rwrandom <- function(prior,
                                                          dimnames_term,
                                                          var_time,
                                                          var_age,
                                                          var_sexgender) {
  make_matrix_along_by_effectfree_inner(prior = prior,
                                        dimnames_term = dimnames_term,
                                        var_time = var_time,
                                        var_age = var_age,
                                        var_sexgender = var_sexgender,
                                        append_zero = FALSE)
}

## HAS_TESTS
#' @export
make_matrix_along_by_effectfree.bage_prior_rwrandomseasfix <- function(prior,
                                                          dimnames_term,
                                                          var_time,
                                                          var_age,
                                                          var_sexgender) {
  make_matrix_along_by_effectfree_inner(prior = prior,
                                        dimnames_term = dimnames_term,
                                        var_time = var_time,
                                        var_age = var_age,
                                        var_sexgender = var_sexgender,
                                        append_zero = FALSE)
}

## HAS_TESTS
#' @export
make_matrix_along_by_effectfree.bage_prior_rwrandomseasvary <- function(prior,
                                                                  dimnames_term,
                                                                  var_time,
                                                                  var_age,
                                                                  var_sexgender) {
  make_matrix_along_by_effectfree_inner(prior = prior,
                                        dimnames_term = dimnames_term,
                                        var_time = var_time,
                                        var_age = var_age,
                                        var_sexgender = var_sexgender,
                                        append_zero = FALSE)
}


## HAS_TESTS
#' @export
make_matrix_along_by_effectfree.bage_prior_rwzero <- function(prior,
                                                              dimnames_term,
                                                              var_time,
                                                              var_age,
                                                              var_sexgender) {
  make_matrix_along_by_effectfree_inner(prior = prior,
                                        dimnames_term = dimnames_term,
                                        var_time = var_time,
                                        var_age = var_age,
                                        var_sexgender = var_sexgender,
                                        append_zero = TRUE)
}

## HAS_TESTS
#' @export
make_matrix_along_by_effectfree.bage_prior_rwzeroseasfix <- function(prior,
                                                          dimnames_term,
                                                          var_time,
                                                          var_age,
                                                          var_sexgender) {
  make_matrix_along_by_effectfree_inner(prior = prior,
                                        dimnames_term = dimnames_term,
                                        var_time = var_time,
                                        var_age = var_age,
                                        var_sexgender = var_sexgender,
                                        append_zero = FALSE)
}

## HAS_TESTS
#' @export
make_matrix_along_by_effectfree.bage_prior_rwzeroseasvary <- function(prior,
                                                                  dimnames_term,
                                                                  var_time,
                                                                  var_age,
                                                                  var_sexgender) {
  make_matrix_along_by_effectfree_inner(prior = prior,
                                        dimnames_term = dimnames_term,
                                        var_time = var_time,
                                        var_age = var_age,
                                        var_sexgender = var_sexgender,
                                        append_zero = FALSE)
}

## HAS_TESTS
#' @export
make_matrix_along_by_effectfree.bage_prior_rw2infant <- function(prior,
                                                                 dimnames_term,
                                                                 var_time,
                                                                 var_age,
                                                                 var_sexgender) {
  make_matrix_along_by_effectfree_inner(prior = prior,
                                        dimnames_term = dimnames_term,
                                        var_time = var_time,
                                        var_age = var_age,
                                        var_sexgender = var_sexgender,
                                        append_zero = FALSE)
}

## HAS_TESTS
#' @export
make_matrix_along_by_effectfree.bage_prior_rw2random <- function(prior,
                                                                 dimnames_term,
                                                                 var_time,
                                                                 var_age,
                                                                 var_sexgender) {
  make_matrix_along_by_effectfree_inner(prior = prior,
                                        dimnames_term = dimnames_term,
                                        var_time = var_time,
                                        var_age = var_age,
                                        var_sexgender = var_sexgender,
                                        append_zero = FALSE)
}

## HAS_TESTS
#' @export
make_matrix_along_by_effectfree.bage_prior_rw2zeroseasfix <- function(prior,
                                                          dimnames_term,
                                                          var_time,
                                                          var_age,
                                                          var_sexgender) {
  make_matrix_along_by_effectfree_inner(prior = prior,
                                        dimnames_term = dimnames_term,
                                        var_time = var_time,
                                        var_age = var_age,
                                        var_sexgender = var_sexgender,
                                        append_zero = FALSE)
}

## HAS_TESTS
#' @export
make_matrix_along_by_effectfree.bage_prior_rw2zeroseasvary <- function(prior,
                                                                  dimnames_term,
                                                                  var_time,
                                                                  var_age,
                                                                  var_sexgender) {
  make_matrix_along_by_effectfree_inner(prior = prior,
                                        dimnames_term = dimnames_term,
                                        var_time = var_time,
                                        var_age = var_age,
                                        var_sexgender = var_sexgender,
                                        append_zero = FALSE)
}

## HAS_TESTS
#' @export
make_matrix_along_by_effectfree.bage_prior_rw2zero <- function(prior,
                                                                  dimnames_term,
                                                                  var_time,
                                                                  var_age,
                                                                  var_sexgender) {
  make_matrix_along_by_effectfree_inner(prior = prior,
                                        dimnames_term = dimnames_term,
                                        var_time = var_time,
                                        var_age = var_age,
                                        var_sexgender = var_sexgender,
                                        append_zero = TRUE)
}

## HAS_TESTS
#' @export
make_matrix_along_by_effectfree.bage_prior_rw2zeroseasfix <- function(prior,
                                                          dimnames_term,
                                                          var_time,
                                                          var_age,
                                                          var_sexgender) {
  make_matrix_along_by_effectfree_inner(prior = prior,
                                        dimnames_term = dimnames_term,
                                        var_time = var_time,
                                        var_age = var_age,
                                        var_sexgender = var_sexgender,
                                        append_zero = FALSE)
}

## HAS_TESTS
#' @export
make_matrix_along_by_effectfree.bage_prior_rw2zeroseasvary <- function(prior,
                                                                  dimnames_term,
                                                                  var_time,
                                                                  var_age,
                                                                  var_sexgender) {
  make_matrix_along_by_effectfree_inner(prior = prior,
                                        dimnames_term = dimnames_term,
                                        var_time = var_time,
                                        var_age = var_age,
                                        var_sexgender = var_sexgender,
                                        append_zero = FALSE)
}

## HAS_TESTS
#' @export
make_matrix_along_by_effectfree.bage_prior_spline <- function(prior,
                                                              dimnames_term,
                                                              var_time,
                                                              var_age,
                                                              var_sexgender) {
  make_matrix_along_by_effectfree_inner(prior = prior,
                                        dimnames_term = dimnames_term,
                                        var_time = var_time,
                                        var_age = var_age,
                                        var_sexgender = var_sexgender,
                                        append_zero = FALSE)
}

## HAS_TESTS
#' @export
make_matrix_along_by_effectfree.bage_prior_svd <- function(prior,
                                                           dimnames_term,
                                                           var_time,
                                                           var_age,
                                                           var_sexgender) {
  dim <- lengths(dimnames_term)
  make_matrix_along_by_effectfree_innermost(prior = prior,
                                            dimnames_term = dimnames_term,
                                            var_time = var_time,
                                            var_age = var_age,
                                            var_sexgender = var_sexgender,
                                            dim = dim)
}

## HAS_TESTS
#' @export
make_matrix_along_by_effectfree.bage_prior_svd_ar <- function(prior,
                                                              dimnames_term,
                                                              var_time,
                                                              var_age,
                                                              var_sexgender) {
  make_matrix_along_by_effectfree_inner(prior = prior,
                                        dimnames_term = dimnames_term,
                                        var_time = var_time,
                                        var_age = var_age,
                                        var_sexgender = var_sexgender,
                                        append_zero = FALSE)
}

## HAS_TESTS
#' @export
make_matrix_along_by_effectfree.bage_prior_svd_rwrandom <- function(prior,
                                                              dimnames_term,
                                                              var_time,
                                                              var_age,
                                                              var_sexgender) {
  make_matrix_along_by_effectfree_inner(prior = prior,
                                        dimnames_term = dimnames_term,
                                        var_time = var_time,
                                        var_age = var_age,
                                        var_sexgender = var_sexgender,
                                        append_zero = FALSE)
}

## HAS_TESTS
#' @export
make_matrix_along_by_effectfree.bage_prior_svd_rwzero <- function(prior,
                                                              dimnames_term,
                                                              var_time,
                                                              var_age,
                                                              var_sexgender) {
  make_matrix_along_by_effectfree_inner(prior = prior,
                                        dimnames_term = dimnames_term,
                                        var_time = var_time,
                                        var_age = var_age,
                                        var_sexgender = var_sexgender,
                                        append_zero = TRUE)
}

## HAS_TESTS
#' @export
make_matrix_along_by_effectfree.bage_prior_svd_rw2random <- function(prior,
                                                               dimnames_term,
                                                               var_time,
                                                               var_age,
                                                               var_sexgender) {
  make_matrix_along_by_effectfree_inner(prior = prior,
                                        dimnames_term = dimnames_term,
                                        var_time = var_time,
                                        var_age = var_age,
                                        var_sexgender = var_sexgender,
                                        append_zero = FALSE)
}

## HAS_TESTS
#' @export
make_matrix_along_by_effectfree.bage_prior_svd_rw2zero <- function(prior,
                                                               dimnames_term,
                                                               var_time,
                                                               var_age,
                                                               var_sexgender) {
  make_matrix_along_by_effectfree_inner(prior = prior,
                                        dimnames_term = dimnames_term,
                                        var_time = var_time,
                                        var_age = var_age,
                                        var_sexgender = var_sexgender,
                                        append_zero = TRUE)
}


## 'make_matrix_along_by_effectfree_innermost' ------------------------------------------

#' Helper function for 'make_matrix_along_by_effectfree'
#'
#' Called by (ordinary, non-method) helper function
#' 'make_matrix_along_by_effectfree_inner'.
#'
#' @param prior Object of class 'bage_prior'
#' @param dimnames_term Dimnames for array
#' representing term
#' @param var_time Name of time variable, or NULL
#' @param var_age Name of age variable, or NULL
#' @param var_sexgender Name of sexgender variable, or NULL
#' @param dim Dimension of the term, once arguments
#' such as 'con' and 'append_zero' have been applied.
#'
#' @returns A matrix.
#'
#' @noRd
make_matrix_along_by_effectfree_innermost <- function(prior,
                                                      dimnames_term,
                                                      var_time,
                                                      var_age,
                                                      var_sexgender,
                                                      dim) {
  UseMethod("make_matrix_along_by_effectfree_innermost")
}


## HAS_TESTS
#' @export
make_matrix_along_by_effectfree_innermost.bage_prior <- function(prior,
                                                                 dimnames_term,
                                                                 var_time,
                                                                 var_age,
                                                                 var_sexgender,
                                                                 dim) {
  i_along <- make_i_along(prior = prior,
                          dimnames_term = dimnames_term,
                          var_time = var_time,
                          var_age = var_age)
  make_matrix_along_by_inner(i_along = i_along,
                             dim = dim)
}

## HAS_TESTS
#' @export
make_matrix_along_by_effectfree_innermost.bage_prior_spline <- function(prior,
                                                                        dimnames_term,
                                                                        var_time,
                                                                        var_age,
                                                                        var_sexgender,
                                                                        dim) {
  i_along <- make_i_along(prior = prior,
                          dimnames_term = dimnames_term,
                          var_time = var_time,
                          var_age = var_age)
  n_along <- dim[[i_along]]
  n_comp <- get_n_comp_spline(prior = prior,
                              n_along = n_along)
  dim[[i_along]] <- n_comp
  make_matrix_along_by_inner(i_along = i_along,
                             dim = dim)
}

## HAS_TESTS
#' @export
make_matrix_along_by_effectfree_innermost.bage_prior_svd <- function(prior, 
                                                                     dimnames_term,
                                                                     var_time,
                                                                     var_age,
                                                                     var_sexgender,
                                                                     dim) {
  dim <- make_dim_svd(prior = prior,
                      dimnames_term = dimnames_term,
                      var_sexgender = var_sexgender,
                      var_age = var_age)
  make_matrix_along_by_inner(i_along = 1L,
                             dim = dim)
}

## HAS_TESTS
#' @export
make_matrix_along_by_effectfree_innermost.bage_prior_svd_ar <- function(prior, 
                                                                        dimnames_term,
                                                                        var_time,
                                                                        var_age,
                                                                        var_sexgender,
                                                                        dim) {
  make_matrix_along_by_svddynamic(prior = prior,
                                  dimnames_term = dimnames_term,
                                  var_time = var_time,
                                  var_age = var_age,
                                  var_sexgender = var_sexgender,
                                  dim = dim)
}

## HAS_TESTS
#' @export
make_matrix_along_by_effectfree_innermost.bage_prior_svd_rwrandom <- function(prior, 
                                                                              dimnames_term,
                                                                              var_time,
                                                                              var_age,
                                                                              var_sexgender,
                                                                              dim) {
  make_matrix_along_by_svddynamic(prior = prior,
                                  dimnames_term = dimnames_term,
                                  var_time = var_time,
                                  var_age = var_age,
                                  var_sexgender = var_sexgender,
                                  dim = dim)
}

## HAS_TESTS
#' @export
make_matrix_along_by_effectfree_innermost.bage_prior_svd_rwzero <- function(prior, 
                                                                            dimnames_term,
                                                                            var_time,
                                                                            var_age,
                                                                            var_sexgender,
                                                                            dim) {
  make_matrix_along_by_svddynamic(prior = prior,
                                  dimnames_term = dimnames_term,
                                  var_time = var_time,
                                  var_age = var_age,
                                  var_sexgender = var_sexgender,
                                  dim = dim)
}

## HAS_TESTS
#' @export
make_matrix_along_by_effectfree_innermost.bage_prior_svd_rw2random <- function(prior, 
                                                                               dimnames_term,
                                                                               var_time,
                                                                               var_age,
                                                                               var_sexgender,
                                                                               dim) {
  make_matrix_along_by_svddynamic(prior = prior,
                                  dimnames_term = dimnames_term,
                                  var_time = var_time,
                                  var_age = var_age,
                                  var_sexgender = var_sexgender,
                                  dim = dim)
}

## HAS_TESTS
#' @export
make_matrix_along_by_effectfree_innermost.bage_prior_svd_rw2zero <- function(prior, 
                                                                             dimnames_term,
                                                                             var_time,
                                                                             var_age,
                                                                             var_sexgender,
                                                                             dim) {
  make_matrix_along_by_svddynamic(prior = prior,
                                  dimnames_term = dimnames_term,
                                  var_time = var_time,
                                  var_age = var_age,
                                  var_sexgender = var_sexgender,
                                  dim = dim)
}


## 'make_matrix_draws_svd' ----------------------------------------------------

#' Make Matrix to Transform Draws for SVD
#'
#' The transform is to add zeros to the
#' start of the time dimension, which is
#' only done with some dynamic SVD priors.
#'
#' Return NULL with non-SVD priors
#' 
#' @param prior Object of class 'bage_prior'
#' @param dimnames_term Dimnames for array
#' representing term
#' @param var_time Name of time variable, or NULL
#' @param var_age Name of age variable, or NULL
#' @param var_sexgender Name of sexgender variable, or NULL
#'
#' @returns A sparse matrix
#'
#' @noRd
make_matrix_draws_svd <- function(prior,
                                  dimnames_term,
                                  var_time,
                                  var_age,
                                  var_sexgender) {
  UseMethod("make_matrix_draws_svd")
}

#' @export
make_matrix_draws_svd.bage_prior <- function(prior,
                                             dimnames_term,
                                             var_time,
                                             var_age,
                                             var_sexgender) {
  NULL
}

## HAS_TESTS
#' @export
make_matrix_draws_svd.bage_prior_svd <- function(prior,
                                                 dimnames_term,
                                                 var_time,
                                                 var_age,
                                                 var_sexgender) {
  make_matrix_draws_svd_nozero(prior = prior,
                               dimnames_term = dimnames_term,
                               var_time = var_time,
                               var_age = var_age,
                               var_sexgender = var_sexgender)
}

## HAS_TESTS
#' @export
make_matrix_draws_svd.bage_prior_svd_ar <- function(prior,
                                                    dimnames_term,
                                                    var_time,
                                                    var_age,
                                                    var_sexgender) {
  make_matrix_draws_svd_nozero(prior = prior,
                               dimnames_term = dimnames_term,
                               var_time = var_time,
                               var_age = var_age,
                               var_sexgender = var_sexgender)
}

## HAS_TESTS
#' @export
make_matrix_draws_svd.bage_prior_svd_rwrandom <- function(prior,
                                                          dimnames_term,
                                                          var_time,
                                                          var_age,
                                                          var_sexgender) {
  make_matrix_draws_svd_nozero(prior = prior,
                               dimnames_term = dimnames_term,
                               var_time = var_time,
                               var_age = var_age,
                               var_sexgender = var_sexgender)
}

## HAS_TESTS
#' @export
make_matrix_draws_svd.bage_prior_svd_rwzero <- function(prior,
                                                    dimnames_term,
                                                    var_time,
                                                    var_age,
                                                    var_sexgender) {
  make_matrix_draws_svd_appendzero(prior = prior,
                                   dimnames_term = dimnames_term,
                                   var_time = var_time,
                                   var_age = var_age,
                                   var_sexgender = var_sexgender)
}

## HAS_TESTS
#' @export
make_matrix_draws_svd.bage_prior_svd_rw2random <- function(prior,
                                                           dimnames_term,
                                                           var_time,
                                                           var_age,
                                                           var_sexgender) {
  make_matrix_draws_svd_nozero(prior = prior,
                               dimnames_term = dimnames_term,
                               var_time = var_time,
                               var_age = var_age,
                               var_sexgender = var_sexgender)
}

## HAS_TESTS
#' @export
make_matrix_draws_svd.bage_prior_svd_rw2zero <- function(prior,
                                                     dimnames_term,
                                                     var_time,
                                                     var_age,
                                                     var_sexgender) {
  make_matrix_draws_svd_appendzero(prior = prior,
                                   dimnames_term = dimnames_term,
                                   var_time = var_time,
                                   var_age = var_age,
                                   var_sexgender = var_sexgender)
}


## 'make_matrix_effectfree_effect' --------------------------------------------

#' Make matrix mapping effectfree to effect
#'
#' Make matrices mapping free parameters
#' for main effects or interactions to
#' full parameter vectors
#' 
#' @param prior Object of class 'bage_prior'
#' @param dimnames_term Dimnames of array representation of term
#' @param var_time Name of time variable
#' @param var_age Name of age variable
#' @param var_sexgender Name of sex/gender variable
#'
#' @returns A sparse matrix.
#'
#' @noRd
make_matrix_effectfree_effect <- function(prior,
                                          dimnames_term,
                                          var_time,
                                          var_age,
                                          var_sexgender) {
  UseMethod("make_matrix_effectfree_effect")
}

## HAS_TESTS
#' @export
make_matrix_effectfree_effect.bage_prior <- function(prior,
                                                     dimnames_term,
                                                     var_time,
                                                     var_age,
                                                     var_sexgender) {
  n <- prod(lengths(dimnames_term))
  Matrix::.sparseDiagonal(n)
}

## HAS_TESTS
#' @export
make_matrix_effectfree_effect.bage_prior_ar <- function(prior,
                                                        dimnames_term,
                                                        var_time,
                                                        var_age,
                                                        var_sexgender) {
  make_matrix_effectfree_effect_inner(prior = prior,
                                      dimnames_term = dimnames_term,
                                      var_time = var_time,
                                      var_age = var_age,
                                      var_sexgender = var_sexgender,
                                      append_zero = FALSE)
}

## HAS_TESTS
#' @export
make_matrix_effectfree_effect.bage_prior_lin <- function(prior,
                                                         dimnames_term,
                                                         var_time,
                                                         var_age,
                                                         var_sexgender) {
  make_matrix_effectfree_effect_inner(prior = prior,
                                      dimnames_term = dimnames_term,
                                      var_time = var_time,
                                      var_age = var_age,
                                      var_sexgender = var_sexgender,
                                      append_zero = FALSE)
}

## HAS_TESTS
#' @export
make_matrix_effectfree_effect.bage_prior_linar <- function(prior,
                                                           dimnames_term,
                                                           var_time,
                                                           var_age,
                                                           var_sexgender) {
  make_matrix_effectfree_effect_inner(prior = prior,
                                      dimnames_term = dimnames_term,
                                      var_time = var_time,
                                      var_age = var_age,
                                      var_sexgender = var_sexgender,
                                      append_zero = FALSE)
}

## HAS_TESTS
#' @export
make_matrix_effectfree_effect.bage_prior_linex <- function(prior,
                                                           dimnames_term,
                                                           var_time,
                                                           var_age,
                                                           var_sexgender) {
  con <- prior$specific$con
  i_along <- make_i_along(prior = prior,
                          dimnames_term = dimnames_term,
                          var_time = var_time,
                          var_age = var_age)
  is_along_first <- i_along == 1L
  dim <- lengths(dimnames_term)
  s <- seq_along(dim)
  n_along <- dim[[i_along]]
  ans <- matrix(seq_len(n_along) - 0.5 * (n_along + 1),
                nrow = n_along,
                ncol = 1L)
  if (con == "by") {
    I <- Matrix::.sparseDiagonal(prod(dim[-i_along] - 1L))
    ans <- Matrix::kronecker(I, ans)
    dim_after <- dim[c(i_along, s[-i_along])]
    m_zero <- make_matrix_unconstr_constr_along(dim_after)
    ans <- m_zero %*% ans
    if (!is_along_first) {
      dim_after <- c(1L, dim[-i_along] - 1L)
      m_to <- make_matrix_perm_along_to_front(i_along = i_along,
                                              dim_after = dim_after)
      m_from <- make_matrix_perm_along_from_front(i_along = i_along,
                                                  dim_after = dim)
      ans <- m_from %*% ans %*% m_to
    }
  }
  else {
    I <- Matrix::.sparseDiagonal(prod(dim[-i_along]))
    ans <- Matrix::kronecker(I, ans)
    if (!is_along_first) {
      dim_after <- c(1L, dim[-i_along])
      m_to <- make_matrix_perm_along_to_front(i_along = i_along,
                                              dim_after = dim_after)
      m_from <- make_matrix_perm_along_from_front(i_along = i_along,
                                                  dim_after = dim)
      ans <- m_from %*% ans %*% m_to
    }
  }
  ans
}

## HAS_TESTS
#' @export
make_matrix_effectfree_effect.bage_prior_rwrandom <- function(prior,
                                                        dimnames_term,
                                                        var_time,
                                                        var_age,
                                                        var_sexgender) {
  make_matrix_effectfree_effect_inner(prior = prior,
                                      dimnames_term = dimnames_term,
                                      var_time = var_time,
                                      var_age = var_age,
                                      var_sexgender = var_sexgender,
                                      append_zero = FALSE)
}

## HAS_TESTS
#' @export
make_matrix_effectfree_effect.bage_prior_rwrandomseasfix <- function(prior,
                                                               dimnames_term,
                                                               var_time,
                                                               var_age,
                                                               var_sexgender) {
  make_matrix_effectfree_effect_inner(prior = prior,
                                      dimnames_term = dimnames_term,
                                      var_time = var_time,
                                      var_age = var_age,
                                      var_sexgender = var_sexgender,
                                      append_zero = FALSE)
}

## HAS_TESTS
#' @export
make_matrix_effectfree_effect.bage_prior_rwrandomseasvary <- function(prior,
                                                                      dimnames_term,
                                                                      var_time,
                                                                      var_age,
                                                                      var_sexgender) {
  make_matrix_effectfree_effect_inner(prior = prior,
                                      dimnames_term = dimnames_term,
                                      var_time = var_time,
                                      var_age = var_age,
                                      var_sexgender = var_sexgender,
                                      append_zero = FALSE)
}

## HAS_TESTS
#' @export
make_matrix_effectfree_effect.bage_prior_rwzero <- function(prior,
                                                        dimnames_term,
                                                        var_time,
                                                        var_age,
                                                        var_sexgender) {
  make_matrix_effectfree_effect_inner(prior = prior,
                                      dimnames_term = dimnames_term,
                                      var_time = var_time,
                                      var_age = var_age,
                                      var_sexgender = var_sexgender,
                                      append_zero = TRUE)
}

## HAS_TESTS
#' @export
make_matrix_effectfree_effect.bage_prior_rwzeroseasfix <- function(prior,
                                                               dimnames_term,
                                                               var_time,
                                                               var_age,
                                                               var_sexgender) {
  make_matrix_effectfree_effect_inner(prior = prior,
                                      dimnames_term = dimnames_term,
                                      var_time = var_time,
                                      var_age = var_age,
                                      var_sexgender = var_sexgender,
                                      append_zero = FALSE)
}

## HAS_TESTS
#' @export
make_matrix_effectfree_effect.bage_prior_rwzeroseasvary <- function(prior,
                                                                dimnames_term,
                                                                var_time,
                                                                var_age,
                                                                var_sexgender) {
  make_matrix_effectfree_effect_inner(prior = prior,
                                      dimnames_term = dimnames_term,
                                      var_time = var_time,
                                      var_age = var_age,
                                      var_sexgender = var_sexgender,
                                      append_zero = FALSE)
}

## HAS_TESTS
#' @export
make_matrix_effectfree_effect.bage_prior_rw2infant <- function(prior,
                                                               dimnames_term,
                                                               var_time,
                                                               var_age,
                                                               var_sexgender) {
  make_matrix_effectfree_effect_inner(prior = prior,
                                      dimnames_term = dimnames_term,
                                      var_time = var_time,
                                      var_age = var_age,
                                      var_sexgender = var_sexgender,
                                      append_zero = FALSE)
}

## HAS_TESTS
#' @export
make_matrix_effectfree_effect.bage_prior_rw2random <- function(prior,
                                                        dimnames_term,
                                                        var_time,
                                                        var_age,
                                                        var_sexgender) {
  make_matrix_effectfree_effect_inner(prior = prior,
                                      dimnames_term = dimnames_term,
                                      var_time = var_time,
                                      var_age = var_age,
                                      var_sexgender = var_sexgender,
                                      append_zero = FALSE)
}

## HAS_TESTS
#' @export
make_matrix_effectfree_effect.bage_prior_rw2randomseasfix <- function(prior,
                                                               dimnames_term,
                                                               var_time,
                                                               var_age,
                                                               var_sexgender) {
  make_matrix_effectfree_effect_inner(prior = prior,
                                      dimnames_term = dimnames_term,
                                      var_time = var_time,
                                      var_age = var_age,
                                      var_sexgender = var_sexgender,
                                      append_zero = FALSE)
}

## HAS_TESTS
#' @export
make_matrix_effectfree_effect.bage_prior_rw2randomseasvary <- function(prior,
                                                                dimnames_term,
                                                                var_time,
                                                                var_age,
                                                                var_sexgender) {
  make_matrix_effectfree_effect_inner(prior = prior,
                                      dimnames_term = dimnames_term,
                                      var_time = var_time,
                                      var_age = var_age,
                                      var_sexgender = var_sexgender,
                                      append_zero = FALSE)
}

## HAS_TESTS
#' @export
make_matrix_effectfree_effect.bage_prior_rw2zero <- function(prior,
                                                        dimnames_term,
                                                        var_time,
                                                        var_age,
                                                        var_sexgender) {
  make_matrix_effectfree_effect_inner(prior = prior,
                                      dimnames_term = dimnames_term,
                                      var_time = var_time,
                                      var_age = var_age,
                                      var_sexgender = var_sexgender,
                                      append_zero = TRUE)
}

## HAS_TESTS
#' @export
make_matrix_effectfree_effect.bage_prior_rw2zeroseasfix <- function(prior,
                                                               dimnames_term,
                                                               var_time,
                                                               var_age,
                                                               var_sexgender) {
  make_matrix_effectfree_effect_inner(prior = prior,
                                      dimnames_term = dimnames_term,
                                      var_time = var_time,
                                      var_age = var_age,
                                      var_sexgender = var_sexgender,
                                      append_zero = FALSE)
}

## HAS_TESTS
#' @export
make_matrix_effectfree_effect.bage_prior_rw2zeroseasvary <- function(prior,
                                                                dimnames_term,
                                                                var_time,
                                                                var_age,
                                                                var_sexgender) {
  make_matrix_effectfree_effect_inner(prior = prior,
                                      dimnames_term = dimnames_term,
                                      var_time = var_time,
                                      var_age = var_age,
                                      var_sexgender = var_sexgender,
                                      append_zero = FALSE)
}

## HAS_TESTS
#' @export
make_matrix_effectfree_effect.bage_prior_spline <- function(prior,
                                                            dimnames_term,
                                                            var_time,
                                                            var_age,
                                                            var_sexgender) {
  make_matrix_effectfree_effect_inner(prior = prior,
                                      dimnames_term = dimnames_term,
                                      var_time = var_time,
                                      var_age = var_age,
                                      var_sexgender = var_sexgender,
                                      append_zero = FALSE)
}

## HAS_TESTS
#' @export
make_matrix_effectfree_effect.bage_prior_svd <- function(prior,
                                                         dimnames_term,
                                                         var_time,
                                                         var_age,
                                                         var_sexgender) {
  make_matrix_effectfree_effect_inner(prior = prior,
                                      dimnames_term = dimnames_term,
                                      var_time = var_time,
                                      var_age = var_age,
                                      var_sexgender = var_sexgender,
                                      append_zero = FALSE)
}

## HAS_TESTS
#' @export
make_matrix_effectfree_effect.bage_prior_svd_ar <- function(prior,
                                                            dimnames_term,
                                                            var_time,
                                                            var_age,
                                                            var_sexgender) {
  make_matrix_effectfree_effect_inner(prior = prior,
                                      dimnames_term = dimnames_term,
                                      var_time = var_time,
                                      var_age = var_age,
                                      var_sexgender = var_sexgender,
                                      append_zero = FALSE)
}

## HAS_TESTS
#' @export
make_matrix_effectfree_effect.bage_prior_svd_rwrandom <- function(prior,
                                                            dimnames_term,
                                                            var_time,
                                                            var_age,
                                                            var_sexgender) {
  make_matrix_effectfree_effect_inner(prior = prior,
                                      dimnames_term = dimnames_term,
                                      var_time = var_time,
                                      var_age = var_age,
                                      var_sexgender = var_sexgender,
                                      append_zero = FALSE)
}

## HAS_TESTS
#' @export
make_matrix_effectfree_effect.bage_prior_svd_rwzero <- function(prior,
                                                            dimnames_term,
                                                            var_time,
                                                            var_age,
                                                            var_sexgender) {
  make_matrix_effectfree_effect_inner(prior = prior,
                                      dimnames_term = dimnames_term,
                                      var_time = var_time,
                                      var_age = var_age,
                                      var_sexgender = var_sexgender,
                                      append_zero = TRUE)
}

## HAS_TESTS
#' @export
make_matrix_effectfree_effect.bage_prior_svd_rw2random <- function(prior,
                                                                   dimnames_term,
                                                                   var_time,
                                                                   var_age,
                                                                   var_sexgender) {
  make_matrix_effectfree_effect_inner(prior = prior,
                                      dimnames_term = dimnames_term,
                                      var_time = var_time,
                                      var_age = var_age,
                                      var_sexgender = var_sexgender,
                                      append_zero = FALSE)
}

## HAS_TESTS
#' @export
make_matrix_effectfree_effect.bage_prior_svd_rw2zero <- function(prior,
                                                             dimnames_term,
                                                             var_time,
                                                             var_age,
                                                             var_sexgender) {
  make_matrix_effectfree_effect_inner(prior = prior,
                                      dimnames_term = dimnames_term,
                                      var_time = var_time,
                                      var_age = var_age,
                                      var_sexgender = var_sexgender,
                                      append_zero = TRUE)
}


## 'make_matrix_sub_orig' -----------------------------------------------------

#' Make Matrix from Subspace to Original Space
#'
#' Used with spline and SVD priors
#'
#' @param prior Object of class 'bage_prior'
#' @param dimnames_term Dimnames of array representation of term
#' @param var_time Name of time variable
#' @param var_age Name of age variable
#' @param var_sexgender Name of sex/gender variable
#' @param dim_after Dimension after applying the
#' transform (which is not necessarily the same as the dimension
#' implied by 'dimnames_term')
#'
#' @returns A sparse matrix or NULL
#'
#' @noRd
make_matrix_sub_orig <- function(prior,
                                 dimnames_term,
                                 var_time,
                                 var_age,
                                 var_sexgender,
                                 dim_after) {
  UseMethod("make_matrix_sub_orig")
}

## HAS_TESTS
#' @export
make_matrix_sub_orig.bage_prior <- function(prior,
                                            dimnames_term,
                                            var_time,
                                            var_age,
                                            var_sexgender,
                                            dim_after) {
  NULL
}

## HAS_TESTS
## Note that the spline is always along the "along"
## dimension, so is never subject to the "by" constraint
#' @export
make_matrix_sub_orig.bage_prior_spline <- function(prior,
                                                   dimnames_term,
                                                   var_time,
                                                   var_age,
                                                   var_sexgender,
                                                   dim_after) {
  i_along <- make_i_along(prior = prior,
                          dimnames_term = dimnames_term,
                          var_time = var_time,
                          var_age = var_age)
  n_along <- dim_after[[i_along]]
  n_by <- prod(dim_after[-i_along])
  n_comp <- get_n_comp_spline(prior = prior, n_along = n_along)
  ans <- make_matrix_spline(n_comp = n_comp, n_along = n_along)
  I <- Matrix::.sparseDiagonal(n_by)
  ans <- Matrix::kronecker(I, ans)
  is_along_first <- i_along == 1L
  if (!is_along_first) {
    m_from <- make_matrix_perm_along_from_front(i_along = i_along,
                                                dim_after = dim_after)
    dim_after <- c(n_comp, dim_after[-i_along])
    m_to <- make_matrix_perm_along_to_front(i_along = i_along,
                                            dim_after = dim_after)
    ans <- m_from %*% ans %*% m_to
  }
  ans
}

## HAS_TESTS
#' @export
make_matrix_sub_orig.bage_prior_svd <- function(prior,
                                                dimnames_term,
                                                var_time,
                                                var_age,
                                                var_sexgender,
                                                dim_after) {
  make_matrix_sub_orig_svd(prior = prior,
                           dimnames_term = dimnames_term,
                           var_age = var_age,
                           var_sexgender = var_sexgender,
                           dim_after = dim_after,
                           con = "none")
}

## HAS_TESTS
#' @export
make_matrix_sub_orig.bage_prior_svd_ar <- function(prior,
                                                   dimnames_term,
                                                   var_time,
                                                   var_age,
                                                   var_sexgender,
                                                   dim_after) {
  con <- prior$specific$con
  make_matrix_sub_orig_svd(prior = prior,
                           dimnames_term = dimnames_term,
                           var_age = var_age,
                           var_sexgender = var_sexgender,
                           dim_after = dim_after,
                           con = con)
}

## HAS_TESTS
#' @export
make_matrix_sub_orig.bage_prior_svd_rwrandom <- function(prior,
                                                         dimnames_term,
                                                         var_time,
                                                         var_age,
                                                         var_sexgender,
                                                         dim_after) {
  con <- prior$specific$con
  make_matrix_sub_orig_svd(prior = prior,
                           dimnames_term = dimnames_term,
                           var_age = var_age,
                           var_sexgender = var_sexgender,
                           dim_after = dim_after,
                           con = con)
}

## HAS_TESTS
#' @export
make_matrix_sub_orig.bage_prior_svd_rwzero <- function(prior,
                                                         dimnames_term,
                                                         var_time,
                                                         var_age,
                                                         var_sexgender,
                                                         dim_after) {
  con <- prior$specific$con
  make_matrix_sub_orig_svd(prior = prior,
                           dimnames_term = dimnames_term,
                           var_age = var_age,
                           var_sexgender = var_sexgender,
                           dim_after = dim_after,
                           con = con)
}

## HAS_TESTS
#' @export
make_matrix_sub_orig.bage_prior_svd_rw2random <- function(prior,
                                                          dimnames_term,
                                                          var_time,
                                                          var_age,
                                                          var_sexgender,
                                                          dim_after) {
  con <- prior$specific$con
  make_matrix_sub_orig_svd(prior = prior,
                           dimnames_term = dimnames_term,
                           var_age = var_age,
                           var_sexgender = var_sexgender,
                           dim_after = dim_after,
                           con = con)
}

## HAS_TESTS
#' @export
make_matrix_sub_orig.bage_prior_svd_rw2zero <- function(prior,
                                                        dimnames_term,
                                                        var_time,
                                                        var_age,
                                                        var_sexgender,
                                                        dim_after) {
  con <- prior$specific$con
  make_matrix_sub_orig_svd(prior = prior,
                           dimnames_term = dimnames_term,
                           var_age = var_age,
                           var_sexgender = var_sexgender,
                           dim_after = dim_after,
                           con = con)
}



## 'make_offset_effectfree_effect' --------------------------------------------------

#' Make offset used in converting effectfree to effect
#'
#' Make offset used in converting
#' free parameters
#' for main effects or interactions to
#' full parameter vectors
#' 
#' @param prior Object of class 'bage_prior'
#' @param dimnames_term Dimnames of array representation of term
#' @param var_time Name of time variable
#' @param var_age Name of age variable
#' @param var_sexgender Name of sex/gender variable
#'
#' @returns A named numeric vector.
#'
#' @noRd
make_offset_effectfree_effect <- function(prior,
                                          dimnames_term,
                                          var_time,
                                          var_age,
                                          var_sexgender) {
  UseMethod("make_offset_effectfree_effect")
}

## HAS_TESTS
#' @export
make_offset_effectfree_effect.bage_prior <- function(prior,
                                                     dimnames_term,
                                                     var_time,
                                                     var_age,
                                                     var_sexgender) {
  n <- prod(lengths(dimnames_term))
  rep.int(0, times = n)
}

## HAS_TESTS
#' @export
make_offset_effectfree_effect.bage_prior_svd <- function(prior,
                                                         dimnames_term,
                                                         var_time,
                                                         var_age,
                                                         var_sexgender)
  make_offset_effectfree_effect_svd(prior = prior,
                                    dimnames_term = dimnames_term,
                                    var_time = var_time,
                                    var_age = var_age,
                                    var_sexgender = var_sexgender)


## HAS_TESTS
#' @export
make_offset_effectfree_effect.bage_prior_svd_ar <- function(prior,
                                                            dimnames_term,
                                                            var_time,
                                                            var_age,
                                                            var_sexgender) {
  make_offset_effectfree_effect_svd(prior = prior,
                                    dimnames_term = dimnames_term,
                                    var_time = var_time,
                                    var_age = var_age,
                                    var_sexgender = var_sexgender)
}

## HAS_TESTS
#' @export
make_offset_effectfree_effect.bage_prior_svd_rwrandom <- function(prior,
                                                            dimnames_term,
                                                            var_time,
                                                            var_age,
                                                            var_sexgender)
  make_offset_effectfree_effect_svd(prior = prior,
                                    dimnames_term = dimnames_term,
                                    var_time = var_time,
                                    var_age = var_age,
                                    var_sexgender = var_sexgender)

## HAS_TESTS
#' @export
make_offset_effectfree_effect.bage_prior_svd_rwzero <- function(prior,
                                                                dimnames_term,
                                                                var_time,
                                                                var_age,
                                                                var_sexgender)
  make_offset_effectfree_effect_svd(prior = prior,
                                    dimnames_term = dimnames_term,
                                    var_time = var_time,
                                    var_age = var_age,
                                    var_sexgender = var_sexgender)

## HAS_TESTS
#' @export
make_offset_effectfree_effect.bage_prior_svd_rw2random <- function(prior,
                                                                   dimnames_term,
                                                                   var_time,
                                                                   var_age,
                                                                   var_sexgender)
  make_offset_effectfree_effect_svd(prior = prior,
                                    dimnames_term = dimnames_term,
                                    var_time = var_time,
                                    var_age = var_age,
                                    var_sexgender = var_sexgender)

## HAS_TESTS
#' @export
make_offset_effectfree_effect.bage_prior_svd_rw2random <- function(prior,
                                                                   dimnames_term,
                                                                   var_time,
                                                                   var_age,
                                                                   var_sexgender)
  make_offset_effectfree_effect_svd(prior = prior,
                                    dimnames_term = dimnames_term,
                                    var_time = var_time,
                                    var_age = var_age,
                                    var_sexgender = var_sexgender)

## HAS_TESTS
#' @export
make_offset_effectfree_effect.bage_prior_svd_rw2zero <- function(prior,
                                                                 dimnames_term,
                                                                 var_time,
                                                                 var_age,
                                                                 var_sexgender)
  make_offset_effectfree_effect_svd(prior = prior,
                                    dimnames_term = dimnames_term,
                                    var_time = var_time,
                                    var_age = var_age,
                                    var_sexgender = var_sexgender)


## 'print' --------------------------------------------------------------------

## HAS_TESTS
#' @export
print.bage_prior_ar <- function(x, ...) {
  nms <- c("min", "max", "s", "along", "con")
  slots <- c("min", "max", "scale", "along", "con")
  nm <- x$specific$nm
  if (identical(nm, "AR")) {
    nms <- c("n_coef", nms)
    slots <- c("n_coef", slots)
  }
  print_prior(x, nms = nms, slots = slots)
}

## HAS_TESTS
#' @export
print.bage_prior_known <- function(x, ...) {
  cat(str_call_prior(x), "\n")
}

## HAS_TESTS
#' @export
print.bage_prior_lin <- function(x, ...) {
  print_prior(x,
              nms = c("s", "mean_slope", "sd_slope", "along", "con"),
              slots = c("scale", "mean_slope", "sd_slope", "along", "con"))
}

## HAS_TESTS
#' @export
print.bage_prior_linar <- function(x, ...) {
  nms <- c("s", "mean_slope", "sd_slope", "min", "max", "along", "con")
  slots <- c("scale", "mean_slope", "sd_slope", "min", "max", "along", "con")
  nm <- x$specific$nm
  if (identical(nm, "Lin_AR")) {
    nms <- c("n_coef", nms)
    slots <- c("n_coef", slots)
  }
  print_prior(x, nms = nms, slots = slots)
}


## HAS_TESTS
#' @export
print.bage_prior_linex <- function(x, ...) {
  n_offset <- get_print_prior_n_offset()
  print_prior_header(x)
  cat(sprintf("% *s: %s\n", n_offset, "s", 0))
  print_prior_slot(prior = x, nm = "mean_slope", slot = "mean_slope")
  print_prior_slot(prior = x, nm = "sd_slope", slot = "sd_slope")
  print_prior_slot(prior = x, nm = "along", slot = "along")
  print_prior_slot(prior = x, nm = "con", slot = "con")
  invisible(x)
}


## HAS_TESTS
#' @export
print.bage_prior_norm <- function(x, ...) {
  print_prior(x,
              nms = "s",
              slots = "scale")
}

## HAS_TESTS
#' @export
print.bage_prior_normfixed <- function(x, ...) {
  print_prior(x,
              nms = "sd",
              slots = "sd")
}

## HAS_TESTS
#' @export
print.bage_prior_rwrandom <- function(x, ...) {
  print_prior(x,
              nms = c("s", "sd", "along", "con"),
              slots = c("scale", "sd", "along", "con"))
}

## HAS_TESTS
#' @export
print.bage_prior_rwrandomseasfix <- function(x, ...) {
  n_offset <- get_print_prior_n_offset()
  print_prior_header(x)
  print_prior_slot(prior = x, nm = "n_seas", slot = "n_seas")
  print_prior_slot(prior = x, nm = "s", slot = "scale")
  print_prior_slot(prior = x, nm = "sd", slot = "sd")
  cat(sprintf("% *s: %s\n", n_offset, "s_seas", 0))
  print_prior_slot(prior = x, nm = "sd_seas", slot = "sd_seas")
  print_prior_slot(prior = x, nm = "along", slot = "along")
  print_prior_slot(prior = x, nm = "con", slot = "con")
  invisible(x)
}

## HAS_TESTS
#' @export
print.bage_prior_rwrandomseasvary <- function(x, ...) {
  print_prior(x,
              nms = c("n", "s", "sd", "s_seas", "sd_seas", "along", "con"),
              slots = c("n", "scale", "sd", "scale_seas", "sd_seas", "along", "con"))
}

## HAS_TESTS
#' @export
print.bage_prior_rwzero <- function(x, ...) {
  n_offset <- get_print_prior_n_offset()
  print_prior_header(x)
  print_prior_slot(prior = x, nm = "s", slot = "scale")
  cat(sprintf("% *s: %s\n", n_offset, "sd", 0))
  print_prior_slot(prior = x, nm = "along", slot = "along")
  print_prior_slot(prior = x, nm = "con", slot = "con")
  invisible(x)
}

## HAS_TESTS
#' @export
print.bage_prior_rwzeroseasfix <- function(x, ...) {
  n_offset <- get_print_prior_n_offset()
  print_prior_header(x)
  print_prior_slot(prior = x, nm = "n_seas", slot = "n_seas")
  print_prior_slot(prior = x, nm = "s", slot = "scale")
  cat(sprintf("% *s: %s\n", n_offset, "sd", 0))
  cat(sprintf("% *s: %s\n", n_offset, "s_seas", 0))
  print_prior_slot(prior = x, nm = "sd_seas", slot = "sd_seas")
  print_prior_slot(prior = x, nm = "along", slot = "along")
  print_prior_slot(prior = x, nm = "con", slot = "con")
  invisible(x)
}

## HAS_TESTS
#' @export
print.bage_prior_rwzeroseasvary <- function(x, ...) {
  n_offset <- get_print_prior_n_offset()
  print_prior_header(x)
  print_prior_slot(prior = x, nm = "n_seas", slot = "n_seas")
  print_prior_slot(prior = x, nm = "s", slot = "scale")
  cat(sprintf("% *s: %s\n", n_offset, "sd", 0))
  print_prior_slot(prior = x, nm = "s_seas", slot = "scale_seas")
  print_prior_slot(prior = x, nm = "sd_seas", slot = "sd_seas")
  print_prior_slot(prior = x, nm = "along", slot = "along")
  print_prior_slot(prior = x, nm = "con", slot = "con")
  invisible(x)
}

## HAS_TESTS
#' @export
print.bage_prior_rw2infant <- function(x, ...) {
  print_prior(x,
              nms = c("s", "sd_slope", "con"),
              slots = c("scale", "sd_slope", "con"))
}

## HAS_TESTS
#' @export
print.bage_prior_rw2random <- function(x, ...) {
  print_prior(x,
              nms = c("s", "sd", "sd_slope", "along", "con"),
              slots = c("scale", "sd", "sd_slope", "along", "con"))
}

## HAS_TESTS
#' @export
print.bage_prior_rw2randomseasfix <- function(x, ...) {
  n_offset <- get_print_prior_n_offset()
  print_prior_header(x)
  print_prior_slot(prior = x, nm = "n_seas", slot = "n_seas")
  print_prior_slot(prior = x, nm = "s", slot = "scale")
  print_prior_slot(prior = x, nm = "sd", slot = "sd")
  print_prior_slot(prior = x, nm = "sd_slope", slot = "sd_slope")
  cat(sprintf("% *s: %s\n", n_offset, "s_seas", 0))
  print_prior_slot(prior = x, nm = "sd_seas", slot = "sd_seas")
  print_prior_slot(prior = x, nm = "along", slot = "along")
  print_prior_slot(prior = x, nm = "con", slot = "con")
  invisible(x)
}

## HAS_TESTS
#' @export
print.bage_prior_rw2randomseasvary <- function(x, ...) {
  print_prior(x,
              nms = c("n_seas",
                      "s", "sd", "sd_slope",
                      "s_seas", "sd_seas",
                      "along", "con"),
              slots = c("n_seas",
                        "scale", "sd", "sd_slope",
                        "scale_seas", "sd_seas",
                        "along", "con"))
}

## HAS_TESTS
#' @export
print.bage_prior_rw2zero <- function(x, ...) {
  n_offset <- get_print_prior_n_offset()
  print_prior_header(x)
  print_prior_slot(prior = x, nm = "s", slot = "scale")
  cat(sprintf("% *s: %s\n", n_offset, "sd", 0))
  print_prior_slot(prior = x, nm = "sd_slope", slot = "sd_slope")
  print_prior_slot(prior = x, nm = "along", slot = "along")
  print_prior_slot(prior = x, nm = "con", slot = "con")
  invisible(x)
}

## HAS_TESTS
#' @export
print.bage_prior_rw2zeroseasfix <- function(x, ...) {
  n_offset <- get_print_prior_n_offset()
  print_prior_header(x)
  print_prior_slot(prior = x, nm = "n_seas", slot = "n_seas")
  print_prior_slot(prior = x, nm = "s", slot = "scale")
  cat(sprintf("% *s: %s\n", n_offset, "sd", 0))
  print_prior_slot(prior = x, nm = "sd_slope", slot = "sd_slope")
  cat(sprintf("% *s: %s\n", n_offset, "s_seas", 0))
  print_prior_slot(prior = x, nm = "sd_seas", slot = "sd_seas")
  print_prior_slot(prior = x, nm = "along", slot = "along")
  print_prior_slot(prior = x, nm = "con", slot = "con")
  invisible(x)
}

## HAS_TESTS
#' @export
print.bage_prior_rw2zeroseasfix <- function(x, ...) {
  n_offset <- get_print_prior_n_offset()
  print_prior_header(x)
  print_prior_slot(prior = x, nm = "n_seas", slot = "n_seas")
  print_prior_slot(prior = x, nm = "s", slot = "scale")
  cat(sprintf("% *s: %s\n", n_offset, "sd", 0))
  print_prior_slot(prior = x, nm = "sd_slope", slot = "sd_slope")
  print_prior_slot(prior = x, nm = "s_seas", slot = "scale_seas")
  print_prior_slot(prior = x, nm = "sd_seas", slot = "sd_seas")
  print_prior_slot(prior = x, nm = "along", slot = "along")
  print_prior_slot(prior = x, nm = "con", slot = "con")
  invisible(x)
}

## HAS_TESTS
#' @export
print.bage_prior_rw2zeroseasvary <- function(x, ...) {
  print_prior(x,
              nms = c("n_seas",
                      "s", "sd", "sd_slope",
                      "s_seas", "sd_seas",
                      "along", "con"),
              slots = c("n_seas",
                        "scale", "sd", "sd_slope",
                        "scale_seas", "sd_seas",
                        "along", "con"))
}

## HAS_TESTS
#' @export
print.bage_prior_spline <- function(x, ...) {
  print_prior(x,
              nms = c("n_comp", "s", "sd", "sd_slope", "along", "con"),
              slots = c("n_comp", "scale", "sd", "sd_slope", "along", "con"))
}

## HAS_TESTS
#' @export
print.bage_prior_svd <- function(x, ...) {
  n_offset <- get_print_prior_n_offset()
  print_prior_header(x)
  print_prior_slot(prior = x, nm = "ssvd", slot = "nm_ssvd")
  print_prior_slot(prior = x, nm = "n_comp", slot = "n_comp")
  indep <- x$specific$indep
  if (!indep)
    print_prior_slot(prior = x, nm = "indep", slot = "indep")
  invisible(x)
}

## HAS_TESTS
#' @export
print.bage_prior_svd_ar <- function(x, ...) {
  n_offset <- get_print_prior_n_offset()
  print_prior_header(x)
  print_prior_slot(prior = x, nm = "ssvd", slot = "nm_ssvd")
  print_prior_slot(prior = x, nm = "n_comp", slot = "n_comp")
  indep <- x$specific$indep
  if (!indep)
    print_prior_slot(prior = x, nm = "indep", slot = "indep")
  nm <- x$specific$nm
  is_ar <- identical(sub("^(.*)_(.*)$", "\\2", nm), "AR")
  if (is_ar)
    print_prior_slot(prior = x, nm = "n_coef", slot = "n_coef")
  print_prior_slot(prior = x, nm = "min", slot = "min")
  print_prior_slot(prior = x, nm = "max", slot = "max")
  print_prior_slot(prior = x, nm = "s", slot = "scale")
  print_prior_slot(prior = x, nm = "along", slot = "along")
  print_prior_slot(prior = x, nm = "con", slot = "con")
  invisible(x)
}

## HAS_TESTS
#' @export
print.bage_prior_svd_rwrandom <- function(x, ...) {
  n_offset <- get_print_prior_n_offset()
  print_prior_header(x)
  print_prior_slot(prior = x, nm = "ssvd", slot = "nm_ssvd")
  print_prior_slot(prior = x, nm = "n_comp", slot = "n_comp")
  indep <- x$specific$indep
  if (!indep)
    print_prior_slot(prior = x, nm = "indep", slot = "indep")
  print_prior_slot(prior = x, nm = "s", slot = "scale")
  print_prior_slot(prior = x, nm = "sd", slot = "sd")
  print_prior_slot(prior = x, nm = "along", slot = "along")
  print_prior_slot(prior = x, nm = "con", slot = "con")
  invisible(x)
}

## HAS_TESTS
#' @export
print.bage_prior_svd_rwzero <- function(x, ...) {
  n_offset <- get_print_prior_n_offset()
  print_prior_header(x)
  print_prior_slot(prior = x, nm = "ssvd", slot = "nm_ssvd")
  print_prior_slot(prior = x, nm = "n_comp", slot = "n_comp")
  indep <- x$specific$indep
  if (!indep)
    print_prior_slot(prior = x, nm = "indep", slot = "indep")
  print_prior_slot(prior = x, nm = "s", slot = "scale")
  print_prior_slot(prior = x, nm = "along", slot = "along")
  print_prior_slot(prior = x, nm = "con", slot = "con")
  invisible(x)
}

## HAS_TESTS
#' @export
print.bage_prior_svd_rw2random <- function(x, ...) {
  n_offset <- get_print_prior_n_offset()
  print_prior_header(x)
  print_prior_slot(prior = x, nm = "ssvd", slot = "nm_ssvd")
  print_prior_slot(prior = x, nm = "n_comp", slot = "n_comp")
  indep <- x$specific$indep
  if (!indep)
    print_prior_slot(prior = x, nm = "indep", slot = "indep")
  print_prior_slot(prior = x, nm = "s", slot = "scale")
  print_prior_slot(prior = x, nm = "sd", slot = "sd")
  print_prior_slot(prior = x, nm = "sd_slope", slot = "sd_slope")
  print_prior_slot(prior = x, nm = "along", slot = "along")
  print_prior_slot(prior = x, nm = "con", slot = "con")
  invisible(x)
}

## HAS_TESTS
#' @export
print.bage_prior_svd_rw2zero <- function(x, ...) {
  n_offset <- get_print_prior_n_offset()
  print_prior_header(x)
  print_prior_slot(prior = x, nm = "ssvd", slot = "nm_ssvd")
  print_prior_slot(prior = x, nm = "n_comp", slot = "n_comp")
  indep <- x$specific$indep
  if (!indep)
    print_prior_slot(prior = x, nm = "indep", slot = "indep")
  print_prior_slot(prior = x, nm = "s", slot = "scale")
  print_prior_slot(prior = x, nm = "sd_slope", slot = "sd_slope")
  print_prior_slot(prior = x, nm = "along", slot = "along")
  print_prior_slot(prior = x, nm = "con", slot = "con")
  invisible(x)
}


## 'str_call_prior' -----------------------------------------------------------

#' Create string describing prior
#'
#' Creates string describing prior that
#' (inspired by printing of objects in Python)
#' looks like a call to the constructor function
#'
#' @param prior An object of class "bage_prior"
#'
#' @returns A string
#'
#' @noRd
str_call_prior <- function(prior) {
  UseMethod("str_call_prior")
}

## HAS_TESTS
#' @export
str_call_prior.bage_prior_ar <- function(prior) {
  nm <- prior$specific$nm
  args_ar <- str_call_args_ar(prior)
  args_along <- str_call_args_along(prior)
  args_con <- str_call_args_con(prior)
  args <- c(args_ar, args_along, args_con)
  args <- args[nzchar(args)]
  args <- paste(args, collapse = ",")
  sprintf("%s(%s)", nm, args)
}

## HAS_TESTS
#' @export
str_call_prior.bage_prior_known <- function(prior) {
  values <- values_known(prior)
  n <- length(values)
  if (n == 1L)
    inner <- sprintf("%s", values)
  else if (n <= 5)
    inner <- sprintf("c(%s)", paste(values, collapse = ","))
  else
    inner <- sprintf("c(%s,...,%s)", values[[1L]], values[[n]])
  sprintf("Known(%s)", inner)
}

## HAS_TESTS
#' @export
str_call_prior.bage_prior_lin <- function(prior) {
  args_scale <- str_call_args_scale(prior)
  args_lin <- str_call_args_lin(prior)
  args_along <- str_call_args_along(prior)
  args_con <- str_call_args_con(prior)
  args <- c(args_scale, args_lin, args_along, args_con)
  args <- args[nzchar(args)]
  args <- paste(args, collapse = ",")
  sprintf("Lin(%s)", args)
}

## HAS_TESTS
#' @export
str_call_prior.bage_prior_linar <- function(prior) {
  nm <- prior$specific$nm
  args_ar <- str_call_args_ar(prior)
  args_lin <- str_call_args_lin(prior)
  args_along <- str_call_args_along(prior)
  args_con <- str_call_args_con(prior)
  args <- c(args_ar, args_lin, args_along, args_con)
  args <- args[nzchar(args)]
  args <- paste(args, collapse = ",")
  sprintf("%s(%s)", nm, args)
}

## HAS_TESTS
#' @export
str_call_prior.bage_prior_linex <- function(prior) {
  args_scale <- "s=0"
  args_lin <- str_call_args_lin(prior)
  args_along <- str_call_args_along(prior)
  args_con <- str_call_args_con(prior)
  args <- c(args_scale, args_lin, args_along, args_con)
  args <- args[nzchar(args)]
  args <- paste(args, collapse = ",")
  sprintf("Lin(%s)", args)
}

## HAS_TESTS
#' @export
str_call_prior.bage_prior_norm <- function(prior) {
  args <- str_call_args_scale(prior)
  args <- args[nzchar(args)]
  args <- paste(args, collapse = ",")
  sprintf("N(%s)", args)
}

## HAS_TESTS
#' @export
str_call_prior.bage_prior_normfixed <- function(prior) {
  arg <- str_call_args_sd(prior)
  sprintf("NFix(%s)", arg)
}

## HAS_TESTS
#' @export
str_call_prior.bage_prior_rwrandom <- function(prior) {
  args_scale <- str_call_args_scale(prior)
  args_sd <- str_call_args_sd(prior)
  args_along <- str_call_args_along(prior)
  args_con <- str_call_args_con(prior)
  args <- c(args_scale, args_sd, args_along, args_con)
  args <- args[nzchar(args)]
  args <- paste(args, collapse = ",")
  sprintf("RW(%s)", args)
}

## HAS_TESTS
#' @export
str_call_prior.bage_prior_rwrandomseasfix <- function(prior) {
  args_n_seas <- str_call_args_n_seas(prior)
  args_scale <- str_call_args_scale(prior)
  args_sd <- str_call_args_sd(prior)
  args_sd_seas <- str_call_args_sd_seas(prior)
  args_along <- str_call_args_along(prior)
  args_con <- str_call_args_con(prior)
  args <- c(args_n_seas,
            args_scale,
            args_sd,
            args_sd_seas,
            args_along,
            args_con)
  args <- args[nzchar(args)]
  args <- paste(args, collapse = ",")
  sprintf("RW_Seas(%s)", args)
}

## HAS_TESTS
#' @export
str_call_prior.bage_prior_rwrandomseasvary <- function(prior) {
  args_n_seas <- str_call_args_n_seas(prior)
  args_scale <- str_call_args_scale(prior)
  args_sd <- str_call_args_sd(prior)
  args_s_seas <- str_call_args_s_seas(prior)
  args_sd_seas <- str_call_args_sd_seas(prior)
  args_along <- str_call_args_along(prior)
  args_con <- str_call_args_con(prior)
  args <- c(args_n_seas,
            args_scale,
            args_sd,
            args_s_seas,
            args_sd_seas,
            args_along,
            args_con)
  args <- args[nzchar(args)]
  args <- paste(args, collapse = ",")
  sprintf("RW_Seas(%s)", args)
}

## HAS_TESTS
#' @export
str_call_prior.bage_prior_rwzero <- function(prior) {
  args_scale <- str_call_args_scale(prior)
  args_sd <- "sd=0"
  args_along <- str_call_args_along(prior)
  args_con <- str_call_args_con(prior)
  args <- c(args_scale,
            args_sd,
            args_along,
            args_con)
  args <- args[nzchar(args)]
  args <- paste(args, collapse = ",")
  sprintf("RW(%s)", args)
}

## HAS_TESTS
#' @export
str_call_prior.bage_prior_rwzeroseasfix <- function(prior) {
  args_n_seas <- str_call_args_n_seas(prior)
  args_scale <- str_call_args_scale(prior)
  args_sd <- "sd=0"
  args_sd_seas <- str_call_args_sd_seas(prior)
  args_along <- str_call_args_along(prior)
  args_con <- str_call_args_con(prior)
  args <- c(args_n_seas,
            args_scale,
            args_sd,
            args_sd_seas,
            args_along,
            args_con)
  args <- args[nzchar(args)]
  args <- paste(args, collapse = ",")
  sprintf("RW_Seas(%s)", args)
}

## HAS_TESTS
#' @export
str_call_prior.bage_prior_rwzeroseasvary <- function(prior) {
  args_n_seas <- str_call_args_n_seas(prior)
  args_scale <- str_call_args_scale(prior)
  args_sd <- "sd=0"
  args_s_seas <- str_call_args_s_seas(prior)
  args_sd_seas <- str_call_args_sd_seas(prior)
  args_along <- str_call_args_along(prior)
  args_con <- str_call_args_con(prior)
  args <- c(args_n_seas,
            args_scale,
            args_sd,
            args_s_seas,
            args_sd_seas,
            args_along,
            args_con)
  args <- args[nzchar(args)]
  args <- paste(args, collapse = ",")
  sprintf("RW_Seas(%s)", args)
}

## HAS_TESTS
#' @export
str_call_prior.bage_prior_rw2infant <- function(prior) {
  args_scale <- str_call_args_scale(prior)
  args_sd_slope <- str_call_args_sd_slope(prior)
  args_along <- str_call_args_along(prior)
  args_con <- str_call_args_con(prior)
  args <- c(args_scale,
            args_sd_slope,
            args_con)
  args <- args[nzchar(args)]
  args <- paste(args, collapse = ",")
  sprintf("RW2_Infant(%s)", args)
}

## HAS_TESTS
#' @export
str_call_prior.bage_prior_rw2random <- function(prior) {
  args_scale <- str_call_args_scale(prior)
  args_sd <- str_call_args_sd(prior)
  args_sd_slope <- str_call_args_sd_slope(prior)
  args_along <- str_call_args_along(prior)
  args_con <- str_call_args_con(prior)
  args <- c(args_scale,
            args_sd,
            args_sd_slope,
            args_along,
            args_con)
  args <- args[nzchar(args)]
  args <- paste(args, collapse = ",")
  sprintf("RW2(%s)", args)
}

## HAS_TESTS
#' @export
str_call_prior.bage_prior_rw2randomseasfix <- function(prior) {
  args_n_seas <- str_call_args_n_seas(prior)
  args_scale <- str_call_args_scale(prior)
  args_sd <- str_call_args_sd(prior)
  args_sd_slope <- str_call_args_sd_slope(prior)
  args_sd_seas <- str_call_args_sd_seas(prior)
  args_along <- str_call_args_along(prior)
  args_con <- str_call_args_con(prior)
  args <- c(args_n_seas,
            args_scale,
            args_sd,
            args_sd_slope,
            args_sd_seas,
            args_along,
            args_con)
  args <- args[nzchar(args)]
  args <- paste(args, collapse = ",")
  sprintf("RW2_Seas(%s)", args)
}

## HAS_TESTS
#' @export
str_call_prior.bage_prior_rw2randomseasvary <- function(prior) {
  args_n_seas <- str_call_args_n_seas(prior)
  args_scale <- str_call_args_scale(prior)
  args_sd <- str_call_args_sd(prior)
  args_sd_slope <- str_call_args_sd_slope(prior)
  args_s_seas <- str_call_args_s_seas(prior)
  args_sd_seas <- str_call_args_sd_seas(prior)
  args_along <- str_call_args_along(prior)
  args_con <- str_call_args_con(prior)
  args <- c(args_n_seas,
            args_scale,
            args_sd,
            args_sd_slope,
            args_s_seas,
            args_sd_seas,
            args_along,
            args_con)
  args <- args[nzchar(args)]
  args <- paste(args, collapse = ",")
  sprintf("RW2_Seas(%s)", args)
}

## HAS_TESTS
#' @export
str_call_prior.bage_prior_rw2zero <- function(prior) {
  args_scale <- str_call_args_scale(prior)
  args_sd <- "sd=0"
  args_sd_slope <- str_call_args_sd_slope(prior)
  args_along <- str_call_args_along(prior)
  args_con <- str_call_args_con(prior)
  args <- c(args_scale,
            args_sd,
            args_sd_slope,
            args_along,
            args_con)
  args <- args[nzchar(args)]
  args <- paste(args, collapse = ",")
  sprintf("RW2(%s)", args)
}

## HAS_TESTS
#' @export
str_call_prior.bage_prior_rw2zeroseasfix <- function(prior) {
  args_n_seas <- str_call_args_n_seas(prior)
  args_scale <- str_call_args_scale(prior)
  args_sd <- "sd=0"
  args_sd_slope <- str_call_args_sd_slope(prior)
  args_sd_seas <- str_call_args_sd_seas(prior)
  args_along <- str_call_args_along(prior)
  args_con <- str_call_args_con(prior)
  args <- c(args_n_seas,
            args_scale,
            args_sd,
            args_sd_slope,
            args_sd_seas,
            args_along,
            args_con)
  args <- args[nzchar(args)]
  args <- paste(args, collapse = ",")
  sprintf("RW2_Seas(%s)", args)
}

## HAS_TESTS
#' @export
str_call_prior.bage_prior_rw2zeroseasvary <- function(prior) {
  args_n_seas <- str_call_args_n_seas(prior)
  args_scale <- str_call_args_scale(prior)
  args_sd <- "sd=0"
  args_sd_slope <- str_call_args_sd_slope(prior)
  args_s_seas <- str_call_args_s_seas(prior)
  args_sd_seas <- str_call_args_sd_seas(prior)
  args_along <- str_call_args_along(prior)
  args_con <- str_call_args_con(prior)
  args <- c(args_n_seas,
            args_scale,
            args_sd,
            args_sd_slope,
            args_s_seas,
            args_sd_seas,
            args_along,
            args_con)
  args <- args[nzchar(args)]
  args <- paste(args, collapse = ",")
  sprintf("RW2_Seas(%s)", args)
}

## HAS_TESTS
#' @export
str_call_prior.bage_prior_spline <- function(prior) {
  args_n_comp <- str_call_args_n_comp(prior)
  args_scale <- str_call_args_scale(prior)
  args_sd <- str_call_args_sd(prior)
  args_sd_slope <- str_call_args_sd_slope(prior)
  args_along <- str_call_args_along(prior)
  args_con <- str_call_args_con(prior)
  args <- c(args_n_comp,
            args_scale,
            args_sd,
            args_sd_slope,
            args_along,
            args_con)
  args <- args[nzchar(args)]
  args <- paste(args, collapse = ",")
  sprintf("Sp(%s)", args)
}

## HAS_TESTS
#' @export
str_call_prior.bage_prior_svd <- function(prior) {
  args <- str_call_args_svd(prior)
  args <- args[nzchar(args)]
  args <- paste(args, collapse = ",")
  sprintf("SVD(%s)", args)
}

## HAS_TESTS
#' @export
str_call_prior.bage_prior_svd_ar <- function(prior) {
  nm <- prior$specific$nm
  args_svd <- str_call_args_svd(prior)
  args_ar <- str_call_args_ar(prior)
  args_along <- str_call_args_along(prior)
  args_con <- str_call_args_con(prior)
  args <- c(args_svd,
            args_ar,
            args_along,
            args_con)
  args <- args[nzchar(args)]
  args <- paste(args, collapse = ",")
  sprintf("%s(%s)", nm, args)
}

## HAS_TESTS
#' @export
str_call_prior.bage_prior_svd_rwrandom <- function(prior) {
  args_svd <- str_call_args_svd(prior)
  args_scale <- str_call_args_scale(prior)
  args_sd <- str_call_args_sd(prior)
  args_along <- str_call_args_along(prior)
  args_con <- str_call_args_con(prior)
  args <- c(args_svd,
            args_scale,
            args_sd,
            args_along,
            args_con)
  args <- args[nzchar(args)]
  args <- paste(args, collapse = ",")
  sprintf("SVD_RW(%s)", args)
}

## HAS_TESTS
#' @export
str_call_prior.bage_prior_svd_rwzero <- function(prior) {
  args_svd <- str_call_args_svd(prior)
  args_scale <- str_call_args_scale(prior)
  args_sd <- "sd=0"
  args_along <- str_call_args_along(prior)
  args_con <- str_call_args_con(prior)
  args <- c(args_svd,
            args_scale,
            args_sd,
            args_along,
            args_con)
  args <- args[nzchar(args)]
  args <- paste(args, collapse = ",")
  sprintf("SVD_RW(%s)", args)
}

## HAS_TESTS
#' @export
str_call_prior.bage_prior_svd_rw2random <- function(prior) {
  args_svd <- str_call_args_svd(prior)
  args_scale <- str_call_args_scale(prior)
  args_sd <- str_call_args_sd(prior)
  args_sd_slope <- str_call_args_sd_slope(prior)
  args_along <- str_call_args_along(prior)
  args_con <- str_call_args_con(prior)
  args <- c(args_svd,
            args_scale,
            args_sd,
            args_sd_slope,
            args_along,
            args_con)
  args <- args[nzchar(args)]
  args <- paste(args, collapse = ",")
  sprintf("SVD_RW2(%s)", args)
}

## HAS_TESTS
#' @export
str_call_prior.bage_prior_svd_rw2zero <- function(prior) {
  args_svd <- str_call_args_svd(prior)
  args_scale <- str_call_args_scale(prior)
  args_sd <- "sd=0"
  args_sd_slope <- str_call_args_sd_slope(prior)
  args_along <- str_call_args_along(prior)
  args_con <- str_call_args_con(prior)
  args <- c(args_svd,
            args_scale,
            args_sd,
            args_sd_slope,
            args_along,
            args_con)
  args <- args[nzchar(args)]
  args <- paste(args, collapse = ",")
  sprintf("SVD_RW2(%s)", args)
}


## 'str_nm_prior' -----------------------------------------------------------

#' Create Function Call Describing Prior
#'
#' Creates string describing prior that
#' looks like the constructor function
#'
#' @param prior An object of class "bage_prior"
#'
#' @returns A string
#'
#' @noRd
str_nm_prior <- function(prior) {
  UseMethod("str_nm_prior")
}

## HAS_TESTS
#' @export
str_nm_prior.bage_prior_ar <- function(prior) {
  nm <- prior$specific$nm
  sprintf("%s()", nm)
}

## HAS_TESTS
#' @export
str_nm_prior.bage_prior_known <- function(prior) {
  "Known()"
}

## HAS_TESTS
#' @export
str_nm_prior.bage_prior_lin <- function(prior) {
  "Lin()"
}

## HAS_TESTS
#' @export
str_nm_prior.bage_prior_linar <- function(prior) {
  nm <- prior$specific$nm
  sprintf("%s()", nm)
}

## HAS_TESTS
#' @export
str_nm_prior.bage_prior_linex <- function(prior) {
  "Lin()"
}

## HAS_TESTS
#' @export
str_nm_prior.bage_prior_norm <- function(prior) {
  "N()"
}

## HAS_TESTS
#' @export
str_nm_prior.bage_prior_normfixed <- function(prior) {
  "NFix()"
}

## HAS_TESTS
#' @export
str_nm_prior.bage_prior_rwrandom <- function(prior) {
  "RW()"
}

## HAS_TESTS
#' @export
str_nm_prior.bage_prior_rwrandomseasfix <- function(prior) {
  "RW_Seas()"
}

## HAS_TESTS
#' @export
str_nm_prior.bage_prior_rwrandomseasvary <- function(prior) {
  "RW_Seas()"
}

## HAS_TESTS
#' @export
str_nm_prior.bage_prior_rwzero <- function(prior) {
  "RW()"
}

## HAS_TESTS
#' @export
str_nm_prior.bage_prior_rwzeroseasfix <- function(prior) {
  "RW_Seas()"
}

## HAS_TESTS
#' @export
str_nm_prior.bage_prior_rwzeroseasvary <- function(prior) {
  "RW_Seas()"
}

## HAS_TESTS
#' @export
str_nm_prior.bage_prior_rw2infant <- function(prior) {
  "RW2_Infant()"
}

## HAS_TESTS
#' @export
str_nm_prior.bage_prior_rw2random <- function(prior) {
  "RW2()"
}

## HAS_TESTS
#' @export
str_nm_prior.bage_prior_rw2randomseasfix <- function(prior) {
  "RW2_Seas()"
}

## HAS_TESTS
#' @export
str_nm_prior.bage_prior_rw2randomseasvary <- function(prior) {
  "RW2_Seas()"
}

## HAS_TESTS
#' @export
str_nm_prior.bage_prior_rw2zero <- function(prior) {
  "RW2()"
}

## HAS_TESTS
#' @export
str_nm_prior.bage_prior_rw2zeroseasfix <- function(prior) {
  "RW2_Seas()"
}

## HAS_TESTS
#' @export
str_nm_prior.bage_prior_rw2zeroseasvary <- function(prior) {
  "RW2_Seas()"
}

## HAS_TESTS
#' @export
str_nm_prior.bage_prior_spline <- function(prior) {
  "Sp()"
}

## HAS_TESTS
#' @export
str_nm_prior.bage_prior_svd <- function(prior) {
  "SVD()"
}

## HAS_TESTS
#' @export
str_nm_prior.bage_prior_svd_ar <- function(prior) {
  nm <- prior$specific$nm
  sprintf("%s()", nm)
}

## HAS_TESTS
#' @export
str_nm_prior.bage_prior_svd_rwrandom <- function(prior) {
  "SVD_RW()"
}

## HAS_TESTS
#' @export
str_nm_prior.bage_prior_svd_rwzero <- function(prior) {
  "SVD_RW()"
}

## HAS_TESTS
#' @export
str_nm_prior.bage_prior_svd_rw2random <- function(prior) {
  "SVD_RW2()"
}

## HAS_TESTS
#' @export
str_nm_prior.bage_prior_svd_rw2zero <- function(prior) {
  "SVD_RW2()"
}


## 'transform_hyper' ----------------------------------------------------------

#' Transform to convert working TMB version
#' of parameter back to original units
#'+
#' @param prior An object of class 'bage_prior'.
#'
#' @returns A list of functions.
#'
#' @noRd
transform_hyper <- function(prior) {
  UseMethod("transform_hyper")
}

## HAS_TESTS
#' @export
transform_hyper.bage_prior_ar <- function(prior)
  transform_hyper_ar(prior)

## HAS_TESTS
#' @export
transform_hyper.bage_prior_known <- function(prior)
  list()

## HAS_TESTS
#' @export
transform_hyper.bage_prior_lin <- function(prior)
  list(sd = exp)

## HAS_TESTS
#' @export
transform_hyper.bage_prior_linar <- function(prior)
  transform_hyper_ar(prior)

## HAS_TESTS
#' @export
transform_hyper.bage_prior_linex <- function(prior)
  list()

## HAS_TESTS
#' @export
transform_hyper.bage_prior_norm <- function(prior)
  list(sd = exp)

## HAS_TESTS
#' @export
transform_hyper.bage_prior_normfixed <- function(prior)
  list()

## HAS_TESTS
#' @export
transform_hyper.bage_prior_rwrandom <- function(prior)
  list(sd = exp)

## HAS_TESTS
#' @export
transform_hyper.bage_prior_rwrandomseasfix <- function(prior)
  list(sd = exp)

## HAS_TESTS
#' @export
transform_hyper.bage_prior_rwrandomseasvary <- function(prior)
  list(sd_seas = exp,
       sd = exp)

## HAS_TESTS
#' @export
transform_hyper.bage_prior_rwzero <- function(prior)
  list(sd = exp)

## HAS_TESTS
#' @export
transform_hyper.bage_prior_rwzeroseasfix <- function(prior)
  list(sd = exp)

## HAS_TESTS
#' @export
transform_hyper.bage_prior_rwzeroseasvary <- function(prior)
  list(sd_seas = exp,
       sd = exp)

## HAS_TESTS
#' @export
transform_hyper.bage_prior_rw2infant <- function(prior)
  list(sd = exp)

## HAS_TESTS
#' @export
transform_hyper.bage_prior_rw2random <- function(prior)
  list(sd = exp)

## HAS_TESTS
#' @export
transform_hyper.bage_prior_rw2randomseasfix <- function(prior)
  list(sd = exp)

## HAS_TESTS
#' @export
transform_hyper.bage_prior_rw2randomseasvary <- function(prior)
  list(sd_seas = exp,
       sd = exp)

## HAS_TESTS
#' @export
transform_hyper.bage_prior_rw2zero <- function(prior)
  list(sd = exp)

## HAS_TESTS
#' @export
transform_hyper.bage_prior_rw2zeroseasfix <- function(prior)
  list(sd = exp)

## HAS_TESTS
#' @export
transform_hyper.bage_prior_rw2zeroseasvary <- function(prior)
  list(sd_seas = exp,
       sd = exp)

## HAS_TESTS
#' @export
transform_hyper.bage_prior_spline <- function(prior)
  list(sd = exp)

## HAS_TESTS
#' @export
transform_hyper.bage_prior_svd <- function(prior)
  list()

## HAS_TESTS
#' @export
transform_hyper.bage_prior_svd_ar <- function(prior)
  transform_hyper_ar(prior)

## HAS_TESTS
#' @export
transform_hyper.bage_prior_svd_rwrandom <- function(prior)
  list(sd = exp)

## HAS_TESTS
#' @export
transform_hyper.bage_prior_svd_rwzero <- function(prior)
  list(sd = exp)

## HAS_TESTS
#' @export
transform_hyper.bage_prior_svd_rw2random <- function(prior)
  list(sd = exp)

## HAS_TESTS
#' @export
transform_hyper.bage_prior_svd_rw2zero <- function(prior)
  list(sd = exp)


## 'uses_along' ---------------------------------------------------------------

#' Whether Prior uses an 'along' Dimension
#'
#' @param prior Object of class 'bage_prior'
#'
#' @returns TRUE or FALSE.
#'
#' @noRd
uses_along <- function(prior) {
  UseMethod("uses_along")
}


## HAS_TESTS
#' @export
uses_along.bage_prior_ar <- function(prior) TRUE

## HAS_TESTS
#' @export
uses_along.bage_prior_known <- function(prior) FALSE

## HAS_TESTS
#' @export
uses_along.bage_prior_lin <- function(prior) TRUE

## HAS_TESTS
#' @export
uses_along.bage_prior_linar <- function(prior) TRUE

## HAS_TESTS
#' @export
uses_along.bage_prior_linex <- function(prior) TRUE

## HAS_TESTS
#' @export
uses_along.bage_prior_norm <- function(prior) FALSE

## HAS_TESTS
#' @export
uses_along.bage_prior_normfixed <- function(prior) FALSE

## HAS_TESTS
#' @export
uses_along.bage_prior_rwrandom <- function(prior) TRUE

## HAS_TESTS
#' @export
uses_along.bage_prior_rwrandomseasfix <- function(prior) TRUE

## HAS_TESTS
#' @export
uses_along.bage_prior_rwrandomseasvary <- function(prior) TRUE

## HAS_TESTS
#' @export
uses_along.bage_prior_rwzero <- function(prior) TRUE

## HAS_TESTS
#' @export
uses_along.bage_prior_rwzeroseasfix <- function(prior) TRUE

## HAS_TESTS
#' @export
uses_along.bage_prior_rwzeroseasvary <- function(prior) TRUE

## HAS_TESTS
#' @export
uses_along.bage_prior_rw2infant <- function(prior) TRUE

## HAS_TESTS
#' @export
uses_along.bage_prior_rw2random <- function(prior) TRUE

## HAS_TESTS
#' @export
uses_along.bage_prior_rw2randomseasfix <- function(prior) TRUE

## HAS_TESTS
#' @export
uses_along.bage_prior_rw2randomseasvary <- function(prior) TRUE

## HAS_TESTS
#' @export
uses_along.bage_prior_rw2zero <- function(prior) TRUE

## HAS_TESTS
#' @export
uses_along.bage_prior_rw2zeroseasfix <- function(prior) TRUE

## HAS_TESTS
#' @export
uses_along.bage_prior_rw2zeroseasvary <- function(prior) TRUE

## HAS_TESTS
#' @export
uses_along.bage_prior_spline <- function(prior) TRUE

## HAS_TESTS
#' @export
uses_along.bage_prior_svd <- function(prior) FALSE

## HAS_TESTS
#' @export
uses_along.bage_prior_svd_ar <- function(prior) TRUE

## HAS_TESTS
#' @export
uses_along.bage_prior_svd_rwrandom <- function(prior) TRUE

## HAS_TESTS
#' @export
uses_along.bage_prior_svd_rwzero <- function(prior) TRUE

## HAS_TESTS
#' @export
uses_along.bage_prior_svd_rw2random <- function(prior) TRUE

## HAS_TESTS
#' @export
uses_along.bage_prior_svd_rw2zero <- function(prior) TRUE


## 'uses_hyperrandfree' -------------------------------------------------------

#' Whether Prior Uses Hyper-Paremters that
#' Can Be Treated As Random Effects
#'
#' @param prior Object of class 'bage_prior'
#'
#' @returns TRUE or FALSE.
#'
#' @noRd
uses_hyperrandfree <- function(prior) {
  UseMethod("uses_hyperrandfree")
}

## HAS_TESTS
#' @export
uses_hyperrandfree.bage_prior <- function(prior) FALSE

## HAS_TESTS
#' @export
uses_hyperrandfree.bage_prior_lin <- function(prior) TRUE

## HAS_TESTS
#' @export
uses_hyperrandfree.bage_prior_linar <- function(prior) TRUE

## HAS_TESTS
#' @export
uses_hyperrandfree.bage_prior_rwrandomseasfix <- function(prior) TRUE

## HAS_TESTS
#' @export
uses_hyperrandfree.bage_prior_rwrandomseasvary <- function(prior) TRUE

## HAS_TESTS
#' @export
uses_hyperrandfree.bage_prior_rwzeroseasfix <- function(prior) TRUE

## HAS_TESTS
#' @export
uses_hyperrandfree.bage_prior_rwzeroseasvary <- function(prior) TRUE

## HAS_TESTS
#' @export
uses_hyperrandfree.bage_prior_rw2randomseasfix <- function(prior) TRUE

## HAS_TESTS
#' @export
uses_hyperrandfree.bage_prior_rw2randomseasvary <- function(prior) TRUE

## HAS_TESTS
#' @export
uses_hyperrandfree.bage_prior_rw2zeroseasfix <- function(prior) TRUE

## HAS_TESTS
#' @export
uses_hyperrandfree.bage_prior_rw2zeroseasvary <- function(prior) TRUE


## 'uses_matrix_effectfree_effect' --------------------------------------------

#' Whether prior uses matrix to convert effectfree to effect
#'
#' @param prior Object of class 'bage_prior'
#'
#' @returns TRUE or FALSE.
#'
#' @noRd
uses_matrix_effectfree_effect <- function(prior) {
  UseMethod("uses_matrix_effectfree_effect")
}

## HAS_TESTS
#' @export
uses_matrix_effectfree_effect.bage_prior <- function(prior) FALSE

## HAS_TESTS
#' @export
uses_matrix_effectfree_effect.bage_prior_lin <- function(prior) {
  con <- prior$specific$con
  identical(con, "by")
}

## HAS_TESTS
#' @export
uses_matrix_effectfree_effect.bage_prior_linar <- function(prior) {
  con <- prior$specific$con
  identical(con, "by")
}

## HAS_TESTS
#' @export
uses_matrix_effectfree_effect.bage_prior_linex <- function(prior) TRUE

## HAS_TESTS
#' @export
uses_matrix_effectfree_effect.bage_prior_rwrandom <- function(prior) TRUE

## HAS_TESTS
#' @export
uses_matrix_effectfree_effect.bage_prior_rwrandomseasfix <- function(prior) TRUE

## HAS_TESTS
#' @export
uses_matrix_effectfree_effect.bage_prior_rwrandomseasvary <- function(prior) TRUE

## HAS_TESTS
#' @export
uses_matrix_effectfree_effect.bage_prior_rwzero <- function(prior) TRUE

## HAS_TESTS
#' @export
uses_matrix_effectfree_effect.bage_prior_rwzeroseasfix <- function(prior) TRUE

## HAS_TESTS
#' @export
uses_matrix_effectfree_effect.bage_prior_rwzeroseasvary <- function(prior) TRUE

## HAS_TESTS
#' @export
uses_matrix_effectfree_effect.bage_prior_rw2infant <- function(prior) TRUE

## HAS_TESTS
#' @export
uses_matrix_effectfree_effect.bage_prior_rw2random <- function(prior) TRUE

## HAS_TESTS
#' @export
uses_matrix_effectfree_effect.bage_prior_rw2randomseasfix <- function(prior) TRUE

## HAS_TESTS
#' @export
uses_matrix_effectfree_effect.bage_prior_rw2randomseasvary <- function(prior) TRUE

## HAS_TESTS
#' @export
uses_matrix_effectfree_effect.bage_prior_rw2zero <- function(prior) TRUE

## HAS_TESTS
#' @export
uses_matrix_effectfree_effect.bage_prior_rw2zeroseasfix <- function(prior) TRUE

## HAS_TESTS
#' @export
uses_matrix_effectfree_effect.bage_prior_rw2zeroseasvary <- function(prior) TRUE

## HAS_TESTS
#' @export
uses_matrix_effectfree_effect.bage_prior_spline <- function(prior) TRUE

## HAS_TESTS
#' @export
uses_matrix_effectfree_effect.bage_prior_svd <- function(prior) TRUE

## HAS_TESTS
#' @export
uses_matrix_effectfree_effect.bage_prior_svd_ar <- function(prior) TRUE

## HAS_TESTS
#' @export
uses_matrix_effectfree_effect.bage_prior_svd_rwrandom <- function(prior) TRUE

## HAS_TESTS
#' @export
uses_matrix_effectfree_effect.bage_prior_svd_rwzero <- function(prior) TRUE

## HAS_TESTS
#' @export
uses_matrix_effectfree_effect.bage_prior_svd_rw2random <- function(prior) TRUE

## HAS_TESTS
#' @export
uses_matrix_effectfree_effect.bage_prior_svd_rw2zero <- function(prior) TRUE


## 'uses_offset_effectfree_effect' --------------------------------------------------

#' Whether prior uses offset to convert effectfree to effect
#'
#' @param prior Object of class 'bage_prior'
#'
#' @returns TRUE or FALSE.
#'
#' @noRd
uses_offset_effectfree_effect <- function(prior) {
  UseMethod("uses_offset_effectfree_effect")
}

## HAS_TESTS
#' @export
uses_offset_effectfree_effect.bage_prior <- function(prior) FALSE

## HAS_TESTS
#' @export
uses_offset_effectfree_effect.bage_prior_svd <- function(prior) TRUE

## HAS_TESTS
#' @export
uses_offset_effectfree_effect.bage_prior_svd_ar <- function(prior) TRUE

## HAS_TESTS
#' @export
uses_offset_effectfree_effect.bage_prior_svd_rwrandom <- function(prior) TRUE

## HAS_TESTS
#' @export
uses_offset_effectfree_effect.bage_prior_svd_rwzero <- function(prior) TRUE

## HAS_TESTS
#' @export
uses_offset_effectfree_effect.bage_prior_svd_rw2random <- function(prior) TRUE

## HAS_TESTS
#' @export
uses_offset_effectfree_effect.bage_prior_svd_rw2zero <- function(prior) TRUE


## 'values_known' -------------------------------------------------------------

#' Given a prior that treats a term as known,
#' extract the known values
#'
#' @param An object of class 'bage_prior'
#'
#' @returns A vector of doubles.
#'
#' @noRd
values_known <- function(prior) {
  UseMethod("values_known")
}

## HAS_TESTS
#' @export
values_known.bage_prior_known <- function(prior) prior$specific$values
