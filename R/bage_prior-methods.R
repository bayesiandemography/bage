
## Note that methods require an '@export' tag,
## even when the generic function is not exported
## https://github.com/r-lib/devtools/issues/2293


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
  along <- prior$specific$along
  coef <- vals_hyper$coef
  sd <- vals_hyper$sd
  matrix_along_by_effect <- make_matrix_along_by_effect(along = along,
                                                        dimnames_term = dimnames_term,
                                                        var_time = var_time,
                                                        var_age = var_age)
  levels_effect <- dimnames_to_levels(dimnames_term)
  draw_vals_ar(coef = coef,
               sd = sd,
               matrix_along_by = matrix_along_by_effect,
               levels_effect = levels_effect)
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
  along <- prior$specific$along
  intercept <- vals_hyperrand$intercept
  slope <- vals_hyperrand$slope
  sd <- vals_hyper$sd
  matrix_along_by_effect <- make_matrix_along_by_effect(along = along,
                                                        dimnames_term = dimnames_term,
                                                        var_time = var_time,
                                                        var_age = var_age)
  levels_effect <- dimnames_to_levels(dimnames_term)
  draw_vals_lin(intercept = intercept,
                slope = slope,
                sd = sd,
                matrix_along_by = matrix_along_by_effect,
                labels = levels_effect)
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
  along <- prior$specific$along
  intercept <- vals_hyperrand$intercept
  slope <- vals_hyperrand$slope
  coef <- vals_hyper$coef
  sd <- vals_hyper$sd
  matrix_along_by_effect <- make_matrix_along_by_effect(along = along,
                                                        dimnames_term = dimnames_term,
                                                        var_time = var_time,
                                                        var_age = var_age)
  levels_effect <- dimnames_to_levels(dimnames_term)
  draw_vals_linar(intercept = intercept,
                  slope = slope,
                  sd = sd,
                  coef = coef,
                  matrix_along_by = matrix_along_by_effect,
                  labels = levels_effect)
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
  n_effect <- length(levels_effect)
  n <- n_effect * n_sim
  sd <- rep(sd, each = n_effect)
  ans <- stats::rnorm(n = n, sd = sd)
  ans <- matrix(ans, nrow = n_effect, ncol = n_sim)
  dimnames(ans) <- list(levels_effect, NULL)
  ans
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
draw_vals_effect.bage_prior_rw <- function(prior,
                                           vals_hyper,
                                           vals_hyperrand,
                                           vals_spline,
                                           vals_svd,
                                           dimnames_term,
                                           var_time,
                                           var_age,
                                           var_sexgender,
                                           n_sim) {
  along <- prior$specific$along
  sd <- vals_hyper$sd
  matrix_along_by_effect <- make_matrix_along_by_effect(along = along,
                                                        dimnames_term = dimnames_term,
                                                        var_time = var_time,
                                                        var_age = var_age)
  levels_effect <- dimnames_to_levels(dimnames_term)
  draw_vals_rw(sd = sd,
               matrix_along_by = matrix_along_by_effect,
               levels_effect = levels_effect)
}

## HAS_TESTS
#' @export
draw_vals_effect.bage_prior_rwseasfix <- function(prior,
                                                  vals_hyper,
                                                  vals_hyperrand,
                                                  vals_spline,
                                                  vals_svd,
                                                  dimnames_term,
                                                  var_time,
                                                  var_age,
                                                  var_sexgender,
                                                  n_sim) {
  along <- prior$specific$along
  sd <- vals_hyper$sd
  seas <- vals_hyperrand$seas
  matrix_along_by_effect <- make_matrix_along_by_effect(along = along,
                                                        dimnames_term = dimnames_term,
                                                        var_time = var_time,
                                                        var_age = var_age)
  levels_effect <- dimnames_to_levels(dimnames_term)
  alpha <- draw_vals_rw(sd = sd,
                        matrix_along_by = matrix_along_by_effect,
                        levels_effect = levels_effect)
  alpha + seas  
}

## HAS_TESTS
#' @export
draw_vals_effect.bage_prior_rwseasvary <- function(prior,
                                                   vals_hyper,
                                                   vals_hyperrand,
                                                   vals_spline,
                                                   vals_svd,
                                                   dimnames_term,
                                                   var_time,
                                                   var_age,
                                                   var_sexgender,
                                                   n_sim) {
  along <- prior$specific$along
  sd <- vals_hyper$sd
  seas <- vals_hyperrand$seas
  matrix_along_by_effect <- make_matrix_along_by_effect(along = along,
                                                        dimnames_term = dimnames_term,
                                                        var_time = var_time,
                                                        var_age = var_age)
  levels_effect <- dimnames_to_levels(dimnames_term)
  alpha <- draw_vals_rw(sd = sd,
                        matrix_along_by = matrix_along_by_effect,
                        levels_effect = levels_effect)
  alpha + seas  
}

## HAS_TESTS
#' @export
draw_vals_effect.bage_prior_rw2 <- function(prior,
                                            vals_hyper,
                                            vals_hyperrand,
                                            vals_spline,
                                            vals_svd,
                                            dimnames_term,
                                            var_time,
                                            var_age,
                                            var_sexgender,
                                            n_sim) {
  along <- prior$specific$along
  sd <- vals_hyper$sd
  matrix_along_by_effect <- make_matrix_along_by_effect(along = along,
                                                        dimnames_term = dimnames_term,
                                                        var_time = var_time,
                                                        var_age = var_age)
  levels_effect <- dimnames_to_levels(dimnames_term)
  draw_vals_rw2(sd = sd,
                matrix_along_by = matrix_along_by_effect,
                levels_effect = levels_effect)
}

## HAS_TESTS
#' @export
draw_vals_effect.bage_prior_rw2seasfix <- function(prior,
                                                   vals_hyper,
                                                   vals_hyperrand,
                                                   vals_spline,
                                                   vals_svd,
                                                   dimnames_term,
                                                   var_time,
                                                   var_age,
                                                   var_sexgender,
                                                   n_sim) {
  along <- prior$specific$along
  sd <- vals_hyper$sd
  seas <- vals_hyperrand$seas
  matrix_along_by_effect <- make_matrix_along_by_effect(along = along,
                                                        dimnames_term = dimnames_term,
                                                        var_time = var_time,
                                                        var_age = var_age)
  levels_effect <- dimnames_to_levels(dimnames_term)
  alpha <- draw_vals_rw2(sd = sd,
                         matrix_along_by = matrix_along_by_effect,
                         levels_effect = levels_effect)
  alpha + seas  
}

## HAS_TESTS
#' @export
draw_vals_effect.bage_prior_rw2seasvary <- function(prior,
                                                    vals_hyper,
                                                    vals_hyperrand,
                                                    vals_spline,
                                                    vals_svd,
                                                    dimnames_term,
                                                    var_time,
                                                    var_age,
                                                    var_sexgender,
                                                    n_sim) {
  along <- prior$specific$along
  sd <- vals_hyper$sd
  seas <- vals_hyperrand$seas
  matrix_along_by_effect <- make_matrix_along_by_effect(along = along,
                                                        dimnames_term = dimnames_term,
                                                        var_time = var_time,
                                                        var_age = var_age)
  levels_effect <- dimnames_to_levels(dimnames_term)
  alpha <- draw_vals_rw2(sd = sd,
                         matrix_along_by = matrix_along_by_effect,
                         levels_effect = levels_effect)
  alpha + seas  
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
  levels_effect <- dimnames_to_levels(dimnames_term)
  m <- make_matrix_effectfree_effect(prior = prior,
                                     dimnames_term = dimnames_term,
                                     var_time = var_time,
                                     var_age = var_age,
                                     var_sexgender = var_sexgender)
  ans <- m %*% vals_spline
  ans <- Matrix::as.matrix(ans)
  rownames(ans) <- levels_effect
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
  svd_to_effect(svd = vals_svd,
                prior = prior,
                dimnames_term = dimnames_term,
                var_age = var_age,
                var_sexgender = var_sexgender)
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
  svd_to_effect(svd = vals_svd,
                prior = prior,
                dimnames_term = dimnames_term,
                var_age = var_age,
                var_sexgender = var_sexgender)
}

## HAS_TESTS
#' @export
draw_vals_effect.bage_prior_svd_rw <- function(prior,
                                               vals_hyper,
                                               vals_hyperrand,
                                               vals_spline,
                                               vals_svd,
                                               dimnames_term,
                                               var_time,
                                               var_age,
                                               var_sexgender,
                                               n_sim) {
  svd_to_effect(svd = vals_svd,
                prior = prior,
                dimnames_term = dimnames_term,
                var_age = var_age,
                var_sexgender = var_sexgender)
}

## HAS_TESTS
#' @export
draw_vals_effect.bage_prior_svd_rw2 <- function(prior,
                                                vals_hyper,
                                                vals_hyperrand,
                                                vals_spline,
                                                vals_svd,
                                                dimnames_term,
                                                var_time,
                                                var_age,
                                                var_sexgender,
                                                n_sim) {
  svd_to_effect(svd = vals_svd,
                prior = prior,
                dimnames_term = dimnames_term,
                var_age = var_age,
                var_sexgender = var_sexgender)
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
  sd <- draw_vals_sd(prior = prior, n_sim = n_sim)
  coef <- draw_vals_coef(prior = prior, n_sim = n_sim)
  list(sd = sd,
       coef = coef)
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
draw_vals_hyper.bage_prior_rw <- function(prior, n_sim) {
  sd <- draw_vals_sd(prior = prior, n_sim = n_sim)
  list(sd = sd)
}

## HAS_TESTS
#' @export
draw_vals_hyper.bage_prior_rwseasfix <- function(prior, n_sim) {
  sd <- draw_vals_sd(prior = prior, n_sim = n_sim)
  list(sd = sd)
}

## HAS_TESTS
#' @export
draw_vals_hyper.bage_prior_rwseasvary <- function(prior, n_sim) {
  sd_seas <- draw_vals_sd_seas(prior = prior, n_sim = n_sim)
  sd <- draw_vals_sd(prior = prior, n_sim = n_sim)
  list(sd_seas = sd_seas,
       sd = sd)
}

## HAS_TESTS
#' @export
draw_vals_hyper.bage_prior_rw2 <- function(prior, n_sim) {
  sd <- draw_vals_sd(prior = prior, n_sim = n_sim)
  list(sd = sd)
}

## HAS_TESTS
#' @export
draw_vals_hyper.bage_prior_rw2seasfix <- function(prior, n_sim) {
  sd <- draw_vals_sd(prior = prior, n_sim = n_sim)
  list(sd = sd)
}

## HAS_TESTS
#' @export
draw_vals_hyper.bage_prior_rw2seasvary <- function(prior, n_sim) {
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
draw_vals_hyper.bage_prior_svd_rw <- function(prior, n_sim) {
  sd <- draw_vals_sd(prior = prior, n_sim = n_sim)
  list(sd = sd)
}

## HAS_TESTS
#' @export
draw_vals_hyper.bage_prior_svd_rw2 <- function(prior, n_sim) {
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
  along <- prior$specific$along
  sd_slope <- prior$const[["sd_slope"]]
  matrix_along_by_effect <- make_matrix_along_by_effect(along = along,
                                                        dimnames_term = dimnames_term,
                                                        var_time = var_time,
                                                        var_age = var_age)
  n_by <- ncol(matrix_along_by_effect)
  intercept <- matrix(stats::rnorm(n = n_by * n_sim), nrow = n_by)
  slope <- draw_vals_slope(sd_slope = sd_slope,
                           matrix_along_by = matrix_along_by_effect,
                           n_sim = n_sim)
  list(intercept = intercept,
       slope = slope)
}

## HAS_TESTS
#' @export
draw_vals_hyperrand.bage_prior_linar <- function(prior,
                                                 vals_hyper,
                                                 dimnames_term,
                                                 var_time,
                                                 var_age,
                                                 n_sim) {
  along <- prior$specific$along
  sd_slope <- prior$const[["sd_slope"]]
  matrix_along_by_effect <- make_matrix_along_by_effect(along = along,
                                                        dimnames_term = dimnames_term,
                                                        var_time = var_time,
                                                        var_age = var_age)
  n_by <- ncol(matrix_along_by_effect)
  intercept <- matrix(stats::rnorm(n = n_by * n_sim), nrow = n_by)
  slope <- draw_vals_slope(sd_slope = sd_slope,
                           matrix_along_by = matrix_along_by_effect,
                           n_sim = n_sim)
  list(intercept = intercept,
       slope = slope)
}

## HAS_TESTS
#' @export
draw_vals_hyperrand.bage_prior_rwseasfix <- function(prior,
                                                     vals_hyper,
                                                     dimnames_term,
                                                     var_time,
                                                     var_age,
                                                     n_sim) {
  along <- prior$specific$along
  n_seas <- prior$specific$n_seas
  matrix_along_by_effect <- make_matrix_along_by_effect(along = along,
                                                        dimnames_term = dimnames_term,
                                                        var_time = var_time,
                                                        var_age = var_age)
  seas <- draw_vals_seasfix(n_seas = n_seas,
                            matrix_along_by = matrix_along_by_effect,
                            n_sim = n_sim)
  list(seas = seas)
}

## HAS_TESTS
#' @export
draw_vals_hyperrand.bage_prior_rwseasvary <- function(prior,
                                                      vals_hyper,
                                                      dimnames_term,
                                                      var_time,
                                                      var_age,
                                                      n_sim) {
  along <- prior$specific$along
  n_seas <- prior$specific$n_seas
  sd_seas <- vals_hyper$sd_seas
  matrix_along_by_effect <- make_matrix_along_by_effect(along = along,
                                                        dimnames_term = dimnames_term,
                                                        var_time = var_time,
                                                        var_age = var_age)
  seas <- draw_vals_seasvary(n_seas = n_seas,
                             sd_seas = sd_seas,
                             matrix_along_by = matrix_along_by_effect)
  list(seas = seas)
}

## HAS_TESTS
#' @export
draw_vals_hyperrand.bage_prior_rw2seasfix <- function(prior,
                                                      vals_hyper,
                                                      dimnames_term,
                                                      var_time,
                                                      var_age,
                                                      n_sim) {
  along <- prior$specific$along
  n_seas <- prior$specific$n_seas
  matrix_along_by_effect <- make_matrix_along_by_effect(along = along,
                                                        dimnames_term = dimnames_term,
                                                        var_time = var_time,
                                                        var_age = var_age)
  seas <- draw_vals_seasfix(n_seas = n_seas,
                            matrix_along_by = matrix_along_by_effect,
                            n_sim = n_sim)
  list(seas = seas)
}

## HAS_TESTS
#' @export
draw_vals_hyperrand.bage_prior_rw2seasvary <- function(prior,
                                                       vals_hyper,
                                                       dimnames_term,
                                                       var_time,
                                                       var_age,
                                                       n_sim) {
  along <- prior$specific$along
  n_seas <- prior$specific$n_seas
  sd_seas <- vals_hyper$sd_seas
  matrix_along_by_effect <- make_matrix_along_by_effect(along = along,
                                                        dimnames_term = dimnames_term,
                                                        var_time = var_time,
                                                        var_age = var_age)
  seas <- draw_vals_seasvary(n_seas = n_seas,
                             sd_seas = sd_seas,
                             matrix_along_by = matrix_along_by_effect)
  list(seas = seas)
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
#' @param levels_effectfree Labels for free parameters
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
                             levels_effectfree,
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
                                        levels_effectfree,
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
                                               levels_effectfree,
                                               n_sim) {
  sd <- vals_hyper$sd
  matrix_along_by_effectfree <- make_matrix_along_by_effectfree(prior = prior,
                                                                dimnames_term = dimnames_term,
                                                                var_time = var_time,
                                                                var_age = var_age,
                                                                var_sexgender = NULL)
  draw_vals_rw2(sd = sd,
                matrix_along_by = matrix_along_by_effectfree,
                levels_effect = levels_effectfree)
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
#' @param levels_effectfree Labels for free parameters
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
                          levels_effectfree,
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
                                     levels_effectfree,
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
                                         levels_effectfree,
                                         n_sim) {
  n_effectfree <- length(levels_effectfree)
  matrix(stats::rnorm(n = n_effectfree * n_sim),
         nrow = n_effectfree,
         ncol = n_sim,
         dimnames = list(levels_effectfree, NULL))
}

## HAS_TESTS
#' @export
draw_vals_svd.bage_prior_svd_ar <- function(prior,
                                            vals_hyper,
                                            dimnames_term,
                                            var_time,
                                            var_age,
                                            var_sexgender,
                                            levels_effectfree,
                                            n_sim) {
  coef <- vals_hyper$coef
  sd <- vals_hyper$sd
  matrix_along_by_effectfree <- make_matrix_along_by_effectfree(prior = prior,
                                                                dimnames_term = dimnames_term,
                                                                var_time = var_time,
                                                                var_age = var_age,
                                                                var_sexgender = var_sexgender)
  draw_vals_ar(coef = coef,
               sd = sd,
               matrix_along_by = matrix_along_by_effectfree,
               levels_effect = levels_effectfree)
}

## HAS_TESTS
#' @export
draw_vals_svd.bage_prior_svd_rw <- function(prior,
                                            vals_hyper,
                                            dimnames_term,
                                            var_time,
                                            var_age,
                                            var_sexgender,
                                            levels_effectfree,
                                            n_sim) {
  sd <- vals_hyper$sd
  matrix_along_by_effectfree <- make_matrix_along_by_effectfree(prior = prior,
                                                                dimnames_term = dimnames_term,
                                                                var_time = var_time,
                                                                var_age = var_age,
                                                                var_sexgender = var_sexgender)
  draw_vals_rw(sd = sd,
               matrix_along_by = matrix_along_by_effectfree,
               levels_effect = levels_effectfree)
}

## HAS_TESTS
#' @export
draw_vals_svd.bage_prior_svd_rw2 <- function(prior,
                                             vals_hyper,
                                             dimnames_term,
                                             var_time,
                                             var_age,
                                             var_sexgender,
                                             levels_effectfree,
                                             n_sim) {
  sd <- vals_hyper$sd
  matrix_along_by_effectfree <- make_matrix_along_by_effectfree(prior = prior,
                                                                dimnames_term = dimnames_term,
                                                                var_time = var_time,
                                                                var_age = var_age,
                                                                var_sexgender = var_sexgender)
  draw_vals_rw2(sd = sd,
                matrix_along_by = matrix_along_by_effectfree,
                levels_effect = levels_effectfree)
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
  along <- prior$specific$along
  nm <- dimnames_to_nm(dimnames_term)
  dimnames_forecast <- replace(dimnames_term, var_time, list(labels_forecast))
  levels_forecast <- dimnames_to_levels(dimnames_forecast)
  matrix_along_by_est <- make_matrix_along_by_effect(along = along,
                                                     dimnames_term = dimnames_term,
                                                     var_time = var_time,
                                                     var_age = var_age)
  matrix_along_by_forecast <- make_matrix_along_by_effect(along = along,
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
  ar_forecast <- forecast_ar(ar_est = ar_est,
                             coef = coef,
                             sd = sd,
                             matrix_along_by_est = matrix_along_by_est,
                             matrix_along_by_forecast = matrix_along_by_forecast)
  tibble::tibble(term = nm,
                 component = "effect",
                 level = levels_forecast,
                 .fitted = ar_forecast)
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
  along <- prior$specific$along
  nm <- dimnames_to_nm(dimnames_term)
  dimnames_forecast <- replace(dimnames_term, var_time, list(labels_forecast))
  levels_forecast <- dimnames_to_levels(dimnames_forecast)
  matrix_along_by_est <- make_matrix_along_by_effect(along = along,
                                                     dimnames_term = dimnames_term,
                                                     var_time = var_time,
                                                     var_age = var_age)
  matrix_along_by_forecast <- make_matrix_along_by_effect(along = along,
                                                          dimnames_term = dimnames_forecast,
                                                          var_time = var_time,
                                                          var_age = var_age)
  is_intercept <- with(components, term == nm & component == "hyper" & startsWith(level, "intercept"))
  is_slope <- with(components, term == nm & component == "hyper" & startsWith(level, "slope"))
  is_sd <- with(components, term == nm & component == "hyper" & level == "sd")
  intercept <- components$.fitted[is_intercept]
  slope <- components$.fitted[is_slope]
  sd <- components$.fitted[is_sd]
  lin_forecast <- forecast_lin(intercept = intercept,
                               slope = slope,
                               sd = sd,
                               matrix_along_by_est = matrix_along_by_est,
                               matrix_along_by_forecast = matrix_along_by_forecast)
  tibble::tibble(term = nm,
                 component = "effect",
                 level = levels_forecast,
                 .fitted = lin_forecast)
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
  along <- prior$specific$along
  nm <- dimnames_to_nm(dimnames_term)
  dimnames_forecast <- replace(dimnames_term, var_time, list(labels_forecast))
  levels_forecast <- dimnames_to_levels(dimnames_forecast)
  matrix_along_by_est <- make_matrix_along_by_effect(along = along,
                                                     dimnames_term = dimnames_term,
                                                     var_time = var_time,
                                                     var_age = var_age)
  matrix_along_by_forecast <- make_matrix_along_by_effect(along = along,
                                                          dimnames_term = dimnames_forecast,
                                                          var_time = var_time,
                                                          var_age = var_age)
  is_effect <- with(components, term == nm & component == "effect")
  is_coef <- with(components, term == nm & component == "hyper" & startsWith(level, "coef"))
  is_intercept <- with(components, term == nm & component == "hyper" & startsWith(level, "intercept"))
  is_slope <- with(components, term == nm & component == "hyper" & startsWith(level, "slope"))
  is_sd <- with(components, term == nm & component == "hyper" & level == "sd")
  effect_est <- components$.fitted[is_effect]
  coef <- components$.fitted[is_coef]
  intercept <- components$.fitted[is_intercept]
  slope <- components$.fitted[is_slope]
  sd <- components$.fitted[is_sd]
  trend_est <- make_lin_trend(intercept = intercept,
                              slope = slope,
                              matrix_along_by = matrix_along_by_est)
  trend_forecast <- forecast_lin_trend(intercept = intercept,
                                       slope = slope,
                                       matrix_along_by_est = matrix_along_by_est,
                                       matrix_along_by_forecast = matrix_along_by_forecast)
  cyclical_est <- effect_est - trend_est
  cyclical_forecast <- forecast_ar(ar_est = cyclical_est,
                                   coef = coef,
                                   sd = sd,
                                   matrix_along_by_est = matrix_along_by_est,
                                   matrix_along_by_forecast = matrix_along_by_forecast)
  effect_forecast <- trend_forecast + cyclical_forecast
  component <- rep(c("effect", "trend", "cyclical"), each = length(effect_forecast))
  level <- rep(levels_forecast, times = 3L)
  .fitted <- c(effect_forecast, trend_forecast, cyclical_forecast)
  tibble::tibble(term = nm,
                 component = component,
                 level = level,
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
forecast_term.bage_prior_rw <- function(prior,
                                        dimnames_term,
                                        var_time,
                                        var_age,
                                        var_sexgender,
                                        components,
                                        labels_forecast) {
  along <- prior$specific$along
  nm <- dimnames_to_nm(dimnames_term)
  dimnames_forecast <- replace(dimnames_term, var_time, list(labels_forecast))
  levels_forecast <- dimnames_to_levels(dimnames_forecast)
  matrix_along_by_est <- make_matrix_along_by_effect(along = along,
                                                     dimnames_term = dimnames_term,
                                                     var_time = var_time,
                                                     var_age = var_age)
  matrix_along_by_forecast <- make_matrix_along_by_effect(along = along,
                                                          dimnames_term = dimnames_forecast,
                                                          var_time = var_time,
                                                          var_age = var_age)
  is_effect <- with(components, term == nm & component == "effect")
  is_sd <- with(components, term == nm & component == "hyper" & level == "sd")
  rw_est <- components$.fitted[is_effect]
  sd <- components$.fitted[is_sd]
  rw_forecast <- forecast_rw(rw_est = rw_est,
                             sd = sd,
                             matrix_along_by_est = matrix_along_by_est,
                             matrix_along_by_forecast = matrix_along_by_forecast)
  tibble::tibble(term = nm,
                 component = "effect",
                 level = levels_forecast,
                 .fitted = rw_forecast)
}

## HAS_TESTS
#' @export
forecast_term.bage_prior_rwseasfix <- function(prior,
                                               dimnames_term,
                                               var_time,
                                               var_age,
                                               var_sexgender,
                                               components,
                                               labels_forecast) {
  along <- prior$specific$along
  n_seas <- prior$specific$n_seas
  nm <- dimnames_to_nm(dimnames_term)
  dimnames_forecast <- replace(dimnames_term, var_time, list(labels_forecast))
  levels_forecast <- dimnames_to_levels(dimnames_forecast)
  matrix_along_by_est <- make_matrix_along_by_effect(along = along,
                                                     dimnames_term = dimnames_term,
                                                     var_time = var_time,
                                                     var_age = var_age)
  matrix_along_by_forecast <- make_matrix_along_by_effect(along = along,
                                                          dimnames_term = dimnames_forecast,
                                                          var_time = var_time,
                                                          var_age = var_age)
  is_trend <- with(components, term == nm & component == "trend")
  is_seasonal <- with(components, term == nm & component == "seasonal")
  is_sd <- with(components, term == nm & component == "hyper" & level == "sd")
  rw_est <- components$.fitted[is_trend]
  seas_est <- components$.fitted[is_seasonal]
  sd <- components$.fitted[is_sd]
  rw_forecast <- forecast_rw(rw_est = rw_est,
                             sd = sd,
                             matrix_along_by_est = matrix_along_by_est,
                             matrix_along_by_forecast = matrix_along_by_forecast)
  seas_forecast <- forecast_seasfix(n_seas = n_seas,
                                    seas_est = seas_est,
                                    matrix_along_by_est = matrix_along_by_est,
                                    matrix_along_by_forecast = matrix_along_by_forecast)
  effect_forecast <- rw_forecast + seas_forecast
  n_level <- length(effect_forecast)
  tibble::tibble(term = nm,
                 component = rep(c("effect", "trend", "seasonal"), each = n_level),
                 level = rep(levels_forecast, times = 3L),
                 .fitted = c(effect_forecast, rw_forecast, seas_forecast))
}

## HAS_TESTS
#' @export
forecast_term.bage_prior_rwseasvary <- function(prior,
                                        dimnames_term,
                                        var_time,
                                        var_age,
                                        var_sexgender,
                                        components,
                                        labels_forecast) {
  along <- prior$specific$along
  n_seas <- prior$specific$n_seas
  nm <- dimnames_to_nm(dimnames_term)
  dimnames_forecast <- replace(dimnames_term, var_time, list(labels_forecast))
  levels_forecast <- dimnames_to_levels(dimnames_forecast)
  matrix_along_by_est <- make_matrix_along_by_effect(along = along,
                                                     dimnames_term = dimnames_term,
                                                     var_time = var_time,
                                                     var_age = var_age)
  matrix_along_by_forecast <- make_matrix_along_by_effect(along = along,
                                                          dimnames_term = dimnames_forecast,
                                                          var_time = var_time,
                                                          var_age = var_age)
  is_trend <- with(components, term == nm & component == "trend")
  is_seasonal <- with(components, term == nm & component == "seasonal")
  is_sd_seas <- with(components, term == nm & component == "hyper" & level == "sd_seas")
  is_sd <- with(components, term == nm & component == "hyper" & level == "sd")
  rw_est <- components$.fitted[is_trend]
  seas_est <- components$.fitted[is_seasonal]
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
  effect_forecast <- rw_forecast + seas_forecast
  n_level <- length(effect_forecast)
  tibble::tibble(term = nm,
                 component = rep(c("effect", "trend", "seasonal"), each = n_level),
                 level = rep(levels_forecast, times = 3L),
                 .fitted = c(effect_forecast, rw_forecast, seas_forecast))
}

## HAS_TESTS
#' @export
forecast_term.bage_prior_rw2 <- function(prior,
                                        dimnames_term,
                                        var_time,
                                        var_age,
                                        var_sexgender,
                                        components,
                                        labels_forecast) {
  along <- prior$specific$along
  nm <- dimnames_to_nm(dimnames_term)
  dimnames_forecast <- replace(dimnames_term, var_time, list(labels_forecast))
  levels_forecast <- dimnames_to_levels(dimnames_forecast)
  matrix_along_by_est <- make_matrix_along_by_effect(along = along,
                                                     dimnames_term = dimnames_term,
                                                     var_time = var_time,
                                                     var_age = var_age)
  matrix_along_by_forecast <- make_matrix_along_by_effect(along = along,
                                                          dimnames_term = dimnames_forecast,
                                                          var_time = var_time,
                                                          var_age = var_age)
  is_effect <- with(components, term == nm & component == "effect")
  is_sd <- with(components, term == nm & component == "hyper" & level == "sd")
  rw2_est <- components$.fitted[is_effect]
  sd <- components$.fitted[is_sd]
  rw2_forecast <- forecast_rw2(rw2_est = rw2_est,
                               sd = sd,
                               matrix_along_by_est = matrix_along_by_est,
                               matrix_along_by_forecast = matrix_along_by_forecast)
  tibble::tibble(term = nm,
                 component = "effect",
                 level = levels_forecast,
                 .fitted = rw2_forecast)
}

## HAS_TESTS
#' @export
forecast_term.bage_prior_rw2seasfix <- function(prior,
                                                dimnames_term,
                                                var_time,
                                                var_age,
                                                var_sexgender,
                                                components,
                                                labels_forecast) {
  along <- prior$specific$along
  n_seas <- prior$specific$n_seas
  nm <- dimnames_to_nm(dimnames_term)
  dimnames_forecast <- replace(dimnames_term, var_time, list(labels_forecast))
  levels_forecast <- dimnames_to_levels(dimnames_forecast)
  matrix_along_by_est <- make_matrix_along_by_effect(along = along,
                                                     dimnames_term = dimnames_term,
                                                     var_time = var_time,
                                                     var_age = var_age)
  matrix_along_by_forecast <- make_matrix_along_by_effect(along = along,
                                                          dimnames_term = dimnames_forecast,
                                                          var_time = var_time,
                                                          var_age = var_age)
  is_trend <- with(components, term == nm & component == "trend")
  is_seasonal <- with(components, term == nm & component == "seasonal")
  is_sd <- with(components, term == nm & component == "hyper" & level == "sd")
  rw2_est <- components$.fitted[is_trend]
  seas_est <- components$.fitted[is_seasonal]
  sd <- components$.fitted[is_sd]
  rw2_forecast <- forecast_rw2(rw2_est = rw2_est,
                               sd = sd,
                               matrix_along_by_est = matrix_along_by_est,
                               matrix_along_by_forecast = matrix_along_by_forecast)
  seas_forecast <- forecast_seasfix(n_seas = n_seas,
                                    seas_est = seas_est,
                                    matrix_along_by_est = matrix_along_by_est,
                                    matrix_along_by_forecast = matrix_along_by_forecast)
  effect_forecast <- rw2_forecast + seas_forecast
  n_level <- length(effect_forecast)
  tibble::tibble(term = nm,
                 component = rep(c("effect", "trend", "seasonal"), each = n_level),
                 level = rep(levels_forecast, times = 3L),
                 .fitted = c(effect_forecast, rw2_forecast, seas_forecast))
}

## HAS_TESTS
#' @export
forecast_term.bage_prior_rw2seasvary <- function(prior,
                                                 dimnames_term,
                                                 var_time,
                                                 var_age,
                                                 var_sexgender,
                                                 components,
                                                 labels_forecast) {
  along <- prior$specific$along
  n_seas <- prior$specific$n_seas
  nm <- dimnames_to_nm(dimnames_term)
  dimnames_forecast <- replace(dimnames_term, var_time, list(labels_forecast))
  levels_forecast <- dimnames_to_levels(dimnames_forecast)
  matrix_along_by_est <- make_matrix_along_by_effect(along = along,
                                                     dimnames_term = dimnames_term,
                                                     var_time = var_time,
                                                     var_age = var_age)
  matrix_along_by_forecast <- make_matrix_along_by_effect(along = along,
                                                          dimnames_term = dimnames_forecast,
                                                          var_time = var_time,
                                                          var_age = var_age)
  is_trend <- with(components, term == nm & component == "trend")
  is_seasonal <- with(components, term == nm & component == "seasonal")
  is_sd_seas <- with(components, term == nm & component == "hyper" & level == "sd_seas")
  is_sd <- with(components, term == nm & component == "hyper" & level == "sd")
  rw2_est <- components$.fitted[is_trend]
  seas_est <- components$.fitted[is_seasonal]
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
  effect_forecast <- rw2_forecast + seas_forecast
  n_level <- length(effect_forecast)
  tibble::tibble(term = nm,
                 component = rep(c("effect", "trend", "seasonal"), each = n_level),
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
  effect_forecast <- svd_to_effect(svd = svd_forecast,
                                   prior = prior,
                                   dimnames_term = dimnames_forecast,
                                   var_age = var_age,
                                   var_sexgender = var_sexgender)
  nm <- dimnames_to_nm(dimnames_term)
  levels_effect <- dimnames_to_levels(dimnames_forecast)
  levels_svd <- make_levels_svd_term(prior = prior,
                                     dimnames_term = dimnames_forecast,
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
forecast_term.bage_prior_svd_rw <- function(prior,
                                            dimnames_term,
                                            var_time,
                                            var_age,
                                            var_sexgender,
                                            components,
                                            labels_forecast) {
  dimnames_forecast <- replace(dimnames_term, var_time, list(labels_forecast))
  svd_forecast <- forecast_rw_svd(prior = prior,
                                  dimnames_term = dimnames_term,
                                  dimnames_forecast = dimnames_forecast,
                                  var_time = var_time,
                                  var_age = var_age,
                                  var_sexgender = var_sexgender,
                                  components = components,
                                  labels_forecast)
  effect_forecast <- svd_to_effect(svd = svd_forecast,
                                   prior = prior,
                                   dimnames_term = dimnames_forecast,
                                   var_age = var_age,
                                   var_sexgender = var_sexgender)
  nm <- dimnames_to_nm(dimnames_term)
  levels_effect <- dimnames_to_levels(dimnames_forecast)
  levels_svd <- make_levels_svd_term(prior = prior,
                                     dimnames_term = dimnames_forecast,
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
forecast_term.bage_prior_svd_rw2 <- function(prior,
                                             dimnames_term,
                                             var_time,
                                             var_age,
                                             var_sexgender,
                                             components,
                                             labels_forecast) {
  dimnames_forecast <- replace(dimnames_term, var_time, list(labels_forecast))
  svd_forecast <- forecast_rw2_svd(prior = prior,
                                   dimnames_term = dimnames_term,
                                   dimnames_forecast = dimnames_forecast,
                                   var_time = var_time,
                                   var_age = var_age,
                                   var_sexgender = var_sexgender,
                                   components = components,
                                   labels_forecast)
  effect_forecast <- svd_to_effect(svd = svd_forecast,
                                   prior = prior,
                                   dimnames_term = dimnames_forecast,
                                   var_age = var_age,
                                   var_sexgender = var_sexgender)
  nm <- dimnames_to_nm(dimnames_term)
  levels_effect <- dimnames_to_levels(dimnames_forecast)
  levels_svd <- make_levels_svd_term(prior = prior,
                                     dimnames_term = dimnames_forecast,
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




## 'has_hyperrand' ------------------------------------------------------------

#' Has Hyper-Parameters that can be Treated as Random Effects
#'
#' @param prior Object of class 'bage_prior'
#'
#' @returns TRUE or FALSE.
#'
#' @noRd
has_hyperrand <- function(prior) {
  UseMethod("has_hyperrand")
}

## HAS_TESTS
#' @export
has_hyperrand.bage_prior <- function(prior) FALSE


## HAS_TESTS
#' @export
has_hyperrand.bage_prior_lin <- function(prior) TRUE

## HAS_TESTS
#' @export
has_hyperrand.bage_prior_linar <- function(prior) TRUE

## HAS_TESTS
#' @export
has_hyperrand.bage_prior_rwseasfix <- function(prior) TRUE

## HAS_TESTS
#' @export
has_hyperrand.bage_prior_rwseasvary <- function(prior) TRUE

## HAS_TESTS
#' @export
has_hyperrand.bage_prior_rw2seasfix <- function(prior) TRUE

## HAS_TESTS
#' @export
has_hyperrand.bage_prior_rw2seasvary <- function(prior) TRUE


## 'infer_trend_cyc_seas_err_one' ---------------------------------------------------

#' Derive Parts of 'Components' Output Dealing with a
#' Prior that has Hyper-Parameters Treated as Random Effects
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
infer_trend_cyc_seas_err_one <- function(prior,
                                         dimnames_term,
                                         var_time,
                                         var_age,
                                         components) {
  UseMethod("infer_trend_cyc_seas_err_one")
}

## HAS_TESTS
#' @export
infer_trend_cyc_seas_err_one.bage_prior <- function(prior,
                                                    dimnames_term,
                                                    var_time,
                                                    var_age,
                                                    components)
  components

## HAS_TESTS
#' @export
infer_trend_cyc_seas_err_one.bage_prior_lin <- function(prior,
                                                        dimnames_term,
                                                        var_time,
                                                        var_age,
                                                        components) {
  nm <- dimnames_to_nm(dimnames_term)
  is_intercept <- with(components,
  (term == nm) & startsWith(component, "hyper") & startsWith(level, "intercept"))
  is_slope <- with(components,
  (term == nm) & startsWith(component, "hyper") & startsWith(level, "slope"))
  components$component[is_intercept] <- "hyper"
  components$component[is_slope] <- "hyper"
  components
}

## HAS_TESTS
#' @export
infer_trend_cyc_seas_err_one.bage_prior_linar <- function(prior,
                                                          dimnames_term,
                                                          var_time,
                                                          var_age,
                                                          components) {
  nm <- dimnames_to_nm(dimnames_term)
  is_intercept <- with(components,
  (term == nm) & startsWith(component, "hyper") & startsWith(level, "intercept"))
  is_slope <- with(components,
  (term == nm) & startsWith(component, "hyper") & startsWith(level, "slope"))
  is_effect <- with(components, (term == nm) & (component == "effect"))
  is_trend <- with(components, (term == nm) & (component == "trend"))
  is_cyclical <- with(components, (term == nm) & (component == "cyclical"))
  effect <- components$.fitted[is_effect]
  intercept <- components$.fitted[is_intercept]
  slope <- components$.fitted[is_slope]
  along <- prior$specific$along
  matrix_along_by_effect <- make_matrix_along_by_effect(along = along,
                                                        dimnames_term = dimnames_term,
                                                        var_time = var_time,
                                                        var_age = var_age)
  trend <- make_lin_trend(intercept = intercept,
                          slope = slope,
                          matrix_along_by = matrix_along_by_effect)
  cyclical <- effect - trend
  components$component[is_intercept] <- "hyper"
  components$component[is_slope] <- "hyper"
  level <- components$level[is_effect]
  trend <- tibble::tibble(term = nm,
                          component = "trend",
                          level = level,
                          .fitted = trend)
  cyclical <- tibble::tibble(term = nm,
                             component = "cyclical",
                             level = level,
                             .fitted = cyclical)
  components <- vctrs::vec_rbind(components, trend, cyclical)
  components
}
  

## HAS_TESTS
#' @export
infer_trend_cyc_seas_err_one.bage_prior_rwseasfix <- function(prior,
                                                              dimnames_term,
                                                              var_time,
                                                              var_age,
                                                              components)
  infer_trend_cyc_seas_err_seasfix(prior = prior,
                                   dimnames_term = dimnames_term,
                                   var_time = var_time,
                                   var_age = var_age,
                                   components = components)

## HAS_TESTS
#' @export
infer_trend_cyc_seas_err_one.bage_prior_rwseasvary <- function(prior,
                                                         dimnames_term,
                                                         var_time,
                                                         var_age,
                                                         components)
  infer_trend_cyc_seas_err_seasvary(prior = prior,
                              dimnames_term = dimnames_term,
                              var_time = var_time,
                              var_age = var_age,
                              components = components)

## HAS_TESTS
#' @export
infer_trend_cyc_seas_err_one.bage_prior_rw2seasfix <- function(prior,
                                                         dimnames_term,
                                                         var_time,
                                                         var_age,
                                                         components)
  infer_trend_cyc_seas_err_seasfix(prior = prior,
                             dimnames_term = dimnames_term,
                             var_time = var_time,
                             var_age = var_age,
                             components = components)

## HAS_TESTS
#' @export
infer_trend_cyc_seas_err_one.bage_prior_rw2seasvary <- function(prior,
                                                          dimnames_term,
                                                          var_time,
                                                          var_age,
                                                          components)
  infer_trend_cyc_seas_err_seasvary(prior = prior,
                              dimnames_term = dimnames_term,
                              var_time = var_time,
                              var_age = var_age,
                              components = components)




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
  is_cyclical <- with(components, (term == nm) & (component == "cyclical"))
  effect <- components$.fitted[is_effect]
  trend <- components$.fitted[is_trend]
  cyclical <- effect - trend
  components$.fitted[is_cyclical] <- cyclical
  components
}
  

## HAS_TESTS
#' @export
infer_trend_cyc_seas_err_forecast_one.bage_prior_rwseasfix <- function(prior,
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
infer_trend_cyc_seas_err_forecast_one.bage_prior_rwseasvary <- function(prior,
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
infer_trend_cyc_seas_err_forecast_one.bage_prior_rw2seasfix <- function(prior,
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
infer_trend_cyc_seas_err_forecast_one.bage_prior_rw2seasvary <- function(prior,
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
#' @param nm Name of term.
#' @param dimnames_term Dimnames for array representation of term
#' @param var_time Name of time variable
#' @param var_age Name of age variable
#' @param var_sexgender Name of sex/gender variable
#'
#' @returns TRUE or raises an error
#'
#' @noRd
is_prior_ok_for_term <- function(prior,
                                 nm,
                                 dimnames_term,
                                 var_time,
                                 var_age,
                                 var_sexgender) {
  UseMethod("is_prior_ok_for_term")
}

## HAS_TESTS
#' @export
is_prior_ok_for_term.bage_prior_ar <- function(prior,
                                               nm,
                                               dimnames_term,
                                               var_time,
                                               var_age,
                                               var_sexgender) {
  along <- prior$specific$along
  n_coef <- prior$specific$n_coef
  matrix_along_by_effect <- make_matrix_along_by_effect(along = along,
                                                        dimnames_term = dimnames_term,
                                                        var_time = var_time,
                                                        var_age = var_age)
  length_along <- nrow(matrix_along_by_effect)
  check_length_along_ge(length_along = length_along,
                        min = n_coef + 1L,
                        nm = nm,
                        prior = prior)
  invisible(TRUE)
}

## HAS_TESTS
#' @export
is_prior_ok_for_term.bage_prior_known <- function(prior,
                                                  nm,
                                                  dimnames_term,
                                                  var_time,
                                                  var_age,
                                                  var_sexgender) {
  values <- prior$specific$values
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
                                                nm,
                                                dimnames_term,
                                                var_time,
                                                var_age,
                                                var_sexgender) {
  along <- prior$specific$along
  matrix_along_by_effect <- make_matrix_along_by_effect(along = along,
                                                        dimnames_term = dimnames_term,
                                                        var_time = var_time,
                                                        var_age = var_age)
  length_along <- nrow(matrix_along_by_effect)
  check_length_along_ge(length_along = length_along,
                        min = 2L,
                        nm = nm,
                        prior = prior)
  invisible(TRUE)
}

## HAS_TESTS
#' @export
is_prior_ok_for_term.bage_prior_linar <- function(prior,
                                                  nm,
                                                  dimnames_term,
                                                  var_time,
                                                  var_age,
                                                  var_sexgender) {
  along <- prior$specific$along
  n_coef <- prior$specific$n_coef
  matrix_along_by_effect <- make_matrix_along_by_effect(along = along,
                                                        dimnames_term = dimnames_term,
                                                        var_time = var_time,
                                                        var_age = var_age)
  length_along <- nrow(matrix_along_by_effect)
  check_length_along_ge(length_along = length_along,
                        min = n_coef + 1L,
                        nm = nm,
                        prior = prior)
  invisible(TRUE)
}

## HAS_TESTS
#' @export
is_prior_ok_for_term.bage_prior_norm <- function(prior,
                                                 nm,
                                                 dimnames_term,
                                                 var_time,
                                                 var_age,
                                                 var_sexgender) {
  length_effect <- prod(lengths(dimnames_term))
  check_length_effect_ge(length_effect = length_effect,
                         min = 2L,
                         nm = nm,
                         prior = prior)
  invisible(TRUE)
}

## HAS_TESTS
#' @export
is_prior_ok_for_term.bage_prior_normfixed <- function(prior,
                                                      nm,
                                                      dimnames_term,
                                                      var_time,
                                                      var_age,
                                                      var_sexgender) {
  length_effect <- prod(lengths(dimnames_term))
  check_length_effect_ge(length_effect = length_effect,
                         min = 1L,
                         nm = nm,
                         prior = prior)
  invisible(TRUE)
}

## HAS_TESTS
#' @export
is_prior_ok_for_term.bage_prior_rw <- function(prior,
                                               nm,
                                               dimnames_term,
                                               var_time,
                                               var_age,
                                               var_sexgender) {
  along <- prior$specific$along
  matrix_along_by_effect <- make_matrix_along_by_effect(along = along,
                                                        dimnames_term = dimnames_term,
                                                        var_time = var_time,
                                                        var_age = var_age)
  length_along <- nrow(matrix_along_by_effect)
  check_length_along_ge(length_along = length_along,
                        min = 2L,
                        nm = nm,
                        prior = prior)
  invisible(TRUE)
}

## HAS_TESTS
#' @export
is_prior_ok_for_term.bage_prior_rwseasfix <- function(prior,
                                                      nm,
                                                      dimnames_term,
                                                      var_time,
                                                      var_age,
                                                      var_sexgender) {
  n_seas <- prior$specific$n_seas
  along <- prior$specific$along
  matrix_along_by_effect <- make_matrix_along_by_effect(along = along,
                                                        dimnames_term = dimnames_term,
                                                        var_time = var_time,
                                                        var_age = var_age)
  length_along <- nrow(matrix_along_by_effect)
  check_length_along_ge(length_along = length_along,
                        min = n_seas,
                        nm = nm,
                        prior = prior)
  invisible(TRUE)
}

## HAS_TESTS
#' @export
is_prior_ok_for_term.bage_prior_rwseasvary <- function(prior,
                                                       nm,
                                                       dimnames_term,
                                                       var_time,
                                                       var_age,
                                                       var_sexgender) {
  n_seas <- prior$specific$n_seas
  along <- prior$specific$along
  matrix_along_by_effect <- make_matrix_along_by_effect(along = along,
                                                        dimnames_term = dimnames_term,
                                                        var_time = var_time,
                                                        var_age = var_age)
  length_along <- nrow(matrix_along_by_effect)
  check_length_along_ge(length_along = length_along,
                        min = n_seas,
                        nm = nm,
                        prior = prior)
  invisible(TRUE)
}

## HAS_TESTS
#' @export
is_prior_ok_for_term.bage_prior_rw2 <- function(prior,
                                                nm,
                                                dimnames_term,
                                                var_time,
                                                var_age,
                                                var_sexgender) {
  along <- prior$specific$along
  matrix_along_by_effect <- make_matrix_along_by_effect(along = along,
                                                        dimnames_term = dimnames_term,
                                                        var_time = var_time,
                                                        var_age = var_age)
  length_along <- nrow(matrix_along_by_effect)
  check_length_along_ge(length_along = length_along,
                        min = 3L,
                        nm = nm,
                        prior = prior)
  invisible(TRUE)
}

## HAS_TESTS
#' @export
is_prior_ok_for_term.bage_prior_rw2seasfix <- function(prior,
                                                       nm,
                                                       dimnames_term,
                                                       var_time,
                                                       var_age,
                                                       var_sexgender) {
  n_seas <- prior$specific$n_seas
  along <- prior$specific$along
  matrix_along_by_effect <- make_matrix_along_by_effect(along = along,
                                                        dimnames_term = dimnames_term,
                                                        var_time = var_time,
                                                        var_age = var_age)
  length_along <- nrow(matrix_along_by_effect)
  check_length_along_ge(length_along = length_along,
                        min = n_seas,
                        nm = nm,
                        prior = prior)
  invisible(TRUE)
}

## HAS_TESTS
#' @export
is_prior_ok_for_term.bage_prior_rw2seasvary <- function(prior,
                                                        nm,
                                                        dimnames_term,
                                                        var_time,
                                                        var_age,
                                                        var_sexgender) {
  n_seas <- prior$specific$n_seas
  along <- prior$specific$along
  matrix_along_by_effect <- make_matrix_along_by_effect(along = along,
                                                        dimnames_term = dimnames_term,
                                                        var_time = var_time,
                                                        var_age = var_age)
  length_along <- nrow(matrix_along_by_effect)
  check_length_along_ge(length_along = length_along,
                        min = n_seas,
                        nm = nm,
                        prior = prior)
  invisible(TRUE)
}

## HAS_TESTS
#' @export
is_prior_ok_for_term.bage_prior_spline <- function(prior,
                                                   nm,
                                                   dimnames_term,
                                                   var_time,
                                                   var_age,
                                                   var_sexgender) {
  along <- prior$specific$along
  matrix_along_by_effect <- make_matrix_along_by_effect(along = along,
                                                        dimnames_term = dimnames_term,
                                                        var_time = var_time,
                                                        var_age = var_age)
  length_along <- nrow(matrix_along_by_effect)
  check_length_effect_ge(length_effect = length_along,
                         min = 2L,
                         nm = nm,
                         prior = prior)
  invisible(TRUE)
}

## HAS_TESTS
#' @export
is_prior_ok_for_term.bage_prior_svd <- function(prior,
                                                nm,
                                                dimnames_term,
                                                var_time,
                                                var_age,
                                                var_sexgender) {
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
                                                   nm,
                                                   dimnames_term,
                                                   var_time,
                                                   var_age,
                                                   var_sexgender) {
  agesex <- make_agesex(nm = nm,
                        var_age = var_age,
                        var_sexgender = var_sexgender)
  check_svd_agesex(prior = prior,
                   nm = nm,
                   var_age = var_age,
                   agesex = agesex)
  check_svd_time(prior = prior,
                 nm = nm,
                 var_time = var_time)
  along <- prior$specific$along
  n_coef <- prior$specific$n_coef
  matrix_along_by_effect <- make_matrix_along_by_effect(along = along,
                                                        dimnames_term = dimnames_term,
                                                        var_time = var_time,
                                                        var_age = var_age)
  length_along <- nrow(matrix_along_by_effect)
  check_length_along_ge(length_along = length_along,
                        min = n_coef + 1L,
                        nm = nm,
                        prior = prior)
  invisible(TRUE)
}

## HAS_TESTS
#' @export
is_prior_ok_for_term.bage_prior_svd_rw <- function(prior,
                                                   nm,
                                                   dimnames_term,
                                                   var_time,
                                                   var_age,
                                                   var_sexgender) {
  agesex <- make_agesex(nm = nm,
                        var_age = var_age,
                        var_sexgender = var_sexgender)
  check_svd_agesex(prior = prior,
                   nm = nm,
                   var_age = var_age,
                   agesex = agesex)
  check_svd_time(prior = prior,
                 nm = nm,
                 var_time = var_time)
  along <- prior$specific$along
  matrix_along_by_effect <- make_matrix_along_by_effect(along = along,
                                                        dimnames_term = dimnames_term,
                                                        var_time = var_time,
                                                        var_age = var_age)
  length_along <- nrow(matrix_along_by_effect)
  check_length_along_ge(length_along = length_along,
                        min = 2L,
                        nm = nm,
                        prior = prior)
  invisible(TRUE)
}

## HAS_TESTS
#' @export
is_prior_ok_for_term.bage_prior_svd_rw2 <- function(prior,
                                                    nm,
                                                    dimnames_term,
                                                    var_time,
                                                    var_age,
                                                    var_sexgender) {
  agesex <- make_agesex(nm = nm,
                        var_age = var_age,
                        var_sexgender = var_sexgender)
  check_svd_agesex(prior = prior,
                   nm = nm,
                   var_age = var_age,
                   agesex = agesex)
  check_svd_time(prior = prior,
                 nm = nm,
                 var_time = var_time)
  along <- prior$specific$along
  matrix_along_by_effect <- make_matrix_along_by_effect(along = along,
                                                        dimnames_term = dimnames_term,
                                                        var_time = var_time,
                                                        var_age = var_age)
  length_along <- nrow(matrix_along_by_effect)
  check_length_along_ge(length_along = length_along,
                        min = 3L,
                        nm = nm,
                        prior = prior)
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
is_svd.bage_prior_svd_rw <- function(prior) TRUE

## HAS_TESTS
#' @export
is_svd.bage_prior_svd_rw2 <- function(prior) TRUE



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
  c("sd", coef)
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
levels_hyper.bage_prior_rw <- function(prior)
  "sd"

## HAS_TESTS
#' @export
levels_hyper.bage_prior_rwseasfix <- function(prior)
  "sd"

## HAS_TESTS
#' @export
levels_hyper.bage_prior_rwseasvary <- function(prior)
  c("sd_seas", "sd")

## HAS_TESTS
#' @export
levels_hyper.bage_prior_rw2 <- function(prior)
  "sd"

## HAS_TESTS
#' @export
levels_hyper.bage_prior_rw2seasfix <- function(prior)
  "sd"

## HAS_TESTS
#' @export
levels_hyper.bage_prior_rw2seasvary <- function(prior)
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
levels_hyper.bage_prior_svd_rw <- function(prior)
  "sd"

## HAS_TESTS
#' @export
levels_hyper.bage_prior_svd_rw2 <- function(prior)
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
#' @param levels_effect Labels made from classifying dimensions
#'
#' @returns A character vector.
#'
#' @noRd
levels_hyperrand <- function(prior,
                             dimnames_term,
                             var_time,
                             var_age,
                             levels_effect) {
  UseMethod("levels_hyperrand")
}

## HAS_TESTS
#' @export
levels_hyperrand.bage_prior <- function(prior,
                                        dimnames_term,
                                        var_time,
                                        var_age,
                                        levels_effect) {
  character()
}

## HAS_TESTS
#' @export
levels_hyperrand.bage_prior_lin <- function(prior,
                                            dimnames_term,
                                            var_time,
                                            var_age,
                                            levels_effect) {
  along <- prior$specific$along
  matrix_along_by_effect <- make_matrix_along_by_effect(along = along,
                                                        dimnames_term = dimnames_term,
                                                        var_time = var_time,
                                                        var_age = var_age)
  n_by <- ncol(matrix_along_by_effect)
  if (n_by > 1L) {
    nms_by <- colnames(matrix_along_by_effect)
    c(paste("intercept", nms_by, sep = "."),
      paste("slope", nms_by, sep = "."))
  }
  else
    c("intercept", "slope")
}

## HAS_TESTS
#' @export
levels_hyperrand.bage_prior_linar <- function(prior,
                                              dimnames_term,
                                              var_time,
                                              var_age,
                                              levels_effect) {
  along <- prior$specific$along
  matrix_along_by_effect <- make_matrix_along_by_effect(along = along,
                                                        dimnames_term = dimnames_term,
                                                        var_time = var_time,
                                                        var_age = var_age)
  n_by <- ncol(matrix_along_by_effect)
  if (n_by > 1L) {
    nms_by <- colnames(matrix_along_by_effect)
    c(paste("intercept", nms_by, sep = "."),
      paste("slope", nms_by, sep = "."))
  }
  else
    c("intercept", "slope")
}

## HAS_TESTS
#' @export
levels_hyperrand.bage_prior_rwseasfix <- function(prior,
                                                  dimnames_term,
                                                  var_time,
                                                  var_age,
                                                  levels_effect) {
  along <- prior$specific$along
  n_seas <- prior$specific$n_seas
  matrix_along_by_effect <- make_matrix_along_by_effect(along = along,
                                                        dimnames_term = dimnames_term,
                                                        var_time = var_time,
                                                        var_age = var_age)
  s <- seq_len(n_seas)
  n_by <- ncol(matrix_along_by_effect)
  if (n_by > 1L) {
    nms_by <- colnames(matrix_along_by_effect)
    paste(s,
          rep(nms_by, each = n_seas),
          sep = ".")
  }
  else
    as.character(s)
}

## HAS_TESTS
#' @export
levels_hyperrand.bage_prior_rwseasvary <- function(prior,
                                                   dimnames_term,
                                                   var_time,
                                                   var_age,
                                                   levels_effect) {
  levels_effect
}

## HAS_TESTS
#' @export
levels_hyperrand.bage_prior_rw2seasfix <- function(prior,
                                                   dimnames_term,
                                                   var_time,
                                                   var_age,
                                                   levels_effect) {
  n_seas <- prior$specific$n_seas
  along <- prior$specific$along
  matrix_along_by_effect <- make_matrix_along_by_effect(along = along,
                                                        dimnames_term = dimnames_term,
                                                        var_time = var_time,
                                                        var_age = var_age)
  s <- seq_len(n_seas)
  n_by <- ncol(matrix_along_by_effect)
  if (n_by > 1L) {
    nms_by <- colnames(matrix_along_by_effect)
    paste(s,
          rep(nms_by, each = n_seas),
          sep = ".")
  }
  else
    as.character(s)
}

## HAS_TESTS
#' @export
levels_hyperrand.bage_prior_rw2seasvary <- function(prior, dimnames_term,
                                                    var_time,
                                                    var_age, levels_effect) {
  levels_effect
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
  uses_along <- uses_along(prior)
  if (n_dim == 0L) {
    ans <- matrix(0L, nrow = 1L)
  }
  else {
    if (uses_along) {
      along <- prior$specific$along
      i_along <- make_i_along(along = along,
                              dimnames_term = dimnames_term,
                              var_time = var_time,
                              var_age = var_age)
    }
    else
      i_along <- 1L
    ans <- make_matrix_along_by_inner(i_along = i_along,
                                      dimnames_term = dimnames_term)
  }
  ans
}

## HAS_TESTS
#' @export
make_matrix_along_by_effectfree.bage_prior_spline <- function(prior,
                                                              dimnames_term,
                                                              var_time,
                                                              var_age,
                                                              var_sexgender) {
  along <- prior$specific$along
  i_along <- make_i_along(along = along,
                          dimnames_term = dimnames_term,
                          var_time = var_time,
                          var_age = var_age)
  n_along <- length(dimnames_term[[i_along]])
  n_comp <- get_n_comp_spline(prior = prior,
                              n_along = n_along)
  labels_along <- paste0("comp", seq_len(n_comp))
  dimnames_term[[i_along]] <- labels_along
  make_matrix_along_by_inner(i_along = i_along,
                             dimnames_term = dimnames_term)
}


#' @export
make_matrix_along_by_effectfree.bage_prior_svd <- function(prior,
                                                           dimnames_term,
                                                           var_time,
                                                           var_age,
                                                           var_sexgender) {
  make_matrix_along_by_effectfree_svd(prior = prior,
                                      dimnames_term = dimnames_term,
                                      var_time = var_time,
                                      var_age = var_age,
                                      var_sexgender = var_sexgender)
}

#' @export
make_matrix_along_by_effectfree.bage_prior_svd_ar <- function(prior,
                                                              dimnames_term,
                                                              var_time,
                                                              var_age,
                                                              var_sexgender) {
  make_matrix_along_by_effectfree_svd(prior = prior,
                                      dimnames_term = dimnames_term,
                                      var_time = var_time,
                                      var_age = var_age,
                                      var_sexgender = var_sexgender)
}

#' @export
make_matrix_along_by_effectfree.bage_prior_svd_rw <- function(prior,
                                                              dimnames_term,
                                                              var_time,
                                                              var_age,
                                                              var_sexgender) {
  make_matrix_along_by_effectfree_svd(prior = prior,
                                      dimnames_term = dimnames_term,
                                      var_time = var_time,
                                      var_age = var_age,
                                      var_sexgender = var_sexgender)
}

#' @export
make_matrix_along_by_effectfree.bage_prior_svd_rw2 <- function(prior,
                                                               dimnames_term,
                                                               var_time,
                                                               var_age,
                                                               var_sexgender) {
  make_matrix_along_by_effectfree_svd(prior = prior,
                                      dimnames_term = dimnames_term,
                                      var_time = var_time,
                                      var_age = var_age,
                                      var_sexgender = var_sexgender)
}


## 'make_matrix_effectfree_effect' --------------------------------------------------

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
make_matrix_effectfree_effect.bage_prior_spline <- function(prior,
                                                            dimnames_term,
                                                            var_time,
                                                            var_age,
                                                            var_sexgender) {
  along <- prior$specific$along
  matrix_along_by_effect <- make_matrix_along_by_effect(along = along,
                                                        dimnames_term = dimnames_term,
                                                        var_time = var_time,
                                                        var_age = var_age)
  n_along <- nrow(matrix_along_by_effect)
  n_by <- ncol(matrix_along_by_effect)
  n_comp <- get_n_comp_spline(prior = prior,
                              n_along = n_along)
  X <- make_spline_matrix(n_comp = n_comp,
                          n_along = n_along)
  I <- Matrix::.sparseDiagonal(n_by)
  X_all_by <- Matrix::kronecker(I, X)
  matrix_alongfirst_to_standard <- make_index_matrix(matrix_along_by_effect)
  matrix_alongfirst_to_standard %*% X_all_by
}

## HAS_TESTS
#' @export
make_matrix_effectfree_effect.bage_prior_svd <- function(prior,
                                                         dimnames_term,
                                                         var_time,
                                                         var_age,
                                                         var_sexgender) {
  make_matrix_effectfree_effect_svd(prior = prior,
                                    dimnames_term = dimnames_term,
                                    var_time = var_time,
                                    var_age = var_age,
                                    var_sexgender = var_sexgender)
}

## HAS_TESTS
#' @export
make_matrix_effectfree_effect.bage_prior_svd_ar <- function(prior,
                                                            dimnames_term,
                                                            var_time,
                                                            var_age,
                                                            var_sexgender) {
  make_matrix_effectfree_effect_svd(prior = prior,
                                    dimnames_term = dimnames_term,
                                    var_time = var_time,
                                    var_age = var_age,
                                    var_sexgender = var_sexgender)
}

## HAS_TESTS
#' @export
make_matrix_effectfree_effect.bage_prior_svd_rw <- function(prior,
                                                            dimnames_term,
                                                            var_time,
                                                            var_age,
                                                            var_sexgender) {
  make_matrix_effectfree_effect_svd(prior = prior,
                                    dimnames_term = dimnames_term,
                                    var_time = var_time,
                                    var_age = var_age,
                                    var_sexgender = var_sexgender)
}

## HAS_TESTS
#' @export
make_matrix_effectfree_effect.bage_prior_svd_rw2 <- function(prior,
                                                             dimnames_term,
                                                             var_time,
                                                             var_age,
                                                             var_sexgender) {
  make_matrix_effectfree_effect_svd(prior = prior,
                                    dimnames_term = dimnames_term,
                                    var_time = var_time,
                                    var_age = var_age,
                                    var_sexgender = var_sexgender)
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
                                                            var_sexgender)
  make_offset_effectfree_effect_svd(prior = prior,
                                    dimnames_term = dimnames_term,
                                    var_time = var_time,
                                    var_age = var_age,
                                    var_sexgender = var_sexgender)

## HAS_TESTS
#' @export
make_offset_effectfree_effect.bage_prior_svd_rw <- function(prior,
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
make_offset_effectfree_effect.bage_prior_svd_rw2 <- function(prior,
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
  nms <- c("min", "max", "s", "along")
  slots <- c("min", "max", "scale", "along")
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
              nms = c("s", "sd", "along"),
              slots = c("scale", "sd_slope", "along"))
}

## HAS_TESTS
#' @export
print.bage_prior_linar <- function(x, ...) {
  nms <- c("s", "sd", "min", "max", "along")
  slots <- c("scale", "sd_slope", "min", "max", "along")
  nm <- x$specific$nm
  if (identical(nm, "Lin_AR")) {
    nms <- c("n_coef", nms)
    slots <- c("n_coef", slots)
  }
  print_prior(x, nms = nms, slots = slots)
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
print.bage_prior_rw <- function(x, ...) {
  print_prior(x,
              nms = c("s", "along"),
              slots = c("scale", "along"))
}

## HAS_TESTS
#' @export
print.bage_prior_rwseasfix <- function(x, ...) {
  n_offset <- get_print_prior_n_offset()
  print_prior_header(x)
  print_prior_slot(prior = x, nm = "n_seas", slot = "n_seas")
  print_prior_slot(prior = x, nm = "s", slot = "scale")
  cat(sprintf("% *s: %s\n", n_offset, "s_seas", 0))
  print_prior_slot(prior = x, nm = "along", slot = "along")
  invisible(x)
}

## HAS_TESTS
#' @export
print.bage_prior_rwseasvary <- function(x, ...) {
  print_prior(x,
              nms = c("n", "s", "s_seas", "along"),
              slots = c("n", "scale", "scale_seas", "along"))
}

## HAS_TESTS
#' @export
print.bage_prior_rw2 <- function(x, ...) {
  print_prior(x,
              nms = c("s", "along"),
              slots = c("scale", "along"))
}

## HAS_TESTS
#' @export
print.bage_prior_rw2seasfix <- function(x, ...) {
  n_offset <- get_print_prior_n_offset()
  print_prior_header(x)
  print_prior_slot(prior = x, nm = "n_seas", slot = "n_seas")
  print_prior_slot(prior = x, nm = "s", slot = "scale")
  cat(sprintf("% *s: %s\n", n_offset, "s_seas", 0))
  print_prior_slot(prior = x, nm = "along", slot = "along")
  invisible(x)
}

## HAS_TESTS
#' @export
print.bage_prior_rw2seasvary <- function(x, ...) {
  print_prior(x,
              nms = c("n_seas", "s", "s_seas", "along"),
              slots = c("n_seas", "scale", "scale_seas", "along"))
}

## HAS_TESTS
#' @export
print.bage_prior_spline <- function(x, ...) {
  print_prior(x,
              nms = c("n_comp", "s", "along"),
              slots = c("n_comp", "scale", "along"))
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
  invisible(x)
}

## HAS_TESTS
#' @export
print.bage_prior_svd_rw <- function(x, ...) {
  n_offset <- get_print_prior_n_offset()
  print_prior_header(x)
  print_prior_slot(prior = x, nm = "ssvd", slot = "nm_ssvd")
  print_prior_slot(prior = x, nm = "n_comp", slot = "n_comp")
  indep <- x$specific$indep
  if (!indep)
    print_prior_slot(prior = x, nm = "indep", slot = "indep")
  print_prior_slot(prior = x, nm = "s", slot = "scale")
  invisible(x)
}

## HAS_TESTS
#' @export
print.bage_prior_svd_rw2 <- function(x, ...) {
  n_offset <- get_print_prior_n_offset()
  print_prior_header(x)
  print_prior_slot(prior = x, nm = "ssvd", slot = "nm_ssvd")
  print_prior_slot(prior = x, nm = "n_comp", slot = "n_comp")
  indep <- x$specific$indep
  if (!indep)
    print_prior_slot(prior = x, nm = "indep", slot = "indep")
  print_prior_slot(prior = x, nm = "s", slot = "scale")
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
  args <- c(args_ar, args_along)
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
  args <- c(args_scale, args_lin, args_along)
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
  args <- c(args_ar, args_lin, args_along)
  args <- args[nzchar(args)]
  args <- paste(args, collapse = ",")
  sprintf("%s(%s)", nm, args)
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
  sd <- prior$specific$sd
  if (isTRUE(all.equal(sd, 1)))
    "NFix()"
  else
    sprintf("NFix(sd=%s)", sd)
}

## HAS_TESTS
#' @export
str_call_prior.bage_prior_rw <- function(prior) {
  args_scale <- str_call_args_scale(prior)
  args_along <- str_call_args_along(prior)
  args <- c(args_scale, args_along)
  args <- args[nzchar(args)]
  args <- paste(args, collapse = ",")
  sprintf("RW(%s)", args)
}

## HAS_TESTS
#' @export
str_call_prior.bage_prior_rwseasfix <- function(prior) {
  args_n_seas <- str_call_args_n_seas(prior)
  args_scale <- str_call_args_scale(prior)
  args_s_seas <- "s_seas=0"
  args_along <- str_call_args_along(prior)
  args <- c(args_n_seas, args_scale, args_s_seas, args_along)
  args <- args[nzchar(args)]
  args <- paste(args, collapse = ",")
  sprintf("RW_Seas(%s)", args)
}

## HAS_TESTS
#' @export
str_call_prior.bage_prior_rwseasvary <- function(prior) {
  args_n_seas <- str_call_args_n_seas(prior)
  args_scale <- str_call_args_scale(prior)
  args_s_seas <- str_call_args_s_seas(prior)
  args_along <- str_call_args_along(prior)
  args <- c(args_n_seas, args_scale, args_s_seas, args_along)
  args <- args[nzchar(args)]
  args <- paste(args, collapse = ",")
  sprintf("RW_Seas(%s)", args)
}

## HAS_TESTS
#' @export
str_call_prior.bage_prior_rw2 <- function(prior) {
  args_scale <- str_call_args_scale(prior)
  args_along <- str_call_args_along(prior)
  args <- c(args_scale, args_along)
  args <- args[nzchar(args)]
  args <- paste(args, collapse = ",")
  sprintf("RW2(%s)", args)
}

## HAS_TESTS
#' @export
str_call_prior.bage_prior_rw2seasfix <- function(prior) {
  args_n_seas <- str_call_args_n_seas(prior)
  args_scale <- str_call_args_scale(prior)
  args_s_seas <- "s_seas=0"
  args_along <- str_call_args_along(prior)
  args <- c(args_n_seas, args_scale, args_s_seas, args_along)
  args <- args[nzchar(args)]
  args <- paste(args, collapse = ",")
  sprintf("RW2_Seas(%s)", args)
}

## HAS_TESTS
#' @export
str_call_prior.bage_prior_rw2seasvary <- function(prior) {
  args_n_seas <- str_call_args_n_seas(prior)
  args_scale <- str_call_args_scale(prior)
  args_s_seas <- str_call_args_s_seas(prior)
  args_along <- str_call_args_along(prior)
  args <- c(args_n_seas, args_scale, args_s_seas, args_along)
  args <- args[nzchar(args)]
  args <- paste(args, collapse = ",")
  sprintf("RW2_Seas(%s)", args)
}

## HAS_TESTS
#' @export
str_call_prior.bage_prior_spline <- function(prior) {
  args_n_comp <- str_call_args_n_comp(prior)
  args_scale <- str_call_args_scale(prior)
  args_along <- str_call_args_along(prior)
  args <- c(args_n_comp, args_scale, args_along)
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
  args <- c(args_svd, args_ar, args_along)
  args <- args[nzchar(args)]
  args <- paste(args, collapse = ",")
  sprintf("%s(%s)", nm, args)
}

## HAS_TESTS
#' @export
str_call_prior.bage_prior_svd_rw <- function(prior) {
  args_svd <- str_call_args_svd(prior)
  args_scale <- str_call_args_scale(prior)
  args_along <- str_call_args_along(prior)
  args <- c(args_svd, args_scale, args_along)
  args <- args[nzchar(args)]
  args <- paste(args, collapse = ",")
  sprintf("SVD_RW(%s)", args)
}

## HAS_TESTS
#' @export
str_call_prior.bage_prior_svd_rw2 <- function(prior) {
  args_svd <- str_call_args_svd(prior)
  args_scale <- str_call_args_scale(prior)
  args_along <- str_call_args_along(prior)
  args <- c(args_svd, args_scale, args_along)
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
str_nm_prior.bage_prior_rw <- function(prior) {
  "RW()"
}

## HAS_TESTS
#' @export
str_nm_prior.bage_prior_rwseasfix <- function(prior) {
  "RW_Seas()"
}

## HAS_TESTS
#' @export
str_nm_prior.bage_prior_rwseasvary <- function(prior) {
  "RW_Seas()"
}

## HAS_TESTS
#' @export
str_nm_prior.bage_prior_rw2 <- function(prior) {
  "RW2()"
}

## HAS_TESTS
#' @export
str_nm_prior.bage_prior_rw2seasfix <- function(prior) {
  "RW2_Seas()"
}

## HAS_TESTS
#' @export
str_nm_prior.bage_prior_rw2seasvary <- function(prior) {
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
str_nm_prior.bage_prior_svd_rw <- function(prior) {
  "SVD_RW()"
}

## HAS_TESTS
#' @export
str_nm_prior.bage_prior_svd_rw2 <- function(prior) {
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
transform_hyper.bage_prior_linar <- function(prior) {
  specific <- prior$specific
  n_coef <- specific$n_coef
  min <- specific$min
  max <- specific$max
  shifted_inv_logit <- function(x) {
    ans_raw <- exp(x) / (1 + exp(x))
    ans <- (max - min) * ans_raw + min
    ans
  }
  rep(list(sd = exp,
           coef = shifted_inv_logit),
      times = c(1L, n_coef))
}

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
transform_hyper.bage_prior_rw <- function(prior)
  list(sd = exp)

## HAS_TESTS
#' @export
transform_hyper.bage_prior_rwseasfix <- function(prior)
  list(sd = exp)

## HAS_TESTS
#' @export
transform_hyper.bage_prior_rwseasvary <- function(prior)
  list(sd_seas = exp,
       sd = exp)

## HAS_TESTS
#' @export
transform_hyper.bage_prior_rw2 <- function(prior)
  list(sd = exp)

## HAS_TESTS
#' @export
transform_hyper.bage_prior_rw2seasfix <- function(prior)
  list(sd = exp)

## HAS_TESTS
#' @export
transform_hyper.bage_prior_rw2seasvary <- function(prior)
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
transform_hyper.bage_prior_svd_rw <- function(prior)
  list(sd = exp)

## HAS_TESTS
#' @export
transform_hyper.bage_prior_svd_rw2 <- function(prior)
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
uses_along.bage_prior_norm <- function(prior) FALSE

## HAS_TESTS
#' @export
uses_along.bage_prior_normfixed <- function(prior) FALSE

## HAS_TESTS
#' @export
uses_along.bage_prior_rw <- function(prior) TRUE

## HAS_TESTS
#' @export
uses_along.bage_prior_rwseasfix <- function(prior) TRUE

## HAS_TESTS
#' @export
uses_along.bage_prior_rwseasvary <- function(prior) TRUE

## HAS_TESTS
#' @export
uses_along.bage_prior_rw2 <- function(prior) TRUE

## HAS_TESTS
#' @export
uses_along.bage_prior_rw2seasfix <- function(prior) TRUE

## HAS_TESTS
#' @export
uses_along.bage_prior_rw2seasvary <- function(prior) TRUE

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
uses_along.bage_prior_svd_rw <- function(prior) TRUE

## HAS_TESTS
#' @export
uses_along.bage_prior_svd_rw2 <- function(prior) TRUE


## 'uses_hyperrand' -----------------------------------------------------------

#' Whether Prior Uses Hyper-Paremters that
#' Can Be Treated As Random Effects
#'
#' @param prior Object of class 'bage_prior'
#'
#' @returns TRUE or FALSE.
#'
#' @noRd
uses_hyperrand <- function(prior) {
  UseMethod("uses_hyperrand")
}

## HAS_TESTS
#' @export
uses_hyperrand.bage_prior <- function(prior) FALSE

## HAS_TESTS
#' @export
uses_hyperrand.bage_prior_lin <- function(prior) TRUE

## HAS_TESTS
#' @export
uses_hyperrand.bage_prior_linar <- function(prior) TRUE

## HAS_TESTS
#' @export
uses_hyperrand.bage_prior_rwseasfix <- function(prior) TRUE

## HAS_TESTS
#' @export
uses_hyperrand.bage_prior_rwseasvary <- function(prior) TRUE

## HAS_TESTS
#' @export
uses_hyperrand.bage_prior_rw2seasfix <- function(prior) TRUE

## HAS_TESTS
#' @export
uses_hyperrand.bage_prior_rw2seasvary <- function(prior) TRUE


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
uses_matrix_effectfree_effect.bage_prior_spline <- function(prior) TRUE

## HAS_TESTS
#' @export
uses_matrix_effectfree_effect.bage_prior_svd <- function(prior) TRUE

## HAS_TESTS
#' @export
uses_matrix_effectfree_effect.bage_prior_svd_ar <- function(prior) TRUE

## HAS_TESTS
#' @export
uses_matrix_effectfree_effect.bage_prior_svd_rw <- function(prior) TRUE

## HAS_TESTS
#' @export
uses_matrix_effectfree_effect.bage_prior_svd_rw2 <- function(prior) TRUE


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
uses_offset_effectfree_effect.bage_prior_svd_rw <- function(prior) TRUE

## HAS_TESTS
#' @export
uses_offset_effectfree_effect.bage_prior_svd_rw2 <- function(prior) TRUE


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
