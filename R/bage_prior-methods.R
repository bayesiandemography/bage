
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
#' @param levels_effect Character vector with labels for effect
#' @param levels_age Values taken by age
#' variable (or NULL if no age variable in data)
#' @param levels_sexgender Values taken by sex/gender
#' variable (or NULL if no sex/gender variable in data)
#' @param agesex String. One of "age", "age:sex",
#' "sex:age" or "other"
#' @param matrix_along_by Matrix with mapping for along, by dimensions
#' @param matrix_agesex Matrix with mapping between age-sex dimensions
#' and full term
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
                             levels_effect,
                             levels_age,
                             levels_sexgender,
                             agesex,
                             matrix_along_by,
                             matrix_agesex,
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
                                           levels_effect,
                                           levels_age,
                                           levels_sexgender,
                                           agesex,
                                           matrix_along_by,
                                           matrix_agesex,
                                           n_sim) {
  coef <- vals_hyper$coef
  sd <- vals_hyper$sd
  draw_vals_ar(coef = coef,
               sd = sd,
               matrix_along_by = matrix_along_by,
               levels_effect = levels_effect)
}


## HAS_TESTS
#' @export
draw_vals_effect.bage_prior_known <- function(prior,
                                              vals_hyper,
                                              vals_hyperrand,
                                              vals_spline,
                                              vals_svd,
                                              levels_effect,
                                              levels_age,
                                              levels_sexgender,
                                              agesex,
                                              matrix_along_by,
                                              matrix_agesex,
                                              n_sim) {
  values <- prior$specific$values
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
                                            levels_effect,
                                            levels_age,
                                            levels_sexgender,
                                            agesex,
                                            matrix_along_by,
                                            matrix_agesex,
                                            n_sim) {
  slope <- vals_hyperrand$slope
  sd <- vals_hyper$sd
  draw_vals_lin(slope = slope,
                sd = sd,
                matrix_along_by = matrix_along_by,
                labels = levels_effect)
}

## HAS_TESTS
#' @export
draw_vals_effect.bage_prior_linar <- function(prior,
                                              vals_hyper,
                                              vals_hyperrand,
                                              vals_spline,
                                              vals_svd,
                                              levels_effect,
                                              levels_age,
                                              levels_sexgender,
                                              agesex,
                                              matrix_along_by,
                                              matrix_agesex,
                                              n_sim) {
  slope <- vals_hyperrand$slope
  coef <- vals_hyper$coef
  sd <- vals_hyper$sd
  draw_vals_linar(slope = slope,
                  sd = sd,
                  coef = coef,
                  matrix_along_by = matrix_along_by,
                  labels = levels_effect)
}

## HAS_TESTS
#' @export
draw_vals_effect.bage_prior_norm <- function(prior,
                                             vals_hyper,
                                             vals_hyperrand,
                                             vals_spline,
                                             vals_svd,
                                             levels_effect,
                                             levels_age,
                                             levels_sexgender,
                                             agesex,
                                             matrix_along_by,
                                             matrix_agesex,
                                             n_sim) {
  sd <- vals_hyper$sd
  n_along <- nrow(matrix_along_by) ## in case being used as error
  n_by <- ncol(matrix_along_by)    ## term in an interaction
  n_effect <- n_along * n_by
  n <- n_effect * n_sim
  sd <- rep(sd, each = n_effect)
  ans <- stats::rnorm(n = n, sd = sd)
  ans <- matrix(ans, nrow = n_effect, ncol = n_sim)
  i <- match(sort(matrix_along_by), matrix_along_by)
  ans <- ans[i, , drop = FALSE]
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
                                                  levels_effect,
                                                  levels_age,
                                                  levels_sexgender,
                                                  agesex,
                                                  matrix_along_by,
                                                  matrix_agesex,
                                                  n_sim) {
  sd <- prior$specific$sd
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
                                           levels_effect,
                                           levels_age,
                                           levels_sexgender,
                                           agesex,
                                           matrix_along_by,
                                           matrix_agesex,
                                           n_sim) {
  sd <- vals_hyper$sd
  draw_vals_rw(sd = sd,
               matrix_along_by = matrix_along_by,
               levels_effect = levels_effect)
}

## HAS_TESTS
#' @export
draw_vals_effect.bage_prior_rwseasfix <- function(prior,
                                                  vals_hyper,
                                                  vals_hyperrand,
                                                  vals_spline,
                                                  vals_svd,
                                                  levels_effect,
                                                  levels_age,
                                                  levels_sexgender,
                                                  agesex,
                                                  matrix_along_by,
                                                  matrix_agesex,
                                                  n_sim) {
  sd <- vals_hyper$sd
  seas <- vals_hyperrand$seas
  alpha <- draw_vals_rw(sd = sd,
                        matrix_along_by = matrix_along_by,
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
                                                   levels_effect,
                                                   levels_age,
                                                   levels_sexgender,
                                                   agesex,
                                                   matrix_along_by,
                                                   matrix_agesex,
                                                   n_sim) {
  sd <- vals_hyper$sd
  seas <- vals_hyperrand$seas
  alpha <- draw_vals_rw(sd = sd,
                        matrix_along_by = matrix_along_by,
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
                                            levels_effect,
                                            levels_age,
                                            levels_sexgender,
                                            agesex,
                                            matrix_along_by,
                                            matrix_agesex,
                                            n_sim) {
  sd <- vals_hyper$sd
  draw_vals_rw2(sd = sd,
                matrix_along_by = matrix_along_by,
                levels_effect = levels_effect)
}

## HAS_TESTS
#' @export
draw_vals_effect.bage_prior_rw2seasfix <- function(prior,
                                                   vals_hyper,
                                                   vals_hyperrand,
                                                   vals_spline,
                                                   vals_svd,
                                                   levels_effect,
                                                   levels_age,
                                                   levels_sexgender,
                                                   agesex,
                                                   matrix_along_by,
                                                   matrix_agesex,
                                                   n_sim) {
  sd <- vals_hyper$sd
  seas <- vals_hyperrand$seas
  alpha <- draw_vals_rw2(sd = sd,
                         matrix_along_by = matrix_along_by,
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
                                                    levels_effect,
                                                    levels_age,
                                                    levels_sexgender,
                                                    agesex,
                                                    matrix_along_by,
                                                    matrix_agesex,
                                                    n_sim) {
  sd <- vals_hyper$sd
  seas <- vals_hyperrand$seas
  alpha <- draw_vals_rw2(sd = sd,
                         matrix_along_by = matrix_along_by,
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
                                               levels_effect,
                                               levels_age,
                                               levels_sexgender,
                                               agesex,
                                               matrix_along_by,
                                               matrix_agesex,
                                               n_sim) {
  m <- make_matrix_effectfree_effect(prior = prior,
                                     levels_effect = levels_effect,
                                     agesex = agesex,
                                     levels_age = levels_age,
                                     levels_sexgender = levels_sexgender,
                                     matrix_along_by = matrix_along_by,
                                     matrix_agesex = matrix_agesex)
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
                                            levels_effect,
                                            levels_age,
                                            levels_sexgender,
                                            agesex,
                                            matrix_along_by,
                                            matrix_agesex,
                                            n_sim) {
  draw_vals_effect_svd(prior = prior,
                       vals_svd = vals_svd,
                       levels_age = levels_age,
                       levels_sexgender = levels_sexgender,
                       agesex = agesex,
                       matrix_agesex = matrix_agesex,
                       levels_effect = levels_effect)
}

## HAS_TESTS
#' @export
draw_vals_effect.bage_prior_svd_ar <- function(prior,
                                               vals_hyper,
                                               vals_hyperrand,
                                               vals_spline,
                                               vals_svd,
                                               levels_effect,
                                               levels_age,
                                               levels_sexgender,
                                               agesex,
                                               matrix_along_by,
                                               matrix_agesex,
                                               n_sim) {
  draw_vals_effect_svd(prior = prior,
                       vals_svd = vals_svd,
                       levels_age = levels_age,
                       levels_sexgender = levels_sexgender,
                       agesex = agesex,
                       matrix_agesex = matrix_agesex,
                       levels_effect = levels_effect)
}

## HAS_TESTS
#' @export
draw_vals_effect.bage_prior_svd_rw <- function(prior,
                                               vals_hyper,
                                               vals_hyperrand,
                                               vals_spline,
                                               vals_svd,
                                               levels_effect,
                                               levels_age,
                                               levels_sexgender,
                                               agesex,
                                               matrix_along_by,
                                               matrix_agesex,
                                               n_sim) {
  draw_vals_effect_svd(prior = prior,
                       vals_svd = vals_svd,
                       levels_age = levels_age,
                       levels_sexgender = levels_sexgender,
                       agesex = agesex,
                       matrix_agesex = matrix_agesex,
                       levels_effect = levels_effect)
}

## HAS_TESTS
#' @export
draw_vals_effect.bage_prior_svd_rw2 <- function(prior,
                                                vals_hyper,
                                                vals_hyperrand,
                                                vals_spline,
                                                vals_svd,
                                                levels_effect,
                                                levels_age,
                                                levels_sexgender,
                                                agesex,
                                                matrix_along_by,
                                                matrix_agesex,
                                                n_sim) {
  draw_vals_effect_svd(prior = prior,
                       vals_svd = vals_svd,
                       levels_age = levels_age,
                       levels_sexgender = levels_sexgender,
                       agesex = agesex,
                       matrix_agesex = matrix_agesex,
                       levels_effect = levels_effect)
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
#' @param matrix_along_by Matrix with map for along and by dimensions
#' @param n_sim Number of simulation draws
#'
#' @returns A named list.
#'
#' @noRd
draw_vals_hyperrand <- function(prior,
                                vals_hyper,
                                matrix_along_by,
                                n_sim) {
  UseMethod("draw_vals_hyperrand")
}

## HAS_TESTS
#' @export
draw_vals_hyperrand.bage_prior <- function(prior,
                                           vals_hyper,
                                           matrix_along_by,
                                           n_sim) {
  list()
}

## HAS_TESTS
#' @export
draw_vals_hyperrand.bage_prior_lin <- function(prior,
                                               vals_hyper,
                                               matrix_along_by,
                                               n_sim) {
  sd_slope <- prior$const[["sd_slope"]]
  slope <- draw_vals_slope(sd_slope = sd_slope,
                           matrix_along_by = matrix_along_by,
                           n_sim = n_sim)
  list(slope = slope)
}

## HAS_TESTS
#' @export
draw_vals_hyperrand.bage_prior_linar <- function(prior,
                                                 vals_hyper,
                                                 matrix_along_by,
                                                 n_sim) {
  sd_slope <- prior$const[["sd_slope"]]
  slope <- draw_vals_slope(sd_slope = sd_slope,
                           matrix_along_by = matrix_along_by,
                           n_sim = n_sim)
  list(slope = slope)
}

## HAS_TESTS
#' @export
draw_vals_hyperrand.bage_prior_rwseasfix <- function(prior,
                                                     vals_hyper,
                                                     matrix_along_by,
                                                     n_sim) {
  n_seas <- prior$specific$n_seas
  seas <- draw_vals_seasfix(n_seas = n_seas,
                            matrix_along_by = matrix_along_by,
                            n_sim = n_sim)
  list(seas = seas)
}

## HAS_TESTS
#' @export
draw_vals_hyperrand.bage_prior_rwseasvary <- function(prior,
                                                      vals_hyper,
                                                      matrix_along_by,
                                                      n_sim) {
  n_seas <- prior$specific$n_seas
  sd_seas <- vals_hyper$sd_seas
  seas <- draw_vals_seasvary(n_seas = n_seas,
                             sd_seas = sd_seas,
                             matrix_along_by = matrix_along_by)
  list(seas = seas)
}

## HAS_TESTS
#' @export
draw_vals_hyperrand.bage_prior_rw2seasfix <- function(prior,
                                                      vals_hyper,
                                                      matrix_along_by,
                                                      n_sim) {
  n_seas <- prior$specific$n_seas
  seas <- draw_vals_seasfix(n_seas = n_seas,
                            matrix_along_by = matrix_along_by,
                            n_sim = n_sim)
  list(seas = seas)
}

## HAS_TESTS
#' @export
draw_vals_hyperrand.bage_prior_rw2seasvary <- function(prior,
                                                       vals_hyper,
                                                       matrix_along_by,
                                                       n_sim) {
  n_seas <- prior$specific$n_seas
  sd_seas <- vals_hyper$sd_seas
  seas <- draw_vals_seasvary(n_seas = n_seas,
                             sd_seas = sd_seas,
                             matrix_along_by = matrix_along_by)
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
#' @param matrix_along_by_free 'matrix_along_by' for free parameters
#' @param levels_effect Character vector with labels for free parameters
#' @param n_sim Number of simulation draws
#'
#' @returns A matrix or NULL
#'
#' @noRd
draw_vals_spline <- function(prior,
                             vals_hyper,
                             matrix_along_by_free,
                             levels_effectfree,
                             n_sim) {
  UseMethod("draw_vals_spline")
}



## HAS_TESTS
#' @export
draw_vals_spline.bage_prior <- function(prior,
                                        vals_hyper,
                                        matrix_along_by_free,
                                        levels_effectfree,
                                        n_sim) {
  NULL
}

## HAS_TESTS
#' @export
draw_vals_spline.bage_prior_spline <- function(prior,
                                               vals_hyper,
                                               matrix_along_by_free,
                                               levels_effectfree,
                                               n_sim) {
  sd <- vals_hyper$sd
  draw_vals_rw2(sd = sd,
                matrix_along_by = matrix_along_by_free,
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
#' @param matrix_along_by_free 'matrix_along_by' for free parameters
#' @param levels_effect Character vector with labels for free parameters
#' @param n_sim Number of simulation draws
#'
#' @returns A matrix or NULL
#'
#' @noRd
draw_vals_svd <- function(prior,
                          vals_hyper,
                          matrix_along_by_free,
                          levels_effectfree,
                          n_sim) {
  UseMethod("draw_vals_svd")
}



## HAS_TESTS
#' @export
draw_vals_svd.bage_prior <- function(prior,
                                     vals_hyper,
                                     matrix_along_by_free,
                                     levels_effectfree,
                                     n_sim) {
  NULL
}

## HAS_TESTS
#' @export
draw_vals_svd.bage_prior_svd <- function(prior,
                                         vals_hyper,
                                         matrix_along_by_free,
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
                                            matrix_along_by_free,
                                            levels_effectfree,
                                            n_sim) {
  coef <- vals_hyper$coef
  sd <- vals_hyper$sd
  draw_vals_ar(coef = coef,
               sd = sd,
               matrix_along_by = matrix_along_by_free,
               levels_effect = levels_effectfree)
}

## HAS_TESTS
#' @export
draw_vals_svd.bage_prior_svd_rw <- function(prior,
                                            vals_hyper,
                                            matrix_along_by_free,
                                            levels_effectfree,
                                            n_sim) {
  sd <- vals_hyper$sd
  draw_vals_rw(sd = sd,
               matrix_along_by = matrix_along_by_free,
               levels_effect = levels_effectfree)
}

## HAS_TESTS
#' @export
draw_vals_svd.bage_prior_svd_rw2 <- function(prior,
                                             vals_hyper,
                                             matrix_along_by_free,
                                             levels_effectfree,
                                             n_sim) {
  sd <- vals_hyper$sd
  draw_vals_rw2(sd = sd,
                matrix_along_by = matrix_along_by_free,
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
#' @param nm_prior Name of the term
#' @param components Tibble with with output
#' from function 'components'
#' @param matrix_along_by_est Matrix mapping
#' along and by dimensions to position in estiamtes
#' @param matrix_along_by_forecast Matrix mapping
#' along and by dimensions to position in forecasts
#' @param levels_forecast Labels for elements
#' of forecasted term
#'
#' @returns A tibble
#'
#' @noRd
forecast_term <- function(prior,
                          nm_prior,
                          components,
                          matrix_along_by_est,
                          matrix_along_by_forecast,
                          levels_forecast) {
  UseMethod("forecast_term")
}

## HAS_TESTS
#' @export
forecast_term.bage_prior <- function(prior,
                                     nm_prior,
                                     components,
                                     matrix_along_by_est,
                                     matrix_along_by_forecast,
                                     levels_forecast) {
  nm_split <- strsplit(nm_prior, split = ":")[[1L]]
  cli::cli_abort(c("Can't forecast term {.val {nm_prior}}.",
                   i = "Term {.val {nm_prior}} has a {.val {str_nm_prior(prior)}} prior.",
                   i = "Terms with a {.val {str_nm_prior(prior)}} prior cannot be forecasted.",
                   i = "For a list of priors that can be forecasted, see {.topic bage::priors}."))
}

## HAS_TESTS
#' @export
forecast_term.bage_prior_ar <- function(prior,
                                        nm_prior,
                                        components,
                                        matrix_along_by_est,
                                        matrix_along_by_forecast,
                                        levels_forecast) {
  is_ar <- with(components,
                term == nm_prior & component == "effect")
  is_coef <- with(components,
                  term == nm_prior & component == "hyper" & startsWith(level, "coef"))
  is_sd <- with(components,
                term == nm_prior & component == "hyper" & level == "sd")
  ar_est <- components$.fitted[is_ar]
  coef <- components$.fitted[is_coef]
  sd <- components$.fitted[is_sd]
  ar_forecast <- forecast_ar(ar_est = ar_est,
                             coef = coef,
                             sd = sd,
                             matrix_along_by_est = matrix_along_by_est,
                             matrix_along_by_forecast = matrix_along_by_forecast)
  tibble::tibble(term = nm_prior,
                 component = "effect",
                 level = levels_forecast,
                 .fitted = ar_forecast)
}

## HAS_TESTS
#' @export
forecast_term.bage_prior_lin <- function(prior,
                                         nm_prior,
                                         components,
                                         matrix_along_by_est,
                                         matrix_along_by_forecast,
                                         levels_forecast) {
  is_slope <- with(components,
                   term == nm_prior & component == "hyper" & startsWith(level, "slope"))
  is_sd <- with(components,
                term == nm_prior & component == "hyper" & level == "sd")
  slope <- components$.fitted[is_slope]
  sd <- components$.fitted[is_sd]
  lin_forecast <- forecast_lin(slope = slope,
                               matrix_along_by_est = matrix_along_by_est,
                               matrix_along_by_forecast = matrix_along_by_forecast)
  error_forecast <- forecast_norm(sd = sd,
                                  matrix_along_by_forecast = matrix_along_by_forecast)
  .fitted <- lin_forecast + error_forecast
  tibble::tibble(term = nm_prior,
                 component = "effect",
                 level = levels_forecast,
                 .fitted = .fitted)
}

## HAS_TESTS
#' @export
forecast_term.bage_prior_linar <- function(prior,
                                           nm_prior,
                                           components,
                                           matrix_along_by_est,
                                           matrix_along_by_forecast,
                                           levels_forecast) {
  is_effect <- with(components,
                    term == nm_prior & component == "effect")
  is_coef <- with(components,
                  term == nm_prior & component == "hyper" & startsWith(level, "coef"))
  is_slope <- with(components,
                   term == nm_prior & component == "hyper" & startsWith(level, "slope"))
  is_sd <- with(components,
                term == nm_prior & component == "hyper" & level == "sd")
  effect_est <- components$.fitted[is_effect]
  coef <- components$.fitted[is_coef]
  slope <- components$.fitted[is_slope]
  sd <- components$.fitted[is_sd]
  lin_est <- estimate_lin(slope = slope,
                          matrix_along_by_est = matrix_along_by_est)
  lin_forecast <- forecast_lin(slope = slope,
                               matrix_along_by_est = matrix_along_by_est,
                               matrix_along_by_forecast = matrix_along_by_forecast)
  error_est <- effect_est - lin_est
  error_forecast <- forecast_ar(ar_est = error_est,
                                   coef = coef,
                                   sd = sd,
                                   matrix_along_by_est = matrix_along_by_est,
                                   matrix_along_by_forecast = matrix_along_by_forecast)
  effect_forecast <- lin_forecast + error_forecast
  component <- rep(c("effect", "trend", "cyclical"), each = length(effect_forecast))
  level <- rep(levels_forecast, times = 3L)
  .fitted <- c(effect_forecast, lin_forecast, error_forecast)
  tibble::tibble(term = nm_prior,
                 component = component,
                 level = level,
                 .fitted = .fitted)
}

## HAS_TESTS
#' @export
forecast_term.bage_prior_norm <- function(prior,
                                          nm_prior,
                                          components,
                                          matrix_along_by_est,
                                          matrix_along_by_forecast,
                                          levels_forecast) {
  is_sd <- with(components,
                term == nm_prior & component == "hyper" & level == "sd")
  sd <- components$.fitted[is_sd]
  norm_forecast <- forecast_norm(sd = sd,
                                 matrix_along_by_forecast = matrix_along_by_forecast)
  tibble::tibble(term = nm_prior,
                 component = "effect",
                 level = levels_forecast,
                 .fitted = norm_forecast)
}

## HAS_TESTS
#' @export
forecast_term.bage_prior_normfixed <- function(prior,
                                               nm_prior,
                                               components,
                                               matrix_along_by_est,
                                               matrix_along_by_forecast,
                                               levels_forecast) {
  n <- length(matrix_along_by_forecast)
  sd <- prior$specific$sd
  n_draw <- rvec::n_draw(components$.fitted[[1L]])
  norm_forecast <- rvec::rnorm_rvec(n = n,
                                    sd = sd,
                                    n_draw = n_draw)
  tibble::tibble(term = nm_prior,
                 component = "effect",
                 level = levels_forecast,
                 .fitted = norm_forecast)
}

## HAS_TESTS
#' @export
forecast_term.bage_prior_rw <- function(prior,
                                        nm_prior,
                                        components,
                                        matrix_along_by_est,
                                        matrix_along_by_forecast,
                                        levels_forecast) {
  is_effect <- with(components,
                    term == nm_prior & component == "effect")
  is_sd <- with(components,
                term == nm_prior & component == "hyper" & level == "sd")
  rw_est <- components$.fitted[is_effect]
  sd <- components$.fitted[is_sd]
  rw_forecast <- forecast_rw(rw_est = rw_est,
                             sd = sd,
                             matrix_along_by_est = matrix_along_by_est,
                             matrix_along_by_forecast = matrix_along_by_forecast)
  tibble::tibble(term = nm_prior,
                 component = "effect",
                 level = levels_forecast,
                 .fitted = rw_forecast)
}

## HAS_TESTS
#' @export
forecast_term.bage_prior_rwseasfix <- function(prior,
                                               nm_prior,
                                               components,
                                               matrix_along_by_est,
                                               matrix_along_by_forecast,
                                               levels_forecast) {
  n_seas <- prior$specific$n_seas
  is_trend <- with(components,
                   term == nm_prior & component == "trend")
  is_seasonal <- with(components,
                      term == nm_prior & component == "seasonal")
  is_sd <- with(components,
                term == nm_prior & component == "hyper" & level == "sd")
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
  tibble::tibble(term = nm_prior,
                 component = rep(c("effect", "trend", "seasonal"), each = n_level),
                 level = rep(levels_forecast, times = 3L),
                 .fitted = c(effect_forecast, rw_forecast, seas_forecast))
}

## HAS_TESTS
#' @export
forecast_term.bage_prior_rwseasvary <- function(prior,
                                                nm_prior,
                                                components,
                                                matrix_along_by_est,
                                                matrix_along_by_forecast,
                                                levels_forecast) {
  n_seas <- prior$specific$n_seas
  is_trend <- with(components,
                   term == nm_prior & component == "trend")
  is_seasonal <- with(components,
                      term == nm_prior & component == "seasonal")
  is_sd_seas <- with(components,
                     term == nm_prior & component == "hyper" & level == "sd_seas")
  is_sd <- with(components,
                term == nm_prior & component == "hyper" & level == "sd")
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
  tibble::tibble(term = nm_prior,
                 component = rep(c("effect", "trend", "seasonal"), each = n_level),
                 level = rep(levels_forecast, times = 3L),
                 .fitted = c(effect_forecast, rw_forecast, seas_forecast))
}

## HAS_TESTS
#' @export
forecast_term.bage_prior_rw2 <- function(prior,
                                         nm_prior,
                                         components,
                                         matrix_along_by_est,
                                         matrix_along_by_forecast,
                                         levels_forecast) {
  is_effect <- with(components,
                    term == nm_prior & component == "effect")
  is_sd <- with(components,
                term == nm_prior & component == "hyper" & level == "sd")
  rw2_est <- components$.fitted[is_effect]
  sd <- components$.fitted[is_sd]
  rw2_forecast <- forecast_rw2(rw2_est = rw2_est,
                               sd = sd,
                               matrix_along_by_est = matrix_along_by_est,
                               matrix_along_by_forecast = matrix_along_by_forecast)
  tibble::tibble(term = nm_prior,
                 component = "effect",
                 level = levels_forecast,
                 .fitted = rw2_forecast)
}

## HAS_TESTS
#' @export
forecast_term.bage_prior_rw2seasfix <- function(prior,
                                                nm_prior,
                                                components,
                                                matrix_along_by_est,
                                                matrix_along_by_forecast,
                                                levels_forecast) {
  n_seas <- prior$specific$n_seas
  is_trend <- with(components,
                   term == nm_prior & component == "trend")
  is_seasonal <- with(components,
                      term == nm_prior & component == "seasonal")
  is_sd <- with(components,
                term == nm_prior & component == "hyper" & level == "sd")
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
  tibble::tibble(term = nm_prior,
                 component = rep(c("effect", "trend", "seasonal"), each = n_level),
                 level = rep(levels_forecast, times = 3L),
                 .fitted = c(effect_forecast, rw2_forecast, seas_forecast))
}

## HAS_TESTS
#' @export
forecast_term.bage_prior_rw2seasvary <- function(prior,
                                                 nm_prior,
                                                 components,
                                                 matrix_along_by_est,
                                                 matrix_along_by_forecast,
                                                 levels_forecast) {
  n_seas <- prior$specific$n_seas
  is_trend <- with(components,
                   term == nm_prior & component == "trend")
  is_seasonal <- with(components,
                      term == nm_prior & component == "seasonal")
  is_sd_seas <- with(components,
                     term == nm_prior & component == "hyper" & level == "sd_seas")
  is_sd <- with(components,
                term == nm_prior & component == "hyper" & level == "sd")
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
  tibble::tibble(term = nm_prior,
                 component = rep(c("effect", "trend", "seasonal"), each = n_level),
                 level = rep(levels_forecast, times = 3L),
                 .fitted = c(effect_forecast, rw2_forecast, seas_forecast))
}

## HAS_TESTS
#' @export
forecast_term.bage_prior_svd_ar <- function(prior, 
                                            nm_prior,
                                            components,
                                            matrix_along_by_est,
                                            matrix_along_by_est_free,
                                            matrix_along_by_forecast,
                                            matrix_along_by_forecast_free,
                                            levels_forecast,
                                            levels_forecast_svd) {
  is_svd <- with(components,
                 term == nm_prior & component == "svd")
  is_coef <- with(components,
                  term == nm_prior & component == "hyper" & startsWith(level, "coef"))
  is_sd <- with(components,
                term == nm_prior & component == "hyper" & level == "sd")
  svd_est <- components$.fitted[is_svd]
  coef <- components$.fitted[is_coef]
  sd <- components$.fitted[is_sd]
  svd_forecast <- forecast_ar(ar_est = svd_est,
                              coef = coef,
                              sd = sd,
                              matrix_along_by_est = matrix_along_by_est_free,
                              matrix_along_by_forecast = matrix_along_by_forecast_free)
  forecast_term_svd(prior = prior,
                    nm_prior = nm_prior,
                    svd_forecast = svd_forecast,
                    levels_age = levels_age,
                    levels_sexgender = levels_sexgender,
                    agesex = agesex,
                    matrix_agesex = matrix_agesex,
                    levels_forecast = levels_forecast,
                    levels_forecast_svd = levels_forecast_svd)
}

## HAS_TESTS
#' @export
forecast_term.bage_prior_svd_rw <- function(prior, 
                                            nm_prior,
                                            components,
                                            matrix_along_by_est,
                                            matrix_along_by_forecast,
                                            levels_forecast) {
  is_svd <- with(components,
                 term == nm_prior & component == "svd")
  is_sd <- with(components,
                term == nm_prior & component == "hyper" & level == "sd")
  svd_est <- components$.fitted[is_svd]
  sd <- components$.fitted[is_sd]
  svd_forecast <- forecast_rw(rw_est = svd_est,
                              sd = sd,
                              matrix_along_by_est = matrix_along_by_est_free,
                              matrix_along_by_forecast = matrix_along_by_forecast_free)
  forecast_effect_svd(prior = prior,
                      svd_forecast = svd_forecast,
                      levels_age = levels_age,
                      levels_sexgender = levels_sexgender,
                      agesex = agesex,
                      matrix_agesex = matrix_agesex)
}

## HAS_TESTS
#' @export
forecast_term.bage_prior_svd_rw2 <- function(prior, 
                                             nm_prior,
                                             components,
                                             matrix_along_by_est,
                                             matrix_along_by_forecast,
                                             levels_forecast) {
  is_svd <- with(components,
                 term == nm_prior & component == "svd")
  is_sd <- with(components,
                term == nm_prior & component == "hyper" & level == "sd")
  svd_est <- components$.fitted[is_svd]
  sd <- components$.fitted[is_sd]
  svd_forecast <- forecast_rw2(rw_est = svd_est,
                               sd = sd,
                               matrix_along_by_est = matrix_along_by_est_free,
                               matrix_along_by_forecast = matrix_along_by_forecast_free)
  forecast_effect_svd(prior = prior,
                      svd_forecast = svd_forecast,
                      levels_age = levels_age,
                      levels_sexgender = levels_sexgender,
                      agesex = agesex,
                      matrix_agesex = matrix_agesex)
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
#' @param matrix_along_by Matrix with mapping to along, by dimensions
#' @param var_time Name of time variable
#' @param var_age Name of age variable
#' @param var_sexgender Name of sex/gender variable
#' @param agesex String. One of "age", "age:sex",
#' "sex:age" or "other"
#'
#' @returns TRUE or raises an error
#'
#' @noRd
is_prior_ok_for_term <- function(prior,
                                 nm,
                                 matrix_along_by,
                                 var_time,
                                 var_age,
                                 var_sexgender,
                                 agesex) {
  UseMethod("is_prior_ok_for_term")
}

## HAS_TESTS
#' @export
is_prior_ok_for_term.bage_prior_ar <- function(prior,
                                               nm,
                                               matrix_along_by,
                                               var_time,
                                               var_age,
                                               var_sexgender,
                                               agesex) {
  length_along <- nrow(matrix_along_by)
  n_coef <- prior$specific$n_coef
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
                                                  matrix_along_by,
                                                  var_time,
                                                  var_age,
                                                  var_sexgender,
                                                  agesex) {
  values <- prior$specific$values
  n_values <- length(values)
  length_effect <- length(matrix_along_by)
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
                                                matrix_along_by,
                                                var_time,
                                                var_age,
                                                var_sexgender,
                                                agesex) {
  length_along <- nrow(matrix_along_by)
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
                                                matrix_along_by,
                                                var_time,
                                                var_age,
                                                var_sexgender,
                                                agesex) {
  length_along <- nrow(matrix_along_by)
  n_coef <- prior$specific$n_coef
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
                                                 matrix_along_by,
                                                 var_time,
                                                 var_age,
                                                 var_sexgender,
                                                 agesex) {
  length_effect <- length(matrix_along_by)
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
                                                      matrix_along_by,
                                                      var_time,
                                                      var_age,
                                                      var_sexgender,
                                                      agesex) {
  length_effect <- length(matrix_along_by)
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
                                               matrix_along_by,
                                               var_time,
                                               var_age,
                                               var_sexgender,
                                               agesex) {
  length_along <- nrow(matrix_along_by)
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
                                                      matrix_along_by,
                                                      var_time,
                                                      var_age,
                                                      var_sexgender,
                                                      agesex) {
  n_seas <- prior$specific$n_seas
  length_along <- nrow(matrix_along_by)
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
                                                       matrix_along_by,
                                                       var_time,
                                                       var_age,
                                                       var_sexgender,
                                                       agesex) {
  n_seas <- prior$specific$n_seas
  length_along <- nrow(matrix_along_by)
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
                                                matrix_along_by,
                                                var_time,
                                                var_age,
                                                var_sexgender,
                                                agesex) {
  length_along <- nrow(matrix_along_by)
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
                                                       matrix_along_by,
                                                       var_time,
                                                       var_age,
                                                       var_sexgender,
                                                       agesex) {
  n_seas <- prior$specific$n_seas
  length_along <- nrow(matrix_along_by)
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
                                                        matrix_along_by,
                                                        var_time,
                                                        var_age,
                                                        var_sexgender,
                                                        agesex) {
  n_seas <- prior$specific$n_seas
  length_along <- nrow(matrix_along_by)
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
                                                   matrix_along_by,
                                                   var_time,
                                                   var_age,
                                                   var_sexgender,
                                                   agesex) {
  length_along <- nrow(matrix_along_by)
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
                                                matrix_along_by,
                                                var_time,
                                                var_age,
                                                var_sexgender,
                                                agesex) {
  check_svd_agesex(prior = prior,
                   nm = nm,
                   var_age = var_age,
                   var_sexgender = var_sexgender,
                   agesex = agesex)
  invisible(TRUE)
}

## HAS_TESTS
#' @export
is_prior_ok_for_term.bage_prior_svd_ar <- function(prior,
                                                   nm,
                                                   matrix_along_by,
                                                   var_time,
                                                   var_age,
                                                   var_sexgender,
                                                   agesex) {
  check_svd_agesex(prior = prior,
                   nm = nm,
                   var_age = var_age,
                   var_sexgender = var_sexgender,
                   agesex = agesex)
  check_svd_time(prior = prior,
                 nm = nm,
                 var_time = var_time)
  length_along <- nrow(matrix_along_by)
  n_coef <- prior$specific$n_coef
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
                                                   matrix_along_by,
                                                   var_time,
                                                   var_age,
                                                   var_sexgender,
                                                   agesex) {
  check_svd_agesex(prior = prior,
                   nm = nm,
                   var_age = var_age,
                   var_sexgender = var_sexgender,
                   agesex = agesex)
  check_svd_time(prior = prior,
                 nm = nm,
                 var_time = var_time)
  length_along <- nrow(matrix_along_by)
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
                                                   matrix_along_by,
                                                   var_time,
                                                   var_age,
                                                   var_sexgender,
                                                   agesex) {
  check_svd_agesex(prior = prior,
                   nm = nm,
                   var_age = var_age,
                   var_sexgender = var_sexgender,
                   agesex = agesex)
  check_svd_time(prior = prior,
                 nm = nm,
                 var_time = var_time)
  length_along <- nrow(matrix_along_by)
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
#' @param matrix_along_by Matrix with mapping for along, by dimensions
#' @param levels_effect Labels made from classifying dimensions
#'
#' @returns A character vector.
#'
#' @noRd
levels_hyperrand <- function(prior, matrix_along_by, levels_effect) {
    UseMethod("levels_hyperrand")
}

## HAS_TESTS
#' @export
levels_hyperrand.bage_prior <- function(prior, matrix_along_by, levels_effect) {
  character()
}

## HAS_TESTS
#' @export
levels_hyperrand.bage_prior_lin <- function(prior, matrix_along_by, levels_effect) {
  n_by <- ncol(matrix_along_by)
  if (n_by > 1L) {
    nms_by <- colnames(matrix_along_by)
    paste("slope", nms_by, sep = ".")
  }
  else
    "slope"
}

## HAS_TESTS
#' @export
levels_hyperrand.bage_prior_linar <- function(prior, matrix_along_by, levels_effect) {
  n_by <- ncol(matrix_along_by)
  if (n_by > 1L) {
    nms_by <- colnames(matrix_along_by)
    paste("slope", nms_by, sep = ".")
  }
  else
    "slope"
}

## HAS_TESTS
#' @export
levels_hyperrand.bage_prior_rwseasfix <- function(prior, matrix_along_by, levels_effect) {
  n_seas <- prior$specific$n_seas
  s <- seq_len(n_seas)
  n_by <- ncol(matrix_along_by)
  if (n_by > 1L) {
    nms_by <- colnames(matrix_along_by)
    paste(s,
          rep(nms_by, each = n_seas),
          sep = ".")
  }
  else
    as.character(s)
}

## HAS_TESTS
#' @export
levels_hyperrand.bage_prior_rwseasvary <- function(prior, matrix_along_by, levels_effect) {
  levels_effect
}

## HAS_TESTS
#' @export
levels_hyperrand.bage_prior_rw2seasfix <- function(prior, matrix_along_by, levels_effect) {
  n_seas <- prior$specific$n_seas
  s <- seq_len(n_seas)
  n_by <- ncol(matrix_along_by)
  if (n_by > 1L) {
    nms_by <- colnames(matrix_along_by)
    paste(s,
          rep(nms_by, each = n_seas),
          sep = ".")
  }
  else
    as.character(s)
}

## HAS_TESTS
#' @export
levels_hyperrand.bage_prior_rw2seasvary <- function(prior, matrix_along_by, levels_effect) {
  levels_effect
}


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




## 'make_matrix_along_by_free' ------------------------------------------------

#' Make an 'along_by' Matrix for Free Parameters
#'
#' Make a matrix mapping position within 'effectfree'
#' to position in 'along' and 'by' variables
#' 
#' @param prior Object of class 'bage_prior'
#' @param var_time Name of time variable
#' @param var_age Name of age variable
#' @param var_sexgender Name of sexgender variable
#' @param dimnames Dimnames for array representing effect
#' @param matrix_along_by Matrix with mapping for along, by dimensions
#'
#' @returns A matrix.
#'
#' @noRd
make_matrix_along_by_free <- function(prior,
                                      var_time,
                                      var_age,
                                      var_sexgender,
                                      dimnames,
                                      matrix_along_by) {
  UseMethod("make_matrix_along_by_free")
}

## HAS_TESTS
#' @export
make_matrix_along_by_free.bage_prior <- function(prior,
                                                 var_time,
                                                 var_age,
                                                 var_sexgender,
                                                 dimnames,
                                                 matrix_along_by) {
  matrix_along_by
}

## HAS_TESTS
#' @export
make_matrix_along_by_free.bage_prior_spline <- function(prior,
                                                        var_time,
                                                        var_age,
                                                        var_sexgender,
                                                        dimnames,
                                                        matrix_along_by) {
  n_along <- nrow(matrix_along_by)
  n_by <- ncol(matrix_along_by)
  n_comp <- get_n_comp_spline(prior = prior, n_along = n_along)
  rownames <- paste0("comp", seq_len(n_comp))
  colnames <- colnames(matrix_along_by)
  matrix(seq.int(from = 0L, length.out = n_comp * n_by),
         nrow = n_comp,
         ncol = n_by,
         dimnames = list(rownames, colnames))
}


## HAS_TESTS
#' @export
make_matrix_along_by_free.bage_prior_svd <- function(prior,
                                                     var_time,
                                                     var_age,
                                                     var_sexgender,
                                                     dimnames,
                                                     matrix_along_by) {
  n_comp <- prior$specific$n_comp
  joint <- prior$specific$joint
  is_indep <- !is.null(joint) && !joint
  labels_svd <- paste0("comp", seq_len(n_comp))
  if (is_indep) {
    levels_sex <- dimnames[[var_sexgender]]
    n_sex <- length(levels_sex)
    n_svd <- n_sex * n_comp
    labels_svd <- paste(rep(levels_sex, each = n_comp),
                        labels_svd,
                        sep = ".")
  }
  else
    n_svd <- n_comp
  non_agesex <- setdiff(names(dimnames), c(var_age, var_sexgender))
  dimnames <- dimnames[non_agesex]
  dimnames <- c(list(labels_svd), dimnames)
  dim <- lengths(dimnames, use.names = FALSE)
  ans <- make_matrix_along_by(i_along = 1L,
                              dim = dim,
                              dimnames = dimnames)
  names(dimnames(ans)) <- NULL
  ans
}

## HAS_TESTS
#' @export
make_matrix_along_by_free.bage_prior_svd_ar <- function(prior,
                                                        var_time,
                                                        var_age,
                                                        var_sexgender,
                                                        dimnames,
                                                        matrix_along_by) {
  make_matrix_along_by_free_svdtime(prior = prior,
                                    var_time = var_time,
                                    var_age = var_age,
                                    var_sexgender = var_sexgender,
                                    dimnames = dimnames)
}

## HAS_TESTS
#' @export
make_matrix_along_by_free.bage_prior_svd_rw <- function(prior,
                                                        var_time,
                                                        var_age,
                                                        var_sexgender,
                                                        dimnames,
                                                        matrix_along_by) {
  make_matrix_along_by_free_svdtime(prior = prior,
                                    var_time = var_time,
                                    var_age = var_age,
                                    var_sexgender = var_sexgender,
                                    dimnames = dimnames)
}

## HAS_TESTS
#' @export
make_matrix_along_by_free.bage_prior_svd_rw2 <- function(prior,
                                                         var_time,
                                                         var_age,
                                                         var_sexgender,
                                                         dimnames,
                                                         matrix_along_by) {
  make_matrix_along_by_free_svdtime(prior = prior,
                                    var_time = var_time,
                                    var_age = var_age,
                                    var_sexgender = var_sexgender,
                                    dimnames = dimnames)
}


## 'make_matrix_effectfree_effect' --------------------------------------------------

#' Make matrix mapping effectfree to effect
#'
#' Make matrices mapping free parameters
#' for main effects or interactions to
#' full parameter vectors
#' 
#' @param prior Object of class 'bage_prior'
#' @param levels_effect Vector of labels for term
#' @param agesex String. One of "age", "age:sex",
#' "age:other", "age:sex:other", or "other"
#' @param levels_age Values taken by age
#' variable (or NULL if no age variable in data)
#' @param levels_sexgender Values taken by sex/gender
#' variable (or NULL if no sex/gender variable in data)
#' @param matrix_along_by Matrix with mapping for along, by dimensions
#' @param matrix_agesex Matrix mapping term
#' to age and sex dimensions.
#'
#' @returns A sparse matrix.
#'
#' @noRd
make_matrix_effectfree_effect <- function(prior,
                                          levels_effect,
                                          agesex,
                                          levels_age,
                                          levels_sexgender,
                                          matrix_along_by,
                                          matrix_agesex) {
  UseMethod("make_matrix_effectfree_effect")
}

## HAS_TESTS
#' @export
make_matrix_effectfree_effect.bage_prior <- function(prior,
                                                     levels_effect,
                                                     agesex,
                                                     levels_age,
                                                     levels_sexgender,
                                                     matrix_along_by,
                                                     matrix_agesex) {
  n <- length(levels_effect)
  Matrix::.sparseDiagonal(n)
}

## HAS_TESTS
#' @export
make_matrix_effectfree_effect.bage_prior_spline <- function(prior,
                                                            levels_effect,
                                                            agesex,
                                                            levels_age,
                                                            levels_sexgender,
                                                            matrix_along_by,
                                                            matrix_agesex) {
  n_along <- nrow(matrix_along_by)
  n_by <- ncol(matrix_along_by)
  n_comp <- get_n_comp_spline(prior = prior, n_along = n_along)
  X <- make_spline_matrix(n_comp = n_comp,
                          n_along = n_along)
  I <- Matrix::.sparseDiagonal(n_by)
  X_all_by <- Matrix::kronecker(I, X)
  matrix_alongfirst_to_standard <- make_index_matrix(matrix_along_by)
  matrix_alongfirst_to_standard %*% X_all_by
}

## HAS_TESTS
#' @export
make_matrix_effectfree_effect.bage_prior_svd <- function(prior,
                                                         levels_effect,
                                                         agesex,
                                                         levels_age,
                                                         levels_sexgender,
                                                         matrix_along_by,
                                                         matrix_agesex)
  make_matrix_effectfree_effect_svd(prior = prior,
                                    agesex = agesex,
                                    levels_age = levels_age,
                                    levels_sexgender = levels_sexgender,
                                    matrix_agesex = matrix_agesex)

## HAS_TESTS
#' @export
make_matrix_effectfree_effect.bage_prior_svd_ar <- function(prior,
                                                            levels_effect,
                                                            agesex,
                                                            levels_age,
                                                            levels_sexgender,
                                                            matrix_along_by,
                                                            matrix_agesex)
  make_matrix_effectfree_effect_svd(prior = prior,
                                    agesex = agesex,
                                    levels_age = levels_age,
                                    levels_sexgender = levels_sexgender,
                                    matrix_agesex = matrix_agesex)

## HAS_TESTS
#' @export
make_matrix_effectfree_effect.bage_prior_svd_rw <- function(prior,
                                                            levels_effect,
                                                            agesex,
                                                            levels_age,
                                                            levels_sexgender,
                                                            matrix_along_by,
                                                            matrix_agesex)
  make_matrix_effectfree_effect_svd(prior = prior,
                                    agesex = agesex,
                                    levels_age = levels_age,
                                    levels_sexgender = levels_sexgender,
                                    matrix_agesex = matrix_agesex)

## HAS_TESTS
#' @export
make_matrix_effectfree_effect.bage_prior_svd_rw2 <- function(prior,
                                                             levels_effect,
                                                             agesex,
                                                             levels_age,
                                                             levels_sexgender,
                                                             matrix_along_by,
                                                             matrix_agesex)
  make_matrix_effectfree_effect_svd(prior = prior,
                                    agesex = agesex,
                                    levels_age = levels_age,
                                    levels_sexgender = levels_sexgender,
                                    matrix_agesex = matrix_agesex)


## 'make_offset_effectfree_effect' --------------------------------------------------

#' Make offset used in converting effectfree to effect
#'
#' Make offset used in converting
#' free parameters
#' for main effects or interactions to
#' full parameter vectors
#' 
#' @param prior Object of class 'bage_prior'
#' @param levels_effect Vector of labels for term
#' @param agesex String. One of "age", "age:sex",
#' "age:other", "age:sex:other", or "other"
#' @param levels_age Values taken by age
#' variable (or NULL if no age variable in data)
#' @param levels_sexgender Values taken by sex/gender
#' variable (or NULL if no sex/gender variable in data)
#' @param matrix_agesex Matrix mapping term to age
#' and sex dimensions.
#'
#' @returns A vector.
#'
#' @noRd
make_offset_effectfree_effect <- function(prior,
                                          levels_effect,
                                          agesex,
                                          levels_age,
                                          levels_sexgender,
                                          matrix_agesex) {
  UseMethod("make_offset_effectfree_effect")
}

## HAS_TESTS
#' @export
make_offset_effectfree_effect.bage_prior <- function(prior,
                                                     levels_effect,
                                                     agesex,
                                                     levels_age,
                                                     levels_sexgender,
                                                     matrix_agesex) {
  n <- length(levels_effect)
  rep.int(0, times = n)
}

## HAS_TESTS
#' @export
make_offset_effectfree_effect.bage_prior_svd <- function(prior,
                                                         levels_effect,
                                                         agesex,
                                                         levels_age,
                                                         levels_sexgender,
                                                         matrix_agesex)
  make_offset_effectfree_effect_svd(prior = prior,
                                    levels_effect = levels_effect,
                                    levels_age = levels_age,
                                    levels_sexgender = levels_sexgender,
                                    agesex = agesex,
                                    matrix_agesex = matrix_agesex)


## HAS_TESTS
#' @export
make_offset_effectfree_effect.bage_prior_svd_ar <- function(prior,
                                                         levels_effect,
                                                         agesex,
                                                         levels_age,
                                                         levels_sexgender,
                                                         matrix_agesex)
  make_offset_effectfree_effect_svd(prior = prior,
                                    levels_effect = levels_effect,
                                    levels_age = levels_age,
                                    levels_sexgender = levels_sexgender,
                                    agesex = agesex,
                                    matrix_agesex = matrix_agesex)


## HAS_TESTS
#' @export
make_offset_effectfree_effect.bage_prior_svd_rw <- function(prior,
                                                            levels_effect,
                                                            agesex,
                                                            levels_age,
                                                            levels_sexgender,
                                                            matrix_agesex)
  make_offset_effectfree_effect_svd(prior = prior,
                                    levels_effect = levels_effect,
                                    levels_age = levels_age,
                                    levels_sexgender = levels_sexgender,
                                    agesex = agesex,
                                    matrix_agesex = matrix_agesex)

## HAS_TESTS
#' @export
make_offset_effectfree_effect.bage_prior_svd_rw2 <- function(prior,
                                                             levels_effect,
                                                             agesex,
                                                             levels_age,
                                                             levels_sexgender,
                                                             matrix_agesex)
  make_offset_effectfree_effect_svd(prior = prior,
                                    levels_effect = levels_effect,
                                    levels_age = levels_age,
                                    levels_sexgender = levels_sexgender,
                                    agesex = agesex,
                                    matrix_agesex = matrix_agesex)


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
  joint <- x$specific$joint
  if (!is.null(joint))
    print_prior_slot(prior = x, nm = "joint", slot = "joint")
  invisible(x)
}

## HAS_TESTS
#' @export
print.bage_prior_svd_ar <- function(x, ...) {
  n_offset <- get_print_prior_n_offset()
  print_prior_header(x)
  print_prior_slot(prior = x, nm = "ssvd", slot = "nm_ssvd")
  print_prior_slot(prior = x, nm = "n_comp", slot = "n_comp")
  joint <- x$specific$joint
  if (!is.null(joint))
    print_prior_slot(prior = x, nm = "joint", slot = "joint")
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
  joint <- x$specific$joint
  if (!is.null(joint))
    print_prior_slot(prior = x, nm = "joint", slot = "joint")
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
  joint <- x$specific$joint
  if (!is.null(joint))
    print_prior_slot(prior = x, nm = "joint", slot = "joint")
  print_prior_slot(prior = x, nm = "s", slot = "scale")
  invisible(x)
}



## 'reformat_hyperrand_one' ---------------------------------------------------

#' Reformat Parts of 'Components' Output Dealing with a
#' Prior that has Hyper-Parameters Treated as Random Effects
#'
#' In all priors with 'hyperrand' elements, the reformatting involves
#' renaming columns. 
#'
#' @param prior Object of class 'bage_prior'.
#' @param nm_prior Name of the prior (ie name of the term).
#' @param matrix_along_by Matrix with mapping for along, by dimensions
#' @param components A data frame.
#'
#' @returns A modifed version of 'components'
#'
#' @noRd
reformat_hyperrand_one <- function(prior,
                                   nm_prior,
                                   matrix_along_by,
                                   components) {
  UseMethod("reformat_hyperrand_one")
}

## HAS_TESTS
#' @export
reformat_hyperrand_one.bage_prior <- function(prior,
                                              nm_prior,
                                              matrix_along_by,
                                              components)
  components

## HAS_TESTS
#' @export
reformat_hyperrand_one.bage_prior_lin <- function(prior,
                                                  nm_prior,
                                                  matrix_along_by,
                                                  components) {
  is_change <- with(components, component == "hyperrand" & term == nm_prior)
  components$component[is_change] <- "hyper"
  components
}

## HAS_TESTS
#' @export
reformat_hyperrand_one.bage_prior_linar <- function(prior,
                                                    nm_prior,
                                                    matrix_along_by,
                                                    components) {
  n_along <- nrow(matrix_along_by)
  n_by <- ncol(matrix_along_by)
  is_effect <- with(components,
                    term == nm_prior & component == "effect")
  is_slope <- with(components,
                   term == nm_prior & component == "hyperrand" & startsWith(level, "slope"))
  effect <- components$.fitted[is_effect]
  slope <- components$.fitted[is_slope]
  level <- components$level[is_effect]
  q <- seq(from = -1, to = 1, length.out = n_along)
  trend <- rep(slope, each = n_along) * rep(q, times = n_by)
  i <- match(sort(matrix_along_by), matrix_along_by)
  trend <- trend[i]
  cyclical <- effect - trend
  components$component[is_slope] <- "hyper"
  trend <- tibble::tibble(term = nm_prior,
                          component = "trend",
                          level = level,
                          .fitted = trend)
  cyclical <- tibble::tibble(term = nm_prior,
                             component = "cyclical",
                             level = level,
                             .fitted = cyclical)
  vctrs::vec_rbind(components, trend, cyclical)
}


## HAS_TESTS
#' @export
reformat_hyperrand_one.bage_prior_rwseasfix <- function(prior,
                                                        nm_prior,
                                                        matrix_along_by,
                                                        components)
  reformat_hyperrand_seasfix(prior = prior,
                             nm_prior = nm_prior,
                             matrix_along_by = matrix_along_by,
                             components = components)

## HAS_TESTS
#' @export
reformat_hyperrand_one.bage_prior_rwseasvary <- function(prior,
                                                         nm_prior,
                                                         matrix_along_by,
                                                         components)
  reformat_hyperrand_seasvary(prior = prior,
                              nm_prior = nm_prior,
                              matrix_along_by = matrix_along_by,
                              components = components)

## HAS_TESTS
#' @export
reformat_hyperrand_one.bage_prior_rw2seasfix <- function(prior,
                                                        nm_prior,
                                                        matrix_along_by,
                                                        components)
  reformat_hyperrand_seasfix(prior = prior,
                             nm_prior = nm_prior,
                             matrix_along_by = matrix_along_by,
                             components = components)

## HAS_TESTS
#' @export
reformat_hyperrand_one.bage_prior_rw2seasvary <- function(prior,
                                                         nm_prior,
                                                         matrix_along_by,
                                                         components)
  reformat_hyperrand_seasvary(prior = prior,
                              nm_prior = nm_prior,
                              matrix_along_by = matrix_along_by,
                              components = components)



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
  joint <- prior$specific$joint
  nm <- if (is.null(joint)) "SVD" else "SVDS"
  args <- str_call_args_svd(prior)
  args <- args[nzchar(args)]
  args <- paste(args, collapse = ",")
  sprintf("%s(%s)", nm, args)
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
  joint <- prior$specific$joint
  args_svd <- str_call_args_svd(prior)
  args_scale <- str_call_args_scale(prior)
  args_along <- str_call_args_along(prior)
  args <- c(args_svd, args_scale, args_along)
  args <- args[nzchar(args)]
  args <- paste(args, collapse = ",")
  nm <- if (is.null(joint)) "SVD_RW" else "SVDS_RW"
  sprintf("%s(%s)", nm, args)
}

## HAS_TESTS
#' @export
str_call_prior.bage_prior_svd_rw2 <- function(prior) {
  joint <- prior$specific$joint
  args_svd <- str_call_args_svd(prior)
  args_scale <- str_call_args_scale(prior)
  args_along <- str_call_args_along(prior)
  args <- c(args_svd, args_scale, args_along)
  args <- args[nzchar(args)]
  args <- paste(args, collapse = ",")
  nm <- if (is.null(joint)) "SVD_RW2" else "SVDS_RW2"
  sprintf("%s(%s)", nm, args)
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
  joint <- prior$specific$joint
  nm <- if (is.null(joint)) "SVD" else "SVDS"
  sprintf("%s()", nm)
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
  joint <- prior$specific$joint
  base <- if (is.null(joint)) "SVD" else "SVDS"
  sprintf("%s_RW()", base)
}

## HAS_TESTS
#' @export
str_nm_prior.bage_prior_svd_rw2 <- function(prior) {
  joint <- prior$specific$joint
  base <- if (is.null(joint)) "SVD" else "SVDS"
  sprintf("%s_RW2()", base)
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
uses_along.bage_prior_svd_ar <- function(prior) FALSE

## HAS_TESTS
#' @export
uses_along.bage_prior_svd_rw <- function(prior) FALSE

## HAS_TESTS
#' @export
uses_along.bage_prior_svd_rw2 <- function(prior) FALSE


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
