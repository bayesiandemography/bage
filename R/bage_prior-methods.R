
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
                                           levels_effect,
                                           levels_age,
                                           levels_sexgender,
                                           agesex,
                                           matrix_along_by,
                                           matrix_agesex,
                                           n_sim) {
  coef <- vals_hyper$coef
  sd <- vals_hyper$sd
  n <- length(levels_effect)
  ans <- draw_vals_ar(n = n, coef = coef, sd = sd)
  ans <- ans - rep(colMeans(ans), each = n)
  dimnames(ans) <- list(levels_effect, seq_len(n_sim))
  ans
}

## HAS_TESTS
#' @export
draw_vals_effect.bage_prior_ear <- function(prior,
                                            vals_hyper,
                                            vals_hyperrand,
                                            levels_effect,
                                            levels_age,
                                            levels_sexgender,
                                            agesex,
                                            matrix_along_by,
                                            matrix_agesex,
                                            n_sim) {
  coef <- vals_hyper$coef
  sd <- vals_hyper$sd
  n_along <- nrow(matrix_along_by)
  n_by <- ncol(matrix_along_by)
  s <- rep(seq_len(n_sim), each = n_by)
  coef <- coef[, s]
  sd <- sd[s]
  ans <- draw_vals_ar(n = n_along, coef = coef, sd = sd)
  ans <- ans - rep(colMeans(ans), each = n_along)
  ans <- matrix(ans, nrow = n_along * n_by, ncol = n_sim)
  i <- match(sort(matrix_along_by), matrix_along_by)
  ans <- ans[i, , drop = FALSE]
  dimnames(ans) <- list(levels_effect, seq_len(n_sim))
  ans
}

## HAS_TESTS
#' @export
draw_vals_effect.bage_prior_elin <- function(prior,
                                             vals_hyper,
                                             vals_hyperrand,
                                             levels_effect,
                                             levels_age,
                                             levels_sexgender,
                                             agesex,
                                             matrix_along_by,
                                             matrix_agesex,
                                             n_sim) {
  mslope <- vals_hyperrand$mslope
  sd <- vals_hyper$sd
  draw_vals_elin(mslope = mslope,
                 sd = sd,
                 matrix_along_by = matrix_along_by,
                 labels = levels_effect) ## standardized internally
}

## HAS_TESTS
#' @export
draw_vals_effect.bage_prior_erw <- function(prior,
                                            vals_hyper,
                                            vals_hyperrand,
                                            levels_effect,
                                            levels_age,
                                            levels_sexgender,
                                            agesex,
                                            matrix_along_by,
                                            matrix_agesex,
                                            n_sim) {
  sd <- vals_hyper$sd
  draw_vals_erw(sd = sd,
                matrix_along_by = matrix_along_by,
                labels = levels_effect) ## standardized internally
}

## HAS_TESTS
#' @export
draw_vals_effect.bage_prior_erw2 <- function(prior,
                                             vals_hyper,
                                             vals_hyperrand,
                                             levels_effect,
                                             levels_age,
                                             levels_sexgender,
                                             agesex,
                                             matrix_along_by,
                                             matrix_agesex,
                                             n_sim) {
  sd <- vals_hyper$sd
  draw_vals_erw2(sd = sd,
                 matrix_along_by = matrix_along_by,
                 labels = levels_effect)  ## standardized internally
}


## HAS_TESTS
#' @export
draw_vals_effect.bage_prior_eseas <- function(prior,
                                              vals_hyper,
                                              vals_hyperrand,
                                              levels_effect,
                                              levels_age,
                                              levels_sexgender,
                                              agesex,
                                              matrix_along_by,
                                              matrix_agesex,
                                              n_sim) {
  n <- prior$specific$n
  sd <- vals_hyper$sd
  draw_vals_eseas(n = n,
                  sd = sd,
                  matrix_along_by = matrix_along_by,
                  labels = levels_effect) ## standardized internally
}

## HAS_TESTS
#' @export
draw_vals_effect.bage_prior_known <- function(prior,
                                              vals_hyper,
                                              vals_hyperrand,
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
         dimnames = list(levels_effect, seq_len(n_sim)))
}

## HAS_TESTS
#' @export
draw_vals_effect.bage_prior_lin <- function(prior,
                                            vals_hyper,
                                            vals_hyperrand,
                                            levels_effect,
                                            levels_age,
                                            levels_sexgender,
                                            agesex,
                                            matrix_along_by,
                                            matrix_agesex,
                                            n_sim) {
  slope <- vals_hyper$slope
  sd <- vals_hyper$sd
  ans <- draw_vals_lin(slope = slope,
                       sd = sd,
                       labels = levels_effect)
  ans <- ans - rep(colMeans(ans), each = nrow(ans))
  ans
}

## HAS_TESTS
#' @export
draw_vals_effect.bage_prior_linar <- function(prior,
                                              vals_hyper,
                                              vals_hyperrand,
                                              levels_effect,
                                              levels_age,
                                              levels_sexgender,
                                              agesex,
                                              matrix_along_by,
                                              matrix_agesex,
                                              n_sim) {
  slope <- vals_hyper$slope
  coef <- vals_hyper$coef
  sd <- vals_hyper$sd
  n_effect <- length(levels_effect)
  n_sim <- length(slope)
  q <- seq(from = -1, to = 1, length.out = n_effect)
  mean <- outer(q, slope)
  error <- draw_vals_ar(n = n_effect,
                        coef = coef,
                        sd = sd)
  ans <- mean + error
  ans <- ans - rep(colMeans(ans), each = n_effect)
  dimnames(ans) <- list(levels_effect, seq_len(n_sim))
  ans
}

## HAS_TESTS
#' @export
draw_vals_effect.bage_prior_norm <- function(prior,
                                             vals_hyper,
                                             vals_hyperrand,
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
  ans <- matrix(ans, nrow = n_along, ncol = n_by * n_sim)
  ans <- ans - rep(colMeans(ans), each = nrow(ans))
  ans <- matrix(ans, nrow = n_effect, ncol = n_sim)
  i <- match(sort(matrix_along_by), matrix_along_by)
  ans <- ans[i, , drop = FALSE]
  dimnames(ans) <- list(levels_effect, seq_len(n_sim))
  ans
}

## HAS_TESTS
#' @export
draw_vals_effect.bage_prior_normfixed <- function(prior,
                                                  vals_hyper,
                                                  vals_hyperrand,
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
                dimnames = list(levels_effect, seq_len(n_sim)))
  ans <- ans - rep(colMeans(ans), each = nrow(ans))
  ans
}

## HAS_TESTS
#' @export
draw_vals_effect.bage_prior_rw <- function(prior,
                                           vals_hyper,
                                           vals_hyperrand,
                                           levels_effect,
                                           levels_age,
                                           levels_sexgender,
                                           agesex,
                                           matrix_along_by,
                                           matrix_agesex,
                                           n_sim) {
  sd <- vals_hyper$sd
  ans <- draw_vals_rw(sd = sd,
                      labels = levels_effect)
  ans <- ans - rep(colMeans(ans), each = nrow(ans))
  ans
}

## HAS_TESTS
#' @export
draw_vals_effect.bage_prior_rw2 <- function(prior,
                                            vals_hyper,
                                            vals_hyperrand,
                                            levels_effect,
                                            levels_age,
                                            levels_sexgender,
                                            agesex,
                                            matrix_along_by,
                                            matrix_agesex,
                                            n_sim) {
  sd <- vals_hyper$sd
  ans <- draw_vals_rw2(sd = sd,
                       labels = levels_effect)
  ans <- ans - rep(colMeans(ans), each = nrow(ans))
  ans
}

## HAS_TESTS
#' @export
draw_vals_effect.bage_prior_seas <- function(prior,
                                             vals_hyper,
                                             vals_hyperrand,
                                             levels_effect,
                                             levels_age,
                                             levels_sexgender,
                                             agesex,
                                             matrix_along_by,
                                             matrix_agesex,
                                             n_sim) {
  n <- prior$specific$n
  sd <- vals_hyper$sd
  ans <- draw_vals_seas(n = n,
                        sd = sd,
                        labels = levels_effect)
  ans <- ans - rep(colMeans(ans), each = nrow(ans))
  ans
}

## HAS_TESTS
#' @export
draw_vals_effect.bage_prior_spline <- function(prior,
                                               vals_hyper,
                                               vals_hyperrand,
                                               levels_effect,
                                               levels_age,
                                               levels_sexgender,
                                               agesex,
                                               matrix_along_by,
                                               matrix_agesex,
                                               n_sim) {
  sd <- vals_hyper$sd
  m <- make_matrix_effectfree_effect(prior = prior,
                                     levels_effect = levels_effect,
                                     agesex = NULL)
  m <- Matrix::as.matrix(m)
  labels <- seq_len(ncol(m))
  effect <- draw_vals_rw2(sd = sd,
                          labels = labels)
  ans <- m %*% effect
  ans <- ans - rep(colMeans(ans), each = nrow(ans))
  ans

}

## HAS_TESTS
#' @export
draw_vals_effect.bage_prior_svd <- function(prior,
                                            vals_hyper,
                                            vals_hyperrand,
                                            levels_effect,
                                            levels_age,
                                            levels_sexgender,
                                            agesex,
                                            matrix_along_by,
                                            matrix_agesex,
                                            n_sim) {
  ssvd <- prior$specific$ssvd
  joint <- prior$specific$joint
  n_comp <- prior$specific$n
  m <- get_matrix_or_offset_svd(ssvd = ssvd,
                                levels_age = levels_age,
                                levels_sexgender = levels_sexgender,
                                joint = joint,
                                agesex = agesex,
                                get_matrix = TRUE,
                                n_comp = n_comp)
  b <- get_matrix_or_offset_svd(ssvd = ssvd,
                                levels_age = levels_age,
                                levels_sexgender = levels_sexgender,
                                joint = joint,
                                agesex = agesex,
                                get_matrix = FALSE,
                                n_comp = n_comp)
  n_comp <- ncol(m)
  z <- stats::rnorm(n = n_comp * n_sim)
  z <- matrix(z, nrow = n_comp, ncol = n_sim)
  ans <- m %*% z + b
  ans <- Matrix::as.matrix(ans)
  colnames(ans) <- seq_len(n_sim)
  names(dimnames(ans)) <- NULL
  ans    
}

## HAS_TESTS
#' @export
draw_vals_effect.bage_prior_esvd <- function(prior,
                                             vals_hyper,
                                             vals_hyperrand,
                                             levels_effect,
                                             levels_age,
                                             levels_sexgender,
                                             agesex,
                                             matrix_along_by,
                                             matrix_agesex,
                                             n_sim) {
  ssvd <- prior$specific$ssvd
  joint <- prior$specific$joint
  n_comp <- prior$specific$n
  n_by <- ncol(matrix_along_by)
  m <- get_matrix_or_offset_svd(ssvd = ssvd,
                                levels_age = levels_age,
                                levels_sexgender = levels_sexgender,
                                joint = joint,
                                agesex = agesex,
                                get_matrix = TRUE,
                                n_comp = n_comp)
  b <- get_matrix_or_offset_svd(ssvd = ssvd,
                                levels_age = levels_age,
                                levels_sexgender = levels_sexgender,
                                joint = joint,
                                agesex = agesex,
                                get_matrix = FALSE,
                                n_comp = n_comp)
  n_comp <- ncol(m)
  n_by <- ncol(matrix_along_by)
  z <- stats::rnorm(n = n_comp * n_by * n_sim)
  z <- matrix(z, nrow = n_comp, ncol = n_by * n_sim)
  ans <- m %*% z + b
  ans <- Matrix::as.matrix(ans)
  ans <- matrix(ans, ncol = n_sim)
  m <- make_matrix_agesex_index(matrix_agesex)
  ans <- m %*% ans
  colnames(ans) <- seq_len(n_sim)
  names(dimnames(ans)) <- NULL
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
draw_vals_hyper.bage_prior_ear <- function(prior, n_sim) {
    coef <- draw_vals_coef(prior = prior, n_sim = n_sim)
    sd <- draw_vals_sd(prior = prior, n_sim = n_sim)
    list(coef = coef,
         sd = sd)
}

## HAS_TESTS
#' @export
draw_vals_hyper.bage_prior_elin <- function(prior, n_sim) {
  slope <- draw_vals_slope(prior = prior,
                           n_sim = n_sim)
  msd <- draw_vals_msd(prior = prior,
                       n_sim = n_sim)
  sd <- draw_vals_sd(prior = prior,
                     n_sim = n_sim)
  list(slope = slope,
       sd = sd,
       msd = msd)
}

## HAS_TESTS
#' @export
draw_vals_hyper.bage_prior_erw <- function(prior, n_sim) {
  sd <- draw_vals_sd(prior = prior,
                     n_sim = n_sim)
  list(sd = sd)
}

## HAS_TESTS
#' @export
draw_vals_hyper.bage_prior_erw2 <- function(prior, n_sim) {
  sd <- draw_vals_sd(prior = prior,
                     n_sim = n_sim)
  list(sd = sd)
}

## HAS_TESTS
#' @export
draw_vals_hyper.bage_prior_eseas <- function(prior, n_sim) {
  sd <- draw_vals_sd(prior = prior,
                     n_sim = n_sim)
  list(sd = sd)
}

## HAS_TESTS
#' @export
draw_vals_hyper.bage_prior_known <- function(prior, n_sim)
    list()

## HAS_TESTS
#' @export
draw_vals_hyper.bage_prior_lin <- function(prior, n_sim) {
  slope <- draw_vals_slope(prior = prior, n_sim = n_sim)
  sd <- draw_vals_sd(prior = prior, n_sim = n_sim)
  list(slope = slope,
       sd = sd)
}

## HAS_TESTS
#' @export
draw_vals_hyper.bage_prior_linar <- function(prior, n_sim) {
  slope <- draw_vals_slope(prior = prior, n_sim = n_sim)
  sd <- draw_vals_sd(prior = prior, n_sim = n_sim)
  coef <- draw_vals_coef(prior = prior, n_sim = n_sim)
  list(slope = slope,
       sd = sd,
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
draw_vals_hyper.bage_prior_rw2 <- function(prior, n_sim) {
    sd <- draw_vals_sd(prior = prior, n_sim = n_sim)
    list(sd = sd)
}

## HAS_TESTS
#' @export
draw_vals_hyper.bage_prior_seas <- function(prior, n_sim) {
    sd <- draw_vals_sd(prior = prior, n_sim = n_sim)
    list(sd = sd)
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
draw_vals_hyper.bage_prior_esvd <- function(prior, n_sim)
    list()


## 'draw_vals_hyperrand' ------------------------------------------------------

#' Draw Values for Hyper-Parameters that can be Treated as Random Effects
#'
#' @param prior Object of class 'bage_prior'
#' @param vals_hyper Named list of values
#' @param levels_effect Character vector with labels for effect
#' @param agesex String. One of "age", "age:sex",
#' "sex:age" or "other"
#' @param matrix_along_by Matrix with map for along and by dimensions
#' @param matrix_agesex Matrix with map between age-sex dimensions
#' and whole term
#' @param n_sim Number of simulation draws
#'
#' @returns A named list.
#'
#' @noRd
draw_vals_hyperrand <- function(prior,
                                vals_hyper,
                                levels_effect,
                                agesex,
                                matrix_along_by,
                                matrix_agesex,
                                n_sim) {
  UseMethod("draw_vals_hyperrand")
}

## HAS_TESTS
#' @export
draw_vals_hyperrand.bage_prior <- function(prior,
                                           vals_hyper,
                                           levels_effect,
                                           agesex,
                                           matrix_along_by,
                                           matrix_agesex,
                                           n_sim) {
  list()
}

## HAS_TESTS
#' @export
draw_vals_hyperrand.bage_prior_elin <- function(prior,
                                                vals_hyper,
                                                levels_effect,
                                                agesex,
                                                matrix_along_by,
                                                matrix_agesex,
                                                n_sim) {
  slope <- vals_hyper$slope
  msd <- vals_hyper$msd
  mslope <- draw_vals_mslope(slope = slope,
                             msd = msd,
                             matrix_along_by = matrix_along_by,
                             n_sim = n_sim)
  list(mslope = mslope)
}


## 'forecast_effect' ----------------------------------------------------------

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
#' @param hyper_est Tibble holding draws
#' for estimates of hyper-parameters (or NULL)
#' @param hyper_forecast Tibble holding draws
#' for forecasts of hyper-parameters (or NULL)
#' @param effect_est Tibble holding
#' draws from estimates of effect
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
forecast_effect <- function(prior,
                            nm_prior,
                            hyper_est,
                            hyper_forecast,
                            effect_est,
                            matrix_along_by_est,
                            matrix_along_by_forecast,
                            levels_forecast) {
  UseMethod("forecast_effect")
}

## HAS_TESTS
#' @export
forecast_effect.bage_prior <- function(prior,
                                       nm_prior,
                                       hyper_est,
                                       hyper_forecast,
                                       effect_est,
                                       matrix_along_by_est,
                                       matrix_along_by_forecast,
                                       levels_forecast) {
  cli::cli_abort(c("Can't forecast term {.val {nm_prior}}.",
                   i = "Term {.val {nm_prior}} has a {.val {str_nm_prior(prior)}} prior.",
                   i = "Terms with a {.val {str_nm_prior(prior)}} prior cannot be forecasted.",
                   i = "For a list of priors that can be forecasted, see {.topic bage::priors}."))
}

## HAS_TESTS
#' @export
forecast_effect.bage_prior_ar <- function(prior,
                                          nm_prior,
                                          hyper_est,
                                          hyper_forecast,
                                          effect_est,
                                          matrix_along_by_est,
                                          matrix_along_by_forecast,
                                          levels_forecast) {
  n_ar <- prior$specific$n
  n_est <- nrow(matrix_along_by_est)
  n_forecast <- nrow(matrix_along_by_forecast)
  coef <- hyper_est$.fitted[[grepl("^coef", hyper_est$level)]]
  sd <- hyper_est$.fitted[[hyper_est$level == "sd"]]
  tmp <- c(utils::tail(effect_est$.fitted, n = n_ar),
           rep(effect_est$.fitted[[1L]], times = n_forecast))
  for (j in seq_len(n_forecast)) {
    s_ar <- seq(from = j, to = j + n_ar - 1L)
    mean <- sum(coef * tmp[s_ar])
    tmp[[j + n_ar]] <- rvec::rnorm_rvec(n = 1L, mean = mean, sd = sd)
  }
  .fitted <- utils::tail(tmp, n = n_forecast)
  tibble::tibble(term = nm_prior,
                 component = "effect",
                 level = levels_forecast,
                 .fitted = .fitted)
}

## HAS_TESTS
#' @export
forecast_effect.bage_prior_ear <- function(prior,
                                           nm_prior,
                                           hyper_est,
                                           hyper_forecast,
                                           effect_est,
                                           matrix_along_by_est,
                                           matrix_along_by_forecast,
                                           levels_forecast) {
  n_ar <- prior$specific$n
  n_along_est <- nrow(matrix_along_by_est)
  n_along_forecast <- nrow(matrix_along_by_forecast)
  n_by <- ncol(matrix_along_by_est)
  coef <- hyper_est$.fitted[[grepl("^coef", hyper_est$level)]]
  sd <- hyper_est$.fitted[[hyper_est$level == "sd"]]
  .fitted <- rep(effect_est$.fitted[[1L]], times = n_along_forecast * n_by)
  tmp <- rep(effect_est$.fitted[[1L]], times = n_along_forecast + n_ar)
  s_head <- seq_len(n_ar)
  s_tail <- seq(to = n_along_est, length.out = n_ar)
  for (i_by in seq_len(n_by)) {
    i_tail <- matrix_along_by_est[s_tail, i_by] + 1L ## matrix uses 0-based index
    tmp[s_head] <- effect_est$.fitted[i_tail]
    for (j in seq_len(n_along_forecast)) {
      s_ar <- seq(from = j, to = j + n_ar - 1L)
      mean <- sum(coef * tmp[s_ar])
      tmp[[j + n_ar]] <- rvec::rnorm_rvec(n = 1L, mean = mean, sd = sd)
    }
    i_fitted <- matrix_along_by_forecast[, i_by] + 1L
    .fitted[i_fitted] <- tmp[-s_head]
  }
  tibble::tibble(term = nm_prior,
                 component = "effect",
                 level = levels_forecast,
                 .fitted = .fitted)
}

## HAS_TESTS
#' @export
forecast_effect.bage_prior_elin <- function(prior,
                                            nm_prior,
                                            hyper_est,
                                            hyper_forecast,
                                            effect_est,
                                            matrix_along_by_est,
                                            matrix_along_by_forecast,
                                            levels_forecast) {
  n_along_est <- nrow(matrix_along_by_est)
  n_along_forecast <- nrow(matrix_along_by_forecast)
  n_by <- ncol(matrix_along_by_est)
  mslope <- hyper_est$.fitted[hyper_est$level == "mslope"]
  sd <- hyper_est$.fitted[[hyper_est$level == "sd"]]
  .fitted <- rep(effect_est$.fitted[[1L]], times = n_along_forecast * n_by)
  incr_q <- 2 / (n_along_est - 1)
  q <- seq(from = 1 + incr_q,
           by = incr_q,
           length.out = n_along_forecast)
  for (i_by in seq_len(n_by)) {
    tmp <- rvec::rnorm_rvec(n = n_along_forecast,
                            mean = mslope[i_by] * q,
                            sd = sd)
    i_fitted <- matrix_along_by_forecast[, i_by] + 1L
    .fitted[i_fitted] <- tmp
  }
  tibble::tibble(term = nm_prior,
                 component = "effect",
                 level = levels_forecast,
                 .fitted = .fitted)
}

## HAS_TESTS
#' @export
forecast_effect.bage_prior_erw <- function(prior,
                                           nm_prior,
                                           hyper_est,
                                           hyper_forecast,
                                           effect_est,
                                           matrix_along_by_est,
                                           matrix_along_by_forecast,
                                           levels_forecast) {
  n_along_est <- nrow(matrix_along_by_est)
  n_along_forecast <- nrow(matrix_along_by_forecast)
  n_by <- ncol(matrix_along_by_est)
  sd <- hyper_est$.fitted[[hyper_est$level == "sd"]]
  .fitted <- rep(effect_est$.fitted[[1L]],
                 times = n_along_forecast * n_by)
  tmp <- rep(effect_est$.fitted[[1L]],
             times = n_along_forecast + 1L)
  for (i_by in seq_len(n_by)) {
    i_last <- matrix_along_by_est[n_along_est, i_by] + 1L
    tmp[[1L]] <- effect_est$.fitted[[i_last]]
    for (j in seq_len(n_along_forecast))
      tmp[[j + 1L]] <- rvec::rnorm_rvec(n = 1L, mean = tmp[[j]], sd = sd)
    i_fitted <- matrix_along_by_forecast[, i_by] + 1L
    .fitted[i_fitted] <- tmp[-1L]
  }
  tibble::tibble(term = nm_prior,
                 component = "effect",
                 level = levels_forecast,
                 .fitted = .fitted)
}

## HAS_TESTS
#' @export
forecast_effect.bage_prior_erw2 <- function(prior,
                                            nm_prior,
                                            hyper_est,
                                            hyper_forecast,
                                            effect_est,
                                            matrix_along_by_est,
                                            matrix_along_by_forecast,
                                            levels_forecast) {
  n_along_est <- nrow(matrix_along_by_est)
  n_along_forecast <- nrow(matrix_along_by_forecast)
  n_by <- ncol(matrix_along_by_est)
  sd <- hyper_est$.fitted[[hyper_est$level == "sd"]]
  .fitted <- rep(effect_est$.fitted[[1L]],
                 times = n_along_forecast * n_by)
  tmp <- rep(effect_est$.fitted[[1L]],
             times = n_along_forecast + 2L)
  for (i_by in seq_len(n_by)) {
    i_last <- matrix_along_by_est[n_along_est, i_by] + 1L
    i_second_last <- matrix_along_by_est[n_along_est, i_by]
    tmp[[2L]] <- effect_est$.fitted[[i_last]]
    tmp[[1L]] <- effect_est$.fitted[[i_second_last]]
    for (j in seq_len(n_along_forecast))
      tmp[[j + 2L]] <- rvec::rnorm_rvec(n = 1L,
                                        mean = 2 * tmp[[j + 1L]] - tmp[[j]],
                                        sd = sd)
    i_fitted <- matrix_along_by_forecast[, i_by] + 1L
    .fitted[i_fitted] <- tmp[-(1:2)]
  }
  tibble::tibble(term = nm_prior,
                 component = "effect",
                 level = levels_forecast,
                 .fitted = .fitted)
}

## HAS_TESTS
#' @export
forecast_effect.bage_prior_eseas <- function(prior,
                                             nm_prior,
                                             hyper_est,
                                             hyper_forecast,
                                             effect_est,
                                             matrix_along_by_est,
                                             matrix_along_by_forecast,
                                             levels_forecast) {
  n_seas <- prior$specific$n
  n_along_est <- nrow(matrix_along_by_est)
  n_along_forecast <- nrow(matrix_along_by_forecast)
  n_by <- ncol(matrix_along_by_est)
  sd <- hyper_est$.fitted[[hyper_est$level == "sd"]]
  .fitted <- rep(effect_est$.fitted[[1L]],
                 times = n_along_forecast * n_by)
  tmp <- rep(effect_est$.fitted[[1L]],
             times = n_along_forecast + n_seas)
  for (i_by in seq_len(n_by)) {
    s_head <- seq_len(n_seas)
    s_tail <- seq.int(to = n_along_est, length.out = n_seas)
    i_tail <- matrix_along_by_est[s_tail, i_by] + 1L
    tmp[s_head] <- effect_est$.fitted[i_tail]
    for (j in seq_len(n_along_forecast))
      tmp[[j + n_seas]] <- rvec::rnorm_rvec(n = 1L, mean = tmp[[j]], sd = sd)
    i_fitted <- matrix_along_by_forecast[, i_by] + 1L
    .fitted[i_fitted] <- tmp[-s_head]
  }
  tibble::tibble(term = nm_prior,
                 component = "seasonal",
                 level = levels_forecast,
                 .fitted = .fitted)
}

## HAS_TESTS
#' @export
forecast_effect.bage_prior_lin <- function(prior,
                                           nm_prior,
                                           hyper_est,
                                           hyper_forecast,
                                           effect_est,
                                           matrix_along_by_est,
                                           matrix_along_by_forecast,
                                           levels_forecast) {
  n_est <- nrow(matrix_along_by_est)
  n_forecast <- nrow(matrix_along_by_forecast)
  slope <- hyper_est$.fitted[hyper_est$level == "slope"]
  sd <- hyper_est$.fitted[hyper_est$level == "sd"]
  incr_q <- 2 / (n_est - 1)
  q <- seq(from = 1 + incr_q,
           by = incr_q,
           length.out = n_forecast)
  .fitted <- rvec::rnorm_rvec(n = n_forecast,
                              mean = slope * q,
                              sd = sd)
  tibble::tibble(term = nm_prior,
                 component = "effect",
                 level = levels_forecast,
                 .fitted = .fitted)
}

## HAS_TESTS
#' @export
forecast_effect.bage_prior_linar <- function(prior,
                                             nm_prior,
                                             hyper_est,
                                             hyper_forecast,
                                             effect_est,
                                             matrix_along_by_est,
                                             matrix_along_by_forecast,
                                             levels_forecast) {
  n_ar <- prior$specific$n
  n_est <- nrow(matrix_along_by_est)
  n_forecast <- nrow(matrix_along_by_forecast)
  sd <- hyper_est$.fitted[hyper_est$level == "sd"]
  slope <- hyper_est$.fitted[hyper_est$level == "slope"]
  coef <- hyper_est$.fitted[grepl("^coef", hyper_est$level)]
  incr_q <- 2 / (n_est - 1)
  q_tail <- seq(to = 1,
                by = incr_q,
                length.out = n_ar)
  mean_tail <- slope * q_tail
  effect_tail <- utils::tail(effect_est$.fitted, n = n_ar)
  error_tail <- effect_tail - mean_tail
  tmp <- c(error_tail, rep(error_tail[[1L]], times = n_forecast))
  for (j in seq_len(n_forecast)) {
    s_ar <- seq(from = j, to = j + n_ar - 1L)
    mean <- sum(coef * tmp[s_ar])
    tmp[[j + n_ar]] <- rvec::rnorm_rvec(n = 1L, mean = mean, sd = sd)
  }
  error_forecast <- utils::tail(tmp, n = n_forecast)
  q_forecast <- seq(from = 1 + incr_q,
                    by = incr_q,
                    length.out = n_forecast)
  mean_forecast <- slope * q_forecast
  .fitted <- mean_forecast + error_forecast
  tibble::tibble(term = nm_prior,
                 component = "effect",
                 level = levels_forecast,
                 .fitted = .fitted)
}

## HAS_TESTS
#' @export
forecast_effect.bage_prior_norm <- function(prior,
                                           nm_prior,
                                           hyper_est,
                                           hyper_forecast,
                                           effect_est,
                                           matrix_along_by_est,
                                           matrix_along_by_forecast,
                                           levels_forecast) {
  n_along_forecast <- nrow(matrix_along_by_forecast)
  sd <- hyper_est$.fitted[[hyper_est$level == "sd"]]
  .fitted <- rvec::rnorm_rvec(n = n_along_forecast,
                              sd = sd)
  tibble::tibble(term = nm_prior,
                 component = "effect",
                 level = levels_forecast,
                 .fitted = .fitted)
}

## HAS_TESTS
#' @export
forecast_effect.bage_prior_normfixed <- function(prior,
                                                 nm_prior,
                                                 hyper_est,
                                                 hyper_forecast,
                                                 effect_est,
                                                 matrix_along_by_est,
                                                 matrix_along_by_forecast,
                                                 levels_forecast) {
  n_along_forecast <- nrow(matrix_along_by_forecast)
  sd <- prior$specific$sd
  n_draw <- rvec::n_draw(effect_est$.fitted[[1L]])
  .fitted <- rvec::rnorm_rvec(n = n_along_forecast,
                              sd = sd,
                              n_draw = n_draw)
  tibble::tibble(term = nm_prior,
                 component = "effect",
                 level = levels_forecast,
                 .fitted = .fitted)
}


## HAS_TESTS
#' @export
forecast_effect.bage_prior_rw <- function(prior,
                                          nm_prior,
                                          hyper_est,
                                          hyper_forecast,
                                          effect_est,
                                          matrix_along_by_est,
                                          matrix_along_by_forecast,
                                          levels_forecast) {
  n_along_est <- nrow(matrix_along_by_est)
  n_along_forecast <- nrow(matrix_along_by_forecast)
  sd <- hyper_est$.fitted[[hyper_est$level == "sd"]]
  tmp <- rep(effect_est$.fitted[[n_along_est]],
             times = n_along_forecast + 1L)
  for (j in seq_len(n_along_forecast))
    tmp[[j + 1L]] <- rvec::rnorm_rvec(n = 1L, mean = tmp[[j]], sd = sd)
  .fitted <- tmp[-1L]
  tibble::tibble(term = nm_prior,
                 component = "effect",
                 level = levels_forecast,
                 .fitted = .fitted)
}

## HAS_TESTS
#' @export
forecast_effect.bage_prior_rw2 <- function(prior,
                                           nm_prior,
                                           hyper_est,
                                           hyper_forecast,
                                           effect_est,
                                           matrix_along_by_est,
                                           matrix_along_by_forecast,
                                           levels_forecast) {
  n_along_est <- nrow(matrix_along_by_est)
  n_along_forecast <- nrow(matrix_along_by_forecast)
  sd <- hyper_est$.fitted[[hyper_est$level == "sd"]]
  tmp <- rep(effect_est$.fitted[[n_along_est - 1L]],
             times = n_along_forecast + 2L)
  tmp[[2L]] <- effect_est$.fitted[[n_along_est]]
  for (j in seq_len(n_along_forecast))
    tmp[[j + 2L]] <- rvec::rnorm_rvec(n = 1L,
                                      mean = 2 * tmp[[j + 1L]] - tmp[[j]],
                                      sd = sd)
  .fitted <- tmp[-(1:2)]
  tibble::tibble(term = nm_prior,
                 component = "effect",
                 level = levels_forecast,
                 .fitted = .fitted)
}

## HAS_TESTS
#' @export
forecast_effect.bage_prior_seas <- function(prior,
                                            nm_prior,
                                            hyper_est,
                                            hyper_forecast,
                                            effect_est,
                                            matrix_along_by_est,
                                            matrix_along_by_forecast,
                                            levels_forecast) {
  n_seas <- prior$specific$n
  n_along_est <- nrow(matrix_along_by_est)
  n_along_forecast <- nrow(matrix_along_by_forecast)
  sd <- hyper_est$.fitted[[hyper_est$level == "sd"]]
  tmp <- rep(effect_est$.fitted[[1L]],
             times = n_along_forecast + n_seas)
  s_head <- seq_len(n_seas)
  s_tail <- seq.int(to = n_along_est, length.out = n_seas)
  tmp[s_head] <- effect_est$.fitted[s_tail]
  for (j in seq_len(n_along_forecast))
    tmp[[j + n_seas]] <- rvec::rnorm_rvec(n = 1L, mean = tmp[[j]], sd = sd)
  .fitted <- tmp[-s_head]
  tibble::tibble(term = nm_prior,
                 component = "seasonal",
                 level = levels_forecast,
                 .fitted = .fitted)
}




## 'forecast_hyper' -----------------------------------------------------------

#' Forecast Hyper-Parameters for Prior
#'
#' No hyper-parameters are currently time-varying,
#' so no forecasting is done. This may change in future,
#' if, for instance, we implement a local trend prior.
#'
#' @param prior Object of class 'bage_prior'
#' @param hypers_est Tibble with
#' estimated values for hyper-parameters
#' @param levels_forecast Character vector
#' with labels for future time periods.
#'
#' @returns A tibble or NULL.
#' 
#' @noRd
forecast_hyper <- function(prior,
                           hyper_est,
                           levels_forecast) {
  UseMethod("forecast_hyper")
}

## HAS_TESTS
#' @export
forecast_hyper.bage_prior <- function(prior,
                                      hyper_est,
                                      levels_forecast) {
  NULL
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
has_hyperrand.bage_prior_elin <- function(prior) TRUE


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
  check_is_main_effect(nm = nm,
                       prior = prior)
  length_effect <- length(matrix_along_by)
  n <- prior$specific$n
  check_length_effect_ge(length_effect = length_effect,
                         min = n + 1L,
                         nm = nm,
                         prior = prior)
  invisible(TRUE)
}

## HAS_TESTS
#' @export
is_prior_ok_for_term.bage_prior_ear <- function(prior,
                                                nm,
                                                matrix_along_by,
                                                var_time,
                                                var_age,
                                                var_sexgender,
                                                agesex) {
  check_is_interaction(nm = nm,
                       prior = prior)
  length_along <- nrow(matrix_along_by)
  n <- prior$specific$n
  check_length_along_ge(length_along = length_along,
                        min = n + 1L,
                        nm = nm,
                        prior = prior)
  invisible(TRUE)
}

## HAS_TESTS
#' @export
is_prior_ok_for_term.bage_prior_elin <- function(prior,
                                                 nm,
                                                 matrix_along_by,
                                                 var_time,
                                                 var_age,
                                                 var_sexgender,
                                                 agesex) {
  check_is_interaction(nm = nm, prior = prior)
  length_along <- nrow(matrix_along_by)
  check_length_along_ge(length_along = length_along,
                        min = 2L,
                        nm = nm,
                        prior = prior)
  invisible(TRUE)
}

## HAS_TESTS
#' @export
is_prior_ok_for_term.bage_prior_erw <- function(prior,
                                                nm,
                                                matrix_along_by,
                                                var_time,
                                                var_age,
                                                var_sexgender,
                                                agesex) {
  check_is_interaction(nm = nm,
                       prior = prior)
  length_along <- nrow(matrix_along_by)
  check_length_along_ge(length_along = length_along,
                        min = 2L,
                        nm = nm,
                        prior = prior)
  invisible(TRUE)
}

## HAS_TESTS
#' @export
is_prior_ok_for_term.bage_prior_erw2 <- function(prior,
                                                 nm,
                                                 matrix_along_by,
                                                 var_time,
                                                 var_age,
                                                 var_sexgender,
                                                 agesex) {
  check_is_interaction(nm = nm,
                       prior = prior)
  length_along <- nrow(matrix_along_by)
  check_length_along_ge(length_along = length_along,
                        min = 3L,
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
  check_is_main_effect(nm = nm, prior = prior)
  length_effect <- length(matrix_along_by)
  check_length_effect_ge(length_effect = length_effect,
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
  check_is_main_effect(nm = nm,
                       prior = prior)
  length_effect <- length(matrix_along_by)
  n <- prior$specific$n
  check_length_effect_ge(length_effect = length_effect,
                         min = n + 1L,
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
  check_is_main_effect(nm = nm, prior = prior)
  length_effect <- length(matrix_along_by)
  check_length_effect_ge(length_effect = length_effect,
                         min = 2L,
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
  check_is_main_effect(nm = nm, prior = prior)
  length_effect <- length(matrix_along_by)
  check_length_effect_ge(length_effect = length_effect,
                         min = 3L,
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
  check_is_main_effect(nm = nm, prior = prior)
  length_effect <- length(matrix_along_by)
  check_length_effect_ge(length_effect = length_effect,
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
  n_dim <- length(strsplit(nm, split = ":")[[1L]])
  str <- str_call_prior(prior)
  joint <- prior$specific$joint
  is_svds <- !is.null(joint)
  msg1 <- "Problem with {.var {str}} prior for {.var {nm}} term."
  ## check that 'var_age' has been identified
  if (is.null(var_age))
    cli::cli_abort(c(msg1,
                     i = "Can't use {.var {str}} prior when age variable not yet identified.",
                     i = "Use function {.fun set_var_age} to identify age variable?"))
  ## check that 'agesex' is not "other"
  if (agesex == "other")
    cli::cli_abort(c(msg1,
                     i = "{.var {str}} prior should be used with terms involving age."))
  ## one dimension - must be age, otherwise 'agesex' would be "other"
  if (n_dim == 1L) {
    if (is_svds)
      cli::cli_abort(c(msg1,
                       i = "{.var {str}} prior should be used for interaction between age and sex/gender.",
                       i = "{.var {nm}} term is an age main effect.",
                       i = "Use function {.fun SVD} instead?"))
  }
  else if (n_dim == 2L) {
    if (agesex %in% c("age:sex", "sex:age")) {
      if (!is_svds)
        cli::cli_abort(c(msg1,
                         i = "{.var {str}} prior should be used for age main effect.",
                         i = "{.var {nm}} term is an interaction between age and sex/gender.",
                         i = "Use function {.fun SVDS} instead?"))
    }
    else if (agesex == "age:other") {
      if (is_svds) {
        if (is.null(var_sexgender))
          msg3 <- c(i = "sex/gender variable not identified.",
                    i = "Use function {.fun set_var_sexgender} to identify?")
        else
          msg3 <- c(i = "{.var {nm}} term does not involve sex/gender.",
                    i = "Use function {.fun ESVD} instead?")
        cli::cli_abort(c(msg1,
                         i = "{.var {str}} prior should be used for interaction between age and sex/gender.",
                         msg3))
      }
      else
        cli::cli_abort(c(msg1,
                         i = "{.var {str}} prior should be used for age main effect.",
                         i = "{.var {nm}} term is an interaction.",
                         i = "Use function {.fun ESVD} instead?"))
    }
    else
      cli::cli_abort("Internal error: unexpected value for {.var agesex}.")
  }
  else { ## n_dim > 2
    if (agesex %in% c("age:sex:other", "sex:age:other")) {
      if (is_svds) {
        cli::cli_abort(c(msg1,
                         i = "{.var {str}} prior should be used for two-way interaction between age and sex/gender.",
                         i = "{.var {nm}} term involves more than two dimensions.",
                         i = "Use function {.fun ESVDS} instead?"))
      }
      else {
        n_extra <- n_dim - 2L
        cli::cli_abort(c(msg1,
                         i = "{.var {str}} prior should be used for age main effect.",
                         i = paste("{.var {nm}} term is an interaction between age, sex/gender,",
                                   "and other {cli::qty(n_extra)} dimension{?s}."),
                         i = "Use function {.fun ESVDS} instead?"))
      }
    }
    else if (agesex == "age:other") {
      if (is_svds) {
        cli::cli_abort(c(msg1,
                         i = "{.var {str}} prior should be used for interaction between age and sex/gender.",
                         i = "{.var {nm}} term does not involve sex/gender.",
                         i = "{.var {nm}} term involves more than two dimensions.",
                         i = "Use function {.fun ESVD} instead?"))
      }
      else {
        cli::cli_abort(c(msg1,
                         i = "{.var {str}} prior should be used for age main effect.",
                         i = "{.var {nm}} term is an interaction.",
                         i = "Use function {.fun ESVD} instead?"))
      }
    }
    else
      cli::cli_abort("Internal error: unexpected value for {.var agesex}.")
  }
  invisible(TRUE)
}

## HAS_TESTS
#' @export
is_prior_ok_for_term.bage_prior_esvd <- function(prior,
                                                 nm,
                                                 matrix_along_by,
                                                 var_time,
                                                 var_age,
                                                 var_sexgender,
                                                 agesex) {
  n_dim <- length(strsplit(nm, split = ":")[[1L]])
  str <- str_call_prior(prior)
  joint <- prior$specific$joint
  is_esvds <- !is.null(joint)
  msg1 <- "Problem with {.var {str}} prior for {.var {nm}} term."
  ## check that 'var_age' has been identified
  if (is.null(var_age))
    cli::cli_abort(c(msg1,
                     i = "Can't use {.var {str}} prior when age variable not yet identified.",
                     i = "Use function {.fun set_var_age} to identify age variable?"))
  ## check that 'agesex' is not "other"
  if (agesex == "other")
    cli::cli_abort(c(msg1,
                     i = "{.var {str}} prior should be used with terms involving age."))
  ## one dimension - must be age, otherwise 'agesex' would be "other"
  if (n_dim == 1L) {
    if (is_esvds)
      cli::cli_abort(c(msg1,
                       i = paste("{.var {str}} prior should be used for interaction",
                                 "involving age and sex/gender."),
                       i = "{.var {nm}} term is an age main effect.",
                       i = "Use function {.fun SVD} instead?"))
    else
      cli::cli_abort(c(msg1,
                       i = "{.var {str}} prior should be used for interaction involving age.",
                       i = "{.var {nm}} term is an age main effect.",
                       i = "Use function {.fun SVD} instead?"))
  }
  else if (n_dim == 2L) {
    if (agesex %in% c("age:sex", "sex:age")) {
      if (is_esvds) {
        cli::cli_abort(c(msg1,
                         i = paste("{.var {str}} prior should be used for interaction involving age,",
                                   "sex/gender, and at least one other dimension."),
                         i = "{.var {nm}} term is an interaction between age and sex/gender only.",
                         i = "Use function {.fun SVDS} prior instead?"))
      }
      else {
        cli::cli_abort(c(msg1,
                         i = paste("{.var {str}} prior should be used for interaction involving age",
                                   "and non-sex, non-gender dimensions."),
                         i = "{.var {nm}} term is an interaction between age and sex/gender.",
                         i = "Use function {.fun SVDS} instead?"))
      }
    }
    else if (agesex == "age:other") {
      if (is_esvds) {
        if (is.null(var_sexgender))
          msg3 <- c(i = "sex/gender variable not identified.",
                    i = "Use function {.fun set_var_sexgender} to identify?")
        else
          msg3 <- c(i = "{.var {nm}} term does not involve sex/gender.",
                    i = "Use function {.fun ESVD} instead?")
        cli::cli_abort(c(msg1,
                         i = paste("{.var {str}} prior should be used for interaction",
                                   "between age and sex/gender."),
                         msg3))
      }
    }
    else
      cli::cli_abort("Internal error: unexpected value for {.var agesex}.")
  }
  else { ## n_dim > 2
    if (agesex %in% c("age:sex:other", "sex:age:other")) {
      if (!is_esvds) {
        cli::cli_abort(c(msg1,
                         i = paste("{.var {str}} prior should be used for interaction involving",
                                   "age and non-sex, non-gender dimensions."),
                         i = "{.var {nm}} term is an interaction involving age and sex/gender.",
                         i = "Use function {.fun ESVDS} instead?"))
      }
    }
    else if (agesex == "age:other") {
      if (is_esvds) {
        cli::cli_abort(c(msg1,
                         i = paste("{.var {str}} prior should be used for interaction",
                                   "involving age and sex/gender."),
                         i = "{.var {nm}} term does not involve sex/gender.",
                         i = "Use function {.fun ESVD} instead?"))
      }
    }
    else
      cli::cli_abort("Internal error: unexpected value for {.var agesex}.")
  }
  invisible(TRUE)
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
  n <- prior$specific$n
  coef <- paste0("coef", seq_len(n))
  c(coef, "sd")
}

## HAS_TESTS
#' @export
levels_hyper.bage_prior_ear <- function(prior) {
  n <- prior$specific$n
  rep(c("coef", "sd"), times = c(n, 1L))
}

## HAS_TESTS
#' @export
levels_hyper.bage_prior_elin <- function(prior) {
  c("slope", "sd", "msd")
}

## HAS_TESTS
#' @export
levels_hyper.bage_prior_erw <- function(prior)
  "sd"

## HAS_TESTS
#' @export
levels_hyper.bage_prior_erw2 <- function(prior)
  "sd"

## HAS_TESTS
#' @export
levels_hyper.bage_prior_eseas <- function(prior)
  "sd"

## HAS_TESTS
#' @export
levels_hyper.bage_prior_known <- function(prior)
    character()

## HAS_TESTS
#' @export
levels_hyper.bage_prior_lin <- function(prior)
    c("slope", "sd")

## HAS_TESTS
#' @export
levels_hyper.bage_prior_linar <- function(prior) {
  n <- prior$specific$n
  coef <- paste0("coef", seq_len(n))
  c("slope", "sd", coef)
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
levels_hyper.bage_prior_rw2 <- function(prior)
  "sd"

## HAS_TESTS
#' @export
levels_hyper.bage_prior_seas <- function(prior)
    "sd"

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
levels_hyper.bage_prior_esvd <- function(prior)
    character()


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
levels_hyperrand.bage_prior_elin <- function(prior, matrix_along_by, levels_effect) {
  n_by <- ncol(matrix_along_by)
  nms_by <- colnames(matrix_along_by)
  paste("mslope", nms_by, sep = ".")
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
                                                            matrix_agesex) {
  n_spline <- prior$specific$n
  if (is.null(n_spline)) {
    n_spline <- 0.7 * length(levels_effect)
    n_spline <- ceiling(n_spline)
    n_spline <- max(n_spline, 4L)
  }
  length_effect <- length(levels_effect)
  ans <- make_spline_matrix(n_spline = n_spline,
                            length_effect = length_effect)
  rownames(ans) <- levels_effect
  ans
}

## HAS_TESTS
#' @export
make_matrix_effectfree_effect.bage_prior_svd <- function(prior,
                                                         levels_effect,
                                                         agesex,
                                                         levels_age,
                                                         levels_sexgender,
                                                         matrix_agesex) {
  ssvd <- prior$specific$ssvd
  joint <- prior$specific$joint
  n_comp <- prior$specific$n
  get_matrix_or_offset_svd(ssvd = ssvd,
                           levels_age,
                           levels_sexgender,
                           joint = joint,
                           agesex = agesex,
                           get_matrix = TRUE,
                           n_comp = n_comp)
}

## HAS_TESTS
#' @export
make_matrix_effectfree_effect.bage_prior_esvd <- function(prior,
                                                          levels_effect,
                                                          agesex,
                                                          levels_age,
                                                          levels_sexgender,
                                                          matrix_agesex) {
  ssvd <- prior$specific$ssvd
  joint <- prior$specific$joint
  is_svds <- !is.null(joint)
  n_comp <- prior$specific$n
  n_by <- ncol(matrix_agesex)
  F <- get_matrix_or_offset_svd(ssvd = ssvd,
                                levels_age,
                                levels_sexgender,
                                joint = joint,
                                agesex = agesex,
                                get_matrix = TRUE,
                                n_comp = n_comp)
  I <- Matrix::.sparseDiagonal(n_by)
  m_inner <- Matrix::kronecker(I, F)
  m_outer <- make_matrix_agesex_index(matrix_agesex)
  m_outer %*% m_inner
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
                                                         matrix_agesex) {
  ssvd <- prior$specific$ssvd
  joint <- prior$specific$joint
  matrix <- get_matrix_or_offset_svd(ssvd = ssvd,
                                     levels_age,
                                     levels_sexgender,
                                     joint = joint,
                                     agesex = agesex,
                                     get_matrix = FALSE,
                                     n_comp = NULL)
}

## HAS_TESTS
#' @export
make_offset_effectfree_effect.bage_prior_esvd <- function(prior,
                                                          levels_effect,
                                                          agesex,
                                                          levels_age,
                                                          levels_sexgender,
                                                          matrix_agesex) {
  ssvd <- prior$specific$ssvd
  joint <- prior$specific$joint
  is_svds <- !is.null(joint)
  n_comp <- prior$specific$n
  n_by <- ncol(matrix_agesex)
  g <- get_matrix_or_offset_svd(ssvd = ssvd,
                                levels_age,
                                levels_sexgender,
                                joint = joint,
                                agesex = agesex,
                                get_matrix = FALSE,
                                n_comp = n_comp)
  ones <- Matrix::sparseMatrix(i = seq_len(n_by),
                               j = rep.int(1L, times = n_by),
                               x = rep.int(1L, times = n_by))
  m_inner <- Matrix::kronecker(ones, g)
  m_outer <- make_matrix_agesex_index(matrix_agesex)
  ans <- m_outer %*% m_inner
  ans <- Matrix::drop(ans)
  ans
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
reformat_hyperrand_one.bage_prior_elin <- function(prior,
                                                   nm_prior,
                                                   matrix_along_by,
                                                   components) {
  is_change <- with(components, component == "hyperrand" & term == nm_prior)
  components$component[is_change] <- "hyper"
  components
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
  specific <- prior$specific
  n <- specific$n
  min <- specific$min
  max <- specific$max
  scale <- specific$scale
  nm <- specific$nm
  if (nm == "AR") {
    args <- character(2L)
    if (n != 2L)
      args[[1L]] <- sprintf("n=%d", n)
    if (scale != 1)
      args[[2L]] <- sprintf("s=%s", scale)
  }
  else if (nm == "AR1") {
    args <- character(3L)
    if (min != 0.8)
      args[[1L]] <- sprintf("min=%s", min)
    if (max != 0.98)
      args[[2L]] <- sprintf("max=%s", max)
    if (scale != 1)
      args[[3L]] <- sprintf("s=%s", scale)
  }
  else
    cli::cli_abort("Internal error: Invalid value for 'nm'.") ## nocov
  args <- args[nzchar(args)]
  args <- paste(args, collapse = ",")
  sprintf("%s(%s)", nm, args)
}

## HAS_TESTS
#' @export
str_call_prior.bage_prior_ear <- function(prior) {
  specific <- prior$specific
  n <- specific$n
  min <- specific$min
  max <- specific$max
  scale <- specific$scale
  nm <- specific$nm
  if (nm == "EAR") {
    args <- character(2L)
    args[[1L]] <- sprintf("n=%d", n)
    if (scale != 1)
      args[[2L]] <- sprintf("s=%s", scale)
  }
  else if (nm == "EAR1") {
    args <- character(3L)
    if (min != 0.8)
      args[[1L]] <- sprintf("min=%s", min)
    if (max != 0.98)
      args[[2L]] <- sprintf("max=%s", max)
    if (scale != 1)
      args[[3L]] <- sprintf("s=%s", scale)
  }
  else
    cli::cli_abort("Internal error: Invalid value for 'nm'.") ## nocov
  args <- args[nzchar(args)]
  args <- paste(args, collapse = ",")
  sprintf("%s(%s)", nm, args)
}

## HAS_TESTS
#' @export
str_call_prior.bage_prior_elin <- function(prior) {
  scale <- prior$specific$scale
  sd_slope <- prior$specific$sd_slope
  mscale <- prior$specific$mscale
  along <- prior$specific$along
  args <- character(4L)
  if (scale != 1)
    args[[1L]] <- sprintf("s=%s", scale)
  if (sd_slope != 1)
    args[[2L]] <- sprintf("sd=%s", sd_slope)
  if (mscale != 1)
    args[[3L]] <- sprintf("ms=%s", mscale)
  if (!is.null(along))
    args[[4L]] <- sprintf('along="%s"', along)
  args <- args[nzchar(args)]
  args <- paste(args, collapse = ",")
  sprintf("ELin(%s)", args)
}

## HAS_TESTS
#' @export
str_call_prior.bage_prior_erw <- function(prior) {
  scale <- prior$specific$scale
  along <- prior$specific$along
  args <- character(2L)
  if (scale != 1)
    args[[1L]] <- sprintf("s=%s", scale)
  if (!is.null(along))
    args[[2L]] <- sprintf('along="%s"', along)
  args <- args[nzchar(args)]
  args <- paste(args, collapse = ",")
  sprintf("ERW(%s)", args)
}

## HAS_TESTS
#' @export
str_call_prior.bage_prior_erw2 <- function(prior) {
  scale <- prior$specific$scale
  along <- prior$specific$along
  args <- character(2L)
  if (scale != 1)
    args[[1L]] <- sprintf("s=%s", scale)
  if (!is.null(along))
    args[[2L]] <- sprintf('along="%s"', along)
  args <- args[nzchar(args)]
  args <- paste(args, collapse = ",")
  sprintf("ERW2(%s)", args)
}

## HAS_TESTS
#' @export
str_call_prior.bage_prior_eseas <- function(prior) {
  n <- prior$specific$n
  scale <- prior$specific$scale
  along <- prior$specific$along
  args <- character(3L)
  args[[1L]] <- sprintf("n=%d", n)
  if (scale != 1)
    args[[2L]] <- sprintf("s=%s", scale)
  if (!is.null(along))
    args[[3L]] <- sprintf('along="%s"', along)
  args <- args[nzchar(args)]
  args <- paste(args, collapse = ",")
  sprintf("ESeas(%s)", args)
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
    scale <- prior$specific$scale
    sd_slope <- prior$specific$sd_slope
    if (isTRUE(all.equal(scale, 1))) {
        if (isTRUE(all.equal(sd_slope, 1)))
            "Lin()"
        else
            sprintf("Lin(sd=%s)", sd_slope)
    }
    else {
        if (isTRUE(all.equal(sd_slope, 1)))
            sprintf("Lin(s=%s)", scale)
        else
            sprintf("Lin(s=%s,sd=%s)", scale, sd_slope)
    }
}

## HAS_TESTS
#' @export
str_call_prior.bage_prior_linar <- function(prior) {
  specific <- prior$specific
  n <- specific$n
  min <- specific$min
  max <- specific$max
  scale <- specific$scale
  sd_slope <- specific$sd_slope
  nm <- specific$nm
  if (nm == "LinAR") {
    args <- character(3L)
    if (n != 2L)
      args[[1L]] <- sprintf("n=%d", n)
    if (scale != 1)
      args[[2L]] <- sprintf("s=%s", scale)
    if (sd_slope != 1)
      args[[3L]] <- sprintf("sd=%s", sd_slope)
  }
  else if (nm == "LinAR1") {
    args <- character(4L)
    if (min != 0.8)
      args[[1L]] <- sprintf("min=%s", min)
    if (max != 0.98)
      args[[2L]] <- sprintf("max=%s", max)
    if (scale != 1)
      args[[3L]] <- sprintf("s=%s", scale)
    if (sd_slope != 1)
      args[[4L]] <- sprintf("sd=%s", sd_slope)
  }
  else
    cli::cli_abort("Internal error: Invalid value for 'nm'.") ## nocov
  args <- args[nzchar(args)]
  args <- paste(args, collapse = ",")
  sprintf("%s(%s)", nm, args)
}

## HAS_TESTS
#' @export
str_call_prior.bage_prior_norm <- function(prior) {
    scale <- prior$specific$scale
    if (isTRUE(all.equal(scale, 1)))
        "N()"
    else
        sprintf("N(s=%s)", scale)
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
    scale <- prior$specific$scale
    if (isTRUE(all.equal(scale, 1)))
        "RW()"
    else
        sprintf("RW(s=%s)", scale)
}

## HAS_TESTS
#' @export
str_call_prior.bage_prior_rw2 <- function(prior) {
  scale <- prior$specific$scale
  if (isTRUE(all.equal(scale, 1)))
    "RW2()"
  else
    sprintf("RW2(s=%s)", scale)
}

## HAS_TESTS
#' @export
str_call_prior.bage_prior_seas <- function(prior) {
  n <- prior$specific$n
  scale <- prior$specific$scale
  args <- character(2L)
  args[[1]] <- sprintf("n=%d", n)
  if (scale != 1)
    args[[2L]] <- sprintf("s=%s", scale)
  args <- args[nzchar(args)]
  args <- paste(args, collapse = ",")
  sprintf("Seas(%s)", args)
}

## HAS_TESTS
#' @export
str_call_prior.bage_prior_spline <- function(prior) {
  n <- prior$specific$n
  scale <- prior$specific$scale
  args <- character(2L)
  if (!is.null(n))
    args[[1L]] <- sprintf("n=%s", n)
  if (scale != 1)
    args[[2L]] <- sprintf("s=%s", scale)
  args <- args[nzchar(args)]
  args <- paste(args, collapse = ",")
  sprintf("Sp(%s)", args)
}

## HAS_TESTS
#' @export
str_call_prior.bage_prior_svd <- function(prior) {
  ssvd <- prior$specific$ssvd
  nm_ssvd <- prior$specific$nm_ssvd
  n <- prior$specific$n
  joint <- prior$specific$joint
  args <- character(3L)
  fun <- if (is.null(joint)) "SVD" else "SVDS"
  args[[1L]] <- nm_ssvd
  n_comp <- get_n_comp(ssvd)
  n_default <- ceiling(n_comp / 2)
  if (n != n_default)
    args[[2L]] <- sprintf("n=%s", n)
  if (!is.null(joint) && joint)
    args[[3L]] <- "joint=TRUE"
  args <- args[nzchar(args)]
  args <- paste(args, collapse = ",")
  sprintf("%s(%s)", fun, args)
}

## HAS_TESTS
#' @export
str_call_prior.bage_prior_esvd <- function(prior) {
  nm_ssvd <- prior$specific$nm_ssvd
  n <- prior$specific$n
  joint <- prior$specific$joint
  args <- character(3L)
  fun <- if (is.null(joint)) "ESVD" else "ESVDS"
  args[[1L]] <- nm_ssvd
  if (n != 5)
    args[[2L]] <- sprintf("n=%s", n)
  if (!is.null(joint) && joint)
    args[[3L]] <- "joint=TRUE"
  args <- args[nzchar(args)]
  args <- paste(args, collapse = ",")
  sprintf("%s(%s)", fun, args)
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
str_nm_prior.bage_prior_ear <- function(prior) {
  nm <- prior$specific$nm
  sprintf("%s()", nm)
}

## HAS_TESTS
#' @export
str_nm_prior.bage_prior_elin <- function(prior) {
  "ELin()"
}

## HAS_TESTS
#' @export
str_nm_prior.bage_prior_erw <- function(prior) {
  "ERW()"
}

## HAS_TESTS
#' @export
str_nm_prior.bage_prior_erw2 <- function(prior) {
  "ERW2()"
}

## HAS_TESTS
#' @export
str_nm_prior.bage_prior_eseas <- function(prior) {
  "ESeas()"
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
str_nm_prior.bage_prior_rw2 <- function(prior) {
  "RW2()"
}

## HAS_TESTS
#' @export
str_nm_prior.bage_prior_seas <- function(prior) {
  "Seas()"
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
str_nm_prior.bage_prior_esvd <- function(prior) {
  joint <- prior$specific$joint
  nm <- if (is.null(joint)) "ESVD" else "ESVDS"
  sprintf("%s()", nm)
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
transform_hyper.bage_prior_ar <- function(prior) {
  specific <- prior$specific
  n <- specific$n
  min <- specific$min
  max <- specific$max
  shifted_inv_logit <- function(x) {
    ans_raw <- exp(x) / (1 + exp(x))
    ans <- (max - min) * ans_raw + min
    ans
  }
  rep(list(coef = shifted_inv_logit, sd = exp),
      times = c(n, 1L))
}

## HAS_TESTS
#' @export
transform_hyper.bage_prior_ear <- function(prior) {
  specific <- prior$specific
  n <- specific$n
  min <- specific$min
  max <- specific$max
  shifted_inv_logit <- function(x) {
    ans_raw <- exp(x) / (1 + exp(x))
    ans <- (max - min) * ans_raw + min
    ans
  }
  rep(list(coef = shifted_inv_logit, sd = exp),
      times = c(n, 1L))
}

## HAS_TESTS
#' @export
transform_hyper.bage_prior_elin <- function(prior) {
  list(slope = identity,
       sd = exp,
       msd = exp)
}

## HAS_TESTS
#' @export
transform_hyper.bage_prior_erw <- function(prior) {
  list(sd = exp)
}

## HAS_TESTS
#' @export
transform_hyper.bage_prior_erw2 <- function(prior) {
  list(sd = exp)
}

## HAS_TESTS
#' @export
transform_hyper.bage_prior_eseas <- function(prior) {
  list(sd = exp)
}

## HAS_TESTS
#' @export
transform_hyper.bage_prior_known <- function(prior)
    list()

## HAS_TESTS
#' @export
transform_hyper.bage_prior_lin <- function(prior)
    list(slope = identity, sd = exp)

## HAS_TESTS
#' @export
transform_hyper.bage_prior_linar <- function(prior) {
  specific <- prior$specific
  n <- specific$n
  min <- specific$min
  max <- specific$max
  shifted_inv_logit <- function(x) {
    ans_raw <- exp(x) / (1 + exp(x))
    ans <- (max - min) * ans_raw + min
    ans
  }
  rep(list(slope = identity,
           sd = exp,
           coef = shifted_inv_logit),
      times = c(1L, 1L, n))
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
transform_hyper.bage_prior_rw2 <- function(prior)
  list(sd = exp)

## HAS_TESTS
#' @export
transform_hyper.bage_prior_seas <- function(prior)
    list(sd = exp)

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
transform_hyper.bage_prior_esvd <- function(prior)
    list()


## 'transform_hyperrand' ------------------------------------------------------

#' Transform to convert working TMB version
#' of parameter back to original units
#'+
#' @param prior An object of class 'bage_prior'.
#' @param matrix_along_by Matrix with mapping for along, by dimensions
#'
#' @returns A list of functions.
#'
#' @noRd
transform_hyperrand <- function(prior, matrix_along_by) {
    UseMethod("transform_hyperrand")
}

## HAS_TESTS
#' @export
transform_hyperrand.bage_prior <- function(prior, matrix_along_by)
    list()

## HAS_TESTS
#' @export
transform_hyperrand.bage_prior_elin <- function(prior, matrix_along_by) {
  n_by <- ncol(matrix_along_by)
  rep(list(mslope = identity),
      times = n_by)
}


## 'use_for_interaction' ------------------------------------------------------

## HAS_TESTS
#' Whether a Prior is Used Exclusively to Model Interactions
#'
#' If a prior is used to model interactions *and* main effects,
#' then the function returns `FALSE`.
#' 
#' @param prior Object of class `"bage_prior"`.
#'
#' @returns `TRUE` or `FALSE`
#'
#' @noRd
use_for_interaction <- function(prior) {
  UseMethod("use_for_interaction")
}

#' @export
use_for_interaction.bage_prior <- function(prior) FALSE

#' @export
use_for_interaction.bage_prior_ear <- function(prior) TRUE

#' @export
use_for_interaction.bage_prior_elin <- function(prior) TRUE

#' @export
use_for_interaction.bage_prior_erw <- function(prior) TRUE

#' @export
use_for_interaction.bage_prior_erw2 <- function(prior) TRUE

#' @export
use_for_interaction.bage_prior_eseas <- function(prior) TRUE

#' @export
use_for_interaction.bage_prior_esvd <- function(prior) TRUE


## 'use_for_main_effect' ------------------------------------------------------

## HAS_TESTS
#' Whether a Prior is Used Exclusively to Model Main Effects
#'
#' If a prior is used to model main effects *and* interactions,
#' then the function returns `FALSE`.
#' 
#' @param prior Object of class `"bage_prior"`.
#'
#' @returns `TRUE` or `FALSE`
#'
#' @noRd
use_for_main_effect <- function(prior) {
  UseMethod("use_for_main_effect")
}

#' @export
use_for_main_effect.bage_prior <- function(prior) FALSE

#' @export
use_for_main_effect.bage_prior_ar <- function(prior) TRUE

#' @export
use_for_main_effect.bage_prior_lin <- function(prior) TRUE

#' @export
use_for_main_effect.bage_prior_linar <- function(prior) TRUE

#' @export
use_for_main_effect.bage_prior_rw <- function(prior) TRUE

#' @export
use_for_main_effect.bage_prior_rw2 <- function(prior) TRUE

#' @export
use_for_main_effect.bage_prior_spline <- function(prior) TRUE


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
uses_along.bage_prior <- function(prior) FALSE

## HAS_TESTS
#' @export
uses_along.bage_prior_ear <- function(prior) TRUE

## HAS_TESTS
#' @export
uses_along.bage_prior_elin <- function(prior) TRUE

## HAS_TESTS
#' @export
uses_along.bage_prior_erw <- function(prior) TRUE

## HAS_TESTS
#' @export
uses_along.bage_prior_erw2 <- function(prior) TRUE

## HAS_TESTS
#' @export
uses_along.bage_prior_eseas <- function(prior) TRUE


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
uses_hyperrand.bage_prior_elin <- function(prior) TRUE


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
uses_matrix_effectfree_effect.bage_prior_esvd <- function(prior) TRUE


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
uses_offset_effectfree_effect.bage_prior_esvd <- function(prior) TRUE


## 'vals_hyper_to_dataframe' --------------------------------------------------

#' Convert 'vals_hyper' into Data Frame
#'
#' Convert 'vals_hyper' into data frame suitable
#' for using in 'draw_components'
#'
#' @param prior Object of class 'bage_prior'
#' @param nm_prior String
#' @param vals_hyper Named list
#' @param n_sim Integer
#'
#' @returns A tibble.
#'
#' @noRd
vals_hyper_to_dataframe <- function(prior, nm_prior, vals_hyper, n_sim) {
  UseMethod("vals_hyper_to_dataframe")
}

## HAS_TESTS
#' @export
vals_hyper_to_dataframe.bage_prior <- function(prior, nm_prior, vals_hyper, n_sim) {
  vals <- vctrs::vec_rbind(!!!vals_hyper, .name_repair = "universal_quiet")
  if (nrow(vals) > 0L) {
    vals <- as.matrix(vals)
    dimnames(vals) <- NULL
  }
  else
    vals <- matrix(NA_real_, nrow = 0L, ncol = n_sim)
  term <- rep(nm_prior, times = nrow(vals))
  component <- rep.int("hyper", times = nrow(vals))
  if (nrow(vals) > 0L) {
    level <- lapply(vals_hyper, rownames)
    no_rownames <- vapply(level, is.null, FALSE)
    level[no_rownames] <- names(vals_hyper[no_rownames])
    level <- unlist(level, use.names = FALSE)
  }
  else
    level <- character()
  .fitted <- rvec::rvec(vals)
  tibble::tibble(term = term,
                 component = component,
                 level = level,
                 .fitted = .fitted)
}


## 'vals_hyperrand_to_dataframe' ----------------------------------------------

#' Convert 'vals_hyperrand' into Data Frame
#'
#' Convert 'vals_hyperrand' into data frame suitable
#' for using in 'draw_components'
#'
#' @param prior Object of class 'bage_prior'
#' @param nm_prior String
#' @param vals_hyperrand Named list
#' @param n_sim Integer
#'
#' @returns A tibble.
#'
#' @noRd
vals_hyperrand_to_dataframe <- function(prior, nm_prior, vals_hyperrand, n_sim) {
  UseMethod("vals_hyperrand_to_dataframe")
}

## HAS_TESTS
#' @export
vals_hyperrand_to_dataframe.bage_prior <- function(prior, nm_prior, vals_hyperrand, n_sim) {
  vals <- vctrs::vec_rbind(!!!vals_hyperrand, .name_repair = "universal_quiet")
  if (nrow(vals) > 0L) {
    vals <- as.matrix(vals)
    dimnames(vals) <- NULL
  }
  else
    vals <- matrix(NA_real_, nrow = 0L, ncol = n_sim)
  term <- rep(nm_prior, times = nrow(vals))
  component <- rep.int("hyper", times = nrow(vals))
  if (nrow(vals) > 0L) {
    level <- lapply(vals_hyperrand, rownames)
    no_rownames <- vapply(level, is.null, FALSE)
    level[no_rownames] <- names(vals_hyperrand[no_rownames])
    level <- unlist(level, use.names = FALSE)
  }
  else
    level <- character()
  .fitted <- rvec::rvec(vals)
  tibble::tibble(term = term,
                 component = component,
                 level = level,
                 .fitted = .fitted)
}


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


