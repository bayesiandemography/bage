## 'datamod_descr' ------------------------------------------------------------

#' Name of Data Model Used in Printing
#'
#' @param datamod An object of class 'bage_datamod'
#'
#' @returns A string
#'
#' @noRd
datamod_descr <- function(datamod) {
    UseMethod("datamod_descr")
}

## HAS_TESTS
#' @export
datamod_descr.bage_datamod_exposure <- function(datamod) "exposure"

## HAS_TESTS
#' @export
datamod_descr.bage_datamod_miscount <- function(datamod) "miscount"

## HAS_TESTS
#' @export
datamod_descr.bage_datamod_noise <- function(datamod) "noise"

## HAS_TESTS
#' @export
datamod_descr.bage_datamod_overcount <- function(datamod) "overcount"

## HAS_TESTS
#' @export
datamod_descr.bage_datamod_undercount <- function(datamod) "undercount"



## 'draw_datamod_param' -------------------------------------------------------

## HAS_TESTS
#' Draw from Prior Distribution for Data Model Parameters
#'
#' @param datamod Object of class 'bage_mod'
#' @param n_sim Number of draws
#'
#' @returns A matrix
#'
#' @noRd
draw_datamod_param <- function(datamod, n_sim) {
  UseMethod("draw_datamod_param")
}

## HAS_TESTS
#' @export
draw_datamod_param.bage_datamod_miscount <- function(datamod, n_sim) {
  prob_mean <- datamod$prob_mean
  prob_disp <- datamod$prob_disp
  rate_mean <- datamod$rate_mean
  rate_disp <- datamod$rate_disp
  n_prob <- length(prob_mean)
  n_rate <- length(rate_mean)
  shape1 <- prob_mean / prob_disp
  shape2 <- (1 - prob_mean) / prob_disp
  shape1 <- rep(shape1, times = n_sim)
  shape2 <- rep(shape2, times = n_sim)
  prob <- stats::rbeta(n = n_prob * n_sim, shape1 = shape1, shape2 = shape2)
  shape <- 1 / rate_disp
  scale <- rate_disp * rate_mean
  rate <- stats::rgamma(n = n_rate * n_sim, shape = shape, scale = scale)
  prob <- matrix(prob, nrow = n_prob, ncol = n_sim)
  rate <- matrix(rate, nrow = n_rate, ncol = n_sim)
  ans <- rbind(prob, rate)
  ans
}

## HAS_TESTS
#' @export
draw_datamod_param.bage_datamod_overcount <- function(datamod, n_sim) {
  rate_mean <- datamod$rate_mean
  rate_disp <- datamod$rate_disp
  n_rate <- length(rate_mean)
  shape <- 1 / rate_disp
  scale <- rate_disp * rate_mean
  rate <- stats::rgamma(n = n_rate * n_sim, shape = shape, scale = scale)
  ans <- matrix(rate, nrow = n_rate, ncol = n_sim)
  ans
}


## HAS_TESTS
#' @export
draw_datamod_param.bage_datamod_undercount <- function(datamod, n_sim) {
  prob_mean <- datamod$prob_mean
  prob_disp <- datamod$prob_disp
  n_prob <- length(prob_mean)
  shape1 <- prob_mean / prob_disp
  shape2 <- (1 - prob_mean) / prob_disp
  shape1 <- rep(shape1, times = n_sim)
  shape2 <- rep(shape2, times = n_sim)
  prob <- stats::rbeta(n = n_prob * n_sim, shape1 = shape1, shape2 = shape2)
  ans <- matrix(prob, nrow = n_prob, ncol = n_sim)
  ans
}


## 'draw_offset_obs_given_true' ----------------------------------------------

#' Draw Values for Observed Offset, Given True Offset,
#' Based on Data Model
#'
#'
#' @param datamod Object of class 'bage_datamod'
#' @param components Tibble with estimates for hyper-parameters
#' @param offset_true True values for offset.
#' Numeric vector.
#'
#' @returns An rvec
#'
#' @noRd
draw_offset_obs_given_true <- function(datamod,
                                       components,
                                       offset_true) {
  UseMethod("draw_offset_obs_given_true")
}

## HAS_TESTS
#' @export
draw_offset_obs_given_true.bage_datamod_exposure <- function(datamod,
                                                             components,
                                                             offset_true) {
  n_offset <- length(offset_true)
  n_draw <- rvec::n_draw(components$.fitted)
  ans <- rvec::new_rvec(length = n_offset, n_draw = n_draw)
  is_ok <- !is.na(offset_true)
  n_ok <- sum(is_ok)
  disp <- get_datamod_disp(datamod)
  disp_inv <- 1 / disp
  shape <- 2 + disp_inv[is_ok]
  rate <- (1 + disp_inv[is_ok]) * offset_true[is_ok]
  ans_inv <- rvec::rgamma_rvec(n = n_ok,
                               shape = shape,
                               rate = rate,
                               n_draw = n_draw)
  ans[is_ok] <- 1 / ans_inv
  ans
}


## 'draw_offset_true_given_obs' ----------------------------------------------

#' Draw Values for True Offset, Given Observed Offset,
#' Based on Data Model
#'
#'
#' @param datamod Object of class 'bage_datamod'
#' @param nm_distn Name of distribution of offset.
#' @param components Data frame with components.
#' @param outcome Values for outcome. Numeric vector
#' @param offset Observed values for offset.
#' Numeric vector.
#' @param expected Expected values for rates/prob/mean.
#' An rvec, the same length as 'outcome'.
#'
#' @returns An rvec
#'
#' @noRd
draw_offset_true_given_obs <- function(datamod,
                                       nm_distn,
                                       components,
                                       outcome,
                                       offset_obs,
                                       expected) {
  UseMethod("draw_offset_true_given_obs")
}

## HAS_TESTS
#' @export
draw_offset_true_given_obs.bage_datamod_exposure <- function(datamod,
                                                             nm_distn,
                                                             components,
                                                             outcome,
                                                             offset_obs,
                                                             expected) {
  if (nm_distn == "pois") {
    n_offset <- length(offset_obs)
    n_draw <- rvec::n_draw(expected)
    offset_obs <- rep(offset_obs, times = n_draw)
    if (rvec::is_rvec(outcome))
      outcome <- as.numeric(outcome)
    else
      outcome <- rep(outcome, times = n_draw)
    expected <- as.numeric(expected)
    is_ok <- !is.na(offset_obs) & !is.na(outcome)
    n_ok <- sum(is_ok)
    ans <- rep.int(NA_real_, times = n_offset * n_draw)
    disp <- get_datamod_disp(datamod)
    disp <- rep.int(disp, times = n_draw)
    shape <- 3 + 1 / disp[is_ok] + outcome[is_ok]
    rate <- (1 + 1 / disp[is_ok]) / offset_obs[is_ok] + expected[is_ok]
    ans[is_ok] <- rvec::rgamma_rvec(n = n_ok,
                                    shape = shape,
                                    rate = rate)
  }
  else {
    cli::cli_abort(paste("Internal error: {.var datamod} has class",
                         "{.cls {class(datamod)}} but {.var nm_distn}",
                         "is {.val {nm_distn}}."))
  }
  ans <- matrix(ans, nrow = n_offset, ncol = n_draw)
  ans <- rvec::rvec_dbl(ans)
  ans
}


## 'draw_outcome_obs_given_true' ----------------------------------------------

#' Draw Values for Observed Outcome Given True Outcome, 
#' Based on Data Model
#'
#' @param datamod Object of class 'bage_datamod'
#' @param components Data frame with components.
#' @param outcome_true True values for outcome.
#' Can be rvec or numeric vector.
#' @param offset Values for offset.
#' Numeric vector.
#' @param fitted Values for rates/prob/mean.
#' An rvec, the same length as 'outcome'.
#'
#' @returns An rvec
#'
#' @noRd
draw_outcome_obs_given_true <- function(datamod,
                                        components,
                                        outcome_true,
                                        offset,
                                        fitted) {
  UseMethod("draw_outcome_obs_given_true")
}

## HAS_TESTS
#' @export
draw_outcome_obs_given_true.bage_datamod_miscount <- function(datamod,
                                                              components,
                                                              outcome_true,
                                                              offset,
                                                              fitted) {
  n_outcome <- length(outcome_true)
  n_draw <- rvec::n_draw(fitted)
  if (rvec::is_rvec(outcome_true))
    outcome_true <- as.numeric(outcome_true)
  else
    outcome_true <- rep(outcome_true, times = n_draw)
  offset <- rep(offset, times = n_draw)
  fitted <- as.numeric(fitted)
  is_ok <- !is.na(outcome_true) & !is.na(offset)
  n_ok <- sum(is_ok)
  ans <- rep.int(NA_real_, times = n_outcome * n_draw)
  prob <- get_datamod_prob(datamod = datamod,
                           components = components)
  rate <- get_datamod_rate(datamod = datamod,
                           components = components)
  prob <- as.numeric(prob)
  rate <- as.numeric(rate)
  prob <- prob[is_ok]
  size <- outcome_true[is_ok]
  obs_true <- rbinom_guarded(prob = prob, size = size)
  lambda <- rate[is_ok] * fitted[is_ok] * offset[is_ok]
  obs_false <- rpois_guarded(lambda)
  ans[is_ok] <- obs_true + obs_false
  ans <- matrix(ans, nrow = n_outcome, ncol = n_draw)
  ans <- rvec::rvec_dbl(ans)
  ans
}

## HAS_TESTS
#' @export
draw_outcome_obs_given_true.bage_datamod_noise <- function(datamod,
                                                           components,
                                                           outcome_true,
                                                           offset,
                                                           fitted) {
  n_outcome <- length(outcome_true)
  n_draw <- rvec::n_draw(fitted)
  ans <- rep.int(NA_real_, times = n_outcome * n_draw)
  if (rvec::is_rvec(outcome_true))
    outcome_true <- as.numeric(outcome_true)
  else
    outcome_true <- rep(outcome_true, times = n_draw)
  is_ok <- !is.na(outcome_true)
  n_ok <- sum(is_ok)
  sd <- get_datamod_sd(datamod)
  sd <- rep(sd, times = n_draw)
  ans[is_ok] <- stats::rnorm(n = n_ok,
                             mean = outcome_true[is_ok],
                             sd = sd[is_ok])
  ans <- matrix(ans, nrow = n_outcome, ncol = n_draw)
  ans <- rvec::rvec_dbl(ans)
  ans
}

## HAS_TESTS
#' @export
draw_outcome_obs_given_true.bage_datamod_overcount <- function(datamod,
                                                               components,
                                                               outcome_true,
                                                               offset,
                                                               fitted) {
  n_outcome <- length(outcome_true)
  n_draw <- rvec::n_draw(fitted)
  ans <- rep.int(NA_real_, times = n_outcome * n_draw)
  if (rvec::is_rvec(outcome_true))
    outcome_true <- as.numeric(outcome_true)
  else
    outcome_true <- rep(outcome_true, times = n_draw)
  offset <- rep(offset, times = n_draw)
  fitted <- as.numeric(fitted)
  is_ok <- !is.na(outcome_true) & !is.na(offset)
  n_ok <- sum(is_ok)
  rate <- get_datamod_rate(datamod = datamod,
                           components = components)
  rate <- as.numeric(rate)
  lambda <- rate[is_ok] * fitted[is_ok] * offset[is_ok]
  obs_false <- rpois_guarded(lambda)
  ans[is_ok] <- outcome_true[is_ok] + obs_false
  ans <- matrix(ans, nrow = n_outcome, ncol = n_draw)
  ans <- rvec::rvec_dbl(ans)
  ans
}

## HAS_TESTS
#' @export
draw_outcome_obs_given_true.bage_datamod_undercount <- function(datamod,
                                                                components,
                                                                outcome_true,
                                                                offset,
                                                                fitted) {
  n_outcome <- length(outcome_true)
  n_draw <- rvec::n_draw(fitted)
  ans <- rep.int(NA_real_, times = n_outcome * n_draw)
  if (rvec::is_rvec(outcome_true))
    outcome_true <- as.numeric(outcome_true)
  else
    outcome_true <- rep(outcome_true, times = n_draw)
  offset <- rep(offset, times = n_draw)
  fitted <- as.numeric(fitted)
  is_ok <- !is.na(outcome_true) & !is.na(offset)
  n_ok <- sum(is_ok)
  prob <- get_datamod_prob(datamod = datamod,
                           components = components)
  prob <- as.numeric(prob)
  size <- outcome_true[is_ok]
  prob <- prob[is_ok]
  ans[is_ok] <- rbinom_guarded(prob = prob, size = size)
  ans <- matrix(ans, nrow = n_outcome, ncol = n_draw)
  ans <- rvec::rvec_dbl(ans)
  ans
}


## 'draw_outcome_true_given_obs' ----------------------------------------------

#' Draw Values for True Outcome, Given Observed Outcome,
#' Based on Data Model
#'
#' Note that 'outcome' is an rvec if it has been inferred
#' from confidentialized values - otherwise it is
#' a numeric vector.
#'
#' @param datamod Object of class 'bage_datamod'
#' @param nm_distn Name of distribution of outcome
#' ("pois", "binom", "norm").
#' @param components Data frame with components.
#' @param outcome Observed values for outcome.
#' Can be rvec or numeric vector.
#' @param offset Observed values for offset.
#' Numeric vector.
#' @param expected Expected values for rates/prob/mean.
#' An rvec, the same length as 'outcome'.
#' @param disp Dispersion. An rvec or NULL
#'
#' @returns An rvec
#'
#' @noRd
draw_outcome_true_given_obs <- function(datamod,
                                        nm_distn,
                                        components,
                                        outcome,
                                        offset,
                                        expected,
                                        disp) {
  UseMethod("draw_outcome_true_given_obs")
}

## HAS_TESTS
#' @export
draw_outcome_true_given_obs.bage_datamod_exposure <- function(datamod,
                                                              nm_distn,
                                                              components,
                                                              outcome,
                                                              offset,
                                                              expected,
                                                              disp) {
  if (nm_distn == "pois") {
    ans <- outcome
  }
  else {
    cli::cli_abort(paste("Internal error: {.var datamod} has class",
                         "{.cls {class(datamod)}} but {.var nm_distn}",
                         "is {.val {nm_distn}}."))
  }
  ans
}

## HAS_TESTS
#' @export
draw_outcome_true_given_obs.bage_datamod_miscount <- function(datamod,
                                                              nm_distn,
                                                              components,
                                                              outcome,
                                                              offset,
                                                              expected,
                                                              disp) {
  has_disp <- !is.null(disp)
  n_outcome <- length(outcome)
  is_ok <- !is.na(outcome) & !is.na(offset)
  n_ok <- sum(is_ok)
  n_draw <- n_draw(expected)
  ans <- rvec::new_rvec(length = n_outcome, n_draw = n_draw)
  if (nm_distn == "pois") {
    prob <- get_datamod_prob(datamod = datamod,
                             components = components)
    rate <- get_datamod_rate(datamod = datamod,
                             components = components)
    prob_obs <- prob[is_ok] / (prob[is_ok] + rate[is_ok])
    outcome_true_obs <- rbinom_guarded(size = outcome[is_ok],
                                       prob = prob_obs)
    if (has_disp) {
      size_unobs <- outcome_true_obs + (1 / disp)
      prob_unobs <- ((1 + prob[is_ok] * expected[is_ok] * offset[is_ok] * disp)
        / (1 + expected[is_ok] * offset[is_ok] * disp))
      outcome_true_unobs <- rvec::rnbinom_rvec(n = n_ok,
                                               size = size_unobs,
                                               prob = prob_unobs)
    }
    else {
      lambda <- (1 - prob[is_ok]) * expected[is_ok] * offset[is_ok]
      outcome_true_unobs <- rpois_guarded(lambda)
    }
    ans[is_ok] <- outcome_true_obs + outcome_true_unobs
  }
  else {
    cli::cli_abort(paste("Internal error: {.var datamod} has class",
                         "{.cls {class(datamod)}} but {.var nm_distn}",
                         "is {.val {nm_distn}}."))
  }
  ans
}

## HAS_TESTS
## Expecting offset, expected, and disp to be on original scale
#' @export
draw_outcome_true_given_obs.bage_datamod_noise <- function(datamod,
                                                           nm_distn,
                                                           components,
                                                           outcome,
                                                           offset,
                                                           expected,
                                                           disp) {
  if (nm_distn == "norm") {
    n_outcome <- length(outcome)
    n_draw <- rvec::n_draw(expected)
    is_ok <- !is.na(outcome) & !is.na(offset)
    n_ok <- sum(is_ok)
    sd_noise <- get_datamod_sd(datamod)
    prec_true <- offset[is_ok] / (disp^2)
    prec_noise <- 1 / (sd_noise[is_ok]^2)
    wt_true <- prec_true / (prec_true + prec_noise)
    mean_true <- (wt_true * expected[is_ok]
      + (1 - wt_true) * outcome[is_ok])
    sd_true <- 1 / sqrt(prec_true + prec_noise)
    ans <- rvec::new_rvec(length = n_outcome,
                          n_draw = n_draw)
    ans[is_ok] <- rvec::rnorm_rvec(n = n_ok,
                                   mean = mean_true,
                                   sd = sd_true)
  }
  else {
    cli::cli_abort(paste("Internal error: {.var datamod} has class",
                         "{.cls {class(datamod)}} but {.var nm_distn}",
                         "is {.val {nm_distn}}."))
  }
  ans
}

## HAS_TESTS
#' @export
draw_outcome_true_given_obs.bage_datamod_overcount <- function(datamod,
                                                               nm_distn,
                                                               components,
                                                               outcome,
                                                               offset,
                                                               expected,
                                                               disp) {
  if (nm_distn == "pois") {
    n_outcome <- length(outcome)
    n_draw <- rvec::n_draw(expected)
    is_ok <- !is.na(outcome) ## not offset, since not used
    n_ok <- sum(is_ok)
    rate <- get_datamod_rate(datamod = datamod,
                             components = components)
    prob_obs <- 1 / (1 + rate[is_ok])
    ans <- rvec::new_rvec(length = n_outcome,
                          n_draw = n_draw)
    ans[is_ok] <- rbinom_guarded(size = outcome[is_ok],
                                 prob = prob_obs)
  }
  else {
    cli::cli_abort(paste("Internal error: {.var datamod} has class",
                         "{.cls {class(datamod)}} but {.var nm_distn}",
                         "is {.val {nm_distn}}."))
  }
  ans
}

## HAS_TESTS
#' @export
draw_outcome_true_given_obs.bage_datamod_undercount <- function(datamod,
                                                                nm_distn,
                                                                components,
                                                                outcome,
                                                                offset,
                                                                expected,
                                                                disp) {
  has_disp <- !is.null(disp)
  n_outcome <- length(outcome)
  is_ok <- !is.na(outcome) & !is.na(offset)
  n_ok <- sum(is_ok)
  n_draw <- rvec::n_draw(expected)
  prob <- get_datamod_prob(datamod = datamod,
                           components = components)
  ans <- rvec::new_rvec(length = n_outcome, n_draw = n_draw)
  if (nm_distn == "pois") {
    if (has_disp) {
      size_unobs <- outcome[is_ok] + (1 / disp)
      prob_unobs <- ((1 + prob[is_ok] * expected[is_ok] * offset[is_ok] * disp)
        / (1 + expected[is_ok] * offset[is_ok] * disp))
      outcome_unobs <- rvec::rnbinom_rvec(n = n_ok,
                                          size = size_unobs,
                                          prob = prob_unobs)
    }
    else {
      lambda <- (1 - prob[is_ok]) * expected[is_ok] * offset[is_ok]
      outcome_unobs <- rpois_guarded(lambda)
    }
    ans[is_ok] <- outcome[is_ok] + outcome_unobs
  }
  else if (nm_distn == "binom") {
    if (has_disp) {
      ## more complex distribution, so gets special function
      ans[is_ok] <- draw_outcome_true_binom_betabinom(offset = offset[is_ok],
                                                      outcome = outcome[is_ok],
                                                      expected = expected[is_ok],
                                                      disp = disp,
                                                      prob = prob[is_ok])
    }
    else {
      size_unobs <- offset[is_ok] - outcome[is_ok]
      prob_unobs <- ((1 - prob[is_ok]) * expected[is_ok]
        / (1 - prob[is_ok] * expected[is_ok]))
      outcome_unobs <- rbinom_guarded(size = size_unobs,
                                      prob = prob_unobs)
      ans[is_ok] <- outcome[is_ok] + outcome_unobs
    }
  }
  else {
    cli::cli_abort(paste("Internal error: {.var datamod} has class",
                         "{.cls {class(datamod)}} but {.var nm_distn}",
                         "is {.val {nm_distn}}."))
  }
  ans
}



## 'get_datamod_transform_param' ----------------------------------------------

#' Get Function to Transform Draws for Data Model Parameters
#'
#' @param datamod Object of class 'bage_datamod'
#'
#' @returns A function
#'
#' @noRd
get_datamod_transform_param <- function(datamod) {
  UseMethod("get_datamod_transform_param")
}

## HAS_TESTS
#' @export
get_datamod_transform_param.bage_datamod_miscount <- function(datamod) {
  prob_mean <- datamod$prob_mean
  n_prob <- length(prob_mean)
  ans <- function(x) {
    s <- seq_len(n_prob)
    logit_prob <- x[s]
    log_rate <- x[-s]
    prob <- poputils::invlogit(logit_prob)
    rate <- exp(log_rate)
    vctrs::vec_c(prob, rate)
  }
}

## HAS_TESTS
#' @export
get_datamod_transform_param.bage_datamod_overcount <- function(datamod) {
  exp
}

## HAS_TESTS
#' @export
get_datamod_transform_param.bage_datamod_undercount <- function(datamod) {
  poputils::invlogit
}


## 'make_datamod_comp' --------------------------------------------------------

## HAS_TESTS
#' Make Character Vector with Comp for Data Model
#'
#' For use in 'components()'
#' 
#' @param datamod Object of class 'bage_datamod'
#'
#' @returns A character vector
#'
#' @noRd
make_datamod_comp <- function(datamod) {
  UseMethod("make_datamod_comp")
}

## HAS_TESTS
#' @export
make_datamod_comp.bage_datamod_miscount <- function(datamod) {
  prob_mean <- datamod$prob_mean
  rate_mean <- datamod$rate_mean
  n_prob <- length(prob_mean)
  n_rate <- length(rate_mean)
  rep(c("prob", "rate"), times = c(n_prob, n_rate))
}

## HAS_TESTS
#' @export
make_datamod_comp.bage_datamod_overcount <- function(datamod) {
  rate_mean <- datamod$rate_mean
  n_rate <- length(rate_mean)
  rep.int("rate", times = n_rate)
}

## HAS_TESTS
#' @export
make_datamod_comp.bage_datamod_undercount <- function(datamod) {
  prob_mean <- datamod$prob_mean
  n_prob <- length(prob_mean)
  rep.int("prob", times = n_prob)
}


## 'make_datamod_consts' ------------------------------------------------------

#' Make Vector to Hold Constants for Data Model
#'
#' @param datamod Object of class 'bage_datamod'
#'
#' @returns Double vector
#'
#' @noRd
make_datamod_consts <- function(datamod) {
  UseMethod("make_datamod_consts")
}

## HAS_TESTS
#' @export
make_datamod_consts.bage_datamod_exposure <- function(datamod) {
  datamod$disp
}

## HAS_TESTS
#' @export
make_datamod_consts.bage_datamod_miscount <- function(datamod) {
  prob_mean <- datamod$prob_mean
  prob_disp <- datamod$prob_disp
  rate_mean <- datamod$rate_mean
  rate_disp <- datamod$rate_disp
  c(prob_mean,
    prob_disp,
    rate_mean,
    rate_disp)
}

## HAS_TESTS
#' @export
make_datamod_consts.bage_datamod_noise <- function(datamod) {
  sd <- datamod$sd_sd
  outcome_sd <- datamod$outcome_sd
  sd / outcome_sd
}

## HAS_TESTS
#' @export
make_datamod_consts.bage_datamod_overcount <- function(datamod) {
  rate_mean <- datamod$rate_mean
  rate_disp <- datamod$rate_disp
  c(rate_mean,
    rate_disp)
}

## HAS_TESTS
#' @export
make_datamod_consts.bage_datamod_undercount <- function(datamod) {
  prob_mean <- datamod$prob_mean
  prob_disp <- datamod$prob_disp
  c(prob_mean,
    prob_disp)
}


## 'make_level_datamod' -------------------------------------------------------

#' Make Level Vector for Data Model
#'
#' Only include levels for parameters
#' estimated in TMB
#'
#' @param datamod Object of class 'bage_datamod'
#'
#' @returns Double vector
#'
#' @noRd
make_level_datamod <- function(datamod) {
  UseMethod("make_level_datamod")
}

## HAS_TESTS
#' @export
make_level_datamod.bage_datamod_miscount <- function(datamod) {
  prob_levels <- datamod$prob_levels
  rate_levels <- datamod$rate_levels
  c(prob_levels, rate_levels)
}

## HAS_TESTS
#' @export
make_level_datamod.bage_datamod_overcount <- function(datamod) {
  datamod$rate_levels
}

## HAS_TESTS
#' @export
make_level_datamod.bage_datamod_undercount <- function(datamod) {
  datamod$prob_levels
}


## 'make_datamod_matrices' ----------------------------------------------------

#' Make List of Matrices for Data Model
#'
#' @param datamod Object of class 'bage_datamod'
#'
#' @returns Double vector
#'
#' @noRd
make_datamod_matrices <- function(datamod) {
  UseMethod("make_datamod_matrices")
}

## HAS_TESTS
#' @export
make_datamod_matrices.bage_datamod_exposure <- function(datamod) {
  disp <- datamod$disp_matrix_outcome
  list(disp)
}

## HAS_TESTS
#' @export
make_datamod_matrices.bage_datamod_miscount <- function(datamod) {
  prob <- datamod$prob_matrix_outcome
  rate <- datamod$rate_matrix_outcome
  list(prob, rate)
}

## HAS_TESTS
#' @export
make_datamod_matrices.bage_datamod_noise <- function(datamod) {
  sd <- datamod$sd_matrix_outcome
  list(sd)
}

## HAS_TESTS
#' @export
make_datamod_matrices.bage_datamod_overcount <- function(datamod) {
  rate <- datamod$rate_matrix_outcome
  list(rate)
}

## HAS_TESTS
#' @export
make_datamod_matrices.bage_datamod_undercount <- function(datamod) {
  prob <- datamod$prob_matrix_outcome
  list(prob)
}


## 'make_datamod_param' ----------------------------------------------------

#' Make Parameter Vector for Data Model
#'
#' @param datamod Object of class 'bage_datamod'
#'
#' @returns Double vector
#'
#' @noRd
make_datamod_param <- function(datamod) {
  UseMethod("make_datamod_param")
}

## HAS_TESTS
#' @export
make_datamod_param.bage_datamod_exposure <- function(datamod) {
  double()
}

## HAS_TESTS
#' @export
make_datamod_param.bage_datamod_miscount <- function(datamod) {
  prob_mean <- datamod$prob_mean
  rate_mean <- datamod$rate_mean
  c(poputils::logit(prob_mean),
    log(rate_mean))
}

## HAS_TESTS
#' @export
make_datamod_param.bage_datamod_noise <- function(datamod) {
  double()
}

## HAS_TESTS
#' @export
make_datamod_param.bage_datamod_overcount <- function(datamod) {
  rate_mean <- datamod$rate_mean
  log(rate_mean)
}

## HAS_TESTS
#' @export
make_datamod_param.bage_datamod_undercount <- function(datamod) {
  prob_mean <- datamod$prob_mean
  poputils::logit(prob_mean)
}


## 'make_i_lik_part' ----------------------------------------------------------

## HAS_TESTS
#' @export
make_i_lik_part.bage_datamod_exposure <- function(x) {
  1000L
}

## HAS_TESTS
#' @export
make_i_lik_part.bage_datamod_miscount <- function(x) {
  2000L
}

## HAS_TESTS
#' @export
make_i_lik_part.bage_datamod_noise <- function(x) {
  3000L
}

## HAS_TESTS
#' @export
make_i_lik_part.bage_datamod_overcount <- function(x) {
  4000L
}

## HAS_TESTS
#' @export
make_i_lik_part.bage_datamod_undercount <- function(x) {
  5000L
}



## Helper function for 'draw_outcome_true_given_obs' --------------------------

## HAS_TESTS
#' Draw Values for True Outcome, Given Observed Outcome,
#' Based on Binomial Model and Undercount Data Model
#'
#' Note that 'outcome' is an rvec if it has been inferred
#' from confidentialized values - otherwise it is
#' a numeric vector.
#'
#' @param outcome Observed values for outcome.
#' Can be rvec or numeric vector.
#' @param offset Observed values for offset.
#' Numeric vector.
#' @param expected Expected values for rates/prob/mean.
#' An rvec.
#' @param disp Dispersion. An rvec.
#' @param prob Probability event/person captured.
#' An rvec.
#'
#' @returns An rvec
#'
#' @noRd
draw_outcome_true_binom_betabinom <- function(outcome,
                                              offset,
                                              expected,
                                              disp,
                                              prob) {
  n_draw <- rvec::n_draw(expected)
  n_val <- length(expected)
  offset <- rep(offset, times = n_draw)
  if (rvec::is_rvec(outcome))
    outcome <- as.numeric(outcome)
  else
    outcome <- rep(outcome, times = n_draw)
  expected <- as.numeric(expected)
  disp <- as.numeric(disp)
  disp <- rep(disp, each = n_val)
  prob <- as.numeric(prob)
  ans <- sample_post_binom_betabinom(n = offset,
                                     y = outcome,
                                     mu = expected,
                                     xi = disp,
                                     pi = prob)
  ans <- matrix(ans, nrow = n_val, ncol = n_draw)
  ans <- rvec::rvec_dbl(ans)
  ans
}


## Helper functions for 'make_expected_obs' -----------------------------------

## HAS_TESTS
#' Make Modified Version of 'expected' to
#' Use with Exposure Data Model
#'
#' @param datamod Object of class "bage_datamod_overcount"
#' @param expected Rvec with expected value from system model
#'
#' @returns An rvec
#'
#' @noRd
make_expected_obs_exposure <- function(datamod,
                                       expected) {
  disp <- get_datamod_disp(datamod)
  numerator <- (3 * disp + 1) * expected
  denominator <- disp + 1
  numerator / denominator
}


## HAS_TESTS
#' Make Modified Version of 'expected' to
#' Use with Miscount Data Model
#'
#' @param datamod Object of class "bage_datamod_miscount"
#' @param components Data frame with estimates of 'prob' and 'rate'
#' @param expected Rvec with expected value from system model
#'
#' @returns An rvec
#'
#' @noRd
make_expected_obs_miscount <- function(datamod,
                                       components,
                                       expected) {
  prob <- get_datamod_prob(datamod = datamod,
                           components = components)
  rate <- get_datamod_rate(datamod = datamod,
                           components = components)
  (prob + rate) * expected
}


## HAS_TESTS
#' Make Modified Version of 'expected' to
#' Use with Overcount Data Model
#'
#' @param datamod Object of class "bage_datamod_overcount"
#' @param components Data frame with estimates of 'disp'
#' @param expected Rvec with expected value from system model
#'
#' @returns An rvec
#'
#' @noRd
make_expected_obs_overcount <- function(datamod,
                                        components,
                                        expected) {
  rate <- get_datamod_rate(datamod = datamod,
                           components = components)
  (1 + rate) * expected
}


## HAS_TESTS
#' Make Modified Version of 'expected' to
#' Use with Undercount Data Model
#'
#' @param datamod Object of class "bage_datamod_miscount"
#' @param components Data frame with estimates of 'prob'
#' @param expected Rvec with expected value from system model
#'
#' @returns An rvec
#'
#' @noRd
make_expected_obs_undercount <- function(datamod,
                                         components,
                                         expected) {
  prob <- get_datamod_prob(datamod = datamod,
                           components = components)
  prob * expected
}
