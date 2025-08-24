## 'datamod_descr' ------------------------------------------------------------

#' Name of Data Model Used in Printing
#'
#' @param mod An object of class 'bage_datamod'
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
  ratio <- get_datamod_ratio(datamod)
  disp <- get_datamod_disp(datamod = datamod,
                           components = components)
  disp_inv <- 1 / disp
  shape <- 2 + disp_inv[is_ok]
  rate <- (1 + disp_inv[is_ok]) * ratio[is_ok] * offset_true[is_ok]
  ans_inv <- rvec::rgamma_rvec(n = n_ok,
                               shape = shape,
                               rate = rate)
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
    is_ok <- !is.na(offset_obs) & !is.na(outcome)
    n_ok <- sum(is_ok)
    ans <- rvec::new_rvec(length = n_offset, n_draw = n_draw)
    ratio <- get_datamod_ratio(datamod)
    disp <- get_datamod_disp(datamod = datamod,
                             components = components)
    shape <- 3 + 1 / disp[is_ok] + outcome[is_ok]
    rate <- (1 + 1 / disp[is_ok]) * (ratio[is_ok] / offset_obs[is_ok]) + expected[is_ok]
    ans[is_ok] <- rvec::rgamma_rvec(n = n_ok, shape = shape, rate = rate)
  }
  else {
    cli::cli_abort(paste("Internal error: {.var datamod} has class",
                         "{.cls {class(datamod)}} but {.var nm_distn}",
                         "is {.val {nm_distn}}."))
  }
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
  mean <- get_datamod_mean(datamod)
  sd <- get_datamod_sd(datamod)
  mean <- rep(mean, times = n_draw)
  sd <- rep(sd, times = n_draw)
  mean <- mean[is_ok]
  sd <- sd[is_ok]
  noise <- stats::rnorm(n = n_ok, mean = mean, sd = sd)
  ans[is_ok] <- outcome_true[is_ok] + noise
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
    mean_noise <- get_datamod_mean(datamod)
    sd_noise <- get_datamod_sd(datamod)
    prec_true <- offset[is_ok] / (disp^2)
    prec_noise <- 1 / (sd_noise[is_ok]^2)
    wt_true <- prec_true / (prec_true + prec_noise)
    mean_true <- (wt_true * expected[is_ok]
      + (1 - wt_true) * (outcome[is_ok] - mean_noise[is_ok]))
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
#' @param components Data frame with estimates of 'ratio' and 'disp'
#' @param expected Rvec with expected value from system model
#'
#' @returns An rvec
#'
#' @noRd
make_expected_obs_exposure <- function(datamod,
                                       components,
                                       expected) {
  ratio <- get_datamod_ratio(datamod)
  disp <- get_datamod_disp(datamod = datamod,
                           components = components)
  numerator <- (3 * disp + 1) * expected
  denominator <- (disp + 1) * ratio
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
