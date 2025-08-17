
## 'draw_offset_true_given_obs' ----------------------------------------------

#' Draw Values for True Offset, Given Observed Offset,
#' Based on Data Model
#'
#'
#' @param datamod Object of class 'bage_datamod'
#' @param nm_distn Name of distribution of offset.
#' @param components Data frame with components.
#' @param offset Observed values for offset.
#' Numeric vector.
#'
#' @returns An rvec
#'
#' @noRd
draw_offset_true_given_obs <- function(datamod,
                                       nm_distn,
                                       components,
                                       offset) {
  UseMethod("draw_offset_true_given_obs")
}

## HAS_TESTS
#' @export
draw_offset_true_given_obs.bage_datamod_exposure <- function(datamod,
                                                             nm_distn,
                                                             components,
                                                             offset) {
  if (nm_distn == "pois") {
    n_offset <- length(offset)
    ratio <- get_datamod_ratio(datamod)
    disp <- get_datamod_disp(datamod = datamod,
                             components = components)
    shape <- 3 + 1 / disp
    rate <- (1 + 1 / disp) * (ratio / offset)
    ans <- rvec::rgamma_rvec(n = n_offset, shape = shape, rate = rate)
  }
  else {
    cli::cli_abort(paste("Internal error: {.var datamod} has class",
                         "{.cls {class(datamod)}} but {.var nm_distn}",
                         "is {.val {nm_distn}}."))
  }
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
#' An rvec.
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
  if (nm_distn == "pois") {
    n_outcome <- length(outcome)
    prob <- get_datamod_prob(datamod = datamod,
                             components = components)
    rate <- get_datamod_rate(datamod = datamod,
                             components = components)
    prob_obs <- prob / (prob + rate)
    outcome_true_obs <- rvec::rbinom_rvec(n = n_outcome,
                                          size = outcome,
                                          prob = prob_obs)
    if (has_disp) {
      size_unobs <- outcome_true_obs + (1 / disp)
      prob_unobs <- ((1 + prob * expected * offset * disp)
        / (1 + expected * offset * disp))
      outcome_true_unobs <- rvec::rnbinom_rvec(n = n_outcome,
                                               size = size_unobs,
                                               prob = prob_unobs)
    }
    else {
      lambda <- (1 - prob) * expected * offset
      outcome_true_unobs <- rvec::rpois_rvec(n = n_outcome,
                                             lambda = lambda)
    }
    ans <- outcome_true_obs + outcome_true_unobs
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
    mean_noise <- get_datamod_mean(datamod)
    sd_noise <- get_datamod_sd(datamod)
    prec_true <- offset / (disp^2)
    prec_noise <- 1 / (sd_noise^2)
    wt_true <- prec_true / (prec_true + prec_noise)
    mean_true <- wt_true * expected + (1 - wt_true) * (outcome - mean_noise)
    sd_true <- 1 / sqrt(prec_true + prec_noise)
    ans <- rvec::rnorm_rvec(n = n_outcome, mean = mean_true, sd = sd_true)
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
    rate <- get_datamod_rate(datamod = datamod,
                             components = components)
    prob_obs <- 1 / (1 + rate)
    ans <- rvec::rbinom_rvec(n = n_outcome,
                             size = outcome,
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
  prob <- get_datamod_prob(datamod = datamod,
                           components = components)
  if (nm_distn == "pois") {
    if (has_disp) {
      size_unobs <- outcome + (1 / disp)
      prob_unobs <- ((1 + prob * expected * offset * disp)
        / (1 + expected * offset * disp))
      outcome_unobs <- rvec::rnbinom_rvec(n = n_outcome,
                                          size = size_unobs,
                                          prob = prob_unobs)
    }
    else {
      lambda <- (1 - prob) * expected * offset
      outcome_unobs <- rvec::rpois_rvec(n = n_outcome,
                                        lambda = lambda)
    }
    ans <- outcome + outcome_unobs
  }
  else if (nm_distn == "binom") {
    if (has_disp) {
      ## more complex distribution, so gets special function
      ans <- draw_outcome_true_binom_betabinom(offset = offset,
                                               outcome = outcome,
                                               expected = expected,
                                               disp = disp,
                                               prob = prob)
    }
    else {
      size_unobs <- offset - outcome
      prob_unobs <- (1 - prob) * expected / (1 - prob * expected)
      outcome_unobs <- rvec::rbinom_rvec(n = n_outcome,
                                         size = size_unobs,
                                         prob = prob_unobs)
      ans <- outcome + outcome_unobs
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

#' @export
make_i_lik_part.bage_datamod_exposure <- function(x) {
  1000L
}

#' @export
make_i_lik_part.bage_datamod_miscount <- function(x) {
  2000L
}

#' @export
make_i_lik_part.bage_datamod_noise <- function(x) {
  3000L
}

#' @export
make_i_lik_part.bage_datamod_overcount <- function(x) {
  4000L
}

#' @export
make_i_lik_part.bage_datamod_undercount <- function(x) {
  5000L
}
