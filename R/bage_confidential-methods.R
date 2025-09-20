
## 'draw_outcome_confidential' ------------------------------------------------

#' Generate Confidentialized Values of Observed Outcome Variable
#'
#' Generate confidentialized values from observed values.
#' The observed values may be subject to measurement error.
#'
#' @param confidential Object of class 'bage_confidential'
#' @param outcome_obs Rvec with observed values for outcome
#'
#' @returns An rvec
#'
#' @noRd
draw_outcome_confidential <- function(confidential,
                                      outcome_obs) {
  UseMethod("draw_outcome_confidential")
}

## HAS_TESTS
#' @export
draw_outcome_confidential.bage_confidential_rr3 <- function(confidential,
                                                            outcome_obs) {
  poputils::rr3(outcome_obs)
} 


## 'draw_outcome_obs_given_conf' ----------------------------------------------

#' Draw Values for Observed Outcome, Given Values for
#' Confidentialized Outcome
#'
#' If 'disp_obs' is not NULL, it has same length as 'expected_obs'.
#'
#' @param confidential Object of class 'bage_confidential'
#' @param nm_distn Name of distribution of outcome.
#' "pois", "binom", or "norm"
#' @param outcome_conf Numeric vector with confidentialised
#' values for outcome.
#' @param offset Numeric vector with (reported) outcome
#' @param expected_obs Rvec with expected value for
#' rate/probability/mean for reported value,
#' including adjustment, where necessary,
#' for measurement error
#' @param disp_obs Dispersion adjusted, where necessary,
#' for measurement error. NULL or rvec.
#' @param sd_obs Standard deviations from data model.
#' Currently only used with noise data model
#' for Poisson (with Skellam distribution) 
#'
#' @returns An rvec
#'
#' @noRd
draw_outcome_obs_given_conf <- function(confidential,
                                        nm_distn,
                                        outcome_conf,
                                        offset,
                                        expected_obs,
                                        disp_obs,
                                        sd_obs) {
  UseMethod("draw_outcome_obs_given_conf")
}

## HAS_TESTS
#' @export
draw_outcome_obs_given_conf.bage_confidential_rr3 <- function(confidential,
                                                              nm_distn,
                                                              outcome_conf,
                                                              offset,
                                                              expected_obs,
                                                              disp_obs,
                                                              sd_obs) {
  ## all variables are organized into
  ## (implicit) arrays with dimension
  ## (n_outcome, n_val, n_draw)
  n_val <- length(expected_obs)
  n_draw <- rvec::n_draw(expected_obs)
  n_outcome <- 5L
  expected_obs <- as.numeric(expected_obs)
  expected_obs <- rep(expected_obs, each = n_outcome)
  s_outcome <- seq.int(from = -2L, to = 2L)
  outcome_true <- outer(s_outcome, outcome_conf, FUN = "+")
  outcome_true <- rep(outcome_true, times = n_draw)
  offset <- rep(offset, each = n_outcome)
  offset <- rep(offset, times = n_draw)
  has_disp_obs <- !is.null(disp_obs)
  has_sd_obs <- !is.null(sd_obs)
  if (has_disp_obs) {
    disp_obs <- as.numeric(disp_obs)
    disp_obs <- rep(disp_obs, each = n_outcome)
    if (nm_distn == "pois") {
      size <- 1 / disp_obs
      prob <- 1 / (1 + expected_obs * offset * disp_obs)
      prob_prior <- stats::dnbinom(x = outcome_true,
                                   size = size,
                                   prob = prob)
    }
    else if (nm_distn == "binom") {
      shape1 <- expected_obs / disp_obs
      shape2 <- (1 - expected_obs) / disp_obs
      prob_prior <- dbetabinom(x = outcome_true,
                               size = offset,
                               shape1 = shape1,
                               shape2 = shape2)
    }
    else
      cli::cli_abort("Internal error: Invalid value for {.var nm_distn}.")
  }
  else {
    if (nm_distn == "pois") {
      lambda <- expected_obs * offset
      if (has_sd_obs) { ## noise data model, Skellam distribution
        sd_obs <- rep(sd_obs, each = n_outcome)
        sd_obs <- rep(sd_obs, times = n_draw)
        mu1 <- lambda + 0.5 * sd_obs^2
        mu2 <- 0.5 * sd_obs^2
        prob_prior <- dskellam(x = outcome_true,
                               mu1 = mu1,
                               mu2 = mu2)
      }
      else {
        prob_prior <- stats::dpois(x = outcome_true,
                                   lambda = lambda)
      }
    }
    else if (nm_distn == "binom") {
      prob_prior <- stats::dbinom(x = outcome_true,
                                  size = offset,
                                  prob = expected_obs)
    }
    else
      cli::cli_abort("Internal error: Invalid value for {.var nm_distn}.")
  }
  prob_prior[outcome_true < 0L] <- 0
  prob_obs_given_true <- c(1/3, 2/3, 1, 2/3, 1/3)
  prob_true <- prob_obs_given_true * prob_prior
  prob_true <- matrix(prob_true,
                      nrow = n_outcome,
                      ncol = n_val * n_draw)
  outcome_true <- matrix(outcome_true,
                         nrow = n_outcome,
                         ncol = n_val * n_draw)
  select_i <- function(prob) {
    if (anyNA(prob))
      NA
    else
      sample.int(n = n_outcome, size = 1L, prob = prob)
  }
  i <- apply(prob_true, MARGIN = 2L, FUN = select_i)
  j <- seq_len(n_val * n_draw)
  ans <- outcome_true[cbind(i, j)]
  ans <- matrix(ans, nrow = n_val, ncol = n_draw)
  ans <- rvec::rvec(ans)
}


## 'make_i_lik_part' --------------------------------------------------

#' Make Part of the 'i_lik' Index used by TMB
#'
#' Create when 'fit' is called, since index
#' can be changed after 'mod' object is
#' constructed.
#'
#' @param x Object of class 'bage_confidential',
#' or 'bage_datamod'
#'
#' @returns An integer scalar
#'
#' @noRd
make_i_lik_part <- function(x) {
  UseMethod("make_i_lik_part")
}

## HAS_TESTS
#' @export
make_i_lik_part.bage_confidential_rr3 <- function(x) {
  10L
}


## 'str_call_confidential' ----------------------------------------------------

#' Create String Describing Confidentialization Process
#'
#' @param confidential An object of class "bage_confidential"
#'
#' @returns A string
#'
#' @noRd
str_call_confidential <- function(confidential) {
  UseMethod("str_call_confidential")
}

## HAS_TESTS
#' @export
str_call_confidential.bage_confidential_rr3 <- function(confidential) {
  "rr3"
}
