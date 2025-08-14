
## 'draw_offset_true_given_obs' ----------------------------------------------

#' Draw Values for True Offset, Given Observed Offset,
#' Based on Data Model
#'
#'
#' @param datamod Object of class 'bage_datamod'
#' @param nm_distn Name of distribution of offset.
#' Currently only "pois" allowed.
#' @param components Data frame with components.
#' @param offset Observed values for offset.
#' Numeric vector.
#' @param expected Expected values for rates.
#' An rvec.
#'
#' @returns An rvec
#'
#' @noRd
draw_offset_true_given_obs <- function(datamod,
                                       nm_distn,
                                       components,
                                       offset,
                                       expected) {
  UseMethod("draw_offset_true_given_obs")
}


draw_offset_true_given_obs.bage_datamod_exposure <- function(datamod,
                                                             nm_distn,
                                                             components,
                                                             offset,
                                                             expected) {
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


#' @export
draw_outcome_true_given_obs.bage_datamod_miscount <- function(datamod,
                                                              nm_distn,
                                                              components,
                                                              outcome,
                                                              offset,
                                                              expected,
                                                              disp) {
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
    size_unobs <- true_obs + (1 / disp)
    prob_unobs <- ((1 + prob * expected * offset * disp)
      / (1 + expected * offset * disp))
    outcome_true_unobs <- rvec::nbinom_rvec(n = n_outcome,
                                            size = size_unobs,
                                            prob = prob_unobs)
    ans <- outcome_true_obs + outcome_true_unobs
  }
  else {
    cli::cli_abort(paste("Internal error: {.var datamod} has class",
                         "{.cls {class(datamod)}} but {.var nm_distn}",
                         "is {.val {nm_distn}}."))
  }
  ans
}

#' Expecting offset, expected, and disp to be on original scale
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
    prec_noise <- 1 / (sd^2)
    wt_true <- prec_true / (prec_true + prec_noise)
    mean_true <- wt_true * expected + (1 - wt_true) * (outcome - mean_noise)
    sd_true <- 1 / (prec_true + prec_noise)
    ans <- rvec::rnorm_rvec(n = n_outcome, mean = mean_true, sd = sd_true)
  }
  else {
    cli::cli_abort(paste("Internal error: {.var datamod} has class",
                         "{.cls {class(datamod)}} but {.var nm_distn}",
                         "is {.val {nm_distn}}."))
  }
  ans
}


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
    prob_obs <- rate / (1 + rate)
    ans <- rvec::nbinom_rvec(n = n_outcome,
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

#' @export
draw_outcome_true_given_obs.bage_datamod_undercount <- function(datamod,
                                                                nm_distn,
                                                                components,
                                                                outcome,
                                                                offset,
                                                                expected,
                                                                disp) {
  if (nm_distn == "pois") {
    n_outcome <- length(outcome)
    prob <- get_datamod_prob(datamod = datamod,
                             components = components)
    size_unobs <- observed + (1 / disp)
    prob_unobs <- ((1 + prob * expected * offset * disp)
      / (1 + expected * offset * disp))
    outcome_unobs <- rvec::nbinom_rvec(n = n_outcome,
                                       size = size_unobs,
                                       prob = prob_unobs)
    ans <- outcome + outcome_unobs
  }
  else if (nm_distn == "binom") {
    stop("not implemented yet")
    ## sample_X_given_Y <- function(n, y, alpha, beta, pi) {
    ##   ks <- y:n
    ##   # log-weights
    ##   log_w <- sapply(ks, function(k) {
    ##     lchoose(n, k) + lchoose(k, y) +
    ##       lbeta(k + alpha, n - k + beta) + (k - y) * log(1 - pi)
    ##   })
    ##   log_w <- log_w - max(log_w)
    ##   w <- exp(log_w)
    ##   probs <- w / sum(w)
    ##   sample(ks, size = 1, prob = probs)
    ## }
  }
  else {
    cli::cli_abort(paste("Internal error: {.var datamod} has class",
                         "{.cls {class(datamod)}} but {.var nm_distn}",
                         "is {.val {nm_distn}}."))
  }
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


## OLD ########################################################################


## 'draw_vals_outcome_obs' ----------------------------------------------------

#' Generate Observed Values of Outcome Variable, Based on Data Model
#'
#' @param datamod Object of class 'bage_datamod'
#' @param outcome_true Numeric vector with true values for outcome.
#'
#' @returns An rvec
#'
#' @noRd
draw_vals_outcome_obs <- function(datamod, outcome_true) {
  UseMethod("draw_vals_outcome_obs")
}

#' @export
draw_vals_outcome_obs.bage_datamod_outcome_rr3 <- function(datamod, outcome_true) {
  poputils::rr3(outcome_true)
} 


## 'draw_vals_outcome_true' ---------------------------------------------------

#' Estimate True Values of Outcome Variable, Based on Data Model
#'
#' @param datamod Object of class 'bage_datamod_outcome'
#' @param nm_distn Name of distribution of outcome variable
#' "pois", "binom", or "norm"
#' @param outcome_obs Numeric vector with values for outcome.
#' Can include NAs.
#' @param fitted Rvec with fitted values for rate, probability or mean
#' @param expected Rvec with expected values for rate or probability. Or NULL.
#' @param disp Rvec with dispersion. Or NULL.
#' @param offset Exposure, size, or weights
#'
#' @returns An rvec
#'
#' @noRd
draw_vals_outcome_true <- function(datamod,
                                   nm_distn,
                                   outcome_obs,
                                   fitted,
                                   expected,
                                   disp,
                                   offset) {
  UseMethod("draw_vals_outcome_true")
}



## 'draw_vals_outcome_true' ---------------------------------------------------

## HAS_TESTS
#' @export
draw_vals_outcome_true.NULL <- function(datamod,
                                        nm_distn,
                                        outcome_obs,
                                        fitted,
                                        expected,
                                        disp,
                                        offset) {
  n_draw <- rvec::n_draw(fitted)
  n_val <- length(fitted)
  is_known <- !is.na(outcome_obs)
  is_impute <- !is_known & !is.na(offset)
  n_impute <- sum(is_impute)
  fitted_impute <- fitted[is_impute]
  offset_impute <- offset[is_impute]
  ans <- matrix(as.double(outcome_obs), nrow = n_val, ncol = n_draw)
  ans <- rvec::rvec(ans)
  if (nm_distn == "pois")
    vals <- rpois_guarded(n = n_impute,
                               lambda = fitted_impute * offset_impute)
  else if (nm_distn == "binom")
    vals <- rvec::rbinom_rvec(n = n_impute,
                              size = offset_impute,
                              prob = fitted_impute)
  else if (nm_distn == "norm")
    vals <- rvec::rnorm_rvec(n = n_impute,
                             mean = fitted_impute,
                             sd = disp / sqrt(offset_impute))
  else
    cli::cli_abort("Internal error: Invalid value for {.var nm_distn}.")
  ans[is_impute] <- vals
  ans
}                            





## HAS_TESTS
#' @export
draw_vals_outcome_true.bage_datamod_outcome_rr3 <- function(datamod,
                                                            nm_distn,
                                                            outcome_obs,
                                                            fitted,
                                                            expected,
                                                            disp,
                                                            offset) {
  n_draw <- rvec::n_draw(fitted)
  n_val <- length(fitted)
  has_disp <- !is.null(disp)
  if (has_disp) {
    expected <- as.matrix(expected)
    disp <- as.numeric(disp)
  }
  else
    fitted <- as.matrix(fitted)
  s <- seq.int(from = -2L, to = 2L)
  outcome_true <- matrix(outcome_obs + rep(s, each = n_val),
                         nrow = n_val,
                         ncol = 5L)
  prob_obs_given_true <- matrix(rep(c(1/3, 2/3, 1, 2/3, 1/3), each = n_val),
                                nrow = n_val,
                                ncol = 5L)
  prob_obs_given_true[outcome_true < 0L] <- 0
  ans <- matrix(NA_integer_, nrow = n_val, ncol = n_draw)
  for (i_draw in seq_len(n_draw)) {
    if ((nm_distn == "pois") && has_disp) {
      shape <- 1 / disp[[i_draw]]
      rate <- 1 / (expected[, i_draw] * offset * disp[[i_draw]])
      prob_true_prior <- stats::dnbinom(outcome_true,
                                        shape = shape,
                                        rate = rate)
    }
    else if (nm_dist == "pois" && !has_disp) {
      lambda <- fitted[, i_draw] * offset
      prob_true_prior <- stats::dpois(outcome_true,
                                      lambda = lambda)
    }
    else if (nm_dist == "binom" && has_disp) {
      shape1 <- expected[, i_draw] / disp[[i_draw]]
      shape2 <- (1 - expected[, i_draw]) / disp[[i_draw]]
      prob_true_prior <- dbetabinom(outcome_true,
                                    size = offset,
                                    shape1 = shape1,
                                    shape2 = shape1)
    }
    else if (nm_distn == "binom" && !has_disp) {
      prob <- fitted[, i_draw]
      prob_true_prior <- stats::dbinom(outcome_true,
                                       size = offset,
                                       prob = prob)
    }
    else
      cli::cli_abort("Internal error: Invalid value for {.var nm_distn}.")
    prob_true_prior <- matrix(prob_true_prior,
                              nrow = n_val,
                              ncol = 5L)
    prob_true <- prob_obs_given_true * prob_true_prior
    for (i_val in seq_len(n_val)) {
      outcome_val <- outcome_obs[[i_val]]
      offset_val <- offset[[i_val]]
      has_outcome <- !is.na(outcome_val)
      has_offset <- !is.na(offset_val)
      if (has_outcome && has_offset) {
        val <- sample(x = outcome_true[i_val, ],
                      size = 1L,
                      prob = prob_true[i_val, ])
      }
      else if (!has_outcome && has_offset) {
        fitted_val <- fitted[i_val, i_draw]
        if (nm_distn == "pois")
          val <- stats::rpois(n = 1L, lambda = fitted_val * offset_val)
        else
          val <- stats::rbinom(n = 1L, size = offset_val, prob = fitted_val)
      }
      else {
        val <- NA_integer_
      }
      ans[i_val, i_draw] <- val
    }
  }
  ans <- rvec::rvec(ans)
  ans
}                            


## 'make_i_lik' ---------------------------------------------------------------

#' Make 'i_lik' Index used by TMB
#'
#' Create when 'fit' is called, since index
#' can be changed after 'mod' object is
#' constructed.
#'
#' @param mod Object of class 'bage_datamod' 
#' @param nm_distn Name of distribution: "pois", "binom", or "norm"
#' @param has_disp Whether the model uses dispersion
#'
#' @returns An integer scalar
#'
#' @noRd
make_i_lik <- function(mod, nm_distn, has_disp) {
  UseMethod("make_i_lik")
}

## HAS_TESTS
#' @export
make_i_lik.bage_datamod_outcome_rr3 <- function(mod, nm_distn, has_disp) {
  if ((nm_distn == "binom") && has_disp)
    104L
  else if ((nm_distn == "binom") && !has_disp)
    102L
  else if ((nm_distn == "pois") && has_disp)
    304L
  else if ((nm_distn == "pois") && !has_disp)
    302L
  else
    cli::cli_abort("Internal error: Invalid inputs.")
}


## 'str_call_datamod' ---------------------------------------------------------

#' Create String Describing Data Model
#'
#' @param prior An object of class "bage_datamod"
#'
#' @returns A string
#'
#' @noRd
str_call_datamod <- function(datamod) {
  UseMethod("str_call_datamod")
}

## HAS_TESTS
#' @export
str_call_datamod.bage_datamod_outcome_rr3 <- function(datamod) {
  "rr3()"
}
