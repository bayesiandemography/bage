
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
#' @param offset Exposure, size, or weights
#'
#' @returns An rvec
#'
#' @noRd
draw_vals_outcome_true <- function(datamod,
                                   nm_distn,
                                   outcome_obs,
                                   fitted,
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
    vals <- rpois_rvec_guarded(n = n_impute,
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
                                                            disp, ## ignored
                                                            offset) {
  n_draw <- rvec::n_draw(fitted)
  n_val <- length(fitted)
  s <- seq.int(from = -2L, to = 2L)
  fitted <- as.matrix(fitted)
  outcome_true <- matrix(outcome_obs + rep(s, each = n_val),
                         nrow = n_val,
                         ncol = 5L)
  prob_obs_given_true <- matrix(rep(c(1/3, 2/3, 1, 2/3, 1/3), each = n_val),
                                nrow = n_val,
                                ncol = 5L)
  prob_obs_given_true[outcome_true < 0L] <- 0
  ans <- matrix(NA_integer_, nrow = n_val, ncol = n_draw)
  for (i_draw in seq_len(n_draw)) {
    if (nm_distn == "pois")
      prob_true_given_fitted <- stats::dpois(outcome_true, lambda = fitted[, i_draw] * offset)
    else if (nm_distn == "binom")
      prob_true_given_fitted <- stats::dbinom(outcome_true, size = offset, prob = fitted[, i_draw])
    else
      cli::cli_abort("Internal error: Invalid value for {.var nm_distn}.")
    prob_true_given_fitted <- matrix(prob_true_given_fitted,
                                     nrow = n_val,
                                     ncol = 5L)
    prob_true <- prob_obs_given_true * prob_true_given_fitted
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
