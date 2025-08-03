
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

#' @export
draw_outcome_confidential.bage_confidential_rr3 <- function(confidential,
                                                            outcome_obs) {
  poputils::rr3(outcome_obs)
} 


## 'draw_outcome_obs_given_conf' ----------------------------------------------

#' Draw Values for Observed Outcome, Given Values for
#' Confidentialized Outcome
#'
#' @param confidential Object of class 'bage_confidential'
#' @param outcome_obs Rvec with observed values for outcome
#' and 
#' (ie may be subject to measurement error)
#'
#' @returns An rvec
#'
#' @noRd
draw_outcome_confidential <- function(confidential,
                                      outcome_obs) {
  UseMethod("draw_outcome_confidential")
}

## NO_TESTS
#' @export
draw_outcome_true.bage_datamod_outcome_rr3 <- function(datamod,
                                                            nm_distn,
                                                            outcome_obs,
                                                            fitted,
                                                            expected,
                                                            disp,
                                                            offset) {
  n_val <- length(outcome_obs)
  has_disp <- !is.null(disp)
  if (has_disp) {
    n_draw <- rvec::n_draw(expected)
    expected <- as.matrix(expected)
    disp <- matrix(as.numeric(disp), nrow = n_val, ncol = n_draw, byrow = TRUE)
    offset <- matrix(offset, nrow = n_val, ncol = n_draw)
    if (nm_distn == "pois") {
      nm_dist_detailed <- "nbinom"
      shape <- 1 / disp
      rate <- 1 / (expected * offset * disp)
      args <- list(shape = shape, rate = rate)
    }
    else if (nm_distn == "binom") {
      nm <- "betabinom"
      shape <- 1 / disp
      rate <- 1 / (expected * offset * disp)
      args <- list(size = offset, shape = shape, rate = rate)
    }
    else
      cli::cli_abort("Internal error: Invalid value for {.var nm_distn}.")
  }
  else {
    n_draw <- rvec::n_draw(fitted)
    fitted <- as.matrix(fitted)
    offset <- matrix(offset, nrow = n_val, ncol = n_draw)
    if (nm_distn == "pois") {
      nm_dist_detailed <- "pois"
      lambda <- fitted * offset
      args <- list(lambda = lambda)
    }
    else if (nm_distn == "binom") {
      nm <- "binom"
      args <- list(size = offset, prob = fitted)
    }
    else
      cli::cli_abort("Internal error: Invalid value for {.var nm_distn}.")
  }
  args <- lapply(seq_along(n_val),
                 function(i) lapply(args, function(j) arg[i, ]))
  draw_vals_outcome_true_rr3(nm_distn_detailed = nm_dist_detailed,
                             outcome_obs = outcome_obs,
                             args = args,
                             offset = offset,
                             n_draw = n_draw)
}                            


## 'make_i_lik_part' --------------------------------------------------

#' Make Part of the 'i_lik' Index used by TMB
#'
#' Create when 'fit' is called, since index
#' can be changed after 'mod' object is
#' constructed.
#'
#' @param x Object of class 'bage_confidential',
#' 'bage_datamod_outcome', or 'bag_datamod_offset'
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
  1L
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
  "rr3()"
}
