
## 'draw_outcome_obs' ----------------------------------------------------

#' Generate Confidentialized Values of Outcome Variable
#'
#' @param confidential Object of class 'bage_confidential'
#' @param outcome_true Rvec with true values for outcome.
#'
#' @returns An rvec
#'
#' @noRd
draw_outcome_obs <- function(confidential,
                             outcome_true) {
  UseMethod("draw_outcome_obs")
}

#' @export
draw_outcome_obs.bage_confidential_rr3 <- function(confidential,
                                                   outcome_true) {
  poputils::rr3(outcome_true)
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
