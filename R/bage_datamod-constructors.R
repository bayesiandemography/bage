
## HAS_TESTS
#' Create New Data Model for Exposures
#'
#' @param ratio Expected ratio of reported exposure
#' to true exposure
#' @param disp_mean Mean for exponential
#' prior for dispersion
#' @param matrix_ratio_outcome Sparse matrix mapping
#' ratio to outcome
#' @param matrix_disp_outcome Sparse matrix mapping
#' disp_mean to outcome
#'
#' @returns Object of class 'bage_datamod_exposure'
#'
#' @noRd
new_bage_datamod_exposure <- function(ratio,
                                      disp_mean,
                                      matrix_ratio_outcome,
                                      matrix_disp_outcome) {
  ans <- list(ratio = ratio,
              disp_mean = disp_mean,
              matrix_ratio_outcome = matrix_ratio_outcome,
              matrix_disp_outcome = matrix_disp_outcome)
  class(ans) <- c("bage_datamod_exposure",
                  "bage_datamod_offset",
                  "bage_datamod")
  ans
}


## HAS_TESTS
#' Create New Miscount Data Model for Outcomes
#'
#' @param prob_mean, prob_disp Parameters
#' for beta prior for 'prob'
#' @param rate_mean,rate_disp Parameters for
#' gamma prior for 'rate'
#' @param matrix_prob_outcome Sparse matrix mapping
#' prob to outcome
#' @param matrix_rate_outcome Sparse matrix mapping
#' rate to outcome
#' 
#' @returns Object of class 'bage_datamod_miscount'
#'
#' @noRd
new_bage_datamod_miscount <- function(prob_mean,
                                      prob_disp,
                                      rate_mean,
                                      rate_disp,
                                      matrix_prob_outcome,
                                      matrix_rate_outcome) {
  ans <- list(prob_mean = prob_mean,
              prob_mean = prob_mean,
              rate_mean = rate_mean,
              rate_disp = rate_disp,
              matrix_prob_outcome = matrix_prob_outcome,
              matrix_rate_outcome = matrix_rate_outcome)
  class(ans) <- c("bage_datamod_miscount",
                  "bage_datamod_outcome",
                  "bage_datamod")
  ans
}


## HAS_TESTS
#' Create New Random Error Data Model for Outcomes
#'
#' @param mean Mean of errors
#' @param sd Standard devation of errors
#' @param matrix_mean_outcome Sparse matrix mapping
#' mean to outcome
#' @param matrix_sd_outcome Sparse matrix mapping
#' sd to outcome
#' 
#' @returns Object of class 'bage_datamod_error'
#'
#' @noRd
new_bage_datamod_noise <- function(mean,
                                   sd,
                                   matrix_mean_outcome,
                                   matrix_sd_outcome) {
  ans <- list(mean = mean,
              sd = sd,
              matrix_mean_outcome = matrix_mean_outcome,
              matrix_sd_outcome = matrix_sd_outcome)
  class(ans) <- c("bage_datamod_noise",
                  "bage_datamod_outcome",
                  "bage_datamod")
  ans
}


## HAS_TESTS
#' Create New Overcount Data Model for Outcomes
#'
#' @param rate_mean,rate_disp Parameters for
#' gamma prior for 'rate'
#' @param matrix_rate_outcome Sparse matrix mapping
#' rate to outcome
#' 
#' @returns Object of class 'bage_datamod_over'
#'
#' @noRd
new_bage_datamod_overcount <- function(rate_mean,
                                       rate_disp,
                                       matrix_rate_outcome) {
  ans <- list(rate_mean = rate_mean,
              rate_disp = rate_disp,
              matrix_rate_outcome = matrix_rate_outcome)
  class(ans) <- c("bage_datamod_overcount",
                  "bage_datamod_outcome",
                  "bage_datamod")
  ans
}


## HAS_TESTS
#' Create New Undercount Data Model for Outcomes
#'
#' @param prob_mean, prob_disp Parameters
#' for beta prior for 'prob'
#' @param matrix_prob_outcome Sparse matrix mapping
#' prob to outcome
#' 
#' @returns Object of class 'bage_datamod_undercount'
#'
#' @noRd
new_bage_datamod_undercount <- function(prob_mean,
                                        prob_disp,
                                        matrix_prob_outcome) {
  ans <- list(prob_mean = prob_mean,
              prob_disp = prob_disp,
              matrix_prob_outcome = matrix_prob_outcome)
  class(ans) <- c("bage_datamod_undercount",
                  "bage_datamod_outcome",
                  "bage_datamod")
  ans
}

