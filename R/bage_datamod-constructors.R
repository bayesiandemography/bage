
## HAS_TESTS
#' Create New Data Model for Exposures
#'
#' @param disp Dispersion. Numeric vector
#' @param disp_levels Levels of 'by' variables for
#' which there are potentially distinct values of 'disp'.
#' A character vector.
#' @param disp_matrix_outcome Sparse matrix mapping
#' disp to outcome
#' @param cv_arg The original 'cv' argument
#' passed by the user (converted to a
#' data frame if original a number.)
#' @param nms_by Names of by variables for disp
#'
#' @returns Object of class 'bage_datamod_exposure'
#'
#' @noRd
new_bage_datamod_exposure <- function(disp,
                                      disp_levels,
                                      disp_matrix_outcome,
                                      cv_arg,
                                      nms_by) {
  ans <- list(disp = disp,
              disp_levels = disp_levels,
              disp_matrix_outcome = disp_matrix_outcome,
              cv_arg = cv_arg,
              nms_by = nms_by)
  class(ans) <- c("bage_datamod_exposure",
                  "bage_datamod_offset",
                  "bage_datamod")
  ans
}


## HAS_TESTS
#' Create New Miscount Data Model for Outcomes
#'
#' @param prob_mean,prob_disp Parameters
#' for beta prior for 'prob'. Numeric vectors
#' @param prob_levels Levels of 'by' variables for
#' which there are potentially distinct values
#' of 'prob_mean' and 'prob_disp'.
#' A character vector.
#' @param prob_matrix_outcome Sparse matrix mapping
#' prob to outcome
#' @param prob_arg The original
#' 'prob'  argument
#' passed by the user.
#' @param rate_mean,rate_disp Parameters for
#' gamma prior for 'rate'. Numeric vector.
#' @param rate_levels Levels of 'by' variables for
#' which there are potentially distinct values
#' of 'rate_mean' and 'rate_disp'.
#' A character vector.
#' @param rate_matrix_outcome Sparse matrix mapping
#' rate to outcome
#' @param rate_arg The original
#' 'rate'  argument
#' passed by the user.
#' @param nms_by Names of by variables for prob, rate
#' 
#' @returns Object of class 'bage_datamod_miscount'
#'
#' @noRd
new_bage_datamod_miscount <- function(prob_mean,
                                      prob_disp,
                                      prob_levels,
                                      prob_matrix_outcome,
                                      prob_arg,
                                      rate_mean,
                                      rate_disp,
                                      rate_levels,
                                      rate_matrix_outcome,
                                      rate_arg,
                                      nms_by) {
  ans <- list(prob_mean = prob_mean,
              prob_disp = prob_disp,
              prob_levels = prob_levels,
              prob_matrix_outcome = prob_matrix_outcome,
              prob_arg = prob_arg,
              rate_mean = rate_mean,
              rate_disp = rate_disp,
              rate_levels = rate_levels,
              rate_matrix_outcome = rate_matrix_outcome,
              rate_arg = rate_arg,
              nms_by = nms_by)
  class(ans) <- c("bage_datamod_miscount",
                  "bage_datamod_outcome",
                  "bage_datamod")
  ans
}


## HAS_TESTS
#' Create New Random Error Data Model for Outcomes
#'
#' @param sd_sd Standard devation of errors Numeric vector
#' @param sd_levels Levels of 'by' variables for
#' which there are potentially distinct values
#' of 'sd_sd'. A character vector.
#' @param sd_matrix_outcome Sparse matrix mapping
#' sd to outcome
#' @param sd_arg The original 'sd' argument
#' passed by the user (converted to a
#' data frame if original a number.)
#' @param nms_by Names of by variables for sd
#' @param outcome_sd Standard deviation of
#' the original, unscaled outcome
#' 
#' @returns Object of class 'bage_datamod_noise'
#'
#' @noRd
new_bage_datamod_noise <- function(sd_sd,
                                   sd_levels,
                                   sd_matrix_outcome,
                                   sd_arg,
                                   nms_by,
                                   outcome_sd) {
  ans <- list(sd_sd = sd_sd,
              sd_levels = sd_levels,
              sd_matrix_outcome = sd_matrix_outcome,
              sd_arg = sd_arg,
              nms_by = nms_by,
              outcome_sd = outcome_sd)
  class(ans) <- c("bage_datamod_noise",
                  "bage_datamod_outcome",
                  "bage_datamod")
  ans
}


## HAS_TESTS
#' Create New Overcount Data Model for Outcomes
#'
#' @param rate_mean,rate_disp Parameters for
#' gamma prior for 'rate'. Numeric vectors
#' @param rate_levels Levels of 'by' variables for
#' which there are potentially distinct values
#' of 'rate_mean', and 'rate_disp'. A character vector.
#' @param rate_matrix_outcome Sparse matrix mapping
#' rate to outcome
#' @param rate_arg The original 'rate' argument
#' passed by the user.
#' @param nms_by Names of by variables for rate
#' 
#' @returns Object of class 'bage_datamod_over'
#'
#' @noRd
new_bage_datamod_overcount <- function(rate_mean,
                                       rate_disp,
                                       rate_levels,
                                       rate_matrix_outcome,
                                       rate_arg,
                                       nms_by) {
  ans <- list(rate_mean = rate_mean,
              rate_disp = rate_disp,
              rate_levels = rate_levels,
              rate_matrix_outcome = rate_matrix_outcome,
              rate_arg = rate_arg,
              nms_by = nms_by)
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
#' @param prob_levels Levels of 'by' variables for
#' which there are potentially distinct values
#' of 'prob_mean', and 'prob_disp'. A character vector.
#' @param prob_matrix_outcome Sparse matrix mapping
#' prob to outcome
#' @param prob_arg The original 'prob' argument
#' passed by the user.
#' @param nms_by Names of by variables for prob
#' 
#' @returns Object of class 'bage_datamod_undercount'
#'
#' @noRd
new_bage_datamod_undercount <- function(prob_mean,
                                        prob_disp,
                                        prob_levels,
                                        prob_matrix_outcome,
                                        prob_arg,
                                        nms_by) {
  ans <- list(prob_mean = prob_mean,
              prob_disp = prob_disp,
              prob_levels = prob_levels,
              prob_matrix_outcome = prob_matrix_outcome,
              prob_arg = prob_arg,
              nms_by = nms_by)
  class(ans) <- c("bage_datamod_undercount",
                  "bage_datamod_outcome",
                  "bage_datamod")
  ans
}

