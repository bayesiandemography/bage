
## HAS_TESTS
#' Create New Data Model for Exposures
#'
#' @param ratio_ratio Expected ratio of reported exposure
#' to true exposure. Numeric vector.
#' @param ratio_levels Levels of 'by' variables for
#' which there are potentially distinct values of 'ratio_ratio'.
#' A character vector.
#' @param ratio_matrix_outcome Sparse matrix mapping
#' ratio to outcome
#' @param disp_mean Mean for exponential
#' prior for dispersion. Numeric vector
#' @param disp_levels Levels of 'by' variables for
#' which there are potentially distinct values of 'disp_mean'.
#' A character vector.
#' @param disp_matrix_outcome Sparse matrix mapping
#' disp_mean to outcome
#' @param nms_by Names of by variables for ratio, disp
#'
#' @returns Object of class 'bage_datamod_exposure'
#'
#' @noRd
new_bage_datamod_exposure <- function(ratio_ratio,
                                      ratio_levels,
                                      ratio_matrix_outcome,
                                      disp_mean,
                                      disp_levels,
                                      disp_matrix_outcome,
                                      nms_by) {
  ans <- list(ratio_ratio = ratio_ratio,
              ratio_levels = ratio_levels,
              ratio_matrix_outcome = ratio_matrix_outcome,
              disp_mean = disp_mean,
              disp_levels = disp_levels,
              disp_matrix_outcome = disp_matrix_outcome,
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
#' @param rate_mean,rate_disp Parameters for
#' gamma prior for 'rate'. Numeric vector.
#' @param rate_levels Levels of 'by' variables for
#' which there are potentially distinct values
#' of 'rate_mean' and 'rate_disp'.
#' A character vector.
#' @param rate_matrix_outcome Sparse matrix mapping
#' rate to outcome
#' @param nms_by Names of by variables for prob, rate
#' 
#' @returns Object of class 'bage_datamod_miscount'
#'
#' @noRd
new_bage_datamod_miscount <- function(prob_mean,
                                      prob_disp,
                                      prob_levels,
                                      prob_matrix_outcome,
                                      rate_mean,
                                      rate_disp,
                                      rate_levels,
                                      rate_matrix_outcome,
                                      nms_by) {
  ans <- list(prob_mean = prob_mean,
              prob_disp = prob_disp,
              prob_levels = prob_levels,
              prob_matrix_outcome = prob_matrix_outcome,
              rate_mean = rate_mean,
              rate_disp = rate_disp,
              rate_levels = rate_levels,
              rate_matrix_outcome = rate_matrix_outcome,
              nms_by = nms_by)
  class(ans) <- c("bage_datamod_miscount",
                  "bage_datamod_outcome",
                  "bage_datamod")
  ans
}


## HAS_TESTS
#' Create New Random Error Data Model for Outcomes
#'
#' @param mean_mean Mean of errors. Numeric vector.
#' @param mean_levels Levels of 'by' variables for
#' which there are potentially distinct values
#' of 'mean_mean'. A character vector.
#' @param mean_matrix_outcome Sparse matrix mapping
#' mean to outcome
#' @param sd_sd Standard devation of errors Numeric vector
#' @param sd_levels Levels of 'by' variables for
#' which there are potentially distinct values
#' of 'sd_sd'. A character vector.
#' @param sd_matrix_outcome Sparse matrix mapping
#' sd to outcome
#' @param nms_by Names of by variables for mean, sd
#' 
#' @returns Object of class 'bage_datamod_noise'
#'
#' @noRd
new_bage_datamod_noise <- function(mean_mean,
                                   mean_levels,
                                   mean_matrix_outcome,
                                   sd_sd,
                                   sd_levels,
                                   sd_matrix_outcome,
                                   nms_by) {
  ans <- list(mean_mean = mean_mean,
              mean_levels = mean_levels,
              mean_matrix_outcome = mean_matrix_outcome,
              sd_sd = sd_sd,
              sd_levels = sd_levels,
              sd_matrix_outcome = sd_matrix_outcome,
              nms_by = nms_by)
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
#' @param nms_by Names of by variables for rate
#' 
#' @returns Object of class 'bage_datamod_over'
#'
#' @noRd
new_bage_datamod_overcount <- function(rate_mean,
                                       rate_disp,
                                       rate_levels,
                                       rate_matrix_outcome,
                                       nms_by) {
  ans <- list(rate_mean = rate_mean,
              rate_disp = rate_disp,
              rate_levels = rate_levels,
              rate_matrix_outcome = rate_matrix_outcome,
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
#' @param nms_by Names of by variables for prob
#' 
#' @returns Object of class 'bage_datamod_undercount'
#'
#' @noRd
new_bage_datamod_undercount <- function(prob_mean,
                                        prob_disp,
                                        prob_levels,
                                        prob_matrix_outcome,
                                        nms_by) {
  ans <- list(prob_mean = prob_mean,
              prob_disp = prob_disp,
              prob_levels = prob_levels,
              prob_matrix_outcome = prob_matrix_outcome,
              nms_by = nms_by)
  class(ans) <- c("bage_datamod_undercount",
                  "bage_datamod_outcome",
                  "bage_datamod")
  ans
}

