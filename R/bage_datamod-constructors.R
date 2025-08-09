




#' Create New Data Model for Exposures
#'
#' @returns Object of class 'bage_datamod_exposure'
#'
#' @noRd
new_bage_datamod_exposure <- function(ratio,
                                      disp_mean,
                                      matrix_ratio_outcome,
                                      matrix_disp_outcome,
                                      index_ratio_used,
                                      index_disp_used) {
  ans <- list(ratio = ratio,
              disp_mean = disp_mean,
              matrix_ratio_outcome = matrix_rate_outcome,
              matrix_disp_outcome = matrix_disp_outcome,
              index_ratio_used = index_rate_used,
              index_disp_used = index_disp_used)
  class(ans) <- c("bage_datamod_exposure",
                  "bage_datamod_offset",
                  "bage_datamod")
  ans
}



#' Create New Miscount Data Model for Outcomes
#'
#' @returns Object of class 'bage_datamod_miscount'
#'
#' @noRd
new_bage_datamod_miscount <- function(prob_shape1,
                                      prob_shape2,
                                      rate_shape,
                                      rate_rate,
                                      matrix_prob_outcome,
                                      matrix_rate_outcome,
                                      index_rate_used,
                                      index_prob_used) {
  ans <- list(prob_shape1 = prob_shape1,
              prob_shape1 = prob_shape1,
              rate_shape = rate_shape,
              rate_rate = rate_rate,
              matrix_prob_outcome = matrix_prob_outcome,
              matrix_rate_outcome = matrix_rate_outcome,
              index_rate_used = index_rate_used,
              index_prob_used = index_prob_used)
  class(ans) <- c("bage_datamod_miscount",
                  "bage_datamod_outcome",
                  "bage_datamod")
  ans
}


#' Create New Random Error Data Model for Outcomes
#'
#' @returns Object of class 'bage_datamod_error'
#'
#' @noRd
new_bage_datamod_noise <- function(mean,
                                   sd,
                                   matrix_mean_outcome,
                                   matrix_sd_outcome,
                                   index_mean_used,
                                   index_sd_used) {
  ans <- list(mean = mean,
              sd = sd,
              matrix_mean_outcome = matrix_mean_outcome,
              matrix_sd_outcome = matrix_sd_outcome,
              index_mean_used = index_mean_used,
              index_sd_used = index_sd_used)
  class(ans) <- c("bage_datamod_noise",
                  "bage_datamod_outcome",
                  "bage_datamod")
  ans
}


#' Create New Overcount Data Model for Outcomes
#'
#' @returns Object of class 'bage_datamod_over'
#'
#' @noRd
new_bage_datamod_overcount <- function(rate,
                                       matrix_rate_outcome,
                                       index_rate_used) {
  ans <- list(rate = rate,
              matrix_rate_outcome = matrix_rate_outcome,
              index_rate_used = index_rate_used)
  class(ans) <- c("bage_datamod_overcount",
                  "bage_datamod_outcome",
                  "bage_datamod")
  ans
}


#' Create New Undercount Data Model for Outcomes
#'
#' @returns Object of class 'bage_datamod_undercount'
#'
#' @noRd
new_bage_datamod_undercount <- function(prob,
                                        matrix_prob_outcome,
                                        index_prob_used) {
  ans <- list(prob = prob,
              matrix_prob_outcome = matrix_prob_outcome,
              index_prob_used = index_prob_used)
  class(ans) <- c("bage_datamod_undercount",
                  "bage_datamod_outcome",
                  "bage_datamod")
  ans
}

