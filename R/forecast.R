
## HAS_TESTS
#' Estimate Values on a Line or Lines
#'
#' @param slope Slope(s) of line(s). An rvec.
#' @param matrix_along_by_est Matrix mapping
#' along and by dimensions to position in estiamtes
#'
#' @returns An rvec
#'
#' @noRd
estimate_lin <- function(slope,
                         matrix_along_by_est) {
  n_along <- nrow(matrix_along_by_est)
  n_by <- ncol(matrix_along_by_est)
  ans <- rep(slope[[1L]], times = n_along * n_by)
  q <- seq(from = -1, to = 1, length.out = n_along)
  for (i_by in seq_len(n_by)) {
    i_ans <- matrix_along_by_est[, i_by] + 1L
    ans[i_ans] <- slope[i_by] * q
  }
  ans
}


## HAS_TESTS
#' Forecast an AR Process
#'
#' @param ar_est Historical estimates. An rvec.
#' @param coef AR coefficients. An rvec.
#' @param sd Standard deviation for AR model. An rvec of length 1.
#' @param matrix_along_by_est Matrix mapping
#' along and by dimensions to position in estiamtes
#' @param matrix_along_by_forecast Matrix mapping
#' along and by dimensions to position in forecasts
#'
#' @returns An rvec
#'
#' @noRd
forecast_ar <- function(ar_est,
                        coef,
                        sd,
                        matrix_along_by_est,
                        matrix_along_by_forecast) {
  n_ar <- length(coef)
  n_along_est <- nrow(matrix_along_by_est)
  n_along_forecast <- nrow(matrix_along_by_forecast)
  n_by <- ncol(matrix_along_by_est)
  ans <- rep(ar_est[[1L]], times = n_along_forecast * n_by)
  tmp <- rep(ar_est[[1L]], times = n_along_forecast + n_ar)
  s_head <- seq_len(n_ar)
  s_tail <- seq(to = n_along_est, length.out = n_ar)
  for (i_by in seq_len(n_by)) {
    i_tail <- matrix_along_by_est[s_tail, i_by] + 1L ## matrix uses 0-based index
    tmp[s_head] <- ar_est[i_tail]
    for (j in seq_len(n_along_forecast)) {
      s_ar <- seq(from = j, to = j + n_ar - 1L)
      mean <- sum(coef * tmp[s_ar])
      tmp[[j + n_ar]] <- rvec::rnorm_rvec(n = 1L, mean = mean, sd = sd)
    }
    i_ans <- matrix_along_by_forecast[, i_by] + 1L
    ans[i_ans] <- tmp[-s_head]
  }
  ans
}


## HAS_TESTS
#' Forecast Time-Varying Effects in 'components'
#'
#' @param mod Object of class 'bage_mod'
#' @param components_est Tibble with results
#' of call to 'components'.
#' @param labels_forecast Vector
#' with labels for future time periods.
#'
#' @returns A tibble
#'
#' @noRd
forecast_components <- function(mod,
                                components_est,
                                labels_forecast) {
  priors <- mod$priors
  var_time <- mod$var_time
  nms_priors <- names(priors)
  matrices_along_by_est <- choose_matrices_along_by(mod)
  matrices_along_by_forecast <- make_matrices_along_by_forecast(mod = mod,
                                                                labels_forecast = labels_forecast)
  levels_forecast_all <- make_levels_forecast_all(mod = mod,
                                                  labels_forecast = labels_forecast)
  is_time_varying_one <- function(nm) var_time %in% strsplit(nm, split = ":")[[1L]]
  is_time_varying <- vapply(nms_priors, is_time_varying_one, TRUE)
  ans <- .mapply(forecast_term,
                 dots = list(prior = priors[is_time_varying],
                             nm_prior = nms_priors[is_time_varying],
                             matrix_along_by_est = matrices_along_by_est[is_time_varying],
                             matrix_along_by_forecast = matrices_along_by_forecast[is_time_varying],
                             levels_forecast = levels_forecast_all[is_time_varying]),
                 MoreArgs = list(components = components_est))
  ans <- vctrs::vec_rbind(!!!ans)
  ans <- sort_components(ans, mod = mod)
  ans
}


## HAS_TESTS
#' Forecast Line or Lines
#'
#' @param slope Slope of line(s).
#' @param matrix_along_by_est Matrix mapping
#' along and by dimensions to position in estiamtes
#' @param matrix_along_by_forecast Matrix mapping
#' along and by dimensions to position in forecasts
#'
#' @returns An rvec
#'
#' @noRd
forecast_lin <- function(slope,
                         matrix_along_by_est,
                         matrix_along_by_forecast) {
  n_along_est <- nrow(matrix_along_by_est)
  n_along_forecast <- nrow(matrix_along_by_forecast)
  n_by <- ncol(matrix_along_by_est)
  ans <- rep(slope[[1L]], times = n_along_forecast * n_by)
  incr_q <- 2 / (n_along_est - 1)
  q <- seq(from = 1 + incr_q,
           by = incr_q,
           length.out = n_along_forecast)
  for (i_by in seq_len(n_by)) {
    i_ans <- matrix_along_by_forecast[, i_by] + 1L
    ans[i_ans] <- slope[i_by] * q
  }
  ans
}

## HAS_TESTS
#' Forecast Normal Distribution
#'
#' @param sd Standard deviation. An rvec of length 1.
#' @param matrix_along_by_forecast Matrix mapping
#' along and by dimensions to position in forecasts
#'
#' @returns An rvec
#'
#' @noRd
forecast_norm <- function(sd,
                          matrix_along_by_forecast) {
  n <- length(matrix_along_by_forecast)
  rvec::rnorm_rvec(n = n, sd = sd)
}


## HAS_TESTS
#' Forecast a Random Walk
#'
#' @param rw_est Historical estimates. An rvec.
#' @param sd Standard deviation for RW model. An rvec of length 1.
#' @param matrix_along_by_est Matrix mapping
#' along and by dimensions to position in estiamtes
#' @param matrix_along_by_forecast Matrix mapping
#' along and by dimensions to position in forecasts
#'
#' @returns An rvec
#'
#' @noRd
forecast_rw <- function(rw_est,
                        sd,
                        matrix_along_by_est,
                        matrix_along_by_forecast) {
  n_along_est <- nrow(matrix_along_by_est)
  n_along_forecast <- nrow(matrix_along_by_forecast)
  n_by <- ncol(matrix_along_by_est)
  ans <- rep(rw_est[[1L]], times = n_along_forecast * n_by)
  tmp <- rep(rw_est[[1L]], times = n_along_forecast + 1L)
  for (i_by in seq_len(n_by)) {
    i_last <- matrix_along_by_est[n_along_est, i_by] + 1L
    tmp[[1L]] <- rw_est[[i_last]]
    for (j in seq_len(n_along_forecast))
      tmp[[j + 1L]] <- rvec::rnorm_rvec(n = 1L, mean = tmp[[j]], sd = sd)
    i_ans <- matrix_along_by_forecast[, i_by] + 1L
    ans[i_ans] <- tmp[-1L]
  }
  ans
}


## HAS_TESTS
#' Forecast a Second Order Random Walk
#'
#' @param rw2_est Historical estimates. An rvec.
#' @param sd Standard deviation for RW model. An rvec of length 1.
#' @param matrix_along_by_est Matrix mapping
#' along and by dimensions to position in estiamtes
#' @param matrix_along_by_forecast Matrix mapping
#' along and by dimensions to position in forecasts
#'
#' @returns An rvec
#'
#' @noRd
forecast_rw2 <- function(rw2_est,
                         sd,
                         matrix_along_by_est,
                         matrix_along_by_forecast) {
  n_along_est <- nrow(matrix_along_by_est)
  n_along_forecast <- nrow(matrix_along_by_forecast)
  n_by <- ncol(matrix_along_by_est)  
  ans <- rep(rw2_est[[1L]], times = n_along_forecast * n_by)
  tmp <- rep(rw2_est[[1L]], times = n_along_forecast + 2L)
  for (i_by in seq_len(n_by)) {
    i_last <- matrix_along_by_est[n_along_est, i_by] + 1L
    i_second_last <- matrix_along_by_est[n_along_est - 1L, i_by] + 1L
    tmp[[2L]] <- rw2_est[[i_last]]
    tmp[[1L]] <- rw2_est[[i_second_last]]
    for (j in seq_len(n_along_forecast))
      tmp[[j + 2L]] <- rvec::rnorm_rvec(n = 1L,
                                        mean = 2 * tmp[[j + 1L]] - tmp[[j]],
                                        sd = sd)
    i_ans <- matrix_along_by_forecast[, i_by] + 1L
    ans[i_ans] <- tmp[-(1:2)]
  }
  ans
}


## HAS_TESTS
#' Forecast Fixed Seasonal Effects
#'
#' Use first 'n_season' entries from historical
#' estimates, within each combination of 'by' variables
#'
#' @param n Number of seasons.
#' @param seas_est Historical estimates. An rvec.
#' @param matrix_along_by_est Matrix mapping
#' along and by dimensions to position in estiamtes
#' @param matrix_along_by_forecast Matrix mapping
#' along and by dimensions to position in forecasts
#'
#' @returns An rvec
#'
#' @noRd
forecast_seasfix <- function(n,
                             seas_est,
                             matrix_along_by_est,
                             matrix_along_by_forecast) {
  n_along_est <- nrow(matrix_along_by_est)
  n_along_forecast <- nrow(matrix_along_by_forecast)
  n_by <- ncol(matrix_along_by_est)
  ans <- rep(seas_est[[1L]], times = n_along_forecast * n_by)
  tmp <- rep(seas_est[[1L]], times = n_along_forecast + n)
  s_head <- seq_len(n)
  s_tail <- seq.int(to = n_along_est, length.out = n)
  for (i_by in seq_len(n_by)) {
    i_tail <- matrix_along_by_est[s_tail, i_by] + 1L
    tmp[s_head] <- seas_est[i_tail]
    for (j in seq_len(n_along_forecast))
      tmp[[j + n]] <- tmp[[j]]
    i_ans <- matrix_along_by_forecast[, i_by] + 1L
    ans[i_ans] <- tmp[-s_head]
  }
  ans
}


## HAS_TESTS
#' Forecast Time-Varying Seasonal Effects
#'
#' @param n Number of seasons.
#' @param seas_est Historical estimates. An rvec.
#' @param sd Standard deviation for seasonal effects. An rvec of length 1.
#' @param matrix_along_by_est Matrix mapping
#' along and by dimensions to position in estiamtes
#' @param matrix_along_by_forecast Matrix mapping
#' along and by dimensions to position in forecasts
#'
#' @returns An rvec
#'
#' @noRd
forecast_seasvary <- function(n,
                              seas_est,
                              sd,
                              matrix_along_by_est,
                              matrix_along_by_forecast) {
  n_along_est <- nrow(matrix_along_by_est)
  n_along_forecast <- nrow(matrix_along_by_forecast)
  n_by <- ncol(matrix_along_by_est)
  ans <- rep(seas_est[[1L]], times = n_along_forecast * n_by)
  tmp <- rep(seas_est[[1L]], times = n_along_forecast + n)
  s_head <- seq_len(n)
  s_tail <- seq.int(to = n_along_est, length.out = n)
  for (i_by in seq_len(n_by)) {
    i_tail <- matrix_along_by_est[s_tail, i_by] + 1L
    tmp[s_head] <- seas_est[i_tail]
    for (j in seq_len(n_along_forecast))
      tmp[[j + n]] <- rvec::rnorm_rvec(n = 1L, mean = tmp[[j]], sd = sd)
    i_ans <- matrix_along_by_forecast[, i_by] + 1L
    ans[i_ans] <- tmp[-s_head]
  }
  ans
}


## HAS_TESTS
#' Create Extra Rows for 'data', holding Values for
#' Predictor Variables Used in Forecast
#'
#' @param mod Object of class 'bage_mod'
#' @param labels_forecast Vector
#' with labels for future time periods
#'
#' @returns A tibble.
#'
#' @noRd
make_data_forecast <- function(mod, labels_forecast) {
  formula <- mod$formula
  data <- mod$data
  var_time <- mod$var_time
  nms_model <- all.vars(formula[-2L])
  ans <- lapply(data[nms_model], unique)
  ans[[var_time]] <- labels_forecast
  ans <- vctrs::vec_expand_grid(!!!ans)
  ans <- vctrs::vec_rbind(data, ans)
  i_original <- seq_len(nrow(data))
  ans <- ans[-i_original, ]
  ans
}
