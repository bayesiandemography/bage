## 'datamod_descr' ------------------------------------------------------------

#' Name of Data Model Used in Printing
#'
#' @param datamod An object of class 'bage_datamod'
#'
#' @returns A string
#'
#' @noRd
datamod_descr <- function(datamod) {
    UseMethod("datamod_descr")
}

## HAS_TESTS
#' @export
datamod_descr.bage_datamod_exposure <- function(datamod) "exposure"

## HAS_TESTS
#' @export
datamod_descr.bage_datamod_miscount <- function(datamod) "miscount"

## HAS_TESTS
#' @export
datamod_descr.bage_datamod_noise <- function(datamod) "noise"

## HAS_TESTS
#' @export
datamod_descr.bage_datamod_overcount <- function(datamod) "overcount"

## HAS_TESTS
#' @export
datamod_descr.bage_datamod_undercount <- function(datamod) "undercount"



## 'draw_datamod_param' -------------------------------------------------------

## HAS_TESTS
#' Draw from Prior Distribution for Data Model Parameters
#'
#' @param datamod Object of class 'bage_mod'
#' @param n_sim Number of draws
#'
#' @returns A matrix
#'
#' @noRd
draw_datamod_param <- function(datamod, n_sim) {
  UseMethod("draw_datamod_param")
}

## HAS_TESTS
#' @export
draw_datamod_param.bage_datamod_miscount <- function(datamod, n_sim) {
  prob_mean <- datamod$prob_mean
  prob_disp <- datamod$prob_disp
  rate_mean <- datamod$rate_mean
  rate_disp <- datamod$rate_disp
  n_prob <- length(prob_mean)
  n_rate <- length(rate_mean)
  shape1 <- prob_mean / prob_disp
  shape2 <- (1 - prob_mean) / prob_disp
  shape1 <- rep(shape1, times = n_sim)
  shape2 <- rep(shape2, times = n_sim)
  prob <- stats::rbeta(n = n_prob * n_sim, shape1 = shape1, shape2 = shape2)
  shape <- 1 / rate_disp
  scale <- rate_disp * rate_mean
  rate <- stats::rgamma(n = n_rate * n_sim, shape = shape, scale = scale)
  prob <- matrix(prob, nrow = n_prob, ncol = n_sim)
  rate <- matrix(rate, nrow = n_rate, ncol = n_sim)
  ans <- rbind(prob, rate)
  ans
}

## HAS_TESTS
#' @export
draw_datamod_param.bage_datamod_overcount <- function(datamod, n_sim) {
  rate_mean <- datamod$rate_mean
  rate_disp <- datamod$rate_disp
  n_rate <- length(rate_mean)
  shape <- 1 / rate_disp
  scale <- rate_disp * rate_mean
  rate <- stats::rgamma(n = n_rate * n_sim, shape = shape, scale = scale)
  ans <- matrix(rate, nrow = n_rate, ncol = n_sim)
  ans
}

## HAS_TESTS
#' @export
draw_datamod_param.bage_datamod_undercount <- function(datamod, n_sim) {
  prob_mean <- datamod$prob_mean
  prob_disp <- datamod$prob_disp
  n_prob <- length(prob_mean)
  shape1 <- prob_mean / prob_disp
  shape2 <- (1 - prob_mean) / prob_disp
  shape1 <- rep(shape1, times = n_sim)
  shape2 <- rep(shape2, times = n_sim)
  prob <- stats::rbeta(n = n_prob * n_sim, shape1 = shape1, shape2 = shape2)
  ans <- matrix(prob, nrow = n_prob, ncol = n_sim)
  ans
}


## 'draw_offset_obs_given_true' ----------------------------------------------

#' Draw Values for Observed Offset, Given True Offset,
#' Based on Data Model
#'
#'
#' @param datamod Object of class 'bage_datamod'
#' @param components Tibble with estimates for hyper-parameters
#' @param offset_true True values for offset.
#' Numeric vector.
#'
#' @returns An rvec
#'
#' @noRd
draw_offset_obs_given_true <- function(datamod,
                                       components,
                                       offset_true) {
  UseMethod("draw_offset_obs_given_true")
}

## HAS_TESTS
#' @export
draw_offset_obs_given_true.bage_datamod_exposure <- function(datamod,
                                                             components,
                                                             offset_true) {
  n_offset <- length(offset_true)
  n_draw <- rvec::n_draw(components$.fitted)
  ans <- rvec::new_rvec(length = n_offset, n_draw = n_draw)
  is_ok <- !is.na(offset_true)
  n_ok <- sum(is_ok)
  disp <- get_datamod_disp(datamod)
  disp_inv <- 1 / disp
  shape <- 2 + disp_inv[is_ok]
  rate <- (1 + disp_inv[is_ok]) * offset_true[is_ok]
  ans_inv <- rvec::rgamma_rvec(n = n_ok,
                               shape = shape,
                               rate = rate,
                               n_draw = n_draw)
  ans[is_ok] <- 1 / ans_inv
  ans
}


## 'draw_offset_true_given_obs' ----------------------------------------------

#' Draw Values for True Offset, Given Observed Offset,
#' Based on Data Model
#'
#'
#' @param datamod Object of class 'bage_datamod'
#' @param nm_distn Name of distribution of offset.
#' @param components Data frame with components.
#' @param outcome Values for outcome. Numeric vector
#' @param offset Observed values for offset.
#' Numeric vector.
#' @param expected Expected values for rates/prob/mean.
#' An rvec, the same length as 'outcome'.
#'
#' @returns An rvec
#'
#' @noRd
draw_offset_true_given_obs <- function(datamod,
                                       nm_distn,
                                       components,
                                       outcome,
                                       offset_obs,
                                       expected) {
  UseMethod("draw_offset_true_given_obs")
}

## HAS_TESTS
#' @export
draw_offset_true_given_obs.bage_datamod_exposure <- function(datamod,
                                                             nm_distn,
                                                             components,
                                                             outcome,
                                                             offset_obs,
                                                             expected) {
  if (nm_distn == "pois") {
    n_offset <- length(offset_obs)
    n_draw <- rvec::n_draw(expected)
    offset_obs <- rep(offset_obs, times = n_draw)
    if (rvec::is_rvec(outcome))
      outcome <- as.numeric(outcome)
    else
      outcome <- rep(outcome, times = n_draw)
    expected <- as.numeric(expected)
    is_ok <- !is.na(offset_obs) & !is.na(outcome)
    n_ok <- sum(is_ok)
    ans <- rep.int(NA_real_, times = n_offset * n_draw)
    disp <- get_datamod_disp(datamod)
    disp <- rep.int(disp, times = n_draw)
    shape <- 3 + 1 / disp[is_ok] + outcome[is_ok]
    rate <- (1 + 1 / disp[is_ok]) / offset_obs[is_ok] + expected[is_ok]
    ans[is_ok] <- rvec::rgamma_rvec(n = n_ok,
                                    shape = shape,
                                    rate = rate)
  }
  else {
    cli::cli_abort(paste("Internal error: {.var datamod} has class",
                         "{.cls {class(datamod)}} but {.var nm_distn}",
                         "is {.val {nm_distn}}."))
  }
  ans <- matrix(ans, nrow = n_offset, ncol = n_draw)
  ans <- rvec::rvec_dbl(ans)
  ans
}


## 'draw_outcome_obs_given_true' ----------------------------------------------

#' Draw Values for Observed Outcome Given True Outcome, 
#' Based on Data Model
#'
#' @param datamod Object of class 'bage_datamod'
#' @param components Data frame with components.
#' @param outcome_true True values for outcome.
#' Can be rvec or numeric vector.
#' @param offset Values for offset.
#' Numeric vector.
#' @param fitted Values for rates/prob/mean.
#' An rvec, the same length as 'outcome'.
#'
#' @returns An rvec
#'
#' @noRd
draw_outcome_obs_given_true <- function(datamod,
                                        components,
                                        outcome_true,
                                        offset,
                                        fitted) {
  UseMethod("draw_outcome_obs_given_true")
}

## HAS_TESTS
#' @export
draw_outcome_obs_given_true.bage_datamod_miscount <- function(datamod,
                                                              components,
                                                              outcome_true,
                                                              offset,
                                                              fitted) {
  n_outcome <- length(outcome_true)
  n_draw <- rvec::n_draw(fitted)
  if (rvec::is_rvec(outcome_true))
    outcome_true <- as.numeric(outcome_true)
  else
    outcome_true <- rep(outcome_true, times = n_draw)
  offset <- rep(offset, times = n_draw)
  fitted <- as.numeric(fitted)
  is_ok <- !is.na(outcome_true) & !is.na(offset)
  n_ok <- sum(is_ok)
  ans <- rep.int(NA_real_, times = n_outcome * n_draw)
  prob <- get_datamod_prob(datamod = datamod,
                           components = components)
  rate <- get_datamod_rate(datamod = datamod,
                           components = components)
  prob <- as.numeric(prob)
  rate <- as.numeric(rate)
  prob <- prob[is_ok]
  size <- outcome_true[is_ok]
  obs_true <- rbinom_guarded(prob = prob, size = size)
  lambda <- rate[is_ok] * fitted[is_ok] * offset[is_ok]
  obs_false <- rpois_guarded(lambda)
  ans[is_ok] <- obs_true + obs_false
  ans <- matrix(ans, nrow = n_outcome, ncol = n_draw)
  ans <- rvec::rvec_dbl(ans)
  ans
}

## HAS_TESTS
#' @export
draw_outcome_obs_given_true.bage_datamod_noise <- function(datamod,
                                                           components,
                                                           outcome_true,
                                                           offset,
                                                           fitted) {
  n_outcome <- length(outcome_true)
  n_draw <- rvec::n_draw(fitted)
  ans <- rep.int(NA_real_, times = n_outcome * n_draw)
  if (rvec::is_rvec(outcome_true))
    outcome_true <- as.numeric(outcome_true)
  else
    outcome_true <- rep(outcome_true, times = n_draw)
  is_ok <- !is.na(outcome_true)
  n_ok <- sum(is_ok)
  sd <- get_datamod_sd(datamod)
  sd <- rep(sd, times = n_draw)
  outcome_sd <- datamod$outcome_sd
  is_skellam <- is.null(outcome_sd)
  if (is_skellam) {
    mu <- 0.5 * (sd[is_ok])^2
    noise <- stats::rpois(n = n_ok, lambda = mu) -
      stats::rpois(n = n_ok, lambda = mu)
  }
  else
    noise <- stats::rnorm(n = n_ok, mean = 0, sd = sd[is_ok])
  ans[is_ok] <- outcome_true[is_ok] + noise
  ans <- matrix(ans, nrow = n_outcome, ncol = n_draw)
  ans <- rvec::rvec_dbl(ans)
  ans
}

## HAS_TESTS
#' @export
draw_outcome_obs_given_true.bage_datamod_overcount <- function(datamod,
                                                               components,
                                                               outcome_true,
                                                               offset,
                                                               fitted) {
  n_outcome <- length(outcome_true)
  n_draw <- rvec::n_draw(fitted)
  ans <- rep.int(NA_real_, times = n_outcome * n_draw)
  if (rvec::is_rvec(outcome_true))
    outcome_true <- as.numeric(outcome_true)
  else
    outcome_true <- rep(outcome_true, times = n_draw)
  offset <- rep(offset, times = n_draw)
  fitted <- as.numeric(fitted)
  is_ok <- !is.na(outcome_true) & !is.na(offset)
  n_ok <- sum(is_ok)
  rate <- get_datamod_rate(datamod = datamod,
                           components = components)
  rate <- as.numeric(rate)
  lambda <- rate[is_ok] * fitted[is_ok] * offset[is_ok]
  error <- rpois_guarded(lambda)
  ans[is_ok] <- outcome_true[is_ok] + error
  ans <- matrix(ans, nrow = n_outcome, ncol = n_draw)
  ans <- rvec::rvec_dbl(ans)
  ans
}

## HAS_TESTS
#' @export
draw_outcome_obs_given_true.bage_datamod_undercount <- function(datamod,
                                                                components,
                                                                outcome_true,
                                                                offset,
                                                                fitted) {
  n_outcome <- length(outcome_true)
  n_draw <- rvec::n_draw(fitted)
  ans <- rep.int(NA_real_, times = n_outcome * n_draw)
  if (rvec::is_rvec(outcome_true))
    outcome_true <- as.numeric(outcome_true)
  else
    outcome_true <- rep(outcome_true, times = n_draw)
  offset <- rep(offset, times = n_draw)
  is_ok <- !is.na(outcome_true) & !is.na(offset)
  n_ok <- sum(is_ok)
  prob <- get_datamod_prob(datamod = datamod,
                           components = components)
  prob <- as.numeric(prob)
  size <- outcome_true[is_ok]
  prob <- prob[is_ok]
  ans[is_ok] <- rbinom_guarded(prob = prob, size = size)
  ans <- matrix(ans, nrow = n_outcome, ncol = n_draw)
  ans <- rvec::rvec_dbl(ans)
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
#' An rvec, the same length as 'outcome'.
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

## HAS_TESTS
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

## HAS_TESTS
#' @export
draw_outcome_true_given_obs.bage_datamod_miscount <- function(datamod,
                                                              nm_distn,
                                                              components,
                                                              outcome,
                                                              offset,
                                                              expected,
                                                              disp) {
  has_disp <- !is.null(disp)
  n_outcome <- length(outcome)
  n_draw <- n_draw(expected)
  if (rvec::is_rvec(outcome))
    outcome <- as.numeric(outcome)
  else
    outcome <- rep(outcome, times = n_draw)
  offset <- rep(offset, times = n_draw)
  expected <- as.numeric(expected)
  if (has_disp) {
    disp <- as.numeric(disp)
    disp <- rep(disp, each = n_outcome)
  }
  is_ok <- !is.na(outcome) & !is.na(offset)
  n_ok <- sum(is_ok)
  ans <- rep(NA_real_, times = n_outcome * n_draw)
  if (nm_distn == "pois") {
    prob <- get_datamod_prob(datamod = datamod,
                             components = components)
    rate <- get_datamod_rate(datamod = datamod,
                             components = components)
    prob <- as.numeric(prob)
    rate <- as.numeric(rate)
    prob_obs <- prob[is_ok] / (prob[is_ok] + rate[is_ok])
    outcome_true_obs <- rbinom_guarded(size = outcome[is_ok],
                                       prob = prob_obs)
    if (has_disp) {
      size_unobs <- outcome_true_obs + (1 / disp[is_ok])
      prob_unobs <- ((1 + prob[is_ok] * expected[is_ok] * offset[is_ok] * disp[is_ok])
        / (1 + expected[is_ok] * offset[is_ok] * disp[is_ok]))
      outcome_true_unobs <- stats::rnbinom(n = n_ok,
                                           size = size_unobs,
                                           prob = prob_unobs)
    }
    else {
      lambda <- (1 - prob[is_ok]) * expected[is_ok] * offset[is_ok]
      outcome_true_unobs <- rpois_guarded(lambda)
    }
    ans[is_ok] <- outcome_true_obs + outcome_true_unobs
  }
  else {
    cli::cli_abort(paste("Internal error: {.var datamod} has class",
                         "{.cls {class(datamod)}} but {.var nm_distn}",
                         "is {.val {nm_distn}}."))
  }
  ans <- matrix(ans, nrow = n_outcome, ncol = n_draw)
  ans <- rvec::rvec_dbl(ans)
  ans
}

## HAS_TESTS
## Expecting offset, expected, and disp to be on original scale
#' @export
draw_outcome_true_given_obs.bage_datamod_noise <- function(datamod,
                                                           nm_distn,
                                                           components,
                                                           outcome,
                                                           offset,
                                                           expected,
                                                           disp) {
  n_outcome <- length(outcome)
  n_draw <- rvec::n_draw(expected)
  if (rvec::is_rvec(outcome))
    outcome <- as.numeric(outcome)
  else
    outcome <- rep(outcome, times = n_draw)
  offset <- rep(offset, times = n_draw)
  expected <- as.numeric(expected)
  is_ok <- !is.na(outcome) & !is.na(offset)
  n_ok <- sum(is_ok)
  sd_noise <- get_datamod_sd(datamod)
  sd_noise <- rep(sd_noise, times = n_draw)
  ans <- rep(NA_real_, times = n_outcome * n_draw)
  if (nm_distn == "norm") {
    disp <- as.numeric(disp)
    disp <- rep(disp, each = n_outcome)
    prec_true <- offset[is_ok] / ((disp[is_ok])^2)
    prec_noise <- 1 / ((sd_noise[is_ok])^2)
    wt_true <- prec_true / (prec_true + prec_noise)
    mean_true <- (wt_true * expected[is_ok]
      + (1 - wt_true) * outcome[is_ok])
    sd_true <- 1 / sqrt(prec_true + prec_noise)
    ans[is_ok] <- stats::rnorm(n = n_ok,
                               mean = mean_true,
                               sd = sd_true)
  }
  else if (nm_distn == "pois") {
    ## draw_true_given_obs_pois_skellam not vectorised
    for (i in seq_along(ans)) {
      if (is_ok[[i]]) {
        y_obs <- outcome[[i]]
        lambda <- expected[[i]] * offset[[i]]
        m <- 0.5 * (sd_noise[[i]])^2
        ans[[i]] <- draw_true_given_obs_pois_skellam(y_obs = y_obs,
                                                     lambda = lambda,
                                                     m = m)
      }
    }
  }
  else {
    cli::cli_abort(paste("Internal error: {.var datamod} has class",
                         "{.cls {class(datamod)}} but {.var nm_distn}",
                         "is {.val {nm_distn}}."))
  }
  ans <- matrix(ans, nrow = n_outcome, ncol = n_draw)
  ans <- rvec::rvec_dbl(ans)
  ans
}

## HAS_TESTS
#' @export
draw_outcome_true_given_obs.bage_datamod_overcount <- function(datamod,
                                                               nm_distn,
                                                               components,
                                                               outcome,
                                                               offset,
                                                               expected,
                                                               disp) {
  n_outcome <- length(outcome)
  n_draw <- rvec::n_draw(expected)
  if (rvec::is_rvec(outcome))
    outcome <- as.numeric(outcome)
  else
    outcome <- rep(outcome, times = n_draw)
  is_ok <- !is.na(outcome) ## not offset, since not used
  n_ok <- sum(is_ok)
  ans <- rep(NA_real_, times = n_outcome * n_draw)
  if (nm_distn == "pois") {
    rate <- get_datamod_rate(datamod = datamod,
                             components = components)
    rate <- as.numeric(rate)
    prob_obs <- 1 / (1 + rate[is_ok])
    ans[is_ok] <- rbinom_guarded(size = outcome[is_ok],
                                 prob = prob_obs)
  }
  else {
    cli::cli_abort(paste("Internal error: {.var datamod} has class",
                         "{.cls {class(datamod)}} but {.var nm_distn}",
                         "is {.val {nm_distn}}."))
  }
  ans <- matrix(ans, nrow = n_outcome, ncol = n_draw)
  ans <- rvec::rvec_dbl(ans)
  ans
}

## HAS_TESTS
#' @export
draw_outcome_true_given_obs.bage_datamod_undercount <- function(datamod,
                                                                nm_distn,
                                                                components,
                                                                outcome,
                                                                offset,
                                                                expected,
                                                                disp) {
  has_disp <- !is.null(disp)
  n_outcome <- length(outcome)
  n_draw <- rvec::n_draw(expected)
  if (rvec::is_rvec(outcome))
    outcome <- as.numeric(outcome)
  else
    outcome <- rep(outcome, times = n_draw)
  offset <- rep(offset, times = n_draw)
  expected <- as.numeric(expected)
  is_ok <- !is.na(outcome) & !is.na(offset)
  n_ok <- sum(is_ok)
  prob <- get_datamod_prob(datamod = datamod,
                           components = components)
  prob <- as.numeric(prob)
  if (has_disp) {
    disp <- as.numeric(disp)
    disp <- rep(disp, each = n_outcome)
  }
  ans <- rep(NA_real_, times = n_outcome * n_draw)
  if (nm_distn == "pois") {
    if (has_disp) {
      shape <- outcome[is_ok] + (1 / disp[is_ok])
      scale <- ((1 - prob[is_ok]) * expected[is_ok] * offset[is_ok] * disp[is_ok]
        / (1 + prob[is_ok] * expected[is_ok] * offset[is_ok] * disp[is_ok]))
      lambda <- stats::rgamma(n = n_ok, shape = shape, scale = scale)
    }
    else {
      lambda <- (1 - prob[is_ok]) * expected[is_ok] * offset[is_ok]
    }
    outcome_unobs <- rpois_guarded(lambda)
    ans[is_ok] <- outcome[is_ok] + outcome_unobs
  }
  else if (nm_distn == "binom") {
    if (has_disp) {
      ## more complex distribution, so gets special function
      ans[is_ok] <- sample_post_binom_betabinom(n = offset[is_ok],
                                                y = outcome[is_ok],
                                                mu = expected[is_ok],
                                                xi = disp[is_ok],
                                                pi = prob[is_ok])
    }
    else {
      size_unobs <- offset[is_ok] - outcome[is_ok]
      prob_unobs <- ((1 - prob[is_ok]) * expected[is_ok]
        / (1 - prob[is_ok] * expected[is_ok]))
      outcome_unobs <- rbinom_guarded(size = size_unobs,
                                      prob = prob_unobs)
      ans[is_ok] <- outcome[is_ok] + outcome_unobs
    }
  }
  else {
    cli::cli_abort(paste("Internal error: {.var datamod} has class",
                         "{.cls {class(datamod)}} but {.var nm_distn}",
                         "is {.val {nm_distn}}."))
  }
  ans <- matrix(ans, nrow = n_outcome, ncol = n_draw)
  ans <- rvec::rvec_dbl(ans)
  ans
}


## 'forecast_datamod_param' ---------------------------------------------------

#' Forecast Values for Data Model Parameters
#'
#' @param datamod Object of class 'bage_datamod'
#' @param data_forecast Data frame with components.
#' @param n_draw Number of posterior draws
#' @param has_newdata Whether user supplied
#' value for 'newdata'.
#'
#' @returns An rvec or NULL
#'
#' @noRd
forecast_datamod_param <- function(datamod,
                                   data_forecast,
                                   n_draw,
                                   has_newdata) {
  UseMethod("forecast_datamod_param")
}


## HAS_TESTS
#' @export
forecast_datamod_param.bage_datamod_miscount <- function(datamod,
                                                         data_forecast,
                                                         n_draw,
                                                         has_newdata) {
  prob_arg <- datamod$prob_arg
  rate_arg <- datamod$rate_arg
  nm_data <- if (has_newdata) "newdata" else "data"
  nms_by <- datamod$nms_by
  nms_prob <- names(prob_arg)
  nms_rate <- names(rate_arg)
  nms_by_prob <- intersect(nms_by, nms_prob)
  nms_by_rate <- intersect(nms_by, nms_rate)
  has_by_prob <- length(nms_by_prob) > 0L
  has_by_rate <- length(nms_by_rate) > 0L
  if (has_by_prob) {
    by_val_prob <- prob_arg[nms_by_prob]
    data <- data_forecast[nms_by_prob]
    check_datamod_by_val(by_val = by_val_prob,
                         data = data,
                         nm_val = "prob",
                         nm_data = nm_data)
    key_prob <- Reduce(paste_dot, by_val_prob)
    key_data <- Reduce(paste_dot, data)
    is_keep_prob <- key_prob %in% key_data
  }
  else
    is_keep_prob <- TRUE
  if (has_by_rate) {
    by_val_rate <- rate_arg[nms_by_rate]
    data <- data_forecast[nms_by_rate]
    check_datamod_by_val(by_val = by_val_rate,
                         data = data,
                         nm_val = "rate",
                         nm_data = nm_data)
    key_rate <- Reduce(paste_dot, by_val_rate)
    key_data <- Reduce(paste_dot, data)
    is_keep_rate <- key_rate %in% key_data
  }
  else
    is_keep_rate <- TRUE
  prob_mean <- prob_arg$mean[is_keep_prob]
  prob_disp <- prob_arg$disp[is_keep_prob]
  rate_mean <- rate_arg$mean[is_keep_rate]
  rate_disp <- rate_arg$disp[is_keep_rate]
  shape1 <- prob_mean / prob_disp
  shape2 <- (1 - prob_mean) / prob_disp
  shape <- 1 / rate_disp
  scale <- rate_disp * rate_mean
  n_keep_prob <- sum(is_keep_prob)
  n_keep_rate <- sum(is_keep_rate)
  prob <- rvec::rbeta_rvec(n = n_keep_prob,
                           shape1 = shape1,
                           shape2 = shape2,
                           n_draw = n_draw)
  rate <- rvec::rgamma_rvec(n = n_keep_rate,
                            shape = shape,
                            scale = scale,
                            n_draw = n_draw)
  .fitted <- c(prob, rate)
  comp_prob <- rep("prob", times = n_keep_prob)
  comp_rate <- rep("rate", times = n_keep_rate)
  comp <- c(comp_prob, comp_rate)
  level_prob <- if (has_by_prob) key_prob[is_keep_prob] else "prob"
  level_rate <- if (has_by_rate) key_rate[is_keep_rate] else "rate"
  component <- c(comp_prob, comp_rate)
  level <- c(level_prob, level_rate)
  level <- as.character(level)
  tibble::tibble(term = "datamod",
                 component = component,
                 level = level,
                 .fitted = .fitted) 
}    


## HAS_TESTS
#' @export
forecast_datamod_param.bage_datamod_overcount <- function(datamod,
                                                          data_forecast,
                                                          n_draw,
                                                          has_newdata) {
  rate_arg <- datamod$rate_arg
  nms_by <- datamod$nms_by
  has_by <- length(nms_by) > 0L
  if (has_by) {
    nms_rate <- names(rate_arg)
    nms_by_rate <- intersect(nms_by, nms_rate)
    by_val_rate <- rate_arg[nms_by_rate]
    data <- data_forecast[nms_by_rate]
    nm_data <- if (has_newdata) "newdata" else "data"
    check_datamod_by_val(by_val = by_val_rate,
                         data = data,
                         nm_val = "rate",
                         nm_data = nm_data)
    key_rate <- Reduce(paste_dot, by_val_rate)
    key_data <- Reduce(paste_dot, data)
    is_keep <- key_rate %in% key_data
  }
  else
    is_keep <- TRUE
  rate_mean <- rate_arg$mean[is_keep]
  rate_disp <- rate_arg$disp[is_keep]
  shape <- 1 / rate_disp
  scale <- rate_mean * rate_disp
  n_keep <- sum(is_keep)
  .fitted <- rvec::rgamma_rvec(n = n_keep,
                               shape = shape,
                               scale = scale,
                               n_draw = n_draw)
  component <- "rate"
  level <- if (has_by) key_rate[is_keep] else "rate"
  level <- as.character(level)
  tibble::tibble(term = "datamod",
                 component = component,
                 level = level,
                 .fitted = .fitted)
}

## HAS_TESTS
#' @export
forecast_datamod_param.bage_datamod_undercount <- function(datamod,
                                                           data_forecast,
                                                           n_draw,
                                                           has_newdata) {
  prob_arg <- datamod$prob_arg
  nms_by <- datamod$nms_by
  has_by <- length(nms_by) > 0L
  if (has_by) {
    nms_prob <- names(prob_arg)
    nms_by_prob <- intersect(nms_by, nms_prob)
    by_val_prob <- prob_arg[nms_by_prob]
    data <- data_forecast[nms_by_prob]
    nm_data <- if (has_newdata) "newdata" else "data"
    check_datamod_by_val(by_val = by_val_prob,
                         data = data,
                         nm_val = "prob",
                         nm_data = nm_data)
    key_prob <- Reduce(paste_dot, by_val_prob)
    key_data <- Reduce(paste_dot, data)
    is_keep <- key_prob %in% key_data
  }
  else
    is_keep <- TRUE
  prob_mean <- prob_arg$mean[is_keep]
  prob_disp <- prob_arg$disp[is_keep]
  shape1 <- prob_mean / prob_disp
  shape2 <- (1 - prob_mean) / prob_disp
  n_keep <- sum(is_keep)
  .fitted <- rvec::rbeta_rvec(n = n_keep,
                              shape1 = shape1,
                              shape2 = shape2,
                              n_draw = n_draw)
  component <- "prob"
  level <- if (has_by) key_prob[is_keep] else "prob"
  level <- as.character(level)
  tibble::tibble(term = "datamod",
                 component = component,
                 level = level,
                 .fitted = .fitted)
}


## 'forecast_outcome_obs_given_obs' ------------------------------------------

#' Forecast Values for Observed Outcome, Given True Outcome,
#' Based on Data Model
#'
#'
#' @param datamod Object of class 'bage_datamod'
#' @param components_forecast Data frame with
#' future values for components (including parameters
#' for data models)
#' @param data_forecast Data frame with future data
#' @param fitted Predicted rates, probabilities, means
#' An rvec.
#' @param outcome_true True value for outcome.
#' An rvec.
#' @param has_newdata Whether user supplied
#' value for 'newdata'.
#'
#' @returns An rvec
#'
#' @noRd
forecast_outcome_obs_given_true <- function(datamod,
                                            components_forecast,
                                            data_forecast,
                                            fitted,
                                            outcome_true,
                                            offset,
                                            has_newdata) {
  UseMethod("forecast_outcome_obs_given_true")
}

## HAS_TESTS
#' @export
forecast_outcome_obs_given_true.bage_datamod_miscount <- function(datamod,
                                                                  components_forecast,
                                                                  data_forecast,
                                                                  fitted,
                                                                  outcome_true,
                                                                  offset,
                                                                  has_newdata) {
  is_prob <- (components_forecast$term == "datamod"
    & components_forecast$component == "prob")
  prob <- components_forecast$.fitted[is_prob]
  level_prob <- components_forecast$level[is_prob]
  is_rate <- (components_forecast$term == "datamod"
    & components_forecast$component == "rate")
  rate <- components_forecast$.fitted[is_rate]
  level_rate <- components_forecast$level[is_rate]
  n_outcome <- length(outcome_true)
  nm_data <- if (has_newdata) "newdata" else "data"
  nms_by <- datamod$nms_by
  prob_arg <- datamod$prob_arg
  rate_arg <- datamod$rate_arg
  nms_prob <- names(prob_arg)
  nms_rate <- names(rate_arg)
  nms_by_prob <- intersect(nms_by, nms_prob)
  nms_by_rate <- intersect(nms_by, nms_rate)
  has_by_prob <- length(nms_by_prob) > 0L
  has_by_rate <- length(nms_by_rate) > 0L
  if (has_by_prob) {
    by_val_prob <- prob_arg[nms_by_prob]
    data <- data_forecast[nms_by_prob]
    check_datamod_by_val(by_val = by_val_prob,
                         data = data,
                         nm_val = "prob",
                         nm_data = nm_data)
    key_prob <- Reduce(paste_dot, by_val_prob)
    key_data <- Reduce(paste_dot, data)
    key_prob <- intersect(key_prob, level_prob)
    i_prob <- match(key_data, key_prob)
  }
  else
    i_prob <- rep(1L, times = n_outcome)
  if (has_by_rate) {
    by_val_rate <- rate_arg[nms_by_rate]
    data <- data_forecast[nms_by_rate]
    check_datamod_by_val(by_val = by_val_rate,
                         data = data,
                         nm_val = "rate",
                         nm_data = nm_data)
    key_rate <- Reduce(paste_dot, by_val_rate)
    key_data <- Reduce(paste_dot, data)
    key_rate <- intersect(key_rate, level_rate)
    i_rate <- match(key_data, key_rate)
  }
  else
    i_rate <- rep(1L, times = n_outcome)
  prob <- prob[i_prob]
  rate <- rate[i_rate]
  observed_true <- rbinom_guarded(size = outcome_true,
                                  prob = prob)
  lambda <- rate * fitted * offset
  observed_false <- rpois_guarded(lambda)
  observed_true + observed_false
}    


## HAS_TESTS
#' @export
forecast_outcome_obs_given_true.bage_datamod_noise <- function(datamod,
                                                               components_forecast,
                                                               data_forecast,
                                                               fitted,
                                                               outcome_true,
                                                               offset,
                                                               has_newdata) {
  sd_arg <- datamod$sd_arg
  outcome_sd <- datamod$outcome_sd
  is_skellam <- is.null(outcome_sd)
  n_outcome <- length(outcome_true)
  n_draw <- rvec::n_draw(outcome_true)
  nms_by <- datamod$nms_by
  has_by <- length(nms_by) > 0L
  if (has_by) {
    nms_sd <- names(sd_arg)
    nms_by_sd <- intersect(nms_by, nms_sd)
    by_val_sd <- sd_arg[nms_by_sd]
    data <- data_forecast[nms_by_sd]
    nm_data <- if (has_newdata) "newdata" else "data"
    check_datamod_by_val(by_val = by_val_sd,
                         data = data,
                         nm_val = "sd",
                         nm_data = nm_data)
    key_sd <- Reduce(paste_dot, by_val_sd)
    key_data <- Reduce(paste_dot, data)
    i <- match(key_data, key_sd)
  }
  else
    i <- rep(1L, times = n_outcome)
  sd <- sd_arg$sd[i]
  if (is_skellam) {
    mu <- 0.5 * sd^2
    noise <- rvec::rpois_rvec(n = n_outcome, lambda = mu, n_draw = n_draw) -
      rvec::rpois_rvec(n = n_outcome, lambda = mu, n_draw = n_draw)
  }
  else
    noise <- rvec::rnorm_rvec(n = n_outcome,
                              mean = 0,
                              sd = sd,
                              n_draw = n_draw)
  outcome_true + noise
}

## HAS_TESTS
#' @export
forecast_outcome_obs_given_true.bage_datamod_overcount <- function(datamod,
                                                                   components_forecast,
                                                                   data_forecast,
                                                                   fitted,
                                                                   outcome_true,
                                                                   offset,
                                                                   has_newdata) {
  is_rate <- (components_forecast$term == "datamod"
    & components_forecast$component == "rate")
  rate <- components_forecast$.fitted[is_rate]
  level_rate <- components_forecast$level[is_rate]
  nms_by <- datamod$nms_by
  rate_arg <- datamod$rate_arg
  has_by <- length(nms_by) > 0L
  if (has_by) {
    nms_rate <- names(rate_arg)
    nms_by_rate <- intersect(nms_by, nms_rate)
    by_val_rate <- rate_arg[nms_by_rate]
    data <- data_forecast[nms_by_rate]
    nm_data <- if (has_newdata) "newdata" else "data"
    check_datamod_by_val(by_val = by_val_rate,
                         data = data,
                         nm_val = "rate",
                         nm_data = nm_data)
    key_rate <- Reduce(paste_dot, by_val_rate)
    key_data <- Reduce(paste_dot, data)
    key_rate <- intersect(key_rate, level_rate)
    i <- match(key_data, key_rate)
  }
  else {
    n_outcome <- length(outcome_true)
    i <- rep(1L, times = n_outcome)
  }
  rate <- rate[i]
  lambda <- rate * fitted * offset
  error <- rpois_guarded(lambda)
  outcome_true + error
}

## HAS_TESTS
#' @export
forecast_outcome_obs_given_true.bage_datamod_undercount <- function(datamod,
                                                                    components_forecast,
                                                                    data_forecast,
                                                                    fitted,
                                                                    outcome_true,
                                                                    offset,
                                                                    has_newdata) {
  is_prob <- (components_forecast$term == "datamod"
    & components_forecast$component == "prob")
  prob <- components_forecast$.fitted[is_prob]
  level_prob <- components_forecast$level[is_prob]
  prob_arg <- datamod$prob_arg
  nms_by <- datamod$nms_by
  has_by <- length(nms_by) > 0L
  if (has_by) {
    nms_prob <- names(prob_arg)
    nms_by_prob <- intersect(nms_by, nms_prob)
    by_val_prob <- prob_arg[nms_by_prob]
    data <- data_forecast[nms_by_prob]
    nm_data <- if (has_newdata) "newdata" else "data"
    check_datamod_by_val(by_val = by_val_prob,
                         data = data,
                         nm_val = "prob",
                         nm_data = nm_data)
    key_prob <- Reduce(paste_dot, by_val_prob)
    key_data <- Reduce(paste_dot, data)
    key_prob <- intersect(key_prob, level_prob)
    i <- match(key_data, key_prob)
  }
  else {
    n_outcome <- length(outcome_true)
    i <- rep(1L, times = n_outcome)
  }
  prob <- prob[i]
  rbinom_guarded(size = outcome_true,
                 prob = prob)
}


## 'get_datamod_transform_param' ----------------------------------------------

#' Get Function to Transform Draws for Data Model Parameters
#'
#' @param datamod Object of class 'bage_datamod'
#'
#' @returns A function
#'
#' @noRd
get_datamod_transform_param <- function(datamod) {
  UseMethod("get_datamod_transform_param")
}

## HAS_TESTS
#' @export
get_datamod_transform_param.bage_datamod_miscount <- function(datamod) {
  prob_mean <- datamod$prob_mean
  prob_disp <- datamod$prob_disp
  n_prob <- length(prob_mean)
  rate_mean <- datamod$rate_mean
  rate_disp <- datamod$rate_disp
  shape1 <- prob_mean / prob_disp
  shape2 <- (1 - prob_mean) / prob_disp
  shape <- 1 / rate_disp
  scale <- rate_mean * rate_disp
  eps <- 1e-12
  ans <- function(x) {
    if (!rvec::is_rvec(x))
      cli::cli_abort("Internal error: {.arg x} is not an rvec.")
    u <- rvec::pnorm_rvec(x)
    u <- rvec::if_else_rvec(u < eps, eps, u)
    u <- rvec::if_else_rvec(u > 1 - eps, 1 - eps, u)
    s <- seq_len(n_prob)
    prob <- rvec::qbeta_rvec(u[s], shape1 = shape1, shape2 = shape2)
    rate <- rvec::qgamma_rvec(u[-s], shape = shape, scale = scale)
    vctrs::vec_c(prob, rate)
  }
}

## HAS_TESTS
#' @export
get_datamod_transform_param.bage_datamod_overcount <- function(datamod) {
  rate_mean <- datamod$rate_mean
  rate_disp <- datamod$rate_disp
  shape <- 1 / rate_disp
  scale <- rate_mean * rate_disp
  eps <- 1e-12
  ans <- function(x) {
    if (!rvec::is_rvec(x))
      cli::cli_abort("Internal error: {.arg x} is not an rvec.")
    u <- rvec::pnorm_rvec(x)
    u <- rvec::if_else_rvec(u < eps, eps, u)
    u <- rvec::if_else_rvec(u > 1 - eps, 1 - eps, u)
    rvec::qgamma_rvec(u, shape = shape, scale = scale)
  }
}

## HAS_TESTS
#' @export
get_datamod_transform_param.bage_datamod_undercount <- function(datamod) {
  prob_mean <- datamod$prob_mean
  prob_disp <- datamod$prob_disp
  shape1 <- prob_mean / prob_disp
  shape2 <- (1 - prob_mean) / prob_disp
  eps <- 1e-12
  ans <- function(x) {
    if (!rvec::is_rvec(x))
      cli::cli_abort("Internal error: {.arg x} is not an rvec.")
    u <- rvec::pnorm_rvec(x)
    u <- rvec::if_else_rvec(u < eps, eps, u)
    u <- rvec::if_else_rvec(u > 1 - eps, 1 - eps, u)
    rvec::qbeta_rvec(u, shape1 = shape1, shape2 = shape2)
  }
}


## 'make_datamod_comp' --------------------------------------------------------

## HAS_TESTS
#' Make Character Vector with Comp for Data Model
#'
#' For use in 'components()'
#' 
#' @param datamod Object of class 'bage_datamod'
#'
#' @returns A character vector
#'
#' @noRd
make_datamod_comp <- function(datamod) {
  UseMethod("make_datamod_comp")
}

## HAS_TESTS
#' @export
make_datamod_comp.bage_datamod_miscount <- function(datamod) {
  prob_mean <- datamod$prob_mean
  rate_mean <- datamod$rate_mean
  n_prob <- length(prob_mean)
  n_rate <- length(rate_mean)
  rep(c("prob", "rate"), times = c(n_prob, n_rate))
}

## HAS_TESTS
#' @export
make_datamod_comp.bage_datamod_overcount <- function(datamod) {
  rate_mean <- datamod$rate_mean
  n_rate <- length(rate_mean)
  rep.int("rate", times = n_rate)
}

## HAS_TESTS
#' @export
make_datamod_comp.bage_datamod_undercount <- function(datamod) {
  prob_mean <- datamod$prob_mean
  n_prob <- length(prob_mean)
  rep.int("prob", times = n_prob)
}


## 'make_datamod_consts' ------------------------------------------------------

#' Make Vector to Hold Constants for Data Model
#'
#' @param datamod Object of class 'bage_datamod'
#'
#' @returns Double vector
#'
#' @noRd
make_datamod_consts <- function(datamod) {
  UseMethod("make_datamod_consts")
}

## HAS_TESTS
#' @export
make_datamod_consts.bage_datamod_exposure <- function(datamod) {
  datamod$disp
}

## HAS_TESTS
#' @export
make_datamod_consts.bage_datamod_miscount <- function(datamod) {
  prob_mean <- datamod$prob_mean
  prob_disp <- datamod$prob_disp
  rate_mean <- datamod$rate_mean
  rate_disp <- datamod$rate_disp
  c(prob_mean,
    prob_disp,
    rate_mean,
    rate_disp)
}

## HAS_TESTS
#' @export
make_datamod_consts.bage_datamod_noise <- function(datamod) {
  sd <- datamod$sd_sd
  outcome_sd <- datamod$outcome_sd
  has_outcome_sd <- !is.null(outcome_sd) ## normal model for outcomes
  if (has_outcome_sd)
    sd <- sd / outcome_sd
  sd
}

## HAS_TESTS
#' @export
make_datamod_consts.bage_datamod_overcount <- function(datamod) {
  rate_mean <- datamod$rate_mean
  rate_disp <- datamod$rate_disp
  c(rate_mean,
    rate_disp)
}

## HAS_TESTS
#' @export
make_datamod_consts.bage_datamod_undercount <- function(datamod) {
  prob_mean <- datamod$prob_mean
  prob_disp <- datamod$prob_disp
  c(prob_mean,
    prob_disp)
}


## 'make_level_datamod' -------------------------------------------------------

#' Make Level Vector for Data Model
#'
#' Only include levels for parameters
#' estimated in TMB
#'
#' @param datamod Object of class 'bage_datamod'
#'
#' @returns Double vector
#'
#' @noRd
make_level_datamod <- function(datamod) {
  UseMethod("make_level_datamod")
}

## HAS_TESTS
#' @export
make_level_datamod.bage_datamod_miscount <- function(datamod) {
  prob_levels <- datamod$prob_levels
  rate_levels <- datamod$rate_levels
  c(prob_levels, rate_levels)
}

## HAS_TESTS
#' @export
make_level_datamod.bage_datamod_overcount <- function(datamod) {
  datamod$rate_levels
}

## HAS_TESTS
#' @export
make_level_datamod.bage_datamod_undercount <- function(datamod) {
  datamod$prob_levels
}


## 'make_datamod_matrices' ----------------------------------------------------

#' Make List of Matrices for Data Model
#'
#' @param datamod Object of class 'bage_datamod'
#'
#' @returns Double vector
#'
#' @noRd
make_datamod_matrices <- function(datamod) {
  UseMethod("make_datamod_matrices")
}

## HAS_TESTS
#' @export
make_datamod_matrices.bage_datamod_exposure <- function(datamod) {
  disp <- datamod$disp_matrix_outcome
  list(disp)
}

## HAS_TESTS
#' @export
make_datamod_matrices.bage_datamod_miscount <- function(datamod) {
  prob <- datamod$prob_matrix_outcome
  rate <- datamod$rate_matrix_outcome
  list(prob, rate)
}

## HAS_TESTS
#' @export
make_datamod_matrices.bage_datamod_noise <- function(datamod) {
  sd <- datamod$sd_matrix_outcome
  list(sd)
}

## HAS_TESTS
#' @export
make_datamod_matrices.bage_datamod_overcount <- function(datamod) {
  rate <- datamod$rate_matrix_outcome
  list(rate)
}

## HAS_TESTS
#' @export
make_datamod_matrices.bage_datamod_undercount <- function(datamod) {
  prob <- datamod$prob_matrix_outcome
  list(prob)
}


## 'make_datamod_param' ----------------------------------------------------

#' Make Parameter Vector for Data Model
#'
#' @param datamod Object of class 'bage_datamod'
#'
#' @returns Double vector
#'
#' @noRd
make_datamod_param <- function(datamod) {
  UseMethod("make_datamod_param")
}

## HAS_TESTS
#' @export
make_datamod_param.bage_datamod_exposure <- function(datamod) {
  double()
}

## HAS_TESTS
#' @export
make_datamod_param.bage_datamod_miscount <- function(datamod) {
  n_prob <- length(datamod$prob_mean)
  n_rate <- length(datamod$rate_mean)
  rep(0, times = n_prob + n_rate)
}

## HAS_TESTS
#' @export
make_datamod_param.bage_datamod_noise <- function(datamod) {
  double()
}

## HAS_TESTS
#' @export
make_datamod_param.bage_datamod_overcount <- function(datamod) {
  n_rate <- length(datamod$rate_mean)
  rep(0, times = n_rate)
}

## HAS_TESTS
#' @export
make_datamod_param.bage_datamod_undercount <- function(datamod) {
  n_prob <- length(datamod$prob_mean)
  rep(0, times = n_prob)
}


## 'make_i_lik_part' ----------------------------------------------------------

## HAS_TESTS
#' @export
make_i_lik_part.bage_datamod_exposure <- function(x) {
  1000L
}

## HAS_TESTS
#' @export
make_i_lik_part.bage_datamod_miscount <- function(x) {
  2000L
}

## HAS_TESTS
#' @export
make_i_lik_part.bage_datamod_noise <- function(x) {
  3000L
}

## HAS_TESTS
#' @export
make_i_lik_part.bage_datamod_overcount <- function(x) {
  4000L
}

## HAS_TESTS
#' @export
make_i_lik_part.bage_datamod_undercount <- function(x) {
  5000L
}


## Helper functions for 'make_expected_obs' -----------------------------------

## HAS_TESTS
#' Make Modified Version of 'expected' to
#' Use with Exposure Data Model
#'
#' @param datamod Object of class "bage_datamod_overcount"
#' @param expected Rvec with expected value from system model
#'
#' @returns An rvec
#'
#' @noRd
make_expected_obs_exposure <- function(datamod,
                                       expected) {
  disp <- get_datamod_disp(datamod)
  numerator <- (3 * disp + 1) * expected
  denominator <- disp + 1
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
#' Use with Noise Data Model
#'
#' Noise data model assumes unbiased data
#' so simply return expected value
#'
#' @param expected Rvec with expected value from system model
#'
#' @returns An rvec
#'
#' @noRd
make_expected_obs_noise <- function(expected) {
  expected
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











