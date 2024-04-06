

#' Forecast Main Effects, Interactions, and Hyper-Parameters
#' Involving Time
#'
#' @param mod Object of class 'bage_mod'
#' @param components_est Tibble with results
#' of call to 'components'.
#' @param labels_forecast Character vector
#' with labels for future time periods
#'
#' @returns A tibble or NULL
#'
#' @noRd
forecast_components <- function(mod,
                                components_est,
                                labels_forecast) {
  nms_components_compose <- c("trend", "cyclical", "seasonal", "error")
  is_hyper <- components_est$component == "hyper"
  is_compose <- components_est$component %in% nms_components_compose
  is_effect <- components_est$component == "effect"
  hypers_est <- components_est[is_hyper, ]
  composes_est <- components_est[is_compose, ]
  effects_est <- components_est[is_effect, ]
  hypers_forecast <- forecast_hypers(mod = mod,
                                     hypers_est = hypers_est,
                                     labels_forecast = labels_forecast)
  composes_forecast <- forecast_composes(mod = mod,
                                         hypers_est = hypers_est,
                                         hypers_forecast = hypers_forecast,
                                         composes_est = composes_est,
                                         labels_forecast = labels_forecast)
  effects_forecast <- forecast_effects(mod = mod,
                                       hypers_est = hypers_est,
                                       hypers_forecast = hypers_forecast,
                                       composes_est = composes_est,
                                       composes_forecast = composes_forecast,
                                       effects_est = effects_est,
                                       labels_forecast = labels_forecast)
  ans <- vctrs::vec_rbind(hypers_forecast,
                          composes_forecast,
                          effects_forecast,
                          .name_repair = "universal_quiet")
  ans <- sort_components(components = ans,
                         mod = mod)
  ans
}


## HAS_TESTS 
#' Forecast Components of 'compose' Prior
#'
#' Returns NULL if no compose priors are present
#' in model.
#' 
#' @param mod Object of class 'bage_mod'
#' @param hypers_est Tibble with estimates
#' for hyper-parameters, obtained from
#' call to 'components'
#' @param hypers_forecast Tibble with estimates
#' for hyper-parameters, obtained from
#' call to 'components'
#' @param composes_est Estimates for
#' components of 'compose' priors
#' (eg "trend", "seasonal")
#' @param labels_forecast Character vector
#' with labels for future time periods
#'
#' @returns A tibble or NULL
#'
#' @noRd
forecast_composes <- function(mod,
                              hypers_est,
                              hypers_forecast,
                              composes_est,
                              labels_forecast) {
  has_composes_est <- !is.null(composes_est) && (nrow(composes_est) > 0L)
  if (!has_composes_est)
    return(NULL)
  priors <- mod$priors
  var_time <- mod$var_time
  nms_priors <- names(priors)
  n_prior <- length(nms_priors)
  levels_forecast <- make_levels_forecast(mod = mod,
                                          labels_forecast = labels_forecast)
  matrices_along_by_est <- choose_matrices_along_by(mod)
  matrices_along_by_forecast <- make_matrices_along_by_forecast(mod = mod,
                                                                labels_forecast = labels_forecast)
  has_hypers_est <- !is.null(hypers_est) && (nrow(hypers_est) > 0L)
  if (has_hypers_est) {
    hypers_est <- vctrs::vec_split(x = hypers_est,
                                   by = hypers_est["term"])
    nms_hypers_est <- hypers_est$key$term
    hypers_est <- hypers_est$val
  }
  else
    nms_hypers_est <- character()
  has_hypers_forecast <- !is.null(hypers_forecast) && (nrow(hypers_forecast) > 0L)
  if (has_hypers_forecast) {
    hypers_forecast <- vctrs::vec_split(x = hypers_forecast,
                                        by = hypers_forecast["term"])
    nms_hypers_forecast <- hypers_forecast$key$term
    hypers_forecast <- hypers_forecast$val
  }
  else
    nms_hypers_forecast <- character()
  composes_est <- vctrs::vec_split(x = composes_est,
                                   by = composes_est["term"])
  nms_composes_est <- composes_est$key$term
  composes_est <- composes_est$val
  ans <- rep(list(NULL), times = n_prior)
  for (i_prior in seq_len(n_prior)) {
    nm_prior <- nms_priors[[i_prior]]
    i_compose <- match(nm_prior, nms_composes_est, nomatch = 0L)
    if (i_compose > 0L) {
      nm_prior_split <- strsplit(nm_prior, split = ":")[[1L]]
      term_involves_time <- var_time %in% nm_prior_split
      if (term_involves_time) {
        prior <- priors[[i_prior]]
        i_hyper_est <- match(nm_prior, nms_hypers_est, nomatch = 0L)
        if (i_hyper_est > 0L)
          hyper_est <- hypers_est[[i_hyper_est]]
        else
          hyper_est <- NULL
        i_hyper_forecast <- match(nm_prior, nms_hypers_forecast, nomatch = 0L)
        if (i_hyper_forecast > 0L)
          hyper_forecast <- hypers_forecast[[i_hyper_forecast]]
        else
          hyper_forecast <- NULL
        compose_est <- composes_est[[i_compose]]
        compose_forecast <- forecast_compose(
          prior = prior,
          nm_prior = nm_prior,
          hyper_est = hyper_est,
          hyper_forecast = hyper_forecast,
          compose_est = compose_est,
          matrix_along_by_est = matrices_along_by_est[[i_prior]],
          matrix_along_by_forecast = matrices_along_by_forecast[[i_prior]],
          levels_forecast = levels_forecast[[i_prior]]
        )
        ans[[i_prior]] <- compose_forecast
      }
    }
  }
  is_null <- vapply(ans, is.null, TRUE)
  if (all(is_null))
    NULL
  else
    vctrs::vec_rbind(!!!ans, .name_repair = "universal_quiet")
}


## HAS_TESTS
#' Forecast Main Effects and Interactions
#' Involving Time
#'
#' Returns NULL if no terms involving timer are present
#' in the model.
#' 
#' @param mod Object of class 'bage_mod'
#' @param hypers_est Tibble with estimates
#' for hyper-parameters, obtained from
#' call to 'components'
#' @param hypers_forecast Tibble with estimates
#' for hyper-parameters, obtained from
#' call to 'components'
#' @param composes_est Estimates for
#' components of 'compose' priors
#' (eg "trend", "seasonal")
#' @param composes_forecast Forecasts for
#' components of 'compose' priors
#' (eg "trend", "seasonal")
#' @param effects_est Estimates for terms
#' @param labels_forecast Character vector
#' with labels for future time periods
#'
#' @returns A tibble or NULL
#'
#' @noRd
forecast_effects <- function(mod,
                             hypers_est,
                             hypers_forecast,
                             composes_est,
                             composes_forecast,
                             effects_est,
                             labels_forecast) {
  priors <- mod$priors
  var_time <- mod$var_time
  nms_priors <- names(priors)
  n_prior <- length(nms_priors)
  matrices_along_by_est <- choose_matrices_along_by(mod)
  matrices_along_by_forecast <- make_matrices_along_by_forecast(
    mod = mod,
    labels_forecast = labels_forecast
  )
  levels_forecast <- make_levels_forecast(mod = mod,
                                          labels_forecast = labels_forecast)
  has_hypers_est <- !is.null(hypers_est) && (nrow(hypers_est) > 0L)
  if (has_hypers_est) {
    hypers_est <- vctrs::vec_split(x = hypers_est,
                                   by = hypers_est["term"])
    nms_hypers_est <- hypers_est$key$term
    hypers_est <- hypers_est$val
  }
  else
    nms_hypers_est <- character()
  has_hypers_forecast <- !is.null(hypers_forecast) && (nrow(hypers_forecast) > 0L)
  if (has_hypers_forecast) {
    hypers_forecast <- vctrs::vec_split(x = hypers_forecast,
                                        by = hypers_forecast["term"])
    nms_hypers_forecast <- hypers_forecast$key$term
    hypers_forecast <- hypers_forecast$val
  }
  else
    nms_hypers_forecast <- character()
  has_composes_est <- !is.null(composes_est) && (nrow(composes_est) > 0L)
  if (has_composes_est) {
    composes_est <- vctrs::vec_split(x = composes_est,
                                     by = composes_est["term"])
    nms_composes_est <- composes_est$key$term
    composes_est <- composes_est$val
  }
  else
    nms_composes_est <- character()
  has_composes_forecast <- !is.null(composes_forecast) && (nrow(composes_forecast) > 0L)
  if (has_composes_forecast) {
    composes_forecast <- vctrs::vec_split(x = composes_forecast,
                                          by = composes_forecast["term"])
    nms_composes_forecast <- composes_forecast$key$term
    composes_forecast <- composes_forecast$val
  }
  else
    nms_composes_forecast <- character()
  effects_est <- vctrs::vec_split(x = effects_est,
                                  by = effects_est["term"])
  nms_effects_est <- effects_est$key$term
  effects_est <- effects_est$val
  ans <- rep(list(NULL), times = n_prior)
  for (i_prior in seq_len(n_prior)) {
    nm_prior <- nms_priors[[i_prior]]
    nm_prior_split <- strsplit(nm_prior, split = ":")[[1L]]
    term_involves_time <- var_time %in% nm_prior_split
    if (term_involves_time) {
      prior <- priors[[i_prior]]
      i_hyper_est <- match(nm_prior, nms_hypers_est, nomatch = 0L)
      if (i_hyper_est > 0L)
        hyper_est <- hypers_est[[i_hyper_est]]
      else
        hyper_est <- NULL
      i_hyper_forecast <- match(nm_prior, nms_hypers_forecast, nomatch = 0L)
      if (i_hyper_forecast > 0L)
        hyper_forecast <- hypers_forecast[[i_hyper_forecast]]
      else
        hyper_forecast <- NULL
      i_compose_est <- match(nm_prior, nms_composes_est, nomatch = 0L)
      if (i_compose_est > 0L)
        compose_est <- composes_est[[i_compose_est]]
      else
        compose_est <- NULL
      i_compose_forecast <- match(nm_prior, nms_composes_forecast, nomatch = 0L)
      if (i_compose_forecast > 0L)
        compose_forecast <- composes_forecast[[i_compose_forecast]]
      else
        compose_forecast <- NULL
      effect_forecast <- forecast_effect(
        prior = prior,
        nm_prior = nm_prior,
        hyper_est = hyper_est,
        hyper_forecast = hyper_forecast,
        compose_est = compose_est,
        compose_forecast = compose_forecast,
        effect_est = effects_est[[i_prior]],
        matrix_along_by_est = matrices_along_by_est[[i_prior]],
        matrix_along_by_forecast = matrices_along_by_forecast[[i_prior]],
        levels_forecast = levels_forecast[[i_prior]]
      )
      ans[[i_prior]] <- effect_forecast
    }
  }
  is_null <- vapply(ans, is.null, TRUE)
  if (all(is_null))
    NULL
  else
    vctrs::vec_rbind(!!!ans, .name_repair = "universal_quiet")
}



## HAS_TESTS 
#' Forecast Hyper-Parameters for Main Effects
#' and Interactions
#'
#' Returns NULL if no hyper-parameters are forecast.
#' 
#' @param mod Object of class 'bage_mod'
#' @param hypers_est Tibble with estimates
#' for hyper-parameters, obtained from
#' call to 'components'
#' @param labels_forecast Character vector
#' with labels for future time periods
#'
#' @returns A tibble or NULL
#'
#' @noRd
forecast_hypers <- function(mod,
                            hypers_est,
                            labels_forecast) {
  has_hypers_est <- !is.null(hypers_est) && (nrow(hypers_est) > 0L)
  if (!has_hypers_est)
    return(NULL)
  priors <- mod$priors
  var_time <- mod$var_time
  nms_priors <- names(priors)
  n_prior <- length(nms_priors)
  levels_forecast <- make_levels_forecast(mod = mod,
                                          labels_forecast = labels_forecast)
  hypers_est <- vctrs::vec_split(x = hypers_est,
                                 by = hypers_est["term"])
  nms_hypers <- hypers_est$key$term
  hypers_est <- hypers_est$val
  ans <- rep(list(NULL), times = n_prior)
  for (i_prior in seq_len(n_prior)) {
    nm_prior <- nms_priors[[i_prior]]
    i_hyper <- match(nm_prior, nms_hypers, nomatch = 0L)
    if (i_hyper > 0L) {
      nm_prior_split <- strsplit(nm_prior, split = ":")[[1L]]
      term_involves_time <- var_time %in% nm_prior_split
      if (term_involves_time) {
        prior <- priors[[i_prior]]
        hyper_est <- hypers_est[[i_hyper]]
        hyper_forecast <- forecast_hyper(prior = prior,
                                         hyper_est = hyper_est,
                                         levels_forecast = levels_forecast[[i_prior]])
        ans[[i_hyper]] <- hyper_forecast
      }
    }
  }
  is_null <- vapply(ans, is.null, TRUE)
  if (all(is_null))
    NULL
  else
    vctrs::vec_rbind(!!!ans, .name_repair = "universal_quiet")
}
