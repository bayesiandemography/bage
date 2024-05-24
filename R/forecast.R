
#' Forecast Contents of 'components'
#'
#' @param mod Object of class 'bage_mod'
#' @param components_est Tibble with results
#' of call to 'components'.
#' @param labels_forecast Vector
#' with labels for future time periods
#' @param include_non_time_varying Whether to
#' include forecasts for non-time-varying
#' parameters in results.
#'
#' @returns A tibble or NULL
#'
#' @noRd
forecast_components <- function(mod,
                                components_est,
                                labels_forecast) {
  is_hyper <- components_est$component == "hyper"
  is_effect <- components_est$component == "effect"
  hypers_est <- components_est[is_hyper, ]
  effects_est <- components_est[is_effect, ]
  hypers_forecast <- forecast_hypers(mod = mod,
                                     hypers_est = hypers_est,
                                     labels_forecast = labels_forecast)
  effects_forecast <- forecast_effects(mod = mod,
                                       hypers_est = hypers_est,
                                       hypers_forecast = hypers_forecast,
                                       effects_est = effects_est,
                                       labels_forecast = labels_forecast)
  time_varying <- vctrs::vec_rbind(hypers_forecast,
                                   effects_forecast,
                                   .name_repair = "universal_quiet")
  is_time_varying <- make_is_time_varying(term = components_est$term,
                                          level = components_est$level,
                                          var_time = mod$var_time)
  non_time_varying <- components_est[!is_time_varying, ]
  ans <- vctrs::vec_rbind(time_varying,
                          non_time_varying,
                          .name_repair = "universal_quiet")
  ans <- sort_components(components = ans,
                         mod = mod)
  ans
}


## HAS_TESTS
#' Forecast Time-Varying Main Effects and Interactions
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
#' @param effects_est Estimates for terms
#' @param labels_forecast Vector
#' with labels for future time periods
#'
#' @returns A tibble or NULL
#'
#' @noRd
forecast_effects <- function(mod,
                             hypers_est,
                             hypers_forecast,
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
      effect_forecast <- forecast_effect(
        prior = prior,
        nm_prior = nm_prior,
        hyper_est = hyper_est,
        hyper_forecast = hyper_forecast,
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
    cli::cli_abort("Internal error: No effects forecasted.") ## nocov
  else
    vctrs::vec_rbind(!!!ans)
}



## HAS_TESTS 
#' Forecast Time-Varying Hyper-Parameters
#' for Main Effects and Interactions
#'
#' Returns NULL if no hyper-parameters are forecast.
#' 
#' @param mod Object of class 'bage_mod'
#' @param hypers_est Tibble with estimates
#' for hyper-parameters, obtained from
#' call to 'components'
#' @param labels_forecast Vector
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
    vctrs::vec_rbind(!!!ans)  ## nocov - no priors currently have time-varying hyper
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
