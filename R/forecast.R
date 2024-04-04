
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
                                     hyper_est = hyper_est,
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
  ans <- rbind(hypers_forecast, composes_forecast, effects_forecast)
  if (has_disp(object)) {
    is_disp <- components_est$term == "disp"
    disp <- components_est[is_disp, , drop = FALSE]
    ans <- rbind(ans, disp)
  }
  ans <- sort_components(components = ans,
                         mod = object)
  ans
}


forecast_hypers <- function(mod,
                            hypers_est,
                            labels_forecast) {
  priors <- mod$priors
  var_time <- mod$var_time
  nms_priors <- names(priors)
  n_prior <- length(nms_priors)
  levels_forecast <- make_levels_forecast(mod = mod,
                                          labels_forecast = labels_forecast)
  hypers_est <- vctrs::vec_split(x = hypers_est,
                                 by = hypers_est["term"])
  nms_hypers <- hypers_est$key$term
  ans <- rep(list(NULL), times = n_prior)
  names(ans) <- nms_ans
  for (i_prior in seq_len(n_prior)) {
    nm_prior <- nms_priors[[i_prior]]
    i_hyper <- match(nm_prior, nms_hypers, nomatch = 0L)
    if (i_hyper > 0L) {
      nm_prior_split <- split(nm_prior, split = ":")[[1L]]
      term_involves_time <- var_time %in% nm_prior_split
      if (term_involves_time) {
        prior <- priors[[i_prior]]
        hyper_est <- hypers_est[[i_hyper]]
        hyper_forecast <- forecast_hyper(prior = prior,
                                         hyper_est = hyper_est,
                                         levels_forecast = levels_forecast)
        ans[[i_hyper]] <- hyper_forecast
      }
    }
  }
  ans <- do.call(rbind, ans)
  ans
}

forecast_composes <- function(mod,
                              hypers_est,
                              hypers_forecast,
                              composes_est,
                              levels_forecast) {
  priors <- mod$priors
  var_time <- mod$var_time
  nms_priors <- names(priors)
  n_prior <- length(nms_priors)
  levels_forecast <- make_levels_forecast(mod = mod,
                                          labels_forecast = labels_forecast)
  hypers_est <- vctrs::vec_split(x = hypers_est,
                                 by = hypers_est["term"])
  hypers_forecast <- vctrs::vec_split(x = hypers_forecast,
                                      by = hypers_forecast["term"])
  composes_est <- vctrs::vec_split(x = composes_est,
                                   by = composes_est["term"])
  nms_hypers_est <- hypers_est$key$term
  nms_hypers_forecast <- hypers_forecast$key$term
  nms_composes_est <- composes_est$key$term
  hypers_est <- hypers_est$val
  hypers_forecast <- hypers_forecast$val
  composes_est <- composes_est$val
  ans <- rep(list(NULL), times = n_prior)
  names(ans) <- nms_ans
  for (i_prior in seq_len(n_prior)) {
    nm_prior <- nms_priors[[i_prior]]
    i_compose <- match(nm_prior, nms_composes_est, nomatch = 0L)
    if (i_compose > 0L) {
      nm_prior_split <- split(nm_prior, split = ":")[[1L]]
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
        compose_forecast <- forecast_compose(prior = prior,
                                             hyper_est = hyper_est,
                                             hyper_forecast = hyper_forecast,
                                             compose_est = compose_est,
                                             levels_forecast = levels_forecast)
        ans[[i_prior]] <- compose_forecast
      }
    }
  }
  ans <- do.call(rbind, ans)
  ans
}


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
  levels_forecast <- make_levels_forecast(mod = mod,
                                          labels_forecast = labels_forecast)
  hypers_est <- vctrs::vec_split(x = hypers_est,
                                 by = hypers_est["term"])
  hypers_forecast <- vctrs::vec_split(x = hypers_forecast,
                                      by = hypers_forecast["term"])
  composes_est <- vctrs::vec_split(x = composes_est,
                                   by = composes_est["term"])
  composes_forecast <- vctrs::vec_split(x = composes_forecast,
                                        by = composes_forecast["term"])
  effect_est <- vctrs::vec_split(x = effects_est,
                                 by = effects_est["term"])
  nms_hypers_est <- hypers_est$key$term
  nms_hypers_forecast <- hypers_forecast$key$term
  nms_composes_est <- composes_est$key$term
  nms_composes_forecast <- composes_forecast$key$term
  nms_effect <- effects_est$key$term
  hypers_est <- hypers_est$val
  hypers_forecast <- hypers_forecast$val
  composes_est <- composes_est$val
  composes_forecast <- composes_forecast$val
  effects_est <- effects_est$val
  matrices_along_by_est <- choose_matrices_along_by(mod)
  matrices_along_by_forecast <- make_matrices_along_by_forecast(mod = mod,
                                                                labels_forecast = labels_forecast)
  ans <- rep(list(NULL), times = n_prior)
  names(ans) <- nms_ans
  for (i_prior in seq_len(n_prior)) {
    nm_prior <- nms_priors[[i_prior]]
    nm_prior_split <- strsplit(nm_prior, split = ":")[[1L]]
    term_involves_time <- var_time %in% nm_prior_split
    if (term_involves_time) {
      prior <- priors[[i_prior]]
      i_hyper <- match(nm_prior, nms_hypers, nomatch = 0L)
      if (i_hyper > 0L)
        hyper_forecast <- hypers_forecast[[i_hyper]]
      else
        hyper_forecast <- NULL
      i_compose <- match(nm_prior, nms_composes, nomatch = 0L)
      if (i_compose > 0L)
        compose_forecast <- composes_forecast[[i_compose]]
      else
        compose_forecast <- NULL
      effect_est <- effects_est[[i_prior]]
      matrix_along_by_est <- matrices_along_by_est[[i_prior]]
      matrix_along_by_forecast <- matrices_along_by_forecast[[i_prior]]
      effect_forecast <- forecast_effect(prior = prior,
                                         nm_prior = nm_prior,
                                         hyper_est = hyper_est,
                                         hyper_forecast = hyper_forecast,
                                         compose_est = compose_est,
                                         compose_forecast = compose_forecast,
                                         effect_est = effect_est,
                                         matrix_along_by_est = matrix_along_by_est,
                                         matrix_along_by_forecast = matrix_along_by_forecast,
                                         levels_forecast = levels_forecast)
      ans[[i_prior]] <- effect_forecast
    }
  }
  ans <- do.call(rbind, ans)
  ans
}

