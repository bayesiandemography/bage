
forecast_components <- function(mod,
                                components_est,
                                labels) {
  compose_hyper <- c("hyper", "trend", "cyclical", "seasonal", "error")
  priors <- object@priors
  is_effect <- components_est$component == "effect"
  is_compose_hyper <- components_est$component %in% compose_hyper
  effect_est <- components_est[is_effect, c("term", "level", ".fitted")]
  compose_hypers_est <- components_est[is_compose_hyper, ]
  compose_hypers_forecast <- forecast_compose_hypers(priors = priors,
                                                     compose_hypers_est = compose_hypers_est,
                                                     labels = labels)
  effects_forecast <- forecast_effects(priors = priors,
                                       effects_est = effect_est,
                                       compose_hypers_forecast = compose_hypers_forecast,
                                       labels = labels)
  ans <- rbind(compose_hypers_forecast, effects_forecast)
  if (has_disp(object)) {
    is_disp <- components_est$term == "disp"
    disp <- components_est[is_disp, , drop = FALSE]
    ans <- rbind(ans, disp)
  }
  ans <- sort_components(components = ans,
                         mod = object)
  ans
}


forecast_compose_hypers <- function(mod,
                                    compose_hypers_est,
                                    labels) {
  priors <- mod$priors
  var_time <- mod$var_time
  nms_priors <- names(priors)
  n_prior <- length(nms_priors)
  compose_hypers_est <- vctrs::vec_split(x = compose_hypers_est[c("component", "level", ".fitted")],
                                         by = compose_hypers_est["term"])
  nms_compose_hypers <- compose_hypers_est$key$term
  compose_hypers_est <- compose_hypers_est$val
  for (i_prior in seq_len(n_prior)) {
    nm_prior <- nms_priors[[i_prior]]
    i_compose_hyper <- match(nm_prior, nms_compose_hypers, nomatch = 0L)
    if (i_compose_hyper > 0L) {
      nm_prior_split <- split(nm_prior, split = ":")[[1L]]
      term_involves_time <- var_time %in% nm_prior_split
      if (term_involves_time) {
        prior <- priors[[i_prior]]
        compose_hyper_est <- compose_hypers_est[[i_term]]
        compose_hyper_forecast <- forecast_compose_hyper(prior = prior,
                                                         compose_hyper_est = compose_hyper_est,
                                                         labels = labels)
        compose_hypers_est[[i_prior]] <- compose_hyper_forecast
      }
    }
  }
  ans <- do.call(rbind, compose_hypers_est)
  ans
}

forecast_effect <- function(mod,
                            effects_est,
                            compose_hypers_forecast,
                            labels) {
  priors <- mod$priors
  var_time <- mod$var_time
  nms_priors <- names(priors)
  n_prior <- length(nms_priors)
  x_effects <- effects_est[c("level", ".fitted")]
  x_compose_hypers <- compose_hypers_forecast[c("component", "level", ".fitted")]
  by_effects <- effects_est["term"]
  by_compose_hypers <- compose_hypers_forecast["term"]
  effect_est <- vctrs::vec_split(x = x_effects, by = by_effects)
  compose_hypers_forecast <- vctrs::vec_split(x = x_compose_hypers, by = by_compose_hypers)
  effects_est <- effects_est$val
  nms_compose_hypers <- compose_hypers_forecast$key$term
  compose_hypers_forecast <- compose_hypers_forecast$val
  for (i_prior in seq_len(n_prior)) {
    nm_prior <- nms_priors[[i_prior]]
    nm_prior_split <- split(nm_prior, split = ":")[[1L]]
    term_involves_time <- var_time %in% nm_prior_split
    if (term_involves_time) {
      prior <- priors[[i_prior]]
      effect_est <- effect_est[[i_prior]]$.fitted
      i_compose_hypers <- match(nm_prior, nms_compose_hypers, nomatch = 0L)
      if (i_compose_hypers > 0L)
        compose_hyper_forecast <- compose_hypers_forecast[[i_compose_hypers]]
      else
        compose_hyper_forecast <- NULL
      effect_forecast <- forecast_effect(prior = prior,
                                         effect_est = effect_est,
                                         compose_hyper_foecast = compose_hyper_forecast,
                                         labels = labels)
      effects_est[[i_prior]] <- effect_forecast
    }
  }
  ans <- do.call(rbind, effect_est)
  ans
}

                                         
  
  
  
  
                                     
  
  
