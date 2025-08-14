
## HAS_TESTS
#' @export
draw_vals_augment_fitted.bage_mod <- function(mod) {
  ## extract values
  data <- mod$data
  dimnames_terms <- mod$dimnames_terms
  outcome <- mod$outcome
  offset <- mod$offset
  seed_augment <- mod$seed_augment
  confidential <- mod$confidential
  datamod <- mod$datamod
  nm_distn <- nm_distn(mod)
  disp <- get_disp(mod)
  inv_transform <- get_fun_inv_transform(mod)
  ## prepare inputs
  components <- components(mod)
  linpred <- make_linpred_from_components(mod = mod,
                                          components = components,
                                          data = data,
                                          dimnames_terms = dimnames_terms)
  expected <- inv_transform(linpred)
  has_confidential <- has_confidential(mod)
  has_datamod <- has_datamod(mod)
  has_datamod_outcome <- has_datamod_outcome(mod)
  has_datamod_offset <- has_datamod && !has_datamod_outcome
  has_missing_outcome <- anyNA(outcome)
  has_disp <- has_disp(mod)
  ## prepare seeds
  seed_restore <- make_seed() ## create randomly-generated seed
  set.seed(seed_augment) ## set pre-determined seed
  ## draw values for outcome and offset, where necessary
  if (has_confidential) {
    expected_obs <- make_expected_obs(mod = mod, expected = expected)
    disp_obs <- make_disp_obs(mod = mod, disp = disp) ## vector
    outcome <- draw_outcome_obs_given_conf(confidential = confidential,
                                           datamod = datamod,
                                           nm_distn = nm_distn,
                                           outcome = outcome,
                                           offset = offset,
                                           expected = expected_obs,
                                           disp = disp_obs)
  }
  if (has_datamod_outcome)
    outcome <- draw_outcome_true_given_obs(datamod = datamod,
                                           nm_distn = nm_distn,
                                           components = components,
                                           outcome = outcome,
                                           offset = offset,
                                           expected = expected,
                                           disp = disp)
  if (has_datamod_offset)
    offset <- draw_offset_true_given_obs(datamod = datamod,
                                         nm_distn = nm_distn,
                                         outcome = outcome,
                                         offset = offset,
                                         expected = expected)
  if (has_missing_outcome)
    outcome <- impute_outcome_true(nm_distn = nm_distn,
                                   outcome = outcome,
                                   offset = offset,
                                   expected = expected,
                                   disp = disp)
  ## derive '.fitted' and, in models with dispersion, '.expected'
  ans <- mod$data
  if (has_disp) {
    ans$.fitted <- draw_fitted_given_outcome(mod = mod,
                                             outcome = outcome,
                                             offset = offset,
                                             expected = expected,
                                             disp = disp)
    ans$.expected <- expected
  }
  else
    ans$.fitted <- expected
  ## restore seed
  set.seed(seed_restore) ## set randomly-generated seed, to restore randomness
  ## assemble and return answer
  has_modified_outcome <- (has_confidential
    || has_datamod_outcome
    || has_missing_outcome)
  if (has_modified_outcome) {
    nm_outcome_data <- get_nm_outcome_data(mod)
    nm_outcome_data_true <- paste0(".", nm_outcome_data)
    cli::cli_alert_info(paste("Adding variable {.var {nm_outcome_data_true}}",
                              "with true values for ",
                              "{.var {nm_outcome_data}}."))
    ans <- insert_after(df = ans,
                        nm_after = nm_outcome_data,
                        x = outcome,
                        nm_x = nm_outcome_data_true)
  }
  if (has_datamod_offset) {
    nm_offset_data <- get_nm_offset_data(mod)
    nm_offset_data_true <- paste0(".", nm_offset_data)
    cli::cli_alert_info(paste("Adding variable {.var {nm_offset_data_true}}",
                              "with true values for ",
                              "{.var {nm_offset_data}}."))
    ans <- insert_after(df = ans,
                        nm_after = nm_offset_data,
                        x = offset,
                        nm_x = nm_offset_data_true)
  }    
  ans
}




## HAS_TESTS
#' Assume no confidentialization, and no
#' data model for offset
#' @export
draw_vals_augment_fitted.bage_mod_norm <- function(mod) {
  ## extract values
  outcome <- mod$outcome
  offset <- mod$offset
  seed_augment <- mod$seed_augment
  datamod <- mod$datamod
  nm_distn <- nm_distn(mod)
  disp <- get_disp(mod)
  orig_scale_linpred <- get_fun_orig_scale_linpred(mod)
  orig_scale_disp <- get_fun_orig_scale_disp(mod)
  orig_scale_offset <- get_fun_orig_scale_offset(mod)
  ## prepare inputs
  linpred <- make_linpred_from_stored_draws(mod = mod, point = FALSE)
  disp <- get_disp(mod)
  has_datamod_outcome <- has_datamod(mod)
  has_missing_outcome <- anyNA(outcome)
  expected_scaled <- scale_linpred(linpred)
  disp_scaled <- scale_disp(disp)
  offset_scaled <- scale_offset(offset)
  ## prepare seeds
  seed_restore <- make_seed() ## create randomly-generated seed
  set.seed(seed_augment) ## set pre-determined seed
  ## draw values for outcome, where necessary
  if (has_datamod_outcome)
    outcome <- draw_outcome_true_given_obs(datamod = datamod,
                                           nm_distn = nm_distn,
                                           components = NULL,
                                           outcome = outcome,
                                           offset = offset_scaled,
                                           expected = expected_scaled,
                                           disp = disp_scaled)
  if (has_missing_outcome)
    outcome <- impute_outcome_true(nm_distn = nm_distn,
                                   outcome = outcome, ## scaled
                                   offset = offset_scaled,
                                   expected = expected_scaled,
                                   disp = disp_scaled)
  ## restore seed
  set.seed(seed_restore) ## set randomly-generated seed, to restore randomness
  ## derive '.fitted'
  ans$.fitted <- expected_scaled
  ## assemble and return answer
  ans <- mod$data
  has_modified_outcome <- (has_datamod_outcome || has_missing_outcome)
  if (has_modified_outcome) {
    nm_outcome_data <- get_nm_outcome_data(mod)
    nm_outcome_data_true <- paste0(".", nm_outcome_data)
    ans <- insert_after(df = ans,
                        nm_after = nm_outcome_data,
                        x = outcome,
                        nm_x = nm_outcome_data_true)
  }
  ans
}


## HAS_TESTS
#' @export
draw_vals_augment_unfitted.bage_mod <- function(mod) {
  ## extract values
  data <- mod$data
  offset <- mod$offset
  seed_augment <- mod$seed_augment
  confidential <- mod$confidential
  datamod <- mod$datamod
  dimnames_terms <- mod$dimnames_terms
  n_draw <- mod$n_draw
  nm_distn <- nm_distn(mod)
  disp <- get_disp(mod)
  inv_transform <- get_fun_inv_transform(mod)
  nm_outcome_data <- get_nm_outcome_data(mod)
  ## prepare inputs
  has_confidential <- has_confidential(mod)
  has_datamod <- has_datamod(mod)
  has_datamod_outcome <- has_datamod_outcome(mod)
  has_datamod_offset <- has_datamod && !has_datamod_outcome
  has_disp <- has_disp(mod)
  ans <- mod$data
  ## prepare seeds
  seed_restore <- make_seed() ## create randomly-generated seed
  set.seed(seed_augment) ## set pre-determined seed
  ## draw values for components
  components <- draw_vals_components_unfitted(mod = mod,
                                              n_sim = n_draw)
  ## derive expected values
  linpred <- make_linpred_from_components(mod = mod,
                                          components = components,
                                          data = data,
                                          dimnames_terms = dimnames_terms)
  expected <- inv_transform(linpred)
  ## obtain values for 'fitted', and, in models with dispersion, 'disp'
  if (has_disp) {
    is_disp <- vals_components$component == "disp"
    disp <- vals_components$.fitted[is_disp]
    fitted <- draw_fitted(nm_distn = nm_distn,
                          expected = expected,
                          disp = disp)
  }
  else
    fitted <- expected
  ## obtain values for true outcome
  outcome <- draw_outcome_true(nm_distn = nm_distn,
                               offset = offset,
                               fitted = fitted,
                               disp = NULL)
  ## if reported outcome different from true outcome,
  ## then record true and reported outcomes;
  ## otherwise just record true outcome
  is_reported_outcome_not_true <- has_datamod_outcome || has_confidential
  if (is_reported_outcome_not_true) {
    outcome_true <- outcome
    if (has_datamod_outcome)
      outcome <- draw_outcome_obs(datamod = datamod,
                                  outcome_true = outcome)
    if (has_confidential)
      outcome <- draw_outcome_confidential(confidential = confidential,
                                           outcome_obs = outcome)
    if (!quiet)
      cli::cli_alert_info(paste("Overwriting existing values for",
                                "{.var {nm_outcome_data}}."))
    ans[[nm_outcome_data]] <- outcome
    nm_outcome_data_true <- paste0(".", nm_outcome_data)
    if (!quiet)
      cli::cli_alert_info(paste("Adding variable {.var {nm_outcome_data_true}}",
                                "with true values for ",
                                "{.var {nm_outcome_data}}."))
    ans <- insert_after(df = ans,
                        nm_after = nm_outcome_data,
                        x = outcome_true,
                        nm_x = nm_outcome_data_true)
  }
  else {
    if (!quiet)
      cli::cli_alert_info(paste("Overwriting existing values for",
                                "{.var {nm_outcome_data}}."))
    ans[[nm_outcome_data]] <- outcome
  }
  ## if reported offset different from
  ## true offset, then record true and
  ## observed offsets
  if (has_datamod_offset) {
    nm_offset_data <- get_nm_offset_data(mod)
    offset_obs <- draw_offset_obs(datamod = datamod,
                                  offset_true = offset)
    if (!quiet)
      cli::cli_alert_info(paste("Overwriting existing values for",
                                "{.var {nm_offset_data}}."))
    ans[[nm_offset_data]] <- offset_obs
    nm_offset_data_true <- paste0(".", nm_offset_data)
    if (!quiet)
      cli::cli_alert_info(paste("Adding variable {.var {nm_offset_data_true}}",
                                "with true values for ",
                                "{.var {nm_outcome_data}}."))
    ans <- insert_after(df = ans,
                        nm_after = nm_offset_data,
                        x = offset,
                        nm_x = nm_offset_data_true)
  }
  ## restore seed
  set.seed(seed_restore) ## set randomly-generated seed, to restore randomness
  ## add '.observed', '.fitted', and, in models
  ## with dispersion, '.expected', and return
  ans$.observed <- num_observed / denom_observed
  ans$.fitted <- fitted
  if (has_disp)
    ans$.expected <- expected
  ans
}



## HAS_TESTS
#' Assumes that no confidentialization procedures,
#' and no data models for offsets exist for Normal models
#' @export
draw_vals_augment_unfitted.bage_mod_norm <- function(mod) {
  ## extract values
  data <- mod$data
  offset <- mod$offset
  seed_augment <- mod$seed_augment
  datamod <- mod$datamod
  dimnames_terms <- mod$dimnames_terms
  n_draw <- mod$n_draw
  nm_distn <- nm_distn(mod)
  disp <- get_disp(mod)
  orig_scale_linpred <- get_fun_orig_scale_linpred(mod)
  orig_scale_disp <- get_fun_orig_scale_disp(mod)
  orig_scale_offset <- get_fun_orig_scale_offset(mod)
  nm_outcome_data <- get_nm_outcome_data(mod)
  ## prepare inputs
  has_datamod <- has_datamod(mod)
  ans <- mod$data
  ## prepare seeds
  seed_restore <- make_seed() ## create randomly-generated seed
  set.seed(seed_augment) ## set pre-determined seed
  ## draw values for components
  components <- draw_vals_components_unfitted(mod = mod,
                                              n_sim = n_draw)
  ## derive linpred
  linpred <- make_linpred_from_components(mod = mod,
                                          components = components,
                                          data = data,
                                          dimnames_terms = dimnames_terms)
  ## obtain values for 'fitted', 'disp', and 'offset' on original scale
  fitted_orig_scale <- orig_scale_linpred(linpred)
  is_disp <- vals_components$component == "disp"
  disp <- vals_components$.fitted[is_disp]
  disp_orig_scale <- orig_scale_disp(disp)
  offset_orig_scale <- orig_scale_offset(offset)
  ## obtain values for true outcome
  outcome <- draw_outcome_true(nm_distn = nm_distn,
                               offset = offset_orig_scale,
                               fitted = fitted_orig_scale,
                               disp = disp_orig_scale)
  ## if model has datamodel for outcome,
  ## then record true and reported outcomes;
  ## otherwise just record true outcome
  if (has_datamod_outcome) {
    outcome_obs <- draw_outcome_obs(datamod = datamod,
                                    outcome_true = outcome)
    if (!quiet)
      cli::cli_alert_info(paste("Overwriting existing values for",
                                "{.var {nm_outcome_data}}."))
    ans[[nm_outcome_data]] <- outcome_obs
    nm_outcome_data_true <- paste0(".", nm_outcome_data)
    if (!quiet)
      cli::cli_alert_info(paste("Adding variable {.var {nm_outcome_data_true}}",
                                "with true values for ",
                                "{.var {nm_outcome_data}}."))
    ans <- insert_after(df = ans,
                        nm_after = nm_outcome_data,
                        x = outcome,
                        nm_x = nm_outcome_data_true)
  }
  else {
    if (!quiet)
      cli::cli_alert_info(paste("Overwriting existing values for",
                                "{.var {nm_outcome_data}}."))
    ans[[nm_outcome_data]] <- outcome
  }
  ## restore seed
  set.seed(seed_restore) ## set randomly-generated seed, to restore randomness
  ## add '.fitted' and return answer
  ans$.fitted <- fitted_orig_scale
  ans
}



## 'make_expected_obs' --------------------------------------------------------

#' Make Expected Values Used in Model for Observed Outcome
#'
#' Make expected value for rate/probability/mean in
#' model of observed outcome that takes
#' account of measurement errors
#'
#' @param mod Object of class 'bage_mod'
#' @param components Data frame with components
#' @param expected Expected value for rate/prob/mean that does
#' not account for measurement errors. An rvec.
#'
#' @returns An rvec
#'
#' @noRd
make_expected_obs <- function(mod, components, expected) {
  UseMethod("make_expected_obs")
}


make_expected_obs.bage_mod_pois <- function(mod, components, exposure) {
  datamod <- mod$datamod
  if (is.null(datamod)) {
    ans <- exposure
  }
  else {
    if (inherits(datamod, "bage_datamod_exposure")) {
      ans <- make_expected_obs_exposure(datamod = datamod,
                                        expected = expected)
    }
    else if (inherits(datamod, "bage_datamod_miscount")) {
      ans <- make_expected_obs_miscount(datamod = datamod,
                                        components = components,
                                        expected = expected)
    }
    else if (inherits(datamod, "bage_datamod_overcount")) {
      ans <- make_expected_obs_overcount(datamod = datamod,
                                         components = components,
                                         expected = expected)
    }
    else if (inherits(datamod, "bage_datamod_undercount")) {
      ans <- make_expected_obs_undercount(datamod = datamod,
                                          components = components,
                                          expected = expected)
    }
    else {
      cli::cli_abort(paste("Internal error: Can't handle data model",
                           "with class {.cls {class(datamod)}}"))
    }
  }
  ans
}


make_expected_obs.bage_mod_binom <- function(mod, exposure) {
  datamod <- mod$datamod
  if (is.null(datamod)) {
    exposure
  }
  else if (inherits(datamod, "bage_datamod_undercount")) {
    components <- components(mod)
    make_expected_obs_undercount(datamod = datamod,
                                 components = components,
                                 expected = expected)
  }
  else {
    cli::cli_abort(paste("Internal error: Can't handle data model",
                         "with class {.cls {class(datamod)}}"))
  }
}

## not needed for bage_mod_norm





## 'make_disp_obs' ------------------------------------------------------------

#' Make Dispersion Used in Model for Observed Outcome
#'
#' Make dispersion in
#' model of observed outcome that takes
#' account of measurement errors
#'
#' @param datamod Object of class 'bage_datamod'
#' @param disp Dispersion (for system or data model). An rvec.
#'
#' @returns An rvec
#'
#' @noRd
make_disp_obs <- function(mod, components, disp) {
  UseMethod("make_disp_obs")
}

make_disp_obs.bage_mod_pois <- function(mod, components, disp) {
  outcome <- mod$outcome
  datamod <- mod$datamod
  if (inherits(datamod, "bage_datamod_exposure")) {
    disp <- get_datamod_disp(datamod = datamod,
                             components = components)
    ans <- disp / (3 * disp + 1)
  }
  else {
    n_outcome <- length(outcome)
    ans <- rep(disp, times = n_outcome)
  }
  ans
}

make_disp_obs.bage_mod_binom <- function(mod, components, disp) {
  outcome <- mod$outcome
  n_outcome <- length(outcome)
  ans <- rep(disp, times = n_outcome)
  ans
}







## 'draw_fitted_given_outcome' ------------------------------------------------

#' Draw Values for '.fitted' when 'outcome' (and 'offset')
#' Known, and when 'disp' non-NULL
#'
#' @param mod Object of class 'bage_mod'
#' @param expected Backtransformed linear predictor. An rvec.
#' @param disp Dispersion. An rvec.
#' @param outcome Values for outcome. A vector or NULL.
#' @param offset Values for offset. NULL iff 'outcome' is NULL;
#' otherwise a vector.
#'
#' @returns An rvec.
#'
#' @noRd
draw_fitted_given_outcome <- function(mod,
                                      outcome,
                                      offset,
                                      expected,
                                      disp) {
  UseMethod("draw_fitted_given_outcome")
}

## HAS_TESTS
#' @export
draw_fitted_given_outcome.bage_mod_pois <- function(mod,
                                                    expected,
                                                    disp,
                                                    outcome,
                                                    offset) {
  ## reformat everything to numeric vectors of
  ## same length to deal with NAs
  ## in 'outcome' and 'offset'
  n_val <- length(expected)
  n_draw <- rvec::n_draw(expected)
  expected <- as.numeric(expected)
  disp <- as.numeric(disp)
  disp <- rep(disp, each = n_val)
  if (rvec::is_rvec(outcome))
    outcome <- as.numeric(outcome)
  else
    outcome <- rep(outcome, times = n_draw)
  if (rvec::is_rvec(offset))
    offset <- as.numeric(offset)
  else
    offset <- rep(offset, times = n_draw)
  is_na <- is.na(outcome) | is.na(offset)
  outcome[is_na] <- 0
  offset[is_na] <- 0
  ans <- stats::rgamma(n = length(expected),
                       shape = outcome + 1 / disp,
                       rate = offset + 1 / (disp * expected))
  ans <- matrix(ans, nrow = n_val, ncol = n_draw)
  ans <- rvec::rvec_dbl(ans)
  ans
}

## HAS_TESTS
#' @export
draw_fitted_given_outcome.bage_mod_binom <- function(mod,
                                                     expected,
                                                     disp,
                                                     outcome,
                                                     offset) {
  ## reformat everything to numeric vectors of
  ## same length to deal with NAs
  ## in 'outcome' and 'offset'
  n_val <- length(expected)
  n_draw <- rvec::n_draw(expected)
  expected <- as.numeric(expected)
  disp <- as.numeric(disp)
  disp <- rep(disp, each = n_val)
  if (rvec::is_rvec(outcome))
    outcome <- as.numeric(outcome)
  else
    outcome <- rep(outcome, times = n_draw)
  if (rvec::is_rvec(offset))
    offset <- as.numeric(offset)
  else
    offset <- rep(offset, times = n_draw)
  is_na <- is.na(outcome) | is.na(offset)
  outcome[is_na] <- 0
  offset[is_na] <- 0
  ans <- stats::rbeta(n = length(expected),
                      shape1 = outcome + expected / disp,
                      shape2 = offset - outcome + (1 - expected) / disp)
  ans <- matrix(ans, nrow = n_val, ncol = n_draw)
  ans <- rvec::rvec_dbl(ans)
  ans
}





## HAS_TESTS
#' @export
forecast_augment.bage_mod <- function(mod,
                                      data_forecast,
                                      components_forecast,
                                      linpred_forecast) {
  ## extract values
  outcome_est <- mod$outcome
  datamod <- mod$datamod
  confidential <- mod$confidential
  seed_augment <- mod$seed_augment
  nms_data_forecast <- names(data_forecast)
  nm_distn <- nm_distn(mod)
  nm_outcome_data <- get_nm_outcome_data(mod)
  nm_offset_data <- get_nm_offset_data(mod)
  nm_offset_data_true <- paste0(".", nm_offset_data)
  has_offset_est <- !is.null(nm_offset_data)
  has_disp <- has_disp(mod)
  inv_transform <- get_fun_inv_transform(mod)
  ## prepare inputs
  has_confidential <- has_confidential(mod)
  has_datamod <- has_datamod(mod)
  has_datamod_outcome <- has_datamod_outcome(mod)
  has_datamod_offset <- has_datamod && !has_datamod_outcome
  is_derive_outcome_obs <- has_datamod_outcome || has_confidential
  has_missing_outcome_est <- anyNA(outcome_est)
  has_offset_forecast <- (has_offset_est
    && (nm_offset_data %in% nms_data_forecast))
  has_offset_forecast_true <- (has_datamod_offset
    && (nm_offset_data_true %in% nms_data_forecast))
  has_offset <- has_offest_forecast || has_offset_forecast_true
  blank <- rep(NA_real_, times = nrow(data_forecast))
  ans <- data_forecast
  ## prepare seeds
  seed_restore <- make_seed() ## create randomly-generated seed
  set.seed(seed_augment) ## set pre-determined seed
  ## forecast fitted and (if has disp) expected
  if (has_disp) {
    expected <- inv_transform(linpred_forecast)
    disp <- get_disp(mod)
    fitted <- draw_fitted(mod = mod,
                          expected = expected,
                          disp = disp)
  }
  else {
    disp <- NULL
    fitted <- inv_transform(linpred_forecast)
  }
  ## if have offset in 'data_forecast', then also
  ## forecast outcome, and possibly true/observed offset
  if (has_offset) {
    if (has_datamod) {
      is_derive_offset_true <- has_offset_forecast && !has_offset_forecast_true
      is_derive_offset_obs <- !has_offset_forecast && has_offset_forecast_true
      if (is_derive_offset_true) {
        offset_forecast_obs <- data_forecast[[nm_offset_data]]
        offset_forecast <- draw_offset_true_given_obs(datamod = datamod,
                                                      nm_distn = nm_distn,
                                                      components = components_forecast,                                                  
                                                      offset = offset_forecast_obs,
                                                      expected = expected)
        ans <- insert_after(df = ans,
                            nm_after = nm_offset_data,
                            x = offset_forecast,
                            nm_x = nm_offset_data_true)
      }
      else if (is_derive_offset_obs) {
        offset_forecast <- data_forecast[[nm_offset_data_true]]
        offset_forecast_obs <- draw_offset_obs(datamod = datamod,
                                               offset_true = offset_forecast)
        ans <- insert_before(df = ans,
                             nm_before = nm_offset_data_true,
                             x = offset_forecast_obs,
                             nm_x = nm_offset_data)
      }
      else {
        offset_forecast <- data_forecast[[nm_offset_data]]
      }
    }
    else
      offset_forecast <- data_forecast[[nm_offset_data]]
    outcome_true <- draw_outcome_true(nm_distn = nm_distn,
                                      offset = offset_forecast,
                                      fitted = fitted,
                                      disp = NULL)
    if (is_derive_outcome_obs) {
      outcome <- outcome_true
      if (has_datamod_outcome)
        outcome <- draw_outcome_obs(datamod = datamod_outcome,
                                    components = components_forecast,
                                    outcome_true = outcome)
      if (has_confidential)
        outcome <- draw_outcome_confidential(confidential = confidential,
                                             outcome_obs = outcome)
      ans[[nm_outcome_data]] <- outcome
      nm_outcome_data_true <- paste0(".", nm_outcome_data)
      ans <- insert_after(df = ans,
                          nm_after = nm_outcome_data,
                          x = outcome_true,
                          nm_x = nm_outcome_data_true)
    }
    else {
      ## even where no confidentialization or mismeasurement,
      ## need to report two values if historical values
      ## contained NAs
      if (has_missing_outcome_est) {
        ans[[nm_outcome_data]] <- blank
        ans <- insert_after(df = ans,
                            nm_after = nm_outcome_data,
                            x = outcome_true,
                            nm_x = nm_outcome_data_true)
      }
      else
        ans[[nm_outcome_data]] <- outcome_true
    }
  }
  else {
    ans[[nm_outcome_data]] <- blank
    if (is_derive_outcome_obs)
      ans <- insert_after(df = ans,
                          nm_after = nm_outcome_data,
                          x = blank,
                          nm_x = nm_outcome_data_true)
  }
  ## restore seed
  set.seed(seed_restore) ## set randomly-generated seed, to restore randomness
  ans$.observed <- NA_real_
  ans$.fitted <- fitted
  if (has_disp)
    ans$.expected <- expected
  ans
}



## HAS_TESTS - NOT YET REVISED
#' @export
forecast_augment.bage_mod_norm <- function(mod,
                                           data_forecast,
                                           components_forecast,
                                           linpred_forecast) {
  outcome_est <- mod$outcome
  datamod_outcome <- mod$datamod_outcome
  seed_augment <- mod$seed_augment
  nm_distn <- nm_distn(mod)
  nm_outcome_data <- get_nm_outcome_data(mod)
  nm_outcome_data_true <- paste0(".", nm_outcome_data)
  nm_offset_data <- get_nm_offset_data(mod)
  has_offset_est <- !is.null(nm_offset_data)
  scale_linpred <- get_fun_orig_scale_linpred(mod)
  has_datamod_outcome <- !is.null(datamod_outcome)
  has_imputed_outcome_est <- anyNA(outcome_est)
  blank <- rep(NA_real_, times = nrow(data_forecast))
  ones <- rep(1, times = nrow(data_forecast))
  if (has_offset_est) {
    if (nm_offset_data %in% names(data_forecast))
      offset_forecast <- data_forecast[[nm_offset_data]]
    else
      offset_forecast <- blank
  }
  else
    offset_forecast <- ones
  has_offset_forecast <- !all(is.na(offset_forecast))
  ans <- data_forecast
  ## Derive fitted
  fitted <- scale_linpred(linpred_forecast)
  ans$.fitted <- fitted
  ## Derive outcome and observed. If have data model
  ## or imputed outcomes in historical estimates,
  ## then have two versions of outcome variable in forecasts
  if (has_offset_forecast)  {
    offset_mean <- mod$offset_mean
    outcome_sd <- mod$outcome_sd
    disp <- get_disp(mod)
    disp <- sqrt(offset_mean) * outcome_sd * disp
    seed_restore <- make_seed() ## create randomly-generated seed
    set.seed(seed_augment) ## set pre-determined seed
    outcome_true <- draw_vals_outcome_true(datamod = datamod_outcome,
                                           nm_distn = nm_distn,
                                           outcome_obs = blank,
                                           fitted = fitted,
                                           disp = disp,
                                           offset = offset_forecast)
    if (has_datamod_outcome) {
      outcome_obs <- draw_vals_outcome_obs(datamod = datamod_outcome,
                                           outcome_true = outcome_true)
      ans[[nm_outcome_data]] <- outcome_obs
      ans <- insert_after(df = ans,
                          nm_after = nm_outcome_data,
                          x = outcome_true,
                          nm_x = nm_outcome_data_true)
    }
    else {
      if (has_imputed_outcome_est) {
        ans[[nm_outcome_data]] <- blank
        ans <- insert_after(df = ans,
                            nm_after = nm_outcome_data,
                            x = outcome_true,
                            nm_x = nm_outcome_data_true)
      }
      else
        ans[[nm_outcome_data]] <- outcome_true
    }
    set.seed(seed_restore) ## set randomly-generated seed, to restore randomness
  }
  else {
    ans[[nm_outcome_data]] <- blank
    if (has_datamod_outcome || has_imputed_outcome_est)
      ans <- insert_after(df = ans,
                          nm_after = nm_outcome_data,
                          x = blank,
                          nm_x = nm_outcome_data_true)
  }
  ans$.fitted <- fitted
  ans
}

    
  
check_datamod_by_val <- function(by_val, data, nm_val) {
  nms_by <- names(by_val)
  nms_data <- names(data)
  i_data <- match(nms_by, nms_data, nomatch = 0L)
  i_nomatch <- match(0L, i_data, nomatch = 0L)
  if (i_nomatch > 0L) {
    nm_nomatch <- nms_by[[i_nomatch]]
    cli::cli_abort(c("{.arg {nm_x}} has variable not found in {.arg data}.",
                     "Variable not found: {.var {nm_nomatch}}."))
  }
  key_by <- Reduce(paste_dot, by_val)
  key_data <- Reduce(paste_dot, data[nms_by])
  i_data <- match(key_by, key_data, nomatch = 0L)
  i_nomatch <- match(0L, i_data, nomatch = 0L)
  if (i_nomatch > 0L) {
    row <- data[i_nomatch, , drop = FALSE]
    levels <- sprintf("%s=%s", names(row), row)
    levels <- paste(levels, collapse = ", ")
    n_by <- length(nms_by)
    cli::cli_abort(c("{.arg {nm_val}} does not cover all combinations in data.",
                     i = paste("{.arg {data} has {levels},",
                               "but {.arg {nm_val}} does not.")))
  }
  i_by <- match(key_data, key_by, nomatch = 0L)
  i_nomatch <- match(0L, i_by, nomatch = 0L)
  if (i_nomatch > 0L) {
    row <- by_val[i_nomatch, , drop = FALSE]
    levels <- sprintf("%s=%s", names(row), row)
    levels <- paste(levels, collapse = ", ")
    n_by <- length(nms_by)
    cli::cli_abort(c("{.arg {nm_val}} has unused rows.",
                     i = paste("{.arg {nm_val}} has {levels},",
                               "but {.arg data} does not.")))
  }
  invisible(TRUE)
}
  


set_datamod_undercount <- function(mod, prob) {
  measure_vars <- c("mean", "disp")
  data <- mod$data
  nm_distn <- nm_distn(mod)
  if (!(nm_distn %in% c("pois", "binom"))) {
    model_descr <- model_descr(mod)
    cli::cli_abort(c(paste("An undercount data model can only be used",
                           "with a Poisson or binomial model."),
                     i = "This is a {model_descr} model."))
  }
  check_datamod_val(x = prob,
                    nm_x = "prob",
                    measure_vars = measure_vars)
  by_vars <- prob[setdiff(names(prob), measure_vars)]
  check_datamod_by_val(by_val = by_val,
                       data = data)
  ## NEED TO MODIFY TO ALLOW FOR ONE-ROW, NO-BY-VAR OPTION
  matrix_prob_outcome <- make_matrix_val_outcome(data = data,
                                                 by_val = by_val)
  datamod <- new_bage_datamod_undercount(prob_mean = prob$mean,
                                         prob_disp = prob$disp,
                                         matrix_prob_outcome = matrix_prob_outcome)
  mod$datamod <- datamod
  mod
}

  
  
