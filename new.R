
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
  if (has_missing_outcome)
    outcome <- impute_outcome_true(nm_distn = nm_distn,
                                   outcome = outcome,
                                   offset = offset,
                                   expected = expected,
                                   disp = disp)
  if (has_datamod_offset)
    offset <- draw_offset_true_given_obs(datamod = datamod,
                                         nm_distn = nm_distn,
                                         outcome = outcome,
                                         offset = offset,
                                         expected = expected,
                                         disp = disp)
  ## derive '.fitted' and, in models with dispersion, '.expected'
  ans <- mod$data
  if (has_disp) {
    ans$.fitted <- draw_vals_fitted_given_outcome(mod = mod,
                                                  vals_expected = expected,
                                                  vals_disp = disp,
                                                  outcome = outcome,
                                                  offset = offset)
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
  has_datamod <- has_datamod(mod)
  has_missing_outcome <- anyNA(outcome)
  expected_scaled <- scale_linpred(linpred)
  disp_scaled <- scale_disp(disp)
  offset_scaled <- scale_offset(offset)
  ## prepare seeds
  seed_restore <- make_seed() ## create randomly-generated seed
  set.seed(seed_augment) ## set pre-determined seed
  ## draw values for outcome, where necessary
  if (has_datamod)
    outcome <- draw_outcome_true_given_obs(confidential = NULL,
                                           datamod = datamod,
                                           nm_distn = nm_distn,
                                           outcome = outcome,
                                           offset = offset_scaled,
                                           expected = expected_scaled,
                                           disp = disp_scaled)
  if (has_missing_outcome)
    outcome <- impute_outcome_true(nm_distn = nm_distn,
                                   outcome = outcome,
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
                               disp = disp)
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
                                  offset_true = offset_true)
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



  

  

  
#' Make Matrix Mapping from Value on to Outcome
#'
#' Assume that all variables found in 'by_val'
#' can be found in 'data'.
#'
#' Assume that 'by_val' is unique.
#' 
#' Assume that all combinations of 'by'
#' variables found in 'data' can be found
#' in 'by_val'.
#'
#' Do not assume that all combinations of
#' variables found in 'by_val'
#' can be found in 'data'.
#'
#' @param data Dataset for model
#' @param by_val Data frame with variables defining
#' level for value. 
#'
#' @returns A sparse matrix
#'
#' @noRd    
make_matrix_val_used_outcome <- function(data, by_val) {
  nms <- names(by_val)
  key_data <- Reduce(paste_dot, data[nms])
  key_val <- Reduce(paste_dot, by_val)
  key_val_used <- key_val[key_val %in% key_data]
  i <- seq_len(key_data)
  j <- match(key_data, key_val_used)
  Matrix::sparseMatrix(x = 1, i = i, j = j)
}


#' Make Index Between All Values and Values
#' that are Mapped on to Outcome
#'
#' Assume that 'by_val' is unique.
#' 
#' Assume that all combinations of categorical
#' variables found in 'data' can be found
#' in 'by_val'.
#'
#' Do not assume that all combinations of
#' categorical variables found in 'by_val'
#' can be found in 'data'.
#'
#' @param data Dataset for model
#' @param by_val Data frame with variables defining
#' level for value. 
#'
#' @returns An integer vector
#'
#' @noRd    
index_val_used <- function(data, by_val) {
  nms <- names(by_val)
  key_data <- Reduce(paste_dot, data[nms])
  key_val <- Reduce(paste_dot, by_val)
  key_val_used <- key_val[key_val %in% key_data]
  match(key_val, key_val_used, nomatch = 0L)
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


## helper functions:

make_expected_obs_exposure <- function(datamod,
                                       components,
                                       expected) {
  ratio <- get_datamod_ratio(datamod)
  disp <- get_datamod_disp(datamod = datamod,
                           components = components)
  numerator <- (3 * disp + 1) * expected
  denominator <- (disp + 1) * ratio
  numerator / denominator
}



make_expected_obs_miscount <- function(datamod,
                                       components,
                                       expected) {
  prob <- get_datamod_prob(datamod = datamod,
                           components = components)
  rate <- get_datamod_rate(datamod = datamod,
                           components = components)
  (prob + rate) * expected
}


    
make_expected_obs_overcount <- function(datamod,
                                        components,
                                        expected) {
  rate <- get_datamod_rate(datamod = datamod,
                           components = components)
  (1 + rate) * expected
}


make_expected_obs_undercount <- function(datamod,
                                         components,
                                         expected) {
  prob <- get_datamod_prob(datamod = datamod,
                           components = components)
  prob * expected
}


get_datamod_ratio <- function(datamod) {
  ratio <- datamod$ratio
  matrix_ratio_outcome <- datamod$matrix_ratio_outcome
  matrix_ratio_outcome <- as.matrix(matrix_ratio_outcome) ## remove after updating rvec
  ratio <- matrix_ratio_outcome %*% ratio
  ratio <- as.numeric(ratio) ## convert from matrix
  ratio
}


get_datamod_disp <- function(datamod, components) {
  matrix_disp_outcome <- datamod$matrix_disp_outcome
  matrix_disp_outcome <- as.matrix(matrix_disp_outcome) ## remove after updating rvec
  is_disp <- (components$term == "datamod"
    & components$component == "disp")
  disp <- components$.fitted[is_disp]
  disp <- matrix_disp_outcome %*% disp
  disp
}



get_datamod_prob <- function(datamod, components) {
  matrix_prob_outcome <- datamod$matrix_prob_outcome
  matrix_prob_outcome <- as.matrix(matrix_prob_outcome) ## remove after updating rvec
  is_prob <- (components$term == "datamod"
    & components$component == "prob")
  prob <- components$.fitted[is_prob]
  prob <- matrix_prob_outcome %*% prob
  prob
}

get_datamod_rate <- function(datamod, components) {
  matrix_rate_outcome <- datamod$matrix_rate_outcome
  matrix_rate_outcome <- as.matrix(matrix_rate_outcome) ## remove after updating rvec
  is_rate <- (components$term == "datamod"
    & components$component == "rate")
  rate <- components$.fitted[is_rate]
  rate <- matrix_rate_outcome %*% rate
  rate
}


get_datamod_mean <- function(datamod) {
  mean <- datamod$mean
  matrix_mean_outcome <- datamod$matrix_mean_outcome
  mean <- matrix_mean_outcome %*% mean
  mean <- as.numeric(mean)
  mean
}

get_datamod_sd <- function(datamod) {
  sd <- datamod$sd
  matrix_sd_outcome <- datamod$matrix_sd_outcome
  sd <- matrix_sd_outcome %*% sd
  sd <- as.numeric(sd)
  sd
}  





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


