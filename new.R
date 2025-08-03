
## HAS_TESTS
#' @export
draw_vals_augment_fitted.bage_mod <- function(mod) {
  ## extract values
  outcome <- mod$outcome
  offset <- mod$offset
  seed_augment <- mod$seed_augment
  confidential <- mod$confidential
  datamod <- mod$datamod
  nm_distn <- nm_distn(mod)
  disp <- get_disp(mod)
  disp_datamod <- get_disp_datamod(mod)
  inv_transform <- get_fun_inv_transform(mod)
  ## prepare inputs
  linpred <- make_linpred_from_stored_draws(mod = mod, point = FALSE)
  expected <- inv_transform(linpred)
  has_confidential <- !is.null(confidential)
  has_datamod <- !is.null(datamod)
  has_datamod_outcome <- inherits(datamod, "bage_datamod_outcome")
  has_datamod_offset <- inherits(datamod, "bage_datamod_offset")
  has_missing_outcome <- anyNA(outcome)
  has_disp <- has_disp(mod)
  ## prepare seeds
  seed_restore <- make_seed() ## create randomly-generated seed
  set.seed(seed_augment) ## set pre-determined seed
  ## draw values for outcome and offset, where necessary
  if (has_confidential) {
    if (has_datamod) {
      expected_obs <- make_expected_obs(datamod = datamod,
                                        expected = expected,
                                        disp_datamod = disp_datamod)
    }
    else
      expected_obs <- expected
    outcome_obs <- draw_outcome_obs_given_conf(confidential = confidential,
                                               nm_distn = nm_distn,
                                               outcome_conf = outcome,
                                               offset = offset,
                                               expected_obs = expected_obs,
                                               disp = disp)
  }
  if (has_datamod) {
    outcome <- draw_outcome_true_given_obs(datamod = datamod,
                                           outcome = outcome,
                                           offset = offset,
                                           nm_distn = nm_distn,
                                           expected = expected,
                                           disp = disp)
    offset <- draw_offset_true_given_obs(datamod = datamod_outcome,
                                         nm_distn = nm_distn,
                                         outcome = outcome,
                                         offset = offset,
                                         expected = expected,
                                         disp = disp)
  }
  if (has_missing_outcome) {
    outcome <- impute_outcome_true(nm_distn = nm_distn,
                                   outcome = outcome,
                                   offset = offset,
                                   expected = expected,
                                   disp = disp)
  }
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
    ans <- insert_after(df = ans,
                        nm_after = nm_outcome_data,
                        x = outcome,
                        nm_x = nm_outcome_data_true)
  }
  if (has_datamod_offset) {
    nm_offset_data <- get_nm_offset_data(mod)
    nm_offset_data_true <- paste0(".", nm_offset_data)
    ans <- insert_after(df = ans,
                        nm_after = nm_offset_data,
                        x = offset,
                        nm_x = nm_offset_data_true)
  }    
  ans
}




## HAS_TESTS
#' @export
draw_vals_augment_fitted.bage_mod_norm <- function(mod) {
  ## extract values
  outcome <- mod$outcome
  offset <- mod$offset
  seed_augment <- mod$seed_augment
  datamod_outcome <- mod$datamod_outcome
  nm_distn <- nm_distn(mod)
  disp <- get_disp(mod)
  scale_linpred <- get_fun_scale_linpred(mod)
  scale_disp <- get_fun_scale_disp(mod)
  scale_offset <- get_fun_scale_offset(mod)
  ## prepare inputs
  linpred <- make_linpred_from_stored_draws(mod = mod, point = FALSE)
  disp <- get_disp(mod)
  has_datamod_outcome <- !is.null(datamod_outcome)
  has_missing_outcome <- anyNA(outcome)
  expected_scaled <- scale_linpred(linpred)
  disp_scaled <- scale_disp(disp)
  offset_scaled <- scale_offset(offset)
  ## prepare seeds
  seed_restore <- make_seed() ## create randomly-generated seed
  set.seed(seed_augment) ## set pre-determined seed
  ## draw values for outcome, where necessary
  if (has_datamod_outcome) {
    outcome <- draw_outcome_true_given_obs(datamod = datamod_outcome,
                                           nm_distn = nm_distn,
                                           outcome = outcome,
                                           offset = offset_scaled,
                                           expected = expected_scaled,
                                           disp = disp_scaled)
  }
  if (has_missing_outcome) {
    outcome <- impute_outcome_true(nm_distn = nm_distn,
                                   outcome = outcome,
                                   offset = offset_scaled,
                                   expected = expected_scaled,
                                   disp = disp_scaled)
  }
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
  datamod_outcome <- mod$datamod_outcome
  datamod_offset <- mod$datamod_offset
  dimnames_terms <- mod$dimnames_terms
  n_draw <- mod$n_draw
  nm_distn <- nm_distn(mod)
  disp <- get_disp(mod)
  inv_transform <- get_fun_inv_transform(mod)
  nm_outcome_data <- get_nm_outcome_data(mod)
  ## prepare inputs
  has_confidential <- !is.null(confidential)
  has_datamod_outcome <- !is.null(datamod_outcome)
  has_datamod_offset <- !is.null(datamod_offset)
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
    fitted <- draw_vals_fitted(mod = mod,
                               vals_expected = expected,
                               vals_disp = disp)
  }
  else
    fitted <- expected
  ## obtain values for true outcome
  outcome <- draw_outcome_true(offset = offset,
                               nm_distn = nm_distn,
                               fitted = fitted)
  ## create modified value of outcome,
  ## where necessary, and record outcome(s)
  has_modified_outcome <- has_datamod_outcome || has_confidential
  if (has_modified_outcome) {
    outcome_true <- outcome
    if (has_datamod_outcome)
      outcome <- draw_outcome_obs(datamod_outcome = datamod_outcome,
                                  outcome_true = outcome)
    if (has_confidential)
      outcome <- draw_outcome_confidential(datamod_outcome = datamod_outcome,
                                           outcome_obs = outcome)
    ans[[nm_outcome_data]] <- outcome
    nm_outcome_data_true <- paste0(".", nm_outcome_data)
    ans[[nm_outcome_data_true]] <- outcome_true
  }
  else
    ans[[nm_outcome_data]] <- outcome
  ## create modified value of true offset,
  ## where necessary, and record offset(s)
  if (has_datamod_offset) {
    offset_true <- offset
    offset <- draw_offset_obs(datamod_offset = datamod_offset,
                              offset_true = offset_true)
    ans[[nm_offset_data]] <- offset
    nm_offset_data_true <- paste0(".", nm_offset_data)
    ans[[nm_offset_data_true]] <- offset_true
  }
  else
    ans[[nm_offset_data]] <- offset
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
#' @export
draw_vals_augment_unfitted.bage_mod_norm <- function(mod) {
  ## extract values
  data <- mod$data
  offset <- mod$offset
  seed_augment <- mod$seed_augment
  datamod_outcome <- mod$datamod_outcome
  dimnames_terms <- mod$dimnames_terms
  n_draw <- mod$n_draw
  nm_distn <- nm_distn(mod)
  disp <- get_disp(mod)
  orig_scale_linpred <- get_fun_orig_scale_linpred(mod)
  orig_scale_disp <- get_fun_orig_scale_disp(mod)
  orig_scale_offset <- get_fun_orig_scale_offset(mod)
  nm_outcome_data <- get_nm_outcome_data(mod)
  ## prepare inputs
  has_confidential <- !is.null(confidential)
  has_datamod_outcome <- !is.null(datamod_outcome)
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
  ## create modified value of outcome,
  ## where necessary, and record outcome(s)
  has_modified_outcome <- has_datamod_outcome || has_confidential
  if (has_modified_outcome) {
    outcome_true <- outcome
    if (has_datamod_outcome)
      outcome <- draw_outcome_obs(datamod_outcome = datamod_outcome,
                                  outcome_true = outcome)
    if (has_confidential)
      outcome <- draw_outcome_confidential(datamod_outcome = datamod_outcome,
                                           outcome_obs = outcome)
    ans[[nm_outcome_data]] <- outcome
    nm_outcome_data_true <- paste0(".", nm_outcome_data)
    ans[[nm_outcome_data_true]] <- outcome_true
  }
  else
    ans[[nm_outcome_data]] <- outcome
  ## restore seed
  set.seed(seed_restore) ## set randomly-generated seed, to restore randomness
  ## assemble and return answer
  ans$.fitted <- fitted_orig_scale
  ans
}



  

  

  
  
    


