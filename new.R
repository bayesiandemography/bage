
## HAS_TESTS
#' @export
draw_vals_augment_fitted.bage_mod <- function(mod) {
  ## extract values
  outcome <- mod$outcome
  offset <- mod$offset
  seed_augment <- mod$seed_augment
  confidential <- mod$confidential
  datamod_outcome <- mod$datamod_outcome
  datamod_offset <- mod$datamod_offset
  nm_distn <- nm_distn(mod)
  disp <- get_disp(mod)
  inv_transform <- get_fun_inv_transform(mod)
  ## prepare inputs
  linpred <- make_linpred_from_stored_draws(mod = mod, point = FALSE)
  expected <- inv_transform(linpred)
  has_confidential <- !is.null(confidential)
  has_datamod_outcome <- !is.null(datamod_outcome)
  has_missing_outcome <- anyNA(outcome)
  has_datamod_offset <- !is.null(datamod_offset)
  has_disp <- has_disp(mod)
  ## prepare seeds
  seed_restore <- make_seed() ## create randomly-generated seed
  set.seed(seed_augment) ## set pre-determined seed
  ## impute values for outcome and offset, where necessary
  if (has_confidential) {
    outcome <- draw_outcome_unconfidential(confidential = confidential,
                                           outcome = outcome,
                                           offset = offset,
                                           nm_distn = nm_distn,
                                           expected = expected,
                                           disp = disp)
  }
  if (has_datamod_outcome) {
    outcome <- draw_outcome_true_given_obs(datamod = datamod_outcome,
                                           outcome = outcome,
                                           offset = offset,
                                           nm_distn = nm_distn,
                                           expected = expected,
                                           disp = disp)
  }
  if (has_missing_outcome) {
    outcome <- impute_missing_outcome(outcome = outcome,
                                      offset = offset,
                                      nm_distn = nm_distn,
                                      expected = expected,
                                      disp = disp)
  }
  if (has_datamod_offset) {
    offset <- draw_offset_true_given_obs(datamod = datamod_outcome,
                                         outcome = outcome,
                                         offset = offset,
                                         nm_distn = nm_distn,
                                         expected = expected,
                                         disp = disp)
  }
  ## derived '.fitted' and, in models with dispersion, '.expected'
  ans <- mod$data
  if (has_disp) {
    ans$.fitted <- draw_vals_fitted(mod = mod,
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
  outcome_sd <- mod$outcome_sd
  offset_mean <- mod$offset_mean
  nm_distn <- nm_distn(mod)
  disp <- get_disp(mod)
  scale_linpred <- get_fun_scale_linpred(mod)
  ## prepare inputs
  expected <- make_linpred_from_stored_draws(mod = mod, point = FALSE)
  has_datamod_outcome <- !is.null(datamod_outcome)
  has_missing_outcome <- anyNA(outcome)
  ## prepare seeds
  seed_restore <- make_seed() ## create randomly-generated seed
  set.seed(seed_augment) ## set pre-determined seed
  ## impute values for outcome, where necessary
  ## NEED TO BE CAREFUL WITH SCALING HERE
  if (has_datamod_outcome) {
    outcome <- draw_outcome_true(datamod = datamod_outcome,
                                 outcome = outcome,
                                 offset = offset,
                                 nm_distn = nm_distn,
                                 expected = expected,
                                 disp = disp)
  }
  if (has_missing_outcome) {
    outcome <- impute_missing_outcome(outcome = outcome,
                                      offset = offset,
                                      nm_distn = nm_distn,
                                      expected = expected,
                                      disp = disp)
  }
  ## restore seed
  set.seed(seed_restore) ## set randomly-generated seed, to restore randomness
  ## derive '.fitted'
  ans$.fitted <- scale_linpred(linpred)
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
  has_datamod_outcome <- !is.null(datamod_outcome)
  has_datamod_offset <- !is.null(datamod_offset)
  has_disp <- has_disp(mod)
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
  ## draw values for observed outcome, where necessary
  if (has_datamod_outcome)
    outcome_obs <- draw_outcome_obs(datamod_outcome = datamod_outcome,
                                    outcome_true = outcome)
  ## draw values for observed offset, where necessary
  if (has_datamod_offset)
    offset_obs <- draw_offset_obs(datamod_offset = datamod_offset,
                                  offset_true = offset)
  ## restore seed
  set.seed(seed_restore) ## set randomly-generated seed, to restore randomness
  ## assemble and return answer
  ans <- mod$data
  if (has_datamod_outcome) {
    ans[[nm_outcome_data]] <- outcome_obs
    nm_outcome_data_true <- paste0(".", nm_outcome_data)
    ans[[nm_outcome_data_true]] <- outcome
    num_observed <- outcome_obs
  }
  else {
    ans[[nm_outcome_data]] <- outcome
    num_observed <- outcome
  }
  if (has_datamod_offset) {
    ans[[nm_offset_data]] <- offset_obs
    nm_offset_data_true <- paste0(".", nm_offset_data)
    ans[[nm_offset_data_true]] <- offset
    denom_observed <- offset_obs
  }
  else {
    ans[[nm_offset_data]] <- offset
    denom_observed <- offset
  }
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
  outcome_sd <- mod$outcome_sd
  offset_mean <- mod$offset_mean
  dimnames_terms <- mod$dimnames_terms
  n_draw <- mod$n_draw
  nm_distn <- nm_distn(mod)
  disp <- get_disp(mod)
  scale_linpred <- get_fun_scale_linpred(mod)
  nm_outcome_data <- get_nm_outcome_data(mod)
  ## prepare inputs
  has_datamod_outcome <- !is.null(datamod_outcome)
  offset_scaled <- offset_mean * offset
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
  ## obtain values for 'fitted' and 'disp'
  vals_fitted <- scale_linpred(vals_linpred)
  is_disp <- vals_components$component == "disp"
  disp <- vals_components$.fitted[is_disp]
  disp_scaled <- sqrt(offset_mean) * outcome_sd * vals_disp
  ## obtain values for true outcome
  ## NEED TO DEAL WITH SCALING
  outcome <- draw_outcome_true(offset = offset,
                               nm_distn = nm_distn,
                               fitted = fitted,
                               disp = disp)
  ## draw values for observed outcome, where necessary
  if (has_datamod_outcome)
    outcome_obs <- draw_outcome_obs(datamod_outcome = datamod_outcome,
                                    outcome_true = outcome)
  ## restore seed
  set.seed(seed_restore) ## set randomly-generated seed, to restore randomness
  ## assemble and return answer
  ans <- mod$data
  if (has_datamod_outcome) {
    ans[[nm_outcome_data]] <- vals_outcome_obs
    nm_outcome_data_true <- paste0(".", nm_outcome_data)
    ans[[nm_outcome_data_true]] <- vals_outcome_true
  }
  else {
    ans[[nm_outcome_data]] <- vals_outcome_true
  }
  ans$.fitted <- vals_fitted
  ans
}




## HAS_TESTS
#' @export
draw_vals_outcome_true.bage_datamod_outcome_rr3 <- function(datamod,
                                                            nm_distn,
                                                            outcome_obs,
                                                            fitted,
                                                            expected,
                                                            disp,
                                                            offset) {
  n_val <- length(outcome_obs)
  has_disp <- !is.null(disp)
  if (has_disp) {
    n_draw <- rvec::n_draw(expected)
    expected <- as.matrix(expected)
    disp <- matrix(as.numeric(disp), nrow = n_val, ncol = n_draw, byrow = TRUE)
    offset <- matrix(offset, nrow = n_val, ncol = n_draw)
    if (nm_distn == "pois") {
      nm_dist_detailed <- "nbinom"
      shape <- 1 / disp
      rate <- 1 / (expected * offset * disp)
      args <- list(shape = shape, rate = rate)
    }
    else if (nm_distn == "binom") {
      nm <- "betabinom"
      shape <- 1 / disp
      rate <- 1 / (expected * offset * disp)
      args <- list(size = offset, shape = shape, rate = rate)
    }
    else
      cli::cli_abort("Internal error: Invalid value for {.var nm_distn}.")
  }
  else {
    n_draw <- rvec::n_draw(fitted)
    fitted <- as.matrix(fitted)
    offset <- matrix(offset, nrow = n_val, ncol = n_draw)
    if (nm_distn == "pois") {
      nm_dist_detailed <- "pois"
      lambda <- fitted * offset
      args <- list(lambda = lambda)
    }
    else if (nm_distn == "binom") {
      nm <- "binom"
      args <- list(size = offset, prob = fitted)
    }
    else
      cli::cli_abort("Internal error: Invalid value for {.var nm_distn}.")
  }
  args <- lapply(seq_along(n_val),
                 function(i) lapply(args, function(j) arg[i, ]))
  draw_vals_outcome_true_rr3(nm_distn_detailed = nm_dist_detailed,
                             outcome_obs = outcome_obs,
                             args = args,
                             offset = offset,
                             n_draw = n_draw)
}                            


#' Draw Values for Outcome Variable When Value Not Supplied
#' 
draw_vals_outcome_no_obs <- function(nm_distn_detailed,
                                     args,
                                     offset) {
  if (nm_distn_detailed == "pois") {
    lambda <- args$lambda
    stats::rpois(n = length(lambda),
                 lambda = lambda)
  }
  else if (nm_distn_detailed == "nbinom") {
    shape <- args$shape
    rate <- args$rate
    stats::rnbinom(n = length(shape),
                   shape = shape,
                   rate = rate)
  }
  else if (nm_distn_detailed == "binom") {
    size <- args$size
    prob <- args$prob
    stats::rbinom(n = length(size),
                  size = size,
                  prob = prob)
  }
  else if (nm_distn_detailed == "betabinom") {
    size <- args$size
    shape1 <- args$shape1
    shape2 <- args$shape2
    stats::rbetabinom(n = length(size),
                      size = size,
                      shape1 = shape1,
                      shape2 = shape2)
  }
  else
    cli::cli_abort(paste("Internal error: Invalid value for",
                         "{.var nm_distn_detailed}."))
}


draw_vals_outcome_true_rr3 <- function(nm_distn_detailed,
                                       outcome_obs,
                                       args,
                                       offset,
                                       n_draw) {
  n_val <- length(outcome_obs)
  ans <- matrix(NA_integer_, nrow = n_val, ncol = n_draw)
  for (i_val in seq_len(n_val)) {
    outcome_obs_i <- outcome_obs[[i_val]]
    offset_i <- offset[[i_val]]
    has_outcome <- is.na(outcome_obs_i)
    has_offset <- is.na(offset_i)
    args_i <- args[[i_val]]
    if (has_offset) {
      if (has_outcome) {
        ans[i_val, ] <- draw_vals_outcome_no_outcome(nm_distn_detailed = nm_dist_detailed,
                                                     args = args_i,
                                                     offset = offset_i)
      }
      else {
        ans[i_val, ] <- draw_vals_outcome_has_outcome_obs_rr3(nm_distn_detailed = nm_dist_detailed,
                                                              n_draw = n_draw,
                                                              args = args_i,
                                                              outcome_obs = outcome_obs_i,
                                                              offset = offset_i)
      }
    }
  }
  ans <- rvec::rvec(ans)
  ans
}



draw_vals_outcome_has_obs_rr3 <- function(nm_distn_detailed,
                                          n_draw,
                                          args,
                                          observed_obs,
                                          offset) {
  is_obs_zero <- observed_obs == 0L
  if (is_obs_zero) {
    outcome_true <- 0:2
    prob_obs_given_true <- c(1, 2/3, 1/3)
  }
  else {
    outcome_true <- seq.int(from = observed_obs - 2L, to = observed_obs + 2L)
    prob_obs_given_true <- c(1/3, 2/3, 1, 2/3, 1/3)
  }
  prob_obs_true <- make_prob_obs_true(nm_distn_detailed = nm_distn_detailed,
                                      n_draw = n_draw,
                                      args = args,
                                      observed_obs = observed_obs,
                                      offset = offset,
                                      outcome_true = outcome_true,
                                      prob_obs_given_true = prob_obs_given_true)
  ans <- integer(length = n_draw)
  for (i_draw in seq_len(n_draw)) {
    ans[[i]] <- sample(x = outcome_true,
                       size = 1L,
                       prob = prob_obs_true[, i_draw])
  }
  ans
}
  


## calculate 'prob_obs_true', the
## (unnormalised) probability of
## each possible value of true outcome
make_prob_obs_true <- function(nm_distn_detailed,
                               n_draw,
                               args,
                               observed,
                               offset,
                               outcome_true,
                               prob_obs_given_true) {
  n_poss <- length(outcome_true)
  outcome_true <- rep.int(outcome_true, times = n_draw)
  prob_obs_given_true <- rep.int(prob_obs_given_true, times = n_draw)
  if (nm_distn_detailed == "pois") {
    lambda <- rep(args$lambda, each = n_poss)
    prob_prior <- stats::rpois(n = length(lambda),
                               lambda = lambda)
  }
  else if (nm_distn_detailed == "nbinom") {
    shape <- rep(args$shape, each = n_poss)
    rate <- repo(args$rate, each = n_poss)
    prob_prior <- stats::rnbinom(n = length(shape),
                                 shape = shape,
                                 rate = rate)
  }
  else if (nm_distn_detailed == "binom") {
    size <- rep(args$size, each = n_poss)
    prob <- rep(args$prob, each = n_poss)
    prob_prior <- stats::rbinom(n = length(size),
                                size = size,
                                prob = prob)
  }
  else if (nm_distn_detailed == "betabinom") {
    size <- args$size
    shape1 <- args$shape1
    shape2 <- args$shape2
    prob_prior <- stats::rbetabinom(n = length(size),
                                    size = size,
                                    shape1 = shape1,
                                    shape2 = shape2)
  }
  else
    cli::cli_abort("Internal error: Invalid value for {.var nm_distn_detailed}.")
  ans <- prob_obs_given_true * prob_prior
  ans <- matrix(ans, nrow = n_poss, ncol = n_draw)
  ans  
}  
  
  
    


