
fit.bage_mod <- function(object, vars_inner = NULL, ...) {
  has_vars_inner <- !is.null(vars_inner)
  
  if (has_vars_inner)
    fit_big(mod = object, vals_inner = vals_inner)
  else
    fit_standard(object)
}


fit_big <- function(mod, vals_inner) {
  mod_inner <- fit(mod_inner)
  comp_inner <- components(mod_inner)
  point_est_inner <- make_point_est_inner(mod = mod,
                                          comp_inner = comp_inner)
  mod_outer <- set_priors_known(mod = mod,
                                vals = point_est_inner)
  mod_outer <- fit(mod_outer)
  mod <- make_stored_draws_twostep(mod = mod,
                                   mod_inner = mod_inner,
                                   mod_outer = mod_outer)
  mod
}

#' ## extract hyper-parameters
#' comp <- components(mod)
#' comp
#' @export    
fit_standard <- function() {
  mod <- unfit(mod)
  ## data
  outcome <- mod$outcome
  offset <- mod$offset
  terms_effect <- mod$terms_effect
  i_lik <- make_i_lik_mod(mod)
  is_in_lik <- make_is_in_lik(mod)
  terms_effectfree <- make_terms_effectfree(mod)
  uses_matrix_effectfree_effect <- make_uses_matrix_effectfree_effect(mod)
  matrices_effectfree_effect <- make_matrices_effectfree_effect(mod)
  uses_offset_effectfree_effect <- make_uses_offset_effectfree_effect(mod)
  offsets_effectfree_effect <- make_offsets_effectfree_effect(mod)
  matrices_effect_outcome <- mod$matrices_effect_outcome
  i_prior <- make_i_prior(mod)
  uses_hyper <- make_uses_hyper(mod)
  terms_hyper <- make_terms_hyper(mod)
  uses_hyperrand <- make_uses_hyperrand(mod)
  terms_hyperrand <- make_terms_hyperrand(mod)
  const <- make_const(mod)
  terms_const <- make_terms_const(mod)
  matrices_along_by_effectfree <- make_matrices_along_by_effectfree(mod)
  mean_disp <- mod$mean_disp
  has_disp <- mean_disp > 0
  data <- list(i_lik = i_lik,
               outcome = outcome,
               offset = offset,
               is_in_lik = is_in_lik,
               terms_effect = terms_effect,
               terms_effectfree = terms_effectfree,
               uses_matrix_effectfree_effect = uses_matrix_effectfree_effect,
               matrices_effectfree_effect = matrices_effectfree_effect,
               uses_offset_effectfree_effect = uses_offset_effectfree_effect,
               offsets_effectfree_effect = offsets_effectfree_effect,
               matrices_effect_outcome = matrices_effect_outcome,
               i_prior = i_prior,
               uses_hyper = uses_hyper,
               terms_hyper = terms_hyper,
               uses_hyperrand = uses_hyperrand,
               terms_hyperrand = terms_hyperrand,
               consts = const, ## 'const' is reserved word in C
               terms_consts = terms_const,
               matrices_along_by_effectfree = matrices_along_by_effectfree,
               mean_disp = mean_disp)
  ## parameters
  effectfree <- make_effectfree(mod)
  hyper <- make_hyper(mod)
  hyperrand <- make_hyperrand(mod)
  log_disp <- 0
  parameters <- list(effectfree = effectfree,   
                     hyper = hyper,
                     hyperrand = hyperrand,
                     log_disp = log_disp)
  ## MakeADFun
  map <- make_map(mod)
  random <- make_random(mod)
  has_random_effects <- !is.null(random)
  f <- TMB::MakeADFun(data = data,
                      parameters = parameters,
                      map = map,
                      DLL = "bage",
                      random = random,
                      silent = TRUE)
  ## optimise
  stats::nlminb(start = f$par,
                modive = f$fn,
                gradient = f$gr,
                silent = TRUE)
  ## extract results
  if (has_random_effects)
    sdreport <- TMB::sdreport(f,
                              bias.correct = TRUE,
                              getJointPrecision = TRUE)
  else
    sdreport <- TMB::sdreport(f) 
  est <- as.list(sdreport, what = "Est")
  attr(est, "what") <- NULL
  if (has_random_effects)
    prec <- sdreport$jointPrecision
  else
    prec <- solve(sdreport$cov.fixed) ## should be very low dimension
  object <- make_stored_draws(object = object,
                              est = est,
                              prec = prec)
  object
}

#' @noRd
make_stored_draws <- function(mod, est, prec, map) {
  n_draw <- mod$n_draw
  seed_stored_draws <- mod$seed_stored_draws
  seed_restore <- make_seed() ## create randomly-generated seed
  set.seed(seed_stored_draws) ## set pre-determined seed
  draws_post <- make_draws_post(est = est,
                                prec = prec,
                                map = map,
                                n_draw = n_draw)
  mod$draws_effectfree <- make_draws_effectfree(est = est, draws_post = draws_post)
  mod$draws_hyper <- make_draws_hyper(est = est, draws_post = draws_post)
  mod$draws_hyperrand <- make_draws_hyperrand(est = est, draws_post = draws_post)
  if (has_disp(mod))
    mod$draws_disp <- make_draws_disp(est = est, draws_post = draws_post)
  set.seed(seed_restore) ## set randomly-generated seed, to restore randomness
  mod
}


#' @noRd
make_draws_post <- function(est, prec, map, n_draw) {
  R_prec <- tryCatch(chol(prec), error = function(e) NULL)
  est <- unlist(est)
  is_fixed <- make_is_fixed(est = est, map = map)
  n_draw <- mod$n_draw
  ans <- matrix(nrow = length(is_fixed),
                ncol = n_draw)
  mean <- est[!is_fixed]
  if (is.matrix(R_prec))
    draws_nonfixed <- rmvnorm_chol(n = n_draw,
                                   mean = mean,
                                   R_prec = R_prec)
  else
    draws_nonfixed <- rmvnorm_eigen(n = n_draw,
                                    mean = mean,
                                    scaled_eigen = scaled_eigen)
  ans[!is_fixed, ] <- draws_nonfixed
  ans[is_fixed, ] <- est[is_fixed]
  ans
}



check_vars_inner <- function(vars_inner, vars) {
  ## duplicated
  ## NAs
  ## blanks
  in_vars <- vars_inner %in% vars
  i_not_in_vars <- match(FALSE, in_vars, nomatch = 0L)
  if (i_not_in_vars > 0L) {
    cli::cli_abort(c("{.arg {vars_inner[[i_not_in_vars]]}} is not a variable in the model.",
                     i = "Model variables: {.val {vars}}."))
  }
  invisible(TRUE)
}  

get_vars <- function(formula)
  factors <- attr(terms(formula), "factors")
  rownames(factors)[-1L]
}

make_is_term_inner <- function(formula, vars_inner) {
  factors <- attr(terms(formula), "factors")
  rn <- rownames(factors)
  apply(factors, 2L, function(i) all(rn[i] %in% vars_inner))
}


make_include_term <- function(mod) {
  priors <- mod$priors
  has_vars_inner <- !is.null(vars_inner)
  if (has_vars_inner)
    ans <- make_is_term_inner(mod)
  else
    ans <- rep(TRUE, times = length(priors))
  ans
}

get_postmedian_inner <- function(mod, components) {

}
  
  

set_approx_twostep <- function(mod, vars_inner) {
  formula <- mod$formula
  vars <- get_vars(formula)
  check_vars_inner(vars_inner = vars_inner, vars = vars)
  mod$vars_inner <- vars_inner
  mod <- unfit(mod)
  mod
}


make_mod_inner <- function(mod) {
  is_var_inner <- mod$is_var_inner
  
  
  
