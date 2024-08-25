
fit_inner_outer <- function(mod, vars_inner) {
  use_term <- make_use_term(mod = mod, vars_inner = vars_inner)
  mod_inner <- reduce_model_terms(mod = mod, use_term = use_term)
  mod_inner <- fit_default(mod_inner)
  comp_inner <- components(mod_inner)
  point_est_effects_inner <- make_point_est_effects(comp_inner)
  mod_outer <- set_priors_known(mod = mod, vals = point_est_effects_inner)
  use_term[] <- TRUE
  mod_outer <- fit_default(mod_outer)
  mod <- make_stored_draws_inner_outer(mod = mod,
                                       mod_inner = mod_inner,
                                       mod_outer = mod_outer)
  mod
}



  

#' @noRd
make_stored_draws_inner_outer <- function(mod, mod_inner, mod_outer, use_term) {
  priors <- mod$priors
  n_term <- length(priors)
  nms_term <- names(priors)
  draws_effectfree <- vector(mode = "list", length = n_term)
  draws_hyper <- vector(mode = "list", length = n_term)
  draws_hyperrand <- vector(mode = "list", length = n_term)
  terms_effectfree_inner <- make_terms_effectfree(mod_inner)
  terms_effectfree_outer <- make_terms_effectfree(mod_outer)
  terms_hyper_inner <- make_terms_hyper(mod_inner)
  terms_hyper_outer <- make_terms_hyper(mod_outer)
  terms_hyperrand_inner <- make_terms_hyperrand(mod_inner)
  terms_hyperrand_outer <- make_terms_hyperrand(mod_outer)
  for (i_term in seq_len(n_term)) {
    nm_term <- nms_term[[i_term]]
    use_inner <- use_term[[i_term]]
    if (use_inner) {
      is_effectfree <- terms_effectfree_inner == nm_term
      draws_effectfree[[i_term]] <- draws_effectfree_inner[is_effectfree, ]
      is_hyper <- terms_hyper_inner == nm_term
      draws_hyper[[i_term]] <- draws_hyper_inner[is_hyper, ]
      is_hyperrand <- terms_hyperrand_inner == nm_term
      draws_hyperrand[[i_term]] <- draws_hyperrand_inner[is_hyperrand, ]
    }
    else {
      is_effectfree <- terms_effectfree_outer == nm_term
      draws_effectfree[[i_term]] <- draws_effectfree_outer[is_effectfree, ]
      is_hyper <- terms_hyper_outer == nm_term
      draws_hyper[[i_term]] <- draws_hyper_outer[is_hyper, ]
      is_hyperrand <- terms_hyperrand_outer == nm_term
      draws_hyperrand[[i_term]] <- draws_hyperrand_outer[is_hyperrand, ]
    }
  }
  draws_effectfree <- vctrs::vec_rbind(!!!draws_effectfree)
  draws_hyper <- vctrs::vec_rbind(!!!draws_hyper)
  draws_hyperrand <- vctrs::vec_rbind(!!!draws_hyperrand)
  mod$draws_effectfree <- draws_effectfree
  mod$draws_hyper <- draws_hyper
  mod$draws_hyperrand <- draws_hyperrand
  if (has_disp(mod))
    mod$draws_disp <- mod_outer$draws_disp
  mod
}

  


  
  
  
  


