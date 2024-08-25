
fit_inner_outer <- function(mod, vars_inner) {
  use_term <- make_use_term(mod = mod, vars_inner = vars_inner)
  mod_inner <- reduce_model_terms(mod = mod, use_term = use_term)
  mod_inner <- fit_default(mod_inner)
  comp_inner <- components(mod_inner)
  point_est_effects_inner <- make_point_est_effects(comp_inner)
  mod_outer <- set_priors_known(mod = mod, vals = point_est_effects_inner)
  mod_outer <- fit_default(mod_outer)
  mod <- make_stored_draws_inner_outer(mod = mod,
                                       mod_inner = mod_inner,
                                       mod_outer = mod_outer)
  mod
}



  

  


  
  
  
  


