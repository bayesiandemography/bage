
mod <- mod_pois(divorces ~ age + sex + time,
                data = nzl_divorces,
                exposure = population) |>
  set_n_draw(n_draw = 100)

data <- mod$data
dimnames_terms <- mod$dimnames_terms
n_draw <- mod$n_draw
datamod_outcome <- mod$datamod_outcome
offset <- mod$offset
nm_distn <- nm_distn(mod)
vals_components <- draw_vals_components_unfitted(mod = mod,
                                                 n_sim = n_draw)
inv_transform <- get_fun_inv_transform(mod)
has_disp <- has_disp(mod)
nm_outcome_data <- get_nm_outcome_data(mod)
vals_linpred <- make_linpred_from_components(mod = mod,
                                             components = vals_components,
                                             data = data,
                                             dimnames_terms = dimnames_terms)

