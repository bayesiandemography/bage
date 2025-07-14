
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

seed_augment <- mod$seed_augment
seed_restore <- make_seed() ## create randomly-generated seed
set.seed(seed_augment) ## set pre-determined seed
vals_expected <- inv_transform(vals_linpred)
is_disp <- vals_components$component == "disp"
vals_disp <- vals_components$.fitted[is_disp]
vals_fitted <- draw_vals_fitted(mod = mod,
                                vals_expected = vals_expected,
                                vals_disp = vals_disp)

## outcome_obs <- rep(NA_real_, times = length(vals_fitted))
## vals_outcome_true <- draw_vals_outcome_true(datamod = datamod_outcome,
##                                             nm_distn = nm_distn,
##                                             outcome_obs = outcome_obs,
##                                             fitted = vals_fitted,
##                                             disp = vals_disp,
##                                             offset = offset)

