
## mod <- mod_pois_sim(y ~ age * sex + time,
##                     data = data,
##                     exposure = popn) %>%
##     set_prior(`(Intercept)` ~ NFix(sd = 0.3)) %>%
##     set_prior(time ~ SVD(HMD))

## report <- report_sim(mod, n_sim = 100)

## is_mod_valid(report)

report_sim <- function(mod_sim, mod_est = NULL, n_sim = 100) {
    comparisons <- vector(mod = "list", length = n_sim)
    if (is_null(mod_est))
        mod_est <- mod_sim
    for (i_sim in seq_len(n_sim)) {
        vals_true <- draw_vals_true(mod_sim)
        outcome <- vals_true$outcome
        mod_est <- update_mod_outcome(mod = mod,
                                      outcome = outcome)
        mod <- fit(mod)
        vals_est <- get_vals_est(mod)
        comparison <- compare_est_true(vals_true = vals_true,
                                       vals_est = vals_est)
        comparisons[[i_sim]] <- comparison
    }
    make_report_sim(mod = mod,
                    comparisons = comparisons)
}

mod_pois_sim <- function(formula, data, exposure) {
    check_formula_has_response(formula)
    check_response_not_in_data(formula = formula,
                               data = data)
    data <- add_response_to_data(formula = formula,
                                 data = data)
    mod_sim <- mod_pois(formula = formula,
                        data = data,
                        exposure = exposure)
    class(mod_sim) <- c("bage_mod", "bage_mod_pois", "bage_mod_pois_sim")
    mod_sim
}

    
draw_vals_true.bage_mod_pois_sim <- function(mod, n_sim) {
    priors <- mod$priors
    offset <- mod$offset
    has_disp <- has_disp(mod)
    has_season <- has_season(mod)
    lengths_par <- make_lengths_par(mod)
    ## randomly generate true parameter values
    hyper_true <- lapply(priors, draw_hyper_true)
    par_true <- .mapply(draw_par_true,
                        dots = list(prior = priors,
                                    hyper = hyper_true,
                                    length_par = lengths_par),
                        MoreArgs = list())
    linpred <- make_linpred_par_true(mod = mod,
                                     par_true = par_true)
    if (has_season) {
        season_true <- draw_season_true(mod)
        linpred_season <- make_linpred_season_true(mod = mod,
                                                   season_true = season_true)
        linpred <- linpred + linpred_season
    }
    if (has_disp)
        disp <- draw_disp_true(mod)
    else
        disp <- NULL
    param_true <- draw_param_pois_true(mod = mod,
                                       linpred = linpred,
                                       disp = disp)
    outcome <- draw_counts_pois_true(mod = mod,
                                     rate_true = rates_true,
                                     offset = offset)
    list(outcome = outcome,
         param_true = param_true,
         disp_true = disp_true,
         par_true = par_true,
         hyper_true = hyper_true)
}

