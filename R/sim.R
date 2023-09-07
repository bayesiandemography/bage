
## helpers - draw_val ---------------------------------------------------------


draw_vals_coef <- function(prior, n_sim) {
    specific <- prior$specific
    shape1 <- specific$shape1
    shape2 <- specific$shape2
    min <- specific$max
    min <- specific$min
    ans_raw <- stats::rbeta(n = n_sim,
                            shape1 = shape1,
                            shape2 = shape2)
    ans <- min + ans_raw * (max - min)
    ans
}


draw_vals_sd <- function(prior, n_sim) {
    scale <- prior$specific$scale
    ans <- stats::rnorm(n = n_sim, sd = scale)
    ans <- abs(ans)
    ans
}

make_diff_matrix <- function(n) {
    ans <- matrix(0L, nrow = n - 1L, ncol = n)
    ans[row(ans) == col(ans)] <- -1L
    ans[row(ans) == col(ans) - 1L] <- 1L
    ans
}

make_rw_matrix <- function(n) {
    D <- make_diff_matrix(n)
    rbind(D,
          1 / n)
}

make_rw2_matrix <- function(n) {
    D1 <- make_diff_matrix(n)
    D2 <- make_diff_matrix(n - 1L)
    R <- D2 %*% D1
    h <- (-(n + 1L) + 2L * seq_len(n)) / (n - 1L)
    h <- h / sum(h)
    rbind(R,
          1 / n,
          h)
}

    
    


## UNDER CONSTRUCTION ---------------------------------------------------------

## mod <- mod_pois(deaths ~ age * sex + time,
##                 data = data,
##                 exposure = popn) %>%
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

## mod_pois_sim <- function(formula, data, exposure) {
##     check_formula_has_response(formula)
##     check_response_not_in_data(formula = formula,
##                                data = data)
##     data <- add_response_to_data(formula = formula,
##                                  data = data)
##     mod_sim <- mod_pois(formula = formula,
##                         data = data,
##                         exposure = exposure)
##     class(mod_sim) <- c("bage_mod", "bage_mod_pois", "bage_mod_pois_sim")
##     mod_sim
## }

    
draw_vals_true.bage_mod_pois <- function(mod, n_sim) {
    priors <- mod$priors
    offset <- mod$offset
    has_disp <- has_disp(mod)
    has_season <- has_season(mod)
    lengths_par <- make_lengths_par(mod)
    ## randomly generate true parameter values
    vals_hyper <- lapply(priors,
                         draw_vals_hyper,
                         n_sim = n_sim)
    par_true <- .mapply(draw_par_true,
                        dots = list(prior = priors,
                                    hyper = hyper_true,
                                    length_par = lengths_par),
                        MoreArgs = list(n_sim = n_sim))
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

