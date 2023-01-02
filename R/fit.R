
## TODO - catch errors, warnings
## TODO - simulate()

#' Fit a model
#'
#' @param An object of class `bage_sysmod`,
#' typically created by a call to [mod_pois()],
#' [mod_binom()], or [mod_norm()].
#'
#' @returns An object of class `bage_sysmod`
#'
#' @export    
fit <- function(mod) {
    priors <- mod$priors
    i_prior <- make_i_prior(priors)
    hyper <- make_hyper(priors)
    term_hyper <- make_term_hyper(priors)
    consts <- make_consts(priors)
    term_consts <- make_term_consts(priors)
    data <- list(outcome = mod$outcome,
                 offset = mod$offset,
                 term_par = mod$term_par,
                 matrices_par = mod$matrices_par,
                 i_prior = i_prior,
                 term_hyper = term_hyper,
                 consts = consts,
                 term_consts = term_consts)
    parameters <- list(par = mod$par,
                       hyper = hyper)
    f <- TMB::MakeADFun(data = data,
                        parameters = parameters,
                        DLL = "bage",
                        random = "par",
                        silent = FALSE)
    fit <- stats::nlminb(start = f$par,
                         objective = f$fn,
                         gradient = f$gr,
                         silent = FALSE)
    sdreport <- TMB::sdreport(f, getJointPrecision = TRUE)
    mod$means <- as.list(sdreport, what = "Est")
    mod$prec <- sdreport$jointPrecision
    mod
}
    
    
    





    
    
