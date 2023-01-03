
## TODO - catch errors, warnings
## TODO - simulate()

#' Fit a model
#'
#' @param mod An object of class `bage_mod`,
#' typically created by a call to [mod_pois()],
#' [mod_binom()], or [mod_norm()].
#'
#' @returns An object of class `bage_mod`
#'
#' @export    
fit <- function(mod) {
    priors <- mod$priors
    i_prior <- make_i_prior(priors)
    hyper <- make_hyper(priors)
    term_hyper <- make_term_hyper(priors)
    consts <- make_consts(priors)
    term_consts <- make_term_consts(priors)
    data <- list(nm_distn = mod$nm_distn,
                 outcome = mod$outcome,
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
                        silent = TRUE)
    fit <- stats::nlminb(start = f$par,
                         objective = f$fn,
                         gradient = f$gr,
                         silent = TRUE)
    sdreport <- TMB::sdreport(f,
                              bias.correct = TRUE,
                              getJointPrecision = TRUE)
    est <- as.list(sdreport, what = "Est")
    std <- as.list(sdreport, what = "Std")
    attr(est, "what") <- NULL
    attr(std, "what") <- NULL
    mod$est <- est
    mod$std <- std
    mod$prec <- sdreport$jointPrecision
    mod
}
    
    
    





    
    
