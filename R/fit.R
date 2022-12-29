
## TODO - catch errors, warnings
## TODO - simulate()
    
fit <- function(mod) {
    priors <- mod$priors
    i_prior <- make_i_prior(priors)
    hyper <- make_hyper(priors)
    index_hyper <- make_index_hyper(priors)
    random <- names(priors)
    data <- list(outcome = mod$outcome,
                 offset = mod$offset,
                 index_par = mod$index_par,
                 matrices_par = mod$matrices_par,
                 i_prior = i_prior,
                 index_hyper = index_hyper)
    parameters <- list(par = mod$par,
                       hyper = hyper)
    f <- TMB::MakeADFun(data = data,
                        parameters = parameters,
                        DLL = "bage",
                        random = random,
                        silent = TRUE)
    fit <- stats::nlminb(start = f$par,
                         objective = f$fn,
                         gradient = f$gr,
                         silent = TRUE)
    sdreport <- TMB::sdreport(f, getJointPrecision = TRUE)
    mod$params <- TMB::as.list(sdreport, what = "Est")
    mod$prec <- sdreport$jointPrecision
    mod
}
    
    
    





    
    
