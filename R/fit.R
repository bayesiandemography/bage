
## TODO - catch errors, warnings
## TODO - simulate()
    
fit <- function(mod) {
    data <- list(outcome = mod$outcome,
                 offset = mod$offset,
                 index_par = mod$index_par,
                 index_hyper = mod$index_hyper,
                 index_prior = mod$index_prior,
                 map_matrices = mod$map_matrices)
    par <- unlist(par, use.names = FALSE)
    hyper <- unlist(hyper, use.names = FALSE)
    parameters <- list(par = par,
                       hyper = hyper)
    random <- levels(indices_par)
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
    
    
    





    
    
