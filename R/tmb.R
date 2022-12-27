
estimate_tmb <- function(x, model = "normal") {
    
    f <- TMB::MakeADFun(data = list(x = x),
                        parameters = list(mu = 0),
                        DLL = "bage",
                        silent = TRUE)
    fit <- stats::nlminb(start = f$par,
                         objective = f$fn,
                         gradient = f$gr,
                         silent = TRUE)
    sdreport <- sdreport(f, getJointPrecision=TRUE)
    mu <- unlist(as.list(sdreport, "Est"))
    mu
}
