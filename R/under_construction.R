

## TODO - normal distribution, scale response (so we can keep using scale = 1 as default)
## TODO - implement 'set_simdata' function
## TODO - set_disp()

new_bage_sysmod <- function(formula,
                            data,
                            y,
                            nm_offset,
                            offset,
                            params,
                            priors) {
    ans <- list(formula = formula,
                distn = distn,
                vname_offset = vname_offset,
                y = NULL,
                
                data = NULL,
                priors = list(),
                params = NULL,
                cov = NULL,
                prec = NULL,
                se = NULL)
    class(ans) <- "bage_sysmod"
    ans
}



mod_pois <- function(formula, data, exposure) {
    ## check individual inputs
    checkmate::assert_formula(formula)
    check_formula_has_response(formula)
    check_formula_has_predictors(formula)
    checkmate::assert_data_frame(data)
    exposure <- deparse1(substitute(exposure))
    ## check consistency between inputs
    check_formula_vnames_in_data(formula = formula,
                                 data = data)
    check_exposure_in_data(exposure = exposure,
                           data = data)
    check_response_nonneg(formula = formula,
                          data = data)
    check_exposure_nonneg(formula = formula,
                          data = data)
    ## make components
    y <- make_y(formula = formula,
                data = data)
    exposure <- make_exposure(exposure = exposure,
                              data = data)
    priors <- make_priors(formula)
    params <- make_params(formula = formula,
                          data = data)
    new_bage_sysmod(formula = formula,
                    distn = "pois",
                    y = y,
                    offset = exposure,
                    priors = priors)
}

norm <- function(scale = 1) {
    scale <- check_and_tidy_scale(scale)
    new_bage_prior_norm(scale = scale)
}

new_bage_prior_norm <- function(scale = 1) {
    ans <- list(scale = scale,
                n_hyper = 1L)
    class(ans) <- c("bage_prior_norm", "bage_prior")
    ans
}
    
    
    
## check_formula_has_response <- function(formula) {
##     has_response <- attr(stats::terms(formula), "response")
##     if (!has_response)
##         stop(gettextf("formula '%s' does not include a response",
##                       deparse(formula)),
##              call. = FALSE)
##     invisible(TRUE)
## }
    


set_data <- function(mod, data) {
    formula <- mod$formula
    mod$data <- data
    mod$y <- make_y(formula = formula,
                    data = data)
    mod$offset <- make_offset(formula = formula,
                              data = data,
                              offset = offset,
                              distn = distn)
    mod$params <- make_params(formula = formula,
                              data = data)
    mod
}
                      


## check_formula_vnames_in_data <- function(formula, data) {
##     nms_formula <- rownames(attr(stats::terms(formula), "factors"))
##     nms_data <- names(data)
##     is_in_data <- nms_formula %in% nms_data
##     i_not_in_data <- match(FALSE, is_in_data, nomatch = 0L)
##     if (i_not_in_data > 0L)
##         stop(gettextf("term '%s' from formula '%s' not found in '%s'",
##                       nms_formula[[i_not_in_data]],
##                       deparse(formula),
##                       "data"),
##              call. = FALSE)
##     invisible(TRUE)
## }


## set_prior(mod, formula) {
##     check_valid_prior_formula(formula)
##     response_name <- as.character(formula[[2L]])
##     prior <- eval(formula[[3L]])
##     mod$priors <- c(mod$priors,
##                     setNames(list(prior), nm = response_name))
##     mod
## }

## RW <- function(scale = 1) {
##     checkmate::assert_number(scale)
##     if (scale <= 0)
##         stop(gettextf("value for '%s' [%s] is non-positive",
##                       "scale", scale),
##              call. = FALSE)
##     new_bage_prior_rw(scale)
## }


## new_bage_prior_rw <- function(scale) {
##     ans <- list(scale = scale)
##     class(ans) <- c("bage_prior_rw", "bage_prior")
##     ans
## }
    


## fit <- function(mod) {
##     formula <- mod$formula
##     data <- mod$data

##     formula_xtabs_response <- make_formula_xtabs_response(formula)
##     has_offset <- !is.null(mod$offset)
##     if (has_offset)
##         formula_xtabs_offset <- make_formula_xtabs_offset(formula = formula,
##                                                           offset = offset)
    

##     formula_no_interact <- make_formula_no_interact(formula)
    
##     formula_xtabs <- make_formula_xtabs(formula)



    

    
##     f <- TMB::MakeADFun(data = data)
##                         parameters = list(mu = 0),
##                         DLL = "bage",
##                         silent = TRUE) ## TODO include random effects
##     fit <- stats::nlminb(start = f$par,
##                          objective = f$fn,
##                          gradient = f$gr,
##                          silent = TRUE)
##     sdreport <- sdreport(f,
##                          getJointPrecision = TRUE) ## TODO depends on random effects
##     mod$params <- as.list(sdreport, what = "Est")
##     mod$prec <- mod$jointPrecision
##     mu
## }
    
    
    





    
    
