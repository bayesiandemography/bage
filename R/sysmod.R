## TODO - mod_fert, mod_mort, mod_mig_in, mod_mig_out, mod_lfp, mod_marr


mod_pois <- function(formula, data, exposure) {
    nm_distn <- "pois"
    exposure <- deparse1(substitute(exposure))
    is_mod_with_offset <- !identical(exposure, "1")
    if (is_mod_with_offset) {
        vname_offset <- exposure
        nm_offset <- "exposure"
    }
    else {
        vname_offset <- NULL
        nm_offset <- NULL
    }
    bage_sysmod(formula = formula,
                data = data,
                nm_distn = nm_distn,
                is_mod_with_offset = is_mod_with_offset,
                vname_offset = vname_offset,
                nm_offset = nm_offset)
}

mod_binom <- function(formula, data, size) {
    nm_distn <- "binom"
    is_mod_with_offset <- TRUE
    vname_offset <- deparse1(substitute(size))
    nm_offset <- "size"    
    bage_sysmod(formula = formula,
                data = data,
                nm_distn = nm_distn,
                is_mod_with_offset = is_mod_with_offset,
                vname_offset = vname_offset,
                nm_offset = nm_offset)
}

## TODO - normal distribution, scale response (so we can keep using scale = 1 as default)
mod_norm <- function(formula, data) {
    nm_distn <- "norm"
    is_mod_with_offset <- FALSE
    vname_offset <- NULL
    nm_offset <- NULL   
    bage_sysmod(formula = formula,
                data = data,
                nm_distn = nm_distn,
                is_mod_with_offset = is_mod_with_offset,
                vname_offset = vname_offset,
                nm_offset = nm_offset)
}

#' par, index_par, matrices_par are all fixed by 'data',
#' and do not change. These are therefore all
#' part of the 'mod' object.
#'
#' priors, and hence index_priors, hyper, and index_hyper
#' can change via 'set_prior'. Only 'priors' is held
#' in the 'mod' object - all the other quantities
#' that depend on them are derived on the fly
#' within function 'fit'.
#' 
new_bage_sysmod <- function(formula,
                            data,
                            nm_distn,
                            is_mod_with_offset,
                            vname_offset,
                            nm_offset) {
    distns_response_nonneg <- c("pois", "binom")
    is_distn_response_nonneg <- nm_distn %in% distns_response_nonneg
    ## check individual inputs
    checkmate::assert_formula(formula)
    check_formula_has_response(formula)
    check_formula_has_predictors(formula)
    checkmate::assert_data_frame(data)
    ## check consistency between inputs
    check_formula_vnames_in_data(formula = formula,
                                 data = data)
    if (is_distn_response_nonneg)
        check_response_nonneg(formula = formula,
                              data = data,
                              nm_distn)
    if (is_mod_with_offset) {
        check_offset_in_data(vname_offset = vname_offset,
                             nm_offset = nm_offset,
                             data = data)
        check_offset_nonneg(vname_offset = vname_offset,
                            nm_offset = nm_offset,
                            data = data)
    }
    ## make components
    outcome <- make_outcome(formula = formula,
                            data = data)
    if (is_mod_with_offset)
        offset <- make_offset(vname_offset = vname_offset,
                              data = data)
    else
        offset <- NULL
    index_par <- make_index_par(formula = formula,
                                outcome = outcome)
    par <- make_par(index_par)
    priors <- make_priors(formula)
    matrices_par <- make_matrices_par(formula = formula,
                                      outcome = outcome)
    means <- NULL
    cov <- NULL
    ## create object and return
    ans <- list(formula = formula,
                data = data,
                nm_distn = nm_distn,
                outcome = outcome,
                offset = offset,
                nm_offset = nm_offset,
                priors = priors,
                par = par,
                index_par = index_par,
                matrices_par = matrices_par,
                means = means,
                cov = cov)
    class(ans) <- "bage_sysmod"
    ans    
}
