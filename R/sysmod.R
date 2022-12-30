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


## HAS_TESTS
#' Create new object of class 'bage_sysmod'
#'
#' Create object holding information about
#' a system model (ie a model of demographic rates.)
#'
#' `new_bage_sysmod()` creates components
#' `par`, `index_par`, `matrices_par`
#' and stores them in the model object. These
#' components can all be determined from `formula`
#' and `data`, and do not subsequently change.
#' and do not subsequently change.
#'
#' `new_bage_sysmod()` does not create components
#' `index_priors`, `hyper`, or `index_hyper`,
#' since these depend on `priors` which can
#' subsequently change via a call to `set_prior()`.
#' Instead, these components are all derived by
#' function `fit()`, just before the model is fitted.
#'
#' @param formula Formula for the model terms
#' @param data Data frame holding data used to
#' create `outcome` and `offset`.
#' @param nm_distn Name of distribution:
#' "pois", "binom", or "norm".
#' @param is_mod_with_offset Does the model
#' contain some sort of offset.
#' @param vname_offset The name of the variable
#' in `data` used to calculate the offset.
#' @param nm_offset The name used to refer to the
#' offset by user-visible functions: "exposure"
#' or "size".
#'
#' @returns An object of class `bage_sysmod`.
#'
#' @noRd
new_bage_sysmod <- function(formula,
                            data,
                            nm_distn,
                            is_mod_with_offset,
                            vname_offset,
                            nm_offset) {
    is_distn_response_nonneg <- nm_distn %in% c("pois", "binom")
    ## check individual inputs supplied by user
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
                              nm_distn = nm_distn)
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
                            data = data,
                            nm_distn = nm_distn)
    if (is_mod_with_offset)
        offset <- make_offset(formula = formula,
                              vname_offset = vname_offset,
                              data = data)
    else
        offset <- NULL
    index_par <- make_index_par(formula = formula,
                                outcome = outcome)
    par <- rep(0.0, times = length(index_par))
    priors <- make_priors(formula)
    matrices_par <- make_matrices_par(formula = formula,
                                      outcome = outcome)
    means <- NULL
    prec <- NULL
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
                prec = prec)
    class(ans) <- "bage_sysmod"
    ans    
}
