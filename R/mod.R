## TODO - mod_fert, mod_mort, mod_mig_in, mod_mig_out, mod_lfp, mod_marr

## HAS_TESTS
#' Specify a Poisson model
#'
#' Specify a model where the outcome is drawn from
#' a Poisson distribution. 
#'
#' - `formula` is a standard R [formula][stats::formula()],
#' specifying
#' the outcome variable and predictors, including interactions
#' between predictors.
#' - `data` holds the outcome variable, the predictors,
#' and, optionally, an exposure term.
#' - `exposure` is the name (bare or quoted) of a variable
#' in `data`, or, if the model does not contain an
#' exposure term, a `1`.
#'
#' If the model includes exposure, then the
#' the first level of the model is
#'
#' \deqn{y \sim \text{Poisson}(\mu w)}
#'
#' where \eqn{\mu} is the underlying rate, and
#' \eqn{w} is exposure. If the model does not
#' include exposure, then the first level is
#'
#' \deqn{y \sim \text{Poisson}(\mu)}
#'
#' TODO - Include error term once specification finalised.
#'
#' @param formula An R [formula][stats::formula()],
#' specifying the outcome and predictors.
#' @param data A data frame containing the outcome,
#' predictors, and, optionally, exposure.
#' @param exposure Name of the exposure variable,
#' or a `1` for a model with no exposure.
#'
#' @returns An object of class `bage_mod`. 
#'
#' @seealso
#' - [mod_binom()] and [mod_norm()] for specification
#' of binomial and normal models
#' - [fit()] to fit a model
#' - [simulate()] to create simulated data
#' - [set_prior()] to specify non-default priors
#'
#' @examples
#' ## model with exposure
#' mod <- mod_pois(injuries ~ age:sex + ethnicity + year,
#'                 data = injuries,
#'                 exposure = popn)
#'
#' ## model without exposure
#' mod <- mod_pois(injuries ~ age:sex + ethnicity + year,
#'                 data = injuries,
#'                 exposure = 1)
#' @export
mod_pois <- function(formula, data, exposure) {
    nm_distn <- "pois"
    exposure <- deparse1(substitute(exposure))
    exposure <- gsub("^\\\"|\\\"$", "", exposure)
    is_mod_with_offset <- !identical(exposure, "1")
    if (is_mod_with_offset) {
        vname_offset <- exposure
        nm_offset <- "exposure"
    }
    else {
        vname_offset <- NULL
        nm_offset <- NULL
    }
    new_bage_mod(formula = formula,
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
    new_bage_mod(formula = formula,
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
    new_bage_mod(formula = formula,
                    data = data,
                    nm_distn = nm_distn,
                    is_mod_with_offset = is_mod_with_offset,
                    vname_offset = vname_offset,
                    nm_offset = nm_offset)
}


## HAS_TESTS
#' Create new object of class 'bage_mod'
#'
#' Create object holding information about
#' a system model (ie a model of demographic rates.)
#'
#' `new_bage_mod()` creates components
#' `par`, `term_par`, `matrices_par`
#' and stores them in the model object. These
#' components can all be determined from `formula`
#' and `data`, and do not subsequently change.
#' and do not subsequently change.
#'
#' `new_bage_mod()` does not create components
#' `index_priors`, `hyper`, `term_hyper`,
#' `consts`, or `term_consts`, 
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
#' @returns An object of class `bage_mod`.
#'
#' @noRd
new_bage_mod <- function(formula,
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
        offset <- make_offset_ones(outcome)
    term_par <- make_term_par(formula = formula,
                                outcome = outcome)
    par <- rep(0.0, times = length(term_par))
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
                term_par = term_par,
                matrices_par = matrices_par,
                means = means,
                prec = prec)
    class(ans) <- "bage_mod"
    ans    
}
