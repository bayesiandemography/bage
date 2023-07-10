
## Note that methods require an '@export' tag,
## even when the generic function is not exported
## https://github.com/r-lib/devtools/issues/2293


## 'augment' ------------------------------------------------------------------

#' @importFrom generics augment
#' @export
generics::augment

## HAS_TESTS
#' Data and values from a fitted model
#'
#' @section Warning:
#' The tidymodels
#' [website](https://www.tidymodels.org/learn/develop/broom/)
#' states that the augment function may change soon.
#' The method here will be updated to match the new interface.
#' 
#' @param x A fitted `bage_mod` object.
#' @param ... Unused. Included for generic consistency only.
#'
#' @returns A [tibble][tibble::tibble-package],
#' consisting of the original `data` argument,
#' to the original model function (eg [mod_pois()])
#' plus two new columns:
#' - `.fitted` An [rvec][rvec::rvec()] holding
#' draws from the posterior distribution.
#' - `.observed` Direct estimates of the rate,
#' probability, or mean.
#'
#' @examples
#' mod_pois(injuries ~ age + sex + year,
#'          data = injuries,
#'          exposure = popn) |>
#'   fit() |>
#'   augment()
#' @export
augment.bage_mod <- function(x, ...) {
    ans <- x$data
    is_fitted <- is_fitted(x)
    if (is_fitted) {
        draws <- make_draws_fitted(x)
        ans[[".fitted"]] <- rvec::rvec_dbl(draws)
    }
    ans[[".observed"]] <- make_observed(x)
    ans <- tibble(ans)
    ans
}


## 'components' ---------------------------------------------------------------

#' @importFrom generics components
#' @export
generics::components

## HAS_TESTS
#' Extract components from a fitted model
#'
#' Extract components from a fitted object
#' of class `bage_mod`.
#'
#' There are two types of component:
#' - `"par"` Intercept, main effects, and interactions.
#' - `"hyper"` Hyper-parameters from priors for intercept,
#' main effects, and interactions.
#'
#' For each component, `components()` returns three things:
#' - `term` Name of the intercept, main effect, or interaction
#' - `level` Element of term
#' - `value` An [rvec][rvec::rvec()] containing
#' draws from the posterior distribution.
#'
#' @param object An fitted model.
#' @param ... Not currently used.
#'
#' @returns A [tibble][tibble::tibble-package] with
#' variables `component`, `term`, `level`, and `value`.
#'
#' @seealso [augment()], [tidy()]
#'
#' @examples
#' mod <- mod_pois(injuries ~ age + sex + year,
#'                 data = injuries,
#'                 exposure = popn) |>
#'   fit()
#' mod
#' components(mod)
#' @export
components.bage_mod <- function(object, ...) {
    is_fitted <- is_fitted(object)
    if (is_fitted) {
        par <- components_par(object)
        hyper <- components_hyper(object)
        vctrs::vec_rbind(par, hyper)
    }
    else
        NULL
}
    

## 'fit' ----------------------------------------------------------------------

#' @importFrom generics fit
#' @export
generics::fit

## HAS_TESTS
#' Fit a model
#'
#' @param object A `bage_mod` object,
#' typically created with [mod_pois()],
#' [mod_binom()], or [mod_norm()].
#' @param ... Not currently used.
#'
#' @returns A `bage_mod` object
#'
#' @export    
fit.bage_mod <- function(object, ...) {
    outcome <- object$outcome
    offset <- object$offset
    matrices_par_outcome <- object$matrices_par_outcome
    nm_distn <- nm_distn(object)
    is_in_lik <- make_is_in_lik(object)
    i_prior <- make_i_prior(object)
    const <- make_const(object)
    terms_const <- make_terms_const(object)
    hyper <- make_hyper(object)
    terms_hyper <- make_terms_hyper(object)
    uses_matrix_parfree_par <- make_uses_matrix_parfree_par(object)
    matrices_parfree_par <- make_matrices_parfree_par(object)
    uses_offset_parfree_par <- make_uses_offset_parfree_par(object)
    offsets_parfree_par <- make_offsets_parfree_par(object)
    terms_par <- make_terms_par(object)
    terms_parfree <- make_terms_parfree(object)
    parfree <- make_parfree(object)
    map <- make_map(object)
    data <- list(nm_distn = nm_distn,
                 outcome = outcome,
                 offset = offset,
                 is_in_lik = is_in_lik,
                 terms_par = terms_par,
                 terms_parfree = terms_parfree,
                 uses_matrix_parfree_par = uses_matrix_parfree_par,
                 matrices_parfree_par = matrices_parfree_par,
                 uses_offset_parfree_par = uses_offset_parfree_par,
                 offsets_parfree_par = offsets_parfree_par,
                 matrices_par_outcome = matrices_par_outcome,
                 i_prior = i_prior,
                 terms_hyper = terms_hyper,
                 consts = const,             ## in TMB template refer to 'consts', 
                 terms_consts = terms_const) ## not 'const', because 'const' is a 
    parameters <- list(parfree = parfree,    ## reserved word
                       hyper = hyper)
    f <- TMB::MakeADFun(data = data,
                        parameters = parameters,
                        map = map,
                        DLL = "bage",
                        random = "parfree",
                        silent = TRUE)
    stats::nlminb(start = f$par,
                  objective = f$fn,
                  gradient = f$gr,
                  silent = TRUE)
    sdreport <- TMB::sdreport(f,
                              bias.correct = TRUE,
                              getJointPrecision = TRUE)
    est <- as.list(sdreport, what = "Est")
    attr(est, "what") <- NULL
    object$est <- est
    object$prec <- sdreport$jointPrecision
    object
}


## 'get_fun_inv_transform' ----------------------------------------------------

#' Get function to calculate inverse tranformation
#'
#' @param mod An object of class 'bage_mod'
#'
#' @returns A function
#'
#' @noRd
get_fun_inv_transform <- function(mod) {
    UseMethod("get_fun_inv_transform")
}

## HAS_TESTS
#' @export
get_fun_inv_transform.bage_mod_pois <- function(mod)
    exp

## HAS_TESTS
#' @export
get_fun_inv_transform.bage_mod_binom <- function(mod)
    function(x) ifelse(x > 0, 1 / (1 + exp(-x)), exp(x) / (1 + exp(x)))

## HAS_TESTS
#' @export
get_fun_inv_transform.bage_mod_norm <- function(mod) identity


## 'get_fun_scale_outcome' ----------------------------------------------------

#' Get function to scale outcome, if necessary
#'
#' Get function to scale outcome, if necessary.
#' The scaling consists of multiplying by the sd
#' of the original outcome, and then adding the
#' mean. Applied only to the normal model.
#' In other cases, the function returned is the
#' identity function.
#'
#' @param mod Object of class "bage_mod"
#'
#' @returns TRUE or FALSE
#'
#' @noRd
get_fun_scale_outcome <- function(mod) {
    UseMethod("get_fun_scale_outcome")
}

#' @export
get_fun_scale_outcome.bage_mod <- function(mod) identity

#' @export
get_fun_scale_outcome.bage_mod_norm <- function(mod) {
    mean <- mod$outcome_mean
    sd <- mod$outcome_sd
    function(x) x * sd + mean
}


## 'is_fitted' ----------------------------------------------------------------

#' Test whether a model has been fitted
#'
#' Test whether [fit()][fit.bage_mod] has been
#' called on a model object.
#'
#' @param x A model object.
#'
#' @returns `TRUE` or `FALSE`
#'
#' @examples
#' mod <- mod_pois(injuries ~ age + sex + year,
#'                 data = injuries,
#'                 exposure = popn)
#' is_fitted(mod)
#' mod <- fit(mod)
#' is_fitted(mod)
#' @export
is_fitted <- function(x) {
    UseMethod("is_fitted")
}

## HAS_TESTS
#' @export
is_fitted.bage_mod <- function(x)
    !is.null(x$est)


## 'model_descr' -----------------------------------------------------------------

#' Name of distribution used in printing
#'
#' @param mod An object of class 'bage_mod'
#'
#' @returns A string
#'
#' @noRd
model_descr <- function(mod) {
    UseMethod("model_descr")
}

## HAS_TESTS
#' @export
model_descr.bage_mod_pois <- function(mod) "Poisson"

## HAS_TESTS
#' @export
model_descr.bage_mod_binom <- function(mod) "binomial"

## HAS_TESTS
#' @export
model_descr.bage_mod_norm <- function(mod) "normal"


## 'nm_distn' -----------------------------------------------------------------

#' Name of distribution used internally
#'
#' @param mod Object of class "bage_mod"
#'
#' @returns A string
#'
#' @noRd
nm_distn <- function(mod) {
    UseMethod("nm_distn")
}

## HAS_TESTS
#' @export
nm_distn.bage_mod_pois <- function(mod) "pois"

## HAS_TESTS
#' @export
nm_distn.bage_mod_binom <- function(mod) "binom"

## HAS_TESTS
#' @export
nm_distn.bage_mod_norm <- function(mod) "norm"


## 'nm_distn' -----------------------------------------------------------------

#' Name of offset used in printing
#'
#' @param mod An object of class 'bage_mod'
#'
#' @returns A string
#'
#' @noRd
nm_offset <- function(mod) {
    UseMethod("nm_offset")
}

## HAS_TESTS
#' @export
nm_offset.bage_mod_pois <- function(mod) "exposure"

## HAS_TESTS
#' @export
nm_offset.bage_mod_binom <- function(mod) "size"

## HAS_TESTS
#' @export
nm_offset.bage_mod_norm <- function(mod) "weights"


## 'print' --------------------------------------------------------------------

#' @export
print.bage_mod <- function(x, ...) {
    nchar_offset <- 10
    ## calculations
    formula <- x$formula
    priors <- x$priors
    n_draw <- x$n_draw
    data <- x$data
    vname_offset <- x$vname_offset
    var_age <- x$var_age
    var_time <- x$var_time
    is_fitted <- is_fitted(x)
    str_title <- sprintf("-- %s %s model --",
                         if (is_fitted) "Fitted" else "Unfitted",
                         model_descr(x))
    nms_priors <- names(priors)
    nchar_response <- nchar(as.character(formula[[2L]]))
    nchar_max <- max(nchar(nms_priors), nchar_response)
    padding_formula <- paste(rep(" ", nchar_max - nchar_response),
                             collapse = "")
    str_var_age <- if (is.null(var_age)) "<not detected>" else var_age
    str_var_time <- if (is.null(var_time)) "<not detected>" else var_time
    nms_priors <- sprintf("% *s", nchar_max, nms_priors)
    calls_priors <- vapply(priors, str_call_prior, "")
    str_priors <- paste(nms_priors, calls_priors, sep = " ~ ")
    str_priors <- paste(str_priors, collapse = "\n")
    has_offset <- !is.null(vname_offset)
    if (has_offset) {
        nm_offset <- nm_offset(x)
        nm_offset <- sprintf("% *s", nchar_offset, nm_offset)
        str_offset <- sprintf("%s: %s", nm_offset, vname_offset)
    }
    ## printing
    cat(str_title)
    cat("\n\n")
    cat(padding_formula)
    cat(paste(deparse(formula), collapse = "\n"))
    cat("\n")
    cat(str_priors)
    cat("\n\n")
    if (has_offset) {
        cat(str_offset)
        cat("\n")
    }
    cat(sprintf("% *s: %s",
                nchar_offset,
                "var_age",
                str_var_age))
    cat("\n")
    cat(sprintf("% *s: %s",
                nchar_offset,
                "var_time",
                str_var_time))
    cat("\n")
    cat(sprintf("% *s: %d",
                nchar_offset,
                "n_draw",
                n_draw))
    cat("\n")
    ## return
    invisible(x)
}


## 'tidy' ---------------------------------------------------------------------

#' @importFrom generics tidy
#' @export
generics::tidy

## HAS_TESTS
#' Main effects and interactions from a fitted model
#'
#' @param x A fitted `bage_mod` object.
#' @param ... Unused. Included for generic consistency only.
#'
#' @returns A [tibble][tibble::tibble-package].
#'
#' @seealso [glimpse()] provides less detailed information,
#' and [augment()] provides more detailed.
#'
#' @examples
#' mod <- mod_pois(injuries ~ age + sex + year,
#'                 data = injuries,
#'                 exposure = popn)
#' mod <- fit(mod)
#' tidy(mod)
#' @export
tidy.bage_mod <- function(x, ...) {
    priors <- x$priors
    term <- names(priors)
    spec <- vapply(priors, str_call_prior, "")
    n <- make_lengths_par(x)
    ans <- tibble::tibble(term, spec, n)
    is_fitted <- is_fitted(x)
    if (is_fitted) {
        par <- x$est$par
        terms <- make_terms_par(x)
        par <- split(par, terms)
        ans[["sd"]] <- vapply(par, stats::sd, 0)
    }
    ans <- tibble::tibble(ans)
    ans
}

