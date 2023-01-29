
## Note that methods require an '@export' tag,
## even when the generic function is not exported
## https://github.com/r-lib/devtools/issues/2293


## 'augment' ------------------------------------------------------------------

#' @importFrom generics augment
#' @export
generics::augment

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
#' plus four new columns:
#' - `.fitted` Point estimates (posterior medians) of
#' the rate, probability, or mean.
#' - `.lower`, `.upper` Lower and upper bounds of
#' 95% credible intervals for the rate, probability,
#' or mean.
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
    is_fitted <- !is.null(x$est)
    if (is_fitted) {
        draws <- make_draws_fitted(x)
        quantiles <- matrixStats::rowQuantiles(draws,
                                               probs = c(0.025, 0.5, 0.975))
        ans$.fitted <- quantiles[, 2L]
        ans$.lower <- quantiles[, 1L]
        ans$.upper <- quantiles[, 3L]
    }
    ans$.observed <- make_observed(x)
    ans
}



## 'fit' ----------------------------------------------------------------------

#' @importFrom generics fit
#' @export
generics::fit

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
    priors <- object$priors
    terms_par <- object$terms_par
    nm_distn <- nm_distn(object)
    i_prior <- make_i_prior(priors)
    consts <- make_consts(priors)
    terms_consts <- make_terms_consts(priors)
    hyper <- make_hyper(priors)
    terms_hyper <- make_terms_hyper(priors)
    par <- make_par(priors = priors,
                    terms_par = terms_par)
    data <- list(nm_distn = nm_distn,
                 outcome = object$outcome,
                 offset = object$offset,
                 terms_par = object$terms_par,
                 matrices_par = object$matrices_par,
                 i_prior = i_prior,
                 terms_hyper = terms_hyper,
                 consts = consts,
                 terms_consts = terms_consts)
    parameters <- list(par = par,
                       hyper = hyper)
    map <- make_map(priors = priors,
                    terms_par = terms_par)
    f <- TMB::MakeADFun(data = data,
                        parameters = parameters,
                        map = map,
                        DLL = "bage",
                        random = "par",
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


## HAS_TESTS
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
get_fun_inv_transform.bage_mod_norm <- function(mod)
    function(x) x



## 'model_descr' -----------------------------------------------------------------

model_descr <- function(mod) {
    UseMethod("model_descr")
}

#' @export
model_descr.bage_mod_pois <- function(mod) "Poisson"

#' @export
model_descr.bage_mod_binom <- function(mod) "binomial"

#' @export
model_descr.bage_mod_norm <- function(mod) "normal"


## 'nm_distn' -----------------------------------------------------------------

nm_distn <- function(mod) {
    UseMethod("nm_distn")
}

#' @export
nm_distn.bage_mod_pois <- function(mod) "pois"

#' @export
nm_distn.bage_mod_binom <- function(mod) "binom"

#' @export
nm_distn.bage_mod_norm <- function(mod) "norm"


## 'nm_distn' -----------------------------------------------------------------

nm_offset <- function(mod) {
    UseMethod("nm_offset")
}

#' @export
nm_offset.bage_mod_pois <- function(mod) "exposure"

#' @export
nm_offset.bage_mod_binom <- function(mod) "size"

#' @export
nm_offset.bage_mod_norm <- function(mod) "weights"


## 'print' --------------------------------------------------------------------

#' @export
print.bage_mod <- function(x, ...) {
    nchar_data <- 10
    ## calculations
    formula <- x$formula
    priors <- x$priors
    n_draw <- x$n_draw
    data <- x$data
    vname_offset <- x$vname_offset
    is_fitted <- !is.null(x$est)
    str_title <- sprintf("-- %s %s model --",
                         if (is_fitted) "Fitted" else "Unfitted",
                         model_descr(x))
    nms_priors <- names(priors)
    nchar_response <- nchar(as.character(formula[[2L]]))
    nchar_max <- max(nchar(nms_priors), nchar_response)
    padding_formula <- paste(rep(" ", nchar_max - nchar_response),
                             collapse = "")
    nms_priors <- sprintf("% *s", nchar_max, nms_priors)
    calls_priors <- vapply(priors, str_call_prior, "")
    str_priors <- paste(nms_priors, calls_priors, sep = " ~ ")
    str_priors <- paste(str_priors, collapse = "\n")
    has_offset <- !is.null(vname_offset)
    if (has_offset) {
        nm_offset <- nm_offset(x)
        nm_offset <- sprintf("% *s", nchar_data, nm_offset)
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
                nchar_data,
                "data",
                paste(names(data), collapse = ", ")))
    cat("\n")
    cat(sprintf("% *s: %d",
                nchar_data,
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

#' Components from a fitted model
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
    terms_est <- x$est$par
    terms_par <- x$terms_par
    priors <- x$priors
    term <- names(priors)
    spec <- vapply(priors, str_call_prior, "")
    n <- as.integer(table(terms_par))
    ans <- tibble::tibble(term, spec, n)
    is_fitted <- !is.null(terms_est)
    if (is_fitted) {
        terms_est <- split(terms_est, terms_par)
        ans$sd <- vapply(terms_est, stats::sd, 0)
    }
    ans
}

