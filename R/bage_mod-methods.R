
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
    is_fitted <- is_fitted(x)
    ans <- x$data
    ## make 'observed'
    observed <- make_observed(x)
    ans$.observed <- observed
    ## if model not fitted, stop here
    if (!is_fitted)
        return(ans)
    ## make transformation from scale/ordering of par
    ## to scale/ordering of outcome
    inv_transform <- get_fun_inv_transform(x)
    align_to_data <- get_fun_align_to_data(x)
    transform <- function(x)
        align_to_data(inv_transform(x))
    ## extract quantities needed in calculations
    has_season <- has_season(x)
    has_disp <- has_disp(x)
    components <- components(x)
    linpred_par <- make_linpred_par(mod = x,
                                    components = components)
    if (has_season) {
        linpred_season <- make_linpred_season(mod = x,
                                              components = components)
        linpred <- linpred_par + linpred_season
        expected <- transform(linpred)
        seasadj <- transform(linpred_par)
    }
    else
        expected <- transform(linpred_par)
    if (has_disp) {
        is_disp <- components$component == "disp"
        disp <- components$.fitted[is_disp]
        fitted <- make_fitted_disp(x = x,
                                   expected = expected,
                                   disp = disp)
    }
    ## Deal with four combinations of dispersion
    ## and seasonal effect. Note that when no
    ## dispersion, fitted and expected are identical.
    if (!has_disp && !has_season) {
        ans$.fitted <- expected
    }
    else if (has_disp && !has_season) {
        ans$.fitted <- fitted
        ans$.expected <- expected
    }
    else if (!has_disp && has_season) {
        ans$.fitted <- expected
        ans$.seasadj <- seasadj
    }
    else { ## has_disp && has_season
        ans$.fitted <- fitted
        ans$.expected <- expected
        ans$.seasadj <- seasadj
    }
    ans
}

## HAS_TESTS
#' @export
augment.bage_mod_norm <- function(x, ...) {
    is_fitted <- is_fitted(x)
    ans <- x$data
    ## if model not fitted, stop here
    if (!is_fitted)
        return(ans)
    ## make transformation from scale/ordering of par
    ## to scale/ordering of outcome
    align_to_data <- get_fun_align_to_data(x)
    scale_outcome <- get_fun_scale_outcome(x)
    transform <- function(x)
        scale_outcome(align_to_data(x))
    ## extract quantities needed in calculations
    components <- components(x)
    linpred_par <- make_linpred_par(mod = x,
                                    components = components)
    has_season <- has_season(x)
    if (has_season) {
        linpred_season <- make_linpred_season(mod = x,
                                              components = components)
        linpred <- linpred_par + linpred_season
        fitted <- transform(linpred)
        seasadj <- transform(linpred_par)
    }
    else
        fitted <- transform(linpred_par)
    ## deal with case with and without seasonal effect
    if (has_season) {
        ans$.fitted <- fitted
        ans$.seasadj <- seasadj
    }
    else { 
        ans$.fitted <- fitted
    }
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
#' There are three types of component:
#' - `"par"` Intercept, main effects, and interactions.
#' - `"hyper"` Hyper-parameters from priors for intercept,
#' main effects, and interactions.
#' - `"season"` Parameters and hyper-parameters for
#' seasonal effect, if present.
#'
#' For each component, `components()` returns three things:
#' - `term` Name of the effect or interaction
#' - `level` Element of term
#' - `.fitted` An [rvec][rvec::rvec()] containing
#' draws from the posterior distribution.
#'
#' @param object An fitted model.
#' @param ... Not currently used.
#'
#' @returns A [tibble][tibble::tibble-package] with
#' variables `component`, `term`, `level`, and `.fitted`.
#'
#' @seealso [augment()], [tidy()]
#'
#' @examples
#' library(dplyr)
#' mod <- mod_pois(injuries ~ age + sex + year,
#'                 data = injuries,
#'                 exposure = popn) %>%
#'   fit()
#' mod
#' mod %>%
#'   components() %>%
#'   filter(component == "par",
#'          term == "age")
#' @export
components.bage_mod <- function(object, ...) {
    if (is_fitted(object)) {
        comp <- make_comp_component(object)
        term <- make_term_components(object)
        level <- make_level_components(object)
        draws <- make_draws_components(object)
        draws <- as.matrix(draws)
        .fitted <- rvec::rvec_dbl(draws)
        tibble::tibble(component = comp,
                       term = term,
                       level = level,
                       .fitted = .fitted)
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
    ## data
    nm_distn <- nm_distn(object)
    outcome <- object$outcome
    offset <- object$offset
    is_in_lik <- make_is_in_lik(object)
    terms_par <- make_terms_par(object)
    terms_parfree <- make_terms_parfree(object)
    uses_matrix_parfree_par <- make_uses_matrix_parfree_par(object)
    matrices_parfree_par <- make_matrices_parfree_par(object)
    uses_offset_parfree_par <- make_uses_offset_parfree_par(object)
    offsets_parfree_par <- make_offsets_parfree_par(object)
    matrices_par_outcome <- object$matrices_par_outcome
    i_prior <- make_i_prior(object)
    uses_hyper <- make_uses_hyper(object)
    terms_hyper <- make_terms_hyper(object)
    const <- make_const(object)
    terms_const <- make_terms_const(object)
    scale_disp <- object$scale_disp
    n_time <- n_time(object)
    n_season <- object$n_season
    has_disp <- scale_disp > 0
    const_season <- make_const_season(object)
    matrix_season_outcome <- object$matrix_season_outcome
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
                 uses_hyper = uses_hyper,
                 terms_hyper = terms_hyper,
                 consts = const, ## 'const' is reserved word in C
                 terms_consts = terms_const,
                 scale_disp = scale_disp,
                 n_time = n_time,
                 n_season = n_season,
                 consts_season = const_season,
                 matrix_season_outcome = matrix_season_outcome)
    ## parameters
    parfree <- make_parfree(object)
    hyper <- make_hyper(object)
    log_disp <- 0
    par_season <- make_par_season(object)
    hyper_season <- make_hyper_season(object)
    parameters <- list(parfree = parfree,   
                       hyper = hyper,
                       log_disp = log_disp,
                       par_season = par_season,
                       hyper_season = hyper_season)
    ## MakeADFun
    map <- make_map(object)
    random <- make_random(object)
    f <- TMB::MakeADFun(data = data,
                        parameters = parameters,
                        map = map,
                        DLL = "bage",
                        random = random,
                        silent = TRUE)
    ## optimise
    stats::nlminb(start = f$par,
                  objective = f$fn,
                  gradient = f$gr,
                  silent = TRUE)
    ## extract results
    sdreport <- TMB::sdreport(f,
                              bias.correct = TRUE,
                              getJointPrecision = TRUE)
    est <- as.list(sdreport, what = "Est")
    attr(est, "what") <- NULL
    is_fixed <- make_is_fixed(est = est, map = map)
    prec <- sdreport$jointPrecision
    R_prec <- chol(prec)
    object$est <- est
    object$is_fixed <- is_fixed
    object$R_prec <- R_prec
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
get_fun_inv_transform.bage_mod_pois <- function(mod) exp

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


## 'has_disp' ----------------------------------------------------------------

#' Test whether a model includes a dispersion parameter
#'
#' @param x A model object.
#'
#' @returns `TRUE` or `FALSE`
#'
#' @noRd
has_disp <- function(mod) {
    UseMethod("has_disp")
}

## HAS_TESTS
#' @export
has_disp.bage_mod <- function(mod) {
    scale_disp <- mod$scale_disp
    scale_disp > 0L
}


## 'has_season' ----------------------------------------------------------------

#' Test whether a model includes a seasonal effect
#'
#' Test whether a seasonal effect has been added
#' to a model (via [set_season()]).
#'
#' @param x A model object.
#'
#' @returns `TRUE` or `FALSE`
#'
#' @noRd
has_season <- function(mod) {
    UseMethod("has_season")
}

## HAS_TESTS
#' @export
has_season.bage_mod <- function(mod) {
    n_season <- mod$n_season
    n_season > 0L
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


## 'make_fitted_disp' ---------------------------------------------------------

#' Make modelled estimates for models
#' with dispersion term
#'
#' @param x A fitted 'bage_mod' object.
#' @param expected An rvec with posterior
#' distribution of expected values,
#' based on (transformed) linear predictor.
#' Aligned to data (not outcome.)
#' @param disp An rvec of length 1 with
#' posterior distribution for
#' dispersion term.
#'
#' @returns A vector of doubles.
#'
#' @noRd
make_fitted_disp <- function(x, expected, disp) {
    UseMethod("make_fitted_disp")
}

## HAS_TESTS
#' @export
make_fitted_disp.bage_mod_pois <- function(x, expected, disp) {
    outcome <- x$outcome
    offset <- x$offset
    outcome <- as.double(outcome) ## so 'align_to_data' works correctly
    offset <- as.double(offset)   ## so 'align_to_data' works correctly
    align_to_data <- get_fun_align_to_data(x)
    outcome <- align_to_data(outcome)
    offset <- align_to_data(offset)
    n_val <- length(outcome)
    n_draw <- rvec::n_draw(expected)
    ans <- rvec::rvec_dbl(matrix(NA, nrow = n_val, ncol = n_draw))
    is_na <- is.na(outcome) | is.na(offset)
    outcome <- outcome[!is_na]
    offset <- offset[!is_na]
    expected <- expected[!is_na]
    ans[!is_na] <- rvec::rgamma_rvec(n = length(outcome),
                                     shape = outcome + 1 / disp,
                                     rate = offset + 1 / (disp * expected))
    ans
}

## HAS_TESTS
#' @export
make_fitted_disp.bage_mod_binom <- function(x, expected, disp) {
    outcome <- x$outcome
    offset <- x$offset
    outcome <- as.double(outcome) ## so 'align_to_data' works correctly
    offset <- as.double(offset)   ## so 'align_to_data' works correctly
    align_to_data <- get_fun_align_to_data(x)
    outcome <- align_to_data(outcome)
    offset <- align_to_data(offset)
    n_val <- length(outcome)
    n_draw <- rvec::n_draw(expected)
    ans <- rvec::rvec_dbl(matrix(NA, nrow = n_val, ncol = n_draw))
    is_na <- is.na(outcome) | is.na(offset)
    outcome <- outcome[!is_na]
    offset <- offset[!is_na]
    expected <- expected[!is_na]
    ans[!is_na] <- rvec::rbeta_rvec(n = length(outcome),
                                    shape1 = outcome + expected / disp,
                                    shape2 = offset - outcome + (1 - expected) / disp)
}


## 'make_matrix_season_outcome' -----------------------------------------------

#' Make matrix mapping season effect to outcome
#'
#' @param x Object of class 'bage_mod'
#' @param by Variable names (tidyselect style) or NULL.
#'
#' @returns Sparse matrix
#'
#' @noRd
make_matrix_season_outcome <- function(x, by) {
    UseMethod("make_matrix_season_outcome")
}

## HAS_TESTS
#' @export
make_matrix_season_outcome.bage_mod <- function(x, by) {
    outcome <- x$outcome
    var_time <- x$var_time
    dim <- dim(outcome)
    dimnames <- dimnames(outcome)
    nms_outcome <- names(dimnames(outcome))
    nms_season <- c(by, var_time)
    is_in_season <- nms_outcome %in% nms_season
    make_matrix_par_outcome_array(dim = dim,
                                  dimnames = dimnames(outcome),
                                  is_in_term = is_in_season)
}

## HAS_TESTS
#' @export
make_matrix_season_outcome.bage_mod_norm <- function(x, by) {
    data <- x$data
    var_time <- x$var_time
    nms_season <- c(by, var_time)
    data_season <- data[nms_season]
    data_season[] <- lapply(data_season, factor)
    contrasts_season <- lapply(data_season, stats::contrasts, contrast = FALSE)
    formula_season <- paste(nms_season, collapse = ":")
    formula_season <- paste0("~", formula_season, "-1")
    formula_season <- stats::as.formula(formula_season)
    ans <- Matrix::sparse.model.matrix(formula_season,
                                       data = data_season,
                                       contrasts.arg = contrasts_season,
                                       row.names = FALSE)
    ans
}


## 'make_observed' ------------------------------------------------------------

#' Make direct estimates
#'
#' @param x A fitted 'bage_mod' object.
#'
#' @returns A vector of doubles.
#'
#' @noRd
make_observed <- function(x) {
    UseMethod("make_observed")
}
              
## HAS_TESTS
#' @export
make_observed.bage_mod <- function(x) {
    outcome <- x$outcome
    offset <- x$offset
    outcome <- as.double(outcome) ## so 'align_to_data' works correctly
    offset <- as.double(offset)   ## so 'align_to_data' works correctly
    align_to_data <- get_fun_align_to_data(x)
    ans <- as.double(outcome / offset)
    ans <- align_to_data(ans)
    ans
}

## HAS_TESTS
#' @export
make_observed.bage_mod_norm <- function(x) {
    cli::cli_abort(paste("Internal error: {.fun make_observed} called on object",
                         "of class {.cls {class(x)}}."))
}


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


## 'n_time' -------------------------------------------------------------------

#' Number of time points in outcome data
#'
#' @param mod Object of class "bage_mod"
#'
#' @returns An integer
#'
#' @noRd
n_time <- function(mod) {
    UseMethod("n_time")
}

## HAS_TESTS
#' @export
n_time.bage_mod <- function(mod) {
    var_time <- mod$var_time
    has_time <- !is.null(var_time)
    if (has_time) {
        matrices_par_outcome <- mod$matrices_par_outcome
        ncol(matrices_par_outcome[[var_time]])
    }
    else
        0L
}


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
    nchar_offset <- 15
    ## calculations
    formula <- x$formula
    priors <- x$priors
    n_draw <- x$n_draw
    data <- x$data
    vname_offset <- x$vname_offset
    var_age <- x$var_age
    var_sexgender <- x$var_sexgender
    var_time <- x$var_time
    scale_disp <- x$scale_disp
    n_season <- x$n_season
    scale_season <- x$scale_season
    is_fitted <- is_fitted(x)
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
    str_disp <- sprintf("% *s: s=%s", nchar_offset, "dispersion", scale_disp)
    has_season <- n_season > 0L
    if (has_season) {
        nm_season <- sprintf("% *s", nchar_offset, "seasonal effect")
        str_season <- sprintf("%s: n=%d", nm_season, n_season)
        if (scale_season != 1)
            str_season <- sprintf("%s, s=%s", str_season, scale_season)
    }
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
    cat("\n\n")
    cat(str_priors)
    cat("\n\n")
    cat(str_disp)
    cat("\n")
    if (has_season) {
        cat(str_season)
        cat("\n")
    }
    if (has_offset) {
        cat(str_offset)
        cat("\n")
    }
    if (!is.null(var_age)) {
        cat(sprintf("% *s: %s",
                    nchar_offset,
                    "var_age",
                    var_age))
        cat("\n")
    }
    if (!is.null(var_sexgender)) {
        cat(sprintf("% *s: %s",
                    nchar_offset,
                    "var_sexgender",
                    var_sexgender))
        cat("\n")
    }
    if (!is.null(var_time)) {
        cat(sprintf("% *s: %s",
                    nchar_offset,
                    "var_time",
                    var_time))
        cat("\n")
    }
    if (has_offset) {
        cat(sprintf("% *s: %d",
                    nchar_offset,
                    "n_draw",
                    n_draw))
        cat("\n")
    }
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
        parfree <- x$est$parfree
        matrix <- make_combined_matrix_parfree_par(x)
        offset <- make_offsets_parfree_par(x)
        par <- matrix %*% parfree + offset
        terms <- make_terms_par(x)
        par <- split(par, terms)
        ans[["sd"]] <- vapply(par, stats::sd, 0)
    }
    ans <- tibble::tibble(ans)
    ans
}

