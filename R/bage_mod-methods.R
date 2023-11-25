
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
    ## make transformation from scale/ordering of effect
    ## to scale/ordering of outcome
    inv_transform <- get_fun_inv_transform(x)
    align_to_data <- get_fun_align_to_data(x)
    transform <- function(x)
        align_to_data(inv_transform(x))
    ## extract quantities needed in calculations
    has_cyclical <- has_cyclical(x)
    has_season <- has_season(x)
    has_disp <- has_disp(x)
    components <- components(x)
    linpred <- make_linpred_effect(mod = x,
                                   components = components)
    if (has_cyclical) {
        trend <- transform(linpred)
        linpred_cyclical <- make_linpred_cyclical(mod = x,
                                                  components = components)
        linpred <- linpred + linpred_cyclical
    }
    if (has_season) {
        seasadj <- transform(linpred)
        linpred_season <- make_linpred_season(mod = x,
                                              components = components)
        linpred <- linpred + linpred_season
    }
    if (has_disp) {
        expected <- transform(linpred)
        is_disp <- components$component == "disp"
        disp <- components$.fitted[is_disp]
        fitted <- make_par_disp(x = x,
                                meanpar = expected,
                                disp = disp)
    }
    else
        fitted <- transform(linpred)
    ans$.fitted <- fitted
    if (has_disp)
        ans$.expected <- expected
    if (has_season)
        ans$.seasadj <- seasadj
    if (has_cyclical)
        ans$.trend <- trend
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
    ## make transformation from scale/ordering of effect
    ## to scale/ordering of outcome
    align_to_data <- get_fun_align_to_data(x)
    scale_outcome <- get_fun_scale_outcome(x)
    transform <- function(x)
        scale_outcome(align_to_data(x))
    ## extract quantities needed in calculations
    components <- components(x)
    linpred_effect <- make_linpred_effect(mod = x,
                                    components = components)
    has_season <- has_season(x)
    if (has_season) {
        linpred_season <- make_linpred_season(mod = x,
                                              components = components)
        linpred <- linpred_effect + linpred_season
        fitted <- transform(linpred)
        seasadj <- transform(linpred_effect)
    }
    else
        fitted <- transform(linpred_effect)
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
#' There are five types of component:
#' - `"effect"` Intercept, main effects, and interactions.
#' - `"hyper"` Hyper-parameters from priors for intercept,
#'   main effects, and interactions.
#' - `"disp"` Dispersion term.
#' - `"cyclical"` Parameters and hyper-parameters for
#'   cyclical effect, if included in model.
#' - `"season"` Parameters and hyper-parameters for
#'   seasonal effect, if included in model.
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
#'   filter(component == "effect",
#'          term == "age")
#' @export
components.bage_mod <- function(object, ...) {
    if (is_fitted(object)) {
        seed_components <- object$seed_components
        comp <- make_comp_components(object)
        term <- make_term_components(object)
        level <- make_level_components(object)
        seed_restore <- make_seed() ## randomly generate seed
        set.seed(seed_components)   ## set pre-determined seed
        draws <- make_draws_components(object)  ## given seed, calculations deterministic
        set.seed(seed_restore)      ## use randomly-generated seed, to restore randomness 
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


## 'draw_vals_disp' -----------------------------------------------------------

#' Draw values for 'disp' from prior
#'
#' @param mod Obejct of class 'bage_mod'
#' @param n_sim Number of draws
#'
#' @returns A named numeric vector.
#'
#' @noRd
draw_vals_disp <- function(mod, n_sim) {
    UseMethod("draw_vals_disp")
}

## HAS_TESTS
#' @export
draw_vals_disp.bage_mod <- function(mod, n_sim) {
    scale <- mod$scale_disp
    u <- stats::runif(n = n_sim)
    ans <- (log(u) / scale)^2 ## log(u) equivalent to log(1-u) when u ~ Unif(0, 1)
    names(ans) <- seq_len(n_sim)
    ans
}


## HAS_TESTS
#' @export
draw_vals_disp.bage_mod_norm <- function(mod, n_sim) {
    scale <- mod$scale_disp
    ans <- stats::rexp(n = n_sim, rate = scale)
    names(ans) <- seq_len(n_sim)
    ans
}


#' Draw values for rate or prob, given back-transformed
#' linear predictor, and possibly dispersion
#'
#' @param mod Object of class 'bage_mod'
#' @param vals_meanpar Numeric matrix, with each
#' column holding one draw
#' @param vals_disp Numeric vector
#'
#' @returns Numeric matrix
#'
#' @noRd
draw_vals_par <- function(mod, vals_meanpar, vals_disp) {
    UseMethod("draw_vals_par")
}

## HAS_TESTS
#' @export
draw_vals_par.bage_mod_pois <- function(mod, vals_meanpar, vals_disp) {
    eps <- 1e-10
    n_outcome <- nrow(vals_meanpar)
    n_sim <- ncol(vals_meanpar)
    vals_disp <- rep(vals_disp, each = n_outcome)
    shape <- 1 / vals_disp
    rate <- 1 / (vals_meanpar * vals_disp)
    ans <- vals_meanpar
    is_nonzero_disp <- vals_disp > eps
    ans[is_nonzero_disp] <- stats::rgamma(n = sum(is_nonzero_disp),
                                          shape = shape[is_nonzero_disp],
                                          rate = rate[is_nonzero_disp])
    ans <- matrix(ans,
                  nrow = n_outcome,
                  ncol = n_sim)
    ans
}

## HAS_TESTS
#' @export
draw_vals_par.bage_mod_binom <- function(mod, vals_meanpar, vals_disp) {
    eps <- 1e-10
    n_outcome <- nrow(vals_meanpar)
    n_sim <- ncol(vals_meanpar)
    vals_disp <- rep(vals_disp, each = n_outcome)
    shape1 <- vals_meanpar / vals_disp
    shape2 <- (1 - vals_meanpar) / vals_disp
    ans <- vals_meanpar
    is_nonzero_disp <- vals_disp > eps
    ans[is_nonzero_disp] <- stats::rbeta(n = sum(is_nonzero_disp),
                                         shape1 = shape1[is_nonzero_disp],
                                         shape2 = shape2[is_nonzero_disp])
    ans <- matrix(ans,
                  nrow = n_outcome,
                  ncol = n_sim)
    ans
}


## 'draw_vals_mod' ------------------------------------------------------------

#' Draw values for hyper-parameters, cell-level parameters
#' and outcome variable
#'
#' @param mod Object of class 'bage_mod'
#' @param n_sim Number of draws
#'
#' @returns A named list
#'
#' @noRd
draw_vals_mod <- function(mod, n_sim) {
    UseMethod("draw_vals_mod")
}

## HAS_TESTS
#' @export
draw_vals_mod.bage_mod_pois <- function(mod, n_sim) {
    offset <- mod$offset
    vals_hyperparam <- draw_vals_hyperparam(mod = mod,
                                            n_sim = n_sim)
    linpred <- vals_hyperparam$linpred
    disp <- vals_hyperparam$disp
    has_disp <- !is.null(disp)
    inv_transform <- get_fun_inv_transform(mod)
    align_to_data <- get_fun_align_to_data(mod)
    if (has_disp) {
        vals_meanpar <- inv_transform(linpred)
        vals_meanpar <- align_to_data(vals_meanpar)
        vals_par <- draw_vals_par(mod = mod,
                                        vals_meanpar = vals_meanpar,
                                        vals_disp = disp)
    }
    else {
        vals_par <- inv_transform(linpred)
        vals_par <- align_to_data(vals_par)
    }
    n_outcome <- nrow(vals_par)
    exposure <- align_to_data(offset)
    exposure <- rep(exposure, times = n_sim)
    lambda <- vals_par * exposure
    vals_outcome <- stats::rpois(n = n_outcome * n_sim,
                                 lambda = lambda)
    vals_outcome <- matrix(vals_outcome,
                           nrow = n_outcome,
                           ncol = n_sim)
    c(vals_hyperparam["effect"],
      vals_hyperparam["hyper"],
      vals_hyperparam["disp"],
      vals_hyperparam["cyclical"],
      vals_hyperparam["season"],
      list(par = vals_par,
           outcome = vals_outcome))
}

## HAS_TESTS
#' @export
draw_vals_mod.bage_mod_binom <- function(mod, n_sim) {
    offset <- mod$offset
    vals_hyperparam <- draw_vals_hyperparam(mod = mod,
                                            n_sim = n_sim)
    linpred <- vals_hyperparam$linpred
    disp <- vals_hyperparam$disp
    has_disp <- !is.null(disp)
    inv_transform <- get_fun_inv_transform(mod)
    align_to_data <- get_fun_align_to_data(mod)
    if (has_disp) {
        vals_meanpar <- inv_transform(linpred)
        vals_meanpar <- align_to_data(vals_meanpar)
        vals_par <- draw_vals_par(mod = mod,
                                        vals_meanpar = vals_meanpar,
                                        vals_disp = disp)
    }
    else {
        vals_par <- inv_transform(linpred)
        vals_par <- align_to_data(vals_par)
    }
    n_outcome <- nrow(vals_par)
    size <- align_to_data(offset)
    size <- rep(size, times = n_sim)
    vals_outcome <- stats::rbinom(n = n_outcome * n_sim,
                                  size = size,
                                  prob = vals_par)
    vals_outcome <- matrix(vals_outcome,
                           nrow = n_outcome,
                           ncol = n_sim)
    c(vals_hyperparam["effect"],
      vals_hyperparam["hyper"],
      vals_hyperparam["disp"],
      vals_hyperparam["cyclical"],
      vals_hyperparam["season"],
      list(par = vals_par,
           outcome = vals_outcome))
}

## HAS_TESTS
#' @export
draw_vals_mod.bage_mod_norm <- function(mod, n_sim) {
    wt <- mod$offset
    vals_hyperparam <- draw_vals_hyperparam(mod = mod,
                                            n_sim = n_sim)
    linpred <- vals_hyperparam$linpred
    disp <- vals_hyperparam$disp
    align_to_data <- get_fun_align_to_data(mod)
    scale_outcome <- get_fun_scale_outcome(mod)
    par <- scale_outcome(align_to_data(linpred))
    n_outcome <- nrow(par)
    sd <- rep(disp, each = n_outcome) / rep(wt, times = n_sim)
    vals_outcome <- stats::rnorm(n = length(par),
                                 mean = par,
                                 sd = sd)
    vals_outcome <- matrix(vals_outcome,
                           nrow = n_outcome,
                           ncol = n_sim)
    c(vals_hyperparam["effect"],
      vals_hyperparam["hyper"],
      vals_hyperparam["disp"],
      vals_hyperparam["cyclical"],
      vals_hyperparam["season"],
      list(par = par,
           outcome = vals_outcome))
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
    terms_effect <- object$terms_effect
    is_in_lik <- make_is_in_lik(object)
    terms_effectfree <- make_terms_effectfree(object)
    uses_matrix_effectfree_effect <- make_uses_matrix_effectfree_effect(object)
    matrices_effectfree_effect <- make_matrices_effectfree_effect(object)
    uses_offset_effectfree_effect <- make_uses_offset_effectfree_effect(object)
    offsets_effectfree_effect <- make_offsets_effectfree_effect(object)
    matrices_effect_outcome <- object$matrices_effect_outcome
    i_prior <- make_i_prior(object)
    uses_hyper <- make_uses_hyper(object)
    terms_hyper <- make_terms_hyper(object)
    const <- make_const(object)
    terms_const <- make_terms_const(object)
    scale_disp <- object$scale_disp
    has_disp <- scale_disp > 0
    n_cyclical <- object$n_cyclical
    const_cyclical <- make_const_cyclical(object)
    matrix_cyclical_outcome <- object$matrix_cyclical_outcome
    n_time <- n_time(object)
    n_season <- object$n_season
    const_season <- make_const_season(object)
    matrix_season_outcome <- object$matrix_season_outcome
    data <- list(nm_distn = nm_distn,
                 outcome = outcome,
                 offset = offset,
                 is_in_lik = is_in_lik,
                 terms_effect = terms_effect,
                 terms_effectfree = terms_effectfree,
                 uses_matrix_effectfree_effect = uses_matrix_effectfree_effect,
                 matrices_effectfree_effect = matrices_effectfree_effect,
                 uses_offset_effectfree_effect = uses_offset_effectfree_effect,
                 offsets_effectfree_effect = offsets_effectfree_effect,
                 matrices_effect_outcome = matrices_effect_outcome,
                 i_prior = i_prior,
                 uses_hyper = uses_hyper,
                 terms_hyper = terms_hyper,
                 consts = const, ## 'const' is reserved word in C
                 terms_consts = terms_const,
                 scale_disp = scale_disp,
                 n_cyclical = n_cyclical,
                 consts_cyclical = const_cyclical,
                 matrix_cyclical_outcome = matrix_cyclical_outcome,
                 n_time = n_time,
                 n_season = n_season,
                 consts_season = const_season,
                 matrix_season_outcome = matrix_season_outcome)
    ## parameters
    effectfree <- make_effectfree(object)
    hyper <- make_hyper(object)
    log_disp <- 0
    effect_cyclical <- make_effect_cyclical(object)
    hyper_cyclical <- make_hyper_cyclical(object)
    effect_season <- make_effect_season(object)
    hyper_season <- make_hyper_season(object)
    parameters <- list(effectfree = effectfree,   
                       hyper = hyper,
                       log_disp = log_disp,
                       effect_cyclical = effect_cyclical,
                       hyper_cyclical = hyper_cyclical,
                       effect_season = effect_season,
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
    R_prec <- tryCatch(chol(prec),
                       error = function(e) e)
    if (is.matrix(R_prec))
        object$R_prec <- R_prec
    else
        object$scaled_eigen <- make_scaled_eigen(prec)
    object$est <- est
    object$is_fixed <- is_fixed
    object
}


## 'forecast' -----------------------------------------------------------------

#' @importFrom generics forecast
#' @export
generics::forecast

## NO_TESTS
#' Forecast a model
#'
#' @param object A `bage_mod` object,
#' typically created with [mod_pois()],
#' [mod_binom()], or [mod_norm()].
#' @param ... Not currently used.
#'
#' @returns A `bage_mod` object
#'
#' @export    
forecast.bage_mod <- function(object, n, ...) {
    var_time <- object$var_time
    if (is.null(var_time))
        cli::cli_abort(c("Can't forecast when time variable not identified.",
                         i = "Please use {.fun set_var_time} to identify time variable."))
    check_n(n, n_arg = "n", min = NULL, max = NULL, null_ok = FALSE)
    levels_time <- 
    time_extra <- 
    n <- as.integer(n)
    
    
    has_cyclical <- has_cyclical(object)
    has_season <- has_season(object)
    



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
    function(x) 1 / (1 + exp(-x))

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


## 'get_vals_est' -------------------------------------------------------------

#' Extract estimated values from a fitted model
#'
#' @param mod A fitted object of class 'bage_mod'
#'
#' @returns A named list of rvecs
#'
#' @noRd
get_vals_est <- function(mod) {
    UseMethod("get_vals_est")
}

## HAS_TESTS
#' @export
get_vals_est.bage_mod <- function(mod) {
    has_disp <- has_disp(mod)
    vals_hyperparam <- get_vals_hyperparam_est(mod)
    inv_transform <- get_fun_inv_transform(mod)
    align_to_data <- get_fun_align_to_data(mod)
    transform <- function(x)
        align_to_data(inv_transform(x))
    linpred <- vals_hyperparam[["linpred"]]
    if (has_disp) {
        disp <- vals_hyperparam[["disp"]]
        meanpar <- transform(linpred)
        par <- make_par_disp(x = mod,
                                   meanpar = meanpar,
                                   disp = disp)
    }
    else
        par <- transform(linpred)
    list(effect = vals_hyperparam[["effect"]],
         hyper = vals_hyperparam[["hyper"]],
         disp = vals_hyperparam[["disp"]],
         cyclical = vals_hyperparam[["cyclical"]],
         season = vals_hyperparam[["season"]],
         par = par)
}

## HAS_TESTS
#' @export
get_vals_est.bage_mod_norm <- function(mod) {
    vals_hyperparam <- get_vals_hyperparam_est(mod)
    linpred <- vals_hyperparam[["linpred"]]
    align_to_data <- get_fun_align_to_data(mod)
    scale_outcome <- get_fun_scale_outcome(mod)
    par <- scale_outcome(align_to_data(linpred))
    list(effect = vals_hyperparam[["effect"]],
         hyper = vals_hyperparam[["hyper"]],
         disp = vals_hyperparam[["disp"]],
         cyclical = vals_hyperparam[["cyclical"]],
         season = vals_hyperparam[["season"]],
         par = par)
}


## 'has_cyclical' ----------------------------------------------------------------

## HAS_TESTS
#' Test whether a model includes a cyclical effect
#'
#' Test whether a cyclicalal effect has been added
#' to a model (via [set_cyclical()]).
#'
#' @param x A model object.
#'
#' @returns `TRUE` or `FALSE`
#'
#' @noRd
has_cyclical <- function(mod) {
    UseMethod("has_cyclical")
}

## HAS_TESTS
#' @export
has_cyclical.bage_mod <- function(mod) {
    n_cyclical <- mod$n_cyclical
    n_cyclical > 0L
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


## 'make_par_disp_inner' ---------------------------------------------------

#' Make random draws for 'make_par_disp'
#' with dispersion term
#'
#' @param x Fitted object of class 'bage_mod'.
#' @param outcome Values for outcome variable
#' (where neither outcome or offset is NA).
#' Aligned to data.
#' @param offset Values for offset variable
#' (where neither outcome or offset is NA).
#' Aligned to data.
#' @param meanpar An rvec with posterior
#' distribution of expected values,
#' based on (transformed) linear predictor.
#' Aligned to data.
#' @param disp An rvec of length 1 with
#' posterior distribution for
#' dispersion term.
#'
#' @returns An rvec
#'
#' @noRd
make_par_disp_inner <- function(x,
                                outcome,
                                offset,
                                meanpar,
                                disp) {
    UseMethod("make_par_disp_inner")
}

## HAS_TESTS
#' @export
make_par_disp_inner.bage_mod_pois <- function(x,
                                              outcome,
                                              offset,
                                              meanpar,
                                              disp) {
    rvec::rgamma_rvec(n = length(outcome),
                      shape = outcome + 1 / disp,
                      rate = offset + 1 / (disp * meanpar))
}

## HAS_TESTS
#' @export
make_par_disp_inner.bage_mod_binom <- function(x,
                                               outcome,
                                               offset,
                                               meanpar,
                                               disp) {
    rvec::rbeta_rvec(n = length(outcome),
                     shape1 = outcome + meanpar / disp,
                     shape2 = offset - outcome + (1 - meanpar) / disp)
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


## 'make_term_par' ---------------------------------------------------------

#' Name to use for par parameter
#'
#' @param mod Object of class 'bage_mod'
#'
#' @returns A string
#'
#' @noRd
make_term_par <- function(mod) {
    UseMethod("make_term_par")
}

## HAS_TESTS
#' @export
make_term_par.bage_mod_pois <- function(mod) "rate"

## HAS_TESTS
#' @export
make_term_par.bage_mod_binom <- function(mod) "prob"

## HAS_TESTS
#' @export
make_term_par.bage_mod_norm <- function(mod) "mean"


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
        matrices_effect_outcome <- mod$matrices_effect_outcome
        ncol(matrices_effect_outcome[[var_time]])
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


## 'replicate_data' -----------------------------------------------------------

#' Create Replicate Data
#'
#' Use a fitted model to create replicate datasets,
#' typically as a way of checking a model.
#'
#' Use `n` draws from the posterior distribution
#' for model parameters to generate `n` simulated datasets.
#' If the model is working well, these simulated
#' datasets should look similar to the actual dataset.
#'
#' @section: The `condition_on` argument
#'
#' With Poisson and binomial models that include
#' dispersion terms (which is the default), there are
#' two options for constructing replicate data.
#'
#' - When `condition_on` is `"par"`,
#' the replicate data is created by (i) drawing values
#' from the posterior distribution for rates or probabilities
#' (the \eqn{\gamma_i} defined in [mod_pois()]
#' and [mod_binom()]), and (ii)  conditional on these
#' rates or probabilities, drawing values for the 
#' outcome variable.
#' - When `condition_on` is `"meanpar"`,
#' the replicate data is created by (i) drawing
#' values from hyper-parameters governing
#' the rates or probabilities 
#' (the \eqn{\mu_i} and \eqn{\xi} defined
#' in [mod_pois()] and [mod_binom()]),
#' then (ii) conditional on these hyper-parameters,
#' drawing values for the rates or probabilties,
#' and finally (iii) conditional on these
#' rates or probabilities, drawing values for the 
#' outcome variable.
#'
#' The default for `condition_on` is `"meanpar"`.
#' The `"meanpar"` option
#' provides a more severe test for
#' a model than the `"par"` option,
#' since "par" values are weighted averages
#' of the "meanpar" values and the original
#' data.
#'
#' As described in [mod_norm()], normal models
#' have a slightly different structure from Poisson
#' and binomial models, and the distinction between
#' `"par"` and `"meanpar"` does not apply.
#'
#' @param x A fitted model, typically created by
#' calling [mod_pois()], [mod_binom()], or [mod_norm()],
#' and then [fit()].
#' @param condition_on Parameters to condition
#' on. Either `"meanpar"` or `"par"`. See
#' details.
#' @param n Number of replicate datasets to create.
#' Default is 19.
#'
#' @returns A tibble with the following structure:
#'
#' |`.replicate`     | data                           |
#' |-----------------|--------------------------------|
#' |`"Original"      | Original data supplied to [mod_pois()], [mod_binom()], [mod_norm()] |
#' |`"Replicate 1"`  | Original data, except that actual outcome replaced by simulated values. |
#' |`"Replicate 2"`  | Original data, except that actual outcome replaced by simulated values. |
#' |\dots            | \dots                          |
#' |`"Replicate <n>"`| Original data, except that actual outcome replaced by simulated values. |
#' 
#' 
#' @seealso
#' - [mod_pois()], [mod_binom()], [mod_norm()] to create models
#' - [fit()] to fit models
#'
#' @examples
#' mod <- mod_pois(injuries ~ age:sex + ethnicity + year,
#'                 data = injuries,
#'                 exposure = 1) |>
#'   fit()
#'
#' rep_data <- mod |>
#'   replicate_data()
#'
#' library(dplyr)
#' rep_data |>
#'   group_by(.replicate) |>
#'   count(wt = injuries)
#' @export
replicate_data <- function(x, condition_on = NULL, n = 19) {
    UseMethod("replicate_data")
}

## HAS_TESTS
#' @export
replicate_data.bage_mod_pois <- function(x, condition_on = NULL, n = 19) {
    if (is.null(condition_on))
        condition_on <- "meanpar"
    else
        condition_on <- match.arg(condition_on, choices = c("meanpar", "par"))
    check_n(n = n,
            n_arg = "n",
            min = 1L,
            max = NULL,
            null_ok = FALSE)
    check_is_fitted(x = x, x_arg = "x")
    data <- x$data
    formula <- x$formula
    outcome <- x$outcome
    offset <- x$offset
    x <- set_n_draw(x, n_draw = n)
    aug <- augment(x)
    n_obs <- nrow(data)
    if (condition_on == "par") {
        par <- aug$.fitted
        lambda <- offset * par
        y_rep <- rvec::rpois_rvec(n = n_obs,
                                  lambda = lambda)
    }
    else if (condition_on == "meanpar") {
        check_has_disp_if_condition_on_meanpar(x)
        meanpar <- aug$.expected
        comp <- components(x)
        disp <- comp[[".fitted"]][comp$component == "disp"]
        size <- 1 / disp
        mu <- offset * meanpar
        y_rep <- rvec::rnbinom_rvec(n = n_obs,
                                    size = size,
                                    mu = mu)
    }
    else
        cli::cli_abort("Internal error: Invalid value for 'condition_on'.")
    outcome_rep <- c(outcome, as.numeric(y_rep))
    nm_outcome <- deparse1(formula[[2L]])
    ans <- make_copies_repdata(data = data, n = n)
    ans[[nm_outcome]] <- outcome_rep
    ans
}

## HAS_TESTS
#' @export
replicate_data.bage_mod_binom <- function(x, condition_on = NULL, n = 19) {
    if (is.null(condition_on))
        condition_on <- "meanpar"
    else
        condition_on <- match.arg(condition_on, choices = c("meanpar", "par"))
    check_n(n = n,
            n_arg = "n",
            min = 1L,
            max = NULL,
            null_ok = FALSE)
    check_is_fitted(x = x, x_arg = "x")
    data <- x$data
    formula <- x$formula
    outcome <- x$outcome
    offset <- x$offset
    x <- set_n_draw(x, n_draw = n)
    aug <- augment(x)
    n_obs <- nrow(data)
    if (condition_on == "par") {
        par <- aug$.fitted
        y_rep <- rvec::rbinom_rvec(n = n_obs,
                                   size = offset,
                                   prob = par)
    }
    else if (condition_on == "meanpar") {
        check_has_disp_if_condition_on_meanpar(x)
        meanpar <- aug$.expected
        comp <- components(x)
        disp <- comp[[".fitted"]][comp$component == "disp"]
        shape1 <- meanpar / disp
        shape2 <- (1 - meanpar) / disp
        prob <- rvec::rbeta_rvec(n = n_obs,
                                 shape1 = shape1,
                                 shape2 = shape2)
        y_rep <- rvec::rbinom_rvec(n = n_obs,
                                   size = offset,
                                   prob = prob)
    }
    else
        cli::cli_abort("Internal error: Invalid value for 'condition_on'.")
    outcome_rep <- c(outcome, as.numeric(y_rep))
    nm_outcome <- deparse1(formula[[2L]])
    ans <- make_copies_repdata(data = data, n = n)
    ans[[nm_outcome]] <- outcome_rep
    ans
}

## HAS_TESTS
#' @export
replicate_data.bage_mod_norm <- function(x, condition_on = NULL, n = 19) {
    if (!is.null(condition_on))
        cli::cli_warn(c("Ignoring value for {.arg condition_on}.",
                        i = paste("{.fun replicate_data} ignores argument {.arg condition_on}",
                                  "when model {.arg x} has a normal likelihood.")))
    check_n(n = n,
            n_arg = "n",
            min = 1L,
            max = NULL,
            null_ok = FALSE)
    check_is_fitted(x = x, x_arg = "x")
    data <- x$data
    formula <- x$formula
    outcome <- x$outcome
    offset <- x$offset
    x <- set_n_draw(x, n_draw = n)
    aug <- augment(x)
    comp <- components(x)
    disp <- comp[[".fitted"]][comp$component == "disp"]
    n_obs <- nrow(data)
    par <- aug$.fitted
    y_rep <- rvec::rnorm_rvec(n = n_obs,
                              mean = par,
                              sd = disp / sqrt(offset))
    outcome_rep <- c(outcome, as.numeric(y_rep))
    nm_outcome <- deparse1(formula[[2L]])
    ans <- make_copies_repdata(data = data, n = n)
    ans[[nm_outcome]] <- outcome_rep
    ans
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
    n <- x$lengths_effect
    terms <- x$terms_effect
    term <- names(priors)
    spec <- vapply(priors, str_call_prior, "")
    ans <- tibble::tibble(term, spec, n)
    is_fitted <- is_fitted(x)
    if (is_fitted) {
        effectfree <- x$est$effectfree
        matrix <- make_combined_matrix_effectfree_effect(x)
        offset <- make_offsets_effectfree_effect(x)
        effect <- matrix %*% effectfree + offset
        effect <- split(effect, terms)
        ans[["sd"]] <- vapply(effect, stats::sd, 0)
    }
    ans <- tibble::tibble(ans)
    ans
}

