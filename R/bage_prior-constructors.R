
## User-visible constructors --------------------------------------------------

## 'bage_prior_ar1' only ever created via 'set_prior()'

## HAS_TESTS
#' AR1 prior
#'
#' Autoregression prior.
#'
#' The model is
#' \deqn{x_0 \sim \text{N}(0, \sigma^2)}
#' \deqn{x_i = \phi x_{i-1} + \sqrt{1 - \phi^2}\epsilon_i}
#' \deqn{\epsilon \sim \text{N}(0, \sigma^2)}
#'
#' \eqn{\sigma} is drawn from a half-normal distribition
#' with scale set by the `s` argument.
#'
#' Correlation parameter \eqn{\phi} is constrained
#' to lie in the interval `(a, b)`,
#' where \eqn{a} = `min` and \eqn{b} = `max`.
#' The prior distribution is for \eqn{\phi}
#' is
#' \deqn{\phi = (b - a) \phi' - a}
#' where
#' \deqn{\phi' \sim \text{beta}(2, 2)}.
#'
#' @param min,max Minimum and maximum values
#' for autocorrelation parameter (\eqn{\phi}).
#' Default to 0.8 and 0.98.
#' @param s Scale of half-normal prior for
#' standard deviation (\eqn{\sigma}).
#' Defaults to 1.
#'
#' @returns An object of class `bage_prior_ar1`.
#'
#' @seealso [N()], [RW()], [RW2()], [Known()].
#' The values for `min` and `max` are based on the
#' defaults for function `forecast::ets()`.
#'
#' @references TMB documentation of
#' [AR1](http://kaskr.github.io/adcomp/classdensity_1_1AR1__t.html#details)
#'
#' @examples
#' AR1()
#' AR1(min = 0, max = 1, s = 2.4)
#' @export
AR1 <- function(min = 0.8, max = 0.98, s = 1) {
    check_scale(s, x_arg = "s", zero_ok = FALSE)
    scale <- as.double(s)
    checkmate::assert_number(min, lower = 0, upper = 1)
    checkmate::assert_number(max, lower = 0, upper = 1)
    if (min >= max)
        cli::cli_abort(c("{.arg min} not less than {.arg max}",
                         i = "{.arg min} is {.val {min}}.",
                         i = "{.arg max} is {.val {max}}."))
    min <- as.double(min)
    max <- as.double(max)
    new_bage_prior_ar1(min = min,
                       max = max,
                       scale = scale)
}


## 'bage_prior_known' only ever created via 'set_prior()'

## HAS_TESTS
#' Treat a model term as known
#'
#' Treat the intercept, a main effect, or an interaction
#' in an model as known.
#'
#' @param values A numeric vector
#'
#' @returns An object of class `bage_prior_known`.
#'
#' @seealso `Known` is usually called within [set_prior()].
#'
#' @examples
#' Known(-2.3)
#' Known(c(0.1, 2, -0.11))
#'
#' @export
Known <- function(values) {
    values <- checkmate::assert_numeric(values,
                                        finite = TRUE,
                                        any.missing = FALSE,
                                        min.len = 1L)
    values <- as.double(values)
    new_bage_prior_known(values = values)
}


## 'bage_prior_norm' can be created during initial call to mod_* function

## HAS_TESTS
#' Normal prior
#'
#' Prior in which units are drawn independently from a normal
#' distribution. The default prior for most terms.
#'
#' The normal distribution has mean \eqn{0} and standard
#' deviation \eqn{\sigma}.
#' 
#' \deqn{x \sim \text{N}(0, \sigma^2)}
#'
#' Standard deviation \eqn{\sigma} is drawn from a half-normal
#' distribution,
#'
#' \deqn{\sigma \sim \text{N}^+(0, \text{s}^2)}
#'
#' (A half-normal distribution has the same shape as a normal
#' distribution, but is defined only for non-negative
#' values.)
#'
#' The scale for the half-normal distribution defaults
#' to 1, but can be set to other values. Lower values
#' lead to more tightly concentrated
#' estimates for `x`, and higher values lead to less tightly
#' concentrated estimates.
#'
#' @param s A positive, finite number.
#'
#' @returns An object of class `bage_prior_norm`.
#'
#' @seealso `N()` is usually called within [set_prior()].
#' Other priors are [RW()], [RW2()].
#'
#' @examples
#' N()
#' N(s = 0.5)
#' @export
N <- function(s = 1) {
    check_scale(s, x_arg = "s", zero_ok = FALSE) 
    scale <- as.double(s)
    new_bage_prior_norm(scale = scale)
}


## 'bage_prior_normfixed' priors can be created during intial call to mod_* function

#' Normal prior with fixed standard deviation
#'
#' Normal prior where, in contrast to [N()], the
#' standard deviation is treated as fixed and known.
#'
#' The distribution has mean equal to `0` and
#' standard deviation `sd`, where `sd` is supplied
#' by the user.
#'
#' \deqn{x \sim \text{N}(0, \text{sd}^2)}
#'
#' `NFix()` is the default prior for the intercept.
#'
#' @param sd Standard deviation.
#' A positive, finite number.
#' Default is `1`.
#'
#' @returns An object of class `bage_prior_normfixed`.
#' `NFix()` is usually called within [set_prior()].
#' Other priors are [N()], [RW()], [RW2()].
#'
#' @examples
#' NFix()
#' NFix(sd = 10) ## prior used for intercept
#' @export
NFix <- function(sd = 1) {
    check_scale(sd, x_arg = "sd", zero_ok = FALSE) 
    sd <- as.double(sd)
    new_bage_prior_normfixed(sd = sd)
}

## 'bage_prior_rw' can be created during initial call to mod_* function

## HAS_TESTS
#' Random walk prior
#'
#' Prior in which units follow a (first order)
#' random waok. Increments between neighbouring
#' `x`s are assumed to be normally distibuted,
#'
#' \deqn{x_i - x_{i-1} \sim \text{N}(0, \sigma^2), i = 2, \cdots, n}
#'
#' For identifiability,
#' the mean value for the $x_i$ is assumed to be
#' (almost) exactly equal to zero.
#' 
#' Standard deviation \eqn{\sigma} is drawn from a
#' half-normal distribution,
#' 
#' \deqn{\sigma \sim \text{N}^+(0, \text{s}^2)}
#'
#' (A half-normal distribution has the same shape as a normal
#' distribution, but is defined only for non-negative
#' values.)
#'
#' The scale for the half-normal distribution, `s`, defaults
#' to 1, but can be set to other values. Lower values
#' for `scale` lead to smoother series of `x`s, and
#' higher values lead to rougher series.
#'
#' @param s A positive, finite number. Default is 1.
#'
#' @returns An object of class `bage_prior_rw`.
#'
#' @seealso
#' - [RW2()] for a random walk with drift
#' (a second-order random walk).
#' - `RW()` is usually called as part of
#' a call to [set_prior()]
#'
#' @examples
#' RW()
#' RW(s = 0.5)
#' @export
RW <- function(s = 1) {
    check_scale(s, x_arg = "s", zero_ok = FALSE)
    scale <- as.double(s)
    new_bage_prior_rw(scale = scale)
}


## 'bage_prior_rw2' only ever created by call to 'set_prior' function

## HAS_TESTS
#' Random walk with drift prior
#'
#' Prior in which units follow a random walk with drift.
#' Second-order differences are normally distributed,
#' 
#' \deqn{(x_i - x_{i-1}) - (x_{i-1} - x_{i-2}) \sim \text{N}(0, \sigma^2),
#'       i = 3, \cdots, n}
#'
#' For identifiability,
#' the mean value for the $x_i$ is assumed to be
#' (almost) exactly equal to zero.
#'
#' When `flat` is `FALSE` (the default),
#' no constraint is placed on the overall slope of the $x_i$.
#' When `flat` is `TRUE`, a line through the middle of the
#' $x_i$ must be be horizontal.
#' 
#' Standard deviation \eqn{\sigma} is drawn from a
#' half-normal distribution,
#' 
#' \deqn{\sigma \sim \text{N}^+(0, \text{s}^2)}
#'
#' (A half-normal distribution has the same shape as a normal
#' distribution, but is defined only for non-negative
#' values.)
#'
#' The scale for the half-normal distribution, `s`, defaults
#' to 1, but can be set to other values. Lower values
#' for `scale` lead to smoother series of `x`s, and
#' higher values lead to rougher series.
#'
#' @param s A positive, finite number. Default is 1.
#' @param flat Whether the overall slope of the
#' terms is constrained to be zero. Default
#' is `FALSE`.
#'
#' @returns An object of class `bage_prior_rw`
#' or `bage_prior_rw2`.
#'
#' @seealso
#' - [RW()] for a first-order random walk.
#' - `RW2()` is usually called as part of
#' a call to [set_prior()]
#'
#' @examples
#' RW2()
#' RW2(flat = TRUE)
#' @export
RW2 <- function(s = 1, flat = FALSE) {
    check_scale(s, x_arg = "s", zero_ok = FALSE)
    scale <- as.double(s)
    check_flag(flat)
    new_bage_prior_rw2(scale = scale,
                       flat = flat)
}


## 'bage_prior_spline' only ever created by call to 'set_prior' function

## HAS_TESTS
#' P-spline prior
#'
#' Specify a P-spline (penalised spline) prior.
#' A P-spline is flexible, but
#' favours profiles that are relatively smooth.
#'
#' @section Mathematical details:
#'
#' The model for the effect, on the log scale, is
#'
#' \deqn{\beta = X \gamma}
#'
#' where
#' - \eqn{\beta} is a main effect,
#' - \eqn{X} is a matrix holding the basis functions
#' for the spline, with `n` columns, and
#' - \eqn{\alpha} is a vector of coefficients,
#' with `n` elements.
#'
#' The elements of \eqn{\gamma} are assumed to follow
#' a second order random walk,
#'
#' \deqn{(\gamma_j - \gamma_{j-1}) - (\gamma_{j-1} - \gamma_{j-2}) \sim \text{N}(0, \sigma^2)}
#'
#' Parameter \eqn{\sigma} has prior
#'
#' \deqn{\sigma \sim \text{N}^+(0, s^2)}
#'
#' where \eqn{\text{N}^+} denotes a half-normal distribution,
#' and a value for \eqn{s} is supplied by the user.
#'
#' @param n Number of spline vectors.
#' By default is `NULL`, in which case the number of
#' vectors is set to `max(ceiling(0.7 * k), 4)`
#' where `k` is the number
#' of elements in the term being modelled.
#' @param s Scale for error term.
#' Default is 1.
#'
#' @returns An object of class `"bage_prior_spline"`.
#'
#' @seealso
#' - [RW2()] etc
#'
#' @examples
#' Spline()
#' Spline(n = 10)
#' @export
Spline <- function(n = NULL, s = 1) {
    check_n(n, min = 4L, max = NULL, null_ok = TRUE)
    if (!is.null(n))
        n <- as.integer(n)
    check_scale(s, x_arg = "s", zero_ok = FALSE)
    scale <- as.double(s)
    new_bage_prior_spline(n = n,
                          scale = scale)
}


## 'bage_prior_svd' only ever created by call to 'set_prior' function

## HAS_TESTS
#' SVD prior
#'
#' Specify a scaled SVD (singular value decomposition)
#' prior for an age variable, or a combination
#' of age and sex/gender variables.
#'
#' @param scaled_svd An object created by [scaled_svd()]
#' holding scaled values from a SVD of
#' age-specific rates, probabilities, or means.
#' @param n Number of components from scaled SVD
#' to use in modelling. Must be between 1 and 10.
#' Default is 10.
#' @param indep Whether, in an interaction,
#' age profiles for different
#' sexes/genders are modelled independently.
#' See description in Details. Default is `TRUE`.
#'
#' @returns An object of class `"bage_prior_svd"`.
#'
#' @export
SVD <- function(scaled_svd, n = 5, indep = TRUE) {
    nm_scaled_svd <- deparse1(substitute(scaled_svd))
    if (!inherits(scaled_svd, "bage_scaled_svd"))
        cli::cli_abort(c("{.arg scaled_svd} does not hold scaled SVD values.",
                         i = "{.arg scaled_svd} has class {.cls {class(scaled_svd)}}.",
                         i = "{.arg scaled_svd} should have class {.cls bage_scaled_svd}."))
    check_n(n, min = 1L, max = 10L, null_ok = FALSE)
    n <- as.integer(n)
    check_flag(indep)
    new_bage_prior_svd(scaled_svd = scaled_svd,
                       nm_scaled_svd = nm_scaled_svd,
                       n = n,
                       indep = indep)
}


## Internal constructors ------------------------------------------------------

## Assume that inputs are all correct.
## (Checking is done by user-visible functions.)

## 'i_prior' is the index number for the prior. 
## It is *very* important that this be 
## consistent with value used by function
## 'logpost' in src/bage.cpp
##
## 'const' is a vector of doubles holding constants
## used in calculation of log-posterior.
##
## 'n_hyper' is the number of elements in the 'hyper'
## vector that are associated with this prior
##
## 'specific' is a general list of objects
## contained in this prior

## HAS_TESTS
new_bage_prior_ar1 <- function(scale, min, max) {
    shape1 <- 2.0
    shape2 <- 2.0
    ans <- list(i_prior = 5L,
                const = c(shape1, shape2, min, max, scale),
                n_hyper = 2L, ## logit_coef, log_sd
                specific = list(shape1 = shape1,
                                shape2 = shape2,
                                min = min,
                                max = max,
                                scale = scale))
    class(ans) <- c("bage_prior_ar1", "bage_prior")
    ans
}

## HAS_TESTS
new_bage_prior_known <- function(values) {
    ans <- list(i_prior = 0L,
                const = 0, ## not used
                n_hyper = 0L,
                specific = list(values = values))
    class(ans) <- c("bage_prior_known", "bage_prior")
    ans
}

## HAS_TESTS
new_bage_prior_norm <- function(scale) {
    ans <- list(i_prior = 1L,
                const = scale,
                n_hyper = 1L, ## log_sd
                specific = list(scale = scale))
    class(ans) <- c("bage_prior_norm", "bage_prior")
    ans
}

## HAS_TESTS
new_bage_prior_normfixed <- function(sd) {
    ans <- list(i_prior = 2L,
                const = sd,
                n_hyper = 0L, ## log_sd
                specific = list(sd = sd))
    class(ans) <- c("bage_prior_normfixed", "bage_prior")
    ans
}

## HAS_TESTS
new_bage_prior_rw <- function(scale) {
    sd_intercept <- 0.001
    ans <- list(i_prior = 3L,
                const = c(scale, sd_intercept),
                n_hyper = 1L, ## log_sd
                specific = list(scale = scale))
    class(ans) <- c("bage_prior_rw", "bage_prior")
    ans
}

## HAS_TESTS
new_bage_prior_rw2 <- function(scale, flat) {
    sd_intercept <- 0.001
    sd_slope <- if (flat) sd_intercept else 1
    ans <- list(i_prior = 4L,
                const = c(scale, sd_intercept, sd_slope),
                n_hyper = 1L, ## log_sd
                specific = list(scale = scale))
    class(ans) <- c("bage_prior_rw2", "bage_prior")
    ans
}

## HAS_TESTS
new_bage_prior_spline <- function(n, scale) {
    sd_intercept <- 0.001
    sd_slope <- 1
    ans <- list(i_prior = 6L,
                const = c(scale, sd_intercept, sd_slope),
                n_hyper = 1L, ## log_sd
                specific = list(n = n,
                                scale = scale))
    class(ans) <- c("bage_prior_spline", "bage_prior")
    ans
}

## HAS_TESTS
new_bage_prior_svd <- function(scaled_svd, nm_scaled_svd, n, indep) {
    ans <- list(i_prior = 7L,
                const = 0, ## not used
                n_hyper = 0L,
                specific = list(scaled_svd = scaled_svd,
                                nm_scaled_svd = nm_scaled_svd,
                                n = n,
                                indep = indep))
    class(ans) <- c("bage_prior_svd", "bage_prior")
    ans
}
