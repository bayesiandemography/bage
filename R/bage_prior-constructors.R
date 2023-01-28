
## User-visible constructors --------------------------------------------------

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
                                        any.missing = FALSE)
    values <- as.double(values)
    new_bage_prior_known(values = values)
}


## HAS_TESTS
#' Normal prior
#'
#' Prior in which units are drawn independently from a normal
#' distribution. The default prior for most terms.
#'
#' The normal distribution has mean `0` and standard
#' deviation `s`.
#' 
#' \deqn{x \sim \text{N}(0, s^2)}
#'
#' Standard deviation `s` is drawn from a half-normal
#' distribution,
#'
#' \deqn{s \sim \text{N}^+(0, \text{scale}^2)}
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
#' @param scale A positive, finite number.
#'
#' @returns An object of class `bage_prior_norm`.
#'
#' @seealso `N()` is usually called within [set_prior()].
#' Other priors are [RW()], [RW2()].
#'
#' @examples
#' N()
#' N(scale = 0.5)
#' @export
N <- function(scale = 1) {
    scale <- check_and_tidy_scale(scale) 
    new_bage_prior_norm(scale = scale)
}


## HAS_TESTS
#' One-dimensional random walk priors
#'
#' Priors in which units follow a one-dimensional
#' random walk or random walk with drift.
#' 
#' With `RW()`, increments between neighbouring
#' `x`s are normally distibuted,
#'
#' \deqn{x_i - x_{i-1} \sim \text{N}(0, s^2)}
#'
#' With `RW2()`, increments in increments
#' are normally distributed,
#' #'
#' \deqn{(x_i - x_{i-1}) - (x_{i-1} - x_{i-2}) \sim \text{N}(0, s^2)}
#' 
#' In both cases, standard deviation `s` is drawn from a
#' half-normal distribution,
#' 
#' \deqn{s \sim \text{N}^+(0, \text{scale}^2)}
#'
#' (A half-normal distribution has the same shape as a normal
#' distribution, but is defined only for non-negative
#' values.)
#'
#' The scale for the half-normal distribution defaults
#' to 1, but can be set to other values. Lower values
#' for `scale` lead to smoother series of `x`s, and
#' higher values lead to rougher series.
#'
#' @param scale A positive, finite number.
#'
#' @returns An object of class `bage_prior_rw`
#' or `bage_prior_rw2`.
#'
#' @seealso `RW()` or `RW2()` are usually called within
#' [set_prior()]. Other priors are [N()].
#'
#' @examples
#' RW()
#' RW(scale = 0.5)
#' RW2()
#' @export
RW <- function(scale = 1) {
    scale <- check_and_tidy_scale(scale)
    new_bage_prior_rw(scale = scale)
}

#' @export
#' @rdname RW
RW2 <- function(scale = 1) {
    scale <- check_and_tidy_scale(scale)
    new_bage_prior_rw2(scale = scale)
}


## MRW <- function() NULL



## Internal constructors ------------------------------------------------------


## Assume that inputs are all correct.
## (Checking is done by user-visible functions.)

## 'i_prior' is the index number for the prior. 
## It is *very* important that this be 
## consistent with value used by function
## 'logpost' in src/bage.cpp
##
## 'consts' is a vector of doubles holding constants
## used in calculation of log-posterior.
##
## 'n_hyper' is the number of elements in the 'hyper'
## vector that are associated with this prior
##
## 'specific' is a list of objects associated
## with this prior that are not used by 'fit.bage_mod',
## but that may be used by other functions,
## eg for printing.

new_bage_prior_known <- function(values) {
    ans <- list(i_prior = 0L,
                consts = double(),
                n_hyper = 0L,
                specific = list(values = values))
    class(ans) <- c("bage_prior_known", "bage_prior")
    ans
}


new_bage_prior_norm <- function(scale) {
    ans <- list(i_prior = 1L,
                consts = scale,
                n_hyper = 1L,
                specific = list(scale = scale))
    class(ans) <- c("bage_prior_norm", "bage_prior")
    ans
}

new_bage_prior_rw <- function(scale) {
    ans <- list(i_prior = 2L,
                consts = scale,
                n_hyper = 1L,
                specific = list(scale = scale))
    class(ans) <- c("bage_prior_rw", "bage_prior")
    ans
}

new_bage_prior_rw2 <- function(scale) {
    ans <- list(i_prior = 3L,
                consts = scale,
                n_hyper = 1L,
                specific = list(scale = scale))
    class(ans) <- c("bage_prior_rw2", "bage_prior")
    ans
}



## Validators -----------------------------------------------------------------


validate_bage_prior <- function(prior) {
    checkmate::assert_integer(prior@i_prior,
                              lower = 0L,
                              len = 1L,
                              any.missing = FALSE)
    checkmate::assert_double(prior@consts,
                             any.missing = FALSE)
    checkmate::assert_integer(prior@n_hyper,
                              lower = 0L,
                              len = 1L,
                              any.missing = FALSE)
    checkmate::assert_list(prior@specific)
    prior
}
    
    
