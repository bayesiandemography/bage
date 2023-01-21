

## 'draw_hyper' ---------------------------------------------------------------

draw_hyper <- function(prior) {
  UseMethod("draw_hyper")
}

draw_hyper.bage_prior_norm <- function(prior) {
    scale <- prior$scale
    sd <- abs(rnorm(n = 1L, sd = scale))
    list(sd = sd)
}

draw_hyper.bage_prior_rw <- function(prior) {
    scale <- prior$scale
    sd <- abs(rnorm(n = 1L, sd = scale))
    list(sd = sd)
}

draw_hyper.bage_prior_rw2 <- function(prior) {
    scale <- prior$scale
    sd <- abs(rnorm(n = 1L, sd = scale))
    list(sd = sd)
}



## 'draw_par' -----------------------------------------------------------------


draw_par <- function(prior, hyper, par) {
  UseMethod("draw_par")
}

draw_par.bage_prior_norm <- function(prior, hyper, par) {
    n <- length(par)
    sd <- hyper$sd
    rnorm(n = n, sd = sd)
}

draw_par.bage_prior_rw <- function(prior, hyper, par) {
    n <- length(par)
    sd <- hyper$sd
    diff <- rnorm(n = n - 1L, sd = sd)
    ans <- c(0, cumsum(diff))
    ans <- ans - mean(ans)
    ans
}

draw_par.bage_prior_rw2 <- function(prior, hyper, par) {
    n <- length(par)
    sd <- hyper$sd
    diff2 <- rnorm(n = n - 2L, sd = sd)
    diff <- c(0, cumsum(diff2))
    ans <- c(0, cumsum(diff))
    ans <- ans - mean(ans)
    ans
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

## Need to ensure that the values for 'i_prior' are
## consistent with the values used by function
## 'logpost' in src/bage.cpp

new_bage_prior_norm <- function(scale = 1) {
    ans <- list(n_hyper = 1L,
                consts = scale,
                i_prior = 1L)
    class(ans) <- c("bage_prior_norm", "bage_prior")
    ans
}

new_bage_prior_rw <- function(scale = 1) {
    ans <- list(scale = scale,
                n_hyper = 1L,
                consts = scale,
                i_prior = 2L)
    class(ans) <- c("bage_prior_rw", "bage_prior")
    ans
}

new_bage_prior_rw2 <- function(scale = 1) {
    ans <- list(scale = scale,
                n_hyper = 1L,
                consts = scale,
                i_prior = 3L)
    class(ans) <- c("bage_prior_rw2", "bage_prior")
    ans
}



## Validators -----------------------------------------------------------------



