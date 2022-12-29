
## User-visible constructors --------------------------------------------------

#' Normal prior
#'
#' Prior in which units are drawn independently from a normal
#' distribution. The default prior for most terms.
#'
#' The normal distribution has mean `0` and standard
#' deviation `s`. Standard deviation `s` is drawn from a
#' half-normal distribution,
#' 
#' \deqn{x \sim N(0, s^2)}
#'
#' \deqn{s \sim N^+(0, \text{scale}^2)}
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
#'
#' @examples
#' N()
#' N(scale = 0.5)
#' @export
N <- function(scale = 1) {
    scale <- check_and_tidy_scale(scale) 
    new_bage_prior_norm(scale = scale)
}

RW <- function(scale = 1) {
    scale <- check_and_tidy_scale(scale)
    new_bage_prior_rw(scale = scale)
}

RW2 <- function(scale = 1) {
    scale <- check_and_tidy_scale(scale)
    new_bage_prior_rw2(scale = scale)
}


## MRW <- function() NULL



## Internal constructors ------------------------------------------------------

new_bage_prior_norm <- function(scale = 1) {
    ans <- list(scale = scale,
                n_hyper = 1L,
                i_prior = 1L)
    class(ans) <- c("bage_prior_norm", "bage_prior")
    ans
}

new_bage_prior_rw <- function(scale = 1) {
    ans <- list(scale = scale,
                n_hyper = 1L,
                i_prior = 2L)
    class(ans) <- c("bage_prior_rw", "bage_prior")
    ans
}

new_bage_prior_rw2 <- function(scale = 1) {
    ans <- list(scale = scale,
                n_hyper = 1L,
                i_prior = 3L)
    class(ans) <- c("bage_prior_rw2", "bage_prior")
    ans
}



## Validators -----------------------------------------------------------------



