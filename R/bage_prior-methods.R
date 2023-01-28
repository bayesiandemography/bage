
## Note that methods require an '@export' tag,
## even when the generic function is not exported
## https://github.com/r-lib/devtools/issues/2293



## 'draw_vals_hyper' ----------------------------------------------------------

draw_vals_hyper <- function(prior, n_draw) {
  UseMethod("draw_vals_hyper")
}

draw_vals_hyper.bage_prior_norm <- function(prior, n_draw) {
    scale <- prior$scale
    sd <- abs(stats::rnorm(n = n_draw, sd = scale))
    list(sd = sd)
}

draw_vals_hyper.bage_prior_rw <- function(prior, n_draw) {
    scale <- prior$scale
    sd <- abs(stats::rnorm(n = n_draw, sd = scale))
    list(sd = sd)
}

draw_vals_hyper.bage_prior_rw2 <- function(prior, n_draw) {
    scale <- prior$scale
    sd <- abs(stats::rnorm(n = n_draw, sd = scale))
    list(sd = sd)
}



## 'draw_vals_par' ------------------------------------------------------------


draw_vals_par <- function(prior, hyper, length_par) {
  UseMethod("draw_vals_par")
}

draw_vals_par.bage_prior_norm <- function(prior, hyper, length_par) {
    sd <- hyper$sd
    n_draw <- length(sd)
    matrix(stats::rnorm(n = length_par * n_draw, sd = sd),
           nrow = length_par,
           ncol = n_draw)
}

draw_vals_par.bage_prior_rw <- function(prior, hyper, length_par) {
    sd <- hyper$sd
    n_draw <- length(sd)
    diff <- matrix(stats::rnorm(n = (length_par - 1L) * n_draw, sd = sd),
                   nrow = length_par - 1L,
                   ncol = n_draw)
    ans <- rbind(rep(0, times = n_draw),
                 matrixStats::colCumsums(diff))
    ans <- scale(ans, center = TRUE, scale = FALSE)
    ans
}

draw_vals_par.bage_prior_rw2 <- function(prior, hyper, length_par) {
    sd <- hyper$sd
    n_draw <- length(sd)
    diff2 <- matrix(stats::rnorm(n = (length_par - 2L) * n_draw, sd = sd),
                   nrow = length_par - 2L,
                   ncol = n_draw)
    diff <- rbind(rep(0, times = n_draw),
                  matrixStats::colCumsums(diff2))
    ans <- rbind(rep(0, times = n_draw),
                 matrixStats::colCumsums(diff))
    ans <- scale(ans, center = TRUE, scale = FALSE)
    ans
}


## 'is_known' -----------------------------------------------------------------

#' Test whether a prior treats an intercept,
#' main effect, or interaction as known and fixed
#'
#' @param prior An object of class 'bage_prior'.
#'
#' @returns TRUE or FALSE
#'
#' @noRd
is_known <- function(prior) {
    UseMethod("is_known")
}

## HAS_TESTS
#' @export
is_known.bage_prior <- function(prior) FALSE

## HAS_TESTS
#' @export
is_known.bage_prior_known <- function(prior) TRUE


## 'values_known' -------------------------------------------------------------

#' Given a prior that treats a term as known,
#' extract the known values
#'
#' @param An object of class 'bage_prior'
#'
#' @returns A vector of doubles.
#'
#' @noRd
values_known <- function(prior) {
    UseMethod("values_known")
}

## HAS_TESTS
#' @export
values_known.bage_prior_known <- function(prior) prior$specific$values
