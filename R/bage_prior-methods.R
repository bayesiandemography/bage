
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


## 'str_call_prior' -----------------------------------------------------------

str_call_prior <- function(prior) {
    UseMethod("str_call_prior")
}

#' @export
str_call_prior.bage_prior_ar1 <- function(prior) {
    min <- prior$specific$min
    max <- prior$specific$max
    scale <- prior$specific$scale
    args <- character(3)
    if (min != 0.8)
        args[[1L]] <- sprintf("min=%s", min)
    if (max != 0.98)
        args[[2L]] <- sprintf("max=%s", max)
    if (scale != 1)
        args[[3L]] <- sprintf("scale=%s", scale)
    args <- args[nzchar(args)]
    args <- paste(args, collapse = ", ")
    sprintf("AR1(%s)", args)
}


#' @export
str_call_prior.bage_prior_known <- function(prior) {
    values <- values_known(prior)
    n <- length(values)
    if (n == 1L)
        inner <- sprintf("%s", values)
    else if (n <= 5)
        inner <- sprintf("c(%s)", paste(values, collapse = ","))
    else
        inner <- sprintf("c(%s,...,%s)", values[[1L]], values[[n]])
    sprintf("Known(%s)", inner)
}


#' @export
str_call_prior.bage_prior_norm <- function(prior) {
    scale <- prior$specific$scale
    if (isTRUE(all.equal(scale, 1)))
        "N()"
    else
        sprintf("N(scale=%s)", scale)
}

#' @export
str_call_prior.bage_prior_rw <- function(prior) {
    scale <- prior$specific$scale
    if (isTRUE(all.equal(scale, 1)))
        "RW()"
    else
        sprintf("RW(scale=%s)", scale)
}

#' @export
str_call_prior.bage_prior_rw2 <- function(prior) {
    scale <- prior$specific$scale
    if (isTRUE(all.equal(scale, 1)))
        "RW2()"
    else
        sprintf("RW2(scale=%s)", scale)
}





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
