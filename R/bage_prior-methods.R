
## Note that methods require an '@export' tag,
## even when the generic function is not exported
## https://github.com/r-lib/devtools/issues/2293



## 'draw_vals_hyper' ----------------------------------------------------------

## Still under construction - possibly to be used for simulation

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

## Still under construction - possibly to be used for simulation

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
    ## better to use matrices
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
    ## better to use matrices
    diff2 <- matrix(stats::rnorm(n = (length_par - 2L) * n_draw, sd = sd),
                   nrow = length_par - 2L,
                   ncol = n_draw)
    diff <- rbind(rep(0, times = n_draw),
                  matrixStats::colCumsums(diff2))
    ans <- rbind(rep(0, times = n_draw),
                 matrixStats::colCumsums(diff))
    ## also need slope
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


## 'levels_const' -------------------------------------------------------------

#' Names of constants
#'
#' @param prior An object of class 'bage_prior'.
#'
#' @returns A character vector.
#'
#' @noRd
levels_const <- function(prior) {
    UseMethod("levels_const")
}

## HAS_TESTS
#' @export
levels_const.bage_prior_ar1 <- function(prior)
    c("shape1", "shape2", "min", "max", "scale")

## HAS_TESTS
#' @export
levels_const.bage_prior_known <- function(prior)
    character()

## HAS_TESTS
#' @export
levels_const.bage_prior_norm <- function(prior)
    "scale"

## HAS_TESTS
#' @export
levels_const.bage_prior_normfixed <- function(prior)
    "sd"

## HAS_TESTS
#' @export
levels_const.bage_prior_rw <- function(prior)
    "scale"

## HAS_TESTS
#' @export
levels_const.bage_prior_rw2 <- function(prior)
    "scale"


## 'levels_hyper' -------------------------------------------------------------

#' Names of hyper-parameters
#'
#' @param prior An object of class 'bage_prior'.
#'
#' @returns A character vector.
#'
#' @noRd
levels_hyper <- function(prior) {
    UseMethod("levels_hyper")
}

## HAS_TESTS
#' @export
levels_hyper.bage_prior_ar1 <- function(prior)
    c("coef", "sd")

## HAS_TESTS
#' @export
levels_hyper.bage_prior_known <- function(prior)
    character()

## HAS_TESTS
#' @export
levels_hyper.bage_prior_norm <- function(prior)
    "sd"

## HAS_TESTS
#' @export
levels_hyper.bage_prior_normfixed <- function(prior)
    character()

## HAS_TESTS
#' @export
levels_hyper.bage_prior_rw <- function(prior)
    "sd"

## HAS_TESTS
#' @export
levels_hyper.bage_prior_rw2 <- function(prior)
    "sd"


## 'str_call_prior' -----------------------------------------------------------

#' Create string describing prior
#'
#' Create string describing prior that
#' (inspired by printing of objects in Python)
#' looks like a call to the constructor function
#'
#' @param prior An object of class "bage_prior"
#'
#' @returns A string
#'
#' @noRd
str_call_prior <- function(prior) {
    UseMethod("str_call_prior")
}

## HAS_TESTS
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

## HAS_TESTS
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

## HAS_TESTS
#' @export
str_call_prior.bage_prior_norm <- function(prior) {
    scale <- prior$specific$scale
    if (isTRUE(all.equal(scale, 1)))
        "N()"
    else
        sprintf("N(scale=%s)", scale)
}

## HAS_TESTS
#' @export
str_call_prior.bage_prior_normfixed <- function(prior) {
    sd <- prior$specific$sd
    if (isTRUE(all.equal(sd, 1)))
        "NFixed()"
    else
        sprintf("NFixed(sd=%s)", sd)
}

## HAS_TESTS
#' @export
str_call_prior.bage_prior_rw <- function(prior) {
    scale <- prior$specific$scale
    if (isTRUE(all.equal(scale, 1)))
        "RW()"
    else
        sprintf("RW(scale=%s)", scale)
}

## HAS_TESTS
#' @export
str_call_prior.bage_prior_rw2 <- function(prior) {
    scale <- prior$specific$scale
    if (isTRUE(all.equal(scale, 1)))
        "RW2()"
    else
        sprintf("RW2(scale=%s)", scale)
}


## 'transform_hyper' ----------------------------------------------------------

#' Transform to convert working TMB version
#' of parameter back to original units
#'
#' @param prior An object of class 'bage_prior'.
#'
#' @returns A list of functions.
#'
#' @noRd
transform_hyper <- function(prior) {
    UseMethod("transform_hyper")
}

## HAS_TESTS
#' @export
transform_hyper.bage_prior_ar1 <- function(prior)
    list(function(x) ifelse(x > 0, 1 / (1 + exp(-x)), exp(x) / (exp(x) + 1)),
         exp)

## HAS_TESTS
#' @export
transform_hyper.bage_prior_known <- function(prior)
    list()

## HAS_TESTS
#' @export
transform_hyper.bage_prior_norm <- function(prior)
    list(exp)

## HAS_TESTS
#' @export
transform_hyper.bage_prior_normfixed <- function(prior)
    list()

## HAS_TESTS
#' @export
transform_hyper.bage_prior_rw <- function(prior)
    list(exp)

## HAS_TESTS
#' @export
transform_hyper.bage_prior_rw2 <- function(prior)
    list(exp)


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
