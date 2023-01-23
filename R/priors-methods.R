

## 'draw_hyper' ---------------------------------------------------------------

draw_hyper <- function(prior) {
  UseMethod("draw_hyper")
}

draw_hyper.bage_prior_norm <- function(prior) {
    scale <- prior$scale
    sd <- abs(stats::rnorm(n = 1L, sd = scale))
    list(sd = sd)
}

draw_hyper.bage_prior_rw <- function(prior) {
    scale <- prior$scale
    sd <- abs(stats::rnorm(n = 1L, sd = scale))
    list(sd = sd)
}

draw_hyper.bage_prior_rw2 <- function(prior) {
    scale <- prior$scale
    sd <- abs(stats::rnorm(n = 1L, sd = scale))
    list(sd = sd)
}



## 'draw_par' -----------------------------------------------------------------


draw_par <- function(prior, hyper, par) {
  UseMethod("draw_par")
}

draw_par.bage_prior_norm <- function(prior, hyper, par) {
    n <- length(par)
    sd <- hyper$sd
    stats::rnorm(n = n, sd = sd)
}

draw_par.bage_prior_rw <- function(prior, hyper, par) {
    n <- length(par)
    sd <- hyper$sd
    diff <- stats::rnorm(n = n - 1L, sd = sd)
    ans <- c(0, cumsum(diff))
    ans <- ans - mean(ans)
    ans
}

draw_par.bage_prior_rw2 <- function(prior, hyper, par) {
    n <- length(par)
    sd <- hyper$sd
    diff2 <- stats::rnorm(n = n - 2L, sd = sd)
    diff <- c(0, cumsum(diff2))
    ans <- c(0, cumsum(diff))
    ans <- ans - mean(ans)
    ans
}
