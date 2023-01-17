
## HAS_TESTS
#' Get inverse tranformation associated
#' with distribution
#'
#' @param nm_distn "pois", "binom", or "norm"
#'
#' @returns A function
#'
#' @noRd
get_inv_transform <- function(nm_distn) {
    if (nm_distn == "pois")
        exp
    else if (nm_distn == "binom")
        function(x) ifelse(x > 0, 1 / (1 + exp(-x)), exp(x) / (1 + exp(x)))
    else if (nm_distn == "norm")
        function(x) x
    else
        stop(gettextf("invalid value for 'nm_distn' : \"%s\"",
                      nm_distn),
             call. = FALSE)
}


## HAS_TESTS
#' Extract point estimates for terms
#'
#' @param mod A fitted model
#'
#' @returns A named list.
#'
#' @noRd
make_terms_est <- function(mod) {
    term_par <- mod$term_par
    est <- mod$est$par
    split(est, term_par)
}


## HAS_TESTS
#' Extract standard errors for terms
#'
#' @param mod A fitted model
#'
#' @returns A named list.
#'
#' @noRd
make_terms_std <- function(mod) {
    term_par <- mod$term_par
    std <- mod$std$par
    split(std, term_par)
}


## IS THIS STILL NEEDED???
## HAS_TESTS
#' Get the mean value for the linear predictor
#' formed from the main effects and interactions
#'
#' @param mod A fitted model
#'
#' @returns A vector of doubles
#'
#' @noRd
make_linear_pred_mean <- function(mod) {
    terms_est <- make_terms_est(mod)
    matrices_par <- mod$matrices_par
    outcome <- mod$outcome
    ans <- double(length = length(outcome))
    for (i_term in seq_along(terms_est)) {
        m <- matrices_par[[i_term]]
        b <- terms_est[[i_term]]
        ans <- ans + as.double(m %*% b)
    }
    ans
}

## make_combined_matrix_par <- function(mod) {
##     matrices_par <- mod$matrices_par
##     has_intercept <- length(matrices_par[[1L]]) == 0L
##     if (has_intercept) {
##         ans <- do.call(


## HAS_TESTS
#' Make point estimates for rates
#'
#' Make point estimates rates/probabilities/means,
#' by back-transforming mean of linear predictor.
#'
#' Note that the posterior distribution
#' for the linear predictor is (thanks
#' to the use of Laplace's Method)
#' exactly normal, so the median of the
#' linear predictor equals the mean.
#' The median of the rate/prob/mean parameter
#' is the backtransformed median of
#' the linear predictor.
#'
#' @param mod A fitted models
#'
#' @returns A vector of doubles.
#'
#' @noRd
make_fitted_point <- function(mod) {
    nm_distn <- mod$nm_distn
    outcome <- mod$outcome
    inv_transform <- get_inv_transform(nm_distn)
    linear_pred_mean <- make_linear_pred_mean(mod)
    ans <- inv_transform(linear_pred_mean)
    if (is.array(outcome)) {
        data <- mod$data
        i <- mapping_array_to_df(a = outcome,
                                 df = data)
        ans <- ans[i]
    }
    ans    
}


make_draws_par <- function(mod) {
    est <- mod$est
    prec <- mod$prec
    n_iter <- mod$n_iter
    mean <- unlist(est, use.names = FALSE)
    chol <- chol(prec)
    sd <- backsolve(prec)
    ans <- matrix(nrow = n_iter, ncol = length(est))
    for (i_iter in seq_len(n_iter)) {
        z <- rnorm(n = n_iter)
        ans[i_iter, ] <- mean + sd %*% z
    }
    ans
}


## make_draws_linear_pred <- function(mod) {
##     draws_par <- make_draws_par(mod)
##     ans <- 
    
    
    


## HAS_TESTS
#' Make lower and upper limits of credible interval
#'
#' @param mod Fitted 'bage_mod' object
#' @param interval F(upper) - F(lower),
#' where F is the cumulative distribution
#' function.
#'
#' @returns Named list
#'
#' @noRd
make_lower_upper <- function(mod, interval) {
    checkmate::assert_number(interval, lower = 0, upper = 1)
    nm_distn <- mod$nm_distn
    outcome <- mod$outcome
    alpha <- (1 - interval) / 2
    mean <- make_linear_pred_mean(mod)
    sd <- make_linear_pred_std(mod)
    lower_trans <- qnorm(alpha, mean = mean, sd = sd)
    upper_trans <- qnorm(1 - alpha, mean = mean, sd = sd)
    inv_transform <- get_inv_transform(nm_distn)
    lower <- inv_transform(lower_trans)
    upper <- inv_transform(upper_trans)
    if (is.array(outcome)) {
        data <- mod$data
        i <- mapping_array_to_df(a = outcome,
                                 df = data)
        lower <- lower[i]
        upper <- upper[i]
    }
    list(lower = lower,
         upper = upper)
}




make_observed <- function(mod) {
    outcome <- mod$outcome
    offset <- mod$offset
    ans <- outcome / offset
    if (is.array(outcome)) {
        data <- mod$data
        i <- mapping_array_to_df(a = outcome,
                                 df = data)
        ans <- ans[i]
    }
    ans
}
        
    
## HAS_TESTS
#' Create a vector mapping an array
#' to rows of a data frame
#'
#' Construct an index vector giving
#' the index in 'a' of each row of 'df' of 'a' assoc
#' Assume that 'a' has a full set of dimnames,
#' and that the dimensions of 'a' all have
#' counterparts in 'df'.
#'
#' @param a An array
#' @param df A data frame
#'
#' @returns An integer vector with
#' length nrow(df).
#' 
#' @noRd
mapping_array_to_df <- function(a, df) {
    dim_a <- dim(a)
    n_a <- length(dim_a)
    dn_a <- dimnames(a)
    nms_a <- names(dn_a)
    nms_df <- names(df)
    ans <- vector(mode = "list", length = n_a)
    for (i_dim in seq_len(n_a)) {
        nm <- nms_a[[i_dim]]
        ans[[i_dim]] <- match(df[[nm]], dn_a[[nm]])
    }
    ans <- do.call(cbind, ans)
    if (n_a > 1L) {
        mult <- c(1L, cumprod(dim_a[-n_a]))
        ans <- ((ans - 1L) %*% mult) + 1L
    }
    ans <- as.integer(ans)
    ans
}
