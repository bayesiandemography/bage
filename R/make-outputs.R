
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
        is_intercept <- length(m) == 0L
        if (is_intercept)
            ans <- ans + b
        else
            ans <- ans + as.double(m %*% b)
    }
    ans
}


## HAS_TESTS
#' Get the mean value for the linear predictor
#' formed from the main effects and interactions
#'
#' @param mod A fitted model
#'
#' @returns A vector of doubles
#'
#' @noRd
make_linear_pred_std <- function(mod) {
    terms_std <- make_terms_std(mod)
    matrices_par <- mod$matrices_par
    outcome <- mod$outcome
    ans <- double(length = length(outcome))
    for (i_term in seq_along(terms_std)) {
        m <- matrices_par[[i_term]]
        s <- terms_std[[i_term]]
        s_sq <- s^2
        is_intercept <- length(m) == 0L
        if (is_intercept)
            ans <- ans + s_sq            
        else
            ans <- ans + as.double(m %*% s_sq) ## m consists entirely of 0s and 1s

    }
    ans <- sqrt(ans)
    ans
}


#' Make point estimates for rates
#'
#' Make point estimates rates/probabilities/means,
#' by back-transforming mean of linear predictor.

make_fitted_point <- function(mod) {
    nm_distn <- mod$nm_distn
    inv_transform <- get_inv_transform(nm_distn)
    linear_pred_mean <- make_linear_pred_mean(mod)
    inv_transform(linear_pred_mean)
}


#' Make lower and upper limits of credible interval
#'
#' @param mod Fitted 'bage_mod' object
#' @param interval p(upper) - p(lower)
#'
#' @returns Named list
#'
#' @noRd
make_lower_upper <- function(mod, interval) {
    alpha <- (1 - interval) / 2
    nm_distn <- mod$nm_distn
    mean <- make_linear_pred_mean(mod)
    sd <- make_linear_pred_sd(mod)
    lower_trans <- pnorm(alpha, mean = mean, sd = sd)
    upper_trans <- pnorm(1 - alpha, mean = mean, sd = sd)
    inv_transform <- get_inv_transform(nm_distn)
    lower <- inv_transform(lower_trans)
    upper <- inv_transform(upper_trans)
    list(lower = lower,
         upper = upper)
}
    



