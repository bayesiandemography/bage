

get_inv_transform <- function(nm_distn) {
    if (nm_distn == "pois")
        exp
    else if (nm_distn == "binom")
        function(x) if_else(x > 0, 1 / (1 + exp(-x)), exp(x) / (1 + exp(x)))
    else if (nm_disn == "norm")
        function(x) x
    else
        stop(gettextf("invalid value for 'nm_distn' : %s",
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

make_terms_std <- function(mod) {
    term_par <- mod$term_par
    std <- mod$std$par
    split(std, term_par)
}


make_linear_pred_mean <- function(mod) {
    terms_est <- make_terms_est(mod)
    matrices_par <- mod$matrices_par
    outcome <- mod$outcome
    ans <- double(length = length(outcome))
    for (i_term in seq_along(terms_est)) {
        m <- matrices_par[[i_term]]
        b <- terms_est[[i_term]]
        if (length(m) > 0L)
            ans <- ans + m %*% b
        else
            ans <- ans + b
    }
    ans
}

make_linear_pred_sd <- function(mod) {
    terms_std <- make_terms_std(mod)
    matrices_par <- mod$matrices_par
    outcome <- mod$outcome
    ans <- double(length = length(outcome))
    for (i_term in seq_along(terms_std)) {
        m <- matrices_par[[i_term]]
        s <- terms_std[[i_term]]
        s_sq <- s^2
        if (length(m) > 0L)
            ans <- ans + m %*% s_sq ## m consists entirely of 0s and 1s
        else
            ans <- ans + s_sq
    }
    ans <- sqrt(ans)
    ans
}

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
    



