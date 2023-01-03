
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

make_linear_pred_var <- function(mod) {
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
    ans
}



