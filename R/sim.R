
## draw_vals helpers ----------------------------------------------------------

## HAS_TESTS
#' Generate an AR1 vector
#'
#' Each column is one draw.
#'
#' @param coef Vector of values
#' @param sd Vector of values
#' @param labels Names of elements
#'
#' @returns A matrix, with dimnames.
#'
#' @noRd
draw_vals_ar1 <- function(coef, sd, labels) {
    n_sim <- length(coef)
    n_par <- length(labels)
    sd_scaled <- sqrt(1 - coef^2) * sd
    ans <- matrix(nrow = n_par,
                  ncol = n_sim,
                  dimnames = list(labels, seq_len(n_sim)))
    ans[1L, ] <- rnorm(n = n_sim, sd = sd)
    for (i in seq_len(n_par - 1L))
        ans[i + 1L, ] <- rnorm(n = n_sim,
                               mean = coef * ans[i, ],
                               sd =  sd_scaled)
    ans
}

## HAS_TESTS
#' Draw the 'coef' parameter for a prior
#'
#' @param prior An object of class 'bage_prior'
#' @param n_sim Number of draws
#'
#' @returns A numeric vector
#'
#' @noRd
draw_vals_coef <- function(prior, n_sim) {
    specific <- prior$specific
    shape1 <- specific$shape1
    shape2 <- specific$shape2
    min <- specific$min
    max <- specific$max
    ans_raw <- stats::rbeta(n = n_sim,
                            shape1 = shape1,
                            shape2 = shape2)
    ans <- min + ans_raw * (max - min)
    ans
}

## HAS_TESTS
#' Generate a RW vector
#'
#' Each column is one draw.
#'
#' @param sd Vector of values
#' @param sd_intercept Scalar
#' @param labels Names of elements
#'
#' @returns A matrix, with dimnames.
#'
#' @noRd
draw_vals_rw <- function(sd, sd_intercept, labels) {
    n_par <- length(labels)
    n_sim <- length(sd)
    A <- make_rw_matrix(n_par)
    sd_v <- rbind(matrix(sd,
                         nrow = n_par - 1L,
                         ncol = n_sim,
                         byrow = TRUE),
                  sd_intercept)
    v <- matrix(stats::rnorm(n = n_par * n_sim, sd = sd_v),
                nrow = n_par,
                ncol = n_sim)
    ans <- solve(A, v)
    dimnames(ans) <- list(labels, seq_len(n_sim))
    ans
}

## HAS_TESTS
#' Generate a RW2 vector
#'
#' Each column is one draw.
#'
#' @param sd Vector of values
#' @param sd_intercept Scalar
#' @param sd_slope Scalar
#' @param labels Names of elements
#'
#' @returns A matrix, with dimnames.
#'
#' @noRd
draw_vals_rw2 <- function(sd, sd_intercept, sd_slope, labels) {
    n_par <- length(labels)
    n_sim <- length(sd)
    A <- make_rw2_matrix(n_par)
    sd_v <- rbind(matrix(sd,
                         nrow = n_par - 2L,
                         ncol = n_sim,
                         byrow = TRUE),
                  sd_intercept,
                  sd_slope)
    v <- matrix(stats::rnorm(n = n_par * n_sim, sd = sd_v),
                nrow = n_par,
                ncol = n_sim)
    ans <- solve(A, v)
    dimnames(ans) <- list(labels, seq_len(n_sim))
    ans
}

## HAS_TESTS
#' Draw the 'sd' parameter for a prior
#'
#' @param prior An object of class 'bage_prior'
#' @param n_sim Number of draws
#'
#' @returns A numeric vector
#'
#' @noRd
draw_vals_sd <- function(prior, n_sim) {
    scale <- prior$specific$scale
    ans <- stats::rnorm(n = n_sim, sd = scale)
    ans <- abs(ans)
    ans
}

## HAS_TESTS
#' Make a difference matrix
#'
#' Make a matrix that takes first differences of an n-element vector
#'
#' @param n Number of elements of vector
#'
#' @returns An n-1 x n matrix
#'
#' @noRd
make_diff_matrix <- function(n) {
    ans <- matrix(0L, nrow = n - 1L, ncol = n)
    ans[row(ans) == col(ans)] <- -1L
    ans[row(ans) == col(ans) - 1L] <- 1L
    ans
}

## HAS_TESTS
#' Make matrix used in generating a random walk
#'
#' Multiplying 'x' by this matrix creates
#' the vector c(diff(x), mean(x))
#'
#' @param n Number of rows/columns of matrix
#'
#' @returns An n x n matrix
#'
#' @noRd
make_rw_matrix <- function(n) {
    D <- make_diff_matrix(n)
    rbind(D,
          1 / n)
}

## HAS_TESTS
#' Make matrix used in generating a random walk
#'
#' Multiplying 'x' by this matrix creates
#' the vector c(diff(x), mean(x))
#'
#' @param n Number of rows/columns of matrix
#'
#' @returns An n x n matrix
#'
#' @noRd
make_rw2_matrix <- function(n) {
    D1 <- make_diff_matrix(n)
    D2 <- make_diff_matrix(n - 1L)
    R <- D2 %*% D1
    h <- (-(n + 1L) + 2L * seq_len(n)) / (n - 1L)
    h <- h / sum(h^2)
    rbind(R,
          1 / n,
          h)
}







## UNDER CONSTRUCTION ---------------------------------------------------------

## mod <- mod_pois(deaths ~ age * sex + time,
##                 data = data,
##                 exposure = popn) %>%
##     set_prior(`(Intercept)` ~ NFix(sd = 0.3)) %>%
##     set_prior(time ~ SVD(HMD))

## report <- report_sim(mod, n_sim = 100)

## is_mod_valid(report)

report_sim <- function(mod_sim, mod_est = NULL, n_sim = 100) {
    comparisons <- vector(mod = "list", length = n_sim)
    if (is_null(mod_est))
        mod_est <- mod_sim
    for (i_sim in seq_len(n_sim)) {
        vals_true <- draw_vals_true(mod_sim)
        outcome <- vals_true$outcome
        mod_est <- update_mod_outcome(mod = mod,
                                      outcome = outcome)
        mod <- fit(mod)
        vals_est <- get_vals_est(mod)
        comparison <- compare_est_true(vals_true = vals_true,
                                       vals_est = vals_est)
        comparisons[[i_sim]] <- comparison
    }
    make_report_sim(mod = mod,
                    comparisons = comparisons)
}

    
draw_vals.bage_mod_pois <- function(mod, n_sim) {
    offset <- mod$offset
    has_season <- has_season(mod)
    has_disp <- has_disp(mod)
    vals_hyper <- draw_vals_hyper_all(mod = mod,
                                      n_sim = n_sim)
    vals_par <- draw_vals_par_all(mod = mod,
                                  vals_hyper = vals_hyper,
                                  n_sim = n_sim)
    vals_linpred_par <- make_vals_linpred_par(mod = mod,
                                              vals_par = vals_par)
    if (has_season) {
        vals_season <- make_vals_season(mod = mod,
                                        n_sim = n_sim)
        vals_linpred_season <- make_vals_linpred_season(mod = mod,
                                                        vals_season = vals_season)
        vals_linpred <- vals_linpred_par + vals_linpred_season
    }
    else
        vals_linpred <- vals_linpred_par
    if (has_disp) {
        vals_disp <- draw_vals_disp(mod = mod,
                                    n_sim = n_sim)
        vals_param <- draw_vals_param_pois(vals_linpred = vals_linpred,
                                           vals_disp = vals_disp)
    }
    else
        vals_param <- exp(vals_linpred)
    vals_outcome <- draw_vals_outcome_pois(vals_param = vals_param,
                                           offset = offset)
    ans <- list(outcome = vals_outcome,
                param = vals_param)
    if (has_disp)
        ans <- c(ans,
                 list(disp = vals_disp))
    ans <- c(ans,
             list(par = vals_par,
                  hyper = vals_hyoper))
    ans
}
        

draw_vals_hyper_all <- function(mod, n_sim) {
    priors <- mod$priors
    lapply(priors,
           draw_vals_hyper,
           n_sim = n_sim)
}

draw_vals_par_all <- function(mod, vals_hyper, n_sim) {
    priors <- mod$priors
    levels_par <- mod$levels_par
    agesex <- make_agesex(mod)
    .mapply(draw_vals_par,
            dots = list(prior = priors,
                        vals_hyper = vals_hyper,
                        levels_par = levels_par,
                        agesex = agesex),
            MoreArgs = list(n_sim = n_sim))
}



draw_vals_linpred <- function(mod, vals_par) {
    
    linpred <- make_linpred_par_true(mod = mod,
                                     par_true = par_true)
    if (has_season) {
        season_true <- draw_season_true(mod)
        linpred_season <- make_linpred_season_true(mod = mod,
                                                   season_true = season_true)
        linpred <- linpred + linpred_season
    }
}
   
    
    
