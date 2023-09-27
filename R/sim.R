
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
#' Draw values for hyper-parameters for all priors in a model
#'
#' @param mod Object of class "bage_mod"
#' @param n_sim Number of draws
#'
#' @returns A named list
#'
#' @noRd
draw_vals_hyper_mod <- function(mod, n_sim) {
    priors <- mod$priors
    lapply(priors,
           draw_vals_hyper,
           n_sim = n_sim)
}

## HAS_TESTS
#' Draw values for hyper-parameters for all priors in a model
#'
#' @param mod Object of class "bage_mod"
#' @param vals_hyper List of lists.
#' @param n_sim Number of draws
#'
#' @returns A named list of matrices.
#'
#' @noRd
draw_vals_par_mod <- function(mod, vals_hyper, n_sim) {
    priors <- mod$priors
    levels_par <- mod$levels_par
    terms_par <- make_terms_par(mod)
    levels_par <- split(levels_par, terms_par)
    agesex <- make_agesex(mod)
    ans <- .mapply(draw_vals_par,
                   dots = list(prior = priors,
                               vals_hyper = vals_hyper,
                               levels_par = levels_par,
                               agesex = agesex),
                   MoreArgs = list(n_sim = n_sim))
    names(ans) <- names(priors)
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
#' Draw values for season effects, and the sd parameter for season effects
#'
#' Note that if 'n_time' is not a multiple of 'n_season', some seasons will
#' have fewer than 'n_time' / 'n_season' effects
#'
#' @param mod Object of class 'bage_mod'
#' @param n_sim Number of draws
#'
#' @returns A named list with the sd and season effect
#'
#' @noRd
draw_vals_season <- function(mod, n_sim) {
    n_season <- mod$n_season
    n_time <- n_time(mod)
    n_par <- ceiling(n_time / n_season)
    const <- make_const_season(mod)
    scale <- const[[1L]]
    sd_intercept <- const[[2L]]
    A <- make_rw_matrix(n_par)
    vals_season <- array(dim = c(n_season, n_par, n_sim))
    vals_sd <- stats::rnorm(n = n_sim, sd = scale)
    vals_sd <- abs(vals_sd)
    sd_v <- array(dim = c(n_season, n_par, n_sim))
    sd_v[ , -n_par, ] <- rep(vals_sd, each = n_season * (n_par - 1L))
    sd_v[ , n_par, ] <- sd_intercept
    for (i in seq_len(n_season)) {
        v <- matrix(stats::rnorm(n = n_par * n_sim, sd = sd_v[i, , ]),
                    nrow = n_par,
                    ncol = n_sim)
        vals_season[i, , ] <- solve(A, v)
    }
    vals_season <- matrix(vals_season, ncol = n_sim)
    vals_season <- vals_season[seq_len(n_time), ]
    list(sd = vals_sd,
         season = vals_season)
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


## HAS_TESTS
#' Combine parameter values, then transform to align with outcome
#'
#' @param mod Object of class 'bage_mod'
#' @param vals_par List of matrices
#'
#' @returns A matrix
#'
#' @noRd
make_vals_linpred_par <- function(mod, vals_par) {
    matrix_par_outcome <- make_combined_matrix_par_outcome(mod)
    matrix_par_outcome <- as.matrix(matrix_par_outcome)
    vals_par <- do.call(rbind, vals_par)
    matrix_par_outcome %*% vals_par
}


## HAS_TESTS
#' Transform seaon effect to align with outcome
#'
#' @param mod Object of class 'bage_mod'
#' @param vals_season List with vector and matrix
#'
#' @returns A matrix
#'
#' @noRd
make_vals_linpred_season <- function(mod, vals_season) {
    matrix_season_outcome <- mod$matrix_season_outcome
    matrix_season_outcome <- as.matrix(matrix_season_outcome)
    season <- vals_season$season
    matrix_season_outcome %*% season
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
    if (is_null(mod_est))
        mod_est <- mod_sim
    vals_sim_true_all <- draw_vals(mod = mod_sim,
                                   n_sim = n_sim)
    comparisons <- vector(mod = "list", length = n_sim)
    for (i_sim in seq_len(n_sim)) {
        vals_sim_true <- vals_sim_true_all[[i_sim]]
        outcome <- vals_sim_true[["outcome"]]
        mod_est <- update_outcome(mod = mod_est,
                                  outcome = outcome)
        mod_est <- fit(mod_est)
        vals_sim_est <- get_vals_sim_est(mod_est)
        comparison <- compare_est_true(vals_est = vals_sim_est,
                                       vals_true = vals_sim_true)
        comparisons[[i_sim]] <- comparison
    }
    make_report_sim(mod = mod,
                    comparisons = comparisons)
}

    
draw_vals_mod <- function(mod, n_sim) {
    offset <- mod$offset
    has_season <- has_season(mod)
    has_disp <- has_disp(mod)
    vals_hyper <- draw_vals_hyper_mod(mod = mod,
                                      n_sim = n_sim)
    vals_par <- draw_vals_par_mod(mod = mod,
                                  vals_hyper = vals_hyper,
                                  n_sim = n_sim)
    vals_linpred <- make_vals_linpred_par(mod = mod,
                                          vals_par = vals_par)
    if (has_season) {
        vals_season <- draw_vals_season(mod = mod,
                                        n_sim = n_sim)
        vals_linpred_season <- make_vals_linpred_season(mod = mod,
                                                        vals_season = vals_season)
        vals_linpred <- vals_linpred + vals_linpred_season
    }
    else
        vals_season <- NULL
    if (has_disp) {
        vals_disp <- draw_vals_disp(mod)
        vals_expected <- transform_linpred(mod = mod,
                                           linpred = linpred)
        vals_fitted <- draw_vals_fitted(mod = mod,
                                        vals_expected = vals_expected,
                                        vals_disp = vals_disp)
    }
    else {
        vals_disp <- NULL
        vals_expected <- NULL
        vals_fitted <- transform_linpred(mod = mod,
                                         linpred = linpred)
    }
    ans <- list(hyper = vals_hyper,
                par = vals_par,
                season = vals_season,
                expected = vals_expected,
                fitted = vals_fitted,
                disp = vals_disp)
    ans <- split_sim(ans)
    ans
}



   
    
    
