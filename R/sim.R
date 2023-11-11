
## HAS_TESTS
#' Calculate errors and coverage for a single
#' set of simulated values
#'
#' @param vals_sim List with simulated values
#' @param mod_est Model being tested. Object
#' of class 'bage_mod'.
#' @param point_est_fun Mean or median. Function used
#' to calculate point estimates
#' @param include_upper Logical. Whether levels
#' of model above 'meanpar' and 'disp' are included:
#' effect, hyper, cyclical, season
#' @param widths Widths of credible intervals.
#'
#' @returns Named list
#'
#' @noRd
assess_performance <- function(vals_sim,
                               mod_est,
                               point_est_fun,
                               include_upper,
                               widths) {
    nms_upper <- c("hyper", "effect", "cyclical", "season")
    mod_est[["outcome"]] <- vals_sim[["outcome"]]
    vals_sim$outcome <- NULL
    mod_est <- fit(mod_est)
    vals_est <- get_vals_est(mod_est)
    if (!include_upper) {
        is_upper_est <- names(vals_est) %in% nms_upper
        is_upper_sim <- names(vals_sim) %in% nms_upper
        vals_est <- vals_est[!is_upper_est]
        vals_sim <- vals_sim[!is_upper_sim]
    }
    error_point_est <- calc_error_point_est(estimate = vals_est,
                                            truth = vals_sim,
                                            point_est_fun = point_est_fun)
    is_in_interval <- calc_is_in_interval(estimate = vals_est,
                                          truth = vals_sim,
                                          widths = widths)
    is_null <- vapply(vals_sim, is.null, FALSE)
    vals_sim <- vals_sim[!is_null]
    list(vals_sim = vals_sim,
         error_point_est = error_point_est,
         is_in_interval = is_in_interval)
}


## HAS_TESTS
#' Calculate point estimates from rvecs, and then calculate
#' differences from truth
#'
#' @param estimate A list of rvecs holding the estimates
#' @param truth A list of numeric vectors holding the true values
#' @param point_est_fun Name of function to use to calculate point
#' estimates from rvec
#'
#' @returns A vector of doubles
#'
#' @noRd
calc_error_point_est <- function(estimate, truth, point_est_fun) {
    if (point_est_fun == "mean")
        rvec_fun <- rvec::draws_mean
    else if (point_est_fun == "median")
        rvec_fun <- rvec::draws_median
    else
        cli::cli_abort("Internal error: Invalid value for 'point_est_fun'.")
    is_null <- vapply(estimate, is.null, FALSE)
    estimate <- estimate[!is_null]
    truth <- truth[!is_null]
    ans <- .mapply(calc_error_point_est_one,
                   dots = list(estimate = estimate,
                               truth = truth),
                   MoreArgs = list(rvec_fun = rvec_fun))
    names(ans) <- names(estimate)
    ans
}


## HAS_TESTS
#' Calculate point estimates from an rvec, and then calculate
#' difference from truth
#'
#' @param estimate An rvec with the estimates
#' @param truth A numeric vector with the true values
#' @param rvec_fun Function to use to calculate point
#' estimates from rvec
#'
#' @returns A vector of doubles
#'
#' @noRd
calc_error_point_est_one <- function(estimate, truth, rvec_fun) {
    point_est <- rvec_fun(estimate)
    as.double(point_est - truth)
}


## HAS_TESTS
#' Given a sets estimates and true values,
#' for each set, see how many true values
#' lie within intervals implied by estimates
#'
#' @param estimate A list of rvecs holding the estimates
#' @param truth A list of numeric vectors holding the true values
#' @param widths Widths of intervals (between 0 and 1)
#'
#' @returns A list of logical vectors
#'
#' @noRd
calc_is_in_interval <- function(estimate, truth, widths) {
    is_null <- vapply(estimate, is.null, FALSE)
    estimate <- estimate[!is_null]
    truth <- truth[!is_null]
    ans <- .mapply(calc_is_in_interval_one,
                   dots = list(estimate = estimate,
                               truth = truth),
                   MoreArgs = list(widths = widths))
    names(ans) <- names(estimate)
    ans
}


## HAS_TESTS
#' See which elements of 'truth' lie within intervals
#' formed from 'estimate'
#'
#' @param estimate An rvec holding estimates
#' @param truth A numeric vector holding the true values
#' @param widths Widths of intervals (between 0 and 1)
#'
#' @returns A logical vector
#'
#' @noRd
calc_is_in_interval_one <- function(estimate, truth, widths) {
    n_widths <- length(widths)
    ans <- vector(mode = "list", length = n_widths)
    names(ans) <- widths
    for (i in seq_len(n_widths)) {
        width <- widths[[i]]
        probs <- c(0.5 - width / 2, 0.5 + width / 2)
        ci <- rvec::draws_quantile(x = estimate, probs = probs)
        lower <- ci[[1L]]
        upper <- ci[[2L]]
        is_in_interval <- (lower <= truth) & (truth <= upper)
        ans[[i]] <- is_in_interval
    }
    ans
}


## HAS_TESTS
#' Generate values from an AR-k series
#'
#' Note that 'sd' is the marginal
#' variance of the k'th term, not the
#' variance of the innovations.
#'
#' We obtain the standard deviation
#' of the innovations, and simulate from
#' the initial values, using brute force,
#' rather than direct calculation.
#' 
#' @param n Number of values to generate
#' @param coef Numeric vector with autocorrelation
#' coefficients
#' @param sd Marginal standard deviation
#'
#' @returns A numeric vector
#'
#' @noRd
draw_vals_ar <- function(n, coef, sd) {
    n_unnorm <- 1000L
    model <- list(ar = coef)
    val_unnorm <- stats::arima.sim(model = model,
                                   n = n_unnorm)
    sd_unnorm <- sd(val_unnorm)
    sd_innov <- sd / sd_unnorm
    rand.gen <- function(n) stats::rnorm(n = n, sd = sd_innov)
    ans <- stats::arima.sim(model = model,
                            n = n,
                            rand.gen = rand.gen)
    as.numeric(ans)
}
    

## HAS_TESTS
#' Draw values for AR coefficients
#'
#' Values drawn from shifted beta distribution
#' that can be used as coefficients for AR-k model.
#' Rejected if lead to non-stationary series.
#'
#' @param p Number of terms
#' @param shape1,shape2 Parameters for beta distribution
#'
#' @returns A vector of length 'p'
#'
#' @noRd
draw_vals_ar_coef <- function(p, shape1, shape2) {
    max_attempt <- 1000L
    found_ans <- FALSE
    for (i_attempt in seq_len(max_attempt)) {
        ans <- stats::rbeta(n = p,
                            shape1 = shape1,
                            shape2 = shape2)
        ans <- 2 * ans - 1
        min_root <- min(Mod(polyroot(c(1, -ans)))) ## taken from stats::arima.sim()
        found_ans <- min_root > 1
        if (found_ans)
            break
    }
    if (!found_ans)
        cli::cli_abort("Internal error: coud not generate stationary distribution.")
    ans
}


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
    n_effect <- length(labels)
    sd_scaled <- sqrt(1 - coef^2) * sd
    ans <- matrix(nrow = n_effect,
                  ncol = n_sim,
                  dimnames = list(labels, seq_len(n_sim)))
    ans[1L, ] <- stats::rnorm(n = n_sim, sd = sd)
    for (i in seq_len(n_effect - 1L))
        ans[i + 1L, ] <- stats::rnorm(n = n_sim,
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
#' Draw values for 'effect', 'hyper',
#' optionally 'disp',
#' optionally cyclical,
#' optionally 'season',
#' and 'linpred'
#'
#' @param mod Object of class 'bage_mod'
#' @param n_sim Number of draws
#'
#' @returns Named list
#'
#' @noRd
draw_vals_hyperparam <- function(mod, n_sim) {
    has_cyclical <- has_cyclical(mod)
    has_season <- has_season(mod)
    has_disp <- has_disp(mod)
    vals_hyper <- draw_vals_hyper_mod(mod = mod,
                                      n_sim = n_sim)
    vals_effect <- draw_vals_effect_mod(mod = mod,
                                        vals_hyper = vals_hyper,
                                        n_sim = n_sim)
    vals_linpred <- make_vals_linpred_effect(mod = mod,
                                             vals_effect = vals_effect)
    if (has_disp)
        vals_disp <- draw_vals_disp(mod = mod,
                                    n_sim = n_sim)
    else
        vals_disp <- NULL
    if (has_cyclical) {
        vals_cyclical <- draw_vals_cyclical(mod = mod,
                                            n_sim = n_sim)
        vals_linpred_cyclical <- make_vals_linpred_cyclical(mod = mod,
                                                            vals_cyclical = vals_cyclical)
        vals_linpred <- vals_linpred + vals_linpred_cyclical
    }
    else
        vals_cyclical <- NULL
    if (has_season) {
        vals_season <- draw_vals_season(mod = mod,
                                        n_sim = n_sim)
        vals_linpred_season <- make_vals_linpred_season(mod = mod,
                                                        vals_season = vals_season)
        vals_linpred <- vals_linpred + vals_linpred_season
    }
    else
        vals_season <- NULL
    list(effect = vals_effect,
         hyper = vals_hyper,
         disp = vals_disp,
         cyclical = vals_cyclical,
         season = vals_season,
         linpred = vals_linpred)
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
draw_vals_effect_mod <- function(mod, vals_hyper, n_sim) {
    priors <- mod$priors
    levels_effect <- mod$levels_effect
    terms_effect <- mod$terms_effect
    levels_effect <- split(levels_effect, terms_effect)
    agesex <- make_agesex(mod)
    ans <- .mapply(draw_vals_effect,
                   dots = list(prior = priors,
                               vals_hyper = vals_hyper,
                               levels_effect = levels_effect,
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
    n_effect <- length(labels)
    n_sim <- length(sd)
    A <- make_rw_matrix(n_effect)
    sd_v <- rbind(matrix(sd,
                         nrow = n_effect - 1L,
                         ncol = n_sim,
                         byrow = TRUE),
                  sd_intercept)
    v <- matrix(stats::rnorm(n = n_effect * n_sim, sd = sd_v),
                nrow = n_effect,
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
    n_effect <- length(labels)
    n_sim <- length(sd)
    A <- make_rw2_matrix(n_effect)
    sd_v <- rbind(matrix(sd,
                         nrow = n_effect - 2L,
                         ncol = n_sim,
                         byrow = TRUE),
                  sd_intercept,
                  sd_slope)
    v <- matrix(stats::rnorm(n = n_effect * n_sim, sd = sd_v),
                nrow = n_effect,
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
#' Draw values for cyclical effects, and the
#' coef and sd hyper-parameters for cyclical effects
#'
#' @param mod Object of class 'bage_mod'
#' @param n_sim Number of draws
#'
#' @returns A named list with the cyclical effects and hyper-parameters
#'
#' @noRd
draw_vals_cyclical <- function(mod, n_sim) {
    n_time <- n_time(mod)
    n_cyclical <- mod$n_cyclical
    const <- make_const_cyclical(mod)
    shape1 <- const[[1L]]
    shape2 <- const[[2L]]
    scale <- const[[3L]]
    ans_coef <- matrix(nrow = n_cyclical, ncol = n_sim)
    ans_sd <- numeric(length = n_sim)
    ans_cyclical <- matrix(nrow = n_time, ncol = n_sim)
    for (i_sim in seq_len(n_sim)) {
        coef <- draw_vals_ar_coef(p = n_cyclical, shape1 = shape1, shape2 = shape2)
        sd <- abs(stats::rnorm(n = 1L, sd = scale))
        cyclical <- draw_vals_ar(n = n_time, coef = coef, sd = sd)
        ans_coef[, i_sim] <- coef
        ans_sd[i_sim] <- sd
        ans_cyclical[, i_sim] <- cyclical
    }
    list(coef = ans_coef,
         sd = ans_sd,
         cyclical = ans_cyclical)
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
    n_effect <- ceiling(n_time / n_season)
    const <- make_const_season(mod)
    scale <- const[[1L]]
    sd_intercept <- const[[2L]]
    A <- make_rw_matrix(n_effect)
    vals_season <- array(dim = c(n_season, n_effect, n_sim))
    vals_sd <- stats::rnorm(n = n_sim, sd = scale)
    vals_sd <- abs(vals_sd)
    sd_v <- array(dim = c(n_season, n_effect, n_sim))
    sd_v[ , -n_effect, ] <- rep(vals_sd, each = n_season * (n_effect - 1L))
    sd_v[ , n_effect, ] <- sd_intercept
    for (i in seq_len(n_season)) {
        v <- matrix(stats::rnorm(n = n_effect * n_sim, sd = sd_v[i, , ]),
                    nrow = n_effect,
                    ncol = n_sim)
        vals_season[i, , ] <- solve(A, v)
    }
    vals_season <- matrix(vals_season, ncol = n_sim)
    vals_season <- vals_season[seq_len(n_time), , drop = FALSE]
    list(season = vals_season,
         sd = vals_sd)
}


## HAS_TESTS
#' Get estimated values for hyper, effect,
#' cyclical (if present), season (if present),
#' linear predictor, and disp (if present) from a fitted model
#'
#' @param mod A fitted object of class 'bage_mod'
#'
#' @returns A named list
#'
#' @noRd
get_vals_hyperparam_est <- function(mod) {
    has_season <- has_season(mod)
    has_cyclical <- has_cyclical(mod)
    has_disp <- has_disp(mod)
    components <- components(mod)
    is_effect <- components$component == "effect"
    is_hyper <- components$component == "hyper"
    effect <- components$.fitted[is_effect]
    hyper <- components$.fitted[is_hyper]
    names(effect) <- paste0(components$term[is_effect],
                            components$level[is_effect])
    names(hyper) <- paste0(components$term[is_hyper],
                           components$level[is_hyper])
    linpred <- make_linpred_effect(mod = mod,
                                   components = components)
    if (has_disp) {
        is_disp <- components$component == "disp"
        disp <- components$.fitted[is_disp]
        names(disp) <- paste0(components$term[is_disp],
                              components$level[is_disp])
    }
    else
        disp <- NULL
    if (has_cyclical) {
        is_cyclical <- components$component == "cyclical"
        cyclical <- components$.fitted[is_cyclical]
        names(cyclical) <- paste0(components$term[is_cyclical],
                                  components$level[is_cyclical])
        linpred_cyclical <- make_linpred_cyclical(mod = mod,
                                                  components = components)
        linpred <- linpred + linpred_cyclical
    }
    else
        cyclical <- NULL
    if (has_season) {
        is_season <- components$component == "season"
        season <- components$.fitted[is_season]
        names(season) <- paste0(components$term[is_season],
                                components$level[is_season])
        linpred_season <- make_linpred_season(mod = mod,
                                              components = components)
        linpred <- linpred + linpred_season
    }
    else
        season <- NULL
    list(effect = effect,
         hyper = hyper,
         disp = disp,
         cyclical = cyclical,
         season = season,
         linpred = linpred)
}


## HAS_TESTS
#' Get all simulated values from a model for one simulation draw
#'
#' @param i_sim The index for the simulation draw
#' to be extracted
#' @param vals A named list containing simulation draws
#'
#' @returns A named list
#'
#' @noRd
get_vals_sim_one <- function(i_sim, vals) {
    f <- function(v) {
        if (is.matrix(v))
            v[ , i_sim]
        else
            v[i_sim]
    }
    ans <- rapply(vals, f, how = "replace")
    ans <- lapply(ans, unlist)
    ans
}


## HAS_TESTS
#' Check whether specifications of cyclical effects are the same
#'
#' Test via isTRUE(all.equal(x, y))
#'
#' @param mod_est,mod_sim Objects of class "bage_mod"
#'
#' @returns TRUE or FALSE
#'
#' @noRd
is_same_cyclical <- function(mod_est, mod_sim) {
    if (!isTRUE(all.equal(mod_est$n_cyclical, mod_sim$n_cyclical)))
        return(FALSE)
    if (!isTRUE(all.equal(mod_est$scale_cyclical, mod_sim$scale_cyclical)))
        return(FALSE)
    if (!isTRUE(all.equal(mod_est$matrix_cyclical_outcome, mod_sim$matrix_cyclical_outcome)))
        return(FALSE)
    TRUE
}


## HAS_TESTS
#' Check whether two named lists of priors are the same
#'
#' Test via isTRUE(all.equal(x, y))
#'
#' If data for mod_est and est is the same,
#' and 'is_same_priors' is TRUE, then mod_est
#' and mod_sim must have same main effects
#' and interactions.
#'
#' @param mod_est,mod_sim Objects of class "bage_mod"
#'
#' @returns TRUE or FALSE
#'
#' @noRd
is_same_priors <- function(mod_est, mod_sim) {
    pr_est <- mod_est$priors
    pr_sim <- mod_sim$priors
    if (length(pr_est) != length(pr_sim))
        return(FALSE)
    nms_est <- names(pr_est)
    nms_sim <- names(pr_sim)
    if (!setequal(nms_est, nms_sim))
        return(FALSE)
    for (nm in nms_est) {
        if (!isTRUE(all.equal(pr_est[[nm]], pr_sim[[nm]])))
            return(FALSE)
    }
    TRUE
}


## HAS_TESTS
#' Check whether specifications of season effects are the same
#'
#' Test via isTRUE(all.equal(x, y))
#'
#' @param mod_est,mod_sim Objects of class "bage_mod"
#'
#' @returns TRUE or FALSE
#'
#' @noRd
is_same_season <- function(mod_est, mod_sim) {
    if (!isTRUE(all.equal(mod_est$n_season, mod_sim$n_season)))
        return(FALSE)
    if (!isTRUE(all.equal(mod_est$scale_season, mod_sim$scale_season)))
        return(FALSE)
    if (!isTRUE(all.equal(mod_est$matrix_season_outcome, mod_sim$matrix_season_outcome)))
        return(FALSE)
    TRUE
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
#' Make tibble with 'component', 'term' and 'level'
#' variables to use in simulation report
#'
#' @param mod Object of class 'bage_mod'
#' @param include_upper Whether levels above 'meanpar' and 'disp'
#' included in test
#'
#' @returns A tibble with three columns
#'
#' @noRd
make_id_vars_report <- function(mod, include_upper) {
    nms_upper <- c("hyper", "effect", "cyclical", "season")
    ans <- tibble::tibble(component = character(),
                          term = character(),
                          level = character())
    if (include_upper) {
        effect <- tibble::tibble(component = "effect",
                                 term = mod$terms_effect,
                                 level = mod$levels_effect)
        hyper <- tibble::tibble(component = "hyper",
                                term = make_terms_hyper(mod),
                                level = make_levels_hyper(mod))
        ans <- vctrs::vec_rbind(ans, effect, hyper)
        if (has_cyclical(mod)) {
            cyclical <- tibble::tibble(component = "cyclical",
                                       term = make_terms_cyclical(mod),
                                       level = make_levels_cyclical(mod))
            ans <- vctrs::vec_rbind(ans, cyclical)
        }
        if (has_cyclical(mod)) {
            cyclical <- tibble::tibble(component = "cyclical",
                                       term = make_terms_cyclical(mod),
                                       level = make_levels_cyclical(mod))
            ans <- vctrs::vec_rbind(ans, cyclical)
        }
        if (has_season(mod)) {
            season <- tibble::tibble(component = "season",
                                     term = make_terms_season(mod),
                                     level = make_levels_season(mod))
            ans <- vctrs::vec_rbind(ans, season)
        }
    }
    if (has_disp(mod)) {
        disp <- tibble::tibble(component = "disp",
                               term = "disp",
                               level = "disp")
        ans <- vctrs::vec_rbind(ans, disp)
    }
    par <- tibble::tibble(component = "par",
                          term = make_term_par(mod),
                          level = as.character(seq_len(nrow(mod$data))))
    ans <- vctrs::vec_rbind(ans, par)
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
#' Transform cyclical effect to align with outcome
#'
#' @param mod Object of class 'bage_mod'
#' @param vals_cyclical List of numeric vectors
#'
#' @returns A matrix
#'
#' @noRd
make_vals_linpred_cyclical <- function(mod, vals_cyclical) {
    matrix_cyclical_outcome <- mod$matrix_cyclical_outcome
    matrix_cyclical_outcome <- as.matrix(matrix_cyclical_outcome)
    cyclical <- vals_cyclical$cyclical
    matrix_cyclical_outcome %*% cyclical
}


## HAS_TESTS
#' Combine parameter values, then transform to align with outcome
#'
#' @param mod Object of class 'bage_mod'
#' @param vals_effect List of matrices
#'
#' @returns A matrix
#'
#' @noRd
make_vals_linpred_effect <- function(mod, vals_effect) {
    matrix_effect_outcome <- make_combined_matrix_effect_outcome(mod)
    matrix_effect_outcome <- as.matrix(matrix_effect_outcome)
    vals_effect <- do.call(rbind, vals_effect)
    matrix_effect_outcome %*% vals_effect
}


## HAS_TESTS
#' Transform season effect to align with outcome
#'
#' @param mod Object of class 'bage_mod'
#' @param vals_season List of numeric vectors
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


## HAS_TESTS
#' Take output from applying 'error_point_est'
#' on multiple simulation draws, and reformat into
#' an rvec
#'
#' @param x A named list with hierarchy
#' sim > batch
#'
#' @returns An object of class rvec_dbl
#'
#' @noRd
reformat_performance_vec <- function(x) {
    n_sim <- length(x)
    x <- unlist(x, use.names = FALSE)
    x <- matrix(x, ncol = n_sim)
    rvec::rvec_dbl(x)
}


## HAS_TESTS
#' Take output from applying 'is_in_interval'
#' on multiple simulation draws,
#' and reformat into 'n_width' rvecs
#'
#' @param x A named list with hierarchy
#' sim > batch > width
#' 
#' @returns A tibble with 'n_width' rvec_lgl
#'
#' @noRd
reformat_is_in_interval <- function(x) {
    n_sim <- length(x)
    n_batch <- length(x[[1L]])
    widths <- names(x[[1L]][[1L]])
    n_width <- length(widths)
    x <- unlist(x, recursive = FALSE, use.names = FALSE)
    x <- unlist(x, recursive = FALSE, use.names = FALSE)
    x <- array(x, dim = c(n_width, n_batch, n_sim))
    x <- aperm(x, perm = c(2L, 3L, 1L))
    x <- lapply(seq_len(n_width), function(i) x[, , i])
    x <- lapply(x, unlist, use.names = FALSE)
    x <- lapply(x, matrix, ncol = n_sim)
    x <- lapply(x, rvec::rvec_lgl)
    names(x) <- paste("coverage", widths, sep = ".")
    tibble::as_tibble(x)
}


## HAS_TESTS
#' Transpose the first two layers of a list
#'
#' Transposing removes names.
#'
#' @param l A list
#'
#' @returns A list
#'
#' @noRd
transpose_list <- function(l) {
    elements <- unlist(l, recursive = FALSE, use.names = FALSE)
    m <- matrix(elements, ncol = length(l))
    m <- t(m)
    apply(m, 2L, identity, simplify = FALSE)
}


## HAS_TESTS
#' Simulation study of a model
#'
#' Use simulated data to assess the performance of
#' and estimated model.
#'
#' @section Comparisons included in report:
#'
#' When `mod_est` and `mod_sim` are identical,
#' the report produced by `report_sim()` has the
#' following components:
#'
#' - `"hyper"` Hyper-parameters from priors for intercept,
#'   main effects, and interactions.
#' - `"effect"` Intercept, main effects, and interactions.
#' - `"cyclical"` Parameters and hyper-parameters for
#'   cyclical effect, if included in model.
#' - `"season"` Parameters and hyper-parameters for
#'   seasonal effect, if included in model.
#' - `"meanpar"` Expected value for rates probabilities.
#'   Poisson and binomial models only.
#' - `"disp"` Dispersion term, if included in model.
#' - `"par"` Rates, probabilities, or means.
#'
#' When `mod_est` and `mod_sim` differ, the
#' report omits `"hyper"`, `"effect"`, `"cyclical"`,
#' and `"season"` since, for most model specifications,
#' these terms are not comparable across models.
#'
#' @param mod_est The model whose performance is being
#' assessed. An object of class `bage_mod`.
#' @param mod_sim The model used to generate the simulated
#' data. If no value is supplied, `mod_est` is used.
#' @param n_sim Number of sets of simulated data to use.
#' Default is 100.
#' @param point_est_fun Name of the function to use
#' to calculate point estimates. The options are `"mean"`
#' and `"median"`. The default is `"mean"`.
#' @param widths Widths of credible intervals.
#' A vector of values in the interval `(0, 1]`.
#' Default is `c(0.5, 0.95)`.
#' @param report_type Amount of detail in return value.
#' Options are `"short"` and `"long"`. Default is `"short"`.
#' @param n_core Number of cores to use for parallel
#' processing. If no value supplied, then no parallel
#' processing is done. CURRENTLY UNUSED.
#'
#' @return
#' **`report_type` is `"short"`**
#' A tibble with the following columns:
#' - `component`. Part of model. See Details.
#' `"par"` is the rate, probability, or mean
#' parameter from the likelihood.
#' - `vals_sim`. Simulated value for parameter, averaged
#' across all simulations and cells.
#' - `error_point_est`. Point estimate minus simulation-true
#' value, averaged across all simulations and cells.
#' - `coverage`. Actual proportion of simulation-true values
#' that fall within each type of interval, averaged across all
#' simulations and cells.
#'
#' **`report_type` is `"long"`**
#' A tibble with the following columns:
#' - `component`. Part of model. See [components()].
#' `"par"` is the rate, probability,
#' or mean parameter from the likelihood.
#' - `term`. Category within `component`.
#' - `level`. Category within `term`.
#' - `vals_sim`. Simulated values for parameter,
#' stored in an [rvec][rvec::rvec()].
#' - `error_point_est`. Point estimates minus simulation-true
#' values, stored in an [rvec][rvec::rvec()].
#' - `coverage`. Actual proportions of simulation-true values
#' falling within each type of interval, stored in
#' an [rvec][rvec::rvec()].
#' 
#' @seealso
#' - [mod_pois()], [mod_binom()], [mod_norm()] to set up
#' models
#'
#' @examples
#' ## results random, so set seed
#' set.seed(0)
#'
#' ## make data - outcome variable (deaths here)
#' ## needs to be present, but is not used
#' data <- data.frame(region = c("A", "B", "C", "D", "E"),
#'                    population = c(100, 200, 300, 400, 500),
#'                    deaths = NA)
#'
#' ## simulation with estimation model same as
#' ## data-generating model
#' mod_est <- mod_pois(deaths ~ region,
#'                     data = data,
#'                     exposure = population) |>
#'   set_prior(`(Intercept)` ~ Known(0))
#' report_sim(mod_est = mod_est,
#'            n_sim = 10) ## in practice should use larger value
#' 
#' ## simulation with estimation model different
#' ## from data-generating model
#' mod_sim <- mod_est |>
#'   set_prior(region ~ N(s = 2))
#' report_sim(mod_est = mod_est,
#'            mod_sim = mod_sim,
#'            n_sim = 10)
#' @export
report_sim <- function(mod_est,
                       mod_sim = NULL,
                       n_sim = 100,
                       point_est_fun = c("median", "mean"),
                       widths = c(0.5, 0.95),
                       report_type = c("short", "long"),
                       n_core = NULL) {
    if (!inherits(mod_est, "bage_mod"))
        cli::cli_abort(c("{.arg mod_est} is not an object of class {.cls bage_mod}.",
                         i = "{.arg mod_est} has class {.cls {class(mod_est)}}."))
    check_n(n = n_sim, n_arg = "n_sim", min = 1L, max = NULL, null_ok = FALSE)
    point_est_fun <- match.arg(point_est_fun)
    check_widths(widths)
    report_type <- match.arg(report_type)
    if (is.null(mod_sim))
        mod_sim <- mod_est
    else
        check_mod_est_est_compatible(mod_est = mod_est,
                                     mod_sim = mod_sim)
    is_same_priors <- is_same_priors(mod_est = mod_est,
                                     mod_sim = mod_sim)
    is_same_cyclical <- is_same_cyclical(mod_est = mod_est,
                                         mod_sim = mod_sim)
    is_same_season <- is_same_season(mod_est = mod_est,
                                     mod_sim = mod_sim)
    include_upper <- is_same_priors && is_same_cyclical && is_same_season
    vals_sim_all <- draw_vals_mod(mod = mod_sim,
                                  n_sim = n_sim)
    vals_sim_all <- lapply(seq_len(n_sim), get_vals_sim_one, vals_sim_all)
    performance <- lapply(vals_sim_all,
                          assess_performance,
                          mod_est = mod_est,
                          point_est_fun = point_est_fun,
                          include_upper = include_upper,
                          widths = widths)
    performance <- transpose_list(performance)
    vals_sim <- performance[[1L]]
    error_point_est <- performance[[2L]]
    is_in_interval <- performance[[3L]]
    vals_sim <- reformat_performance_vec(vals_sim)
    error_point_est <- reformat_performance_vec(error_point_est)
    is_in_interval <- reformat_is_in_interval(is_in_interval)
    id_vars <- make_id_vars_report(mod = mod_est,
                                   include_upper = include_upper)
    ans <- tibble::tibble(id_vars,
                          vals_sim = vals_sim,
                          error_point_est = error_point_est,
                          is_in_interval)
    if (report_type == "short")
        summarise_sim(ans)
    else if (report_type == "long")
        ans
    else
        cli::cli_abort("Internal error: Invalid value for 'report_type'.")
}


## HAS_TESTS
#' Summarise detailed output from simulation
#'
#' Summaries are means across cells and across simulations
#'
#' @param data Tibble with output from simulation
#'
#' @returns A tibble
#'
#' @noRd
summarise_sim <- function(data) {
    x <- data[setdiff(names(data), c("component", "term", "level"))]
    f <- factor(data$component, levels = unique(data$component))
    data <- split(x = x, f = f)
    summarise_one_chunk <- function(y)
        vapply(y, function(z) mean(as.numeric(z)), 0)
    ans <- lapply(data, summarise_one_chunk)
    ans <- do.call(rbind, ans)
    ans <- data.frame(component = names(data), ans)
    ans <- tibble::tibble(ans)
    ans
}
