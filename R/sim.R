
## HAS_TESTS
#' Calculate point estimates from rvecs, and then calculate
#' differences from truth
#'
#' @param estimate A list of rvecs holding the estimates
#' @param truth A list of numeric vectors holding the true values
#' @param point_est_fun Name of function to use to calculate point
#' estimates from rvec
#' @param include_priors Whether to include 'hyper' and 'par'
#' in calculations
#'
#' @returns A vector of doubles
#'
#' @noRd
calc_error_point_est <- function(estimate, truth, point_est_fun, include_priors) {
    if (point_est_fun == "mean")
        rvec_fun <- rvec::draws_mean
    else if (point_est_fun == "median")
        rvec_fun <- rvec::draws_median
    else
        cli::cli_abort("Internal error: Invalid value for 'point_est_fun'.")
    if (!include_priors) {
        is_prior_est <- names(estimate) %in% c("hyper", "par")
        is_prior_tr <- names(truth) %in% c("hyper", "par")
        estimate <- estimate[!is_prior_est]
        truth <- truth[!is_prior_tr]
    }
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
#' @param include_priors Whether to include 'hyper' and 'par'
#' in calculations
#'
#' @returns A list of logical vectors
#'
#' @noRd
calc_is_in_interval <- function(estimate, truth, widths, include_priors) {
    if (!include_priors) {
        is_prior_est <- names(estimate) %in% c("hyper", "par")
        is_prior_tr <- names(truth) %in% c("hyper", "par")
        estimate <- estimate[!is_prior_est]
        truth <- truth[!is_prior_tr]
    }
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
    ans[1L, ] <- stats::rnorm(n = n_sim, sd = sd)
    for (i in seq_len(n_par - 1L))
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


#' Draw vales for 'hyper', 'par', 'linpred', and, optionally,
#' for 'season' and 'disp'
#'
#' @param mod Object of class 'bage_mod'
#' @param n_sim Number of draws
#'
#' @returns Named list
#'
#' @noRd
draw_vals_hyperparam <- function(mod, n_sim) {
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
    if (has_disp)
        vals_disp <- draw_vals_disp(mod = mod,
                                    n_sim = n_sim)
    else
        vals_disp <- NULL
    list(hyper = vals_hyper,
         par = vals_par,
         season = vals_season,
         linpred = vals_linpred,
         disp = vals_disp)
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
    terms_par <- mod$terms_par
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
#' Get estimated values for hyper, par, season (if present),
#' linear predictor, and disp (if present) from a fitted model
#'
#' @param mod A fitted object of class 'bage_mod'
#'
#' @returns A named list
#'
#' @noRd
get_vals_hyperparam_est <- function(mod) {
    has_season <- has_season(mod)
    has_disp <- has_disp(mod)
    components <- components(mod)
    is_hyper <- components$component == "hyper"
    is_par <- components$component == "par"
    hyper <- components$.fitted[is_hyper]
    par <- components$.fitted[is_par]
    linpred <- make_linpred_par(mod = mod,
                                components = components)
    if (has_season) {
        is_season <- components$component == "season"
        season <- components$.fitted[is_season]
        linpred_season <- make_linpred_season(mod = mod,
                                              components = components)
        linpred <- linpred + linpred_season
    }
    else
        season <- NULL
    if (has_disp) {
        is_disp <- components$component == "disp"
        disp <- components$.fitted[is_disp]
    }
    else
        disp <- NULL
    list(hyper = hyper,
         par = par,
         season = season,
         linpred = linpred,
         disp = disp)
}


## HAS_TESTS
#' Get all simulated values from a model for one simulation draw
#'
#' @param x A named list containing simulation draws
#' @param i_sim The index for the simulation draw
#' to be extracted
#'
#' @returns A named list
#'
#' @noRd
get_vals_sim_one <- function(x, i_sim) {
    f <- function(v) {
        if (is.matrix(v))
            v[ , i_sim, drop = FALSE]
        else
            v[i_sim]
    }
    rapply(x, f, how = "replace")
}


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


report_sim <- function(mod_est,
                       mod_sim = NULL,
                       n_sim = 100,
                       point_est_fun = c("median", "mean"),
                       widths = c(0.5, 0.95)) {
    if (!inherits(mod_est, "bage_mod"))
        cli::cli_abort(c("{.arg mod_est} is not an object of class {.cls bage_mod}.",
                         i = "{.arg mod_est} has class {.cls {class(mod_est)}}."))
    check_n(n = n_sim, n_arg = "n_sim", min = 1L, max = NULL, null_ok = FALSE)
    point_est_fun <- match.arg(point_est_fun)
    check_widths(widths)
    if (is.null(mod_sim))
        mod_sim <- mod_est
    else
        check_mod_est_est_compatible(mod_est = mod_est,
                                     mod_sim = mod_sim)
    is_same_priors <- is_same_priors(mod_est = mod_est,
                                     mod_sim = mod_sim)
    vals_sim_all <- draw_vals_mod(mod = mod_sim,
                                  n_sim = n_sim)
    error_point_est <- vector(mode = "list", length = n_sim)
    is_in_interval <- vector(mode = "list", length = n_sim)
    for (i_sim in seq_len(n_sim)) {
        vals_sim <- get_vals_sim_one(vals_sim_all, i_sim = i_sim)
        mod[["outcome"]] <- vals_sim[["outcome"]]
        mod_est <- fit(mod_est)
        vals_est <- get_vals_est(mod_est)
        error_point_est[[i_sim]] <- calc_error_point_est(estimate = vals_est,
                                                         truth = vals_sim,
                                                         point_est_fun = point_est_fun,
                                                         include_priors = is_same_priors)
        is_in_interval[[i_sim]] <- calc_is_in_interval(estimate = vals_est,
                                                       truth = vals_sim,
                                                       widths = widths,
                                                       include_priors = is_same_priors)
    }
    make_report_sim(mod = mod,
                    error_point_est = error_point_est,
                    is_in_interval = is_in_interval)
}


    
                             
    
