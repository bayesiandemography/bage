
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
#' effect, hyper
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
    nms_upper <- c("hyper", "effect")
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
    width_interval <- calc_interval_width(estimate = vals_est,
                                          widths = widths)
    is_null <- vapply(vals_sim, is.null, FALSE)
    vals_sim <- vals_sim[!is_null]
    list(vals_sim = vals_sim,
         error_point_est = error_point_est,
         is_in_interval = is_in_interval,
         width_interval = width_interval)
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
#' Given a sets of estimates,
#' for each set, calculate the widths
#' of the credible intervals
#'
#' @param estimate A list of rvecs holding the estimates
#' @param widths Widths of intervals (between 0 and 1)
#'
#' @returns A list of logical vectors
#'
#' @noRd
calc_interval_width <- function(estimate, widths) {
    is_null <- vapply(estimate, is.null, FALSE)
    estimate <- estimate[!is_null]
    ans <- .mapply(calc_interval_width_one,
                   dots = list(estimate = estimate),
                   MoreArgs = list(widths = widths))
    names(ans) <- names(estimate)
    ans
}

## HAS_TESTS
#' Calculates widths of credible intervals
#'
#' @param estimate An rvec holding estimates
#' @param widths Widths of intervals (between 0 and 1)
#'
#' @returns A logical vector
#'
#' @noRd
calc_interval_width_one <- function(estimate, widths) {
    n_widths <- length(widths)
    ans <- vector(mode = "list", length = n_widths)
    names(ans) <- widths
    for (i in seq_len(n_widths)) {
        width <- widths[[i]]
        probs <- c(0.5 - width / 2, 0.5 + width / 2)
        ci <- rvec::draws_quantile(x = estimate, probs = probs)
        lower <- ci[[1L]]
        upper <- ci[[2L]]
        width <- upper - lower
        ans[[i]] <- width
    }
    ans
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


#' Generate Values for Multiple AR-k Series of Length 'n'
#'
#' @param n Length of series (not order of series)
#' @param coef Matrix, each column of which is a vector
#' of autocorellation coefficients
#' @param sd Vector, each element of which is a
#' marginal standard deviation
#'
#' @returns A matrix with 'n' rows and 'length(sd)' columns
#'
#' @noRd
draw_vals_ar <- function(n, coef, sd) {
  n_sim <- length(sd)
  ans <- matrix(nrow = n, ncol = n_sim)
  for (i_sim in seq_len(n_sim))
    ans[, i_sim] <- draw_vals_ar_one(n = n,
                                     coef = coef[, i_sim],
                                     sd = sd[i_sim])
  ans
}


## HAS_TESTS
#' Generate Values for a Single AR-k Series of Length 'n'
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
#' @param n Length of series (not order of series)
#' @param coef Numeric vector with autocorrelation
#' coefficients
#' @param sd Marginal standard deviation
#'
#' @returns A numeric vector
#'
#' @noRd
draw_vals_ar_one <- function(n, coef, sd) {
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
    

## ## HAS_TESTS
## #' Draw Values that Would be Produced by a Call to 'augment'
## #'
## #' @param mod Object of class 'bage_mod'
## #' @param n_sim Number of draws
## #'
## #' @returns Named list
## #'
## #' @noRd
## draw_vals_augment <- function(mod, vals_components, n_sim) {
##   inv_transform <- get_fun_inv_transform(mod)
##   align_to_data <- get_fun_align_to_data(mod)
##   transform <- function(mod)
##     align_to_data(inv_transform(mod))
##   ## extract quantities needed in calculations
##   has_disp <- has_disp(mod)
##   linpred <- make_linpred_effect(mod = mod,
##                                  components = components)
##   if (has_disp) {
##     expected <- transform(linpred)
##     is_disp <- components$component == "disp"
##     disp <- components$.fitted[is_disp]
##     fitted <- make_par_disp(mod = mod,
##                             meanpar = expected,
##                             disp = disp)
##   }
##   else
##     fitted <- transform(linpred)
##   ans$.fitted <- fitted
##   if (has_disp)
##     ans$.expected <- expected
##   ans
## }


##   priors <- mod$priors
##   nms_priors <- names(priors)
##   has_disp <- has_disp(mod)
##   vals_hyper <- draw_vals_hyper_mod(mod = mod,
##                                     n_sim = n_sim)
##   vals_hyperrand <- draw_vals_hyperrand_mod(mod = mod,
##                                             vals_hyper = vals_hyper,
##                                             n_sim = n_sim)
##   vals_effect <- draw_vals_effect_mod(mod = mod,
##                                       vals_hyper = vals_hyper,
##                                       vals_hyperrand = vals_hyperrand,
##                                       n_sim = n_sim)
##   vals_effect <- standardize_vals_effect(mod = mod,
##                                          vals_effect = vals_effect)
##   vals_hyper <- .mapply(vals_hyper_to_dataframe,
##                         dots = list(prior = priors,
##                                     nm_prior = nms_priors,
##                                     vals_hyper = vals_hyper),
##                         MoreArgs = list(n_sim = n_sim))
##   vals_hyper <- vctrs::vec_rbind(!!!vals_hyper)
##   vals_hyperrand <- .mapply(vals_hyperrand_to_dataframe,
##                             dots = list(prior = priors,
##                                         nm_prior = nms_priors,
##                                         vals_hyperrand = vals_hyperrand),
##                             MoreArgs = list(n_sim = n_sim))
##   vals_hyperrand <- vctrs::vec_rbind(!!!vals_hyperrand)
##   vals_effect <- vals_effect_to_dataframe(vals_effect)
##   ans <- vctrs::vec_rbind(vals_hyper, vals_hyperrand, vals_effect)
##   if (has_disp) {
##     vals_disp <- draw_vals_disp(mod = mod,
##                                 n_sim = n_sim)
##     vals_disp <- vals_disp_to_dataframe(vals_disp)
##     ans <- vctrs::vec_rbind(ans, vals_disp)
##   }
##   ans    
## }


## HAS_TESTS
#' Draw values for AR coefficients
#'
#' Values drawn from shifted beta distribution
#' that can be used as coefficients for AR-k model.
#' Rejected if lead to non-stationary series.
#'
#' @param n Number of terms
#' @param shape1,shape2 Parameters for beta distribution
#' @param min,max Limits on parameter values
#'
#' @returns A vector of length 'n'
#'
#' @noRd
draw_vals_coef <- function(prior, n_sim) {
  max_attempt <- 1000L
  specific <- prior$specific
  n <- specific$n
  shape1 <- specific$shape1
  shape2 <- specific$shape2
  min <- specific$min
  max <- specific$max
  ans <- matrix(nrow = n, ncol = n_sim)
  for (i_sim in seq_len(n_sim)) {
    found_val <- FALSE
    for (i_attempt in seq_len(max_attempt)) {
      val_raw <- stats::rbeta(n = n,
                              shape1 = shape1,
                              shape2 = shape2)
      val <- min + (max - min) * val_raw
      min_root <- min(Mod(polyroot(c(1, -val)))) ## taken from stats::arima.sim()
      found_val <- min_root > 1
      if (found_val)
        break
    }
    if (!found_val)
      cli::cli_abort("Internal error: coud not generate stationary distribution.")
    ans[, i_sim] <- val
  }
  rownames(ans) <- paste0("coef", seq_len(n))
  ans
}


## HAS_TESTS
#' Draw Values that Would be Produced by a Call to 'components'
#'
#' @param mod Object of class 'bage_mod'
#' @param n_sim Number of draws
#'
#' @returns Named list
#'
#' @noRd
draw_vals_components <- function(mod, n_sim) {
  priors <- mod$priors
  nms_priors <- names(priors)
  has_disp <- has_disp(mod)
  vals_hyper <- draw_vals_hyper_mod(mod = mod,
                                    n_sim = n_sim)
  vals_hyperrand <- draw_vals_hyperrand_mod(mod = mod,
                                            vals_hyper = vals_hyper,
                                            n_sim = n_sim)
  vals_effect <- draw_vals_effect_mod(mod = mod,
                                      vals_hyper = vals_hyper,
                                      vals_hyperrand = vals_hyperrand,
                                      n_sim = n_sim)
  vals_effect <- standardize_vals_effect(mod = mod,
                                         vals_effect = vals_effect)
  vals_hyper <- .mapply(vals_hyper_to_dataframe,
                        dots = list(prior = priors,
                                    nm_prior = nms_priors,
                                    vals_hyper = vals_hyper),
                        MoreArgs = list(n_sim = n_sim))
  vals_hyper <- vctrs::vec_rbind(!!!vals_hyper)
  vals_hyperrand <- .mapply(vals_hyperrand_to_dataframe,
                            dots = list(prior = priors,
                                        nm_prior = nms_priors,
                                        vals_hyperrand = vals_hyperrand),
                            MoreArgs = list(n_sim = n_sim))
  vals_hyperrand <- vctrs::vec_rbind(!!!vals_hyperrand)
  vals_effect <- vals_effect_to_dataframe(vals_effect)
  ans <- vctrs::vec_rbind(vals_hyper, vals_hyperrand, vals_effect)
  if (has_disp) {
    vals_disp <- draw_vals_disp(mod = mod,
                                n_sim = n_sim)
    vals_disp <- vals_disp_to_dataframe(vals_disp)
    ans <- vctrs::vec_rbind(ans, vals_disp)
  }
  ans    
}


## HAS_TESTS
#' Draw Values for all Effects
#'
#' Draw values for intercept, main effects, interactions.
#'
#' @param mod Object of class "bage_mod"
#' @param vals_hyper List of lists.
#' @param vals_hyperrand List of lists.
#' @param n_sim Number of draws
#'
#' @returns A named list of matrices.
#'
#' @noRd
draw_vals_effect_mod <- function(mod, vals_hyper, vals_hyperrand, n_sim) {
    priors <- mod$priors
    levels_effect <- mod$levels_effect
    terms_effect <- mod$terms_effect
    levels_effect <- split(levels_effect, terms_effect)
    agesex <- make_agesex(mod)
    matrices_along_by <- choose_matrices_along_by(mod)
    ans <- .mapply(draw_vals_effect,
                   dots = list(prior = priors,
                               vals_hyper = vals_hyper,
                               vals_hyperrand = vals_hyperrand,
                               levels_effect = levels_effect,
                               agesex = agesex,
                               matrix_along_by = matrices_along_by),
                   MoreArgs = list(n_sim = n_sim))
    names(ans) <- names(priors)
    ans
}


## HAS_TESTS
#' Generate Draws from ELin
#'
#' Each column is one draw.
#'
#' @param mslope Matrix of values
#' @param sd Vector of values
#' @param matrix_along_by Matrix with map for along and by dimensions
#' @param labels Names of elements
#'
#' @returns A matrix, with dimnames.
#'
#' @noRd
draw_vals_elin <- function(mslope, sd, matrix_along_by, labels) {
  n_sim <- ncol(mslope)
  n_along <- nrow(matrix_along_by)
  n_by <- ncol(matrix_along_by)
  q <- seq(from = -1, to = 1, length.out = n_along)
  q <- rep(q, times = n_by * n_sim)
  mslope <- rep(mslope, each = n_along)
  sd <- rep(sd, each = n_along * n_by)
  ans <- stats::rnorm(n = n_along * n_by * n_sim,
                      mean = q * mslope,
                      sd = sd)
  ans <- matrix(ans,
                nrow = n_along * n_by,
                ncol = n_sim)
  i <- match(sort(matrix_along_by), matrix_along_by)
  ans <- ans[i, , drop = FALSE]
  dimnames(ans) <- list(labels, seq_len(n_sim))
  ans
}


## HAS_TESTS
#' Generate Draws from ERW
#'
#' Each column is one draw.
#'
#' @param sd Vector of values
#' @param matrix_along_by Matrix with map for along and by dimensions
#' @param labels Names of elements
#'
#' @returns A matrix, with dimnames.
#'
#' @noRd
draw_vals_erw <- function(sd, matrix_along_by, labels) {
  n_sim <- length(sd)
  n_along <- nrow(matrix_along_by)
  n_by <- ncol(matrix_along_by)
  ans <- matrix(nrow = n_along, ncol = n_by * n_sim)
  ans[1L, ] <- stats::rnorm(n = n_by * n_sim)
  sd <- rep(sd, each = n_by)
  for (i_along in seq.int(from = 2L, to = n_along))
    ans[i_along, ] <- stats::rnorm(n = n_by * n_sim,
                                   mean = ans[i_along - 1L, ],
                                   sd = sd)
  ans <- matrix(ans,
                nrow = n_along * n_by,
                ncol = n_sim)
  i <- match(sort(matrix_along_by), matrix_along_by)
  ans <- ans[i, , drop = FALSE]
  dimnames(ans) <- list(labels, seq_len(n_sim))
  ans
}


## HAS_TESTS
#' Generate Draws from ESeas
#'
#' Each column is one draw.
#'
#' @param n Number of seasons
#' @param sd Vector of values
#' @param matrix_along_by Matrix with map for along and by dimensions
#' @param labels Names of elements
#'
#' @returns A matrix, with dimnames.
#'
#' @noRd
draw_vals_eseas <- function(n, sd, matrix_along_by, labels) {
  n_sim <- length(sd)
  n_along <- nrow(matrix_along_by)
  n_by <- ncol(matrix_along_by)
  ans <- matrix(nrow = n_along, ncol = n_by * n_sim)
  ans[seq_len(n), ] <- stats::rnorm(n = n * n_by * n_sim)
  sd <- rep(sd, each = n_by)
  for (i_along in seq.int(from = n + 1L, to = n_along))
    ans[i_along, ] <- stats::rnorm(n = n_by * n_sim,
                                   mean = ans[i_along - n, ],
                                   sd = sd)
  ans <- matrix(ans,
                nrow = n_along * n_by,
                ncol = n_sim)
  i <- match(sort(matrix_along_by), matrix_along_by)
  ans <- ans[i, , drop = FALSE]
  dimnames(ans) <- list(labels, seq_len(n_sim))
  ans
}


## HAS_TESTS
#' Draw Values for Ordinary Hyper-Parameters
#'
#' @param mod Object of class "bage_mod"
#' @param n_sim Number of draws
#'
#' @returns A named list
#'
#' @noRd
draw_vals_hyper_mod <- function(mod, n_sim) {
  priors <- mod$priors
  lapply(priors, draw_vals_hyper, n_sim = n_sim)
}


## HAS_TESTS
#' Draw Values for Hyper-Parameters that can be
#' Treated as Random Effects
#'
#' @param mod Object of class "bage_mod"
#' @param n_sim Number of draws
#'
#' @returns A named list
#'
#' @noRd
draw_vals_hyperrand_mod <- function(mod, vals_hyper, n_sim) {
  priors <- mod$priors
  levels_effect <- mod$levels_effect
  terms_effect <- mod$terms_effect
  levels_effect <- split(levels_effect, terms_effect)
  agesex <- make_agesex(mod)
  matrices_along_by <- choose_matrices_along_by(mod)
  ans <- .mapply(draw_vals_hyperrand,
                 dots = list(prior = priors,
                             vals_hyper = vals_hyper,
                             levels_effect = levels_effect,
                             agesex = agesex,
                             matrix_along_by = matrices_along_by),
                 MoreArgs = list(n_sim = n_sim))
  names(ans) <- names(priors)
  ans
}


## HAS_TESTS
#' Generate Draws from Lin
#'
#' Each column is one draw.
#'
#' @param slope Vector of values
#' @param sd Vector of values
#' @param labels Names of elements
#'
#' @returns A matrix, with dimnames.
#'
#' @noRd
draw_vals_lin <- function(slope, sd, labels) {
  n_effect <- length(labels)
  n_sim <- length(slope)
  q <- seq(from = -1, to = 1, length.out = n_effect)
  mean <- outer(q, slope)
  error <- stats::rnorm(n = n_effect * n_sim,
                        sd = rep(sd, each = n_effect))
  ans <- mean + error
  dimnames(ans) <- list(labels, seq_len(n_sim))
  ans
}


## HAS_TESTS
#' Draw Values for the 'msd' Hyper-Parameters of a Prior
#'
#' @param prior An object of class 'bage_prior'
#' @param n_sim Number of draws
#'
#' @returns A numeric vector
#'
#' @noRd
draw_vals_msd <- function(prior, n_sim) {
  mscale <- prior$specific$mscale
  abs(stats::rnorm(n = n_sim, sd = mscale))
}


## HAS_TESTS
#' Draw Values for the 'mslope' Hyper-Parameters of a Prior
#'
#' @param prior An object of class 'bage_prior'
#' @param slope Vector of mean slopes
#' @param msd Vector of standard deviations
#' @param matrix_along_by Matrix with mapping for along and by dimensions
#' @param n_sim Number of draws
#'
#' @returns A matrix
#'
#' @noRd
draw_vals_mslope <- function(slope, msd, matrix_along_by, n_sim) {
  n_by <- ncol(matrix_along_by)
  ans <- stats::rnorm(n = n_by * n_sim,
                      mean = rep(slope, each = n_by),
                      sd = rep(msd, each = n_by))
  ans <- matrix(ans, nrow = n_by, ncol = n_sim)
  nms_by <- colnames(matrix_along_by)
  rownames <- paste("mslope", nms_by, sep = ".")
  rownames(ans) <- rownames
  ans
}


## HAS_TESTS
#' Generate Draws from RW 
#'
#' Each column is one draw.
#'
#' @param sd Vector of values
#' @param labels Names of elements
#'
#' @returns A matrix, with dimnames.
#'
#' @noRd
draw_vals_rw <- function(sd, labels) {
  n_effect <- length(labels)
  n_sim <- length(sd)
  ans <- matrix(nrow = n_effect,
                ncol = n_sim,
                dimnames = list(labels, seq_len(n_sim)))
  ans[1L, ] <- stats::rnorm(n = n_sim)
  for (i_effect in seq.int(from = 2L, to = n_effect))
    ans[i_effect, ] <- stats::rnorm(n = n_sim,
                                    mean = ans[i_effect - 1L, ],
                                    sd = sd)
  ans
}


## HAS_TESTS
#' Generate Draws from RW2
#'
#' Each column is one draw.
#'
#' @param sd Vector of values
#' @param sd_slope Scalar
#' @param labels Names of elements
#'
#' @returns A matrix, with dimnames.
#'
#' @noRd
draw_vals_rw2 <- function(sd, sd_slope, labels) {
  n_effect <- length(labels)
  n_sim <- length(sd)
  ans <- matrix(nrow = n_effect,
                ncol = n_sim,
                dimnames = list(labels, seq_len(n_sim)))
  ans[1L, ] <- stats::rnorm(n = n_sim)
  ans[2L, ] <- stats::rnorm(n = n_sim,
                            mean = ans[1L, ],
                            sd = sd_slope)
  for (i_effect in seq.int(from = 3L, to = n_effect))
    ans[i_effect, ] <- stats::rnorm(n = n_sim,
                                    mean = 2 * ans[i_effect - 1L, ] - ans[i_effect - 2L, ],
                                    sd = sd)
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
#' Generate Draws from Seas
#'
#' Each column is one draw.
#'
#' @param n Number of seasons
#' @param sd Vector of values
#' @param labels Names of elements
#'
#' @returns A matrix, with dimnames.
#'
#' @noRd
draw_vals_seas <- function(n, sd, labels) {
  n_effect <- length(labels)
  n_sim <- length(sd)
  ans <- matrix(nrow = n_effect,
                ncol = n_sim,
                dimnames = list(labels, seq_len(n_sim)))
  ans[seq_len(n), ] <- stats::rnorm(n = n_sim)
  for (i_effect in seq.int(from = n + 1L, to = n_effect))
    ans[i_effect, ] <- stats::rnorm(n = n_sim,
                                    mean = ans[i_effect - n, ],
                                    sd = sd)
  ans
}

## HAS_TESTS
#' Draw Values for the 'slope' Parameter of a Prior
#'
#' @param prior An object of class 'bage_prior'
#' @param n_sim Number of draws
#'
#' @returns A numeric vector
#'
#' @noRd
draw_vals_slope <- function(prior, n_sim) {
    sd_slope <- prior$specific$sd_slope
    stats::rnorm(n = n_sim, sd = sd_slope)
}


## HAS_TESTS
#' Get estimated values for hyper, effect,
#' linear predictor, and disp (if present) from a fitted model
#'
#' @param mod A fitted object of class 'bage_mod'
#'
#' @returns A named list
#'
#' @noRd
get_vals_hyperparam_est <- function(mod) {
  nms <- c("effect",
           "hyper",
           "trend", "cyclical", "seasonal", "error",
           "disp")
  components <- components(mod)
  ans <- list()
  for (nm in nms) {
    is_nm <- components$component == nm
    if (any(is_nm)) {
      vals <- components$.fitted[is_nm]
      names(vals) <- paste(components$term[is_nm],
                           components$level[is_nm],
                           sep = ".")
    }
    else {
      vals <- NULL
    }
    vals <- list(vals)
    names(vals) <- nm
    ans <- c(ans, vals)
  }
  linpred <- make_linpred_effect(mod = mod,
                                 components = components)
  linpred <- list(linpred)
  names(linpred) <- "linpred"
  ans <- c(ans, linpred)
  ans
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
    nms_upper <- c("hyper", "effect")
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
#' @returns A tibble with 'n_width' rvecs
#'
#' @noRd
reformat_interval <- function(x, nm) {
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
    x <- lapply(x, rvec::rvec)
    names(x) <- paste(nm, widths, sep = ".")
    tibble::as_tibble(x)
}


## HAS_TESTS
#' Standardize Effects
#'
#' Apply same standardization to simulated effects
#' that is applied to estimated effects.
#'
#' @param mod Object of class 'bage_mod'
#' @param vals_effect A list of tibbles.
#'
#' @returns A modified version of 'vals_effect'
#'
#' @noRd
standardize_vals_effect <- function(mod, vals_effect) {
  matrices_effect_outcome <- mod$matrices_effect_outcome
  matrices_effect_outcome <- lapply(matrices_effect_outcome, Matrix::as.matrix)
  matrix_effect_outcome <- Reduce(cbind, matrices_effect_outcome)
  effects <- do.call(rbind, vals_effect)
  linpred <- matrix_effect_outcome %*% effects
  n_effect <- length(vals_effect)
  ans <- vector(mode = "list", length = n_effect)
  for (i_effect in seq_len(n_effect)) {
    M <- matrices_effect_outcome[[i_effect]]
    n <- colSums(M)
    effect <- crossprod(M, linpred) / n
    linpred <- linpred - M %*% effect
    ans[[i_effect]] <- effect
  }
  if (any(abs(linpred) > 0.001))
    cli::cli_abort("Internal error: Final residual not 0")
  names(ans) <- names(vals_effect)
  ans
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
#' - `"meanpar"` Expected value for rates probabilities.
#'   Poisson and binomial models only.
#' - `"disp"` Dispersion term, if included in model.
#' - `"par"` Rates, probabilities, or means.
#'
#' When `mod_est` and `mod_sim` differ, the
#' report omits `"hyper"`, and `"effect"`, 
#' since, for most model specifications,
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
#' processing. If `n_core` is `1` (the default),
#' no parallel processing is done.
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
                       n_core = 1) {
    check_bage_mod(x = mod_est, nm_x = "mod_est")
    check_n(n = n_sim,
            nm_n = "n_sim",
            min = 1L,
            max = NULL,
            null_ok = FALSE)
    point_est_fun <- match.arg(point_est_fun)
    check_widths(widths)
    report_type <- match.arg(report_type)
    check_n(n = n_core,
            nm_n = "n_core",
            min = 1L,
            max = NULL,
            null_ok = FALSE)
    vals_components <- draw_vals_components(mod = mod,
                                            n_sim = n_sim)
    vals_augment <- draw_vals_augment(mod = mod,
                                      val_components = vals_components,
                                      n_sim = n_sim)
    if (n_core > 1L) {
        iseed <- make_seed()
        cl <- parallel::makeCluster(n_core)
        on.exit(parallel::stopCluster(cl))
        parallel::clusterSetRNGStream(cl, iseed = iseed)
        assess_performance_inner <- function(vals_sim)
            assess_performance(vals_sim,
                               mod_est = mod_est,
                               point_est_fun = point_est_fun,
                               include_upper = is_comparable_mod,
                               widths = widths)
        performance <- parallel::parLapply(cl = cl,
                                           X = vals_sim_all,
                                           fun = assess_performance_inner)
    }
    else {
        performance <- lapply(vals_sim_all,
                              assess_performance,
                              mod_est = mod_est,
                              point_est_fun = point_est_fun,
                              include_upper = is_comparable_mod,
                              widths = widths)
    }
    performance <- transpose_list(performance)
    vals_sim <- performance[[1L]]
    error_point_est <- performance[[2L]]
    is_in_interval <- performance[[3L]]
    width_interval <- performance[[4L]]
    vals_sim <- reformat_performance_vec(vals_sim)
    error_point_est <- reformat_performance_vec(error_point_est)
    is_in_interval <- reformat_interval(is_in_interval, nm = "coverage")
    width_interval <- reformat_interval(width_interval, nm = "width")
    id_vars <- make_id_vars_report(mod = mod_est,
                                   include_upper = is_comparable_mod)
    ans <- tibble::tibble(id_vars,
                          vals_sim = vals_sim,
                          error_point_est = error_point_est,
                          is_in_interval,
                          width_interval)
    if (report_type == "short")
        summarise_sim(ans)
    else if (report_type == "long")
        ans
    else
        cli::cli_abort("Internal error: Invalid value for 'report_type'.")
}

## report_sim <- function() {
##   vals_prior_predict <- prior_predict(mod_sim, n_sim = n_sim)
  
  
  
  




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


## HAS_TESTS
#' Convert a List of Simulated Effects to a Data Frame
#'
#' @param vals_effect A named list of matrices
#'
#' @returns A tibble with columns 'component'
#' 'term', 'level', and '.fitted'
#' 
#' @noRd
vals_disp_to_dataframe <- function(vals_disp) {
  .fitted <- matrix(vals_disp, nrow = 1L)
  .fitted <- rvec::rvec_dbl(.fitted)
  tibble::tibble(component = "disp",
                 term = "disp",
                 level = "disp",
                 .fitted = .fitted)
}


## HAS_TESTS
#' Convert a List of Simulated Effects to a Data Frame
#'
#' @param vals_effect A named list of matrices
#'
#' @returns A tibble with columns 'component'
#' 'term', 'level', and '.fitted'
#' 
#' @noRd
vals_effect_to_dataframe <- function(vals_effect) {
  nrow <- vapply(vals_effect, nrow, 0L)
  component <- rep.int("effect", times = sum(nrow))
  term <- rep.int(names(vals_effect), times = nrow)
  level <- lapply(vals_effect, rownames)
  level <- unlist(level, use.names = FALSE)
  .fitted <- do.call(rbind, vals_effect)
  .fitted <- rvec::rvec(.fitted)
  tibble::tibble(component = component,
                 term = term,
                 level = level,
                 .fitted = .fitted)
}
