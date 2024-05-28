
## HAS_TESTS
#' Aggregate Simulation Report for 'augment'
#'
#' @param report_aug A tibble
#'
#' @returns A tibble
#'
#' @noRd
aggregate_report_aug <- function(report_aug) {
  nms_x <- grep("^\\.observed|^\\.error|^\\.cover|^\\.length",
                names(report_aug),
                value = TRUE)
  ans <- stats::aggregate(report_aug[nms_x], report_aug[".var"], mean)
  ans <- tibble::tibble(ans)
  ord <- order(match(ans$.var, report_aug$.var))
  ans <- ans[ord, , drop = FALSE]
  ans
}


## HAS_TESTS
#' Aggregate Simulation Report for 'components'
#'
#' @param report_comp A tibble
#'
#' @returns A tibble
#'
#' @noRd
aggregate_report_comp <- function(report_comp) {
  nms_x <- grep("^\\.error|^\\.cover|^\\.length",
                names(report_comp),
                value = TRUE)
  nms_by <- c("term", "component")
  ans <- stats::aggregate(report_comp[nms_x], report_comp[nms_by], mean)
  ans <- tibble::tibble(ans)
  ord <- order(match(ans$term, report_comp$term),
               match(ans$component, report_comp$component))
  ans <- ans[ord, , drop = FALSE]
  ans
}


## HAS_TESTS
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
      cli::cli_abort("Internal error: coud not generate stationary distribution.")  ## nocov
    ans[, i_sim] <- val
  }
  rn <- if (n == 1L) "coef" else paste0("coef", seq_len(n))
  rownames(ans) <- rn
  ans
}


## HAS_TESTS
#' Draw Values that Would be Produced by a Call to 'components'
#'
#' @param mod Object of class 'bage_mod'
#' @param n_sim Number of draws
#' @param center Whether to center effects (other than
#' the first effect with a SVD prior)
#'
#' @returns Named list
#'
#' @noRd
draw_vals_components_unfitted <- function(mod, n_sim, center) {
  priors <- mod$priors
  nms_priors <- names(priors)
  has_disp <- has_disp(mod)
  seed_components <- mod$seed_components
  seed_restore <- make_seed() ## create randomly-generated seed
  set.seed(seed_components) ## set pre-determined seed
  vals_hyper <- draw_vals_hyper_mod(mod = mod,
                                    n_sim = n_sim)
  vals_hyperrand <- draw_vals_hyperrand_mod(mod = mod,
                                            n_sim = n_sim)
  vals_effect <- draw_vals_effect_mod(mod = mod,
                                      vals_hyper = vals_hyper,
                                      vals_hyperrand = vals_hyperrand,
                                      n_sim = n_sim)
  vals_effect <- standardize_draws_effect(mod = mod,
                                          vals_effect = vals_effect,
                                          center = center)  
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
  set.seed(seed_restore) ## set randomly-generated seed, to restore randomness
  ans    
}


## HAS_TESTS
#' Draw Values for 'disp' from Prior
#'
#' @param mod Obejct of class 'bage_mod'
#' @param n_sim Number of draws
#'
#' @returns An rvec
#'
#' @noRd
draw_vals_disp <- function(mod, n_sim) {
  mean <- mod$mean_disp
  rate <- 1 / mean
  ans <- stats::rexp(n = n_sim, rate = rate)
  ans <- matrix(ans, nrow = 1L)
  ans <- rvec::rvec_dbl(ans)
  ans
}


## HAS_TESTS
#' Draw Values for all Effects
#'
#' Draw values for main effects and interactions.
#'
#' Set intercept to 0
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
  levels_age <- make_levels_age(mod)
  levels_sexgender <- make_levels_sexgender(mod)
  agesex <- make_agesex(mod)
  matrices_along_by <- choose_matrices_along_by(mod)
  matrices_agesex <- make_matrices_agesex(mod)
  ans <- .mapply(draw_vals_effect,
                 dots = list(prior = priors,
                             vals_hyper = vals_hyper,
                             vals_hyperrand = vals_hyperrand,
                             levels_effect = levels_effect,
                             agesex = agesex,
                             matrix_along_by = matrices_along_by,
                             matrix_agesex = matrices_agesex),
                 MoreArgs = list(levels_age = levels_age,
                                 levels_sexgender = levels_sexgender,
                                 n_sim = n_sim))
  ## has_intercept <- identical(nrow(ans[[1L]]), 1L)
  ## if (has_intercept)
  ##   ans[[1L]][] <- 0
  names(ans) <- names(priors)
  ans
}


## HAS_TESTS
#' Generate Draws from Lin
#'
#' Each column is one draw.
#'
#' Results are standardized
#'
#' @param slope Matrix of values
#' @param sd Vector of values
#' @param matrix_along_by Matrix with map for along and by dimensions
#' @param labels Names of elements
#'
#' @returns A matrix, with dimnames.
#'
#' @noRd
draw_vals_lin <- function(slope, sd, matrix_along_by, labels) {
  n_sim <- ncol(slope)
  n_along <- nrow(matrix_along_by)
  n_by <- ncol(matrix_along_by)
  q <- seq(from = -1, to = 1, length.out = n_along)
  q <- rep(q, times = n_by * n_sim)
  slope <- rep(slope, each = n_along)
  sd <- rep(sd, each = n_along * n_by)
  ans <- stats::rnorm(n = n_along * n_by * n_sim,
                      mean = q * slope,
                      sd = sd)
  ans <- matrix(ans, nrow = n_along * n_by, ncol = n_sim)
  i <- match(sort(matrix_along_by), matrix_along_by)
  ans <- ans[i, , drop = FALSE]
  dimnames(ans) <- list(labels, seq_len(n_sim))
  ans
}

## HAS_TESTS
#' Generate Draws from LinAR
#'
#' Each column is one draw.
#'
#' Results are standardized
#'
#' @param slope Matrix of values
#' @param sd Vector of values
#' @param matrix_along_by Matrix with map for along and by dimensions
#' @param labels Names of elements
#'
#' @returns A matrix, with dimnames.
#'
#' @noRd
draw_vals_linar <- function(slope, sd, coef, matrix_along_by, labels) {
  n_sim <- ncol(slope)
  n_along <- nrow(matrix_along_by)
  n_by <- ncol(matrix_along_by)
  n_ar <- nrow(coef)
  q <- seq(from = -1, to = 1, length.out = n_along)
  slope <- as.numeric(slope)
  mean <- outer(q, slope)
  coef <- array(coef, dim = c(n_ar, n_sim, n_by))
  coef <- aperm(coef, perm = c(1L, 3L, 2L))
  coef <- matrix(coef, nrow = n_ar, ncol = n_by * n_sim)
  sd <- rep(sd, each = n_by)
  error <- draw_vals_ar(n = n_along, coef = coef, sd = sd)
  ans <- mean + error
  ans <- matrix(ans, nrow = n_along * n_by, ncol = n_sim)
  i <- match(sort(matrix_along_by), matrix_along_by)
  ans <- ans[i, , drop = FALSE]
  dimnames(ans) <- list(labels, seq_len(n_sim))
  ans
}


## HAS_TESTS
#' Generate Draws from RW
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
draw_vals_rw <- function(sd, matrix_along_by, labels) {
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
  ans <- matrix(ans, nrow = n_along * n_by, ncol = n_sim)
  i <- match(sort(matrix_along_by), matrix_along_by)
  ans <- ans[i, , drop = FALSE]
  dimnames(ans) <- list(labels, seq_len(n_sim))
  ans
}

## HAS_TESTS
#' Generate Draws from RW2
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
draw_vals_rw2 <- function(sd, matrix_along_by, labels) {
  n_sim <- length(sd)
  n_along <- nrow(matrix_along_by)
  n_by <- ncol(matrix_along_by)
  sd <- rep(sd, each = n_by)
  ans <- matrix(nrow = n_along, ncol = n_by * n_sim)
  ans[1L, ] <- stats::rnorm(n = n_by * n_sim)
  ans[2L, ] <- stats::rnorm(n = n_by * n_sim)
  for (i_along in seq.int(from = 3L, to = n_along))
    ans[i_along, ] <- stats::rnorm(n = n_by * n_sim,
                                   mean = 2 * ans[i_along - 1L, ] - ans[i_along - 2L, ],
                                   sd = sd)
  ans <- matrix(ans, nrow = n_along * n_by, ncol = n_sim)
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
draw_vals_hyperrand_mod <- function(mod, n_sim) {
  priors <- mod$priors
  matrices_along_by <- choose_matrices_along_by(mod)
  ans <- .mapply(draw_vals_hyperrand,
                 dots = list(prior = priors,
                             matrix_along_by = matrices_along_by),
                 MoreArgs = list(n_sim = n_sim))
  names(ans) <- names(priors)
  ans
}


## HAS_TESTS
#' Draw Values for the 'slope' Hyper-Parameters of a Prior
#'
#' @param prior An object of class 'bage_prior'
#' @param matrix_along_by Matrix with mapping for along and by dimensions
#' @param n_sim Number of draws
#'
#' @returns A matrix
#'
#' @noRd
draw_vals_slope <- function(sd_slope, matrix_along_by, n_sim) {
  n_by <- ncol(matrix_along_by)
  ans <- stats::rnorm(n = n_by * n_sim, sd = sd_slope)
  ans <- matrix(ans, nrow = n_by, ncol = n_sim)
  nms_by <- colnames(matrix_along_by)
  rownames <- paste("slope", nms_by, sep = ".")
  rownames(ans) <- rownames
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
#' Calculate Errors from Using Point Estimates from Posterior Distribution
#'
#' @param var_est An rvec holding a posterior distribution.
#' @param var_sim A numeric vector holding the true valuevalues
#' @param point_est_fun Name of function to calculate point estimate
#'
#' @returns A vector of doubles
#'
#' @noRd
error_point_est <- function(var_est, var_sim, point_est_fun) {
  if (point_est_fun == "mean")
    point_est <- rvec::draws_mean(var_est)
  else if (point_est_fun == "median")
    point_est <- rvec::draws_median(var_est)
  else
    cli::cli_abort("Internal error: Invalid value for 'point_est_fun'.")  ## nocov
  point_est - var_sim
}


## HAS_TESTS
#' Extract 'error_point_est' Results
#' from Performance Statistics
#'
#' @param perform A list, each element of
#' which is the return value of
#' 'perform_aug' or 'preform_comp'
#' for one simulation
#'
#' @returns A tibble
#'
#' @noRd
get_error_point_est <- function(perform) {
  s_sim <- seq_along(perform)
  nms_var <- names(perform[[1L]])
  ans <- lapply(nms_var,
                function(nm_var)
                  lapply(s_sim,
                         function(i_sim) perform[[i_sim]][[nm_var]][["error_point_est"]]))
  ans <- lapply(ans, function(x) rvec::rvec(do.call(cbind, x)))
  names(ans) <- nms_var
  ans <- lapply(ans, tibble::as_tibble)
  ans <- lapply(ans, stats::setNames, ".error")
  ans
}


## HAS_TESTS
#' Extract 'is_in_interval' Results
#' from Performance Statistics
#'
#' @param perform A list, each element of
#' which is the return value of
#' 'perform_aug' or 'preform_comp'
#' for one simulation
#'
#' @returns A tibble
#'
#' @noRd
get_is_in_interval <- function(perform) {
  nms_var <- names(perform[[1L]])
  nms_width <- names(perform[[1L]][[1L]][["is_in_interval"]])
  ans <- lapply(nms_var,
                function(nm_var)
                  lapply(nms_width,
                         function(nm_width)
                           lapply(perform,
                                  function(x)
                                    x[[nm_var]][["is_in_interval"]][[nm_width]])))
  names(ans) <- nms_var
  ans <- lapply(ans,
                function(val_var)
                  lapply(val_var,
                         function(val_width)
                           rvec::rvec(do.call(cbind, val_width))))
  nms_cols <- paste(".cover", nms_width, sep = "_")
  ans <- lapply(ans, function(val_var) stats::setNames(val_var, nms_cols))
  ans <- lapply(ans, tibble::as_tibble)
  ans
}


## HAS_TESTS
#' Extract 'length_interval' Results
#' from Performance Statistics
#'
#' @param perform A list, each element of
#' which is the return value of
#' 'perform_aug' or 'preform_comp'
#' for one simulation
#'
#' @returns A tibble
#'
#' @noRd
get_length_interval <- function(perform) {
  nms_vars <- names(perform[[1L]])
  nms_widths <- names(perform[[1L]][[1L]][["length_interval"]])
  ans <- lapply(nms_vars,
                function(nm_var)
                  lapply(nms_widths,
                         function(nm_width)
                           lapply(perform,
                                  function(x)
                                    x[[nm_var]][["length_interval"]][[nm_width]])))
  names(ans) <- nms_vars
  ans <- lapply(ans,
                function(val_var)
                  lapply(val_var,
                         function(val_width)
                           rvec::rvec(do.call(cbind, val_width))))
  nms_cols <- paste(".length", nms_widths, sep = "_")
  ans <- lapply(ans, function(val_var) stats::setNames(val_var, nms_cols))
  ans <- lapply(ans, tibble::as_tibble)
  ans
}


## HAS_TESTS
#' Test Whether Interval(s) Contain the Truth
#'
#' @param var_est An rvec holding a posterior distribution
#' @param var_sim A numeric vector with the simulation-true values
#' @param widths Widths of intervals (between 0 and 1)
#'
#' @returns A list of logical vectors
#'
#' @noRd
is_in_interval <- function(var_est, var_sim, widths) { 
    n_widths <- length(widths)
    ans <- vector(mode = "list", length = n_widths)
    names(ans) <- 100 * widths
    for (i in seq_len(n_widths)) {
        width <- widths[[i]]
        probs <- c(0.5 - width / 2, 0.5 + width / 2)
        ci <- rvec::draws_quantile(x = var_est, probs = probs)
        lower <- ci[[1L]]
        upper <- ci[[2L]]
        is_in_interval <- (lower <= var_sim) & (var_sim <= upper)
        ans[[i]] <- is_in_interval
    }
    ans
}


## HAS_TESTS
#' Calculates Lengths of Credible Intervals
#'
#' @param var_est An rvec with a posterior distributions.
#' @param widths Widths of intervals (between 0 and 1)
#'
#' @returns A list of logical vectors
#'
#' @noRd
length_interval <- function(var_est, widths) {
  n_widths <- length(widths)
  ans <- vector(mode = "list", length = n_widths)
  names(ans) <- 100 * widths
  for (i in seq_len(n_widths)) {
    width <- widths[[i]]
    probs <- c(0.5 - 0.5 * width, 0.5 + 0.5 * width)
    ci <- rvec::draws_quantile(x = var_est, probs = probs)
    lower <- ci[[1L]]
    upper <- ci[[2L]]
    length <- upper - lower
    ans[[i]] <- length
  }
  ans
}  


## HAS_TESTS
#' Make Simulation Report for 'aug'
#'
#' @param perform A list, each element of
#' which is the return value of 'perform_aug'
#' for one simulation
#' @param comp_sim Tibble created by calling
#' 'components' an an unfitted model.
#'
#' @returns A tibble
#'
#' @noRd
make_report_aug <- function(perform_aug, aug_sim) {
  nms_aug <- names(aug_sim)
  nms_remove <- c(".observed", ".fitted", ".expected")
  nms_keep <- setdiff(nms_aug, nms_remove)
  byvar_aug <- aug_sim[nms_keep]
  error_point_est_aug <- get_error_point_est(perform_aug)
  is_in_interval_aug <- get_is_in_interval(perform_aug)
  length_interval_aug <- get_length_interval(perform_aug)
  nms_var <- names(error_point_est_aug)
  .var <- rep(nms_var, each = nrow(aug_sim))
  byvar_aug <- vctrs::vec_rep(byvar_aug, times = length(nms_var))
  if (".observed" %in% nms_aug)
    .observed <- vctrs::vec_rep(aug_sim[[".observed"]], times = length(nms_var))
  else
    .observed <- NULL
  error_point_est_aug <- vctrs::vec_rbind(!!!error_point_est_aug,
                                          .name_repair = "universal_quiet")
  is_in_interval_aug <- vctrs::vec_rbind(!!!is_in_interval_aug,
                                         .name_repair = "universal_quiet")
  length_interval_aug <- vctrs::vec_rbind(!!!length_interval_aug,
                                          .name_repair = "universal_quiet")
  vctrs::vec_cbind(.var = .var,
                   byvar_aug,
                   .observed = .observed,
                   error_point_est_aug,
                   is_in_interval_aug,
                   length_interval_aug)
}


## HAS_TESTS
#' Make Simulation Report for 'components'
#'
#' Merge two tibbles to allow for possibility
#' that mod_est and mod_sim differ.
#'
#' @param perform_comp A list, each element of
#' which is the return value of
#' 'perform_comp' for one simulation
#' @param comp_est Tibble created by calling
#' 'components' an a fitted model.
#' @param comp_sim Tibble created by calling
#' 'components' an an unfitted model.
#'
#' @returns A tibble
#'
#' @noRd
make_report_comp <- function(perform_comp, comp_est, comp_sim) {
  byvar_comp <- merge(x = comp_est[c("term", "component", "level")],
                      y = comp_sim[c("term", "component", "level")],
                      sort = FALSE)
  error_point_est_comp <- get_error_point_est(perform_comp)
  is_in_interval_comp <- get_is_in_interval(perform_comp)
  length_interval_comp <- get_length_interval(perform_comp)
  vctrs::vec_cbind(byvar_comp,
                   error_point_est_comp$.fitted,
                   is_in_interval_comp$.fitted,
                   length_interval_comp$.fitted)
}


## HAS_TESTS
#' Calculate Performance Measures for a Single 
#' Set of Simulated 'augment' Values
#'
#' 'est' and 'sim' assumed to be aligned
#'
#' @param est Data frame that includes
#' posterior distribution(s)
#' @param sim Data frame that includes
#' simulation-true values
#' @param i_sim Simulation draw to use for performance
#' measures.
#' @param point_est_fun Mean or median. Function used
#' to calculate point estimates
#' @param widths Widths of credible intervals.
#'
#' @returns Named list
#'
#' @noRd
perform_aug <- function(est,
                        sim,
                        i_sim,
                        point_est_fun,
                        widths) {
  nms_vars <- c(".fitted", ".expected")
  nms_vars <- intersect(names(est), nms_vars)
  nms_vars <- intersect(names(sim), nms_vars)
  for (nm in nms_vars) {
    i_nm <- match(nm, names(sim))
    sim[[i_nm]] <- as.matrix(sim[[i_nm]])[, i_sim]
  }
  ans <- vector(mode = "list", length = length(nms_vars))
  names(ans) <- nms_vars
  for (nm in nms_vars) {
    var_est <- est[[nm]]
    var_sim <- sim[[nm]]
    ans[[nm]] <- list(error_point_est = error_point_est(var_est = var_est,
                                                        var_sim = var_sim,
                                                        point_est_fun = point_est_fun),
                      is_in_interval = is_in_interval(var_est = var_est,
                                                      var_sim = var_sim,
                                                      widths = widths),
                      length_interval = length_interval(var_est = var_est,
                                                        widths = widths))
  }
  ans
}


## HAS_TESTS
#' Calculate Performance Measures for a Single 
#' Set of Simulated 'components' Values
#'
#' Use merging to accomodate possibility that 'mod_est'
#' and 'mod_sim' may be different.
#'
#' @param est Data frame with 'by' variables
#' and posterior distribution(s)
#' @param sim Data frame with 'by' variables
#' and simulation-true values
#' @param i_sim Simulation draw to use for performance
#' measures.
#' @param point_est_fun Mean or median. Function used
#' to calculate point estimates
#' @param widths Widths of credible intervals.
#'
#' @returns Named list
#'
#' @noRd
perform_comp <- function(est,
                         sim,
                         i_sim,
                         point_est_fun,
                         widths) {
  names(est)[match(".fitted", names(est))] <- ".fitted_est"
  names(sim)[match(".fitted", names(sim))] <- ".fitted_sim"
  sim[[".fitted_sim"]] <- as.matrix(sim[[".fitted_sim"]])[, i_sim]
  merged <- merge(est, sim, sort = FALSE)
  var_est <- merged[[".fitted_est"]]
  var_sim <- merged[[".fitted_sim"]]
  .fitted <- list(error_point_est = error_point_est(var_est = var_est,
                                                    var_sim = var_sim,
                                                    point_est_fun = point_est_fun),
                  is_in_interval = is_in_interval(var_est = var_est,
                                                  var_sim = var_sim,
                                                  widths = widths),
                  length_interval = length_interval(var_est = var_est,
                                                    widths = widths))
  list(.fitted = .fitted)
}


## HAS_TESTS
#' Simulation study of a model
#'
#' Use simulated data to assess the performance of
#' an estimation model.
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
#' @section Centering:
#'
#' TODO - COMPLETE THIS
#'
#' @param mod_est The model whose performance is being
#' assessed. An object of class `bage_mod`.
#' @param mod_sim The model used to generate the simulated
#' data. If no value is supplied, `mod_est` is used.
#' @param n_sim Number of sets of simulated data to use.
#' Default is 100.
#' @param center Whether to centre effects (apart from
#' an effect with an SVD-type prior) at 0.
#' See below for details. Default is `TRUE`.
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
#' - `cover`. Actual proportion of simulation-true values
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
#' - `cover`. Actual proportions of simulation-true values
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
                       center = TRUE,
                       point_est_fun = c("median", "mean"),
                       widths = c(0.5, 0.95),
                       report_type = c("short", "long", "full"),
                       n_core = 1) {
  ## check inputs
  check_bage_mod(x = mod_est, nm_x = "mod_est")
  if (is.null(mod_sim))
    mod_sim <- mod_est
  else
    check_mod_est_sim_compatible(mod_est = mod_est,
                                 mod_sim = mod_sim)
  check_n(n = n_sim,
          nm_n = "n_sim",
          min = 1L,
          max = NULL,
          null_ok = FALSE)
  check_flag(x = center, nm_x = "center")
  point_est_fun <- match.arg(point_est_fun)
  check_widths(widths)
  report_type <- match.arg(report_type)
  check_n(n = n_core,
          nm_n = "n_core",
          min = 1L,
          max = NULL,
          null_ok = FALSE)
  ## use 'mod_sim' to generate 'n_sim' sets of simulation-truth
  mod_sim$n_draw <- n_sim
  comp_sim <- components(mod_sim, quiet = TRUE)
  aug_sim <- augment(mod_sim, quiet = TRUE)
  nm_outcome <- get_nm_outcome(mod_sim)
  outcome_sim <- aug_sim[[nm_outcome]]
  outcome_sim <- as.matrix(outcome_sim)
  ## create closure for evaluating 'mod_est' fitted
  ## to a single simulation-truth, and apply to
  ## the 'n_sim' sets
  report_sim_inner <- function(i_sim) {
    library(bage)
    perform_comp <- utils::getFromNamespace("perform_comp", ns = "bage")
    perform_aug <- utils::getFromNamespace("perform_aug", ns = "bage")
    outcome <- outcome_sim[, i_sim]
    mod_est$outcome <- outcome
    mod_est <- fit(mod_est)
    comp_est <- components(mod_est)
    aug_est <- augment(mod_est)
    results_comp <- perform_comp(est = comp_est,
                                 sim = comp_sim,
                                 i_sim = i_sim,
                                 point_est_fun = point_est_fun,
                                 widths = widths)
    results_aug <- perform_aug(est = aug_est,
                               sim = aug_sim,
                               i_sim = i_sim,
                               point_est_fun = point_est_fun,
                               widths = widths)
    list(results_comp, results_aug)
  }
  s_sim <- seq_len(n_sim)
  if (n_core > 1L) {
    iseed <- make_seed()
    cl <- parallel::makeCluster(n_core)
    on.exit(parallel::stopCluster(cl))
    parallel::clusterSetRNGStream(cl, iseed = iseed)
    results <- parallel::parLapply(cl = cl,
                                   X = s_sim,
                                   fun = report_sim_inner)
  }
  else
    results <- lapply(s_sim, report_sim_inner)
  ## extract the results and apply
  perform_comp <- lapply(results, `[[`, 1L)
  perform_aug <- lapply(results, `[[`, 2L)
  comp_est <- components(mod_est, quiet = TRUE)
  report_comp <- make_report_comp(perform_comp = perform_comp,
                                  comp_est = comp_est,
                                  comp_sim = comp_sim)
  report_aug <- make_report_aug(perform_aug = perform_aug,
                                aug_sim = aug_sim)
  if (report_type == "full")
    return(list(components = report_comp,
                augment = report_aug))
  report_comp <- rvec_to_mean(report_comp)
  report_aug <- rvec_to_mean(report_aug)
  if (report_type == "long")
    return(list(components = report_comp,
                augment = report_aug))
  report_comp <- aggregate_report_comp(report_comp)
  report_aug <- aggregate_report_aug(report_aug)
  if (report_type == "short")
    return(list(components = report_comp,
                augment = report_aug))
  cli::cli_abort("Internal error: Invalid value for {.val report_type}.") ## nocov
}


## HAS_TESTS
#' Apply Standardization Algorithm to Simulated Effects
#'
#' @param mod Object of class `"bage_mod"`
#' @param vals_effect List of matrices, each holding 'n_sim'
#' draws of an effect
#'
#' @returns A list of standardized values
#'
#' @noRd
standardize_draws_effect <- function(mod, vals_effect, center) {
  eps <- 0.0001
  max_iter <- 100L
  matrices_effect_outcome <- mod$matrices_effect_outcome
  priors <- mod$priors
  n_effect <- length(priors)
  linpred <- .mapply(`%*%`,
                     dots = list(x = matrices_effect_outcome,
                                 y = vals_effect),
                     MoreArgs = list())
  linpred <- Reduce(`+`, linpred)
  set_matrix_to_zero <- function(x) {
    x[] <- 0
    x
  }
  ans <- lapply(vals_effect, set_matrix_to_zero)
  n_outcome <- nrow(linpred)
  n_draw <- ncol(linpred)
  n_element <- vapply(matrices_effect_outcome, ncol, 1L)
  mult <- n_element / n_outcome
  found_svd <- FALSE
  for (i_iter in seq_len(max_iter)) {
    for (i_effect in seq_len(n_effect)) {
      M <- matrices_effect_outcome[[i_effect]]
      effect <- mult[[i_effect]] * Matrix::crossprod(M, linpred)
      contrib_to_linpred <- M %*% effect
      linpred <- linpred - contrib_to_linpred
      is_intercept <- i_effect == 1L
      if (center) {
        if (!is_intercept)
          ans[[i_effect]] <- ans[[i_effect]] + effect
        if (!found_svd && is_svd(priors[[i_effect]])) {
          ans[[1L]][] <- Matrix::colMeans(vals_effect[[i_effect]])
          found_svd <- TRUE
        }
      }
      else
        ans[[i_effect]] <- ans[[i_effect]] + effect
    }
    max_remainder <- max(abs(linpred))
    if (max_remainder < eps) {
      ans <- lapply(ans, Matrix::as.matrix)
      return(ans)
    }
  }
  cli::cli(c("Internal error: Unable to standardize effects.",   ## nocov
             i = "Maximum remainder: {.val {max_remainder}}."))  ## nocov
}


## HAS_TESTS
#' Convert Simulated Values for 'disp' to a Data Frame
#'
#' @param vals_disp An rvec
#'
#' @returns A tibble with columns 'component'
#' 'term', 'level', and '.fitted'
#' 
#' @noRd
vals_disp_to_dataframe <- function(vals_disp) {
  tibble::tibble(term = "disp",
                 component = "disp",
                 level = "disp",
                 .fitted = vals_disp)
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
  term <- rep.int(names(vals_effect), times = nrow)
  component <- rep.int("effect", times = sum(nrow))
  level <- lapply(vals_effect, rownames)
  level <- unlist(level, use.names = FALSE)
  .fitted <- do.call(rbind, vals_effect)
  .fitted <- unname(.fitted)
  .fitted <- rvec::rvec(.fitted)
  tibble::tibble(term = term,
                 component = component,
                 level = level,
                 .fitted = .fitted)
}
 
