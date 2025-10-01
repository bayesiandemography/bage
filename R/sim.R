
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
#' Draw Values for Fitted, Given Values
#' for '.expected' and 'disp'
#'
#' Used only with simulation and unfitted models,
#' since does not use information on outcomes.
#'
#' Used only with Poisson and binomial,
#' in models that include dispersion.
#'
#' @param nm_distn Name of distribution, eg "pois"
#' @param fitted Rates/probs/means. An rvec.
#' @param disp Dispersion. An rvec.
#'
#' @returns An rvec
#'
#' @noRd
draw_fitted <- function(nm_distn,
                        expected,
                        disp) {
  n <- length(expected)
  if (nm_distn == "pois") {
    shape <- 1 / disp
    rate <- 1 / (disp * expected)
    ans <- rvec::rgamma_rvec(n = n, shape = shape, rate = rate)
  }
  else if (nm_distn == "binom") {
    shape1 <- expected / disp
    shape2 <- (1 - expected) / disp
    ans <- rvec::rbeta_rvec(n = n, shape1 = shape1, shape2 = shape2)
  }
  else
    cli::cli_abort("Internal error: Invalid value for {.var nm_distn}.")
  ans
}


## HAS_TESTS
#' Draw Values for Outcome, Given Values for '.fitted',
#' and, Where Appropriate, 'disp'
#'
#' Used only with simulation and unfitted models,
#' since does not use information on outcomes.
#'
#' If distribution is normal, assume that 'fitted'
#' and 'disp' are on the original scale.
#'
#' @param nm_distn Name of distribution, eg "pois"
#' @param fitted Rates/probs/means. An rvec.
#' @param offset True values for offset
#' @param disp Dispersion. An rvec. Normal model only
#'
#' @returns An rvec
#'
#' @noRd
draw_outcome_true <- function(nm_distn,
                              fitted,
                              offset,
                              disp) {
  n_draw <- rvec::n_draw(fitted)
  n_val <- length(fitted)
  has_disp <- !is.null(disp)
  fitted <- as.numeric(fitted)
  if (rvec::is_rvec(offset))
    offset <- as.numeric(offset)
  else
    offset <- rep(offset, times = n_draw)
  if (has_disp) {
    disp <- as.numeric(disp)
    disp <- rep(disp, each = n_val)
  }
  is_ok <- !is.na(offset) & !is.na(fitted)
  n_ok <- sum(is_ok)
  ans <- rep.int(NA_real_, times = n_val * n_draw)
  if (nm_distn == "pois") {
    lambda <- fitted[is_ok] * offset[is_ok]
    ans[is_ok] <- rpois_guarded(lambda)
  }
  else if (nm_distn == "binom") {
    size <- offset[is_ok]
    prob <- fitted[is_ok]
    ans[is_ok] <- rbinom_guarded(size = size, prob = prob)
  }
  else if (nm_distn == "norm") {
    mean <- fitted[is_ok]
    sd <- disp[is_ok] / sqrt(offset[is_ok])
    ans[is_ok] <- stats::rnorm(n = n_ok, mean = mean, sd = sd)
  }
  else
    cli::cli_abort("Internal error: Invalid value for {.var nm_distn}.")
  ans <- matrix(ans, nrow = n_val, ncol = n_draw)
  ans <- rvec::rvec_dbl(ans)
  ans
}


## HAS_TESTS
#' Generate AR Values to Use in a Prior with an AR Component
#'
#' @param coef Matrix, each column of which is a vector
#' of autocorrelation coefficients
#' @param sd Vector, each element of which is a
#' marginal standard deviation
#' @param matrix_along_by Matrix mapping position in along and
#' by dimensions to effect
#' @param levels_effect Names of elements of effect
#'
#' @returns A matrix
#'
#' @noRd
draw_vals_ar <- function(coef,
                         sd,
                         matrix_along_by,
                         levels_effect) {
  n_along <- nrow(matrix_along_by)
  n_by <- ncol(matrix_along_by)
  n_sim <- ncol(coef)
  s <- rep(seq_len(n_sim), each = n_by)
  coef <- coef[, s, drop = FALSE]
  sd <- sd[s]
  ans <- draw_vals_ar_inner(n = n_along, coef = coef, sd = sd)
  ans <- matrix(ans, nrow = n_along * n_by, ncol = n_sim)
  i <- match(sort(matrix_along_by), matrix_along_by)
  ans <- ans[i, , drop = FALSE]
  dimnames(ans) <- list(levels_effect, NULL)
  ans
}


## HAS_TESTS
#' Generate Values for Multiple AR-k Series of Length 'n'
#'
#' @param n Length of series (not order of series)
#' @param coef Matrix, each column of which is a vector
#' of autocorrelation coefficients
#' @param sd Vector, each element of which is a
#' marginal standard deviation
#'
#' @returns A matrix with 'n' rows and 'length(sd)' columns
#'
#' @noRd
draw_vals_ar_inner <- function(n, coef, sd) {
  n_sim <- length(sd)
  ans <- matrix(NA_real_, nrow = n, ncol = n_sim)
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
#' @param prior Object of class "bage_prior"
#' @param n_sim Number of draws
#'
#' @returns A vector of length 'n'
#'
#' @noRd
draw_vals_coef <- function(prior, n_sim) {
  max_attempt <- 1000L
  specific <- prior$specific
  n_coef <- specific$n_coef
  shape1 <- specific$shape1
  shape2 <- specific$shape2
  min <- specific$min
  max <- specific$max
  ans <- matrix(NA_real_, nrow = n_coef, ncol = n_sim)
  for (i_sim in seq_len(n_sim)) {
    found_val <- FALSE
    for (i_attempt in seq_len(max_attempt)) {
      val_raw <- stats::rbeta(n = n_coef,
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
  rn <- if (n_coef == 1L) "coef" else paste0("coef", seq_len(n_coef))
  rownames(ans) <- rn
  ans
}


## HAS_TESTS
#' Draw Values that Would be Produced by a Call to 'components'
#'
#' @param mod Object of class 'bage_mod'
#' @param n_sim Number of draws
#' estimates
#'
#' @returns Named list
#'
#' @noRd
draw_vals_components_unfitted <- function(mod, n_sim) {
  data <- mod$data
  priors <- mod$priors
  dimnames_terms <- mod$dimnames_terms
  var_time <- mod$var_time
  var_age <- mod$var_age
  var_sexgender <- mod$var_sexgender
  has_covariates <- has_covariates(mod)
  has_disp <- has_disp(mod)
  has_datamod_param <- has_datamod_param(mod)
  seed_components <- mod$seed_components
  seed_restore <- make_seed() ## create randomly-generated seed
  set.seed(seed_components) ## set pre-determined seed
  vals_hyper <- draw_vals_hyper_mod(mod = mod,
                                    n_sim = n_sim)
  vals_hyperrand <- draw_vals_hyperrand_mod(mod = mod,
                                            vals_hyper = vals_hyper,
                                            n_sim = n_sim)
  vals_spline <- draw_vals_spline_mod(mod = mod,
                                      vals_hyper = vals_hyper,
                                      n_sim = n_sim)
  vals_svd <- draw_vals_svd_mod(mod = mod,
                                vals_hyper = vals_hyper,
                                n_sim = n_sim)
  vals_effect <- draw_vals_effect_mod(mod = mod,
                                      vals_hyper = vals_hyper,
                                      vals_hyperrand = vals_hyperrand,
                                      vals_spline = vals_spline,
                                      vals_svd = vals_svd,
                                      n_sim = n_sim)
  vals_hyper <- vals_hyper_to_dataframe(vals_hyper = vals_hyper,
                                        n_sim = n_sim)
  vals_hyperrand <- vals_hyperrand_to_dataframe(mod = mod,
                                                vals_hyperrand = vals_hyperrand,
                                                n_sim = n_sim)
  vals_spline <- vals_spline_to_dataframe(vals_spline = vals_spline,
                                          n_sim = n_sim)
  vals_svd <- vals_svd_to_dataframe(vals_svd = vals_svd,
                                    n_sim = n_sim)
  vals_effect <- vals_effect_to_dataframe(vals_effect)
  ans <- vctrs::vec_rbind(vals_hyper,
                          vals_hyperrand,
                          vals_effect,
                          vals_spline,
                          vals_svd)
  if (has_covariates) {
    vals_covariates <- draw_vals_covariates(mod = mod,
                                            n_sim = n_sim)
    vals_covariates <- vals_covariates_to_dataframe(vals_covariates)
    ans <- vctrs::vec_rbind(ans, vals_covariates)
  }
  if (has_disp) {
    vals_disp <- draw_vals_disp(mod = mod,
                                n_sim = n_sim)
    vals_disp <- vals_disp_to_dataframe(vals_disp)
    ans <- vctrs::vec_rbind(ans, vals_disp)
  }
  if (has_datamod_param) {
    datamod <- mod$datamod
    vals_datamod <- draw_datamod_param(datamod = datamod,
                                       n_sim = n_sim)
    vals_datamod <- vals_datamod_to_dataframe(vals_datamod = vals_datamod,
                                              datamod = datamod)
    ans <- vctrs::vec_rbind(ans, vals_datamod)
  }    
  set.seed(seed_restore) ## set randomly-generated seed, to restore randomness
  ans    
}


## HAS_TESTS
#' Draw Values for Covariates
#'
#' @param mod Object of class "bage_mod"
#' @param n_sim Number of draws
#'
#' @returns A named list
#'
#' @noRd
draw_vals_covariates <- function(mod, n_sim) {
  covariates_nms <- mod$covariates_nms
  n_covariates <- length(covariates_nms)
  coef <- stats::rnorm(n = n_covariates * n_sim)
  coef <- matrix(coef,
                 nrow = n_covariates,
                 ncol = n_sim,
                 dimnames = list(covariates_nms, NULL))
  ans <- list(coef = coef)
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
#' @param mod Object of class "bage_mod"
#' @param vals_hyper List of lists.
#' @param vals_hyperrand List of lists.
#' @param vals_spline List of lists.
#' @param vals_svd List of lists.
#' @param n_sim Number of draws
#'
#' @returns A named list of matrices.
#'
#' @noRd
draw_vals_effect_mod <- function(mod,
                                 vals_hyper,
                                 vals_hyperrand,
                                 vals_spline,
                                 vals_svd,
                                 n_sim) {
  priors <- mod$priors
  dimnames_terms <- mod$dimnames_terms
  var_time <- mod$var_time
  var_age <- mod$var_age
  var_sexgender <- mod$var_sexgender
  ans <- .mapply(draw_vals_effect,
                 dots = list(prior = priors,
                             vals_hyper = vals_hyper,
                             vals_hyperrand = vals_hyperrand,
                             vals_spline = vals_spline,
                             vals_svd = vals_svd,
                             dimnames_term = dimnames_terms),
                 MoreArgs = list(var_time = var_time,
                                 var_age = var_age,
                                 var_sexgender = var_sexgender,
                                 n_sim = n_sim))
  names(ans) <- names(priors)
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
#' @param vals_hyper List of lists.
#' @param n_sim Number of draws
#'
#' @returns A named list
#'
#' @noRd
draw_vals_hyperrand_mod <- function(mod, vals_hyper, n_sim) {
  priors <- mod$priors
  dimnames_terms <- mod$dimnames_terms
  var_time <- mod$var_time
  var_age <- mod$var_age
  ans <- .mapply(draw_vals_hyperrand,
                 dots = list(prior = priors,
                             vals_hyper = vals_hyper,
                             dimnames_term = dimnames_terms),
                 MoreArgs = list(var_time = var_time,
                                 var_age = var_age,
                                 n_sim = n_sim))
  names(ans) <- names(priors)
  ans
}


## HAS_TESTS
#' Generate Draws from Lin
#'
#' Each column is one draw.
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
  n_sim <- length(sd)
  n_along <- nrow(matrix_along_by)
  n_by <- ncol(matrix_along_by)
  slope <- rep(slope, each = n_along)
  intercept <- -0.5 * (n_along + 1) * slope
  sd <- rep(sd, each = n_along * n_by)
  s <- seq_len(n_along)
  ans <- stats::rnorm(n = n_along * n_by * n_sim,
                      mean = intercept + slope * s,
                      sd = sd)
  ans <- matrix(ans, nrow = n_along * n_by, ncol = n_sim)
  i <- match(sort(matrix_along_by), matrix_along_by)
  ans <- ans[i, , drop = FALSE]
  dimnames(ans) <- list(labels, NULL)
  ans
}


## HAS_TESTS
#' Generate Draws from Lin_AR
#'
#' Each column is one draw.
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
  n_coef <- nrow(coef)
  slope <- as.numeric(slope)
  intercept <- -0.5 * (n_along + 1) * slope
  s <- seq_len(n_along)
  mean <- outer(s, slope) + rep(intercept, each = n_along)
  coef <- array(coef, dim = c(n_coef, n_sim, n_by))
  coef <- aperm(coef, perm = c(1L, 3L, 2L))
  coef <- matrix(coef, nrow = n_coef, ncol = n_by * n_sim)
  sd <- rep(sd, each = n_by)
  error <- draw_vals_ar_inner(n = n_along, coef = coef, sd = sd)
  ans <- mean + error
  ans <- matrix(ans, nrow = n_along * n_by, ncol = n_sim)
  i <- match(sort(matrix_along_by), matrix_along_by)
  ans <- ans[i, , drop = FALSE]
  dimnames(ans) <- list(labels, NULL)
  ans
}


## HAS_TESTS
#' Generate Draws from a Linear Trend
#'
#' Each column is one draw.
#'
#' @param slope Matrix of values
#' @param matrix_along_by Matrix with map for along and by dimensions
#' @param labels Names of elements
#'
#' @returns A matrix, with dimnames.
#'
#' @noRd
draw_vals_lintrend <- function(slope, matrix_along_by, labels) {
  n_sim <- ncol(slope)
  n_along <- nrow(matrix_along_by)
  n_by <- ncol(matrix_along_by)
  slope <- rep(slope, each = n_along)
  intercept <- -0.5 * (n_along + 1) * slope
  s <- seq_len(n_along)
  ans <- intercept + slope * s
  ans <- matrix(ans, nrow = n_along * n_by, ncol = n_sim)
  i <- match(sort(matrix_along_by), matrix_along_by)
  ans <- ans[i, , drop = FALSE]
  dimnames(ans) <- list(labels, NULL)
  ans
}


## HAS_TESTS
#' Generate Draws from Normal Distribution
#'
#' @param sd Vector of values
#' @param labels Names of elements
#'
#' @returns A matrix, with dimnames.
#'
#' @noRd
draw_vals_norm <- function(sd, labels) {
  n_sim <- length(sd)
  n_effect <- length(labels)
  n <- n_effect * n_sim
  sd <- rep(sd, each = n_effect)
  ans <- stats::rnorm(n = n, sd = sd)
  ans <- matrix(ans, nrow = n_effect, ncol = n_sim)
  dimnames(ans) <- list(labels, NULL)
  ans
}


## HAS_TESTS
#' Generate Draws from RW
#'
#' Each column is one draw.
#'
#' @param sd Vector of values
#' @param sd_init Standard deviation of initial values. 
#' @param matrix_along_by Matrix with map for along and by dimensions
#' @param levels_effect Names of elements
#'
#' @returns A matrix, with dimnames.
#'
#' @noRd
draw_vals_rw <- function(sd, sd_init, matrix_along_by, levels_effect) {
  n_sim <- length(sd)
  n_along <- nrow(matrix_along_by)
  n_by <- ncol(matrix_along_by)
  ans <- matrix(NA_real_, nrow = n_along, ncol = n_by * n_sim)
  ans[1L, ] <- stats::rnorm(n = n_by * n_sim, sd = sd_init)
  sd <- rep(sd, each = n_by)
  for (i_along in seq.int(from = 2L, to = n_along))
    ans[i_along, ] <- stats::rnorm(n = n_by * n_sim,
                                   mean = ans[i_along - 1L, ],
                                   sd = sd)
  ans <- matrix(ans, nrow = n_along * n_by, ncol = n_sim)
  i <- match(sort(matrix_along_by), matrix_along_by)
  ans <- ans[i, , drop = FALSE]
  dimnames(ans) <- list(levels_effect, NULL)
  ans
}


## HAS_TESTS
#' Generate Draws from RW2
#'
#' Each column is one draw.
#'
#' @param sd Vector of values
#' @param sd_slope Double
#' @param matrix_along_by Matrix with map for along and by dimensions
#' @param levels_effect Names of elements
#'
#' @returns A matrix, with dimnames.
#'
#' @noRd
draw_vals_rw2 <- function(sd, sd_init, sd_slope, matrix_along_by, levels_effect) {
  n_sim <- length(sd)
  n_along <- nrow(matrix_along_by)
  n_by <- ncol(matrix_along_by)
  sd <- rep(sd, each = n_by)
  ans <- matrix(NA_real_, nrow = n_along, ncol = n_by * n_sim)
  ans[1L, ] <- stats::rnorm(n = n_by * n_sim, sd = sd_init)
  ans[2L, ] <- stats::rnorm(n = n_by * n_sim, mean = ans[1L, ], sd = sd_slope)
  for (i_along in seq.int(from = 3L, to = n_along))
    ans[i_along, ] <- stats::rnorm(n = n_by * n_sim,
                                   mean = 2 * ans[i_along - 1L, ] - ans[i_along - 2L, ],
                                   sd = sd)
  ans <- matrix(ans, nrow = n_along * n_by, ncol = n_sim)
  i <- match(sort(matrix_along_by), matrix_along_by)
  ans <- ans[i, , drop = FALSE]
  dimnames(ans) <- list(levels_effect, NULL)
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
#' Draw the 'sd' parameter for a prior
#'
#' @param prior An object of class 'bage_prior'
#' @param n_sim Number of draws
#'
#' @returns A numeric vector
#'
#' @noRd
draw_vals_sd_seas <- function(prior, n_sim) {
    scale <- prior$specific$scale_seas
    ans <- stats::rnorm(n = n_sim, sd = scale)
    ans <- abs(ans)
    ans
}

## HAS_TESTS
#' Generate Draws for Fixed Seasonal Effects
#'
#' Each column is one draw.
#'
#' @param n_seas Number of seasons
#' @param sd_init Scalar. SD of initial values
#' @param matrix_along_by Matrix with map for along and by dimensions
#' @param n_sim Number of draws
#'
#' @returns A matrix, with dimnames.
#'
#' @noRd
draw_vals_seasfix <- function(n_seas, sd_init, matrix_along_by, n_sim) {
  n_along <- nrow(matrix_along_by)
  n_by <- ncol(matrix_along_by)
  ans <- matrix(NA_real_, nrow = n_along, ncol = n_by * n_sim)
  m <- diag(n_seas - 1L)
  m[, 1L] <- 1
  m <- rbind(m, -1 * colSums(m))
  z <- matrix(stats::rnorm(n = (n_seas - 1L) * n_by * n_sim, sd = sd_init),
              nrow = n_seas - 1L)
  ans[seq_len(n_seas), ] <- m %*% z
  for (i_along in seq.int(from = n_seas + 1L, to = n_along))
    ans[i_along, ] <- ans[i_along - n_seas, ]
  ans <- matrix(ans, nrow = n_along * n_by, ncol = n_sim)
  i <- match(sort(matrix_along_by), matrix_along_by)
  ans <- ans[i, , drop = FALSE]
  ans
}


## HAS_TESTS
#' Generate Draws for Time-Varying Seasonal Effects
#'
#' Each column is one draw.
#'
#' @param n_seas Number of seasons
#' @param sd_init Scalar. SD of initial values.
#' @param sd_innov Vector of values. SD of innoviations
#' @param matrix_along_by Matrix with map for along and by dimensions
#'
#' @returns A matrix, with dimnames.
#'
#' @noRd
draw_vals_seasvary <- function(n_seas, sd_init, sd_innov, matrix_along_by) {
  n_sim <- length(sd_innov)
  n_along <- nrow(matrix_along_by)
  n_by <- ncol(matrix_along_by)
  ans <- matrix(0, nrow = n_along, ncol = n_by * n_sim)
  m <- diag(n_seas - 1L)
  m[, 1L] <- 1
  m <- rbind(m, -1 * colSums(m))
  z <- matrix(stats::rnorm(n = (n_seas - 1L) * n_by * n_sim, sd = sd_init),
              nrow = n_seas - 1L)
  ans[seq_len(n_seas), ] <- m %*% z
  sd_innov <- rep(sd_innov, each = n_by)
  for (i_along in seq.int(from = n_seas + 1L, to = n_along)) {
    is_first_seas <- (i_along - 1L) %% n_seas == 0L
    if (!is_first_seas)
      ans[i_along, ] <- stats::rnorm(n = n_by * n_sim,
                                     mean = ans[i_along - n_seas, ],
                                     sd = sd_innov)
  }
  ans <- matrix(ans, nrow = n_along * n_by, ncol = n_sim)
  i <- match(sort(matrix_along_by), matrix_along_by)
  ans <- ans[i, , drop = FALSE]
  ans
}


## HAS_TESTS
#' Draw Values for the 'slope' Hyper-Parameters of a Prior
#'
#' @param mean_slope Prior mean of slope. A scalar.
#' @param sd_slope Prior standard deviation of slope. A positive scalar.
#' @param matrix_along_by Matrix with mapping for along and by dimensions
#' @param n_sim Number of draws
#'
#' @returns A matrix
#'
#' @noRd
draw_vals_slope <- function(mean_slope, sd_slope, matrix_along_by, n_sim) {
  n_by <- ncol(matrix_along_by)
  ans <- stats::rnorm(n = n_by * n_sim, mean = mean_slope, sd = sd_slope)
  ans <- matrix(ans, nrow = n_by, ncol = n_sim)
  nms_by <- colnames(matrix_along_by)
  if (n_by > 1L)
    rownames <- paste("slope", nms_by, sep = ".")
  else
    rownames <- "slope"
  rownames(ans) <- rownames
  ans
}


## HAS_TESTS
#' Draw Values for Spline Coefficients
#'
#' @param mod Object of class "bage_mod"
#' @param vals_hyper List of lists.
#' @param n_sim Number of draws
#'
#' @returns A named list
#'
#' @noRd
draw_vals_spline_mod <- function(mod, vals_hyper, n_sim) {
  priors <- mod$priors
  dimnames_terms <- mod$dimnames_terms
  var_time <- mod$var_time
  var_age <- mod$var_age
  levels_spline <- make_levels_spline(mod, unlist = FALSE)
  ans <- .mapply(draw_vals_spline,
                 dots = list(prior = priors,
                             vals_hyper = vals_hyper,
                             dimnames_term = dimnames_terms,
                             levels_spline = levels_spline),
                 MoreArgs = list(var_time = var_time,
                                 var_age = var_age,
                                 n_sim = n_sim))
  names(ans) <- names(priors)
  ans
}


## HAS_TESTS
#' Draw Values for SVD Coefficients
#'
#' @param mod Object of class "bage_mod"
#' @param vals_hyper List of lists.
#' @param n_sim Number of draws
#'
#' @returns A named list
#'
#' @noRd
draw_vals_svd_mod <- function(mod, vals_hyper, n_sim) {
  priors <- mod$priors
  dimnames_terms <- mod$dimnames_terms
  var_time <- mod$var_time
  var_age <- mod$var_age
  var_sexgender <- mod$var_sexgender
  levels_svd <- make_levels_svd(mod, unlist = FALSE)
  ans <- .mapply(draw_vals_svd,
                 dots = list(prior = priors,
                             vals_hyper = vals_hyper,
                             dimnames_term = dimnames_terms,
                             levels_svd = levels_svd),
                 MoreArgs = list(var_time = var_time,
                                 var_age = var_age,
                                 var_sexgender = var_sexgender,
                                 n_sim = n_sim))
  names(ans) <- names(priors)
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
#' @param prior_class_est Data frame giving class
#' of each term in priors for 'mod_est'
#' @param prior_class_sim Data frame giving class
#' of each term in prior for 'mod_sim'
#'
#' @returns A tibble
#'
#' @noRd
make_report_comp <- function(perform_comp,
                             comp_est,
                             comp_sim,
                             prior_class_est,
                             prior_class_sim) {
  byvar_comp <- merge(x = comp_est[c("term", "component", "level")],
                      y = comp_sim[c("term", "component", "level")],
                      sort = FALSE)
  prior_class <- merge(prior_class_est, prior_class_sim, by = "term")
  prior_class$is_class_diff <- prior_class$class.x != prior_class$class.y
  is_class_diff <- prior_class$is_class_diff[match(byvar_comp$term, prior_class$term)]
  is_hyper <- byvar_comp$component == "hyper"
  is_remove <- is_class_diff & is_hyper
  byvar_comp <- byvar_comp[!is_remove, , drop = FALSE]
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
    sim[[i_nm]] <- rvec::extract_draw(sim[[i_nm]], i = i_sim)
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
#' @param prior_class_est Data frame giving class
#' of each term in priors for 'mod_est'
#' @param prior_class_sim Data frame giving class
#' of each term in prior for 'mod_sim'
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
                         prior_class_est,
                         prior_class_sim,
                         i_sim,
                         point_est_fun,
                         widths) {
  names(est)[match(".fitted", names(est))] <- ".fitted_est"
  names(sim)[match(".fitted", names(sim))] <- ".fitted_sim"
  sim[[".fitted_sim"]] <- rvec::extract_draw(sim[[".fitted_sim"]], i = i_sim)  
  merged <- merge(est, sim, sort = FALSE)
  prior_class <- merge(prior_class_est, prior_class_sim, by = "term")
  prior_class$is_class_diff <- prior_class$class.x != prior_class$class.y
  is_class_diff <- prior_class$is_class_diff[match(merged$term, prior_class$term)]
  is_hyper <- merged$component == "hyper"
  is_remove <- is_class_diff & is_hyper
  merged <- merged[!is_remove, , drop = FALSE]
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
#' Simulation Study of a Model
#'
#' Use simulated data to assess the performance of
#' an estimation model.
#'
#' @section Warning:
#'
#' The interface for `report_sim()` is still under development
#' and may change in future.
#'
#' @param mod_est The model whose performance is being
#' assessed. An object of class `bage_mod`.
#' @param mod_sim The model used to generate the simulated
#' data. If no value is supplied, `mod_est` is used.
#' @param method Estimation method used for `mod_est`.
#' See [fit()].
#' @param vars_inner Variables used in inner model
#' with `"inner-outer"`estimation method.
#' See [fit()].
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
#' @returns
#'
#' A named list with a tibble called `"components"` and a
#' tibble called `"augment"`.
#'
#' @seealso
#' - [mod_pois()] Specify binomial model
#' - [mod_binom()] Specify binomial model
#' - [mod_norm()] Specify normal model
#' - [set_prior()] Specify non-default prior for term
#' - [set_disp()] Specify non-default prior for dispersion
#' - [fit()] Fit a model
#' - [replicate_data()] Generate replicate
#'   data for a model
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
                       method = c("standard", "inner-outer"),
                       vars_inner = NULL,
                       n_sim = 100,
                       point_est_fun = c("median", "mean"),
                       widths = c(0.5, 0.95),
                       report_type = c("short", "long", "full"),
                       n_core = 1) {
  ## check inputs
  check_bage_mod(x = mod_est, nm_x = "mod_est")
  if (is_fitted(mod_est)) {
    mod_est <- unfit(mod_est)
    cli::cli_alert("Unfitting {.arg mod_est}.")
  }
  if (is.null(mod_sim))
    mod_sim <- mod_est
  else {
    check_mod_est_sim_compatible(mod_est = mod_est,
                                 mod_sim = mod_sim)
    if (is_fitted(mod_sim)) {
      mod_sim <- unfit(mod_sim)
      cli::cli_alert("Unfitting {.arg mod_sim}.")
    }
  }    
  poputils::check_n(n = n_sim,
                    nm_n = "n_sim",
                    min = 1L,
                    max = NULL,
                    divisible_by = NULL)
  point_est_fun <- match.arg(point_est_fun)
  check_widths(widths)
  report_type <- match.arg(report_type)
  poputils::check_n(n = n_core,
                    nm_n = "n_core",
                    min = 1L,
                    max = NULL,
                    divisible_by = NULL)
  ## use 'mod_sim' to generate 'n_sim' sets of simulation-truth
  mod_sim$n_draw <- n_sim
  comp_sim <- components(mod_sim, quiet = TRUE)
  aug_sim <- augment(mod_sim, quiet = TRUE)
  nm_outcome_obs <- get_nm_outcome_data(mod_sim)
  outcome_obs_sim <- aug_sim[[nm_outcome_obs]]
  outcome_obs_sim <- as.matrix(outcome_obs_sim)
  prior_class_est <- make_prior_class(mod_est)
  prior_class_sim <- make_prior_class(mod_sim)
  ## create closure for evaluating 'mod_est' fitted
  ## to a single simulation-truth, and apply to
  ## the 'n_sim' sets
  report_sim_inner <- function(i_sim) {
    library(bage)
    perform_comp <- utils::getFromNamespace("perform_comp", ns = "bage")
    perform_aug <- utils::getFromNamespace("perform_aug", ns = "bage")
    outcome <- outcome_obs_sim[, i_sim]
    mod_est$outcome <- outcome
    mod_est <- set_seeds(mod_est)
    mod_est <- fit(mod_est, method = method, vars_inner = vars_inner)
    comp_est <- components(mod_est)
    aug_est <- augment(mod_est, quiet = TRUE)
    results_comp <- perform_comp(est = comp_est,
                                 sim = comp_sim,
                                 prior_class_est = prior_class_est,
                                 prior_class_sim = prior_class_sim,
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
                                  comp_sim = comp_sim,
                                  prior_class_est = prior_class_est,
                                  prior_class_sim = prior_class_sim)
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
#' Convert a List of Simulated Covariates to a Data Frame
#'
#' @param vals_covariates A named list of matrices
#'
#' @returns A tibble with columns 'component'
#' 'term', 'level', and '.fitted'
#' 
#' @noRd
vals_covariates_to_dataframe <- function(vals_covariates) {
  nms <- names(vals_covariates)
  nrow <- vapply(vals_covariates, nrow, 0L)
  term <- rep("covariates", times = sum(nrow))
  component <- rep("coef", times = sum(nrow))
  level <- lapply(vals_covariates, rownames)
  level <- unlist(level, use.names = FALSE)
  .fitted <- do.call(rbind, vals_covariates)
  .fitted <- unname(.fitted)
  .fitted <- rvec::rvec(.fitted)
  tibble::tibble(term = term,
                 component = component,
                 level = level,
                 .fitted = .fitted)
}

## HAS_TESTS
#' Convert Simulated Values for 'datamod' to a Data Frame
#'
#' @param vals_data A matrix
#' @param datamod Object of class 'bage_datamod'
#'
#' @returns A tibble with columns 'component'
#' 'term', 'level', and '.fitted'
#' 
#' @noRd
vals_datamod_to_dataframe <- function(vals_datamod, datamod) {
  term <- rep("datamod", times = nrow(vals_datamod))
  component <- make_datamod_comp(datamod)
  level <- make_level_datamod(datamod)
  .fitted <- rvec::rvec_dbl(vals_datamod)
  tibble::tibble(term = term,
                 component = component,
                 level = level,
                 .fitted = .fitted)
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


## 'vals_hyper_to_dataframe' --------------------------------------------------

## HAS_TESTS
#' Convert List of 'vals_hyper' into a Data Frame
#'
#' @param mod Object of class 'bage_mod'
#' @param vals_hyper Named list
#' @param n_sim Number of draws
#'
#' @returns A tibble.
#'
#' @noRd
vals_hyper_to_dataframe <- function(mod, vals_hyper, n_sim) {
  nms <- names(vals_hyper)
  ans <- .mapply(vals_hyper_to_dataframe_one,
                 dots = list(nm = nms,
                             vals_hyper = vals_hyper),
                 MoreArgs = list(n_sim = n_sim))
  ans <- vctrs::vec_rbind(!!!ans)
  ans
}


## 'vals_hyper_to_dataframe_one' ----------------------------------------------

## HAS_TESTS
#' Convert One 'vals_hyper' into a Data Frame
#'
#' @param nm String
#' @param vals_hyper Named list
#' @param n_sim Integer
#'
#' @returns A tibble.
#'
#' @noRd
vals_hyper_to_dataframe_one <- function(nm, vals_hyper, n_sim) {
  vals <- vctrs::vec_rbind(!!!vals_hyper, .name_repair = "universal_quiet")
  if (nrow(vals) > 0L) {
    vals <- as.matrix(vals)
    dimnames(vals) <- NULL
  }
  else
    vals <- matrix(NA_real_, nrow = 0L, ncol = n_sim)
  term <- rep(nm, times = nrow(vals))
  component <- rep.int("hyper", times = nrow(vals))
  if (nrow(vals) > 0L) {
    level <- lapply(vals_hyper, rownames)
    no_rownames <- vapply(level, is.null, FALSE)
    level[no_rownames] <- names(vals_hyper[no_rownames])
    level <- unlist(level, use.names = FALSE)
  }
  else
    level <- character()
  .fitted <- rvec::rvec(vals)
  tibble::tibble(term = term,
                 component = component,
                 level = level,
                 .fitted = .fitted)
}


## 'vals_hyperrand_to_dataframe' ----------------------------------------------

## HAS_TESTS
#' Convert List of 'vals_hyperrand' into a Data Frame
#'
#' @param mod Object of class 'bage_mod'
#' @param vals_hyperrand Named list
#' @param n_sim Number of draws
#'
#' @returns A tibble.
#'
#' @noRd
vals_hyperrand_to_dataframe <- function(mod, vals_hyperrand, n_sim) {
  priors <- mod$priors
  dimnames_term <- mod$dimnames_term
  var_age <- mod$var_age
  var_time <- mod$var_time
  nms <- names(vals_hyperrand)
  ans <- .mapply(vals_hyperrand_to_dataframe_one,
                 dots = list(prior = priors,
                             vals_hyperrand = vals_hyperrand,
                             dimnames_term = dimnames_term),
                 MoreArgs = list(var_age = var_age,
                                 var_time = var_time,
                                 n_sim = n_sim))
  ans <- vctrs::vec_rbind(!!!ans)
  ans
}


## 'vals_hyperrand_to_dataframe_one' ------------------------------------------

## HAS_TESTS
#' Convert One 'vals_hyperrand' into a Data Frame
#'
#' @param nm String
#' @param vals_hyperrand Named list
#' @param n_sim Integer
#'
#' @returns A tibble.
#'
#' @noRd
vals_hyperrand_to_dataframe_one <- function(prior,
                                            vals_hyperrand,
                                            dimnames_term,
                                            var_age,
                                            var_time,
                                            n_sim) {
  nm <- dimnames_to_nm(dimnames_term)
  vals <- vctrs::vec_rbind(!!!vals_hyperrand, .name_repair = "universal_quiet")
  if (nrow(vals) > 0L) {
    vals <- as.matrix(vals)
    dimnames(vals) <- NULL
  }
  else
    vals <- matrix(NA_real_, nrow = 0L, ncol = n_sim)
  term <- rep(nm, times = nrow(vals))
  component <- comp_hyperrand(prior = prior,
                              dimnames_term = dimnames_term,
                              var_age = var_age,
                              var_time = var_time)
  if (nrow(vals) > 0L) {
    level <- lapply(vals_hyperrand, rownames)
    no_rownames <- vapply(level, is.null, FALSE)
    level[no_rownames] <- names(vals_hyperrand[no_rownames])
    level <- unlist(level, use.names = FALSE)
  }
  else
    level <- character()
  .fitted <- rvec::rvec(vals)
  tibble::tibble(term = term,
                 component = component,
                 level = level,
                 .fitted = .fitted)
}


## 'vals_spline_to_dataframe' -------------------------------------------------

## HAS_TESTS
#' Convert List of 'vals_spline' into a Data Frame
#'
#' @param mod Object of class 'bage_mod'
#' @param vals_spline Named list
#' @param n_sim Number of draws
#'
#' @returns A tibble.
#'
#' @noRd
vals_spline_to_dataframe <- function(mod, vals_spline, n_sim) {
  nms <- names(vals_spline)
  ans <- .mapply(vals_spline_to_dataframe_one,
                 dots = list(nm = nms,
                             vals_spline = vals_spline),
                 MoreArgs = list(n_sim = n_sim))
  ans <- vctrs::vec_rbind(!!!ans)
  ans
}


## 'vals_spline_to_dataframe_one' ---------------------------------------------

## HAS_TESTS
#' Convert One 'vals_spline' into a Data Frame
#'
#' @param vals_hyper Matrix
#' @param nm String
#' @param n_sim Integer
#'
#' @returns A tibble.
#'
#' @noRd
vals_spline_to_dataframe_one <- function(vals_spline, nm, n_sim) {
  if (!is.null(vals_spline) && nrow(vals_spline) > 0L) {
    vals <- vals_spline
    dimnames(vals) <- NULL
    level <- rownames(vals_spline)
  }
  else {
    vals <- matrix(NA_real_, nrow = 0L, ncol = n_sim)
    level <- character()
  }
  term <- rep(nm, times = nrow(vals))
  component <- rep.int("spline", times = nrow(vals))
  .fitted <- rvec::rvec(vals)
  tibble::tibble(term = term,
                 component = component,
                 level = level,
                 .fitted = .fitted)
}


## 'vals_svd_to_dataframe' ----------------------------------------------------

## HAS_TESTS
#' Convert List of 'vals_svd' into a Data Frame
#'
#' @param mod Object of class 'bage_mod'
#' @param vals_svd Named list
#' @param n_sim Number of draws
#'
#' @returns A tibble.
#'
#' @noRd
vals_svd_to_dataframe <- function(mod, vals_svd, n_sim) {
  nms <- names(vals_svd)
  ans <- .mapply(vals_svd_to_dataframe_one,
                 dots = list(nm = nms,
                             vals_svd = vals_svd),
                 MoreArgs = list(n_sim = n_sim))
  ans <- vctrs::vec_rbind(!!!ans)
  ans
}


## 'vals_svd_to_dataframe_one' ------------------------------------------------

## HAS_TESTS
#' Convert One 'vals_svd' into a Data Frame
#'
#' @param vals_hyper Matrix
#' @param nm String
#' @param n_sim Integer
#'
#' @returns A tibble.
#'
#' @noRd
vals_svd_to_dataframe_one <- function(vals_svd, nm, n_sim) {
  if (!is.null(vals_svd) && nrow(vals_svd) > 0L) {
    vals <- vals_svd
    dimnames(vals) <- NULL
    level <- rownames(vals_svd)
  }
  else {
    vals <- matrix(NA_real_, nrow = 0L, ncol = n_sim)
    level <- character()
  }
  term <- rep(nm, times = nrow(vals))
  component <- rep.int("svd", times = nrow(vals))
  .fitted <- rvec::rvec(vals)
  tibble::tibble(term = term,
                 component = component,
                 level = level,
                 .fitted = .fitted)
}
