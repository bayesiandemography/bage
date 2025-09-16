
## HAS_TESTS
#' Density for Beta-Binomial Distribution
#'
#' @param x Vector of quantiles
#' @param size Vector of numbers of trials
#' @param shape1,shape2 Beta parameters. Vectors
#' @param log Whether to return log of density
#'
#' @returns A numeric vector
#'
#' @noRd
dbetabinom <- function(x, size, shape1, shape2, log = FALSE) {
  args <- list(x, size, shape1, shape2)
  n <- max(lengths(args))
  x <- rep_len(x, n)
  size <- rep_len(size, n)
  shape1 <- rep_len(shape1, n)
  shape2 <- rep_len(shape2, n)
  is_ok <- ((x >= 0) &
              (x <= size) &
              (x == floor(x)) & 
              is.finite(x) &
              is.finite(size) &
              is.finite(shape1) &
              is.finite(shape2) &
              (shape1 > 0) &
              (shape2 > 0) &
              (size == floor(size)) &
              (size >= 0))
  log_dens <- rep.int(NA_real_, times = n)
  log_dens[is_ok] <- (lchoose(n = size[is_ok], k = x[is_ok])
    + lbeta(a = x[is_ok] + shape1[is_ok],
            b = size[is_ok] - x[is_ok] + shape2[is_ok])
    - lbeta(a = shape1[is_ok],
            b = shape2[is_ok]))
  if (log) log_dens else exp(log_dens)
}


## HAS_TESTS
#' Insert a Variable Into a Data Frame,
#' After Another Variable
#'
#' @param df A data frame (including a tibble)
#' @param nm_after Name of the variable that the
#' 'x' should come after
#' @param x New variable
#' @param nm_x Name of new variable
#'
#' @returns A modified version of 'df'
#'
#' @noRd
insert_after <- function(df, nm_after, x, nm_x) {
  nms_df <- names(df)
  n_df <- length(nms_df)
  i_after <- match(nm_after, names(df))
  if (i_after < n_df) {
    s_before <- seq_len(i_after)
    s_after <- seq.int(from = i_after + 1L, to = n_df)
    ans <- vctrs::vec_cbind(df[s_before],
                            x,
                            df[s_after],
                            .name_repair = "universal_quiet")
  }
  else {
    ans <- vctrs::vec_cbind(df,
                            x,
                            .name_repair = "universal_quiet")
  }
  names(ans)[[i_after + 1L]] <- nm_x
  ans
}


## HAS_TESTS
#' Insert a Variable Into a Data Frame,
#' Before Another Variable
#'
#' @param df A data frame (including a tibble)
#' @param nm_before Name of the variable that the
#' 'x' should come before
#' @param x New variable
#' @param nm_x Name of new variable
#'
#' @returns A modified version of 'df'
#'
#' @noRd
insert_before <- function(df, nm_before, x, nm_x) {
  nms_df <- names(df)
  n_df <- length(nms_df)
  i_before <- match(nm_before, names(df))
  if (i_before > 1L) {
    s_before <- seq_len(i_before - 1L)
    s_after <- seq.int(from = i_before, to = n_df)
    ans <- vctrs::vec_cbind(df[s_before],
                            x,
                            df[s_after],
                            .name_repair = "universal_quiet")
  }
  else {
    ans <- vctrs::vec_cbind(x,
                            df,
                            .name_repair = "universal_quiet")
  }
  names(ans)[[i_before]] <- nm_x
  ans
}


## HAS_TESTS
#' Check Whether Currently in Test or Snapshot
#'
#' Based on testthat::is_testing() and testthat::is_snapshot()
#' 
#' @returns TRUE or FALSE
#'
#' @noRd
is_not_testing_or_snapshot <- function() {
  is_testing <- identical(Sys.getenv("TESTTHAT"), "true")
  is_snapshot <- identical(Sys.getenv("TESTTHAT_IS_SNAPSHOT"), "true")
  !is_testing && !is_snapshot
}


## HAS_TESTS
#' Test Whether Two Objects Have the Same Class
#'
#' @param x,y Objects
#'
#' @returns TRUE or FALSE
#'
#' @noRd
is_same_class <- function(x, y)
  identical(class(x)[[1L]], class(y)[[1L]])

## HAS_TESTS
#' Use a precision matrix to construct scaled eigen vectors
#'
#' The scaled eigen vectors can be used to draw from a
#' multivariate normal distribution with the given
#' pecision matrix.
#'
#' @param prec A symmetric positive definite matrix.
#'
#' @returns A matrix with the same dimensions as 'prec'
#'
#' @noRd
make_scaled_eigen <- function(prec) {
    tolerance <- 1e-6
    eigen <- eigen(prec, symmetric = TRUE)
    vals <- eigen$values
    vecs <- eigen$vectors
    min_valid_val <- -tolerance * abs(vals[[1L]]) ## based on MASS::mvrnorm
    if (any(vals < min_valid_val))
        cli::cli_abort("Estimated precision matrix not positive definite.")  ## nocov
    vals <- pmax(vals, abs(min_valid_val))
    sqrt_inv_vals <- sqrt(1 / vals)
    vecs %*% diag(sqrt_inv_vals)
}


#' Paste Two Vectors Separated by a Dot
#'
#' @param x A vector
#' @param y A vector
#'
#' @returns A character vector
#'
#' @noRd
paste_dot <- function(x, y) paste(x, y, sep = ".")


## HAS_TESTS
#' Version of 'rpois' With Upper Limit on size * prob
#'
#' Binomial can have numerical problems
#' and valgrind errors with very large size * prob, so switch
#' to just setting random variate to size * prob, above a
#' given threshold. Warn the user that this is happening.
#'
#' Assume that length(size) == length(prob)
#' (Which may mean length(as.numeric(size))
#'   != length(as.numeric(prob)))
#'
#' @param size Trials. A numeric vector
#' or an rvec.
#' @param prob Probability of success. A
#' numeric vector or an rvec.
#'
#' @returns A numeric vector or an rvec
#'
#' @noRd
rbinom_guarded <- function(size, prob) {
  threshold <- 1e8
  if (!identical(length(size), length(prob)))
    cli::cli_abort("Internal error: size and prob have different lengths.")
  is_rvec_size <- rvec::is_rvec(size)
  is_rvec_prob <- rvec::is_rvec(prob)
  has_rvec <- is_rvec_size || is_rvec_prob
  if (has_rvec) {
    if (is_rvec_size) {
      n_val <- length(size) 
      n_draw <- rvec::n_draw(size)
    }
    else {
      n_val <- length(prob)
      n_draw <- rvec::n_draw(prob)
    }
    if (is_rvec_size)
      size <- as.numeric(size)
    else
      size <- rep(size, times = n_draw)
    if (is_rvec_prob)
      prob <- as.numeric(prob)
    else
      prob <- rep(prob, times = n_draw)
  }
  mean <- size * prob
  is_gt <- !is.na(mean) & (mean > threshold)
  n_gt <- sum(is_gt)
  if (n_gt > 0L) {
    pc <- 100 * mean(is_gt)
    pc <- signif(pc, digits = 2)
    cli::cli_warn(c("Large values for {.arg size} * {.arg prob} used to generate binomial variates.",
                    i = "{.val {pc}} percent of values exceed {.val {threshold}}.",
                    i = "Using deterministic approximation to generate variates for these values."))
  }
  ans <- mean
  is_lt <- !is_gt
  ans[is_lt] <- stats::rbinom(n = sum(is_lt),
                              size = size[is_lt],
                              prob = prob[is_lt])
  
  if (has_rvec) {
    ans <- matrix(ans, nrow = n_val, ncol = n_draw)
    ans <- rvec::rvec_dbl(ans)
  }
  ans
}


## HAS_TESTS
#' Draw from multivariate normal, using results
#' from a Cholesky decomposition
#'
#' @param n Number of draws
#' @param mean Mean of distribution
#' @param R_prec Cholesky decomposition of precision matrix
#'
#' @returns A matrix, with each columns being one draw
#'
#' @noRd
rmvnorm_chol <- function(n, mean, R_prec) {
    n_val <- length(mean)
    Z <- matrix(stats::rnorm(n = n_val * n),
                nrow = n_val,
                ncol = n)
    mean + backsolve(R_prec, Z)
}


## HAS_TESTS
#' Draw from multivariate normal, using results
#' from an eigen decomposition
#'
#' @param n Number of draws
#' @param mean Mean of distribution
#' @param scaled_eigen Matrix of scaled eigenvalues
#'
#' @returns A matrix, with each columns being one draw
#'
#' @noRd
rmvnorm_eigen <- function(n, mean, scaled_eigen) {
    n_val <- length(mean)
    Z <- matrix(stats::rnorm(n = n_val * n),
                nrow = n_val,
                ncol = n)
    mean + scaled_eigen %*% Z
}


## HAS_TESTS
#' Version of 'nbinom' With Upper Limit on Mean
#'
#' Negative binomial can have numerical problems
#' and valgrind errors with very large lambda, so switch
#' to just setting random variate to lambda, above a
#' given threshold. Warn the user that this is happening.
#'
#' @param lambda Expected values. A numeric vector
#' or an rvec.
#'
#' @returns A numeric vector or an rvec
#'
#' @noRd
rpois_guarded <- function(lambda) {
  threshold <- 1e8
  is_rvec <- rvec::is_rvec(lambda)
  if (is_rvec) {
    n_val <- length(lambda)
    n_draw <- rvec::n_draw(lambda)
    lambda <- as.numeric(lambda)
  }
  is_gt <- !is.na(lambda) & (lambda > threshold)
  n_gt <- sum(is_gt)
  if (n_gt > 0L) {
    pc <- 100 * mean(is_gt)
    pc <- signif(pc, digits = 2)
    cli::cli_warn(c("Large values for {.arg lambda} used to generate Poisson variates.",
                    i = "{.val {pc}} percent of values for {.arg lambda} are above {.val {threshold}}.",
                    i = "Using deterministic approximation to generate variates for these values."))
  }
  ans <- lambda
  is_lt <- !is_gt
  ans[is_lt] <- stats::rpois(n = sum(is_lt), lambda = lambda[is_lt])
  if (is_rvec) {
    ans <- matrix(ans, nrow = n_val, ncol = n_draw)
    ans <- rvec::rvec_dbl(ans)
  }
  ans
}


## HAS_TESTS
#' Version of 'rpois' With Upper Limit on Lambda
#'
#' Poisson can have numerical problems
#' and valgrind errors with very large lambda, so switch
#' to just setting random variate to lambda, above a
#' given threshold. Warn the user that this is happening.
#'
#' @param lambda Expected values. A numeric vector
#' or an rvec.
#'
#' @returns A numeric vector or an rvec
#'
#' @noRd
rpois_guarded <- function(lambda) {
  threshold <- 1e8
  is_rvec <- rvec::is_rvec(lambda)
  if (is_rvec) {
    n_val <- length(lambda)
    n_draw <- rvec::n_draw(lambda)
    lambda <- as.numeric(lambda)
  }
  is_gt <- !is.na(lambda) & (lambda > threshold)
  n_gt <- sum(is_gt)
  if (n_gt > 0L) {
    pc <- 100 * mean(is_gt)
    pc <- signif(pc, digits = 2)
    cli::cli_warn(c("Large values for {.arg lambda} used to generate Poisson variates.",
                    i = "{.val {pc}} percent of values for {.arg lambda} are above {.val {threshold}}.",
                    i = "Using deterministic approximation to generate variates for these values."))
  }
  ans <- lambda
  is_lt <- !is_gt
  ans[is_lt] <- stats::rpois(n = sum(is_lt), lambda = lambda[is_lt])
  if (is_rvec) {
    ans <- matrix(ans, nrow = n_val, ncol = n_draw)
    ans <- rvec::rvec_dbl(ans)
  }
  ans
}


#' Draw from Posterior Distribution of True Values
#' Given Observed Values, for Poisson plus Symmetric Skellam
#'
#' @param y_obs Observed value
#' @param lambda Expected value
#' @param m Mu for symmetric Skellam
#'
#' @returns A single draw
#'
#' @noRd
draw_true_given_obs_pois_skellam <- function(y_obs, lambda, m) {
  threshold <- 50
  window_sd <- 8L
  p0_thresh <- 0.01
  if ((lambda < threshold) && (m < threshold))
    draw_true_given_obs_pois_skellam_exact(y_obs = y_obs,
                                           lambda = lambda,
                                           m = m,
                                           window_sd = window_sd)
  else
    draw_true_given_obs_pois_skellam_approx(y_obs = y_obs,
                                            lambda = lambda,
                                            m = m,
                                            window_sd = window_sd,
                                            p0_thresh = p0_thresh)
}



#' Draw from Posterior Distribution of True Values
#' Given Observed Values, for Poisson plus Symmetric Skellam -
#' approx version
#
# Gaussian posterior from linear-Gaussian approximation:
#' X ~ N(lambda, lambda), U ~ N(0, 2m), Y = X + U
#' 
#' @param y_obs Observed value
#' @param lambda Expected value
#' @param m Mu for symmetric Skellam
#' @param window_sd Width of window, in SDs
#' @param p0_thresh If approx Pr(y_true=0) > p0_threshold, use windowing
#'
#' @returns A single draw
#'
#' @noRd
draw_true_given_obs_pois_skellam_approx <- function(y_obs,
                                                    lambda,
                                                    m,
                                                    window_sd,
                                                    p0_thresh) {
  mu_post  <- lambda + (lambda / (lambda + 2 * m)) * (y_obs - lambda)
  var_post <- (lambda * 2 * m) / (lambda + 2 * m)
  sd_post  <- sqrt(var_post)
  ## heuristics for choosing discrete window vs truncnorm + rounding
  near_boundary <- (mu_post < 3 * sd_post)
  p0_approx <- pnorm(0.5, mean = mu_post, sd = sd_post) -
    pnorm(-0.5, mean = mu_post, sd = sd_post)
  p0_above_threshold <- p0_approx > p0_thresh
  need_window <- near_boundary || p0_above_threshold
  if (!need_window) {
    # truncnorm + rounding
    u <- runif(n = 1L)
    alpha <- (0 - mu_post) / sd_post
    p0_trunc <- pnorm(alpha)
    z <- qnorm(p0_trunc + (1 - p0_trunc) * u)
    ans <- floor(mu_post + sd_post * z + 0.5)
    ans <- max(ans, 0L)
  }
  else {
    ## window around mean
    L <- floor(mu_post - window_sd * sd_post)
    L <- max(0L, L)
    R <- ceiling(mu_post + window_sd * sd_post)
    R <- max(L, R)
    y_trues <- L:R
    lw <- dnorm(y_trues, mean = mu_post, sd = sd_post, log = TRUE)
    M <- max(lw)
    prob <- exp(lw - M)
    ans <- sample(y_trues, size = 1L, prob = prob)
  }
  ans
}


#' Draw from Posterior Distribution of True Values
#' Given Observed Values, for Poisson plus Symmetric Skellam
#' - Exact, Small Sample
#'
#' Assume lambda and m both less than 50
#'
#' @param y_obs Observed value
#' @param lambda Expected value
#' @param m Mu for symmetric Skellam
#' @param window_sd Width of window, in SDs
#'
#' @returns A single draw
#'
#' @noRd
draw_true_given_obs_pois_skellam_exact <- function(y_obs,
                                                   lambda,
                                                   m,
                                                   window_sd) {
  ## --- Choose a fixed window around y_obs ---
  ## rough posterior scale: Var(y_obs) \approx lambda + 2 * m
  sd_y_obs <- sqrt(lambda + 2 * m)
  ## window boundaries
  L <- floor(y_obs - window_sd * sd_y_obs)
  L <- max(0L, L)
  R <- max(y_obs + window_sd * sd_y_obs,
           lambda + window_sd * sqrt(lambda + 1))
  R <- ceiling(R)
  R <- max(L + 1L, R)
  y_trues <- L:R
  ## stable log Bessel I_{|y_obs - y_true|}(2m): scaled besselI + 2m
  nu <- abs(y_obs - y_trues)
  logI <- log(besselI(2 * m, nu = nu, expon.scaled = TRUE)) + 2 * m
  ## Unnormalized log posterior weights
  logw <- y_trues * log(lambda) - lgamma(y_trues + 1) + logI
  ## subtract max before exponentiating
  M <- max(logw)
  prob <- exp(logw - M)
  # draw single value of y_true
  sample(y_trues, size = 1L, prob = prob)
}


## HAS_TESTS
#' Convert Rvec Columns to Numeric Columns by Taking Means
#'
#' @param data A data frame
#'
#' @return A data frame
#'
#' @noRd
rvec_to_mean <- function(data) {
  is_rvec <- vapply(data, rvec::is_rvec, TRUE)
  data[is_rvec] <- lapply(data[is_rvec], rvec::draws_mean)
  data
}



## HAS_TESTS
#' Obtain a Draws from the Posterior
#' of a Beta-Binomial Model with Binomial
#' Measurement Error
#'
#' Obtain draws of \eqn{x | n, \mu, \xi, \pi}
#' from the model
#'
#' \deqn{x \sim \text{BetaBinom}(n, \mu/\xi (1-\mu)/\xi)}
#' \deqn{y \sim \text{Binomial}(x, \pi)}.
#'
#' That is, draw \eqn{x}, where
#' deqn{p(x | n, y, mu, xi, pi) \propto \frac{(1-pi)^x B(x+mu/xi, n-x+(1-mu)/xi)}{(x-y)!(n-x)!}}
#' for \eqn{x = y, \cdots, n}.
#'
#' @param n The total number of trials. Numeric vector.
#' @param y The observed number of successes. 
#' Vector with same length as 'n'
#' @param mu The probability of a success.
#' Vector with same length as 'n'
#' @param xi Dispersion.
#' Vector of positive reals with same length as 'n'
#' @param pi The probability of detection.
#' Vector of reals between 0 and 1 with same length as 'n'
#'
#' @returns An integer
#'
#' @noRd
sample_post_binom_betabinom <- function(n, y, mu, xi, pi) {
  stopifnot(is.numeric(n),
            all(n >= 0, na.rm = TRUE),
            !any(is.infinite(n), na.rm = TRUE),
            all(round(n) == n, na.rm = TRUE))
  stopifnot(identical(length(y), length(n)),
            is.numeric(y),
            all(y >= 0, na.rm = TRUE),
            all(round(y) == y, na.rm = TRUE),
            all(y <= n, na.rm = TRUE))
  stopifnot(identical(length(mu), length(n)),
            is.numeric(mu),
            all(mu >= 0, na.rm = TRUE),
            all(mu <= 1, na.rm = TRUE))
  stopifnot(identical(length(xi), length(n)),
            is.numeric(xi),
            all(xi > 0, na.rm = TRUE))
  stopifnot(identical(length(pi), length(n)),
            is.numeric(pi),
            all(pi >= 0, na.rm = TRUE),
            all(pi <= 1, na.rm = TRUE))
  ans <- numeric(length = length(n))
  for (i in seq_along(ans)) {
    ans[[i]] <- sample_post_binom_betabinom_inner(n = n[[i]],
                                                  y = y[[i]],
                                                  mu = mu[[i]],
                                                  xi = xi[[i]],
                                                  pi = pi[[i]])
  }
  ans
}



## HAS_TESTS
#' Obtain a Single Draw from the Posterior
#' of a Beta-Binomial Model with Binomial
#' Measurement Error
#'
#' Obtain a single draw of \eqn{x | n, \mu, \xi, \pi}
#' from the model
#'
#' \deqn{x \sim \text{BetaBinom}(n, \mu/\xi (1-\mu)/\xi)}
#' \deqn{y \sim \text{Binomial}(x, \pi)}.
#'
#' That is, draw \eqn{x}, where
#' deqn{p(x | n, y, mu, xi, pi) \propto \frac{(1-pi)^x B(x+mu/xi, n-x+(1-mu)/xi)}{(x-y)!(n-x)!}}
#' for \eqn{x = y, \cdots, n}.
#'
#' @param n The total number of trials. An integer.
#' @param y The observed number of successes. An integer.
#' @param mu The probability of a success. A real between 0 and 1.
#' @param xi Dispersion. A positive real.
#' @param pi The probability of detection. A real between 0 and 1.
#'
#' @returns An integer
#'
#' @noRd
sample_post_binom_betabinom_inner <- function(n, y, mu, xi, pi) {
  if (is.na(n) || is.na(y) || is.na(mu) || is.na(xi) || is.na(pi))
    return(NA_real_)
  if (y == n)
    return(y)
  x <- seq.int(from = y, to = n)
  num1 <- x * log(1 - pi)
  num2 <- lbeta(x + mu / xi, n - x + (1 - mu) / xi)
  den1 <- lfactorial(x - y)
  den2 <- lfactorial(n - x)
  log_wt <- num1 + num2 - den1 - den2
  log_wt <- log_wt - max(log_wt)
  wt <- exp(log_wt)
  sample(x, size = 1L, prob = wt)
}

  
                                              
  



