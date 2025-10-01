
## 'chr_to_int' ---------------------------------------------------------------

test_that("'chr_to_int' works with valid inputs", {
  df <- data.frame(a = 1,
                   b = "1",
                   c = "a",
                   d = factor("a"),
                   e = factor("-1"))
  ans_obtained <- chr_to_int(df)
  ans_expected <- data.frame(a = 1,
                             b = 1L,
                             c = "a",
                             d = factor("a"),
                             e = -1L)
  expect_identical(ans_obtained, ans_expected)
})

test_that("'chr_to_int' works with empty df", {
  df <- data.frame()
  ans_obtained <- chr_to_int(df)
  ans_expected <- df
  expect_identical(ans_obtained, ans_expected)
})


## 'dbetabinom' ---------------------------------------------------------------

test_that("basic correctness for known value", {
  # P(X = 1 | n = 2, alpha = 1, shape2 = 1) = choose(2,1) * B(2,2)/B(1,1) = 2 * 1/6 = 1/3
  expect_equal(dbetabinom(1, 2, 1, 1), 1/3, tolerance = 1e-10)
})

test_that("vectorization over x", {
  probs <- dbetabinom(0:2, 2, 1, 1)
  expect_equal(length(probs), 3)
  expect_equal(sum(probs), 1, tolerance = 1e-10)
})

test_that("vectorization over size", {
  x <- 0:2
  size <- 2:4
  probs <- dbetabinom(x, size, shape1 = 1, shape2 = 1)
  expect_equal(length(probs), 3)
  expect_true(all(probs >= 0 & probs <= 1))
})

test_that("vectorization over shape1 and shape2", {
  x <- c(1, 1, 1)
  size <- c(2, 2, 2)
  shape1 <- c(1, 2, 3)
  shape2 <- c(1, 2, 3)
  probs <- dbetabinom(x, size, shape1, shape2)
  expect_equal(length(probs), 3)
  expect_true(all(probs >= 0 & probs <= 1))
})

test_that("log = TRUE is consistent with log(prob)", {
  log_p <- dbetabinom(3, 10, 2, 5, log = TRUE)
  p <- dbetabinom(3, 10, 2, 5, log = FALSE)
  expect_equal(log_p, log(p), tolerance = 1e-10)
})

test_that("invalid values return NA", {
  res <- dbetabinom(c(-1, 11, 1.5, NA), size = 10, shape1 = 2, shape2 = 2)
  expect_true(all(is.na(res)))
})

test_that("accepts vector inputs of mixed length (recycling)", {
  x <- c(0, 1, 2, 3)
  size <- 3
  shape1 <- c(1, 2)
  shape2 <- 2
  result <- dbetabinom(x, size, shape1, shape2)
  expect_equal(length(result), 4)
})

test_that("works for x = 0 and x = size", {
  p0 <- dbetabinom(0, 10, 2, 5)
  pN <- dbetabinom(10, 10, 2, 5)
  expect_gt(p0, 0)
  expect_gt(pN, 0)
})


## 'dskellam' -----------------------------------------------------------------

test_that("'dskellam' degenerate cases: both mus are zero -> point mass at 0", {
  ans_obtained <- dskellam(0, mu1 = 0, mu2 = 0)
  ans_expected <- 1
  expect_equal(ans_obtained, ans_expected)
})

test_that("'dskellam' degenerate cases: one-sided Poisson when one mu is zero", {
  mu1 <- rep(0, 5)
  mu2 <- rep(3, 5)
  x <- -5:(-1)
  ans_obtained <- dskellam(x, mu1, mu2)
  ans_expected <- dpois(5:1, 3)
  expect_equal(ans_obtained, ans_expected)
})

test_that("'dskellam' degenerate cases: one-sided Poisson when one mu is zero", {
  mu1 <- rep(3, 5)
  mu2 <- rep(0, 5)
  x <- 1:5
  ans_obtained <- dskellam(x, mu1, mu2)
  ans_expected <- dpois(1:5, 3)
  expect_equal(ans_obtained, ans_expected)
})

test_that("'dskellam' basic properties: non-negativity and finite probabilities", {
  set.seed(1)
  x   <- sample(-50:50, size = 101, replace = TRUE)
  mu1 <- runif(101, min = 0, max = 20)
  mu2 <- runif(101, min = 0, max = 20)
  p <- dskellam(x, mu1, mu2)
  expect_true(all(is.finite(p)))
  expect_true(all(p >= 0))
  expect_true(all(p <= 1))
})

test_that("'dskellam' symmetry when mu1 == mu2", {
  mu <- 2.5
  x  <- -30:30
  p1 <- dskellam(x,  mu, mu)
  p2 <- dskellam(-x, mu, mu)
  expect_equal(p1, p2, tolerance = 1e-12)
})

test_that("'dskellam' matches closed-form for small (exact) regime", {
  # For small mu and moderate |x|, compare to Bessel-based formula
  mu1 <- c(0.8, 1.2, 2.0)
  mu2 <- c(0.7, 0.5, 2.5)
  x   <- c(-3L, 0L, 4L)
  # elementwise
  v  <- 2 * sqrt(mu1 * mu2)
  nu <- abs(x)
  # Use scaled Bessel: I_nu(v) = besselI(v, nu, TRUE) * exp(v)
  logp_ref <- -(mu1 + mu2) + 0.5 * x * (log(mu1) - log(mu2)) +
              log(besselI(v, nu, expon.scaled = TRUE)) + v
  pref <- exp(logp_ref)
  p <- dskellam(x, mu1, mu2)
  expect_equal(p, pref, tolerance = 1e-10)
})

test_that("'dskellam' vectorization: elementwise equals scalar calls", {
  x   <- -5:5
  mu1 <- rep(3, length(x))
  mu2 <- rep(2, length(x))
  p_vec <- dskellam(x, mu1, mu2)
  p_sca <- vapply(seq_along(x),
                  function(i) dskellam(x[i], mu1[i], mu2[i]),
                  numeric(1))
  expect_equal(p_vec, p_sca, tolerance = 0)  # exact identity expected
})

test_that("'dskellam' no NA/NaN around method-switch thresholds", {
  # Around internal thresholds: mu1+mu2 ~ 5, |x| ~ 30
  mu1 <- c(2.4, 2.6, 4.9, 5.1)
  mu2 <- c(2.6, 2.4, 0.1, -0)  # non-negative, last zero
  x   <- c(-31L, -29L, 30L, 31L)
  p <- dskellam(x, mu1, mu2)
  expect_true(all(is.finite(p)))
  expect_true(all(p >= 0))
})


## 'dskellam_approx' -----------------------------------------------------------

test_that("'dskellam_approx' works with large values", {
  set.seed(0)
  mu1 <- rep(30, 100)
  mu2 <- rep(10, 100)
  x <- rpois(n = 100, lambda = mu1) -
    rpois(n = 100, lambda = mu2)
  ans_obtained <- dskellam_approx(x = x, mu1 = mu1, mu2 = mu2)
  ans_expected <- exp(-mu1 - mu2) * (mu1/mu2)^(x/2) * besselI(2*sqrt(mu1*mu2),
                                                              abs(x))
  expect_equal(ans_obtained, ans_expected, tolerance = 0.01)
})


## 'dskellam_exact' -----------------------------------------------------------

test_that("'dskellam_exact' works with small values", {
  set.seed(0)
  mu1 <- rep(3, 100)
  mu2 <- rep(0.5, 100)
  x <- rpois(n = 100, lambda = mu1) -
    rpois(n = 100, lambda = mu2)
  ans_obtained <- dskellam_exact(x = x, mu1 = mu1, mu2 = mu2)
  ans_expected <- exp(-mu1 - mu2) * (mu1/mu2)^(x/2) * besselI(2*sqrt(mu1*mu2),
                                                              abs(x))
  expect_equal(ans_obtained, ans_expected)
})


## 'draw_true_given_obs_pois_skellam ------------------------------------------

test_that("'draw_true_given_obs_pois_skellam' returns single nonnegative integer", {
  set.seed(1)
  ans <- draw_true_given_obs_pois_skellam(y_obs = 10, lambda = 12, m = 15)
  expect_length(ans, 1L)
  expect_true(is.finite(ans))
  expect_true(is.integer(ans) || (is.numeric(ans) && ans == as.integer(ans)))
  expect_gte(ans, 0L)
})

test_that("'draw_true_given_obs_pois_skellam' chooses exact method when both lambda and m below threshold", {
  set.seed(2)
  lambda <- 10
  m <- 20
  y_obs <- 8L
  set.seed(0)
  ans_dispatch <- draw_true_given_obs_pois_skellam(y_obs, lambda, m)
  set.seed(0)
  ans_exact <- draw_true_given_obs_pois_skellam_exact(y_obs, lambda, m, window_sd = 8L)
  expect_identical(ans_dispatch, ans_exact)
})

test_that("'draw_true_given_obs_pois_skellam' chooses approx method when either lambda or m above threshold", {
  set.seed(3)
  # Make lambda large so approx branch is used
  lambda <- 100
  m <- 20
  y_obs <- 60L
  set.seed(0)
  ans_dispatch <- draw_true_given_obs_pois_skellam(y_obs, lambda, m)
  set.seed(0)
  ans_approx <- draw_true_given_obs_pois_skellam_approx(y_obs, lambda, m,
                                                        window_sd = 8L, p0_thresh = 0.01)
  expect_identical(ans_dispatch, ans_approx)
})

test_that("draw_true_given_obs_pois_skellam - negative y_obs handled correctly by truncation", {
  set.seed(4)
  # Negative observed value: final draw should always be >= 0
  ans <- draw_true_given_obs_pois_skellam(y_obs = -5, lambda = 10, m = 20)
  expect_gte(ans, 0L)
})


## 'draw_true_given_obs_pois_skellam_approx' ----------------------------------

test_that("'draw_true_given_obs_pois_skellam_approx' returns one nonnegative integer", {
  set.seed(1)
  ans <- draw_true_given_obs_pois_skellam_approx(
    y_obs = 37, lambda = 40, m = 20, window_sd = 6L, p0_thresh = 0.01
  )
  expect_length(ans, 1L)
  expect_true(is.finite(ans))
  expect_true(is.integer(ans) || (is.numeric(ans) && ans == as.integer(ans)))
  expect_gte(ans, 0L)
})

test_that("'draw_true_given_obs_pois_skellam_approx' works when far from boundary", {
  set.seed(0)
  y_obs <- 1000
  lambda <- 1000
  m <- 800
  mu_post <- lambda + (lambda / (lambda + 2*m)) * (y_obs - lambda)
  sd_post <- sqrt((lambda * 2*m) / (lambda + 2*m))
  draws <- replicate(200,
    draw_true_given_obs_pois_skellam_approx(y_obs, lambda, m, window_sd = 6L, p0_thresh = 0.01)
  )
  expect_true(all(draws >= 0))
  expect_lt(mean(draws == 0L), 0.01)
  expect_lt(abs(mean(draws) - mu_post), 0.2 * sd_post)
})

test_that("'draw_true_given_obs_pois_skellam_approx' works near boundary (mu_post small) or high p0", {
  set.seed(4)
  # Make mu_post small: lambda small, y not too large
  y_obs <- 2
  lambda <- 4
  m <- 20
  # Here mu_post ~ lambda + lambda/(lambda+2m)*(y-lambda) is small, near boundary
  draws <- replicate(400,
    draw_true_given_obs_pois_skellam_approx(y_obs, lambda, m, window_sd = 8L, p0_thresh = 0.05)
  )
  expect_true(all(draws >= 0))
  # Substantial mass near 0 expected
  expect_gt(mean(draws <= 1L), 0.1)
})

test_that("'draw_true_given_obs_pois_skellam_approx' empirical mean tracks Gaussian posterior mean (moderate case)", {
  skip_on_cran()  # Monte Carlo
  set.seed(5)
  y_obs <- 120
  lambda <- 100
  m <- 60
  mu_post <- lambda + (lambda / (lambda + 2*m)) * (y_obs - lambda)
  sd_post <- sqrt((lambda * 2*m) / (lambda + 2*m))
  n <- 5000
  draws <- replicate(n,
    draw_true_given_obs_pois_skellam_approx(y_obs, lambda, m, window_sd = 6L, p0_thresh = 0.01)
  )
  # Sampling error ~ sd / sqrt(n); allow a modest multiple
  tol <- 3 * sd_post / sqrt(n)
  expect_lt(abs(mean(draws) - mu_post), max(tol, 0.15))  # floor tolerance a bit for discreteness
})

test_that("draw_true_given_obs_pois_skellam_approx does not return NA/NaN/Inf in a few edge-y settings", {
  set.seed(6)
  params <- list(
    list(y = 0,   lam = 0.1, m = 0.1),
    list(y = 20,  lam = 5,   m = 40),
    list(y = 200, lam = 150, m = 10),
    list(y = 10,  lam = 300, m = 200)
  )
  for (p in params) {
    ans <- draw_true_given_obs_pois_skellam_approx(
      y_obs = p$y, lambda = p$lam, m = p$m, window_sd = 6L, p0_thresh = 0.02
    )
    expect_true(is.finite(ans))
    expect_gte(ans, 0L)
  }
})

test_that("'draw_true_given_obs_pois_skellam_approx' degenerate branch triggers with tiny sd and integer mean", {
  set.seed(1)
  y_obs <- 10L
  lambda <- 9.9                 # makes mu_post approx y_obs when m is tiny
  m <- 1e-30                    # makes var_post approx 0 (after epsilon floor)
  window_sd <- 8L
  p0_thresh <- 0.01
  # Run several times; if the window branch were non-degenerate it might sample
  # different integers, but in the degenerate case it always returns round(mu_post).
  out <- replicate(20, draw_true_given_obs_pois_skellam_approx(
    y_obs = y_obs, lambda = lambda, m = m,
    window_sd = window_sd, p0_thresh = p0_thresh
  ))
  # Compute the implied mu_post for the same inputs (mirrors the function)
  denom <- lambda + 2 * m
  mu_post  <- lambda + (lambda / denom) * (y_obs - lambda)
  expect_true(all(out == as.integer(round(max(0, mu_post)))),
              info = "Should always fall back to rounded mu_post in degenerate case")
})

test_that("'draw_true_given_obs_pois_skellam_approx' non-degenerate typical case samples around mu_post", {
  set.seed(2)
  y_obs <- 50L
  lambda <- 60
  m <- 40                        # gives a reasonable sd_post
  window_sd <- 8L
  p0_thresh <- 0.01
  out <- replicate(200, draw_true_given_obs_pois_skellam_approx(
    y_obs = y_obs, lambda = lambda, m = m,
    window_sd = window_sd, p0_thresh = p0_thresh
  ))
  expect_true(length(unique(out)) > 1,
              info = "Should not be degenerate; expect variability in draws")
  # sanity: outputs are non-negative integers and near mu_post
  denom <- lambda + 2 * m
  mu_post  <- lambda + (lambda / denom) * (y_obs - lambda)
  var_post <- (lambda * 2 * m) / denom
  sd_post  <- sqrt(max(var_post, .Machine$double.eps))
  expect_true(all(out >= 0))
  expect_true(median(out) >= mu_post - 2 * sd_post &&
              median(out) <= mu_post + 2 * sd_post)
})

test_that("'draw_true_given_obs_pois_skellam_approx' extreme tiny sd but non-integer mean still degenerates", {
  set.seed(3)
  # Make sd_post approx eps by taking lambda tiny and m tiny relative to lambda,
  # but keep mu_post extremely close to an integer + small offset.
  y_obs <- 10L
  lambda <- 10 - 1e-8            
  m <- 1e-30
  window_sd <- 8L
  p0_thresh <- 0.01
  out <- draw_true_given_obs_pois_skellam_approx(
    y_obs = y_obs, lambda = lambda, m = m,
    window_sd = window_sd, p0_thresh = p0_thresh
  )
  denom <- lambda + 2 * m
  mu_post  <- lambda + (lambda / denom) * (y_obs - lambda)
  expect_identical(out, round(max(0, mu_post)),
                   info = "Degenerate guard returns rounded mu_post")
})

test_that("'draw_true_given_obs_pois_skellam_approx' edge guards produce valid integer for m==0 or lambda<=0", {
  set.seed(4)
  y_obs <- 7L
  window_sd <- 8L
  p0_thresh <- 0.01
  out1 <- draw_true_given_obs_pois_skellam_approx(
    y_obs, lambda = 5, m = 0, window_sd, p0_thresh)
  out2 <- draw_true_given_obs_pois_skellam_approx(
    y_obs, lambda = 0, m = 3, window_sd, p0_thresh)
  expect_true(out1 %in% c(0L, y_obs))
  expect_true(out2 %in% c(0L, y_obs))
})


## 'draw_true_given_obs_pois_skellam_exact' -----------------------------------

test_that("'draw_true_given_obs_pois_skellam_exact' returns one nonnegative integer", {
  set.seed(1)
  ans <- draw_true_given_obs_pois_skellam_exact(
    y_obs = 7, lambda = 12, m = 6, window_sd = 8L
  )
  expect_length(ans, 1L)
  expect_true(is.finite(ans))
  expect_true(is.integer(ans) || (is.numeric(ans) && ans == as.integer(ans)))
  expect_gte(ans, 0L)
})

test_that("'draw_true_given_obs_pois_skellam_exact' - empirical posterior mean matches exact posterior mean (small/moderate case)", {
  set.seed(4)
  y_obs  <- 8L
  lambda <- 10
  m      <- 6
  # Build an exact posterior pmf over a generous finite window
  sdY <- sqrt(lambda + 2*m)
  L   <- max(0L, floor(y_obs - 8 * sdY))
  R   <- max(L + 1L, ceiling(max(y_obs + 8 * sdY, lambda + 8 * sqrt(lambda + 1))))
  xs  <- L:R
  # Unnormalized log weights:
  #   log w(x) = x log λ - log(x!) + log I_{|y-x|}(2m)
  nu   <- abs(y_obs - xs)
  logI <- log(besselI(2 * m, nu = nu, expon.scaled = TRUE)) + 2 * m
  logw <- xs * log(lambda) - lgamma(xs + 1) + logI
  M    <- max(logw)
  w    <- exp(logw - M)
  p    <- w / sum(w)
  exact_mean <- sum(xs * p)
  # Draw a bunch and compare means
  n <- 8000
  draws <- replicate(n, draw_true_given_obs_pois_skellam_exact(y_obs, lambda, m, window_sd = 8L))
  # MC standard error ~ sd/sqrt(n); set a practical tolerance with floor for discreteness
  tol <- max(sd(draws) / sqrt(n) * 4, 0.1)
  expect_lt(abs(mean(draws) - exact_mean), tol)
})

test_that("'draw_true_given_obs_pois_skellam_exact' - probability mass function is respected for a tiny case (enumeration check)", {
  set.seed(5)
  y_obs  <- 3L
  lambda <- 2
  m      <- 1
  # Build exact posterior over 0..K where K is modest (manual enumeration feasible)
  K  <- 20L
  xs <- 0:K
  nu <- abs(y_obs - xs)
  logI <- log(besselI(2 * m, nu = nu, expon.scaled = TRUE)) + 2 * m
  logw <- xs * log(lambda) - lgamma(xs + 1) + logI
  p <- exp(logw - max(logw)); p <- p / sum(p)
  # Empirical frequencies
  n <- 20000
  draws <- replicate(n, draw_true_given_obs_pois_skellam_exact(y_obs, lambda, m, window_sd = 8L))
  tab <- table(factor(draws, levels = xs)) / n
  # Compare over the bulk support (exclude far tail where p is tiny)
  keep <- p > 1e-4
  expect_lt(max(abs(tab[keep] - p[keep])), 0.02)  # 2% sup error over bulk is reasonable
})

test_that("'draw_true_given_obs_pois_skellam_exact' - no NA/NaN/Inf returned for a handful of edge-ish inputs", {
  set.seed(6)
  params <- list(
    list(y = 0L,  lam = 0.1, m = 0.1),
    list(y = 15L, lam = 4,   m = 10),
    list(y = 25L, lam = 12,  m = 6),
    list(y = 5L,  lam = 40,  m = 35)
  )
  for (p in params) {
    ans <- draw_true_given_obs_pois_skellam_exact(
      y_obs = p$y, lambda = p$lam, m = p$m, window_sd = 8L
    )
    expect_true(is.finite(ans))
    expect_gte(ans, 0L)
  }
})


## 'draw_true_given_obs_pois_skellam_exact' vs 'draw_true_given_obs_pois_skellam_approx' -----

# Helper: draw n samples from a 1-draw function
.draw_n <- function(fun, n, ...) {
  v <- integer(n)
  for (i in seq_len(n)) v[i] <- fun(...)
  v
}

# Helper: empirical CDF at integer grid
.emp_cdf <- function(x, grid = sort(unique(x))) {
  fx <- ecdf(x)
  fx(grid)
}

test_that("approx vs exact agree: moderate counts, near mean", {
  skip_on_cran()
  set.seed(101)
  y_obs  <- 28L
  lambda <- 30
  m      <- 20
  n      <- 4000
  approx_draws <- .draw_n(draw_true_given_obs_pois_skellam_approx, n,
                          y_obs = y_obs, lambda = lambda, m = m,
                          window_sd = 6L, p0_thresh = 0.01)
  exact_draws  <- .draw_n(draw_true_given_obs_pois_skellam_exact,  n,
                          y_obs = y_obs, lambda = lambda, m = m,
                          window_sd = 8L)
  # Validity
  expect_true(all(is.finite(approx_draws) & approx_draws >= 0))
  expect_true(all(is.finite(exact_draws)  & exact_draws  >= 0))
  # Means close (allow for MC noise + discreteness)
  tol_mean <- 0.15 + 3 * sd(exact_draws) / sqrt(n)
  expect_lt(abs(mean(approx_draws) - mean(exact_draws)), tol_mean)
  # Empirical CDF sup distance (on shared grid)
  grid <- sort(unique(c(approx_draws, exact_draws)))
  d_sup <- max(abs(.emp_cdf(approx_draws, grid) - .emp_cdf(exact_draws, grid)))
  expect_lt(d_sup, 0.06)   # 6% KS-like distance is tight for n=4000
})

test_that("approx vs exact agree: large counts", {
  skip_on_cran()
  set.seed(202)
  y_obs  <- 110L
  lambda <- 100
  m      <- 80
  n      <- 4000
  approx_draws <- .draw_n(draw_true_given_obs_pois_skellam_approx, n,
                          y_obs = y_obs, lambda = lambda, m = m,
                          window_sd = 6L, p0_thresh = 0.01)
  exact_draws  <- .draw_n(draw_true_given_obs_pois_skellam_exact,  n,
                          y_obs = y_obs, lambda = lambda, m = m,
                          window_sd = 8L)
  expect_true(all(approx_draws >= 0L & exact_draws >= 0L))
  tol_mean <- 0.2 + 3 * sd(exact_draws) / sqrt(n)
  expect_lt(abs(mean(approx_draws) - mean(exact_draws)), tol_mean)
  grid <- sort(unique(c(approx_draws, exact_draws)))
  d_sup <- max(abs(.emp_cdf(approx_draws, grid) - .emp_cdf(exact_draws, grid)))
  expect_lt(d_sup, 0.04)   # tighter with large counts
})

test_that("approx vs exact agree: boundary-ish (small mu_post, nontrivial mass at 0)", {
  skip_on_cran()
  set.seed(303)
  y_obs  <- 2L
  lambda <- 8
  m      <- 20
  n      <- 6000
  approx_draws <- .draw_n(draw_true_given_obs_pois_skellam_approx, n,
                          y_obs = y_obs, lambda = lambda, m = m,
                          window_sd = 8L, p0_thresh = 0.05)
  exact_draws  <- .draw_n(draw_true_given_obs_pois_skellam_exact,  n,
                          y_obs = y_obs, lambda = lambda, m = m,
                          window_sd = 8L)
  expect_true(all(approx_draws >= 0L & exact_draws >= 0L))
  # Compare mass at a few small points explicitly (0,1,2)
  for (k in 0:2) {
    p_a <- mean(approx_draws == k)
    p_e <- mean(exact_draws  == k)
    expect_lt(abs(p_a - p_e), 0.03)  # 3% absolute difference
  }
  # Mean tolerance a bit looser near boundary due to truncation/rounding
  tol_mean <- 0.25 + 4 * sd(exact_draws) / sqrt(n)
  expect_lt(abs(mean(approx_draws) - mean(exact_draws)), tol_mean)
  # Distribution closeness
  grid <- sort(unique(c(approx_draws, exact_draws)))
  d_sup <- max(abs(.emp_cdf(approx_draws, grid) - .emp_cdf(exact_draws, grid)))
  expect_lt(d_sup, 0.07)
})

test_that("approx vs exact agree: symmetric case around zero (uses windowing path)", {
  skip_on_cran()
  set.seed(404)
  # Symmetric Skellam with y near lambda gives mu_post near lambda
  y_obs  <- 15L
  lambda <- 15
  m      <- 15
  n      <- 5000
  approx_draws <- .draw_n(draw_true_given_obs_pois_skellam_approx, n,
                          y_obs = y_obs, lambda = lambda, m = m,
                          window_sd = 7L, p0_thresh = 0.02)
  exact_draws  <- .draw_n(draw_true_given_obs_pois_skellam_exact,  n,
                          y_obs = y_obs, lambda = lambda, m = m,
                          window_sd = 8L)
  expect_true(all(approx_draws >= 0L & exact_draws >= 0L))
  tol_mean <- 0.2 + 3 * sd(exact_draws) / sqrt(n)
  expect_lt(abs(mean(approx_draws) - mean(exact_draws)), tol_mean)
  grid <- sort(unique(c(approx_draws, exact_draws)))
  d_sup <- max(abs(.emp_cdf(approx_draws, grid) - .emp_cdf(exact_draws, grid)))
  expect_lt(d_sup, 0.05)
})




## 'insert_after' -------------------------------------------------------

test_that("'insert_after' works with data frames", {
  df <- data.frame(x = 1:3, y = 3:1)
  x <- 11:13
  nm_x = "new"
  ans_obtained <- insert_after(df = df,
                               nm_after = "x",
                               x = x,
                               nm_x = nm_x)
  ans_expected <- data.frame(x = 1:3, new = 11:13, y = 3:1)
  expect_identical(ans_obtained, ans_expected)
  ans_obtained <- insert_after(df = df,
                               nm_after = "y",
                               x = x,
                               nm_x = nm_x)
  ans_expected <- data.frame(x = 1:3, y = 3:1, new = 11:13)
  expect_identical(ans_obtained, ans_expected)
})

test_that("'insert_after' works with tibbles", {
  df <- tibble::tibble(x = 1:3, y = 3:1)
  x <- 11:13
  nm_x = "new"
  ans_obtained <- insert_after(df = df,
                               nm_after = "x",
                               x = x,
                               nm_x = nm_x)
  ans_expected <- tibble(x = 1:3, new = 11:13, y = 3:1)
  expect_identical(ans_obtained, ans_expected)
  ans_obtained <- insert_after(df = df,
                               nm_after = "y",
                               x = x,
                               nm_x = nm_x)
  ans_expected <- tibble(x = 1:3, y = 3:1, new = 11:13)
  expect_identical(ans_obtained, ans_expected)
})


## 'insert_before' -------------------------------------------------------

test_that("'insert_before' works with data frames", {
  df <- data.frame(x = 1:3, y = 3:1)
  x <- 11:13
  nm_x = "new"
  ans_obtained <- insert_before(df = df,
                                nm_before = "x",
                                x = x,
                                nm_x = nm_x)
  ans_expected <- data.frame(new = 11:13, x = 1:3, y = 3:1)
  expect_identical(ans_obtained, ans_expected)
  ans_obtained <- insert_before(df = df,
                                nm_before = "y",
                                x = x,
                                nm_x = nm_x)
  ans_expected <- data.frame(x = 1:3, new = 11:13, y = 3:1)
  expect_identical(ans_obtained, ans_expected)
})

test_that("'insert_before' works with tibbles", {
  df <- tibble::tibble(x = 1:3, y = 3:1)
  x <- 11:13
  nm_x = "new"
  ans_obtained <- insert_before(df = df,
                                nm_before = "x",
                                x = x,
                                nm_x = nm_x)
  ans_expected <- tibble(new = 11:13, x = 1:3, y = 3:1)
  expect_identical(ans_obtained, ans_expected)
  ans_obtained <- insert_before(df = df,
                                nm_before = "y",
                                x = x,
                                nm_x = nm_x)
  ans_expected <- tibble(x = 1:3, new = 11:13, y = 3:1)
  expect_identical(ans_obtained, ans_expected)
})


## 'is_not_testing_or_snapshot' -----------------------------------------------

test_that("'is_not_testing_or_snapshot' is FALSE in a test", {
  expect_false(is_not_testing_or_snapshot())
})


## 'is_same_class' ------------------------------------------------------------

test_that("'is_same_class' returns TRUE when classes same", {
    expect_true(is_same_class(AR1(), AR1()))
    expect_true(is_same_class(1L, 2L))
})

test_that("'is_same_class' returns FALSE when classes different", {
    expect_false(is_same_class(AR1(), N()))
    expect_false(is_same_class(1L, FALSE))
})


## 'log_skellam_safe' ---------------------------------------------------------

test_that("'log_skellam_safe' gives correct answer with valid inputs", {
  ref_log_skellam <- function(k, m) {
    # Symmetric Skellam: P(U=k) = exp(-2m) I_|k|(2m)
    # Using scaled Bessel: log P = log( I_nu(2m) * exp(-2m) )
    if (m == 0) {
      return(ifelse(k == 0, 0, -Inf))
    }
    nu <- abs(as.integer(k))
    val <- log(besselI(2 * m, nu = nu, expon.scaled = TRUE))
    # Replace non-finite with -Inf (true probability zero)
    val[!is.finite(val)] <- -Inf
    val
  }
  m <- 3
  k <- 0:30
  thr <- 1000L # ensure bessel branch is used entirely
  ans_obtained <- log_skellam_safe(x = k, m = m, threshold = thr)
  ans_expected <- vapply(k, ref_log_skellam, numeric(1), m = m)
  expect_length(ans_obtained, length(k))
  expect_true(all(is.finite(ans_obtained)))  # all finite for m>0 on scaled-bessel
  expect_equal(ans_obtained, ans_expected, tolerance = 1e-12)
})

test_that("log_skellam_safe is symmetric in k", {
  m <- 2.5
  k <- -30:30
  thr <- 1000L
  ans_expected <- log_skellam_safe(x = abs(k), m = m, threshold = thr)
  ans_obtained <- log_skellam_safe(x = k, m = m, threshold = thr)
  expect_equal(ans_obtained, ans_expected, tolerance = 1e-12)
})

test_that("no NaN produced; only -Inf for true zero-prob cases", {
  m <- 0.7
  k <- c(0:10, 100, 500, 1000)
  thr <- 50L
  got <- log_skellam_safe(x = k, m = m, threshold = thr)
  expect_false(any(is.nan(got)))
  # For m>0, scaled-bessel/approx should be finite (not -Inf),
  # though extreme cases may return very negative but finite values.
  expect_true(all(is.finite(got)))
})

test_that("handles m = 0 edge case as point mass at k = 0", {
  m <- 0
  k <- -5:5
  thr <- 10L
  # Expect log pmf is 0 at k=0, -Inf otherwise
  got <- log_skellam_safe(x = k, m = m, threshold = thr)
  expect_equal(got[k == 0], 0)
  expect_true(all(is.infinite(got[k != 0]) & got[k != 0] < 0))
})

test_that("vectorization and length are correct", {
  m <- 4
  k <- sample(-100:100, size = 37)
  thr <- 25L
  got <- log_skellam_safe(x = k, m = m, threshold = thr)
  expect_length(got, length(k))
  # symmetry per-element
  got_sym <- log_skellam_safe(x = -k, m = m, threshold = thr)
  expect_equal(got, got_sym, tolerance = 0.01)
})

test_that("monotonic decrease away from 0 (unimodality check)", {
  m <- 3
  k <- 0:50
  thr <- 1000L
  g0 <- log_skellam_safe(x = k, m = m, threshold = thr)
  # non-increasing as k grows from 0
  expect_true(all(diff(g0) <= 1e-6))
})


## 'make_scaled_eigen' --------------------------------------------------------

## See also tests for rvnorm_eigen

test_that("'make_scaled_eigen' works with positive definite matrix", {
    set.seed(0)
    prec <- solve(crossprod(matrix(rnorm(25), 5)))
    ans <- make_scaled_eigen(prec)
    expect_identical(dim(ans), dim(prec))
})

test_that("'make_scaled_eigen' works with non-negative definite matrix", {
    set.seed(0)
    prec <- solve(crossprod(matrix(rnorm(25), 5)))
    prec[5,] <- 0
    prec[,5] <- 0
    ans <- make_scaled_eigen(prec)
    expect_identical(dim(ans), dim(prec))
})


## 'paste_dot' ----------------------------------------------------------------

test_that("'paste_dot' works with valid inputs", {
  expect_identical(paste_dot(1:3, 3:1), c("1.3", "2.2", "3.1"))
})


## 'rbinom_guarded' ------------------------------------------------------------

test_that("'rbinom_guarded' works when values below threshold - size, prob both rvec", {
  set.seed(0)
  prob <- rvec::rvec_dbl(matrix(runif(n = 200), nrow = 10))
  size <- rvec::rvec_int(matrix(rpois(n = 200, lambda = 10), nrow = 10))
  set.seed(1)
  ans_obtained <- rbinom_guarded(size = size, prob = prob)
  set.seed(1)
  ans_expected <- rvec::rbinom_rvec(n = 10, size = size, prob = prob)
  expect_identical(ans_obtained, ans_expected)
})

test_that("'rbinom_guarded' works when values below threshold - size rvec, prob numeric", {
  set.seed(0)
  prob <- runif(n = 10)
  size <- rvec::rvec_int(matrix(rpois(n = 200, lambda = 10), nrow = 10))
  set.seed(1)
  ans_obtained <- rbinom_guarded(size = size, prob = prob)
  set.seed(1)
  ans_expected <- rvec::rbinom_rvec(n = 10, size = size, prob = prob)
  expect_identical(ans_obtained, ans_expected)
})

test_that("'rbinom_guarded' works when values below threshold - size numeric, prob rvec", {
  set.seed(0)
  prob <- rvec::rvec_dbl(matrix(runif(n = 200), nrow = 10))
  size <- rpois(n = 10, lambda = 10)
  set.seed(1)
  ans_obtained <- rbinom_guarded(size = size, prob = prob)
  set.seed(1)
  ans_expected <- rvec::rbinom_rvec(n = 10, size = size, prob = prob)
  expect_identical(ans_obtained, ans_expected)
})

test_that("'rbinom_guarded' works when values below threshold - size, prob both numeric", {
  set.seed(0)
  prob <- runif(n = 200)
  size <- rpois(n = 200, lambda = 10) 
  set.seed(1)
  ans_obtained <- rbinom_guarded(size = size, prob = prob)
  set.seed(1)
  ans_expected <- as.double(rbinom(n = 200, size = size, prob = prob))
  expect_identical(ans_obtained, ans_expected)
})

test_that("'rbinom_guarded' works when values above threshold - size, prob both rvec", {
  set.seed(0)
  size <- rvec::rvec_dbl(matrix(rpois(n = 200, lambda = 100), nrow = 10))
  size[10] <- 1e10
  prob <- c(rvec::rvec_dbl(matrix(runif(n = 180), nrow = 9)), 0.9)
  set.seed(1)
  expect_warning(
    ans_obtained <- rbinom_guarded(size = size, prob = prob),
    "Large values for `size` \\* `prob` used to generate binomial variates."
  )
  set.seed(1)
  ans_expected <- c(rvec::rbinom_rvec(n = 9,
                                      size = size[1:9],
                                      prob = prob[1:9]),
                    0.9 * 1e10)
  expect_identical(ans_obtained, ans_expected)
})

test_that("'rbinom_guarded' throws error when size, prob different lengths", {
  set.seed(0)
  prob <- rvec::rvec_dbl(matrix(runif(n = 200), nrow = 10))
  size <- rpois(n = 9, lambda = 10)
  expect_error(rbinom_guarded(size = size, prob = prob),
               "Internal error")
})


## 'rmvnorm_chol', 'rmvnorm_eigen' --------------------------------------------

test_that("'rmvnorm_chol' and 'rmvnorm_eigen' give the same answer", {
    set.seed(0)
    prec <- crossprod(matrix(rnorm(25), 5))
    mean <- rnorm(5)
    R_prec <- chol(prec)
    scaled_eigen <- make_scaled_eigen(prec)
    ans_chol <- rmvnorm_chol(n = 100000, mean = mean, R_prec = R_prec)
    ans_eigen <- rmvnorm_eigen(n = 100000, mean = mean, scaled_eigen = scaled_eigen)
    expect_equal(rowMeans(ans_chol), rowMeans(ans_eigen), tolerance = 0.02)
    expect_equal(cov(t(ans_chol)), cov(t(ans_eigen)), tolerance = 0.02)
})


## 'rpois_guarded' ------------------------------------------------------------

test_that("'rpois_guarded' works when values below threshold - rvec", {
  set.seed(0)
  lambda <- rvec::rvec_dbl(matrix(runif(n = 200, max = 100), nrow = 10))
  set.seed(1)
  ans_obtained <- rpois_guarded(lambda = lambda)
  set.seed(1)
  ans_expected <- rvec::rpois_rvec(n = 10, lambda = lambda)
  expect_identical(ans_obtained, ans_expected)
})

test_that("'rpois_guarded' works when values below threshold - numeric", {
  set.seed(0)
  lambda <- runif(n = 200, max = 100)
  set.seed(1)
  ans_obtained <- rpois_guarded(lambda = lambda)
  set.seed(1)
  ans_expected <- as.double(rpois(n = 200, lambda = lambda))
  expect_identical(ans_obtained, ans_expected)
})

test_that("'rpois_guarded' works when values above threshold - rvec", {
  set.seed(0)
  lambda <- rvec::rvec_dbl(matrix(runif(n = 200, max = 100), nrow = 10))
  lambda[10] <- 1e9
  set.seed(1)
  expect_warning(
    ans_obtained <- rpois_guarded(lambda = lambda),
    "Large values for `lambda` used to generate Poisson variates."
  )
  set.seed(1)
  ans_expected <- c(rvec::rpois_rvec(n = 9, lambda = lambda[1:9]), 1e9)
  expect_identical(ans_obtained, ans_expected)
})

test_that("'rpois_guarded' works when values above threshold - numeric", {
  set.seed(0)
  lambda <- runif(n = 200, max = 100)
  lambda[10] <- 1e9
  set.seed(1)
  expect_warning(
    ans_obtained <- rpois_guarded(lambda = lambda),
    "Large values for `lambda` used to generate Poisson variates."
  )
  set.seed(1)
  ans_expected <- rep(1e9, times = 200)
  ans_expected[-10] <- rpois(n = 199, lambda = lambda[-10])
  expect_identical(ans_obtained, ans_expected)
})


## 'rvec_to_mean' -------------------------------------------------------------

test_that("'rvec_to_mean' works with valid inputs", {
  data <- tibble::tibble(a = 1:3,
                         b = rvec::rvec(matrix(1:12, nr = 3)),
                         c = rvec::rvec(matrix(FALSE, nr = 3, nc = 2)),
                         d = c("a", "b", "c"))
  ans_obtained <- rvec_to_mean(data)
  ans_expected <- tibble::tibble(a = 1:3,
                         b = rowMeans(matrix(1:12, nr = 3)),
                         c = rowMeans(matrix(FALSE, nr = 3, nc = 2)),
                         d = c("a", "b", "c"))
  expect_identical(ans_obtained, ans_expected)
})



## 'sample_post_binom_betabinom' ----------------------------------------------

test_that("basic valid inputs work and return integers", {
  set.seed(123)
  n <- c(5, 10)
  y <- c(2, 4)
  mu <- c(0.5, 0.3)
  xi <- c(1, 2)
  pi <- c(0.7, 0.8)
  draws <- sample_post_binom_betabinom(n, y, mu, xi, pi)
  # correct length
  expect_length(draws, length(n))
  # numeric and integer-valued
  expect_true(is.numeric(draws))
  expect_true(all(draws == as.integer(draws)))
  # within allowed range
  expect_true(all(draws >= y & draws <= n))
})

test_that("returns NA when any input element is NA", {
  set.seed(123)
  out <- sample_post_binom_betabinom(n = c(5, NA),
                                     y = c(2, 1),
                                     mu = c(0.5, 0.4),
                                     xi = c(1, 1),
                                     pi = c(0.7, 0.7))
  expect_true(is.na(out[2]))
})

test_that("errors for non-integer n or y", {
  expect_error(sample_post_binom_betabinom(n = c(5.5, 10),
                                           y = c(2, 4),
                                           mu = c(0.5, 0.3),
                                           xi = c(1, 2),
                                           pi = c(0.7, 0.8)))
  expect_error(sample_post_binom_betabinom(n = c(5, 10),
                                           y = c(2.2, 4),
                                           mu = c(0.5, 0.3),
                                           xi = c(1, 2),
                                           pi = c(0.7, 0.8)))
})

test_that("errors for mu, xi, pi out of range", {
  expect_error(sample_post_binom_betabinom(n = 5,
                                           y = 2,
                                           mu = 1.5,
                                           xi = 1,
                                           pi = 0.5))
  expect_error(sample_post_binom_betabinom(n = 5,
                                           y = 2,
                                           mu = 0.5,
                                           xi = -1,
                                           pi = 0.5))
  expect_error(sample_post_binom_betabinom(n = 5,
                                           y = 2,
                                           mu = 0.5,
                                           xi = 1,
                                           pi = 1.5))
})

test_that("works with vectorised inputs", {
  set.seed(42)
  n <- 4:6
  y <- 1:3
  mu <- rep(0.4, 3)
  xi <- rep(1, 3)
  pi <- rep(0.8, 3)
  draws <- sample_post_binom_betabinom(n, y, mu, xi, pi)
  expect_length(draws, 3)
  expect_true(all(draws >= y & draws <= n))
})


## 'sample_post_binom_betabinom_inner' ----------------------------------------

test_that("returns NA when any argument is NA", {
  expect_true(is.na(sample_post_binom_betabinom_inner(NA, 1, 0.5, 1, 0.8)))
  expect_true(is.na(sample_post_binom_betabinom_inner(10, NA, 0.5, 1, 0.8)))
  expect_true(is.na(sample_post_binom_betabinom_inner(10, 1, NA, 1, 0.8)))
  expect_true(is.na(sample_post_binom_betabinom_inner(10, 1, 0.5, NA, 0.8)))
  expect_true(is.na(sample_post_binom_betabinom_inner(10, 1, 0.5, 1, NA)))
})

test_that("returns y when y == n", {
  expect_equal(sample_post_binom_betabinom_inner(5, 5, 0.5, 1, 0.8), 5)
  expect_equal(sample_post_binom_betabinom_inner(10, 10, 0.2, 0.5, 0.1), 10)
})

test_that("output is always between y and n inclusive", {
  set.seed(123)
  draws <- replicate(100,
                     sample_post_binom_betabinom_inner(10, 3, 0.5, 1, 0.8))
  expect_true(all(draws >= 3 & draws <= 10))
})

test_that("function returns an integer", {
  set.seed(123)
  out <- sample_post_binom_betabinom_inner(10, 2, 0.3, 1, 0.7)
  expect_true(is.integer(out))
})

test_that("weights are non-degenerate", {
  # should not always return the same value unless forced (like y==n)
  set.seed(123)
  draws <- replicate(200,
                     sample_post_binom_betabinom_inner(15, 5, 0.5, 1, 0.5))
  expect_gt(length(unique(draws)), 1)
})

test_that("extreme pi values behave sensibly", {
  set.seed(123)
  draws1 <- replicate(100, sample_post_binom_betabinom_inner(10, 3, 0.5, 1, pi = 0.0001))
  draws2 <- replicate(100, sample_post_binom_betabinom_inner(10, 3, 0.5, 1, pi = 0.9999))
  expect_true(all(draws1 >= 3 & draws1 <= 10))
  expect_true(all(draws2 >= 3 & draws2 <= 10))
})

test_that("recovers distribution", {
  set.seed(0)
  n <- 1000
  mu <- 0.4
  xi <- 3
  pi <- 0.9
  n_iter <- 10000
  x_true <- numeric(n_iter)
  x_post <- numeric(n_iter)
  for (i in seq_len(n_iter)) {
    p <- rbeta(n = 1, shape1 = mu/xi, shape2 = (1-mu)/xi)
    x <- rbinom(n = 1, size = n, prob = p)
    y <- rbinom(n = 1, size = x, prob = pi)
    xx <- sample_post_binom_betabinom_inner(n = n,
                                            y = y,
                                            mu = mu,
                                            xi = xi,
                                            pi = pi)
    x_true[i] <- x
    x_post[i] <- xx
  }
  expect_equal(mean(x_true), mean(x_post), tolerance = 0.01)
  expect_equal(sd(x_true), sd(x_post), tolerance = 0.01)
})
    
