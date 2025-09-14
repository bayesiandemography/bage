
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



## 'log_dskellam_R' -----------------------------------------------------------

log_dskellam_call_ <- function(k, mu1, mu2, x_thresh = 700) {
  .Call("C_log_dskellam_R",
        as.integer(k),
        as.numeric(mu1),
        as.numeric(mu2),
        as.numeric(x_thresh))
}


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
    
