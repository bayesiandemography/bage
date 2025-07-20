
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


## 'rpois_rvec_guarded' -------------------------------------------------------

test_that("'rpois_rvec_guarded' works when values below threshold", {
  set.seed(0)
  lambda <- rvec::rvec_dbl(matrix(runif(n = 200, max = 100), nrow = 10))
  set.seed(1)
  ans_obtained <- rpois_rvec_guarded(n = 10, lambda = lambda)
  set.seed(1)
  ans_expected <- rvec::rpois_rvec(n = 10, lambda = lambda)
  expect_identical(ans_obtained, ans_expected)
})

test_that("'rpois_rvec_guarded' works when values above threshold", {
  set.seed(0)
  lambda <- rvec::rvec_dbl(matrix(runif(n = 200, max = 100), nrow = 10))
  lambda[10] <- 1e9
  set.seed(1)
  expect_warning(
    ans_obtained <- rpois_rvec_guarded(n = 10, lambda = lambda),
    "Large values for `lambda` used to generate Poisson variates."
  )
  set.seed(1)
  ans_expected <- c(rvec::rpois_rvec(n = 9, lambda = lambda[1:9]), 1e9)
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

