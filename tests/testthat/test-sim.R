
## draw_vals helpers ----------------------------------------------------------

test_that("'draw_vals_ar1' works", {
    set.seed(0)
    prior <- AR1()
    n_sim <- 10
    sd <- draw_vals_sd(prior = prior, n_sim = n_sim)
    coef <- draw_vals_coef(prior = prior, n_sim = n_sim)
    labels <- 1:10000
    set.seed(0)
    ans <- draw_vals_ar1(coef = coef,
                         sd = sd,
                         labels = labels)
    expect_equal(unname(apply(ans, 2, sd)), sd, tolerance = 0.05)
    get_slope <- function(x) {
        v1 <- x[-length(x)]
        v2 <- x[-1]
        coef(lm(v2 ~ v1))[["v1"]]
    }
    expect_equal(unname(apply(ans, 2, get_slope)), coef, tolerance = 0.05)
    expect_identical(dim(ans), c(10000L, 10L))
    expect_identical(dimnames(ans),
                     list(as.character(seq_len(10000)),
                          as.character(seq_len(10))))
})

test_that("'draw_vals_coef' works", {
    prior <- AR1()
    n_sim <- 10
    set.seed(0)
    ans_obtained <- draw_vals_coef(prior = prior, n_sim = n_sim)
    set.seed(0)
    ans_expected <- 0.8 + 0.18 * rbeta(n = 10, shape1 = 2, shape2 = 2)
    expect_equal(ans_obtained, ans_expected)
})

test_that("'draw_vals_rw' works", {
    set.seed(0)
    prior <- RW()
    n_sim <- 1000
    sd <- draw_vals_sd(prior = prior, n_sim = n_sim)
    sd_intercept <- 0.001
    labels <- 1:200
    set.seed(0)
    ans <- draw_vals_rw(sd = sd,
                        sd_intercept = sd_intercept,
                        labels = labels)
    expect_equal(unname(apply(ans, 2, function(x) sd(diff(x)))),
                 sd,
                 tolerance = 0.05)
    expect_equal(sd(colMeans(ans)),
                 sd_intercept,
                 tolerance = 0.05)
    expect_identical(dim(ans), c(200L, 1000L))
    expect_identical(dimnames(ans),
                     list(as.character(seq_len(200)),
                          as.character(seq_len(1000))))
})

test_that("'draw_vals_rw' works", {
    set.seed(0)
    prior <- RW2()
    n_sim <- 1000
    sd <- draw_vals_sd(prior = prior, n_sim = n_sim)
    sd_intercept <- 0.001
    sd_slope <- 1
    labels <- 1:100
    set.seed(0)
    ans <- draw_vals_rw2(sd = sd,
                         sd_intercept = sd_intercept,
                         sd_slope = sd_slope,
                         labels = labels)
    expect_equal(unname(apply(ans, 2, function(x) sd(diff(x, diff = 2)))),
                 sd,
                 tolerance = 0.1)
    expect_equal(sd(colMeans(ans)),
                 sd_intercept,
                 tolerance = 0.1)
    get_slope <- function(x) {
        h <- seq(from = -1, to = 1, along.with = x)
        coef(lm(x ~ h))[["h"]]
    }
    expect_equal(sd(apply(ans, 2, get_slope)),
                 sd_slope,
                 tolerance = 0.1)
    expect_identical(dim(ans), c(100L, 1000L))
    expect_identical(dimnames(ans),
                     list(as.character(seq_len(100)),
                          as.character(seq_len(1000))))
})

test_that("'draw_vals_sd' works", {
    prior <- N(s = 0.2)
    n_sim <- 10
    set.seed(0)
    ans_obtained <- draw_vals_sd(prior = prior, n_sim = n_sim)
    set.seed(0)
    ans_expected <- abs(rnorm(n = 10, sd = 0.2))
    expect_identical(ans_obtained, ans_expected)
})

test_that("'make_diff_matrix' works", {
    set.seed(0)
    m <- make_diff_matrix(10)
    x <- rnorm(10)
    ans_obtained <- as.numeric(m %*% x)
    ans_expected <- diff(x)
    expect_equal(ans_obtained, ans_expected)
})

test_that("'make_rw_matrix' works", {
    set.seed(0)
    m <- make_rw_matrix(10)
    x <- rnorm(10)
    ans_obtained <- as.numeric(m %*% x)
    ans_expected <- c(diff(x), mean(x))
    expect_equal(ans_obtained, ans_expected)
})

test_that("'make_rw2_matrix' works", {
    set.seed(0)
    m <- make_rw2_matrix(10)
    x <- rnorm(10)
    ans_obtained <- as.numeric(m %*% x)
    h <- seq(from = -1, to = 1, length.out = 10)
    slope <- coef(lm(x ~ h))[["h"]]
    ans_expected <- c(diff(x, diff = 2), mean(x), slope)
    expect_equal(ans_obtained, ans_expected)
})


          
