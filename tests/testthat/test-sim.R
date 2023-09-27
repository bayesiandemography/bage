
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

test_that("'draw_vals_hyper_mod' works with bage_mod_pois", {
    set.seed(0)
    data <- expand.grid(age = 0:9, time = 2000:2005, sex = c("F", "M"))
    data$popn <- rpois(n = nrow(data), lambda = 100)
    data$deaths <- rpois(n = nrow(data), lambda = 10)
    formula <- deaths ~ age * time + sex
    mod <- mod_pois(formula = formula,
                    data = data,
                    exposure = popn)
    ans <- draw_vals_hyper_mod(mod, n_sim = 10)
    expect_identical(names(ans), c("(Intercept)", "age", "time", "sex", "age:time"))
    expect_identical(length(ans$age$sd), 10L)
})

test_that("'draw_vals_par_mod' works with bage_mod_pois", {
    set.seed(0)
    data <- expand.grid(age = 0:9, time = 2000:2005, sex = c("F", "M"))
    data$popn <- rpois(n = nrow(data), lambda = 100)
    data$deaths <- rpois(n = nrow(data), lambda = 10)
    formula <- deaths ~ age * time + sex
    mod <- mod_pois(formula = formula,
                    data = data,
                    exposure = popn)
    vals_hyper <- draw_vals_hyper_mod(mod, n_sim = n_sim)
    ans <- draw_vals_par_mod(mod,
                             vals_hyper = vals_hyper,
                             n_sim = 10)
    expect_identical(names(ans), c("(Intercept)", "age", "time", "sex", "age:time"))
    expect_true(all(sapply(ans, ncol) == 10L))
    expect_identical(sapply(ans, nrow), sapply(mod$matrices_par_outcome, ncol))
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

test_that("'draw_vals_season' works", {
    set.seed(0)
    data <- expand.grid(age = 0:9, time = 2001:2007, sex = c("F", "M"))
    data$popn <- rpois(n = nrow(data), lambda = 100)
    data$deaths <- rpois(n = nrow(data), lambda = 10)
    formula <- deaths ~ age + sex + time
    mod <- mod_pois(formula = formula,
                    data = data,
                    exposure = popn)
    mod <- set_season(mod, n = 2)
    mod <- set_prior(mod, age ~ Spline())
    mod <- set_prior(mod, time ~ RW2())
    ans <- draw_vals_season(mod, n_sim = 100)
    expect_identical(length(ans$sd), 100L)
    expect_identical(nrow(ans$season), 7L)
    expect_identical(ncol(ans$season), 100L)
    expect_equal(mean(apply(ans$season[c(1, 3, 5, 7),], 2, mean)), 0, tolerance = 0.01)
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
    expect_equal(ans_obtained, ans_expected)eee
})

test_that("'make_vals_linpred_par' works", {
    set.seed(0)
    data <- expand.grid(age = 0:9, time = 2000:2005, sex = c("F", "M"))
    data$popn <- rpois(n = nrow(data), lambda = 100)
    data$deaths <- rpois(n = nrow(data), lambda = 10)
    formula <- deaths ~ age * time + sex
    mod <- mod_pois(formula = formula,
                    data = data,
                    exposure = popn)
    n_sim <- 10L
    vals_hyper <- draw_vals_hyper_mod(mod, n_sim = n_sim)
    vals_par <- draw_vals_par_mod(mod,
                                  vals_hyper = vals_hyper,
                                  n_sim = n_sim)
    ans <- make_vals_linpred_par(mod = mod,
                                 vals_par = vals_par)
    expect_identical(nrow(ans), length(mod$outcome))
    expect_identical(ncol(ans), n_sim)
})

test_that("'make_vals_linpred_season' works", {
    set.seed(0)
    data <- expand.grid(age = 0:9, time = 2000:2006, sex = c("F", "M"))
    data$popn <- rpois(n = nrow(data), lambda = 100)
    data$deaths <- rpois(n = nrow(data), lambda = 10)
    formula <- deaths ~ age * time + sex
    mod <- mod_pois(formula = formula,
                    data = data,
                    exposure = popn)
    mod <- set_season(mod, n = 2)
    n_sim <- 10L
    vals_season <- draw_vals_season(mod, n_sim = n_sim)
    ans <- make_vals_linpred_season(mod = mod,
                                    vals_season = vals_season)
    expect_identical(nrow(ans), length(mod$outcome))
    expect_identical(ncol(ans), n_sim)
    expect_true(all(apply(ans, 2, function(x) length(unique(x))) == 7L))
})

