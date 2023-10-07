
## calc_error_point_est -------------------------------------------------------

test_that("'calc_error_point_est' works - include_priors is TRUE", {
    estimate <- list(a = rvec::rvec_dbl(matrix(1:12, nr = 3)),
                     b = rvec::rvec_dbl(matrix(13:24, nr = 3)))
    truth <- list(a = 1:3, b = 4:6)
    point_est_fun <- "mean"
    ans_obtained <- calc_error_point_est(estimate = estimate,
                                         truth = truth,
                                         point_est_fun = point_est_fun,
                                         include_priors = TRUE)
    ans_expected <- list(a = calc_error_point_est_one(estimate = estimate[[1]],
                                                      truth = truth[[1]],
                                                      rvec_fun = rvec::draws_mean),
                         b = calc_error_point_est_one(estimate = estimate[[2]],
                                                      truth = truth[[2]],
                                                      rvec_fun = rvec::draws_mean))
    expect_equal(ans_obtained, ans_expected)
})

test_that("'calc_error_point_est' works - include_priors is FALSE", {
    estimate <- list(hyper = rvec::rvec_dbl(matrix(1:12, nr = 3)),
                     b = rvec::rvec_dbl(matrix(13:24, nr = 3)))
    truth <- list(hyper = 1:3, b = 4:6)
    point_est_fun <- "mean"
    ans_obtained <- calc_error_point_est(estimate = estimate,
                                         truth = truth,
                                         point_est_fun = point_est_fun,
                                         include_priors = FALSE)
    ans_expected <- list(b = calc_error_point_est_one(estimate = estimate[[2]],
                                                      truth = truth[[2]],
                                                      rvec_fun = rvec::draws_mean))
    expect_equal(ans_obtained, ans_expected)
})


## calc_error_point_est_one ---------------------------------------------------

test_that("'calc_error_point_est_one' works", {
    estimate <- rvec::rvec_dbl(matrix(1:12, nr = 3))
    truth <- 1:3
    rvec_fun <- rvec::draws_mean
    ans_obtained <- calc_error_point_est_one(estimate = estimate,
                                             truth = truth,
                                             rvec_fun = rvec_fun)
    ans_expected <- rowMeans(matrix(1:12, nr = 3)) - 1:3
    expect_equal(ans_obtained, ans_expected)
})

## calc_is_in_interval --------------------------------------------------------

test_that("'calc_is_in_interval' works - include_priors is TRUE", {
    estimate <- list(par = rvec::rvec_dbl(matrix(1:12, nr = 3)),
                     b = rvec::rvec_dbl(matrix(13:24, nr = 3)))
    truth <- list(par = 1:3, b = 4:6)
    widths <- c(0.5, 0.9, 1)
    ans_obtained <- calc_is_in_interval(estimate = estimate,
                                        truth = truth,
                                        widths = widths,
                                        include_priors = TRUE)
    ans_expected <- list(par = calc_is_in_interval_one(estimate = estimate[[1]],
                                                     truth = truth[[1]],
                                                     widths = widths),
                         b = calc_is_in_interval_one(estimate = estimate[[2]],
                                                     truth = truth[[2]],
                                                     widths = widths))
    expect_equal(ans_obtained, ans_expected)
})

test_that("'calc_is_in_interval' works - include_priors is FALSE", {
    estimate <- list(par = rvec::rvec_dbl(matrix(1:12, nr = 3)),
                     b = rvec::rvec_dbl(matrix(13:24, nr = 3)))
    truth <- list(par = 1:3, b = 4:6)
    widths <- c(0.5, 0.9, 1)
    ans_obtained <- calc_is_in_interval(estimate = estimate,
                                        truth = truth,
                                        widths = widths,
                                        include_priors = FALSE)
    ans_expected <- list(b = calc_is_in_interval_one(estimate = estimate[[2]],
                                                     truth = truth[[2]],
                                                     widths = widths))
    expect_equal(ans_obtained, ans_expected)
})


## calc_is_in_interval_one ---------------------------------------------------

test_that("'calc_is_in_interval_one' works", {
    estimate <- rvec::rvec_dbl(matrix(1:12, nr = 3))
    truth <- rep(2, 3)
    widths <- c(0.9, 1)
    ans_obtained <- calc_is_in_interval_one(estimate = estimate,
                                            truth = truth,
                                            widths = widths)
    ans_expected <- list("0.9" = c(TRUE, FALSE, FALSE),
                         "1" = c(TRUE, TRUE, FALSE))
    expect_equal(ans_obtained, ans_expected)
})                                       


## draw_vals_ar1 --------------------------------------------------------------

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


## draw_vals_coef -------------------------------------------------------------

test_that("'draw_vals_coef' works", {
    prior <- AR1()
    n_sim <- 10
    set.seed(0)
    ans_obtained <- draw_vals_coef(prior = prior, n_sim = n_sim)
    set.seed(0)
    ans_expected <- 0.8 + 0.18 * rbeta(n = 10, shape1 = 2, shape2 = 2)
    expect_equal(ans_obtained, ans_expected)
})


## draw_vals_hyper_mod --------------------------------------------------------

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


## draw_vals_hyperparam -------------------------------------------------------

test_that("'draw_vals_hyperparam' works - has season and disp", {
    set.seed(0)
    data <- expand.grid(age = 0:9, time = 2001:2007, sex = c("F", "M"))
    data$popn <- rpois(n = nrow(data), lambda = 100)
    data$deaths <- rpois(n = nrow(data), lambda = 10)
    formula <- deaths ~ age + sex + time
    mod <- mod_pois(formula = formula,
                    data = data,
                    exposure = popn)
    mod <- set_season(mod, n = 2)
    ans <- draw_vals_hyperparam(mod, n_sim = 10)
    expect_identical(names(ans),
                     c("par", "hyper", "disp", "season", "linpred"))
    expect_false(any(sapply(ans, is.null)))
})

test_that("'draw_vals_hyperparam' works - no season or disp", {
    set.seed(0)
    data <- expand.grid(age = 0:9, time = 2001:2007, sex = c("F", "M"))
    data$popn <- rpois(n = nrow(data), lambda = 100)
    data$deaths <- rpois(n = nrow(data), lambda = 10)
    formula <- deaths ~ age + sex + time
    mod <- mod_pois(formula = formula,
                    data = data,
                    exposure = popn)
    mod <- set_disp(mod, s = 0)
    ans <- draw_vals_hyperparam(mod, n_sim = 10)
    expect_identical(names(ans),
                     c("par", "hyper", "disp", "season", "linpred"))
    expect_identical(sapply(ans, is.null),
                     c(par = FALSE,
                       hyper = FALSE,
                       disp = TRUE,
                       season = TRUE,
                       linpred = FALSE))
})


## draw_vals_par_mod ----------------------------------------------------------

test_that("'draw_vals_par_mod' works with bage_mod_pois", {
    set.seed(0)
    data <- expand.grid(age = 0:9, time = 2000:2005, sex = c("F", "M"))
    data$popn <- rpois(n = nrow(data), lambda = 100)
    data$deaths <- rpois(n = nrow(data), lambda = 10)
    formula <- deaths ~ age * time + sex
    mod <- mod_pois(formula = formula,
                    data = data,
                    exposure = popn)
    n_sim <- 10
    vals_hyper <- draw_vals_hyper_mod(mod, n_sim = n_sim)
    ans <- draw_vals_par_mod(mod,
                             vals_hyper = vals_hyper,
                             n_sim = n_sim)
    expect_identical(names(ans), c("(Intercept)", "age", "time", "sex", "age:time"))
    expect_true(all(sapply(ans, ncol) == n_sim))
    expect_identical(sapply(ans, nrow), sapply(mod$matrices_par_outcome, ncol))
})


## draw_vals_rw ---------------------------------------------------------------

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


## draw_vals_rw2 --------------------------------------------------------------

test_that("'draw_vals_rw2' works", {
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


## draw_vals_sd ---------------------------------------------------------------

test_that("'draw_vals_sd' works", {
    prior <- N(s = 0.2)
    n_sim <- 10
    set.seed(0)
    ans_obtained <- draw_vals_sd(prior = prior, n_sim = n_sim)
    set.seed(0)
    ans_expected <- abs(rnorm(n = 10, sd = 0.2))
    expect_identical(ans_obtained, ans_expected)
})


## 'draw_vals_season' ---------------------------------------------------------

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


## 'get_vals_hyperparam_est' --------------------------------------------------

test_that("'get_vals_hyperparam_est' works", {
    set.seed(0)
    data <- expand.grid(age = 0:9, time = 2000:2005, sex = c("F", "M"))
    data$popn <- rpois(n = nrow(data), lambda = 100)
    data$deaths <- rpois(n = nrow(data), lambda = 10)
    formula <- deaths ~ age + sex + time
    mod <- mod_pois(formula = formula,
                    data = data,
                    exposure = popn)
    mod <- set_season(mod, n = 2)
    mod <- fit(mod)
    ans_obtained <- get_vals_hyperparam_est(mod)
    components <- components(mod)
    ans_expected <- list(par = subset(components, component == "par", select = ".fitted")[[1]],
                         hyper = subset(components, component == "hyper", select = ".fitted")[[1]],
                         disp = subset(components, component == "disp", select = ".fitted")[[1]],
                         season = subset(components, component == "season", select = ".fitted")[[1]],
                         linpred = make_linpred_par(mod, components) +
                             make_linpred_season(mod, components))
    names(ans_expected[[1L]]) <- paste0(subset(components, component == "par", select = "term")[[1]],
                                        subset(components, component == "par", select = "level")[[1]])
    names(ans_expected[[2L]]) <- paste0(subset(components, component == "hyper", select = "term")[[1]],
                                        subset(components, component == "hyper", select = "level")[[1]])
    names(ans_expected[[3L]]) <- paste0(subset(components, component == "disp", select = "term")[[1]],
                                        subset(components, component == "disp", select = "level")[[1]])
    names(ans_expected[[4L]]) <- paste0(subset(components, component == "season", select = "term")[[1]],
                                        subset(components, component == "season", select = "level")[[1]])
    expect_identical(ans_obtained, ans_expected)
})


## 'get_vals_sim_one' ---------------------------------------------------------

test_that("'get_vals_sim_one' works - include_priors = TRUE", {
    set.seed(0)
    data <- expand.grid(age = 0:9, time = 2000:2005, sex = c("F", "M"))
    data$popn <- rpois(n = nrow(data), lambda = 100)
    data$deaths <- rpois(n = nrow(data), lambda = 10)
    formula <- deaths ~ age + sex + time
    mod <- mod_pois(formula = formula,
                    data = data,
                    exposure = popn)
    mod <- set_season(mod, n = 2)
    vals <- draw_vals_mod(mod, n_sim = 10)
    ans_obtained <- get_vals_sim_one(vals, i_sim = 3L)
    ans_expected <- list(par = list("(Intercept)" = vals$par[["(Intercept)"]][,3,drop = FALSE],
                                    age = vals$par$age[, 3, drop = FALSE],
                                    sex = vals$par$sex[, 3, drop = FALSE],
                                    time = vals$par$time[, 3, drop = FALSE]),
                         hyper = list("(Intercept)" = list(),
                                      age = list(sd = vals$hyper$age$sd[3]),
                                      sex = list(sd = vals$hyper$sex$sd[3]),
                                      time = list(sd = vals$hyper$time$sd[3])),
                         disp = vals$disp[3],
                         season = list(season = vals$season$season[,3,drop = FALSE],
                                       sd = vals$season$sd[3]),
                         fitted = vals$fitted[,3,drop = FALSE],
                         outcome = vals$outcome[,3,drop = FALSE])
    expect_equal(as.numeric(unlist(ans_obtained)),
                 as.numeric(unlist(ans_expected)))
})


## 'is_same_priors' -----------------------------------------------------------

test_that("'is_same_priors' returns TRUE when priors same", {
    set.seed(0)
    data <- expand.grid(age = 0:9, time = 2000:2005, sex = c("F", "M"))
    data$popn <- rpois(n = nrow(data), lambda = 100)
    data$deaths <- rpois(n = nrow(data), lambda = 10)
    mod <- mod_pois(formula = deaths ~ age + sex + time,
                    data = data,
                    exposure = popn)
    expect_true(is_same_priors(mod, mod))
})

test_that("'is_same_priors' returns TRUE when priors same - different order", {
    set.seed(0)
    data <- expand.grid(age = 0:9, time = 2000:2005, sex = c("F", "M"))
    data$popn <- rpois(n = nrow(data), lambda = 100)
    data$deaths <- rpois(n = nrow(data), lambda = 10)
    mod1 <- mod_pois(formula = deaths ~ age + sex + time,
                    data = data,
                    exposure = popn)
    mod2 <- mod_pois(formula = deaths ~ age + time + sex,
                    data = data,
                    exposure = popn)
    expect_true(is_same_priors(mod1, mod2))
})

test_that("'is_same_priors' returns FALSE when different numbers of priors", {
    set.seed(0)
    data <- expand.grid(age = 0:9, time = 2000:2005, sex = c("F", "M"))
    data$popn <- rpois(n = nrow(data), lambda = 100)
    data$deaths <- rpois(n = nrow(data), lambda = 10)
    mod1 <- mod_pois(formula = deaths ~ age + sex,
                    data = data,
                    exposure = popn)
    mod2 <- mod_pois(formula = deaths ~ age + time + sex,
                    data = data,
                    exposure = popn)
    expect_false(is_same_priors(mod1, mod2))
})

test_that("'is_same_priors' returns FALSE when priors have different classes", {
    set.seed(0)
    data <- expand.grid(age = 0:9, time = 2000:2005, sex = c("F", "M"))
    data$popn <- rpois(n = nrow(data), lambda = 100)
    data$deaths <- rpois(n = nrow(data), lambda = 10)
    mod1 <- mod_pois(formula = deaths ~ age + sex + time,
                    data = data,
                    exposure = popn) |>
                    set_prior(age ~ N())
    mod2 <- mod_pois(formula = deaths ~ age + time + sex,
                    data = data,
                    exposure = popn)
    expect_false(is_same_priors(mod1, mod2))
})


## make_diff_matrix -----------------------------------------------------------

test_that("'make_diff_matrix' works", {
    set.seed(0)
    m <- make_diff_matrix(10)
    x <- rnorm(10)
    ans_obtained <- as.numeric(m %*% x)
    ans_expected <- diff(x)
    expect_equal(ans_obtained, ans_expected)
})


## make_rw_matrix -------------------------------------------------------------

test_that("'make_rw_matrix' works", {
    set.seed(0)
    m <- make_rw_matrix(10)
    x <- rnorm(10)
    ans_obtained <- as.numeric(m %*% x)
    ans_expected <- c(diff(x), mean(x))
    expect_equal(ans_obtained, ans_expected)
})


## make_rw2_matrix ------------------------------------------------------------

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


## make_vals_linpred_par ------------------------------------------------------

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


## make_vals_linpred_season ---------------------------------------------------

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


## 'report_sim' ---------------------------------------------------------------

test_that("'report_sim' works when mod_sim is identical to mod_est", {
    set.seed(10)
    data <- expand.grid(age = 0:9, time = 2000:2006, sex = c("F", "M"))
    data$popn <- rpois(n = nrow(data), lambda = 100)
    data$deaths <- rpois(n = nrow(data), lambda = 10)
    formula <- deaths ~ age * time + sex
    mod <- mod_pois(formula = formula,
                    data = data,
                    exposure = popn) |>
                    set_prior(age ~ RW2()) |>
                    set_prior(`(Intercept)` ~ NFix(sd = 0.01))
    ans_obtained <- report_sim(mod, n_sim = 3)
    expect_true(is.data.frame(ans_obtained))
})
