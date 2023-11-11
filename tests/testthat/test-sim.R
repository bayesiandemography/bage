
## 'assess_performance' -------------------------------------------------------

test_that("'assess_performance' works with valid inputs - include_upper is TRUE", {
    set.seed(0)
    data <- expand.grid(age = 0:9, time = 2000:2005, sex = c("F", "M"))
    data$popn <- rpois(n = nrow(data), lambda = 100)
    data$deaths <- rpois(n = nrow(data), lambda = 10)
    formula <- deaths ~ age + sex + time
    mod <- mod_pois(formula = formula,
                    data = data,
                    exposure = popn)
    mod <- set_season(mod, n = 2)
    vals <- draw_vals_mod(mod, n_sim = 3)
    vals_sim <- get_vals_sim_one(vals, i_sim = 3)
    ans <- assess_performance(vals_sim = vals_sim,
                              mod_est = mod,
                              point_est_fun = "mean",
                              include_upper = TRUE,
                              widths = c(0.5, 0.95))
    expect_identical(sapply(ans, length),
                     c(vals_sim = 5L, error_point_est = 5L, is_in_interval = 5L))
})

test_that("'assess_performance' works with valid inputs - include_upper is FALSE", {
    set.seed(0)
    data <- expand.grid(age = 0:9, time = 2000:2005, sex = c("F", "M"))
    data$popn <- rpois(n = nrow(data), lambda = 100)
    data$deaths <- rpois(n = nrow(data), lambda = 10)
    formula <- deaths ~ age + sex + time
    mod <- mod_pois(formula = formula,
                    data = data,
                    exposure = popn)
    mod <- set_season(mod, n = 2)
    vals <- draw_vals_mod(mod, n_sim = 3)
    vals_sim <- get_vals_sim_one(vals, i_sim = 3)
    ans <- assess_performance(vals_sim = vals_sim,
                              mod_est = mod,
                              point_est_fun = "mean",
                              include_upper = FALSE,
                              widths = c(0.5, 0.95))
    expect_identical(sapply(ans, length),
                     c(vals_sim = 2L, error_point_est = 2L, is_in_interval = 2L))
})


## calc_error_point_est -------------------------------------------------------

test_that("'calc_error_point_est' works", {
    estimate <- list(a = rvec::rvec_dbl(matrix(1:12, nr = 3)),
                     b = rvec::rvec_dbl(matrix(13:24, nr = 3)))
    truth <- list(a = 1:3, b = 4:6)
    point_est_fun <- "mean"
    ans_obtained <- calc_error_point_est(estimate = estimate,
                                         truth = truth,
                                         point_est_fun = point_est_fun)
    ans_expected <- list(a = calc_error_point_est_one(estimate = estimate[[1]],
                                                      truth = truth[[1]],
                                                      rvec_fun = rvec::draws_mean),
                         b = calc_error_point_est_one(estimate = estimate[[2]],
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

test_that("'calc_is_in_interval' works - include_upper is TRUE", {
    estimate <- list(par = rvec::rvec_dbl(matrix(1:12, nr = 3)),
                     b = rvec::rvec_dbl(matrix(13:24, nr = 3)))
    truth <- list(par = 1:3, b = 4:6)
    widths <- c(0.5, 0.9, 1)
    ans_obtained <- calc_is_in_interval(estimate = estimate,
                                        truth = truth,
                                        widths = widths)
    ans_expected <- list(par = calc_is_in_interval_one(estimate = estimate[[1]],
                                                     truth = truth[[1]],
                                                     widths = widths),
                         b = calc_is_in_interval_one(estimate = estimate[[2]],
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


## draw_vals_ar ---------------------------------------------------------------

test_that("'draw_vals_ar' works with p = 1", {
    set.seed(0)
    coef <- draw_vals_ar_coef(p = 1, shape1 = 2, shape2 = 2)
    ans <- draw_vals_ar(n = 1000, coef = coef, sd = 0.3)
    expect_equal(sd(ans), 0.3, tolerance = 0.01)
})

test_that("'draw_vals_ar' works with p = 2", {
    set.seed(0)
    coef <- draw_vals_ar_coef(p = 2, shape1 = 2, shape2 = 2)
    ans <- draw_vals_ar(n = 1000, coef = coef, sd = 0.3)
    expect_equal(sd(ans), 0.3, tolerance = 0.01)
})

test_that("'draw_vals_ar' works with p = 10", {
    set.seed(0)
    coef <- draw_vals_ar_coef(p = 10, shape1 = 2, shape2 = 2)
    ans <- draw_vals_ar(n = 10000, coef = coef, sd = 0.3)
    expect_equal(sd(ans), 0.3, tolerance = 0.02)
})


## draw_vals_ar_coef ----------------------------------------------------------

test_that("'draw_vals_ar_coef' works with p = 1", {
    set.seed(0)
    ans <- draw_vals_ar_coef(p = 1, shape1 = 2, shape2 = 2)
    expect_true(abs(ans) < 1)
})

test_that("'draw_vals_ar_coef' works with p = 2", {
    set.seed(0)
    ans <- draw_vals_ar_coef(p = 2, shape1 = 2, shape2 = 2)
    expect_true(all(abs(ans) < 1))
})

test_that("'draw_vals_ar_coef' works with p = 10", {
    ans <- draw_vals_ar_coef(p = 2, shape1 = 2, shape2 = 2)
    expect_true(all(abs(ans) < 1))
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


## 'draw_vals_cyclical' -------------------------------------------------------

test_that("'draw_vals_cyclical' works", {
    set.seed(0)
    data <- expand.grid(age = 0:9, time = 2001:2007, sex = c("F", "M"))
    data$popn <- rpois(n = nrow(data), lambda = 100)
    data$deaths <- rpois(n = nrow(data), lambda = 10)
    formula <- deaths ~ age + sex + time
    mod <- mod_pois(formula = formula,
                    data = data,
                    exposure = popn)
    mod <- set_cyclical(mod, n = 2)
    mod <- set_prior(mod, age ~ Spline())
    mod <- set_prior(mod, time ~ RW2())
    ans <- draw_vals_cyclical(mod, n_sim = 100)
    expect_identical(length(ans$sd), 100L)
    expect_identical(nrow(ans$coef), 2L)
    expect_identical(ncol(ans$coef), 100L)
    expect_identical(nrow(ans$cyclical), 7L)
    expect_identical(ncol(ans$cyclical), 100L)
    expect_equal(mean(ans$cyclical), 0, tolerance = 0.05)
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

test_that("'draw_vals_hyperparam' works - has cyclical, season and disp", {
    set.seed(0)
    data <- expand.grid(age = 0:9, time = 2001:2007, sex = c("F", "M"))
    data$popn <- rpois(n = nrow(data), lambda = 100)
    data$deaths <- rpois(n = nrow(data), lambda = 10)
    formula <- deaths ~ age + sex + time
    mod <- mod_pois(formula = formula,
                    data = data,
                    exposure = popn)
    mod <- set_cyclical(mod)
    mod <- set_season(mod, n = 2)
    ans <- draw_vals_hyperparam(mod, n_sim = 10)
    expect_identical(names(ans),
                     c("effect", "hyper", "disp", "cyclical", "season", "linpred"))
    expect_false(any(sapply(ans, is.null)))
})

test_that("'draw_vals_hyperparam' works - no cyclical, season or disp", {
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
                     c("effect", "hyper", "disp", "cyclical", "season", "linpred"))
    expect_identical(sapply(ans, is.null),
                     c(effect = FALSE,
                       hyper = FALSE,
                       disp = TRUE,
                       cyclical = TRUE,
                       season = TRUE,
                       linpred = FALSE))
})


## draw_vals_effect_mod ----------------------------------------------------------

test_that("'draw_vals_effect_mod' works with bage_mod_pois", {
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
    ans <- draw_vals_effect_mod(mod,
                                vals_hyper = vals_hyper,
                                n_sim = n_sim)
    expect_identical(names(ans), c("(Intercept)", "age", "time", "sex", "age:time"))
    expect_true(all(sapply(ans, ncol) == n_sim))
    expect_identical(sapply(ans, nrow), sapply(mod$matrices_effect_outcome, ncol))
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
    mod <- set_cyclical(mod)
    mod <- set_season(mod, n = 2)
    mod <- fit(mod)
    ans_obtained <- get_vals_hyperparam_est(mod)
    components <- components(mod)
    ans_expected <- list(effect = subset(components, component == "effect", select = ".fitted")[[1]],
                         hyper = subset(components, component == "hyper", select = ".fitted")[[1]],
                         disp = subset(components, component == "disp", select = ".fitted")[[1]],
                         cyclical = subset(components, component == "cyclical",
                                           select = ".fitted")[[1]],
                         season = subset(components, component == "season", select = ".fitted")[[1]],
                         linpred = make_linpred_effect(mod, components) +
                             make_linpred_cyclical(mod, components) +
                             make_linpred_season(mod, components))
    names(ans_expected[[1L]]) <- paste0(subset(components, component == "effect",
                                               select = "term")[[1]],
                                        subset(components, component == "effect",
                                               select = "level")[[1]])
    names(ans_expected[[2L]]) <- paste0(subset(components, component == "hyper",
                                               select = "term")[[1]],
                                        subset(components, component == "hyper",
                                               select = "level")[[1]])
    names(ans_expected[[3L]]) <- paste0(subset(components, component == "disp",
                                               select = "term")[[1]],
                                        subset(components, component == "disp",
                                               select = "level")[[1]])
    names(ans_expected[[4L]]) <- paste0(subset(components, component == "cyclical",
                                               select = "term")[[1]],
                                        subset(components, component == "cyclical",
                                               select = "level")[[1]])
    names(ans_expected[[5L]]) <- paste0(subset(components, component == "season",
                                               select = "term")[[1]],
                                        subset(components, component == "season",
                                               select = "level")[[1]])
    expect_identical(ans_obtained, ans_expected)
})


## 'get_vals_sim_one' ---------------------------------------------------------

test_that("'get_vals_sim_one' works - include_upper = TRUE", {
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
    ans_obtained <- get_vals_sim_one(i_sim = 3, vals = vals)
    ans_expected <- list(effect = list("(Intercept)" = vals$effect[["(Intercept)"]][,3,drop = FALSE],
                                       age = vals$effect$age[, 3, drop = FALSE],
                                       sex = vals$effect$sex[, 3, drop = FALSE],
                                       time = vals$effect$time[, 3, drop = FALSE]),
                         hyper = list("(Intercept)" = list(),
                                      age = list(sd = vals$hyper$age$sd[3]),
                                      sex = list(sd = vals$hyper$sex$sd[3]),
                                      time = list(sd = vals$hyper$time$sd[3])),
                         disp = vals$disp[3],
                         cyclical = NULL,
                         season = list(season = vals$season$season[,3,drop = FALSE],
                                       sd = vals$season$sd[3]),
                         par = vals$par[,3,drop = FALSE],
                         outcome = vals$outcome[,3,drop = FALSE])
    expect_equal(as.numeric(unlist(ans_obtained)),
                 as.numeric(unlist(ans_expected)))
})


## 'is_same_cyclical' ---------------------------------------------------------

test_that("'is_same_cyclical' returns TRUE when cyclical effect same", {
    set.seed(0)
    data <- expand.grid(age = 0:9, time = 2000:2005, sex = c("F", "M"))
    data$popn <- rpois(n = nrow(data), lambda = 100)
    data$deaths <- rpois(n = nrow(data), lambda = 10)
    mod <- mod_pois(formula = deaths ~ age + sex + time,
                    data = data,
                    exposure = popn)
    mod <- set_cyclical(mod, n = 2)
    expect_true(is_same_cyclical(mod, mod))
})

test_that("'is_same_cyclical' returns TRUE when different n", {
    set.seed(0)
    data <- expand.grid(age = 0:9, time = 2000:2005, sex = c("F", "M"))
    data$popn <- rpois(n = nrow(data), lambda = 100)
    data$deaths <- rpois(n = nrow(data), lambda = 10)
    mod <- mod_pois(formula = deaths ~ age + sex + time,
                    data = data,
                    exposure = popn)
    mod_est <- set_cyclical(mod, n = 2)
    mod_sim <- set_cyclical(mod, n = 3)
    expect_false(is_same_cyclical(mod_est, mod_sim))
})

test_that("'is_same_cyclical' returns TRUE when different 'scale'", {
    set.seed(0)
    data <- expand.grid(age = 0:9, time = 2000:2010, sex = c("F", "M"))
    data$popn <- rpois(n = nrow(data), lambda = 100)
    data$deaths <- rpois(n = nrow(data), lambda = 10)
    mod <- mod_pois(formula = deaths ~ age + sex + time,
                    data = data,
                    exposure = popn)
    mod_est <- set_cyclical(mod, n = 2)
    mod_sim <- set_cyclical(mod, n = 2, s = 0.1)
    expect_false(is_same_cyclical(mod_est, mod_sim))
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


## 'is_same_season' -----------------------------------------------------------

test_that("'is_same_season' returns TRUE when season effect same", {
    set.seed(0)
    data <- expand.grid(age = 0:9, time = 2000:2005, sex = c("F", "M"))
    data$popn <- rpois(n = nrow(data), lambda = 100)
    data$deaths <- rpois(n = nrow(data), lambda = 10)
    mod <- mod_pois(formula = deaths ~ age + sex + time,
                    data = data,
                    exposure = popn)
    mod <- set_season(mod, n = 2)
    expect_true(is_same_season(mod, mod))
})

test_that("'is_same_season' returns TRUE when different n", {
    set.seed(0)
    data <- expand.grid(age = 0:9, time = 2000:2005, sex = c("F", "M"))
    data$popn <- rpois(n = nrow(data), lambda = 100)
    data$deaths <- rpois(n = nrow(data), lambda = 10)
    mod <- mod_pois(formula = deaths ~ age + sex + time,
                    data = data,
                    exposure = popn)
    mod_est <- set_season(mod, n = 2)
    mod_sim <- set_season(mod, n = 3)
    expect_false(is_same_season(mod_est, mod_sim))
})

test_that("'is_same_season' returns TRUE when different 'by'", {
    set.seed(0)
    data <- expand.grid(age = 0:9, time = 2000:2010, sex = c("F", "M"))
    data$popn <- rpois(n = nrow(data), lambda = 100)
    data$deaths <- rpois(n = nrow(data), lambda = 10)
    mod <- mod_pois(formula = deaths ~ age + sex + time,
                    data = data,
                    exposure = popn)
    mod_est <- set_season(mod, n = 2, by = age)
    mod_sim <- set_season(mod, n = 2, by = sex)
    expect_false(is_same_season(mod_est, mod_sim))
})

test_that("'is_same_season' returns TRUE when different 'scale'", {
    set.seed(0)
    data <- expand.grid(age = 0:9, time = 2000:2010, sex = c("F", "M"))
    data$popn <- rpois(n = nrow(data), lambda = 100)
    data$deaths <- rpois(n = nrow(data), lambda = 10)
    mod <- mod_pois(formula = deaths ~ age + sex + time,
                    data = data,
                    exposure = popn)
    mod_est <- set_season(mod, n = 2)
    mod_sim <- set_season(mod, n = 2, s = 0.1)
    expect_false(is_same_season(mod_est, mod_sim))
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


## 'make_id_vars_report' -------------------------------------------------------------

test_that("'make_id_vars_report' works with include_upper TRUE", {
    set.seed(0)
    data <- expand.grid(age = 0:9, time = 2000:2005, sex = c("F", "M"))
    data$popn <- rpois(n = nrow(data), lambda = 100)
    data$deaths <- rpois(n = nrow(data), lambda = 10)
    mod <- mod_pois(formula = deaths ~ age + sex + time,
                    data = data,
                    exposure = popn)
    ans_obtained <- make_id_vars_report(mod, include_upper = TRUE)
    expect_identical(names(ans_obtained), c("component", "term", "level"))
    expect_setequal(ans_obtained$component, c("effect", "hyper", "disp", "par"))
})

test_that("'make_id_vars_report' works with include_upper FALSE", {
    set.seed(0)
    data <- expand.grid(age = 0:9, time = 2000:2005, sex = c("F", "M"))
    data$popn <- rpois(n = nrow(data), lambda = 100)
    data$deaths <- rpois(n = nrow(data), lambda = 10)
    mod <- mod_pois(formula = deaths ~ age + sex + time,
                    data = data,
                    exposure = popn) |>
                    set_season(n = 2)
    ans_obtained <- make_id_vars_report(mod, include_upper = FALSE)
    expect_identical(names(ans_obtained), c("component", "term", "level"))
    expect_setequal(ans_obtained$component, c("disp", "par"))
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


## 'make_vals_linpred_cylical' ------------------------------------------------

test_that("'make_vals_linpred_cyclical' works", {
    set.seed(0)
    data <- expand.grid(age = 0:9, time = 2000:2006, sex = c("F", "M"))
    data$popn <- rpois(n = nrow(data), lambda = 100)
    data$deaths <- rpois(n = nrow(data), lambda = 10)
    formula <- deaths ~ age * time + sex
    mod <- mod_pois(formula = formula,
                    data = data,
                    exposure = popn)
    mod <- set_cyclical(mod, n = 2)
    n_sim <- 10L
    vals_cyclical <- draw_vals_cyclical(mod, n_sim = n_sim)
    ans <- make_vals_linpred_cyclical(mod = mod,
                                      vals_cyclical = vals_cyclical)
    expect_identical(nrow(ans), length(mod$outcome))
    expect_identical(ncol(ans), n_sim)
    expect_true(all(apply(ans, 2, function(x) length(unique(x))) == 7L))
})


## make_vals_linpred_effect ------------------------------------------------------

test_that("'make_vals_linpred_effect' works", {
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
    vals_effect <- draw_vals_effect_mod(mod,
                                        vals_hyper = vals_hyper,
                                        n_sim = n_sim)
    ans <- make_vals_linpred_effect(mod = mod,
                                    vals_effect = vals_effect)
    expect_identical(nrow(ans), length(mod$outcome))
    expect_identical(ncol(ans), n_sim)
})


## 'make_vals_linpred_season' -------------------------------------------------

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


## 'reformat_performance_vec' -------------------------------------------------

test_that("'reformat_performance_vec' works with valid input", {
    x <- list(list(a = c(0, 0.1),
                   b = c(1, 1.1, 1.2)),
              list(a = c(10, 10.1),
                   b = c(11, 11.1, 11.2)))
    ans_obtained <- reformat_performance_vec(x)
    ans_expected <- rvec::rvec_dbl(cbind(c(0, 0.1, 1, 1.1, 1.2),
                                         c(10, 10.1, 11, 11.1, 11.2)))
    expect_identical(ans_obtained, ans_expected)
})


## 'reformat_is_in_interval' --------------------------------------------------

test_that("'reformat_is_in_interval' works with valid input", {
    x <- list(list(a = list("0.5" = c(T,F), "0.95" = c(F,T)),
                   b = list("0.5" = c(F,T,F), "0.95" = c(T,F,T))),
              list(a = list("0.5" = c(T,F), "0.95" = c(F,T)),
                   b = list("0.5" = c(F,T,F), "0.95" = c(F,T,F))))
    ans_obtained <- reformat_is_in_interval(x)
    ans_expected <- tibble::tibble(coverage.0.5 =
                                       rvec::rvec_lgl(cbind(c(T,F,F,T,F),c(T,F,F,T,F))),
                                   coverage.0.95 =
                                       rvec::rvec_lgl(cbind(c(F,T,T,F,T), c(F,T,F,T,F))))
    expect_identical(ans_obtained, ans_expected)
})


## 'report_sim' ---------------------------------------------------------------

test_that("'report_sim' works when mod_sim is identical to mod_est - short", {
    set.seed(0)
    data <- expand.grid(age = 0:9, sex = c("F", "M"))
    data$popn <- rpois(n = nrow(data), lambda = 100)
    data$deaths <- rpois(n = nrow(data), lambda = 10)
    formula <- deaths ~ age + sex
    mod <- mod_pois(formula = formula,
                    data = data,
                    exposure = popn) |>
                    set_prior(`(Intercept)` ~ NFix(sd = 0.01))
    ans_obtained <- report_sim(mod, n_sim = 2)
    expect_true(is.data.frame(ans_obtained))
    expect_identical(nrow(ans_obtained), 4L)
})

test_that("'report_sim' works when mod_sim is identical to mod_est - long", {
    set.seed(0)
    data <- expand.grid(age = 0:9, sex = c("F", "M"))
    data$popn <- rpois(n = nrow(data), lambda = 100)
    data$deaths <- rpois(n = nrow(data), lambda = 10)
    formula <- deaths ~ age + sex
    mod <- mod_pois(formula = formula,
                    data = data,
                    exposure = popn) |>
                    set_prior(`(Intercept)` ~ NFix(sd = 0.01))
    ans_obtained <- report_sim(mod,
                               n_sim = 2,
                               report_type = "long",
                               point_est_fun = "med")
    expect_true(is.data.frame(ans_obtained))
})


## 'summarise_sim' ---------------------------------------------------------------

test_that("'summarise_sim' works with valid inputs", {
    set.seed(0)
    data <- expand.grid(age = 0:9, sex = c("F", "M"))
    data$popn <- rpois(n = nrow(data), lambda = 100)
    data$deaths <- rpois(n = nrow(data), lambda = 10)
    formula <- deaths ~ age + sex
    mod <- mod_pois(formula = formula,
                    data = data,
                    exposure = popn) |>
                    set_prior(`(Intercept)` ~ NFix(sd = 0.01))
    data <- report_sim(mod, n_sim = 2, report_type = "long")
    ans <- summarise_sim(data)
    expect_identical(names(ans), setdiff(names(data), c("term", "level")))
    expect_true(all(sapply(ans[-1], is.double)))
})



## 'transpose_list' -----------------------------------------------------------

test_that("'transpose_list' works with valid inputs - all lengths at least 1", {
    l <- list(a = list(x = 1, y = 2, z = 3),
              b = list(x = 10, y = 20, z = 30))
    ans_obtained <- transpose_list(l)
    ans_expected <- list(list(1, 10),
                         list(2, 20),
                         list(3, 30))
    expect_identical(ans_obtained, ans_expected)
})
