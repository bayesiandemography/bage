
## ## 'assess_performance' -------------------------------------------------------

## test_that("'assess_performance' works with valid inputs - include_upper is TRUE", {
##     set.seed(0)
##     data <- expand.grid(age = 0:9, time = 2000:2005, sex = c("F", "M"))
##     data$popn <- rpois(n = nrow(data), lambda = 100)
##     data$deaths <- rpois(n = nrow(data), lambda = 10)
##     formula <- deaths ~ age + sex + time
##     mod <- mod_pois(formula = formula,
##                     data = data,
##                     exposure = popn)
##     mod <- set_season(mod, n = 2)
##     vals <- draw_vals_mod(mod, n_sim = 3)
##     vals_sim <- get_vals_sim_one(vals, i_sim = 3)
##     ans <- assess_performance(vals_sim = vals_sim,
##                               mod_est = mod,
##                               point_est_fun = "mean",
##                               include_upper = TRUE,
##                               widths = c(0.5, 0.95))
##     expect_identical(sapply(ans, length),
##                      c(vals_sim = 5L,
##                        error_point_est = 5L,
##                        is_in_interval = 5L,
##                        width_interval = 5L))
## })

## test_that("'assess_performance' works with valid inputs - include_upper is FALSE", {
##     set.seed(0)
##     data <- expand.grid(age = 0:9, time = 2000:2005, sex = c("F", "M"))
##     data$popn <- rpois(n = nrow(data), lambda = 100)
##     data$deaths <- rpois(n = nrow(data), lambda = 10)
##     formula <- deaths ~ age + sex + time
##     mod <- mod_pois(formula = formula,
##                     data = data,
##                     exposure = popn)
##     mod <- set_season(mod, n = 2)
##     vals <- draw_vals_mod(mod, n_sim = 3)
##     vals_sim <- get_vals_sim_one(vals, i_sim = 3)
##     ans <- assess_performance(vals_sim = vals_sim,
##                               mod_est = mod,
##                               point_est_fun = "mean",
##                               include_upper = FALSE,
##                               widths = c(0.5, 0.95))
##     expect_identical(sapply(ans, length),
##                      c(vals_sim = 2L,
##                        error_point_est = 2L,
##                        is_in_interval = 2L,
##                        width_interval = 2L))
## })


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


## calc_interval_width --------------------------------------------------------

test_that("'calc_interval_width' works - include_upper is TRUE", {
    estimate <- list(par = rvec::rvec_dbl(matrix(1:12, nr = 3)),
                     b = rvec::rvec_dbl(matrix(13:24, nr = 3)))
    truth <- list(par = 1:3, b = 4:6)
    widths <- c(0.5, 0.9, 1)
    ans_obtained <- calc_interval_width(estimate = estimate,
                                        widths = widths)
    ans_expected <- list(par = calc_interval_width_one(estimate = estimate[[1]],
                                                       widths = widths),
                         b = calc_interval_width_one(estimate = estimate[[2]],
                                                     widths = widths))
    expect_equal(ans_obtained, ans_expected)
})


## calc_interval_width_one ---------------------------------------------------

test_that("'calc_interval_width_one' works", {
    estimate <- rvec::rvec_dbl(matrix(1:12, nr = 3))
    widths <- c(0.9, 1)
    ans_obtained <- calc_interval_width_one(estimate = estimate,
                                            widths = widths)
    ci.0.9 <- rvec::draws_ci(estimate, width = 0.9)
    ci.1 <- rvec::draws_ci(estimate, width = 1)
    ans_expected <- list("0.9" = ci.0.9$estimate.upper - ci.0.9$estimate.lower,
                         "1" = ci.1$estimate.upper - ci.1$estimate.lower)
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


## 'draw_vals_ar' -------------------------------------------------------------

test_that("'draw_vals_ar' works", {
    set.seed(0)
    prior <- AR(n = 2)
    coef <- draw_vals_coef(prior, n_sim = 5L)
    ans <- draw_vals_ar(n = 10000, coef = coef, sd = seq(0.6, 1, 0.1))
    expect_identical(nrow(ans), 10000L)
    expect_identical(ncol(ans), 5L)
    expect_equal(apply(ans, 2, sd), seq(0.6, 1, 0.1), tolerance = 0.02)
})


## 'draw_vals_ar_one' ---------------------------------------------------------

test_that("'draw_vals_ar_one' works", {
    set.seed(0)
    prior <- AR(n = 2)
    coef <- draw_vals_coef(prior, n_sim = 1L)
    ans <- draw_vals_ar_one(n = 1000, coef = coef, sd = 0.5)
    expect_identical(length(ans), 1000L)
    expect_equal(sd(ans), 0.5, tolerance = 0.01)
})


## 'draw_vals_coef' -----------------------------------------------------------

test_that("'draw_vals_coef' works with n = 1", {
  set.seed(0)
  prior <- AR1()
  ans <- draw_vals_coef(prior, n_sim = 5)
  expect_true(all(ans > 0.8))
  expect_true(all(ans < 0.98))
  prior <- AR(n = 1)
  ans <- draw_vals_coef(prior, n_sim = 5)
  expect_true(all(ans > -1))
  expect_true(all(ans < 1))
})

test_that("'draw_vals_coef' works with n = 2", {
  set.seed(0)
  prior <- AR(n = 2)
  ans <- draw_vals_coef(prior, n_sim = 3)
  expect_true(all(abs(ans) < 1))
  expect_identical(dim(ans), c(2L, 3L))
})

test_that("'draw_vals_coef' works with n = 10", {
  set.seed(0)
  prior <- AR(n = 10)
  ans <- draw_vals_coef(prior, n_sim = 3)
  expect_true(all(abs(ans) < 1))
  expect_identical(dim(ans), c(10L, 3L))
})


## 'draw_vals_components' -----------------------------------------------------

test_that("'draw_vals_components' works", {
  set.seed(0)
  data <- expand.grid(age = 0:9, time = 2000:2005, sex = c("F", "M"))
  data$popn <- rpois(n = nrow(data), lambda = 100)
  data$deaths <- rpois(n = nrow(data), lambda = 10)
  formula <- deaths ~ age * time + sex
  mod <- mod_pois(formula = formula,
                  data = data,
                  exposure = popn)
  mod <- set_prior(mod, time ~ compose_time(RW(), seasonal = Seas(n = 2), error = N()))
  n_sim <- 2
  ans <- draw_vals_components(mod = mod, n_sim = n_sim)
  ans_est <- components(fit(mod))
  comb <- merge(ans, ans_est, by = c("component", "term", "level"), all.x = TRUE,
                all.y = TRUE)
  expect_true(identical(nrow(comb), nrow(ans)))
})


## 'draw_vals_elin' -----------------------------------------------------------

test_that("'draw_vals_elin' works - along dimension is first", {
  set.seed(0)
  prior <- ELin()
  n_sim <- 10
  matrix_along_by <- matrix(0:11, nr = 3)
  colnames(matrix_along_by) <- 11:14
  slope <- draw_vals_slope(prior = prior,
                           n_sim = n_sim)
  sd <- draw_vals_sd(prior = prior,
                     n_sim = n_sim)
  msd <- draw_vals_msd(prior = prior,
                       n_sim = n_sim)
  mslope <- draw_vals_mslope(slope = slope,
                             msd = msd,
                             matrix_along_by = matrix_along_by,
                             n_sim = n_sim)
  labels <- 1:12
  set.seed(0)
  ans_obtained <- draw_vals_elin(mslope = mslope,
                                 sd = sd,
                                 matrix_along_by = matrix_along_by,
                                 labels = labels)
  q <- -(3 + 1) / (3 - 1) + (1:3) * 2 / (3 - 1)
  mean <- matrix(q, nrow = 12, ncol = n_sim) *
    matrix(rep(mslope, each = 3), nrow = 12)
  sd <- matrix(rep(sd, each = 12), ncol = n_sim)
  set.seed(0)
  ans_expected <- matrix(rnorm(n = 12 * n_sim, mean = mean, sd = sd),
                         ncol = n_sim)
  dimnames(ans_expected) <- list(1:12, 1:n_sim)
  expect_equal(ans_obtained, ans_expected)  
})

test_that("'draw_vals_elin' works - along dimension is second", {
  set.seed(0)
  prior <- ELin()
  n_sim <- 10
  matrix_along_by <- t(matrix(0:11, nr = 3))
  colnames(matrix_along_by) <- 1:3
  slope <- draw_vals_slope(prior = prior,
                           n_sim = n_sim)
  sd <- draw_vals_sd(prior = prior,
                     n_sim = n_sim)
  msd <- draw_vals_msd(prior = prior,
                       n_sim = n_sim)
  mslope <- draw_vals_mslope(slope = slope,
                             msd = msd,
                             matrix_along_by = matrix_along_by,
                             n_sim = n_sim)
  labels <- 1:12
  set.seed(0)
  ans_obtained <- draw_vals_elin(mslope = mslope,
                                 sd = sd,
                                 matrix_along_by = matrix_along_by,
                                 labels = labels)
  q <- -(4 + 1) / (4 - 1) + (1:4) * 2 / (4 - 1)
  mean <- matrix(q, nrow = 12, ncol = n_sim) *
    matrix(rep(mslope, each = 4), nrow = 12)
  sd <- matrix(rep(sd, each = 12), ncol = n_sim)
  set.seed(0)
  ans_expected <- matrix(rnorm(n = 12 * n_sim, mean = mean, sd = sd),
                         ncol = n_sim)
  ans_expected <- ans_expected[c(1, 5, 9, 2, 6, 10, 3, 7, 11, 4, 8, 12),]
  dimnames(ans_expected) <- list(1:12, 1:n_sim)
  expect_equal(ans_obtained, ans_expected)  
})


## 'draw_vals_erw' ----------------------------------------------------------

test_that("'draw_vals_erw' works - along dimension is first", {
  set.seed(0)
  prior <- ERW()
  n_sim <- 10
  matrix_along_by <- matrix(0:2999, nc = 3)
  sd <- draw_vals_sd(prior = prior,
                     n_sim = n_sim)
  labels <- 1:3000
  set.seed(0)
  ans <- draw_vals_erw(sd = sd,
                       matrix_along_by = matrix_along_by,
                       labels = labels)
  expect_identical(dim(ans), c(3000L, 10L))
  expect_identical(dimnames(ans), list(as.character(1:3000), as.character(1:10)))
  ans <- matrix(ans, nrow = 1000)
  expect_equal(mean(ans[1, ]),
               0,
               tolerance = 0.05)
  expect_equal(unname(apply(ans[-1,], 2, function(x) sd(diff(x)))),
               rep(sd, each = 3),
               tolerance = 0.05)
})

test_that("'draw_vals_erw' works - along dimension is second", {
  set.seed(0)
  prior <- ERW()
  n_sim <- 10
  matrix_along_by <- t(matrix(0:2999, nc = 1000))
  sd <- draw_vals_sd(prior = prior,
                     n_sim = n_sim)
  labels <- 1:3000
  set.seed(0)
  ans <- draw_vals_erw(sd = sd,
                       matrix_along_by = matrix_along_by,
                       labels = labels)
  expect_identical(dim(ans), c(3000L, 10L))
  expect_identical(dimnames(ans), list(as.character(1:3000), as.character(1:10)))
  ans <- array(ans, dim = c(3, 1000, 10))
  ans <- aperm(ans, perm = c(2, 1, 3))
  ans <- matrix(ans, nrow = 1000)
  expect_equal(mean(ans[1, ]),
               0,
               tolerance = 0.05)
  expect_equal(unname(apply(ans[-1,], 2, function(x) sd(diff(x)))),
               rep(sd, each = 3),
               tolerance = 0.03)
})


## 'draw_vals_eseas' ----------------------------------------------------------

test_that("'draw_vals_eseas' works - along dimension is first", {
  set.seed(0)
  prior <- ESeas(n = 4)
  n_sim <- 10
  matrix_along_by <- matrix(0:2999, nc = 3)
  sd <- draw_vals_sd(prior = prior,
                     n_sim = n_sim)
  labels <- 1:3000
  set.seed(0)
  ans <- draw_vals_eseas(n = 4,
                         sd = sd,
                         matrix_along_by = matrix_along_by,
                         labels = labels)
  expect_identical(dim(ans), c(3000L, 10L))
  expect_identical(dimnames(ans), list(as.character(1:3000), as.character(1:10)))
  ans <- matrix(ans, nrow = 1000)
  expect_equal(mean(ans[1:4, ]),
               0,
               tolerance = 0.02)
  expect_equal(unname(apply(ans[-(1:4),], 2, function(x) sd(diff(x, lag = 4)))),
               rep(sd, each = 3),
               tolerance = 0.05)
})

test_that("'draw_vals_eseas' works - along dimension is second", {
  set.seed(0)
  prior <- ESeas(n = 4)
  n_sim <- 10
  matrix_along_by <- t(matrix(0:2999, nc = 1000))
  sd <- draw_vals_sd(prior = prior,
                     n_sim = n_sim)
  labels <- 1:3000
  set.seed(0)
  ans <- draw_vals_eseas(n = 4,
                         sd = sd,
                         matrix_along_by = matrix_along_by,
                         labels = labels)
  expect_identical(dim(ans), c(3000L, 10L))
  expect_identical(dimnames(ans), list(as.character(1:3000), as.character(1:10)))
  ans <- array(ans, dim = c(3, 1000, 10))
  ans <- aperm(ans, perm = c(2, 1, 3))
  ans <- matrix(ans, nrow = 1000)
  expect_equal(mean(ans[1:4, ]),
               0,
               tolerance = 0.01)
  expect_equal(unname(apply(ans[-(1:4),], 2, function(x) sd(diff(x, lag = 4)))),
               rep(sd, each = 3),
               tolerance = 0.03)
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


## draw_vals_hyperrand_mod --------------------------------------------------------

test_that("'draw_vals_hyperrand_mod' works with bage_mod_pois", {
    set.seed(0)
    data <- expand.grid(age = 0:9, time = 2000:2005, sex = c("F", "M"))
    data$popn <- rpois(n = nrow(data), lambda = 100)
    data$deaths <- rpois(n = nrow(data), lambda = 10)
    formula <- deaths ~ age * time + sex
    mod <- mod_pois(formula = formula,
                    data = data,
                    exposure = popn)
    mod <- set_prior(mod, age:time ~ ELin())
    vals_hyper <- draw_vals_hyper_mod(mod, n_sim = 10)
    ans <- draw_vals_hyperrand_mod(mod, vals_hyper = vals_hyper, n_sim = 10)
    expect_identical(names(ans), c("(Intercept)", "age", "time", "sex", "age:time"))
    expect_identical(nrow(ans[["age:time"]]$mslope), 10L)
    expect_identical(sapply(ans, length),
                     c("(Intercept)" = 0L, age = 0L, time = 0L, sex = 0L, "age:time" = 1L))
})


## ## draw_vals_hyperparam -------------------------------------------------------

## test_that("'draw_vals_hyperparam' works - has cyclical, season and disp", {
##     set.seed(0)
##     data <- expand.grid(age = 0:9, time = 2001:2007, sex = c("F", "M"))
##     data$popn <- rpois(n = nrow(data), lambda = 100)
##     data$deaths <- rpois(n = nrow(data), lambda = 10)
##     formula <- deaths ~ age + sex + time
##     mod <- mod_pois(formula = formula,
##                     data = data,
##                     exposure = popn)
##     mod <- set_cyclical(mod)
##     mod <- set_season(mod, n = 2)
##     ans <- draw_vals_hyperparam(mod, n_sim = 10)
##     expect_identical(names(ans),
##                      c("effect", "hyper", "disp", "cyclical", "season", "linpred"))
##     expect_false(any(sapply(ans, is.null)))
## })

## test_that("'draw_vals_hyperparam' works - no cyclical, season or disp", {
##     set.seed(0)
##     data <- expand.grid(age = 0:9, time = 2001:2007, sex = c("F", "M"))
##     data$popn <- rpois(n = nrow(data), lambda = 100)
##     data$deaths <- rpois(n = nrow(data), lambda = 10)
##     formula <- deaths ~ age + sex + time
##     mod <- mod_pois(formula = formula,
##                     data = data,
##                     exposure = popn)
##     mod <- set_disp(mod, s = 0)
##     ans <- draw_vals_hyperparam(mod, n_sim = 10)
##     expect_identical(names(ans),
##                      c("effect", "hyper", "disp", "cyclical", "season", "linpred"))
##     expect_identical(sapply(ans, is.null),
##                      c(effect = FALSE,
##                        hyper = FALSE,
##                        disp = TRUE,
##                        cyclical = TRUE,
##                        season = TRUE,
##                        linpred = FALSE))
## })


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
  mod <- set_prior(mod, time ~ compose_time(RW(), seasonal = Seas(n = 2), error = N()))
  n_sim <- 2
  vals_hyper <- draw_vals_hyper_mod(mod, n_sim = n_sim)
  vals_hyperrand <- draw_vals_hyperrand_mod(mod,
                                            vals_hyper = vals_hyper,
                                            n_sim = n_sim)
  ans <- draw_vals_effect_mod(mod,
                              vals_hyper = vals_hyper,
                              vals_hyperrand = vals_hyperrand,
                              n_sim = n_sim)
  expect_identical(names(ans), c("(Intercept)", "age", "time", "sex", "age:time"))
  expect_true(all(sapply(ans, ncol) == n_sim))
  expect_identical(sapply(ans, nrow), sapply(mod$matrices_effect_outcome, ncol))
})


## 'draw_vals_lin' ------------------------------------------------------------

test_that("'draw_vals_lin' works", {
  set.seed(0)
  prior <- Lin()
  n_sim <- 10
  slope <- draw_vals_slope(prior = prior, n_sim = n_sim)
  sd <- draw_vals_sd(prior = prior, n_sim = n_sim)
  labels <- 1:20
  set.seed(0)
  ans_obtained <- draw_vals_lin(slope = slope,
                                sd = sd,
                                labels = labels)
  q <- -(20 + 1) / (20 - 1) + (1:20) * 2 / (20 - 1)
  ans_expected <- matrix(rep(q, times = n_sim) * rep(slope, each = 20),
                         nrow = 20)
  set.seed(0)
  ans_expected <- ans_expected + rnorm(n = n_sim * 20, sd = rep(sd, each = 20))
  dimnames(ans_expected) <- list(1:20, 1:n_sim)
  expect_equal(ans_obtained, ans_expected)  
})

## draw_vals_msd --------------------------------------------------------------

test_that("'draw_vals_msd' works", {
  prior <- ELin(ms = 0.5)
  n_sim <- 1000
  set.seed(0)
  ans_obtained <- draw_vals_msd(prior = prior, n_sim = n_sim)
  set.seed(0)
  ans_expected <- abs(rnorm(n = 1000, sd = 0.5))
  expect_identical(ans_obtained, ans_expected)
})


## draw_vals_mslope -----------------------------------------------------------

test_that("'draw_vals_mslope' works", {
  set.seed(0)
  prior <- ELin(ms = 0.5)
  n_sim <- 1000
  slope <- draw_vals_slope(prior = prior, n_sim = n_sim)
  msd <- draw_vals_msd(prior = prior, n_sim = n_sim)
  matrix_along_by <- matrix(0:11, nr = 3)
  colnames(matrix_along_by) <- 11:14
  set.seed(0)
  ans_obtained <- draw_vals_mslope(slope = slope,
                                   msd = msd,
                                   matrix_along_by = matrix_along_by,
                                   n_sim = n_sim)
  set.seed(0)
  ans_expected <- matrix(rnorm(n = 4000,
                               mean = rep(slope, each = 4),
                               sd = rep(msd, each = 4)),
                         nr = 4)
  rownames(ans_expected) <- paste("mslope", 11:14, sep = ".")
  expect_identical(ans_obtained, ans_expected)
})

  
## draw_vals_rw ---------------------------------------------------------------

test_that("'draw_vals_rw' works", {
    set.seed(0)
    prior <- RW()
    n_sim <- 1000
    sd <- draw_vals_sd(prior = prior, n_sim = n_sim)
    labels <- 1:200
    set.seed(0)
    ans <- draw_vals_rw(sd = sd,
                        labels = labels)
    expect_equal(mean(ans[1, ]),
                 0,
                 tolerance = 0.02)
    expect_equal(unname(apply(ans, 2, function(x) sd(diff(x)))),
                 sd,
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
  sd_slope <- 0.5
  labels <- 1:100
  set.seed(0)
  ans <- draw_vals_rw2(sd = sd,
                       sd_slope = sd_slope,
                       labels = labels)
  expect_equal(unname(apply(ans, 2, function(x) sd(diff(x, diff = 2)))),
               sd,
               tolerance = 0.1)
  expect_equal(sd(ans[1,]),
               1,
               tolerance = 0.1)
  expect_equal(sd(ans[2,]),
               sqrt(1 + sd_slope^2),
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


## draw_vals_seas -------------------------------------------------------------

test_that("'draw_vals_seas' works", {
  set.seed(0)
  prior <- Seas(n = 4)
  n_sim <- 1000
  sd <- draw_vals_sd(prior = prior, n_sim = n_sim)
  labels <- 1:200
  set.seed(0)
  ans <- draw_vals_seas(n = 4L,
                        sd = sd,
                        labels = labels)
  expect_equal(mean(ans[1:4, ]),
               0,
               tolerance = 0.02)
  expect_equal(unname(apply(ans, 2, function(x) sd(diff(x, lag = 4)))),
               sd,
               tolerance = 0.05)
  expect_identical(dim(ans), c(200L, 1000L))
  expect_identical(dimnames(ans),
                   list(as.character(seq_len(200)),
                        as.character(seq_len(1000))))
})


## draw_vals_slope ------------------------------------------------------------

test_that("'draw_vals_slope' works", {
  prior <- Lin(sd = 0.2)
  n_sim <- 10
  set.seed(0)
  ans_obtained <- draw_vals_slope(prior = prior, n_sim = n_sim)
  set.seed(0)
  ans_expected <- rnorm(n = 10, sd = 0.2)
  expect_identical(ans_obtained, ans_expected)
})


## ## 'draw_vals_season' ---------------------------------------------------------

## test_that("'draw_vals_season' works", {
##     set.seed(0)
##     data <- expand.grid(age = 0:9, time = 2001:2007, sex = c("F", "M"))
##     data$popn <- rpois(n = nrow(data), lambda = 100)
##     data$deaths <- rpois(n = nrow(data), lambda = 10)
##     formula <- deaths ~ age + sex + time
##     mod <- mod_pois(formula = formula,
##                     data = data,
##                     exposure = popn)
##     mod <- set_season(mod, n = 2)
##     mod <- set_prior(mod, age ~ Sp())
##     mod <- set_prior(mod, time ~ RW2())
##     ans <- draw_vals_season(mod, n_sim = 100)
##     expect_identical(length(ans$sd), 100L)
##     expect_identical(nrow(ans$season), 7L)
##     expect_identical(ncol(ans$season), 100L)
##     expect_equal(mean(apply(ans$season[c(1, 3, 5, 7),], 2, mean)), 0, tolerance = 0.01)
## })


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
  mod <- set_prior(mod,
                   time ~ compose_time(trend = RW2(), cyclical = AR(), seasonal = Seas(n = 2)))
  mod <- fit(mod)
  ans_obtained <- get_vals_hyperparam_est(mod)
  components <- components(mod)
  ans_expected <- list(effect = subset(components, component == "effect", select = ".fitted")[[1]],
                       hyper = subset(components, component == "hyper", select = ".fitted")[[1]],
                       trend = subset(components, component == "trend", select = ".fitted")[[1]],
                       cyclical = subset(components, component == "cyclical",
                                         select = ".fitted")[[1]],
                       seasonal = subset(components, component == "seasonal",
                                         select = ".fitted")[[1]],
                       error = NULL,
                       disp = subset(components, component == "disp", select = ".fitted")[[1]],
                       linpred = make_linpred_effect(mod, components))
  names(ans_expected[[1L]]) <- paste(subset(components, component == "effect",
                                            select = "term")[[1]],
                                     subset(components, component == "effect",
                                            select = "level")[[1]],
                                     sep = ".")
  names(ans_expected[[2L]]) <- paste(subset(components, component == "hyper",
                                            select = "term")[[1]],
                                     subset(components, component == "hyper",
                                            select = "level")[[1]],
                                     sep = ".")
  names(ans_expected[[3L]]) <- paste(subset(components, component == "trend",
                                            select = "term")[[1]],
                                     subset(components, component == "trend",
                                            select = "level")[[1]],
                                     sep = ".")
  names(ans_expected[[4L]]) <- paste(subset(components, component == "cyclical",
                                            select = "term")[[1]],
                                     subset(components, component == "cyclical",
                                            select = "level")[[1]],
                                     sep = ".")
  names(ans_expected[[5L]]) <- paste(subset(components, component == "seasonal",
                                            select = "term")[[1]],
                                     subset(components, component == "seasonal",
                                            select = "level")[[1]],
                                     sep = ".")
  names(ans_expected[[7L]]) <- paste(subset(components, component == "disp",
                                            select = "term")[[1]],
                                     subset(components, component == "disp",
                                            select = "level")[[1]],
                                     sep = ".")
  expect_identical(ans_obtained, ans_expected)
})


## ## 'get_vals_sim_one' ---------------------------------------------------------

## test_that("'get_vals_sim_one' works - include_upper = TRUE", {
##     set.seed(0)
##     data <- expand.grid(age = 0:9, time = 2000:2005, sex = c("F", "M"))
##     data$popn <- rpois(n = nrow(data), lambda = 100)
##     data$deaths <- rpois(n = nrow(data), lambda = 10)
##     formula <- deaths ~ age + sex + time
##     mod <- mod_pois(formula = formula,
##                     data = data,
##                     exposure = popn)
##     mod <- set_season(mod, n = 2)
##     vals <- draw_vals_mod(mod, n_sim = 10)
##     ans_obtained <- get_vals_sim_one(i_sim = 3, vals = vals)
##     ans_expected <- list(effect = list("(Intercept)" = vals$effect[["(Intercept)"]][,3,drop = FALSE],
##                                        age = vals$effect$age[, 3, drop = FALSE],
##                                        sex = vals$effect$sex[, 3, drop = FALSE],
##                                        time = vals$effect$time[, 3, drop = FALSE]),
##                          hyper = list("(Intercept)" = list(),
##                                       age = list(sd = vals$hyper$age$sd[3]),
##                                       sex = list(sd = vals$hyper$sex$sd[3]),
##                                       time = list(sd = vals$hyper$time$sd[3])),
##                          disp = vals$disp[3],
##                          cyclical = NULL,
##                          season = list(season = vals$season$season[,3,drop = FALSE],
##                                        sd = vals$season$sd[3]),
##                          par = vals$par[,3,drop = FALSE],
##                          outcome = vals$outcome[,3,drop = FALSE])
##     expect_equal(as.numeric(unlist(ans_obtained)),
##                  as.numeric(unlist(ans_expected)))
## })



## ## make_diff_matrix -----------------------------------------------------------

## test_that("'make_diff_matrix' works", {
##     set.seed(0)
##     m <- make_diff_matrix(10)
##     x <- rnorm(10)
##     ans_obtained <- as.numeric(m %*% x)
##     ans_expected <- diff(x)
##     expect_equal(ans_obtained, ans_expected)
## })


## ## 'make_id_vars_report' -------------------------------------------------------------

## test_that("'make_id_vars_report' works with include_upper TRUE", {
##     set.seed(0)
##     data <- expand.grid(age = 0:9, time = 2000:2005, sex = c("F", "M"))
##     data$popn <- rpois(n = nrow(data), lambda = 100)
##     data$deaths <- rpois(n = nrow(data), lambda = 10)
##     mod <- mod_pois(formula = deaths ~ age + sex + time,
##                     data = data,
##                     exposure = popn)
##     ans_obtained <- make_id_vars_report(mod, include_upper = TRUE)
##     expect_identical(names(ans_obtained), c("component", "term", "level"))
##     expect_setequal(ans_obtained$component, c("effect", "hyper", "disp", "par"))
## })

## test_that("'make_id_vars_report' works with include_upper FALSE", {
##     set.seed(0)
##     data <- expand.grid(age = 0:9, time = 2000:2005, sex = c("F", "M"))
##     data$popn <- rpois(n = nrow(data), lambda = 100)
##     data$deaths <- rpois(n = nrow(data), lambda = 10)
##     mod <- mod_pois(formula = deaths ~ age + sex + time,
##                     data = data,
##                     exposure = popn) |>
##                     set_season(n = 2)
##     ans_obtained <- make_id_vars_report(mod, include_upper = FALSE)
##     expect_identical(names(ans_obtained), c("component", "term", "level"))
##     expect_setequal(ans_obtained$component, c("disp", "par"))
## })


## ## make_sim_season_matrix -----------------------------------------------------

## test_that("'make_sim_season_matrix' works", {
##     set.seed(0)
##     m <- make_sim_season_matrix(n_time = 10, n_season = 2L)
##     x <- rnorm(10)
##     ans_obtained <- as.numeric(m %*% x)
##     ans_expected <- c(diff(x, lag = 2), mean(x))
##     expect_equal(ans_obtained, ans_expected)
## })


## ## make_rw2_matrix ------------------------------------------------------------

## test_that("'make_rw2_matrix' works", {
##     set.seed(0)
##     m <- make_rw2_matrix(10)
##     x <- rnorm(10)
##     ans_obtained <- as.numeric(m %*% x)
##     h <- seq(from = -1, to = 1, length.out = 10)
##     slope <- coef(lm(x ~ h))[["h"]]
##     ans_expected <- c(diff(x, diff = 2), mean(x), slope)
##     expect_equal(ans_obtained, ans_expected)
## })


## ## 'make_vals_linpred_cylical' ------------------------------------------------

## test_that("'make_vals_linpred_cyclical' works", {
##     set.seed(0)
##     data <- expand.grid(age = 0:9, time = 2000:2006, sex = c("F", "M"))
##     data$popn <- rpois(n = nrow(data), lambda = 100)
##     data$deaths <- rpois(n = nrow(data), lambda = 10)
##     formula <- deaths ~ age * time + sex
##     mod <- mod_pois(formula = formula,
##                     data = data,
##                     exposure = popn)
##     mod <- set_cyclical(mod, n = 2)
##     n_sim <- 10L
##     vals_cyclical <- draw_vals_cyclical(mod, n_sim = n_sim)
##     ans <- make_vals_linpred_cyclical(mod = mod,
##                                       vals_cyclical = vals_cyclical)
##     expect_identical(nrow(ans), length(mod$outcome))
##     expect_identical(ncol(ans), n_sim)
##     expect_true(all(apply(ans, 2, function(x) length(unique(x))) == 7L))
## })


## ## make_vals_linpred_effect ------------------------------------------------------

## test_that("'make_vals_linpred_effect' works", {
##     set.seed(0)
##     data <- expand.grid(age = 0:9, time = 2000:2005, sex = c("F", "M"))
##     data$popn <- rpois(n = nrow(data), lambda = 100)
##     data$deaths <- rpois(n = nrow(data), lambda = 10)
##     formula <- deaths ~ age * time + sex
##     mod <- mod_pois(formula = formula,
##                     data = data,
##                     exposure = popn)
##     n_sim <- 10L
##     vals_hyper <- draw_vals_hyper_mod(mod, n_sim = n_sim)
##     vals_effect <- draw_vals_effect_mod(mod,
##                                         vals_hyper = vals_hyper,
##                                         n_sim = n_sim)
##     ans <- make_vals_linpred_effect(mod = mod,
##                                     vals_effect = vals_effect)
##     expect_identical(nrow(ans), length(mod$outcome))
##     expect_identical(ncol(ans), n_sim)
## })


## ## 'make_vals_linpred_season' -------------------------------------------------

## test_that("'make_vals_linpred_season' works", {
##     set.seed(0)
##     data <- expand.grid(age = 0:9, time = 2000:2006, sex = c("F", "M"))
##     data$popn <- rpois(n = nrow(data), lambda = 100)
##     data$deaths <- rpois(n = nrow(data), lambda = 10)
##     formula <- deaths ~ age * time + sex
##     mod <- mod_pois(formula = formula,
##                     data = data,
##                     exposure = popn)
##     mod <- set_season(mod, n = 2)
##     n_sim <- 10L
##     vals_season <- draw_vals_season(mod, n_sim = n_sim)
##     ans <- make_vals_linpred_season(mod = mod,
##                                     vals_season = vals_season)
##     expect_identical(nrow(ans), length(mod$outcome))
##     expect_identical(ncol(ans), n_sim)
##     expect_true(all(apply(ans, 2, function(x) length(unique(x))) == 7L))
## })


## ## 'reformat_performance_vec' -------------------------------------------------

## test_that("'reformat_performance_vec' works with valid input", {
##     x <- list(list(a = c(0, 0.1),
##                    b = c(1, 1.1, 1.2)),
##               list(a = c(10, 10.1),
##                    b = c(11, 11.1, 11.2)))
##     ans_obtained <- reformat_performance_vec(x)
##     ans_expected <- rvec::rvec_dbl(cbind(c(0, 0.1, 1, 1.1, 1.2),
##                                          c(10, 10.1, 11, 11.1, 11.2)))
##     expect_identical(ans_obtained, ans_expected)
## })


## ## 'reformat_interval' --------------------------------------------------

## test_that("'reformat_interval' works with valid input", {
##     x <- list(list(a = list("0.5" = c(T,F), "0.95" = c(F,T)),
##                    b = list("0.5" = c(F,T,F), "0.95" = c(T,F,T))),
##               list(a = list("0.5" = c(T,F), "0.95" = c(F,T)),
##                    b = list("0.5" = c(F,T,F), "0.95" = c(F,T,F))))
##     ans_obtained <- reformat_interval(x, nm = "coverage")
##     ans_expected <- tibble::tibble(coverage.0.5 =
##                                        rvec::rvec_lgl(cbind(c(T,F,F,T,F),c(T,F,F,T,F))),
##                                    coverage.0.95 =
##                                        rvec::rvec_lgl(cbind(c(F,T,T,F,T), c(F,T,F,T,F))))
##     expect_identical(ans_obtained, ans_expected)
## })


## ## 'report_sim' ---------------------------------------------------------------

## test_that("'report_sim' works when mod_sim is identical to mod_est - short", {
##     set.seed(0)
##     data <- expand.grid(age = 0:9, sex = c("F", "M"))
##     data$popn <- rpois(n = nrow(data), lambda = 100)
##     data$deaths <- rpois(n = nrow(data), lambda = 10)
##     formula <- deaths ~ age + sex
##     mod <- mod_pois(formula = formula,
##                     data = data,
##                     exposure = popn) |>
##                     set_prior(`(Intercept)` ~ NFix(sd = 0.01))
##     ans_obtained <- report_sim(mod, n_sim = 2)
##     expect_true(is.data.frame(ans_obtained))
##     expect_identical(nrow(ans_obtained), 4L)
## })

## test_that("'report_sim' works when mod_sim is identical to mod_est - long", {
##     set.seed(0)
##     data <- expand.grid(age = 0:9, sex = c("F", "M"))
##     data$popn <- rpois(n = nrow(data), lambda = 100)
##     data$deaths <- rpois(n = nrow(data), lambda = 10)
##     formula <- deaths ~ age + sex
##     mod <- mod_pois(formula = formula,
##                     data = data,
##                     exposure = popn) |>
##                     set_prior(`(Intercept)` ~ NFix(sd = 0.01))
##     ans_obtained <- report_sim(mod,
##                                n_sim = 2,
##                                report_type = "long",
##                                point_est_fun = "med")
##     expect_true(is.data.frame(ans_obtained))
## })


## test_that("'report_sim' works when mod_sim is identical to mod_est - parallel processing", {
##     set.seed(0)
##     data <- expand.grid(age = 0:9, sex = c("F", "M"))
##     data$popn <- rpois(n = nrow(data), lambda = 100)
##     data$deaths <- rpois(n = nrow(data), lambda = 10)
##     formula <- deaths ~ age + sex
##     mod <- mod_pois(formula = formula,
##                     data = data,
##                     exposure = popn) |>
##                     set_prior(`(Intercept)` ~ NFix(sd = 0.01))
##     ans_obtained <- report_sim(mod,
##                                n_sim = 2,
##                                report_type = "long",
##                                point_est_fun = "med",
##                                n_core = 2)
##     expect_true(is.data.frame(ans_obtained))
## })




## test_that("'report_sim' works when mod_sim is identical to mod_est - parallel processing", {
##     set.seed(0)
##     data <- expand.grid(age = 0:9, sex = c("F", "M"))
##     data$popn <- rpois(n = nrow(data), lambda = 100)
##     data$deaths <- rpois(n = nrow(data), lambda = 10)
##     formula <- deaths ~ age + sex
##     mod <- mod_pois(formula = formula,
##                     data = data,
##                     exposure = popn) |>
##                     set_prior(`(Intercept)` ~ NFix(sd = 0.01))
##     mod_est <- fit(mod)
##     mod_sim <- fit(mod)
##     comp_est <- components(mod_est)
##     comp_sim <- components(mod_sim)
##     aug_est <- augment(mod_est)
##     aug_sim <- augment(mod_sim)
##     comp <- merge(comp_sim, comp_est, by =     
    
##     ans_obtained <- report_sim(mod,
##                                n_sim = 2,
##                                report_type = "long",
##                                point_est_fun = "med",
##                                n_core = 2)
##     expect_true(is.data.frame(ans_obtained))
## })



test_that("'stanardize_vals_effect' works", {
  set.seed(0)
  data <- expand.grid(age = 0:9, time = 2000:2005, sex = c("F", "M"))
  data$popn <- rpois(n = nrow(data), lambda = 100)
  data$deaths <- rpois(n = nrow(data), lambda = 10)
  formula <- deaths ~ age * time + sex
  mod <- mod_pois(formula = formula,
                  data = data,
                  exposure = popn)
  mod <- set_prior(mod, time ~ compose_time(RW(), seasonal = Seas(n = 2), error = N()))
  n_sim <- 2
  vals_hyper <- draw_vals_hyper_mod(mod, n_sim = n_sim)
  vals_hyperrand <- draw_vals_hyperrand_mod(mod,
                                            vals_hyper = vals_hyper,
                                            n_sim = n_sim)
  vals_effect <- draw_vals_effect_mod(mod,
                                      vals_hyper = vals_hyper,
                                      vals_hyperrand = vals_hyperrand,
                                      n_sim = n_sim)
  ans <- standardize_vals_effect(mod = mod, vals_effect = vals_effect)
  means <- lapply(ans[-1], function(x) colMeans(x))
  expect_equal(mean(unlist(means)), 0)
  m <- make_combined_matrix_effect_outcome(mod)
  x_raw <- do.call(rbind, vals_effect)
  x_standard <- do.call(rbind, ans)
  expect_equal(m %*% x_raw, m %*% x_standard)
})


## ## 'summarise_sim' ---------------------------------------------------------------

## test_that("'summarise_sim' works with valid inputs", {
##     set.seed(0)
##     data <- expand.grid(age = 0:9, sex = c("F", "M"))
##     data$popn <- rpois(n = nrow(data), lambda = 100)
##     data$deaths <- rpois(n = nrow(data), lambda = 10)
##     formula <- deaths ~ age + sex
##     mod <- mod_pois(formula = formula,
##                     data = data,
##                     exposure = popn) |>
##                     set_prior(`(Intercept)` ~ NFix(sd = 0.01))
##     data <- report_sim(mod, n_sim = 2, report_type = "long")
##     ans <- summarise_sim(data)
##     expect_identical(names(ans), setdiff(names(data), c("term", "level")))
##     expect_true(all(sapply(ans[-1], is.double)))
## })



## ## 'transpose_list' -----------------------------------------------------------

## test_that("'transpose_list' works with valid inputs - all lengths at least 1", {
##     l <- list(a = list(x = 1, y = 2, z = 3),
##               b = list(x = 10, y = 20, z = 30))
##     ans_obtained <- transpose_list(l)
##     ans_expected <- list(list(1, 10),
##                          list(2, 20),
##                          list(3, 30))
##     expect_identical(ans_obtained, ans_expected)
## })


## 'vals_disp_to_dataframe' ---------------------------------------------------

test_that("'draw_vals_disp' works with 'bage_mod_pois'", {
    set.seed(0)
    data <- expand.grid(age = 0:9, time = 2000:2005, sex = c("F", "M"))
    data$popn <- rpois(n = nrow(data), lambda = 100)
    data$deaths <- rpois(n = nrow(data), lambda = 10)
    formula <- deaths ~ age + sex + time
    mod <- mod_pois(formula = formula,
                    data = data,
                    exposure = popn)
    vals_disp <- draw_vals_disp(mod, n_sim = 10)
    ans_obtained <- vals_disp_to_dataframe(vals_disp)
    ans_expected <- tibble::tibble(term = "disp",
                                   component = "disp",
                                   level = "disp",
                                   .fitted = vals_disp)
    expect_equal(ans_obtained, ans_expected)
})


## 'vals_effect_to_dataframe' -------------------------------------------------

test_that("'vals_effect_to_dataframe' works", {
  set.seed(0)
  data <- expand.grid(age = 0:9, time = 2000:2005, sex = c("F", "M"))
  data$popn <- rpois(n = nrow(data), lambda = 100)
  data$deaths <- rpois(n = nrow(data), lambda = 10)
  formula <- deaths ~ age * time + sex
  mod <- mod_pois(formula = formula,
                  data = data,
                  exposure = popn)
  mod <- set_prior(mod, time ~ compose_time(RW(), seasonal = Seas(n = 2), error = N()))
  n_sim <- 2
  vals_hyper <- draw_vals_hyper_mod(mod, n_sim = n_sim)
  vals_hyperrand <- draw_vals_hyperrand_mod(mod,
                                            vals_hyper = vals_hyper,
                                            n_sim = n_sim)
  vals_effect <- draw_vals_effect_mod(mod,
                                      vals_hyper = vals_hyper,
                                      vals_hyperrand = vals_hyperrand,
                                      n_sim = n_sim)
  vals_effect <- standardize_vals_effect(mod = mod, vals_effect = vals_effect)
  ans_obtained <- vals_effect_to_dataframe(vals_effect)
  ans_expected <- tibble::tibble(term = rep(c("(Intercept)", "age", "time", "sex", "age:time"),
                                            times = c(1, 10, 6, 2, 60)),
                                 component = "effect",
                                 level = c("(Intercept)",
                                           0:9,
                                           2000:2005,
                                           c("F", "M"),
                                           paste(0:9, rep(2000:2005, each = 10), sep = ".")),
                                 .fitted = rvec::rvec(do.call(rbind, vals_effect)))
  expect_equal(ans_obtained, ans_expected)
})
