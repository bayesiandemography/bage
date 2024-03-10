
## 'aggregate_report_aug' -----------------------------------------------------

test_that("'aggregate_report_aug' works with valid inputs", {
  set.seed(0)
  data <- expand.grid(age = 0:9, time = 2000:2005, sex = c("F", "M"))
  data$popn <- rpois(n = nrow(data), lambda = 100)
  data$deaths <- rpois(n = nrow(data), lambda = 10)
  formula <- deaths ~ age + sex + time
  mod <- mod_pois(formula = formula,
                  data = data,
                  exposure = popn)
  mod_sim <- mod
  mod_est <- mod
  mod_sim <- set_n_draw(mod, n = 1)
  aug_sim <- augment(mod_sim)
  mod_est$outcome <- as.numeric(aug_sim$deaths)
  mod_est <- fit(mod_est)
  aug_est <- augment(mod_est)
  perform_aug <- list(perform_aug(est = aug_est,
                                  sim = aug_sim,
                                  i_sim = 1L,
                                  point_est_fun = "median",
                                  widths = c(0.6, 0.8)))
  report_aug <- make_report_aug(perform_aug = perform_aug,
                                  aug_sim = aug_sim)
  report_aug <- rvec_to_mean(report_aug)
  ans_obtained <- aggregate_report_aug(report_aug)
  expect_identical(names(ans_obtained),
                   setdiff(names(report_aug),
                           c("age", "sex", "time", "popn", "deaths")))
  expect_identical(unique(ans_obtained$.var), unique(report_aug$.var))
})


## 'aggregate_report_comp' ----------------------------------------------------

test_that("'aggregate_report_comp' works with valid inputs", {
  set.seed(0)
  data <- expand.grid(age = 0:9, time = 2000:2005, sex = c("F", "M"))
  data$popn <- rpois(n = nrow(data), lambda = 100)
  data$deaths <- rpois(n = nrow(data), lambda = 10)
  formula <- deaths ~ age + sex + time
  mod <- mod_pois(formula = formula,
                  data = data,
                  exposure = popn)
  mod_sim <- mod
  mod_est <- mod
  mod_sim <- set_n_draw(mod, n = 1)
  comp_sim <- components(mod_sim)
  aug_sim <- augment(mod_sim)
  mod_est$outcome <- as.numeric(aug_sim$deaths)
  mod_est <- fit(mod_est)
  comp_est <- components(mod_est)
  perform_comp <- list(perform_comp(est = comp_est,
                                    sim = comp_sim,
                                    i_sim = 1L,
                                    point_est_fun = "median",
                                    widths = c(0.5, 0.95)))
  report_comp <- make_report_comp(perform_comp = perform_comp,
                                  comp_sim = comp_sim)
  report_comp <- rvec_to_mean(report_comp)
  ans_obtained <- aggregate_report_comp(report_comp)
  expect_identical(names(ans_obtained), setdiff(names(report_comp), "level"))
  expect_identical(unique(ans_obtained$term), unique(report_comp$term))
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


## 'draw_vals_disp' -----------------------------------------------------------

test_that("'draw_vals_disp' works with 'bage_mod_norm'", {
    set.seed(0)
    data <- expand.grid(age = 0:9, time = 2000:2005, sex = c("F", "M"))
    data$popn <- rpois(n = nrow(data), lambda = 100)
    data$deaths <- rpois(n = nrow(data), lambda = 10)
    formula <- deaths ~ age + sex + time
    mod <- mod_norm(formula = formula,
                    data = data,
                    weights = 1)
    ans <- draw_vals_disp(mod, n_sim = 10000)
    expect_equal(rvec::draws_median(ans), qexp(0.5), tolerance = 0.01)
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


## error_point_est -------------------------------------------------------

test_that("'error_point_est' works", {
  var_est <- rvec::rvec_dbl(matrix(rnorm(300), nr = 3))
  var_sim <- c(0, -1, 1)
  point_est_fun <- "mean"
  ans_obtained <- error_point_est(var_est = var_est,
                                  var_sim = var_sim,
                                  point_est_fun = point_est_fun)
  ans_expected <- rvec::draws_mean(var_est) - var_sim
  expect_equal(ans_obtained, ans_expected)
})


## 'get_error_point_est' ------------------------------------------------------

test_that("'get_error_point_est' works with valid inputs", {
  set.seed(0)
  data <- expand.grid(age = 0:9, time = 2000:2005, sex = c("F", "M"))
  data$popn <- rpois(n = nrow(data), lambda = 100)
  data$deaths <- rpois(n = nrow(data), lambda = 10)
  formula <- deaths ~ age + sex + time
  mod <- mod_pois(formula = formula,
                  data = data,
                  exposure = popn)
  mod_sim <- mod
  mod_est <- mod
  mod_sim <- set_n_draw(mod, n = 1)
  aug_sim <- augment(mod_sim)
  mod_est$outcome <- as.numeric(aug_sim$deaths)
  mod_est <- fit(mod_est)
  aug_est <- augment(mod_est)
  perform <- list(perform_aug(est = aug_est,
                              sim = aug_sim,
                              i_sim = 1L,
                              point_est_fun = "median",
                              widths = c(0.6, 0.8)))
  ans_obtained <- get_error_point_est(perform)
  expect_identical(sapply(ans_obtained, names),
                   c(.fitted = ".error", .expected = ".error"))
  expect_identical(sapply(ans_obtained, nrow),
                   c(.fitted = nrow(aug_est), .expected = nrow(aug_est)))
})


## 'get_is_in_interval' -------------------------------------------------------

test_that("'get_is_in_interval' works with valid inputs", {
  set.seed(0)
  data <- expand.grid(age = 0:9, time = 2000:2005, sex = c("F", "M"))
  data$popn <- rpois(n = nrow(data), lambda = 100)
  data$deaths <- rpois(n = nrow(data), lambda = 10)
  formula <- deaths ~ age + sex + time
  mod <- mod_pois(formula = formula,
                  data = data,
                  exposure = popn)
  mod_sim <- mod
  mod_est <- mod
  mod_sim <- set_n_draw(mod, n = 1)
  aug_sim <- augment(mod_sim)
  mod_est$outcome <- as.numeric(aug_sim$deaths)
  mod_est <- fit(mod_est)
  aug_est <- augment(mod_est)
  perform <- list(perform_aug(est = aug_est,
                              sim = aug_sim,
                              i_sim = 1L,
                              point_est_fun = "median",
                              widths = c(0.5, 0.95)))
  ans_obtained <- get_is_in_interval(perform)
  expect_identical(lapply(ans_obtained, names),
                   list(.fitted = c(".cover_50", ".cover_95"),
                        .expected = c(".cover_50", ".cover_95")))
  expect_identical(sapply(ans_obtained, nrow),
                   c(.fitted = nrow(aug_est), .expected = nrow(aug_est)))
})


## 'get_length_interval' -------------------------------------------------------

test_that("'get_error_point_est' works with valid inputs", {
  set.seed(0)
  data <- expand.grid(age = 0:9, time = 2000:2005, sex = c("F", "M"))
  data$popn <- rpois(n = nrow(data), lambda = 100)
  data$deaths <- rpois(n = nrow(data), lambda = 10)
  formula <- deaths ~ age + sex + time
  mod <- mod_pois(formula = formula,
                  data = data,
                  exposure = popn)
  mod_sim <- mod
  mod_est <- mod
  mod_sim <- set_n_draw(mod, n = 1)
  aug_sim <- augment(mod_sim)
  mod_est$outcome <- as.numeric(aug_sim$deaths)
  mod_est <- fit(mod_est)
  aug_est <- augment(mod_est)
  perform <- list(perform_aug(est = aug_est,
                                  sim = aug_sim,
                                  i_sim = 1L,
                                  point_est_fun = "median",
                                  widths = c(0.5, 0.95)))
  ans_obtained <- get_length_interval(perform)
  expect_identical(lapply(ans_obtained, names),
                   list(.fitted = c(".length_50", ".length_95"),
                        .expected = c(".length_50", ".length_95")))
  expect_identical(sapply(ans_obtained, nrow),
                   c(.fitted = nrow(aug_est), .expected = nrow(aug_est)))
})


## is_in_interval --------------------------------------------------------

test_that("'is_in_interval' works - include_upper is TRUE", {
  var_est <- rvec::rvec_dbl(matrix(rnorm(300), nr = 3))
  var_sim <- c(-1, 0, 1)
  widths <- c(0.5, 0.9, 1)
  ans_obtained <- is_in_interval(var_est = var_est,
                                 var_sim = var_sim,
                                 widths = widths)
  q <- rvec::draws_quantile(var_est, probs = c(0, 0.05, 0.25, 0.75, 0.95, 1))
  ans_expected <- list("50" = (var_sim >= q[[3]] & var_sim <= q[[4]]),
                       "90" = (var_sim >= q[[2]] & var_sim <= q[[5]]),
                       "100" = (var_sim >= q[[1]] & var_sim <= q[[6]]))
  expect_equal(ans_obtained, ans_expected)
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


## length_interval ------------------------------------------------------------

test_that("'length_interval' works", {
  var_est <- rvec::rvec_dbl(matrix(1:300, nr = 3))
  widths <- c(0.5, 0.9, 1)
  ans_obtained <- length_interval(var_est = var_est,
                                  widths = widths)
  q <- rvec::draws_quantile(var_est, probs = c(0, 0.05, 0.25, 0.75, 0.95, 1))
  ans_expected <- list("50" = q[[4]] - q[[3]],
                       "90" = q[[5]] - q[[2]],
                       "100" = q[[6]] - q[[1]])
  expect_equal(ans_obtained, ans_expected)
})


## 'make_report_aug' ----------------------------------------------------------

test_that("'make_report_aug' works with valid inputs", {
  set.seed(0)
  data <- expand.grid(age = 0:9, time = 2000:2005, sex = c("F", "M"))
  data$popn <- rpois(n = nrow(data), lambda = 100)
  data$deaths <- rpois(n = nrow(data), lambda = 10)
  formula <- deaths ~ age + sex + time
  mod <- mod_pois(formula = formula,
                  data = data,
                  exposure = popn)
  mod_sim <- mod
  mod_est <- mod
  mod_sim <- set_n_draw(mod, n = 1)
  aug_sim <- augment(mod_sim)
  mod_est$outcome <- as.numeric(aug_sim$deaths)
  mod_est <- fit(mod_est)
  aug_est <- augment(mod_est)
  perform_aug <- list(perform_aug(est = aug_est,
                                  sim = aug_sim,
                                  i_sim = 1L,
                                  point_est_fun = "median",
                                  widths = c(0.6, 0.8)))
  ans_obtained <- make_report_aug(perform_aug = perform_aug,
                                  aug_sim = aug_sim)
  expect_setequal(names(ans_obtained),
                  c(".var", "age", "time", "sex",
                    "deaths", "popn", ".observed", ".error",
                    ".cover_60", ".cover_80",
                    ".length_60", ".length_80"))
  expect_identical(nrow(ans_obtained), 2L * nrow(aug_sim))
})


## 'make_report_comp' ---------------------------------------------------------

test_that("'make_report_comp' works with valid inputs", {
  set.seed(0)
  data <- expand.grid(age = 0:9, time = 2000:2005, sex = c("F", "M"))
  data$popn <- rpois(n = nrow(data), lambda = 100)
  data$deaths <- rpois(n = nrow(data), lambda = 10)
  formula <- deaths ~ age + sex + time
  mod <- mod_pois(formula = formula,
                  data = data,
                  exposure = popn)
  mod_sim <- mod
  mod_est <- mod
  mod_sim <- set_n_draw(mod, n = 1)
  comp_sim <- components(mod_sim)
  aug_sim <- augment(mod_sim)
  mod_est$outcome <- as.numeric(aug_sim$deaths)
  mod_est <- fit(mod_est)
  comp_est <- components(mod_est)
  perform_comp <- list(perform_comp(est = comp_est,
                                    sim = comp_sim,
                                    i_sim = 1L,
                                    point_est_fun = "median",
                                    widths = c(0.5, 0.95)))
  ans_obtained <- make_report_comp(perform_comp = perform_comp,
                                   comp_sim = comp_sim)
  expect_setequal(names(ans_obtained),
                  c("term", "component", "level", ".error",
                    ".cover_50", ".cover_95",
                    ".length_50", ".length_95"))
})


## 'perform_aug' ------------------------------------------------------

test_that("'perform_aug' works with valid inputs - models same", {
  set.seed(0)
  data <- expand.grid(age = 0:9, time = 2000:2005, sex = c("F", "M"))
  data$popn <- rpois(n = nrow(data), lambda = 100)
  data$deaths <- rpois(n = nrow(data), lambda = 10)
  formula <- deaths ~ age + sex + time
  mod <- mod_pois(formula = formula,
                  data = data,
                  exposure = popn)
  mod_sim <- mod
  mod_est <- mod
  mod_sim <- set_n_draw(mod, n = 1)
  aug_sim <- augment(mod_sim)
  mod_est$outcome <- as.numeric(aug_sim$deaths)
  mod_est <- fit(mod_est)
  aug_est <- augment(mod_est)
  ans_obtained <- perform_aug(est = aug_est,
                                      sim = aug_sim,
                                      i_sim = 1L,
                                      point_est_fun = "median",
                                      widths = c(0.6, 0.8))
  names(aug_est)[6:8] <- paste(names(aug_est)[6:8], "est", sep = "_")
  names(aug_sim)[6:8] <- paste(names(aug_sim)[6:8], "sim", sep = "_")
  ans_expected <- list(.fitted =
                         list(error_point_est =
                                error_point_est(var_est = aug_est[[".fitted_est"]],
                                                var_sim = as.numeric(aug_sim[[".fitted_sim"]]),
                                                point_est_fun = "median"),
                              is_in_interval =
                                is_in_interval(var_est = aug_est[[".fitted_est"]],
                                               var_sim = as.numeric(aug_sim[[".fitted_sim"]]),
                                               widths = c(0.6, 0.8)),
                              length_interval =
                                length_interval(var_est = aug_est[[".fitted_est"]],
                                               widths = c(0.6, 0.8))),
                       .expected =
                         list(error_point_est =
                                error_point_est(var_est = aug_est[[".expected_est"]],
                                                var_sim = as.numeric(aug_sim[[".expected_sim"]]),
                                                point_est_fun = "median"),
                              is_in_interval =
                                is_in_interval(var_est = aug_est[[".expected_est"]],
                                               var_sim = as.numeric(aug_sim[[".expected_sim"]]),
                                               widths = c(0.6, 0.8)),
                              length_interval =
                                length_interval(var_est = aug_est[[".expected_est"]],
                                               widths = c(0.6, 0.8))))
  expect_equal(ans_obtained, ans_expected)
})

test_that("'perform_aug' works with valid inputs - models different", {
  set.seed(0)
  data <- expand.grid(age = 0:9, time = 2000:2005, sex = c("F", "M"))
  data$popn <- rpois(n = nrow(data), lambda = 100)
  data$deaths <- rpois(n = nrow(data), lambda = 10)
  mod_sim <- mod_pois(deaths ~ age + sex + time,
                      data = data,
                      exposure = popn)
  mod_est <- mod_pois(deaths ~ age * sex + time,
                      data = data,
                      exposure = popn)
  mod_sim <- set_n_draw(mod_sim, n = 1)
  aug_sim <- augment(mod_sim)
  mod_est$outcome <- as.numeric(aug_sim$deaths)
  mod_est <- fit(mod_est)
  aug_est <- augment(mod_est)
  ans_obtained <- perform_aug(est = aug_est,
                              sim = aug_sim,
                              i_sim = 1L,
                              point_est_fun = "median",
                              widths = c(0.6, 0.8))
  names(aug_est)[6:8] <- paste(names(aug_est)[6:8], "est", sep = "_")
  names(aug_sim)[6:8] <- paste(names(aug_sim)[6:8], "sim", sep = "_")
  ans_expected <- list(.fitted =
                         list(error_point_est =
                                error_point_est(var_est = aug_est[[".fitted_est"]],
                                                var_sim = as.numeric(aug_sim[[".fitted_sim"]]),
                                                point_est_fun = "median"),
                              is_in_interval =
                                is_in_interval(var_est = aug_est[[".fitted_est"]],
                                               var_sim = as.numeric(aug_sim[[".fitted_sim"]]),
                                               widths = c(0.6, 0.8)),
                              length_interval =
                                length_interval(var_est = aug_est[[".fitted_est"]],
                                               widths = c(0.6, 0.8))),
                       .expected =
                         list(error_point_est =
                                error_point_est(var_est = aug_est[[".expected_est"]],
                                                var_sim = as.numeric(aug_sim[[".expected_sim"]]),
                                                point_est_fun = "median"),
                              is_in_interval =
                                is_in_interval(var_est = aug_est[[".expected_est"]],
                                               var_sim = as.numeric(aug_sim[[".expected_sim"]]),
                                               widths = c(0.6, 0.8)),
                              length_interval =
                                length_interval(var_est = aug_est[[".expected_est"]],
                                               widths = c(0.6, 0.8))))
  expect_equal(ans_obtained, ans_expected)
})


## 'perform_comp' ---------------------------------------------------------

test_that("'perform_comp' works with valid inputs - models same", {
  set.seed(0)
  data <- expand.grid(age = 0:9, time = 2000:2005, sex = c("F", "M"))
  data$popn <- rpois(n = nrow(data), lambda = 100)
  data$deaths <- rpois(n = nrow(data), lambda = 10)
  formula <- deaths ~ age + sex + time
  mod <- mod_pois(formula = formula,
                  data = data,
                  exposure = popn)
  mod_sim <- mod
  mod_est <- mod
  mod_sim <- set_n_draw(mod, n = 1)
  comp_sim <- components(mod_sim)
  aug_sim <- augment(mod_sim)
  mod_est$outcome <- as.numeric(aug_sim$deaths)
  mod_est <- fit(mod_est)
  comp_est <- components(mod_est)
  ans_obtained <- perform_comp(est = comp_est,
                                   sim = comp_sim,
                                   i_sim = 1L,
                                   point_est_fun = "median",
                                   widths = c(0.6, 0.8))
  names(comp_est)[[4]] <- ".fitted_est"
  names(comp_sim)[[4]] <- ".fitted_sim"
  merged <- merge(comp_est, comp_sim, sort = FALSE) 
  ans_expected <- list(.fitted =
                         list(error_point_est =
                                error_point_est(var_est = merged[[".fitted_est"]],
                                                var_sim = as.numeric(merged[[".fitted_sim"]]),
                                                point_est_fun = "median"),
                              is_in_interval =
                                is_in_interval(var_est = merged[[".fitted_est"]],
                                               var_sim = as.numeric(merged[[".fitted_sim"]]),
                                               widths = c(0.6, 0.8)),
                              length_interval =
                                length_interval(var_est = merged[[".fitted_est"]],
                                               widths = c(0.6, 0.8))))
  expect_equal(ans_obtained, ans_expected)
})

test_that("'perform_comp' works with valid inputs - models different", {
  set.seed(0)
  data <- expand.grid(age = 0:9, time = 2000:2005, sex = c("F", "M"))
  data$popn <- rpois(n = nrow(data), lambda = 100)
  data$deaths <- rpois(n = nrow(data), lambda = 10)
  formula_est <- deaths ~ age + sex + time
  mod_est <- mod_pois(formula = formula_est,
                      data = data,
                      exposure = popn)
  formula_sim <- deaths ~ age * sex + time
  mod_sim <- mod_pois(formula = formula_sim,
                      data = data,
                      exposure = popn)
  mod_sim <- set_n_draw(mod_sim, n = 1)
  comp_sim <- components(mod_sim)
  aug_sim <- augment(mod_sim)
  mod_est$outcome <- as.numeric(aug_sim$deaths)
  mod_est <- fit(mod_est)
  comp_est <- components(mod_est)
  ans_obtained <- perform_comp(est = comp_est,
                                   sim = comp_sim,
                                   i_sim = 1L,
                                   point_est_fun = "median",
                                   widths = c(0.6, 0.8))
  names(comp_est)[[4]] <- ".fitted_est"
  names(comp_sim)[[4]] <- ".fitted_sim"
  merged <- merge(comp_est, comp_sim, sort = FALSE) 
  ans_expected <- list(.fitted =
                         list(error_point_est =
                                error_point_est(var_est = merged[[".fitted_est"]],
                                                var_sim = as.numeric(merged[[".fitted_sim"]]),
                                                point_est_fun = "median"),
                              is_in_interval =
                                is_in_interval(var_est = merged[[".fitted_est"]],
                                               var_sim = as.numeric(merged[[".fitted_sim"]]),
                                               widths = c(0.6, 0.8)),
                              length_interval =
                                length_interval(var_est = merged[[".fitted_est"]],
                                               widths = c(0.6, 0.8))))
  expect_equal(ans_obtained, ans_expected)
})


## 'report_sim' ---------------------------------------------------------------

test_that("'report_sim' works when mod_sim is identical to mod_est - short", {
    set.seed(0)
    data <- expand.grid(age = 0:20, sex = c("F", "M"))
    data$popn <- rpois(n = nrow(data), lambda = 100)
    data$deaths <- rpois(n = nrow(data), lambda = 10)
    formula <- deaths ~ age + sex
    mod <- mod_pois(formula = formula,
                    data = data,
                    exposure = popn) |>
                    set_prior(age ~ Sp())
    set.seed(0)
    ans_obtained <- report_sim(mod, n_sim = 2)
    expect_setequal(names(ans_obtained), c("components", "augment"))
})

## test_that("'report_sim' works when mod_sim is identical to mod_est - parallel processing", {
##     set.seed(0)
##     data <- expand.grid(age = 0:9, sex = c("F", "M"))
##     data$popn <- rpois(n = nrow(data), lambda = 100)
##     data$deaths <- rpois(n = nrow(data), lambda = 10)
##     formula <- deaths ~ age + sex
##     mod <- mod_pois(formula = formula,
##                     data = data,
##                     exposure = popn)
##     ans_obtained <- report_sim(mod,
##                                n_sim = 4,
##                                report_type = "long",
##                                point_est_fun = "med",
##                                n_core = 2)
##     expect_identical(names(ans_obtained), c("components", "augment"))
## })


## 'standardize_vals_effect' --------------------------------------------------

test_that("'standardize_vals_effect' works", {
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
                                 .fitted = rvec::rvec(unname(do.call(rbind, vals_effect))))
  expect_equal(ans_obtained, ans_expected)
})
