
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
  aug_sim <- augment(mod_sim, quiet = TRUE)
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
  prior_class_est <- make_prior_class(mod_est)
  prior_class_sim <- make_prior_class(mod_sim)
  mod_sim <- set_n_draw(mod, n = 1)
  comp_sim <- components(mod_sim, quiet = TRUE)
  aug_sim <- augment(mod_sim, quiet = TRUE)
  mod_est$outcome <- as.numeric(aug_sim$deaths)
  mod_est <- fit(mod_est)
  comp_est <- components(mod_est)
  perform_comp <- list(perform_comp(est = comp_est,
                                    sim = comp_sim,
                                    prior_class_est = prior_class_est,
                                    prior_class_sim = prior_class_sim,
                                    i_sim = 1L,
                                    point_est_fun = "median",
                                    widths = c(0.5, 0.95)))
  report_comp <- make_report_comp(perform_comp = perform_comp,
                                  comp_est = comp_est,
                                  comp_sim = comp_sim,
                                  prior_class_est = prior_class_est,
                                  prior_class_sim = prior_class_sim)
  report_comp <- rvec_to_mean(report_comp)
  ans_obtained <- aggregate_report_comp(report_comp)
  expect_identical(names(ans_obtained), setdiff(names(report_comp), "level"))
  expect_identical(unique(ans_obtained$term), unique(report_comp$term))
})


## 'draw_fitted' --------------------------------------------------------------

test_that("'draw_fitted' works with 'pois'", {
  set.seed(0)
  expected <- exp(rvec::rnorm_rvec(n = 20, n_draw = 50))
  disp <- rvec::runif_rvec(n = 1, n_draw = 50)
  set.seed(1)
  ans_obtained <- draw_fitted(nm_distn = "pois",
                              expected = expected,
                              disp = disp)
  set.seed(1)
  ans_expected <- rvec::rgamma_rvec(n = 20,
                                    shape = 1 / disp,
                                    rate = 1 / (disp * expected))
  expect_equal(ans_obtained, ans_expected)
})

test_that("'draw_fitted' works with 'binom'", {
  set.seed(0)
  expected <- rvec::rbeta_rvec(n = 20, shape1 = 3, shape2 = 4, n_draw = 50)
  disp <- rvec::runif_rvec(n = 1, n_draw = 50)
  set.seed(1)
  ans_obtained <- draw_fitted(nm_distn = "binom",
                              expected = expected,
                              disp = disp)
  set.seed(1)
  ans_expected <- rvec::rbeta_rvec(n = 20,
                                   shape1 = expected / disp,
                                   shape2 = (1 - expected) / disp)
  expect_equal(ans_obtained, ans_expected)
})

test_that("'draw_fitted' throws expected error with invalid 'nm_distn'", {
  set.seed(0)
  expected <- rvec::rbeta_rvec(n = 20, shape1 = 3, shape2 = 4, n_draw = 50)
  disp <- rvec::runif_rvec(n = 1, n_draw = 50)
  expect_error(draw_fitted(nm_distn = "wrong",
                           expected = expected,
                           disp = disp),
               "Internal error")
})


## 'draw_outcome_true' ---------------------------------------------------

test_that("'draw_outcome_true' works with pois - offset numeric, no NA", {
  set.seed(0)
  offset <- runif(n = 20, max = 100)
  fitted <- rvec::rpois_rvec(n = 20, lambda = 10, n_draw = 50)
  set.seed(1)
  ans_obtained <- draw_outcome_true(nm_distn = "pois",
                                    fitted = fitted,
                                    offset = offset,
                                    disp = NULL)
  set.seed(1)
  ans_expected <- rvec::rpois_rvec(n = 20, lambda = offset * fitted)
  expect_equal(ans_obtained, ans_expected)
})

test_that("'draw_outcome_true' works with pois - offset rvec, has NA", {
  set.seed(0)
  offset <- rvec::runif_rvec(n = 20, max = 100, n_draw = 10)
  offset[1] <- NA
  fitted <- rvec::rpois_rvec(n = 20, lambda = 10, n_draw = 10)
  set.seed(1)
  ans_obtained <- draw_outcome_true(nm_distn = "pois",
                                    fitted = fitted,
                                    offset = offset,
                                    disp = NULL)
  set.seed(1)
  ans_expected <- vctrs::vec_c(NA,
                               rvec::rpois_rvec(n = 19,
                                                lambda = offset[-1] * fitted[-1]))
  expect_equal(ans_obtained, ans_expected)
})

test_that("'draw_outcome_true' works with pois - no offset, fitted has NA", {
  set.seed(0)
  offset <- rep(1, 20)
  fitted <- rvec::rpois_rvec(n = 20, lambda = 10, n_draw = 10)
  fitted[1] <- NA
  set.seed(1)
  ans_obtained <- draw_outcome_true(nm_distn = "pois",
                                    fitted = fitted,
                                    offset = offset,
                                    disp = NULL)
  set.seed(1)
  ans_expected <- vctrs::vec_c(NA,
                               rvec::rpois_rvec(n = 19,
                                                lambda = fitted[-1]))
  expect_equal(ans_obtained, ans_expected)
})

test_that("'draw_outcome_true' works with binom", {
  set.seed(0)
  offset <- rpois(n = 20, lambda = 100)
  fitted <- rvec::rbeta_rvec(n = 20, shape1 = 1, shape2 = 2, n_draw = 50)
  set.seed(1)
  ans_obtained <- draw_outcome_true(nm_distn = "binom",
                                    fitted = fitted,
                                    offset = offset,
                                    disp = NULL)
  set.seed(1)
  ans_expected <- rvec::rbinom_rvec(n = 20,
                                    size = offset,
                                    prob = fitted)
  expect_equal(ans_obtained, ans_expected)
})

test_that("'draw_outcome_true' works with norm - has weights, no NA", {
  set.seed(0)
  offset <- runif(n = 20)
  fitted <- rvec::rnorm_rvec(n = 20, mean = 3, sd = 2, n_draw = 50)
  disp <- rvec::runif_rvec(n = 1, n_draw = 50)
  set.seed(1)
  ans_obtained <- draw_outcome_true(nm_distn = "norm",
                                    fitted = fitted,
                                    offset = offset,
                                    disp = disp)
  set.seed(1)
  ans_expected <- rvec::rnorm_rvec(n = 20,
                                   mean = fitted,
                                   sd = disp / sqrt(offset))
  expect_equal(ans_obtained, ans_expected)
})

test_that("'draw_outcome_true' works with norm - no weights, fitted has NA", {
  set.seed(0)
  offset <- rep(1, 20)
  fitted <- rvec::rnorm_rvec(n = 20, mean = 3, sd = 2, n_draw = 50)
  fitted[20] <- NA
  disp <- rvec::runif_rvec(n = 1, n_draw = 50)
  set.seed(1)
  ans_obtained <- draw_outcome_true(nm_distn = "norm",
                                    fitted = fitted,
                                    offset = offset,
                                    disp = disp)
  set.seed(1)
  ans_expected <- vctrs::vec_c(
    rvec::rnorm_rvec(n = 19,
                     mean = fitted[-20],
                     sd = disp / sqrt(offset[-20])),
    NA)
  expect_equal(ans_obtained, ans_expected)
})

test_that("'draw_outcome_true' throws expected error with invalid 'nm_distn'", {
  set.seed(0)
  offset <- runif(n = 20)
  fitted <- rvec::rnorm_rvec(n = 20, mean = 3, sd = 2, n_draw = 50)
  disp <- rvec::runif_rvec(n = 1, n_draw = 50)
  expect_error(draw_outcome_true(nm_distn = "wrong",
                                 fitted = fitted,
                                 offset = offset,
                                 disp = disp),
               "Internal error")
})


## 'draw_vals_ar' -------------------------------------------------------------

test_that("'draw_vals_ar' works with bage_prior_ar - n_by = 1", {
  prior <- AR(n_coef = 3)
  n_sim <- 10
  vals_hyper <- draw_vals_hyper(prior = prior,
                                n_sim = n_sim)
  vals_hyperrand <- list()
  levels_effect <- letters
  matrix_along_by <- matrix(0:25, nr = 26, dimnames = list(letters, NULL))
  ans <- draw_vals_ar(coef = vals_hyper$coef,
                      sd = vals_hyper$sd,
                      matrix_along_by = matrix_along_by,
                      levels_effect = levels_effect)
  expect_identical(dimnames(ans), list(letters, NULL))
})

test_that("'draw_vals_ar' works with bage_prior_ar - n_by = 2", {
  prior <- AR(n_coef = 3)
  matrix_along_by <- matrix(0:25, nr = 13, dimnames = list(letters[1:13], NULL))
  n_sim <- 10
  vals_hyper <- draw_vals_hyper(prior = prior,
                                n_sim = n_sim)
  ans <- draw_vals_ar(coef = vals_hyper$coef,
                      sd = vals_hyper$sd,
                      matrix_along_by = matrix_along_by,
                      levels_effect = letters)
  expect_identical(dimnames(ans), list(letters, NULL))
})


## 'draw_vals_ar_inner' -------------------------------------------------------

test_that("'draw_vals_ar_inner' works", {
    set.seed(0)
    prior <- AR(n = 2)
    coef <- draw_vals_coef(prior, n_sim = 5L)
    ans <- draw_vals_ar_inner(n = 10000, coef = coef, sd = seq(0.6, 1, 0.1))
    expect_identical(nrow(ans), 10000L)
    expect_identical(ncol(ans), 5L)
    expect_equal(apply(ans, 2, sd), seq(0.6, 1, 0.1), tolerance = 0.02)
})


## 'draw_vals_ar_one' ---------------------------------------------------------

test_that("'draw_vals_ar_one' works", {
    set.seed(0)
    prior <- AR(n = 2)
    coef <- draw_vals_coef(prior, n_sim = 1L)
    ans <- draw_vals_ar_one(n = 5000L, coef = coef, sd = 0.5)
    expect_identical(length(ans), 5000L)
    expect_equal(sd(ans), 0.5, tolerance = 0.02)
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


## 'draw_vals_components_unfitted' --------------------------------------------

test_that("'draw_vals_components_unfitted' works", {
  set.seed(0)
  data <- expand.grid(age = 0:9, time = 2000:2005, sex = c("F", "M"))
  data$popn <- rpois(n = nrow(data), lambda = 100)
  data$deaths <- rpois(n = nrow(data), lambda = 10)
  formula <- deaths ~ age * time + sex
  mod <- mod_pois(formula = formula,
                  data = data,
                  exposure = popn)
  mod <- set_prior(mod, age ~ Sp())
  mod <- set_prior(mod, age:time ~ Sp())
  n_sim <- 2
  ans <- draw_vals_components_unfitted(mod = mod, n_sim = n_sim)
  ans_est <- components(fit(mod))
  comb <- merge(ans, ans_est, by = c("component", "term", "level"), all.x = TRUE,
                all.y = TRUE)
  expect_true(identical(nrow(comb), nrow(ans)))
})

test_that("'draw_vals_components_unfitted' works - with covariates, datamod", {
  set.seed(0)
  data <- expand.grid(age = 0:9, time = 2000:2005, sex = c("F", "M"))
  data$popn <- rpois(n = nrow(data), lambda = 100)
  data$deaths <- rpois(n = nrow(data), lambda = 10)
  data$income <- rnorm(n = nrow(data))
  formula <- deaths ~ age * time + sex
  mod <- mod_pois(formula = formula,
                  data = data,
                  exposure = popn)
  mod <- set_prior(mod, age ~ Sp())
  mod <- set_prior(mod, age:time ~ Sp())
  mod <- set_covariates(mod, ~ income)
  mod <- set_datamod_undercount(mod, prob = data.frame(mean = 0.9, disp = 0.1))
  n_sim <- 2
  ans <- draw_vals_components_unfitted(mod = mod, n_sim = n_sim)
  ans_est <- components(fit(mod))
  comb <- merge(ans, ans_est, by = c("component", "term", "level"), all.x = TRUE,
                all.y = TRUE)
  expect_true(identical(nrow(comb), nrow(ans)))
})


## 'draw_vals_covariates' -----------------------------------------------------

test_that("'draw_vals_covariates' works", {
  set.seed(0)
  data <- expand.grid(age = 0:3,
                      time = 2000:2002,
                      reg = letters[1:3],
                      sex = c("F", "M"))
  data$popn <- rpois(n = nrow(data), lambda = 100)
  data$deaths <- rpois(n = nrow(data), lambda = 10)
  formula <- deaths ~ age * sex + time
  mod <- mod_pois(formula = formula,
                  data = data,
                  exposure = popn)
  mod <- set_covariates(mod, ~ reg)
  set.seed(0)
  ans_obtained <- draw_vals_covariates(mod, n_sim = 5)
  set.seed(0)
  coef <- rnorm(n = 10, sd = 1)
  ans_expected <- list(coef = matrix(coef, nr = 2, dimnames = list(c("regb", "regc"), NULL)))
  expect_identical(ans_obtained, ans_expected)
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


## draw_vals_effect_mod ----------------------------------------------------------

test_that("'draw_vals_effect_mod' works with bage_mod_pois", {
  set.seed(0)
  data <- expand.grid(age = c(0:59, "60+"), time = 2000:2002, sex = c("F", "M"))
  data$popn <- rpois(n = nrow(data), lambda = 100)
  data$deaths <- rpois(n = nrow(data), lambda = 10)
  formula <- deaths ~ age * time + age:sex
  mod <- mod_pois(formula = formula,
                  data = data,
                  exposure = popn)
  mod <- set_prior(mod, age:sex ~ SVD(HMD))
  n_sim <- 2
  vals_hyper <- draw_vals_hyper_mod(mod, n_sim = n_sim)
  vals_hyperrand <- draw_vals_hyperrand_mod(mod,
                                            vals_hyper = vals_hyper,
                                            n_sim = n_sim)
  vals_spline <- draw_vals_spline_mod(mod = mod,
                                      vals_hyper = vals_hyper,
                                      n_sim = n_sim)
  vals_svd <- draw_vals_svd_mod(mod = mod,
                                vals_hyper = vals_hyper,
                                n_sim = n_sim)
  ans <- draw_vals_effect_mod(mod,
                              vals_hyper = vals_hyper,
                              vals_hyperrand = vals_hyperrand,
                              vals_spline = vals_spline,
                              vals_svd = vals_svd,
                              n_sim = n_sim)
  expect_setequal(names(ans), c("(Intercept)", "age", "time", "age:time", "age:sex"))
  expect_true(all(sapply(ans, ncol) == n_sim))
  expect_identical(sapply(ans, nrow),
                   sapply(make_matrices_effect_outcome(mod$data, mod$dimnames_terms), ncol))
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
    mod <- set_prior(mod, age:time ~ Lin(con = "by"))
    vals_hyper <- draw_vals_hyper_mod(mod = mod, n_sim = 10)
    ans <- draw_vals_hyperrand_mod(mod, vals_hyper = vals_hyper, n_sim = 10)
    expect_identical(names(ans), c("(Intercept)", "age", "time", "sex", "age:time"))
    expect_identical(nrow(ans[["age:time"]]$slope), 10L)
    expect_identical(sapply(ans, length),
                     c("(Intercept)" = 0L, age = 0L, time = 0L, sex = 0L, "age:time" = 3L))
})


## 'draw_vals_lin' -----------------------------------------------------------

test_that("'draw_vals_lin' works - along dimension is first", {
  set.seed(0)
  prior <- Lin()
  n_sim <- 10
  matrix_along_by <- matrix(0:11, nr = 3)
  colnames(matrix_along_by) <- 11:14
  slope <- draw_vals_slope(mean_slope = prior$const[["mean_slope"]],
                           sd_slope = prior$const[["sd_slope"]],
                           matrix_along_by = matrix_along_by,
                           n_sim = n_sim)
  sd <- draw_vals_sd(prior = prior,
                     n_sim = n_sim)
  labels <- 1:12
  set.seed(0)
  ans_obtained <- draw_vals_lin(slope,
                                sd = sd,
                                matrix_along_by = matrix_along_by,
                                labels = labels)
  set.seed(0)
  slope1 <- rep(slope, each = 3)
  intercept1 <- -((3 + 1)/2) * slope1
  sd1 <- rep(sd, each = 12)
  ans_expected <- rnorm(12 * n_sim, mean = intercept1 + (1:3) * slope1, sd = sd1)
  ans_expected <- matrix(ans_expected, ncol = n_sim)
  dimnames(ans_expected) <- list(1:12, NULL)
  expect_equal(ans_obtained, ans_expected)  
})

test_that("'draw_vals_lin' works - along dimension is second", {
  set.seed(0)
  prior <- Lin()
  n_sim <- 10
  matrix_along_by <- t(matrix(0:11, nr = 3))
  colnames(matrix_along_by) <- 1:3
  sd <- draw_vals_sd(prior = prior,
                     n_sim = n_sim)
  slope <- draw_vals_slope(mean_slope = prior$const[["mean_slope"]],
                           sd_slope = prior$const[["sd_slope"]],
                           matrix_along_by = matrix_along_by,
                           n_sim = n_sim)
  labels <- 1:12
  set.seed(0)
  ans_obtained <- draw_vals_lin(slope = slope,
                                sd = sd,
                                matrix_along_by = matrix_along_by,
                                labels = labels)
  set.seed(0)
  slope1 <- rep(slope, each = 4)
  intercept1 <- -0.5 * (4 + 1) * slope1
  sd1 <- rep(sd, each = 12)
  ans_expected <- rnorm(12 * n_sim, mean = intercept1 + (1:4) * slope1, sd = sd1)
  ans_expected <- matrix(ans_expected, nrow = 12)
  ans_expected <- ans_expected[c(1, 5, 9, 2, 6, 10, 3, 7, 11, 4, 8, 12),]
  dimnames(ans_expected) <- list(1:12, NULL)
  expect_equal(ans_obtained, ans_expected)  
})

## 'draw_vals_linar' -----------------------------------------------------------

test_that("'draw_vals_linar' works - along dimension is first", {
  set.seed(0)
  prior <- Lin_AR()
  n_sim <- 10
  matrix_along_by <- matrix(0:11, nr = 3)
  colnames(matrix_along_by) <- 11:14
  slope <- draw_vals_slope(mean_slope = prior$const[["mean_slope"]],
                           sd_slope = prior$const[["sd_slope"]],
                           matrix_along_by = matrix_along_by,
                           n_sim = n_sim)
  sd <- draw_vals_sd(prior = prior,
                     n_sim = n_sim)
  coef <- draw_vals_coef(prior = prior, n_sim = n_sim)
  labels <- 1:12
  set.seed(0)
  ans_obtained <- draw_vals_linar(slope = slope,
                                  sd = sd,
                                  coef = coef,
                                  matrix_along_by = matrix_along_by,
                                  labels = labels)
  intercept <- -(3 + 1) * slope / 2
  mean <- matrix(1:3, nrow = 3, ncol = 4 * n_sim) *
    rep(slope, each = 3) + rep(intercept, each = 3)
  sd <- rep(sd, each = 4)
  coef <- matrix(apply(coef, 2, rep, times = 4), nr = 2)
  set.seed(0)
  error <- draw_vals_ar_inner(n = 3, coef = coef, sd = sd)
  ans_expected <- mean + error
  ans_expected <- matrix(ans_expected, ncol = n_sim)
  dimnames(ans_expected) <- list(1:12, NULL)
  expect_equal(ans_obtained, ans_expected)  
})

## 'draw_vals_lintrend' -------------------------------------------------------

test_that("'draw_vals_lintrend' works - along dimension is first", {
  set.seed(0)
  prior <- Lin()
  n_sim <- 10
  matrix_along_by <- matrix(0:11, nr = 3)
  colnames(matrix_along_by) <- 11:14
  slope <- draw_vals_slope(mean_slope = prior$const[["mean_slope"]],
                           sd_slope = prior$const[["sd_slope"]],
                           matrix_along_by = matrix_along_by,
                           n_sim = n_sim)
  labels <- 1:12
  set.seed(0)
  ans_obtained <- draw_vals_lintrend(slope,
                                     matrix_along_by = matrix_along_by,
                                     labels = labels)
  set.seed(0)
  slope1 <- rep(slope, each = 3)
  intercept1 <- -((3 + 1)/2) * slope1
  ans_expected <- intercept1 + (1:3) * slope1
  ans_expected <- matrix(ans_expected, ncol = n_sim)
  dimnames(ans_expected) <- list(1:12, NULL)
  expect_equal(ans_obtained, ans_expected)  
})

test_that("'draw_vals_lintrend' works - along dimension is second", {
  set.seed(0)
  prior <- Lin()
  n_sim <- 10
  matrix_along_by <- t(matrix(0:11, nr = 3))
  colnames(matrix_along_by) <- 1:3
  slope <- draw_vals_slope(mean_slope = prior$const[["mean_slope"]],
                           sd_slope = prior$const[["sd_slope"]],
                           matrix_along_by = matrix_along_by,
                           n_sim = n_sim)
  labels <- 1:12
  set.seed(0)
  ans_obtained <- draw_vals_lintrend(slope = slope,
                                     matrix_along_by = matrix_along_by,
                                     labels = labels)
  set.seed(0)
  slope1 <- rep(slope, each = 4)
  intercept1 <- -0.5 * (4 + 1) * slope1
  ans_expected <- intercept1 + (1:4) * slope1
  ans_expected <- matrix(ans_expected, nrow = 12)
  ans_expected <- ans_expected[c(1, 5, 9, 2, 6, 10, 3, 7, 11, 4, 8, 12),]
  dimnames(ans_expected) <- list(1:12, NULL)
  expect_equal(ans_obtained, ans_expected)  
})


## 'draw_vals_norm' -------------------------------------------------------

test_that("'draw_vals_norm' works", {
  set.seed(0)
  sd <- runif(10)
  labels <- letters
  set.seed(0)
  ans_obtained <- draw_vals_norm(sd = sd, labels = labels)
  set.seed(0)
  ans_expected <- rnorm(n = 26 * 10, sd = rep(sd, each = 26))
  ans_expected <- matrix(ans_expected, nc = 10)
  dimnames(ans_expected) <- list(letters, NULL)
  expect_equal(ans_obtained, ans_expected)  
})


## 'draw_vals_rw' ----------------------------------------------------------

test_that("'draw_vals_rw' works - n_by = 1, sd_init 0", {
    set.seed(0)
    prior <- RW()
    n_sim <- 1000
    sd <- draw_vals_sd(prior = prior, n_sim = n_sim)
    levels_effect <- 1:200
    matrix_along_by <- matrix(0:199, 200)
    set.seed(0)
    ans <- draw_vals_rw(sd = sd,
                        sd_init = 0,
                        matrix_along_by = matrix_along_by,
                        levels_effect = levels_effect)
    expect_equal(unname(apply(ans, 2, function(x) sd(diff(x)))),
                 sd,
                 tolerance = 0.05)
    expect_identical(dim(ans), c(200L, 1000L))
    expect_identical(dimnames(ans),
                     list(as.character(seq_len(200)), NULL))
})

test_that("'draw_vals_rw' works - along dimension is first, sd_init 0.2", {
  set.seed(0)
  prior <- RW()
  n_sim <- 10
  matrix_along_by <- matrix(0:2999, nc = 3)
  sd <- draw_vals_sd(prior = prior,
                     n_sim = n_sim)
  levels_effect <- 1:3000
  set.seed(0)
  ans <- draw_vals_rw(sd = sd,
                      sd_init = 0.2,
                      matrix_along_by = matrix_along_by,
                      levels_effect = levels_effect)
  expect_identical(dim(ans), c(3000L, 10L))
  expect_identical(dimnames(ans), list(as.character(1:3000), NULL))
  ans <- matrix(ans, nrow = 1000)
  expect_equal(unname(apply(ans[2:1000,], 2, function(x) sd(diff(x)))),
               rep(sd, each = 3),
               tolerance = 0.05)
})

test_that("'draw_vals_rw' works - along dimension is second", {
  set.seed(0)
  prior <- RW()
  n_sim <- 10
  matrix_along_by <- t(matrix(0:2999, nc = 1000))
  sd <- draw_vals_sd(prior = prior,
                     n_sim = n_sim)
  levels_effect <- 1:3000
  set.seed(0)
  ans <- draw_vals_rw(sd = sd,
                      sd_init = 0.2,
                      matrix_along_by = matrix_along_by,
                      levels_effect = levels_effect)
  expect_identical(dim(ans), c(3000L, 10L))
  expect_identical(dimnames(ans), list(as.character(1:3000), NULL))
  ans <- array(ans, dim = c(3, 1000, 10))
  ans <- aperm(ans, perm = c(2, 1, 3))
  ans <- matrix(ans, nrow = 1000)
  expect_equal(unname(apply(ans[2:1000,], 2, function(x) sd(diff(x)))),
               rep(sd, each = 3),
               tolerance = 0.05)
})


## 'draw_vals_rw2' -----------------------------------------------------------

test_that("'draw_vals_rw2' works", {
  set.seed(0)
  prior <- RW2()
  n_sim <- 1000
  sd <- draw_vals_sd(prior = prior, n_sim = n_sim)
  sd_slope <- 0.5
  levels_effect <- 1:100
  set.seed(0)
  ans <- draw_vals_rw2(sd = sd,
                       sd_init = 0,
                       sd_slope = 0.5,
                       matrix_along_by = matrix(0:99, nr = 100),
                       levels_effect = levels_effect)
  expect_equal(unname(apply(ans, 2, function(x) sd(diff(x, diff = 2)))),
               sd,
               tolerance = 0.1)
  expect_identical(dim(ans), c(100L, 1000L))
  expect_identical(dimnames(ans), list(as.character(seq_len(100)), NULL))
})

test_that("'draw_vals_rw2' works - along dimension is first", {
  set.seed(0)
  prior <- RW2()
  n_sim <- 10
  matrix_along_by <- matrix(0:2999, nc = 3)
  sd <- draw_vals_sd(prior = prior,
                     n_sim = n_sim)
  levels_effect <- 1:3000
  set.seed(0)
  ans <- draw_vals_rw2(sd = sd,
                       sd_init = 0.5,
                       sd_slope = 0.1,
                       matrix_along_by = matrix_along_by,
                       levels_effect = levels_effect)
  expect_identical(dim(ans), c(3000L, 10L))
  expect_identical(dimnames(ans), list(as.character(1:3000), NULL))
  ans <- matrix(ans, nrow = 1000)
  expect_equal(unname(apply(ans[3:1000,], 2, function(x) sd(diff(x, diff = 2)))),
               rep(sd, each = 3),
               tolerance = 0.05)
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


## draw_vals_sd_seas ---------------------------------------------------------------

test_that("'draw_vals_sd_seas' works", {
  prior <- RW_Seas(n = 2, s_seas = 0.5)
  n_sim <- 10
  set.seed(0)
  ans_obtained <- draw_vals_sd_seas(prior = prior, n_sim = n_sim)
  set.seed(0)
  ans_expected <- abs(rnorm(n = 10, sd = 0.5))
  expect_identical(ans_obtained, ans_expected)
})


## 'draw_vals_seasfix' ----------------------------------------------------------

test_that("'draw_vals_seasfix' works - along dimension is first", {
  set.seed(0)
  n_sim <- 10
  matrix_along_by <- matrix(0:29, nc = 3)
  set.seed(0)
  ans <- draw_vals_seasfix(n = 4,
                           sd_init = 0.2,
                           matrix_along_by = matrix_along_by,
                           n_sim = n_sim)
  expect_identical(dim(ans), c(30L, 10L))
  expect_equal(ans[1:4,], ans[5:8,], ignore_attr = "dimnames")
  expect_equal(colSums(ans[1:4,]), rep(0, times = 10))
  expect_equal(sd(c(as.numeric(ans[1:3,]),
                    as.numeric(ans[11:13,]),
                    as.numeric(ans[21:23,]))),
               0.2,
               tolerance = 0.2)
})

test_that("'draw_vals_seasfix' works - along dimension is second", {
  set.seed(0)
  n_sim <- 10
  matrix_along_by <- t(matrix(0:29, nc = 10))
  set.seed(0)
  ans <- draw_vals_seasfix(n = 4,
                           sd_init = 0.2,
                           matrix_along_by = matrix_along_by,
                           n_sim = 10)
  expect_identical(dim(ans), c(30L, 10L))
  ans <- array(ans, dim = c(3, 10, 10))
  ans <- aperm(ans, perm = c(2, 1, 3))
  ans <- matrix(ans, nrow = 10)
  expect_equal(ans[1:4,], ans[5:8,], ignore_attr = "dimnames")
})


## 'draw_vals_seasvary' ----------------------------------------------------------

test_that("'draw_vals_seasvary' works - along dimension is first", {
  set.seed(0)
  n_sim <- 10
  matrix_along_by <- matrix(0:2999, nc = 3)
  sd_innov <- abs(rnorm(n = 10))
  set.seed(0)
  ans <- draw_vals_seasvary(n = 4,
                            sd_init = 0.3,
                            sd_innov = sd_innov,
                            matrix_along_by = matrix_along_by)
  expect_identical(dim(ans), c(3000L, 10L))
  ans <- matrix(ans, nrow = 1000)
  ans <- ans[seq.int(from = 0, to = nrow(ans) - 1) %% 4 != 0,]
  expect_equal(unname(apply(ans[-(1:3),], 2, function(x) sd(diff(x, lag = 3)))),
               rep(sd_innov, each = 3),
               tolerance = 0.05)
})

test_that("'draw_vals_seasvary' works - along dimension is second", {
  set.seed(0)
  n_sim <- 10
  matrix_along_by <- t(matrix(0:2999, nc = 1000))
  sd_innov <- abs(rnorm(n = 10))
  set.seed(0)
  ans <- draw_vals_seasvary(n = 4,
                            sd_init = 0.5,
                            sd_innov = sd_innov,
                            matrix_along_by = matrix_along_by)
  expect_identical(dim(ans), c(3000L, 10L))
  ans <- array(ans, dim = c(3, 1000, 10))
  ans <- aperm(ans, perm = c(2, 1, 3))
  ans <- matrix(ans, nrow = 1000)
  ans <- ans[seq.int(from = 0, to = nrow(ans) - 1) %% 4 != 0,]
  expect_equal(unname(apply(ans[-(1:3),], 2, function(x) sd(diff(x, lag = 3)))),
               rep(sd_innov, each = 3),
               tolerance = 0.05)
})


## draw_vals_slope ------------------------------------------------------------

test_that("'draw_vals_slope' works - has 'by' variables", {
  set.seed(0)
  n_sim <- 1000
  mean_slope <- -0.1
  sd_slope <- 0.5
  matrix_along_by <- matrix(0:9, nr = 5, dimnames = list(1:5, c("a", "b")))
  ans_obtained <- draw_vals_slope(mean_slope = mean_slope,
                                  sd_slope = sd_slope,
                                  matrix_along_by = matrix_along_by,
                                  n_sim = n_sim)
  set.seed(0)
  ans_expected <- matrix(rnorm(n = 2000,
                               mean = rep(-0.1, each = 2),
                               sd = rep(0.5, each = 2)),
                         nr = 2)
  rownames(ans_expected) <- paste("slope", c("a", "b"), sep = ".")
  expect_identical(ans_obtained, ans_expected)
})

test_that("'draw_vals_slope' works - no 'by' variables", {
  set.seed(0)
  n_sim <- 1000
  mean_slope <- 0.1
  sd_slope <- 0.5
  matrix_along_by <- matrix(0:9, nr = 10, dimnames = list(1:10))
  ans_obtained <- draw_vals_slope(mean_slope = mean_slope,
                                  sd_slope = sd_slope,
                                  matrix_along_by = matrix_along_by,
                                  n_sim = n_sim)
  set.seed(0)
  ans_expected <- matrix(rnorm(n = 1000, 0.1, 0.5),
                         nr = 1)
  rownames(ans_expected) <- "slope"
  expect_identical(ans_obtained, ans_expected)
})


## draw_vals_spline_mod -------------------------------------------------------

test_that("'draw_vals_spline_mod' works", {
    set.seed(0)
    data <- expand.grid(age = 0:9, time = 2000:2005, sex = c("F", "M"))
    data$popn <- rpois(n = nrow(data), lambda = 100)
    data$deaths <- rpois(n = nrow(data), lambda = 10)
    formula <- deaths ~ age * time + sex
    mod <- mod_pois(formula = formula,
                    data = data,
                    exposure = popn)
    mod <- set_prior(mod, age ~ Sp(n_comp = 4))
    vals_hyper <- draw_vals_hyper_mod(mod = mod, n_sim = 10)
    ans <- draw_vals_spline_mod(mod, vals_hyper = vals_hyper, n_sim = 10)
    expect_identical(names(ans), c("(Intercept)", "age", "time", "sex", "age:time"))
    expect_identical(dim(ans[["age"]]), c(4L, 10L))
})


## draw_vals_svd_mod -------------------------------------------------------

test_that("'draw_vals_svd_mod' works", {
    set.seed(0)
    data <- expand.grid(age = poputils::age_labels(type = "lt", max = 60),
                        time = 2000:2005,
                        sex = c("F", "M"))
    data$popn <- rpois(n = nrow(data), lambda = 100)
    data$deaths <- rpois(n = nrow(data), lambda = 10)
    formula <- deaths ~ age * time + sex
    mod <- mod_pois(formula = formula,
                    data = data,
                    exposure = popn)
    mod <- set_prior(mod, age ~ SVD(HMD))
    mod <- set_prior(mod, age:time ~ SVD_RW(HMD))
    vals_hyper <- draw_vals_hyper_mod(mod = mod, n_sim = 10)
    ans <- draw_vals_svd_mod(mod, vals_hyper = vals_hyper, n_sim = 10)
    expect_identical(names(ans), c("(Intercept)", "age", "time", "sex", "age:time"))
    expect_identical(dim(ans[["age"]]), c(3L, 10L))
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
  aug_sim <- augment(mod_sim, quiet = TRUE)
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
  aug_sim <- augment(mod_sim, quiet = TRUE)
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
  aug_sim <- augment(mod_sim, quiet = TRUE)
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

test_that("'make_report_aug' works with valid inputs - Poisson", {
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
  aug_sim <- augment(mod_sim, quiet = TRUE)
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

test_that("'make_report_aug' works with valid inputs - normal", {
  set.seed(0)
  data <- expand.grid(age = 0:9, time = 2000:2005, sex = c("F", "M"))
  data$weights <- rpois(n = nrow(data), lambda = 100)
  data$income <- rnorm(n = nrow(data))
  formula <- income ~ age + sex + time
  mod <- mod_norm(formula = formula,
                  data = data,
                  weights = weights)
  mod_sim <- mod
  mod_est <- mod
  mod_sim <- set_n_draw(mod, n = 1)
  aug_sim <- augment(mod_sim, quiet = TRUE)
  mod_est$outcome <- as.numeric(aug_sim$income)
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
                    "income", "weights", ".error",
                    ".cover_60", ".cover_80",
                    ".length_60", ".length_80"))
  expect_identical(nrow(ans_obtained), nrow(aug_sim))
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
  prior_class_est <- make_prior_class(mod_est)
  prior_class_sim <- make_prior_class(mod_sim)
  mod_sim <- set_n_draw(mod, n = 1)
  comp_sim <- components(mod_sim, quiet = TRUE)
  aug_sim <- augment(mod_sim, quiet = TRUE)
  mod_est$outcome <- as.numeric(aug_sim$deaths)
  mod_est <- fit(mod_est)
  comp_est <- components(mod_est)
  perform_comp <- list(perform_comp(est = comp_est,
                                    sim = comp_sim,
                                    prior_class_est = prior_class_est,
                                    prior_class_sim = prior_class_sim,
                                    i_sim = 1L,
                                    point_est_fun = "median",
                                    widths = c(0.5, 0.95)))
  ans_obtained <- make_report_comp(perform_comp = perform_comp,
                                   comp_est = comp_est,
                                   comp_sim = comp_sim,
                                   prior_class_est = prior_class_est,
                                   prior_class_sim = prior_class_sim)
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
  aug_sim <- augment(mod_sim, quiet = TRUE)
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
  aug_sim <- augment(mod_sim, quiet = TRUE)
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
  prior_class_est <- make_prior_class(mod_est)
  prior_class_sim <- make_prior_class(mod_sim)
  mod_sim <- set_n_draw(mod, n = 1)
  comp_sim <- components(mod_sim, quiet = TRUE)
  aug_sim <- augment(mod_sim, quiet = TRUE)
  mod_est$outcome <- as.numeric(aug_sim$deaths)
  mod_est <- fit(mod_est)
  comp_est <- components(mod_est)
  ans_obtained <- perform_comp(est = comp_est,
                               sim = comp_sim,
                               prior_class_est = prior_class_est,
                               prior_class_sim = prior_class_sim,
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
  mod_sim <- set_prior(mod_sim, time ~ AR1())
  mod_sim <- set_n_draw(mod_sim, n = 1)
  comp_sim <- components(mod_sim, quiet = TRUE)
  aug_sim <- augment(mod_sim, quiet = TRUE)
  mod_est$outcome <- as.numeric(aug_sim$deaths)
  mod_est <- fit(mod_est)
  comp_est <- components(mod_est)
  prior_class_est <- make_prior_class(mod_est)
  prior_class_sim <- make_prior_class(mod_sim)
  ans_obtained <- perform_comp(est = comp_est,
                               sim = comp_sim,
                               prior_class_est = prior_class_est,
                               prior_class_sim = prior_class_sim,
                               i_sim = 1L,
                               point_est_fun = "median",
                               widths = c(0.6, 0.8))
  names(comp_est)[[4]] <- ".fitted_est"
  names(comp_sim)[[4]] <- ".fitted_sim"
  merged <- merge(comp_est, comp_sim, sort = FALSE)
  merged <- merged[!(merged$term == "time" & merged$component == "hyper"),]
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
    data <- expand.grid(age = 0:5, sex = c("F", "M"))
    data$popn <- rpois(n = nrow(data), lambda = 100)
    data$deaths <- rpois(n = nrow(data), lambda = 10)
    formula <- deaths ~ age + sex
    mod <- mod_pois(formula = formula,
                    data = data,
                    exposure = popn)
    set.seed(0)
    ans_obtained <- report_sim(mod, n_sim = 2)
    expect_setequal(names(ans_obtained), c("components", "augment"))
    ans_obtained_long <- report_sim(mod, n_sim = 2, report = "long")
    expect_setequal(names(ans_obtained_long), c("components", "augment"))
    ans_obtained_full <- report_sim(mod, n_sim = 2, report = "full")
    expect_setequal(names(ans_obtained_full), c("components", "augment"))
})

test_that("'report_sim' works when mod_sim more complicated that mod_est", {
    set.seed(0)
    data <- expand.grid(age = 0:5, sex = c("F", "M"))
    data$popn <- rpois(n = nrow(data), lambda = 100)
    data$deaths <- rpois(n = nrow(data), lambda = 10)
    mod_est <- mod_pois(deaths ~ age + sex,
                        data = data,
                        exposure = popn)
    mod_sim <- mod_pois(deaths ~ age * sex,
                        data = data,
                        exposure = popn) |>
      set_prior(age ~ N())
    set.seed(0)
    ans_obtained <- report_sim(mod_est = mod_est, mod_sim = mod_sim, n_sim = 2)
    expect_setequal(names(ans_obtained), c("components", "augment"))
})

test_that("'report_sim' works with rr3 model", {
    set.seed(0)
    data <- expand.grid(age = poputils::age_labels(type = "five", max = 60),
                        sex = c("F", "M"))
    data$popn <- rpois(n = nrow(data), lambda = 100)
    data$deaths <- 3 * rpois(n = nrow(data), lambda = 5)
    mod_est <- mod_pois(deaths ~ age + sex,
                        data = data,
                        exposure = popn) |>
                        set_prior(age ~ SVD(HMD)) |>
                        set_confidential_rr3()
    set.seed(0)
    ans_obtained <- report_sim(mod_est, n_sim = 2)
    expect_setequal(names(ans_obtained), c("components", "augment"))
})

test_that("'report_sim' works when mod_sim is identical to mod_est - parallel processing", {
  set.seed(0)
  data <- expand.grid(age = 0:4, sex = c("F", "M"))
  data$popn <- rpois(n = nrow(data), lambda = 100)
  data$deaths <- rpois(n = nrow(data), lambda = 10)
  formula <- deaths ~ age + sex
  mod <- mod_pois(formula = formula,
                  data = data,
                  exposure = popn)
  ans_obtained <- report_sim(mod,
                             n_sim = 2,
                             n_core = 2)
  expect_identical(names(ans_obtained), c("components", "augment"))
})

test_that("'report_sim' works with fitted model", {
  set.seed(0)
  data <- expand.grid(age = 0:2, time = 2000:2002, sex = c("F", "M"))
  data$popn <- rpois(n = nrow(data), lambda = 100)
  data$deaths <- rpois(n = nrow(data), lambda = 10)
  formula <- deaths ~ age * time + sex
  mod <- mod_pois(formula = formula,
                  data = data,
                  exposure = popn) |>
                  fit()
  set.seed(0)
  ans1 <- suppressMessages(report_sim(mod_est = mod, n_sim = 1))
  expect_true(is.list(ans1))
  set.seed(0)
  ans2 <- suppressMessages(report_sim(mod_est = mod, mod_sim = mod, n_sim = 1))
  expect_identical(ans1, ans2)
})


## 'vals_covariates_to_dataframe' ---------------------------------------------

test_that("'vals_covariates_to_dataframe' works", {
  set.seed(0)
  data <- expand.grid(age = 0:3,
                      time = 2000:2002,
                      reg = letters[1:3],
                      sex = c("F", "M"))
  data$popn <- rpois(n = nrow(data), lambda = 100)
  data$deaths <- rpois(n = nrow(data), lambda = 10)
  formula <- deaths ~ age * sex + time
  mod <- mod_pois(formula = formula,
                  data = data,
                  exposure = popn)
  mod <- set_covariates(mod, ~ reg)
  set.seed(0)
  vals <- draw_vals_covariates(mod, n_sim = 5)
  ans_obtained <- vals_covariates_to_dataframe(vals)
  ans_expected <- tibble::tibble(term = "covariates",
                                 component = "coef",
                                 level = c("regb", "regc"),
                                 .fitted = rvec::rvec(unname(vals[[1]])))
  expect_identical(ans_obtained, ans_expected)
})


## 'vals_datamod_to_dataframe' ------------------------------------------------

test_that("'vals_datamod_to_dataframe' works with bage_datamod_miscount", {
  set.seed(0)
  prob_mean <- c(0.5, 0.2, 0.3, 0.4)
  prob_disp <- rep(0.4, 4)
  prob_levels <- 1:4
  prob_matrix_outcome <- Matrix::Matrix(kronecker(rep(1, 3), diag(4)))
  prob_arg <- data.frame(sex = prob_levels, mean = prob_mean, disp = prob_disp)
  rate_mean <- c(0.3, 0.3, 0.2)
  rate_disp <- c(0.5, 0.2, 0.6)
  rate_levels <- 1:3
  rate_matrix_outcome <- Matrix::Matrix(kronecker(diag(3), rep(1, 4)))
  rate_arg <- data.frame(age = rate_levels, mean = rate_mean, disp = rate_disp)
  datamod <- new_bage_datamod_miscount(prob_mean = prob_mean,
                                       prob_disp = prob_disp,
                                       prob_levels = prob_levels,
                                       prob_matrix_outcome = prob_matrix_outcome,
                                       prob_arg = prob_arg,
                                       rate_mean = rate_mean,
                                       rate_disp = rate_disp,
                                       rate_levels = rate_levels,
                                       rate_matrix_outcome = rate_matrix_outcome,
                                       rate_arg = rate_arg,
                                       nms_by = c("sex", "age"))
  vals_datamod <- draw_datamod_param(datamod, n_sim = 1000)
  ans <- vals_datamod_to_dataframe(vals_datamod = vals_datamod,
                                   datamod = datamod)
  expect_setequal(names(ans), c("term", "component", "level", ".fitted"))
  expect_true(rvec::is_rvec(ans$.fitted))
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
  n_sim <- 2
  vals_hyper <- draw_vals_hyper_mod(mod, n_sim = n_sim)
  vals_hyperrand <- draw_vals_hyperrand_mod(mod,
                                            vals_hyper = vals_hyper,
                                            n_sim = n_sim)
  vals_spline <- draw_vals_spline_mod(mod,
                                      vals_hyper = vals_hyper,
                                      n_sim = n_sim)
  vals_svd <- draw_vals_svd_mod(mod,
                                vals_hyper = vals_hyper,
                                n_sim = n_sim)
  vals_effect <- draw_vals_effect_mod(mod,
                                      vals_hyper = vals_hyper,
                                      vals_hyperrand = vals_hyperrand,
                                      vals_spline = vals_spline,
                                      vals_svd = vals_svd,
                                      n_sim = n_sim)
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


## vals_hyper_to_dataframe ----------------------------------------------------

test_that("'vals_hyper_to_dataframe' works", {
  set.seed(0)
  n_sim <- 5
  data <- expand.grid(age = poputils::age_labels(type = "lt", max = 60),
                      time = 2000:2009,
                      sex = c("F", "M"))
  data$popn <- rpois(n = nrow(data), lambda = 100)
  data$deaths <- rpois(n = nrow(data), lambda = 10)
  formula <- deaths ~ age * sex + time
  mod <- mod_pois(formula = formula,
                  data = data,
                  exposure = popn)
  mod <- set_prior(mod, age:sex ~ SVD(HMD))
  mod <- set_prior(mod, age ~ AR())
  vals_hyper <- draw_vals_hyper_mod(mod = mod, n_sim = n_sim)
  ans <- vals_hyper_to_dataframe(mod, vals_hyper = vals_hyper, n_sim = n_sim)
  expect_setequal(names(ans), c("term", "component", "level", ".fitted"))
})

## vals_hyper_to_dataframe_one ------------------------------------------------

test_that("'vals_hyper_to_dataframe_one' works", {
  prior <- AR(n_coef = 3)
  vals_hyper <- draw_vals_hyper(prior = prior,
                                n_sim = 10)
  ans_obtained <- vals_hyper_to_dataframe_one(nm = "time",
                                              vals_hyper = vals_hyper,
                                              n_sim = 10)
  ans_expected <- tibble::tibble(term = "time",
                                 component = "hyper",
                                 level = c("coef1", "coef2", "coef3", "sd"),
                                 .fitted = rvec::rvec(unname(rbind(vals_hyper[[1]], vals_hyper[[2]]))))
  expect_equal(ans_obtained, ans_expected)
})

test_that("'vals_hyper_to_dataframe_one' works", {
  prior <- NFix()
  vals_hyper <- draw_vals_hyper(prior = prior,
                                n_sim = 10)
  ans_obtained <- vals_hyper_to_dataframe_one(vals_hyper = vals_hyper,
                                              nm = "time",
                                              n_sim = 10)
  ans_expected <- tibble::tibble(term = character(),
                                 component = character(),
                                 level = character(),
                                 .fitted = rvec::rvec_dbl(matrix(0, nr = 0, nc = 10)))
  expect_equal(ans_obtained, ans_expected)
})


## vals_hyperrand_to_dataframe ------------------------------------------------

test_that("'vals_hyper_to_dataframe' works", {
  set.seed(0)
  n_sim <- 5
  data <- expand.grid(age = poputils::age_labels(type = "lt", max = 60),
                      time = 2000:2009,
                      sex = c("F", "M"))
  data$popn <- rpois(n = nrow(data), lambda = 100)
  data$deaths <- rpois(n = nrow(data), lambda = 10)
  formula <- deaths ~ age * sex + time
  mod <- mod_pois(formula = formula,
                  data = data,
                  exposure = popn)
  mod <- set_prior(mod, age:sex ~ SVD(HMD))
  mod <- set_prior(mod, age ~ AR())
  vals_hyper <- draw_vals_hyper_mod(mod = mod, n_sim = n_sim)
  vals_hyperrand <- draw_vals_hyperrand_mod(mod = mod,
                                            vals_hyper = vals_hyper,
                                            n_sim = n_sim)
  ans <- vals_hyper_to_dataframe(mod, vals_hyper = vals_hyper, n_sim = n_sim)
  expect_setequal(names(ans), c("term", "component", "level", ".fitted"))
})


## vals_hyperrand_to_dataframe_one --------------------------------------------

test_that("'vals_hyperrand_to_dataframe_one' works with 'bage_prior_ar'", {
  prior <- AR(n_coef = 3)
  vals_hyperrand <- list()
  ans_obtained <- vals_hyperrand_to_dataframe_one(prior = prior,
                                                  vals_hyperrand = vals_hyperrand,
                                                  dimnames_term = list(time = 2001:2010),
                                                  var_age = "age",
                                                  var_time = "time",
                                                  n_sim = 10)
  ans_expected <- tibble::tibble(term = character(),
                                 component = character(),
                                 level = character(),
                                 .fitted = rvec::rvec(matrix(0, nrow = 1, ncol = 10)))
  expect_equal(ans_obtained, ans_expected)
})

test_that("'vals_hyperrand_to_dataframe_one' works with bage_prior_lin", {
  set.seed(0)
  prior <- Lin()
  n_sim <- 10
  dimnames_term <- list(time = 2001:2010,
                        region = c("a", "b", "c"))
  var_age <- "age"
  var_time <- "time"
  vals_hyper <- list(sd = runif(n_sim))
  vals_hyperrand <- draw_vals_hyperrand(prior = prior,
                                        vals_hyper = vals_hyper,
                                        dimnames_term = dimnames_term,
                                        var_time = var_time,
                                        var_age = var_age,
                                        n_sim = n_sim)
  ans_obtained <- vals_hyperrand_to_dataframe_one(prior = prior,
                                                  dimnames_term = list(time = 2001:2010,
                                                                       region = 1:3),
                                                  var_age = "age",
                                                  var_time = "time",
                                                  vals_hyperrand = vals_hyperrand,
                                                  n_sim = n_sim)
  .fitted <- rbind(vals_hyperrand$slope,
                   vals_hyperrand$trend,
                   vals_hyperrand$error)
  .fitted <- unname(.fitted)
  .fitted <- rvec::rvec(.fitted)
  ans_expected <- tibble::tibble(term = "time:region",
                                 component = rep(c("hyper", "trend", "error"),
                                                 times = c(3, 30, 30)),
                                 level = c(paste("slope", c("a", "b", "c"), sep = "."),
                                           rep(paste(2001:2010,
                                                     rep(c("a", "b", "c"), each = 10),
                                                     sep = "."),
                                               times = 2)),
                                 .fitted = .fitted)
  expect_equal(ans_obtained, ans_expected)
})


## 'vals_spline_to_dataframe' -------------------------------------------------

test_that("'vals_spline_to_dataframe' works", {
  set.seed(0)
  data <- expand.grid(age = 0:9, time = 2000:2005, sex = c("F", "M"))
  data$popn <- rpois(n = nrow(data), lambda = 100)
  data$deaths <- rpois(n = nrow(data), lambda = 10)
  formula <- deaths ~ age * time + sex
  mod <- mod_pois(formula = formula,
                  data = data,
                  exposure = popn)
  mod <- set_prior(mod, age ~ Sp(n_comp = 4))
  mod <- set_prior(mod, time ~ Sp(n_comp = 4))
  vals_hyper <- draw_vals_hyper_mod(mod = mod, n_sim = 10)
  vals_spline <- draw_vals_spline_mod(mod, vals_hyper = vals_hyper, n_sim = 10)
  ans <- vals_spline_to_dataframe(mod = mod,
                                  vals_spline = vals_spline,
                                  n_sim = 10)
  expect_setequal(names(ans), c("term", "component", "level", ".fitted"))
})


## 'vals_spline_to_dataframe_one' ---------------------------------------------

test_that("'vals_spline_to_dataframe_one' works", {
  set.seed(0)
  data <- expand.grid(age = 0:9, time = 2000:2005, sex = c("F", "M"))
  data$popn <- rpois(n = nrow(data), lambda = 100)
  data$deaths <- rpois(n = nrow(data), lambda = 10)
  formula <- deaths ~ age * time + sex
  mod <- mod_pois(formula = formula,
                  data = data,
                  exposure = popn)
  mod <- set_prior(mod, age ~ Sp(n_comp = 4))
  vals_hyper <- draw_vals_hyper_mod(mod = mod, n_sim = 10)
  vals_spline <- draw_vals_spline_mod(mod, vals_hyper = vals_hyper, n_sim = 10)$age
  ans_obtained <- vals_spline_to_dataframe_one(vals_spline = vals_spline,
                                               nm = "age",
                                               n_sim = 10)
  ans_expected <- tibble::tibble(term = "age",
                                 component = "spline",
                                 level = paste0("comp", 1:4),
                                 .fitted = rvec::rvec(unname(vals_spline)))
  expect_identical(ans_obtained, ans_expected)                                                 
})


## 'vals_svd_to_dataframe' ----------------------------------------------------

test_that("'vals_svd_to_dataframe' works", {
  set.seed(0)
  data <- expand.grid(age = 0:9, time = 2000:2005, sex = c("F", "M"))
  data$popn <- rpois(n = nrow(data), lambda = 100)
  data$deaths <- rpois(n = nrow(data), lambda = 10)
  formula <- deaths ~ age * time + sex
  mod <- mod_pois(formula = formula,
                  data = data,
                  exposure = popn)
  mod <- set_prior(mod, age ~ Sp(n_comp = 4))
  mod <- set_prior(mod, time ~ Sp(n_comp = 4))
  vals_hyper <- draw_vals_hyper_mod(mod = mod, n_sim = 10)
  vals_svd <- draw_vals_svd_mod(mod, vals_hyper = vals_hyper, n_sim = 10)
  ans <- vals_svd_to_dataframe(mod = mod,
                               vals_svd = vals_svd,
                               n_sim = 10)
  expect_setequal(names(ans), c("term", "component", "level", ".fitted"))
})


## 'vals_svd_to_dataframe_one' ------------------------------------------------

test_that("'vals_svd_to_dataframe_one' works", {
  set.seed(0)
  data <- expand.grid(age = poputils::age_labels(type = "lt", max = 60),
                      time = 2000:2005,
                      sex = c("F", "M"))
  data$popn <- rpois(n = nrow(data), lambda = 100)
  data$deaths <- rpois(n = nrow(data), lambda = 10)
  formula <- deaths ~ age * time + sex
  mod <- mod_pois(formula = formula,
                  data = data,
                  exposure = popn)
  mod <- set_prior(mod, age ~ SVD(HMD, n_comp = 4))
  vals_hyper <- draw_vals_hyper_mod(mod = mod, n_sim = 10)
  vals_svd <- draw_vals_svd_mod(mod, vals_hyper = vals_hyper, n_sim = 10)$age
  ans_obtained <- vals_svd_to_dataframe_one(vals_svd = vals_svd,
                                            nm = "age",
                                            n_sim = 10)
  ans_expected <- tibble::tibble(term = "age",
                                 component = "svd",
                                 level = paste0("comp", 1:4),
                                 .fitted = rvec::rvec(unname(vals_svd)))
  expect_identical(ans_obtained, ans_expected)                                                 
})
