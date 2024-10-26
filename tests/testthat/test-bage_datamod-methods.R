
## 'draw_vals_outcome_true' ---------------------------------------------------

test_that("'draw_vals_outcome_true' works with NULL, pois, offset complete", {
  set.seed(0)
  n_sim <- 10
  data <- expand.grid(age = 0:9, time = 2000:2005, sex = c("F", "M"))
  data$deaths <- rpois(n = nrow(data), lambda = 20)
  data$deaths[c(1, 5, 10)] <- NA
  formula <- deaths ~ age + sex + time
  mod <- mod_pois(formula = formula,
                  data = data,
                  exposure = 1)
  vals_components <- draw_vals_components_unfitted(mod = mod,
                                                   n_sim = n_sim)
  vals_disp <- vals_components$.fitted[vals_components$component == "disp"]
  vals_expected <- exp(make_linpred_comp(components = vals_components,
                                         data = mod$data,
                                         dimnames_term = mod$dimnames_terms))
  vals_fitted <- draw_vals_fitted(mod = mod,
                                  vals_expected = vals_expected,
                                  vals_disp = vals_disp)
  set.seed(1)
  ans_obtained <- draw_vals_outcome_true(datamod = NULL,
                                         nm_distn = "pois",
                                         outcome_obs = mod$outcome,
                                         fitted = vals_fitted,
                                         disp = vals_disp,
                                         offset = mod$offset)
  set.seed(1)
  ans_expected <- rvec::rvec_dbl(matrix(data$deaths, nrow = nrow(data), ncol = 10))
  ans_expected[c(1, 5, 10)] <- rvec::rpois_rvec(n = 3,
                                                lambda = mod$offset[c(1, 5, 10)] * vals_fitted[c(1, 5, 10)])
  expect_equal(ans_obtained, ans_expected)
})

test_that("'draw_vals_outcome_true' works with pois, NULL, offset has NA", {
  set.seed(0)
  n_sim <- 10
  data <- expand.grid(age = 0:9, time = 2000:2005, sex = c("F", "M"))
  data$popn <- rpois(n = nrow(data), lambda = 100)
  data$deaths <- rpois(n = nrow(data), lambda = 0.5 * data$popn)
  data$deaths[3] <- NA
  data$popn[3] <- NA
  formula <- deaths ~ age + sex + time
  mod <- mod_pois(formula = formula,
                  data = data,
                  exposure = popn)
  vals_components <- draw_vals_components_unfitted(mod = mod,
                                                   n_sim = n_sim)
  vals_disp <- vals_components$.fitted[vals_components$component == "disp"]
  vals_expected <- exp(make_linpred_comp(components = vals_components,
                                         data = mod$data,
                                         dimnames_term = mod$dimnames_terms))
  vals_fitted <- draw_vals_fitted(mod = mod,
                                  vals_expected = vals_expected,
                                  vals_disp = vals_disp)
  set.seed(1)
  ans_obtained <- draw_vals_outcome_true(datamod = NULL,
                                         nm_distn = "pois",
                                         outcome_obs = mod$outcome,
                                         fitted = vals_fitted,
                                         disp = vals_disp,
                                         offset = mod$offset)
  set.seed(1)
  ans_expected <- rvec::rvec(matrix(1 * data$deaths, nrow = nrow(data), ncol = 10))
  ans_expected[3] <- NA
  expect_equal(ans_obtained, ans_expected)
})

test_that("'draw_vals_outcome_true' works with NULL, binom, data complete", {
  set.seed(0)
  n_sim <- 10
  data <- expand.grid(age = 0:9, time = 2000:2005, sex = c("F", "M"))
  data$popn <- rpois(n = nrow(data), lambda = 20)
  data$deaths <- rbinom(n = nrow(data), size = data$popn, prob = 0.8)
  formula <- deaths ~ age + sex + time
  mod <- mod_binom(formula = formula,
                   data = data,
                   size = popn)
  vals_components <- draw_vals_components_unfitted(mod = mod,
                                                   n_sim = n_sim)
  vals_disp <- vals_components$.fitted[vals_components$component == "disp"]
  invlogit <- function(x) exp(x) / (1 + exp(x))
  vals_expected <- invlogit(make_linpred_comp(components = vals_components,
                                              data = mod$data,
                                              dimnames_term = mod$dimnames_terms))
  vals_fitted <- draw_vals_fitted(mod = mod,
                                  vals_expected = vals_expected,
                                  vals_disp = vals_disp)
  set.seed(1)
  ans_obtained <- draw_vals_outcome_true(datamod = NULL,
                                         nm_distn = "binom",
                                         outcome_obs = mod$outcome,
                                         fitted = vals_fitted,
                                         disp = vals_disp,
                                         offset = mod$offset)
  set.seed(1)
  ans_expected <- rvec::rvec(matrix(1*data$deaths, nrow = nrow(data), ncol = 10))
  expect_equal(ans_obtained, ans_expected)
})

test_that("'draw_vals_outcome_true' works with NULL, binom, has offset has na", {
  set.seed(0)
  n_sim <- 10
  data <- expand.grid(age = 0:9, time = 2000:2005, sex = c("F", "M"))
  data$popn <- rpois(n = nrow(data), lambda = 20)
  data$deaths <- rbinom(n = nrow(data), size = data$popn, prob = 0.8)
  data$deaths[1] <- NA
  data$popn[3] <- NA
  formula <- deaths ~ age + sex + time
  mod <- mod_binom(formula = formula,
                   data = data,
                   size = popn)
  vals_components <- draw_vals_components_unfitted(mod = mod,
                                                   n_sim = n_sim)
  vals_disp <- vals_components$.fitted[vals_components$component == "disp"]
  invlogit <- function(x) exp(x) / (1 + exp(x))
  vals_expected <- invlogit(make_linpred_comp(components = vals_components,
                                              data = mod$data,
                                              dimnames_term = mod$dimnames_terms))
  vals_fitted <- draw_vals_fitted(mod = mod,
                                  vals_expected = vals_expected,
                                  vals_disp = vals_disp)
  set.seed(1)
  ans_obtained <- draw_vals_outcome_true(datamod = NULL,
                                         nm_distn = "binom",
                                         outcome_obs = mod$outcome,
                                         fitted = vals_fitted,
                                         disp = vals_disp,
                                         offset = mod$offset)
  set.seed(1)
  ans_expected <- rvec::rvec(matrix(1*data$deaths, nrow = nrow(data), ncol = 10))
  ans_expected[1] <- rvec::rbinom_rvec(n = 1 ,
                                       size = mod$offset[1],
                                       prob = vals_fitted[1])
  expect_equal(ans_obtained, ans_expected)
})

test_that("'draw_vals_outcome_true' works with NULL, norm, no na", {
  set.seed(0)
  n_sim <- 10
  data <- expand.grid(age = 0:9, time = 2000:2005, sex = c("F", "M"))
  data$income <- rnorm(n = nrow(data), mean = 20, sd = 3)
  data$income[1] <- NA
  data$wt <- rpois(n = nrow(data), lambda = 100)
  formula <- income ~ age + sex + time
  mod <- mod_norm(formula = formula,
                  data = data,
                  weights = wt)
  vals_components <- draw_vals_components_unfitted(mod = mod,
                                                   n_sim = n_sim)
  vals_disp <- vals_components$.fitted[vals_components$component == "disp"]
  scale_outcome <- get_fun_scale_outcome(mod)
  vals_fitted <- scale_outcome(make_linpred_comp(components = vals_components,
                                                 data = mod$data,
                                                 dimnames_term = mod$dimnames_terms))
  set.seed(1)
  ans_obtained <- draw_vals_outcome_true(datamod = NULL,
                                         nm_distn = "norm",
                                         outcome_obs = scale_outcome(mod$outcome),
                                         fitted = vals_fitted,
                                         disp = mod$outcome_sd * vals_disp,
                                         offset = mod$offset)
  set.seed(1)
  ans_expected <- rvec::rvec(matrix(data$income, nrow = nrow(data), ncol = 10))
  ans_expected[1] <- rvec::rnorm_rvec(n = 1 ,
                                      mean = vals_fitted[1],
                                      sd = mod$outcome_sd * vals_disp / sqrt(mod$offset[1]))
  expect_equal(ans_obtained, ans_expected)
})

test_that("'draw_vals_outcome_true' works with NULL, norm, has NA", {
  set.seed(0)
  n_sim <- 10
  data <- expand.grid(age = 0:9, time = 2000:2005, sex = c("F", "M"))
  data$income <- rnorm(n = nrow(data), mean = 20, sd = 3)
  data$wt <- rpois(n = nrow(data), lambda = 100)
  data$income[3] <- NA
  data$wt[3] <- NA
  formula <- income ~ age + sex + time
  mod <- mod_norm(formula = formula,
                  data = data,
                  weights = wt)
  vals_components <- draw_vals_components_unfitted(mod = mod,
                                                   n_sim = n_sim)
  vals_disp <- vals_components$.fitted[vals_components$component == "disp"]
  vals_disp <- mod$outcome_sd * vals_disp
  scale_outcome <- get_fun_scale_outcome(mod)
  vals_fitted <- scale_outcome(make_linpred_comp(components = vals_components,
                                                 data = mod$data,
                                                 dimnames_term = mod$dimnames_terms))
  set.seed(1)
  ans_obtained <- draw_vals_outcome_true(datamod = NULL,
                                         nm_distn = "norm",
                                         outcome_obs = scale_outcome(mod$outcome),
                                         fitted = vals_fitted,
                                         disp = vals_disp,
                                         offset = mod$offset)
  set.seed(1)
  ans_expected <- rvec::rvec(matrix(data$income, nrow = nrow(data), ncol = 10))
  ans_expected[3]<- NA
  expect_equal(ans_obtained, ans_expected)
})

test_that("'draw_vals_outcome_true' method for NULL throws correct error with invalid nm_distn", {
  set.seed(0)
  n_sim <- 10
  data <- expand.grid(age = 0:9, time = 2000:2005, sex = c("F", "M"))
  data$deaths <- rpois(n = nrow(data), lambda = 20)
  data$deaths[c(1, 5, 10)] <- NA
  formula <- deaths ~ age + sex + time
  mod <- mod_pois(formula = formula,
                  data = data,
                  exposure = 1)
  vals_components <- draw_vals_components_unfitted(mod = mod,
                                                   n_sim = n_sim)
  vals_disp <- vals_components$.fitted[vals_components$component == "disp"]
  vals_expected <- exp(make_linpred_comp(components = vals_components,
                                         data = mod$data,
                                         dimnames_term = mod$dimnames_terms))
  vals_fitted <- draw_vals_fitted(mod = mod,
                                  vals_expected = vals_expected,
                                  vals_disp = vals_disp)
  set.seed(1)
  expect_error(draw_vals_outcome_true(datamod = NULL,
                                         nm_distn = "wrong",
                                         outcome_obs = mod$outcome,
                                         fitted = vals_fitted,
                                         disp = vals_disp,
                                      offset = mod$offset),
               "Internal error: Invalid value for `nm_distn`.")
})


test_that("'draw_vals_outcome_true' works with rr3, pois, no na", {
  set.seed(0)
  fitted <- rvec::rgamma_rvec(n = 100, shape = 2, rate = 0.4, n_draw = 100)
  offset <- rep(10, 100)
  outcome_true <- rpois(n = 100, lambda = 50)
  outcome_obs <- poputils::rr3(outcome_true)
  datamod <- new_bage_datamod_outcome_rr3()
  ans <- draw_vals_outcome_true(datamod = datamod,
                                nm_distn = "pois",
                                outcome_obs = outcome_obs,
                                fitted = fitted,
                                offset = offset)
  ans <- rvec::draws_mean(ans)
  expect_equal(mean(ans), 50, tolerance = 0.05)
  expect_true(all(abs(as.matrix(ans) - outcome_obs) <= 2L))
})

test_that("'draw_vals_outcome_true' works with rr3, pois, has na", {
  set.seed(0)
  fitted <- rvec::rgamma_rvec(n = 100, shape = 2, rate = 0.4, n_draw = 100)
  offset <- rep(10, 100)
  offset[1] <- NA
  outcome_true <- rpois(n = 100, lambda = 50)
  outcome_obs <- poputils::rr3(outcome_true)
  outcome_obs[4] <- NA
  datamod <- new_bage_datamod_outcome_rr3()
  ans <- draw_vals_outcome_true(datamod = datamod,
                                nm_distn = "pois",
                                outcome_obs = outcome_obs,
                                fitted = fitted,
                                offset = offset)
  ans <- rvec::draws_mean(ans)
  expect_equal(mean(ans, na.rm = TRUE), 50, tolerance = 0.05)
  expect_true(all(abs(as.matrix(ans[-c(1, 4)]) - outcome_obs[-c(1, 4)]) <= 2L))
})

test_that("'draw_vals_outcome_true' works with rr3, binom, no na", {
  set.seed(0)
  fitted <- rvec::rbeta_rvec(n = 100, shape1 = 10, shape2 = 10, n_draw = 100)
  offset <- rep(100, 100)
  outcome_true <- rbinom(n = 100, size = 100, prob = 0.5)
  outcome_obs <- poputils::rr3(outcome_true)
  datamod <- new_bage_datamod_outcome_rr3()
  ans <- draw_vals_outcome_true(datamod = datamod,
                                nm_distn = "binom",
                                outcome_obs = outcome_obs,
                                fitted = fitted,
                                offset = offset)
  ans <- rvec::draws_mean(ans)
  expect_equal(mean(ans), 50, tolerance = 0.05)
  expect_true(all(abs(as.matrix(ans) - outcome_obs) <= 2L))
})

test_that("'draw_vals_outcome_true' works with rr3, binom, has na", {
  set.seed(0)
  fitted <- rvec::rbeta_rvec(n = 100, shape1 = 10, shape2 = 10, n_draw = 100)
  offset <- rep(100, 100)
  offset[1] <- NA
  outcome_true <- rpois(n = 100, lambda = 50)
  outcome_obs <- poputils::rr3(outcome_true)
  outcome_obs[4] <- NA
  datamod <- new_bage_datamod_outcome_rr3()
  ans <- draw_vals_outcome_true(datamod = datamod,
                                nm_distn = "binom",
                                outcome_obs = outcome_obs,
                                fitted = fitted,
                                offset = offset)
  ans <- rvec::draws_mean(ans)
  expect_equal(mean(ans, na.rm = TRUE), 50, tolerance = 0.05)
  expect_true(all(abs(as.matrix(ans[-c(1, 4)]) - outcome_obs[-c(1, 4)]) <= 2L))
})

test_that("'draw_vals_outcome_true' throws appropriate error with invalid nm_distn", {
  set.seed(0)
  fitted <- rvec::rgamma_rvec(n = 100, shape = 2, rate = 0.4, n_draw = 100)
  offset <- rep(10, 100)
  outcome_true <- rpois(n = 100, lambda = 50)
  outcome_obs <- poputils::rr3(outcome_true)
  datamod <- new_bage_datamod_outcome_rr3()
  expect_error(draw_vals_outcome_true(datamod = datamod,
                                      nm_distn = "wrong",
                                      outcome_obs = outcome_obs,
                                      fitted = fitted,
                                      disp = NULL,
                                      offset = offset),
               "Internal error: Invalid value for `nm_distn`.")
})


## 'make_i_lik' ---------------------------------------------------------------

test_that("'make_i_lik' works with bage_datamod_outcome_rr3", {
  x <- new_bage_datamod_outcome_rr3()
  expect_identical(make_i_lik(x, nm_distn = "binom", has_disp = FALSE), 102L)
  expect_identical(make_i_lik(x, nm_distn = "pois", has_disp = FALSE), 302L)
  expect_identical(make_i_lik(x, nm_distn = "binom", has_disp = TRUE), 104L)
  expect_identical(make_i_lik(x, nm_distn = "pois", has_disp = TRUE), 304L)
  expect_error(make_i_lik(x, nm_distn = "wrong", has_disp = TRUE),
               "Internal error: Invalid inputs.")
})


## 'str_call_datamod' ----------------------------------------------------------------------

test_that("'str_call_datamod' works", {
  expect_identical(str_call_datamod(new_bage_datamod_outcome_rr3()),
                   "rr3()")
})



  
  

