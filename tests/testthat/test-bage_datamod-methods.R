

## 'make_expected_obs_exposure' -----------------------------------------------

test_that("'make_expected_obs_exposure' works", {
  ratio <- c(0.5, 0.2, 0.3, 0.4)
  disp_mean <- c(0.3, 0.2, 0.3, 0.2)
  matrix_ratio_outcome <- Matrix::Matrix(kronecker(rep(1, 3), diag(4)))
  matrix_disp_outcome <- Matrix::Matrix(kronecker(rep(1, 3), diag(4)))
  datamod <- new_bage_datamod_exposure(ratio = ratio,
                                       disp_mean = disp_mean,
                                       matrix_ratio_outcome = matrix_ratio_outcome,
                                       matrix_disp_outcome = matrix_disp_outcome)
  components <- tibble::tibble(
    term = c("(Intercept)", rep("datamod", 4)),
    component = c("(Intercept)", rep("disp", 4)),
    level = c("(Intercept)", 0:3),
    .fitted = rvec::runif_rvec(n = 5, n_draw = 10)
  )
  expected <- exp(rvec::rnorm_rvec(n = 12, n_draw = 10))
  ans_obtained <- make_expected_obs_exposure(datamod = datamod,
                                             components = components,
                                             expected = expected)
  ratio <- as.numeric(matrix_ratio_outcome %*% ratio)
  disp <- as.matrix(matrix_disp_outcome) %*% components$.fitted[-1]
  ans_expected <- ((3 + 1/disp)/(1 + 1/disp)) * (expected / ratio)
  expect_equal(ans_obtained, ans_expected)
})


## 'make_expected_obs_miscount' ----------------------------------------------

test_that("'make_expected_obs_miscount' works", {
  prob_mean <- c(0.5, 0.2, 0.3, 0.4)
  prob_disp <- c(0.3, 0.2, 0.3, 0.2)
  rate_mean <- c(0.5, 0.2, 0.3, 0.4)
  rate_disp <- c(0.3, 0.2, 0.3, 0.2)
  matrix_prob_outcome <- Matrix::Matrix(kronecker(rep(1, 3), diag(4)))
  matrix_rate_outcome <- Matrix::Matrix(kronecker(rep(1, 3), diag(4)))
  datamod <- new_bage_datamod_miscount(prob_mean = prob_mean,
                                       prob_disp = prob_disp,
                                       rate_mean = rate_mean,
                                       rate_disp = rate_disp,
                                       matrix_prob_outcome = matrix_prob_outcome,
                                       matrix_rate_outcome = matrix_rate_outcome)
  components <- tibble::tibble(
    term = c("(Intercept)", rep("datamod", 8)),
    component = c("(Intercept)", rep(c("prob", "rate"), each = 4)),
    level = c("(Intercept)", 0:3, 0:3),
    .fitted = rvec::runif_rvec(n = 9, n_draw = 10)
  )
  expected <- exp(rvec::rnorm_rvec(n = 12, n_draw = 10))
  ans_obtained <- make_expected_obs_miscount(datamod = datamod,
                                             components = components,
                                             expected = expected)
  prob <- as.matrix(matrix_prob_outcome) %*% components$.fitted[2:5]
  rate <- as.matrix(matrix_rate_outcome) %*% components$.fitted[6:9]
  ans_expected <- (prob + rate) * expected
  expect_identical(ans_obtained, ans_expected)
})


## 'make_expected_obs_overcount' ----------------------------------------------

test_that("'make_expected_obs_overcount' works", {
  rate_mean <- c(0.5, 0.2, 0.3, 0.4)
  rate_disp <- c(0.3, 0.2, 0.3, 0.2)
  matrix_rate_outcome <- Matrix::Matrix(kronecker(rep(1, 3), diag(4)))
  datamod <- new_bage_datamod_overcount(rate_mean = rate_mean,
                                        rate_disp = rate_disp,
                                        matrix_rate_outcome = matrix_rate_outcome)
  components <- tibble::tibble(
    term = c("(Intercept)", rep("datamod", 4)),
    component = c("(Intercept)", rep("rate", 4)),
    level = c("(Intercept)", 0:3),
    .fitted = rvec::runif_rvec(n = 5, n_draw = 10)
  )
  expected <- exp(rvec::rnorm_rvec(n = 12, n_draw = 10))
  ans_obtained <- make_expected_obs_overcount(datamod = datamod,
                                              components = components,
                                              expected = expected)
  rate <- as.matrix(matrix_rate_outcome) %*% components$.fitted[-1]
  ans_expected <- (1 + rate) * expected
  expect_identical(ans_obtained, ans_expected)
})


## 'make_expected_obs_undercount' ----------------------------------------------

test_that("'make_expected_obs_undercount' works", {
  prob_mean <- c(0.5, 0.2, 0.3, 0.4)
  prob_disp <- c(0.3, 0.2, 0.3, 0.2)
  matrix_prob_outcome <- Matrix::Matrix(kronecker(rep(1, 3), diag(4)))
  datamod <- new_bage_datamod_undercount(prob_mean = prob_mean,
                                         prob_disp = prob_disp,
                                         matrix_prob_outcome = matrix_prob_outcome)
  components <- tibble::tibble(
    term = c("(Intercept)", rep("datamod", 4)),
    component = c("(Intercept)", rep("prob", 4)),
    level = c("(Intercept)", 0:3),
    .fitted = rvec::runif_rvec(n = 5, n_draw = 10)
  )
  expected <- exp(rvec::rnorm_rvec(n = 12, n_draw = 10))
  ans_obtained <- make_expected_obs_undercount(datamod = datamod,
                                               components = components,
                                               expected = expected)
  prob <- as.matrix(matrix_prob_outcome) %*% components$.fitted[-1]
  ans_expected <- prob * expected
  expect_identical(ans_obtained, ans_expected)
})



## OLD ################################################################


## 'draw_vals_outcome_true' ---------------------------------------------------

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
  vals_expected <- exp(make_linpred_from_components(mod = mod,
                                                    components = vals_components,
                                                    data = mod$data,
                                                    dimnames_term = mod$dimnames_terms))
  vals_fitted <- draw_vals_fitted(mod = mod,
                                  vals_expected = vals_expected,
                                  vals_disp = vals_disp,
                                  outcome = NULL,
                                  offset = NULL)
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
  vals_expected <- invlogit(make_linpred_from_components(mod = mod,
                                                         components = vals_components,
                                                         data = mod$data,
                                                         dimnames_term = mod$dimnames_terms))
  vals_fitted <- draw_vals_fitted(mod = mod,
                                  vals_expected = vals_expected,
                                  vals_disp = vals_disp,
                                  outcome = NULL,
                                  offset = NULL)
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
  vals_expected <- invlogit(make_linpred_from_components(mod = mod,
                                                         components = vals_components,
                                                         data = mod$data,
                                                         dimnames_term = mod$dimnames_terms))
  vals_fitted <- draw_vals_fitted(mod = mod,
                                  vals_expected = vals_expected,
                                  vals_disp = vals_disp,
                                  outcome = NULL,
                                  offset = NULL)
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
  scale_linpred <- get_fun_orig_scale_linpred(mod)
  vals_fitted <- scale_linpred(make_linpred_from_components(mod = mod,
                                                            components = vals_components,
                                                            data = mod$data,
                                                            dimnames_term = mod$dimnames_terms))
  set.seed(1)
  ans_obtained <- draw_vals_outcome_true(datamod = NULL,
                                         nm_distn = "norm",
                                         outcome_obs = scale_linpred(mod$outcome),
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
  scale_linpred <- get_fun_orig_scale_linpred(mod)
  vals_fitted <- scale_linpred(make_linpred_from_components(mod = mod,
                                                            components = vals_components,
                                                            data = mod$data,
                                                            dimnames_term = mod$dimnames_terms))
  set.seed(1)
  ans_obtained <- draw_vals_outcome_true(datamod = NULL,
                                         nm_distn = "norm",
                                         outcome_obs = scale_linpred(mod$outcome),
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
  vals_expected <- exp(make_linpred_from_components(mod = mod,
                                                    components = vals_components,
                                                    data = mod$data,
                                                    dimnames_term = mod$dimnames_terms))
  vals_fitted <- draw_vals_fitted(mod = mod,
                                  vals_expected = vals_expected,
                                  vals_disp = vals_disp,
                                  outcome = NULL,
                                  offset = NULL)
  set.seed(1)
  expect_error(draw_vals_outcome_true(datamod = NULL,
                                      nm_distn = "wrong",
                                      outcome_obs = mod$outcome,
                                      fitted = vals_fitted,
                                      disp = vals_disp,
                                      offset = mod$offset),
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



  
  

