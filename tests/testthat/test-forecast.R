
## 'forecast_components' ----------------------------------------------------------

test_that("'forecast_components' works", {
  set.seed(0)
  data <- expand.grid(age = 0:9, time = 2000:2005, sex = c("F", "M"))
  data$deaths <- rpois(n = nrow(data), lambda = 10000)
  formula <- deaths ~ age * sex + sex * time
  mod <- mod_pois(formula = formula,
                  data = data,
                  exposure = 1)
  mod <- set_n_draw(mod, n = 10)
  mod <- fit(mod)
  components_est <- components(mod)
  labels_forecast <- 2006:2008
  set.seed(1)
  ans <- forecast_components(mod = mod,
                             components_est = components_est,
                             labels_forecast = labels_forecast)
  expect_setequal(components_est$component, ans$component)
  expect_setequal(components_est$term, ans$term)
  expect_setequal(c(2006:2008, "sd"),
                  ans$level[ans$term == "time"])
  expect_setequal(c(paste("F", 2006:2008, sep = "."),
                    paste("M", 2006:2008, sep = "."),
                    "sd"),
                  ans$level[ans$term == "sex:time"])
  expect_true(all(c("(Intercept)", "age", "age:sex", "disp") %in% ans$term))                  
})


## 'forecast_effects' ---------------------------------------------------------

test_that("'forecast_effects' works with ordinary time priors", {
  set.seed(0)
  data <- expand.grid(age = 0:9, time = 2000:2005, sex = c("F", "M"))
  data$deaths <- rpois(n = nrow(data), lambda = 10000)
  formula <- deaths ~ age + sex + time
  mod <- mod_pois(formula = formula,
                  data = data,
                  exposure = 1)
  mod <- set_n_draw(mod, n = 10)
  mod <- fit(mod)
  comp <- components(mod)
  hypers_est <- comp[comp$component == "hyper", ]
  hypers_forecast <- hypers_est ## include for testing - ignored by forecast_effect methods
  labels_forecast <- 2006:2008
  set.seed(1)
  effects_est <- comp[comp$component == "effect", ]
  set.seed(1)
  ans_obtained <- forecast_effects(mod = mod,
                                   hypers_est = hypers_est,
                                   hypers_forecast = hypers_forecast,
                                   effects_est = effects_est,
                                   labels_forecast = labels_forecast)
  set.seed(1)
  ans_expected <- forecast_effect(prior = mod$priors[[4]],
                                  nm_prior = "time",
                                  hyper_est = comp[comp$term == "time" & comp$component == "hyper",],
                                  hyper_forecast = NULL,
                                  effect_est = comp[comp$term == "time" & comp$component == "effect",],
                                  matrix_along_by_est = matrix(0:5, nr = 6),
                                  matrix_along_by_forecast = matrix(0:2, nr = 3),
                                  levels_forecast = as.character(2006:2008))
  expect_identical(ans_obtained, ans_expected)
})

test_that("'forecast_effects' works with no hyper-parameters", {
  set.seed(0)
  data <- expand.grid(age = 0:9, time = 2000:2005, sex = c("F", "M"))
  data$deaths <- rpois(n = nrow(data), lambda = 10000)
  formula <- deaths ~ age + sex + time
  mod <- mod_pois(formula = formula,
                  data = data,
                  exposure = 1)
  mod <- set_prior(mod, age ~ NFix(sd = 0.2))
  mod <- set_prior(mod, time ~ NFix(sd = 0.2))
  mod <- set_n_draw(mod, n = 10)
  mod <- fit(mod)
  comp <- components(mod)
  labels_forecast <- 2006:2008
  set.seed(1)
  hypers_est <- comp[comp$component == "hyper",]
  effects_est <- comp[comp$component == "effect", ]
  set.seed(1)
  ans_obtained <- forecast_effects(mod = mod,
                                   hypers_est = hypers_est,
                                   hypers_forecast = NULL,
                                   effects_est = effects_est,
                                   labels_forecast = labels_forecast)
  set.seed(1)
  ans_expected <- forecast_effect(prior = mod$priors[[4]],
                                  nm_prior = "time",
                                  hyper_est = NULL,
                                  hyper_forecast = NULL,
                                  effect_est = comp[comp$term == "time" & comp$component == "effect",],
                                  matrix_along_by_est = matrix(0:5, nr = 6),
                                  matrix_along_by_forecast = matrix(0:2, nr = 3),
                                  levels_forecast = as.character(2006:2008))
  expect_identical(ans_obtained, ans_expected)
})


## 'forecast_hypers' ----------------------------------------------------------

## TODO - Add a new test when we have a prior where the
## hyper-parameters are forecast (eg a local trend prior)
test_that("'forecast_hypers' works when no hypers are forecast", {
    set.seed(0)
    data <- expand.grid(age = 0:9, time = 2000:2005, sex = c("F", "M"))
    data$deaths <- rpois(n = nrow(data), lambda = 10000)
    formula <- deaths ~ age + sex + time
    mod <- mod_pois(formula = formula,
                    data = data,
                    exposure = 1)
    mod <- fit(mod)
    comp <- components(mod)
    hypers_est <- comp[comp$component == "hyper", ]
    ans_obtained <- forecast_hypers(mod = mod,
                                    hypers_est = hypers_est,
                                    labels_forecast = 2006:2008)
    ans_expected <- NULL
    expect_identical(ans_obtained, ans_expected)
})

test_that("'forecast_hypers' works when no hypers are present", {
    set.seed(0)
    data <- expand.grid(age = 0:9, time = 2000:2005, sex = c("F", "M"))
    data$deaths <- rpois(n = nrow(data), lambda = 10000)
    formula <- deaths ~ 1
    mod <- mod_pois(formula = formula,
                    data = data,
                    exposure = 1)
    mod <- fit(mod)
    comp <- components(mod)
    hypers_est <- comp[comp$component == "hyper", ]
    ans_obtained <- forecast_hypers(mod = mod,
                                    hypers_est = hypers_est,
                                    labels_forecast = 2006:2008)
    ans_expected <- NULL
    expect_identical(ans_obtained, ans_expected)
    hypers_est <- NULL
    ans_obtained <- forecast_hypers(mod = mod,
                                    hypers_est = hypers_est,
                                    labels_forecast = 2006:2008)
    ans_expected <- NULL
    expect_identical(ans_obtained, ans_expected)
})


## 'forecast_rw' --------------------------------------------------------------

test_that("'forecast_rw' works with n_by = 1", {
  set.seed(0)
  rw_est <- rvec::rnorm_rvec(n = 5, n_draw = 10)
  sd <- rvec::runif_rvec(n = 1, n_draw = 10)
  matrix_along_by_est <- matrix(0:4, nr = 5)
  matrix_along_by_forecast <- matrix(0:5, nr = 6)
  set.seed(1)
  ans_obtained <- forecast_rw(rw_est = rw_est,
                              sd = sd,
                              matrix_along_by_est = matrix_along_by_est,
                              matrix_along_by_forecast = matrix_along_by_forecast)
  ans_expected <- rvec::rnorm_rvec(n = 6, n_draw = 10)
  set.seed(1)
  ans_expected[1] <- rvec::rnorm_rvec(n = 1,
                                      mean = rw_est[5],
                                      sd = sd)
  for (i in 2:6)
    ans_expected[i] <- rvec::rnorm_rvec(n = 1,
                                        mean = ans_expected[i-1],
                                        sd = sd)
  expect_equal(ans_obtained, ans_expected)
})


test_that("'forecast_rw' works with  n_by = 2", {
  set.seed(0)
  rw_est <- rvec::rnorm_rvec(n = 10, n_draw = 10)
  sd <- rvec::runif_rvec(n = 1, n_draw = 10)
  matrix_along_by_est <- matrix(0:9, nr = 5)
  matrix_along_by_forecast <- matrix(0:11, nr = 6)
  set.seed(1)
  ans_obtained <- forecast_rw(rw_est = rw_est,
                              sd = sd,
                              matrix_along_by_est = matrix_along_by_est,
                              matrix_along_by_forecast = matrix_along_by_forecast)
  ans_expected <- rvec::rnorm_rvec(n = 12, n_draw = 10)
  set.seed(1)
  ans_expected[1] <- rvec::rnorm_rvec(n = 1,
                                      mean = rw_est[5],
                                      sd = sd)
  for (i in 2:6)
    ans_expected[i] <- rvec::rnorm_rvec(n = 1,
                                        mean = ans_expected[i-1],
                                        sd = sd)
  ans_expected[7] <- rvec::rnorm_rvec(n = 1,
                                      mean = rw_est[10],
                                      sd = sd)
  for (i in 8:12)
    ans_expected[i] <- rvec::rnorm_rvec(n = 1,
                                        mean = ans_expected[i-1],
                                        sd = sd)
  expect_equal(ans_obtained, ans_expected)
})

test_that("'forecast_seasfix' works", {
  set.seed(0)
  seas_est <- rvec::rnorm_rvec(n = 4, n_draw = 10)
  matrix_along_by_est <- matrix(0:9, nr = 5)
  matrix_along_by_forecast <- matrix(0:11, nr = 6)
  set.seed(1)
  ans_obtained <- forecast_seasfix(seas_est,
                                   matrix_along_by_est = matrix_along_by_est,
                                   matrix_along_by_forecast = matrix_along_by_forecast)
  ans_expected <- rep(seas_est[1], 12)
  ans_expected[c(1, 3, 5)] <- seas_est[2]
  ans_expected[c(2, 4, 6)] <- seas_est[1]
  ans_expected[c(7, 9, 11)] <- seas_est[4]
  ans_expected[c(8, 10, 12)] <- seas_est[3]
  expect_equal(ans_obtained, ans_expected)
})

test_that("'forecast_seasvary' works", {
  set.seed(0)
  seas_est <- rvec::rnorm_rvec(n = 10, n_draw = 10)
  sd <- rvec::runif_rvec(n = 1, n_draw = 10)
  matrix_along_by_est <- matrix(0:9, nr = 5)
  matrix_along_by_forecast <- matrix(0:11, nr = 6)
  set.seed(1)
  ans_obtained <- forecast_seasvary(seas_est,
                                sd = sd,
                                matrix_along_by_est = matrix_along_by_est,
                                matrix_along_by_forecast = matrix_along_by_forecast)
  ans_expected <- rvec::rnorm_rvec(n = 12, n_draw = 10)
  set.seed(1)
  ans_expected[1] <- rvec::rnorm_rvec(n = 1,
                                      seas_est[4],
                                      sd = sd)
  ans_expected[2] <- rvec::rnorm_rvec(n = 1,
                                      mean = seas_est[5],
                                      sd = sd)
  for (i in 3:6)
    ans_expected[i] <- rvec::rnorm_rvec(n = 1,
                                        mean = ans_expected[i-2],
                                        sd = sd)
  ans_expected[7] <- rvec::rnorm_rvec(n = 1,
                                      seas_est[9],
                                      sd = sd)
  ans_expected[8] <- rvec::rnorm_rvec(n = 1,
                                      mean = seas_est[10],
                                      sd = sd)
  for (i in 9:12)
    ans_expected[i] <- rvec::rnorm_rvec(n = 1,
                                        mean = ans_expected[i-2],
                                        sd = sd)
  expect_equal(ans_obtained, ans_expected)
})



## 'make_data_forecast' -------------------------------------------------------

test_that("'forecast_components' works", {
  set.seed(0)
  data <- expand.grid(age = 0:9, time = 2000:2005, sex = c("F", "M"))
  data$deaths <- rpois(n = nrow(data), lambda = 100)
  data$exposure <- 100
  data$unused <- 33
  formula <- deaths ~ age * sex + sex * time
  mod <- mod_pois(formula = formula,
                  data = data,
                  exposure = exposure)
  mod <- set_n_draw(mod, n = 10)
  ans <- make_data_forecast(mod = mod,
                            labels_forecast = 2006:2008)
  expect_identical(names(ans), names(data))
  expect_true(all(is.na(ans$deaths)))
  expect_true(all(is.na(ans$exposure)))
  expect_true(all(is.na(ans$unused)))
  expect_setequal(ans$age, data$age)
  expect_setequal(ans$sex, data$sex)
  expect_setequal(ans$time, 2006:2008)
})
