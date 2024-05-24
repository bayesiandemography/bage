
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
