
## 'set_datamod_outcome_rr3' --------------------------------------------------

test_that("'set_datamod_outcome_rr3' works with Poisson", {
  data <- expand.grid(age = 0:2, time = 2000:2001, sex = 1:2)
  data$popn <- seq_len(nrow(data))
  data$deaths <- sample(c(0, 3, 9, 12), size = nrow(data), replace = TRUE)
  formula <- deaths ~ age:sex + time
  mod <- mod_pois(formula = formula,
                  data = data,
                  exposure = popn)
  ans <- set_datamod_outcome_rr3(mod)
  expect_identical(ans$datamod_outcome, new_bage_datamod_outcome_rr3())
})

test_that("'set_datamod_outcome_rr3' works with Poisson - outcome has NA", {
  data <- expand.grid(age = 0:2, time = 2000:2001, sex = 1:2)
  data$popn <- seq_len(nrow(data))
  data$deaths <- sample(c(0, 3, 9, 12), size = nrow(data), replace = TRUE)
  data$deaths[1] <- NA
  formula <- deaths ~ age:sex + time
  mod <- mod_pois(formula = formula,
                  data = data,
                  exposure = popn)
  ans <- set_datamod_outcome_rr3(mod)
  expect_identical(ans$datamod_outcome, new_bage_datamod_outcome_rr3())
})

test_that("'set_datamod_outcome_rr3' works with binomial", {
  data <- expand.grid(age = 0:2, time = 2000:2001, sex = 1:2)
  data$popn <- seq_len(nrow(data)) + 12
  data$deaths <- sample(c(0, 3, 9, 12), size = nrow(data), replace = TRUE)
  formula <- deaths ~ age:sex + time
  mod <- mod_binom(formula = formula,
                   data = data,
                   size = popn)
  ans <- set_datamod_outcome_rr3(mod)
  expect_identical(ans$datamod_outcome, new_bage_datamod_outcome_rr3())
})

test_that("'set_datamod_outcome_rr3' throws correct error when used with non-Poisson, non-binom", {
  data <- expand.grid(age = 0:2, time = 2000:2001, sex = 1:2)
  data$popn <- seq_len(nrow(data))
  data$deaths <- sample(c(0, 3, 9, 12), size = nrow(data), replace = TRUE)
  formula <- deaths ~ age:sex + time
  mod <- mod_norm(formula = formula,
                  data = data,
                  weights = 1)
  expect_error(set_datamod_outcome_rr3(mod),
               "Outcome has \"norm\" distribution.")
})

test_that("'set_datamod_outcome_rr3' throws correct error when used with non-Poisson, non-binom", {
  data <- expand.grid(age = 0:2, time = 2000:2001, sex = 1:2)
  data$popn <- seq_len(nrow(data))
  data$deaths <- 1
  formula <- deaths ~ age:sex + time
  mod <- mod_pois(formula = formula,
                  data = data,
                  exposure = 1)
  expect_error(set_datamod_outcome_rr3(mod),
               "Outcome variable has values not divisible by 3.")
  data$deaths <- c(1, rep(3, times = nrow(data) - 1))
  formula <- deaths ~ age:sex + time
  mod <- mod_pois(formula = formula,
                  data = data,
                  exposure = 1)
  expect_error(set_datamod_outcome_rr3(mod),
               "Outcome variable has value not divisible by 3.")
})


## 'set_disp' -----------------------------------------------------------------

test_that("'set_disp' works with Poisson", {
    data <- expand.grid(age = 0:2, time = 2000:2001, sex = 1:2)
    data$popn <- seq_len(nrow(data))
    data$deaths <- rev(seq_len(nrow(data)))
    formula <- deaths ~ age:sex + time
    mod <- mod_pois(formula = formula,
                    data = data,
                    exposure = popn)
    expect_identical(mod$mean_disp, 1)
    mod <- set_disp(mod, mean = 0)
    expect_identical(mod$mean_disp, 0)
    mod <- set_disp(mod, mean = 0.5)
    expect_identical(mod$mean_disp, 0.5)
})

test_that("'set_disp' works with normal", {
    data <- expand.grid(age = 0:2, time = 2000:2001, sex = 1:2)
    data$deaths <- rpois(n = nrow(data), lambda = 100)
    formula <- deaths ~ age:sex + time
    mod <- mod_norm(formula = formula,
                    data = data,
                    weights = 1)
    expect_identical(mod$mean_disp, 1)
    mod <- set_disp(mod, mean = 0.5)
    expect_identical(mod$mean_disp, 0.5)
    expect_error(set_disp(mod, mean = 0))
})


## 'set_n_draw' ---------------------------------------------------------------

test_that("'set_n_draw' works with valid inputs", {
  data <- expand.grid(age = 0:2, time = 2000:2001, sex = 1:2)
  data$popn <- seq_len(nrow(data))
  data$deaths <- rev(seq_len(nrow(data)))
  formula <- deaths ~ age:sex + time
  mod <- mod_pois(formula = formula,
                  data = data,
                  exposure = popn)
  mod <- set_n_draw(mod, n_draw = 10L)
  expect_identical(mod$n_draw, 10L)
  mod <- fit(mod)
  mod <- set_n_draw(mod, n_draw = 5)
  expect_identical(mod$n_draw, 5L)
  expect_true(is_fitted(mod))
  expect_message(set_n_draw(mod, n_draw = 10),
                 "New value")
})


## 'set_prior' ----------------------------------------------------------------

test_that("'set_prior' works with valid inputs", {
    data <- expand.grid(age = 0:3, time = 2000:2004, sex = 1:2)
    data$popn <- seq_len(nrow(data))
    data$deaths <- rev(seq_len(nrow(data)))
    formula <- deaths ~ age*sex + time
    mod <- mod_pois(formula = formula,
                    data = data,
                    exposure = popn)
    mod_rw <- set_prior(mod, age ~ RW2())
    expect_s3_class(mod_rw$priors[["age"]], "bage_prior_rw2random")
    mod_rw <- set_prior(mod, age:sex ~ NFix())
    expect_s3_class(mod_rw$priors[["age:sex"]], "bage_prior_normfixed")
})

test_that("'set_prior' throws correct error with invalid response", {
    data <- expand.grid(age = 0:2, time = 2000:2001, sex = 1:2)
    data$popn <- seq_len(nrow(data))
    data$deaths <- rev(seq_len(nrow(data)))
    formula <- deaths ~ age + sex + time
    mod <- mod_pois(formula = formula,
                    data = data,
                    exposure = popn)
    expect_error(set_prior(mod, wrong ~ RW()),
                 "Problem with prior formula `wrong ~ RW\\(\\)`.")
})

test_that("'set_prior' throws correct error with invalid prior function", {
    data <- expand.grid(age = 0:2, time = 2000:2001, sex = 1:2)
    data$popn <- seq_len(nrow(data))
    data$deaths <- rev(seq_len(nrow(data)))
    formula <- deaths ~ age:sex + time
    mod <- mod_pois(formula = formula,
                    data = data,
                    exposure = popn)
    expect_error(set_prior(mod, age:sex ~ Wrong()),
                 "Problem with prior formula `age:sex ~ Wrong\\(\\)`")
})

test_that("'set_prior' throws correct error with SVD prior but var_age not indentified", {
    data <- expand.grid(v = 0:2, time = 2000:2001, sex = 1:2)
    data$popn <- seq_len(nrow(data))
    data$deaths <- rev(seq_len(nrow(data)))
    formula <- deaths ~ v*sex + time
    mod <- mod_pois(formula = formula,
                    data = data,
                    exposure = popn)
    expect_error(set_prior(mod, v:sex ~ SVD(HMD)),
                 "Problem with `SVD\\(\\)` prior for term `v:sex`.")
})

test_that("'set_prior' unfits a fitted model", {
    data <- expand.grid(age = 0:2, time = 2000:2005, sex = 1:2)
    data$popn <- seq_len(nrow(data))
    data$deaths <- rev(seq_len(nrow(data)))
    formula <- deaths ~ age:sex + time
    mod <- mod_pois(formula = formula,
                    data = data,
                    exposure = popn)
    mod <- fit(mod)
    mod <- set_prior(mod, time ~ RW())
    expect_false(is_fitted(mod))
})

test_that("'set_prior' works with when order of components of interaction changed", {
    data <- expand.grid(age = 0:2, time = 2000:2001, sex = 1:2)
    data$popn <- seq_len(nrow(data))
    data$deaths <- rev(seq_len(nrow(data)))
    formula <- deaths ~ age + sex:age + time
    mod <- mod_pois(formula = formula,
                    data = data,
                    exposure = popn)
    mod <- set_prior(mod, sex:age ~ NFix())
    expect_s3_class(mod$priors[["age:sex"]], "bage_prior_normfixed")
})


## 'set_var_age' --------------------------------------------------------------

test_that("'set_var_age' works with valid inputs - no existing age var", {
    data <- expand.grid(oldness = 0:2, time = 2000:2001, sex = 1:2)
    data$popn <- seq_len(nrow(data))
    data$deaths <- rev(seq_len(nrow(data)))
    formula <- deaths ~ oldness*sex + time
    mod <- mod_pois(formula = formula,
                    data = data,
                    exposure = popn)
    expect_identical(mod$var_age, NULL)
    mod <- set_var_age(mod, name = "oldness")
    expect_identical(mod$var_age, "oldness")
    expect_s3_class(mod$priors[["oldness"]], "bage_prior_rwrandom")
})

test_that("'set_var_age' works with valid inputs - has existing age var", {
    data <- expand.grid(age = 0:2, time = 2000:2001, sex = 1:2, oldness = 1:3)
    data$popn <- seq_len(nrow(data))
    data$deaths <- rev(seq_len(nrow(data)))
    formula <- deaths ~ oldness*sex + time + age
    mod <- mod_pois(formula = formula,
                    data = data,
                    exposure = popn)
    expect_identical(mod$var_age, "age")
    expect_s3_class(mod$priors[["age"]], "bage_prior_rwrandom")
    expect_s3_class(mod$priors[["oldness"]], "bage_prior_norm")
    mod <- set_var_age(mod, name = "oldness")
    expect_identical(mod$var_age, "oldness")
    expect_s3_class(mod$priors[["age"]], "bage_prior_norm")
    expect_s3_class(mod$priors[["oldness"]], "bage_prior_rwrandom")
})


## 'set_var_sexgender' --------------------------------------------------------------

test_that("'set_var_sexgender' works with valid inputs - no existing sexgender var", {
    data <- expand.grid(age = 0:2, time = 2000:2001, sexx = 1:2)
    data$popn <- seq_len(nrow(data))
    data$deaths <- rev(seq_len(nrow(data)))
    formula <- deaths ~ age*sexx + time
    mod <- mod_pois(formula = formula,
                    data = data,
                    exposure = popn)
    expect_identical(mod$var_sexgender, NULL)
    mod <- set_var_sexgender(mod, name = "sexx")
    expect_identical(mod$var_sexgender, "sexx")
    expect_s3_class(mod$priors[["sexx"]], "bage_prior_normfixed")
})

test_that("'set_var_sexgender' works with valid inputs - has existing sexgender var", {
    data <- expand.grid(age = 0:2, time = 2000:2001, sex = 1:2, gend = 1:3)
    data$popn <- seq_len(nrow(data))
    data$deaths <- rev(seq_len(nrow(data)))
    formula <- deaths ~ gend*sex + time + age
    mod <- mod_pois(formula = formula,
                    data = data,
                    exposure = popn)
    expect_identical(mod$var_sexgender, "sex")
    expect_s3_class(mod$priors[["sex"]], "bage_prior_normfixed")
    mod <- set_var_sexgender(mod, name = "gend")
    expect_identical(mod$var_sexgender, "gend")
    expect_s3_class(mod$priors[["gend"]], "bage_prior_norm")
    expect_s3_class(mod$priors[["sex"]], "bage_prior_normfixed")
})


## 'set_var_time' --------------------------------------------------------------

test_that("'set_var_time' works with valid inputs", {
    data <- expand.grid(age = 0:2, timex = 2000:2011, sex = 1:2)
    data$popn <- seq_len(nrow(data))
    data$deaths <- rev(seq_len(nrow(data)))
    formula <- deaths ~ age*sex + timex
    mod <- mod_pois(formula = formula,
                    data = data,
                    exposure = popn)
    expect_identical(mod$var_time, NULL)
    mod <- set_var_time(mod, name = "timex")
    expect_identical(mod$var_time, "timex")
    expect_s3_class(mod$priors[["timex"]], "bage_prior_rwrandom")
})


## 'set_var_inner' ------------------------------------------------------------

test_that("'set_var_inner' works with valid inputs - no existing var", {
    data <- expand.grid(oldness = 0:2, time = 2000:2001, sex = 1:2)
    data$popn <- seq_len(nrow(data))
    data$deaths <- rev(seq_len(nrow(data)))
    formula <- deaths ~ oldness*sex + time
    mod <- mod_pois(formula = formula,
                    data = data,
                    exposure = popn)
    expect_identical(mod$var_age, NULL)
    mod <- set_var_inner(mod, name = "oldness", var = "age")
    expect_identical(mod$var_age, "oldness")
    expect_s3_class(mod$priors[["oldness"]], "bage_prior_rwrandom")
})

test_that("'set_var_inner' works with valid inputs - has existing var", {
    data <- expand.grid(age = 0:2, time = 2000:2001, sex = 1:2, oldness = 1:3)
    data$popn <- seq_len(nrow(data))
    data$deaths <- rev(seq_len(nrow(data)))
    formula <- deaths ~ oldness*sex + time + age
    mod <- mod_pois(formula = formula,
                    data = data,
                    exposure = popn)
    expect_identical(mod$var_age, "age")
    expect_s3_class(mod$priors[["age"]], "bage_prior_rwrandom")
    expect_s3_class(mod$priors[["oldness"]], "bage_prior_norm")
    mod <- set_var_inner(mod, name = "oldness", var = "age")
    expect_identical(mod$var_age, "oldness")
    expect_s3_class(mod$priors[["age"]], "bage_prior_norm")
    expect_s3_class(mod$priors[["oldness"]], "bage_prior_rwrandom")
})

test_that("'set_var_inner' gives correct errors with invalid inputs", {
    data <- expand.grid(age = 0:2, time = 2000:2001, sex = 1:2)
    data$popn <- seq_len(nrow(data))
    data$deaths <- rev(seq_len(nrow(data)))
    formula <- deaths ~ age*sex + time
    mod <- mod_pois(formula = formula,
                    data = data,
                    exposure = popn)
    expect_error(set_var_inner(mod = mod,
                               name = "age",
                               var = "time"),
                 "Variables for time and age have the same name.")
})


## 'unfit' --------------------------------------------------------------------

test_that("'set_n_draw' works with valid inputs", {
    data <- expand.grid(age = 0:2, time = 2000:2001, sex = 1:2)
    data$popn <- seq_len(nrow(data))
    data$deaths <- rev(seq_len(nrow(data)))
    formula <- deaths ~ age:sex + time
    mod_unfit <- mod_pois(formula = formula,
                    data = data,
                    exposure = popn)
    mod_fit <- fit(mod_unfit)
    mod_fit_unfit <- unfit(mod_unfit)
    nms <- c("draws_effectfree",
             "draws_hyper",
             "draws_hyperrandfree",
             "draws_disp",
             "point_effectfree",
             "point_hyper",
             "point_hyperrandfree",
             "point_disp",
             "computations",
             "oldpar")
    expect_true(all(nms %in% names(mod_unfit)))
    expect_true(all(nms %in% names(mod_fit)))
    expect_true(all(nms %in% names(mod_fit_unfit)))
    for (nm in nms)
      expect_false(isTRUE(all.equal(mod_fit[[nm]], mod_unfit[[nm]])))
    for (nm in nms)
      expect_true(isTRUE(all.equal(mod_fit_unfit[[nm]], mod_unfit[[nm]])))
})
