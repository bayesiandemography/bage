
## 'set_disp' -----------------------------------------------------------------

test_that("'set_disp' works with Poisson", {
    data <- expand.grid(age = 0:2, time = 2000:2001, sex = 1:2)
    data$popn <- seq_len(nrow(data))
    data$deaths <- rev(seq_len(nrow(data)))
    formula <- deaths ~ age:sex + time
    mod <- mod_pois(formula = formula,
                    data = data,
                    exposure = popn)
    expect_identical(mod$scale_disp, 1)
    mod <- set_disp(mod, s = 0)
    expect_identical(mod$scale_disp, 0)
    mod <- set_disp(mod, s = 0.5)
    expect_identical(mod$scale_disp, 0.5)
})

test_that("'set_disp' works with normal", {
    data <- expand.grid(age = 0:2, time = 2000:2001, sex = 1:2)
    data$deaths <- rpois(n = nrow(data), lambda = 100)
    formula <- deaths ~ age:sex + time
    mod <- mod_norm(formula = formula,
                    data = data,
                    weights = 1)
    expect_identical(mod$scale_disp, 1)
    mod <- set_disp(mod, s = 0.5)
    expect_identical(mod$scale_disp, 0.5)
    expect_error(set_disp(mod, s = 0))
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
    mod <- set_n_draw(mod, n_draw = 10)
    ans_obtained <- mod$n_draw
    ans_expected <- 10L
    expect_identical(ans_obtained, ans_expected)
})


## 'set_prior' ----------------------------------------------------------------

test_that("'set_prior' works with valid inputs", {
    data <- expand.grid(age = 0:2, time = 2000:2001, sex = 1:2)
    data$popn <- seq_len(nrow(data))
    data$deaths <- rev(seq_len(nrow(data)))
    formula <- deaths ~ age*sex + time
    mod <- mod_pois(formula = formula,
                    data = data,
                    exposure = popn)
    mod_rw <- set_prior(mod, age ~ RW2())
    expect_s3_class(mod_rw$priors[["age"]], "bage_prior_rw2")
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
                 "Can't use `SVD\\(HMD\\)` prior for interaction when age or sex/gender variable not yet identified.")
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
    expect_false(is.null(mod$est))
    mod <- set_prior(mod, time ~ RW())
    expect_true(is.null(mod$est))
    expect_true("est" %in% names(mod))
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
    expect_s3_class(mod$priors[["oldness"]], "bage_prior_rw")
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
    expect_s3_class(mod$priors[["age"]], "bage_prior_rw")
    expect_s3_class(mod$priors[["oldness"]], "bage_prior_norm")
    mod <- set_var_age(mod, name = "oldness")
    expect_identical(mod$var_age, "oldness")
    expect_s3_class(mod$priors[["age"]], "bage_prior_norm")
    expect_s3_class(mod$priors[["oldness"]], "bage_prior_rw")
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
    expect_s3_class(mod$priors[["sexx"]], "bage_prior_norm")
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
    expect_s3_class(mod$priors[["sex"]], "bage_prior_norm")
    mod <- set_var_sexgender(mod, name = "gend")
    expect_identical(mod$var_sexgender, "gend")
    expect_s3_class(mod$priors[["gend"]], "bage_prior_norm")
    expect_s3_class(mod$priors[["sex"]], "bage_prior_norm")
})


## 'set_var_time' --------------------------------------------------------------

test_that("'set_var_time' works with valid inputs", {
    data <- expand.grid(age = 0:2, timex = 2000:2001, sex = 1:2)
    data$popn <- seq_len(nrow(data))
    data$deaths <- rev(seq_len(nrow(data)))
    formula <- deaths ~ age*sex + timex
    mod <- mod_pois(formula = formula,
                    data = data,
                    exposure = popn)
    expect_identical(mod$var_time, NULL)
    mod <- set_var_time(mod, name = "timex")
    expect_identical(mod$var_time, "timex")
    expect_s3_class(mod$priors[["timex"]], "bage_prior_rw")
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
    expect_s3_class(mod$priors[["oldness"]], "bage_prior_rw")
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
    expect_s3_class(mod$priors[["age"]], "bage_prior_rw")
    expect_s3_class(mod$priors[["oldness"]], "bage_prior_norm")
    mod <- set_var_inner(mod, name = "oldness", var = "age")
    expect_identical(mod$var_age, "oldness")
    expect_s3_class(mod$priors[["age"]], "bage_prior_norm")
    expect_s3_class(mod$priors[["oldness"]], "bage_prior_rw")
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
    mod <- mod_pois(formula = formula,
                    data = data,
                    exposure = popn)
    mod_fit_unfit <- unfit(fit(mod))
    expect_identical(mod$est, mod_fit_unfit$est)
    expect_identical(mod$is_fixed, mod_fit_unfit$is_fixed)
    expect_identical(mod$R_prec, mod_fit_unfit$R_prec)
})
