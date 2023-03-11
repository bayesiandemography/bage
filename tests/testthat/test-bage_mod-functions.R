
## 'set_age_var' --------------------------------------------------------------

test_that("'set_age_var' works with valid inputs - no existing age var", {
    data <- expand.grid(oldness = 0:2, time = 2000:2001, sex = 1:2)
    data$popn <- seq_len(nrow(data))
    data$deaths <- rev(seq_len(nrow(data)))
    formula <- deaths ~ oldness*sex + time
    mod <- mod_pois(formula = formula,
                    data = data,
                    exposure = popn)
    expect_identical(mod$age_var, NULL)
    mod <- set_age_var(mod, name = "oldness")
    expect_identical(mod$age_var, "oldness")
    expect_s3_class(mod$priors[["oldness"]], "bage_prior_rw")
})

test_that("'set_age_var' works with valid inputs - has existing age var", {
    data <- expand.grid(age = 0:2, time = 2000:2001, sex = 1:2, oldness = 1:3)
    data$popn <- seq_len(nrow(data))
    data$deaths <- rev(seq_len(nrow(data)))
    formula <- deaths ~ oldness*sex + time + age
    mod <- mod_pois(formula = formula,
                    data = data,
                    exposure = popn)
    expect_identical(mod$age_var, "age")
    expect_s3_class(mod$priors[["age"]], "bage_prior_rw")
    expect_s3_class(mod$priors[["oldness"]], "bage_prior_norm")
    mod <- set_age_var(mod, name = "oldness")
    expect_identical(mod$age_var, "oldness")
    expect_s3_class(mod$priors[["age"]], "bage_prior_norm")
    expect_s3_class(mod$priors[["oldness"]], "bage_prior_rw")
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
    formula <- deaths ~ age:sex + time
    mod <- mod_pois(formula = formula,
                    data = data,
                    exposure = popn)
    mod_rw <- set_prior(mod, age:sex ~ RW())
    expect_s3_class(mod_rw$priors[["age:sex"]], "bage_prior_rw")
    mod_rw <- set_prior(mod, age:sex ~ RW2())
    expect_s3_class(mod_rw$priors[["age:sex"]], "bage_prior_rw2")
})

test_that("'set_prior' throws correct error with invalid response", {
    data <- expand.grid(age = 0:2, time = 2000:2001, sex = 1:2)
    data$popn <- seq_len(nrow(data))
    data$deaths <- rev(seq_len(nrow(data)))
    formula <- deaths ~ age:sex + time
    mod <- mod_pois(formula = formula,
                    data = data,
                    exposure = popn)
    expect_error(set_prior(mod, sex:age ~ RW()),
                 paste("response in prior formula 'sex:age ~ RW\\(\\)' not",
                       "a valid term from model formula 'deaths ~ age:sex \\+ time'",
                       ": valid terms are '\\(Intercept\\)', 'time', 'age:sex'"))
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
                 paste("prior formula 'age:sex ~ Wrong\\(\\)' invalid :",
                       "could not find function \"Wrong\""))
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



