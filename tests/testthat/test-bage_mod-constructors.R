
## 'mod_pois' -----------------------------------------------------------------

test_that("'mod_pois' works with valid inputs - with exposure", {
    data <- expand.grid(age = 0:2, time = 2000:2001, sex = 1:2)
    data$popn <- seq_len(nrow(data))
    data$deaths <- rev(seq_len(nrow(data)))
    formula <- deaths ~ age:sex + time
    ans_obtained <- mod_pois(formula = formula,
                             data = data,
                             exposure = popn)
    expect_s3_class(ans_obtained, "bage_mod_pois")
    expect_s3_class(ans_obtained, "bage_mod")
    set.seed(0)
    ans_noquote <- mod_pois(formula = formula,
                            data = data,
                            exposure = popn)
    set.seed(0)
    ans_withquote <- mod_pois(formula = formula,
                              data = data,
                              exposure = "popn")
    expect_identical(ans_noquote, ans_withquote)
    set.seed(0)
    ans_squote <- mod_pois(formula = formula,
                           data = data,
                           exposure = 'popn')
    expect_identical(ans_noquote, ans_squote)
})

test_that("'mod_pois' works with valid inputs - no exposure", {
    data <- expand.grid(age = 0:2, time = 2000:2001, sex = 1:2)
    data$popn <- seq_len(nrow(data))
    data$deaths <- rev(seq_len(nrow(data)))
    formula <- deaths ~ age:sex + time
    ans_obtained <- mod_pois(formula = formula,
                             data = data,
                             exposure = 1)
    expect_s3_class(ans_obtained, "bage_mod_pois")
})

test_that("'mod_pois' gives correct error when no exposure specified", {
    data <- expand.grid(age = 0:2, time = 2000:2001, sex = 1:2)
    data$popn <- seq_len(nrow(data))
    data$deaths <- rev(seq_len(nrow(data)))
    formula <- deaths ~ age:sex + time
    expect_error(mod_pois(formula = formula,
                          data = data),
                 "Argument `exposure` is missing, with no default.")
})



## 'mod_binom' ----------------------------------------------------------------

test_that("'mod_binom' works with valid inputs", {
    data <- expand.grid(age = 0:2, time = 2000:2001, sex = 1:2)
    data$popn <- seq_len(nrow(data))
    data$deaths <- round(data$popn / 3)
    formula <- deaths ~ age:sex + time
    ans_obtained <- mod_binom(formula = formula,
                              data = data,
                              size = popn)
    expect_s3_class(ans_obtained, "bage_mod_binom")
    expect_s3_class(ans_obtained, "bage_mod")
    set.seed(0)
    ans_noquote <- mod_binom(formula = formula,
                             data = data,
                             size = popn)
    set.seed(0)
    ans_withquote <- mod_binom(formula = formula,
                               data = data,
                               size = "popn")
    expect_identical(ans_noquote, ans_withquote)
})

test_that("'mod_binom' gives correct error when no size supplied", {
    data <- expand.grid(age = 0:2, time = 2000:2001, sex = 1:2)
    data$popn <- seq_len(nrow(data))
    data$deaths <- round(data$popn / 3)
    formula <- deaths ~ age:sex + time
    expect_error(mod_binom(formula = formula,
                           data = data),
                 "Argument `size` is missing, with no default.")
})



## 'mod_norm' -----------------------------------------------------------------

test_that("'mod_norm' works with valid inputs - with weights", {
    data <- expand.grid(age = 0:2, time = 2000:2001, sex = 1:2)
    data$popn <- seq_len(nrow(data))
    data$income <- rnorm(nrow(data))
    formula <- income ~ age:sex + time
    ans_obtained <- mod_norm(formula = formula,
                             data = data,
                             weights = popn)
    expect_s3_class(ans_obtained, "bage_mod_norm")
    expect_s3_class(ans_obtained, "bage_mod")
    set.seed(0)
    ans_noquote <- mod_norm(formula = formula,
                            data = data,
                            weights = popn)
    set.seed(0)
    ans_withquote <- mod_norm(formula = formula,
                              data = data,
                              weights = "popn")
    expect_identical(ans_noquote, ans_withquote)
})

test_that("'mod_norm' works with valid inputs - no weights", {
    data <- expand.grid(age = 0:2, time = 2000:2001, sex = 1:2)
    data$popn <- seq_len(nrow(data))
    data$income <- rnorm(nrow(data))
    formula <- income ~ age:sex + time
    ans_obtained <- mod_norm(formula = formula,
                             data = data,
                             weights = 1)
    expect_s3_class(ans_obtained, "bage_mod_norm")
})

test_that("'mod_norm' works with no values for outcome variable", {
    data <- expand.grid(age = 0:2, time = 2000:2001, sex = 1:2)
    data$popn <- seq_len(nrow(data))
    data$income <- NA
    formula <- income ~ age:sex + time
    ans_obtained <- mod_norm(formula = formula,
                             data = data,
                             weights = 1)
    expect_s3_class(ans_obtained, "bage_mod_norm")
    expect_identical(ans_obtained$outcome_mean, 0)
    expect_identical(ans_obtained$outcome_sd, 1)
})

test_that("'mod_norm' gives correct error when no weights supplied", {
    data <- expand.grid(age = 0:2, time = 2000:2001, sex = 1:2)
    data$popn <- seq_len(nrow(data))
    data$income <- rnorm(nrow(data))
    formula <- income ~ age:sex + time
    expect_error(mod_norm(formula = formula,
                          data = data),
                 "Argument `weights` is missing, with no default.")
})


## 'mod_helper' ---------------------------------------------------------------

test_that("'mod_helper' works with valid inputs", {
  data <- expand.grid(age = 0:2, time = 2000:2001, sex = 1:2)
  data$popn <- seq_len(nrow(data))
  data$income <- rnorm(nrow(data))
  formula <- income ~ age:sex + time
  ans <- mod_helper(formula = formula,
                    data = data,
                    n_draw = 5L)
  expect_identical(ans$n_draw, 5L)
  expect_identical(length(names(ans)), length(ans))
  expect_false(any(duplicated(names(ans))))
})
