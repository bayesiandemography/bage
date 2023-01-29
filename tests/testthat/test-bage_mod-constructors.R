
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
    ans_noquote <- mod_pois(formula = formula,
                            data = data,
                            exposure = popn)
    ans_withquote <- mod_pois(formula = formula,
                              data = data,
                              exposure = "popn")
    expect_identical(ans_noquote, ans_withquote)
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
    ans_noquote <- mod_binom(formula = formula,
                             data = data,
                             size = popn)
    ans_withquote <- mod_binom(formula = formula,
                               data = data,
                               size = "popn")
    expect_identical(ans_noquote, ans_withquote)
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
    ans_noquote <- mod_norm(formula = formula,
                            data = data,
                            weights = popn)
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


