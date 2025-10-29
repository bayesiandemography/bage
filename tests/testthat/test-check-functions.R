
## 'check_along_is_time' ------------------------------------------------------

test_that("'check_along_is_time' returns true with valid model object", {
    set.seed(0)
    data <- expand.grid(age = 0:9, sex = c("F", "M"), time = 2001:2005)
    data$popn <- rpois(n = nrow(data), lambda = 100)
    data$deaths <- rpois(n = nrow(data), lambda = 10)
    formula <- deaths ~ (age + sex + time)^2
    mod <- mod_pois(formula = formula,
                    data = data,
                    exposure = popn)
    expect_true(check_along_is_time(mod))
    mod <- set_prior(mod, age:time ~ N())
    expect_true(check_along_is_time(mod))    
})

test_that("'check_along_is_time' returns true when there is no time variable", {
    set.seed(0)
    data <- expand.grid(age = 0:9, sex = c("F", "M"))
    data$popn <- rpois(n = nrow(data), lambda = 100)
    data$deaths <- rpois(n = nrow(data), lambda = 10)
    formula <- deaths ~ (age + sex)^2
    mod <- mod_pois(formula = formula,
                    data = data,
                    exposure = popn)
    mod <- set_prior(mod, age:sex ~ RW(along = "age"))
    expect_true(check_along_is_time(mod))
})

test_that("'check_bage_mod' returns expected error message with invalid model object", {
    set.seed(0)
    data <- expand.grid(age = 0:9, sex = c("F", "M"), time = 2001:2005)
    data$popn <- rpois(n = nrow(data), lambda = 100)
    data$deaths <- rpois(n = nrow(data), lambda = 10)
    formula <- deaths ~ (age + sex + time)^2
    mod <- mod_pois(formula = formula,
                    data = data,
                    exposure = popn)
    mod <- set_prior(mod, age:time ~ RW(along = "age", s = 0.5))
    expect_error(check_along_is_time(mod),
                 "Unable to forecast term \"age:time\".")
})


## 'check_bage_mod' -----------------------------------------------------------

test_that("'check_bage_mod' returns true with valid model object", {
    set.seed(0)
    data <- expand.grid(age = 0:9, sex = c("F", "M"))
    data$popn <- rpois(n = nrow(data), lambda = 100)
    data$deaths <- rpois(n = nrow(data), lambda = 10)
    formula <- deaths ~ age + sex
    mod <- mod_pois(formula = formula,
                    data = data,
                    exposure = popn)
    expect_true(check_bage_mod(mod))
})

test_that("'check_bage_mod' returns expected error message with invalid model object", {
    expect_error(check_bage_mod(x = NULL, nm_x = "model"),
                 "`model` does not have class")
})


## 'check_con_n_by' ------------------------------------------------------

test_that("'check_con_n_by' returns TRUE with valid inputs", {
  expect_true(check_con_n_by(con = "by", n_by = 2L, nm = "x"))
  expect_true(check_con_n_by(con = "none", n_by = 2L, nm = "x"))
  expect_true(check_con_n_by(con = "none", n_by = 1L, nm = "x"))
})

test_that("'check_con_n_by' throws correct error with invalid inputs", {
  expect_error(check_con_n_by(con = "by", n_by = 1L, nm = "x"),
               "`con` is \"by\" but `x` term is a main effect.")                                   
})


## 'check_covariates_formula' -------------------------------------------------

test_that("'check_covariates_formula' works with valid inputs", {
    set.seed(0)
    data <- expand.grid(age = 0:9,
                        region = c("a", "b"),
                        sex = c("F", "M"))
    data$popn <- rpois(n = nrow(data), lambda = 100)
    data$deaths <- rpois(n = nrow(data), lambda = 10)
    data$income <- runif(n = nrow(data))
    mod <- mod_pois(formula = deaths ~ age * sex,
                    data = data,
                    exposure = popn)
    formula <- ~ income * region
    expect_true(check_covariates_formula(formula = formula, mod = mod))
})

test_that("'check_covariates_formula' throws the correct error when 'formula' is not a formula", {
    set.seed(0)
    data <- expand.grid(age = 0:9,
                        region = c("a", "b"),
                        sex = c("F", "M"))
    data$popn <- rpois(n = nrow(data), lambda = 100)
    data$deaths <- rpois(n = nrow(data), lambda = 10)
    data$income <- runif(n = nrow(data))
    mod <- mod_pois(formula = deaths ~ age * sex,
                    data = data,
                    exposure = popn)
    formula <- NULL
    expect_error(check_covariates_formula(formula = formula, mod = mod),
                 "`formula` is not a formula.")
})

test_that("'check_covariates_formula' throws the correct error when 'formula' includes a response", {
    set.seed(0)
    data <- expand.grid(age = 0:9,
                        region = c("a", "b"),
                        sex = c("F", "M"))
    data$popn <- rpois(n = nrow(data), lambda = 100)
    data$deaths <- rpois(n = nrow(data), lambda = 10)
    data$income <- runif(n = nrow(data))
    mod <- mod_pois(formula = deaths ~ age * sex,
                    data = data,
                    exposure = popn)
    formula <- deaths ~ region * income
    expect_error(check_covariates_formula(formula = formula, mod = mod),
                 "`formula` includes a response variable.")
})

test_that("'check_covariates_formula' throws the correct error when 'formula' includes response from main model", {
    set.seed(0)
    data <- expand.grid(age = 0:9,
                        region = c("a", "b"),
                        sex = c("F", "M"))
    data$popn <- rpois(n = nrow(data), lambda = 100)
    data$deaths <- rpois(n = nrow(data), lambda = 10)
    data$income <- runif(n = nrow(data))
    mod <- mod_pois(formula = deaths ~ age * sex,
                    data = data,
                    exposure = popn)
    formula <- ~ region * income + deaths
    expect_error(check_covariates_formula(formula = formula, mod = mod),
                 "`formula` includes response from `mod`.")
})

test_that("'check_covariates_formula' throws the correct error when 'formula' includes variable from main model", {
    set.seed(0)
    data <- expand.grid(age = 0:9,
                        region = c("a", "b"),
                        sex = c("F", "M"))
    data$popn <- rpois(n = nrow(data), lambda = 100)
    data$deaths <- rpois(n = nrow(data), lambda = 10)
    data$income <- runif(n = nrow(data))
    mod <- mod_pois(formula = deaths ~ age * sex,
                    data = data,
                    exposure = popn)
    formula <- ~ region * income + age
    expect_error(check_covariates_formula(formula = formula, mod = mod),
                 "`formula` includes variable from `mod`.")
    formula <- ~ region * income + age:sex
    expect_error(check_covariates_formula(formula = formula, mod = mod),
                 "`formula` includes variables from `mod`.")
})

test_that("'check_covariates_formula' throws the correct error when 'formula' includes variable from main model", {
    set.seed(0)
    data <- expand.grid(age = 0:9,
                        region = c("a", "b"),
                        sex = c("F", "M"))
    data$popn <- rpois(n = nrow(data), lambda = 100)
    data$deaths <- rpois(n = nrow(data), lambda = 10)
    data$income <- runif(n = nrow(data))
    mod <- mod_pois(formula = deaths ~ age * sex,
                    data = data,
                    exposure = popn)
    formula <- ~ region * income + age
    expect_error(check_covariates_formula(formula = formula, mod = mod),
                 "`formula` includes variable from `mod`.")
    formula <- ~ region * income + age:sex
    expect_error(check_covariates_formula(formula = formula, mod = mod),
                 "`formula` includes variables from `mod`.")
})

test_that("'check_covariates_formula' throws the correct error when 'formula' includes offset from main model", {
  set.seed(0)
  data <- expand.grid(age = 0:9,
                      region = c("a", "b"),
                      sex = c("F", "M"))
  data$popn <- rpois(n = nrow(data), lambda = 100)
  data$deaths <- rpois(n = nrow(data), lambda = 10)
  data$income <- runif(n = nrow(data))
  mod <- mod_pois(formula = deaths ~ age * sex,
                  data = data,
                  exposure = popn)
  formula <- ~ region * income + popn
  expect_error(check_covariates_formula(formula = formula, mod = mod),
               "`formula` includes exposure from `mod`.")
})

test_that("'check_covariates_formula' throws the correct error when 'formula' includes variables not in data", {
    set.seed(0)
    data <- expand.grid(age = 0:9,
                        region = c("a", "b"),
                        sex = c("F", "M"))
    data$popn <- rpois(n = nrow(data), lambda = 100)
    data$deaths <- rpois(n = nrow(data), lambda = 10)
    data$income <- runif(n = nrow(data))
    mod <- mod_pois(formula = deaths ~ age * sex,
                    data = data,
                    exposure = popn)
    formula <- ~ region * income + wrong
    expect_error(check_covariates_formula(formula = formula, mod = mod),
                 "variable `wrong` from `formula` not found in data.")
})



## 'check_datamod_by_val' -----------------------------------------------------

test_that("'check_datamod_by_val' returns TRUE with valid inputs", {
  set.seed(0)
  by_val <- expand.grid(age = 0:2, sex = c("f", "m"))
  data <- expand.grid(reg = 1:2, age = 0:1, sex = c("f", "m"))
  expect_true(check_datamod_by_val(by_val = by_val,
                                   data = data,
                                   nm_val = "prob",
                                   nm_data = "data"))
})

test_that("'check_datamod_by_val' throws correct error when by_val has extra variable", {
  set.seed(0)
  by_val <- expand.grid(age = 0:2, sex = c("f", "m"), wrong = 1:2)
  data <- expand.grid(reg = 1:2, age = 0:1, sex = c("f", "m"))
  expect_error(check_datamod_by_val(by_val = by_val,
                                    data = data,
                                    nm_val = "prob",
                                    nm_data = "df"),
               "Variable `wrong` from `prob` not found in `df`.")
})

test_that("'check_datamod_by_val' throws correct error when by_val missing combination of by variables", {
  set.seed(0)
  by_val <- expand.grid(age = 0:2, sex = c("f", "m"))
  data <- expand.grid(reg = 1:2, age = 0:3, sex = c("f", "m"))
  expect_error(check_datamod_by_val(by_val = by_val,
                                    data = data,
                                    nm_val = "prob",
                                    nm_data = "df"),
               "`prob` does not include all combinations of 'by' variables.")
})

test_that("'check_datamod_by_val' throws correct error when by_val missing single by variables", {
  set.seed(0)
  by_val <- expand.grid(age = 0:2)
  data <- expand.grid(reg = 1:2, age = 0:3, sex = c("f", "m"))
  expect_error(check_datamod_by_val(by_val = by_val,
                                    data = data,
                                    nm_val = "prob",
                                    nm_data = "df"),
               "`prob` does not include all levels of 'by' variable.")
})


## 'check_datamod_val' --------------------------------------------------------

test_that("'check_datamod_val' returns TRUE with valid inputs", {
  set.seed(0)
  x <- expand.grid(age = 0:2, sex = c("f", "m"))
  x$mean <- runif(6)
  x$disp <- runif(6)
  expect_true(check_datamod_val(x = x,
                                nm_x = "prob",
                                measure_vars = c("mean", "disp")))
})

test_that("'check_datamod_val' raises correct error wtih duplicated variables", {
  set.seed(0)
  x <- expand.grid(age = 0:2, sex = c("f", "m"))
  x$mean <- runif(6)
  x$disp <- runif(6)
  names(x)[2] <- "age"
  expect_error(check_datamod_val(x = x,
                                 nm_x = "prob",
                                 measure_vars = c("mean", "disp")),
               "`prob` has more than one variable called \"age\".")
})

test_that("'check_datamod_val' raises correct error missing measure var", {
  set.seed(0)
  x <- expand.grid(age = 0:2, sex = c("f", "m"))
  x$mean <- runif(6)
  expect_error(check_datamod_val(x = x,
                                 nm_x = "prob",
                                 measure_vars = c("mean", "disp")),
               "`prob` does not have a variable called \"disp\".")
})

test_that("'check_datamod_val' raises correct error with non-numeric measure var", {
  set.seed(0)
  x <- expand.grid(age = 0:2, sex = c("f", "m"))
  x$mean <- runif(6)
  x$disp <- "a"
  expect_error(check_datamod_val(x = x,
                                 nm_x = "prob",
                                 measure_vars = c("mean", "disp")),
               "Variable `disp` in `prob` is non-numeric.")
})

test_that("'check_datamod_val' raises correct error with NA in measure var", {
  set.seed(0)
  x <- expand.grid(age = 0:2, sex = c("f", "m"))
  x$mean <- runif(6)
  x$disp <- c(runif(5), NA)
  expect_error(check_datamod_val(x = x,
                                 nm_x = "prob",
                                 measure_vars = c("mean", "disp")),
               "Variable `disp` in `prob` has NA.")
  x$disp <- c(NA, runif(4), NA)
  expect_error(check_datamod_val(x = x,
                                 nm_x = "prob",
                                 measure_vars = c("mean", "disp")),
               "Variable `disp` in `prob` has NAs.")
})

test_that("'check_datamod_val' raises correct error with Inf in measure var", {
  set.seed(0)
  x <- expand.grid(age = 0:2, sex = c("f", "m"))
  x$mean <- runif(6)
  x$disp <- c(runif(5), Inf)
  expect_error(check_datamod_val(x = x,
                                 nm_x = "prob",
                                 measure_vars = c("mean", "disp")),
               "Variable `disp` in `prob` has non-finite value.")
  x$disp <- c(-Inf, runif(4), Inf)
  expect_error(check_datamod_val(x = x,
                                 nm_x = "prob",
                                 measure_vars = c("mean", "disp")),
               "Variable `disp` in `prob` has non-finite values.")
})

test_that("'check_datamod_val' raises correct error with no by variables", {
  set.seed(0)
  x <- data.frame(mean = runif(6), disp = runif(6))
  expect_error(check_datamod_val(x = x,
                                 nm_x = "prob",
                                 measure_vars = c("mean", "disp")),
               "`prob` has more than one row, but does not have any 'by' variables.")
})

test_that("'check_datamod_val' raises correct error with duplicated by variables - two variables", {
  set.seed(0)
  x <- expand.grid(age = 0:2, sex = c("f", "m"))
  x <- rbind(x, data.frame(age = 0, sex = "f"))
  x$mean <- runif(7)
  x$disp <- runif(7)
  expect_error(check_datamod_val(x = x,
                                 nm_x = "prob",
                                 measure_vars = c("mean", "disp")),
               "`prob` has duplicated combinations of 'by' variables.")
})

test_that("'check_datamod_val' raises correct error with duplicated by variables - one variable", {
  set.seed(0)
  x <- data.frame(age = c(0:2, 0))
  x$mean <- runif(4)
  x$disp <- runif(4)
  expect_error(check_datamod_val(x = x,
                                 nm_x = "prob",
                                 measure_vars = c("mean", "disp")),
               "`prob` has duplicated levels for 'by' variable.")
})


## 'check_est' ----------------------------------------------------------------

test_that("'check_est' returns TRUE with valid inputs", {
  est <- list(effectfree = c(a = 1, a = 2, b = 3, c = 4, c = 5),
              hyper = c(a = 1),
              hyperrandfree = double(),
              log_disp = c(disp = 3))
  expect_true(check_est(est))
})

test_that("'check_est' returns correct error message with one NA", {
  est <- list(effectfree = c(a = 1, a = 2, b = 3, c = 4, c = NA),
              hyper = c(a = 1),
              hyperrandfree = double(),
              log_disp = c(disp = 3))
  expect_error(check_est(est),
               "Problem deriving posterior distribution.")
})

test_that("'check_est' returns correct error message with multiple NAs", {
  est <- list(effectfree = c(a = 1, a = 2, b = NA, c = 4, c = NA),
              hyper = c(a = 1),
              hyperrandfree = double(),
              log_disp = c(disp = NA))
  expect_error(check_est(est),
               "Problem deriving posterior distribution.")
})


## 'check_flag' ---------------------------------------------------------------

test_that("'check_flag' returns TRUE with valid inputs", {
    x <- TRUE
    expect_true(check_flag(x, nm_x = "x"))
    x <- FALSE
    expect_true(check_flag(x))
})

test_that("'check_flag' throws expected error non-length-1", {
    y <- logical()
    expect_error(check_flag(y, nm_x = "z"),
                 "`z` does not have length 1")
    z <- c(TRUE, TRUE)
    expect_error(check_flag(z, nm_x = "z"),
                 "`z` does not have length 1")
})

test_that("'check_flag' throws expected error non-logical", {
    x <- "hello"
    expect_error(check_flag(x, nm_x = "x"),
                 "`x` does not have class <logical>")
})

test_that("'check_flag' throws expected error NA", {
    x <- NA
    expect_error(check_flag(x, nm_x = "xx"),
                 "`xx` is NA")
})


## 'check_format_prior_formula' -----------------------------------------------

test_that("'check_format_prior_formula' returns TRUE with valid inputs", {
    expect_true(check_format_prior_formula(age ~ N()))
    expect_true(check_format_prior_formula(time:sex~RW()))
})

test_that("'check_format_prior_formula' returns correct error with invalid inputs", {
    expect_error(check_format_prior_formula(N()),
                 "`formula` not a formula.")
    expect_error(check_format_prior_formula("age~N()"),
                 "`formula` not a formula.")
    expect_error(check_format_prior_formula(~N()),
                 "`formula` has too few elements")
})


## 'check_formula_has_intercept' ----------------------------------------------

test_that("'check_formula_has_intercept' returns TRUE with valid inputs", {
    expect_true(check_formula_has_intercept(y ~ x + z))
    expect_true(check_formula_has_intercept(y ~ 1))
})

test_that("'check_formula_has_intercept' returns correct error with invalid inputs", {
    expect_error(check_formula_has_intercept(y ~ age - 1),
                 "`formula` does not include an intercept.")
})


## 'check_formula_has_response' -----------------------------------------------

test_that("'check_formula_has_response' returns TRUE with valid inputs", {
    expect_true(check_formula_has_response(y ~ x + z))
    expect_true(check_formula_has_response(y ~ x + z - 1))
    expect_true(check_formula_has_response(y ~ 1))
})

test_that("'check_formula_has_response' returns correct error with invalid inputs", {
    expect_error(check_formula_has_response(~ x + z),
                 "`formula` does not include a response variable.")
    expect_error(check_formula_has_response(~ 1),
                 "`formula` does not include a response variable.")
})


## 'check_formula_has_variable' -----------------------------------------------

test_that("'check_formula_has_variable' returns TRUE with valid inputs", {
    expect_true(check_formula_has_variable(name = "age",
                                           formula = deaths ~ age*sex + time))
    expect_true(check_formula_has_variable(name = "age",
                                           formula = deaths ~ age:sex + time))
    expect_true(check_formula_has_variable(name = "time",
                                           formula = deaths ~ age*sex + time))
})

test_that("'check_formula_has_variable' returns correct error with invalid inputs", {
    expect_error(check_formula_has_variable(name = "wrong",
                                            formula = deaths ~ age*sex + time),
                 "`formula` does not have variable \"wrong\"")
})


## 'check_formula_vnames_in_data' ---------------------------------------------

test_that("'check_formula_vnames_in_data' returns TRUE with valid inputs", {
    expect_true(check_formula_vnames_in_data(y ~ x + z,
                                             data.frame(x = 1, y = 2, z = 3)))
    expect_true(check_formula_vnames_in_data(y ~ x + z - 1,
                                             data.frame(x = 1, y = 2, z = 3)))
    expect_true(check_formula_vnames_in_data(y ~ 1,
                                             data.frame(x = 1, y = 2, z = 3)))
})

test_that("'check_formula_vnames_in_data' returns correct error with invalid inputs", {
    expect_error(check_formula_vnames_in_data(y ~ wrong,
                                              data.frame(x = 1, y = 2, z = 3)),
                 "Variable `wrong` from `formula` not found in `data`.")
})


## 'check_has_disp_if_condition_on_expected' ----------------------------------

test_that("'check_has_disp_if_condition_on_expected' works", {
    set.seed(0)
    data <- expand.grid(age = 0:9, sex = c("F", "M"))
    data$popn <- rpois(n = nrow(data), lambda = 100)
    data$deaths <- rpois(n = nrow(data), lambda = 10)
    formula <- deaths ~ age + sex
    mod <- mod_pois(formula = formula,
                    data = data,
                    exposure = popn)
    expect_true(check_has_disp_if_condition_on_expected(mod))
    mod <- set_disp(mod, mean = 0)
    expect_error(check_has_disp_if_condition_on_expected(mod),
                 "`condition_on` is \"expected\" but model has no dispersion term")
})


## 'check_has_no_dots' --------------------------------------------------------

test_that("'check_has_no_dots works with unnamed invalid arguments", {
  f <- function(x, ...) {
    check_has_no_dots(...)
  }
  expect_true(f(x = 3))
  expect_error(f(x = 3, "wrong"),
               "Invalid unnamed argument")
  expect_error(f(x = 3, "wrong", 1:10),
               "2 invalid unnamed arguments")
})

test_that("'check_has_no_dots works with named invalid arguments", {
  f <- function(x, y, ...) {
    check_has_no_dots(...)
  }
  expect_true(f(x = 3, y = 1))
  expect_error(f(x = 3, y = 4, z = "wrong"),
               "`z` is not a valid argument.")
  expect_error(f(x = 3, z = "wrong", y = 4, q = "alsowrong"),
               "`z` is not a valid argument.")
})


## 'check_inf' ----------------------------------------------------------------

test_that("'check_inf' returns TRUE with valid inputs", {
  x <- 1:5
  expect_true(check_inf(x, nm_x = "x"))
  x <- c(-1, 0.2)
  expect_true(check_inf(x))
})

test_that("'check_inf' throws correct error with invalid inputs", {
  x <- c(1:5, -Inf)
  expect_error(check_inf(x, nm_x = "x"),
               "`x` has infinite value.")
  x <- c(-1, Inf, 0.2, Inf)
  expect_error(check_inf(x, nm_x = "x"),
               "`x` has infinite values.")
})


## 'check_is_dataframe' -------------------------------------------------------

test_that("'check_is_dataframe' works with valid inputs", {
  expect_true(check_is_dataframe(x = data.frame(), nm_x = "data"))
  expect_true(check_is_dataframe(x = tibble(), nm_x = data))
})

test_that("'check_is_dataframe' throws correct error with non-dataframe", {
  expect_error(check_is_dataframe(x = "a", nm_x = "val"),
               "`val` is not a data frame.")
})
          

## 'check_is_fitted' ----------------------------------------------------------

test_that("'check_is_fitted' works", {
    set.seed(0)
    data <- expand.grid(age = 0:9, sex = c("F", "M"))
    data$popn <- rpois(n = nrow(data), lambda = 100)
    data$deaths <- rpois(n = nrow(data), lambda = 10)
    formula <- deaths ~ age + sex
    mod <- mod_pois(formula = formula,
                    data = data,
                    exposure = popn)
    expect_error(check_is_fitted(x = mod, nm_x = "Y"),
                 "`Y` has not been fitted.")
    mod <- fit(mod)
    expect_true(check_is_fitted(x = mod, nm_x = "Y"))
})


## 'check_is_formula' ---------------------------------------------------------

test_that("'check_is_formula' returns TRUE when 'formula' is a formula", {
    expect_true(check_is_formula(deaths ~ age + sex))
    expect_true(check_is_formula(~ age))
})

test_that("'check_is_formula' throws correct error when not formula", {
    expect_error(check_is_formula("~age"),
                 "`formula` is not an R formula.")
})


## 'check_is_matrix' ----------------------------------------------------------

test_that("'check_is_matrix' works with valid inputs", {
  expect_true(check_is_matrix(x = matrix(), nm_x = "data"))
})

test_that("'check_is_matrix' throws correct error with non-matrix", {
  expect_error(check_is_matrix(x = "a", nm_x = "val"),
               "`val` is not a matrix.")
})


## 'check_is_ssvd' ------------------------------------------------------------

test_that("'check_is_ssvd' works with valid inputs", {
  expect_true(check_is_ssvd(x = HMD, nm_x = "ssvd"))
  expect_error(check_is_ssvd(x = NULL, nm_x = "ssvd"),
               "`ssvd` does not hold scaled SVD values.")
})


## 'check_length_effect_ge' ---------------------------------------------------

test_that("'check_length_effect_ge' returns TRUE with valid inputs", {
    expect_true(check_length_effect_ge(length_effect = 10L,
                                       min = 3L,
                                       nm = "age",
                                       prior = N()))
})

test_that("'check_length_effect_ge' throws correct error with length less than min", {
    expect_error(check_length_effect_ge(length_effect = 1L,
                                        min = 2L,
                                        nm = "age",
                                        prior = N()),
                 "`N\\(\\)` prior cannot be used for `age` term.")                
})


## 'check_lt_one' -------------------------------------------------------------

test_that("'check_lt_one' returns TRUE with valid inputs", {
  expect_true(check_lt_one(c(0.9, -30, 0.5, NA, -Inf), nm_x = "x"))
})

test_that("'check_lt_one' throws correct error with valid inputs", {
  expect_error(check_lt_one(c(1, -0.000001, 0.3, NA, -Inf), nm_x = "x", nm_df = "d"),
               "Variable `x` in `d` has value greater than or equal to 1.")
  expect_error(check_lt_one(c(1, -0.000001, 3, NA, Inf), nm_x = "x", nm_df = "d"),
               "Variable `x` in `d` has values greater than or equal to 1.")
})


## 'check_mod_est_sim_compatible' ---------------------------------------------

test_that("'check_mod_est_sim_compatible' returns TRUE with indentical models", {
    set.seed(0)
    data <- expand.grid(age = 0:9, time = 2000:2005, sex = c("F", "M"))
    data$popn <- rpois(n = nrow(data), lambda = 100)
    data$deaths <- rpois(n = nrow(data), lambda = 10)
    formula <- deaths ~ age + sex + time
    mod <- mod_pois(formula = formula,
                    data = data,
                    exposure = popn)
    expect_true(check_mod_est_sim_compatible(mod, mod))
})

test_that("'check_mod_est_sim_compatible' raises correct error with different classes", {
    set.seed(0)
    data <- expand.grid(age = 0:9, time = 2000:2005, sex = c("F", "M"))
    data$popn <- rpois(n = nrow(data), lambda = 100)
    data$deaths <- rpois(n = nrow(data), lambda = 10)
    formula <- deaths ~ age + sex + time
    mod <- mod_pois(formula = formula,
                    data = data,
                    exposure = popn)
    expect_error(check_mod_est_sim_compatible(mod, 1L),
                 "`mod_est` and `mod_sim` have different classes")
})

test_that("'check_mod_est_sim_compatible' raises correct error with different outcomes", {
    set.seed(0)
    data <- expand.grid(age = 0:9, time = 2000:2005, sex = c("F", "M"))
    data$popn <- rpois(n = nrow(data), lambda = 100)
    data$deaths1 <- rpois(n = nrow(data), lambda = 10)
    data$deaths2 <- rpois(n = nrow(data), lambda = 10)
    formula1 <- deaths1 ~ age + sex + time
    formula2 <- deaths2 ~ age + sex + time
    mod1 <- mod_pois(formula = formula1,
                    data = data,
                    exposure = popn)
    mod2 <- mod_pois(formula = formula2,
                    data = data,
                    exposure = popn)
    expect_error(check_mod_est_sim_compatible(mod1, mod2),
                 "`mod_est` and `mod_sim` have different outcome variables")
})

test_that("'check_mod_est_sim_compatible' raises correct error when data have different variables", {
    set.seed(0)
    data <- expand.grid(age = 0:9, time = 2000:2005, sex = c("F", "M"))
    data$popn <- rpois(n = nrow(data), lambda = 100)
    data$deaths <- rpois(n = nrow(data), lambda = 10)
    data2 <- data
    data2$region <- "a"
    formula <- deaths ~ age + sex + time
    mod1 <- mod_pois(formula = formula,
                    data = data,
                    exposure = popn)
    mod2 <- mod_pois(formula = formula,
                    data = data2,
                    exposure = popn)
    expect_error(check_mod_est_sim_compatible(mod1, mod2),
                 "`mod_est` and `mod_sim` have different variables.")
})

test_that("'check_mod_est_sim_compatible' raises correct error when data have different values", {
    set.seed(0)
    data <- expand.grid(age = 0:9, time = 2000:2005, sex = c("F", "M"))
    data$popn <- rpois(n = nrow(data), lambda = 100)
    data$deaths <- rpois(n = nrow(data), lambda = 10)
    data2 <- data
    data2$time[118] <- 2004
    formula <- deaths ~ age + sex + time
    mod1 <- mod_pois(formula = formula,
                    data = data,
                    exposure = popn)
    mod2 <- mod_pois(formula = formula,
                    data = data2,
                    exposure = popn)
    expect_error(check_mod_est_sim_compatible(mod1, mod2),
                 "`mod_est` and `mod_sim` have different data")
})


## 'check_mod_has_obs' --------------------------------------------------------

test_that("'check_mod_has_obs' returns TRUE with valid data", {
    set.seed(0)
    data <- expand.grid(age = 0:9, time = 2000:2005, sex = c("F", "M"))
    data$popn <- rpois(n = nrow(data), lambda = 100)
    data$deaths <- rpois(n = nrow(data), lambda = 10)
    formula <- deaths ~ age + sex + time
    mod <- mod_pois(formula = formula,
                    data = data,
                    exposure = popn)
    expect_true(check_mod_has_obs(mod))
})

test_that("'check_mod_has_obs' returns correct error with zero-row data", {
    set.seed(0)
    data <- expand.grid(age = 0:9, time = 2000:2005, sex = c("F", "M"))
    data$popn <- rpois(n = nrow(data), lambda = 100)
    data$deaths <- rpois(n = nrow(data), lambda = 10)
    formula <- deaths ~ age + sex + time
    mod <- mod_pois(formula = formula,
                    data = data[FALSE, ],
                    exposure = popn)
    expect_error(check_mod_has_obs(mod),
                 "No data for fitting model.")
})

test_that("'check_mod_has_obs' returns correct error with no valid rows - has exposure", {
    set.seed(0)
    data <- expand.grid(age = 0:9, time = 2000:2005, sex = c("F", "M"))
    data$popn <- rpois(n = nrow(data), lambda = 100)
    data$deaths <- rpois(n = nrow(data), lambda = 10)
    data$popn[1:5] <- NA
    data$popn[6] <- 0
    data$deaths[6] <- 0
    data$deaths[-(1:6)] <- NA
    formula <- deaths ~ age + sex + time
    mod <- mod_pois(formula = formula,
                    data = data,
                    exposure = popn)
    expect_error(check_mod_has_obs(mod),
                 "No data for fitting model.")
})

test_that("'check_mod_has_obs' returns correct error with no valid rows - no exposure", {
    set.seed(0)
    data <- expand.grid(age = 0:9, time = 2000:2005, sex = c("F", "M"))
    data$popn <- rpois(n = nrow(data), lambda = 100)
    data$deaths <- rpois(n = nrow(data), lambda = 10)
    data$deaths[-1] <- NA
    data$time[1] <- NA
    formula <- deaths ~ age + sex + time
    mod <- mod_pois(formula = formula,
                    data = data,
                    exposure = 1)
    expect_error(check_mod_has_obs(mod),
                 "No data for fitting model.")
})


## 'check_min_max_ar' ---------------------------------------------------------

test_that("'check_min_max_ar' returns TRUE with valid inputs", {
    expect_true(check_min_max_ar(min = -1, max = 1))
    expect_true(check_min_max_ar(min = 0, max = 0.9))
})

test_that("'check_min_max_ar' returns correct error with non-numeric", {
    expect_error(check_min_max_ar(min = "-1", max = 1),
                 "`min` is non-numeric.")
})

test_that("'check_min_max_ar' returns correct error with wrong length", {
    expect_error(check_min_max_ar(min = -1, max = 1:2),
                 "`max` does not have length 1.")
})

test_that("'check_min_max_ar' returns correct error with NA", {
    expect_error(check_min_max_ar(min = NA_real_, max = 1),
                 "`min` is NA.")
})

test_that("'check_min_max_ar' returns correct error with min < -1", {
    expect_error(check_min_max_ar(min = -1.00001, max = 1),
                 "`min` is less than -1.")
})

test_that("'check_min_max_ar' returns correct error with max < 1.0001", {
    expect_error(check_min_max_ar(min = -1, max = 1.00001),
                 "`max` is greater than 1.")
})

test_that("'check_min_max_ar' returns correct error with max <= min", {
    expect_error(check_min_max_ar(min = 0, max = 0),
                 "`max` is less than or equal to `min`.")
})


## 'check_mult_high_rate' -----------------------------------------------------

test_that("'check_mult_high_rate' returns TRUE with valid values", {
  expect_true(check_mult_high_rate(1000))
  expect_true(check_mult_high_rate(1000L))
  expect_true(check_mult_high_rate(NULL))
  expect_true(check_mult_high_rate(Inf))
})

test_that("'check_mult_high_rate' throws correct error if not NULL or numeric", {
  expect_error(check_mult_high_rate("1"),
               "`mult_high_rate` not NULL or numeric.")
})

test_that("'check_mult_high_rate' throws correct error if not length 1", {
  expect_error(check_mult_high_rate(numeric()),
               "`mult_high_rate` has length 0")
  expect_error(check_mult_high_rate(1:2),
               "`mult_high_rate` has length 2")
})

test_that("'check_mult_high_rate' throws correct error if NA", {
  expect_error(check_mult_high_rate(NA_real_),
               "`mult_high_rate` is NA")
})

test_that("'check_mult_high_rate' throws correct error if non-positive", {
  expect_error(check_mult_high_rate(0),
               "`mult_high_rate` non-positive")
  expect_error(check_mult_high_rate(-1),
               "`mult_high_rate` non-positive")
  expect_error(check_mult_high_rate(-Inf),
               "`mult_high_rate` non-positive")
})


## 'check_n_along_ge' ---------------------------------------------------------

test_that("'check_n_along_ge' returns TRUE with valid inputs", {
  expect_true(check_n_along_ge(n_along = 10L,
                               min = 3L,
                               nm = "age:sex",
                               prior = Lin()))
})

test_that("'check_n_along_ge' throws correct error with length less than min", {
  expect_error(check_n_along_ge(n_along = 1L,
                                min = 2L,
                                nm = "age:sex",
                                prior = Lin()),
               "`Lin\\(\\)` prior cannot be used for `age:sex` term.")                
})


## 'check_nan' ----------------------------------------------------------------

test_that("'check_nan' returns TRUE with valid inputs", {
  x <- 1:5
  expect_true(check_nan(x, nm_x = "x"))
  x <- c(-1, 0.2)
  expect_true(check_nan(x))
})

test_that("'check_nan' throws correct error with invalid inputs", {
  x <- c(1:5, NaN)
  expect_error(check_nan(x, nm_x = "x"),
               "`x` has NaN.")
  x <- c(-1, NaN, 0.2, NA)
  expect_error(check_nan(x, nm_x = "x"),
               "`x` has NaN.")
})


## 'check_na' -----------------------------------------------------------------

test_that("'check_na' returns TRUE with valid inputs", {
  x <- 1:5
  expect_true(check_na(x, nm_x = "x"))
  x <- c(-1, 0.2)
  expect_true(check_na(x))
})

test_that("'check_na' throws correct error with invalid inputs", {
  x <- c(1:5, NA)
  expect_error(check_na(x, nm_x = "x"),
               "`x` has NA.")
  x <- c(-1, NA, 0.2, NA)
  expect_error(check_na(x, nm_x = "x"),
               "`x` has NA.")
})


## 'check_new_seeds' ----------------------------------------------------------

test_that("'check_new_seeds works with valid inputs", {
  expect_true(check_new_seeds(NULL))
  expect_true(check_new_seeds(list(seed_components = 1,
                                   seed_augment = 2,
                                   seed_forecast_components = 3,
                                   seed_forecast_augment = 4)))
})

test_that("'check_new_seeds' throws correct error with non-NULL, non-list", {
  expect_error(check_new_seeds(1),
               "`new_seeds` is not a list.")
})

test_that("'check_new_seeds' throws correct error with wrong number of elements", {
  expect_error(check_new_seeds(list(seed_components = 2,
                                    seed_augment = 3,
                                    seed_forecast_components = 4)),
               "`new_seeds` does not have 4 elements.")
})

test_that("'check_new_seeds' throws correct error with no names", {
  expect_error(check_new_seeds(list(1, 2, 3, 4)),
               "`new_seeds` does not have names.")
})

test_that("'check_new_seeds' throws correct error with wrong names", {
  expect_error(check_new_seeds(list(seed_components = 2,
                                    wrong = 3,
                                    seed_forecast_components = 4,
                                    seed_forecast_augment = 5)),
               "`new_seeds` does not have expected names.")
})

test_that("'check_new_seeds' throws correct error with non-numeric", {
  expect_error(check_new_seeds(list(seed_components = 2,
                                    seed_augment = 3,
                                    seed_forecast_components = "4",
                                    seed_forecast_augment = 5)),
               "`new_seeds` has non-numeric element.")
})

test_that("'check_new_seeds' throws correct error with element not length 1", {
  expect_error(check_new_seeds(list(seed_components = 2,
                                    seed_augment = 3,
                                    seed_forecast_components = integer(),
                                    seed_forecast_augment = 5)),
               "`new_seeds` has element not of length 1.")
})


## 'check_number' ------------------------------------------------------------------

test_that("'check_number' returns TRUE with valid inputs", {
    expect_true(check_number(x = 1L, nm_x = "x"))
    expect_true(check_number(x = -1, nm_x = "x"))
})

test_that("'check_number' throws correct error with non-numeric", {
    expect_error(check_number(x = "4", nm_x = "x"),
                 "`x` is non-numeric.")
})

test_that("'check_number' throws correct error with length not equal to 1", {
    expect_error(check_number(x = integer(), nm_x = "x"), 
                 "`x` has length 0.")
    expect_error(check_number(x = 1:2, nm_x = "x"), 
                 "`x` has length 2.")
})

test_that("'check_number' throws correct error with NA", {
    expect_error(check_number(x = NA_real_, nm_x = "x"),
                 "`x` is NA.")
})

test_that("'check_number' throws correct error with Inf", {
    expect_error(check_number(x = Inf, nm_x = "x"),
                 "`x` is non-finite.")
})


## 'check_numeric' ------------------------------------------------------------------

test_that("'check_numeric' returns TRUE with valid inputs", {
    expect_true(check_numeric(x = 1:4, nm_x = "x"))
    expect_true(check_numeric(x = c(-1, 0), nm_x = "x"))
})

test_that("'check_numeric' throws correct error with non-numeric", {
    expect_error(check_numeric(x = "4", nm_x = "x"),
                 "`x` is non-numeric")
})

test_that("'check_numeric' throws correct error with 0 length", {
    expect_error(check_numeric(x = integer(), nm_x = "x"), 
                 "`x` has length 0")
})

test_that("'check_numeric' throws correct error with NA", {
    expect_error(check_numeric(x = c(1, NA), nm_x = "x"),
                 "`x` has NA")
})

test_that("'check_numeric' throws correct error with Inf", {
    expect_error(check_numeric(x = c(100, -1, Inf), nm_x = "x"),
                 "`x` has non-finite value.")
})


## 'check_offset_formula_not_used' --------------------------------------------

test_that("'check_offset_formula_not_used' returns TRUE with valid offset", {
  expect_true(check_offset_formula_not_used(nm_offset_data = "popn"))
  rlang::local_options(lifecycle_verbosity = "warning")
  expect_warning(check_offset_formula_not_used(nm_offset_data = "~popn + deaths"),
                 "Using a formula to specify")
})


## 'check_offset_in_data' -----------------------------------------------------

test_that("'check_offset_in_data' returns TRUE with valid formula", {
    expect_true(check_offset_in_data(nm_offset_data = "~popn + deaths",
                                     nm_offset_mod = "exposure",
                                     data = data.frame(deaths = 1, popn = 2)))
})

test_that("'check_offset_in_data' throws correct error with invalid formula", {
  expect_error(check_offset_in_data(nm_offset_data = "~popn + wrong",
                                    nm_offset_mod = "exposure",
                                    data = data.frame(deaths = 1, popn = 2)),
               "Problem with formula used for `exposure`.")
})

test_that("'check_offset_in_data' returns TRUE with valid name", {
    expect_true(check_offset_in_data(nm_offset_data = "popn",
                                     nm_offset_mod = "exposure",
                                     data = data.frame(deaths = 1, popn = 2)))
})

test_that("'check_offset_in_data' returns correct error with invalid name - exposure", {
    expect_error(check_offset_in_data(nm_offset_data = "popn",
                                      nm_offset_mod = "exposure",
                                      data = data.frame(deaths = 1, wrong = 2)),
                 "`exposure` not found in `data`")
})

test_that("'check_offset_in_data' returns correct error with invalid name - size", {
    expect_error(check_offset_in_data(nm_offset_data = "popn",
                                      nm_offset_mod = "size",
                                      data = data.frame(deaths = 1, wrong = 2)),
                 "`size` not found in `data`")
})

## 'check_offset_nonneg' ----------------------------------------------------

test_that("'check_offset_nonneg' returns TRUE with valid inputs - formula", {
    expect_true(check_offset_nonneg(nm_offset_data = "~popn - deaths",
                                    nm_offset_mod = "exposure",
                                    data = data.frame(sex = 1:2,
                                                      popn = c(0, 1.1),
                                                      deaths = 0:1)))
})

test_that("'check_offset_nonneg' returns TRUE with valid inputs - name", {
    expect_true(check_offset_nonneg(nm_offset_data = "popn",
                                    nm_offset_mod = "exposure",
                                    data = data.frame(sex = 1:2,
                                                      popn = c(0, 1.1),
                                                      deaths = 0:1)))
})

test_that("'check_offset_nonneg' returns correct error with invalid inputs - formula", {
  expect_error(check_offset_nonneg(nm_offset_data = "~popn - 1",
                                   nm_offset_mod = "exposure",
                                   data = data.frame(sex = 1:2,
                                                     popn = c(-1, 1),
                                                     deaths = 0:1)),
               "`exposure` has negative value.")
})

test_that("'check_offset_nonneg' returns correct error with invalid inputs - name", {
    expect_error(check_offset_nonneg(nm_offset_data = "popn",
                                    nm_offset_mod = "exposure",
                                    data = data.frame(sex = 1:3,
                                                      popn = c(-1, 1, -3),
                                                      deaths = 0:2)),
                 "`exposure` has negative values.")
})


## 'check_offset_not_in_formula' ----------------------------------------------

test_that("'check_offset_not_in_formula' returns TRUE with valid inputs - formula", {
    expect_true(check_offset_not_in_formula(nm_offset_data = "~popn^2 + log(popn) + deaths",
                                            nm_offset_mod = "exposure",
                                            formula = deaths ~ age + time))
})

test_that("'check_offset_not_in_formula' returns TRUE with valid inputs - name", {
    expect_true(check_offset_not_in_formula(nm_offset_data = "popn",
                                            nm_offset_mod = "exposure",
                                            formula = deaths ~ age + time))
})

test_that("'check_offset_not_in_formula' returns correct error with invalid inputs - name", {
    expect_error(check_offset_not_in_formula(nm_offset_data = "popn",
                                             nm_offset_mod = "exposure",
                                             formula = popn ~ age + time),
                 "`exposure` included in `formula`.")
})


## 'check_old_version' --------------------------------------------------------

test_that("'check_old_version' returns TRUE with valid version model", {
  set.seed(0)
  data <- expand.grid(age = 0:9, sex = c("F", "M"), time = 2001:2005)
  data$popn <- rpois(n = nrow(data), lambda = 100)
  data$deaths <- rpois(n = nrow(data), lambda = 10)
  formula <- deaths ~ (age + sex + time)^2
  mod <- mod_pois(formula = formula,
                  data = data,
                  exposure = popn)
  expect_true(check_old_version(mod))
})

test_that("'check_old_version' raises error with invalid version", {
  set.seed(0)
  data <- expand.grid(age = 0:9, sex = c("F", "M"), time = 2001:2005)
  data$popn <- rpois(n = nrow(data), lambda = 100)
  data$deaths <- rpois(n = nrow(data), lambda = 10)
  formula <- deaths ~ (age + sex + time)^2
  mod <- mod_pois(formula = formula,
                  data = data,
                  exposure = popn)
  mod_no_hyper <- mod
  mod_no_hyper$draws_hyperrandfree <- NULL
  expect_error(check_old_version(mod_no_hyper, nm_x = "object"),
               "`object` appears to have been created with an old version of bage.")
  mod_has_stored <- mod
  mod_has_stored$seed_stored_draws <- 1
  expect_error(check_old_version(mod_has_stored, nm_x = "object"),
               "`object` appears to have been created with an old version of bage.")
})


## 'check_original_scale' ----------------------------------------------

test_that("'check_original_scale' returns TRUE with valid inputs - Poisson", {
  data <- expand.grid(age = 0:9, sex = c("F", "M"), time = 2001:2005)
  data$popn <- rpois(n = nrow(data), lambda = 100)
  data$deaths <- rpois(n = nrow(data), lambda = 10)
  mod <- mod_pois(deaths ~ age + sex,
                  data = data,
                  exposure = popn)
  expect_true(check_original_scale(original_scale = FALSE,
                                              mod = mod))
})

test_that("'check_original_scale' returns TRUE with valid inputs - normal", {
  data <- expand.grid(age = 0:9, sex = c("F", "M"), time = 2001:2005)
  data$popn <- rpois(n = nrow(data), lambda = 100)
  data$deaths <- rpois(n = nrow(data), lambda = 10)
  mod <- mod_norm(deaths ~ age + sex,
                  data = data,
                  weights = popn)
  expect_true(check_original_scale(original_scale = TRUE,
                                              mod = mod))
  expect_true(check_original_scale(original_scale = FALSE,
                                              mod = mod))
})

test_that("'check_original_scale' returns TRUE with valid inputs - covariates", {
  data <- expand.grid(age = 0:9, sex = c("F", "M"), time = 2001:2005)
  data$popn <- rpois(n = nrow(data), lambda = 100)
  data$deaths <- rpois(n = nrow(data), lambda = 10)
  data$income <- rnorm(n = nrow(data))
  mod <- mod_pois(deaths ~ age + sex,
                  data = data,
                  exposure = popn) |>
    set_covariates(~income)
  expect_true(check_original_scale(original_scale = FALSE,
                                              mod = mod))
})

test_that("'check_original_scale' raises warning when 'original_scale' ignored", {
  data <- expand.grid(age = 0:9, sex = c("F", "M"), time = 2001:2005)
  data$popn <- rpois(n = nrow(data), lambda = 100)
  data$deaths <- rpois(n = nrow(data), lambda = 10)
  mod <- mod_pois(deaths ~ age + sex,
                  data = data,
                  exposure = popn)
  expect_warning(check_original_scale(original_scale = TRUE,
                                                 mod = mod),
                 paste("`components\\(\\)` ignores `original_scale` if `object` was not created",
                       "with `mod_norm\\(\\)`."))
})
              

## 'check_positive' -----------------------------------------------------------

test_that("'check_positive' returns TRUE with valid inputs", {
  expect_true(check_positive(c(1, 0.000001, 3, NA, Inf), nm_x = "x", nm_df = "d"))
})

test_that("'check_positive' throws correct error with valid inputs", {
  expect_error(check_positive(c(1, -0.000001, 3, NA, Inf), nm_x = "x", nm_df = "d"),
               "Variable `x` in `d` has value less than or equal to 0.")
  expect_error(check_positive(c(1, -0.000001, -3, NA, Inf), nm_x = "x", nm_df = "d"),
               "Variable `x` in `d` has values less than or equal to 0.")
})


## 'check_resp_le_offset' -----------------------------------------------------

test_that("'check_resp_le_offset' returns TRUE with valid inputs - formula", {
  data <- data.frame(deaths = c(0, 1, NA, 0,  NA),
                     sex = rep("F", 5),
                     popn =   c(0, 1, 2,  NA, NA))
  expect_true(check_resp_le_offset(formula = deaths ~ sex,
                                   nm_offset_data = "~  popn",
                                   nm_offset_mod = "size",
                                   data = data))
})

test_that("'check_resp_le_offset' returns TRUE with valid inputs - name", {
  data <- data.frame(deaths = c(0, 1, NA, 0,  NA),
                     sex = rep("F", 5),
                     popn =   c(0, 1, 2,  NA, NA))
  expect_true(check_resp_le_offset(formula = deaths ~ sex,
                                   nm_offset_data = "popn",
                                   nm_offset_mod = "size",
                                   data = data))
})

test_that("'check_resp_le_offset' raises correct error with invalid inputs - formula", {
    data <- data.frame(deaths = c(0, 1, NA, 0,  2),
                       sex = rep("F", 5),
                       popn =   c(0, 1, 2,  NA, 1))
    expect_error(check_resp_le_offset(formula = deaths ~ sex,
                                      nm_offset_data = "~popn - 0.1",
                                      nm_offset_mod = "size",
                                      data = data),
                 "Response greater than `size`.")
})

test_that("'check_resp_le_offset' raises correct error with invalid inputs - formula", {
    data <- data.frame(deaths = c(0, 1, NA, 0,  2),
                       sex = rep("F", 5),
                       popn =   c(0, 1, 2,  NA, 1))
    expect_error(check_resp_le_offset(formula = deaths ~ sex,
                                      nm_offset_data = "popn",
                                      nm_offset_mod = "size",
                                      data = data),
                 "Response greater than `size`.")
})


## 'check_resp_zero_if_offset_zero' -------------------------------------------

test_that("'check_resp_zero_if_offset_zero' returns TRUE with valid inputs - formula", {
    data <- data.frame(deaths = c(0, 1, NA, 0,  NA),
                       sex = rep("F", 5),
                       popn =   c(0, 1, 2,  NA, NA))
    expect_true(check_resp_zero_if_offset_zero(formula = deaths ~ sex,
                                               nm_offset_data = "~popn^2",
                                               nm_offset_mod = "exposure",
                                               data = data))
})

test_that("'check_resp_zero_if_offset_zero' returns TRUE with valid inputs - name", {
    data <- data.frame(deaths = c(0, 1, NA, 0,  NA),
                       sex = rep("F", 5),
                       popn =   c(0, 1, 2,  NA, NA))
    expect_true(check_resp_zero_if_offset_zero(formula = deaths ~ sex,
                                               nm_offset_data = "popn",
                                               nm_offset_mod = "exposure",
                                               data = data))
})

test_that("'check_resp_zero_if_offset_zero' raises correct error with invalid inputs - formula", {
  data <- data.frame(deaths = c(0, 1, NA, 0,  1),
                     sex = rep("F", 5),
                     popn =   c(0, 1, 2,  NA, 0))
  expect_error(check_resp_zero_if_offset_zero(formula = deaths ~ sex,
                                              nm_offset_data = "~popn^2",
                                              nm_offset_mod = "exposure",
                                              data = data),
               "Response is non-zero but `exposure` is zero.")
})

test_that("'check_resp_zero_if_offset_zero' raises correct error with invalid inputs - name", {
  data <- data.frame(deaths = c(0, 1, NA, 0,  1),
                     sex = rep("F", 5),
                     popn =   c(0, 1, 2,  NA, 0))
  expect_error(check_resp_zero_if_offset_zero(formula = deaths ~ sex,
                                              nm_offset_data = "popn",
                                              nm_offset_mod = "exposure",
                                              data = data),
               "Response is non-zero but `exposure` is zero.")
})


## 'check_response_nonneg' ----------------------------------------------------

test_that("'check_response_nonneg' returns TRUE with valid inputs", {
    expect_true(check_response_nonneg(formula = deaths ~ sex,
                                      data = data.frame(sex = 1:2, deaths = 0:1),
                                      nm_distn = "pois"))
})

test_that("'check_response_nonneg' returns correct error with invalid inputs", {
    expect_error(check_response_nonneg(formula = deaths ~ sex,
                                       data.frame(sex = 1:2, deaths = c(-1, 1)),
                                       nm_distn = "Poisson"),
                 "Model uses Poisson distribution but response variable has negative value.")
    expect_error(check_response_nonneg(formula = deaths ~ sex,
                                       data.frame(sex = 1:2, deaths = c(-1, -2)),
                                       nm_distn = "Poisson"),
                 "Model uses Poisson distribution but response variable has negative values.")
})


## 'check_response_not_call' --------------------------------------------------

test_that("'check_response_not_call' returns TRUE with valid inputs", {
    expect_true(check_response_not_call(deaths ~ sex))
})

test_that("'check_response_not_call' returns correct error with invalid inputs", {
  expect_error(check_response_not_call(log(deaths) ~ sex),
               "Response includes function call.")
})


## 'check_scale' --------------------------------------------------------------

test_that("'check_scale' returns TRUE with valid inputs", {
    expect_true(check_scale(1L, nm_x = "x", zero_ok = FALSE))
    expect_true(check_scale(0.001, nm_x = "x", zero_ok = FALSE))
})

test_that("'check_scale' returns correct error with non-numeric", {
    expect_error(check_scale("1", nm_x = "x", zero_ok = FALSE),
                 "`x` is non-numeric.")
})

test_that("'check_scale' returns correct error with non-numeric", {
    expect_error(check_scale("1", nm_x = "x", zero_ok = FALSE),
                 "`x` is non-numeric.")
})

test_that("'check_scale' returns correct error with wrong length", {
    expect_error(check_scale(1:2, nm_x = "x", zero_ok = FALSE),
                 "`x` does not have length 1.")
})

test_that("'check_scale' returns correct error with NA", {
    expect_error(check_scale(NA_real_, nm_x = "x", zero_ok = FALSE),
                 "`x` is NA.")
})

test_that("'check_scale' returns correct error with Inf", {
    expect_error(check_scale(Inf, nm_x = "x", zero_ok = FALSE),
                 "`x` is infinite.")
})

test_that("'check_scale' returns correct error with negative", {
    expect_true(check_scale(0, nm_x = "x", zero_ok = TRUE))
    expect_error(check_scale(-1, nm_x = "x", zero_ok = TRUE),
                 "`x` is negative.")
})

test_that("'check_scale' returns correct error with non-positive", {
    expect_error(check_scale(0, nm_x = "x", zero_ok = FALSE),
                 "`x` is non-positive.")
    expect_error(check_scale(-1, nm_x = "x", zero_ok = FALSE),
                 "`x` is non-positive.")
})


## 'check_string' --------------------------------------------------------------

test_that("'check_string' returns TRUE with valid inputs", {
    expect_true(check_string("age", nm_x = "x"))
})

test_that("'check_string' returns correct error with non-numeric", {
    expect_error(check_string(1, nm_x = "x"),
                 "`x` is non-character.")
})

test_that("'check_string' returns correct error with wrong length", {
    expect_error(check_string(c("a", "b"), nm_x = "x"),
                 "`x` does not have length 1.")
})

test_that("'check_string' returns correct error with NA", {
    expect_error(check_string(NA_character_, nm_x = "x"),
                 "`x` is NA.")
})

test_that("'check_string' returns correct error with blank", {
    expect_error(check_string("", nm_x = "x"),
                 "`x` is blank.")
})


## 'check_ssvd_has_sexgender' -------------------------------------------------

test_that("'check_ssvd_has_sexgender' works", {
  expect_true(check_ssvd_has_sexgender(x = HMD, nm_x = "ssvd"))
  HMD_nogender <- HMD
  HMD_nogender$data <- HMD_nogender$data[sapply(HMD_nogender$data$labels_sexgender, is.null),]
  expect_error(check_ssvd_has_sexgender(x = HMD_nogender, nm_x = "ssvd"),
               "`ssvd` does not have a sex/gender dimension.")
})


## 'check_svd_agesex' ---------------------------------------------------------

test_that("'check_svd_agesex' works with bage_prior_svd, correct inputs", {
  prior <- SVD(HMD)
  expect_true(check_svd_agesex(prior = prior,
                               var_age = "age",
                               nm = "age:sex",
                               agesex = "age:sex"))
})

test_that("'check_svd_agesex'  throws correct error when 'var_age' is NULL", {
  prior <- SVD_AR1(HMD)
  expect_error(check_svd_agesex(prior = prior,
                                nm = "bla",
                                var_age = NULL,
                                agesex = "other"),
               "Problem with `SVD_AR1\\(\\)` prior for term `bla`.")
})

test_that("'check_svd_agesex'  throws correct error when agesex is 'other'", {
  prior <- SVD_RW(HMD)
  expect_error(check_svd_agesex(prior = prior,
                                nm = "bla",
                                var_age = "age",
                                agesex = "other"),
               "Problem with `SVD_RW\\(\\)` prior for term `bla`.")
})


## 'check_prior_age' ---------------------------------------------------------

test_that("'check_prior_age' works with bage_prior_rw2infant, correct inputs", {
  prior <- RW2_Infant()
  expect_true(check_prior_age(prior = prior,
                              nm = "age:year:gender",
                              var_age = "year"))
})

test_that("'check_prior_age'  throws correct error when age variable not identified", {
  prior <- RW2_Infant()
  expect_error(check_prior_age(prior = prior,
                               nm = "reg:time:age",
                               var_age = NULL),
               "Problem with `RW2_Infant\\(\\)` prior for term `reg:time:age`.")
})

test_that("'check_prior_age'  throws correct error when age variable not present", {
  prior <- RW2_Infant()
  expect_error(check_prior_age(prior = prior,
                               nm = "reg:time:sex",
                               var_age = "age"),
               "Problem with `RW2_Infant\\(\\)` prior for term `reg:time:sex`.")
})


## 'check_prior_time' ---------------------------------------------------------

test_that("'check_prior_time' works with bage_prior_svd_ar, correct inputs", {
  prior <- SVD_AR1(HMD)
  expect_true(check_prior_time(prior = prior,
                             nm = "age:year:gender",
                             var_time = "year"))
})

test_that("'check_prior_time'  throws correct error when time variable not identified", {
  prior <- SVD_RW(HMD)
  expect_error(check_prior_time(prior = prior,
                                nm = "reg:age:time",
                                var_time = NULL),
               "Problem with `SVD_RW\\(\\)` prior for term `reg:age:time`.")
})

test_that("'check_prior_time'  throws correct error when time variable not present", {
  prior <- SVD_RW2(HMD)
  expect_error(check_prior_time(prior = prior,
                              nm = "reg:age:sex",
                              var_time = "time"),
               "Problem with `SVD_RW2\\(\\)` prior for term `reg:age:sex`.")
})


## 'check_var_prec' -----------------------------------------------------------

test_that("'check_var_prec' returns TRUE with valid inputs", {
  x <- matrix(1, nr = 7, nc = 7,
              dimnames = list(c("a", "a", "b", "c", "c", "a", "disp"),
                              c("a", "a", "b", "c", "c", "a", "disp")))
  est <- list(effectfree = c(a = 1, a = 2, b = 3, c = 4, c = 5),
              hyper = c(a = 1),
              hyperrandfree = double(),
              log_disp = c(disp = 3))
  expect_true(check_var_prec(x = x, est = est))
})

test_that("'check_var_prec' throws correct error message with single NA", {
  x <- matrix(1, nr = 7, nc = 7,
              dimnames = list(c("a", "a", "b", "c", "c", "a", "disp"),
                              c("a", "a", "b", "c", "c", "a", "disp")))
  x[2,2] <- NA
  est <- list(effectfree = c(a = 1, a = 2, b = 3, c = 4, c = 5),
              hyper = c(a = 1),
              hyperrandfree = double(),
              log_disp = c(disp = 3))
  expect_error(check_var_prec(x = x, est = est),
               "Problem deriving posterior distribution.")
})

test_that("'check_var_prec' throws correct error message with multiple NAs", {
  x <- matrix(1, nr = 7, nc = 7,
              dimnames = list(c("a", "a", "b", "c", "c", "a", "disp"),
                              c("a", "a", "b", "c", "c", "a", "disp")))
  x[3,3] <- NA
  x[6,6] <- NA
  x[7,7] <- NA
  est <- list(effectfree = c(a = 1, a = 2, b = 3, c = 4, c = 5),
              hyper = c(a = 1),
              hyperrandfree = double(),
              log_disp = c(disp = 3))
  expect_error(check_var_prec(x = x, est = est),
               "Problem deriving posterior distribution.")
})



## 'check_vars_inner' ---------------------------------------------------------

test_that("'check_vars_inner' returns TRUE with valid inputs", {
  expect_true(check_vars_inner(c("age", "sex")))
})

test_that("'check_vars_inner' throws correct error with non-character", {
  expect_error(check_vars_inner(1:2),
               "`vars_inner` is not a character vector.")
})

test_that("'check_vars_inner' throws correct error with length 0", {
  expect_error(check_vars_inner(character()),
               "`vars_inner` has length 0.")
})

test_that("'check_vars_inner' throws correct error with NA", {
  expect_error(check_vars_inner(c("age", NA)),
               "`vars_inner` has NA.")
  expect_error(check_vars_inner(c("age", NA, NA)),
               "`vars_inner` has NAs.")
})

test_that("'check_vars_inner' throws correct error with blanks", {
  expect_error(check_vars_inner(c("age", "")),
               "`vars_inner` has blank.")
  expect_error(check_vars_inner(c("age", "", "")),
               "`vars_inner` has blanks.")
})

test_that("'check_vars_inner' throws correct error with duplicates", {
  expect_error(check_vars_inner(c("age", "age")),
               "`vars_inner` has duplicate.")
  expect_error(check_vars_inner(c("age", "age", "age")),
               "`vars_inner` has duplicates.")
})


## 'check_widths' -------------------------------------------------------------

test_that("'check_widths' returns TRUE with valid inputs", {
    expect_true(check_widths(0.96))
    expect_true(check_widths(c(0.2, 0.8)))
    expect_true(check_widths(c(0.001, 1)))
})

test_that("'check_widths' throws expected error non-numeric", {
    expect_error(check_widths(TRUE),
                 "`widths` has class <logical>.")
})

test_that("'check_widths' throws expected error length 0", {
    expect_error(check_widths(integer()),
                 "`widths` has length 0.")
})

test_that("'check_widths' throws expected error NA", {
    expect_error(check_widths(c(NA, 1)),
                 "`widths` has NA.")
    expect_error(check_widths(c(NA, 1, NA)),
                 "`widths` has NAs.")
})

test_that("'check_widths' throws expected error too low", {
    expect_error(check_widths(0),
                 "`widths` has value not in interval \\(0, 1\\].")
})

test_that("'check_widths' throws expected error too high", {
    expect_error(check_widths(c(1.2, 1.3)),
                 "`widths` has values not in interval \\(0, 1\\].")
})


## 'check_v_ssvd' -------------------------------------------------------------

test_that("'check_v_ssvd' returns TRUE with valid inputs", {
  expect_true(check_v_ssvd(v = NULL, ssvd = HMD, nm_ssvd = "HMD"))
  expect_true(check_v_ssvd(v = "v2024", ssvd = HMD, nm_ssvd = "HMD"))
})

test_that("'check_v_ssvd' throws correct error with invalid inputs, multiple versions", {
  expect_error(check_v_ssvd(v = "wrong", ssvd = HMD, nm_ssvd = "HMD"),
               "Invalid value for version parameter `v`.")
})

test_that("'check_v_ssvd' throws correct error with invalid inputs, single version", {
  ssvd <- sim_ssvd()
  expect_error(check_v_ssvd(v = "wrong", ssvd = ssvd, nm_ssvd = "SSVD"),
               "Invalid value for version parameter `v`.")
})


