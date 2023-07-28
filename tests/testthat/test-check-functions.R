
## 'check_flag' ---------------------------------------------------------------

test_that("'check_flag' returns TRUE with valid inputs", {
    x <- TRUE
    expect_true(check_flag(x))
    x <- FALSE
    expect_true(check_flag(x))
})

test_that("'check_flag' throws expected error non-length-1", {
    y <- logical()
    expect_error(check_flag(y),
                 "`y` does not have length 1")
    z <- c(TRUE, TRUE)
    expect_error(check_flag(z),
                 "`z` does not have length 1")
})

test_that("'check_flag' throws expected error non-logical", {
    x <- "hello"
    expect_error(check_flag(x),
                 "`x` does not have class <logical>")
})

test_that("'check_flag' throws expected error NA", {
    x <- NA
    expect_error(check_flag(x),
                 "`x` is NA")
})


## 'check_format_prior_formula' -----------------------------------------------

test_that("'check_format_prior_formula' returns TRUE with valid inputs", {
    expect_true(check_format_prior_formula(age ~ N()))
    expect_true(check_format_prior_formula(time:sex~RW()))
})

test_that("'check_format_prior_formula' returns correct error with invalid inputs", {
    expect_error(check_format_prior_formula("age~N()"))
    expect_error(check_format_prior_formula(~N()),
                 "prior formula '~N\\(\\)' has too few elements")
})


## 'check_formula_has_predictors' -----------------------------------------------

test_that("'check_formula_has_predictors' returns TRUE with valid inputs", {
    expect_true(check_formula_has_predictors(y ~ x + z))
    expect_true(check_formula_has_predictors(y ~ x + z - 1))
})

test_that("'check_formula_has_predictors' returns correct error with invalid inputs", {
    expect_error(check_formula_has_predictors(~ 1),
                 "formula '~1' does not have any predictors")
})


## 'check_formula_has_response' -----------------------------------------------

test_that("'check_formula_has_response' returns TRUE with valid inputs", {
    expect_true(check_formula_has_response(y ~ x + z))
    expect_true(check_formula_has_response(y ~ x + z - 1))
    expect_true(check_formula_has_response(y ~ 1))
})

test_that("'check_formula_has_response' returns correct error with invalid inputs", {
    expect_error(check_formula_has_response(~ x + z),
                 "formula '~x \\+ z' does not have a response variable")
    expect_error(check_formula_has_response(~ 1),
                 "formula '~1' does not have a response variable")
})


## 'check_formula_as_variable' ------------------------------------------------

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
                 "formula 'deaths ~ age \\* sex \\+ time' does not have variable \"wrong\"")
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
                 "variable 'wrong' from formula 'y ~ wrong' not found in 'data'")
})


## 'check_is_main_effect' -----------------------------------------------------

test_that("'check_is_main_effect' returns TRUE with valid inputs", {
    expect_true(check_is_main_effect("age", N()))
})

test_that("'check_is_main_effect' throws correct error when interaction", {
    expect_error(check_is_main_effect("age:sex", AR1()),
                 "`AR1\\(\\)` prior cannot be used for `age:sex` term.")                 
})    


## 'check_length_par_gt' ------------------------------------------------------

test_that("'check_length_par_gt' returns TRUE with valid inputs", {
    expect_true(check_length_par_gt(length_par = 10L,
                                    min = 3L,
                                    nm = "age",
                                    prior = N()))
})

test_that("'check_length_par_gt' throws correct error with length less than min", {
    expect_error(check_length_par_gt(length_par = 1L,
                                     min = 2L,
                                     nm = "age",
                                     prior = N()),
                 "`N\\(\\)` prior cannot be used for `age` term.")                
})


## 'check_offset_in_data' -----------------------------------------------------

test_that("'check_offset_in_data' returns TRUE with valid inputs", {
    expect_true(check_offset_in_data(vname_offset = "popn",
                                     nm_offset = "exposure",
                                     data = data.frame(deaths = 1, popn = 2)))
})

test_that("'check_offset_in_data' returns correct error with invalid inputs", {
    expect_error(check_offset_in_data(vname_offset = "popn",
                                      nm_offset = "exposure",
                                      data = data.frame(deaths = 1, wrong = 2)),
                 "exposure variable \\[popn\\] not found in 'data'")
})


## 'check_n' ------------------------------------------------------------------

test_that("'check_n' returns TRUE with valid inputs", {
    expect_true(check_n(n = 4, min = 4L, max = NULL, null_ok = FALSE))
    expect_true(check_n(n = NULL, min = 4L, max = NULL, null_ok = TRUE))
})

test_that("'check_n' throws correct error with non-numeric", {
    expect_error(check_n(n = "4", min = 4L, max = NULL, null_ok = FALSE),
                 "`n` is non-numeric")
})

test_that("'check_n' throws correct error with wrong length", {
    expect_error(check_n(n = integer(), min = 4L, max = NULL, null_ok = FALSE),
                 "`n` does not have length 1")
    expect_error(check_n(n = 10:11, min = 4L, max = NULL, null_ok = FALSE),
                 "`n` does not have length 1")
})

test_that("'check_n' throws correct error with NA", {
    expect_error(check_n(n = NA_real_, min = 4L, max = NULL, null_ok = FALSE),
                 "`n` is NA")
})

test_that("'check_n' throws correct error with Inf", {
    expect_error(check_n(n = Inf, min = 4L, max = NULL, null_ok = FALSE),
                 "`n` is Inf")
})

test_that("'check_n' throws correct error with non-integer", {
    expect_error(check_n(n = 6.4, min = 4L, max = NULL, null_ok = FALSE),
                 "`n` is not an integer")
})

test_that("'check_n' throws correct error when less than min", {
    expect_error(check_n(n = 3, min = 4L, max = NULL, null_ok = FALSE),
                 "`n` is less than 4")
})

test_that("'check_n' throws correct error when greater than max", {
    expect_error(check_n(n = 60, min = 4, max = 10, null_ok = FALSE),
                 "`n` is greater than 10")
})



## 'check_offset_nonneg' ----------------------------------------------------

test_that("'check_offset_nonneg' returns TRUE with valid inputs", {
    expect_true(check_offset_nonneg(vname_offset = "popn",
                                    nm_offset = "exposure",
                                    data = data.frame(sex = 1:2,
                                                      popn = c(0, 1.1),
                                                      deaths = 0:1)))
})

test_that("'check_offset_nonneg' returns correct error with invalid inputs", {
    expect_error(check_offset_nonneg(vname_offset = "popn",
                                    nm_offset = "exposure",
                                    data = data.frame(sex = 1:2,
                                                      popn = c(-1, 1),
                                                      deaths = 0:1)),
                 "exposure variable \\[popn\\] has negative values")
})


## 'check_offset_not_in_formula' ----------------------------------------------

test_that("'check_offset_not_in_formula' returns TRUE with valid inputs", {
    expect_true(check_offset_not_in_formula(vname_offset = "popn",
                                            nm_offset = "exposure",
                                            formula = deaths ~ age + time))
})

test_that("'check_offset_not_in_formula' returns correct error with invalid inputs", {
    expect_error(check_offset_not_in_formula(vname_offset = "popn",
                                             nm_offset = "exposure",
                                             formula = popn ~ age + time),
                 "exposure variable \\[popn\\] included in formula 'popn ~ age \\+ time'")
})


## 'check_resp_le_offset' -----------------------------------------------------

test_that("'check_resp_le_offset' returns TRUE with valid inputs", {
    data <- data.frame(deaths = c(0, 1, NA, 0,  NA),
                       sex = rep("F", 5),
                       popn =   c(0, 1, 2,  NA, NA))
    expect_true(check_resp_le_offset(formula = deaths ~ sex,
                                               vname_offset = "popn",
                                               data = data))
})

test_that("'check_resp_zero_if_offset_zero' raises correct error with invalid inputs", {
    data <- data.frame(deaths = c(0, 1, NA, 0,  2),
                       sex = rep("F", 5),
                       popn =   c(0, 1, 2,  NA, 1))
    expect_error(check_resp_le_offset(formula = deaths ~ sex,
                                      vname_offset = "popn",
                                      data = data),
                 "'deaths' \\[2\\] is greater than 'popn' \\[1\\]")
})


## 'check_resp_zero_if_offset_zero' -------------------------------------------

test_that("'check_resp_zero_if_offset_zero' returns TRUE with valid inputs", {
    data <- data.frame(deaths = c(0, 1, NA, 0,  NA),
                       sex = rep("F", 5),
                       popn =   c(0, 1, 2,  NA, NA))
    expect_true(check_resp_zero_if_offset_zero(formula = deaths ~ sex,
                                               vname_offset = "popn",
                                               data = data))
})

test_that("'check_resp_zero_if_offset_zero' raises correct error with invalid inputs", {
    data <- data.frame(deaths = c(0, 1, NA, 0,  1),
                       sex = rep("F", 5),
                       popn =   c(0, 1, 2,  NA, 0))
    expect_error(check_resp_zero_if_offset_zero(formula = deaths ~ sex,
                                               vname_offset = "popn",
                                               data = data),
                 "'deaths' \\[1\\] is non-zero but 'popn' is zero")
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
                                       nm_distn = "pois"),
                 "distribution is \"pois\" but response variable \\[deaths\\] has negative values")
})


## 'check_scale' --------------------------------------------------------------

test_that("'check_scale' returns double with valid inputs", {
    expect_true(check_scale(1L, x_arg = "x"))
    expect_true(check_scale(0.001, x_arg = "x"))
})

test_that("'check_scale' returns correct error with non-numeric", {
    expect_error(check_scale("1", x_arg = "x"),
                 "`x` is non-numeric.")
})

test_that("'check_scale' returns correct error with non-numeric", {
    expect_error(check_scale("1", x_arg = "x"),
                 "`x` is non-numeric.")
})

test_that("'check_scale' returns correct error with wrong length", {
    expect_error(check_scale(1:2, x_arg = "x"),
                 "`x` does not have length 1.")
})

test_that("'check_scale' returns correct error with NA", {
    expect_error(check_scale(NA_real_, x_arg = "x"),
                 "`x` is NA.")
})

test_that("'check_scale' returns correct error with Inf", {
    expect_error(check_scale(Inf, x_arg = "x"),
                 "`x` is infinite.")
})

test_that("'check_scale' returns correct error with non-positive", {
    expect_error(check_scale(0, x_arg = "x"),
                 "`x` is non-positive.")
    expect_error(check_scale(-1, x_arg = "x"),
                 "`x` is non-positive.")
})
