
## 'check_and_tidy_scale' -----------------------------------------------------

test_that("'check_and_tidy_scale' returns double with valid inputs", {
    expect_identical(check_and_tidy_scale(1L),
                     as.double(1L))
    expect_identical(check_and_tidy_scale(0.001),
                     0.001)
})

test_that("'check_and_tidy_scale' returns correct error with invalid inputs", {
    expect_error(check_and_tidy_scale(0),
                 "'scale' equals 0")
    expect_error(check_and_tidy_scale(Inf))
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




