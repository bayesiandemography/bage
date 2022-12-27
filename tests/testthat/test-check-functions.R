
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





