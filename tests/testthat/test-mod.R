

## 'mod_pois' -----------------------------------------------------------------

test_that("'new_bage_mod' works with valid inputs - has exposure", {
    data <- expand.grid(age = 0:2, time = 2000:2001, sex = 1:2)
    data$popn <- seq_len(nrow(data))
    data$deaths <- rev(seq_len(nrow(data)))
    formula <- deaths ~ age:sex + time
    ans_obtained <- mod_pois(formula = formula,
                             data = data,
                             exposure = popn)
    expect_s3_class(ans_obtained, "bage_mod")
    ans_noquote <- mod_pois(formula = formula,
                            data = data,
                            exposure = popn)
    ans_withquote <- mod_pois(formula = formula,
                              data = data,
                              exposure = "popn")
    expect_identical(ans_noquote, ans_withquote)
})

test_that("'new_bage_mod' works with valid inputs - no exposure", {
    data <- expand.grid(age = 0:2, time = 2000:2001, sex = 1:2)
    data$popn <- seq_len(nrow(data))
    data$deaths <- rev(seq_len(nrow(data)))
    formula <- deaths ~ age:sex + time
    ans_obtained <- mod_pois(formula = formula,
                             data = data,
                             exposure = 1)
    expect_s3_class(ans_obtained, "bage_mod")
})


## 'new_bage_mod' ----------------------------------------------------------

test_that("'new_bage_mod' works with valid inputs - has offset", {
    data <- expand.grid(age = 0:2, time = 2000:2001, sex = 1:2)
    data$popn <- seq_len(nrow(data))
    data$deaths <- rev(seq_len(nrow(data)))
    formula <- deaths ~ age:sex + time
    ans_obtained <- new_bage_mod(formula = formula,
                                    data = data,
                                    nm_distn = "pois",
                                    is_mod_with_offset = TRUE,
                                    vname_offset = "popn",
                                    nm_offset = "exposure")
    expect_s3_class(ans_obtained, "bage_mod")
})

test_that("'new_bage_mod' works with valid inputs - no offset", {
    data <- expand.grid(age = 0:2, time = 2000:2001, sex = 1:2)
    data$deaths <- rev(seq_len(nrow(data)))
    formula <- deaths ~ age:sex + time
    ans_obtained <- new_bage_mod(formula = formula,
                                    data = data,
                                    nm_distn = "pois",
                                    is_mod_with_offset = FALSE,
                                    vname_offset = NULL,
                                    nm_offset = NULL)
    expect_s3_class(ans_obtained, "bage_mod")
})

