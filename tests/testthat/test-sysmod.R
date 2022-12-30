
## 'new_bage_sysmod' ----------------------------------------------------------

test_that("'new_bage_sysmod' works with valid inputs - has offset", {
    data <- expand.grid(age = 0:2, time = 2000:2001, sex = 1:2)
    data$popn <- seq_len(nrow(data))
    data$deaths <- rev(seq_len(nrow(data)))
    formula <- deaths ~ age:sex + time
    ans_obtained <- new_bage_sysmod(formula = formula,
                                    data = data,
                                    nm_distn = "pois",
                                    is_mod_with_offset = TRUE,
                                    vname_offset = "popn",
                                    nm_offset = "exposure")
    expect_s3_class(ans_obtained, "bage_sysmod")
})

test_that("'new_bage_sysmod' works with valid inputs - no offset", {
    data <- expand.grid(age = 0:2, time = 2000:2001, sex = 1:2)
    data$deaths <- rev(seq_len(nrow(data)))
    formula <- deaths ~ age:sex + time
    ans_obtained <- new_bage_sysmod(formula = formula,
                                    data = data,
                                    nm_distn = "pois",
                                    is_mod_with_offset = FALSE,
                                    vname_offset = NULL,
                                    nm_offset = NULL)
    expect_s3_class(ans_obtained, "bage_sysmod")
})

