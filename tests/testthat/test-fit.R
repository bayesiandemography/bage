

## 'fit' -----------------------------------------------------------------

test_that("'fit' works with valid inputs", {
    data <- expand.grid(age = 0:9, time = 2000:2005, sex = 1:2)
    data$popn <- rpois(n = nrow(data), lambda = 100)
    data$deaths <- rpois(n = nrow(data), lambda = 10)
    formula <- deaths ~ age + sex
    mod <- mod_pois(formula = formula,
                    data = data,
                    exposure = popn)
    ans_obtained <- fit(mod)
    expect_s3_class(ans_obtained, "bage_sysmod")
})
