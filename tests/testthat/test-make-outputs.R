

## 'make_terms_est' -----------------------------------------------------------------

test_that("'make_terms_est' works with valid inputs", {
    set.seed(0)
    data <- expand.grid(age = 0:9, time = 2000:2005, sex = c("F", "M"))
    data$popn <- rpois(n = nrow(data), lambda = 100)
    data$deaths <- rpois(n = nrow(data), lambda = 10)
    formula <- deaths ~ age + sex
    mod <- mod_pois(formula = formula,
                    data = data,
                    exposure = popn)
    mod <- fit(mod)
    ans_obtained <- make_terms_est(mod)
    expect_identical(names(ans_obtained), names(mod$priors))
    expect_identical(as.numeric(unlist(ans_obtained)),
                     as.numeric(unlist(mod$est$par)))
})
