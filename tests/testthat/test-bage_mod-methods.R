

## 'fit' -----------------------------------------------------------------

test_that("'fit' works with valid inputs - pois", {
    set.seed(0)    
    data <- expand.grid(age = 0:9, time = 2000:2005, sex = c("F", "M"))
    data$popn <- rpois(n = nrow(data), lambda = 100)
    data$deaths <- rpois(n = nrow(data), lambda = 10)
    formula <- deaths ~ age + sex + time
    mod <- mod_pois(formula = formula,
                    data = data,
                    exposure = popn)
    ans_obtained <- fit(mod)
    expect_s3_class(ans_obtained, "bage_mod")
})

test_that("'fit' works with valid inputs - binom", {
    set.seed(0)    
    data <- expand.grid(age = 0:9, time = 2000:2005, sex = c("F", "M"))
    data$popn <- rpois(n = nrow(data), lambda = 100)
    data$deaths <- rbinom(n = nrow(data), size = data$popn, prob = 0.3)
    formula <- deaths ~ age + sex + time
    mod <- mod_binom(formula = formula,
                    data = data,
                    size = popn)
    ans_obtained <- fit(mod)
    expect_s3_class(ans_obtained, "bage_mod")
})

test_that("'fit' works with valid inputs - norm", {
    data <- expand.grid(age = 0:9, time = 2000:2005, sex = c("F", "M"))
    data[] <- lapply(data, factor)
    data$wt <- rpois(n = nrow(data), lambda = 100)
    data$val <- rnorm(n = nrow(data), mean = (data$sex == "F"))
    formula <- val ~ age + sex + time
    mod <- mod_norm(formula = formula,
                    data = data,
                    weights = wt)
    ans_obtained <- fit(mod)
    expect_s3_class(ans_obtained, "bage_mod")
})

test_that("'fit' works with known intercept and sex effect", {
    set.seed(0)    
    data <- expand.grid(age = 0:9, time = 2000:2005, sex = c("F", "M"))
    data$popn <- rpois(n = nrow(data), lambda = 100)
    data$deaths <- rpois(n = nrow(data), lambda = 10)
    formula <- deaths ~ age + sex + time
    mod <- mod_pois(formula = formula,
                    data = data,
                    exposure = popn)
    mod <- set_prior(mod, `(Intercept)` ~ Known(values = -2))
    mod <- set_prior(mod, sex ~ Known(values = c(-0.1, 0.1)))
    ans_obtained <- fit(mod)
    expect_equal(ans_obtained$est$par[[1L]], -2)
    expect_equal(ans_obtained$est$par[mod$terms_par == "sex"], c(-0.1, 0.1))
})








