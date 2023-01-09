

## 'get_inv_transform' --------------------------------------------------------

test_that("'get_inv_transform' works with valid inputs", {
    set.seed(0)
    x <- runif(100)
    logit <- function(x) log(x) - log(1 - x)
    expect_equal(get_inv_transform("pois")(log(x)), x)
    expect_equal(get_inv_transform("binom")(logit(x)), x)
    expect_equal(get_inv_transform("norm")(x), x)
})

test_that("'get_inv_transform' gives expected error with invalid input", {
    expect_error(get_inv_transform("wrong"),
                 "invalid value for 'nm_distn' : \"wrong\"")
})


## 'make_terms_est', 'make_terms_std' -----------------------------------------

test_that("'make_terms_est', 'make_terms_std' work with valid inputs", {
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
    ans_obtained <- make_terms_std(mod)
    expect_identical(names(ans_obtained), names(mod$priors))
    expect_identical(as.numeric(unlist(ans_obtained)),
                     as.numeric(unlist(mod$std$par)))
})

## 'make_linear_pred_mean', 'make_linear_pred_std' ----------------------------

test_that("'make_linear_pred_mean', 'make_linear_pred_std' work with valid inputs", {
    set.seed(0)
    data <- expand.grid(age = 0:9, time = 2000:2005, sex = c("F", "M"))
    data$popn <- rpois(n = nrow(data), lambda = 100)
    data$deaths <- rpois(n = nrow(data), lambda = 10)
    formula <- deaths ~ age:sex + time
    mod <- mod_pois(formula = formula,
                    data = data,
                    exposure = popn)
    mod <- fit(mod)
    ans_obtained <- make_linear_pred_mean(mod)
    ans_expected <- mod$est$par[1L] +
        rep(mod$est$par[2:7], each = 20) +
        mod$est$par[8:27]
    expect_identical(ans_obtained, ans_expected)
    ans_obtained <- make_linear_pred_std(mod)
    ans_expected <- sqrt(mod$std$par[1L]^2 +
        rep(mod$std$par[2:7], each = 20)^2 +
        mod$std$par[8:27]^2)
    expect_identical(ans_obtained, ans_expected)
})
    
    


