
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


## 'make_fitted_point', 'make_lower_upper' ------------------------------------

test_that("'make_fitted_point', 'make_lower_upper' work with valid inputs", {
    set.seed(0)
    data <- expand.grid(age = 0:9, time = 2000:2005, sex = c("F", "M"))
    data$popn <- rpois(n = nrow(data), lambda = 100)
    data$deaths <- rpois(n = nrow(data), lambda = 10)
    formula <- deaths ~ age:sex + time
    mod <- mod_pois(formula = formula,
                    data = data,
                    exposure = popn)
    mod <- fit(mod)
    ## make_fitted_point
    ans_obtained <- make_fitted_point(mod)
    log_mean <- mod$est$par[1L] +
        rep(mod$est$par[2:7], each = 20) +
        mod$est$par[8:27]
    log_mean <- as.double(aperm(array(log_mean, dim = c(10, 2, 6)),
                                perm = c(1, 3, 2)))
    ans_expected <- exp(log_mean)
    expect_identical(ans_obtained, ans_expected)
    ## make_lower_upper
    ans_obtained <- make_lower_upper(mod, interval = 0.9)
    log_sd <- sqrt(mod$std$par[1L]^2 +
                   rep(mod$std$par[2:7], each = 20)^2 +
                   mod$std$par[8:27]^2)
    log_sd <- as.double(aperm(array(log_sd, dim = c(10, 2, 6)),
                              perm = c(1, 3, 2)))
    lower_tr <- qnorm(0.05, mean = log_mean, sd = log_sd)
    upper_tr <- qnorm(0.95, mean = log_mean, sd = log_sd)
    ans_expected <- list(lower = exp(lower_tr),
                         upper = exp(upper_tr))
    expect_identical(ans_obtained, ans_expected)
})


## 'mapping_array_to_df' ------------------------------------------------------

test_that("'mapping_array_to_df' works with 2 dimensions, 'df' and 'a' have same values", {
    a <- array(21:32, dim = 3:4, dimnames = list(a = 1:3, b = 1:4))
    df <- as.data.frame.table(a)
    df <- df[12:1,]
    ans_obtained <- mapping_array_to_df(a = a, df = df)
    ans_expected <- 12:1
    expect_identical(ans_obtained, ans_expected)
})

test_that("'mapping_array_to_df' works with 2 dimensions, 'df' has subset of values in 'a'", {
    a <- array(1:6, dim = 3:2, dimnames = list(a = 1:3, b = 1:2))
    df <- data.frame(b = c(1, 2, 2, 1), a = c(1, 2, 1, 2))
    ans_obtained <- mapping_array_to_df(a = a, df = df)
    ans_expected <- c(1L, 5L, 4L, 2L)
    expect_identical(ans_obtained, ans_expected)
})

test_that("'mapping_array_to_df' works with 3 dimensions, 'df' has subset of values in 'a'", {
    a <- array(1:12, dim = c(3:2, 2), dimnames = list(a = 1:3, b = 1:2, c = 1:2))
    df <- data.frame(b = c(1, 2, 2, 1), a = c(1, 2, 1, 2), c = c(2, 2, 1, 1))
    ans_obtained <- mapping_array_to_df(a = a, df = df)
    ans_expected <- c(7L, 11L, 4L, 2L)
    expect_identical(ans_obtained, ans_expected)
})

test_that("'mapping_array_to_df' works with 1 dimension, 'df' has all values in 'a'", {
    a <- array(11:14, dim = 4, dimnames = list(a = 1:4))
    df <- data.frame(b = c(1, 2, 2, 1), a = 4:1)
    ans_obtained <- mapping_array_to_df(a = a, df = df)
    ans_expected <- 4:1
    expect_identical(ans_obtained, ans_expected)
})

    
    


