
## 'get_align_to_data' ------------------------------------------------------

test_that("'get_align_to_data' works with 2 dimensions, 'data' and 'outcome' have same values", {
    outcome <- array(21:32, dim = 3:4, dimnames = list(a = 1:3, b = 1:4))
    data <- as.data.frame.table(outcome)
    data <- data[12:1,]
    mod <- list(data = data, outcome = outcome)
    align_to_data <- get_align_to_data(mod)
    ans_obtained <- align_to_data(as.integer(outcome))
    ans_expected <- 32:21
    expect_identical(ans_obtained, ans_expected)
})

test_that("'get_align_to_data' works with 2 dimensions, 'data' has subset of values in 'outcome'", {
    outcome <- array(1:6, dim = 3:2, dimnames = list(a = 1:3, b = 1:2))
    data <- data.frame(b = c(1, 2, 2, 1), a = c(1, 2, 1, 2))
    mod <- list(data = data, outcome = outcome)
    align_to_data <- get_align_to_data(mod)
    ans_obtained <- align_to_data(as.integer(outcome))
    ans_expected <- c(1L, 5L, 4L, 2L)
    expect_identical(ans_obtained, ans_expected)
})

test_that("'get_align_to_data' works with 3 dimensions, 'data' has subset of values in 'outcome'", {
    outcome <- array(1:12, dim = c(3:2, 2), dimnames = list(a = 1:3, b = 1:2, c = 1:2))
    data <- data.frame(b = c(1, 2, 2, 1), a = c(1, 2, 1, 2), c = c(2, 2, 1, 1))
    mod <- list(data = data, outcome = outcome)
    align_to_data <- get_align_to_data(mod)
    ans_obtained <- align_to_data(as.integer(outcome))
    ans_expected <- c(7L, 11L, 4L, 2L)
    expect_identical(ans_obtained, ans_expected)
})

test_that("'get_align_to_data' works with 1 dimension, 'data' has all values in 'outcome'", {
    outcome <- array(11:14, dim = 4, dimnames = list(a = 1:4))
    data <- data.frame(b = c(1, 2, 2, 1), a = 4:1)
    mod <- list(data = data, outcome = outcome)
    align_to_data <- get_align_to_data(mod)
    ans_obtained <- align_to_data(21:24)
    ans_expected <- 24:21
    expect_identical(ans_obtained, ans_expected)
})


## 'get_inv_transform' --------------------------------------------------------

test_that("'get_inv_transform' works with valid inputs", {
    set.seed(0)
    x <- runif(100)
    logit <- function(x) log(x) - log(1 - x)
    expect_equal(get_inv_transform(list(nm_distn = "pois"))(log(x)), x)
    expect_equal(get_inv_transform(list(nm_distn = "binom"))(logit(x)), x)
    expect_equal(get_inv_transform(list(nm_distn = "norm"))(x), x)
})

test_that("'get_inv_transform' gives expected error with invalid input", {
    expect_error(get_inv_transform(list(nm_distn = "wrong")),
                 "invalid value for 'nm_distn' : \"wrong\"")
})


## 'make_combined_matrix_par' -------------------------------------------------

test_that("'make_combined_matrix_par' works with valid inputs", {
    set.seed(0)
    data <- expand.grid(age = 0:9, time = 2000:2005, sex = c("F", "M"))
    data$popn <- rpois(n = nrow(data), lambda = 100)
    data$deaths <- rpois(n = nrow(data), lambda = 10)
    formula <- deaths ~ age * sex + age * time
    mod <- mod_pois(formula = formula,
                    data = data,
                    exposure = popn)
    ans_obtained <- make_combined_matrix_par(mod)
    expect_identical(nrow(ans_obtained), nrow(data))
    expect_identical(ncol(ans_obtained), length(mod$par))
})


## 'make_draws_par', 'make_draws_linear_pred', 'make_draws_fitted' ------------

test_that("'make_draws_par', 'make_draws_linear_pred', 'make_draws_fitted', 'make_observed' work with valid inputs", {
    set.seed(0)
    data <- expand.grid(age = 0:9, time = 2000:2005, sex = c("F", "M"))
    data$popn <- rpois(n = nrow(data), lambda = 100)
    data$deaths <- rpois(n = nrow(data), lambda = 10)
    formula <- deaths ~ age + sex
    mod <- mod_pois(formula = formula,
                    data = data,
                    exposure = popn)
    mod <- fit(mod)
    mod <- set_n_draw(mod, n_draw = 100L)
    ## 'make_draws_par'
    set.seed(1)
    draws_par <- make_draws_par(mod)
    expect_identical(dim(draws_par), c(length(mod$par), mod$n_draw))
    expect_equal(rowMeans(draws_par), unlist(mod$est$par), tolerance = 0.1)
    ## 'make_draws_linear_pred'
    set.seed(1)
    linear_pred <- make_draws_linear_pred(mod)
    m <- make_combined_matrix_par(mod)
    lp_exp <- matrix(nr = length(mod$outcome), nc = mod$n_draw)
    for (i in seq_len(mod$n_draw))
        lp_exp[,i] <- as.double(m %*% draws_par[,i])
    expect_equal(Matrix::rowMeans(linear_pred), rowMeans(lp_exp))
    ## 'make_draws_fitted'
    set.seed(1)
    fitted <- make_draws_fitted(mod)
    align_fun <- get_align_to_data(mod)
    fit_exp <- align_fun(exp(linear_pred))
    expect_equal(fitted, fit_exp)
    ## 'make_draws_fitted'
    observed <- make_observed(mod)
    align_fun <- get_align_to_data(mod)
    obs_exp <- align_fun(as.double(mod$outcome / mod$offset))
    expect_equal(observed, obs_exp)
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

## ## 'make_linear_pred_mean' ----------------------------------------------------

## test_that("'make_linear_pred_mean' works with valid inputs", {
##     set.seed(0)
##     data <- expand.grid(age = 0:9, time = 2000:2005, sex = c("F", "M"))
##     data$popn <- rpois(n = nrow(data), lambda = 100)
##     data$deaths <- rpois(n = nrow(data), lambda = 10)
##     formula <- deaths ~ age:sex + time
##     mod <- mod_pois(formula = formula,
##                     data = data,
##                     exposure = popn)
##     mod <- fit(mod)
##     ans_obtained <- make_linear_pred_mean(mod)
##     ans_expected <- mod$est$par[1L] +
##         rep(mod$est$par[2:7], each = 20) +
##         mod$est$par[8:27]
##     expect_identical(ans_obtained, ans_expected)
## })


## 'make_fitted_point', 'make_lower_upper' ------------------------------------

## test_that("'make_fitted_point', 'make_lower_upper' work with valid inputs", {
##     set.seed(0)
##     data <- expand.grid(age = 0:9, time = 2000:2005, sex = c("F", "M"))
##     data$popn <- rpois(n = nrow(data), lambda = 100)
##     data$deaths <- rpois(n = nrow(data), lambda = 10)
##     formula <- deaths ~ age:sex + time
##     mod <- mod_pois(formula = formula,
##                     data = data,
##                     exposure = popn)
##     mod <- fit(mod)
##     ## make_fitted_point
##     ans_obtained <- make_fitted_point(mod)
##     log_mean <- mod$est$par[1L] +
##         rep(mod$est$par[2:7], each = 20) +
##         mod$est$par[8:27]
##     log_mean <- as.double(aperm(array(log_mean, dim = c(10, 2, 6)),
##                                 perm = c(1, 3, 2)))
##     ans_expected <- exp(log_mean)
##     expect_identical(ans_obtained, ans_expected)
##     ## make_lower_upper
##     ans_obtained <- make_lower_upper(mod, interval = 0.9)
##     log_sd <- sqrt(mod$std$par[1L]^2 +
##                    rep(mod$std$par[2:7], each = 20)^2 +
##                    mod$std$par[8:27]^2)
##     log_sd <- as.double(aperm(array(log_sd, dim = c(10, 2, 6)),
##                               perm = c(1, 3, 2)))
##     lower_tr <- qnorm(0.05, mean = log_mean, sd = log_sd)
##     upper_tr <- qnorm(0.95, mean = log_mean, sd = log_sd)
##     ans_expected <- list(lower = exp(lower_tr),
##                          upper = exp(upper_tr))
##     expect_identical(ans_obtained, ans_expected)
## })


test_that("'rmvn' gives correct answer with valid inputs", {
    set.seed(0)
    mean <- rnorm(3)
    prec <- matrix(runif(n = 9, max = 10), nr = 3)
    prec <- t(prec) %*% prec
    ans <- rmvn(n = 100000,
                mean = mean,
                prec = prec)
    expect_equal(rowMeans(ans), mean, tolerance = 0.01)
    expect_equal(solve(cov(t(ans))), prec, tolerance = 0.01)
})
    
