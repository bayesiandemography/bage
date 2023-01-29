
## 'get_fun_align_to_data' ----------------------------------------------------

test_that("'get_fun_align_to_data' works with 2 dimensions, 'data' and 'outcome' have same values", {
    outcome <- array(21:32, dim = 3:4, dimnames = list(a = 1:3, b = 1:4))
    data <- as.data.frame.table(outcome)
    data <- data[12:1,]
    mod <- list(data = data, outcome = outcome)
    align_to_data <- get_fun_align_to_data(mod)
    ans_obtained <- align_to_data(as.integer(outcome))
    ans_expected <- 32:21
    expect_identical(ans_obtained, ans_expected)
})

test_that("'get_fun_align_to_data' works with 2 dimensions, 'data' has subset of values in 'outcome'", {
    outcome <- array(1:6, dim = 3:2, dimnames = list(a = 1:3, b = 1:2))
    data <- data.frame(b = c(1, 2, 2, 1), a = c(1, 2, 1, 2))
    mod <- list(data = data, outcome = outcome)
    align_to_data <- get_fun_align_to_data(mod)
    ans_obtained <- align_to_data(as.integer(outcome))
    ans_expected <- c(1L, 5L, 4L, 2L)
    expect_identical(ans_obtained, ans_expected)
})

test_that("'get_fun_align_to_data' works with 3 dimensions, 'data' has subset of values in 'outcome'", {
    outcome <- array(1:12, dim = c(3:2, 2), dimnames = list(a = 1:3, b = 1:2, c = 1:2))
    data <- data.frame(b = c(1, 2, 2, 1), a = c(1, 2, 1, 2), c = c(2, 2, 1, 1))
    mod <- list(data = data, outcome = outcome)
    align_to_data <- get_fun_align_to_data(mod)
    ans_obtained <- align_to_data(as.integer(outcome))
    ans_expected <- c(7L, 11L, 4L, 2L)
    expect_identical(ans_obtained, ans_expected)
})

test_that("'get_fun_align_to_data' works with 1 dimension, 'data' has all values in 'outcome'", {
    outcome <- array(11:14, dim = 4, dimnames = list(a = 1:4))
    data <- data.frame(b = c(1, 2, 2, 1), a = 4:1)
    mod <- list(data = data, outcome = outcome)
    align_to_data <- get_fun_align_to_data(mod)
    ans_obtained <- align_to_data(21:24)
    ans_expected <- 24:21
    expect_identical(ans_obtained, ans_expected)
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
    expect_identical(ncol(ans_obtained), length(mod$terms_par))
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
    expect_identical(dim(draws_par), c(length(mod$terms_par), mod$n_draw))
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
    align_fun <- get_fun_align_to_data(mod)
    fit_exp <- align_fun(exp(linear_pred))
    expect_equal(fitted, fit_exp)
    ## 'make_draws_fitted'
    observed <- make_observed(mod)
    align_fun <- get_fun_align_to_data(mod)
    obs_exp <- align_fun(as.double(mod$outcome / mod$offset))
    expect_equal(observed, obs_exp)
})

test_that("'make_draws_par' works with Known priors", {
    set.seed(0)
    data <- expand.grid(age = 0:9, time = 2000:2005, sex = c("F", "M"))
    data$popn <- rpois(n = nrow(data), lambda = 100)
    data$deaths <- rpois(n = nrow(data), lambda = 10)
    formula <- deaths ~ age + sex
    mod <- mod_pois(formula = formula,
                    data = data,
                    exposure = popn)
    mod <- set_prior(mod, `(Intercept)` ~ Known(-1))
    mod <- set_prior(mod, sex ~ Known(c(-0.1, 0.1)))    
    mod <- fit(mod)
    mod <- set_n_draw(mod, n_draw = 100L)
    set.seed(1)
    ans_obtained <- make_draws_par(mod)
    set.seed(1)
    mean <- mod$est$par[2:11]
    V1 <- mod$prec[1:10, 1:10]
    V2 <- mod$prec[11, 11]
    R <- as.double(mod$prec[11, 1:10])
    prec <- V1 - outer(R, R) / V2
    ans_expected <- rmvn(n = 100, mean = mean, prec = prec)
    ans_expected <- rbind(rep(mod$est$par[1], 100),
                          ans_expected,
                          matrix(mod$est$par[12:13], nr = 2, nc = 100))
    expect_equal(ans_obtained, ans_expected, tolerance = 0.02)
})


## 'make_terms_est' -----------------------------------------------------------

test_that("'make_terms_est' work with valid inputs", {
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


## 'rmvn' ---------------------------------------------------------------------

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
    
