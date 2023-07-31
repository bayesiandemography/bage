
## 'components_par', 'components_hyper', 'components_season' ------------------

test_that("'components_par', 'components_hyper', 'components_season' work with valid data", {
    set.seed(0)
    data <- expand.grid(age = 0:9, time = 2000:2005, sex = c("F", "M"))
    data$popn <- rpois(n = nrow(data), lambda = 100)
    data$deaths <- rpois(n = nrow(data), lambda = 10)
    formula <- deaths ~ age + sex:time + time
    mod <- mod_pois(formula = formula,
                    data = data,
                    exposure = popn)
    mod <- set_prior(mod, time ~ AR1())
    mod <- set_season(mod, n = 3)
    mod <- fit(mod)
    ## components_par
    ans_par <- components_par(mod)
    expect_true(is.data.frame(ans_par))
    ## components_hyper
    ans_hyper <- components_hyper(mod)
    expect_true(is.data.frame(ans_hyper))
    expect_identical(nrow(ans_hyper), length(mod$est$hyper))
    ## components_season
    ans_season <- components_season(mod)
    expect_true(is.data.frame(ans_season))
    expect_identical(nrow(ans_season),
                     length(mod$est$par_season) + length(mod$est$hyper_season))
})


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


## 'make_combined_matrix_par_outcome' -----------------------------------------

test_that("'make_combined_matrix_parfree_outcome' works with valid inputs", {
    set.seed(0)
    data <- expand.grid(age = 0:9, time = 2000:2005, sex = c("F", "M"))
    data$popn <- rpois(n = nrow(data), lambda = 100)
    data$deaths <- rpois(n = nrow(data), lambda = 10)
    formula <- deaths ~ age * sex + age * time
    mod <- mod_pois(formula = formula,
                    data = data,
                    exposure = popn)
    ans_obtained <- make_combined_matrix_par_outcome(mod)
    expect_identical(nrow(ans_obtained), nrow(data))
    expect_identical(ncol(ans_obtained), length(make_terms_par(mod)))
})


## 'make_combined_matrix_parfree_par' -----------------------------------------

test_that("'make_combined_matrix_parfree_par' works with valid inputs", {
    set.seed(0)
    data <- expand.grid(age = 0:9, time = 2000:2005, sex = c("F", "M"))
    data$popn <- rpois(n = nrow(data), lambda = 100)
    data$deaths <- rpois(n = nrow(data), lambda = 10)
    formula <- deaths ~ age * sex + age * time
    mod <- mod_pois(formula = formula,
                    data = data,
                    exposure = popn)
    ans_obtained <- make_combined_matrix_parfree_par(mod)
    expect_identical(nrow(ans_obtained), length(make_terms_par(mod)))
    expect_identical(ncol(ans_obtained), length(make_terms_parfree(mod)))
})


## 'make_draws_linear_pred' ---------------------------------------------------

test_that("'make_draws_linear_pred' works with valid inputs", {
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
    set.seed(1)
    linear_pred <- make_draws_linear_pred(mod)
    m <- make_combined_matrix_par_outcome(mod)
    lp_exp <- matrix(nr = length(mod$outcome), nc = mod$n_draw)
    set.seed(1)
    draws_par <- make_draws_par(mod)
    for (i in seq_len(mod$n_draw))
        lp_exp[,i] <- as.double(m %*% draws_par[,i])
    expect_equal(Matrix::rowMeans(linear_pred), rowMeans(lp_exp))
})


## 'make_draws_fitted' --------------------------------------------------------

test_that("'make_draws_fitted' works", {
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
    set.seed(1)
    linear_pred <- make_draws_linear_pred(mod)
    set.seed(1)
    fitted <- make_draws_fitted(mod)
    align_fun <- get_fun_align_to_data(mod)
    fit_exp <- align_fun(exp(linear_pred))
    expect_equal(fitted, fit_exp)
})


## 'make_draws_hyper' ---------------------------------------------------------

test_that("'make_draws_hyper' works", {
    set.seed(0)
    data <- expand.grid(age = 0:9, time = 2000:2005, sex = c("F", "M"))
    data$popn <- rpois(n = nrow(data), lambda = 100)
    data$deaths <- rpois(n = nrow(data), lambda = 10)
    formula <- deaths ~ age + sex
    mod <- mod_pois(formula = formula,
                    data = data,
                    exposure = popn)
    mod <- fit(mod)
    set.seed(1)
    draws <- make_draws_hyper(mod)
    expect_identical(length(draws), length(make_terms_hyper(mod)))
    expect_identical(length(draws[[1L]]), mod$n_draw)
    expect_equal(sapply(draws, median), exp(mod$est$hyper), tolerance = 0.01)
})


## 'make_draws_par' -----------------------------------------------------------

test_that("'make_draws_par' works", {
    set.seed(0)
    data <- expand.grid(age = 0:9, time = 2000:2005, sex = c("F", "M"))
    data$popn <- rpois(n = nrow(data), lambda = 100)
    data$deaths <- rpois(n = nrow(data), lambda = 10)
    formula <- deaths ~ age + sex
    mod <- mod_pois(formula = formula,
                    data = data,
                    exposure = popn)
    mod <- fit(mod)
    mod <- set_n_draw(mod, n_draw = 10L)
    set.seed(1)
    draws <- make_draws_par(mod)
    expect_identical(dim(draws), c(length(make_terms_par(mod)), mod$n_draw))
})


## 'make_draws_parfree' -------------------------------------------------------

test_that("'make_draws_parfree' works - ordinary priors", {
    set.seed(0)
    data <- expand.grid(age = 0:9, time = 2000:2005, sex = c("F", "M"))
    data$popn <- rpois(n = nrow(data), lambda = 100)
    data$deaths <- rpois(n = nrow(data), lambda = 10)
    formula <- deaths ~ age + sex
    mod <- mod_pois(formula = formula,
                    data = data,
                    exposure = popn)
    mod <- fit(mod)
    mod <- set_n_draw(mod, n_draw = 10000L)
    set.seed(1)
    draws <- make_draws_parfree(mod)
    expect_identical(dim(draws), c(length(make_terms_parfree(mod)), mod$n_draw))
    expect_equal(rowMeans(draws), unname(unlist(mod$est$parfree)), tolerance = 0.1)
})

test_that("'make_draws_parfree' works - has Known priors", {
    set.seed(0)
    data <- expand.grid(age = 0:9, time = 2000:2005, sex = c("F", "M"))
    data$popn <- rpois(n = nrow(data), lambda = 100)
    data$deaths <- rpois(n = nrow(data), lambda = 10)
    formula <- deaths ~ age + sex
    mod <- mod_pois(formula = formula,
                    data = data,
                    exposure = popn)
    mod <- set_prior(mod, `(Intercept)` ~ Known(2))
    mod <- set_prior(mod, sex ~ Known(c(-0.1, 0.1)))
    mod <- fit(mod)
    mod <- set_n_draw(mod, n_draw = 10000L)
    set.seed(1)
    draws <- make_draws_parfree(mod)
    expect_identical(dim(draws), c(length(make_terms_parfree(mod)), mod$n_draw))
    expect_equal(rowMeans(draws), unname(unlist(mod$est$parfree)), tolerance = 0.1)
})


## 'make_draws_season' ---------------------------------------------------------

test_that("'make_draws_season' works", {
    set.seed(0)
    data <- expand.grid(age = 0:9, time = 2000:2005, sex = c("F", "M"))
    data$popn <- rpois(n = nrow(data), lambda = 100)
    data$deaths <- rpois(n = nrow(data), lambda = 10)
    formula <- deaths ~ age + sex + time
    mod <- mod_pois(formula = formula,
                    data = data,
                    exposure = popn)
    mod <- set_season(mod, n = 2)
    mod <- fit(mod)
    set.seed(1)
    draws <- make_draws_season(mod)
    expect_identical(length(draws), length(make_terms_season(mod)))
    expect_identical(length(draws[[1L]]), mod$n_draw)
    expect_equal(sapply(draws[1:6], mean), mod$est$par_season, tolerance = 0.05)
    expect_equal(median(draws[[7]]), exp(mod$est$hyper_season), tolerance = 0.05)
})


## 'make_levels_hyper' ----------------------------------------------------------

test_that("'make_levels_hyper' works", {
    set.seed(0)
    data <- expand.grid(age = 0:9, time = 2000:2005, sex = c("F", "M"))
    data$popn <- rpois(n = nrow(data), lambda = 100)
    data$deaths <- rpois(n = nrow(data), lambda = 10)
    formula <- deaths ~ age * sex + time
    mod <- mod_pois(formula = formula,
                    data = data,
                    exposure = popn)
    mod <- fit(mod)
    ans_obtained <- make_levels_hyper(mod)
    ans_expected <- c("sd", "sd", "sd", "sd")
    expect_identical(ans_obtained, ans_expected)                      
})


## 'make_levels_season' -------------------------------------------------------

test_that("'make_levels_season' works", {
    set.seed(0)
    data <- expand.grid(age = 0:9, time = 2000:2005, sex = c("F", "M"))
    data$popn <- rpois(n = nrow(data), lambda = 100)
    data$deaths <- rpois(n = nrow(data), lambda = 10)
    formula <- deaths ~ age * sex + time
    mod <- mod_pois(formula = formula,
                    data = data,
                    exposure = popn)
    mod <- set_season(mod, n = 2)
    mod <- fit(mod)
    ans_obtained <- make_levels_season(mod)
    ans_expected <- c(2000:2005, "sd")
    expect_identical(ans_obtained, ans_expected)                      
})


## 'make_observed' ------------------------------------------------------------

test_that("'make_observed' works", {
    set.seed(0)
    data <- expand.grid(age = 0:9, time = 2000:2005, sex = c("F", "M"))
    data$popn <- rpois(n = nrow(data), lambda = 100)
    data$deaths <- rpois(n = nrow(data), lambda = 10)
    formula <- deaths ~ age + sex
    mod <- mod_pois(formula = formula,
                    data = data,
                    exposure = popn)
    ans_obtained <- make_observed(mod)
    align_fun <- get_fun_align_to_data(mod)
    ans_expected <- align_fun(as.double(mod$outcome / mod$offset))
    expect_equal(ans_obtained, ans_expected)
})


## 'make_terms_season' --------------------------------------------------------

test_that("'make_terms_season' works with models with seasonal effect", {
    set.seed(0)
    data <- expand.grid(age = 0:9, time = 2000:2005, sex = c("F", "M"))
    data$popn <- rpois(n = nrow(data), lambda = 100)
    data$income <- rnorm(n = nrow(data))
    formula <- income ~ age * sex + time
    mod <- mod_norm(formula = formula,
                    data = data,
                    weights = popn)
    mod <- set_season(mod, n = 2)
    mod <- fit(mod)
    ans_obtained <- make_terms_season(mod)
    ans_expected <- factor(c("par", "par", "par", "par", "par", "par", "hyper"),
                           levels = c("par", "hyper"))
    expect_identical(ans_obtained, ans_expected)                      
})

test_that("'make_terms_season' works with models without seasonal effect", {
    set.seed(0)
    data <- expand.grid(age = 0:9, time = 2000:2005, sex = c("F", "M"))
    data$popn <- rpois(n = nrow(data), lambda = 100)
    data$income <- rnorm(n = nrow(data))
    formula <- income ~ age * sex + time
    mod <- mod_norm(formula = formula,
                    data = data,
                    weights = popn)
    mod <- fit(mod)
    ans_obtained <- make_terms_season(mod)
    ans_expected <- factor()
    expect_identical(ans_obtained, ans_expected)                      
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
    
