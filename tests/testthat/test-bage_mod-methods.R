
## 'augment' ---------------------------------------------------------------

test_that("'augment' works with Poisson, disp", {
    set.seed(0)
    data <- expand.grid(age = 0:9, time = 2000:2005, sex = c("F", "M"),
                        KEEP.OUT.ATTRS = FALSE)
    data$popn <- rpois(n = nrow(data), lambda = 100)
    data$deaths <- rpois(n = nrow(data), lambda = 10)
    formula <- deaths ~ age + sex + time
    mod <- mod_pois(formula = formula,
                    data = data,
                    exposure = popn)
    expect_identical(augment(mod), tibble(data, .observed = make_observed(mod)))
    mod_fitted <- fit(mod)
    ans <- augment(mod_fitted)
    expect_true(is.data.frame(ans))
    expect_identical(names(ans),
                     c(names(data), c(".observed", ".fitted", ".expected")))
})

test_that("'augment' works with binomial, no disp", {
    set.seed(0)
    data <- expand.grid(age = 0:9, time = 2000:2005, sex = c("F", "M"),
                        KEEP.OUT.ATTRS = FALSE)
    data$popn <- rpois(n = nrow(data), lambda = 100)
    data$deaths <- rbinom(n = nrow(data), size = data$popn, prob = 0.5)
    formula <- deaths ~ age + sex + time
    mod <- mod_pois(formula = formula,
                    data = data,
                    exposure = popn)
    mod <- set_disp(mod, s = 0)
    expect_identical(augment(mod), tibble(data, .observed = make_observed(mod)))
    mod_fitted <- fit(mod)
    ans <- augment(mod_fitted)
    expect_true(is.data.frame(ans))
    expect_identical(names(ans),
                     c(names(data), c(".observed", ".fitted")))
})

test_that("'augment' works with normal", {
    set.seed(0)
    data <- expand.grid(age = 0:9, time = 2000:2005, sex = c("F", "M"),
                        KEEP.OUT.ATTRS = FALSE)
    data$deaths <- rpois(n = nrow(data), lambda = 100)
    formula <- deaths ~ age + sex + time
    mod <- mod_norm(formula = formula,
                    data = data,
                    weights = 1)
    expect_identical(augment(mod), tibble(data))
    mod_fitted <- fit(mod)
    ans <- augment(mod_fitted)
    expect_true(is.data.frame(ans))
    expect_identical(names(ans),
                     c(names(data), ".fitted"))
})

test_that("'augment' gives same answer when run twice", {
    set.seed(0)
    data <- expand.grid(age = 0:9, time = 2000:2005, sex = c("F", "M"),
                        KEEP.OUT.ATTRS = FALSE)
    data$popn <- rpois(n = nrow(data), lambda = 100)
    data$deaths <- rbinom(n = nrow(data), size = data$popn, prob = 0.5)
    formula <- deaths ~ age + sex + time
    mod <- mod_pois(formula = formula,
                    data = data,
                    exposure = popn)
    mod_fitted <- fit(mod)
    ans1 <- augment(mod_fitted)
    ans2 <- augment(mod_fitted)
    expect_identical(ans1, ans2)
})


## 'components' ---------------------------------------------------------------

test_that("'components' works with no disp", {
    set.seed(0)
    data <- expand.grid(age = 0:9, time = 2000:2005, sex = c("F", "M"))
    data$popn <- rpois(n = nrow(data), lambda = 100)
    data$deaths <- rpois(n = nrow(data), lambda = 10)
    formula <- deaths ~ age + sex + time
    mod <- mod_pois(formula = formula,
                    data = data,
                    exposure = popn)
    mod <- set_disp(mod, s = 0)
    expect_identical(components(mod), NULL)
    mod_fitted <- fit(mod)
    ans <- components(mod_fitted)
    expect_true(is.data.frame(ans))
    expect_identical(unique(ans$component), c("effect", "hyper"))
})

test_that("'components' gives same answer when run twice", {
    set.seed(0)
    data <- expand.grid(age = 0:9, time = 2000:2005, sex = c("F", "M"))
    data$popn <- rpois(n = nrow(data), lambda = 100)
    data$deaths <- rpois(n = nrow(data), lambda = 10)
    formula <- deaths ~ age + sex + time
    mod <- mod_pois(formula = formula,
                    data = data,
                    exposure = popn)
    mod <- set_disp(mod, s = 0)
    expect_identical(components(mod), NULL)
    mod_fitted <- fit(mod)
    ans1 <- components(mod_fitted)
    ans2 <- components(mod_fitted)
    expect_identical(ans1, ans2)
})

test_that("'components' works with compose_time", {
    set.seed(0)
    data <- expand.grid(age = 0:9, time = 2000:2005, sex = c("F", "M"))
    data$popn <- rpois(n = nrow(data), lambda = 100)
    data$deaths <- rpois(n = nrow(data), lambda = 10)
    formula <- deaths ~ age + sex + time
    mod <- mod_pois(formula = formula,
                    data = data,
                    exposure = popn)
    mod <- set_prior(mod, time ~ compose_time(trend = Lin(), error = N()))
    expect_identical(components(mod), NULL)
    mod_fitted <- fit(mod)
    ans <- components(mod_fitted)
    expect_true(is.data.frame(ans))
    expect_identical(unique(ans$component), c("effect", "hyper", "trend", "error", "disp"))
})


## ## 'draw_vals_par' -----------------------------------------------------------

## test_that("'draw_vals_par' works with 'bage_mod_pois'", {
##     set.seed(0)
##     data <- expand.grid(age = 0:9, time = 2000:2005, sex = c("F", "M"))
##     data$popn <- rpois(n = nrow(data), lambda = 100)
##     data$deaths <- rpois(n = nrow(data), lambda = 10)
##     formula <- deaths ~ age + sex + time
##     mod <- mod_pois(formula = formula,
##                     data = data,
##                     exposure = popn)
##     mod <- set_prior(mod, (Intercept) ~ NFix(0.01))
##     mod <- set_prior(mod, age ~ RW(s = 0.1))
##     mod <- set_prior(mod, sex ~ RW(s = 0.1))
##     mod <- set_prior(mod, time ~ RW(s = 0.1))
##     mod <- set_disp(mod, s = 10)
##     vals_hyperparam <- draw_vals_hyperparam(mod, n_sim = 1000)
##     ans <- draw_vals_par(mod,
##                          vals_meanpar = exp(vals_hyperparam$linpred),
##                          vals_disp = vals_hyperparam$disp)
##     expect_identical(dim(ans), dim(vals_hyperparam$linpred))
##     expect_true(all(ans >= 0))
## })

## test_that("'draw_vals_par' works with 'bage_mod_binom'", {
##     set.seed(0)
##     data <- expand.grid(age = 0:9, time = 2000:2005, sex = c("F", "M"))
##     data$popn <- rpois(n = nrow(data), lambda = 100)
##     data$deaths <- rbinom(n = nrow(data), size = data$popn, prob = 0.3)
##     formula <- deaths ~ age + sex + time
##     mod <- mod_binom(formula = formula,
##                      data = data,
##                      size = popn)
##     mod <- set_prior(mod, (Intercept) ~ NFix(0.01))
##     mod <- set_prior(mod, age ~ RW(s = 0.1))
##     mod <- set_prior(mod, sex ~ RW(s = 0.1))
##     mod <- set_prior(mod, time ~ RW(s = 0.1))
##     mod <- set_disp(mod, s = 10)
##     vals_hyperparam <- draw_vals_hyperparam(mod, n_sim = 1000)
##     logit <- function(x) 1 / (1 + exp(-x))
##     ans <- draw_vals_par(mod,
##                          vals_meanpar = logit(vals_hyperparam$linpred),
##                          vals_disp = vals_hyperparam$disp)
##     expect_identical(dim(ans), dim(vals_hyperparam$linpred))
##     expect_true(all(ans <= 1))
##     expect_true(all(ans >= 0))
## })


## 'draw_vals_disp' -----------------------------------------------------------

test_that("'draw_vals_disp' works with 'bage_mod_pois'", {
    set.seed(0)
    data <- expand.grid(age = 0:9, time = 2000:2005, sex = c("F", "M"))
    data$popn <- rpois(n = nrow(data), lambda = 100)
    data$deaths <- rpois(n = nrow(data), lambda = 10)
    formula <- deaths ~ age + sex + time
    mod <- mod_pois(formula = formula,
                    data = data,
                    exposure = popn)
    ans <- draw_vals_disp(mod, n_sim = 10000)
    expect_equal(median(ans), log(0.5)^2, tolerance = 0.01)
})

test_that("'draw_vals_disp' works with 'bage_mod_norm'", {
    set.seed(0)
    data <- expand.grid(age = 0:9, time = 2000:2005, sex = c("F", "M"))
    data$popn <- rpois(n = nrow(data), lambda = 100)
    data$deaths <- rpois(n = nrow(data), lambda = 10)
    formula <- deaths ~ age + sex + time
    mod <- mod_norm(formula = formula,
                    data = data,
                    weights = 1)
    ans <- draw_vals_disp(mod, n_sim = 10000)
    expect_equal(median(ans), qexp(0.5), tolerance = 0.01)
})


## ## 'draw_vals_mod' -----------------------------------------------------------

## test_that("'draw_vals_mod' works with 'bage_mod_pois'", {
##     set.seed(0)
##     data <- expand.grid(age = 0:9, time = 2000:2005, sex = c("F", "M"))
##     data$popn <- rpois(n = nrow(data), lambda = 100)
##     data$deaths <- rpois(n = nrow(data), lambda = 10)
##     formula <- deaths ~ age + sex + time
##     mod <- mod_pois(formula = formula,
##                     data = data,
##                     exposure = popn)
##     mod <- set_season(mod, n = 2)
##     ans <- draw_vals_mod(mod, n_sim = 10)
##     expect_identical(names(ans),
##                      c("effect", "hyper", "disp", "cyclical", "season", 
##                        "par", "outcome"))
## })

## test_that("'draw_vals_mod' works with 'bage_mod_pois' - outcome different from data ", {
##     set.seed(0)
##     data <- expand.grid(age = 0:9, time = 2000:2005, sex = c("F", "M"), region = c("a", "b"))
##     data$popn <- rpois(n = nrow(data), lambda = 100)
##     data$deaths <- rpois(n = nrow(data), lambda = 10)
##     formula <- deaths ~ age + sex + time
##     mod <- mod_pois(formula = formula,
##                     data = data,
##                     exposure = popn)
##     mod <- set_season(mod, n = 2)
##     ans <- draw_vals_mod(mod, n_sim = 10)
##     expect_identical(names(ans),
##                      c("effect", "hyper", "disp", "cyclical", "season", 
##                        "par", "outcome"))
##     expect_identical(nrow(ans$outcome), nrow(data))
## })

## test_that("'draw_vals_mod' works with 'bage_mod_binom'", {
##     set.seed(0)
##     data <- expand.grid(age = 0:9, time = 2000:2005, sex = c("F", "M"))
##     data$popn <- rpois(n = nrow(data), lambda = 100)
##     data$deaths <- rbinom(n = nrow(data), size = data$popn, prob = 0.3)
##     formula <- deaths ~ age + sex + time
##     mod <- mod_binom(formula = formula,
##                     data = data,
##                     size = popn)
##     mod <- set_season(mod, n = 2)
##     ans <- draw_vals_mod(mod, n_sim = 10)
##     expect_identical(names(ans),
##                      c("effect", "hyper", "disp", "cyclical", "season", 
##                        "par", "outcome"))
##     expect_identical(nrow(ans$outcome), nrow(data))
## })

## test_that("'draw_vals_mod' works with 'bage_mod_binom' - outcome different from data", {
##     set.seed(0)
##     data <- expand.grid(age = 0:9, time = 2000:2005, sex = c("F", "M"), region = c("a", "b"))
##     data$popn <- rpois(n = nrow(data), lambda = 100)
##     data$deaths <- rbinom(n = nrow(data), size = data$popn, prob = 0.3)
##     formula <- deaths ~ age + sex + time
##     mod <- mod_binom(formula = formula,
##                     data = data,
##                     size = popn)
##     mod <- set_season(mod, n = 2)
##     ans <- draw_vals_mod(mod, n_sim = 10)
##     expect_identical(names(ans),
##                      c("effect", "hyper", "disp", "cyclical", "season", 
##                        "par", "outcome"))
##     expect_identical(nrow(ans$outcome), nrow(data))
## })

## test_that("'draw_vals_mod' works with 'bage_mod_norm'", {
##     set.seed(0)
##     data <- expand.grid(age = 0:9, time = 2000:2005, sex = c("F", "M"))
##     data$income <- rnorm(n = nrow(data), mean = 100, sd = 5)
##     data$wt <- runif(n = nrow(data), min = 0.1)
##     formula <- income ~ age + sex + time
##     mod <- mod_norm(formula = formula,
##                     data = data,
##                     weights = wt)
##     mod <- set_season(mod, n = 2)
##     ans <- draw_vals_mod(mod, n_sim = 10)
##     expect_identical(names(ans),
##                      c("effect", "hyper", "disp", "cyclical", "season",
##                        "par", "outcome"))
##     expect_identical(nrow(ans$outcome), nrow(data))
## })

## test_that("'draw_vals_mod' works with 'bage_mod_norm' - some rows of data missing", {
##     set.seed(0)
##     data <- expand.grid(age = 0:9, time = 2000:2005, sex = c("F", "M"))
##     data <- data[-c(5, 10),]
##     data$income <- rnorm(n = nrow(data), mean = 100, sd = 5)
##     data$wt <- runif(n = nrow(data), min = 0.1)
##     formula <- income ~ age + sex + time
##     mod <- mod_norm(formula = formula,
##                     data = data,
##                     weights = wt)
##     mod <- set_season(mod, n = 2)
##     ans <- draw_vals_mod(mod, n_sim = 10)
##     expect_identical(names(ans),
##                      c("effect", "hyper", "disp", "cyclical", "season",
##                        "par", "outcome"))
##     expect_identical(nrow(ans$outcome), nrow(data))
## })


## 'fit' -----------------------------------------------------------------

test_that("'fit' works with valid inputs - pois has exposure", {
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


test_that("'fit' works with valid inputs - pois has exposure", {
    set.seed(0)
    data <- expand.grid(age = 0:9, time = 2000:2005, sex = c("F", "M"))
    data$deaths <- rpois(n = nrow(data), lambda = 10000)
    formula <- deaths ~ age + sex + time
    mod <- mod_pois(formula = formula,
                    data = data,
                    exposure = 1)
    mod <- set_prior(mod, age ~ RW())
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
    expect_equal(ans_obtained$est$effectfree[[1L]], -2)
    expect_equal(ans_obtained$est$effectfree[names(ans_obtained$est$effectfree) == "sex"], c(sex = -0.1, sex = 0.1))
})

test_that("'fit' works with AR1", {
    set.seed(0)
    data <- expand.grid(age = 0:4, time = 2000:2005, sex = c("F", "M"))
    data$popn <- rpois(n = nrow(data), lambda = 100)
    data$deaths <- rpois(n = nrow(data), lambda = 10)
    formula <- deaths ~ age:sex + age:time + time
    mod <- mod_pois(formula = formula,
                    data = data,
                    exposure = popn)
    mod <- set_prior(mod, time ~ AR1())
    ans_obtained <- fit(mod)
    expect_s3_class(ans_obtained, "bage_mod")
})

test_that("'fit' gives the same imputed rate when outcome is NA and offset is NA", {
    set.seed(0)
    data <- expand.grid(age = 0:4, time = 2000:2002, sex = c("F", "M"))
    data$popn <- rpois(n = nrow(data), lambda = 100)
    data$deaths <- rpois(n = nrow(data), lambda = 10)
    formula <- deaths ~ age:sex + age:time + time
    ## outcome NA
    set.seed(0)
    data_outcome <- data
    data_outcome$deaths[5] <- NA
    mod_outcome <- mod_pois(formula = formula,
                            data = data_outcome,
                            exposure = popn)
    mod_outcome <- fit(mod_outcome)
    ans_outcome <- augment(mod_outcome)$.fitted[5]
    ## offset NA
    set.seed(0)
    data_offset <- data
    data_offset$popn[5] <- NA
    mod_offset <- mod_pois(formula = formula,
                           data = data_offset,
                           exposure = popn)
    mod_offset <- fit(mod_offset)
    ans_offset <- augment(mod_offset)$.fitted[5]
    ## compare
    expect_equal(ans_outcome, ans_offset)
})

test_that("'fit' works when all observed values for one year are NA", {
    data <- data.frame(deaths = c(NA, 2:10),
                       age = rep(1:2, each = 5),
                       time = 2001:2010)
    mod <- mod_pois(deaths ~ age + time,
                    data = data,
                    exposure = 1)
    mod_fitted <- fit(mod)
    expect_false(is.null(mod_fitted$est))
})

test_that("'fit' works when single dimension", {
    data <- data.frame(deaths = 1:10,
                       time = 2001:2010)
    mod <- mod_pois(deaths ~ time,
                    data = data,
                    exposure = 1)
    mod_fitted <- fit(mod)
    expect_identical(length(mod_fitted$est$effectfree), nrow(data) + 1L)
})

test_that("'fit' works with SVD", {
    set.seed(0)
    data <- expand.grid(age = poputils::age_labels(type = "five", max = 60),
                        time = 2000:2005,
                        sex = c("Female", "Male"))
    data$popn <- rpois(n = nrow(data), lambda = 100)
    data$deaths <- rpois(n = nrow(data), lambda = 10)
    formula <- deaths ~ age:sex + age:time + time
    mod <- mod_pois(formula = formula,
                    data = data,
                    exposure = popn)
    mod <- set_prior(mod, age:sex ~ SVD(HMD))
    ans_obtained <- fit(mod)
    expect_s3_class(ans_obtained, "bage_mod")
})

test_that("'fit' works with Lin", {
    set.seed(0)
    data <- expand.grid(age = 0:4, time = 2000:2005, sex = c("F", "M"))
    data$popn <- rpois(n = nrow(data), lambda = 100)
    data$deaths <- rpois(n = nrow(data), lambda = 10)
    formula <- deaths ~ sex + age + time
    mod <- mod_pois(formula = formula,
                    data = data,
                    exposure = popn)
    mod <- set_prior(mod, time ~ Lin())
    ans_obtained <- fit(mod)
    expect_s3_class(ans_obtained, "bage_mod")
})

test_that("'fit' works with ELin", {
    set.seed(0)
    data <- expand.grid(age = 0:4, time = 2000:2005, sex = c("F", "M"))
    data$popn <- rpois(n = nrow(data), lambda = 100)
    data$deaths <- rpois(n = nrow(data), lambda = 10)
    formula <- deaths ~ sex * time + age
    mod <- mod_pois(formula = formula,
                    data = data,
                    exposure = popn)
    mod <- set_prior(mod, sex:time ~ ELin())
    ans_obtained <- fit(mod)
    expect_s3_class(ans_obtained, "bage_mod")
})

test_that("'fit' works with ERW", {
    set.seed(0)
    data <- expand.grid(age = 0:4, time = 2000:2005, sex = c("F", "M"))
    data$popn <- rpois(n = nrow(data), lambda = 100)
    data$deaths <- rpois(n = nrow(data), lambda = 10)
    formula <- deaths ~ sex * time + age
    mod <- mod_pois(formula = formula,
                    data = data,
                    exposure = popn)
    mod <- set_prior(mod, sex:time ~ compose_time(ERW(), error = N()))
    ans_obtained <- fit(mod)
    expect_s3_class(ans_obtained, "bage_mod")
})

test_that("'fit' works with compose_time", {
    set.seed(0)
    data <- expand.grid(age = 0:4, time = 2000:2009, sex = c("F", "M"))
    data$popn <- rpois(n = nrow(data), lambda = 100)
    data$deaths <- rpois(n = nrow(data), lambda = 10)
    formula <- deaths ~ sex * time + age
    mod <- mod_pois(formula = formula,
                    data = data,
                    exposure = popn)
    mod <- set_prior(mod, time ~ compose_time(trend = Lin(), error = N()))
    ans_obtained <- fit(mod)
    expect_s3_class(ans_obtained, "bage_mod")
})

test_that("'fit' works with no hyper", {
    set.seed(0)
    data <- expand.grid(age = 0:4, time = 2000:2005, sex = c("F", "M"))
    data$popn <- rpois(n = nrow(data), lambda = 100)
    data$deaths <- rpois(n = nrow(data), lambda = 10)
    formula <- deaths ~ sex
    mod <- mod_pois(formula = formula,
                    data = data,
                    exposure = popn)
    mod <- set_prior(mod, sex ~ NFix())
    ans_obtained <- fit(mod)
    expect_s3_class(ans_obtained, "bage_mod")
})

test_that("'fit' works with AR", {
    set.seed(0)
    data <- expand.grid(age = 0:4, time = 2000:2005, sex = c("F", "M"))
    data$popn <- rpois(n = nrow(data), lambda = 100)
    data$deaths <- rpois(n = nrow(data), lambda = 10)
    formula <- deaths ~ sex * time + age
    mod <- mod_pois(formula = formula,
                    data = data,
                    exposure = popn)
    mod <- set_prior(mod, time ~ AR(n = 2))
    ans_obtained <- fit(mod)
    expect_s3_class(ans_obtained, "bage_mod")
})



## 'get_vals_est' -------------------------------------------------------------

test_that("'get_vals_est' works with 'bage_mod_pois'", {
    set.seed(0)
    data <- expand.grid(age = 0:9, time = 2000:2005, sex = c("F", "M"))
    data$popn <- rpois(n = nrow(data), lambda = 100)
    data$deaths <- rpois(n = nrow(data), lambda = 10)
    formula <- deaths ~ age + sex + time
    mod <- mod_pois(formula = formula,
                    data = data,
                    exposure = popn)
    mod <- set_disp(mod, s = 0)
    mod <- set_n_draw(mod, n = 5)
    mod <- set_prior(mod, time ~ compose_time(trend = RW(), cyclical = AR()))
    mod <- fit(mod)
    ans_obtained <- get_vals_est(mod)
    comp <- components(mod)
    aug <- augment(mod)
    ans_expected <- list(par = comp$.fitted[[comp$component == "effect"]],
                         hyper = comp$.fitted[[comp$component == "hyper"]],
                         trend = comp$.fitted[[comp$component == "trend"]],
                         cyclical = comp$.fitted[[comp$component == "cyclical"]],
                         seasonal = NULL,
                         error = NULL,
                         disp = NULL,
                         fitted = aug$.fitted)
    expect_identical(as.numeric(unlist(ans_obtained)),
                     as.numeric(unlist(ans_expected)))
})

test_that("'get_vals_est' works with 'bage_mod_norm'", {
    set.seed(0)
    data <- expand.grid(age = 0:9, time = 2000:2005, sex = c("F", "M"))
    data$income <- rpois(n = nrow(data), lambda = 100)
    data$wt <- rpois(n = nrow(data), lambda = 10)
    formula <- income ~ age + sex + time
    mod <- mod_norm(formula = formula,
                    data = data,
                    weights = wt)
    mod <- set_n_draw(mod, n = 5)
    mod <- fit(mod)
    ans_obtained <- get_vals_est(mod)
    comp <- components(mod)
    aug <- augment(mod)
    ans_expected <- list(par = comp$.fitted[[comp$component == "effect"]],
                         hyper = comp$.fitted[[comp$component == "hyper"]],
                         trend = NULL,
                         cyclical = NULL,
                         seasonal = NULL,
                         error = NULL,
                         disp = comp$.fitted[[comp$component == "disp"]],
                         fitted = aug$.fitted)
    expect_identical(as.numeric(unlist(ans_obtained)),
                     as.numeric(unlist(ans_expected)))
})


## 'get_fun_inv_transform' ----------------------------------------------------

test_that("'get_fun_inv_transform' works with valid inputs", {
    set.seed(0)
    x <- runif(100)
    logit <- function(x) log(x) - log(1 - x)
    expect_equal(get_fun_inv_transform(structure(1, class = "bage_mod_pois"))(log(x)), x)
    expect_equal(get_fun_inv_transform(structure(1, class = "bage_mod_binom"))(logit(x)), x)
    expect_equal(get_fun_inv_transform(structure(1, class = "bage_mod_norm"))(x), x)
})


## 'get_fun_scale_outcome' --------------------------------------------------------

test_that("'get_fun_scale_outcome' works with valid inputs", {
    expect_equal(get_fun_scale_outcome(structure(1, class = c("bage_mod_pois", "bage_mod")))(1), 1)
    expect_equal(get_fun_scale_outcome(structure(1, class = c("bage_mod_binom", "bage_mod")))(1), 1)
    expect_equal(get_fun_scale_outcome(structure(list(outcome_mean = 3, outcome_sd = 2),
                                                 class = c("bage_mod_norm", "bage_mod")))(1), 5)
})


## 'has_disp' -----------------------------------------------------------------

test_that("'is_fitted' works with valid inputs", {
    data <- data.frame(deaths = 1:10,
                       time = 2001:2010)
    mod <- mod_pois(deaths ~ time,
                    data = data,
                    exposure = 1)
    expect_true(has_disp(mod))
    mod <- set_disp(mod, s = 0)
    expect_false(has_disp(mod))
})


## 'is_fitted' ----------------------------------------------------------------

test_that("'is_fitted' works with valid inputs", {
    data <- data.frame(deaths = 1:10,
                       time = 2001:2010)
    mod <- mod_pois(deaths ~ time,
                    data = data,
                    exposure = 1)
    expect_false(is_fitted(mod))
    mod <- fit(mod)
    expect_true(is_fitted(mod))
})


## 'make_par_disp_inner' ---------------------------------------------------

test_that("'make_par_disp_inner' works with bage_mod_pois", {
    set.seed(0)
    data <- expand.grid(age = 0:9, time = 2000:2005, sex = c("F", "M"))
    data$popn <- rpois(n = nrow(data), lambda = 100)
    data$deaths <- rpois(n = nrow(data), lambda = 10)
    formula <- deaths ~ age + time + sex
    mod <- mod_pois(formula = formula,
                    data = data,
                    exposure = popn)
    outcome <- data$deaths
    offset <- data$popn
    meanpar <- rvec::rpois_rvec(n = 120, lambda = outcome, n_draw = 5)
    disp <- rvec::runif_rvec(n = 1, min = 0.1, max = 0.5, n_draw = 5)
    set.seed(0)
    ans_obtained <- make_par_disp_inner(mod,
                                        outcome = outcome,
                                        offset = offset,
                                        meanpar = meanpar,
                                        disp = disp)
    set.seed(0)
    ans_expected <- rvec::rgamma_rvec(n = length(meanpar),
                                      data$deaths + 1/disp,
                                      data$popn + 1/(disp*meanpar))
    expect_equal(ans_obtained, ans_expected)
})

test_that("'make_par_disp_inner' works with bage_mod_pois", {
    set.seed(0)
    data <- expand.grid(age = 0:9, time = 2000:2005, sex = c("F", "M"))
    data$popn <- rpois(n = nrow(data), lambda = 100)
    data$deaths <- rbinom(n = nrow(data), size = data$popn, prob = 0.3)
    formula <- deaths ~ age + time + sex
    mod <- mod_binom(formula = formula,
                     data = data,
                     size = popn)
    outcome <- data$deaths
    offset <- data$popn
    meanpar <- rvec::runif_rvec(n = 120, n_draw = 5)
    disp <- rvec::runif_rvec(n = 1, min = 0.1, max = 0.8, n_draw = 5)
    set.seed(0)
    ans_obtained <- make_par_disp_inner(mod,
                                        outcome = outcome,
                                        offset = offset,
                                        meanpar = meanpar,
                                        disp = disp)
    set.seed(0)
    ans_expected <- rvec::rbeta_rvec(n = length(meanpar),
                                     data$deaths + meanpar/disp,
                                     data$popn - data$deaths + (1 - meanpar)/disp)
    expect_equal(ans_obtained, ans_expected)
})


## 'make_observed' ------------------------------------------------------------

test_that("'make_observed' works with bage_mod_pois", {
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

test_that("'make_observed' works with bage_mod_binom", {
    set.seed(0)
    data <- expand.grid(age = 0:9, time = 2000:2005, sex = c("F", "M"))
    data$popn <- rpois(n = nrow(data), lambda = 100)
    data$deaths <- rbinom(n = nrow(data), size = data$popn, prob = 0.3)
    formula <- deaths ~ age + sex
    mod <- mod_binom(formula = formula,
                     data = data,
                     size = popn)
    ans_obtained <- make_observed(mod)
    align_fun <- get_fun_align_to_data(mod)
    ans_expected <- align_fun(as.double(mod$outcome / mod$offset))
    expect_equal(ans_obtained, ans_expected)
})

test_that("'make_observed' throws expected with bage_mod_norm", {
    set.seed(0)
    data <- expand.grid(age = 0:9, time = 2000:2005, sex = c("F", "M"))
    data$popn <- rpois(n = nrow(data), lambda = 100)
    data$deaths <- rpois(n = nrow(data), lambda = 10)
    formula <- deaths ~ age + sex
    mod <- mod_norm(formula = formula,
                    data = data,
                    weights = 1)
    expect_error(make_observed(mod),
                 "Internal error: `make_observed\\(\\)` called on object of class")
})


## 'make_term_par' ---------------------------------------------------------

test_that("'make_term_par' works with valid inputs", {
    expect_identical(make_term_par(structure(1, class = "bage_mod_pois")), "rate")
    expect_identical(make_term_par(structure(1, class = "bage_mod_binom")), "prob")
    expect_identical(make_term_par(structure(1, class = "bage_mod_norm")), "mean")
})


## 'model_descr' --------------------------------------------------------------

test_that("'model_descr' works with valid inputs", {
    expect_identical(model_descr(structure(1, class = "bage_mod_pois")), "Poisson")
    expect_identical(model_descr(structure(1, class = "bage_mod_binom")), "binomial")
    expect_identical(model_descr(structure(1, class = "bage_mod_norm")), "normal")
})


## 'n_time' -------------------------------------------------------------------

test_that("'n_time' works when outcome has time variable", {
    set.seed(0)
    data <- expand.grid(age = 0:9, time = 2000:2005, sex = c("F", "M"))
    data$popn <- rpois(n = nrow(data), lambda = 100)
    data$deaths <- rpois(n = nrow(data), lambda = 10)
    formula <- deaths ~ age + sex + time
    mod <- mod_pois(formula = formula,
                    data = data,
                    exposure = popn)
    ans_obtained <- n_time(mod)
    ans_expected <- 6L
    expect_identical(ans_obtained, ans_expected)
})

test_that("'n_time' works when outcome does not have time variable", {
    set.seed(0)
    data <- expand.grid(age = 0:9, time = 2000:2005, sex = c("F", "M"))
    data$popn <- rpois(n = nrow(data), lambda = 100)
    data$deaths <- rpois(n = nrow(data), lambda = 10)
    formula <- deaths ~ age + sex
    mod <- mod_pois(formula = formula,
                    data = data,
                    exposure = popn)
    ans_obtained <- n_time(mod)
    ans_expected <- 0L
    expect_identical(ans_obtained, ans_expected)
})


## 'nm_distn' -----------------------------------------------------------------

test_that("'nm_distn' works with valid inputs", {
    expect_identical(nm_distn(structure(1, class = "bage_mod_pois")), "pois")
    expect_identical(nm_distn(structure(1, class = "bage_mod_binom")), "binom")
    expect_identical(nm_distn(structure(1, class = "bage_mod_norm")), "norm")
})


## 'nm_offset' ----------------------------------------------------------------

test_that("'nm_offset' works with valid inputs", {
    expect_identical(nm_offset(structure(1, class = "bage_mod_pois")), "exposure")
    expect_identical(nm_offset(structure(1, class = "bage_mod_binom")), "size")
    expect_identical(nm_offset(structure(1, class = "bage_mod_norm")), "weights")
})


## 'replicate_data' -----------------------------------------------------------

test_that("'replicate_data' works with mod_pois", {
    set.seed(0)
    data <- expand.grid(age = 0:9, time = 2000:2005, sex = c("F", "M"))
    data$popn <- rpois(n = nrow(data), lambda = 100)
    data$deaths <- rpois(n = nrow(data), lambda = 0.4 * data$popn)
    formula <- deaths ~ age + sex + time
    mod <- mod_pois(formula = formula,
                    data = data,
                    exposure = popn)
    mod <- fit(mod)
    ans <- replicate_data(mod)
    expect_identical(names(ans), c(".replicate", names(data)))
    expect_identical(nrow(ans), nrow(data) * 20L)
    tab <- tapply(ans$deaths, ans$.replicate, sd)
    expect_false(any(duplicated(tab)))
    ans_fit <- replicate_data(mod, condition_on = "par")
    expect_equal(mean(ans_fit$deaths), mean(ans$deaths), tolerance = 0.01)
})

test_that("'replicate_data' works with mod_binom", {
    set.seed(0)
    data <- expand.grid(age = 0:19, time = 2000:2002, sex = c("F", "M"))
    data$popn <- rpois(n = nrow(data), lambda = 100)
    data$deaths <- rbinom(n = nrow(data), size = data$popn, prob = 0.3)
    formula <- deaths ~ age + sex + time
    mod <- mod_binom(formula = formula,
                    data = data,
                    size = popn)
    mod <- fit(mod)
    ans <- replicate_data(mod)
    expect_identical(names(ans), c(".replicate", names(data)))
    expect_identical(nrow(ans), nrow(data) * 20L)
    tab <- tapply(ans$deaths, ans$.replicate, mean)
    expect_false(any(duplicated(tab)))
    ans_fit <- replicate_data(mod, condition_on = "par")
    expect_equal(mean(ans_fit$deaths), mean(ans$deaths), tolerance = 0.01)
})

test_that("'replicate_data' works with mod_norm", {
    set.seed(0)
    data <- expand.grid(age = 0:9, time = 2000:2005, sex = c("F", "M"))
    data$income <- rnorm(n = nrow(data))
    formula <- income ~ age + sex + time
    mod <- mod_norm(formula = formula,
                    data = data,
                    weights = 1)
    mod <- fit(mod)
    ans <- replicate_data(mod)
    expect_identical(names(ans), c(".replicate", names(data)))
    expect_identical(nrow(ans), nrow(data) * 20L)
    tab <- tapply(ans$income, ans$.replicate, mean)
    expect_false(any(duplicated(tab)))
    expect_warning(replicate_data(mod, condition_on = "expected"),
                   "Ignoring value for `condition_on`.")
})


## 'tidy' ---------------------------------------------------------------------

test_that("'tidy' works with valid inputs", {
    set.seed(0)
    data <- expand.grid(age = 0:9, time = 2000:2005, sex = c("F", "M"))
    data$popn <- rpois(n = nrow(data), lambda = 100)
    data$deaths <- rbinom(n = nrow(data), size = data$popn, prob = 0.3)
    formula <- deaths ~ age + sex
    mod <- mod_binom(formula = formula,
                    data = data,
                    size = popn)
    ans_unfit <- tidy(mod)
    expect_true(is.data.frame(ans_unfit))
    expect_identical(names(ans_unfit), c("term", "spec", "n"))
    mod_fitted <- fit(mod)
    ans_fitted <- tidy(mod_fitted)
    expect_true(is.data.frame(ans_fitted))
    expect_identical(names(ans_fitted), c("term", "spec", "n", "sd"))
})
