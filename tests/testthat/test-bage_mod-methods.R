
## 'augment' ---------------------------------------------------------------

test_that("'augment' works with valid inputs", {
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
                     c(names(data), c(".fitted", ".observed")))
})


## 'components' ---------------------------------------------------------------

test_that("'components' works with valid inputs", {
    set.seed(0)    
    data <- expand.grid(age = 0:9, time = 2000:2005, sex = c("F", "M"))
    data$popn <- rpois(n = nrow(data), lambda = 100)
    data$deaths <- rpois(n = nrow(data), lambda = 10)
    formula <- deaths ~ age + sex + time
    mod <- mod_pois(formula = formula,
                    data = data,
                    exposure = popn)
    expect_identical(components(mod), NULL)
    mod_fitted <- fit(mod)
    ans <- components(mod_fitted)
    expect_true(is.data.frame(ans))
    expect_identical(unique(ans$component), c("par", "hyper", "const"))
})


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
    expect_equal(ans_obtained$est$parfree[[1L]], -2)
    expect_equal(ans_obtained$est$parfree[11:12], c(sex = -0.1, sex = 0.1))
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
    expect_identical(length(mod_fitted$est$parfree), nrow(data))
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


## 'model_descr' --------------------------------------------------------------

test_that("'model_descr' works with valid inputs", {
    expect_identical(model_descr(structure(1, class = "bage_mod_pois")), "Poisson")
    expect_identical(model_descr(structure(1, class = "bage_mod_binom")), "binomial")
    expect_identical(model_descr(structure(1, class = "bage_mod_norm")), "normal")
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
