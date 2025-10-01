
## 'set_confidential_rr3' --------------------------------------------------

test_that("'set_confidential_rr3' works with Poisson", {
  data <- expand.grid(age = 0:2, time = 2000:2001, sex = 1:2)
  data$popn <- seq_len(nrow(data))
  data$deaths <- sample(c(0, 3, 9, 12), size = nrow(data), replace = TRUE)
  formula <- deaths ~ age:sex + time
  mod <- mod_pois(formula = formula,
                  data = data,
                  exposure = popn)
  ans <- set_confidential_rr3(mod)
  expect_identical(ans$confidential, new_bage_confidential_rr3())
})

test_that("'set_confidential_rr3' works with Poisson - outcome has NA", {
  data <- expand.grid(age = 0:2, time = 2000:2001, sex = 1:2)
  data$popn <- seq_len(nrow(data))
  data$deaths <- sample(c(0, 3, 9, 12), size = nrow(data), replace = TRUE)
  data$deaths[1] <- NA
  formula <- deaths ~ age:sex + time
  mod <- mod_pois(formula = formula,
                  data = data,
                  exposure = popn)
  ans <- set_confidential_rr3(mod)
  expect_identical(ans$confidential, new_bage_confidential_rr3())
})

test_that("'set_confidential_rr3' works with binomial", {
  data <- expand.grid(age = 0:2, time = 2000:2001, sex = 1:2)
  data$popn <- seq_len(nrow(data)) + 12
  data$deaths <- sample(c(0, 3, 9, 12), size = nrow(data), replace = TRUE)
  formula <- deaths ~ age:sex + time
  mod <- mod_binom(formula = formula,
                   data = data,
                   size = popn)
  ans <- set_confidential_rr3(mod)
  expect_identical(ans$confidential, new_bage_confidential_rr3())
})

test_that("'set_confidential_rr3' throws correct error when used with non-Poisson, non-binom", {
  data <- expand.grid(age = 0:2, time = 2000:2001, sex = 1:2)
  data$popn <- seq_len(nrow(data))
  data$deaths <- sample(c(0, 3, 9, 12), size = nrow(data), replace = TRUE)
  formula <- deaths ~ age:sex + time
  mod <- mod_norm(formula = formula,
                  data = data,
                  weights = 1)
  expect_error(set_confidential_rr3(mod),
               "Outcome has \"norm\" distribution.")
})

test_that("'set_confidential_rr3' throws correct error when used with non-Poisson, non-binom", {
  data <- expand.grid(age = 0:2, time = 2000:2001, sex = 1:2)
  data$popn <- seq_len(nrow(data))
  data$deaths <- 1
  formula <- deaths ~ age:sex + time
  mod <- mod_pois(formula = formula,
                  data = data,
                  exposure = 1)
  expect_error(set_confidential_rr3(mod),
               "Outcome variable has values not divisible by 3.")
  data$deaths <- c(1, rep(3, times = nrow(data) - 1))
  formula <- deaths ~ age:sex + time
  mod <- mod_pois(formula = formula,
                  data = data,
                  exposure = 1)
  expect_error(set_confidential_rr3(mod),
               "Outcome variable has value not divisible by 3.")
})


## 'set_covariates' -----------------------------------------------------------

test_that("'set_covariates' works with Poisson", {
  data <- expand.grid(age = 0:2, time = 2000:2001, sex = 1:2)
  data$popn <- seq_len(nrow(data))
  data$deaths <- rpois(n = nrow(data), lambda = 3)
  data$income <- rnorm(n = nrow(data))
  formula <- deaths ~ age:sex + time
  mod <- mod_pois(formula = formula,
                  data = data,
                  exposure = popn)
  ans_obtained <- set_covariates(mod, ~ income)
  ans_expected <- mod
  ans_expected$formula_covariates <- ~ income
  ans_expected$covariates_nms <- "income"
  expect_identical(ans_obtained, ans_expected)
})

test_that("'set_covariates' works with binomial", {
  data <- expand.grid(age = 0:2, time = 2000:2001, sex = 1:2)
  data$popn <- seq_len(nrow(data))
  data$deaths <- rbinom(n = nrow(data), size = data$popn, prob = 0.3)
  data$income <- rnorm(n = nrow(data))
  formula <- deaths ~ age:sex + time
  mod <- mod_binom(formula = formula,
                   data = data,
                   size = popn)
  ans_obtained <- set_covariates(mod, ~ income)
  ans_expected <- mod
  ans_expected$formula_covariates <- ~income
  ans_expected$covariates_nms <- "income"
  expect_identical(ans_obtained, ans_expected)
})

test_that("'set_covariates' issues message with existing covariates", {
  data <- expand.grid(age = 0:2, time = 2000:2001, sex = 1:2)
  data$popn <- seq_len(nrow(data))
  data$deaths <- rpois(n = nrow(data), lambda = 3)
  data$income <- rnorm(n = nrow(data))
  formula <- deaths ~ age:sex + time
  mod <- mod_pois(formula = formula,
                  data = data,
                  exposure = popn) |>
    set_covariates(~ income)
  expect_message(set_covariates(mod, ~ income),
                "Model already has covariates\\. Deleting these\\.")
})


## 'set_datamod_exposure' -----------------------------------------------------

test_that("'set_datamod_exposure' works with 2 by variables", {
  data <- expand.grid(age = 0:2, time = 2000:2001, sex = 1:2)
  data$popn <- seq_len(nrow(data))
  data$deaths <- 1
  cv <- expand.grid(age = 0:3, sex = 1:2)
  cv$cv <- 0.6
  formula <- deaths ~ age:sex + time
  mod <- mod_pois(formula = formula,
                  data = data,
                  exposure = popn) |>
    set_disp(mean = 0)
  mod <- set_datamod_exposure(mod, cv = cv)
  expect_s3_class(mod$datamod, "bage_datamod_exposure")
  expect_identical(mod$datamod$nms_by, c("age", "sex"))
  expect_message(set_datamod_exposure(mod,
                                      cv = cv),
                 paste("Replacing existing \"exposure\" data model",
                       "with new \"exposure\" data model."))
})

test_that("'set_datamod_exposure' works with 1 by variable", {
  data <- expand.grid(age = 0:2, time = 2000:2001, sex = 1:2)
  data$popn <- seq_len(nrow(data))
  data$deaths <- 1
  cv <- expand.grid(age = 0:3)
  cv$cv <- (1:4)/10
  formula <- deaths ~ age:sex + time
  mod <- mod_pois(formula = formula,
                  data = data,
                  exposure = popn) |>
    set_disp(mean = 0)
  mod <- set_datamod_exposure(mod, cv = cv)
  expect_s3_class(mod$datamod, "bage_datamod_exposure")
  expect_identical(mod$datamod$nms_by, "age")
  expect_equal(mod$datamod$disp, ((1:3)/10)^2)
})

test_that("'set_datamod_exposure' works with numeric disp", {
  data <- expand.grid(age = 0:2, time = 2000:2001, sex = 1:2)
  data$popn <- seq_len(nrow(data))
  data$deaths <- 1
  cv <- 0.1
  formula <- deaths ~ age:sex + time
  mod <- mod_pois(formula = formula,
                  data = data,
                  exposure = popn) |>
    set_disp(mean = 0)
  mod <- set_datamod_exposure(mod, cv = cv)
  expect_s3_class(mod$datamod, "bage_datamod_exposure")
  expect_identical(mod$datamod$nms_by, character())
  expect_equal(mod$datamod$disp, 0.01)
})

test_that("'set_datamod_exposure' throws correct error with non-Poisson", {
  data <- expand.grid(age = 0:2, time = 2000:2001, sex = 1:2)
  data$popn <- seq_len(nrow(data))
  data$deaths <- 1
  cv <- expand.grid(age = 0:3)
  cv$cv <- 1:4
  formula <- deaths ~ age:sex + time
  mod <- mod_binom(formula = formula,
                  data = data,
                  size = popn) |>
    set_disp(mean = 0)
  expect_error(set_datamod_exposure(mod, cv = cv),
               "`mod` is a binomial model.")
})

test_that("'set_datamod_exposure' throws correct error with exposure specified through formula", {
  data <- expand.grid(age = 0:2, time = 2000:2001, sex = 1:2)
  data$popn <- seq_len(nrow(data))
  data$deaths <- 1
  cv <- expand.grid(age = 0:3)
  cv$cv <- 1:4
  formula <- deaths ~ age:sex + time
  suppressWarnings(mod <- mod_pois(formula = formula,
                                   data = data,
                                   exposure = ~ popn + 1) |>
                     set_disp(mean = 0))
  expect_error(set_datamod_exposure(mod, cv = cv),
               "`set_datamod_exposure\\(\\)` cannot be used with models where exposure specified using formula.")
})

test_that("'set_datamod_exposure' throws correct error when applied to model without exposure", {
  data <- data.frame(deaths = 1:10 * 3,
                     time = 2001:2010)
  mod <- mod_pois(deaths ~ time,
                  data = data,
                  exposure = 1)  |>
    set_disp(mean = 0)
  expect_error(set_datamod_exposure(mod, cv = 0.2),
               "`mod` does not include exposure.")
})

test_that("'set_datamod_exposure' throws appropriate message when Poisson disp non-zero", {
  data <- expand.grid(age = 0:2, time = 2000:2001, sex = 1:2)
  data$popn <- seq_len(nrow(data))
  data$deaths <- 1
  cv <- expand.grid(age = 0:3, sex = 1:2)
  cv$cv <- 0.6
  formula <- deaths ~ age:sex + time
  mod <- mod_pois(formula = formula,
                  data = data,
                  exposure = popn)
  expect_message(set_datamod_exposure(mod, cv = cv),
                 "Setting dispersion to zero.")
})

test_that("'set_datamod_exposure' throws appropriate message when Poisson disp non-zero", {
  data <- expand.grid(age = 0:2, time = 2000:2001, sex = 1:2)
  data$popn <- seq_len(nrow(data))
  data$deaths <- 1
  formula <- deaths ~ age:sex + time
  mod <- mod_pois(formula = formula,
                  data = data,
                  exposure = popn) |>
    set_disp(mean = 0)
    expect_error(set_datamod_exposure(mod, cv = -1),
                 "`cv` less than or equal to 0.")
})


## 'set_datamod_miscount' -----------------------------------------------------

test_that("'set_datamod_miscount' works with 2 by variables", {
  data <- expand.grid(age = 0:2, time = 2000:2001, sex = 1:2)
  data$popn <- seq_len(nrow(data))
  data$deaths <- 1
  prob <- data.frame(mean = 0.9, disp = 2)
  rate <- expand.grid(age = 0:3, sex = 1:2)
  rate$mean <- 0.6
  rate$disp <- 1:8
  formula <- deaths ~ age:sex + time
  mod <- mod_pois(formula = formula,
                  data = data,
                  exposure = popn)
  mod <- set_datamod_miscount(mod, prob = prob, rate = rate)
  expect_s3_class(mod$datamod, "bage_datamod_miscount")
  expect_identical(mod$datamod$nms_by, c("age", "sex"))
  expect_message(set_datamod_miscount(mod,
                                      prob = prob,
                                      rate = rate),
                 paste("Replacing existing \"miscount\" data model",
                       "with new \"miscount\" data model."))
})

test_that("'set_datamod_miscount' works with 1 by variable", {
  data <- expand.grid(age = 0:2, time = 2000:2001, sex = 1:2)
  data$popn <- seq_len(nrow(data))
  data$deaths <- 1
  prob <- expand.grid(age = 0:3)
  prob$mean <- 0.5
  prob$disp <- 1:4
  rate <- data.frame(mean = 1, disp = 0.1)
  formula <- deaths ~ age:sex + time
  mod <- mod_pois(formula = formula,
                  data = data,
                  exposure = popn)
  mod <- set_datamod_miscount(mod, prob = prob, rate = rate)
  expect_s3_class(mod$datamod, "bage_datamod_miscount")
})

test_that("'set_datamod_miscount' throws correct error with non-Poisson", {
  data <- expand.grid(age = 0:2, time = 2000:2001, sex = 1:2)
  data$popn <- seq_len(nrow(data))
  data$deaths <- 1
  prob <- expand.grid(age = 0:3)
  prob$mean <- 0.5
  prob$disp <- 1:4
  rate <- data.frame(mean = 1, disp = 0.1)
  formula <- deaths ~ age:sex + time
  mod <- mod_binom(formula = formula,
                  data = data,
                  size = popn)
  expect_error(set_datamod_miscount(mod, prob = prob, rate = rate),
               "A miscount data model can only be used with a Poisson model.")
})


## 'set_datamod_noise' -----------------------------------------------------

test_that("'set_datamod_noise' works with 2 by variables", {
  set.seed(0)
  data <- expand.grid(age = 0:2, time = 2000:2001, sex = 1:2)
  data$income <- rnorm(nrow(data))
  data$wt <- 1
  sd <- expand.grid(age = 0:3, sex = 1:2)
  sd$sd <- 0.6
  formula <- income ~ age:sex + time
  mod <- mod_norm(formula = formula,
                  data = data,
                  weight = wt)
  mod <- set_datamod_noise(mod, sd = sd)
  expect_s3_class(mod$datamod, "bage_datamod_noise")
  expect_equal(mod$datamod$sd_sd, rep(0.6, times = 6))
  expect_identical(mod$datamod$nms_by, c("age", "sex"))
  expect_identical(mod$datamod$outcome_sd, mod$outcome_sd)
  expect_message(set_datamod_noise(mod,
                                   sd = sd),
                 paste("Replacing existing \"noise\" data model",
                       "with new \"noise\" data model."))
})

test_that("'set_datamod_noise' works with 1 by variable", {
  set.seed(0)
  data <- expand.grid(age = 0:2, time = 2000:2001, sex = 1:2)
  data$income <- rnorm(nrow(data))
  data$wt <- 1
  sd <- expand.grid(age = 0:3)
  sd$sd <- 1:4
  formula <- income ~ age:sex + time
  mod <- mod_norm(formula = formula,
                  data = data,
                  weights = wt)
  mod <- set_datamod_noise(mod, sd = sd)
  expect_s3_class(mod$datamod, "bage_datamod_noise")
  expect_equal(mod$datamod$sd_sd, 1:3)
})

test_that("'set_datamod_noise' works with numeric sd", {
  set.seed(0)
  data <- expand.grid(age = 0:2, time = 2000:2001, sex = 1:2)
  data$income <- rnorm(nrow(data))
  data$wt <- 1
  formula <- income ~ age:sex + time
  mod <- mod_norm(formula = formula,
                  data = data,
                  weights = wt)
  mod <- set_datamod_noise(mod, sd = 0.2)
  expect_s3_class(mod$datamod, "bage_datamod_noise")
  expect_equal(mod$datamod$sd_sd, 0.2)
})

test_that("'set_datamod_noise' works with numeric sd, poisson model", {
  set.seed(0)
  data <- expand.grid(age = 0:2, time = 2000:2001, sex = 1:2)
  data$deaths <- rpois(nrow(data), lambda = 10)
  data$popn <- 100
  formula <- deaths ~ age:sex + time
  mod <- mod_pois(formula = formula,
                  data = data,
                  exposure = popn)
  expect_message(mod <- set_datamod_noise(mod, sd = 0.2),
                 "Setting dispersion to zero.")
  expect_s3_class(mod$datamod, "bage_datamod_noise")
  expect_equal(mod$datamod$sd_sd, 0.2)
})

test_that("'set_datamod_noise' throws error with negative sd", {
  set.seed(0)
  data <- expand.grid(age = 0:2, time = 2000:2001, sex = 1:2)
  data$deaths <- rpois(nrow(data), lambda = 10)
  data$popn <- 100
  formula <- deaths ~ age:sex + time
  mod <- mod_pois(formula = formula,
                  data = data,
                  exposure = popn) |>
    set_disp(mean = 0)
  expect_error(mod <- set_datamod_noise(mod, sd = -1),
               "`sd` less than or equal to 0.")
})

test_that("'set_datamod_noise' throws correct error with non-normal, non-pois", {
  data <- expand.grid(age = 0:2, time = 2000:2001, sex = 1:2)
  data$popn <- seq_len(nrow(data))
  data$deaths <- 1
  mean <- data.frame(mean = 0.9)
  sd <- expand.grid(age = 0:3)
  sd$sd <- 1
  formula <- deaths ~ age:sex + time
  mod <- mod_binom(formula = formula,
                   data = data,
                   size = popn)
  expect_error(set_datamod_noise(mod, sd = sd),
               "`mod` is a binomial model.")
})


## 'set_datamod_overcount' -----------------------------------------------------

test_that("'set_datamod_overcount' works with 2 by variables", {
  data <- expand.grid(age = 0:2, time = 2000:2001, sex = 1:2)
  data$popn <- seq_len(nrow(data))
  data$deaths <- 1
  rate <- expand.grid(age = 0:3, sex = 1:2)
  rate$mean <- 0.6
  rate$disp <- 1:8
  formula <- deaths ~ age:sex + time
  mod <- mod_pois(formula = formula,
                  data = data,
                  exposure = popn)
  mod <- set_datamod_overcount(mod, rate = rate)
  expect_s3_class(mod$datamod, "bage_datamod_overcount")
  expect_identical(mod$datamod$nms_by, c("age", "sex"))
  expect_message(set_datamod_overcount(mod,
                                       rate = rate),
                 paste("Replacing existing \"overcount\" data model",
                       "with new \"overcount\" data model."))
})

test_that("'set_datamod_overcount' works with 1 by variable", {
  data <- expand.grid(age = 0:2, time = 2000:2001, sex = 1:2)
  data$popn <- seq_len(nrow(data))
  data$deaths <- 1
  rate <- expand.grid(age = 0:3)
  rate$mean <- 0.5
  rate$disp <- 1:4
  formula <- deaths ~ age:sex + time
  mod <- mod_pois(formula = formula,
                  data = data,
                  exposure = popn)
  mod <- set_datamod_overcount(mod, rate = rate)
  expect_s3_class(mod$datamod, "bage_datamod_overcount")
})

test_that("'set_datamod_overcount' throws correct error with non-Poisson", {
  data <- expand.grid(age = 0:2, time = 2000:2001, sex = 1:2)
  data$popn <- seq_len(nrow(data))
  data$deaths <- 1
  rate <- data.frame(mean = 1, disp = 0.1)
  formula <- deaths ~ age:sex + time
  mod <- mod_binom(formula = formula,
                   data = data,
                   size = popn)
  expect_error(set_datamod_overcount(mod, rate = rate),
               "An overcount data model can only be used with a Poisson model.")
})


## 'set_datamod_undercount' ---------------------------------------------------

test_that("'set_datamod_undercount' works with 2 by variables - Poisson", {
  data <- expand.grid(age = 0:2, time = 2000:2001, sex = 1:2)
  data$popn <- seq_len(nrow(data))
  data$deaths <- 1
  prob <- expand.grid(age = 0:3, sex = 1:2)
  prob$mean <- 0.6
  prob$disp <- 1:8
  formula <- deaths ~ age:sex + time
  mod <- mod_pois(formula = formula,
                  data = data,
                  exposure = popn)
  mod <- set_datamod_undercount(mod, prob = prob)
  expect_s3_class(mod$datamod, "bage_datamod_undercount")
  expect_identical(mod$datamod$nms_by, c("age", "sex"))
  expect_message(set_datamod_undercount(mod,
                                        prob = prob),
                 paste("Replacing existing \"undercount\" data model",
                       "with new \"undercount\" data model."))
})

test_that("'set_datamod_undercount' works with 2 by variables - binomial", {
  data <- expand.grid(age = 0:2, time = 2000:2001, sex = 1:2)
  data$popn <- seq_len(nrow(data))
  data$deaths <- 1
  prob <- expand.grid(age = 0:3, sex = 1:2)
  prob$mean <- 0.6
  prob$disp <- 1:8
  formula <- deaths ~ age:sex + time
  mod <- mod_binom(formula = formula,
                  data = data,
                  size = popn)
  mod <- set_datamod_undercount(mod, prob = prob)
  expect_s3_class(mod$datamod, "bage_datamod_undercount")
})

test_that("'set_datamod_undercount' works with 1 by variable - Poisson", {
  data <- expand.grid(age = 0:2, time = 2000:2001, sex = 1:2)
  data$popn <- seq_len(nrow(data))
  data$deaths <- 1
  prob <- expand.grid(age = 0:3)
  prob$mean <- 0.5
  prob$disp <- 1:4
  formula <- deaths ~ age:sex + time
  mod <- mod_pois(formula = formula,
                  data = data,
                  exposure = popn)
  mod <- set_datamod_undercount(mod, prob = prob)
  expect_s3_class(mod$datamod, "bage_datamod_undercount")
})

test_that("'set_datamod_undercount' works with 1 by variable - binomial", {
  data <- expand.grid(age = 0:2, time = 2000:2001, sex = 1:2)
  data$popn <- seq_len(nrow(data))
  data$deaths <- 1
  prob <- expand.grid(age = 0:3)
  prob$mean <- 0.5
  prob$disp <- 1:4
  formula <- deaths ~ age:sex + time
  mod <- mod_binom(formula = formula,
                   data = data,
                   size = popn)
  mod <- set_datamod_undercount(mod, prob = prob)
  expect_s3_class(mod$datamod, "bage_datamod_undercount")
})

test_that("'set_datamod_undercount' throws correct error with non-Poisson", {
  data <- expand.grid(age = 0:2, time = 2000:2001, sex = 1:2)
  data$popn <- seq_len(nrow(data))
  data$income <- 1
  prob <- data.frame(mean = 1, disp = 0.1)
  formula <- income ~ age:sex + time
  mod <- mod_norm(formula = formula,
                  data = data,
                  weights = popn)
  expect_error(set_datamod_undercount(mod, prob = prob),
               "An undercount data model can only be used with a Poisson or binomial model.")
})


## 'set_datamod_outcome_rr3' --------------------------------------------------

test_that("'set_datamod_outcome_rr3' throws deprecation warning but returns value", {
  data <- expand.grid(age = 0:2, time = 2000:2001, sex = 1:2)
  data$popn <- seq_len(nrow(data))
  data$deaths <- sample(c(0, 3, 9, 12), size = nrow(data), replace = TRUE)
  formula <- deaths ~ age:sex + time
  mod <- mod_pois(formula = formula,
                  data = data,
                  exposure = popn)
  rlang::local_options(lifecycle_verbosity = "warning")
  expect_warning(ans_obtained <- set_datamod_outcome_rr3(mod))
  expect_identical(ans_obtained$confidential, new_bage_confidential_rr3())
})


## 'set_disp' -----------------------------------------------------------------

test_that("'set_disp' works with Poisson", {
    data <- expand.grid(age = 0:2, time = 2000:2001, sex = 1:2)
    data$popn <- seq_len(nrow(data))
    data$deaths <- rev(seq_len(nrow(data)))
    formula <- deaths ~ age:sex + time
    mod <- mod_pois(formula = formula,
                    data = data,
                    exposure = popn)
    expect_identical(mod$mean_disp, 1)
    mod <- set_disp(mod, mean = 0)
    expect_identical(mod$mean_disp, 0)
    mod <- set_disp(mod, mean = 0.5)
    expect_identical(mod$mean_disp, 0.5)
    mod <- set_disp(mod)
    expect_identical(mod$mean_disp, 1)
})

test_that("'set_disp' works with normal", {
    data <- expand.grid(age = 0:2, time = 2000:2001, sex = 1:2)
    data$deaths <- rpois(n = nrow(data), lambda = 100)
    formula <- deaths ~ age:sex + time
    mod <- mod_norm(formula = formula,
                    data = data,
                    weights = 1)
    expect_identical(mod$mean_disp, 1)
    mod <- set_disp(mod, mean = 0.5)
    expect_identical(mod$mean_disp, 0.5)
    expect_error(set_disp(mod, mean = 0))
    mod <- set_disp(mod)
    expect_identical(mod$mean_disp, 1)
})


## 'set_n_draw' ---------------------------------------------------------------

test_that("'set_n_draw' works with valid inputs", {
  data <- expand.grid(age = 0:2, time = 2000:2001, sex = 1:2)
  data$popn <- seq_len(nrow(data))
  data$deaths <- rev(seq_len(nrow(data)))
  formula <- deaths ~ age:sex + time
  mod <- mod_pois(formula = formula,
                  data = data,
                  exposure = popn)
  mod <- set_n_draw(mod, n_draw = 10L)
  expect_identical(mod$n_draw, 10L)
  mod <- fit(mod)
  mod <- set_n_draw(mod, n_draw = 5)
  expect_identical(mod$n_draw, 5L)
  expect_true(is_fitted(mod))
  expect_message(set_n_draw(mod, n_draw = 10),
                 "New value")
})

test_that("'set_n_draw' reduces draws in fitted object", {
  data <- expand.grid(age = 0:2, time = 2000:2001, sex = 1:2)
  data$popn <- seq_len(nrow(data))
  data$deaths <- rev(seq_len(nrow(data)))
  data$income <- rnorm(nrow(data))
  formula <- deaths ~ age:sex + time
  mod <- mod_pois(formula = formula,
                  data = data,
                  exposure = popn)
  mod <- set_prior(mod, time ~ Lin_AR1())
  mod <- set_covariates(mod, ~ income)
  mod <- set_datamod_overcount(mod, rate = data.frame(mean = 0.1, disp = 0.2))
  mod <- set_n_draw(mod, n_draw = 10L)
  mod <- fit(mod)
  expect_identical(ncol(mod$draws_effectfree), 10L)
  expect_identical(ncol(mod$draws_hyper), 10L)
  expect_identical(ncol(mod$draws_hyperrandfree), 10L)
  expect_identical(ncol(mod$draws_coef_covariates), 10L)
  expect_identical(ncol(mod$draws_datamod_param), 10L)
  mod <- set_n_draw(mod, n_draw = 5)
  expect_identical(mod$n_draw, 5L)
  expect_identical(ncol(mod$draws_effectfree), 5L)
  expect_identical(ncol(mod$draws_hyper), 5L)
  expect_identical(ncol(mod$draws_hyperrandfree), 5L)
  expect_identical(ncol(mod$draws_coef_covariates), 5L)
  expect_identical(ncol(mod$draws_datamod_param), 5L)
})


## 'set_prior' ----------------------------------------------------------------

test_that("'set_prior' works with valid inputs", {
    data <- expand.grid(age = 0:3, time = 2000:2004, sex = 1:2)
    data$popn <- seq_len(nrow(data))
    data$deaths <- rev(seq_len(nrow(data)))
    formula <- deaths ~ age*sex + time
    mod <- mod_pois(formula = formula,
                    data = data,
                    exposure = popn)
    mod_rw <- set_prior(mod, age ~ RW2())
    expect_s3_class(mod_rw$priors[["age"]], "bage_prior_rw2random")
    mod_rw <- set_prior(mod, age:sex ~ NFix())
    expect_s3_class(mod_rw$priors[["age:sex"]], "bage_prior_normfixed")
})

test_that("'set_prior' throws correct error with invalid response", {
    data <- expand.grid(age = 0:2, time = 2000:2001, sex = 1:2)
    data$popn <- seq_len(nrow(data))
    data$deaths <- rev(seq_len(nrow(data)))
    formula <- deaths ~ age + sex + time
    mod <- mod_pois(formula = formula,
                    data = data,
                    exposure = popn)
    expect_error(set_prior(mod, wrong ~ RW()),
                 "Problem with prior formula `wrong ~ RW\\(\\)`.")
})

test_that("'set_prior' throws correct error with invalid prior function", {
    data <- expand.grid(age = 0:2, time = 2000:2001, sex = 1:2)
    data$popn <- seq_len(nrow(data))
    data$deaths <- rev(seq_len(nrow(data)))
    formula <- deaths ~ age:sex + time
    mod <- mod_pois(formula = formula,
                    data = data,
                    exposure = popn)
    expect_error(set_prior(mod, age:sex ~ Wrong()),
                 "Problem with prior formula `age:sex ~ Wrong\\(\\)`")
})

test_that("'set_prior' throws correct error with SVD prior but var_age not indentified", {
    data <- expand.grid(v = 0:2, time = 2000:2001, sex = 1:2)
    data$popn <- seq_len(nrow(data))
    data$deaths <- rev(seq_len(nrow(data)))
    formula <- deaths ~ v*sex + time
    mod <- mod_pois(formula = formula,
                    data = data,
                    exposure = popn)
    expect_error(set_prior(mod, v:sex ~ SVD(HMD)),
                 "Problem with `SVD\\(\\)` prior for term `v:sex`.")
})

test_that("'set_prior' unfits a fitted model", {
    data <- expand.grid(age = 0:2, time = 2000:2005, sex = 1:2)
    data$popn <- seq_len(nrow(data))
    data$deaths <- rev(seq_len(nrow(data)))
    formula <- deaths ~ age:sex + time
    mod <- mod_pois(formula = formula,
                    data = data,
                    exposure = popn)
    mod <- fit(mod)
    mod <- set_prior(mod, time ~ RW())
    expect_false(is_fitted(mod))
})

test_that("'set_prior' works with when order of components of interaction changed", {
    data <- expand.grid(age = 0:2, time = 2000:2001, sex = 1:2)
    data$popn <- seq_len(nrow(data))
    data$deaths <- rev(seq_len(nrow(data)))
    formula <- deaths ~ age + sex:age + time
    mod <- mod_pois(formula = formula,
                    data = data,
                    exposure = popn)
    mod <- set_prior(mod, sex:age ~ NFix())
    expect_s3_class(mod$priors[["age:sex"]], "bage_prior_normfixed")
})


## 'set_seeds' --------------------------------------------------------------

test_that("'set_seeds' works with NULL", {
  data <- expand.grid(age = 0:2, time = 2000:2001, sex = 1:2)
  data$popn <- seq_len(nrow(data))
  data$deaths <- rpois(n = nrow(data), lambda = 3)
  data$income <- rnorm(n = nrow(data))
  formula <- deaths ~ age:sex + time
  mod <- mod_pois(formula = formula,
                  data = data,
                  exposure = popn) |>
    fit()
  aug1 <- augment(mod)
  aug2 <- augment(mod)
  expect_identical(aug1, aug2)
  comp1 <- components(mod)
  comp2 <- components(mod)
  expect_identical(comp1, comp2)
  faug1 <- forecast(mod, labels = 2002)
  faug2 <- forecast(mod, labels = 2002)
  expect_identical(faug1, faug2)
  comp1 <- components(mod)
  comp2 <- components(mod)
  expect_identical(comp1, comp2)
  fcomp1 <- forecast(mod, labels = 2002, output = "comp")
  fcomp2 <- forecast(mod, labels = 2002, output = "comp")
  expect_identical(fcomp1, fcomp2)
  mod <- set_seeds(mod)
  expect_false(is_fitted(mod))
  mod <- fit(mod)
  aug3 <- augment(mod)
  expect_false(identical(aug1, aug3))
  comp3 <- components(mod)
  expect_false(identical(comp1, comp3))
  comp4 <- components(mod)
  expect_identical(comp3, comp4)
  faug3 <- forecast(mod, labels = 2002)
  expect_false(identical(faug1, faug3))
  fcomp3 <- forecast(mod, labels = 2002, output = "comp")
  expect_false(identical(fcomp1, fcomp3))
  fcomp4 <- forecast(mod, labels = 2002, output = "comp")
  expect_identical(fcomp3, fcomp4)
})

test_that("'set_seeds' works with list", {
  data <- expand.grid(age = 0:2, time = 2000:2001, sex = 1:2)
  data$popn <- seq_len(nrow(data))
  data$deaths <- rpois(n = nrow(data), lambda = 3)
  data$income <- rnorm(n = nrow(data))
  formula <- deaths ~ age:sex + time
  mod <- mod_pois(formula = formula,
                  data = data,
                  exposure = popn) |>
    fit()
  aug1 <- augment(mod)
  aug2 <- augment(mod)
  expect_identical(aug1, aug2)
  comp1 <- components(mod)
  comp2 <- components(mod)
  expect_identical(comp1, comp2)
  mod <- set_seeds(mod,
                     new_seeds = list(seed_components = 1,
                                      seed_augment = 2,
                                      seed_forecast_components = 3,
                                      seed_forecast_augment = 4))
  expect_false(is_fitted(mod))
  mod <- fit(mod)
  aug3 <- augment(mod)
  expect_false(identical(aug1, aug3))
  comp3 <- components(mod)
  expect_false(identical(comp1, comp3))
  expect_identical(mod$seed_components, 1)
  expect_identical(mod$seed_augment, 2)
  expect_identical(mod$seed_forecast_components, 3)
  expect_identical(mod$seed_forecast_augment, 4)
})


## 'set_var_age' --------------------------------------------------------------

test_that("'set_var_age' works with valid inputs - no existing age var", {
    data <- expand.grid(oldness = 0:2, time = 2000:2001, sex = 1:2)
    data$popn <- seq_len(nrow(data))
    data$deaths <- rev(seq_len(nrow(data)))
    formula <- deaths ~ oldness*sex + time
    mod <- mod_pois(formula = formula,
                    data = data,
                    exposure = popn)
    expect_identical(mod$var_age, NULL)
    mod <- set_var_age(mod, name = "oldness")
    expect_identical(mod$var_age, "oldness")
    expect_s3_class(mod$priors[["oldness"]], "bage_prior_rwrandom")
})

test_that("'set_var_age' works with valid inputs - has existing age var", {
    data <- expand.grid(age = 0:2, time = 2000:2001, sex = 1:2, oldness = 1:3)
    data$popn <- seq_len(nrow(data))
    data$deaths <- rev(seq_len(nrow(data)))
    formula <- deaths ~ oldness*sex + time + age
    mod <- mod_pois(formula = formula,
                    data = data,
                    exposure = popn)
    expect_identical(mod$var_age, "age")
    expect_s3_class(mod$priors[["age"]], "bage_prior_rwrandom")
    expect_s3_class(mod$priors[["oldness"]], "bage_prior_norm")
    mod <- set_var_age(mod, name = "oldness")
    expect_identical(mod$var_age, "oldness")
    expect_s3_class(mod$priors[["age"]], "bage_prior_norm")
    expect_s3_class(mod$priors[["oldness"]], "bage_prior_rwrandom")
})


## 'set_var_sexgender' --------------------------------------------------------------

test_that("'set_var_sexgender' works with valid inputs - no existing sexgender var", {
    data <- expand.grid(age = 0:2, time = 2000:2001, sexx = 1:2)
    data$popn <- seq_len(nrow(data))
    data$deaths <- rev(seq_len(nrow(data)))
    formula <- deaths ~ age*sexx + time
    mod <- mod_pois(formula = formula,
                    data = data,
                    exposure = popn)
    expect_identical(mod$var_sexgender, NULL)
    mod <- set_var_sexgender(mod, name = "sexx")
    expect_identical(mod$var_sexgender, "sexx")
    expect_s3_class(mod$priors[["sexx"]], "bage_prior_normfixed")
})

test_that("'set_var_sexgender' works with valid inputs - has existing sexgender var", {
    data <- expand.grid(age = 0:2, time = 2000:2001, sex = 1:2, gend = 1:3)
    data$popn <- seq_len(nrow(data))
    data$deaths <- rev(seq_len(nrow(data)))
    formula <- deaths ~ gend*sex + time + age
    mod <- mod_pois(formula = formula,
                    data = data,
                    exposure = popn)
    expect_identical(mod$var_sexgender, "sex")
    expect_s3_class(mod$priors[["sex"]], "bage_prior_normfixed")
    mod <- set_var_sexgender(mod, name = "gend")
    expect_identical(mod$var_sexgender, "gend")
    expect_s3_class(mod$priors[["gend"]], "bage_prior_norm")
    expect_s3_class(mod$priors[["sex"]], "bage_prior_normfixed")
})


## 'set_var_time' --------------------------------------------------------------

test_that("'set_var_time' works with valid inputs", {
    data <- expand.grid(age = 0:2, timex = 2000:2011, sex = 1:2)
    data$popn <- seq_len(nrow(data))
    data$deaths <- rev(seq_len(nrow(data)))
    formula <- deaths ~ age*sex + timex
    mod <- mod_pois(formula = formula,
                    data = data,
                    exposure = popn)
    expect_identical(mod$var_time, NULL)
    mod <- set_var_time(mod, name = "timex")
    expect_identical(mod$var_time, "timex")
    expect_s3_class(mod$priors[["timex"]], "bage_prior_rwrandom")
})


## 'set_var_inner' ------------------------------------------------------------

test_that("'set_var_inner' works with valid inputs - no existing var", {
    data <- expand.grid(oldness = 0:2, time = 2000:2001, sex = 1:2)
    data$popn <- seq_len(nrow(data))
    data$deaths <- rev(seq_len(nrow(data)))
    formula <- deaths ~ oldness*sex + time
    mod <- mod_pois(formula = formula,
                    data = data,
                    exposure = popn)
    expect_identical(mod$var_age, NULL)
    mod <- set_var_inner(mod, name = "oldness", var = "age")
    expect_identical(mod$var_age, "oldness")
    expect_s3_class(mod$priors[["oldness"]], "bage_prior_rwrandom")
})

test_that("'set_var_inner' works with valid inputs - has existing var", {
    data <- expand.grid(age = 0:2, time = 2000:2001, sex = 1:2, oldness = 1:3)
    data$popn <- seq_len(nrow(data))
    data$deaths <- rev(seq_len(nrow(data)))
    formula <- deaths ~ oldness*sex + time + age
    mod <- mod_pois(formula = formula,
                    data = data,
                    exposure = popn)
    expect_identical(mod$var_age, "age")
    expect_s3_class(mod$priors[["age"]], "bage_prior_rwrandom")
    expect_s3_class(mod$priors[["oldness"]], "bage_prior_norm")
    mod <- set_var_inner(mod, name = "oldness", var = "age")
    expect_identical(mod$var_age, "oldness")
    expect_s3_class(mod$priors[["age"]], "bage_prior_norm")
    expect_s3_class(mod$priors[["oldness"]], "bage_prior_rwrandom")
})

test_that("'set_var_inner' gives correct errors with invalid inputs", {
    data <- expand.grid(age = 0:2, time = 2000:2001, sex = 1:2)
    data$popn <- seq_len(nrow(data))
    data$deaths <- rev(seq_len(nrow(data)))
    formula <- deaths ~ age*sex + time
    mod <- mod_pois(formula = formula,
                    data = data,
                    exposure = popn)
    expect_error(set_var_inner(mod = mod,
                               name = "age",
                               var = "time"),
                 "Variables for time and age have the same name.")
})


## 'unfit' --------------------------------------------------------------------

test_that("'unfit' works with valid inputs", {
  data <- expand.grid(age = 0:2, time = 2000:2001, sex = 1:2)
  data$popn <- seq_len(nrow(data))
  data$deaths <- rev(seq_len(nrow(data)))
  data$income <- rnorm(n = nrow(data))
  formula <- deaths ~ age:sex + time
  mod_unfit <- mod_pois(formula = formula,
                        data = data,
                        exposure = popn) |>
    set_covariates(~ income) |>
    set_datamod_overcount(rate = data.frame(mean = 0.1, disp = 0.3))
  mod_fit <- fit(mod_unfit)
  mod_fit_unfit <- unfit(mod_unfit)
  nms <- c("draws_effectfree",
           "draws_hyper",
           "draws_hyperrandfree",
           "draws_coef_covariates",
           "draws_disp",
           "point_effectfree",
           "point_hyper",
           "point_hyperrandfree",
           "point_disp",
           "point_coef_covariates",
           "point_datamod_param",
           "computations",
           "oldpar")
  expect_true(all(nms %in% names(mod_unfit)))
  expect_true(all(nms %in% names(mod_fit)))
  expect_true(all(nms %in% names(mod_fit_unfit)))
  for (nm in nms)
    expect_false(isTRUE(all.equal(mod_fit[[nm]], mod_unfit[[nm]])))
  for (nm in nms)
    expect_true(isTRUE(all.equal(mod_fit_unfit[[nm]], mod_unfit[[nm]])))
})
