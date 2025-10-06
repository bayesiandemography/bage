
## 'draw_vals_and_record' -----------------------------------------------------

test_that("'draw_vals_and_record' works", {
  set.seed(10)
  data <- expand.grid(age = 0:4, time = 2000:2005, sex = c("F", "M"))
  data$popn <- rpois(n = nrow(data), lambda = 100)
  data$deaths <- rpois(n = nrow(data), lambda = 10)
  formula <- deaths ~ age * sex * time
  mod <- mod_pois(formula = formula,
                  data = data,
                  exposure = popn)
  data <- make_fit_data(mod, aggregate = FALSE)
  parameters <- make_fit_parameters(mod)
  map <- make_fit_map(mod)
  random <- make_fit_random(mod)
  f <- TMB::MakeADFun(data = data,
                      parameters = parameters,
                      map = map,
                      DLL = "bage",
                      random = random,
                      silent = TRUE)
  out <- optimize_adfun(f = f, quiet = TRUE, optimizer = "multi", random = random, map = map,
                        is_test_nonconv = FALSE)
  est_prec <- extract_est_prec(f = out$f, has_random_effects = TRUE)
  ans <- draw_vals_and_record(mod = mod,
                              est = est_prec$est,
                              prec = est_prec$prec,
                              map = map,
                              max_jitter = max_jitter)
  expect_identical(ncol(ans$draws_effectfree), 1000L)
  expect_identical(length(ans$point_effectfree), nrow(ans$draws_effectfree))
})


## 'extract_est_prec' ---------------------------------------------------------

test_that("'extract_est_prec' works", {
  set.seed(10)
  data <- expand.grid(age = 0:4, time = 2000:2005, sex = c("F", "M"))
  data$popn <- rpois(n = nrow(data), lambda = 100)
  data$deaths <- rpois(n = nrow(data), lambda = 10)
  formula <- deaths ~ age * sex * time
  mod <- mod_pois(formula = formula,
                  data = data,
                  exposure = popn)
  data <- make_fit_data(mod, aggregate = FALSE)
  parameters <- make_fit_parameters(mod)
  map <- make_fit_map(mod)
  random <- make_fit_random(mod)
  f <- TMB::MakeADFun(data = data,
                      parameters = parameters,
                      map = map,
                      DLL = "bage",
                      random = random,
                      silent = TRUE)
  out <- optimize_adfun(f = f, quiet = TRUE, optimizer = "multi", random = random, map = map,
                        is_test_nonconv = FALSE)
  ans <- extract_est_prec(f = out$f, has_random_effects = TRUE)
  expect_setequal(names(ans), c("est", "prec"))
})


## 'fit_default' --------------------------------------------------------------

test_that("'fit_default' works with pois, optimzier is 'multi'", {
  set.seed(10)
  data <- expand.grid(age = 0:4, time = 2000:2005, sex = c("F", "M"))
  data$popn <- rpois(n = nrow(data), lambda = 100)
  data$deaths <- rpois(n = nrow(data), lambda = 10)
  formula <- deaths ~ age * sex * time
  mod <- mod_pois(formula = formula,
                  data = data,
                  exposure = popn) |>
    set_prior(age ~ AR()) |>
    set_prior(age:sex ~ RW2(sd = 0)) |>
    set_prior(age:sex:time ~ AR())
  ans_obtained <- fit_default(mod,
                              optimizer = "multi",
                              quiet = TRUE,
                              aggregate = TRUE,
                              max_jitter = 1e-4,
                              start_oldpar = FALSE)
  expect_s3_class(ans_obtained, "bage_mod")
})

test_that("'fit_default' works with pois, optimzier is 'nlminb'", {
  set.seed(0)
  data <- expand.grid(age = 0:9, time = 2000:2005, sex = c("F", "M"))
  data$popn <- rpois(n = nrow(data), lambda = 100)
  data$deaths <- rpois(n = nrow(data), lambda = 10)
  formula <- deaths ~ age + sex + time
  mod <- mod_pois(formula = formula,
                  data = data,
                  exposure = popn)
  ans_obtained <- fit_default(mod,
                              optimizer = "nlminb",
                              quiet = TRUE,
                              aggregate = TRUE,
                              max_jitter = 1e-4,
                              start_oldpar = FALSE)
  expect_s3_class(ans_obtained, "bage_mod")
})

test_that("'fit_default' works with pois - start_oldpar", {
  set.seed(0)
  data <- expand.grid(age = 0:9, time = 2000:2005, sex = c("F", "M"))
  data$popn <- rpois(n = nrow(data), lambda = 100)
  data$deaths <- rpois(n = nrow(data), lambda = 10)
  formula <- deaths ~ age + sex + time
  mod <- mod_pois(formula = formula,
                  data = data,
                  exposure = popn)
  mod <- fit_default(mod, optimizer = "nlminb", quiet = TRUE, aggregate = TRUE,
                     start_oldpar = FALSE)
  ans_obtained <- fit_default(mod,
                              optimizer = "nlminb",
                              quiet = TRUE,
                              aggregate = TRUE,
                              max_jitter = 1e-4,
                              start_oldpar = TRUE)
  expect_s3_class(ans_obtained, "bage_mod")
})

test_that("'fit_default' gives error with 'start_oldpar' if model fitted", {
  set.seed(0)
  data <- expand.grid(age = 0:9, time = 2000:2005, sex = c("F", "M"))
  data$popn <- rpois(n = nrow(data), lambda = 100)
  data$deaths <- rpois(n = nrow(data), lambda = 10)
  formula <- deaths ~ age + sex + time
  mod <- mod_pois(formula = formula,
                  data = data,
                  exposure = popn)
  expect_error(fit_default(mod,
                           optimizer = "nlminb",
                           quiet = TRUE,
                           aggregate = TRUE,
                           max_jitter = 1e-4,
                           start_oldpar = TRUE),
               "`start_oldpar` is TRUE but model has not been fitted.")
})

test_that("'fit_default' gives same answer with and without aggregation - Poisson, has offset", {
  set.seed(10)
  data <- expand.grid(age = 0:4,
                      time = 2000:2005,
                      sex = c("F", "M"),
                      replicate = 1:10)
  data$deaths <- with(data, -1995 + (sex == "F") - age + time)
  data$popn <- runif(n = nrow(data), min = 1000, max = 1200)
  formula <- deaths ~ age + sex + time
  mod <- mod_pois(formula = formula,
                  data = data,
                  exposure = popn)
  mod_ag <- fit_default(mod,
                        optimizer = "multi",
                        quiet = TRUE,
                        aggregate = TRUE,
                        max_jitter = 1e-4,
                        start_oldpar = FALSE)
  mod_nonag <- fit_default(mod,
                           optimizer = "multi",
                           quiet = TRUE,
                           aggregate = FALSE,
                           max_jitter = 1e-4,
                           start_oldpar = FALSE)
  disp_ag <- mod_ag |>
    components() |>
    dplyr::filter(term == "disp") |>
    dplyr::pull(.fitted) |>
    rvec::draws_median()
  disp_nonag <- mod_nonag |>
    components() |>
    dplyr::filter(term == "disp") |>
    dplyr::pull(.fitted) |>
    rvec::draws_median()
  expect_equal(disp_ag, disp_nonag, tolerance = 0.1)
})

test_that("'fit_default' gives same answer with and without aggregation - Poisson, no offset", {
  set.seed(10)
  data <- expand.grid(age = 0:4,
                      time = 2000:2005,
                      sex = c("F", "M"),
                      replicate = 1:10)
  data$deaths <- with(data, -1995 + (sex == "F") - age + time)
  formula <- deaths ~ age + sex + time
  mod <- mod_pois(formula = formula,
                  data = data,
                  exposure = 1)
  mod_ag <- fit_default(mod,
                        optimizer = "multi",
                        quiet = TRUE,
                        aggregate = TRUE,
                        max_jitter = 1e-4,
                        start_oldpar = FALSE)
  mod_nonag <- fit_default(mod,
                           optimizer = "multi",
                           quiet = TRUE,
                           aggregate = FALSE,
                           max_jitter = 1e-4,
                           start_oldpar = FALSE)
  disp_ag <- mod_ag |>
    components() |>
    dplyr::filter(term == "disp") |>
    dplyr::pull(.fitted) |>
    rvec::draws_median()
  disp_nonag <- mod_nonag |>
    components() |>
    dplyr::filter(term == "disp") |>
    dplyr::pull(.fitted) |>
    rvec::draws_median()
  expect_equal(disp_ag, disp_nonag, tolerance = 0.1)
})

test_that("'fit_default' gives same answer with and without aggregation - binomial", {
  set.seed(10)
  data <- expand.grid(age = 0:4,
                      time = 2000:2005,
                      sex = c("F", "M"),
                      replicate = 1:10)
  data$deaths <- with(data, -1995 + (sex == "F") - age + time)
  data$popn <- max(data$deaths) + rpois(n = nrow(data), lambda = 50)
  formula <- deaths ~ age + sex + time
  mod <- mod_binom(formula = formula,
                   data = data,
                   size = popn)
  mod_ag <- fit_default(mod,
                        optimizer = "multi",
                        quiet = TRUE,
                        aggregate = TRUE,
                        max_jitter = 1e-4,
                        start_oldpar = FALSE)
  mod_nonag <- fit_default(mod,
                           optimizer = "multi",
                           quiet = TRUE,
                           aggregate = FALSE,
                           max_jitter = 1e-4,
                           start_oldpar = FALSE)
  disp_ag <- mod_ag |>
    components() |>
    dplyr::filter(term == "disp") |>
    dplyr::pull(.fitted) |>
    rvec::draws_median()
  disp_nonag <- mod_nonag |>
    components() |>
    dplyr::filter(term == "disp") |>
    dplyr::pull(.fitted) |>
    rvec::draws_median()
  expect_equal(disp_ag, disp_nonag, tolerance = 0.1)
})

test_that("'fit_default' gives same answer with and without aggregation - norm, offset = 1", {
  set.seed(10)
  data <- expand.grid(age = 0:4,
                      time = 2000:2005,
                      sex = c("F", "M"),
                      replicate = 1:10)
  mean <- with(data, -1995 + (sex == "F") - age + time)
  data$income <- rnorm(n = nrow(data), mean = mean, sd = 0.1)
  data$wt <- runif(n = nrow(data), min = 1, max = 2)
  formula <- income ~ age + sex + time
  mod <- mod_norm(formula = formula,
                  data = data,
                  weights = 1)
  mod_ag <- fit_default(mod,
                        optimizer = "multi",
                        quiet = TRUE,
                        aggregate = TRUE,
                        max_jitter = 1e-4,
                        start_oldpar = FALSE)
  mod_nonag <- fit_default(mod,
                           optimizer = "multi",
                           quiet = TRUE,
                           aggregate = FALSE,
                           max_jitter = 1e-4,
                           start_oldpar = FALSE)
  disp_ag <- mod_ag |>
    components(original_scale = TRUE, quiet = TRUE) |>
    dplyr::filter(term == "disp") |>
    dplyr::pull(.fitted) |>
    rvec::draws_median()
  disp_nonag <- mod_nonag |>
    components(original_scale = TRUE, quiet = TRUE) |>
    dplyr::filter(term == "disp") |>
    dplyr::pull(.fitted) |>
    rvec::draws_median()
  expect_equal(disp_ag, disp_nonag, tolerance = 0.1)
})

test_that("'fit_default' gives same answer with and without aggregation - norm, offset varying", {
  set.seed(10)
  data <- expand.grid(age = 0:4,
                      time = 2000:2005,
                      sex = c("F", "M"),
                      replicate = 1:10)
  mean <- with(data, -1995 + (sex == "F") - age + time)
  data$income <- rnorm(n = nrow(data), mean = mean, sd = 0.1)
  data$wt <- runif(n = nrow(data), min = 10, max = 20)
  formula <- income ~ age + sex + time
  mod <- mod_norm(formula = formula,
                  data = data,
                  weights = wt)
  mod_ag <- fit_default(mod,
                        optimizer = "multi",
                        quiet = TRUE,
                        aggregate = TRUE,
                        max_jitter = 1e-4,
                        start_oldpar = FALSE)
  mod_nonag <- fit_default(mod,
                           optimizer = "multi",
                           quiet = TRUE,
                           aggregate = FALSE,
                           max_jitter = 1e-4,
                           start_oldpar = FALSE)
  disp_ag <- mod_ag |>
    components(original_scale = TRUE, quiet = TRUE) |>
    dplyr::filter(term == "disp") |>
    dplyr::pull(.fitted) |>
    rvec::draws_median()
  disp_nonag <- mod_nonag |>
    components(original_scale = TRUE, quiet = TRUE) |>
    dplyr::filter(term == "disp") |>
    dplyr::pull(.fitted) |>
    rvec::draws_median()
  expect_equal(disp_ag, disp_nonag, tolerance = 0.1)
})


## 'fit_inner_outer' ----------------------------------------------------------

test_that("'fit_inner_outer' works with with pois", {
  set.seed(0)
  data <- expand.grid(age = 0:5,
                      time = 2000:2003,
                      sex = c("F", "M"),
                      region = c("a", "b"))
  data$popn <- rpois(n = nrow(data), lambda = 1000)
  data$deaths <- rnbinom(n = nrow(data), mu = 0.1 * data$popn, size = 100)
  formula <- deaths ~ age * sex + region * time
  mod <- mod_pois(formula = formula,
                  data = data,
                  exposure = popn)
  set.seed(0)
  ans_inner_outer <- fit_inner_outer(mod,
                                     optimizer = "nlminb",
                                     quiet = TRUE,
                                     start_oldpar = FALSE,
                                     vars_inner = c("age", "sex"))
  set.seed(0)
  ans_default <- fit_default(mod,
                             optimizer = "nlminb",
                             quiet = TRUE,
                             start_oldpar = FALSE,
                             max_jitter = 1e-4,
                             aggregate = TRUE)
  aug_inner_outer <- ans_inner_outer |>
    augment()
  fit_inner_outer <- rvec::draws_median(aug_inner_outer$.fitted)
  aug_default <- ans_default |>
    augment()
  fit_default <- rvec::draws_median(aug_default$.fitted)
  expect_true(cor(fit_inner_outer, fit_default) > 0.98)
})

test_that("'fit_inner_outer' works with with binom", {
  set.seed(0)
  data <- expand.grid(age = 0:5,
                      time = 2000:2003,
                      sex = c("F", "M"),
                      region = c("a", "b"))
  data$popn <- rpois(n = nrow(data), lambda = 1000)
  data$deaths <- rbinom(n = nrow(data), prob = 0.2, size = data$popn)
  formula <- deaths ~ age * sex + region * time
  mod <- mod_binom(formula = formula,
                   data = data,
                   size = popn)
  set.seed(0)
  ans_inner_outer <- fit_inner_outer(mod,
                                     optimizer = "nlminb",
                                     quiet = TRUE,
                                     start_oldpar = FALSE,
                                     vars_inner = NULL)
  set.seed(0)
  ans_default <- fit_default(mod,
                             optimizer = "nlminb",
                             quiet = TRUE,
                             start_oldpar = FALSE,
                             max_jitter = 1e-4,
                             aggregate = TRUE)
  aug_inner_outer <- ans_inner_outer |>
    augment()
  fit_inner_outer <- rvec::draws_median(aug_inner_outer$.fitted)
  aug_default <- ans_default |>
    augment()
  fit_default <- rvec::draws_median(aug_default$.fitted)
  expect_true(cor(fit_inner_outer, fit_default) > 0.98)
})

test_that("'fit_inner_outer' works with with norm", {
  set.seed(0)
  data <- expand.grid(age = 0:5,
                      time = 2000:2003,
                      sex = c("F", "M"),
                      region = c("a", "b"))
  data$wt <- rpois(n = nrow(data), lambda = 10)
  data$income <- rnorm(n = nrow(data), mean = data$age + data$time/100, sd = 5 / sqrt(data$wt))
  formula <- income ~ age * sex + region * time
  mod <- mod_norm(formula = formula,
                  data = data,
                  weights = wt)
  set.seed(0)
  ans_inner_outer <- fit_inner_outer(mod,
                                     optimizer = "BFGS",
                                     quiet = TRUE,
                                     start_oldpar = FALSE,
                                     vars_inner = c("age", "sex"))
  set.seed(0)
  ans_default <- fit_default(mod,
                             optimizer = "BFGS",
                             quiet = TRUE,
                             start_oldpar = FALSE,
                             max_jitter = 1e-4,
                             aggregate = TRUE)
  aug_inner_outer <- ans_inner_outer |>
    augment()
  fit_inner_outer <- rvec::draws_median(aug_inner_outer$.fitted)
  aug_default <- ans_default |>
    augment()
  fit_default <- rvec::draws_median(aug_default$.fitted)
  expect_true(cor(fit_inner_outer, fit_default) > 0.98)
})

test_that("'fit_inner_outer' throws error when 'start_oldpar' is TRUE", {
  set.seed(0)
  data <- expand.grid(age = 0:5,
                      time = 2000:2003,
                      sex = c("F", "M"),
                      region = c("a", "b"))
  data$wt <- rpois(n = nrow(data), lambda = 10)
  data$income <- rnorm(n = nrow(data), mean = data$age + data$time/100, sd = 5 / sqrt(data$wt))
  formula <- income ~ age * sex + region * time
  mod <- mod_norm(formula = formula,
                  data = data,
                  weights = wt)
  set.seed(0)
  expect_error(fit_inner_outer(mod,
                               optimizer = "BFGS",
                               quiet = TRUE,
                               start_oldpar = TRUE,
                               vars_inner = c("age", "sex")),
               "`start_oldpar` must be FALSE when using \"inner-outer\" method.")
})

test_that("'fit_inner_outer' throws error when model has covariates", {
  set.seed(0)
  data <- expand.grid(age = 0:5,
                      time = 2000:2003,
                      sex = c("F", "M"),
                      region = c("a", "b"))
  data$wt <- rpois(n = nrow(data), lambda = 10)
  data$income <- rnorm(n = nrow(data), mean = data$age + data$time/100, sd = 5 / sqrt(data$wt))
  data$wealth <- rnorm(n = nrow(data))
  formula <- income ~ age * sex + region * time
  mod <- mod_norm(formula = formula,
                  data = data,
                  weights = wt) |>
    set_covariates(~ wealth)  
  set.seed(0)
  expect_error(fit_inner_outer(mod,
                               optimizer = "BFGS",
                               quiet = TRUE,
                               start_oldpar = FALSE,
                               vars_inner = c("age", "sex")),
               "\"inner-outer\" method cannot be used with models that include covariates.")
})

test_that("'fit_inner_outer' throws error when model has data model", {
  set.seed(0)
  data <- expand.grid(age = 0:5,
                      time = 2000:2003,
                      sex = c("F", "M"),
                      region = c("a", "b"))
  data$wt <- rpois(n = nrow(data), lambda = 10)
  data$income <- rnorm(n = nrow(data), mean = data$age + data$time/100, sd = 5 / sqrt(data$wt))
  formula <- income ~ age * sex + region * time
  mod <- mod_norm(formula = formula,
                  data = data,
                  weights = wt) |>
    set_datamod_noise(sd = 0.3)
  set.seed(0)
  expect_error(fit_inner_outer(mod,
                               optimizer = "BFGS",
                               quiet = TRUE,
                               start_oldpar = FALSE,
                               vars_inner = c("age", "sex")),
               "\"inner-outer\" method cannot be used with models that include a data model.")
})




## 'make_f_new' ---------------------------------------------------------------

test_that("'make_f_new' works", {
  set.seed(10)
  data <- expand.grid(age = 0:4, time = 2000:2005, sex = c("F", "M"))
  data$popn <- rpois(n = nrow(data), lambda = 100)
  data$deaths <- rpois(n = nrow(data), lambda = 10)
  formula <- deaths ~ age * sex * time
  mod <- mod_pois(formula = formula,
                  data = data,
                  exposure = popn)
  data <- make_fit_data(mod, aggregate = FALSE)
  parameters <- make_fit_parameters(mod)
  map <- make_fit_map(mod)
  random <- make_fit_random(mod)
  f_old <- TMB::MakeADFun(data = data,
                          parameters = parameters,
                          map = map,
                          DLL = "bage",
                          random = random,
                          silent = TRUE)
  out <- optimize_cg(f = f_old, quiet = TRUE)
  f_new <- make_f_new(f_old = f_old,
                      quiet = TRUE,
                      data = data,
                      random = random,
                      map = map,
                      optimizer_old = "CG",
                      optimizer_new = "BFGS")
  expect_true(identical(names(f_new), names(f_old)))
  f_new <- make_f_new(f_old = f_old,
                      quiet = TRUE,
                      data = data,
                      random = NULL,
                      map = map,
                      optimizer_old = "CG",
                      optimizer_new = "BFGS")
  expect_true(identical(names(f_new), names(f_old)))
  expect_snapshot(f <- make_f_new(f_old = f_old,
                                  quiet = FALSE,
                                  data = data,
                                  random = random,
                                  map = map,
                                  optimizer_old = "CG",
                                  optimizer_new = "BFGS"))
})


## 'make_fit_data' ------------------------------------------------------------

test_that("'make_fit_data' works - no covariates, no data model", {
  set.seed(10)
  data <- expand.grid(age = 0:4, time = 2000:2005, sex = c("F", "M"))
  data$popn <- rpois(n = nrow(data), lambda = 100)
  data$deaths <- rpois(n = nrow(data), lambda = 10)
  formula <- deaths ~ age * sex * time
  mod <- mod_pois(formula = formula,
                  data = data,
                  exposure = popn)
  ans_ag <- make_fit_data(mod = mod, aggregate = TRUE)
  expect_true(is.list(ans_ag))
  ans_nonag <- make_fit_data(mod = mod, aggregate = FALSE)
  expect_true(is.list(ans_nonag))
})

test_that("'make_fit_data' works - with covariates", {
  set.seed(10)
  data <- expand.grid(age = 0:4, time = 2000:2005, sex = c("F", "M"), reg = letters[1:5])
  data$popn <- rpois(n = nrow(data), lambda = 100)
  data$deaths <- rpois(n = nrow(data), lambda = 10)
  data$income <- rnorm(n = nrow(data))
  formula <- deaths ~ age * sex * time
  mod <- mod_pois(formula = formula,
                  data = data,
                  exposure = popn) |>
    set_covariates(~income + reg)
  ans_ag <- make_fit_data(mod = mod, aggregate = TRUE)
  expect_true(is.list(ans_ag))
  ans_nonag <- make_fit_data(mod = mod, aggregate = FALSE)
  expect_true(is.list(ans_nonag))
  expect_identical(ncol(ans_nonag$matrix_covariates), 5L)
})

test_that("'make_fit_data' works - with data model", {
  set.seed(10)
  data <- expand.grid(age = 0:4, time = 2000:2005, sex = c("F", "M"))
  data$popn <- rpois(n = nrow(data), lambda = 100)
  data$deaths <- rpois(n = nrow(data), lambda = 10)
  formula <- deaths ~ age * sex * time
  mod <- mod_pois(formula = formula,
                  data = data,
                  exposure = popn) |>
    set_datamod_undercount(prob = data.frame(mean = 0.9, disp = 0.2))
  ans_ag <- make_fit_data(mod = mod, aggregate = TRUE)
  expect_true(is.list(ans_ag))
  ans_nonag <- make_fit_data(mod = mod, aggregate = FALSE)
  expect_true(is.list(ans_nonag))
})


## 'make_fit_datamod_consts' --------------------------------------------------

test_that("'make_fit_datamod_consts' works with valid inputs", {
  set.seed(10)
  data <- expand.grid(age = 0:4, time = 2000:2005, sex = c("F", "M"))
  data$popn <- rpois(n = nrow(data), lambda = 100)
  data$deaths <- rpois(n = nrow(data), lambda = 10)
  data$income <- rnorm(n = nrow(data))
  formula <- deaths ~ age * sex * time
  mod <- mod_pois(formula = formula,
                  data = data,
                  exposure = popn)
  expect_identical(make_fit_datamod_consts(mod), double())
  mod <- mod |>
    set_datamod_undercount(prob = data.frame(mean = 0.5, disp = 0.2))
  expect_identical(make_fit_datamod_consts(mod),
                   c(0.5, 0.2))
})


## 'make_fit_datamod_matrices' ------------------------------------------------

test_that("'make_fit_datamod_matrices' works with valid inputs", {
  set.seed(10)
  data <- expand.grid(age = 0:4, time = 2000:2005, sex = c("F", "M"))
  data$popn <- rpois(n = nrow(data), lambda = 100)
  data$deaths <- rpois(n = nrow(data), lambda = 10)
  data$income <- rnorm(n = nrow(data))
  formula <- deaths ~ age * sex * time
  mod <- mod_pois(formula = formula,
                  data = data,
                  exposure = popn)
  expect_identical(make_fit_datamod_matrices(mod), list())
  mod <- mod |>
    set_datamod_undercount(prob = data.frame(mean = 0.5, disp = 0.2))
  expect_identical(make_fit_datamod_matrices(mod),
                   list(mod$datamod$prob_matrix_outcome))
})


## 'make_fit_datamod_param' ---------------------------------------------------

test_that("'make_fit_datamod_param' works with valid inputs", {
  set.seed(10)
  data <- expand.grid(age = 0:4, time = 2000:2005, sex = c("F", "M"))
  data$popn <- rpois(n = nrow(data), lambda = 100)
  data$deaths <- rpois(n = nrow(data), lambda = 10)
  data$income <- rnorm(n = nrow(data))
  formula <- deaths ~ age * sex * time
  mod <- mod_pois(formula = formula,
                  data = data,
                  exposure = popn)
  expect_identical(make_fit_datamod_param(mod), double())
  mod <- mod |>
    set_datamod_undercount(prob = data.frame(mean = 0.5, disp = 0.2))
  expect_identical(make_fit_datamod_param(mod),
                   0)
})


## 'make_fit_i_datamod' -------------------------------------------------------

test_that("'make_fit_i_datamod' with valid inputs", {
  set.seed(10)
  data <- expand.grid(age = 0:4, time = 2000:2005, sex = c("F", "M"))
  data$popn <- rpois(n = nrow(data), lambda = 100)
  data$deaths <- rpois(n = nrow(data), lambda = 10)
  data$income <- rnorm(n = nrow(data))
  formula <- deaths ~ age * sex * time
  mod <- mod_pois(formula = formula,
                  data = data,
                  exposure = popn)
  expect_identical(make_fit_i_datamod(mod), 0L)
  mod <- mod |>
    set_datamod_undercount(prob = data.frame(mean = 0.5, disp = 0.2))
  expect_identical(make_fit_i_datamod(mod), 5000L)
})


## 'make_fit_map' -------------------------------------------------------------

test_that("'make_fit_map' works with no parameters fixed", {
    set.seed(0)
    data <- expand.grid(time = 2000:2009,
                        region = 1:2,
                        SEX = c("F", "M"))
    data$popn <- rpois(n = nrow(data), lambda = 100)
    data$deaths <- rpois(n = nrow(data), lambda = 10)
    formula <- deaths ~ time * SEX + region
    mod <- mod_pois(formula = formula,
                    data = data,
                    exposure = popn)
    ans_obtained <- make_fit_map(mod)
    ans_expected <- NULL
    expect_identical(ans_obtained, ans_expected)
})

test_that("'make_fit_map' works when 'effectfree' contains known values", {
    set.seed(0)
    data <- expand.grid(time = 0:3,
                        SEX = c("F", "M"))
    data$popn <- rpois(n = nrow(data), lambda = 100)
    data$deaths <- rpois(n = nrow(data), lambda = 10)
    formula <- deaths ~ time * SEX
    mod <- mod_pois(formula = formula,
                    data = data,
                    exposure = popn)
    mod <- set_prior(mod, SEX ~ Known(c(0.1, -0.1)))
    ans_obtained <- make_fit_map(mod)
    ans_expected <- list(effectfree = factor(c("(Intercept)" = 1,
                                            time = 2,
                                            time = 3,
                                            time = 4,
                                            time = 5,
                                            SEX = NA,
                                            SEX = NA,
                                            "time:SEX" = 6,
                                            "time:SEX" = 7,
                                            "time:SEX" = 8,
                                            "time:SEX" = 9,
                                            "time:SEX" = 10,
                                            "time:SEX" = 11,
                                            "time:SEX" = 12,
                                            "time:SEX" = 13)))
    expect_identical(ans_obtained, ans_expected)
})

test_that("'make_fit_map' works dispersion is 0", {
    set.seed(0)
    data <- expand.grid(time = 2000:2009,
                        region = 1:2,
                        SEX = c("F", "M"))
    data$popn <- rpois(n = nrow(data), lambda = 100)
    data$deaths <- rpois(n = nrow(data), lambda = 10)
    formula <- deaths ~ time * SEX + region
    mod <- mod_pois(formula = formula,
                    data = data,
                    exposure = popn)
    mod <- set_disp(mod, mean = 0)
    ans_obtained <- make_fit_map(mod)
    ans_expected <- list(log_disp = factor(NA))
    expect_identical(ans_obtained, ans_expected)
})

test_that("'make_fit_map' works when effectfree has known values", {
    set.seed(0)
    data <- expand.grid(time = 0:3,
                        SEX = c("F", "M"))
    data$popn <- rpois(n = nrow(data), lambda = 100)
    data$deaths <- rpois(n = nrow(data), lambda = 10)
    formula <- deaths ~ time * SEX
    mod <- mod_pois(formula = formula,
                    data = data,
                    exposure = popn) |>
      set_prior(time ~ RW(sd = 0)) |>
      set_prior(time:SEX ~ RW(sd = 0))
    mod <- set_prior(mod, SEX ~ Known(c(0.1, -0.1)))
    ans_obtained <- make_fit_map(mod)
    ans_expected <- list(effectfree = factor(c("(Intercept)" = 1,
                                            time = 2,
                                            time = 3,
                                            time = 4,
                                            SEX = NA,
                                            SEX = NA,
                                            "time:SEX" = 5,
                                            "time:SEX" = 6,
                                            "time:SEX" = 7,
                                            "time:SEX" = 8,
                                            "time:SEX" = 9,
                                            "time:SEX" = 10)))
    expect_identical(ans_obtained, ans_expected)
})


## 'make_fit_parameters' ------------------------------------------------------

test_that("'make_fit_parameters' works - no covariates", {
  set.seed(10)
  data <- expand.grid(age = 0:4, time = 2000:2005, sex = c("F", "M"))
  data$popn <- rpois(n = nrow(data), lambda = 100)
  data$deaths <- rpois(n = nrow(data), lambda = 10)
  formula <- deaths ~ age * sex * time
  mod <- mod_pois(formula = formula,
                  data = data,
                  exposure = popn)
  ans <- make_fit_parameters(mod)
  expect_true(is.list(ans))
  expect_identical(names(ans), c("effectfree", "hyper", "hyperrandfree", "log_disp",
                                 "coef_covariates", "datamod_param"))
})

test_that("'make_fit_parameters' works - has covariates", {
  set.seed(10)
  data <- expand.grid(age = 0:4, time = 2000:2005, sex = c("F", "M"))
  data$popn <- rpois(n = nrow(data), lambda = 100)
  data$deaths <- rpois(n = nrow(data), lambda = 10)
  data$income <- rnorm(n = nrow(data))
  formula <- deaths ~ age * sex * time
  mod <- mod_pois(formula = formula,
                  data = data,
                  exposure = popn) |>
    set_covariates(~income)
  ans <- make_fit_parameters(mod)
  expect_true(is.list(ans))
  expect_identical(names(ans), c("effectfree", "hyper", "hyperrandfree", "log_disp",
                                 "coef_covariates", "datamod_param"))
  expect_identical(ans$coef_covariates, c(income = 0))
})

test_that("'make_fit_parameters' works - has datamod", {
  set.seed(10)
  data <- expand.grid(age = 0:4, time = 2000:2005, sex = c("F", "M"))
  data$popn <- rpois(n = nrow(data), lambda = 100)
  data$deaths <- rpois(n = nrow(data), lambda = 10)
  formula <- deaths ~ age * sex * time
  mod <- mod_pois(formula = formula,
                  data = data,
                  exposure = popn) |>
    set_datamod_overcount(rate = data.frame(mean = 0.2, disp = 0.5))
  ans <- make_fit_parameters(mod)
  expect_true(is.list(ans))
  expect_identical(names(ans), c("effectfree", "hyper", "hyperrandfree", "log_disp",
                                 "coef_covariates", "datamod_param"))
  expect_identical(ans$datamod_param, 0)
})


## 'make_fit_random' --------------------------------------------------------------

test_that("'make_fit_random' works when no hyper, no hyperrandfree", {
  set.seed(10)
  data <- expand.grid(age = 0:4, time = 2000:2005, sex = c("F", "M"))
  data$popn <- rpois(n = nrow(data), lambda = 100)
  data$deaths <- rpois(n = nrow(data), lambda = 10)
  data$income <- rnorm(n = nrow(data))
  mod <- mod_pois(deaths ~ 1,
                  data = data,
                  exposure = popn)
  expect_identical(make_fit_random(mod), NULL)
})

test_that("'make_fit_random' works when hyper, no hyperrandfree", {
  data <- expand.grid(age = 0:4, time = 2000:2005, sex = c("F", "M"))
  data$popn <- rpois(n = nrow(data), lambda = 100)
  data$deaths <- rpois(n = nrow(data), lambda = 10)
  data$income <- rnorm(n = nrow(data))
  mod <- mod_pois(deaths ~ age,
                  data = data,
                  exposure = popn)
  expect_identical(make_fit_random(mod), "effectfree")
})

test_that("'make_fit_random' works when hyper, hyperrand", {
  data <- expand.grid(age = 0:4, time = 2000:2005, sex = c("F", "M"))
  data$popn <- rpois(n = nrow(data), lambda = 100)
  data$deaths <- rpois(n = nrow(data), lambda = 10)
  data$income <- rnorm(n = nrow(data))
  mod <- mod_pois(deaths ~ age,
                  data = data,
                  exposure = popn) |>
    set_prior(age ~ Lin())
  expect_identical(make_fit_random(mod), c("effectfree", "hyperrandfree"))
})

test_that("'make_fit_random' works when hyper, hyperrand, covariates", {
  set.seed(10)
  data <- expand.grid(age = 0:4, time = 2000:2005, sex = c("F", "M"))
  data$popn <- rpois(n = nrow(data), lambda = 100)
  data$deaths <- rpois(n = nrow(data), lambda = 10)
  data$income <- rnorm(n = nrow(data))
  formula <- deaths ~ age * sex * time
  mod <- mod_pois(formula = formula,
                  data = data,
                  exposure = popn) |>
    set_prior(age ~ Lin()) |>
    set_covariates(~income)
  expect_identical(make_fit_random(mod), c("effectfree", "hyperrandfree", "coef_covariates"))
})

test_that("'make_fit_random' works when hyper, hyperrand, covariates, data model", {
  set.seed(10)
  data <- expand.grid(age = 0:4, time = 2000:2005, sex = c("F", "M"))
  data$popn <- rpois(n = nrow(data), lambda = 100)
  data$deaths <- rpois(n = nrow(data), lambda = 10)
  data$income <- rnorm(n = nrow(data))
  formula <- deaths ~ age * sex * time
  mod <- mod_pois(formula = formula,
                  data = data,
                  exposure = popn) |>
    set_prior(age ~ Lin()) |>
    set_covariates(~income) |>
    set_datamod_undercount(prob = data.frame(mean = 0.8, disp = 0.2))
  expect_identical(make_fit_random(mod),
                   c("effectfree",
                     "hyperrandfree",
                     "coef_covariates",
                     "datamod_param"))
})



## 'make_fit_times' -----------------------------------------------------------

test_that("'make_fit_times' works", {
  t_start <- Sys.time()
  t_optim <- t_start + 20
  t_report <- t_optim + 25
  t_end <- t_report + 10
  ans_obtained <- make_fit_times(t_start = t_start,
                        t_optim = t_optim,
                        t_report = t_report,
                        t_end = t_end)
  ans_expected <- list(time_total = 55,
                       time_max = 25,
                       time_draw = 10)
  expect_identical(ans_obtained, ans_expected)
})


## 'optimize_adfun' -----------------------------------------------------------

test_that("'optimize_adfun' works - converges", {
  set.seed(10)
  data <- expand.grid(age = 0:4, time = 2000:2005, sex = c("F", "M"))
  data$popn <- rpois(n = nrow(data), lambda = 100)
  data$deaths <- rpois(n = nrow(data), lambda = 10)
  formula <- deaths ~ age * sex * time
  mod <- mod_pois(formula = formula,
                  data = data,
                  exposure = popn)
  data <- make_fit_data(mod, aggregate = FALSE)
  parameters <- make_fit_parameters(mod)
  map <- make_fit_map(mod)
  random <- make_fit_random(mod)
  f <- TMB::MakeADFun(data = data,
                      parameters = parameters,
                      map = map,
                      DLL = "bage",
                      random = random,
                      silent = TRUE)
  ans <- optimize_adfun(f = f, quiet = TRUE, optimizer = "multi",
                        data = data, random = random, map = map,
                        is_test_nonconv = FALSE)
  expect_setequal(names(ans), c("f", "iter", "message", "converged", "optimizer"))
  expect_true(ans$converged)
})

test_that("'optimize_adfun' works - does not converge", {
  set.seed(10)
  data <- expand.grid(age = 0:4, time = 2000:2005, sex = c("F", "M"))
  data$popn <- rpois(n = nrow(data), lambda = 100)
  data$deaths <- round(runif(n = nrow(data), max = 1000000))
  formula <- deaths ~ age * sex * time
  mod <- mod_pois(formula = formula,
                  data = data,
                  exposure = popn)
  data <- make_fit_data(mod, aggregate = FALSE)
  parameters <- make_fit_parameters(mod)
  map <- make_fit_map(mod)
  random <- make_fit_random(mod)
  f <- TMB::MakeADFun(data = data,
                      parameters = parameters,
                      map = map,
                      DLL = "bage",
                      random = random,
                      silent = TRUE)
  ans <- optimize_adfun(f = f, quiet = TRUE, optimizer = "multi",
                        data = data, random = random, map = map,
                        is_test_nonconv = TRUE)
  expect_setequal(names(ans), c("f", "iter", "message", "converged", "optimizer"))
  expect_identical(ans$optimizer, "nlminb + BFGS")
  expect_true(ans$converged)
})


test_that("'optimize_adfun' throws correct error when optimizer invalid ", {
  set.seed(10)
  data <- expand.grid(age = 0:4, time = 2000:2005, sex = c("F", "M"))
  data$popn <- rpois(n = nrow(data), lambda = 10)
  data$deaths <- rpois(n = nrow(data), lambda = 0.3)
  formula <- deaths ~ age * sex + time
  mod <- mod_pois(formula = formula,
                  data = data,
                  exposure = popn) |>
    set_prior(time ~ RW2())
  data <- make_fit_data(mod, aggregate = FALSE)
  parameters <- make_fit_parameters(mod)
  map <- make_fit_map(mod)
  random <- make_fit_random(mod)
  f <- TMB::MakeADFun(data = data,
                      parameters = parameters,
                      map = map,
                      DLL = "bage",
                      random = random,
                      silent = TRUE)
  expect_error(optimize_adfun(f = f, quiet = TRUE, optimizer = "wrong",
                              data = data, random = random, map = map,
                              is_test_nonconv = FALSE),
               "Internal error: \"wrong\" is not a valid value for `optimizer`.")
})


## 'optimize_bfgs' ----------------------------------------------------------

test_that("'optimize_bfgs' works", {
  set.seed(10)
  data <- expand.grid(age = 0:4, time = 2000:2005, sex = c("F", "M"))
  data$popn <- rpois(n = nrow(data), lambda = 100)
  data$deaths <- rpois(n = nrow(data), lambda = 10)
  formula <- deaths ~ age * sex * time
  mod <- mod_pois(formula = formula,
                  data = data,
                  exposure = popn)
  data <- make_fit_data(mod, aggregate = FALSE)
  parameters <- make_fit_parameters(mod)
  map <- make_fit_map(mod)
  random <- make_fit_random(mod)
  f <- TMB::MakeADFun(data = data,
                      parameters = parameters,
                      map = map,
                      DLL = "bage",
                      random = random,
                      silent = TRUE)
  ans <- optimize_bfgs(f = f, quiet = TRUE)
  expect_setequal(names(ans), c("f", "iter", "message", "converged", "optimizer"))
  expect_true(ans$converged)
})


## 'optimize_cg' ----------------------------------------------------------

test_that("'optimize_cg' works", {
  set.seed(10)
  data <- expand.grid(age = 0:4, time = 2000:2005, sex = c("F", "M"))
  data$popn <- rpois(n = nrow(data), lambda = 100)
  data$deaths <- rpois(n = nrow(data), lambda = 10)
  formula <- deaths ~ age * sex * time
  mod <- mod_pois(formula = formula,
                  data = data,
                  exposure = popn)
  data <- make_fit_data(mod, aggregate = FALSE)
  parameters <- make_fit_parameters(mod)
  map <- make_fit_map(mod)
  random <- make_fit_random(mod)
  f <- TMB::MakeADFun(data = data,
                      parameters = parameters,
                      map = map,
                      DLL = "bage",
                      random = random,
                      silent = TRUE)
  ans <- optimize_cg(f = f, quiet = TRUE)
  expect_setequal(names(ans), c("f", "iter", "message", "converged", "optimizer"))
  expect_true(ans$converged)
})


## 'optimize_nlminb' ----------------------------------------------------------

test_that("'optimize_nlminb' works", {
  set.seed(10)
  data <- expand.grid(age = 0:4, time = 2000:2005, sex = c("F", "M"))
  data$popn <- rpois(n = nrow(data), lambda = 100)
  data$deaths <- rpois(n = nrow(data), lambda = 10)
  formula <- deaths ~ age * sex * time
  mod <- mod_pois(formula = formula,
                  data = data,
                  exposure = popn)
  data <- make_fit_data(mod, aggregate = FALSE)
  parameters <- make_fit_parameters(mod)
  map <- make_fit_map(mod)
  random <- make_fit_random(mod)
  f <- TMB::MakeADFun(data = data,
                      parameters = parameters,
                      map = map,
                      DLL = "bage",
                      random = random,
                      silent = TRUE)
  ans <- optimize_nlminb(f = f, quiet = TRUE)
  expect_setequal(names(ans), c("f", "iter", "message", "converged", "optimizer"))
  expect_true(ans$converged)  
})



## 'record_metadata' ----------------------------------------------------------

test_that("'record_metadata' works", {
  set.seed(10)
  data <- expand.grid(age = 0:4, time = 2000:2005, sex = c("F", "M"))
  data$popn <- rpois(n = nrow(data), lambda = 100)
  data$deaths <- rpois(n = nrow(data), lambda = 10)
  formula <- deaths ~ age * sex * time
  mod <- mod_pois(formula = formula,
                  data = data,
                  exposure = popn)
  data <- make_fit_data(mod, aggregate = FALSE)
  parameters <- make_fit_parameters(mod)
  map <- make_fit_map(mod)
  random <- make_fit_random(mod)
  f <- TMB::MakeADFun(data = data,
                      parameters = parameters,
                      map = map,
                      DLL = "bage",
                      random = random,
                      silent = TRUE)
  out <- optimize_adfun(f = f, quiet = TRUE, optimizer = "multi", random = random, map = map,
                        is_test_nonconv = FALSE)
  est_prec <- extract_est_prec(f = out$f, has_random_effects = TRUE)
  mod <- draw_vals_and_record(mod = mod,
                              est = est_prec$est,
                              prec = est_prec$prec,
                              map = map,
                              max_jitter = max_jitter)
  times <- make_fit_times(t_start = Sys.time(),
                          t_optim = Sys.time(),
                          t_report = Sys.time(),
                          t_end = Sys.time())
  mod <- record_metadata(mod = mod,
                         est = est_prec$est,
                         optimizer = out$optimizer,
                         iter = out$iter,
                         converged = out$converged,
                         message = out$message,
                         times = times)
  expect_identical(mod$optimizer, "nlminb")
  expect_true(is.data.frame(mod$computations))
})
