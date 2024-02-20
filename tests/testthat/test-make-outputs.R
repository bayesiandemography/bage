
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


## 'insert_draws_known' -------------------------------------------------------

test_that("'insert_draws_known' works", {
    draws <- matrix(1:4, nrow = 4, ncol = 3)
    est <- c(1, -1, 2, -(2:3), 3:4, -4)
    is_fixed <- c(FALSE, TRUE, FALSE, TRUE, TRUE, FALSE, FALSE, TRUE)
    ans_obtained <- insert_draws_known(draws = draws,
                                       est = est,
                                       is_fixed = is_fixed)
    ans_expected <- matrix(est, nrow = 8, ncol = 3)
    expect_identical(ans_obtained, ans_expected)
})


## 'make_combined_matrix_effect_outcome' -----------------------------------------

test_that("'make_combined_matrix_effect_outcome' works with valid inputs", {
    set.seed(0)
    data <- expand.grid(age = 0:9, time = 2000:2005, sex = c("F", "M"))
    data$popn <- rpois(n = nrow(data), lambda = 100)
    data$deaths <- rpois(n = nrow(data), lambda = 10)
    formula <- deaths ~ age * sex + age * time
    mod <- mod_pois(formula = formula,
                    data = data,
                    exposure = popn)
    ans_obtained <- make_combined_matrix_effect_outcome(mod)
    expect_identical(nrow(ans_obtained), nrow(data))
    expect_identical(ncol(ans_obtained), length(mod$terms_effect))
})


## 'make_combined_matrix_effectfree_effect' -----------------------------------------

test_that("'make_combined_matrix_effectfree_effect' works with valid inputs", {
    set.seed(0)
    data <- expand.grid(age = 0:9, time = 2000:2005, sex = c("F", "M"))
    data$popn <- rpois(n = nrow(data), lambda = 100)
    data$deaths <- rpois(n = nrow(data), lambda = 10)
    formula <- deaths ~ age * sex + age * time
    mod <- mod_pois(formula = formula,
                    data = data,
                    exposure = popn)
    ans_obtained <- make_combined_matrix_effectfree_effect(mod)
    expect_identical(nrow(ans_obtained), length(mod$terms_effect))
    expect_identical(ncol(ans_obtained), length(make_terms_effectfree(mod)))
})


## 'make_comp_components' -----------------------------------------------------

test_that("'make_comp_components' works", {
    set.seed(0)
    data <- expand.grid(age = 0:9, time = 2000:2005, sex = c("F", "M"))
    data$popn <- rpois(n = nrow(data), lambda = 100)
    data$deaths <- rpois(n = nrow(data), lambda = 10)
    formula <- deaths ~ age + sex + time
    mod <- mod_pois(formula = formula,
                    data = data,
                    exposure = popn)
    mod <- set_n_draw(mod, n = 1)
    mod <- fit(mod)
    draws <- make_draws_components(mod)
    ans <- make_comp_components(mod)
    expect_identical(nrow(draws), length(ans))
    expect_setequal(ans, c("effect", "hyper", "disp"))
})

## 'make_copies_repdata' ------------------------------------------------------

test_that("'make_copies_repdata' works with valid inputs", {
    data <- tibble::tibble(age = 0:4, deaths = 1:5, population = 11:15)
    n <- 2
    ans_obtained <- make_copies_repdata(data = data, n = n)
    ans_expected <- tibble::tibble(.replicate = factor(rep(c("Original", "Replicate 1", "Replicate 2"),
                                                           each = 5),
                                                       levels = c("Original",
                                                                  "Replicate 1", "Replicate 2")),
                                   age = rep(0:4, 3),
                                   deaths = rep(1:5, 3),
                                   population = rep(11:15, 3))
    expect_identical(ans_obtained, ans_expected)
})


## 'make_draws_comp_raw' ------------------------------------------------------

test_that("'make_draws_comp_raw' works with valid inputs", {
    set.seed(0)
    data <- expand.grid(age = 0:9, time = 2000:2005, sex = c("F", "M"))
    data$popn <- rpois(n = nrow(data), lambda = 100)
    data$deaths <- rpois(n = nrow(data), lambda = 10)
    formula <- deaths ~ age + sex
    mod <- mod_pois(formula = formula,
                    data = data,
                    exposure = popn)
    mod <- fit(mod)
    mod <- set_n_draw(mod, n = 20000)
    ans <- make_draws_comp_raw(mod)
    mean <- unlist(mod$est, use.names = FALSE)[!mod$is_fixed]
    prec <- crossprod(mod$R_prec)
    expect_equal(rowMeans(ans), mean, tolerance = 0.01)
    expect_equal(solve(cov(t(ans))), unname(prec), tolerance = 0.05)
})


## 'make_draws_components' -----------------------------------------------

test_that("'make_draws_components' works", {
    set.seed(0)
    data <- expand.grid(age = 0:9, time = 2000:2005, sex = c("F", "M"))
    data$popn <- rpois(n = nrow(data), lambda = 100)
    data$deaths <- rpois(n = nrow(data), lambda = 10)
    formula <- deaths ~ age + sex + time
    mod <- mod_pois(formula = formula,
                    data = data,
                    exposure = popn)
    mod <- set_prior(mod, age ~ Sp())
    mod <- set_n_draw(mod, n = 1)
    mod <- fit(mod)
    set.seed(0)
    ans_obtained <- make_draws_components(mod)
    set.seed(0)
    est <- mod$est
    is_fixed <- mod$is_fixed
    matrix <- make_combined_matrix_effectfree_effect(mod)
    offset <- make_offsets_effectfree_effect(mod)
    transforms <- make_transforms_hyper(mod)
    draws <- make_draws_comp_raw(mod)
    draws <- insert_draws_known(draws = draws,
                                est = est,
                                is_fixed = is_fixed)
    draws <- transform_draws_hyper(draws,
                                   transforms = transforms)
    draws <- transform_draws_effect(draws = draws,
                                 matrix = matrix,
                                 offset = offset)
    ans_expected <- draws
    expect_identical(ans_obtained, ans_expected)
})


## 'is_same_class' ------------------------------------------------------------

test_that("'is_same_class' returns TRUE when classes same", {
    expect_true(is_same_class(AR1(), AR1()))
    expect_true(is_same_class(1L, 2L))
})

test_that("'is_same_class' returns FALSE when classes different", {
    expect_false(is_same_class(AR1(), N()))
    expect_false(is_same_class(1L, FALSE))
})


## 'make_par_disp' ------------------------------------------------------------

test_that("'make_par_disp' works with bage_mod_pois", {
    set.seed(0)
    data <- expand.grid(age = 0:9, time = 2000:2005, sex = c("F", "M"))
    data$popn <- rpois(n = nrow(data), lambda = 100)
    data$deaths <- rpois(n = nrow(data), lambda = 10)
    formula <- deaths ~ age + time + sex
    mod <- mod_pois(formula = formula,
                    data = data,
                    exposure = popn)
    mod <- fit(mod)
    components <- components(mod)
    meanpar <- exp(make_linpred_effect(mod, components = components))
    disp <- components$.fitted[components$component == "disp"]
    ans_obtained <- make_par_disp(mod,
                                  meanpar = meanpar,
                                  disp = disp)
    set.seed(mod$seed_fitted)
    ans_expected <- rvec::rgamma_rvec(n = length(meanpar),
                                      data$deaths + 1/disp,
                                      data$popn + 1/(disp*meanpar))
    expect_equal(ans_obtained, ans_expected)
})

test_that("'make_par_disp' works with bage_mod_binom", {
    set.seed(0)
    data <- expand.grid(age = 0:9, time = 2000:2005, sex = c("F", "M"))
    data$popn <- rpois(n = nrow(data), lambda = 100)
    data$deaths <- rbinom(n = nrow(data), size = data$popn, prob = 0.3)
    formula <- deaths ~ age + time + sex
    mod <- mod_binom(formula = formula,
                     data = data,
                     size = popn)
    mod <- fit(mod)
    components <- components(mod)
    invlogit <- function(x) 1 / (1 + exp(-x))
    meanpar <- invlogit(make_linpred_effect(mod, components = components))
    disp <- components$.fitted[components$component == "disp"]
    set.seed(1)
    ans_obtained <- make_par_disp(mod,
                                  meanpar = meanpar,
                                  disp = disp)
    set.seed(mod$seed_fitted)
    ans_expected <- rvec::rbeta_rvec(n = length(meanpar),
                                     data$deaths + meanpar/disp,
                                     data$popn - data$deaths + (1 - meanpar)/disp)
    expect_equal(ans_obtained, ans_expected)
})


## 'make_is_fixed' ------------------------------------------------------------

test_that("'make_is_fixed' works when nothing fixed", {
    set.seed(0)
    data <- expand.grid(age = 0:9, time = 2000:2005, sex = c("F", "M"))
    data$popn <- rpois(n = nrow(data), lambda = 100)
    data$deaths <- rpois(n = nrow(data), lambda = 10)
    formula <- deaths ~ age * sex + time
    mod <- mod_pois(formula = formula,
                    data = data,
                    exposure = popn)
    mod <- fit(mod)
    est <- mod$est
    map <- make_map(mod)
    expect_true(is.null(map))
    expect_true(length(unlist(est)) > 0L)
    ans_obtained <- make_is_fixed(est = est, map = map)
    ans_expected <- rep(FALSE, times = length(unlist(est)))
})

test_that("'make_is_fixed' works", {
  set.seed(0)
  data <- expand.grid(age = 0:9, time = 2000:2005, sex = c("F", "M"))
  data$popn <- rpois(n = nrow(data), lambda = 100)
  data$deaths <- rpois(n = nrow(data), lambda = 10)
  formula <- deaths ~ age * sex + time
  mod <- mod_pois(formula = formula,
                  data = data,
                  exposure = popn)
  mod <- fit(mod)
  est <- mod$est
  map <- make_map(mod)
  ans_obtained <- make_is_fixed(est = est, map = map)
  ans_expected <- rep(FALSE, 
                      times = length(est$effectfree)
                      + length(est$hyper)
                      + length(est$log_disp))
  expect_identical(unname(ans_obtained), ans_expected)
})

test_that("'make_is_fixed' works when Known prior", {
    set.seed(0)
    data <- expand.grid(age = 0:9, time = 2000:2005, sex = c("F", "M"))
    data$popn <- rpois(n = nrow(data), lambda = 100)
    data$deaths <- rpois(n = nrow(data), lambda = 10)
    formula <- deaths ~ age * sex + time
    mod <- mod_pois(formula = formula,
                    data = data,
                    exposure = popn)
    mod <- set_prior(mod, sex ~ Known(c(0.1, -0.1)))
    mod <- fit(mod)
    est <- mod$est
    map <- make_map(mod)
    ans_obtained <- make_is_fixed(est = est, map = map)
    ans_expected <- rep(c(FALSE, TRUE, FALSE),
                        times = c(11,
                                  2,
                                  6 + 20 + length(est$hyper) + length(est$log_disp)))
    expect_identical(unname(ans_obtained), ans_expected)
})


## 'make_level_components' ----------------------------------------------------

test_that("'make_level_components' works", {
    set.seed(0)
    data <- expand.grid(age = 0:9, time = 2000:2005, sex = c("F", "M"))
    data$popn <- rpois(n = nrow(data), lambda = 100)
    data$deaths <- rpois(n = nrow(data), lambda = 10)
    formula <- deaths ~ age + sex + time
    mod <- mod_pois(formula = formula,
                    data = data,
                    exposure = popn)
    mod <- set_disp(mod, s = 0)
    mod <- fit(mod)
    comp <- make_comp_components(mod)
    ans <- make_level_components(mod)
    expect_identical(length(ans), length(comp))
})


## 'make_levels_hyper' --------------------------------------------------------

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
    ans_expected <- c(age = "sd",
                      sex = "sd",
                      time = "sd",
                      "age:sex" = "sd")
    expect_identical(ans_obtained, ans_expected)                      
})

## 'make_levels_hyperrand' ----------------------------------------------------

test_that("'make_levels_hyperrand' works", {
    set.seed(0)
    data <- expand.grid(age = 0:9, time = 2000:2005, sex = c("F", "M"))
    data$popn <- rpois(n = nrow(data), lambda = 100)
    data$deaths <- rpois(n = nrow(data), lambda = 10)
    formula <- deaths ~ age + sex:time
    mod <- mod_pois(formula = formula,
                    data = data,
                    exposure = popn)
    mod <- set_prior(mod, sex:time ~ ILin())
    mod <- fit(mod)
    ans_obtained <- make_levels_hyperrand(mod)
    ans_expected <- c("mslope", "mslope")
    expect_identical(ans_obtained, ans_expected)                      
})


## 'make_levels_replicate' ----------------------------------------------------

test_that("'make_levels_replicate' works", {
    ans_obtained <- make_levels_replicate(n = 2, n_row_data = 3)
    ans_expected <- factor(rep(c("Original", "Replicate 1", "Replicate 2"),
                               each = 3),
                           levels = c("Original", "Replicate 1", "Replicate 2"))
    expect_identical(ans_obtained, ans_expected)
})


## 'make_linpred_effect' ---------------------------------------------------------

test_that("'make_linpred_effect' works with valid inputs", {
    set.seed(0)
    data <- expand.grid(age = 0:9, time = 2000:2005, sex = c("F", "M"))
    data$popn <- rpois(n = nrow(data), lambda = 100)
    data$deaths <- rpois(n = nrow(data), lambda = 10)
    formula <- deaths ~ age + sex
    mod <- mod_pois(formula = formula,
                    data = data,
                    exposure = popn)
    mod <- set_n_draw(mod, n_draw = 100L)
    mod <- fit(mod)
    comp <- components(mod)
    set.seed(1)
    ans <- make_linpred_effect(mod = mod,
                            components = comp)
    expect_identical(length(ans), length(mod$outcome))
    expect_s3_class(ans, "rvec")
})


## 'make_scaled_eigen' --------------------------------------------------------

## See also tests for rvnorm_eigen

test_that("'make_scaled_eigen' works with positive definite matrix", {
    set.seed(0)
    prec <- solve(crossprod(matrix(rnorm(25), 5)))
    ans <- make_scaled_eigen(prec)
    expect_identical(dim(ans), dim(prec))
})

test_that("'make_scaled_eigen' works with non-negative definite matrix", {
    set.seed(0)
    prec <- solve(crossprod(matrix(rnorm(25), 5)))
    prec[5,] <- 0
    prec[,5] <- 0
    ans <- make_scaled_eigen(prec)
    expect_identical(dim(ans), dim(prec))
})


## 'make_term_components' -----------------------------------------------------

test_that("'make_term_components' works", {
    set.seed(0)
    data <- expand.grid(age = 0:9, time = 2000:2005, sex = c("F", "M"))
    data$popn <- rpois(n = nrow(data), lambda = 100)
    data$deaths <- rpois(n = nrow(data), lambda = 10)
    formula <- deaths ~ age + sex + time
    mod <- mod_pois(formula = formula,
                    data = data,
                    exposure = popn)
    mod <- set_disp(mod, s = 0)
    mod <- set_prior(mod, age ~ Sp())
    mod <- set_n_draw(mod, n = 1)       
    mod <- fit(mod)
    comp <- make_comp_components(mod)
    ans <- make_term_components(mod)
    expect_identical(length(ans), length(comp))
})


## 'make_transforms_hyper' ---------------------------------------------------------

test_that("'make_transforms_hyper' works", {
    set.seed(0)
    data <- expand.grid(age = 0:9, time = 2000:2005, sex = c("F", "M"))
    data$popn <- rpois(n = nrow(data), lambda = 100)
    data$deaths <- rpois(n = nrow(data), lambda = 10)
    formula <- deaths ~ age + sex + time
    mod <- mod_pois(formula = formula,
                    data = data,
                    exposure = popn)
    mod <- fit(mod)
    ans_obtained <- make_transforms_hyper(mod)
    invlogit2 <- function(x) {
        ans <- exp(x) / (1 + exp(x))
        2 * ans - 1
    }
    ans_expected <- c(rep(list(NULL), 19),
                      rep(list(exp), 3),
                      list(exp))
    expect_identical(unname(ans_obtained), ans_expected,
                     ignore_function_env = TRUE)
})


## 'rmvnorm_chol', 'rmvnorm_eigen' --------------------------------------------

test_that("'rmvnorm_chol' and 'rmvnorm_eigen' give the same answer", {
    set.seed(0)
    prec <- crossprod(matrix(rnorm(25), 5))
    mean <- rnorm(5)
    R_prec <- chol(prec)
    scaled_eigen <- make_scaled_eigen(prec)
    ans_chol <- rmvnorm_chol(n = 100000, mean = mean, R_prec = R_prec)
    ans_eigen <- rmvnorm_eigen(n = 100000, mean = mean, scaled_eigen = scaled_eigen)
    expect_equal(rowMeans(ans_chol), rowMeans(ans_eigen), tolerance = 0.02)
    expect_equal(cov(t(ans_chol)), cov(t(ans_eigen)), tolerance = 0.02)
})


## 'transform_draws_hyper' ----------------------------------------------------

test_that("'transform_draws_hyper' works", {
    set.seed(0)
    data <- expand.grid(age = 0:9, time = 2000:2005, sex = c("F", "M"))
    data$popn <- rpois(n = nrow(data), lambda = 100)
    data$deaths <- rpois(n = nrow(data), lambda = 10)
    formula <- deaths ~ age + sex + time
    mod <- mod_pois(formula = formula,
                    data = data,
                    exposure = popn)
    mod <- set_n_draw(mod, 5)
    mod <- fit(mod)
    draws <- make_draws_comp_raw(mod)
    transforms <- make_transforms_hyper(mod)
    ans_obtained <- transform_draws_hyper(draws = draws,
                                          transforms = transforms)
    ans_expected <- rbind(draws[1:19, ],
                          exp(draws[20:22, ]),
                          exp(draws[23, ]))
    expect_identical(unname(ans_obtained), ans_expected)
})


## 'transform_draws_effect' ------------------------------------------------------

test_that("'transform_draws_effect' works", {
  set.seed(0)
  data <- expand.grid(age = 0:9, time = 2000:2005, sex = c("F", "M"))
  data$popn <- rpois(n = nrow(data), lambda = 100)
  data$deaths <- rpois(n = nrow(data), lambda = 10)
  formula <- deaths ~ age + sex + time
  mod <- mod_pois(formula = formula,
                  data = data,
                  exposure = popn)
  mod <- set_prior(mod, age ~ Sp())
  mod <- set_n_draw(mod, n = 5)
  mod <- fit(mod)
  est <- mod$est
  is_fixed <- mod$is_fixed
  matrix <- make_combined_matrix_effectfree_effect(mod)
  offset <- make_offsets_effectfree_effect(mod)
  draws <- make_draws_comp_raw(mod)
  draws <- insert_draws_known(draws = draws,
                              est = est,
                              is_fixed = is_fixed)
  ans_obtained <- transform_draws_effect(draws = draws,
                                         matrix = matrix,
                                         offset = offset)
  ans_expected <- rbind(matrix %*% draws[1:16, ] + offset,
                        draws[17:20, ])
  expect_identical(ans_obtained, ans_expected)
})
