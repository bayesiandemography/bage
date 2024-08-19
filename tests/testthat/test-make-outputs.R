
## 'center_all' ---------------------------------------------------------------

test_that("'center_all' works - along = TRUE", {
  set.seed(0)
  data <- expand.grid(age = poputils::age_labels(type = "lt", max = 60),
                      time = 2000:2010,
                      sex = c("F", "M"))
  data$popn <- rpois(n = nrow(data), lambda = 100)
  data$deaths <- rpois(n = nrow(data), lambda = 10)
  formula <- deaths ~ age * sex + sex * time
  mod <- mod_pois(formula = formula,
                  data = data,
                  exposure = popn)
  mod <- set_prior(mod, sex:time ~ Lin_AR())
  mod <- set_prior(mod, time ~ RW_Seas(n = 2))
  mod <- set_n_draw(mod, n = 5)
  mod <- fit(mod)
  components <- components(mod, standardize = "none")
  ans <- center_all(components = components,
                    priors = mod$priors,
                    dimnames_terms = mod$dimnames_terms,
                    var_time = mod$var_time,
                    var_age = mod$var_age,
                    var_sexgender = mod$var_sexgender,
                    center_along = TRUE)
  i <- ans$term == "(Intercept)" & ans$component == "effect"
  expect_true(all(abs(as.numeric(mean(ans$.fitted[i]))) > 0.000001))
  i <- ans$term == "time" & ans$component == "effect"
  expect_true(all(abs(as.numeric(mean(ans$.fitted[i]))) < 0.000001))
  i <- ans$term == "time" & ans$component == "trend"
  expect_true(all(abs(as.numeric(mean(ans$.fitted[i]))) < 0.000001))
  i <- ans$term == "time" & ans$component == "seasonal"
  expect_true(all(abs(as.numeric(mean(ans$.fitted[i]))) < 0.000001))
  i <- ans$term == "sex:time" & ans$component == "trend"
  expect_true(all(abs(as.numeric(mean(ans$.fitted[i]))) < 0.000001))
  i <- ans$term == "sex:time" & ans$component == "cyclical"
  expect_true(all(abs(as.numeric(mean(ans$.fitted[i]))) < 0.000001))
})


## 'center_effects' -----------------------------------------------------------

test_that("'center_effects' works - center_along is TRUE", {
  set.seed(0)
  data <- expand.grid(age = poputils::age_labels(type = "lt", max = 60),
                      time = 2000:2010,
                      sex = c("F", "M"))
  data$popn <- rpois(n = nrow(data), lambda = 100)
  data$deaths <- rpois(n = nrow(data), lambda = 10)
  formula <- deaths ~ age * sex + sex * time
  mod <- mod_pois(formula = formula,
                  data = data,
                  exposure = popn)
  mod <- set_prior(mod, sex:time ~ Lin_AR())
  mod <- set_prior(mod, time ~ RW_Seas(n = 2))
  mod <- set_n_draw(mod, n = 5)
  mod <- fit(mod)
  components <- components(mod, standardize = "none")
  ans <- center_effects(components = components,
                        priors = mod$priors,
                        dimnames_terms = mod$dimnames_terms,
                        var_time = mod$var_time,
                        var_age = mod$var_age,
                        center_along = TRUE)
  i <- ans$term == "(Intercept)" & ans$component == "effect"
  expect_true(all(abs(as.numeric(mean(ans$.fitted[i]))) > 0.000001))
  i <- ans$term == "time" & ans$component == "effect"
  expect_true(all(abs(as.numeric(mean(ans$.fitted[i]))) < 0.000001))
  i <- ans$term == "sex:time" & ans$component == "effect"
  expect_true(all(abs(as.numeric(mean(ans$.fitted[i]))) < 0.000001))
})

test_that("'center_effects' works - center_along is FALSE", {
  set.seed(0)
  data <- expand.grid(age = poputils::age_labels(type = "lt", max = 60),
                      time = 2000:2010,
                      sex = c("F", "M"))
  data$popn <- rpois(n = nrow(data), lambda = 100)
  data$deaths <- rpois(n = nrow(data), lambda = 10)
  formula <- deaths ~ age * sex + sex * time
  mod <- mod_pois(formula = formula,
                  data = data,
                  exposure = popn)
  mod <- set_prior(mod, age ~ SVD(HMD))
  mod <- set_prior(mod, sex:time ~ Lin_AR())
  mod <- set_prior(mod, time ~ RW_Seas(n = 2))
  mod <- set_n_draw(mod, n = 5)
  mod <- fit(mod)
  components <- components(mod, standardize = "none")
  ans <- center_effects(components = components,
                        priors = mod$priors,
                        dimnames_terms = mod$dimnames_terms,
                        var_time = mod$var_time,
                        var_age = mod$var_age,
                        center_along = FALSE)
  i <- ans$term == "(Intercept)" & ans$component == "effect"
  expect_true(all(abs(as.numeric(mean(ans$.fitted[i]))) > 0.000001))
  i <- ans$term == "age" & ans$component == "effect"
  expect_true(all(abs(as.numeric(mean(ans$.fitted[i]))) < 0.000001))
  i <- ans$term == "time" & ans$component == "effect"
  expect_true(all(abs(as.numeric(mean(ans$.fitted[i]))) > 0.000001))
  i <- ans$term == "sex:time" & ans$component == "effect"
  expect_true(all(abs(as.numeric(mean(ans$.fitted[i]))) < 0.000001))
  i <- ans$term == "sex:time" & ans$component == "effect" & startsWith(ans$level, "M")
  expect_true(all(abs(as.numeric(mean(ans$.fitted[i]))) > 0.000001))
})


## 'center_svd_spline' --------------------------------------------------------

test_that("'center_svd_spline' works - center_along is TRUE", {
  set.seed(0)
  data <- expand.grid(age = poputils::age_labels(type = "lt", max = 60),
                      time = 2000:2005,
                      sex = c("F", "M"))
  data$popn <- rpois(n = nrow(data), lambda = 100)
  data$deaths <- rpois(n = nrow(data), lambda = 10)
  formula <- deaths ~ age * sex + age * time
  mod <- mod_pois(formula = formula,
                  data = data,
                  exposure = popn)
  mod <- set_prior(mod, age:sex ~ SVD(HMD))
  mod <- set_prior(mod, age ~ Sp(n = 5))
  mod <- set_prior(mod, age:time ~ SVD_RW(HMD))
  mod <- set_n_draw(mod, n = 5)
  mod <- fit(mod)
  components <- components(mod, standardize = "none")
  ans <- center_svd_spline(components = components,
                           priors = mod$priors,
                           dimnames_terms = mod$dimnames_terms,
                           var_time = mod$var_time,
                           var_age = mod$var_age,
                           var_sexgender = mod$var_sexgender,
                           center_along = TRUE)
  expect_true(rvec::draws_mean(mean(ans$.fitted[ans$term == "age:sex" & ans$component == "svd"]))
              < 0.000001)
  expect_true(rvec::draws_mean(mean(ans$.fitted[ans$term == "age" & ans$component == "spline"]))
              < 0.000001)
  expect_true(rvec::draws_mean(mean(ans$.fitted[ans$term == "age:time" & ans$component == "svd"]))
              < 0.000001)
  expect_true(all(abs(as.numeric(mean(ans$.fitted[ans$term == "age:time" & ans$component == "svd" & startsWith(ans$level, "comp1")])))
                  < 0.000001))
})

test_that("'center_svd_spline' works - center_along is FALSE", {
  set.seed(0)
  data <- expand.grid(age = poputils::age_labels(type = "lt", max = 60),
                      time = 2000:2005,
                      sex = c("F", "M"))
  data$popn <- rpois(n = nrow(data), lambda = 100)
  data$deaths <- rpois(n = nrow(data), lambda = 10)
  formula <- deaths ~ age * sex + age * time
  mod <- mod_pois(formula = formula,
                  data = data,
                  exposure = popn)
  mod <- set_prior(mod, age:sex ~ SVD(HMD))
  mod <- set_prior(mod, age ~ Sp(n = 5))
  mod <- set_prior(mod, age:time ~ SVD_RW(HMD))
  mod <- set_n_draw(mod, n = 5)
  mod <- fit(mod)
  components <- components(mod, standardize = "none")
  ans <- center_svd_spline(components = components,
                           priors = mod$priors,
                           dimnames_terms = mod$dimnames_terms,
                           var_time = mod$var_time,
                           var_age = mod$var_age,
                           var_sexgender = mod$var_sexgender,
                           center_along = FALSE)
  expect_true(rvec::draws_mean(mean(ans$.fitted[ans$term == "age:sex" & ans$component == "svd"]))
              < 0.000001)
  expect_true(rvec::draws_mean(mean(ans$.fitted[ans$term == "age" & ans$component == "spline"]))
              < 0.000001)
  expect_true(rvec::draws_mean(mean(ans$.fitted[ans$term == "age:time" & ans$component == "svd"]))
              < 0.000001)
  expect_true(rvec::draws_mean(mean(ans$.fitted[ans$term == "age:time" & ans$component == "svd"]))
              < 0.000001)
  expect_true(all(abs(as.numeric(mean(ans$.fitted[ans$term == "age:time" & ans$component == "svd" & startsWith(ans$level, "comp1")])))
                  > 0.000001))
})


## 'center_trend_cyc_seas_err' ------------------------------------------------

test_that("'center_trend_cyc_seas_err' works - along = TRUE", {
  set.seed(0)
  data <- expand.grid(age = poputils::age_labels(type = "lt", max = 60),
                      time = 2000:2010,
                      sex = c("F", "M"))
  data$popn <- rpois(n = nrow(data), lambda = 100)
  data$deaths <- rpois(n = nrow(data), lambda = 10)
  formula <- deaths ~ age * sex + sex * time
  mod <- mod_pois(formula = formula,
                  data = data,
                  exposure = popn)
  mod <- set_prior(mod, sex:time ~ Lin_AR())
  mod <- set_prior(mod, time ~ RW_Seas(n = 2))
  mod <- set_n_draw(mod, n = 5)
  mod <- fit(mod)
  components <- components(mod, standardize = "none")
  ans <- center_trend_cyc_seas_err(components = components,
                                   priors = mod$priors,
                                   dimnames_terms = mod$dimnames_terms,
                                   var_time = mod$var_time,
                                   var_age = mod$var_age,
                                   center_along = TRUE)
  i <- ans$term == "time" & ans$component == "trend"
  expect_true(all(abs(as.numeric(mean(ans$.fitted[i]))) < 0.000001))
  i <- ans$term == "time" & ans$component == "seasonal"
  expect_true(all(abs(as.numeric(mean(ans$.fitted[i]))) < 0.000001))
  i <- ans$term == "sex:time" & ans$component == "trend"
  expect_true(all(abs(as.numeric(mean(ans$.fitted[i]))) < 0.000001))
  i <- ans$term == "sex:time" & ans$component == "cyclical"
  expect_true(all(abs(as.numeric(mean(ans$.fitted[i]))) < 0.000001))
})

test_that("'center_trend_cyc_seas_err' works - along = FALSE", {
  set.seed(0)
  data <- expand.grid(age = poputils::age_labels(type = "lt", max = 60),
                      time = 2000:2010,
                      sex = c("F", "M"))
  data$popn <- rpois(n = nrow(data), lambda = 100)
  data$deaths <- rpois(n = nrow(data), lambda = 10)
  formula <- deaths ~ age * sex + sex * time
  mod <- mod_pois(formula = formula,
                  data = data,
                  exposure = popn)
  mod <- set_prior(mod, sex:time ~ Lin_AR())
  mod <- set_prior(mod, time ~ RW_Seas(n = 2))
  mod <- set_n_draw(mod, n = 5)
  mod <- fit(mod)
  components <- components(mod, standardize = "none")
  ans <- center_trend_cyc_seas_err(components = components,
                                   priors = mod$priors,
                                   dimnames_terms = mod$dimnames_terms,
                                   var_time = mod$var_time,
                                   var_age = mod$var_age,
                                   center_along = FALSE)
  i <- ans$term == "time" & ans$component == "trend"
  expect_true(all(abs(as.numeric(mean(ans$.fitted[i]))) > 0.000001))
  i <- ans$term == "time" & ans$component == "seasonal"
  expect_true(all(abs(as.numeric(mean(ans$.fitted[i]))) > 0.000001))
  i <- ans$term == "sex:time" & ans$component == "trend"
  expect_true(all(abs(as.numeric(mean(ans$.fitted[i]))) < 0.000001))
  i <- ans$term == "sex:time" & ans$component == "cyclical"
  expect_true(all(abs(as.numeric(mean(ans$.fitted[i]))) < 0.000001))
})

test_that("'center_trend_cyc_seas_err' throws appropriate error when prior does not have along", {
  set.seed(0)
  data <- expand.grid(age = poputils::age_labels(type = "lt", max = 60),
                      time = 2000:2010,
                      sex = c("F", "M"))
  data$popn <- rpois(n = nrow(data), lambda = 100)
  data$deaths <- rpois(n = nrow(data), lambda = 10)
  formula <- deaths ~ age * sex + sex * time
  mod <- mod_pois(formula = formula,
                  data = data,
                  exposure = popn)
  mod <- set_prior(mod, sex:time ~ Lin_AR())
  mod <- set_prior(mod, time ~ RW_Seas(n = 2))
  mod <- set_n_draw(mod, n = 5)
  mod <- fit(mod)
  components <- components(mod, standardize = "none")
  mod$priors$time <- N()
  expect_error(center_trend_cyc_seas_err(components = components,
                                         priors = mod$priors,
                                         dimnames_terms = mod$dimnames_terms,
                                         var_time = mod$var_time,
                                         var_age = mod$var_age,
                                         center_along = FALSE),
               "Internal error: Prior for term \"time\" does not use along.")               
})


## 'center_within_along_by' --------------------------------------------------

test_that("'center_within_along_by' works with numeric vector - center_along = TRUE", {
  x <- matrix(rnorm(12), nr = 4)
  matrix_along_by <- t(matrix(0:11, nrow = 4))
  ans_obtained <- center_within_along_by(as.numeric(x), matrix_along_by, center_along = TRUE)
  ans_expected <- x - rowMeans(x)
  ans_expected <- ans_expected - rep(colMeans(ans_expected), each = 4)
  ans_expected <- as.numeric(ans_expected)
  expect_equal(ans_obtained, ans_expected)
})

test_that("'center_within_along_by' works with numeric vector - center_along = FALSE", {
  x <- matrix(rnorm(12), nr = 4)
  matrix_along_by <- t(matrix(0:11, nrow = 4))
  ans_obtained <- center_within_along_by(as.numeric(x), matrix_along_by, center_along = FALSE)
  ans_expected <- x - rep(colMeans(x), each = 4)
  ans_expected <- as.numeric(ans_expected)
  expect_equal(ans_obtained, ans_expected)
})

test_that("'center_within_along_by' works with rvec - center_along = TRUE", {
  x <- rvec::rvec(matrix(rnorm(120), nr = 12))
  matrix_along_by <- matrix(0:11, nrow = 4)
  ans_obtained <- center_within_along_by(x, matrix_along_by, center_along = TRUE)
  ans_expected <- c(x[1:4] - mean(x[1:4]),
                    x[5:8] - mean(x[5:8]),
                    x[9:12] - mean(x[9:12]))
  ans_expected <- c(ans_expected[c(1, 5, 9)] - mean(ans_expected[c(1, 5, 9)]),
                    ans_expected[c(2, 6, 10)] - mean(ans_expected[c(2, 6, 10)]),
                    ans_expected[c(3, 7, 11)] - mean(ans_expected[c(3, 7, 11)]),
                    ans_expected[c(4, 8, 12)] - mean(ans_expected[c(4, 8, 12)]))[c(1,4,7,10,
                                                                                   2,5,8,11,
                                                                                   3,6,9,12)]
  expect_equal(ans_obtained, ans_expected)
})

test_that("'center_within_along_by' works with rvec - center_along = FALSE", {
  x <- rvec::rvec(matrix(rnorm(120), nr = 12))
  matrix_along_by <- matrix(0:11, nrow = 4)
  ans_obtained <- center_within_along_by(x, matrix_along_by, center_along = FALSE)
  ans_expected <- c(x[c(1, 5, 9)] - mean(x[c(1, 5, 9)]),
                    x[c(2, 6, 10)] - mean(x[c(2, 6, 10)]),
                    x[c(3, 7, 11)] - mean(x[c(3, 7, 11)]),
                    x[c(4, 8, 12)] - mean(x[c(4, 8, 12)]))[c(1,4,7,10,
                                                             2,5,8,11,
                                                             3,6,9,12)]
  expect_equal(ans_obtained, ans_expected)
})


## 'draw_vals_components_fitted' ----------------------------------------------

test_that("'draw_vals_components_fitted' works - standardize = 'terms'", {
  set.seed(0)
  data <- expand.grid(age = poputils::age_labels(type = "lt", max = 60),
                      time = 2000:2005,
                      sex = c("F", "M"))
  data$popn <- rpois(n = nrow(data), lambda = 100)
  data$deaths <- rpois(n = nrow(data), lambda = 10)
  formula <- deaths ~ age * sex + time
  mod <- mod_pois(formula = formula,
                  data = data,
                  exposure = popn)
  mod <- set_n_draw(mod, n = 5)
  mod <- set_prior(mod, age ~ Sp())
  mod <- set_prior(mod, age:sex ~ SVD(HMD))
  mod <- fit(mod)
  set.seed(0)
  ans <- draw_vals_components_fitted(mod, standardize = "terms")
  expect_identical(names(ans), c("term", "component", "level", ".fitted"))
  i <- ans$term == "age:sex" & ans$component == "effect"
  expect_equal(mean(as.numeric(sum(ans$.fitted[i]))), 0)
})

test_that("'draw_vals_components_fitted' works - standardize = 'anova'", {
  set.seed(0)
  data <- expand.grid(age = poputils::age_labels(type = "lt", max = 60),
                      time = 2000:2005,
                      sex = c("F", "M"))
  data$popn <- rpois(n = nrow(data), lambda = 100)
  data$deaths <- rpois(n = nrow(data), lambda = 10)
  formula <- deaths ~ age * sex + time
  mod <- mod_pois(formula = formula,
                  data = data,
                  exposure = popn)
  mod <- set_n_draw(mod, n = 5)
  mod <- set_prior(mod, age ~ Sp())
  mod <- set_prior(mod, age:sex ~ SVD(HMD))
  mod <- fit(mod)
  set.seed(0)
  ans <- draw_vals_components_fitted(mod, standardize = "anova")
  expect_identical(names(ans), c("term", "component", "level", ".fitted"))
  i <- ans$term == "age:sex" & ans$component == "effect"
  expect_equal(mean(as.numeric(sum(ans$.fitted[i]))), 0)
})

test_that("'draw_vals_components_fitted' works - standardize = 'none'", {
  set.seed(0)
  data <- expand.grid(age = poputils::age_labels(type = "lt", max = 60),
                      time = 2000:2005,
                      sex = c("F", "M"))
  data$popn <- rpois(n = nrow(data), lambda = 100)
  data$deaths <- rpois(n = nrow(data), lambda = 10)
  formula <- deaths ~ age * sex + time
  mod <- mod_pois(formula = formula,
                  data = data,
                  exposure = popn)
  mod <- set_n_draw(mod, n = 5)
  mod <- set_prior(mod, age ~ Sp())
  mod <- set_prior(mod, age:sex ~ SVD(HMD))
  mod <- fit(mod)
  set.seed(0)
  ans <- draw_vals_components_fitted(mod, standardize = "none")
  expect_identical(names(ans), c("term", "component", "level", ".fitted"))
  i <- ans$term == "age:sex" & ans$component == "effect"
  expect_true(mean(abs(as.numeric(sum(ans$.fitted[i])))) > 0)
})

test_that("'draw_vals_components_fitted' gives correct error with invalid standarize", {
  set.seed(0)
  data <- expand.grid(age = poputils::age_labels(type = "lt", max = 60),
                      time = 2000:2005,
                      sex = c("F", "M"))
  data$popn <- rpois(n = nrow(data), lambda = 100)
  data$deaths <- rpois(n = nrow(data), lambda = 10)
  formula <- deaths ~ age * sex + time
  mod <- mod_pois(formula = formula,
                  data = data,
                  exposure = popn)
  mod <- fit(mod)
  expect_error(draw_vals_components_fitted(mod, standardize = "wrong"),
               "Internal error: Invalid value for `standardize`.")
})


## 'get_disp' -----------------------------------------------------------------

test_that("'get_disp' works - unfitted", {
  set.seed(0)
  data <- expand.grid(age = 0:9, time = 2000:2005, sex = c("F", "M"))
  data$popn <- rpois(n = nrow(data), lambda = 100)
  data$deaths <- rpois(n = nrow(data), lambda = 10)
  formula <- deaths ~ age * sex + sex * time
  mod <- mod_pois(formula = formula,
                  data = data,
                  exposure = popn)
  mod <- set_n_draw(mod, n_draw = 5)
  set.seed(1)
  ans_obtained <- get_disp(mod)
  set.seed(1)
  ans_expected <- draw_vals_disp(mod, n_sim = 5)
  expect_identical(ans_obtained, ans_expected)
})

test_that("'get_disp' works - fitted", {
  set.seed(0)
  data <- expand.grid(age = 0:9, time = 2000:2005, sex = c("F", "M"))
  data$popn <- rpois(n = nrow(data), lambda = 100)
  data$deaths <- rpois(n = nrow(data), lambda = 10)
  formula <- deaths ~ age * sex + sex * time
  mod <- mod_pois(formula = formula,
                  data = data,
                  exposure = popn)
  mod <- set_n_draw(mod, n_draw = 5)
  mod <- fit(mod)
  set.seed(1)
  ans_obtained <- get_disp(mod)
  set.seed(1)
  ans_expected <- rvec::rvec(matrix(mod$draws_disp, nr = 1))
  expect_identical(ans_obtained, ans_expected)
})  


## 'insert_after' -------------------------------------------------------

test_that("'insert_after' works with data frames", {
  df <- data.frame(x = 1:3, y = 3:1)
  x <- 11:13
  nm_x = "new"
  ans_obtained <- insert_after(df = df,
                               nm_after = "x",
                               x = x,
                               nm_x = nm_x)
  ans_expected <- data.frame(x = 1:3, new = 11:13, y = 3:1)
  expect_identical(ans_obtained, ans_expected)
  ans_obtained <- insert_after(df = df,
                               nm_after = "y",
                               x = x,
                               nm_x = nm_x)
  ans_expected <- data.frame(x = 1:3, y = 3:1, new = 11:13)
  expect_identical(ans_obtained, ans_expected)
})

test_that("'insert_after' works with tibbles", {
  df <- tibble::tibble(x = 1:3, y = 3:1)
  x <- 11:13
  nm_x = "new"
  ans_obtained <- insert_after(df = df,
                               nm_after = "x",
                               x = x,
                               nm_x = nm_x)
  ans_expected <- tibble(x = 1:3, new = 11:13, y = 3:1)
  expect_identical(ans_obtained, ans_expected)
  ans_obtained <- insert_after(df = df,
                               nm_after = "y",
                               x = x,
                               nm_x = nm_x)
  ans_expected <- tibble(x = 1:3, y = 3:1, new = 11:13)
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


## 'is_same_class' ------------------------------------------------------------

test_that("'is_same_class' returns TRUE when classes same", {
    expect_true(is_same_class(AR1(), AR1()))
    expect_true(is_same_class(1L, 2L))
})

test_that("'is_same_class' returns FALSE when classes different", {
    expect_false(is_same_class(AR1(), N()))
    expect_false(is_same_class(1L, FALSE))
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


## 'make_combined_offset_effectfree_effect' -----------------------------------------

test_that("'make_combined_offset_effectfree_effect' works with valid inputs", {
    set.seed(0)
    data <- expand.grid(age = 0:9, time = 2000:2005, sex = c("F", "M"))
    data$popn <- rpois(n = nrow(data), lambda = 100)
    data$deaths <- rpois(n = nrow(data), lambda = 10)
    formula <- deaths ~ age * sex + age * time
    mod <- mod_pois(formula = formula,
                    data = data,
                    exposure = popn)
    ans_obtained <- make_combined_offset_effectfree_effect(mod)
    expect_identical(length(ans_obtained), length(mod$terms_effect))
    expect_true(all(ans_obtained == 0))
})


## 'make_comp_components' -----------------------------------------------------

test_that("'make_comp_components' works - no hyperrand", {
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
    expect_identical(length(draws), length(ans))
    expect_setequal(ans, c("effect", "hyper", "disp"))
})

test_that("'make_comp_components' works - has hyperrand", {
    set.seed(0)
    data <- expand.grid(age = 0:9, time = 2000:2005, sex = c("F", "M"))
    data$popn <- rpois(n = nrow(data), lambda = 100)
    data$deaths <- rpois(n = nrow(data), lambda = 10)
    formula <- deaths ~ age + sex * time
    mod <- mod_pois(formula = formula,
                    data = data,
                    exposure = popn) |>
                    set_prior(sex:time ~ Lin())
    mod <- set_n_draw(mod, n = 1)
    mod <- fit(mod)
    draws <- make_draws_components(mod)
    ans <- make_comp_components(mod)
    expect_identical(length(draws), length(ans))
    expect_setequal(ans, c("effect", "hyper", "hyperrand", "disp"))
})

test_that("'make_comp_components' works - has svd", {
  set.seed(0)
  data <- expand.grid(age = poputils::age_labels(type = "lt", max = 60),
                      time = 2000:2005,
                      sex = c("F", "M"))
  data$popn <- rpois(n = nrow(data), lambda = 100)
  data$deaths <- rpois(n = nrow(data), lambda = 10)
  formula <- deaths ~ age * sex + time
  mod <- mod_pois(formula = formula,
                  data = data,
                  exposure = popn)
  mod <- set_n_draw(mod, n = 5)
  mod <- set_prior(mod, age ~ SVD(HMD))
  mod <- set_prior(mod, age:sex ~ Sp())
  set.seed(0)
  mod <- fit(mod)
  term <- make_term_components(mod)
  ans <- make_comp_components(mod)
  expect_identical(length(term), length(ans))
  expect_setequal(ans, c("effect", "hyper", "svd", "spline", "disp"))
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


## 'make_draws_components' -----------------------------------------------

test_that("'make_draws_components' works - no svd, spline", {
  set.seed(0)
  data <- expand.grid(age = poputils::age_labels(type = "lt", max = 60),
                      time = 2000:2005,
                      sex = c("F", "M"))
  data$popn <- rpois(n = nrow(data), lambda = 100)
  data$deaths <- rpois(n = nrow(data), lambda = 10)
  formula <- deaths ~ age * sex + time
  mod <- mod_pois(formula = formula,
                  data = data,
                  exposure = popn)
  mod <- set_n_draw(mod, n = 5)
  mod <- fit(mod)
  set.seed(0)
  ans_obtained <- make_draws_components(mod)
  expect_true(rvec::is_rvec(ans_obtained))
  expect_identical(length(ans_obtained), length(unlist(mod$est)))
})

test_that("'make_draws_components' works - has spline", {
  set.seed(0)
  data <- expand.grid(age = poputils::age_labels(type = "lt", max = 60),
                      time = 2000:2005,
                      sex = c("F", "M"))
  data$popn <- rpois(n = nrow(data), lambda = 100)
  data$deaths <- rpois(n = nrow(data), lambda = 10)
  formula <- deaths ~ age * sex + time
  mod <- mod_pois(formula = formula,
                  data = data,
                  exposure = popn)
  mod <- set_n_draw(mod, n = 5)
  mod <- set_prior(mod, age ~ Sp())
  mod <- fit(mod)
  set.seed(0)
  ans_obtained <- make_draws_components(mod)
  expect_true(rvec::is_rvec(ans_obtained))
  expect_identical(length(ans_obtained), length(unlist(mod$est)) +
                                           length(unique(data$age)))
})

test_that("'make_draws_components' works - has svd", {
  set.seed(0)
  data <- expand.grid(age = poputils::age_labels(type = "lt", max = 60),
                      time = 2000:2005,
                      sex = c("F", "M"))
  data$popn <- rpois(n = nrow(data), lambda = 100)
  data$deaths <- rpois(n = nrow(data), lambda = 10)
  formula <- deaths ~ age * sex + time
  mod <- mod_pois(formula = formula,
                  data = data,
                  exposure = popn)
  mod <- set_n_draw(mod, n = 5)
  mod <- set_prior(mod, age ~ SVD(HMD))
  mod <- fit(mod)
  set.seed(0)
  ans_obtained <- make_draws_components(mod)
  expect_true(rvec::is_rvec(ans_obtained))
  expect_identical(length(ans_obtained), length(unlist(mod$est)) +
                                           length(unique(data$age)))
})


## 'make_draws_disp' ----------------------------------------------------

test_that("'make_draws_disp' works", {
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
    draws <- make_draws_post(mod)
    ans_obtained <- make_draws_disp(mod, draws_post = draws)
    ans_expected <- exp(draws[22,])
    expect_identical(unname(ans_obtained), ans_expected)
})


## 'make_draws_effectfree' ----------------------------------------------------

test_that("'make_draws_effectfree' works", {
  set.seed(0)
  data <- expand.grid(age = 0:9, time = 2000:2005, sex = c("F", "M"))
  data$popn <- rpois(n = nrow(data), lambda = 100)
  data$deaths <- rpois(n = nrow(data), lambda = 10)
  formula <- deaths ~ age * sex + time
  mod <- mod_pois(formula = formula,
                  data = data,
                  exposure = popn)
  mod <- set_prior(mod, age ~ Sp())
  mod <- set_n_draw(mod, n = 5)
  mod <- fit(mod)
  est <- mod$est
  draws <- make_draws_post(mod)
  ans_obtained <- make_draws_effectfree(mod = mod, draws_post = draws)
  ans_expected <- draws[seq_along(est$effectfree), ]
  expect_equal(ans_obtained, ans_expected)
})


## 'make_draws_hyper' ----------------------------------------------------

test_that("'make_draws_hyper' works", {
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
    draws <- make_draws_post(mod)
    ans_obtained <- make_draws_hyper(mod, draws_post = draws)
    ans_expected <- exp(draws[20:21, ])
    expect_identical(unname(ans_obtained), ans_expected)
})


## 'make_draws_hyperrand' -----------------------------------------------------

test_that("'make_draws_hyperrand' works - has hyperrand", {
    set.seed(0)
    data <- expand.grid(age = 0:9, time = 2000:2005, sex = c("F", "M"))
    data$popn <- rpois(n = nrow(data), lambda = 100)
    data$deaths <- rpois(n = nrow(data), lambda = 10)
    formula <- deaths ~ age:time + sex
    mod <- mod_pois(formula = formula,
                    data = data,
                    exposure = popn)
    mod <- set_n_draw(mod, 5)
    mod <- set_prior(mod, age:time ~ Lin())
    mod <- fit(mod)
    draws <- make_draws_post(mod)
    ans_obtained <- make_draws_hyperrand(mod, draws_post = draws)
    ans_expected <- draws[65:84, ]
    expect_identical(unname(ans_obtained), ans_expected)
})

test_that("'make_draws_hyperrand' works - no hyperrand", {
    set.seed(0)
    data <- expand.grid(age = 0:9, time = 2000:2005, sex = c("F", "M"))
    data$popn <- rpois(n = nrow(data), lambda = 100)
    data$deaths <- rpois(n = nrow(data), lambda = 10)
    formula <- deaths ~ age:time + sex
    mod <- mod_pois(formula = formula,
                    data = data,
                    exposure = popn)
    mod <- set_n_draw(mod, 5)
    mod <- fit(mod)
    draws <- make_draws_post(mod)
    ans_obtained <- make_draws_hyperrand(mod, draws_post = draws)
    ans_expected <- matrix(NA_real_, nrow = 0, ncol = 5)
    expect_identical(unname(ans_obtained), ans_expected)
})


## 'make_draws_post' ------------------------------------------------------

test_that("'make_draws_post' works with valid inputs - has R_prec", {
    set.seed(0)
    data <- expand.grid(age = 0:9, time = 2000:2005, sex = c("F", "M"))
    data$popn <- rpois(n = nrow(data), lambda = 100)
    data$deaths <- rpois(n = nrow(data), lambda = 10)
    formula <- deaths ~ age + sex
    mod <- mod_pois(formula = formula,
                    data = data,
                    exposure = popn)
    mod <- set_prior(mod, sex ~ Known(c(0.1, -0.1)))
    mod <- fit(mod)
    mod <- set_n_draw(mod, n = 100000)
    ans <- make_draws_post(mod)
    mean <- unlist(mod$est, use.names = FALSE)[!mod$is_fixed]
    prec <- crossprod(mod$R_prec)
    expect_equal(rowMeans(ans), unlist(mod$est, use.names = FALSE), tolerance = 0.02)
    expect_equal(solve(cov(t(ans[!mod$is_fixed,]))), unname(prec), tolerance = 0.02)
})

test_that("'make_draws_post' works with valid inputs - no R_prec", {
  set.seed(0)
  data <- expand.grid(age = 0:9, time = 2000:2005, sex = c("F", "M"))
  data$popn <- rpois(n = nrow(data), lambda = 100)
  data$deaths <- rpois(n = nrow(data), lambda = 10)
  formula <- deaths ~ age + sex
  mod <- mod_pois(formula = formula,
                  data = data,
                  exposure = popn)
  mod <- unfit(mod)
  ## data
  nm_distn <- nm_distn(mod)
  outcome <- mod$outcome
  offset <- mod$offset
  terms_effect <- mod$terms_effect
  i_lik <- make_i_lik_mod(mod)
  is_in_lik <- make_is_in_lik(mod)
  terms_effectfree <- make_terms_effectfree(mod)
  uses_matrix_effectfree_effect <- make_uses_matrix_effectfree_effect(mod)
  matrices_effectfree_effect <- make_matrices_effectfree_effect(mod)
  uses_offset_effectfree_effect <- make_uses_offset_effectfree_effect(mod)
  offsets_effectfree_effect <- make_offsets_effectfree_effect(mod)
  matrices_effect_outcome <- mod$matrices_effect_outcome
  i_prior <- make_i_prior(mod)
  uses_hyper <- make_uses_hyper(mod)
  terms_hyper <- make_terms_hyper(mod)
  uses_hyperrand <- make_uses_hyperrand(mod)
  terms_hyperrand <- make_terms_hyperrand(mod)
  const <- make_const(mod)
  terms_const <- make_terms_const(mod)
  matrices_along_by_effectfree <- make_matrices_along_by_effectfree(mod)
  mean_disp <- mod$mean_disp
  has_disp <- mean_disp > 0
  data <- list(nm_distn = nm_distn,
               outcome = outcome,
               offset = offset,
               i_lik = i_lik,
               is_in_lik = is_in_lik,
               terms_effect = terms_effect,
               terms_effectfree = terms_effectfree,
               uses_matrix_effectfree_effect = uses_matrix_effectfree_effect,
               matrices_effectfree_effect = matrices_effectfree_effect,
               uses_offset_effectfree_effect = uses_offset_effectfree_effect,
               offsets_effectfree_effect = offsets_effectfree_effect,
               matrices_effect_outcome = matrices_effect_outcome,
               i_prior = i_prior,
               uses_hyper = uses_hyper,
               terms_hyper = terms_hyper,
               uses_hyperrand = uses_hyperrand,
               terms_hyperrand = terms_hyperrand,
               consts = const, ## 'const' is reserved word in C
               terms_consts = terms_const,
               matrices_along_by_effectfree = matrices_along_by_effectfree,
               mean_disp = mean_disp)
  ## parameters
  effectfree <- make_effectfree(mod)
  hyper <- make_hyper(mod)
  hyperrand <- make_hyperrand(mod)
  log_disp <- 0
  parameters <- list(effectfree = effectfree,   
                     hyper = hyper,
                     hyperrand = hyperrand,
                     log_disp = log_disp)
  ## MakeADFun
  map <- make_map(mod)
  random <- make_random(mod)
  has_random_effects <- !is.null(random)
  f <- TMB::MakeADFun(data = data,
                      parameters = parameters,
                      map = map,
                      DLL = "bage",
                      random = random,
                      silent = TRUE)
  ## optimise
  stats::nlminb(start = f$par,
                objective = f$fn,
                gradient = f$gr,
                silent = TRUE)
  ## extract results
  sdreport <- TMB::sdreport(f,
                            bias.correct = TRUE,
                            getJointPrecision = TRUE)
  est <- as.list(sdreport, what = "Est")
  attr(est, "what") <- NULL
  mod$is_fixed <- make_is_fixed(est = est, map = map)
  mod$est <- est
  prec <- sdreport$jointPrecision
  mod$R_prec <- NULL
  mod$scaled_eigen <- make_scaled_eigen(prec)
  mod <- set_n_draw(mod, n = 100000)
  ans <- make_draws_post(mod)
  mean <- unlist(mod$est, use.names = FALSE)[!mod$is_fixed]
  expect_equal(rowMeans(ans), unlist(mod$est, use.names = FALSE), tolerance = 0.02)
  expect_equal(solve(cov(t(ans[!mod$is_fixed,]))),
               unname(Matrix::as.matrix(prec)),
               tolerance = 0.02)
})


## 'make_levels_spline' ----------------------------------------------------------

test_that("'make_levels_spline' works - unlist is FALSE", {
  set.seed(0)
  data <- expand.grid(age = poputils::age_labels(type = "lt", max = 60),
                      time = 2000:2005,
                      sex = c("F", "M"))
  data$popn <- rpois(n = nrow(data), lambda = 100)
  data$deaths <- rpois(n = nrow(data), lambda = 10)
  formula <- deaths ~ sex * age + age*time
  mod <- mod_pois(formula = formula,
                  data = data,
                  exposure = popn)
  mod <- set_n_draw(mod, n = 5)
  mod <- set_prior(mod, age ~ Sp(n_comp = 5))
  mod <- set_prior(mod, age:sex ~ Sp(n_comp = 5))
  mod <- set_prior(mod, age:time ~ Sp(n_comp = 5, along = "age"))
  set.seed(0)
  ans_obtained <- make_levels_spline(mod, unlist = FALSE)
  ans_expected <- list("(Intercept)" = NULL,
                       sex = NULL,
                       age = paste0("comp", 1:5),
                       time = NULL,
                       "sex:age" = paste(paste0("comp", 1:5), rep(c("F", "M"), each = 5), sep = "."),
                       "age:time" = paste0("comp", 1:5, ".", rep(2000:2005, each = 5)))
  expect_identical(ans_obtained, ans_expected)
})

test_that("'make_levels_spline' works - unlist is TRUE", {
  set.seed(0)
  data <- expand.grid(age = poputils::age_labels(type = "lt", max = 60),
                      time = 2000:2005,
                      sex = c("F", "M"))
  data$popn <- rpois(n = nrow(data), lambda = 100)
  data$deaths <- rpois(n = nrow(data), lambda = 10)
  formula <- deaths ~ sex * age + age*time
  mod <- mod_pois(formula = formula,
                  data = data,
                  exposure = popn)
  mod <- set_n_draw(mod, n = 5)
  mod <- set_prior(mod, age ~ Sp(n_comp = 5))
  mod <- set_prior(mod, age:sex ~ Sp(n_comp = 5))
  mod <- set_prior(mod, age:time ~ Sp(n_comp = 5, along = "age"))
  set.seed(0)
  ans_obtained <- make_levels_spline(mod, unlist = TRUE)
  ans_expected <- c(paste0("comp", 1:5),
                    paste(paste0("comp", 1:5), rep(c("F", "M"), each = 5), sep = "."),
                    paste0("comp", 1:5, ".", rep(2000:2005, each = 5)))
  expect_identical(ans_obtained, ans_expected)
})


## 'make_levels_spline_term' --------------------------------------------------

test_that("'make_levels_spline_term' works", {
  prior <- Sp(n = 5)
  dimnames_term <- list(reg = 1:2,
                        age = 1:20)
  ans_obtained <- make_levels_spline_term(prior = prior,
                                          dimnames_term = dimnames_term,
                                          var_time = "time",
                                          var_age = "age")
  ans_expected <- paste(paste0("comp", 1:5), rep(1:2, each = 5), sep = ".")
  expect_identical(ans_obtained, ans_expected)
})


## 'make_levels_svd' ----------------------------------------------------------

test_that("'make_levels_svd' works - unlist is FALSE", {
  set.seed(0)
  data <- expand.grid(age = poputils::age_labels(type = "lt", max = 60),
                      time = 2000:2005,
                      sex = c("F", "M"))
  data$popn <- rpois(n = nrow(data), lambda = 100)
  data$deaths <- rpois(n = nrow(data), lambda = 10)
  formula <- deaths ~ sex * age + age*time
  mod <- mod_pois(formula = formula,
                  data = data,
                  exposure = popn)
  mod <- set_n_draw(mod, n = 5)
  mod <- set_prior(mod, age ~ SVD(HMD))
  mod <- set_prior(mod, age:sex ~ SVD(HMD))
  mod <- set_prior(mod, age:time ~ SVD_RW(HMD))
  set.seed(0)
  ans_obtained <- make_levels_svd(mod, unlist = FALSE)
  ans_expected <- list("(Intercept)" = NULL,
                       sex = NULL,
                       age = paste0("comp", 1:3),
                       time = NULL,
                       "sex:age" = paste0(rep(c("F", "M"), each = 3), ".comp", 1:3),
                       "age:time" = paste0("comp", 1:3, ".", rep(2000:2005, each = 3)))
  expect_identical(ans_obtained, ans_expected)
})

test_that("'make_levels_svd' works - unlist is TRUE", {
  set.seed(0)
  data <- expand.grid(age = poputils::age_labels(type = "lt", max = 60),
                      time = 2000:2005,
                      sex = c("F", "M"))
  data$popn <- rpois(n = nrow(data), lambda = 100)
  data$deaths <- rpois(n = nrow(data), lambda = 10)
  formula <- deaths ~ sex * age + age*time
  mod <- mod_pois(formula = formula,
                  data = data,
                  exposure = popn)
  mod <- set_n_draw(mod, n = 5)
  mod <- set_prior(mod, age ~ SVD(HMD, n_comp = 5))
  mod <- set_prior(mod, age:sex ~ SVD(HMD, n_comp = 5))
  mod <- set_prior(mod, age:time ~ SVD_RW(HMD, n_comp = 5))
  set.seed(0)
  ans_obtained <- make_levels_svd(mod, unlist = TRUE)
  ans_expected <- c(paste0("comp", 1:5),
                    paste0(rep(c("F", "M"), each = 5), ".comp", 1:5),
                    paste0("comp", 1:5, ".", rep(2000:2005, each = 5)))
  expect_identical(ans_obtained, ans_expected)
})


## 'make_levels_svd_term' ----------------------------------------------------------

test_that("'make_levels_svd_term' works - total, main effect", {
  prior <- SVD(HMD)
  dimnames_term <- list(age = c(0:59, "60+"))
  var_age <- "age"
  var_sexgender <- "sex"
  ans_obtained <- make_levels_svd_term(prior = prior,
                                       dimnames_term = dimnames_term,
                                       var_age = var_age,
                                       var_sexgender = var_sexgender)
  ans_expected <- paste0("comp", 1:3)
  expect_identical(ans_obtained, ans_expected)
})

test_that("'make_levels_svd_term' works - joint, age:sex", {
  prior <- SVD(HMD, n_comp = 5)
  dimnames_term <- list(age = c(0:59, "60+"), sex = c("M", "F"))
  var_age <- "age"
  var_sexgender <- "sex"
  ans_obtained <- make_levels_svd_term(prior = prior,
                                       dimnames_term = dimnames_term,
                                       var_age = var_age,
                                       var_sexgender = var_sexgender)
  ans_expected <- paste(rep(c("M", "F"), each = 5), paste0("comp", 1:5), sep = ".")
  expect_identical(ans_obtained, ans_expected)
})

test_that("'make_levels_svd_term' works - indep, age:sex:reg", {
  prior <- SVD(HMD, indep = FALSE, n_comp = 5)
  dimnames_term <- list(reg = 1:2, age = c(0:59, "60+"), sex = c("M", "F"))
  var_age <- "age"
  var_sexgender <- "sex"
  ans_obtained <- make_levels_svd_term(prior = prior,
                                       dimnames_term = dimnames_term,
                                       var_age = var_age,
                                       var_sexgender = var_sexgender)
  ans_expected <- paste(paste0("comp", 1:5),
                        rep(1:2, each = 5),
                        sep = ".")
  expect_identical(ans_obtained, ans_expected)
})


## 'make_lin_trend' -----------------------------------------------------------

test_that("'make_lin_trend' works with valid inputs - n_by = 1", {
  intercept <- rvec::rvec(matrix(as.numeric(2:6), nr = 1))
  slope <- rvec::rvec(matrix(as.numeric(1:5), nr = 1))
  matrix_along_by <- matrix(0:9, nr = 10)
  ans_obtained <- make_lin_trend(intercept = intercept,
                                 slope = slope,
                                 matrix_along_by = matrix_along_by)
  ones <- rep(1, times = 10)
  s <- 1:10
  ans_expected <- rvec::rvec(outer(ones, 2:6) + outer(s, 1:5))
  expect_identical(ans_obtained, ans_expected)
})

test_that("'make_lin_trend' works with valid inputs - n_by = 2, transposed", {
  intercept <- rvec::rvec(matrix(rnorm(10), nr = 2))
  slope <- rvec::rvec(matrix(rnorm(10), nr = 2))
  matrix_along_by <- t(matrix(0:9, nr = 2))
  ans_obtained <- make_lin_trend(intercept = intercept,
                                 slope = slope,
                                 matrix_along_by = matrix_along_by)
  s <- 1:5
  ans_expected <- c(intercept[1] + slope[1] * s, intercept[2] + slope[2] * s)
  ans_expected <- ans_expected[c(1, 6, 2, 7, 3, 8, 4, 9, 5, 10)]
  expect_identical(ans_obtained, ans_expected)
})


## 'make_stored_draws' --------------------------------------------------------

test_that("'make_stored_draws' works with valid inputs", {
    set.seed(0)
    data <- expand.grid(age = 0:9, time = 2000:2005, sex = c("F", "M"))
    data$popn <- rpois(n = nrow(data), lambda = 100)
    data$deaths <- rpois(n = nrow(data), lambda = 10)
    formula <- deaths ~ age + sex
    mod <- mod_pois(formula = formula,
                    data = data,
                    exposure = popn)
    mod <- set_prior(mod, sex ~ Known(c(0.1, -0.1)))
    mod <- set_n_draw(mod, n = 10)
    mod <- fit(mod)
    ans <- make_stored_draws(mod)
    expect_identical(ncol(ans$draws_effectfree), 10L)
    expect_identical(ncol(ans$draws_hyper), 10L)
    expect_identical(length(ans$draws_disp), 10L)
})

test_that("'make_stored_draws' throws error if model not fitted", {
    set.seed(0)
    data <- expand.grid(age = 0:9, time = 2000:2005, sex = c("F", "M"))
    data$popn <- rpois(n = nrow(data), lambda = 100)
    data$deaths <- rpois(n = nrow(data), lambda = 10)
    formula <- deaths ~ age + sex
    mod <- mod_pois(formula = formula,
                    data = data,
                    exposure = popn)
    expect_error(make_stored_draws(mod),
                 "Can't make stored draws for an unfitted model.")
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
    mod <- set_n_draw(mod, n = 10)
    mod <- fit(mod)
    components <- components(mod)
    meanpar <- exp(make_linpred_comp(components = components,
                                     data = mod$data,
                                     dimnames_terms = mod$dimnames_terms))
    disp <- components$.fitted[components$component == "disp"]
    set.seed(1)
    ans_obtained <- make_par_disp(mod,
                                  meanpar = meanpar,
                                  disp = disp)
    set.seed(1)
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
  mod <- set_n_draw(mod, n = 10)
  components <- components(mod)
  invlogit <- function(x) 1 / (1 + exp(-x))
  meanpar <- invlogit(make_linpred_comp(components = components,
                                        data = mod$data,
                                        dimnames_terms = mod$dimnames_terms))
  disp <- components$.fitted[components$component == "disp"]
  set.seed(1)
  ans_obtained <- make_par_disp(mod,
                                meanpar = meanpar,
                                disp = disp)
  set.seed(1)
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

test_that("'make_level_components' works - no hyperrand", {
    set.seed(0)
    data <- expand.grid(age = 0:9, time = 2000:2005, sex = c("F", "M"))
    data$popn <- rpois(n = nrow(data), lambda = 100)
    data$deaths <- rpois(n = nrow(data), lambda = 10)
    formula <- deaths ~ age + sex + time
    mod <- mod_pois(formula = formula,
                    data = data,
                    exposure = popn)
    mod <- set_disp(mod, mean = 0)
    mod <- fit(mod)
    comp <- make_comp_components(mod)
    ans <- make_level_components(mod)
    expect_identical(length(ans), length(comp))
})

test_that("'make_level_components' works - has hyperrand", {
    set.seed(0)
    data <- expand.grid(age = 0:9, time = 2000:2005, sex = c("F", "M"))
    data$popn <- rpois(n = nrow(data), lambda = 100)
    data$deaths <- rpois(n = nrow(data), lambda = 10)
    formula <- deaths ~ age + sex * time
    mod <- mod_pois(formula = formula,
                    data = data,
                    exposure = popn)
    mod <- set_disp(mod, mean = 0)
    mod <- set_prior(mod, sex:time ~ Lin())
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
    mod <- set_prior(mod, sex:time ~ Lin())
    mod <- fit(mod)
    ans_obtained <- make_levels_hyperrand(mod)
    ans_expected <- c("intercept.F", "intercept.M", "slope.F", "slope.M")
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


## 'make_linpred_comp' --------------------------------------------------------

test_that("'make_linpred_comp' works with valid inputs", {
  set.seed(0)
  data <- expand.grid(age = 0:9, time = 2000:2005, sex = c("F", "M"))
  data$popn <- rpois(n = nrow(data), lambda = 100)
  data$deaths <- rpois(n = nrow(data), lambda = 10)
  formula <- deaths ~ age + sex
  mod <- mod_pois(formula = formula,
                  data = data,
                  exposure = popn)
  mod <- set_n_draw(mod, n_draw = 10L)
  comp <- components(mod, standardize = "none", quiet = TRUE)
  ans <- make_linpred_comp(components = comp,
                           data = mod$data,
                           dimnames_terms = mod$dimnames_terms)
  expect_identical(length(ans), length(mod$outcome))
})


## 'make_linpred_raw' ---------------------------------------------------------

test_that("'make_linpred_raw' works with valid inputs", {
  set.seed(0)
  data <- expand.grid(age = 0:9, time = 2000:2005, sex = c("F", "M"))
  data$popn <- rpois(n = nrow(data), lambda = 100)
  data$deaths <- rpois(n = nrow(data), lambda = 10)
  formula <- deaths ~ age + sex
  mod <- mod_pois(formula = formula,
                  data = data,
                  exposure = popn)
  mod <- set_n_draw(mod, n_draw = 10L)
  mod <- fit(mod)
  ans_obtained <- make_linpred_raw(mod)
  comp <- components(mod, standardize = "none", quiet = TRUE)
  ans_expected <- make_linpred_comp(components = comp,
                                    data = mod$data,
                                    dimnames_terms = mod$dimnames_terms)
  expect_equal(ans_obtained, ans_expected)
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


## 'make_spline' --------------------------------------------------------------

test_that("'make_spline' works", {
  set.seed(0)
  data <- expand.grid(age = poputils::age_labels(type = "lt", max = 60),
                      time = 2000:2005,
                      sex = c("F", "M"))
  data$popn <- rpois(n = nrow(data), lambda = 100)
  data$deaths <- rpois(n = nrow(data), lambda = 10)
  formula <- deaths ~ age * sex + time
  mod <- mod_pois(formula = formula,
                  data = data,
                  exposure = popn)
  mod <- set_prior(mod, age:sex ~ Sp(n = 5))
  mod <- set_n_draw(mod, n = 5)
  mod <- fit(mod)
  est <- mod$est
  draws_post <- make_draws_post(mod)
  effectfree <- make_draws_effectfree(mod = mod, draws_post = draws_post)
  ans_obtained <- make_spline(mod = mod, effectfree = effectfree)
  ans_expected <- effectfree[24:33,]
  expect_equal(ans_obtained, ans_expected)
})


## 'make_standardized_effect' -------------------------------------------------

test_that("'make_standardized_effect' works - Intercept", {
  linpred <- rvec::rnorm_rvec(n = 10, n_draw = 20)
  indices_linpred <- rep(1L, 10)
  ans_obtained <- make_standardized_effect(linpred = linpred,
                                           indices_linpred = indices_linpred)
  ans_expected <- mean(linpred)
  expect_equal(ans_obtained, ans_expected)
})

test_that("'make_standardized_effect' works - ordinary term", {
  linpred <- rvec::rnorm_rvec(n = 10, n_draw = 20)
  indices_linpred <- rep(1:2, 5)
  ans_obtained <- make_standardized_effect(linpred = linpred,
                                           indices_linpred = indices_linpred)
  ans_expected <- c(mean(linpred[c(1, 3, 5, 7, 9)]),
                    mean(linpred[c(2, 4, 6, 8, 10)]))                         
  expect_equal(ans_obtained, ans_expected)
})


## 'make_svd' -----------------------------------------------------------------

test_that("'make_svd' works", {
  set.seed(0)
  data <- expand.grid(age = poputils::age_labels(type = "lt", max = 60),
                      time = 2000:2005,
                      sex = c("F", "M"))
  data$popn <- rpois(n = nrow(data), lambda = 100)
  data$deaths <- rpois(n = nrow(data), lambda = 10)
  formula <- deaths ~ age * sex + time
  mod <- mod_pois(formula = formula,
                  data = data,
                  exposure = popn)
  mod <- set_prior(mod, age:sex ~ SVD(HMD, n_comp = 5))
  mod <- set_n_draw(mod, n = 5)
  mod <- fit(mod)
  est <- mod$est
  draws_post <- make_draws_post(mod)
  effectfree <- make_draws_effectfree(mod = mod, draws_post = draws_post)
  ans_obtained <- make_svd(mod = mod, effectfree = effectfree)
  ans_expected <- effectfree[24:33,]
  expect_equal(ans_obtained, ans_expected)
})


## 'make_term_components' -----------------------------------------------------

test_that("'make_term_components' works - no disp", {
    set.seed(0)
    data <- expand.grid(age = 0:9, time = 2000:2005, sex = c("F", "M"))
    data$popn <- rpois(n = nrow(data), lambda = 100)
    data$deaths <- rpois(n = nrow(data), lambda = 10)
    formula <- deaths ~ age + sex + time
    mod <- mod_pois(formula = formula,
                    data = data,
                    exposure = popn)
    mod <- set_disp(mod, mean = 0)
    mod <- set_prior(mod, age ~ Sp())
    mod <- set_n_draw(mod, n = 1)       
    mod <- fit(mod)
    comp <- make_comp_components(mod)
    ans <- make_term_components(mod)
    expect_identical(length(ans), length(comp))
})

test_that("'make_term_components' works - has hyperrand", {
    set.seed(0)
    data <- expand.grid(age = 0:9, time = 2000:2005, sex = c("F", "M"))
    data$popn <- rpois(n = nrow(data), lambda = 100)
    data$deaths <- rpois(n = nrow(data), lambda = 10)
    formula <- deaths ~ age + sex * time
    mod <- mod_pois(formula = formula,
                    data = data,
                    exposure = popn)
    mod <- set_prior(mod, sex:time ~ Lin())
    mod <- set_n_draw(mod, n = 1)       
    mod <- fit(mod)
    comp <- make_comp_components(mod)
    ans <- make_term_components(mod)
    expect_identical(length(ans), length(comp))
})

test_that("'make_term_components' works - has svd", {
  set.seed(0)
  data <- expand.grid(age = poputils::age_labels(type = "lt", max = 60),
                      time = 2000:2005,
                      sex = c("F", "M"))
  data$popn <- rpois(n = nrow(data), lambda = 100)
  data$deaths <- rpois(n = nrow(data), lambda = 10)
  formula <- deaths ~ age * sex + time
  mod <- mod_pois(formula = formula,
                  data = data,
                  exposure = popn)
  mod <- set_n_draw(mod, n = 1)       
  mod <- fit(mod)
  comp <- make_comp_components(mod)
  ans <- make_term_components(mod)
  expect_identical(length(ans), length(comp))
})


## 'make_term_spline' ------------------------------------------------------------

test_that("'make_term_spline' works - no spline", {
  set.seed(0)
  data <- expand.grid(age = poputils::age_labels(type = "lt", max = 60),
                      time = 2000:2005,
                      sex = c("F", "M"))
  data$popn <- rpois(n = nrow(data), lambda = 100)
  data$deaths <- rpois(n = nrow(data), lambda = 10)
  formula <- deaths ~ age * sex + time
  mod <- mod_pois(formula = formula,
                  data = data,
                  exposure = popn)
  mod <- set_n_draw(mod, n = 5)
  ans_obtained <- make_term_spline(mod)
  ans_expected <- factor()
  expect_identical(ans_obtained, ans_expected)
})

test_that("'make_term_spline' works - has spline", {
  set.seed(0)
  data <- expand.grid(age = poputils::age_labels(type = "lt", max = 60),
                      time = 2000:2005,
                      sex = c("F", "M"))
  data$popn <- rpois(n = nrow(data), lambda = 100)
  data$deaths <- rpois(n = nrow(data), lambda = 10)
  formula <- deaths ~ age * sex + time
  mod <- mod_pois(formula = formula,
                  data = data,
                  exposure = popn)
  mod <- set_n_draw(mod, n = 5)
  mod <- set_prior(mod, age ~ Sp(n_comp = 5))
  ans_obtained <- make_term_spline(mod)
  ans_expected <- factor(rep("age", times = 5))
  expect_identical(ans_obtained, ans_expected)
})


## 'make_term_svd' ------------------------------------------------------------

test_that("'make_term_svd' works - no svd", {
  set.seed(0)
  data <- expand.grid(age = poputils::age_labels(type = "lt", max = 60),
                      time = 2000:2005,
                      sex = c("F", "M"))
  data$popn <- rpois(n = nrow(data), lambda = 100)
  data$deaths <- rpois(n = nrow(data), lambda = 10)
  formula <- deaths ~ age * sex + time
  mod <- mod_pois(formula = formula,
                  data = data,
                  exposure = popn)
  mod <- set_n_draw(mod, n = 3)
  ans_obtained <- make_term_svd(mod)
  ans_expected <- factor()
  expect_identical(ans_obtained, ans_expected)
})

test_that("'make_term_svd' works - has svd", {
  set.seed(0)
  data <- expand.grid(age = poputils::age_labels(type = "lt", max = 60),
                      time = 2000:2005,
                      sex = c("F", "M"))
  data$popn <- rpois(n = nrow(data), lambda = 100)
  data$deaths <- rpois(n = nrow(data), lambda = 10)
  formula <- deaths ~ age * sex + time
  mod <- mod_pois(formula = formula,
                  data = data,
                  exposure = popn)
  mod <- set_n_draw(mod, n = 5)
  mod <- set_prior(mod, age ~ SVD(HMD))
  ans_obtained <- make_term_svd(mod)
  ans_expected <- factor(rep("age", times = 3))
  expect_identical(ans_obtained, ans_expected)
})


## 'paste_dot' ----------------------------------------------------------------

test_that("'paste_dot' works with valid inputs", {
  expect_identical(paste_dot(1:3, 3:1), c("1.3", "2.2", "3.1"))
})


## 'rescale_lin_intercept' ----------------------------------------------------

test_that("'rescale_lin_intercept' works with valid inputs", {
  set.seed(0)
  slope <- rvec::rnorm_rvec(n = 4, n_draw = 10)
  effect <- rvec::rnorm_rvec(n = 20, n_draw = 10)
  matrix_along_by <- matrix(0:19, nrow = 5)
  ans_obtained <- rescale_lin_intercept(slope = slope,
                                        effect = effect,
                                        matrix_along_by = matrix_along_by)
  ans_expected <- slope
  for (i in 1:4)
    ans_expected[i] <- mean(effect[1:5 + (i - 1) * 5]) - mean(slope[i] * (1:5))
  expect_equal(ans_obtained, ans_expected)
})


## 'make_transforms_hyper' ----------------------------------------------------

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
    ans_expected <- list(exp, exp)
    expect_identical(unname(ans_obtained), ans_expected,
                     ignore_function_env = TRUE)
})


## 'infer_trend_cyc_seas_err' -------------------------------------------------------

test_that("'infer_trend_cyc_seas_err' works", {
  set.seed(0)
  data <- expand.grid(age = 0:4, time = 2000:2005, sex = c("F", "M"))
  data$popn <- rpois(n = nrow(data), lambda = 100)
  data$deaths <- rpois(n = nrow(data), lambda = 10)
  formula <- deaths ~ sex * time + age
  mod <- mod_pois(formula = formula,
                  data = data,
                  exposure = popn) |>
                  set_prior(time ~ RW2()) |>
                  set_prior(sex:time ~ Lin()) |>
                  fit(mod)
  mod <- set_n_draw(mod, 5)
  comp <- make_comp_components(mod)
  term <- make_term_components(mod)
  level <- make_level_components(mod)
  draws <- make_draws_components(mod)
  draws <- as.matrix(draws)
  .fitted <- rvec::rvec_dbl(draws)
  components <- tibble::tibble(component = comp,
                               term = term,
                               level = level,
                               .fitted = .fitted)
  ans_obtained <- infer_trend_cyc_seas_err(components = components,
                                           priors = mod$priors,
                                           dimnames_terms = mod$dimnames_terms,
                                           var_time = mod$var_time,
                                           var_age = mod$var_age)
  expect_setequal(ans_obtained$component, c("effect", "hyper", "disp"))
})


## 'infer_trend_seas_err_forecast' --------------------------------------------

test_that("'infer_trend_cyc_seas_err_forecast' works", {
  set.seed(0)
  data <- expand.grid(age = 0:4, time = 2000:2005, sex = c("F", "M"))
  data$popn <- rpois(n = nrow(data), lambda = 100)
  data$deaths <- rpois(n = nrow(data), lambda = 10)
  formula <- deaths ~ sex * time + age
  mod <- mod_pois(formula = formula,
                  data = data,
                  exposure = popn) |>
                  set_prior(time ~ RW2()) |>
                  set_prior(sex:time ~ Lin()) |>
                  fit(mod)
  mod <- set_n_draw(mod, 5)
  mod <- fit(mod)
  comp_est <- components(mod)
  comp_forecast <- forecast(mod, labels = 2006:2007, standardize = "none", output = "components")
  dimnames_terms_forecast <- make_dimnames_terms_forecast(dimnames_terms = mod$dimnames_terms,
                                                          var_time = mod$var_time,
                                                          labels_forecast = 2006:2007,
                                                          time_only = TRUE)
  ans_obtained <- infer_trend_cyc_seas_err_forecast(components = comp_forecast,
                                                    priors = mod$priors,
                                                    dimnames_terms = dimnames_terms_forecast,
                                                    var_time = mod$var_time,
                                                    var_age = mod$var_age)
  expect_equal(ans_obtained[1:3], comp_forecast[1:3])
})


## 'infer_trend_cyc_seas_err_seasfix' -----------------------------------------------

test_that("'infer_trend_cyc_seas_err_seasfix' works", {
  set.seed(0)
  data <- expand.grid(age = 0:4, time = 2000:2005, sex = c("F", "M"))
  data$popn <- rpois(n = nrow(data), lambda = 100)
  data$deaths <- rpois(n = nrow(data), lambda = 10)
  formula <- deaths ~ sex * time + age
  mod <- mod_pois(formula = formula,
                  data = data,
                  exposure = popn) |>
                  set_prior(sex:time ~ RW_Seas(n = 3, s_seas = 0)) |>
                  set_n_draw(n = 10) |>
                  fit(mod)
  term <- make_term_components(mod)
  comp <- make_comp_components(mod)
  level <- make_level_components(mod)
  draws <- make_draws_components(mod)
  draws <- as.matrix(draws)
  .fitted <- rvec::rvec_dbl(draws)
  components <- tibble::tibble(term = term,
                               component = comp,
                               level = level,
                               .fitted = .fitted)
  ans_obtained <- infer_trend_cyc_seas_err_seasfix(prior = mod$priors[["sex:time"]],
                                                   dimnames_term = mod$dimnames_terms[["sex:time"]],
                                                   var_time = mod$var_time,
                                                   var_age = mod$var_age,
                                                   components = components)
  ans_expected <- components
  seas <- ans_expected$.fitted[ans_expected$component == "hyperrand" & ans_expected$term == "sex:time"]
  seas <- seas[c(1,4,2,5,3,6,1,4,2,5,3,6)]
  level <- ans_expected$level[ans_expected$component == "effect" & ans_expected$term == "sex:time"]
  seasonal <- tibble::tibble(term = "sex:time",
                             component = "seasonal",
                             level = level,
                             .fitted = seas)
  effect <- ans_expected$.fitted[ans_expected$component == "effect" & ans_expected$term == "sex:time"]
  trend <- effect - seas
  trend <- tibble::tibble(term = "sex:time",
                          component = "trend",
                          level = level,
                          .fitted = trend)
  ans_expected <- ans_expected[!(ans_expected$component == "hyperrand" & ans_expected$term == "sex:time"),]
  ans_expected <- vctrs::vec_rbind(ans_expected, seasonal, trend)
  expect_equal(ans_obtained, ans_expected)
})


## 'infer_trend_cyc_seas_err_seasfix' -----------------------------------------------

test_that("'infer_trend_cyc_seas_err_seasfix_forecast' works", {
  set.seed(0)
  data <- expand.grid(age = 0:4, time = 2000:2005, sex = c("F", "M"))
  data$popn <- rpois(n = nrow(data), lambda = 100)
  data$deaths <- rpois(n = nrow(data), lambda = 10)
  formula <- deaths ~ sex * time + age
  mod <- mod_pois(formula = formula,
                  data = data,
                  exposure = popn) |>
                  set_prior(sex:time ~ RW_Seas(n = 3, s_seas = 0)) |>
                  set_n_draw(n = 10) |>
                  fit(mod)
  components <- components(mod)
  ans <- infer_trend_cyc_seas_err_seasfix_forecast(prior = mod$priors[["sex:time"]],
                                                   dimnames_term = mod$dimnames_terms[["sex:time"]],
                                                   var_time = mod$var_time,
                                                   var_age = mod$var_age,
                                                   components = components)
  season <- ans$.fitted[ans$term == "sex:time" & ans$component == "seasonal"]
  trend <- ans$.fitted[ans$term == "sex:time" & ans$component == "trend"]
  effect <- ans$.fitted[ans$term == "sex:time" & ans$component == "effect"]
  expect_equal(effect, season + trend)
})


## 'infer_trend_cyc_seas_err_seasvary' ----------------------------------------------

test_that("'infer_trend_cyc_seas_err_seasvary' works", {
  set.seed(0)
  data <- expand.grid(age = 0:4, time = 2000:2020, sex = c("F", "M"))
  data$popn <- rpois(n = nrow(data), lambda = 100)
  data$deaths <- rpois(n = nrow(data), lambda = 10)
  formula <- deaths ~ sex * time + age
  mod <- mod_pois(formula = formula,
                  data = data,
                  exposure = popn) |>
                  set_prior(sex:time ~ RW_Seas(n = 3)) |>
                  set_n_draw(n_draw = 10) |>
                  fit(mod)
  term <- make_term_components(mod)
  comp <- make_comp_components(mod)
  level <- make_level_components(mod)
  draws <- make_draws_components(mod)
  draws <- as.matrix(draws)
  .fitted <- rvec::rvec_dbl(draws)
  components <- tibble::tibble(term = term,
                               component = comp,
                               level = level,
                               .fitted = .fitted)
  matrix_along_by <- make_matrix_along_by_effect(along = mod$priors[["sex:time"]]$specific$along,
                                                 dimnames_term = mod$dimnames_terms[["sex:time"]],
                                                 var_time = mod$var_time,
                                                 var_age = mod$var_age)
  ans_obtained <- infer_trend_cyc_seas_err_seasvary(prior = mod$priors[["sex:time"]],
                                                    dimnames_term = mod$dimnames_terms[["sex:time"]],
                                                    var_time = mod$var_time,
                                                    var_age = mod$var_age,
                                                    components = components)
  ans_expected <- components
  seas <- ans_expected$.fitted[ans_expected$component == "hyperrand" & ans_expected$term == "sex:time"]
  ans_expected$.fitted[ans_expected$component == "hyperrand" & ans_expected$term == "sex:time"] <- seas
  ans_expected$component[ans_expected$component == "hyperrand" & ans_expected$term == "sex:time"] <- "seasonal"
  effect <- ans_expected$.fitted[ans_expected$component == "effect" & ans_expected$term == "sex:time"]
  trend <- effect - seas
  level <- ans_expected$level[ans_expected$component == "effect" & ans_expected$term == "sex:time"]
  trend <- tibble::tibble(term = "sex:time",
                          component = "trend",
                          level = level,
                          .fitted = trend)
  ans_expected <- vctrs::vec_rbind(ans_expected, trend)
  expect_equal(ans_obtained, ans_expected)
})


## 'infer_trend_cyc_seas_err_seasvary_forecast' -------------------------------

test_that("'infer_trend_cyc_seas_err_seasvary_forecast' works", {
  set.seed(0)
  data <- expand.grid(age = 0:4, time = 2000:2005, sex = c("F", "M"))
  data$popn <- rpois(n = nrow(data), lambda = 100)
  data$deaths <- rpois(n = nrow(data), lambda = 10)
  formula <- deaths ~ sex * time + age
  mod <- mod_pois(formula = formula,
                  data = data,
                  exposure = popn) |>
                  set_prior(sex:time ~ RW_Seas(n = 3)) |>
                  set_n_draw(n = 10) |>
                  fit(mod)
  components <- components(mod)
  ans <- infer_trend_cyc_seas_err_seasvary_forecast(prior = mod$priors[["sex:time"]],
                                                    dimnames_term = mod$dimnames_terms[["sex:time"]],
                                                    var_time = mod$var_time,
                                                    var_age = mod$var_age,
                                                    components = components)
  season <- ans$.fitted[ans$term == "sex:time" & ans$component == "seasonal"]
  trend <- ans$.fitted[ans$term == "sex:time" & ans$component == "trend"]
  effect <- ans$.fitted[ans$term == "sex:time" & ans$component == "effect"]
  expect_equal(effect, season + trend)
})


## 'rvec_to_mean' -------------------------------------------------------------

test_that("'rvec_to_mean' works with valid inputs", {
  data <- tibble::tibble(a = 1:3,
                         b = rvec::rvec(matrix(1:12, nr = 3)),
                         c = rvec::rvec(matrix(FALSE, nr = 3, nc = 2)),
                         d = c("a", "b", "c"))
  ans_obtained <- rvec_to_mean(data)
  ans_expected <- tibble::tibble(a = 1:3,
                         b = rowMeans(matrix(1:12, nr = 3)),
                         c = rowMeans(matrix(FALSE, nr = 3, nc = 2)),
                         d = c("a", "b", "c"))
  expect_identical(ans_obtained, ans_expected)
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


## 'sort_components' ----------------------------------------------------------

test_that("'sort_components' works with valid inputs", {
  components <- tibble::tribble(~term,         ~component, ~level,
                                "sex",          "effect",   "m",
                                "time",         "seasonal", "2000",
                                "sex",          "hyper",    "sd",
                                "time",         "effect",   "2000",
                                "sex",          "effect",   "f",
                                "(Intercept)", "effect",   "(Intercept)",
                                "time",         "cyclical", "2000")
  mod <- list(formula = deaths ~ time + sex)
  ans_obtained <- sort_components(components = components, mod = mod)
  ans_expected <- tibble::tribble(~term,         ~component, ~level,
                                  "(Intercept)", "effect",   "(Intercept)",
                                  "time",         "effect",   "2000",
                                  "time",         "cyclical", "2000",
                                  "time",         "seasonal", "2000",
                                  "sex",          "effect",   "m",
                                  "sex",          "effect",   "f",
                                  "sex",          "hyper",    "sd")
  expect_identical(ans_obtained, ans_expected)
})

test_that("'sort_components' raises correct effor with invalid component", {
  components <- tibble::tribble(~term,         ~component, ~level,
                                "(Intercept)", "effect",   "(Intercept)",
                                "time",         "seasonal", "2000",
                                "sex",          "wrong",    "sd")
  expect_error(sort_components(components, mod = list(formula = deaths ~ age)),
               "Internal error: \"wrong\" not a valid value for `component`.")
})


## 'standardize_anova' --------------------------------------------------------

test_that("'standardize_anova' works", {
  set.seed(0)
  data <- expand.grid(age = 0:4, time = 2000:2005, sex = c("F", "M"))
  data$popn <- rpois(n = nrow(data), lambda = 100)
  data$deaths <- rpois(n = nrow(data), lambda = 10)
  formula <- deaths ~ sex * time + age
  mod <- mod_pois(formula = formula,
                  data = data,
                  exposure = popn)
  mod <- set_n_draw(mod, 5)
  mod <- fit(mod)
  comp <- components(mod, standardize = "none", quiet = TRUE)
  data <- mod$data
  dimnames_terms <- mod$dimnames_terms
  linpred <- make_linpred_comp(components = comp,
                               data = data,
                               dimnames_terms = dimnames_terms)
  ans <- standardize_anova(components = comp,
                           data = data,
                           linpred = linpred,
                           dimnames_terms = dimnames_terms)
  effect <- comp$.fitted[comp$component == "effect"]
  m <- as.matrix(make_combined_matrix_effect_outcome(mod))
  expect_equal(m %*% ans$.fitted[ans$component == "effect"], m %*% effect)
})


## 'transform_hyper_ar' -------------------------------------------------------

test_that("'transform_hyper_ar' works with 'bage_prior_ar - AR1'", {
  shifted_invlogit <- function(x) {
    ans <- exp(x) / (1 + exp(x))
    0.18 * ans + 0.8
  }
  l <- transform_hyper_ar(prior = AR1())
  expect_equal(l[[1]](0.35), shifted_invlogit(0.35))
  expect_equal(l[[2]](0.35), exp(0.35))
})

test_that("'transform_hyper_ar' works with 'bage_prior_svd_ar - AR'", {
  shifted_invlogit <- function(x) {
    ans <- exp(x) / (1 + exp(x))
    2 * ans - 1
  }
  l <- transform_hyper_ar(prior = SVD_AR(HMD))
  expect_equal(l[[1]](0.35), shifted_invlogit(0.35))
  expect_equal(l[[2]](0.35), shifted_invlogit(0.35))
  expect_equal(l[[3]](0.35), exp(0.35))
})

