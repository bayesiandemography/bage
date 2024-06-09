
## 'estimate_lin' -------------------------------------------------------------

test_that("'estimate_lin' works with valid inputs - n_by = 1", {
  slope <- rvec::rvec(matrix(as.numeric(1:5), nr = 1))
  matrix_along_by <- matrix(0:9, nr = 10)
  ans_obtained <- estimate_lin(slope = slope, matrix_along_by_est = matrix_along_by)
  s <- seq(from = -1, to = 1, length.out = 10)
  ans_expected <- rvec::rvec(outer(s, 1:5))
  expect_identical(ans_obtained, ans_expected)
})

test_that("'estimate_lin' works with valid inputs - n_by = 1", {
  slope <- rvec::rvec(matrix(rnorm(10), nr = 2))
  matrix_along_by <- matrix(0:9, nr = 5)
  ans_obtained <- estimate_lin(slope = slope, matrix_along_by_est = matrix_along_by)
  s <- seq(from = -1, to = 1, length.out = 5)
  ans_expected <- c(slope[1] * s, slope[2] * s)
  expect_identical(ans_obtained, ans_expected)
})

## 'forecast_ar' --------------------------------------------------------------

test_that("'forecast_ar' works, n_by = 1", {
  set.seed(0)
  ar_est <- rvec::rnorm_rvec(n = 5, n_draw = 10)
  coef <- rvec::runif_rvec(n = 3, n_draw = 10)
  sd <- rvec::runif_rvec(n = 1, n_draw = 10)
  matrix_along_by_est <- matrix(0:4, nr = 5)
  matrix_along_by_forecast <- matrix(0:5, nr = 6)
  levels_forecast <- letters[6:11]
  set.seed(1)
  ans_obtained <- forecast_ar(ar_est = ar_est,
                              coef = coef,
                              sd = sd,
                              matrix_along_by_est = matrix_along_by_est,
                              matrix_along_by_forecast = matrix_along_by_forecast)
  set.seed(1)
  ans_expected <- rep(ar_est[1], 6)
  ans_expected[1] <- rvec::rnorm_rvec(n = 1,
                                      mean = sum(coef * ar_est[3:5]),
                                      sd = sd)
  ans_expected[2] <- rvec::rnorm_rvec(n = 1,
                                      mean = sum(coef * c(ar_est[4:5],
                                                          ans_expected[1])),
                                      sd = sd)
  ans_expected[3] <- rvec::rnorm_rvec(n = 1,
                                      mean = sum(coef * c(ar_est[5], ans_expected[1:2])),
                                      sd = sd)
  ans_expected[4] <- rvec::rnorm_rvec(n = 1,
                                      mean = sum(coef * ans_expected[1:3]),
                                      sd = sd)
  ans_expected[5] <- rvec::rnorm_rvec(n = 1,
                                      mean = sum(coef * ans_expected[2:4]),
                                      sd = sd)
  ans_expected[6] <- rvec::rnorm_rvec(n = 1,
                                      mean = sum(coef * ans_expected[3:5]),
                                      sd = sd)
  expect_equal(ans_obtained, ans_expected)
})

test_that("'forecast_ar' works, n_by = 2", {
  set.seed(0)
  ar_est <- rvec::rnorm_rvec(n = 10, n_draw = 10)
  coef <- rvec::runif_rvec(n = 2, n_draw = 10)
  sd <- rvec::runif_rvec(n = 1, n_draw = 10)
  matrix_along_by_est <- matrix(0:9, nr = 5)
  matrix_along_by_forecast <- matrix(0:11, nr = 6)
  levels_forecast <- letters[11:22]
  set.seed(1)
  ans_obtained <- forecast_ar(ar_est = ar_est,
                              coef = coef,
                              sd = sd,
                              matrix_along_by_est = matrix_along_by_est,
                              matrix_along_by_forecast = matrix_along_by_forecast)
  ans_expected <- rvec::rnorm_rvec(n = 12, n_draw = 10)
  set.seed(1)
  ans_expected[1] <- rvec::rnorm_rvec(n = 1,
                                      mean = sum(coef * ar_est[4:5]),
                                      sd = sd)
  ans_expected[2] <- rvec::rnorm_rvec(n = 1,
                                      mean = sum(coef * c(ar_est[5],
                                                          ans_expected[1])),
                                      sd = sd)
  ans_expected[3] <- rvec::rnorm_rvec(n = 1,
                                      mean = sum(coef * ans_expected[1:2]),
                                      sd = sd)
  ans_expected[4] <- rvec::rnorm_rvec(n = 1,
                                      mean = sum(coef * ans_expected[2:3]),
                                      sd = sd)
  ans_expected[5] <- rvec::rnorm_rvec(n = 1,
                                      mean = sum(coef * ans_expected[3:4]),
                                      sd = sd)
  ans_expected[6] <- rvec::rnorm_rvec(n = 1,
                                      mean = sum(coef * ans_expected[4:5]),
                                      sd = sd)
  ans_expected[7] <- rvec::rnorm_rvec(n = 1,
                                      mean = sum(coef * ar_est[9:10]),
                                      sd = sd)
  ans_expected[8] <- rvec::rnorm_rvec(n = 1,
                                      mean = sum(coef * c(ar_est[10],
                                                          ans_expected[7])),
                                      sd = sd)
  ans_expected[9] <- rvec::rnorm_rvec(n = 1,
                                      mean = sum(coef * ans_expected[7:8]),
                                      sd = sd)
  ans_expected[10] <- rvec::rnorm_rvec(n = 1,
                                       mean = sum(coef * ans_expected[8:9]),
                                       sd = sd)
  ans_expected[11] <- rvec::rnorm_rvec(n = 1,
                                       mean = sum(coef * ans_expected[9:10]),
                                       sd = sd)
  ans_expected[12] <- rvec::rnorm_rvec(n = 1,
                                       mean = sum(coef * ans_expected[10:11]),
                                       sd = sd)
  expect_equal(ans_obtained, ans_expected)
})


## 'forecast_components' ------------------------------------------------------

test_that("'forecast_components' works", {
  set.seed(0)
  data <- expand.grid(age = 0:9, time = 2000:2005, sex = c("F", "M"))
  data$deaths <- rpois(n = nrow(data), lambda = 10000)
  formula <- deaths ~ age * sex + sex * time
  mod <- mod_pois(formula = formula,
                  data = data,
                  exposure = 1)
  mod <- set_n_draw(mod, n = 10)
  mod <- fit(mod)
  components_est <- components(mod)
  labels_forecast <- 2006:2008
  set.seed(1)
  ans <- forecast_components(mod = mod,
                             components_est = components_est,
                             labels_forecast = labels_forecast)
  expect_setequal(2006:2008,
                  ans$level[ans$term == "time"])
  expect_setequal(c(paste("F", 2006:2008, sep = "."),
                    paste("M", 2006:2008, sep = ".")),
                  ans$level[ans$term == "sex:time"])
})


## 'forecast_lin' -------------------------------------------------------------

test_that("'forecast_lin' works with n_by = 1", {
  set.seed(0)
  slope <- rvec::rnorm_rvec(n = 1, n_draw = 10)
  matrix_along_by_est <- matrix(0:4, nr = 5)
  matrix_along_by_forecast <- matrix(0:5, nr = 6)
  set.seed(1)
  ans_obtained <- forecast_lin(slope = slope,
                               matrix_along_by_est = matrix_along_by_est,
                               matrix_along_by_forecast = matrix_along_by_forecast)
  ans_expected <- seq(from = 1.5, length.out = 6, by = 0.5) * slope
  expect_equal(ans_obtained, ans_expected)
})

test_that("'forecast_lin' works with  n_by = 2", {
  set.seed(0)
  slope <- rvec::rnorm_rvec(n = 2, n_draw = 10)
  matrix_along_by_est <- matrix(0:9, nr = 5)
  matrix_along_by_forecast <- matrix(0:11, nr = 6)
  set.seed(1)
  ans_obtained <- forecast_lin(slope = slope,
                               matrix_along_by_est = matrix_along_by_est,
                               matrix_along_by_forecast = matrix_along_by_forecast)
  s <- seq(from = 1.5, length.out = 6, by = 0.5)
  ans_expected <- c(s * slope[1], s * slope[2])
  expect_equal(ans_obtained, ans_expected)
})


## 'forecast_norm' -------------------------------------------------------------

test_that("'forecast_lin' works with n_by = 1", {
  set.seed(0)
  sd <- rvec::runif_rvec(n = 1, n_draw = 10)
  matrix_along_by_forecast <- matrix(0:5, nr = 6)
  set.seed(1)
  ans_obtained <- forecast_norm(sd = sd,
                                matrix_along_by_forecast = matrix_along_by_forecast)
  set.seed(1)
  ans_expected <- rvec::rnorm_rvec(n = 6, sd = sd)
  expect_equal(ans_obtained, ans_expected)
})


## 'forecast_rw' --------------------------------------------------------------

test_that("'forecast_rw' works with n_by = 1", {
  set.seed(0)
  rw_est <- rvec::rnorm_rvec(n = 5, n_draw = 10)
  sd <- rvec::runif_rvec(n = 1, n_draw = 10)
  matrix_along_by_est <- matrix(0:4, nr = 5)
  matrix_along_by_forecast <- matrix(0:5, nr = 6)
  set.seed(1)
  ans_obtained <- forecast_rw(rw_est = rw_est,
                              sd = sd,
                              matrix_along_by_est = matrix_along_by_est,
                              matrix_along_by_forecast = matrix_along_by_forecast)
  ans_expected <- rvec::rnorm_rvec(n = 6, n_draw = 10)
  set.seed(1)
  ans_expected[1] <- rvec::rnorm_rvec(n = 1,
                                      mean = rw_est[5],
                                      sd = sd)
  for (i in 2:6)
    ans_expected[i] <- rvec::rnorm_rvec(n = 1,
                                        mean = ans_expected[i-1],
                                        sd = sd)
  expect_equal(ans_obtained, ans_expected)
})

test_that("'forecast_rw' works with  n_by = 2", {
  set.seed(0)
  rw_est <- rvec::rnorm_rvec(n = 10, n_draw = 10)
  sd <- rvec::runif_rvec(n = 1, n_draw = 10)
  matrix_along_by_est <- matrix(0:9, nr = 5)
  matrix_along_by_forecast <- matrix(0:11, nr = 6)
  set.seed(1)
  ans_obtained <- forecast_rw(rw_est = rw_est,
                              sd = sd,
                              matrix_along_by_est = matrix_along_by_est,
                              matrix_along_by_forecast = matrix_along_by_forecast)
  ans_expected <- rvec::rnorm_rvec(n = 12, n_draw = 10)
  set.seed(1)
  ans_expected[1] <- rvec::rnorm_rvec(n = 1,
                                      mean = rw_est[5],
                                      sd = sd)
  for (i in 2:6)
    ans_expected[i] <- rvec::rnorm_rvec(n = 1,
                                        mean = ans_expected[i-1],
                                        sd = sd)
  ans_expected[7] <- rvec::rnorm_rvec(n = 1,
                                      mean = rw_est[10],
                                      sd = sd)
  for (i in 8:12)
    ans_expected[i] <- rvec::rnorm_rvec(n = 1,
                                        mean = ans_expected[i-1],
                                        sd = sd)
  expect_equal(ans_obtained, ans_expected)
})


## 'forecast_rw2' -------------------------------------------------------------

test_that("'forecast_rw2' works with n_by = 1", {
  set.seed(0)
  rw2_est <- rvec::rnorm_rvec(n = 5, n_draw = 10)
  sd <- rvec::runif_rvec(n = 1, n_draw = 10)
  matrix_along_by_est <- matrix(0:4, nr = 5)
  matrix_along_by_forecast <- matrix(0:5, nr = 6)
  set.seed(1)
  ans_obtained <- forecast_rw2(rw2_est = rw2_est,
                               sd = sd,
                               matrix_along_by_est = matrix_along_by_est,
                               matrix_along_by_forecast = matrix_along_by_forecast)
  ans_expected <- rvec::rnorm_rvec(n = 6, n_draw = 10)
  set.seed(1)
  ans_expected[1] <- rvec::rnorm_rvec(n = 1,
                                      mean = 2 * rw2_est[5] - rw2_est[4],
                                      sd = sd)
  ans_expected[2] <- rvec::rnorm_rvec(n = 1,
                                      mean = 2 * ans_expected[1] - rw2_est[5],
                                      sd = sd)
  for (i in 3:6)
    ans_expected[i] <- rvec::rnorm_rvec(n = 1,
                                        mean = 2 * ans_expected[i-1] - ans_expected[i-2],,
                                        sd = sd)
  expect_equal(ans_obtained, ans_expected)
})

test_that("'forecast_rw' works with  n_by = 2", {
  set.seed(0)
  rw2_est <- rvec::rnorm_rvec(n = 10, n_draw = 10)
  sd <- rvec::runif_rvec(n = 1, n_draw = 10)
  matrix_along_by_est <- matrix(0:9, nr = 5)
  matrix_along_by_forecast <- matrix(0:11, nr = 6)
  set.seed(1)
  ans_obtained <- forecast_rw2(rw2_est = rw2_est,
                               sd = sd,
                               matrix_along_by_est = matrix_along_by_est,
                               matrix_along_by_forecast = matrix_along_by_forecast)
  ans_expected <- rvec::rnorm_rvec(n = 12, n_draw = 10)
  set.seed(1)
  ans_expected[1] <- rvec::rnorm_rvec(n = 1,
                                      mean = 2 * rw2_est[5] - rw2_est[4],
                                      sd = sd)
  ans_expected[2] <- rvec::rnorm_rvec(n = 1,
                                      mean = 2 * ans_expected[1] - rw2_est[5],
                                      sd = sd)
  for (i in 3:6)
    ans_expected[i] <- rvec::rnorm_rvec(n = 1,
                                        mean = 2 * ans_expected[i-1] - ans_expected[i-2],,
                                        sd = sd)
  ans_expected[7] <- rvec::rnorm_rvec(n = 1,
                                      mean = 2 * rw2_est[10] - rw2_est[9],
                                      sd = sd)
  ans_expected[8] <- rvec::rnorm_rvec(n = 1,
                                      mean = 2 * ans_expected[7] - rw2_est[10],
                                      sd = sd)
  for (i in 9:12)
    ans_expected[i] <- rvec::rnorm_rvec(n = 1,
                                        mean = 2 * ans_expected[i-1] - ans_expected[i-2],
                                        sd = sd)
  expect_equal(ans_obtained, ans_expected)
})


## 'forecast_seasfix' ---------------------------------------------------------

test_that("'forecast_seasfix' works", {
  set.seed(0)
  seas_est <- rvec::rnorm_rvec(n = 4, n_draw = 10)
  seas_est <- seas_est[c(1,2,1,2,1, 3,4,3,4,3)]
  matrix_along_by_est <- matrix(0:9, nr = 5)
  matrix_along_by_forecast <- matrix(0:11, nr = 6)
  set.seed(1)
  ans_obtained <- forecast_seasfix(n = 2L,
                                   seas_est,
                                   matrix_along_by_est = matrix_along_by_est,
                                   matrix_along_by_forecast = matrix_along_by_forecast)
  ans_expected <- rep(seas_est[1], 12)
  ans_expected[c(1, 3, 5)] <- seas_est[2]
  ans_expected[c(2, 4, 6)] <- seas_est[1]
  ans_expected[c(7, 9, 11)] <- seas_est[7]
  ans_expected[c(8, 10, 12)] <- seas_est[6]
  expect_equal(ans_obtained, ans_expected)
})


## 'forecast_seasfix' ---------------------------------------------------------

test_that("'forecast_seasvary' works", {
  set.seed(0)
  seas_est <- rvec::rnorm_rvec(n = 10, n_draw = 10)
  sd <- rvec::runif_rvec(n = 1, n_draw = 10)
  matrix_along_by_est <- matrix(0:9, nr = 5)
  matrix_along_by_forecast <- matrix(0:11, nr = 6)
  set.seed(1)
  ans_obtained <- forecast_seasvary(n = 2L,
                                    seas_est = seas_est,
                                    sd = sd,
                                    matrix_along_by_est = matrix_along_by_est,
                                    matrix_along_by_forecast = matrix_along_by_forecast)
  ans_expected <- rvec::rnorm_rvec(n = 12, n_draw = 10)
  set.seed(1)
  ans_expected[1] <- rvec::rnorm_rvec(n = 1,
                                      seas_est[4],
                                      sd = sd)
  ans_expected[2] <- rvec::rnorm_rvec(n = 1,
                                      mean = seas_est[5],
                                      sd = sd)
  for (i in 3:6)
    ans_expected[i] <- rvec::rnorm_rvec(n = 1,
                                        mean = ans_expected[i-2],
                                        sd = sd)
  ans_expected[7] <- rvec::rnorm_rvec(n = 1,
                                      seas_est[9],
                                      sd = sd)
  ans_expected[8] <- rvec::rnorm_rvec(n = 1,
                                      mean = seas_est[10],
                                      sd = sd)
  for (i in 9:12)
    ans_expected[i] <- rvec::rnorm_rvec(n = 1,
                                        mean = ans_expected[i-2],
                                        sd = sd)
  expect_equal(ans_obtained, ans_expected)
})


## 'make_data_forecast' -------------------------------------------------------

test_that("'forecast_components' works", {
  set.seed(0)
  data <- expand.grid(age = 0:9, time = 2000:2005, sex = c("F", "M"))
  data$deaths <- rpois(n = nrow(data), lambda = 100)
  data$exposure <- 100
  data$unused <- 33
  formula <- deaths ~ age * sex + sex * time
  mod <- mod_pois(formula = formula,
                  data = data,
                  exposure = exposure)
  mod <- set_n_draw(mod, n = 10)
  ans <- make_data_forecast(mod = mod,
                            labels_forecast = 2006:2008)
  expect_identical(names(ans), names(data))
  expect_true(all(is.na(ans$deaths)))
  expect_true(all(is.na(ans$exposure)))
  expect_true(all(is.na(ans$unused)))
  expect_setequal(ans$age, data$age)
  expect_setequal(ans$sex, data$sex)
  expect_setequal(ans$time, 2006:2008)
})
