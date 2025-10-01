
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
                                      mean = sum(rev(coef) * ar_est[3:5]),
                                      sd = sd)
  ans_expected[2] <- rvec::rnorm_rvec(n = 1,
                                      mean = sum(rev(coef) * c(ar_est[4:5],
                                                          ans_expected[1])),
                                      sd = sd)
  ans_expected[3] <- rvec::rnorm_rvec(n = 1,
                                      mean = sum(rev(coef) * c(ar_est[5], ans_expected[1:2])),
                                      sd = sd)
  ans_expected[4] <- rvec::rnorm_rvec(n = 1,
                                      mean = sum(rev(coef) * ans_expected[1:3]),
                                      sd = sd)
  ans_expected[5] <- rvec::rnorm_rvec(n = 1,
                                      mean = sum(rev(coef) * ans_expected[2:4]),
                                      sd = sd)
  ans_expected[6] <- rvec::rnorm_rvec(n = 1,
                                      mean = sum(rev(coef) * ans_expected[3:5]),
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
                                      mean = sum(rev(coef) * ar_est[4:5]),
                                      sd = sd)
  ans_expected[2] <- rvec::rnorm_rvec(n = 1,
                                      mean = sum(rev(coef) * c(ar_est[5],
                                                          ans_expected[1])),
                                      sd = sd)
  ans_expected[3] <- rvec::rnorm_rvec(n = 1,
                                      mean = sum(rev(coef) * ans_expected[1:2]),
                                      sd = sd)
  ans_expected[4] <- rvec::rnorm_rvec(n = 1,
                                      mean = sum(rev(coef) * ans_expected[2:3]),
                                      sd = sd)
  ans_expected[5] <- rvec::rnorm_rvec(n = 1,
                                      mean = sum(rev(coef) * ans_expected[3:4]),
                                      sd = sd)
  ans_expected[6] <- rvec::rnorm_rvec(n = 1,
                                      mean = sum(rev(coef) * ans_expected[4:5]),
                                      sd = sd)
  ans_expected[7] <- rvec::rnorm_rvec(n = 1,
                                      mean = sum(rev(coef) * ar_est[9:10]),
                                      sd = sd)
  ans_expected[8] <- rvec::rnorm_rvec(n = 1,
                                      mean = sum(rev(coef) * c(ar_est[10],
                                                          ans_expected[7])),
                                      sd = sd)
  ans_expected[9] <- rvec::rnorm_rvec(n = 1,
                                      mean = sum(rev(coef) * ans_expected[7:8]),
                                      sd = sd)
  ans_expected[10] <- rvec::rnorm_rvec(n = 1,
                                       mean = sum(rev(coef) * ans_expected[8:9]),
                                       sd = sd)
  ans_expected[11] <- rvec::rnorm_rvec(n = 1,
                                       mean = sum(rev(coef) * ans_expected[9:10]),
                                       sd = sd)
  ans_expected[12] <- rvec::rnorm_rvec(n = 1,
                                       mean = sum(rev(coef) * ans_expected[10:11]),
                                       sd = sd)
  expect_equal(ans_obtained, ans_expected)
})


## 'forecast_ar_svd' ----------------------------------------------------------

test_that("'forecast_ar_svd' works - con is 'none'", {
  set.seed(0)
  prior <- SVD_AR1(HMD)
  dimnames_term <- list(year = letters[1:5],
                        age = poputils::age_labels(type = "lt", max = 60))
  var_time <- "year"
  var_age <- "age"
  var_sexgender <- "sex"
  labels_forecast <- letters[6:11]
  dimnames_forecast <- replace(dimnames_term, var_time, list(labels_forecast))
  components <- vctrs::vec_rbind(tibble::tibble(term = "year:age",
                                                component = "hyper",
                                                level = c("coef", "sd"),
                                                .fitted = rvec::runif_rvec(n = 2, n_draw = 10)),
                                 tibble::tibble(term = "year:age",
                                                component = "svd",
                                                level = paste(paste0("comp", 1:3),
                                                              rep(letters[1:5], each = 3),
                                                              sep = "."),
                                                .fitted = rvec::rnorm_rvec(n = 15, n_draw = 10)))
  set.seed(1)
  ans_obtained <- forecast_ar_svd(prior = prior,
                                  dimnames_term = dimnames_term,
                                  dimnames_forecast = dimnames_forecast,
                                  var_time = var_time,
                                  var_age = var_age,
                                  var_sexgender = var_sexgender,
                                  components = components,
                                  labels_forecast = labels_forecast)
  ans_expected <- rvec::rnorm_rvec(n = 18, n_draw = 10)
  coef <- components$.fitted[components$level == "coef"]
  sd <- components$.fitted[components$level == "sd"]
  set.seed(1)
  for (i in 1:3) {
    ans_expected[i] <- rvec::rnorm_rvec(n = 1,
                                          mean = coef * components$.fitted[14 + i],
                                          sd = sd)
    for (j in 1:5)
      ans_expected[i + j * 3] <- rvec::rnorm_rvec(n = 1,
                                                  mean = coef * ans_expected[i + (j-1) * 3],
                                                  sd = sd)
  }
  expect_equal(ans_obtained, ans_expected)
})

test_that("'forecast_ar_svd' works - con is 'by' (does not affect results)", {
  set.seed(0)
  prior <- SVD_AR1(HMD, con = "by")
  dimnames_term <- list(year = letters[1:5],
                        age = poputils::age_labels(type = "lt", max = 60))
  var_time <- "year"
  var_age <- "age"
  var_sexgender <- "sex"
  labels_forecast <- letters[6:11]
  dimnames_forecast <- replace(dimnames_term, var_time, list(labels_forecast))
  components <- vctrs::vec_rbind(tibble::tibble(term = "year:age",
                                                component = "hyper",
                                                level = c("coef", "sd"),
                                                .fitted = rvec::runif_rvec(n = 2, n_draw = 10)),
                                 tibble::tibble(term = "year:age",
                                                component = "svd",
                                                level = paste(paste0("comp", 1:3),
                                                              rep(letters[1:5], each = 3),
                                                              sep = "."),
                                                .fitted = rvec::rnorm_rvec(n = 15, n_draw = 10)))
  set.seed(1)
  ans_obtained <- forecast_ar_svd(prior = prior,
                                  dimnames_term = dimnames_term,
                                  dimnames_forecast = dimnames_forecast,
                                  var_time = var_time,
                                  var_age = var_age,
                                  var_sexgender = var_sexgender,
                                  components = components,
                                  labels_forecast = labels_forecast)
  ans_expected <- rvec::rnorm_rvec(n = 18, n_draw = 10)
  coef <- components$.fitted[components$level == "coef"]
  sd <- components$.fitted[components$level == "sd"]
  set.seed(1)
  for (i in 1:3) {
    ans_expected[i] <- rvec::rnorm_rvec(n = 1,
                                          mean = coef * components$.fitted[14 + i],
                                          sd = sd)
    for (j in 1:5)
      ans_expected[i + j * 3] <- rvec::rnorm_rvec(n = 1,
                                                  mean = coef * ans_expected[i + (j-1) * 3],
                                                  sd = sd)
  }
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
  data_forecast <- expand.grid(labels_forecast,
                               age = 0:9,
                               sex = c("F", "M"),
                               KEEP.OUT.ATTRS = FALSE)
  set.seed(1)
  ans <- forecast_components(mod = mod,
                             components_est = components_est,
                             data_forecast = data_forecast,
                             labels_forecast = labels_forecast,
                             has_newdata = TRUE,
                             is_forecast_obs = FALSE)
  expect_setequal(2006:2008,
                  ans$level[ans$term == "time"])
  expect_setequal(c(paste("F", 2006:2008, sep = "."),
                    paste("M", 2006:2008, sep = ".")),
                  ans$level[ans$term == "sex:time"])
})

test_that("'forecast_components' works - includes data model", {
  set.seed(0)
  data <- expand.grid(age = 0:9, time = 2000:2005, sex = c("F", "M"))
  data$deaths <- rpois(n = nrow(data), lambda = 10000)
  formula <- deaths ~ age * sex + sex * time
  mod <- mod_pois(formula = formula,
                  data = data,
                  exposure = 1) |>
    set_n_draw(n = 10) |>
    set_datamod_undercount(prob = data.frame(mean = 0.9, disp = 0.1)) |>
    fit()
  components_est <- components(mod)
  labels_forecast <- 2006:2008
  data_forecast <- expand.grid(time = labels_forecast,
                               age = 0:9,
                               sex = c("F", "M"),
                               KEEP.OUT.ATTRS = FALSE)
  set.seed(1)
  ans <- forecast_components(mod = mod,
                             components_est = components_est,
                             data_forecast = data_forecast,
                             labels_forecast = labels_forecast,
                             has_newdata = TRUE,
                             is_forecast_obs = TRUE)
  expect_setequal(2006:2008,
                  ans$level[ans$term == "time"])
  expect_setequal(c(paste("F", 2006:2008, sep = "."),
                    paste("M", 2006:2008, sep = ".")),
                  ans$level[ans$term == "sex:time"])
  expect_equal(nrow(subset(ans, term == "datamod")), 1L)
})

test_that("'forecast_components' throws error if can't forecast data model and is_forecast_obs is TRUE", {
  set.seed(0)
  data <- expand.grid(age = 0:9, time = 2000:2005, sex = c("F", "M"))
  data$deaths <- rpois(n = nrow(data), lambda = 10000)
  formula <- deaths ~ age * sex + sex * time
  mod <- mod_pois(formula = formula,
                  data = data,
                  exposure = 1) |>
    set_n_draw(n = 10) |>
    set_datamod_undercount(prob = data.frame(time = 2000:2005,
                                             mean = 0.9,
                                             disp = 0.1)) |>
    fit()
  components_est <- components(mod)
  labels_forecast <- 2006:2008
  data_forecast <- expand.grid(time = labels_forecast,
                               age = 0:9,
                               sex = c("F", "M"),
                               KEEP.OUT.ATTRS = FALSE)
  set.seed(1)
  expect_error(
    forecast_components(mod = mod,
                        components_est = components_est,
                        data_forecast = data_forecast,
                        labels_forecast = labels_forecast,
                        has_newdata = TRUE,
                        is_forecast_obs = TRUE),
    "`prob` does not include all levels of")
})


test_that("'forecast_components' ignore data model if can't forecast is and is_forecast_obs is FALSE", {
  set.seed(0)
  data <- expand.grid(age = 0:9, time = 2000:2005, sex = c("F", "M"))
  data$deaths <- rpois(n = nrow(data), lambda = 10000)
  formula <- deaths ~ age * sex + sex * time
  mod <- mod_pois(formula = formula,
                  data = data,
                  exposure = 1) |>
    set_n_draw(n = 10) |>
    set_datamod_undercount(prob = data.frame(time = 2000:2005,
                                             mean = 0.9,
                                             disp = 0.1)) |>
    fit()
  components_est <- components(mod)
  labels_forecast <- 2006:2008
  data_forecast <- expand.grid(time = labels_forecast,
                               age = 0:9,
                               sex = c("F", "M"),
                               KEEP.OUT.ATTRS = FALSE)
  set.seed(1)
  ans <- forecast_components(mod = mod,
                             components_est = components_est,
                             data_forecast = data_forecast,
                             labels_forecast = labels_forecast,
                             has_newdata = TRUE,
                             is_forecast_obs = FALSE)
  expect_setequal(2006:2008,
                  ans$level[ans$term == "time"])
  expect_setequal(c(paste("F", 2006:2008, sep = "."),
                    paste("M", 2006:2008, sep = ".")),
                  ans$level[ans$term == "sex:time"])
  expect_false("datamod" %in% ans$term)
})


## 'forecast_lin' -------------------------------------------------------------

test_that("'forecast_lin' works with n_by = 1", {
  set.seed(0)
  slope <- rvec::rnorm_rvec(n = 1, n_draw = 10)
  sd <- rvec::runif_rvec(n = 1, n_draw = 10)
  matrix_along_by_est <- matrix(0:4, nr = 5)
  matrix_along_by_forecast <- matrix(0:5, nr = 6)
  set.seed(1)
  ans_obtained <- forecast_lin(slope = slope,
                               sd = sd,
                               matrix_along_by_est = matrix_along_by_est,
                               matrix_along_by_forecast = matrix_along_by_forecast)
  set.seed(1)
  intercept <- -1 * (5 + 1) * slope / 2
  ans_expected <- rvec::rnorm_rvec(n = 6,
                                   mean = intercept + 6:11 * slope,
                                   sd = sd)
  expect_equal(ans_obtained, ans_expected)
})

test_that("'forecast_lin' works with  n_by = 2", {
  set.seed(0)
  slope <- rvec::rnorm_rvec(n = 2, n_draw = 10)
  sd <- rvec::runif_rvec(n = 1, n_draw = 10)
  matrix_along_by_est <- t(matrix(0:9, nr = 2))
  matrix_along_by_forecast <- t(matrix(0:11, nr = 2))
  set.seed(1)
  ans_obtained <- forecast_lin(slope = slope,
                               sd = sd,
                               matrix_along_by_est = matrix_along_by_est,
                               matrix_along_by_forecast = matrix_along_by_forecast)
  set.seed(1)
  intercept <- -0.5 * (5 + 1) * slope
  ans_expected <- c(rvec::rnorm_rvec(n = 6, mean = intercept[1] + 6:11 * slope[1], sd = sd),
                    rvec::rnorm_rvec(n = 6, mean = intercept[2] + 6:11 * slope[2], sd = sd))
  ans_expected <- ans_expected[c(1, 7, 2, 8, 3, 9, 4, 10, 5, 11, 6, 12)]
  expect_equal(ans_obtained, ans_expected)
})


## 'forecast_lin_trend' -------------------------------------------------------

test_that("'forecast_lin_trend' works with n_by = 1", {
  set.seed(0)
  slope <- rvec::rnorm_rvec(n = 1, n_draw = 10)
  matrix_along_by_est <- matrix(0:4, nr = 5)
  matrix_along_by_forecast <- matrix(0:5, nr = 6)
  set.seed(1)
  ans_obtained <- forecast_lin_trend(slope = slope,
                                     matrix_along_by_est = matrix_along_by_est,
                                     matrix_along_by_forecast = matrix_along_by_forecast)
  set.seed(1)
  intercept <- -0.5 * 6 * slope
  ans_expected <- intercept + 6:11 * slope
  expect_equal(ans_obtained, ans_expected)
})

test_that("'forecast_lin_trend' works with  n_by = 2, transposed", {
  set.seed(0)
  slope <- rvec::rnorm_rvec(n = 2, n_draw = 10)
  matrix_along_by_est <- t(matrix(0:9, nr = 2))
  matrix_along_by_forecast <- t(matrix(0:11, nr = 2))
  set.seed(1)
  ans_obtained <- forecast_lin_trend(slope = slope,
                                     matrix_along_by_est = matrix_along_by_est,
                                     matrix_along_by_forecast = matrix_along_by_forecast)
  set.seed(1)
  intercept <- -0.5 * 6 * slope
  ans_expected <- c(intercept[1] + 6:11 * slope[1], intercept[2] + 6:11 * slope[2])
  ans_expected <- ans_expected[c(1, 7, 2, 8, 3, 9, 4, 10, 5, 11, 6, 12)]
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

## 'forecast_rw_svd' ----------------------------------------------------------

test_that("'forecast_rw_svd' works", {
  set.seed(0)
  prior <- SVD_RW(HMD)
  dimnames_term <- list(year = letters[1:5],
                        age = poputils::age_labels(type = "lt", max = 60))
  var_time <- "year"
  var_age <- "age"
  var_sexgender <- "sex"
  labels_forecast <- letters[6:11]
  dimnames_forecast <- replace(dimnames_term, var_time, list(labels_forecast))
  components <- vctrs::vec_rbind(tibble::tibble(term = "year:age",
                                                component = "hyper",
                                                level = "sd",
                                                .fitted = rvec::runif_rvec(n = 1, n_draw = 10)),
                                 tibble::tibble(term = "year:age",
                                                component = "svd",
                                                level = paste(paste0("comp", 1:3),
                                                              rep(letters[1:5], each = 3),
                                                              sep = "."),
                                                .fitted = rvec::rnorm_rvec(n = 15, n_draw = 10)))
  set.seed(1)
  ans_obtained <- forecast_rw_svd(prior = prior,
                                  dimnames_term = dimnames_term,
                                  dimnames_forecast = dimnames_forecast,
                                  var_time = var_time,
                                  var_age = var_age,
                                  var_sexgender = var_sexgender,
                                  components = components,
                                  labels_forecast = labels_forecast)
  ans_expected <- rvec::rnorm_rvec(n = 18, n_draw = 10)
  sd <- components$.fitted[components$level == "sd"]
  set.seed(1)
  for (i in 1:3) {
    ans_expected[i] <- rvec::rnorm_rvec(n = 1,
                                        mean = components$.fitted[13 + i],
                                        sd = sd)
    for (j in 1:5)
      ans_expected[i + j * 3] <- rvec::rnorm_rvec(n = 1,
                                                  mean = ans_expected[i + (j-1) * 3],
                                                  sd = sd)
  }
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

test_that("'forecast_rw2' works with  n_by = 2", {
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


## 'forecast_rw2_svd' ----------------------------------------------------------

test_that("'forecast_rw2_svd' works", {
  set.seed(0)
  prior <- SVD_RW2(HMD)
  dimnames_term <- list(year = letters[1:5],
                        age = poputils::age_labels(type = "lt", max = 60))
  var_time <- "year"
  var_age <- "age"
  var_sexgender <- "sex"
  labels_forecast <- letters[6:11]
  dimnames_forecast <- replace(dimnames_term, var_time, list(labels_forecast))
  components <- vctrs::vec_rbind(tibble::tibble(term = "year:age",
                                                component = "hyper",
                                                level = "sd",
                                                .fitted = rvec::runif_rvec(n = 1, n_draw = 10)),
                                 tibble::tibble(term = "year:age",
                                                component = "svd",
                                                level = paste(paste0("comp", 1:3),
                                                              rep(letters[1:5], each = 3),
                                                              sep = "."),
                                                .fitted = rvec::rnorm_rvec(n = 15, n_draw = 10)))
  set.seed(1)
  ans_obtained <- forecast_rw2_svd(prior = prior,
                                   dimnames_term = dimnames_term,
                                   dimnames_forecast = dimnames_forecast,
                                   var_time = var_time,
                                   var_age = var_age,
                                   var_sexgender = var_sexgender,
                                   components = components,
                                   labels_forecast = labels_forecast)
  ans_expected <- rvec::rnorm_rvec(n = 18, n_draw = 10)
  sd <- components$.fitted[components$level == "sd"]
  set.seed(1)
  for (i in 1:3) {
    ans_expected[i] <- rvec::rnorm_rvec(n = 1,
                                        mean = 2 * components$.fitted[13 + i] -
                                          components$.fitted[10 + i],
                                        sd = sd)
    ans_expected[i + 3] <- rvec::rnorm_rvec(n = 1,
                                            mean = 2 * ans_expected[i] -
                                              components$.fitted[13 + i],
                                            sd = sd)
    for (j in 2:5)
      ans_expected[i + j * 3] <- rvec::rnorm_rvec(n = 1,
                                                  mean = 2 * ans_expected[i + (j-1) * 3] -
                                                 ans_expected[i + (j-2) * 3],
                                                  sd = sd)
  }
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


## 'make_data_forecast_labels' ------------------------------------------------

test_that("'make_data_forecast_labels' works - no covariates", {
  set.seed(0)
  data <- expand.grid(age = 0:9, time = 2000:2005, sex = c("F", "M"))
  data$deaths <- rpois(n = nrow(data), lambda = 100)
  data$exposure <- 100
  data$unused <- 33
  formula <- deaths ~ age * sex + sex * time
  mod <- mod_pois(formula = formula,
                  data = data,
                  exposure = exposure)
  ans <- make_data_forecast_labels(mod = mod,
                                   labels_forecast = 2006:2008)
  expect_identical(names(ans), names(data))
  expect_true(all(is.na(ans$deaths)))
  expect_true(all(is.na(ans$exposure)))
  expect_true(all(is.na(ans$unused)))
  expect_setequal(ans$age, data$age)
  expect_setequal(ans$sex, data$sex)
  expect_setequal(ans$time, 2006:2008)
})

test_that("'make_data_forecast_labels' works - with covariates", {
  set.seed(0)
  data <- expand.grid(age = 0:9, time = 2000:2005, sex = c("F", "M"))
  data$deaths <- rpois(n = nrow(data), lambda = 100)
  data$exposure <- 100
  data$unused <- 33
  data$income <- data$age + 1
  formula <- deaths ~ age * sex + sex * time
  mod <- mod_pois(formula = formula,
                  data = data,
                  exposure = exposure) |>
    set_covariates(~income)
  ans <- make_data_forecast_labels(mod = mod,
                                   labels_forecast = 2006:2008)
  expect_identical(names(ans), names(data))
  expect_true(all(is.na(ans$deaths)))
  expect_true(all(is.na(ans$exposure)))
  expect_true(all(is.na(ans$unused)))
  expect_setequal(ans$age, data$age)
  expect_setequal(ans$sex, data$sex)
  expect_setequal(ans$time, 2006:2008)
  expect_setequal(ans$income, ans$age + 1)
})


## 'make_data_forecast_labels_covariates' -------------------------------------

test_that("'make_data_forecast_labels_covariates' works - numeric covariate", {
  set.seed(0)
  data <- expand.grid(age = 0:9, time = 2000:2005, sex = c("F", "M"))
  data$deaths <- rpois(n = nrow(data), lambda = 100)
  data$exposure <- 100
  data$unused <- 33
  data$income <- 3 * data$age
  data_forecast <- expand.grid(age = 0:9, time = 2006:2007, sex = c("F", "M"),
                               KEEP.OUT.ATTRS = FALSE)
  data_forecast$unused <- NA_real_
  data_forecast$income <- NA_real_
  mod <- mod_pois(deaths ~ age * sex + sex * time,
                  data = data,
                  exposure = exposure) |>
    set_covariates(~income)
  ans_obtained <- make_data_forecast_labels_covariates(mod = mod,
                                                       data_forecast = data_forecast)
  ans_expected <- data_forecast
  ans_expected$income <- ans_expected$age * 3
  ans_expected <- tibble::tibble(ans_expected)
  expect_equal(ans_obtained, ans_expected)
})

test_that("'make_data_forecast_labels_covariates' works with numeric and categorical covariates", {
  set.seed(0)
  data <- expand.grid(age = 0:9, time = 2000:2005, sex = c("F", "M"))
  data$reg <- ifelse(data$age %in% 0:4, "a", "b")
  data$deaths <- rpois(n = nrow(data), lambda = 100)
  data$exposure <- 100
  data$unused <- 33
  data$income <- 3 * data$age
  data_forecast <- expand.grid(age = 0:9, time = 2006:2007, sex = c("F", "M"),
                               KEEP.OUT.ATTRS = FALSE)
  data_forecast$region <- NA_character_
  data_forecast$unused <- NA_real_
  data_forecast$income <- NA_real_
  mod <- mod_pois(deaths ~ age * sex + sex * time,
                  data = data,
                  exposure = exposure) |>
    set_covariates(~income + reg)
  ans_obtained <- make_data_forecast_labels_covariates(mod = mod,
                                                       data_forecast = data_forecast)
  ans_expected <- data_forecast
  ans_expected$income <- ans_expected$age * 3
  ans_expected$reg <- ifelse(ans_expected$age %in% 0:4, "a", "b")
  ans_expected <- tibble::tibble(ans_expected)
  expect_equal(ans_obtained, ans_expected)
})

test_that("'make_data_forecast_labels_covariates' throws appropriate error when cannot infer future values", {
  set.seed(0)
  data <- expand.grid(age = 0:9, time = 2000:2005, sex = c("F", "M"))
  data <- merge(data,
                expand.grid(age = 0:9, sex = c("F", "M"), reg = letters[1:4]),
                by = c("age", "sex"))
  data$deaths <- rpois(n = nrow(data), lambda = 100)
  data$exposure <- 100
  data$unused <- 33
  data$income <- 3 * data$age
  data_forecast <- expand.grid(age = 0:9, time = 2006:2007, sex = c("F", "M"),
                               KEEP.OUT.ATTRS = FALSE)
  data_forecast$region <- NA_character_
  data_forecast$unused <- NA_real_
  data_forecast$income <- NA_real_
  mod <- mod_pois(deaths ~ age * sex + sex * time,
                  data = data,
                  exposure = exposure) |>
    set_covariates(~income + reg)
  expect_error(make_data_forecast_labels_covariates(mod = mod,
                                                    data_forecast = data_forecast),
               "Unable to derive covariate values for forecasted periods.")
})


## 'make_data_forecast_newdata' -----------------------------------------------

test_that("'make_data_forecast_newdata' works", {
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
  newdata <- make_data_forecast_labels(mod = mod,
                                       labels_forecast = 2006:2008)
  ans_obtained <- make_data_forecast_newdata(mod = mod, newdata = newdata)
  expect_identical(ans_obtained, newdata)
})

test_that("'make_data_forecast_newdata' works with covariates", {
  set.seed(0)
  data <- expand.grid(age = 0:9, time = 2000:2005, sex = c("F", "M"))
  data$deaths <- rpois(n = nrow(data), lambda = 100)
  data$exposure <- 100
  data$unused <- 33
  data$income <- data$age  + 1
  formula <- deaths ~ age * sex + sex * time
  mod <- mod_pois(formula = formula,
                  data = data,
                  exposure = exposure)
  mod <- set_n_draw(mod, n = 10)
  mod <- set_covariates(mod, ~ income)
  newdata <- make_data_forecast_labels(mod = mod,
                                       labels_forecast = 2006:2008)
  ans_obtained <- make_data_forecast_newdata(mod = mod, newdata = newdata)
  expect_identical(ans_obtained, newdata)
})


test_that("'make_data_forecast_newdata' raises correct error with variables missing", {
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
  newdata <- make_data_forecast_labels(mod = mod,
                                       labels_forecast = 2006:2008)
  newdata <- newdata[-1]
  expect_error(make_data_forecast_newdata(mod = mod, newdata = newdata),
               "Variable in model but not in `newdata`: \"age\".")
  newdata <- newdata[-1]
  expect_error(make_data_forecast_newdata(mod = mod, newdata = newdata),
               "Variables in model but not in `newdata`: \"age\" and \"time\".")
})

test_that("'make_data_forecast_newdata' raises correct error when periods overlap - time is integer", {
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
  newdata <- make_data_forecast_labels(mod = mod,
                                       labels_forecast = 2005:2008)
  expect_error(make_data_forecast_newdata(mod = mod, newdata = newdata),
               "Times in `newdata` and `data` overlap.")
})

test_that("'make_data_forecast_newdata' raises correct error when periods overlap - time is date", {
  set.seed(0)
  data <- expand.grid(age = 0:9,
                      time = as.Date(paste0(2000:2005,"-01-01")),
                      sex = c("F", "M"))
  data$deaths <- rpois(n = nrow(data), lambda = 100)
  data$exposure <- 100
  data$unused <- 33
  formula <- deaths ~ age * sex + sex * time
  mod <- mod_pois(formula = formula,
                  data = data,
                  exposure = exposure)
  mod <- set_n_draw(mod, n = 10)
  newdata <- make_data_forecast_labels(mod = mod,
                                       labels_forecast = as.Date(paste0(2005:2008, "-01-01")))
  expect_error(make_data_forecast_newdata(mod = mod, newdata = newdata),
               "Times in `newdata` and `data` overlap.")
})



## 'make_dimnames_terms_forecast' ---------------------------------------------

test_that("'make_dimnames_terms_forecast' works - time_only is FALSE", {
  data <- expand.grid(age = 0:9, time = 2000:2005, sex = c("F", "M"))
  data$deaths <- rpois(n = nrow(data), lambda = 100)
  data$exposure <- 100
  formula <- deaths ~ age * sex + sex * time
  mod <- mod_pois(formula = formula,
                  data = data,
                  exposure = exposure)
  labels_forecast <- as.character(2006:2007)
  ans_obtained <- make_dimnames_terms_forecast(dimnames_terms = mod$dimnames_terms,
                                               var_time = mod$var_time,
                                               labels_forecast = labels_forecast,
                                               time_only = FALSE)
  ans_expected <- mod$dimnames_terms
  ans_expected$time$time <- labels_forecast
  ans_expected[["sex:time"]]$time <- labels_forecast
  expect_identical(ans_obtained, ans_expected)
})

test_that("'make_dimnames_terms_forecast' works - time_only is TRUE", {
  data <- expand.grid(age = 0:9, time = 2000:2005, sex = c("F", "M"))
  data$deaths <- rpois(n = nrow(data), lambda = 100)
  data$exposure <- 100
  formula <- deaths ~ age * sex + sex * time
  mod <- mod_pois(formula = formula,
                  data = data,
                  exposure = exposure)
  labels_forecast <- as.character(2006:2007)
  ans_obtained <- make_dimnames_terms_forecast(dimnames_terms = mod$dimnames_terms,
                                               var_time = mod$var_time,
                                               labels_forecast = labels_forecast,
                                               time_only = TRUE)
  ans_expected <- mod$dimnames_terms[c("time", "sex:time")]
  ans_expected$time$time <- labels_forecast
  ans_expected[["sex:time"]]$time <- labels_forecast
  expect_identical(ans_obtained, ans_expected)
})


## 'make_mapping_final_time_effect' -------------------------------------------

test_that("'make_mapping_time_effect' works", {
  data <- expand.grid(age = 0:9, time = 2000:2005, sex = c("F", "M"))
  data$deaths <- rpois(n = nrow(data), lambda = 100)
  data$exposure <- 100
  formula <- deaths ~ age * sex + sex * time
  mod <- mod_pois(formula = formula,
                  data = data,
                  exposure = exposure)
  ans_obtained <- make_mapping_final_time_effect(mod, labels_forecast = 2006:2007)
  ans_expected <- tibble::tibble(level = c(2006:2007, "F.2006", "F.2007", "M.2006", "M.2007"),
                                 level_final = rep(c("2005", "F.2005", "M.2005"), each = 2))
  expect_identical(ans_obtained, ans_expected)
})


## 'make_mapping_final_time_svd' ----------------------------------------------

test_that("'make_mapping_final_time_svd' works", {
  data <- expand.grid(age = poputils::age_labels(type = "lt", max = 60),
                      time = 2000:2005,
                      sex = c("F", "M"))
  data$deaths <- rpois(n = nrow(data), lambda = 100)
  data$exposure <- 100
  formula <- deaths ~ age * sex + age * time
  mod <- mod_pois(formula = formula,
                  data = data,
                  exposure = exposure) |>
                  set_prior(age:sex ~ SVD(HMD)) |>
                  set_prior(age:time ~ SVD_RW(HMD))
  ans_obtained <- make_mapping_final_time_svd(mod, labels_forecast = 2006:2007)
  ans_expected <- tibble::tibble(level = paste(rep(paste0("comp", 1:3), each = 2),
                                               2006:2007,
                                               sep = "."),
                                 level_final = rep(paste0("comp", 1:3, ".2005"), each = 2))
  expect_identical(ans_obtained, ans_expected)
})
                  

## 'make_term_level_final_time_effect' ----------------------------------------

test_that("'make_term_level_final_time' works", {
  data <- expand.grid(age = 0:9, time = 2000:2005, sex = c("F", "M"))
  data$deaths <- rpois(n = nrow(data), lambda = 100)
  data$exposure <- 100
  formula <- deaths ~ age * sex + sex * time
  mod <- mod_pois(formula = formula,
                  data = data,
                  exposure = exposure)
  ans_obtained <- make_term_level_final_time_effect(mod)
  ans_expected <- tibble::tibble(term = c("time", "sex:time", "sex:time"),
                                 level = c("2005", "F.2005", "M.2005"))
  expect_identical(ans_obtained, ans_expected)
})


## 'make_term_level_final_time_svd' -------------------------------------------

test_that("'make_term_level_final_time_svd' works", {
  data <- expand.grid(age = poputils::age_labels(type = "lt", max = 60),
                      time = 2000:2005,
                      sex = c("F", "M"))
  data$deaths <- rpois(n = nrow(data), lambda = 100)
  data$exposure <- 100
  formula <- deaths ~ age * sex + age * time
  mod <- mod_pois(formula = formula,
                  data = data,
                  exposure = exposure) |>
                  set_prior(age:sex ~ SVD(HMD)) |>
                  set_prior(age:time ~ SVD_RW(HMD))
  ans_obtained <- make_term_level_final_time_svd(mod)
  ans_expected <- tibble::tibble(term = "age:time",
                                 level = paste0(paste0("comp", 1:3),  ".2005"))
  expect_identical(ans_obtained, ans_expected)
})



