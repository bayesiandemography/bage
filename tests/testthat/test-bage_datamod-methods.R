
## 'datamod_descr" ------------------------------------------------------------

test_that("'datamod_descr' returns expected string", {
  x <- 1
  class(x) <- "bage_datamod_exposure"
  expect_identical(datamod_descr(x), "exposure")
  class(x) <- "bage_datamod_miscount"
  expect_identical(datamod_descr(x), "miscount")
  class(x) <- "bage_datamod_noise"
  expect_identical(datamod_descr(x), "noise")
  class(x) <- "bage_datamod_overcount"
  expect_identical(datamod_descr(x), "overcount")
  class(x) <- "bage_datamod_undercount"
  expect_identical(datamod_descr(x), "undercount")
})


## 'draw_datamod_param' -------------------------------------------------------

test_that("'draw_datamod_param' works with bage_datamod_miscount", {
  set.seed(0)
  prob_mean <- c(0.5, 0.2, 0.3, 0.4)
  prob_disp <- rep(0.4, 4)
  prob_levels <- 1:4
  prob_matrix_outcome <- Matrix::Matrix(kronecker(rep(1, 3), diag(4)))
  rate_mean <- c(0.3, 0.3, 0.2)
  rate_disp <- c(0.5, 0.2, 0.6)
  rate_levels <- 1:3
  rate_matrix_outcome <- Matrix::Matrix(kronecker(diag(3), rep(1, 4)))
  datamod <- new_bage_datamod_miscount(prob_mean = prob_mean,
                                       prob_disp = prob_disp,
                                       prob_levels = prob_levels,
                                       prob_matrix_outcome = prob_matrix_outcome,
                                       rate_mean = rate_mean,
                                       rate_disp = rate_disp,
                                       rate_levels = rate_levels,
                                       rate_matrix_outcome = rate_matrix_outcome,
                                       nms_by = c("sex", "age"))
  ans <- draw_datamod_param(datamod, n_sim = 10000)
  expect_equal(rowMeans(ans), c(prob_mean, rate_mean), tolerance = 0.01)
})

test_that("'draw_datamod_param' works with bage_datamod_overcount", {
  set.seed(0)
  rate_mean <- c(0.3, 0.3, 0.2)
  rate_disp <- c(0.5, 0.2, 0.6)
  rate_levels <- 1:3
  rate_matrix_outcome <- Matrix::Matrix(kronecker(diag(3), rep(1, 4)))
  datamod <- new_bage_datamod_overcount(rate_mean = rate_mean,
                                        rate_disp = rate_disp,
                                        rate_levels = rate_levels,
                                        rate_matrix_outcome = rate_matrix_outcome,
                                        nms_by = c("sex", "age"))
  ans <- draw_datamod_param(datamod, n_sim = 10000)
  expect_equal(rowMeans(ans), rate_mean, tolerance = 0.01)
})

test_that("'draw_datamod_param' works with bage_datamod_undercount", {
  set.seed(0)
  prob_mean <- c(0.5, 0.2, 0.3, 0.4)
  prob_disp <- rep(0.4, 4)
  prob_levels <- 1:4
  prob_matrix_outcome <- Matrix::Matrix(kronecker(rep(1, 3), diag(4)))
  datamod <- new_bage_datamod_undercount(prob_mean = prob_mean,
                                       prob_disp = prob_disp,
                                       prob_levels = prob_levels,
                                       prob_matrix_outcome = prob_matrix_outcome,
                                       nms_by = c("sex", "age"))
  ans <- draw_datamod_param(datamod, n_sim = 10000)
  expect_equal(rowMeans(ans), prob_mean, tolerance = 0.01)
})


## 'draw_offset_obs_given_true' -----------------------------------------------

test_that("'draw_offset_obs_given_true' works with bage_datamod_exposure", {
  set.seed(0)
  disp <- c(0.3, 0.2, 0.3, 0.2)
  disp_levels <- 1:4
  disp_matrix_outcome <- Matrix::Matrix(kronecker(rep(1, 3), diag(4)))
  datamod <- new_bage_datamod_exposure(disp = disp,
                                       disp_levels = disp_levels,
                                       disp_matrix_outcome = disp_matrix_outcome,
                                       nms_by = c("sex", "age"))
  components <- tibble::tibble(
    term = c("(Intercept)", rep("datamod", 4)),
    component = c("(Intercept)", rep("disp", 4)),
    level = c("(Intercept)", 0:3),
    .fitted = rvec::runif_rvec(n = 5, n_draw = 10)
  )
  offset <- runif(12, max = 10)
  offset[1] <- NA
  set.seed(1)
  ans_obtained <- draw_offset_obs_given_true(datamod = datamod,
                                             components = components,
                                             offset_true = offset)
  set.seed(1)
  disp <- get_datamod_disp(datamod)
  ans_inv <- vctrs::vec_c(NA,
                          rvec::rgamma_rvec(n = 11,
                                            2 + (1/disp[-1]),
                                            (1 + (1/disp[-1]))  * offset[-1],
                                            n_draw = 10))
  ans_expected <- 1 / ans_inv
  expect_equal(ans_obtained, ans_expected)
})


## 'draw_offset_true_given_obs' -----------------------------------------------

test_that("'draw_offset_true_given_obs' works with bage_datamod_exposure, outcome_obs is numeric, has NA", {
  set.seed(0)
  disp <- c(0.3, 0.2, 0.3, 0.2)
  disp_levels <- 1:4
  disp_matrix_outcome <- Matrix::Matrix(kronecker(rep(1, 3), diag(4)))
  datamod <- new_bage_datamod_exposure(disp = disp,
                                       disp_levels = disp_levels,
                                       disp_matrix_outcome = disp_matrix_outcome,
                                       nms_by = c("sex", "age"))
  components <- tibble::tibble(
    term = c("(Intercept)", rep("datamod", 4)),
    component = c("(Intercept)", rep("disp", 4)),
    level = c("(Intercept)", 0:3),
    .fitted = rvec::runif_rvec(n = 5, n_draw = 10)
  )
  outcome <- rpois(n = 12, lambda = 5)
  offset_obs <- runif(12, max = 10)
  offset_obs[1] <- NA
  expected <- rvec::runif_rvec(n = 12, n_draw = 10)
  set.seed(1)
  ans_obtained <- draw_offset_true_given_obs(datamod = datamod,
                                             nm_distn = "pois",
                                             components = components,
                                             outcome = outcome,
                                             offset_obs = offset_obs,
                                             expected = expected)
  set.seed(1)
  disp <- get_datamod_disp(datamod)
  ans_expected <- vctrs::vec_c(NA,
                               rvec::rgamma_rvec(n = 11,
                                                 3 + (1/disp[-1]) + outcome[-1],
                                                 (1 + (1/disp[-1])) / offset_obs[-1] + expected[-1]))
  expect_equal(ans_obtained, ans_expected)
})

test_that("'draw_offset_true_given_obs' throws expectd error with non-Poisson", {
  set.seed(0)
  disp <- c(0.3, 0.2, 0.3, 0.2)
  disp_levels <- 1:4
  disp_matrix_outcome <- Matrix::Matrix(kronecker(rep(1, 3), diag(4)))
  datamod <- new_bage_datamod_exposure(disp = disp,
                                       disp_levels = disp_levels,
                                       disp_matrix_outcome = disp_matrix_outcome,
                                       nms_by = c("sex", "age"))
  components <- tibble::tibble(
    term = c("(Intercept)", rep("datamod", 4)),
    component = c("(Intercept)", rep("disp", 4)),
    level = c("(Intercept)", 0:3),
    .fitted = rvec::runif_rvec(n = 5, n_draw = 10)
  )
  outcome <- rpois(n = 12, lambda = 5)
  offset_obs <- runif(12, max = 10)
  offset_obs[1] <- NA
  expected <- rvec::runif_rvec(n = 12, n_draw = 10)
  set.seed(1)
  expect_error(draw_offset_true_given_obs(datamod = datamod,
                                          nm_distn = "binom",
                                          components = components,
                                          outcome = outcome,
                                          offset_obs = offset_obs,
                                          expected = expected),
               "Internal error: `datamod` has class")
})


## 'draw_outcome_obs_given_true' ----------------------------------------------

test_that("'draw_outcome_obs_given_true' works with bage_datamod_miscount - outcome_true is numeric", {
  set.seed(0)
  prob_mean <- c(0.5, 0.2, 0.3, 0.4)
  prob_disp <- rep(0.4, 4)
  prob_levels <- 1:4
  prob_matrix_outcome <- Matrix::Matrix(kronecker(rep(1, 3), diag(4)))
  rate_mean <- c(0.3, 0.3, 0.2)
  rate_disp <- c(0.5, 0.2, 0.6)
  rate_levels <- 1:3
  rate_matrix_outcome <- Matrix::Matrix(kronecker(diag(3), rep(1, 4)))
  datamod <- new_bage_datamod_miscount(prob_mean = prob_mean,
                                       prob_disp = prob_disp,
                                       prob_levels = prob_levels,
                                       prob_matrix_outcome = prob_matrix_outcome,
                                       rate_mean = rate_mean,
                                       rate_disp = rate_disp,
                                       rate_levels = rate_levels,
                                       rate_matrix_outcome = rate_matrix_outcome,
                                       nms_by = c("sex", "age"))
  components <- tibble::tibble(
    term = c("(Intercept)", rep("datamod", 7)),
    component = c("(Intercept)", rep(c("prob", "rate"), times = c(4, 3))),
    level = c("(Intercept)", 0:3, 1:3),
    .fitted = rvec::runif_rvec(n = 8, n_draw = 10)
  )
  outcome_true <- rpois(12, lambda = 5)
  offset <- runif(12, max = 10)
  offset[1] <- NA
  fitted <- rvec::rgamma_rvec(12, shape = 1, rate = 0.2, n_draw = 10)
  set.seed(1)
  ans_obtained <- draw_outcome_obs_given_true(datamod = datamod,
                                              components = components,
                                              outcome_true = outcome_true,
                                              offset = offset,
                                              fitted = fitted)
  set.seed(1)
  pi <- rep(components$.fitted[2:5], times = 3)
  alpha <- rep(components$.fitted[6:8], each = 4)
  u <- rvec::rbinom_rvec(n = 11, size = outcome_true[-1], prob = pi[-1])
  v <- rvec::rpois_rvec(n = 11, lambda = alpha[-1] * fitted[-1] * offset[-1])
  ans_expected <- vctrs::vec_c(NA, u + v)
  expect_equal(ans_obtained, ans_expected)
})

test_that("'draw_outcome_obs_given_true' works with bage_datamod_miscount - outcome_true is rvec", {
  set.seed(0)
  prob_mean <- c(0.5, 0.2, 0.3, 0.4)
  prob_disp <- rep(0.4, 4)
  prob_levels <- 1:4
  prob_matrix_outcome <- Matrix::Matrix(kronecker(rep(1, 3), diag(4)))
  rate_mean <- c(0.3, 0.3, 0.2)
  rate_disp <- c(0.5, 0.2, 0.6)
  rate_levels <- 1:3
  rate_matrix_outcome <- Matrix::Matrix(kronecker(diag(3), rep(1, 4)))
  datamod <- new_bage_datamod_miscount(prob_mean = prob_mean,
                                       prob_disp = prob_disp,
                                       prob_levels = prob_levels,
                                       prob_matrix_outcome = prob_matrix_outcome,
                                       rate_mean = rate_mean,
                                       rate_disp = rate_disp,
                                       rate_levels = rate_levels,
                                       rate_matrix_outcome = rate_matrix_outcome,
                                       nms_by = c("sex", "age"))
  components <- tibble::tibble(
    term = c("(Intercept)", rep("datamod", 7)),
    component = c("(Intercept)", rep(c("prob", "rate"), times = c(4, 3))),
    level = c("(Intercept)", 0:3, 1:3),
    .fitted = rvec::runif_rvec(n = 8, n_draw = 10)
  )
  outcome_true <- rvec::rpois_rvec(12, lambda = 5, n_draw = 10)
  offset <- runif(12, max = 10)
  offset[1] <- NA
  fitted <- rvec::rgamma_rvec(12, shape = 1, rate = 0.2, n_draw = 10)
  set.seed(1)
  ans_obtained <- draw_outcome_obs_given_true(datamod = datamod,
                                              components = components,
                                              outcome_true = outcome_true,
                                              offset = offset,
                                              fitted = fitted)
  set.seed(1)
  pi <- rep(components$.fitted[2:5], times = 3)
  alpha <- rep(components$.fitted[6:8], each = 4)
  u <- rvec::rbinom_rvec(n = 11, size = outcome_true[-1], prob = pi[-1])
  v <- rvec::rpois_rvec(n = 11, lambda = alpha[-1] * fitted[-1] * offset[-1])
  ans_expected <- vctrs::vec_c(NA, u + v)
  expect_equal(ans_obtained, ans_expected)
})

test_that("'draw_outcome_obs_given_true' works with bage_datamod_noise, outcome_true is numeric, model is normal", {
  set.seed(0)
  sd_sd <- c(0.3, 0.3, 0.2)
  sd_levels <- 1:3
  sd_matrix_outcome <- Matrix::Matrix(kronecker(diag(3), rep(1, 4)))
  datamod <- new_bage_datamod_noise(sd = sd_sd,
                                    sd_levels = sd_levels,
                                    sd_matrix_outcome = sd_matrix_outcome,
                                    nms_by = c("sex", "age"),
                                    outcome_sd = 2)
  outcome <- rnorm(12)
  outcome[12] <- NA
  offset <- runif(12, min = 1, max = 10)
  fitted <- rvec::rnorm_rvec(12, n_draw = 10)
  set.seed(1)
  ans_obtained <- draw_outcome_obs_given_true(datamod = datamod,
                                              components = NULL,
                                              outcome_true = outcome,
                                              offset = offset,
                                              fitted = fitted)
  set.seed(1)
  noise <- rvec::rnorm_rvec(n = 11,
                            mean = 0,
                            sd = get_datamod_sd(datamod)[-12],
                            n_draw = 10)
  ans_expected <- vctrs::vec_c(outcome[-12] + noise, NA)
  expect_equal(ans_obtained, ans_expected)
})

test_that("'draw_outcome_obs_given_true' works with bage_datamod_noise, outcome_true is rvec, model is normal", {
  set.seed(0)
  sd_sd <- c(0.3, 0.3, 0.2)
  sd_levels <- 1:3
  sd_matrix_outcome <- Matrix::Matrix(kronecker(diag(3), rep(1, 4)))
  datamod <- new_bage_datamod_noise(sd = sd_sd,
                                    sd_levels = sd_levels,
                                    sd_matrix_outcome = sd_matrix_outcome,
                                    nms_by = c("sex", "age"),
                                    outcome_sd = 2)
  outcome <- rvec::rnorm_rvec(n = 12, n_draw = 10)
  outcome[12] <- NA
  offset <- runif(12, min = 1, max = 10)
  fitted <- rvec::rnorm_rvec(12, n_draw = 10)
  set.seed(1)
  ans_obtained <- draw_outcome_obs_given_true(datamod = datamod,
                                              components = NULL,
                                              outcome_true = outcome,
                                              offset = offset,
                                              fitted = fitted)
  set.seed(1)
  noise <- rvec::rnorm_rvec(n = 11,
                            mean = 0,
                            sd = get_datamod_sd(datamod)[-12],
                            n_draw = 10)
  ans_expected <- vctrs::vec_c(outcome[-12] + noise, NA)
  expect_equal(ans_obtained, ans_expected)
})

test_that("'draw_outcome_obs_given_true' works with bage_datamod_noise, outcome_true is numeric, model is Poisson", {
  set.seed(0)
  sd_sd <- c(0.3, 0.3, 0.2)
  sd_levels <- 1:3
  sd_matrix_outcome <- Matrix::Matrix(kronecker(diag(3), rep(1, 4)))
  datamod <- new_bage_datamod_noise(sd = sd_sd,
                                    sd_levels = sd_levels,
                                    sd_matrix_outcome = sd_matrix_outcome,
                                    nms_by = c("sex", "age"),
                                    outcome_sd = NULL)
  outcome <- rpois(12, lambda = 12)
  outcome[12] <- NA
  offset <- runif(12, min = 1, max = 10)
  fitted <- rvec::rgamma_rvec(12, shape = 1, scale = 1, n_draw = 10)
  set.seed(1)
  ans_obtained <- draw_outcome_obs_given_true(datamod = datamod,
                                              components = NULL,
                                              outcome_true = outcome,
                                              offset = offset,
                                              fitted = fitted)
  set.seed(1)
  noise <- rvec::rpois_rvec(n = 11,
                            lambda = 0.5 * (get_datamod_sd(datamod)^2)[-12],
                            n_draw = 10) -
    rvec::rpois_rvec(n = 11,
                     lambda = 0.5 * (get_datamod_sd(datamod)^2)[-12],
                     n_draw = 10)
  ans_expected <- vctrs::vec_c(outcome[-12] + noise, NA)
  expect_equal(ans_obtained, ans_expected)
})

test_that("'draw_outcome_obs_given_true' works with bage_datamod_noise, outcome_true is rvec, model is Poisson", {
  set.seed(0)
  sd_sd <- c(0.3, 0.3, 0.2)
  sd_levels <- 1:3
  sd_matrix_outcome <- Matrix::Matrix(kronecker(diag(3), rep(1, 4)))
  datamod <- new_bage_datamod_noise(sd = sd_sd,
                                    sd_levels = sd_levels,
                                    sd_matrix_outcome = sd_matrix_outcome,
                                    nms_by = c("sex", "age"),
                                    outcome_sd = NULL)
  outcome <- rvec::rpois_rvec(n = 12, lambda = 20, n_draw = 10)
  outcome[12] <- NA
  offset <- runif(12, min = 1, max = 10)
  fitted <- rvec::rgamma_rvec(12, shape = 1, scale = 1, n_draw = 10)
  set.seed(1)
  ans_obtained <- draw_outcome_obs_given_true(datamod = datamod,
                                              components = NULL,
                                              outcome_true = outcome,
                                              offset = offset,
                                              fitted = fitted)
  set.seed(1)
  noise <- rvec::rpois_rvec(n = 11,
                            lambda = 0.5 * (get_datamod_sd(datamod)[-12])^2,
                            n_draw = 10) -
    rvec::rpois_rvec(n = 11,
                     lambda = 0.5 * (get_datamod_sd(datamod)[-12])^2,
                     n_draw = 10)
  ans_expected <- vctrs::vec_c(outcome[-12] + noise, NA)
  expect_equal(ans_obtained, ans_expected)
})


test_that("'draw_outcome_obs_given_true' works with bage_datamod_overcount - oucome_obs is numeric", {
  set.seed(0)
  rate_mean <- c(0.3, 0.3, 0.2)
  rate_disp <- c(0.5, 0.2, 0.6)
  rate_levels <- 1:3
  rate_matrix_outcome <- Matrix::Matrix(kronecker(diag(3), rep(1, 4)))
  datamod <- new_bage_datamod_overcount(rate_mean = rate_mean,
                                        rate_disp = rate_disp,
                                        rate_levels = rate_levels,
                                        rate_matrix_outcome = rate_matrix_outcome,
                                        nms_by = c("sex", "age"))
  components <- tibble::tibble(
    term = c("(Intercept)", rep("datamod", 3)),
    component = c("(Intercept)", rep("rate", times = 3)),
    level = c("(Intercept)", 1:3),
    .fitted = rvec::runif_rvec(n = 4, n_draw = 10)
  )
  outcome_true <- rpois(12, lambda = 5)
  offset <- runif(12, max = 10)
  offset[1] <- NA
  fitted <- rvec::rgamma_rvec(12, shape = 1, rate = 0.2, n_draw = 10)
  set.seed(1)
  ans_obtained <- draw_outcome_obs_given_true(datamod = datamod,
                                              components = components,
                                              outcome_true = outcome_true,
                                              offset = offset,
                                              fitted = fitted)
  set.seed(1)
  alpha <- rep(components$.fitted[2:4], each = 4)
  v <- rvec::rpois_rvec(n = 11,
                        lambda = alpha[-1] * fitted[-1] * offset[-1])
  ans_expected <- vctrs::vec_c(NA, outcome_true[-1] + v)
  expect_equal(ans_obtained, ans_expected)
})

test_that("'draw_outcome_obs_given_true' works with bage_datamod_overcount - outcome_true is rvec", {
  set.seed(0)
  rate_mean <- c(0.3, 0.3, 0.2)
  rate_disp <- c(0.5, 0.2, 0.6)
  rate_levels <- 1:3
  rate_matrix_outcome <- Matrix::Matrix(kronecker(diag(3), rep(1, 4)))
  datamod <- new_bage_datamod_overcount(rate_mean = rate_mean,
                                        rate_disp = rate_disp,
                                        rate_levels = rate_levels,
                                        rate_matrix_outcome = rate_matrix_outcome,
                                        nms_by = c("sex", "age"))
  components <- tibble::tibble(
    term = c("(Intercept)", rep("datamod", 3)),
    component = c("(Intercept)", rep("rate", times = 3)),
    level = c("(Intercept)", 1:3),
    .fitted = rvec::runif_rvec(n = 4, n_draw = 10)
  )
  outcome_true <- rvec::rpois_rvec(12, lambda = 5, n_draw = 10)
  offset <- runif(12, max = 10)
  offset[1] <- NA
  fitted <- rvec::rgamma_rvec(12, shape = 1, rate = 0.2, n_draw = 10)
  set.seed(1)
  ans_obtained <- draw_outcome_obs_given_true(datamod = datamod,
                                              components = components,
                                              outcome_true = outcome_true,
                                              offset = offset,
                                              fitted = fitted)
  set.seed(1)
  alpha <- rep(components$.fitted[2:4], each = 4)
  v <- rvec::rpois_rvec(n = 11,
                        lambda = alpha[-1] * fitted[-1] * offset[-1])
  ans_expected <- vctrs::vec_c(NA, outcome_true[-1] + v)
  expect_equal(ans_obtained, ans_expected)
})

test_that("'draw_outcome_obs_given_true' works with bage_datamod_undercount - outcome_true is numeric", {
  set.seed(0)
  prob_mean <- c(0.5, 0.2, 0.3, 0.4)
  prob_disp <- rep(0.4, 4)
  prob_levels <- 1:4
  prob_matrix_outcome <- Matrix::Matrix(kronecker(rep(1, 3), diag(4)))
  datamod <- new_bage_datamod_undercount(prob_mean = prob_mean,
                                         prob_disp = prob_disp,
                                         prob_levels = prob_levels,
                                         prob_matrix_outcome = prob_matrix_outcome,
                                         nms_by = c("sex", "age"))
  components <- tibble::tibble(
    term = c("(Intercept)", rep("datamod", 4)),
    component = c("(Intercept)", rep("prob", times = 4)),
    level = c("(Intercept)", 0:3),
    .fitted = rvec::runif_rvec(n = 5, n_draw = 10)
  )
  outcome_true <- rpois(12, lambda = 5)
  offset <- runif(12, max = 10)
  offset[1] <- NA
  fitted <- rvec::rgamma_rvec(12, shape = 1, rate = 0.2, n_draw = 10)
  set.seed(1)
  ans_obtained <- draw_outcome_obs_given_true(datamod = datamod,
                                              components = components,
                                              outcome_true = outcome_true,
                                              offset = offset,
                                              fitted = fitted)
  set.seed(1)
  pi <- rep(components$.fitted[2:5], times = 3)
  u <- rvec::rbinom_rvec(n = 11, size = outcome_true[-1], prob = pi[-1])
  ans_expected <- vctrs::vec_c(NA, u)
  expect_equal(ans_obtained, ans_expected)
})

test_that("'draw_outcome_obs_given_true' works with bage_datamod_undercount - outcome_true is rvec", {
  set.seed(0)
  prob_mean <- c(0.5, 0.2, 0.3, 0.4)
  prob_disp <- rep(0.4, 4)
  prob_levels <- 1:4
  prob_matrix_outcome <- Matrix::Matrix(kronecker(rep(1, 3), diag(4)))
  datamod <- new_bage_datamod_undercount(prob_mean = prob_mean,
                                         prob_disp = prob_disp,
                                         prob_levels = prob_levels,
                                         prob_matrix_outcome = prob_matrix_outcome,
                                         nms_by = c("sex", "age"))
  components <- tibble::tibble(
    term = c("(Intercept)", rep("datamod", 4)),
    component = c("(Intercept)", rep("prob", times = 4)),
    level = c("(Intercept)", 0:3),
    .fitted = rvec::runif_rvec(n = 5, n_draw = 10)
  )
  outcome_true <- rvec::rpois_rvec(12, lambda = 5, n_draw = 10)
  offset <- runif(12, max = 10)
  offset[1] <- NA
  fitted <- rvec::rgamma_rvec(12, shape = 1, rate = 0.2, n_draw = 10)
  set.seed(1)
  ans_obtained <- draw_outcome_obs_given_true(datamod = datamod,
                                              components = components,
                                              outcome_true = outcome_true,
                                              offset = offset,
                                              fitted = fitted)
  set.seed(1)
  pi <- rep(components$.fitted[2:5], times = 3)
  u <- rvec::rbinom_rvec(n = 11, size = outcome_true[-1], prob = pi[-1])
  ans_expected <- vctrs::vec_c(NA, u)
  expect_equal(ans_obtained, ans_expected)
})


## 'draw_outcome_true_given_obs' -----------------------------------------------

test_that("'draw_outcome_true_given_obs' works with bage_datamod_exposure", {
  set.seed(0)
  disp <- c(0.3, 0.2, 0.3, 0.2)
  disp_levels <- 1:4
  disp_matrix_outcome <- Matrix::Matrix(kronecker(rep(1, 3), diag(4)))
  datamod <- new_bage_datamod_exposure(disp = disp,
                                       disp_levels = disp_levels,
                                       disp_matrix_outcome = disp_matrix_outcome,
                                       nms_by = c("sex", "age"))
  components <- tibble::tibble(
    term = c("(Intercept)", rep("datamod", 4)),
    component = c("(Intercept)", rep("disp", 4)),
    level = c("(Intercept)", 0:3),
    .fitted = rvec::runif_rvec(n = 5, n_draw = 10)
  )
  outcome <- rpois(12, lambda = 5)
  offset <- runif(12, max = 10)
  expected <- rvec::rgamma_rvec(12, shape = 1, rate = 0.2, n_draw = 10)
  disp <- rvec::runif_rvec(1)
  set.seed(1)
  ans_obtained <- draw_outcome_true_given_obs(datamod = datamod,
                                              nm_distn = "pois",
                                              components = components,
                                              outcome = outcome,
                                              offset = offset,
                                              expected = expected,
                                              disp = disp)
  set.seed(1)
  ans_expected <- outcome
  expect_equal(ans_obtained, ans_expected)
})

test_that("'draw_outcome_true_given_obs' throws expected error with wrong distribution and bage_datamod_exposure", {
  set.seed(0)
  disp <- c(0.3, 0.2, 0.3, 0.2)
  disp_levels <- 1:4
  disp_matrix_outcome <- Matrix::Matrix(kronecker(rep(1, 3), diag(4)))
  datamod <- new_bage_datamod_exposure(disp = disp,
                                       disp_levels = disp_levels,
                                       disp_matrix_outcome = disp_matrix_outcome,
                                       nms_by = c("sex", "age"))
  components <- tibble::tibble(
    term = c("(Intercept)", rep("datamod", 4)),
    component = c("(Intercept)", rep("disp", 4)),
    level = c("(Intercept)", 0:3),
    .fitted = rvec::runif_rvec(n = 5, n_draw = 10)
  )
  outcome <- rpois(12, lambda = 5)
  offset <- runif(12, max = 10)
  expected <- rvec::rgamma_rvec(12, shape = 1, rate = 0.2, n_draw = 10)
  disp <- rvec::runif_rvec(1)
  expect_error(draw_outcome_true_given_obs(datamod = datamod,
                                           nm_distn = "binom",
                                           components = components,
                                           outcome = outcome,
                                           offset = offset,
                                           expected = expected,
                                           disp = disp),
               "Internal error: `datamod` has class")
})

test_that("'draw_outcome_true_given_obs' works with bage_datamod_miscount - has disp, no na", {
  set.seed(0)
  prob_mean <- c(0.5, 0.2, 0.3, 0.4)
  prob_disp <- rep(0.4, 4)
  prob_levels <- 1:4
  prob_matrix_outcome <- Matrix::Matrix(kronecker(rep(1, 3), diag(4)))
  rate_mean <- c(0.3, 0.3, 0.2)
  rate_disp <- c(0.5, 0.2, 0.6)
  rate_levels <- 1:3
  rate_matrix_outcome <- Matrix::Matrix(kronecker(diag(3), rep(1, 4)))
  datamod <- new_bage_datamod_miscount(prob_mean = prob_mean,
                                       prob_disp = prob_disp,
                                       prob_levels = prob_levels,
                                       prob_matrix_outcome = prob_matrix_outcome,
                                       rate_mean = rate_mean,
                                       rate_disp = rate_disp,
                                       rate_levels = rate_levels,
                                       rate_matrix_outcome = rate_matrix_outcome,
                                       nms_by = c("sex", "age"))
  components <- tibble::tibble(
    term = c("(Intercept)", rep("datamod", 7)),
    component = c("(Intercept)", rep(c("prob", "rate"), times = c(4, 3))),
    level = c("(Intercept)", 0:3, 1:3),
    .fitted = rvec::runif_rvec(n = 8, n_draw = 10)
  )
  outcome <- rpois(12, lambda = 5)
  offset <- runif(12, max = 10)
  expected <- rvec::rgamma_rvec(12, shape = 1, rate = 0.2, n_draw = 10)
  disp <- rvec::runif_rvec(1, n_draw = 10)
  set.seed(1)
  ans_obtained <- draw_outcome_true_given_obs(datamod = datamod,
                                              nm_distn = "pois",
                                              components = components,
                                              outcome = outcome,
                                              offset = offset,
                                              expected = expected,
                                              disp = disp)
  set.seed(1)
  pi <- rep(components$.fitted[2:5], times = 3)
  alpha <- rep(components$.fitted[6:8], each = 4)
  u <- rvec::rbinom_rvec(n = 12, size = outcome, prob = pi / (pi + alpha))
  w <- rvec::rnbinom_rvec(n = 12,
                          size = u + (1/disp),
                          prob = (1 + pi * expected * offset * disp) / (1 + expected * offset * disp))
  ans_expected <- u + w
  expect_equal(ans_obtained, ans_expected)
})

test_that("'draw_outcome_true_given_obs' works with bage_datamod_miscount - has disp, has na", {
  set.seed(0)
  prob_mean <- c(0.5, 0.2, 0.3, 0.4)
  prob_disp <- rep(0.4, 4)
  prob_levels <- 1:4
  prob_matrix_outcome <- Matrix::Matrix(kronecker(rep(1, 3), diag(4)))
  rate_mean <- c(0.3, 0.3, 0.2)
  rate_disp <- c(0.5, 0.2, 0.6)
  rate_levels <- 1:3
  rate_matrix_outcome <- Matrix::Matrix(kronecker(diag(3), rep(1, 4)))
  datamod <- new_bage_datamod_miscount(prob_mean = prob_mean,
                                       prob_disp = prob_disp,
                                       prob_levels = prob_levels,
                                       prob_matrix_outcome = prob_matrix_outcome,
                                       rate_mean = rate_mean,
                                       rate_disp = rate_disp,
                                       rate_levels = rate_levels,
                                       rate_matrix_outcome = rate_matrix_outcome,
                                       nms_by = c("sex", "age"))
  components <- tibble::tibble(
    term = c("(Intercept)", rep("datamod", 7)),
    component = c("(Intercept)", rep(c("prob", "rate"), times = c(4, 3))),
    level = c("(Intercept)", 0:3, 1:3),
    .fitted = rvec::runif_rvec(n = 8, n_draw = 10)
  )
  outcome <- rpois(12, lambda = 5)
  offset <- runif(12, max = 10)
  outcome[1] <- NA
  offset[2] <- NA
  expected <- rvec::rgamma_rvec(12, shape = 1, rate = 0.2, n_draw = 10)
  disp <- rvec::runif_rvec(1, n_draw = 10)
  set.seed(1)
  ans_obtained <- draw_outcome_true_given_obs(datamod = datamod,
                                              nm_distn = "pois",
                                              components = components,
                                              outcome = outcome,
                                              offset = offset,
                                              expected = expected,
                                              disp = disp)
  set.seed(1)
  pi <- rep(components$.fitted[2:5], times = 3)
  alpha <- rep(components$.fitted[6:8], each = 4)
  u <- rvec::rbinom_rvec(n = 10, size = outcome[3:12], prob = pi[3:12] / (pi[3:12] + alpha[3:12]))
  w <- rvec::rnbinom_rvec(n = 10,
                          size = u + (1/disp),
                          prob = (1 + pi[3:12] * expected[3:12] * offset[3:12] * disp) /
                            (1 + expected[3:12] * offset[3:12] * disp))
  ans_expected <- vctrs::vec_c(NA, NA, u + w)
  expect_equal(ans_obtained, ans_expected)
})

test_that("'draw_outcome_true_given_obs' works with bage_datamod_miscount - no disp, no na", {
  set.seed(0)
  prob_mean <- c(0.5, 0.2, 0.3, 0.4)
  prob_disp <- rep(0.4, 4)
  prob_levels <- 1:4
  prob_matrix_outcome <- Matrix::Matrix(kronecker(rep(1, 3), diag(4)))
  rate_mean <- c(0.3, 0.3, 0.2)
  rate_disp <- c(0.5, 0.2, 0.6)
  rate_levels <- 1:3
  rate_matrix_outcome <- Matrix::Matrix(kronecker(diag(3), rep(1, 4)))
  datamod <- new_bage_datamod_miscount(prob_mean = prob_mean,
                                       prob_disp = prob_disp,
                                       prob_levels = prob_levels,
                                       prob_matrix_outcome = prob_matrix_outcome,
                                       rate_mean = rate_mean,
                                       rate_disp = rate_disp,
                                       rate_levels = rate_levels,
                                       rate_matrix_outcome = rate_matrix_outcome,
                                       nms_by = c("sex", "age"))
  components <- tibble::tibble(
    term = c("(Intercept)", rep("datamod", 7)),
    component = c("(Intercept)", rep(c("prob", "rate"), times = c(4, 3))),
    level = c("(Intercept)", 0:3, 1:3),
    .fitted = rvec::runif_rvec(n = 8, n_draw = 10)
  )
  outcome <- rpois(12, lambda = 5)
  offset <- runif(12, max = 10)
  expected <- rvec::rgamma_rvec(12, shape = 1, rate = 0.2, n_draw = 10)
  set.seed(1)
  ans_obtained <- draw_outcome_true_given_obs(datamod = datamod,
                                              nm_distn = "pois",
                                              components = components,
                                              outcome = outcome,
                                              offset = offset,
                                              expected = expected,
                                              disp = NULL)
  set.seed(1)
  pi <- rep(components$.fitted[2:5], times = 3)
  alpha <- rep(components$.fitted[6:8], each = 4)
  u <- rvec::rbinom_rvec(n = 12, size = outcome, prob = pi / (pi + alpha))
  w <- rvec::rpois_rvec(n = 12, lambda = (1 - pi) * expected * offset)
  ans_expected <- u + w
  expect_equal(ans_obtained, ans_expected)
})

test_that("'draw_outcome_true_given_obs' works with bage_datamod_miscount - no disp, has na", {
  set.seed(0)
  prob_mean <- c(0.5, 0.2, 0.3, 0.4)
  prob_disp <- rep(0.4, 4)
  prob_levels <- 1:4
  prob_matrix_outcome <- Matrix::Matrix(kronecker(rep(1, 3), diag(4)))
  rate_mean <- c(0.3, 0.3, 0.2)
  rate_disp <- c(0.5, 0.2, 0.6)
  rate_levels <- 1:3
  rate_matrix_outcome <- Matrix::Matrix(kronecker(diag(3), rep(1, 4)))
  datamod <- new_bage_datamod_miscount(prob_mean = prob_mean,
                                       prob_disp = prob_disp,
                                       prob_levels = prob_levels,
                                       prob_matrix_outcome = prob_matrix_outcome,
                                       rate_mean = rate_mean,
                                       rate_disp = rate_disp,
                                       rate_levels = rate_levels,
                                       rate_matrix_outcome = rate_matrix_outcome,
                                       nms_by = c("sex", "age"))
  components <- tibble::tibble(
    term = c("(Intercept)", rep("datamod", 7)),
    component = c("(Intercept)", rep(c("prob", "rate"), times = c(4, 3))),
    level = c("(Intercept)", 0:3, 1:3),
    .fitted = rvec::runif_rvec(n = 8, n_draw = 10)
  )
  outcome <- rpois(12, lambda = 5)
  offset <- runif(12, max = 10)
  outcome[12] <- NA
  expected <- rvec::rgamma_rvec(12, shape = 1, rate = 0.2, n_draw = 10)
  set.seed(1)
  ans_obtained <- draw_outcome_true_given_obs(datamod = datamod,
                                              nm_distn = "pois",
                                              components = components,
                                              outcome = outcome,
                                              offset = offset,
                                              expected = expected,
                                              disp = NULL)
  set.seed(1)
  pi <- rep(components$.fitted[2:5], times = 3)
  alpha <- rep(components$.fitted[6:8], each = 4)
  u <- rvec::rbinom_rvec(n = 11, size = outcome[-12], prob = (pi / (pi + alpha))[-12])
  w <- rvec::rpois_rvec(n = 11, lambda = ((1 - pi) * expected * offset)[-12])
  ans_expected <- vctrs::vec_c(u + w, NA)
  expect_equal(ans_obtained, ans_expected)
})

test_that("'draw_outcome_true_given_obs' throws expected error with wrong distribution and bage_datamod_miscount", {
  set.seed(0)
  prob_mean <- c(0.5, 0.2, 0.3, 0.4)
  prob_disp <- rep(0.4, 4)
  prob_levels <- 1:4
  prob_matrix_outcome <- Matrix::Matrix(kronecker(rep(1, 3), diag(4)))
  rate_mean <- c(0.3, 0.3, 0.2)
  rate_disp <- c(0.5, 0.2, 0.6)
  rate_levels <- 1:3
  rate_matrix_outcome <- Matrix::Matrix(kronecker(diag(3), rep(1, 4)))
  datamod <- new_bage_datamod_miscount(prob_mean = prob_mean,
                                       prob_disp = prob_disp,
                                       prob_levels = prob_levels,
                                       prob_matrix_outcome = prob_matrix_outcome,
                                       rate_mean = rate_mean,
                                       rate_disp = rate_disp,
                                       rate_levels = rate_levels,
                                       rate_matrix_outcome = rate_matrix_outcome,
                                       nms_by = c("sex", "age"))
  components <- tibble::tibble(
    term = c("(Intercept)", rep("datamod", 7)),
    component = c("(Intercept)", rep(c("prob", "rate"), times = c(4, 3))),
    level = c("(Intercept)", 0:3, 1:3),
    .fitted = rvec::runif_rvec(n = 8, n_draw = 10)
  )
  outcome <- rpois(12, lambda = 5)
  offset <- runif(12, max = 10)
  expected <- rvec::rgamma_rvec(12, shape = 1, rate = 0.2, n_draw = 10)
  disp <- rvec::runif_rvec(1)
  expect_error(draw_outcome_true_given_obs(datamod = datamod,
                                           nm_distn = "binom",
                                           components = components,
                                           outcome = outcome,
                                           offset = offset,
                                           expected = expected,
                                           disp = disp),
               "Internal error: `datamod` has class")
})

test_that("'draw_outcome_true_given_obs' works with bage_datamod_noise - no na", {
  set.seed(0)
  sd_sd <- c(0.3, 0.3, 0.2)
  sd_levels <- 1:3
  sd_matrix_outcome <- Matrix::Matrix(kronecker(diag(3), rep(1, 4)))
  datamod <- new_bage_datamod_noise(sd = sd_sd,
                                    sd_levels = sd_levels,
                                    sd_matrix_outcome = sd_matrix_outcome,
                                    nms_by = c("sex", "age"),
                                    outcome_sd = 2)
  outcome <- rnorm(12)
  offset <- runif(12, max = 10)
  expected <- rvec::rnorm_rvec(12, n_draw = 10)
  disp <- rvec::runif_rvec(1, n_draw = 10)
  set.seed(1)
  ans_obtained <- draw_outcome_true_given_obs(datamod = datamod,
                                              nm_distn = "norm",
                                              components = NULL,
                                              outcome = outcome,
                                              offset = offset,
                                              expected = expected,
                                              disp = disp)
  set.seed(1)
  tau1 <- offset / (disp^2)
  tau2 <- 1 / (get_datamod_sd(datamod)^2)
  mean <- (tau1/(tau1+tau2)) * expected +
    (tau2/(tau1+tau2)) * outcome
  sd <- sqrt(1/(tau1+tau2))
  ans_expected <- rvec::rnorm_rvec(n = 12, mean = mean, sd = sd)
  expect_equal(ans_obtained, ans_expected)
})

test_that("'draw_outcome_true_given_obs' works with bage_datamod_noise - has na", {
  set.seed(0)
  sd_sd <- c(0.3, 0.3, 0.2)
  sd_levels <- 1:3
  sd_matrix_outcome <- Matrix::Matrix(kronecker(diag(3), rep(1, 4)))
  datamod <- new_bage_datamod_noise(sd = sd_sd,
                                    sd_levels = sd_levels,
                                    sd_matrix_outcome = sd_matrix_outcome,
                                    nms_by = c("sex", "age"),
                                    outcome_sd = 2)
  outcome <- rnorm(12)
  offset <- runif(12, max = 10)
  offset[1] <- NA
  expected <- rvec::rnorm_rvec(12, n_draw = 10)
  disp <- rvec::runif_rvec(1, n_draw = 10)
  set.seed(1)
  ans_obtained <- draw_outcome_true_given_obs(datamod = datamod,
                                              nm_distn = "norm",
                                              components = NULL,
                                              outcome = outcome,
                                              offset = offset,
                                              expected = expected,
                                              disp = disp)
  set.seed(1)
  tau1 <- offset[-1] / (disp^2)
  tau2 <- 1 / (get_datamod_sd(datamod)^2)[-1]
  mean <- (tau1/(tau1+tau2)) * expected[-1] +
    (tau2/(tau1+tau2)) * outcome[-1]
  sd <- sqrt(1/(tau1+tau2))
  ans_expected <- vctrs::vec_c(NA, rvec::rnorm_rvec(n = 11, mean = mean, sd = sd))
  expect_equal(ans_obtained, ans_expected)
})

test_that("'draw_outcome_true_given_obs' throws expected error with wrong distribution and bage_datamod_noise", {
  set.seed(0)
  sd_sd <- c(0.3, 0.3, 0.2)
  sd_levels <- 1:3
  sd_matrix_outcome <- Matrix::Matrix(kronecker(diag(3), rep(1, 4)))
  datamod <- new_bage_datamod_noise(sd = sd_sd,
                                    sd_levels = sd_levels,
                                    sd_matrix_outcome = sd_matrix_outcome,
                                    nms_by = c("sex", "age"),
                                    outcome_sd = 2)
  outcome <- rnorm(12)
  offset <- runif(12, max = 10)
  expected <- rvec::rnorm_rvec(12, n_draw = 10)
  disp <- rvec::runif_rvec(1)
  set.seed(1)
  expect_error(draw_outcome_true_given_obs(datamod = datamod,
                                           nm_distn = "binom",
                                           components = NULL,
                                           outcome = outcome,
                                           offset = offset,
                                           expected = expected,
                                           disp = disp),
               "Internal error: `datamod` has class")
})

test_that("'draw_outcome_true_given_obs' works with bage_datamod_overcount - no na", {
  set.seed(0)
  rate_mean <- c(0.3, 0.3, 0.2)
  rate_disp <- c(0.5, 0.2, 0.6)
  rate_levels <- 1:3
  rate_matrix_outcome <- Matrix::Matrix(kronecker(diag(3), rep(1, 4)))
  datamod <- new_bage_datamod_overcount(rate_mean = rate_mean,
                                        rate_disp = rate_disp,
                                        rate_levels = rate_levels,
                                        rate_matrix_outcome = rate_matrix_outcome,
                                        nms_by = c("sex", "age"))
  components <- tibble::tibble(
    term = c("(Intercept)", rep("datamod", 3)),
    component = c("(Intercept)", rep("rate", times = 3)),
    level = c("(Intercept)", 1:3),
    .fitted = rvec::runif_rvec(n = 4, n_draw = 10)
  )
  outcome <- rpois(12, lambda = 5)
  offset <- runif(12, max = 10)
  expected <- rvec::rgamma_rvec(12, shape = 1, rate = 0.2, n_draw = 10)
  disp <- rvec::runif_rvec(1)
  set.seed(1)
  ans_obtained <- draw_outcome_true_given_obs(datamod = datamod,
                                              nm_distn = "pois",
                                              components = components,
                                              outcome = outcome,
                                              offset = offset,
                                              expected = expected,
                                              disp = disp)
  set.seed(1)
  alpha <- rep(components$.fitted[2:4], each = 4)
  ans_expected <- rvec::rbinom_rvec(n = 12,
                                    size = outcome,
                                    prob = 1 / (1 + alpha))
  expect_equal(ans_obtained, ans_expected)
})

test_that("'draw_outcome_true_given_obs' works with bage_datamod_overcount - has na", {
  set.seed(0)
  rate_mean <- c(0.3, 0.3, 0.2)
  rate_disp <- c(0.5, 0.2, 0.6)
  rate_levels <- 1:3
  rate_matrix_outcome <- Matrix::Matrix(kronecker(diag(3), rep(1, 4)))
  datamod <- new_bage_datamod_overcount(rate_mean = rate_mean,
                                        rate_disp = rate_disp,
                                        rate_levels = rate_levels,
                                        rate_matrix_outcome = rate_matrix_outcome,
                                        nms_by = c("sex", "age"))
  components <- tibble::tibble(
    term = c("(Intercept)", rep("datamod", 3)),
    component = c("(Intercept)", rep("rate", times = 3)),
    level = c("(Intercept)", 1:3),
    .fitted = rvec::runif_rvec(n = 4, n_draw = 10)
  )
  outcome <- rpois(12, lambda = 5)
  outcome[12] <- NA
  offset <- runif(12, max = 10)
  offset[1] <- NA
  expected <- rvec::rgamma_rvec(12, shape = 1, rate = 0.2, n_draw = 10)
  disp <- rvec::runif_rvec(1)
  set.seed(1)
  ans_obtained <- draw_outcome_true_given_obs(datamod = datamod,
                                              nm_distn = "pois",
                                              components = components,
                                              outcome = outcome,
                                              offset = offset,
                                              expected = expected,
                                              disp = disp)
  set.seed(1)
  alpha <- rep(components$.fitted[2:4], each = 4)
  ans_expected <- vctrs::vec_c(rvec::rbinom_rvec(n = 11,
                                                 size = outcome[-12],
                                                 prob = 1 / (1 + alpha[-12])),
                               NA)
  expect_equal(ans_obtained, ans_expected)
})

test_that("'draw_outcome_true_given_obs' throws expected error with wrong distribution and bage_datamod_overcount", {
  set.seed(0)
  rate_mean <- c(0.3, 0.3, 0.2)
  rate_disp <- c(0.5, 0.2, 0.6)
  rate_levels <- 1:3
  rate_matrix_outcome <- Matrix::Matrix(kronecker(diag(3), rep(1, 4)))
  datamod <- new_bage_datamod_overcount(rate_mean = rate_mean,
                                        rate_disp = rate_disp,
                                        rate_levels = rate_levels,
                                        rate_matrix_outcome = rate_matrix_outcome,
                                        nms_by = c("sex", "age"))
  components <- tibble::tibble(
    term = c("(Intercept)", rep("datamod", 3)),
    component = c("(Intercept)", rep("rate", times = 3)),
    level = c("(Intercept)", 1:3),
    .fitted = rvec::runif_rvec(n = 4, n_draw = 10)
  )
  outcome <- rpois(12, lambda = 5)
  offset <- runif(12, max = 10)
  expected <- rvec::rgamma_rvec(12, shape = 1, rate = 0.2, n_draw = 10)
  disp <- rvec::runif_rvec(1)
  set.seed(1)
  expect_error(draw_outcome_true_given_obs(datamod = datamod,
                                           nm_distn = "binom",
                                           components = components,
                                           outcome = outcome,
                                           offset = offset,
                                           expected = expected,
                                           disp = disp),
               "Internal error: `datamod` has class")
})

test_that("'draw_outcome_true_given_obs' works with bage_datamod_undercount - Poisson - has disp, has na", {
  set.seed(0)
  prob_mean <- c(0.5, 0.2, 0.3, 0.4)
  prob_disp <- rep(0.4, 4)
  prob_levels <- 1:4
  prob_matrix_outcome <- Matrix::Matrix(kronecker(rep(1, 3), diag(4)))
  datamod <- new_bage_datamod_undercount(prob_mean = prob_mean,
                                         prob_disp = prob_disp,
                                         prob_levels = prob_levels,
                                         prob_matrix_outcome = prob_matrix_outcome,
                                         nms_by = c("sex", "age"))
  components <- tibble::tibble(
    term = c("(Intercept)", rep("datamod", 4)),
    component = c("(Intercept)", rep("prob", times = 4)),
    level = c("(Intercept)", 0:3),
    .fitted = rvec::runif_rvec(n = 5, n_draw = 10)
  )
  outcome <- rpois(12, lambda = 5)
  offset <- runif(12, max = 10)
  offset[1] <- NA
  expected <- rvec::rgamma_rvec(12, shape = 1, rate = 0.2, n_draw = 10)
  disp <- rvec::runif_rvec(1, n_draw = 10)
  set.seed(1)
  ans_obtained <- draw_outcome_true_given_obs(datamod = datamod,
                                              nm_distn = "pois",
                                              components = components,
                                              outcome = outcome,
                                              offset = offset,
                                              expected = expected,
                                              disp = disp)
  set.seed(1)
  pi <- rep(components$.fitted[2:5], times = 3)
  p <- (1 + pi[-1] * expected[-1] * offset[-1] * disp) /
    (1 + expected[-1] * offset[-1] * disp)
  lambda <- rvec::rgamma_rvec(n = 11,
                              shape = outcome[-1] + (1/disp),
                              scale = (1 - p) / p)
  w <- rvec::rpois_rvec(n = 11, lambda = lambda)
  ans_expected <- vctrs::vec_c(NA, outcome[-1] + w)
  expect_equal(ans_obtained, ans_expected)
})

test_that("'draw_outcome_true_given_obs' works with bage_datamod_undercount - Poisson - no disp, has na", {
  set.seed(0)
  prob_mean <- c(0.5, 0.2, 0.3, 0.4)
  prob_disp <- rep(0.4, 4)
  prob_levels <- 1:4
  prob_matrix_outcome <- Matrix::Matrix(kronecker(rep(1, 3), diag(4)))
  datamod <- new_bage_datamod_undercount(prob_mean = prob_mean,
                                       prob_disp = prob_disp,
                                       prob_levels = prob_levels,
                                       prob_matrix_outcome = prob_matrix_outcome,
                                       nms_by = c("sex", "age"))
  components <- tibble::tibble(
    term = c("(Intercept)", rep("datamod", 4)),
    component = c("(Intercept)", rep("prob", times = 4)),
    level = c("(Intercept)", 0:3),
    .fitted = rvec::runif_rvec(n = 5, n_draw = 10)
  )
  outcome <- rpois(12, lambda = 5)
  outcome[12] <- NA
  offset <- runif(12, max = 10)
  expected <- rvec::rgamma_rvec(12, shape = 1, rate = 0.2, n_draw = 10)
  set.seed(1)
  ans_obtained <- draw_outcome_true_given_obs(datamod = datamod,
                                              nm_distn = "pois",
                                              components = components,
                                              outcome = outcome,
                                              offset = offset,
                                              expected = expected,
                                              disp = NULL)
  set.seed(1)
  pi <- rep(components$.fitted[2:5], times = 3)
  w <- rvec::rpois_rvec(n = 11,
                        lambda = (1 - pi[-12])* expected[-12] * offset[-12])
  ans_expected <- vctrs::vec_c(outcome[-12] + w, NA)
  expect_equal(ans_obtained, ans_expected)
})

test_that("'draw_outcome_true_given_obs' works with bage_datamod_undercount - binomial - has disp, has NA", {
  set.seed(0)
  prob_mean <- c(0.5, 0.2, 0.3, 0.4)
  prob_disp <- rep(0.4, 4)
  prob_levels <- 1:4
  prob_matrix_outcome <- Matrix::Matrix(kronecker(rep(1, 3), diag(4)))
  datamod <- new_bage_datamod_undercount(prob_mean = prob_mean,
                                         prob_disp = prob_disp,
                                         prob_levels = prob_levels,
                                         prob_matrix_outcome = prob_matrix_outcome,
                                         nms_by = c("sex", "age"))
  components <- tibble::tibble(
    term = c("(Intercept)", rep("datamod", 4)),
    component = c("(Intercept)", rep("prob", times = 4)),
    level = c("(Intercept)", 0:3),
    .fitted = rvec::runif_rvec(n = 5, n_draw = 10)
  )
  outcome <- rpois(12, lambda = 5)
  offset <- rpois(12, lambda = 100)
  offset[1] <- NA
  expected <- rvec::runif_rvec(12, n_draw = 10)
  disp <- rvec::runif_rvec(1, n_draw = 10)
  set.seed(1)
  ans_obtained <- draw_outcome_true_given_obs(datamod = datamod,
                                              nm_distn = "binom",
                                              components = components,
                                              outcome = outcome,
                                              offset = offset,
                                              expected = expected,
                                              disp = disp)
  set.seed(1)
  pi <- rep(components$.fitted[2:5], times = 3)
  ans <- sample_post_binom_betabinom(n = rep(as.numeric(offset[-1]), times = 10),
                                     y = rep(as.numeric(outcome[-1]), times = 10),
                                     mu = as.numeric(expected[-1]),
                                     xi = rep(as.numeric(disp), each = 11),
                                     pi = as.numeric(pi[-1]))
  ans_expected <- rvec::rvec(matrix(ans, nrow = 11))
  ans_expected <- vctrs::vec_c(NA, ans_expected)
  expect_equal(ans_obtained, ans_expected)
})

test_that("'draw_outcome_true_given_obs' works with bage_datamod_undercount - binomial - no disp, has NA", {
  set.seed(0)
  prob_mean <- c(0.5, 0.2, 0.3, 0.4)
  prob_disp <- rep(0.4, 4)
  prob_levels <- 1:4
  prob_matrix_outcome <- Matrix::Matrix(kronecker(rep(1, 3), diag(4)))
  datamod <- new_bage_datamod_undercount(prob_mean = prob_mean,
                                       prob_disp = prob_disp,
                                       prob_levels = prob_levels,
                                       prob_matrix_outcome = prob_matrix_outcome,
                                       nms_by = c("sex", "age"))
  components <- tibble::tibble(
    term = c("(Intercept)", rep("datamod", 4)),
    component = c("(Intercept)", rep("prob", times = 4)),
    level = c("(Intercept)", 0:3),
    .fitted = rvec::runif_rvec(n = 5, n_draw = 10)
  )
  outcome <- rpois(12, lambda = 5)
  offset <- rpois(12, lambda = 100)
  offset[1] <- NA
  expected <- rvec::runif_rvec(12, n_draw = 10)
  set.seed(1)
  ans_obtained <- draw_outcome_true_given_obs(datamod = datamod,
                                              nm_distn = "binom",
                                              components = components,
                                              outcome = outcome,
                                              offset = offset,
                                              expected = expected,
                                              disp = NULL)
  set.seed(1)
  pi <- rep(components$.fitted[2:5], times = 3)
  w <- rvec::rbinom_rvec(n = 11,
                         size = offset[-1] - outcome[-1],
                         prob = (1-pi[-1]) * expected[-1] / (1-pi[-1]*expected[-1]))
  ans_expected <- vctrs::vec_c(NA, outcome[-1] + w)
  expect_equal(ans_obtained, ans_expected)
})

test_that("'draw_outcome_true_given_obs' gives expected error with bage_datamod_undercount - normal", {
  set.seed(0)
  prob_mean <- c(0.5, 0.2, 0.3, 0.4)
  prob_disp <- rep(0.4, 4)
  prob_levels <- 1:4
  prob_matrix_outcome <- Matrix::Matrix(kronecker(rep(1, 3), diag(4)))
  datamod <- new_bage_datamod_undercount(prob_mean = prob_mean,
                                         prob_disp = prob_disp,
                                         prob_levels = prob_levels,
                                         prob_matrix_outcome = prob_matrix_outcome,
                                         nms_by = c("sex", "age"))
  components <- tibble::tibble(
    term = c("(Intercept)", rep("datamod", 4)),
    component = c("(Intercept)", rep("prob", times = 4)),
    level = c("(Intercept)", 0:3),
    .fitted = rvec::runif_rvec(n = 5, n_draw = 10)
  )
  outcome <- rpois(12, lambda = 5)
  offset <- rpois(12, lambda = 100)
  expected <- rvec::runif_rvec(12, n_draw = 10)
  set.seed(1)
  expect_error(draw_outcome_true_given_obs(datamod = datamod,
                                           nm_distn = "norm",
                                           components = components,
                                           outcome = outcome,
                                           offset = offset,
                                           expected = expected,
                                           disp = NULL),
               "Internal error: `datamod` has class")
})


## 'get_datamod_transform_param' ----------------------------------------------

test_that("'get_datamod_transform_param' works with bage_datamod_miscount", {
  set.seed(0)
  prob_mean <- c(0.5, 0.2, 0.3, 0.4)
  prob_disp <- rep(0.4, 4)
  prob_levels <- 1:4
  prob_matrix_outcome <- Matrix::Matrix(kronecker(rep(1, 3), diag(4)))
  rate_mean <- c(0.3, 0.3, 0.2)
  rate_disp <- c(0.5, 0.2, 0.6)
  rate_levels <- 1:3
  rate_matrix_outcome <- Matrix::Matrix(kronecker(diag(3), rep(1, 4)))
  datamod <- new_bage_datamod_miscount(prob_mean = prob_mean,
                                       prob_disp = prob_disp,
                                       prob_levels = prob_levels,
                                       prob_matrix_outcome = prob_matrix_outcome,
                                       rate_mean = rate_mean,
                                       rate_disp = rate_disp,
                                       rate_levels = rate_levels,
                                       rate_matrix_outcome = rate_matrix_outcome,
                                       nms_by = c("sex", "age"))
  fun <- get_datamod_transform_param(datamod)
  x <- rvec::rnorm_rvec(7, n_draw = 10)
  s1 <- prob_mean / prob_disp
  s2 <- (1 - prob_mean) / prob_disp
  shape <- 1 / rate_disp
  rate <- 1 / (rate_disp * rate_mean)
  expect_identical(fun(x),
                   c(rvec::qbeta_rvec(xx <- rvec::pnorm_rvec(x[1:4]), s1, s2),
                     rvec::qgamma_rvec(rvec::pnorm_rvec(x[5:7]),
                                                        shape,
                                                        rate = rate)))
})

test_that("'get_datamod_transform_param' works with bage_datamod_overcount", {
  set.seed(0)
  rate_mean <- c(0.3, 0.3, 0.2)
  rate_disp <- c(0.5, 0.2, 0.6)
  rate_levels <- 1:3
  rate_matrix_outcome <- Matrix::Matrix(kronecker(diag(3), rep(1, 4)))
  datamod <- new_bage_datamod_overcount(rate_mean = rate_mean,
                                        rate_disp = rate_disp,
                                        rate_levels = rate_levels,
                                        rate_matrix_outcome = rate_matrix_outcome,
                                        nms_by = c("sex", "age"))
  fun <- get_datamod_transform_param(datamod)
  x <- rvec::rnorm_rvec(n = 3, n_draw = 10)
  ans_obtained <- fun(x)
  shape <- 1 / rate_disp
  rate <- 1 / (rate_disp * rate_mean)
  ans_expected <- rvec::qgamma_rvec(rvec::pnorm_rvec(x),
                                    shape = shape,
                                    rate = rate)
  expect_equal(ans_obtained, ans_expected)
})

test_that("'get_datamod_transform_param' works with bage_datamod_undercount", {
  set.seed(0)
  prob_mean <- c(0.5, 0.2, 0.3, 0.4)
  prob_disp <- rep(0.4, 4)
  prob_levels <- 1:4
  prob_matrix_outcome <- Matrix::Matrix(kronecker(rep(1, 3), diag(4)))
  datamod <- new_bage_datamod_undercount(prob_mean = prob_mean,
                                       prob_disp = prob_disp,
                                       prob_levels = prob_levels,
                                       prob_matrix_outcome = prob_matrix_outcome,
                                       nms_by = c("sex", "age"))
  fun <- get_datamod_transform_param(datamod)
  x <- rvec::rnorm_rvec(4, n_draw = 5)
  ans_obtained <- fun(x)
  s1 <- prob_mean / prob_disp
  s2 <- (1 - prob_mean) / prob_disp
  ans_expected <- rvec::qbeta_rvec(rvec::pnorm_rvec(x), s1, s2)
  expect_equal(ans_obtained, ans_expected)
})


## 'make_datamod_comp' --------------------------------------------------------

test_that("'make_datamod_comp' works with bage_datamod_miscount", {
  set.seed(0)
  prob_mean <- c(0.5, 0.2, 0.3, 0.4)
  prob_disp <- rep(0.4, 4)
  prob_levels <- 1:4
  prob_matrix_outcome <- Matrix::Matrix(kronecker(rep(1, 3), diag(4)))
  rate_mean <- c(0.3, 0.3, 0.2)
  rate_disp <- c(0.5, 0.2, 0.6)
  rate_levels <- 1:3
  rate_matrix_outcome <- Matrix::Matrix(kronecker(diag(3), rep(1, 4)))
  datamod <- new_bage_datamod_miscount(prob_mean = prob_mean,
                                       prob_disp = prob_disp,
                                       prob_levels = prob_levels,
                                       prob_matrix_outcome = prob_matrix_outcome,
                                       rate_mean = rate_mean,
                                       rate_disp = rate_disp,
                                       rate_levels = rate_levels,
                                       rate_matrix_outcome = rate_matrix_outcome,
                                       nms_by = c("sex", "age"))
  ans_obtained <- make_datamod_comp(datamod)
  ans_expected <- rep(c("prob", "rate"), times = 4:3)
  expect_identical(ans_obtained, ans_expected)
})

test_that("'make_datamod_comp' works with bage_datamod_overcount", {
  set.seed(0)
  rate_mean <- c(0.3, 0.3, 0.2)
  rate_disp <- c(0.5, 0.2, 0.6)
  rate_levels <- 1:3
  rate_matrix_outcome <- Matrix::Matrix(kronecker(diag(3), rep(1, 4)))
  datamod <- new_bage_datamod_overcount(rate_mean = rate_mean,
                                        rate_disp = rate_disp,
                                        rate_levels = rate_levels,
                                        rate_matrix_outcome = rate_matrix_outcome,
                                        nms_by = c("sex", "age"))
  ans_obtained <- make_datamod_comp(datamod)
  ans_expected <- rep("rate", 3)
  expect_identical(ans_obtained, ans_expected)
})

test_that("'make_datamod_comp' works with bage_datamod_undercount", {
  set.seed(0)
  prob_mean <- c(0.5, 0.2, 0.3, 0.4)
  prob_disp <- rep(0.4, 4)
  prob_levels <- 1:4
  prob_matrix_outcome <- Matrix::Matrix(kronecker(rep(1, 3), diag(4)))
  datamod <- new_bage_datamod_undercount(prob_mean = prob_mean,
                                       prob_disp = prob_disp,
                                       prob_levels = prob_levels,
                                       prob_matrix_outcome = prob_matrix_outcome,
                                       nms_by = c("sex", "age"))
  ans_obtained <- make_datamod_comp(datamod)
  ans_expected <- rep("prob", 4)
  expect_identical(ans_obtained, ans_expected)
})


## 'make_datamod_consts' ------------------------------------------------------

test_that("'make_datamod_consts' works with bage_datamod_exposure", {
  set.seed(0)
  disp <- c(0.3, 0.2, 0.3, 0.2)
  disp_levels <- 1:4
  disp_matrix_outcome <- Matrix::Matrix(kronecker(rep(1, 3), diag(4)))
  datamod <- new_bage_datamod_exposure(disp = disp,
                                       disp_levels = disp_levels,
                                       disp_matrix_outcome = disp_matrix_outcome,
                                       nms_by = c("sex", "age"))
  ans_obtained <- make_datamod_consts(datamod)
  ans_expected <- disp
  expect_equal(ans_obtained, ans_expected)
})

test_that("'make_i_datamod' works with bage_datamod_miscount", {
  set.seed(0)
  prob_mean <- c(0.5, 0.2, 0.3, 0.4)
  prob_disp <- rep(0.4, 4)
  prob_levels <- 1:4
  prob_matrix_outcome <- Matrix::Matrix(kronecker(rep(1, 3), diag(4)))
  rate_mean <- c(0.3, 0.3, 0.2)
  rate_disp <- c(0.5, 0.2, 0.6)
  rate_levels <- 1:3
  rate_matrix_outcome <- Matrix::Matrix(kronecker(diag(3), rep(1, 4)))
  datamod <- new_bage_datamod_miscount(prob_mean = prob_mean,
                                       prob_disp = prob_disp,
                                       prob_levels = prob_levels,
                                       prob_matrix_outcome = prob_matrix_outcome,
                                       rate_mean = rate_mean,
                                       rate_disp = rate_disp,
                                       rate_levels = rate_levels,
                                       rate_matrix_outcome = rate_matrix_outcome,
                                       nms_by = c("sex", "age"))
  ans_obtained <- make_datamod_consts(datamod)
  ans_expected <- c(prob_mean,
                    prob_disp,
                    rate_mean,
                    rate_disp)
  expect_equal(ans_obtained, ans_expected)
})

test_that("'make_datamod_consts' works with bage_datamod_noise - has outcome_sd", {
  set.seed(0)
  sd_sd <- c(0.3, 0.3, 0.2)
  sd_levels <- 1:3
  sd_matrix_outcome <- Matrix::Matrix(kronecker(diag(3), rep(1, 4)))
  datamod <- new_bage_datamod_noise(sd = sd_sd,
                                    sd_levels = sd_levels,
                                    sd_matrix_outcome = sd_matrix_outcome,
                                    nms_by = c("sex", "age"),
                                    outcome_sd = 2)
  ans_obtained <- make_datamod_consts(datamod)
  ans_expected <- sd_sd / 2
  expect_equal(ans_obtained, ans_expected)
})

test_that("'make_datamod_consts' works with bage_datamod_noise - no outcome_sd", {
  set.seed(0)
  sd_sd <- c(0.3, 0.3, 0.2)
  sd_levels <- 1:3
  sd_matrix_outcome <- Matrix::Matrix(kronecker(diag(3), rep(1, 4)))
  datamod <- new_bage_datamod_noise(sd = sd_sd,
                                    sd_levels = sd_levels,
                                    sd_matrix_outcome = sd_matrix_outcome,
                                    nms_by = c("sex", "age"),
                                    outcome_sd = NULL)
  ans_obtained <- make_datamod_consts(datamod)
  ans_expected <- sd_sd
  expect_equal(ans_obtained, ans_expected)
})

test_that("'make_datamod_consts' works with bage_datamod_overcount", {
  set.seed(0)
  rate_mean <- c(0.3, 0.3, 0.2)
  rate_disp <- c(0.5, 0.2, 0.6)
  rate_levels <- 1:3
  rate_matrix_outcome <- Matrix::Matrix(kronecker(diag(3), rep(1, 4)))
  datamod <- new_bage_datamod_overcount(rate_mean = rate_mean,
                                        rate_disp = rate_disp,
                                        rate_levels = rate_levels,
                                        rate_matrix_outcome = rate_matrix_outcome,
                                        nms_by = c("sex", "age"))
  ans_obtained <- make_datamod_consts(datamod)
  ans_expected <- c(rate_mean, rate_disp)
  expect_equal(ans_obtained, ans_expected)
})

test_that("'make_datamod_consts' works with bage_datamod_undercount", {
  set.seed(0)
  prob_mean <- c(0.5, 0.2, 0.3, 0.4)
  prob_disp <- rep(0.4, 4)
  prob_levels <- 1:4
  prob_matrix_outcome <- Matrix::Matrix(kronecker(rep(1, 3), diag(4)))
  datamod <- new_bage_datamod_undercount(prob_mean = prob_mean,
                                       prob_disp = prob_disp,
                                       prob_levels = prob_levels,
                                       prob_matrix_outcome = prob_matrix_outcome,
                                       nms_by = c("sex", "age"))
  ans_obtained <- make_datamod_consts(datamod)
  ans_expected <- c(prob_mean, prob_disp)
  expect_equal(ans_obtained, ans_expected)
})


## 'make_datamod_matrices' ----------------------------------------------------

test_that("'make_datamod_matrices' works with bage_datamod_exposure", {
  set.seed(0)
  disp <- c(0.3, 0.2, 0.3, 0.2)
  disp_levels <- 1:4
  disp_matrix_outcome <- Matrix::Matrix(kronecker(rep(1, 3), diag(4)))
  datamod <- new_bage_datamod_exposure(disp = disp,
                                       disp_levels = disp_levels,
                                       disp_matrix_outcome = disp_matrix_outcome,
                                       nms_by = c("sex", "age"))
  ans_obtained <- make_datamod_matrices(datamod)
  ans_expected <- list(disp_matrix_outcome)
  expect_equal(ans_obtained, ans_expected)
})

test_that("'make_datamod_matrices' works with bage_datamod_miscount", {
  set.seed(0)
  prob_mean <- c(0.5, 0.2, 0.3, 0.4)
  prob_disp <- rep(0.4, 4)
  prob_levels <- 1:4
  prob_matrix_outcome <- Matrix::Matrix(kronecker(rep(1, 3), diag(4)))
  rate_mean <- c(0.3, 0.3, 0.2)
  rate_disp <- c(0.5, 0.2, 0.6)
  rate_levels <- 1:3
  rate_matrix_outcome <- Matrix::Matrix(kronecker(diag(3), rep(1, 4)))
  datamod <- new_bage_datamod_miscount(prob_mean = prob_mean,
                                       prob_disp = prob_disp,
                                       prob_levels = prob_levels,
                                       prob_matrix_outcome = prob_matrix_outcome,
                                       rate_mean = rate_mean,
                                       rate_disp = rate_disp,
                                       rate_levels = rate_levels,
                                       rate_matrix_outcome = rate_matrix_outcome,
                                       nms_by = c("sex", "age"))
  ans_obtained <- make_datamod_matrices(datamod)
  ans_expected <- list(prob_matrix_outcome,
                       rate_matrix_outcome)
  expect_equal(ans_obtained, ans_expected)
})

test_that("'make_datamod_matrices' works with bage_datamod_noise", {
  set.seed(0)
  sd_sd <- c(0.3, 0.3, 0.2)
  sd_levels <- 1:3
  sd_matrix_outcome <- Matrix::Matrix(kronecker(diag(3), rep(1, 4)))
  datamod <- new_bage_datamod_noise(sd = sd_sd,
                                    sd_levels = sd_levels,
                                    sd_matrix_outcome = sd_matrix_outcome,
                                    nms_by = c("sex", "age"),
                                    outcome_sd = 2)
  ans_obtained <- make_datamod_matrices(datamod)
  ans_expected <- list(sd_matrix_outcome)
  expect_equal(ans_obtained, ans_expected)
})

test_that("'make_datamod_matrices' works with bage_datamod_overcount", {
  set.seed(0)
  rate_mean <- c(0.3, 0.3, 0.2)
  rate_disp <- c(0.5, 0.2, 0.6)
  rate_levels <- 1:3
  rate_matrix_outcome <- Matrix::Matrix(kronecker(diag(3), rep(1, 4)))
  datamod <- new_bage_datamod_overcount(rate_mean = rate_mean,
                                        rate_disp = rate_disp,
                                        rate_levels = rate_levels,
                                        rate_matrix_outcome = rate_matrix_outcome,
                                        nms_by = c("sex", "age"))
  ans_obtained <- make_datamod_matrices(datamod)
  ans_expected <- list(rate_matrix_outcome)
  expect_equal(ans_obtained, ans_expected)
})

test_that("'make_datamod_matrices' works with bage_datamod_undercount", {
  set.seed(0)
  prob_mean <- c(0.5, 0.2, 0.3, 0.4)
  prob_disp <- rep(0.4, 4)
  prob_levels <- 1:4
  prob_matrix_outcome <- Matrix::Matrix(kronecker(rep(1, 3), diag(4)))
  datamod <- new_bage_datamod_undercount(prob_mean = prob_mean,
                                       prob_disp = prob_disp,
                                       prob_levels = prob_levels,
                                       prob_matrix_outcome = prob_matrix_outcome,
                                       nms_by = c("sex", "age"))
  ans_obtained <- make_datamod_matrices(datamod)
  ans_expected <- list(prob_matrix_outcome)
  expect_equal(ans_obtained, ans_expected)
})


## 'make_datamod_param' -------------------------------------------------------

test_that("'make_datamod_param' works with bage_datamod_exposure", {
  set.seed(0)
  disp <- c(0.3, 0.2, 0.3, 0.2)
  disp_levels <- 1:4
  disp_matrix_outcome <- Matrix::Matrix(kronecker(rep(1, 3), diag(4)))
  datamod <- new_bage_datamod_exposure(disp = disp,
                                       disp_levels = disp_levels,
                                       disp_matrix_outcome = disp_matrix_outcome,
                                       nms_by = c("sex", "age"))
  ans_obtained <- make_datamod_param(datamod)
  ans_expected <- double()
  expect_equal(ans_obtained, ans_expected)
})

test_that("'make_datamod_param' works with bage_datamod_miscount", {
  set.seed(0)
  prob_mean <- c(0.5, 0.2, 0.3, 0.4)
  prob_disp <- rep(0.4, 4)
  prob_levels <- 1:4
  prob_matrix_outcome <- Matrix::Matrix(kronecker(rep(1, 3), diag(4)))
  rate_mean <- c(0.3, 0.3, 0.2)
  rate_disp <- c(0.5, 0.2, 0.6)
  rate_levels <- 1:3
  rate_matrix_outcome <- Matrix::Matrix(kronecker(diag(3), rep(1, 4)))
  datamod <- new_bage_datamod_miscount(prob_mean = prob_mean,
                                       prob_disp = prob_disp,
                                       prob_levels = prob_levels,
                                       prob_matrix_outcome = prob_matrix_outcome,
                                       rate_mean = rate_mean,
                                       rate_disp = rate_disp,
                                       rate_levels = rate_levels,
                                       rate_matrix_outcome = rate_matrix_outcome,
                                       nms_by = c("sex", "age"))
  ans_obtained <- make_datamod_param(datamod)
  ans_expected <- rep(0, times = 7)
  expect_equal(ans_obtained, ans_expected)
})

test_that("'make_datamod_param' works with bage_datamod_noise", {
  set.seed(0)
  sd_sd <- c(0.3, 0.3, 0.2)
  sd_levels <- 1:3
  sd_matrix_outcome <- Matrix::Matrix(kronecker(diag(3), rep(1, 4)))
  datamod <- new_bage_datamod_noise(sd = sd_sd,
                                    sd_levels = sd_levels,
                                    sd_matrix_outcome = sd_matrix_outcome,
                                    nms_by = c("sex", "age"),
                                    outcome_sd = 2)
  ans_obtained <- make_datamod_param(datamod)
  ans_expected <- double()
  expect_equal(ans_obtained, ans_expected)
})

test_that("'make_datamod_param' works with bage_datamod_overcount", {
  set.seed(0)
  rate_mean <- c(0.3, 0.3, 0.2)
  rate_disp <- c(0.5, 0.2, 0.6)
  rate_levels <- 1:3
  rate_matrix_outcome <- Matrix::Matrix(kronecker(diag(3), rep(1, 4)))
  datamod <- new_bage_datamod_overcount(rate_mean = rate_mean,
                                        rate_disp = rate_disp,
                                        rate_levels = rate_levels,
                                        rate_matrix_outcome = rate_matrix_outcome,
                                        nms_by = c("sex", "age"))
  ans_obtained <- make_datamod_param(datamod)
  ans_expected <- rep(0, times = 3)
  expect_equal(ans_obtained, ans_expected)
})

test_that("'make_datamod_param' works with bage_datamod_undercount", {
  set.seed(0)
  prob_mean <- c(0.5, 0.2, 0.3, 0.4)
  prob_disp <- rep(0.4, 4)
  prob_levels <- 1:4
  prob_matrix_outcome <- Matrix::Matrix(kronecker(rep(1, 3), diag(4)))
  datamod <- new_bage_datamod_undercount(prob_mean = prob_mean,
                                       prob_disp = prob_disp,
                                       prob_levels = prob_levels,
                                       prob_matrix_outcome = prob_matrix_outcome,
                                       nms_by = c("sex", "age"))
  ans_obtained <- make_datamod_param(datamod)
  ans_expected <- rep(0, times = 4)
  expect_equal(ans_obtained, ans_expected)
})


## 'make_level_datamod' -------------------------------------------------------

test_that("'make_i_datamod' works with bage_datamod_miscount", {
  set.seed(0)
  prob_mean <- c(0.5, 0.2, 0.3, 0.4)
  prob_disp <- rep(0.4, 4)
  prob_levels <- 1:4
  prob_matrix_outcome <- Matrix::Matrix(kronecker(rep(1, 3), diag(4)))
  rate_mean <- c(0.3, 0.3, 0.2)
  rate_disp <- c(0.5, 0.2, 0.6)
  rate_levels <- 1:3
  rate_matrix_outcome <- Matrix::Matrix(kronecker(diag(3), rep(1, 4)))
  datamod <- new_bage_datamod_miscount(prob_mean = prob_mean,
                                       prob_disp = prob_disp,
                                       prob_levels = prob_levels,
                                       prob_matrix_outcome = prob_matrix_outcome,
                                       rate_mean = rate_mean,
                                       rate_disp = rate_disp,
                                       rate_levels = rate_levels,
                                       rate_matrix_outcome = rate_matrix_outcome,
                                       nms_by = c("sex", "age"))
  ans_obtained <- make_level_datamod(datamod)
  ans_expected <- c(prob_levels, rate_levels)
  expect_identical(ans_obtained, ans_expected)
})

test_that("'make_level_datamod' works with bage_datamod_overcount", {
  set.seed(0)
  rate_mean <- c(0.3, 0.3, 0.2)
  rate_disp <- c(0.5, 0.2, 0.6)
  rate_levels <- 1:3
  rate_matrix_outcome <- Matrix::Matrix(kronecker(diag(3), rep(1, 4)))
  datamod <- new_bage_datamod_overcount(rate_mean = rate_mean,
                                        rate_disp = rate_disp,
                                        rate_levels = rate_levels,
                                        rate_matrix_outcome = rate_matrix_outcome,
                                        nms_by = c("sex", "age"))
  ans_obtained <- make_level_datamod(datamod)
  ans_expected <- rate_levels
  expect_identical(ans_obtained, ans_expected)
})

test_that("'make_level_datamod' works with bage_datamod_undercount", {
  set.seed(0)
  prob_mean <- c(0.5, 0.2, 0.3, 0.4)
  prob_disp <- rep(0.4, 4)
  prob_levels <- 1:4
  prob_matrix_outcome <- Matrix::Matrix(kronecker(rep(1, 3), diag(4)))
  datamod <- new_bage_datamod_undercount(prob_mean = prob_mean,
                                       prob_disp = prob_disp,
                                       prob_levels = prob_levels,
                                       prob_matrix_outcome = prob_matrix_outcome,
                                       nms_by = c("sex", "age"))
  ans_obtained <- make_level_datamod(datamod)
  ans_expected <- prob_levels
  expect_identical(ans_obtained, ans_expected)
})


## 'make_expected_obs_exposure' -----------------------------------------------

test_that("'make_expected_obs_exposure' works", {
  disp <- c(0.3, 0.2, 0.3, 0.2)
  disp_levels <- 1:4
  disp_matrix_outcome <- Matrix::Matrix(kronecker(rep(1, 3), diag(4)))
  datamod <- new_bage_datamod_exposure(disp = disp,
                                       disp_levels = disp_levels,
                                       disp_matrix_outcome = disp_matrix_outcome,
                                       nms_by = c("sex", "age"))
  expected <- exp(rvec::rnorm_rvec(n = 12, n_draw = 10))
  ans_obtained <- make_expected_obs_exposure(datamod = datamod,
                                             expected = expected)
  disp <- get_datamod_disp(datamod)
  ans_expected <- ((3 + 1/disp)/(1 + 1/disp)) * expected
  expect_equal(ans_obtained, ans_expected)
})


## 'make_expected_obs_miscount' ----------------------------------------------

test_that("'make_expected_obs_miscount' works", {
  prob_mean <- c(0.5, 0.2, 0.3, 0.4)
  prob_disp <- c(0.3, 0.2, 0.3, 0.2)
  prob_levels <- 1:4
  prob_matrix_outcome <- Matrix::Matrix(kronecker(rep(1, 3), diag(4)))
  rate_mean <- c(0.5, 0.2, 0.3, 0.4)
  rate_disp <- c(0.3, 0.2, 0.3, 0.2)
  rate_levels <- 1:4
  rate_matrix_outcome <- Matrix::Matrix(kronecker(rep(1, 3), diag(4)))
  datamod <- new_bage_datamod_miscount(prob_mean = prob_mean,
                                       prob_disp = prob_disp,
                                       prob_levels = prob_levels,
                                       prob_matrix_outcome = prob_matrix_outcome,
                                       rate_mean = rate_mean,
                                       rate_disp = rate_disp,
                                       rate_levels = rate_levels,
                                       rate_matrix_outcome = rate_matrix_outcome,
                                       nms_by = c("sex", "age"))
  components <- tibble::tibble(
    term = c("(Intercept)", rep("datamod", 8)),
    component = c("(Intercept)", rep(c("prob", "rate"), each = 4)),
    level = c("(Intercept)", 0:3, 0:3),
    .fitted = rvec::runif_rvec(n = 9, n_draw = 10)
  )
  expected <- exp(rvec::rnorm_rvec(n = 12, n_draw = 10))
  ans_obtained <- make_expected_obs_miscount(datamod = datamod,
                                             components = components,
                                             expected = expected)
  prob <- as.matrix(prob_matrix_outcome) %*% components$.fitted[2:5]
  rate <- as.matrix(rate_matrix_outcome) %*% components$.fitted[6:9]
  ans_expected <- (prob + rate) * expected
  expect_identical(ans_obtained, ans_expected)
})


## 'make_expected_obs_noise' -----------------------------------------------

test_that("'make_expected_obs_noise' works", {
  sd_sd <- c(0.3, 0.2, 0.3, 0.2)
  sd_levels <- 1:4
  sd_matrix_outcome <- Matrix::Matrix(kronecker(rep(1, 3), diag(4)))
  datamod <- new_bage_datamod_noise(sd_sd = sd_sd,
                                    sd_levels = sd_levels,
                                    sd_matrix_outcome = sd_matrix_outcome,
                                    nms_by = c("sex", "age"),
                                    outcome_sd = NULL)
  expected <- exp(rvec::rnorm_rvec(n = 12, n_draw = 10))
  ans_obtained <- make_expected_obs_noise(expected)
  ans_expected <- expected
  expect_equal(ans_obtained, ans_expected)
})


## 'make_expected_obs_overcount' ----------------------------------------------

test_that("'make_expected_obs_overcount' works", {
  rate_mean <- c(0.5, 0.2, 0.3, 0.4)
  rate_disp <- c(0.3, 0.2, 0.3, 0.2)
  rate_levels <- 1:4
  rate_matrix_outcome <- Matrix::Matrix(kronecker(rep(1, 3), diag(4)))
  datamod <- new_bage_datamod_overcount(rate_mean = rate_mean,
                                        rate_disp = rate_disp,
                                        rate_levels = rate_levels,
                                        rate_matrix_outcome = rate_matrix_outcome,
                                        nms_by = c("sex", "age"))
  components <- tibble::tibble(
    term = c("(Intercept)", rep("datamod", 4)),
    component = c("(Intercept)", rep("rate", 4)),
    level = c("(Intercept)", 0:3),
    .fitted = rvec::runif_rvec(n = 5, n_draw = 10)
  )
  expected <- exp(rvec::rnorm_rvec(n = 12, n_draw = 10))
  ans_obtained <- make_expected_obs_overcount(datamod = datamod,
                                              components = components,
                                              expected = expected)
  rate <- as.matrix(rate_matrix_outcome) %*% components$.fitted[-1]
  ans_expected <- (1 + rate) * expected
  expect_identical(ans_obtained, ans_expected)
})


## 'make_expected_obs_undercount' ----------------------------------------------

test_that("'make_expected_obs_undercount' works", {
  prob_mean <- c(0.5, 0.2, 0.3, 0.4)
  prob_disp <- c(0.3, 0.2, 0.3, 0.2)
  prob_levels <- 1:4
  prob_matrix_outcome <- Matrix::Matrix(kronecker(rep(1, 3), diag(4)))
  datamod <- new_bage_datamod_undercount(prob_mean = prob_mean,
                                         prob_disp = prob_disp,
                                         prob_levels = prob_levels,
                                         prob_matrix_outcome = prob_matrix_outcome,
                                         nms_by = c("sex", "age"))
  components <- tibble::tibble(
    term = c("(Intercept)", rep("datamod", 4)),
    component = c("(Intercept)", rep("prob", 4)),
    level = c("(Intercept)", 0:3),
    .fitted = rvec::runif_rvec(n = 5, n_draw = 10)
  )
  expected <- exp(rvec::rnorm_rvec(n = 12, n_draw = 10))
  ans_obtained <- make_expected_obs_undercount(datamod = datamod,
                                               components = components,
                                               expected = expected)
  prob <- as.matrix(prob_matrix_outcome) %*% components$.fitted[-1]
  ans_expected <- prob * expected
  expect_identical(ans_obtained, ans_expected)
})


## 'make_i_lik_part' ---------------------------------------------------------------

test_that("'make_i_lik' works with bage_datamod_exposure", {
  x <- new_bage_datamod_exposure(disp = 1,
                                 disp_levels = "disp",
                                 disp_matrix_outcome = Matrix::sparseMatrix(x = rep(1, 5),
                                                                            i = 1:5,
                                                                            j = rep(1, 5)),
                                 nms_by = c("sex", "age"))
  expect_identical(make_i_lik_part(x), 1000L)
})

test_that("'make_i_lik' works with bage_datamod_miscount", {
  x <- new_bage_datamod_miscount(prob_mean = 0.5,
                                 prob_disp = 1,
                                 prob_levels = "prob",
                                 prob_matrix_outcome = Matrix::sparseMatrix(x = rep(1, 5),
                                                                            i = 1:5,
                                                                            j = rep(1, 5)),
                                 rate_mean = 0.5,
                                 rate_disp = 1,
                                 rate_levels = "rate",
                                 rate_matrix_outcome = Matrix::sparseMatrix(x = rep(1, 5),
                                                                            i = 1:5,
                                                                            j = rep(1, 5)),
                                 nms_by = c("sex", "age"))
  expect_identical(make_i_lik_part(x), 2000L)
})

test_that("'make_i_lik' works with bage_datamod_noise", {
  x <- new_bage_datamod_noise(sd_sd = 0.5,
                              sd_levels = "sd",
                              sd_matrix_outcome = Matrix::sparseMatrix(x = rep(1, 5),
                                                                       i = 1:5,
                                                                       j = rep(1, 5)),
                              nms_by = c("sex", "age"),
                              outcome_sd = 2)
  expect_identical(make_i_lik_part(x), 3000L)
})

test_that("'make_i_lik' works with bage_datamod_overcount", {
  x <- new_bage_datamod_overcount(rate_mean = 0.5,
                                  rate_disp = 1,
                                  rate_levels = "rate",
                                  rate_matrix_outcome = Matrix::sparseMatrix(x = rep(1, 5),
                                                                             i = 1:5,
                                                                             j = rep(1, 5)),
                                  nms_by = c("sex", "age"))
  expect_identical(make_i_lik_part(x), 4000L)
})

test_that("'make_i_lik' works with bage_datamod_undercount", {
  x <- new_bage_datamod_undercount(prob_mean = 0.5,
                                   prob_disp = 1,
                                   prob_levels = "prob",
                                   prob_matrix_outcome = Matrix::sparseMatrix(x = rep(1, 5),
                                                                              i = 1:5,
                                                                              j = rep(1, 5)),
                                   nms_by = c("sex", "age"))
  expect_identical(make_i_lik_part(x), 5000L)
})




  
  

