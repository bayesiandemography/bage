
## 'draw_offset_true_given_obs' -----------------------------------------------

test_that("'draw_offset_true_given_obs' works with bage_datamod_exposure", {
  set.seed(0)
  ratio_ratio <- c(0.5, 0.2, 0.3, 0.4)
  ratio_levels <- 1:4
  ratio_matrix_outcome <- Matrix::Matrix(kronecker(rep(1, 3), diag(4)))
  disp_mean <- c(0.3, 0.2, 0.3, 0.2)
  disp_levels <- 1:4
  disp_matrix_outcome <- Matrix::Matrix(kronecker(rep(1, 3), diag(4)))
  datamod <- new_bage_datamod_exposure(ratio_ratio = ratio_ratio,
                                       ratio_levels = ratio_levels,
                                       ratio_matrix_outcome = ratio_matrix_outcome,
                                       disp_mean = disp_mean,
                                       disp_levels = disp_levels,
                                       disp_matrix_outcome = disp_matrix_outcome)
  components <- tibble::tibble(
    term = c("(Intercept)", rep("datamod", 4)),
    component = c("(Intercept)", rep("disp", 4)),
    level = c("(Intercept)", 0:3),
    .fitted = rvec::runif_rvec(n = 5, n_draw = 10)
  )
  offset <- runif(12, max = 10)
  set.seed(1)
  ans_obtained <- draw_offset_true_given_obs(datamod = datamod,
                                             nm_distn = "pois",
                                             components = components,
                                             offset = offset)
  set.seed(1)
  disp <- get_datamod_disp(datamod = datamod,
                           components = components)
  ratio <- get_datamod_ratio(datamod)
  ans_expected <- rvec::rgamma_rvec(n = 12,
                                    3 + (1/disp),
                                    (1 + (1/disp)) * ratio / offset)
  expect_equal(ans_obtained, ans_expected)
})


## 'draw_outcome_true_given_obs' -----------------------------------------------

test_that("'draw_outcome_true_given_obs' works with bage_datamod_exposure", {
  set.seed(0)
  ratio_ratio <- c(0.5, 0.2, 0.3, 0.4)
  ratio_levels <- 1:4
  ratio_matrix_outcome <- Matrix::Matrix(kronecker(rep(1, 3), diag(4)))
  disp_mean <- c(0.3, 0.2, 0.3, 0.2)
  disp_levels <- 1:4
  disp_matrix_outcome <- Matrix::Matrix(kronecker(rep(1, 3), diag(4)))
  datamod <- new_bage_datamod_exposure(ratio_ratio = ratio_ratio,
                                       ratio_levels = ratio_levels,
                                       ratio_matrix_outcome = ratio_matrix_outcome,
                                       disp_mean = disp_mean,
                                       disp_levels = disp_levels,
                                       disp_matrix_outcome = disp_matrix_outcome)
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
  ratio_ratio <- c(0.5, 0.2, 0.3, 0.4)
  ratio_levels <- 1:4
  ratio_matrix_outcome <- Matrix::Matrix(kronecker(rep(1, 3), diag(4)))
  disp_mean <- c(0.3, 0.2, 0.3, 0.2)
  disp_levels <- 1:4
  disp_matrix_outcome <- Matrix::Matrix(kronecker(rep(1, 3), diag(4)))
  datamod <- new_bage_datamod_exposure(ratio_ratio = ratio_ratio,
                                       ratio_levels = ratio_levels,
                                       ratio_matrix_outcome = ratio_matrix_outcome,
                                       disp_mean = disp_mean,
                                       disp_levels = disp_levels,
                                       disp_matrix_outcome = disp_matrix_outcome)
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

test_that("'draw_outcome_true_given_obs' works with bage_datamod_miscount - has disp", {
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
                                       rate_matrix_outcome = rate_matrix_outcome)
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


test_that("'draw_outcome_true_given_obs' works with bage_datamod_miscount - no disp", {
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
                                       rate_matrix_outcome = rate_matrix_outcome)
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
                                       rate_matrix_outcome = rate_matrix_outcome)
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

test_that("'draw_outcome_true_given_obs' works with bage_datamod_noise", {
  set.seed(0)
  mean_mean <- c(0.5, 0.2, 0.3, -0.4)
  mean_levels <- 1:4
  mean_matrix_outcome <- Matrix::Matrix(kronecker(rep(1, 3), diag(4)))
  sd_sd <- c(0.3, 0.3, 0.2)
  sd_levels <- 1:3
  sd_matrix_outcome <- Matrix::Matrix(kronecker(diag(3), rep(1, 4)))
  datamod <- new_bage_datamod_noise(mean_mean = mean_mean,
                                    mean_levels = mean_levels,
                                    mean_matrix_outcome = mean_matrix_outcome,
                                    sd = sd_sd,
                                    sd_levels = sd_levels,
                                    sd_matrix_outcome = sd_matrix_outcome)
  outcome <- rnorm(12)
  offset <- runif(12, max = 10)
  expected <- rvec::rnorm_rvec(12, n_draw = 10)
  disp <- rvec::runif_rvec(1)
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
    (tau2/(tau1+tau2)) * (outcome - get_datamod_mean(datamod))
  sd <- sqrt(1/(tau1+tau2))
  ans_expected <- rvec::rnorm_rvec(n = 12, mean = mean, sd = sd)
  expect_equal(ans_obtained, ans_expected)
})

test_that("'draw_outcome_true_given_obs' throws expected error with wrong distribution and bage_datamod_noise", {
  set.seed(0)
  mean_mean <- c(0.5, 0.2, 0.3, -0.4)
  mean_levels <- 1:4
  mean_matrix_outcome <- Matrix::Matrix(kronecker(rep(1, 3), diag(4)))
  sd_sd <- c(0.3, 0.3, 0.2)
  sd_levels <- 1:3
  sd_matrix_outcome <- Matrix::Matrix(kronecker(diag(3), rep(1, 4)))
  datamod <- new_bage_datamod_noise(mean_mean = mean_mean,
                                    mean_levels = mean_levels,
                                    mean_matrix_outcome = mean_matrix_outcome,
                                    sd = sd_sd,
                                    sd_levels = sd_levels,
                                    sd_matrix_outcome = sd_matrix_outcome)
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

test_that("'draw_outcome_true_given_obs' works with bage_datamod_overcount", {
  set.seed(0)
  rate_mean <- c(0.3, 0.3, 0.2)
  rate_disp <- c(0.5, 0.2, 0.6)
  rate_levels <- 1:3
  rate_matrix_outcome <- Matrix::Matrix(kronecker(diag(3), rep(1, 4)))
  datamod <- new_bage_datamod_overcount(rate_mean = rate_mean,
                                        rate_disp = rate_disp,
                                        rate_levels = rate_levels,
                                        rate_matrix_outcome = rate_matrix_outcome)
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

test_that("'draw_outcome_true_given_obs' throws expected error with wrong distribution and bage_datamod_overcount", {
  set.seed(0)
  rate_mean <- c(0.3, 0.3, 0.2)
  rate_disp <- c(0.5, 0.2, 0.6)
  rate_levels <- 1:3
  rate_matrix_outcome <- Matrix::Matrix(kronecker(diag(3), rep(1, 4)))
  datamod <- new_bage_datamod_overcount(rate_mean = rate_mean,
                                        rate_disp = rate_disp,
                                        rate_levels = rate_levels,
                                        rate_matrix_outcome = rate_matrix_outcome)
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

test_that("'draw_outcome_true_given_obs' works with bage_datamod_undercount - Poisson - has disp", {
  set.seed(0)
  prob_mean <- c(0.5, 0.2, 0.3, 0.4)
  prob_disp <- rep(0.4, 4)
  prob_levels <- 1:4
  prob_matrix_outcome <- Matrix::Matrix(kronecker(rep(1, 3), diag(4)))
  datamod <- new_bage_datamod_undercount(prob_mean = prob_mean,
                                       prob_disp = prob_disp,
                                       prob_levels = prob_levels,
                                       prob_matrix_outcome = prob_matrix_outcome)
  components <- tibble::tibble(
    term = c("(Intercept)", rep("datamod", 4)),
    component = c("(Intercept)", rep("prob", times = 4)),
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
  pi <- rep(components$.fitted[2:5], times = 3)
  w <- rvec::rnbinom_rvec(n = 12,
                          size = outcome + (1/disp),
                          prob = (1 + pi * expected * offset * disp) / (1 + expected * offset * disp))
  ans_expected <- outcome + w
  expect_equal(ans_obtained, ans_expected)
})

test_that("'draw_outcome_true_given_obs' works with bage_datamod_undercount - Poisson - no disp", {
  set.seed(0)
  prob_mean <- c(0.5, 0.2, 0.3, 0.4)
  prob_disp <- rep(0.4, 4)
  prob_levels <- 1:4
  prob_matrix_outcome <- Matrix::Matrix(kronecker(rep(1, 3), diag(4)))
  datamod <- new_bage_datamod_undercount(prob_mean = prob_mean,
                                       prob_disp = prob_disp,
                                       prob_levels = prob_levels,
                                       prob_matrix_outcome = prob_matrix_outcome)
  components <- tibble::tibble(
    term = c("(Intercept)", rep("datamod", 4)),
    component = c("(Intercept)", rep("prob", times = 4)),
    level = c("(Intercept)", 0:3),
    .fitted = rvec::runif_rvec(n = 5, n_draw = 10)
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
  w <- rvec::rpois_rvec(n = 12,
                        lambda = (1 - pi)* expected * offset)
  ans_expected <- outcome + w
  expect_equal(ans_obtained, ans_expected)
})

test_that("'draw_outcome_true_given_obs' works with bage_datamod_undercount - binomial - has disp", {
  set.seed(0)
  prob_mean <- c(0.5, 0.2, 0.3, 0.4)
  prob_disp <- rep(0.4, 4)
  prob_levels <- 1:4
  prob_matrix_outcome <- Matrix::Matrix(kronecker(rep(1, 3), diag(4)))
  datamod <- new_bage_datamod_undercount(prob_mean = prob_mean,
                                         prob_disp = prob_disp,
                                         prob_levels = prob_levels,
                                         prob_matrix_outcome = prob_matrix_outcome)
  components <- tibble::tibble(
    term = c("(Intercept)", rep("datamod", 4)),
    component = c("(Intercept)", rep("prob", times = 4)),
    level = c("(Intercept)", 0:3),
    .fitted = rvec::runif_rvec(n = 5, n_draw = 10)
  )
  outcome <- rpois(12, lambda = 5)
  offset <- rpois(12, lambda = 100)
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
  ans_expected <- draw_outcome_true_binom_betabinom(outcome = outcome,
                                                    offset = offset,
                                                    expected = expected,
                                                    disp = disp,
                                                    prob = pi)
  expect_equal(ans_obtained, ans_expected)
})

test_that("'draw_outcome_true_given_obs' works with bage_datamod_undercount - binomial - no disp", {
  set.seed(0)
  prob_mean <- c(0.5, 0.2, 0.3, 0.4)
  prob_disp <- rep(0.4, 4)
  prob_levels <- 1:4
  prob_matrix_outcome <- Matrix::Matrix(kronecker(rep(1, 3), diag(4)))
  datamod <- new_bage_datamod_undercount(prob_mean = prob_mean,
                                       prob_disp = prob_disp,
                                       prob_levels = prob_levels,
                                       prob_matrix_outcome = prob_matrix_outcome)
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
  ans_obtained <- draw_outcome_true_given_obs(datamod = datamod,
                                              nm_distn = "binom",
                                              components = components,
                                              outcome = outcome,
                                              offset = offset,
                                              expected = expected,
                                              disp = NULL)
  set.seed(1)
  pi <- rep(components$.fitted[2:5], times = 3)
  w <- rvec::rbinom_rvec(n = 12,
                         size = offset - outcome,
                         prob = (1-pi) * expected / (1-pi*expected))
  ans_expected <- outcome + w
  expect_equal(ans_obtained, ans_expected)
})

test_that("'draw_outcome_true_given_obs' gives expected with bage_datamod_undercount - normal", {
  set.seed(0)
  prob_mean <- c(0.5, 0.2, 0.3, 0.4)
  prob_disp <- rep(0.4, 4)
  prob_levels <- 1:4
  prob_matrix_outcome <- Matrix::Matrix(kronecker(rep(1, 3), diag(4)))
  datamod <- new_bage_datamod_undercount(prob_mean = prob_mean,
                                         prob_disp = prob_disp,
                                         prob_levels = prob_levels,
                                         prob_matrix_outcome = prob_matrix_outcome)
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


## 'make_expected_obs_exposure' -----------------------------------------------

test_that("'make_expected_obs_exposure' works", {
  ratio_ratio <- c(0.5, 0.2, 0.3, 0.4)
  ratio_levels <- 1:4
  ratio_matrix_outcome <- Matrix::Matrix(kronecker(rep(1, 3), diag(4)))
  disp_mean <- c(0.3, 0.2, 0.3, 0.2)
  disp_levels <- 1:4
  disp_matrix_outcome <- Matrix::Matrix(kronecker(rep(1, 3), diag(4)))
  datamod <- new_bage_datamod_exposure(ratio_ratio = ratio_ratio,
                                       ratio_levels = ratio_levels,
                                       ratio_matrix_outcome = ratio_matrix_outcome,
                                       disp_mean = disp_mean,
                                       disp_levels = disp_levels,
                                       disp_matrix_outcome = disp_matrix_outcome)
  components <- tibble::tibble(
    term = c("(Intercept)", rep("datamod", 4)),
    component = c("(Intercept)", rep("disp", 4)),
    level = c("(Intercept)", 0:3),
    .fitted = rvec::runif_rvec(n = 5, n_draw = 10)
  )
  expected <- exp(rvec::rnorm_rvec(n = 12, n_draw = 10))
  ans_obtained <- make_expected_obs_exposure(datamod = datamod,
                                             components = components,
                                             expected = expected)
  ratio <- as.numeric(ratio_matrix_outcome %*% ratio_ratio)
  disp <- as.matrix(disp_matrix_outcome) %*% components$.fitted[-1]
  ans_expected <- ((3 + 1/disp)/(1 + 1/disp)) * (expected / ratio)
  expect_equal(ans_obtained, ans_expected)
})





## 'draw_outcome_true_binom_betabinom' ----------------------------------------

test_that("'draw_outcome_true_binom_betabinom' works with valid inputs", {
  set.seed(0)
  outcome <- rpois(12, lambda = 5)
  offset <- rpois(12, lambda = 100) + 1
  expected <- rvec::runif_rvec(12, n_draw = 10)
  disp <- rvec::runif_rvec(1, n_draw = 10)
  prob <- rvec::runif_rvec(n = 12, n_draw = 10)
  set.seed(1)
  ans <- draw_outcome_true_binom_betabinom(outcome = outcome,
                                           offset = offset,
                                           expected = expected,
                                           disp = disp,
                                           prob = prob)
  expect_true(rvec::is_rvec(ans))
  expect_true(all(draws_all(ans <= offset)))
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
                                       rate_matrix_outcome = rate_matrix_outcome)
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


## 'make_expected_obs_overcount' ----------------------------------------------

test_that("'make_expected_obs_overcount' works", {
  rate_mean <- c(0.5, 0.2, 0.3, 0.4)
  rate_disp <- c(0.3, 0.2, 0.3, 0.2)
  rate_levels <- 1:4
  rate_matrix_outcome <- Matrix::Matrix(kronecker(rep(1, 3), diag(4)))
  datamod <- new_bage_datamod_overcount(rate_mean = rate_mean,
                                        rate_disp = rate_disp,
                                        rate_levels = rate_levels,
                                        rate_matrix_outcome = rate_matrix_outcome)
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
                                         prob_matrix_outcome = prob_matrix_outcome)
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



## OLD ################################################################


## 'draw_vals_outcome_true' ---------------------------------------------------

test_that("'draw_vals_outcome_true' works with pois, NULL, offset has NA", {
  set.seed(0)
  n_sim <- 10
  data <- expand.grid(age = 0:9, time = 2000:2005, sex = c("F", "M"))
  data$popn <- rpois(n = nrow(data), lambda = 100)
  data$deaths <- rpois(n = nrow(data), lambda = 0.5 * data$popn)
  data$deaths[3] <- NA
  data$popn[3] <- NA
  formula <- deaths ~ age + sex + time
  mod <- mod_pois(formula = formula,
                  data = data,
                  exposure = popn)
  vals_components <- draw_vals_components_unfitted(mod = mod,
                                                   n_sim = n_sim)
  vals_disp <- vals_components$.fitted[vals_components$component == "disp"]
  vals_expected <- exp(make_linpred_from_components(mod = mod,
                                                    components = vals_components,
                                                    data = mod$data,
                                                    dimnames_term = mod$dimnames_terms))
  vals_fitted <- draw_vals_fitted(mod = mod,
                                  vals_expected = vals_expected,
                                  vals_disp = vals_disp,
                                  outcome = NULL,
                                  offset = NULL)
  set.seed(1)
  ans_obtained <- draw_vals_outcome_true(datamod = NULL,
                                         nm_distn = "pois",
                                         outcome_obs = mod$outcome,
                                         fitted = vals_fitted,
                                         disp = vals_disp,
                                         offset = mod$offset)
  set.seed(1)
  ans_expected <- rvec::rvec(matrix(1 * data$deaths, nrow = nrow(data), ncol = 10))
  ans_expected[3] <- NA
  expect_equal(ans_obtained, ans_expected)
})

test_that("'draw_vals_outcome_true' works with NULL, binom, data complete", {
  set.seed(0)
  n_sim <- 10
  data <- expand.grid(age = 0:9, time = 2000:2005, sex = c("F", "M"))
  data$popn <- rpois(n = nrow(data), lambda = 20)
  data$deaths <- rbinom(n = nrow(data), size = data$popn, prob = 0.8)
  formula <- deaths ~ age + sex + time
  mod <- mod_binom(formula = formula,
                   data = data,
                   size = popn)
  vals_components <- draw_vals_components_unfitted(mod = mod,
                                                   n_sim = n_sim)
  vals_disp <- vals_components$.fitted[vals_components$component == "disp"]
  invlogit <- function(x) exp(x) / (1 + exp(x))
  vals_expected <- invlogit(make_linpred_from_components(mod = mod,
                                                         components = vals_components,
                                                         data = mod$data,
                                                         dimnames_term = mod$dimnames_terms))
  vals_fitted <- draw_vals_fitted(mod = mod,
                                  vals_expected = vals_expected,
                                  vals_disp = vals_disp,
                                  outcome = NULL,
                                  offset = NULL)
  set.seed(1)
  ans_obtained <- draw_vals_outcome_true(datamod = NULL,
                                         nm_distn = "binom",
                                         outcome_obs = mod$outcome,
                                         fitted = vals_fitted,
                                         disp = vals_disp,
                                         offset = mod$offset)
  set.seed(1)
  ans_expected <- rvec::rvec(matrix(1*data$deaths, nrow = nrow(data), ncol = 10))
  expect_equal(ans_obtained, ans_expected)
})

test_that("'draw_vals_outcome_true' works with NULL, binom, has offset has na", {
  set.seed(0)
  n_sim <- 10
  data <- expand.grid(age = 0:9, time = 2000:2005, sex = c("F", "M"))
  data$popn <- rpois(n = nrow(data), lambda = 20)
  data$deaths <- rbinom(n = nrow(data), size = data$popn, prob = 0.8)
  data$deaths[1] <- NA
  data$popn[3] <- NA
  formula <- deaths ~ age + sex + time
  mod <- mod_binom(formula = formula,
                   data = data,
                   size = popn)
  vals_components <- draw_vals_components_unfitted(mod = mod,
                                                   n_sim = n_sim)
  vals_disp <- vals_components$.fitted[vals_components$component == "disp"]
  invlogit <- function(x) exp(x) / (1 + exp(x))
  vals_expected <- invlogit(make_linpred_from_components(mod = mod,
                                                         components = vals_components,
                                                         data = mod$data,
                                                         dimnames_term = mod$dimnames_terms))
  vals_fitted <- draw_vals_fitted(mod = mod,
                                  vals_expected = vals_expected,
                                  vals_disp = vals_disp,
                                  outcome = NULL,
                                  offset = NULL)
  set.seed(1)
  ans_obtained <- draw_vals_outcome_true(datamod = NULL,
                                         nm_distn = "binom",
                                         outcome_obs = mod$outcome,
                                         fitted = vals_fitted,
                                         disp = vals_disp,
                                         offset = mod$offset)
  set.seed(1)
  ans_expected <- rvec::rvec(matrix(1*data$deaths, nrow = nrow(data), ncol = 10))
  ans_expected[1] <- rvec::rbinom_rvec(n = 1 ,
                                       size = mod$offset[1],
                                       prob = vals_fitted[1])
  expect_equal(ans_obtained, ans_expected)
})

test_that("'draw_vals_outcome_true' works with NULL, norm, no na", {
  set.seed(0)
  n_sim <- 10
  data <- expand.grid(age = 0:9, time = 2000:2005, sex = c("F", "M"))
  data$income <- rnorm(n = nrow(data), mean = 20, sd = 3)
  data$income[1] <- NA
  data$wt <- rpois(n = nrow(data), lambda = 100)
  formula <- income ~ age + sex + time
  mod <- mod_norm(formula = formula,
                  data = data,
                  weights = wt)
  vals_components <- draw_vals_components_unfitted(mod = mod,
                                                   n_sim = n_sim)
  vals_disp <- vals_components$.fitted[vals_components$component == "disp"]
  scale_linpred <- get_fun_orig_scale_linpred(mod)
  vals_fitted <- scale_linpred(make_linpred_from_components(mod = mod,
                                                            components = vals_components,
                                                            data = mod$data,
                                                            dimnames_term = mod$dimnames_terms))
  set.seed(1)
  ans_obtained <- draw_vals_outcome_true(datamod = NULL,
                                         nm_distn = "norm",
                                         outcome_obs = scale_linpred(mod$outcome),
                                         fitted = vals_fitted,
                                         disp = mod$outcome_sd * vals_disp,
                                         offset = mod$offset)
  set.seed(1)
  ans_expected <- rvec::rvec(matrix(data$income, nrow = nrow(data), ncol = 10))
  ans_expected[1] <- rvec::rnorm_rvec(n = 1 ,
                                      mean = vals_fitted[1],
                                      sd = mod$outcome_sd * vals_disp / sqrt(mod$offset[1]))
  expect_equal(ans_obtained, ans_expected)
})

test_that("'draw_vals_outcome_true' works with NULL, norm, has NA", {
  set.seed(0)
  n_sim <- 10
  data <- expand.grid(age = 0:9, time = 2000:2005, sex = c("F", "M"))
  data$income <- rnorm(n = nrow(data), mean = 20, sd = 3)
  data$wt <- rpois(n = nrow(data), lambda = 100)
  data$income[3] <- NA
  data$wt[3] <- NA
  formula <- income ~ age + sex + time
  mod <- mod_norm(formula = formula,
                  data = data,
                  weights = wt)
  vals_components <- draw_vals_components_unfitted(mod = mod,
                                                   n_sim = n_sim)
  vals_disp <- vals_components$.fitted[vals_components$component == "disp"]
  vals_disp <- mod$outcome_sd * vals_disp
  scale_linpred <- get_fun_orig_scale_linpred(mod)
  vals_fitted <- scale_linpred(make_linpred_from_components(mod = mod,
                                                            components = vals_components,
                                                            data = mod$data,
                                                            dimnames_term = mod$dimnames_terms))
  set.seed(1)
  ans_obtained <- draw_vals_outcome_true(datamod = NULL,
                                         nm_distn = "norm",
                                         outcome_obs = scale_linpred(mod$outcome),
                                         fitted = vals_fitted,
                                         disp = vals_disp,
                                         offset = mod$offset)
  set.seed(1)
  ans_expected <- rvec::rvec(matrix(data$income, nrow = nrow(data), ncol = 10))
  ans_expected[3]<- NA
  expect_equal(ans_obtained, ans_expected)
})

test_that("'draw_vals_outcome_true' method for NULL throws correct error with invalid nm_distn", {
  set.seed(0)
  n_sim <- 10
  data <- expand.grid(age = 0:9, time = 2000:2005, sex = c("F", "M"))
  data$deaths <- rpois(n = nrow(data), lambda = 20)
  data$deaths[c(1, 5, 10)] <- NA
  formula <- deaths ~ age + sex + time
  mod <- mod_pois(formula = formula,
                  data = data,
                  exposure = 1)
  vals_components <- draw_vals_components_unfitted(mod = mod,
                                                   n_sim = n_sim)
  vals_disp <- vals_components$.fitted[vals_components$component == "disp"]
  vals_expected <- exp(make_linpred_from_components(mod = mod,
                                                    components = vals_components,
                                                    data = mod$data,
                                                    dimnames_term = mod$dimnames_terms))
  vals_fitted <- draw_vals_fitted(mod = mod,
                                  vals_expected = vals_expected,
                                  vals_disp = vals_disp,
                                  outcome = NULL,
                                  offset = NULL)
  set.seed(1)
  expect_error(draw_vals_outcome_true(datamod = NULL,
                                      nm_distn = "wrong",
                                      outcome_obs = mod$outcome,
                                      fitted = vals_fitted,
                                      disp = vals_disp,
                                      offset = mod$offset),
               "Internal error: Invalid value for `nm_distn`.")
})



## 'make_i_lik' ---------------------------------------------------------------

test_that("'make_i_lik' works with bage_datamod_outcome_rr3", {
  x <- new_bage_datamod_outcome_rr3()
  expect_identical(make_i_lik(x, nm_distn = "binom", has_disp = FALSE), 102L)
  expect_identical(make_i_lik(x, nm_distn = "pois", has_disp = FALSE), 302L)
  expect_identical(make_i_lik(x, nm_distn = "binom", has_disp = TRUE), 104L)
  expect_identical(make_i_lik(x, nm_distn = "pois", has_disp = TRUE), 304L)
  expect_error(make_i_lik(x, nm_distn = "wrong", has_disp = TRUE),
               "Internal error: Invalid inputs.")
})


## 'str_call_datamod' ----------------------------------------------------------------------

test_that("'str_call_datamod' works", {
  expect_identical(str_call_datamod(new_bage_datamod_outcome_rr3()),
                   "rr3()")
})



  
  

