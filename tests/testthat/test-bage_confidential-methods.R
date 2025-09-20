
## 'draw_outcome_confidential' ------------------------------------------------

test_that("'draw_outcome_confidential' works with 'bage_confidential_rr3'", {
  set.seed(0)
  confidential <- new_bage_confidential_rr3()
  outcome_obs <- rvec::rpois_rvec(n = 5, lambda = 10, n_draw = 5)
  set.seed(1)
  ans_obtained <- draw_outcome_confidential(confidential = confidential,
                                            outcome_obs = outcome_obs)
  set.seed(1)
  ans_expected <- poputils::rr3(outcome_obs)
  expect_identical(ans_obtained, ans_expected)
})


## 'draw_outcome_obs_given_conf' ----------------------------------------------

test_that("'draw_outcome_obs_given_conf' works with rr3, pois, no na, has disp", {
  set.seed(0)
  confidential <- new_bage_confidential_rr3()
  outcome_obs <- rpois(n = 100, lambda = 50)
  outcome_conf <- poputils::rr3(outcome_obs)
  offset <- rep(10, 100)
  expected_obs <- rvec::rgamma_rvec(n = 100,
                                    shape = 2,
                                    rate = 0.4,
                                    n_draw = 100)
  disp_obs <- rvec::runif_rvec(n = 100, n_draw = 100)
  ans <- draw_outcome_obs_given_conf(confidential = confidential,
                                     nm_distn = "pois",
                                     outcome_conf = outcome_conf,
                                     offset = offset,
                                     expected_obs = expected_obs,
                                     disp_obs = disp_obs,
                                     sd_obs = NULL)
  expect_true(all(abs(as.matrix(ans) - outcome_conf) <= 2L))
  expect_equal(mean(as.numeric(ans)), 50, tolerance = 0.05)
})

test_that("'draw_outcome_obs_given_conf' works with rr3, pois, no na, no disp", {
  set.seed(0)
  confidential <- new_bage_confidential_rr3()
  outcome_obs <- rpois(n = 100, lambda = 50)
  outcome_conf <- poputils::rr3(outcome_obs)
  offset <- rep(10, 100)
  expected_obs <- rvec::rgamma_rvec(n = 100,
                                    shape = 2,
                                    rate = 0.4,
                                    n_draw = 100)
  ans <- draw_outcome_obs_given_conf(confidential = confidential,
                                     nm_distn = "pois",
                                     outcome_conf = outcome_conf,
                                     offset = offset,
                                     expected_obs = expected_obs,
                                     disp_obs = NULL,
                                     sd_obs = NULL)
  expect_true(all(abs(as.matrix(ans) - outcome_conf) <= 2L))
  expect_equal(mean(as.numeric(ans)), 50, tolerance = 0.05)
})

test_that("'draw_outcome_obs_given_conf' works with rr3, pois, has na, has disp", {
  set.seed(0)
  confidential <- new_bage_confidential_rr3()
  outcome_obs <- rpois(n = 100, lambda = 50)
  outcome_conf <- poputils::rr3(outcome_obs)
  outcome_conf[4] <- NA
  offset <- rep(10, 100)
  offset[1] <- NA
  expected_obs <- rvec::rgamma_rvec(n = 100,
                                    shape = 2,
                                    rate = 0.4,
                                    n_draw = 100)
  disp_obs <- rvec::runif_rvec(n = 100, n_draw = 100)
  ans <- draw_outcome_obs_given_conf(confidential = confidential,
                                     nm_distn = "pois",
                                     outcome_conf = outcome_conf,
                                     offset = offset,
                                     expected_obs = expected_obs,
                                     disp_obs = disp_obs,
                                     sd_obs = NULL)
  expect_true(all(abs(as.matrix(ans[-c(1, 4)]) - outcome_conf[-c(1, 4)]) <= 2L))
  expect_equal(mean(as.numeric(ans), na.rm = TRUE), 50, tolerance = 0.01)
})

test_that("'draw_outcome_obs_given_conf' works with rr3, pois, has na, no disp", {
  set.seed(0)
  confidential <- new_bage_confidential_rr3()
  outcome_obs <- rpois(n = 100, lambda = 50)
  outcome_conf <- poputils::rr3(outcome_obs)
  outcome_conf[4] <- NA
  offset <- rep(10, 100)
  offset[1] <- NA
  expected_obs <- rvec::rgamma_rvec(n = 100,
                                    shape = 2,
                                    rate = 0.4,
                                    n_draw = 100)
  ans <- draw_outcome_obs_given_conf(confidential = confidential,
                                     nm_distn = "pois",
                                     outcome_conf = outcome_conf,
                                     offset = offset,
                                     expected_obs = expected_obs,
                                     disp_obs = NULL,
                                     sd_obs = NULL)
  expect_true(all(abs(as.matrix(ans[-c(1, 4)]) - outcome_conf[-c(1, 4)]) <= 2L))
  expect_equal(mean(as.numeric(ans), na.rm = TRUE), 50, tolerance = 0.01)
})

test_that("'draw_outcome_obs_given_conf' works with rr3, pois, noise datamod", {
  set.seed(0)
  confidential <- new_bage_confidential_rr3()
  outcome_obs <- rpois(n = 100, lambda = 50)
  outcome_conf <- poputils::rr3(outcome_obs)
  offset <- rep(10, 100)
  expected_obs <- rvec::rgamma_rvec(n = 100,
                                    shape = 2,
                                    rate = 0.4,
                                    n_draw = 100)
  ans <- draw_outcome_obs_given_conf(confidential = confidential,
                                     nm_distn = "pois",
                                     outcome_conf = outcome_conf,
                                     offset = offset,
                                     expected_obs = expected_obs,
                                     disp_obs = NULL,
                                     sd_obs = rep(3, 100))
  expect_true(all(as.numeric(ans - outcome_conf) <= 2))
  expect_equal(mean(as.numeric(ans)), 50, tolerance = 0.01)
})

test_that("'draw_outcome_obs_given_conf' works with rr3, binom, no na, has disp", {
  set.seed(0)
  confidential <- new_bage_confidential_rr3()
  outcome_obs <- rbinom(n = 100, size = 100, prob = 0.5)
  outcome_conf <- poputils::rr3(outcome_obs)
  offset <- rep(100, 100)
  expected_obs <- rvec::rbeta_rvec(n = 100,
                                   shape1 = 10,
                                   shape2 = 10,
                                   n_draw = 100)
  disp_obs <- rvec::runif_rvec(n = 100, n_draw = 100)
  ans <- draw_outcome_obs_given_conf(confidential = confidential,
                                     nm_distn = "binom",
                                     outcome_conf = outcome_conf,
                                     offset = offset,
                                     expected_obs = expected_obs,
                                     disp_obs = disp_obs,
                                     sd_obs = NULL)
  expect_true(all(abs(as.matrix(ans) - outcome_conf) <= 2L))
  ans <- rvec::draws_mean(ans)
  expect_equal(mean(ans), 50, tolerance = 0.01)
})

test_that("'draw_outcome_obs_given_conf' works with rr3, binom, no na, no disp", {
  set.seed(0)
  confidential <- new_bage_confidential_rr3()
  outcome_obs <- rbinom(n = 100, size = 100, prob = 0.5)
  outcome_conf <- poputils::rr3(outcome_obs)
  offset <- rep(100, 100)
  expected_obs <- rvec::rbeta_rvec(n = 100,
                                   shape1 = 10,
                                   shape2 = 10,
                                   n_draw = 100)
  disp_obs <- rvec::runif_rvec(n = 100, n_draw = 100)
  ans <- draw_outcome_obs_given_conf(confidential = confidential,
                                     nm_distn = "binom",
                                     outcome_conf = outcome_conf,
                                     offset = offset,
                                     expected_obs = expected_obs,
                                     disp_obs = NULL,
                                     sd_obs = NULL)
  expect_true(all(abs(as.matrix(ans) - outcome_conf) <= 2L))
  ans <- rvec::draws_mean(ans)
  expect_equal(mean(ans), 50, tolerance = 0.01)
})

test_that("'draw_outcome_obs_given_conf' works with rr3, binom, has na, has disp", {
  set.seed(0)
  confidential <- new_bage_confidential_rr3()
  outcome_obs <- rbinom(n = 100, size = 100, prob = 0.5)
  outcome_obs[4] <- NA
  outcome_conf <- poputils::rr3(outcome_obs)
  offset <- rep(100, 100)
  offset[1] <- NA
  expected_obs <- rvec::rbeta_rvec(n = 100,
                                   shape1 = 10,
                                   shape2 = 10,
                                   n_draw = 100)
  disp_obs <- rvec::runif_rvec(n = 100, n_draw = 100)
  ans <- draw_outcome_obs_given_conf(confidential = confidential,
                                     nm_distn = "binom",
                                     outcome_conf = outcome_conf,
                                     offset = offset,
                                     expected_obs = expected_obs,
                                     disp_obs = disp_obs,
                                     sd_obs = NULL)
  expect_true(all(abs(as.matrix(ans[-c(1, 4)]) - outcome_conf[-c(1, 4)]) <= 2L))
  ans <- rvec::draws_mean(ans)
  expect_equal(mean(ans, na.rm = TRUE), 50, tolerance = 0.01)
})

test_that("'draw_outcome_obs_given_conf' works with rr3, binom, has na, no disp", {
  set.seed(0)
  confidential <- new_bage_confidential_rr3()
  outcome_obs <- rbinom(n = 100, size = 100, prob = 0.5)
  outcome_obs[4] <- NA
  outcome_conf <- poputils::rr3(outcome_obs)
  offset <- rep(100, 100)
  offset[1] <- NA
  expected_obs <- rvec::rbeta_rvec(n = 100,
                                   shape1 = 10,
                                   shape2 = 10,
                                   n_draw = 100)
  disp_obs <- rvec::runif_rvec(n = 100, n_draw = 100)
  ans <- draw_outcome_obs_given_conf(confidential = confidential,
                                     nm_distn = "binom",
                                     outcome_conf = outcome_conf,
                                     offset = offset,
                                     expected_obs = expected_obs,
                                     disp_obs = NULL,
                                     sd_obs = NULL)
  expect_true(all(abs(as.matrix(ans[-c(1, 4)]) - outcome_conf[-c(1, 4)]) <= 2L))
  ans <- rvec::draws_mean(ans)
  expect_equal(mean(ans, na.rm = TRUE), 50, tolerance = 0.01)
})

test_that("'draw_outcome_obs_given_conf' throws appropriate error with invalid nm_distn", {
  set.seed(0)
  set.seed(0)
  confidential <- new_bage_confidential_rr3()
  outcome_obs <- rbinom(n = 100, size = 100, prob = 0.5)
  outcome_obs[4] <- NA
  outcome_conf <- poputils::rr3(outcome_obs)
  offset <- rep(100, 100)
  offset[1] <- NA
  disp_obs <- rvec::runif_rvec(n = 100, n_draw = 100)
  expected_obs <- rvec::rbeta_rvec(n = 100,
                                   shape1 = 10,
                                   shape2 = 10,
                                   n_draw = 100)
  expect_error(draw_outcome_obs_given_conf(confidential = confidential,
                                           nm_distn = "wrong",
                                           outcome_conf = outcome_conf,
                                           offset = offset,
                                           expected_obs = expected_obs,
                                           disp_obs = disp_obs,
                                           sd_obs = NULL),
               "Internal error: Invalid value for `nm_distn`.")
  expect_error(draw_outcome_obs_given_conf(confidential = confidential,
                                           nm_distn = "wrong",
                                           outcome_conf = outcome_conf,
                                           offset = offset,
                                           expected_obs = expected_obs,
                                           disp_obs = NULL,
                                           sd_obs = NULL),
               "Internal error: Invalid value for `nm_distn`.")
})


## 'make_i_lik_part' ----------------------------------------------------------

test_that("'make_i_lik_part' works with bage_confidential_rr3", {
  x <- new_bage_confidential_rr3()
  expect_identical(make_i_lik_part(x), 10L)
})


## 'str_call_confidential' ----------------------------------------------------

test_that("'str_call_confidential' works", {
  expect_identical(str_call_confidential(new_bage_confidential_rr3()),
                   "rr3")
})



  
  

