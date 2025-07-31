
## 'draw_outcome_obs' ---------------------------------------------------------

test_that("'draw_outcome_obs' works with 'bage_confidential_rr3'", {
  set.seed(0)
  confidential <- new_bage_confidential_rr3()
  outcome_true <- rvec::rpois_rvec(n = 5, lambda = 10, n_draw = 5)
  set.seed(1)
  ans_obtained <- draw_outcome_obs(confidential = confidential,
                                   outcome_true = outcome_true)
  set.seed(1)
  ans_expected <- poputils::rr3(outcome_true)
  expect_identical(ans_obtained, ans_expected)
})


## 'make_i_lik_part' ----------------------------------------------------------

test_that("'make_i_lik_part' works with bage_confidential_rr3", {
  x <- new_bage_confidential_rr3()
  expect_identical(make_i_lik_part(x), 1L)
})


## 'str_call_confidential' ----------------------------------------------------

test_that("'str_call_confidential' works", {
  expect_identical(str_call_confidential(new_bage_confidential_rr3()),
                   "rr3()")
})



  
  

