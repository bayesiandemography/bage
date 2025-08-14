
## 'bage_datamod_exposure' ----------------------------------------------------

test_that("'new_bage_datamod_exposure' works", {
  ratio <- c(0.1, 0.4, 0.2)
  disp_mean <- c(0.5, 0.2, 0.3, 0.4)
  matrix_ratio_outcome <- Matrix::Matrix(kronecker(diag(3), rep(1, 4)))
  matrix_disp_outcome <- Matrix::Matrix(kronecker(rep(1, 3), diag(4)))
  x <- new_bage_datamod_exposure(ratio = ratio,
                                 disp_mean = disp_mean,
                                 matrix_ratio_outcome = matrix_ratio_outcome,
                                 matrix_disp_outcome = matrix_disp_outcome)
  expect_s3_class(x, "bage_datamod_exposure")
  expect_s3_class(x, "bage_datamod_offset")
})


## 'bage_datamod_miscount' ----------------------------------------------------

test_that("'new_bage_datamod_miscount' works", {
  prob_mean <- c(0.1, 0.4, 0.2)
  prob_disp <- c(0.3, 0.4, 1)
  rate_mean <- c(0.5, 0.2, 0.3, 0.4)
  rate_disp <- c(2, 0.2, 0.3, 0.1)
  matrix_prob_outcome <- Matrix::Matrix(kronecker(diag(3), rep(1, 4)))
  matrix_rate_outcome <- Matrix::Matrix(kronecker(rep(1, 3), diag(4)))
  x <- new_bage_datamod_miscount(prob_mean = prob_mean,
                                 prob_disp = prob_disp,
                                 rate_mean = rate_mean,
                                 rate_disp = rate_disp,
                                 matrix_prob_outcome = matrix_prob_outcome,
                                 matrix_rate_outcome = matrix_rate_outcome)
  expect_s3_class(x, "bage_datamod_miscount")
  expect_s3_class(x, "bage_datamod_outcome")
})


## 'bage_datamod_noise' -------------------------------------------------------

test_that("'new_bage_datamod_noise' works", {
  mean <- c(0.1, 0.4, 0.2)
  sd <- c(0.3, 0.4, 1)
  matrix_mean_outcome <- Matrix::Matrix(kronecker(diag(3), rep(1, 4)))
  matrix_sd_outcome <- Matrix::Matrix(kronecker(rep(1, 3), diag(4)))
  x <- new_bage_datamod_noise(mean = mean,
                              sd = sd,
                              matrix_mean_outcome = matrix_mean_outcome,
                              matrix_sd_outcome = matrix_sd_outcome)
  expect_s3_class(x, "bage_datamod_noise")
  expect_s3_class(x, "bage_datamod_outcome")
})


## 'bage_datamod_overcount' ---------------------------------------------------

test_that("'new_bage_datamod_overcount' works", {
  rate_mean <- c(0.5, 0.2, 0.3, 0.4)
  rate_disp <- c(2, 0.2, 0.3, 0.1)
  matrix_rate_outcome <- Matrix::Matrix(kronecker(rep(1, 3), diag(4)))
  x <- new_bage_datamod_overcount(rate_mean = rate_mean,
                                  rate_disp = rate_disp,
                                  matrix_rate_outcome = matrix_rate_outcome)
  expect_s3_class(x, "bage_datamod_overcount")
  expect_s3_class(x, "bage_datamod_outcome")
})


## 'bage_datamod_undercount' --------------------------------------------------

test_that("'new_bage_datamod_undercount' works", {
  prob_mean <- c(0.1, 0.4, 0.2)
  prob_disp <- c(0.3, 0.4, 1)
  matrix_prob_outcome <- Matrix::Matrix(kronecker(diag(3), rep(1, 4)))
  x <- new_bage_datamod_undercount(prob_mean = prob_mean,
                                   prob_disp = prob_disp,
                                   matrix_prob_outcome = matrix_prob_outcome)
  expect_s3_class(x, "bage_datamod_undercount")
  expect_s3_class(x, "bage_datamod_outcome")
})


  
