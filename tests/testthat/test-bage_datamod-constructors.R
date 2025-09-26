
## 'bage_datamod_exposure' ----------------------------------------------------

test_that("'new_bage_datamod_exposure' works", {
  disp <- c(0.5, 0.2, 0.3, 0.4)
  disp_levels <- 1:4
  disp_matrix_outcome <- Matrix::Matrix(kronecker(rep(1, 3), diag(4)))
  nms_by <- "age"
  cv <- data.frame(age = 0:3, cd = sqrt(disp))
  x <- new_bage_datamod_exposure(disp = disp,
                                 disp_levels = disp_levels,
                                 disp_matrix_outcome = disp_matrix_outcome,
                                 cv_arg = cv,
                                 nms_by = nms_by)
  expect_s3_class(x, "bage_datamod_exposure")
  expect_s3_class(x, "bage_datamod_offset")
})


## 'bage_datamod_miscount' ----------------------------------------------------

test_that("'new_bage_datamod_miscount' works", {
  prob_mean <- c(0.1, 0.4, 0.2)
  prob_disp <- c(0.3, 0.4, 1)
  prob_levels <- c("a", "b", "c")
  prob_matrix_outcome <- Matrix::Matrix(kronecker(diag(3), rep(1, 4)))
  rate_mean <- c(0.5, 0.2, 0.3, 0.4)
  rate_disp <- c(2, 0.2, 0.3, 0.1)
  rate_levels <- 1:4
  rate_matrix_outcome <- Matrix::Matrix(kronecker(rep(1, 3), diag(4)))
  nms_by <- c("region", "age")
  prob <- data.frame(region = prob_levels, mean = prob_mean, disp = prob_disp)
  rate <- data.frame(age = rate_levels, mean = rate_mean, disp = rate_disp)
  x <- new_bage_datamod_miscount(prob_mean = prob_mean,
                                 prob_disp = prob_disp,
                                 prob_levels = prob_levels,
                                 prob_matrix_outcome = prob_matrix_outcome,                                 
                                 rate_mean = rate_mean,
                                 rate_disp = rate_disp,
                                 rate_levels = rate_levels,
                                 rate_matrix_outcome = rate_matrix_outcome,
                                 prob_arg = prob,
                                 rate_arg = rate,
                                 nms_by = nms_by)
  expect_s3_class(x, "bage_datamod_miscount")
  expect_s3_class(x, "bage_datamod_outcome")
})


## 'bage_datamod_noise' -------------------------------------------------------

test_that("'new_bage_datamod_noise' works", {
  sd_sd <- c(0.3, 0.4, 1, 0.2)
  sd_levels <- 1:4
  sd_matrix_outcome <- Matrix::Matrix(kronecker(rep(1, 3), diag(4)))
  sd_arg <- data.frame(region = sd_levels, sd = sd_sd)
  nms_by <- "region"
  outcome_sd <- 5
  x <- new_bage_datamod_noise(sd_sd = sd_sd,
                              sd_levels = sd_levels,
                              sd_matrix_outcome = sd_matrix_outcome,
                              sd_arg = sd_arg,
                              nms_by = nms_by,
                              outcome_sd = outcome_sd)
  expect_s3_class(x, "bage_datamod_noise")
  expect_s3_class(x, "bage_datamod_outcome")
})


## 'bage_datamod_overcount' ---------------------------------------------------

test_that("'new_bage_datamod_overcount' works", {
  rate_mean <- c(0.5, 0.2, 0.3, 0.4)
  rate_disp <- c(2, 0.2, 0.3, 0.1)
  rate_levels <- 1:4
  rate_matrix_outcome <- Matrix::Matrix(kronecker(rep(1, 3), diag(4)))
  rate_arg <- data.frame(age = rate_levels, mean = rate_mean, disp = rate_disp)
  nms_by <- "age"
  x <- new_bage_datamod_overcount(rate_mean = rate_mean,
                                  rate_disp = rate_disp,
                                  rate_levels = rate_levels,
                                  rate_matrix_outcome = rate_matrix_outcome,
                                  rate_arg = rate_arg,
                                  nms_by = nms_by)
  expect_s3_class(x, "bage_datamod_overcount")
  expect_s3_class(x, "bage_datamod_outcome")
})


## 'bage_datamod_undercount' --------------------------------------------------

test_that("'new_bage_datamod_undercount' works", {
  prob_mean <- c(0.1, 0.4, 0.2)
  prob_disp <- c(0.3, 0.4, 1)
  prob_levels <- 1:3
  prob_matrix_outcome <- Matrix::Matrix(kronecker(diag(3), rep(1, 4)))
  prob_arg <- data.frame(age = prob_levels, mean = prob_mean, disp = prob_disp)
  nms_by <- "age"
  x <- new_bage_datamod_undercount(prob_mean = prob_mean,
                                   prob_disp = prob_disp,
                                   prob_levels = prob_levels,
                                   prob_matrix_outcome = prob_matrix_outcome,
                                   prob_arg = prob_arg,
                                   nms_by = nms_by)
  expect_s3_class(x, "bage_datamod_undercount")
  expect_s3_class(x, "bage_datamod_outcome")
})

  
