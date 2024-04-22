
## 'generate' -----------------------------------------------------------------

test_that("'generate' works with ssvd - all defaults", {
  set.seed(0)
  ssvd <- sim_ssvd()
  set.seed(0)
  ans_obtained <- generate(ssvd, n_comp = 2)
  set.seed(0)
  ans_expected <- ssvd$data$matrix[[1]][,1:2] %*% matrix(rnorm(40), nr = 2) + ssvd$data$offset[[1]]
  ans_expected <- tibble::tibble(draw = rep(1:20, each = 2),
                                 age = rep(c("0-4", "5-9"), times = 20),
                                 value = as.double(ans_expected))
  expect_identical(ans_obtained, ans_expected)
})

test_that("'generate' works with ssvd - indep", {
  set.seed(0)
  ssvd <- sim_ssvd()
  set.seed(0)
  ans_obtained <- generate(ssvd, n_comp = 2, joint = FALSE)
  set.seed(0)
  ans_expected <- (ssvd$data$matrix[[3]][,c(1:2, 11:12)] %*% matrix(rnorm(80), nr = 4)
    + ssvd$data$offset[[3]])
  ans_expected <- tibble::tibble(draw = rep(1:20, each = 4),
                                 sexgender = rep(c("Female", "Female", "Male", "Male"),
                                                 times = 20),
                                 age = rep(c("0-4", "5-9"), times = 40),
                                 value = as.double(ans_expected))
  expect_identical(ans_obtained, ans_expected)
})

test_that("'generate' works with ssvd - joint", {
  age_labels <- poputils::age_labels(type = "lt", max = 65)
  set.seed(0)
  ans_obtained <- generate(HMD, joint = TRUE, age_labels = age_labels, n_draw = 2)
  set.seed(0)
  ans_expected <- (HMD$data$matrix[[79]][,1:5] %*% matrix(rnorm(10), nr = 5)
    + HMD$data$offset[[79]])
  ans_expected <- tibble::tibble(draw = rep(1:2, each = 30),
                                 sexgender = rep(rep(c("Female", "Male"), each = 15),
                                                 times = 2),
                                 age = rep(age_labels, times = 4),
                                 value = as.double(ans_expected))
  expect_identical(ans_obtained, ans_expected)
})

test_that("'generate' works with ssvd - joint", {
  age_labels <- poputils::age_labels(type = "lt", max = 65)
  set.seed(0)
  ans_obtained <- generate(HMD, joint = TRUE, age_labels = age_labels, n_draw = 2)
  set.seed(0)
  ans_expected <- (HMD$data$matrix[[79]][,1:5] %*% matrix(rnorm(10), nr = 5)
    + HMD$data$offset[[79]])
  ans_expected <- tibble::tibble(draw = rep(1:2, each = 30),
                                 sexgender = rep(rep(c("Female", "Male"), each = 15),
                                                 times = 2),
                                 age = rep(age_labels, times = 4),
                                 value = as.double(ans_expected))
  expect_identical(ans_obtained, ans_expected)
})

test_that("'generate' method for ssvd - gives expected error with invalid age labels", {
  age_labels <- poputils::age_labels(type = "lt", max = 65)
  age_labels[10] <- "wrong"
  expect_error(generate(HMD, age_labels = age_labels),
               "Problem with `age_labels`")
})

test_that("'generate' method for ssvd - gives expected error with age labels not in x", {
  age_labels <- poputils::age_labels(type = "lt", max = 120)
  expect_error(generate(HMD, age_labels = age_labels),
               "Can't find labels from `age_labels` in `x`")
})


## 'n_comp' -------------------------------------------------------------------

test_that("'print' works with ssvd", {
  set.seed(0)
  ssvd <- sim_ssvd()
  ans_obtained <- n_comp(ssvd)
  ans_expected <- 10L
  expect_identical(ans_obtained, ans_expected)
})



## 'print' --------------------------------------------------------------------

test_that("'print' works with ssvd", {
  set.seed(0)
  ssvd <- sim_ssvd()
  expect_snapshot(print(ssvd))
})
