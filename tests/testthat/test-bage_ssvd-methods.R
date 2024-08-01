
## 'components' ---------------------------------------------------------------

test_that("'components' works with ssvd - all defaults", {
  ssvd <- sim_ssvd()
  ans <- components(ssvd)
  expect_identical(names(ans), c("component", "age", "value"))
  expect_identical(ans$value,
                   c(as.numeric(ssvd$data$offset[[1]]),
                     as.numeric(ssvd$data$matrix[[1L]][,1:5])))
})

test_that("'components' works with ssvd - indep", {
  ssvd <- sim_ssvd()
  ans <- components(ssvd, indep = TRUE, n_comp = 3)
  expect_identical(names(ans), c("component", "sex", "age", "value"))
  expect_identical(ans$value,
                   c(as.numeric(ssvd$data$offset[[3]]),
                     as.numeric(rbind(ssvd$data$matrix[[3]][1:2,1:3],
                                      ssvd$data$matrix[[3]][3:4,11:13]))))
})

test_that("'components' works with ssvd - joint", {
  age_labels <- poputils::age_labels(type = "lt", max = 65)
  ans <- components(HMD, indep = FALSE, age_labels = age_labels, n_comp = 1)
  expect_identical(names(ans), c("component", "sex", "age", "value"))
  matrix <- Matrix::as.matrix(HMD$data$matrix[[79]][,1, drop = FALSE])
  expect_identical(ans$value,
                   c(as.numeric(HMD$data$offset[[79]]),
                     as.numeric(HMD$data$matrix[[79]][,1])))
})

test_that("'components' method for ssvd - gives expected error with invalid age labels", {
  age_labels <- poputils::age_labels(type = "lt", max = 65)
  age_labels[10] <- "wrong"
  expect_error(components(HMD, age_labels = age_labels),
               "Problem with `age_labels`")
})

test_that("'components' method for ssvd - gives expected error when 'n_comp' too high", {
  age_labels <- poputils::age_labels(type = "lt", max = 80)
  expect_error(components(HMD, age_labels = age_labels, n_comp = 11),
               "`n_comp` larger than number of components of `object`.")
})

test_that("'components' method for ssvd - gives expected error with age labels not in x", {
  age_labels <- poputils::age_labels(type = "lt", max = 120)
  expect_error(components(HMD, age_labels = age_labels),
               "Can't find labels from `age_labels` in `object`")
})

test_that("'components' method for ssvd - gives expected error when joint supplied by no sex/gender", {
  ssvd <- sim_ssvd()
  ssvd$data <- ssvd$data[1,]
  expect_error(components(ssvd, age_labels = c("0-4", "5-9"), n_comp = 3, indep = FALSE),
               "Value supplied for `indep`, but `object` does not have a sex/gender dimension.")
})


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
  ans_obtained <- generate(ssvd, n_comp = 2, indep = TRUE)
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
  ans_obtained <- generate(HMD, indep = FALSE, age_labels = age_labels, n_draw = 2)
  set.seed(0)
  ans_expected <- (HMD$data$matrix[[79]][,1:3] %*% matrix(rnorm(6), nr = 3)
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
  ans_obtained <- generate(HMD, indep = FALSE, age_labels = age_labels, n_draw = 2)
  set.seed(0)
  ans_expected <- (HMD$data$matrix[[79]][,1:3] %*% matrix(rnorm(6), nr = 3)
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

test_that("'generate' method for ssvd - gives expected error when 'n_comp' too high", {
  age_labels <- poputils::age_labels(type = "lt", max = 80)
  expect_error(generate(HMD, age_labels = age_labels, n_comp = 11),
               "`n_comp` larger than number of components of `x`.")
})
!
test_that("'generate' method for ssvd - gives expected error when joint supplied by no sex/gender", {
  ssvd <- sim_ssvd()
  ssvd$data <- ssvd$data[1,]
  expect_error(generate(ssvd, age_labels = c("0-4", "5-9"), n_comp = 3, indep = TRUE),
               "Value supplied for `indep`, but `x` does not have a sex/gender dimension.")
})


## 'print' --------------------------------------------------------------------

test_that("'print' works with ssvd", {
  set.seed(0)
  expect_snapshot(print(HMD))
})
