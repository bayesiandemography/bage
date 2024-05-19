
## 'components' ---------------------------------------------------------------

test_that("'components' works with ssvd - all defaults", {
  set.seed(0)
  ssvd <- sim_ssvd()
  set.seed(0)
  ans_obtained <- components(ssvd)
  set.seed(0)
  matrix <- Matrix::as.matrix(ssvd$data$matrix[[1]][,1:5])
  colnames(matrix) <- 1:5
  ans_expected <- list(matrix = matrix,
                       offset = ssvd$data$offset[[1]])
  expect_identical(ans_obtained, ans_expected)
})

test_that("'components' works with ssvd - indep", {
  set.seed(0)
  ssvd <- sim_ssvd()
  set.seed(0)
  ans_obtained <- components(ssvd, joint = FALSE, n_comp = 3)
  set.seed(0)
  matrix <- Matrix::as.matrix(ssvd$data$matrix[[3]][,c(1:3, 11:13)])
  colnames(matrix) <- paste(rep(c("Female", "Male"), each = 3), 1:3, sep = ".")
  ans_expected <- list(matrix = matrix,
                       offset = ssvd$data$offset[[3]])
  expect_identical(ans_obtained, ans_expected)
})

test_that("'generate' works with ssvd - joint", {
  age_labels <- poputils::age_labels(type = "lt", max = 65)
  ans_obtained <- components(HMD, joint = TRUE, age_labels = age_labels, n_comp = 1)
  matrix <- Matrix::as.matrix(HMD$data$matrix[[79]][,1, drop = FALSE])
  colnames(matrix) <- 1
  ans_expected <- list(matrix = matrix,
                       offset = HMD$data$offset[[79]])
  expect_identical(ans_obtained, ans_expected)
})

test_that("'components' method for ssvd - gives expected error with invalid age labels", {
  age_labels <- poputils::age_labels(type = "lt", max = 65)
  age_labels[10] <- "wrong"
  expect_error(components(HMD, age_labels = age_labels),
               "Problem with `age_labels`")
})

test_that("'components' method for ssvd - gives expected error with age labels not in x", {
  age_labels <- poputils::age_labels(type = "lt", max = 120)
  expect_error(components(HMD, age_labels = age_labels),
               "Can't find labels from `age_labels` in `object`")
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

test_that("'generate' method for ssvd - gives expected when 'n_comp' too high", {
  age_labels <- poputils::age_labels(type = "lt", max = 80)
  expect_error(generate(HMD, age_labels = age_labels, n_comp = 11),
               "`n_comp` larger than number of components of `x`.")
})


## 'print' --------------------------------------------------------------------

test_that("'print' works with ssvd", {
  set.seed(0)
  ssvd <- sim_ssvd()
  expect_snapshot(print(ssvd))
})
