
## 'get_labels_svd' -----------------------------------------------------------

test_that("'get_labels_svd' works - total", {
  prior <- SVD(LFP)
  dimnames_term <- list(region = c("A", "B"),
                        age = poputils::age_labels(type = "five", min = 15, max = 60))
  var_sexgender <- NULL
  ans_obtained <- get_labels_svd(prior,
                                 dimnames_term = dimnames_term,
                                 var_sexgender = var_sexgender)
  ans_expected <- c("comp1", "comp2", "comp3")
  expect_identical(ans_obtained, ans_expected)
})

test_that("'get_labels_svd' works - indep", {
  prior <- SVD(LFP)
  dimnames_term <- list(region = c("A", "B"),
                        sex = c("M", "F"),
                        age = poputils::age_labels(type = "five", min = 15, max = 60))
  var_sexgender <- "sex"
  ans_obtained <- get_labels_svd(prior,
                                 dimnames_term = dimnames_term,
                                 var_sexgender = var_sexgender)
  ans_expected <- c("M.comp1", "M.comp2", "M.comp3",
                    "F.comp1", "F.comp2", "F.comp3")
  expect_identical(ans_obtained, ans_expected)
})

test_that("'get_labels_svd' works - joint", {
  prior <- SVD(LFP, indep = FALSE)
  dimnames_term <- list(region = c("A", "B"),
                        sex = c("M", "F"),
                        age = poputils::age_labels(type = "five", min = 15, max = 60))
  var_sexgender <- "sex"
  ans_obtained <- get_labels_svd(prior,
                                 dimnames_term = dimnames_term,
                                 var_sexgender = var_sexgender)
  ans_expected <- c("comp1", "comp2", "comp3")
  expect_identical(ans_obtained, ans_expected)
})


## 'make_i_along' -------------------------------------------------------------

test_that("'make_i_along' works when 'along' is NULL but 'var_time' supplied", {
  along <- NULL
  dimnames_term <- list(reg = letters,
                        period = 2001:2020)
  var_time <- "period"
  var_age <- "age"
  ans_obtained <- make_i_along(along = along,
                               dimnames_term = dimnames_term,
                               var_time = var_time,
                               var_age = var_age)
  ans_expected <- 2L
  expect_identical(ans_obtained, ans_expected)
})

test_that("'make_i_along' works when 'along' is NULL but 'var_age' supplied", {
  along <- NULL
  dimnames_term <- list(reg = letters,
                        age = 0:3)
  var_time <- "period"
  var_age <- "age"
  ans_obtained <- make_i_along(along = along,
                               dimnames_term = dimnames_term,
                               var_time = var_time,
                               var_age = var_age)
  ans_expected <- 2L
  expect_identical(ans_obtained, ans_expected)
})

test_that("'make_i_along' throws correct error when 'along' is NULL and 'var_time' nor 'var_age' supplied", {
  along <- NULL
  dimnames_term <- list(reg = letters,
                        sex = c("F", "M"),
                        oldness = 0:3)
  var_time <- NULL
  var_age <- NULL
  expect_error(make_i_along(along = along,
                            dimnames_term = dimnames_term,
                            var_time = var_time,
                            var_age = var_age),
               "Prior for term `reg:sex:oldness` does not have a value for `along`.")
})

test_that("'make_i_along' throws correct error when 'along' has length 2", {
  along <- c("age", "oldness")
  dimnames_term <- list(reg = letters,
                        sex = c("F", "M"),
                        oldness = 0:3)
  var_time <- NULL
  var_age <- NULL
  expect_error(make_i_along(along = along,
                            dimnames_term = dimnames_term,
                            var_time = var_time,
                            var_age = var_age),
               "Internal error: `along` does not have length 1.")
})

test_that("'make_i_along' throws correct error when 'along' has length 2", {
  along <- c("age", "oldness")
  dimnames_term <- list(reg = letters,
                        sex = c("F", "M"),
                        oldness = 0:3)
  var_time <- NULL
  var_age <- NULL
  expect_error(make_i_along(along = along,
                            dimnames_term = dimnames_term,
                            var_time = var_time,
                            var_age = var_age),
               "Internal error: `along` does not have length 1.")
})

test_that("'make_i_along' works when 'along' supplied", {
  along <- "oldness"
  dimnames_term <- list(reg = letters,
                        sex = c("F", "M"),
                        oldness = 0:3)
  var_time <- NULL
  var_age <- NULL
  ans_obtained <- make_i_along(along = along,
                               dimnames_term = dimnames_term,
                               var_time = var_time,
                               var_age = var_age)
  ans_expected <- 3L
  expect_identical(ans_obtained, ans_expected)
})

test_that("'make_i_along' throws correct error when 'along' not found", {
  along <- "wrong"
  dimnames_term <- list(reg = letters,
                        sex = c("F", "M"),
                        oldness = 0:3)
  var_time <- "time"
  var_age <- "oldness"
  expect_error(make_i_along(along = along,
                            dimnames_term = dimnames_term,
                            var_time = var_time,
                            var_age = var_age),
               "Prior for `reg:sex:oldness` has invalid value for `along`.")
})


## 'make_i_time' --------------------------------------------------------------

test_that("'make_i_time' works with valid inputs", {
  var_time <- "period"
  dimnames_term <- list(reg = letters,
                        period = 2001:2020)
  ans_obtained <- make_i_time(var_time = var_time,
                              dimnames_term = dimnames_term)
  ans_expected <- 2L
  expect_identical(ans_obtained, ans_expected)
})

test_that("'make_i_time' gives correct error when 'var_time' is NULL", {
  var_time <- NULL
  dimnames_term <- list(reg = letters,
                        period = 2001:2020)
  expect_error(make_i_time(var_time = var_time,
                           dimnames_term = dimnames_term),
               "Internal error: `var_time` is NULL.")
})

test_that("'make_i_time' gives correct error when 'var_time' is NULL", {
  var_time <- "wrong"
  dimnames_term <- list(reg = letters,
                        period = 2001:2020)
  expect_error(make_i_time(var_time = var_time,
                           dimnames_term = dimnames_term),
               "Internal error: Term `reg:period` does not include time variable.")
})


## 'make_matrix_agesex' -------------------------------------------------------

test_that("'make_matrix_agesex' works - single age dimension", {
  dimnames_term <- list(age = 0:3)
  var_age <- "age"
  var_sexgender <- "sex"
  ans_obtained <- make_matrix_agesex(dimnames_term = dimnames_term,
                                     var_age = var_age,
                                     var_sexgender = var_sexgender)
  ans_expected <- matrix(0:3,
                         nr = 4,
                         dimnames = list(age = 0:3,
                                         NULL))
  expect_identical(ans_obtained, ans_expected)
})

test_that("'make_matrix_agesex' works - age, sex, and other dimension", {
  dimnames_term <- list(age = 0:3,
                   sex = c("F", "M"),
                   reg = c("A", "B"))
  var_age <- "age"
  var_sexgender <- "sex"
  ans_obtained <- make_matrix_agesex(dimnames_term = dimnames_term,
                                     var_age = var_age,
                                     var_sexgender = var_sexgender)
  ans_expected <- matrix(0:15,
                         nr = 8,
                         dimnames = list("age:sex" = paste(0:3, rep(c("F", "M"), each = 4), sep = "."),
                                         reg = c("A", "B")))
  expect_identical(ans_obtained, ans_expected)
})

test_that("'make_matrix_agesex' works - sex and age order reversed", {
  dimnames_term <- list(sex = c("F", "M"),
                        reg = c("A", "B"),
                        age = 0:3)
  var_age <- "age"
  var_sexgender <- "sex"
  ans_obtained <- make_matrix_agesex(dimnames_term = dimnames_term,
                                     var_age = var_age,
                                     var_sexgender = var_sexgender)
  ans_expected <- matrix(c(0:1, 4:5, 8:9, 12:13, 2:3, 6:7, 10:11, 14:15),
                         nr = 8,
                         dimnames = list("sex:age" = paste(c("F", "M"), rep(0:3, each = 2), sep = "."),
                                         reg = c("A", "B")))
  expect_identical(ans_obtained, ans_expected)
})

test_that("'make_matrix_agesex' throws correct error when var_age is NULL", {
  dimnames_term <- list(sex = c("F", "M"),
                        reg = c("A", "B"),
                        age = 0:3)
  var_age <- NULL
  var_sexgender <- "sex"
  expect_error(make_matrix_agesex(dimnames_term = dimnames_term,
                                     var_age = var_age,
                                  var_sexgender = var_sexgender),
               "Internal error: `var_age` not specified.")
})

test_that("'make_matrix_agesex' throws correct error when term does not have age dimension", {
  dimnames_term <- list(sex = c("F", "M"),
                        reg = c("A", "B"))
  var_age <- "age"
  var_sexgender <- "sex"
  expect_error(make_matrix_agesex(dimnames_term = dimnames_term,
                                  var_age = var_age,
                                  var_sexgender = var_sexgender),
               "Internal error: Term `sex:reg` does not have an age dimension.")
})


## 'make_matrix_along_by_effect' ----------------------------------------------

test_that("'make_matrix_along_by_effect' works with single dimension", {
  along <- "age"
  dimnames_term <- list(age = 0:9)
  var_time <- "time"
  var_age <- "age"
  ans_obtained <- make_matrix_along_by_effect(along = along,
                                              dimnames_term = dimnames_term,
                                              var_time = var_time,
                                              var_age = var_age)
  ans_expected <- matrix(0:9,
                         nr = 10,
                         dimnames = list(age = 0:9,
                                         NULL))
  expect_identical(ans_obtained, ans_expected)
})

test_that("'make_matrix_along_by_effect' works with two dimensions", {
  along <- NULL
  dimnames_term <- list(age = 0:9,
                        time = 2001:2002)
  var_time <- "time"
  var_age <- "age"
  ans_obtained <- make_matrix_along_by_effect(along = along,
                                              dimnames_term = dimnames_term,
                                              var_time = var_time,
                                              var_age = var_age)
  ans_expected <- t(matrix(0:19, nr = 10))
  dimnames(ans_expected) <- list(time = 2001:2002,
                                 age = 0:9)
  expect_identical(ans_obtained, ans_expected)
})


## 'make_matrix_along_by_effectfree_svd' ----------------------------------------

test_that("'make_matrix_along_by_effectfree_svd' works - age x time interaction", {
  prior <- SVD_RW(HMD)
  dimnames <- list(age = c(0:79, "80+"),
                   time = 2001:2003)
  var_age <- "age"
  var_time <- "time"
  var_sexgender <- NULL
  ans_obtained <- make_matrix_along_by_effectfree_svd(prior = prior,
                                                      var_time = var_time,
                                                      var_age = var_age,
                                                      var_sexgender = var_sexgender,
                                                      dimnames_term = dimnames)
  ans_expected <- t(matrix(0:8,
                           nr = 3,
                           dimnames = list(.svd = paste0("comp", 1:3),
                                           time = 2001:2003)))
  expect_identical(ans_obtained, ans_expected)
})

test_that("'make_matrix_along_by_effectfree_svd' works -  time x age interaction", {
  prior <- SVD_AR(HMD, n_comp = 5)
  dimnames <- list(time = 2001:2003,
                   age = c(0:79, "80+"))
  var_age <- "age"
  var_time <- "time"
  var_sexgender <- NULL
  ans_obtained <- make_matrix_along_by_effectfree_svd(prior = prior,
                                                      var_time = var_time,
                                                      var_age = var_age,
                                                      var_sexgender = var_sexgender,
                                                      dimnames_term = dimnames)
  ans_expected <- matrix(0:14,
                         nr = 3,
                         byrow = TRUE,
                         dimnames = list(time = 2001:2003,
                                         .svd = paste0("comp", 1:5)))
  expect_identical(ans_obtained, ans_expected)
})

test_that("'make_matrix_along_by_effectfree_svd' works -  time x sex x age interaction - indep", {
  prior <- SVD_AR(HMD, n_comp = 5)
  dimnames <- list(time = 2001:2003,
                   sex = c("F", "M", "D"),
                   age = c(0:79, "80+"))
  var_age <- "age"
  var_time <- "time"
  var_sexgender <- "sex"
  ans_obtained <- make_matrix_along_by_effectfree_svd(prior = prior,
                                                      var_time = var_time,
                                                      var_age = var_age,
                                                      var_sexgender = var_sexgender,
                                                      dimnames_term = dimnames)
  ans_expected <- matrix(0:44,
                         nr = 3,
                         byrow = TRUE,
                         dimnames = list(time = 2001:2003,
                                         .svd = paste(rep(c("F","M","D"), each = 5),
                                                      paste0("comp", 1:5),
                                                      sep = ".")))
  expect_identical(ans_obtained, ans_expected)
})

test_that("'make_matrix_along_by_effectfree_svd' works -  time x sex x age interaction - joint", {
  prior <- SVD_AR(HMD, indep = FALSE, n_comp = 5)
  dimnames <- list(time = 2001:2003,
                   sex = c("F", "M"),
                   age = c(0:79, "80+"))
  var_age <- "age"
  var_time <- "time"
  var_sexgender <- "sex"
  ans_obtained <- make_matrix_along_by_effectfree_svd(prior = prior,
                                                      var_time = var_time,
                                                      var_age = var_age,
                                                      var_sexgender = var_sexgender,
                                                      dimnames_term = dimnames)
  ans_expected <- matrix(0:14,
                         nr = 3,
                         byrow = TRUE,
                         dimnames = list(time = 2001:2003,
                                         .svd = paste0("comp", 1:5)))
  expect_identical(ans_obtained, ans_expected)
})


## 'make_matrix_along_by_inner' -----------------------------------------------

test_that("'make_matrix_along_by_inner' works when 'i_along' is 1", {
  i_along <- 1L
  dim <- 2:4
  dimnames <- list(a = 1:2, b = 1:3, c = 1:4)
  ans_obtained <- make_matrix_along_by_inner(i_along = i_along,
                                             dimnames = dimnames)
  ans_expected <- matrix(0:23,
                         nr = 2,
                         dimnames = list(a = 1:2,
                                         "b:c" = paste(1:3, rep(1:4, each = 3), sep = ".")))
  expect_identical(ans_obtained, ans_expected)
})

test_that("'make_matrix_along_by_inner' works when 'i_along' is 2", {
  i_along <- 2L
  dim <- 2:4
  dimnames <- list(a = 1:2, b = 1:3, c = 1:4)
  ans_obtained <- make_matrix_along_by_inner(i_along = i_along,
                                             dimnames = dimnames)
  ans_expected <- matrix(c(0L, 2L, 4L,
                           1L, 3L, 5L,
                           6L, 8L, 10L,
                           7L, 9L, 11L,
                           12L, 14L, 16L,
                           13L, 15L, 17L,
                           18L, 20L, 22L,
                           19L, 21L, 23L),
                         nr = 3,
                         dimnames = list(b = 1:3,
                                         "a:c" = paste(1:2, rep(1:4, each = 2), sep = ".")))
  expect_identical(ans_obtained, ans_expected)
})

test_that("'make_matrix_along_by_inner' works when 'i_along' is 3", {
  i_along <- 3L
  dim <- 2:4
  dimnames <- list(a = 1:2, b = 1:3, c = 1:4)
  ans_obtained <- make_matrix_along_by_inner(i_along = i_along,
                                             dimnames = dimnames)
  ans_expected <- matrix(c(0L, 6L, 12L, 18L,
                           1L, 7L, 13L, 19L,
                           2L, 8L, 14L, 20L,
                           3L, 9L, 15L, 21L,
                           4L, 10L, 16L, 22L,
                           5L, 11L, 17L, 23L),
                         nrow = 4,
                         dimnames = list(c = 1:4,
                                         "a:b" = paste(1:2, rep(1:3, each = 2), sep = ".")))
  expect_identical(ans_obtained, ans_expected)
})

test_that("'make_matrix_along_by_inner' works when only one dimension", {
  i_along <- 1L
  dim <- 3L
  dimnames <- list(a = 1:3)
  ans_obtained <- make_matrix_along_by_inner(i_along = i_along,
                                       dimnames = dimnames)
  ans_expected <- matrix(0:2, nr = 3, dimnames = list(a = 1:3, NULL))
  expect_identical(ans_obtained, ans_expected)
})

test_that("'make_matrix_along_by_inner' works when only one element", {
  i_along <- 1L
  dim <- 1L
  dimnames <- list(a = 1)
  ans_obtained <- make_matrix_along_by_inner(i_along = i_along,
                                             dimnames = dimnames)
  ans_expected <- matrix(0L, nr = 1, dimnames = list(a = 1, NULL))
  expect_identical(ans_obtained, ans_expected)
})

test_that("'make_matrix_along_by_inner' works when 'i_along' is 1:2", {
  i_along <- 1:2
  dim <- 2:4
  dimnames <- list(a = 1:2, b = 1:3, c = 1:4)
  ans_obtained <- make_matrix_along_by_inner(i_along = i_along,
                                             dimnames = dimnames)
  ans_expected <- matrix(0:23,
                         nr = 6,
                         dimnames = list("a:b" = paste(1:2, rep(1:3, each = 2), sep = "."),
                                         c = 1:4))
  expect_identical(ans_obtained, ans_expected)
})

test_that("'make_matrix_along_by_inner' works when 'i_along' is 1:2", {
  i_along <- c(1L, 3L)
  dim <- 2:4
  dimnames <- list(a = 1:2, b = 1:3, c = 1:4)
  ans_obtained <- make_matrix_along_by_inner(i_along = i_along,
                                             dimnames = dimnames)
  ans_expected <- matrix(c(0L, 1L, 6L, 7L, 12L, 13L, 18L, 19L,
                           2L, 3L, 8L, 9L, 14L, 15L, 20L, 21L,
                           4L, 5L, 10L, 11L, 16L, 17L, 22L, 23L),
                         nr = 8,
                         dimnames = list("a:c" = paste(1:2, rep(1:4, each = 2), sep = "."),
                                         b = 1:3))
  expect_identical(ans_obtained, ans_expected)
})



## 'svd_to_effect' ------------------------------------------------------------

test_that("'svd_to_effect' works with matrix", {
  prior <- SVD(HMD, n_comp = 5)
  dimnames_term <- list(age = c(0:59, "60+"),
                        reg = c("a", "b", "c", "d", "e"))
  var_age <- "age"
  var_sexgender <- "sex"
  vals_svd <- draw_vals_svd(prior = prior,
                            vals_hyper = list(),
                            dimnames_term = dimnames_term,
                            var_time = var_time,
                            var_age = var_age,
                            var_sexgender = var_sexgender,
                            levels_effectfree = paste(paste0("comp", 1:5),
                                                      rep(c("a", "b", "c", "d", "e"),
                                                          each = 5),
                                                      sep = "."),
                            n_sim = 10)
  ans <- svd_to_effect(svd = vals_svd,
                       prior = prior,
                       dimnames_term = dimnames_term,
                       var_age = var_age,
                       var_sexgender = var_sexgender)
  expect_identical(dim(ans), c(61L * 5L, 10L))
})


test_that("'svd_to_effect' works with matrix", {
  prior <- SVD(HMD, n_comp = 5)
  dimnames_term <- list(age = c(0:59, "60+"),
                        sex = c("F", "M"))
  var_age <- "age"
  var_sexgender <- "sex"
  vals_svd <- draw_vals_svd(prior = prior,
                            vals_hyper = list(),
                            dimnames_term = dimnames_term,
                            var_time = var_time,
                            var_age = var_age,
                            var_sexgender = var_sexgender,
                            levels_effectfree = paste(paste0("comp", 1:5),
                                                      rep(c("F", "M"), each = 5),
                                                      sep = "."),
                            n_sim = 10)
  ans <- svd_to_effect(svd = vals_svd,
                       prior = prior,
                       dimnames_term = dimnames_term,
                       var_age = var_age,
                       var_sexgender = var_sexgender)
  expect_identical(dim(ans), c(61L * 2L, 10L))
})


test_that("'svd_to_effect' works with matrix", {
  prior <- SVD(HMD, indep = FALSE, n_comp = 5)
  dimnames_term <- list(age = c(0:59, "60+"),
                        sex = c("F", "M"))
  var_age <- "age"
  var_sexgender <- "sex"
  vals_svd <- draw_vals_svd(prior = prior,
                            vals_hyper = list(),
                            dimnames_term = dimnames_term,
                            var_time = var_time,
                            var_age = var_age,
                            var_sexgender = var_sexgender,
                            levels_effectfree = paste0("comp", 1:5),
                            n_sim = 10)
  ans <- svd_to_effect(svd = vals_svd,
                       prior = prior,
                       dimnames_term = dimnames_term,
                       var_age = var_age,
                       var_sexgender = var_sexgender)
  expect_identical(dim(ans), c(61L * 2L, 10L))
})



test_that("'svd_to_effect' works with rvec", {
  prior <- SVD(HMD, n_comp = 5)
  dimnames_term <- list(age = c(0:59, "60+"),
                        reg = c("a", "b", "c", "d", "e"))
  var_age <- "age"
  var_sexgender <- "sex"
  vals_svd <- draw_vals_svd(prior = prior,
                            vals_hyper = list(),
                            dimnames_term = dimnames_term,
                            var_time = var_time,
                            var_age = var_age,
                            var_sexgender = var_sexgender,
                            levels_effectfree = paste(paste0("comp", 1:5),
                                                      rep(c("a", "b", "c", "d", "e"),
                                                          each = 5),
                                                      sep = "."),
                            n_sim = 10)
  vals_svd <- rvec::rvec(vals_svd)
  ans <- svd_to_effect(svd = vals_svd,
                       prior = prior,
                       dimnames_term = dimnames_term,
                       var_age = var_age,
                       var_sexgender = var_sexgender)
  expect_identical(length(ans), 61L * 5L)
})

