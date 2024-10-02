
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


## 'make_matrix_along_by' -----------------------------------------------------

test_that("'make_matrix_along_by' works when 'i_along' is 1", {
  i_along <- 1L
  dim <- 2:4
  dimnames <- list(a = 1:2, b = 1:3, c = 1:4)
  ans_obtained <- make_matrix_along_by(i_along = i_along,
                                       dim = dim,
                                       dimnames = dimnames)
  ans_expected <- matrix(0:23,
                         nr = 2,
                         dimnames = list(a = 1:2,
                                         b.c = paste(1:3, rep(1:4, each = 3), sep = ".")))
  expect_identical(ans_obtained, ans_expected)
})

test_that("'make_matrix_along_by' works when 'i_along' is 2", {
  i_along <- 2L
  dim <- 2:4
  dimnames <- list(a = 1:2, b = 1:3, c = 1:4)
  ans_obtained <- make_matrix_along_by(i_along = i_along,
                                       dim = dim,
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
                                         a.c = paste(1:2, rep(1:4, each = 2), sep = ".")))
  expect_identical(ans_obtained, ans_expected)
})

test_that("'make_matrix_along_by' works when 'i_along' is 3", {
  i_along <- 3L
  dim <- 2:4
  dimnames <- list(a = 1:2, b = 1:3, c = 1:4)
  ans_obtained <- make_matrix_along_by(i_along = i_along,
                                       dim = dim,
                                       dimnames = dimnames)
  ans_expected <- matrix(c(0L, 6L, 12L, 18L,
                           1L, 7L, 13L, 19L,
                           2L, 8L, 14L, 20L,
                           3L, 9L, 15L, 21L,
                           4L, 10L, 16L, 22L,
                           5L, 11L, 17L, 23L),
                         nrow = 4,
                         dimnames = list(c = 1:4,
                                         a.b = paste(1:2, rep(1:3, each = 2), sep = ".")))
  expect_identical(ans_obtained, ans_expected)
})

test_that("'make_matrix_along_by' works when only one dimension", {
  i_along <- 1L
  dim <- 3L
  dimnames <- list(a = 1:3)
  ans_obtained <- make_matrix_along_by(i_along = i_along,
                                       dim = dim,
                                       dimnames = dimnames)
  ans_expected <- matrix(0:2, nr = 3)
  rownames(ans_expected) <- 1:3
  names(dimnames(ans_expected))[1] <- "a"
  expect_identical(ans_obtained, ans_expected)
})

test_that("'make_matrix_along_by' works when only one element", {
  i_along <- 1L
  dim <- 1L
  dimnames <- list(a = 1)
  ans_obtained <- make_matrix_along_by(i_along = i_along,
                                       dim = dim,
                                       dimnames = dimnames)
  ans_expected <- matrix(0L, nr = 1)
  rownames(ans_expected) <- 1
  names(dimnames(ans_expected))[1] <- "a"
  expect_identical(ans_obtained, ans_expected)
})

test_that("'make_matrix_along_by' works when 'i_along' is 1:2", {
  i_along <- 1:2
  dim <- 2:4
  dimnames <- list(a = 1:2, b = 1:3, c = 1:4)
  ans_obtained <- make_matrix_along_by(i_along = i_along,
                                       dim = dim,
                                       dimnames = dimnames)
  ans_expected <- matrix(0:23,
                         nr = 6,
                         dimnames = list(a.b = paste(1:2, rep(1:3, each = 2), sep = "."),
                                         c = 1:4))
  expect_identical(ans_obtained, ans_expected)
})

test_that("'make_matrix_along_by' works when 'i_along' is 1:2", {
  i_along <- c(1L, 3L)
  dim <- 2:4
  dimnames <- list(a = 1:2, b = 1:3, c = 1:4)
  ans_obtained <- make_matrix_along_by(i_along = i_along,
                                       dim = dim,
                                       dimnames = dimnames)
  ans_expected <- matrix(c(0L, 1L, 6L, 7L, 12L, 13L, 18L, 19L,
                           2L, 3L, 8L, 9L, 14L, 15L, 20L, 21L,
                           4L, 5L, 10L, 11L, 16L, 17L, 22L, 23L),
                         nr = 8,
                         dimnames = list(a.c = paste(1:2, rep(1:4, each = 2), sep = "."),
                                         b = 1:3))
  expect_identical(ans_obtained, ans_expected)
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


## 'make_matrix_along_by_effectfree_rw' ----------------------------------------

test_that("'make_matrix_along_by_effectfree_rw' works - reg x time interaction", {
  dimnames <- list(reg = 1:4,
                   time = 2001:2010)
  var_age <- "age"
  var_time <- "time"
  ans_obtained <- make_matrix_along_by_effectfree_rw(along = "time",
                                                      var_time = var_time,
                                                      var_age = var_age,
                                                      dimnames_term = dimnames)
  ans_expected <- t(matrix(0:35,
                           nr = 4,
                           dimnames = list(reg = 1:4,
                                           time = 2002:2010)))
  expect_identical(ans_obtained, ans_expected)
})


## 'make_matrix_along_by_svd' ----------------------------------------

test_that("'make_matrix_along_by_svd' works - age x time interaction, drop_first_along is FALSE", {
  prior <- SVD_RW(HMD)
  dimnames <- list(age = c(0:79, "80+"),
                   time = 2001:2003)
  var_age <- "age"
  var_time <- "time"
  var_sexgender <- NULL
  ans_obtained <- make_matrix_along_by_svd(prior = prior,
                                                      var_time = var_time,
                                                      var_age = var_age,
                                                      var_sexgender = var_sexgender,
                                                      dimnames_term = dimnames,
                                                      drop_first_along = FALSE)
  ans_expected <- t(matrix(0:8,
                           nr = 3,
                           dimnames = list(.svd = paste0("comp", 1:3),
                                           time = 2001:2003)))
  expect_identical(ans_obtained, ans_expected)
})

test_that("'make_matrix_along_by_svd' works -  time x age interaction, drop_first_along is TRUE", {
  prior <- SVD_AR(HMD, n_comp = 5)
  dimnames <- list(time = 2001:2003,
                   age = c(0:79, "80+"))
  var_age <- "age"
  var_time <- "time"
  var_sexgender <- NULL
  ans_obtained <- make_matrix_along_by_svd(prior = prior,
                                                      var_time = var_time,
                                                      var_age = var_age,
                                                      var_sexgender = var_sexgender,
                                                      dimnames_term = dimnames,
                                                      drop_first_along = TRUE)
  ans_expected <- matrix(0:9,
                         nr = 2,
                         byrow = TRUE,
                         dimnames = list(time = 2002:2003,
                                         .svd = paste0("comp", 1:5)))
  expect_identical(ans_obtained, ans_expected)
})

test_that("'make_matrix_along_by_svd' works -  time x sex x age interaction - indep,
drop_first_along is FALSE", {
  prior <- SVD_AR(HMD, n_comp = 5)
  dimnames <- list(time = 2001:2003,
                   sex = c("F", "M", "D"),
                   age = c(0:79, "80+"))
  var_age <- "age"
  var_time <- "time"
  var_sexgender <- "sex"
  ans_obtained <- make_matrix_along_by_svd(prior = prior,
                                                      var_time = var_time,
                                                      var_age = var_age,
                                                      var_sexgender = var_sexgender,
                                                      dimnames_term = dimnames,
                                                      drop_first_along = FALSE)
  ans_expected <- matrix(0:44,
                         nr = 3,
                         byrow = TRUE,
                         dimnames = list(time = 2001:2003,
                                         .svd = paste(rep(c("F","M","D"), each = 5),
                                                      paste0("comp", 1:5),
                                                      sep = ".")))
  expect_identical(ans_obtained, ans_expected)
})

test_that("'make_matrix_along_by_svd' works -  time x sex x age interaction - joint, drop_first_along is TRUE", {
  prior <- SVD_AR(HMD, indep = FALSE, n_comp = 5)
  dimnames <- list(time = 2001:2003,
                   sex = c("F", "M"),
                   age = c(0:79, "80+"))
  var_age <- "age"
  var_time <- "time"
  var_sexgender <- "sex"
  ans_obtained <- make_matrix_along_by_svd(prior = prior,
                                                      var_time = var_time,
                                                      var_age = var_age,
                                                      var_sexgender = var_sexgender,
                                                      dimnames_term = dimnames,
                                                      drop_first_along = TRUE)
  ans_expected <- matrix(0:9,
                         nr = 2,
                         byrow = TRUE,
                         dimnames = list(time = 2002:2003,
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


## 'make_matrix_along_by_spline' ----------------------------------------------

test_that("'make_matrix_along_by_spline' works - age x reg interaction, drop_first_along is FALSE", {
  prior <- Sp(n = 8)
  dimnames <- list(age = c(0:79, "80+"),
                   reg = 1:3)
  var_age <- "age"
  var_time <- "time"
  ans_obtained <- make_matrix_along_by_spline(prior = prior,
                                              dimnames_term = dimnames,
                                              var_time = var_time,
                                              var_age = var_age,
                                              drop_first_along = FALSE)
  ans_expected <- matrix(0:23, nr = 8, dimnames = list(age = paste0("comp", 1:8), reg = 1:3))
  expect_identical(ans_obtained, ans_expected)
})

test_that("'make_matrix_along_by_spline' works - sex x age reg interaction, drop_first_along is TRUE", {
  prior <- Sp(n = 8)
  dimnames <- list(sex = c("F", "M"),
                   age = c(0:79, "80+"))
  var_age <- "age"
  var_time <- "time"
  ans_obtained <- make_matrix_along_by_spline(prior = prior,
                                              dimnames_term = dimnames,
                                              var_time = var_time,
                                              var_age = var_age,
                                              drop_first_along = TRUE)
  ans_expected <- t(matrix(0:13,
                           nr = 2,
                           dimnames = list(sex = c("F", "M"), age = paste0("comp", 2:8))))
  expect_identical(ans_obtained, ans_expected)
})


## 'make_matrix_effectfree_effect_rw' -----------------------------------------

test_that("'make_matrix_effectfree_effect_rw' works - age x time interaction", {
  along <- "time"
  dimnames <- list(age = 0:4,
                   time = 2001:2003)
  var_age <- "age"
  var_time <- "time"
  ans <- make_matrix_effectfree_effect_rw(along = along,
                                          var_time = var_time,
                                          var_age = var_age,
                                          dimnames_term = dimnames)
  expect_identical(dim(ans), c(15L, 10L))
  expect_identical(rownames(ans), paste(rep(2001:2003, each = 5), 0:4, sep = "."))
})


## 'make_matrix_effectfree_effect_svd' ----------------------------------------

test_that("'make_matrix_effectfree_effect_svd' works with bage_prior_svd - age main effect", {
  s <- sim_ssvd()
  prior <- SVD(ssvd = s, n_comp = 3)
  dimnames_term <- list(age = c("0-4", "5-9"))
  var_time <- "time"
  var_age <- "age"
  var_sexgender <- "sex"
  ans_obtained <- make_matrix_effectfree_effect_svd(prior = prior,
                                                    dimnames_term = dimnames_term,
                                                    var_time = var_time,
                                                    var_age = var_age,
                                                    var_sexgender = var_sexgender)
  ans_expected <- s$data$matrix[s$data$type == "total"][[1L]][,1:3]
  ans_expected <- Matrix::sparseMatrix(i = row(ans_expected),
                                       j = col(ans_expected),
                                       x = as.double(ans_expected),
                                       dimnames = dimnames(ans_expected))
  expect_identical(ans_obtained, ans_expected)
})

test_that("'make_matrix_effectfree_effect_svd' works with bage_prior_svd - age-sex interaction, joint", {
  s <- sim_ssvd()
  prior <- SVD(ssvd = s, n_comp = 3, indep = FALSE)
  dimnames_term <- list(sex = c("Female", "Male"),
                        age = c("0-4", "5-9"))
  var_time <- "time"
  var_age <- "age"
  var_sexgender <- "sex"
  ans_obtained <- make_matrix_effectfree_effect_svd(prior = prior,
                                                    dimnames_term = dimnames_term,
                                                    var_time = var_time,
                                                    var_age = var_age,
                                                    var_sexgender = var_sexgender)
  ans_expected <- s$data$matrix[s$data$type == "joint"][[1L]][c(1,3,2,4),1:3]
  ans_expected <- Matrix::sparseMatrix(i = row(ans_expected),
                                       j = col(ans_expected),
                                       x = as.double(ans_expected))
  rownames(ans_expected) <- c("Female.0-4", "Male.0-4", "Female.5-9", "Male.5-9")
  expect_identical(ans_obtained, ans_expected)
})

test_that("'make_matrix_effectfree_effect_svd' works with bage_prior_svd - age x reg interaction", {
  s <- sim_ssvd()
  prior <- SVD(ssvd = s, n_comp = 3)
  dimnames_term <- list(age = c("0-4", "5-9"),
                        x = 1:2)
  var_time <- "time"
  var_age <- "age"
  var_sexgender <- "sex"
  ans_obtained <- make_matrix_effectfree_effect_svd(prior = prior,
                                                    dimnames_term = dimnames_term,
                                                    var_time = var_time,
                                                    var_age = var_age,
                                                    var_sexgender = var_sexgender)
  m2 <- s$data$matrix[s$data$type == "total"][[1L]][,1:3]
  m2 <- Matrix::kronecker(Matrix::.sparseDiagonal(2), m2)
  matrix_agesex <- make_matrix_agesex(dimnames_term = dimnames_term,
                                      var_age = var_age,
                                      var_sexgender = var_sexgender)
  m1 <- make_index_matrix(matrix_agesex)
  ans_expected <- m1 %*% m2
  expect_identical(ans_obtained, ans_expected)
})

test_that("'make_matrix_effectfree_effect_svd' works with bage_prior_svd - sex x reg x age interaction", {
  prior <- SVD(HMD)
  dimnames_term = list(sex = c("F", "M"),
                       age = c(0, "1-4", paste(seq(5, 55, 5), seq(9, 59, 5), sep = "--"), "60+"),
                       reg <- c("A", "B"))
  var_time <- "time"
  var_age <- "age"
  var_sexgender <- "sex"
  ans_obtained <- make_matrix_effectfree_effect_svd(prior = prior,
                                                    dimnames_term = dimnames_term,
                                                    var_time = var_time,
                                                    var_age = var_age,
                                                    var_sexgender = var_sexgender)
  m2 <- HMD$data$matrix[[35]][as.integer(t(matrix(1:28,nr=14))), c(1:3, 6:8)]
  m2 <- Matrix::kronecker(Matrix::.sparseDiagonal(2), m2)
  matrix_agesex <- make_matrix_agesex(dimnames_term = dimnames_term,
                                      var_age = var_age,
                                      var_sexgender = var_sexgender)
  m1 <- make_index_matrix(matrix_agesex)
  ans_expected <- m1 %*% m2
  expect_identical(ans_obtained, ans_expected)
})


## 'make_matrix_effectfree_spline' --------------------------------------------

test_that("'make_matrix_effectfree_splien' works with main effect", {
  prior <- Sp(n = 4)
  dimnames_term <- list(age = 0:10)
  var_time <- "time"
  var_age <- "age"
  ans_obtained <- make_matrix_effectfree_spline(prior = prior,
                                                dimnames_term = dimnames_term,
                                                var_time = var_time,
                                                var_age = var_age)
  ans_expected <- rbind(0, Matrix::.sparseDiagonal(3))
  expect_identical(ans_obtained, ans_expected)
})

test_that("'make_matrix_effectfree_spline' works with interaction", {
  prior <- Sp(n = 4)
  dimnames_term <- list(sex = c("f", "m"),
                        age = 0:10)
  var_time <- "time"
  var_age <- "age"
  ans_obtained <- make_matrix_effectfree_spline(prior = prior,
                                                dimnames_term = dimnames_term,
                                                var_time = var_time,
                                                var_age = var_age)
  ans_expected <- rbind(cbind(rbind(0, Matrix::.sparseDiagonal(3)), matrix(0, nr = 4, nc = 3)),
                        cbind(matrix(0, nr = 4, nc = 3), rbind(0, Matrix::.sparseDiagonal(3))))
  expect_identical(ans_obtained, ans_expected)
})



## 'make_offset_effectfree_effect_svd' ----------------------------------------

test_that("'make_offset_effectfree_effect_svd' works with bage_prior_svd - age x time interaction", {
  s <- sim_ssvd()
  prior <- SVD(ssvd = s, n_comp = 2)
  dimnames_term <- list(age = c("0-4", "5-9"),
                        time = 2001:2003)
  var_time <- "time"
  var_age <- "age"
  var_sexgender <- "sex"
  ans_obtained <- make_offset_effectfree_effect_svd(prior = prior,
                                                    dimnames_term = dimnames_term,
                                                    var_time = var_time,
                                                    var_age = var_age,
                                                    var_sexgender = var_sexgender)
  ans_expected <- s$data$offset[s$data$type == "total"][[1L]]
  ans_expected <- rep(ans_expected, 3)
  levels_effect = c("0-4.2001", "5-9.2001", "0-4.2002", "5-9.2002", "0-4.2003", "5-9.2003")
  names(ans_expected) <- levels_effect
  expect_identical(ans_obtained, ans_expected)
})

test_that("'make_offset_effectfree_effect_svd' works with bage_prior_svd - age x time interaction", {
  s <- sim_ssvd()
  prior <- SVD_AR1(ssvd = s, n_comp = 2)
  dimnames_term <- list(age = c("0-4", "5-9"),
                        time = 2001:2003)
  var_time <- "time"
  var_age <- "age"
  var_sexgender <- "sex"
  levels_effect = c("0-4.2001", "5-9.2001", "0-4.2002", "5-9.2002", "0-4.2003", "5-9.2003")
  ans_obtained <- make_offset_effectfree_effect_svd(prior = prior,
                                                    dimnames_term = dimnames_term,
                                                    var_time = var_time,
                                                    var_age = var_age,
                                                    var_sexgender = var_sexgender)
  ans_expected <- s$data$offset[s$data$type == "total"][[1L]]
  ans_expected <- rep(ans_expected, 3)
  names(ans_expected) <- levels_effect
  expect_identical(ans_obtained, ans_expected)
})

test_that("'make_offset_effectfree_effect_svd' works with bage_prior_svd - age x time interaction", {
  s <- sim_ssvd()
  prior <- SVD_RW(ssvd = s, n_comp = 2)
  dimnames_term <- list(age = c("0-4", "5-9"),
                        time = 2001:2003)
  var_time <- "time"
  var_age <- "age"
  var_sexgender <- "sex"
  levels_effect = c("0-4.2001", "5-9.2001", "0-4.2002", "5-9.2002", "0-4.2003", "5-9.2003")
  ans_obtained <- make_offset_effectfree_effect_svd(prior = prior,
                                                    dimnames_term = dimnames_term,
                                                    var_time = var_time,
                                                    var_age = var_age,
                                                    var_sexgender = var_sexgender)
  ans_expected <- s$data$offset[s$data$type == "total"][[1L]]
  ans_expected <- rep(ans_expected, 3)
  names(ans_expected) <- levels_effect
  expect_identical(ans_obtained, ans_expected)
})

test_that("'make_offset_effectfree_effect_svd' works with bage_prior_svd - age x time interaction", {
  s <- sim_ssvd()
  prior <- SVD_RW2(ssvd = s, n_comp = 2)
  levels_effect = c("0-4.2001", "5-9.2001", "0-4.2002", "5-9.2002", "0-4.2003", "5-9.2003")
  dimnames_term <- list(age = c("0-4", "5-9"),
                        time = 2001:2003)
  var_time <- "time"
  var_age <- "age"
  var_sexgender <- "sex"
  ans_obtained <- make_offset_effectfree_effect_svd(prior = prior,
                                                    dimnames_term = dimnames_term,
                                                    var_time = var_time,
                                                    var_age = var_age,
                                                    var_sexgender = var_sexgender)
  ans_expected <- s$data$offset[s$data$type == "total"][[1L]]
  ans_expected <- rep(ans_expected, 3)
  names(ans_expected) <- levels_effect
  expect_identical(ans_obtained, ans_expected)
})

test_that("'make_offset_effectfree_effect_svd' works with bage_prior_svd - sex x age interaction", {
  s <- sim_ssvd()
  prior <- SVD(ssvd = s, n_comp = 2)
  levels_effect = c("F.0-4", "M.0-4", "F.5-9", "M.5-9")
  dimnames_term <- list(sex = c("F", "M"),
                        age = c("0-4", "5-9"))
  var_time <- "time"
  var_age <- "age"
  var_sexgender <- "sex"
  ans_obtained <- make_offset_effectfree_effect_svd(prior = prior,
                                                    dimnames_term = dimnames_term,
                                                    var_time = var_time,
                                                    var_age = var_age,
                                                    var_sexgender = var_sexgender)
  ans_expected <- s$data$offset[s$data$type == "indep"][[1L]][c(1,3,2,4)]
  names(ans_expected) <- levels_effect
  expect_identical(ans_obtained, ans_expected)
})

test_that("'make_offset_effectfree_effect_svd' works with bage_prior_svd - sex x age interaction", {
  s <- sim_ssvd()
  prior <- SVD(ssvd = s, n_comp = 2, indep = FALSE)
  levels_effect = c("F.0-4", "M.0-4", "F.5-9", "M.5-9")
  dimnames_term <- list(sex = c("F", "M"),
                        age = c("0-4", "5-9"))
  var_time <- "time"
  var_age <- "age"
  var_sexgender <- "sex"
  ans_obtained <- make_offset_effectfree_effect_svd(prior = prior,
                                                    dimnames_term = dimnames_term,
                                                    var_time = var_time,
                                                    var_age = var_age,
                                                    var_sexgender = var_sexgender)
  ans_expected <- s$data$offset[s$data$type == "joint"][[1L]][c(1,3,2,4)]
  names(ans_expected) <- levels_effect
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
                            levels_svd = paste(paste0("comp", 1:5),
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
                            levels_svd = paste(paste0("comp", 1:5),
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
                            levels_svd = paste0("comp", 1:5),
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
                            levels_svd = paste(paste0("comp", 1:5),
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

