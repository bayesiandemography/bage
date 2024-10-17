
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


## 'make_index_matrix' -------------------------------------------------

test_that("'make_index_matrix' works with valid inputs", {
  set.seed(0)
  data <- expand.grid(sex = c("F", "M"),
                      age = 0:3,
                      reg = c("A", "B"))
  data$popn <- rpois(n = nrow(data), lambda = 100)
  data$deaths <- rpois(n = nrow(data), lambda = 10)
  mod <- mod_pois(deaths ~ reg * age * sex,
                  data = data,
                  exposure = popn)
  matrix_agesex <- make_matrix_agesex(dimnames_term = mod$dimnames_terms[["reg:age:sex"]],
                                      var_age = mod$var_age,
                                      var_sexgender = mod$var_sexgender)
  ans_obtained <- make_index_matrix(matrix_agesex)
  cn <- paste(0:3, rep(c("F", "M"), each = 4), rep(c("A", "B"), each = 8), sep = ".")
  rn <- paste(rep(0:3, each = 2), rep(c("F", "M"), each = 8), c("A", "B"), sep = ".")
  ans_expected <- Matrix::sparseMatrix(i = c(1L, 3L, 5L, 7L, 9L, 11L, 13L, 15L,
                                             2L, 4L, 6L, 8L, 10L, 12L, 14L, 16L),
                                       j = 1:16,
                                       x = rep(1L, 16),
                                       dimnames = list(rn, cn))
  expect_identical(ans_obtained, ans_expected)
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


## 'make_matrix_along_by_drop_first_along' ------------------------------------

test_that("'make_matrix_along_by_drop_first_along' works - reg x time interaction", {
  dimnames <- list(reg = 1:4,
                   time = 2001:2010)
  var_age <- "age"
  var_time <- "time"
  ans_obtained <- make_matrix_along_by_drop_first_along(along = "time",
                                                        var_time = var_time,
                                                        var_age = var_age,
                                                        dimnames_term = dimnames)
  ans_expected <- t(matrix(0:35,
                           nr = 4,
                           dimnames = list(reg = 1:4,
                                           time = 2002:2010)))
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

test_that("'make_matrix_along_by_spline' works - age x reg interaction", {
  prior <- Sp(n = 8)
  dimnames <- list(age = c(0:79, "80+"),
                   reg = 1:3)
  var_age <- "age"
  var_time <- "time"
  ans_obtained <- make_matrix_along_by_spline(prior = prior,
                                              dimnames_term = dimnames,
                                              var_time = var_time,
                                              var_age = var_age)
  ans_expected <- matrix(0:23, nr = 8, dimnames = list(age = paste0("comp", 1:8), reg = 1:3))
  expect_identical(ans_obtained, ans_expected)
})


## 'make_matrix_along_first' --------------------------------------------------

test_that("'make_matrix_along_first' works - original vector has along first", {
  m_along_by <- matrix(0:11, nrow = 2)
  m <- make_matrix_along_first(m_along_by)
  expect_identical(1:12, as.integer(m %*% 1:12))
})

test_that("'make_matrix_along_first' works - original vector has along second", {
  m_along_by <- t(matrix(0:11, nrow = 4))
  m <- make_matrix_along_first(m_along_by)
  expect_identical(as.integer(m %*% 1:12),
                   c(1L, 5L, 9L, 2L, 6L, 10L, 3L, 7L, 11L, 4L, 8L, 12L))
})


## 'make_matrix_append_zero' --------------------------------------------------

test_that("'make_matrix_append_zero' works - along second", {
  v <- array(11:25,
             dim = c(3, 5),
             dimnames = list(age = 0:2, time = 2001:2005))
  m <- make_matrix_append_zero(dimnames(v))
  ans_obtained <- matrix(as.integer(m %*% as.integer(v)), nr = 3)
  ans_expected <- unname(cbind(0L, v))
  expect_identical(ans_obtained, ans_expected)
})

test_that("'make_matrix_append_zero' works - append column and append zero, along first", {
  v <- array(11:25,
             dim = c(3, 5),
             dimnames = list(time = 2001:2003, age = 0:4))
  m <- make_matrix_append_zero(dimnames(v))
  expect_identical(dim(m), c(24L, 15L))
  v <- m %*% as.integer(v)
  v <- matrix(v, nrow = 4)
  expect_true(all(v[1,] == 0))
  expect_equal(rowSums(v), rep(0, 4))
})


## 'make_matrix_effectfree_effect_inner' --------------------------------------------------

test_that("'make_matrix_effectfree_effect_inner' works - append column, along first", {
  v <- array(11:25,
             dim = c(3, 5),
             dimnames = list(time = 2001:2003, age = 0:4))
  matrix_along_by <- matrix(0:14, nr = 3)
  m <- make_matrix_effectfree_effect_inner(matrix_along_by, append_column = TRUE, append_zero = FALSE)
  ans_obtained <- rowSums(matrix(m %*% as.integer(v), nrow = 3))
  ans_expected <- rep(0, 3)
  expect_equal(ans_obtained, ans_expected)
})

test_that("'make_matrix_effectfree_effect_inner' works - append column, along second", {
  v <- array(11:25,
             dim = c(3, 5),
             dimnames = list(age = 0:2, time = 2001:2005))
  matrix_along_by <- t(matrix(0:14, nr = 3))
  m <- make_matrix_effectfree_effect_inner(matrix_along_by, append_column = TRUE, append_zero = FALSE)
  ans_obtained <- colSums(matrix(m %*% as.integer(v), nrow = 4))
  ans_expected <- rep(0, 5)
  expect_equal(ans_obtained, ans_expected)
})

test_that("'make_matrix_effectfree_effect_inner' works - append zero, along first", {
  v <- array(11:25,
             dim = c(3, 5),
             dimnames = list(time = 2001:2003, age = 0:4))
  matrix_along_by <- matrix(0:14, nr = 3)
  m <- make_matrix_effectfree_effect_inner(matrix_along_by, append_column = FALSE, append_zero = TRUE)
  ans_obtained <- matrix(as.integer(m %*% as.integer(v)), nr = 4)
  ans_expected <- unname(rbind(0L, v))
  expect_identical(ans_obtained, ans_expected)
})

test_that("'make_matrix_effectfree_effect_inner' works - append zero, along second", {
  v <- array(11:25,
             dim = c(3, 5),
             dimnames = list(age = 0:2, time = 2001:2005))
  matrix_along_by <- t(matrix(0:14, nr = 3))
  m <- make_matrix_effectfree_effect_inner(matrix_along_by, append_column = FALSE, append_zero = TRUE)
  ans_obtained <- matrix(as.integer(m %*% as.integer(v)), nr = 3)
  ans_expected <- unname(cbind(0L, v))
  expect_identical(ans_obtained, ans_expected)
})

test_that("'make_matrix_effectfree_effect_inner' works - append column and append zero, along first", {
  v <- array(11:25,
             dim = c(3, 5),
             dimnames = list(time = 2001:2003, age = 0:4))
  matrix_along_by <- matrix(0:14, nr = 3)
  m <- make_matrix_effectfree_effect_inner(matrix_along_by, append_column = TRUE, append_zero = TRUE)
  expect_identical(dim(m), c(24L, 15L))
  v <- m %*% as.integer(v)
  v <- matrix(v, nrow = 4)
  expect_true(all(v[1,] == 0))
  expect_equal(rowSums(v), rep(0, 4))
})

test_that("'make_matrix_effectfree_effect_inner' works - append column and append zero, along second", {
  v <- array(11:25,
             dim = c(3, 5),
             dimnames = list(age = 0:2, time = 2001:2005))
  matrix_along_by <- t(matrix(0:14, nr = 3))
  m <- make_matrix_effectfree_effect_inner(matrix_along_by, append_column = TRUE, append_zero = TRUE)
  expect_identical(dim(m), c(24L, 15L))
  v <- m %*% as.integer(v)
  v <- matrix(v, nrow = 4)
  expect_true(all(v[,1] == 0))
  expect_equal(colSums(v), rep(0, 6))
})


## 'make_matrix_expand_svd' ---------------------------------------------------

test_that("'make_matrix_expand_svd' works with bage_prior_svd - age main effect", {
  s <- sim_ssvd()
  prior <- SVD(ssvd = s, n_comp = 3)
  dimnames_term <- list(age = c("0-4", "5-9"))
  var_time <- "time"
  var_age <- "age"
  var_sexgender <- "sex"
  ans_obtained <- make_matrix_expand_svd(prior = prior,
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

test_that("'make_matrix_expand_svd' works with bage_prior_svd - age-sex interaction, joint", {
  s <- sim_ssvd()
  prior <- SVD(ssvd = s, n_comp = 3, indep = FALSE)
  dimnames_term <- list(sex = c("Female", "Male"),
                        age = c("0-4", "5-9"))
  var_time <- "time"
  var_age <- "age"
  var_sexgender <- "sex"
  ans_obtained <- make_matrix_expand_svd(prior = prior,
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

test_that("'make_matrix_expand_svd' works with bage_prior_svd - age x reg interaction", {
  s <- sim_ssvd()
  prior <- SVD(ssvd = s, n_comp = 3)
  dimnames_term <- list(age = c("0-4", "5-9"),
                        x = 1:2)
  var_time <- "time"
  var_age <- "age"
  var_sexgender <- "sex"
  ans_obtained <- make_matrix_expand_svd(prior = prior,
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

test_that("'make_matrix_expand_svd' works with bage_prior_svd - sex x reg x age interaction", {
  prior <- SVD(HMD)
  dimnames_term = list(sex = c("F", "M"),
                       age = c(0, "1-4", paste(seq(5, 55, 5), seq(9, 59, 5), sep = "--"), "60+"),
                       reg <- c("A", "B"))
  var_time <- "time"
  var_age <- "age"
  var_sexgender <- "sex"
  ans_obtained <- make_matrix_expand_svd(prior = prior,
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


## 'make_matrix_perm_along_to_front' ------------------------------------------

test_that("'make_matrix_perm_along_to_front' works with 2 dimensions, along first", {
  along <- NULL
  dimnames_term <- list(age = 1:3, sex = 1:2)
  var_time <- "time"
  var_age <- "age"
  ans_obtained <- make_matrix_perm_along_to_front(along = along,
                                            dimnames_term = dimnames_term,
                                            var_time = var_time,
                                            var_age = var_age)
  ans_expected <- Matrix::.sparseDiagonal(6)
  expect_identical(ans_obtained, ans_expected)
  expect_identical(as.matrix(crossprod(m)), diag(6))
})

test_that("'make_matrix_perm_along_to_front' works with 2 dimensions, along second", {
  along <- NULL
  dimnames_term <- list(sex = 1:2, age = 1:3)
  var_time <- "time"
  var_age <- "age"
  m <- make_matrix_perm_along_to_front(along = along,
                                       dimnames_term = dimnames_term,
                                       var_time = var_time,
                                       var_age = var_age)
  x <- matrix(1:6, nr = 2)
  expect_identical(matrix(m %*% as.integer(x), nr = 3),
                   1 * t(x))
  expect_identical(as.matrix(crossprod(m)), diag(6))
})

test_that("'make_matrix_perm_along_to_front' works with 3 dimensions, along second", {
  along <- NULL
  dimnames_term <- list(sex = 1:2, age = 1:3, reg = 1:2)
  var_time <- "time"
  var_age <- "age"
  m <- make_matrix_perm_along_to_front(along = along,
                                       dimnames_term = dimnames_term,
                                       var_time = var_time,
                                       var_age = var_age)
  x <- array(1:12, dim = c(2, 3, 2))
  expect_identical(array(m %*% as.integer(x), dim = c(3, 2, 2)),
                   1 * aperm(x, perm = c(2, 1, 3)))
  expect_identical(as.matrix(crossprod(m)), diag(12))
})


## 'make_matrix_sub_orig_spline' ----------------------------------------------

test_that("'make_matrix_sub_orig_spline' works with main effect", {
  prior <- Sp(n = 4)
  dimnames_term <- list(age = 0:10)
  var_time <- "time"
  var_age <- "age"
  ans_obtained <- make_matrix_sub_orig_spline(prior = prior,
                                              dimnames_term = dimnames_term,
                                              var_time = var_time,
                                              var_age = var_age)
  ans_expected <- make_spline_matrix(n_comp = 4, n_along = 11)
  expect_identical(ans_obtained, ans_expected)
})

test_that("'make_matrix_sub_orig_spline' works with interaction", {
  prior <- Sp(n = 4)
  dimnames_term <- list(sex = c("f", "m"),
                        age = 0:10)
  var_time <- "time"
  var_age <- "age"
  ans_obtained <- make_matrix_sub_orig_spline(prior = prior,
                                              dimnames_term = dimnames_term,
                                              var_time = var_time,
                                              var_age = var_age)
  m <- make_spline_matrix(n_comp = 4, n_along = 11)
  ans_expected <- rbind(cbind(m, matrix(0, nr = 11, nc = 4)),
                        cbind(matrix(0, nr = 11, nc = 4), m))[c(1, 12, 2, 13, 3, 14, 4, 15, 5, 16,
                                                                6, 17, 7, 18, 8, 19, 9, 20, 10, 21,
                                                                11, 22),c(1,5,2,6,3,7,4,8)]
  expect_identical(unname(as.matrix(ans_obtained)), as.matrix(ans_expected))
})


## 'make_matrix_sub_orig_svd' ---------------------------------------------------

test_that("'make_matrix_sub_orig_svd' works with bage_prior_svd - age main effect", {
  s <- sim_ssvd()
  prior <- SVD(ssvd = s, n_comp = 3)
  dimnames_term <- list(age = c("0-4", "5-9"))
  var_age <- "age"
  var_sexgender <- "sex"
  ans_obtained <- make_matrix_sub_orig_svd(prior = prior,
                                           dimnames_term = dimnames_term,
                                           var_age = var_age,
                                           var_sexgender = var_sexgender)
  ans_expected <- s$data$matrix[s$data$type == "total"][[1L]][,1:3]
  ans_expected <- Matrix::sparseMatrix(i = row(ans_expected),
                                       j = col(ans_expected),
                                       x = as.double(ans_expected),
                                       dimnames = dimnames(ans_expected))
  expect_identical(ans_obtained, ans_expected)
})

test_that("'make_matrix_sub_orig_svd' works with bage_prior_svd - age-sex interaction, joint", {
  s <- sim_ssvd()
  prior <- SVD(ssvd = s, n_comp = 3, indep = FALSE)
  dimnames_term <- list(sex = c("Female", "Male"),
                        age = c("0-4", "5-9"))
  var_age <- "age"
  var_sexgender <- "sex"
  ans_obtained <- make_matrix_sub_orig_svd(prior = prior,
                                           dimnames_term = dimnames_term,
                                           var_age = var_age,
                                           var_sexgender = var_sexgender)
  ans_expected <- s$data$matrix[s$data$type == "joint"][[1L]][c(1,3,2,4),1:3]
  ans_expected <- Matrix::sparseMatrix(i = row(ans_expected),
                                       j = col(ans_expected),
                                       x = as.double(ans_expected))
  rownames(ans_expected) <- c("Female.0-4", "Male.0-4", "Female.5-9", "Male.5-9")
  expect_identical(ans_obtained, ans_expected)
})

test_that("'make_matrix_sub_orig_svd' works with bage_prior_svd - age x reg interaction", {
  s <- sim_ssvd()
  prior <- SVD(ssvd = s, n_comp = 3)
  dimnames_term <- list(age = c("0-4", "5-9"),
                        x = 1:2)
  var_age <- "age"
  var_sexgender <- "sex"
  ans_obtained <- make_matrix_sub_orig_svd(prior = prior,
                                         dimnames_term = dimnames_term,
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

test_that("'make_matrix_sub_orig_svd' works with bage_prior_svd - sex x reg x age interaction", {
  prior <- SVD(HMD)
  dimnames_term = list(sex = c("F", "M"),
                       age = c(0, "1-4", paste(seq(5, 55, 5), seq(9, 59, 5), sep = "--"), "60+"),
                       reg <- c("A", "B"))
  var_age <- "age"
  var_sexgender <- "sex"
  ans_obtained <- make_matrix_sub_orig_svd(prior = prior,
                                         dimnames_term = dimnames_term,
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


## 'make_matrix_svddynamic_effect' --------------------------------------------

test_that("'make_matrix_svddynamic_effect' works with bage_prior_svd_ar - age x time interaction, drop_first_along is TRUE", {
  prior <- SVD_RW(HMD)
  dimnames_term = list(age = poputils::age_labels(type = "five", max = 60),
                       time = 2001:2005)
  var_time <- "time"
  var_age <- "age"
  var_sexgender <- "sex"
  ans <- make_matrix_svddynamic_effect(prior = prior,
                                       dimnames_term = dimnames_term,
                                       var_time = var_time,
                                       var_age = var_age,
                                       var_sexgender = var_sexgender,
                                       drop_first_along = TRUE)
  expect_equal(dim(ans), c(prod(lengths(dimnames_term)), 4 * 3))
})

test_that("'make_matrix_svddynamic_effect' works with bage_prior_svd_ar - time x time interaction, drop_first_along is FALSE", {
  prior <- SVD_RW(HMD)
  dimnames_term = list(time = 2001:2005,
                       age = poputils::age_labels(type = "five", max = 60))
  var_time <- "time"
  var_age <- "age"
  var_sexgender <- "sex"
  ans <- make_matrix_svddynamic_effect(prior = prior,
                                       dimnames_term = dimnames_term,
                                       var_time = var_time,
                                       var_age = var_age,
                                       var_sexgender = var_sexgender,
                                       drop_first_along = FALSE)
  expect_equal(dim(ans), c(prod(lengths(dimnames_term)), 5 * 3))
})


## 'make_matrix_transpose' ----------------------------------------------------

test_that("'make_matrix_transpose' works with square matrix", {
  m <- make_matrix_transpose(nr = 3, nc = 3)
  x <- 1:9
  ans_obtained <- as.numeric(m %*% x)
  ans_expected <- as.numeric(t(matrix(x, nr = 3)))
  expect_equal(ans_obtained, ans_expected)
})

test_that("'make_matrix_transpose' works with rectangular matrix", {
  m <- make_matrix_transpose(nr = 3, nc = 4)
  x <- 1:12
  ans_obtained <- as.numeric(m %*% x)
  ans_expected <- as.numeric(t(matrix(x, nr = 3)))
  expect_equal(ans_obtained, ans_expected)
})


## 'make_matrix_unconstr_to_constr' -------------------------------------------

test_that("'make_matrix_unconstr_to_constr' works when array representation of constrained vector has dimension 3 x 2", {
  set.seed(0)
  dimnames_term <- list(age = 1:3, sex = 1:2)
  m <- make_matrix_unconstr_to_constr(dimnames_term)
  x <- rnorm(3)
  y <- matrix(m %*% x, nr = 3)
  expect_equal(dim(y), c(3L, 2L))
  expect_equal(rowSums(y), rep(0, 3))
})

test_that("'make_matrix_unconstr_to_constr' works when array representation of constrained vector has dimension 4 x 3", {
  set.seed(0)
  dimnames_term <- list(age = 1:4, sex = 1:3)
  m <- make_matrix_unconstr_to_constr(dimnames_term)
  x <- rnorm(8)
  y <- matrix(m %*% x, nr = 4)
  expect_equal(dim(y), c(4L, 3L))
  expect_equal(rowSums(y), rep(0, 4))
})

test_that("'make_matrix_unconstr_to_constr' works when array representation of constrained vector has dimension 1 x 3", {
  set.seed(0)
  dimnames_term <- list(age = 1, sex = 1:3)
  m <- make_matrix_unconstr_to_constr(dimnames_term)
  x <- rnorm(2)
  y <- matrix(m %*% x, nr = 1)
  expect_equal(dim(y), c(1L, 3L))
  expect_equal(rowSums(y), 0)
})

test_that("'make_matrix_unconstr_to_constr' works when array representation of constrained vector has dimension 2 x 3 x 4", {
  set.seed(0)
  dimnames_term <- list(time = 1:2, age = 1:3, sex = 1:4)
  m <- make_matrix_unconstr_to_constr(dimnames_term)
  x <- rnorm(12)
  y <- array(m %*% x, dim = 2:4)
  expect_equal(rowSums(y[1,,]), rep(0, 3))
  expect_equal(colSums(y[1,,]), rep(0, 4))
  expect_equal(rowSums(y[2,,]), rep(0, 3))
  expect_equal(colSums(y[2,,]), rep(0, 4))
})

test_that("'make_matrix_unconstr_to_constr' works when array representation of constrained vector has dimension 3 x 4 x 5 x 6", {
  set.seed(0)
  dimnames_term <- list(time = 1:3, age = 1:4, sex = 1:5, reg = 1:6)
  m <- make_matrix_unconstr_to_constr(dimnames_term)
  x <- rnorm(3 * prod(3:5))
  y <- array(m %*% x, dim = 3:6)
  expect_equal(rowSums(y[1,,,1]), rep(0, 4))
  expect_equal(rowSums(y[1,1,,]), rep(0, 5))
  expect_equal(colSums(y[1,1,,]), rep(0, 6))
  expect_equal(rowSums(y[2,,,1]), rep(0, 4))
  expect_equal(rowSums(y[2,1,,]), rep(0, 5))
  expect_equal(colSums(y[2,1,,]), rep(0, 6))
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

