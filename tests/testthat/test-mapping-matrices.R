
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


## 'make_dim_svd' -------------------------------------------------------------

test_that("'make_dim_svd' works - total", {
  prior <- SVD(LFP)
  dimnames_term <- list(region = c("A", "B"),
                        age = poputils::age_labels(type = "five", min = 15, max = 60))
  var_sexgender <- NULL
  var_age <- "age"
  ans_obtained <- make_dim_svd(prior,
                               dimnames_term = dimnames_term,
                               var_sexgender = var_sexgender,
                               var_age = var_age)
  ans_expected <- c(3L, region = 2L)
  expect_identical(ans_obtained, ans_expected)
})

test_that("'make_dim_svd' works - indep", {
  prior <- SVD(LFP)
  dimnames_term <- list(region = c("A", "B"),
                        sex = c("M", "F"),
                        age = poputils::age_labels(type = "five", min = 15, max = 60))
  var_sexgender <- "sex"
  var_age <- "age"
  ans_obtained <- make_dim_svd(prior,
                               dimnames_term = dimnames_term,
                               var_sexgender = var_sexgender,
                               var_age = var_age)
  ans_expected <- c(6L, region = 2L)
  expect_identical(ans_obtained, ans_expected)
})

test_that("'make_dim_svd' works - joint", {
  prior <- SVD(LFP, indep = FALSE)
  dimnames_term <- list(region = c("A", "B"),
                        sex = c("M", "F"),
                        age = poputils::age_labels(type = "five", min = 15, max = 60))
  var_sexgender <- "sex"
  var_age <- "age"
  ans_obtained <- make_dim_svd(prior,
                               dimnames_term = dimnames_term,
                               var_sexgender = var_sexgender,
                               var_age = var_age)
  ans_expected <- c(3L, region = 2L)
  expect_identical(ans_obtained, ans_expected)
})


## 'make_i_along_agetime' -----------------------------------------------------

test_that("'make_i_along_agetime' works when age required", {
  prior <- RW2_Infant()
  dimnames_term <- list(age = 1:3,
                        period = 2001:2020)
  var_time <- "period"
  var_age <- "age"
  ans_obtained <- make_i_along_agetime(prior = prior,
                                       dimnames_term = dimnames_term,
                                       var_time = var_time,
                                       var_age = var_age,
                                       agetime = "age")
  ans_expected <- 1L
  expect_identical(ans_obtained, ans_expected)
})

test_that("'make_i_along_agetime' works when time required", {
  prior <- SVD_RW(HMD)
  dimnames_term <- list(age = 1:3,
                        period = 2001:2020)
  var_time <- "period"
  var_age <- "age"
  ans_obtained <- make_i_along_agetime(prior = prior,
                                       dimnames_term = dimnames_term,
                                       var_time = var_time,
                                       var_age = var_age,
                                       agetime = "time")
  ans_expected <- 2L
  expect_identical(ans_obtained, ans_expected)
})

test_that("'make_i_along_agetime' throws correct error when age/time variable not yet identified", {
  prior <- SVD_RW(HMD)
  dimnames_term <- list(age = 1:3,
                        epoch = 2001:2020)
  var_time <- NULL
  var_age <- "age"
  expect_error(make_i_along_agetime(prior = prior,
                                    dimnames_term = dimnames_term,
                                    var_time = var_time,
                                    var_age = var_age,
                                    agetime = "time"),
               "Using `SVD_RW\\(\\)` prior when time variable not identified.")
})

test_that("'make_i_along_agetime' throws correct error when age/time variable not yet identified", {
  prior <- SVD_RW(HMD)
  dimnames_term <- list(age = 1:3,
                        cohort = 2001:2020)
  var_time <- "time"
  var_age <- "age"
  expect_error(make_i_along_agetime(prior = prior,
                                    dimnames_term = dimnames_term,
                                    var_time = var_time,
                                    var_age = var_age,
                                    agetime = "time"),
               "Using `SVD_RW\\(\\)` prior with a term that does not involve time.")
})





## 'make_i_along_inner' -------------------------------------------------------

test_that("'make_i_along_inner' works when 'along' is NULL but 'var_time' supplied", {
  along <- NULL
  dimnames_term <- list(reg = letters,
                        period = 2001:2020)
  var_time <- "period"
  var_age <- "age"
  ans_obtained <- make_i_along_inner(along = along,
                                     dimnames_term = dimnames_term,
                                     var_time = var_time,
                                     var_age = var_age)
  ans_expected <- 2L
  expect_identical(ans_obtained, ans_expected)
})

test_that("'make_i_along_inner' works when 'along' is NULL but 'var_age' supplied", {
  along <- NULL
  dimnames_term <- list(reg = letters,
                        age = 0:3)
  var_time <- "period"
  var_age <- "age"
  ans_obtained <- make_i_along_inner(along = along,
                                     dimnames_term = dimnames_term,
                                     var_time = var_time,
                                     var_age = var_age)
  ans_expected <- 2L
  expect_identical(ans_obtained, ans_expected)
})

test_that("'make_i_along_inner' throws correct error when 'along' is NULL and 'var_time' nor 'var_age' supplied", {
  along <- NULL
  dimnames_term <- list(reg = letters,
                        sex = c("F", "M"),
                        oldness = 0:3)
  var_time <- NULL
  var_age <- NULL
  expect_error(make_i_along_inner(along = along,
                                  dimnames_term = dimnames_term,
                                  var_time = var_time,
                                  var_age = var_age),
               "Prior for term `reg:sex:oldness` does not have a value for `along`.")
})

test_that("'make_i_along_inner' throws correct error when 'along' has length 2", {
  along <- c("age", "oldness")
  dimnames_term <- list(reg = letters,
                        sex = c("F", "M"),
                        oldness = 0:3)
  var_time <- NULL
  var_age <- NULL
  expect_error(make_i_along_inner(along = along,
                                  dimnames_term = dimnames_term,
                                  var_time = var_time,
                                  var_age = var_age),
               "Internal error: `along` does not have length 1.")
})

test_that("'make_i_along_inner' throws correct error when 'along' has length 2", {
  along <- c("age", "oldness")
  dimnames_term <- list(reg = letters,
                        sex = c("F", "M"),
                        oldness = 0:3)
  var_time <- NULL
  var_age <- NULL
  expect_error(make_i_along_inner(along = along,
                                  dimnames_term = dimnames_term,
                                  var_time = var_time,
                                  var_age = var_age),
               "Internal error: `along` does not have length 1.")
})

test_that("'make_i_along_inner' works when 'along' supplied", {
  along <- "oldness"
  dimnames_term <- list(reg = letters,
                        sex = c("F", "M"),
                        oldness = 0:3)
  var_time <- NULL
  var_age <- NULL
  ans_obtained <- make_i_along_inner(along = along,
                                     dimnames_term = dimnames_term,
                                     var_time = var_time,
                                     var_age = var_age)
  ans_expected <- 3L
  expect_identical(ans_obtained, ans_expected)
})

test_that("'make_i_along_inner' throws correct error when 'along' not found", {
  along <- "wrong"
  dimnames_term <- list(reg = letters,
                        sex = c("F", "M"),
                        oldness = 0:3)
  var_time <- "time"
  var_age <- "oldness"
  expect_error(make_i_along_inner(along = along,
                                  dimnames_term = dimnames_term,
                                  var_time = var_time,
                                  var_age = var_age),
               "Prior for `reg:sex:oldness` has invalid value for `along`.")
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


## 'make_matrix_along_by_effectfree_inner' ------------------------------------

test_that("'make_matrix_along_by_effectfree_inner' works - reg x time interaction, append_zero is TRUE, con is 'by'", {
  prior <- RW(con = "by")
  dimnames_term <- list(reg = 1:4,
                        time = 2001:2010)
  var_age <- "age"
  var_time <- "time"
  var_sexgender <- "sex"
  ans_obtained <- make_matrix_along_by_effectfree_inner(prior = prior,
                                                        dimnames_term = dimnames_term,
                                                        var_time = var_time,
                                                        var_age = var_age,
                                                        var_sexgender = var_sexgender,
                                                        append_zero = TRUE)
  ans_expected <- make_matrix_along_by_inner(i_along = 2L, dim = c(3L, 9L))
  expect_identical(ans_obtained, ans_expected)
})

test_that("'make_matrix_along_by_effectfree_inner' works - reg x time interaction, append_zero is FALSE, con is 'none'", {
  prior <- RW()
  dimnames_term <- list(reg = 1:4,
                        time = 2001:2010)
  var_age <- "age"
  var_time <- "time"
  var_sexgender <- "sex"
  ans_obtained <- make_matrix_along_by_effectfree_inner(prior = prior,
                                                        dimnames_term = dimnames_term,
                                                        var_time = var_time,
                                                        var_age = var_age,
                                                        var_sexgender = var_sexgender,
                                                        append_zero = FALSE)
  ans_expected <- make_matrix_along_by_inner(i_along = 2L, dim = c(4L, 10L))
  expect_identical(ans_obtained, ans_expected)
})


## 'make_matrix_along_by_effect' ----------------------------------------------

test_that("'make_matrix_along_by_effect' works with single dimension", {
  prior = RW()
  dimnames_term <- list(age = 0:9)
  var_time <- "time"
  var_age <- "age"
  ans_obtained <- make_matrix_along_by_effect(prior = prior,
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
  prior <- RW()
  dimnames_term <- list(age = 0:9,
                        time = 2001:2002)
  var_time <- "time"
  var_age <- "age"
  ans_obtained <- make_matrix_along_by_effect(prior = prior,
                                              dimnames_term = dimnames_term,
                                              var_time = var_time,
                                              var_age = var_age)
  ans_expected <- t(matrix(0:19, nr = 10))
  dimnames(ans_expected) <- list(time = 2001:2002,
                                 age = 0:9)
  expect_identical(ans_obtained, ans_expected)
})


## 'make_matrix_along_by_inner' -----------------------------------------------

test_that("'make_matrix_along_by_inner' works when 'i_along' is 1", {
  i_along <- 1L
  dim <- 2:4
  ans_obtained <- make_matrix_along_by_inner(i_along = i_along, dim = dim)
  ans_expected <- matrix(0:23, nr = 2)
  expect_identical(ans_obtained, ans_expected)
})

test_that("'make_matrix_along_by_inner' works when 'i_along' is 2", {
  i_along <- 2L
  dim <- 2:4
  ans_obtained <- make_matrix_along_by_inner(i_along = i_along, dim = dim)
  ans_expected <- matrix(c(0L, 2L, 4L,
                           1L, 3L, 5L,
                           6L, 8L, 10L,
                           7L, 9L, 11L,
                           12L, 14L, 16L,
                           13L, 15L, 17L,
                           18L, 20L, 22L,
                           19L, 21L, 23L),
                         nr = 3)
  expect_identical(ans_obtained, ans_expected)
})

test_that("'make_matrix_along_by_inner' works when 'i_along' is 3", {
  i_along <- 3L
  dim <- 2:4
  ans_obtained <- make_matrix_along_by_inner(i_along = i_along, dim = dim)
  ans_expected <- matrix(c(0L, 6L, 12L, 18L,
                           1L, 7L, 13L, 19L,
                           2L, 8L, 14L, 20L,
                           3L, 9L, 15L, 21L,
                           4L, 10L, 16L, 22L,
                           5L, 11L, 17L, 23L),
                         nrow = 4)
  expect_identical(ans_obtained, ans_expected)
})

test_that("'make_matrix_along_by_inner' works when only one dimension", {
  i_along <- 1L
  dim <- 3L
  ans_obtained <- make_matrix_along_by_inner(i_along = i_along, dim = dim)
  ans_expected <- matrix(0:2, nr = 3)
  expect_identical(ans_obtained, ans_expected)
})

test_that("'make_matrix_along_by_inner' works when only one element", {
  i_along <- 1L
  dim <- 1L
  ans_obtained <- make_matrix_along_by_inner(i_along = i_along, dim = dim)
  ans_expected <- matrix(0L, nr = 1)
  expect_identical(ans_obtained, ans_expected)
})

test_that("'make_matrix_along_by_inner' works when 'i_along' is 1:2", {
  i_along <- 1:2
  dim <- 2:4
  ans_obtained <- make_matrix_along_by_inner(i_along = i_along, dim = dim)
  ans_expected <- matrix(0:23,
                         nr = 6)
  expect_identical(ans_obtained, ans_expected)
})

test_that("'make_matrix_along_by_inner' works when 'i_along' is 1:2", {
  i_along <- c(1L, 3L)
  dim <- 2:4
  ans_obtained <- make_matrix_along_by_inner(i_along = i_along, dim = dim)
  ans_expected <- matrix(c(0L, 1L, 6L, 7L, 12L, 13L, 18L, 19L,
                           2L, 3L, 8L, 9L, 14L, 15L, 20L, 21L,
                           4L, 5L, 10L, 11L, 16L, 17L, 22L, 23L),
                         nr = 8)
  expect_identical(ans_obtained, ans_expected)
})


## 'make_matrix_along_by_svddynamic' ------------------------------------------

test_that("'make_matrix_along_by_svddynamic' works - age, sex, time", {
  prior <- SVD_AR(HMD, n_comp = 4)
  ans_obtained <- make_matrix_along_by_svddynamic(prior = prior,
                                                  dimnames_term = list(sex = c("f", "m"),
                                                                       age = c(0:99,
                                                                               "100+"),
                                                                       time = 1:10),
                                                  var_time = "time",
                                                  var_age = "age",
                                                  var_sexgender = "sex",
                                                  dim = c(2L, 101L, 10L))
  ans_expected <- make_matrix_along_by_inner(2L, c(8L, 10L))
  expect_identical(ans_obtained, ans_expected)
})

test_that("'make_matrix_along_by_svddynamic' works - time, age", {
  prior <- SVD_AR(HMD, n_comp = 4)
  ans_obtained <- make_matrix_along_by_svddynamic(prior = prior,
                                                  dimnames_term = list(time = 1:10,
                                                                       age = c(0:99,
                                                                               "100+")),
                                                  var_time = "time",
                                                  var_age = "age",
                                                  var_sexgender = "sex",
                                                  dim = c(10L, 101L))
  ans_expected <- make_matrix_along_by_inner(2L, c(4L, 10L))
  expect_identical(ans_obtained, ans_expected)
})


## 'make_matrix_append_zero' --------------------------------------------------

test_that("'make_matrix_append_zero' works", {
  m <- make_matrix_append_zero(c(3L, 5L))
  ans_obtained <- matrix(m %*% (1:10), nr = 3)
  ans_expected <- rbind(0, matrix(1:10, nr = 2))
  expect_identical(ans_obtained, ans_expected)
})


## 'make_matrix_con_by' -------------------------------------------------------

test_that("'make_matrix_con_by' works with one 'by' dimension, along first", {
  m <- make_matrix_con_by(i_along = 1L, dim = c(4, 3))
  ans <- m %*% rnorm(12)
  expect_equal(rowMeans(matrix(ans, nr = 4)), rep(0, 4))
})

test_that("'make_matrix_con_by' works with one 'by' dimension, along second", {
  m <- make_matrix_con_by(i_along = 2L, dim = c(4, 3))
  ans <- m %*% rnorm(12)
  expect_equal(colMeans(matrix(ans, nr = 4)), rep(0, 3))
})

test_that("'make_matrix_con_by' works with two 'by' dimensions, along first", {
  m <- make_matrix_con_by(i_along = 1L, dim = c(4, 3, 2))
  ans <- array(m %*% rnorm(24), dim = 4:2)
  expect_equal(as.numeric(apply(ans, 1:2, mean)), rep(0, 12))
  expect_equal(as.numeric(apply(ans, c(1, 3), mean)), rep(0, 8))
})

test_that("'make_matrix_con_by' works with two 'by' dimensions, along second", {
  m <- make_matrix_con_by(i_along = 2L, dim = c(4, 3, 2))
  ans <- array(m %*% rnorm(24), dim = 4:2)
  expect_equal(as.numeric(apply(ans, 1:2, mean)), rep(0, 12))
  expect_equal(as.numeric(apply(ans, c(2, 3), mean)), rep(0, 6))
})


## 'make_matrix_constraints' --------------------------------------------------

test_that("'make_matrix_constraints' works", {
  ans_obtained <- make_matrix_constraints(c(2, 3))
  ans_expected <- matrix(c(1, 1, 0, 0, 0, 0,
                           0, 0, 1, 1, 0, 0,
                           0, 0, 0, 0, 1, 1,
                           1, 0, 1, 0, 1, 0,
                           0, 1, 0, 1, 0, 1),
                         byrow = TRUE,
                         nrow = 5)
  expect_identical(ans_obtained, ans_expected)
})


## 'make_matrix_draws_svd_appendzero' ---------------------------------------------

test_that("'make_matrix_draws_svd_appendzero' works - age and time", {
  prior <- SVD(HMD)
  dimnames_term <- list(age = poputils::age_labels(type = "lt", max = 60),
                        time = 2001:2005)
  ans_obtained <- make_matrix_draws_svd_appendzero(prior = prior,
                                                   dimnames_term = dimnames_term,
                                                   var_time = "time",
                                                   var_age = "age",
                                                   var_sexgender = "sex")
  ans_expected <- Matrix::kronecker(rbind(0, Matrix::.sparseDiagonal(4)),
                                    Matrix::.sparseDiagonal(3))
  expect_identical(ans_obtained, ans_expected)
})

test_that("'make_matrix_draws_svd_appendzero' works - time and age", {
  prior <- SVD(HMD)
  dimnames_term <- list(time = 2001:2005,
                        age = poputils::age_labels(type = "lt", max = 60))
  ans_obtained <- make_matrix_draws_svd_appendzero(prior = prior,
                                                   dimnames_term = dimnames_term,
                                                   var_time = "time",
                                                   var_age = "age",
                                                   var_sexgender = "sex")
  ans_expected <- Matrix::kronecker(rbind(0, Matrix::.sparseDiagonal(4)),
                                    Matrix::.sparseDiagonal(3))
  expect_identical(ans_obtained, ans_expected)
})


test_that("'make_matrix_draws_svd_appendzero' works - region, time, sex and age", {
  prior <- SVD(HMD)
  dimnames_term <- list(reg = 1:2,
                        time = 2001:2005,
                        sex = c("f", "m"),
                        age = poputils::age_labels(type = "lt", max = 60))
  m <- make_matrix_draws_svd_appendzero(prior = prior,
                                        dimnames_term = dimnames_term,
                                        var_time = "time",
                                        var_age = "age",
                                        var_sexgender = "sex")
  x <- array(1:48, dim = c(6, 2, 4))
  ans_obtained <- array(m %*% as.integer(x), dim = c(6, 2, 5))
  ans_expected <- array(0L, dim = c(6, 2, 5))
  ans_expected[,,2:5] <- 1 * x
  expect_identical(ans_obtained, ans_expected)
})




## 'make_matrix_draws_svd_nozero' ---------------------------------------------

test_that("'make_matrix_draws_svd_nozero' works", {
  prior <- SVD(HMD)
  dimnames_term <- list(age = poputils::age_labels(type = "lt", max = 60))
  ans_obtained <- make_matrix_draws_svd_nozero(prior = prior,
                                               dimnames_term = dimnames_term,
                                               var_time = "time",
                                               var_age = "age",
                                               var_sexgender = "sex")
  ans_expected <- Matrix::.sparseDiagonal(3)
  expect_identical(ans_obtained, ans_expected)
})


## 'make_matrix_effectfree_effect_inner' --------------------------------------

test_that("'make_matrix_effectfree_effect_inner' works - con is 'by', along first", {
  dimnames_term <- list(time = 2001:2003, age = 0:4)
  prior <- AR1(con = "by")
  m <- make_matrix_effectfree_effect_inner(prior = prior,
                                           dimnames_term = dimnames_term,
                                           var_time = "time",
                                           var_age = "age",
                                           var_sexgender = "sex",
                                           append_zero = FALSE)
  expect_equal(rowSums(matrix(m %*% (1:12), nrow = 3)), rep(0, 3))
})

test_that("'make_matrix_effectfree_effect_inner' works - con is 'by', along second", {
  prior <- AR1(con = "by")
  dimnames_term <- list(age = 0:2, time = 2001:2005)
  m <- make_matrix_effectfree_effect_inner(prior = prior,
                                           dimnames_term = dimnames_term,
                                           var_time = "time",
                                           var_age = "age",
                                           var_sexgender = "sex",
                                           append_zero = FALSE)
  ans_obtained <- colSums(matrix(m %*% rnorm(10), nrow = 3))
  ans_expected <- rep(0, 5)
  expect_equal(ans_obtained, ans_expected)
})

test_that("'make_matrix_effectfree_effect_inner' works - append zero, along first", {
  set.seed(0)
  prior <- AR1()  
  dimnames_term <- list(time = 2001:2003, age = 0:4)
  m <- make_matrix_effectfree_effect_inner(prior = prior,
                                           dimnames_term = dimnames_term,
                                           var_time = "time",
                                           var_age = "age",
                                           var_sexgender = var_sexgender,
                                           append_zero = TRUE)
  x <- rnorm(10)
  ans_obtained <- matrix(m %*% x, nr = 3)
  ans_expected <- rbind(0, matrix(x, nr = 2))
  expect_identical(ans_obtained, ans_expected)
})

test_that("'make_matrix_effectfree_effect_inner' works - append zero, along second", {
  set.seed(0)
  prior <- RW()
  dimnames_term <- list(age = 0:4, time = 2001:2003)
  m <- make_matrix_effectfree_effect_inner(prior = prior,
                                           dimnames_term = dimnames_term,
                                           var_time = "time",
                                           var_age = "age",
                                           var_sexgender = "sex",
                                           append_zero = TRUE)
  x <- rnorm(10)
  ans_obtained <- matrix(m %*% x, nr = 5)
  ans_expected <- cbind(0, matrix(x, nr = 5))
  expect_identical(ans_obtained, ans_expected)
})

test_that("'make_matrix_effectfree_effect_inner' works - append column and append zero, along first", {
  set.seed(0)
  prior <- RW(con = "by")
  dimnames_term <- list(time = 2001:2003, age = 0:4)
  m <- make_matrix_effectfree_effect_inner(prior = prior,
                                           dimnames_term = dimnames_term,
                                           var_time = "time",
                                           var_age = "age",
                                           var_sexgender = var_sexgender,
                                           append_zero = TRUE)
  expect_identical(dim(m), c(15L, 8L))
  ans <- m %*% rnorm(8)
  ans <- matrix(ans, nrow = 3)
  expect_true(all(ans[1,] == 0))
  expect_equal(rowSums(ans), rep(0, 3))
})

test_that("'make_matrix_effectfree_effect_inner' works - append column and append zero, along second", {
  set.seed(0)
  prior <- AR1(con = "by")
  dimnames_term <- list(age = 0:4, time = 2001:2003)
  m <- make_matrix_effectfree_effect_inner(prior = prior,
                                           dimnames_term = dimnames_term,
                                           var_time = "time",
                                           var_age = "age",
                                           var_sexgender = "sex",
                                           append_zero = TRUE)
  expect_identical(dim(m), c(15L, 8L))
  ans <- m %*% rnorm(8)
  ans <- matrix(ans, nrow = 5)
  expect_true(all(ans[,1] == 0))
  expect_equal(colSums(ans), rep(0, 3))
})

test_that("'make_matrix_effectfree_effect' works with SVD-based matrix_sub_orig", {
  dimnames_term <- list(age = poputils::age_labels(type = "five", max = 60),
                        time = 2001:2005)
  var_time <- "time"
  var_age <- "age"
  var_sexgender <- "sex"
  prior <- SVD_RW(HMD)
  ans <- make_matrix_effectfree_effect_inner(prior = prior,
                                             dimnames_term = dimnames_term,
                                             var_time = var_time,
                                             var_age = var_age,
                                             var_sexgender = var_sexgender,
                                             append_zero = TRUE)
  expect_equal(dim(ans), c(5 * 13, 4 * 3))
})

test_that("'make_matrix_effectfree_effect' works with SVD-based matrix_sub_orig, con is 'by'", {
  dimnames_term <- list(age = poputils::age_labels(type = "five", max = 60),
                        time = 2001:2005)
  var_time <- "time"
  var_age <- "age"
  var_sexgender <- "sex"
  prior <- SVD_RW(HMD, con = "by")
  ans <- make_matrix_effectfree_effect_inner(prior = prior,
                                             dimnames_term = dimnames_term,
                                             var_time = var_time,
                                             var_age = var_age,
                                             var_sexgender = var_sexgender,
                                             append_zero = TRUE)
  expect_equal(dim(ans), c(prod(lengths(dimnames_term)), 4L * 3L))
})


## 'make_matrix_perm_agesex_from_font' ----------------------------------------

test_that("'make_matrix_perm_agesex_from_front' works - age and sex", {
  i_age <- 2L
  i_sexgender <- 3L
  dim_after <- c(4L, 5L, 2L)
  m <- make_matrix_perm_agesex_from_front(i_age = i_age,
                                          i_sexgender = i_sexgender,
                                          dim_after = dim_after)
  a_orig <- array(1:40, dim = c(4, 5, 2))
  a_perm <- aperm(a_orig, perm = c(2, 3, 1))
  expect_identical(as.integer(m %*% as.integer(a_perm)), as.integer(a_orig))
})

test_that("'make_matrix_perm_agesex_from_front' works - just age", {
  i_age <- 2L
  i_sexgender <- 0L
  dim_after <- c(4L, 5L)
  m <- make_matrix_perm_agesex_from_front(i_age = i_age,
                                          i_sexgender = i_sexgender,
                                          dim_after = dim_after)
  a_orig <- array(1:20, dim = c(4, 5))
  a_perm <- t(a_orig)
  expect_identical(as.integer(m %*% as.integer(a_perm)), as.integer(a_orig))
})

test_that("'make_matrix_perm_agesex_from_front' works - just age, age first", {
  i_age <- 1L
  i_sexgender <- 0L
  dim_after <- c(4L, 5L)
  m <- make_matrix_perm_agesex_from_front(i_age = i_age,
                                          i_sexgender = i_sexgender,
                                          dim_after = dim_after)
  expect_identical(as.matrix(m), diag(20))
})


## 'make_matrix_perm_along_from_front' ----------------------------------------

test_that("'make_matrix_perm_along_from_front' reverses effects of 'make_matrix_perm_along_to_front'", {
  m1 <- make_matrix_perm_along_to_front(dim_after = c(4L, 2:3), i_along = 3L)
  m2 <- make_matrix_perm_along_from_front(dim_after = 2:4, i_along = 3L)
  expect_identical(as.matrix(m2 %*% m1), diag(24L))
  m1 <- make_matrix_perm_along_to_front(dim_after = c(3L, 2L, 4L), i_along = 2L)
  m2 <- make_matrix_perm_along_from_front(dim_after = 2:4, i_along = 2L)
  expect_identical(as.matrix(m2 %*% m1), diag(24L))
  m1 <- make_matrix_perm_along_to_front(dim_after = 2:4, i_along = 1L)
  m2 <- make_matrix_perm_along_from_front(dim_after = 2:4, i_along = 1L)
  expect_identical(as.matrix(m2 %*% m1), diag(24L))
})


## 'make_matrix_perm_along_to_front' ------------------------------------------

test_that("'make_matrix_perm_along_to_front' works with 2 dimensions, along first", {
  ans_obtained <- make_matrix_perm_along_to_front(i_along = 1L, dim_after = 3:2)
  ans_expected <- Matrix::.sparseDiagonal(6)
  expect_identical(ans_obtained, ans_expected)
})

test_that("'make_matrix_perm_along_to_front' works with 2 dimensions, along second", {
  m <- make_matrix_perm_along_to_front(i_along = 2L, dim_after = 2:3)
  x <- matrix(1:6, nr = 3)
  expect_identical(matrix(m %*% as.integer(x), nr = 2),
                   1 * t(x))
  expect_identical(as.matrix(crossprod(m)), diag(6))
})

test_that("'make_matrix_perm_along_to_front' works with 3 dimensions, along second", {
  m <- make_matrix_perm_along_to_front(i_along = 2L, dim_after = c(2L, 3L, 2L))
  x <- array(1:12, dim = c(3, 2, 2))
  expect_identical(array(m %*% as.integer(x), dim = c(2, 3, 2)),
                   1 * aperm(x, perm = c(2, 1, 3)))
  expect_identical(as.matrix(crossprod(m)), diag(12))
})


## 'make_matrix_spline' -------------------------------------------------------

test_that("'make_matrix_spline' works", {
    set.seed(0)
    m <- make_matrix_spline(n_along = 10, n_comp = 5)
    expect_equal(dim(m), c(10L, 5L))
    expect_equal(colSums(as.matrix(m)), rep(0, times = 5))
})


## 'make_matrix_sub_orig_svd' -------------------------------------------------

test_that("'make_matrix_sub_orig_svd' works with bage_prior_svd_ar - time x age interaction, con is 'none'", {
  prior <- SVD_RW(HMD)
  dimnames_term <- list(time = 2001:2005,
                        age = poputils::age_labels(type = "five", max = 60))
  var_age <- "age"
  var_sexgender <- "sex"
  dim_after <- c(4L, 13L)
  ans <- make_matrix_sub_orig_svd(prior = prior,
                                  dimnames_term = dimnames_term,
                                  var_age = var_age,
                                  var_sexgender = var_sexgender,
                                  dim_after = dim_after,
                                  con = "none")
  expect_equal(dim(ans), c(prod(dim_after), 12L))
})

test_that("'make_matrix_sub_orig_svd' works with bage_prior_svd_ar - time x age interaction, con is 'none'", {
  prior <- SVD_RW(HMD)
  dimnames_term <- list(time = 2001:2005,
                        age = poputils::age_labels(type = "five", max = 60))
  var_age <- "age"
  var_sexgender <- "sex"
  dim_after <- c(4L, 12L)
  ans <- make_matrix_sub_orig_svd(prior = prior,
                                  dimnames_term = dimnames_term,
                                  var_age = var_age,
                                  var_sexgender = var_sexgender,
                                  dim_after = dim_after,
                                  con = "by")
  expect_equal(dim(ans), c(prod(dim_after), 12L))
})

test_that("'make_matrix_sub_orig_svd' works with bage_prior_svd_ar - sex x time x age interaction, con is 'none'", {
  prior <- SVD_RW(HMD)
  dimnames_term <- list(sex = c("f", "m"),
                        time = 2001:2005,
                        age = poputils::age_labels(type = "five", max = 60))
  var_age <- "age"
  var_sexgender <- "sex"
  dim_after <- c(1L, 4L, 12L)
  ans <- make_matrix_sub_orig_svd(prior = prior,
                                  dimnames_term = dimnames_term,
                                  var_age = var_age,
                                  var_sexgender = var_sexgender,
                                  dim_after = dim_after,
                                  con = "by")
  expect_equal(dim(ans), c(prod(dim_after), 24L))
})


## 'make_matrix_unconstr_constr' ----------------------------------------------

test_that("'make_matrix_unconstr_constr' works when array representation of constrained vector has dimension 4", {
  set.seed(0)
  m <- make_matrix_unconstr_constr(4)
  x <- rnorm(3)
  y <- m %*% x
  expect_identical(length(y), 4L)
  expect_equal(sum(y), 0)
})


## 'make_matrix_unconstr_constr_along' ----------------------------------------

test_that("'make_matrix_unconstr_constr_along' works when array representation of unconstrained vector has dimension 3 x 2", {
  set.seed(0)
  m <- make_matrix_unconstr_constr_along(c(3L, 2L))
  x <- rnorm(3)
  y <- matrix(m %*% x, nr = 3)
  expect_equal(dim(y), c(3L, 2L))
  expect_equal(rowSums(y), rep(0, 3))
})

test_that("'make_matrix_unconstr_constr_along' works when array representation of unconstrained vector has dimension 4 x 3", {
  set.seed(0)
  m <- make_matrix_unconstr_constr_along(c(4L, 3L))
  x <- rnorm(8)
  y <- matrix(m %*% x, nr = 4)
  expect_equal(dim(y), c(4L, 3L))
  expect_equal(rowSums(y), rep(0, 4))
})

test_that("'make_matrix_unconstr_constr_along' works when array representation of unconstrained vector has dimension 1 x 3", {
  set.seed(0)
  m <- make_matrix_unconstr_constr_along(c(1L, 3L))
  x <- rnorm(2)
  y <- matrix(m %*% x, nr = 1)
  expect_equal(dim(y), c(1L, 3L))
  expect_equal(rowSums(y), 0)
})

test_that("'make_matrix_unconstr_constr_along' works when array representation of constrained vector has dimension 2 x 3 x 4", {
  set.seed(0)
  m <- make_matrix_unconstr_constr_along(2:4)
  x <- rnorm(12)
  y <- array(m %*% x, dim = 2:4)
  expect_equal(rowSums(y[1,,]), rep(0, 3))
  expect_equal(colSums(y[1,,]), rep(0, 4))
  expect_equal(rowSums(y[2,,]), rep(0, 3))
  expect_equal(colSums(y[2,,]), rep(0, 4))
})

test_that("'make_matrix_unconstr_constr_along' works when array representation of constrained vector has dimension 3 x 4 x 5 x 6", {
  set.seed(0)
  m <- make_matrix_unconstr_constr_along(3:6)
  x <- rnorm(3 * prod(3:5))
  y <- array(m %*% x, dim = 3:6)
  expect_equal(rowSums(y[1,,,1]), rep(0, 4))
  expect_equal(rowSums(y[1,1,,]), rep(0, 5))
  expect_equal(rowSums(y[1,2,,]), rep(0, 5))
  expect_equal(rowSums(y[1,,,4]), rep(0, 4))
  expect_equal(colSums(y[1,,1,]), rep(0, 6))
  expect_equal(colSums(y[1,,2,]), rep(0, 6))
  expect_equal(rowSums(y[2,,,1]), rep(0, 4))
  expect_equal(rowSums(y[2,1,,]), rep(0, 5))
  expect_equal(colSums(y[2,,1,]), rep(0, 6))
})


## 'make_offset_effectfree_effect_svd' ----------------------------------------

test_that("'make_offset_effectfree_effect_svd' works with bage_prior_svd - age effect", {
  prior <- SVD(HMD)
  dimnames_term <- list(age = poputils::age_labels(type = "lt", max = 60))
  var_time <- "time"
  var_age <- "age"
  var_sexgender <- "sex"
  ans <- make_offset_effectfree_effect_svd(prior = prior,
                                           dimnames_term = dimnames_term,
                                           var_time = var_time,
                                           var_age = var_age,
                                           var_sexgender = var_sexgender)
  expect_identical(length(ans), length(dimnames_term[[1]]))
})

test_that("'make_offset_effectfree_effect_svd' works with bage_prior_svd_RW - age x time interaction, con is 'none'", {
  prior <- SVD_RW(HMD)
  dimnames_term <- list(age = poputils::age_labels(type = "lt", max = 60),
                        time = 2001:2010)
  var_time <- "time"
  var_age <- "age"
  var_sexgender <- "sex"
  ans <- make_offset_effectfree_effect_svd(prior = prior,
                                           dimnames_term = dimnames_term,
                                           var_time = var_time,
                                           var_age = var_age,
                                           var_sexgender = var_sexgender)
  expect_equal(length(ans), prod(lengths(dimnames_term)))
})

test_that("'make_offset_effectfree_effect_svd' works with bage_prior_svd_RW - age x time interaction, con is 'by'", {
  prior <- SVD_RW(HMD, con = "by")
  dimnames_term <- list(age = poputils::age_labels(type = "lt", max = 60),
                        time = 2001:2010)
  var_time <- "time"
  var_age <- "age"
  var_sexgender <- "sex"
  ans <- make_offset_effectfree_effect_svd(prior = prior,
                                           dimnames_term = dimnames_term,
                                           var_time = var_time,
                                           var_age = var_age,
                                           var_sexgender = var_sexgender)
  expect_equal(length(ans), prod(lengths(dimnames_term)))
})


## 'make_offset_sub_orig_svd' ----------------------------------------

test_that("'make_offset_sub_orig_svd' works with bage_prior_svd - age x time interaction", {
  s <- sim_ssvd()
  prior <- SVD(ssvd = s, n_comp = 2)
  dimnames_term <- list(age = c("0-4", "5-9"),
                        time = 2001:2003)
  var_time <- "time"
  var_age <- "age"
  var_sexgender <- "sex"
  ans_obtained <- make_offset_sub_orig_svd(prior = prior,
                                           dimnames_term = dimnames_term,
                                           var_age = var_age,
                                           var_sexgender = var_sexgender,
                                           dim_after = c(2L, 3L),
                                           con = "none")
  ans_expected <- s$data$offset[s$data$type == "total"][[1L]]
  ans_expected <- unname(rep(ans_expected, 3))
  expect_identical(ans_obtained, ans_expected)
})

test_that("'make_offset_sub_orig_svd' works with bage_prior_svd - age x time interaction, con is 'none'", {
  s <- sim_ssvd()
  prior <- SVD_AR1(ssvd = s, n_comp = 2)
  dimnames_term <- list(age = c("0-4", "5-9"),
                        time = 2001:2003)
  var_time <- "time"
  var_age <- "age"
  var_sexgender <- "sex"
  levels_effect = c("0-4.2001", "5-9.2001", "0-4.2002", "5-9.2002", "0-4.2003", "5-9.2003")
  ans_obtained <- make_offset_sub_orig_svd(prior = prior,
                                           dimnames_term = dimnames_term,
                                           var_age = var_age,
                                           var_sexgender = var_sexgender,
                                           dim_after = c(2L, 3L),
                                           con = "none")
  ans_expected <- s$data$offset[s$data$type == "total"][[1L]]
  ans_expected <- unname(rep(ans_expected, 3))
  expect_identical(ans_obtained, ans_expected)
})

test_that("'make_offset_sub_orig_svd' works with bage_prior_svd_ar - age x time interaction, con is 'by'", {
  s <- sim_ssvd()
  prior <- SVD_AR1(ssvd = s, n_comp = 2)
  dimnames_term <- list(age = c("0-4", "5-9"),
                        time = 2001:2003)
  var_time <- "time"
  var_age <- "age"
  var_sexgender <- "sex"
  ans_obtained <- make_offset_sub_orig_svd(prior = prior,
                                           dimnames_term = dimnames_term,
                                           var_age = var_age,
                                           var_sexgender = var_sexgender,
                                           dim_after = c(1L, 3L),
                                           con = "by")
  ans_expected <- s$data$offset[s$data$type == "total"][[1L]]
  ans_expected <- unname(rep(sqrt(0.5), 3))
  expect_equal(ans_obtained, ans_expected)
})

test_that("'make_offset_sub_orig_svd' works with bage_prior_svd_rw - age x time interaction", {
  s <- sim_ssvd()
  prior <- SVD_RW(ssvd = s, n_comp = 2)
  dimnames_term <- list(age = c("0-4", "5-9"),
                        time = 2001:2003)
  var_time <- "time"
  var_age <- "age"
  var_sexgender <- "sex"
  levels_effect = c("0-4.2001", "5-9.2001", "0-4.2002", "5-9.2002", "0-4.2003", "5-9.2003")
  ans_obtained <- make_offset_sub_orig_svd(prior = prior,
                                                    dimnames_term = dimnames_term,
                                                    var_age = var_age,
                                           var_sexgender = var_sexgender,
                                           dim_after = c(2L, 3L),
                                           con = "none")
  ans_expected <- s$data$offset[s$data$type == "total"][[1L]]
  ans_expected <- unname(rep(ans_expected, 3))
  expect_identical(ans_obtained, ans_expected)
})

test_that("'make_offset_sub_orig_svd' works with bage_prior_svd - sex x age interaction, n = 2", {
  s <- sim_ssvd()
  prior <- SVD(ssvd = s, n_comp = 2)
  levels_effect = c("F.0-4", "M.0-4", "F.5-9", "M.5-9")
  dimnames_term <- list(sex = c("F", "M"),
                        age = c("0-4", "5-9"))
  var_age <- "age"
  var_sexgender <- "sex"
  ans_obtained <- make_offset_sub_orig_svd(prior = prior,
                                           dimnames_term = dimnames_term,
                                           var_age = var_age,
                                           var_sexgender = var_sexgender,
                                           dim_after = c(2L, 2L),
                                           con = "none")
  ans_expected <- unname(s$data$offset[s$data$type == "indep"][[1L]][c(1,3,2,4)])
  expect_identical(ans_obtained, ans_expected)
})

test_that("'make_offset_sub_orig_svd' works with bage_prior_svd - sex x age interaction, indep", {
  s <- sim_ssvd()
  prior <- SVD(ssvd = s, n_comp = 2, indep = FALSE)
  levels_effect = c("F.0-4", "M.0-4", "F.5-9", "M.5-9")
  dimnames_term <- list(sex = c("F", "M"),
                        age = c("0-4", "5-9"))
  var_time <- "time"
  var_age <- "age"
  var_sexgender <- "sex"
  ans_obtained <- make_offset_sub_orig_svd(prior = prior,
                                           dimnames_term = dimnames_term,
                                           var_age = var_age,
                                           var_sexgender = var_sexgender,
                                           dim_after = c(2L, 2L),
                                           con = "none")
  ans_expected <- unname(s$data$offset[s$data$type == "joint"][[1L]][c(1,3,2,4)])
  expect_identical(ans_obtained, ans_expected)
})



