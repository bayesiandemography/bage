## const ----------------------------------------------------------------------

test_that("'const' works with bage_prior_ar", {
  prior <- AR(n = 3)
  ans_obtained <- const(prior)
  ans_expected <- prior$const
  expect_identical(ans_obtained, ans_expected)
})


## draw_vals_effect --------------------------------------------------------------

test_that("'draw_vals_effect' works with bage_prior_ar - n_by = 1", {
  prior <- AR(n = 3)
  n_sim <- 10
  vals_hyper <- draw_vals_hyper(prior = prior,
                                n_sim = n_sim)
  vals_hyperrand <- list()
  levels_effect <- letters
  matrix_along_by <- matrix(0:25, nr = 26)
  ans <- draw_vals_effect(prior = prior,
                          vals_hyper = vals_hyper,
                          vals_hyperrand = vals_hyperrand,
                          levels_effect = levels_effect,
                          agesex = "other",
                          matrix_along_by = matrix_along_by,
                          matrix_agesex = NULL,
                          n_sim = n_sim)
  expect_identical(dimnames(ans), list(letters, as.character(1:10)))
})

test_that("'draw_vals_effect' works with bage_prior_ar - n_by = 2", {
  prior <- AR(n = 3)
  matrix_along_by <- matrix(0:25, nr = 13)
  n_sim <- 10
  vals_hyper <- draw_vals_hyper(prior = prior,
                                n_sim = n_sim)
  vals_hyperrand <- list()
  levels_effect <- letters
  ans <- draw_vals_effect(prior = prior,
                          vals_hyper = vals_hyper,
                          vals_hyperrand = vals_hyperrand,
                          levels_effect = levels_effect,
                          agesex = "other",
                          matrix_along_by = matrix_along_by,
                          matrix_agesex = NULL,
                          n_sim = n_sim)
  expect_identical(dimnames(ans), list(letters, as.character(1:10)))
})

test_that("'draw_vals_effect' works with bage_prior_known", {
  prior <- Known(c(-0.1, 0, 0.1))
  n_sim <- 10
  vals_hyperrand <- list()
  levels_effect <- c("a", "b", "c")
  matrix_along_by = matrix(0:2, nc = 1)
  ans <- draw_vals_effect(prior = prior,
                          vals_hyper = NULL,
                          vals_hyperrand = vals_hyperrand,
                          levels_effect = levels_effect,
                          agesex = "other",
                          matrix_along_by = matrix_along_by,
                          matrix_agesex = NULL,
                          n_sim = n_sim)
  expect_identical(dimnames(ans), list(c("a", "b", "c"), as.character(1:10)))
})

test_that("'draw_vals_effect' works with bage_prior_lin - n_by = 2", {
  prior <- Lin()
  n_sim <- 10
  matrix_along_by  <-  matrix(0:25, nc = 2, dimnames = list(NULL, c("a", "b")))
  vals_hyperrand <- draw_vals_hyperrand(prior = prior,
                                        matrix_along_by = matrix_along_by,
                                        n_sim = n_sim)
  vals_hyper <- draw_vals_hyper(prior = prior,
                                n_sim = n_sim)
  levels_effect <- letters
  ans <- draw_vals_effect(prior = prior,
                          vals_hyper = vals_hyper,
                          vals_hyperrand = vals_hyperrand,
                          levels_effect = levels_effect,
                          agesex = "other",
                          matrix_along_by = matrix_along_by,
                          matrix_agesex = NULL,
                          n_sim = n_sim)
  expect_identical(dimnames(ans),
                   list(letters, as.character(1:10)))
})

test_that("'draw_vals_effect' works with bage_prior_lin - n_by = 4", {
  prior <- Lin(s = 0.01)
  matrix_along_by <- matrix(0:11, nr = 3)
  colnames(matrix_along_by) <- 1:4
  n_sim <- 10
  vals_hyper <- draw_vals_hyper(prior = prior,
                                n_sim = n_sim)
  vals_hyperrand <- draw_vals_hyperrand(prior = prior,
                                        matrix_along_by = matrix_along_by,
                                        n_sim = n_sim)
  levels_effect <- letters[1:12]
  ans <- draw_vals_effect(prior = prior,
                          vals_hyper = vals_hyper,
                          vals_hyperrand = vals_hyperrand,
                          levels_effect = levels_effect,
                          agesex = "other",
                          matrix_along_by = matrix_along_by,
                          matrix_agesex = NULL,
                          n_sim = n_sim)
  expect_identical(dimnames(ans),
                   list(letters[1:12], as.character(1:10)))
})

test_that("'draw_vals_effect' works with bage_prior_linar", {
  prior <- LinAR()
  n_sim <- 10
  matrix_along_by  <-  matrix(0:25, nc = 2, dimnames = list(NULL, c("a", "b")))
  vals_hyperrand <- draw_vals_hyperrand(prior = prior,
                                        matrix_along_by = matrix_along_by,
                                        n_sim = n_sim)
  vals_hyper <- draw_vals_hyper(prior = prior,
                                n_sim = n_sim)
  levels_effect <- letters
  ans <- draw_vals_effect(prior = prior,
                          vals_hyper = vals_hyper,
                          vals_hyperrand = vals_hyperrand,
                          levels_effect = levels_effect,
                          agesex = "other",
                          matrix_along_by = matrix_along_by,
                          matrix_agesex = NULL,
                          n_sim = n_sim)
  expect_identical(dimnames(ans),
                   list(letters, as.character(1:10)))
})

test_that("'draw_vals_effect' works with bage_prior_norm", {
  prior <- N()
  n_sim <- 10
  vals_hyper <- draw_vals_hyper(prior = prior,
                                n_sim = n_sim)
  vals_hyperrand <- list()
  levels_effect <- seq_len(1000)
  matrix_along_by = matrix(0:999, nc = 10)
  ans <- draw_vals_effect(prior = prior,
                          vals_hyperrand = vals_hyperrand,
                          vals_hyper = vals_hyper,
                          levels_effect = levels_effect,
                          agesex = "other",
                          matrix_along_by = matrix_along_by,
                          matrix_agesex = NULL,
                          n_sim = n_sim)
  expect_identical(dimnames(ans),
                   list(as.character(1:1000), as.character(1:10)))
  expect_equal(unname(apply(ans, 2, sd)), vals_hyper$sd, tolerance = 0.05)
})

test_that("'draw_vals_effect' works with bage_prior_normfixed", {
  prior <- NFix(sd = 0.3)
  n_sim <- 10
  vals_hyperrand <- list()
  matrix_along_by = matrix(0:999, nc = 10)
  levels_effect <- seq_len(1000)
  ans <- draw_vals_effect(prior = prior,
                          vals_hyper = NULL,
                          vals_hyperrand = vals_hyperrand,
                          levels_effect = levels_effect,
                          agesex = "other",
                          matrix_along_by = matrix_along_by,
                          matrix_agesex = NULL,
                          n_sim = n_sim)
  expect_identical(dimnames(ans),
                   list(as.character(1:1000), as.character(1:10)))
  expect_equal(unname(apply(ans, 2, sd)), rep(0.3, 10), tolerance = 0.05)
})

test_that("'draw_vals_effect' works with bage_prior_rw - n_by = 1", {
  prior <- RW()
  n_sim <- 10
  vals_hyper <- draw_vals_hyper(prior = prior,
                                n_sim = n_sim)
  vals_hyperrand <- list()
  levels_effect <- letters
  matrix_along_by = matrix(0:25, nc = 1)
  ans <- draw_vals_effect(prior = prior,
                          vals_hyper = vals_hyper,
                          vals_hyperrand = vals_hyperrand,
                          levels_effect = levels_effect,
                          agesex = "other",
                          matrix_along_by = matrix_along_by,
                          matrix_agesex = NULL,
                          n_sim = n_sim)
  expect_identical(dimnames(ans),
                   list(letters, as.character(1:10)))
})

test_that("'draw_vals_effect' works with bage_prior_rw - n_by = 4", {
  prior <- RW(s = 0.01)
  matrix_along_by <- matrix(0:11, nr = 3)
  n_sim <- 10
  vals_hyper <- draw_vals_hyper(prior = prior,
                                n_sim = n_sim)
  vals_hyperrand <- list()
  levels_effect <- letters[1:12]
  ans <- draw_vals_effect(prior = prior,
                          vals_hyper = vals_hyper,
                          vals_hyperrand = vals_hyperrand,
                          levels_effect = levels_effect,
                          agesex = "other",
                          matrix_along_by = matrix_along_by,
                          matrix_agesex = NULL,
                          n_sim = n_sim)
  expect_identical(dimnames(ans),
                   list(letters[1:12], as.character(1:10)))
})

test_that("'draw_vals_effect' works with bage_prior_rwseasfix", {
  prior <- RWSeas(n = 2, s = 0.01, s_seas = 0)
  matrix_along_by <- matrix(0:11, nr = 3)
  n_sim <- 10
  vals_hyper <- draw_vals_hyper(prior = prior,
                                n_sim = n_sim)
  vals_hyperrand <- draw_vals_hyperrand(prior = prior,
                                        vals_hyper = vals_hyper,
                                        matrix_along_by = matrix_along_by,
                                        n_sim = n_sim)
  levels_effect <- letters[1:12]
  ans <- draw_vals_effect(prior = prior,
                          vals_hyper = vals_hyper,
                          vals_hyperrand = vals_hyperrand,
                          levels_effect = levels_effect,
                          agesex = "other",
                          matrix_along_by = matrix_along_by,
                          matrix_agesex = NULL,
                          n_sim = n_sim)
  expect_identical(dimnames(ans),
                   list(letters[1:12], as.character(1:10)))
})

test_that("'draw_vals_effect' works with bage_prior_rwseasvary", {
  prior <- RWSeas(n = 2, s = 0.01, s_seas = 0.5)
  matrix_along_by <- matrix(0:11, nr = 3)
  n_sim <- 10
  vals_hyper <- draw_vals_hyper(prior = prior,
                                n_sim = n_sim)
  vals_hyperrand <- draw_vals_hyperrand(prior = prior,
                                        vals_hyper = vals_hyper,
                                        matrix_along_by = matrix_along_by,
                                        n_sim = n_sim)
  levels_effect <- letters[1:12]
  ans <- draw_vals_effect(prior = prior,
                          vals_hyper = vals_hyper,
                          vals_hyperrand = vals_hyperrand,
                          levels_effect = levels_effect,
                          agesex = "other",
                          matrix_along_by = matrix_along_by,
                          matrix_agesex = NULL,
                          n_sim = n_sim)
  expect_identical(dimnames(ans),
                   list(letters[1:12], as.character(1:10)))
})

test_that("'draw_vals_effect' works with bage_prior_rw2 - n_by = 1", {
  prior <- RW2()
  n_sim <- 10
  vals_hyper <- draw_vals_hyper(prior = prior,
                                n_sim = n_sim)
  vals_hyperrand <- list()
  levels_effect <- letters
  matrix_along_by = matrix(0:25, nc = 1)
  ans <- draw_vals_effect(prior = prior,
                          vals_hyper = vals_hyper,
                          vals_hyperrand = vals_hyperrand,
                          levels_effect = levels_effect,
                          agesex = "other",
                          matrix_along_by = matrix_along_by,
                          matrix_agesex = NULL,
                          n_sim = n_sim)
  expect_identical(dimnames(ans),
                   list(letters, as.character(1:10)))
})

test_that("'draw_vals_effect' works with bage_prior_rw2 - n_by = 4", {
  prior <- RW2(s = 0.01)
  matrix_along_by <- matrix(0:11, nr = 3)
  n_sim <- 10
  vals_hyper <- draw_vals_hyper(prior = prior,
                                n_sim = n_sim)
  vals_hyperrand <- list()
  levels_effect <- letters[1:12]
  ans <- draw_vals_effect(prior = prior,
                          vals_hyper = vals_hyper,
                          vals_hyperrand = vals_hyperrand,
                          levels_effect = levels_effect,
                          agesex = "other",
                          matrix_along_by = matrix_along_by,
                          matrix_agesex = NULL,
                          n_sim = n_sim)
  expect_identical(dimnames(ans),
                   list(letters[1:12], as.character(1:10)))
})

test_that("'draw_vals_effect' works with bage_prior_rw2seasfix", {
  prior <- RW2Seas(n = 2, s = 0.01, s_seas = 0)
  matrix_along_by <- matrix(0:11, nr = 3)
  n_sim <- 10
  vals_hyper <- draw_vals_hyper(prior = prior,
                                n_sim = n_sim)
  vals_hyperrand <- draw_vals_hyperrand(prior = prior,
                                        vals_hyper = vals_hyper,
                                        matrix_along_by = matrix_along_by,
                                        n_sim = n_sim)
  levels_effect <- letters[1:12]
  ans <- draw_vals_effect(prior = prior,
                          vals_hyper = vals_hyper,
                          vals_hyperrand = vals_hyperrand,
                          levels_effect = levels_effect,
                          agesex = "other",
                          matrix_along_by = matrix_along_by,
                          matrix_agesex = NULL,
                          n_sim = n_sim)
  expect_identical(dimnames(ans),
                   list(letters[1:12], as.character(1:10)))
})

test_that("'draw_vals_effect' works with bage_prior_rw2seasvary", {
  prior <- RW2Seas(n = 2, s = 0.01, s_seas = 0.5)
  matrix_along_by <- matrix(0:11, nr = 3)
  n_sim <- 10
  vals_hyper <- draw_vals_hyper(prior = prior,
                                n_sim = n_sim)
  vals_hyperrand <- draw_vals_hyperrand(prior = prior,
                                        vals_hyper = vals_hyper,
                                        matrix_along_by = matrix_along_by,
                                        n_sim = n_sim)
  levels_effect <- letters[1:12]
  ans <- draw_vals_effect(prior = prior,
                          vals_hyper = vals_hyper,
                          vals_hyperrand = vals_hyperrand,
                          levels_effect = levels_effect,
                          agesex = "other",
                          matrix_along_by = matrix_along_by,
                          matrix_agesex = NULL,
                          n_sim = n_sim)
  expect_identical(dimnames(ans),
                   list(letters[1:12], as.character(1:10)))
})

test_that("'draw_vals_effect' works with bage_prior_spline - n_by = 1", {
  prior <- Sp()
  n_sim <- 10
  vals_hyper <- draw_vals_hyper(prior = prior,
                                n_sim = n_sim)
  vals_hyperrand <- list()
  levels_effect <- letters
  matrix_along_by = matrix(0:25, nc = 1)
  ans <- draw_vals_effect(prior = prior,
                          vals_hyper = vals_hyper,
                          vals_hyperrand = vals_hyperrand,
                          levels_effect = levels_effect,
                          agesex = "other",
                          matrix_along_by = matrix_along_by,
                          matrix_agesex = NULL,
                          n_sim = n_sim)
  expect_identical(dimnames(ans),
                   list(letters, as.character(1:10)))
})

test_that("'draw_vals_effect' works with bage_prior_spline - n_by = 2", {
  prior <- Sp()
  n_sim <- 10
  vals_hyper <- draw_vals_hyper(prior = prior,
                                n_sim = n_sim)
  vals_hyperrand <- list()
  levels_effect <- letters[1:12]
  matrix_along_by  <- t(matrix(0:11, nc = 6))
  ans <- draw_vals_effect(prior = prior,
                          vals_hyper = vals_hyper,
                          vals_hyperrand = vals_hyperrand,
                          levels_effect = levels_effect,
                          agesex = "other",
                          matrix_along_by = matrix_along_by,
                          matrix_agesex = NULL,
                          n_sim = n_sim)
  expect_identical(dimnames(ans),
                   list(letters[1:12], as.character(1:10)))
})

test_that("'draw_vals_effect' works with bage_prior_svd - age main effect", {
  prior <- SVD(HMD)
  n_sim <- 10
  vals_hyper <- draw_vals_hyper(prior = prior,
                                n_sim = n_sim)
  vals_hyperrand <- list()
  levels_age <- c(0:79, "80+")
  levels_sex <- c("F", "M")
  matrix_along_by <- matrix(0:80, nc = 1)
  matrix_agesex <- make_matrix_along_by(i_along = 1L,
                                        dim = 81L,
                                        dimnames = list(levels_age))
  ans <- draw_vals_effect(prior = prior,
                          vals_hyper = vals_hyper,
                          vals_hyperrand = vals_hyperrand,
                          levels_effect = levels_age,
                          levels_age = levels_age,
                          levels_sex = levels_sex,
                          agesex = "age",
                          matrix_agesex = matrix_agesex,
                          matrix_along_by = matrix_along_by,
                          n_sim = n_sim)
  expect_identical(dimnames(ans),
                   list(levels_age, as.character(1:10)))
})

test_that("'draw_vals_effect' works with bage_prior_svd - age:sex interaction", {
  prior <- SVDS(HMD)
  n_sim <- 10
  vals_hyper <- draw_vals_hyper(prior = prior,
                                n_sim = n_sim)
  vals_hyperrand <- list()
  levels_age <- c(0:79, "80+")
  levels_sex <- c("F", "M")
  levels_effect <- paste(rep(levels_sex, each = 81), levels_age, sep = ".")
  matrix_along_by <- NULL
  matrix_agesex <- make_matrix_along_by(i_along = 1:2,
                                        dim = c(81L, 2L),
                                        dimnames = list(levels_age, levels_sex))
  ans <- draw_vals_effect(prior = prior,
                          vals_hyper = vals_hyper,
                          vals_hyperrand = vals_hyperrand,
                          levels_effect = levels_effect,
                          levels_age = levels_age,
                          levels_sex = levels_sex,
                          agesex = "sex:age",
                          matrix_along_by = matrix_along_by,
                          matrix_agesex = matrix_agesex,
                          n_sim = n_sim)
  expect_identical(dim(ans), c(162L, 10L))
})

test_that("'draw_vals_effect' works with bage_prior_svd, with region", {
  prior <- SVD(HMD)
  n_sim <- 10
  vals_hyper <- draw_vals_hyper(prior = prior,
                                n_sim = n_sim)
  vals_hyperrand <- list()
  levels_age <- c(0:79, "80+")
  levels_region <- c("A", "B")
  levels_sex <- NULL
  levels_effect <- paste(levels_age, rep(levels_region, each = 81), sep = ".")
  matrix_along_by <- make_matrix_along_by(i_along = 2L,
                                          dim = c(2L, 81L),
                                          dimnames = list(levels_region, levels_age))
  matrix_agesex <- make_matrix_along_by(i_along = 2L,
                                        dim = c(2L, 81L),
                                        dimnames = list(levels_region, levels_age))
  levels_effect <- paste(rep(levels_age, each = 2), levels_region, sep = ".")
  ans <- draw_vals_effect(prior = prior,
                          vals_hyper = vals_hyper,
                          vals_hyperrand = vals_hyperrand,
                          levels_effect = levels_effect,
                          levels_age = levels_age,
                          levels_sex = levels_sex,
                          agesex = "age:other",
                          matrix_along_by = matrix_along_by,
                          matrix_agesex = matrix_agesex,
                          n_sim = n_sim)
  expect_identical(dimnames(ans),
                   list(levels_effect, as.character(1:10)))
})

test_that("'draw_vals_effect' works with bage_prior_svd - age main effect", {
  prior <- SVD(HMD)
  n_sim <- 10
  vals_hyper <- draw_vals_hyper(prior = prior,
                                n_sim = n_sim)
  vals_hyperrand <- list()
  levels_age <- c(0:79, "80+")
  levels_sex <- NULL
  matrix_along_by <- matrix(0:80, nc = 1)
  ans <- draw_vals_effect(prior = prior,
                          vals_hyper = vals_hyper,
                          vals_hyperrand = vals_hyperrand,
                          levels_effect = levels_age,
                          levels_age = levels_age,
                          levels_sex = levels_sex,
                          agesex = "age",
                          matrix_along_by = matrix_along_by,
                          matrix_agesex = matrix_along_by,
                          n_sim = n_sim)
  expect_identical(dimnames(ans),
                   list(levels_age, as.character(1:10)))
})

test_that("'draw_vals_effect' works with bage_prior_svd - age:sex interaction - indep", {
  prior <- SVDS(HMD)
  n_sim <- 10
  vals_hyper <- draw_vals_hyper(prior = prior,
                                n_sim = n_sim)
  vals_hyperrand <- list()
  levels_age <- c(0:79, "80+")
  levels_sex <- c("F", "M")
  levels_effect <- paste(rep(levels_sex, each = 81), levels_age, sep = ".")
  matrix_agesex <- matrix(0:161, nc = 1)
  ans <- draw_vals_effect(prior = prior,
                          vals_hyper = vals_hyper,
                          vals_hyperrand = vals_hyperrand,
                          levels_effect = levels_effect,
                          levels_age = levels_age,
                          levels_sex = levels_sex,
                          agesex = "sex:age",
                          matrix_along_by = NULL,
                          matrix_agesex = matrix_agesex, 
                          n_sim = n_sim)
  expect_identical(dim(ans), c(162L, 10L))
})

test_that("'draw_vals_effect' works with bage_prior_svd - age:sex interaction - joint", {
  prior <- SVDS(HMD, joint = TRUE)
  n_sim <- 10
  vals_hyper <- draw_vals_hyper(prior = prior,
                                n_sim = n_sim)
  vals_hyperrand <- list()
  levels_age <- c(0:79, "80+")
  levels_sex <- c("F", "M")
  levels_effect <- paste(rep(levels_sex, each = 81), levels_age, sep = ".")
  matrix_agesex <- matrix(0:161, nc = 1)
  ans <- draw_vals_effect(prior = prior,
                          vals_hyper = vals_hyper,
                          vals_hyperrand = vals_hyperrand,
                          levels_effect = levels_effect,
                          levels_age = levels_age,
                          levels_sex = levels_sex,
                          agesex = "sex:age",
                          matrix_along_by = NULL,
                          matrix_agesex = matrix_agesex, 
                          n_sim = n_sim)
  expect_identical(dim(ans), c(162L, 10L))
})


## draw_vals_hyper ------------------------------------------------------------

test_that("'draw_vals_hyper' works with bage_prior_ar", {
  prior <- AR(n = 3)
  ans <- draw_vals_hyper(prior = prior,
                         n_sim = 10)
  expect_identical(names(ans), c("coef", "sd"))
  expect_identical(dim(ans$coef), c(3L, 10L))
})

test_that("'draw_vals_hyper' works with bage_prior_known", {
  prior <- Known(c(0.1, 0.3))
  ans <- draw_vals_hyper(prior = prior,
                         n_sim = 10)
  expect_identical(ans, list())
})

test_that("'draw_vals_hyper' works with bage_prior_lin", {
  prior <- Lin()
  ans <- draw_vals_hyper(prior = prior,
                         n_sim = 10)
  expect_identical(names(ans), "sd")
  expect_identical(length(ans$sd), 10L)
})

test_that("'draw_vals_hyper' works with bage_prior_linar", {
  prior <- LinAR()
  ans <- draw_vals_hyper(prior = prior,
                         n_sim = 10)
  expect_identical(names(ans), c("sd", "coef"))
  expect_identical(length(ans$sd), 10L)
})

test_that("'draw_vals_hyper' works with bage_prior_norm", {
  prior <- N()
  ans <- draw_vals_hyper(prior = prior,
                         n_sim = 10)
  expect_identical(names(ans), "sd")
  expect_identical(length(ans$sd), 10L)
})

test_that("'draw_vals_hyper' works with bage_prior_normfixed", {
  prior <- NFix()
  ans <- draw_vals_hyper(prior = prior,
                         n_sim = 10)
  expect_identical(ans, list())
})

test_that("'draw_vals_hyper' works with bage_prior_rw", {
  prior <- RW()
  ans <- draw_vals_hyper(prior = prior,
                         n_sim = 10)
  expect_identical(names(ans), "sd")
  expect_identical(length(ans$sd), 10L)
})

test_that("'draw_vals_hyper' works with bage_prior_rwseasfix", {
  prior <- RWSeas(n = 2, s_seas = 0)
  ans <- draw_vals_hyper(prior = prior,
                         n_sim = 10)
  expect_identical(names(ans), "sd")
  expect_identical(length(ans$sd), 10L)
})

test_that("'draw_vals_hyper' works with bage_prior_rwseasvary", {
  prior <- RWSeas(n = 2, s_seas = 0.1)
  ans <- draw_vals_hyper(prior = prior,
                         n_sim = 10)
  expect_identical(names(ans), c("sd_seas", "sd"))
  expect_identical(length(ans$sd_seas), 10L)
  expect_identical(length(ans$sd), 10L)
})

test_that("'draw_vals_hyper' works with bage_prior_rw2", {
  prior <- RW2()
  ans <- draw_vals_hyper(prior = prior,
                         n_sim = 10)
  expect_identical(names(ans), "sd")
  expect_identical(length(ans$sd), 10L)
})

test_that("'draw_vals_hyper' works with bage_prior_rw2seasfix", {
  prior <- RW2Seas(n = 2, s_seas = 0)
  ans <- draw_vals_hyper(prior = prior,
                         n_sim = 10)
  expect_identical(names(ans), "sd")
  expect_identical(length(ans$sd), 10L)
})

test_that("'draw_vals_hyper' works with bage_prior_rw2seasvary", {
  prior <- RW2Seas(n = 2, s_seas = 0.1)
  ans <- draw_vals_hyper(prior = prior,
                         n_sim = 10)
  expect_identical(names(ans), c("sd_seas", "sd"))
  expect_identical(length(ans$sd_seas), 10L)
  expect_identical(length(ans$sd), 10L)
})

test_that("'draw_vals_hyper' works with bage_prior_spline", {
  prior <- Sp()
  ans <- draw_vals_hyper(prior = prior,
                         n_sim = 10)
  expect_identical(names(ans), "sd")
  expect_identical(length(ans$sd), 10L)
})

test_that("'draw_vals_hyper' works with bage_prior_svd", {
  prior <- SVD(HMD)
  ans <- draw_vals_hyper(prior = prior,
                         n_sim = 10)
  expect_identical(ans, list())
})


## 'draw_vals_hyperrand' ------------------------------------------------------

test_that("'draw_vals_hyperrand' works with bage_prior_lin", {
  set.seed(0)
  prior <- Lin()
  matrix_along_by <- matrix(0:11, nr = 3)
  colnames(matrix_along_by) <- 1:4
  n_sim <- 10
  vals_hyper <- draw_vals_hyper(prior = prior,
                                n_sim = n_sim)
  ans <- draw_vals_hyperrand(prior = prior,
                             vals_hyper = vals_hyper,
                             matrix_along_by = matrix_along_by,
                             n_sim = n_sim)
  expect_identical(names(ans), "slope")
  expect_identical(lengths(ans),
                   c(slope = 40L))
})

test_that("'draw_vals_hyperrand' works with bage_prior_linar", {
  set.seed(0)
  prior <- LinAR()
  matrix_along_by <- matrix(0:11, nr = 12)
  n_sim <- 10
  vals_hyper <- draw_vals_hyper(prior = prior,
                                n_sim = n_sim)
  ans <- draw_vals_hyperrand(prior = prior,
                             vals_hyper = vals_hyper,
                             matrix_along_by = matrix_along_by,
                             n_sim = n_sim)
  expect_identical(names(ans), "slope")
  expect_identical(lengths(ans),
                   c(slope = 10L))
})

test_that("'draw_vals_hyperrand' works with bage_prior_rwseasfix", {
  set.seed(0)
  prior <- RWSeas(n = 2, s = 0.01, s_seas = 0)
  matrix_along_by <- matrix(0:11, nr = 3)
  n_sim <- 10
  vals_hyper <- draw_vals_hyper(prior = prior,
                                n_sim = n_sim)
  set.seed(1)
  ans_obtained <- draw_vals_hyperrand(prior = prior,
                                      vals_hyper = vals_hyper,
                                      matrix_along_by = matrix_along_by,
                                      n_sim = n_sim)
  set.seed(1)
  ans_expected <- list(seas = draw_vals_seasfix(n = 2,
                                                matrix_along_by = matrix_along_by,
                                                n_sim = n_sim))
  expect_identical(ans_obtained, ans_expected)
})

test_that("'draw_vals_hyperrand' works with bage_prior_rwseasvary", {
  set.seed(0)
  prior <- RWSeas(n = 2, s = 0.01, s_seas = 0.4)
  matrix_along_by <- matrix(0:11, nr = 3)
  n_sim <- 10
  vals_hyper <- draw_vals_hyper(prior = prior,
                                n_sim = n_sim)
  set.seed(1)
  ans_obtained <- draw_vals_hyperrand(prior = prior,
                                      vals_hyper = vals_hyper,
                                      matrix_along_by = matrix_along_by,
                                      n_sim = n_sim)
  set.seed(1)
  ans_expected <- list(seas = draw_vals_seasvary(n = 2,
                                                 sd_seas = vals_hyper$sd_seas,
                                                 matrix_along_by = matrix_along_by))
  expect_identical(ans_obtained, ans_expected)
})

test_that("'draw_vals_hyperrand' works with bage_prior_rw2seasfix", {
  set.seed(0)
  prior <- RW2Seas(n = 2, s = 0.01, s_seas = 0)
  matrix_along_by <- matrix(0:11, nr = 3)
  n_sim <- 10
  vals_hyper <- draw_vals_hyper(prior = prior,
                                n_sim = n_sim)
  set.seed(1)
  ans_obtained <- draw_vals_hyperrand(prior = prior,
                                      vals_hyper = vals_hyper,
                                      matrix_along_by = matrix_along_by,
                                      n_sim = n_sim)
  set.seed(1)
  ans_expected <- list(seas = draw_vals_seasfix(n = 2,
                                                matrix_along_by = matrix_along_by,
                                                n_sim = n_sim))
  expect_identical(ans_obtained, ans_expected)
})

test_that("'draw_vals_hyperrand' works with bage_prior_rw2seasvary", {
  set.seed(0)
  prior <- RW2Seas(n = 2, s = 0.01, s_seas = 0.4)
  matrix_along_by <- matrix(0:11, nr = 3)
  n_sim <- 10
  vals_hyper <- draw_vals_hyper(prior = prior,
                                n_sim = n_sim)
  set.seed(1)
  ans_obtained <- draw_vals_hyperrand(prior = prior,
                                      vals_hyper = vals_hyper,
                                      matrix_along_by = matrix_along_by,
                                      n_sim = n_sim)
  set.seed(1)
  ans_expected <- list(seas = draw_vals_seasvary(n = 2,
                                                 sd_seas = vals_hyper$sd_seas,
                                                 matrix_along_by = matrix_along_by))
  expect_identical(ans_obtained, ans_expected)
})



## forecast_term ------------------------------------------------------------

test_that("'forecast_term' raises error with time-varying term for which method does not exist", {
  set.seed(0)
  prior <- Known(value = 1:5)
  components <- tibble::tibble(term = "year",
                               component = "effect",
                               level = letters[1:5],
                               .fitted = c(-2, -1, 0, 1, 2))
  matrix_along_by_est <- matrix(0:4, nr = 5)
  matrix_along_by_forecast <- matrix(0:5, nr = 6)
  levels_forecast <- letters[6:11]
  expect_error(forecast_term(prior = prior,
                             nm_prior = "year",
                             components = components,
                             matrix_along_by_est = matrix_along_by_est,
                             matrix_along_by_forecast = matrix_along_by_forecast,
                             levels_forecast = levels_forecast),
               "Can't forecast term \"year\"")
})

test_that("'forecast_term' works with bage_prior_ar", {
  set.seed(0)
  prior <- AR(n = 3)
  components <- vctrs::vec_rbind(tibble::tibble(term = "year",
                                                component = "hyper",
                                                level = c("coef", "coef", "coef", "sd"),
                                                .fitted = rvec::runif_rvec(n = 4, n_draw = 10)),
                                 tibble::tibble(term = "year",
                                                component = "effect",
                                                level = letters[1:5],
                                                .fitted = rvec::rnorm_rvec(n = 5, n_draw = 10)))
  matrix_along_by_est <- matrix(0:4, nr = 5)
  matrix_along_by_forecast <- matrix(0:5, nr = 6)
  levels_forecast <- letters[6:11]
  set.seed(1)
  ans_obtained <- forecast_term(prior = prior,
                                nm_prior = "year",
                                components = components,
                                matrix_along_by_est = matrix_along_by_est,
                                matrix_along_by_forecast = matrix_along_by_forecast,
                                levels_forecast = levels_forecast)
  ans_expected <- tibble::tibble(term = "year",
                                 component = "effect",
                                 level = letters[6:11])
  ans_expected$.fitted <- rvec::rnorm_rvec(n = 6, n_draw = 10)
  coef <- components$.fitted[components$level == "coef"]
  sd <- components$.fitted[components$level == "sd"]
  set.seed(1)
  ans_expected$.fitted[1] <- rvec::rnorm_rvec(n = 1,
                                              mean = sum(coef * components$.fitted[4 + 3:5]),
                                              sd = sd)
  ans_expected$.fitted[2] <- rvec::rnorm_rvec(n = 1,
                                              mean = sum(coef * c(components$.fitted[4 + 4:5],
                                                                  ans_expected$.fitted[1])),
                                              sd = sd)
  ans_expected$.fitted[3] <- rvec::rnorm_rvec(n = 1,
                                              mean = sum(coef * c(components$.fitted[4 + 5],
                                                                  ans_expected$.fitted[1:2])),
                                              sd = sd)
  ans_expected$.fitted[4] <- rvec::rnorm_rvec(n = 1,
                                              mean = sum(coef * ans_expected$.fitted[1:3]),
                                              sd = sd)
  ans_expected$.fitted[5] <- rvec::rnorm_rvec(n = 1,
                                              mean = sum(coef * ans_expected$.fitted[2:4]),
                                              sd = sd)
  ans_expected$.fitted[6] <- rvec::rnorm_rvec(n = 1,
                                              mean = sum(coef * ans_expected$.fitted[3:5]),
                                              sd = sd)
  expect_equal(ans_obtained, ans_expected)
})

test_that("'forecast_term' works with bage_prior_lin", {
  set.seed(0)
  prior <- Lin()
  components <- vctrs::vec_rbind(tibble::tibble(term = "year:reg",
                                                component = "hyper",
                                                level = c("slope", "slope", "sd"),
                                                .fitted = rvec::runif_rvec(n = 3, n_draw = 10)),
                                 tibble::tibble(term = "year:reg",
                                                component = "effect",
                                                level = letters[1:10],
                                                .fitted = rvec::rnorm_rvec(n = 10, n_draw = 10)))
  matrix_along_by_est <- matrix(0:9, nr = 5)
  matrix_along_by_forecast <- matrix(0:11, nr = 6)
  levels_forecast <- letters[11:22]
  set.seed(1)
  ans_obtained <- forecast_term(prior = prior,
                                nm_prior = "year:reg",
                                components = components,
                                matrix_along_by_est = matrix_along_by_est,
                                matrix_along_by_forecast = matrix_along_by_forecast,
                                levels_forecast = levels_forecast)
  ans_expected <- tibble::tibble(term = "year:reg",
                                 component = "effect",
                                 level = letters[11:22])
  slope <- components$.fitted[components$level == "slope"]
  sd <- components$.fitted[components$level == "sd"]
  set.seed(1)
  q <- seq(from = 1.5, by = 0.5, length.out = 6)
  ans_expected$.fitted <- rvec::rnorm_rvec(n = 12,
                                           mean = c(q * slope[1], q * slope[2]),
                                           sd = sd)
  expect_equal(ans_obtained, ans_expected)
})

test_that("'forecast_term' works with bage_prior_linar - n__by = 1", {
  set.seed(0)
  prior <- LinAR()
  components <- vctrs::vec_rbind(tibble::tibble(term = "year",
                                                component = "hyper",
                                                level = c("slope", "sd", "coef1", "coef2"),
                                                .fitted = rvec::runif_rvec(n = 4, n_draw = 10)),
                                 tibble::tibble(term = "year",
                                                component = "effect",
                                                level = letters[1:5],
                                                .fitted = rvec::rnorm_rvec(n = 5, n_draw = 10)))
  matrix_along_by_est <- matrix(0:4, nr = 5)
  matrix_along_by_forecast <- matrix(0:5, nr = 6)
  levels_forecast <- letters[6:11]
  set.seed(1)
  ans_obtained <- forecast_term(prior = prior,
                                nm_prior = "year",
                                components = components,
                                matrix_along_by_est = matrix_along_by_est,
                                matrix_along_by_forecast = matrix_along_by_forecast,
                                levels_forecast = levels_forecast)
  ans_expected <- tibble::tibble(term = "year",
                                 component = "effect",
                                 level = letters[6:11])
  ans_expected$.fitted <- rvec::rnorm_rvec(n = 6, n_draw = 10)
  slope <- components$.fitted[components$level == "slope"]
  sd <- components$.fitted[components$level == "sd"]
  coef <- components$.fitted[components$level %in% c("coef1", "coef2")]
  q_est <- seq(from = -1, by = 0.5, to = 1)
  q_forecast <- seq(from = 1.5, by = 0.5, length.out = 6)
  set.seed(1)
  ans_expected$.fitted[1] <-
    rvec::rnorm_rvec(n = 1,
                     mean = sum(coef * (components$.fitted[4 + 4:5] - slope * q_est[4:5])) +
                       slope * q_forecast[1],
                     sd = sd)
  ans_expected$.fitted[2] <-
    rvec::rnorm_rvec(n = 1,
                     mean = sum(coef * c(components$.fitted[4 + 5] - slope * q_est[5],
                                         ans_expected$.fitted[1] - slope * q_forecast[1]))+
                       slope * q_forecast[2],
                     sd = sd)
  ans_expected$.fitted[3] <-
    rvec::rnorm_rvec(n = 1,
                     mean = sum(coef * (ans_expected$.fitted[1:2] - slope * q_forecast[1:2]))+
                       slope * q_forecast[3],
                     sd = sd)
  ans_expected$.fitted[4] <-
    rvec::rnorm_rvec(n = 1,
                     mean = sum(coef * (ans_expected$.fitted[2:3] - slope * q_forecast[2:3])) +
                       slope * q_forecast[4],
                     sd = sd)
  ans_expected$.fitted[5] <-
    rvec::rnorm_rvec(n = 1,
                     mean = sum(coef * (ans_expected$.fitted[3:4] - slope * q_forecast[3:4]))+
                       slope * q_forecast[5],
                     sd = sd)
  ans_expected$.fitted[6] <-
    rvec::rnorm_rvec(n = 1,
                     mean = sum(coef * (ans_expected$.fitted[4:5] - slope * q_forecast[4:5]))+
                       slope * q_forecast[6],
                     sd = sd)
  expect_equal(ans_obtained, ans_expected)
})

test_that("'forecast_term' works with bage_prior_norm", {
  set.seed(0)
  prior <- N()
  components <- vctrs::vec_rbind(tibble::tibble(term = "year",
                                                component = "hyper",
                                                level = "sd",
                                                .fitted = rvec::runif_rvec(n = 1, n_draw = 10)),
                                 tibble::tibble(term = "year",
                                                component = "effect",
                                                level = letters[1:5],
                                                .fitted = rvec::rnorm_rvec(n = 5, n_draw = 10)))
  matrix_along_by_est <- matrix(0:4, nr = 5)
  matrix_along_by_forecast <- matrix(0:5, nr = 6)
  levels_forecast <- letters[6:11]
  set.seed(1)
  ans_obtained <- forecast_term(prior = prior,
                                nm_prior = "year",
                                components = components,
                                matrix_along_by_est = matrix_along_by_est,
                                matrix_along_by_forecast = matrix_along_by_forecast,
                                levels_forecast = levels_forecast)
  ans_expected <- tibble::tibble(term = "year",
                                 component = "effect",
                                 level = letters[6:11])
  sd <- components$.fitted[components$level == "sd"]
  set.seed(1)
  ans_expected$.fitted <- rvec::rnorm_rvec(n = 6, mean = 0, sd = sd)
  expect_equal(ans_obtained, ans_expected)
})

test_that("'forecast_term' works with bage_prior_normfixed", {
  set.seed(0)
  prior <- NFix()
  components <- tibble::tibble(term = "year",
                               component = "effect",
                               level = letters[1:5],
                               .fitted = rvec::rnorm_rvec(n = 5, n_draw = 10))
  matrix_along_by_est <- matrix(0:4, nr = 5)
  matrix_along_by_forecast <- matrix(0:5, nr = 6)
  levels_forecast <- letters[6:11]
  set.seed(1)
  ans_obtained <- forecast_term(prior = prior,
                                nm_prior = "year",
                                components = components,
                                matrix_along_by_est = matrix_along_by_est,
                                matrix_along_by_forecast = matrix_along_by_forecast,
                                levels_forecast = levels_forecast)
  ans_expected <- tibble::tibble(term = "year",
                                 component = "effect",
                                 level = letters[6:11])
  set.seed(1)
  ans_expected$.fitted <- rvec::rnorm_rvec(n = 6, mean = 0, sd = 1, n_draw = 10)
  expect_equal(ans_obtained, ans_expected)
})

test_that("'forecast_term' works with bage_prior_rw - n_by = 1", {
  set.seed(0)
  prior <- RW()
  components <- vctrs::vec_rbind(tibble::tibble(term = "year",
                                                component = "hyper",
                                                level = "sd",
                                                .fitted = rvec::runif_rvec(n = 1, n_draw = 10)),
                                 tibble::tibble(term = "year",
                                                component = "effect",
                                                level = letters[1:5],
                                                .fitted = rvec::rnorm_rvec(n = 5, n_draw = 10)))
  matrix_along_by_est <- matrix(0:4, nr = 5)
  matrix_along_by_forecast <- matrix(0:5, nr = 6)
  levels_forecast <- letters[6:11]
  set.seed(1)
  ans_obtained <- forecast_term(prior = prior,
                                nm_prior = "year",
                                components = components,
                                matrix_along_by_est = matrix_along_by_est,
                                matrix_along_by_forecast = matrix_along_by_forecast,
                                levels_forecast = levels_forecast)
  ans_expected <- tibble::tibble(term = "year",
                                 component = "effect",
                                 level = letters[6:11])
  ans_expected$.fitted <- rvec::rnorm_rvec(n = 6, n_draw = 10)
  sd <- components$.fitted[components$level == "sd"]
  set.seed(1)
  ans_expected$.fitted[1] <- rvec::rnorm_rvec(n = 1,
                                              mean = components$.fitted[6],
                                              sd = sd)
  for (i in 2:6)
    ans_expected$.fitted[i] <- rvec::rnorm_rvec(n = 1,
                                                mean = ans_expected$.fitted[i-1],
                                                sd = sd)
  expect_equal(ans_obtained, ans_expected)
})

test_that("'forecast_term' works with bage_prior_rw - n_by = 2", {
  set.seed(0)
  prior <- RW()
  components <- vctrs::vec_rbind(tibble::tibble(term = "year:reg",
                                                component = "hyper",
                                                level = "sd",
                                                .fitted = rvec::runif_rvec(n = 1, n_draw = 10)),
                                 tibble::tibble(term = "year:reg",
                                                component = "effect",
                                                level = letters[1:10],
                                                .fitted = rvec::rnorm_rvec(n = 10, n_draw = 10)))
  matrix_along_by_est <- matrix(0:9, nr = 5)
  matrix_along_by_forecast <- matrix(0:11, nr = 6)
  levels_forecast <- letters[11:22]
  set.seed(1)
  ans_obtained <- forecast_term(prior = prior,
                                nm_prior = "year:reg",
                                components = components,
                                matrix_along_by_est = matrix_along_by_est,
                                matrix_along_by_forecast = matrix_along_by_forecast,
                                levels_forecast = levels_forecast)
  ans_expected <- tibble::tibble(term = "year:reg",
                                 component = "effect",
                                 level = letters[11:22])
  ans_expected$.fitted <- rvec::rnorm_rvec(n = 12, n_draw = 10)
  sd <- components$.fitted[components$level == "sd"]
  set.seed(1)
  ans_expected$.fitted[1] <- rvec::rnorm_rvec(n = 1,
                                              mean = components$.fitted[1 + 5],
                                              sd = sd)
  for (i in 2:6)
    ans_expected$.fitted[i] <- rvec::rnorm_rvec(n = 1,
                                                mean = ans_expected$.fitted[i-1],
                                                sd = sd)
  ans_expected$.fitted[7] <- rvec::rnorm_rvec(n = 1,
                                              mean = components$.fitted[1 + 10],
                                              sd = sd)
  for (i in 8:12)
    ans_expected$.fitted[i] <- rvec::rnorm_rvec(n = 1,
                                                mean = ans_expected$.fitted[i-1],
                                                sd = sd)
  expect_equal(ans_obtained, ans_expected)
})

test_that("'forecast_term' works with bage_prior_rwseasfix", {
  set.seed(0)
  prior <- RWSeas(n = 2, s_seas = 0)
  components <- vctrs::vec_rbind(tibble::tibble(term = "year",
                                                component = "hyper",
                                                level = "sd",
                                                .fitted = rvec::runif_rvec(n = 1, n_draw = 10)),
                                 tibble::tibble(term = "year",
                                                component = "effect",
                                                level = letters[1:5],
                                                .fitted = rvec::rnorm_rvec(n = 5, n_draw = 10)),
                                 tibble::tibble(term = "year",
                                                component = "seasonal",
                                                level = letters[1:5],
                                                .fitted = rep(rvec::rnorm_rvec(n = 2, n_draw = 10),
                                                              length.out = 5)))
  trend <- components$.fitted[2:6] - components$.fitted[7:11]
  components <- vctrs::vec_rbind(components,
                                 tibble::tibble(term = "year",
                                                component = "trend",
                                                level = letters[1:5],
                                                .fitted = trend))
  matrix_along_by_est <- matrix(0:4, nr = 5)
  matrix_along_by_forecast <- matrix(0:5, nr = 6)
  levels_forecast <- letters[6:11]
  set.seed(1)
  ans_obtained <- forecast_term(prior = prior,
                                nm_prior = "year",
                                components = components,
                                matrix_along_by_est = matrix_along_by_est,
                                matrix_along_by_forecast = matrix_along_by_forecast,
                                levels_forecast = levels_forecast)
  ans_expected <- tibble::tibble(term = "year",
                                 component = rep(c("effect", "trend", "seasonal"), each = 6),
                                 level = rep(letters[6:11], 3))
  ans_expected$.fitted <- rvec::rnorm_rvec(n = 18, n_draw = 10)
  rw_est <- components$.fitted[12:16]
  rw_forecast <- rvec::rnorm_rvec(n = 6, n_draw = 10)
  sd <- components$.fitted[[1]]
  set.seed(1)
  rw_forecast[1] <- rvec::rnorm_rvec(n = 1, mean = rw_est[5], sd = sd)
  for (i in 2:6)
    rw_forecast[i] <- rvec::rnorm_rvec(n = 1, mean = rw_forecast[i-1], sd = sd)
  ans_expected$.fitted[13] <- components$.fitted[8]
  ans_expected$.fitted[14] <- components$.fitted[7]
  ans_expected$.fitted[15] <- components$.fitted[8]
  ans_expected$.fitted[16] <- components$.fitted[7]
  ans_expected$.fitted[17] <- components$.fitted[8]
  ans_expected$.fitted[18] <- components$.fitted[7]
  effect_forecast <- ans_expected$.fitted[13:18] + rw_forecast
  ans_expected$.fitted[1:6] <- effect_forecast
  ans_expected$.fitted[7:12] <- rw_forecast
  expect_equal(ans_obtained, ans_expected)
})

test_that("'forecast_term' works with bage_prior_rwseasvary", {
  set.seed(0)
  prior <- RWSeas(n = 2)
  components <- vctrs::vec_rbind(tibble::tibble(term = "year",
                                                component = "hyper",
                                                level = c("sd_seas", "sd"),
                                                .fitted = rvec::runif_rvec(n = 2, n_draw = 10)),
                                 tibble::tibble(term = "year",
                                                component = "effect",
                                                level = letters[1:5],
                                                .fitted = rvec::rnorm_rvec(n = 5, n_draw = 10)),
                                 tibble::tibble(term = "year",
                                                component = "seasonal",
                                                level = letters[1:5],
                                                .fitted = rep(rvec::rnorm_rvec(n = 2, n_draw = 10),
                                                              length.out = 5)))
  trend <- components$.fitted[3:7] - components$.fitted[8:12]
  components <- vctrs::vec_rbind(components,
                                 tibble::tibble(term = "year",
                                                component = "trend",
                                                level = letters[1:5],
                                                .fitted = trend))
  matrix_along_by_est <- matrix(0:4, nr = 5)
  matrix_along_by_forecast <- matrix(0:5, nr = 6)
  levels_forecast <- letters[6:11]
  set.seed(1)
  ans_obtained <- forecast_term(prior = prior,
                                nm_prior = "year",
                                components = components,
                                matrix_along_by_est = matrix_along_by_est,
                                matrix_along_by_forecast = matrix_along_by_forecast,
                                levels_forecast = levels_forecast)
  ans_expected <- tibble::tibble(term = "year",
                                 component = rep(c("effect", "trend", "seasonal"), each = 6),
                                 level = rep(letters[6:11], 3))
  ans_expected$.fitted <- rvec::rnorm_rvec(n = 18, n_draw = 10)
  rw_est <- components$.fitted[13:17]
  rw_forecast <- rvec::rnorm_rvec(n = 6, n_draw = 10)
  sd_seas <- components$.fitted[[1]]
  sd <- components$.fitted[[2]]
  set.seed(1)
  rw_forecast[1] <- rvec::rnorm_rvec(n = 1, mean = rw_est[5], sd = sd)
  for (i in 2:6)
    rw_forecast[i] <- rvec::rnorm_rvec(n = 1, mean = rw_forecast[i-1], sd = sd)
  ans_expected$.fitted[13] <- rvec::rnorm_rvec(n = 1,
                                               components$.fitted[11],
                                               sd = sd_seas)
  ans_expected$.fitted[14] <- rvec::rnorm_rvec(n = 1,
                                               components$.fitted[12],
                                               sd = sd_seas)
  for (i in 3:6)
    ans_expected$.fitted[12 + i] <- rvec::rnorm_rvec(n = 1,
                                                     ans_expected$.fitted[10 + i],
                                                     sd = sd_seas)
  effect_forecast <- ans_expected$.fitted[13:18] + rw_forecast
  ans_expected$.fitted[1:6] <- effect_forecast
  ans_expected$.fitted[7:12] <- rw_forecast
  expect_equal(ans_obtained, ans_expected)
})

test_that("'forecast_term' works with bage_prior_rw2 - n_by = 1", {
  set.seed(0)
  prior <- RW2()
  components <- vctrs::vec_rbind(tibble::tibble(term = "year",
                                                component = "hyper",
                                                level = "sd",
                                                .fitted = rvec::runif_rvec(n = 1, n_draw = 10)),
                                 tibble::tibble(term = "year",
                                                component = "effect",
                                                level = letters[1:5],
                                                .fitted = rvec::rnorm_rvec(n = 5, n_draw = 10)))
  matrix_along_by_est <- matrix(0:4, nr = 5)
  matrix_along_by_forecast <- matrix(0:5, nr = 6)
  levels_forecast <- letters[6:11]
  set.seed(1)
  ans_obtained <- forecast_term(prior = prior,
                                nm_prior = "year",
                                components = components,
                                matrix_along_by_est = matrix_along_by_est,
                                matrix_along_by_forecast = matrix_along_by_forecast,
                                levels_forecast = levels_forecast)
  ans_expected <- tibble::tibble(term = "year",
                                 component = "effect",
                                 level = letters[6:11])
  ans_expected$.fitted <- rvec::rnorm_rvec(n = 6, n_draw = 10)
  sd <- components$.fitted[components$level == "sd"]
  set.seed(1)
  ans_expected$.fitted[1] <- rvec::rnorm_rvec(n = 1,
                                              mean = 2 * components$.fitted[6] - components$.fitted[5],
                                              sd = sd)
  ans_expected$.fitted[2] <- rvec::rnorm_rvec(n = 1,
                                              mean = 2 * ans_expected$.fitted[1] -
                                                components$.fitted[6],
                                              sd = sd)
  for (i in 3:6)
    ans_expected$.fitted[i] <- rvec::rnorm_rvec(n = 1,
                                                mean = 2 * ans_expected$.fitted[i-1] -
                                                  ans_expected$.fitted[i-2],
                                                sd = sd)
  expect_equal(ans_obtained, ans_expected)
})

test_that("'forecast_term' works with bage_prior_rw2 - n_by = 2", {
  set.seed(0)
  prior <- RW2()
  components <- vctrs::vec_rbind(tibble::tibble(term = "year:reg",
                                                component = "hyper",
                                                level = "sd",
                                                .fitted = rvec::runif_rvec(n = 1, n_draw = 10)),
                                 tibble::tibble(term = "year:reg",
                                                component = "effect",
                                                level = letters[1:10],
                                                .fitted = rvec::rnorm_rvec(n = 10, n_draw = 10)))
  matrix_along_by_est <- matrix(0:9, nr = 5)
  matrix_along_by_forecast <- matrix(0:11, nr = 6)
  levels_forecast <- letters[11:22]
  set.seed(1)
  ans_obtained <- forecast_term(prior = prior,
                                nm_prior = "year:reg",
                                components = components,
                                matrix_along_by_est = matrix_along_by_est,
                                matrix_along_by_forecast = matrix_along_by_forecast,
                                levels_forecast = levels_forecast)
  ans_expected <- tibble::tibble(term = "year:reg",
                                 component = "effect",
                                 level = letters[11:22])
  ans_expected$.fitted <- rvec::rnorm_rvec(n = 12, n_draw = 10)
  sd <- components$.fitted[components$level == "sd"]
  set.seed(1)
  ans_expected$.fitted[1] <- rvec::rnorm_rvec(n = 1,
                                              mean = 2 * components$.fitted[6] - components$.fitted[5],
                                              sd = sd)
  ans_expected$.fitted[2] <- rvec::rnorm_rvec(n = 1,
                                              mean = 2 * ans_expected$.fitted[1] -
                                                components$.fitted[6],
                                              sd = sd)
  for (i in 3:6)
    ans_expected$.fitted[i] <- rvec::rnorm_rvec(n = 1,
                                                mean = 2 * ans_expected$.fitted[i-1] -
                                                  ans_expected$.fitted[i-2],
                                                sd = sd)
  ans_expected$.fitted[7] <- rvec::rnorm_rvec(n = 1,
                                              mean = 2 * components$.fitted[11] -
                                                components$.fitted[10],
                                              sd = sd)
  ans_expected$.fitted[8] <- rvec::rnorm_rvec(n = 1,
                                              mean = 2 * ans_expected$.fitted[7] -
                                                components$.fitted[11],
                                              sd = sd)
  for (i in 9:12)
    ans_expected$.fitted[i] <- rvec::rnorm_rvec(n = 1,
                                                mean = 2 * ans_expected$.fitted[i-1] -
                                                  ans_expected$.fitted[i-2],
                                                sd = sd)
  expect_equal(ans_obtained, ans_expected)
})

test_that("'forecast_term' works with bage_prior_rw2seasfix", {
  set.seed(0)
  prior <- RW2Seas(n = 2, s_seas = 0)
  components <- vctrs::vec_rbind(tibble::tibble(term = "year",
                                                component = "hyper",
                                                level = "sd",
                                                .fitted = rvec::runif_rvec(n = 1, n_draw = 10)),
                                 tibble::tibble(term = "year",
                                                component = "effect",
                                                level = letters[1:5],
                                                .fitted = rvec::rnorm_rvec(n = 5, n_draw = 10)),
                                 tibble::tibble(term = "year",
                                                component = "seasonal",
                                                level = letters[1:5],
                                                .fitted = rep(rvec::rnorm_rvec(n = 2, n_draw = 10),
                                                              length.out = 5)))
  trend <- components$.fitted[2:6] - components$.fitted[7:11]
  components <- vctrs::vec_rbind(components,
                                 tibble::tibble(term = "year",
                                                component = "trend",
                                                level = letters[1:5],
                                                .fitted = trend))
  matrix_along_by_est <- matrix(0:4, nr = 5)
  matrix_along_by_forecast <- matrix(0:5, nr = 6)
  levels_forecast <- letters[6:11]
  set.seed(1)
  ans_obtained <- forecast_term(prior = prior,
                                nm_prior = "year",
                                components = components,
                                matrix_along_by_est = matrix_along_by_est,
                                matrix_along_by_forecast = matrix_along_by_forecast,
                                levels_forecast = levels_forecast)
  ans_expected <- tibble::tibble(term = "year",
                                 component = rep(c("effect", "trend", "seasonal"), each = 6),
                                 level = rep(letters[6:11], 3))
  ans_expected$.fitted <- rvec::rnorm_rvec(n = 18, n_draw = 10)
  rw2_est <- components$.fitted[12:16]
  rw2_forecast <- rvec::rnorm_rvec(n = 6, n_draw = 10)
  sd <- components$.fitted[[1]]
  set.seed(1)
  rw2_forecast[1] <- rvec::rnorm_rvec(n = 1, mean = 2 * rw2_est[5] - rw2_est[4], sd = sd)
  rw2_forecast[2] <- rvec::rnorm_rvec(n = 1, mean = 2 * rw2_forecast[1] - rw2_est[5], sd = sd)
  for (i in 3:6)
    rw2_forecast[i] <- rvec::rnorm_rvec(n = 1,
                                        mean = 2 * rw2_forecast[i-1] - rw2_forecast[i-2],
                                        sd = sd)
  ans_expected$.fitted[13] <- components$.fitted[8]
  ans_expected$.fitted[14] <- components$.fitted[7]
  ans_expected$.fitted[15] <- components$.fitted[8]
  ans_expected$.fitted[16] <- components$.fitted[7]
  ans_expected$.fitted[17] <- components$.fitted[8]
  ans_expected$.fitted[18] <- components$.fitted[7]
  effect_forecast <- ans_expected$.fitted[13:18] + rw2_forecast
  ans_expected$.fitted[1:6] <- effect_forecast
  ans_expected$.fitted[7:12] <- rw2_forecast
  expect_equal(ans_obtained, ans_expected)
})

test_that("'forecast_term' works with bage_prior_rw2seasvary", {
  set.seed(0)
  prior <- RW2Seas(n = 2)
  components <- vctrs::vec_rbind(tibble::tibble(term = "year",
                                                component = "hyper",
                                                level = c("sd_seas", "sd"),
                                                .fitted = rvec::runif_rvec(n = 2, n_draw = 10)),
                                 tibble::tibble(term = "year",
                                                component = "effect",
                                                level = letters[1:5],
                                                .fitted = rvec::rnorm_rvec(n = 5, n_draw = 10)),
                                 tibble::tibble(term = "year",
                                                component = "seasonal",
                                                level = letters[1:5],
                                                .fitted = rep(rvec::rnorm_rvec(n = 2, n_draw = 10),
                                                              length.out = 5)))
  trend <- components$.fitted[3:7] - components$.fitted[8:12]
  components <- vctrs::vec_rbind(components,
                                 tibble::tibble(term = "year",
                                                component = "trend",
                                                level = letters[1:5],
                                                .fitted = trend))
  matrix_along_by_est <- matrix(0:4, nr = 5)
  matrix_along_by_forecast <- matrix(0:5, nr = 6)
  levels_forecast <- letters[6:11]
  set.seed(1)
  ans_obtained <- forecast_term(prior = prior,
                                nm_prior = "year",
                                components = components,
                                matrix_along_by_est = matrix_along_by_est,
                                matrix_along_by_forecast = matrix_along_by_forecast,
                                levels_forecast = levels_forecast)
  ans_expected <- tibble::tibble(term = "year",
                                 component = rep(c("effect", "trend", "seasonal"), each = 6),
                                 level = rep(letters[6:11], 3))
  ans_expected$.fitted <- rvec::rnorm_rvec(n = 18, n_draw = 10)
  rw2_est <- components$.fitted[13:17]
  rw2_forecast <- rvec::rnorm_rvec(n = 6, n_draw = 10)
  sd_seas <- components$.fitted[[1]]
  sd <- components$.fitted[[2]]
  set.seed(1)
  rw2_forecast[1] <- rvec::rnorm_rvec(n = 1, mean = 2 * rw2_est[5] - rw2_est[4], sd = sd)
  rw2_forecast[2] <- rvec::rnorm_rvec(n = 1, mean = 2 * rw2_forecast[1] - rw2_est[5], sd = sd)
  for (i in 3:6)
    rw2_forecast[i] <- rvec::rnorm_rvec(n = 1,
                                        mean = 2 * rw2_forecast[i-1] - rw2_forecast[i-2],
                                        sd = sd)
  ans_expected$.fitted[13] <- rvec::rnorm_rvec(n = 1,
                                               components$.fitted[11],
                                               sd = sd_seas)
  ans_expected$.fitted[14] <- rvec::rnorm_rvec(n = 1,
                                               components$.fitted[12],
                                               sd = sd_seas)
  for (i in 3:6)
    ans_expected$.fitted[12 + i] <- rvec::rnorm_rvec(n = 1,
                                                     ans_expected$.fitted[10 + i],
                                                     sd = sd_seas)
  effect_forecast <- ans_expected$.fitted[13:18] + rw2_forecast
  ans_expected$.fitted[1:6] <- effect_forecast
  ans_expected$.fitted[7:12] <- rw2_forecast
  expect_equal(ans_obtained, ans_expected)
})


## 'has_hyperrand' ------------------------------------------------------------

test_that("'has_hyperrand' returns FALSE with prior without hyperrand", {
  prior <- N()
  expect_false(has_hyperrand(prior))
})

test_that("'has_hyperrand' returns TRUE with prior with hyperrand", {
  expect_true(has_hyperrand(Lin()))
  expect_true(has_hyperrand(LinAR()))
  expect_true(has_hyperrand(RWSeas(n=2, s_seas = 0)))
  expect_true(has_hyperrand(RWSeas(n=2, s_seas = 0.1)))
  expect_true(has_hyperrand(RW2Seas(n=2, s_seas = 0)))
  expect_true(has_hyperrand(RW2Seas(n=2, s_seas = 0.1)))
})


## is_known -------------------------------------------------------------------

test_that("'is_known' works with valid inputs", {
    expect_false(is_known(N()))
    expect_true(is_known(Known(values = 1:3)))
})


## is_prior_ok_for_term -------------------------------------------------------

test_that("'is_prior_ok_for_term' works with bage_prior_ar1 - n_by = 1", {
    expect_true(is_prior_ok_for_term(prior = AR1(),
                                     nm = "time",
                                     matrix_along_by = matrix(0:3, nc = 1),
                                     var_time = "time",
                                     var_age = "age",
                                     var_sexgender = "sex",
                                     agesex = "other"))
})

test_that("'is_prior_ok_for_term' works with bage_prior_ar - n_by = 3", {
    expect_true(is_prior_ok_for_term(prior = AR(n = 3),
                                     nm = "time:region",
                                     matrix_along_by = matrix(0:29, nc = 3),
                                     var_time = "time",
                                     var_age = "age",
                                     var_sexgender = "sex",
                                     agesex = "other"))
})

test_that("'is_prior_ok_for_term' throws expected error with bage_prior_known", {
  expect_error(is_prior_ok_for_term(prior = Known(c(0.1, -0.1)),
                                    nm = "sex",
                                    matrix_along_by = matrix(0:2, nc = 1),
                                    var_time = "time",
                                    var_age = "age",
                                    var_sexgender = "sex",
                                    agesex = "other"),
               "`Known\\(c\\(0.1,-0.1\\)\\)` prior for `sex` term invalid.")    
})

test_that("'is_prior_ok_for_term' works with bage_prior_lin - n_by = 2", {
    expect_true(is_prior_ok_for_term(prior = Lin(),
                                     nm = "sex:time",
                                     matrix_along_by = matrix(0:11, nc = 2),
                                     var_time = "time",
                                     var_age = "age",
                                     var_sexgender = "sex",
                                     agesex = "other"))
})

test_that("'is_prior_ok_for_term' works with bage_prior_lin - n_by = 1", {
  expect_true(is_prior_ok_for_term(prior = Lin(),
                                   nm = "sex",
                                   matrix_along_by = matrix(0:1, nc = 1),
                                   var_time = "time",
                                   var_age = "age",
                                   var_sexgender = "sex",
                                   agesex = "other"))
})

test_that("'is_prior_ok_for_term' works with bage_prior_linar - n_by = 1", {
  expect_true(is_prior_ok_for_term(prior = LinAR(),
                                   nm = "sex",
                                   matrix_along_by = matrix(0:2, nc = 1),
                                   var_time = "time",
                                   var_age = "age",
                                   var_sexgender = "sex",
                                   agesex = "other"))
})

test_that("'is_prior_ok_for_term' works with bage_prior_norm", {
    expect_true(is_prior_ok_for_term(prior = N(),
                                     nm = "sex",
                                     matrix_along_by = matrix(0:1, nc = 1),
                                     var_time = "time",
                                     var_age = "age",
                                     var_sexgender = "sex",
                                     agesex = "other"))
})

test_that("'is_prior_ok_for_term' works with bage_prior_normfixed", {
    expect_true(is_prior_ok_for_term(prior = NFix(),
                                     nm = "sex",
                                     matrix_along_by = matrix(0, nc = 1),
                                     var_time = "time",
                                     var_age = "age",
                                     var_sexgender = "sex",
                                     agesex = "other"))
})

test_that("'is_prior_ok_for_term' works with bage_prior_rw - n_by = 1", {
    expect_true(is_prior_ok_for_term(prior = RW(),
                                     nm = "time",
                                     matrix_along_by = matrix(0:1, nc = 1),
                                     var_time = "time",
                                     var_age = "age",
                                     var_sexgender = "sex",
                                     agesex = "other"))
})

test_that("'is_prior_ok_for_term' throws correct error with bage_prior_rw - n_by = 3", {
  expect_true(is_prior_ok_for_term(prior = RW(),
                                   nm = "age:time",
                                   matrix_along_by = matrix(0:11, nc = 3),
                                   var_time = "time",
                                   var_age = "age",
                                   var_sexgender = "sex",
                                   agesex = "other"))
})

test_that("'is_prior_ok_for_term' works with bage_prior_rwseasfix", {
    expect_true(is_prior_ok_for_term(prior = RWSeas(n = 2, s_seas = 0),
                                     nm = "time",
                                     matrix_along_by = matrix(0:1, nc = 1),
                                     var_time = "time",
                                     var_age = "age",
                                     var_sexgender = "sex",
                                     agesex = "other"))
})

test_that("'is_prior_ok_for_term' works with bage_prior_rwseasvary", {
    expect_true(is_prior_ok_for_term(prior = RWSeas(n = 2),
                                     nm = "time",
                                     matrix_along_by = matrix(0:1, nc = 1),
                                     var_time = "time",
                                     var_age = "age",
                                     var_sexgender = "sex",
                                     agesex = "other"))
})

test_that("'is_prior_ok_for_term' throws correct error with bage_prior_rw2 - n_by = 3", {
  expect_true(is_prior_ok_for_term(prior = RW2(),
                                   nm = "age:time",
                                   matrix_along_by = matrix(0:11, nc = 3),
                                   var_time = "time",
                                   var_age = "age",
                                   var_sexgender = "sex",
                                   agesex = "other"))
})

test_that("'is_prior_ok_for_term' works with bage_prior_rw2 - n_by = 1", {
    expect_true(is_prior_ok_for_term(prior = RW2(),
                                     nm = "time",
                                     matrix_along_by = matrix(0:2, nc = 1),
                                     var_time = "time",
                                     var_age = "age",
                                     agesex = "other"))
})

test_that("'is_prior_ok_for_term' works with bage_prior_rw2seasfix", {
    expect_true(is_prior_ok_for_term(prior = RW2Seas(n = 2, s_seas = 0),
                                     nm = "time",
                                     matrix_along_by = matrix(0:1, nc = 1),
                                     var_time = "time",
                                     var_age = "age",
                                     var_sexgender = "sex",
                                     agesex = "other"))
})

test_that("'is_prior_ok_for_term' works with bage_prior_rw2seasvary", {
    expect_true(is_prior_ok_for_term(prior = RW2Seas(n = 2),
                                     nm = "time",
                                     matrix_along_by = matrix(0:1, nc = 1),
                                     var_time = "time",
                                     var_age = "age",
                                     var_sexgender = "sex",
                                     agesex = "other"))
})

test_that("'is_prior_ok_for_term' works with bage_prior_spline - n_by = 1", {
    expect_true(is_prior_ok_for_term(prior = Sp(),
                                     nm = "time",
                                     matrix_along_by = matrix(0:1, nc = 1),
                                     var_time = "time",
                                     var_age = "age",
                                     var_sexgender = "sex",
                                     agesex = "other"))
})

test_that("'is_prior_ok_for_term' works with bage_prior_svd, correct inputs", {
  s <- sim_ssvd()
  expect_true(is_prior_ok_for_term(prior = SVDS(s),
                                   nm = "age:ses",
                                   matrix_along_by = matrix(0:9, nc = 2),
                                   var_time = "time",
                                   var_age = "age",
                                   var_sexgender = "sex",
                                   agesex = "age:sex"))
})

test_that("'is_prior_ok_for_term' method for bage_prior_svd throws correct error when 'var_age' is NULL", {
  s <- sim_ssvd()
  expect_error(is_prior_ok_for_term(prior = SVD(s),
                                    nm = "bla",
                                    matrix_along_by = matrix(0:9, nc = 1),
                                    var_time = "time",
                                    var_age = NULL,
                                    var_sexgender = "sex",
                                    agesex = "other"),
               "Problem with `SVD\\(\\)` prior for `bla` term.")
})

test_that("'is_prior_ok_for_term' method for bage_prior_svd throws correct error when agesex is 'other'", {
  s <- sim_ssvd()
  expect_error(is_prior_ok_for_term(prior = SVDS(s),
                                    nm = "bla",
                                    matrix_along_by = matrix(0:9, nc = 1),
                                    var_time = "time",
                                    var_age = "age",
                                    var_sexgender = "sex",
                                    agesex = "other"),
               "Problem with `SVDS\\(\\)` prior for `bla` term.")
})

test_that("'is_prior_ok_for_term' method for bage_prior_svd throws correct error when SVDS is used for age main effect", {
  s <- sim_ssvd()
  expect_error(is_prior_ok_for_term(prior = SVDS(s),
                                    nm = "age",
                                    matrix_along_by = matrix(0:9, nc = 1),
                                    var_time = "time",
                                    var_age = "age",
                                    var_sexgender = "sex",
                                    agesex = "age"),
               "Problem with `SVDS\\(\\)` prior for `age` term.")
})

test_that("'is_prior_ok_for_term' method for bage_prior_svd throws correct error when SVD is used for age-sex interaction", {
  s <- sim_ssvd()
  expect_error(is_prior_ok_for_term(prior = SVD(s),
                                    nm = "sex:age",
                                    matrix_along_by = matrix(0:9, nc = 2),
                                    var_time = "time",
                                    var_age = "age",
                                    var_sexgender = "sex",
                                    agesex = "sex:age"),
               "Problem with `SVD\\(\\)` prior for `sex:age` term.")
})

test_that("'is_prior_ok_for_term' method for bage_prior_svd throws correct error when SVDS is used for misclassified age-sex interaction, var_sexgender is NULL", {
  s <- sim_ssvd()
  expect_error(is_prior_ok_for_term(prior = SVDS(s),
                                    nm = "sex:age",
                                    matrix_along_by = matrix(0:9, nc = 2),
                                    var_time = "time",
                                    var_age = "age",
                                    var_sexgender = NULL,
                                    agesex = "age:other"),
               "Problem with `SVDS\\(\\)` prior for `sex:age` term.")
})

test_that("'is_prior_ok_for_term' method for bage_prior_svd throws correct error when SVDS is used for misclassified age-sex interaction, var_sexgender is NULL", {
  s <- sim_ssvd()
  expect_error(is_prior_ok_for_term(prior = SVDS(s),
                                    nm = "bla:age",
                                    matrix_along_by = matrix(0:9, nc = 2),
                                    var_time = "time",
                                    var_age = "age",
                                    var_sexgender = "sex",
                                    agesex = "age:other"),
               "Problem with `SVDS\\(\\)` prior for `bla:age` term.")
})

test_that("'is_prior_ok_for_term' method for bage_prior_svd throws correct error when dim has length 2 and 'agesex' not recognised", {
  s <- sim_ssvd()
  expect_error(is_prior_ok_for_term(prior = SVDS(s),
                                    nm = "bla:age",
                                    matrix_along_by = matrix(0:9, nc = 2),
                                    var_time = "time",
                                    var_age = "age",
                                    var_sexgender = "sex",
                                    agesex = "wrong"),
               "Internal error: unexpected value for `agesex`.")
})

test_that("'is_prior_ok_for_term' method for bage_prior_svd works when SVDS is used for 3-way interaction", {
  s <- sim_ssvd()
  expect_true(is_prior_ok_for_term(prior = SVDS(s),
                                    nm = "sex:age:time",
                                    matrix_along_by = matrix(0:29, nc = 2),
                                    var_time = "time",
                                    var_age = "age",
                                    var_sexgender = "sex",
                                    agesex = "sex:age:other"))
})

test_that("'is_prior_ok_for_term' method for bage_prior_svd throws correct error when SVD is used for 3-way interaction involving age, sex", {
  s <- sim_ssvd()
  expect_error(is_prior_ok_for_term(prior = SVD(s),
                                    nm = "sex:age:time",
                                    matrix_along_by = matrix(0:29, nc = 2),
                                    var_time = "time",
                                    var_age = "age",
                                    var_sexgender = "sex",
                                    agesex = "sex:age:other"),
               "Problem with `SVD\\(\\)` prior for `sex:age:time` term.")
})

test_that("'is_prior_ok_for_term' method for bage_prior_svd throws correct error when SVDS is used for 3-way interaction involving age, not sex", {
  s <- sim_ssvd()
  expect_error(is_prior_ok_for_term(prior = SVDS(s),
                                    nm = "reg:age:time",
                                    matrix_along_by = matrix(0:29, nc = 2),
                                    var_time = "time",
                                    var_age = "age",
                                    var_sexgender = "sex",
                                    agesex = "age:other"),
               "Problem with `SVDS\\(\\)` prior for `reg:age:time` term.")
})

test_that("'is_prior_ok_for_term' method for bage_prior_svd works when SVD is used for 3-way interaction involving age, not sex", {
  s <- sim_ssvd()
  expect_true(is_prior_ok_for_term(prior = SVD(s),
                                   nm = "reg:age:time",
                                   matrix_along_by = matrix(0:29, nc = 2),
                                   var_time = "time",
                                   var_age = "age",
                                   var_sexgender = "sex",
                                   agesex = "age:other"))
})

test_that("'is_prior_ok_for_term' method for bage_prior_svd throws correct error when 'agesex' wrong", {
  s <- sim_ssvd()
  expect_error(is_prior_ok_for_term(prior = SVD(s),
                                    nm = "reg:age:time",
                                    matrix_along_by = matrix(0:29, nc = 2),
                                    var_time = "time",
                                    var_age = "age",
                                    var_sexgender = "sex",
                                    agesex = "wrong"),
               "Internal error: unexpected value for `agesex`.")
})


## is_svd -------------------------------------------------------------------

test_that("'is_svd' works with valid inputs", {
    expect_false(is_svd(N()))
    expect_true(is_svd(SVDS(HMD)))
})


## levels_hyper ---------------------------------------------------------------

test_that("'levels_hyper' works with 'bage_prior_ar'", {
  expect_identical(levels_hyper(prior = AR(n = 2)),
                   c("coef1", "coef2", "sd"))
  expect_identical(levels_hyper(prior = AR1()),
                   c("coef", "sd"))
})

test_that("'levels_hyper' works with 'bage_prior_known'", {
  matrix_along_by <- matrix(0:9, ncol = 1L)
  expect_identical(levels_hyper(prior = Known(1)),
                   character())
})

test_that("'levels_hyper' works with 'bage_prior_lin'", {
  matrix_along_by <- matrix(0:9, ncol = 1L)
  expect_identical(levels_hyper(prior = Lin()),
                   "sd")
})

test_that("'levels_hyper' works with 'bage_prior_linar'", {
  matrix_along_by <- matrix(0:9, ncol = 1L)
  expect_identical(levels_hyper(prior = LinAR()),
                   c("sd", "coef1", "coef2"))
  expect_identical(levels_hyper(prior = LinAR1()),
                   c("sd", "coef"))
})

test_that("'levels_hyper' works with 'bage_prior_norm'", {
  matrix_along_by <- matrix(0:9, ncol = 1L)
  expect_identical(levels_hyper(prior = N()),
                   "sd")
})

test_that("'levels_hyper' works with 'bage_prior_normfixed'", {
  matrix_along_by <- matrix(0:9, ncol = 1L)
  expect_identical(levels_hyper(prior = NFix()),
                   character())
})

test_that("'levels_hyper' works with 'bage_prior_rw'", {
  matrix_along_by <- matrix(0:9, ncol = 2L)
  expect_identical(levels_hyper(prior = RW()),
                   "sd")
})

test_that("'levels_hyper' works with 'bage_prior_rwseasfix'", {
  matrix_along_by <- matrix(0:9, ncol = 2L)
  expect_identical(levels_hyper(prior = RWSeas(n = 3, s_seas = 0)),
                   "sd")
})

test_that("'levels_hyper' works with 'bage_prior_rwseasvary'", {
  matrix_along_by <- matrix(0:9, ncol = 2L)
  expect_identical(levels_hyper(prior = RWSeas(n = 3)),
                   c("sd_seas", "sd"))
})

test_that("'levels_hyper' works with 'bage_prior_rw2'", {
  matrix_along_by <- matrix(0:9, ncol = 2L)
  expect_identical(levels_hyper(prior = RW2()),
                   "sd")
})

test_that("'levels_hyper' works with 'bage_prior_rw2seasfix'", {
  matrix_along_by <- matrix(0:9, ncol = 2L)
  expect_identical(levels_hyper(prior = RW2Seas(n = 3, s_seas = 0)),
                   "sd")
})

test_that("'levels_hyper' works with 'bage_prior_rw2seasvary'", {
  matrix_along_by <- matrix(0:9, ncol = 2L)
  expect_identical(levels_hyper(prior = RW2Seas(n = 3)),
                   c("sd_seas", "sd"))
})

test_that("'levels_hyper' works with 'bage_prior_spline'", {
  expect_identical(levels_hyper(prior = Sp()), 
                   "sd")
})

test_that("'levels_hyper' works with 'bage_prior_svd'", {
  expect_identical(levels_hyper(prior = SVD(sim_ssvd())),
                   character())
})


## levels_hyperrand ---------------------------------------------------------------

test_that("'levels_hyperrand' works with 'bage_prior_ar'", {
  levels_effect <- letters
  matrix_along_by <- matrix(0:25, nr = 26)
  rownames(matrix_along_by) <- letters
  ans_obtained <- levels_hyperrand(prior = AR(n = 2), levels_effect = levels_effect)
  ans_expected <- character()
  expect_identical(ans_obtained, ans_expected)
})

test_that("'levels_hyperrand' works with 'bage_prior_lin'", {
  levels_effect <- paste(letters[1:13], rep(c("a", "b"), each = 2), sep = ".")
  matrix_along_by <- matrix(0:25,
                            nr = 13,
                            dimnames = list(x = letters[1:13], y = c("a", "b")))
  ans_obtained <- levels_hyperrand(prior = Lin(),
                                   matrix_along_by = matrix_along_by,
                                   levels_effect = levels_effect)
  ans_expected <- c("slope.a", "slope.b")
  expect_identical(ans_obtained, ans_expected)                   
})

test_that("'levels_hyperrand' works with 'bage_prior_lin' - n_by = 1", {
  levels_effect <- letters[1:13]
  matrix_along_by <- matrix(0:12,
                            nr = 13,
                            dimnames = list(x = letters[1:13], y = NULL))
  ans_obtained <- levels_hyperrand(prior = Lin(),
                                   matrix_along_by = matrix_along_by,
                                   levels_effect = levels_effect)
  ans_expected <- "slope"
  expect_identical(ans_obtained, ans_expected)                   
})


test_that("'levels_hyperrand' works with 'bage_prior_linar'", {
  levels_effect <- paste(letters[1:13], rep(c("a", "b"), each = 2), sep = ".")
  matrix_along_by <- matrix(0:25,
                            nr = 13,
                            dimnames = list(x = letters[1:13], y = c("a", "b")))
  ans_obtained <- levels_hyperrand(prior = LinAR(),
                                   matrix_along_by = matrix_along_by,
                                   levels_effect = levels_effect)
  ans_expected <- c("slope.a", "slope.b")
  expect_identical(ans_obtained, ans_expected)                   
})

test_that("'levels_hyperrand' works with 'bage_prior_linar' - n_by = 1", {
  levels_effect <- letters[1:13]
  matrix_along_by <- matrix(0:12,
                            nr = 13,
                            dimnames = list(x = letters[1:13], y = NULL))
  ans_obtained <- levels_hyperrand(prior = LinAR(),
                                   matrix_along_by = matrix_along_by,
                                   levels_effect = levels_effect)
  ans_expected <- "slope"
  expect_identical(ans_obtained, ans_expected)                   
})

test_that("'levels_hyperrand' works with 'bage_prior_rwseasfix'", {
  levels_effect <- letters[1:13]
  matrix_along_by <- matrix(0:12,
                            nr = 13,
                            dimnames = list(x = letters[1:13], y = NULL))
  ans_obtained <- levels_hyperrand(prior = RWSeas(n = 3, s_seas = 0),
                                   matrix_along_by = matrix_along_by,
                                   levels_effect = levels_effect)
  ans_expected <- c("1", "2", "3")
  expect_identical(ans_obtained, ans_expected)                   
})

test_that("'levels_hyperrand' works with 'bage_prior_rwseasfix'", {
  levels_effect <- letters[1:13]
  matrix_along_by <- matrix(0:12,
                            nr = 13,
                            dimnames = list(x = letters[1:13], y = NULL))
  ans_obtained <- levels_hyperrand(prior = RWSeas(n = 3),
                                   matrix_along_by = matrix_along_by,
                                   levels_effect = levels_effect)
  ans_expected <- levels_effect
  expect_identical(ans_obtained, ans_expected)                   
})

test_that("'levels_hyperrand' works with 'bage_prior_rw2seasfix'", {
  levels_effect <- letters[1:13]
  matrix_along_by <- matrix(0:12,
                            nr = 13,
                            dimnames = list(x = letters[1:13], y = NULL))
  ans_obtained <- levels_hyperrand(prior = RW2Seas(n = 3, s_seas = 0),
                                   matrix_along_by = matrix_along_by,
                                   levels_effect = levels_effect)
  ans_expected <- c("1", "2", "3")
  expect_identical(ans_obtained, ans_expected)                   
})

test_that("'levels_hyperrand' works with 'bage_prior_rw2seasfix'", {
  levels_effect <- letters[1:13]
  matrix_along_by <- matrix(0:12,
                            nr = 13,
                            dimnames = list(x = letters[1:13], y = NULL))
  ans_obtained <- levels_hyperrand(prior = RW2Seas(n = 3),
                                   matrix_along_by = matrix_along_by,
                                   levels_effect = levels_effect)
  ans_expected <- levels_effect
  expect_identical(ans_obtained, ans_expected)                   
})


## 'make_matrix_effectfree_effect' --------------------------------------------------

test_that("'make_matrix_effectfree_effect' works with bage_prior_ar1", {
  prior <- AR1()
  levels_effect <- 2001:2005
  agesex <- "other"
  matrix_along_by <- matrix(0:4, nr = 5)
  ans_obtained <- make_matrix_effectfree_effect(prior = prior,
                                                levels_effect = levels_effect,
                                                levels_age = NULL,
                                                levels_sexgender = NULL,
                                                agesex = agesex,
                                                matrix_along_by = matrix_along_by,
                                                matrix_agesex = NULL)
  ans_expected <- Matrix::.sparseDiagonal(5)
  expect_identical(ans_obtained, ans_expected)
})

test_that("'make_matrix_effectfree_effect' works with bage_prior_spline - n supplied", {
  prior <- Sp(n = 5)
  levels_effect <- 1:10
  matrix_along_by <- matrix(0:9, nc = 1)
  ans_obtained <- make_matrix_effectfree_effect(prior = prior,
                                                levels_effect = levels_effect,
                                                levels_age = 0:9,
                                                levels_sexgender = NULL,
                                                agesex = agesex,
                                                matrix_along_by = matrix_along_by,
                                                matrix_agesex = NULL)
  ans_expected <- make_spline_matrix(n_along = 10,
                                     n_spline = 5)
  expect_identical(ans_obtained, ans_expected)
})

test_that("'make_matrix_effectfree_effect' works with bage_prior_spline - n NULL", {
  prior <- Sp()
  levels_effect <- 1:10
  matrix_along_by <- matrix(0:9, nc = 1)
  ans_obtained <- make_matrix_effectfree_effect(prior = prior,
                                                levels_effect = levels_effect,
                                                levels_age = 0:9,
                                                levels_sexgender = NULL,
                                                agesex = agesex,
                                                matrix_along_by = matrix_along_by,
                                                matrix_agesex = NULL)
  ans_expected <- make_spline_matrix(n_along = 10,
                                     n_spline = 7)
  expect_identical(ans_obtained, ans_expected)
})

test_that("'make_matrix_effectfree_effect' works with bage_prior_spline - age x reg interaction", {
  prior <- Sp(n = 4)
  levels_effect = c("a.0-4", "b.0-4", "a.5-9", "b.5-9", "a.10-14", "b.10-14",
                    "a.15-19", "b.15-19")
  matrix_along_by <- t(matrix(0:7, nc = 4))
  levels_age <- c("0-4", "5-9", "10-14", "15-19")
  agesex <- "age:other"
  ans <- make_matrix_effectfree_effect(prior = prior,
                                       levels_effect = levels_effect,
                                       levels_age = levels_age,
                                       levels_sexgender = NULL,
                                       agesex = agesex,
                                       matrix_along_by = matrix_along_by,
                                       matrix_agesex = NULL)
  expect_identical(dim(ans), c(length(levels_effect), 2L * 4L))
})

test_that("'make_matrix_effectfree_effect' works with bage_prior_svd - age main effect", {
  s <- sim_ssvd()
  prior <- SVD(ssvd = s, n = 3)
  levels_effect <- c("0-4", "5-9")
  agesex <- "age"
  matrix_along_by <- matrix(0:1, nr = 2)
  matrix_agesex <- matrix(0:1, nr = 2, dimnames = list(levels_effect, NULL))
  ans_obtained <- make_matrix_effectfree_effect(prior = prior,
                                                levels_effect = levels_effect,
                                                levels_age = levels_effect,
                                                levels_sexgender = NULL,
                                                agesex = agesex,
                                                matrix_along_by = matrix_along_by,
                                                matrix_agesex = matrix_agesex)
  ans_expected <- s$data$matrix[s$data$type == "total"][[1L]][,1:3]
  ans_expected <- Matrix::sparseMatrix(i = row(ans_expected),
                                       j = col(ans_expected),
                                       x = as.double(ans_expected),
                                       dimnames = dimnames(ans_expected))
  expect_identical(ans_obtained, ans_expected)
})

test_that("'make_matrix_effectfree_effect' works with bage_prior_svd - age-sex interaction, joint", {
  s <- sim_ssvd()
  prior <- SVDS(ssvd = s, n = 3, joint = TRUE)
  levels_effect <- c("Female.0-4", "Female.5-9", "Male.0-4", "Male.5-9")
  levels_age <- c("0-4", "5-9")
  levels_sex <- c("Female", "Male")
  agesex <- "sex:age"
  matrix_along_by <- matrix(c(0L, 2L, 1L, 3L), nr = 2)
  matrix_agesex <- matrix(c(0L, 2L, 1L, 3L), nr = 4)
  ans_obtained <- make_matrix_effectfree_effect(prior = prior,
                                                levels_effect = levels_effect,
                                                levels_age = levels_age,
                                                levels_sex = levels_sex,
                                                agesex = agesex,
                                                matrix_along_by = matrix_along_by,
                                                matrix_agesex = matrix_agesex)
  ans_expected <- s$data$matrix[s$data$type == "joint"][[1L]][c(1,3,2,4),1:3]
  ans_expected <- Matrix::sparseMatrix(i = row(ans_expected),
                                       j = col(ans_expected),
                                       x = as.double(ans_expected))
  expect_identical(ans_obtained, ans_expected)
})

test_that("'make_matrix_effectfree_effect' works with bage_prior_svd - age x reg interaction", {
  s <- sim_ssvd()
  prior <- SVD(ssvd = s, n = 3)
  levels_effect = c("a.0-4", "b.0-4", "a.5-9", "b.5-9")
  levels_age <- c("0-4", "5-9")
  agesex <- "age:other"
  matrix_agesex <- matrix(c(0L, 2L, 1L, 3L), nr = 2,
                          dimnames = list(c("0-4", "5-9"), c("a", "b")))
  ans_obtained <- make_matrix_effectfree_effect(prior = prior,
                                                levels_effect = levels_effect,
                                                levels_age = levels_age,
                                                levels_sexgender = NULL,
                                                agesex = agesex,
                                                matrix_along_by = NULL,
                                                matrix_agesex = matrix_agesex)
  m2 <- s$data$matrix[s$data$type == "total"][[1L]][,1:3]
  m2 <- Matrix::kronecker(Matrix::.sparseDiagonal(2), m2)
  m1 <- make_index_matrix(matrix_agesex)
  ans_expected <- m1 %*% m2
  expect_identical(ans_obtained, ans_expected)
})

test_that("'make_matrix_effectfree_effect' works with bage_prior_svd - sex x reg x age interaction", {
  prior <- SVDS(HMD)
  levels_age <- c(0, "1-4", paste(seq(5, 55, 5), seq(9, 59, 5), sep = "--"), "60+")
  levels_sex <- c("F", "M")
  levels_reg <- c("A", "B")
  levels_effect <-  paste(levels_sex, rep(levels_reg, each = 2), rep(levels_age, each = 4), sep = ".")
  agesex <- "sex:age:other"
  matrix_agesex <- make_matrix_along_by(i_along = c(1, 3),
                                        dim = c(2, 2, 14),
                                        dimnames = list(levels_sex, levels_reg, levels_age))
  ans_obtained <- make_matrix_effectfree_effect(prior = prior,
                                                levels_effect = levels_effect,
                                                levels_age = levels_age,
                                                levels_sexgender = levels_sex,
                                                agesex = agesex,
                                                matrix_along_by = NULL,
                                                matrix_agesex = matrix_agesex)
  m2 <- HMD$data$matrix[[35]][as.integer(t(matrix(1:28,nr=14))), c(1:5, 11:15)]
  m2 <- Matrix::kronecker(Matrix::.sparseDiagonal(2), m2)
  m1 <- make_index_matrix(matrix_agesex)
  ans_expected <- m1 %*% m2
  expect_identical(ans_obtained, ans_expected)
})


## 'make_offset_effectfree_effect' --------------------------------------------------

test_that("'make_offset_effectfree_effect' works with bage_prior_ar1", {
  prior <- AR1()
  levels_effect <- 2001:2005
  agesex <- "other"
  ans_obtained <- make_offset_effectfree_effect(prior = prior,
                                                levels_effect = levels_effect,
                                                agesex = agesex)
  ans_expected <- rep(0, 5)
  expect_identical(ans_obtained, ans_expected)
})

test_that("'make_offset_effectfree_effect' works with bage_prior_svd - age main effect", {
  s <- sim_ssvd()
  prior <- SVD(ssvd = s, n = 3)
  agesex <- "age"
  ans_obtained <- make_offset_effectfree_effect(prior = prior,
                                                levels_age = c("0-4", "5-9"),
                                                levels_sex = NULL,
                                                levels_effect = c("0-4", "5-9"),
                                                agesex = agesex,
                                                matrix_agesex = matrix(0:1, nr = 2))
  ans_expected <- s$data$offset[s$data$type == "total"][[1L]]
  expect_identical(ans_obtained, ans_expected)
})

test_that("'make_offset_effectfree_effect' works with bage_prior_svd - age-sex interaction, joint", {
  s <- sim_ssvd()
  prior <- SVDS(ssvd = s, n = 3, joint = TRUE)
  agesex <- "sex:age"
  matrix_agesex <- matrix(0:3, nc = 1)
  ans_obtained <- make_offset_effectfree_effect(prior = prior,
                                                levels_age = c("0-4", "5-9"),
                                                levels_sexgender = c("Female", "Male"),
                                                levels_effect = c("Female.0-4", "Male.0-4",
                                                                  "Female.5-9", "Male.5-9"),
                                                agesex = agesex,
                                                matrix_agesex = matrix_agesex)
  ans_expected <- s$data$offset[s$data$type == "joint"][[1L]][c(1,3,2,4)]
  expect_identical(ans_obtained, ans_expected)
})

test_that("'make_offset_effectfree_effect' works with bage_prior_svd - age x reg interaction", {
  s <- sim_ssvd()
  prior <- SVD(ssvd = s, n = 3)
  levels_effect = c("0-4.a", "0-4.b", "5-9.a", "5-9.b")
  levels_age <- c("0-4", "5-9")
  agesex <- "age:other"
  matrix_agesex <- matrix(c(0L, 2L, 1L, 3L), nr = 2,
                          dimnames = list(c("0-4", "5-9"), c("a", "b")))
  ans_obtained <- make_offset_effectfree_effect(prior = prior,
                                                levels_effect = levels_effect,
                                                levels_age = levels_age,
                                                levels_sexgender = NULL,
                                                agesex = agesex,
                                                matrix_agesex = matrix_agesex)
  off <- s$data$offset[s$data$type == "total"][[1L]]
  off <- c(off, off)
  m <- make_index_matrix(matrix_agesex)
  ans_expected <- Matrix::drop(m %*% off)
  expect_identical(ans_obtained, ans_expected)
})


## 'reformat_hyperrand_one' ---------------------------------------------------

test_that("'reformat_hyperrand_one' works with prior with no hyperrand", {
  set.seed(0)
  data <- expand.grid(age = 0:4, time = 2000:2005, sex = c("F", "M"))
  data$popn <- rpois(n = nrow(data), lambda = 100)
  data$deaths <- rpois(n = nrow(data), lambda = 10)
  formula <- deaths ~ sex * time + age
  mod <- mod_pois(formula = formula,
                  data = data,
                  exposure = popn)
  mod <- fit(mod)
  matrix_along_by <- choose_matrices_along_by(mod)[[2]]
  comp <- components(mod)
  ans_obtained <- reformat_hyperrand_one(prior = mod$priors[[2]],
                                         nm_prior <- names(mod$priors)[[2]],
                                         matrix_along_by = matrix_along_by,
                                         components = comp)
  ans_expected <- comp
  expect_identical(ans_obtained, ans_expected)
})

test_that("'reformat_hyperrand_one' works with bage_prior_lin", {
  set.seed(0)
  data <- expand.grid(age = 0:4, time = 2000:2005, sex = c("F", "M"))
  data$popn <- rpois(n = nrow(data), lambda = 100)
  data$deaths <- rpois(n = nrow(data), lambda = 10)
  formula <- deaths ~ sex * time + age
  mod <- mod_pois(formula = formula,
                  data = data,
                  exposure = popn) |>
                  set_prior(sex:time ~ Lin()) |>
                  fit(mod)
  comp <- components(mod)
  matrix_along_by <- choose_matrices_along_by(mod)[["sex:time"]]
  ans_obtained <- reformat_hyperrand_one(prior = mod$priors[["sex:time"]],
                                         nm_prior <- "sex:time",
                                         matrix_along_by = matrix_along_by,
                                         components = comp)
  ans_expected <- comp
  ans_expected$component[ans_expected$component == "hyperrand" &
                           ans_expected$term == "sex:time"] <- "hyper"
  expect_identical(ans_obtained, ans_expected)
})

test_that("'reformat_hyperrand_one' works with bage_prior_linar", {
  set.seed(0)
  data <- expand.grid(age = 0:4, time = 2000:2005, sex = c("F", "M"))
  data$popn <- rpois(n = nrow(data), lambda = 100)
  data$deaths <- rpois(n = nrow(data), lambda = 10)
  formula <- deaths ~ sex * time + age
  mod <- mod_pois(formula = formula,
                  data = data,
                  exposure = popn) |>
                  set_prior(sex:time ~ LinAR()) |>
                  fit(mod)
  term <- make_term_components(mod)
  comp <- make_comp_components(mod)
  level <- make_level_components(mod)
  draws <- make_draws_components(mod)
  draws <- as.matrix(draws)
  .fitted <- rvec::rvec_dbl(draws)
  components <- tibble::tibble(term = term,
                               component = comp,
                               level = level,
                               .fitted = .fitted)
  matrix_along_by <- choose_matrices_along_by(mod)[["sex:time"]]
  ans_obtained <- reformat_hyperrand_one(prior = mod$priors[["sex:time"]],
                                         nm_prior = "sex:time",
                                         matrix_along_by = matrix_along_by,
                                         components = components)
  trend <- ans_obtained$.fitted[ans_obtained$component == "trend"]
  cyclical <- ans_obtained$.fitted[ans_obtained$component == "cyclical"]
  effect <- ans_obtained$.fitted[ans_obtained$component == "effect" & ans_obtained$term == "sex:time"]
  expect_equal(effect, trend + cyclical)
  expect_equal(rvec::draws_mean(sum(trend)), 0)
  expect_identical(ans_obtained$component[ans_obtained$term == "sex:time"
                                          & grepl("slope", ans_obtained$level)],
                   c("hyper", "hyper"))
})

test_that("'reformat_hyperrand_one' works with bage_prior_rwseasfix", {
  set.seed(0)
  data <- expand.grid(age = 0:4, time = 2000:2005, sex = c("F", "M"))
  data$popn <- rpois(n = nrow(data), lambda = 100)
  data$deaths <- rpois(n = nrow(data), lambda = 10)
  formula <- deaths ~ sex * time + age
  mod <- mod_pois(formula = formula,
                  data = data,
                  exposure = popn) |>
                  set_prior(sex:time ~ RWSeas(n = 3, s_seas = 0)) |>
                  set_n_draw(n = 10) |>
                  fit(mod)
  term <- make_term_components(mod)
  comp <- make_comp_components(mod)
  level <- make_level_components(mod)
  draws <- make_draws_components(mod)
  draws <- as.matrix(draws)
  .fitted <- rvec::rvec_dbl(draws)
  components <- tibble::tibble(term = term,
                               component = comp,
                               level = level,
                               .fitted = .fitted)
  matrix_along_by <- choose_matrices_along_by(mod)[["sex:time"]]
  ans_obtained <- reformat_hyperrand_one(prior = mod$priors[["sex:time"]],
                                         nm_prior = "sex:time",
                                         matrix_along_by = matrix_along_by,
                                         components = components)
  ans_expected <- components
  seas <- ans_expected$.fitted[ans_expected$component == "hyperrand" & ans_expected$term == "sex:time"]
  seas <- center_within_across_by(seas, matrix(0:5, nr = 3))
  seas <- seas[c(1,4,2,5,3,6,1,4,2,5,3,6)]
  level <- ans_expected$level[ans_expected$component == "effect" & ans_expected$term == "sex:time"]
  seasonal <- tibble::tibble(term = "sex:time",
                             component = "seasonal",
                             level = level,
                             .fitted = seas)
  effect <- ans_expected$.fitted[ans_expected$component == "effect" & ans_expected$term == "sex:time"]
  trend <- effect - seas
  trend <- tibble::tibble(term = "sex:time",
                         component = "trend",
                         level = level,
                         .fitted = trend)
  ans_expected <- ans_expected[!(ans_expected$component == "hyperrand" & ans_expected$term == "sex:time"),]
  ans_expected <- vctrs::vec_rbind(ans_expected, seasonal, trend)
  ## ans_expected <- sort_components(ans_expected, mod = mod)
  expect_equal(ans_obtained, ans_expected)
})

test_that("'reformat_hyperrand_one' works with bage_prior_rwseasvary", {
  set.seed(0)
  data <- expand.grid(age = 0:4, time = 2000:2020, sex = c("F", "M"))
  data$popn <- rpois(n = nrow(data), lambda = 100)
  data$deaths <- rpois(n = nrow(data), lambda = 10)
  formula <- deaths ~ sex * time + age
  mod <- mod_pois(formula = formula,
                  data = data,
                  exposure = popn) |>
                  set_prior(sex:time ~ RWSeas(n = 3)) |>
                  fit(mod)
  term <- make_term_components(mod)
  comp <- make_comp_components(mod)
  level <- make_level_components(mod)
  draws <- make_draws_components(mod)
  draws <- as.matrix(draws)
  .fitted <- rvec::rvec_dbl(draws)
  components <- tibble::tibble(term = term,
                               component = comp,
                               level = level,
                               .fitted = .fitted)
  matrix_along_by <- choose_matrices_along_by(mod)[["sex:time"]]
  ans_obtained <- reformat_hyperrand_one(prior = mod$priors[["sex:time"]],
                                         nm_prior = "sex:time",
                                         matrix_along_by = matrix_along_by,
                                         components = components)
  ans_expected <- components
  seas <- ans_expected$.fitted[ans_expected$component == "hyperrand" & ans_expected$term == "sex:time"]
  seas <- center_within_across_by(seas, matrix_along_by = matrix_along_by)
  ans_expected$.fitted[ans_expected$component == "hyperrand" & ans_expected$term == "sex:time"] <- seas
  ans_expected$component[ans_expected$component == "hyperrand" & ans_expected$term == "sex:time"] <- "seasonal"
  effect <- ans_expected$.fitted[ans_expected$component == "effect" & ans_expected$term == "sex:time"]
  trend <- effect - seas
  level <- ans_expected$level[ans_expected$component == "effect" & ans_expected$term == "sex:time"]
  trend <- tibble::tibble(term = "sex:time",
                         component = "trend",
                         level = level,
                         .fitted = trend)
  ans_expected <- vctrs::vec_rbind(ans_expected, trend)
  expect_equal(ans_obtained, ans_expected)
})

test_that("'reformat_hyperrand_one' works with bage_prior_rwseasvary", {
  set.seed(0)
  data <- expand.grid(age = 0:4, time = 2000:2020, sex = c("F", "M"))
  data$popn <- rpois(n = nrow(data), lambda = 100)
  data$deaths <- rpois(n = nrow(data), lambda = 10)
  formula <- deaths ~ sex * time + age
  mod <- mod_pois(formula = formula,
                  data = data,
                  exposure = popn) |>
                  set_prior(sex:time ~ RWSeas(n = 3)) |>
                  fit(mod)
  term <- make_term_components(mod)
  comp <- make_comp_components(mod)
  level <- make_level_components(mod)
  draws <- make_draws_components(mod)
  draws <- as.matrix(draws)
  .fitted <- rvec::rvec_dbl(draws)
  components <- tibble::tibble(term = term,
                               component = comp,
                               level = level,
                               .fitted = .fitted)
  matrix_along_by <- choose_matrices_along_by(mod)[["sex:time"]]
  ans_obtained <- reformat_hyperrand_one(prior = mod$priors[["sex:time"]],
                                         nm_prior = "sex:time",
                                         matrix_along_by = matrix_along_by,
                                         components = components)
  ans_expected <- components
  seas <- ans_expected$.fitted[ans_expected$component == "hyperrand" & ans_expected$term == "sex:time"]
  seas <- center_within_across_by(seas, matrix_along_by = matrix_along_by)
  ans_expected$.fitted[ans_expected$component == "hyperrand" & ans_expected$term == "sex:time"] <- seas
  ans_expected$component[ans_expected$component == "hyperrand" & ans_expected$term == "sex:time"] <- "seasonal"
  effect <- ans_expected$.fitted[ans_expected$component == "effect" & ans_expected$term == "sex:time"]
  trend <- effect - seas
  level <- ans_expected$level[ans_expected$component == "effect" & ans_expected$term == "sex:time"]
  trend <- tibble::tibble(term = "sex:time",
                         component = "trend",
                         level = level,
                         .fitted = trend)
  ans_expected <- vctrs::vec_rbind(ans_expected, trend)
  expect_equal(ans_obtained, ans_expected)
})




test_that("'reformat_hyperrand_one' works with bage_prior_rw2seasfix", {
  set.seed(0)
  data <- expand.grid(age = 0:4, time = 2000:2005, sex = c("F", "M"))
  data$popn <- rpois(n = nrow(data), lambda = 100)
  data$deaths <- rpois(n = nrow(data), lambda = 10)
  formula <- deaths ~ sex * time + age
  mod <- mod_pois(formula = formula,
                  data = data,
                  exposure = popn) |>
                  set_prior(sex:time ~ RW2Seas(n = 3, s_seas = 0)) |>
                  set_n_draw(n = 10) |>
                  fit(mod)
  term <- make_term_components(mod)
  comp <- make_comp_components(mod)
  level <- make_level_components(mod)
  draws <- make_draws_components(mod)
  draws <- as.matrix(draws)
  .fitted <- rvec::rvec_dbl(draws)
  components <- tibble::tibble(term = term,
                               component = comp,
                               level = level,
                               .fitted = .fitted)
  matrix_along_by <- choose_matrices_along_by(mod)[["sex:time"]]
  ans_obtained <- reformat_hyperrand_one(prior = mod$priors[["sex:time"]],
                                         nm_prior = "sex:time",
                                         matrix_along_by = matrix_along_by,
                                         components = components)
  ans_expected <- components
  seas <- ans_expected$.fitted[ans_expected$component == "hyperrand" & ans_expected$term == "sex:time"]
  seas <- center_within_across_by(seas, matrix(0:5, nr = 3))
  seas <- seas[c(1,4,2,5,3,6,1,4,2,5,3,6)]
  level <- ans_expected$level[ans_expected$component == "effect" & ans_expected$term == "sex:time"]
  seasonal <- tibble::tibble(term = "sex:time",
                             component = "seasonal",
                             level = level,
                             .fitted = seas)
  effect <- ans_expected$.fitted[ans_expected$component == "effect" & ans_expected$term == "sex:time"]
  trend <- effect - seas
  trend <- tibble::tibble(term = "sex:time",
                         component = "trend",
                         level = level,
                         .fitted = trend)
  ans_expected <- ans_expected[!(ans_expected$component == "hyperrand" & ans_expected$term == "sex:time"),]
  ans_expected <- vctrs::vec_rbind(ans_expected, seasonal, trend)
  ## ans_expected <- sort_components(ans_expected, mod = mod)
  expect_equal(ans_obtained, ans_expected)
})

test_that("'reformat_hyperrand_one' works with bage_prior_rw2seasvary", {
  set.seed(0)
  data <- expand.grid(age = 0:4, time = 2000:2020, sex = c("F", "M"))
  data$popn <- rpois(n = nrow(data), lambda = 100)
  data$deaths <- rpois(n = nrow(data), lambda = 10)
  formula <- deaths ~ sex * time + age
  mod <- mod_pois(formula = formula,
                  data = data,
                  exposure = popn) |>
                  set_prior(sex:time ~ RW2Seas(n = 3)) |>
                  fit(mod)
  term <- make_term_components(mod)
  comp <- make_comp_components(mod)
  level <- make_level_components(mod)
  draws <- make_draws_components(mod)
  draws <- as.matrix(draws)
  .fitted <- rvec::rvec_dbl(draws)
  components <- tibble::tibble(term = term,
                               component = comp,
                               level = level,
                               .fitted = .fitted)
  matrix_along_by <- choose_matrices_along_by(mod)[["sex:time"]]
  ans_obtained <- reformat_hyperrand_one(prior = mod$priors[["sex:time"]],
                                         nm_prior = "sex:time",
                                         matrix_along_by = matrix_along_by,
                                         components = components)
  ans_expected <- components
  seas <- ans_expected$.fitted[ans_expected$component == "hyperrand" & ans_expected$term == "sex:time"]
  seas <- center_within_across_by(seas, matrix_along_by = matrix_along_by)
  ans_expected$.fitted[ans_expected$component == "hyperrand" & ans_expected$term == "sex:time"] <- seas
  ans_expected$component[ans_expected$component == "hyperrand" & ans_expected$term == "sex:time"] <- "seasonal"
  effect <- ans_expected$.fitted[ans_expected$component == "effect" & ans_expected$term == "sex:time"]
  trend <- effect - seas
  level <- ans_expected$level[ans_expected$component == "effect" & ans_expected$term == "sex:time"]
  trend <- tibble::tibble(term = "sex:time",
                         component = "trend",
                         level = level,
                         .fitted = trend)
  ans_expected <- vctrs::vec_rbind(ans_expected, trend)
  expect_equal(ans_obtained, ans_expected)
})


## 'str_call_prior' -----------------------------------------------------------

test_that("'str_call_prior' works with bage_prior_ar - AR1", {
    expect_identical(str_call_prior(AR1()), "AR1()")
    expect_identical(str_call_prior(AR1(min = 0.5)), "AR1(min=0.5)")
    expect_identical(str_call_prior(AR1(max = 0.95)),"AR1(max=0.95)")
    expect_identical(str_call_prior(AR1(s = 0.3)), "AR1(s=0.3)")
    expect_identical(str_call_prior(AR1(min = 0.5, max = 0.95, along = "age", s = 0.3)),
                     "AR1(min=0.5,max=0.95,s=0.3,along=\"age\")")
})

test_that("'str_call_prior' works with bage_prior_ar - AR", {
    expect_identical(str_call_prior(AR(n = 1)), "AR(n=1)")
    expect_identical(str_call_prior(AR(n = 3, s = 0.3)), "AR(n=3,s=0.3)")
    expect_identical(str_call_prior(AR(s = 0.3, along = "cohort", n = 2)),
                     "AR(n=2,s=0.3,along=\"cohort\")")
})

test_that("'str_call_prior' works with bage_prior_known", {
    expect_identical(str_call_prior(Known(1)), "Known(1)")
    expect_identical(str_call_prior(Known(c(2, 3, -2, 0))),
                                    "Known(c(2,3,-2,0))")
    expect_identical(str_call_prior(Known(c(2, 3, -2, 0,7, 3))),
                                    "Known(c(2,...,3))")
})

test_that("'str_call_prior' works with bage_prior_lin", {
    expect_identical(str_call_prior(Lin()), "Lin()")
    expect_identical(str_call_prior(Lin(sd = 0.5, s = 2, along = "a")),
                     "Lin(s=2,sd=0.5,along=\"a\")")
})

test_that("'str_call_prior' works with bage_prior_linar - AR format", {
    expect_identical(str_call_prior(LinAR()), "LinAR()")
    expect_identical(str_call_prior(LinAR(sd = 0.5)), "LinAR(sd=0.5)")
    expect_identical(str_call_prior(LinAR(sd=2L,s = 0.95)), "LinAR(s=0.95,sd=2)")
    expect_identical(str_call_prior(LinAR(sd = 0.1, along = "cohort", s = 0.95,n=3)),
                     "LinAR(n=3,s=0.95,sd=0.1,along=\"cohort\")")
})

test_that("'str_call_prior' works with bage_prior_linar - AR1 format", {
    expect_identical(str_call_prior(LinAR1()), "LinAR1()")
    expect_identical(str_call_prior(LinAR1(along="age",sd = 0.5)), "LinAR1(sd=0.5,along=\"age\")")
    expect_identical(str_call_prior(LinAR1(sd=2L,s = 0.95)), "LinAR1(s=0.95,sd=2)")
    expect_identical(str_call_prior(LinAR1(sd = 0.1, max=1,s = 0.95, min = 0.5)),
                     "LinAR1(min=0.5,max=1,s=0.95,sd=0.1)")
})

test_that("'str_call_prior' works with bage_prior_norm", {
    expect_identical(str_call_prior(N()), "N()")
    expect_identical(str_call_prior(N(s = 0.95)), "N(s=0.95)")
})

test_that("'str_call_prior' works with bage_prior_normfixed", {
    expect_identical(str_call_prior(NFix()), "NFix()")
    expect_identical(str_call_prior(NFix(sd = 0.95)), "NFix(sd=0.95)")
})

test_that("'str_call_prior' works with bage_prior_rw", {
    expect_identical(str_call_prior(RW()), "RW()")
    expect_identical(str_call_prior(RW(along = "a", s = 2)),
                     "RW(s=2,along=\"a\")")
})

test_that("'str_call_prior' works with bage_prior_rwseasfix", {
    expect_identical(str_call_prior(RWSeas(n=2, s_seas = 0)), "RWSeas(n=2,s_seas=0)")
    expect_identical(str_call_prior(RWSeas(along = "a", s = 2, n = 5, s_seas=0)),
                     "RWSeas(n=5,s=2,s_seas=0,along=\"a\")")
})

test_that("'str_call_prior' works with bage_prior_rwseasvary", {
    expect_identical(str_call_prior(RWSeas(n=2)), "RWSeas(n=2)")
    expect_identical(str_call_prior(RWSeas(along = "a", s = 2, n = 5, s_seas=0.1)),
                     "RWSeas(n=5,s=2,s_seas=0.1,along=\"a\")")
})

test_that("'str_call_prior' works with bage_prior_rw2", {
    expect_identical(str_call_prior(RW2()), "RW2()")
    expect_identical(str_call_prior(RW2(along = "a", s = 2)),
                     "RW2(s=2,along=\"a\")")
})

test_that("'str_call_prior' works with bage_prior_rw2seasfix", {
    expect_identical(str_call_prior(RW2Seas(n=2, s_seas = 0)), "RW2Seas(n=2,s_seas=0)")
    expect_identical(str_call_prior(RW2Seas(along = "a", s = 2, n = 5, s_seas=0)),
                     "RW2Seas(n=5,s=2,s_seas=0,along=\"a\")")
})

test_that("'str_call_prior' works with bage_prior_rw2seasvary", {
    expect_identical(str_call_prior(RW2Seas(n=2)), "RW2Seas(n=2)")
    expect_identical(str_call_prior(RW2Seas(along = "a", s = 2, n = 5, s_seas=0.1)),
                     "RW2Seas(n=5,s=2,s_seas=0.1,along=\"a\")")
})

test_that("'str_call_prior' works with bage_prior_spline", {
    expect_identical(str_call_prior(Sp()), "Sp()")
    expect_identical(str_call_prior(Sp(n = 5L)), "Sp(n=5)")
    expect_identical(str_call_prior(Sp(s = 0.1)), "Sp(s=0.1)")
    expect_identical(str_call_prior(Sp(s = 3,along = "cohort", n = 5L)),
                     "Sp(n=5,s=3,along=\"cohort\")")
})

test_that("'str_call_prior' works with bage_prior_svd", {
    s <- sim_ssvd()
    expect_identical(str_call_prior(SVD(s)), "SVD(s)")
    expect_identical(str_call_prior(SVDS(s,joint = TRUE)), "SVDS(s,joint=TRUE)")
    expect_identical(str_call_prior(SVD(s,n = 6L)), "SVD(s,n=6)")
    expect_identical(str_call_prior(SVDS(s,joint=F,n = 3L)),
                     "SVDS(s,n=3)")
})


## 'str_nm_prior' -----------------------------------------------------------

test_that("'str_nm_prior' works with bage_prior_ar - AR1", {
  expect_identical(str_nm_prior(AR1(s = 3)), "AR1()")
})

test_that("'str_nm_prior' works with bage_prior_ar - AR", {
   expect_identical(str_nm_prior(AR(n = 1)), "AR()")
   expect_identical(str_nm_prior(AR(n = 3, s = 0.3)), "AR()")
})

test_that("'str_nm_prior' works with bage_prior_known", {
    expect_identical(str_nm_prior(Known(1)), "Known()")
    expect_identical(str_nm_prior(Known(c(2, 3, -2, 0,7, 3))), "Known()")
})

test_that("'str_nm_prior' works with bage_prior_lin", {
    expect_identical(str_nm_prior(Lin()), "Lin()")
    expect_identical(str_nm_prior(Lin(sd = 0.1, s = 0.95)), "Lin()")
})

test_that("'str_nm_prior' works with bage_prior_linar - AR", {
   expect_identical(str_nm_prior(LinAR(n = 1)), "LinAR()")
   expect_identical(str_nm_prior(LinAR(n = 3, s = 0.3)), "LinAR()")
})

test_that("'str_nm_prior' works with bage_prior_linar - AR1", {
   expect_identical(str_nm_prior(LinAR1(max = 1)), "LinAR1()")
   expect_identical(str_nm_prior(LinAR1(sd = 3, s = 0.3)), "LinAR1()")
})

test_that("'str_nm_prior' works with bage_prior_norm", {
    expect_identical(str_nm_prior(N()), "N()")
    expect_identical(str_nm_prior(N(s = 0.95)), "N()")
})

test_that("'str_nm_prior' works with bage_prior_normfixed", {
    expect_identical(str_nm_prior(NFix()), "NFix()")
    expect_identical(str_nm_prior(NFix(sd = 0.95)), "NFix()")
})

test_that("'str_nm_prior' works with bage_prior_rw", {
    expect_identical(str_nm_prior(RW()), "RW()")
    expect_identical(str_nm_prior(RW(s = 0.95)), "RW()")
})

test_that("'str_nm_prior' works with bage_prior_rwseasfix", {
    expect_identical(str_nm_prior(RWSeas(n = 3, s_seas = 0)), "RWSeas()")
    expect_identical(str_nm_prior(RWSeas(n = 3, s = 0.95, s_seas = 0)), "RWSeas()")
})

test_that("'str_nm_prior' works with bage_prior_rwseasvary", {
    expect_identical(str_nm_prior(RWSeas(n = 3, s_seas = 4)), "RWSeas()")
    expect_identical(str_nm_prior(RWSeas(n = 3, s = 0.95)), "RWSeas()")
})

test_that("'str_nm_prior' works with bage_prior_rw2", {
    expect_identical(str_nm_prior(RW2()), "RW2()")
    expect_identical(str_nm_prior(RW2(s = 0.95)), "RW2()")
})

test_that("'str_nm_prior' works with bage_prior_rw2seasfix", {
    expect_identical(str_nm_prior(RW2Seas(n = 3, s_seas = 0)), "RW2Seas()")
    expect_identical(str_nm_prior(RW2Seas(n = 3, s = 0.95, s_seas = 0)), "RW2Seas()")
})

test_that("'str_nm_prior' works with bage_prior_rw2seasvary", {
    expect_identical(str_nm_prior(RW2Seas(n = 3, s_seas = 4)), "RW2Seas()")
    expect_identical(str_nm_prior(RW2Seas(n = 3, s = 0.95)), "RW2Seas()")
})

test_that("'str_nm_prior' works with bage_prior_spline", {
    expect_identical(str_nm_prior(Sp()), "Sp()")
    expect_identical(str_nm_prior(Sp(s = 3,n = 5L)), "Sp()")
})

test_that("'str_nm_prior' works with bage_prior_svd", {
    s <- sim_ssvd()
    expect_identical(str_nm_prior(SVD(s)), "SVD()")
    expect_identical(str_nm_prior(SVDS(s,joint=F,n = 3L)), "SVDS()")
})


## transform_hyper ------------------------------------------------------------

test_that("'transform_hyper' works with 'bage_prior_ar - AR1'", {
  shifted_invlogit <- function(x) {
    ans <- exp(x) / (1 + exp(x))
    0.18 * ans + 0.8
  }
  l <- transform_hyper(prior = AR1())
  expect_equal(l[[1]](0.35), shifted_invlogit(0.35))
  expect_equal(l[[2]](0.35), exp(0.35))
})

test_that("'transform_hyper' works with 'bage_prior_ar - AR'", {
  shifted_invlogit <- function(x) {
    ans <- exp(x) / (1 + exp(x))
    2 * ans - 1
  }
  l <- transform_hyper(prior = AR(n = 2))
  expect_equal(l[[1]](0.35), shifted_invlogit(0.35))
  expect_equal(l[[2]](0.35), shifted_invlogit(0.35))
  expect_equal(l[[3]](0.35), exp(0.35))
})

test_that("'transform_hyper' works with 'bage_prior_known'", {
  l <- transform_hyper(prior = Known(1))
  expect_identical(l, list())
})

test_that("'transform_hyper' works with 'bage_prior_lin'", {
  l <- transform_hyper(prior = Lin())
  expect_equal(exp(0.35), l[[1]](0.35))
})

test_that("'transform_hyper' works with 'bage_prior_linar - AR'", {
  shifted_invlogit <- function(x) {
    ans <- exp(x) / (1 + exp(x))
    2 * ans - 1
  }
  l <- transform_hyper(prior = LinAR(n = 2))
  expect_equal(l[[1]](0.35), exp(0.35))
  expect_equal(l[[2]](0.35), shifted_invlogit(0.35))
  expect_equal(l[[3]](0.35), shifted_invlogit(0.35))
})

test_that("'transform_hyper' works with 'bage_prior_linar - AR1'", {
  shifted_invlogit <- function(x) {
    ans <- exp(x) / (1 + exp(x))
    0.18 * ans + 0.8
  }
  l <- transform_hyper(prior = LinAR1())
  expect_equal(l[[1]](0.35), exp(0.35))
  expect_equal(l[[2]](0.35), shifted_invlogit(0.35))
})

test_that("'transform_hyper' works with 'bage_prior_norm'", {
  l <- transform_hyper(prior = N())
  expect_equal(0.35, l[[1]](log(0.35)))
})

test_that("'transform_hyper' works with 'bage_prior_normfixed'", {
  l <- transform_hyper(prior = NFix())
  expect_identical(l, list())
})

test_that("'transform_hyper' works with 'bage_prior_rw'", {
  l <- transform_hyper(prior = RW())
  expect_equal(0.35, l[[1]](log(0.35)))
})

test_that("'transform_hyper' works with 'bage_prior_rwseasfix'", {
  l <- transform_hyper(prior = RWSeas(n = 3, s_seas=0))
  expect_equal(0.35, l[[1]](log(0.35)))
})

test_that("'transform_hyper' works with 'bage_prior_rwseasvary'", {
  l <- transform_hyper(prior = RWSeas(n = 3))
  expect_equal(0.35, l[[1]](log(0.35)))
  expect_equal(0.35, l[[2]](log(0.35)))
})

test_that("'transform_hyper' works with 'bage_prior_rw2'", {
  l <- transform_hyper(prior = RW2())
  expect_equal(0.35, l[[1]](log(0.35)))
})

test_that("'transform_hyper' works with 'bage_prior_rw2seasfix'", {
  l <- transform_hyper(prior = RW2Seas(n = 3, s_seas=0))
  expect_equal(0.35, l[[1]](log(0.35)))
})

test_that("'transform_hyper' works with 'bage_prior_rw2seasvary'", {
  l <- transform_hyper(prior = RW2Seas(n = 3))
  expect_equal(0.35, l[[1]](log(0.35)))
  expect_equal(0.35, l[[2]](log(0.35)))
})

test_that("'transform_hyper' works with 'bage_prior_spline'", {
  l <- transform_hyper(prior = Sp())
  expect_equal(0.35, l[[1]](log(0.35)))
})

test_that("'transform_hyper' works with 'bage_prior_svd'", {
  l <- transform_hyper(prior = SVD(HMD))
  expect_identical(l, list())
})


## uses_along -----------------------------------------------------------------

test_that("'uses_along' works with valid inputs", {
  expect_true(uses_along(AR()))
  expect_false(uses_along(Known(c(a = 1))))
  expect_true(uses_along(Lin()))
  expect_true(uses_along(LinAR()))
  expect_false(uses_along(N()))
  expect_false(uses_along(NFix()))
  expect_true(uses_along(RW()))
  expect_true(uses_along(RWSeas(n = 3, s_seas = 0)))
  expect_true(uses_along(RWSeas(n = 3)))
  expect_true(uses_along(RW2()))
  expect_true(uses_along(RW2Seas(n = 3, s_seas = 0)))
  expect_true(uses_along(RW2Seas(n = 3)))
  expect_true(uses_along(Sp()))
  expect_false(uses_along(SVD(HMD)))
})


## uses_hyperrand ------------------------------------------------------

test_that("'uses_hyperrand' returns FALSE with priors that do not use hyperrand parameters", {
  expect_false(uses_hyperrand(AR1()))
  expect_false(uses_hyperrand(Known(c(a = 1, b = -1))))
  expect_false(uses_hyperrand(N()))
  expect_false(uses_hyperrand(NFix()))
  expect_false(uses_hyperrand(RW()))
  expect_true(uses_hyperrand(RWSeas(n = 3, s_seas = 0)))
  expect_true(uses_hyperrand(RWSeas(n = 3)))
  expect_false(uses_hyperrand(RW2()))
  expect_true(uses_hyperrand(RW2Seas(n = 3, s_seas = 0)))
  expect_true(uses_hyperrand(RW2Seas(n = 3)))
  expect_false(uses_hyperrand(Sp()))
  expect_false(uses_hyperrand(SVD(HMD)))
})

test_that("'uses_hyperrand' returns TRUE with priors do use hyperrand parameters", {
  expect_true(uses_hyperrand(Lin()))
  expect_true(uses_hyperrand(LinAR()))
})


## uses_matrix_effectfree_effect ----------------------------------------------

test_that("'uses_matrix_effectfree_effect' works with valid inputs", {
    expect_false(uses_matrix_effectfree_effect(N()))
    expect_true(uses_matrix_effectfree_effect(Sp()))
    expect_true(uses_matrix_effectfree_effect(SVD(HMD)))
})


## uses_offset_effectfree_effect ----------------------------------------------

test_that("'uses_offset_effectfree_effect' works with valid inputs", {
    expect_false(uses_offset_effectfree_effect(N()))
    expect_false(uses_offset_effectfree_effect(Sp()))
    expect_true(uses_offset_effectfree_effect(SVD(HMD)))
})


## vals_hyper_to_dataframe ----------------------------------------------------

test_that("'vals_hyper_to_dataframe' works with bage_prior_ar", {
  prior <- AR(n = 3)
  vals_hyper <- draw_vals_hyper(prior = prior,
                                n_sim = 10)
  ans_obtained <- vals_hyper_to_dataframe(prior = prior,
                                          nm_prior = "time",
                                          vals_hyper = vals_hyper,
                                          n_sim = 10)
  ans_expected <- tibble::tibble(term = "time",
                                 component = "hyper",
                                 level = c("coef1", "coef2", "coef3", "sd"),
                                 .fitted = rvec::rvec(unname(rbind(vals_hyper[[1]], vals_hyper[[2]]))))
  expect_equal(ans_obtained, ans_expected)
})

test_that("'vals_hyper_to_dataframe' works with bage_prior_fixed", {
  prior <- NFix()
  vals_hyper <- draw_vals_hyper(prior = prior,
                                n_sim = 10)
  ans_obtained <- vals_hyper_to_dataframe(prior = prior,
                                          nm_prior = "time",
                                          vals_hyper = vals_hyper,
                                          n_sim = 10)
  ans_expected <- tibble::tibble(term = character(),
                                 component = character(),
                                 level = character(),
                                 .fitted = rvec::rvec_dbl(matrix(0, nr = 0, nc = 10)))
  expect_equal(ans_obtained, ans_expected)
})


## vals_hyperrand_to_dataframe ----------------------------------------------------

test_that("'vals_hyper_to_dataframe' works with bage_prior_ar", {
  prior <- AR(n = 3)
  vals_hyperrand <- list()
  ans_obtained <- vals_hyperrand_to_dataframe(prior = prior,
                                              nm_prior = "time",
                                              vals_hyperrand = vals_hyperrand,
                                              n_sim = 10)
  ans_expected <- tibble::tibble(term = character(),
                                 component = character(),
                                 level = character(),
                                 .fitted = rvec::rvec(matrix(0, nrow = 1, ncol = 10)))
  expect_equal(ans_obtained, ans_expected)
})

test_that("'vals_hyper_to_dataframe' works with bage_prior_lin", {
  set.seed(0)
  prior <- Lin()
  vals_hyperrand <- list(slope = matrix(rnorm(30), nr = 3))
  ans_obtained <- vals_hyperrand_to_dataframe(prior = prior,
                                              nm_prior = "time",
                                              vals_hyperrand = vals_hyperrand,
                                              n_sim = 10)
  ans_expected <- tibble::tibble(term = "time",
                                 component = "hyperrand",
                                 level = c("slope", "slope", "slope"),
                                 .fitted = rvec::rvec(vals_hyperrand$slope))
  expect_equal(ans_obtained, ans_expected)
})


## values_known ---------------------------------------------------------------

test_that("'values_known' works with valid inputs", {
    expect_identical(values_known(Known(values = 1:3)),
                     as.double(1:3))
})
